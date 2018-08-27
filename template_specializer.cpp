#include "ast_visitor.hpp"
#include "rewriter.hpp"

#include <clang/Lex/Lexer.h>
#include <clang/AST/Expr.h>

#include <iostream>
#include <numeric>
#include <variant>

namespace cftf {

static auto SourceRangeLength(clang::SourceManager& sm, clang::SourceRange range) {
    auto begin_data = sm.getCharacterData(range.getBegin());
    auto end_data = sm.getCharacterData(range.getEnd());
    return (end_data - begin_data);
}

static auto SourceRangeToString(clang::SourceManager& sm, clang::SourceRange range) {
    auto begin_data = sm.getCharacterData(range.getBegin());
    return std::string(begin_data, SourceRangeLength(sm, range));
}

static bool IsSubRange(clang::SourceManager& sm, clang::SourceRange inner, clang::SourceRange outer) {
    return (sm.isPointWithin(inner.getBegin(), outer.getBegin(), outer.getEnd()) &&
            sm.isPointWithin(inner.getEnd(), outer.getBegin(), outer.getEnd()));
}

/**
 * Rewriter that takes a copy of the given range and performs manipulations on
 * it based on original SourceLocations but without modifying the original text
 *
 * In contrast to clang::Rewriter this class allows for "hierarchical"
 * rewriting, where multiple rewrite rules might operate on nested parts
 * of a single expressions (e.g. a parameter pack expansion including
 * binary literals).
 */
class HierarchicalRewriter final : public RewriterBase {
public:
    HierarchicalRewriter(clang::SourceManager& sm, clang::SourceRange range)
        : sm(sm), root{range, true, SourceRangeToString(sm, range)} {
    }

    std::string GetContents() const {
        return root.Concatenate();
    }

private:
    struct SourceNode {
        // Half-open interval of source contained by this node. Beginning is included, end is not.
        clang::SourceRange range;

        // true if this is original, unmodified source data; false otherwise.
        // if false, the contents of this child may not be split up during rewrites. In other words,
        // the child must either be left untouched or replaced as a whole
        bool rewriteable;

        bool IsLeaf() const {
            return std::holds_alternative<std::string>(data);
        }

        /// Child nodes or node content
        std::variant<std::vector<SourceNode>, std::string> data;

        std::vector<SourceNode>& GetChildren() {
            return std::get<std::vector<SourceNode>>(data);
        }

        const std::vector<SourceNode>& GetChildren() const {
            return std::get<std::vector<SourceNode>>(data);
        }

        std::string& GetContent() {
            return std::get<std::string>(data);
        }

        std::string Concatenate() const {
            if (IsLeaf()) {
                return std::get<std::string>(data);
            } else {
                return std::accumulate(GetChildren().cbegin(), GetChildren().cend(), std::string{},
                                       [](const std::string& str, const SourceNode& node) {
                                           return str + node.Concatenate();
                                       });
            }
        }
    };

    bool ReplaceTextIncludingEndToken(clang::SourceRange subrange, llvm::StringRef new_str) override {
        clang::SourceRange extended_range {subrange.getBegin(), clang::Lexer::getLocForEndOfToken(subrange.getEnd(), 0, sm, {}) };
        return ReplaceTextExcludingEndToken(extended_range, new_str);
    }

    // TODO: Remove this
    std::string GetHalfOpenStringFor(clang::SourceRange range) {
        auto begin_data = sm.getCharacterData(range.getBegin());
        auto end_data = sm.getCharacterData(range.getEnd());
        return std::string(begin_data, end_data - begin_data);
    }
    bool ReplaceTextExcludingEndToken(SourceNode& node, clang::SourceRange replaced_range, llvm::StringRef new_str) {
        if (node.IsLeaf()) {
            if (node.range == replaced_range) {
                // Node coincides with replaced range, so just replace it directly
                node.rewriteable = false;
                node.data = new_str;
            } else {
                // Split this leaf into (up to) 3 children and replace the inner part
                std::vector<SourceNode> children;

                auto left_range = clang::SourceRange { node.range.getBegin(), replaced_range.getBegin() };
                if (left_range.getBegin() != left_range.getEnd()) {
                    children.emplace_back(SourceNode { left_range, true, GetHalfOpenStringFor(left_range) });
                }

                children.emplace_back(SourceNode { replaced_range, false, new_str });

                auto right_range = clang::SourceRange { replaced_range.getEnd(), node.range.getEnd() };
                if (right_range.getBegin() != right_range.getEnd()) {
                    children.emplace_back(SourceNode { right_range, true, GetHalfOpenStringFor(right_range) });
                }

                // If the node wasn't wholly covered, we should have more than one child now
                assert(children.size() > 1);

                node.data = std::move(children);
            }
        } else {
            // Recurse into the smallest child that wholly covers replaced_range
            auto& children = node.GetChildren();
            auto child_it = std::find_if(children.begin(), children.end(),
                                         [&](SourceNode& child) {
                                             return IsSubRange(sm, replaced_range, child.range);
                                         });
            if (child_it == children.end()) {
                // Not implemented, currently. Not sure if we need this?
                assert(false);
            } else {
                ReplaceTextExcludingEndToken(*child_it, replaced_range, new_str);
            }
        }

        // Report success
        return false;
    }
    bool ReplaceTextExcludingEndToken(clang::SourceRange subrange, llvm::StringRef new_str) override {
        return ReplaceTextExcludingEndToken(root, subrange, new_str);
    }

    clang::SourceManager& getSourceMgr() override {
        return sm;
    }

    clang::SourceManager& sm;

    SourceNode root;
};

/**
 * Utility AST visitor that traverses a function and checks whether there is a need
 * to explicitly specialize it (e.g. because other transformations depend on types being well-known)
 */
class FunctionNeedsExplicitSpecializationChecker : public clang::RecursiveASTVisitor<FunctionNeedsExplicitSpecializationChecker> {
public:
    FunctionNeedsExplicitSpecializationChecker(clang::FunctionDecl* decl) {
        // If this is a function template specialization, continue, otherwise we trivially don't need to specialize this function
        if (decl->getPrimaryTemplate() != nullptr && !decl->getTemplateSpecializationInfo()->isExplicitSpecialization()) {
            TraverseFunctionDecl(decl);
            needs_specialization = true; // TODO: Revert
        }
    }

    operator bool() const {
        return needs_specialization;
    }

    bool VisitIfStmt(clang::IfStmt* stmt) {
        if (features::constexpr_if && stmt->isConstexpr()) {
            needs_specialization = true;
            // Abort traversal for this check
            return false;
        }

        return true;
    }

    bool needs_specialization = false;
};

bool ASTVisitor::VisitFunctionTemplateDecl(clang::FunctionTemplateDecl*) {
    return true;
}

bool ASTVisitor::VisitFunctionDecl(clang::FunctionDecl*) {
    return true;
}

clang::ParmVarDecl* ASTVisitor::CurrentFunctionInfo::FindTemplatedParamDecl(clang::ParmVarDecl* specialized) const {
    auto it = std::find_if(parameters.begin(), parameters.end(),
                           [specialized](const Parameter& param) {
                               auto it = std::find_if(param.specialized.begin(), param.specialized.end(),
                                                      [=](clang::ParmVarDecl* decl) {
                                                          return (decl == specialized);
                                                      });
                               return (it != param.specialized.end());
                           });
    if (it == parameters.end()) {
        return nullptr;
    }

    return it->templated;
}

const std::vector<clang::ParmVarDecl*>& ASTVisitor::CurrentFunctionInfo::FindSpecializedParamDecls(clang::ParmVarDecl* templated) const {
    auto it = std::find_if(parameters.begin(), parameters.end(),
                           [templated](const Parameter& param) {
                               return (param.templated == templated);
                           });
    assert (it != parameters.end());

    return it->specialized;
}

// TODO: Move elsewhere
bool ASTVisitor::VisitSizeOfPackExpr(clang::SizeOfPackExpr* expr) {
    rewriter->ReplaceTextIncludingEndToken({ expr->getLocStart(), expr->getLocEnd() }, std::to_string(expr->getPackLength()));
    return true;
}

bool ASTVisitor::TraverseFunctionDecl(clang::FunctionDecl* decl) {
    decltype(rewriter) old_rewriter;

    std::cerr << "Visiting FunctionDecl:" << std::endl;
    if (decl->getDescribedFunctionTemplate() != nullptr) {
        // This is the actual template definition, which we don't care about:
        // The rest of this function is concerned with explicitly generating
        // what's usually an implicit template specialization. We need to do
        // this for some rewriting rules to be effective, since some things
        // cannot generally be rewritten in a dependent context.
        // TODO: Instead of ignoring this, we should traverse it and gather
        //       a list of things that are "awkward" to rewrite, e.g.
        //       parameter pack expansions. Having this list available later
        //       when creating the explicit specializations will hopefully
        //       be useful.
        return true;
    }

    bool specialize = FunctionNeedsExplicitSpecializationChecker(decl);
    if (specialize && context.getFullLoc(decl->getLocStart()).isInSystemHeader()) {
        // Don't specialize functions from system headers
        specialize = false;
    }

    CurrentFunctionInfo current_function = { decl, {} };
    if (specialize) {
        // Temporarily exchange our clang::Rewriter with an internal rewriter that writes to a copy of the current function (which will act as an explicit instantiation)
        // TODO: This will fail sooner or later; functions can be nested e.g. by declaring a class inside a function!
        assert(old_rewriter == nullptr);
        old_rewriter = std::exchange(rewriter, std::make_unique<HierarchicalRewriter>(rewriter->getSourceMgr(), clang::SourceRange{ decl->getLocStart(), getLocForEndOfToken(decl->getLocEnd()) }));

        // Add template argument list for this specialization
        {
            std::string addendum;
            llvm::raw_string_ostream ss(addendum);
            ss << '<';
            assert(decl->getTemplateSpecializationArgs());
            auto&& template_args = decl->getTemplateSpecializationArgs()->asArray();
            for (auto it = template_args.begin(); it != template_args.end(); ++it) {
                if (it != template_args.begin()) {
                    ss << ", ";
                }
                clang::LangOptions policy; // TODO: Get this from the proper source!
                if (it->getKind() == clang::TemplateArgument::Pack) {
                    // Print each item in the parameter pack individually
                    for (auto pack_it = it->pack_begin(); pack_it < it->pack_end(); ++pack_it) {
                        if (pack_it != it->pack_begin()) {
                            ss << ", ";
                        }
                        pack_it->print(policy, ss);
                    }
                } else {
                    it->print(policy, ss);
                }
            }
            ss << '>';
            ss.flush();
            rewriter->InsertTextAfter(decl->getLocation(), addendum);
        }

        // The template generally contains references to the template parameters (in the body and in the function parameter list).
        // This is a problem in our generated specializations, which don't define the template parameters (i.e. there is no
        // "template<typename T>" preceding them) but must use the actual template arguments instead.
        // We address this as follows:
        // * In the specialization body, we insert type aliases and constants at the top to manually declare template parameters.
        //   This is much easier than trying to manually replace all occurrences of template parameters with concrete arguments.
        // * The parameter list is replaced by the FunctionDecl parameter list provided by clang. Stringifying this correctly
        //   is reasonably easy and gets rid of all template parameter references automatically.
        //
        // NOTE: We only need to replace anything for non-empty parameter lists, but note that a specialization's parameter list
        //       may well be empty while the actual template function's parameter list is not. In particular, this happens for
        //       template functions of the form
        //
        //           template<typename... T> void func(T... t)
        //
        //       when specialized for empty parameter packs.

        auto templated_function_decl = decl->getPrimaryTemplate()->getTemplatedDecl();
        std::transform(templated_function_decl->param_begin(), templated_function_decl->param_end(), std::back_inserter(current_function.parameters),
                       [&](clang::ParmVarDecl* templated_param_decl) {
                            // Unfortunately, there doesn't seem to be a better way to do this than to compare the parameters by name...
                            auto is_same_decl = [&](const clang::ParmVarDecl* specialized_param_decl) {
                                                 return (specialized_param_decl->getName() == templated_param_decl->getName());
                                             };
                            auto first_it = std::find_if (decl->param_begin(), decl->param_end(), is_same_decl);
                            auto last_it = std::find_if_not(first_it, decl->param_end(), is_same_decl);
                            return CurrentFunctionInfo::Parameter { templated_param_decl, std::vector(first_it, last_it) };
                       });


        // Replace template parameters in the specialized signature with the actual template parameters
        if (templated_function_decl->param_size()) {
            std::string addendum;

            clang::SourceLocation parameters_loc_start = templated_function_decl->parameters().front()->getLocStart();
            clang::SourceLocation parameters_loc_end = templated_function_decl->parameters().back()->getLocEnd();
            bool first_printed_parameter = true;

            for (auto parameter_it = decl->param_begin(); parameter_it != decl->param_end();) {
                // Check if we are in a block of parameters generated from a parameter pack,
                // and if we are process the entire block at once
                size_t parameters_in_current_pack = 1;
                auto templated_parameter = current_function.FindTemplatedParamDecl(*parameter_it);
                if (templated_parameter && templated_parameter->isParameterPack()) {
                    // TODO: Change API to return the Parameter directly so we don't need to do this utterly irrelevant lookup
                    auto generated_parameters = current_function.FindSpecializedParamDecls(templated_parameter);
                    parameters_in_current_pack = generated_parameters.size();
                }
                const auto parameter_pack_begin_it = parameter_it;
                const auto parameter_pack_end_it   = parameter_it + parameters_in_current_pack;

                for (; parameter_it != parameter_pack_end_it; ++parameter_it) {
                    assert(parameter_it != decl->param_end());
                    auto* parameter = *parameter_it;

                    if (!first_printed_parameter) {
                        addendum += ", ";
                    } else {
                        first_printed_parameter = false;
                    }
                    // TODO: For on-the-fly declared template arguments like e.g. in "func<struct unnamed>()", getAsString will print spam such as "struct(anonymous namespace)::unnamed". We neither want that namespace nor do we want the "struct" prefix!
                    // NOTE: CppInsights has a lot more code to handle getting the parameter name and type...
                    addendum += parameter->getType().getAsString();
                    if (!parameter->getNameAsString().empty()) {
                        addendum += " ";
                        addendum += parameter->getNameAsString();

                        // Parameter generated from a parameter pack will be assigned the same name,
                        // so we need to distinguish the generated parameter names manually.

                        if (templated_parameter->isParameterPack()) {
                            // TODO: This will break if there is already a parameter with the new name
                            addendum += std::to_string(1 + std::distance(parameter_pack_begin_it, parameter_it));
                        }
                    }
                }
            }
            rewriter->ReplaceTextIncludingEndToken({ parameters_loc_start, parameters_loc_end }, addendum);
        }
    }

    // From here on below, assume we have a self-contained definition that we can freely rewrite code in
    this->current_function = current_function;

    Parent::TraverseFunctionDecl(decl);

    this->current_function = std::nullopt;

    if (specialize) {
        // Fix up references to template parameters in the specialization by adding an explicit
        // declaration of them at the top of the specialization body
        assert(decl->getPrimaryTemplate());
        auto template_parameters = decl->getPrimaryTemplate()->getTemplateParameters();
        auto specialization_args = decl->getTemplateSpecializationArgs()->asArray();
        assert(template_parameters);
        assert(template_parameters->size() == specialization_args.size());
        std::string aliases = "\n";
        auto parameter_it = template_parameters->begin();
        auto argument_it = specialization_args.begin();
        for (;parameter_it != template_parameters->end(); ++parameter_it, ++argument_it) {
            auto& parameter = *parameter_it;
            auto& argument = *argument_it;
            assert(parameter);

            if (parameter->getNameAsString().empty()) {
                // If the parameter was never named, we don't need to reexport it
                continue;
            }

            switch (argument.getKind()) {
            case clang::TemplateArgument::Type:
                // e.g. template<typename Type>
                aliases += "using " + parameter->getNameAsString() + " = " + argument.getAsType().getAsString() + ";\n";
                break;

            case clang::TemplateArgument::Integral:
                // e.g. template<int Val>
                // TODO: Get the actual (possibly const-qualified) type!
                aliases += "auto " + parameter->getNameAsString() + " = " + argument.getAsIntegral().toString(10) + ";\n";
                break;

            case clang::TemplateArgument::Declaration:
                // e.g. template<void* Ptr> with Ptr=&some_global_variable
                std::cerr << "WARNING: TemplateArgument::Declaration not unsupported, yet" << std::endl;
                aliases += "TODO " + parameter->getNameAsString() + " = TODO;\n";
                break;

            case clang::TemplateArgument::NullPtr:
                // e.g. template<void* Ptr> with Ptr=nullptr
                aliases += "decltype(nullptr) " + parameter->getNameAsString() + " = nullptr;\n";
                break;

            case clang::TemplateArgument::Template:
                // e.g. template<template<typename> Templ>
                // TODO: How should we handle these? Function bodies can't include templates!
                // TODO: Instead of ignoring this error, abort specializing this template
                std::cerr << "WARNING: Template template parameters unsupported" << std::endl;
                aliases += "TODO template<typename> " + parameter->getNameAsString() + " = TODO;\n";
                break;

            case clang::TemplateArgument::Pack:
                // e.g. template<typename... Types>
                // e.g. template<int... Vals>
                // e.g. template<template<typename>... Templs>

                // We don't need to do anything here, since we expand all parameter packs in the function body
                std::cerr << "WARNING: Variadic templates support is incomplete" << std::endl;
                break;

            default:
                std::cerr << "WARNING: Unsupported template argument type: " << static_cast<int>(argument.getKind()) << std::endl;
                aliases += "TODO " + parameter->getNameAsString() + " = TODO;\n";
                assert(false);
                break;
            }
        }
        rewriter->InsertTextAfter(decl->getBody()->getLocStart(), aliases);
        std::swap(rewriter, old_rewriter);
        std::string content = static_cast<HierarchicalRewriter*>(old_rewriter.get())->GetContents();
        rewriter->InsertTextAfter(decl->getLocEnd(), "\n\n// Specialization generated by CFTF\ntemplate<>\n" + content);
    }

    // TODO: When we're done with the last specialization of this function, remove the original template function definition!

    return true;
}

} // namespace cftf
