#include "ast_visitor.hpp"
#include "rewriter.hpp"

#include <clang/Lex/Lexer.h>
#include <clang/AST/Expr.h>

#include <iostream>

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
    auto inner_begin = sm.getCharacterData(inner.getBegin());
    auto outer_begin = sm.getCharacterData(outer.getBegin());
    auto inner_end = sm.getCharacterData(inner.getEnd());
    auto outer_end = sm.getCharacterData(outer.getEnd());
    return (inner_begin >= outer_begin && inner_end <= outer_end);
}

/**
 * Rewriter that takes a copy of the given range and performs manipulations on
 * it based on original SourceLocations but without modifying the original text
 *
 */
class StagingRewriter final : public RewriterBase {
public:
    StagingRewriter(clang::SourceManager& sm, clang::SourceRange range)
        : sm(sm), unmodified_contents({std::make_pair(std::string::size_type{0}, range)}), content(SourceRangeToString(sm, range)) {
    }

    const std::string& GetContents() {
        return content;
    }

private:
    // TODO: Double-check this includes the end token or not
    bool ReplaceTextIncludingEndToken(clang::SourceRange subrange, llvm::StringRef new_str) override {
        clang::SourceRange extended_range {subrange.getBegin(), clang::Lexer::getLocForEndOfToken(subrange.getEnd(), 0, sm, {}) };
        return ReplaceTextExcludingEndToken(extended_range, new_str);
    }

    // TODO: Remove these
std::string GetClosedStringFor(clang::SourceLocation begin, clang::SourceLocation end) {
    auto begin_data = sm.getCharacterData(begin);
    auto end_data = sm.getCharacterData(clang::Lexer::getLocForEndOfToken(end, 0, sm, {}));
    return std::string(begin_data, end_data - begin_data);
}
std::string GetClosedStringFor(clang::SourceRange range) {
    auto begin_data = sm.getCharacterData(range.getBegin());
    auto end_data = sm.getCharacterData(clang::Lexer::getLocForEndOfToken(range.getEnd(), 0, sm, {}));
    return std::string(begin_data, end_data - begin_data);
}
    bool ReplaceTextExcludingEndToken(clang::SourceRange subrange, llvm::StringRef new_str) override {
        std::cerr << "Attempting to replace subrange " << "\"" << GetClosedStringFor(subrange.getBegin(), subrange.getEnd()) << "\"" << std::endl;;
        auto range_it = std::find_if(unmodified_contents.begin(), unmodified_contents.end(),
                     [&](const auto& offset_and_range) {
                         auto& [offset, range] = offset_and_range;
                         return IsSubRange(sm, subrange, range);
                     });
        if (range_it == unmodified_contents.end()) {
            assert(false);

            // Report error: Tried to replace an already modified range (or a range that didn't lie within the original range to begin with)
            return true;
        }

        auto range = *range_it;

        auto subrange_length = SourceRangeLength(sm, subrange);
        for (auto follower_it = std::prev(unmodified_contents.end()); follower_it != range_it; --follower_it) {
            if (new_str.size() == subrange_length) {
                break;
            }

            // Relocate subsequent elements by TODO
            auto new_offset = follower_it->first + new_str.size() - subrange_length;
            assert(unmodified_contents.count(new_offset) == 0);
            unmodified_contents.emplace(new_offset, follower_it->second);
            follower_it = unmodified_contents.erase(follower_it);
        }
        unmodified_contents.erase(range_it);

        std::cerr << "Replacing in \"" << content << "\": \""
                  << content.substr(range.first + SourceRangeLength(sm, { range.second.getBegin(), subrange.getBegin() }), subrange_length)
                  << "\"-> \"" << new_str.str() << "\"" << std::endl << std::endl << std::endl << std::endl;
        content.replace(range.first + SourceRangeLength(sm, { range.second.getBegin(), subrange.getBegin() }), subrange_length, new_str);

        // Begin of previously unmodified range - begin of replaced range
        clang::SourceRange pre_subrange { range.second.getBegin(), subrange.getBegin() };
        if (pre_subrange.getBegin() != pre_subrange.getEnd()) {
            auto offset = range.first;
            assert(offset < content.size());
            unmodified_contents.emplace(offset, pre_subrange);
        }

        // End of replaced range - begin of previously unmodified range
        clang::SourceRange post_subrange { subrange.getEnd(), range.second.getEnd() };
        if (post_subrange.getBegin() != post_subrange.getEnd()) {
            auto offset = range.first + SourceRangeLength(sm, { range.second.getBegin(), subrange.getBegin() }) + new_str.size();
            assert(offset < content.size());
            unmodified_contents.emplace(offset, post_subrange);
        }

        return false;
    }

    clang::SourceManager& getSourceMgr() override {
        return sm;
    }

    clang::SourceManager& sm;

    /**
     * Map from offsets into the content string to unmodified original ranges.
     */
    std::map<std::string::size_type, clang::SourceRange> unmodified_contents;

    // TODO: Might want to use a different data structure here. Maybe clang::RewriteRope?
    std::string content;
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

bool ASTVisitor::TraverseFunctionDecl(clang::FunctionDecl* decl) {
    decltype(rewriter) old_rewriter;

    std::cerr << "Visiting FunctionDecl:" << std::endl;
    if (decl->getDescribedFunctionTemplate() != nullptr) {
        // This is the actual template definition, which we don't care about:
        // The rest of this function is concerned with explicitly generating
        // what's usually an implicit template specialization. We need to do
        // this for some rewriting rules to be effective, since some things
        // cannot generally be rewritten in a dependent context.
        return true;
    }

    bool specialize = FunctionNeedsExplicitSpecializationChecker(decl);
    if (specialize && context.getFullLoc(decl->getLocStart()).isInSystemHeader()) {
        // Don't specialize functions from system headers
        specialize = false;
    }
    if (specialize) {
        // Temporarily exchange our clang::Rewriter with an internal rewriter that writes to a copy of the current function (which will act as an explicit instantiation)
        // TODO: This will fail sooner or later; functions can be nested e.g. by declaring a class inside a function!
        assert(old_rewriter == nullptr);
        old_rewriter = std::exchange(rewriter, std::make_unique<StagingRewriter>(rewriter->getSourceMgr(), clang::SourceRange{ decl->getLocStart(), getLocForEndOfToken(decl->getLocEnd()) }));

        // Add template argument list for this specialization
        {
            std::string addendum = "<";
            assert(decl->getTemplateSpecializationArgs());
            auto&& template_args = decl->getTemplateSpecializationArgs()->asArray();
            for (auto it = template_args.begin(); it != template_args.end(); ++it) {
                if (it != template_args.begin()) {
                    addendum += ", ";
                }
                std::string str;
                llvm::raw_string_ostream ss(str);
                clang::LangOptions policy; // TODO: Get this from the proper source!
                // TODO: *it might be a parameter pack, which is printed with "<>" on the outside!
                it->print(policy, ss);
                ss.flush();
                addendum += ss.str();
            }
            addendum += '>';
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
        if (decl->param_size()) {
            std::string addendum;

            clang::SourceLocation parameters_loc_start;
            clang::SourceLocation parameters_loc_end;
            bool first = true;
            for (auto* parameter : decl->parameters()) {
                if (!first) {
                    addendum += ", ";
                } else {
                    parameters_loc_start = parameter->getLocStart();
                    first = false;
                }
                // TODO: For on-the-fly declared template arguments like e.g. in "func<struct unnamed>()", this will print spam such as "struct(anonymous namespace)::unnamed". We neither want that namespace nor do we want the "struct" prefix!
                // TODO: For parameter packs, this doesn't seem to include the "..." at the end!
                parameters_loc_end = parameter->getLocEnd();

                // TODO: Need to ensure names of parameter pack types are unique! (e.g. "T... val" may get expanded to "int val, int val", currently)

                // TODO: CppInsights has a lot more code to handle getting the parameter name and type...
                addendum += parameter->getType().getAsString();
                addendum += " ";
                addendum += parameter->getNameAsString();
            }
            rewriter->ReplaceTextIncludingEndToken({ parameters_loc_start, parameters_loc_end }, addendum);
        }
    }

    // From here on below, assume we have a self-contained definition that we can freely rewrite code in
    in_fully_specialized_function = true; // TODO: Reset this properly!
    current_function = decl;

    Parent::TraverseFunctionDecl(decl);

    current_function = nullptr;
    in_fully_specialized_function = false;

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
                // TODO: We need to manually expand parameter packs in the function body, which we don't support yet.
                // TODO: Instead of ignoring this error, abort specializing this template
                aliases += "TODO... " + parameter->getNameAsString() + " = TODO...;\n";
                std::cerr << "WARNING: Parameter packs unsupported" << std::endl;
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
        rewriter->InsertTextAfter(decl->getLocEnd(), "\n\n// Specialization generated by CFTF\ntemplate<>\n" + static_cast<StagingRewriter*>(old_rewriter.get())->GetContents());
    }

    // TODO: When we're done with the last specialization of this function, remove the original template function definition!

    return true;
}

} // namespace cftf
