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
 *
 * This rewriter also supports copy-operations ("instances") such that
 * consecutive edits of common subexpressions are visible in all instances
 * while allowing to do individual edits as well.
 *
 * For example, when specializing the expression
 *     "my_function((0b0010 * ts)...)"
 * for ts=<5, 10, 'c'>, the following operations may be performed:
 * a) The parameter pack expansion rules creates three instances of the
 *    subexpression "(0b1000 * ts)..."
 * b) The separator ", " is added to the first two instances
 * c) The DeclRefExpr matcher replaces "ts" in each instance with a unique
 *    numbered identifier (ts1, ts2, ts3)
 * d) The IntegerLiteral matcher replaces 0b0010 with 2 in all instances
 * The difference between (c) and (d) is that (c) edits each instance
 * separately whereas in (d), the rewriter class automatically distributes the
 * edit across all instances.
 * The resulting generated source code is
 *     "my_function((2 * ts1), (2 * ts2), (2 * ts3))".
 */
class HierarchicalRewriter final : public RewriterBase {
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

        size_t node_id = 0;
        size_t instance_id = 0;

        static constexpr size_t all_instances = std::numeric_limits<size_t>::max();

        bool operator==(const SourceNode& oth) const {
            return range == oth.range && rewriteable == oth.rewriteable && data == oth.data && instance_id == oth.instance_id;
        }
    };

    size_t running_node_id = 1000;

public:
    struct InstanceHandle {
    private:
        InstanceHandle(SourceNode& node, SourceNode& parent) : node_id(node.node_id), parent_id(parent.node_id) {}
        InstanceHandle(size_t node_id, size_t parent_id) : node_id(node_id), parent_id(parent_id) {}

        // TODO: To support nested instances, these will likely need to be std::vectors of nodes in the future
        size_t node_id;
        size_t parent_id;

        friend class HierarchicalRewriter;
    };

    HierarchicalRewriter(clang::SourceManager& sm, clang::SourceRange range)
        : sm(sm), root{range, true, SourceRangeToString(sm, range)} {
    }

    std::string GetContents() const {
        return root.Concatenate();
    }

    InstanceHandle MakeInstanceHandle(clang::SourceRange subrange) {
        return MakeInstanceHandle(root, subrange);
    }

    SourceNode& FindNode(SourceNode& parent, size_t id) {
        auto ptr = FindNodeHelper(parent, id);
        assert(ptr);
        return *ptr;
    }

    InstanceHandle CreateNewInstance(const InstanceHandle& base_instance) {
        auto& parent = FindNode(root, base_instance.parent_id);
        auto node = FindNode(parent, base_instance.node_id); // Copy intended
        auto& children = parent.GetChildren();
        auto node_it = std::find(children.begin(), children.end(), node);

        // Find the most recent instance of this node, and insert a new instance after it
        while (std::next(node_it) != children.end() && node_it->instance_id < std::next(node_it)->instance_id) {
            ++node_it;
        }

        auto new_instance_id = 1 + node_it->instance_id;
        auto new_instance = children.insert(node_it + 1, node);
        new_instance->instance_id = new_instance_id;
        new_instance->node_id = ++running_node_id;
        return InstanceHandle { new_instance->node_id, base_instance.parent_id };
    }

    bool ReplaceTextExcludingEndToken(InstanceHandle& instance, clang::SourceRange replaced_range, llvm::StringRef new_str) {
        return ReplaceTextExcludingEndToken(FindNode(root, instance.node_id), replaced_range, new_str);
    }

private:
    SourceNode* FindNodeHelper(SourceNode& parent, size_t id) {
        if (parent.node_id == id) {
            return &parent;
        }

        if (parent.IsLeaf()) {
            return nullptr;
        } else {
            for (auto& child : parent.GetChildren()) {
                auto ptr = FindNodeHelper(child, id);
                if (ptr) return ptr;
            }
            return nullptr;
        }
    }

    // Returns a reference to the inner node
    SourceNode& SplitNodeAt(SourceNode& node, clang::SourceRange subrange) {
        std::vector<SourceNode> children;
        size_t index_for_inner_node = 0;

        auto left_range = clang::SourceRange { node.range.getBegin(), subrange.getBegin() };
        if (left_range.getBegin() != left_range.getEnd()) {
            children.emplace_back(SourceNode { left_range, true, GetHalfOpenStringFor(left_range), ++running_node_id });
            index_for_inner_node = 1;
        }

        children.emplace_back(SourceNode { subrange, true, GetHalfOpenStringFor(subrange), ++running_node_id });

        auto right_range = clang::SourceRange { subrange.getEnd(), node.range.getEnd() };
        if (right_range.getBegin() != right_range.getEnd()) {
            children.emplace_back(SourceNode { right_range, true, GetHalfOpenStringFor(right_range), ++running_node_id });
        }

        // If the node wasn't wholly covered, we should have more than one child now
        assert(children.size() > 1);

        node.data = std::move(children);

        return node.GetChildren()[index_for_inner_node];
    }

    bool ReplaceTextIncludingEndToken(clang::SourceRange subrange, llvm::StringRef new_str) override {
        clang::SourceRange extended_range {subrange.getBegin(), clang::Lexer::getLocForEndOfToken(subrange.getEnd(), 0, sm, {}) };
        return ReplaceTextExcludingEndToken(extended_range, new_str);
    }

    // TODO: Remove this
public:
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
                auto& inner_node = SplitNodeAt(node, replaced_range);
                inner_node.rewriteable = false;
                inner_node.data = new_str;
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
                // No child contains the entire SourceRange found, so we'll replace all the children in this node that are covered.
                auto child_is_fully_covered = [&](SourceNode& child) {
                    bool fully_covered = IsSubRange(sm, child.range, replaced_range);
                    return fully_covered;
                };

                auto first_child = std::find_if(children.begin(), children.end(), child_is_fully_covered);

                // If replacement string is non-empty, replace the first matching child in-place and drop all other children.
                // Otherwise, drop all matching children
                auto first_child_to_remove = new_str.empty() ? first_child : (first_child + 1);
                auto children_to_remove = std::remove_if(first_child_to_remove, children.end(), child_is_fully_covered);

                if (!new_str.empty()) {
                    clang::SourceLocation range_end = (children_to_remove != children.end()) ? children.back().range.getEnd() : first_child->range.getEnd();
                    *first_child = SourceNode { { first_child->range.getBegin(), range_end }, false, new_str, ++running_node_id };
                }

                children.erase(children_to_remove, children.end());
            } else {
                // Recurse into the child (each instance separately)
                size_t instance = 0;
                auto instance_it = child_it;
                while (instance_it != children.end() && instance_it->instance_id == instance) {
                    ReplaceTextExcludingEndToken(*child_it, replaced_range, new_str);
                    ++instance;
                    ++instance_it;
                }

                // We should have reached either the end of children or the start of another block of instances
                assert(instance_it == children.end() || instance_it->instance_id == 0);
            }
        }

        // Report success
        return false;
    }
    bool ReplaceTextExcludingEndToken(clang::SourceRange subrange, llvm::StringRef new_str) override {
        return ReplaceTextExcludingEndToken(root, subrange, new_str);
    }

    InstanceHandle MakeInstanceHandle(SourceNode& parent, clang::SourceRange subrange) {
        if (parent.IsLeaf()) {
            if (parent.range == subrange) {
                // Bleh, generate a nested child, just so we don't need to look up the proper parent ourselves now...
                SourceNode child = std::move(parent);
                parent = SourceNode { child.range, child.rewriteable, std::vector(1, child), ++running_node_id };
                return InstanceHandle { parent.GetChildren()[0], parent };
            } else {
                auto& inner_node = SplitNodeAt(parent, subrange);
                return InstanceHandle { inner_node, parent };
            }
        } else {
            auto& children = parent.GetChildren();
            auto child_it = std::find_if(children.begin(), children.end(),
                                        [&](SourceNode& child) {
                                            return IsSubRange(sm, subrange, child.range);
                                        });
            if (child_it == children.end()) {
                // Not implemented, currently. Not sure if we need this?
                assert(false);
                throw nullptr;
            } else {
                // Recurse into the child

                // TODO: Support nested instances.
                //       We will need to capture all different branches that
                //       can reach the given subrange in the InstanceHandle
                //       for this!
                assert(std::next(child_it) == children.end() || std::next(child_it)->instance_id == 0);

                return MakeInstanceHandle(*child_it, subrange);
            }
        }
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

/**
 * Utility AST visitor that determines the size of the parameter pack expanded
 * by the given PackExpansionExpr based on an implicitly specialized
 * FunctionDecl.
 *
 * This helper is provided because libclang provides no direct means of getting
 * the size of a parameter pack used for a specialization of a variadic
 * function template.
 *
 * Internally, this helper traverses the entire function (up to the point of
 * parameter pack expansion) to find the immediate children of expansion_expr
 * in the function body and to count the number of their appearances.
 * When using this helper, be cautious about the performance implications of
 * this full traversal.
 *
 * @note One might be tempted to assume we could just calculate the parameter
 *       pack size as the difference between the total number of arguments used
 *       for the current specialization and the number of non-variadic template
 *       parameters. E.g. for "template<typename T, typename... Us> void f()"
 *       and the specialization "f<int, char, char>", this would yield the
 *       correct value 3 - 1 = 2.
 *       However, that breaks e.g. for function templates like
 *       "template<typename... Ts, typename... Us> void f(Us... u)".
 *       Maybe this approach could work with less naive inference rules, but
 *       I haven't further explored that idea.
 *
 * @note We also can't get this info from
 *       FunctionDecl::getTemplateSpecializationInfo (which provides a
 *       template argument list), since we don't know the expanded parameter
 *       pack. We might get away with just taking any parameter pack contained
 *       within the expanded expression, but there are contrived (and evil)
 *       examples of referencing multiple parameter packs in the same
 *       expansion.
 *       That said, maybe this could be used as a faster default, and the full
 *       function traversal could be used as a reliable fallback for contrived
 *       examples.
 */
class DetermineParameterPackSizeVisitor : public clang::RecursiveASTVisitor<DetermineParameterPackSizeVisitor> {
public:
    DetermineParameterPackSizeVisitor(clang::FunctionDecl* decl, clang::PackExpansionExpr* expansion_expr) : expr(expansion_expr->getPattern()) {
        TraverseFunctionDecl(decl);
    }

    operator size_t() const {
        return count;
    }

    bool VisitStmt(clang::Stmt* stmt) {
        // Compared for equality based on StmtClass and SourceLocations
        // Find the first statement that was generated from the parameter pack expansion.
        // We recognize this statement by comparing against the StmtClass and source location
        auto is_generated_stmt = [this](clang::Stmt* candidate) {
            auto expected_stmt_class = expr->getStmtClass();

            if (clang::CXXUnresolvedConstructExpr::classof(expr) && clang::CXXFunctionalCastExpr::classof(candidate)) {
                // CXXUnresolvedConstructExprs get turned into
                // CXXFunctionalCastExprsGenerated in implicit specializations.
                // The SourceRange doesn't change, so we can still use it for
                // the purpose of comparison.
                // This was observed e.g. in "func(T{}...)".

                expected_stmt_class = candidate->getStmtClass();
            } else if (clang::ImplicitCastExpr::classof(candidate)) {
                // Generated expressions are often wrapped in a generated
                // ImplicitCastExpr, so unfold that one by refering to the
                // child instead.
                // In particular, this occurs in expressions like "func((t)...)"

                // There should only be one child in this expression
                assert(std::distance(candidate->child_begin(), candidate->child_end()) == 1);

                candidate = *candidate->child_begin();
            }

            return  candidate->getStmtClass() == expected_stmt_class &&
                    candidate->getSourceRange() == expr->getSourceRange();
        };
        auto count = std::count_if(stmt->child_begin(), stmt->child_end(), is_generated_stmt);
        if (count) {
            this->count = count;
            // Abort traversal if we found the expression (TODO: Does this abort the entire traversal or just move back to parent? Abort the entire thing if the latter!)
            return false;
        } else {
            // Keep looking for a generated statement
            // NOTE: If the parameter pack was empty, this algorithm will need to scan the entire function to detect that :/
            return true;
        }
    }

    clang::Expr* expr; // Pattern of the given PackExpansionExpr
    size_t count = 0;
};

clang::ParmVarDecl* ASTVisitor::CurrentFunctionInfo::FindTemplatedParamDecl(clang::ParmVarDecl* specialized) const {
    auto it = std::find_if(parameters.begin(), parameters.end(),
                           [specialized](const Parameter& param) {
                               auto it = std::find_if(param.specialized.begin(), param.specialized.end(),
                                                      [=](const Parameter::SpecializedParameter& parameter) {
                                                          return (parameter.decl == specialized);
                                                      });
                               return (it != param.specialized.end());
                           });
    if (it == parameters.end()) {
        return nullptr;
    }

    return it->templated;
}

const std::vector<ASTVisitor::CurrentFunctionInfo::Parameter::SpecializedParameter>& ASTVisitor::CurrentFunctionInfo::FindSpecializedParamDecls(clang::ParmVarDecl* templated) const {
    auto it = std::find_if(parameters.begin(), parameters.end(),
                           [templated](const Parameter& param) {
                               return (param.templated == templated);
                           });
    assert (it != parameters.end());

    return it->specialized;
}

// TODO: Move elsewhere
bool ASTVisitor::VisitSizeOfPackExpr(clang::SizeOfPackExpr* expr) {
    if (!current_function) {
        return true;
    }
    rewriter->ReplaceTextIncludingEndToken({ expr->getLocStart(), expr->getLocEnd() }, "/*" + GetClosedStringFor(expr->getLocStart(), expr->getLocEnd()) + "*/" + std::to_string(expr->getPackLength()));
    return true;
}

bool ASTVisitor::VisitPackExpansionExpr(clang::PackExpansionExpr* expr) {
    if (!current_function_template)
        return true;


    // NOTE: We only ever visit this once, in the general template. So we need to iterate over all implicit specializations of this function and fill in the gaps ourselves later.
    current_function_template->param_pack_expansions.push_back(FunctionTemplateInfo::ParamPackExpansionInfo{expr, std::vector<clang::DeclRefExpr*>{}});
    std::cerr << "Visiting pack expansion, registering to " << current_function_template << std::endl;

    return true;
}

bool ASTVisitor::TraversePackExpansionExpr(clang::PackExpansionExpr* expr) {
    if (!current_function_template)
        return true;

    assert(!current_function_template->in_param_pack_expansion);
    current_function_template->in_param_pack_expansion = true;
    Parent::TraversePackExpansionExpr(expr);
    current_function_template->in_param_pack_expansion = false;
    return true;
}

bool ASTVisitor::VisitDeclRefExpr(clang::DeclRefExpr* expr) {
    // Record uses of parameter packs within pack expansions

    if (!current_function_template || !current_function_template->in_param_pack_expansion) {
        return true;
    }

    auto parm_var_decl = clang::dyn_cast<clang::ParmVarDecl>(expr->getDecl());
    if (parm_var_decl && parm_var_decl->isParameterPack()) {
        current_function_template->param_pack_expansions.back().referenced_packs.push_back(expr);
    }
    return true;
}

static std::string MakeUniqueParameterPackName(clang::ParmVarDecl* decl, size_t index) {
    // Just append a 1-based counter for now
    // TODO: This will break if there is already a parameter with the new name
    return decl->getNameAsString() + std::to_string(1 + index);
}

bool ASTVisitor::TraverseFunctionTemplateDecl(clang::FunctionTemplateDecl* decl) {
    WalkUpFromFunctionTemplateDecl(decl);

    std::cerr << "Visiting FunctionTemplateDecl:" << decl << std::endl;
    auto templated_decl = decl->getTemplatedDecl();
    {
        // This is the actual template definition (i.e. not one of the
        // specializations generated implicitly by clang). We do a prepass over
        // the template definition to gather a list of things that would be
        // difficult to rewrite otherwise, such as parameter pack expansions.

        auto [it, ignored] = function_templates.emplace(templated_decl, FunctionTemplateInfo{});
        current_function_template = &it->second;
        std::cerr << "Template: " << templated_decl->getNameAsString() << std::endl;

        // TODO: Will we traverse this decl twice now?
        Parent::TraverseFunctionDecl(templated_decl);

        current_function_template = nullptr;

    }

    // The rest of this function is concerned with generating explicit
    // specializations from what's an implicit template specialization in
    // libclang's AST. Hence, return early from this code path.

    for (auto* specialized_decl : decl->specializations()) {
        std::cerr << "Specialization " << specialized_decl << std::endl;

        bool specialize = FunctionNeedsExplicitSpecializationChecker(specialized_decl);
        if (specialize && context.getFullLoc(specialized_decl->getLocStart()).isInSystemHeader()) {
            // Don't specialize functions from system headers
            specialize = false;
        }

        decltype(rewriter) old_rewriter;

        CurrentFunctionInfo current_function = { specialized_decl, {} };
        if (specialize) {
            current_function.template_info = &function_templates.at(templated_decl);

            // Temporarily exchange our clang::Rewriter with an internal rewriter that writes to a copy of the current function (which will act as an explicit instantiation)
            // TODO: This will fail sooner or later; functions can be nested e.g. by declaring a class inside a function!
            // TODO: Should probably use the locations from templated_decl instead!
            old_rewriter = std::exchange(rewriter, std::make_unique<HierarchicalRewriter>(rewriter->getSourceMgr(), clang::SourceRange{ specialized_decl->getLocStart(), getLocForEndOfToken(specialized_decl->getLocEnd()) }));

            // Add template argument list for this specialization
            {
                std::string addendum;
                llvm::raw_string_ostream ss(addendum);
                ss << '<';
                assert(specialized_decl->getTemplateSpecializationArgs());
                auto&& template_args = specialized_decl->getTemplateSpecializationArgs()->asArray();
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
                // TODO: Templated_decl locs!
                rewriter->InsertTextAfter(specialized_decl->getLocation(), addendum);
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

            std::transform(templated_decl->param_begin(), templated_decl->param_end(), std::back_inserter(current_function.parameters),
                        [&](clang::ParmVarDecl* templated_param_decl) {
                                // TODO: Unify this with FindTemplatedDecl!
                                auto is_same_decl = [&](const clang::ParmVarDecl* specialized_param_decl) {
                                                    // Unfortunately, there doesn't seem to be a better way to do this than to compare the parameters by name...
                                                    return (specialized_param_decl->getName() == templated_param_decl->getName());
                                                };
                                auto first_it = std::find_if (specialized_decl->param_begin(), specialized_decl->param_end(), is_same_decl);
                                auto last_it = std::find_if_not(first_it, specialized_decl->param_end(), is_same_decl);

                                CurrentFunctionInfo::Parameter ret { templated_param_decl, {} };

                                if (first_it + 1 == last_it) {
                                    // Just one argument
                                    ret.specialized.push_back({*first_it, templated_param_decl->getNameAsString()});
                                } else {
                                    // Templated parameter refers to a parameter pack for which multiple (or none) arguments were generated;
                                    // to prevent name collisions, generate a unique name for each of them.
                                    for (auto it = first_it; it != last_it; ++it) {
                                        std::string unique_name = MakeUniqueParameterPackName(templated_param_decl, std::distance(first_it, it));
                                        ret.specialized.push_back({*it, std::move(unique_name)});
                                    }
                                }

                                return ret;
                        });


            // Remove empty parameter packs from the specialized signature
            // (Non-empty parameter packs are handled in VisitVarDecl)
            for (auto templated_parameter_it = templated_decl->param_begin();
                    templated_parameter_it != templated_decl->param_end();
                    ++templated_parameter_it) {
                auto* templated_parameter = *templated_parameter_it;
                if (templated_parameter->isParameterPack() && current_function.FindSpecializedParamDecls(templated_parameter).empty()) {
                    const bool is_first_parameter = (templated_parameter_it == templated_decl->param_end());
                    const bool is_last_parameter = (std::next(templated_parameter_it) == templated_decl->param_end());

                    // Remove the parameter (including any preceding or following commas)
                    clang::SourceLocation start_loc = is_first_parameter ? templated_decl->parameters().front()->getLocStart() : templated_parameter->getLocStart();
                    if (is_last_parameter) {
                        // Delete up to the end of the function signature
                        clang::SourceLocation end_loc = templated_decl->parameters().back()->getLocEnd();
                        rewriter->ReplaceTextIncludingEndToken({start_loc, end_loc}, "");
                    } else {
                        // Delete up to the beginning of the next parameter
                        auto end_loc = (*std::next(templated_parameter_it))->getLocStart();
                        rewriter->ReplaceTextExcludingEndToken({start_loc, end_loc}, "");
                    }
                }
            }
        }

        // From here on below, assume we have a self-contained definition that we can freely rewrite code in
        this->current_function = current_function;

        // Patch up body (parameter pack expansions, fold expressions)
        /*if (decl2->getPrimaryTemplate())*/ {
            auto current_function_template_it = function_templates.find(templated_decl);
            if (current_function_template_it != function_templates.end()) {
                auto current_function_template = &current_function_template_it->second;
                assert(current_function_template);

                for (auto [pack_expansion_expr, pack_uses] : current_function_template->param_pack_expansions) {
                    auto rewriter = static_cast<HierarchicalRewriter*>(this->rewriter.get());
                    auto* pattern = pack_expansion_expr->getPattern();
                    auto range_end = clang::Lexer::getLocForEndOfToken(pack_expansion_expr->getEllipsisLoc(), 0, rewriter->getSourceMgr(), {});
                    auto base_instance = rewriter->MakeInstanceHandle({pattern->getLocStart(), range_end});

                    size_t pack_length = DetermineParameterPackSizeVisitor { specialized_decl, pack_expansion_expr };

                    std::vector<HierarchicalRewriter::InstanceHandle> instances;
                    for (size_t instance_id = 0; instance_id < pack_length; ++instance_id) {
                        instances.push_back(rewriter->CreateNewInstance(base_instance));
                    }

                    // Delete the original pack expansion first, then re-add one copy for each parameter pack element
                    rewriter->ReplaceTextExcludingEndToken(base_instance, {pattern->getLocStart(), range_end}, "/*" + GetClosedStringFor(pattern->getLocStart(), range_end) + " of size " + std::to_string(pack_length) + "*/");

                    for (size_t instance_id = 0; instance_id < pack_length; ++instance_id) {
                        // Insert separators for all but the last instance
                        const char* replacement = ", ";
                        if (instance_id == pack_length - 1) {
                            // No separator needed, so just remove the ellipsis
                            replacement = "";
                        }
                        rewriter->ReplaceTextExcludingEndToken(instances[instance_id], {pack_expansion_expr->getEllipsisLoc(), range_end}, replacement);

                        // We generate unique names for function parameters
                        // expanded from parameter packs. Those now need to be
                        // patched into the function body whenever the parameter
                        // pack is referenced.
                        for (auto* pack_expr : pack_uses) {
                            clang::SourceRange range = { pack_expr->getLocStart(), clang::Lexer::getLocForEndOfToken(pack_expr->getLocEnd(), 0, rewriter->getSourceMgr(), {}) };
                            auto parm_var_decl = clang::dyn_cast<clang::ParmVarDecl>(pack_expr->getDecl());
                            assert(parm_var_decl && parm_var_decl->isParameterPack());

                            const auto& unique_name = current_function.FindSpecializedParamDecls(parm_var_decl)[instance_id].unique_name;
                            rewriter->ReplaceTextExcludingEndToken(instances[instance_id], range, unique_name);
                        }

                        // TODO: Also patch "T..." uses in the function body
                    }
                }
            }
        }

        Parent::TraverseFunctionDecl(specialized_decl);

        this->current_function = std::nullopt;

        if (specialize) {
            // Fix up references to template parameters in the specialization by adding an explicit
            // declaration of them at the top of the specialization body
            auto template_parameters = decl->getTemplateParameters();
            auto specialization_args = specialized_decl->getTemplateSpecializationArgs()->asArray();
            assert(template_parameters);
            assert(template_parameters->size() == specialization_args.size());
            std::string aliases = "\n";
            auto parameter_it = template_parameters->begin();
            auto argument_it = specialization_args.begin();
            for (; parameter_it != template_parameters->end(); ++parameter_it, ++argument_it) {
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
            // TODO: Templated_decl locations
            rewriter->InsertTextAfter(specialized_decl->getBody()->getLocStart(), aliases);
            std::swap(rewriter, old_rewriter);
            std::string content = static_cast<HierarchicalRewriter*>(old_rewriter.get())->GetContents();
            rewriter->InsertTextAfter(specialized_decl->getLocEnd(), "\n\n// Specialization generated by CFTF\ntemplate<>\n" + content);
        }
    }

    // TODO: Now that we're done with the last specialization of this function, remove the original template function definition!
    // TODO: Adapt "auto" return type in function template declaration if necessary!

    return true;
}

static std::string RebuildVarDecl(clang::SourceManager& sm, clang::VarDecl* decl) {
    // TODO: Turn types like pseudo-code "(int[5])&& array" (currently printed as "int &&[5] t") into "int (&&t)[5]"
    std::string new_decl = clang::QualType::getAsString(decl->getType().getSplitDesugaredType(), clang::PrintingPolicy{{}});
    new_decl +=  ' ' + decl->getName().str();
    if (auto init = decl->getInit()) {
        new_decl += " = " + SourceRangeToString(sm, { init->getLocStart(), clang::Lexer::getLocForEndOfToken(init->getLocEnd(), 0, sm, {})});
    }
    return new_decl;
}

namespace ranges {

template<typename It, typename EndIt>
struct reverse_iterator {
    reverse_iterator& operator++() {
        --it;
        return *this;
    }

    auto operator* () {
        return *std::prev(it);
    }

    auto operator* () const {
        return *std::prev(it);
    }

    bool operator!=(reverse_iterator oth) {
        return it != oth.it;
    }

    It it;
};

template<typename Rng>
struct reversed {
    using ForwardIt = decltype(std::declval<Rng>().begin());
    using ForwardEndIt = decltype(std::declval<Rng>().end());

    using iterator = reverse_iterator<ForwardIt, ForwardEndIt>;

    reversed(Rng&& rng) : rng(std::forward<Rng>(rng)) {}

    iterator begin() const {
        return { rng.end() };
    }

    iterator end() const {
        return { rng.begin() };
    }

    Rng&& rng;
};

} // namespace ranges

bool ASTVisitor::VisitDeclStmt(clang::DeclStmt* stmt) {
    if (!IsInFullySpecializedFunction()) {
        return true;
    }

    // The types used in declarations might be dependent on template
    // parameters. That's not an issue usually since we re-export template
    // parameter names in the specialized template, however for parameter packs
    // this cannot be done. In those cases, we just replace the declaration
    // type by the desugared type to get rid of the template parameter uses.
    //
    // Clang doesn't provide us with the SourceLocations to the type, so
    // we need to replace the entire declaration with a manually crafted one
    // instead of replacing just the type.
    //
    // When doing these rewrites, we need to be careful about multiple
    // variables declared in the same line,
    // e.g. "stuff<T> first, *second = &first;").
    // The easiest way to make sure we do this correctly is to just split up
    // the declarations into separate statements.

    for (auto decl : ranges::reversed(stmt->decls())) {
        if (auto var_decl = clang::dyn_cast<clang::VarDecl>(decl)) {
            // TODO: This needs to be more sophisticated for inplace-defined struct types!
            auto new_decl = RebuildVarDecl(rewriter->getSourceMgr(), var_decl) + ';';
            rewriter->InsertTextAfter(clang::Lexer::getLocForEndOfToken(stmt->getLocEnd(), 0, rewriter->getSourceMgr(), {}), new_decl);
        } else if (clang::StaticAssertDecl::classof(decl)) {
            // Nothing to do
        } else {
            std::cerr << "WARNING: Unimplemented Decl: " << decl->getDeclKindName() << std::endl;
        }
    }

    // Delete the old declaration(s)
    rewriter->ReplaceTextIncludingEndToken(stmt->getSourceRange(), "");

    return true;
}

clang::Decl* ASTVisitor::FunctionTemplateInfo::FindTemplatedDecl(clang::SourceManager& sm, clang::Decl* specialized) const {
    auto is_templated_decl = [&sm,specialized](clang::Decl* candidate) {
        // Heuristic to match Decls against each other:
        // * DeclKind must be the same
        // * The SourceRange of the specialized Decl must be fully covered by
        //   the templated one (they don't need to be equal because specialized
        //   Decls may e.g. exclude the "..." from parameter packs, etc)
        return  candidate->getKind() == specialized->getKind() &&
                sm.isPointWithin(specialized->getLocStart(), candidate->getLocStart(), candidate->getLocEnd()) &&
                sm.isPointWithin(specialized->getLocEnd(), candidate->getLocStart(), candidate->getLocEnd());
    };

    auto match_it = std::find_if(decls.begin(), decls.end(), is_templated_decl);
    if (match_it == decls.end()) {
        return nullptr;
    }
    return *match_it;
}

bool ASTVisitor::VisitVarDecl(clang::VarDecl* decl) {
    if (!IsInFullySpecializedFunction()) {
        if (current_function_template) {
            current_function_template->decls.push_back(decl);
        }
        return true;
    }

    // TODO: If the type of the declared variable is dependent on a template parameter, replace it
    // NOTE: We only need to replace dependent types, but since it would be extra work to determine whether a type is dependent, we just apply this transformation to all types for now
    //       TODO: To reduce the danger of incorrect transformations, we should probably put in the extra work :/

    {
        // NOTE: getParents() only seems to return reliable results when called
        //       on the templated declaration rather than on decl directly.
        auto templated_decl = current_function->template_info->FindTemplatedDecl(rewriter->getSourceMgr(), decl);
        assert(templated_decl);
        auto parents = context.getParents(*templated_decl);
        auto node_is_decl_stmt = [](auto& node) {
            auto ptr = node.template get<clang::Stmt>();
            return ptr && clang::DeclStmt::classof(ptr);
        };
        if (std::any_of(parents.begin(), parents.end(), node_is_decl_stmt)) {
            std::cerr << "Skipping VarDecl visitation because it was already handled in VisitDeclStmt" << std::endl;
            return true;
        }
    }


    // Ideally, we'd just rewrite the type in this declaration. However, libclang provides no way to get the SourceRange for this, so we instead rebuild a new declaration from scratch...
    // NOTE: We silently drop the default arguments from the specialized
    //       signature. Keeping them certainly wouldn't be legal C++, since
    //       they are just the same as specified in the templated function
    //       declaration.
    auto new_decl = RebuildVarDecl(rewriter->getSourceMgr(), decl);
    if (auto pv_decl = clang::dyn_cast<clang::ParmVarDecl>(decl)) {
        // This is part of a function signature, so the declaration needs more
        // complicated treatment than other declarations since it could have
        // been generated from an expanded parameter pack

        auto templated = current_function->FindTemplatedParamDecl(pv_decl);
        assert(templated);

        if (templated->isParameterPack()) {
            std::string addendum;
            bool first_parameter = true;
            for (auto& parameter_and_unique_name : current_function->FindSpecializedParamDecls(templated)) {
                auto* parameter = parameter_and_unique_name.decl;

                if (!first_parameter) {
                    addendum += ", ";
                }
                first_parameter = false;

                // TODO: For on-the-fly declared template arguments like e.g. in "func<struct unnamed>()", getAsString will print spam such as "struct(anonymous namespace)::unnamed". We neither want that namespace nor do we want the "struct" prefix!
                // NOTE: CppInsights has a lot more code to handle getting the parameter name and type...
                addendum += parameter->getType().getAsString();
                if (!parameter->getNameAsString().empty()) {
                    addendum += " ";
                    // Parameter generated from a parameter pack will be assigned the same name,
                    // so we need to distinguish the generated parameter names manually.
                    addendum += parameter_and_unique_name.unique_name;
                }
            }

            // NOTE: decl->getLocEnd() and templated->getLocEnd() return
            //       different results in some cases. E.g. for
            //       "decltype(T{}) t", the former coincides with the start of
            //       the declaration, whereas the latter correctly spans the
            //       entire declaration. Similarly, for unnamed parameter packs
            //       such as "T...", decl->getLocEnd() stops before the
            //       ellipsis, whereas templated->getLocEnd() includes it
            rewriter->ReplaceTextIncludingEndToken(templated->getSourceRange(), "/*Expansion of " + GetClosedStringFor(decl->getLocStart(), templated->getLocEnd()) + "{-*/" + addendum + "/*-}*/");
        } else {
            auto old_decl = GetClosedStringFor(decl->getLocStart(), decl->getLocEnd());
            if (new_decl != old_decl) {
                rewriter->ReplaceTextIncludingEndToken(decl->getSourceRange(), "/*" + old_decl + "*/" + new_decl);
            } else {
                std::cerr << "Skipped rewrite due to matching declarations" << std::endl;
            }
        }
    } else {
        // NOTE: In particular, VarDecls should always have been handled as
        //       part of VisitDeclStmt
        std::cerr << "Unknown VarDecl kind " << decl->getDeclKindName() << std::endl;
        assert(false);
    }

    return true;
}


} // namespace cftf
