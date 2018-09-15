#ifndef CFTF_AST_VISITOR_HPP
#define CFTF_AST_VISITOR_HPP

#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Rewrite/Core/Rewriter.h>

namespace cftf {

// TODO: This should be moved elsewhere
namespace features {

inline bool constexpr_if = true;

}

class RewriterBase;

class ASTVisitor : public clang::RecursiveASTVisitor<ASTVisitor> {
    using Parent = clang::RecursiveASTVisitor<ASTVisitor>;

public:
    ASTVisitor(clang::ASTContext& context, clang::Rewriter& rewriter_);

    bool VisitDeclStmt(clang::DeclStmt* stmt);

    bool VisitVarDecl(clang::VarDecl* decl);

    bool VisitSizeOfPackExpr(clang::SizeOfPackExpr* expr);

    bool TraversePackExpansionExpr(clang::PackExpansionExpr* expr);

    bool VisitPackExpansionExpr(clang::PackExpansionExpr* expr);

    bool VisitDeclRefExpr(clang::DeclRefExpr* expr);

    bool VisitCXXFoldExpr(clang::CXXFoldExpr* expr);

    bool TraverseCXXFoldExpr(clang::CXXFoldExpr* expr);

    // Used to explicitly specialize function templates which are otherwise specialized implicitly
    bool TraverseFunctionTemplateDecl(clang::FunctionTemplateDecl* decl);

    bool VisitIfStmt(clang::IfStmt* stmt);

    bool VisitStaticAssertDecl(clang::StaticAssertDecl* decl);

    bool shouldTraversePostOrder() const;

    bool shouldVisitTemplateInstantiations() const { return true; }

    struct FunctionTemplateInfo {
        // The DeclRefExpr are parameter packs referenced in the pack expansion
        struct ParamPackExpansionInfo {
            clang::PackExpansionExpr* expr;
            std::vector<clang::DeclRefExpr*> referenced_packs;
        };
        std::vector<ParamPackExpansionInfo> param_pack_expansions;
        bool in_param_pack_expansion = false;

        // List of all Decls in this template. Used for reverse-lookup in
        // implicit specializations to find templated Decls from specialized
        // ones.
        std::vector<clang::Decl*> decls;

        // Assumes there is only a single one of these.
        // Don't use this for ParmVarDecls, since they might be parameter packs
        // (i.e. multiple specialized Decls per single templated Decl)!
        clang::Decl* FindTemplatedDecl(clang::SourceManager& sm, clang::Decl* specialized) const;
    };

    struct CurrentFunctionInfo {
        clang::FunctionDecl* decl;

        FunctionTemplateInfo* template_info;

        struct Parameter {
            // decl in the primary function template
            clang::ParmVarDecl* templated;

            // Decl and unique name for parameter(s) in a CFTF-specialized
            // function template. Note that there may be muliple of these,
            // since multiple arguments may be passed for a single variadic
            // template parameter
            struct SpecializedParameter {
                clang::ParmVarDecl* decl;

                // Unique name generated for this argument: Generally, we just
                // copy over the templated parameter name into this field,
                // however for variadic parameters this would cause all
                // arguments generated from a parameter pack to be assigned the
                // same name.
                std::string unique_name;
            };

            // decl in the specialized function.
            // If "templated" is a parameter pack, there may be multiple decls here (or none)
            // NOTE: This is populated only if the parameter is named!
            std::vector<SpecializedParameter> specialized;
        };

        std::vector<Parameter> parameters;

        clang::ParmVarDecl* FindTemplatedParamDecl(clang::ParmVarDecl* specialized) const;
        const std::vector<Parameter::SpecializedParameter>& FindSpecializedParamDecls(clang::ParmVarDecl* templated) const;
    };


private:
    // Gets the string of the contents enclosed by the two SourceLocations extended to the end of the last token
    clang::SourceLocation getLocForEndOfToken(clang::SourceLocation end);

    // Gets the string of the contents enclosed by the two SourceLocations extended to the end of the last token
    std::string GetClosedStringFor(clang::SourceLocation begin, clang::SourceLocation end);

    clang::ASTContext& context;
    std::unique_ptr<RewriterBase> rewriter;

    bool IsInFullySpecializedFunction() const {
        return current_function.has_value();
    }

    std::optional<CurrentFunctionInfo> current_function;

    // Only valid while traversing a template function.
    // In particular, not valid in implicit specializations
    FunctionTemplateInfo* current_function_template = nullptr;

    std::unordered_map<clang::FunctionDecl*, FunctionTemplateInfo> function_templates;

};

} // namespace cftf

#endif // CFTF_AST_VISITOR_HPP
