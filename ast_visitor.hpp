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

    bool VisitSizeOfPackExpr(clang::SizeOfPackExpr* expr);

    bool VisitCXXFoldExpr(clang::CXXFoldExpr* expr);

    bool TraverseCXXFoldExpr(clang::CXXFoldExpr* expr);

    // TODO: Shouldn't be necessary
    bool VisitFunctionDecl(clang::FunctionDecl* decl);

    bool TraverseFunctionDecl(clang::FunctionDecl* decl);

    // Used to explicitly specialize function templates which might otherwise be specialized implicitly
    bool VisitFunctionTemplateDecl(clang::FunctionTemplateDecl* decl);

    bool VisitIfStmt(clang::IfStmt* stmt);

    bool VisitStaticAssertDecl(clang::StaticAssertDecl* decl);

    bool shouldTraversePostOrder() const;

    struct CurrentFunctionInfo {
        clang::FunctionDecl* decl;

        struct Parameter {
            // decl in the primary function template
            clang::ParmVarDecl* templated;

            // decl in the specialized function.
            // If "templated" is a parameter pack, there may be multiple decls here (or none)
            // NOTE: This is populated only if the parameter is named!
            std::vector<clang::ParmVarDecl*> specialized;
        };

        std::vector<Parameter> parameters;

        clang::ParmVarDecl* FindTemplatedParamDecl(clang::ParmVarDecl* specialized) const;
        const std::vector<clang::ParmVarDecl*>& FindSpecializedParamDecls(clang::ParmVarDecl* templated) const;
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
};

} // namespace cftf

#endif // CFTF_AST_VISITOR_HPP
