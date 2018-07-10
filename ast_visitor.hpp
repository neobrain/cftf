#ifndef CFTF_AST_VISITOR_HPP
#define CFTF_AST_VISITOR_HPP

#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Rewrite/Core/Rewriter.h>

namespace cftf {

class ASTVisitor : public clang::RecursiveASTVisitor<ASTVisitor> {
    using Parent = clang::RecursiveASTVisitor<ASTVisitor>;

public:
    ASTVisitor(clang::ASTContext& context, clang::Rewriter& rewriter_)
        : context(context), rewriter(rewriter_) {}

    bool VisitCXXFoldExpr(clang::CXXFoldExpr* expr);

    bool TraverseCXXFoldExpr(clang::CXXFoldExpr* expr);

    bool VisitStaticAssertDecl(clang::StaticAssertDecl* decl);

    bool shouldTraversePostOrder() const;

private:
    // Gets the string of the contents enclosed by the two SourceLocations extended to the end of the last token
    clang::SourceLocation getLocForEndOfToken(clang::SourceLocation end);

    // Gets the string of the contents enclosed by the two SourceLocations extended to the end of the last token
    std::string GetClosedStringFor(clang::SourceLocation begin, clang::SourceLocation end);

    clang::ASTContext& context;
    clang::Rewriter& rewriter;
};

} // namespace cftf

#endif // CFTF_AST_VISITOR_HPP
