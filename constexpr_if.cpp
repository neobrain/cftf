#include "ast_visitor.hpp"
#include "rewriter.hpp"

#include <iostream>

namespace cftf {

/**
 * Traverses the templated version of a function and tries to find a match for the given if statement.
 * This is used for "constexpr if", where clang helpfully strips away untaken branches in specialized
 * functions. Looking up the original statement allows us to get the SourceLocation information of
 * those branches we need to do our transformations reliably.
 */
class FinderForOriginalIfStmt : public clang::RecursiveASTVisitor<FinderForOriginalIfStmt> {
public:
    FinderForOriginalIfStmt(clang::FunctionDecl* func, clang::IfStmt* stmt) : specialized_stmt(stmt) {
        // Make sure "func" is indeed a specialization
        assert (func->getPrimaryTemplate() != nullptr);
        {
            TraverseFunctionDecl(func->getPrimaryTemplate()->getTemplatedDecl());
        }
    }

    operator clang::IfStmt*() const {
        return match;
    }

    bool VisitIfStmt(clang::IfStmt* stmt) {
        if (specialized_stmt->getLocStart() == stmt->getLocStart()) {
            match = stmt;
            // Abort traversal for this check
            return false;
        }

        return true;
    }

    bool needs_specialization = false;

    clang::IfStmt* specialized_stmt;
    clang::IfStmt* match = nullptr;
};

bool ASTVisitor::VisitIfStmt(clang::IfStmt* stmt) {
    if (features::constexpr_if && stmt->isConstexpr()) {
        // TODO: Only use this if VisitFunctionDecl has ensured we can modify the current function!
    }

    if (!in_fully_specialized_function) {
        return true;
    }

    clang::Expr* cond = stmt->getCond();
    assert(cond);

    bool result;
    bool eval_succeeded = cond->EvaluateAsBooleanCondition(result, context);
    // TODO: For newer clang:
    // clang::EvalResult result;
    // bool eval_succeeded = cond->EvaluateAsConstantExpr(result, clang::Expr::EvaluateForCodeGen, context);
    if (!eval_succeeded) {
        // This shouldn't have compiled in the first place: "if constexpr" requires a constant expression
        std::cerr << "Couldn't evaluate constexpr if condition!" << std::endl;
        cond->dump();
        // TODO: This actually does happen currently when we run this in a function template. Just silently ignore it for now hence! (This is covered by our tests)
        //assert(false);
        return true;
    }

    clang::Stmt* branch = result ? stmt->getThen() : stmt->getElse();


    assert(current_function);
    if (current_function->getPrimaryTemplate()) {
        // In function template specializations, clang overly helpfully strips out untaken else-branches right away...
        // While that could have been very convenient, it breaks our design since we these branches will still be in the
        // StagingRewriter, and now we don't get the SourceLocations of what needs to be removed.
        // Hence, we need traverse the entire function we're in and find the "if constexpr" corresponding to the one in the
        // specialized function. We detect this correspondence based on stmt->getLocStart(), which hopefully shouldn't
        // have changed.
        // TODO: This probably breaks down for manually specialized functions

        clang::IfStmt* original_statement = FinderForOriginalIfStmt(current_function, stmt);
        assert(original_statement);
        branch = result ? original_statement->getThen() : original_statement->getElse();
        stmt = original_statement;
    }

    if (branch) {
        // Remove all parts of the statement that we statically know aren't needed.
        //
        // We keep:
        // * The conditional (including everything enclosed by the parentheses following "if constexpr")
        // * The body of the branch that succeeded (including curly braces {}, if any)
        //
        // We throw away:
        // * "if constexpr", "else", "else if", and the parentheses surrounding their conditions
        //   (required "if"/"else" keywords might be re-added manually later)
        // * Bodies of branches that are not taken (replaced with an empty body {})
        //
        // Instead of replacing the entire IfStmt with only what's needed, we smartly remove all unneeded parts individually.
        // This ensures the associated SourceLocations stay valid and hence rewrite rules in
        // nested nodes apply properly.
        //
        // NOTE: Naively we might even go as far as removing the conditional entirely;
        //       however, we do need to carry around any variables defined in the condition
        //       since they are valid even in else-branches.

        // Initializing expression comes first, then the condition variable declaration, then a plain condition.
        // Only getCond() is guaranteed to return a non-null value.
        // Test for and assign in the appropriate order.
        clang::SourceLocation cond_first = stmt->getCond()->getLocStart();
        if (stmt->getInit()) {
            cond_first = stmt->getInit()->getLocStart();
        } else if (stmt->getConditionVariableDeclStmt()) {
            cond_first = stmt->getConditionVariableDeclStmt()->getLocStart();
        }

        // Don't need to check for initializing statement here since that precedes the condition anyway
        clang::SourceLocation cond_last = stmt->getCond()->getLocEnd();
        if (stmt->getConditionVariableDeclStmt()) {
            cond_last = stmt->getConditionVariableDeclStmt()->getLocEnd();
        }

        rewriter->ReplaceTextExcludingEndToken({ stmt->getLocStart(), cond_first }, "if (");
        rewriter->RemoveTextExcludingEndToken({ getLocForEndOfToken(cond_last), branch->getLocStart() });
        if (result) {
            rewriter->InsertTextAfter(cond_last, ") ");
        } else {
            // Add an empty branch body
            rewriter->InsertTextAfter(cond_last, ") {} else\n");
        }

        rewriter->RemoveTextIncludingEndToken({ getLocForEndOfToken(branch->getLocEnd()), stmt->getLocEnd() });
    } else {
        // Condition was false and no else-branch has been given, so just remove the entire statement
        rewriter->RemoveTextIncludingEndToken({ stmt->getLocStart(), stmt->getLocEnd() });
    }

    return true;
}

} // namespace cftf
