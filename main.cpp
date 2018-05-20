#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Lex/Lexer.h>
#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Frontend/FrontendAction.h>
#include <clang/Frontend/ASTConsumers.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Tooling/Tooling.h>
#include <clang/Rewrite/Core/Rewriter.h>
#include <llvm/Support/CommandLine.h>

#include <iostream>
#include <string_view>

namespace ct = clang::tooling;

namespace cftf {

static llvm::cl::OptionCategory tool_category("tool options");

class ASTVisitor : public clang::RecursiveASTVisitor<ASTVisitor> {
    using Parent = clang::RecursiveASTVisitor<ASTVisitor>;

public:
    ASTVisitor(clang::Rewriter& rewriter_) : rewriter(rewriter_) {}

    bool VisitCXXFoldExpr(clang::CXXFoldExpr* expr) {
        std::cerr << "Visiting CXX fold expression" << std::endl;
        std::cerr << "  " << std::flush;
        auto* pattern = expr->getPattern();
        pattern->dumpColor();
        std::cerr << std::endl;
        auto& sm = rewriter.getSourceMgr();
        const auto pattern_base_str = GetClosedStringFor(pattern->getLocStart(), pattern->getLocEnd());

        // TODO: Support operators: + - * / % ^ & | << >>, all of these with an = at the end; ==, !=, <, >, <=, >=, &&, ||, ",", .*, ->*
        using namespace std::literals::string_view_literals;
        std::map<clang::BinaryOperatorKind, std::string_view> operators;
        operators[clang::BO_Add] = "add"sv;
        operators[clang::BO_Sub] = "sub"sv;
        operators[clang::BO_Mul] = "mul"sv;
        operators[clang::BO_Div] = "div"sv;
        operators[clang::BO_Rem] = "mod"sv;
        operators[clang::BO_Xor] = "xor"sv;
        operators[clang::BO_And] = "and"sv;
        operators[clang::BO_Or]  = "or"sv;
        operators[clang::BO_Shl] = "shl"sv;
        operators[clang::BO_Shr] = "shr"sv;

        operators[clang::BO_AddAssign] = "add_assign"sv;
        operators[clang::BO_SubAssign] = "sub_assign"sv;
        operators[clang::BO_MulAssign] = "mul_assign"sv;
        operators[clang::BO_DivAssign] = "div_assign"sv;
        operators[clang::BO_RemAssign] = "mod_assign"sv;
        operators[clang::BO_XorAssign] = "xor_assign"sv;
        operators[clang::BO_AndAssign] = "and_assign"sv;
        operators[clang::BO_OrAssign] = "or_assign"sv;
        operators[clang::BO_ShlAssign] = "shl_assign"sv;
        operators[clang::BO_ShrAssign] = "shr_assign"sv;

        operators[clang::BO_Assign] = "assign"sv;
        operators[clang::BO_EQ] = "equals"sv;
        operators[clang::BO_NE] = "notequals"sv;
        operators[clang::BO_LT] = "less"sv;
        operators[clang::BO_GT] = "greater"sv;
        operators[clang::BO_LE] = "lessequals"sv;
        operators[clang::BO_GE] = "greaterequals"sv;
        operators[clang::BO_LAnd] = "land"sv;
        operators[clang::BO_LOr] = "lor"sv;
        operators[clang::BO_Comma] = "comma"sv;

        auto fold_op = expr->getOperator();
        if (fold_op == clang::BO_PtrMemD || fold_op == clang::BO_PtrMemI) {
            // TODO: These might just work, actually...
            throw std::runtime_error("Fold expressions on member access operators not supported, yet!");
        }

        auto init_value_str = expr->getInit() ? GetClosedStringFor(expr->getInit()->getLocStart(), expr->getInit()->getLocEnd()) : "";

        // TODO: What value category should we use for the arguments?
        //       Currently, assigment operators take lvalue-refs, and anything else copies by value
        auto pattern_str = std::string("fold_expr_").append(operators.at(fold_op));
        if (expr->isLeftFold()) {
            pattern_str += "_left(";
            if (expr->getInit()) {
                pattern_str += init_value_str + ", ";
            }
        } else {
            pattern_str += "_right(";
        }
        pattern_str += pattern_base_str + "...";
        if (expr->isRightFold() && expr->getInit()) {
            pattern_str += ", " + init_value_str;
        }
        pattern_str += ")";

        std::cerr << "  Pattern: \"" << pattern_str << '"' << std::endl;
        rewriter.ReplaceText(expr->getLocStart(), sm.getCharacterData(getLocForEndOfToken(expr->getLocEnd())) - sm.getCharacterData(expr->getLocStart()), pattern_str); 
        return true;
    }

    bool TraverseCXXFoldExpr(clang::CXXFoldExpr* expr) {
        // We currently can't perform any nested replacements within a fold expressions
        // hence, visit this node but none of its children, and instead process those in the next pass

        std::cerr << "Traversing fold expression: " << GetClosedStringFor(expr->getLocStart(), expr->getLocEnd()) << std::endl;

        Parent::WalkUpFromCXXFoldExpr(expr);

        return true;
    }

    bool VisitStaticAssertDecl(clang::StaticAssertDecl* decl) {
        if (decl->getMessage() == nullptr) {
            // Add empty assertion message
            auto assert_cond = GetClosedStringFor(decl->getAssertExpr()->getLocStart(), decl->getAssertExpr()->getLocEnd());
            auto& sm = rewriter.getSourceMgr();

            auto new_assert = std::string("static_assert(") + assert_cond + ", \"\")";
            rewriter.ReplaceText(decl->getLocStart(), sm.getCharacterData(getLocForEndOfToken(decl->getLocEnd())) - sm.getCharacterData(decl->getLocStart()), new_assert);

            std::cerr << "ololol";
        }

        return true;
    }

    bool shouldTraversePostOrder() const {
        // ACTUALLY, visit top-nodes first; that way, we can withhold further transformations in its child nodes if necessary
        return false;

        // Visit leaf-nodes first (so we transform the innermost expressions first)
        //return true;
    }

private:
    // Gets the string of the contents enclosed by the two SourceLocations extended to the end of the last token
    clang::SourceLocation getLocForEndOfToken(clang::SourceLocation end) {
        return clang::Lexer::getLocForEndOfToken(end, 0, rewriter.getSourceMgr(), {});
    }

    // Gets the string of the contents enclosed by the two SourceLocations extended to the end of the last token
    std::string GetClosedStringFor(clang::SourceLocation begin, clang::SourceLocation end) {
        auto& sm = rewriter.getSourceMgr();
        auto begin_data = sm.getCharacterData(begin);
        auto end_data = sm.getCharacterData(getLocForEndOfToken(end));
        return std::string(begin_data, end_data - begin_data);
    }

    clang::Rewriter& rewriter;
};

class ASTConsumer : public clang::ASTConsumer {
public:
    ASTConsumer(clang::Rewriter& rewriter) : visitor(rewriter) {}

    bool HandleTopLevelDecl(clang::DeclGroupRef ref) override {
        std::cerr << "\nASTConsumer handling top level declaration" << std::endl;

        for (auto elem : ref) {
            visitor.TraverseDecl(elem);
            elem->dumpColor();
        }

        return true;
    }

private:
    ASTVisitor visitor;
};

class FrontendAction : public clang::ASTFrontendAction {
public:
    FrontendAction() {}

    void EndSourceFileAction() override {
        std::cerr << "Executing action" << std::endl;

        clang::SourceManager& sm = rewriter.getSourceMgr();
        rewriter.getEditBuffer(sm.getMainFileID()).write(llvm::outs());
    }

    std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(clang::CompilerInstance& ci, clang::StringRef file) override {
        std::cerr << "Creating AST consumer for: " << file.str() << std::endl;
        rewriter.setSourceMgr(ci.getSourceManager(), ci.getLangOpts());
        return llvm::make_unique<ASTConsumer>(rewriter);
    }

private:
    clang::Rewriter rewriter;
};

} // namespace cftf

int main(int argc, const char* argv[]){
    ct::CommonOptionsParser options_parser(argc, argv, cftf::tool_category);
    ct::ClangTool tool(options_parser.getCompilations(), options_parser.getSourcePathList());

    int result = tool.run(ct::newFrontendActionFactory<cftf::FrontendAction>().get());

    return result;
}

