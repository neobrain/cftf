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

