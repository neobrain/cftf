#include "ast_visitor.hpp"
#include "rewriter.hpp"

#include <clang/Lex/Lexer.h>
#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Frontend/FrontendAction.h>
#include <clang/Frontend/ASTConsumers.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Tooling/Tooling.h>
#include <clang/Rewrite/Core/Rewriter.h>
#include <llvm/Support/CommandLine.h>
#include "llvm/Support/Error.h"

#include <iostream>
#include <memory>
#include <numeric>
#include <string_view>

#ifdef __linux__
namespace llvm {
/**
 * http://lists.llvm.org/pipermail/llvm-dev/2017-January/109621.html
 * We can't rebuild llvm, but we can define symbol missed in llvm build.
 */
//int DisableABIBreakingChecks = 1;
}
#endif

namespace ct = clang::tooling;

namespace cftf {

static llvm::cl::OptionCategory tool_category("tool options");

ASTVisitor::ASTVisitor(clang::ASTContext& context, clang::Rewriter& rewriter_)
        : context(context), rewriter(std::unique_ptr<RewriterBase>(new Rewriter(rewriter_))) {
}

bool ASTVisitor::VisitCXXFoldExpr(clang::CXXFoldExpr* expr) {
    std::cerr << "Visiting CXX fold expression" << std::endl;
    std::cerr << "  " << std::flush;
    auto* pattern = expr->getPattern();
    pattern->dumpColor();
    std::cerr << std::endl;
    auto& sm = rewriter->getSourceMgr();
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
    rewriter->ReplaceTextIncludingEndToken({expr->getLocStart(), expr->getLocEnd()}, pattern_str);
    return true;
}

bool ASTVisitor::TraverseCXXFoldExpr(clang::CXXFoldExpr* expr) {
    // We currently can't perform any nested replacements within a fold expressions
    // hence, visit this node but none of its children, and instead process those in the next pass

    std::cerr << "Traversing fold expression: " << GetClosedStringFor(expr->getLocStart(), expr->getLocEnd()) << std::endl;

    Parent::WalkUpFromCXXFoldExpr(expr);

    return true;
}

bool ASTVisitor::VisitStaticAssertDecl(clang::StaticAssertDecl* decl) {
    if (decl->getMessage() == nullptr) {
        // Add empty assertion message
        auto assert_cond = GetClosedStringFor(decl->getAssertExpr()->getLocStart(), decl->getAssertExpr()->getLocEnd());
        auto& sm = rewriter->getSourceMgr();

        auto new_assert = std::string("static_assert(") + assert_cond + ", \"\")";
        rewriter->ReplaceTextIncludingEndToken({decl->getLocStart(), decl->getLocEnd()}, new_assert);
    }

    return true;
}

bool ASTVisitor::shouldTraversePostOrder() const {
    // ACTUALLY, visit top-nodes first; that way, we can withhold further transformations in its child nodes if necessary
    return false;

    // Visit leaf-nodes first (so we transform the innermost expressions first)
    //return true;
}

clang::SourceLocation ASTVisitor::getLocForEndOfToken(clang::SourceLocation end) {
    return clang::Lexer::getLocForEndOfToken(end, 0, rewriter->getSourceMgr(), {});
}

std::string ASTVisitor::GetClosedStringFor(clang::SourceLocation begin, clang::SourceLocation end) {
    auto& sm = rewriter->getSourceMgr();
    auto begin_data = sm.getCharacterData(begin);
    auto end_data = sm.getCharacterData(getLocForEndOfToken(end));
    return std::string(begin_data, end_data - begin_data);
}

class ASTConsumer : public clang::ASTConsumer {
public:
    ASTConsumer(clang::Rewriter& rewriter) : rewriter(rewriter) {}

    virtual void Initialize(clang::ASTContext& context) override {
        visitor = std::make_unique<ASTVisitor>(context, rewriter);
    }

    bool HandleTopLevelDecl(clang::DeclGroupRef ref) override {
        std::cerr << "\nASTConsumer handling top level declaration" << std::endl;

        for (auto elem : ref) {
            visitor->TraverseDecl(elem);
            elem->dumpColor();
        }

        return true;
    }

    void HandleTranslationUnit(clang::ASTContext&) override {
        std::cerr << "\nASTConsumer handling translation unit" << std::endl;
        visitor.reset();
    }

private:
    clang::Rewriter& rewriter;

    std::unique_ptr<ASTVisitor> visitor;
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



struct ParsedCommandLine {
    // Indexes into argv, referring to detected input filenames
    std::vector<size_t> input_filename_arg_indices;

    // Indexes into argv, referring to command line arguments that need to be forwarded to the internal libtooling pass
    std::vector<size_t> input_indexes;
};


ParsedCommandLine ParseCommandLine(size_t argc, const char* argv[]) {
    using namespace std::string_literals;

    // TODO: Strip CFTF-specific options from argument list
    // TODO: Add options specific to the CFTF pass to argument list

    // Indexes into argv, referring to detected input filenames (including the executable name)
    std::vector<size_t> input_filename_arg_indices;

    // Indexes into known command line arguments
    std::vector<size_t> input_indexes;

    for (size_t arg_idx = 1; arg_idx < argc; ++arg_idx) {
        auto arg = argv[arg_idx];

        auto is_cpp_file = [](const char* str) -> bool {
            // TODO: This is a very incomplete heuristic:
            //       * Not everybody uses .cpp for C++ files
            //       * The source language can be overridden by user flags
            auto len = std::strlen(str);
            return (len > 3) && (0 == std::strcmp(str + len - 4, ".cpp"));
        };

        if (std::strcmp(arg, "-") == 0) {
            // stdin. TODO.
            std::cerr << "Using stdin as input not handled yet" << std::endl;
            std::exit(1);
        } else if (arg[0] == '-') {
            // This argument is some sort of flag.
            if (arg[1] == 'D') {
                // Preprocessor define
                input_indexes.push_back(arg_idx);
                if (arg[2] == ' ') {
                    if (arg_idx + 1 == argc) {
                        std::cerr << "Invalid input: Expected symbol after \"-D\", got end of command line" << std::endl;
                        std::exit(1);
                    }
                    // Include the actual definition, too.
                    // Note that if a value is provided, it's provided via "=VALUE", i.e. it cannot be space-separated.
                    input_indexes.push_back(++arg_idx);
                }
            } else {
                std::cerr << "Ignoring command line option \"" << arg << "\"" << std::endl;
            }
        } else if (is_cpp_file(arg)) {
            // TODO: Does this catch all inputs? Is there a way to specify inputs via non-positional command line arguments?
            input_filename_arg_indices.push_back(arg_idx);
        }
    }

    return { std::move(input_filename_arg_indices), std::move(input_indexes) };
}

struct InternalCommandLine {
    std::vector<const char*> args;
    std::string internal_storage;
};

InternalCommandLine BuildInternalCommandLine(const ParsedCommandLine& parse_cmdline, const char* argv[]) {
    using namespace std::string_literals;

    auto&& [ input_filename_arg_indices, input_indexes ] = parse_cmdline;

    // Build a restricted command line that only includes all input files
    InternalCommandLine internal_command_line;
    std::vector<const char*>& internal_argv = internal_command_line.args;

    internal_command_line.internal_storage.reserve(1000); // TODO: HACK!!
    std::transform(input_indexes.begin(), input_indexes.end(), std::back_inserter(internal_argv),
                    [&argv, &storage=internal_command_line.internal_storage](size_t idx) {
                        const char* ptr = &*storage.end();
                        storage += argv[idx];
                        storage += '\0';
                        return ptr;
                    });
    std::transform(input_filename_arg_indices.begin(), input_filename_arg_indices.end(), std::back_inserter(internal_argv), [argv](size_t idx) { return argv[idx]; });
    return internal_command_line;
}

class CompilationDatabase : public ct::CompilationDatabase {
    std::vector<std::string> infiles;
    std::vector<ct::CompileCommand> commands;

public:
    CompilationDatabase(const ParsedCommandLine& parsed_cmdline, const InternalCommandLine& internal_cmdline, const char* argv[]) {
        // TODO: Support multiple input files
        assert(parsed_cmdline.input_filename_arg_indices.size() == 1);

        std::transform(parsed_cmdline.input_filename_arg_indices.begin(), parsed_cmdline.input_filename_arg_indices.end(), std::back_inserter(infiles),
                    [argv](size_t index) {
                        return argv[index];
                    });

        ct::CompileCommand cmd;
        cmd.Directory = "."; // Current working directory
        cmd.Filename = argv[parsed_cmdline.input_filename_arg_indices[0]];

        // The first argument is always skipped over, since it's just the executable name. We add it here so libtooling doesn't skip over data that's actually important
        cmd.CommandLine.push_back("cftf");
        std::copy(internal_cmdline.args.begin(), internal_cmdline.args.end(), std::back_inserter(cmd.CommandLine));

        if (cmd.Filename == "-") {
            std::cerr << "stdin not supported, yet" << std::endl;
            std::exit(1);
        } else {
            // Insert "_cftf_out" before the file extension (or at the end of the filename if there is no file extension)
            // TODO: The output should be written to a temporary folder instead
            auto period_pos = cmd.Filename.find_last_of('.');
            if (period_pos == std::string::npos) {
                period_pos = cmd.Filename.length();
            }
            std::copy(cmd.Filename.begin(), cmd.Filename.begin() + period_pos, std::back_inserter(cmd.Output));
            cmd.Output += "_cftf_out";
            std::copy(cmd.Filename.begin() + period_pos, cmd.Filename.end(), std::back_inserter(cmd.Output));
        }

        commands.emplace_back(cmd);
    }

    std::vector<ct::CompileCommand> getCompileCommands(llvm::StringRef) const override {
        // TODO: Take the given path into consideration
        return commands;
    }

    std::vector<std::string> getAllFiles() const override {
        return infiles;
    }
};


int main(int argc, const char* argv[]){
    auto parsed_cmdline = ParseCommandLine(static_cast<size_t>(argc), argv);
    auto internal_argv = BuildInternalCommandLine(parsed_cmdline, argv);

    CompilationDatabase compilation_database(parsed_cmdline, internal_argv, argv);

    // Run FrontendAction on each input file
    for (auto& file : compilation_database.getAllFiles()) {
        std::cerr << "Processing file " << file << std::endl;

        for (auto& cmd : compilation_database.getCompileCommands(file)) {
            std::cerr << "  Directory: " << cmd.Directory << std::endl;
            std::cerr << "  Command:   ";
            for (auto& cmd2 : cmd.CommandLine) {
                std::cerr << cmd2 << " ";
            }
            std::cerr << std::endl;
            std::cerr << "  Output:    " << cmd.Output << std::endl;
        }
        std::cerr << std::endl;

        ct::ClangTool tool(compilation_database, file);
        int result = tool.run(ct::newFrontendActionFactory<cftf::FrontendAction>().get());
        if (result != 0) {
            std::cerr << "CFTF FrontendAction failed on file \"" << file << "\" with code " << result << std::endl;
            std::exit(1);
        }
    }

    const char* frontend_command = std::getenv("CFTF_FRONTEND_CXX");
    if (!frontend_command || frontend_command[0] == 0) {
        std::cerr << "Error: CFTF_FRONTEND_CXX not set" << std::endl;
        exit(1);
    }

    // Replace original input filenames with the corresponding cftf output
    std::string modified_cmdline = frontend_command;
    for (size_t arg_idx = 0; arg_idx < static_cast<size_t>(argc); ++arg_idx) {
        using namespace std::literals::string_literals;
        auto arg = argv[arg_idx];

        if (parsed_cmdline.input_filename_arg_indices.end() != std::find(parsed_cmdline.input_filename_arg_indices.begin(), parsed_cmdline.input_filename_arg_indices.end(), arg_idx)) {
            auto compile_commands = compilation_database.getCompileCommands(arg);
            assert(!compile_commands.empty());
            if (compile_commands.size() > 1) {
                std::cerr << "Compiling the same file multiple times is not supported yet. Please raise a bug report if you run into this issue" << std::endl;
                std::exit(1);
            }
            auto temp_output_filename = compile_commands[0].Output;
            std::cerr << "Replacing presumable input argument \"" << arg << "\" with \"" << temp_output_filename << "\"" << std::endl;
            // TODO: Wrap filename in quotes!
            modified_cmdline += " "s + temp_output_filename;
        } else {
            // Other argument; just copy this to the new command line
            // TODO: Wrap arguments in quotes or escape them!
            modified_cmdline += " "s + arg;
        }
    }

    // Trigger proper compilation
    std::cerr << "Invoking \"" << modified_cmdline << "\"" << std::endl;
    std::system(modified_cmdline.c_str());
}

