#include "rewriter.hpp"

#include <clang/Rewrite/Core/Rewriter.h>
#include <clang/Lex/Lexer.h>

namespace cftf {

bool RewriterBase::InsertTextAfter(clang::SourceLocation loc, llvm::StringRef new_str) {
    loc = clang::Lexer::getLocForEndOfToken(loc, 0, getSourceMgr(), {});
    return ReplaceTextExcludingEndToken({loc, loc}, new_str);
}

bool RewriterBase::RemoveTextIncludingEndToken(clang::SourceRange range) {
    return ReplaceTextIncludingEndToken(range, "");
}

bool RewriterBase::RemoveTextExcludingEndToken(clang::SourceRange range) {
    return ReplaceTextExcludingEndToken(range, "");
}

bool Rewriter::ReplaceTextIncludingEndToken(clang::SourceRange range, llvm::StringRef new_str) {
    return rewriter.ReplaceText(range, new_str);
}

bool Rewriter::ReplaceTextExcludingEndToken(clang::SourceRange range, llvm::StringRef ref) {
    // It seems that clang::Rewriter provides no way of replacing contents up to a given location without also removing the token at that location,
    // so just insert the content before and then remove the previous range.

    if (range.getBegin() != range.getEnd()) {
        if (rewriter.RemoveText(clang::CharSourceRange::getCharRange(range))) {
            return true;
        }
    }

    return rewriter.InsertTextBefore(range.getBegin(), ref);
}

clang::SourceManager& Rewriter::getSourceMgr() {
    return rewriter.getSourceMgr();
}

} // namespace cftf
