#ifndef CFTF_REWRITER_HPP
#define CFTF_REWRITER_HPP

namespace clang {
class Rewriter;
class SourceLocation;
class SourceRange;
class SourceManager;
}

namespace llvm {
class StringRef;
}

namespace cftf {

/**
 * Abstract interface akin to clang::Rewriter.
 * This allows us to do more sophisticated source manipulation than through Rewriter alone.
 */
class RewriterBase {
public:
    virtual ~RewriterBase() = default;
    virtual bool ReplaceTextIncludingEndToken(clang::SourceRange, llvm::StringRef) = 0;
    virtual bool ReplaceTextExcludingEndToken(clang::SourceRange, llvm::StringRef) = 0;
    bool InsertTextAfter(clang::SourceLocation, llvm::StringRef);
    bool RemoveTextIncludingEndToken(clang::SourceRange);
    bool RemoveTextExcludingEndToken(clang::SourceRange);
    virtual clang::SourceManager& getSourceMgr() = 0;
};

/**
 * clang::Rewriter based Rewriter that directly operates on the contents of an input source file
 */
class Rewriter final : public RewriterBase {
public:
    Rewriter(clang::Rewriter& rewriter) : rewriter(rewriter) {}

private:
    bool ReplaceTextIncludingEndToken(clang::SourceRange, llvm::StringRef) override;
    bool ReplaceTextExcludingEndToken(clang::SourceRange, llvm::StringRef) override;

    clang::SourceManager& getSourceMgr() override;

    clang::Rewriter& rewriter;
};

} // namespace cftf

#endif // CFTF_REWRITER_HPP
