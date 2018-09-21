# Clang From The Future

CFTF is a source-to-source compiler for C++ developers who want to make use of C++14 and C++17 features on platforms/toolchains that usually don't support doing so. To this end, the tool converts modern C++ language features into their equivalent "old school" versions so that your current toolchain can process it.

CFTF is intended to be used as a preprocessor for other compilers and hence integrates transparently into your existing build system. When using CMake, this process is very easy to set up.

In theory, CFTF works with any compiler, although currently only compilers with gcc/clang's CLI interfaces have been tested in practice. Patches for MSVC support or other platforms are very much welcome!

## Why?

A lot of the features added in C++14 and C++17 are purely syntactical sugar, so it always bothered me that we have to wait for a compiler update rather than being able to "just" make our current compiler see through the abstraction. CFTF is my attempt of teaching existing compilers how new language features work.

There are a number of use cases for this:

* Early adoption of new standards while waiting for official support from your toolchain vendor
* Porting an existing C++14/17 code base to a toolchain that doesn't receive any vendor updates anymore
* Enabling use of libraries implemented in C++17 such as [hana](https://github.com/boostorg/hana) or [range-v3](https://github.com/ericniebler/range-v3) in a codebase that uses C++11 apart from those libraries

## Build and Usage Instructions

Build CFTF using

    mkdir build
    cd build
    cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..

Then, to compile a CMake-based C++14/17 project with your existing toolchain compiler (e.g. g++), use:

    CXX=/usr/local/bin/cftf cmake -DCMAKE_CXX_FLAGS="-frontend-compiler=/usr/bin/g++" ..

If the clang compiler executable is not installed on the system, you also need to add libclang's resource directory to `CMAKE_CXX_FLAGS` via `-resource-dir=/usr/lib64/clang/6.0.1` (the actual path depends on yout libclang installation).

Projects not using CMake need to resort to hacky solutions, currently. One method is to rename your existing compiler executable and put a copy of the `cftf` executable in its old place. To point CFTF to the frontend compiler, set the CFTF_FRONTEND_CXX, e.g. `export CFTF_FRONTEND_CXX=/usr/bin/g++`.

## Current Status

CFTF is ready enough to be tried out "for fun", but it's still mostly a proof-of-concept. I encourage you to try it out if the idea sounds useful to you, but do note it's not ready for use in production currently. I'm hoping with feedback from the community this will soon change, though!

At the moment, all of the following C++ features will be converted to C++11-compatible code, with support for more features planned in the future:
* Structured bindings
* Constexpr if
* Function return type deduction, e.g. `auto func() { return 5; }`
* Optional static assertion messages, e.g. `static_assert(sizeof(T) > 4)`
* Fold expressions (soon!)

Furthermore, CFTF can convert parameter pack expansions to C++98-compatible code.

## Future

The current feature list is small compared to the total list of [C++14](https://en.wikipedia.org/wiki/C%2B%2B14) and [C++17](https://en.wikipedia.org/wiki/C%2B%2B17) changes. The set of supported features is intentionally kept small for now until support for them works robustly and correctly in all weird corner cases that might arise.

That said, once things are rock-stable, support for new features will be added, and I will also explore ways of supporting pre-C++11 targets.
