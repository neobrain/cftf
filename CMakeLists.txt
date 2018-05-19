# 3.10 is the version I'm using, but if you're on a lower version feel free to comment out this line and report back results...
cmake_minimum_required(VERSION 3.10)
project(cftf)

find_package(Clang REQUIRED CONFIG HINTS /usr/lib64/cmake/clang)
find_package(LLVM REQUIRED CONFIG HINTS /usr/lib64/cmake/llvm)

add_executable(cftf main.cpp)
target_link_libraries(cftf LLVM clangAST clangBasic clangFrontend clangLex clangTooling clangRewrite)
set_property(TARGET cftf PROPERTY CXX_STANDARD 17)
set_property(TARGET cftf PROPERTY CXX_STANDARD_REQUIRED ON)
