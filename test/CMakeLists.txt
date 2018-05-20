# This is just a dummy CMake project, used solely for creating compile_commands.json
# Run this CMake script within the tests/ directory, i.e. do not attempt an out-of-source build
cmake_minimum_required(VERSION 3.10)

set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

project(tests)
add_executable(tests
    n3928_static_assert_with_optional_message.cpp)
set_property(TARGET tests PROPERTY CXX_STANDARD 17)
set_property(TARGET tests PROPERTY CXX_STANDARD_REQUIRED ON)
