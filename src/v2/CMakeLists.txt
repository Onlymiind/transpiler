cmake_minimum_required(VERSION 3.18)

project(compiler_v2 CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_subdirectory(../../Catch2 ../../Catch2/build)

set(COMPILER_SRC
    common/token.h
    common/util.h
    common/expression.h
    common/file.h
    common/types.h
    common/module.h
    common/literals.h

    lexer/lexer.h
    lexer/lexer.cpp

    parser/parser.h
    parser/parser.cpp

    checker/checker.h
    checker/checker.cpp

    codegen/generator.h
    codegen/generator.cpp

    compiler/compiler.h
    compiler/compiler.cpp
)

add_library(compiler_lib ${COMPILER_SRC})
target_include_directories(compiler_lib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(compiler main.cpp)
target_include_directories(compiler PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(compiler compiler_lib)

set(TEST_SRCS
    tests/lexer_test.cpp
    tests/parser_test.cpp
    tests/checker_test.cpp
    tests/generator_test.cpp
)

add_executable(tests ${TEST_SRCS})
target_include_directories(tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(tests Catch2WithMain compiler_lib)

include(CTest)
include(Catch)
catch_discover_tests(tests)


