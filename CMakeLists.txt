cmake_minimum_required(VERSION 3.11)

project(transpiler CXX)
set(CMAKE_CXX_STANDARD 20)

set(LEXER_MODULE
    src/lexer/lexer.h
    src/lexer/lexer.cpp
)

set(PARSER_MODULE
    src/parser/parser.h
    src/parser/parser.cpp
    src/parser/expression.h
    src/parser/expression.cpp

    src/parser/declaration.h
    src/parser/statement.h
)

set(CHECKER_MODULE
    src/checker/checker.h
    src/checker/checker.cpp
    src/checker/module.h
    src/checker/module.cpp
    src/checker/type_resolver.h
    src/checker/type_resolver.cpp
)

set(UTIL_MODULE
    src/util/util.h
    src/util/util.cpp
    src/util/arena.h
    src/util/variant.h
    src/util/error_handler.h
)

add_library(transpiler_lib
    ${LEXER_MODULE}
    ${PARSER_MODULE}
    ${CHECKER_MODULE}
    ${UTIL_MODULE}
)

add_executable(transpiler
    src/main.cpp
)

add_executable(tests
    tests/expression_test.cpp
)

add_subdirectory(${CMAKE_SOURCE_DIR}/Catch2)

set(BENCHMARK_ENABLE_TESTING OFF)
add_subdirectory(${CMAKE_SOURCE_DIR}/benchmark)

target_link_libraries(transpiler PRIVATE transpiler_lib)
target_link_libraries(tests PRIVATE transpiler_lib Catch2WithMain)

target_include_directories(transpiler_lib PUBLIC ${CMAKE_SOURCE_DIR}/src)
#target_include_directories(tests PRIVATE ${CMAKE_SOURCE_DIR}/test)

add_test("Transpiler tests" tests)
