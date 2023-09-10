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
    src/parser/declaration.h
)

set(CHECKER_MODULE
    src/checker/statement.h
    src/checker/traits.h
    src/checker/sym_table.h
    src/checker/sym_table.cpp
    src/checker/type_checker.h
    src/checker/type_checker.cpp
)

set(UTIL_MODULE
    src/util/util.h
    src/util/util.cpp
    src/util/arena.h
    src/util/variant.h
    src/util/error_handler.h
    src/util/error_handler.cpp
)

set(TYPES_MODULE
    src/types/token.h
    src/types/token.cpp
)

add_library(transpiler_lib
    ${LEXER_MODULE}
    ${PARSER_MODULE}
    ${CHECKER_MODULE}
    ${UTIL_MODULE}
    ${TYPES_MODULE}
)

add_executable(transpiler
    src/main.cpp
)

add_executable(tests
    tests/pn_parser.h
    tests/pn_parser.cpp
    tests/expression_test.cpp
    tests/parser_tests.cpp
)

add_subdirectory(${CMAKE_SOURCE_DIR}/Catch2)

set(BENCHMARK_ENABLE_TESTING OFF)
add_subdirectory(${CMAKE_SOURCE_DIR}/benchmark)

target_link_libraries(transpiler PRIVATE transpiler_lib)
target_link_libraries(tests PRIVATE transpiler_lib Catch2WithMain)

target_include_directories(transpiler_lib PUBLIC ${CMAKE_SOURCE_DIR}/src)
#target_include_directories(tests PRIVATE ${CMAKE_SOURCE_DIR}/test)

add_test("Transpiler tests" tests, WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
