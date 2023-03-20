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
)

add_library(transpiler_lib
    ${LEXER_MODULE}
    ${PARSER_MODULE}

    src/util/hashmap.h
    src/util/util.h
    src/util/util.cpp
)

add_executable(transpiler
    src/main.cpp
)

add_executable(tests
    tests/expression_test.cpp
    tests/hashmap_test.cpp
)

add_executable(benchmarks
    benchmarks/hashmap_bench.cpp
)

add_subdirectory(${CMAKE_SOURCE_DIR}/Catch2)

set(BENCHMARK_ENABLE_TESTING OFF)
add_subdirectory(${CMAKE_SOURCE_DIR}/benchmark)

target_link_libraries(transpiler PRIVATE transpiler_lib)
target_link_libraries(tests PRIVATE transpiler_lib Catch2WithMain)
target_link_libraries(benchmarks PRIVATE transpiler_lib benchmark::benchmark_main)

target_include_directories(transpiler_lib PUBLIC ${CMAKE_SOURCE_DIR}/src)
#target_include_directories(tests PRIVATE ${CMAKE_SOURCE_DIR}/test)

add_test("Transpiler tests" tests)
