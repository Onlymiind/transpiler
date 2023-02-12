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

    src/parser2/parser.h
    src/parser2/parser.cpp
)

add_executable(transpiler
    ${LEXER_MODULE}
    ${PARSER_MODULE}

    src/util/util.h
    src/util/util.cpp
    src/main.cpp
)

target_include_directories(transpiler PRIVATE
    ${CMAKE_SOURCE_DIR}/src
)