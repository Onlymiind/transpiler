#include "common/literals.h"
#include "lexer/lexer.h"
#include "parser/parser.h"

lexer::LexerResult lex(const std::string &str);
std::pair<parser::Parser, common::Identifiers> parse(const std::string &str);
