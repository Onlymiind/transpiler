#pragma once

#include "parser/statement.h"
#include "types/token.h"
#include "types/operators.h"
#include "util/arena.h"

class RPNParser {
public:
    RPNParser(types::Tokens tokens, util::Arena<parser::Expression>& arena)
        :rem_(tokens), arena_(arena)
    {}

    parser::Expression* parse_expression();

    parser::UnaryExpression parse_unary_expression();

    void consume() {
        rem_ = rem_.subspan(1);
    }

    const types::Token& next() {
        return rem_[0];
    }

private:
    types::Tokens rem_;
    util::Arena<parser::Expression>& arena_;
};

bool compare(const parser::Expression* rhs, const parser::Expression* lhs);
