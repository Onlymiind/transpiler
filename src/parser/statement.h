#pragma once 
#include <vector>
#include <variant>
#include <string_view>

#include "parser/expression.h"
#include "parser/declaration.h"
#include "util/variant.h"


namespace parser {
    struct Return {
        Expression value;
    };

    struct VariableDecl {
        std::string_view name;
        Declaration type;
        Expression value;
    };


    struct Statement{
        util::Variant<Expression, VariableDecl, Return> smt;
    };

    struct Block {
        std::vector<Statement> statements;
    };
}