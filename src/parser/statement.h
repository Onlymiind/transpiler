#pragma once 
#include <vector>
#include <variant>
#include <string>

#include "parser/expression.h"
#include "parser/declaration.h"
#include "util/variant.h"


namespace parser {
    struct Return {
        Expression value;
    };

    struct VariableDecl {
        std::string name;
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
