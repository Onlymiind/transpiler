#pragma once 
#include <vector>
#include <variant>
#include <string>

#include "parser/expression.h"
#include "parser/declaration.h"
#include "util/variant.h"


namespace parser {
    struct Return {
        Expression* value;
    };

    struct VariableDecl {
        util::StringConstRef name;
        Declaration* type;
        Expression* value;
    };

    struct Block;

    struct IfStatement {
        Expression* condition = nullptr;
        Block* then = nullptr;
        IfStatement* otherwise = nullptr;
    };

    using Statement = util::Variant<Expression*, VariableDecl, Return, IfStatement*>;

    struct Block {
        std::vector<Statement> statements;
    };
}
