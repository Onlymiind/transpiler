#pragma once 
#include <vector>
#include <variant>
#include <string>

#include "parser/expression.h"
#include "parser/declaration.h"
#include "util/arena.h"
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

    struct Assignment {
        util::StringConstRef name;
        Expression* value;
    };

    struct Loop {
        util::Variant<std::monostate, Assignment> init;
        Expression* condition = nullptr;
        util::Variant<std::monostate, Assignment, Expression*> step;
        Block* body = nullptr;
    };

    using Statement = util::Variant<Expression*, VariableDecl, Return, IfStatement*, Assignment, Loop>;

    struct Block {
        std::vector<Statement> statements;
    };
}
