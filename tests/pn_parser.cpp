#include "pn_parser.h"
#include "parser/statement.h"
#include "types/operators.h"
#include "types/token.h"
#include "util/arena.h"
#include <stdexcept>


parser::Expression* PNParser::parse_expression() {
    if(rem_.empty()) {
        throw std::runtime_error("expected expression");
    }

    parser::Expression* result = arena_.allocate();
    if(types::g_binary_ops.contains(next().category)) {
        auto op = types::g_binary_ops.at(next().category).op;
        consume();
        result->expr = parser::BinaryExpression{
            .lhs = parse_expression(),
            .rhs = parse_expression(),
            .op = op,
        };
        return result;
    }

    using cat = types::Category;
    switch (next().category) {
    case cat::IDENTIFIER:
        result->expr = next().value.get<util::StringConstRef>();
        consume();
        break;
    case cat::STRING:
    case cat::INTEGER:
    case cat::FLOAT:
    case cat::CHAR:
        result->expr = next();
        consume();
        break;
    case cat::LPAREN:
        consume();
        result->expr = parse_unary_expression();
        consume(); //TODO: probably should check for right parenthesis
        break;
    default:
        throw std::runtime_error("unsupported token");
    }


    return result;
}

parser::UnaryExpression PNParser::parse_unary_expression() {
    if(!types::g_unary_ops.contains(next().category)) {
        throw std::runtime_error("expected unary operator");
    }

    auto op = types::g_unary_ops.at(next().category).op;
    consume();
    return parser::UnaryExpression{
        .expr = parse_expression(),
        .op = op,
    };
}

bool compare(const parser::Expression* rhs, const parser::Expression* lhs) {
    if(!rhs && !lhs) {
        return true;
    } else if(!rhs || !lhs) {
        return false;
    } else if(rhs->expr.is<types::Token>() && lhs->expr.is<types::Token>()) {
        
    }
}
