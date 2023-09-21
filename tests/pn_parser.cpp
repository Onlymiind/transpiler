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
        //TODO: function calls
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
    } else if(rhs->expr.is<parser::FunctionCall>() || lhs->expr.is<parser::FunctionCall>()) {
        throw std::invalid_argument("function calls are not supported by polish notation parser yet");
    } else if(rhs->expr.is<types::Token>() && lhs->expr.is<types::Token>()) {
        auto lhs_tok = lhs->expr.get<types::Token>();
        auto rhs_tok = rhs->expr.get<types::Token>();
        return  lhs_tok == rhs_tok;
    } else if(rhs->expr.is<parser::BinaryExpression>() && lhs->expr.is<parser::BinaryExpression>()) {
        auto rhs_expr = rhs->expr.get<parser::BinaryExpression>();
        auto lhs_expr = lhs->expr.get<parser::BinaryExpression>();
        return rhs_expr.op == lhs_expr.op 
            && compare(rhs_expr.lhs, lhs_expr.lhs)
            && compare(rhs_expr.lhs, lhs_expr.lhs);
    } else if(rhs->expr.is<parser::UnaryExpression>() && lhs->expr.is<parser::UnaryExpression>()) {
        auto rhs_expr = rhs->expr.get<parser::UnaryExpression>();
        auto lhs_expr = lhs->expr.get<parser::UnaryExpression>();
        return rhs_expr.op == lhs_expr.op && compare(rhs_expr.expr, lhs_expr.expr);
    } else if(rhs->expr.is<util::StringConstRef>() && lhs->expr.is<util::StringConstRef>()) {
        auto lhs_ref = lhs->expr.get<util::StringConstRef>();
        auto rhs_ref = rhs->expr.get<util::StringConstRef>();
        return lhs_ref == rhs_ref;
    }

    return false;
}
