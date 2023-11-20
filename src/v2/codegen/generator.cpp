#include "codegen/generator.h"
#include "common/expression.h"
#include "common/token.h"
#include "common/types.h"
#include <iomanip>
#include <limits>

namespace codegen {
    void Generator::codegen() {
        if (!out_ || !mod_) {
            return;
        }

        common::Expression expr = mod_->file().start();
        if (expr.is_error()) {
            return;
        }
        std::optional<common::BuiltinTypes> type = mod_->get_builtin(mod_->get_expression_type(expr));
        if (!type) {
            return;
        }

        *out_ << g_prelude;
        codegen(*type);
        if (error_occured()) {
            return;
        }

        *out_ << " result =";

        codegen(mod_->file().start());
        if (error_occured()) {
            return;
        }

        *out_ << ";\nprintf(\"%";
        switch (*type) {
        case common::BuiltinTypes::BOOL: *out_ << "d"; break;
        case common::BuiltinTypes::UINT: *out_ << "u"; break;
        case common::BuiltinTypes::FLOAT: *out_ << "f"; break;
        default:
            report_error("result printing: unknown type");
            return;
        }
        *out_ << "\\n\", result);\n"
              << g_postlude;
    }

    void Generator::codegen(common::Expression expr) {
        if (expr.is_error()) {
            return;
        }

        switch (expr.type) {
        case common::ExpressionType::BINARY:
            codegen(*mod_->file().get_binary_expression(expr.id));
            break;
        case common::ExpressionType::UNARY:
            codegen(*mod_->file().get_unary_expression(expr.id));
            break;
        case common::ExpressionType::LITERAL:
            codegen(*mod_->file().get_literal(expr.id), expr);
            break;
        default:
            report_error("unknown expression type");
            break;
        }
    }

    void Generator::codegen(common::Literal lit, common::Expression expr) {
        *out_ << '(';
        codegen(*mod_->get_builtin(mod_->get_expression_type(expr)));
        *out_ << ')';

        switch (lit.value.type) {
        case common::TokenType::BOOL:
            *out_ << lit.value.bool_val;
            break;
        case common::TokenType::INTEGER:
            *out_ << lit.value.int_val;
            break;
        case common::TokenType::FLOAT: {
            int presicion = out_->precision();
            *out_ << std::setprecision(std::numeric_limits<double>::digits10)
                  << lit.value.float_val << std::setprecision(presicion);
            break;
        }
        default:
            report_error("unknown literal");
            break;
        }
    }

    void Generator::codegen(common::UnaryExpression expr) {
        *out_ << ' ';
        switch (expr.op) {
        case common::UnaryOp::NOT: *out_ << '!'; break;
        case common::UnaryOp::NEGATE: *out_ << '-'; break;
        default:
            report_error("unknown unary operator");
            break;
        }

        codegen(expr.expr);
    }

    void Generator::codegen(common::BinaryExpression expr) {
        *out_ << " (";
        codegen(expr.lhs);
        if (error_occured()) {
            return;
        }

        *out_ << ' ';
        using enum common::BinaryOp;
        switch (expr.op) {
        case ADD: *out_ << '+'; break;
        case SUB: *out_ << '-'; break;
        case MUL: *out_ << '*'; break;
        case DIV: *out_ << '/'; break;
        case REMAINDER: *out_ << '%'; break;
        case AND: *out_ << "&&"; break;
        case OR: *out_ << "||"; break;
        case EQUALS: *out_ << "=="; break;
        case NOT_EQUALS: *out_ << "!="; break;
        case LESS: *out_ << '<'; break;
        case GREATER: *out_ << '>'; break;
        case LESS_EQUALS: *out_ << "<="; break;
        case GREATER_EQUALS: *out_ << ">="; break;
        default:
            report_error("unlnown binary operator");
            return;
        }
        *out_ << ' ';

        codegen(expr.rhs);
        *out_ << ')';
    }

    void Generator::codegen(common::BuiltinTypes type) {
        using enum common::BuiltinTypes;
        switch (type) {
        case BOOL:
            *out_ << "int";
            break;
        case UINT:
            *out_ << "uint64_t";
            break;
        case FLOAT:
            *out_ << "double";
            break;
        default:
            report_error("unknown builtin type");
            break;
        }
    }
} // namespace codegen
