#include "codegen/generator.h"
#include "common/ast.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/token.h"
#include "common/types.h"
#include <iomanip>
#include <ios>
#include <limits>

namespace codegen {
    void Generator::codegen() {
        if (!out_ || !mod_) {
            return;
        }

        *out_ << g_prelude;
        codegen_forward_decls();
        const auto &functions = mod_->file().functions();
        for (const common::Function &func : functions) {
            codegen(func);
            if (error_occured()) {
                break;
            }
        }
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
            codegen(*mod_->file().get_literal(expr.id), expr.id);
            break;
        case common::ExpressionType::CAST:
            codegen(*mod_->file().get_cast(expr.id), expr.id);
            break;
        case common::ExpressionType::FUNCTION_CALL:
            codegen(*mod_->file().get_call(expr.id));
            break;
        default:
            report_error("unknown expression type");
            break;
        }
    }

    void Generator::codegen(common::Literal lit, common::ExpressionID expr) {
        *out_ << '(';
        codegen(mod_->get_builtin(mod_->get_expression_type(expr))->type);
        *out_ << ')';

        switch (lit.type) {
        case common::LiteralType::BOOL:
            if (lit.value == common::g_false_id) {
                *out_ << '0';
            } else if (lit.value == common::g_true_id) {
                *out_ << '1';
            } else {
                report_error("livalid literal id for boolean");
            }
            break;
        case common::LiteralType::UINT:
            *out_ << *literals_->get_integer(lit.value);
            break;
        case common::LiteralType::FLOAT: {
            int presicion = out_->precision();
            *out_ << std::setprecision(std::numeric_limits<double>::digits10)
                  << *literals_->get_double(lit.value) << std::setprecision(presicion);
            break;
        }
        default:
            report_error("unknown literal");
            break;
        }
    }

    void Generator::codegen(common::UnaryExpression expr) {
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
        *out_ << '(';
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

    void Generator::codegen(common::Cast cast, common::ExpressionID expr) {
        if (cast.from.type == common::ExpressionType::LITERAL) {
            // avoid unnecessary casts
            codegen(*mod_->file().get_literal(cast.from.id), expr);
            return;
        }

        *out_ << '(';
        codegen(mod_->get_builtin(mod_->get_type(cast.to))->type);
        *out_ << ')';
        codegen(cast.from);
    }

    void Generator::codegen_forward_decls() {
        const auto &functions = mod_->file().functions();
        for (const common::Function &func : functions) {
            if (func.body.is_error()) {
                continue;
            }
            const std::string &name = *identifiers_->get(func.name);
            if (name == "main") {
                continue;
            }

            codegen(mod_->get_builtin(mod_->get_expression_type(func.body.id))->type);
            *out_ << ' ';
            *out_ << name;
            *out_ << "(void);\n";
        }
    }

    void Generator::codegen(common::Function func) {
        if (func.body.is_error()) {
            return;
        }

        const std::string &name = *identifiers_->get(func.name);
        if (name == "main") {
            *out_ << "int";
        } else {
            codegen(mod_->get_builtin(mod_->get_expression_type(func.body.id))->type);
        }
        *out_ << ' ';
        *out_ << name;
        *out_ << "(void) {\nreturn ";
        if (name == "main") {
            *out_ << "(int)";
        }
        codegen(func.body);
        *out_ << ";\n}\n";
    }

    void Generator::codegen(common::FunctionCall call) {
        *out_ << *identifiers_->get(call.name) << "()";
    }
} // namespace codegen
