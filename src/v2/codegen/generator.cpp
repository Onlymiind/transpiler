#include "codegen/generator.h"
#include "common/ast.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/statement.h"
#include "common/token.h"
#include "common/types.h"
#include "common/util.h"
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
        const auto &functions = ast_->functions();
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

        switch (expr.kind) {
        case common::ExpressionKind::BINARY:
            codegen(*ast_->get_binary_expression(expr.id));
            break;
        case common::ExpressionKind::UNARY:
            codegen(*ast_->get_unary_expression(expr.id));
            break;
        case common::ExpressionKind::LITERAL:
            codegen(*ast_->get_literal(expr.id), expr);
            break;
        case common::ExpressionKind::CAST:
            codegen(*ast_->get_cast(expr.id), expr);
            break;
        case common::ExpressionKind::FUNCTION_CALL:
            codegen(*ast_->get_call(expr.id));
            break;
        default:
            report_error("unknown expression type");
            break;
        }
    }

    void Generator::codegen(common::Literal lit, common::Expression expr) {
        *out_ << '(';
        codegen(mod_->global_scope()->get_type(expr.type)->type);
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

    void Generator::codegen(common::Cast cast, common::Expression expr) {
        if (cast.from.kind == common::ExpressionKind::LITERAL) {
            // avoid unnecessary casts
            codegen(*ast_->get_literal(cast.from.id), expr);
            return;
        }

        *out_ << '(';
        codegen(mod_->global_scope()->get_type(mod_->global_scope()->find(cast.to))->type);
        *out_ << ')';
        codegen(cast.from);
    }

    void Generator::codegen_forward_decls() {
        const auto &functions = ast_->functions();
        for (const common::Function &func : functions) {
            const std::string &name = *identifiers_->get(func.name);
            if (name == "main") {
                continue;
            }

            if (func.return_type == common::g_void_type) {
                *out_ << "void";
            } else {
                codegen(mod_->global_scope()->get_type(func.return_type)->type);
            }
            *out_ << ' ';
            *out_ << name;
            *out_ << "(void);\n";
        }
    }

    void Generator::codegen(common::Function func) {
        const std::string &name = *identifiers_->get(func.name);
        if (name == "main") {
            *out_ << "int";
        } else if (func.return_type == common::g_void_type) {
            *out_ << "void";
        } else {
            codegen(mod_->global_scope()->get_type(func.return_type)->type);
        }

        *out_ << ' ';
        *out_ << name;
        *out_ << "(void) {\nreturn ";
        if (name == "main") {
            *out_ << "(int)";
        }
        for (common::Statement smt : func.body.smts) {
            if (smt.type == common::StatementType::RETURN) {
                *out_ << "return ";
            }
            codegen(ast_->get_expression(smt.id));
        }
        *out_ << ";\n}\n";
    }

    void Generator::codegen(common::FunctionCall call) {
        *out_ << *identifiers_->get(call.name) << "()";
    }
} // namespace codegen
