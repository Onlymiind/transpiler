#include "codegen/generator.h"
#include "common/ast.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/statement.h"
#include "common/token.h"
#include "common/types.h"
#include "common/util.h"
#include <cstddef>
#include <iomanip>
#include <ios>
#include <limits>

namespace codegen {
    void Generator::codegen() {
        if (!out_ || !mod_) {
            return;
        }

        *out_ << g_prelude;
        codegen_decls();
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
        case common::ExpressionKind::VARIABLE_REF:
            *out_ << *identifiers_->get(ast_->get_variable_ref(expr.id));
            break;
        default:
            report_error("unknown expression type");
            break;
        }
    }

    void Generator::codegen(common::Literal lit, common::Expression expr) {
        *out_ << '(';
        *out_ << *identifiers_->get(mod_->get_scope(expr.type.scope)->get_type(expr.type.id)->name);
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
        case ASSIGN: *out_ << '='; break;
        default:
            report_error("unlnown binary operator");
            return;
        }
        *out_ << ' ';

        codegen(expr.rhs);
        *out_ << ')';
    }

    void Generator::codegen(common::Cast cast, common::Expression expr) {
        if (cast.from.kind == common::ExpressionKind::LITERAL) {
            // avoid unnecessary casts
            codegen(*ast_->get_literal(cast.from.id), expr);
            return;
        }

        *out_ << '(';
        *out_ << *identifiers_->get(cast.to);
        *out_ << ')';
        codegen(cast.from);
    }

    void Generator::codegen_decls() {
        const auto &functions = mod_->global_scope()->functions();
        for (const common::FunctionID &func_id : functions) {
            const common::Function &func = *ast_->get_function(func_id);
            const std::string &name = *identifiers_->get(func.name);
            if (name == "main") {
                continue;
            }

            if (func.return_type.is_void()) {
                *out_ << "void";
            } else {
                *out_ << *identifiers_->get(func.return_typename);
            }
            *out_ << ' ';
            *out_ << name;
            *out_ << '(';
            for (size_t i = 0; i < func.params.size(); ++i) {
                common::Variable &param = *ast_->get_var(func.params[i]);
                if (i != 0) {
                    *out_ << ", ";
                }
                *out_ << *identifiers_->get(param.explicit_type);
                if (param.name != common::IdentifierID{}) {
                    *out_ << ' ' << *identifiers_->get(param.name);
                }
            }
            if (func.params.empty()) {
                *out_ << "void";
            }
            *out_ << ");\n";
        }

        const auto &variables = mod_->global_scope()->variables();
        for (const auto &var : variables) {
            codegen(*ast_->get_var(var));
            *out_ << '\n';
        }
    }

    void Generator::codegen(const common::Function &func) {
        if (func.decl_only) {
            return;
        }

        const std::string &name = *identifiers_->get(func.name);
        if (name == "main") {
            *out_ << "int main(void)";
        } else {
            codegen_function_decl(func);
        }
        *out_ << " {";
        for (common::Statement smt : func.body.smts) {
            *out_ << '\n';
            switch (smt.type) {
            case common::StatementType::RETURN:
                *out_ << "return ";
                if (name == "main") {
                    *out_ << "(int)";
                }
                [[fallthrough]];
            case common::StatementType::EXPRESSION: {
                common::Expression *expr = ast_->get_expression(smt.id);
                if (expr->kind == common::ExpressionKind::EMPTY) {
                    continue;
                }
                codegen(*expr);
                *out_ << ';';
                break;
            }
            case common::StatementType::VARIABLE:
                codegen(*ast_->get_var(smt.id));
            }
        }
        if (!func.body.smts.empty()) {
            *out_ << '\n';
        }
        *out_ << "}\n";
    }

    void Generator::codegen(const common::FunctionCall &call) {
        *out_ << *identifiers_->get(call.name) << '(';
        for (size_t i = 0; i < call.args.size(); ++i) {
            if (i != 0) {
                *out_ << ", ";
            }
            codegen(call.args[i]);
        }
        *out_ << ')';
    }

    void Generator::codegen(const common::Variable &var) {
        *out_ << *identifiers_->get(mod_->get_scope(var.type.scope)->get_type(var.type.id)->name)
              << ' ' << *identifiers_->get(var.name) << " = ";
        if (var.initial_value.is_error()) {
            // TODO: proper zero-initialization
            *out_ << '(' << *identifiers_->get(mod_->get_scope(var.type.scope)->get_type(var.type.id)->name) << ")0;";
            return;
        }
        codegen(var.initial_value);
        *out_ << ';';
    }

    void Generator::codegen_function_decl(const common::Function &func) {
        if (func.return_type.is_void()) {
            *out_ << "void";
        } else {
            *out_ << *identifiers_->get(func.return_typename);
        }
        *out_ << ' ';
        *out_ << *identifiers_->get(func.name);
        *out_ << '(';
        for (size_t i = 0; i < func.params.size(); ++i) {
            common::Variable &param = *ast_->get_var(func.params[i]);
            if (i != 0) {
                *out_ << ", ";
            }
            *out_ << *identifiers_->get(param.explicit_type);
            if (param.name != common::IdentifierID{}) {
                *out_ << ' ' << *identifiers_->get(param.name);
            }
        }
        if (func.params.empty()) {
            *out_ << "void";
        }
        *out_ << ')';
    }
} // namespace codegen
