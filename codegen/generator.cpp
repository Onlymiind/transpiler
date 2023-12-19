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
#include <cstdint>
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
            codegen_function(func);
            if (error_occured()) {
                break;
            }
        }
        *out_ << g_main;
    }

    void Generator::codegen_expression(const common::Expression &expr) {
        if (expr.is_error()) {
            return;
        }

        switch (expr.kind()) {
        case common::ExpressionKind::BINARY:
            codegen_binary(common::downcast<common::BinaryExpression>(expr));
            break;
        case common::ExpressionKind::UNARY:
            codegen_unary(common::downcast<common::UnaryExpression>(expr));
            break;
        case common::ExpressionKind::LITERAL:
            codegen_literal(common::downcast<common::Literal>(expr));
            break;
        case common::ExpressionKind::CAST:
            codegen_cast(common::downcast<common::Cast>(expr));
            break;
        case common::ExpressionKind::FUNCTION_CALL:
            codegen_call(common::downcast<common::FunctionCall>(expr));
            break;
        case common::ExpressionKind::VARIABLE_REF:
            codegen_var_name(common::downcast<common::VariableReference>(expr).name());
            break;
        default:
            report_error("unknown expression type");
            break;
        }
    }

    void Generator::codegen_literal(const common::Literal &lit) {
        *out_ << '(';
        codegen_type(lit.type());
        *out_ << ')';

        if (const bool *b = lit.get<bool>(); b) {
            *out_ << *b;
        } else if (const uint64_t *uint = lit.get<uint64_t>(); uint) {
            *out_ << *uint;
        } else if (const double *d = lit.get<double>(); d) {
            int presicion = out_->precision();
            *out_ << std::setprecision(std::numeric_limits<double>::digits10)
                  << *d << std::setprecision(presicion);
        } else if (lit.is<std::nullptr_t>()) {
            *out_ << '0';
        } else {
            report_error("unknown literal type");
        }
    }

    void Generator::codegen_unary(const common::UnaryExpression &expr) {
        switch (expr.op()) {
        case common::UnaryOp::NOT: *out_ << '!'; break;
        case common::UnaryOp::NEGATE: *out_ << '-'; break;
        case common::UnaryOp::ADDRESS_OF: *out_ << '&'; break;
        case common::UnaryOp::DEREFERENCE:
            *out_ << '*';
            *out_ << '(';
            codegen_type(expr.expression()->type());
            *out_ << ")check_pointer(";
            codegen_expression(*expr.expression());
            *out_ << ')';
            return;
        default:
            report_error("unknown unary operator");
            break;
        }

        codegen_expression(*expr.expression());
    }

    void Generator::codegen_binary(const common::BinaryExpression &expr) {
        *out_ << '(';
        codegen_expression(*expr.lhs());
        if (error_occured()) {
            return;
        }

        *out_ << ' ';
        using enum common::BinaryOp;
        switch (expr.op()) {
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
        case BITWISE_AND: *out_ << '&'; break;
        case BITWISE_OR: *out_ << '|'; break;
        default:
            report_error("unlnown binary operator");
            return;
        }
        *out_ << ' ';

        codegen_expression(*expr.rhs());
        *out_ << ')';
    }

    void Generator::codegen_cast(const common::Cast &cast) {
        *out_ << '(';
        codegen_type(cast.type());
        *out_ << ')';
        codegen_expression(*cast.from());
    }

    void Generator::codegen_decls() {
        const auto &functions = mod_->global_scope()->functions();
        for (const common::FunctionID &func_id : functions) {
            codegen_function_decl(*ast_->get_function(func_id));
            *out_ << ";\n";
        }

        const auto &variables = mod_->global_scope()->variables();
        for (const auto &var : variables) {
            codegen_var(*ast_->get_var(var));
            *out_ << '\n';
        }
    }

    void Generator::codegen_function(const common::Function &func) {
        if (func.decl_only) {
            return;
        }

        const std::string &name = *identifiers_->get(func.name);
        codegen_function_decl(func);
        *out_ << ' ';
        codegen_block(func.body);
        *out_ << '\n';
    }

    void Generator::codegen_call(const common::FunctionCall &call) {
        codegen_func_name(call.name());
        *out_ << '(';
        const auto &args = call.arguments();
        for (size_t i = 0; i < args.size(); ++i) {
            if (i != 0) {
                *out_ << ", ";
            }
            codegen_expression(*args[i]);
        }
        *out_ << ')';
    }

    void Generator::codegen_var(const common::Variable &var) {
        codegen_type(var.type);
        *out_ << ' ';
        codegen_var_name(var.name);
        *out_ << " = ";
        if (!var.initial_value) {
            // TODO: proper zero-initialization
            *out_ << '(';
            codegen_type(var.type);
            *out_ << ")0;";
            return;
        }
        codegen_expression(*var.initial_value);
        *out_ << ';';
    }

    void Generator::codegen_function_decl(const common::Function &func) {
        codegen_type(func.return_type);
        *out_ << ' ';
        codegen_func_name(func.name);
        *out_ << '(';
        for (size_t i = 0; i < func.params.size(); ++i) {
            common::Variable &param = *ast_->get_var(func.params[i]);
            if (i != 0) {
                *out_ << ", ";
            }
            codegen_type(param.type);
            if (param.name != common::IdentifierID{}) {
                codegen_var_name(param.name);
            }
        }
        if (func.params.empty()) {
            *out_ << "void";
        }
        *out_ << ')';
    }

    void Generator::codegen_branch(const common::Branch &branch) {
        *out_ << "if(";
        codegen_expression(*branch.predicate());
        *out_ << ") ";
        codegen_block(branch.true_branch());
        if (!branch.false_branch() || branch.false_branch()->statements().empty()) {
            return;
        }
        *out_ << "else ";
        const common::Block &false_branch = *branch.false_branch();
        if (false_branch.statements().size() == 1 && false_branch.statements()[0]->kind() == common::StatementType::BRANCH) {
            codegen_branch(common::downcast<common::Branch>(*false_branch.statements()[0]));
        } else {
            codegen_block(false_branch);
        }
    }

    void Generator::codegen_block(const common::Block &block) {
        *out_ << '{';
        for (const auto &smt : block.statements()) {
            *out_ << '\n';
            codegen_statement(*smt);
            if (!err_.empty()) {
                return;
            }
        }
        if (!block.statements().empty()) {
            *out_ << '\n';
        }
        *out_ << '}';
    }

    void Generator::codegen_loop(const common::Loop &loop) {
        *out_ << "for (";
        if (loop.init()) {
            codegen_statement(*loop.init());
        } else {
            *out_ << ';';
        }
        if (loop.condition()) {
            codegen_expression(*loop.condition());
        }
        *out_ << ';';
        if (loop.iteration()) {
            codegen_expression(*loop.iteration());
        }
        *out_ << ") ";
        codegen_block(loop.body());
    }

    void Generator::codegen_statement(const common::Statement &smt) {
        switch (smt.kind()) {
        case common::StatementType::RETURN: {
            *out_ << "return ";
            const common::Expression *expr = common::downcast<common::Return>(smt).expression();
            if (expr) {
                codegen_expression(*expr);
            }
            *out_ << ';';
            break;
        }
        case common::StatementType::EXPRESSION: {
            const common::Expression *expr = common::downcast<common::ExpressionStatement>(smt).expression();
            codegen_expression(*expr);
            *out_ << ';';
            break;
        }
        case common::StatementType::VARIABLE:
            codegen_var(*ast_->get_var(common::downcast<common::VariableDeclatarion>(smt).variable()));
            break;
        case common::StatementType::BRANCH:
            codegen_branch(common::downcast<common::Branch>(smt));
            break;
        case common::StatementType::LOOP:
            codegen_loop(common::downcast<common::Loop>(smt));
            break;
        case common::StatementType::BREAK: *out_ << "break;"; break;
        case common::StatementType::CONTINUE: *out_ << "continue;"; break;
        default:
            report_error("statement type not supported");
            return;
        }
    }

    void Generator::codegen_type(common::Type type) {
        if (type.is_nullptr()) {
            *out_ << "void*";
            return;
        } else if (type.is_void()) {
            *out_ << "void";
            return;
        } else if (type.is_error()) {
            report_error("trying to generate error type");
            return;
        }
        *out_ << *identifiers_->get(mod_->get_scope(type.sym.scope)->get_type(type.sym.id)->name);
        for (uint64_t i = 0; i < type.indirection_level; ++i) {
            *out_ << '*';
        }
    }

    void Generator::codegen_var_name(common::IdentifierID name) {
        *out_ << "var_" << *identifiers_->get(name);
    }

    void Generator::codegen_func_name(common::IdentifierID name) {
        *out_ << "func_" << *identifiers_->get(name);
    }
} // namespace codegen
