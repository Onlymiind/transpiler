#include "codegen/generator.h"
#include "common/ast.h"
#include "common/base_classes.h"
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
#include <string>

namespace codegen {
    void Generator::codegen() {
        if (!body_ || !header_ || !mod_) {
            return;
        }

        *header_ << g_prelude;
        codegen_decls();
        const auto &functions = ast_->functions();
        for (const common::Function &func : functions) {
            codegen_function(func);
            if (error_occured()) {
                break;
            }
        }
        *body_ << g_main;
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
            codegen_var_name(
                common::downcast<common::VariableReference>(expr).name());
            break;
        default: report_error("unknown expression type"); break;
        }
    }

    void Generator::codegen_literal(const common::Literal &lit) {
        *body_ << '(';
        codegen_type(lit.type());
        *body_ << ')';

        if (const bool *b = lit.get<bool>(); b) {
            *body_ << *b;
        } else if (const uint64_t *uint = lit.get<uint64_t>(); uint) {
            *body_ << *uint;
        } else if (const double *d = lit.get<double>(); d) {
            int presicion = body_->precision();
            *body_ << std::setprecision(std::numeric_limits<double>::digits10)
                   << *d << std::setprecision(presicion);
        } else if (lit.is<std::nullptr_t>()) {
            *body_ << '0';
        } else {
            report_error("unknown literal type");
        }
    }

    void Generator::codegen_unary(const common::UnaryExpression &expr) {
        switch (expr.op()) {
        case common::UnaryOp::NOT: *body_ << '!'; break;
        case common::UnaryOp::NEGATE: *body_ << '-'; break;
        case common::UnaryOp::ADDRESS_OF: *body_ << '&'; break;
        case common::UnaryOp::DEREFERENCE:
            *body_ << '*';
            *body_ << '(';
            codegen_type(expr.expression()->type());
            *body_ << ")check_pointer(";
            codegen_expression(*expr.expression());
            *body_ << ')';
            return;
        default: report_error("unknown unary operator"); break;
        }

        codegen_expression(*expr.expression());
    }

    void Generator::codegen_binary(const common::BinaryExpression &expr) {
        *body_ << '(';
        codegen_expression(*expr.lhs());
        if (error_occured()) {
            return;
        }

        *body_ << ' ';
        using enum common::BinaryOp;
        switch (expr.op()) {
        case ADD: *body_ << '+'; break;
        case SUB: *body_ << '-'; break;
        case MUL: *body_ << '*'; break;
        case DIV: *body_ << '/'; break;
        case REMAINDER: *body_ << '%'; break;
        case AND: *body_ << "&&"; break;
        case OR: *body_ << "||"; break;
        case EQUALS: *body_ << "=="; break;
        case NOT_EQUALS: *body_ << "!="; break;
        case LESS: *body_ << '<'; break;
        case GREATER: *body_ << '>'; break;
        case LESS_EQUALS: *body_ << "<="; break;
        case GREATER_EQUALS: *body_ << ">="; break;
        case ASSIGN: *body_ << '='; break;
        case BITWISE_AND: *body_ << '&'; break;
        case BITWISE_OR: *body_ << '|'; break;
        default: report_error("unlnown binary operator"); return;
        }
        *body_ << ' ';

        codegen_expression(*expr.rhs());
        *body_ << ')';
    }

    void Generator::codegen_cast(const common::Cast &cast) {
        *body_ << '(';
        codegen_type(cast.type());
        *body_ << ')';
        codegen_expression(*cast.from());
    }

    void Generator::codegen_decls() {
        for (const common::Function &func : ast_->functions()) {
            codegen_function_decl(func);
            *body_ << ";\n";
        }

        for (common::VariableID var : ast_->global_variables()) {
            codegen_var(*ast_->get_var(var));
            *body_ << '\n';
        }
    }

    void Generator::codegen_function(const common::Function &func) {
        if (func.decl_only) {
            return;
        }

        const std::string &name = *identifiers_->get(func.name);
        codegen_function_decl(func);
        *body_ << ' ';
        codegen_block(func.body);
        *body_ << '\n';
    }

    void Generator::codegen_call(const common::FunctionCall &call) {
        codegen_func_name(call.name());
        *body_ << '(';
        const auto &args = call.arguments();
        for (size_t i = 0; i < args.size(); ++i) {
            if (i != 0) {
                *body_ << ", ";
            }
            codegen_expression(*args[i]);
        }
        *body_ << ')';
    }

    void Generator::codegen_var(const common::Variable &var) {
        codegen_type(var.type);
        *body_ << ' ';
        codegen_var_name(var.name);
        if (!var.initial_value) {
            // TODO: proper zero-initialization
            if (var.type->kind() == common::TypeKind::ARRAY) {
                *body_ << ";\n";
                *body_ << "memset(";
                codegen_var_name(var.name);
                *body_ << ".data, 0, sizeof(";
                codegen_type(var.type);
                *body_ << "));";
            } else {
                *body_ << " = (";
                codegen_type(var.type);
                *body_ << ")0;";
            }
            return;
        }
        *body_ << " = ";
        codegen_expression(*var.initial_value);
        *body_ << ';';
    }

    void Generator::codegen_function_decl(const common::Function &func) {
        codegen_type(func.return_type);
        *body_ << ' ';
        codegen_func_name(func.name);
        *body_ << '(';
        for (size_t i = 0; i < func.params.size(); ++i) {
            common::Variable &param = *ast_->get_var(func.params[i]);
            if (i != 0) {
                *body_ << ", ";
            }
            codegen_type(param.type);
            if (param.name != common::IdentifierID{}) {
                *body_ << ' ';
                codegen_var_name(param.name);
            }
        }
        if (func.params.empty()) {
            *body_ << "void";
        }
        *body_ << ')';
    }

    void Generator::codegen_branch(const common::Branch &branch) {
        *body_ << "if(";
        codegen_expression(*branch.predicate());
        *body_ << ") ";
        codegen_block(branch.true_branch());
        if (!branch.false_branch() ||
            branch.false_branch()->statements().empty()) {
            return;
        }
        *body_ << "else ";
        const common::Block &false_branch = *branch.false_branch();
        if (false_branch.statements().size() == 1 &&
            false_branch.statements()[0]->kind() ==
                common::StatementType::BRANCH) {
            codegen_branch(common::downcast<common::Branch>(
                *false_branch.statements()[0]));
        } else {
            codegen_block(false_branch);
        }
    }

    void Generator::codegen_block(const common::Block &block) {
        *body_ << '{';
        for (const auto &smt : block.statements()) {
            *body_ << '\n';
            codegen_statement(*smt);
            if (!err_.empty()) {
                return;
            }
        }
        if (!block.statements().empty()) {
            *body_ << '\n';
        }
        *body_ << '}';
    }

    void Generator::codegen_loop(const common::Loop &loop) {
        *body_ << "for (";
        if (loop.init()) {
            codegen_statement(*loop.init());
        } else {
            *body_ << ';';
        }
        if (loop.condition()) {
            codegen_expression(*loop.condition());
        }
        *body_ << ';';
        if (loop.iteration()) {
            codegen_expression(*loop.iteration());
        }
        *body_ << ") ";
        codegen_block(loop.body());
    }

    void Generator::codegen_statement(const common::Statement &smt) {
        switch (smt.kind()) {
        case common::StatementType::RETURN: {
            *body_ << "return ";
            const common::Expression
                *expr = common::downcast<common::Return>(smt).expression();
            if (expr) {
                codegen_expression(*expr);
            }
            *body_ << ';';
            break;
        }
        case common::StatementType::EXPRESSION: {
            const common::Expression
                *expr = common::downcast<common::ExpressionStatement>(smt)
                            .expression();
            codegen_expression(*expr);
            *body_ << ';';
            break;
        }
        case common::StatementType::VARIABLE:
            codegen_var(*ast_->get_var(
                common::downcast<common::VariableDeclatarion>(smt).variable()));
            break;
        case common::StatementType::BRANCH:
            codegen_branch(common::downcast<common::Branch>(smt));
            break;
        case common::StatementType::LOOP:
            codegen_loop(common::downcast<common::Loop>(smt));
            break;
        case common::StatementType::BREAK: *body_ << "break;"; break;
        case common::StatementType::CONTINUE: *body_ << "continue;"; break;
        default: report_error("statement type not supported"); return;
        }
    }

    void Generator::codegen_type(const common::Type *type, std::ostream *out) {
        if (!out) {
            out = body_;
        }
        if (!type) {
            *out << "void";
            return;
        }
        switch (type->kind()) {
        case common::TypeKind::PRIMITIVE:
            *out << *identifiers_->get(
                common::downcast<common::PrimitiveType>(*type).name());
            break;
        case common::TypeKind::POINTER:
            codegen_type(common::downcast<common::PointerType>(*type)
                             .pointee_type(),
                         out);
            *out << '*';
            break;
        case common::TypeKind::ARRAY:
            if (!codegen_type_decl(type)) {
                return;
            }
            *out << *identifiers_->get(type_names_.at(type));
            break;
        default: report_error("unsupported type"); break;
        }
    }

    bool Generator::codegen_type_decl(const common::Type *type) {
        if (!type || type_names_.contains(type) ||
            type->kind() == common::TypeKind::PRIMITIVE) {
            return true;
        }
        if (type->kind() == common::TypeKind::POINTER) {
            return codegen_type_decl(
                common::downcast<common::PointerType>(*type).pointee_type());
        }
        if (type->kind() != common::TypeKind::ARRAY) {
            report_error("type declaration generation: unsupported type kind");
            return false;
        }

        const common::ArrayType &array = common::downcast<common::ArrayType>(
            *type);
        if (!codegen_type_decl(array.element_type())) {
            return false;
        }
        common::IdentifierID array_name = generate_type_name(type);
        if (array_name == common::IdentifierID{}) {
            return false;
        }
        *header_ << "typedef struct { ";
        codegen_type(array.element_type(), header_);
        *header_ << " data[" << array.count() << "];} "
                 << *identifiers_->get(array_name) << ";\n";

        return true;
    }

    common::IdentifierID
    Generator::generate_type_name(const common::Type *type) {
        auto it = type_names_.find(type);
        if (it != type_names_.end()) {
            return it->second;
        }
        auto add_name = [type, this](common::IdentifierID name) {
            type_names_[type] = name;
            return name;
        };

        if (!type) {
            return add_name(identifiers_->add("void"));
        }

        switch (type->kind()) {
        case common::TypeKind::PRIMITIVE:
            return add_name(
                common::downcast<common::PrimitiveType>(*type).name());
        case common::TypeKind::POINTER: {
            const common::PointerType
                &ptr = common::downcast<common::PointerType>(*type);
            common::IdentifierID pointee_name = generate_type_name(
                ptr.pointee_type());
            if (pointee_name == common::IdentifierID{}) {
                return add_name(common::IdentifierID{});
            }
            return add_name(
                identifiers_->add(*identifiers_->get(pointee_name) + "_p"));
        }
        case common::TypeKind::ARRAY: {
            const common::ArrayType
                &array = common::downcast<common::ArrayType>(*type);
            common::IdentifierID element_name = generate_type_name(
                array.element_type());
            if (element_name == common::IdentifierID{}) {
                return add_name(common::IdentifierID{});
            }
            return add_name(identifiers_->add(*identifiers_->get(element_name) +
                                              "_array_" +
                                              std::to_string(array.count())));
        }
        default:
            report_error("unknown type kind");
            return add_name(common::IdentifierID{});
        }
    }

    void Generator::codegen_var_name(common::IdentifierID name) {
        *body_ << "var_" << *identifiers_->get(name);
    }

    void Generator::codegen_func_name(common::IdentifierID name) {
        *body_ << "func_" << *identifiers_->get(name);
    }
} // namespace codegen
