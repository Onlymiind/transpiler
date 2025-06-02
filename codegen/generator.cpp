#include "codegen/generator.h"
#include "common/ast.h"
#include "common/base_classes.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/statement.h"
#include "common/types.h"
#include "common/util.h"
#include "vm/vm.h"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <limits>
#include <string>
#include <vector>

namespace codegen {
    bool Generator::codegen() {
        if (!body_ || !header_ || !mod_) {
            return false;
        }

        *header_ << g_prelude;
        codegen_decls();
        const auto &functions = ast_->functions();
        for (const common::Function &func : functions) {
            if (!codegen_function(func)) {
                break;
            }
        }
        *body_ << g_main;
        return true;
    }

    bool Generator::codegen_expression(const common::Expression &expr,
                                       bool want_ptr) {
        if (expr.is_error()) {
            return false;
        }

        switch (expr.kind()) {
        case common::ExpressionKind::BINARY:
            return codegen_binary(
                dynamic_cast<const common::BinaryExpression &>(expr));
        case common::ExpressionKind::UNARY:
            return codegen_unary(dynamic_cast<const common::UnaryExpression &>(
                                     expr),
                                 want_ptr);
        case common::ExpressionKind::LITERAL:
            return codegen_literal(dynamic_cast<const common::Literal &>(expr));
        case common::ExpressionKind::CAST:
            return codegen_cast(dynamic_cast<const common::Cast &>(expr));
        case common::ExpressionKind::FUNCTION_CALL:
            return codegen_call(
                dynamic_cast<const common::FunctionCall &>(expr));
        case common::ExpressionKind::VARIABLE_REF:
            return codegen_var_ref(dynamic_cast<
                                       const common::VariableReference &>(expr),
                                   want_ptr);
        case common::ExpressionKind::INDEX:
            return codegen_index_expression(dynamic_cast<
                                                const common::IndexExpression
                                                    &>(expr),
                                            want_ptr);
        case common::ExpressionKind::MEMBER_ACCESS:
            return codegen_member_access(dynamic_cast<const common::MemberAccess
                                                          &>(expr),
                                         want_ptr);
        default: report_error("unknown expression type"); return false;
        }
    }

    bool Generator::codegen_literal(const common::Literal &lit) {
        uint64_t value = 0;
        if (const bool *b = lit.get<bool>(); b) {
            value = *b ? vm::VM::true_value : vm::VM::false_value;
        } else if (const int64_t *integer = lit.get<int64_t>(); integer) {
            value = static_cast<uint64_t>(*integer);
        } else if (const double *d = lit.get<double>(); d) {
            value = std::bit_cast<uint64_t>(*d);
        } else if (lit.is<std::nullptr_t>()) {
            value = vm::VM::null_value;
        } else {
            report_error("unknown literal type");
            return false;
        }

        output.push_back(vm::Instruction{.op = vm::Op::PUSH, .arg = value});
        return true;
    }

    bool Generator::codegen_unary(const common::UnaryExpression &expr,
                                  bool want_ptr) {
        using enum common::UnaryOp;
        if (!codegen_expression(*expr.expression(), expr.op() == ADDRESS_OF)) {
            return false;
        }

        switch (expr.op()) {
        case NOT: [[fallthrough]];
        case NEGATE: push_op(vm::Op::NOT); break;
        case ADDRESS_OF: break;
        case DEREFERENCE: {
            if (want_ptr) {
                break;
            }

            const common::PointerType
                *typ = dynamic_cast<const common::PointerType *>(expr.type());
            if (!(typ->pointee_type()->is_pointer() ||
                  typ->pointee_type()->is_primitive())) {
                break;
            }

            push_op(vm::Op::READ);
            break;
        }
        default: report_error("unknown unary operator"); return false;
        }

        return false;
    }

    void Generator::push_equals(const common::Type *type) {
        if (type->is_pointer() || type->is_primitive()) {
            push_op(vm::Op::EQUALS);
        } else {
            push_op(vm::Op::MEM_EQUALS);
        }
    }

    void Generator::push_assign(const common::Type *type) {
        if (type->is_pointer() || type->is_primitive()) {
            push_op(vm::Op::WRITE);
        } else {
            push_op(vm::Op::COPY);
        }
    }

    bool Generator::codegen_binary(const common::BinaryExpression &expr) {
        using enum common::BinaryOp;
        if (!codegen_expression(*expr.lhs(), expr.op() == ASSIGN)) {
            return false;
        } else if (!codegen_expression(*expr.rhs())) {
            return false;
        }

        switch (expr.op()) {
        case ADD: push_binop(expr, vm::Op::ADD_I, vm::Op::ADD_F); break;
        case SUB: push_binop(expr, vm::Op::SUB_I, vm::Op::SUB_F); break;
        case MUL: push_binop(expr, vm::Op::MUL_I, vm::Op::MUL_F); break;
        case DIV: push_binop(expr, vm::Op::DIV_I, vm::Op::DIV_F); break;
        case REMAINDER: push_op(vm::Op::REM); break;
        case AND: push_op(vm::Op::BITWISE_AND); break;
        case OR: push_op(vm::Op::BITWISE_OR); break;
        case EQUALS: push_equals(expr.lhs()->type()); break;
        case NOT_EQUALS:
            push_equals(expr.lhs()->type());
            push_op(vm::Op::NOT);
            break;
        case LESS: push_binop(expr, vm::Op::LESS_I, vm::Op::LESS_F); break;
        case GREATER:
            push_binop(expr, vm::Op::GREATER_I, vm::Op::GREATER_F);
            break;
        case LESS_EQUALS:
            push_binop(expr, vm::Op::GREATER_I, vm::Op::GREATER_F);
            push_op(vm::Op::NOT);
            break;
        case GREATER_EQUALS:
            push_binop(expr, vm::Op::LESS_I, vm::Op::LESS_F);
            push_op(vm::Op::NOT);
            break;
        case ASSIGN: push_assign(expr.lhs()->type()); break;
        case BITWISE_AND: push_op(vm::Op::BITWISE_AND); break;
        case BITWISE_OR: push_op(vm::Op::BITWISE_OR); break;
        default: report_error("unlnown binary operator"); return false;
        }
        return true;
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

    bool Generator::codegen_call(const common::FunctionCall &call) {
        const auto &args = call.arguments();
        for (const auto &arg : args) {
            if (!codegen_expression(*arg)) {
                return false;
            }
        }

        auto idx = get_func_idx(call.id());
        if (!idx) {
            return false;
        }
        push_op(vm::Op::PUSH, *idx);
        push_op(vm::Op::CALL);
        return false;
    }

    bool Generator::push_allocate(const common::Type *type) {
        auto type_idx = get_type_info_idx(type);
        if (!type_idx) {
            return false;
        }

        if (type->kind() == common::TypeKind::ARRAY) {
            const common::ArrayType
                *arr = dynamic_cast<const common::ArrayType *>(type);
            push_op(vm::Op::PUSH, arr->count());
            push_op(vm::Op::ALLOCATE_ARRAY, *type_idx);
        } else {
            push_op(vm::Op::ALLOCATE, *type_idx);
        }

        return true;
    }

    bool Generator::codegen_var(const common::Variable &var) {
        if (!push_allocate(var.type)) {
            return false;
        }

        push_op(vm::Op::DUP);
        if (var.initial_value) {
            if (!codegen_expression(*var.initial_value)) {
                return false;
            }
        }

        push_assign(var.type);
        return true;
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

    bool Generator::codegen_branch(const common::Branch &branch) {
        if (!codegen_expression(*branch.predicate())) {
            return false;
        }

        push_op(vm::Op::NOT);
        vm::Instruction &false_branch = push_op(vm::Op::BRANCH);

        if (!codegen_block(branch.true_branch())) {
            return false;
        }

        vm::Instruction *end = nullptr;

        if (branch.false_branch()) {
            end = &push_op(vm::Op::JUMP);
        }

        false_branch.arg = output.size();

        if (branch.false_branch()) {
            if (!codegen_block(*branch.false_branch())) {
                return false;
            }
            // FIXME: this will jump out of bounds if the branch is the last
            // statement in a fucntion
            end->arg = output.size();
        }
    }

    bool Generator::codegen_block(const common::Block &block) {
        if (!block.reachable()) {
            return true;
        }

        for (const auto &stmt : block.statements()) {
            if (!stmt->reachable()) {
                break;
            }

            if (!codegen_statement(*stmt)) {
                return false;
            }
        }

        return true;
    }

    bool Generator::codegen_loop(const common::Loop &loop) {
        if (loop.init()) {
            if (!codegen_statement(*loop.init())) {
                return false;
            }
        }

        break_jumps.push_back(std::vector<vm::Instruction *>{});
        continue_jumps.push_back(std::vector<vm::Instruction *>{});
        uint64_t start = output.size();
        if (loop.condition()) {
            if (!codegen_expression(*loop.condition())) {
                return false;
            }
            push_op(vm::Op::NOT);

            break_jumps.back().push_back(&push_op(vm::Op::BRANCH));
        }

        if (!codegen_block(loop.body())) {
            return false;
        }

        for (auto jump : continue_jumps.back()) {
            jump->arg = output.size();
        }
        continue_jumps.pop_back();

        if (loop.iteration()) {
            if (!codegen_expression(*loop.iteration())) {
                return false;
            }
        }

        push_op(vm::Op::JUMP, start);
        for (auto jump : break_jumps.back()) {
            jump->arg = output.size();
        }
        break_jumps.pop_back();
        return true;
    }

    bool Generator::codegen_statement(const common::Statement &smt) {
        switch (smt.kind()) {
        case common::StatementType::RETURN: {
            const common::Return &ret = dynamic_cast<const common::Return &>(
                smt);
            if (ret.expression()) {
                if (!codegen_expression(*ret.expression())) {
                    return false;
                }

                push_op(vm::Op::RETURN, 1);
            } else {
                push_op(vm::Op::RETURN);
            }
            break;
        }
        case common::StatementType::EXPRESSION: {
            const common::Expression
                *expr = dynamic_cast<const common::ExpressionStatement &>(smt)
                            .expression();
            codegen_expression(*expr);
            if (expr->type()) {
                push_op(vm::Op::POP);
            }
            break;
        }
        case common::StatementType::VARIABLE:
            return codegen_var(*ast_->get_var(
                dynamic_cast<const common::VariableDeclatarion &>(smt)
                    .variable()));
        case common::StatementType::BRANCH:
            return codegen_branch(dynamic_cast<const common::Branch &>(smt));
            break;
        case common::StatementType::LOOP:
            return codegen_loop(dynamic_cast<const common::Loop &>(smt));
            break;
        case common::StatementType::BREAK:
            break_jumps.back().push_back(&push_op(vm::Op::JUMP));
            break;
        case common::StatementType::CONTINUE:
            continue_jumps.back().push_back(&push_op(vm::Op::JUMP));
            break;
        default: report_error("statement type not supported"); return false;
        }

        return true;
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
                dynamic_cast<const common::PrimitiveType &>(*type).name());
            break;
        case common::TypeKind::POINTER:
            codegen_type(dynamic_cast<const common::PointerType &>(*type)
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
                dynamic_cast<const common::PointerType &>(*type)
                    .pointee_type());
        }
        if (type->kind() != common::TypeKind::ARRAY) {
            report_error("type declaration generation: unsupported type kind");
            return false;
        }

        const common::ArrayType
            &array = dynamic_cast<const common::ArrayType &>(*type);
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
                dynamic_cast<const common::PrimitiveType &>(*type).name());
        case common::TypeKind::POINTER: {
            const common::PointerType
                &ptr = dynamic_cast<const common::PointerType &>(*type);
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
                &array = dynamic_cast<const common::ArrayType &>(*type);
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

    bool
    Generator::codegen_index_expression(const common::IndexExpression &expr) {
        codegen_expression(*expr.container());
        if (error_occured()) {
            return false;
        }
        *body_ << ".data[";
        if (expr.index()->kind() != common::ExpressionKind::LITERAL) {
            *body_ << "check_index(";
            codegen_expression(*expr.index());
            if (error_occured()) {
                return false;
            }
            *body_ << ", "
                   << std::to_string(dynamic_cast<const common::ArrayType &>(
                                         *expr.container()->type())
                                         .count())
                   << ')';
        } else {
            codegen_expression(*expr.index());
            if (error_occured()) {
                return false;
            }
        }
        *body_ << ']';
        return true;
    }

    void Generator::codegen_var_name(common::IdentifierID name) {
        *body_ << "var_" << *identifiers_->get(name);
    }

    void Generator::codegen_func_name(common::IdentifierID name) {
        *body_ << "func_" << *identifiers_->get(name);
    }
} // namespace codegen
