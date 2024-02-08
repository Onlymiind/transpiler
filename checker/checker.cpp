#include "checker/checker.h"
#include "common/ast.h"
#include "common/base_classes.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/module.h"
#include "common/parsed_types.h"
#include "common/statement.h"
#include "common/types.h"
#include "common/util.h"

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <sys/types.h>

namespace checker {

#define CONCATENATE_TOKENS_INNER(prefix, postfix) prefix##postfix
#define CONCATENATE_TOKENS(prefix, postfix)                                    \
    CONCATENATE_TOKENS_INNER(prefix, postfix)
#define ERROR_GUARD(position)                                                  \
    common::RAIIGuard CONCATENATE_TOKENS(error_guard_, __LINE__) {             \
        std::bind([this](size_t pos) { err_positions_.push(pos); }, position), \
            [this]() { err_positions_.pop(); }                                 \
    }
#define REACHABILITY_GUARD(reachability)                                       \
    common::RAIIGuard CONCATENATE_TOKENS(reachability_guard_, __LINE__) {      \
        std::bind([this](Reachability                                          \
                             reach) { reachability_stack_.push(reach); },      \
                  reachability),                                               \
            [this]() { reachability_stack_.pop(); }                            \
    }
#define SCOPE_GUARD()                                                          \
    common::RAIIGuard CONCATENATE_TOKENS(scope_guard_, __LINE__) {             \
        [this]() { module_.push_scope(); }, [this]() { module_.pop_scope(); }  \
    }

    Checker::Checker(common::AST &ast, common::Identifiers &identifiers,
                     bool do_constant_folding)
        : ast_(&ast), identifiers_(&identifiers),
          do_constant_folding_(do_constant_folding) {
        std::vector<common::PrimitiveType>
            primitives{common::PrimitiveType(identifiers_->add("u64"),
                                             common::BuiltinTypes::UINT,
                                             common::TypeTraits::INTEGER, 8),
                       common::PrimitiveType(identifiers_->add("f64"),
                                             common::BuiltinTypes::FLOAT,
                                             common::TypeTraits::FLOATING_POINT,
                                             8),
                       common::PrimitiveType(identifiers_->add("bool"),
                                             common::BuiltinTypes::BOOL,
                                             common::TypeTraits::BOOLEAN, 1)};
        global_types_ = std::make_unique<common::Global>(std::move(primitives));
        for (const auto &[name, type] : global_types_->primitives()) {
            module_.add_type(name, &type);
            builtin_types_[type.type()] = &type;
        }
    }

    void Checker::add_declarations() {
        auto &functions = ast_->functions();
        for (auto &func : functions) {
            if (!check_function_decl(func)) {
                return;
            }
        }

        auto &variables = ast_->global_variables();
        for (common::VariableID var_id : variables) {
            common::Variable &var = *ast_->get_var(var_id);
            ERROR_GUARD(var.pos);
            if (!check_variable(var)) {
                return;
            }
            if (var.initial_value && !var.initial_value->is_error() &&
                var.initial_value->kind() != common::ExpressionKind::LITERAL) {
                report_error("global variable initializer must be a constant "
                             "expression");
                return;
            }
        }
    }

    void Checker::check() {
        REACHABILITY_GUARD(Reachability::REACHABLE);
        add_declarations();
        if (!err_.empty()) {
            return;
        }
        auto &functions = ast_->functions();
        for (auto &func : functions) {
            ERROR_GUARD(func.pos);
            if (!check_function(func)) {
                return;
            }

            if (*identifiers_->get(func.name) == "main") {
                module_.entrypoint(func.id);
            }
        }

        if (module_.entrypoint() == common::FunctionID{}) {
            ERROR_GUARD(0);
            report_error("entrypoint not declared");
        }
    }

    bool Checker::check_expression(std::unique_ptr<common::Expression> &expr) {
        if (!expr) {
            return false;
        }
        if (expr->is_error()) {
            return false;
        }
        ERROR_GUARD(expr->pos());
        bool result = false;
        switch (expr->kind()) {
        case common::ExpressionKind::LITERAL:
            return set_literal_type(common::downcast<common::Literal>(*expr));
        case common::ExpressionKind::UNARY: {
            common::UnaryExpression
                &unary = common::downcast<common::UnaryExpression>(*expr);
            result = check_unary_expression(unary);
            if (result && do_constant_folding_) {
                if (auto computed = try_compute(unary)) {
                    computed->type(expr->type());
                    expr = std::move(computed);
                }
            }
            break;
        }
        case common::ExpressionKind::BINARY: {
            common::BinaryExpression
                &binary = common::downcast<common::BinaryExpression>(*expr);
            result = check_binary_expression(binary);
            if (result && do_constant_folding_) {
                if (auto computed = try_compute(binary)) {
                    computed->type(expr->type());
                    expr = std::move(computed);
                }
            }
            break;
        }
        case common::ExpressionKind::CAST: {
            common::Cast &cast = common::downcast<common::Cast>(*expr);
            result = check_cast(cast);
            if (result && do_constant_folding_) {
                if (auto computed = try_compute(cast)) {
                    computed->type(expr->type());
                    expr = std::move(computed);
                }
            }
            break;
        }
        case common::ExpressionKind::FUNCTION_CALL:
            return check_function_call(
                common::downcast<common::FunctionCall>(*expr));
        case common::ExpressionKind::VARIABLE_REF:
            return check_variable_ref(
                common::downcast<common::VariableReference>(*expr));
        default: report_error("unknown expression type"); return false;
        }
        return result;
    }

    bool Checker::check_unary_expression(common::UnaryExpression &expr) {
        if (!expr.expression() || expr.expression()->is_error()) {
            return false;
        }
        if (!check_expression(expr.expression())) {
            return false;
        }

        switch (expr.op()) {
        case common::UnaryOp::NOT:
            if (!expr.expression()->type()->has_trait(
                    common::TypeTraits::BOOLEAN)) {
                report_error("operator ! is defined only for booleans");
                return false;
            }
            expr.type(expr.expression()->type());
            break;
        case common::UnaryOp::NEGATE:
            // no signed integer type for now
            if (!expr.expression()->type()->has_trait(
                    common::TypeTraits::FLOATING_POINT)) {
                report_error("unary - is defined only for floats");
                return false;
            }
            expr.type(expr.expression()->type());
            break;
        case common::UnaryOp::ADDRESS_OF:
            if (!expr.expression()->is_lvalue()) {
                report_error("can't take address of an rvalue");
                return false;
            }
            expr.type(global_types_->get_pointer(expr.expression()->type()));
            break;
        case common::UnaryOp::DEREFERENCE:
            if (!expr.expression()->type()->is_pointer()) {
                report_error("can't dereference type that is not a pointer");
                return false;
            }
            {
                const common::PointerType
                    &ptr = common::downcast<common::PointerType>(
                        *expr.expression()->type());
                if (ptr.is_nullptr()) {
                    report_error("null dereference");
                    return false;
                }
                expr.type(ptr.pointee_type());
            }
            break;
        default: report_error("unknown unary operator"); return false;
        }
        return true;
    }

    bool Checker::check_binary_expression(common::BinaryExpression &expr) {
        if (!expr.lhs() || expr.lhs()->is_error() || !expr.rhs() ||
            expr.rhs()->is_error()) {
            return false;
        }

        if (!check_expression(expr.lhs())) {
            return false;
        }
        if (!check_expression(expr.rhs())) {
            return false;
        }

        const common::Type *lhs = expr.lhs()->type();
        const common::Type *rhs = expr.rhs()->type();

        if (lhs->is_pointer() && rhs->is_pointer()) {
            if (common::downcast<common::PointerType>(*lhs).is_nullptr()) {
                lhs = rhs;
                expr.lhs()->type(rhs);
            } else if (common::downcast<common::PointerType>(*rhs)
                           .is_nullptr()) {
                rhs = lhs;
                expr.rhs()->type(lhs);
            } else if (lhs != rhs) {
                return false;
            }

        } else if (lhs != rhs) {
            report_error("type mismatch: can not convert types");
            return false;
        }

        if (expr.op() == common::BinaryOp::ASSIGN) {
            if (!expr.lhs()->is_lvalue()) {
                report_error("can not assing to rvalue");
                return false;
            }
            return true;
        }

        if (common::is_equality_op(expr.op())) {
            // for now, every type is comparable
            // later maybe should add COMPARABLE type trait
            expr.type(builtin_types_[common::BuiltinTypes::BOOL]);
            return true;
        }

        if (lhs->is_pointer() || rhs->is_pointer()) {
            report_error("can not use pointers in binary expressions except "
                         "for assignment and equality tests");
            return false;
        }

        if (common::is_logic_op(expr.op())) {
            if (!lhs->has_trait(common::TypeTraits::BOOLEAN)) {

                report_error("type mismatch: expected boolean");
                return false;
            }
            expr.type(lhs);
            return true;
        } else if (common::is_relational(expr.op())) {
            if (!lhs->has_trait(common::TypeTraits::ORDERED)) {
                report_error("type mismatch: expected ordered type");
                return false;
            }
            expr.type(builtin_types_[common::BuiltinTypes::BOOL]);
            return true;
        } else if (common::is_bitwise(expr.op()) &&
                   !lhs->has_trait(common::TypeTraits::INTEGER)) {
            report_error("bitwise operations are allowed only on integers");
            return false;
        } else if (expr.op() == common::BinaryOp::REMAINDER &&
                   !lhs->has_trait(common::TypeTraits::INTEGER)) {
            report_error(
                "type mismatch: expected integer operands for % operator");
            return false;
        } else if (!lhs->has_trait(common::TypeTraits::NUMERIC)) {
            report_error("type mismatch: expected numeric type");
            return false;
        }

        expr.type(lhs);

        return true;
    }

    bool Checker::check_cast(common::Cast &cast) {
        if (!cast.to() || cast.to()->kind() != common::ParsedTypeKind::NAMED) {
            report_error("unsupported parsed type");
            return false;
        }

        common::ParsedNamedType
            &parsed = common::downcast<common::ParsedNamedType>(*cast.to());

        cast.type(get_type(*cast.to()));
        if (!cast.type()) {
            report_error("cast: unknown destination type");
            return false;
        }
        if (!check_expression(cast.from())) {
            return false;
        }
        // Trivial case
        if (cast.type() == cast.from()->type()) {
            return true;
        }

        if (cast.type()->is_pointer()) {
            if (!cast.from()->type()->is_pointer()) {
                report_error("conversion between non-pointer types and "
                             "pointers is not allowed");
                return false;
            } else if (!common::downcast<common::PointerType>(
                            *cast.from()->type())
                            .is_nullptr()) {
                report_error("can not convert between different pointer types");
                return false;
            }
            return true;
        }
        if (cast.type()->has_trait(common::TypeTraits::BOOLEAN) ||
            cast.from()->type()->has_trait(common::TypeTraits::BOOLEAN)) {
            report_error(
                "conversion to and from boolean types are not allowed");
            return false;
        }
        return true;
    }

    bool Checker::set_literal_type(common::Literal &lit) {
        if (lit.is<bool>()) {
            lit.type(builtin_types_.at(common::BuiltinTypes::BOOL));
        } else if (lit.is<uint64_t>()) {
            lit.type(builtin_types_.at(common::BuiltinTypes::UINT));
        } else if (lit.is<double>()) {
            lit.type(builtin_types_.at(common::BuiltinTypes::FLOAT));
        } else if (lit.is<std::nullptr_t>()) {
            lit.type(global_types_->get_pointer(nullptr));
        } else {
            report_error("unknown literal type");
            return false;
        }
        return true;
    }

    bool Checker::check_function_call(common::FunctionCall &call) {
        common::FunctionID func_id = module_.get_function(call.name());
        if (func_id == common::FunctionID{}) {
            report_error(*identifiers_->get(call.name()) +
                         " is not a function");
            return false;
        }

        common::Function &func = *ast_->get_function(func_id);
        call.id(func.id);
        if (func.decl_only) {
            // function declared, but not defined
            report_error("function not defined");
            return false;
        }
        if (*identifiers_->get(func.name) == "main") {
            report_error("main() must not be called");
            return false;
        }
        auto &args = call.arguments();
        if (args.size() != func.params.size()) {
            report_error("function call argument count mismatch, expected " +
                         std::to_string(func.params.size()) + ", got " +
                         std::to_string(args.size()));
            return false;
        }
        for (size_t i = 0; i < func.params.size(); ++i) {
            ERROR_GUARD(args[i]->pos());
            common::Variable &param = *ast_->get_var(func.params[i]);

            if (!check_expression(args[i])) {
                return false;
            }
            if (args[i]->type() != param.type) {
                report_error("function call argument's type mismatch");
                return false;
            }
        }
        call.type(func.return_type);
        return true;
    }

    bool Checker::check_function(common::Function &func) {
        SCOPE_GUARD();
        REACHABILITY_GUARD(Reachability::REACHABLE);
        current_function_ = func.id;
        for (common::VariableID param_id : func.params) {
            // All of the checks are made in check_function_decl, here just add
            // the parameters to the scope
            module_.add_variable(ast_->get_var(param_id)->name, param_id);
        }
        if (!check_block(func.body)) {
            return false;
        }
        if (func.return_type &&
            reachability_stack_.top() != Reachability::RETURNS) {
            report_error("missing return statement in non-void function");
            return false;
        }
        return true;
    }

    bool Checker::check_variable_ref(common::VariableReference &name) {
        common::VariableID var_id = module_.get_variable(name.name());
        if (var_id == common::VariableID{}) {
            report_error("identifier does not name a variable");
            return false;
        }

        common::Variable &var = *ast_->get_var(var_id);
        name.id(var.id);
        name.type(var.type);
        return true;
    }

    bool Checker::check_function_decl(common::Function &func) {
        ERROR_GUARD(func.pos);
        if (!module_.add_function(func.name, func.id)) {
            report_error("can not declare a function: name " +
                         *identifiers_->get(func.name) + " already used");
            return false;
        }
        if (*identifiers_->get(func.name) == "main") {
            if (func.parsed_return_type) {
                report_error("main() must return void");
                return false;
            } else if (!func.params.empty()) {
                report_error("main() must have zero arguments");
                return false;
            }
        }
        if (func.parsed_return_type) {
            func.return_type = get_type(*func.parsed_return_type);
            if (!func.return_type) {
                report_error("unknown return type");
                return false;
            }
        }

        // NOTE: maybe it wasn't a good idea to get rid of persistently stored
        // scopes, since this forces to loop over functions' arguments twice:
        // one time at declaration check and once to add all of the parameters
        for (common::VariableID param_id : func.params) {
            common::Variable &param = *ast_->get_var(param_id);
            ERROR_GUARD(param.pos);
            if (module_.has_name(param.name)) {
                report_error(
                    "can't declare function parameter: name already declared");
                return false;
            }
            if (!param.explicit_type) {
                report_error("function parameters must be explicitly typed");
                return false;
            }

            param.type = get_type(*param.explicit_type);
            if (!param.type) {
                report_error("invalid function parameter type");
                return false;
            }
            if (param.initial_value) {
                report_error(
                    "default values for function parameters are not supported");
                return false;
            }
        }
        return true;
    }

    bool Checker::check_branch(common::Branch &branch) {
        if (!check_expression(branch.predicate())) {
            return false;
        } else if (!branch.predicate()->type()->has_trait(
                       common::TypeTraits::BOOLEAN)) {
            ERROR_GUARD(branch.predicate()->pos());
            report_error("expected boolean expression in an if statement");
            return false;
        }

        Reachability reachability = reachability_stack_.top();
        {
            SCOPE_GUARD();
            REACHABILITY_GUARD(reachability_stack_.top());
            if (!check_block(branch.true_branch())) {
                return false;
            }
            reachability = reachability_stack_.top();
        }
        if (!branch.false_branch()) {
            return true;
        }

        // else if: do not push unneded scope
        if (branch.false_branch()->statements().size() == 1 &&
            branch.false_branch()->statements()[0]->kind() ==
                common::StatementType::BRANCH) {
            if (!check_branch(common::downcast<common::Branch>(
                    *branch.false_branch()->statements()[0]))) {
                return false;
            }
            reachability = unite_reachability(reachability,
                                              reachability_stack_.top());
        } else {
            SCOPE_GUARD();
            REACHABILITY_GUARD(reachability_stack_.top());
            if (!check_block(*branch.false_branch())) {
                return false;
            }
            reachability = unite_reachability(reachability,
                                              reachability_stack_.top());
        }
        reachability_stack_.top() = reachability;
        return true;
    }

    bool Checker::check_statement(common::Statement &smt) {
        ERROR_GUARD(smt.pos());
        switch (smt.kind()) {
        case common::StatementType::EXPRESSION:
            return check_expression(
                common::downcast<common::ExpressionStatement>(smt)
                    .expression());
        case common::StatementType::BRANCH:
            return check_branch(common::downcast<common::Branch>(smt));
        case common::StatementType::RETURN: {
            if (is_reachable()) {
                reachability_stack_.top() = Reachability::RETURNS;
            }
            common::Function &func = *ast_->get_function(current_function_);
            auto &ret_expr = common::downcast<common::Return>(smt).expression();
            if (!ret_expr) {
                if (func.return_type) {
                    report_error("trying to return a value from "
                                 "function with no return type");
                    return false;
                }
                return true;
            }
            if (!check_expression(ret_expr)) {
                return false;
            }
            if (func.return_type != ret_expr->type()) {
                report_error("wrong return type");
                return false;
            }
            return true;
        }
        case common::StatementType::VARIABLE:
            return check_variable(*ast_->get_var(
                common::downcast<common::VariableDeclatarion>(smt).variable()));
        case common::StatementType::LOOP:
            return check_loop(common::downcast<common::Loop>(smt));
        case common::StatementType::BREAK: [[fallthrough]];
        case common::StatementType::CONTINUE:
            reachability_stack_.top() = Reachability::UNREACHABLE;
            if (loop_cout_ == 0) {
                report_error(
                    "'break' and 'continue' must be used only inside loops");
            }
            return true;
        default: report_error("unknown statement type"); return false;
        }
    }

    bool Checker::check_block(common::Block &block) {
        for (auto &smt : block.statements()) {
            smt->reachable(is_reachable());
            if (!check_statement(*smt)) {
                return false;
            }
        }
        return true;
    }

    bool Checker::check_variable(common::Variable &var) {
        if (!module_.add_variable(var.name, var.id)) {
            report_error("variable declaration: name already used");
            return false;
        }

        if (var.explicit_type) {
            var.type = get_type(*var.explicit_type);
            if (!var.type) {
                report_error("invalid variable type");
                return false;
            }
        }

        if (var.initial_value) {
            if (!check_expression(var.initial_value)) {
                return false;
            }
            if (var.initial_value->type()->is_pointer() &&
                common::downcast<common::PointerType>(
                    *var.initial_value->type())
                    .is_nullptr()) {
                if (!var.type) {
                    report_error("null must be explicitly converted when "
                                 "initializing implicitly-typed variable");
                    return false;
                }
                var.initial_value->type(var.type);
            }
        }
        if (var.type) {
            if (var.initial_value && var.type != var.initial_value->type()) {
                report_error("type mismatch: can not initialize variabe with "
                             "expression of wrong type");
                return false;
            }
        } else if (var.initial_value) {
            var.type = var.initial_value->type();
        } else {
            report_error("can not determine type for variable " +
                         *identifiers_->get(var.name));
            return false;
        }
        return true;
    }

    bool Checker::check_loop(common::Loop &loop) {
        SCOPE_GUARD();
        REACHABILITY_GUARD(reachability_stack_.top());
        if (loop.init()) {
            if (!check_statement(*loop.init())) {
                return false;
            }
        }
        if (loop.condition()) {
            if (!check_expression(loop.condition())) {
                return false;
            }
            ERROR_GUARD(loop.condition()->pos());
            if (!loop.condition()->type()->has_trait(
                    common::TypeTraits::BOOLEAN)) {
                report_error("loop's condition must have boolean type");
                return false;
            }
        }
        if (loop.iteration()) {
            if (!check_expression(loop.iteration())) {
                return false;
            }
        }
        ++loop_cout_;
        bool result = check_block(loop.body());
        --loop_cout_;
        return result;
    }

    Reachability Checker::unite_reachability(Reachability lhs,
                                             Reachability rhs) {
        if (lhs == rhs) {
            return lhs;
        } else if (lhs == Reachability::REACHABLE ||
                   rhs == Reachability::REACHABLE) {
            return Reachability::REACHABLE;
        }
        return Reachability::UNREACHABLE;
    }

    std::unique_ptr<common::Expression>
    Checker::try_compute(common::UnaryExpression &expr) {
        if (expr.expression()->kind() != common::ExpressionKind::LITERAL) {
            return nullptr;
        }

        common::Literal &lit = common::downcast<common::Literal>(
            *expr.expression());
        switch (expr.op()) {
        case common::UnaryOp::NEGATE:
            return std::make_unique<common::Literal>(-*lit.get<double>(),
                                                     expr.pos());
        case common::UnaryOp::NOT:
            return std::make_unique<common::Literal>(!*lit.get<bool>(),
                                                     expr.pos());
        default: return nullptr;
        }
    }

    std::unique_ptr<common::Expression>
    Checker::try_compute(common::BinaryExpression &expr) {
        if (expr.lhs()->kind() != common::ExpressionKind::LITERAL ||
            expr.rhs()->kind() != common::ExpressionKind::LITERAL) {
            return nullptr;
        }

        common::Literal &lhs = common::downcast<common::Literal>(*expr.lhs());
        common::Literal &rhs = common::downcast<common::Literal>(*expr.rhs());
        if (!lhs.same_type(rhs)) {
            return nullptr;
        }

        auto do_rel_op = []<typename T>(common::BinaryOp op, T lhs,
                                        T rhs) -> std::optional<bool> {
            switch (op) {
            case common::BinaryOp::LESS: return lhs < rhs;
            case common::BinaryOp::LESS_EQUALS: return lhs <= rhs;
            case common::BinaryOp::GREATER: return lhs > rhs;
            case common::BinaryOp::GREATER_EQUALS: return lhs >= rhs;
            case common::BinaryOp::NOT_EQUALS: return lhs != rhs;
            case common::BinaryOp::EQUALS: return lhs == rhs;
            default: return {};
            }
        };
        auto do_integer_op = [this](common::BinaryOp op, uint64_t lhs,
                                    uint64_t rhs) -> std::optional<uint64_t> {
            switch (op) {
            case common::BinaryOp::BITWISE_OR: return lhs | rhs;
            case common::BinaryOp::BITWISE_AND: return lhs & rhs;
            case common::BinaryOp::REMAINDER:
                if (rhs == 0) {
                    report_error("division by zero");
                    return {};
                }
                return lhs % rhs;
            default: return {};
            }
        };
        auto do_op = [this]<typename T>(common::BinaryOp op, T lhs,
                                        T rhs) -> std::optional<T> {
            switch (op) {
            case common::BinaryOp::ADD: return lhs + rhs;
            case common::BinaryOp::SUB: return lhs - rhs;
            case common::BinaryOp::MUL: return lhs * rhs;
            case common::BinaryOp::DIV:
                if (rhs == T{0}) {
                    report_error("division by zero");
                    return {};
                }
                return lhs / rhs;
            case common::BinaryOp::AND: return lhs && rhs;
            case common::BinaryOp::OR: return lhs || rhs;
            default: return {};
            }
        };

        if (lhs.is<bool>()) {
            std::optional<bool> val;
            if (common::is_relational(expr.op())) {
                val = do_rel_op(expr.op(), *lhs.get<bool>(), *rhs.get<bool>());
            } else {
                val = do_op(expr.op(), *lhs.get<bool>(), *rhs.get<bool>());
            }
            if (!val) {
                return nullptr;
            }
            return std::make_unique<common::Literal>(*val, expr.pos());
        } else if (lhs.is<uint64_t>()) {
            if (common::is_relational(expr.op())) {
                std::optional<bool> val = do_rel_op(expr.op(),
                                                    *lhs.get<uint64_t>(),
                                                    *rhs.get<uint64_t>());
                if (!val) {
                    return nullptr;
                }
                return std::make_unique<common::Literal>(*val, expr.pos());
            } else {
                std::optional<uint64_t> val;
                if (common::is_bitwise(expr.op()) ||
                    expr.op() == common::BinaryOp::REMAINDER) {
                    val = do_integer_op(expr.op(), *lhs.get<uint64_t>(),
                                        *rhs.get<uint64_t>());
                } else {
                    val = do_op(expr.op(), *lhs.get<uint64_t>(),
                                *rhs.get<uint64_t>());
                }
                if (!val) {
                    return nullptr;
                }
                return std::make_unique<common::Literal>(*val, expr.pos());
            }
        } else if (lhs.is<double>()) {
            if (common::is_relational(expr.op())) {
                std::optional<bool> val = do_rel_op(expr.op(),
                                                    *lhs.get<double>(),
                                                    *rhs.get<double>());
                if (!val) {
                    return nullptr;
                }
                return std::make_unique<common::Literal>(*val, expr.pos());
            } else {
                std::optional<double> val = do_op(expr.op(), *lhs.get<double>(),
                                                  *rhs.get<double>());
                if (!val) {
                    return nullptr;
                }
                return std::make_unique<common::Literal>(*val, expr.pos());
            }
        }
        return nullptr;
    }

    std::unique_ptr<common::Expression>
    Checker::try_compute(common::Cast &cast) {
        if (cast.from()->kind() != common::ExpressionKind::LITERAL) {
            return nullptr;
        }
        const common::Type
            *floating_type = builtin_types_[common::BuiltinTypes::FLOAT];
        const common::Type
            *integer_type = builtin_types_[common::BuiltinTypes::UINT];
        const common::Type
            *boolean_type = builtin_types_[common::BuiltinTypes::BOOL];

        common::Literal &from = common::downcast<common::Literal>(*cast.from());
        std::optional<common::Literal> result;
        if (cast.type() == floating_type) {
            if (!from.is<uint64_t>() && !from.is<double>()) {
                return nullptr;
            } else if (uint64_t *uint = from.get<uint64_t>(); uint) {
                result = common::Literal{static_cast<double>(*uint),
                                         cast.pos()};
            } else {
                result = common::Literal{*from.get<double>(), cast.pos()};
            }
        } else if (cast.type() == integer_type) {
            if (!from.is<uint64_t>() && !from.is<double>()) {
                return nullptr;
            } else if (double *d = from.get<double>(); d) {
                result = common::Literal{static_cast<uint64_t>(*d), cast.pos()};
            } else {
                result = common::Literal{*from.get<uint64_t>(), cast.pos()};
            }
        } else if (cast.type() == boolean_type) {
            if (!from.is<bool>()) {
                return nullptr;
            }
            result = common::Literal{*from.get<bool>(), cast.pos()};
        }

        return result ? std::make_unique<common::Literal>(std::move(*result))
                      : nullptr;
    }

    const common::Type *Checker::get_type(const common::ParsedType &parsed) {
        // For now, just assert that parsed has a named base type
        const common::ParsedNamedType
            &named = common::downcast<common::ParsedNamedType>(parsed);

        const common::Type *result = module_.get_type(named.name());
        if (!result) {
            return nullptr;
        }
        for (size_t i = 0; i < named.indirection_level(); ++i) {
            result = global_types_->get_pointer(result);
        }
        return result;
    }

} // namespace checker
