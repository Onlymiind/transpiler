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

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
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
        std::bind([this](common::TokenPos pos) { err_positions_.push(pos); },  \
                  position),                                                   \
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
        std::vector<common::PrimitiveType> primitives{
            common::PrimitiveType(identifiers_->add("int"),
                                  common::BuiltinTypes::INT,
                                  common::TypeTraits::INTEGER, 8),
            common::PrimitiveType(identifiers_->add("float"),
                                  common::BuiltinTypes::FLOAT,
                                  common::TypeTraits::FLOATING_POINT, 8),
            common::PrimitiveType(identifiers_->add("bool"),
                                  common::BuiltinTypes::BOOL,
                                  common::TypeTraits::BOOLEAN, 1),
            common::PrimitiveType(identifiers_->add("char"),
                                  common::BuiltinTypes::CHAR,
                                  common::TypeTraits::INTEGER, 1),
        };
        global_types_ = std::make_unique<common::Global>(std::move(primitives));
        for (const auto &[name, type] : global_types_->primitives()) {
            module_.add_type(name, type);
            builtin_types_[type->type()] = type;
        }

        cap_name_ = identifiers_->add("cap");
        len_name_ = identifiers_->add("len");
        data_name_ = identifiers_->add("data");
        append_name_ = identifiers_->add("append");
        get_slice(builtin_types_[common::BuiltinTypes::CHAR]);
    }

    void Checker::add_declarations() {
        auto &structs = ast_->structs();
        for (auto &record : structs) {
            ERROR_GUARD(record.pos());
            common::StructType *ptr = module_.add_struct(
                common::StructType{record.name()});
            if (!ptr) {
                report_error("duplicate name used for struct declaration");
                return;
            }
            structs_[ptr] = &record;
        }

        for (auto [record, info] : structs_) {
            if (!check_struct(*record, *info)) {
                return;
            }
        }

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
            if (!module_.add_variable(var.name, var.id)) {
                report_error("name " + *identifiers_->get(var.name) +
                             " is already taken");
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

        auto &vars = ast_->global_variables();
        for (auto var : vars) {
            if (!check_variable(*ast_->get_var(var), true)) {
                return;
            }
        }

        auto &functions = ast_->functions();
        for (auto &func : functions) {
            ERROR_GUARD(func.pos);
            if (!check_function(func)) {
                return;
            }
        }
    }

    bool Checker::check_expression(std::unique_ptr<common::Expression> &expr,
                                   bool allow_assing) {
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
            result = set_literal_type(dynamic_cast<common::Literal &>(*expr));
            break;
        case common::ExpressionKind::UNARY: {
            common::UnaryExpression
                &unary = dynamic_cast<common::UnaryExpression &>(*expr);
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
                &binary = dynamic_cast<common::BinaryExpression &>(*expr);
            result = check_binary_expression(binary, allow_assing);
            if (result && do_constant_folding_) {
                if (auto computed = try_compute(binary)) {
                    computed->type(expr->type());
                    expr = std::move(computed);
                }
            }
            break;
        }
        case common::ExpressionKind::CAST: {
            common::Cast &cast = dynamic_cast<common::Cast &>(*expr);
            result = check_cast(cast);
            if (result && do_constant_folding_) {
                if (auto computed = try_compute(cast)) {
                    computed->type(expr->type());
                    expr = std::move(computed);
                }
            }
            break;
        }
        case common::ExpressionKind::INDEX:
            result = check_index_expression(
                dynamic_cast<common::IndexExpression &>(*expr));
            break;
        case common::ExpressionKind::FUNCTION_CALL:
            result = check_function_call(
                dynamic_cast<common::FunctionCall &>(*expr));
            break;
        case common::ExpressionKind::VARIABLE_REF:
            result = check_variable_ref(
                dynamic_cast<common::VariableReference &>(*expr));
            break;
        case common::ExpressionKind::MEMBER_ACCESS:
            result = check_member_access(expr);
            break;
        default: report_error("unknown expression type"); return false;
        }
        if (!expr->type() && !allow_assing) {
            report_error("expression does not evaluate to a type");
            return false;
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
        case common::UnaryOp::INVERT:
            if (!expr.expression()->type()->has_trait(
                    common::TypeTraits::INTEGER)) {
                report_error(
                    "bitwise operations are defined only for integers");
                return false;
            }
            expr.type(expr.expression()->type());
            break;
        case common::UnaryOp::NEGATE:
            // no signed integer type for now
            if (!expr.expression()->type()->has_trait(
                    common::TypeTraits::NUMERIC)) {
                report_error("unary '-' is defined only for numbers");
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
        case common::UnaryOp::DEREFERENCE: {
            if (!expr.expression()->type()->is_pointer()) {
                report_error("can't dereference type that is not a pointer");
                return false;
            }

            const common::PointerType
                &ptr = dynamic_cast<const common::PointerType &>(
                    *expr.expression()->type());
            if (ptr.is_nullptr()) {
                report_error("null dereference");
                return false;
            }
            if (ptr.pointee_type()->size() == 0) {
                report_error("cannot dereference pointer to zero-sized type");
                return false;
            }

            expr.type(ptr.pointee_type());
            break;
        }
        default: report_error("unknown unary operator"); return false;
        }
        return true;
    }

    bool Checker::check_binary_expression(common::BinaryExpression &expr,
                                          bool allow_assing) {
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

        if (!allow_assing && expr.op() == common::BinaryOp::ASSIGN) {
            report_error("assignment doesn't return a value and cannot be used "
                         "in expressions");
            return false;
        }

        // Note that `allow_assign` isn't passed further, so lhs and rhs are
        // non-null ath this point
        const common::Type *lhs = expr.lhs()->type();
        const common::Type *rhs = expr.rhs()->type();

        if (lhs->is_pointer() && rhs->is_pointer()) {
            if (dynamic_cast<const common::PointerType &>(*lhs).is_nullptr()) {
                lhs = rhs;
                expr.lhs()->type(rhs);
            } else if (dynamic_cast<const common::PointerType &>(*rhs)
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
        if (!cast.to()) {
            report_error(
                "missing destination type for a cast (probably a bug)");
            return false;
        }

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

        if ((!cast.type()->is_pointer() && !cast.type()->is_primitive()) ||
            (!cast.from()->type()->is_pointer() &&
             !cast.from()->type()->is_primitive())) {
            report_error(
                "conversion between non-primitive types is not allowed");
            return false;
        }

        if (cast.type()->is_pointer()) {
            if (!cast.from()->type()->is_pointer()) {
                report_error("conversion between non-pointer types and "
                             "pointers is not allowed");
                return false;
            } else if (!dynamic_cast<const common::PointerType &>(
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
        } else if (lit.is<int64_t>()) {
            lit.type(builtin_types_.at(common::BuiltinTypes::INT));
        } else if (lit.is<double>()) {
            lit.type(builtin_types_.at(common::BuiltinTypes::FLOAT));
        } else if (lit.is<std::nullptr_t>()) {
            lit.type(global_types_->get_pointer(nullptr));
        } else if (lit.is<char>()) {
            lit.type(builtin_types_.at(common::BuiltinTypes::CHAR));
        } else if (lit.is<common::StringID>()) {
            lit.type(get_slice(builtin_types_.at(common::BuiltinTypes::CHAR)));
        } else {
            report_error("unknown literal type");
            return false;
        }
        return true;
    }

    const common::Type *Checker::get_slice(const common::Type *element_type) {
        return global_types_->get_slice(builtin_types_.at(
                                            common::BuiltinTypes::INT),
                                        element_type, cap_name_, len_name_,
                                        data_name_);
    }

    bool Checker::check_function_call(common::FunctionCall &call) {
        ERROR_GUARD(call.pos());
        if (call.name() == append_name_) {
            auto &args = call.arguments();
            if (args.size() < 2) {
                report_error("'append' accepts at least 2 arguments");
                return false;
            }

            if (!check_expression(args[0])) {
                return false;
            }
            if (!args[0]->type()->is_slice()) {
                ERROR_GUARD(args[0]->pos());
                report_error("first argumant to 'append' must be a slice");
                return false;
            }

            const common::StructType
                *slice = dynamic_cast<const common::StructType *>(
                    args[0]->type());
            const common::Type
                *element_type = dynamic_cast<const common::PointerType *>(
                                    slice->get_field(data_name_)->type)
                                    ->pointee_type();

            for (size_t i = 1; i < args.size(); ++i) {
                ERROR_GUARD(args[i]->pos());
                if (!check_expression(args[i])) {
                    return false;
                }

                if (args[i]->type() != element_type) {
                    report_error("'append' argument type mismatch");
                    return false;
                }
            }

            call.type(args[0]->type());
            return true;
        }

        common::FunctionID func_id = module_.get_function(call.name());
        if (func_id == common::FunctionID{}) {
            report_error(*identifiers_->get(call.name()) +
                         " is not a function");
            return false;
        }

        common::Function &func = *ast_->get_function(func_id);
        call.id(func.id);
        if (func.decl_only && !func.is_native) {
            // function declared, but not defined
            report_error("function not defined");
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
                report_error("function call argument type mismatch");
                return false;
            }
        }
        call.type(func.return_type);
        return true;
    }

    bool Checker::check_function(common::Function &func) {
        if (func.decl_only) {
            // declaration has already been checked
            return true;
        }
        SCOPE_GUARD();
        REACHABILITY_GUARD(Reachability::REACHABLE);
        current_function_ = func.id;
        for (common::VariableID param_id : func.params) {
            // NOTE: by now parameters should be typechecked by
            // check_function_decl
            const common::Variable &param = *ast_->get_var(param_id);
            ERROR_GUARD(param.pos);
            if (!module_.add_variable(param.name, param_id)) {
                report_error("can not use name \"" +
                             *identifiers_->get(param.name) +
                             "\" because it is already declared");
                return false;
            }
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
        if (!var.type && var.explicit_type) {
            var.type = get_type(*var.explicit_type);
            if (!var.type) {
                return false;
            }
        } else if (!var.type) {
            report_error("reference to a variable without a type");
            return false;
        }
        name.id(var.id);
        name.type(var.type);
        return true;
    }

    bool Checker::check_function_decl(common::Function &func) {
        ERROR_GUARD(func.pos);
        if (func.name == append_name_) {
            report_error(
                "can not declare a function: name 'append' already used");
            return false;
        }

        if (!module_.add_function(func.name, func.id)) {
            report_error("can not declare a function: name '" +
                         *identifiers_->get(func.name) + "' already used");
            return false;
        }

        if (func.parsed_return_type) {
            func.return_type = get_type(*func.parsed_return_type);
            if (!func.return_type) {
                report_error("unknown return type");
                return false;
            }
            if (func.return_type->size() == 0) {
                report_error("function return type size must be non-zero");
                return false;
            }
        }

        // NOTE: maybe it wasn't a good idea to get rid of persistently stored
        // scopes, since this forces to loop over functions' arguments twice:
        // one time at declaration check and once to add all of the parameters
        for (common::VariableID param_id : func.params) {
            common::Variable &param = *ast_->get_var(param_id);
            ERROR_GUARD(param.pos);
            // NOTE: do not check for name redeclaration here, as at this point
            // not all functions and variables have been added to current module
            if (!param.explicit_type) {
                report_error("function parameters must be explicitly typed");
                return false;
            }

            param.type = get_type(*param.explicit_type);
            if (!param.type) {
                report_error("invalid function parameter type");
                return false;
            }
            if (param.type->size() == 0) {
                report_error("parameter type size must be non-zero");
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
            if (!check_branch(dynamic_cast<common::Branch &>(
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
            return check_expression(dynamic_cast<common::ExpressionStatement &>(
                                        smt)
                                        .expression(),
                                    true);
        case common::StatementType::BRANCH:
            return check_branch(dynamic_cast<common::Branch &>(smt));
        case common::StatementType::RETURN: {
            if (is_reachable()) {
                reachability_stack_.top() = Reachability::RETURNS;
            }
            common::Function &func = *ast_->get_function(current_function_);
            auto &ret_expr = dynamic_cast<common::Return &>(smt).expression();
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
                dynamic_cast<common::VariableDeclatarion &>(smt).variable()));
        case common::StatementType::LOOP:
            return check_loop(dynamic_cast<common::Loop &>(smt));
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

    bool Checker::check_variable(common::Variable &var, bool global) {
        if (!global && !module_.add_variable(var.name, var.id)) {
            report_error("variable declaration: name already used");
            return false;
        }

        if (var.explicit_type) {
            var.type = get_type(*var.explicit_type);
            if (!var.type) {
                report_error("invalid variable type");
                return false;
            }
            if (var.type->size() == 0) {
                report_error("cannot declare a variable with zero size");
                return false;
            }
        }

        if (var.initial_value) {
            if (!check_expression(var.initial_value)) {
                return false;
            }
            if (var.initial_value->type()->is_pointer() &&
                dynamic_cast<const common::PointerType &>(
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
            if (!check_expression(loop.iteration(), true)) {
                return false;
            }
        }
        ++loop_cout_;
        bool result = check_block(loop.body());
        --loop_cout_;
        return result;
    }

    bool Checker::check_index_expression(common::IndexExpression &expr) {
        if (!check_expression(expr.container()) ||
            !check_expression(expr.index())) {
            return false;
        }

        if (!expr.container()->type()->has_trait(
                common::TypeTraits::INDEXABLE)) {
            report_error(
                "index expressions are allowed only for array and slice types");
            return false;
        }

        if (expr.container()->type()->kind() == common::TypeKind::ARRAY) {
            const common::ArrayType
                &array = dynamic_cast<const common::ArrayType &>(
                    *expr.container()->type());
            expr.type(array.element_type());
        } else {
            const common::StructType
                &slice = dynamic_cast<const common::StructType &>(
                    *expr.container()->type());
            expr.type(dynamic_cast<const common::PointerType *>(
                          slice.get_field(data_name_)->type)
                          ->pointee_type());
        }

        if (!expr.index()->type()->has_trait(common::TypeTraits::INTEGER)) {
            report_error("index must be an integer");
            return false;
        }
        return true;
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

        common::Literal &lit = dynamic_cast<common::Literal &>(
            *expr.expression());
        switch (expr.op()) {
        case common::UnaryOp::NEGATE:
            if (lit.is<int64_t>()) {
                int64_t val = *lit.get<int64_t>();
                if (val != std::numeric_limits<int64_t>::lowest()) {
                    val = -val;
                }
                return std::make_unique<common::Literal>(val, expr.pos());
            } else if (lit.is<double>()) {
                return std::make_unique<common::Literal>(-*lit.get<double>(),
                                                         expr.pos());
            }
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

        common::Literal &lhs = dynamic_cast<common::Literal &>(*expr.lhs());
        common::Literal &rhs = dynamic_cast<common::Literal &>(*expr.rhs());
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
        } else if (lhs.is<int64_t>()) {
            if (common::is_relational(expr.op())) {
                std::optional<bool> val = do_rel_op(expr.op(),
                                                    *lhs.get<int64_t>(),
                                                    *rhs.get<int64_t>());
                if (!val) {
                    return nullptr;
                }
                return std::make_unique<common::Literal>(*val, expr.pos());
            } else {
                std::optional<int64_t> val;
                if (common::is_bitwise(expr.op()) ||
                    expr.op() == common::BinaryOp::REMAINDER) {
                    val = do_integer_op(expr.op(), *lhs.get<int64_t>(),
                                        *rhs.get<int64_t>());
                } else {
                    val = do_op(expr.op(), *lhs.get<int64_t>(),
                                *rhs.get<int64_t>());
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
            *integer_type = builtin_types_[common::BuiltinTypes::INT];
        const common::Type
            *boolean_type = builtin_types_[common::BuiltinTypes::BOOL];

        common::Literal &from = dynamic_cast<common::Literal &>(*cast.from());
        std::optional<common::Literal> result;
        if (cast.type() == floating_type) {
            if (!from.is<int64_t>() && !from.is<double>()) {
                return nullptr;
            } else if (int64_t *uint = from.get<int64_t>(); uint) {
                result = common::Literal{static_cast<double>(*uint),
                                         cast.pos()};
            } else {
                result = common::Literal{*from.get<double>(), cast.pos()};
            }
        } else if (cast.type() == integer_type) {
            if (!from.is<int64_t>() && !from.is<double>()) {
                return nullptr;
            } else if (double *d = from.get<double>(); d) {
                result = common::Literal{static_cast<int64_t>(*d), cast.pos()};
            } else {
                result = common::Literal{*from.get<int64_t>(), cast.pos()};
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

    const common::Type *Checker::get_type(common::ParsedType &parsed) {

        const common::Type *result = nullptr;
        switch (parsed.kind()) {
        case common::ParsedTypeKind::NAMED:
            result = module_.get_type(
                dynamic_cast<common::ParsedNamedType &>(parsed).name());
            break;
        case common::ParsedTypeKind::ARRAY: {
            common::ParsedArrayType
                &array = dynamic_cast<common::ParsedArrayType &>(parsed);
            if (!check_expression(array.size())) {
                return nullptr;
            }
            if (array.size()->kind() != common::ExpressionKind::LITERAL) {
                report_error("array size must be a costant expression");
                return nullptr;
            }
            const common::Literal &lit = dynamic_cast<const common::Literal &>(
                *array.size());
            if (!lit.is<int64_t>() || *lit.get<int64_t>() <= 0) {
                report_error("array size must be a positive integer");
                return nullptr;
            }
            size_t count = static_cast<size_t>(*lit.get<int64_t>());
            if (count == 0) {
                report_error("zero-sized arrays are not supported");
                return nullptr;
            }
            const common::Type *element = get_type(*array.element_type());
            if (!element) {
                return nullptr;
            }
            result = global_types_->get_array(count, element);
            break;
        }
        case common::ParsedTypeKind::SLICE: {
            common::ParsedSliceType
                &slice = dynamic_cast<common::ParsedSliceType &>(parsed);
            const common::Type *element_type = get_type(*slice.element_type());
            if (!element_type) {
                return nullptr;
            }
            result = get_slice(element_type);
            break;
        }
        default: report_error("unknown parsed type kind");
        }

        if (!result) {
            return nullptr;
        }
        for (size_t i = 0; i < parsed.indirection_level(); ++i) {
            result = global_types_->get_pointer(result);
        }
        return result;
    }

    bool Checker::check_struct(
        common::StructType &record, common::ParsedStructType &info,
        std::unordered_set<const common::StructType *> *in_progress) {
        if (!in_progress) {
            std::unordered_set<const common::StructType *> in_progress;
            return check_struct(record, info, &in_progress);
        }
        if (record.is_defined()) {
            return true;
        }

        in_progress->insert(&record);

        std::vector<common::Variable> checked_fields;
        checked_fields.reserve(info.fields().size());

        auto &fields = info.fields();
        for (auto &field : fields) {
            ERROR_GUARD(field.pos);

            field.type = get_type(*field.explicit_type);
            if (!field.type) {
                return false;
            }

            const common::StructType *struct_ptr = nullptr;
            if (field.type->kind() == common::TypeKind::STRUCT) {
                struct_ptr = dynamic_cast<const common::StructType *>(
                    field.type);
            } else if (field.type->kind() == common::TypeKind::ARRAY) {
                auto array = dynamic_cast<const common::ArrayType *>(
                    field.type);
                if (array->element_type()->kind() == common::TypeKind::STRUCT) {
                    struct_ptr = dynamic_cast<const common::StructType *>(
                        array->element_type());
                }
            }

            if (!struct_ptr) {
                continue;
            }

            if (in_progress->contains(struct_ptr)) {
                report_error(
                    "infinite loop detected in struct field declaration");
                return false;
            }

            auto it = structs_.find(
                const_cast<common::StructType *>(struct_ptr));
            if (it == structs_.end()) {
                report_error("internal error: unknown struct");
            }

            auto [typ, parsed] = *it;
            if (!check_struct(*typ, *parsed, in_progress)) {
                return false;
            }
            if (typ->size() == 0) {
                report_error("record fields cannot have zero-sized type");
                return false;
            }
        }

        record.add_fields(std::move(info.fields()));
        return true;
    }

    bool
    Checker::check_member_access(std::unique_ptr<common::Expression> &access) {
        common::MemberAccess *access_ptr = dynamic_cast<common::MemberAccess *>(
            access.get());
        if (!access_ptr) {
            return false;
        }

        ERROR_GUARD(access_ptr->pos());
        check_expression(access_ptr->record());

        if (access_ptr->member()->kind() ==
            common::ExpressionKind::FUNCTION_CALL) {
            auto call_ptr = std::move(access_ptr->member());
            auto call = dynamic_cast<common::FunctionCall *>(call_ptr.get());
            call->arguments().insert(call->arguments().begin(),
                                     std::move(access_ptr->record()));

            access = std::move(call_ptr);
            return check_function_call(*call);
        } else if (access_ptr->member()->kind() !=
                   common::ExpressionKind::VARIABLE_REF) {
            report_error("expected a function call or member reference after a "
                         "'.' operator");
            return false;
        }

        common::IdentifierID name = dynamic_cast<common::VariableReference *>(
                                        access_ptr->member().get())
                                        ->name();
        access_ptr->member_name(name);

        const common::Type *type = access_ptr->record()->type();
        if (type->kind() == common::TypeKind::POINTER) {
            auto ptr_type = dynamic_cast<const common::PointerType *>(type);
            if (ptr_type->is_nullptr()) {
                report_error("nullptr does not have any members");
                return false;
            } else if (ptr_type->pointee_type()->kind() !=
                       common::TypeKind::STRUCT) {
                report_error("dot operator can only be applied to structs and "
                             "pointers to structs");
                return false;
            }

            auto deref = std::make_unique<
                common::UnaryExpression>(common::UnaryOp::DEREFERENCE,
                                         std::move(access_ptr->record()),
                                         access_ptr->pos());
            if (!check_unary_expression(*deref)) {
                return false;
            }
            type = deref->type();
            access_ptr->record(std::move(deref));
        }

        if (type->kind() == common::TypeKind::ARRAY && name == len_name_) {
            size_t size = dynamic_cast<const common::ArrayType *>(type)
                              ->count();
            access = std::make_unique<common::Literal>(static_cast<int64_t>(
                                                           size),
                                                       access->pos());
            access->type(builtin_types_[common::BuiltinTypes::INT]);
            return true;
        }

        if (type->kind() != common::TypeKind::STRUCT) {
            report_error("dot operator can only be applied to structs and "
                         "pointers to structs");
            return false;
        }

        auto struct_ptr = dynamic_cast<const common::StructType *>(type);
        auto field = struct_ptr->get_field(name);
        if (!field) {
            report_error("field with name " + *identifiers_->get(name) +
                         " is not defined on this struct");
            return false;
        } else if (field->has_flag(common::FieldFlags::HIDDEN)) {
            report_error("field with name " + *identifiers_->get(name) +
                         " is inaccessible");
            return false;
        }
        access_ptr->type(field->type);

        return true;
    }
} // namespace checker
