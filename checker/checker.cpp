#include "checker/checker.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/scope.h"
#include "common/statement.h"
#include "common/types.h"
#include "common/util.h"
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <sys/types.h>

namespace checker {

    void Checker::add_builtins() {
        module_.make_scope();
        common::ScopeID global = module_.global_scope()->id();
        builtin_types_[common::BuiltinTypes::BOOL] = common::Type{common::Symbol{
            global,
            module_.global_scope()->add(common::BuiltinType{
                .name = identifiers_->add("bool"),
                .type = common::BuiltinTypes::BOOL,
                .traits = common::TypeTraits::BOOLEAN,
            })}};
        builtin_types_[common::BuiltinTypes::UINT] = common::Type{common::Symbol{
            global,
            module_.global_scope()->add(common::BuiltinType{
                .name = identifiers_->add("u64"),
                .type = common::BuiltinTypes::UINT,
                .traits = common::TypeTraits::INTEGER,
            })}};
        builtin_types_[common::BuiltinTypes::FLOAT] = common::Type{common::Symbol{
            global,
            module_.global_scope()->add(common::BuiltinType{
                .name = identifiers_->add("f64"),
                .type = common::BuiltinTypes::FLOAT,
                .traits = common::TypeTraits::FLOATING_POINT,
            })}};
    }

    void Checker::add_declarations() {
        add_builtins();
        auto &functions = ast_->functions();
        for (auto &func : functions) {
            check_function_decl(func);
            if (!err_.empty()) {
                return;
            }
        }

        auto &variables = ast_->global_variables();
        for (common::VariableID var_id : variables) {
            // TODO: this should be handled by check_variable()
            common::Variable &var = *ast_->get_var(var_id);
            ErrorGuard eg{*this, var.pos};
            if (common::Symbol sym = module_.find(var.name); !sym.is_error()) {
                report_error("can not declare a variable: name " + *identifiers_->get(var.name) + " already used");
                return;
            }

            var.type = common::Type{module_.find(var.explicit_type.name), var.explicit_type.indirection_level};
            if (var.type.is_error()) {
                report_error("variable type not declared");
                return;
            }
            if (common::Scope::type(var.type.sym.id) != common::SymbolType::BUILTIN_TYPE) {
                report_error("invalid variable type");
                return;
            }
            module_.global_scope()->add(var.name, var.id);
        }
    }

    void Checker::check() {
        add_declarations();
        if (!err_.empty()) {
            return;
        }
        ScopeGuard g{*this, module_.global_scope()->id(), Reachability::REACHABLE};
        auto &functions = ast_->functions();
        for (auto &func : functions) {
            ErrorGuard eg{*this, func.pos};
            check_function(func);
            if (!err_.empty()) {
                return;
            }

            if (*identifiers_->get(func.name) == "main") {
                module_.set_entrypoint(func.id);
            }
        }

        if (module_.entrypoint() == common::FunctionID{}) {
            ErrorGuard eg1{*this, 1};
            report_error("entrypoint not declared");
        }
    }

    common::Type Checker::check_expression(common::Expression &expr) {
        if (expr.is_error()) {
            return common::Type{};
        }

        common::Type result{};
        ErrorGuard g{*this, expr.pos};
        switch (expr.kind) {
        case common::ExpressionKind::LITERAL:
            result = get_type_for_literal(*ast_->get_literal(expr.id));
            break;
        case common::ExpressionKind::UNARY: {
            common::UnaryExpression &unary = *ast_->get_unary_expression(expr.id);
            result = check_unary_expression(unary);
            if (!result.is_error() && do_constant_folding_) {
                try_compute(unary, expr);
            }
            break;
        }
        case common::ExpressionKind::BINARY: {
            common::BinaryExpression &binary = *ast_->get_binary_expression(expr.id);
            result = check_binary_expression(binary);
            if (!result.is_error() && do_constant_folding_) {
                try_compute(binary, expr);
            }
            break;
        }
        case common::ExpressionKind::CAST: {
            common::Cast &cast = *ast_->get_cast(expr.id);
            result = check_cast(cast);
            if (!result.is_error() && do_constant_folding_) {
                try_compute(cast, expr);
            }
            break;
        }
        case common::ExpressionKind::FUNCTION_CALL:
            result = check_function_call(*ast_->get_call(expr.id), expr);
            break;
        case common::ExpressionKind::VARIABLE_REF:
            result = check_variable_ref(ast_->get_variable_ref(expr.id));
            break;
        case common::ExpressionKind::EMPTY:
            result = common::Type{common::g_void};
            break;
        default:
            report_error("unknown expression type");
            return common::Type{};
        }

        if (result.is_error()) {
            return result;
        }
        expr.type = result;
        return result;
    }

    common::Type Checker::check_unary_expression(common::UnaryExpression &expr) {
        if (expr.expr.is_error()) {
            return common::Type{};
        }
        common::Type type = check_expression(expr.expr);
        if (type.is_error()) {
            return common::Type{};
        }

        common::TypeTraits traits = module_.get_scope(type.sym.scope)->get_traits(type.sym.id);
        switch (expr.op) {
        case common::UnaryOp::NOT:
            if (common::empty(traits & common::TypeTraits::BOOLEAN)) {
                report_error("operator ! is defined only for booleans");
                return common::Type{};
            }
            return type;
        case common::UnaryOp::NEGATE:
            // no signed integer type for now
            if (common::empty(traits & common::TypeTraits::FLOATING_POINT)) {
                report_error("unary - is defined only for floats");
                return common::Type{};
            }
            return type;
        case common::UnaryOp::ADDRESS_OF:
            if (!is_lvalue(expr.expr)) {
                report_error("can't take address of an rvalue");
                return common::Type{};
            }
            ++type.indirection_level;
            return type;
        case common::UnaryOp::DEREFERENCE:
            if (!type.is_pointer()) {
                report_error("can't dereference type that is not a pointer");
                return common::Type{};
            }
            --type.indirection_level;
            return type;
        default:
            report_error("unknown unary operator");
            return common::Type{};
        }
    }

    common::Type Checker::check_binary_expression(common::BinaryExpression &expr) {
        if (expr.lhs.is_error() || expr.rhs.is_error()) {
            return common::Type{};
        }

        common::Type lhs = check_expression(expr.lhs);
        if (lhs.is_error()) {
            return common::Type{};
        }
        common::Type rhs = check_expression(expr.rhs);
        if (rhs.is_error()) {
            return common::Type{};
        }

        if (lhs != rhs) {
            report_error("type mismatch: can not convert types");
            return common::Type{};
        }

        if (expr.op == common::BinaryOp::ASSIGN) {
            if (!is_lvalue(expr.lhs)) {
                report_error("can not assing to rvalue");
                return common::Type{};
            }
            return common::Type{common::g_void};
        }

        if (lhs.is_pointer() || rhs.is_pointer()) {
            report_error("can not use pointers in binary expressions except for assignment");
            return common::Type{};
        }

        common::TypeTraits traits = module_.get_scope(lhs.sym.scope)->get_traits(lhs.sym.id);

        if (common::is_logic_op(expr.op)) {
            if (common::empty(traits & common::TypeTraits::BOOLEAN)) {

                report_error("type mismatch: expected boolean");
                return common::Type{};
            } else {
                return lhs;
            }
        } else if (common::is_equality_op(expr.op)) {
            // for now, every type is comparable
            // later maybe should add COMPARABLE type trait
            return builtin_types_[common::BuiltinTypes::BOOL];
        } else if (common::is_relational(expr.op)) {
            if (common::empty(traits & common::TypeTraits::ORDERED)) {
                report_error("type mismatch: expected ordered type");
                return common::Type{};
            } else {
                return builtin_types_[common::BuiltinTypes::BOOL];
            }
        } else if (common::is_bitwise(expr.op) && common::empty(traits & common::TypeTraits::INTEGER)) {
            report_error("bitwise operations are allowed only on integers");
            return common::Type{};
        } else if (expr.op == common::BinaryOp::REMAINDER && common::empty(traits & common::TypeTraits::INTEGER)) {
            report_error("type mismatch: expected integer operands for % operator");
            return common::Type{};
        } else if (common::empty(traits & common::TypeTraits::NUMERIC)) {
            report_error("type mismatch: expected numeric type");
            return common::Type{};
        }

        return lhs;
    }

    common::Type Checker::check_cast(common::Cast &cast) {
        if (cast.to.indirection_level != 0) {
            report_error("cats to pointer types are not allowed");
            return common::Type{};
        }
        common::Symbol dst_sym = module_.find(cast.to.name);
        cast.dst_type = common::Type{dst_sym};
        std::optional<common::BuiltinType> dst = module_.get_scope(dst_sym.scope)->get_type(dst_sym.id);
        if (!dst) {
            report_error("can not cast to unknown type");
            return common::Type{};
        }

        common::Type src = check_expression(cast.from);
        if (src.is_error()) {
            return common::Type{};
        } else if (src.is_pointer()) {
            report_error("casts from pointer types are not allowed");
            return common::Type{};
        }
        common::TypeTraits src_traits = module_.get_scope(src.sym.scope)->get_type(src.sym.id)->traits;

        // TODO: empty traits
        if (dst->traits == src_traits) {
            return common::Type{dst_sym};
        }

        if (!common::empty(dst->traits & common::TypeTraits::BOOLEAN) ||
            !common::empty(src_traits & common::TypeTraits::BOOLEAN)) {
            report_error("can not cast to bool");
            return common::Type{};
        }

        return cast.dst_type;
    }

    common::Type Checker::get_type_for_literal(common::Literal lit) {
        if (lit.is<bool>()) {
            return builtin_types_.at(common::BuiltinTypes::BOOL);
        } else if (lit.is<uint64_t>()) {
            return builtin_types_.at(common::BuiltinTypes::UINT);
        } else if (lit.is<double>()) {
            return builtin_types_.at(common::BuiltinTypes::FLOAT);
        }
        report_error("unknown literal type");
        return common::Type{};
    }

    common::Type Checker::check_function_call(common::FunctionCall &call, common::Expression &incoming_edge) {
        if (common::Scope::type(module_.global_scope()->find(call.name)) != common::SymbolType::FUNCTION) {
            report_error(*identifiers_->get(call.name) + " is not a function");
            return common::Type{};
        }

        common::FunctionID func_id = module_.global_scope()->get_function(call.name);
        if (func_id == common::FunctionID{}) {
            report_error("function not defined");
            return common::Type{};
        }
        common::Function &func = *ast_->get_function(func_id);
        if (func.decl_only) {
            // function declared, but not defined
            report_error("function not defined");
            return common::Type{};
        }
        if (*identifiers_->get(func.name) == "main") {
            report_error("main() must not be called");
            return common::Type{};
        }

        if (call.args.size() != func.params.size()) {
            report_error("function call argument count mismatch, expected " +
                         std::to_string(func.params.size()) + ", got " + std::to_string(call.args.size()));
            return common::Type{};
        }
        for (size_t i = 0; i < func.params.size(); ++i) {
            ErrorGuard eg{*this, call.args[i].pos};
            common::Variable &param = *ast_->get_var(func.params[i]);

            common::Type type = check_expression(call.args[i]);
            if (type.is_error()) {
                return common::Type{};
            }
            if (type != param.type) {
                report_error("function call argument's type mismatch");
                return common::Type{};
            }
        }

        return func.return_type;
    }

    void Checker::check_function(common::Function &func) {
        ScopeGuard g{*this, func.scope, Reachability::REACHABLE};
        current_function_ = func.id;
        check_block(func.body);
        if (!err_.empty()) {
            return;
        }
        if (!func.return_type.is_void() && reachability_stack_.top() != Reachability::RETURNS) {
            report_error("missing return statement in non-void function");
            return;
        }
    }

    bool Checker::is_lvalue(common::Expression expr) {
        if (expr.kind == common::ExpressionKind::VARIABLE_REF) {
            return true;
        } else if (expr.kind != common::ExpressionKind::UNARY) {
            return false;
        }
        const common::UnaryExpression &unary = *ast_->get_unary_expression(expr.id);
        return unary.op == common::UnaryOp::DEREFERENCE;
    }

    common::Type Checker::check_variable_ref(common::IdentifierID name) {
        common::Symbol sym = module_.find(name, scope_stack_.top());
        if (sym.is_error() || common::Scope::type(sym.id) != common::SymbolType::VARIABLE) {
            report_error("identifier does not name a variable");
            return common::Type{};
        }

        common::Variable &var = *ast_->get_var(module_.get_scope(sym.scope)->get_variable(sym.id));
        return var.type;
    }

    void Checker::check_function_decl(common::Function &func) {
        ErrorGuard eg{*this, func.pos};
        if (common::Symbol sym = module_.find(func.name); !sym.is_error()) {
            report_error("can not declare a function: name " + *identifiers_->get(func.name) + " already used");
            return;
        }
        module_.global_scope()->add(func.name, func.id);
        const std::string &name = *identifiers_->get(func.name);
        if (name == "main") {
            if (!func.return_typename.is_error()) {
                report_error("main() must return void");
                return;
            } else if (!func.params.empty()) {
                report_error("main() must not accept arguments");
                return;
            }
        }
        if (!func.return_typename.is_error()) {
            func.return_type = common::Type{module_.find(func.return_typename.name), func.return_typename.indirection_level};
            if (func.return_type.is_error()) {
                report_error("unknown return type");
                return;
            }
            if (common::Scope::type(func.return_type.sym.id) != common::SymbolType::BUILTIN_TYPE) {
                report_error("function's return type must be a typename");
                return;
            }
        } else {
            func.return_type = common::Type{common::g_void};
        }

        func.scope = module_.make_scope(module_.global_scope()->id());
        for (common::VariableID param_id : func.params) {
            common::Variable &param = *ast_->get_var(param_id);
            ErrorGuard eg_inner{*this, param.pos};
            if (!module_.find(param.name).is_error()) {
                report_error("can't declare function parameter: name already declared");
                return;
            }
            if (param.explicit_type.is_error()) {
                report_error("function parameters must be explicitly typed");
                return;
            }
            param.type = common::Type{module_.find(param.explicit_type.name), param.explicit_type.indirection_level};
            if (param.type.is_error()) {
                report_error("invalid function parameter type");
                return;
            }
            if (!param.initial_value.is_error()) {
                report_error("default values for function parameters are not supported");
                return;
            }
            module_.get_scope(func.scope)->add(param.name, param.id);
        }
    }

    void Checker::check_branch(common::Branch &branch) {
        common::Type type = check_expression(branch.predicate);
        branch.predicate.type = type;
        if (type.is_error()) {
            return;
        } else if (type.is_pointer() ||
                   common::empty(module_.get_scope(type.sym.scope)->get_traits(type.sym.id) & common::TypeTraits::BOOLEAN)) {
            ErrorGuard eg{*this, branch.predicate.pos};
            report_error("expected boolean expression in an if statement");
            return;
        }

        Reachability reachability = reachability_stack_.top();
        {
            ScopeGuard g{*this, module_.make_scope(scope_stack_.top()), reachability_stack_.top()};
            check_block(branch.then);
            if (!err_.empty()) {
                return;
            }
            reachability = reachability_stack_.top();
        }
        if (branch.otherwise.smts.empty()) {
            return;
        }

        // else if: do not push unneded scope
        if (branch.otherwise.smts.size() == 1 && branch.otherwise.smts[0].type == common::StatementType::BRANCH) {
            check_branch(*ast_->get_branch(branch.otherwise.smts[0].id));
            if (!err_.empty()) {
                return;
            }
            reachability = unite_reachability(reachability, reachability_stack_.top());
        } else {
            ScopeGuard g{*this, module_.make_scope(scope_stack_.top()), reachability_stack_.top()};
            check_block(branch.otherwise);
            if (!err_.empty()) {
                return;
            }
            reachability = unite_reachability(reachability, reachability_stack_.top());
        }
        reachability_stack_.top() = reachability;
    }

    void Checker::check_statement(common::Statement &smt) {
        ErrorGuard g{*this, smt.pos};
        switch (smt.type) {
        case common::StatementType::EXPRESSION:
            check_expression(*ast_->get_expression(smt.id));
            break;
        case common::StatementType::BRANCH:
            check_branch(*ast_->get_branch(smt.id));
            break;
        case common::StatementType::RETURN: {
            if (is_reachable()) {
                reachability_stack_.top() = Reachability::RETURNS;
            }
            common::Function &func = *ast_->get_function(current_function_);
            common::Type ret = check_expression(*ast_->get_expression(smt.id));
            if (!ret.is_error() && func.return_type != ret) {
                report_error("wrong return type");
            }
            break;
        }
        case common::StatementType::VARIABLE:
            check_variable(*ast_->get_var(smt.id));
            break;
        case common::StatementType::LOOP:
            check_loop(*ast_->get_loop(smt.id));
            break;
        case common::StatementType::BREAK: [[fallthrough]];
        case common::StatementType::CONTINUE:
            reachability_stack_.top() = Reachability::UNREACHABLE;
            if (loop_cout_ == 0) {
                report_error("'break' and 'continue' must be used only inside loops");
            }
            break;
        case common::StatementType::EMPTY: break;
        default:
            report_error("unknown statement type");
            break;
        }
    }

    void Checker::check_block(common::Block &block) {
        for (common::Statement &smt : block.smts) {
            smt.is_reachable = is_reachable();
            check_statement(smt);
            if (!err_.empty()) {
                return;
            }
        }
    }

    void Checker::check_variable(common::Variable &var) {
        if (!module_.find(var.name, scope_stack_.top()).is_error()) {
            report_error("variable declaration: name already used");
            return;
        }

        if (!var.explicit_type.is_error()) {
            var.type = common::Type{
                module_.find(var.explicit_type.name, scope_stack_.top()),
                var.explicit_type.indirection_level,
            };
            if (var.type.is_error() || common::Scope::type(var.type.sym.id) != common::SymbolType::BUILTIN_TYPE) {
                report_error("invalid variable type");
                return;
            }
        }

        common::Type expected_type = var.type;
        if (!var.initial_value.is_error()) {
            var.initial_value.type = check_expression(var.initial_value);
            expected_type = var.initial_value.type;
        }
        if (!var.explicit_type.is_error()) {
            if (var.type != expected_type) {
                report_error("type mismatch: can not initialize variabe with expression of wrong type");
                return;
            }
        } else {
            var.type = expected_type;
        }

        module_.get_scope(scope_stack_.top())->add(var.name, var.id);
    }

    void Checker::check_loop(common::Loop &loop) {
        ScopeGuard g{*this, module_.make_scope(scope_stack_.top()), reachability_stack_.top()};
        check_statement(loop.init);
        if (!err_.empty()) {
            return;
        }
        loop.condition.type = check_expression(loop.condition);
        if (loop.condition.type.is_error()) {
            return;
        } else if (!loop.condition.type.is_void()) {
            ErrorGuard eg{*this, loop.condition.pos};
            common::Scope &scope = *module_.get_scope(loop.condition.type.sym.scope);
            if (loop.condition.type.is_pointer() || common::empty(scope.get_traits(loop.condition.type.sym.id) & common::TypeTraits::BOOLEAN)) {
                report_error("loop's condition must have boolean type");
                return;
            }
        }
        loop.iteration.type = check_expression(loop.iteration);
        if (loop.iteration.type.is_error()) {
            return;
        }
        ++loop_cout_;
        check_block(loop.body);
        --loop_cout_;
    }

    Reachability Checker::unite_reachability(Reachability lhs, Reachability rhs) {
        if (lhs == rhs) {
            return lhs;
        } else if (lhs == Reachability::REACHABLE || rhs == Reachability::REACHABLE) {
            return Reachability::REACHABLE;
        }
        return Reachability::UNREACHABLE;
    }

    void Checker::try_compute(common::UnaryExpression &expr, common::Expression &ref_to_this) {
        if (expr.expr.kind != common::ExpressionKind::LITERAL) {
            return;
        }

        common::Literal lit = *ast_->get_literal(expr.expr.id);
        switch (expr.op) {
        case common::UnaryOp::NEGATE: lit = -*lit.get<double>(); break;
        case common::UnaryOp::NOT: lit = !*lit.get<bool>(); break;
        default: return;
        }
        ast_->free_literal(expr.expr.id);
        ref_to_this.kind = common::ExpressionKind::LITERAL;
        ref_to_this.id = ast_->add(lit);
    }

    void Checker::try_compute(common::BinaryExpression &expr, common::Expression &ref_to_this) {
        if (expr.lhs.kind != common::ExpressionKind::LITERAL || expr.rhs.kind != common::ExpressionKind::LITERAL) {
            return;
        }

        common::Literal lhs = *ast_->get_literal(expr.lhs.id);
        common::Literal rhs = *ast_->get_literal(expr.rhs.id);
        if (!lhs.same_type(rhs)) {
            return;
        }

        auto do_rel_op = []<typename T>(common::BinaryOp op, T lhs, T rhs) -> std::optional<bool> {
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
        auto do_integer_op = [this](common::BinaryOp op, uint64_t lhs, uint64_t rhs) -> std::optional<uint64_t> {
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
        auto do_op = [this]<typename T>(common::BinaryOp op, T lhs, T rhs) -> std::optional<T> {
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

        common::Literal result;
        if (lhs.is<bool>()) {
            std::optional<bool> val;
            if (common::is_relational(expr.op)) {
                val = do_rel_op(expr.op, *lhs.get<bool>(), *rhs.get<bool>());
            } else {
                val = do_op(expr.op, *lhs.get<bool>(), *rhs.get<bool>());
            }
            if (!val) {
                return;
            }
            result = *val;
        } else if (lhs.is<uint64_t>()) {
            if (common::is_relational(expr.op)) {
                std::optional<bool> val = do_rel_op(expr.op, *lhs.get<uint64_t>(), *rhs.get<uint64_t>());
                if (!val) {
                    return;
                }
                result = *val;
            } else {
                std::optional<uint64_t> val;
                if (common::is_bitwise(expr.op) || expr.op == common::BinaryOp::REMAINDER) {
                    val = do_integer_op(expr.op, *lhs.get<uint64_t>(), *rhs.get<uint64_t>());
                } else {
                    val = do_op(expr.op, *lhs.get<uint64_t>(), *rhs.get<uint64_t>());
                }
                if (!val) {
                    return;
                }
                result = *val;
            }
        } else if (lhs.is<double>()) {
            if (common::is_relational(expr.op)) {
                std::optional<bool> val = do_rel_op(expr.op, *lhs.get<double>(), *rhs.get<double>());
                if (!val) {
                    return;
                }
                result = *val;
            } else {
                std::optional<double> val = do_op(expr.op, *lhs.get<double>(), *rhs.get<double>());
                if (!val) {
                    return;
                }
                result = *val;
            }
        } else {
            return;
        }

        ast_->free_literal(expr.lhs.id);
        ast_->free_literal(expr.rhs.id);
        ref_to_this.kind = common::ExpressionKind::LITERAL;
        ref_to_this.id = ast_->add(result);
    }

    void Checker::try_compute(common::Cast &cast, common::Expression &ref_to_this) {
        if (cast.from.kind != common::ExpressionKind::LITERAL) {
            return;
        }
        common::Type floating_type = builtin_types_[common::BuiltinTypes::FLOAT];
        common::Type integer_type = builtin_types_[common::BuiltinTypes::UINT];
        common::Type boolean_type = builtin_types_[common::BuiltinTypes::BOOL];

        common::Literal from = *ast_->get_literal(cast.from.id);
        if (cast.dst_type == floating_type) {
            if (!from.is<uint64_t>() && !from.is<double>()) {
                return;
            } else if (uint64_t *uint = from.get<uint64_t>(); uint) {
                from = static_cast<double>(*uint);
            }
        } else if (cast.dst_type == integer_type) {
            if (!from.is<uint64_t>() && !from.is<double>()) {
                return;
            } else if (double *d = from.get<double>(); d) {
                from = static_cast<uint64_t>(*d);
            }
        } else if (cast.dst_type == boolean_type) {
            if (!from.is<bool>()) {
                return;
            }
        }

        ast_->free_literal(cast.from.id);
        ref_to_this.kind = common::ExpressionKind::LITERAL;
        ref_to_this.id = ast_->add(from);
    }

} // namespace checker
