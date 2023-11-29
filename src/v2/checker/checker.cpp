#include "checker/checker.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/scope.h"
#include "common/statement.h"
#include "common/types.h"
#include "common/util.h"

namespace checker {

    void Checker::add_builtins() {
        module_.make_scope();
        common::ScopeID global = module_.global_scope()->id();
        builtin_types_[common::BuiltinTypes::BOOL] = common::Symbol{
            global,
            module_.global_scope()->add(common::BuiltinType{
                .name = identifiers_->add("bool"),
                .type = common::BuiltinTypes::BOOL,
                .traits = common::TypeTraits::BOOLEAN,
            })};
        builtin_types_[common::BuiltinTypes::UINT] = common::Symbol{
            global,
            module_.global_scope()->add(common::BuiltinType{
                .name = identifiers_->add("u64"),
                .type = common::BuiltinTypes::UINT,
                .traits = common::TypeTraits::INTEGER,
            })};
        builtin_types_[common::BuiltinTypes::FLOAT] = common::Symbol{
            global,
            module_.global_scope()->add(common::BuiltinType{
                .name = identifiers_->add("f64"),
                .type = common::BuiltinTypes::FLOAT,
                .traits = common::TypeTraits::FLOATING_POINT,
            })};
    }

    void Checker::add_declarations() {
        add_builtins();
        auto &functions = ast_->functions();
        for (auto &func : functions) {
            ErrorGuard eg{*this, func.pos};
            if (common::Symbol sym = module_.find(func.name); !sym.is_error()) {
                report_error("can not declare a function: name " + *identifiers_->get(func.name) + " already used");
                return;
            }

            if (func.return_typename != common::IdentifierID{}) {
                if (*identifiers_->get(func.name) == "main") {
                    report_error("main() must return void");
                    return;
                }
                func.return_type = module_.find(func.return_typename);
                if (func.return_type.is_error()) {
                    report_error("unknown return type");
                    return;
                }
                if (common::Scope::type(func.return_type.id) != common::SymbolType::BUILTIN_TYPE) {
                    report_error("function's return type must be a typename");
                    return;
                }
            } else {
                func.return_type = common::g_void;
            }

            func.scope = module_.make_scope(module_.global_scope()->id());
            module_.global_scope()->add(func.name, func.id);
        }

        auto &variables = ast_->global_variables();
        for (common::VariableID var_id : variables) {
            common::Variable &var = *ast_->get_var(var_id);
            ErrorGuard eg{*this, var.pos};
            if (common::Symbol sym = module_.find(var.name); !sym.is_error()) {
                report_error("can not declare a variable: name " + *identifiers_->get(var.name) + " already used");
                return;
            }

            var.type = module_.find(var.explicit_type);
            if (var.type.is_error()) {
                report_error("variable type not declared");
                return;
            }
            if (common::Scope::type(var.type.id) != common::SymbolType::BUILTIN_TYPE) {
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
        ScopeGuard g{*this, module_.global_scope()->id()};
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

    common::Symbol Checker::check_expression(common::Expression &expr) {
        if (expr.is_error()) {
            return common::Symbol{};
        }

        common::Symbol result{};
        ErrorGuard g{*this, expr.pos};
        switch (expr.kind) {
        case common::ExpressionKind::LITERAL:
            result = get_type_for_literal(*ast_->get_literal(expr.id));
            break;
        case common::ExpressionKind::UNARY:
            result = check_unary_expression(*ast_->get_unary_expression(expr.id));
            break;
        case common::ExpressionKind::BINARY:
            result = check_binary_expression(*ast_->get_binary_expression(expr.id));
            break;
        case common::ExpressionKind::CAST:
            result = check_cast(*ast_->get_cast(expr.id));
            break;
        case common::ExpressionKind::FUNCTION_CALL:
            result = check_function_call(*ast_->get_call(expr.id), expr);
            break;
        case common::ExpressionKind::VARIABLE_REF:
            result = check_variable_ref(ast_->get_variable_ref(expr.id));
            break;
        case common::ExpressionKind::EMPTY:
            result = common::g_void;
            break;
        default:
            report_error("unknown expression type");
            return common::Symbol{};
        }

        if (result.is_error()) {
            return result;
        }
        expr.type = result;
        return result;
    }

    common::Symbol Checker::check_unary_expression(common::UnaryExpression &expr) {
        if (expr.expr.is_error()) {
            return common::Symbol{};
        }
        common::Symbol type = check_expression(expr.expr);
        if (type.is_error()) {
            return common::Symbol{};
        }

        common::TypeTraits traits = module_.get_scope(type.scope)->get_traits(type.id);
        switch (expr.op) {
        case common::UnaryOp::NOT:
            if (common::empty(traits & common::TypeTraits::BOOLEAN)) {
                report_error("operator ! is defined only for booleans");
                return common::Symbol{};
            }
            return type;
        case common::UnaryOp::NEGATE:
            // no signed integer type for now
            if (common::empty(traits & common::TypeTraits::FLOATING_POINT)) {
                report_error("unary - is defined only for floats");
                return common::Symbol{};
            }
            return type;
        default:
            report_error("unknown unary operator");
            return common::Symbol{};
        }
    }

    common::Symbol Checker::check_binary_expression(common::BinaryExpression &expr) {
        if (expr.lhs.is_error() || expr.rhs.is_error()) {
            return common::Symbol{};
        }

        common::Symbol lhs = check_expression(expr.lhs);
        if (lhs.is_error()) {
            return common::Symbol{};
        }
        common::Symbol rhs = check_expression(expr.rhs);
        if (rhs.is_error()) {
            return common::Symbol{};
        }

        if (lhs != rhs) {
            report_error("type mismatch: can not convert types");
            return common::Symbol{};
        }

        if (expr.op == common::BinaryOp::ASSIGN) {
            if (!is_assignable(expr.lhs)) {
                report_error("can not assing to rvalue");
                return common::Symbol{};
            }
            return common::g_void;
        }

        common::TypeTraits traits = module_.get_scope(lhs.scope)->get_traits(lhs.id);

        if (common::is_logic_op(expr.op)) {
            if (common::empty(traits & common::TypeTraits::BOOLEAN)) {

                report_error("type mismatch: expected boolean");
                return common::Symbol{};
            } else {
                return lhs;
            }
        }
        if (common::is_equality_op(expr.op)) {
            // for now, every type is comparable
            // later maybe should add COMPARABLE type trait
            return builtin_types_[common::BuiltinTypes::BOOL];
        } else if (common::is_relational(expr.op)) {
            if (common::empty(traits & common::TypeTraits::ORDERED)) {
                report_error("type mismatch: expected ordered type");
                return common::Symbol{};
            } else {
                return builtin_types_[common::BuiltinTypes::BOOL];
            }
        }

        // arithmetic operators
        if (common::empty(traits & common::TypeTraits::NUMERIC)) {
            report_error("type mismatch: expected numeric type");
            return common::Symbol{};
        } else if (expr.op == common::BinaryOp::REMAINDER && common::empty(traits & common::TypeTraits::INTEGER)) {
            report_error("type mismatch: expected integer operands for % operator");
            return common::Symbol{};
        }

        return lhs;
    }

    common::Symbol Checker::check_cast(common::Cast &cast) {
        common::Symbol dst_sym = module_.find(cast.to);
        std::optional<common::BuiltinType> dst = module_.get_scope(dst_sym.scope)->get_type(dst_sym.id);
        if (!dst) {
            report_error("can not cast to unknown type");
            return common::Symbol{};
        }

        common::Symbol src = check_expression(cast.from);
        if (src.is_error()) {
            return common::Symbol{};
        }
        common::TypeTraits src_traits = module_.get_scope(src.scope)->get_type(src.id)->traits;

        // TODO: empty traits
        if (dst->traits == src_traits) {
            return dst_sym;
        }

        if (!common::empty(dst->traits & common::TypeTraits::BOOLEAN) ||
            !common::empty(src_traits & common::TypeTraits::BOOLEAN)) {
            report_error("can not cast to bool");
            return common::Symbol{};
        }

        return dst_sym;
    }

    common::Symbol Checker::get_type_for_literal(common::Literal lit) {
        switch (lit.type) {
        case common::LiteralType::BOOL:
            return builtin_types_.at(common::BuiltinTypes::BOOL);
        case common::LiteralType::UINT:
            return builtin_types_.at(common::BuiltinTypes::UINT);
        case common::LiteralType::FLOAT:
            return builtin_types_.at(common::BuiltinTypes::FLOAT);
        default:
            report_error("unknown literal type");
            return common::Symbol{};
        }
    }

    common::Symbol Checker::check_function_call(common::FunctionCall &call, common::Expression &incoming_edge) {
        if (common::Scope::type(module_.global_scope()->find(call.name)) == common::SymbolType::BUILTIN_TYPE) {
            if (call.args.size() != 1) {
                report_error("expected exactly 1 argument for a cast");
                return common::Symbol{};
            }

            common::Cast cast{.to = call.name, .from = call.args[0]};
            common::Symbol result = check_cast(cast);
            if (result.is_error()) {
                return common::Symbol{};
            }
            incoming_edge.kind = common::ExpressionKind::CAST;
            incoming_edge.id = ast_->add_cast(cast);
            return result;
        }

        common::FunctionID func_id = module_.global_scope()->get_function(call.name);
        if (func_id == common::FunctionID{}) {
            report_error("function not defined");
            return common::Symbol{};
        }
        common::Function &func = *ast_->get_function(func_id);
        if (func.decl_only) {
            // function declared, but not defined
            report_error("function not defined");
            return common::Symbol{};
        }
        return func.return_type;
    }

    void Checker::check_function(common::Function &func) {
        bool has_return = false;

        ScopeGuard g{*this, func.scope};
        for (common::Statement smt : func.body.smts) {
            ErrorGuard eg{*this, smt.pos};
            switch (smt.type) {
            case common::StatementType::EXPRESSION:
                if (check_expression(*ast_->get_expression(smt.id)).is_error()) {
                    return;
                }
                break;
            case common::StatementType::RETURN: {
                has_return = true;
                common::Expression *expr = ast_->get_expression(smt.id);
                common::Symbol ret = check_expression(*expr);
                if (ret.is_error()) {
                    return;
                }
                if (ret != func.return_type) {
                    report_error("wrong return type");
                    return;
                }
                break;
            }
            case common::StatementType::VARIABLE: {
                common::Variable &var = *ast_->get_var(smt.id);
                if (!module_.find(var.name, scope_stack_.top()).is_error()) {
                    report_error("local variable declaration: name already used");
                    return;
                }

                if (var.explicit_type != common::IdentifierID{}) {
                    var.type = module_.find(var.explicit_type, scope_stack_.top());
                    if (var.type.is_error() || common::Scope::type(var.type.id) != common::SymbolType::BUILTIN_TYPE) {
                        report_error("invalid local variable type");
                        return;
                    }
                }

                common::Symbol expected_type = var.type;
                if (!var.initial_value.is_error()) {
                    expected_type = check_expression(var.initial_value);
                }
                if (var.explicit_type != common::IdentifierID{}) {
                    if (var.type != expected_type) {
                        report_error("type mismatch: can not initialize local variabe with expression of wrong type");
                        return;
                    }
                } else {
                    var.type = expected_type;
                }

                module_.get_scope(scope_stack_.top())->add(var.name, var.id);
                break;
            }
            default:
                report_error("statement type not implemented");
                return;
            }
        }
        if (!has_return && !func.return_type.is_void()) {
            report_error("no return statement in non-void function");
            return;
        }
    }

    bool Checker::is_assignable(common::Expression expr) {
        return expr.kind == common::ExpressionKind::VARIABLE_REF;
    }

    common::Symbol Checker::check_variable_ref(common::IdentifierID name) {
        common::Symbol sym = module_.find(name, scope_stack_.top());
        if (sym.is_error() || common::Scope::type(sym.id) != common::SymbolType::VARIABLE) {
            report_error("identifier does not name a variable");
            return common::Symbol{};
        }

        common::Variable &var = *ast_->get_var(module_.get_scope(sym.scope)->get_variable(sym.id));
        return var.type;
    }
} // namespace checker
