#include "checker/checker.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/scope.h"
#include "common/types.h"
#include "common/util.h"

namespace checker {

    void Checker::add_builtins() {
        module_.make_scope();
        builtin_types_[common::BuiltinTypes::BOOL] = module_.global_scope()->add(common::BuiltinType{
            .name = identifiers_->add("bool"),
            .type = common::BuiltinTypes::BOOL,
            .traits = common::TypeTraits::BOOLEAN,
        });
        builtin_types_[common::BuiltinTypes::UINT] = module_.global_scope()->add(common::BuiltinType{
            .name = identifiers_->add("u64"),
            .type = common::BuiltinTypes::UINT,
            .traits = common::TypeTraits::INTEGER,
        });
        builtin_types_[common::BuiltinTypes::FLOAT] = module_.global_scope()->add(common::BuiltinType{
            .name = identifiers_->add("f64"),
            .type = common::BuiltinTypes::FLOAT,
            .traits = common::TypeTraits::FLOATING_POINT,
        });
    }

    void Checker::add_declarations() {
        add_builtins();
        const auto &functions = ast_->functions();
        for (const auto &func : functions) {
            if (module_.global_scope()->add(func.name, func.id) == common::SymbolID{}) {
                err_positions_.push(func.pos);
                report_error("function redeclaration");
                return;
            }
        }
    }

    void Checker::check() {
        add_declarations();
        if (!err_.empty()) {
            return;
        }
        auto &functions = ast_->functions();
        for (auto &func : functions) {
            check_function(func);
            if (!err_.empty()) {
                return;
            }

            if (*identifiers_->get(func.name) == "main") {
                module_.set_entrypoint(func.id);
            }
        }

        if (module_.entrypoint() == common::FunctionID{}) {
            err_positions_.push(1);
            report_error("entrypoint not declared");
        }
    }

    common::SymbolID Checker::check_expression(common::Expression &expr) {
        if (expr.is_error()) {
            return common::SymbolID{};
        }

        common::SymbolID result{};
        err_positions_.push(expr.pos);
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
        default:
            report_error("unknown expression type");
            return common::SymbolID{};
        }

        if (result == common::SymbolID{}) {
            return result;
        }
        expr.type = result;
        err_positions_.pop();
        return result;
    }

    common::SymbolID Checker::check_unary_expression(common::UnaryExpression &expr) {
        if (expr.expr.is_error()) {
            return common::SymbolID{};
        }
        common::SymbolID type = check_expression(expr.expr);
        if (type == common::SymbolID{}) {
            return common::SymbolID{};
        }

        common::TypeTraits traits = module_.global_scope()->get_traits(type);
        switch (expr.op) {
        case common::UnaryOp::NOT:
            if (common::empty(traits & common::TypeTraits::BOOLEAN)) {
                report_error("operator ! is defined only for booleans");
                return common::SymbolID{};
            }
            return type;
        case common::UnaryOp::NEGATE:
            // no signed integer type for now
            if (common::empty(traits & common::TypeTraits::FLOATING_POINT)) {
                report_error("unary - is defined only for floats");
                return common::SymbolID{};
            }
            return type;
        default:
            report_error("unknown unary operator");
            return common::SymbolID{};
        }
    }

    common::SymbolID Checker::check_binary_expression(common::BinaryExpression &expr) {
        if (expr.lhs.is_error() || expr.rhs.is_error()) {
            return common::SymbolID{};
        }

        common::SymbolID lhs = check_expression(expr.lhs);
        if (lhs == common::SymbolID{}) {
            return common::SymbolID{};
        }
        common::SymbolID rhs = check_expression(expr.rhs);
        if (rhs == common::SymbolID{}) {
            return common::SymbolID{};
        }

        if (lhs != rhs) {
            report_error("type mismatch: can not convert types");
            return common::SymbolID{};
        }

        common::TypeTraits traits = module_.global_scope()->get_traits(lhs);

        if (common::is_logic_op(expr.op)) {
            if (common::empty(traits & common::TypeTraits::BOOLEAN)) {

                report_error("type mismatch: expected boolean");
                return common::SymbolID{};
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
                return common::SymbolID{};
            } else {
                return builtin_types_[common::BuiltinTypes::BOOL];
            }
        }

        // arithmetic operators
        if (common::empty(traits & common::TypeTraits::NUMERIC)) {
            report_error("type mismatch: expected numeric type");
            return common::SymbolID{};
        } else if (expr.op == common::BinaryOp::REMAINDER && common::empty(traits & common::TypeTraits::INTEGER)) {
            report_error("type mismatch: expected integer operands for % operator");
            return common::SymbolID{};
        }

        return lhs;
    }

    common::SymbolID Checker::check_cast(common::Cast &cast) {
        common::SymbolID dst_sym = module_.global_scope()->find(cast.to);
        std::optional<common::BuiltinType> dst = module_.global_scope()->get_type(dst_sym);
        if (!dst) {
            report_error("can not cast to unknown type");
            return common::SymbolID{};
        }

        common::SymbolID src = check_expression(cast.from);
        if (src == common::SymbolID{}) {
            return common::SymbolID{};
        }
        common::TypeTraits src_traits = module_.global_scope()->get_type(src)->traits;

        // TODO: empty traits
        if (dst->traits == src_traits) {
            return dst_sym;
        }

        if (!common::empty(dst->traits & common::TypeTraits::BOOLEAN) ||
            !common::empty(src_traits & common::TypeTraits::BOOLEAN)) {
            report_error("can not cast to bool");
            return common::SymbolID{};
        }

        return dst_sym;
    }

    common::SymbolID Checker::get_type_for_literal(common::Literal lit) {
        switch (lit.type) {
        case common::LiteralType::BOOL:
            return builtin_types_.at(common::BuiltinTypes::BOOL);
        case common::LiteralType::UINT:
            return builtin_types_.at(common::BuiltinTypes::UINT);
        case common::LiteralType::FLOAT:
            return builtin_types_.at(common::BuiltinTypes::FLOAT);
        default:
            report_error("unknown literal type");
            return common::SymbolID{};
        }
    }

    common::SymbolID Checker::check_function_call(common::FunctionCall &call, common::Expression &incoming_edge) {
        if (common::Scope::type(module_.global_scope()->find(call.name)) == common::SymbolType::BUILTIN_TYPE) {
            if (call.args.size() != 1) {
                report_error("expected exactly 1 argument for a cast");
                return common::SymbolID{};
            }

            common::Cast cast{.to = call.name, .from = call.args[0]};
            common::SymbolID result = check_cast(cast);
            if (result == common::SymbolID{}) {
                return common::SymbolID{};
            }
            incoming_edge.kind = common::ExpressionKind::CAST;
            incoming_edge.id = ast_->add_cast(cast);
            return result;
        }

        common::FunctionID id = module_.global_scope()->get_function(call.name);
        if (id == common::FunctionID{}) {
            report_error("function not defined");
            return common::SymbolID{};
        }
        common::Expression &expr = ast_->get_function(id)->body;
        if (expr.is_error()) {
            // function declared, but not defined
            report_error("function not defined");
            return common::SymbolID{};
        }
        if (expr.type != common::SymbolID{}) {
            return expr.type;
        }

        return check_expression(expr);
    }

    void Checker::check_function(common::Function &func) {
        check_expression(func.body);
    }
} // namespace checker
