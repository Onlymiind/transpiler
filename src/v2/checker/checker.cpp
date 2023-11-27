#include "checker/checker.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/scope.h"
#include "common/types.h"
#include "common/util.h"

namespace checker {

    void Checker::add_builtins() {
        module_.make_scope();
        builtin_types_[common::BuiltinTypes::BOOL].id = common::TypeID{module_.global_scope()->add(common::BuiltinType{
            .name = identifiers_->add("bool"),
            .type = common::BuiltinTypes::BOOL,
            .traits = common::TypeTraits::BOOLEAN,
        })};
        builtin_types_[common::BuiltinTypes::UINT].id = common::TypeID{module_.global_scope()->add(common::BuiltinType{
            .name = identifiers_->add("u64"),
            .type = common::BuiltinTypes::UINT,
            .traits = common::TypeTraits::INTEGER,
        })};
        builtin_types_[common::BuiltinTypes::FLOAT].id = common::TypeID{module_.global_scope()->add(common::BuiltinType{
            .name = identifiers_->add("f64"),
            .type = common::BuiltinTypes::FLOAT,
            .traits = common::TypeTraits::FLOATING_POINT,
        })};
    }

    void Checker::add_declarations() {
        add_builtins();
        const auto &functions = ast_->functions();
        for (const auto &func : functions) {
            if (module_.global_scope()->add(func.name, func.id) == common::SymbolID{common::g_invalid_id}) {
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

        if (module_.entrypoint() == common::FunctionID{common::g_invalid_id}) {
            err_positions_.push(1);
            report_error("entrypoint not declared");
        }
    }

    common::Type Checker::check_expression(common::Expression &expr) {
        if (expr.is_error()) {
            return common::Type{};
        }

        common::Type result{};
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
            return common::Type{};
        }

        if (result.is_error()) {
            return result;
        }
        expr.type = result.id;
        err_positions_.pop();
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

        common::TypeTraits traits = module_.global_scope()->get_traits(common::SymbolID{type.id});
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

        common::TypeTraits traits = module_.global_scope()->get_traits(common::SymbolID{lhs.id});

        if (common::is_logic_op(expr.op)) {
            if (common::empty(traits & common::TypeTraits::BOOLEAN)) {

                report_error("type mismatch: expected boolean");
                return common::Type{};
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
                return common::Type{};
            } else {
                return builtin_types_[common::BuiltinTypes::BOOL];
            }
        }

        // arithmetic operators
        if (common::empty(traits & common::TypeTraits::NUMERIC)) {
            report_error("type mismatch: expected numeric type");
            return common::Type{};
        } else if (expr.op == common::BinaryOp::REMAINDER && common::empty(traits & common::TypeTraits::INTEGER)) {
            report_error("type mismatch: expected integer operands for % operator");
            return common::Type{};
        }

        return lhs;
    }

    common::Type Checker::check_cast(common::Cast &cast) {
        common::SymbolID dst_sym = module_.global_scope()->find(cast.to);
        std::optional<common::BuiltinType> dst = module_.global_scope()->get_type(dst_sym);
        if (!dst) {
            report_error("can not cast to unknown type");
            return common::Type{};
        }

        common::Type src = check_expression(cast.from);
        if (src.is_error()) {
            return common::Type{};
        }
        common::TypeTraits src_traits = module_.global_scope()->get_type(src.id)->traits;

        // TODO: empty traits
        if (dst->traits == src_traits) {
            return common::Type{common::TypeID{dst_sym}};
        }

        if (!common::empty(dst->traits & common::TypeTraits::BOOLEAN) ||
            !common::empty(src_traits & common::TypeTraits::BOOLEAN)) {
            report_error("can not cast to bool");
            return common::Type{};
        }

        return common::Type{common::TypeID{dst_sym}};
    }

    common::Type Checker::get_type_for_literal(common::Literal lit) {
        switch (lit.type) {
        case common::LiteralType::BOOL:
            return builtin_types_.at(common::BuiltinTypes::BOOL);
        case common::LiteralType::UINT:
            return builtin_types_.at(common::BuiltinTypes::UINT);
        case common::LiteralType::FLOAT:
            return builtin_types_.at(common::BuiltinTypes::FLOAT);
        default:
            report_error("unknown literal type");
            return common::Type{};
        }
    }

    common::Type Checker::check_function_call(common::FunctionCall &call, common::Expression &incoming_edge) {
        if (common::Scope::type(module_.global_scope()->find(call.name)) == common::SymbolType::BUILTIN_TYPE) {
            if (call.args.size() != 1) {
                report_error("expected exactly 1 argument for a cast");
                return common::Type{};
            }

            common::Cast cast{.to = call.name, .from = call.args[0]};
            common::Type result = check_cast(cast);
            if (result.is_error()) {
                return common::Type{};
            }
            incoming_edge.kind = common::ExpressionKind::CAST;
            incoming_edge.id = ast_->add_cast(cast);
            return result;
        }

        common::FunctionID id = module_.global_scope()->get_function(call.name);
        if (id == common::FunctionID{common::g_invalid_id}) {
            report_error("function not defined");
            return common::Type{};
        }
        common::Expression &expr = ast_->get_function(id)->body;
        if (expr.is_error()) {
            // function declared, but not defined
            report_error("function not defined");
            return common::Type{};
        }
        if (expr.type != common::TypeID{common::g_invalid_id}) {
            return common::Type{expr.type};
        }

        return check_expression(expr);
    }

    void Checker::check_function(common::Function &func) {
        check_expression(func.body);
    }
} // namespace checker
