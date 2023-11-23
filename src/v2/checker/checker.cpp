#include "checker/checker.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/types.h"

namespace checker {

    void Checker::add_builtins() {
        builtin_types_[common::BuiltinTypes::BOOL] = module_.add(common::BuiltinType{
            .name = module_.file().literals().add("bool"),
            .type = common::BuiltinTypes::BOOL,
            .traits = common::TypeTraits::BOOLEAN,
        });
        builtin_types_[common::BuiltinTypes::UINT] = module_.add(common::BuiltinType{
            .name = module_.file().literals().add("u64"),
            .type = common::BuiltinTypes::UINT,
            .traits = common::TypeTraits::INTEGER,
        });
        builtin_types_[common::BuiltinTypes::FLOAT] = module_.add(common::BuiltinType{
            .name = module_.file().literals().add("f64"),
            .type = common::BuiltinTypes::FLOAT,
            .traits = common::TypeTraits::FLOATING_POINT,
        });
    }

    void Checker::add_declarations() {
        add_builtins();
        const auto &functions = module_.file().functions();
        for (const auto &func : functions) {
            if (!module_.add(func)) {
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
        auto &functions = module_.file().functions();
        auto &literals = module_.file().literals();
        for (auto &func : functions) {
            check_function(func);
            if (!err_.empty()) {
                return;
            }

            if (*literals.get_string(func.name) == "main") {
                module_.set_entrypoint(func.id);
            }
        }

        if (module_.entrypoint() == common::Function::g_invalid_id) {
            err_positions_.push(0);
            report_error("entrypoint not declared");
        }
    }

    common::Type Checker::check_expression(common::Expression &expr) {
        if (expr.is_error()) {
            return common::Type{};
        }

        common::Type result{};
        err_positions_.push(expr.pos);
        switch (expr.type) {
        case common::ExpressionType::LITERAL:
            result = get_type_for_literal(*module_.file().get_literal(expr.id));
            break;
        case common::ExpressionType::UNARY:
            result = check_unary_expression(*module_.file().get_unary_expression(expr.id));
            break;
        case common::ExpressionType::BINARY:
            result = check_binary_expression(*module_.file().get_binary_expression(expr.id));
            break;
        case common::ExpressionType::CAST:
            result = check_cast(*module_.file().get_cast(expr.id));
            break;
        case common::ExpressionType::FUNCTION_CALL:
            result = check_function_call(*module_.file().get_call(expr.id), expr);
            break;
        default:
            report_error("unknown expression type");
            return common::Type{};
        }

        if (result.is_error()) {
            return result;
        }
        module_.set_expression_type(expr.id, result);
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

        common::TypeTraits traits = module_.get_traits(type);
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

        common::TypeTraits traits = module_.get_traits(lhs);

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
        common::Type dst = module_.get_type(cast.to);
        if (dst.is_error()) {
            report_error("can not cast to unknown type");
            return common::Type{};
        }
        common::TypeTraits dst_traits = module_.get_traits(dst);

        common::Type src = check_expression(cast.from);
        if (src.is_error()) {
            return common::Type{};
        }
        common::TypeTraits src_traits = module_.get_traits(src);

        // TODO: empty traits
        if (dst_traits == src_traits) {
            return dst;
        }

        if (!common::empty(dst_traits & common::TypeTraits::BOOLEAN) ||
            !common::empty(src_traits & common::TypeTraits::BOOLEAN)) {
            report_error("can not cast to bool");
            return common::Type{};
        }

        return dst;
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

    common::Type Checker::check_function_call(common::FunctionCall call, common::Expression &incoming_edge) {
        if (!module_.get_type(call.name).is_error()) {
            if (call.args.size() != 1) {
                report_error("expected exactly 1 argument for a cast");
                return common::Type{};
            }
            common::Cast cast{.to = call.name, .from = call.args[0]};
            incoming_edge.type = common::ExpressionType::CAST;
            incoming_edge.id = module_.file().add(cast);
            return check_cast(cast);
        }

        common::Function::ID id = module_.get_function(call.name);
        if (id == common::Function::g_invalid_id) {
            report_error("function not defined");
            return common::Type{};
        }
        common::Expression &expr = module_.file().get_function(id)->body;
        if (expr.is_error()) {
            // function declared, but not defined
            report_error("function not defined");
            return common::Type{};
        }
        if (common::Type result = module_.get_expression_type(expr.id); !result.is_error()) {
            return result;
        }

        return check_expression(expr);
    }

    void Checker::check_function(common::Function &func) {
        check_expression(func.body);
    }
} // namespace checker
