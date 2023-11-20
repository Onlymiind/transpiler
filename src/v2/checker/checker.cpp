#include "checker/checker.h"
#include "common/expression.h"
#include "common/types.h"

namespace checker {
    void Checker::check() {
        builtin_types_[common::BuiltinTypes::BOOL] = module_.add_type(common::BuiltinTypes::BOOL, common::TypeTraits::BOOLEAN);
        builtin_types_[common::BuiltinTypes::UINT] = module_.add_type(common::BuiltinTypes::UINT, common::TypeTraits::INTEGER);
        builtin_types_[common::BuiltinTypes::FLOAT] = module_.add_type(common::BuiltinTypes::FLOAT, common::TypeTraits::FLOATING_POINT);
        check_expression(module_.file().start());
    }

    common::Type Checker::check_expression(common::Expression expr) {
        if (expr.is_error()) {
            return common::Type{};
        }

        common::Type result{};
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
        default:
            report_error("unknown expression type");
            return common::Type{};
        }

        if (result.is_error()) {
            return result;
        }
        module_.set_expression_type(expr, result);
        return result;
    }

    common::Type Checker::check_unary_expression(common::UnaryExpression expr) {
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

    common::Type Checker::check_binary_expression(common::BinaryExpression expr) {
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

    common::Type Checker::get_type_for_literal(common::Literal lit) {
        switch (lit.value.type) {
        case common::TokenType::BOOL:
            return builtin_types_.at(common::BuiltinTypes::BOOL);
        case common::TokenType::INTEGER:
            return builtin_types_.at(common::BuiltinTypes::UINT);
        case common::TokenType::FLOAT:
            return builtin_types_.at(common::BuiltinTypes::FLOAT);
        default:
            report_error("unknown literal type");
            return common::Type{};
        }
    }

    bool Checker::can_implicitly_convert(common::Type dst, common::Type src) const {
        std::optional<common::BuiltinTypes> builtin_dst = module_.get_builtin(dst);
        if (!builtin_dst) {
            return false;
        }
        std::optional<common::BuiltinTypes> builtin_src = module_.get_builtin(src);
        if (!builtin_src) {
            return false;
        }

        return builtin_dst == common::BuiltinTypes::FLOAT && builtin_src == common::BuiltinTypes::UINT;
    }
} // namespace checker
