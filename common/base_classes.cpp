#include "common/base_classes.h"
#include "common/expression.h"
#include "common/util.h"

namespace common {

    bool Expression::is_lvalue() const {
        if (kind_ == ExpressionKind::VARIABLE_REF) {
            return true;
        } else if (kind_ != ExpressionKind::UNARY) {
            return false;
        }
        return downcast<UnaryExpression>(*this).op() == UnaryOp::DEREFERENCE;
    }

} // namespace common
