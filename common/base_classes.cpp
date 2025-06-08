#include "common/base_classes.h"
#include "common/expression.h"
#include "common/types.h"
#include "common/util.h"

namespace common {

    bool Expression::is_lvalue() const {
        if (kind_ == ExpressionKind::VARIABLE_REF) {
            return true;
        } else if (kind_ == ExpressionKind::INDEX) {
            const common::IndexExpression
                &expr = dynamic_cast<const common::IndexExpression &>(*this);
            return expr.container() && expr.container()->is_lvalue();
        } else if (kind_ == ExpressionKind::MEMBER_ACCESS) {
            const common::MemberAccess
                &access = dynamic_cast<const common::MemberAccess &>(*this);
            if (!access.record() ||
                access.member_name() == common::IdentifierID{} ||
                !access.record()->is_lvalue()) {
                return false;
            }
            const common::Type *type = access.record()->type();
            if (!type || type->kind() != common::TypeKind::STRUCT) {
                return false;
            }

            const common::StructType
                *record = dynamic_cast<const common::StructType *>(type);
            return record->get_field(access.member_name()) &&
                   !record->get_field(access.member_name())
                        ->has_flag(common::FieldFlags::READONLY);
        } else if (kind_ != ExpressionKind::UNARY) {
            return false;
        }
        return dynamic_cast<const UnaryExpression &>(*this).op() ==
               UnaryOp::DEREFERENCE;
    }

} // namespace common
