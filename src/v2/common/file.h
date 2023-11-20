#ifndef COMPILER_V2_COMMON_FILE_HDR_
#define COMPILER_V2_COMMON_FILE_HDR_
#include "common/expression.h"

#include <vector>

namespace common {
    class File {
      public:
        File() = default;

        const common::UnaryExpression *get_unary_expression(common::Expression::ID id) const {
            if (*id >= unary_exprs_.size()) {
                return nullptr;
            }
            return &unary_exprs_[*id];
        }

        const common::BinaryExpression *get_binary_expression(common::Expression::ID id) const {
            if (*id >= binary_exprs_.size()) {
                return nullptr;
            }
            return &binary_exprs_[*id];
        }

        const common::Literal *get_literal(common::Expression::ID id) const {
            if (*id >= literals_.size()) {
                return nullptr;
            }
            return &literals_[*id];
        }

        common::Expression::ID add(common::BinaryExpression expr) {
            common::Expression::ID result{binary_exprs_.size()};
            binary_exprs_.push_back(expr);
            return result;
        }

        common::Expression::ID add(common::UnaryExpression expr) {
            common::Expression::ID result{unary_exprs_.size()};
            unary_exprs_.push_back(expr);
            return result;
        }

        common::Expression::ID add(common::Literal lit) {
            common::Expression::ID result{literals_.size()};
            literals_.push_back(lit);
            return result;
        }

        // TODO: this is temporary API
        common::Expression start() const { return start_; }
        void set_start(common::Expression expr) { start_ = expr; }

        bool operator==(const File &other) const {
            return start_ == other.start_ &&
                   unary_exprs_ == other.unary_exprs_ &&
                   binary_exprs_ == other.binary_exprs_ &&
                   literals_ == other.literals_;
        }

      private:
        common::Expression start_{};
        std::vector<common::UnaryExpression> unary_exprs_;
        std::vector<common::BinaryExpression> binary_exprs_;
        std::vector<common::Literal> literals_;
    };
} // namespace common
#endif
