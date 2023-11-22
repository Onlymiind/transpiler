#ifndef COMPILER_V2_COMMON_FILE_HDR_
#define COMPILER_V2_COMMON_FILE_HDR_
#include "common/expression.h"
#include "common/literals.h"

#include <cstdint>
#include <optional>
#include <unordered_map>
#include <vector>

namespace common {
    class File {
      public:
        File(Literals &&literals) : literals_(std::move(literals)) {}

        std::optional<UnaryExpression> get_unary_expression(Expression::ID id) const {
            auto it = unary_exprs_.find(id);
            return it == unary_exprs_.end() ? std::optional<UnaryExpression>{} : it->second;
        }

        std::optional<BinaryExpression> get_binary_expression(Expression::ID id) const {
            auto it = binary_exprs_.find(id);
            return it == binary_exprs_.end() ? std::optional<BinaryExpression>{} : it->second;
        }

        std::optional<Literal> get_literal(Expression::ID id) const {
            auto it = literal_exprs_.find(id);
            return it == literal_exprs_.end() ? std::optional<Literal>{} : it->second;
        }

        std::optional<Cast> get_cast(Expression::ID id) const {
            auto it = casts_.find(id);
            return it == casts_.end() ? std::optional<Cast>{} : it->second;
        }

        Expression::ID add(BinaryExpression expr) {
            Expression::ID result{current_id_};
            ++current_id_;
            binary_exprs_[result] = expr;
            return result;
        }

        Expression::ID add(UnaryExpression expr) {
            Expression::ID result{current_id_};
            ++current_id_;
            unary_exprs_[result] = expr;
            return result;
        }

        Expression::ID add(Literal lit) {
            Expression::ID result{current_id_};
            ++current_id_;
            literal_exprs_[result] = lit;
            return result;
        }

        Expression::ID add(Cast cast) {
            Expression::ID result{current_id_};
            ++current_id_;
            casts_[result] = cast;
            return result;
        }

        Literals &literals() { return literals_; }
        const Literals &literals() const { return literals_; }

        // TODO: this is temporary API
        Expression start() const { return start_; }
        void set_start(Expression expr) { start_ = expr; }

        bool operator==(const File &other) const {
            return start_ == other.start_ &&
                   unary_exprs_ == other.unary_exprs_ &&
                   binary_exprs_ == other.binary_exprs_ &&
                   literal_exprs_ == other.literal_exprs_ &&
                   literals_ == other.literals_;
        }

      private:
        Expression start_{};
        std::unordered_map<Expression::ID, UnaryExpression> unary_exprs_;
        std::unordered_map<Expression::ID, BinaryExpression> binary_exprs_;
        std::unordered_map<Expression::ID, Literal> literal_exprs_;
        std::unordered_map<Expression::ID, Cast> casts_;
        Literals literals_;
        uint64_t current_id_ = 0;
    };
} // namespace common
#endif
