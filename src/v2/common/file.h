#ifndef COMPILER_V2_COMMON_FILE_HDR_
#define COMPILER_V2_COMMON_FILE_HDR_
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/util.h"

#include <cstdint>
#include <optional>
#include <unordered_map>
#include <vector>

namespace common {
    class File {
      public:
        File(Literals &&literals) : literals_(std::move(literals)) {}

        UnaryExpression *get_unary_expression(Expression::ID id) {
            auto [type, idx] = deconstruct(id);
            if (type != ExpressionType::UNARY || idx >= unary_exprs_.size()) {
                return nullptr;
            }
            return &unary_exprs_[idx];
        }

        BinaryExpression *get_binary_expression(Expression::ID id) {
            auto [type, idx] = deconstruct(id);
            if (type != ExpressionType::BINARY || idx >= binary_exprs_.size()) {
                return nullptr;
            }
            return &binary_exprs_[idx];
        }

        std::optional<Literal> get_literal(Expression::ID id) const {
            auto [type, idx] = deconstruct(id);
            if (type != ExpressionType::LITERAL || idx >= literal_exprs_.size()) {
                return {};
            }
            return literal_exprs_[idx];
        }

        Cast *get_cast(Expression::ID id) {
            auto [type, idx] = deconstruct(id);
            if (type != ExpressionType::CAST || idx >= casts_.size()) {
                return nullptr;
            }
            return &casts_[idx];
        }

        FunctionCall *get_call(Expression::ID id) {
            auto [type, idx] = deconstruct(id);
            if (type != ExpressionType::FUNCTION_CALL || idx >= calls_.size()) {
                return nullptr;
            }
            return &calls_[idx];
        }

        Expression::ID add(BinaryExpression expr) {
            Expression::ID result{make_id(ExpressionType::BINARY, binary_exprs_.size())};
            binary_exprs_.push_back(expr);
            return result;
        }

        Expression::ID add(UnaryExpression expr) {
            Expression::ID result{make_id(ExpressionType::UNARY, unary_exprs_.size())};
            unary_exprs_.push_back(expr);
            return result;
        }

        Expression::ID add(Literal lit) {
            Expression::ID result{make_id(ExpressionType::LITERAL, literal_exprs_.size())};
            literal_exprs_.push_back(lit);
            return result;
        }

        Expression::ID add(Cast cast) {
            Expression::ID result{make_id(ExpressionType::CAST, casts_.size())};
            casts_.push_back(cast);
            return result;
        }

        Expression::ID add(FunctionCall cast) {
            Expression::ID result{make_id(ExpressionType::FUNCTION_CALL, calls_.size())};
            calls_.push_back(cast);
            return result;
        }

        Function::ID add(Function func) {
            func.id = Function::ID{functions_.size()};
            functions_.push_back(func);
            return func.id;
        }

        Function *get_function(Function::ID id) {
            if (*id >= functions_.size()) {
                return nullptr;
            }
            return &functions_[*id];
        }

        std::vector<Function> &functions() { return functions_; }
        const std::vector<Function> &functions() const { return functions_; }

        Literals &literals() { return literals_; }
        const Literals &literals() const { return literals_; }

        // TODO: this is temporary API for tests
        // later this should be replaced by something more permanent
        Expression start_expression() const { return start_; }
        void set_start_expression(Expression expr) { start_ = expr; }

        bool operator==(const File &other) const {
            return start_ == other.start_ &&
                   unary_exprs_ == other.unary_exprs_ &&
                   binary_exprs_ == other.binary_exprs_ &&
                   literal_exprs_ == other.literal_exprs_ &&
                   literals_ == other.literals_;
        }

      private:
        static Expression::ID make_id(ExpressionType type, uint64_t id) {
            return Expression::ID{(static_cast<uint64_t>(type) << 56) | id};
        }

        static std::pair<ExpressionType, uint64_t> deconstruct(Expression::ID id) {
            return {static_cast<ExpressionType>(*id >> 56), *id & 0xffffffffffffff};
        }

        Expression start_{};
        std::vector<UnaryExpression> unary_exprs_;
        std::vector<BinaryExpression> binary_exprs_;
        std::vector<Literal> literal_exprs_;
        std::vector<Cast> casts_;
        std::vector<FunctionCall> calls_;
        std::vector<Function> functions_;
        Literals literals_;
    };
} // namespace common
#endif
