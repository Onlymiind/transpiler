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

    class AST {
      public:
        AST() = default;

        UnaryExpression *get_unary_expression(ExpressionID id) {
            auto [type, idx] = deconstruct(id);
            if (type != ExpressionType::UNARY || idx >= unary_exprs_.size()) {
                return nullptr;
            }
            return &unary_exprs_[idx];
        }

        BinaryExpression *get_binary_expression(ExpressionID id) {
            auto [type, idx] = deconstruct(id);
            if (type != ExpressionType::BINARY || idx >= binary_exprs_.size()) {
                return nullptr;
            }
            return &binary_exprs_[idx];
        }

        std::optional<Literal> get_literal(ExpressionID id) const {
            auto [type, idx] = deconstruct(id);
            if (type != ExpressionType::LITERAL || idx >= literal_exprs_.size()) {
                return {};
            }
            return literal_exprs_[idx];
        }

        Cast *get_cast(ExpressionID id) {
            auto [type, idx] = deconstruct(id);
            if (type != ExpressionType::CAST || idx >= casts_.size()) {
                return nullptr;
            }
            return &casts_[idx];
        }

        FunctionCall *get_call(ExpressionID id) {
            auto [type, idx] = deconstruct(id);
            if (type != ExpressionType::FUNCTION_CALL || idx >= calls_.size()) {
                return nullptr;
            }
            return &calls_[idx];
        }

        ExpressionID add(BinaryExpression expr) {
            ExpressionID result{make_id(ExpressionType::BINARY, binary_exprs_.size())};
            binary_exprs_.push_back(expr);
            return result;
        }

        ExpressionID add(UnaryExpression expr) {
            ExpressionID result{make_id(ExpressionType::UNARY, unary_exprs_.size())};
            unary_exprs_.push_back(expr);
            return result;
        }

        ExpressionID add(Literal lit) {
            ExpressionID result{make_id(ExpressionType::LITERAL, literal_exprs_.size())};
            literal_exprs_.push_back(lit);
            return result;
        }

        ExpressionID add(Cast cast) {
            ExpressionID result{make_id(ExpressionType::CAST, casts_.size())};
            casts_.push_back(cast);
            return result;
        }

        ExpressionID add(FunctionCall cast) {
            ExpressionID result{make_id(ExpressionType::FUNCTION_CALL, calls_.size())};
            calls_.push_back(cast);
            return result;
        }

        FunctionID add(Function func) {
            func.id = FunctionID{functions_.size()};
            functions_.push_back(func);
            return func.id;
        }

        Function *get_function(FunctionID id) {
            if (*id >= functions_.size()) {
                return nullptr;
            }
            return &functions_[*id];
        }

        std::vector<Function> &functions() { return functions_; }
        const std::vector<Function> &functions() const { return functions_; }

        bool operator==(const AST &other) const {
            return unary_exprs_ == other.unary_exprs_ &&
                   binary_exprs_ == other.binary_exprs_ &&
                   literal_exprs_ == other.literal_exprs_;
        }

      private:
        static ExpressionID make_id(ExpressionType type, uint64_t id) {
            return ExpressionID{(static_cast<uint64_t>(type) << 56) | id};
        }

        static std::pair<ExpressionType, uint64_t> deconstruct(ExpressionID id) {
            return {static_cast<ExpressionType>(*id >> 56), *id & 0xffffffffffffff};
        }

        std::vector<UnaryExpression> unary_exprs_;
        std::vector<BinaryExpression> binary_exprs_;
        std::vector<Literal> literal_exprs_;
        std::vector<Cast> casts_;
        std::vector<FunctionCall> calls_;
        std::vector<Function> functions_;
    };
} // namespace common
#endif
