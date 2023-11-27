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
            auto [type, idx] = decompose<ExpressionKind>(id);
            if (type != ExpressionKind::UNARY || idx >= unary_exprs_.size()) {
                return nullptr;
            }
            return &unary_exprs_[idx];
        }

        BinaryExpression *get_binary_expression(ExpressionID id) {
            auto [type, idx] = decompose<ExpressionKind>(id);
            if (type != ExpressionKind::BINARY || idx >= binary_exprs_.size()) {
                return nullptr;
            }
            return &binary_exprs_[idx];
        }

        std::optional<Literal> get_literal(ExpressionID id) const {
            auto [type, idx] = decompose<ExpressionKind>(id);
            if (type != ExpressionKind::LITERAL || idx >= literal_exprs_.size()) {
                return {};
            }
            return literal_exprs_[idx];
        }

        Cast *get_cast(ExpressionID id) {
            auto [type, idx] = decompose<ExpressionKind>(id);
            if (type != ExpressionKind::CAST || idx >= casts_.size()) {
                return nullptr;
            }
            return &casts_[idx];
        }

        FunctionCall *get_call(ExpressionID id) {
            auto [type, idx] = decompose<ExpressionKind>(id);
            if (type != ExpressionKind::FUNCTION_CALL || idx >= calls_.size()) {
                return nullptr;
            }
            return &calls_[idx];
        }

        ExpressionID add(BinaryExpression expr) {
            ExpressionID result{make_id<ExpressionID>(ExpressionKind::BINARY, binary_exprs_.size())};
            binary_exprs_.push_back(expr);
            return result;
        }

        ExpressionID add(UnaryExpression expr) {
            ExpressionID result{make_id<ExpressionID>(ExpressionKind::UNARY, unary_exprs_.size())};
            unary_exprs_.push_back(expr);
            return result;
        }

        ExpressionID add(Literal lit) {
            ExpressionID result{make_id<ExpressionID>(ExpressionKind::LITERAL, literal_exprs_.size())};
            literal_exprs_.push_back(lit);
            return result;
        }

        ExpressionID add_cast(Cast cast) {
            ExpressionID result{make_id<ExpressionID>(ExpressionKind::CAST, casts_.size())};
            casts_.push_back(cast);
            return result;
        }

        ExpressionID add(FunctionCall cast) {
            ExpressionID result{make_id<ExpressionID>(ExpressionKind::FUNCTION_CALL, calls_.size())};
            calls_.push_back(cast);
            return result;
        }

        FunctionID add(Function func) {
            func.id = FunctionID{functions_.size()};
            functions_.push_back(std::move(func));
            return func.id;
        }

        StatementID add(Expression expr) {
            StatementID result{statements_.size()};
            statements_.push_back(expr);
            return result;
        }

        Expression *get_expression(StatementID smt) {
            return smt == StatementID{} || *smt >= statements_.size() ? nullptr : &statements_[*smt];
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
        std::vector<UnaryExpression> unary_exprs_;
        std::vector<BinaryExpression> binary_exprs_;
        std::vector<Literal> literal_exprs_;
        std::vector<Cast> casts_;
        std::vector<FunctionCall> calls_;
        std::vector<Function> functions_;

        std::vector<Expression> statements_;
    };
} // namespace common
#endif
