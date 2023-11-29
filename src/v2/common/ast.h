#ifndef COMPILER_V2_COMMON_FILE_HDR_
#define COMPILER_V2_COMMON_FILE_HDR_
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/statement.h"
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

        IdentifierID get_variable_ref(ExpressionID id) {
            auto [type, idx] = decompose<ExpressionKind>(id);
            return type != ExpressionKind::VARIABLE_REF || idx >= variable_refs_.size() ? IdentifierID{} : variable_refs_[idx];
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

        ExpressionID add_variable_ref(IdentifierID name) {
            ExpressionID result{make_id<ExpressionID>(ExpressionKind::VARIABLE_REF, variable_refs_.size())};
            variable_refs_.push_back(name);
            return result;
        }

        void add(Function func) {
            func.id = FunctionID{functions_.size()};
            functions_.push_back(std::move(func));
        }

        void add_global(Variable var) {
            var.id = VariableID{vars_.size()};
            global_variables_.push_back(var.id);
            vars_.push_back(std::move(var));
        }

        StatementID add(Expression expr) {
            StatementID result{make_id<StatementID>(StatementType::EXPRESSION, statements_.size())};
            statements_.push_back(expr);
            return result;
        }

        StatementID add_local(Variable var) {
            var.id = VariableID{vars_.size()};
            StatementID result{make_id<StatementID>(StatementType::VARIABLE, *var.id)};
            vars_.push_back(std::move(var));
            return result;
        }

        Expression *get_expression(StatementID smt) {
            auto [type, idx] = decompose<StatementType>(smt);
            return type != StatementType::EXPRESSION || idx >= statements_.size() ? nullptr : &statements_[idx];
        }

        Variable *get_var(VariableID id) {
            return *id >= vars_.size() ? nullptr : &vars_[*id];
        }

        Variable *get_var(StatementID smt) {
            auto [type, idx] = decompose<StatementType>(smt);
            return type != StatementType::VARIABLE || idx >= vars_.size() ? nullptr : &vars_[idx];
        }

        Function *get_function(FunctionID id) {
            if (*id >= functions_.size()) {
                return nullptr;
            }
            return &functions_[*id];
        }

        std::vector<Function> &functions() { return functions_; }
        const std::vector<Function> &functions() const { return functions_; }

        std::vector<Variable> &variables() { return vars_; }
        const std::vector<Variable> &variables() const { return vars_; }

        std::vector<VariableID> &global_variables() { return global_variables_; }
        const std::vector<VariableID> &global_variables() const { return global_variables_; }

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
        std::vector<IdentifierID> variable_refs_;

        std::vector<Function> functions_;
        std::vector<Variable> vars_;

        std::vector<Expression> statements_;

        std::vector<VariableID> global_variables_;
    };
} // namespace common
#endif
