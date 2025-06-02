#ifndef COMPILER_V2_COMMON_STATEMENT_HDR_
#define COMPILER_V2_COMMON_STATEMENT_HDR_

#include "common/base_classes.h"
#include "common/util.h"

#include <memory>
#include <optional>
#include <utility>
#include <vector>

namespace common {

    class BreakStatement final : public Statement {
      public:
        explicit BreakStatement(TokenPos pos)
            : Statement(static_kind(), pos, true) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(BreakStatement,
                                                     StatementType,
                                                     StatementType::BREAK,
                                                     default)
    };

    class ErrorStatement final : public Statement {
      public:
        explicit ErrorStatement(TokenPos pos)
            : Statement(static_kind(), pos, true) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(ErrorStatement,
                                                     StatementType,
                                                     StatementType::ERROR,
                                                     default)
    };

    class ContinueStatement final : public Statement {
      public:
        explicit ContinueStatement(TokenPos pos)
            : Statement(static_kind(), pos, true) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(ContinueStatement,
                                                     StatementType,
                                                     StatementType::CONTINUE,
                                                     default)
    };

    class ExpressionStatement final : public Statement {
      public:
        ExpressionStatement(std::unique_ptr<Expression> &&expr, TokenPos pos)
            : Statement(static_kind(), pos, true), expr_(std::move(expr)) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(ExpressionStatement,
                                                     StatementType,
                                                     StatementType::EXPRESSION,
                                                     delete)

        std::unique_ptr<Expression> &expression() noexcept { return expr_; }
        const Expression *expression() const noexcept { return expr_.get(); }

        void set_expression(std::unique_ptr<Expression> &&expr) {
            expr_ = std::move(expr);
        }

      private:
        std::unique_ptr<Expression> expr_;
    };

    class Return final : public Statement {
      public:
        Return(std::unique_ptr<Expression> &&expr, TokenPos pos)
            : Statement(static_kind(), pos, true), expr_(std::move(expr)) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(Return, StatementType,
                                                     StatementType::RETURN,
                                                     delete)

        std::unique_ptr<Expression> &expression() noexcept { return expr_; }
        const Expression *expression() const noexcept { return expr_.get(); }

        void set_expression(std::unique_ptr<Expression> &&expr) {
            expr_ = std::move(expr);
        }

      private:
        std::unique_ptr<Expression> expr_;
    };

    class Block final : public Statement {
      public:
        Block(TokenPos pos, std::vector<std::unique_ptr<Statement>> &&smts = {})
            : Statement(static_kind(), pos, true), smts_(std::move(smts)) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(Block, StatementType,
                                                     StatementType::BLOCK,
                                                     delete)

        std::vector<std::unique_ptr<Statement>> &statements() noexcept {
            return smts_;
        }
        const std::vector<std::unique_ptr<Statement>> &
        statements() const noexcept {
            return smts_;
        }

      private:
        std::vector<std::unique_ptr<Statement>> smts_;
    };

    class Branch final : public Statement {
      public:
        Branch(std::unique_ptr<Expression> &&predicate, Block &&true_branch,
               std::optional<Block> &&false_branch, TokenPos pos)
            : Statement(static_kind(), pos, true),
              predicate_(std::move(predicate)),
              true_branch_(std::move(true_branch)),
              false_branch_(std::move(false_branch)) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(Branch, StatementType,
                                                     StatementType::BRANCH,
                                                     delete)

        std::unique_ptr<Expression> &predicate() noexcept { return predicate_; }
        const Expression *predicate() const noexcept {
            return predicate_.get();
        }

        Block &true_branch() noexcept { return true_branch_; }
        std::optional<Block> &false_branch() noexcept { return false_branch_; }

        const Block &true_branch() const noexcept { return true_branch_; }
        const std::optional<Block> &false_branch() const noexcept {
            return false_branch_;
        }

      private:
        std::unique_ptr<Expression> predicate_;
        Block true_branch_;
        std::optional<Block> false_branch_;
    };

    class VariableDeclatarion final : public Statement {
      public:
        VariableDeclatarion(VariableID var, TokenPos pos)
            : Statement(static_kind(), pos, true), var_(var) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(VariableDeclatarion,
                                                     StatementType,
                                                     StatementType::VARIABLE,
                                                     default)

        VariableID variable() const noexcept { return var_; }

      private:
        VariableID var_;
    };

    class Loop final : public Statement {
      public:
        explicit Loop(TokenPos pos)
            : Statement(static_kind(), pos, true), body_(pos) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(Loop, StatementType,
                                                     StatementType::LOOP,
                                                     delete)

        Statement *init() noexcept { return init_.get(); }
        std::unique_ptr<Expression> &condition() noexcept { return condition_; }
        std::unique_ptr<Expression> &iteration() noexcept { return iteration_; }
        Block &body() noexcept { return body_; }

        const Statement *init() const noexcept { return init_.get(); }
        const Expression *condition() const noexcept {
            return condition_.get();
        }
        const Expression *iteration() const noexcept {
            return iteration_.get();
        }
        const Block &body() const noexcept { return body_; }

        void set_init(std::unique_ptr<Statement> &&smt) {
            init_ = std::move(smt);
        }
        void set_condition(std::unique_ptr<Expression> &&expr) {
            condition_ = std::move(expr);
        }
        void set_iteration(std::unique_ptr<Expression> &&expr) {
            iteration_ = std::move(expr);
        }

      private:
        std::unique_ptr<Statement> init_;
        std::unique_ptr<Expression> condition_;
        std::unique_ptr<Expression> iteration_;
        Block body_;
    };

} // namespace common

#endif
