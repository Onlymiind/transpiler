#ifndef COMPILER_V2_COMMON_STATEMENT_HDR_
#define COMPILER_V2_COMMON_STATEMENT_HDR_

#include "common/base_classes.h"
#include "common/expression.h"
#include "common/types.h"
#include "common/util.h"

#include <cstddef>
#include <memory>
#include <mutex>
#include <utility>
#include <vector>

namespace common {

    class BreakStatement final : public Statement {
      public:
        BreakStatement(size_t pos)
            : Statement(StatementType::BREAK, pos, true) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(BreakStatement, default)
    };

    class EmptyStatement final : public Statement {
      public:
        EmptyStatement(size_t pos)
            : Statement(StatementType::EMPTY, pos, true) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(EmptyStatement, default)
    };

    class ErrorStatement final : public Statement {
      public:
        ErrorStatement(size_t pos)
            : Statement(StatementType::ERROR, pos, true) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(ErrorStatement, default)
    };

    class ContinueStatement final : public Statement {
      public:
        ContinueStatement(size_t pos)
            : Statement(StatementType::CONTINUE, pos, true) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(ContinueStatement, default)
    };

    class ExpressionStatement final : public Statement {
      public:
        ExpressionStatement(std::unique_ptr<Expression> &&expr, size_t pos)
            : Statement(StatementType::EXPRESSION, pos, true), expr_(std::move(expr)) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(ExpressionStatement, delete)

        std::unique_ptr<Expression> &expression() noexcept { return expr_; }
        const Expression *expression() const noexcept { return expr_.get(); }

        void set_expression(std::unique_ptr<Expression> &&expr) { expr_ = std::move(expr); }

      private:
        std::unique_ptr<Expression> expr_;
    };

    class Return final : public Statement {
      public:
        Return(std::unique_ptr<Expression> &&expr, size_t pos)
            : Statement(StatementType::RETURN, pos, true), expr_(std::move(expr)) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(Return, delete)

        std::unique_ptr<Expression> &expression() noexcept { return expr_; }
        const Expression *expression() const noexcept { return expr_.get(); }

        void set_expression(std::unique_ptr<Expression> &&expr) { expr_ = std::move(expr); }

      private:
        std::unique_ptr<Expression> expr_;
    };

    class Block final : public Statement {
      public:
        Block(size_t pos, std::vector<std::unique_ptr<Statement>> &&smts = {})
            : Statement(StatementType::BLOCK, pos, true), smts_(std::move(smts)) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(Block, delete)

        std::vector<std::unique_ptr<Statement>> &statements() noexcept { return smts_; }
        const std::vector<std::unique_ptr<Statement>> &statements() const noexcept { return smts_; }

      private:
        std::vector<std::unique_ptr<Statement>> smts_;
    };

    class Branch final : public Statement {
      public:
        Branch(std::unique_ptr<Expression> &&predicate, Block &&true_branch, std::optional<Block> &&false_branch, size_t pos)
            : Statement(StatementType::BRANCH, pos, true), predicate_(std::move(predicate)),
              true_branch_(std::move(true_branch)), false_branch_(std::move(false_branch)) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(Branch, delete)

        std::unique_ptr<Expression> &predicate() noexcept { return predicate_; }
        const Expression *predicate() const noexcept { return predicate_.get(); }

        Block &true_branch() noexcept { return true_branch_; }
        std::optional<Block> &false_branch() noexcept { return false_branch_; }

        const Block &true_branch() const noexcept { return true_branch_; }
        const std::optional<Block> &false_branch() const noexcept { return false_branch_; }

      private:
        std::unique_ptr<Expression> predicate_;
        Block true_branch_;
        std::optional<Block> false_branch_;
    };

    class VariableDeclatarion final : public Statement {
      public:
        VariableDeclatarion(VariableID var, size_t pos)
            : Statement(StatementType::VARIABLE, pos, true), var_(var) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(VariableDeclatarion, default)

        VariableID variable() const noexcept { return var_; }

      private:
        VariableID var_;
    };

    class Loop final : public Statement {
      public:
        Loop(size_t pos)
            : Statement(StatementType::LOOP, pos, true), body_(pos) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(Loop, delete)

        Statement *init() noexcept { return init_.get(); }
        std::unique_ptr<Expression> &condition() noexcept { return condition_; }
        std::unique_ptr<Expression> &iteration() noexcept { return iteration_; }
        Block &body() noexcept { return body_; }

        const Statement *init() const noexcept { return init_.get(); }
        const Expression *condition() const noexcept { return condition_.get(); }
        const Expression *iteration() const noexcept { return iteration_.get(); }
        const Block &body() const noexcept { return body_; }

        void set_init(std::unique_ptr<Statement> &&smt) { init_ = std::move(smt); }
        void set_condition(std::unique_ptr<Expression> &&expr) { condition_ = std::move(expr); }
        void set_iteration(std::unique_ptr<Expression> &&expr) { iteration_ = std::move(expr); }

      private:
        std::unique_ptr<Statement> init_;
        std::unique_ptr<Expression> condition_;
        std::unique_ptr<Expression> iteration_;
        Block body_;
    };

} // namespace common

#endif
