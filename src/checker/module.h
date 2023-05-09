#pragma once
#include <climits>
#include <cstddef>
#include <utility>
#include <vector>
#include <string>
#include <optional>
#include <unordered_map>

#include "parser/declaration.h"
#include "parser/expression.h"
#include "parser/parser.h"
#include "parser/statement.h"
#include "util/arena.h"
#include "util/util.h"
#include "util/variant.h"

namespace module {

    using TypeID = size_t;
    using VariableID = size_t;

    struct Expression;

    struct BinaryExpression {
        parser::ActionType action;
        Expression* lhs;
        Expression* rhs;
    };

    struct UnaryExpression {
        parser::ActionType action;
        Expression* arg;
    };

    struct FunctionCall {
        TypeID function;
        std::vector<Expression> args;
    };

    struct Literal {
        util::Variant<uint64_t, double> value;
    };

    struct Expression {
        util::Variant<VariableID, Literal, BinaryExpression, UnaryExpression, FunctionCall> expr;
        TypeID result_type;
        parser::ActionType action;
        util::Position pos;
    };

    struct Variable {
        TypeID type;
        Expression* initial_value = nullptr;
    };

    struct AliasInfo {
        TypeID underlying_type;
    };

    struct StructInfo {
        std::unordered_map<std::string, TypeID> fields;
    };

    struct FunctionInfo {
        std::vector<std::pair<std::string, TypeID>> args;
        std::optional<TypeID> return_type;
    };

    enum class TypeProperties {
        INTEGRAL, FLOATING_POINT, BOOLEAN, POINTER, CALLABLE
    };

    enum class TypeCategory {
        ALIAS, FUNCTION, STRUCT, BUILTIN
    };

    struct TypeInfo {
        std::string name;
        size_t size;
        TypeProperties properties;
        TypeCategory category;
    };


    class Module {
    public:

        std::optional<TypeID> type_by_name(std::string_view name) const;
        std::optional<TypeID> type_by_decl(const parser::Declaration& decl) const;

        std::optional<VariableID> var_by_name(const std::string& name) const;

        std::string_view name_by_id(TypeID id) const;

        AliasInfo* alias_by_id(TypeID id);
        StructInfo* struct_by_id(TypeID id);
        FunctionInfo* function_by_id(TypeID id);

        const AliasInfo* alias_by_id(TypeID id) const;
        const StructInfo* struct_by_id(TypeID id) const;
        const FunctionInfo* function_by_id(TypeID id) const;

        bool has_action_support(TypeID id, parser::ActionType action) const;

        bool is_builtin(TypeID type) const;
        bool is_builtin(const parser::Declaration& decl) const;

        bool is_numeric(TypeID type) const;
        bool is_integral(TypeID type) const;
        bool is_floating_point(TypeID type) const;
        bool is_boolean(TypeID type) const;
        bool is_callable(TypeID type) const;

        TypeID register_type(parser::Declaration decl);
        VariableID register_variable(std::string name, Variable var);

        //TODO: decide on expression storage and allocation
        template<typename... Args>
        Expression* allocate_expression(Args&&... args) {
            return expressions_arena_.allocate(std::forward<Args>(args)...);
        }

        void reserve_expression_storage(size_t count) {
            expressions_arena_.reserve(count);
        }
    private:
        std::vector<AliasInfo> aliases_;
        std::vector<StructInfo> structs_;
        std::vector<FunctionInfo> funcions_;

        std::vector<Variable> variables_;

        //should this be here?
        util::Arena<Expression> expressions_arena_;

        std::unordered_map<std::string_view, TypeID> name_to_type_id_;
        std::unordered_map<TypeID, TypeInfo> id_to_name_;

        std::unordered_map<std::string, VariableID> name_to_var_id_;
    };
}
