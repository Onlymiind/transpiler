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

    constexpr TypeID g_invalid_typeid = TypeID(-1);
    constexpr TypeID g_none_type = TypeID(-2);

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
        std::string name;
        TypeID type;
        Expression* initial_value = nullptr;
    };

    enum class TypeProperties {
        NONE, INTEGRAL, FLOATING_POINT, BOOLEAN, POINTER, CALLABLE
    };

    enum class TypeCategory {
        UNKNOWN, ALIAS, FUNCTION, STRUCT, BUILTIN
    };

    struct TypeInfo {
        std::string name;
        size_t size = 0;
        TypeProperties properties = TypeProperties::NONE;
        TypeCategory category = TypeCategory::UNKNOWN;
    };

    struct AliasInfo {
        TypeInfo info;
        TypeID underlying_type;
    };

    struct StructInfo {
        TypeInfo info;
        std::vector<Variable> fields;
    };

    struct FunctionInfo {
        TypeInfo info;
        std::vector<Variable> args;
        std::optional<TypeID> return_type;
    };

    class Module {
    public:

        std::optional<TypeID> get_type_id(std::string_view name) const;
        std::optional<TypeID> type_by_decl(const parser::Declaration& decl) const;

        std::optional<VariableID> get_var_id(std::string_view name) const;

        TypeInfo* get_info(TypeID id);
        AliasInfo* get_alias_info(TypeID id);
        StructInfo* get_struct_info(TypeID id);
        FunctionInfo* get_funtion_info(TypeID id);

        const TypeInfo* get_info(TypeID id) const;
        const AliasInfo* get_alias_info(TypeID id) const;
        const StructInfo* get_struct_info(TypeID id) const;

        const FunctionInfo* get_function_info(TypeID id) const;
        bool has_action_support(TypeID id, parser::ActionType action) const;

        bool is_builtin(TypeID type) const;
        bool is_builtin(const parser::Declaration& decl) const;

        bool is_numeric(TypeID type) const;
        bool is_integral(TypeID type) const;
        bool is_floating_point(TypeID type) const;
        bool is_boolean(TypeID type) const;
        bool is_callable(TypeID type) const;

        TypeID instantiate_union(std::vector<TypeID> variants);
        TypeID instantiate_tuple(std::vector<TypeID> members);

        TypeID register_alias(AliasInfo info);
        TypeID register_struct(StructInfo info);
        TypeID register_function(FunctionInfo info);
        VariableID register_variable(Variable var);

        //TODO: decide on expression storage and allocation
        template<typename... Args>
        Expression* allocate_expression(Args&&... args) {
            return expressions_arena_.allocate(std::forward<Args>(args)...);
        }

        void reserve_expression_storage(size_t count) {
            expressions_arena_.reserve(count);
        }
    private:
        TypeID next_id_ = 0;
        std::deque<AliasInfo> aliases_;
        std::deque<StructInfo> structs_;
        std::deque<FunctionInfo> functions_;

        std::deque<Variable> variables_;

        //should this be here?
        util::Arena<Expression> expressions_arena_;

        std::unordered_map<std::string_view, TypeID> name_to_type_id_;
        std::unordered_map<TypeID, TypeInfo*> id_to_name_;

        std::unordered_map<std::string_view, VariableID> name_to_var_id_;
    };
}
