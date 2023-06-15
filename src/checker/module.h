#pragma once
#include <climits>
#include <cstddef>
#include <stdexcept>
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
#include "util/error_handler.h"

namespace module {

    using TypeID = uint64_t;
    using VariableID = size_t;

    constexpr TypeID g_invalid_typeid = 0;
    constexpr TypeID g_none_type = TypeID(-2);

    constexpr TypeID g_max_type_id = uint64_t(-1) - (uint64_t(0xff) << 56);

    enum class TypeKind : uint8_t {
        ALIAS = 1, STRUCT, FUNCTION, MODIFIED_TYPE, BUILTIN
    };

    inline TypeKind get_kind(TypeID id) noexcept {
        return TypeKind((id >> 56) & 0xff);
    }

    inline uint64_t get_index(TypeID id) noexcept {
        return id & g_max_type_id;
    }

    inline TypeID make_type_id(TypeKind kind, uint64_t index) {
        if (index > g_max_type_id) {
            throw std::out_of_range("out of range of valid ids for type kind: " + std::to_string(int(kind)));
        }

        return (uint64_t(kind) << 56) | index;
    }

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
        TypeID function{};
        std::vector<Expression> args;
    };

    struct Literal {
        util::Variant<uint64_t, double> value;
    };

    struct Expression {
        util::Variant<VariableID, Literal, BinaryExpression, UnaryExpression, FunctionCall> expr;
        std::optional<TypeID> result_type;
        parser::ActionType action;
        util::Position pos;
    };

    struct Variable {
        std::string name;
        TypeID type{};
        Expression* initial_value = nullptr;
    };

    enum class TypeProperties {
        NONE, INTEGRAL, FLOATING_POINT, BOOLEAN, POINTER, CALLABLE
    };

    struct TypeInfo {
        std::string name;
        size_t size = 0;
        TypeProperties properties = TypeProperties::NONE;
    };

    struct AliasInfo {
        TypeID underlying_type{};
    };

    struct StructInfo {
        std::vector<Variable> fields;
    };

    struct FunctionInfo {
        std::vector<Variable> args;
        std::optional<TypeID> return_type;
    };

    struct ModifiedType {
        std::vector<parser::TypeModifiers> modifiers;
        TypeID underlying_type{};
    };

    class Module {
    public:
        Module(util::ErrorHandler& err)
            : err_(&err)
        {}

        std::optional<TypeID> get_type_id(std::string_view name) const;

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

        TypeID register_alias(AliasInfo info, TypeInfo general_info) { 
            return register_info(std::move(info), std::move(general_info), aliases_, TypeKind::ALIAS); 
        }

        TypeID register_struct(StructInfo info, TypeInfo general_info) { 
            return register_info(std::move(info), std::move(general_info), structs_, TypeKind::STRUCT); 
        }

        TypeID register_function(FunctionInfo info, TypeInfo general_info) { 
            return register_info(std::move(info), std::move(general_info), functions_, TypeKind::FUNCTION); 
        }

        TypeID register_modified_type(ModifiedType info, TypeInfo general_info) {
            return register_info(std::move(info), std::move(general_info), modified_types_, TypeKind::MODIFIED_TYPE);
        }

        TypeID register_builtin(TypeInfo info);

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
        template<typename T>
        TypeID register_info(T&& info, TypeInfo general_info, std::deque<T>& container, TypeKind kind) {
            if(name_to_type_id_.contains(general_info.name)) {
                err_->checker_error("type " + general_info.name + " already declared");
            }
            TypeID id = make_type_id(kind, container.size());
            auto& ref = container.emplace_back(std::forward<T>(info));
            TypeInfo& gen_info = id_to_info_[id];
            gen_info = std::move(general_info);
            if(!gen_info.name.empty()) {
                name_to_type_id_[gen_info.name] = id;
            }
            return id;
        }

        TypeID next_id_ = 0;
        std::deque<AliasInfo> aliases_;
        std::deque<StructInfo> structs_;
        std::deque<FunctionInfo> functions_;
        std::deque<ModifiedType> modified_types_;

        std::deque<Variable> variables_;

        //should this be here?
        util::Arena<Expression> expressions_arena_;

        std::unordered_map<std::string_view, TypeID> name_to_type_id_;
        std::unordered_map<TypeID, TypeInfo> id_to_info_;

        std::unordered_map<std::string_view, VariableID> name_to_var_id_;

        util::ErrorHandler* err_;
    };
}
