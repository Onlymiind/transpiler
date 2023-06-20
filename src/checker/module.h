#pragma once
#include <climits>
#include <cstddef>
#include <stdexcept>
#include <utility>
#include <vector>
#include <string>
#include <optional>
#include <unordered_map>
#include <unordered_set>

#include "parser/declaration.h"
#include "parser/expression.h"
#include "parser/parser.h"
#include "parser/statement.h"
#include "util/arena.h"
#include "util/util.h"
#include "util/variant.h"
#include "util/error_handler.h"

namespace module {

    using ID = uint64_t;

    constexpr ID g_invalid_id = 0;
    constexpr ID g_none_type = ID(-2);

    constexpr ID g_max_id = uint64_t(-1) - (uint64_t(0xff) << 56);

    enum class IDKind : uint8_t {
        ALIAS = 1, STRUCT, FUNCTION, MODIFIED_TYPE, BUILTIN, VARIABLE
    };

    inline IDKind get_kind(ID id) noexcept {
        return IDKind((id >> 56) & 0xff);
    }

    inline uint64_t get_index(ID id) noexcept {
        return id & g_max_id;
    }

    inline ID make_id(IDKind kind, uint64_t index) {
        if (index > g_max_id) {
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
        ID function{};
        std::vector<Expression> args;
    };

    struct Literal {
        util::Variant<uint64_t, double> value;
    };

    struct Expression {
        util::Variant<ID, Literal, BinaryExpression, UnaryExpression, FunctionCall> expr;
        std::optional<ID> result_type;
        parser::ActionType action;
        util::Position pos;
    };

    struct Variable {
        ID type{};
        Expression* initial_value = nullptr;
    };

    struct Field {
        std::string name;
        ID type;
        Expression* default_value = nullptr;
    };

    struct AliasInfo {
        ID underlying_type{};
    };

    struct StructInfo {
        std::vector<Field> fields;
    };

    struct FunctionInfo {
        std::vector<Field> args;
        std::optional<ID> return_type;
    };

    struct ModifiedType {
        std::vector<parser::TypeModifiers> modifiers;
        ID underlying_type{};
    };

    class Module {
    public:
        Module(util::ErrorHandler& err)
            : err_(&err)
        {}

        std::optional<ID> get_type_id(const std::string& name) const;

        std::optional<ID> get_var_id(const std::string& name) const;

        AliasInfo* get_alias_info(ID id);
        StructInfo* get_struct_info(ID id);
        FunctionInfo* get_funtion_info(ID id);

        const AliasInfo* get_alias_info(ID id) const;
        const StructInfo* get_struct_info(ID id) const;
        const FunctionInfo* get_function_info(ID id) const;

        ID register_alias(AliasInfo info, const std::string& name) { 
            return register_info(std::move(info), name, aliases_, IDKind::ALIAS); 
        }

        ID register_struct(StructInfo info, const std::string& name) { 
            return register_info(std::move(info), name, structs_, IDKind::STRUCT); 
        }


        ID register_modified_type(ModifiedType info, const std::string& name) {
            return register_info(std::move(info), name, modified_types_, IDKind::MODIFIED_TYPE);
        }

        ID register_builtin(const std::string& name);

        ID register_function(FunctionInfo info, const std::string& name);

        ID register_variable(Variable var, const std::string& name);

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
        ID register_info(T&& info, const std::string& name, std::deque<T>& container, IDKind kind) {
            ID id = make_id(kind, container.size());
            add_name(name, id);
            container.emplace_back(std::forward<T>(info));
            return id;
        }

        void add_name(const std::string& name, ID type_id) {
            if(name.empty()) {
                return;
            }
            if(sym_table_.contains(name)) {
                err_->checker_error("name " + name + " already declared");
            }
            sym_table_[name] = type_id;
        }

        std::deque<AliasInfo> aliases_;
        std::deque<StructInfo> structs_;
        std::deque<FunctionInfo> functions_;
        std::deque<ModifiedType> modified_types_;
        std::deque<Variable> variables_;

        //should this be here?
        util::Arena<Expression> expressions_arena_;

        std::unordered_map<std::string, ID> sym_table_;

        util::ErrorHandler* err_;
    };
}
