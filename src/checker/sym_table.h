#pragma once
#include <cstddef>
#include <cstdint>
#include <limits>
#include <unordered_map>
#include <vector>
#include <list>

#include "util/arena.h"
#include "util/util.h"
#include "util/variant.h"


namespace checker {
    struct Scope;
    struct Symbol;


    using ScopeID = util::Distinct<int32_t, Symbol>;



    constexpr ScopeID k_invalid_scope = ScopeID(-1);
    constexpr ScopeID k_global_scope = ScopeID(0);

    struct SymbolID {
        ScopeID scope = k_global_scope;
        int32_t id = -1;
    };

    constexpr SymbolID k_invalid_symbol = SymbolID{};

    using TypeID = util::Distinct<SymbolID, void>;
    constexpr TypeID k_invalid_id = TypeID(k_invalid_symbol);
    constexpr TypeID k_undefined_id = TypeID(SymbolID{.id = -2});
    constexpr TypeID k_none_id = TypeID(SymbolID{.id = -3});

    struct TypeInfo {
        util::StringConstRef name = nullptr;
        size_t size = 0;
        size_t pos = 0;
    };

    struct Expression;
    struct Block;

    struct Field {
        util::StringConstRef name;
        size_t pos = 0;
        TypeID type = k_undefined_id;
        Expression* default_value = nullptr;
    };

    struct Function {
        std::vector<Field> params;
        TypeID return_type = k_undefined_id;
        Block* body;
    };

    struct FunctionType {
        std::vector<TypeID> params;
        TypeID return_type = k_undefined_id;
    };

    struct Struct {
        std::vector<Field> fields;
    };

    struct TupleOrUnion {
        std::vector<TypeID> types;
        bool is_union = false;
    };

    struct Alias {
        TypeID underlying_type = k_undefined_id;
    };

    using Type = util::Variant<FunctionType, Struct, TupleOrUnion, Alias>;
    
    struct Variable {
        TypeID type = k_undefined_id;
        Expression* default_value = nullptr;
    };

    struct Symbol {
        util::StringConstRef name = nullptr;
        size_t pos = 0;
        size_t size = 0;
        util::Variant<Function, Type, Variable> info;
        bool poisoned = false;
    };

    struct ScopedSymbol {
        ScopeID scope = k_global_scope;
        SymbolID symbol = k_invalid_symbol;
    };

    class Scope {
    public:
        SymbolID add_symbol(Symbol sym);
        Symbol& get_symbol(SymbolID id);
        Symbol* get_symbol(util::StringConstRef name);

        ScopeID get_parent() const;
        bool is_global() const { return parent == k_global_scope; }
    private:
        std::unordered_map<util::StringConstRef, SymbolID> name_to_symbol;
        std::vector<Symbol> symbols;
        size_t start_pos = 0;
        size_t end_pos = 0;
        ScopeID parent = k_invalid_scope;
    };

    class Module_ {
    public:

        Symbol& get_symbol_by_name(util::StringConstRef name, ScopeID scope = k_global_scope);
        TypeID get_type_id_by_name(util::StringConstRef name, ScopeID scope = k_global_scope);

        Symbol& get_symbol(SymbolID id);
        //Type& get_type(SymbolID id);

        bool is_type(util::StringConstRef name, ScopeID scope = k_global_scope);
        bool is_function(util::StringConstRef name, ScopeID scope = k_global_scope);
        bool is_variable(util::StringConstRef name, ScopeID scope = k_global_scope);

        ScopeID push_scope();
        void pop_scope();

        SymbolID add_symbol_to_current_scope(Symbol symbol);

    private:
        //TODO: imports
        ScopeID current_scope = k_global_scope;
        std::vector<Scope> scopes;
        std::unordered_map<util::StringConstRef, std::list<ScopedSymbol>> sym_table;
    };

}
