#pragma once
#include <cstddef>
#include <cstdint>
#include <limits>
#include <unordered_map>
#include <vector>
#include <list>

#include "types/ids.h"
#include "types/statement.h"
#include "util/arena.h"
#include "util/util.h"
#include "util/variant.h"


namespace checker {

    struct TypeInfo {
        util::StringConstRef name = nullptr;
        size_t size = 0;
        size_t pos = 0;
    };

    struct Field {
        util::StringConstRef name;
        size_t pos = 0;
        types::TypeID type = types::k_undefined_type;
        types::Expression* default_value = nullptr;
    };

    struct Function {
        std::vector<Field> params;
        types::TypeID return_type = types::k_undefined_type;
        types::ScopeID id = types::k_invalid_scope;
        types::Block* body = nullptr;
    };

    struct FunctionType {
        std::vector<types::TypeID> params;
        types::TypeID return_type = types::k_undefined_type;
    };

    struct Struct {
        std::vector<Field> fields;
    };

    struct TupleOrUnion {
        std::vector<types::TypeID> types;
        bool is_union = false;
    };

    struct Alias {
        types::TypeID underlying_type = types::k_undefined_type;
    };

    using Type = util::Variant<FunctionType, Struct, TupleOrUnion, Alias>;
    
    struct Variable {
        types::TypeID type = types::k_undefined_type;
        types::Expression* default_value = nullptr;
    };

    struct Symbol {
        util::StringConstRef name = nullptr;
        size_t pos = 0;
        size_t size = 0;
        util::Variant<Function, Type, Variable> info;
        bool poisoned = false;
    };

    struct ScopedSymbol {
        types::ScopeID scope = types::k_global_scope;
        types::SymbolID symbol = types::k_invalid_symbol;
    };

    class Scope {
    public:
        types::SymbolID add_symbol(Symbol sym);
        Symbol& get_symbol(types::SymbolID id);
        Symbol* get_symbol(util::StringConstRef name);

        types::ScopeID get_parent() const;
        bool is_global() const { return parent == types::k_global_scope; }
    private:
        std::unordered_map<util::StringConstRef, types::SymbolID> name_to_symbol;
        std::vector<Symbol> symbols;
        size_t start_pos = 0;
        size_t end_pos = 0;
        types::ScopeID parent = types::k_invalid_scope;
    };

    class Module {
    public:

        Symbol& get_symbol_by_name(util::StringConstRef name, types::ScopeID scope = types::k_global_scope);
        types::TypeID get_type_id_by_name(util::StringConstRef name, types::ScopeID scope = types::k_global_scope);

        Symbol& get_symbol(types::SymbolID id);
        //Type& get_type(SymbolID id);

        bool is_type(util::StringConstRef name, types::ScopeID scope = types::k_global_scope);
        bool is_function(util::StringConstRef name, types::ScopeID scope = types::k_global_scope);
        bool is_variable(util::StringConstRef name, types::ScopeID scope = types::k_global_scope);

        types::ScopeID push_scope();
        void pop_scope();

        types::SymbolID add_symbol_to_current_scope(Symbol symbol);

    private:
        //TODO: imports
        types::ScopeID current_scope = types::k_global_scope;
        std::vector<Scope> scopes;
        std::unordered_map<util::StringConstRef, std::list<ScopedSymbol>> sym_table;
    };

}
