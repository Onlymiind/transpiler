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
#include "util/error_handler.h"
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
        std::vector<types::SymbolID> params;
        types::TypeID return_type = types::k_none_type;
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

    class Scope {
    public:
        Scope(types::ScopeID parent, types::ScopeID this_id, util::ErrorHandler& err)
            : parent_(parent), err_(err)
        {}

        types::SymbolID add_symbol(Symbol sym);
        Symbol& get_symbol(types::SymbolID id);
        Symbol* get_symbol(util::StringConstRef name);

        types::ScopeID get_parent() const { return parent_; }
    private:
        std::unordered_map<util::StringConstRef, types::SymbolID> name_to_symbol_;
        std::vector<Symbol> symbols_;
        types::ScopeID parent_ = types::k_invalid_scope;
        util::ErrorHandler& err_;
    };

    class Module {
    public:
        Module(util::ErrorHandler& err)
            : err_(err)
        {}

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
        types::ScopeID current_scope_ = types::k_global_scope;
        std::vector<Scope> scopes_;
        std::unordered_map<util::StringConstRef, std::list<types::SymbolID>> sym_table_;
        std::unordered_map<types::ScopeID, std::unordered_set<types::ScopeID>> scope_to_parents_;
        util::ErrorHandler& err_;
    };

}
