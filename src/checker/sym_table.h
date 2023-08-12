#pragma once
#include <cstddef>
#include <cstdint>
#include <limits>
#include <unordered_map>
#include <vector>
#include <list>

#include "checker/traits.h"
#include "types/operators.h"
#include "util/arena.h"
#include "util/error_handler.h"
#include "util/util.h"
#include "util/variant.h"
#include "checker/statement.h"


namespace checker {

    struct TypeInfo {
        util::StringConstRef name = nullptr;
        size_t size = 0;
        size_t pos = 0;
    };

    struct Field {
        util::StringConstRef name;
        size_t pos = 0;
        TypeID type = k_undefined_type;
        checker::Expression* default_value = nullptr;
    };

    struct Function {
        std::vector<SymbolID> params;
        TypeID return_type = k_none_type;
        checker::Block* body = nullptr;
    };

    struct FunctionType {
        std::vector<TypeID> params;
        TypeID return_type = k_undefined_type;
    };

    struct Struct {
        std::vector<Field> fields;
    };

    struct TupleOrUnion {
        std::vector<TypeID> types;
        bool is_union = false;
    };

    struct Alias {
        TypeID underlying_type = k_undefined_type;
    };

    using Type = util::Variant<FunctionType, Struct, TupleOrUnion, Alias>;
    
    struct Variable {
        TypeID type = k_undefined_type;
        checker::Expression* default_value = nullptr;
    };

    struct Symbol {
        util::StringConstRef name = nullptr;
        size_t pos = 0;
        size_t size = 0;
        util::Variant<Function, Type, Variable> info;
        TypeTraits traits = TypeTraits::NONE;
        bool poisoned = false;
    };

    class Scope {
    public:
        Scope(ScopeID parent, ScopeID this_id, util::ErrorHandler& err)
            : parent_(parent), err_(err)
        {}

        SymbolID add_symbol(Symbol sym);
        Symbol& get_symbol(SymbolID id);
        Symbol* get_symbol(util::StringConstRef name);

        ScopeID get_parent() const { return parent_; }
    private:
        std::unordered_map<util::StringConstRef, SymbolID> name_to_symbol_;
        std::vector<Symbol> symbols_;
        ScopeID parent_ = k_invalid_scope;
        util::ErrorHandler& err_;
    };

    class Module {
    public:
        Module(util::ErrorHandler& err)
            : err_(err)
        {}

        Symbol& get_symbol_by_name(util::StringConstRef name, ScopeID scope = k_global_scope);
        TypeID get_type_id_by_name(util::StringConstRef name, ScopeID scope = k_global_scope);

        Symbol& get_symbol(SymbolID id);
        TypeTraits get_traits(TypeID id);
        //Type& get_type(SymbolID id);

        bool is_type(util::StringConstRef name, ScopeID scope = k_global_scope);
        bool is_function(util::StringConstRef name, ScopeID scope = k_global_scope);
        bool is_variable(util::StringConstRef name, ScopeID scope = k_global_scope);

        ScopeID push_scope();
        void pop_scope();
        ScopeID get_current_scope_id() const { return current_scope_; }

        Symbol& push_symbol();
        void pop_symbol();
        SymbolID get_current_symbol_id() const;

        SymbolID add_symbol_to_current_scope(Symbol symbol);

    private:
        //TODO: imports
        ScopeID current_scope_ = k_global_scope;
        std::vector<Scope> scopes_;
        std::unordered_map<util::StringConstRef, std::list<SymbolID>> sym_table_;
        std::unordered_map<ScopeID, std::unordered_set<ScopeID>> scope_to_parents_;
        util::ErrorHandler& err_;
    };

}
