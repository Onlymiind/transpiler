#pragma once
#include <cstddef>
#include <cstdint>
#include <limits>
#include <unordered_map>
#include <vector>
#include <list>
#include <stack>

#include "checker/traits.h"
#include "types/operators.h"
#include "types/token.h"
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
        Expression* default_value = nullptr;
    };

    struct FunctionType {
        std::vector<TypeID> params;
        TypeID return_type = k_undefined_type;
    };

    struct Function {
        FunctionType type;
        Block* body = nullptr;
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
        Expression* default_value = nullptr;
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
        Scope(ScopeID parent)
            : parent_(parent)
        {}

        SymbolID add_symbol(Symbol sym);
        Symbol& get_symbol(SymbolID id);
        Symbol* get_symbol(util::StringConstRef name);

        ScopeID get_parent() const { return parent_; }
    private:
        std::unordered_map<util::StringConstRef, SymbolID> name_to_symbol_;
        std::vector<Symbol> symbols_;
        ScopeID parent_ = k_invalid_scope;
    };

    class Module {
    public:
        Module(util::ErrorHandler& err)
            : err_(err)
        {}

        SymbolID get_symbol_id_by_name(util::StringConstRef name, ScopeID scope = k_global_scope);
        TypeID get_type_id_by_name(util::StringConstRef name, ScopeID scope = k_global_scope);

        TypeID get_symbol_type(SymbolID id);

        Symbol& get_symbol(SymbolID id);
        TypeTraits get_traits(TypeID id);

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
        std::stack<SymbolID> symbol_stack_;
        std::vector<Scope> scopes_;
        std::unordered_map<util::StringConstRef, std::list<SymbolID>> sym_table_;
        std::unordered_map<ScopeID, std::unordered_set<ScopeID>> scope_to_parents_;
        util::ErrorHandler& err_;
    };

}
