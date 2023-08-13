#include "sym_table.h"
#include "checker/traits.h"
#include "parser/declaration.h"
#include "parser/parser.h"
#include "util/arena.h"
#include "util/error_handler.h"
#include <limits>
#include <stdexcept>
#include <vector>

namespace checker {

    SymbolID Scope::add_symbol(Symbol sym) {
        SymbolID id = SymbolID{.id = int32_t(symbols_.size())};
        auto name = sym.name;
        symbols_.emplace_back(std::move(sym));
        name_to_symbol_[name] = id;
        return id;
    }

    Symbol& Scope::get_symbol(SymbolID id) {
        if(id.id >= symbols_.size()) {
            throw std::runtime_error("symbol id out of bounds");
        }
        return symbols_[id.id];
    }

    Symbol* Scope::get_symbol(util::StringConstRef name) {
        auto it = name_to_symbol_.find(name);
        if(it != name_to_symbol_.end()) {
            return &symbols_[it->second.id];
        }
        return nullptr;
    }

    SymbolID Module::get_symbol_id_by_name(util::StringConstRef name, ScopeID scope) {
        if(scope == k_invalid_scope) {
            return k_invalid_symbol;
        }
        auto it = sym_table_.find(name);
        if(it == sym_table_.end()) {
            return k_invalid_symbol;
        }

        auto& list = it->second;
        for(auto id : list) {
            if(scope == id.scope || scope_to_parents_[scope].contains(id.scope)) {
                return id;
            }
        }

        return k_invalid_symbol;
    }

    TypeID Module::get_type_id_by_name(util::StringConstRef name, ScopeID scope) {
        SymbolID id = get_symbol_id_by_name(name, scope);
        if(id == k_invalid_symbol) {
            return k_invalid_type;
        }

        return get_symbol(id).info.is<Type>() ? TypeID(id) : k_invalid_type;
    }

    TypeID Module::get_symbol_type(SymbolID sym) {

    }

    TypeID Module::get_type_for_constant(types::Token constant) {

    }

    Symbol& Module::get_symbol(SymbolID id) {
        if(id == k_invalid_symbol) {
            throw std::invalid_argument("invalid symbol id");
        }

        return scopes_[int(id.scope)].get_symbol(id);
    }

    TypeTraits Module::get_traits(TypeID id) {

    }

    bool Module::is_type(util::StringConstRef name, ScopeID scope) {
        SymbolID id = get_symbol_id_by_name(name, scope);
        if(id == k_invalid_symbol) {
            return false;
        }
        return get_symbol(id).info.is<Type>();
    }

    bool Module::is_function(util::StringConstRef name, ScopeID scope) {
        SymbolID id = get_symbol_id_by_name(name, scope);
        if(id == k_invalid_symbol) {
            return false;
        }
        return get_symbol(id).info.is<Function>();
    }

    bool Module::is_variable(util::StringConstRef name, ScopeID scope) {
        SymbolID id = get_symbol_id_by_name(name, scope);
        if(id == k_invalid_symbol) {
            return false;
        }
        return get_symbol(id).info.is<Variable>();
    }

    ScopeID Module::push_scope() {
        size_t idx = scopes_.size();
        if(idx > std::numeric_limits<int>::max()) {
            throw std::runtime_error("too many scopes, shouldn't have used int for scope ids");
        }

        ScopeID id = ScopeID(int(idx));
        scopes_.emplace_back(Scope{current_scope_});
        scope_to_parents_[id] = scope_to_parents_[current_scope_];
        scope_to_parents_[id].emplace(current_scope_);
        current_scope_ = id;
        return id;
    }

    void Module::pop_scope() {
        current_scope_ = scopes_[int(current_scope_)].get_parent();
    }

    Symbol& Module::push_symbol() {

    }

    void Module::pop_symbol() {

    }

    SymbolID Module::get_current_symbol_id() const {

    }

    SymbolID Module::add_symbol_to_current_scope(Symbol symbol) {
        SymbolID id = scopes_[int(current_scope_)].add_symbol(std::move(symbol));
        id.scope = current_scope_;
        sym_table_[symbol.name].push_back(id);
        return id;
    }
}
