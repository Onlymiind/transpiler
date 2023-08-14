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
        if(name_to_symbol_.contains(sym.name)) {
            return k_invalid_symbol;
        }

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

    SymbolID Scope::get_symbol(util::StringConstRef name) {
        auto it = name_to_symbol_.find(name);
        if(it == name_to_symbol_.end()) {
            return k_invalid_symbol;
        }
        return it->second;
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

    TypeID Module::get_symbol_type(SymbolID id) {
        if(id == k_invalid_symbol) {
            return k_invalid_type;
        }

        Symbol& sym = get_symbol(id);
        if(sym.info.is<Variable>()) {
            return sym.info.get<Variable>().type;
        } else {
            throw std::invalid_argument("get_symbol_type is implemented only for variables");
        }
    }

    Symbol& Module::get_symbol(SymbolID id) {
        if(id == k_invalid_symbol) {
            throw std::invalid_argument("invalid symbol id");
        }

        if(id.module != id_) {
            return module_manager_.get_module(id.module).get_symbol(id);
        }

        return scopes_[id.scope.value()].get_symbol(id);
    }

    TypeTraits Module::get_traits(TypeID id) {
        return get_symbol(SymbolID(id)).traits;
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
        if(idx > std::numeric_limits<ScopeID::Underlying>::max()) {
            throw std::runtime_error("too many scopes, shouldn't have used int for scope ids");
        }

        ScopeID id = ScopeID(ScopeID::Underlying(idx));
        scopes_.emplace_back(Scope{current_scope_});
        scope_to_parents_[id] = scope_to_parents_[current_scope_];
        scope_to_parents_[id].emplace(current_scope_);
        current_scope_ = id;
        return id;
    }

    void Module::pop_scope() {
        current_scope_ = scopes_[current_scope_.value()].get_parent();
    }

    Symbol& Module::push_symbol() {
        SymbolID id = add_symbol_to_current_scope(Symbol{});
        symbol_stack_.push(id);
        return get_symbol(id);
    }

    void Module::pop_symbol() {
        if(!symbol_stack_.empty()) {
            symbol_stack_.pop();
        }
    }

    SymbolID Module::get_current_symbol_id() const {
        if(symbol_stack_.empty()) {
            return k_invalid_symbol;
        }

        return symbol_stack_.top();
    }

    SymbolID Module::add_symbol_to_current_scope(Symbol symbol) {
        SymbolID id = scopes_[current_scope_.value()].add_symbol(std::move(symbol));
        if(id == k_invalid_symbol) {
            err_.redeclaration_error(symbol.pos, get_symbol(get_symbol_id_by_name(symbol.name)).pos);
            return id;
        }

        id.scope = current_scope_;
        sym_table_[symbol.name].push_back(id);
        return id;
    }

    void Module::add_foreign_symbol(util::StringConstRef name, SymbolID symbol) {
        if(symbol.scope != k_global_scope) {
            throw std::invalid_argument("can't add foreign symbols not from global scope");
        } else if(scopes_[k_global_scope.value()].get_symbol(name) != k_invalid_symbol) {
            throw std::runtime_error("always process foreign symbols before the rest of the module");
        }

        sym_table_[name].push_back(symbol);
    }

    void Module::add_module(util::StringConstRef name, ModuleID mod) {
        name_to_module_[name] = mod;
    }

    void Module::add_globals_from_module(ModuleID mod) {
        const auto& globals = module_manager_.get_module(mod).get_globals();
        for(auto [name, id] : globals) {
            add_foreign_symbol(name, id);
        }
    }
}
