#include "sym_table.h"
#include "parser/declaration.h"
#include "parser/parser.h"
#include "types/ids.h"
#include "util/arena.h"
#include "util/error_handler.h"
#include <stdexcept>
#include <vector>

namespace checker {

    types::SymbolID Scope::add_symbol(Symbol sym) {
        types::SymbolID id = types::SymbolID{.id = int32_t(symbols_.size())};
        auto name = sym.name;
        symbols_.emplace_back(std::move(sym));
        name_to_symbol_[name] = id;
        return id;
    }

    Symbol& Scope::get_symbol(types::SymbolID id) {
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
}
