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

    std::vector<std::pair<types::SymbolID, const parser::Decl*>> first_pass(Module& mod, const parser::File& file) {
        std::vector<std::pair<types::SymbolID, const parser::Decl*>> result;
        result.reserve(file.types.size());

        for(const auto& [name, decl] : file.types) {
            Symbol sym{.name = name, .pos = decl.pos};
            if(decl.decl.is<parser::Struct>()) {
                Struct info;
                info.fields.reserve(decl.decl.get<parser::Struct>().fields.size());
                sym.info = std::move(info);
            } else if(decl.decl.is<parser::Alias>()) {
                sym.info = Alias{};
            } else if(decl.decl.is<parser::TupleOrUnion>()) {
                auto& parsed_info = decl.decl.get<parser::TupleOrUnion>();
                TupleOrUnion info{.is_union = parsed_info.is_union};
                info.types.reserve(parsed_info.types.size());
                sym.info = std::move(info);
            } else if(decl.decl.is<parser::FunctionType>()) {
                FunctionType info;
                info.params.reserve(decl.decl.get<parser::FunctionType>().params.size());
                sym.info = std::move(info);
            }
            result.emplace_back(mod.add_symbol_to_current_scope(std::move(sym)), &decl);
        }
        return result;
    }

    void define_struct(Module& mod, Struct& info, const parser::Struct& parsed) {
        for(const auto& field : parsed.fields) {
            info.fields.push_back(Field{
                .name = field.name,
                .pos = field.pos,
                .type = mod.get_type_id_by_name(field.type.get<util::StringConstRef>()),
            });
        }
    }

    void define_function_type(Module& mod, FunctionType& info, const parser::FunctionType& parsed) {
        info.return_type = mod.get_type_id_by_name(parsed.return_type);
        for(const auto& param : parsed.params) {
            info.params.push_back(mod.get_type_id_by_name(param.type.get<util::StringConstRef>()));
        }
    }

    void define_tuple_or_union(Module& mod, TupleOrUnion& info, const parser::TupleOrUnion& parsed) {
        for(auto typname : parsed.types) {
            info.types.push_back(mod.get_type_id_by_name(typname));
        }
    }

    Module define_global_types(parser::File file, util::ErrorHandler& err) {
        Module result{err};
        
        auto preprocessed = first_pass(result, file);

#define DEFINE_OR_POISON(symbol, ...) try { __VA_ARGS__; } catch(const util::CheckerError&) { symbol.poisoned = true; }
        for(auto [id, parsed] : preprocessed) {
            auto& sym = result.get_symbol(id);
            auto& def = sym.info.get<Type>();
            if(parsed->decl.is<parser::Struct>()) {
                DEFINE_OR_POISON(sym, define_struct(result, def.get<Struct>(), parsed->decl.get<parser::Struct>()));
            } else if(parsed->decl.is<parser::FunctionType>()) {
                DEFINE_OR_POISON(sym, define_function_type(result, def.get<FunctionType>(), parsed->decl.get<parser::FunctionType>()));
            } else if(parsed->decl.is<parser::Alias>()) {
                DEFINE_OR_POISON(sym, def.get<Alias>().underlying_type = result.get_type_id_by_name(parsed->decl.get<parser::Alias>().underlying_type));
            } else if(parsed->decl.is<parser::TupleOrUnion>()) {
                DEFINE_OR_POISON(sym, def.get<TupleOrUnion>(), parsed->decl.get<parser::TupleOrUnion>());
            }
        }
#undef DEFINE_OR_POISON

        for(auto& var : file.global_variables) {

        }

        return result;
    }

    

}
