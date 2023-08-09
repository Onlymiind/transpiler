#include "checker/type_checker.h"

namespace checker {
    Block* TypeChecker::check_block(const parser::Block* block, types::ScopeID scope) {
        if(!block) {
            return nullptr;
        }
        
        Block* result = arena_.allocate<Block>();
        for(const auto& smt : block->statements) {
            result->statements.push_back(check_statement(smt));
        }
        return result;
    }

    Expression* TypeChecker::check_expression(const parser::Expression* expr, types::ScopeID scope) {

    }

    Statement TypeChecker::check_statement(const parser::Statement& smt) {
        if(smt.is<parser::Expression*>()) {
            return check_expression(smt.get<parser::Expression*>());
        }
        else if(smt.is<parser::Return>()) {
            check_expression(smt.get<parser::Return>().value);
            auto& sym = mod_.get_symbol(mod_.get_current_symbol_id());

        }
        else if(smt.is<parser::IfStatement*>());
        else if(smt.is<parser::Assignment>());
        else if(smt.is<parser::Loop>());
    }

    types::SymbolID TypeChecker::check_and_add_function(const parser::Function& func) {
        auto& func_sym = mod_.push_symbol();
        func_sym.name = func.name;
        func_sym.pos = func.pos;
        func_sym.info = Function{};
        Function& info = func_sym.info.get<Function>();
        if(func.type.return_type)
            info.return_type = mod_.get_type_id_by_name(func.type.return_type);

        info.params.reserve(func.type.params.size());
        types::ScopeID parent_scope = mod_.get_current_scope_id();
        types::ScopeID scope_id = mod_.push_scope();

        for(const auto& param : func.type.params) {
            info.params.emplace_back(mod_.add_symbol_to_current_scope(Symbol{
                .name = param.name,
                .pos = param.pos,
                .info = Variable{
                    .type = mod_.get_type_id_by_name(param.type),
                    .default_value = check_expression(param.value, parent_scope),
                },
            }));
        }

        info.body = check_block(func.body, scope_id);

        mod_.pop_scope();
        types::SymbolID func_id = mod_.get_current_symbol_id();
        mod_.pop_symbol();
        return func_id;
    }


    void TypeChecker::define_globals() {
        auto preprocessed = define_globals_first_pass();

#define DEFINE_OR_POISON(symbol, ...) try { __VA_ARGS__; } catch(const util::CheckerError&) { symbol.poisoned = true; }
        for(auto [id, parsed] : preprocessed) {
            auto& sym = mod_.get_symbol(id);
            auto& def = sym.info.get<Type>();
            if(parsed->decl.is<parser::Struct>()) {
                DEFINE_OR_POISON(sym, define_struct(def.get<Struct>(), parsed->decl.get<parser::Struct>()));
            } else if(parsed->decl.is<parser::FunctionType>()) {
                DEFINE_OR_POISON(sym, define_function_type(def.get<FunctionType>(), parsed->decl.get<parser::FunctionType>()));
            } else if(parsed->decl.is<parser::Alias>()) {
                DEFINE_OR_POISON(sym, def.get<Alias>().underlying_type = mod_.get_type_id_by_name(parsed->decl.get<parser::Alias>().underlying_type));
            } else if(parsed->decl.is<parser::TupleOrUnion>()) {
                DEFINE_OR_POISON(sym, def.get<TupleOrUnion>(), parsed->decl.get<parser::TupleOrUnion>());
            }
        }
#undef DEFINE_OR_POISON

        for (const auto& [name, func] : file_.functions) {
            check_and_add_function(func);
        }

        for(auto& var : file_.global_variables) {
            //TODO: global variables
        }
    }

    std::vector<std::pair<types::SymbolID, const parser::Decl*>> TypeChecker::define_globals_first_pass() {
        std::vector<std::pair<types::SymbolID, const parser::Decl*>> result;
        result.reserve(file_.types.size());

        for(const auto& [name, decl] : file_.types) {
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
            result.emplace_back(mod_.add_symbol_to_current_scope(std::move(sym)), &decl);
        }
        return result;
    }

    void TypeChecker::define_struct(Struct& info, const parser::Struct& parsed) {
        for(const auto& field : parsed.fields) {
            info.fields.push_back(Field{
                .name = field.name,
                .pos = field.pos,
                .type = mod_.get_type_id_by_name(field.type),
            });
        }
    }

    void TypeChecker::define_function_type(FunctionType& info, const parser::FunctionType& parsed) {
        info.return_type = mod_.get_type_id_by_name(parsed.return_type);
        for(const auto& param : parsed.params) {
            info.params.push_back(mod_.get_type_id_by_name(param.type));
        }
    }

    void TypeChecker::define_tuple_or_union(TupleOrUnion& info, const parser::TupleOrUnion& parsed) {
        for(auto typname : parsed.types) {
            info.types.push_back(mod_.get_type_id_by_name(typname));
        }
    }
}
