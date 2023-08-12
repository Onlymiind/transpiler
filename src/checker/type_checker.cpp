#include "checker/type_checker.h"
#include "checker/statement.h"
#include "checker/sym_table.h"
#include "checker/traits.h"
#include "parser/statement.h"
#include "util/error_handler.h"
#include <stdexcept>

namespace checker {

#define TRY_OR_POISON(poisoned, ...) try { __VA_ARGS__; } catch(const util::CheckerError) { poisoned = true; }

    SymbolID TypeChecker::check_and_add_variable(const parser::Assignment& parsed_var, ScopeID scope) {
        if(scope == k_invalid_scope) {
            scope = mod_.get_current_scope_id();
        }

        Variable var;
        bool poisoned = false;

        TRY_OR_POISON(poisoned, var.default_value = check_expression(parsed_var.value, scope));

        if(parsed_var.type) {
            TRY_OR_POISON(poisoned, 
                var.type = mod_.get_type_id_by_name(parsed_var.type, scope);
                if(var.default_value && !are_types_compatible(var.type, var.default_value->type)) {
                    //TODO: also specify types
                    err_.checker_error(var.default_value->pos, "cannot convert bewteen two types");
                }
            );
        } else if(var.default_value) {
            var.type = var.default_value->type;
        } else {
            err_.checker_error(parsed_var.pos, "declaration of variable of unknown type");
        }

        return mod_.add_symbol_to_current_scope(Symbol{
            .name = parsed_var.name,
            .pos = parsed_var.pos,
            .info = var,
            .traits = mod_.get_traits(var.type),
            .poisoned = poisoned
        });
    }

    Assignment TypeChecker::check_assignment(const parser::Assignment& assignment, ScopeID scope) {
        if(!assignment.value) {
            err_.checker_error(assignment.pos, "empty assignment");
        }
        if(scope == k_invalid_scope) {
            scope = mod_.get_current_scope_id();
        }
        Assignment result;

        SymbolID id = mod_.get_symbol_id_by_name(assignment.name);
        if(id == k_invalid_symbol) {
            err_.checker_error(assignment.pos, "variable not declared: ", assignment.name);
        }

        Symbol& sym = mod_.get_symbol(id);
        if(!sym.info.is<Variable>()) {
            err_.checker_error(assignment.pos, "cannot assign to symbol that is not a variable");
        }

        result.var = id;
        result.value = check_expression(assignment.value);
        if(sym.poisoned) {
            return result;
        }
        if(!are_types_compatible(result.value->type, sym.info.get<Variable>().type)) {
            err_.checker_error(result.value->pos, "cannot convert between two types");
        }
        
        return result;
    }

    Block* TypeChecker::check_block(const parser::Block* block, ScopeID scope) {
        if(!block) {
            return nullptr;
        }
        
        Block* result = arena_.allocate<Block>();
        result->scope = scope;
        for(const auto& smt : block->statements) {
            result->statements.push_back(check_statement(smt));
        }
        return result;
    }

    Expression* TypeChecker::check_expression(const parser::Expression* expr, ScopeID scope) {

    }

    IfStatement* TypeChecker::check_if(const parser::IfStatement* smt) {
        if(!smt) {
            return nullptr;
        }

        IfStatement* result = arena_.allocate<IfStatement>();
        result->condition = check_expression(smt->condition);
        if(!result->condition || mod_.get_traits(result->condition->type) != TypeTraits::BOOLEAN) {
            err_.checker_error(result->condition->pos, "type mismatch, expected boolean type");
        }
        auto scope = mod_.push_scope();
        result->then = check_block(smt->then, scope);
        mod_.pop_scope();
        result->otherwise = check_if(smt->otherwise);
        return result;
    }

    Loop TypeChecker::check_loop(const parser::Loop& loop, ScopeID scope) {
        if(scope == k_invalid_scope) {
            scope = mod_.get_current_scope_id();
        }

        Loop result;
        if(loop.init) {
            result.init = check_assignment(*loop.init, scope);
        }

        result.condition = check_expression(loop.condition, scope);
        if(result.condition && mod_.get_traits(result.condition->type) != TypeTraits::BOOLEAN) {
            err_.checker_error(result.condition->pos, "loop condition is not a boolean expression");
        }
        if(loop.step.is<parser::Assignment>()) {
            result.step = check_assignment(loop.step.get<parser::Assignment>(), scope);
        } else if(loop.step.is<parser::Expression*>()) {
            result.step = check_expression(loop.step.get<parser::Expression*>(), scope);
        }

        ScopeID new_scope = mod_.push_scope();
        try {
            result.body = check_block(loop.body, new_scope);
        } catch(const util::CheckerError&) {}
        mod_.pop_scope();

        return result;
    }

    Return TypeChecker::check_return(const parser::Return& ret) {
        Return result;
        result.value = check_expression(ret.value);
        SymbolID id = mod_.get_current_symbol_id();
        if(id == k_invalid_symbol) {
            err_.checker_error(ret.value ? ret.value->pos : 0, "return statement not in function");
        }

        const Symbol& func = mod_.get_symbol(id);
        if(!func.info.is<Function>()) {
            err_.checker_error(ret.value ? ret.value->pos : 0, "return statement not in function");
        }

        TypeID return_type = func.info.get<Function>().return_type;
        if(!result.value) {
            if(return_type != k_invalid_type) {
                err_.checker_error(0, "cannot return void in funvtion with return type specified");
            }
        } else if(!are_types_compatible(return_type, result.value->type)) {
            err_.checker_error(result.value->pos, "cannot convert between types");
        }

        return result;
    }

    Statement TypeChecker::check_statement(const parser::Statement& smt) {
        if(smt.is<parser::Expression*>()) {
            return check_expression(smt.get<parser::Expression*>());
        } else if(smt.is<parser::Return>()) {
            return check_return(smt.get<parser::Return>());
        } else if(smt.is<parser::IfStatement*>()) {
            return check_if(smt.get<parser::IfStatement*>());
        } else if(smt.is<parser::Assignment>()) {
            return check_assignment(smt.get<parser::Assignment>());
        } else if(smt.is<parser::Loop>()) {
            return check_loop(smt.get<parser::Loop>());
        } else {
            throw std::invalid_argument("statement type not supprted");
        }
    }

    SymbolID TypeChecker::check_and_add_function(const parser::Function& func) {
        auto& func_sym = mod_.push_symbol();
        func_sym.name = func.name;
        func_sym.pos = func.pos;
        func_sym.info = Function{};
        Function& info = func_sym.info.get<Function>();
        if(func.type.return_type)
            info.return_type = mod_.get_type_id_by_name(func.type.return_type);

        info.params.reserve(func.type.params.size());
        ScopeID parent_scope = mod_.get_current_scope_id();
        ScopeID scope_id = mod_.push_scope();
        TRY_OR_POISON(func_sym.poisoned,
            for(const auto& param : func.type.params) {
                info.params.emplace_back(check_and_add_variable(param, parent_scope));
            }

            info.body = check_block(func.body, scope_id);
        );

        mod_.pop_scope();
        SymbolID func_id = mod_.get_current_symbol_id();
        mod_.pop_symbol();
        return func_id;
    }


    void TypeChecker::define_globals() {
        auto preprocessed = define_globals_first_pass();

        for(auto [id, parsed] : preprocessed) {
            auto& sym = mod_.get_symbol(id);
            auto& def = sym.info.get<Type>();
            if(parsed->decl.is<parser::Struct>()) {
                TRY_OR_POISON(sym.poisoned, define_struct(def.get<Struct>(), parsed->decl.get<parser::Struct>()));
            } else if(parsed->decl.is<parser::FunctionType>()) {
                TRY_OR_POISON(sym.poisoned, define_function_type(def.get<FunctionType>(), parsed->decl.get<parser::FunctionType>()));
            } else if(parsed->decl.is<parser::Alias>()) {
                TRY_OR_POISON(sym.poisoned, def.get<Alias>().underlying_type = mod_.get_type_id_by_name(parsed->decl.get<parser::Alias>().underlying_type));
            } else if(parsed->decl.is<parser::TupleOrUnion>()) {
                TRY_OR_POISON(sym.poisoned, def.get<TupleOrUnion>(), parsed->decl.get<parser::TupleOrUnion>());
            }
        }

        for (const auto& [name, func] : file_.functions) {
            check_and_add_function(func);
        }

        for(const auto& var : file_.global_variables) {
            check_and_add_variable(var);
        }
    }

    std::vector<std::pair<SymbolID, const parser::Decl*>> TypeChecker::define_globals_first_pass() {
        std::vector<std::pair<SymbolID, const parser::Decl*>> result;
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
