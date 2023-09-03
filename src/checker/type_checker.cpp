#include "checker/type_checker.h"
#include "checker/statement.h"
#include "checker/sym_table.h"
#include "checker/traits.h"
#include "parser/statement.h"
#include "types/operators.h"
#include "types/token.h"
#include "util/arena.h"
#include "util/error_handler.h"
#include <stdexcept>

namespace checker {

#define TRY_OR_POISON(poisoned, ...) try { __VA_ARGS__; } catch(const util::CheckerError) { poisoned = true; }

    SymbolID TypeChecker::check_and_add_variable(const parser::Assignment& parsed_var, ScopeID scope) {
        if(scope == k_invalid_scope) {
            scope = mod_.get_current_scope_id();
        }

        Variable var;

        var.default_value = check_expression(parsed_var.value, scope);

        if(parsed_var.type) {
            var.type = mod_.get_type_id_by_name(parsed_var.type, scope);
            if(var.default_value && !are_types_compatible(var.type, var.default_value->type)) {
                err_.checker_error(var.default_value->pos, "cannot convert bewteen two types");
            }
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
            return Assignment{};
        }

        Symbol& sym = mod_.get_symbol(id);
        if(!sym.info.is<Variable>()) {
            err_.checker_error(assignment.pos, "cannot assign to symbol that is not a variable");
            return Assignment{};
        }

        result.var = id;
        result.value = check_expression(assignment.value);
        //TODO
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

    std::pair<util::Variant<TypeCast, FunctionCall>, TypeID> TypeChecker::check_function_call(const parser::FunctionCall& call, size_t pos, ScopeID scope) {
        if(scope == k_invalid_scope) {
            scope = mod_.get_current_scope_id();
        }
        auto err_return = [this, &call, scope](SymbolID sym) {
            FunctionCall result {.func = sym };
            result.args.reserve(call.args.size());
            for(auto expr : call.args) {
                result.args.push_back(check_expression(expr, scope));
            }
            return std::pair{std::move(result), k_invalid_type};
        };


        SymbolID sym_id = mod_.get_symbol_id_by_name(call.func, scope);
        if(sym_id == k_invalid_symbol) {
            return err_return(sym_id);
        }

        Symbol& sym = mod_.get_symbol(sym_id);
        if(sym.info.is<Type>()) {
            if(call.args.size() != 1) {
                err_.checker_error(pos, "expected exactly one argument for a type cast");
                return err_return(k_invalid_symbol);
            }
            TypeCast result{.expr = check_expression(call.args[0])};
            if(!result.expr) {
                result.dst_type = k_invalid_type;
                //can this even happen?
                err_.checker_error(0, "expected expression");
                return {result, result.dst_type};
            }

            result.dst_type = get_cast_result(result.expr->type, TypeID(sym_id));
            if(result.dst_type == k_invalid_type) {
                err_.checker_error(result.expr->pos, "invalid cast");
            }

            return std::pair{result, result.dst_type};
        } else if(sym.info.is<Function>()) {
            auto& func = sym.info.get<Function>();
            FunctionCall result{.func = sym_id};
            if(call.args.size() != func.type.params.size()) {
                err_.checker_error(pos, "wrong number of arguments for fucntion call");
                return std::pair{result, func.type.return_type};
            }
            result.args.reserve(call.args.size());
            for(size_t i = 0; i < call.args.size(); ++i) {
                auto& arg = call.args[i];
                result.args.push_back(check_expression(arg, scope));
                //TODO: can expression in call.args even be a nullptr?
                if(!are_types_compatible(result.args.back()->type, func.type.params[i])) {
                    err_.checker_error(result.args.back()->pos, "function argument type mismatch");
                }
            }
            return std::pair{std::move(result), func.type.return_type};
        } else {
            return err_return(k_invalid_symbol);
        }
    }

    Expression* TypeChecker::check_expression(const parser::Expression* expr, ScopeID scope) {
        if(!expr) {
            return nullptr;
        }

        if(scope == k_invalid_scope) {
            scope = mod_.get_current_scope_id();
        }

        Expression* result = arena_.allocate<Expression>(Expression{.pos = expr->pos});
        if(expr->expr.is<parser::BinaryExpression>()) {
            auto& parsed = expr->expr.get<parser::BinaryExpression>();
            auto bin_expr = BinaryExpression{
                .lhs = check_expression(parsed.lhs, scope),
                .rhs = check_expression(parsed.rhs, scope),
                .op = parsed.op
            };

            if(bin_expr.rhs && bin_expr.lhs) {
                result->type = get_binop_result(bin_expr.lhs->type, bin_expr.rhs->type, bin_expr.op);
                if(result->type == k_invalid_type) {
                    err_.checker_error(result->pos, "invalid types of arguments");
                }
            }
            result->expr = bin_expr;
        } else if(expr->expr.is<parser::UnaryExpression>()) {
            auto& parsed = expr->expr.get<parser::UnaryExpression>();
            auto un_expr = UnaryExpression{
                .expr = check_expression(parsed.expr, scope),
                .op = parsed.op
            };

            if(un_expr.expr) {
                result->type = get_unop_result(un_expr.expr->type, un_expr.op);
                if(result->type == k_invalid_type) {
                    err_.checker_error(result->pos, "invalid argument type");
                }
            }
            result->expr = un_expr;
        } else if(expr->expr.is<util::StringConstRef>()) {
            SymbolID sym = mod_.get_symbol_id_by_name(expr->expr.get<util::StringConstRef>(), scope);
            result->type = mod_.get_symbol_type(sym);
            result->expr = sym;
        } else if(expr->expr.is<parser::FunctionCall>()) {
            auto [call, type] = check_function_call(expr->expr.get<parser::FunctionCall>(), expr->pos, scope);
            result->type = type;
            call.visit([&result](auto val) { result->expr = std::move(val); });
        } else if(expr->expr.is<types::Token>()) {
            types::Token constant = expr->expr.get<types::Token>();
            result->type = get_type_for_constant(constant);
            constant.value.visit([&result](auto val){
                if constexpr(std::is_same_v<util::StringConstRef, decltype(val)>) {
                    result->expr = val;
                } else if constexpr(std::is_same_v<uint64_t, decltype(val)>) {
                    result->expr = val;
                } else if constexpr(std::is_same_v<double, decltype(val)>) {
                    result->expr = val;
                }
            });
        }


        return result;
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
        result.body = check_block(loop.body, new_scope);
        mod_.pop_scope();

        return result;
    }

    Return TypeChecker::check_return(const parser::Return& ret) {
        Return result;
        result.value = check_expression(ret.value);
        SymbolID id = mod_.get_current_symbol_id();
        if(id == k_invalid_symbol) {
            err_.checker_error(ret.pos, "return statement not in function");
        }

        const Symbol& func = mod_.get_symbol(id);
        if(!func.info.is<Function>()) {
            err_.checker_error(ret.pos, "return statement not in function");
        }

        TypeID return_type = func.info.get<Function>().type.return_type;
        if(!result.value) {
            if(return_type != k_none_type) {
                err_.checker_error(ret.pos, "cannot return void in function with return type specified");
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
            info.type.return_type = mod_.get_type_id_by_name(func.type.return_type);

        info.type.params.reserve(func.type.params.size());
        ScopeID parent_scope = mod_.get_current_scope_id();
        ScopeID scope_id = mod_.push_scope();
        for(const auto& param : func.type.params) {
            info.type.params.emplace_back(mod_.get_symbol_type(check_and_add_variable(param, parent_scope)));
        }

        info.body = check_block(func.body, scope_id);

        mod_.pop_scope();
        SymbolID func_id = mod_.get_current_symbol_id();
        mod_.pop_symbol();
        return func_id;
    }


    void TypeChecker::define_globals() {
        auto preprocessed = define_globals_first_pass();

        for(auto [id, parsed] : preprocessed) {
            if(id == k_invalid_symbol) {
                //redeclaration
                continue;
            }

            auto& sym = mod_.get_symbol(id);
            auto& def = sym.info.get<Type>();
            if(parsed->decl.is<parser::Struct>()) {
                define_struct(def.get<Struct>(), parsed->decl.get<parser::Struct>());
            } else if(parsed->decl.is<parser::FunctionType>()) {
                define_function_type(def.get<FunctionType>(), parsed->decl.get<parser::FunctionType>());
            } else if(parsed->decl.is<parser::Alias>()) {
                def.get<Alias>().underlying_type = mod_.get_type_id_by_name(parsed->decl.get<parser::Alias>().underlying_type);
            } else if(parsed->decl.is<parser::TupleOrUnion>()) {
                define_tuple_or_union(def.get<TupleOrUnion>(), parsed->decl.get<parser::TupleOrUnion>());
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

    TypeID TypeChecker::get_binop_result(TypeID lhs, TypeID rhs, types::Operation op) {
        if(lhs != rhs) {
            return k_invalid_type;
        }
        TypeTraits traits = mod_.get_traits(lhs);

        using op_t = types::Operation;
        switch(op) {
        case op_t::LESS:
        case op_t::LESS_EQUALS:
        case op_t::GREATER:
        case op_t::GREATER_EQUALS:
            if(traits != TypeTraits::INTEGRAL && traits != TypeTraits::FLOATING_POINT) {
                return k_invalid_type;
            }
        case op_t::NOT_EQUALS:
        case op_t::EQUALS:
            return mod_.get_type_id_by_name(alloc_.allocate("bool"));
        case op_t::BAND:
        case op_t::BOR:
        case op_t::XOR:
        case op_t::LSHIFT:
        case op_t::RSHIFT:
            return traits == TypeTraits::INTEGRAL ? lhs : k_invalid_type;
        default:
            if(traits != TypeTraits::INTEGRAL && traits != TypeTraits::FLOATING_POINT) {
                return k_invalid_type;
            }
            return lhs;
        }
    }

    TypeID TypeChecker::get_unop_result(TypeID type, types::Operation op) {
        using op_t = types::Operation;
        TypeTraits traits = mod_.get_traits(type);
        switch(op) {
        case op_t::DEREF:
            return get_dereference_result_type(type, traits);
        case op_t::NOT:
            return traits == TypeTraits::BOOLEAN ? type : k_invalid_type;
        case op_t::INV:
            return traits == TypeTraits::INTEGRAL ? type : k_invalid_type;
        default:
            return (traits == TypeTraits::INTEGRAL || traits == TypeTraits::FLOATING_POINT) ? type : k_invalid_type;
        }
    }

    TypeID TypeChecker::get_dereference_result_type(TypeID type, TypeTraits traits) {
        throw std::runtime_error("not implemented");
        if(traits != TypeTraits::DEREFERENCABLE) {
            return k_invalid_type;
        }

        Symbol& info = mod_.get_symbol(SymbolID(type));
        //TODO
    }

    TypeID TypeChecker::get_cast_result(TypeID src, TypeID dst) {
        if(src == dst) {
            return dst;
        }
    }

    bool TypeChecker::are_types_compatible(TypeID lhs, TypeID rhs) {

    }

    TypeID TypeChecker::get_type_for_constant(types::Token constant) {

    }
}
