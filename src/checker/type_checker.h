#pragma once
#include "checker/traits.h"
#include "parser/declaration.h"
#include "parser/statement.h"
#include "types/operators.h"
#include "util/arena.h"
#include "util/error_handler.h"
#include "checker/sym_table.h"
#include "checker/statement.h"
#include "parser/parser.h"
#include "util/variant.h"

namespace checker {

    class TypeChecker {
    public:
        TypeChecker(util::ErrorHandler& err, parser::File&& file)
            : mod_(err), file_(std::move(file)), err_(err)
        {}

        Block* check_block(const parser::Block* block, ScopeID scope);
        Statement check_statement(const parser::Statement& smt);
        IfStatement* check_if(const parser::IfStatement* smt);
        Expression* check_expression(const parser::Expression* expr, ScopeID scope = k_invalid_scope);
        Assignment check_assignment(const parser::Assignment& assignment, ScopeID scope = k_invalid_scope);
        Loop check_loop(const parser::Loop& loop, ScopeID scope = k_invalid_scope);
        Return check_return(const parser::Return& ret);
        std::pair<util::Variant<TypeCast, FunctionCall>, TypeID> check_function_call(const parser::FunctionCall& call, ScopeID scope = k_invalid_scope);

        SymbolID check_and_add_function(const parser::Function& func);
        SymbolID check_and_add_variable(const parser::Assignment& var, ScopeID scope = k_invalid_scope);


        void define_globals();
        std::vector<std::pair<SymbolID, const parser::Decl*>> define_globals_first_pass();
        void define_struct(Struct& info, const parser::Struct& parsed);
        void define_function_type(FunctionType& info, const parser::FunctionType& parsed);
        void define_tuple_or_union(TupleOrUnion& info, const parser::TupleOrUnion& parsed);

        bool is_binop_legal(TypeID lhs, TypeID rhs, types::Operation op);
        TypeID get_binop_result(TypeID lhs, TypeID rhs, types::Operation op);
        bool is_unop_legal(TypeID type, types::Operation op);
        TypeID get_unop_result(TypeID type, types::Operation op);
        bool is_cast_legal(TypeID src, TypeID dst);
        bool are_types_compatible(TypeID lhs, TypeID rhs);


    private:
        Module mod_;
        util::ArenaPool<Block, Expression, IfStatement> arena_;
        parser::File file_;
        util::ErrorHandler& err_;
    };
}
