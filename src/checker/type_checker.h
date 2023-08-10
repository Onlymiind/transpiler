#pragma once
#include "parser/declaration.h"
#include "parser/statement.h"
#include "types/operators.h"
#include "util/arena.h"
#include "util/error_handler.h"
#include "checker/sym_table.h"
#include "checker/statement.h"
#include "parser/parser.h"

namespace checker {
    inline TypeID k_bool = TypeID(SymbolID{k_global_scope, 1});

    class TypeChecker {
    public:
        TypeChecker(util::ErrorHandler& err, parser::File&& file)
            : mod_(err), file_(std::move(file)), err_(err)
        {}

        Block* check_block(const parser::Block* block, ScopeID scope);
        Statement check_statement(const parser::Statement& smt);
        IfStatement* check_if(const parser::IfStatement* smt);
        Expression* check_expression(const parser::Expression* expr, ScopeID scope = k_invalid_scope);
        SymbolID check_and_add_function(const parser::Function& func);


        void define_globals();
        std::vector<std::pair<SymbolID, const parser::Decl*>> define_globals_first_pass();
        void define_struct(Struct& info, const parser::Struct& parsed);
        void define_function_type(FunctionType& info, const parser::FunctionType& parsed);
        void define_tuple_or_union(TupleOrUnion& info, const parser::TupleOrUnion& parsed);

        bool is_binop_legal(TypeID lhs, TypeID rhs, types::Operation op);
        bool is_unop_legal(TypeID type, types::Operation op);
        bool is_cast_legal(TypeID src, TypeID dst);


    private:
        Module mod_;
        util::ArenaPool<Block, Expression, IfStatement> arena_;
        parser::File file_;
        util::ErrorHandler& err_;
    };
}