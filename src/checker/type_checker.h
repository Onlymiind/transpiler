#pragma once
#include "parser/declaration.h"
#include "types/ids.h"
#include "types/operators.h"
#include "util/arena.h"
#include "util/error_handler.h"
#include "checker/sym_table.h"
#include "checker/statement.h"
#include "parser/parser.h"

namespace checker {
    class TypeChecker {
    public:
        TypeChecker(util::ErrorHandler& err, parser::File&& file)
            : mod_(err), file_(std::move(file)), err_(err)
        {}

        Block* check_block(const parser::Block* block, types::ScopeID scope);
        Expression* check_expression(const parser::Expression* expr, types::ScopeID scope = types::k_invalid_scope);
        Statement check_statement(const parser::Statement& smt);
        types::SymbolID check_and_add_function(const parser::Function& func);


        void define_globals();
        std::vector<std::pair<types::SymbolID, const parser::Decl*>> define_globals_first_pass();
        void define_struct(Struct& info, const parser::Struct& parsed);
        void define_function_type(FunctionType& info, const parser::FunctionType& parsed);
        void define_tuple_or_union(TupleOrUnion& info, const parser::TupleOrUnion& parsed);


    private:
        Module mod_;
        util::ArenaPool<Block, Expression, IfStatement> arena_;
        parser::File file_;
        util::ErrorHandler& err_;
    };
}
