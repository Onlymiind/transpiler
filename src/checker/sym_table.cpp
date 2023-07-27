#include "sym_table.h"
#include "parser/parser.h"
#include "util/error_handler.h"

namespace checker {



    Module_ check_types(parser::File file, util::ErrorHandler err) {
        Module_ result;
        
        for(auto& [name, decl] : file.types) {

        }

        for(auto& var : file.variables) {

        }

    }

}
