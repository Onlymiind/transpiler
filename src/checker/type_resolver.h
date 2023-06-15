#pragma once
#include <vector>

#include "parser/declaration.h"
#include "parser/parser.h"
#include "checker/module.h"
#include "util/error_handler.h"


namespace type_resolver {
    std::string make_name(const parser::Declaration& decl);

    module::Module resolve_types(parser::File file, std::vector<module::TypeInfo> predefined_types, util::ErrorHandler& err);
}
