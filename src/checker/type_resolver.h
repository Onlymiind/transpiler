#pragma once
#include <vector>

#include "parser/declaration.h"
#include "parser/parser.h"
#include "checker/module.h"
#include "util/arena.h"
#include "util/error_handler.h"


namespace type_resolver {
    util::StringConstRef make_name(const parser::Declaration& decl, util::StringAllocator& alloc);

    module::Module resolve_types(parser::File file, std::vector<std::string> predefined_types, util::StringAllocator& alloc, util::ErrorHandler& err);

    module::Expression* resolve_expression(const module::Module& module, parser::Expression expr, util::ErrorHandler err);
}
