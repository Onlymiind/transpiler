#ifndef COMPILER_V2_COMPILER_COMPILER_HDR_
#define COMPILER_V2_COMPILER_COMPILER_HDR_

#include "vm/vm.h"
#include <iostream>

namespace compiler {

    [[nodiscard]] std::optional<vm::Program> compile(std::istream &file,
                                                     std::ostream *err);
}

#endif
