#ifndef COMPILER_V2_COMPILER_COMPILER_HDR_
#define COMPILER_V2_COMPILER_COMPILER_HDR_

#include <iostream>

namespace compiler {

    void compile(std::istream &file, std::ostream &out, std::ostream &err, bool do_constant_folding = true);
}

#endif
