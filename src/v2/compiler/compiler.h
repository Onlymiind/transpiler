#ifndef COMLILER_V2_COMPILER_COMPILER_HDR_
#define COMLILER_V2_COMPILER_COMPILER_HDR_

#include <iostream>

namespace compiler {

    void compile(std::istream &file, std::ostream &out, std::ostream &err);
}

#endif
