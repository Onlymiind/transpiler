#include "types/token.h"

namespace types {
    std::ostream& operator<<(std::ostream& out, const Token& token) {
        if(token.value && !token.value->empty()) {
            out << "value: " << *token.value << ", ";
        }

        out << "category: " << to_string(token.category) 
            << " line: " << token.pos.line << " byte: " << token.pos.byte;
        return out;
    }
}
