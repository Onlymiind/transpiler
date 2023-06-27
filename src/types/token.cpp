#include "types/token.h"

namespace types {
    std::ostream& operator<<(std::ostream& out, const Token& token) {
        if(!token.value.empty()) {
            out << "value: " << token.value << ", ";
        }

        out << "category: " << to_string(token.category) 
            << " byte offset: " << token.pos;
        return out;
    }
}
