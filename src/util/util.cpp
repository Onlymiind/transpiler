#include "util/util.h"
#include <stack>
#include <optional>


namespace util {

    std::ostream& operator<<(std::ostream& out, const types::Token& token) {
        if(!token.value.empty()) {
            out << "value: " << token.value << ", ";
        }

        out << "category: " << to_string(token.category) << " pos: " << token.pos;
        return out;
    }

    size_t consume_scope(types::Tokens tokens, size_t start, std::pair<types::Category, types::Category> scope_delimiters) {
        if(start >= tokens.size() || tokens[start].category != scope_delimiters.first) {
            return start;
        }

        size_t i = start;
        size_t scope = 0;
        for(; i < tokens.size(); i++) {
            if(tokens[i].category == scope_delimiters.first) {
                scope++;
            } else if(tokens[i].category == scope_delimiters.second) {
                scope--;
            }

            if(scope == 0) {
                break;
            }
        }

        return i;
    }

    std::optional<size_t> find_in_current_scope(types::Tokens tokens, types::Category cat) {
        size_t scope{};
        for(size_t i = 0; i < tokens.size(); i++) {
            
            // "{" will be consumed by util::consume_scope, so check for it here 
            if(cat == types::Category::LBRACE && tokens[i].category == cat) {
                return i;
            }

            i = util::consume_scope(tokens, i, {types::Category::LBRACE, types::Category::RBRACE});
            if(i >= tokens.size()) {
                break;
            }

            if(scope == 0 && tokens[i].category == cat) {
                return i;
            }
        }

        return {};
    }

}
