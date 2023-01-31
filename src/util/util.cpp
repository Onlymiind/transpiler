#include "util/util.h"
#include <stack>
#include <optional>


namespace util {

    template<typename T>
    void print(std::ostream& out, const T& val) {
        out << "value: " << val << ' ';
    }

    template<>
    void print(std::ostream& out, const std::monostate& val) {}

    std::ostream& operator<<(std::ostream& out, const Token& token) {
        if(!token.value.empty()) {
            out << "value: " << token.value << ", ";
        }

        out << "category: " << to_string(token.category) << " line: " << token.line;
        return out;
    }

    size_t consume_scope(std::span<const Token> tokens, size_t start, std::pair<Category, Category> scope_delimiters) {
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

    std::optional<size_t> consume_scopes(std::span<const Token> tokens, size_t start, std::unordered_map<Category, Category> scope_delimiters) {
        if(start >= tokens.size() || !scope_delimiters.contains(tokens[start].category)) {
            return start;
        }

        std::stack<Category> scope;
        auto inverse = util::inverse(scope_delimiters);
        size_t i = start;
        for(; i < tokens.size(); i++) {
            if(scope_delimiters.contains(tokens[i].category)) {
                scope.push(tokens[i].category);
            } else if(inverse.contains(tokens[i].category)) {
                if(scope.top() != inverse[tokens[i].category]) {
                    return {};
                }
                scope.pop();
            }


            if(scope.empty()) {
                break;
            }
        }

        return i;
    }
}