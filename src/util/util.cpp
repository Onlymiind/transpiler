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

        out << "category: " << to_string(token.category) << " pos: " << token.pos;
        return out;
    }

    size_t consume_scope(Tokens tokens, size_t start, std::pair<Category, Category> scope_delimiters) {
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

    std::optional<size_t> consume_scopes(Tokens tokens, size_t start, std::unordered_map<Category, Category> scope_delimiters) {
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

    Tokens split(Tokens& tokens, Category delim) {
        if(tokens.empty()) {
            return tokens;
        }

        auto offset = find_in_current_scope(tokens, delim);
        if(!offset) {
            return Tokens{};
        }

        auto result = tokens.subspan(0, *offset);
        tokens = tokens.subspan(std::min(tokens.size(), *offset + 1));
        return result;
    }

    std::optional<size_t> find_in_current_scope(Tokens tokens, util::Category cat) {
        size_t scope{};
        for(size_t i = 0; i < tokens.size(); i++) {
            
            // "{" will be consumed by util::consume_scope, so check for it here 
            if(cat == util::Category::LBRACE && tokens[i].category == cat) {
                return i;
            }

            i = util::consume_scope(tokens, i, {util::Category::LBRACE, util::Category::RBRACE});
            if(i >= tokens.size()) {
                break;
            }

            if(scope == 0 && tokens[i].category == cat) {
                return i;
            }
        }

        return {};
    }

    std::optional<std::pair<util::Category, size_t>> find_in_current_scope(Tokens tokens, const std::unordered_set<Category>& categories) {
        size_t scope{};
        bool has_lbrace = categories.contains(Category::LBRACE);
        for(size_t i = 0; i < tokens.size(); i++) {

            Category cat = tokens[i].category;
            
            // "{" will be consumed by util::consume_scope, so check for it here 
            if(has_lbrace && cat == Category::LBRACE) {
                return std::pair{cat, i};
            }

            i = util::consume_scope(tokens, i, {util::Category::LBRACE, util::Category::RBRACE});
            if(i >= tokens.size()) {
                break;
            }

            if(scope == 0 && categories.contains(cat)) {
                return std::pair{cat, i};
            }
        }

        return {};
    }

}