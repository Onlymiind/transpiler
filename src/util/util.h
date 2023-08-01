#pragma once
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <vector>
#include <variant>
#include <ostream>
#include <sstream>
#include <iterator>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <optional>

#include "types/token.h"

namespace util {
    struct Error {
        size_t pos = 0;
        std::string msg;
    };

    inline std::ostream& operator<<(std::ostream& out, const Error& err) {
        out << "Error on pos " << err.pos << ", message: " << err.msg;
        return out;
    }

    template<typename Key, typename Val>
    inline std::unordered_map<Val, Key> inverse(const std::unordered_map<Key, Val>& map) {
        std::unordered_map<Val, Key> inverse;
        inverse.reserve(map.size());
        std::transform(map.begin(), map.end(), std::inserter(inverse, inverse.begin()), [](const auto& pair) {
            return std::pair<Val, Key>{pair.second, pair.first};
        });

        return inverse;
    }

    size_t consume_scope(types::Tokens tokens, size_t start, std::pair<types::Category, types::Category> scope_delimiters);

    std::optional<size_t> consume_scopes(types::Tokens tokens, size_t start, std::unordered_map<types::Category, types::Category> scope_delimiters);

    types::Tokens split(types::Tokens& tokens, types::Category delim);

    std::optional<size_t> find_in_current_scope(types::Tokens tokens, types::Category cat);

    std::optional<std::pair<types::Category, size_t>> find_in_current_scope(types::Tokens tokens, const std::unordered_set<types::Category>& categories);

    template<typename T, typename... Args>
    concept Function = std::is_invocable_v<T, Args...>;

    template<typename... T>
    std::string sprint(T... args) {
        std::ostringstream out;
        (out << ... << args);
        return out.str();
    }

    template<typename T>
    constexpr bool deep_eq(T* lhs, T* rhs) {
        return (!lhs && !rhs) || *lhs == *rhs;
    }

    template<typename T, typename Tag>
    class Distinct {
    public:
        constexpr Distinct() = default;
        explicit constexpr Distinct(T val)
            : val_(val)
        {}

        explicit constexpr operator T() const {
            return val_;
        }

        bool operator==(Distinct<T, Tag> other) const { return val_ == other.val_; }

    private:
        T val_{};
    };
}
