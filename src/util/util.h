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

    size_t consume_scope(types::Tokens tokens, size_t start, std::pair<types::Category, types::Category> scope_delimiters);

    std::optional<size_t> find_in_current_scope(types::Tokens tokens, types::Category cat);

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

namespace std {
    template<typename T, typename Tag>
    struct hash<util::Distinct<T, Tag>> {
        size_t operator()(util::Distinct<T, Tag> val) const {
            return hash<T>{}(T(val));
        }
    };
}
