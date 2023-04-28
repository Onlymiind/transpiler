#pragma once 
#include <utility>
#include <variant>

namespace util {

    //this is just a wrapper to provide a more convenient interface to std::variant
    template<typename... Args>
    class Variant {
    public:
        Variant() = default;

        template<typename T>
        Variant(T&& val)
            : val_(std::forward<T>(val))
        {}

        template<typename T>
        constexpr bool is() noexcept {
            return std::holds_alternative<T>(val_);
        }

        template<typename T>
        constexpr T* get_if() noexcept {
            return std::get_if<T>(&val_);
        }

        template<typename T>
        constexpr T& get() {
            return std::get<T>(val_);
        }

        constexpr bool operator==(const Variant<Args...> other) const {
            return val_ == other.val_;
        }
    private:
        std::variant<Args...> val_;
    };
}