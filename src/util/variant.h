#pragma once 
#include <ostream>
#include <type_traits>
#include <utility>
#include <variant>

#include "util/arena.h"

namespace util {

    //this is just a wrapper to provide a more convenient interface to std::variant
    template<typename... Args>
    class Variant {
        template<typename... Args1>
        friend std::ostream& operator<<(std::ostream& out, const Variant<Args1...> v);
    public:
        Variant() = default;

        template<typename T>
        Variant(T&& val)
            : val_(std::forward<T>(val))
        {}

        template<typename T>
        constexpr bool is() const noexcept {
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

        template<typename T>
        constexpr const T* get_if() const noexcept {
            return std::get_if<T>(&val_);
        }

        template<typename T>
        constexpr const T& get() const {
            return std::get<T>(val_);
        }

        template<typename Function>
        constexpr void visit(Function func) {
            std::visit(func, val_);
        }

        constexpr bool operator==(const Variant<Args...> other) const {
            return val_ == other.val_;
        }

        constexpr bool empty() const {
            return std::holds_alternative<std::monostate>(val_);
        }
    private:
        std::variant<Args...> val_;
    };

    template<typename... Args>
    std::ostream& operator<<(std::ostream& out, const Variant<Args...> v) {
        std::visit([&out](const auto& val) {
            using typ = std::decay_t<decltype(val)>;
            if constexpr (std::is_same_v<std::monostate, typ>) {
                return;
            } else if constexpr (std::is_same_v<util::StringConstRef, typ>){
                out << *val;
            } else {
                out << val;
            }
        }, v.val_);
        return out;
    }
}
