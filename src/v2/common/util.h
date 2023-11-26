#ifndef COMPILER_V2_COMMON_UTIL_HDR_
#define COMPILER_V2_COMMON_UTIL_HDR_

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <string_view>
#include <type_traits>

namespace common {
    template <typename T, typename Tag>
    class Distinct {
      public:
        constexpr Distinct() = default;
        constexpr explicit Distinct(T val) : val_(val) {}

        template <typename Tag2>
        constexpr explicit Distinct(Distinct<T, Tag2> other) : val_(*other) {}

        constexpr explicit operator T() const { return val_; }

        constexpr T &operator*() noexcept { return val_; }

        constexpr const T &operator*() const noexcept { return val_; }

        constexpr T *operator->() noexcept { return &val_; }
        constexpr const T *operator->() const noexcept { return &val_; }

        constexpr bool operator==(const Distinct<T, Tag> &other) const { return val_ == other.val_; }

      private:
        T val_{};
    };

    template <typename T>
    constexpr inline auto to_underlying(T val)
        requires std::is_enum_v<T>
    {
        return static_cast<std::underlying_type_t<T>>(val);
    }

    template <std::unsigned_integral T>
    constexpr inline T set_bit(uint8_t bit) {
        return T{1} << bit;
    }

    struct Error {
        std::string_view msg{};
        size_t pos = 0;

        constexpr bool empty() const { return msg.empty(); }
    };

    template <typename T>
    using IDBase = Distinct<uint64_t, T>;
    using GenericID = IDBase<void>;
    constexpr inline GenericID g_invalid_id{static_cast<uint64_t>(-1)};

} // namespace common

namespace std {
    template <typename T, typename Tag>
    struct hash<common::Distinct<T, Tag>> {

        constexpr size_t operator()(const common::Distinct<T, Tag> &val) const {
            return std::hash<T>{}(*val);
        }
    };
} // namespace std

#endif
