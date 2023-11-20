#ifndef COMPILER_V2_COMMON_UTIL_HDR_
#define COMPILER_V2_COMMON_UTIL_HDR_

#include <cstddef>
#include <functional>
#include <type_traits>

namespace common {
    template <typename T, typename Tag>
    class Distinct {
      public:
        constexpr Distinct() = default;
        constexpr explicit Distinct(T val) : val_(val) {}

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
