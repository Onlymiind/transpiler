#ifndef COMPILER_V2_COMMON_UTIL_HDR_
#define COMPILER_V2_COMMON_UTIL_HDR_

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>

namespace common {
    template <typename T, size_t TAG, T DEFAULT>
    class Distinct {
      public:
        constexpr Distinct() = default;
        constexpr explicit Distinct(T val) : val_(val) {}

        template <size_t TAG2, T DEFAULT2>
        constexpr explicit Distinct(Distinct<T, TAG2, DEFAULT2> other)
            requires(TAG2 != TAG)
            : val_(*other) {}

        Distinct<T, TAG, DEFAULT> &
        operator=(const Distinct<T, TAG, DEFAULT> &) = default;

        constexpr explicit operator T() const { return val_; }

        constexpr T &operator*() noexcept { return val_; }

        constexpr const T &operator*() const noexcept { return val_; }

        constexpr T *operator->() noexcept { return &val_; }
        constexpr const T *operator->() const noexcept { return &val_; }

        constexpr bool
        operator==(const Distinct<T, TAG, DEFAULT> &other) const {
            return val_ == other.val_;
        }

      private:
        T val_ = DEFAULT;
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
        std::string msg;
        size_t pos = 0;

        constexpr bool empty() const { return msg.empty(); }
    };

    namespace IDType {
        enum IDType : size_t {
            IDENTIFIER,
            FUNCTION,
            STATEMENT,
            VARIABLE,
            BASIC_BLOCK,

            COUNT,
        };
        static_assert(COUNT <= 255, "too many IDs");
    } // namespace IDType

    template <size_t TAG>
    using IDBase = Distinct<uint64_t, TAG, static_cast<uint64_t>(-1)>;
    using IdentifierID = IDBase<IDType::IDENTIFIER>;
    using FunctionID = IDBase<IDType::FUNCTION>;
    using VariableID = IDBase<IDType::VARIABLE>;
    using BasicBlockID = IDBase<IDType::BASIC_BLOCK>;

    template <typename ID, typename Type>
    constexpr inline ID make_id(Type tag, uint64_t idx)
        requires std::is_enum_v<Type> &&
                 std::is_same_v<std::underlying_type_t<Type>, uint8_t>
    {
        return ID{(static_cast<uint64_t>(tag) << 56) | idx};
    }

    template <typename Type, size_t TAG>
    constexpr inline std::pair<Type, uint64_t> decompose(IDBase<TAG> id)
        requires std::is_enum_v<Type> &&
                 std::is_same_v<std::underlying_type_t<Type>, uint8_t>
    {
        return {static_cast<Type>(*id >> 56), *id & 0xffffffffffffff};
    }

    template <typename Derived, typename Base>
    inline Derived &downcast(Base &base)
        requires std::is_base_of_v<std::decay_t<Base>, Derived>
    {
        if (base.kind() != Derived::static_kind()) {
            throw std::runtime_error("downcasting to wrong derived type");
        }
        return static_cast<Derived &>(base);
    }

    template <typename Derived, typename Base>
    inline const Derived &downcast(const Base &base)
        requires std::is_base_of_v<std::decay_t<Base>, Derived>
    {
        if (base.kind() != Derived::static_kind()) {
            throw std::runtime_error("downcasting to wrong derived type");
        }
        return static_cast<const Derived &>(base);
    }

    template <typename ReleaseFunc>
    class RAIIGuard {
      public:
        template <typename AcquireFunc>
        RAIIGuard(AcquireFunc aquire_func, ReleaseFunc release_func)
            : release_func_(std::move(release_func)) {
            aquire_func();
        }
        ~RAIIGuard() { release_func_(); }
        RAIIGuard(const RAIIGuard &) = delete;
        RAIIGuard(RAIIGuard &&) = delete;
        RAIIGuard &operator=(const RAIIGuard &) = delete;
        RAIIGuard &operator=(RAIIGuard &&) = delete;

      private:
        ReleaseFunc release_func_;
    };

} // namespace common

namespace std {
    template <typename T, size_t TAG, T DEFAULT>
    struct hash<common::Distinct<T, TAG, DEFAULT>> {

        constexpr size_t
        operator()(const common::Distinct<T, TAG, DEFAULT> &val) const {
            return std::hash<T>{}(*val);
        }
    };
} // namespace std

#endif
