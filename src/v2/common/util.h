#ifndef COMPILER_V2_COMMON_UTIL_HDR_
#define COMPILER_V2_COMMON_UTIL_HDR_

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
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
            requires(TAG2 !=
                     TAG)
            : val_(*other) {}

        Distinct<T, TAG, DEFAULT> &operator=(const Distinct<T, TAG, DEFAULT> &) = default;

        constexpr explicit operator T() const { return val_; }

        constexpr T &operator*() noexcept { return val_; }

        constexpr const T &operator*() const noexcept { return val_; }

        constexpr T *operator->() noexcept { return &val_; }
        constexpr const T *operator->() const noexcept { return &val_; }

        constexpr bool operator==(const Distinct<T, TAG, DEFAULT> &other) const { return val_ == other.val_; }

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
            GENERIC,
            LITERAL,
            IDENTIFIER,
            EXPRESSION,
            FUNCTION,
            TYPE,
            STATEMENT,
            SCOPE,
            SYMBOL,
            VARIABLE,
            BASIC_BLOCK,

            COUNT,
        };
        static_assert(COUNT <= 255, "too many IDs");
    } // namespace IDType

    template <size_t TAG>
    using IDBase = Distinct<uint64_t, TAG, static_cast<uint64_t>(-1)>;
    using GenericID = IDBase<IDType::GENERIC>;
    using LiteralID = IDBase<IDType::LITERAL>;
    using IdentifierID = IDBase<IDType::IDENTIFIER>;
    using ExpressionID = IDBase<IDType::EXPRESSION>;
    using FunctionID = IDBase<IDType::FUNCTION>;
    using TypeID = IDBase<IDType::TYPE>;
    using StatementID = IDBase<IDType::STATEMENT>;
    using ScopeID = IDBase<IDType::SCOPE>;
    using SymbolID = IDBase<IDType::SYMBOL>;
    using VariableID = IDBase<IDType::VARIABLE>;
    using BasicBlockID = IDBase<IDType::BASIC_BLOCK>;

    constexpr inline GenericID g_invalid_id{static_cast<uint64_t>(-1)};
    constexpr inline LiteralID g_false_id{2};
    constexpr inline LiteralID g_true_id{1};

    template <typename ID, typename Type>
    constexpr inline ID make_id(Type tag, uint64_t idx)
        requires std::is_enum_v<Type> && std::is_same_v<std::underlying_type_t<Type>, uint8_t>
    {
        return ID{(static_cast<uint64_t>(tag) << 56) | idx};
    }

    template <typename Type, size_t TAG>
    constexpr inline std::pair<Type, uint64_t> decompose(IDBase<TAG> id)
        requires std::is_enum_v<Type> && std::is_same_v<std::underlying_type_t<Type>, uint8_t>
    {
        return {static_cast<Type>(*id >> 56), *id & 0xffffffffffffff};
    }

} // namespace common

namespace std {
    template <typename T, size_t TAG, T DEFAULT>
    struct hash<common::Distinct<T, TAG, DEFAULT>> {

        constexpr size_t operator()(const common::Distinct<T, TAG, DEFAULT> &val) const {
            return std::hash<T>{}(*val);
        }
    };
} // namespace std

#endif
