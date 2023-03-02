#pragma once 
#include <array>
#include <utility>
#include <cstdint>
#include <cstddef>
#include <limits>

namespace util {

    // if value is greater than greatest pow of 2
    // that can be represented by size_t
    // the function will return std::numeric_limits<size_t>::max()
    inline constexpr size_t next_pow_of_two(size_t val) {
        /* can be done faster, but this is simpler 
           and will probably be computed at compile time anyway*/
        size_t i = 0;
        for(;val != 0; val >>= 1) {
            i++;
        }
        size_t result = static_cast<size_t>(1) << i;
        if(i >= std::numeric_limits<size_t>::digits) {
            result = std::numeric_limits<size_t>::max();
        }

        return result;
    }

    template<typename T>
    struct Hash {
        constexpr size_t operator()(const T&) const noexcept;
    };

    template<typename Key, typename Val, size_t Count>
    class Hashmap {
    public:
        using Storage = std::array<std::pair<Key, Val>, next_pow_of_two(Count)>;

        using iterator = typename Storage::const_iterator;
    
        constexpr Hashmap(std::array<std::pair<Key, Val>, Count> items) noexcept;

        constexpr const Val* operator[](const Key& key) const noexcept;

        constexpr bool contains(const Key& key) const noexcept;

        constexpr iterator find(const Key& key) const noexcept;

        constexpr iterator begin() const noexcept{
            return storage_.begin();
        }

        constexpr iterator end() const noexcept{
            return storage_.end();
        }
    private:
        std::array<std::pair<Key, Val>, next_pow_of_two(Count)> storage_;
        Hash<Key> hash1_;
        Hash<Key> hash2_;
    };
}
