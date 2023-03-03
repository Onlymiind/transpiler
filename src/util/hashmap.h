#pragma once 
#include <array>
#include <optional>
#include <utility>
#include <cstdint>
#include <cstddef>
#include <limits>

namespace util {

    // if value is greater than greatest pow of 2
    // that can be represented by size_t
    // the function will return std::numeric_limits<size_t>::max()
    inline consteval size_t next_pow_of_two(size_t val) {
        /* can be done faster, but this is simpler 
           and will be computed at compile time anyway*/
        size_t i = 1;
        for(;val != 0; val >>= 1) {
            i++;
        }
        size_t result = 0;
        if(i >= (std::numeric_limits<size_t>::digits - 1)) {
            result = std::numeric_limits<size_t>::max();
        } else {
            result = static_cast<size_t>(1) << i;
        }

        return result;
    }

    inline consteval size_t get_hashmap_storage_size(size_t elem_count) {
        size_t next_pow = next_pow_of_two(elem_count);
        double load_factor = double(elem_count) / double(next_pow);
        size_t result = next_pow;
        if(load_factor > 0.5) {
            if(next_pow >= (size_t(1) << (std::numeric_limits<size_t>::digits - 1))) {
                result = std::numeric_limits<size_t>::max();
            } else {
                result *= 2;
            }
        }

        return result;
    }

    template<typename T>
    constexpr size_t hash(T val) noexcept {
        return static_cast<size_t>(val);
    }

    template<typename T>
    constexpr size_t odd_hash(T val) noexcept {
        return (static_cast<size_t>(val) * 7919) | size_t(1);
    }

    template<typename Key, typename Val, size_t Count>
    class Hashmap {
    public:
        using StoredType = std::optional<std::pair<Key, Val>>;
        using Storage = std::array<StoredType, get_hashmap_storage_size(Count)>;

        using iterator = typename Storage::const_iterator;
    
        constexpr Hashmap(std::array<std::pair<Key, Val>, Count> items) noexcept {
            bool dummy = false;
            for(auto pair : items) {
                // insertion should always succeed at this point
                dummy = insert(pair);
            }
        }

        constexpr std::optional<Val> operator[](Key key) const noexcept {
            iterator it = find(key);
            return it == end() ? std::optional<Val>{} : std::optional<Val>{(*it)->second};
        }

        constexpr bool contains(Key key) const noexcept {
            return find(key) == end();
        }

        constexpr iterator find(Key key) const noexcept {
            for(size_t i = 0; i < storage_.size(); i++) {
                size_t idx = hash(key, i);
                if(!storage_[idx]) {
                    return end();
                }
                if(storage_[idx]->first == key) {
                    return begin() + idx;
                }
            }

            return end();
        }

        constexpr iterator begin() const noexcept{
            return storage_.begin();
        }

        constexpr iterator end() const noexcept{
            return storage_.end();
        }

        [[nodiscard]] constexpr bool insert(Key k, Val v) noexcept {
            return insert(std::pair<Key, Val>(k, v));
        }

        [[nodiscard]] constexpr bool insert(std::pair<Key, Val> p) noexcept {
            for(size_t i = 0; i < storage_.size(); i++) {
                size_t idx = hash(p.first, i);
                if(!storage_[idx]) {
                    storage_[idx] = p;
                    return true;
                }
            }
        
            return false;
        }

        constexpr size_t hash(Key k, size_t probe) const noexcept {
            return (util::hash(k) + probe * odd_hash(k)) % storage_.size();
        }
        
    private:
        Storage storage_;
    };

    template<typename T, typename... KeyValPairs>
    consteval auto make_hashmap(KeyValPairs... args) {
        return Hashmap{std::array<T, sizeof...(KeyValPairs)>{args...}};
    }
}
