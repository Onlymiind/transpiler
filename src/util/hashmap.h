#pragma once 
#include <array>
#include <optional>
#include <string_view>
#include <utility>
#include <cstdint>
#include <cstddef>
#include <limits>
#include <bit>

namespace util {

    // if value is greater than greatest pow of 2
    // that can be represented by size_t
    // the function will return std::numeric_limits<size_t>::max()
    inline consteval size_t next_pow_of_two(size_t val) {
        /* can be done faster, but this is simpler 
           and will be computed at compile time anyway*/
        size_t i = 0;
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

    template<>
    constexpr size_t hash(std::string_view val) noexcept {
        uint64_t result = 0;
        size_t rem = val.size() % 8;
        for(size_t i = 0; i < rem; i++) {
            result |= uint64_t(val[i]);
            result <<= 1;
        }
        uint32_t buf = 0;
        for(size_t i = rem; i < val.size(); i += 8) {
            buf |= uint64_t(val[i]);
            buf |= uint64_t(val[i + 1]) << 1;
            buf |= uint64_t(val[i + 2]) << 2;
            buf |= uint64_t(val[i + 3]) << 3;
            buf |= uint64_t(val[i + 4]) << 4;
            buf |= uint64_t(val[i + 5]) << 5;
            buf |= uint64_t(val[i + 6]) << 6;
            buf |= uint64_t(val[i + 7]) << 7;
            result = result * 7919 + buf;
        }
        return result;
    }

    // auxilary struct for hash map
    // scince main use case is small lookup tables
    // it is ok to limit idx and probe to uin8_t
    struct ItemInfo {
        uint8_t idx = 0;
        uint8_t probe = 0;
        uint8_t free = true;
    };

    template<typename Key, typename Val, size_t Count>
    class Hashmap {
    public:
        
        using Storage = std::array<ItemInfo, next_pow_of_two(Count)>;

        using iterator = typename std::array<std::pair<Key, Val>, Count>::const_iterator;
    
        constexpr Hashmap(std::array<std::pair<Key, Val>, Count> items) noexcept 
            : items_(items)
        {
            bool dummy = false;
            for(size_t i = 0; i < items_.size(); i++) {
                // insertion should always succeed at this point
                dummy = insert(items_[i].first, i);
            }
        }

        constexpr std::optional<Val> operator[](Key key) const noexcept {
            iterator it = find(key);
            return it == end() ? std::optional<Val>{} : std::optional<Val>{it->second};
        }

        constexpr bool contains(Key key) const noexcept {
            return find(key) == end();
        }

        constexpr iterator find(Key key) const noexcept {
            size_t key_hash = hash(key);
            size_t idx = key_hash;
            for(size_t i = 0; i < storage_.size(); i++) {
                idx = (idx + i) % storage_.size();
                const auto& item = storage_[idx];
                if(item.free) {
                    return end();
                } else if(items_[item.idx].first == key) {
                    return begin() + item.idx;
                } else if(item.probe > i) {
                    return end();
                }
            }

            return end();
        }

        constexpr iterator begin() const noexcept{
            return items_.begin();
        }

        constexpr iterator end() const noexcept{
            return items_.end();
        }

        constexpr size_t size() const noexcept {
            return Count;
        }

        constexpr size_t capacity() const noexcept {
            return Count;
        }

        [[nodiscard]] constexpr bool insert(Key key, size_t item_idx) noexcept {
            size_t idx = hash(key);
            ItemInfo item = ItemInfo{
                .idx = uint8_t(item_idx),
                .free = false
            };
            for(size_t i = 0; i < storage_.size(); i++) {
                idx = (idx + i) % storage_.size();
                if(storage_[idx].free) {
                    storage_[idx] = item;
                    return true;
                } else if (storage_[idx].probe < item.probe) {
                    std::swap(item, storage_[idx]);
                }
                item.probe++;
            }
            return false;
        }
        
    private:
        Storage storage_;
        std::array<std::pair<Key, Val>, Count> items_;
    };


    template<typename T, typename V, typename... Args>
    consteval Hashmap<T, V, sizeof...(Args)> make_hashmap(Args... args) {
        return Hashmap{std::array<std::pair<T, V>, sizeof...(Args)>{args...}};
    }
    
}

