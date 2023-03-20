#pragma once 
#include <array>
#include <optional>
#include <string_view>
#include <utility>
#include <cstdint>
#include <cstddef>
#include <limits>
#include <iostream>

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

    // TODO: write actually fast string hash
    template<>
    constexpr size_t hash(std::string_view val) noexcept {
        uint64_t result = 0;
        size_t rem = val.size() % 8;
        for(size_t i = 0; i < rem; i++) {
            result |= uint64_t(val[i]) << i;
        }
        uint64_t buf = 0;
        for(size_t i = rem; i < val.size(); i += 8) {
            buf |= uint64_t(val[i]);
            buf |= uint64_t(val[i + 1]) << 1;
            buf |= uint64_t(val[i + 2]) << 2;
            buf |= uint64_t(val[i + 3]) << 3;
            buf |= uint64_t(val[i + 4]) << 4;
            buf |= uint64_t(val[i + 5]) << 5;
            buf |= uint64_t(val[i + 6]) << 6;
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
    };

    template<typename Key, typename Val, size_t Count>
    class Hashmap {
    public:
        
        using Storage = std::array<ItemInfo, next_pow_of_two(Count)>;

        using iterator = typename std::array<std::pair<Key, Val>, Count>::const_iterator;
    
        constexpr Hashmap(std::array<std::pair<Key, Val>, Count> items) noexcept 
            : items_(items)
        {   
            for(size_t i = 0; i < items_.size(); i++) {
                insert(items_[i].first, i);
            }
        }

        constexpr std::optional<Val> operator[](Key key) const noexcept {
            iterator it = find(key);
            return it == end() ? std::optional<Val>{} : std::optional<Val>{it->second};
        }

        constexpr bool contains(Key key) const noexcept {
            return find(key) != end();
        }

        constexpr iterator find(Key key) const noexcept {
            size_t key_hash = hash(key);
            size_t idx = key_hash % storage_.size();
            for(size_t i = 0; i < storage_.size(); i++) {
                const auto& item = storage_[idx];
                if(item.probe == 0) {
                    return end();
                } else if(items_[item.idx].first == key) {
                    return begin() + item.idx;
                } else if(item.probe < i) {
                    return end();
                }
                idx = (idx + 1) % storage_.size();
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

        constexpr auto storage() const noexcept {
            return storage_;
        }
    private:
        constexpr void insert(Key key, size_t item_idx) noexcept {
            ItemInfo item = ItemInfo{
                .idx = uint8_t(item_idx),
                .probe = 1
            };
            // this will never run forever because storage_.size() is guaranteed 
            // to be enough to hold all elements
            for(size_t idx = hash(key) % storage_.size(); ; idx = (idx + 1) % storage_.size()) {
                if(storage_[idx].probe == 0) {
                    storage_[idx] = item;
                    break;
                } else if (storage_[idx].probe < item.probe) {
                    std::swap(item, storage_[idx]);
                }
                item.probe += 1;
            }
        }

        Storage storage_;
        std::array<std::pair<Key, Val>, Count> items_;
    };
    
}

