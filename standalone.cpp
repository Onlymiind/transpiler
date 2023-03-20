#include <array>
#include <optional>
#include <string_view>
#include <utility>
#include <cstdint>
#include <cstddef>
#include <limits>
#include <unordered_map>
#include <random>

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
        bool free = true;
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
    private:
        constexpr void insert(Key key, size_t item_idx) noexcept {
            size_t idx = hash(key);
            ItemInfo item = ItemInfo{
                .idx = uint8_t(item_idx),
                .free = false
            };
            for(size_t i = 0; i < storage_.size(); i++) {
                idx = (idx + i) % storage_.size();
                if(storage_[idx].free) {
                    storage_[idx] = item;
                } else if (storage_[idx].probe < item.probe) {
                    std::swap(item, storage_[idx]);
                }
                item.probe++;
            }
        }

        Storage storage_;
        std::array<std::pair<Key, Val>, Count> items_;
    };
    
}

    constexpr util::Hashmap map = util::Hashmap{std::array{
        std::pair{'!', 1},
        std::pair{'&', 2},
        std::pair{'|', 3},
        std::pair{'/', 4},
        std::pair{'*', 5},
        std::pair{'(', 6},
        std::pair{')', 7},
        std::pair{'{', 8},
        std::pair{'}', 9},
        std::pair{'[', 10},
        std::pair{']', 11},
        std::pair{',', 12},
        std::pair{'\'', 13},
        std::pair{'\"', 14},
        std::pair{':', 15},
        std::pair{';', 16},
        std::pair{'.', 17},
        std::pair{'+', 18},
        std::pair{'-', 19},
        std::pair{'=', 20},
        std::pair{'?', 21},
        std::pair{'<', 22},
        std::pair{'>', 23},
        std::pair{'~', 24},
        std::pair{'^', 25}
    }};
void BM_const_hashmap() {

    std::random_device device{};
    std::mt19937 rand{device()};

    volatile auto c = map.find(rand());
}

    static const std::unordered_map<char, int> std_map{
        {'!', 1},
        {'&', 2},
        {'|', 3},
        {'/', 4},
        {'*', 5},
        {'(', 6},
        {')', 7},
        {'{', 8},
        {'}', 9},
        {'[', 10},
        {']', 11},
        {',', 12},
        {'\'', 13},
        {'\"', 14},
        {':', 15},
        {';', 16},
        {'.', 17},
        {'+', 18},
        {'-', 19},
        {'=', 20},
        {'?', 21},
        {'<', 22},
        {'>', 23},
        {'~', 24},
        {'^', 25}
    };
void BM_std_hashmap() {

    std::random_device device{};
    std::mt19937 rand{device()};

    volatile auto c = std_map.find(rand());

}

constexpr inline int switch_func(char c) {
        switch(c) 
        {
        case '!': return  1;
        case '&': return  2;
        case '|': return  3;
        case '/': return  4;
        case '*': return  5;
        case '(': return  6;
        case ')': return  7;
        case '{': return  8;
        case '}': return  9;
        case '[': return  10;
        case ']': return  11;
        case ',': return  12;
        case '\'': return  13;
        case '\"': return  14;
        case ':': return  15;
        case ';': return  16;
        case '.': return  17;
        case '+': return  18;
        case '-': return  19;
        case '=': return  20;
        case '?': return  21;
        case '<': return  22;
        case '>': return  23;
        case '~': return  24;
        case '^': return  25;
        default: return -1;
        }
}

void BM_switch() {
    std::random_device device{};
    std::mt19937 rand{device()};

    volatile auto c = switch_func(rand());
}

int main() {
    BM_const_hashmap();
    BM_std_hashmap();
    BM_switch();
}
