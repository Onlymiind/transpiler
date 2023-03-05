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

    template<>
    constexpr size_t hash(std::string_view val) noexcept {
        size_t result = 0;
        for(char c : val) {
            result *= 7919;
            result += c;
        }
        return result;
    }

    template<typename T>
    constexpr size_t odd_hash(T val) noexcept {
        return (static_cast<size_t>(val) * 7919) | size_t(1);
    }

    template<>
    constexpr size_t odd_hash(std::string_view val) noexcept {
        size_t result = 0;
        for(char c : val) {
            result *= 7919;
            result += c;
        }
        return result | size_t(1);
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

    template<typename T, typename V, typename... Args>
    consteval Hashmap<T, V, sizeof...(Args)> make_hashmap(Args... args) {
        return Hashmap{std::array<std::pair<T, V>, sizeof...(Args)>{args...}};
    }
}

void BM_const_hashmap() {
    constexpr util::Hashmap map = util::make_hashmap<char, int>(
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
    );

    std::random_device device{};
    std::mt19937 rand{device()};

    volatile auto c = map.find(rand());
}

void BM_std_hashmap() {
    static const std::unordered_map<char, int> map{
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

    std::random_device device{};
    std::mt19937 rand{device()};

    volatile auto c = map.find(rand());

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
