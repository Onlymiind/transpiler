#pragma once
#include <vector>
#include <string>
#include <type_traits>
#include <span>
#include <tuple>

namespace util {

    template<typename T>
    class Arena {
        struct Block {
            size_t size = 0;
            size_t occupied = 0;
            T* data = nullptr;
        };
    public:
        constexpr Arena() noexcept = default;
        Arena(const Arena<T>&) = delete;
        constexpr Arena(Arena<T>&&) noexcept = default;

        constexpr ~Arena() {
            for(Block& b : blocks_) {
                delete[] b.data;
            }
        }

        template<typename... Args>
        constexpr T* allocate(Args&&... args) {
            return new (allocate_()) T(std::forward<Args>(args)...);
        }

        constexpr size_t get_block_size() const noexcept{
            return g_block_size;
        }

        constexpr Arena& operator=(const Arena<T>&) = delete;
        constexpr Arena& operator=(Arena<T>&&) = default;
    private:
        constexpr T* allocate_() {
            if(!blocks_.empty()) {
                Block& last = blocks_.back();
                if(last.occupied < last.size) {
                    T* ptr = last.data + last.occupied;
                    last.occupied++;
                    return ptr;
                }
            }

            Block b {
                .size = g_block_size,
                .data = new T[g_block_size],
            };

            try {
                blocks_.push_back(b);
            } catch(...) {
                blocks_.pop_back();
                throw;
            }

            Block& last = blocks_.back();
            T* ptr = last.data + last.occupied;
            last.occupied++;
            return ptr;
        }

        std::vector<Block> blocks_;
        static constexpr size_t g_block_size = std::max<size_t>(512 / sizeof(T), 10);
    };

    template<typename... Types>
    class ArenaPool {
    public:
        template<typename T>
        constexpr inline Arena<T>& get() noexcept {
            return std::get<Arena<T>>(arenas_);
        }

        template<typename T, typename... Args>
        constexpr inline T* allocate(Args&&... args) {
            return std::get<Arena<T>>(arenas_).allocate(std::forward<Args>(args)...);
        }
    private:
        std::tuple<Arena<Types>...> arenas_;
    };
}