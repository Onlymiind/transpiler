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
        Arena() noexcept = default;
        Arena(const Arena<T>&) = delete;
        Arena(Arena<T>&&) noexcept = default;

        ~Arena() {
            for(Block& b : blocks_) {
                delete[] b.data;
            }
        }

        template<typename... Args>
        T* allocate(Args&&... args) {
            //TODO: what if constructor throws?
            T* result = new (allocate_()) T(std::forward<Args>(args)...);
            size_++;
            return result;
        }

        void reserve(size_t count) {
            size_t capacity = 0;
            for(size_t i = last_block_; i < blocks_.size(); i++) {
                capacity += blocks_[i].size - blocks_[i].capacity;
            }
            if(count <= capacity) {
                return;
            }
            allocate_block(count - capacity);
        }

        size_t get_block_size() const noexcept{
            return g_block_size;
        }

        size_t size() const {
            return size_;
        }

        Arena& operator=(const Arena<T>&) = delete;
        Arena& operator=(Arena<T>&&) = default;
    private:
        T* allocate_() {
            if(!has_space()) {
                allocate_block(g_block_size);
            }

            return next();
        }

        bool has_space() const noexcept {
            return (blocks_.size() > last_block_ + 1) && (blocks_[last_block_].occupied < blocks_[last_block_].size);
        }

        T* next() {
            Block& last = blocks_[last_block_];
            T* ptr = last.data + last.occupied;
            last.occupied++;
            if(last.occupied == last.size) {
                last_block_++;
            }
            return ptr;
        }

        void allocate_block(size_t count) {
            Block b {
                .size = count,
                .data = new T[count],
            };

            try {
                blocks_.push_back(b);
            } catch(...) {
                blocks_.pop_back();
                throw;
            }
        }

        std::vector<Block> blocks_;
        size_t last_block_ = 0;
        size_t size_ = 0;
        static constexpr size_t g_block_size = std::max<size_t>(512 / sizeof(T), 10);
    };

    template<typename... Types>
    class ArenaPool {
    public:
        template<typename T>
        Arena<T>& get() noexcept {
            return std::get<Arena<T>>(arenas_);
        }

        template<typename T, typename... Args>
        T* allocate(Args&&... args) {
            return std::get<Arena<T>>(arenas_).allocate(std::forward<Args>(args)...);
        }

        template<typename T>
        void reserve(size_t count) {
            return std::get<Arena<T>>(arenas_).reserve(count);
        }

        template<typename T>
        size_t size() const {
            return std::get<Arena<T>>(arenas_).size();
        }
    private:
        std::tuple<Arena<Types>...> arenas_;
    };
}
