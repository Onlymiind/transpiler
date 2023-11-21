#ifndef COMPILER_V2_COMMON_LITERALS_HDR_
#define COMPILER_V2_COMMON_LITERALS_HDR_

#include "common/util.h"
#include <cstdint>
#include <optional>
#include <unordered_map>
#include <utility>
namespace common {

    // TODO: store each unique literal only once
    class Literals {
        constexpr static uint64_t g_id_start = 2;

      public:
        using ID = Distinct<uint64_t, Literals>;
        constexpr static ID g_invalid_id = ID{static_cast<uint64_t>(-1)};
        constexpr static ID g_false_id = ID{0};
        constexpr static ID g_true_id = ID{1};

        Literals() = default;
        ~Literals() = default;

        Literals(const Literals &) = default;
        Literals(Literals &&other)
            : integers_(std::move(other.integers_)), doubles_(std::move(other.doubles_)), current_id_(other.current_id_) {
            other.current_id_ = g_id_start;
        }

        Literals &operator=(const Literals &) = default;
        Literals &operator=(Literals &&other) {
            integers_ = std::move(other.integers_);
            doubles_ = std::move(other.doubles_);
            current_id_ = other.current_id_;
            other.current_id_ = g_id_start;
            return *this;
        }

        ID add(double val) {
            ID result{current_id_};
            doubles_[result] = val;
            ++current_id_;
            return result;
        }

        ID add(uint64_t val) {
            ID result{current_id_};
            integers_[result] = val;
            ++current_id_;
            return result;
        }

        std::optional<uint64_t> get_integer(ID id) const {
            auto it = integers_.find(id);
            if (it == integers_.end()) {
                return {};
            }
            return it->second;
        }

        std::optional<double> get_double(ID id) const {
            auto it = doubles_.find(id);
            if (it == doubles_.end()) {
                return {};
            }
            return it->second;
        }

        bool operator==(const Literals &other) const {
            return integers_ == other.integers_ && doubles_ == other.doubles_;
        }

      private:
        std::unordered_map<ID, uint64_t> integers_;
        std::unordered_map<ID, double> doubles_;

        uint64_t current_id_ = g_id_start;
    };
} // namespace common

#endif
