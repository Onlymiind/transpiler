#ifndef COMPILER_V2_COMMON_LITERALS_HDR_
#define COMPILER_V2_COMMON_LITERALS_HDR_

#include "common/util.h"
#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>

namespace common {

    // TODO: store each unique literal only once
    // NOTE: also used to store identifiers' strings
    class Literals {
        constexpr static uint64_t g_id_start = 2;

      public:
        using ID = Distinct<uint64_t, Literals>;
        constexpr static ID g_invalid_id = ID{static_cast<uint64_t>(-1)};
        constexpr static ID g_false_id = ID{0};
        constexpr static ID g_true_id = ID{1};

        Literals() = default;
        ~Literals() = default;

        Literals(const Literals &) = delete;
        Literals(Literals &&other)
            : integers_(std::move(other.integers_)), doubles_(std::move(other.doubles_)),
              strings_(std::move(other.strings_)), string_to_id_(std::move(other.string_to_id_)),
              string_storage_(std::move(other.string_storage_)), current_id_(other.current_id_) {
            other.current_id_ = g_id_start;
        }

        Literals &operator=(const Literals &) = delete;
        Literals &operator=(Literals &&other) {
            integers_ = std::move(other.integers_);
            doubles_ = std::move(other.doubles_);
            strings_ = std::move(other.strings_);
            string_to_id_ = std::move(other.string_to_id_);
            string_storage_ = std::move(other.string_storage_);
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

        ID add(std::string val) {
            auto [it, inserted] = string_storage_.insert(std::move(val));
            if (!inserted) {
                return string_to_id_.at(&*it);
            }
            ID result{current_id_};
            ++current_id_;
            strings_[result] = &(*it);
            string_to_id_[&(*it)] = result;
            return result;
        }

        std::optional<uint64_t> get_integer(ID id) const {
            auto it = integers_.find(id);
            return it == integers_.end() ? std::optional<uint64_t>{} : it->second;
        }

        std::optional<double> get_double(ID id) const {
            auto it = doubles_.find(id);
            return it == doubles_.end() ? std::optional<double>{} : it->second;
        }

        const std::string *get_string(ID id) const {
            auto it = strings_.find(id);
            return it == strings_.end() ? nullptr : it->second;
        }

        bool operator==(const Literals &other) const {
            return integers_ == other.integers_ &&
                   doubles_ == other.doubles_ &&
                   strings_ == other.strings_ &&
                   string_storage_ == other.string_storage_;
        }

      private:
        std::unordered_map<ID, uint64_t> integers_;
        std::unordered_map<ID, double> doubles_;
        std::unordered_map<ID, const std::string *> strings_;
        std::unordered_map<const std::string *, ID> string_to_id_;
        std::unordered_set<std::string> string_storage_;

        uint64_t current_id_ = g_id_start;
    };
} // namespace common

#endif
