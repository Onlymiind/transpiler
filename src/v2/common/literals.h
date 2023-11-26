#ifndef COMPILER_V2_COMMON_LITERALS_HDR_
#define COMPILER_V2_COMMON_LITERALS_HDR_

#include "common/expression.h"
#include "common/util.h"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>

namespace common {

    // TODO: do string literals really need to be stored only once per unique literal?
    class Literals {
      public:
        Literals() = default;
        ~Literals() = default;

        Literals(const Literals &) = delete;
        Literals(Literals &&other) = default;

        Literals &operator=(const Literals &) = delete;
        Literals &operator=(Literals &&other) = default;

        LiteralID add(double val) {
            LiteralID result{make_id(LiteralType::FLOAT, doubles_.size())};
            doubles_.push_back(val);
            return result;
        }

        LiteralID add(uint64_t val) {
            LiteralID result{make_id(LiteralType::UINT, integers_.size())};
            integers_.push_back(val);
            return result;
        }

        LiteralID add(std::string val) {
            LiteralID result = make_id(LiteralType::STRING, strings_.size());
            auto [it, inserted] = string_storage_.try_emplace(std::move(val), result);
            if (!inserted) {
                return it->second;
            }
            strings_.push_back(&it->first);
            return result;
        }

        std::optional<uint64_t> get_integer(LiteralID id) const {
            auto [type, idx] = decompose(id);
            return type != LiteralType::UINT || idx >= integers_.size() ? std::optional<uint64_t>{} : integers_[idx];
        }

        std::optional<double> get_double(LiteralID id) const {
            auto [type, idx] = decompose(id);
            return type != LiteralType::FLOAT || idx >= doubles_.size() ? std::optional<double>{} : doubles_[idx];
        }

        const std::string *get_string(LiteralID id) const {
            auto [type, idx] = decompose(id);
            return type != LiteralType::STRING || idx >= strings_.size() ? nullptr : strings_[idx];
        }

        bool operator==(const Literals &other) const {
            return integers_ == other.integers_ &&
                   doubles_ == other.doubles_ &&
                   strings_ == other.strings_ &&
                   string_storage_ == other.string_storage_;
        }

      private:
        static constexpr LiteralID make_id(LiteralType type, uint64_t idx) {
            return LiteralID{(static_cast<uint64_t>(type) << 56) | idx};
        }

        static constexpr std::pair<LiteralType, uint64_t> decompose(LiteralID id) {
            return {static_cast<LiteralType>(*id >> 56), *id & 0xffffffffffffff};
        }

        std::vector<uint64_t> integers_;
        std::vector<double> doubles_;
        std::vector<const std::string *> strings_;
        std::unordered_map<std::string, LiteralID> string_storage_;
    };

    class Identifiers {
      public:
        Identifiers() = default;
        ~Identifiers() = default;
        Identifiers(Identifiers &&) = default;
        Identifiers &operator=(Identifiers &&) = default;

        Identifiers(const Identifiers &) = delete;
        Identifiers &operator=(const Identifiers &) = delete;

        IdentifierID add(std::string str) {
            auto [it, inserted] = name_to_id_.try_emplace(std::move(str), id_to_name_.size());
            if (inserted) {
                id_to_name_.push_back(&it->first);
            }
            return it->second;
        }

        const std::string *get(IdentifierID id) const {
            return id == IdentifierID{g_invalid_id} || *id >= id_to_name_.size() ? nullptr : id_to_name_[*id];
        }

        IdentifierID get(const std::string &name) const {
            auto it = name_to_id_.find(name);
            return it == name_to_id_.end() ? IdentifierID{g_invalid_id} : it->second;
        }

      private:
        std::vector<const std::string *> id_to_name_;
        std::unordered_map<std::string, IdentifierID> name_to_id_;
    };
} // namespace common

#endif
