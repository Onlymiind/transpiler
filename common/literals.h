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
            return id == IdentifierID{} || *id >= id_to_name_.size() ? nullptr : id_to_name_[*id];
        }

        IdentifierID get(const std::string &name) const {
            auto it = name_to_id_.find(name);
            return it == name_to_id_.end() ? IdentifierID{} : it->second;
        }

      private:
        std::vector<const std::string *> id_to_name_;
        std::unordered_map<std::string, IdentifierID> name_to_id_;
    };
} // namespace common

#endif
