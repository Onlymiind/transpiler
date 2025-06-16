#ifndef COMPILER_V2_COMMON_LITERALS_HDR_
#define COMPILER_V2_COMMON_LITERALS_HDR_

#include "common/util.h"

#include <string>
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
            auto [it, inserted] = name_to_id_.try_emplace(std::move(str),
                                                          id_to_name_.size());
            if (inserted) {
                id_to_name_.push_back(&it->first);
            }
            return it->second;
        }

        StringID add_string(std::string str) {
            auto [it, inserted] = string_to_id_.try_emplace(std::move(str),
                                                            strings_.size());
            if (inserted) {
                strings_.push_back(&it->first);
            }
            return it->second;
        }

        const std::string *get(IdentifierID id) const {
            return id == IdentifierID{} || *id >= id_to_name_.size()
                       ? nullptr
                       : id_to_name_[*id];
        }

        IdentifierID get(std::string_view name) const {
            auto it = name_to_id_.find(name);
            return it == name_to_id_.end() ? IdentifierID{} : it->second;
        }

        const std::string *get(StringID id) const {
            return id == StringID{} || *id >= strings_.size() ? nullptr
                                                              : strings_[*id];
        }

        const std::vector<const std::string *> &strings() const noexcept {
            return strings_;
        }

      private:
        std::vector<const std::string *> id_to_name_;
        StringMap<std::string, IdentifierID> name_to_id_;

        std::vector<const std::string *> strings_;
        StringMap<std::string, StringID> string_to_id_;
    };
} // namespace common

#endif
