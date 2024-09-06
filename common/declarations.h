#ifndef COMPILER_V2_COMMON_DECLARATIONS_HDR_
#define COMPILER_V2_COMMON_DECLARATIONS_HDR_

#include "common/base_classes.h"
#include "common/statement.h"
#include "common/util.h"

#include <cstddef>
#include <memory>
#include <unordered_map>
#include <vector>

namespace common {
    struct Function {

        FunctionID id;
        IdentifierID name;
        std::unique_ptr<ParsedType> parsed_return_type;
        Block body{1};

        size_t pos = 0;
        bool decl_only = false;

        const Type *return_type = nullptr;

        std::vector<VariableID> params;

        constexpr bool is_error() const { return id == FunctionID{}; }
        constexpr bool operator==(const Function &other) const {
            return id == other.id && name == other.name;
        }
    };

    struct Variable {
        VariableID id;
        IdentifierID name;
        std::unique_ptr<ParsedType> explicit_type;
        std::unique_ptr<Expression> initial_value;
        size_t pos = 0;
        const Type *type = nullptr;
    };

    class Struct : public DeclaredType {
      public:
        Struct(IdentifierID name, std::vector<VariableID> fields,
               std::unordered_map<IdentifierID, VariableID> name_to_field,
               size_t pos)
            : DeclaredType(name, static_kind(), pos),
              fields_(std::move(fields)),
              name_to_field_(std::move(name_to_field)){};

        static constexpr DeclaredTypeKind static_kind() noexcept {
            return DeclaredTypeKind::STRUCT;
        }
        const std::vector<VariableID> &fields() const noexcept {
            return fields_;
        }

        VariableID get_field(IdentifierID name) const {
            auto it = name_to_field_.find(name);
            return it == name_to_field_.end() ? VariableID{} : it->second;
        }

        bool has_field(IdentifierID name) const {
            return name_to_field_.contains(name);
        }

      private:
        std::vector<VariableID> fields_;
        std::unordered_map<IdentifierID, VariableID> name_to_field_;
    };

} // namespace common

#endif
