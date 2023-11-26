#ifndef COMPILER_V2_COMMON_MODULE_HDR_
#define COMPILER_V2_COMMON_MODULE_HDR_

#include "common/ast.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/token.h"
#include "common/types.h"

#include <cstdint>
#include <unordered_map>

namespace common {

    class Module {
      public:
        Module() = default;

        Type add(BuiltinType type) {
            Type result{.id = TypeID{current_id_}};
            ++current_id_;
            type_to_info_[result.id] = type;
            types_[result.id] = type.traits;
            name_to_type_[type.name] = result.id;

            return result;
        }

        bool add(Function func) {
            if (name_to_type_.contains(func.name)) {
                return false;
            }
            return name_to_function_.insert(std::pair<LiteralID, FunctionID>{func.name, func.id}).second;
        }

        std::optional<BuiltinType> get_builtin(TypeID type) const {
            auto it = type_to_info_.find(type);
            if (it == type_to_info_.end()) {
                return {};
            }

            return it->second;
        }

        Type get_type(IdentifierID name) const {
            auto it = name_to_type_.find(name);
            return it == name_to_type_.end() ? Type{} : Type{.id = it->second};
        }

        FunctionID get_function(IdentifierID name) const {
            auto it = name_to_function_.find(name);
            return it == name_to_function_.end() ? FunctionID{g_invalid_id} : it->second;
        }

        TypeTraits get_traits(TypeID type) const {
            return types_.at(type);
        }

        common::FunctionID entrypoint() const { return entrypoint_; }
        void set_entrypoint(common::FunctionID id) { entrypoint_ = id; }

      private:
        std::unordered_map<TypeID, TypeTraits> types_;
        std::unordered_map<TypeID, BuiltinType> type_to_info_;
        std::unordered_map<IdentifierID, TypeID> name_to_type_;
        std::unordered_map<IdentifierID, FunctionID> name_to_function_;
        std::unordered_map<ExpressionID, Type> expression_types_;

        FunctionID entrypoint_ = FunctionID{g_invalid_id};
        uint64_t current_id_ = 0;
    };
} // namespace common

#endif
