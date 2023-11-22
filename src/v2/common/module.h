#ifndef COMPILER_V2_COMMON_MODULE_HDR_
#define COMPILER_V2_COMMON_MODULE_HDR_

#include "common/expression.h"
#include "common/file.h"
#include "common/literals.h"
#include "common/token.h"
#include "common/types.h"

#include <cstdint>
#include <unordered_map>

namespace common {

    class Module {
      public:
        Module(File &&file) : file_(std::move(file)) {}

        Type add_type(BuiltinType type) {
            Type result{.id = Type::ID{current_id_}};
            ++current_id_;
            type_to_info_[result.id] = type;
            types_[result.id] = type.traits;
            name_to_type_[type.name] = result.id;

            return result;
        }

        std::optional<BuiltinType> get_builtin(Type type) const {
            auto it = type_to_info_.find(type.id);
            if (it == type_to_info_.end()) {
                return {};
            }

            return it->second;
        }

        Type get_type(common::Literals::ID name) const {
            auto it = name_to_type_.find(name);
            return it == name_to_type_.end() ? Type{} : Type{.id = it->second};
        }

        Type get_expression_type(Expression::ID expr) const {
            auto it = expression_types_.find(expr);
            if (it == expression_types_.end()) {
                return Type{};
            }

            return it->second;
        }

        TypeTraits get_traits(Type type) const {
            return types_.at(type.id);
        }

        void set_expression_type(Expression::ID expr, Type type) {
            expression_types_[expr] = type;
        }

        File &file() { return file_; }
        const File &file() const { return file_; }

      private:
        File file_;

        std::unordered_map<Type::ID, TypeTraits> types_;
        std::unordered_map<Type::ID, BuiltinType> type_to_info_;
        std::unordered_map<Literals::ID, Type::ID> name_to_type_;
        std::unordered_map<Expression::ID, Type> expression_types_;
        uint64_t current_id_ = 0;
    };
} // namespace common

#endif
