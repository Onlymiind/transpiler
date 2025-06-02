#ifndef COMPILER_V2_COMMON_TYPES_HDR_
#define COMPILER_V2_COMMON_TYPES_HDR_

#include "common/base_classes.h"
#include "common/declarations.h"
#include "common/util.h"

#include <cstddef>
#include <unordered_map>

namespace common {
    enum class BuiltinTypes {
        BOOL,
        UINT,
        FLOAT,
    };

    class PrimitiveType final : public Type {
      public:
        PrimitiveType(IdentifierID name, BuiltinTypes type, TypeTraits traits,
                      size_t size)
            : Type(static_kind(), traits, size), name_(name), type_(type) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(PrimitiveType, TypeKind,
                                                     TypeKind::PRIMITIVE,
                                                     default)

        IdentifierID name() const noexcept { return name_; }
        BuiltinTypes type() const noexcept { return type_; }

      private:
        IdentifierID name_;
        BuiltinTypes type_{};
    };

    class ArrayType final : public Type {
      public:
        ArrayType(const Type &element_type, size_t count)
            : Type(static_kind(), TypeTraits::INDEXABLE,
                   element_type.size() * count),
              count_(count), element_type_(&element_type) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(ArrayType, TypeKind,
                                                     TypeKind::ARRAY, default)

        size_t count() const noexcept { return count_; }
        const Type *element_type() const noexcept { return element_type_; }

      private:
        size_t count_ = 0;
        const Type *element_type_ = nullptr;
    };

    // FIXME: representing each pointer type as a separate Type instance seems
    // extremely excessive, as the only information required on per-instance
    // basis is pointee_type_ (or something like base type and indirection
    // level)
    class PointerType final : public Type {
      public:
        explicit PointerType(const Type &pointee)
            : Type(static_kind(), TypeTraits::DEREFERENCABLE, g_pointer_size),
              pointee_type_(&pointee) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(PointerType, TypeKind,
                                                     TypeKind::POINTER, default)
        const Type *pointee_type() const noexcept { return pointee_type_; }

        static PointerType make_nullptr_type() { return PointerType(); }

        bool is_nullptr() const noexcept { return pointee_type_ == nullptr; }

      private:
        PointerType() : Type(static_kind(), TypeTraits::NONE, g_pointer_size) {}

        const Type *pointee_type_ = nullptr;
    };

    class StructType final : public Type {
      public:
        StructType(IdentifierID name)
            : Type(static_kind(), TypeTraits::NONE, 0) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(StructType, TypeKind,
                                                     TypeKind::STRUCT, delete);

        bool add_fields(std::vector<Variable> &&fields);

        bool is_defined() const noexcept { return is_defined_; }

        IdentifierID name() const noexcept { return name_; }
        const std::vector<Variable> &fields() const noexcept {
            return fields_;
        };

        bool has_field(IdentifierID name) const {
            return name_to_field_.contains(name);
        }
        const Variable *get_field(IdentifierID name) const {
            auto it = name_to_field_.find(name);
            return it == name_to_field_.end() ? nullptr : it->second;
        }

      private:
        IdentifierID name_;
        std::vector<Variable> fields_;
        std::unordered_map<IdentifierID, const Variable *> name_to_field_;
        bool is_defined_ = false;
    };

} // namespace common

#endif
