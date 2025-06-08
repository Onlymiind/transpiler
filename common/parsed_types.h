#ifndef COMPILER_V2_COMMON_PARSED_TYPES_HDR_
#define COMPILER_V2_COMMON_PARSED_TYPES_HDR_

#include "common/base_classes.h"
#include "common/declarations.h"
#include "common/util.h"

#include <cstdint>
#include <memory>
#include <vector>

namespace common {

    class ParsedNamedType final : public ParsedType {
      public:
        explicit ParsedNamedType(IdentifierID name,
                                 uint64_t indirection_level = 0)
            : ParsedType(static_kind(), indirection_level), name_(name) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(ParsedNamedType,
                                                     ParsedTypeKind,
                                                     ParsedTypeKind::NAMED,
                                                     default)

        IdentifierID name() const noexcept { return name_; }

      private:
        IdentifierID name_;
    };

    class ParsedArrayType final : public ParsedType {
      public:
        ParsedArrayType(std::unique_ptr<Expression> &&size,
                        std::unique_ptr<ParsedType> &&base,
                        uint64_t indirection_level = 0)
            : ParsedType(static_kind(), indirection_level),
              size_(std::move(size)), element_type_(std::move(base)) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(ParsedArrayType,
                                                     ParsedTypeKind,
                                                     ParsedTypeKind::ARRAY,
                                                     delete)

        std::unique_ptr<Expression> &size() noexcept { return size_; }
        std::unique_ptr<ParsedType> &element_type() noexcept {
            return element_type_;
        }

        const Expression *size() const noexcept { return size_.get(); }
        const ParsedType *element_type() const noexcept {
            return element_type_.get();
        }

      private:
        std::unique_ptr<Expression> size_;
        std::unique_ptr<ParsedType> element_type_;
    };

    class ParsedSliceType final : public ParsedType {
      public:
        explicit ParsedSliceType(std::unique_ptr<ParsedType> &&element_type,
                                 uint64_t indirection_level = 0)
            : ParsedType(static_kind(), indirection_level),
              element_type_(std::move(element_type)) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(ParsedSliceType,
                                                     ParsedTypeKind,
                                                     ParsedTypeKind::SLICE,
                                                     delete)
        std::unique_ptr<ParsedType> &element_type() noexcept {
            return element_type_;
        }

        const ParsedType *element_type() const noexcept {
            return element_type_.get();
        }

      private:
        std::unique_ptr<ParsedType> element_type_;
    };

    class ParsedStructType final : public ParsedType {
      public:
        ParsedStructType(IdentifierID name, std::vector<Variable> &&fields,
                         TokenPos pos)
            : ParsedType(static_kind(), 0), name_(name),
              fields_(std::move(fields)), pos_(pos) {}

        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(ParsedStructType,
                                                     ParsedTypeKind,
                                                     ParsedTypeKind::STRUCT,
                                                     delete)
        IdentifierID name() const noexcept { return name_; }
        std::vector<Variable> &fields() noexcept { return fields_; }
        const std::vector<Variable> &fields() const noexcept { return fields_; }

        TokenPos pos() const noexcept { return pos_; }

      private:
        IdentifierID name_;
        std::vector<Variable> fields_;
        TokenPos pos_;
    };

    class ParsedErrorType final : public ParsedType {
      public:
        ParsedErrorType() : ParsedType(ParsedTypeKind::ERROR, 0) {}
    };

} // namespace common

#endif
