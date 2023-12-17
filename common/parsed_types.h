#ifndef COMPILER_V2_COMMON_PARSED_TYPES_HDR_
#define COMPILER_V2_COMMON_PARSED_TYPES_HDR_

#include "common/base_classes.h"
#include "common/expression.h"
#include "common/util.h"

#include <cstdint>

namespace common {

    class ParsedNamedType final : public ParsedType {
      public:
        explicit ParsedNamedType(IdentifierID name, uint64_t indirection_level = 0)
            : ParsedType(static_kind(), indirection_level), name_(name) {}
        COMPILER_V2_DECLARE_SPECIAL_MEMBER_FUNCTIONS(ParsedNamedType, ParsedTypeKind, ParsedTypeKind::NAMED, default)

        IdentifierID name() const noexcept { return name_; }

      private:
        IdentifierID name_;
    };

    class ParsedErrorType final : public ParsedType {
      public:
        ParsedErrorType()
            : ParsedType(ParsedTypeKind::ERROR, 0) {}
    };

} // namespace common

#endif
