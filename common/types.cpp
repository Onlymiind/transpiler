#include "common/types.h"

namespace common {

    bool StructType::add_fields(std::vector<Variable> &&fields) {
        if (is_defined_) {
            return false;
        }

        fields_ = std::move(fields);

        size_t size = 0;
        for (const auto &field : fields_) {
            if (name_to_field_.contains(field.name)) {
                return false;
            }

            size += field.type->size();
        }

        set_size(size);

        return true;
    }

} // namespace common
