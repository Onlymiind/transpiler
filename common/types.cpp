#include "common/types.h"
#include "common/base_classes.h"
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <stdexcept>
#include <vector>

namespace common {

    bool StructType::add_fields(std::vector<Variable> &&fields) {
        if (is_defined_) {
            return false;
        }
        is_defined_ = true;
        if (fields.empty()) {
            set_size(0);
            set_alignment(0);
            return true;
        }

        size_t size = 0;
        size_t max_align = 0;
        for (const auto &var : fields) {
            if (!var.type) {
                return false;
            }
            size_t align = var.type->alignment();
            max_align = std::max(max_align, align);

            if (size % align != 0) {
                size += align - size % align;
            }

            auto &field = fields_.emplace_back(Field{.name = var.name,
                                                     .type = var.type,
                                                     .offset = size,
                                                     .pos = var.pos});
            auto [it, success] = name_to_field_.try_emplace(field.name, &field);
            if (!success) {
                return false;
            }
            size += var.type->size();
        }

        if (size % max_align != 0) {
            size += max_align - size % max_align;
        }

        set_size(size);
        set_alignment(max_align);

        return true;
    }

    bool StructType::add_fields(std::vector<Field> &&fields) {
        if (is_defined_) {
            return false;
        }
        is_defined_ = true;
        if (fields.empty()) {
            set_size(0);
            set_alignment(0);
            return true;
        }

        size_t size = 0;
        size_t max_align = 0;
        for (auto &field : fields) {
            if (!field.type) {
                return false;
            }
            size_t align = field.type->alignment();
            max_align = std::max(max_align, align);

            if (size % align != 0) {
                size += align - size % align;
            }

            field.offset = size;
            auto &f = fields_.emplace_back(std::move(field));
            auto [it, success] = name_to_field_.try_emplace(f.name, &f);
            if (!success) {
                return false;
            }
            size += f.type->size();
        }

        if (size % max_align != 0) {
            size += max_align - size % max_align;
        }

        set_size(size);
        set_alignment(max_align);

        return true;
    }

    StructType StructType::make_slice_type(const Type *size_type,
                                           const Type *element_ptr_type,
                                           IdentifierID cap_name,
                                           IdentifierID size_name,
                                           IdentifierID data_name) {
        StructType record{TypeTraits::INDEXABLE, 3 * sizeof(uint64_t)};
        if (!record.add_fields(std::vector<Field>{
                Field{
                    .name = cap_name,
                    .type = size_type,
                    .flags = FieldFlags::READONLY,
                },
                Field{
                    .name = size_name,
                    .type = size_type,
                    .flags = FieldFlags::READONLY,
                },
                Field{
                    .name = data_name,
                    .type = element_ptr_type,
                    .flags = FieldFlags::HIDDEN,
                },
            })) {
            throw std::invalid_argument{
                "failed to initialize slice struct type"};
        }

        return record;
    }

} // namespace common
