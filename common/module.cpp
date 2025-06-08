#include "common/module.h"
#include "common/base_classes.h"
#include "common/types.h"
#include "common/util.h"
#include <memory>
#include <valarray>

namespace common {
    Global::Global(std::vector<PrimitiveType> primitives) {
        for (auto &type : primitives) {
            std::unique_ptr<common::PrimitiveType>
                ptr = std::make_unique<common::PrimitiveType>(std::move(type));

            primitives_.emplace(ptr->name(), ptr.get());
            types_.emplace_back(std::move(ptr));
        }

        auto null = std::make_unique<common::PointerType>(
            PointerType::make_nullptr_type());
        pointers_.emplace(nullptr, null.get());
        types_.emplace_back(std::move(null));
    }

    const PrimitiveType *
    Global::get_primitive(IdentifierID name) const noexcept {
        auto it = primitives_.find(name);
        return it == primitives_.end() ? nullptr : it->second;
    }

    const ArrayType *Global::get_array(size_t count, const Type *element_type) {
        if (!element_type) {
            return nullptr;
        }
        std::pair<size_t, const Type *> key{count, element_type};
        if (arrays_.contains(key)) {
            return arrays_.at(key);
        }

        auto ptr = std::make_unique<common::ArrayType>(*element_type, count);
        const ArrayType *result = ptr.get();
        types_.emplace_back(std::move(ptr));

        arrays_.emplace(key, result);
        return result;
    }

    const StructType *Global::get_slice(const Type *size_type,
                                        const Type *element_type,
                                        IdentifierID cap_name,
                                        IdentifierID len_name,
                                        IdentifierID data_name) {

        if (slices_.contains(element_type)) {
            return slices_.at(element_type);
        }

        auto ptr = std::make_unique<common::StructType>(
            StructType::make_slice_type(size_type, get_pointer(element_type),
                                        cap_name, len_name, data_name));
        const StructType *result = ptr.get();
        types_.emplace_back(std::move(ptr));
        slices_.emplace(element_type, result);
        return result;
    }

    const PointerType *Global::get_pointer(const Type *to) {
        if (!to) {
            return pointers_.at(nullptr);
        } else if (pointers_.contains(to)) {
            return pointers_.at(to);
        }

        auto ptr = std::make_unique<common::PointerType>(*to);
        const PointerType *result = ptr.get();
        types_.emplace_back(std::move(ptr));
        pointers_.emplace(to, result);
        return result;
    }

    StructType *Module::add_struct(StructType record) {
        IdentifierID name = record.name();

        if (named_types_.contains(name)) {
            return nullptr;
        }

        StructType *ptr = structs_
                              .emplace_back(std::make_unique<StructType>(
                                  std::move(record)))
                              .get();
        named_types_[name] = ptr;
        return ptr;
    }

    bool Module::try_add_name(IdentifierID name) {
        bool result = defined_names_.insert(name).second;
        if (!result) {
            return result;
        }

        if (scopes_.empty()) {
            return result;
        }
        scopes_.top().push_back(name);
        return result;
    }

    void Module::pop_scope() {
        if (scopes_.empty()) {
            return;
        }

        for (IdentifierID name : scopes_.top()) {
            defined_names_.erase(name);
            // TODO: maybe should check what type of symbol the name refers to
            named_types_.erase(name);
            variables_.erase(name);
            functions_.erase(name);
        }
        scopes_.pop();
    }

    bool Module::add_type(IdentifierID name, const Type *type) {
        if (!try_add_name(name)) {
            return false;
        }
        named_types_[name] = type;
        return true;
    }

    bool Module::add_function(IdentifierID name, FunctionID function) {
        if (!try_add_name(name)) {
            return false;
        }
        functions_[name] = function;
        return true;
    }

    bool Module::add_variable(IdentifierID name, VariableID variable) {
        if (!try_add_name(name)) {
            return false;
        }
        variables_[name] = variable;
        return true;
    }
} // namespace common
