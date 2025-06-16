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
        type_to_mod_[nullptr].ptr = null.get();
        types_.emplace_back(std::move(null));
    }

    const PrimitiveType *
    Global::get_primitive(IdentifierID name) const noexcept {
        auto it = primitives_.find(name);
        return it == primitives_.end() ? nullptr : it->second;
    }

    const ArrayType *Global::get_array(size_t count, const Type *element_type) {
        if (!element_type || count == 0) {
            return nullptr;
        }
        ModifiedType &entry = type_to_mod_[element_type];
        if (entry.arrays.contains(count)) {
            return entry.arrays.at(count);
        }

        auto ptr = std::make_unique<common::ArrayType>(*element_type, count);
        const ArrayType *result = ptr.get();
        types_.emplace_back(std::move(ptr));

        entry.arrays[count] = result;
        return result;
    }

    const StructType *Global::get_slice(const Type *size_type,
                                        const Type *element_type,
                                        IdentifierID cap_name,
                                        IdentifierID len_name,
                                        IdentifierID data_name) {

        if (!size_type || !element_type) {
            return nullptr;
        }

        ModifiedType &entry = type_to_mod_[element_type];

        if (entry.slice) {
            return entry.slice;
        }

        auto ptr = std::make_unique<common::StructType>(
            StructType::make_slice_type(size_type, get_pointer(element_type),
                                        cap_name, len_name, data_name));
        entry.slice = ptr.get();
        types_.emplace_back(std::move(ptr));
        return entry.slice;
    }

    const PointerType *Global::get_pointer(const Type *to) {
        if (!to) {
            return type_to_mod_.at(nullptr).ptr;
        }

        ModifiedType &entry = type_to_mod_[to];
        if (entry.ptr) {
            return entry.ptr;
        }

        auto ptr = std::make_unique<common::PointerType>(*to);
        entry.ptr = ptr.get();
        types_.emplace_back(std::move(ptr));
        return entry.ptr;
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

    TypeStorage::TypeStorage(Global &&global) { import(std::move(global)); }

    const common::Type *
    TypeStorage::get_by_name(common::IdentifierID name) const {
        auto it = name_to_type_.find(name);
        return it == name_to_type_.end() ? nullptr : it->second;
    }

    const common::Type *TypeStorage::get_ptr(const common::Type *to) const {
        if (!to || !type_to_mod_.contains(to)) {
            return nullptr;
        }
        return type_to_mod_.at(to).ptr;
    }

    const common::Type *
    TypeStorage::get_slice(const common::Type *element) const {
        if (!element || !type_to_mod_.contains(element)) {
            return nullptr;
        }
        return type_to_mod_.at(element).slice;
    }

    const common::Type *TypeStorage::get_array(const common::Type *element,
                                               size_t size) const {
        if (!element || !type_to_mod_.contains(element)) {
            return nullptr;
        }

        const ModifiedType &mod = type_to_mod_.at(element);
        auto it = mod.arrays.find(size);
        return it == mod.arrays.end() ? nullptr : it->second;
    }

    const std::vector<std::unique_ptr<common::Type>> &
    TypeStorage::types() const noexcept {
        return types_;
    }

    void TypeStorage::import(Global &&global) {
        types_ = std::move(global.types());
        type_to_mod_ = std::move(global.type_mods());
    }

    void TypeStorage::import(Module &mod) {
        auto &structs = mod.get_structs();
        types_.reserve(structs.size());
        types_.insert(types_.end(), std::move_iterator(structs.begin()),
                      std::move_iterator(structs.end()));

        for (auto [name, type] : mod.named_types()) {
            name_to_type_[name] = type;
        }
    }
} // namespace common
