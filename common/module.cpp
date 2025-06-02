#include "common/module.h"
#include "common/base_classes.h"
#include "common/util.h"

namespace common {

    StructType *Module::add_struct(StructType record) {
        IdentifierID name = record.name();

        if (named_types_.contains(name)) {
            return nullptr;
        }

        StructType *ptr = &structs_.emplace_back(std::move(record));
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
