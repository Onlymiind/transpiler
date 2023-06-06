#include "checker/module.h"
#include "parser/expression.h"

namespace module {
    std::optional<TypeID> Module::get_type_id(std::string_view name) const {
        auto it = name_to_type_id_.find(name);
        if(it == name_to_type_id_.end()) {
            return {};
        }
        return it->second;
    }

    //std::optional<TypeID> Module::type_by_decl(const parser::Declaration& decl) const;

    std::optional<VariableID> Module::get_var_id(std::string_view name) const {
        auto it = name_to_var_id_.find(name);
        if(it == name_to_var_id_.end()) {
            return {};
        }
        return it->second;
    }

    TypeInfo* Module::get_info(TypeID id) {
        auto it = id_to_info_.find(id);
        if(it == id_to_info_.end()) {
            return nullptr;
        }
        return &it->second;
    }

    AliasInfo* Module::get_alias_info(TypeID id) {
        if(id >= g_none_type || aliases_.size() <= id) {
            return nullptr;
        }
        return &aliases_[id];
    }

    //StructInfo* Module::get_struct_info(TypeID id);
    //FunctionInfo* Module::get_funtion_info(TypeID id);

    const TypeInfo* Module::get_info(TypeID id) const {
        auto it = id_to_info_.find(id);
        if(it == id_to_info_.end()) {
            return nullptr;
        }
        return &it->second;
    }

    const AliasInfo* Module::get_alias_info(TypeID id) const {
        if(id >= g_none_type || aliases_.size() <= id) {
            return nullptr;
        }
        return &aliases_[id];
    }

    const StructInfo* Module::get_struct_info(TypeID id) const {
        if(id >= g_none_type || structs_.size() <= id) {
            return nullptr;
        }
        return &structs_[id];
    }

    const FunctionInfo* Module::get_function_info(TypeID id) const {
        if(id >= g_none_type || functions_.size() <= id) {
            return nullptr;
        }
        return &functions_[id];
    }

    bool Module::has_action_support(TypeID id, parser::ActionType action) const {
        using ac = parser::ActionType;
        switch(action) {
        case ac::ADD:
        case ac::SUB:
        case ac::MUL:
        case ac::DIV:
        case ac::REM:
        case ac::NEGATE:
            return is_numeric(id);
        case ac::AND:
        case ac::NOT:
        case ac::OR:
            return is_boolean(id);
        case ac::BAND:
        case ac::BOR:
        case ac::INV:
        case ac::LSHIFT:
        case ac::RSHIFT:
        case ac::XOR:
            return is_integral(id);
        case ac::NONE: return true;
        default: return false;
        }
    }

    //bool Module::is_builtin(TypeID type) const;

    bool Module::is_numeric(TypeID type) const {
        auto info = get_info(type);
        return info && (info->properties == TypeProperties::INTEGRAL || info->properties == TypeProperties::FLOATING_POINT);
    }

    bool Module::is_integral(TypeID type) const {
        auto info = get_info(type);
        return info && info->properties == TypeProperties::INTEGRAL;
    }

    bool Module::is_floating_point(TypeID type) const {
        auto info = get_info(type);
        return info && info->properties == TypeProperties::FLOATING_POINT;
    }

    bool Module::is_boolean(TypeID type) const {
        auto info = get_info(type);
        return info && info->properties == TypeProperties::BOOLEAN;
    }

    bool Module::is_callable(TypeID type) const {
        auto info = get_info(type);
        return info && info->properties == TypeProperties::CALLABLE;
    }

    //TypeID Module::instantiate_union(std::vector<TypeID> variants);
    //TypeID Module::instantiate_tuple(std::vector<TypeID> members);

    VariableID Module::register_variable(Variable var) {
        VariableID id = next_id_++;
        auto& ref = variables_.emplace_back(std::move(var));
        name_to_var_id_[ref.name] = id;
        return id;
    }
}
