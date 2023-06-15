#include "checker/module.h"
#include "parser/expression.h"

namespace module {
    std::optional<TypeID> Module::get_type_id(std::string_view name) const {
        auto it = sym_table_.find(name);
        if(it == sym_table_.end()) {
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
        uint64_t index = get_index(id);
        if(id >= g_none_type || aliases_.size() <= index) {
            return nullptr;
        }
        return &aliases_[index];
    }

    //StructInfo* Module::get_struct_info(TypeID id);
    //FunctionInfo* Module::get_funtion_info(TypeID id);

    StructInfo* Module::get_struct_info(TypeID id) {
        uint64_t index = get_index(id);
        if(id >= g_none_type || structs_.size() <= index) {
            return nullptr;
        }
        return &structs_[index];
    }

    const TypeInfo* Module::get_info(TypeID id) const {
        auto it = id_to_info_.find(id);
        if(it == id_to_info_.end()) {
            return nullptr;
        }
        return &it->second;
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

    TypeID Module::register_builtin(TypeInfo info) {
        TypeID id = make_type_id(TypeKind::BUILTIN, next_id_++);
        add_name(info.name, id);
        id_to_info_[id] = std::move(info);
        return id;
    }

    //TypeID Module::instantiate_union(std::vector<TypeID> variants);
    //TypeID Module::instantiate_tuple(std::vector<TypeID> members);

    VariableID Module::register_variable(Variable var) {
        VariableID id = next_id_++;
        add_name(var.name, var.type);
        name_to_var_id_[var.name] = id;
        variables_.emplace_back(std::move(var));
        return id;
    }
}
