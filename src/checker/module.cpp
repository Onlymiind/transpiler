#include "checker/module.h"
#include "parser/expression.h"

namespace module {
    std::optional<ID> Module::get_type_id(const std::string& name) const {
        auto it = sym_table_.find(name);
        if(it == sym_table_.end()) {
            return {};
        }
        return it->second;
    }

    std::optional<ID> Module::get_var_id(const std::string& name) const {
        auto it = sym_table_.find(name);
        if(it == sym_table_.end() || get_kind(it->second) != IDKind::VARIABLE) {
            return {};
        }
        return it->second;
    }

    AliasInfo* Module::get_alias_info(ID id) {
        uint64_t index = get_index(id);
        if(id >= g_none_type || aliases_.size() <= index) {
            return nullptr;
        }
        return &aliases_[index];
    }

    StructInfo* Module::get_struct_info(ID id) {
        uint64_t index = get_index(id);
        if(id >= g_none_type || structs_.size() <= index) {
            return nullptr;
        }
        return &structs_[index];
    }

    ID Module::register_builtin(const std::string& name) {
        static size_t next_id = 0;
        ID id = make_id(IDKind::BUILTIN, next_id++);
        add_name(name, id);
        return id;
    }

    ID Module::register_variable(Variable var, const std::string& name) {
        ID id = make_id(IDKind::VARIABLE, variables_.size());
        auto& var_ref = variables_.emplace_back(std::move(var));
        add_name(name, var_ref.type);
        return id;
    }

    ID Module::register_function(FunctionInfo info, const std::string& name) {
        ID id = make_id(IDKind::FUNCTION, functions_.size());
        functions_.emplace_back(std::move(info));
        add_name(name, id);
        return id;
    }
}
