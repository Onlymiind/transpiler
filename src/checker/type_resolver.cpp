#include "checker/type_resolver.h"

#include <string_view>
#include <unordered_map>
#include <vector>

#include "checker/checker.h"
#include "checker/module.h"
#include "parser/declaration.h"


namespace type_resolver {

    std::string make_name(const parser::Declaration& decl) {
        if(!decl.name.empty()) {
            return decl.name;
        }

        std::string name;
        switch (decl.type) {
        case parser::DeclarationType::UNION:
            name += "union";
            break;
        case parser::DeclarationType::TUPLE:
            name += "tuple";
            break;
        case parser::DeclarationType::FUNCTION:
            name += "func";
            break;
        }

        if(!decl.generic_params.empty()) {
            name.push_back('<');
        }
        for(size_t i = 0; i < decl.generic_params.size(); i++) {
            if (i != 0) {
                name.push_back(',');
            }
            name += decl.generic_params[i].name;
        }
        if(!decl.generic_params.empty()) {
            name.push_back('>');
        }

        return name;
    }

    module::Module resolve_types(parser::File file, std::vector<module::TypeInfo> predefined_types) {
        module::Module result;

        for(auto& info : predefined_types) {
            result.register_builtin(std::move(info));
        }

        //First pass: register all the types
        //Their definitions will be filled in the second pass
        for(auto& decl : file.types) {
            switch(decl.declaration->type) {
            case parser::DeclarationType::ALIAS:
                result.register_alias(module::AliasInfo{}, module::TypeInfo{.name = decl.declaration->name, .category = module::TypeCategory::ALIAS});
                break;
            case parser::DeclarationType::STRUCT: {
                module::StructInfo info;
                for(auto& field : decl.declaration->fields) {
                    info.fields.emplace_back(module::Variable{field.first});
                }
                result.register_struct(std::move(info), module::TypeInfo{.name = decl.declaration->name, .category = module::TypeCategory::STRUCT});
                break;
            }
            case parser::DeclarationType::FUNCTION:
                break;
            case parser::DeclarationType::TUPLE:
                break;
            case parser::DeclarationType::UNION:
                break;
            }
        }

        //Second pass
        for(auto& decl : file.types) {
            module::TypeID id = *result.get_type_id(decl.declaration->name);
            switch(decl.declaration->type) {
            case parser::DeclarationType::ALIAS:{
                module::AliasInfo* info = result.get_alias_info(id);
                auto maybe_id = result.get_type_id(decl.declaration->underlying_type->name);
                if(!maybe_id) {
                    throw checker::CheckerError("unknown type: " + decl.declaration->underlying_type->name);
                }
                info->underlying_type = *maybe_id;
                break;
            }
            case parser::DeclarationType::STRUCT: {
                module::StructInfo* info = result.get_struct_info(id);
                for(size_t i = 0; i < info->fields.size(); ++i) {
                    auto maybe_id = result.get_type_id(decl.declaration->fields[i].second->name);
                    if(!maybe_id) {
                        throw checker::CheckerError("unknown type: " + decl.declaration->fields[i].first);
                    }
                    info->fields[i].type = *maybe_id;
                }
                break;
            }
            case parser::DeclarationType::FUNCTION:
                break;
            case parser::DeclarationType::TUPLE:
                break;
            case parser::DeclarationType::UNION:
                break;
            }
        }

        return result;
    }
}
