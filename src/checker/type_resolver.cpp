#include "checker/type_resolver.h"

#include <string_view>
#include <unordered_map>
#include <vector>

#include "checker/checker.h"
#include "checker/module.h"
#include "parser/declaration.h"


namespace type_resolver {

    std::string make_name(const parser::Declaration& decl) {
        if(!(decl.name.empty() || decl.type == parser::DeclarationType::FUNCTION)) {
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

        if(!decl.fields.empty()) {
            name += '(';
        }
        bool first = true;
        for(const auto& field : decl.fields) {
            name.reserve(field.second->name.size() + 1);
            if(!first) {
                name += ',';
            }
            name += field.second->name;
            first = false;
        }   
        if(!decl.fields.empty()) {
            name += ')';
        }
        if(decl.return_type) {
            name += decl.return_type->name;
        }

        return name;
    }

    module::Module resolve_types(parser::File file, std::vector<module::TypeInfo> predefined_types) {
        module::Module result;

        for(auto& info : predefined_types) {
            result.register_builtin(std::move(info));
        }

        //First pass: register all user-declared types
        //Their definitions will be filled in the second pass
        for(auto& decl : file.types) {
            switch(decl.declaration->type) {
            case parser::DeclarationType::ALIAS:
                result.register_alias(module::AliasInfo{}, module::TypeInfo{.name = decl.declaration->name});
                break;
            case parser::DeclarationType::STRUCT: {
                module::StructInfo info;
                info.fields.reserve(decl.declaration->fields.size());
                for(auto& field : decl.declaration->fields) {
                    info.fields.emplace_back(module::Variable{field.first});
                }
                result.register_struct(std::move(info), module::TypeInfo{.name = decl.declaration->name});
                break;
            }
            default:
                break;
            }
        }

        auto get_id = [&result](const std::string& name) -> module::TypeID {
            auto maybe_id = result.get_type_id(name);
            if(!maybe_id) {
                throw checker::CheckerError("unknown type: " + name);
            }

            return *maybe_id;
        };

        //Second pass
        //TODO: anonymous types, pointers, optionals etc.
        for(auto& decl : file.types) {
            module::TypeID id = *result.get_type_id(decl.declaration->name);
            switch(decl.declaration->type) {
            case parser::DeclarationType::ALIAS:{
                module::AliasInfo* info = result.get_alias_info(id);
                info->underlying_type = get_id(decl.declaration->underlying_type->name);
                break;
            }
            case parser::DeclarationType::STRUCT: {
                module::StructInfo* info = result.get_struct_info(id);
                for(size_t i = 0; i < info->fields.size(); ++i) {
                    info->fields[i].type = get_id(decl.declaration->fields[i].second->name);
                }
                break;
            }
            case parser::DeclarationType::FUNCTION: {
                module::FunctionInfo info;
                if(decl.declaration->return_type){
                    info.return_type = get_id(decl.declaration->return_type->name);
                }
                info.args.reserve(decl.declaration->fields.size());
                for(auto& param : decl.declaration->fields) {
                    info.args.emplace_back(module::Variable{.name = param.first, .type = get_id(param.second->name)});
                }

                result.register_function(std::move(info), module::TypeInfo{.name = make_name(*decl.declaration)});
                break;
            }
            case parser::DeclarationType::TUPLE:
                break;
            case parser::DeclarationType::UNION:
                break;
            }
        }

        return result;
    }
}
