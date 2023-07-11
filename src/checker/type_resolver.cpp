#include "checker/type_resolver.h"

#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "checker/module.h"
#include "parser/declaration.h"
#include "parser/expression.h"
#include "util/arena.h"
#include "util/error_handler.h"
#include "util/util.h"


namespace type_resolver {

    util::StringConstRef make_name(const parser::Declaration& decl, util::StringAllocator& alloc) {
        if(decl.name) {
            return decl.name;
        }

        auto push_ref = [](std::string& str, util::StringConstRef ref) {
            size_t pos = str.size();
            str.resize(pos + sizeof(util::StringConstRef));
            auto& val = *((util::StringConstRef*)(&str[pos]));
            val = ref;
        };

        std::string name = decl.type == parser::DeclarationType::FUNCTION ? "0" : "1";
        name.reserve(sizeof(util::StringConstRef) * (decl.fields.size() + (decl.return_type != nullptr)));

        if(decl.return_type) {
            push_ref(name, make_name(*decl.return_type, alloc));
        }

        //TODO: should names of struct's fields be part of a type?
        for(const auto& [field_name, field] : decl.fields) {
            push_ref(name, make_name(*field, alloc));
        }

        return alloc.allocate(std::move(name));
    }

    module::ID get_id(module::Module& module, util::StringConstRef name, util::ErrorHandler err) {
        auto maybe_id = module.get_type_id(name);
        if(!maybe_id) {
            if(name) {
                err.checker_error(0, "unknown type: ", *name);
            } else {
                err.checker_error(0, "unknown unnamed type");
            }
        }

        return *maybe_id;
    }

    util::StringConstRef register_unnamed(module::Module& module, const parser::Declaration& decl, util::StringAllocator& alloc, util::ErrorHandler& err) {
        if(decl.name) {
            return decl.name;
        }

        if(decl.type == parser::DeclarationType::FUNCTION) {
            module::FunctionInfo info;
            if(decl.return_type){
                info.return_type = get_id(module, register_unnamed(module, *decl.return_type, alloc, err), err);
            }
            info.args.reserve(decl.fields.size());
            for(auto& param : decl.fields) {
                info.args.emplace_back(module::Field{
                    .name = param.first, 
                    .type = get_id(module, register_unnamed(module, *param.second, alloc, err), err)
                });
            }

            auto name = make_name(decl, alloc);
            module.register_function(std::move(info), name);
            return name;
        } else if(decl.type == parser::DeclarationType::STRUCT) {
            module::StructInfo info;
            info.fields.reserve(decl.fields.size());
            for(auto [name, decl_ptr] : decl.fields) {
                info.fields.emplace_back(module::Field{
                    .name = name,
                    .type = get_id(module, register_unnamed(module, *decl_ptr, alloc, err), err),
                });
            }
            auto name = make_name(decl, alloc);
            module.register_struct(std::move(info), name);
            return name;
        } else {
            err.checker_error(0, "only anonymous structs and functions supported");
        }
    }

    module::Module resolve_types(parser::File file, std::vector<std::string> predefined_types, util::StringAllocator& alloc, util::ErrorHandler& err) {
        module::Module result{err};

        for(auto& info : predefined_types) {
            result.register_builtin(alloc.allocate(std::move(info)));
        }

        //First pass: register all user-declared types
        //Their definitions will be filled in the second pass
        std::unordered_set<size_t> err_types;
        for(size_t i = 0; i < file.types.size(); ++i) {
            auto& decl = file.types[i];
            try {
                switch(decl.declaration->type) {
                case parser::DeclarationType::ALIAS:
                    result.register_alias(module::AliasInfo{}, decl.declaration->name);
                    break;
                case parser::DeclarationType::STRUCT: {
                    module::StructInfo info;
                    info.fields.reserve(decl.declaration->fields.size());
                    for(auto& field : decl.declaration->fields) {
                        info.fields.emplace_back(module::Field{field.first});
                    }
                    result.register_struct(std::move(info), decl.declaration->name);
                    break;
                }
                default:
                    break;
                }
            } catch(const util::CheckerError&) {
                err_types.emplace(i);
            }
        }

        auto reg_anon = [&result, &alloc, &err] (const parser::Declaration* decl) {
            return register_unnamed(result, *decl, alloc, err);
        };

        //Second pass
        //TODO: pointers, optionals etc.
        for(size_t i = 0; i < file.types.size(); ++i) {
            if(err_types.contains(i)) {
                continue;
            }
            auto& decl = file.types[i];
            module::ID id = *result.get_type_id(decl.declaration->name);
            switch(decl.declaration->type) {
            case parser::DeclarationType::ALIAS:{
                module::AliasInfo* info = result.get_alias_info(id);
                info->underlying_type = get_id(result, reg_anon(decl.declaration->underlying_type), err);
                break;
            }
            case parser::DeclarationType::STRUCT: {
                module::StructInfo* info = result.get_struct_info(id);
                for(size_t i = 0; i < info->fields.size(); ++i) {
                    info->fields[i].type = get_id(result, reg_anon(decl.declaration->fields[i].second), err);
                }
                break;
            }
            case parser::DeclarationType::FUNCTION: {
                module::FunctionInfo info{};
                if(decl.declaration->return_type){
                    info.return_type = get_id(result, reg_anon(decl.declaration->return_type), err);
                }
                info.args.reserve(decl.declaration->fields.size());
                for(auto& param : decl.declaration->fields) {
                    info.args.emplace_back(module::Field{.name = param.first, .type = get_id(result, reg_anon(param.second), err)});
                }

                result.register_function(std::move(info), decl.declaration->name);
                break;
            }
            default:
                //TODO: print declaration type as string
                err.checker_error(0, "unknown declaration type", int(decl.declaration->type));
                break;
            }
        }

        return result;
    }

    module::Expression* resolve_expression(const module::Module& module, parser::Expression expr, util::ErrorHandler& err) {
        if(expr.expr.is<types::Token>()) {

        } else if(expr.expr.is<parser::Expr>()) {

        } else if(expr.expr.is<parser::FunctionCall>()) {
            err.checker_error(0, "function calls not supported");
        }
    }
}
