#include "checker/type_resolver.h"

#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "checker/module.h"
#include "parser/declaration.h"
#include "parser/expression.h"
#include "util/error_handler.h"
#include "util/util.h"


namespace type_resolver {

    std::string make_name(const parser::Declaration& decl) {
        static uint64_t id = 0;
        return std::to_string(id++);
    }

    module::Module resolve_types(parser::File file, std::vector<std::string> predefined_types, util::ErrorHandler& err) {
        module::Module result{err};

        for(auto& info : predefined_types) {
            result.register_builtin(std::move(info));
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

        auto get_id = [&result](const std::string& name) -> module::ID {
            auto maybe_id = result.get_type_id(name);
            if(!maybe_id) {
                throw util::CheckerError("unknown type: " + name);
            }

            return *maybe_id;
        };

        //Second pass
        //TODO: anonymous types, pointers, optionals etc.
        for(size_t i = 0; i < file.types.size(); ++i) {
            if(err_types.contains(i)) {
                continue;
            }
            auto& decl = file.types[i];
            module::ID id = *result.get_type_id(decl.declaration->name);
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
                module::FunctionInfo info{};
                if(decl.declaration->return_type){
                    info.return_type = get_id(decl.declaration->return_type->name);
                }
                info.args.reserve(decl.declaration->fields.size());
                for(auto& param : decl.declaration->fields) {
                    info.args.emplace_back(module::Field{.name = param.first, .type = get_id(param.second->name)});
                }

                result.register_function(std::move(info), decl.declaration->name);
                break;
            }
            default:
                //TODO: print declaration type as string
                err.checker_error("unknown declaration type", int(decl.declaration->type));
                break;
            }
        }

        return result;
    }

    module::Expression* resolve_expression(const module::Module& module, parser::Expression expr, util::ErrorHandler err) {
        if(expr.expr.is<types::Token>()) {

        } else if(expr.expr.is<parser::Expr>()) {

        } else if(expr.expr.is<parser::FunctionCall>()) {
            err.checker_error("function calls not supported");
        }
    }
}
