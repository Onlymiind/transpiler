#include "checker/checker.h"
#include "parser/parser.h"

namespace checker {

    Checker::Checker(parser::File&& file, std::ostream* err_out)
        : file_(std::move(file)), err_out_(err_out)
    {
        for(size_t i = 0; i < file_.types.size(); i++) {
            const auto& decl = file_.types[i];
            if(name_to_type_.contains(decl.declaration->name)) {
                if(err_out_) {
                    *err_out_ << "redeclaration: " << decl.declaration->name << std::endl;
                }
                continue;
            }

            name_to_type_[decl.declaration->name] = i;
        }
    }

    void Checker::check_function(TypeID id) {
        const auto& info = type_info(id);

        for(const auto& [param_name, type] : info.fields) {
            if(!type_id_by_name(type->name)) {
                error("in function ", info.name, "unknown argument type: ", type->name);
            }
        }

        if(info.return_type && !type_id_by_name(info.return_type->name)) {
            error("in function ", info.name, "unknown return type: ", info.return_type->name);
        }
    }
}
