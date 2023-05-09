#include "checker/type_resolver.h"

#include <string_view>
#include <unordered_map>
#include <vector>

#include "checker/module.h"


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

    struct Node {
        
        std::vector<Node*> dependencies;
    };

    module::Module resolve_types(parser::File file, std::vector<module::TypeInfo> predefined_types) {
        std::unordered_map<std::string, Node> name_to_node;
    }
}
