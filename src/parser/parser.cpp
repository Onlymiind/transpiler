#include "parser/parser.h"

#include <algorithm>
#include <stack>
#include <iostream>
#include <memory>

namespace parser {

    std::optional<size_t> find_in_current_scope(Tokens tokens, util::Category cat) {
        size_t scope{};
        for(size_t i = 0; i < tokens.size(); i++) {

            i = util::consume_scope(tokens, i, {util::Category::LBRACE, util::Category::RBRACE});
            if(i >= tokens.size()) {
                break;
            }

            if(scope == 0 && tokens[i].category == cat) {
                return i;
            }
        }

        return {};
    }

    std::optional<std::pair<util::Category, size_t>> find_in_current_scope(Tokens tokens, const std::unordered_set<util::Category>& categories) {
        std::unordered_map<util::Category, size_t> found;
        for(auto cat : categories) {
            auto idx = find_in_current_scope(tokens, cat);
            if(idx) {
                found[cat] = *idx;
            }
        }

        if(found.empty()) {
            return {};
        }

        auto it = std::min_element(found.begin(), found.end(), [](const auto& lhs, const auto& rhs) { return lhs.second < rhs.second; });

        return *it;
    }

    std::optional<size_t> first_not_comment(Tokens tokens) {
        auto it = std::find_if(tokens.begin(), tokens.end(), [](const auto& token){
            return token.category != util::Category::COMMENT;
        });

        if(it == tokens.end()) {
            return {};
        }

        return it - tokens.begin();
    }

    std::vector<util::Result<std::variant<TypeInfo, NamedField>>> split(Tokens tokens) {
        static const std::unordered_set<util::Category> decl_tokens {
            util::Category::TYPE,
            util::Category::FUNC,
            util::Category::VAR
        };

        std::vector<util::Result<std::variant<TypeInfo, NamedField>>> result;
        auto assert_only_comments = [&result](Tokens tokens) {
            if(tokens.size() == 0) {
                return;
            }
            auto pos = first_not_comment(tokens);
            if(pos) {
                result.emplace_back(util::Error{
                    .line = tokens[*pos].line,
                    .msg = "unexpected token"
                });
            }
        };

        auto unwrap = [] <typename T> (util::Result<T> res) -> util::Result<std::variant<TypeInfo, NamedField>>{
            if(auto ptr = std::get_if<util::Error>(&res)) {
                return *ptr;
            } else {
                return std::get<0>(res);
            }
        };

        auto decl_start = find_in_current_scope(tokens, decl_tokens);
        while(decl_start) {
            if(decl_start->second != 0) {
                assert_only_comments(tokens.subspan(0, decl_start->second));
                tokens = tokens.subspan(decl_start->second);
            }

            if(tokens.size() < 3) {
                result.emplace_back(util::Error{
                    .line = tokens[0].line,
                    .msg = "invalid declaration"
                });
                return result;
            }

            std::unordered_set<util::Category> decl_end_categories {
                util::Category::SEMICOLON
            };

            if((decl_start->first != util::Category::VAR) && (tokens[2].category != util::Category::IDENTIFIER)) {

                decl_end_categories.insert(util::Category::RBRACE);
            }

            auto decl_end = find_in_current_scope(tokens, decl_end_categories);
            if(!decl_end) {
                result.emplace_back(util::Error{
                    .line = tokens[0].line,
                    .msg = "declaration does not end"
                });
                return result;
            }

            switch(decl_start->first) {
            case util::Category::TYPE:
                result.emplace_back(unwrap(parse_type_declaration(tokens.subspan(0, decl_end->second + 1))));
                break;
            case util::Category::FUNC:
                //decl.type = Type_::FUNCTION;
                break;
            case util::Category::VAR:
                result.emplace_back(unwrap(parse_variable(tokens.subspan(1, decl_end->second - 1))));
                break;
            }

            tokens = tokens.subspan(decl_end->second + 1);
            decl_start = find_in_current_scope(tokens, decl_tokens);
        }

        assert_only_comments(tokens);
        return result;
    }

    std::vector<util::Result<GenericParam>> parse_generic_params(Tokens& tokens) {
        size_t end = util::consume_scope(tokens, 0, {util::Category::LESS, util::Category::GREATER});
        if(end <= 1) {
            return {};
        }

        std::vector<util::Result<GenericParam>> result;
        for(size_t i = 1; i < end; i++) {
            switch(tokens[i].category) {
            case util::Category::COMMA:
                continue;
            case util::Category::IDENTIFIER:
                result.emplace_back(GenericParam{tokens[i].value});
                break;
            default:
                result.push_back(util::Error{tokens[i].line, "generic arguments: expected comma separated list of identifiers"});
            }
        }
        tokens = tokens.subspan(end + 1);
        return result;
    }

    util::Result<NamedField> parse_variable(Tokens tokens) {
        if(tokens.empty()) {
            return util::Error{0, "empty field declaration, possibly a bug in a compiler"};
        }
        if(tokens.size() < 3) {
            return util::Error{tokens[0].line, "field declaration: expected \"field_name : field_type\""};
        }

        NamedField result;
        auto& [name, decl] = result;

        if(tokens[0].category != util::Category::IDENTIFIER) {
            return util::Error{tokens[0].line, "field declaration: expected field_name, got: " + std::string{util::to_string(tokens[0].category)}};
        }
        name = tokens[0].value;

        if(tokens[1].category != util::Category::COLON) {
            return util::Error{tokens[1].line, "field declaration: expected \":\", got: " + std::string{util::to_string(tokens[1].category)}};
        } else if(tokens[2].category != util::Category::IDENTIFIER) {
            return util::Error{tokens[2].line, "field declaration: expected type_name, got: " + std::string{util::to_string(tokens[2].category)}};
        }

        decl.type.name = tokens[2].value;
        auto rest = tokens.subspan(3);
        auto generic_params = parse_generic_params(rest);
        decl.type.generic_params.reserve(generic_params.size());
        for(auto& p : generic_params) {
            if(auto ptr = std::get_if<util::Error>(&p)) {
                return *ptr;
            }

            decl.type.generic_params.push_back(std::get<GenericParam>(p));
        }

        if(!rest.empty()) {
            return util::Error{rest[0].line, "field declaration: unexpected end"};
        }


        return result;
    }

    util::Result<TypeInfo> parse_alias(Declaration decl, Tokens definition) {
        static const std::unordered_map<util::Category, DeclarationType> definition_to_type{
            {util::Category::IDENTIFIER, DeclarationType::UNKNOWN},
            {util::Category::TUPLE, DeclarationType::TUPLE},
            {util::Category::UNION, DeclarationType::UNION}
        };

        TypeInfo result {.declaration = std::move(decl)};
        Declaration underlying_type;

        underlying_type.type = definition_to_type.at(definition[0].category);
        if(definition[0].category == util::Category::IDENTIFIER) {
            underlying_type.name = definition[0].value;
        }

        auto rest = definition.subspan(1);
        auto generic_params = parse_generic_params(rest);
        underlying_type.generic_params.reserve(generic_params.size());
        for(auto& param : generic_params) {
            if(auto ptr = std::get_if<util::Error>(&param)) {
                return *ptr;
            }

            underlying_type.generic_params.push_back(std::get<GenericParam>(param));
        }
        
        result.definition = std::move(underlying_type);
        return result;
    }

    util::Result<TypeInfo> parse_struct(Declaration decl, Tokens definition) {
        if(definition.empty()) {
            return util::Error{0, "struct declaration: empty definition, possibly a bug in a compiler"};
        }

        TypeInfo result{std::move(decl)};
        result.definition = StructInfo{};
        auto& struct_definition = std::get<StructInfo>(result.definition);
        if(definition.size() == 1 && definition[0].category == util::Category::SEMICOLON) {
            return result;
        }
        if(definition.size() == 2 && definition[0].category == util::Category::LBRACE && definition[1].category == util::Category::RBRACE) {
            return result;
        }
        if(definition[0].category == util::Category::LBRACE) {
            definition = definition.subspan(1);
        }

        auto next_definition = [&definition]() -> Tokens {
            auto it = std::find_if(definition.begin(), definition.end(), [](const util::Token& token) { return token.category == util::Category::SEMICOLON; });
            if(it == definition.end()) {
                return Tokens{};
            }

            // not + 1 to ignore semicolon itself
            size_t end = it - definition.begin();
            Tokens result = definition.subspan(0, end);
            definition = definition.subspan(end + 1);
            return result;
        };

        for(Tokens field_def = next_definition(); !field_def.empty(); field_def = next_definition()) {
            auto field = parse_variable(field_def);
            if(auto ptr = std::get_if<util::Error>(&field)) {
                return *ptr;
            }

            struct_definition.fields.insert(std::get<NamedField>(field));
        }
        
        return result;
    }

    util::Result<TypeInfo> parse_type_declaration(Tokens decl) {
        static const std::unordered_map<util::Category, DeclarationType> definition_to_type{
            {util::Category::IDENTIFIER, DeclarationType::ALIAS},
            {util::Category::INTERFACE, DeclarationType::INTERFACE},
            {util::Category::STRUCT, DeclarationType::STRUCT},
            {util::Category::TUPLE, DeclarationType::ALIAS},
            {util::Category::UNION, DeclarationType::ALIAS},
            {util::Category::ENUM, DeclarationType::ENUM}
        };

        Declaration info;
        decl = decl.subspan(1);
        if(decl.empty() || decl[0].category != util::Category::IDENTIFIER) {
            return util::Error{decl[0].line, "type declaration: expected a type name after a \"type\" keyword"};
        }

        info.name = decl[0].value;
        decl = decl.subspan(1);

        auto generic_params = parse_generic_params(decl);
        info.generic_params.reserve(generic_params.size());
        for(auto& p : generic_params) {
            if(std::holds_alternative<util::Error>(p)) {
                return std::get<util::Error>(p);
            }
            info.generic_params.push_back(std::get<GenericParam>(p));
        }
        if(decl.empty()) {
            return util::Error{decl[0].line, "type declaration: expected type definition after a type name"};
        }

        auto it = definition_to_type.find(decl[0].category);
        if(it == definition_to_type.end()) {
            return util::Error{
                decl[0].line,
                "type declaration: expected one of the: type_name, tuple, union, enum, struct, got " + std::string{util::to_string(decl[0].category)}
            };
        }
        info.type = it->second;

        if(info.type != DeclarationType::ALIAS) {
            decl = decl.subspan(1);
        }

        switch(info.type) {
        case DeclarationType::ALIAS:
            return parse_alias(std::move(info), decl);
        case DeclarationType::STRUCT:
            return parse_struct(std::move(info), decl);
        }

        return TypeInfo{.declaration = std::move(info), .definition = decl};
    }

    void parse(std::vector<util::Token> tokens) {
        auto decls_and_errs = split(tokens);
        std::vector<util::Error> errs;
        std::vector<std::variant<TypeInfo, NamedField>> decls;

        for(auto& v : decls_and_errs) {
            if(auto ptr = std::get_if<util::Error>(&v)) {
                errs.emplace_back(std::move(*ptr));
            } else {
                decls.emplace_back(std::move(std::get<std::variant<TypeInfo, NamedField>>(v)));
            }
        }

        std::cout << "Done\n";
    }
}
