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

    void parse(std::vector<util::Token> tokens) {
        Parser p{std::move(tokens), &std::cout};
        auto decls = p.pasre();

        std::cout << "Done\n";
    }


    std::vector<std::variant<TypeInfo, NamedField>> Parser::pasre() {
        static const std::unordered_set<util::Category> decl_tokens {
            util::Category::TYPE,
            util::Category::FUNC,
            util::Category::VAR
        };

        std::vector<std::variant<TypeInfo, NamedField>> result;
        auto tokens = Tokens{tokens_};
        auto decl_start = find_in_current_scope(tokens, decl_tokens);
        while(decl_start) {
            if(decl_start->second != 0) {
                tokens = tokens.subspan(decl_start->second);
            }

            tokens = tokens.subspan(1);

            if(tokens.size() < 3) {
                errors_.emplace_back(tokens[0].line, "invalid declaration");
                return result;
            }

            std::unordered_set<util::Category> decl_end_categories {
                util::Category::SEMICOLON
            };

            if((decl_start->first != util::Category::VAR) && (tokens[1].category != util::Category::IDENTIFIER)) {

                decl_end_categories.insert(util::Category::RBRACE);
            }

            auto decl_end = find_in_current_scope(tokens, decl_end_categories);
            if(!decl_end) {
                errors_.emplace_back(tokens[0].line, "declaration does not end");
                return result;
            }

            try {   
                switch(decl_start->first) {
                case util::Category::TYPE:
                    result.emplace_back(parse_type_declaration(tokens.subspan(0, decl_end->second + 1)));
                    break;
                case util::Category::FUNC:
                    //decl.type = Type_::FUNCTION;
                    break;
                case util::Category::VAR:
                    result.emplace_back(parse_variable(tokens.subspan(0, decl_end->second)));
                    break;
                }
            } catch(const ParserError& e) {
                if(err_out_) {
                    *err_out_ << e.what() << '\n';
                }
            }

            tokens = tokens.subspan(decl_end->second + 1);
            decl_start = find_in_current_scope(tokens, decl_tokens);
        }

        return result;
    }

    std::vector<GenericParam> Parser::parse_generic_params(Tokens& tokens) {
        size_t end = util::consume_scope(tokens, 0, {util::Category::LESS, util::Category::GREATER});
        if(end <= 1) {
            return {};
        }

        std::vector<GenericParam> result;
        for(size_t i = 1; i < end; i++) {
            switch(tokens[i].category) {
            case util::Category::COMMA:
                continue;
            case util::Category::IDENTIFIER:
                result.emplace_back(GenericParam{tokens[i].value});
                break;
            default:
                error(tokens[i].line, "generic arguments: expected comma separated list of identifiers");
            }
        }
        tokens = tokens.subspan(end + 1);
        return result;
    }

    NamedField Parser::parse_variable(Tokens tokens) {
        if(tokens.empty()) {
            error(0, "empty field declaration, possibly a bug in a compiler");
        }
        if(tokens.size() < 3) {
            error(tokens[0].line, "field declaration: expected \"field_name : field_type\"");
        }

        NamedField result;
        auto& [name, decl] = result;

        if(tokens[0].category != util::Category::IDENTIFIER) {
            error(tokens[0].line, "field declaration: expected field_name, got: " + std::string{util::to_string(tokens[0].category)});
        }
        name = tokens[0].value;

        if(tokens[1].category != util::Category::COLON) {
            error(tokens[1].line, "field declaration: expected \":\", got: " + std::string{util::to_string(tokens[1].category)});
        }

        size_t line = tokens[1].line;
        tokens = tokens.subspan(2);
        decl.type = parse_type(tokens, line);

        if(!tokens.empty()) {
            error(tokens[0].line, "field declaration: unexpected end");
        }

        return result;
    }

    TypeInfo Parser::parse_function(Tokens decl) {
        error(0, "not implemented");
    }

    TypeInfo Parser::parse_type_declaration(Tokens decl) {
        static const std::unordered_map<util::Category, DeclarationType> definition_to_type{
            {util::Category::IDENTIFIER, DeclarationType::ALIAS},
            {util::Category::INTERFACE, DeclarationType::INTERFACE},
            {util::Category::STRUCT, DeclarationType::STRUCT},
            {util::Category::TUPLE, DeclarationType::ALIAS},
            {util::Category::UNION, DeclarationType::ALIAS},
            {util::Category::ENUM, DeclarationType::ENUM}
        };

        Declaration info;
        if(decl.empty() || decl[0].category != util::Category::IDENTIFIER) {
            error(decl[0].line, "type declaration: expected a type name after a \"type\" keyword");
        }

        info.name = decl[0].value;
        decl = decl.subspan(1);

        info.generic_params = parse_generic_params(decl);
        if(decl.empty()) {
            error(decl[0].line, "type declaration: expected type definition after a type name");
        }

        auto it = definition_to_type.find(decl[0].category);
        if(it == definition_to_type.end()) {
            error(decl[0].line,
                "type declaration: expected one of the: type_name, tuple, union, enum, struct, got " + std::string{util::to_string(decl[0].category)}
            );
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

    TypeInfo Parser::parse_alias(Declaration decl, Tokens definition) {
        return TypeInfo{
            .declaration = std::move(decl),
            .definition = parse_type(definition)
        };
    }

    TypeInfo Parser::parse_struct(Declaration decl, Tokens definition, size_t start_line) {
        if(definition.empty()) {
            error(start_line, "struct declaration: empty definition, possibly a bug in a compiler");
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
        if(definition[0].category != util::Category::LBRACE) {
            error(definition[0].line, "expected \"{\", got: " + std::string{util::to_string(definition[0].category)});
        }
        if(definition.back().category != util::Category::RBRACE) {
            error(definition.back().line, "expected \"}\", got: " + std::string{util::to_string(definition.back().category)});
        }

        definition = definition.subspan(1);

        auto next_field = [&definition]() -> Tokens {
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

        for(Tokens field_def = next_field(); !field_def.empty(); field_def = next_field()) {
            struct_definition.fields.insert(parse_variable(field_def));
        }

        if(definition.size() > 1) {
            error(definition[0].line, "unexpected end");
        }
        
        return result;
    }

    Declaration Parser::parse_type(Tokens& type, size_t start_line) {
        static const std::unordered_map<util::Category, DeclarationType> definition_to_type{
            {util::Category::IDENTIFIER, DeclarationType::UNKNOWN},
            {util::Category::TUPLE, DeclarationType::TUPLE},
            {util::Category::UNION, DeclarationType::UNION}
        };

        if(type.empty()) {
            error(start_line, "expected a type");
        }

        Declaration result;
        auto it = definition_to_type.find(type[0].category);
        if(it == definition_to_type.end()) {
            error(type[0].line, "expected one of the: type_name, tuple, unoin");
        }

        result.type = it->second;

        if(type[0].category == util::Category::IDENTIFIER) {
            result.name = type[0].value;
        }

        type = type.subspan(1);
        result.generic_params = parse_generic_params(type);

        return result;
    }

    void Parser::error(size_t line, const std::string& msg) {
        errors_.emplace_back(line, msg);
        throw ParserError{"line " + std::to_string(line) + ", parser error: " + msg};
    }
}
