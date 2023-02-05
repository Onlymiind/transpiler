#include "parser/parser.h"

#include <algorithm>
#include <stack>
#include <iostream>
#include <memory>

namespace parser {

    std::optional<std::pair<util::Category, size_t>> find_in_current_scope(util::Tokens tokens, const std::unordered_set<util::Category>& categories) {
        std::unordered_map<util::Category, size_t> found;
        for(auto cat : categories) {
            auto idx = util::find_in_current_scope(tokens, cat);
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

    std::optional<size_t> first_not_comment(util::Tokens tokens) {
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
        auto tokens = util::Tokens{tokens_};
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
                    result.emplace_back(parse_function(tokens.subspan(0, decl_end->second + 1)));
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

    std::vector<GenericParam> Parser::parse_generic_params(util::Tokens& tokens) {
        size_t end = util::consume_scope(tokens, 0, {util::Category::LESS, util::Category::GREATER});
        if(end <= 1) {
            return {};
        }

        std::vector<GenericParam> result;
        util::Tokens params = tokens;
        for(size_t i = 1; i < end; i++) {
            switch(params[i].category) {
            case util::Category::COMMA:
                continue;
            case util::Category::IDENTIFIER:
                result.emplace_back(GenericParam{params[i].value});
                break;
            default:
                error(params[i].line, "generic arguments: expected comma separated list of identifiers");
            }
        }

        params = params.subspan(std::min(tokens.size(), end + 1));
        tokens = params;
        return result;
    }

    TypeInfo Parser::parse_type_declaration(util::Tokens decl) {
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

        size_t line = decl[0].line;
        if(info.type != DeclarationType::ALIAS) {
            decl = decl.subspan(1);
        }

        switch(info.type) {
        case DeclarationType::ALIAS:
            return parse_alias(std::move(info), decl);
        case DeclarationType::STRUCT:
            return parse_struct(std::move(info), decl);
        }

        error(line, "parse_type_declaration: type not implemented");
    }

    TypeInfo Parser::parse_alias(Declaration decl, util::Tokens definition) {
        return TypeInfo{
            .declaration = std::move(decl),
            .definition = parse_type(definition)
        };
    }

    NamedField Parser::parse_variable(util::Tokens tokens) {
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

    TypeInfo Parser::parse_function(util::Tokens decl, size_t start_line) {
        if(decl.size() <= 1) {
            error(start_line, "empty function declaration, possibly a bug in a compiler");
        }

        // func keyword
        if(decl.empty() || decl[0].category != util::Category::IDENTIFIER) {
            error(start_line, "function declaration: expected function name");
        }

        TypeInfo result;
        size_t line = decl[0].line;
        result.declaration.type = DeclarationType::FUNCTION;
        result.declaration.name = decl[0].value;
        decl = decl.subspan(1);
        result.declaration.generic_params = parse_generic_params(decl);

        if(decl.empty()) {
            error(line, "function declaration: expected function parameters");
        }

        if(decl[0].category != util::Category::LPAREN) {
            error(decl[0].line, "function declaration: expected \"(\"");
        }
        line = decl[0].line;
        decl = decl.subspan(1);

        util::Tokens decl_copy = decl;
        util::Tokens params = util::split(decl_copy, util::Category::RPAREN);

        // no "("
        if(decl_copy.size() == decl.size()) {
            error(line, "function declaration: expected a \")\"");
        }

        decl = decl_copy;
        if(decl.empty()) {
            error(line, "function declaration: expected \";\" or \"{\"");
        }

        auto next_param = [&params]() -> util::Tokens {
            return util::split(params, util::Category::COMMA);
        };

        FunctionInfo def;
        for(util::Tokens param = next_param(); !param.empty(); param = next_param()) {
            def.params.insert(parse_variable(param));
        }
        if(!params.empty()) {
            def.params.insert(parse_variable(params));
        }

        if(decl[0].category != util::Category::LBRACE && decl[0].category != util::Category::SEMICOLON) {
            def.return_type = parse_type(decl);
        }

        if(decl.empty()) {
            error(line, "function declaration: expected \";\" or \"{\"");
        }
        if(decl.size() == 1 && decl[0].category != util::Category::SEMICOLON) {
            error(decl[0].line, "function declaration: expected \";\"");
        }
        if(decl[0].category != util::Category::LBRACE || decl.back().category != util::Category::RBRACE) {
            error(decl[0].line, "function declaration: expected a block");
        }

        if(decl[0].category == util::Category::LBRACE) {
            def.body = decl;
        }

        result.definition = def;
        return result;
    }

    TypeInfo Parser::parse_struct(Declaration decl, util::Tokens definition, size_t start_line) {
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

        auto next_field = [&definition]() -> util::Tokens {
            return util::split(definition, util::Category::SEMICOLON);
        };

        for(util::Tokens field_def = next_field(); !field_def.empty(); field_def = next_field()) {
            struct_definition.fields.insert(parse_variable(field_def));
        }

        if(definition.size() > 1) {
            error(definition[0].line, "unexpected end");
        }
        
        return result;
    }

    Declaration Parser::parse_type(util::Tokens& tokens, size_t start_line) {
        static const std::unordered_map<util::Category, DeclarationType> definition_to_type{
            {util::Category::IDENTIFIER, DeclarationType::UNKNOWN},
            {util::Category::TUPLE, DeclarationType::TUPLE},
            {util::Category::UNION, DeclarationType::UNION}
        };

        Declaration result;
        util::Tokens type = tokens;
        while(!type.empty() && type[0].is_type_modifier()) {
            switch(type[0].category) {
            case util::Category::OPTIONAL:
                result.modifiers.push_back(TypeModifiers::OPTIONAL);
                break;
            case util::Category::MULTIPLY:
                result.modifiers.push_back(TypeModifiers::POINTER);
                break;
            default:
                error(type[0].line, "modifier " + std::string{util::to_string(type[0].category)} + " not implemented");
            }

            type = type.subspan(1);
        }

        if(type.empty()) {
            error(start_line, "expected a type");
        }

        auto it = definition_to_type.find(type[0].category);
        if(it == definition_to_type.end()) {
            error(type[0].line, "expected one of the: type_name, tuple, union");
        }

        result.type = it->second;
        if(type[0].category == util::Category::IDENTIFIER) {
            result.name = type[0].value;
        }
        type = type.subspan(1);

        result.generic_params = parse_generic_params(type);
        tokens = type;

        return result;
    }

    void Parser::error(size_t line, const std::string& msg) {
        errors_.emplace_back(line, msg);
        throw ParserError{"line " + std::to_string(line) + ", parser error: " + msg};
    }
}
