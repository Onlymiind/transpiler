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

    std::vector<util::Result<Declaration>> split(Tokens tokens) {
        static const std::unordered_set<util::Category> decl_tokens {
            util::Category::TYPE,
            util::Category::FUNC,
            util::Category::VAR
        };

        std::vector<util::Result<Declaration>> result;
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

        auto decl_start = find_in_current_scope(tokens, decl_tokens);
        while(decl_start) {
            if(decl_start->second != 0) {
                assert_only_comments(tokens.subspan(0, decl_start->second));
                tokens = tokens.subspan(decl_start->second);
            } 

            Declaration decl;
            switch(decl_start->first) {
            case util::Category::TYPE:
                decl.type = DeclarationType::TYPE;
                break;
            case util::Category::FUNC:
                decl.type = DeclarationType::FUNCTION;
                break;
            case util::Category::VAR:
                decl.type = DeclarationType::VARIABLE;
                break;
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

            if((decl.type != DeclarationType::VARIABLE)
                && (tokens[2].category != util::Category::IDENTIFIER)
                && (tokens[2].category != util::Category::ASSIGN)) {

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

            decl.tokens = tokens.subspan(0, decl_end->second + 1);

            result.emplace_back(decl);

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

    util::Result<TypeInfo> parse_alias(TypeDeclaration decl, Tokens definition) {
        static const std::unordered_map<util::Category, Type> definition_to_type{
            {util::Category::IDENTIFIER, Type::UNKNOWN},
            {util::Category::TUPLE, Type::TUPLE},
            {util::Category::UNION, Type::UNION}
        };

        TypeInfo result {.declaration = std::move(decl)};
        TypeDeclaration underlying_type;

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

    util::Result<TypeInfo> parse_struct(TypeDeclaration decl, Tokens definition) {
        TypeInfo result{std::move(decl)};

        StructInfo definition;
    }

    util::Result<TypeInfo> parse_type_declaration(const Declaration& decl) {
        static const std::unordered_map<util::Category, Type> definition_to_type{
            {util::Category::IDENTIFIER, Type::ALIAS},
            {util::Category::INTERFACE, Type::INTERFACE},
            {util::Category::STRUCT, Type::STRUCT},
            {util::Category::TUPLE, Type::ALIAS},
            {util::Category::UNION, Type::ALIAS},
            {util::Category::ENUM, Type::ENUM}
        };

        TypeDeclaration info;
        Tokens tokens = decl.tokens.subspan(1);
        if(tokens.empty() || tokens[0].category != util::Category::IDENTIFIER) {
            return util::Error{decl.tokens[0].line, "type declaration: expected a type name after a \"type\" keyword"};
        }

        info.name = tokens[0].value;
        tokens = tokens.subspan(1);

        auto generic_params = parse_generic_params(tokens);
        info.generic_params.reserve(generic_params.size());
        for(auto& p : generic_params) {
            if(std::holds_alternative<util::Error>(p)) {
                return std::get<util::Error>(p);
            }
            info.generic_params.push_back(std::get<GenericParam>(p));
        }
        if(tokens.empty()) {
            return util::Error{tokens[0].line, "type declaration: expected type definition after a type name"};
        }

        auto it = definition_to_type.find(tokens[0].category);
        if(it == definition_to_type.end()) {
            return util::Error{
                tokens[0].line,
                "type declaration: expected one of the: type_name, tuple, union, enum, struct, got " + std::string{util::to_string(tokens[0].category)}
            };
        }
        info.type = it->second;

        if(info.type != Type::ALIAS) {
            tokens = tokens.subspan(1);
        } else {
            return parse_alias(std::move(info), tokens);
        }

        return TypeInfo{.declaration = std::move(info), .definition = tokens};
    }

    void parse(std::vector<util::Token> tokens) {
        auto decls_and_errs = split(tokens);
        std::vector<util::Error> errs;
        std::vector<Declaration> decls;

        for(auto& v : decls_and_errs) {
            if(auto ptr = std::get_if<util::Error>(&v); ptr) {
                errs.emplace_back(std::move(*ptr));
            } else if(auto ptr = std::get_if<Declaration>(&v); ptr) {
                decls.emplace_back(std::move(*ptr));
            }
        }

        std::vector<TypeInfo> types;
        for(const auto& decl : decls) {
            if(decl.type != DeclarationType::TYPE) {
                continue;
            }
            auto type = parse_type_declaration(decl);
            if(std::holds_alternative<util::Error>(type)) {
                std::cout << std::get<util::Error>(type) << '\n';
                continue;
            }
            types.push_back(std::move(std::get<TypeInfo>(type)));
        }

        std::cout << "Done\n";
    }
}
