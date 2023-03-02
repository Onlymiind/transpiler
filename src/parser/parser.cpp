#include "parser/parser.h"

#include <algorithm>
#include <stack>
#include <iostream>
#include <memory>

namespace parser {
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

        std::vector<std::variant<TypeInfo, NamedField>> result;

        while(!remainder_.empty()) {
            ignore_comments();
            if(remainder_.empty()) {
                break;
            }

            switch(remainder_[0].category) {
            case util::Category::IMPORT: {
                // TODO: parse import
                util::Token::Pos pos = remainder_[0].pos;
                consume(1);
                error(pos, "imports are not implemented");
                break;
            }
            case util::Category::TYPE:
                result.emplace_back(parse_type_declaration());
                break;
            case util::Category::VAR:
                result.emplace_back(parse_variable());
                break;
            case util::Category::FUNC:
                result.emplace_back(parse_function());
                break;
            default:
                consume(1);
            }
        }

        return result;
    }

    std::vector<GenericParam> Parser::parse_generic_params() {
        if(remainder_[0].category != util::Category::LESS) {
            return {};
        }
        consume(1);

        std::vector<GenericParam> result;
        result.emplace_back(GenericParam{remainder_[0].value});
        consume_expected(util::Category::IDENTIFIER, "generic params");

        while(remainder_[0].category != util::Category::GREATER) {
            consume_expected(util::Category::COMMA, "generic params");
            result.emplace_back(GenericParam{remainder_[0].value});
            consume_expected(util::Category::IDENTIFIER, "generic params");
        }

        consume(1);

        return result;
    }

    TypeInfo Parser::parse_type_declaration() {
        consume_expected(util::Category::TYPE, "type declaration");
        TypeInfo info;
        info.declaration.name = remainder_[0].value;
        consume_expected(util::Category::IDENTIFIER, "type declaration");

        info.declaration.generic_params = parse_generic_params();

        if(remainder_[0].is_type_modifier()) {
            info.declaration.type = DeclarationType::ALIAS;
            info.definition = parse_type();
            return info;
        }

        switch(remainder_[0].category) {
        case util::Category::IDENTIFIER:
        case util::Category::TUPLE:
        case util::Category::UNION:
            info.declaration.type = DeclarationType::ALIAS;
            info.definition = parse_type();
            consume_expected(util::Category::SEMICOLON, "alias declaration");
            return info;
        case util::Category::STRUCT:
            info.declaration.type = DeclarationType::STRUCT;
            consume(1);
            info.definition = parse_struct_def();
            return info;
        case util::Category::ENUM:
        case util::Category::INTERFACE:
            error(remainder_[0].pos, "not implemented");
        default:
            errorn(remainder_[0].pos, "type declaration: expected one of the: type_name, tuple, union, enum, struct, got ", remainder_[0].category);
        }
    }

    StructInfo Parser::parse_struct_def() {
        consume_expected(util::Category::LBRACE, "struct definition");
        StructInfo result;
        while(remainder_[0].category != util::Category::RBRACE) {
            do_with_recovery(util::Category::SEMICOLON, [&result, this]() {
                auto& field = result.fields[remainder_[0].value];
                consume_expected(util::Category::IDENTIFIER, "struct definition");
                consume_expected(util::Category::COLON, "struct definition");
                field.type = parse_type();
                consume_expected(util::Category::SEMICOLON, "struct definition");
            });
        }
        consume_expected(util::Category::RBRACE, "struct definition");

        return result;
    }

    Declaration Parser::parse_function_decl(bool unnamed) {
        consume_expected(util::Category::FUNC, "function declaration");
        Declaration result{.type = DeclarationType::FUNCTION};
        if(!unnamed) {
            result.name = remainder_[0].value;
            consume_expected(util::Category::IDENTIFIER, "function declaration");
        }

        result.generic_params = parse_generic_params();

        consume_expected(util::Category::LPAREN, "function declaration");
        size_t unnamed_param_cnt{0};
        while(remainder_[0].category != util::Category::RPAREN) {
            // TODO: unnamed params
            std::string_view param_name;
            if(remainder_[1].category == util::Category::COLON) {
                param_name = remainder_[0].value;
                consume_expected(util::Category::IDENTIFIER, "function declaration");
                consume_expected(util::Category::COLON, "function declaration");
            } else {
                for(size_t i = unnamed_params_.size(); i <= unnamed_param_cnt; i++) {
                    unnamed_params_.push_back(std::to_string(i));
                }
                param_name = unnamed_params_[unnamed_param_cnt];
                unnamed_param_cnt++;
            }
            auto& param = result.func_params[param_name];
            param = std::make_unique<Field>(Field{parse_type()});
            if(remainder_[0].category != util::Category::RPAREN) {
                consume_expected(util::Category::COMMA, "function declaration");
            }
        }
        consume(1);

        static const std::unordered_set<util::Category> decl_end{
            util::Category::COMMA, util::Category::SEMICOLON, util::Category::LBRACE
        };

        if(!decl_end.contains(remainder_[0].category)) {
            result.return_type = std::make_unique<Declaration>(parse_type());
        }

        return result;
    }

    NamedField Parser::parse_variable() {
        consume_expected(util::Category::VAR, "variable declaration");
        NamedField result;
        result.first = remainder_[0].value;
        consume_expected(util::Category::IDENTIFIER, "variable declaration");
        consume_expected(util::Category::COLON, "variable declaration");
        result.second = Field{.type = parse_type()};

        return result;
    }

    TypeInfo Parser::parse_function() {
        TypeInfo result;
        result.declaration = parse_function_decl();
        if(remainder_[0].category == util::Category::SEMICOLON) {
            consume(1);
            return result;
        }

        auto end = util::find_in_current_scope(remainder_, util::Category::RBRACE);
        if(!end) {
            errorn(remainder_[0].pos, "function definition does not end");
        }
        result.definition = FunctionInfo{remainder_.first(*end + 1)};
        remainder_ = remainder_.subspan(*end + 1);
        return result;
    }

    Declaration Parser::parse_type() {
        Declaration result;
        for(;remainder_[0].is_type_modifier(); consume(1)) {
            switch(remainder_[0].category) {
            case util::Category::OPTIONAL:
                result.modifiers.push_back(TypeModifiers::OPTIONAL);
                break;
            case util::Category::MULTIPLY:
                result.modifiers.push_back(TypeModifiers::POINTER);
                break;
            default:
                error(remainder_[0].pos, "not implemented");
            }
        }

        switch(remainder_[0].category) {
        case util::Category::IDENTIFIER:
            result.name = remainder_[0].value;
            break;
        case util::Category::TUPLE:
            result.type = DeclarationType::TUPLE;
            break;
        case util::Category::UNION:
            result.type = DeclarationType::UNION;
            break;
        case util::Category::FUNC:
            return parse_function_decl(true);
        default:
            errorn(remainder_[0].pos, "type: expected one of the type_name, union, tuple, got: ", remainder_[0].category);
        }
        consume(1);
        result.generic_params = parse_generic_params();
        return result;
    }

    Expression Parser::parse_expression() {
        Expression result;
        switch(remainder_[0].category) {
        case util::Category::MINUS:
            consume(1);
            result = Expression{.action = Action::NEGATE, .lhs = std::make_unique<Expression>(parse_expression())};
            break;
        case util::Category::PLUS:
            consume(1);
            result = parse_expression();
            break;
        case util::Category::IDENTIFIER:
        case util::Category::INTEGER:
        case util::Category::FLOAT:
        case util::Category::STRING:
            result.terminal = remainder_[0];
            consume(1);
            break;
        default:
            errorn(remainder_[0].pos, "unexpected token: ", remainder_[0].category);
        }

        return result;
    }

    void Parser::error(size_t pos, const std::string& msg) {
        errors_.emplace_back(util::Error{pos, msg});
        throw ParserError{"pos " + std::to_string(pos) + ", parser error: " + msg};
    }
}
