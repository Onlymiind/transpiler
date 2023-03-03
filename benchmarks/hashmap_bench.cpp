#include <unordered_map>
#include <vector>
#include <algorithm>
#include <random>
#include <iostream>
#include <string_view>

#include <benchmark/benchmark.h>

#include "util/util.h"
#include "util/hashmap.h"

std::vector<char> data{
    '!','&','|','/','*','(',')','{','}','[',']',',','\'',
    '\'',':',';','.','+','-','=','?','<','>','~','^', ' ',
    'i', 'b', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p',
    's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'z', 'x', 'c'
};

void BM_const_hashmap(benchmark::State& state) {
    constexpr util::Hashmap map = util::make_hashmap<char, util::Category>(
        std::pair{'!', util::Category::NOT},
        std::pair{'&', util::Category::BIWISE_AND},
        std::pair{'|', util::Category::BITWISE_OR},
        std::pair{'/', util::Category::DIVIDE},
        std::pair{'*', util::Category::MULTIPLY},
        std::pair{'(', util::Category::LPAREN},
        std::pair{')', util::Category::RPAREN},
        std::pair{'{', util::Category::LBRACE},
        std::pair{'}', util::Category::RBRACE},
        std::pair{'[', util::Category::LBRACKET},
        std::pair{']', util::Category::RBRACKET},
        std::pair{',', util::Category::COMMA},
        std::pair{'\'', util::Category::SINGLE_QUOTE},
        std::pair{'\"', util::Category::DOUBLE_QUOTE},
        std::pair{':', util::Category::COLON},
        std::pair{';', util::Category::SEMICOLON},
        std::pair{'.', util::Category::DOT},
        std::pair{'+', util::Category::PLUS},
        std::pair{'-', util::Category::MINUS},
        std::pair{'=', util::Category::ASSIGN},
        std::pair{'?', util::Category::OPTIONAL},
        std::pair{'<', util::Category::LESS},
        std::pair{'>', util::Category::GREATER},
        std::pair{'~', util::Category::INVERT},
        std::pair{'^', util::Category::XOR}
    );

    std::shuffle(data.begin(), data.end(), std::mt19937{std::random_device{}()});

    size_t i = 0;
    size_t size = data.size();
    
    for(auto _ : state) {
        volatile auto c = map.find(data[i]);
        i = (i + 1) % size;
    }
}

void BM_std_hashmap(benchmark::State& state) {
    const std::unordered_map<char, util::Category> map{
        {'!', util::Category::NOT},
        {'&', util::Category::BIWISE_AND},
        {'|', util::Category::BITWISE_OR},
        {'/', util::Category::DIVIDE},
        {'*', util::Category::MULTIPLY},
        {'(', util::Category::LPAREN},
        {')', util::Category::RPAREN},
        {'{', util::Category::LBRACE},
        {'}', util::Category::RBRACE},
        {'[', util::Category::LBRACKET},
        {']', util::Category::RBRACKET},
        {',', util::Category::COMMA},
        {'\'', util::Category::SINGLE_QUOTE},
        {'"', util::Category::DOUBLE_QUOTE},
        {':', util::Category::COLON},
        {';', util::Category::SEMICOLON},
        {'.', util::Category::DOT},
        {'+', util::Category::PLUS},
        {'-', util::Category::MINUS},
        {'=', util::Category::ASSIGN},
        {'?', util::Category::OPTIONAL},
        {'<', util::Category::LESS},
        {'>', util::Category::GREATER},
        {'~', util::Category::INVERT},
        {'^', util::Category::XOR}
    };

    std::shuffle(data.begin(), data.end(), std::mt19937{std::random_device{}()});
    size_t i = 0;
    size_t size = data.size();

    for(auto _ : state) {
        volatile auto c = map.find(data[i]);
        i = (i + 1) % size;
    }

}

constexpr inline util::Category switch_func(char c) {
        switch(c) 
        {
        case '!': return  util::Category::NOT;
        case '&': return  util::Category::BIWISE_AND;
        case '|': return  util::Category::BITWISE_OR;
        case '/': return  util::Category::DIVIDE;
        case '*': return  util::Category::MULTIPLY;
        case '(': return  util::Category::LPAREN;
        case ')': return  util::Category::RPAREN;
        case '{': return  util::Category::LBRACE;
        case '}': return  util::Category::RBRACE;
        case '[': return  util::Category::LBRACKET;
        case ']': return  util::Category::RBRACKET;
        case ',': return  util::Category::COMMA;
        case '\'': return util::Category::SINGLE_QUOTE;
        case '"': return  util::Category::DOUBLE_QUOTE;
        case ':': return  util::Category::COLON;
        case ';': return  util::Category::SEMICOLON;
        case '.': return  util::Category::DOT;
        case '+': return  util::Category::PLUS;
        case '-': return  util::Category::MINUS;
        case '=': return  util::Category::ASSIGN;
        case '?': return  util::Category::OPTIONAL;
        case '<': return  util::Category::LESS;
        case '>': return  util::Category::GREATER;
        case '~': return  util::Category::INVERT;
        case '^': return  util::Category::XOR;
        default: return util::Category::NONE;
        }
}

void BM_switch(benchmark::State& state) {
    std::shuffle(data.begin(), data.end(), std::mt19937{std::random_device{}()});
    size_t i = 0;
    size_t size = data.size();

    for(auto _ : state) {
        volatile auto c = switch_func(data[i]);
        i = (i + 1) % size;
    }
}

BENCHMARK(BM_const_hashmap);
BENCHMARK(BM_std_hashmap);
BENCHMARK(BM_switch);

