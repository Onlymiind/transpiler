#include <unordered_map>
#include <vector>
#include <algorithm>
#include <random>
#include <iostream>
#include <string_view>

#include <benchmark/benchmark.h>

#include "util/util.h"
#include "util/hashmap.h"

/*
        std::pair{'!', 4},
        std::pair{'&', 19},
        std::pair{'|', 16},
        std::pair{'/', 8},
        std::pair{'*', 76},
        std::pair{'(', 39},
        std::pair{')', 33},
        std::pair{'{', 47},
        std::pair{'}', 174},
        std::pair{'[', 534},
        std::pair{']', 823},
        std::pair{',', 800},
        std::pair{'\'', 240},
        std::pair{'\"', 907},
        std::pair{':', 282},
        std::pair{';', 424},
        std::pair{'.', 275},
        std::pair{'+', 866},
        std::pair{'-', 884},
        std::pair{'=', 346},
        std::pair{'?', 539},
        std::pair{'<', 236},
        std::pair{'>', 151},
        std::pair{'~', 125},
        std::pair{'^', 186},
        std::pair{'q', 944},
        std::pair{'w', 650},
        std::pair{'e', 916},
        std::pair{'r', 210},
        std::pair{'t', 642},
        std::pair{'y', 536},
        std::pair{'u', 531},
        std::pair{'i', 519},
        std::pair{'o', 264},
        std::pair{'p', 883},
        std::pair{'a', 185},
        std::pair{'s', 933},
        std::pair{'d', 318},
        std::pair{'f', 127},
        std::pair{'g', 141},
        std::pair{'h',  686},
        std::pair{'j', 717},
        std::pair{'k', 450},
        std::pair{'l', 254},
        std::pair{'z', 478},
        std::pair{'x', 209},
        std::pair{'c', 226},
        std::pair{'v', 306},
        std::pair{'b', 975},
        std::pair{'n', 726},
        std::pair{'m', 356},
        std::pair{'1', 610},
        std::pair{'2', 510},
        std::pair{'3', 839},
        std::pair{'4', 520},
        std::pair{'5', 942}
        
*/

void BM_const_hashmap(benchmark::State& state) {
    constexpr util::Hashmap map = util::Hashmap(
        std::array{std::pair{'!', 4},
        std::pair{'&', 19},
        std::pair{'|', 16},
        std::pair{'/', 8},
        std::pair{'*', 76},
        std::pair{'(', 39},
        std::pair{')', 33},
        std::pair{'{', 47},
        std::pair{'}', 174},
        std::pair{'[', 534},
        std::pair{']', 823},
        std::pair{',', 800},
        std::pair{'\'', 240},
        std::pair{'\"', 907},
        std::pair{':', 282},
        std::pair{';', 424},
        std::pair{'.', 275},
        std::pair{'+', 866},
        std::pair{'-', 884},
        std::pair{'=', 346},
        std::pair{'?', 539},
        std::pair{'<', 236},
        std::pair{'>', 151},
        std::pair{'~', 125},
        std::pair{'^', 186},
        std::pair{'q', 944},
        std::pair{'w', 650},
        std::pair{'e', 916},
        std::pair{'r', 210},
        std::pair{'t', 642},
        std::pair{'y', 536},
        std::pair{'u', 531},
        std::pair{'i', 519},
        std::pair{'o', 264},
        std::pair{'p', 883},
        std::pair{'a', 185},
        std::pair{'s', 933},
        std::pair{'d', 318},
        std::pair{'f', 127},
        std::pair{'g', 141},
        std::pair{'h',  686},
        std::pair{'j', 717},
        std::pair{'k', 450},
        std::pair{'l', 254},
        std::pair{'z', 478},
        std::pair{'x', 209},
        std::pair{'c', 226},
        std::pair{'v', 306},
        std::pair{'b', 975},
        std::pair{'n', 726},
        std::pair{'m', 356},
        std::pair{'1', 610},
        std::pair{'2', 510},
        std::pair{'3', 839},
        std::pair{'4', 520},
        std::pair{'5', 942}}
    );
    
    std::random_device device{};
    std::mt19937 rand{device()};
    
    for(auto _ : state) {
        volatile auto c = map.find(rand() % 128);
    }
}

void BM_const_robinmap(benchmark::State& state) {
    constexpr util::RobinMap map = util::RobinMap(
        std::array{std::pair{'!', 4},
        std::pair{'&', 19},
        std::pair{'|', 16},
        std::pair{'/', 8},
        std::pair{'*', 76},
        std::pair{'(', 39},
        std::pair{')', 33},
        std::pair{'{', 47},
        std::pair{'}', 174},
        std::pair{'[', 534},
        std::pair{']', 823},
        std::pair{',', 800},
        std::pair{'\'', 240},
        std::pair{'\"', 907},
        std::pair{':', 282},
        std::pair{';', 424},
        std::pair{'.', 275},
        std::pair{'+', 866},
        std::pair{'-', 884},
        std::pair{'=', 346},
        std::pair{'?', 539},
        std::pair{'<', 236},
        std::pair{'>', 151},
        std::pair{'~', 125},
        std::pair{'^', 186},
        std::pair{'q', 944},
        std::pair{'w', 650},
        std::pair{'e', 916},
        std::pair{'r', 210},
        std::pair{'t', 642},
        std::pair{'y', 536},
        std::pair{'u', 531},
        std::pair{'i', 519},
        std::pair{'o', 264},
        std::pair{'p', 883},
        std::pair{'a', 185},
        std::pair{'s', 933},
        std::pair{'d', 318},
        std::pair{'f', 127},
        std::pair{'g', 141},
        std::pair{'h',  686},
        std::pair{'j', 717},
        std::pair{'k', 450},
        std::pair{'l', 254},
        std::pair{'z', 478},
        std::pair{'x', 209},
        std::pair{'c', 226},
        std::pair{'v', 306},
        std::pair{'b', 975},
        std::pair{'n', 726},
        std::pair{'m', 356},
        std::pair{'1', 610},
        std::pair{'2', 510},
        std::pair{'3', 839},
        std::pair{'4', 520},
        std::pair{'5', 942}}
    );

    std::random_device device{};
    std::mt19937 rand{device()};
    
    for(auto _ : state) {
        volatile auto c = map.find(rand() % 128);
    }
}

void BM_std_hashmap(benchmark::State& state) {
    static const std::unordered_map<char, int> map{
        std::pair{'!', 4},
        std::pair{'&', 19},
        std::pair{'|', 16},
        std::pair{'/', 8},
        std::pair{'*', 76},
        std::pair{'(', 39},
        std::pair{')', 33},
        std::pair{'{', 47},
        std::pair{'}', 174},
        std::pair{'[', 534},
        std::pair{']', 823},
        std::pair{',', 800},
        std::pair{'\'', 240},
        std::pair{'\"', 907},
        std::pair{':', 282},
        std::pair{';', 424},
        std::pair{'.', 275},
        std::pair{'+', 866},
        std::pair{'-', 884},
        std::pair{'=', 346},
        std::pair{'?', 539},
        std::pair{'<', 236},
        std::pair{'>', 151},
        std::pair{'~', 125},
        std::pair{'^', 186},
        std::pair{'q', 944},
        std::pair{'w', 650},
        std::pair{'e', 916},
        std::pair{'r', 210},
        std::pair{'t', 642},
        std::pair{'y', 536},
        std::pair{'u', 531},
        std::pair{'i', 519},
        std::pair{'o', 264},
        std::pair{'p', 883},
        std::pair{'a', 185},
        std::pair{'s', 933},
        std::pair{'d', 318},
        std::pair{'f', 127},
        std::pair{'g', 141},
        std::pair{'h',  686},
        std::pair{'j', 717},
        std::pair{'k', 450},
        std::pair{'l', 254},
        std::pair{'z', 478},
        std::pair{'x', 209},
        std::pair{'c', 226},
        std::pair{'v', 306},
        std::pair{'b', 975},
        std::pair{'n', 726},
        std::pair{'m', 356},
        std::pair{'1', 610},
        std::pair{'2', 510},
        std::pair{'3', 839},
        std::pair{'4', 520},
        std::pair{'5', 942}
    };

    std::random_device device{};
    std::mt19937 rand{device()};
    
    for(auto _ : state) {
        volatile auto c = map.find(rand() % 128);
    }

}

constexpr inline int switch_func(char c) {
        switch(c) 
        {
        case '!' : return 1 ;
        case '&' : return 201 ;
        case '|' : return 3 ;
        case '/' : return 4 ;
        case '*' : return 5322 ;
        case '(' : return 632910 ;
        case ')' : return 7 ;
        case '{' : return 8 ;
        case '}' : return 9 ;
        case '[' : return 10 ;
        case ']' : return 11 ;
        case ',' : return 12124 ;
        case '\'' : return 13 ;
        case '\"' : return 14 ;
        case ':' : return 15343 ;
        case ';' : return 16 ;
        case '.' : return 17 ;
        case '+' : return 18132 ;
        case '-' : return 191 ;
        case '=' : return 210 ;
        case '?' : return 219 ;
        case '<' : return 2232 ;
        case '>' : return 23 ;
        case '~' : return 2423 ;
        case '^' : return 254 ;
        default: return -1;
        }
}

void BM_switch(benchmark::State& state) {
    std::random_device device{};
    std::mt19937 rand{device()};
    
    for(auto _ : state) {
        volatile auto c = switch_func(rand() % 128);
    }
}

BENCHMARK(BM_const_hashmap);
BENCHMARK(BM_const_robinmap);
BENCHMARK(BM_std_hashmap);
BENCHMARK(BM_switch);

