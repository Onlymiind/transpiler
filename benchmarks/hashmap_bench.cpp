#include <unordered_map>
#include <vector>
#include <algorithm>
#include <random>
#include <iostream>
#include <string_view>

#include <benchmark/benchmark.h>

#include "util/util.h"
#include "util/hashmap.h"

using namespace std::literals;

constexpr std::array test_data {
    std::pair{"!das"sv, 4},
    std::pair{"&gedas"sv, 19},
    std::pair{"|asldlka"sv, 16},
    std::pair{"/sdfa2qw"sv, 8},
    std::pair{"*skaj"sv, 76},
    std::pair{"asfger("sv, 39},
    std::pair{"fdls,v)"sv, 33},
    std::pair{"{akwodpqka"sv, 47},
    std::pair{"}wdwfp[2p"sv, 174},
    std::pair{"svce[pkef3[qw["sv, 534},
    std::pair{"]qqwiej31-ol[w"sv, 823},
    std::pair{"sv,alw[pkdiq[wpl"sv, 800},
    std::pair{"\as'd;kcwopwmx"sv, 240},
    std::pair{"alpwld=]\""sv, 907},
    std::pair{":awk[dpk"sv, 282},
    std::pair{"qx,p][3w,pd;"sv, 424},
    std::pair{".faewq2q2e"sv, 275},
    std::pair{"1343edsfa+"sv, 866},
    std::pair{"-dqw[2p[ek"sv, 884},
    std::pair{"12ekw[dcml="sv, 346},
    std::pair{"    dnicwecbn?"sv, 539},
    std::pair{"v\\wrl  <"sv, 236},
    std::pair{"> qp92eiodm ,1=-30"sv, 151},
    std::pair{"1d[c 1~"sv, 125},
    std::pair{"^"sv, 186},
    std::pair{" q}{wk21=eq"sv, 944},
    std::pair{"wqm [oek3\nq,d"sv, 650},
    std::pair{"kvnkmlzse"sv, 916},
    std::pair{"123r"sv, 210},
    std::pair{"tdwa234"sv, 642},
    std::pair{"yc24q9-j8cm"sv, 536},
    std::pair{"u qpe9ok"sv, 531},
    std::pair{" qp 29u3rni"sv, 519},
    std::pair{" 3jeropnqnfio"sv, 264},
    std::pair{"o]o-kp"sv, 883},
    std::pair{"amxq pwiem"sv, 185},
    std::pair{" aoeWK]Ps"sv, 933},
    std::pair{"CAMWE-]K,d"sv, 318},
    std::pair{"C 'EWKMof,f"sv, 127},
    std::pair{" pFCM;WEg"sv, 141},
    std::pair{"237YFHUIh"sv,  686},
    std::pair{"jy789ujHJI"sv, 717},
    std::pair{"qi[j3dk"sv, 450},
    std::pair{"c'qml"sv, 254},
    std::pair{"zE332J[R"sv, 478},
    std::pair{"CEWPMFLx"sv, 209},
    std::pair{"12e;onc"sv, 226},
    std::pair{"vcpin"sv, 306},
    std::pair{"bc2o-3jkkl "sv, 975},
    std::pair{"nd1pokemd"sv, 726},
    std::pair{"21jekml/mm"sv, 356},
    std::pair{"d3o-12kpd1"sv, 610},
    std::pair{"2d1k,["sv, 510},
    std::pair{"3d12-ookdmn,m"sv, 839},
    std::pair{"d12-jdokmln4"sv, 520},
    std::pair{"c2jpmkl5"sv, 942}
};

constexpr std::array test_data_ints {
    std::pair{10, 4},
    std::pair{319, 19},
    std::pair{435, 16},
    std::pair{874, 8},
    std::pair{293, 76},
    std::pair{134, 39},
    std::pair{176, 33},
    std::pair{93, 47},
    std::pair{876, 174},
    std::pair{131, 534},
    std::pair{452, 823},
    std::pair{762, 800},
    std::pair{4319018, 240},
    std::pair{138342, 907},
    std::pair{1218537, 282},
    std::pair{123978, 424},
    std::pair{1283789, 275},
    std::pair{3484, 866},
    std::pair{9823, 884},
    std::pair{65135, 346},
    std::pair{86451, 539},
    std::pair{3615767, 236},
    std::pair{982316, 151},
    std::pair{563717, 125},
    std::pair{0, 186},
    std::pair{13879, 944},
    std::pair{67813, 650},
    std::pair{6567183, 916},
    std::pair{9999, 210},
    std::pair{123785, 642},
    std::pair{987613, 536},
    std::pair{36981, 531},
    std::pair{8930123, 519},
    std::pair{10973, 264},
    std::pair{2389013, 883},
    std::pair{500089, 185},
    std::pair{7089, 933},
    std::pair{709038, 318},
    std::pair{58098, 127},
    std::pair{900812, 141},
    std::pair{709831,  686},
    std::pair{34205, 717},
    std::pair{706901, 450},
    std::pair{123904, 254},
    std::pair{1239, 478},
    std::pair{29301, 209},
    std::pair{92318037, 226},
    std::pair{90834, 306},
    std::pair{203819, 975},
    std::pair{20138, 726},
    std::pair{913298, 356},
    std::pair{29318, 610},
    std::pair{219385, 510},
    std::pair{129847830, 839},
    std::pair{81963, 520},
    std::pair{12038, 942}
};

inline volatile bool b;

std::string random_string(size_t max_len = 100) {
    std::random_device device{};
    std::mt19937 random{device()};
    std::string result;
    result.resize(random() % max_len);
    for(char& c : result) {
        c = static_cast<char>(random() % 128);
    }

    return result;
}

void BM_hashmap_string(benchmark::State& state) {
    util::Hashmap map = util::Hashmap{test_data};

    std::random_device device{};
    std::mt19937 random{device()};
    std::vector<std::string> data;
    data.resize(1024);
    for(auto& c : data) {
        c = random_string();
    }

    constexpr size_t ii = sizeof(map);

    size_t i = 0;
    for(auto _ : state) {
        b = map.find(data[i]) == map.end();
        i = (i + 1) % 1024;
    }
}

void BM_hashmap_ints(benchmark::State& state) {
    util::Hashmap map = util::Hashmap{test_data_ints};

    std::random_device device{};
    std::mt19937 random{device()};
    std::vector<int> data;
    data.resize(1024);
    for(auto& c : data) {
        c = int(random());
    }

    constexpr size_t ii = sizeof(map);

    size_t i = 0;
    for(auto _ : state) {
        b = map.find(data[i]) == map.end();
        i = (i + 1) % 1024;
    }
}

template<typename T>
struct Hasher {
    constexpr size_t operator()(T val) const noexcept {
        return util::hash(val);
    }
};

void BM_std_hashmap_string(benchmark::State& state) {
    std::unordered_map<std::string_view, int, Hasher<std::string_view>> map{test_data.begin(), test_data.end()};

    std::random_device device{};
    std::mt19937 random{device()};
    std::vector<std::string> data;
    data.resize(1024);
    for(auto& c : data) {
        c = random_string();
    }

    size_t i = 0;
    for(auto _ : state) {
        b = map.find(data[i]) == map.end();
        i = (i + 1) % 1024;
    }

}

void BM_std_hashmap_ints(benchmark::State& state) {
    std::unordered_map<int, int, Hasher<int>> map{test_data_ints.begin(), test_data_ints.end()};

    std::random_device device{};
    std::mt19937 random{device()};
    std::vector<int> data;
    data.resize(1024);
    for(auto& c : data) {
        c = int(random());
    }

    size_t i = 0;
    for(auto _ : state) {
        b = map.find(data[i]) == map.end();
        i = (i + 1) % 1024;
    }

}

BENCHMARK(BM_hashmap_string);
BENCHMARK(BM_hashmap_ints);
BENCHMARK(BM_std_hashmap_string);
BENCHMARK(BM_std_hashmap_ints);

