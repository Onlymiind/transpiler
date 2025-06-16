#include "catch2/catch_test_macros.hpp"

#include "vm/vm.h"

#include <fstream>
#include <iostream>
#include <span>
#include <sstream>

bool vm_assert(vm::VM &vm, std::span<vm::Value> val, vm::Value &ret,
               void *called) {
    REQUIRE(val.size() == 1);
    REQUIRE(called);
    *(bool *)called = true;

    auto b = val[0].get_bool();

    REQUIRE(b.has_value());
    REQUIRE(*b);

    return true;
}

bool vm_print(vm::VM &vm, std::span<vm::Value> val, vm::Value &ret, void *) {
    REQUIRE(val.size() == 1);

    auto value = val[0].get_int();

    REQUIRE(value.has_value());
    INFO(std::to_string(*value));
    std::cout << *value << '\n';

    return true;
}

TEST_CASE("vm", "[vm]") {
    std::ifstream in{"vm_tests"};

    std::stringstream err;
    auto vm = vm::VM::create(in, &err);

    std::string err_str = err.str();
    INFO(err_str);
    REQUIRE(vm);
    REQUIRE(err_str.empty());

    bool called = false;
    REQUIRE(vm->bind_native("assert", vm_assert, &called));
    REQUIRE(vm->bind_native("print", vm_print));
    vm::Value ret = vm->make_value(nullptr);

    SECTION("integer ops") {
        bool res = vm->call_function("test_int_ops", {}, &err_str, &ret);
        INFO(err_str);
        REQUIRE(res);
        REQUIRE(ret.empty());
        REQUIRE(called);
    }
    called = false;

    SECTION("float ops") {
        bool res = vm->call_function("test_float_ops", {}, &err_str, &ret);
        INFO(err_str);
        REQUIRE(res);
        REQUIRE(ret.empty());
        REQUIRE(called);
    }

    SECTION("char ops") {
        bool res = vm->call_function("test_char_ops", {}, &err_str, &ret);
        INFO(err_str);
        REQUIRE(res);
        REQUIRE(ret.empty());
        REQUIRE(called);
    }

    SECTION("bool ops") {
        bool res = vm->call_function("test_bool_ops", {}, &err_str, &ret);
        INFO(err_str);
        REQUIRE(res);
        REQUIRE(ret.empty());
        REQUIRE(called);
    }

    SECTION("misc ops") {
        bool res = vm->call_function("test_misc_ops", {}, &err_str, &ret);
        INFO(err_str);
        REQUIRE(res);
        REQUIRE(ret.empty());
        REQUIRE(called);
    }

    SECTION("mem") {
        bool res = vm->call_function("test_mem", {}, &err_str, &ret);
        INFO(err_str);
        REQUIRE(res);
        REQUIRE(ret.empty());
        REQUIRE(called);
    }
}
