#include <catch2/catch_test_macros.hpp>

#include "util/error_handler.h"
#include "util/arena.h"
#include "checker/module.h"

util::StringAllocator g_alloc;
util::ErrorHandler g_err;

bool operator==(const module::Field& lhs, const module::Field& rhs) {
    return lhs.name == rhs.name && lhs.type == rhs.type && lhs.default_value == rhs.default_value;
}

TEST_CASE("register builtin") {
    module::Module m{g_err};
    auto name = g_alloc.allocate("u8");
    auto id = m.register_builtin(name);

    REQUIRE(module::get_kind(id) == module::IDKind::BUILTIN);
    REQUIRE(m.get_type_id(name));
    REQUIRE(!m.get_alias_info(id));
    REQUIRE(!m.get_struct_info(id));
    REQUIRE(!m.get_function_info(id));
}

TEST_CASE("redeclaration") {
    module::Module m{g_err};
    auto name = g_alloc.allocate("u8");
    auto id = m.register_builtin(name);

    REQUIRE_THROWS_AS(m.register_alias(module::AliasInfo{}, name), util::CheckerError);
    REQUIRE_THROWS_AS(m.register_builtin(name), util::CheckerError);
    REQUIRE_THROWS_AS(m.register_function(module::FunctionInfo{}, name), util::CheckerError);
    REQUIRE_THROWS_AS(m.register_struct(module::StructInfo{}, name), util::CheckerError);
}

TEST_CASE("register alias") {
    module::Module m{g_err};
    auto underlying_id = m.register_builtin(g_alloc.allocate("u8"));
    module::AliasInfo i{underlying_id};
    auto name = g_alloc.allocate("alias");
    auto id = m.register_alias(i, name);

    REQUIRE(module::get_kind(id) == module::IDKind::ALIAS);
    REQUIRE(m.get_type_id(name));
    auto ptr = m.get_alias_info(id);
    REQUIRE((ptr && ptr->underlying_type == i.underlying_type));
    REQUIRE(!m.get_struct_info(id));
    REQUIRE(!m.get_function_info(id));
}

TEST_CASE("register struct") {
    module::Module m{g_err};
    auto fid = m.register_builtin(g_alloc.allocate("u8"));
    auto fname = g_alloc.allocate("x");
    module::StructInfo info{.fields = {{fname, fid}}};
    auto name = g_alloc.allocate("name");
    auto id = m.register_struct(info, name);

    REQUIRE(module::get_kind(id) == module::IDKind::STRUCT);
    REQUIRE(m.get_type_id(name));
    auto ptr = m.get_struct_info(id);
    REQUIRE(ptr);
    REQUIRE(ptr->fields.size() == 1);
    REQUIRE((ptr->fields[0] == info.fields[0]));

    REQUIRE(!m.get_alias_info(id));
    REQUIRE(!m.get_function_info(id));
}

TEST_CASE("register function") {
    module::Module m{g_err};
    auto pid = m.register_builtin(g_alloc.allocate("u8"));
    auto rid = m.register_builtin(g_alloc.allocate("u16"));

    auto pname = g_alloc.allocate("x");
    module::FunctionInfo info{.args = {{pname, pid}}, .return_type = rid};
    auto name = g_alloc.allocate("name");
    auto id = m.register_function(info, name);

    REQUIRE(module::get_kind(id) == module::IDKind::FUNCTION);
    REQUIRE(m.get_type_id(name));
    auto ptr = m.get_function_info(id);
    REQUIRE(ptr);
    REQUIRE(ptr->args.size() == 1);
    REQUIRE((ptr->args[0] == info.args[0]));
    REQUIRE(ptr->return_type);
    REQUIRE((ptr->return_type == rid));

    REQUIRE(!m.get_alias_info(id));
    REQUIRE(!m.get_struct_info(id));
}

TEST_CASE("multiple types") {
    module::Module m{g_err};
    auto bname = g_alloc.allocate("u8");
    m.register_builtin(bname);

    auto aname = g_alloc.allocate("alias");
    m.register_alias(module::AliasInfo{}, aname);

    auto sname = g_alloc.allocate("struct");
    m.register_struct(module::StructInfo{}, sname);

    auto fname = g_alloc.allocate("func");
    m.register_function(module::FunctionInfo{}, fname);

    REQUIRE(m.get_type_id(bname));
    REQUIRE(m.get_alias_info(*m.get_type_id(aname)));
    REQUIRE(m.get_struct_info(*m.get_type_id(sname)));
    REQUIRE(m.get_function_info(*m.get_type_id(fname)));
}
