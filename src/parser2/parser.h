#pragma once
#include <vector>
#include <memory>
#include <optional>

#include "util/util.h"

namespace parser_2 {
    enum class Type {
        TERMINAL,
        ROOT,

    };

    struct Node;

    using Kids = std::vector<std::unique_ptr<Node>>;

    struct Node {
        Type type = Type::TERMINAL;
        std::variant<util::Token, Kids> value;
    };
}