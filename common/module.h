#ifndef COMPILER_V2_COMMON_MODULE_HDR_
#define COMPILER_V2_COMMON_MODULE_HDR_

#include "common/ast.h"
#include "common/base_classes.h"
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/token.h"
#include "common/types.h"
#include "common/util.h"

#include <cstdint>
#include <stack>
#include <unordered_map>

namespace common {

    // handles primitives, arrays and pointers
    class Global {
      public:
        Global(std::vector<PrimitiveType> primitives) {
            for (auto &type : primitives) {
                primitives_.emplace(type.name(), std::move(type));
            }
            pointers_.emplace(nullptr, PointerType::make_nullptr_type());
        }

        const PrimitiveType *get_primitive(IdentifierID name) const noexcept {
            auto it = primitives_.find(name);
            return it == primitives_.end() ? nullptr : &it->second;
        }

        const ArrayType *get_array(size_t count, const Type *element_type) {
            if (!element_type) {
                return nullptr;
            }
            std::pair<size_t, const Type *> key{count, element_type};
            return &arrays_.try_emplace(key, ArrayType(*element_type, count))
                        .first->second;
        }

        const PointerType *get_pointer(const Type *to) {
            if (!to) {
                return &pointers_.at(nullptr);
            }
            return &pointers_.try_emplace(to, PointerType(*to)).first->second;
        }

        const std::unordered_map<IdentifierID, PrimitiveType> &
        primitives() const noexcept {
            return primitives_;
        }

      private:
        struct PairHash {
            size_t
            operator()(std::pair<size_t, const Type *> p) const noexcept {
                return std::hash<size_t>{}(p.first) * 7919 +
                       std::hash<const Type *>{}(p.second);
            }
        };

        std::unordered_map<IdentifierID, PrimitiveType> primitives_;
        std::unordered_map<std::pair<size_t, const Type *>, ArrayType, PairHash>
            arrays_;
        std::unordered_map<const Type *, PointerType> pointers_;
    };

    class Module {
      public:
        Module() = default;

        bool add_type(IdentifierID name, const Type *type);
        bool add_function(IdentifierID name, FunctionID function);
        bool add_variable(IdentifierID name, VariableID var);

        const Type *get_type(IdentifierID name) const {
            auto it = named_types_.find(name);
            return it == named_types_.end() ? nullptr : it->second;
        }
        VariableID get_variable(IdentifierID name) const {
            auto it = variables_.find(name);
            return it == variables_.end() ? VariableID{} : it->second;
        }
        FunctionID get_function(IdentifierID name) const {
            auto it = functions_.find(name);
            return it == functions_.end() ? FunctionID{} : it->second;
        }

        void push_scope() { scopes_.push(std::vector<IdentifierID>{}); }
        void pop_scope();

        const std::unordered_map<IdentifierID, VariableID> &
        variables() const noexcept {
            return variables_;
        }
        const std::unordered_map<IdentifierID, FunctionID> &
        functions() const noexcept {
            return functions_;
        }

        FunctionID entrypoint() const noexcept { return entrypoint_; }
        void entrypoint(FunctionID function) { entrypoint_ = function; }

        bool has_name(IdentifierID name) const {
            return defined_names_.contains(name);
        }

      private:
        bool try_add_name(IdentifierID name);

        std::unordered_set<IdentifierID> defined_names_;
        std::stack<std::vector<IdentifierID>> scopes_;

        std::unordered_map<IdentifierID, const Type *> named_types_;
        std::unordered_map<IdentifierID, VariableID> variables_;
        std::unordered_map<IdentifierID, FunctionID> functions_;

        FunctionID entrypoint_;
    };
} // namespace common

#endif
