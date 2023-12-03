#ifndef COMPILER_V2_COMMON_SCOPE_HDR_
#define COMPILER_V2_COMMON_SCOPE_HDR_

#include "common/types.h"
#include "common/util.h"

#include <cstdint>
#include <optional>
#include <vector>

namespace common {

    enum class SymbolType : uint8_t {
        VOID = 0,
        BUILTIN_TYPE,
        FUNCTION,
        VARIABLE,
    };

    class Scope {
      public:
        Scope(ScopeID self, ScopeID parent = ScopeID{}) : parent_(parent), self_(self) {}

        SymbolID add(BuiltinType type) {
            SymbolID result{make_id<SymbolID>(SymbolType::BUILTIN_TYPE, builtin_types_.size())};
            if (!name_to_symbol_.try_emplace(type.name, result).second) {
                return SymbolID{g_invalid_id};
            }
            builtin_types_.push_back(type);
            return result;
        }

        SymbolID add(IdentifierID name, FunctionID function) {
            SymbolID result{make_id<SymbolID>(SymbolType::FUNCTION, functions_.size())};
            if (!name_to_symbol_.try_emplace(name, result).second) {
                return SymbolID{g_invalid_id};
            }
            functions_.push_back(function);
            return result;
        }

        SymbolID add(IdentifierID name, VariableID var) {
            SymbolID result{make_id<SymbolID>(SymbolType::VARIABLE, variables_.size())};
            if (!name_to_symbol_.try_emplace(name, result).second) {
                return SymbolID{g_invalid_id};
            }
            variables_.push_back(var);
            return result;
        }

        TypeTraits get_traits(SymbolID id) {
            auto [type, idx] = decompose<SymbolType>(id);
            return type != SymbolType::BUILTIN_TYPE || idx >= builtin_types_.size() ? TypeTraits::NONE : builtin_types_[idx].traits;
        }

        std::optional<BuiltinType> get_type(SymbolID id) {
            auto [type, idx] = decompose<SymbolType>(id);
            return type != SymbolType::BUILTIN_TYPE || idx >= builtin_types_.size() ? std::optional<BuiltinType>{} : builtin_types_[idx];
        }

        std::optional<BuiltinType> get_type(IdentifierID name) { return get_type(find(name)); }

        FunctionID get_function(SymbolID id) {
            auto [type, idx] = decompose<SymbolType>(id);
            return type != SymbolType::FUNCTION || idx >= functions_.size() ? FunctionID{} : functions_[idx];
        }

        FunctionID get_function(IdentifierID name) { return get_function(find(name)); }

        VariableID get_variable(SymbolID id) {
            auto [type, idx] = decompose<SymbolType>(id);
            return type != SymbolType::VARIABLE || idx >= variables_.size() ? VariableID{} : variables_[idx];
        }

        VariableID get_variable(IdentifierID name) { return get_variable(find(name)); }

        ScopeID parent() const noexcept { return parent_; }
        ScopeID id() const noexcept { return self_; }
        bool is_global() const noexcept { return parent_ == ScopeID{g_invalid_id}; }

        constexpr static SymbolType type(SymbolID id) noexcept { return decompose<SymbolType>(id).first; }

        SymbolID find(IdentifierID id) const noexcept {
            auto it = name_to_symbol_.find(id);
            return it == name_to_symbol_.end() ? SymbolID{g_invalid_id} : it->second;
        }

        const std::vector<FunctionID> &functions() const { return functions_; }
        const std::vector<VariableID> &variables() const { return variables_; }

      private:
        ScopeID parent_;
        ScopeID self_;

        std::vector<BuiltinType> builtin_types_;
        std::vector<FunctionID> functions_;
        std::vector<VariableID> variables_;

        std::unordered_map<IdentifierID, SymbolID> name_to_symbol_;
    };
} // namespace common

#endif
