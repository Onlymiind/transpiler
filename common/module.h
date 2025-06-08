#ifndef COMPILER_V2_COMMON_MODULE_HDR_
#define COMPILER_V2_COMMON_MODULE_HDR_

#include "common/base_classes.h"
#include "common/types.h"
#include "common/util.h"

#include <stack>
#include <unordered_map>
#include <unordered_set>

namespace common {

    // handles primitives, arrays and pointers
    class Global {
      public:
        explicit Global(std::vector<PrimitiveType> primitives);

        const PrimitiveType *get_primitive(IdentifierID name) const noexcept;

        const ArrayType *get_array(size_t count, const Type *element_type);

        const PointerType *get_pointer(const Type *to);

        const StructType *get_slice(const Type *size_type,
                                    const Type *element_type,
                                    IdentifierID cap_name,
                                    IdentifierID len_name,
                                    IdentifierID data_name);

        const std::unordered_map<IdentifierID, const PrimitiveType *> &
        primitives() const noexcept {
            return primitives_;
        }

        std::vector<std::unique_ptr<common::Type>> &types() noexcept {
            return types_;
        }

        const std::vector<std::unique_ptr<common::Type>> &
        types() const noexcept {
            return types_;
        }

      private:
        struct PairHash {
            size_t
            operator()(std::pair<size_t, const Type *> p) const noexcept {
                return std::hash<size_t>{}(p.first) * 7919 +
                       std::hash<const Type *>{}(p.second);
            }
        };

        std::vector<std::unique_ptr<common::Type>> types_;
        std::unordered_map<IdentifierID, const PrimitiveType *> primitives_;
        std::unordered_map<std::pair<size_t, const Type *>, const ArrayType *,
                           PairHash>
            arrays_;
        std::unordered_map<const Type *, const PointerType *> pointers_;
        std::unordered_map<const Type *, const StructType *> slices_;
    };

    class Module {
      public:
        Module() = default;

        bool add_type(IdentifierID name, const Type *type);
        bool add_function(IdentifierID name, FunctionID function);
        bool add_variable(IdentifierID name, VariableID var);
        StructType *add_struct(StructType record);

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

        bool has_name(IdentifierID name) const {
            return defined_names_.contains(name);
        }

        std::vector<std::unique_ptr<StructType>> &get_structs() noexcept {
            return structs_;
        }

      private:
        bool try_add_name(IdentifierID name);

        std::unordered_set<IdentifierID> defined_names_;
        std::stack<std::vector<IdentifierID>> scopes_;

        std::unordered_map<IdentifierID, const Type *> named_types_;
        std::unordered_map<IdentifierID, VariableID> variables_;
        std::unordered_map<IdentifierID, FunctionID> functions_;

        std::vector<std::unique_ptr<StructType>> structs_;
    };
} // namespace common

#endif
