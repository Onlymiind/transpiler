#ifndef COMPILER_V2_COMMON_FILE_HDR_
#define COMPILER_V2_COMMON_FILE_HDR_
#include "common/declarations.h"
#include "common/expression.h"
#include "common/literals.h"
#include "common/parsed_types.h"
#include "common/statement.h"
#include "common/util.h"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <unordered_map>
#include <vector>

namespace common {

    class AST {
      public:
        AST() = default;

        void add(Function func) {
            func.id = FunctionID{functions_.size()};
            functions_.push_back(std::move(func));
        }

        void add_global(Variable var) {
            var.id = VariableID{vars_.size()};
            global_variables_.push_back(var.id);
            vars_.push_back(std::move(var));
        }

        VariableID add_local(Variable var) {
            VariableID result = VariableID{vars_.size()};
            var.id = result;
            vars_.push_back(std::move(var));
            return result;
        }

        VariableID add_func_param(Variable param) {
            param.id = VariableID{vars_.size()};
            VariableID result = param.id;
            vars_.push_back(std::move(param));
            return result;
        }

        Variable *get_var(VariableID id) { return *id >= vars_.size() ? nullptr : &vars_[*id]; }
        const Variable *get_var(VariableID id) const { return *id >= vars_.size() ? nullptr : &vars_[*id]; }

        Function *get_function(FunctionID id) {
            if (*id >= functions_.size()) {
                return nullptr;
            }
            return &functions_[*id];
        }

        std::vector<Function> &functions() { return functions_; }
        const std::vector<Function> &functions() const { return functions_; }

        std::vector<Variable> &variables() { return vars_; }
        const std::vector<Variable> &variables() const { return vars_; }

        std::vector<VariableID> &global_variables() { return global_variables_; }
        const std::vector<VariableID> &global_variables() const { return global_variables_; }

      private:
        std::vector<Function> functions_;
        std::vector<Variable> vars_;
        std::vector<VariableID> global_variables_;
    };
} // namespace common
#endif
