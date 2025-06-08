#ifndef COMPILER_V2_COMMON_FILE_HDR_
#define COMPILER_V2_COMMON_FILE_HDR_
#include "common/declarations.h"
#include "common/parsed_types.h"
#include "common/util.h"

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
            var.is_gloabl = true;
            vars_.push_back(std::move(var));
        }

        VariableID add_local(Variable var) {
            VariableID result = VariableID{vars_.size()};
            var.id = result;
            var.is_gloabl = false;
            vars_.push_back(std::move(var));
            return result;
        }

        VariableID add_func_param(Variable param) {
            param.id = VariableID{vars_.size()};
            VariableID result = param.id;
            vars_.push_back(std::move(param));
            return result;
        }

        void add_struct(ParsedStructType &&record) {
            structs_.emplace_back(std::move(record));
        }

        Variable *get_var(VariableID id) {
            return *id >= vars_.size() ? nullptr : &vars_[*id];
        }
        const Variable *get_var(VariableID id) const {
            return *id >= vars_.size() ? nullptr : &vars_[*id];
        }

        Function *get_function(FunctionID id) {
            if (*id >= functions_.size()) {
                return nullptr;
            }
            return &functions_[*id];
        }

        std::vector<Function> &functions() noexcept { return functions_; }
        const std::vector<Function> &functions() const noexcept {
            return functions_;
        }

        std::vector<Variable> &variables() noexcept { return vars_; }
        const std::vector<Variable> &variables() const noexcept {
            return vars_;
        }

        std::vector<VariableID> &global_variables() noexcept {
            return global_variables_;
        }
        const std::vector<VariableID> &global_variables() const noexcept {
            return global_variables_;
        }

        std::vector<ParsedStructType> &structs() noexcept { return structs_; }
        const std::vector<ParsedStructType> &structs() const noexcept {
            return structs_;
        }

      private:
        std::vector<Function> functions_;
        std::vector<Variable> vars_;
        std::vector<VariableID> global_variables_;
        std::vector<ParsedStructType> structs_;
    };
} // namespace common
#endif
