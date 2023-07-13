#include "util/error_handler.h"
#include <vector>
#include <algorithm>
#include <fstream>

namespace util {
    void ErrorHandler::report_errors(std::ostream& out, const std::filesystem::path& file_path) {
        if(errors_.empty()) {
            return;
        }
        std::fstream file{file_path};
        if(!file.is_open()) {
            //TODO: should this throw?
            return;
        }

        //find newlines positions
        //TODO: do this in lexer scince it already processes all characters in file
        std::vector<size_t> newlines;
        size_t pos = 0;
        for(char c = file.get(); file; c = file.get()) {
            ++pos;
            if(c == '\n') {
                newlines.push_back(pos);
            }
        }

        for(const auto& err : errors_) {
            //TODO: this assumes that all positions in errors are valid
            size_t line = std::lower_bound(newlines.begin(), newlines.end(), err.pos) - newlines.begin();
            out << "Error on line " << line + 1 << ", byte ";
            if(line != 0) {
                out << err.pos - newlines[line - 1] + 1;
            } else {
                out << err.pos + 1;
            }
            out << ", message: " << err.msg;
            if(err.prev_pos) {
                line = std::lower_bound(newlines.begin(), newlines.end(), *err.prev_pos) - newlines.begin();
                out << "line " << line + 1 << ", byte ";
                if(line != 0) {
                    out << ", byte " << *err.prev_pos - newlines[line - 1] + 1;
                } else {
                    out << *err.prev_pos + 1;
                }
            }
            out << '\n';
        }

    }
}
