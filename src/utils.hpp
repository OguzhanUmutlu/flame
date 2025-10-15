#pragma once
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <vector>
#include <sstream>

namespace Flame {
    template <typename T>
    using Opt = std::optional<T>;

    template <typename T>
    using vector = std::vector<T>;

    using ostream = std::ostream;
    using string = std::string;

    template <typename T>
    using uptr = std::unique_ptr<T>;

    template <typename T>
    static ostream& PrintVec(ostream& os, const vector<T>& ls) {
        os << '[';
        for (size_t i = 0; i < ls.size(); i++) {
            os << ls[i];
            if (i + 1 < ls.size()) os << ", ";
        }
        os << ']';
        return os;
    }

    template <typename T>
    static ostream& PrintVecLine(ostream& os, const vector<T>& ls) {
        if (ls.empty()) return os << "[]";
        if (ls.size() == 1) return os << "[" << ls[0] << "]";
        os << "[\n";
        for (size_t i = 0; i < ls.size(); i++) {
            std::stringstream ss;
            ss << "  " << ls[i];
            for (size_t j = 0; j < ss.str().size(); j++) {
                if (ss.str()[j] == '\n') os << "\n  ";
                else os << ss.str()[j];
            }
            if (i + 1 < ls.size()) os << ",\n";
        }
        os << "\n]";
        return os;
    }

    template <typename T>
    static ostream& PrintVecP(ostream& os, const vector<T*>& ls) {
        os << '[';
        for (size_t i = 0; i < ls.size(); i++) {
            os << *ls[i];
            if (i + 1 < ls.size()) os << ", ";
        }
        os << ']';
        return os;
    }

    template <typename T>
    ostream& operator<<(ostream& os, const Opt<T>& opt) {
        if (opt.has_value()) os << opt.value();
        else os << "null";
        return os;
    }

    string GetFileContent(const string& filename);
    void GetFileContent(const string& filename, string& content);

    template <typename T>
    static ostream& operator<<(ostream& os, const vector<T>& ls) {
        PrintVecLine(os, ls);
        return os;
    }
}
