#pragma once
#include <iostream>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <vector>
#include <sstream>
#include <unordered_map>
#include <variant>

namespace Flame {
    template <typename T>
    using optional = std::optional<T>;

    template <typename T>
    using vector = std::vector<T>;
    template <typename T, typename K>
    using hashmap = std::unordered_map<T, K>;
    template <typename... Types>
    using variant = std::variant<Types...>;

    using ostream = std::ostream;
    using string = std::string;
    using string_view = std::string_view;

    template <typename T>
    using uptr = std::unique_ptr<T>;

    template <typename T>
    static ostream& printVec(ostream& os, const vector<T>& ls) {
        os << '[';
        for (size_t i = 0; i < ls.size(); i++) {
            os << ls[i];
            if (i + 1 < ls.size()) os << ", ";
        }
        os << ']';
        return os;
    }

    template <typename T>
    static ostream& printVecLine(ostream& os, const vector<T>& ls, const string& indent = "") {
        if (ls.empty()) return os << "[]";
        if (ls.size() == 1) return os << "[" << ls[0] << "]";
        os << "[\n";
        for (size_t i = 0; i < ls.size(); i++) {
            std::stringstream ss;
            ss << indent << "  " << ls[i];
            for (size_t j = 0; j < ss.str().size(); j++) {
                if (ss.str()[j] == '\n') os << '\n' << indent << "  ";
                else os << ss.str()[j];
            }
            if (i + 1 < ls.size()) os << ",\n";
        }
        os << '\n' << indent << ']';
        return os;
    }

    template <typename T>
    ostream& operator<<(ostream& os, const optional<T>& opt) {
        if (opt.has_value()) os << opt.value();
        else os << "null";
        return os;
    }

    void getFileContent(const string& filename, string& content);

    template <typename T>
    static ostream& operator<<(ostream& os, const vector<T>& ls) {
        printVecLine(os, ls);
        return os;
    }

    template <typename... Args>
    static void println(Args&&... args) {
        (std::cout << ... << args) << std::endl;
    }

    template <typename... Args>
    static void print(Args&&... args) {
        (std::cout << ... << args);
    }
}
