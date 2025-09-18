#pragma once
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

namespace Flame {
    template <typename T>
    using Opt = std::optional<T>;

    template <typename T>
    static std::ostream& PrintVec(std::ostream& os, const std::vector<T>& ls) {
        os << '[';
        for (size_t i = 0; i < ls.size(); i++) {
            os << ls[i];
            if (i + 1 < ls.size()) os << ", ";
        }
        os << ']';
        return os;
    }

    template <typename T>
    static std::ostream& PrintVecP(std::ostream& os, const std::vector<T*>& ls) {
        os << '[';
        for (size_t i = 0; i < ls.size(); i++) {
            os << *ls[i];
            if (i + 1 < ls.size()) os << ", ";
        }
        os << ']';
        return os;
    }

    template <typename T>
    std::ostream& operator<<(std::ostream& os, const Opt<T>& opt) {
        if (opt.has_value()) os << opt.value();
        else os << "null";
        return os;
    }

    template <typename T>
    std::ostream& operator<<(std::ostream& os, const std::unique_ptr<T>& opt) {
        if (opt) os << *opt;
        else os << "null";
        return os;
    }

    std::string GetFileContent(const std::string& filename);
    void GetFileContent(const std::string& filename, std::string& content);

    template <typename T>
    static std::ostream& operator<<(std::ostream& os, const std::vector<T>& ls) {
        PrintVec(os, ls);
        return os;
    }
}
