#include "utils.hpp"

#include <fstream>
#include <stdexcept>

std::string Flame::GetFileContent(const std::string& filename) {
    std::ifstream file(filename.data());
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + std::string(filename));
    }

    std::string content((std::istreambuf_iterator(file)), std::istreambuf_iterator<char>());
    file.close();
    return content;
}

void Flame::GetFileContent(const std::string& filename, std::string& content) {
    std::ifstream file(filename.data());
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + std::string(filename));
    }

    content = std::string((std::istreambuf_iterator(file)), std::istreambuf_iterator<char>());
    file.close();
}
