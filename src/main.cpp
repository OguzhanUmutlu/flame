#include <iostream>
#include <ostream>

#include "lexer.hpp"
#include "utils.hpp"

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <filename>" << std::endl;
        return 1;
    }

    std::string content;
    Flame::GetFileContent(argv[1], content);

    std::vector<Flame::Token> tokens;
    Flame::Tokenize(content, tokens);

    Flame::DebugTokens(tokens);
    return 0;
}
