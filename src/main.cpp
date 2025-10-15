#include <iostream>
#include <ostream>

#include "lexer.hpp"
#include "parser.hpp"
#include "utils.hpp"

using namespace Flame;

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <filename>" << std::endl;
        return 1;
    }

    Arena arena{};

    Tokenizer tokenizer;

    tokenizer.Tokenize(argv[1]);

    std::cout << tokenizer.files.begin()->second.tokens << std::endl;

    auto parser = Parser(arena, tokenizer);
    std::vector<ASTNode*> statements;

    parser.ParseStatements(statements);

    std::cout << statements << std::endl;

    return 0;
}
