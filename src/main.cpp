#include <iostream>
#include <ostream>

#include "lexer.hpp"
#include "parser.hpp"
#include "utils.hpp"

// todo: optimize compile time loops where if let's say there are 50 empty cases and 1 valid case just run that one instead of looping
//     llvm might do this automatically but not sure

using namespace Flame;

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <filename>" << std::endl;
        return 1;
    }

    Arena arena{};

    Tokenizer tokenizer;

    tokenizer.Tokenize(argv[1]);

    // for (const auto& tok : tokenizer.files.begin()->second.tokens) {
    //     std::cout << tok << std::endl;
    // }

    auto parser = Parser(arena, tokenizer);
    std::vector<ASTNode> statements;

    parser.ParseStatements(statements);

    std::cout << statements << std::endl;

    return 0;
}
