#include <iostream>
#include <ostream>

#include "compiler.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "utils.hpp"

using namespace Flame;


#include <stdio.h>

struct X {
    int x;
    float y;

    X(int x, float y) : x(x), y(y) {
    }

    virtual void test() {
    }
};

int main(int argc, char* argv[]) {
    if (argc != 2) {
        println("Usage: ", argv[0], " <filename>");
        return 1;
    }

    Arena arena{};

    Tokenizer tokenizer;

    tokenizer.tokenize(argv[1]);

    println(tokenizer);

    Parser parser(arena, tokenizer);

    vector<ASTNode> statements;
    parser.parseStatements(statements);

    println(statements);

    Compiler compiler(parser, statements);

    compiler.compile();

    return 0;
}
