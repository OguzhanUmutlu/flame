#pragma once
#include <variant>

#include "lexer.hpp"

namespace Flame {
    struct Value {
        enum class Type {
            None,
            Integer,
            Double,
            String,
            Stack
        };

        union Data {
            int64_t intValue;
            double doubleValue;
            std::string stringValue;
            Value* stackValue;

            Data() : intValue(0) {
            }

            ~Data() {
            }
        };
    };

    struct FunctionDefinition {
    };

    struct Module {
        IdentifierToken name;
        std::vector<Module> children;
    };

    struct Analyzer {
    };
}
