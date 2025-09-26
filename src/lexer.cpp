#include "lexer.hpp"
#include "utfcpp/utf8.h"

#include <cstring>

using namespace Flame;

constexpr char charOperators[] = {
    '=', '+', '-', '*', '/', '%', '!', '<', '>', '&', '|', '^', '~', '@'
};
const std::string operators[] = {
    "<<=", ">>=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "@=",
    "==", "!=", "<=", ">=", "&&", "||", "++", "--", "<<", ">>"
};
constexpr char symbols[] = {
    '(', ')', '{', '}', '[', ']', ',', ':', '.', '?'
};
const std::string keywords[] = {
    "if", "else", "while", "for", "return", "fun", "class",
    "import", "var", "val", "break", "continue", "when", "try",
    "catch", "finally", "throw", "get", "set", "default", "delete",
    "in", "as", "do", "alias", "enum"
};

const std::string TRUE_STRING = "true";
const std::string FALSE_STRING = "false";

std::string Flame::GetTokenValue(TokenType type) {
    if (type <= TokenType::NewLine) return GetTokenTypeName(type);
    if (type >= TokenType::Keywords && type < TokenType::OperatorChars) {
        return keywords[static_cast<int>(type) - static_cast<int>(TokenType::Keywords)];
    }
    if (type >= TokenType::OperatorChars && type < TokenType::OperatorStrings) {
        return std::string{charOperators[static_cast<int>(type) - static_cast<int>(TokenType::OperatorChars)]};
    }
    if (type >= TokenType::OperatorStrings && type < TokenType::Symbols) {
        return operators[static_cast<int>(type) - static_cast<int>(TokenType::OperatorStrings)];
    }
    return std::string{symbols[static_cast<int>(type) - static_cast<int>(TokenType::Symbols)]};
}

[[nodiscard]] constexpr int GetOperator(utf8::utfchar32_t c) {
    int size = std::size(charOperators);
    for (int i = 0; i < size; i++) if (c == charOperators[i]) return i;
    return -1;
}

[[nodiscard]] constexpr int GetSymbol(utf8::utfchar32_t c) {
    constexpr int size = std::size(symbols);
    for (int i = 0; i < size; i++) if (c == symbols[i]) return i;
    return -1;
}

[[nodiscard]] constexpr bool IsIdentifierChar(utf8::utfchar32_t c) {
    return GetSymbol(c) == -1 && GetOperator(c) == -1 && !IsSpace(c);
}

TokenType operator+(TokenType lhs, int rhs) {
    return static_cast<TokenType>(static_cast<int>(lhs) + rhs);
}

void FileTokenizer::Tokenize() {
    size_t fstrings = 0;
    while (!Over()) {
        auto c = Peek();

        if (IsNewLine(c) || c == ';') {
            AddToken(TokenType::NewLine, 1);
            Skip();
            continue;
        }

        if (IsSpace(c)) {
            Skip();
            continue;
        }

        if (IsDigit(c)) {
            auto start = it;
            Skip();
            bool is_float = false;
            SkipDigits();
            if (Peek() == '.' && IsDigit(Peek(1))) {
                is_float = true;
                Skip();
                Skip();
                SkipDigits();
                auto c2 = Peek();
                if (c2 == 'e' || c2 == 'E') {
                    Skip();
                    if (!IsDigit(Peek())) {
                        ThrowError("Expected a digit");
                    }
                    SkipDigits();
                }
            }
            AddToken(is_float ? TokenType::Float : TokenType::Integer, start, it - start);
            continue;
        }

        if (fstrings > 0 && c == '}') {
            Skip();
            fstrings--;
            bool slash = false;
            auto start = it;
            utf8::utfchar32_t last{0};
            while (!Over()) {
                last = Next();
                if (last == '"' && !slash) break;

                if (last == '$' && !slash) {
                    last = Peek();
                    if (last == '{') {
                        Skip();
                        break;
                    }
                    AddToken(TokenType::StringMiddle, start, it - start);
                    auto id_index = it;
                    while (!Over()) {
                        last = Next();
                        if (!IsIdentifierChar(last)) break;
                    }
                    AddToken(TokenType::Identifier, id_index, it - id_index);
                    start = it;
                    continue;
                }

                if (last == '\\') {
                    slash = !slash;
                } else slash = false;
            }
            if (last == '"') {
                AddToken(TokenType::StringEnd, start, it - start - 1);
            } else if (last == '{') {
                AddToken(TokenType::StringMiddle, start, it - start - 2);
                fstrings++;
            } else {
                ThrowError("Unterminated string literal", start);
            }
            continue;
        }

        if (c == '.' && Peek(1) == '.' && Peek(2) == '.') {
            AddToken(TokenType::SymbolEllipsis, it, 3);
            Skip();
            Skip();
            Skip();
            continue;
        }

        if (c == '-' && Peek(1) == '>') {
            AddToken(TokenType::SymbolArrow, it, 2);
            Skip();
            Skip();
            continue;
        }

        int sym_index = GetSymbol(c);

        if (sym_index != -1) {
            AddToken(TokenType::Symbols + sym_index, it, 1);
            Skip();
            continue;
        }

        if (c == '/' && Peek(1) == '/') {
            Skip();
            Skip();
            while (IsNewLine(Peek())) {
                Skip();
            }
            continue;
        }

        int op_index = GetOperator(c);

        if (op_index != -1) {
            bool found = false;
            int size = std::size(operators);
            for (int j = 0; j < size; j++) {
                auto& op = operators[j];
                if (StringStartsWith(it, op)) {
                    AddToken(TokenType::OperatorStrings + j, it, op.length());
                    it += op.length();
                    found = true;
                    break;
                }
            }
            if (!found) {
                AddToken(TokenType::OperatorChars + op_index, it, 1);
                Skip();
            }
            continue;
        }

        if (c == '\'') {
            continue;
        }

        if (c == '"') {
            bool slash = false;
            bool has_format = false;
            Skip();
            auto start = it;
            utf8::utfchar32_t last{0};
            while (!Over()) {
                last = Next();
                if (last == '"' && !slash) {
                    break;
                }
                if (last == '$' && !slash) {
                    last = Peek();
                    if (last == '{') {
                        Skip();
                        break;
                    }
                    AddToken(has_format ? TokenType::StringMiddle : TokenType::StringStart, start, it - start - 2);
                    has_format = true;
                    auto id_index = it;
                    while (!Over()) {
                        Skip();
                        last = Peek();
                        if (!IsIdentifierChar(last)) break;
                    }
                    AddToken(TokenType::Identifier, id_index, it - id_index);
                    start = it;
                    continue;
                }
                if (last == '\\') {
                    slash = !slash;
                } else slash = false;
            }

            if (!has_format && last == '"') {
                AddToken(TokenType::String, start, it - start - 1);
                continue;
            }

            if (last == '{') {
                AddToken(has_format ? TokenType::StringMiddle : TokenType::StringStart, start, it - start - 2);
                fstrings++;
            }

            if (Over()) {
                ThrowError("Unterminated string literal", start);
            }
        }

        auto start = it;

        while (!Over()) {
            Skip();
            auto c2 = Peek();
            if (!IsIdentifierChar(c2) && !IsDigit(c2)) break;
        }

        auto len = it - start;
        if (len == TRUE_STRING.length() && StringStartsWith(start, TRUE_STRING)) {
            AddToken(TokenType::Boolean, start, 4);
            continue;
        }

        if (len == FALSE_STRING.length() && StringStartsWith(start, FALSE_STRING)) {
            AddToken(TokenType::Boolean, start, 5);
            continue;
        }

        if (len > 0) {
            bool is_keyword = false;
            int size = std::size(keywords);

            for (int j = 0; j < size; j++) {
                auto& kw = keywords[j];
                if (len == kw.length() && StringStartsWith(start, kw)) {
                    AddToken(TokenType::Keywords + j, start, len);
                    is_keyword = true;
                    break;
                }
            }
            if (!is_keyword) {
                AddToken(TokenType::Identifier, start, len);
            }
            continue;
        }

        ThrowError("Unexpected character");
    }

    tokens.shrink_to_fit();
}
