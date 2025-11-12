#include "lexer.hpp"
#include "utfcpp/utf8.h"

using namespace Flame;

namespace Flame {
    ostream& operator<<(ostream& os, OperatorType op) {
        return os << operators[static_cast<int>(op) - static_cast<int>(TokenType::Operators)];
    }

    ostream& operator<<(ostream& os, SymbolType op) {
        return os << symbols[static_cast<int>(op) - static_cast<int>(TokenType::Symbols)];
    }
}

string Flame::getTokenValue(TokenType type) {
    if (type <= TokenType::NewLine) return getTokenTypeName(type);
    if (type < TokenType::KeywordsEnd) {
        return keywords[static_cast<int>(type) - static_cast<int>(TokenType::Keywords)];
    }
    if (type <= TokenType::OperatorsEnd) {
        return operators[static_cast<int>(type) - static_cast<int>(TokenType::Operators)];
    }
    return string{symbols[static_cast<int>(type) - static_cast<int>(TokenType::Symbols)]};
}

[[nodiscard]] constexpr bool isOperatorChar(utf8::utfchar32_t c) {
    for (int i = 0; i < std::size(operators); i++) if (c == operators[i][0]) return true;
    return false;
}

[[nodiscard]] constexpr int FileTokenizer::getOperator() const {
    for (int j = 0; j < std::size(operators); j++) if (stringStartsWith(it, operators[j])) return j;
    return -1;
}

[[nodiscard]] constexpr int getSymbol(utf8::utfchar32_t c) {
    for (int i = 0; i < std::size(symbols); i++) if (c == symbols[i]) return i;
    return -1;
}

[[nodiscard]] constexpr bool isIdentifierChar(utf8::utfchar32_t c) {
    return getSymbol(c) == -1 && !isOperatorChar(c) && !isSpace(c) && !isNewLine(c) && c != ';';
}

TokenType operator+(TokenType lhs, int rhs) {
    return static_cast<TokenType>(static_cast<int>(lhs) + rhs);
}

void FileTokenizer::Tokenize() {
    size_t fstrings = 0;
    while (!over()) {
        auto c = peek();

        if (isNewLine(c) || c == ';') {
            addToken(TokenType::NewLine, 1);
            skip();
            continue;
        }

        if (isSpace(c)) {
            skip();
            continue;
        }

        if (isDigit(c)) {
            auto start = it;
            skip();
            bool is_float = false;
            skipDigits();
            if (peek() == '.' && isDigit(peek(1))) {
                is_float = true;
                skip();
                skip();
                skipDigits();
                auto c2 = peek();
                if (c2 == 'e' || c2 == 'E') {
                    skip();
                    if (!isDigit(peek())) {
                        throwError("Expected a digit");
                    }
                    skipDigits();
                }
            }
            addToken(is_float ? TokenType::Float : TokenType::Integer, start, it - start);
            continue;
        }

        if (fstrings > 0 && c == '}') {
            skip();
            fstrings--;
            bool slash = false;
            auto start = it;
            utf8::utfchar32_t last{0};
            while (!over()) {
                last = next();
                if (last == '"' && !slash) break;

                if (last == '$' && !slash) {
                    last = peek();
                    if (last == '{') {
                        skip();
                        break;
                    }
                    addToken(TokenType::StringMiddle, start, it - start);
                    auto id_index = it;
                    while (!over()) {
                        last = next();
                        if (!isIdentifierChar(last)) break;
                    }
                    addToken(TokenType::Identifier, id_index, it - id_index);
                    start = it;
                    continue;
                }

                if (last == '\\') {
                    slash = !slash;
                } else slash = false;
            }
            if (last == '"') {
                addToken(TokenType::StringEnd, start, it - start - 1);
            } else if (last == '{') {
                addToken(TokenType::StringMiddle, start, it - start - 2);
                fstrings++;
            } else {
                throwError("Unterminated string literal", start);
            }
            continue;
        }

        if (c == '.' && peek(1) == '.' && peek(2) == '.') {
            addToken(TokenType::SymbolEllipsis, it, 3);
            skip();
            skip();
            skip();
            continue;
        }

        if (c == '-' && peek(1) == '>') {
            addToken(TokenType::SymbolArrow, it, 2);
            skip();
            skip();
            continue;
        }

        if (c == ':' && peek(1) == '=') {
            addToken(TokenType::SymbolWalrus, it, 2);
            skip();
            skip();
            continue;
        }

        int sym_index = getSymbol(c);

        if (sym_index != -1) {
            addToken(TokenType::Symbols + sym_index, it, 1);
            skip();
            continue;
        }

        if (c == '/' && peek(1) == '/') {
            skip();
            skip();
            while (isNewLine(peek())) {
                skip();
            }
            continue;
        }

        int op_index = getOperator();

        if (op_index != -1) {
            auto& op = operators[op_index];
            addToken(TokenType::Operators + op_index, it, op.length());
            it += op.length();
            continue;
        }

        if (c == '\'') {
            continue;
        }

        if (c == '"') {
            bool slash = false;
            bool has_format = false;
            skip();
            auto start = it;
            utf8::utfchar32_t last{0};
            while (!over()) {
                last = next();
                if (last == '"' && !slash) {
                    break;
                }
                if (last == '$' && !slash) {
                    last = peek();
                    if (last == '{') {
                        skip();
                        break;
                    }
                    addToken(has_format ? TokenType::StringMiddle : TokenType::StringStart, start, it - start - 2);
                    has_format = true;
                    auto id_index = it;
                    while (!over()) {
                        skip();
                        last = peek();
                        if (!isIdentifierChar(last)) break;
                    }
                    addToken(TokenType::Identifier, id_index, it - id_index);
                    start = it;
                    continue;
                }
                if (last == '\\') {
                    slash = !slash;
                } else slash = false;
            }

            if (!has_format && last == '"') {
                addToken(TokenType::String, start, it - start - 1);
                continue;
            }

            if (last == '{') {
                addToken(has_format ? TokenType::StringMiddle : TokenType::StringStart, start, it - start - 2);
                fstrings++;
            }

            if (over()) {
                throwError("Unterminated string literal", start);
            }
        }

        auto start = it;

        while (!over()) {
            skip();
            auto c2 = peek();
            if (!isIdentifierChar(c2) && !isDigit(c2)) break;
        }

        auto len = it - start;

        if (len > 0) {
            bool is_keyword = false;

            for (int j = 0; j < std::size(keywords); j++) {
                auto& kw = keywords[j];
                if (len == kw.length() && stringStartsWith(start, kw)) {
                    addToken(TokenType::Keywords + j, start, len);
                    is_keyword = true;
                    break;
                }
            }
            if (!is_keyword) {
                addToken(TokenType::Identifier, start, len);
            }
            continue;
        }

        throwError("Unexpected character");
    }

    tokens.shrink_to_fit();
}
