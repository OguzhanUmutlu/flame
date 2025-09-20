#include "lexer.hpp"

#include <cstring>

using namespace Flame;

constexpr char charOperators[] = {
    '=', '+', '-', '*', '/', '%', '!', '<', '>', '&', '|', '^', '~'
};
const char* operators[] = {
    "<<=", ">>=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=",
    "==", "!=", "<=", ">=", "&&", "||", "++", "--", "<<", ">>"
};
constexpr char symbols[] = {
    '(', ')', '{', '}', '[', ']', ',', ':', '.', '?'
};
const char* keywords[] = {
    "if", "else", "while", "for", "return", "fun", "class",
    "import", "var", "val", "break", "continue", "when", "try",
    "catch", "finally", "throw", "get", "set", "default", "delete",
    "in", "as", "do", "alias", "enum"
};

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

int GetOperator(char c) {
    int size = std::size(charOperators);
    for (int i = 0; i < size; i++) if (c == charOperators[i]) return i;
    return -1;
}

int GetSymbol(char c) {
    constexpr int size = std::size(symbols);
    for (int i = 0; i < size; i++) if (c == symbols[i]) return i;
    return -1;
}

bool IsIdentifierChar(char c) {
    return GetSymbol(c) == -1 && GetOperator(c) == -1 && !std::isspace(c);
}

TokenType operator+(TokenType lhs, int rhs) {
    return static_cast<TokenType>(static_cast<int>(lhs) + rhs);
}

void FileTokenizer::Tokenize() {
    auto addToken = [&](TokenType type, size_t start, size_t length) {
        tokens.emplace_back(type, content.data() + start, length);
    };

    size_t i = 0;
    size_t fstrings = 0;
    while (i < content.size()) {
        char c = content[i];

        if (c == '\n' || c == ';') {
            if (tokens.empty() || tokens.back().type != TokenType::NewLine) {
                addToken(TokenType::NewLine, i, 1);
            }
            i++;
            continue;
        }

        if (std::isspace(c)) {
            i++;
            continue;
        }

        if (std::isdigit(c)) {
            size_t start = i;
            bool is_float = false;
            i++;
            while (i < content.size()) {
                char c2 = content[i];
                if (std::isdigit(c2) || c2 == '_') i++;
                else break;
            }
            if (content[i] == '.'
                && i + 1 < content.size()
                && std::isdigit(content[i + 1])
            ) {
                is_float = true;
                i++;
                while (i < content.size()) {
                    char c2 = content[i];
                    if (std::isdigit(c2) || c2 == '_') i++;
                    else break;
                }
                char c2 = content[i];
                if (c2 == 'e' || c2 == 'E') {
                    i++;
                    if (i < content.size() || !std::isdigit(content[i])) {
                        ThrowError("Expected a digit", i);
                    }
                    while (i < content.size()) {
                        char c3 = content[i];
                        if (std::isdigit(c3) || c3 == '_') i++;
                        else break;
                    }
                }
            }
            addToken(is_float ? TokenType::Float : TokenType::Integer, start, i - start);
            continue;
        }

        if (fstrings > 0 && c == '}') {
            i++;
            fstrings--;
            bool slash = false;
            size_t start = i;
            while (i < content.size()) {
                char c2 = content[i];
                i++;
                if (c2 == '"' && !slash) {
                    break;
                }
                if (c2 == '$' && !slash) {
                    c2 = content[i];
                    if (c2 == '{') {
                        i++;
                        break;
                    }
                    addToken(TokenType::StringMiddle, start, i - start);
                    size_t id_index = i;
                    while (i < content.size()) {
                        i++;
                        c2 = content[i];
                        if (!IsIdentifierChar(c2)) break;
                    }
                    addToken(TokenType::Identifier, id_index, i - id_index);
                    start = i;
                    continue;
                }
                if (c2 == '\\') {
                    slash = !slash;
                } else slash = false;
            }
            char c2 = content[i - 1];
            if (c2 == '"') {
                addToken(TokenType::StringEnd, start, i - start - 1);
            } else if (c2 == '{') {
                addToken(TokenType::StringMiddle, start, i - start - 2);
                fstrings++;
            } else {
                ThrowError("Unterminated string literal", start);
            }
            continue;
        }

        if (c == '.' && i + 2 < content.size() && content[i + 1] == '.' && content[i + 2] == '.') {
            addToken(TokenType::SymbolEllipsis, i, 3);
            i += 3;
            continue;
        }

        if (c == '-' && i + 1 < content.size() && content[i + 1] == '>') {
            addToken(TokenType::SymbolArrow, i, 2);
            i += 2;
            continue;
        }

        int sym_index = GetSymbol(c);

        if (sym_index != -1) {
            addToken(TokenType::Symbols + sym_index, i, 1);
            i++;
            continue;
        }

        if (c == '/' && i + 1 < content.size() && content[i + 1] == '/') {
            while (i < content.size() && content[i] != '\n') {
                i++;
            }
            continue;
        }

        int op_index = GetOperator(c);

        if (op_index != -1) {
            bool found = false;
            int size = std::size(operators);
            for (int j = 0; j < size; j++) {
                const char* op = operators[j];
                size_t len = strlen(op);
                if (content.compare(i, len, op) == 0) {
                    addToken(TokenType::OperatorStrings + j, i, len);
                    i += len;
                    found = true;
                    break;
                }
            }
            if (!found) {
                addToken(TokenType::OperatorChars + op_index, i, 1);
                i++;
            }
            continue;
        }

        if (c == '"') {
            bool slash = false;
            bool has_format = false;
            i++;
            size_t start = i;
            while (i < content.size()) {
                char c2 = content[i];
                i++;
                if (c2 == '"' && !slash) {
                    break;
                }
                if (c2 == '$' && !slash) {
                    c2 = content[i];
                    if (c2 == '{') {
                        i++;
                        break;
                    }
                    addToken(has_format ? TokenType::StringMiddle : TokenType::StringStart, start, i - start - 2);
                    has_format = true;
                    size_t id_index = i;
                    while (i < content.size()) {
                        i++;
                        c2 = content[i];
                        if (!IsIdentifierChar(c2)) break;
                    }
                    addToken(TokenType::Identifier, id_index, i - id_index);
                    start = i;
                    continue;
                }
                if (c2 == '\\') {
                    slash = !slash;
                } else slash = false;
            }

            char c2 = content[i - 1];
            if (!has_format && c2 == '"') {
                addToken(TokenType::String, start, i - start - 1);
                continue;
            }

            if (c2 == '{') {
                addToken(has_format ? TokenType::StringMiddle : TokenType::StringStart, start, i - start - 2);
                fstrings++;
            }

            if (i >= content.size()) {
                ThrowError("Unterminated string literal", start);
            }
        }

        size_t start = i;

        while (i < content.size()) {
            i++;
            char c2 = content[i];
            if (!IsIdentifierChar(c2) && !std::isdigit(c2)) break;
        }

        size_t len = i - start;
        if (len == 4 && content.compare(start, 4, "true") == 0) {
            addToken(TokenType::Boolean, start, 4);
            continue;
        }

        if (len == 5 && content.compare(start, 5, "false") == 0) {
            addToken(TokenType::Boolean, start, 5);
            continue;
        }

        if (len > 0) {
            bool is_keyword = false;
            int size = std::size(keywords);

            for (int j = 0; j < size; j++) {
                const char* kw = keywords[j];
                if (len == strlen(kw) && content.compare(start, len, kw) == 0) {
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

        ThrowError("Unexpected character", i);
    }

    tokens.shrink_to_fit();
}
