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
    "catch", "finally", "throw"
};

int GetOperator(char c) {
    int size = std::size(charOperators);
    for (int i = 0; i < size; i++) if (c == charOperators[i]) return i;
    return -1;
}

int GetSymbol(char c) {
    int size = std::size(symbols);
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
    size_t i = 0;
    size_t fstrings = 0;
    while (i < content.size()) {
        char c = content[i];

        if (c == '\n' || c == ';') {
            if (tokens.empty() || tokens.back().type != TokenType::NewLine) {
                tokens.emplace_back(TokenType::NewLine, content.data() + i, 1);
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
            tokens.emplace_back(is_float ? TokenType::Float : TokenType::Integer,
                                content.data() + start, i - start);
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
                    tokens.emplace_back(TokenType::StringMiddle, content.data() + start, i - start);
                    size_t id_index = i;
                    while (i < content.size()) {
                        i++;
                        c2 = content[i];
                        if (!IsIdentifierChar(c2)) break;
                    }
                    tokens.emplace_back(TokenType::Identifier, content.data() + id_index, i - id_index);
                    start = i;
                    continue;
                }
                if (c2 == '\\') {
                    slash = !slash;
                } else slash = false;
            }
            char c2 = content[i - 1];
            if (c2 == '"') {
                tokens.emplace_back(TokenType::StringEnd, content.data() + start, i - start - 1);
            } else if (c2 == '{') {
                tokens.emplace_back(TokenType::StringMiddle, content.data() + start, i - start - 2);
                fstrings++;
            } else {
                ThrowError("Unterminated string literal", start);
            }
            continue;
        }

        if (c == '-' && i + 1 < content.size() && content[i + 1] == '>') {
            tokens.emplace_back(TokenType::SymbolArrow, content.data() + i, 2);
            i += 2;
            continue;
        }

        int sym_index = GetSymbol(c);

        if (sym_index != -1) {
            tokens.emplace_back(TokenType::Symbols + sym_index, content.data() + i, 1);
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
                    tokens.emplace_back(TokenType::OperatorStrings + j, content.data() + i, len);
                    i += len;
                    found = true;
                    break;
                }
            }
            if (!found) {
                tokens.emplace_back(TokenType::OperatorChars + op_index, content.data() + i, 1);
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
                    tokens.emplace_back(has_format ? TokenType::StringMiddle : TokenType::StringStart,
                                        content.data() + start, i - start - 2);
                    has_format = true;
                    size_t id_index = i;
                    while (i < content.size()) {
                        i++;
                        c2 = content[i];
                        if (!IsIdentifierChar(c2)) break;
                    }
                    tokens.emplace_back(TokenType::Identifier, content.data() + id_index, i - id_index);
                    start = i;
                    continue;
                }
                if (c2 == '\\') {
                    slash = !slash;
                } else slash = false;
            }

            char c2 = content[i - 1];
            if (!has_format && c2 == '"') {
                tokens.emplace_back(TokenType::String, content.data() + start, i - start);
                continue;
            }

            if (c2 == '{') {
                tokens.emplace_back(has_format ? TokenType::StringMiddle : TokenType::StringStart,
                                    content.data() + start, i - start - 2);
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
            tokens.emplace_back(TokenType::Boolean, "true", 4);
            continue;
        }

        if (len == 5 && content.compare(start, 5, "false") == 0) {
            tokens.emplace_back(TokenType::Boolean, "false", 5);
            continue;
        }

        if (len > 0) {
            bool is_keyword = false;
            int size = std::size(keywords);

            for (int j = 0; j < size; j++) {
                const char* kw = keywords[j];
                if (len == strlen(kw) && content.compare(start, len, kw) == 0) {
                    tokens.emplace_back(TokenType::Keywords + j, kw, len);
                    is_keyword = true;
                    break;
                }
            }
            if (!is_keyword) {
                tokens.emplace_back(TokenType::Identifier, content.data() + start, len);
            }
            continue;
        }

        ThrowError("Unexpected character", i);
    }

    tokens.shrink_to_fit();
}
