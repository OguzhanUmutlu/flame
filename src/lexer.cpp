#include "lexer.hpp"

#include <iostream>

int GetOperator(char c) {
    for (int i = 0; i < std::size(charOperators); i++) if (c == charOperators[i]) return i;
    return -1;
}

int GetSymbol(char c) {
    for (int i = 0; i < std::size(symbols); i++) if (c == symbols[i]) return i;
    return -1;
}

bool IsIdentifierChar(char c) {
    return GetSymbol(c) == -1 && GetOperator(c) == -1 && !std::isspace(c);
}

Flame::Token::Type operator+(Flame::Token::Type lhs, int rhs) {
    return static_cast<Flame::Token::Type>(static_cast<int>(lhs) + rhs);
};

void Flame::Tokenize(const std::string& content, std::vector<Token>& tokens) {
    int i = 0;
    size_t fstrings = 0;
    while (i < content.size()) {
        char c = content[i];

        if (c == '\n' || c == ';') {
            if (tokens.empty() || tokens.back().type != Token::Type::NewLine) {
                tokens.emplace_back(Token::Type::NewLine, content.data() + i, 1);
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
                        throw std::runtime_error("Expected a digit");
                    }
                    while (i < content.size()) {
                        char c3 = content[i];
                        if (std::isdigit(c3) || c3 == '_') i++;
                        else break;
                    }
                }
            }
            tokens.emplace_back(is_float ? Token::Type::Float : Token::Type::Integer,
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
                    tokens.emplace_back(Token::Type::StringMiddle,
                                        content.data() + start, i - start);
                    size_t id_index = i;
                    while (i < content.size()) {
                        i++;
                        c2 = content[i];
                        if (!IsIdentifierChar(c2)) break;
                    }
                    tokens.emplace_back(Token::Type::Identifier, content.data() + id_index, i - id_index);
                    start = i;
                    continue;
                }
                if (c2 == '\\') {
                    slash = !slash;
                } else slash = false;
            }
            char c2 = content[i - 1];
            if (c2 == '"') {
                tokens.emplace_back(Token::Type::StringEnd, content.data() + start, i - start);
            } else if (c2 == '{') {
                tokens.emplace_back(Token::Type::StringMiddle, content.data() + start, i - start);
                fstrings++;
            } else {
                throw std::runtime_error("Unterminated string literal");
            }
            continue;
        }

        int sym_index = GetSymbol(c);

        if (sym_index != -1) {
            tokens.emplace_back(Token::Type::Symbols + sym_index, content.data() + i, 1);
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
            for (int j = 0; j < std::size(operators); j++) {
                const char* op = operators[j];
                size_t len = strlen(op);
                if (content.compare(i, len, op) == 0) {
                    tokens.emplace_back(Token::Type::OperatorStrings + j, content.data() + i, len);
                    i += len;
                    found = true;
                    break;
                }
            }
            if (!found) {
                tokens.emplace_back(Token::Type::OperatorChars + op_index, content.data() + i, 1);
                i++;
            }
            continue;
        }

        if (c == '"') {
            size_t start = i;
            bool slash = false;
            bool has_format = false;
            i++;
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
                    tokens.emplace_back(has_format ? Token::Type::StringMiddle : Token::Type::StringStart,
                                        content.data() + start, i - start);
                    has_format = true;
                    size_t id_index = i;
                    while (i < content.size()) {
                        i++;
                        c2 = content[i];
                        if (!IsIdentifierChar(c2)) break;
                    }
                    tokens.emplace_back(Token::Type::Identifier, content.data() + id_index, i - id_index);
                    start = i;
                    continue;
                }
                if (c2 == '\\') {
                    slash = !slash;
                } else slash = false;
            }

            char c2 = content[i - 1];
            if (!has_format && c2 == '"') {
                tokens.emplace_back(Token::Type::String, content.data() + start, i - start);
                continue;
            }

            if (c2 == '{') {
                tokens.emplace_back(has_format ? Token::Type::StringMiddle : Token::Type::StringStart,
                                    content.data() + start, i - start);
                fstrings++;
            }

            if (i >= content.size()) {
                throw std::runtime_error("Unterminated string literal");
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
            tokens.emplace_back(Token::Type::Boolean, "true", 4);
            continue;
        }

        if (len == 5 && content.compare(start, 5, "false") == 0) {
            tokens.emplace_back(Token::Type::Boolean, "false", 5);
            continue;
        }

        if (len > 0) {
            bool is_keyword = false;
            for (int j = 0; j < std::size(keywords); j++) {
                const char* kw = keywords[j];
                if (len == strlen(kw) && content.compare(start, len, kw) == 0) {
                    tokens.emplace_back(Token::Type::Keywords + j, kw, len);
                    is_keyword = true;
                    break;
                }
            }
            if (!is_keyword) {
                tokens.emplace_back(Token::Type::Identifier, content.data() + start, len);
            }
            continue;
        }

        throw std::runtime_error("Unexpected character: " + std::string(1, c));
    }

    tokens.shrink_to_fit();
}

void Flame::DebugTokens(const std::vector<Token>& tokens) {
    for (const auto& token : tokens) {
        std::string type;
        switch (token.type) {
        case Token::Type::Identifier: type = "Identifier";
            break;
        case Token::Type::Integer: type = "Integer";
            break;
        case Token::Type::Float: type = "Float";
            break;
        case Token::Type::String: type = "String";
            break;
        case Token::Type::Boolean: type = "Boolean";
            break;
        case Token::Type::StringStart: type = "StringStart";
            break;
        case Token::Type::StringMiddle: type = "StringMiddle";
            break;
        case Token::Type::StringEnd: type = "StringEnd";
            break;
        case Token::Type::NewLine:
            std::cout << "[NewLine]\n";
            break;
        default:
            if (token.type >= Token::Type::Symbols) {
                type = "Symbol";
            } else if (token.type >= Token::Type::OperatorStrings) {
                type = "OperatorString";
            } else if (token.type >= Token::Type::OperatorChars) {
                type = "OperatorChar";
            } else if (token.type >= Token::Type::Keywords) {
                type = "Keyword";
            } else {
                type = "Unknown";
            }
            break;
        }

        if (!type.empty()) std::cout << "[" << type << "] '" << token.value << "'\n";
    }
}
