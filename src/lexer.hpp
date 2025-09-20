#pragma once
#include <algorithm>
#include <charconv>
#include <iostream>
#include <process.h>
#include <ranges>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "utils.hpp"

static bool PrintTokenValue = true;

namespace Flame {
    enum class TokenType {
        EndOfFile,
        Identifier,
        Integer,
        Float,
        String,
        Boolean,
        StringStart,
        StringMiddle,
        StringEnd,
        NewLine,

        Keywords,
        KeywordIf = Keywords, KeywordElse, KeywordWhile, KeywordFor, KeywordReturn, KeywordFun, KeywordClass,
        KeywordImport, KeywordVar, KeywordVal, KeywordBreak, KeywordContinue, KeywordWhen, KeywordTry, KeywordCatch,
        KeywordFinally, KeywordThrow, KeywordGet, KeywordSet, KeywordDefault, KeywordDelete, KeywordIn, KeywordAs,
        KeywordDo, KeywordAlias, KeywordEnum,

        OperatorChars, Operator = OperatorChars,
        OperatorAssign          = OperatorChars, OperatorAdd, OperatorSub, OperatorMul, OperatorDiv, OperatorMod,
        OperatorNot, OperatorLt, OperatorGt, OperatorBitAnd, OperatorBitOr, OperatorBitXor, OperatorBitNot,

        OperatorStrings,
        OperatorSetShl = OperatorStrings, OperatorSetShr, OperatorSetAdd, OperatorSetSub, OperatorSetMul,
        OperatorSetDiv, OperatorSetMod, OperatorSetBitAnd, OperatorSetBitOr, OperatorSetBitXor,

        OperatorEq, OperatorNeq, OperatorLte, OperatorGte, OperatorAnd, OperatorOr, OperatorInc, OperatorDec,
        OperatorShl, OperatorShr,

        Symbols, Symbol       = Symbols,
        SymbolLeftParenthesis = Symbols, SymbolRightParenthesis, SymbolLeftBrace, SymbolRightBrace, SymbolLeftBracket,
        SymbolRightBracket, SymbolComma, SymbolColon, SymbolDot, SymbolQuestion, SymbolArrow, SymbolEllipsis
    };

    static std::string GetTokenTypeName(TokenType type) {
        if (type == TokenType::EndOfFile) return "EndOfFile";
        if (type == TokenType::Identifier) return "Identifier";
        if (type == TokenType::Integer) return "Integer";
        if (type == TokenType::Float) return "Float";
        if (type == TokenType::String || type == TokenType::StringStart || type == TokenType::StringMiddle || type ==
            TokenType::StringEnd)
            return "String";
        if (type == TokenType::Boolean) return "Boolean";
        if (type == TokenType::NewLine) return "NewLine";
        if (type < TokenType::OperatorChars) return "Keyword";
        if (type < TokenType::Symbols) return "Operator";
        return "Symbol";
    }

    std::string GetTokenValue(TokenType type);

    enum class OperatorType {
        Assign = static_cast<int>(TokenType::OperatorAssign), Add, Sub, Mul, Div, Mod,
        Not, Lt, Gt, BitAnd, BitOr, BitXor, BitNot,

        SetShl, SetShr, SetAdd, SetSub, SetMul,
        SetDiv, SetMod, SetBitAnd, SetBitOr, SetBitXor,

        Eq, Neq, Lte, Gte, And, Or, Inc, Dec,
        Shl, Shr
    };

    enum class SymbolType {
        LeftPar = static_cast<int>(TokenType::SymbolLeftParenthesis), RightPar, LeftBrace, RightBrace, LeftBracket,
        RightBracket, Comma, Semicolon, Colon, Dot, Question, Arrow
    };

    struct TokenImpl {
        std::string_view value;

        TokenImpl() = default;

        explicit TokenImpl(std::string_view value) : value(value) {
        }

        TokenImpl(const char* start, size_t length) : value(start, length) {
        }

        [[nodiscard]] static constexpr bool is_literal(TokenType type) {
            return type == TokenType::Integer || type == TokenType::Float || type == TokenType::String || type ==
                TokenType::Boolean;
        }

        [[nodiscard]] static constexpr bool is_keyword(TokenType type) {
            return type >= TokenType::Keywords && type < TokenType::OperatorChars;
        }

        [[nodiscard]] static constexpr bool is_operator(TokenType type) {
            return (type >= TokenType::OperatorChars && type < TokenType::OperatorStrings)
                || (type >= TokenType::OperatorStrings && type < TokenType::Symbols);
        }

        [[nodiscard]] static constexpr bool is_symbol(TokenType type) {
            return type >= TokenType::Symbols && type < TokenType::Keywords;
        }

        [[nodiscard]] static constexpr bool is_set_operator(TokenType type) {
            return type == TokenType::OperatorAssign || (type >= TokenType::OperatorSetShl && type <=
                TokenType::OperatorSetBitXor);
        }

        [[nodiscard]] static constexpr bool is_arithmetic_operator(TokenType type) {
            return (type > TokenType::OperatorChars && type < TokenType::OperatorStrings)
                || (type >= TokenType::OperatorEq && type <= TokenType::OperatorShr);
        }

        [[nodiscard]] std::int64_t get_int() const {
            std::string_view sv = value;
            std::string cleaned;

            if (std::ranges::find(sv, '_') != sv.end()) {
                cleaned.reserve(sv.size());
                std::ranges::copy_if(sv, std::back_inserter(cleaned),
                                     [](char c) {
                                         return c != '_';
                                     });
                sv = cleaned;
            }

            bool negative = false;
            if (!sv.empty() && (sv.front() == '-' || sv.front() == '+')) {
                negative = (sv.front() == '-');
                sv.remove_prefix(1);
            }

            int base = 10;
            if (sv.size() > 2 && sv[0] == '0') {
                switch (sv[1]) {
                case 'x':
                case 'X': base = 16;
                    sv.remove_prefix(2);
                    break;
                case 'b':
                case 'B': base = 2;
                    sv.remove_prefix(2);
                    break;
                case 'o':
                case 'O': base = 8;
                    sv.remove_prefix(2);
                    break;
                default:
                    break;
                }
            }

            std::int64_t result = 0;
            auto [ptr, ec] = std::from_chars(sv.data(), sv.data() + sv.size(), result, base);
            if (ec != std::errc() || ptr != sv.data() + sv.size()) {
                throw std::runtime_error("Invalid integer literal" + std::string(value));
            }

            return negative ? -result : result;
        }

        [[nodiscard]] double get_float() const {
            std::string_view sv = value;
            std::string cleaned;

            if (std::ranges::find(sv, '_') != sv.end()) {
                cleaned.reserve(sv.size());
                std::ranges::copy_if(sv, std::back_inserter(cleaned),
                                     [](char c) {
                                         return c != '_';
                                     });
                sv = cleaned;
            }

#if defined(__cpp_lib_to_chars) && __cpp_lib_to_chars >= 201611L
            double result;
            auto [ptr, ec] = std::from_chars(sv.data(), sv.data() + sv.size(), result);
            if (ec != std::errc() || ptr != sv.data() + sv.size()) {
                throw std::runtime_error("Invalid float literal: " + std::string(value));
            }
            return result;
#else
            char* endPtr = nullptr;
            double result = std::strtod(sv.data(), &endPtr);
            if (endPtr != sv.data() + sv.size()) {
                throw std::runtime_error("Invalid float literal: " + std::string(value));
            }
            return result;
#endif
        }
    };

    template <TokenType type>
    struct TokenComp : TokenImpl {
        using TokenImpl::TokenImpl;

        [[nodiscard]] static constexpr bool is_literal() {
            return TokenImpl::is_literal(type);
        }

        [[nodiscard]] static constexpr bool is_keyword() {
            return TokenImpl::is_keyword(type);
        }

        [[nodiscard]] static constexpr bool is_operator() {
            return TokenImpl::is_operator(type);
        }

        [[nodiscard]] static constexpr bool is_symbol() {
            return TokenImpl::is_symbol(type);
        }

        [[nodiscard]] static constexpr bool is_set_operator() {
            return TokenImpl::is_set_operator(type);
        }

        [[nodiscard]] static constexpr bool is_arithmetic_operator() {
            return TokenImpl::is_arithmetic_operator(type);
        }
    };

    template <TokenType T>
    static std::ostream& operator<<(std::ostream& os, const TokenComp<T>& tok) {
        if (PrintTokenValue) {
            os << '\'' << tok.value << '\'';
            return os;
        }
        os << "Token(type=";
        if constexpr (T == TokenType::Identifier) {
            os << "Identifier";
        } else if constexpr (T == TokenType::Integer) {
            os << "Integer";
        } else if constexpr (T == TokenType::Float) {
            os << "Float";
        } else if constexpr (T == TokenType::String) {
            os << "String";
        } else if constexpr (T == TokenType::Boolean) {
            os << "Boolean";
        } else if constexpr (T == TokenType::StringStart) {
            os << "StringStart";
        } else if constexpr (T == TokenType::StringMiddle) {
            os << "StringMiddle";
        } else if constexpr (T == TokenType::StringEnd) {
            os << "StringEnd";
        } else {
            os << "Unknown";
        }
        os << ", value='" << tok.value << "')";
        return os;
    }

    struct OperatorToken : TokenImpl {
        OperatorToken(OperatorType type, const std::string_view& value)
            : TokenImpl(value), type(type) {
        }

        OperatorToken(OperatorType type, const char* start, size_t length)
            : TokenImpl(start, length),
              type(type) {
        }

        OperatorType type;

        [[nodiscard]] constexpr bool is_set_operator() const {
            return TokenImpl::is_set_operator(static_cast<TokenType>(type));
        }

        [[nodiscard]] constexpr bool is_arithmetic_operator() const {
            return TokenImpl::is_arithmetic_operator(static_cast<TokenType>(type));
        }

        friend std::ostream& operator<<(std::ostream& os, const OperatorToken& token) {
            if (PrintTokenValue) {
                os << '\'' << token.value << '\'';
                return os;
            }
            os << "OperatorToken(" << token.value << ")";
            return os;
        }
    };

    struct SymbolToken : TokenImpl {
        SymbolToken(SymbolType type, const std::string_view& value)
            : TokenImpl(value), type(type) {
        }

        SymbolToken(SymbolType type, const char* start, size_t length)
            : TokenImpl(start, length),
              type(type) {
        }

        SymbolType type;

        friend std::ostream& operator<<(std::ostream& os, const SymbolToken& token) {
            if (PrintTokenValue) {
                os << '\'' << token.value << '\'';
                return os;
            }
            os << "SymbolToken(" << token.value << ")";
            return os;
        }
    };

    struct Token : TokenImpl {
        Token(TokenType type, const std::string_view& value)
            : TokenImpl(value), type(type) {
        }

        Token(TokenType type, const char* start, size_t length)
            : TokenImpl(start, length),
              type(type) {
        }

        TokenType type;

        [[nodiscard]] constexpr bool is_literal() const {
            return TokenImpl::is_literal(type);
        }

        [[nodiscard]] constexpr bool is_keyword() const {
            return TokenImpl::is_keyword(type);
        }

        [[nodiscard]] constexpr bool is_operator() const {
            return TokenImpl::is_operator(type);
        }

        [[nodiscard]] constexpr bool is_symbol() const {
            return TokenImpl::is_symbol(type);
        }

        [[nodiscard]] constexpr bool is_set_operator() const {
            return TokenImpl::is_set_operator(type);
        }

        [[nodiscard]] constexpr bool is_arithmetic_operator() const {
            return TokenImpl::is_arithmetic_operator(type);
        }

        template <TokenType T>
        constexpr TokenComp<T> comp() const {
            return TokenComp<T>(value);
        }

        [[nodiscard]] OperatorToken op() const {
            if (!is_operator()) throw std::runtime_error("Token is not an operator");
            return {static_cast<OperatorType>(type), value};
        }

        [[nodiscard]] constexpr OperatorType op_type() const {
            if (!is_operator()) throw std::runtime_error("Token is not an operator");
            return static_cast<OperatorType>(type);
        }

        [[nodiscard]] SymbolToken sym() const {
            if (!is_symbol()) throw std::runtime_error("Token is not a symbol");
            return {static_cast<SymbolType>(type), value};
        }

        [[nodiscard]] constexpr SymbolType sym_type() const {
            if (!is_symbol()) throw std::runtime_error("Token is not a symbol");
            return static_cast<SymbolType>(type);
        }

        friend std::ostream& operator<<(std::ostream& os, const Token& token) {
            if (PrintTokenValue) {
                os << '\'' << token.value << '\'';
                return os;
            }
            const char* typeName;
            switch (token.type) {
            case TokenType::Identifier: typeName = "Identifier";
                break;
            case TokenType::Integer: typeName = "Integer";
                break;
            case TokenType::Float: typeName = "Float";
                break;
            case TokenType::String: typeName = "String";
                break;
            case TokenType::Boolean: typeName = "Boolean";
                break;
            case TokenType::StringStart: typeName = "StringStart";
                break;
            case TokenType::StringMiddle: typeName = "StringMiddle";
                break;
            case TokenType::StringEnd: typeName = "StringEnd";
                break;
            case TokenType::NewLine:
                os << "Token(type=NewLine)";
                return os;
            default:
                if (token.type >= TokenType::Symbols) {
                    typeName = "Symbol";
                } else if (token.type >= TokenType::OperatorStrings) {
                    typeName = "OperatorString";
                } else if (token.type >= TokenType::OperatorChars) {
                    typeName = "OperatorChar";
                } else if (token.type >= TokenType::Keywords) {
                    typeName = "Keyword";
                } else {
                    typeName = "Unknown";
                }
                break;
            }

            os << "Token(type=" << typeName << ", value='" << token.value << "')";
            return os;
        }
    };

    using IdentifierToken = TokenComp<TokenType::Identifier>;
    using IntegerToken = TokenComp<TokenType::Integer>;
    using FloatToken = TokenComp<TokenType::Float>;
    using StringToken = TokenComp<TokenType::String>;
    using BooleanToken = TokenComp<TokenType::Boolean>;
    using StringStartToken = TokenComp<TokenType::StringStart>;
    using StringMiddleToken = TokenComp<TokenType::StringMiddle>;
    using StringEndToken = TokenComp<TokenType::StringEnd>;
    using NewLineToken = TokenComp<TokenType::NewLine>;

    constexpr auto RED = "\033[31m";
    constexpr auto YELLOW = "\033[33m";
    constexpr auto BOLD = "\033[1m";
    constexpr auto RESET = "\033[0m";

    struct FileTokenizer {
        std::string filepath;
        std::string content;
        std::vector<Token> tokens;

        void Tokenize();

        [[noreturn]] void ThrowError(const std::string& message, size_t start, size_t length = 1) const {
            if (content[start] == '\n' && start + 1 < content.size()) {
                ++start;
            }

            size_t lineStart = content.rfind('\n', start);
            if (lineStart == std::string::npos) lineStart = 0;
            else lineStart += 1;

            size_t lineNumber = 1;
            for (size_t i = 0; i < lineStart; i++) {
                if (content[i] == '\n') lineNumber++;
            }

            std::vector<std::string_view> lines;
            size_t pos = 0;
            while (pos < content.size()) {
                size_t next = content.find('\n', pos);
                if (next == std::string::npos) next = content.size();
                lines.emplace_back(content.data() + pos, next - pos);
                pos = next + 1;
            }

            for (int i = static_cast<int>(lineNumber) - 3; i <= static_cast<int>(lineNumber) + 1; i++) {
                if (i < 0 || i >= static_cast<int>(lines.size())) continue;
                std::cerr << RED << (i + 1 == static_cast<int>(lineNumber) ? "> " : "  ") << (i + 1) << ") ";
                auto& curLine = lines[i];

                if (i + 1 == static_cast<int>(lineNumber)) {
                    auto colStart = start - lineStart;
                    if (colStart > curLine.size()) colStart = curLine.size(); // clamp

                    auto before = curLine.substr(0, colStart);
                    auto errorPart = curLine.substr(colStart, length);
                    auto after = (colStart + length <= curLine.size())
                                     ? curLine.substr(colStart + length)
                                     : "";

                    std::cerr << before << BOLD << YELLOW << errorPart << RED << after << RESET << "\n";
                } else {
                    std::cerr << curLine << RESET << "\n";
                }
            }

            std::cerr << "\n" << RED << filepath << ": " << message << std::endl;

            //throw std::runtime_error("test");
            exit(1);
        }

        [[noreturn]] void ThrowError(const std::string& message, const std::string_view& view) const {
            ThrowError(message, view.data() - content.data(), view.size());
        }

        [[noreturn]] void ThrowError(const std::string& message, const TokenImpl& token) const {
            ThrowError(message, token.value);
        }
    };

    struct Tokenizer {
        std::unordered_map<std::string, FileTokenizer> files;

        void Tokenize(const std::string& filepath) {
            if (files.contains(filepath)) return;
            auto& tokenizer = files[filepath];
            tokenizer.filepath = filepath;
            GetFileContent(filepath, tokenizer.content);
            tokenizer.Tokenize();
        }

        [[noreturn]] void ThrowError(const std::string& message, const Token& token) const {
            for (const auto& file : files | std::views::values) {
                if (token.value.data() >= file.content.data()
                    && token.value.data() < file.content.data() + file.content.size()
                ) {
                    file.ThrowError(message, token);
                }
            }

            throw std::runtime_error(message + " at unknown location");
        }
    };

    template <TokenType T>
    static std::ostream& operator<<(std::ostream& os, const std::vector<TokenComp<T>>& ls) {
        PrintVec(os, ls);
        return os;
    }
};
