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
#include "utfcpp/utf8/checked.h"

static bool PrintTokenValue = true;

namespace Flame {
    static const string operators[] = {
        "<<=", ">>=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "@=",

        "&&", "||", "++", "--", "<<", ">>", "+", "-", "*", "/", "%", "!=", "!", "<=", ">=", "<", ">", "&", "|",
        "^", "~", "@", "==",

        "="
    };
    static constexpr char symbols[] = {
        '(', ')', '{', '}', '[', ']', ',', ':', '.', '?'
    };
    // keyword "unsafe" is reserved for now, will be used later for unsafe code blocks and functions
    // You can freely edit any keyword, even use Unicode characters! Should be in the same order as TokenType enum down below
    static const string keywords[] = {
        "if", "else", "while", "for", "return", "fun", "struct",
        "import", "var", "val", "break", "continue", "when", "try",
        "catch", "finally", "throw", "get", "set", "default", "delete",
        "in", "as", "do", "alias", "enum", "public", "protected", "private",
        "operator", "interface", "extends", "implements", "true", "false",
        "yield", "unsafe", "move"
    };

    enum class TokenType {
        None,
        EndOfFile,
        Identifier,
        Integer,
        Float,
        Char,
        String,
        StringStart,
        StringMiddle,
        StringEnd,
        NewLine,

        Keywords,

        KeywordIf = Keywords, KeywordElse, KeywordWhile, KeywordFor, KeywordReturn, KeywordFun, KeywordStruct,
        KeywordImport, KeywordVar, KeywordVal, KeywordBreak, KeywordContinue, KeywordWhen, KeywordTry, KeywordCatch,
        KeywordFinally, KeywordThrow, KeywordGet, KeywordSet, KeywordDefault, KeywordDelete, KeywordIn, KeywordAs,
        KeywordDo, KeywordAlias, KeywordEnum, KeywordPublic, KeywordProtected, KeywordPrivate, KeywordOperator,
        KeywordInterface, KeywordExtends, KeywordImplements, KeywordTrue, KeywordFalse, KeywordYield, KeywordUnsafe,
        KeywordMove,

        KeywordsEnd, // This is not a keyword, it's just marking the end of keywords
        Operators = KeywordsEnd,

        SetOperators   = Operators,
        OperatorSetShl = Operators, OperatorSetShr, OperatorSetAdd, OperatorSetSub, OperatorSetMul,
        OperatorSetDiv, OperatorSetMod, OperatorSetBitAnd, OperatorSetBitOr, OperatorSetBitXor,
        OperatorSetMatMul,

        SetOperatorsEnd = OperatorSetMatMul,
        NonSetOperators,

        OperatorAnd = NonSetOperators, OperatorOr, OperatorInc, OperatorDec, OperatorShl, OperatorShr,
        OperatorAdd, OperatorSub, OperatorMul, OperatorDiv, OperatorMod, OperatorNeq, OperatorNot, OperatorLte,
        OperatorGte, OperatorLt, OperatorGt, OperatorBitAnd, OperatorBitOr, OperatorBitXor, OperatorBitNot,
        OperatorMatMul, OperatorEq,

        NonSetOperatorsEnd = OperatorEq,

        OperatorAssign, // Special. '='

        OperatorsEnd = OperatorAssign,

        Symbols,
        SymbolLeftParenthesis = Symbols, SymbolRightParenthesis, SymbolLeftBrace, SymbolRightBrace, SymbolLeftBracket,
        SymbolRightBracket, SymbolComma, SymbolColon, SymbolDot, SymbolQuestion, SymbolArrow, SymbolEllipsis,
        SymbolWalrus,
        SymbolsEnd = SymbolWalrus
    };

    enum class OperatorType {
        None   = static_cast<int>(TokenType::None),
        Assign = static_cast<int>(TokenType::OperatorAssign), SetShl, SetShr, SetAdd, SetSub, SetMul,
        SetDiv, SetMod, SetBitAnd, SetBitOr, SetBitXor, SetMatMul,
        SetsEnd = SetMatMul,

        Add, Sub, Mul, Div, Mod, Not, Lte, Gte, Lt, Gt, BitAnd, BitOr,
        BitXor, BitNot, MatMul, Eq, Neq, And, Or, Inc, Dec, Shl, Shr,
    };

    enum class SymbolType {
        None    = static_cast<int>(TokenType::None),
        LeftPar = static_cast<int>(TokenType::Symbols), RightPar, LeftBrace, RightBrace, LeftBracket,
        RightBracket, Comma, Semicolon, Colon, Dot, Question, Arrow
    };

    static string getTokenTypeName(TokenType type) {
        if (type == TokenType::EndOfFile) return "EndOfFile";
        if (type == TokenType::Identifier) return "Identifier";
        if (type == TokenType::Integer) return "Integer";
        if (type == TokenType::Float) return "Float";
        if (type == TokenType::Char) return "Char";
        if (type == TokenType::String || type == TokenType::StringStart || type == TokenType::StringMiddle
            || type == TokenType::StringEnd)
            return "String";
        if (type == TokenType::NewLine) return "NewLine";
        if (type < TokenType::KeywordsEnd) return "Keyword";
        if (type <= TokenType::OperatorsEnd) return "Operator";
        return "Symbol";
    }

    string getTokenValue(TokenType type);

    struct TokenImpl {
        string_view value;

        TokenImpl() = default;

        explicit TokenImpl(string_view value) : value(value) {
        }

        TokenImpl(const char* start, size_t length) : value(start, length) {
        }

        [[nodiscard]] static constexpr bool isLiteral(TokenType type) {
            return type == TokenType::Integer || type == TokenType::Float || type == TokenType::Char
                || type == TokenType::String || type == TokenType::KeywordTrue || type == TokenType::KeywordFalse;
        }

        [[nodiscard]] static constexpr bool isKeyword(TokenType type) {
            return type >= TokenType::Keywords && type < TokenType::KeywordsEnd;
        }

        [[nodiscard]] static constexpr bool isOperator(TokenType type) {
            return type >= TokenType::Operators && type <= TokenType::OperatorsEnd;
        }

        [[nodiscard]] static constexpr bool isSymbol(TokenType type) {
            return type >= TokenType::Symbols && type < TokenType::SymbolsEnd;
        }

        [[nodiscard]] static constexpr bool isSetOperator(TokenType type) {
            return type >= TokenType::SetOperators && type < TokenType::SetOperatorsEnd;
        }

        [[nodiscard]] static constexpr bool isNonSetOperator(TokenType type) {
            return type >= TokenType::NonSetOperators && type < TokenType::NonSetOperatorsEnd;
        }

        [[nodiscard]] int64_t getInt() const {
            string_view sv = value;
            string cleaned;

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

            int64_t result = 0;
            auto [ptr, ec] = std::from_chars(sv.data(), sv.data() + sv.size(), result, base);
            if (ec != std::errc() || ptr != sv.data() + sv.size()) {
                throw std::runtime_error("Invalid integer literal" + string(value));
            }

            return negative ? -result : result;
        }

        [[nodiscard]] double getFloat() const {
            string_view sv = value;
            string cleaned;

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
                throw std::runtime_error("Invalid float literal: " + string(value));
            }
            return result;
#else
            char* endPtr = nullptr;
            double result = std::strtod(sv.data(), &endPtr);
            if (endPtr != sv.data() + sv.size()) {
                throw std::runtime_error("Invalid float literal: " + string(value));
            }
            return result;
#endif
        }
    };

    template <TokenType type>
    struct TokenComp : TokenImpl {
        using TokenImpl::TokenImpl;

        [[nodiscard]] static constexpr bool isLiteral() {
            return TokenImpl::isLiteral(type);
        }

        [[nodiscard]] static constexpr bool isKeyword() {
            return TokenImpl::isKeyword(type);
        }

        [[nodiscard]] static constexpr bool isOperator() {
            return TokenImpl::isOperator(type);
        }

        [[nodiscard]] static constexpr bool isSymbol() {
            return TokenImpl::isSymbol(type);
        }

        [[nodiscard]] static constexpr bool isSetOperator() {
            return TokenImpl::isSetOperator(type);
        }

        [[nodiscard]] static constexpr bool isNonSetOperator() {
            return TokenImpl::isNonSetOperator(type);
        }

        [[nodiscard]] constexpr TokenImpl impl() const {
            return static_cast<TokenImpl>(*this);
        }

        constexpr bool operator==(const TokenComp& rhs) const noexcept {
            return value == rhs.value;
        }
    };

    template <TokenType T>
    static ostream& operator<<(ostream& os, const TokenComp<T>& tok) {
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
        } else if constexpr (T == TokenType::Char) {
            os << "Char";
        } else if constexpr (T == TokenType::String) {
            os << "String";
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
        OperatorToken() : TokenImpl(), type(OperatorType::None) {
        }

        OperatorToken(OperatorType type, const string_view& value)
            : TokenImpl(value), type(type) {
        }

        OperatorToken(OperatorType type, const char* start, size_t length)
            : TokenImpl(start, length),
              type(type) {
        }

        OperatorType type;

        [[nodiscard]] constexpr bool isSetOperator() const {
            return TokenImpl::isSetOperator(static_cast<TokenType>(type));
        }

        [[nodiscard]] constexpr bool isNonSetOperator() const {
            return TokenImpl::isNonSetOperator(static_cast<TokenType>(type));
        }

        [[nodiscard]] TokenImpl impl() const {
            return TokenImpl{value};
        }

        friend ostream& operator<<(ostream& os, const OperatorToken& token) {
            if (PrintTokenValue) {
                os << '\'' << token.value << '\'';
                return os;
            }
            os << "OperatorToken(" << token.value << ")";
            return os;
        }
    };

    struct SymbolToken : TokenImpl {
        SymbolToken(SymbolType type, const string_view& value)
            : TokenImpl(value), type(type) {
        }

        SymbolToken(SymbolType type, const char* start, size_t length)
            : TokenImpl(start, length),
              type(type) {
        }

        SymbolType type;

        [[nodiscard]] constexpr TokenImpl Impl() const {
            return static_cast<TokenImpl>(*this);
        }

        friend ostream& operator<<(ostream& os, const SymbolToken& token) {
            if (PrintTokenValue) {
                os << '\'' << token.value << '\'';
                return os;
            }
            os << "SymbolToken(" << token.value << ")";
            return os;
        }
    };

    struct Token : TokenImpl {
        Token() : TokenImpl(), type(TokenType::None) {
        }

        Token(TokenType type, const string_view& value)
            : TokenImpl(value), type(type) {
        }

        Token(TokenType type, const char* start, size_t length)
            : TokenImpl(start, length),
              type(type) {
        }

        TokenType type;

        [[nodiscard]] constexpr bool isLiteral() const {
            return TokenImpl::isLiteral(type);
        }

        [[nodiscard]] constexpr bool isKeyword() const {
            return TokenImpl::isKeyword(type);
        }

        [[nodiscard]] constexpr bool isOperator() const {
            return TokenImpl::isOperator(type);
        }

        [[nodiscard]] constexpr bool isSymbol() const {
            return TokenImpl::isSymbol(type);
        }

        [[nodiscard]] constexpr bool isSetOperator() const {
            return TokenImpl::isSetOperator(type);
        }

        [[nodiscard]] constexpr bool isNonSetOperator() const {
            return TokenImpl::isNonSetOperator(type);
        }

        template <TokenType T>
        constexpr TokenComp<T> comp() const {
            return TokenComp<T>(value);
        }

        [[nodiscard]] OperatorToken op() const {
            if (!isOperator()) throw std::runtime_error("Token is not an operator");
            return {static_cast<OperatorType>(type), value};
        }

        [[nodiscard]] constexpr OperatorType opType() const {
            if (!isOperator()) throw std::runtime_error("Token is not an operator");
            return static_cast<OperatorType>(type);
        }

        [[nodiscard]] SymbolToken sym() const {
            if (!isSymbol()) throw std::runtime_error("Token is not a symbol");
            return {static_cast<SymbolType>(type), value};
        }

        [[nodiscard]] constexpr SymbolType symType() const {
            if (!isSymbol()) throw std::runtime_error("Token is not a symbol");
            return static_cast<SymbolType>(type);
        }

        [[nodiscard]] constexpr TokenImpl impl() const {
            return static_cast<TokenImpl>(*this);
        }

        friend ostream& operator<<(ostream& os, const Token& token) {
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
            case TokenType::Char: typeName = "Char";
                break;
            case TokenType::String: typeName = "String";
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
                } else if (token.type >= TokenType::Operators) {
                    typeName = "Operator";
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
    using CharToken = TokenComp<TokenType::Char>;
    using StringToken = TokenComp<TokenType::String>;
    using StringStartToken = TokenComp<TokenType::StringStart>;
    using StringMiddleToken = TokenComp<TokenType::StringMiddle>;
    using StringEndToken = TokenComp<TokenType::StringEnd>;
    using NewLineToken = TokenComp<TokenType::NewLine>;

    constexpr auto RED = "\033[31m";
    constexpr auto YELLOW = "\033[33m";
    constexpr auto BOLD = "\033[1m";
    constexpr auto RESET = "\033[0m";

    [[nodiscard]] static constexpr bool isDigit(utf8::utfchar32_t c) {
        return c >= '0' && c <= '9';
    }

    [[nodiscard]] static constexpr bool isNewLine(utf8::utfchar32_t c) {
        return c == '\n' || c == 0x2028 || c == 0x2029;
    }

    [[nodiscard]] static constexpr bool isSpace(utf8::utfchar32_t c) {
        return c == ' ' || c == '\t' || c == '\r';
    }

    template <typename T>
    [[nodiscard]] inline utf8::utfchar32_t UTFNext(T& offset, T end) {
        if (offset >= end) return 0;
        return utf8::next(offset, end);
    }

    template <typename T>
    inline void UTFSkip(T& offset, T end) {
        if (offset >= end) return;
        utf8::next(offset, end);
    }

    template <typename T>
    [[nodiscard]] static utf8::utfchar32_t UTFPeek(T offset, T end, int amount = 0) {
        if (offset >= end) return 0;
        for (int i = 0; i < amount; i++) {
            UTFSkip(offset, end);
            if (offset >= end) return 0;
        }
        return utf8::next(offset, end);
    }

    template <typename T>
    [[nodiscard]] static utf8::utfchar32_t UTFBack(T& offset, T start) {
        if (offset >= start) return 0;
        return utf8::prior(offset, start);
    }

    template <typename T>
    static void UTFSkipBack(T& offset, T start) {
        if (offset >= start) return;
        utf8::prior(offset, start);
    }

    template <typename T>
    [[nodiscard]] static utf8::utfchar32_t UTFPeekBack(T offset, T start, int amount = 0) {
        if (offset >= start) return 0;
        for (int i = 0; i < amount; i++) {
            UTFSkipBack(offset, start);
            if (offset >= start) return 0;
        }
        return utf8::prior(offset, start);
    }

    [[nodiscard]] static bool stringStartsWith(const char* offset, const string& str) {
        auto end = offset + str.length();
        auto strStart = str.data();
        auto strEnd = strStart + str.length();
        while (*strStart != 0) {
            if (UTFNext(offset, end) != UTFNext(strStart, strEnd)) return false;
        }
        return true;
    }

    struct FileTokenizer {
        string filepath;
        string _content;
        char* it{nullptr};
        char* end{nullptr};
        vector<Token> tokens;

        [[nodiscard]] constexpr int getOperator() const;
        void Tokenize();

        [[nodiscard]] utf8::utfchar32_t peek() const {
            return UTFPeek(it, end);
        }

        [[nodiscard]] utf8::utfchar32_t peek(int amount) const {
            return UTFPeek(it, end, amount);
        }

        [[nodiscard]] utf8::utfchar32_t next() {
            return UTFNext(it, end);
        }

        void skip() {
            UTFSkip(it, end);
        }

        void skipDigits() {
            while (!over()) {
                auto c2 = peek();
                if (c2 != '_' && !isDigit(c2)) break;
                skip();
            }
        }

        [[nodiscard]] constexpr bool over() const {
            return it >= end;
        }

        void addToken(TokenType type, char* offset, size_t length) {
            tokens.emplace_back(type, offset, length);
        }

        void addToken(TokenType type, size_t length) {
            tokens.emplace_back(type, it, length);
        }

        [[noreturn]] void throwError(const string& message, const char* offset, size_t length = 1) const {
            auto start = _content.data();
            const char* constEnd = this->end;
            if (offset >= constEnd) {
                offset = constEnd;
                utf8::prior(offset, start);
            }

            if (isNewLine(UTFPeek(offset, constEnd)) && UTFPeek(offset, constEnd, 1)) {
                UTFSkip(offset, constEnd);
            }

            vector<string_view> lines;
            size_t wantedLine = 0;
            size_t walkLineIndex = 0;
            auto lineStart = start;
            auto pos = start;
            while (pos < constEnd) {
                auto c = UTFPeek(pos, constEnd);
                if (isNewLine(c)) {
                    if (offset >= lineStart && offset < pos) wantedLine = walkLineIndex;
                    lines.emplace_back(lineStart, pos - lineStart);
                    UTFSkip(pos, constEnd);
                    lineStart = pos;
                    walkLineIndex++;
                    continue;
                }
                UTFSkip(pos, constEnd);
            }

            if (lineStart < constEnd) {
                if (offset >= lineStart && offset <= constEnd) wantedLine = walkLineIndex;
                lines.emplace_back(lineStart, constEnd - lineStart);
            }

            for (auto i = wantedLine > 2 ? wantedLine - 3 : 0; i <= wantedLine + 1; i++) {
                if (i < 0 || i >= lines.size()) continue;
                std::cerr << RED << (i == wantedLine ? "> " : "  ") << (i + 1) << ") ";
                auto& curLine = lines[i];

                if (i == wantedLine) {
                    size_t colStart = offset - curLine.data();
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

            exit(1);
        }

        [[noreturn]] void throwError(const string& message, size_t length = 1) const {
            throwError(message, it, length);
        }

        [[noreturn]] void throwError(const string& message, const string_view& view) const {
            throwError(message, view.data(), view.size());
        }

        [[noreturn]] void throwError(const string& message, const TokenImpl& token) const {
            throwError(message, token.value);
        }
    };

    struct Tokenizer {
        hashmap<string, FileTokenizer> files;

        void tokenize(const string& filepath) {
            if (files.contains(filepath)) return;
            auto& tokenizer = files[filepath];
            tokenizer.filepath = filepath;
            getFileContent(filepath, tokenizer._content);
            tokenizer.it = tokenizer._content.data();
            tokenizer.end = tokenizer._content.data() + tokenizer._content.size();
            tokenizer.Tokenize();
        }

        [[noreturn]] void throwError(const string& message, const Token& token) const {
            for (const auto& file : files | std::views::values) {
                if (token.value.data() >= file._content.data()
                    && token.value.data() < file._content.data() + file._content.size()
                ) {
                    file.throwError(message, token);
                }
            }

            throw std::runtime_error(message + " at unknown location");
        }

        friend ostream& operator<<(ostream& os, const Tokenizer& tokenizer) {
            os << "Tokenizer {\n";
            for (const auto& pair : tokenizer.files) {
                auto& [filename, file] = pair;
                os << "  \"" << filename << "\": ";
                printVecLine(os, file.tokens, "  ");
                if (&pair != &*std::prev(tokenizer.files.end())) {
                    os << ",\n";
                }
            }
            os << "\n}";
            return os;
        }
    };

    template <TokenType T>
    static ostream& operator<<(ostream& os, const vector<TokenComp<T>>& ls) {
        printVec(os, ls);
        return os;
    }

    ostream& operator<<(ostream& os, OperatorType op);
    ostream& operator<<(ostream& os, SymbolType op);
}

namespace std {
    template <>
    struct hash<Flame::TokenComp<Flame::TokenType::Identifier>> {
        size_t operator()(const Flame::TokenComp<Flame::TokenType::Identifier>& p) const noexcept {
            return hash<string_view>()(p.value);
        }
    };
}
