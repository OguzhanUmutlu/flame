#pragma once
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

static constexpr char charOperators[] = {
    '+', '-', '*', '/', '%', '=', '!', '<', '>', '&', '|', '^', '~'
};
static const char* operators[] = {
    "<<=", ">>=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=",
    "==", "!=", "<=", ">=", "&&", "||", "++", "--", "<<", ">>"
};
static constexpr char symbols[] = {
    '(', ')', '{', '}', '[', ']', ',', ';', ':', '.', '?'
};
static const char* keywords[] = {
    "if", "else", "while", "for", "return", "fun", "class",
    "import", "var", "val", "break", "continue", "when", "try",
    "catch", "finally", "throw"
};

namespace Flame {
    struct Token {
        enum class Type {
            Identifier,
            Integer,
            Float,
            String,
            Boolean,
            StringStart,
            StringMiddle,
            StringEnd,
            NewLine,

            Keywords  = 100,
            KeywordIf = 100, KeywordElse, KeywordWhile, KeywordFor, KeywordReturn, KeywordFun, KeywordClass,
            KeywordImport, KeywordVar, KeywordVal, KeywordBreak, KeywordContinue, KeywordWhen, KeywordTry, KeywordCatch,
            KeywordFinally, KeywordThrow,

            OperatorChars = 200,
            OperatorAdd   = 200, OperatorSub, OperatorMul, OperatorDiv, OperatorMod, OperatorAssign, OperatorNot,
            OperatorLt, OperatorGt, OperatorBitAnd, OperatorBitOr, OperatorBitXor, OperatorBitNot,

            OperatorStrings = 300,
            OperatorSetShl  = 300, OperatorSetShr, OperatorSetAdd, OperatorSetSub, OperatorSetMul, OperatorSetDiv,
            OperatorSetMod, OperatorSetBitAnd, OperatorSetBitOr, OperatorSetBitXor,

            OperatorEq, OperatorNeq, OperatorLte, OperatorGte, OperatorAnd, OperatorOr, OperatorInc, OperatorDec,
            OperatorShl, OperatorShr,

            Symbols       = 400,
            SymbolLeftPar = 400, SymbolRightPar, SymbolLeftBrace, SymbolRightBrace, SymbolLeftBracket,
            SymbolRightBracket, SymbolComma, SymbolSemicolon, SymbolColon, SymbolDot, SymbolQuestion
        };

        Type type;
        std::string_view value;

        Token(Type type, std::string_view value)
            : type(type), value(value) {
        }

        Token(Type type, const char* str, size_t len) : type(type), value(str, len) {
        }

        bool is_keyword() const {
            return type >= Type::Keywords && type < Type::OperatorChars;
        }

        bool is_operator() const {
            return (type >= Type::OperatorChars && type < Type::OperatorStrings)
                || (type >= Type::OperatorStrings && type < Type::Symbols);
        }

        bool is_symbol() const {
            return type >= Type::Symbols && type < Type::Keywords;
        }

        bool is_set_operator() const {
            return type >= Type::OperatorSetShl && type <= Type::OperatorSetBitXor;
        }

        bool is_arithmetic_operator() const {
            return (type >= Type::OperatorChars && type < Type::OperatorStrings)
                || (type >= Type::OperatorEq && type <= Type::OperatorShr);
        }

        int parse_int() const {
            if (type == Type::Integer) return std::stoi(std::string(value));
            throw std::runtime_error("Token is not an integer");
        }

        double parse_float() const {
            if (type == Type::Float) return std::stod(std::string(value));
            throw std::runtime_error("Token is not a float");
        }
    };

    void Tokenize(const std::string& content, std::vector<Token>& tokens);
    void DebugTokens(const std::vector<Token>& tokens);
};
