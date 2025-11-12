#pragma once
#include <optional>
#include <variant>

#include "arena.hpp"
#include "lexer.hpp"
#include "utils.hpp"

#define PARAM(type, name) type name
#define INIT(name) name(name)

#define COTR1(struct_name, m1) \
explicit struct_name(PARAM m1) : INIT(GET_NAME m1) {}

#define COTR2(struct_name, m1, m2) \
explicit struct_name(PARAM m1, PARAM m2) : INIT(GET_NAME m1), INIT(GET_NAME m2) {}

#define COTR3(struct_name, m1, m2, m3) \
explicit struct_name(PARAM m1, PARAM m2, PARAM m3) : INIT(GET_NAME m1), INIT(GET_NAME m2), INIT(GET_NAME m3) {}

#define COTR4(struct_name, m1, m2, m3, m4) \
explicit struct_name(PARAM m1, PARAM m2, PARAM m3, PARAM m4) \
: INIT(GET_NAME m1), INIT(GET_NAME m2), INIT(GET_NAME m3), INIT(GET_NAME m4) {}

#define COTR5(struct_name, m1, m2, m3, m4, m5) \
explicit struct_name(PARAM m1, PARAM m2, PARAM m3, PARAM m4, PARAM m5) \
: INIT(GET_NAME m1), INIT(GET_NAME m2), INIT(GET_NAME m3), INIT(GET_NAME m4), INIT(GET_NAME m5) {}

#define GET_NAME(type, name) name

#define PP_NARG(...) PP_NARG_(__VA_ARGS__, 8,7,6,5,4,3,2,1,0)
#define PP_NARG_(_1,_2,_3,_4,_5,_6,_7,_8,N,...) N

#define CONCAT(a, b) CONCAT_(a, b)
#define CONCAT_(a, b) a##b

#define COTR(struct_name, ...) CONCAT(COTR, PP_NARG(__VA_ARGS__))(struct_name, __VA_ARGS__)

namespace Flame {
    struct ASTNodeStruct;
    using ASTNode = ASTNodeStruct*;

    struct ASTNodeStruct {
        template <typename T, typename... Args>
        static auto create(Arena& arena, Args&&... args) {
            auto mem = arena.alloc<T>();
            if constexpr (std::is_aggregate_v<T>) {
                new(mem) T{std::forward<Args>(args)...};
            } else {
                new(mem) T(std::forward<Args>(args)...);
            }
            return mem;
        }

        template <typename T>
        [[nodiscard]] bool is() const {
            return dynamic_cast<const T*>(this) != nullptr;
        }

        virtual void print(ostream& os) const = 0;
        [[nodiscard]] virtual constexpr TokenImpl getToken() const = 0;
        virtual ~ASTNodeStruct() = default;

        friend ostream& operator<<(ostream& os, const ASTNodeStruct& node) {
            node.print(os);
            return os;
        }

        friend ostream& operator<<(ostream& os, const ASTNodeStruct* node) {
            if (node == nullptr) {
                os << "NONE";
                return os;
            }
            return os << *node;
        }
    };

    struct Expr : ASTNodeStruct {
    };

    struct TypeExpr : ASTNodeStruct {
    };

    struct Stmt : ASTNodeStruct {
    };

    struct Parameter {
        IdentifierToken id;
        ASTNode type;
        ASTNode default_;

        friend ostream& operator<<(ostream& os, const Parameter& par) {
            os << "Parameter(id=" << par.id
                << ", type=" << par.type
                << ", default=" << par.default_
                << ")";
            return os;
        }
    };

    struct EnumMember {
        bool manual;
        IdentifierToken id;
        ASTNode value;

        friend ostream& operator<<(ostream& os, const EnumMember& mem) {
            os << "EnumMember(manual=" << (mem.manual ? "true" : "false")
                << ", id=" << mem.id
                << ", value=" << mem.value
                << ")";
            return os;
        }
    };

    struct CallArgument {
        bool spread;
        optional<IdentifierToken> name;
        ASTNode value;

        friend ostream& operator<<(ostream& os, const CallArgument& arg) {
            os << "CallArgument(spread=" << (arg.spread ? "true" : "false")
                << ", name=" << arg.name
                << ", value=" << arg.value
                << ")";
            return os;
        }
    };

    struct Generic {
        IdentifierToken id;
        ASTNode constraint;
        ASTNode default_;

        friend ostream& operator<<(ostream& os, const Generic& gen) {
            os << "Generic(id=" << gen.id
                << ", constraint=" << gen.constraint
                << ", default=" << gen.default_
                << ")";
            return os;
        }
    };

    enum class PropVisibility {
        Public,
        Protected,
        Private
    };

    static ostream& operator<<(ostream& os, PropVisibility par) {
        switch (par) {
        case PropVisibility::Public:
            os << "Public";
            break;
        case PropVisibility::Protected:
            os << "Protected";
            break;
        case PropVisibility::Private:
            os << "Private";
            break;
        }
        return os;
    }

    struct NoneNode final : ASTNodeStruct {
        void print(ostream& os) const override {
            os << "NONE";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return TokenImpl{};
        }
    };

    static NoneNode noneNodeStruct{};
    static ASTNode noneNode = &noneNodeStruct;


    // NOLINTBEGIN(*-explicit-constructor)
    struct VariableExpr final : Expr {
        IdentifierToken name;

        COTR1(VariableExpr, (IdentifierToken, name));

        void print(ostream& os) const override {
            os << "VariableExpr(" << name << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return name;
        }
    };

    struct PropertyExpr final : Expr {
        ASTNode callee;
        IdentifierToken id;

        COTR2(PropertyExpr, (ASTNode, callee), (IdentifierToken, id));

        void print(ostream& os) const override {
            os << "PropertyExpr(" << callee << ", " << id << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return callee->getToken();
        }
    };

    struct VariableIncDecExpr final : Expr {
        enum class Type {
            IncLeft,
            IncRight,
            DecLeft,
            DecRight
        };

        Type type{Type::IncLeft};
        ASTNode var;

        COTR2(VariableIncDecExpr, (Type, type), (ASTNode, var));

        void print(ostream& os) const override {
            os << "VariableIncDecExpr(";
            switch (type) {
            case Type::IncLeft:
                os << "++";
                os << ", " << var << ")";
                break;
            case Type::IncRight:
                os << var;
                os << ", ++)";
                break;
            case Type::DecLeft:
                os << "--";
                os << ", " << var << ")";
                break;
            case Type::DecRight:
                os << var;
                os << ", --)";
                break;
            }
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return var->getToken();
        }
    };

    struct CallExpr final : Expr {
        ASTNode callee;
        vector<CallArgument> args;

        COTR1(CallExpr, (ASTNode, callee));

        void print(ostream& os) const override {
            os << "CallExpr(callee=" << callee
                << ", args=" << args
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return callee->getToken();
        }
    };

    struct UnaryExpr final : Expr {
        OperatorToken op;
        ASTNode expr;

        COTR2(UnaryExpr, (OperatorToken, op), (ASTNode, expr));

        void print(ostream& os) const override {
            os << "UnaryExpr(" << op << ", " << expr << ")";
        }

        [[nodiscard]] TokenImpl getToken() const override {
            return op.impl();
        }
    };

    struct BinaryExpr final : Expr {
        ASTNode left;
        OperatorToken op;
        ASTNode right;

        COTR3(BinaryExpr, (ASTNode, left), (OperatorToken, op), (ASTNode, right));

        void print(ostream& os) const override {
            os << "BinaryExpr(" << left << ", " << op << ", " << right << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return left->getToken();
        }
    };

    struct LiteralExpr final : Expr {
        Token value; // IntegerToken | FloatToken | StringToken | BooleanToken | KeywordNullToken

        COTR1(LiteralExpr, (Token, value));

        void print(ostream& os) const override {
            switch (value.type) {
            case TokenType::Integer:
                os << "Int(" << value.value << ")";
                break;
            case TokenType::Float:
                os << "Float(" << value.value << ")";
                break;
            case TokenType::String:
                os << "String(" << value.value << ")";
                break;
            case TokenType::Char:
                os << "Char(" << value.value << ")";
                break;
            case TokenType::KeywordTrue:
            case TokenType::KeywordFalse:
                os << "Bool(" << value.value << ")";
                break;
            default:
                os << "UnknownLiteral(" << value.value << ")";
                break;
            }
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return value.impl();
        }
    };

    struct FStringExpr final : Expr {
        StringStartToken start;
        StringEndToken end;
        vector<ASTNode> expressions;
        vector<StringMiddleToken> middles;

        COTR1(FStringExpr, (StringStartToken, start));

        void print(ostream& os) const override {
            os << "FStringExpr(start=" << start
                << ", expressions=" << expressions
                << ", middles=" << middles
                << ", end=" << end
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return start;
        }
    };

    struct IndexExpr final : Expr {
        ASTNode callee;
        ASTNode index;

        COTR2(IndexExpr, (ASTNode, callee), (ASTNode, index));

        void print(ostream& os) const override {
            os << "IndexExpr(callee=" << callee
                << ", index=" << index << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return callee->getToken();
        }
    };

    struct TupleExpr final : Expr {
        TokenComp<TokenType::SymbolLeftParenthesis> leftPar;
        vector<ASTNode> elements;

        COTR1(TupleExpr, (TokenComp<TokenType::SymbolLeftParenthesis>, leftPar));

        void print(ostream& os) const override {
            os << "TupleExpr("
                << elements
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return leftPar.impl();
        }
    };

    struct IfExpr final : Expr {
        TokenComp<TokenType::KeywordIf> ifToken;
        ASTNode condition;
        vector<ASTNode> body;
        vector<ASTNode> elseBody;

        COTR2(IfExpr, (TokenComp<TokenType::KeywordIf>, ifToken), (ASTNode, condition));

        void print(ostream& os) const override {
            os << "IfExpr(condition=" << condition
                << ", body=" << body
                << ", elseBody=" << elseBody
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return ifToken.impl();
        }
    };

    struct RangeExpr final : Expr {
        ASTNode start;
        ASTNode end;
        ASTNode step;

        COTR3(RangeExpr, (ASTNode, start), (ASTNode, end), (ASTNode, step));

        void print(ostream& os) const override {
            os << "RangeExpr(start=" << start
                << ", end=" << end
                << ", step=" << step
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return start->getToken();
        }
    };

    struct LambdaExpr final : Expr {
        TokenComp<TokenType::SymbolLeftParenthesis> leftPar;
        ASTNode returns;
        vector<Parameter> params;
        vector<ASTNode> body;

        COTR2(LambdaExpr, (TokenComp<TokenType::SymbolLeftParenthesis>, leftPar), (ASTNode, returns));

        void print(ostream& os) const override {
            os << "LambdaExpr(params=" << params
                << ", returns=" << returns
                << ", body=" << body
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return leftPar;
        }
    };

    struct ScopeExpr final : Expr {
        TokenComp<TokenType::SymbolLeftBrace> token_;
        vector<ASTNode> body;

        COTR1(ScopeExpr, (TokenComp<TokenType::SymbolLeftBrace>, token_));

        void print(ostream& os) const override {
            os << "ScopeExpr(" << body << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct MoveExpr final : Expr {
        TokenComp<TokenType::KeywordMove> token_;
        ASTNode expression;

        COTR2(MoveExpr, (TokenComp<TokenType::KeywordMove>, token_), (ASTNode, expression));

        void print(ostream& os) const override {
            os << "MoveExpr(" << expression << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };


    struct GenericTypeExpr final : TypeExpr {
        ASTNode id;
        vector<ASTNode> arguments;

        COTR1(GenericTypeExpr, (ASTNode, id));

        void print(ostream& os) const override {
            os << "GenericTypeExpr(id=" << id
                << ", arguments=" << arguments
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return id->getToken();
        }
    };

    struct OptionalTypeExpr final : TypeExpr {
        ASTNode type;

        COTR1(OptionalTypeExpr, (ASTNode, type));

        void print(ostream& os) const override {
            os << "OptionalTypeExpr(" << type << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return type->getToken();
        }
    };

    struct FunctionTypeExpr final : TypeExpr {
        ASTNode returns;
        vector<ASTNode> params;

        COTR1(FunctionTypeExpr, (ASTNode, returns));

        void print(ostream& os) const override {
            os << "FunctionTypeExpr(params=" << params
                << ", returns=" << returns
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return returns->getToken();
        }
    };

    struct ReferenceTypeExpr final : TypeExpr {
        ASTNode type;

        COTR1(ReferenceTypeExpr, (ASTNode, type));

        void print(ostream& os) const override {
            os << "ReferenceTypeExpr(" << type << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return type->getToken();
        }
    };


    struct ReturnStmt final : Stmt {
        TokenImpl token_;
        ASTNode value;

        COTR2(ReturnStmt, (TokenImpl, token_), (ASTNode, value));

        void print(ostream& os) const override {
            os << "ReturnStmt(" << value << ")";
        }

        [[nodiscard]] TokenImpl getToken() const override {
            return token_;
        }
    };

    struct YieldStmt final : Stmt {
        TokenComp<TokenType::KeywordYield> token_;
        ASTNode value;

        COTR2(YieldStmt, (TokenComp<TokenType::KeywordYield>, token_), (ASTNode, value));

        void print(ostream& os) const override {
            os << "YieldStmt(" << value << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct BreakStmt final : Stmt {
        TokenComp<TokenType::KeywordBreak> token_;

        COTR1(BreakStmt, (TokenComp<TokenType::KeywordBreak>, token_));

        void print(ostream& os) const override {
            os << "BreakStmt()";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct ContinueStmt final : Stmt {
        TokenComp<TokenType::KeywordContinue> token_;

        COTR1(ContinueStmt, (TokenComp<TokenType::KeywordContinue>, token_));

        void print(ostream& os) const override {
            os << "ContinueStmt()";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct DeleteStmt final : Stmt {
        TokenComp<TokenType::KeywordDelete> token_;
        ASTNode expression;

        COTR2(DeleteStmt, (TokenComp<TokenType::KeywordDelete>, token_), (ASTNode, expression));

        void print(ostream& os) const override {
            os << "DeleteStmt(" << expression << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct ExpressionStmt final : Stmt {
        ASTNode expr;

        COTR1(ExpressionStmt, (ASTNode, expr));

        void print(ostream& os) const override {
            os << "ExpressionStmt(" << expr << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return expr->getToken();
        }
    };

    struct ImportStmt final : Stmt {
        TokenComp<TokenType::KeywordImport> token_;
        bool star;
        ASTNode module;
        optional<IdentifierToken> alias;

        COTR4(ImportStmt, (TokenComp<TokenType::KeywordImport>, token_), (bool, star), (ASTNode, module),
              (optional<IdentifierToken>, alias));

        void print(ostream& os) const override {
            os << "ImportStmt(star=" << (star ? "true" : "false")
                << ", module=" << module
                << ", alias=" << alias
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct AliasStmt final : Stmt {
        TokenComp<TokenType::KeywordAlias> token_;
        IdentifierToken id;
        ASTNode type;

        COTR3(AliasStmt, (TokenComp<TokenType::KeywordAlias>, token_), (IdentifierToken, id), (ASTNode, type));

        void print(ostream& os) const override {
            os << "AliasStmt(id=" << id << ", type=" << type << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct VariableDefineStmt final : Stmt {
        bool constant;
        ASTNode id;
        ASTNode type;
        ASTNode value;

        COTR4(VariableDefineStmt, (bool, constant), (ASTNode, id), (ASTNode, type), (ASTNode, value));

        void print(ostream& os) const override {
            os << "VariableDefineStmt(constant=" << (constant ? "true" : "false")
                << ", id=" << id
                << ", type=" << type <<
                ", value=" << value
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return id->getToken();
        }
    };

    struct ForStmt final : Stmt {
        TokenComp<TokenType::KeywordFor> token_;
        bool constant;
        ASTNode iterable;
        vector<IdentifierToken> identifiers;
        vector<ASTNode> body;
        vector<ASTNode> elseBody;

        COTR3(ForStmt, (TokenComp<TokenType::KeywordFor>, token_), (bool, constant), (ASTNode, iterable));

        void print(ostream& os) const override {
            os << "ForStmt(identifiers=" << identifiers
                << ", constant=" << (constant ? "true" : "false")
                << ", iterable=" << iterable
                << ", body=" << body
                << ", elseBody=" << elseBody
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct FunctionStmt final : Stmt {
        TokenComp<TokenType::KeywordFun> token_;
        PropVisibility visibility;
        ASTNode id;
        ASTNode returns;
        vector<Generic> generics;
        vector<Parameter> params;
        vector<ASTNode> body;

        COTR4(FunctionStmt, (TokenComp<TokenType::KeywordFun>, token_), (PropVisibility, visibility), (ASTNode, id),
              (ASTNode, returns));

        void print(ostream& os) const override {
            os << "FunctionStmt(visibility=" << visibility
                << ", id=" << id
                << ", generics=" << generics
                << ", params=" << params
                << ", returns=" << returns
                << ", body=" << body
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct OperatorFunctionStmt final : Stmt {
        TokenComp<TokenType::KeywordFun> token_;
        PropVisibility visibility;
        OperatorType op;
        ASTNode id;
        ASTNode returns;
        vector<Parameter> params;
        vector<Generic> generics;
        vector<ASTNode> body;

        COTR5(OperatorFunctionStmt, (TokenComp<TokenType::KeywordFun>, token_), (PropVisibility, visibility),
              (OperatorType, op), (ASTNode, id), (ASTNode, returns));

        void print(ostream& os) const override {
            os << "BinaryOperatorFunctionStmt(visibility=" << visibility
                << ", id=" << id
                << ", op=" << op
                << ", params=" << params
                << ", generics=" << generics
                << ", returns=" << returns
                << ", body=" << body
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct GetFunctionStmt final : Stmt {
        TokenComp<TokenType::KeywordFun> token_;
        PropVisibility visibility;
        ASTNode id;
        ASTNode returns;
        vector<Generic> generics;
        vector<ASTNode> body;

        COTR4(GetFunctionStmt, (TokenComp<TokenType::KeywordFun>, token_), (PropVisibility, visibility), (ASTNode, id),
              (ASTNode, returns));

        void print(ostream& os) const override {
            os << "GetFunctionStmt(visibility=" << visibility
                << ", id=" << id
                << ", generics=" << generics
                << ", returns=" << returns
                << ", body=" << body
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct SetFunctionStmt final : Stmt {
        TokenComp<TokenType::KeywordFun> token_;
        PropVisibility visibility;
        ASTNode id;
        Parameter param;
        vector<Generic> generics;
        vector<ASTNode> body;

        COTR4(SetFunctionStmt, (TokenComp<TokenType::KeywordFun>, token_), (PropVisibility, visibility), (ASTNode, id),
              (Parameter, param));

        void print(ostream& os) const override {
            os << "SetFunctionStmt(visibility=" << visibility
                << ", id=" << id
                << ", generics=" << generics
                << ", param=" << param
                << ", body=" << body
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct WhileStmt final : Stmt {
        TokenComp<TokenType::KeywordWhile> token_;
        ASTNode condition;
        vector<ASTNode> body;
        vector<ASTNode> elseBody;

        COTR2(WhileStmt, (TokenComp<TokenType::KeywordWhile>, token_), (ASTNode, condition));

        void print(ostream& os) const override {
            os << "WhileStmt(condition=" << condition
                << ", body=" << body
                << ", elseBody=" << elseBody
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct DoWhileStmt final : Stmt {
        TokenComp<TokenType::KeywordDo> token_;
        ASTNode condition;
        vector<ASTNode> body;
        vector<ASTNode> elseBody;

        COTR2(DoWhileStmt, (TokenComp<TokenType::KeywordDo>, token_), (ASTNode, condition));

        void print(ostream& os) const override {
            os << "DoWhileStmt(body=" << body
                << ", condition=" << condition
                << ", elseBody=" << elseBody
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct StructStmt final : Stmt {
        TokenComp<TokenType::KeywordStruct> token_;
        bool isEnum;
        IdentifierToken id;
        ASTNode extends;
        vector<Generic> generics;
        vector<ASTNode> implements;
        vector<ASTNode> statements;
        vector<EnumMember> enumMembers;

        COTR4(StructStmt, (TokenComp<TokenType::KeywordStruct>, token_), (bool, isEnum), (IdentifierToken, id),
              (ASTNode, extends));

        void print(ostream& os) const override {
            os << "StructStmt(id=" << id
                << ", isEnum=" << (isEnum ? "true" : "false")
                << ", generics=" << generics
                << ", extends=" << extends
                << ", implements=" << implements
                << ", statements=" << statements
                << ", enumMembers=" << enumMembers
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    struct InterfaceStmt final : Stmt {
        TokenComp<TokenType::KeywordInterface> token_;
        ASTNode id;
        vector<Generic> generics;
        vector<ASTNode> extends;
        vector<ASTNode> statements;

        COTR2(InterfaceStmt, (TokenComp<TokenType::KeywordInterface>, token_), (ASTNode, id));

        void print(ostream& os) const override {
            os << "InterfaceStmt(id=" << id
                << ", generics=" << generics
                << ", extends=" << extends
                << ", statements=" << statements
                << ")";
        }

        [[nodiscard]] constexpr TokenImpl getToken() const override {
            return token_;
        }
    };

    // NOLINTEND(*-explicit-constructor)

    struct Parser {
    private:
        const FileTokenizer& cur;
        size_t current{0};

    public:
        const Tokenizer& tokenizer;
        Arena& arena;

        explicit Parser(Arena& arena, const Tokenizer& tokenizer)
            : cur(tokenizer.files.begin()->second), tokenizer(tokenizer), arena(arena) {
        }

        [[nodiscard]] constexpr size_t tokenCount() const {
            return cur.tokens.size();
        }

        [[nodiscard]] Token get(size_t index) const {
            if (index >= tokenCount()) return {TokenType::EndOfFile, cur._content.end()._Ptr, 0};
            return cur.tokens[index];
        }

        [[nodiscard]] Token peek() const {
            if (over()) return {TokenType::EndOfFile, cur._content.end()._Ptr, 0};
            return cur.tokens[current];
        }

        [[nodiscard]] bool peekIs(TokenType type) const {
            return peek().type == type;
        }

        [[nodiscard]] Token peekNoLine() const {
            for (auto i = current; i < tokenCount(); i++) {
                if (cur.tokens[i].type != TokenType::NewLine) {
                    return cur.tokens[i];
                }
            }
            return {TokenType::EndOfFile, cur._content.end()._Ptr, 0};
        }

        [[nodiscard]] Token peek(int offset) const {
            if (current + offset >= tokenCount()) return {TokenType::EndOfFile, cur._content.end()._Ptr, 0};
            return cur.tokens[current + offset];
        }

        [[nodiscard]] Token peekNoLine(int offset) const {
            for (auto i = current + offset; i < tokenCount(); i++) {
                if (cur.tokens[i].type != TokenType::NewLine) {
                    return cur.tokens[i];
                }
            }
            return {TokenType::EndOfFile, cur._content.end()._Ptr, 0};
        }

        [[nodiscard]] Token next() {
            if (over()) return {TokenType::EndOfFile, cur._content.end()._Ptr, 0};
            return cur.tokens[current++];
        }

        void Expect(TokenType type) {
            if (!peekIs(type)) {
                throwError(
                    "Expected '" + getTokenValue(type) + "', got '" +
                    string(peek().value) + (
                        peek().type <= TokenType::NewLine ? "'" : "' which is a " + getTokenTypeName(peek().type)
                    ), peek()
                );
            }
            current++;
        }

        [[nodiscard]] bool over() const {
            return current >= tokenCount();
        }

        [[noreturn]] void throwError(const string& message) const {
            cur.throwError(message, peek());
        }

        [[noreturn]] void throwError(const string& message, const TokenImpl& token) const {
            cur.throwError(message, token.value);
        }

        [[noreturn]] void throwError(const string& message, const string_view& view) const {
            cur.throwError(message, view);
        }

        [[noreturn]] void throwError(const string& message, size_t start, size_t length = 1) const {
            cur.throwError(message, cur._content.data() + start, length);
        }

        [[noreturn]] void throwError(const string& message, const ASTNodeStruct* node) const {
            throwError(message, node->getToken());
        }

        template <typename T, typename... Args>
        T* createNode(Args&&... args) {
            return ASTNodeStruct::create<T>(arena, std::forward<Args>(args)...);
        }

        void skipNewLines() {
            while (peekIs(TokenType::NewLine)) current++;
        }

        ASTNode parseSingleExpression();
        ASTNode parseExpression(int rbp = 0, bool ignoreNewLines = false, bool endAtSet = false, bool endAtGt = false,
                          bool allowType = false);
        void parseExpressions(vector<ASTNode>& result, bool ignoreNewLines = false);
        ASTNode parseTypeExpr(bool ignoreNewLines = false, bool endAtGt = false);
        void parseTypeExpressions(vector<ASTNode>& result);
        void parseGenerics(vector<Generic>& result);
        ASTNode parsePropertyExpr(); // despite the name it might return VariableExpr
        void parseFunctionArgument(Parameter& param);
        void parseFunctionArguments(vector<Parameter>& result);
        ASTNode parseVariableDefine(bool const_);
        ASTNode parseForLoop(TokenComp<TokenType::KeywordFor> tok);
        ASTNode parseWhileLoop(TokenComp<TokenType::KeywordWhile> tok);
        ASTNode parseDoWhileLoop(TokenComp<TokenType::KeywordDo> tok);
        ASTNode parseAliasStmt(TokenComp<TokenType::KeywordAlias> tok);
        ASTNode parseImportStmt(TokenComp<TokenType::KeywordImport> tok);
        ASTNode parseFunctionStmt(TokenComp<TokenType::KeywordFun> funTok, PropVisibility visibility);
        ASTNode parseStructStmt(TokenComp<TokenType::KeywordStruct> tok, bool isEnum);
        ASTNode parseInterfaceStmt(TokenComp<TokenType::KeywordInterface> tok);
        ASTNode parseStatement();
        void parseBracedBlock(vector<ASTNode>& result);
        void parseStatements(vector<ASTNode>& result);
    };
}
