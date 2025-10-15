#pragma once
#include <optional>
#include <variant>

#include "arena.hpp"
#include "lexer.hpp"
#include "utils.hpp"

#define PARAM(type, name) type name
#define INIT(name) name(name)

#define COTR1(struct_name, m1) \
struct_name() = default; \
struct_name(PARAM m1) : INIT(GET_NAME m1) {}

#define COTR2(struct_name, m1, m2) \
COTR1(struct_name, m1) \
struct_name(PARAM m1, PARAM m2) : INIT(GET_NAME m1), INIT(GET_NAME m2) {}

#define COTR3(struct_name, m1, m2, m3) \
COTR2(struct_name, m1, m2) \
struct_name(PARAM m1, PARAM m2, PARAM m3) : INIT(GET_NAME m1), INIT(GET_NAME m2), INIT(GET_NAME m3) {}

#define COTR4(struct_name, m1, m2, m3, m4) \
COTR3(struct_name, m1, m2, m3) \
struct_name(PARAM m1, PARAM m2, PARAM m3, PARAM m4) \
: INIT(GET_NAME m1), INIT(GET_NAME m2), INIT(GET_NAME m3), INIT(GET_NAME m4) {}

#define COTR5(struct_name, m1, m2, m3, m4, m5) \
COTR4(struct_name, m1, m2, m3, m4) \
struct_name(PARAM m1, PARAM m2, PARAM m3, PARAM m4, PARAM m5) \
: INIT(GET_NAME m1), INIT(GET_NAME m2), INIT(GET_NAME m3), INIT(GET_NAME m4), INIT(GET_NAME m5) {}

#define COTR6(struct_name, m1, m2, m3, m4, m5, m6) \
COTR5(struct_name, m1, m2, m3, m4, m5) \
struct_name(PARAM m1, PARAM m2, PARAM m3, PARAM m4, PARAM m5, PARAM m6) \
: INIT(GET_NAME m1), INIT(GET_NAME m2), INIT(GET_NAME m3), INIT(GET_NAME m4), INIT(GET_NAME m5), INIT(GET_NAME m6) {}

#define COTR7(struct_name, m1, m2, m3, m4, m5, m6, m7) \
COTR6(struct_name, m1, m2, m3, m4, m5, m6) \
struct_name(PARAM m1, PARAM m2, PARAM m3, PARAM m4, PARAM m5, PARAM m6, PARAM m7) \
: INIT(GET_NAME m1), INIT(GET_NAME m2), INIT(GET_NAME m3), INIT(GET_NAME m4), INIT(GET_NAME m5), INIT(GET_NAME m6), INIT(GET_NAME m7) {}

#define COTR8(struct_name, m1, m2, m3, m4, m5, m6, m7, m8) \
COTR7(struct_name, m1, m2, m3, m4, m5, m6, m7) \
struct_name(PARAM m1, PARAM m2, PARAM m3, PARAM m4, PARAM m5, PARAM m6, PARAM m7, PARAM m8) \
: INIT(GET_NAME m1), INIT(GET_NAME m2), INIT(GET_NAME m3), INIT(GET_NAME m4), INIT(GET_NAME m5), INIT(GET_NAME m6), INIT(GET_NAME m7), INIT(GET_NAME m8) {}

#define GET_NAME(type, name) name

#define PP_NARG(...) PP_NARG_(__VA_ARGS__, 8,7,6,5,4,3,2,1,0)
#define PP_NARG_(_1,_2,_3,_4,_5,_6,_7,_8,N,...) N

#define CONCAT(a, b) CONCAT_(a, b)
#define CONCAT_(a, b) a##b

#define COTR(struct_name, ...) CONCAT(COTR, PP_NARG(__VA_ARGS__))(struct_name, __VA_ARGS__)

namespace Flame {
    struct ASTNode {
        template <typename T, typename... Args>
        static auto Create(Arena& arena, Args&&... args) {
            auto mem = arena.alloc<T>();
            if constexpr (std::is_aggregate_v<T>) {
                new(mem) T{std::forward<Args>(args)...};
            } else {
                new(mem) T(std::forward<Args>(args)...);
            }
            return mem;
        }

        template <typename T>
        [[nodiscard]] bool Is() const {
            return dynamic_cast<const T*>(this) != nullptr;
        }

        virtual void Print(std::ostream& os) const = 0;
        [[nodiscard]] virtual constexpr TokenImpl GetToken() const = 0;
        virtual ~ASTNode() = default;

        friend std::ostream& operator<<(std::ostream& os, const ASTNode& node) {
            node.Print(os);
            return os;
        }

        friend std::ostream& operator<<(std::ostream& os, const ASTNode* node) {
            node->Print(os);
            return os;
        }
    };

    struct Expr : ASTNode {
    };

    struct TypeExpr : ASTNode {
    };

    struct Stmt : ASTNode {
    };

    struct Parameter {
        IdentifierToken id;
        ASTNode* type;
        ASTNode* default_;

        friend std::ostream& operator<<(std::ostream& os, const Parameter& par) {
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
        ASTNode* value;

        friend std::ostream& operator<<(std::ostream& os, const EnumMember& mem) {
            os << "EnumMember(manual=" << (mem.manual ? "true" : "false")
                << ", id=" << mem.id
                << ", value=" << mem.value
                << ")";
            return os;
        }
    };

    struct CallArgument {
        bool spread;
        std::optional<IdentifierToken> name;
        ASTNode* value;

        friend std::ostream& operator<<(std::ostream& os, const CallArgument& arg) {
            os << "CallArgument(spread=" << (arg.spread ? "true" : "false")
                << ", name=" << arg.name
                << ", value=" << arg.value
                << ")";
            return os;
        }
    };

    struct Generic {
        IdentifierToken id;
        ASTNode* constraint;
        ASTNode* default_;

        friend std::ostream& operator<<(std::ostream& os, const Generic& gen) {
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

    static std::ostream& operator<<(std::ostream& os, PropVisibility par) {
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

    struct NoneNode final : ASTNode {
        void Print(std::ostream& os) const override {
            os << "NONE";
        }

        constexpr TokenImpl GetToken() const override {
            return TokenImpl{};
        }
    };


    struct VariableExpr final : Expr {
        IdentifierToken name;

        COTR1(VariableExpr, (IdentifierToken, name));

        void Print(std::ostream& os) const override {
            os << "VariableExpr(" << name << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return name;
        }
    };

    struct PropertyExpr final : Expr {
        ASTNode* callee;
        IdentifierToken id;

        COTR2(PropertyExpr, (ASTNode*, callee), (IdentifierToken, id));

        void Print(std::ostream& os) const override {
            os << "PropertyExpr(" << callee << ", " << id << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return callee->GetToken();
        }
    };

    struct VariableIncDecExpr final : Expr {
        enum class Type {
            IncLeft,
            IncRight,
            DecLeft,
            DecRight
        };

        Type type;
        ASTNode* var;

        COTR2(VariableIncDecExpr, (Type, type), (ASTNode*, var));

        void Print(std::ostream& os) const override {
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

        constexpr TokenImpl GetToken() const override {
            return var->GetToken();
        }
    };

    struct CallExpr final : Expr {
        ASTNode* callee;
        std::vector<CallArgument> args;

        COTR2(CallExpr, (ASTNode*, callee), (std::vector<CallArgument>, args));

        void Print(std::ostream& os) const override {
            os << "CallExpr(callee=" << callee
                << ", args=" << args
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return callee->GetToken();
        }
    };

    struct UnaryExpr final : Expr {
        OperatorToken op;
        ASTNode* expr;

        COTR2(UnaryExpr, (OperatorToken, op), (ASTNode*, expr));

        void Print(std::ostream& os) const override {
            os << "UnaryExpr(" << op << ", " << expr << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return op.Impl();
        }
    };

    struct BinaryExpr final : Expr {
        ASTNode* left;
        OperatorToken op;
        ASTNode* right;

        COTR3(BinaryExpr, (ASTNode*, left), (OperatorToken, op), (ASTNode*, right));

        void Print(std::ostream& os) const override {
            os << "BinaryExpr(" << left << ", " << op << ", " << right << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return left->GetToken();
        }
    };

    struct LiteralExpr final : Expr {
        Token value; // IntegerToken | FloatToken | StringToken | BooleanToken | KeywordNullToken

        COTR1(LiteralExpr, (Token, value));

        void Print(std::ostream& os) const override {
            os << "Literal(" << value << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return value.Impl();
        }
    };

    struct FStringExpr final : Expr {
        StringStartToken start;
        std::vector<ASTNode*> expressions;
        std::vector<StringMiddleToken> middles;
        StringEndToken end;

        COTR4(FStringExpr, (StringStartToken, start), (std::vector<ASTNode*>, expressions),
              (std::vector<StringMiddleToken>, middles), (StringEndToken, end));

        void Print(std::ostream& os) const override {
            os << "FStringExpr(start=" << start
                << ", expressions=" << expressions
                << ", middles=" << middles
                << ", end=" << end
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return start;
        }
    };

    struct IndexExpr final : Expr {
        ASTNode* callee;
        ASTNode* index;

        COTR2(IndexExpr, (ASTNode*, callee), (ASTNode*, index));

        void Print(std::ostream& os) const override {
            os << "IndexExpr(callee=" << callee
                << ", index=" << index << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return callee->GetToken();
        }
    };

    struct TupleExpr final : Expr {
        TokenComp<TokenType::SymbolLeftParenthesis> leftPar;
        std::vector<ASTNode*> elements;

        COTR2(TupleExpr, (TokenComp<TokenType::SymbolLeftParenthesis>, leftPar), (std::vector<ASTNode*>, elements));

        void Print(std::ostream& os) const override {
            os << "TupleExpr("
                << elements
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return leftPar.Impl();
        }
    };

    struct IfExpr final : Expr {
        TokenComp<TokenType::KeywordIf> ifToken;
        ASTNode* condition;
        std::vector<ASTNode*> body;
        std::vector<ASTNode*> elseBody;

        COTR4(IfExpr, (TokenComp<TokenType::KeywordIf>, ifToken), (ASTNode*, condition), (std::vector<ASTNode*>, body),
              (std::vector<ASTNode*>, elseBody));

        void Print(std::ostream& os) const override {
            os << "IfExpr(condition=" << condition
                << ", body=" << body
                << ", elseBody=" << elseBody
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return ifToken.Impl();
        }
    };

    struct RangeExpr final : Expr {
        ASTNode* start;
        ASTNode* end;
        ASTNode* step;

        COTR3(RangeExpr, (ASTNode*, start), (ASTNode*, end), (ASTNode*, step));

        void Print(std::ostream& os) const override {
            os << "RangeExpr(start=" << start
                << ", end=" << end
                << ", step=" << step
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return start->GetToken();
        }
    };

    struct LambdaExpr final : Expr {
        TokenComp<TokenType::SymbolLeftParenthesis> leftPar;
        std::vector<Parameter> params;
        ASTNode* returns;
        std::vector<ASTNode*> body;

        COTR4(LambdaExpr, (TokenComp<TokenType::SymbolLeftParenthesis>, leftPar), (std::vector<Parameter>, params),
              (ASTNode*, returns), (std::vector<ASTNode*>, body));

        void Print(std::ostream& os) const override {
            os << "LambdaExpr(params=" << params
                << ", returns=" << returns
                << ", body=" << body
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return leftPar;
        }
    };

    struct ScopeExpr final : Expr {
        TokenImpl token_;
        std::vector<ASTNode*> body;

        COTR2(ScopeExpr, (TokenImpl, token_), (std::vector<ASTNode*>, body));

        void Print(std::ostream& os) const override {
            os << "ScopeExpr(" << body << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };


    struct GenericTypeExpr final : TypeExpr {
        ASTNode* id;
        std::vector<ASTNode*> arguments;

        COTR2(GenericTypeExpr, (ASTNode*, id), (std::vector<ASTNode*>, arguments));

        void Print(std::ostream& os) const override {
            os << "GenericTypeExpr(id=" << id
                << ", arguments=" << arguments
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return id->GetToken();
        }
    };

    struct OptionalTypeExpr final : TypeExpr {
        ASTNode* type;

        COTR1(OptionalTypeExpr, (ASTNode*, type));

        void Print(std::ostream& os) const override {
            os << "OptionalTypeExpr(" << type << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return type->GetToken();
        }
    };

    struct FunctionTypeExpr final : TypeExpr {
        ASTNode* returns;
        std::vector<ASTNode*> params;

        COTR2(FunctionTypeExpr, (ASTNode*, returns), (std::vector<ASTNode*>, params));

        void Print(std::ostream& os) const override {
            os << "FunctionTypeExpr(params=" << params
                << ", returns=" << returns
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return returns->GetToken();
        }
    };

    struct ReferenceTypeExpr final : TypeExpr {
        ASTNode* type;

        COTR1(ReferenceTypeExpr, (ASTNode*, type));

        void Print(std::ostream& os) const override {
            os << "ReferenceTypeExpr(" << type << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return type->GetToken();
        }
    };


    struct ReturnStmt final : Stmt {
        ASTNode* value;

        COTR1(ReturnStmt, (ASTNode*, value));

        void Print(std::ostream& os) const override {
            os << "ReturnStmt(" << value << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return value->GetToken();
        }
    };

    struct YieldStmt final : Stmt {
        ASTNode* value;

        COTR1(YieldStmt, (ASTNode*, value));

        void Print(std::ostream& os) const override {
            os << "YieldStmt(" << value << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return value->GetToken();
        }
    };

    struct BreakStmt final : Stmt {
        TokenComp<TokenType::KeywordBreak> token_;

        COTR1(BreakStmt, (TokenComp<TokenType::KeywordBreak>, token_));

        void Print(std::ostream& os) const override {
            os << "BreakStmt()";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct ContinueStmt final : Stmt {
        TokenComp<TokenType::KeywordContinue> token_;

        COTR1(ContinueStmt, (TokenComp<TokenType::KeywordContinue>, token_));

        void Print(std::ostream& os) const override {
            os << "ContinueStmt()";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct ExpressionStmt final : Stmt {
        ASTNode* expr;

        COTR1(ExpressionStmt, (ASTNode*, expr));

        void Print(std::ostream& os) const override {
            os << "ExpressionStmt(" << expr << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return expr->GetToken();
        }
    };

    struct ImportStmt final : Stmt {
        TokenComp<TokenType::KeywordImport> token_;
        bool star;
        ASTNode* module;
        Opt<IdentifierToken> alias;

        COTR4(ImportStmt, (TokenComp<TokenType::KeywordImport>, token_), (bool, star), (ASTNode*, module),
              (Opt<IdentifierToken>, alias));

        void Print(std::ostream& os) const override {
            os << "ImportStmt(star=" << (star ? "true" : "false")
                << ", module=" << module
                << ", alias=" << alias
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct AliasStmt final : Stmt {
        TokenComp<TokenType::KeywordAlias> token_;
        IdentifierToken id;
        ASTNode* type;

        COTR3(AliasStmt, (TokenComp<TokenType::KeywordAlias>, token_), (IdentifierToken, id), (ASTNode*, type));

        void Print(std::ostream& os) const override {
            os << "AliasStmt(id=" << id << ", type=" << type << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct VariableDefineStmt final : Stmt {
        bool constant;
        ASTNode* id;
        ASTNode* type;
        ASTNode* value;

        COTR4(VariableDefineStmt, (bool, constant), (ASTNode*, id), (ASTNode*, type), (ASTNode*, value));

        void Print(std::ostream& os) const override {
            os << "VariableDefineStmt(constant=" << (constant ? "true" : "false")
                << ", id=" << id
                << ", type=" << type <<
                ", value=" << value
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return id->GetToken();
        }
    };

    struct IfStmt final : Stmt {
        TokenComp<TokenType::KeywordIf> token_;
        ASTNode* condition;
        std::vector<ASTNode*> body;
        ASTNode* elseBody;

        COTR4(IfStmt, (TokenComp<TokenType::KeywordIf>, token_), (ASTNode*, condition), (std::vector<ASTNode*>, body),
              (ASTNode*, elseBody));

        void Print(std::ostream& os) const override {
            os << "IfStmt(condition=" << condition
                << ", body=" << body
                << ", elseBody=" << elseBody
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct ForStmt final : Stmt {
        TokenComp<TokenType::KeywordFor> token_;
        ASTNode* iterable;
        std::vector<IdentifierToken> identifiers;
        std::vector<ASTNode*> body;
        std::vector<ASTNode*> elseBody;

        COTR5(ForStmt, (TokenComp<TokenType::KeywordFor>, token_), (ASTNode*, iterable),
              (std::vector<IdentifierToken>, identifiers), (std::vector<ASTNode*>, body),
              (std::vector<ASTNode*>, elseBody));

        void Print(std::ostream& os) const override {
            os << "ForStmt(identifiers=" << identifiers
                << ", iterable=" << iterable
                << ", body=" << body
                << ", elseBody=" << elseBody
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct FunctionStmt final : Stmt {
        TokenComp<TokenType::KeywordFun> token_;
        PropVisibility visibility;
        ASTNode* id;
        ASTNode* returns;
        std::vector<Generic> generics;
        std::vector<Parameter> params;
        std::vector<ASTNode*> body;

        COTR7(FunctionStmt, (TokenComp<TokenType::KeywordFun>, token_), (PropVisibility, visibility), (ASTNode*, id),
              (ASTNode*, returns), (std::vector<Generic>, generics), (std::vector<Parameter>, params),
              (std::vector<ASTNode*>, body));

        void Print(std::ostream& os) const override {
            os << "FunctionStmt(visibility=" << visibility
                << ", id=" << id
                << ", generics=" << generics
                << ", params=" << params
                << ", returns=" << returns
                << ", body=" << body
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct OperatorFunctionStmt final : Stmt {
        TokenComp<TokenType::KeywordFun> token_;
        PropVisibility visibility;
        OperatorType op;
        ASTNode* id;
        ASTNode* returns;
        std::vector<Parameter> params;
        std::vector<Generic> generics;
        std::vector<ASTNode*> body;

        COTR8(OperatorFunctionStmt, (TokenComp<TokenType::KeywordFun>, token_), (PropVisibility, visibility),
              (OperatorType, op), (ASTNode*, id), (ASTNode*, returns), (std::vector<Parameter>, params),
              (std::vector<Generic>, generics), (std::vector<ASTNode*>, body));

        void Print(std::ostream& os) const override {
            os << "BinaryOperatorFunctionStmt(visibility=" << visibility
                << ", id=" << id
                << ", op=" << op
                << ", params=" << params
                << ", generics=" << generics
                << ", returns=" << returns
                << ", body=" << body
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct GetFunctionStmt final : Stmt {
        TokenComp<TokenType::KeywordFun> token_;
        PropVisibility visibility;
        ASTNode* id;
        ASTNode* returns;
        std::vector<Generic> generics;
        std::vector<ASTNode*> body;

        COTR6(GetFunctionStmt, (TokenComp<TokenType::KeywordFun>, token_), (PropVisibility, visibility), (ASTNode*, id),
              (ASTNode*, returns), (std::vector<Generic>, generics), (std::vector<ASTNode*>, body));

        void Print(std::ostream& os) const override {
            os << "GetFunctionStmt(visibility=" << visibility
                << ", id=" << id
                << ", generics=" << generics
                << ", returns=" << returns
                << ", body=" << body
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct SetFunctionStmt final : Stmt {
        TokenComp<TokenType::KeywordFun> token_;
        PropVisibility visibility;
        ASTNode* id;
        Parameter param;
        std::vector<Generic> generics;
        std::vector<ASTNode*> body;

        COTR6(SetFunctionStmt, (TokenComp<TokenType::KeywordFun>, token_), (PropVisibility, visibility), (ASTNode*, id),
              (Parameter, param), (std::vector<Generic>, generics), (std::vector<ASTNode*>, body));

        void Print(std::ostream& os) const override {
            os << "SetFunctionStmt(visibility=" << visibility
                << ", id=" << id
                << ", generics=" << generics
                << ", param=" << param
                << ", body=" << body
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct WhileStmt final : Stmt {
        TokenComp<TokenType::KeywordWhile> token_;
        ASTNode* condition;
        std::vector<ASTNode*> body;
        std::vector<ASTNode*> elseBody;

        COTR4(WhileStmt, (TokenComp<TokenType::KeywordWhile>, token_), (ASTNode*, condition),
              (std::vector<ASTNode*>, body), (std::vector<ASTNode*>, elseBody));

        void Print(std::ostream& os) const override {
            os << "WhileStmt(condition=" << condition
                << ", body=" << body
                << ", elseBody=" << elseBody
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct DoWhileStmt final : Stmt {
        TokenComp<TokenType::KeywordDo> token_;
        ASTNode* condition;
        std::vector<ASTNode*> body;
        std::vector<ASTNode*> elseBody;

        COTR4(DoWhileStmt, (TokenComp<TokenType::KeywordDo>, token_), (ASTNode*, condition),
              (std::vector<ASTNode*>, body), (std::vector<ASTNode*>, elseBody));

        void Print(std::ostream& os) const override {
            os << "DoWhileStmt(body=" << body
                << ", condition=" << condition
                << ", elseBody=" << elseBody
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct StructStmt final : Stmt {
        TokenComp<TokenType::KeywordStruct> token_;
        bool isEnum;
        IdentifierToken id;
        ASTNode* extends;
        std::vector<Generic> generics;
        std::vector<ASTNode*> implements;
        std::vector<ASTNode*> statements;
        std::vector<EnumMember> enumMembers;

        COTR8(StructStmt, (TokenComp<TokenType::KeywordStruct>, token_), (bool, isEnum), (IdentifierToken, id),
              (ASTNode*, extends), (std::vector<Generic>, generics), (std::vector<ASTNode*>, implements),
              (std::vector<ASTNode*>, statements), (std::vector<EnumMember>, enumMembers));

        void Print(std::ostream& os) const override {
            os << "StructStmt(id=" << id
                << ", isEnum=" << (isEnum ? "true" : "false")
                << ", generics=" << generics
                << ", extends=" << extends
                << ", implements=" << implements
                << ", statements=" << statements
                << ", enumMembers=" << enumMembers
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct InterfaceStmt final : Stmt {
        TokenComp<TokenType::KeywordInterface> token_;
        ASTNode* id;
        std::vector<Generic> generics;
        std::vector<ASTNode*> extends;
        std::vector<ASTNode*> statements;

        COTR5(InterfaceStmt, (TokenComp<TokenType::KeywordInterface>, token_), (ASTNode*, id),
              (std::vector<Generic>, generics), (std::vector<ASTNode*>, extends), (std::vector<ASTNode*>, statements));

        void Print(std::ostream& os) const override {
            os << "InterfaceStmt(id=" << id
                << ", generics=" << generics
                << ", extends=" << extends
                << ", statements=" << statements
                << ")";
        }

        constexpr TokenImpl GetToken() const override {
            return token_;
        }
    };

    struct Parser {
    private:
        const Tokenizer& tokenizer;
        const FileTokenizer& cur;
        size_t current{0};
        Arena& arena;

    public:
        ASTNode* noneNode = CreateNode<NoneNode>();

        explicit Parser(Arena& arena, const Tokenizer& tokenizer)
            : tokenizer(tokenizer), cur(tokenizer.files.begin()->second), arena(arena) {
        }

        [[nodiscard]] constexpr size_t TokenCount() const {
            return cur.tokens.size();
        }

        [[nodiscard]] Token Get(size_t index) const {
            if (index >= TokenCount()) return {TokenType::EndOfFile, cur._content.end()._Ptr, 0};
            return cur.tokens[index];
        }

        [[nodiscard]] Token Peek() const {
            if (Over()) return {TokenType::EndOfFile, cur._content.end()._Ptr, 0};
            return cur.tokens[current];
        }

        [[nodiscard]] bool PeekIs(TokenType type) const {
            return Peek().type == type;
        }

        [[nodiscard]] Token PeekNoLine() const {
            for (auto i = current; i < TokenCount(); i++) {
                if (cur.tokens[i].type != TokenType::NewLine) {
                    return cur.tokens[i];
                }
            }
            return {TokenType::EndOfFile, cur._content.end()._Ptr, 0};
        }

        [[nodiscard]] Token Peek(int offset) const {
            if (current + offset >= TokenCount()) return {TokenType::EndOfFile, cur._content.end()._Ptr, 0};
            return cur.tokens[current + offset];
        }

        [[nodiscard]] Token PeekNoLine(int offset) const {
            for (auto i = current + offset; i < TokenCount(); i++) {
                if (cur.tokens[i].type != TokenType::NewLine) {
                    return cur.tokens[i];
                }
            }
            return {TokenType::EndOfFile, cur._content.end()._Ptr, 0};
        }

        [[nodiscard]] Token Next() {
            if (Over()) return {TokenType::EndOfFile, cur._content.end()._Ptr, 0};
            return cur.tokens[current++];
        }

        void Expect(TokenType type) {
            if (!PeekIs(type)) {
                ThrowError(
                    "Expected '" + GetTokenValue(type) + "', got '" +
                    std::string(Peek().value) + (
                        Peek().type <= TokenType::NewLine ? "'" : "' which is a " + GetTokenTypeName(Peek().type)
                    ), Peek()
                );
            }
            current++;
        }

        [[nodiscard]] bool Over() const {
            return current >= TokenCount();
        }

        [[noreturn]] void ThrowError(const std::string& message) const {
            cur.ThrowError(message, Peek());
        }

        [[noreturn]] void ThrowError(const std::string& message, const TokenImpl& token) const {
            cur.ThrowError(message, token.value);
        }

        [[noreturn]] void ThrowError(const std::string& message, const std::string_view& view) const {
            cur.ThrowError(message, view);
        }

        [[noreturn]] void ThrowError(const std::string& message, size_t start, size_t length = 1) const {
            cur.ThrowError(message, cur._content.data() + start, length);
        }

        [[noreturn]] void ThrowError(const std::string& message, const ASTNode* node) const {
            ThrowError(message, node->GetToken());
        }

        template <typename T, typename... Args>
        T* CreateNode(Args&&... args) {
            return ASTNode::Create<T>(arena, std::forward<Args>(args)...);
        }

        void SkipNewLines() {
            while (PeekIs(TokenType::NewLine)) current++;
        }

        ASTNode* ParseSingle();
        ASTNode* ParseExpr(int rbp = 0, bool ignoreNewLines = false, bool endAtSet = false, bool endAtGt = false,
                           bool allowType = false);
        void ParseExpressions(std::vector<ASTNode*>& result, bool ignoreNewLines = false);
        ASTNode* ParseTypeExpr(bool ignoreNewLines = false, bool endAtGt = false);
        void ParseTypeExpressions(std::vector<ASTNode*>& result);
        void ParseGenerics(std::vector<Generic>& result);
        ASTNode* ParsePropertyExpr(); // despite the name it might return VariableExpr
        void ParseFunctionArgument(Parameter& param);
        void ParseFunctionArguments(std::vector<Parameter>& result);
        ASTNode* ParseVariableDefine(bool const_);
        ASTNode* ParseForLoop(TokenComp<TokenType::KeywordFor> tok);
        ASTNode* ParseWhileLoop(TokenComp<TokenType::KeywordWhile> tok);
        ASTNode* ParseDoWhileLoop(TokenComp<TokenType::KeywordDo> tok);
        ASTNode* ParseAliasStmt(TokenComp<TokenType::KeywordAlias> tok);
        ASTNode* ParseImportStmt(TokenComp<TokenType::KeywordImport> tok);
        ASTNode* ParseFunctionStmt(TokenComp<TokenType::KeywordFun> funTok, PropVisibility visibility);
        ASTNode* ParseStructStmt(TokenComp<TokenType::KeywordStruct> tok, bool isEnum);
        ASTNode* ParseInterfaceStmt(TokenComp<TokenType::KeywordInterface> tok);
        ASTNode* ParseStatement();
        void ParseBracedBlock(std::vector<ASTNode*>& result);
        void ParseStatements(std::vector<ASTNode*>& result);
    };
}
