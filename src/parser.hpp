#pragma once
#include <optional>
#include <variant>

#include "arena.hpp"
#include "../.old/arena_vec_hpp_old"
#include "lexer.hpp"
#include "utils.hpp"

namespace Flame {
    enum class ASTNodeKind : uint8_t {
        NONE,
        VariableExpr, PropertyExpr, VariableIncDecExpr, CallExpr, UnaryExpr, BinaryExpr, LiteralExpr,
        FStringExpr, IndexExpr, TupleExpr, IfExpr, RangeExpr, LambdaExpr, ScopeExpr,

        IdentifierTypeExpr, ArrayTypeExpr, OptionalTypeExpr, FunctionTypeExpr, ReferenceTypeExpr,

        ReturnStmt, BreakStmt, ContinueStmt, ExpressionStmt, ImportStmt, AliasStmt,
        VariableDefineStmt, IfStmt, ForStmt, FunctionStmt, GetFunctionStmt,
        SetFunctionStmt, WhileStmt, BinaryOpFunctionStmt, UnaryOpFunctionStmt,
        DoWhileStmt, ClassStmt
    };

    struct ASTNode {
        ASTNodeKind type;

        ASTNode() : type(ASTNodeKind::NONE), ptr(nullptr) {
        };

        ASTNode(ASTNodeKind type, void* ptr) : type(type), ptr(ptr) {
        };

        template <typename T, typename... Args>
        static ASTNode create(Arena& arena, ASTNodeKind type, Args&&... args) {
            auto mem = arena.alloc<T>();
            new(mem) T(std::forward<Args>(args)...);
            return ASTNode{type, mem};
        }

        template <typename T>
        [[nodiscard]] T& as() const {
            return *static_cast<T*>(ptr);
        }

    private:
        void* ptr;
    };

    std::ostream& operator<<(std::ostream& os, const ASTNode& node);

    struct Parameter {
        IdentifierToken id;
        ASTNode type;
        ASTNode default_;

        friend std::ostream& operator<<(std::ostream& os, const Parameter& par) {
            os << "Parameter(id=" << par.id
                << ", type=" << par.type
                << ", default=" << par.default_
                << ")";
            return os;
        }
    };

    struct GenericParameter {
        IdentifierToken id;
        ASTNode constraint;

        friend std::ostream& operator<<(std::ostream& os, const GenericParameter& par) {
            os << "GenericParameter(id=" << par.id << ", constraint=" << par.constraint << ")";
            return os;
        }
    };

    struct EnumMember {
        bool manual{};
        IdentifierToken id;
        ASTNode value;

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
        ASTNode value;

        friend std::ostream& operator<<(std::ostream& os, const CallArgument& par) {
            os << "CallArgument(spread=" << (par.spread ? "true" : "false")
                << ", name=" << par.name
                << ", value=" << par.value
                << ")";
            return os;
        }
    };

    enum class PropVisibility {
        Public    = 1,
        Protected = 2,
        Private   = 4
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

    struct NoneNode {
        friend std::ostream& operator<<(std::ostream& os, const NoneNode& par) {
            os << "NONE";
            return os;
        }
    };


    struct VariableExpr {
        IdentifierToken name;

        friend std::ostream& operator<<(std::ostream& os, const VariableExpr& par) {
            os << "VariableExpr(" << par.name << ")";
            return os;
        }
    };

    struct PropertyExpr {
        ASTNode callee;
        IdentifierToken id;

        friend std::ostream& operator<<(std::ostream& os, const PropertyExpr& par) {
            os << "PropertyExpr(" << par.callee << ", " << par.id << ")";
            return os;
        }
    };

    struct VariableIncDecExpr {
        enum class Type {
            IncLeft,
            IncRight,
            DecLeft,
            DecRight
        };

        Type type{};
        ASTNode var;

        friend std::ostream& operator<<(std::ostream& os, const VariableIncDecExpr& par) {
            os << "VariableIncDecExpr(";
            switch (par.type) {
            case Type::IncLeft:
                os << "++";
                os << ", " << par.var << ")";
                break;
            case Type::IncRight:
                os << par.var;
                os << ", ++)";
                break;
            case Type::DecLeft:
                os << "--";
                os << ", " << par.var << ")";
                break;
            case Type::DecRight:
                os << par.var;
                os << ", --)";
                break;
            }
            return os;
        }
    };

    struct CallExpr {
        ASTNode callee;
        std::vector<CallArgument> args;
        std::vector<ASTNode> typeArgs;

        friend std::ostream& operator<<(std::ostream& os, const CallExpr& par) {
            os << "CallExpr(callee=" << par.callee
                << ", args=" << par.args
                << ", typeArgs=" << par.typeArgs
                << ")";
            return os;
        }
    };

    struct UnaryExpr {
        OperatorToken op;
        ASTNode expr;

        friend std::ostream& operator<<(std::ostream& os, const UnaryExpr& par) {
            os << "UnaryExpr(" << par.op << ", " << par.expr << ")";
            return os;
        }
    };

    struct BinaryExpr {
        ASTNode left;
        OperatorToken op;
        ASTNode right;

        friend std::ostream& operator<<(std::ostream& os, const BinaryExpr& par) {
            os << "BinaryExpr(" << par.left << ", " << par.op << ", " << par.right << ")";
            return os;
        }
    };

    struct LiteralExpr {
        Token value; // IntegerToken | FloatToken | StringToken | BooleanToken | KeywordNullToken

        friend std::ostream& operator<<(std::ostream& os, const LiteralExpr& par) {
            os << "Literal(" << par.value << ")";
            return os;
        }
    };

    struct FStringExpr {
        StringStartToken start;
        std::vector<ASTNode> expressions;
        std::vector<StringMiddleToken> middles;
        StringEndToken end;

        friend std::ostream& operator<<(std::ostream& os, const FStringExpr& par) {
            os << "FStringExpr(start=" << par.start
                << ", expressions=" << par.expressions
                << ", middles=" << par.middles
                << ", end=" << par.end
                << ")";
            return os;
        }
    };

    struct IndexExpr {
        ASTNode callee;
        ASTNode index;

        friend std::ostream& operator<<(std::ostream& os, const IndexExpr& par) {
            os << "IndexExpr(callee=" << par.callee
                << ", index=" << par.index << ")";
            return os;
        }
    };

    struct TupleExpr {
        Token leftPar;
        std::vector<ASTNode> elements;

        friend std::ostream& operator<<(std::ostream& os, const TupleExpr& par) {
            os << "TupleExpr("
                << par.elements
                << ")";
            return os;
        }
    };

    struct IfExpr {
        Token ifToken;
        ASTNode condition;
        std::vector<ASTNode> body;
        std::vector<ASTNode> elseBody;

        friend std::ostream& operator<<(std::ostream& os, const IfExpr& par) {
            os << "IfExpr(condition=" << par.condition
                << ", body=" << par.body
                << ", elseBody=" << par.elseBody
                << ")";
            return os;
        }
    };

    struct RangeExpr {
        ASTNode start;
        ASTNode end;
        ASTNode step;

        friend std::ostream& operator<<(std::ostream& os, const RangeExpr& par) {
            os << "RangeExpr(start=" << par.start
                << ", end=" << par.end
                << ", step=" << par.step
                << ")";
            return os;
        }
    };

    struct LambdaExpr {
        std::vector<Parameter> params;
        ASTNode returns;
        std::vector<ASTNode> body;

        friend std::ostream& operator<<(std::ostream& os, const LambdaExpr& par) {
            os << "LambdaExpr(params=" << par.params
                << ", returns=" << par.returns
                << ", body=" << par.body
                << ")";
            return os;
        }
    };

    struct ScopeExpr {
        std::vector<ASTNode> body;

        friend std::ostream& operator<<(std::ostream& os, const ScopeExpr& par) {
            os << "ScopeExpr(" << par.body << ")";
            return os;
        }
    };


    struct IdentifierTypeExpr {
        IdentifierToken id;
        std::vector<ASTNode> arguments;

        friend std::ostream& operator<<(std::ostream& os, const IdentifierTypeExpr& par) {
            os << "IdentifierTypeExpr(id=" << par.id
                << ", arguments=" << par.arguments
                << ")";
            return os;
        }
    };

    struct ArrayTypeExpr {
        ASTNode elementType;
        ASTNode size;

        friend std::ostream& operator<<(std::ostream& os, const ArrayTypeExpr& par) {
            os << "ArrayTypeExpr(elementType=" << par.elementType
                << ", size=" << par.size
                << ")";
            return os;
        }
    };

    struct OptionalTypeExpr {
        ASTNode type;

        friend std::ostream& operator<<(std::ostream& os, const OptionalTypeExpr& par) {
            os << "OptionalTypeExpr(" << par.type << ")";
            return os;
        }
    };

    struct FunctionTypeExpr {
        std::vector<ASTNode> params;
        ASTNode returns;

        friend std::ostream& operator<<(std::ostream& os, const FunctionTypeExpr& par) {
            os << "FunctionTypeExpr(params=" << par.params
                << ", returns=" << par.returns
                << ")";
            return os;
        }
    };

    struct ReferenceTypeExpr {
        ASTNode type;

        friend std::ostream& operator<<(std::ostream& os, const ReferenceTypeExpr& par) {
            os << "ReferenceTypeExpr(" << par.type << ")";
            return os;
        }
    };


    struct ReturnStmt {
        ASTNode value;

        friend std::ostream& operator<<(std::ostream& os, const ReturnStmt& par) {
            os << "ReturnStmt(" << par.value << ")";
            return os;
        }
    };

    struct BreakStmt {
        Token token;

        friend std::ostream& operator<<(std::ostream& os, const BreakStmt& par) {
            os << "BreakStmt()";
            return os;
        }
    };

    struct ContinueStmt {
        Token token;

        friend std::ostream& operator<<(std::ostream& os, const ContinueStmt& par) {
            os << "ContinueStmt()";
            return os;
        }
    };

    struct ExpressionStmt {
        ASTNode expr;

        friend std::ostream& operator<<(std::ostream& os, const ExpressionStmt& par) {
            os << "ExpressionStmt(" << par.expr << ")";
            return os;
        }
    };

    struct ImportStmt {
        bool star;
        ASTNode module;
        Opt<IdentifierToken> alias;

        friend std::ostream& operator<<(std::ostream& os, const ImportStmt& par) {
            os << "ImportStmt(star=" << (par.star ? "true" : "false")
                << ", module=" << par.module
                << ", alias=" << par.alias
                << ")";
            return os;
        }
    };

    struct AliasStmt {
        IdentifierToken id;
        ASTNode type;

        friend std::ostream& operator<<(std::ostream& os, const AliasStmt& par) {
            os << "AliasStmt(id=" << par.id << ", type=" << par.type << ")";
            return os;
        }
    };

    struct VariableDefineStmt {
        bool constant;
        ASTNode id;
        ASTNode type;
        ASTNode value;

        friend std::ostream& operator<<(std::ostream& os, const VariableDefineStmt& par) {
            os << "VariableDefineStmt(constant=" << (par.constant ? "true" : "false")
                << ", id=" << par.id
                << ", type=" << par.type <<
                ", value=" << par.value
                << ")";
            return os;
        }
    };

    struct IfStmt {
        std::vector<ASTNode> conditions;
        std::vector<std::vector<ASTNode>> bodies;
        std::vector<ASTNode> elseBody;

        friend std::ostream& operator<<(std::ostream& os, const IfStmt& par) {
            os << "IfStmt(conditions=" << par.conditions
                << ", bodies=" << par.bodies
                << ", elseBody=" << par.elseBody
                << ")";
            return os;
        }
    };

    struct ForStmt {
        std::vector<IdentifierToken> identifiers;
        ASTNode iterable;
        std::vector<ASTNode> body;
        std::vector<ASTNode> elseBody;

        friend std::ostream& operator<<(std::ostream& os, const ForStmt& par) {
            os << "ForStmt(identifiers=" << par.identifiers
                << ", iterable=" << par.iterable
                << ", body=" << par.body
                << ", elseBody=" << par.elseBody
                << ")";
            return os;
        }
    };

    struct FunctionStmt {
        PropVisibility visibility;
        ASTNode id;
        std::vector<GenericParameter> generics;
        std::vector<Parameter> params;
        ASTNode returns;
        std::vector<ASTNode> body;

        friend std::ostream& operator<<(std::ostream& os, const FunctionStmt& par) {
            os << "FunctionStmt(visibility=" << par.visibility
                << ", id=" << par.id
                << ", generics=" << par.generics
                << ", params=" << par.params
                << ", returns=" << par.returns
                << ", body=" << par.body
                << ")";
            return os;
        }
    };

    struct BinaryOpFunctionStmt {
        PropVisibility visibility;
        ASTNode id;
        std::vector<Parameter> params;
        ASTNode returns;
        std::vector<ASTNode> body;

        friend std::ostream& operator<<(std::ostream& os, const BinaryOpFunctionStmt& par) {
            os << "BinaryOpFunctionStmt(visibility=" << par.visibility
                << ", id=" << par.id
                << ", params=" << par.params
                << ", returns=" << par.returns
                << ", body=" << par.body
                << ")";
            return os;
        }
    };

    struct UnaryOpFunctionStmt {
        PropVisibility visibility;
        ASTNode id;
        std::optional<Parameter> param;
        ASTNode returns;
        std::vector<ASTNode> body;

        friend std::ostream& operator<<(std::ostream& os, const UnaryOpFunctionStmt& par) {
            os << "UnaryOpFunctionStmt(visibility=" << par.visibility
                << ", id=" << par.id
                << ", param=" << par.param
                << ", returns=" << par.returns
                << ", body=" << par.body
                << ")";
            return os;
        }
    };

    struct GetFunctionStmt {
        PropVisibility visibility;
        ASTNode id;
        std::optional<Parameter> param;
        ASTNode returns;
        std::vector<ASTNode> body;

        friend std::ostream& operator<<(std::ostream& os, const GetFunctionStmt& par) {
            os << "GetFunctionStmt(visibility=" << par.visibility
                << ", id=" << par.id
                << ", param=" << par.param
                << ", returns=" << par.returns
                << ", body=" << par.body
                << ")";
            return os;
        }
    };

    struct SetFunctionStmt {
        PropVisibility visibility;
        ASTNode id;
        std::vector<Parameter> params;
        std::vector<ASTNode> body;

        friend std::ostream& operator<<(std::ostream& os, const SetFunctionStmt& par) {
            os << "SetFunctionStmt(visibility=" << par.visibility
                << ", id=" << par.id
                << ", params=" << par.params
                << ", body=" << par.body
                << ")";
            return os;
        }
    };

    struct WhileStmt {
        ASTNode condition;
        std::vector<ASTNode> body;
        std::vector<ASTNode> elseBody;

        friend std::ostream& operator<<(std::ostream& os, const WhileStmt& par) {
            os << "WhileStmt(condition=" << par.condition
                << ", body=" << par.body
                << ", elseBody=" << par.elseBody
                << ")";
            return os;
        }
    };

    struct DoWhileStmt {
        std::vector<ASTNode> body;
        ASTNode condition;
        std::vector<ASTNode> elseBody;

        friend std::ostream& operator<<(std::ostream& os, const DoWhileStmt& par) {
            os << "DoWhileStmt(body=" << par.body
                << ", condition=" << par.condition
                << ", elseBody=" << par.elseBody
                << ")";
            return os;
        }
    };

    struct ClassStmt {
        bool isEnum;
        IdentifierToken id;
        std::vector<GenericParameter> generics;
        ASTNode superClass;
        std::vector<GenericParameter> superClassGenerics;
        std::vector<FunctionStmt*> constructors;
        std::vector<VariableDefineStmt*> properties;
        std::vector<GetFunctionStmt*> getters;
        std::vector<SetFunctionStmt*> setters;
        std::vector<FunctionStmt*> methods;
        std::vector<FunctionStmt*> staticMethods;
        std::vector<BinaryOpFunctionStmt*> binaryOperators;
        std::vector<UnaryOpFunctionStmt*> unaryOperators;
        std::vector<EnumMember> enumMembers;

        friend std::ostream& operator<<(std::ostream& os, const ClassStmt& par) {
            os << "ClassStmt(id=" << par.id
                << ", isEnum=" << (par.isEnum ? "true" : "false")
                << ", generics=" << par.generics
                << ", superClass=" << par.superClass
                << ", superClassGenerics=" << par.superClassGenerics
                << ", constructors=" << par.constructors
                << ", properties=" << par.properties
                << ", getters=" << par.getters
                << ", setters=" << par.setters
                << ", methods=" << par.methods
                << ", staticMethods=" << par.staticMethods
                << ", binaryOperators=" << par.binaryOperators
                << ", unaryOperators=" << par.unaryOperators
                << ", enumMembers=" << par.enumMembers
                << ")";
            return os;
        }
    };

    template <typename Visitor>
    auto visit(const ASTNode& node, Visitor&& visitor) {
        switch (node.type) {
        case ASTNodeKind::NONE:
            return visitor(node.as<NoneNode>());
        case ASTNodeKind::VariableExpr:
            return visitor(node.as<VariableExpr>());
        case ASTNodeKind::PropertyExpr:
            return visitor(node.as<PropertyExpr>());
        case ASTNodeKind::VariableIncDecExpr:
            return visitor(node.as<VariableIncDecExpr>());
        case ASTNodeKind::CallExpr:
            return visitor(node.as<CallExpr>());
        case ASTNodeKind::UnaryExpr:
            return visitor(node.as<UnaryExpr>());
        case ASTNodeKind::BinaryExpr:
            return visitor(node.as<BinaryExpr>());
        case ASTNodeKind::LiteralExpr:
            return visitor(node.as<LiteralExpr>());
        case ASTNodeKind::FStringExpr:
            return visitor(node.as<FStringExpr>());
        case ASTNodeKind::IndexExpr:
            return visitor(node.as<IndexExpr>());
        case ASTNodeKind::TupleExpr:
            return visitor(node.as<TupleExpr>());
        case ASTNodeKind::IfExpr:
            return visitor(node.as<IfExpr>());
        case ASTNodeKind::RangeExpr:
            return visitor(node.as<RangeExpr>());
        case ASTNodeKind::LambdaExpr:
            return visitor(node.as<LambdaExpr>());
        case ASTNodeKind::ScopeExpr:
            return visitor(node.as<ScopeExpr>());
        case ASTNodeKind::IdentifierTypeExpr:
            return visitor(node.as<IdentifierTypeExpr>());
        case ASTNodeKind::ArrayTypeExpr:
            return visitor(node.as<ArrayTypeExpr>());
        case ASTNodeKind::OptionalTypeExpr:
            return visitor(node.as<OptionalTypeExpr>());
        case ASTNodeKind::FunctionTypeExpr:
            return visitor(node.as<FunctionTypeExpr>());
        case ASTNodeKind::ReferenceTypeExpr:
            return visitor(node.as<ReferenceTypeExpr>());
        case ASTNodeKind::ReturnStmt:
            return visitor(node.as<ReturnStmt>());
        case ASTNodeKind::BreakStmt:
            return visitor(node.as<BreakStmt>());
        case ASTNodeKind::ContinueStmt:
            return visitor(node.as<ContinueStmt>());
        case ASTNodeKind::ExpressionStmt:
            return visitor(node.as<ExpressionStmt>());
        case ASTNodeKind::ImportStmt:
            return visitor(node.as<ImportStmt>());
        case ASTNodeKind::AliasStmt:
            return visitor(node.as<AliasStmt>());
        case ASTNodeKind::VariableDefineStmt:
            return visitor(node.as<VariableDefineStmt>());
        case ASTNodeKind::IfStmt:
            return visitor(node.as<IfStmt>());
        case ASTNodeKind::ForStmt:
            return visitor(node.as<ForStmt>());
        case ASTNodeKind::FunctionStmt:
            return visitor(node.as<FunctionStmt>());
        case ASTNodeKind::BinaryOpFunctionStmt:
            return visitor(node.as<BinaryOpFunctionStmt>());
        case ASTNodeKind::UnaryOpFunctionStmt:
            return visitor(node.as<UnaryOpFunctionStmt>());
        case ASTNodeKind::GetFunctionStmt:
            return visitor(node.as<GetFunctionStmt>());
        case ASTNodeKind::SetFunctionStmt:
            return visitor(node.as<SetFunctionStmt>());
        case ASTNodeKind::WhileStmt:
            return visitor(node.as<WhileStmt>());
        case ASTNodeKind::DoWhileStmt:
            return visitor(node.as<DoWhileStmt>());
        case ASTNodeKind::ClassStmt:
            return visitor(node.as<ClassStmt>());
        }
        return visitor(node.as<NoneNode>());
    }

    struct Parser {
    private:
        const Tokenizer& tokenizer;
        const FileTokenizer& cur;
        size_t current{0};
        Arena& arena;
        ASTNode noneNode{ASTNode::create<NoneNode>(arena, ASTNodeKind::NONE)};

    public:
        explicit Parser(Arena& arena, const Tokenizer& tokenizer)
            : tokenizer(tokenizer), cur(tokenizer.files.begin()->second), arena(arena) {
        }

        [[nodiscard]] Token Get(size_t index) const {
            if (index >= cur.tokens.size()) return {TokenType::EndOfFile, cur.content.end()._Ptr, 0};
            return cur.tokens[index];
        }

        [[nodiscard]] Token Peek() const {
            if (Over()) return {TokenType::EndOfFile, cur.content.end()._Ptr, 0};
            return cur.tokens[current];
        }

        [[nodiscard]] Token PeekNoLine() const {
            for (auto i = current; i < cur.tokens.size(); i++) {
                if (cur.tokens[i].type != TokenType::NewLine) {
                    return cur.tokens[i];
                }
            }
            return {TokenType::EndOfFile, cur.content.end()._Ptr, 0};
        }

        [[nodiscard]] Token Peek(int offset) const {
            if (current + offset >= cur.tokens.size()) return {TokenType::EndOfFile, cur.content.end()._Ptr, 0};
            return cur.tokens[current + offset];
        }

        [[nodiscard]] Token PeekNoLine(int offset) const {
            for (auto i = current + offset; i < cur.tokens.size(); i++) {
                if (cur.tokens[i].type != TokenType::NewLine) {
                    return cur.tokens[i];
                }
            }
            return {TokenType::EndOfFile, cur.content.end()._Ptr, 0};
        }

        [[nodiscard]] Token Next() {
            if (Over()) return {TokenType::EndOfFile, cur.content.end()._Ptr, 0};
            return cur.tokens[current++];
        }

        void Expect(TokenType type) {
            if (Peek().type != type) {
                ThrowError(
                    "Expected '" + GetTokenValue(type) + "', got '" +
                    std::string(Peek().value) + "' which is a " + GetTokenTypeName(Peek().type), Peek()
                );
            }
            current++;
        }

        void Expect(SymbolType type) {
            return Expect(static_cast<TokenType>(type));
        }

        void Expect(OperatorType type) {
            return Expect(static_cast<TokenType>(type));
        }

        [[nodiscard]] bool Over() const {
            return current >= cur.tokens.size();
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
            cur.ThrowError(message, start, length);
        }

        [[noreturn]] void ThrowError(const std::string& message, const ASTNode& node) const {
            switch (node.type) {
            case ASTNodeKind::NONE:
                throw std::runtime_error(message + " at NONE node");
            case ASTNodeKind::VariableExpr:
                ThrowError(message, node.as<VariableExpr>().name);
            case ASTNodeKind::PropertyExpr:
                ThrowError(message, node.as<PropertyExpr>().callee);
            case ASTNodeKind::VariableIncDecExpr:
                ThrowError(message, node.as<VariableIncDecExpr>().var);
            case ASTNodeKind::CallExpr:
                ThrowError(message, node.as<CallExpr>().callee);
            case ASTNodeKind::UnaryExpr:
                ThrowError(message, node.as<UnaryExpr>().op);
            case ASTNodeKind::BinaryExpr:
                ThrowError(message, node.as<BinaryExpr>().left);
            case ASTNodeKind::LiteralExpr:
                ThrowError(message, node.as<LiteralExpr>().value);
            case ASTNodeKind::FStringExpr:
                ThrowError(message, node.as<FStringExpr>().start);
            case ASTNodeKind::IndexExpr:
                ThrowError(message, node.as<IndexExpr>().callee);
            case ASTNodeKind::TupleExpr:
                ThrowError(message, node.as<TupleExpr>().leftPar);
            case ASTNodeKind::IfExpr:
                ThrowError(message, node.as<IfExpr>().ifToken);
            case ASTNodeKind::RangeExpr:
                ThrowError(message, node.as<RangeExpr>().start);
            case ASTNodeKind::LambdaExpr:
                ThrowError(message, node.as<LambdaExpr>().params[0].id);
            case ASTNodeKind::ScopeExpr:
                ThrowError(message, node.as<ScopeExpr>().body[0]);
            case ASTNodeKind::IdentifierTypeExpr:
                ThrowError(message, node.as<IdentifierTypeExpr>().id);
            case ASTNodeKind::ArrayTypeExpr:
                ThrowError(message, node.as<ArrayTypeExpr>().elementType);
            case ASTNodeKind::OptionalTypeExpr:
                ThrowError(message, node.as<OptionalTypeExpr>().type);
            case ASTNodeKind::FunctionTypeExpr:
                ThrowError(message, node.as<FunctionTypeExpr>().params[0]);
            case ASTNodeKind::ReferenceTypeExpr:
                ThrowError(message, node.as<ReferenceTypeExpr>().type);
            case ASTNodeKind::ReturnStmt:
                ThrowError(message, node.as<ReturnStmt>().value);
            case ASTNodeKind::BreakStmt:
                ThrowError(message, node.as<BreakStmt>().token);
            case ASTNodeKind::ContinueStmt:
                ThrowError(message, node.as<ContinueStmt>().token);
            case ASTNodeKind::ExpressionStmt:
                ThrowError(message, node.as<ExpressionStmt>().expr);
            case ASTNodeKind::ImportStmt:
                ThrowError(message, node.as<ImportStmt>().module);
            case ASTNodeKind::AliasStmt:
                ThrowError(message, node.as<AliasStmt>().id);
            case ASTNodeKind::VariableDefineStmt:
                ThrowError(message, node.as<VariableDefineStmt>().id);
            case ASTNodeKind::IfStmt:
                ThrowError(message, node.as<IfStmt>().conditions[0]);
            case ASTNodeKind::ForStmt:
                ThrowError(message, node.as<ForStmt>().iterable);
            case ASTNodeKind::FunctionStmt:
                ThrowError(message, node.as<FunctionStmt>().id);
            case ASTNodeKind::BinaryOpFunctionStmt:
                ThrowError(message, node.as<BinaryOpFunctionStmt>().id);
            case ASTNodeKind::UnaryOpFunctionStmt:
                ThrowError(message, node.as<UnaryOpFunctionStmt>().id);
            case ASTNodeKind::GetFunctionStmt:
                ThrowError(message, node.as<GetFunctionStmt>().id);
            case ASTNodeKind::SetFunctionStmt:
                ThrowError(message, node.as<SetFunctionStmt>().id);
            case ASTNodeKind::WhileStmt:
                ThrowError(message, node.as<WhileStmt>().condition);
            case ASTNodeKind::DoWhileStmt:
                ThrowError(message, node.as<DoWhileStmt>().body[0]);
            case ASTNodeKind::ClassStmt:
                ThrowError(message, node.as<ClassStmt>().id);
            }
        }

        template <typename T, typename... Args>
        ASTNode CreateNode(ASTNodeKind type, Args&&... args) {
            void* mem = arena.alloc<T>();
            new(mem) T(std::forward<Args>(args)...);
            return ASTNode{type, mem};
        }

        void SkipNewLines() {
            while (Peek().type == TokenType::NewLine) current++;
        }

        ASTNode ParseSingle();
        ASTNode ParseExpr(int rbp = 0, bool endAtGt = false, bool ignoreNewLines = false);
        void ParseExpressions(std::vector<ASTNode>& result, bool endAtGt = false, bool ignoreNewLines = false);
        ASTNode ParseSingleTypeExpr(bool allowExpr = false);
        ASTNode ParseTypeExpr(bool allowExpr = false);
        void ParseTypeExpressions(std::vector<ASTNode>& result, bool allowExpr = false);
        ASTNode ParsePropertyExpr(); // despite the name it might return VariableExpr
        ASTNode ParseStmt();
        void ParseBracedBlock(std::vector<ASTNode>& result);
        void ParseStatements(std::vector<ASTNode>& result);
    };
}
