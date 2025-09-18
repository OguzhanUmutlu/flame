#include "parser.hpp"

using namespace Flame;

namespace Flame {
    std::ostream& operator<<(std::ostream& os, const ASTNode& node) {
        visit(node, [&](const auto& n) {
            os << n;
        });
        return os;
    }
}

int LeftBindingPower(OperatorType op) {
    switch (op) {
    case OperatorType::Assign:
    case OperatorType::SetShl:
    case OperatorType::SetShr:
    case OperatorType::SetAdd:
    case OperatorType::SetSub:
    case OperatorType::SetMul:
    case OperatorType::SetDiv:
    case OperatorType::SetMod:
    case OperatorType::SetBitAnd:
    case OperatorType::SetBitOr:
    case OperatorType::SetBitXor:
        return 10;

    case OperatorType::Or:
        return 20;

    case OperatorType::Eq:
    case OperatorType::Neq:
        return 30;

    case OperatorType::Lt:
    case OperatorType::Gt:
    case OperatorType::Lte:
    case OperatorType::Gte:
        return 40;

    case OperatorType::BitOr:
        return 50;
    case OperatorType::BitXor:
        return 55;
    case OperatorType::BitAnd:
        return 60;

    case OperatorType::Shl:
    case OperatorType::Shr:
        return 70;

    case OperatorType::Add:
    case OperatorType::Sub:
        return 80;

    case OperatorType::Mul:
    case OperatorType::Div:
    case OperatorType::Mod:
        return 90;

    case OperatorType::Inc:
    case OperatorType::Dec:
        return 100; // higher than * / %
    default:
        return 0;
    }
}

ASTNode Parser::ParseSingle() {
    auto tok = Next();

    if (tok.type == TokenType::Integer || tok.type == TokenType::Float || tok.type == TokenType::String) {
        return CreateNode<LiteralExpr>(ASTNodeKind::LiteralExpr, tok);
    }

    if (tok.type == TokenType::Identifier) {
        return CreateNode<VariableExpr>(ASTNodeKind::VariableExpr, IdentifierToken(tok.value));
    }

    if (
        tok.type == TokenType::OperatorAdd
        || tok.type == TokenType::OperatorSub
        || tok.type == TokenType::OperatorNot
        || tok.type == TokenType::OperatorBitNot
    ) {
        return CreateNode<UnaryExpr>(
            ASTNodeKind::UnaryExpr, tok.op(), ParseSingle()
        );
    }

    if (tok.type == TokenType::OperatorInc || tok.type == TokenType::OperatorDec) {
        auto in = ParseSingle();

        if (in.type != ASTNodeKind::PropertyExpr) {
            ThrowError("Can only apply ++/-- to variables", in);
        }

        auto type = (tok.type == TokenType::OperatorInc)
                        ? VariableIncDecExpr::Type::IncLeft
                        : VariableIncDecExpr::Type::DecLeft;

        return CreateNode<VariableIncDecExpr>(
            ASTNodeKind::VariableIncDecExpr, type, in
        );
    }

    if (tok.type == TokenType::SymbolLeftPar) {
        std::vector<ASTNode> elements;

        if (Peek().type != TokenType::SymbolRightPar) {
            while (true) {
                auto out = ParseExpr();

                if (Peek().type == TokenType::SymbolComma) {
                    current++;
                } else if (elements.empty()) return out;
                else {
                    elements.emplace_back(out);
                    break;
                }

                elements.emplace_back(out);
            }
        }

        Expect(TokenType::SymbolRightPar);

        return CreateNode<TupleExpr>(
            ASTNodeKind::TupleExpr, tok, std::move(elements)
        );
    }

    if (tok.type == TokenType::StringStart) {
        auto out = CreateNode<FStringExpr>(
            ASTNodeKind::FStringExpr, tok.comp<TokenType::StringStart>(),
            std::vector<ASTNode>(), std::vector<StringMiddleToken>(), StringEndToken()
        );

        while (true) {
            if (Peek().type == TokenType::StringEnd) {
                auto endTok = Next();
                out.as<FStringExpr>().end = endTok.comp<TokenType::StringEnd>();
                return out;
            }

            if (Peek().type == TokenType::StringMiddle) {
                auto midTok = Next();
                out.as<FStringExpr>().middles.push_back(midTok.comp<TokenType::StringMiddle>());
                continue;
            }

            out.as<FStringExpr>().expressions.push_back(ParseExpr());

            if (Over()) {
                ThrowError("Unterminated f-string", tok);
            }
        }
    }

    ThrowError("Unexpected token", tok);
}

ASTNode Parser::ParseExpr(int rbp, bool endAtGt) {
    auto out = ParseSingle();

    while (!Over()) {
        auto t = Peek();
        if (endAtGt && t.type == TokenType::OperatorGt || t.type == TokenType::OperatorGte) break;

        if (t.type == TokenType::SymbolColon) {
            current++;
            auto end = ParseSingle();
            auto t2 = Peek();
            if (t2.type == TokenType::SymbolColon) {
                current++;
                auto step = ParseSingle();
                out = CreateNode<RangeExpr>(ASTNodeKind::RangeExpr, out, end, step);
            } else {
                out = CreateNode<RangeExpr>(ASTNodeKind::RangeExpr, out, end, noneNode);
            }
            continue;
        }

        if (t.type == TokenType::SymbolDot) {
            current++;
            auto idTok = Next();
            if (idTok.type != TokenType::Identifier) {
                ThrowError("Expected identifier after '.'", idTok);
            }
            out = CreateNode<PropertyExpr>(ASTNodeKind::PropertyExpr, out, idTok.comp<TokenType::Identifier>());
            continue;
        }

        bool foundTypeArgs = false;

        if (
            (out.type == ASTNodeKind::VariableExpr || out.type == ASTNodeKind::PropertyExpr)
            && t.type == TokenType::OperatorLt
        ) {
            for (size_t i = current + 1; i < cur.tokens.size() - 1; i++) {
                auto curTok = cur.tokens[i];
                if (curTok.type == TokenType::OperatorGte) break;
                if (curTok.type == TokenType::OperatorGt) {
                    if (cur.tokens[i + 1].type != TokenType::SymbolLeftPar) {
                        ThrowError("Expected function call after type arguments", curTok);
                    }
                    foundTypeArgs = true;
                    break;
                }
            }

            if (foundTypeArgs) {
                out = CreateNode<CallExpr>(ASTNodeKind::CallExpr, out, std::vector<ASTNode>(), std::vector<ASTNode>());
                current++;
                ParseTypeGenerics(out.as<CallExpr>().typeArgs);
                Expect(TokenType::OperatorGt);
                t = Peek();
            }
        }

        if (t.type == TokenType::SymbolLeftPar) {
            current++;
            if (!foundTypeArgs) {
                out = CreateNode<CallExpr>(ASTNodeKind::CallExpr, out, std::vector<ASTNode>(), std::vector<ASTNode>());
            }

            if (Peek().type != TokenType::SymbolRightPar) {
                ParseExpressions(out.as<CallExpr>().args, endAtGt);
            }
            Expect(TokenType::SymbolRightPar);
            continue;
        }

        if (t.type == TokenType::SymbolLeftBracket) {
            current++;
            auto index = ParseExpr();
            Expect(TokenType::SymbolRightBracket);
            out = CreateNode<IndexExpr>(ASTNodeKind::IndexExpr, out, index);
            continue;
        }

        int lbp{};
        OperatorType op{};

        if (!t.is_operator() || rbp >= (lbp = LeftBindingPower(op = t.op_type()))) return out;
        current++;

        switch (op) {
        case OperatorType::Add:
        case OperatorType::Sub:
        case OperatorType::Mul:
        case OperatorType::Div:
        case OperatorType::Mod:
        case OperatorType::Lt:
        case OperatorType::Gt:
        case OperatorType::Lte:
        case OperatorType::Gte:
        case OperatorType::Eq:
        case OperatorType::Neq:
        case OperatorType::And:
        case OperatorType::Or:
        case OperatorType::BitAnd:
        case OperatorType::BitOr:
        case OperatorType::BitXor:
        case OperatorType::Shl:
        case OperatorType::Shr: {
            out = CreateNode<BinaryExpr>(ASTNodeKind::BinaryExpr, out, t.op(), ParseExpr(lbp));
        }
        break;
        case OperatorType::Assign:
        case OperatorType::SetAdd:
        case OperatorType::SetSub:
        case OperatorType::SetMul:
        case OperatorType::SetDiv:
        case OperatorType::SetMod:
        case OperatorType::SetBitAnd:
        case OperatorType::SetBitOr:
        case OperatorType::SetBitXor:
        case OperatorType::SetShl:
        case OperatorType::SetShr: {
            out = CreateNode<BinaryExpr>(ASTNodeKind::BinaryExpr, out, t.op(), ParseExpr(lbp - 1));
        }
        break;
        case OperatorType::Inc:
        case OperatorType::Dec: {
            auto type = (op == OperatorType::Inc)
                            ? VariableIncDecExpr::Type::IncRight
                            : VariableIncDecExpr::Type::DecRight;

            out = CreateNode<VariableIncDecExpr>(
                ASTNodeKind::VariableIncDecExpr, type, out
            );
        }
        break;
        case OperatorType::Not:
            out = CreateNode<BinaryExpr>(ASTNodeKind::BinaryExpr, out, t.op(), noneNode);
            break;
        case OperatorType::BitNot:
            ThrowError("Bitwise NOT (~) cannot be used as a suffix/binary operator", t);
        }
    }

    return out;
}

void Parser::ParseExpressions(std::vector<ASTNode>& result, bool endAtGt) {
    while (true) {
        auto expr = ParseExpr(endAtGt);
        result.push_back(expr);

        if (Peek().type == TokenType::SymbolComma) {
            current++;
        } else break;
    }
}

ASTNode Parser::ParseIdentifierTypeExpr() {
    Token idTok = Next();
    auto typeExpr = CreateNode<IdentifierTypeExpr>(
        ASTNodeKind::IdentifierTypeExpr, IdentifierToken{idTok.value}, std::vector<ASTNode>()
    );

    if (Peek().type == TokenType::OperatorLt) {
        current++;
        while (true) {
            auto arg = ParseTypeExpr();
            typeExpr.as<IdentifierTypeExpr>().arguments.push_back(arg);

            if (Peek().type == TokenType::SymbolComma) {
                current++;
            } else break;
        }
        Expect(TokenType::OperatorGt);
    }

    while (Peek().type == TokenType::SymbolLeftBracket) {
        if (Peek().type == TokenType::SymbolQuestion) {
            current++;
            typeExpr = CreateNode<OptionalTypeExpr>(ASTNodeKind::OptionalTypeExpr, typeExpr);
        }
        current++;
        ASTNode size;
        if (Peek().type != TokenType::SymbolRightBracket) {
            size = ParseExpr();
        }
        Expect(TokenType::SymbolRightBracket);
        typeExpr = CreateNode<ArrayTypeExpr>(
            ASTNodeKind::ArrayTypeExpr, typeExpr, size
        );
    }

    if (Peek().type == TokenType::SymbolQuestion) {
        current++;
        typeExpr = CreateNode<OptionalTypeExpr>(ASTNodeKind::OptionalTypeExpr, typeExpr);
    }

    return typeExpr;
}

ASTNode Parser::ParseFunctionTypeExpr() {
    Expect(TokenType::SymbolLeftPar);

    std::vector<TypeParameter*> params;
    if (Peek().type != TokenType::SymbolRightPar) {
        while (true) {
            auto p = arena.make<TypeParameter>();
            p->ref = false;

            if (Peek().type == TokenType::OperatorBitAnd) {
                current++;
                p->ref = true;
            }

            p->type = ParseTypeExpr();

            params.push_back(p);

            if (Peek().type == TokenType::SymbolComma) {
                current++;
            } else break;
        }
    }

    Expect(TokenType::SymbolRightPar);
    Expect(TokenType::SymbolArrow);

    auto returnType = ParseTypeExpr();

    return CreateNode<FunctionTypeExpr>(ASTNodeKind::FunctionTypeExpr, std::move(params), returnType);
}

ASTNode Parser::ParseTypeExpr() {
    Token tok = Peek();

    if (tok.type == TokenType::Identifier) {
        return ParseIdentifierTypeExpr();
    }

    if (tok.type == TokenType::SymbolLeftPar) {
        return ParseFunctionTypeExpr();
    }

    return ParseExpr(0, true);
}

void Parser::ParseTypeGenerics(std::vector<ASTNode>& result) {
    while (true) {
        auto type = ParseTypeExpr();
        result.push_back(type);

        if (Peek().type == TokenType::SymbolComma) {
            current++;
        } else break;
    }
}
