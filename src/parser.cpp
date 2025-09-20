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

        if (in.type != ASTNodeKind::VariableExpr && in.type != ASTNodeKind::PropertyExpr) {
            ThrowError("Can only apply ++/-- to variables", in);
        }

        auto type = (tok.type == TokenType::OperatorInc)
                        ? VariableIncDecExpr::Type::IncLeft
                        : VariableIncDecExpr::Type::DecLeft;

        return CreateNode<VariableIncDecExpr>(
            ASTNodeKind::VariableIncDecExpr, type, in
        );
    }

    if (tok.type == TokenType::SymbolLeftParenthesis) {
        size_t parEnd = 0;
        size_t parAmount = 0;
        // finding the matching right parenthesis
        for (auto i = current; i < cur.tokens.size(); i++) {
            auto curTok = cur.tokens[i];
            if (curTok.type == TokenType::SymbolLeftParenthesis) {
                parAmount++;
            } else if (curTok.type == TokenType::SymbolRightParenthesis) {
                if (parAmount == 0) {
                    parEnd = i;
                    break;
                }
                parAmount--;
            }
        }

        if (parEnd == 0) {
            ThrowError("Unclosed parenthesis", tok);
        }

        auto parEndTok = Get(parEnd + 1);
        bool lambdaSure = parEndTok.type == TokenType::SymbolArrow;

        if (parEndTok.type == TokenType::SymbolColon) {
            parAmount = 0;
            for (auto i = parEnd + 2; i < cur.tokens.size(); i++) {
                auto curTok = cur.tokens[i];
                if (
                    curTok.type == TokenType::SymbolLeftParenthesis
                    || curTok.type == TokenType::SymbolLeftBracket
                    || curTok.type == TokenType::SymbolLeftBrace
                ) {
                    parAmount++;
                } else if (
                    curTok.type == TokenType::SymbolRightBracket
                    || curTok.type == TokenType::SymbolRightBrace
                    || curTok.type == TokenType::SymbolRightParenthesis
                ) {
                    if (parAmount == 0) {
                        break;
                    }
                    parAmount--;
                } else if (curTok.type == TokenType::SymbolArrow && parAmount == 0) {
                    lambdaSure = true;
                    break;
                }
            }
        }

        if (lambdaSure) {
            auto out = CreateNode<LambdaExpr>(
                ASTNodeKind::LambdaExpr, std::vector<Parameter>(), noneNode, std::vector<ASTNode>()
            );

            if (Peek().type != TokenType::SymbolRightParenthesis) {
                while (true) {
                    auto tok2 = Peek();
                    if (tok2.type != TokenType::Identifier) {
                        ThrowError("Expected a parameter name", tok2);
                    }
                    auto type = noneNode;
                    auto default_ = noneNode;
                    current++;
                    if (Peek().type == TokenType::SymbolColon) {
                        current++;
                        type = ParseTypeExpr();
                    }
                    if (Peek().type == TokenType::OperatorAssign) {
                        current++;
                        default_ = ParseExpr(0, false, true);
                    }
                    out.as<LambdaExpr>().params.emplace_back(tok2.comp<TokenType::Identifier>(), type, default_);
                    if (Peek().type == TokenType::SymbolComma) {
                        current++;
                    } else {
                        break;
                    }
                }
            }

            Expect(TokenType::SymbolRightParenthesis);

            if (parEndTok.type == TokenType::SymbolColon) {
                out.as<LambdaExpr>().returns = ParseTypeExpr();
            }

            Expect(TokenType::SymbolArrow);

            ParseBracedBlock(out.as<LambdaExpr>().body);

            return out;
        }

        std::vector<ASTNode> elements;

        if (Peek().type != TokenType::SymbolRightParenthesis) {
            while (true) {
                auto out = ParseExpr(0, false, true);

                if (Peek().type == TokenType::SymbolComma) {
                    current++;
                } else if (elements.empty()) {
                    Expect(TokenType::SymbolRightParenthesis);
                    return out;
                } else {
                    elements.emplace_back(out);
                    break;
                }

                elements.emplace_back(out);
            }
        }

        Expect(TokenType::SymbolRightParenthesis);

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
            SkipNewLines();
            auto tok2 = Peek();

            if (tok2.type == TokenType::StringEnd) {
                auto endTok = Next();
                out.as<FStringExpr>().end = endTok.comp<TokenType::StringEnd>();
                return out;
            }

            if (tok2.type == TokenType::StringMiddle) {
                auto midTok = Next();
                out.as<FStringExpr>().middles.push_back(midTok.comp<TokenType::StringMiddle>());
                continue;
            }

            out.as<FStringExpr>().expressions.push_back(ParseExpr(0, false, true));

            if (Over()) {
                ThrowError("Unterminated f-string", tok);
            }
        }
    }

    if (tok.type == TokenType::KeywordIf) {
        Expect(TokenType::SymbolLeftParenthesis);

        auto out = CreateNode<IfExpr>(
            ASTNodeKind::IfExpr, tok,
            ParseExpr(0, false, true), std::vector<ASTNode>(), std::vector<ASTNode>()
        );

        Expect(TokenType::SymbolRightParenthesis);

        ParseBracedBlock(out.as<IfExpr>().body);

        if (Peek().type == TokenType::KeywordElse) {
            current++;
            ParseBracedBlock(out.as<IfExpr>().elseBody);
        }

        return out;
    }

    if (tok.type == TokenType::SymbolLeftBrace) {
        auto out = CreateNode<ScopeExpr>(ASTNodeKind::ScopeExpr, std::vector<ASTNode>());
        current--;
        ParseBracedBlock(out.as<ScopeExpr>().body);
        return out;
    }

    ThrowError("Unexpected token", tok);
}

ASTNode Parser::ParseExpr(int rbp, bool endAtGt, bool ignoreNewLines) {
    if (ignoreNewLines) SkipNewLines();
    auto out = ParseSingle();

    while (!Over()) {
        auto tok = Peek();
        if (tok.type == TokenType::NewLine && ignoreNewLines) {
            current++;
            continue;
        }
        if (endAtGt && tok.type == TokenType::OperatorGt || tok.type == TokenType::OperatorGte) break;

        if (tok.type == TokenType::NewLine) {
            // Skip new lines where something like this happens:
            // obj
            // .property
            for (auto i = current + 1; i < cur.tokens.size(); i++) {
                auto tok2 = cur.tokens[i];
                if (tok2.type == TokenType::NewLine) continue;
                if (tok2.type == TokenType::SymbolDot) {
                    current = i;
                    tok = tok2;
                }
                break;
            }
        }

        if (tok.type == TokenType::SymbolColon) {
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

        if (tok.type == TokenType::SymbolDot) {
            current++;
            SkipNewLines(); // after a dot we can have new lines
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
            && tok.type == TokenType::OperatorLt
        ) {
            for (auto i = current + 1; i < cur.tokens.size() - 1; i++) {
                auto curTok = cur.tokens[i];
                if (curTok.type == TokenType::OperatorGte) break;
                if (curTok.type == TokenType::OperatorGt) {
                    if (cur.tokens[i + 1].type != TokenType::SymbolLeftParenthesis) {
                        ThrowError("Expected function call after type arguments", curTok);
                    }
                    foundTypeArgs = true;
                    break;
                }
            }

            if (foundTypeArgs) {
                out = CreateNode<CallExpr>(ASTNodeKind::CallExpr, out, std::vector<CallArgument>(),
                                           std::vector<ASTNode>());
                current++;
                ParseTypeExpressions(out.as<CallExpr>().typeArgs, true);
                Expect(TokenType::OperatorGt);
                SkipNewLines();
                tok = Peek();
            }
        }

        if (tok.type == TokenType::SymbolLeftParenthesis) {
            current++;
            if (!foundTypeArgs) {
                out = CreateNode<CallExpr>(ASTNodeKind::CallExpr, out, std::vector<CallArgument>(),
                                           std::vector<ASTNode>());
            }

            SkipNewLines();
            if (Peek().type != TokenType::SymbolRightParenthesis) {
                while (true) {
                    if (Peek().type == TokenType::NewLine) {
                        current++;
                        continue;
                    }
                    if (Peek().type == TokenType::Identifier && PeekNoLine(1).type == TokenType::SymbolColon) {
                        auto nameTok = Next();
                        SkipNewLines();
                        Expect(TokenType::SymbolColon);
                        auto expr = ParseExpr(0, false, true);
                        out.as<CallExpr>().args.emplace_back(false, nameTok.comp<TokenType::Identifier>(), expr);
                    } else if (Peek().type == TokenType::SymbolEllipsis) {
                        current++;
                        auto expr = ParseExpr(0, false, true);
                        out.as<CallExpr>().args.emplace_back(true, std::nullopt, expr);
                    } else {
                        auto expr = ParseExpr(0, false, true);
                        out.as<CallExpr>().args.emplace_back(false, std::nullopt, expr);
                    }

                    if (Peek().type == TokenType::SymbolComma) {
                        current++;
                    } else break;
                }
            }
            SkipNewLines();
            Expect(TokenType::SymbolRightParenthesis);
            continue;
        }

        if (tok.type == TokenType::SymbolLeftBracket) {
            current++;
            SkipNewLines();
            auto index = ParseExpr(0, false, true);
            SkipNewLines();
            Expect(TokenType::SymbolRightBracket);
            out = CreateNode<IndexExpr>(ASTNodeKind::IndexExpr, out, index);
            continue;
        }

        int lbp{};
        OperatorType op{};

        if (!tok.is_operator() || rbp >= (lbp = LeftBindingPower(op = tok.op_type()))) return out;
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
            SkipNewLines(); // after an operator we can have new lines
            out = CreateNode<BinaryExpr>(ASTNodeKind::BinaryExpr, out, tok.op(),
                                         ParseExpr(lbp, endAtGt, ignoreNewLines));
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
            SkipNewLines(); // after an operator we can have new lines
            out = CreateNode<BinaryExpr>(ASTNodeKind::BinaryExpr, out, tok.op(),
                                         ParseExpr(lbp - 1, endAtGt, ignoreNewLines));
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
            out = CreateNode<BinaryExpr>(ASTNodeKind::BinaryExpr, out, tok.op(), noneNode);
            break;
        case OperatorType::BitNot:
            ThrowError("Bitwise NOT (~) cannot be used as a suffix/binary operator", tok);
        }
    }

    return out;
}

void Parser::ParseExpressions(std::vector<ASTNode>& result, bool endAtGt, bool ignoreNewLines) {
    while (true) {
        auto expr = ParseExpr(0, endAtGt, ignoreNewLines);
        result.push_back(expr);

        if (Peek().type == TokenType::SymbolComma) {
            current++;
        } else break;
    }
}

ASTNode Parser::ParseSingleTypeExpr(bool allowExpr) {
    auto tok = Next();
    if (tok.type == TokenType::OperatorBitAnd) {
        return CreateNode<ReferenceTypeExpr>(ASTNodeKind::ReferenceTypeExpr, ParseTypeExpr());
    }

    if (tok.type == TokenType::Identifier) {
        auto out = CreateNode<IdentifierTypeExpr>(
            ASTNodeKind::IdentifierTypeExpr, IdentifierToken{tok.value}, std::vector<ASTNode>()
        );
        if (Peek().type == TokenType::OperatorLt) {
            current++;
            ParseTypeExpressions(out.as<IdentifierTypeExpr>().arguments, true);
            auto n = Peek();
            if (n.type != TokenType::OperatorGt && n.type != TokenType::OperatorGte) {
                ThrowError("Expected '>' to close generic type arguments", n);
            }
            if (n.type == TokenType::OperatorGt) {
                current++;
            }
        }
        return out;
    }

    if (tok.type == TokenType::SymbolLeftParenthesis) {
        size_t parAmount = 0;
        bool lambdaSure = false;
        for (auto i = current + 1; i < cur.tokens.size(); i++) {
            auto curTok = cur.tokens[i];
            if (curTok.type == TokenType::SymbolLeftParenthesis) {
                parAmount++;
            } else if (curTok.type == TokenType::SymbolRightParenthesis) {
                if (parAmount == 0) {
                    lambdaSure = Get(i + 1).type == TokenType::SymbolArrow;
                    break;
                }
                parAmount--;
            }
        }

        if (lambdaSure) {
            Expect(TokenType::SymbolLeftParenthesis);

            std::vector<ASTNode> params;
            if (Peek().type != TokenType::SymbolRightParenthesis) {
                ParseTypeExpressions(params);
            }

            Expect(TokenType::SymbolRightParenthesis);
            SkipNewLines();
            Expect(TokenType::SymbolArrow);
            SkipNewLines();

            auto returnType = ParseTypeExpr();

            return CreateNode<FunctionTypeExpr>(ASTNodeKind::FunctionTypeExpr, std::move(params), returnType);
        }
        current++;
        SkipNewLines();
        auto type = ParseTypeExpr();
        SkipNewLines();
        Expect(TokenType::SymbolRightParenthesis);
        SkipNewLines();
        return type;
    }

    ThrowError("Expected a type", tok);
}

ASTNode Parser::ParseTypeExpr(bool allowExpr) {
    auto out = ParseSingleTypeExpr(allowExpr);

    while (true) {
        auto tok = Peek();
        if (tok.type == TokenType::SymbolLeftBracket) {
            current++;
            SkipNewLines();
            if (Peek().type == TokenType::SymbolRightBracket) {
                current++;
                out = CreateNode<ArrayTypeExpr>(ASTNodeKind::ArrayTypeExpr, out, noneNode);
                continue;
            }
            auto size = ParseExpr(0, false, true);
            SkipNewLines();
            Expect(TokenType::SymbolRightBracket);
            out = CreateNode<ArrayTypeExpr>(ASTNodeKind::ArrayTypeExpr, out, size);
            continue;
        }

        break;
    }

    return out;
}

void Parser::ParseTypeExpressions(std::vector<ASTNode>& result, bool allowExpr) {
    SkipNewLines();
    while (true) {
        auto type = ParseTypeExpr(allowExpr);
        SkipNewLines();
        result.push_back(type);

        if (Peek().type == TokenType::SymbolComma) {
            current++;
        } else break;
    }
}

// despite the name it might return VariableExpr
ASTNode Parser::ParsePropertyExpr() {
    auto out = ParseSingle();

    if (out.type != ASTNodeKind::VariableExpr) {
        ThrowError("Expected variable name", out);
    }

    if (Peek().type != TokenType::SymbolDot) {
        return out;
    }

    while (Peek().type == TokenType::SymbolDot) {
        current++;
        auto idTok = Next();
        if (idTok.type != TokenType::Identifier) {
            ThrowError("Expected identifier after '.'", idTok);
        }
        out = CreateNode<PropertyExpr>(ASTNodeKind::PropertyExpr, out, idTok.comp<TokenType::Identifier>());
    }

    return out;
}

ASTNode Parser::ParseStmt() {
    auto tok = Next();

    if (tok.type == TokenType::KeywordReturn) {
        auto value = ParseExpr();
        return CreateNode<ReturnStmt>(ASTNodeKind::ReturnStmt, value);
    }

    if (tok.type == TokenType::KeywordBreak) {
        return CreateNode<BreakStmt>(ASTNodeKind::BreakStmt, tok);
    }

    if (tok.type == TokenType::KeywordContinue) {
        return CreateNode<ContinueStmt>(ASTNodeKind::ContinueStmt, tok);
    }

    if (tok.type == TokenType::KeywordVal || tok.type == TokenType::KeywordVar) {
        SkipNewLines();
        auto name = ParsePropertyExpr();
        SkipNewLines();
        auto type = noneNode;
        auto typed = Peek().type == TokenType::SymbolColon;
        if (typed) {
            current++;
            type = ParseTypeExpr();
            SkipNewLines();
        }
        auto n = Next();
        if (n.type != TokenType::OperatorAssign && (!typed || n.type != TokenType::OperatorGte)) {
            ThrowError("Expected '=' after variable name", n);
        }
        SkipNewLines();
        auto value = ParseExpr();
        return CreateNode<VariableDefineStmt>(
            ASTNodeKind::VariableDefineStmt, tok.type == TokenType::KeywordVal, name, type, value
        );
    }

    if (tok.type == TokenType::KeywordFor) {
        SkipNewLines();
        Expect(TokenType::SymbolLeftParenthesis);
        SkipNewLines();
        auto out = CreateNode<ForStmt>(
            ASTNodeKind::ForStmt,
            std::vector<IdentifierToken>(), noneNode, std::vector<ASTNode>(), std::vector<ASTNode>()
        );
        while (true) {
            auto idTok = Next();
            if (idTok.type != TokenType::Identifier) {
                ThrowError("Expected identifier in for loop", idTok);
            }

            SkipNewLines();
            out.as<ForStmt>().identifiers.push_back(idTok.comp<TokenType::Identifier>());
            SkipNewLines();

            if (Peek().type == TokenType::SymbolComma) {
                current++;
            } else break;
        }
        SkipNewLines();
        Expect(TokenType::KeywordIn);
        SkipNewLines();
        out.as<ForStmt>().iterable = ParseExpr();
        SkipNewLines();
        Expect(TokenType::SymbolRightParenthesis);
        SkipNewLines();
        ParseBracedBlock(out.as<ForStmt>().body);
        SkipNewLines();
        if (Peek().type == TokenType::KeywordElse) {
            current++;
            SkipNewLines();
            ParseBracedBlock(out.as<ForStmt>().elseBody);
        }
        return out;
    }

    if (tok.type == TokenType::KeywordWhile) {
        SkipNewLines();
        Expect(TokenType::SymbolLeftParenthesis);
        auto out = CreateNode<WhileStmt>(
            ASTNodeKind::WhileStmt,
            ParseExpr(0, false, true), std::vector<ASTNode>(), std::vector<ASTNode>()
        );
        SkipNewLines();
        Expect(TokenType::SymbolRightParenthesis);
        SkipNewLines();
        ParseBracedBlock(out.as<WhileStmt>().body);
        SkipNewLines();
        if (Peek().type == TokenType::KeywordElse) {
            current++;
            SkipNewLines();
            ParseBracedBlock(out.as<WhileStmt>().elseBody);
        }
        return out;
    }

    if (tok.type == TokenType::KeywordDo) {
        auto out = CreateNode<DoWhileStmt>(
            ASTNodeKind::DoWhileStmt,
            std::vector<ASTNode>(), noneNode, std::vector<ASTNode>()
        );
        SkipNewLines();
        ParseBracedBlock(out.as<DoWhileStmt>().body);
        SkipNewLines();
        Expect(TokenType::KeywordWhile);
        SkipNewLines();
        Expect(TokenType::SymbolLeftParenthesis);
        out.as<DoWhileStmt>().condition = ParseExpr(0, false, true);
        SkipNewLines();
        Expect(TokenType::SymbolRightParenthesis);
        SkipNewLines();

        if (Peek().type == TokenType::KeywordElse) {
            current++;
            SkipNewLines();
            ParseBracedBlock(out.as<DoWhileStmt>().elseBody);
        }
        return out;
    }

    if (tok.type == TokenType::KeywordAlias) {
        SkipNewLines();
        auto idTok = Next();
        if (idTok.type != TokenType::Identifier) {
            ThrowError("Expected identifier after 'alias'", idTok);
        }
        SkipNewLines();
        Expect(TokenType::OperatorAssign);
        SkipNewLines();
        auto type = ParseTypeExpr();
        return CreateNode<AliasStmt>(ASTNodeKind::AliasStmt, idTok.comp<TokenType::Identifier>(), type);
    }

    if (tok.type == TokenType::KeywordImport) {
        SkipNewLines();
        auto id = ParsePropertyExpr();
        if (Peek().type != TokenType::KeywordAs) {
            return CreateNode<ImportStmt>(ASTNodeKind::ImportStmt, false, id);
        }
        current++;
        auto aliasTok = Next();
        if (aliasTok.type == TokenType::OperatorMul) {
            return CreateNode<ImportStmt>(ASTNodeKind::ImportStmt, true, id);
        }

        if (aliasTok.type != TokenType::Identifier) {
            ThrowError("Expected identifier after 'as'", aliasTok);
        }

        return CreateNode<ImportStmt>(ASTNodeKind::ImportStmt, false, id, aliasTok.comp<TokenType::Identifier>());
    }

    current--;
    return CreateNode<ExpressionStmt>(ASTNodeKind::ExpressionStmt, ParseExpr());
};

void Parser::ParseBracedBlock(std::vector<ASTNode>& result) {
    if (Peek().type == TokenType::SymbolLeftBrace) {
        current++;
        SkipNewLines();
        if (Peek().type != TokenType::SymbolRightBrace) {
            ParseStatements(result);
        }
        Expect(TokenType::SymbolRightBrace);
    } else {
        result.push_back(ParseStmt());
    }
}

void Parser::ParseStatements(std::vector<ASTNode>& result) {
    while (!Over()) {
        auto tok = Peek();
        if (tok.type == TokenType::SymbolRightBrace) break;
        if (tok.type == TokenType::NewLine) {
            current++;
            continue;
        }
        auto stmt = ParseStmt();
        result.push_back(stmt);
    }
};
