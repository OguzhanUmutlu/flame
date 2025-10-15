#include "parser.hpp"

#include "analyzer.hpp"
#include "analyzer.hpp"
#include "analyzer.hpp"
#include "analyzer.hpp"

using namespace Flame;

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

ASTNode* Parser::ParseSingle() {
    auto tok = Next();

    if (tok.IsLiteral()) {
        return CreateNode<LiteralExpr>(tok);
    }

    if (tok.type == TokenType::Identifier) {
        return CreateNode<VariableExpr>(IdentifierToken(tok.value));
    }

    if (
        tok.type == TokenType::OperatorAdd
        || tok.type == TokenType::OperatorSub
        || tok.type == TokenType::OperatorNot
        || tok.type == TokenType::OperatorBitNot
    ) {
        return CreateNode<UnaryExpr>(tok.Op(), ParseSingle());
    }

    if (tok.type == TokenType::OperatorInc || tok.type == TokenType::OperatorDec) {
        auto in = ParseSingle();

        if (!in->Is<VariableExpr>() && !in->Is<PropertyExpr>()) {
            ThrowError("Can only apply ++/-- to variables", in);
        }

        auto type = (tok.type == TokenType::OperatorInc)
                        ? VariableIncDecExpr::Type::IncLeft
                        : VariableIncDecExpr::Type::DecLeft;

        return CreateNode<VariableIncDecExpr>(type, in);
    }

    if (tok.type == TokenType::SymbolLeftParenthesis) {
        size_t parEnd = 0;
        size_t parAmount = 0;
        // finding the matching right parenthesis
        for (auto i = current; i < TokenCount(); i++) {
            auto curTok = Get(i);
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
            for (auto i = parEnd + 2; i < TokenCount(); i++) {
                auto curTok = Get(i);
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
            auto out = CreateNode<LambdaExpr>();

            if (!PeekIs(TokenType::SymbolRightParenthesis)) {
                while (true) {
                    auto tok2 = Peek();
                    if (tok2.type != TokenType::Identifier) {
                        ThrowError("Expected a parameter name", tok2);
                    }
                    ASTNode* type{};
                    ASTNode* default_{};
                    current++;
                    if (PeekIs(TokenType::SymbolColon)) {
                        current++;
                        type = ParseTypeExpr();
                    }
                    if (PeekIs(TokenType::OperatorAssign)) {
                        current++;
                        default_ = ParseExpr(0, true);
                    }
                    out->params.emplace_back(tok2.comp<TokenType::Identifier>(), type, default_);
                    if (PeekIs(TokenType::SymbolComma)) {
                        current++;
                    } else {
                        break;
                    }
                }

                out->params.shrink_to_fit();
            }

            Expect(TokenType::SymbolRightParenthesis);

            if (parEndTok.type == TokenType::SymbolColon) {
                out->returns = ParseTypeExpr();
            }

            Expect(TokenType::SymbolArrow);

            ParseBracedBlock(out->body);

            return out;
        }

        std::vector<ASTNode*> elements;

        if (!PeekIs(TokenType::SymbolRightParenthesis)) {
            while (true) {
                auto out = ParseExpr(0, true);

                if (PeekIs(TokenType::SymbolComma)) {
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

            elements.shrink_to_fit();
        }

        Expect(TokenType::SymbolRightParenthesis);

        return CreateNode<TupleExpr>(tok.comp<TokenType::SymbolLeftParenthesis>(), std::move(elements));
    }

    if (tok.type == TokenType::StringStart) {
        auto out = CreateNode<FStringExpr>(tok.comp<TokenType::StringStart>());

        while (true) {
            SkipNewLines();
            auto tok2 = Peek();

            if (tok2.type == TokenType::StringEnd) {
                auto endTok = Next();
                out->end = endTok.comp<TokenType::StringEnd>();
                return out;
            }

            if (tok2.type == TokenType::StringMiddle) {
                auto midTok = Next();
                out->middles.push_back(midTok.comp<TokenType::StringMiddle>());
                continue;
            }

            out->expressions.push_back(ParseExpr(0, true));

            if (Over()) {
                ThrowError("Unterminated f-string", tok);
            }
        }

        out->middles.shrink_to_fit();
        out->expressions.shrink_to_fit();
    }

    if (tok.type == TokenType::KeywordIf) {
        Expect(TokenType::SymbolLeftParenthesis);

        auto out = CreateNode<IfExpr>(tok.comp<TokenType::KeywordIf>(), ParseExpr(0, true));

        Expect(TokenType::SymbolRightParenthesis);

        ParseBracedBlock(out->body);

        if (PeekIs(TokenType::KeywordElse)) {
            current++;
            ParseBracedBlock(out->elseBody);
        }

        return out;
    }

    if (tok.type == TokenType::SymbolLeftBrace) {
        auto out = CreateNode<ScopeExpr>();
        current--;
        ParseBracedBlock(out->body);
        return out;
    }

    ThrowError("Unexpected token", tok);
}

ASTNode* Parser::ParseExpr(int rbp, bool ignoreNewLines, bool endAtSet, bool endAtGt, bool allowType) {
    if (ignoreNewLines) SkipNewLines();
    auto out = ParseSingle();

    while (!Over()) {
        auto tok = Peek();
        if (ignoreNewLines && tok.type == TokenType::NewLine) {
            current++;
            continue;
        }

        if (endAtSet && tok.type == TokenType::OperatorAssign) break;
        if (endAtGt && (tok.type == TokenType::OperatorGt || tok.type == TokenType::OperatorGte)) break;

        if (tok.type == TokenType::NewLine) {
            // Skip new lines where something like this happens:
            // obj
            // .property
            for (auto i = current + 1; i < TokenCount(); i++) {
                auto tok2 = Get(i);
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
                out = CreateNode<RangeExpr>(out, end, step);
            } else {
                out = CreateNode<RangeExpr>(out, end);
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
            out = CreateNode<PropertyExpr>(out, idTok.comp<TokenType::Identifier>());
            continue;
        }


        if (
            (out->Is<VariableExpr>() || out->Is<PropertyExpr>())
            && tok.type == TokenType::OperatorLt
        ) {
            bool foundTypeArgs = false;
            for (auto i = current + 1; i < TokenCount(); i++) {
                auto tok2 = Get(i);
                if (tok2.type == TokenType::OperatorAssign && endAtSet) break;
                if (tok2.type == TokenType::OperatorGte) {
                    foundTypeArgs = endAtSet;
                    break;
                }
                if (tok2.type == TokenType::OperatorGt) {
                    auto next = Get(i + 1);
                    if (
                        !allowType
                        && next.type != TokenType::SymbolLeftParenthesis
                        && next.type != TokenType::SymbolLeftBracket
                        && next.type != TokenType::OperatorAssign
                    ) {
                        ThrowError(
                            "Expected function call, array type notation or variable definition right after type arguments",
                            tok2);
                    }
                    foundTypeArgs = true;
                    break;
                }
            }

            if (foundTypeArgs) {
                auto gen = CreateNode<GenericTypeExpr>(out);
                out = gen;
                current++;
                ParseTypeExpressions(gen->arguments);
                auto p = Next();
                if (p.type != TokenType::OperatorGt && p.type != TokenType::OperatorGte) {
                    ThrowError("Expected '>' after type arguments", p);
                }
                if (p.type == TokenType::OperatorGte) {
                    current--;
                    break;
                }
                continue;
            }
        }

        if (tok.type == TokenType::SymbolLeftParenthesis) {
            current++;
            auto call = CreateNode<CallExpr>(out);
            out = call;

            SkipNewLines();
            if (!PeekIs(TokenType::SymbolRightParenthesis)) {
                while (true) {
                    if (PeekIs(TokenType::NewLine)) {
                        current++;
                        continue;
                    }
                    if (PeekIs(TokenType::Identifier) && PeekNoLine(1).type == TokenType::SymbolColon) {
                        auto nameTok = Next();
                        SkipNewLines();
                        Expect(TokenType::SymbolColon);
                        auto expr = ParseExpr(0, true);
                        call->args.emplace_back(false, nameTok.comp<TokenType::Identifier>(), expr);
                    } else if (PeekIs(TokenType::SymbolEllipsis)) {
                        current++;
                        auto expr = ParseExpr(0, true);
                        call->args.emplace_back(true, std::nullopt, expr);
                    } else {
                        auto expr = ParseExpr(0, true);
                        call->args.emplace_back(false, std::nullopt, expr);
                    }

                    if (PeekIs(TokenType::SymbolComma)) {
                        current++;
                    } else break;
                }

                call->args.shrink_to_fit();
            }
            SkipNewLines();
            Expect(TokenType::SymbolRightParenthesis);
            continue;
        }

        if (tok.type == TokenType::SymbolLeftBracket) {
            current++;
            SkipNewLines();
            if (PeekIs(TokenType::SymbolRightBracket)) {
                current++;
                out = CreateNode<IndexExpr>(out);
                continue;
            }
            auto index = ParseExpr(0, true);
            SkipNewLines();
            Expect(TokenType::SymbolRightBracket);
            out = CreateNode<IndexExpr>(out, index);
            continue;
        }

        int lbp{};
        OperatorType op{};

        if (!tok.IsOperator() || rbp >= (lbp = LeftBindingPower(op = tok.OpType()))) return out;
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
        case OperatorType::Shr:
        case OperatorType::MatMul: {
            SkipNewLines(); // after an operator we can have new lines
            out = CreateNode<BinaryExpr>(out, tok.Op(),
                                         ParseExpr(lbp, ignoreNewLines, endAtSet, endAtGt));
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
        case OperatorType::SetShr:
        case OperatorType::SetMatMul: {
            SkipNewLines(); // after an operator we can have new lines
            out = CreateNode<BinaryExpr>(out, tok.Op(),
                                         ParseExpr(lbp - 1, ignoreNewLines, endAtSet, endAtGt));
        }
        break;
        case OperatorType::Inc:
        case OperatorType::Dec: {
            auto type = (op == OperatorType::Inc)
                            ? VariableIncDecExpr::Type::IncRight
                            : VariableIncDecExpr::Type::DecRight;

            out = CreateNode<VariableIncDecExpr>(type, out);
        }
        break;
        case OperatorType::Not:
            out = CreateNode<BinaryExpr>(out, tok.Op());
            break;
        case OperatorType::BitNot:
            ThrowError("Bitwise NOT (~) cannot be used as a suffix/binary operator", tok);
        case OperatorType::None:
            ThrowError("How did we get here?", tok);
        }
    }

    return out;
}

void Parser::ParseExpressions(std::vector<ASTNode*>& result, bool ignoreNewLines) {
    while (true) {
        auto expr = ParseExpr(0, ignoreNewLines);
        result.push_back(expr);

        if (PeekIs(TokenType::SymbolComma)) {
            current++;
        } else break;
    }

    result.shrink_to_fit();
}

ASTNode* Parser::ParseTypeExpr(bool ignoreNewLines, bool endAtGt) {
    auto tok = Peek();

    if (tok.type == TokenType::OperatorBitAnd) {
        current++;
        return CreateNode<ReferenceTypeExpr>(ParseTypeExpr());
    }

    if (tok.type == TokenType::SymbolLeftParenthesis) {
        size_t parAmount = 0;
        bool lambdaSure = false;
        for (auto i = current + 1; i < TokenCount(); i++) {
            auto curTok = Get(i);
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
            current++;
            Expect(TokenType::SymbolLeftParenthesis);

            auto out = CreateNode<FunctionTypeExpr>();

            if (!PeekIs(TokenType::SymbolRightParenthesis)) {
                ParseTypeExpressions(out->params);
            }

            Expect(TokenType::SymbolRightParenthesis);
            SkipNewLines();
            Expect(TokenType::SymbolArrow);
            SkipNewLines();

            out->returns = ParseTypeExpr(true);

            return out;
        }
        current++;
        SkipNewLines();
        auto type = ParseExpr(0, true);
        SkipNewLines();
        Expect(TokenType::SymbolRightParenthesis);
        SkipNewLines();
        return type;
    }

    return ParseExpr(0, ignoreNewLines, true, endAtGt, true);
}

void Parser::ParseTypeExpressions(std::vector<ASTNode*>& result) {
    SkipNewLines();
    while (true) {
        auto type = ParseTypeExpr(true, true);
        SkipNewLines();
        result.push_back(type);

        if (PeekIs(TokenType::SymbolComma)) {
            current++;
        } else break;
    }

    result.shrink_to_fit();
}

void Parser::ParseGenerics(std::vector<Generic>& result) {
    SkipNewLines();
    while (true) {
        auto idTok = Next();
        if (idTok.type != TokenType::Identifier) {
            ThrowError("Expected generic name", idTok);
        }
        SkipNewLines();
        ASTNode* type = noneNode;
        if (PeekIs(TokenType::SymbolColon)) {
            current++;
            SkipNewLines();
            type = ParseTypeExpr(true, true);
            SkipNewLines();
        }
        ASTNode* default_;
        if (PeekIs(TokenType::OperatorAssign)) {
            current++;
            default_ = ParseExpr(0, true);
            SkipNewLines();
        }
        result.emplace_back(idTok.comp<TokenType::Identifier>(), type, default_);

        if (PeekIs(TokenType::SymbolComma)) {
            current++;
        } else break;
    }

    result.shrink_to_fit();
}

// despite the name it might return VariableExpr
ASTNode* Parser::ParsePropertyExpr() {
    auto out = ParseSingle();

    if (!out->Is<VariableExpr>()) {
        ThrowError("Expected variable name", out);
    }

    if (!PeekIs(TokenType::SymbolDot)) {
        return out;
    }

    while (PeekIs(TokenType::SymbolDot)) {
        current++;
        auto idTok = Next();
        if (idTok.type != TokenType::Identifier) {
            ThrowError("Expected identifier after '.'", idTok);
        }
        out = CreateNode<PropertyExpr>(out, idTok.comp<TokenType::Identifier>());
    }

    return out;
}

void Parser::ParseFunctionArgument(Parameter& param) {
    auto tok = Peek();
    if (tok.type != TokenType::Identifier) {
        ThrowError("Expected parameter name", tok);
    }
    current++;
    ASTNode* type = noneNode;
    ASTNode* default_ = noneNode;
    SkipNewLines();
    if (PeekIs(TokenType::SymbolColon)) {
        current++;
        type = ParseTypeExpr();
        SkipNewLines();
    }
    if (PeekIs(TokenType::OperatorAssign)) {
        current++;
        default_ = ParseExpr(0, true);
        SkipNewLines();
    }
    param.type = type;
    param.id = tok.comp<TokenType::Identifier>();
    param.default_ = default_;
}

void Parser::ParseFunctionArguments(std::vector<Parameter>& result) {
    SkipNewLines();
    if (!PeekIs(TokenType::SymbolRightParenthesis)) {
        while (true) {
            auto tok = Peek();
            if (tok.type != TokenType::Identifier) {
                ThrowError("Expected parameter name", tok);
            }
            current++;
            ASTNode* type = noneNode;
            ASTNode* default_ = noneNode;
            SkipNewLines();
            if (PeekIs(TokenType::SymbolColon)) {
                current++;
                type = ParseTypeExpr();
                SkipNewLines();
            }
            if (PeekIs(TokenType::OperatorAssign)) {
                current++;
                default_ = ParseExpr(0, true);
                SkipNewLines();
            }
            result.emplace_back(tok.comp<TokenType::Identifier>(), type, default_);
            if (PeekIs(TokenType::SymbolComma)) {
                current++;
                SkipNewLines();
            } else break;
        }

        result.shrink_to_fit();
    }
}

ASTNode* Parser::ParseVariableDefine(bool const_) {
    SkipNewLines();
    auto name = ParsePropertyExpr();
    SkipNewLines();
    ASTNode* type = noneNode;
    auto typed = PeekIs(TokenType::SymbolColon);
    if (typed) {
        current++;
        type = ParseTypeExpr();
        SkipNewLines();
    }
    auto n = Next();
    auto out = CreateNode<VariableDefineStmt>(const_, name, type);
    if (n.type == TokenType::OperatorAssign || (typed && n.type == TokenType::OperatorGte)) {
        SkipNewLines();
        out->value = ParseExpr();
    }
    return out;
}

ASTNode* Parser::ParseForLoop(TokenComp<TokenType::KeywordFor> tok) {
    SkipNewLines();
    Expect(TokenType::SymbolLeftParenthesis);
    SkipNewLines();
    auto out = CreateNode<ForStmt>(tok);
    while (true) {
        auto idTok = Next();
        if (idTok.type != TokenType::Identifier) {
            ThrowError("Expected identifier in for loop", idTok);
        }

        SkipNewLines();
        out->identifiers.push_back(idTok.comp<TokenType::Identifier>());
        SkipNewLines();

        if (PeekIs(TokenType::SymbolComma)) {
            current++;
        } else break;
    }

    out->identifiers.shrink_to_fit();

    SkipNewLines();
    Expect(TokenType::KeywordIn);
    SkipNewLines();
    out->iterable = ParseExpr();
    SkipNewLines();
    Expect(TokenType::SymbolRightParenthesis);
    SkipNewLines();
    ParseBracedBlock(out->body);
    SkipNewLines();
    if (PeekIs(TokenType::KeywordElse)) {
        current++;
        SkipNewLines();
        ParseBracedBlock(out->elseBody);
    }
    return out;
}

ASTNode* Parser::ParseWhileLoop(TokenComp<TokenType::KeywordWhile> tok) {
    SkipNewLines();
    Expect(TokenType::SymbolLeftParenthesis);
    auto out = CreateNode<WhileStmt>(tok, ParseExpr(0, true));
    SkipNewLines();
    Expect(TokenType::SymbolRightParenthesis);
    SkipNewLines();
    ParseBracedBlock(out->body);
    SkipNewLines();
    if (PeekIs(TokenType::KeywordElse)) {
        current++;
        SkipNewLines();
        ParseBracedBlock(out->elseBody);
    }
    return out;
}

ASTNode* Parser::ParseDoWhileLoop(TokenComp<TokenType::KeywordDo> tok) {
    auto out = CreateNode<DoWhileStmt>(tok);
    SkipNewLines();
    ParseBracedBlock(out->body);
    SkipNewLines();
    Expect(TokenType::KeywordWhile);
    SkipNewLines();
    Expect(TokenType::SymbolLeftParenthesis);
    out->condition = ParseExpr(0, true);
    SkipNewLines();
    Expect(TokenType::SymbolRightParenthesis);
    SkipNewLines();

    if (PeekIs(TokenType::KeywordElse)) {
        current++;
        SkipNewLines();
        ParseBracedBlock(out->elseBody);
    }
    return out;
}

ASTNode* Parser::ParseAliasStmt(TokenComp<TokenType::KeywordAlias> tok) {
    SkipNewLines();
    auto idTok = Next();
    if (idTok.type != TokenType::Identifier) {
        ThrowError("Expected identifier after 'alias'", idTok);
    }
    SkipNewLines();
    Expect(TokenType::OperatorAssign);
    SkipNewLines();
    auto type = ParseTypeExpr();
    return CreateNode<AliasStmt>(tok, idTok.comp<TokenType::Identifier>(), type);
}

ASTNode* Parser::ParseImportStmt(TokenComp<TokenType::KeywordImport> tok) {
    SkipNewLines();
    auto id = ParsePropertyExpr();
    if (!PeekIs(TokenType::KeywordAs)) {
        return CreateNode<ImportStmt>(tok, false, id);
    }
    current++;
    auto aliasTok = Next();
    if (aliasTok.type == TokenType::OperatorMul) {
        return CreateNode<ImportStmt>(tok, true, id);
    }

    if (aliasTok.type != TokenType::Identifier) {
        ThrowError("Expected identifier after 'as'", aliasTok);
    }

    return CreateNode<ImportStmt>(tok, false, id, aliasTok.comp<TokenType::Identifier>());
}

ASTNode* Parser::ParseFunctionStmt(TokenComp<TokenType::KeywordFun> funTok, PropVisibility visibility) {
    auto tok = Peek();

    if (tok.type == TokenType::KeywordOperator) {
        // protected fun operator <a: int = 5> +(x, y: int): xyz = 10 + 5
        // protected fun operator test.test<a: int = 5> +(x, y: int): xyz = 10 + 5
        current++;
        auto out = CreateNode<OperatorFunctionStmt>(funTok, visibility);

        if (PeekIs(TokenType::Identifier)) {
            out->id = ParsePropertyExpr();
            SkipNewLines();
        }

        if (PeekIs(TokenType::OperatorLt)) {
            current++;
            ParseGenerics(out->generics);
            SkipNewLines();
            Expect(TokenType::OperatorGt);
        }

        if (!Peek().IsOperator()) ThrowError("Expected operator");

        out->op = Next().OpType();
        SkipNewLines();

        Expect(TokenType::SymbolLeftParenthesis);
        ParseFunctionArguments(out->params);
        Expect(TokenType::SymbolRightParenthesis);

        if (PeekIs(TokenType::SymbolColon)) {
            current++;
            out->returns = ParseTypeExpr();
            SkipNewLines();
        }

        SkipNewLines();

        if (PeekIs(TokenType::OperatorAssign)) {
            current++;
            out->body.push_back(ParseExpr(0, true));
            return out;
        }

        Expect(TokenType::SymbolLeftBrace);
        current--;
        ParseBracedBlock(out->body);

        return out;
    }

    if (tok.type == TokenType::KeywordGet) {
        // protected fun get something<a: int = 5>(): xyz = 10
        // protected fun get something<a: int = 5>(): xyz {}
        current++;
        auto out = CreateNode<GetFunctionStmt>(funTok, visibility);

        out->id = ParsePropertyExpr();

        if (PeekIs(TokenType::OperatorLt)) {
            current++;
            ParseGenerics(out->generics);
            SkipNewLines();
            Expect(TokenType::OperatorGt);
        }

        SkipNewLines();

        Expect(TokenType::SymbolLeftParenthesis);
        if (!PeekIs(TokenType::SymbolRightParenthesis)) {
            ThrowError("Get function cannot have parameters");
        }

        SkipNewLines();

        if (PeekIs(TokenType::SymbolColon)) {
            current++;
            out->returns = ParseTypeExpr();
            SkipNewLines();
        }

        if (PeekIs(TokenType::OperatorAssign)) {
            current++;
            out->body.push_back(ParseExpr(0, true));
            return out;
        }

        Expect(TokenType::SymbolLeftBrace);
        current--;
        ParseBracedBlock(out->body);

        return out;
    }

    if (tok.type == TokenType::KeywordSet) {
        // protected fun set something<a: int = 5>(x: int) = 10
        // protected fun set something<a: int = 5>(x: int) {}
        current++;
        auto out = CreateNode<SetFunctionStmt>(funTok, visibility);

        out->id = ParsePropertyExpr();

        if (PeekIs(TokenType::OperatorLt)) {
            current++;
            ParseGenerics(out->generics);
            SkipNewLines();
            Expect(TokenType::OperatorGt);
        }

        SkipNewLines();

        Expect(TokenType::SymbolLeftParenthesis);
        ParseFunctionArgument(out->param);
        if (!PeekIs(TokenType::SymbolRightParenthesis)) {
            ThrowError("Set function can only have one parameter");
        }

        SkipNewLines();

        if (PeekIs(TokenType::SymbolColon)) {
            ThrowError("Set function cannot have a return type");
        }

        if (PeekIs(TokenType::OperatorAssign)) {
            current++;
            out->body.push_back(ParseExpr(0, true));
            return out;
        }

        Expect(TokenType::SymbolLeftBrace);
        current--;
        ParseBracedBlock(out->body);

        return out;
    }
    // protected fun test.test<a: int = 5>(x, y: int): xyz {}
    // protected fun test.test<a: int = 5>(x, y: int): xyz = 10 + 5

    auto out = CreateNode<FunctionStmt>(funTok, visibility);

    out->id = ParsePropertyExpr();

    if (PeekIs(TokenType::OperatorLt)) {
        current++;
        ParseGenerics(out->generics);
        SkipNewLines();
        Expect(TokenType::OperatorGt);
    }

    Expect(TokenType::SymbolLeftParenthesis);
    ParseFunctionArguments(out->params);
    Expect(TokenType::SymbolRightParenthesis);

    if (PeekIs(TokenType::SymbolColon)) {
        current++;
        out->returns = ParseTypeExpr();
        SkipNewLines();
    }

    if (PeekIs(TokenType::OperatorAssign)) {
        current++;
        out->body.push_back(ParseExpr(0, true));
        return out;
    }

    Expect(TokenType::SymbolLeftBrace);
    current--;
    ParseBracedBlock(out->body);

    return out;
}

ASTNode* Parser::ParseStructStmt(TokenComp<TokenType::KeywordStruct> tok, bool isEnum) {
    // enum struct MyStruct<T, K: int, L: int = 5> extends SomeStruct implements SomeInterface, AnotherInterface {
    //
    // }
    SkipNewLines();
    auto idTok = Next();
    if (idTok.type != TokenType::Identifier) {
        ThrowError("Expected identifier after 'struct'", idTok);
    }
    auto out = CreateNode<StructStmt>(tok, isEnum, idTok.comp<TokenType::Identifier>());

    if (PeekIs(TokenType::OperatorLt)) {
        current++;
        ParseGenerics(out->generics);
        SkipNewLines();
        Expect(TokenType::OperatorGt);
    }

    if (PeekIs(TokenType::KeywordExtends)) {
        current++;
        SkipNewLines();
        out->extends = ParseTypeExpr();
    }

    if (PeekIs(TokenType::KeywordImplements)) {
        current++;
        SkipNewLines();
        ParseTypeExpressions(out->implements);
    }

    if (out->extends->Is<NoneNode>() && PeekIs(TokenType::KeywordExtends)) {
        current++;
        SkipNewLines();
        out->extends = ParseTypeExpr();
    }

    Expect(TokenType::SymbolLeftBrace);
    SkipNewLines();
    if (isEnum && !PeekIs(TokenType::SymbolRightBrace)) {
        while (true) {
            auto name = Next();
            if (name.type != TokenType::Identifier) {
                ThrowError("Expected identifier in enum", name);
            }
            bool manual = false;
            ASTNode* value = noneNode;
            auto eq = Peek();
            if (eq.type == TokenType::OperatorAssign || eq.type == TokenType::SymbolWalrus) {
                manual = eq.type == TokenType::SymbolWalrus;
                current++;
                value = ParseExpr(0, true);
            }
            out->enumMembers.emplace_back(manual, name.comp<TokenType::Identifier>(), value);
            while (true) {
                auto p = Peek();
                if (p.type == TokenType::NewLine && p.value[0] != ';') current++;
                else break;
            }
            if (!PeekIs(TokenType::SymbolComma)) break;
            current++;
            SkipNewLines();
        }
    }

    ParseStatements(out->statements);
    Expect(TokenType::SymbolRightBrace);

    return out;
}

ASTNode* Parser::ParseInterfaceStmt(TokenComp<TokenType::KeywordInterface> tok) {
    // interface MyInterface<T, K: int, L: int = 5> extends SomeInterface {
    //
    // }
    auto idTok = Next();
    if (idTok.type != TokenType::Identifier) {
        ThrowError("Expected identifier after 'interface'", idTok);
    }

    auto out = CreateNode<InterfaceStmt>();

    if (PeekIs(TokenType::OperatorLt)) {
        current++;
        ParseGenerics(out->generics);
        SkipNewLines();
        Expect(TokenType::OperatorGt);
    }

    if (PeekIs(TokenType::KeywordExtends)) {
        current++;
        SkipNewLines();
        ParseTypeExpressions(out->extends);
    }

    Expect(TokenType::SymbolLeftBrace);
    SkipNewLines();
    ParseStatements(out->statements);
    SkipNewLines();
    Expect(TokenType::SymbolRightBrace);

    return out;
}

ASTNode* Parser::ParseStatement() {
    auto tok = Next();

    if (tok.type == TokenType::KeywordReturn) {
        auto value = ParseExpr();
        return CreateNode<ReturnStmt>(value);
    }

    if (tok.type == TokenType::KeywordYield) {
        auto value = ParseExpr();
        return CreateNode<ReturnStmt>(value);
    }

    if (tok.type == TokenType::KeywordBreak) {
        return CreateNode<BreakStmt>(tok.comp<TokenType::KeywordBreak>());
    }

    if (tok.type == TokenType::KeywordContinue) {
        return CreateNode<ContinueStmt>(tok.comp<TokenType::KeywordContinue>());
    }

    if (tok.type == TokenType::KeywordVal || tok.type == TokenType::KeywordVar) {
        return ParseVariableDefine(tok.type == TokenType::KeywordVal);
    }

    if (tok.type == TokenType::KeywordFor) {
        return ParseForLoop(tok.comp<TokenType::KeywordFor>());
    }

    if (tok.type == TokenType::KeywordWhile) {
        return ParseWhileLoop(tok.comp<TokenType::KeywordWhile>());
    }

    if (tok.type == TokenType::KeywordDo) {
        return ParseDoWhileLoop(tok.comp<TokenType::KeywordDo>());
    }

    if (tok.type == TokenType::KeywordAlias) {
        return ParseAliasStmt(tok.comp<TokenType::KeywordAlias>());
    }

    if (tok.type == TokenType::KeywordImport) {
        return ParseImportStmt(tok.comp<TokenType::KeywordImport>());
    }

    if (
        tok.type == TokenType::KeywordPublic
        || tok.type == TokenType::KeywordProtected
        || tok.type == TokenType::KeywordPrivate
        || tok.type == TokenType::KeywordFun
    ) {
        auto funTok = tok;
        auto visibility = PropVisibility::Public;

        if (tok.type != TokenType::KeywordFun) {
            current++;
            funTok = Peek();
            Expect(TokenType::KeywordFun);
        }

        if (tok.type == TokenType::KeywordProtected) visibility = PropVisibility::Protected;
        else if (tok.type == TokenType::KeywordPrivate) visibility = PropVisibility::Private;

        return ParseFunctionStmt(funTok.comp<TokenType::KeywordFun>(), visibility);
    }

    if (tok.type == TokenType::KeywordEnum || tok.type == TokenType::KeywordStruct) {
        auto structTok = tok;
        if (tok.type == TokenType::KeywordEnum) {
            structTok = Peek();
            Expect(TokenType::KeywordStruct);
        }
        return ParseStructStmt(structTok.comp<TokenType::KeywordStruct>(), tok.type == TokenType::KeywordEnum);
    }

    if (tok.type == TokenType::KeywordInterface) {
        return ParseInterfaceStmt(tok.comp<TokenType::KeywordInterface>());
    }

    current--;
    return CreateNode<ExpressionStmt>(ParseExpr());
};

void Parser::ParseBracedBlock(std::vector<ASTNode*>& result) {
    if (PeekIs(TokenType::SymbolLeftBrace)) {
        current++;
        SkipNewLines();
        ParseStatements(result);
        Expect(TokenType::SymbolRightBrace);
    } else {
        result.push_back(ParseStatement());
    }

    result.shrink_to_fit();
}

void Parser::ParseStatements(std::vector<ASTNode*>& result) {
    while (!Over()) {
        auto tok = Peek();
        if (tok.type == TokenType::SymbolRightBrace) break;
        if (tok.type == TokenType::NewLine) {
            current++;
            continue;
        }
        auto stmt = ParseStatement();
        result.push_back(stmt);
    }

    result.shrink_to_fit();
};
