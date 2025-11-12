#include "parser.hpp"

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

ASTNode Parser::parseSingleExpression() {
    auto tok = next();

    if (tok.isLiteral()) {
        return createNode<LiteralExpr>(tok);
    }

    if (tok.type == TokenType::Identifier) {
        return createNode<VariableExpr>(IdentifierToken(tok.value));
    }

    if (tok.type == TokenType::KeywordMove) {
        auto value = parseSingleExpression();
        return createNode<MoveExpr>(tok.comp<TokenType::KeywordMove>(), value);
    }

    if (
        tok.type == TokenType::OperatorAdd
        || tok.type == TokenType::OperatorSub
        || tok.type == TokenType::OperatorNot
        || tok.type == TokenType::OperatorBitNot
    ) {
        return createNode<UnaryExpr>(tok.op(), parseSingleExpression());
    }

    if (tok.type == TokenType::OperatorInc || tok.type == TokenType::OperatorDec) {
        auto in = parseSingleExpression();

        if (!in->is<VariableExpr>() && !in->is<PropertyExpr>()) {
            throwError("Can only apply ++/-- to variables", in);
        }

        auto type = (tok.type == TokenType::OperatorInc)
                        ? VariableIncDecExpr::Type::IncLeft
                        : VariableIncDecExpr::Type::DecLeft;

        return createNode<VariableIncDecExpr>(type, in);
    }

    if (tok.type == TokenType::SymbolLeftParenthesis) {
        size_t parEnd = 0;
        size_t parAmount = 0;
        // finding the matching right parenthesis
        for (auto i = current; i < tokenCount(); i++) {
            auto curTok = get(i);
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
            throwError("Unclosed parenthesis", tok);
        }

        auto parEndTok = get(parEnd + 1);
        bool lambdaSure = parEndTok.type == TokenType::SymbolArrow;

        if (parEndTok.type == TokenType::SymbolColon) {
            parAmount = 0;
            for (auto i = parEnd + 2; i < tokenCount(); i++) {
                auto curTok = get(i);
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
            auto out = createNode<LambdaExpr>(tok.comp<TokenType::SymbolLeftParenthesis>(), noneNode);

            if (!peekIs(TokenType::SymbolRightParenthesis)) {
                while (true) {
                    auto tok2 = peek();
                    if (tok2.type != TokenType::Identifier) {
                        throwError("Expected a parameter name", tok2);
                    }
                    ASTNode type{};
                    ASTNode default_{};
                    current++;
                    if (peekIs(TokenType::SymbolColon)) {
                        current++;
                        type = parseTypeExpr();
                    }
                    if (peekIs(TokenType::OperatorAssign)) {
                        current++;
                        default_ = parseExpression(0, true);
                    }
                    out->params.emplace_back(tok2.comp<TokenType::Identifier>(), type, default_);
                    if (peekIs(TokenType::SymbolComma)) {
                        current++;
                    } else {
                        break;
                    }
                }

                out->params.shrink_to_fit();
            }

            Expect(TokenType::SymbolRightParenthesis);

            if (parEndTok.type == TokenType::SymbolColon) {
                out->returns = parseTypeExpr();
            }

            Expect(TokenType::SymbolArrow);

            parseBracedBlock(out->body);

            return out;
        }

        std::vector<ASTNode> elements;

        if (!peekIs(TokenType::SymbolRightParenthesis)) {
            while (true) {
                auto out = parseExpression(0, true);

                if (peekIs(TokenType::SymbolComma)) {
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

        auto out = createNode<TupleExpr>(tok.comp<TokenType::SymbolLeftParenthesis>());

        out->elements = std::move(elements);

        return out;
    }

    if (tok.type == TokenType::StringStart) {
        auto out = createNode<FStringExpr>(tok.comp<TokenType::StringStart>());

        while (true) {
            skipNewLines();
            auto tok2 = peek();

            if (tok2.type == TokenType::StringEnd) {
                auto endTok = next();
                out->end = endTok.comp<TokenType::StringEnd>();
                return out;
            }

            if (tok2.type == TokenType::StringMiddle) {
                auto midTok = next();
                out->middles.push_back(midTok.comp<TokenType::StringMiddle>());
                continue;
            }

            out->expressions.push_back(parseExpression(0, true));

            if (over()) {
                throwError("Unterminated f-string", tok);
            }
        }

        out->middles.shrink_to_fit();
        out->expressions.shrink_to_fit();
    }

    if (tok.type == TokenType::KeywordIf) {
        Expect(TokenType::SymbolLeftParenthesis);

        auto out = createNode<IfExpr>(tok.comp<TokenType::KeywordIf>(), parseExpression(0, true));

        Expect(TokenType::SymbolRightParenthesis);

        parseBracedBlock(out->body);

        if (peekIs(TokenType::KeywordElse)) {
            current++;
            parseBracedBlock(out->elseBody);
        }

        return out;
    }

    if (tok.type == TokenType::SymbolLeftBrace) {
        auto out = createNode<ScopeExpr>(tok.comp<TokenType::SymbolLeftBrace>());
        current--;
        parseBracedBlock(out->body);
        return out;
    }

    throwError("Unexpected token", tok);
}

ASTNode Parser::parseExpression(int rbp, bool ignoreNewLines, bool endAtSet, bool endAtGt, bool allowType) {
    if (ignoreNewLines) skipNewLines();
    auto out = parseSingleExpression();

    while (!over()) {
        auto tok = peek();
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
            for (auto i = current + 1; i < tokenCount(); i++) {
                auto tok2 = get(i);
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
            auto end = parseSingleExpression();
            auto t2 = peek();
            if (t2.type == TokenType::SymbolColon) {
                current++;
                auto step = parseSingleExpression();
                out = createNode<RangeExpr>(out, end, step);
            } else {
                out = createNode<RangeExpr>(out, end, noneNode);
            }
            continue;
        }

        if (tok.type == TokenType::SymbolDot) {
            current++;
            skipNewLines(); // after a dot we can have new lines
            auto idTok = next();
            if (idTok.type != TokenType::Identifier) {
                throwError("Expected identifier after '.'", idTok);
            }
            out = createNode<PropertyExpr>(out, idTok.comp<TokenType::Identifier>());
            continue;
        }


        if (
            (out->is<VariableExpr>() || out->is<PropertyExpr>())
            && tok.type == TokenType::OperatorLt
        ) {
            bool foundTypeArgs = false;
            for (auto i = current + 1; i < tokenCount(); i++) {
                auto tok2 = get(i);
                if (tok2.type == TokenType::OperatorAssign && endAtSet) break;
                if (tok2.type == TokenType::OperatorGte) {
                    foundTypeArgs = endAtSet;
                    break;
                }
                if (tok2.type == TokenType::OperatorGt) {
                    auto next = get(i + 1);
                    if (
                        !allowType
                        && next.type != TokenType::SymbolLeftParenthesis
                        && next.type != TokenType::SymbolLeftBracket
                        && next.type != TokenType::OperatorAssign
                    ) {
                        throwError(
                            "Expected function call, array type notation or variable definition right after type arguments",
                            tok2);
                    }
                    foundTypeArgs = true;
                    break;
                }
            }

            if (foundTypeArgs) {
                auto gen = createNode<GenericTypeExpr>(out);
                out = gen;
                current++;
                parseTypeExpressions(gen->arguments);
                auto p = next();
                if (p.type != TokenType::OperatorGt && p.type != TokenType::OperatorGte) {
                    throwError("Expected '>' after type arguments", p);
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
            auto call = createNode<CallExpr>(out);
            out = call;

            skipNewLines();
            if (!peekIs(TokenType::SymbolRightParenthesis)) {
                while (true) {
                    if (peekIs(TokenType::NewLine)) {
                        current++;
                        continue;
                    }
                    if (peekIs(TokenType::Identifier) && peekNoLine(1).type == TokenType::SymbolColon) {
                        auto nameTok = next();
                        skipNewLines();
                        Expect(TokenType::SymbolColon);
                        auto expr = parseExpression(0, true);
                        call->args.emplace_back(false, nameTok.comp<TokenType::Identifier>(), expr);
                    } else if (peekIs(TokenType::SymbolEllipsis)) {
                        current++;
                        auto expr = parseExpression(0, true);
                        call->args.emplace_back(true, std::nullopt, expr);
                    } else {
                        auto expr = parseExpression(0, true);
                        call->args.emplace_back(false, std::nullopt, expr);
                    }

                    if (peekIs(TokenType::SymbolComma)) {
                        current++;
                    } else break;
                }

                call->args.shrink_to_fit();
            }
            skipNewLines();
            Expect(TokenType::SymbolRightParenthesis);
            continue;
        }

        if (tok.type == TokenType::SymbolLeftBracket) {
            current++;
            skipNewLines();
            if (peekIs(TokenType::SymbolRightBracket)) {
                current++;
                out = createNode<IndexExpr>(out, noneNode);
                continue;
            }
            auto index = parseExpression(0, true);
            skipNewLines();
            Expect(TokenType::SymbolRightBracket);
            out = createNode<IndexExpr>(out, index);
            continue;
        }

        int lbp{};
        OperatorType op{};

        if (!tok.isOperator() || rbp >= (lbp = LeftBindingPower(op = tok.opType()))) return out;
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
            skipNewLines(); // after an operator we can have new lines
            out = createNode<BinaryExpr>(out, tok.op(),
                                         parseExpression(lbp, ignoreNewLines, endAtSet, endAtGt));
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
            skipNewLines(); // after an operator we can have new lines
            out = createNode<BinaryExpr>(out, tok.op(),
                                         parseExpression(lbp - 1, ignoreNewLines, endAtSet, endAtGt));
        }
        break;
        case OperatorType::Inc:
        case OperatorType::Dec: {
            auto type = (op == OperatorType::Inc)
                            ? VariableIncDecExpr::Type::IncRight
                            : VariableIncDecExpr::Type::DecRight;

            out = createNode<VariableIncDecExpr>(type, out);
        }
        break;
        case OperatorType::Not:
            out = createNode<UnaryExpr>(tok.op(), out);
            break;
        case OperatorType::BitNot:
            throwError("Bitwise NOT (~) cannot be used as a suffix/binary operator", tok);
        case OperatorType::None:
            throwError("How did we get here?", tok);
        }
    }

    return out;
}

void Parser::parseExpressions(std::vector<ASTNode>& result, bool ignoreNewLines) {
    while (true) {
        auto expr = parseExpression(0, ignoreNewLines);
        result.push_back(expr);

        if (peekIs(TokenType::SymbolComma)) {
            current++;
        } else break;
    }

    result.shrink_to_fit();
}

ASTNode Parser::parseTypeExpr(bool ignoreNewLines, bool endAtGt) {
    auto tok = peek();

    if (tok.type == TokenType::OperatorBitAnd) {
        current++;
        return createNode<ReferenceTypeExpr>(parseTypeExpr());
    }

    if (tok.type == TokenType::SymbolLeftParenthesis) {
        size_t parAmount = 0;
        bool lambdaSure = false;
        for (auto i = current + 1; i < tokenCount(); i++) {
            auto curTok = get(i);
            if (curTok.type == TokenType::SymbolLeftParenthesis) {
                parAmount++;
            } else if (curTok.type == TokenType::SymbolRightParenthesis) {
                if (parAmount == 0) {
                    lambdaSure = get(i + 1).type == TokenType::SymbolArrow;
                    break;
                }
                parAmount--;
            }
        }

        if (lambdaSure) {
            current++;
            Expect(TokenType::SymbolLeftParenthesis);

            auto out = createNode<FunctionTypeExpr>(noneNode);

            if (!peekIs(TokenType::SymbolRightParenthesis)) {
                parseTypeExpressions(out->params);
            }

            Expect(TokenType::SymbolRightParenthesis);
            skipNewLines();
            Expect(TokenType::SymbolArrow);
            skipNewLines();

            out->returns = parseTypeExpr(true);

            return out;
        }
        current++;
        skipNewLines();
        auto type = parseExpression(0, true);
        skipNewLines();
        Expect(TokenType::SymbolRightParenthesis);
        skipNewLines();
        return type;
    }

    return parseExpression(0, ignoreNewLines, true, endAtGt, true);
}

void Parser::parseTypeExpressions(std::vector<ASTNode>& result) {
    skipNewLines();
    while (true) {
        auto type = parseTypeExpr(true, true);
        skipNewLines();
        result.push_back(type);

        if (peekIs(TokenType::SymbolComma)) {
            current++;
        } else break;
    }

    result.shrink_to_fit();
}

void Parser::parseGenerics(std::vector<Generic>& result) {
    skipNewLines();
    while (true) {
        auto idTok = next();
        if (idTok.type != TokenType::Identifier) {
            throwError("Expected generic name", idTok);
        }
        skipNewLines();
        ASTNode type = noneNode;
        if (peekIs(TokenType::SymbolColon)) {
            current++;
            skipNewLines();
            type = parseTypeExpr(true, true);
            skipNewLines();
        }
        ASTNode default_;
        if (peekIs(TokenType::OperatorAssign)) {
            current++;
            default_ = parseExpression(0, true);
            skipNewLines();
        }
        result.emplace_back(idTok.comp<TokenType::Identifier>(), type, default_);

        if (peekIs(TokenType::SymbolComma)) {
            current++;
        } else break;
    }

    result.shrink_to_fit();
}

// despite the name it might return VariableExpr
ASTNode Parser::parsePropertyExpr() {
    auto out = parseSingleExpression();

    if (!out->is<VariableExpr>()) {
        throwError("Expected variable name", out);
    }

    if (!peekIs(TokenType::SymbolDot)) {
        return out;
    }

    while (peekIs(TokenType::SymbolDot)) {
        current++;
        auto idTok = next();
        if (idTok.type != TokenType::Identifier) {
            throwError("Expected identifier after '.'", idTok);
        }
        out = createNode<PropertyExpr>(out, idTok.comp<TokenType::Identifier>());
    }

    return out;
}

void Parser::parseFunctionArgument(Parameter& param) {
    auto tok = peek();
    if (tok.type != TokenType::Identifier) {
        throwError("Expected parameter name", tok);
    }
    current++;
    ASTNode type = noneNode;
    ASTNode default_ = noneNode;
    skipNewLines();
    if (peekIs(TokenType::SymbolColon)) {
        current++;
        type = parseTypeExpr();
        skipNewLines();
    }
    if (peekIs(TokenType::OperatorAssign)) {
        current++;
        default_ = parseExpression(0, true);
        skipNewLines();
    }
    param.type = type;
    param.id = tok.comp<TokenType::Identifier>();
    param.default_ = default_;
}

void Parser::parseFunctionArguments(std::vector<Parameter>& result) {
    skipNewLines();
    if (!peekIs(TokenType::SymbolRightParenthesis)) {
        while (true) {
            auto tok = peek();
            if (tok.type != TokenType::Identifier) {
                throwError("Expected parameter name", tok);
            }
            current++;
            ASTNode type = noneNode;
            ASTNode default_ = noneNode;
            skipNewLines();
            if (peekIs(TokenType::SymbolColon)) {
                current++;
                type = parseTypeExpr();
                skipNewLines();
            }
            if (peekIs(TokenType::OperatorAssign)) {
                current++;
                default_ = parseExpression(0, true);
                skipNewLines();
            }
            result.emplace_back(tok.comp<TokenType::Identifier>(), type, default_);
            if (peekIs(TokenType::SymbolComma)) {
                current++;
                skipNewLines();
            } else break;
        }

        result.shrink_to_fit();
    }
}

ASTNode Parser::parseVariableDefine(bool const_) {
    skipNewLines();
    auto name = parsePropertyExpr();
    skipNewLines();
    ASTNode type = noneNode;
    auto typed = peekIs(TokenType::SymbolColon);
    if (typed) {
        current++;
        type = parseTypeExpr();
        skipNewLines();
    }
    auto n = next();
    ASTNode value = noneNode;
    if (n.type == TokenType::OperatorAssign || (typed && n.type == TokenType::OperatorGte)) {
        skipNewLines();
        value = parseExpression();
    }
    return createNode<VariableDefineStmt>(const_, name, type, value);
}

ASTNode Parser::parseForLoop(TokenComp<TokenType::KeywordFor> tok) {
    skipNewLines();
    Expect(TokenType::SymbolLeftParenthesis);
    skipNewLines();
    auto constTok = next();
    bool constant = constTok.type == TokenType::KeywordVal;
    if (!constant && constTok.type != TokenType::KeywordVar) {
        throwError("Expected 'val' or 'var' in for loop", constTok);
    }
    skipNewLines();
    auto out = createNode<ForStmt>(tok, constant, noneNode);
    while (true) {
        auto idTok = next();
        if (idTok.type != TokenType::Identifier) {
            throwError("Expected identifier in for loop", idTok);
        }

        skipNewLines();
        out->identifiers.push_back(idTok.comp<TokenType::Identifier>());
        skipNewLines();

        if (peekIs(TokenType::SymbolComma)) {
            current++;
        } else break;
    }

    out->identifiers.shrink_to_fit();

    skipNewLines();
    Expect(TokenType::KeywordIn);
    skipNewLines();
    out->iterable = parseExpression();
    skipNewLines();
    Expect(TokenType::SymbolRightParenthesis);
    skipNewLines();
    parseBracedBlock(out->body);
    skipNewLines();
    if (peekIs(TokenType::KeywordElse)) {
        current++;
        skipNewLines();
        parseBracedBlock(out->elseBody);
    }
    return out;
}

ASTNode Parser::parseWhileLoop(TokenComp<TokenType::KeywordWhile> tok) {
    skipNewLines();
    Expect(TokenType::SymbolLeftParenthesis);
    auto out = createNode<WhileStmt>(tok, parseExpression(0, true));
    skipNewLines();
    Expect(TokenType::SymbolRightParenthesis);
    skipNewLines();
    parseBracedBlock(out->body);
    skipNewLines();
    if (peekIs(TokenType::KeywordElse)) {
        current++;
        skipNewLines();
        parseBracedBlock(out->elseBody);
    }
    return out;
}

ASTNode Parser::parseDoWhileLoop(TokenComp<TokenType::KeywordDo> tok) {
    auto out = createNode<DoWhileStmt>(tok, noneNode);
    skipNewLines();
    parseBracedBlock(out->body);
    skipNewLines();
    Expect(TokenType::KeywordWhile);
    skipNewLines();
    Expect(TokenType::SymbolLeftParenthesis);
    out->condition = parseExpression(0, true);
    skipNewLines();
    Expect(TokenType::SymbolRightParenthesis);
    skipNewLines();

    if (peekIs(TokenType::KeywordElse)) {
        current++;
        skipNewLines();
        parseBracedBlock(out->elseBody);
    }
    return out;
}

ASTNode Parser::parseAliasStmt(TokenComp<TokenType::KeywordAlias> tok) {
    skipNewLines();
    auto idTok = next();
    if (idTok.type != TokenType::Identifier) {
        throwError("Expected identifier after 'alias'", idTok);
    }
    skipNewLines();
    Expect(TokenType::OperatorAssign);
    skipNewLines();
    auto type = parseTypeExpr();
    return createNode<AliasStmt>(tok, idTok.comp<TokenType::Identifier>(), type);
}

ASTNode Parser::parseImportStmt(TokenComp<TokenType::KeywordImport> tok) {
    skipNewLines();
    auto id = parsePropertyExpr();
    if (!peekIs(TokenType::KeywordAs)) {
        return createNode<ImportStmt>(tok, false, id, std::nullopt);
    }
    current++;
    auto aliasTok = next();
    if (aliasTok.type == TokenType::OperatorMul) {
        return createNode<ImportStmt>(tok, true, id, std::nullopt);
    }

    if (aliasTok.type != TokenType::Identifier) {
        throwError("Expected identifier after 'as'", aliasTok);
    }

    return createNode<ImportStmt>(tok, false, id, aliasTok.comp<TokenType::Identifier>());
}

ASTNode Parser::parseFunctionStmt(TokenComp<TokenType::KeywordFun> funTok, PropVisibility visibility) {
    auto tok = peek();

    if (tok.type == TokenType::KeywordOperator) {
        // protected fun operator <a: int = 5> +(x, y: int): xyz = 10 + 5
        // protected fun operator test.test<a: int = 5> +(x, y: int): xyz = 10 + 5
        current++;
        auto out = createNode<OperatorFunctionStmt>(funTok, visibility, OperatorType{}, noneNode, noneNode);

        if (peekIs(TokenType::Identifier)) {
            out->id = parsePropertyExpr();
            skipNewLines();
        }

        if (peekIs(TokenType::OperatorLt)) {
            current++;
            parseGenerics(out->generics);
            skipNewLines();
            Expect(TokenType::OperatorGt);
        }

        if (!peek().isOperator()) throwError("Expected operator");

        out->op = next().opType();
        skipNewLines();

        Expect(TokenType::SymbolLeftParenthesis);
        parseFunctionArguments(out->params);
        Expect(TokenType::SymbolRightParenthesis);

        if (peekIs(TokenType::SymbolColon)) {
            current++;
            out->returns = parseTypeExpr();
            skipNewLines();
        }

        skipNewLines();

        if (peekIs(TokenType::OperatorAssign)) {
            current++;
            auto expr = parseExpression();
            out->body.push_back(createNode<ReturnStmt>(expr->getToken(), expr));
            return out;
        }

        Expect(TokenType::SymbolLeftBrace);
        current--;
        parseBracedBlock(out->body);

        return out;
    }

    if (tok.type == TokenType::KeywordGet) {
        // protected fun get something<a: int = 5>(): xyz = 10
        // protected fun get something<a: int = 5>(): xyz {}
        current++;
        auto out = createNode<GetFunctionStmt>(funTok, visibility, noneNode, noneNode);

        out->id = parsePropertyExpr();

        if (peekIs(TokenType::OperatorLt)) {
            current++;
            parseGenerics(out->generics);
            skipNewLines();
            Expect(TokenType::OperatorGt);
        }

        skipNewLines();

        Expect(TokenType::SymbolLeftParenthesis);
        if (!peekIs(TokenType::SymbolRightParenthesis)) {
            throwError("Get function cannot have parameters");
        }

        skipNewLines();

        if (peekIs(TokenType::SymbolColon)) {
            current++;
            out->returns = parseTypeExpr();
            skipNewLines();
        }

        if (peekIs(TokenType::OperatorAssign)) {
            current++;
            auto expr = parseExpression();
            out->body.push_back(createNode<ReturnStmt>(expr->getToken(), expr));
            return out;
        }

        Expect(TokenType::SymbolLeftBrace);
        current--;
        parseBracedBlock(out->body);

        return out;
    }

    if (tok.type == TokenType::KeywordSet) {
        // protected fun set something<a: int = 5>(x: int) = 10
        // protected fun set something<a: int = 5>(x: int) {}
        current++;
        auto out = createNode<SetFunctionStmt>(funTok, visibility, noneNode, Parameter{{}, noneNode, noneNode});

        out->id = parsePropertyExpr();

        if (peekIs(TokenType::OperatorLt)) {
            current++;
            parseGenerics(out->generics);
            skipNewLines();
            Expect(TokenType::OperatorGt);
        }

        skipNewLines();

        Expect(TokenType::SymbolLeftParenthesis);
        parseFunctionArgument(out->param);
        if (!peekIs(TokenType::SymbolRightParenthesis)) {
            throwError("Set function can only have one parameter");
        }

        skipNewLines();

        if (peekIs(TokenType::SymbolColon)) {
            throwError("Set function cannot have a return type");
        }

        if (peekIs(TokenType::OperatorAssign)) {
            current++;
            auto expr = parseExpression();
            out->body.push_back(createNode<ReturnStmt>(expr->getToken(), expr));
            return out;
        }

        Expect(TokenType::SymbolLeftBrace);
        current--;
        parseBracedBlock(out->body);

        return out;
    }
    // protected fun test.test<a: int = 5>(x, y: int): xyz {}
    // protected fun test.test<a: int = 5>(x, y: int): xyz = 10 + 5

    auto out = createNode<FunctionStmt>(funTok, visibility, noneNode, noneNode);

    out->id = parsePropertyExpr();

    if (peekIs(TokenType::OperatorLt)) {
        current++;
        parseGenerics(out->generics);
        skipNewLines();
        Expect(TokenType::OperatorGt);
    }

    Expect(TokenType::SymbolLeftParenthesis);
    parseFunctionArguments(out->params);
    Expect(TokenType::SymbolRightParenthesis);

    if (peekIs(TokenType::SymbolColon)) {
        current++;
        out->returns = parseTypeExpr();
        skipNewLines();
    }

    if (peekIs(TokenType::OperatorAssign)) {
        current++;
        auto expr = parseExpression();
        out->body.push_back(createNode<ReturnStmt>(expr->getToken(), expr));
        return out;
    }

    Expect(TokenType::SymbolLeftBrace);
    current--;
    parseBracedBlock(out->body);

    return out;
}

ASTNode Parser::parseStructStmt(TokenComp<TokenType::KeywordStruct> tok, bool isEnum) {
    // enum struct MyStruct<T, K: int, L: int = 5> extends SomeStruct implements SomeInterface, AnotherInterface {
    //
    // }
    skipNewLines();
    auto idTok = next();
    if (idTok.type != TokenType::Identifier) {
        throwError("Expected identifier after 'struct'", idTok);
    }
    auto out = createNode<StructStmt>(tok, isEnum, idTok.comp<TokenType::Identifier>(), noneNode);

    if (peekIs(TokenType::OperatorLt)) {
        current++;
        parseGenerics(out->generics);
        skipNewLines();
        Expect(TokenType::OperatorGt);
    }

    if (peekIs(TokenType::KeywordExtends)) {
        current++;
        skipNewLines();
        out->extends = parseTypeExpr();
    }

    if (peekIs(TokenType::KeywordImplements)) {
        current++;
        skipNewLines();
        parseTypeExpressions(out->implements);
    }

    if (out->extends->is<NoneNode>() && peekIs(TokenType::KeywordExtends)) {
        current++;
        skipNewLines();
        out->extends = parseTypeExpr();
    }

    Expect(TokenType::SymbolLeftBrace);
    skipNewLines();
    if (isEnum && !peekIs(TokenType::SymbolRightBrace)) {
        while (true) {
            auto name = next();
            if (name.type != TokenType::Identifier) {
                throwError("Expected identifier in enum", name);
            }
            bool manual = false;
            ASTNode value = noneNode;
            auto eq = peek();
            if (eq.type == TokenType::OperatorAssign || eq.type == TokenType::SymbolWalrus) {
                manual = eq.type == TokenType::SymbolWalrus;
                current++;
                value = parseExpression(0, true);
            }
            out->enumMembers.emplace_back(manual, name.comp<TokenType::Identifier>(), value);
            while (true) {
                auto p = peek();
                if (p.type == TokenType::NewLine && p.value[0] != ';') current++;
                else break;
            }
            if (!peekIs(TokenType::SymbolComma)) break;
            current++;
            skipNewLines();
        }
    }

    parseStatements(out->statements);
    Expect(TokenType::SymbolRightBrace);

    return out;
}

ASTNode Parser::parseInterfaceStmt(TokenComp<TokenType::KeywordInterface> tok) {
    // interface MyInterface<T, K: int, L: int = 5> extends SomeInterface {
    //
    // }
    auto idTok = next();
    if (idTok.type != TokenType::Identifier) {
        throwError("Expected identifier after 'interface'", idTok);
    }

    auto out = createNode<InterfaceStmt>(tok, noneNode);

    if (peekIs(TokenType::OperatorLt)) {
        current++;
        parseGenerics(out->generics);
        skipNewLines();
        Expect(TokenType::OperatorGt);
    }

    if (peekIs(TokenType::KeywordExtends)) {
        current++;
        skipNewLines();
        parseTypeExpressions(out->extends);
    }

    Expect(TokenType::SymbolLeftBrace);
    skipNewLines();
    parseStatements(out->statements);
    skipNewLines();
    Expect(TokenType::SymbolRightBrace);

    return out;
}

ASTNode Parser::parseStatement() {
    auto tok = next();

    if (tok.type == TokenType::KeywordDelete) {
        auto value = parseExpression();
        return createNode<DeleteStmt>(tok.comp<TokenType::KeywordDelete>(), value);
    }

    if (tok.type == TokenType::KeywordReturn) {
        auto value = parseExpression();
        return createNode<ReturnStmt>(tok.comp<TokenType::KeywordReturn>(), value);
    }

    if (tok.type == TokenType::KeywordYield) {
        auto value = parseExpression();
        return createNode<YieldStmt>(tok.comp<TokenType::KeywordYield>(), value);
    }

    if (tok.type == TokenType::KeywordBreak) {
        return createNode<BreakStmt>(tok.comp<TokenType::KeywordBreak>());
    }

    if (tok.type == TokenType::KeywordContinue) {
        return createNode<ContinueStmt>(tok.comp<TokenType::KeywordContinue>());
    }

    if (tok.type == TokenType::KeywordVal || tok.type == TokenType::KeywordVar) {
        return parseVariableDefine(tok.type == TokenType::KeywordVal);
    }

    if (tok.type == TokenType::KeywordFor) {
        return parseForLoop(tok.comp<TokenType::KeywordFor>());
    }

    if (tok.type == TokenType::KeywordWhile) {
        return parseWhileLoop(tok.comp<TokenType::KeywordWhile>());
    }

    if (tok.type == TokenType::KeywordDo) {
        return parseDoWhileLoop(tok.comp<TokenType::KeywordDo>());
    }

    if (tok.type == TokenType::KeywordAlias) {
        return parseAliasStmt(tok.comp<TokenType::KeywordAlias>());
    }

    if (tok.type == TokenType::KeywordImport) {
        return parseImportStmt(tok.comp<TokenType::KeywordImport>());
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
            funTok = peek();
            Expect(TokenType::KeywordFun);
        }

        if (tok.type == TokenType::KeywordProtected) visibility = PropVisibility::Protected;
        else if (tok.type == TokenType::KeywordPrivate) visibility = PropVisibility::Private;

        return parseFunctionStmt(funTok.comp<TokenType::KeywordFun>(), visibility);
    }

    if (tok.type == TokenType::KeywordEnum || tok.type == TokenType::KeywordStruct) {
        auto structTok = tok;
        if (tok.type == TokenType::KeywordEnum) {
            structTok = peek();
            Expect(TokenType::KeywordStruct);
        }
        return parseStructStmt(structTok.comp<TokenType::KeywordStruct>(), tok.type == TokenType::KeywordEnum);
    }

    if (tok.type == TokenType::KeywordInterface) {
        return parseInterfaceStmt(tok.comp<TokenType::KeywordInterface>());
    }

    current--;
    return createNode<ExpressionStmt>(parseExpression());
};

void Parser::parseBracedBlock(std::vector<ASTNode>& result) {
    if (peekIs(TokenType::SymbolLeftBrace)) {
        current++;
        skipNewLines();
        parseStatements(result);
        Expect(TokenType::SymbolRightBrace);
    } else {
        result.push_back(parseStatement());
    }

    result.shrink_to_fit();
}

void Parser::parseStatements(std::vector<ASTNode>& result) {
    while (!over()) {
        auto tok = peek();
        if (tok.type == TokenType::SymbolRightBrace) break;
        if (tok.type == TokenType::NewLine) {
            current++;
            continue;
        }
        auto stmt = parseStatement();
        result.push_back(stmt);
    }

    result.shrink_to_fit();
};
