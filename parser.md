```css
NoneNode = NoneNode

Token = IdentifierToken
      | IntegerToken
      | FloatToken
      | CharToken
      | StringToken
      | BooleanToken
      | StringStartToken
      | StringMiddleToken
      | StringEndToken
      | NewLineToken
      | KeywordToken
      | OperatorToken
      | SetOperatorToken
      | SymbolToken

CallArgument = CallArgument(spread: bool, name: IdentifierToken | NoneNode, value: Expr)

VariableIncDecExpr(type: ++x --x x++ x--, id: VariableExpr | PropertyExpr)
CallExpr(callee: Expr, args: CallArgument[])
UnaryExpr(operator: OperatorToken, right: Expr)
BinaryExpr(left: Expr, operator: OperatorToken, right: Expr)
LiteralExpr(
    value: IntegerToken
         | FloatToken
         | CharToken
         | StringToken
         | BooleanToken
         | KeywordNullToken
)
FStringExpr(
    start: StringStartToken,
    exprs: Expr[],
    middles: StringMiddleToken[],
    end: StringEndToken
)
VariableExpr(id: IdentifierToken)
PropertyExpr(callee: Expr, id: IdentifierToken)
IndexExpr(callee: Expr, index: Expr)
TupleExpr(elements: Expr[])
IfExpr(
    condition: Expr[],
    body: Stmt[],
    else: Stmt[]
)
RangeExpr(start: Expr, end: Expr | NoneNode, step: Expr | NoneNode)
LambdaExpr(
    params: Parameter[],
    returnType: TypeExpr | NoneNode,
    body: Stmt[]
)
ScopeExpr(body: Stmt[])
MoveExpr(expression: Expr)

IdentifierTypeExpr(id: VariableExpr | PropertyExpr, arguments: TypeExpr[])
OptionalTypeExpr(type: TypeExpr)
FunctionTypeExpr(params: TypeExpr[], returnType: TypeExpr | NoneNode)
ReferenceTypeExpr(expr: TypeExpr)
DefaultTypeExpr(expr: TypeExpr, default: Expr)

Parameter(ref: bool, id: IdentifierToken, type: TypeExpr | NoneNode, default: Expr | NoneNode)
Generic(id: IdentifierToken, constraint: TypeExpr | NoneNode)
EnumMember(manual: bool, id: IdentifierToken, value: Expr | NoneNode)
PropVisibilityFlags = 1 | 2 | 4 // public, protected, private

ReturnStmt(value: Expr | NoneNode)
BreakStmt()
ContinueStmt()
DeleteStmt(expression: Expr)
ExpressionStmt(expression: Expr)
ImportStmt(module: VariableExpr | PropertyExpr, alias: IdentifierToken | NoneNode)
AliasStmt(id: IdentifierToken, type: TypeExpr)
VariableDefineStmt(
    constant: bool,
    id: VariableExpr | PropertyExpr,
    type: TypeExpr | NoneNode,
    value: Expr
)
ForStmt(
    identifiers: IdentifierToken[],
    iterable: Expr,
    body: Stmt[],
    elseBody: Stmt[]
)
WhileStmt(
    condition: Expr,
    body: Stmt[],
    elseBody: Stmt[]
)
DoWhileStmt(
    condition: Expr,
    body: Stmt[],
    elseBody: Stmt[]
)
FunctionStmt(
    visibility: PropVisibility,
    id: VariableExpr | PropertyExpr,
    generics: Generic[],
    params: Parameter[],
    returnType: TypeExpr | NoneNode,
    body: Stmt[]
)
OperatorFunctionStmt(
    visibility: PropVisibility,
    id: VariableExpr | PropertyExpr | NoneNode,
    op: Token,
    generics: Generic[],
    params: Parameter[],
    returnType: TypeExpr | NoneNode,
    body: Stmt[]
)
GetFunctionStmt(
    visibility: PropVisibility,
    id: VariableExpr | PropertyExpr,
    generics: Generic[],
    returnType: TypeExpr | NoneNode,
    body: Stmt[]
)
SetFunctionStmt(
    visibility: PropVisibility,
    id: VariableExpr | PropertyExpr,
    generics: Generic[],
    param: Parameter,
    body: Stmt[]
)
ClassStmt(
    enum: bool,
    id: VariableExpr | PropertyExpr,
    generics: Generic[],
    extends: VariableExpr | PropertyExpr | NoneNode,
    interfaces: (VariableExpr | PropertyExpr)[],
    statements: Stmt[],
    enumMembers: EnumMember[]
)
InterfaceStmt(
    id: VariableExpr | PropertyExpr,
    generics: Generic[],
    extends: (VariableExpr | PropertyExpr)[],
    statements: Stmt[]
)
```