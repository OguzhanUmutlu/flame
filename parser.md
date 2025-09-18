```css
NoneNode = NoneNode

Token = IdentifierToken
      | IntegerToken
      | FloatToken
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

Expr = VariableSetExpr
     | CallExpr
     | UnaryExpr
     | BinaryExpr
     | LiteralExpr
     | FStringExpr
     | PropertyExpr
     | IndexExpr
     | TupleExpr
     | IfExpr
     | RangeExpr
     | ScopeExpr
     | LambdaExpr

VariableIncDecExpr(type: ++x --x x++ x--, id: VariableExpr | PropertyExpr)
CallExpr(callee: Expr, args: Expr[], typeArgs: TypeExpr[])
UnaryExpr(operator: OperatorToken, right: Expr)
BinaryExpr(left: Expr, operator: OperatorToken, right: Expr)
LiteralExpr(
    value: IntegerToken
         | FloatToken
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
    conditions: Expr[],
    bodies: Stmt[][],
    else: Stmt[]
)
RangeExpr(start: Expr, end: Expr | NoneNode, step: Expr | NoneNode)
ScopeExpr(body: Stmt[])
LambdaExpr(
    params: Parameter[],
    returnType: TypeExpr | NoneNode,
    body: Stmt[]
)

TypeExpr =
    IdentifierTypeExpr |
    ArrayTypeExpr |
    OptionalTypeExpr |
    FunctionTypeExpr

IdentifierTypeExpr(id: IdentifierToken, arguments: TypeExpr[])
ArrayTypeExpr(elementType: TypeExpr, size: Expr | NoneNode)
OptionalTypeExpr(type: TypeExpr)
FunctionTypeExpr(params: TypeExpr[], returnType: TypeExpr | NoneNode)

Parameter(ref: bool, id: IdentifierToken, type: TypeExpr | NoneNode)
Generic(id: IdentifierToken, constraint: TypeExpr | NoneNode)
PropVisibilityFlags = 1 | 2 | 4 // public, protected, private
AccessGetFunction(
    visibility: PropVisibility,
    returnType: TypeExpr | NoneNode,
    body: Stmt[]
)
AccessSetFunction(
    visibility: PropVisibility,
    body: Stmt[]
)

ReturnStmt(value: Expr | NoneNode)
BreakStmt()
ContinueStmt()
ExpressionStmt(expression: Expr)
VariableDefineStmt(
    constant: bool,
    id: VariableExpr | PropertyExpr,
    type: TypeExpr | NoneNode,
    value: Expr
)
VariableAccessDefineStmt(
    constant: bool,
    id: VariableExpr | PropertyExpr,
    type: TypeExpr | NoneNode,
    get: AccessGetFunction[],
    set: AccessSetFunction[]
)
ForStmt(
    id: IdentifierToken,
    iterable: Expr,
    body: Stmt[],
    elseBody: Stmt[]
)
FunctionStmt(
    modifiers: PropVisibility,
    id: VariableExpr | PropertyExpr,
    generics: Generic[],
    params: Parameter[],
    returnType: TypeExpr | NoneNode,
    body: Stmt[]
)
BinaryOpFunctionStmt(
    visibility: PropVisibility,
    id: VariableExpr | PropertyExpr,
    param: Parameter,
    returnType: TypeExpr | NoneNode,
    body: Stmt[]
)
UnaryOpFunctionStmt(
    visibility: PropVisibility,
    id: VariableExpr | PropertyExpr,
    returnType: TypeExpr | NoneNode,
    body: Stmt[]
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
AliasStmt(
    id: IdentifierToken,
    type: TypeExpr
)
ClassStmt(
    id: IdentifierToken,
    generics: Generic[],
    superClass: VariableExpr | PropertyExpr | NoneNode,
    properties: VariableDefineStmt[],
    accessors: VariableAccessDefineStmt[],
    methods: FunctionStmt[],
    staticMethods: FunctionStmt[],
    constructor: FunctionStmt | NoneNode,
    binaryOperators: BinaryOpFunctionStmt[],
    unaryOperators: UnaryOpFunctionStmt[]
)
ImportStmt(module: VariableExpr | PropertyExpr, alias: IdentifierToken | NoneNode)
Program(stmts: Stmt[])
```