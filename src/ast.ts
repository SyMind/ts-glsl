export type Program<Body = []> = {
    body: Body
}

export type StatementList = StatementNoNewScope[]

export type StatementNoNewScope =
    | CompoundStatementWithScope
    | SimpleStatement

export type CompoundStatementWithScope<Body = []> = {
    body: Body
}

export type SimpleStatement =
    | DeclarationStatement
    // | ExpressionStatement
    // | SelectionStatement
    // | IterationStatement
    // | JumpStatement

export type DeclarationStatement =
    | FunctionPrototype
    | InitDeclaratorList

export type FunctionPrototype<Identifier = '', Params = [], Body = []> = {
    identifier: Identifier;
    params: Params;
    body: Body;
}

export type ParameterDeclarator<Specifer, Identifier> = {
    specifer: Specifer;
    identifier: Identifier;
}

export type InitDeclaratorList = {
}

export type SingleDeclaration<
    Type,
    Identifier = undefined
> = {
    type: Type;
    identifier: Identifier;
}

export type FullySpecifiedType<
    Specifier = '',
    Qualifier = undefined
> = {
    specifier: Specifier;
    qualifier: Qualifier;
}

export type EmptyExpressionStatement = {}

export type AssignmentExpressionStatement<Right, Left, Operator> = {
    operator: Operator;
    right: Right;
    left: Left;
}
