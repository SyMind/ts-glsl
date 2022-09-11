export type Program<Body = []> = {
    body: Body
}

export type StatementList = StatementNoNewScope[]

export type StatementNoNewScope =
    | CompoundStatementWithScope
    | SimpleStatement

export type CompoundStatementWithScope<Body extends StatementList = []> = {
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

export type FunctionPrototype = {

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
