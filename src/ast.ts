export type Program<Body = []> = {
    body: Body
}

export type StatementList = StatementNoNewScope[]

export type StatementNoNewScope =
    | BlockStatement
    | SimpleStatement

export type SimpleStatement =
    | DeclarationStatement
    // | ExpressionStatement
    // | SelectionStatement
    // | IterationStatement
    // | JumpStatement

export type DeclarationStatement =
    | FunctionPrototype
    | InitDeclaratorList

export type FunctionPrototype<Header = FunctionHeader, Params = []> = {
    header: Header;
    params: Params;
}

export type FunctionHeader<
    Type = FullySpecifiedType,
    Identifier = string
> = {
    type: Type;
    identifier: Identifier;
}

export type ParameterDeclarator<Specifer, Identifier> = {
    specifer: Specifer;
    identifier: Identifier;
}

/*
 type_qualifier parameter_qualifier parameter_declarator
*/
export type ParameterDeclaration<
TypeQualifier,
ParameterQualifier,
ParameterDeclarator
> = {
    typeQualifier: TypeQualifier;
    parameterQualifier: ParameterQualifier;
    parameterDeclarator: ParameterDeclarator;
}

export type InitDeclaratorList = {
}

export type SingleDeclaration<
    Type,
    Identifier = null
> = {
    type: Type;
    identifier: Identifier;
}

export type FullySpecifiedType<
    Specifier = '',
    Qualifier = null
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

type Expression = any

// selection statement

export type IfStatement<Test, Consequent, Alternate = null> = {
    test: Test;
    consequent: Consequent;
    alternate: Alternate;
}

// compound statement no new scope

export type BlockStatement<Body = []> = {
    brand: 'BlockStatement';
    body: Body;
}

// iteration statement

export type WhileStatement<Test, Body> = {
    test: Test;
    body: Body;
}

export type ForStatement<Init, Test, Update, Body> = {
    init: Init
    test: Test
    update: Update
    body: Body
}

export type DoWhileStatement<Test, Body> = {
    test: Test;
    body: Body;
}

// jump statement

export type ContinueStatement = {}

export type BreakStatement = {}

export type ReturnStatement<Body extends Expression | null = null> = {
    body: Body;
}

export type DiscardStatement = {}
