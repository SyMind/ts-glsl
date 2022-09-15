export type Program<Body extends Statement[] = Statement[]> = {
    brand: 'Program';
    body: Body;
}

export type Statement =
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
    TypeSpecifier = string,
    TypeQualifier = string,
    Identifier = string
> = {
    typeSpecifier: TypeSpecifier;
    typeQualifier: TypeQualifier;
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
    TypeSpecifier extends string = string,
    TypeQualifier extends string | void = void,
    Identifier extends string | void = void,
> = {
    typeSpecifier: TypeSpecifier;
    typeQualifier: TypeQualifier;
    identifier: Identifier;
}

export type EmptyExpressionStatement = {}

export type AssignmentExpression<Operator, Right, Left> = {
    operator: Operator;
    right: Right;
    left: Left;
}

// selection statement

export type IfStatement<Test, Consequent, Alternate = void> = {
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

export type ContinueStatement = {
    brand: 'ContinueStatement';
}

export type BreakStatement = {
    brand: 'BreakStatement';
}

export type ReturnStatement<Body extends Expression | void = void> = {
    brand: 'ReturnStatement';
    body: Body;
}

export type DiscardStatement = {
    brand: 'DiscardStatement';
}

// expression

export type Expression =
    | BinaryExpression<string, any, any>
    | MemberExpression
    | UpdateExpression
    | ConditionalExpression<any, any, any>
    | Identifier

export type Identifier<Name extends string = string> = {
    brand: 'Identifier',
    name: Name
}

export type BinaryExpression<
    Operator extends string = string,
    Left extends Expression = Expression,
    Right extends Expression = Expression,
> = {
    brand: 'BinaryExpression';
    operator: Operator;
    left: Left;
    right: Right;
}

export type MemberExpression<
    Object extends string = string,
    Property extends string = string,
> = {
    brand: 'MemberExpression';
    object: Object;
    property: Property;
}

export type UpdateExpression<
    Prefix extends boolean = boolean,
    Operator extends string = string,
    Argument extends boolean = boolean,
> = {
    brand: 'UpdateExpression';
    prefix: Prefix;
    operator: Operator;
    argument: Argument;
}

export type ConditionalExpression<
    Test extends Expression = Expression,
    Consequent extends Expression = Expression,
    Alternate extends Expression = Expression,
> = {
    brand: 'ConditionalExpression';
    test: Test;
    consequent: Consequent;
    alternate: Alternate;
}
