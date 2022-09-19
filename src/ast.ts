export type Program<Body extends ProgramBody = ProgramBody> = {
    brand: 'Program';
    body: Body;
}

export type ProgramBody = (Statement | FunctionDefinition)[]

export type Statement =
    | BlockStatement
    | SimpleStatement

export type FunctionDefinition<
    TypeQualifier extends string | void = string | void,
    TypeSpecifier extends string = string,
    Name extends Identifier = Identifier,
    Parameters extends ParameterDeclaration[] | void = ParameterDeclaration[] | void,
    Body extends BlockStatement = BlockStatement
> = {
    brand: 'FunctionDefinition';
    typeQualifier: TypeQualifier;
    typeSpecifier: TypeSpecifier;
    name: Name;
    parameters: Parameters;
    body: Body;
}

export type ParameterDeclaration<
    TypeQualifier extends string | void = void,
    ParameterQualifier extends string | void = void,
    TypeSpecifier extends string = string,
    Name extends Identifier = Identifier
> = {
    typeQualifier: TypeQualifier;
    parameterQualifier: ParameterQualifier;
    typeSpecifier: TypeSpecifier;
    name: Name;
}

export type SimpleStatement =
    | DeclarationStatement
    // | ExpressionStatement
    // | SelectionStatement
    // | IterationStatement
    // | JumpStatement

export type DeclarationStatement =
    | FunctionDeclaration
    | InitDeclaratorList

export type FunctionDeclaration<
    TypeSpecifier extends string = string,
    TypeQualifier extends string | void = void,
    Name extends Identifier = Identifier,
    Params = []
> = {
    typeSpecifier: TypeSpecifier;
    typeQualifier: TypeQualifier;
    name: Name;
    params: Params;
}

export type InitDeclaratorList = {
}

export type SingleDeclaration<
    TypeSpecifier extends string = string,
    TypeQualifier extends string | void = string | void,
    Identifier extends string | void = string | void
> = {
    typeSpecifier: TypeSpecifier;
    typeQualifier: TypeQualifier;
    identifier: Identifier;
}

export type EmptyExpressionStatement = {}

export type AssignmentExpression<
    Operator extends string = string,
    Right extends Expression = Expression,
    Left extends Expression = Expression
> = {
    operator: Operator;
    right: Right;
    left: Left;
}

// selection statement

export type IfStatement<
    Test extends Expression[] = Expression[],
    Consequent extends BlockStatement = BlockStatement,
    Alternate extends BlockStatement | void = void
> = {
    brand: 'IfStatement';
    test: Test;
    consequent: Consequent;
    alternate: Alternate;
}

// compound statement no new scope

export type BlockStatement<Body = any> = {
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
    | UpdateExpression<boolean, string, any>
    | ConditionalExpression<any, any, any>
    | Identifier
    | BoolLiteral
    | IntLiteral
    | FloatLiteral

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
    Property extends Identifier = Identifier,
> = {
    brand: 'MemberExpression';
    object: Object;
    property: Property;
}

export type UpdateExpression<
    Prefix extends boolean = boolean,
    Operator extends string = string,
    Argument extends Expression = Expression,
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

export type BoolLiteral<Text extends string = string> = {
    brand: 'BoolLiteral';
    text: Text;
}

export type IntLiteral<Text extends string = string> = {
    brand: 'IntLiteral';
    text: Text;
}

export type FloatLiteral<Text extends string = string> = {
    brand: 'FloatLiteral';
    text: Text;
}
