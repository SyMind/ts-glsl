import {Trim} from './string'
import {
    Program,
    SingleDeclaration,
    FullySpecifiedType,
    EmptyExpressionStatement,
    ParameterDeclarator,
    FunctionHeader,
    FunctionPrototype,
    ContinueStatement,
    BreakStatement,
    ReturnStatement,
    DiscardStatement,
    WhileStatement,
    DoWhileStatement,
    BlockStatement,
    ForStatement,
    IfStatement,
    MemberExpression,
    UpdateExpression,
    BinaryExpression
} from './ast'

type ScanIdentifier<T> =
    Trim<T> extends `${infer Token},${infer Tail}` ? [Token, Tail] :
    Trim<T> extends `${infer Token}(${infer Tail}` ? [Token, Tail] :
    Trim<T> extends `${infer Token})${infer Tail}` ? [Token, Tail] :
    Trim<T> extends `${infer Token};${infer Tail}` ? [Token, Tail] :
    Trim<T> extends `${infer Token})` ? [Token, ')'] :
    Trim<T> extends `${infer Token};` ? [Token, ';'] :
    Trim<T> extends `${infer Token} ${infer Tail}` ? [Token, Tail] :
    [Trim<T>, '']

export type Parse<T> = ParseStatementList<T> extends [...infer Result]
    ? Program<Result>
    : never

/*
statement_list :
    statement_no_new_scope
    statement_list statement_no_new_scope
*/
export type ParseStatementList<T> = ParseStatementNoNewScope<T> extends [infer Statement, infer Rest]
    ?  Trim<Rest> extends ''
        ? [Statement]
        : ParseStatementList<Rest> extends [...infer StatementList]
            ? [Statement, ...StatementList]
            : never
    : never

/*
statement_no_new_scope :
    compound_statement_with_scope
    simple_statement
*/
type ParseStatementNoNewScope<T> =
    | ParseCompoundStatementWithScope<T>
    | ParseSimpleStatement<T>

/*
compound_statement_with_scope:
    LEFT_BRACE RIGHT_BRACE
    LEFT_BRACE statement_list RIGHT_BRACE
*/
export type ParseCompoundStatementWithScope<T> = Trim<T> extends `{${infer SL}}${infer R}`
    ? Trim<SL> extends ''
        ? [BlockStatement<[]>, R]
        : ParseStatementList<SL> extends [...infer Result]
            ? [BlockStatement<Result>, R]
            : never
    : never

/*
simple_statement :
    declaration_statement
    expression_statement
    selection_statement
    iteration_statement
    jump_statement
*/
export type ParseSimpleStatement<T> =
    | ParseDeclarationStatement<T>
    | ParseExpressionStatement<T>
    | ParseSelectionStatement<T>
    | ParseIterationStatement<T>
    | ParseJumpStatement<T>

/*
expression_statement :
    SEMICOLON
    expression SEMICOLON
*/
type ParseExpressionStatement<T> = Trim<T> extends `${infer E};${infer R}`
    ? E extends ''
        ? [EmptyExpressionStatement, R]
        : ParseExpression<T>
    : never

/*
expression :
    assignment_expression
    expression COMMA assignment_expression
*/
type ParseExpression<T> = never

/*
assignment_expression :
    conditional_expression
    unary_expression assignment_operator assignment_expression
*/
type ParseAssignmentExpression<T> = never

/*
conditional_expression :
    logical_or_expression
    logical_or_expression QUESTION expression COLON assignment_expression
*/
type ParseConditionalExpression<T> = never

/*
logical_or_expression :
    logical_xor_expression
    logical_or_expression OR_OP logical_xor_expression
*/
type ParseLogicalOrExpression<T> = never

/*
logical_xor_expression :
    logical_and_expression
    logical_xor_expression XOR_OP logical_and_expression
*/
type ParseLogicalXorExpression<T> = never

/*
logical_and_expression :
    inclusive_or_expression
    logical_and_expression AND_OP inclusive_or_expression
*/
type ParseLogicalAndExpression<T> = never

/*
inclusive_or_expression :
    exclusive_or_expression
    inclusive_or_expression VERTICAL_BAR exclusive_or_expression
*/
type ParseInclusiveOrExpression<T> = never

/*
exclusive_or_expression :
    and_expression
    exclusive_or_expression CARET and_expression
*/
type ParseExclusiveOrExpression<T> = never

/*
and_expression :
    equality_expression
    and_expression AMPERSAND equality_expression
*/
type ParseAndExpression<T> = never

/*
equality_expression :
    relational_expression
    equality_expression EQ_OP relational_expression
    equality_expression NE_OP relational_expression
*/
type ParseEqualityExpression<T> = never

/*
relational_expression :
    shift_expression
    relational_expression LEFT_ANGLE shift_expression
    relational_expression RIGHT_ANGLE shift_expression
    relational_expression LE_OP shift_expression
    relational_expression GE_OP shift_expression
*/
type ParseRelationalExpression<T> = never

/*
shift_expression :
    additive_expression
    shift_expression LEFT_OP additive_expression
    shift_expression RIGHT_OP additive_expression
*/
type ParseShiftExpression<T> = never

/*
additive_expression :
    multiplicative_expression
    additive_expression PLUS multiplicative_expression
    additive_expression DASH multiplicative_expression
*/
export type ParseAdditiveExpression<T> = Trim<T> extends `${infer A}+${infer R1}`
    ? [ParseAdditiveExpression<A>] extends [[infer A, infer R2]]
        ? Trim<R2> extends ''
            ? [ParseMultiplicativeExpression<R1>] extends [[infer M, infer R3]]
                ? [BinaryExpression<'+', A, M>, R3]
                : never
            : never
        : never
    : Trim<T> extends `${infer A}-${infer R1}`
        ? [ParseAdditiveExpression<A>] extends [[infer A, infer R2]]
            ? Trim<R2> extends ''
                ? [ParseMultiplicativeExpression<R1>] extends [[infer M, infer R3]]
                    ? [BinaryExpression<'-', A, M>, R3]
                    : never
                : never
            : never
        : ParseMultiplicativeExpression<T>

/*
multiplicative_expression :
    unary_expression
    multiplicative_expression STAR unary_expression
    multiplicative_expression SLASH unary_expression
    multiplicative_expression PERCENT unary_expression
*/
export type ParseMultiplicativeExpression<T> = Trim<T> extends `${infer M}*${infer R1}`
    ? [ParseMultiplicativeExpression<M>] extends [[infer M, infer R2]]
        ? Trim<R2> extends ''
            ? [ParseUnaryExpression<R1>] extends [[infer U, infer R3]]
                ? [BinaryExpression<'*', M, U>, R3]
                : never
            : never
        : never
    : Trim<T> extends `${infer M}/${infer R1}`
        ? [ParseMultiplicativeExpression<M>] extends [[infer M, infer R2]]
            ? Trim<R2> extends ''
                ? [ParseUnaryExpression<R1>] extends [[infer U, infer R3]]
                    ? [BinaryExpression<'/', M, U>, R3]
                    : never
                : never
            : never
        : Trim<T> extends `${infer M}%${infer R1}`
            ? [ParseMultiplicativeExpression<M>] extends [[infer M, infer R2]]
                ? Trim<R2> extends ''
                    ? [ParseUnaryExpression<R1>] extends [[infer U, infer R3]]
                        ? [BinaryExpression<'%', M, U>, R3]
                        : never
                    : never
                : never
            : ParseUnaryExpression<T>

/*
unary_expression:
    postfix_expression
    INC_OP unary_expression
    DEC_OP unary_expression
    unary_operator unary_expression
*/
export type ParseUnaryExpression<T> = Trim<T> extends `++${infer R}`
    ? [ParseUnaryExpression<R>] extends [[infer E, infer R]]
        ? [UpdateExpression<true, '++', E>, R]
        : never
    : Trim<T> extends `--${infer U}`
        ? [ParseUnaryExpression<U>] extends [[infer E, infer R]]
            ? [UpdateExpression<true, '--', E>, R]
            : never
        : Trim<T> extends `${infer O} ${infer R}`
            ? O extends UnaryOperator
                ? [ParseUnaryExpression<R>] extends [[infer E, infer R]]
                    ? [UpdateExpression<true, '--', E>, R]
                    : never
                : never
            : ParsePostfixExpression<T>

/*
unary_operator :
    PLUS
    DASH
    BANG
    TILDE
*/
type UnaryOperator = '+' | '-' | '!' | '~';

/*
postfix_expression:
    primary_expression
    postfix_expression LEFT_BRACKET integer_expression RIGHT_BRACKET function_call
    postfix_expression DOT FIELD_SELECTION
    postfix_expression INC_OP
    postfix_expression DEC_OP
*/
export type ParsePostfixExpression<T> = Trim<T> extends `${infer P}[${infer I}]${infer R}`
    ? 1
    : Trim<T> extends `${infer P}.${infer F}`
        ? ScanIdentifier<F> extends [infer I, infer R]
            ? I extends ''
                ? never
                : [MemberExpression<P, I>, R]
            : never
        : Trim<T> extends `${infer P}++${infer R1}`
            ? ParsePostfixExpression<P> extends [infer P, infer R2]
                ? Trim<R2> extends ''
                    ? [UpdateExpression<false, '++', P>, R1]
                    : never
                : never
            :  Trim<T> extends `${infer P}--${infer R1}`
                ? ParsePostfixExpression<P> extends [infer P, infer R2]
                    ? Trim<R2> extends ''
                        ? [UpdateExpression<false, '--', P>, R1]
                        : never
                    : never
                : ParsePrimaryExpression<T>


/*
primary_expression:
    variable_identifier
    INTCONSTANT
    FLOATCONSTANT
    BOOLCONSTANT
    LEFT_PAREN expression RIGHT_PAREN
*/
type ParsePrimaryExpression<T> = Trim<T> extends `(${infer E})${infer R}`
    ? [ParseExpression<E>, R]
    : ScanIdentifier<T>

/*
variable_identifier :
    IDENTIFIER
*/
type ParseVariableIdentifier<T> = ScanIdentifier<T>

/*
selection_statement :
    IF LEFT_PAREN expression RIGHT_PAREN selection_rest_statement
*/
export type ParseSelectionStatement<T> = Trim<T> extends `if${infer R}`
    ? Trim<R> extends `(${infer E})${infer R}`
        ? [ParseExpression<E>] extends [[infer E, '']]
            ? [ParseSelectionRestStatement<R>] extends [[infer S, infer R]]
                ? S extends {consequent: infer C, alternate: infer A}
                    ? [IfStatement<E, C, A>, R]
                    :  S extends {consequent: infer C}
                        ? [IfStatement<E, C>, R]
                        : never
                : never
            : never
        : never
    : never

/*
selection_rest_statement :
    statement_with_scope ELSE statement_with_scope
    statement_with_scope
*/
export type ParseSelectionRestStatement<T> = Trim<T> extends `${infer S1}else${infer S2}`
    ? [ParseStatementWithScope<S1>] extends [infer S1, '']
        ? [ParseStatementWithScope<S2>] extends [infer S2, infer R]
            ? [{consequent: S1, alternate: S2}, R]
            : never
        : never
    : [ParseStatementWithScope<T>] extends [[infer S, infer R]]
        ? [{consequent: S}, R]
        : never

/*
iteration_statement :
    WHILE LEFT_PAREN condition RIGHT_PAREN statement_no_new_scope
    DO statement_with_scope WHILE LEFT_PAREN expression RIGHT_PAREN SEMICOLON
    FOR LEFT_PAREN for_init_statement for_rest_statement RIGHT_PAREN statement_no_new_scope
*/
export type ParseIterationStatement<T> = T extends `${infer K} ${infer R}`
    ? K extends 'while'
        ? Trim<R> extends `(${infer C})${infer R}`
            ? [ParseCondition<C>] extends [[infer C, '']]
                ? [ParseStatementNoNewScope<R>] extends [[infer S, infer R]]
                    ? [WhileStatement<C, S>, R]
                    : never
                : never
            : never
        : K extends 'do'
            ? Trim<R> extends `${infer S}while(${infer E});${infer R}`
                ? [ParseStatementWithScope<S>] extends [[infer S, '']]
                    ? [ParseExpression<E>] extends [[infer E, '']]
                        ? DoWhileStatement<E, S>
                        : never
                    : never
                : never
            : K extends 'for'
                ? Trim<R> extends `(${infer F})${infer R1}`
                    ? [ParseForInitStatement<F>] extends [[infer FI, infer R2]]
                        ? [ParseForRestStatement<R2>] extends [[infer FR extends {test: any, update: any}, '']]
                            ? [ParseStatementNoNewScope<R1>] extends [[infer S, infer R3]]
                                ? ForStatement<FI, FR['test'], FR['update'], S>
                                : never
                            : never
                        : never
                    : never
                : never
    : never

/*
statement_with_scope :
    compound_statement_no_new_scope
    simple_statement
*/
export type ParseStatementWithScope<T> =
    | ParseCompoundStatementNoNewScope<T>
    | ParseSimpleStatement<T>

/*
compound_statement_no_new_scope :
    LEFT_BRACE RIGHT_BRACE
    LEFT_BRACE statement_list RIGHT_BRACE
*/
type ParseCompoundStatementNoNewScope<T> = Trim<T> extends `{${infer S}}${infer R}`
    ? Trim<S> extends ''
        ? [BlockStatement<[]>, R]
        : [ParseStatementList<S>] extends [[infer S, '']]
            ? [BlockStatement<S>, R]
            : never
    : never

/*
for_init_statement :
    expression_statement
    declaration_statement
*/
export type ParseForInitStatement<T> =
    | ParseExpressionStatement<T>
    | ParseDeclarationStatement<T>

/*
for_rest_statement :
    conditionopt SEMICOLON
    conditionopt SEMICOLON expression
*/
export type ParseForRestStatement<T> = T extends `${infer C};${infer R}`
    ? [ParseConditionopt<C>] extends [[infer C, '']]
        ? [ParseExpression<R>] extends [[infer E, infer R]]
            ? [{test: C, update: E}, R]
            : never
        : never
    : never

/*
conditionopt :
    condition
    * empty *
*/
export type ParseConditionopt<T> = Trim<T> extends ''
    ? [undefined, '']
    : [ParseCondition<T>] extends [[infer C, '']]
        ? [C, '']
        : never

/*
condition :
    expression
    fully_specified_type IDENTIFIER EQUAL initializer
*/
type ParseCondition<T> =
    | ParseFullySpecifiedType<T>
    | ParseExpression<T>

/*
jump_statement :
    continue;
    break;
    return;
    return expression;
    discard;
*/
type ParseJumpStatement<T> = T extends `${infer J};${infer R}`
    ? Trim<J> extends 'continue'
        ? [ContinueStatement, R]
        : Trim<J> extends 'break'
            ? [BreakStatement, R]
            : Trim<J> extends 'return'
                ? [ReturnStatement, R]
                : Trim<J> extends 'discard'
                    ? [DiscardStatement, R]
                    : never
    : T extends `${infer J} ${infer R}`
        ? Trim<J> extends 'return'
            ? [ParseExpression<R>] extends [[infer E, infer R]]
                ? [ReturnStatement<E>, R]
                : never
            : never
        : never

/*
declaration_statement :
    declaration

declaration :
    function_prototype SEMICOLON
    init_declarator_list SEMICOLON

    // TODO
    PRECISION precision_qualifier type_specifier_no_prec SEMICOLON
*/
export type ParseDeclarationStatement<T> = Trim<T> extends `${infer Statement};${infer Rest}`
    ? ParseFunctionPrototype<Statement> extends never
        ? ParseInitDeclaratorList<Statement> extends never
            ? never
            : [ParseInitDeclaratorList<Statement>, Rest]
        : [ParseFunctionPrototype<Statement>, Rest]
    : never

/*
function_prototype :
    function_declarator RIGHT_PAREN
*/
export type ParseFunctionPrototype<T> = T extends `${infer F})${infer R}`
    ? ParseFunctionDeclarator<F> extends [infer Declarator, infer Rest]
        ? Trim<Rest> extends ''
            ? ParseParameterDeclaration<Rest>
            : void
        : void
    : void

/*
function_declarator :
    function_header
    function_header_with_parameters
*/
type ParseFunctionDeclarator<T> = ParseFunctionHeader<T> extends [infer Header, infer Rest]
    ? Trim<Rest> extends ''
        ? FunctionPrototype<Header>
        : void
    : void

/*
function_header_with_parameters :
    function_header parameter_declaration
    function_header_with_parameters COMMA parameter_declaration
*/
/*
export type ParseStatementList<T> = ParseStatementNoNewScope<T> extends [infer Statement, infer Rest]
    ?  Trim<Rest> extends ''
        ? [Statement]
        : ParseStatementList<Rest> extends [...infer StatementList]
            ? [Statement, ...StatementList]
            : never
    : never
*/
type ParseFunctionHeaderWithParameters<T> = ParseFunctionHeader<T> extends [infer Header, infer Rest]
    ? ParseParameterDeclaration<Rest>
    : never

/*
function_header : fully_specified_type IDENTIFIER LEFT_PAREN
*/
export type ParseFunctionHeader<T> = ParseFullySpecifiedType<T> extends [infer Type, infer Rest]
    ? Rest extends `${infer I}(${infer R}`
        ? [FunctionHeader<Type, I>, R]
        : void
    : void

/*
parameter_declaration :
    type_qualifier parameter_qualifier parameter_declarator
    parameter_qualifier parameter_declarator
    type_qualifier parameter_qualifier parameter_type_specifier
    parameter_qualifier parameter_type_specifier
*/
export type ParseParameterDeclaration<T> = ParseTypeQualifier<T> extends [infer Qualifier, infer Rest]
    ? ParseParameterQualifier<Rest> extends [infer PQualifier, infer Rest]
        ? ParseParameterDeclarator<Rest> extends [infer PDeclarator, infer Rest]
            ? never // TODO
            : void
        : void
    : void

/*
parameter_qualifier :
    *empty*
    IN
    OUT
    INOUT
*/
type ParameterQualifier =
    | 'in'
    | 'out'
    | 'inout'

export type ParseParameterQualifier<T> = T extends `${infer Q} ${infer R}`
    ? Q extends ParameterQualifier
        ? [Q, R]
        : ['', R]
    : ['', T]

/*
parameter_declarator :
    type_specifier IDENTIFIER

    // TODO
    type_specifier IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET
*/
export type ParseParameterDeclarator<T> = ParseTypeSpecifier<T> extends [infer Specifer, infer Rest]
    ? ScanIdentifier<Rest> extends [infer Identifier, infer Rest]
        ? [ParameterDeclarator<Specifer, Identifier>, Rest]
        : void
    : void

/*
init_declarator_list :
    single_declaration

    // TODO
    init_declarator_list COMMA IDENTIFIER
    init_declarator_list COMMA IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET
    init_declarator_list COMMA IDENTIFIER EQUAL initializer
*/
type ParseInitDeclaratorList<T> = ParseSingleDeclaration<T>

/*
single_declaration :
    fully_specified_type
    fully_specified_type IDENTIFIER

    // TODO
    fully_specified_type IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET
    fully_specified_type IDENTIFIER EQUAL initializer
    INVARIANT IDENTIFIER
*/
export type ParseSingleDeclaration<T> = ParseFullySpecifiedType<T> extends [infer Type, infer Rest]
    ? Trim<Rest> extends ''
        ? SingleDeclaration<Type>
        : Rest extends `${infer I}`
            ? SingleDeclaration<Type, I>
            : void
    : void

/*
fully_specified_type :
    type_specifier
    type_qualifier type_specifier
*/
export type ParseFullySpecifiedType<T> = ParseTypeQualifier<T> extends [infer Qualifier, infer Rest]
    ? ParseTypeSpecifier<Rest> extends [infer Specifer, infer Rest]
        ? [FullySpecifiedType<Specifer, Qualifier>, Rest]
        : void
    : ParseTypeSpecifier<T> extends [infer Specifer, infer Rest]
        ? [FullySpecifiedType<Specifer>, Rest]
        : void

/*
parameter_type_specifier:
    type_specifier
    type_specifier LEFT_BRACKET constant_expression RIGHT_BRACKET
*/
export type ParseParameterTypeSpecifier<T> = T extends `[${infer C}]${infer R}`
    ? [ParseConstantExpression<C>] extends [[infer ConstantExpression, infer Rest]]
        ? Rest extends ''
            ? [ConstantExpression, R]
            : never
        : never
    : ParseTypeSpecifier<T>

/*
constant_expression :
    conditional_expression
*/
export type ParseConstantExpression<T> = never

/*
type_specifier :
    VOID
    FLOAT
    INT
    UINT
    BOOL
    VEC2
    VEC3
    VEC4
    BVEC2
    BVEC3
    BVEC4
    IVEC2
    IVEC3
    IVEC4
    UVEC2
    UVEC3
    UVEC4
    MAT2
    MAT3
    MAT4
    MAT2X2
    MAT2X3
    MAT2X4
    MAT3X2
    MAT3X3
    MAT3X4
    MAT4X2
    MAT4X3
    MAT4X4
    SAMPLER2D
    SAMPLER3D
    SAMPLERCUBE
    SAMPLER2DSHADOW
    SAMPLERCUBESHADOW
    SAMPLER2DARRAY
    SAMPLER2DARRAYSHADOW
    ISAMPLER2D
    ISAMPLER3D
    ISAMPLERCUBE
    ISAMPLER2DARRAY
    USAMPLER2D
    USAMPLER3D
    USAMPLERCUBE
    USAMPLER2DARRAY
    struct_specifier
    TYPE_NAME
*/
export type ParseTypeSpecifier<T> = Trim<T> extends `${infer S} ${infer R}`
    ? [S, R]
    : Trim<T> extends `${infer S}`
        ? [S, '']
        : never

/*
type_qualifier:
    CONST
    ATTRIBUTE
    VARYING
    INVARIANT VARYING
    UNIFORM
*/
type TypeQualifier =
    |  'const'
    |  'attribute'
    |  'varying'
    |  'invariant varying'
    |  'uniform'

export type ParseTypeQualifier<T> = T extends `${infer Qualifier} ${infer Rest}`
    ? Trim<Qualifier> extends 'invariant'
        ? Rest extends `${infer V} ${infer R}`
            ? Trim<V> extends 'varying'
                ? ['invariant varying', R]
                : never
            : Trim<Rest> extends 'varying'
                ? ['invariant varying', '']
                : never
        : Trim<Qualifier> extends TypeQualifier
            ? [Trim<Qualifier>, Rest]
            : never
    : T extends `${infer Q}`
        ? Trim<Q> extends TypeQualifier
            ? [Trim<Q>, '']
            : never
        : never
