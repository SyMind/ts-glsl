import {Trim} from './string'
import {
    Program,
    CompoundStatementWithScope,
    SingleDeclaration,
    FullySpecifiedType,
    EmptyExpressionStatement,
    ParameterDeclarator,
    FunctionHeader
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
        ? [CompoundStatementWithScope<[]>, R]
        : ParseStatementList<SL> extends [...infer Result]
            ? [CompoundStatementWithScope<Result>, R]
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
type ParseAdditiveExpression<T> = never

/*
multiplicative_expression :
    unary_expression
    multiplicative_expression STAR unary_expression
    multiplicative_expression SLASH unary_expression
    multiplicative_expression PERCENT unary_expression
*/
type ParseMultiplicativeExpression<T> = never

/*
unary_expression:
    postfix_expression
    INC_OP unary_expression
    DEC_OP unary_expression
    unary_operator unary_expression
*/
type ParseUnaryExpression<T> = never

/*
postfix_expression:
    primary_expression
    postfix_expression LEFT_BRACKET integer_expression RIGHT_BRACKET function_call
    postfix_expression DOT FIELD_SELECTION
    postfix_expression INC_OP
    postfix_expression DEC_OP
*/
type ParsePostfixExpression<T> = never

/*
primary_expression:
    variable_identifier
    INTCONSTANT
    FLOATCONSTANT
    BOOLCONSTANT
    LEFT_PAREN expression RIGHT_PAREN
*/
type ParsePrimaryExpression<T> = Trim<T> extends `(${infer E})`
    ? ParseExpression<E>
    : never

/*
variable_identifier: IDENTIFIER
*/
type ParseVariableIdentifier<T> = never

// TODO
type ParseSelectionStatement<T> = never

// TODO
type ParseIterationStatement<T> = never

// TODO
type ParseJumpStatement<T> = never

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
export type ParseFunctionPrototype<T> = ParseFunctionDeclarator<T> extends [infer Declarator, infer Rest]
    ? Rest extends `)${infer Rest}`
        ? [Declarator, Rest]
        : void
    : void

/*
function_declarator :
    function_header
    function_header_with_parameters
*/
type ParseFunctionDeclarator<T> = ParseFunctionHeader<T>

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
export type ParseParameterDeclaration<T> = never
// export type ParseParameterDeclaration<T> = ParseTypeQualifier<T> extends [infer Qualifier, infer Rest]
//     ? Qualifier extends never
//         ? never
//         : ParseParameterQualifier<Rest> extends [infer ParamQualifier, infer Rest]
//             ? ParamQualifier extends never
//                 ? never
//                 : [ParamQualifier, ]
//     : never

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

    // TODO
    type_specifier LEFT_BRACKET constant_expression RIGHT_BRACKET
*/
export type ParseParameterTypeSpecifier<T> = ParseTypeSpecifier<T>

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
        : void

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
                : void
            : Trim<Rest> extends 'varying'
                ? ['invariant varying', '']
                : void
        : Trim<Qualifier> extends TypeQualifier
            ? [Trim<Qualifier>, Rest]
            : void
    : T extends `${infer Q}`
        ? Trim<Q> extends TypeQualifier
            ? [Trim<Q>, '']
            : void
        : void

