import {Trim} from './string'
import {
    Program,
    StatementList,
    CompoundStatementWithScope,
    SingleDeclaration,
    FullySpecifiedType
} from './ast'

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
type ParseCompoundStatementWithScope<T> = T extends `{${infer SL}}`
    ? Trim<SL> extends ''
        ? CompoundStatementWithScope<[]>
        : Parse<SL> extends [...infer Result]
            ? CompoundStatementWithScope<Result & StatementList>
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

// TODO
type ParseExpressionStatement<T> = never

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

// TODO
type ParseFunctionPrototype<T> = never

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
            : never
    : never

/*
fully_specified_type :
    type_specifier
    type_qualifier type_specifier
*/
export type ParseFullySpecifiedType<T> = ParseTypeQualifier<T> extends [infer Qualifier, infer Rest]
    ? Qualifier extends string
        ? ParseTypeSpecifier<Rest> extends [infer Specifer, infer Rest]
            ? [FullySpecifiedType<Specifer, Qualifier>, Rest]
            : never
        : ParseTypeSpecifier<T> extends [infer Specifer, infer Rest]
            ? Specifer extends string
                ? [FullySpecifiedType<Specifer>, Rest]
                : never
            : never
    : never

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

