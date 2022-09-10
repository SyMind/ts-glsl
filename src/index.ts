import {Trim} from './string'

/*
statement_list :
    statement_no_new_scope
    statement_list statement_no_new_scope
*/
type Parse<T> = ParseStatementNoNewScope<T> extends [infer Statement, infer Tail]
    ? Trim<Tail> extends infer R
        ? R extends ''
            ? [Statement]
            : [Statement, Parse<R>]
        : never
    : never

/*
statement_no_new_scope :
    compound_statement_with_scope
    simple_statement
*/
type ParseStatementNoNewScope<T> = ParseCompoundStatementWithScope<T> extends [infer Result, infer Tail]
    ? [Result, Tail]
    : ParseSimpleStatement<T>

// TODO
type ParseCompoundStatementWithScope<T> = ''

/*
simple_statement :
    declaration_statement
    expression_statement
    selection_statement
    iteration_statement
    jump_statement
*/
type ParseSimpleStatement<T> = ParseDeclarationStatement<T> extends [infer Result, infer Tail]
    ? [Result, Tail]
    : ParseExpressionStatement<T> extends [infer Result, infer Tail]
        ? [Result, Tail]
        : ParseSelectionStatement<T> extends [infer Result, infer Tail]
            ? [Result, Tail]
            : ParseIterationStatement<T> extends [infer Result, infer Tail]
                ? [Result, Tail]
                : ParseJumpStatement<T>

// TODO
type ParseExpressionStatement<T> = ''

// TODO
type ParseSelectionStatement<T> = ''

// TODO
type ParseIterationStatement<T> = ''

// TODO
type ParseJumpStatement<T> = ''

/*
declaration_statement :
    declaration

declaration :
    function_prototype SEMICOLON
    init_declarator_list SEMICOLON

    // TODO
    PRECISION precision_qualifier type_specifier_no_prec SEMICOLON
*/
type ParseDeclarationStatement<T> = T extends `${infer Statement};${infer Tail}`
    ? ParseFunctionPrototype<Statement> extends never
        ? [ParseInitDeclaratorList<Statement>, Tail]
        : ParseFunctionPrototype<Statement>
    : never

// TODO
type ParseFunctionPrototype<T> = ''

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
export type ParseSingleDeclaration<T> = ParseFullySpecifiedType<T> extends [infer Type, infer Rest1]
    ? Trim<Rest1> extends ''
        ? SingleDeclaration<Type>
        : Rest1 extends `${infer Identifier}`
            ? SingleDeclaration<Type, Identifier>
            : never
    : never

/*
fully_specified_type :
    type_specifier
    type_qualifier type_specifier
*/
export type ParseFullySpecifiedType<T> = T extends `${infer Qualifier} ${infer Rest1}`
    ? ParseTypeQualifier<Qualifier> extends never
        ? never
        : ParseTypeSpecifier<Rest1> extends [infer Type, infer Rest2]
            ? [FullySpecifiedType<Type, Qualifier>, Rest2]
            : never
    : ParseTypeSpecifier<T> extends never
        ? never
        : ParseTypeSpecifier<T> extends never
            ? never
            : [FullySpecifiedType<ParseTypeSpecifier<T>>, '']

/*
type_specifier :
    type_specifier_nonarray
    type_specifier_nonarray array_specifier
*/
export type ParseTypeSpecifier<T> = T extends `${infer TypeSpecifier} ${infer Rest}`
    ? [ParseTypeSpecifierNonarray<TypeSpecifier>, Rest]
    : ParseTypeSpecifierNonarray<T> extends never
        ? never
        : [ParseTypeSpecifierNonarray<T>, '']

/*
array_specifier :
    LEFT_BRACKET RIGHT_BRACKET
    LEFT_BRACKET conditional_expression RIGHT_BRACKET
    array_specifier LEFT_BRACKET RIGHT_BRACKET
    array_specifier LEFT_BRACKET conditional_expression RIGHT_BRACKET
*/
type ParseArraySpecifier<T> = T extends '[]' ? true : false

/*
type_specifier_nonarray :
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
type ParseTypeSpecifierNonarray<T> = T

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

type ParseTypeQualifier<T> = T extends TypeQualifier ? T : never
