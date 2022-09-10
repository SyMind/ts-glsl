import {Trim} from './string'

/*
statement_list :
    statement
    statement_list statement
*/
type Parse<T> = ParseStatement<T> extends [infer Statement, infer Rest]
    ? Trim<Rest> extends infer R
        ? R extends ''
            ? [Statement]
            : [Statement, Parse<R>]
        : never
    : never

/*
statement :
    compound_statement
    simple_statement
*/
type ParseStatement<T> = ParseCompoundStatement<T> extends [infer Statement, infer Rest]
    ? [Statement, Rest]
    : ParseSimpleStatement<T>

type ParseCompoundStatement<T> = ''

/*
simple_statement :
    declaration_statement
    expression_statement
    selection_statement
    switch_statement
    case_label
    iteration_statement
    jump_statement
*/
type ParseSimpleStatement<T> = ParseDeclarationStatement<T>

/*
declaration_statement :
    declaration

declaration :
    function_prototype SEMICOLON
    init_declarator_list SEMICOLON
    PRECISION precision_qualifier type_specifier SEMICOLON
    type_qualifier IDENTIFIER LEFT_BRACE struct_declaration_list RIGHT_BRACE SEMICOLON type_qualifier IDENTIFIER LEFT_BRACE struct_declaration_list RIGHT_BRACE IDENTIFIER SEMICOLON
    type_qualifier IDENTIFIER LEFT_BRACE struct_declaration_list RIGHT_BRACE IDENTIFIER array_specifier SEMICOLON
    type_qualifier SEMICOLON
    type_qualifier IDENTIFIER SEMICOLON
    type_qualifier IDENTIFIER identifier_list SEMICOLON
*/
type ParseDeclarationStatement<T> = T extends `${infer Statement};${infer Rest}`
    ? ParseInitDeclaratorList<Statement> extends infer Result
        ? Result extends never
            ? never
            : [Result, Rest]
        : never
    : never

/*
init_declarator_list :
    single_declaration
    init_declarator_list COMMA IDENTIFIER
    init_declarator_list COMMA IDENTIFIER array_specifier init_declarator_list COMMA IDENTIFIER array_specifier EQUAL initializer init_declarator_list COMMA IDENTIFIER EQUAL initializer
*/
type ParseInitDeclaratorList<T> = ParseSingleDeclaration<T>

/*
single_declaration :
    fully_specified_type
    fully_specified_type IDENTIFIER
    fully_specified_type IDENTIFIER array_specifier fully_specified_type IDENTIFIER array_specifier EQUAL initializer fully_specified_type IDENTIFIER EQUAL initializer
*/
type ParseSingleDeclaration<T> = ParseFullySpecifiedType<T>

/*
fully_specified_type :
    type_specifier
    type_qualifier type_specifier
*/
type ParseFullySpecifiedType<T> = ParseTypeSpecifier<T>

/*
type_specifier :
    type_specifier_nonarray
    type_specifier_nonarray array_specifier
*/
type ParseTypeSpecifier<T> = T extends `${infer type}${infer array}`
    ? ParseTypeSpecifierNonarray<T>
    : never

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
    DOUBLE
    INT
    UINT
    BOOL
    VEC2
    VEC3
    VEC4
    DVEC2
    DVEC3
    DVEC4
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
    DMAT2
    DMAT3
    DMAT4
    DMAT2X2
    DMAT2X3
    DMAT2X4
    DMAT3X2
    DMAT3X3
    DMAT3X4
    DMAT4X2
    DMAT4X3
    DMAT4X4
    ATOMIC_UINT
    SAMPLER2D
    SAMPLER3D
    SAMPLERCUBE
    SAMPLER2DSHADOW
    SAMPLERCUBESHADOW
    SAMPLER2DARRAY
    SAMPLER2DARRAYSHADOW
    SAMPLERCUBEARRAY
    SAMPLERCUBEARRAYSHADOW
    ISAMPLER2D
    ISAMPLER3D
    ISAMPLERCUBE
    ISAMPLER2DARRAY
    ISAMPLERCUBEARRAY
    USAMPLER2D
    USAMPLER3D
    USAMPLERCUBE
    USAMPLER2DARRAY
    USAMPLERCUBEARRAY
    SAMPLER1D
    SAMPLER1DSHADOW
    SAMPLER1DARRAY
    SAMPLER1DARRAYSHADOW
    ISAMPLER1D
    ISAMPLER1DARRAY
    USAMPLER1D
    USAMPLER1DARRAY
    SAMPLER2DRECT
    SAMPLER2DRECTSHADOW
    ISAMPLER2DRECT
    USAMPLER2DRECT
    SAMPLERBUFFER
    ISAMPLERBUFFER
    USAMPLERBUFFER
    SAMPLER2DMS
    ISAMPLER2DMS
    USAMPLER2DMS
    SAMPLER2DMSARRAY
    ISAMPLER2DMSARRAY
    USAMPLER2DMSARRAY
    IMAGE2D
    IIMAGE2D
    UIMAGE2D
    IMAGE3D
    IIMAGE3D
    UIMAGE3D
    IMAGECUBE
    IIMAGECUBE
    UIMAGECUBE
    IMAGEBUFFER
    IIMAGEBUFFER
    UIMAGEBUFFER
    IMAGE1D
    IIMAGE1D
    UIMAGE1D
    IMAGE1DARRAY
    IIMAGE1DARRAY
    UIMAGE1DARRAY
    IMAGE2DRECT
    IIMAGE2DRECT
    UIMAGE2DRECT
    IMAGE2DARRAY
    IIMAGE2DARRAY
    UIMAGE2DARRAY
    IMAGECUBEARRAY
    IIMAGECUBEARRAY
    UIMAGECUBEARRAY
    IMAGE2DMS
    IIMAGE2DMS
    UIMAGE2DMS
    IMAGE2DMSARRAY
    IIMAGE2DMSARRAY
    UIMAGE2DMSARRAY
    struct_specifier
    TYPE_NAME
*/
type TypeSpecifierNonarray = 
    | 'void'
    | 'float'
    | 'double'
    | 'int'
    | 'uint'
    | 'bool'
    | 'vec2'
    | 'vec3'
    | 'vec4'
    | 'dvec2'
    | 'dvec3'
    | 'dvec4'
    | 'bvec2'
    | 'bvec3'
    | 'bvec4'
    | 'ivec2'
    | 'ivec3'
    | 'ivec4'
    | 'uvec2'
    | 'uvec3'
    | 'uvec4'
    | 'mat2'
    | 'mat3'
    | 'mat4'
    | 'mat2x2'
    | 'mat2x3'
    | 'mat2x4'
    | 'mat3x2'
    | 'mat3x3'
    | 'mat3x4'
    | 'mat4x2'
    | 'mat4x3'
    | 'mat4x4'
    | 'dmat2'
    | 'dmat3'
    | 'dmat4'
    | 'dmat2x2'
    | 'dmat2x3'
    | 'dmat2x4'
    | 'dmat3x2'
    | 'dmat3x3'
    | 'dmat3x4'
    | 'dmat4x2'
    | 'dmat4x3'
    | 'dmat4x4'
    | 'atomic_uint'
    | 'sampler2D'
    | 'sampler3D'
    | 'samplerCube'

type ParseTypeSpecifierNonarray<T> = T extends TypeSpecifierNonarray ? T : never

// Example

const glsl = `attribute vec3 position;`

type Program = Parse<typeof glsl>;

type A = ParseStatement<typeof glsl>

type B = true extends [infer Statement, infer Rest] ? true : false
