import {expectTypeOf} from 'expect-type'
import {
    Parse,
    ParseStatementList,
    ParseSimpleStatement,
    ParseSingleDeclaration,
    ParseDeclarationStatement,
    ParseFullySpecifiedType,
    ParseTypeQualifier,
    ParseTypeSpecifier,
    ParseCompoundStatementWithScope,
    ParseFunctionHeader,
    ParseFunctionPrototype,
    ParseSelectionStatement,
    ParseStatementWithScope,
    ParsePostfixExpression,
    ParseUnaryExpression,
    ParseMultiplicativeExpression,
    ParseAdditiveExpression,
    ParseShiftExpression
} from '.'
import {BlockStatement, MemberExpression, UpdateExpression, BinaryExpression} from './ast'

type Program1 = Parse<`
attribute vec3 position;
varying vec2 uv;
`>
type Program2 = Parse<`
attribute vec3 position;
vec2 uv;
`>

type statementWithScope1 = ParseCompoundStatementWithScope<`
{
    attribute vec3 position;
    vec2 uv;
}
`>

type StatementList1 = ParseStatementList<`
attribute vec3 position;
varying vec2 uv;
`>

type SimpleStatement1 = ParseSimpleStatement<`
attribute vec3 position;
varying vec2 uv;
`>

type DeclarationStatement1 = ParseDeclarationStatement<`
attribute vec3 position;
varying vec2 uv;
`>

type Declaration1 = ParseSingleDeclaration<'attribute vec3 position'>;
type Declaration2 = ParseSingleDeclaration<'vec3 position'>;

type Type1 = ParseFullySpecifiedType<'attribute vec3 position'>;
type Type2 = ParseFullySpecifiedType<'vec3 position'>;

type Qualifier1 = ParseTypeQualifier<'attribute'>;
type Qualifier2 = ParseTypeQualifier<'invariant varying'>;
type Qualifier3 = ParseTypeQualifier<'attribute '>;

type Specifier1 = ParseTypeSpecifier<'vec3 position'>;

type FunctionHeader1 = ParseFunctionHeader<`
void main() {
    gl_Position = vec4(position, 1.0);
    uv = position.xy;
}
`>;
type FunctionHeader2 = ParseFunctionHeader<`
float rand(const in vec2 uv) {
}
`>;

type FunctionPrototype1 = ParseFunctionPrototype<'void main();'>;

type ForStatement1 = ParseSelectionStatement<'if (foo > 0) {}'>;
// expectTypeOf<ParseSelectionStatement<'if (foo > 0) {}'>>().toMatchTypeOf<[BlockStatement<[]>, '']>()

expectTypeOf<ParseStatementWithScope<'{}'>>().toMatchTypeOf<[BlockStatement<[]>, '']>()

expectTypeOf<ParsePostfixExpression<'foo++'>>().toMatchTypeOf<[UpdateExpression<false, "++", "foo">, '']>()
expectTypeOf<ParsePostfixExpression<'foo--'>>().toMatchTypeOf<[UpdateExpression<false, "--", "foo">, '']>()
expectTypeOf<ParsePostfixExpression<'foo.a'>>().toMatchTypeOf<[MemberExpression<'foo', 'a'>, '']>()

expectTypeOf<ParseUnaryExpression<'++foo'>>().toMatchTypeOf<[UpdateExpression<true, "++", "foo">, '']>()

expectTypeOf<ParseMultiplicativeExpression<'a*b'>>().toMatchTypeOf<[BinaryExpression<'*', 'a', 'b'>, '']>()

expectTypeOf<ParseAdditiveExpression<'a+b'>>().toMatchTypeOf<[BinaryExpression<'+', 'a', 'b'>, '']>()

expectTypeOf<ParseShiftExpression<'a>>b'>>().toMatchTypeOf<[BinaryExpression<'>>', 'a', 'b'>, '']>()
