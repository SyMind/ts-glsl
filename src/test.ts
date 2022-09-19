import {expectTypeOf} from 'expect-type'
import {TrimLeft, TrimRight, Trim} from './string'
import {
    Parse,
    ParseSimpleStatement,
    ParseSingleDeclaration,
    ParseDeclarationStatement,
    ParseFullySpecifiedType,
    ParseTypeQualifier,
    ParseTypeSpecifier,
    ParseCompoundStatementWithScope,
    ParseFunctionHeader,
    ParseSelectionStatement,
    ParseStatementWithScope,
    ParsePostfixExpression,
    ParseUnaryExpression,
    ParseMultiplicativeExpression,
    ParseAdditiveExpression,
    ParseShiftExpression,
    ParseRelationalExpression,
    ParseIdentifier,
    ParseFloatConstant,
    ParseBoolConstant,
    ParseIntConstant,
    ParseParameterDeclaration,
    ParseParameterDeclarator,
    ParseProgramBody
} from '.'
import {
    BlockStatement,
    IfStatement,
    SingleDeclaration,
    MemberExpression,
    UpdateExpression,
    BinaryExpression,
    Identifier,
    BoolLiteral,
    IntLiteral,
    FloatLiteral,
    ParameterDeclaration
} from './ast'
import {GetProgramAttributes} from './helper'

// utils

expectTypeOf<TrimLeft<' a'>>().toMatchTypeOf<'a'>()
expectTypeOf<TrimLeft<'\na'>>().toMatchTypeOf<'a'>()

expectTypeOf<TrimRight<'a\n'>>().toMatchTypeOf<'a'>()
expectTypeOf<TrimRight<'a '>>().toMatchTypeOf<'a'>()

expectTypeOf<Trim<'\na\n'>>().toMatchTypeOf<'a'>()
expectTypeOf<Trim<' a '>>().toMatchTypeOf<'a'>()

// parser

expectTypeOf<ParseIdentifier<'foo'>>().toMatchTypeOf<[Identifier<'foo'>, '']>()
expectTypeOf<ParseIdentifier<'foo1'>>().toMatchTypeOf<[Identifier<'foo1'>, '']>()
expectTypeOf<ParseIdentifier<'foo+'>>().toMatchTypeOf<[Identifier<'foo'>, '+']>()
expectTypeOf<ParseIdentifier<'attribute'>>().toMatchTypeOf<never>()

type Program1 = ParseProgramBody<`
attribute vec3 position;
varying vec2 uv;
void main() {
    gl_Position = vec4(position, 1.0);
    uv = position.xy;
}
`>
type Program2 = Parse<`
attribute vec3 position;
`>

type StatementWithScope1 = ParseCompoundStatementWithScope<`
{
    attribute vec3 position;
}
`>

type StatementList1 = ParseSimpleStatement<`
attribute vec3 position;
varying vec2 uv;
`>

expectTypeOf<ParseDeclarationStatement<'attribute vec3 position;'>>().toMatchTypeOf<[SingleDeclaration<'vec3', 'attribute', 'position'>, '']>()

expectTypeOf<ParseSingleDeclaration<'attribute vec3 position'>>().toMatchTypeOf<{typeSpecifier: 'vec3', typeQualifier: 'attribute', identifier: 'position'}>()
expectTypeOf<ParseSingleDeclaration<'vec3 position'>>().toMatchTypeOf<{typeSpecifier: 'vec3', typeQualifier: void, identifier: 'position'}>()

expectTypeOf<ParseFullySpecifiedType<'attribute vec3'>>().toMatchTypeOf<[{typeSpecifier: 'vec3', typeQualifier: 'attribute'}, '']>()
expectTypeOf<ParseFullySpecifiedType<'vec3'>>().toMatchTypeOf<[{typeSpecifier: 'vec3'}, '']>()

expectTypeOf<ParseTypeQualifier<'attribute'>>().toMatchTypeOf<['attribute', '']>()
expectTypeOf<ParseTypeQualifier<'invariant varying'>>().toMatchTypeOf<['invariant varying', '']>()

expectTypeOf<ParseTypeSpecifier<'vec3 position'>>().toMatchTypeOf<['vec3', 'position']>()

expectTypeOf<ParseFunctionHeader<'void main()'>>().toMatchTypeOf<[{name: Identifier<'main'>, typeSpecifier: 'void', typeQualifier: void}, ')']>()

type FunctionHeader2 = ParseFunctionHeader<`
float rand(const in vec2 uv) {
}
`>;

expectTypeOf<ParseParameterDeclaration<'const in vec2 uv'>>().toMatchTypeOf<[ParameterDeclaration<'const', 'in', 'vec2', Identifier<'uv'>>, '']>()

expectTypeOf<ParseParameterDeclarator<'vec2 uv'>>().toMatchTypeOf<[{typeSpecifer: 'vec2', name: Identifier<'uv'>}, '']>()

expectTypeOf<ParseSelectionStatement<'if (foo > 0) {}'>>().toMatchTypeOf<[IfStatement<[BinaryExpression<'>', Identifier<'foo'>, IntLiteral<'0'>>], BlockStatement<[]>, void>, '']>()

expectTypeOf<ParseRelationalExpression<'foo > 0'>>().toMatchTypeOf<[BinaryExpression<'>', Identifier<'foo'>, IntLiteral<'0'>>, '']>()

expectTypeOf<ParseStatementWithScope<'{}'>>().toMatchTypeOf<[BlockStatement<[]>, '']>()

expectTypeOf<ParsePostfixExpression<'foo++'>>().toMatchTypeOf<[UpdateExpression<false, '++', Identifier<'foo'>>, '']>()
expectTypeOf<ParsePostfixExpression<'foo--'>>().toMatchTypeOf<[UpdateExpression<false, '--', Identifier<'foo'>>, '']>()
expectTypeOf<ParsePostfixExpression<'foo.a'>>().toMatchTypeOf<[MemberExpression<'foo', Identifier<'a'>>, '']>()

expectTypeOf<ParseUnaryExpression<'++foo'>>().toMatchTypeOf<[UpdateExpression<true, '++', Identifier<'foo'>>, '']>()
expectTypeOf<ParseUnaryExpression<'foo > 0'>>().toMatchTypeOf<[Identifier<'foo'>, ' > 0']>()

expectTypeOf<ParseMultiplicativeExpression<'a*b'>>().toMatchTypeOf<[BinaryExpression<'*', Identifier<'a'>, Identifier<'b'>>, '']>()

expectTypeOf<ParseAdditiveExpression<'a+b'>>().toMatchTypeOf<[BinaryExpression<'+', Identifier<'a'>, Identifier<'b'>>, '']>()

expectTypeOf<ParseShiftExpression<'a>>b'>>().toMatchTypeOf<[BinaryExpression<'>>', Identifier<'a'>, Identifier<'b'>>, '']>()

expectTypeOf<ParseRelationalExpression<'a>=b'>>().toMatchTypeOf<[BinaryExpression<'>=', Identifier<'a'>, Identifier<'b'>>, '']>()

expectTypeOf<ParseBoolConstant<'true'>>().toMatchTypeOf<[BoolLiteral<'true'>, '']>()
expectTypeOf<ParseBoolConstant<'false'>>().toMatchTypeOf<[BoolLiteral<'false'>, '']>()

expectTypeOf<ParseIntConstant<'123'>>().toMatchTypeOf<[IntLiteral<'123'>, '']>()

expectTypeOf<ParseFloatConstant<'123.0'>>().toMatchTypeOf<[FloatLiteral<'123.0'>, '']>()

// helper

const code = `
attribute vec3 position;
varying vec2 uv;
void main() {
    gl_Position = vec4(position, 1.0);
    uv = position.xy;
}
`

type Program = Parse<typeof code>;

expectTypeOf<GetProgramAttributes<Program>>().toMatchTypeOf<{position: 'vec3'}>()
