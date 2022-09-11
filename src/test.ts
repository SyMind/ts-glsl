import {
    Parse,
    ParseStatementList,
    ParseSimpleStatement,
    ParseSingleDeclaration,
    ParseDeclarationStatement,
    ParseFullySpecifiedType,
    ParseTypeQualifier,
    ParseTypeSpecifier
} from '.'

type Program1 = Parse<`
attribute vec3 position;
varying vec2 uv;
`>
type Program2 = Parse<`
attribute vec3 position;
vec2 uv;
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
// Wrong
type Qualifier4 = ParseTypeQualifier<'vec3 position'>;

type Specifier1 = ParseTypeSpecifier<'vec3 position'>;
