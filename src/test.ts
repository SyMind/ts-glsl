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
void main() {
    gl_Position = vec4(position, 1.0);
    uv = position.xy;
}
`>

type StatementList1 = ParseStatementList<`
attribute vec3 position;
varying vec2 uv;
void main() {
    gl_Position = vec4(position, 1.0);
    uv = position.xy;
}
`>

type SimpleStatement1 = ParseSimpleStatement<`
attribute vec3 position;
varying vec2 uv;
void main() {
    gl_Position = vec4(position, 1.0);
    uv = position.xy;
}
`>

type DeclarationStatement1 = ParseDeclarationStatement<`
attribute vec3 position;
varying vec2 uv;
void main() {
    gl_Position = vec4(position, 1.0);
    uv = position.xy;
}
`>

type Declaration1 = ParseSingleDeclaration<'attribute vec3 position'>;

type Type1 = ParseFullySpecifiedType<'attribute vec3 position'>;

type Qualifier1 = ParseTypeQualifier<'attribute'>;
type Qualifier2 = ParseTypeQualifier<'invariant varying'>;
type Qualifier3 = ParseTypeQualifier<'attribute '>;

type Specifier1 = ParseTypeSpecifier<'vec3 position'>;
