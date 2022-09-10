import {ParseSingleDeclaration, ParseFullySpecifiedType} from '.'

type Declaration1 = ParseSingleDeclaration<'attribute vec3 position'>;

type Type1 = ParseFullySpecifiedType<'attribute vec3 position'>;
