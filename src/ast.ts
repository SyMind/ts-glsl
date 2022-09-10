type SingleDeclaration<
    Type extends FullySpecifiedType,
    Identifier = undefined
> = {
    specifier: Type['specifier'];
    qualifier: Type['qualifier'];
    identifier: Identifier;
}

interface FullySpecifiedType<
    Specifier = string,
    Qualifier = undefined
> {
    specifier: Specifier;
    qualifier: Qualifier;
}
