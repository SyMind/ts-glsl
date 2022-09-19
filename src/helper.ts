import {Program, SingleDeclaration} from './ast'

export type GetProgramAttributes<T> = T extends Program
    ? GetProgramAttributesInternal<T['body']>
    : 1

type GetProgramAttributesInternal<T> = T extends [infer S, ...infer R]
? S extends SingleDeclaration
    ? S['typeQualifier'] extends 'attribute'
        ? S['identifier'] extends string
            ? R extends []
                ? {[key in S['identifier']]: S['typeSpecifier']}
                : GetProgramAttributes<R> extends infer R
                    ? R extends {[key in string]: string}
                        ? Pick<R & {[key in S['identifier']]: S['typeSpecifier']}, keyof R | S['identifier']>
                        : {[key in S['identifier']]: S['typeSpecifier']}
                    : never
            : never
        : GetProgramAttributes<R>
    : GetProgramAttributes<R>
: {}
