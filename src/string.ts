export type TrimLeft<T> = T extends ` ${infer R}`
    ? TrimLeft<R>
    : T extends `\n${infer R}`
        ? TrimLeft<R>
        : T;

export type TrimRight<T> = T extends `${infer R} `
    ? TrimRight<R>
    : T extends `${infer R}\n`
        ? TrimRight<R>
        : T;

export type Trim<T> = TrimRight<TrimLeft<T>>
