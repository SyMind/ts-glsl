export type Trim<T> = T extends ` ${infer Rest}`
    ? Trim<Rest>
    : T extends `\n${infer Rest}`
        ? Trim<Rest>
        : T;

type A = Trim<' a'>
type B = Trim<'\na'>
