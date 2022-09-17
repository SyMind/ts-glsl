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

export type Alphabet =
    | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'
    | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z'

export type NumberLiterals = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
