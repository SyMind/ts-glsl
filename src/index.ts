import {TrimLeft, TrimRight, Trim, Alphabet, NumberLiterals} from './string'
import {
    Program,
    Statement,
    SingleDeclaration,
    EmptyExpressionStatement,
    ParameterDeclarator,
    FunctionHeader,
    FunctionPrototype,
    ContinueStatement,
    BreakStatement,
    ReturnStatement,
    DiscardStatement,
    WhileStatement,
    DoWhileStatement,
    BlockStatement,
    ForStatement,
    IfStatement,
    MemberExpression,
    UpdateExpression,
    BinaryExpression,
    ConditionalExpression,
    AssignmentExpression,
    Expression,
    Identifier,
    BoolLiteral,
    IntLiteral,
    FloatLiteral
} from './ast'

export type Parse<T> = ParseStatementList<T> extends [...infer SL]
    ? SL extends Statement[]
        ? Program<SL>
        : never
    : never

type ScanIdentifierBeginning<T> = T extends `${infer F}${infer R}`
    ? F extends Alphabet
        ? [F, R]
        : ['', T]
    : ['', T]

type ScanIdentifierRest<T> = T extends `${infer I1}${infer R}`
    ? I1 extends (Alphabet | NumberLiterals | '_')
        ? ScanIdentifierRest<R> extends [infer I2, infer R]
            ? I2 extends ''
                ? [I1, R]
                : I2 extends string
                    ? [`${I1}${I2}`, R]
                    : [I1, R]
            : [I1, R]
        : ['', T]
    : ['', T]

type ScanIdentifier<T> = ScanIdentifierBeginning<TrimLeft<T>> extends [infer B, infer R]
    ? B extends ''
        ? ['', R]
        : B extends string
            ? ScanIdentifierRest<R> extends [infer I, infer R]
                ? I extends string
                    ? [`${B}${I}`, R]
                    : [B, R]
                : [B, R]
            : ['', R]
    : ['', T]

// [A-Za-z_][A-Za-z_0-9]*
export type ParseIdentifier<T> = ScanIdentifier<T> extends [infer I, infer R]
    ? I extends '' | TypeQualifier
        ? never
        : I extends string
            ? [Identifier<I>, R]
            : never
    : never

/*
statement_list :
    statement_no_new_scope
    statement_list statement_no_new_scope
*/
export type ParseStatementList<T> = ParseStatementNoNewScope<T> extends [infer S, infer R]
    ? S extends Statement
        ? TrimLeft<R> extends ''
            ? [S]
            : ParseStatementList<R> extends [...infer SL]
                ? SL extends Statement[]
                    ? [S, ...SL]
                    : never
                : never
        : never
    : never

/*
statement_no_new_scope :
    compound_statement_with_scope
    simple_statement
*/
type ParseStatementNoNewScope<T> =
    | ParseCompoundStatementWithScope<T>
    | ParseSimpleStatement<T>

/*
compound_statement_with_scope:
    LEFT_BRACE RIGHT_BRACE
    LEFT_BRACE statement_list RIGHT_BRACE
*/
export type ParseCompoundStatementWithScope<T> = TrimLeft<T> extends `{${infer SL}}${infer R}`
    ? TrimLeft<SL> extends ''
        ? [BlockStatement<[]>, R]
        : ParseStatementList<SL> extends [...infer SL]
            ? SL extends Statement[]
                ? [BlockStatement<SL>, R]
                : never
            : never
    : never

/*
simple_statement :
    declaration_statement
    expression_statement
    selection_statement
    iteration_statement
    jump_statement
*/
export type ParseSimpleStatement<T> =
    | ParseDeclarationStatement<T>
    | ParseExpressionStatement<T>
    | ParseSelectionStatement<T>
    | ParseIterationStatement<T>
    | ParseJumpStatement<T>

/*
expression_statement :
    SEMICOLON
    expression SEMICOLON
*/
type ParseExpressionStatement<T> = TrimLeft<T> extends `${infer E};${infer R}`
    ? E extends ''
        ? [EmptyExpressionStatement, R]
        : ParseExpression<T>
    : never

/*
expression :
    assignment_expression
    expression COMMA assignment_expression
*/
export type ParseExpression<T> = ParseAssignmentExpression<T> extends [infer A, infer R]
    ? A extends Expression
        ? TrimLeft<R> extends `,${infer R}`
            ? ParseExpression<R> extends [...infer E]
                ? E extends Expression[]
                    ? [A, ...E]
                    : never
                : never
            : [A]
        : never
    : never

/*
assignment_expression :
    conditional_expression
    unary_expression assignment_operator assignment_expression
*/
export type ParseAssignmentExpression<T> = ParseConditionalExpression<T> extends [infer C, infer R]
    ? C extends Expression
        ? [C, R]
        : ParseUnaryExpression<T> extends [infer U, infer R]
            ? U extends Expression
                ? ParseAssignmentOperator<R> extends [infer O, infer R]
                    ? O extends string
                        ? ParseAssignmentExpression<R> extends [infer A, infer R]
                            ? A extends Expression
                                ? [AssignmentExpression<'>>=', U, A>, R]
                                : never
                            : never
                        : never
                    : never
                : never
            : never
    : never 
    
/*
assignment_operator:
    EQUAL
    MUL_ASSIGN
    DIV_ASSIGN
    MOD_ASSIGN
    ADD_ASSIGN
    SUB_ASSIGN
    LEFT_ASSIGN
    RIGHT_ASSIGN
    AND_ASSIGN
    XOR_ASSIGN
    OR_ASSIGN
*/
export type ParseAssignmentOperator<T> = TrimLeft<T> extends `<<=${infer R}`
    ? ['<<=', R]
    : TrimLeft<T> extends `>>=${infer R}`
        ? ['>>=', R]
        : TrimLeft<T> extends `${infer F}${infer R}`
            ? F extends '='
                ? ['=', R]
                : F extends '*' | '/' | '%' | '+' | '-' | '&' | '^' | '|'
                    ? R extends `${infer A}${infer R}`
                        ? A extends '='
                            ? [`${F}${A}`, R]
                            : never
                        : never
                    : never
            : never

/*
conditional_expression :
    logical_or_expression
    logical_or_expression QUESTION expression COLON assignment_expression
*/
export type ParseConditionalExpression<T> = ParseLogicalOrExpression<T> extends [infer L, infer R]
    ? L extends Expression
        ? TrimLeft<R> extends `?${infer R}`
            ? ParseExpression<R> extends [infer E, infer R]
                ? E extends Expression
                    ? TrimLeft<R> extends `:${infer R}`
                        ? ParseAssignmentExpression<R> extends [infer A, infer R]
                            ? A extends Expression
                                ? [ConditionalExpression<L, E, A>, R]
                                : never
                            : never
                        : never
                    : never
                : never
            : [L, R]
        : never
    : never

/*
logical_or_expression :
    logical_xor_expression
    logical_or_expression OR_OP logical_xor_expression
*/
export type ParseLogicalOrExpression<T> = TrimLeft<T> extends `${infer O}||${infer R1}`
    ? ParseLogicalOrExpression<O> extends [infer O, infer R2]
        ? O extends Expression
            ? TrimLeft<R2> extends ''
                ? ParseLogicalXorExpression<R1> extends [infer L, infer R3]
                    ? L extends Expression
                        ? [BinaryExpression<'||', O, L>, R3]
                        : never
                    : never
                : never
            : never
        : never
    : ParseLogicalXorExpression<T>

/*
logical_xor_expression :
    logical_and_expression
    logical_xor_expression XOR_OP logical_and_expression
*/
export type ParseLogicalXorExpression<T> = TrimLeft<T> extends `${infer X}^^${infer R1}`
    ? ParseLogicalXorExpression<X> extends [infer X, infer R2]
        ? X extends Expression
            ? TrimLeft<R2> extends ''
                ? ParseLogicalAndExpression<R1> extends [infer L, infer R3]
                    ? L extends Expression
                        ? [BinaryExpression<'^^', X, L>, R3]
                        : never
                    : never
                : never
            : never
        : never
    : ParseLogicalAndExpression<T>

/*
logical_and_expression :
    inclusive_or_expression
    logical_and_expression AND_OP inclusive_or_expression
*/
export type ParseLogicalAndExpression<T> = TrimLeft<T> extends `${infer L}&&${infer R1}`
    ? ParseLogicalAndExpression<L> extends [infer L, infer R2]
        ? L extends Expression
            ? TrimLeft<R2> extends ''
                ? ParseInclusiveOrExpression<R1> extends [infer I, infer R3]
                    ? I extends Expression
                        ? [BinaryExpression<'&&', L, I>, R3]
                        : never
                    : never
                : never
            : never
        : never
    : ParseInclusiveOrExpression<T>

/*
inclusive_or_expression :
    exclusive_or_expression
    inclusive_or_expression VERTICAL_BAR exclusive_or_expression
*/
export type ParseInclusiveOrExpression<T> = TrimLeft<T> extends `${infer I}|${infer R1}`
    ? ParseInclusiveOrExpression<I> extends [infer I, infer R2]
        ? I extends Expression
            ? TrimLeft<R2> extends ''
                ? ParseExclusiveOrExpression<R1> extends [infer E, infer R3]
                    ? E extends Expression
                        ? [BinaryExpression<'|', I, E>, R3]
                        : never
                    : never
                : never
            : never
        : never
    : ParseExclusiveOrExpression<T>

/*
exclusive_or_expression :
    and_expression
    exclusive_or_expression CARET and_expression
*/
export type ParseExclusiveOrExpression<T> = TrimLeft<T> extends `${infer E}^${infer R1}`
    ? ParseExclusiveOrExpression<E> extends [infer E, infer R2]
        ? E extends Expression
            ? TrimLeft<R2> extends ''
                ? ParseAndExpression<R1> extends [infer A, infer R3]
                    ? A extends Expression
                        ? [BinaryExpression<'^', E, A>, R3]
                        : never
                    : never
                : never
            : never
        : never
    : ParseAndExpression<T>

/*
and_expression :
    equality_expression
    and_expression AMPERSAND equality_expression
*/
export type ParseAndExpression<T> = TrimLeft<T> extends `${infer A}&${infer R1}`
    ? ParseAndExpression<A> extends [infer A, infer R2]
        ? A extends Expression
            ? TrimLeft<R2> extends ''
                ? ParseEqualityExpression<R1> extends [infer E, infer R3]
                    ? E extends Expression
                        ? [BinaryExpression<'&', A, E>, R3]
                        : never
                    : never
                : never
            : never
        : never
    : ParseEqualityExpression<T>

/*
equality_expression :
    relational_expression
    equality_expression EQ_OP relational_expression
    equality_expression NE_OP relational_expression
*/
export type ParseEqualityExpression<T> = TrimLeft<T> extends `${infer E}==${infer R1}`
    ? ParseEqualityExpression<E> extends [infer E, infer R2]
        ? E extends Expression
            ? TrimLeft<R2> extends ''
                ? ParseRelationalExpression<R1> extends [infer R, infer R3]
                    ? R extends Expression
                        ? [BinaryExpression<'==', E, R>, R3]
                        : never
                    : never
                : never
            : never
        : never
    : TrimLeft<T> extends `${infer E}!=${infer R1}`
        ? ParseEqualityExpression<E> extends [infer E, infer R2]
            ? E extends Expression
                ? TrimLeft<R2> extends ''
                    ? ParseRelationalExpression<R1> extends [infer R, infer R3]
                        ? R extends Expression
                            ? [BinaryExpression<'!=', E, R>, R3]
                            : never
                        : never
                    : never
                : never
            : never
        : ParseRelationalExpression<T>

/*
relational_expression :
    shift_expression
    relational_expression LEFT_ANGLE shift_expression
    relational_expression RIGHT_ANGLE shift_expression
    relational_expression LE_OP shift_expression
    relational_expression GE_OP shift_expression
*/
export type ParseRelationalExpression<T> = TrimLeft<T> extends `${infer R}<=${infer R1}`
    ? ParseRelationalExpression<TrimRight<R>> extends [infer R, '']
        ? R extends Expression
            ? ParseShiftExpression<R1> extends [infer S, infer R2]
                ? S extends Expression
                    ? [BinaryExpression<'<=', R, S>, R2]
                    : never
                : never
            : never
        : never
    : TrimLeft<T> extends `${infer R}>=${infer R1}`
        ? ParseRelationalExpression<TrimRight<R>> extends [infer R, '']
            ? R extends Expression
                ? ParseShiftExpression<R1> extends [infer S, infer R2]
                    ? S extends Expression
                        ? [BinaryExpression<'>=', R, S>, R2]
                        : never
                    : never
                : never
            : never
        : TrimLeft<T> extends `${infer R}<${infer R1}`
            ? ParseRelationalExpression<TrimRight<R>> extends [infer R, '']
                ? R extends Expression
                    ? ParseShiftExpression<R1> extends [infer S, infer R2]
                        ? S extends Expression
                            ? [BinaryExpression<'<', R, S>, R2]
                            : never
                        : never
                    : never
                : never
            : TrimLeft<T> extends `${infer R}>${infer R1}`
                ? ParseRelationalExpression<TrimRight<R>> extends [infer R, '']
                    ? R extends Expression
                        ? ParseShiftExpression<R1> extends [infer S, infer R2]
                            ? S extends Expression
                                ? [BinaryExpression<'>', R, S>, R2]
                                : never
                            : never
                        : never
                    : never
                : ParseShiftExpression<T>

/*
shift_expression :
    additive_expression
    shift_expression LEFT_OP additive_expression
    shift_expression RIGHT_OP additive_expression
*/
export type ParseShiftExpression<T> = TrimLeft<T> extends `${infer S}<<${infer R1}`
    ? ParseShiftExpression<TrimRight<S>> extends [infer S, '']
        ? S extends Expression
            ? ParseAdditiveExpression<R1> extends [infer A, infer R2]
                ? A extends Expression
                    ? [BinaryExpression<'<<', S, A>, R2]
                    : never
                : never
            : never
        : never
    : TrimLeft<T> extends `${infer S}>>${infer R1}`
        ? ParseShiftExpression<TrimRight<S>> extends [infer S, '']
            ? S extends Expression
                ? ParseAdditiveExpression<R1> extends [infer A, infer R2]
                    ? A extends Expression
                        ? [BinaryExpression<'>>', S, A>, R2]
                        : never
                    : never
                : never
            : never
        : ParseAdditiveExpression<T>

/*
additive_expression :
    multiplicative_expression
    additive_expression PLUS multiplicative_expression
    additive_expression DASH multiplicative_expression
*/
export type ParseAdditiveExpression<T> = TrimLeft<T> extends `${infer A}+${infer R1}`
    ? ParseAdditiveExpression<TrimRight<A>> extends [infer A, infer R2]
        ? A extends Expression
            ? TrimLeft<R2> extends ''
                ? ParseMultiplicativeExpression<R1> extends [infer M, infer R3]
                    ? M extends Expression
                        ? [BinaryExpression<'+', A, M>, R3]
                        : never
                    : never
                : never
            : never
        : never
    : TrimLeft<T> extends `${infer A}-${infer R1}`
        ? ParseAdditiveExpression<TrimRight<A>> extends [infer A, infer R2]
            ? A extends Expression
                ? TrimLeft<R2> extends ''
                    ? ParseMultiplicativeExpression<R1> extends [infer M, infer R3]
                        ? M extends Expression
                            ? [BinaryExpression<'-', A, M>, R3]
                            : never
                        : never
                    : never
                : never
            : never
        : ParseMultiplicativeExpression<T>

/*
multiplicative_expression :
    unary_expression
    multiplicative_expression STAR unary_expression
    multiplicative_expression SLASH unary_expression
    multiplicative_expression PERCENT unary_expression
*/
export type ParseMultiplicativeExpression<T> = TrimLeft<T> extends `${infer M}*${infer R1}`
    ? ParseMultiplicativeExpression<M> extends [infer M, infer R2]
        ? M extends Expression
            ? TrimLeft<R2> extends ''
                ? ParseUnaryExpression<R1> extends [infer U, infer R3]
                    ? U extends Expression
                        ? [BinaryExpression<'*', M, U>, R3]
                        : never
                    : never
                : never
            : never
        : never
    : TrimLeft<T> extends `${infer M}/${infer R1}`
        ? ParseMultiplicativeExpression<M> extends [infer M, infer R2]
            ? M extends Expression
                ? TrimLeft<R2> extends ''
                    ? ParseUnaryExpression<R1> extends [infer U, infer R3]
                        ? U extends Expression
                            ? [BinaryExpression<'/', M, U>, R3]
                            : never
                        : never
                    : never
                : never
            : never
        : TrimLeft<T> extends `${infer M}%${infer R1}`
            ? ParseMultiplicativeExpression<M> extends [infer M, infer R2]
                ? M extends Expression
                    ? TrimLeft<R2> extends ''
                        ? ParseUnaryExpression<R1> extends [infer U, infer R3]
                            ? U extends Expression
                                ? [BinaryExpression<'%', M, U>, R3]
                                : never
                            : never
                        : never
                    : never
                : never
            : ParseUnaryExpression<T>

/*
unary_expression:
    postfix_expression
    INC_OP unary_expression
    DEC_OP unary_expression
    unary_operator unary_expression
*/
export type ParseUnaryExpression<T> = TrimLeft<T> extends `++${infer R}`
    ? ParseUnaryExpression<R> extends [infer E, infer R]
        ? E extends Expression
            ? [UpdateExpression<true, '++', E>, R]
            : never
        : never
    : TrimLeft<T> extends `--${infer U}`
        ? ParseUnaryExpression<U> extends [infer E, infer R]
            ? E extends Expression
                ? [UpdateExpression<true, '--', E>, R]
                : never
            : never
        : TrimLeft<T> extends `${infer O} ${infer R}`
            ? O extends UnaryOperator
                ? ParseUnaryExpression<R> extends [infer E, infer R]
                    ? E extends Expression
                        ? [UpdateExpression<true, '--', E>, R]
                        : never
                    : never
                : ParsePostfixExpression<T>
            : ParsePostfixExpression<T>

/*
unary_operator :
    PLUS
    DASH
    BANG
    TILDE
*/
type UnaryOperator = '+' | '-' | '!' | '~';

/*
postfix_expression:
    primary_expression
    postfix_expression LEFT_BRACKET integer_expression RIGHT_BRACKET
    function_call
    postfix_expression DOT FIELD_SELECTION
    postfix_expression INC_OP
    postfix_expression DEC_OP
*/
export type ParsePostfixExpression<T> = TrimLeft<T> extends `${infer P}[${infer I}]${infer R}`
    ? 1 // TODO
    : TrimLeft<T> extends `${infer P}.${infer F}`
        ? ParseIdentifier<F> extends [infer I, infer R] // TODO 优化
            ? I extends ''
                ? never
                : I extends Identifier
                    ? [MemberExpression<P, I>, R]
                    : never
            : never
        : TrimLeft<T> extends `${infer P}++${infer R1}`
            ? ParsePostfixExpression<P> extends [infer P, infer R2]
                ? P extends Expression
                    ? TrimLeft<R2> extends ''
                        ? [UpdateExpression<false, '++', P>, R1]
                        : never
                    : never
                : never
            :  TrimLeft<T> extends `${infer P}--${infer R1}`
                ? ParsePostfixExpression<P> extends [infer P, infer R2]
                    ? P extends Expression
                        ? TrimLeft<R2> extends ''
                            ? [UpdateExpression<false, '--', P>, R1]
                            : never
                        : never
                    : never
                : ParsePrimaryExpression<T>

export type ParseBoolConstant<T> = T extends `true${infer R}`
    ? [BoolLiteral<'true'>, R]
    : T extends `false${infer R}`
        ? [BoolLiteral<'false'>, R]
        : never

type ScanIntConstant<T> = TrimLeft<T> extends `${infer I1}${infer R}`
    ? I1 extends NumberLiterals
        ? ScanIdentifierRest<R> extends [infer I2, infer R]
            ? I2 extends ''
                ? [I1, R]
                : I2 extends string
                    ? [`${I1}${I2}`, R]
                    : [I1, R]
            : [I1, R]
        : ['', T]
    : ['', T]

export type ParseIntConstant<T> = ScanIntConstant<T> extends [infer I, infer R]
    ? I extends ''
        ? never
        : I extends string
            ? [IntLiteral<I>, R]
            : never
    : never

export type ParseFloatConstant<T> = ScanIntConstant<T> extends [infer I1, infer R]
    ? I1 extends ''
        ? never
        : I1 extends string
            ? R extends `.${infer R}`
                ? ScanIntConstant<R> extends [infer I2, infer R]
                    ? I2 extends ''
                        ? never
                        : I2 extends string
                            ? [FloatLiteral<`${I1}.${I2}`>, R]
                            : never
                    : never
                : never
            : never
    : never

/*
primary_expression:
    variable_identifier
    INTCONSTANT
    FLOATCONSTANT // TODO
    BOOLCONSTANT
    LEFT_PAREN expression RIGHT_PAREN
*/
export type ParsePrimaryExpression<T> = TrimLeft<T> extends `(${infer E})${infer R}`
    ? ParseExpression<TrimRight<E>> extends [infer E, '']
        ? E extends Expression
            ? [E, R]
            : never
        : never
    : ParseBoolConstant<T> | ParseIntConstant<T> | ParseVariableIdentifier<T>

/*
variable_identifier :
    IDENTIFIER
*/
type ParseVariableIdentifier<T> = ParseIdentifier<T>

/*
selection_statement :
    IF LEFT_PAREN expression RIGHT_PAREN selection_rest_statement
*/
export type ParseSelectionStatement<T> = TrimLeft<T> extends `if${infer R}`
    ? TrimLeft<R> extends `(${infer E})${infer R}`
        ? ParseExpression<Trim<E>> extends [...infer E]
            ? E extends Expression[]
                ? ParseSelectionRestStatement<R> extends [infer S, infer R]
                    ? S extends {consequent: infer C, alternate: infer A}
                        ? C extends BlockStatement
                            ? A extends BlockStatement | void
                                ? [IfStatement<E, C, A>, R]
                                : never
                            : never
                        : never
                    : never
                : never
            : never
        : never
    : never

/*
selection_rest_statement :
    statement_with_scope ELSE statement_with_scope
    statement_with_scope
*/
export type ParseSelectionRestStatement<T> = TrimLeft<T> extends `${infer S1}else${infer S2}`
    ? [ParseStatementWithScope<S1>] extends [infer S1, '']
        ? [ParseStatementWithScope<S2>] extends [infer S2, infer R]
            ? [{consequent: S1, alternate: S2}, R]
            : never
        : never
    : [ParseStatementWithScope<T>] extends [[infer S, infer R]]
        ? [{consequent: S, alternate: void}, R]
        : never

/*
iteration_statement :
    WHILE LEFT_PAREN condition RIGHT_PAREN statement_no_new_scope
    DO statement_with_scope WHILE LEFT_PAREN expression RIGHT_PAREN SEMICOLON
    FOR LEFT_PAREN for_init_statement for_rest_statement RIGHT_PAREN statement_no_new_scope
*/
export type ParseIterationStatement<T> = T extends `${infer K} ${infer R}`
    ? K extends 'while'
        ? TrimLeft<R> extends `(${infer C})${infer R}`
            ? [ParseCondition<C>] extends [[infer C, '']]
                ? [ParseStatementNoNewScope<R>] extends [[infer S, infer R]]
                    ? [WhileStatement<C, S>, R]
                    : never
                : never
            : never
        : K extends 'do'
            ? TrimLeft<R> extends `${infer S}while(${infer E});${infer R}`
                ? [ParseStatementWithScope<S>] extends [[infer S, '']]
                    ? [ParseExpression<E>] extends [[infer E, '']]
                        ? DoWhileStatement<E, S>
                        : never
                    : never
                : never
            : K extends 'for'
                ? TrimLeft<R> extends `(${infer F})${infer R1}`
                    ? [ParseForInitStatement<F>] extends [[infer FI, infer R2]]
                        ? [ParseForRestStatement<R2>] extends [[infer FR extends {test: any, update: any}, '']]
                            ? [ParseStatementNoNewScope<R1>] extends [[infer S, infer R3]]
                                ? ForStatement<FI, FR['test'], FR['update'], S>
                                : never
                            : never
                        : never
                    : never
                : never
    : never

/*
statement_with_scope :
    compound_statement_no_new_scope
    simple_statement
*/
export type ParseStatementWithScope<T> =
    | ParseCompoundStatementNoNewScope<T>
    | ParseSimpleStatement<T>

/*
compound_statement_no_new_scope :
    LEFT_BRACE RIGHT_BRACE
    LEFT_BRACE statement_list RIGHT_BRACE
*/
type ParseCompoundStatementNoNewScope<T> = TrimLeft<T> extends `{${infer S}}${infer R}`
    ? TrimLeft<S> extends ''
        ? [BlockStatement<[]>, R]
        : [ParseStatementList<S>] extends [[infer S, '']]
            ? [BlockStatement<S>, R]
            : never
    : never

/*
for_init_statement :
    expression_statement
    declaration_statement
*/
export type ParseForInitStatement<T> =
    | ParseExpressionStatement<T>
    | ParseDeclarationStatement<T>

/*
for_rest_statement :
    conditionopt SEMICOLON
    conditionopt SEMICOLON expression
*/
export type ParseForRestStatement<T> = T extends `${infer C};${infer R}`
    ? [ParseConditionopt<C>] extends [[infer C, '']]
        ? [ParseExpression<R>] extends [[infer E, infer R]]
            ? [{test: C, update: E}, R]
            : never
        : never
    : never

/*
conditionopt :
    condition
    * empty *
*/
export type ParseConditionopt<T> = TrimLeft<T> extends ''
    ? [undefined, '']
    : [ParseCondition<T>] extends [[infer C, '']]
        ? [C, '']
        : never

/*
condition :
    expression
    fully_specified_type IDENTIFIER EQUAL initializer
*/
type ParseCondition<T> =
    | ParseFullySpecifiedType<T>
    | ParseExpression<T>

/*
jump_statement :
    continue;
    break;
    return;
    return expression;
    discard;
*/
type ParseJumpStatement<T> = T extends `${infer J};${infer R}`
    ? TrimLeft<J> extends 'continue'
        ? [ContinueStatement, R]
        : TrimLeft<J> extends 'break'
            ? [BreakStatement, R]
            : TrimLeft<J> extends 'return'
                ? [ReturnStatement, R]
                : TrimLeft<J> extends 'discard'
                    ? [DiscardStatement, R]
                    : never
    : T extends `${infer J} ${infer R}`
        ? TrimLeft<J> extends 'return'
            ? ParseExpression<R> extends [infer E, infer R]
                ? E extends Expression
                    ? [ReturnStatement<E>, R]
                    : never
                : never
            : never
        : never

/*
declaration_statement :
    declaration

declaration :
    function_prototype SEMICOLON
    init_declarator_list SEMICOLON

    // TODO
    PRECISION precision_qualifier type_specifier_no_prec SEMICOLON
*/
export type ParseDeclarationStatement<T> = TrimLeft<T> extends `${infer Statement};${infer Rest}`
    ? ParseFunctionPrototype<Statement> extends never
        ? ParseInitDeclaratorList<Statement> extends never
            ? never
            : [ParseInitDeclaratorList<Statement>, Rest]
        : [ParseFunctionPrototype<Statement>, Rest]
    : never

/*
function_prototype :
    function_declarator RIGHT_PAREN
*/
export type ParseFunctionPrototype<T> = T extends `${infer F})${infer R}`
    ? ParseFunctionDeclarator<TrimRight<F>> extends [infer D, '']
        ? [D, R]
        : never
    : never

/*
function_declarator :
    function_header
    function_header_with_parameters
*/
type ParseFunctionDeclarator<T> = ParseFunctionHeader<T> extends [infer H, infer R]
    ? R extends ''
        ? FunctionPrototype<H>
        : never
    : never

/*
function_header_with_parameters :
    function_header parameter_declaration
    function_header_with_parameters COMMA parameter_declaration
*/
/*
export type ParseStatementList<T> = ParseStatementNoNewScope<T> extends [infer Statement, infer Rest]
    ?  TrimLeft<Rest> extends ''
        ? [Statement]
        : ParseStatementList<Rest> extends [...infer StatementList]
            ? [Statement, ...StatementList]
            : never
    : never
*/
type ParseFunctionHeaderWithParameters<T> = ParseFunctionHeader<T> extends [infer Header, infer Rest]
    ? ParseParameterDeclaration<Rest>
    : never

/*
function_header :
    fully_specified_type IDENTIFIER LEFT_PAREN
*/
export type ParseFunctionHeader<T> = ParseFullySpecifiedType<T> extends [infer T, infer R]
    ? T extends {typeSpecifier: infer S, typeQualifier: infer Q}
        ? R extends `${infer I}(${infer R}`
            ? [FunctionHeader<{name: I, typeSpecifier: S, typeQualifier: Q}, I>, R]
            : never
        : never
    : never

/*
parameter_declaration :
    type_qualifier parameter_qualifier parameter_declarator
    parameter_qualifier parameter_declarator
    type_qualifier parameter_qualifier parameter_type_specifier
    parameter_qualifier parameter_type_specifier
*/
export type ParseParameterDeclaration<T> = ParseTypeQualifier<T> extends [infer Qualifier, infer Rest]
    ? ParseParameterQualifier<Rest> extends [infer PQualifier, infer Rest]
        ? ParseParameterDeclarator<Rest> extends [infer PDeclarator, infer Rest]
            ? never // TODO
            : never
        : never
    : never

/*
parameter_qualifier :
    *empty*
    IN
    OUT
    INOUT
*/
type ParameterQualifier =
    | 'in'
    | 'out'
    | 'inout'

export type ParseParameterQualifier<T> = T extends `${infer Q} ${infer R}`
    ? Q extends ParameterQualifier
        ? [Q, R]
        : ['', R]
    : ['', T]

/*
parameter_declarator :
    type_specifier IDENTIFIER

    // TODO
    type_specifier IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET
*/
export type ParseParameterDeclarator<T> = ParseTypeSpecifier<T> extends [infer S, infer R]
    ? ParseIdentifier<R> extends [infer I, infer R]
        ? [ParameterDeclarator<S, I>, R]
        : never
    : never

/*
init_declarator_list :
    single_declaration

    // TODO
    init_declarator_list COMMA IDENTIFIER
    init_declarator_list COMMA IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET
    init_declarator_list COMMA IDENTIFIER EQUAL initializer
*/
type ParseInitDeclaratorList<T> = ParseSingleDeclaration<T>

/*
single_declaration :
    fully_specified_type
    fully_specified_type IDENTIFIER

    // TODO
    fully_specified_type IDENTIFIER LEFT_BRACKET constant_expression RIGHT_BRACKET
    fully_specified_type IDENTIFIER EQUAL initializer
    INVARIANT IDENTIFIER
*/
export type ParseSingleDeclaration<T> = ParseFullySpecifiedType<T> extends [infer T, infer R]
    ? TrimLeft<R> extends ''
        ? T extends {typeSpecifier: infer S, typeQualifier: infer Q}
            ? S extends string
                ? Q extends string | void
                    ? SingleDeclaration<S, Q, void>
                    : never
                : never
            : never
        : TrimLeft<R> extends `${infer I}`
            ? T extends {typeSpecifier: infer S, typeQualifier: infer Q}
                ? S extends string
                    ? Q extends string | void
                        ? SingleDeclaration<S, Q, I>
                        : never
                    : never
                : never
            : never
    : never

/*
fully_specified_type :
    type_specifier
    type_qualifier type_specifier
*/
export type ParseFullySpecifiedType<T> = ParseTypeQualifier<T> extends [infer Q, infer R]
    ? Q extends string
        ? ParseTypeSpecifier<R> extends [infer S, infer R]
            ? [{typeSpecifier: S, typeQualifier: Q}, R]
            : never
        : ParseTypeSpecifier<T> extends [infer S, infer R]
            ? S extends string
                ? [{typeSpecifier: S, typeQualifier: void}, R]
                : never
            : never
    : never

/*
parameter_type_specifier:
    type_specifier
    type_specifier LEFT_BRACKET constant_expression RIGHT_BRACKET
*/
export type ParseParameterTypeSpecifier<T> = T extends `[${infer C}]${infer R}`
    ? ParseConstantExpression<C> extends [infer ConstantExpression, infer Rest]
        ? Rest extends ''
            ? [ConstantExpression, R]
            : never
        : never
    : ParseTypeSpecifier<T>

/*
constant_expression :
    conditional_expression
*/
export type ParseConstantExpression<T> = never

/*
type_specifier :
    VOID
    FLOAT
    INT
    UINT
    BOOL
    VEC2
    VEC3
    VEC4
    BVEC2
    BVEC3
    BVEC4
    IVEC2
    IVEC3
    IVEC4
    UVEC2
    UVEC3
    UVEC4
    MAT2
    MAT3
    MAT4
    MAT2X2
    MAT2X3
    MAT2X4
    MAT3X2
    MAT3X3
    MAT3X4
    MAT4X2
    MAT4X3
    MAT4X4
    SAMPLER2D
    SAMPLER3D
    SAMPLERCUBE
    SAMPLER2DSHADOW
    SAMPLERCUBESHADOW
    SAMPLER2DARRAY
    SAMPLER2DARRAYSHADOW
    ISAMPLER2D
    ISAMPLER3D
    ISAMPLERCUBE
    ISAMPLER2DARRAY
    USAMPLER2D
    USAMPLER3D
    USAMPLERCUBE
    USAMPLER2DARRAY
    struct_specifier
    TYPE_NAME
*/
export type ParseTypeSpecifier<T> = TrimLeft<T> extends `${infer S} ${infer R}`
    ? [S, R]
    : TrimLeft<T> extends `${infer S}`
        ? [S, '']
        : never

/*
type_qualifier:
    CONST
    ATTRIBUTE
    VARYING
    INVARIANT VARYING
    UNIFORM
*/
type TypeQualifier =
    |  'const'
    |  'attribute'
    |  'varying'
    |  'invariant varying'
    |  'uniform'

export type ParseTypeQualifier<T> = T extends `${infer Qualifier} ${infer Rest}`
    ? TrimLeft<Qualifier> extends 'invariant'
        ? Rest extends `${infer V} ${infer R}`
            ? TrimLeft<V> extends 'varying'
                ? ['invariant varying', R]
                : never
            : TrimLeft<Rest> extends 'varying'
                ? ['invariant varying', '']
                : never
        : TrimLeft<Qualifier> extends TypeQualifier
            ? [TrimLeft<Qualifier>, Rest]
            : never
    : T extends `${infer Q}`
        ? TrimLeft<Q> extends TypeQualifier
            ? [TrimLeft<Q>, '']
            : never
        : never

/*
function_definition :
    function_prototype compound_statement_no_new_scope
*/
// export type ParseFunctionDefinition<T> = ParseFunctionPrototype<T> extends []
