# Concrete slices 0–11 token ballot

Closed on 2026-11-27 by unanimous agreement of Claude, Codex and agy under the owner's delegation. The [decision record](../decisions.md#decided-2026-11-27-token-ballot) gives the verdicts and the separate E2 stable-key decision. The [compatibility draft](README.md) checks the cumulative grammar.

## Reservation at the pre-slice-2 lock

The lexer reserves every keyword whose construct and spelling are decided. Each later slice adds productions using this vocabulary. Words with undecided spellings stay available. The first consumer determines when a keyword can appear in a production. [Shared word classifier](../../ConcreteSpec/Spec/GrammarVocab.lean)

| First consumer | Reserved words |
|---|---|
| 0 | `fn`, `let`, `return`, `u8` |
| 1 | `struct`, `move`, `_` (reserved identifier for the wildcard) |
| 2 | `i8`, `i16`, `i32`, `i64`, `u16`, `u32`, `u64`, `type` (builtin target marker) |
| 3 | `if`, `else`, `mut`, `true`, `false`, `bool`, `Unit` |
| 4 | `exact`, `while`, `from`, `until`, `for`, `break`, `continue` |
| 6 | `Copy`, `enum`, `match` |
| 7 | `borrow` |
| 8 | `pub`, `const`, `mod`, `import` |
| 9 | `trait`, `impl`, `defer`, `Self`, `cap` |
| 9 capability arguments, 11 grants | `Fs`, `Network`, `Env`, `Time`, `Alloc`, `Device`, `Console`, `Process`, `Unsafe`, `Entropy` |
| 10 | `spec`, `ghost`, `assert`, `assume`, `by` |
| 11 | `with`, `char` |
| 16–17 | `trusted`, `extern`, `kept` |

`PRIMITIVE` groups the Q1 keyword tokens for type and qualified-call productions. The earlier `u8` terminal also starts a qualified call. Non-primitive builtin types such as `Option`, `Result`, `Heap`, `Vec` and `String` retain their identifier tokens. Local binders may shadow neither a builtin type, `main`, nor an intrinsic. Name resolution uses the shared binder rule and intrinsic catalog. The names `math`, `self` and `as` remain available as identifiers. [Q1 and Q3](../decisions.md#decided-2026-11-27-token-ballot) · [Builtin catalog clarification](../decisions.md#decided-2026-11-24-builtin-names)

## Builtin names by first consumer

The lexer returns an identifier for each intrinsic. The binder rule prevents local redeclaration. The [November 24 clarification](../decisions.md#decided-2026-11-24-builtin-names) retains `byte_at` and releases `alloc_array`, `free_array`, `vec_pop` and `string_length`.

| First consumer | Intrinsic names |
|---|---|
| 1 explicit terminal abort | `abort` |
| 2 wrapping, saturation and conversions | `widen`, `wrapping_add`, `wrapping_sub`, `wrapping_mul`, `saturating_add`, `saturating_sub`, `saturating_mul`, `truncate`, `reinterpret` |
| 3 explicit discard | `discard` |
| 6 checked integer results and fallible conversion | `checked_add`, `checked_sub`, `checked_mul`, `checked_div`, `checked_rem`, `checked_shl`, `checked_shr`, `checked_neg`, `try_convert` |
| 9 destruction | `destroy` |
| 11 UTF-8 byte access | `byte_at` |
| Later allocation and owned operations | `alloc`, `read_owned` |
| Later layout queries, argument builtin and float operations | `sizeof`, `alignof`, `get_args`, `trunc`, `round`, `floor`, `ceil` |
| Later owned-string operations | `string_concat`, `string_slice`, `string_substr`, `string_trim`, `int_to_string`, `bool_to_string`, `float_to_string`, `string_push_char`, `string_append`, `string_append_int`, `string_append_bool`, `string_reserve` |

The shared `builtinTypeNames` catalog defines the local binder reservations for types and traits. It includes the primitive keywords. Slice 6 uses `Copy`, `Option`, `Result`, `ArithmeticError` and `OutOfRange`. Slice 9 uses the `Destroy` trait, and slice 11 uses `StaticStr`, `StdIn`, `StdOut` and `StdErr`. Later consumers use `Heap`, `HeapArray`, `Vec`, `String`, `f32` and `f64`. File and directory types acquire entries when their spellings are decided. [Builtin type and trait catalog](../../ConcreteSpec/Spec/Surface0.lean) · [Vertical slices](../../ROADMAP.md#vertical-slices)

## Decided forms

| First slice | Form | Rule |
|---|---|---|
| 1 | `let _: u8 = x;` | The typed wildcard consumes its own reserved identifier token. |
| 2 | `widen(type: u32, x)`, `wrapping_add(type: u32, x, y)` | Every target-typed numeric intrinsic takes the target first. This includes wrapping, saturating and checked operations, `truncate`, `reinterpret` and `try_convert` when each becomes available. Intrinsic names lex as identifiers. |
| 2 | `~x` | Bitwise complement. Slice 3 adds boolean negation `!x`. |
| 4 | `#[name]`, `#[name(args)]` | One envelope for all attributes. Each listed attribute's argument and attachment rules still apply. |
| 4 | `exact { x + 1 }` | Exact arithmetic in proof expressions. The keyword requires a braced operand and can start a condition head directly, as in `if exact { x + 1 } > x { … }`. |
| 6 | `match x { 0 => a, _ => b, }` | Every arm consumes its own comma. |
| 7 | `borrow mut v = a from 0 until n { … }` | Both `mut` and the whole range are optional. The binder always precedes the place. |
| 8 | `import a::b::{x, y};` | The item list is nonempty. Use sites remain qualified, for example `b::x`. |

## Other punctuation and boundaries

| First consumer | Form | Use |
|---|---|---|
| 0 | `( )`, `{ }`, `->`, `:`, `=`, `;`, `+` | Grouping, blocks, return types, annotations, initializers, statement ends and checked addition. |
| 2 | `,`, `-`, `*`, `/`, `%`, `&`, `\|`, `^`, `<<`, `>>` | Numeric arguments, signed arithmetic, bitwise operations and shifts. |
| 3 | `==`, `!=`, `<`, `<=`, `>`, `>=`, `&&`, `\|\|`, `!` | Comparisons and boolean operations. A trailing expression has no semicolon. [OD42](../decisions.md#od42) |
| 4 | `'outer:`, `break 'outer;`, `continue 'outer;` | A colon follows a label at its declaration. |
| 6 | `.`, `::`, `=>`, `\|`, `..` | Field access, qualified paths, arm bodies, pattern alternatives and an explicitly ignored variant payload. [OD25](../decisions.md#od25) · [OD128](../decisions.md#od128) |
| 7 | `&`, `&mut`, `*`, `[ ]` | Shared and exclusive references, dereferences, array literals, types and indexing. The exclusive form takes two tokens. |
| 9 | `<…>`, `::<…>`, `cap C`, `+`, `{}` | Declaration and type arguments use angle brackets. Expression generics require the turbofish. Maximal munch preserves `x>>y`, and the parser splits nested generic closers. Type arguments precede capabilities. [OD87](../decisions.md#od87) · [OD88](../decisions.md#od88) · [Generic closers](../decisions.md#recovered-59-34) |
| 10 | `spec fn … = e;`, `assert(e) by lemma(args);`, `assume e;` | Proof declarations and statements. |
| 11 | `with(...)` | Authority clause before the return arrow. Resource operations use ordinary calls. |

The draft checks the statement boundary of a body-leading `match`, including its trailing-value continuation and the shared unary operators. It also checks attribute attachment, struct invariants and counted-loop restrictions. [Boundary controls](README.md) · [OD104](../decisions.md#od104)

## Slice 2 literals

The draft scanner exercises these decisions from slice 2. The production lexer currently refuses
`1_000` and `'\xff'`. R-0722 owns their implementation. [Slice 2 literal work](../../ROADMAP.md#r-0722)

Digit separators may occur between digits, including in nondecimal integers. `0xff_ff` is accepted. Uppercase hex digits, `0x_ff`, adjacent separators and trailing separators are refused. A suffix supplies a type only where the surrounding expression supplies none. `255u8` is redundant in a `u8` return or initializer. [Suffix typing](../decisions.md#decided-2026-11-08-language-literals)

UTF-8 strings admit `\u{...}` for Unicode scalar values. They refuse `\u{d800}`, values above U+10FFFF and `\x41`. Byte literals admit `'\x00'` through `'\xff'`. All hexadecimal digits, including digits in escapes, are lowercase.
