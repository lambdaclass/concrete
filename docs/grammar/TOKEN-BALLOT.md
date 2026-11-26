# Concrete slices 0–11 token ballot

Options for the owner under E1. Approvals are staged by slice. The [compatibility
draft](README.md) identifies the checked token shapes and the remaining approval work.

## Keyword ballot and existing dispositions

Use one lexical classification in each staged revision. A decided keyword is
reserved everywhere, including before its first accepted production. The table
separates reservation from admission. A proposed keyword slot is instantiated
only inside a draft projection. `main`, `self`, `as`, attribute names and builtin
function names remain identifier tokens. Builtin-name collisions are checked
during name resolution. `in`, `newtype`, `union` and `total` are dropped
reservations. [Removed words](../decisions.md#decided-2026-11-08-removed-words) ·
[OD43](../decisions.md#od43) · [OD153](../decisions.md#od153)

| First consumer | Exact words | Disposition |
|---|---|---|
| 0 function, let, return, type | `fn`, `let`, `return`, `u8` | Reserved on main. |
| 1 empty linear struct and named move | `struct`, `move` | Already tokenized on main. Q8.2 and the November 25 delegation cover the slice 1 forms. |
| 2 integer types | `i8`, `i16`, `i32`, `i64`, `u16`, `u32`, `u64` | Q1 below. |
| 2 builtin target-type candidate | `type` | Already reserved on main. E4 option B would first use it here. |
| 3 conditionals, mutable lets, boolean literals and types | `if`, `else`, `mut`, `true`, `false`, `bool`, `Unit` | Constructs scheduled. Reservation of the first five is proposed. Types are Q1. |
| 4 exact contract arithmetic | `math` | Reserved-word candidate for the explicit boundary. See the punctuation ballot. |
| 4 loops, bounds and transfers | `while`, `from`, `until`, `for`, `break`, `continue` | First three reserved on main. Last three decided by OD47–49. |
| 6 types, patterns and matches | `Copy`, `type`, `enum`, `match` | First two reserved on main. Enum and match constructs are decided. Proposed reservation of the last two requires the slice delta. |
| 7 scoped borrow | `borrow` or `loan` | Open. Each option reserves that word throughout its candidate revision. `mut` is reused for the exclusive form. |
| 8 public items, constants, external submodules and import lists | `pub`, `const`, `mod`, `import` or `use` | Public visibility, constants and `mod X;` are decided. Import spelling is open. Both import options preserve item lists and qualified uses. |
| 9 traits, impls, deferred calls and capability parameters | `trait`, `impl`, `defer`, `Self`, `cap` | Constructs decided. `Self` and `cap` already reserved. Proposed reservation of the other three requires the slice delta. |
| 10 proof declarations and statements | `assert`, `assume`, `spec`, `by`, `ghost` | Constructs decided. Last three already reserved. Proposed reservation of `assert` and `assume` requires the slice delta. |
| 11 authority clauses and scalar text type | `with`, `char` | `with` construct decided, proposed reservation subject to slice delta. `char` is Q1. |
| 9 explicit capability arguments, 11 grants and effect clauses | `Fs`, `Network`, `Env`, `Time`, `Alloc`, `Device`, `Console`, `Process`, `Unsafe`, `Entropy` | Reserved on main. Concrete operations consume them as their authority families land. |
| 17 foreign parameters | `kept` | Already reserved on main. First consumed by FFI extern parameters under Q3.12 and [recovered item 52](../decisions.md#recovered-59-52). |
| 16–17 trust and foreign declarations | `trusted`, `extern` | Q2 below. |

Authority for the already reserved spellings is reflected in
[Surface0.word](../../ConcreteSpec/Spec/Surface0.lean), including OD50, OD73,
OD81 and OD90 and the [authority reservations](../decisions.md#decided-2026-11-02-authority).
Slice placement comes from the [vertical slices](../../ROADMAP.md#vertical-slices).

### Q1. Other primitive type names

- A. Add keyword tokens like `u8` for `i8`, `i16`, `i32`, `i64`,
  `u16`, `u32`, `u64`, `bool`, `Unit` and `char` at their respective deltas.
  The draft groups these keyword terminals as `PRIMITIVE` where they have the
  same grammar, including as a qualified-call root. `u8` retains its own
  terminal and also starts a qualified call. Both classifications preserve
  `char::from_ascii`, `char::from_code_point` and `char::code_point` in slice 11.
- B. Keep those names as identifier tokens and resolve them as builtin types under
  OD43(c), which prevents user type declarations from taking their names.

### Q2. When to reserve `trusted` and `extern`

- A. Reserve both now. They have no accepting productions through slice 11.
- B. Reserve each globally in the revision that first consumes it, at slices
  16–17. Recommended because it keeps the current identifier space available
  until a workload uses the syntax, and publishes each change alongside the
  grammar and rules that consume it. Either policy uses uniform reservations
  within a revision. Under A, an identifier with either spelling is refused
  lexically. Under B, it can still be an identifier before the relevant
  revision. Neither creates a distinct prediction set through slice 11.
  The checker exercises each reservation timing using real function names.
  [Design philosophy](../PHILOSOPHY.md) · [Vertical slices](../../ROADMAP.md#vertical-slices)

### Q3. Builtin names and first consumers

The current OD153(a) list below is transcribed from the spec's
[`intrinsicNames`](../../ConcreteSpec/Surface1.lean). Every spelling is an
identifier token. Reservation prevents redeclaration and does not admit a call
before its slice. Rows marked later have no consumer in this draft.

| First consumer | Exact current spellings |
|---|---|
| 1 explicit terminal abort | `abort` |
| 2 wrapping, saturation and named lossy integer conversions | `wrapping_add`, `wrapping_sub`, `wrapping_mul`, `saturating_add`, `saturating_sub`, `saturating_mul`, `truncate`, `reinterpret` |
| 3 explicit discard | `discard` |
| 6 checked integer results and fallible conversion | `checked_add`, `checked_sub`, `checked_mul`, `checked_div`, `checked_rem`, `checked_shl`, `checked_shr`, `checked_neg`, `try_convert` |
| 9 destruction | `destroy` |
| 11 UTF-8 byte access | `byte_at` |
| Later allocation and owned operations | `alloc`, `read_owned` |
| Later layout queries, argument builtin and float operations | `sizeof`, `alignof`, `get_args`, `trunc`, `round`, `floor`, `ceil` |
| Later owned-string operations | `string_concat`, `string_slice`, `string_substr`, `string_trim`, `int_to_string`, `bool_to_string`, `float_to_string`, `string_push_char`, `string_append`, `string_append_int`, `string_append_bool`, `string_reserve` |

- Keep the current reservation catalog. The [November 24
  clarification](../decisions.md#decided-2026-11-24-builtin-names) retains
  `byte_at` and releases `alloc_array`, `free_array`, `vec_pop` and
  `string_length`.
- Slice 2 lossless conversion ballot: add `widen` as a reserved builtin
  identifier, or choose another builtin name under E4. No new reservation is
  assumed. E4 separately asks how to supply the target type.
  A. Use the expected type and refuse a call without one.
  B. Add a type argument at a builtin-only position. For compatibility testing
  the draft uses `widen(type: u32, x)` and
  `wrapping_add(type: u32, x, y)` as concrete candidate shapes.
  The draft parses the reserved marker and checks the bare identifier and
  the value-argument count. E4 still owns the exact eligible builtin catalog. The type marker, colon and comma use existing tokens.
  C. Use per-target builtin identifiers such as `to_u32(x)`, which have the
  ordinary call shape. The exact builtin catalog and the form in B are open.
  The draft checks B as a separate option alongside ordinary calls from slice 2.

Illustrative OD43(c) type and trait list. An exhaustive type and resource
reservation catalog needs a separate disposition before the relevant
delta. The names below include candidates already represented as reserved
keyword tokens as indicated in the keyword table.
The current [`builtinTypeNames`](../../ConcreteSpec/Surface1.lean) also reserves
`Heap` (slice 12), `HeapArray` (13), `Vec` and `String` (14), and `f32` and
`f64` (18). These reservations have no dedicated productions through slice 11.

| First consumer | Builtin types or traits |
|---|---|
| 0 | `u8` |
| 2 | `i8`, `i16`, `i32`, `i64`, `u16`, `u32`, `u64` |
| 3 | `bool`, `Unit` |
| 6 | `Copy`, `Option`, `Result`, `ArithmeticError`, `OutOfRange` |
| 7 | Array and view forms use punctuation and types rather than a new mandatory builtin name. |
| 9 | `Destroy` |
| 11 | `StaticStr`, `char`, the `StdIn`, `StdOut` and `StdErr` grants, and file and directory handle and failure types. The exact file API type spellings remain open in this catalog. |

## Operator, punctuation and literal ballot

| First consumer | Exact token or form | Disposition or options |
|---|---|---|
| Existing | `(`, `)`, `{`, `}`, `->`, `:`, `=`, `;`, `+`, `-`, `*`, `/`, `%` | Decided. Slice 0 admits grouping, blocks, return type, annotations, initialization, statement termination and checked addition. Slice 2 adds unary/signed `-` and the other arithmetic operators. |
| Existing lexer | decimal/radix integers, `"text"`, ASCII byte `'x'`, label `'name` | Existing lexical kinds. Consumer for integer is slice 0 (decimal); slice 2 extends integer forms and admits byte literals; slice 4 adds labels; slice 11 adds text expressions. Slice 4 uses strings as attribute arguments. Literal-revision options below. |
| 2 | `,`, `&`, `\|`, `^`, `<<`, `>>` | Comma first separates the two arguments of numeric builtins. Remaining tokens are bitwise/shift operators. |
| 2 | `~` or `!` for bitwise NOT | A. `~x` (recommended, keeps `!` boolean-only). B. `!x` distinguished by the operand type from slice 3. This is one prefix grammar alternative under B. |
| 3 | `==`, `!=`, `<`, `<=`, `>`, `>=`, `&&`, `\|\|`, `!` | Decided comparisons and boolean operators (OD42, Q6.5). Trailing expressions have no semicolon. |
| 3, 6–7 | Block statement boundary | SYN-8 gives a statement-position `match` its own boundary and permits a match in a trailing value. For slice approval, the draft admits `match x { _ => 40, } + 2` as a trailing value. A shared token goes to the next body item. This applies to `-` in slice 6 and also to `*` and `&` once slice 7 adds dereference and reference forms. At slice 6, `*` and `&` continue a trailing match expression. Use `(match x { _ => 40, }) - 2` for subtraction in that position. Reserving shared tokens for binary continuation would require a separate boundary for following unary items. The draft also applies the semicolon-free statement boundary to body-leading `if` and scoped borrow for slice approval. |
| 4 | `LABEL :` and `break LABEL;` / `continue LABEL;` | The loop declaration uses a colon. The transfer uses the label alone. |
| 4–5 | `#[name]`, `#[name(args)]` | Listed forms for `invariant`, `variant`, `may_not_terminate`, `requires`, `ensures` are decided. General attribute syntax is open. A. Keep the same `#[...]` envelope. B. Use `@[...]` for general attributes, preserving the listed `#[...]` forms. Attribute names stay identifiers. Both envelopes use the same IDENT argument grammar. A draft check validates the listed name, envelope, argument shape and parsed attachment. |
| 4 | `math { expr }` | The owner requires an explicit exact-arithmetic boundary. A. Reserve `math` for this candidate, checked in loop and later function contracts. B. Keep `math` as an ordinary identifier with the same braces. Factor the braced suffix with struct literals and recognize exact arithmetic by the identifier spelling and suffix shape. In a condition head this candidate requires shielding, for example `if (math { x + 1 } > x) { … }` where exact arithmetic is permitted. The checker exercises the factored candidate and plants the conflict from admitting the opening brace freely in a head. Exact spelling remains open. [Recovered item 10](../decisions.md#recovered-59-10) |
| 4 | `#[may_not_terminate("reason")]` | Decided. One shared statement prefix branches into a loop or an expression statement under either envelope option. In `#[may_not_terminate("reason")] fp(x) + g()` the permission attaches to `fp(x)`, the immediately following postfix expression. The semantic checker requires an indirect call as the expression target and a nonempty reason. [Termination permission](../decisions.md#decided-project-no-hidden-traps) |
| 1 | `_` | OD62 spelling decided. A. Punctuation token. B. A reserved identifier token distinct from ordinary identifiers. Both candidates include Slice 1 typed wildcard lets and reuse the classification for later bindings and patterns. Main currently resolves the Slice 1 spelling through an identifier token. |
| 6 | `.`, `::`, `=>`, `\|` | Field selection, qualified paths, pattern/body arrow, and pattern alternatives. OD128 decides `=>`; PAT-7 supplies `\|` in match patterns. |
| 6 | `..` in `Variant { .. }` | Explicit ignored payload under [OD25](../decisions.md#od25). |
| 6 | Match arm terminator | A. `,`, as recorded in [recovered item 48](../decisions.md#recovered-59-48). B. `;` is infeasible under the owner’s recorded rule that every arm ends with a comma. |
| 7 | Prefix `&`, `&mut`, `*`, `[`, `]` | Shared/exclusive borrows and dereference, array types/literals and postfix indexing. `&mut` is two tokens. |
| 7 | Scoped borrow range | A. `borrow place from lo until hi -> view { … }`. B. `borrow place from lo .. hi -> view { … }`. Either range combines with either borrow keyword and binder delimiter. The upper endpoint uses the same depth-zero block boundary as a condition head. |
| 7 | Scoped borrow binder | A. `-> view`. B. `: view`. C. Prefix `borrow view = place { … }`, the earlier [recovered item 11](../decisions.md#recovered-59-11) candidate pending the spelling round. C puts the optional range after the place and before the block, such as `borrow view = place from lo until hi { … }`. Both range spellings are checked with this placement. `as` remains an ordinary identifier. |
| 8 | Import list | A. `import a::b::{x, y};`, as recorded in [recovered item 44](../decisions.md#recovered-59-44). B. `use a::b::{x, y};`. Both use `b::x` at the use site. The later imported-use decision leaves the exact import spelling open. |
| 9 | `<`, `>`, explicit expression `::<...>` | Declaration and type arguments use angle delimiters. The expression turbofish has a recorded precedent (OD87). A bare expression `<...>` conflicts with comparison, as the planted control shows. [Recovered item 34](../decisions.md#recovered-59-34) specifies longest match with `>>` split only where it closes generics. The draft tests the source strings `Outer<Inner<u8>>` and `x>>y`, including an adapter-disabled control for the generic closers. The exact lexer/parser interface for contextual splitting needs the slice 9 approval. |
| 9 | `cap C`, `+`, `{}` | Type parameters and arguments precede capability parameters and arguments. `cap` marks declarations, and sets use unions of names or the empty set. [OD88](../decisions.md#od88) · [recovered item 46](../decisions.md#recovered-59-46) |
| 10 | Existing `(`, `)`, `;`, `#[`, `]` | Expression-bodied `spec fn … = e;`, `assert(e);`, `assert(e) by lemma(args);`, `assume e;`, proof paths, `#[proof(path)]` and `#[test]`. |
| 11 | Existing `(`, `)`, `,` | `with(...)` before the return arrow and ordinary call syntax for resource operations. |
| — | Compound assignment | Outside the proposal. |

Sources for the fixed forms include the [literal
answers](../decisions.md#decided-2026-11-08-language-literals),
[OD42](../decisions.md#od42), the [namespace
decision](../decisions.md#decided-2026-11-02-namespace),
[OD128](../decisions.md#od128), [OD87](../decisions.md#od87),
[Q4.7](../decisions.md#decided-2026-11-05-core) and [imported
uses](../decisions.md#decided-2026-11-08-imports-qualified).

Slice 2 literal choices under R-0722 belong to the E4 ballot. For each, keep
the existing lexer rule or approve the listed extension.

- `_` between digits, with a separate choice about immediately after a radix prefix.
- Uppercase hex digits in addition to lowercase hex digits.
- Matching redundant type suffixes such as `255u8`.
- String escapes `\u{...}` for Unicode scalar values, excluding surrogates.
- String escapes `\x00`–`\x7F`.
- Byte-literal escapes `\x00`–`\xFF`.

The predictive grammar uses the same literal token categories for these
options. The draft adapter distinguishes the digit and escape extensions.
The redundant suffix probe exercises both policies with a supplied expected
type. Inference of that type remains with E4. Raw-byte literal
syntax has a separate future disposition.
