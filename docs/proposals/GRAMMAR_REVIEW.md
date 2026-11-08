# Concrete surface grammar — owner review sheet (v2)

Review date: 2026-11-08. Revised against the final Codex, Kimi and agy reviews; see [finding dispositions](GRAMMAR_CHANGES.md). **Proposed review sheet, not an approved grammar.**

This is the byte/token/surface-language inventory for the independent frontend judgment in Core **Q8.3**, not the ANF syntax of Core. [Q8.3 and Q9.1](../docs/decisions.md#decided-2026-11-05-core) distinguish those objects. In particular, Core `copy`, `move`, fresh numeric identities and condition regions are not new source spellings.

Decided answers govern over the frozen grammar and parser: [OD150](../docs/decisions.md#od150), [OD77](../docs/decisions.md#od77). Core rows of November 5 are provisionally approved design, not a completed Core freeze. The existing [Slice1Syntax](../ConcreteSpec/Spec/Slice1Syntax.lean), [Borrow](../ConcreteSpec/Spec/Borrow.lean), [Brand](../ConcreteSpec/Spec/Brand.lean), [Authority](../ConcreteSpec/Spec/Authority.lean), [Cleanup](../ConcreteSpec/Spec/Cleanup.lean) and [Rep](../ConcreteSpec/Spec/Rep.lean) definitions model parts of the meaning; they do not supply a full independent byte grammar.

**Approval boundary.** A citation of a semantic decision does not approve punctuation that the decision never mentions. Such details are **OPEN** below. An old rule can supply evidence for a recommendation, but cannot masquerade as a spelling decision in decisions.md. The final numbered list includes every OPEN identifier used here. No OPEN alternative is implemented by this sheet.

**LL(1) status.** The expression and declaration kernels below are factored for predictive parsing. There are unresolved surface choices, especially statement/value blocks and contextual words. This is not a claim that a complete grammar has passed a mechanical LL(1) check. Section 8 identifies the remaining conflicts and the decisions needed; it would be false to promise a fully checked LL(1) grammar before those choices are approved. No decided spelling is changed to hide a conflict.

## 1. Reading the grammar and slices

EBNF: `=` defines; `|` separates alternatives; `[x]` is optional; `{x}` repeats; quoted text is a token; `ε` is empty. `IDENT`, `INT`, `FLOAT`, `CHAR`, `STRING`, `LABEL`, `EOF` are token classes. Quoted identifier-shaped attribute names are contextual token tests, **not** automatically reserved words. `List(X)` and `List1(X)` below are explicitly defined list expansions. No uppercase/lowercase path heuristic is permitted: [OD152](../docs/decisions.md#od152).

Each grammar group has a slice/provenance table. Auxiliary productions inherit that group's slice, or the stated later extension. Slice numbers mean first availability of the form, not permission to use all its eventual operands immediately. The source is the [approved cumulative slice table](../ROADMAP.md#vertical-slices), under the [vertical-slices decision](../docs/decisions.md#decided-project-vertical-slices-2026-11-06). The roadmap's LP0 adds no syntax.

| Slice | Surface increment |
|---|---|
| 0 | parameterless `fn main() -> u8`, typed immutable lets, decimal integers, local reads, parentheses, `+`, `return` |
| 1 | empty linear structs, literals, moves by ordinary reads, struct destructuring; `abort()` |
| 2 | all integer widths/bases, integer operators, casts, named wrapping/saturating and lossy conversions |
| 3 | booleans, comparisons, logic, `Unit`, `()`, value `if`, value-block tails, `let mut`, assignment, `discard` |
| 4 | while/counting loops, labels and exits, loop invariant/variant/termination permission |
| 5 | ordinary functions, parameters/calls/recursion, requires/ensures, function variant/permission |
| 6 | capabilities, `with`, target-grant parameters, entropy permission (name OPEN-3) |
| 7 | scalar `Secret<T>`, classify/declassify/constant-time operations |
| 8 | assert/assume, ghost bindings, total specification functions, theorem attributes, tests |
| 9 | nonempty named-field structs, Copy declarations, projections/writes, type aliases |
| 10 | enums and match/patterns |
| 11 | generics, bounds, capability parameters/arguments |
| 12 | Option/Result, checked arithmetic and try_convert |
| 13 | references, scoped borrows, reference patterns/lending |
| 14 | traits/impls/methods/qualified trait calls, Destroy, defer |
| 15 | arrays, indexing, range views, swap_at, secret arrays and array/view ct_eq |
| 16 | static text/string literals, char, text views and byte_at |
| 17 | modules/imports/pub/constants, struct invariants |
| 18 | file/directory/environment APIs, ordinary call syntax |
| 19 | brands, allocators, heap allocation/opening, secret heap values |
| 20 | function values/indirect calls, expression termination permission; function-type contracts still require design |
| 21 | heap arrays/views/drain/map_into |
| 22 | Vec and allocating String APIs |
| 23 | maps |
| 24 | raw pointers, trusted functions, Unsafe |
| 25 | extern declarations, symbol/layout attributes, kept/Kept |
| 26 | floats and float conversions |
| 27 | clock/network/device APIs, ordinary call syntax |
| 28 | concurrency: model and all new syntax OPEN-24 |

The slice-0 subset admits only its restricted `main` body, not arbitrary calls or declarations just because a later production appears here. Unsupported forms need classified refusals: [slice-0 decision](../docs/decisions.md#decided-slice0-surface-2026-11-06).

## 2. Lexical grammar

### Encoding, space, comments, names

UTF-8 source and UTF-8 static text are supported by [Core Q4.1–Q4.4](../docs/decisions.md#decided-2026-11-05-core); the explicit legacy source-encoding and escape rule is [SYN-5](../legacy/docs/spec/syntax.md#SYN-5). ASCII char literals are decided by [OD137](../docs/decisions.md#od137). Exact source whitespace, identifier alphabet and source byte edge cases lack a dedicated decision: **OPEN-1**, **OPEN-2**. Recommended lexical core:

```ebnf
source       = { whitespace | line_comment | token }, EOF ;
whitespace   = " " | HT | LF | CR ;
line_comment = "//", { character_except_LF_or_CR }, ( LF | CR | EOF ) ;
letter       = "A" … "Z" | "a" … "z" ;
digit        = "0" … "9" ;
identifier   = (letter | "_"), {letter | digit | "_"} ;
label        = "'", identifier ;
```

Slices: identifier/space/source decoding 0; line comments recommended at 0; labels 4. `//` comments have no nesting; `/*…*/` is absent in [SYN-6](../legacy/docs/spec/syntax.md#SYN-6), but its precise status as a current owner spelling choice is **OPEN-1**. LF, CR or EOF ends a comment; CRLF therefore ends it at CR, with LF remaining whitespace (OPEN-1 recommendation). A BOM, shebang, non-ASCII whitespace, Unicode identifiers, embedded source NUL are not silently accepted by this recommendation; see OPEN-1/2.

Names are case-sensitive (recommended, OPEN-2), and casing does not determine a path's syntactic category ([OD152](../docs/decisions.md#od152)). `_` is an unused binding, not a license to lose a linear value or Option/Result: [OD62](../docs/decisions.md#od62), [audit](../docs/decisions.md#decided-2026-11-02-audit).

### Reserved list

The following is a **complete proposed lexer-reserved list**, not a claim that one existing decision enumerates it. **OPEN-3** asks the owner to approve its closure and the disposition of obsolete words. Existing explicit reservations remain binding.

```text
Alloc Console Copy Device Env Fs Network Process Time Unsafe
as assert assume borrow break cap const continue defer else enum extern
false fn for from ghost if impl import let match mod mut pub return
struct trait true trusted type until while with spec by kept Self
```

Explicit decisions: `from`/`until` [OD50](../docs/decisions.md#od50); `ghost` [OD81](../docs/decisions.md#od81); `Copy` [OD90](../docs/decisions.md#od90); the nine capabilities, including `Console`, [authority rule 13](../docs/decisions.md#decided-2026-11-02-authority), as revised by [Q7.11](../docs/decisions.md#decided-2026-11-05-core). [OD73](../docs/decisions.md#od73) forbids reserved words in identifier positions, including import aliases and variant names. The entropy capability arrives at slice 6, but its **name is not supplied by that decision**: [stack/randomness decision](../docs/decisions.md#decided-2026-11-07-stack-randomness). Do not reintroduce the historical capability `Random` without approval.

Recommend reserving `spec`, `by`, `kept` and `Self` under OPEN-3 (included in the proposed list above); `while` is the decided loop keyword (OD47). `Self` has special type meaning under [TRAIT-9](../legacy/docs/spec/traits.md#TRAIT-9), and is not an ordinary free type name. Recommended contextual words: `self`; attribute names `proof`, `may_not_terminate`, `requires`, `ensures`, `invariant`, `variant`, `test`, `repr`, `link_name`, `export_name`; repr arguments `C`, `packed`, `align`. `result` is a special bound name only inside ensures. These contextual statuses, where not expressly settled, are OPEN-3. `self`, `repr`, `packed`, `align` were expressly left unchanged by OD90, not newly reserved by it.

Canonical builtin type names are **reserved declaration names**, not necessarily keyword tokens: `i8 i16 i32 i64 u8 u16 u32 u64 bool char f32 f64 Unit`, plus the builtin nominal constructors admitted by their slices (`Secret`, `StaticStr`, `Option`, `Result`, `Allocator`, `Heap`, `HeapArray`, `Vec`, `String`, `OrderedMap`, `HashMap`, `AllocFailure`, `ArithmeticError`, `OutOfRange`, `Kept`, etc.). [OD43](../docs/decisions.md#od43), [OD102](../docs/decisions.md#od102), [OD157](../docs/decisions.md#od157) fix collisions/canonical spellings; [TYPE-19](../legacy/docs/spec/types.md#TYPE-19) lists scalar names. Builtin function names cannot be redeclared ([OD133](../docs/decisions.md#od133), [OD153](../docs/decisions.md#od153)). The exact complete builtin/grant/primitive name inventory is **OPEN-4**; `etc.` here is an identified inventory gap, not a wildcard accepted by the parser. Compiler-owned `__` names, `user_main` and the `__destr_` binding prefix have legacy restrictions ([MOD-3](../legacy/docs/spec/modules.md#MOD-3), [SYN-3](../legacy/docs/spec/syntax.md#SYN-3)); carry their exact scope for approval in OPEN-4.

### Numbers and quoted literals

```ebnf
DECIMAL = "0" | nonzero_digit, {digit} ;
HEX     = "0x", hex_digit, {hex_digit} ;
BINARY  = "0b", binary_digit, {binary_digit} ;
OCTAL   = "0o", octal_digit, {octal_digit} ;
INT     = DECIMAL | HEX | BINARY | OCTAL ;
FLOAT   = digit, {digit}, ".", digit, {digit} ;
STRING  = '"', {string_character | string_escape}, '"' ;
CHAR    = "'", (ascii_char_character | char_escape), "'" ;
```

Here `nonzero_digit = 1…9`, `hex_digit = 0…9 | a…f | A…F`, `binary_digit = 0|1`, `octal_digit = 0…7`. These helper classes are lexical expansions, not names available in source.

Decimal integers arrive at 0; other bases at 2; FLOAT at 26; STRING/CHAR at 16 (a reason STRING in an earlier annotation is metadata, not a runtime StaticStr feature). Leading-zero decimal integers are explicitly refused: [slice-0 surface](../docs/decisions.md#decided-slice0-surface-2026-11-06). The old forms are documented by [SYN-6](../legacy/docs/spec/syntax.md#SYN-6). **OPEN-5**: approve lowercase base prefixes, no separators, float digit/point syntax and a minimal typed-integer suffix policy and boundary handling; the frozen lexer also accepted uppercase prefixes. The [approved Q6.2 field example](../docs/decisions.md#decided-2026-11-05-q6-2-fields) contains `200u8`. A blanket suffix refusal conflicts with that example; OPEN-5 recommends retaining typed integer suffixes, but their complete width/base/boundary grammar needs approval. The displayed INT production is the unsuffixed kernel, not a complete suffix grammar. No exponent notation, `1.`, `.5`, hex floats or special NaN/Inf literals are proposed.

Minus is an operator, not part of INT/FLOAT. Literal expected typing and range checking include negation ([OD31](../docs/decisions.md#od31), [OD63](../docs/decisions.md#od63), [OD138](../docs/decisions.md#od138)); an unconstrained literal is refused, not defaulted. Float literals round directly to their expected width; no lexical suffix is implied by that rule.

Only these escapes are proposed, copied exactly from SYN-5/6; **OPEN-6** requests explicit owner ratification where decisions.md does not enumerate them:

| Literal | Allowed escapes | Raw content |
|---|---|---|
| string | `\n`, `\t`, `\r`, `\\`, `\"`, `\0` | UTF-8 characters other than quote/backslash; raw line-break/control policy OPEN-6 |
| char | `\n`, `\t`, `\r`, `\0`, `\\`, `\'` | exactly one ASCII character other than quote/backslash; controls OPEN-6 |

No `\xHH`, `\u{…}`, interpolation, raw-string delimiter or byte-string prefix is admitted by this proposal. Embedded NUL in static text is explicitly permitted by Core Q4.2, using `\0`; ASCII literal restrictions do not restrict every computed `char` to 0…127 ([TYPE-20](../legacy/docs/spec/types.md#TYPE-20)). A char ending in a closing quote takes priority over LABEL; a bare apostrophe plus a name is LABEL. The exact lexical distinction and malformed-quote refusal is OPEN-6, not a token-level LL(1) claim about individual bytes.

### Punctuation and operators

Complete punctuation/operator set used by the proposed productions:

```text
( ) [ ] { } , ; : . :: #
+ - * / % ! ~ & | ^ && ||
= == != < <= > >= << >> -> => ..
```

`'` and `"` delimit lexical tokens, not standalone expression operators. `..` is only a pattern payload-rest marker (slice 10), not a range expression or update. `->` is only a result arrow (5; main already 0). `=>` is the match-arm arrow (10), fixed by [OD128](../docs/decisions.md#od128). No postfix `?`, `->`, `++`, `--`, compound assignments, range `..=`, or implication operator is introduced. Maximal munch, the precise token inventory and adjacent-operator handling are **OPEN-7**, with [SYN-7](../legacy/docs/spec/syntax.md#SYN-7)'s special split of nested type `>>` retained for approval. A forbidden construct may be recognized to issue a targeted diagnostic without being a valid production.

Cast-result postfix continuations are deliberately inherited from the frozen `cast_expr` production: `x as T.f()` operates on the completed cast, while `x.f() as T` selects the field first. This contextual continuation is not an additional global precedence level; its full grammar remains OPEN-8.

Precedence, low to high, from [SYN-2](../legacy/docs/spec/syntax.md#SYN-2) and the [frozen grammar](../legacy/grammar/concrete.ebnf); the complete table lacks a dedicated decided row, so **OPEN-8** requests ratification:

| Level | Operators | Associativity | Slice |
|---|---|---|---|
| 1 | `\|\|` | left | 3 |
| 2 | `&&` | left | 3 |
| 3 | `== !=` | left | 3 |
| 4 | `< <= > >=` | left | 3 |
| 5 | `\|` | left | 2 |
| 6 | `^` | left | 2 |
| 7 | `&` | left | 2 |
| 8 | `<< >>` | left | 2 |
| 9 | `+ -` | left | `+` 0; `-` 2 |
| 10 | `* / %` | left | 2 |
| 11 | `as` | left, chained casts | 2 |
| 12 | prefix `- ! ~ & &mut *` | right | integer 2, `!` 3, references/deref 13, raw 24 |
| 13 | calls, indexing, member/method access, turbofish continuation | left | call 5, field 9, generic 11, method 14, index 15, indirect call 20 |

`-x as i32` means `(-x) as i32`; `-c.n` means `-(c.n)`. Assignment is a statement, not a precedence level. Left grouping of comparisons does not make a type-invalid chain legal. [OD91](../docs/decisions.md#od91) explicitly decides `x as T < y` as comparison and `x as (Vec<T>)` for a generic cast target.

## 3. Items, attributes, generics and types

### Item grammar

```ebnf
program       = {item}, EOF ;
item          = attributes, ["pub"], item_core ;
item_core     = "fn", fn_rest
              | "trusted", trusted_rest
              | "spec", "fn", spec_rest
              | "struct", ["Copy"], IDENT, [parameters], "{", fields, "}"
              | "enum", ["Copy"], IDENT, [parameters], "{", variants, "}"
              | "type", IDENT, "=", type, ";"
              | "const", IDENT, ":", type, "=", expr, ";"
              | "mod", IDENT, ";"
              | "import", IDENT, import_path_tail
              | "trait", IDENT, "{", {trait_method}, "}"
              | "impl", [parameters], named_type, impl_tail
              | "extern", "fn", extern_rest ;
trusted_rest  = "fn", fn_rest | "extern", "fn", extern_rest ;
fn_rest       = IDENT, [parameters], signature, fn_body ;
signature     = "(", [List1(param)], ")", [cap_clause], ["->", type] ;
fn_body       = function_block | ";" ;
spec_rest     = IDENT, [parameters], signature, "=", contract_expr, ";" ;
extern_rest   = IDENT, "(", [List1(extern_param)], ")", cap_clause,
                ["->", type], ";" ;
param         = IDENT, ":", type ;
extern_param  = ["kept"], IDENT, ":", type ;
fields        = [field, field_tail] ;
field_tail    = [",", field_after_comma] ;
field_after_comma = field, field_tail | ε ;
field         = ["pub"], IDENT, ":", type ;
variants      = variant, variant_tail ;
variant_tail  = [",", variant_after_comma] ;
variant_after_comma = variant, variant_tail | ε ;
variant       = IDENT, ["{", fields, "}"] ;
import_path_tail = ".", import_after_dot ;
import_after_dot = IDENT, import_path_tail
                 | "{", List1(import_symbol), "}", ";" ;
import_symbol = IDENT, ["as", IDENT] ;
impl_tail     = "for", named_type, [cap_clause], "{", {impl_method}, "}"
              | "{", {impl_method}, "}" ;
impl_method   = attributes, ["pub"], ["trusted"], "fn", IDENT,
                [parameters], method_signature, function_block ;
trait_method  = attributes, "fn", IDENT, method_signature, ";" ;
method_signature = "(", [method_params], ")", [cap_clause], ["->", type] ;
method_params = "&", ["mut"], "self", [",", List1(param)]
              | IDENT, method_ident_tail ;
method_ident_tail = ":", type, {",", param}
                  | [",", List1(param)] ;
```

In `method_params`, the second `method_ident_tail` alternative is legal only when the consumed identifier is contextual `self`; the first is an ordinary typed parameter. Its contextual-token treatment is OPEN-3/12. The ordinary typed-parameter alternative does not admit `self: T` as a second receiver spelling in this recommendation: a contextual self receiver uses only self/&self/&mut self; [OD60](../docs/decisions.md#od60) chooses one consuming form.

| Productions/form | First slice | Spelling authority or gap |
|---|---|---|
| program, main function shell, function_block | 0 | [slice-0 decision](../docs/decisions.md#decided-slice0-surface-2026-11-06); arbitrary functions/signatures at 5 |
| item prefixes, `pub`, `trusted fn`, `trusted extern fn` | 17 / 24 / 25 | [OD9](../docs/decisions.md#od9), [OD22](../docs/decisions.md#od22), [OD61](../docs/decisions.md#od61); prefix order/allowed targets **OPEN-9** |
| function body or `;` | 5 (bodyless declaration placement OPEN) | [OD77](../docs/decisions.md#od77) retains bodyless declarations; permitted locations/linkage **OPEN-9** |
| struct/fields/literal shell | empty 1, named fields/Copy 9, field pub 17 | [OD5](../docs/decisions.md#od5), [OD65](../docs/decisions.md#od65), [OD70](../docs/decisions.md#od70); exact ordinary declaration punctuation **OPEN-9**, [TYPE-7](../legacy/docs/spec/types.md#TYPE-7) evidence |
| enum/variants | 10 | [OD71](../docs/decisions.md#od71), [OD72](../docs/decisions.md#od72), [OD77](../docs/decisions.md#od77); exact declaration punctuation **OPEN-9** |
| type alias | 9 | feature in slice table; [TYPE-12](../legacy/docs/spec/types.md#TYPE-12) evidence; `type N = T;` **OPEN-10** |
| const | 17 | [OD8](../docs/decisions.md#od8), [Q6.4](../docs/decisions.md#decided-2026-11-05-core); `const N: T = e;` **OPEN-10** |
| mod | 17 | `mod X;` [OD78](../docs/decisions.md#od78), no inline body [OD33](../docs/decisions.md#od33); file X.con, children in X/ [OD142](../docs/decisions.md#od142) |
| import path/symbol/rename | 17 | explicit imports [OD139](../docs/decisions.md#od139); renaming [OD77](../docs/decisions.md#od77); dot module separators [SYN-10](../legacy/docs/spec/syntax.md#SYN-10); exact `. {…}` boundary/list **OPEN-11** |
| trait/impl/method heads | 14; generic impl 14 using slice-11 parameters | qualified calls [OD6](../docs/decisions.md#od6); trait parameters forbidden [OD148](../docs/decisions.md#od148); generic trait methods forbidden [OD77](../docs/decisions.md#od77); precise heads/receiver punctuation **OPEN-12** |
| spec_rest | 8 | `spec fn`, body required/total [OD115](../docs/decisions.md#od115); `= expr;` versus block body **OPEN-13** |
| extern_rest/extern_param | 25 | `extern fn` [OD4](../docs/decisions.md#od4); explicit with [FFI-3](../legacy/docs/spec/ffi.md#FFI-3); marker `kept` and receipt `Kept<H>` [Q3.12](../docs/decisions.md#decided-2026-11-05-core); `kept buf: H` and unmarked call-only default already decided; remaining API qualification **OPEN-23** |

Empty structs are legal, empty enums are refused. Commas between fields/variants are mandatory; trailing-comma availability in each list is OPEN-9/11/14, not permission to omit separators. Trait declarations have neither type nor capability parameters; trait methods have no generic list; trait impls cannot declare capability parameters ([OD89](../docs/decisions.md#od89)). `impl Trait for Type` is recommended, matching the actual parser; the stale EBNF comment reverses it. Coherence, orphan checks and the Copy/Destroy-disjoint impl exception are name/type judgments ([OD6](../docs/decisions.md#od6), [Core Q2.8](../docs/decisions.md#decided-2026-11-05-core)). They do not require disjunctive-bound syntax.

### Attributes

```ebnf
attributes    = {attribute} ;
attribute     = "#", "[", attribute_body, "]" ;
attribute_body = "proof", "(", theorem_path, ")"
               | "may_not_terminate", "(", STRING, ")"
               | "requires", "(", contract_expr, ")"
               | "ensures", "(", contract_expr, ")"
               | "invariant", "(", contract_expr, ")"
               | "variant", "(", contract_expr, ")"
               | "test"
               | "repr", "(", List1(repr_option), ")"
               | "link_name", "=", STRING
               | "export_name", "=", STRING ;
repr_option   = "C" | "packed" | "align", "(", INT, ")" ;
theorem_path  = IDENT, {"::", IDENT} ;
```

Attribute names are dispatched after `#[` with a closed spelling table; contextual token handling must distinguish them from unrestricted IDENT (OPEN-3). Unknown attributes are refused, not ignored ([OD77](../docs/decisions.md#od77)). The table above is the proposed complete ordinary-source table, subject to **OPEN-15** for inherited attributes and symbol-attribute payload punctuation.

| Attribute | Slice | Decided spelling and attachment |
|---|---|---|
| `#[requires(e)]`, `#[ensures(e)]` | 5 | [OD125](../docs/decisions.md#od125) discusses these forms; [CON-2](../legacy/docs/spec/contracts.md#CON-2); function declarations; logical claims, safety use requires evidence |
| `#[invariant(e)]`, `#[variant(e)]` | 4 | [OD82](../docs/decisions.md#od82), [OD104](../docs/decisions.md#od104), [OD105](../docs/decisions.md#od105); next loop; a counted-loop variant is refused |
| function `#[variant(e)]` | 5 | [no-hidden-traps](../docs/decisions.md#decided-project-no-hidden-traps); one measure per recursive cycle |
| `#[may_not_terminate("reason")]` | loop 4, function 5, indirect expression 20, extern 25 | [no-hidden-traps spellings](../docs/decisions.md#decided-project-no-hidden-traps); nonempty reason; while, designated recursive function, extern/blocking boundary, immediately following indirect postfix call; never waives spec-function totality |
| `#[proof(path::to::theorem)]` | 8 | [no-hidden-traps spellings](../docs/decisions.md#decided-project-no-hidden-traps); repeatable function attribute, typed theorem path, not quoted text |
| `#[test]` | 8 | [OD146](../docs/decisions.md#od146), revised [main decision](../docs/decisions.md#decided-2026-11-02-main): result u8 only; parameterless per [STMT-10](../legacy/docs/spec/statements.md#STMT-10), not reopened by OPEN-15 |
| struct `#[invariant(e)]` | 17 | [SPARK decision/revision](../docs/decisions.md#decided-project-spark-2026-11-06); struct with private fields; exact field names/self scope **OPEN-15** |
| `#[repr(C)]`, `#[repr(packed)]`, `#[repr(align(N))]` | 25 | [OD147](../docs/decisions.md#od147), [OD111](../docs/decisions.md#od111), [FFI-4/8/10](../legacy/docs/spec/ffi.md#FFI-4); combined C/packed explicitly approved; remaining payload details **OPEN-15** |
| link/export names | 25 | [OD96](../docs/decisions.md#od96) foreign binding identity; [FFI-5](../legacy/docs/spec/ffi.md#FFI-5); `= STRING` **OPEN-15** |

A declaration attribute attaches to the next declaration; loop attributes to the next loop, not a freestanding item ([OD82](../docs/decisions.md#od82)). Repeated requires/invariants mean conjunction ([CON-8](../legacy/docs/spec/contracts.md#CON-8)); duplicate options and other repeatability details are OPEN-15. Packed plus align is refused; alignment is a power of two within FFI-4's constraint; C plus packed means C order, no padding, alignment 1 ([OD147](../docs/decisions.md#od147)). Enums have no repr form. Default layout was revised to declaration order/natural alignment in the [runtime decision](../docs/decisions.md#decided-project-runtime-2026-11-06); the old “unspecified layout” prose does not override it.

### Generic lists, bounds, capability sets, types

```ebnf
parameters    = "<", parameter, {",", parameter}, ">" ;
parameter     = "cap", IDENT | IDENT, [":", bounds] ;
bounds        = bound_name, {"+", bound_name} ;
bound_name    = "Copy" | IDENT ;
arguments     = "<", argument, {",", argument}, ">" ;
argument      = "{", "}" | type, ["+", cap_name, {"+", cap_name}] ;
cap_name      = IDENT | "Fs" | "Network" | "Env" | "Time" | "Alloc"
              | "Device" | "Console" | "Process" | "Unsafe" ;
cap_clause    = "with", "(", [cap_name, {",", cap_name}], ")" ;
type          = "&", ["mut"], type
              | "*", pointer_mode, type
              | "fn", "(", [List1(type)], ")", [cap_clause], ["->", type]
              | "[", type, ";", INT, "]"
              | named_type ;
pointer_mode  = "const" | "mut" ;
named_type    = type_name, {"::", type_name}, [arguments] ;
type_name     = IDENT | "Self" | cap_type_token ;
cap_type_token = "Fs" | "Network" | "Env" | "Time" | "Alloc"
               | "Device" | "Console" | "Process" | "Unsafe" ;
cast_type     = "(", type, ")" | cast_simple ;
cast_simple   = "&", ["mut"], cast_simple
              | "*", pointer_mode, cast_simple
              | (IDENT | "Self"), {"::", IDENT} ;
List1(X)      = X, {",", X} ;
List(X)       = [List1(X)] ;
```

`Self` is the special impl/trait type form, validated in its permitted context, not an ordinary declaration name. In `impl Trait for Type`, the trait head must resolve to a nongeneric trait with no arguments (OD148); an inherent impl head may be an ordinary generic named type. Reserved capability names are not valid impl target types.

`cap_type_token` is only used to parse a *generic argument candidate*, not to admit a reserved capability as an ordinary value type. Operationally parse the common named argument prefix once, then classify by the declared kind; an ordinary `type` position rejects a capability-only name. This sharing avoids competing IDENT-started alternatives for types, brands and capability names. **OPEN-14** covers the exact kind-neutral argument AST/grammar, generic ordering and brand syntax; the notation above exposes the issue rather than pretending capabilities are value types.

| Production | Slice | Decision / OPEN detail |
|---|---|---|
| parameters, bounds | 11 | `<T>`, `T: Copy` [OD5](../docs/decisions.md#od5); nonempty lists [OD75](../docs/decisions.md#od75); `cap C` and `+` evidence in old grammar, **OPEN-14** for full binder syntax |
| arguments, cap_name | 11 | expression capability arguments [OD87](../docs/decisions.md#od87), shape/order [OD88](../docs/decisions.md#od88), symbolic unions [Q5.2](../docs/decisions.md#decided-2026-11-05-core); bare reserved capability atom and delimiters **OPEN-14** |
| cap_clause | 6; variables 11 | [authority](../docs/decisions.md#decided-2026-11-02-authority), [OD108](../docs/decisions.md#od108); names/variables only, no repeated names, no user aliases; exact union use inside with **OPEN-14** |
| named_type | scalars 0/2/3/16/26, structs 1/9, enums 10, applications 11, qualified 17 | [OD43](../docs/decisions.md#od43), [OD151](../docs/decisions.md#od151), [OD152](../docs/decisions.md#od152); punctuation **OPEN-14** where not fixed |
| reference type | 13 | [OD1](../docs/decisions.md#od1), [OD2](../docs/decisions.md#od2), [OD14](../docs/decisions.md#od14): second-class, no stored/returned refs |
| array type | 15 | `[T; N]` [OD35](../docs/decisions.md#od35); exact permitted length syntax **OPEN-14** |
| function type | 20 | exact capability sets [OD93](../docs/decisions.md#od93); `fn(&mut C, T) with(K) -> Step<T, E>` [Q2.4](../docs/decisions.md#decided-2026-11-05-core); function-type safety contracts **OPEN-22** |
| raw pointer type | 24 | `*const T`/`*mut T` required [OD74](../docs/decisions.md#od74); casts [OD98](../docs/decisions.md#od98) |
| cast_type/cast_simple | 2; ref/raw extensions 13/24 | [OD91](../docs/decisions.md#od91) fixes generic cast parentheses; complete composite target grammar **OPEN-14** |

Semantic filters: ordinary generic type arguments precede capability arguments (OD88); explicit capability arguments are mandatory at every polymorphic call ([Q5.1/Q5.4 revision](../docs/decisions.md#decided-2026-11-05-revisions)); type arguments may be inferred from checked argument types only ([OD46](../docs/decisions.md#od46)), and enum literals follow that same argument-based rule, with explicit arguments otherwise, never inference from an expected result type ([OD129](../docs/decisions.md#od129)). A singleton name is classified by its declaration and parameter kind, never initial case. Unions contain names only, not composite types. `{}` denotes the empty capability set; it is not an empty type argument list. Exact set equality, no widening, no `with(f)` callback shortcut ([OD92](../docs/decisions.md#od92), [OD93](../docs/decisions.md#od93)).

Builtin nominal applications require no extra expression grammar: `Secret<T>`, `Option<T>`, `Result<T,E>`, `Allocator<A>`, `Heap<T,A>`, `String<A>` and others use named_type. Brand binders, existential packaging and second-class view type spellings are not thereby decided; see section 7.

## 4. Patterns, statements, blocks and loops

### Patterns

```ebnf
binder        = IDENT ;
let_head      = "let", ["mut"], IDENT, let_after_name ;
let_after_name = [":", type], "=", expr, ";"
               | "{", pattern_fields, "}", "=", expr, ";"
               | "::", IDENT, let_qualified_pattern_tail ;
let_qualified_pattern_tail = "::", IDENT, let_qualified_pattern_tail
               | ["{", pattern_fields, "}"], "=", expr, ";" ;
ghost_let     = "ghost", "let", ["mut"], IDENT, [":", type], "=", expr, ";" ;
pattern_fields = [pattern_field, pattern_field_tail] ;
pattern_field_tail = [",", pattern_field_after_comma] ;
pattern_field_after_comma = pattern_field, pattern_field_tail | ε ;
pattern_field = IDENT, [":", binder] ;
match_pattern = INT | "-", INT | "true" | "false" | CHAR
              | IDENT, match_name_tail ;
match_name_tail = "::", IDENT, match_qualified_tail | ε ;
match_qualified_tail = "::", IDENT, match_qualified_tail
                     | ["{", payload_pattern, "}"] ;
payload_pattern = ".." | pattern_fields ;
arm           = match_pattern, {"|", match_pattern}, "=>", arm_body, "," ;
arm_body      = "return", [expr], ";" | arm_block | arm_expr ;
```

Slices: let name/binder 0; empty struct destructuring 1; named field patterns 9; match/variant/OR/rest 10; CHAR patterns no earlier than char at 16. `binder` may be `_`; it is not a general binding catch-all pattern in match. Recommended bare match IDENT is only `_`; bare qualified path denotes a fieldless enum variant. Irrefutable enum lets and mutable pattern binders are retained ([PAT-10/11](../legacy/docs/spec/patterns.md#PAT-10)); the factored let path admits module-qualified struct and enum heads, subject to OPEN-17 exact path closure. Nested patterns remain deferred, and fields must be handled explicitly or through the decided ignore forms (OD25). Generic pattern paths such as `E::<T>::V` are accepted by the frozen parser but not established by that fact as a new decision: OPEN-17 must explicitly disposition them; the kernel above does not yet include their arguments. Char literal patterns and the precise payload-rest combination grammar remain OPEN-17.

**Wording conflict requiring clarification:** OD25/PAT-3 allow a “bare variant name” only for a fieldless variant; the chapter’s “No bare variant names” subsection requires enum qualification. “Bare” may mean no payload braces rather than no enum prefix. Retain qualified patterns in this proposed kernel, but do not declare an authoritative general refusal of unqualified fieldless variants without owner clarification. No nested-pattern admission is proposed. The grammar deliberately does not silently add nested tuple/array/struct match patterns.

Decisions: struct destructuring [OD84](../docs/decisions.md#od84); binding renaming [OD77](../docs/decisions.md#od77); named fields and `_` [OD25](../docs/decisions.md#od25), [OD130](../docs/decisions.md#od130); no unqualified struct match pattern [OD76](../docs/decisions.md#od76); no guards [OD55](../docs/decisions.md#od55); no ranges [OD56](../docs/decisions.md#od56); unbraced arm restricted to expression/block/return [OD86](../docs/decisions.md#od86). The exact OR/arm-comma and pattern grammar inherited from [PAT-1/3](../legacy/docs/spec/patterns.md#PAT-1) is OPEN-17. `arm_expr` is the non-block-root expression alternative; `arm_block` is a block parsed once and checked for value/statement use (OPEN-16), preventing two `{` alternatives.

First matching arm wins and unreachable arms are refused ([OD18](../docs/decisions.md#od18)); integer matching always needs a catch-all, even after every literal ([OD41](../docs/decisions.md#od41)). Borrowed-scrutinee payload binders are always references ([OD3](../docs/decisions.md#od3)); the source does not invent `ref` patterns. Pattern completeness, duplicate field checking and linear wildcard restrictions are judgments after parsing.

### Statements and block positions

```ebnf
function_block = "{", statement_sequence, "}" ;
statement_sequence = {statement} ;
value_block    = "{", value_contents, "}" ;
statement      = attributes, statement_core ;
statement_core = let_head
               | ghost_let
               | "return", [expr], ";"
               | "if", head_expr, statement_block, ["else", statement_else]
               | loop
               | LABEL, ":", loop
               | "break", [LABEL], ";"
               | "continue", [LABEL], ";"
               | "match", head_expr, "{", {arm}, "}"
               | "defer", deferred_call, ";"
               | borrow_statement
               | "assert", assertion_rest
               | "assume", contract_expr, ";"
               | expr_statement_start, expr_statement_finish ;
statement_else = "if", head_expr, statement_block, ["else", statement_else]
               | statement_block ;
loop           = "while", head_expr, statement_block
               | "for", IDENT, "from", expr, "until", head_expr, statement_block ;
assertion_rest = contract_expr, ["by", theorem_path, call_args], ";" ;
```

The expression-statement common prefix is parsed **once**. After its postfix/binary continuation, the next token selects `= expr ;` (the accumulated prefix must be a place) or `;` (ordinary Unit statement). It cannot be implemented as `place '=' … | expr ';'`: those alternatives overlap. To make that syntax concrete:

```ebnf
expr_statement_finish = "=", expr, ";" | ";" ;
```

`expr_statement_start` is `expr` with root `if`/`match` and leading attributes excluded, because those prefixes already dispatch above. Attributes are consumed once before statement dispatch; in a value context they use the expression-prefix production of section 5. `statement_block`, `arm_block`, `value_contents` and the treatment of a root `if`/`match` as a final value have the **OPEN-16** decision exposed in section 8. They are specified by the following proposed block algorithm, not left as an unexplained claim of LL(1):

1. Consume `{`. Select the grammatical block mode from its enclosing construct: function, statement, or value.
2. Dispatch keyword statements on their first token. For any expression-leading token, parse the common expression once. `;` completes a statement; `=` selects checked-place assignment; `}` selects a trailing value only in value mode.
3. In a value block, parse a leading `if` or `match` once as a control node. A closing `}` immediately after it can mark it as the tail. If it is followed by more statements, classify it as a statement. Value-if must have else; statement-if need not. The control node's branch blocks therefore need delayed statement/value classification. This last step needs a grammar proof or owner-approved restriction; it is not asserted to be a plain LL(1) EBNF expansion today.
4. A function block rejects any unterminated final expression, including a final value-if or value-match. Return/abort/divergence supplies a function's result. There are no implicit function returns.

| Form | Slice | Authority / gap |
|---|---|---|
| typed let/name/read/return | 0 | [slice-0 decision](../docs/decisions.md#decided-slice0-surface-2026-11-06); unannotated let details **OPEN-18** |
| ghost let | 8 | [OD81](../docs/decisions.md#od81); erased, cannot influence runtime |
| let mut/assignment/place check | 3; projected writes 9/13/15/24 | [OD17](../docs/decisions.md#od17), [OD77](../docs/decisions.md#od77); exact ordinary statement punctuation **OPEN-18** |
| statement if/else and value branches | 3 | [OD16](../docs/decisions.md#od16), [OD34](../docs/decisions.md#od34); [SYN-9](../legacy/docs/spec/syntax.md#SYN-9), [STMT-8](../legacy/docs/spec/statements.md#STMT-8); detailed block grammar **OPEN-16** |
| while/counting loop | 4 | exact `while cond {…}` and `for i from a until b {…}` [OD47](../docs/decisions.md#od47); bounds constrained to one expected integer type [OD48](../docs/decisions.md#od48) |
| labels, break, continue | 4 | label support in slice table, `break <expr>` forbidden [OD27](../docs/decisions.md#od27); apostrophe/colon/target syntax **OPEN-18** |
| defer | 14 | [OD2](../docs/decisions.md#od2), canonical Unit-only [audit](../docs/decisions.md#decided-2026-11-02-audit); exact call grammar **OPEN-19** |
| borrow_statement | 13/15/16/19/21 | section 6; [OD58](../docs/decisions.md#od58), [Core Q4.7](../docs/decisions.md#decided-2026-11-05-core) |
| assert and theorem suffix | 8 | [OD79](../docs/decisions.md#od79), [SPARK](../docs/decisions.md#decided-project-spark-2026-11-06); suffix only on parenthesized assertion per decided example; exact factoring **OPEN-13** |
| assume | 8 | [OD80](../docs/decisions.md#od80), never safety admission; semicolon shape [CON-6](../legacy/docs/spec/contracts.md#CON-6), **OPEN-13** |
| statement match and arm forms | 10 | [OD77](../docs/decisions.md#od77), [OD86](../docs/decisions.md#od86), [SYN-8](../legacy/docs/spec/syntax.md#SYN-8); remaining mode detail **OPEN-16/17** |

The assertion production is a common-prefix parser proposal: parse the complete contract expression and record whether its root is a parenthesized group; only that grouped root admits optional `by path(args)`. An expression merely starting with `(`, such as `(a) == b`, has a binary root and does not qualify. This restriction reflects the approved `assert(e) by thm(...)` spelling; it is not an approved extension to `assert e by ...`.

`deferred_call` is a parsed postfix call whose final suffix is `call_args` (a method call counts); its result must be Unit. It shares the section-5 postfix parser and rejects a binary expression, constructor or block. That syntactic restriction is the OPEN-19 recommendation. `statement_block` means a braced statement-mode block under the OPEN-16 algorithm; `arm_block` uses the surrounding match mode; `arm_expr` means section-5 expr without a block-root alternative. `value_contents` is precisely the sequence/tail selection described in that algorithm, whose EBNF expansion remains OPEN-16. These are explicit parameterized parsing obligations, not unexplained extra terminals.

Loop bounds are evaluated once; index immutable; half-open ascending interval; continue advances. No stepped, reversed or collection form ([OD47](../docs/decisions.md#od47), [OD51](../docs/decisions.md#od51)). A counted loop's proof measure is mathematical ([OD49](../docs/decisions.md#od49)), not a source `b-i` that can overflow. `defer` holds arguments/loans until execution, late reads, LIFO cleanup, result computed before cleanup, ensures after cleanup. Locals receive storage teardown, never implicit Destroy ([OD69](../docs/decisions.md#od69), [OD144](../docs/decisions.md#od144)).

## 5. Expressions, paths, constructors and contracts

```ebnf
expr          = logical_or ;
logical_or    = logical_and, {"||", logical_and} ;
logical_and   = equality, {"&&", equality} ;
equality      = comparison, {("==" | "!="), comparison} ;
comparison    = bit_or, {("<" | "<=" | ">" | ">="), bit_or} ;
bit_or        = bit_xor, {"|", bit_xor} ;
bit_xor       = bit_and, {"^", bit_and} ;
bit_and       = shift, {"&", shift} ;
shift         = sum, {("<<" | ">>"), sum} ;
sum           = product, {("+" | "-"), product} ;
product       = cast, {("*" | "/" | "%"), cast} ;
cast          = unary, {"as", cast_type, {postfix_suffix}} ;
unary         = "-", unary | "!", unary | "~", unary
              | "&", ["mut"], unary | "*", unary
              | attributed_postfix ;
attributed_postfix = attributes, postfix ;
postfix       = primary, {postfix_suffix} ;
postfix_suffix = call_args | "[", expr, "]" | ".", IDENT, [method_tail] ;
method_tail   = call_args | "::", arguments, call_args ;
call_args     = "(", [List1(expr)], ")" ;
primary       = INT | FLOAT | STRING | CHAR | "true" | "false"
              | "(", paren_rest
              | "[", array_rest
              | IDENT, path_tail, [constructor_body]
              | "if", head_expr, value_block, "else", value_else
              | "match", head_expr, "{", {arm}, "}" ;
paren_rest    = ")" | expr, ")" ;
array_rest    = expr, array_tail ;
array_tail    = ";", INT, "]" | array_list_tail ;
array_list_tail = "]" | ",", expr, array_list_tail ;
path_tail     = ["::", path_after_separator] ;
path_after_separator = arguments, ["::", IDENT, path_tail]
                     | IDENT, path_tail ;
constructor_body = "{", [field_value, field_value_tail], "}" ;
field_value_tail = [",", field_value_after_comma] ;
field_value_after_comma = field_value, field_value_tail | ε ;
field_value   = IDENT, ":", expr ;
value_else    = value_block
              | "if", head_expr, value_block, "else", value_else ;
```

All expression auxiliaries inherit the slices of their operators/atoms from section 2. `path_tail` and arguments arrive at 11 for generics, 14 for associated/trait calls, 17 for module paths, and 10 for `Enum::Variant`; simple calls at 5. The proposed path production parses a common neutral path and defers declaration classification; it does not approve arguments on arbitrary intermediate module segments. Those restrictions and explicit generic enum constructors are **OPEN-14/20**. The syntax distinguishes `f::<T>(x)` from `x < y`, with a single-token choice after `::`: `<` starts arguments, IDENT continues the path. Actual calls to a function value become legal only at slice 20.

`head_expr` is the same precedence hierarchy with a separate **head primary**: at enclosing `(`/`[` depth zero it has no constructor-body suffix. Entering grouping, call parentheses, array brackets or index brackets restores ordinary expr for the contents. Thus `if LIMIT {…}` ends the head, while `if test(P { x: 1 }) {…}` permits a literal inside parentheses. `as` also terminates the source of a range borrow, so that source must use the place grammar below rather than unrestricted expr. [OD16](../docs/decisions.md#od16), [OD34](../docs/decisions.md#od34) fix the head rule, not a casing convention.

`attributes` before a postfix are checked to contain only expression-legal `may_not_terminate` permissions, target an indirect call, and attach to that entire immediately following postfix, not the next binary operand ([no-hidden-traps](../docs/decisions.md#decided-project-no-hidden-traps)). The enclosing statement parser consumes a leading attribute prefix only once; it hands the same prefix to either loop or expression classification. Nested attributes inside arguments are parsed by expr itself. Attachment to parenthesized postfix chains, duplicate expression prefixes and interaction with casts are **OPEN-15/16** for the exact frontend judgment.

The place sublanguage, used for assignment checks and borrow-source parsing, is proposed as:

```ebnf
place         = place_base, {place_projection} ;
place_base    = IDENT | "(", place, ")" | "*", place_base ;
place_projection = ".", IDENT | "[", expr, "]" ;
```

Place identity is structural; not every syntactic place is valid for every type. [OD121](../docs/decisions.md#od121), [audit projection rule](../docs/decisions.md#decided-2026-11-02-audit) require a local or projection through a place; bind call results before projecting. No tuple `.0` remains after [OD54](../docs/decisions.md#od54). Heap opening has its own scoped form, not `(&h).f` ([OD36](../docs/decisions.md#od36)); no automatic receiver borrow ([OD32](../docs/decisions.md#od32)): use `(&s).m()` or `(&mut s).m()`. Precise grouped/dereferenced place precedence and method-call versus projection treatment are **OPEN-20**.

| Atom/form | Authority / OPEN detail |
|---|---|
| value if, value match, block tails | [OD77](../docs/decisions.md#od77), [SYN-8/9](../legacy/docs/spec/syntax.md#SYN-8), [STMT-8](../legacy/docs/spec/statements.md#STMT-8); value if requires else; OPEN-16 |
| constructors `S { f: e }`, `E::V { f: e }` | enum separator [OD77](../docs/decisions.md#od77), argument-based enum inference, otherwise explicit [OD129](../docs/decisions.md#od129); struct literal shell slice 1; explicit expression fields recommended under OPEN-20; empty struct literal already admitted by OD65 and approved slice 1 |
| array `[a,b]`, `[e; N]` | repetition evaluated once, Copy [OD149](../docs/decisions.md#od149); empty list literals refused by TYPE-7; separators and allowed length OPEN-14/20 |
| `()` | [OD66](../docs/decisions.md#od66), [OD157](../docs/decisions.md#od157) |
| `Trait::method(&x, ...)` | [OD6](../docs/decisions.md#od6), [audit](../docs/decisions.md#decided-2026-11-02-audit) |
| multilevel module path | [OD151](../docs/decisions.md#od151); imports required [OD139](../docs/decisions.md#od139); name classification [OD152](../docs/decisions.md#od152) |
| borrowed expression `&place` / `&mut place` | [OD1](../docs/decisions.md#od1), [OD2](../docs/decisions.md#od2); parse unary syntax then reject non-place/temporary roots |
| `*r`, `*h`, `*p` | reference 13, heap 19, raw 24; [OD59](../docs/decisions.md#od59), [OD13](../docs/decisions.md#od13); semantics depend on type; raw non-Copy move-out must use read_owned |

No free-standing block expression has been added merely because a value branch has a block. Whether standalone `{…}` is a primary or statement is **OPEN-16**. No tuples, closures, anonymous functions, named call arguments, variadics or operator overloading are inferred from general-purpose expression notation.

### Contract expression restriction

`contract_expr` uses the expression operator grammar over a restricted, pure, non-consuming specification judgment. It is **not** a second permissive executable language. Ordinary runtime calls, allocation, mutation, linear consumption and capabilities do not become contract operations. Calls are to total `spec fn` and explicitly permitted machine operations. Mathematical integer arithmetic is the default; named machine operations retain machine meaning ([OD12](../docs/decisions.md#od12), [OD145](../docs/decisions.md#od145), [OD115](../docs/decisions.md#od115), [OD116](../docs/decisions.md#od116), [CON-3](../legacy/docs/spec/contracts.md#CON-3)). The exact syntactic restriction and allowed specification atom/type vocabulary is **OPEN-13**; the parser can share syntax, while a separate judgment rejects forbidden forms.

`requires` sees parameters at entry and permitted declarations; `ensures` sees result and surviving parameters after cleanup, never body locals or an implicit entry snapshot; loop clauses/assertions see their lexical scope including eligible ghost locals. The November 2 [audit](../docs/decisions.md#decided-2026-11-02-audit) refines [OD143](../docs/decisions.md#od143). No `old`, quantifier, implication, equivalence or snapshot source form ([OD117](../docs/decisions.md#od117)); such properties are Lean theorems. `#[proof(T)]` means a typed theorem path T, not a type argument or string.

## 6. Named operations, secrets, borrows and raw pointers

These names are ordinary builtin paths/calls parsed by section 5. They do not need magic expression alternatives that overlap IDENT. Exact operation signatures are type judgments; the names listed here must still be reserved against user replacement under OD153.

| Construct | Slice | Decided spelling / shape |
|---|---|---|
| wrapping arithmetic | 2 | `wrapping_add(a,b)`, `wrapping_sub(a,b)`, `wrapping_mul(a,b)`; [OD10](../docs/decisions.md#od10), [OD145](../docs/decisions.md#od145), exact six operations in [Core Q6.2](../docs/decisions.md#decided-2026-11-05-core), [ARITH-4](../legacy/docs/spec/arithmetic.md#ARITH-4) |
| saturating arithmetic | 2 | `saturating_add(a,b)`, `saturating_sub(a,b)`, `saturating_mul(a,b)`; same rows; no unapproved family members |
| checked arithmetic | 12 | `checked_add/sub/mul/div/rem/shl/shr(a,b)`, `checked_neg(a)` (each expanded name, not slash syntax); `Result<T,ArithmeticError>`; [no-hidden-traps spellings](../docs/decisions.md#decided-project-no-hidden-traps) |
| arithmetic errors | 12 | `ArithmeticError::Overflow`, `::DivisionByZero`, `::InvalidShift`; enum member names from same decision; signed MIN/-1 div/rem gives Overflow; valid shifts discard bits |
| conversions | integer 2; checked Result 12; floats 26 | `truncate::<T>(x)`, `reinterpret::<T>(x)`, `try_convert::<T>(x)`, `trunc`, `round`, `floor`, `ceil`; [OD99](../docs/decisions.md#od99), [OD106](../docs/decisions.md#od106), [OD107](../docs/decisions.md#od107); exact floating target-argument convention **OPEN-21** |
| try_convert result | 12 | `Result<T,OutOfRange>`, not Option; [OD106](../docs/decisions.md#od106); [ARITH-10](../legacy/docs/spec/arithmetic.md#ARITH-10) |
| Secret | scalar 7; array 15; heap 19 | `ct_eq` in the scalar secret operation inventory (exact overloads OPEN-21), `Secret<T>`, `classify(x)`, `declassify(x, "reason")`, `ct_select`, secret comparisons yielding `Secret<bool>`; [information-flow decision](../docs/decisions.md#decided-project-infoflow-2026-11-06) |
| secret arrays/views | 15 | `ct_eq`; scalar secrets Copy, aggregates linear and wiped; same decision; exact ct_select/ct_eq/comparison API **OPEN-21** |
| text | 16 | `StaticStr`, `byte_at`, scoped subviews; [Core Q4.1–Q4.7](../docs/decisions.md#decided-2026-11-05-core); precise view type/API names **OPEN-21** |
| resources | 6, 18, 27 | `give_up`, `close_dir`, grant-backed ordinary calls; [authority](../docs/decisions.md#decided-2026-11-02-authority), [Core Q7](../docs/decisions.md#decided-2026-11-05-core); Q7.10 already approves the exhaustive hosted grant signatures and contracts; transcribe them without reopening approval (OPEN-4 only closes the combined name inventory) |
| fixed-array replacement | 15 | `swap_at`; [approved slice 15](../ROADMAP.md#vertical-slices); later heap-view replacement follows Core Q3.6 |
| allocation/containers | 19/21/22/23 | `Allocator<A>`, `Heap<T,A>`, `AllocFailure<T>`, `String<A>`; `alloc`, `destroy`, `drain(v,ctx,f)`, `map_into`, `swap_at(v,i,x)`; [allocation](../docs/decisions.md#decided-2026-11-02-allocation), [Core Q1/Q2/Q3](../docs/decisions.md#decided-2026-11-05-core); complete branded signatures **OPEN-21** |
| raw memory | 24 | `ptr::offset`, `ptr::diff` (ordering from sign, no ptr::cmp), `read_owned(p)`, explicit `expose`/`from_exposed`; [OD26](../docs/decisions.md#od26), [OD37](../docs/decisions.md#od37), [OD13](../docs/decisions.md#od13), [Core Q3.7–Q3.11](../docs/decisions.md#decided-2026-11-05-core); exact qualification/signatures **OPEN-23** |

`checked_*` above expands to exactly eight complete names: checked_add, checked_sub, checked_mul, checked_div, checked_rem, checked_shl, checked_shr, checked_neg. Plain arithmetic, indexing, byte_at and unhandled range formation need closed safety evidence; neither trust nor assume silently admits an unproved safe operation ([no-hidden-traps](../docs/decisions.md#decided-project-no-hidden-traps)).

Secret-dependent branch conditions, indices and lengths are refused; the closed operation table also rejects plain secret arithmetic, secret divisors and shift counts. A reason at declassification is a source audit site, not an ambient capability. Scalar classify converts an input value; it is not a hidden entropy source. No new `secret` declaration keyword or cryptographic literal is implied.

### Scoped borrow and view grammar

```ebnf
borrow_statement = "borrow", ["mut"], place, borrow_tail ;
borrow_tail      = "as", IDENT, statement_block
                 | "from", expr, "until", range_bound, "as", IDENT,
                   statement_block, ["else", statement_block] ;
```

Slice 13 introduces ordinary reference borrows; 15 array range views; 16 text ranges; 19 heap-cell opening; 21 heap-array opening/views. The spellings `borrow [mut] source [from a until b] as view {…} [else {…}]` are decided by [Core Q4.7/Q9.5](../docs/decisions.md#decided-2026-11-05-core). There is no `in R` ([OD57](../docs/decisions.md#od57)), and no other heap opening form ([OD36](../docs/decisions.md#od36)). No-range else is not proposed; Q4.7's precise optional-clause combination is **OPEN-19**.

`range_bound` must stop at the delimiter `as`. Ordinary expr also consumes `as` casts; the unrestricted expansion `until expr as IDENT` is **not LL(1)**. **OPEN-19** recommends a bound expression mode with top-level casts disabled and casts allowed in parentheses. Example recommendation: `borrow a from lo until (n as u64) as v {…}`. This restricts only an undecided delimiter edge; it does not relabel the decided `as`.

The source is a place rooted in a binding; exact allowed source projections/grouping is OPEN-19/20. Opening a heap array produces a second-class shared/mutable view, **never an element reference**. Views pass downward only, retain owner identity, obey whole-root conflicts, and use u64 lengths/indices (Core Q3.1–Q3.6). Explicit type names for views are OPEN-21. A no-else range requires evidence of validity; else is the explicit recovery path, not a permission to trap implicitly.

### Raw pointers and FFI

Raw types are `*const T` / `*mut T` (slice 24). Dereference/store use unary `*` and place assignment; `as` is not a blanket pointer/integer conversion mechanism: reference-to-raw conversion is gated by [OD98](../docs/decisions.md#od98). Reference-to-reference casts are refused ([OD103](../docs/decisions.md#od103)); pointer/integer round trips use Q3.9’s `expose` / `from_exposed`, not an invented `as` conversion. Integers carry no pointer provenance; every raw access requires typed validity/provenance/aliasing premises (Core Q3.7–Q3.11). Reference-derived pointers stay confined to the borrow and can never reach a kept parameter; kept retention transfers an owned allocation H by value and returns a linear Kept<H> receipt ([OD64](../docs/decisions.md#od64), Core Q3.12). A write derived from shared &T remains forbidden ([OD112](../docs/decisions.md#od112)).

Slice 25 declares C bindings as `extern fn`, optionally `trusted extern fn`, explicit `with(...)`, mandatory semicolon and no body. `#[link_name = "symbol"]` and `#[export_name = "symbol"]` payload forms are recommendations (OPEN-15), not an inferred `extern "C" {…}` block. Direct calls only: foreign function values and `extern "C" fn` types are not admitted ([OD4](../docs/decisions.md#od4)). FFI-safe types exclude enums ([OD110](../docs/decisions.md#od110)); C structs cross by value at every size ([OD122](../docs/decisions.md#od122)). `kept buf: H`, no marker meaning call-only, and the paired receipt discipline are already decided by the [full Q3.12 row](../docs/archive/decisions-2026-11-05/core_rows.json). The submit contract names a paired extern taking exactly Kept<H> and returning H; borrow-derived pointers are forbidden there. A kept raw pointer is not a substitute for the owned allocation. OPEN-23 concerns remaining raw API qualification and transcription, not these settled choices. Semantic validity obligations must not be replaced by inventing an `unsafe {…}` source block.

## 7. Required forms whose surface is not yet decided

These gaps prevent this document from being a fully closed language grammar. They are first-class owner questions, not omissions to be filled by the parser author.

| Required meaning | Slice | What is already decided | OPEN surface recommendation (not approved) |
|---|---|---|---|
| Brand kind and explicit brand arguments | 19 | separate erased Brand, nominal equality, explicit arguments; Core Q1.1/Q1.4 | **OPEN-22**: `<brand A>` binder, ordinary `::<A>` argument; alternative `<A: Brand>` |
| Fresh brands/reset | 19 | generative binder, reset freshens brand, existential loop state; Q1.2/Q1.13 | **OPEN-22**: `brand A { … }`; alternative `with_brand A { … }`; neither spelling exists by decision |
| Existential formation/opening/repackaging | 19 | explicit pack, lexical unpack, repackage escapes; Q1.3 | **OPEN-22**: `exists<brand A> T`, `pack::<A>(e)`, `unpack p as <brand A> x { … }`; alternative library-shaped scoped eliminator; binder scope/loop-carried packaging also needs approval |
| Downward view parameter types | 15/16/21 | shared/mutable views, no element refs; Q3/Q4.7 | **OPEN-21**: `View<T>` / `ViewMut<T>` as recognized second-class type constructors; alternative `&[T]` / `&mut [T]` (would require an unsized type form) |
| Function values with context | 20 | function pointer plus explicit context, exact capability set | **OPEN-22**: reuse named functions plus explicit context parameters; alternative explicit pair constructor; no closure syntax chosen |
| Safety preconditions in function types, trait methods included | prerequisite of 20 | mandatory design task, no broadening to all non-safety contracts | **OPEN-22**: named parameter binders plus prefix requires on fn type; alternative named contract/interface alias; no usable EBNF until binding/scope approved |
| Concurrency | 28 | only final-slice placement, model not decided | **OPEN-24**: first review scoped spawn/join with linear transfer; alternative explicit runtime handle API; no grammar committed |

Authorities: [Core Q1/Q3/Q9](../docs/decisions.md#decided-2026-11-05-core), [SPARK function-type contracts](../docs/decisions.md#decided-project-spark-2026-11-06), [final-slices decision](../docs/decisions.md#decided-2026-11-07-final-slices). Spec-level `Brand` datatypes are not evidence that a source binder is spelled `brand`. The table deliberately gives tentative syntax as requested, without laundering it into the decided grammar.

## 8. LL(1) review

This is a manual conflict inventory, not the output of a generator. A mechanical FIRST/FOLLOW test must be run on the owner-approved expansions and contextual-token model before encoding the independent grammar in Lean. The frozen [conflict ledger](../legacy/grammar/ll1_known_conflicts.tsv) and [EBNF](../legacy/grammar/concrete.ebnf) are useful controls, not proof that this revised grammar is LL(1).

| Choice/conflict | Kind | Resolution/status |
|---|---|---|
| repeated binary operators | common prefix/left recursion in a naive grammar | precedence ladder and repetition above; no left recursion; left-associative fold; precedence ratification OPEN-8 |
| IDENT followed by `::`, call, fields or nothing | FIRST/FIRST if paths/calls/variants are separate alternatives | consume IDENT once; factor path_tail and postfix; declaration judgment resolves meaning, never initial case (OD152) |
| `:: IDENT` vs `::<…>` | FIRST/FIRST on `::` | consume `::`; next token IDENT or `<` chooses; no rollback |
| field `.name` vs `.name(…)` vs `.name::<…>(…)` | common prefix | consume dot/name once, then dispatch on `(`, `::`, or FOLLOW |
| struct literal `{` vs head's following block | FIRST/FOLLOW | separate head-expression grammar at parenthesis/bracket depth zero (OD16/34); arguments and brackets use ordinary expr |
| range borrow's terminal `as` vs cast repetition | FIRST/FOLLOW | **unresolved** without OPEN-19 bound-mode approval; parenthesized cast recommendation; decided borrow spelling unchanged |
| cast named type `<` vs less-than | FIRST/FOLLOW | OD91: bare cast target has no type-argument suffix; generic target parenthesized; complete composite type expansion OPEN-14 |
| nested generic `>>` vs shift | lexical/context boundary | split into two `>` only while closing generic lists (SYN-7 evidence); shifts unchanged in expression context; OPEN-7 ratifies exact token rule |
| `T`, capability C, brand A generic arguments | FIRST/FIRST if separately IDENT-led | single parsed candidate, kind-check later; names-only capability unions and argument ordering checked; OPEN-14/22 must close the binder grammar |
| reserved Copy vs optional Copy marker | FIRST/FIRST if Copy is IDENT | OD90 reserves Copy; optional marker and struct name now distinct |
| contextual `self`/attribute words vs IDENT | token-class overlap | **OPEN-3/12/13**: no checker may treat unreserved quoted words as disjoint from IDENT; explicit contextual dispatch/table must be modeled; spec/by/kept/Self proposed reserved; existing self remains contextual under OD90 |
| import module path dot vs final dot before `{` | common prefix | consume dot; IDENT continues path, `{` starts list; exact terminal dot approval OPEN-11 |
| trailing comma in a naive `X {',' X} [',']` | FIRST/FOLLOW on comma | explicit list tails consume comma then dispatch X/closing delimiter; this also applies to patterns, arguments, import lists, arrays if trailing commas approved |
| `let IDENT` binding vs struct/enum destructuring | common prefix | consume let/mut/name once; `{` selects destructuring, `:`/`=` ordinary let; consume qualified path before field list; generic heads OPEN-17 |
| patterns `_`, enum path, field binder | FIRST/FIRST if separate IDENT alternatives | parse one name/path; validate role by declaration/context; no arbitrary bare match binder added |
| assignment vs expression statement | FIRST/FIRST, e.g. IDENT or `*` | parse common expression prefix once; after expression choose `=` or `;`; place judgment refuses `f() = x`; do not backtrack between place and expr |
| statement expression vs value-block tail | FIRST/FIRST | common-prefix parse with `;`, `=` or `}` suffix; OPEN-16 must supply the fully expanded grammar |
| statement if/match vs value if/match at block tail | FIRST/FIRST and mode-dependent FOLLOW | **unresolved as plain LL(1) EBNF**: current proposal parses control once and delays branch-mode checking; prove a factored grammar or approve a restriction (OPEN-16); do not claim omission from primary makes it LL(1) |
| block vs block-root expression in arm | FIRST/FIRST on `{` | no standalone block primary in current proposal; parse one arm block; if standalone block expressions approved, factor common `{` once (OPEN-16) |
| `if … else if …` dangling else | FIRST/FOLLOW in an unbraced grammar | branch bodies are braced; else belongs to immediately enclosing braced if; no unbraced statement branch |
| root statement attribute vs expression-prefix attribute | FIRST/FIRST on `#` | one prefix parsed before statement dispatch, handed to loop or expression classifier; suppress only the duplicate root attributed_postfix prefix, restoring it after a unary prefix or nested expression entry (decided OD82 extension); nested expression prefix only at nested expr entry |
| prefix attribute target extent | potential ambiguous attachment | attaches immediately following postfix, before binary continuation; not whole `fp(x)+g()`; no duplicated optional prefix at statement and unary entry |
| `assert(expr)` vs `assert expr` | FIRST/FIRST on `assert` and `(` | parse the complete contract expression once; retain whether its ROOT is grouping. Only then accept by on a grouped root. `(a) == b` has a binary root and cannot take by; exact annotated grammar remains OPEN-13 |
| optional arm separator vs negative next pattern | FIRST/FOLLOW on `-` | require a comma after every arm in this proposal (OPEN-17); otherwise `x -1` can continue the previous expression or begin the next pattern |
| contextual kept vs parameter named kept | FIRST/FIRST if kept is IDENT | reserve kept under OPEN-3; if owner keeps it contextual, consume name once and dispatch on `:` versus the next parameter name |
| named generic trait head | semantic overacceptance, not FIRST/FIRST | impl prefix `<` versus named head is distinct; after parsing, refuse arguments on the trait in `impl Trait for Type` (OD148), and reject capability-only value types |
| empty unit vs grouped expression | common prefix `(` | consume `(`; `)` chooses Unit, any expr starter chooses grouping |
| array repetition vs list | common prefix `[` expr | `;` chooses repetition; comma/`]` chooses list; trailing-comma policy OPEN-20 |
| char vs apostrophe label | lexical prefix conflict | closed char scan before label token classification; OPEN-6 approves exact malformed-input policy; token-level LL(1) does not imply one-byte lexing |
| unary borrow/root dereference versus statement borrow | none after lexing | `&`/`*` vs reserved `borrow` distinct; place validity checked separately |
| struct/loop invariant names | no syntactic ambiguity | same attribute syntax, target kind determined after one attribute parse; attach by kind and reject wrong target |

There is no mechanical acceptance certificate attached. The important owner-visible blockers are OPEN-3, OPEN-14, OPEN-16, OPEN-19 and OPEN-22; several are specification gaps rather than language-spelling conflicts. This sheet refuses to resolve them by changing approved `as`, `if`, `match`, `#[…]`, or path spellings.

## 9. Explicit refusals and migration diagnostics

A diagnostic may recognize these spellings to explain their replacement; recognizing an error is not accepting the form.

| Refuse | Decided source | Suggested diagnostic direction |
|---|---|---|
| inline `mod m {…}` | [OD33](../docs/decisions.md#od33), [OD61](../docs/decisions.md#od61) | use mod m; with m.con |
| `X/mod.con` layout | [OD142](../docs/decisions.md#od142) | use X.con and X/ for children |
| C-style or infinite `for`, collection/reverse/step loops | [OD47](../docs/decisions.md#od47), [OD51](../docs/decisions.md#od51) | while or counted from/until |
| `if let`, `while let`, `let … else` | [OD52](../docs/decisions.md#od52), [OD85](../docs/decisions.md#od85) | explicit match |
| `newtype`, `.0` unwrap, builtin unwrap | [OD54](../docs/decisions.md#od54), [OD155](../docs/decisions.md#od155) | named structs / match as applicable |
| struct update `..base` | [OD53](../docs/decisions.md#od53) | explicit fields |
| guards/range patterns | [OD55](../docs/decisions.md#od55), [OD56](../docs/decisions.md#od56) | explicit branch/comparison |
| `in R` borrow region | [OD57](../docs/decisions.md#od57) | lexical scoped borrow |
| `break value` | [OD27](../docs/decisions.md#od27), [OD77](../docs/decisions.md#od77) | assign an outer result and break |
| trusted mod / trusted impl | [OD61](../docs/decisions.md#od61), [OD22](../docs/decisions.md#od22) | per-function trusted declaration |
| E#V, arm `->` | [OD77](../docs/decisions.md#od77), [OD128](../docs/decisions.md#od128) | E::V and => |
| implicit function return | [OD77](../docs/decisions.md#od77), [STMT-8](../legacy/docs/spec/statements.md#STMT-8) | explicit return; only value blocks have tails |
| union, empty enums, empty generic lists | [OD131](../docs/decisions.md#od131), [OD72](../docs/decisions.md#od72), [OD75](../docs/decisions.md#od75) | no admitted equivalent until a new decision |
| generic traits/generic trait methods | [OD148](../docs/decisions.md#od148), [OD77](../docs/decisions.md#od77) | nongeneric trait interface |
| bare `*T` | [OD74](../docs/decisions.md#od74) | choose *const or *mut |
| call-site `with(Alloc = e)` | [OD83](../docs/decisions.md#od83) | explicit allocator argument plus with(Alloc) |
| capability aliases, Std/other umbrella capability, with(callback) shortcut | [audit](../docs/decisions.md#decided-2026-11-02-audit), [authority](../docs/decisions.md#decided-2026-11-02-authority), [OD92](../docs/decisions.md#od92) | exact explicit capability set/parameters |
| free / take heap API | [OD59](../docs/decisions.md#od59) | *h / destroy(h); other raw replacement API requires explicit approval |
| total fn; old/quantifiers/implication/snapshots | [OD116](../docs/decisions.md#od116), [OD117](../docs/decisions.md#od117) | spec fn or Lean theorem |
| foreign function values | [OD4](../docs/decisions.md#od4) | direct extern calls |
| static mut / global cells / interior mutation through shared borrow | [OD8](../docs/decisions.md#od8), [OD112](../docs/decisions.md#od112) | explicit resources and mutable borrows |
| old type aliases Int/Uint/Float64/Float32/Bool/Char | [OD102](../docs/decisions.md#od102), [TYPE-19](../legacy/docs/spec/types.md#TYPE-19) | canonical scalar names |
| overflow_checked attribute | [no-hidden-traps retirement](../docs/decisions.md#decided-project-no-hidden-traps) | evidence or checked/wrapping/saturating operation |
| process_exit | [OD136](../docs/decisions.md#od136) | return from main; no process spawning on current target |

Postfix `?` is a **decided permanent refusal**, through [P16/refusal statuses](../docs/decisions.md#decided-project-refusals) and [the binding refusal list](../docs/PHILOSOPHY.md#what-concrete-refuses). OPEN-25 does not reopen it. Value `while … else` and postfix member `->` are documented removals in the [frozen grammar](../legacy/grammar/concrete.ebnf); no separate deciding row was located for those two. OPEN-25 requests ratification only for them. The function result arrow remains. Block-comment and exponent policy still needs OPEN-1/5 approval.

Also refuse exceptions/unwinding, capturing closures, trait objects/hidden dynamic dispatch, source macros/derive, derived equality, default and variadic functions, shadowing, implicit conversions and truthiness under the [binding refusal list](../docs/PHILOSOPHY.md#what-concrete-refuses) and [P16–P17](../docs/decisions.md#decided-project-refusals). Diagnostics should point to explicit match/error arms, named functions/context, static dispatch, reviewed generated source, written arguments and canonical numeric conversions as applicable. General comptime and broad `#[cfg]` remain refused pending the stated workload/proof/audit reopening conditions; they are not permanent refusals. `ptr::cmp` is refused by [OD37](../docs/decisions.md#od37) (use the sign of ptr::diff), and reference-to-reference casts by OD103. No new refusal is decided here.

`discard(e)` is an ordinary reserved builtin call, not a missing keyword statement: [builtin catalog](../legacy/docs/spec/types.md#builtin-functions), OD62/OD153. Its early availability at slice 3 is an explicit builtin-call exception before general calls at slice 5. Adding `discard expr;` would be a new spelling proposal, not a correction.

Further checked refusals, rather than grammar exclusions: missing usage of named bindings, dropping linear/Result/Option via `_` or discard, no expected literal type, invalid receiver auto-borrow, non-place projections, unproven safety, secret-dependent control, invalid repr targets, wrong attribute attachment, builtin collisions. They need specific diagnostics from the relevant judgments, not fake parse errors.

## 10. Numbered OPEN items for owner approval

Every recommendation below is a proposal. Alternatives are supplied for comparison, not implemented. An approval of one does not imply approval of unrelated syntax in its group.

1. **OPEN-1 — Source trivia/byte policy (slice 0).** Recommend valid UTF-8, space/tab/LF/CR whitespace, `//` to LF, CR or EOF, no block comments/BOM/shebang/raw NUL. Alternative: allow UTF-8 BOM and nested block comments. Reason: a small explicit byte language matches SYN-5/6 without inheriting host lexer behavior. CRLF leaves LF as whitespace.
2. **OPEN-2 — Identifier alphabet/case (0).** Recommend ASCII `[A-Za-z_][A-Za-z0-9_]*`, case-sensitive; no casing-based category. Alternative: Unicode XID with a specified normalization policy. Reason: ASCII is auditable and matches old EBNF; OD152 still governs declaration-based resolution.
3. **OPEN-3 — Full keyword table and contextual tokens (0–25).** Recommend the complete reserved list in section 2, retain self/repr/packed/align contextual, reserve spec/by/kept/Self, and propose `Entropy` for the new entropy capability subject to owner approval. Alternative: keep spec/by/kept contextual with a fully factored token model, while retaining the existing special Self type meaning. Reason: preserve explicit existing reservations and contextual self under OD90 while making contextual FIRST sets honest. Also decide whether obsolete in/newtype/union/total stay reserved for diagnostics; no current row settles that complete set.
4. **OPEN-4 — Reserved declaration/builtin inventory (by slice).** Recommend one explicit closed table of every admitted builtin type, grant type, primitive and function, with compiler prefixes and collision scopes; emit no aliases. Alternative: reserve only a compiler namespace and qualify builtin calls. Reason: OD43/133/153 require a publishable complete set; the present materials do not enumerate the final branded/secret/grant surface in one place. This approves an inventory, not unseen APIs. Q7.10 already approves the hosted grant table; only consolidation/transcription is outstanding there.
5. **OPEN-5 — Numeric token details (2/26; leading-zero decision already binds 0).** Recommend lowercase base prefixes, no separators; retain typed integer suffixes to preserve the approved Q6.2 example, with exact suffix grammar still for approval, digits-dot-digits floats without exponents; reject malformed adjacent numeric text as one diagnostic. Alternative: retain uppercase prefixes and add explicit separators/exponent forms. Reason: do not silently contradict `200u8` in the approved field example; freeze the accepted token language independently of lexer accidents; do not reopen decided decimal-leading-zero refusal.
6. **OPEN-6 — Quoted tokens/escapes (16; metadata strings earlier).** Recommend precisely the two escape lists in section 2, no raw line breaks/control characters, char-first closed-quote recognition before labels, malformed quotes refused. Alternative: add hex/Unicode escapes and multiline text. Reason: SYN-5/6 give a small historical set, but source lexical edge cases need explicit approval; Core Q4.2's embedded NUL remains allowed through \0.
7. **OPEN-7 — Punctuation and tokenization (by operator slice).** Recommend exactly section 2's token set, longest match, contextual >> splitting only in generic closers. Alternative: lexer always emits single > tokens and parser composes shifts. Reason: preserve expression shifts and nested generics without whitespace-dependent syntax.
8. **OPEN-8 — Complete precedence/associativity (2/3/13+).** Recommend section 2's legacy SYN-2 table, binary and casts left-associated, unary right-associated, postfix tightest. Alternative: reject repeated comparison/equality operators syntactically. Reason: the old table is precise but no dedicated decisions.md row approves every level.
9. **OPEN-9 — Declaration shell/prefix/list closure (1/5/9/10/17/24/25).** Recommend attributes then pub then declaration/trusted, the item shells in section 3, mandatory field/variant commas with optional trailing comma, and bodyless functions only where an explicit interface/extern rule permits. Alternative: pub before attributes and no trailing commas/bodyless ordinary declarations. Reason: OD77 approves bodyless syntax without settling all placement/linkage or modifier combinations; no prefix should silently authorize pub impl or trusted spec fn.
10. **OPEN-10 — Alias/constant spelling (9/17).** Recommend `type N = T;` and `const N: T = e;`, no generic aliases unless separately approved. Alternative: `alias N = T;` and inferred constants. Reason: retain historical spellings while acknowledging that semantic approval alone does not settle each delimiter.
11. **OPEN-11 — Imports (17).** Recommend `import a.b.{x, y as z};`, nonempty list, mandatory commas, optional trailing comma after factoring; wildcard already refused by [MOD-2](../legacy/docs/spec/modules.md#MOD-2), no whole-module alias or alternate missing-dot form. Alternative: `import a.b {x, y as z};`. Reason: old parser accepts both final-dot styles, so the owner should choose one canonical form.
12. **OPEN-12 — Trait/impl/receiver shells (14).** Recommend `impl Trait for Type`, `impl Type`, method self/&self/&mut self, and signatures from section 3, no where clause or associated types. Alternative: typed receiver parameters only. Reason: use actual legacy direction and the one consuming receiver protocol without copying the stale reversed grammar comment.
13. **OPEN-13 — Proof statement/spec body/contract grammar (8).** Recommend `spec fn f(...) -> T = e;`, `assert e;`, `assert(e) by path(args);`, `assume e;`, ghost let restricted to a plain name binding, a separately checked pure expression subset. Alternative: braced spec bodies and only parenthesized assertions. Reason: OD115 fixes required total bodies, not their full grammar; OD79/SPARK fix spellings that still need one factored production. Contract atom/types and theorem-argument syntax require explicit closure.
14. **OPEN-14 — Generic/type grammar closure (11/15/19/20).** Recommend nonempty comma lists, type parameters before cap parameters, type/brand arguments before capability arguments, `cap C`, `+` name unions and `{}` empty set, `[T; INT]`, generic cast targets parenthesized; parse named arguments once then check kind. Alternative: explicit kind-tagged arguments and semicolon-separated capability arguments. Reason: respect OD88/Q5 without pretending IDENT types and sets have disjoint FIRST sets. Approve qualified names, composite cast targets, array length expressions and union spelling inside with individually.
15. **OPEN-15 — Attribute closure/target rules (4–25).** Recommend section 3's closed ordinary-source attribute set; link/export `= "symbol"`; no source intrinsic/langitem or legacy proof_by/ensures_proof/proof_coverage/proof_fingerprint metadata without new approval; test functions already parameterless (STMT-10), result u8 (OD146 revised); do not reopen either. Alternative: keep an explicitly segregated compiler-only attribute grammar and additional historical proof metadata. Reason: OD77 requires unknown attributes refused, so the final list cannot be inferred from a permissive legacy whitelist. Also approve duplicate options, ordering, struct-invariant field scope and expression-prefix edge cases.
16. **OPEN-16 — Blocks and LL(1) statement/value modes (3/10/20).** Recommend retain decided statement/value behavior, explicit function returns, no standalone block primary, and derive one common-prefix block grammar with delayed control-node classification as described in section 4. Alternative: require explicit grouping of tail if/match or a tail marker, **only after checking and explicitly revising any decided acceptance it changes**. Reason: a position-blind grammar with both `{stmt}` and optional expr is not LL(1); the independent grammar must settle this rather than omit value alternatives. Approval must include a mechanical check, not just the algorithm description.
17. **OPEN-17 — Exact kept pattern/arm surface (9/10/16).** Recommend qualified enum patterns, `_` catch-all, integer/bool/char literals, OR patterns, named field renaming, whole-payload `{..}` and a mandatory comma after every arm. Keep irrefutable enum lets, mut applying to every binder, explicit field handling and the deferred status of nested patterns. Alternative: comma only after non-block arms, subject to FIRST/FOLLOW proof. Clarify OD25’s “bare” fieldless wording against the chapter’s qualification rule; explicitly disposition generic `E::<T>::V` and module-qualified let paths. Do not treat the frozen parser as authority for a new accepted form.
18. **OPEN-18 — Ordinary statement/label details (0/3/4/9).** Recommend semicolon-terminated let/return/assignment/expression statements, unannotated lets where typing determines a type, `'label: while/for`, `break 'label;`/`continue 'label;`, no empty statement or block-local item. Alternative: distinct `label name` declaration and explicit type on every binding. Reason: slice availability and semantic decisions do not individually approve every inherited label/semicolon rule.
19. **OPEN-19 — Borrow-bound delimiter and defer target (13–21).** Recommend place-only borrow source, top-level no-cast range_bound with parenthesized casts, else only for range form, no-else requires evidence; defer accepts a syntactic direct/associated/method call (indirect call when slice 20 admits it), checked Unit result, no arbitrary expression/block. Alternative: require parentheses around both borrow bounds and defer call arguments at a dedicated defer form. Reason: preserve Q4.7's as delimiter and Unit-only deferred calls while eliminating FIRST/FOLLOW ambiguity. Ordinary borrow syntax remains decided.
20. **OPEN-20 — Constructors/projections/array expression details (1/9/10/15/17).** Recommend declaration-neutral paths, explicit constructor field values (no expression puns); retain decided pattern punning and empty struct literals, nonempty array list literals (TYPE-7) and repeat arrays (zero-count policy still for approval), no tuple projection, place-based field/index access, calls to values only at 20; explicitly factor trailing commas. Alternative: retain expression puns. Reason: explicit values distinguish field labels from moved locals; close historical conveniences without extending removed newtypes or call-result projections. Choose exact generic enum construction placement and grouped/dereference place grammar.
21. **OPEN-21 — API/view spellings and branded signatures (7/15/16/19/21–27).** Recommend View/ViewMut second-class parameter constructors and ordinary calls for the decided ct_select/ct_eq/text/container APIs; use exact explicit branded type arguments; floating conversions use `name::<Target>(x)`. Alternative: &[]-style view types and qualified associated APIs. Reason: feature names in Q3/Q4 and the information-flow decision do not decide every arity, type position, qualifier or conversion target syntax. Submit the exhaustive concrete signature table before encoding it; no fabricated complete signatures here.
22. **OPEN-22 — Brand/package/function-contract surface (19/20).** Recommend the explicit brand/exists/pack/unpack proposals in section 7 and named binder contracts on fn types, reusing ordinary explicit context parameters for function values. Alternative: scoped library eliminators plus named contract aliases. Reason: Core Q1 approves these mechanisms, while R-0601 explicitly still owns a binding design; surface syntax cannot be recovered from ANF constructors. Approve each binder, packaging expression, escape and loop-carried existential rule separately.
23. **OPEN-23 — Remaining raw/FFI API closure (24/25).** Preserve decided `kept buf: H` for an owned allocation, unmarked call-only parameters, paired `Kept<H>` completion, and the absolute ban on borrow-derived kept arguments (full Q3.12). Recommend ptr-qualified raw APIs except decided `read_owned(p)`, and extern fn without ABI-string blocks. Alternative for the remaining qualification choice: global raw operation names. Marker placement/default and receipt signatures are transcription obligations, not open alternatives.
24. **OPEN-24 — Concurrency (28).** Recommend present scoped spawn/join with linear transfer for owner model review before proposing tokens. Alternative: an explicit handle-based runtime API. Reason: the slice decision explicitly leaves the model undecided; neither option is currently Concrete syntax.
25. **OPEN-25 — Two historical refusals needing located authority.** Recommend ratify refusal of value while…else and postfix member ->; keep the function result arrow. Alternative: consider each separately as a new proposal. Postfix ? is already permanently refused and is excluded from this question. Reason: historical removal alone must not be assigned an invented deciding OD.

### Reviewer choices and v2 disposition

“Agree” means agreement with v1, not approval by the owner. The named choices below are taken from final answer sections only. Codex/Kimi checked the repository; agy could not. All 25 original IDs are retained so narrowed questions remain traceable.

| OPEN | Topic | Codex | Kimi | agy | v2 recommendation and reason |
|---|---|---|---|---|---|
| 1 | LF/CR/EOF comments | Agree; clarify CR | LF/CR/EOF | Agree LF/EOF | Changed to CR termination too: matches proposed whitespace without swallowing a CR-only line. |
| 2 | ASCII identifiers | Agree | Agree | Agree | Keep; no case heuristic. |
| 3 | Keywords | Agree v1 | Reserve spec/by/kept/Self/Entropy; add while | Reserve spec/by/kept/self/discard/Entropy | Reserve spec/by/kept/Self, add while; keep self contextual (OD90), discard a builtin; Entropy remains proposed. |
| 4 | Name inventory | Agree; grant signatures settled | Agree | Agree | Keep consolidation; exclude already-approved Q7.10 signatures from reapproval. |
| 5 | Numeric tokens | Agree no suffixes | No suffixes; revise Q6.2 example | Agree no suffixes | Change: retain suffix evidence from 200u8; ask exact suffix grammar. Do not rewrite an approved example by recommendation. |
| 6 | Escapes | Agree | Agree | Agree | Keep. |
| 7 | Token splitting | Agree | Agree | Agree | Keep. |
| 8 | Precedence | Agree | Agree | Nonassociative comparisons; remove cast postfix | Keep legacy table and cast continuation evidence; no demonstrated reading gain justifies another restriction. |
| 9 | Declarations | Agree | Agree | No trailing commas; bodyless only trait/extern | Keep proposed shell; ordinary bodyless placement still open. Simplification list separately asks removal. |
| 10 | Aliases/constants | Agree | Agree | Agree | Keep. |
| 11 | Imports | Agree | Agree; final dot; glob already refused | Final dot; no trailing comma | Keep final dot; wildcard refusal is settled, list punctuation remains open. |
| 12 | Receivers | Agree | Only self/&self/&mut self; no self:T | Agree shorthand only | Keep shorthand as canonical receiver; typed self cannot add a second Destroy protocol. Separate proposal may remove dot calls. |
| 13 | Proof syntax | Agree | Agree except name-only ghost | Parenthesized assert/assume only | Keep existing assert/assume forms and grouped-root by rule; fix ghost name restriction. Parenthesis unification is a separate owner proposal. |
| 14 | Types/generics | Agree | Agree | Kind-separated caps; type/brand before cap | Keep kind-neutral argument candidate, but explicitly reject cap names as ordinary types; ordering already constrained by OD88. |
| 15 | Attributes | Agree | Agree; test shape settled | Agree | Keep; test shape is not OPEN. |
| 16 | Blocks | Agree conditional on mechanical expansion | Agree | All statements end ; or explicit return/grouping | Keep decided value behavior; grammar proof remains required. New semicolons would revise accepted syntax. |
| 17 | Patterns/arms | Mandatory arm commas | Qualified only; settled fields; no nesting; mandatory commas | Qualified only; no trailing field commas; nonblock arm commas | Require every arm comma; retain decided fields/no nesting. Bare wording conflict remains explicit; generic heads need disposition. |
| 18 | Statements | Agree | Agree | All typed lets; add discard statement | Keep inference proposal outside slice 0; discard(e) already ordinary builtin. Explicit types separately recommended for owner review. |
| 19 | Borrows/defer | Agree | Agree | Agree | Keep restriction proposal; Q4.7 clause combination remains explicit question. |
| 20 | Constructors | Explicit expression fields; keep pattern puns | No empty arrays/puns; empty structs kept | No puns/trailing commas | Change to explicit expression fields; retain pattern puns; reject empty array lists and retain empty structs. |
| 21 | API/views | Agree | Agree | Agree | Keep; distinguish scalar ct_eq inventory and array/view extension; add slice-15 swap_at. |
| 22 | Brands/fn contracts | Agree subject to separate binder/scope approvals | Agree | Agree | Keep proposals pending owner; no inferred approval from Core constructors. |
| 23 | FFI | Owned kept H, default settled, no borrowed retention | Agree v1; reserve kept | Agree v1 | Correct to full Q3.12; remaining qualification only is open. |
| 24 | Concurrency | Agree | Agree | Agree | Keep model review before tokens. |
| 25 | Historical refusals | ? settled; ratify other two | ? settled; refuse other two | Agree ratify all three | Exclude decided ?; ask only while…else and member ->. |

Recommendations follow [PHILOSOPHY.md](../docs/PHILOSOPHY.md#the-rules): explicit authority and control, one spelling per situation, and one independent checkable definition take precedence over convenience. They do not reopen decided rows.

Review record: v2 reconciles the final Codex (6 findings), Kimi (18 findings) and agy (10 findings plus citation checklist) sections. agy did not read the repository; every assertion is therefore checked here rather than treated as verified by that reviewer. See GRAMMAR_CHANGES.md for individual outcomes. Recommendations remain unapproved; the owner alone decides revisions. The reviewers have not reviewed this merged v2. No compiler, legacy gate or parser was run; there is no mechanical LL(1) certificate. Only this review artifact is requested for approval; it changes no language rule or implementation.
