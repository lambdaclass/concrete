# Open Owner Decisions

This chapter lists the language questions that only the project owner decides: OD1 to OD105 of
[ROADMAP.md](../../ROADMAP.md#owner-decisions) (exit 2n for OD1 to OD32, exit 2q for the rest). Each entry states the question and encodes
no answer. The rules it bears on are named in its rationale, together with what current main does,
which stays provisional until the owner answers. The answer, when given, is recorded in ROADMAP and
turns the entry into the rules it decides; until then no agent picks a side
([AGENTS.md](../project/AGENTS.md#rules)). OD1 to OD22 were answered on 2026-10-29 and are listed under
Decided; OD23 to OD32 were decided the same day; OPEN-33 to OPEN-46 and OPEN-48 to OPEN-51, raised while recording them, are open, and so are OPEN-52 to OPEN-91, allocated on 2026-10-30 for that day's owner picks, OPEN-92 to OPEN-103, raised the same day while absorbing the last docs/language documents, and OPEN-104 and OPEN-105, raised the same day by the review of the loop rules.

## Decisions to be made, in order

Every open question of this chapter, in the order the owner is asked to read them: the removals first,
because they make other questions moot, then the questions grouped by area. Each row names the item's
label in the owner picks of 2026-10-30 and the coordinator's pick after that day's review, which is a
recommendation and not a decision; the entry below states the question, the options and what main does.
The picks mark Q1, Q2, Q8, Q9 and S14 as already agreed by the owner (ROADMAP records no dated answer for
them); they are not separate entries here
(OPEN-37 and OPEN-61 cite Q8 and S14).

| # | entry | label | question | Austral | pick (not a decision) | proof impact | deletes or simplifies |
|---|---|---|---|---|---|---|---|
| 1 | [OPEN-52](#OPEN-52) | S1 | `if let`, `while let` and `let ... else` | none of the three; `case` is the only refutable form | (b) remove | helps | deletes PAT-6, the `let ... else` rule of PAT-8 and three grammar forms; settles OPEN-40 (no synthesized fallback arm remains) |
| 2 | [OPEN-53](#OPEN-53) | S2 | Struct update `..base` | no struct update; every field is written | (b) remove | helps | deletes one aggregate form and its value-flow row in OWN-15 (the partial move out of `base`) |
| 3 | [OPEN-54](#OPEN-54) | S3 | `newtype` and `.0` | no newtype; a type is made opaque by the module interface hiding its definition (a record being one possible definition) | (b) remove | helps | deletes a declaration kind, the `.0` projection, the newtype cast rule of ARITH-5 and the `unwrap` intrinsic |
| 4 | [OPEN-55](#OPEN-55) | S10 | Match guards | no guards | (b) remove | needs requirement 4 | deletes PAT-4 and the guard case of exhaustiveness (a guarded arm never counts toward coverage) |
| 5 | [OPEN-56](#OPEN-56) | S11 | Range patterns | no range patterns | (b) remove; then close OPEN-41 as range-specific, recording in PAT-2 and PAT-9 that an integer match still needs a catch-all and that listing every literal is not coverage, so that catch-all is never unreachable | needs requirement 4 | deletes the range half of PAT-7 and the range-coverage question (OPEN-41) |
| 6 | [OPEN-58](#OPEN-58) | S12 | Borrow blocks stay | the borrow statement is the way to hold a reference across statements | (a) keep (recorded as already agreed in the picks; no owner record yet) | helps | nothing; it is the form OD36's pick builds on |
| 7 | [OPEN-57](#OPEN-57) | S12b | The region name of a borrow block | the borrow statement names its region (`borrow x as r in R do ... end`); anonymous borrows `&x` in call arguments name none | (b) drop `in R` | helps if the internal loan and scope identities stay | deletes a name with no use and its scope rule |
| 8 | [OPEN-59](#OPEN-59) | S7 | One way to consume a `Heap` | not checked | (b): `*h` and a `destroy(h)` extended to a Heap, no `free`, no `take` (a `take` would be another spelling of `*h`) | helps | deletes `free` and one of three spellings of the same move-out |
| 9 | [OPEN-60](#OPEN-60) | S8 | One consuming receiver form for `Destroy` | no destructor trait; a linear type's module exports an ordinary consuming function | (b) one consuming form | helps | one signature for every `Destroy` impl |
| 10 | [OPEN-33](#OPEN-33) | — | Inline submodules under one file per module | one module per file pair, no inline modules | no inline `mod m { }`: one file per module | helps | deletes inline submodules; moves the specification's examples into project examples |
| 11 | [OPEN-61](#OPEN-61) | review new 7 | One coordinated removal list | not applicable | (a) one list, including dropping TRUST-6's `trusted mod` marker, which S14 removes (S14 is marked agreed in the picks; ROADMAP records no owner answer for it) | neutral | one migration of std, examples and tests instead of ten |
| 12 | [OPEN-62](#OPEN-62) | Q3 | Unused values and variables | not checked | (b), stated as one invariant: every name a body binds is used or is `_` (a bodyless declaration has no body, so needs no exception); `discard` and wildcard drops refuse `Result` and `Option`, Copy or not; to clarify: `_` as a loop index, and an unused `&mut` binding under REF-10 | needs requirement 3 | one rule for every unused name instead of a type-specific one |
| 13 | [OPEN-63](#OPEN-63) | Q4 | Float literals take their expected type | not checked | (a) | needs a specified semantics | one literal rule for integers and floats |
| 14 | [OPEN-40](#OPEN-40) | — | PAT-9 and the fallback arm of a desugared form | not applicable (no `if let`) | moot if OD52 removes the desugared forms | helps | — |
| 15 | [OPEN-41](#OPEN-41) | — | PAT-9 and integer ranges that span their type | not applicable (no range patterns) | close as range-specific under OD56, recording in PAT-2 and PAT-9 that an integer match still needs a catch-all and that listing every literal is not coverage | helps | — |
| 16 | [OPEN-85](#OPEN-85) | C8 | Variant destructuring with `else` in the grammar | no `let ... else` | follows OD52 (removed under its pick) | follows OD52 | — |
| 17 | [OPEN-76](#OPEN-76) | G1-b | Unqualified struct patterns in `match` | not checked | (a) refuse in `match`; fix PAT-1's wording | neutral | one place a struct is destructured |
| 18 | [OPEN-78](#OPEN-78) | C1 (review new 3) | `mod X;` in the grammar | each module is its own pair of files | (a) add, item by item | neutral for Core (loading stays frontend correspondence) | — |
| 19 | [OPEN-39](#OPEN-39) | — | The interface artifact format | interface files are written by hand | the `pub` declarations rendered as Concrete source without bodies, generated by the compiler, with an API-diff gate; it also carries private layout, impl methods, attributes, constants, capabilities and generic bodies, and is not read as FFI-6 bodyless foreign bindings | needs requirement 5 | — |
| 20 | [OPEN-44](#OPEN-44) | — | Two packages with one module name | not applicable | (a) refused at load, naming both packages, until packages depend on packages (R-0440) | neutral for Core | — |
| 21 | [OPEN-43](#OPEN-43) | — | How a type position resolves a name | builtin type names are reserved | (c) refused, for builtin-name collisions only, with the complete reserved set published (also for aliases and import renames); this answer would not retire OPEN-43, whose type-versus-value lookup and child-module questions stay open in it. A user `struct Copy HeapArray` is treated as linear today | helps | removes the shadowing question |
| 22 | [OPEN-67](#OPEN-67) | review new 4 (SCOPE-4) | Locals that reuse a top-level name | not checked | none given (raised by the review) | depends on the option | (b) would make SCOPE-1 one rule for every name |
| 23 | [OPEN-34](#OPEN-34) | Q5 | A struct literal inside brackets in a condition head | not applicable (records are built with `R(f => v)`) | only an enclosing `(` or `[` parenthesizes, stated as one invariant: in a condition head, a `{` at bracket depth zero opens the block | neutral for Core | one invariant, no exception |
| 24 | [OPEN-50](#OPEN-50) | — | Reserved or contextual `from` and `until` | its loop words are reserved | (a) reserved everywhere | neutral | one lexical rule |
| 25 | [OPEN-70](#OPEN-70) | P2 | Struct fields with no comma | separators are required | (a) refuse | neutral | one separator rule |
| 26 | [OPEN-71](#OPEN-71) | P3 | Enum variants with no comma | separators are required | (a) refuse | neutral | one separator rule |
| 27 | [OPEN-72](#OPEN-72) | P4 | An enum with no variants | not checked | (a) refuse until a workload needs an uninhabited type | neutral | no empty-match question |
| 28 | [OPEN-73](#OPEN-73) | P5 | Reserved words used as identifiers | keywords are reserved | (a) refuse | neutral | one lexical rule |
| 29 | [OPEN-74](#OPEN-74) | P6 | A raw pointer type with no qualifier | not applicable | (a) refuse; the qualifier is the safety-relevant part | neutral | one spelling per pointer type |
| 30 | [OPEN-75](#OPEN-75) | P7 | An empty type parameter list | not applicable | (a) refuse | neutral | one spelling |
| 31 | [OPEN-90](#OPEN-90) | Copy | `struct Copy <T>` and contextual keywords | its keywords are reserved | (a) reserve `Copy`: one lexical rule, consistent with OD50 (reserved `from` and `until`) and OD43 (builtin names refused as user names) | neutral | removes the `struct Copy <T>` ambiguity |
| 32 | [OPEN-91](#OPEN-91) | LL(1) type\|5.2 | A cast followed by `<` | a conversion is the cast expression `e : T`, with no type arguments after it | none given (in the LL(1) ledger, not the picks) | neutral for Core | — |
| 33 | [OPEN-77](#OPEN-77) | B | Resync the grammar to the decided language | not applicable | (a) approve, with actual productions, a `place` prefix and `break <expr>` removed | needs | the grammar and the parser agree on the decided forms |
| 34 | [OPEN-79](#OPEN-79) | C2 | `assert` in the grammar | not checked | individual review required; no pick recorded | needs the production reviewed on its own (no blanket neutrality) | — |
| 35 | [OPEN-80](#OPEN-80) | C3 | `assume` in the grammar | not checked | individual review required; no pick recorded | needs the production reviewed on its own (no blanket neutrality) | — |
| 36 | [OPEN-81](#OPEN-81) | C4 | `ghost let` in the grammar | not applicable | individual review required; no pick recorded | needs the production reviewed on its own (no blanket neutrality) | — |
| 37 | [OPEN-82](#OPEN-82) | C5 (R7) | Loop contract attributes and where attributes attach | not checked | (a): attach by kind, declaration attributes to the next declaration, `#[invariant]` and `#[variant]` to the next loop | needs review | — |
| 38 | [OPEN-83](#OPEN-83) | C6 | The call-site allocator binding in the grammar | not checked | individual review required; no pick recorded | needs the production reviewed on its own (no blanket neutrality) | — |
| 39 | [OPEN-84](#OPEN-84) | C7 | Struct destructuring `let` in the grammar | destructuring `let` of records exists (not checked) | individual review required; no pick recorded | needs the production reviewed on its own (no blanket neutrality) | — |
| 40 | [OPEN-86](#OPEN-86) | R1 | The body of an unbraced match arm | not applicable | (a) narrow | neutral | — |
| 41 | [OPEN-87](#OPEN-87) | R2/R3 | Capability arguments in an expression turbofish | not applicable | (a) narrow | neutral | — |
| 42 | [OPEN-88](#OPEN-88) | R8 | The shape of capability arguments | not applicable | (a) | neutral | — |
| 43 | [OPEN-89](#OPEN-89) | R9 | Capability parameters on a trait impl | not applicable | none given (in the divergence report, not the picks) | neutral | — |
| 44 | [OPEN-45](#OPEN-45) | — | Capability-variable inference | not applicable (capabilities are values) | (b) bind from the checked type of every argument; refuse unresolved or inconsistent ones; no result-context inference | neutral for Core | inference no longer depends on the argument's spelling |
| 45 | [OPEN-46](#OPEN-46) | — | Generic type-argument inference | type parameters are inferred from the arguments, and a `let` declaration's type can disambiguate a return-type-polymorphic call | (b) from checked argument types only, never from the expected result; arguments that need an expected type are refused, not defaulted | neutral for Core | inference no longer depends on the argument's spelling |
| 46 | [OPEN-42](#OPEN-42) | — | Comparisons on `bool` and `char` | not checked | keep `==` and `!=` on `bool` and `char` and ordering on `char`, refuse ordering on `bool`; amend TYPE-6 and OD26 explicitly | neutral (char comparisons are not thereby admitted to the proof subset) | — |
| 47 | [OPEN-66](#OPEN-66) | review new 2 | The unit literal `()` | the unit value is written `nil` | none given (raised by the review) | neutral | — |
| 48 | [OPEN-65](#OPEN-65) | review new 1 | Empty structs | not checked | none given (raised by the review) | needs requirement 6 | — |
| 49 | [OPEN-35](#OPEN-35) | Q6 | How an array satisfies a `Destroy` bound | no destructor trait | a language impl `[T; N]: Destroy` when `T: Destroy`, elements destroyed in index order, with its own identity in coherence | neutral with conditions | — |
| 50 | [OPEN-69](#OPEN-69) | review new 6 | What "the remaining locals are destroyed" means | no implicit destruction | none given (raised by the review) | depends on the option | — |
| 51 | [OPEN-68](#OPEN-68) | review new 5 | Failure inside a deferred call | not checked | none given (raised by the review) | depends on the option | — |
| 52 | [OPEN-36](#OPEN-36) | Q7 | The explicit place form that opens a heap cell | the borrow statement | only through a borrow block (`borrow h as r { r.f }`), no `(&h).f`; specify the result type for `Heap<T>`, `&Heap<T>` and `&mut Heap<T>`, one heap layer per block (MEM-4), and `HeapArray` elements | helps | no new expression form |
| 53 | [OPEN-37](#OPEN-37) | Q8 | The names of the pointer difference and ordering functions | no named difference or ordering functions; its `Pointer` and `Address` types are compared with the ordering operators | one trusted `ptr::diff` (same-allocation premise), ordering read from its sign, no `ptr::cmp`; specify the representability of its result, one-past pointers included (Q8 is marked agreed in the picks; ROADMAP records no owner answer for it) | needs the trusted pointer operations specified and preserved across memory lowering | one function instead of two |
| 54 | [OPEN-38](#OPEN-38) | — | The root resource API | `main(root: RootCapability)` | `fn main(root: Root) -> u8` (or Unit), `Root` split into linear tokens by trusted functions; specify the split functions and how leftover authority is consumed | needs requirement 1 | — |
| 55 | [OPEN-64](#OPEN-64) | G19 | Raw pointers derived from a borrow | not checked | (a): no returning, no escaping storage (a local `let` inside the block stays allowed), no converting to an integer and no capturing in an outer `defer`; borrow-derived pointers go only to call-only `extern` parameters; C uses it only during a call (memory C keeps must come from an allocation with its own owner); `extern` parameters say call-only (the default) or kept; field pointers come from the same borrow of the whole owner; deferred calls in the block run before the borrow ends; provenance survives copies, offsets, field pointers, helpers and indirect calls; a shared borrow never yields write access; `ptr::expose` is refused on such a pointer; the annotation's syntax gets an LL(1) check | needs | one validity rule for every borrow-derived pointer |
| 56 | [OPEN-48](#OPEN-48) | — | The index type of a counted loop over two literals | not checked | (a) refused, as OD31 refuses any literal with no expected type; a typed bound supplies the other bound's type, in either order | neutral | — |
| 57 | [OPEN-49](#OPEN-49) | — | The arithmetic of a counted loop's termination measure | not applicable | (a) a mathematical integer in the proof only, never computed at run time | needs requirement 2 | — |
| 58 | [OPEN-51](#OPEN-51) | — | Reverse, stepped and collection iteration | a counted loop and `while` | (a) none now; each a later decision when a workload needs it | neutral | one loop construct in proofs |
| 59 | [OPEN-92](#OPEN-92) | absorption | A capability set inferred from callbacks | not checked | none given (raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES) | not assessed | — |
| 60 | [OPEN-93](#OPEN-93) | absorption | Widening a function value's capability set | not checked | none given (raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES) | not assessed | — |
| 61 | [OPEN-94](#OPEN-94) | absorption | Names in `with(...)` that are not capabilities | not checked | none given (raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES) | not assessed | — |
| 62 | [OPEN-95](#OPEN-95) | absorption | What `Time`, `Env`, `Process` and `Alloc` mean | not checked | none given (raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES) | not assessed | — |
| 63 | [OPEN-96](#OPEN-96) | absorption | Two foreign bindings of one C symbol | not checked | none given (raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES) | not assessed | — |
| 64 | [OPEN-97](#OPEN-97) | absorption | A build profile that omits or demands an overflow check | not checked | none given (raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL) | not assessed | — |
| 65 | [OPEN-98](#OPEN-98) | absorption | Pointer and reference conversions written with `as` | not checked | none given (raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL) | not assessed | — |
| 66 | [OPEN-99](#OPEN-99) | absorption | The names of the named conversions | not checked | none given (raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL) | not assessed | — |
| 67 | [OPEN-100](#OPEN-100) | absorption | Comparisons on a type parameter | not checked | none given (raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL) | not assessed | — |
| 68 | [OPEN-101](#OPEN-101) | absorption | What a trap writes | not checked | none given (raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL) | not assessed | — |
| 69 | [OPEN-102](#OPEN-102) | absorption | `Float32` and `Char` as type spellings | not checked | none given (raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL) | not assessed | — |
| 70 | [OPEN-103](#OPEN-103) | absorption | `as` applied to a reference | not checked | none given (raised while absorbing MEMORY_SEMANTICS) | not assessed | — |
| 71 | [OPEN-104](#OPEN-104) | s7 review | A `#[variant]` written on a counted loop | not applicable | none given | neutral | — |
| 72 | [OPEN-105](#OPEN-105) | s7 review | Where a counted loop's `#[invariant]` is checked | not applicable | none given | (b) needs requirement 2 | — |

<a id="OPEN-33"></a>
### OPEN-33. Inline submodules under one file per module

**OPEN-33@1.** Whether an inline submodule (`mod m { ... }` written inside its parent's file) is compatible with OD9's one file per module is an open owner decision (OD33).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD33
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-4 and MOD-7. Current main: inline submodules are accepted, and most
examples of this specification use them. Raised while recording OD9 (2026-10-29). The options are in
ROADMAP's row OD33; the owner's answer is recorded there. Austral: one module per file pair, no inline modules. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): no inline `mod m { }`: one file per module. The pick rests on principle 10 and admission check 4 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: module boundaries and declaration identities kept.

<a id="OPEN-34"></a>
### OPEN-34. A struct literal inside brackets in a condition head

**OPEN-34@1.** Whether a struct literal written inside a call's argument list, an index or an array literal within an `if`, `while` or `match` head counts as parenthesized under SYN-4 is an open owner decision (OD34).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD34
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-4. Current main: `if same(p, P { v: 1 }) { ... }` is accepted (an
uppercase struct literal is read anywhere in a head). The grammar distinguishes a grouped expression
`( expr )` from the parentheses of a call's arguments, and OD16's answer does not say which one it
means. Raised while recording OD16 (2026-10-29). The options are in ROADMAP's row OD34; the owner's
answer is recorded there. Austral: not applicable (records are built with `R(f => v)`). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): only an enclosing `(` or `[` parenthesizes, stated as one invariant: in a condition head, a `{` at bracket depth zero opens the block. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral for Core; the frontend still needs a specified semantics under the source-to-native theorem (V1c, S15). Item Q5 of the owner picks of 2026-10-30.

<a id="OPEN-35"></a>
### OPEN-35. How an array satisfies a `Destroy` bound

**OPEN-35@1.** Whether arrays satisfy a `Destroy` bound through one impl written in std or through one language impl that is its own entry is an open owner decision (OD35).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD35
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRAIT-10. OD28's answer allows either form and does not choose; current main
counts an array of destroyable elements as `Destroy` with no impl (bug 324). The options and a
recommendation are in ROADMAP's row OD35; the owner's answer is recorded there. Austral: no destructor trait. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): a language impl `[T; N]: Destroy` when `T: Destroy`, elements destroyed in index order, with its own identity in coherence. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral with conditions: elements destroyed in index order, effects kept, stopping on trap or divergence; identity, bounds and capabilities survive specialization. Item Q6 of the owner picks of 2026-10-30.

<a id="OPEN-36"></a>
### OPEN-36. The explicit place form that opens a heap cell

**OPEN-36@1.** The spelling of the explicit non-consuming place form that opens a `Heap` or `HeapArray` for a field access is an open owner decision (OD36).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD36
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MEM-4. OD30's answer requires such a form and keeps `*` as the consuming
move-out, but names no spelling; current main opens one heap layer implicitly (bug 326). The options
and a recommendation are in ROADMAP's row OD36; the owner's answer is recorded there. Austral: the borrow statement. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): only through a borrow block (`borrow h as r { r.f }`), no `(&h).f`; specify the result type for `Heap<T>`, `&Heap<T>` and `&mut Heap<T>`, one heap layer per block (MEM-4), and `HeapArray` elements. The pick rests on principle 4 and admission check 2 (behaviour visible) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps, and needs the opening types specified (Heap shells and HeapArray projection are outside Borrow.lean's model). Item Q7 of the owner picks of 2026-10-30.

<a id="OPEN-37"></a>
### OPEN-37. The names of the pointer difference and ordering functions

**OPEN-37@1.** The names and signatures of the named functions that replace pointer difference and pointer ordering, beside `ptr::offset`, are an open owner decision (OD37).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD37
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-6 and TRUST-1. OD26 removes the operators and asks for named functions without naming them; TRUST-1 already names `ptr::offset`, `ptr::expose` and `ptr::from_exposed`. The options and a recommendation are in ROADMAP's row OD37;
the owner's answer is recorded there. Current main: trusted code compares raw pointers with `==` and `<` (bug 322). Austral: no named difference or ordering functions; its `Pointer` and `Address` types are compared with the ordering operators. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): one trusted `ptr::diff` (same-allocation premise), ordering read from its sign, no `ptr::cmp`; specify the representability of its result, one-past pointers included (Q8 is marked agreed in the picks; ROADMAP records no owner answer for it). The pick rests on principle 11 (honest trust boundaries) and principle 10 (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: needs the trusted pointer operations specified and preserved across memory lowering, with the same-allocation and representability premises (one-past included) in their contracts. Item Q8 of the owner picks of 2026-10-30.

<a id="OPEN-38"></a>
### OPEN-38. The root resource API

**OPEN-38@1.** The type of the root resource value `main` receives, how it is split into tokens, and the signature `main` declares to receive it are an open owner decision (OD38).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD38
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-9 and STMT-7. OD8 decides that `main` receives a root value and splits it, in the spirit of Austral's `RootCapability`, without naming its type or the splitting functions. The options and a recommendation are in ROADMAP's row OD38;
the owner's answer is recorded there. Current main: `main` takes no parameters (STMT-7). Austral: `main(root: RootCapability)`. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): `fn main(root: Root) -> u8` (or Unit), `Root` split into linear tokens by trusted functions; specify the split functions and how leftover authority is consumed. The pick rests on principle 5 (capabilities visible) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: needs requirement 1: unforgeable token construction (an empty exported struct is not constructible by users), contracts relating split outputs and residual authority to the input, one acquisition of each exclusive resource, and a startup relation that supplies the authorized Root; TRUST-4's capability containment kept.

<a id="OPEN-39"></a>
### OPEN-39. The interface artifact format

**OPEN-39@1.** The format and location of the interface artifact the compiler generates for each module are an open owner decision (OD39).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD39
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-7. OD9 decides that the compiler generates the artifact and a gate diffs it, without fixing its format. The options and a recommendation are in ROADMAP's row OD39;
the owner's answer is recorded there. Current main: the compiler has the declaration-level `FileSummary` and `--report interface`, but no persisted per-module interface artifact and no API-diff gate of the proposed form. Austral: interface files are written by hand. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): the `pub` declarations rendered as Concrete source without bodies, generated by the compiler, with an API-diff gate; it also carries private layout, impl methods, attributes, constants, capabilities and generic bodies, and is not read as FFI-6 bodyless foreign bindings. The pick rests on principle 8 (artifact-first auditability) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: needs requirement 5: the interface, the checked module, its dependencies and the linked object are bound together; private layout stays metadata and grants no construction rights; imported contracts are checked.

<a id="OPEN-40"></a>
### OPEN-40. PAT-9 and the fallback arm of a desugared form

**OPEN-40@1.** Whether PAT-9's refusal of an unreachable arm applies to the fallback arm the parser adds when it desugars `if let`, `let ... else` and `while let` is an open owner decision (OD40).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD40
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-9. OD18 is stated for a `match`; over a one-variant enum the fallback arm of `if let` can never run, and refusing it would refuse `if let` there. Bug 317's fix, unlanded, does not judge that arm. The options and a recommendation are in ROADMAP's row OD40;
the owner's answer is recorded there. Current main: `if let` over a one-variant enum is accepted. Austral: not applicable (no `if let`). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): moot if OD52 removes the desugared forms. The pick follows OD52. Proof impact: helps: no synthetic fallback arm.

<a id="OPEN-41"></a>
### OPEN-41. PAT-9 and integer ranges that span their type

**OPEN-41@1.** Whether a catch-all after integer ranges that together span the scrutinee's whole type is an unreachable arm under PAT-9 is an open owner decision (OD41).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD41
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-9 and PAT-2. PAT-2 requires a catch-all over an integer scrutinee, so refusing it there would make such a match impossible to write. Bug 317's fix, unlanded, does not count the type's range as covered. The options and a recommendation are in ROADMAP's row OD41;
the owner's answer is recorded there. Current main: a catch-all after ranges that span the type is accepted. Austral: not applicable (no range patterns). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): close as range-specific under OD56, recording in PAT-2 and PAT-9 that an integer match still needs a catch-all and that listing every literal is not coverage. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: the integer catch-all is never unreachable.

<a id="OPEN-42"></a>
### OPEN-42. Comparisons on `bool` and `char`

**OPEN-42@1.** Whether OD26's "comparisons take numbers only" removes equality and ordering on `bool` and `char` is an open owner decision (OD42).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD42
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-6. Current main compares `bool` and `char`, and TYPE-6@2 keeps that
while this is open. The options and a recommendation are in ROADMAP's row OD42; the owner's answer is
recorded there. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): keep `==` and `!=` on `bool` and `char` and ordering on `char`, refuse ordering on `bool`; amend TYPE-6 and OD26 explicitly. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral (char comparisons are not thereby admitted to the proof subset).

<a id="OPEN-43"></a>
### OPEN-43. How a type position resolves a name

**OPEN-43@1.** How a name in a type position resolves (the type namespace alone, a user type against a builtin of the same name, a child module's type declarations) is an open owner decision (OD43).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD43
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-1. Main, since bug 229: a type position searches the type namespace only, a user `enum Option` is always the builtin `Option`, and a child module's newtypes are known types in its parent. Raised while writing the rules the 2026-10 bug rulings
found missing. The options and a recommendation are in ROADMAP's row OD43; the owner's answer is
recorded there. Austral: builtin type names are reserved. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (c) refused, for builtin-name collisions only, with the complete reserved set published (also for aliases and import renames); this answer would not retire OPEN-43, whose type-versus-value lookup and child-module questions stay open in it. A user `struct Copy HeapArray` is treated as linear today. The pick rests on admission check 1 (one name, one meaning) and principle 12 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: identity-based resolution still needed.

<a id="OPEN-44"></a>
### OPEN-44. Two packages with one module name

**OPEN-44@1.** Whether two packages of one program may define the same top-level module name, refused or qualified by package, is an open owner decision (OD44).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD44
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-1 and MOD-4. Main, since bug 074: loading a project refuses the collision, naming both packages. Raised while writing the rules the 2026-10 bug rulings
found missing. The options and a recommendation are in ROADMAP's row OD44; the owner's answer is
recorded there. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refused at load, naming both packages, until packages depend on packages (R-0440). The pick rests on principle 12 (compiler-owned identity) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: neutral for Core; the frontend still needs a specified semantics under the source-to-native theorem (V1c, S15).

<a id="OPEN-45"></a>
### OPEN-45. Capability-variable inference

**OPEN-45@1.** How a call binds a capability variable (`cap C`) of the callee from its arguments, and when such a variable is refused as uninferable, is an open owner decision (OD45).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD45
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-4 and TYPE-1. Main binds the variable from the capability set of an argument's function type where its type peek can compute that type without checking the argument, and refuses one no argument determines (E0242). Raised while writing the rules the 2026-10 bug rulings
found missing. The options and a recommendation are in ROADMAP's row OD45; the owner's answer is
recorded there. Austral: not applicable (capabilities are values). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) bind from the checked type of every argument; refuse unresolved or inconsistent ones; no result-context inference. The pick rests on admission checks 1 (one rule) and 7 (failures local to an argument) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral for Core; the inferred capability substitution is validated against the source judgment, and containment survives Mono.

<a id="OPEN-46"></a>
### OPEN-46. Generic type-argument inference

**OPEN-46@1.** How a call of a generic function without explicit type arguments binds each type parameter from its arguments is an open owner decision (OD46).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD46
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-13 and TYPE-15. Main binds each type parameter from the types its type peek computes for the arguments given for it (a value `if` or `match` included since bug 308, but not a generic call's result), and refuses a parameter no argument determines. Raised while writing the rules the 2026-10 bug rulings
found missing. The options and a recommendation are in ROADMAP's row OD46; the owner's answer is
recorded there. Austral: type parameters are inferred from the arguments, and a `let` declaration's type can disambiguate a return-type-polymorphic call. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) from checked argument types only, never from the expected result; arguments that need an expected type are refused, not defaulted. The pick rests on admission checks 1 (one rule) and 7 (failures local to an argument) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral for Core; substitutions preserve bounds, Copy facts and reference restrictions (REF-4, PASS-1, PASS-2, PASS-6).

<a id="OPEN-48"></a>
### OPEN-48. The index type of a counted loop over two literals

**OPEN-48@1.** Whether a counted loop whose bounds are both untyped integer literals (`for i from 0 until 7`) is refused or gives its index a type, and which, is an open owner decision (OD48).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD48
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-6 and ARITH-2. OD47 gives the index the bounds' shared integer type, and OD31 removes the `Int` fallback for a literal with no expected type, so two literal bounds leave the index's type open: the loop could be refused, as OD31 refuses an unconstrained literal, or the index could take a type such as `Int`. The counted loop is not on main yet (bug 331). Raised while recording OD47 (2026-10-29). The options and
a recommendation are in ROADMAP's row OD48; the owner's answer is recorded there. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refused, as OD31 refuses any literal with no expected type; a typed bound supplies the other bound's type, in either order. The pick rests on admission check 1 (OD31's principle) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral: type discovery does not reorder the bounds' evaluation.

<a id="OPEN-49"></a>
### OPEN-49. The arithmetic of a counted loop's termination measure

**OPEN-49@1.** Whether the termination measure `b - i` of a counted loop is a mathematical integer, is computed in the unsigned type of the bounds' width, or makes the loop refused when it does not fit the bounds' type (a signed type with `a` near its minimum and `b` near its maximum) is an open owner decision (OD49).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD49
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-6, ARITH-3 and the loop variant of the contracts chapter. OD47 says the counted form carries the measure `b - i` and desugars to `while`; ARITH-3 checks ordinary arithmetic, and silently choosing wrapping, a wider type or a mathematical integer would add a rule. Raised while recording OD47 (2026-10-29). The options and
a recommendation are in ROADMAP's row OD49; the owner's answer is recorded there. Current main: the counted loop is not on main (bug 331). Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) a mathematical integer in the proof only, never computed at run time. The pick rests on principle 9 (proofs attach to real code) and admission check 9 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: needs requirement 2: the measure proves only that the iteration count is finite, so termination also needs the bounds, the body and cleanup to terminate; the counted-loop proof rule (zero iterations, `break`, labelled `continue`) bound to the exact desugared loop; an invariant checked at body entry establishes nothing after an empty loop; bounds-check removal needs an in-range certificate, not only an immutable length.

<a id="OPEN-50"></a>
### OPEN-50. Reserved or contextual `from` and `until`

**OPEN-50@1.** Whether `from` and `until` are reserved words or keywords only in a counted loop's head is an open owner decision (OD50).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD50
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-6 and SYN-1. OD47 named them reserved; the reviews of 2026-10-29 raised the contextual alternative, under which `let from: Int` stays legal. Main has neither word (bug 331). The options and a recommendation are in ROADMAP's row
OD50; the owner's answer is recorded there. Austral: its loop words are reserved. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) reserved everywhere. The pick rests on admission check 1 (one lexical rule) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral.

<a id="OPEN-51"></a>
### OPEN-51. Reverse, stepped and collection iteration

**OPEN-51@1.** Whether reverse, stepped or collection iteration stays out of the language, the counted loop covering only an ascending range by one, is an open owner decision (OD51).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD51
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-6. OD47 adds the ascending half-open loop only; the reviews of 2026-10-29 asked whether counting down, a step other than 1 and iterating a collection stay with `while`. The options and a recommendation are in ROADMAP's row
OD51; the owner's answer is recorded there. Current main: main has no counted loop (bug 331). Austral: a counted loop and `while`. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) none now; each a later decision when a workload needs it. The pick rests on principle 10 and admission check 9 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral.

<a id="OPEN-52"></a>
### OPEN-52. `if let`, `while let` and `let ... else`

**OPEN-52@1.** Whether `if let`, `while let` and `let ... else` stay in the language or are removed in favour of `match` is an open owner decision (OD52).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD52
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-6, PAT-8, PAT-9, OPEN-40, SYN-1. Current main: all three are accepted and desugar to a `match` with a catch-all arm (PAT-6). Options: (a) keep the three forms (PAT-6, their desugaring to `match`); (b) remove them, `match` being the only refutable pattern form. Austral: none of the three; `case` is the only refutable form. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove. The pick rests on principle 10 (small analyzable core) and admission check 1 (a simple invariant) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: fewer desugarings; match ownership and branch accounting kept. Item S1 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD52.

<a id="OPEN-53"></a>
### OPEN-53. Struct update `..base`

**OPEN-53@1.** Whether the struct update form `S { a: 1, ..base }` stays in the language is an open owner decision (OD53).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD53
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-7, OWN-15. Current main: accepted (TYPE-7, OWN-15's value flow for `..base`). Options: (a) keep it; (b) remove it, every field written. Austral: no struct update; every field is written. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove. The pick rests on principle 10 and admission check 2 (behaviour visible: every field written) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: removes the explicit-fields-then-base evaluation case. Item S2 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD53.

<a id="OPEN-54"></a>
### OPEN-54. `newtype` and `.0`

**OPEN-54@1.** Whether `newtype` declarations and the `.0` projection stay, or a one-field struct with a private field replaces them is an open owner decision (OD54).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD54
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-7, TYPE-11, ARITH-5. Current main: `newtype` declarations and `.0` are accepted; `unwrap(id)` is an intrinsic. Options: (a) keep `newtype` and `.0`; (b) remove both, using a one-field struct whose field is private to its module. Austral: no newtype; a type is made opaque by the module interface hiding its definition (a record being one possible definition). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove. The pick rests on principle 10 and admission check 1 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps, with migration care: the one-field struct's Copy declaration and private construction keep the invariant (TYPE-11's private-destructuring gap). Item S3 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD54.

<a id="OPEN-55"></a>
### OPEN-55. Match guards

**OPEN-55@1.** Whether match guards (`pattern if cond =>`) stay in the language is an open owner decision (OD55).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD55
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-4, PAT-2, PAT-9. Current main: guards are accepted and read-only (PAT-4). Options: (a) keep them (PAT-4); (b) remove them, the condition written inside the arm. Austral: no guards. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove. The pick rests on principle 10 and admission check 9 (the proof story: coverage without guards) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps, and needs requirement 4: it narrows PROVABLE_V1's OD19 admission of guards and ranges, which is amended explicitly with a compatibility record and any affected evidence migrated. Item S10 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD55.

<a id="OPEN-56"></a>
### OPEN-56. Range patterns

**OPEN-56@1.** Whether integer range patterns (`1..=9 =>`) stay in the language (this amends OD19) is an open owner decision (OD56).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD56
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-7, PAT-2, PAT-9, OPEN-41, OD19. Current main: range patterns are accepted (PAT-7). Options: (a) keep them (PAT-7); (b) remove them, the comparison written in the arm or an `if` chain. Austral: no range patterns. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove; then close OPEN-41 as range-specific, recording in PAT-2 and PAT-9 that an integer match still needs a catch-all and that listing every literal is not coverage, so that catch-all is never unreachable. The pick rests on principle 10 and admission check 1 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps, and needs requirement 4: it narrows PROVABLE_V1's OD19 admission of guards and ranges, which is amended explicitly with a compatibility record and any affected evidence migrated. Item S11 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD56.

<a id="OPEN-57"></a>
### OPEN-57. The region name of a borrow block

**OPEN-57@1.** Whether a borrow block keeps its region name (`borrow x as r in R { }`) or drops `in R` is an open owner decision (OD57).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD57
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-8, REF-11, SCOPE-D1. Current main: the region name is written and has no other use in the language. Options: (a) keep `in R`; (b) drop it, the block itself being the region. Austral: the borrow statement names its region (`borrow x as r in R do ... end`); anonymous borrows `&x` in call arguments name none. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) drop `in R`. The pick rests on principle 10 and admission check 1 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps if the internal loan and scope identities stay. Item S12b of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD57.

<a id="OPEN-58"></a>
### OPEN-58. Borrow blocks stay

**OPEN-58@1.** Whether borrow blocks (`borrow x as r in R { }`, as written today) stay the way to hold a reference across statements is an open owner decision (OD58).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD58
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-6, REF-8, REF-11, OPEN-36. Current main: borrow blocks are accepted (REF-8, REF-11). Options: (a) keep them; (b) remove them, references living only within one call's arguments. Austral: the borrow statement is the way to hold a reference across statements. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) keep (recorded as already agreed in the picks; no owner record yet). The pick rests on principle 4 (references are scoped access, not ordinary values) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: helps: lexical loan boundaries kept. Item S12 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD58.

<a id="OPEN-59"></a>
### OPEN-59. One way to consume a `Heap`

**OPEN-59@1.** Which operations consume a `Heap<T>`: the move-out `*h` (OD30), `destroy(h)`, `free(h)` or a new `take(h)` is an open owner decision (OD59).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD59
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MEM-5, OWN-13, OPEN-36. Current main: `*h` moves out and frees and `free(h)` frees the cell and returns its value (MEM-5); `destroy(h)` on a Heap is refused (E0272: destroy() requires a named type). Options: (a) keep `*h` and `free(h)` as today; (b) `*h` as move-out-and-free and `destroy(h)` extended to a Heap, `free` deleted, no `take`; (c) `take(h)` replaces `*h` as the move-out, `destroy(h)` extended to a Heap, `free` deleted. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b): `*h` and a `destroy(h)` extended to a Heap, no `free`, no `take` (a `take` would be another spelling of `*h`). The pick rests on principle 10 and admission check 1 (one spelling per operation) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: `*h` (move out and free) and `destroy(h)` stay two distinct effects. Item S7 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD59.

<a id="OPEN-60"></a>
### OPEN-60. One consuming receiver form for `Destroy`

**OPEN-60@1.** Whether `Destroy` is implemented through one consuming receiver form only is an open owner decision (OD60).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD60
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on OWN-13, TRAIT-9, TRAIT-10. Current main: a `Destroy` impl may take `self` or `&self`; `&mut self` is refused (E0587). Options: (a) keep the receiver forms main accepts; (b) one consuming form (`fn destroy(self)`) only. Austral: no destructor trait; a linear type's module exports an ordinary consuming function. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) one consuming form. The pick rests on principle 10 and admission check 1 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: helps: the destructor body still accounts for every field. Item S8 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD60.

<a id="OPEN-61"></a>
### OPEN-61. One coordinated removal list

**OPEN-61@1.** Whether the decided and proposed removals (inline `mod`, the C-style `for`, `if let` / `while let` / `let ... else`, `newtype`, `..base`, range patterns, guards, `in R`, `break <expr>` and the `trusted mod` marker) are migrated as one coordinated list is an open owner decision (OD61).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD61
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-6, STMT-6, PAT-6, PAT-7, PAT-4, TYPE-7, MOD-4, SYN-1. Current main: TRUST-6 (pending) still requires the `trusted mod` marker, which S14's pick would drop (no owner answer is recorded for S14). Options: (a) one list, migrated in exit 2q's single transition; (b) each removal on its own schedule. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) one list, including dropping TRUST-6's `trusted mod` marker, which S14 removes (S14 is marked agreed in the picks; ROADMAP records no owner answer for it). The pick rests on principle 10 (one migration, one transition) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: neutral: affected rules and fixtures are retired or versioned, never silently deleted. Item review new 7 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD61.

<a id="OPEN-62"></a>
### OPEN-62. Unused values and variables

**OPEN-62@1.** Whether an unused local variable or parameter is refused (Go's rule) and whether an unused `Result` or `Option` may be dropped by `discard` is an open owner decision (OD62).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD62
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on OWN-5, OWN-6, OWN-7, REF-10. Current main: unused Copy locals and parameters are accepted; OWN-6 refuses an unused fallible result. Options: (a) refuse only an unused `Result` or `Option` (OWN-6); (b) Go's rule: any unused local or parameter is refused (an intentionally unused parameter is named `_`), and `discard` refuses `Result` and `Option`. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b), stated as one invariant: every name a body binds is used or is `_` (a bodyless declaration has no body, so needs no exception); `discard` and wildcard drops refuse `Result` and `Option`, Copy or not; to clarify: `_` as a loop index, and an unused `&mut` binding under REF-10. The pick rests on admission check 1 (one invariant) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: needs requirement 3: `_` waives only the unused-name check, never ownership: `fn sink(_: Heap<T>) {}` stays refused (OWN-2). Item Q3 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD62.

<a id="OPEN-63"></a>
### OPEN-63. Float literals take their expected type

**OPEN-63@1.** Whether a float literal takes the float type its position expects and is refused where nothing is expected, as ARITH-2@2 states under OD31's note that float literals get an explicit rule under the same principle is an open owner decision (OD63).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD63
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on ARITH-2, ARITH-7. Current main: `let x = 1.5;` is accepted as `Float64` (ARITH-2@2 is partial, bug 327). Options: (a) yes, as ARITH-2@2 states for integers and floats alike; (b) a float literal defaults to `f64`. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a). The pick rests on admission check 1 (OD31's principle, one literal rule) (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: needs a specified semantics: the context chooses the float format and so the literal's value (ARITH-7). Item Q4 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD63.

<a id="OPEN-64"></a>
### OPEN-64. Raw pointers derived from a borrow

**OPEN-64@1.** What a raw pointer derived from a borrow may do: whether it is valid only while the borrow lives, may escape the borrow block, may be passed to C, and how extern parameters say so is an open owner decision (OD64).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD64
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-14, TRUST-1, TRUST-3, FFI-3, OWN-12. Current main: no pass assumes `&mut` exclusivity (OD15); REF-14 bounds aliases by their loan. Options: (a) valid only while the borrow lives and never escaping it; (b) the operational loan rules of REF-14 only. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a): no returning, no escaping storage (a local `let` inside the block stays allowed), no converting to an integer and no capturing in an outer `defer`; borrow-derived pointers go only to call-only `extern` parameters; C uses it only during a call (memory C keeps must come from an allocation with its own owner); `extern` parameters say call-only (the default) or kept; field pointers come from the same borrow of the whole owner; deferred calls in the block run before the borrow ends; provenance survives copies, offsets, field pointers, helpers and indirect calls; a shared borrow never yields write access; `ptr::expose` is refused on such a pointer; the annotation's syntax gets an LL(1) check. The pick rests on principle 11 (honest trust boundaries) and principle 4 (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: needs: the escape and provenance restrictions are premises of checker soundness; call-only foreign use is a recorded contract; raw accesses respect the current loan's permissions, not only its lifetime. Item G19 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD64.

<a id="OPEN-65"></a>
### OPEN-65. Empty structs

**OPEN-65@1.** Whether a struct with no fields (`struct S {}`) is allowed, as a unit-like type for capability and root tokens, or refused is an open owner decision (OD65).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD65
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-7, TYPE-16, TRUST-9, OPEN-38. Current main: the parser accepts `struct S {}` and `S {}`; the grammar requires at least one field. Options: (a) allowed; (b) refused. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Proof impact: needs requirement 6: a full representation contract (size, alignment, arrays, heap, ABI), with distinct logical ownership even at size 0. Item review new 1 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD65.

<a id="OPEN-66"></a>
### OPEN-66. The unit literal `()`

**OPEN-66@1.** Whether `()` is the unit value's literal, given that the grammar has no production for it is an open owner decision (OD66).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD66
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, TYPE-4. Current main: the grammar has no production for `()`. Options: (a) add `()` to the grammar; (b) no unit literal, Unit produced only by a statement or a Unit call. Austral: the unit value is written `nil`. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Proof impact: neutral: syntax for the existing Unit value. Item review new 2 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD66.

<a id="OPEN-67"></a>
### OPEN-67. Locals that reuse a top-level name

**OPEN-67@1.** Whether a local binding may reuse the name of a top-level function, an import alias or an intrinsic (MEMORY_SEMANTICS §15.4 also lists constants) is an open owner decision (OD67).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD67
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SCOPE-1, SCOPE-2, MOD-1. Current main: allowed. Options: (a) allowed, as today; (b) refused, as SCOPE-1 refuses shadowing a local. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Proof impact: depends on the option: (b) would remove resolution cases; (a) keeps them. Item review new 4 (SCOPE-4) of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD67.

<a id="OPEN-68"></a>
### OPEN-68. Failure inside a deferred call

**OPEN-68@1.** What OWN-12's text says about a failure inside a deferred call, given that an abort runs no deferred call (STMT-1, STMT-2) is an open owner decision (OD68).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD68
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on OWN-12, STMT-1, STMT-2, STMT-11. Current main: failure is abort only (STMT-2); OWN-12's rationale leaves the case open. Options: (a) OWN-12 states that a trap inside a deferred call ends the program and runs no further deferred call, as STMT-2 and OD10 say for every abort; (b) the remaining deferred calls still run after a trapping one, which would amend STMT-2 and OD10. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Proof impact: depends on the option: (a) matches the proved cleanup model (a trapping deferred call stops later cleanup); (b) would need cleanup after a trap specified and proved. Item review new 5 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD68.

<a id="OPEN-69"></a>
### OPEN-69. What "the remaining locals are destroyed" means

**OPEN-69@1.** Whether STMT-11's last exit step ("the remaining locals are destroyed") means storage teardown only, with no synthesized `Destroy` call, as OWN-13 requires is an open owner decision (OD69).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD69
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-11, OWN-13. Current main: nothing is destroyed implicitly (OWN-13). Options: (a) storage teardown only, no call, as OWN-13 requires; (b) a remaining local whose type implements `Destroy` is destroyed by a synthesized call, which would amend OWN-13. Austral: no implicit destruction. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Proof impact: depends on the option: (a) keeps teardown free of synthesized Destroy calls, as the proved linear model assumes; (b) would add them to the model. Item review new 6 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD69.

<a id="OPEN-70"></a>
### OPEN-70. Struct fields with no comma

**OPEN-70@1.** Whether struct fields written without a separating comma (`struct S { a: i32 b: i32 }`) are refused is an open owner decision (OD70).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD70
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1. Current main: the parser accepts it. Options: (a) refused, the grammar wins; (b) the grammar adds the form. Austral: separators are required. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse. The pick rests on admission check 1 (the grammar and the parser state one language) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item P2 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD70.

<a id="OPEN-71"></a>
### OPEN-71. Enum variants with no comma

**OPEN-71@1.** Whether enum variants written without a separating comma are refused is an open owner decision (OD71).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD71
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1. Current main: the parser accepts it. Options: (a) refused; (b) the grammar adds the form. Austral: separators are required. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item P3 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD71.

<a id="OPEN-72"></a>
### OPEN-72. An enum with no variants

**OPEN-72@1.** Whether an empty enum `enum E { }` is refused or is the uninhabited type is an open owner decision (OD72).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD72
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, PAT-2. Current main: the parser accepts it. Options: (a) refused; (b) the grammar adds it as the uninhabited type. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse until a workload needs an uninhabited type. The pick rests on principle 10 (until a workload needs it) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: neutral: no uninhabited-type case. Item P4 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD72.

<a id="OPEN-73"></a>
### OPEN-73. Reserved words used as identifiers

**OPEN-73@1.** Whether `cap` and `type`, which the lexer reserves, are refused as identifiers where the parser lets them through is an open owner decision (OD73).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD73
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1. Current main: `expectIdent` accepts `cap` (and `type`) as a variant name. Options: (a) refused, reserved words stay reserved; (b) the grammar lets them through there. Austral: keywords are reserved. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse. The pick rests on admission check 1 (one lexical rule) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item P5 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD73.

<a id="OPEN-74"></a>
### OPEN-74. A raw pointer type with no qualifier

**OPEN-74@1.** Whether a bare `*T` is refused or read as `*mut T` is an open owner decision (OD74).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD74
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, TRUST-1. Current main: the parser reads `*T` as `*mut T`. Options: (a) refused, `*const` or `*mut` required; (b) the grammar adds `*T` as `*mut T`. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse; the qualifier is the safety-relevant part. The pick rests on principle 5 (trust boundaries visible: the qualifier is the safety-relevant part) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: neutral. Item P6 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD74.

<a id="OPEN-75"></a>
### OPEN-75. An empty type parameter list

**OPEN-75@1.** Whether `impl<> S` (an empty type parameter list) is refused is an open owner decision (OD75).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD75
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1. Current main: the parser accepts it. Options: (a) refused; (b) the grammar adds it. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item P7 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD75.

<a id="OPEN-76"></a>
### OPEN-76. Unqualified struct patterns in `match`

**OPEN-76@1.** Whether an unqualified struct pattern is allowed in `match` (`match s { S { a } => ... }`), given that only `let` destructures structs today (the `if let` form follows OD52) is an open owner decision (OD76).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD76
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-1, PAT-5, SYN-1. Current main: the parser refuses it in `match` and `if let` and accepts it in `let`. Options: (a) refused in `match`, PAT-1's wording fixed to say so; (b) allowed. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse in `match`; fix PAT-1's wording. The pick rests on principle 10 and admission check 1 (PROJECT.md, [principles](../project/PROJECT.md#principles) and [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item G1-b of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD76.

<a id="OPEN-77"></a>
### OPEN-77. Resync the grammar to the decided language

**OPEN-77@1.** Whether `grammar/concrete.ebnf` is resynchronized for the items the language already decided: `E#V` becomes `E::V`, binding renaming, a function body with no trailing value and `;` for bodyless functions, no generic trait methods, assignment targets are places, unknown attribute names refused, and no `break <expr>` is an open owner decision (OD77).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD77
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, PAT-1, STMT-6, STMT-8, OWN-8, FFI-6. Current main: the grammar is stale on these items (grammar/conformance_known.tsv). Options: (a) approve the resync, as actual productions with a factored `place` prefix for assignment; (b) defer it, the divergences staying recorded in grammar/conformance_known.tsv (the decided language does not change either way). Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) approve, with actual productions, a `place` prefix and `break <expr>` removed. The pick rests on admission check 1 (the grammar and the parser state one language) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: needs: place assignment and the function-body forms correspond to STMT-4, STMT-8 and STMT-9; neutral for Core; the frontend still needs a specified semantics under the source-to-native theorem (V1c, S15). Item B of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD77.

<a id="OPEN-78"></a>
### OPEN-78. `mod X;` in the grammar

**OPEN-78@1.** Whether the grammar gains `mod name;` (a module in its own file), which the parser accepts and OD33's pick would make the only module form is an open owner decision (OD78).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD78
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-4, OPEN-33, SYN-1. Current main: the parser accepts it; the grammar lacks it. Options: (a) add it to the grammar; (b) keep the recorded divergence for now. Austral: each module is its own pair of files. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) add, item by item. The pick rests on admission check 4 (cross-file dependencies declaration-level only) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral for Core (loading stays frontend correspondence). Item C1 (review new 3) of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD78.

<a id="OPEN-79"></a>
### OPEN-79. `assert` in the grammar

**OPEN-79@1.** Whether the grammar gains the proof-annotation statement `assert expr;` is an open owner decision (OD79).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD79
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-2, CON-3, SYN-1. Current main: the parser accepts it. Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Proof impact: needs the production reviewed on its own (no blanket neutrality). Item C2 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD79.

<a id="OPEN-80"></a>
### OPEN-80. `assume` in the grammar

**OPEN-80@1.** Whether the grammar gains the proof-annotation statement `assume expr;` is an open owner decision (OD80).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD80
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-2, CON-3, SYN-1. Current main: the parser accepts it. Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Proof impact: needs the production reviewed on its own (no blanket neutrality). Item C3 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD80.

<a id="OPEN-81"></a>
### OPEN-81. `ghost let` in the grammar

**OPEN-81@1.** Whether the grammar gains the proof-only binding `ghost let` (with `ghost` a contextual keyword) is an open owner decision (OD81).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD81
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-4, SYN-1. Current main: the parser accepts it. Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Proof impact: needs the production reviewed on its own (no blanket neutrality). Item C4 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD81.

<a id="OPEN-82"></a>
### OPEN-82. Loop contract attributes and where attributes attach

**OPEN-82@1.** Whether the grammar gains `#[invariant(e)]` and `#[variant(e)]` before a loop, and whether attributes attach by kind (declaration attributes to the next declaration, loop attributes to the next loop) is an open owner decision (OD82).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD82
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-2, STMT-12, SYN-1. Current main: the parser accepts them. Options: (a) add them to the grammar and attach attributes by kind; (b) remove them from the parser; (c) keep the recorded divergence for now. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a): attach by kind, declaration attributes to the next declaration, `#[invariant]` and `#[variant]` to the next loop. The pick rests on admission check 1 (attributes attach by kind) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: needs review: where an attribute attaches decides which proposition is proved. Item C5 (R7) of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD82.

<a id="OPEN-83"></a>
### OPEN-83. The call-site allocator binding in the grammar

**OPEN-83@1.** Whether the grammar gains the call-site allocator binding `f(...) with(Alloc = e)` is an open owner decision (OD83).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD83
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-4, MEM-5, SYN-1. Current main: the parser accepts it. Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Austral: not checked. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Proof impact: needs the production reviewed on its own (no blanket neutrality). Item C6 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD83.

<a id="OPEN-84"></a>
### OPEN-84. Struct destructuring `let` in the grammar

**OPEN-84@1.** Whether the grammar gains struct destructuring `let S { a } = e;` is an open owner decision (OD84).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD84
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-5, SYN-1. Current main: the parser accepts it (PAT-5). Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Austral: destructuring `let` of records exists (not checked). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Proof impact: needs the production reviewed on its own (no blanket neutrality). Item C7 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD84.

<a id="OPEN-85"></a>
### OPEN-85. Variant destructuring with `else` in the grammar

**OPEN-85@1.** Whether the grammar gains `let E::V { a } = e else { ... };`, which OD52's pick would remove is an open owner decision (OD85).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD85
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-8, OPEN-52, SYN-1. Current main: the parser accepts it (PAT-8). Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Austral: no `let ... else`. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): follows OD52 (removed under its pick). The pick follows OD52. Proof impact: follows OD52. Item C8 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD85.

<a id="OPEN-86"></a>
### OPEN-86. The body of an unbraced match arm

**OPEN-86@1.** Whether the grammar narrows an unbraced match-arm body to an expression, a block or `return`, which is what the parser accepts (`continue;` as an unbraced body is refused) is an open owner decision (OD86).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD86
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, PAT-3. Current main: the parser accepts an expression, a block or `return` as an unbraced arm body and refuses other statements. Options: (a) narrow the grammar; (b) widen the parser. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) narrow. The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item R1 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD86.

<a id="OPEN-87"></a>
### OPEN-87. Capability arguments in an expression turbofish

**OPEN-87@1.** Whether capability arguments (`T + C`, `{}`) may be written in an expression or method turbofish is an open owner decision (OD87).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD87
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, TRUST-4, OPEN-45. Current main: the parser refuses them; the grammar derives them. Options: (a) the grammar narrows to type arguments there until a workload needs them; (b) the parser adds them. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) narrow. The pick rests on principle 10 (until a workload needs it) (PROJECT.md, [principles](../project/PROJECT.md#principles)). Proof impact: neutral. Item R2/R3 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD87.

<a id="OPEN-88"></a>
### OPEN-88. The shape of capability arguments

**OPEN-88@1.** Whether the grammar states the capability-argument shape rules the parser checks (a union of names only; type arguments before capability arguments) is an open owner decision (OD88).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD88
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, TRUST-4. Current main: the parser checks them; the grammar does not. Options: (a) state them in the grammar; (b) leave them to the parser. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a). The pick rests on admission check 1 (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral. Item R8 of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD88.

<a id="OPEN-89"></a>
### OPEN-89. Capability parameters on a trait impl

**OPEN-89@1.** Whether a trait impl may declare capability parameters, which the grammar derives and the parser refuses is an open owner decision (OD89).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD89
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, TRAIT-9, TRUST-4. Current main: the parser refuses them (declare them on an inherent impl). Options: (a) refused, the grammar narrowed; (b) allowed. Austral: not applicable. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (in the divergence report, not the picks). Proof impact: neutral. Item R9 of the parser/grammar divergence report of 2026-10; the owner's answer is recorded in ROADMAP's row OD89.

<a id="OPEN-90"></a>
### OPEN-90. `struct Copy <T>` and contextual keywords

**OPEN-90@1.** Whether `Copy` stays a contextual keyword, with the LL(1) checkers taught to model it, or becomes a reserved word (the other unreserved words, `self`, `repr`, `packed` and `align`, stay as they are unless decided separately) is an open owner decision (OD90).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD90
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, OWN-D1. Current main: contextual; the `struct Copy <T>` conflict is invisible to the checkers' token model. Options: (a) reserve `Copy` as a keyword, refusing it as an identifier; (b) keep it contextual, the LL(1) checkers taught to model a quoted unreserved terminal as also IDENT, each conflict found listed for the owner (letters as in the divergence report). Austral: its keywords are reserved. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) reserve `Copy`: one lexical rule, consistent with OD50 (reserved `from` and `until`) and OD43 (builtin names refused as user names). The pick rests on admission checks 1 (a simple invariant) and 3 (no phase coupling) (PROJECT.md, [admission checks](../project/PROJECT.md#design-policy)). Proof impact: neutral: conditional Copy semantics unchanged. Item Copy of the owner picks of 2026-10-30; the owner's answer is recorded in ROADMAP's row OD90.

<a id="OPEN-91"></a>
### OPEN-91. A cast followed by `<`

**OPEN-91@1.** How `x as T < y` parses, given that `<` after a cast's type name may start type arguments or be the less-than operator is an open owner decision (OD91).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD91
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1, SYN-2, ARITH-5. Current main: the parser always reads type arguments, so `x as T < y` is refused (grammar/ll1_known_conflicts.tsv, row type|5.2). Options: (a) a cast's type takes no type arguments unless parenthesized (`x as (T<U>)`), so `<` is less-than; (b) the parser's reading: `<` always starts type arguments, so the comparison needs parentheses (`(x as T) < y`); (c) refuse `<` directly after a cast's type and require parentheses either way. Austral: a conversion is the cast expression `e : T`, which takes no type arguments after it, so the conflict does not arise. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (in the LL(1) ledger, not the picks). Proof impact: neutral for Core; the frontend still needs a specified semantics under the source-to-native theorem (V1c, S15). Item LL(1) type|5.2 of grammar/ll1_known_conflicts.tsv; the owner's answer is recorded in ROADMAP's row OD91.

<a id="OPEN-104"></a>
### OPEN-104. A `#[variant]` written on a counted loop

**OPEN-104@1.** Whether a `#[variant]` attribute written on a counted loop is refused, checked like any loop variant, or ignored is an open owner decision (OD104).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD104
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-12 and CON-2. Current main: the counted loop is not on main (bug 331). Options: (a) refused, the loop's own measure `b - i` being its only variant; (b) allowed and checked as a loop variant (CON-2); (c) allowed and ignored. Austral: not applicable (no loop contracts). No pick given. Proof impact: neutral. Raised by the Codex and Kimi review of the OD47 loop rules (2026-10-30), which found the refusal stated as a rule though OD47 did not decide it; the owner's answer is recorded in ROADMAP's row OD104.

<a id="OPEN-105"></a>
### OPEN-105. Where a counted loop's `#[invariant]` is checked

**OPEN-105@1.** Where an `#[invariant]` attribute on a counted loop is checked, given that CON-2 checks a loop invariant before the loop and after every iteration is an open owner decision (OD105).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD105
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-12 and CON-2. Current main: the counted loop is not on main (bug 331). Options: (a) as CON-2 says for every loop, before the loop and after every iteration; (b) at each entry to the body, so nothing is checked when the loop runs no iteration. Austral: not applicable. No pick given. Proof impact: (b) needs requirement 2 of the proof-impact review: an invariant checked at body entry establishes nothing after an empty loop. Raised by the same review, which found body-entry checking stated as a rule though OD47 did not decide it; the owner's answer is recorded in ROADMAP's row OD105.

<a id="OPEN-92"></a>
### OPEN-92. A capability set inferred from callbacks

**OPEN-92@1.** Whether a combinator may write its capability set as that of its callbacks (`with(f)`), the checker computing the union of the sets of the callbacks its body calls, is an open owner decision (OD92).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD92
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-D1, TYPE-17, TRUST-4. Current main: no such form; `with(f)` reads `f` as a capability name (OPEN-94), so a combinator names a capability parameter (`fn map<T, U, cap C>(.., f: fn(&T) with(C) -> U) with(C)`). The callable-values design (2026-06-12, [CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md) §6) recorded the shortcut as specified and not built, after [research/language/capability-polymorphism.md](../../research/language/capability-polymorphism.md); no roadmap item tracks it. Options: (a) no shortcut, combinators name capability parameters; (b) add it, as set propagation and not row unification. Austral: not applicable (capabilities are values). Pick: none given (raised by the absorption of 2026-10-30).

<a id="OPEN-93"></a>
### OPEN-93. Widening a function value's capability set

**OPEN-93@1.** Whether a function value converts to a function type whose capability set contains its own, implicitly by subset or by an explicit conversion, is an open owner decision (OD93).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD93
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-4, TYPE-17, TRUST-4. Current main: refused (E0220): `fn(Int) -> Int` is not a `fn(Int) with(Console) -> Int`, nor `with(File)` a `with(Std)`, and there is no explicit conversion; a capability-polymorphic parameter accepts either by instantiating its variable. The callable-values design (§10) says there is no implicit widening, and that "widening is explicit or via lattice subsumption only", with a lattice example (`Concurrent ⊇ Async`) whose names are not capabilities; HANDLE_CAPABILITIES R7 keeps handle types invariant, as TYPE-4 does. Options: (a) exact types, as main; (b) subsumption into a larger set; (c) an explicit conversion. Austral: not applicable (capabilities are values). Pick: none given (raised by the absorption of 2026-10-30).

<a id="OPEN-94"></a>
### OPEN-94. Names in `with(...)` that are not capabilities

**OPEN-94@1.** Whether a name in `with(...)` that is not a capability, a capability alias or a capability variable in scope is refused is an open owner decision (OD94).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD94
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-D1, TYPE-D1, TRUST-4. Current main: accepted as a capability of its own: `fn main() with(Foo) -> Int` compiles and runs, and a call to a function that requires `Foo` is refused only in a caller that does not list it (E0520). The compiler's `validCaps`, TRUST-4's rationale and [SAFETY.md](../verification/SAFETY.md) name nine capabilities. Options: (a) refuse an unknown name; (b) an open vocabulary. Austral: not applicable (capabilities are values). Pick: none given (raised by the absorption of 2026-10-30).

<a id="OPEN-95"></a>
### OPEN-95. What `Time`, `Env`, `Process` and `Alloc` mean

**OPEN-95@1.** What authority each of `Time`, `Env`, `Process` and `Alloc` names is an open owner decision (OD95).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD95
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-D1, TRUST-4. Current main: no document states them; HANDLE_CAPABILITIES §2 defined `Console`, `File` and `Network`, and SAFETY.md `Random` (TRUST-D1). std requires `Time` for its clock and sleep functions, `Env` for environment variables and for `std.net`'s `bind` and `connect`, `Process` for exit, spawn, wait and kill, and `Alloc` for heap allocation. Options: state each as std uses it, or narrow or split it (std's `Env` on socket functions is the case to settle). Austral: not applicable (capabilities are values). Pick: none given (raised by the absorption of 2026-10-30).

<a id="OPEN-96"></a>
### OPEN-96. Two foreign bindings of one C symbol

**OPEN-96@1.** Whether two foreign bindings of one C symbol (`#[link_name]`) must agree on its C signature is an open owner decision (OD96).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD96
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on FFI-3, FFI-5. Current main: accepted: `abs_a(x: i32) with() -> i32` and `abs_b(x: i64) with() -> i64`, both bound to `abs`, compile and the binary runs, while the interpreter stops at run time on the mismatched width. HANDLE_CAPABILITIES §5 (encoding A, one binding per effect for one C symbol) planned to refuse a mismatch with a diagnostic. Options: (a) refuse; (b) allow, each binding's signature trusted on its own. Austral: not applicable. Pick: none given (raised by the absorption of 2026-10-30).

<a id="OPEN-97"></a>
### OPEN-97. A build profile that omits or demands an overflow check

**OPEN-97@1.** Whether a build profile may omit the run-time check of an integer operation that no proof discharges, or refuse such an operation at compile time, is an open owner decision (OD97).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD97
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on ARITH-3, ARITH-9. Current main: every profile emits every check, and no option turns one off; ARITH-3's rationale says a profile changes reports, never code generation, and OD10 decided that arithmetic is checked "by default". The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §3.1 instead let a release profile omit a check that is "explicitly configured off and reported", and a provable or high-integrity profile refuse an unproved overflow site. Options: (a) every profile keeps every check that no discharged proof removes; (b) a release profile may turn checks off when configured and reported, never wrapping; (c) as (a), and a profile may also refuse an operation whose no-overflow obligation is not discharged. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30).

<a id="OPEN-98"></a>
### OPEN-98. Pointer and reference conversions written with `as`

**OPEN-98@1.** Which conversions between raw pointers and references `as` writes (`&x as *const T`, `p as &T`, `p as &mut T`, `p as *mut U`) is an open owner decision (OD98).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD98
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-1, TRUST-3, REF-5, ARITH-5. Current main: `&x as *const T` is accepted in safe code, `p as &T` from a raw pointer is accepted in trusted or `Unsafe` code (the binary and `--interp` agree), and a reference cast between different pointee types (`a as &Int` with `a: &A`) is refused (E0553). No entry states these: TRUST-3 forms a reference from a raw pointer with `&*p`, and REF-5 lists borrowing and projection as the ways a reference is formed. The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §8 kept `as` "the spelling of pointer and reference casts (`as *const T`, `as *mut T`, `as &K`, `as &mut V`)", 281 lines of std. Options: (a) `as` keeps every one of them; (b) `as` converts only a reference to a raw pointer, and a reference is formed from a raw pointer only by `&*p` (TRUST-3); (c) pointer conversions become named functions. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30).

<a id="OPEN-99"></a>
### OPEN-99. The names of the named conversions

**OPEN-99@1.** The spellings of the named conversions of ARITH-5 and ARITH-10 (`try_convert`, `truncate`, `reinterpret`, and the float roundings `trunc`, `round`, `floor`, `ceil`) are an open owner decision (OD99).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD99
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on ARITH-5, ARITH-10. Current main: none of them exists (E0101). ARITH-5's sentence names `truncate` and `reinterpret`, while the archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §8.2 calls its spellings indicative, to be fixed by the API's owner. Options: (a) the spellings above, as builtins; (b) other names, or std functions rather than builtins. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30).

<a id="OPEN-100"></a>
### OPEN-100. Comparisons on a type parameter

**OPEN-100@1.** Whether a comparison on a value of a `Copy`-bounded type parameter is accepted and checked again at each instantiation, or refused in the generic body, is an open owner decision (OD100).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD100
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-6, TYPE-9, PASS-1, OPEN-42. Current main: `a < b` with `a, b: T` and `T: Copy` is accepted, and an instantiation at a type with no comparison is refused after monomorphization (E0229, `post-mono`); without the `Copy` bound it is refused in the body (E0229, `check`). TYPE-9's rationale says a generic body is checked once, from its bounds, which a per-instantiation check departs from. Options: (a) keep main's per-instantiation check; (b) refuse a comparison on a type parameter until a bound states it (a comparison trait); (c) refuse it. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md §8a (2026-10-30).

<a id="OPEN-101"></a>
### OPEN-101. What a trap writes

**OPEN-101@1.** Whether a run-time trap writes a message naming the operation, its operands and its source location before the process ends, and in what form, is an open owner decision (OD101).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD101
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on STMT-2, STMT-7, ARITH-3. Current main: the binary writes nothing and ends by `SIGABRT`, and `--interp` reports a run-time error naming the trap (`arithmetic overflow (checked +)`). The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §9.2 described a message `concrete: arithmetic overflow in function 'f' at file:line` with the operation, the operands and the policy. Options: (a) the binary writes nothing; (b) it writes one line naming the trap and its location; (c) the §9.2 form. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30).

<a id="OPEN-102"></a>
### OPEN-102. `Float32` and `Char` as type spellings

**OPEN-102@1.** Whether `Float32` and `Char` are other spellings of `f32` and `char`, as `Float64` and `Bool` are of `f64` and `bool`, is an open owner decision (OD102).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD102
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-19, SYN-3. Current main: the parser maps `Float64` and `Bool` to `f64` and `bool` but not `Float32` or `Char`, so `fn g(x: f32) -> Float32 { return x; }` is refused (E0220) while the resolver's `tyFromName` maps all four. The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §1 names `Float32`/`f32` as two spellings. Options: (a) `Float32` and `Char` become spellings of `f32` and `char`; (b) one spelling per primitive type, the capitalized spellings removed. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30).

<a id="OPEN-103"></a>
### OPEN-103. `as` applied to a reference

**OPEN-103@1.** Whether `as` may be applied to a reference, given that REF-5 lists borrowing and projecting as the only operations that form a reference, is an open owner decision (OD103).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD103
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-5, REF-6, ARITH-5, TRUST-4. Current main: an identity cast between equal reference types is accepted and denotes the operand's place (`read(r as &Int)` with `r: &Int`, and `read(&v as &Int)`, exit 42 on both consumers); every other cast between reference types, `&mut Int as &Int` included, is E0553 at core-check under ARITH-5's tag, whose sentence covers only numeric, `bool` and `char` conversions; a cast from a reference to a raw pointer is allowed (TRUST-4). The design record (MEMORY_SEMANTICS §15.3) listed a cast of a traced reference binding (`r as &U`) among the reference-forming operations. Options: (a) refuse `as` whenever its operand or target is a reference type; (b) allow the identity cast as a no-op on the operand's place. Austral: not checked. Pick: none given (raised by the MEMORY_SEMANTICS absorption, 2026-10-30).

## Decided

These entries were retired when the owner answered them; their IDs are never reused.

- **OD47** (loops, decided 2026-10-29, never an open entry): `while` and the counted loop `for i from a
  until b`, the C-style `for` removed. Now STMT-6@3, STMT-11@2 and STMT-12 (pending, bug 331); OD47 left OPEN-48 to
  OPEN-51.
- **OD23 – OD32** (the strictness pass, decided 2026-10-29, never open entries): REF-11@3 (OD23),
  STMT-7@3 (OD24), PAT-3@3 and PAT-5@3 (OD25), TYPE-6@2 (OD26), STMT-6@2 (OD27), TRAIT-10@2 (OD28),
  OWN-7@3 (OD29), MEM-4@3 (OD30), ARITH-2@2 (OD31) and OWN-15@3 (OD32), each pending its bug (319 –
  328); OD28 and OD30 left OPEN-35 and OPEN-36.
- **OPEN-1** (OD1, references): decided 2026-10-29. Now REF-5 (formation, `&s.f`, no temporary root), REF-1 and REF-3 (second-class), TRUST-3 (the raw-pointer validity premise, pending) and REF-8 (loans keyed by the root variable, as OD15 (a) confirmed).
- **OPEN-4** (OD4, foreign function values): decided 2026-10-29. Now FFI-1.
- **OPEN-5** (OD5, generics and Copy): decided 2026-10-29. Now OWN-D1, TYPE-13 and TYPE-14.
- **OPEN-6** (OD6, trait coherence): decided 2026-10-29. Now TRAIT-1 to TRAIT-7.
- **OPEN-7** (OD7, an unsafe-module marker): decided 2026-10-29. Now TRUST-6@2 (pending, R-0509).
- **OPEN-10** (OD10, arithmetic): decided 2026-10-29. Now ARITH-3, ARITH-4 and STMT-2 (pending).
- **OPEN-11** (OD11, casts): decided 2026-10-29. Now ARITH-5 (partial) and TRUST-1 (pending).
- **OPEN-12** (OD12, contracts): decided 2026-10-29. Now CON-1 (pending) and CON-3 (partial).
- **OPEN-13** (OD13, raw-pointer ownership): decided 2026-10-29. Now TRUST-7.
- **OPEN-14** (OD14, type validity): decided 2026-10-29. Now REF-2 and REF-4 (the single judgment before specialization is R-0499's).
- **OPEN-18** (OD18, match arm order and unreachable arms): decided 2026-10-29. Now PAT-9 (pending, R-0500).
- **OPEN-19** (OD19, the provable subset): decided 2026-10-29. Now the scope of [PROVABLE_V1.md](../verification/PROVABLE_V1.md), not a language rule.
- **OPEN-2** (OD2, `&mut` arguments, deferred loans and the exit order): decided 2026-10-29. Now
  REF-10@2 (lending), REF-11@3 (exclusivity at a call, keyed by the root as OD15 (a) tightened it), REF-13 (a `&mut` refers to the
  place), OWN-16 (a deferred call's borrow lives until it runs) and STMT-11 (one exit sequence).
- **OPEN-9** (OD9, modules and interfaces): decided 2026-10-29. Now MOD-7 (pending, R-0556).
- **OPEN-3** (OD3, pattern binders through a borrowed scrutinee): decided 2026-10-29, as enforced.
  Now REF-9 (the binder is always a reference), PASS-2 (its mode stored in Core) and REF-8 (the root
  is not reassigned while the binder's loan lives).
- **OPEN-15** (OD15, freezing and `&mut` exclusivity): decided 2026-10-29. Part (a), whole-owner
  freezing with loans keyed by the root binding, is REF-11@3 (it tightens OD2's part 2; partial, bug 319,
  for OD23's re-lent case).
  Part (b), the strict option, is REF-14 (pending, R-0496): no pass exploits `&mut` exclusivity until
  it is proved, and an alias derived from a loan is valid only while the loan lives.
- **OPEN-16** (OD16, a struct literal in a condition head): decided 2026-10-29, refused unless
  parenthesized, with no exceptions. Now SYN-4 (pending, bug 318); what counts as parenthesized inside
  a call's arguments is OPEN-34.
- **OPEN-17** (OD17, field writes and `mut`): decided 2026-10-29. Now OWN-10@2 (partial, bug 216).
- **OPEN-8** (OD8, global state): decided 2026-10-29, constants only and global resources as linear
  tokens. Now TRUST-8, TRUST-9 and TRUST-10 of [trusted.md](trusted.md#TRUST-8) (pending, R-0557).

- **OPEN-20** (OD20, the visibility of std's `std.libc` bindings): decided 2026-10-29, private to std.
  Now MOD-6 of [modules.md](modules.md#MOD-6) (enforced by bug 226's fix).
- **OPEN-21** (OD21, a parent module's rights over its child): decided 2026-10-29, only the declaring
  module. Now TYPE-11 of [types.md](types.md#TYPE-11), enforced as written for a parent too (bug
  311's fix).
- **OPEN-22** (OD22, trust on an `impl` block): decided 2026-10-29, per method only. Now TRUST-5@2 of
  [trusted.md](trusted.md#TRUST-5).

