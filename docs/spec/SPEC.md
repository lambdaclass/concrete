# The Concrete Language Specification

<!-- GENERATED from docs/spec/*.md by `python3 scripts/spec/spec.py single`; do not edit: edit the chapters. -->

This is the whole specification as one document, generated from its chapters in reading order. Each
rule keeps its anchor, so `#REF-3` links here as in its chapter. How to read a rule is in the overview,
the first chapter.

## Contents

- [Overview](#ch-overview)
- [Syntax](#ch-syntax)
  - [SYN-1. Decided syntax governs the derived grammar](#SYN-1)
  - [SYN-2. Operators group by a fixed precedence](#SYN-2)
  - [SYN-3. Compiler spellings are reserved](#SYN-3)
  - [SYN-4. A struct literal in a condition head is parenthesized](#SYN-4)
  - [SYN-5. Source text and string literals are UTF-8](#SYN-5)
  - [SYN-6. Tokens](#SYN-6)
  - [SYN-7. `>>` closes two type-argument lists](#SYN-7)
  - [SYN-8. A `match` that begins a statement is a statement](#SYN-8)
  - [SYN-9. A value `if` has an `else`](#SYN-9)
  - [SYN-10. The separators `.` and `::`](#SYN-10)
- [Modules and Names](#ch-modules)
  - [MOD-1. Every name resolves to one declaration](#MOD-1)
  - [MOD-2. Only `pub` declarations cross a module boundary](#MOD-2)
  - [MOD-3. A module binds each name once](#MOD-3)
  - [MOD-4. A `mod` declaration loads one module file](#MOD-4)
  - [MOD-6. The bindings of `std.libc` are private to std](#MOD-6)
  - [MOD-7. A module's interface is its `pub` declarations, generated](#MOD-7)
  - [MOD-8. A generated call is bound by identity](#MOD-8)
  - [MOD-9. An import binds the declaration it names](#MOD-9)
- [Scope and Binding](#ch-scope)
  - [SCOPE-D1. Visible](#SCOPE-D1)
  - [SCOPE-1. One lexical name, one binding](#SCOPE-1)
  - [SCOPE-2. What is not a collision](#SCOPE-2)
  - [SCOPE-3. Blocks are lexical on every consumer](#SCOPE-3)
- [Types](#ch-types)
  - [TYPE-D1. Capability set](#TYPE-D1)
  - [TYPE-1. A callable value is a function pointer with explicit context](#TYPE-1)
  - [TYPE-2. No dynamic dispatch](#TYPE-2)
  - [TYPE-3. `Text` is owned](#TYPE-3)
  - [TYPE-4. One type per expression, no implicit conversion](#TYPE-4)
  - [TYPE-5. Conditions are `bool`](#TYPE-5)
  - [TYPE-6. Operators take their declared primitive types](#TYPE-6)
  - [TYPE-7. Aggregates are built and read by their declarations](#TYPE-7)
  - [TYPE-8. Only a reference or a raw pointer is dereferenced](#TYPE-8)
  - [TYPE-9. Calls match their signatures](#TYPE-9)
  - [TYPE-10. Builtins have fixed signatures](#TYPE-10)
  - [TYPE-11. A module keeps its types' representation](#TYPE-11)
  - [TYPE-12. A type alias is another name for the same type](#TYPE-12)
  - [TYPE-13. A bare type parameter is linear-capable](#TYPE-13)
  - [TYPE-14. Copy is declared, never derived](#TYPE-14)
  - [TYPE-15. A generic type takes the arguments it declares](#TYPE-15)
  - [TYPE-16. A struct has a finite size](#TYPE-16)
  - [TYPE-17. A function type carries its capability set](#TYPE-17)
  - [TYPE-18. Capabilities have no run-time representation](#TYPE-18)
  - [TYPE-19. The primitive numeric types](#TYPE-19)
  - [TYPE-20. A `char` is one byte](#TYPE-20)
  - [TYPE-21. Newtype declarations are refused](#TYPE-21)
  - [TYPE-22. A constant is evaluated where it is used](#TYPE-22)
  - [TYPE-23. The builtin `Option` and `Result`](#TYPE-23)
  - [TYPE-24. Newtype conversions are refused](#TYPE-24)
- [Arithmetic](#ch-arithmetic)
  - [ARITH-1. Floating point is IEEE-754 with round-to-nearest-even](#ARITH-1)
  - [ARITH-2. An integer literal fits its type](#ARITH-2)
  - [ARITH-3. Integer arithmetic is checked](#ARITH-3)
  - [ARITH-4. Wrapping and saturating arithmetic are named operations](#ARITH-4)
  - [ARITH-5. `as` converts without loss](#ARITH-5)
  - [ARITH-6. Comparisons follow their operand type](#ARITH-6)
  - [ARITH-7. A float literal is correctly rounded](#ARITH-7)
  - [ARITH-8. The fill of a right shift](#ARITH-8)
  - [ARITH-9. No ambient arithmetic mode](#ARITH-9)
  - [ARITH-10. The named conversions' results](#ARITH-10)
  - [ARITH-11. The named float conversions round as stated](#ARITH-11)
  - [ARITH-12. Float remainder](#ARITH-12)
  - [ARITH-13. A division by a proven zero or a proven overflow is refused](#ARITH-13)
- [Statements, Expressions and Execution](#ch-statements)
  - [STMT-1. Exit, abort and divergence are distinct behaviours](#STMT-1)
  - [STMT-2. Allocation errors conserve; traps abort](#STMT-2)
  - [STMT-3. Programs are single-threaded](#STMT-3)
  - [STMT-4. Evaluation is left to right, assignment evaluates its value first](#STMT-4)
  - [STMT-5. No exit inside an operand](#STMT-5)
  - [STMT-6. Loops are statements; `break` and `continue` name a loop](#STMT-6)
  - [STMT-7. `main` declares target grants and returns u8](#STMT-7)
  - [STMT-8. A trailing expression is the block's value](#STMT-8)
  - [STMT-9. A function with a result type returns on every path](#STMT-9)
  - [STMT-10. A test function is an entry point](#STMT-10)
  - [STMT-11. One exit sequence](#STMT-11)
  - [STMT-12. A counted loop carries its own measure](#STMT-12)
  - [STMT-13. `abort()` needs `Process`](#STMT-13)
- [Patterns](#ch-patterns)
  - [PAT-1. A pattern field may be bound under another name](#PAT-1)
  - [PAT-2. A match is exhaustive](#PAT-2)
  - [PAT-3. Every arm fits its scrutinee](#PAT-3)
  - [PAT-4. Match guards are refused](#PAT-4)
  - [PAT-5. A destructuring `let` binds every field](#PAT-5)
  - [PAT-6. Conditional destructuring sugar is refused](#PAT-6)
  - [PAT-7. Alternatives remain; ranges and guards are refused](#PAT-7)
  - [PAT-8. C-style loop headers are refused](#PAT-8)
  - [PAT-9. The first matching arm wins](#PAT-9)
  - [PAT-10. Irrefutable enum destructuring matches the rest of its block](#PAT-10)
  - [PAT-11. `let mut` makes every binder of a pattern mutable](#PAT-11)
- [Ownership](#ch-ownership)
  - [OWN-D1. Copy](#OWN-D1)
  - [OWN-1. A moved value is not used again](#OWN-1)
  - [OWN-2. Every owned linear binding is consumed before every exit](#OWN-2)
  - [OWN-3. Branches agree on what they consume](#OWN-3)
  - [OWN-4. A loop body does not consume an outer value](#OWN-4)
  - [OWN-5. A linear value is never discarded](#OWN-5)
  - [OWN-6. A fallible result is used](#OWN-6)
  - [OWN-7. A result is not thrown away unwritten](#OWN-7)
  - [OWN-8. A non-Copy part is not read out of a place](#OWN-8)
  - [OWN-9. An assignment never overwrites a live linear value](#OWN-9)
  - [OWN-10. Only a `mut` binding is assigned or mutably borrowed](#OWN-10)
  - [OWN-11. A move happens only where its code always runs](#OWN-11)
  - [OWN-12. `defer` reserves its arguments until the scope exits](#OWN-12)
  - [OWN-13. Nothing is destroyed implicitly](#OWN-13)
  - [OWN-14. A `Copy` declaration has only Copy fields](#OWN-14)
  - [OWN-15. Every surface form has one value-flow behaviour](#OWN-15)
  - [OWN-16. A borrow given to a deferred call lives until the call runs](#OWN-16)
  - [OWN-17. A deferred call returns only Unit](#OWN-17)
- [References](#ch-references)
  - [REF-D1. Contains a reference](#REF-D1)
  - [REF-D2. Holds a reference](#REF-D2)
  - [REF-D3. Borrow block](#REF-D3)
  - [REF-1. No function returns a reference](#REF-1)
  - [REF-2. No function type returns a reference](#REF-2)
  - [REF-3. No reference is stored](#REF-3)
  - [REF-4. Generic instantiation preserves REF-1 and REF-3](#REF-4)
  - [REF-5. A reference is a root binding plus projections](#REF-5)
  - [REF-6. Reference bindings are never introduced by `let` and never assigned](#REF-6)
  - [REF-7. No reference is written through a reference](#REF-7)
  - [REF-8. A reference's root stays the same live binding](#REF-8)
  - [REF-9. A binder through a reference is a reference](#REF-9)
  - [REF-10. A `&mut` argument is an implicit reborrow](#REF-10)
  - [REF-11. Loans are keyed by root binding](#REF-11)
  - [REF-12. Nothing is mutated through a shared reference](#REF-12)
  - [REF-13. A `&mut` refers to the place](#REF-13)
  - [REF-14. An alias derived from a loan lives no longer than the loan](#REF-14)
- [Memory](#ch-memory)
  - [MEM-D1. Ownership state](#MEM-D1)
  - [MEM-1. Two-layer memory](#MEM-1)
  - [MEM-2. Addresses are quasi-concrete](#MEM-2)
  - [MEM-3. Every binding is initialized](#MEM-3)
  - [MEM-4. Field access dereferences at most one reference](#MEM-4)
  - [MEM-5. A `Heap` value owns one heap cell](#MEM-5)
  - [MEM-6. An array index is checked](#MEM-6)
  - [MEM-7. The heap builtins require `Alloc`](#MEM-7)
  - [MEM-8. Indexing dereferences at most one reference](#MEM-8)
  - [MEM-9. An index proven out of bounds is refused](#MEM-9)
- [Traits](#ch-traits)
  - [TRAIT-D1. Impl target](#TRAIT-D1)
  - [TRAIT-1. One impl of a trait per type](#TRAIT-1)
  - [TRAIT-2. Impls of a trait do not overlap](#TRAIT-2)
  - [TRAIT-3. An impl lives with its trait or its type](#TRAIT-3)
  - [TRAIT-4. An impl covers every instantiation of its type](#TRAIT-4)
  - [TRAIT-5. One method of a name per type](#TRAIT-5)
  - [TRAIT-6. A bound call has one candidate](#TRAIT-6)
  - [TRAIT-7. Impl selection does not depend on order](#TRAIT-7)
  - [TRAIT-8. An impl names a declared trait](#TRAIT-8)
  - [TRAIT-9. An impl conforms to its trait](#TRAIT-9)
  - [TRAIT-10. A bound is satisfied by an impl](#TRAIT-10)
  - [TRAIT-11. Destruction cannot fail](#TRAIT-11)
- [Contracts](#ch-contracts)
  - [CON-1. Claims are partial correctness](#CON-1)
  - [CON-2. What `requires`, `ensures`, `invariant` and `variant` claim](#CON-2)
  - [CON-3. A contract expression is a pure specification](#CON-3)
  - [CON-4. Ghost code stays out of runtime code](#CON-4)
  - [CON-5. A specification function is erased](#CON-5)
  - [CON-6. What `assert` and `assume` claim](#CON-6)
  - [CON-7. `result` is bound only in a postcondition](#CON-7)
  - [CON-8. Stacked clauses are a conjunction](#CON-8)
- [Trusted Code and Raw Pointers](#ch-trusted)
  - [TRUST-D1. What a capability means](#TRUST-D1)
  - [TRUST-1. Pointer arithmetic and address exposure are named operations](#TRUST-1)
  - [TRUST-2. Safe code has no undefined behaviour](#TRUST-2)
  - [TRUST-3. A reference from a raw pointer carries a premise](#TRUST-3)
  - [TRUST-4. A function exercises only the capabilities it declares](#TRUST-4)
  - [TRUST-5. `trusted` absorbs `Unsafe` and nothing else](#TRUST-5)
  - [TRUST-6. Trust is inventoried per declaration](#TRUST-6)
  - [TRUST-7. Moving a value out of raw memory is explicit](#TRUST-7)
  - [TRUST-8. A program has no mutable global state](#TRUST-8)
  - [TRUST-9. Resources originate in target grants](#TRUST-9)
  - [TRUST-10. Explicit resources and exact callback permissions](#TRUST-10)
  - [TRUST-11. A raw store neither reads nor destroys the old contents](#TRUST-11)
  - [TRUST-12. Trusted code keeps the move, cleanup and freeze rules](#TRUST-12)
  - [TRUST-13. Reference-to-pointer conversion is gated](#TRUST-13)
- [Foreign Functions](#ch-ffi)
  - [FFI-1. A foreign function is only called directly](#FFI-1)
  - [FFI-2. One Layout authority for AAPCS64](#FFI-2)
  - [FFI-3. An extern declaration declares its effects and FFI-safe types](#FFI-3)
  - [FFI-4. Layout attributes are well formed](#FFI-4)
  - [FFI-5. Concrete symbols cannot collide with C symbols](#FFI-5)
  - [FFI-6. A compiler intrinsic is not a foreign binding](#FFI-6)
  - [FFI-7. Layout is unspecified unless a `repr` fixes it](#FFI-7)
  - [FFI-8. What `repr(C)` and `repr(packed)` promise](#FFI-8)
  - [FFI-9. What `trusted extern fn` asserts](#FFI-9)
  - [FFI-10. The layout forms](#FFI-10)
  - [FFI-11. A `repr(C)` struct crosses an extern call by value](#FFI-11)
  - [FFI-12. The stride of a buffer is `sizeof`](#FFI-12)
- [Pipeline Preservation](#ch-pipeline)
  - [PASS-1. Specialization preserves meaning](#PASS-1)
  - [PASS-2. Binder modes are fixed before specialization](#PASS-2)
  - [PASS-3. The native profile is a forward simulation from checked Core to AArch64](#PASS-3)
  - [PASS-4. The LLVM profile is trusted and differential-tested](#PASS-4)
  - [PASS-5. After Check: linearity resolved, operand types agree](#PASS-5)
  - [PASS-6. After Elab: desugaring adds nothing](#PASS-6)
  - [PASS-7. After Mono: no type variable survives](#PASS-7)
  - [PASS-8. After CoreCheck: no invalid Core proceeds](#PASS-8)
  - [PASS-9. After Lower: verified SSA with every runtime check](#PASS-9)
  - [PASS-10. After Emit: checked operations stay checked, executables have an entry point](#PASS-10)
  - [PASS-11. BackendIR keeps backend intent structured](#PASS-11)
  - [PASS-12. An accepted program is compiled](#PASS-12)
  - [PASS-13. Every consumer gives the checked Core behaviour](#PASS-13)
- [Owner Decisions](#ch-decisions)

<a id="ch-overview"></a>
## Overview

Concrete is a small systems language for software that must be reviewed and proved. Its values are
linear: a value that owns a resource is consumed exactly once, and nothing is destroyed implicitly.
References are scoped access, never stored or returned. A function's authority is written in its
signature (`with(Fs, Alloc)`), arithmetic is checked, traps abort, and allocation failure is an ordinary conserving error. Its compiler is
written in Lean 4 and is being built toward an end-to-end verified pipeline. The design follows
Austral and departs from it only with a written reason.

### How to read this specification

The specification is a set of entries. An entry has an ID (`REF-3`), a title and one normative
sentence tagged with its version (`REF-3@1`); a diagnostic cites the same tag. The sentence is the
rule. The fields after it give the entry's kind (a refusal, which the compiler reports with a code;
a guarantee about what programs do; a definition; a pipeline stage's contract; or an open question
only the project owner decides), whether it is enforced, partly enforced or pending, and the compiler
declarations that enforce it. The rationale explains the rule and is not normative. The examples are
programs the gate compiles and runs: an accepted example exits with the status it states, a rejected
one fails at the stated stage with the stated code, and a pending one records what the compiler does
until the rule is implemented. Text outside entries, like this chapter, states no rule.

Where Core's semantics in Lean defines a rule, that definition governs and the entry names it
(`Semantics:`); Lean is the only normative source as it lands; until then chapters state decided rules and
[decisions.md](#ch-decisions) governs any remaining disagreement, under the 2026-11-03 prose cleanup decision. The owner's open
questions are the last chapter, in the order to read them. The whole specification is also one
generated page, [SPEC.md](SPEC.md), and how it is maintained (format, registry, gates) is
[MAINTAINERS.md](MAINTAINERS.md).
The terms this specification uses are in the [glossary](#glossary) at the end of this chapter, and
the entries that govern each construct are in [rules by construct](#rules-by-construct).

### Chapters

The chapters in reading order. The prefix is the one their entry IDs carry.

| chapter | prefix | covers |
|---|---|---|
| [Overview](#ch-overview) | `LANG-` | what Concrete is, how to read this specification, the language's invariants |
| [Syntax](#ch-syntax) | `SYN-` | the grammar, operator precedence, reserved spellings, struct literals in conditions |
| [Modules and Names](#ch-modules) | `MOD-` | name resolution, visibility, module files, imports, interfaces |
| [Scope and Binding](#ch-scope) | `SCOPE-` | where a binding is visible, no shadowing |
| [Types](#ch-types) | `TYPE-` | the types, typing of expressions and calls, aggregates, generics, capability sets in types |
| [Arithmetic](#ch-arithmetic) | `ARITH-` | integers and floats, literals, checked arithmetic, conversions, comparisons |
| [Statements, Expressions and Execution](#ch-statements) | `STMT-` | outcomes, evaluation order, loops, `main`, the exit sequence, failure |
| [Patterns](#ch-patterns) | `PAT-` | `match`, exhaustiveness, destructuring `let` |
| [Ownership](#ch-ownership) | `OWN-` | linearity, Copy, discards, assignment, `defer`, destruction |
| [References](#ch-references) | `REF-` | forming, passing and borrowing references, loans |
| [Memory](#ch-memory) | `MEM-` | ownership states, initialization, field access, `Heap`, array bounds |
| [Traits](#ch-traits) | `TRAIT-` | traits, impls, bounds, coherence |
| [Contracts](#ch-contracts) | `CON-` | `requires`, `ensures`, invariants, ghost code |
| [Trusted Code and Raw Pointers](#ch-trusted) | `TRUST-` | capabilities, `trusted`, raw pointers, global resources |
| [Foreign Functions](#ch-ffi) | `FFI-` | `extern`, layout, symbols |
| [Pipeline Preservation](#ch-pipeline) | `PASS-` | what each compiler stage preserves |
| [Open Owner Decisions](#ch-decisions) | `OPEN-` | the questions only the project owner decides |

### The language's invariants

This chapter is explanatory: it states the invariants the whole language keeps and names the
entries of this specification that make each one exact. It states no rule of its own and has no
entries. An invariant with no entry yet is a gap in the specification, tracked by R-0555 (b).

| invariant | what it means | stated by |
|---|---|---|
| Authority-free by default | A function without `with(...)` exercises no capability and calls nothing that needs one. This is an authority claim, not a totality claim: it may still trap on a checked operation, diverge or exhaust the stack; proof eligibility and stronger claims are separate judgments. | TRUST-4 |
| True linear types | Every linear value is consumed exactly once: a leak and a double use are compile errors. Passing by value, returning, moving into a struct field or an array literal element, a `match` or destructuring `let` over it (every field bound, E0533) consume; borrowing, taking a raw pointer, and a `defer` (which reserves until it runs) do not. Each stage that adds a consumption form updates OWN-15. | OWN-D1, OWN-1 to OWN-15 |
| No hidden control flow | `a + b` on integers is primitive addition, not a call; the compiler never inserts a destructor call; allocation shows as `with(Alloc)`; a recoverable error leaves a function only through a written `return`. A `defer` is not hidden: the programmer writes it, its run point (the scope's exit) is visible from the block structure, and it calls exactly the function written. | OWN-12, OWN-13, TRUST-4, STMT-11 |
| No lexical value shadowing | A new local binding may not reuse the spelling of a visible local value binding; assignment introduces no binding (`acc = f(acc, x)` is legal for a `mut` place whose old value it consumes); sibling arms may reuse a name. Elab still renames binders apart (PRINCIPLES #12, bug 045): the source rule improves audit clarity and does not replace compiler-owned identity. | SCOPE-1 to SCOPE-3 |
| No uninitialized variables | Every binding is declared with a value. | MEM-3 |
| No operator overloading | The operators are built in on the primitive types; a user type uses named functions. | TYPE-6, TYPE-2 |
| No implicit conversions | No silent coercion; every conversion is written. | TYPE-4, ARITH-5 |
| No null | An optional value is an `Option<T>`. | gap: no entry yet |
| No exceptions | Errors are values, propagated by an exhaustive `match` and an explicit `return`; allocation failure conserves inputs and returns an ordinary error; traps abort. | STMT-2, "Recoverable failure" in statements.md |
| No global mutable state | A program holds no mutable global state; resource values originate in the target grants `main` declares and consumes, with permissions checked separately (authority rules). | TRUST-8, TRUST-9, TRUST-10 (pending) |
| No interior mutability in safe code | All mutation flows through `&mut`; a later low-level `UnsafeCell<T>` would be gated by `Unsafe`. | decided: OD112; no safe interior mutability |
| Local-only type inference | Signatures are fully annotated; inside a body a local's type may come from its initializer, right to left only, with no cross-statement constraint solving and no broad bidirectional inference. | TYPE-4 (in part); gap |
| LL(1) grammar | Every parsing decision takes one token of lookahead, with no ambiguity and no backtracking. | SYN-1 |
| `abort()` is terminal | No deferred call, `Destroy` implementation or drop glue runs after an abort; hosted execution ends the process (usually status 134 on POSIX, so tests check for a nonzero status), and a freestanding profile calls a declared non-returning handler (R-0223, R-0322). | STMT-2, "Abort and failure" in statements.md |

**Project and compiler commitments** that are not rules of the language but are kept with it:

- **Readability is part of correctness.** Surface syntax, std APIs and trusted boundaries stay
  explicit enough that ordinary code reads and audits predictably; a feature that hides low-level
  behaviour raises audit cost and is a correctness concern, not style.
- **New features justify their cost** in grammar, audit and proof; one that makes the language
  harder to parse, review or prove without a correspondingly large benefit is rejected or delayed
  ([admission test](../project/PHILOSOPHY.md#admission-test)).
- **Reproducible builds.** The same source and compiler give the same binary: no timestamps, random
  seeds or environment-dependent output.
- **Diagnostics stay structured** through the main pipeline; the compiler may report several errors
  across functions or modules but does not rely on broad recovery or ambiguous parse fallback.
- **Front-end acceptance implies back-end compilation.** A program the checker accepts reaches a
  binary or fails only at a documented trap: an internal rejection (the E07xx SSA-verify family) of
  checked code is always a compiler bug, and `--interp` runs no program the compiler cannot compile.
  It was violated when `typesCompatible` let mixed-width operations (`i8 < i32`) through Check and
  the interpreter and they died at SSA verification (E0715; fixed 2026-07-01, E0228 now refuses
  them); shared predicates (`binOpOperandsAgree`) are the mechanism. Enforced by the differential
  fuzzer's taxonomy (`fuzz_differential.py` classifies any E07xx, panic or crash on a well-typed
  generated program as a compiler bug) and `check_mixed_width_binops.sh`; PASS-5 to PASS-10 state it
  per stage.

### Glossary

This section is explanatory and states no rule. Each term points at the entry or section that fixes
it; where an entry defines the term, that entry is the definition and this table only finds it.

| term | meaning in this specification | defined or governed by |
|---|---|---|
| entry | an item with an ID, a versioned normative sentence and fields, of one of the kinds below | "How to read this specification" above, [MAINTAINERS.md](MAINTAINERS.md) §2.1 |
| refusal, guarantee, definition, preservation, open | the kinds an entry may have; the `Kind:` field of MAINTAINERS.md §2.1 defines each | MAINTAINERS.md §2.1 (`Kind:`) |
| open entry | an entry of kind `open`: a question only the project owner decides; it encodes no answer | [decisions.md](#ch-decisions), MAINTAINERS.md §2.1 |
| enforced, partial, pending | an entry's status: implemented; holding on its examples that are not pending examples, with the pending examples tracked separately; decided and not implemented | MAINTAINERS.md §2.1 (`Status:`) |
| pending example | an example of an entry not yet implemented, run with `now=` recording what the compiler does today; it never counts as a pass | MAINTAINERS.md §2.2 |
| consumer | a program that runs a checked program: the native binary, `--interp`, and Core's executable semantics once it exists | MAINTAINERS.md §2.2, PASS-13 |
| profile | a way the compiler produces an executable: the native profile and the LLVM profile | PASS-3, PASS-4, OPEN-124 |
| Core | the compiler's internal language after elaboration; MAINTAINERS.md ("Authority") states the standing of its Lean semantics | MAINTAINERS.md ("Authority"), [pipeline.md](#ch-pipeline) |
| Check | the stage that checks the surface program: types, ownership, references, capabilities | PASS-5 |
| Elab | elaboration: the stage that turns the checked program into Core, desugaring as it goes | PASS-6 |
| CoreCheck | the stage that checks Core before it proceeds | PASS-8 |
| Mono | monomorphization: the stage that specializes every generic at its type arguments | PASS-1, PASS-2, PASS-7 |
| Lower, Emit | the stages that turn Core into verified SSA and SSA into LLVM IR | PASS-9, PASS-10, PASS-11 |
| builtin | a function, type or enum the compiler provides with no declaration in the program | functions: the [builtin catalog](#builtin-functions), TYPE-10; primitive types: TYPE-19, TYPE-20; `Option` and `Result`: TYPE-23; names: MOD-1 |
| intrinsic | an operation the compiler implements, identified by compiler identity with fixed effects; source annotations do not grant intrinsic identity | FFI-6, the builtin catalog |
| place | a root binding followed by field or index projections | REF-5 |
| owned value | a value a local binding holds by value rather than through a reference; MEM-D1 names its states | MEM-D1 |
| Copy | a type whose values may be duplicated | OWN-D1, TYPE-14, OWN-14 |
| linear | a type that is not Copy (OWN-D1); OWN-1 and OWN-2 govern how its values are used | OWN-D1, OWN-1, OWN-2 |
| consume, move | the uses of a value that OWN-15 lists as moves (passing it by value, returning it, moving it into a field, destructuring it and others); MEM-D1 and OWN-1 govern the binding's state afterwards, and reading or moving a Copy binding leaves that binding's state unchanged (MEM-D1) | OWN-15, MEM-D1, OWN-1 |
| reserved, frozen | two binding states MEM-D1 names: reserved is a binding passed by value to a pending `defer` (OWN-12), frozen is the owner of an enclosing borrow block (REF-D3) | MEM-D1, OWN-12, REF-D3 |
| reference | a value of type `&T` or `&mut T`; the entries cited govern where it may flow | REF-D1, REF-D2, REF-1, REF-3, REF-5 |
| borrow | forming a reference to a place (`&x`, `&mut x`), or a borrow block | REF-5, REF-D3 |
| borrow block | `borrow x as r { B }`, which lends `x` to `r` for `B` | REF-D3 |
| loan | a borrow seen from its root binding | REF-11, REF-8, STMT-11 |
| capability | a named logical authority, such as `Fs` or `Alloc` | TRUST-D1, TRUST-4 |
| capability set | the capabilities and capability variables of a `with(...)` or a capability argument | TYPE-D1, TYPE-17, TYPE-18 |
| `Unsafe` | the capability for raw-memory and foreign operations | TRUST-D1, TRUST-4 |
| trusted | a `trusted fn` (TRUST-5, TRUST-12) or a `trusted mod` (TRUST-6, pending); "Trusted premises" collects what they leave unchecked | TRUST-5, TRUST-6, TRUST-12; "Trusted premises" in [trusted.md](#ch-trusted) |
| premise | a fact the compiler relies on and does not check; "Trusted premises" lists each and who states it | TRUST-2, "Trusted premises" in trusted.md |
| abort, trap | the ending of a process by `abort()` or by a failed run-time check; the entries cited govern both | STMT-2, STMT-13, ARITH-3, MEM-6 |
| behaviour | the trace of external events of one execution with its ending | STMT-1 |
| exit sequence | the sequence STMT-11 (pending) describes for a scope exit that does not abort | STMT-11 |
| visible | where a local binding may be named | SCOPE-D1 |
| impl target | the type an impl is for | TRAIT-D1 |
| ghost | code and bindings for contracts only, erased before code generation | CON-4, CON-5 |

### Rules by construct

This section is explanatory and states no rule. For each construct of the language it lists the
entries that govern it, so a reader can go from a piece of syntax to its rules. The list follows the
chapters; an entry not listed here can still bear on a construct.

| construct | entries |
|---|---|
| `let x: T = e` | MEM-3, TYPE-4, SCOPE-1, SCOPE-D1, REF-6, OWN-5 |
| destructuring `let`, `let ... else` | PAT-5, PAT-8, PAT-10, PAT-11, PAT-1, OPEN-52 |
| `mut` | OWN-10, PAT-11 |
| assignment `p = e` | OWN-9, OWN-10, OWN-4, REF-6, REF-7, REF-12, STMT-4 |
| expression statement `e;` | OWN-5, OWN-6, OWN-7 |
| block value (trailing expression) | STMT-8 |
| `if` | TYPE-5, SYN-9, SYN-4, STMT-8, OWN-3 |
| `match` | PAT-2, PAT-3, PAT-9, PAT-7, PAT-1, SYN-8, OWN-3, REF-9, PASS-2 |
| match guard `if g` | PAT-4, TYPE-5, OPEN-55 |
| `if let`, `while let` | PAT-6, OPEN-52 |
| `_` | OWN-5, PAT-2, PAT-3 |
| `while` | STMT-6, TYPE-5, OWN-4 |
| `for i from a until b` | STMT-6, STMT-12, STMT-11, OPEN-48, OPEN-49, OPEN-104, OPEN-105 |
| `break`, `continue` | STMT-6, STMT-5, STMT-11 |
| `return` | STMT-9, STMT-5, STMT-11, REF-1 |
| `defer` | OWN-12, OWN-16, OWN-17, STMT-11 |
| `fn main` | STMT-7, STMT-11, TRUST-9, PASS-10 |
| `#[test]` | STMT-10 |
| function call `f(a, b)` | TYPE-9, TYPE-4, STMT-4, STMT-5, REF-10, REF-11, OWN-15 |
| method call `x.m()` | TYPE-9, TRAIT-5, TRAIT-6, OWN-15, SYN-10 |
| function value, `fn(..) -> R` type | TYPE-1, TYPE-17, REF-2, FFI-1 |
| `with(...)` | TRUST-4, TRUST-D1, TYPE-D1, TYPE-17, TYPE-18, STMT-13, MEM-7 |
| `trusted fn` | TRUST-5, TRUST-6, TRUST-12, TRUST-2 |
| `trusted mod` | TRUST-6 |
| `extern fn`, `trusted extern fn` | FFI-1, FFI-3, FFI-5, FFI-9, TRUST-4, TRUST-5, MOD-6 |
| `#[intrinsic = "n"]` | FFI-6 |
| `#[repr(...)]` | FFI-4, FFI-7, FFI-8, FFI-10, FFI-11 |
| `struct` declaration | TYPE-16, TYPE-14, OWN-14, REF-3, FFI-7, OPEN-65 |
| struct literal `S { f: e }` | TYPE-7, TYPE-11, SYN-4, STMT-4, STMT-5, OPEN-53 |
| field access `e.f` | MEM-4, TYPE-7, TYPE-11, OWN-8 |
| `enum` declaration, variant literal | TYPE-7, OWN-14, PAT-3 |
| `Option`, `Result` | TYPE-23 |
| `newtype`, `.0` | TYPE-21, TYPE-24, TYPE-11, OPEN-54 |
| `type A = T` | TYPE-12 |
| `const` | TYPE-22, TRUST-8, REF-6 |
| generics `<T>`, `T: Copy` | TYPE-13, TYPE-15, TRAIT-10, REF-4, PASS-1, PASS-7 |
| `trait`, `impl` | TRAIT-D1, TRAIT-1 to TRAIT-11 |
| array literal `[a, b]`, `[e; N]` | TYPE-7, STMT-4, OPEN-149 |
| array index `a[i]` | MEM-6, MEM-8, TYPE-7, OWN-8, STMT-4 |
| `&x`, `&mut x` | REF-5, REF-8, REF-11, REF-12, REF-13, OWN-10 |
| `borrow x as r { B }` | REF-D3, REF-8, OPEN-57, OPEN-58 |
| `*e` | TYPE-8, TRUST-3, TRUST-7, TRUST-11; of an owned `Heap`: MEM-5, MEM-7 |
| raw pointers `*mut T`, `*const T` | TRUST-1, TRUST-4, TRUST-13, TRUST-7, TRUST-11 |
| `as` | ARITH-5, TYPE-24, TRUST-1, OPEN-98 |
| `+`, `-`, `*`, `/`, `%` | ARITH-3, TYPE-6, SYN-2, ARITH-1, ARITH-12, OPEN-114 |
| `<<`, `>>` | ARITH-3, ARITH-8, TYPE-6 |
| `==`, `<` and the other comparisons | ARITH-6, TYPE-6, TYPE-5, OPEN-42, OPEN-100 |
| `&&`, `\|\|`, `!` | TYPE-5, STMT-4, OWN-11 |
| integer literal | ARITH-2, SYN-6 |
| float literal | ARITH-7, OPEN-63 |
| string and char literals | SYN-5, SYN-6, TYPE-20, OPEN-137 |
| `wrapping_*`, `saturating_*` | ARITH-4 |
| `alloc`, consuming `*h`, `Heap<T, A>` | MEM-5, MEM-7 |
| `destroy`, `Destroy` | OWN-13, TRAIT-8, TRAIT-11 |
| `discard(e)` | OWN-5, OWN-7 |
| `abort()` | STMT-13, STMT-2 |
| other builtins | TYPE-10 for argument checking; the [builtin catalog](#builtin-functions) names the governing entries of each |
| `mod`, `import`, `pub` | MOD-2, MOD-4, MOD-7, MOD-9 |
| names and `::`, `.` | MOD-1, MOD-3, SYN-10, SCOPE-1 to SCOPE-3 |
| `#[requires]`, `#[ensures]`, `#[invariant]`, `#[variant]` | CON-1, CON-2, CON-3, CON-7, CON-8 |
| `assert`, `assume` | CON-6 |
| `ghost let`, `spec fn` | CON-4, CON-5 |

The explanatory section "Program execution" in [statements.md](#ch-statements) follows `fn main`
from source files to process exit and names the entries for each step.


<a id="ch-syntax"></a>
## Syntax

The decided language governs syntax (SYN-1, OD150). The derived
[EBNF](../../grammar/concrete.ebnf) and its LL(1) checks are awaiting resynchronization under OD77;
the frozen parser still accepts removed forms and refuses some decided forms.

Capability names `Fs`, `Network`, `Env`, `Time`, `Alloc`, `Device`, `Process` and `Unsafe` are
reserved everywhere (authority rule 13). `ghost`, `from` and `until` are reserved (OD73, OD81,
OD90 and OD50). Primitive types have canonical spellings (TYPE-19), and Unit is written `Unit`,
its value `()` (OD66, OD157). There are no capability aliases or umbrella capabilities.

Attributes form the decided fixed surface; `assert`, `assume` and ghost code are proof metadata
without runtime effects. Removed forms include inline modules, C-style for, conditional or
refutable destructuring sugar, newtypes, struct update, ranges, guards, named borrow regions,
value-carrying break and `trusted mod` (OD61). Refusal examples may contain a removed form to
show its rejection; accepted examples use its decided replacement.

### Rules

<a id="SYN-1"></a>
#### SYN-1. Decided syntax governs the derived grammar

**SYN-1@2.** A source file is parsed according to the decided language recorded in decisions.md, with grammar/concrete.ebnf a derived grammar awaiting resynchronization under OD77 rather than an independent authority over acceptance.

- Kind: refusal
- Uses: none
- Codes: E0001 (parse)
- Status: pending R-0555
- Enforced by: Concrete.parse
- Semantics: none
- Obligation: the recursive-descent parser accepts exactly the grammar's language (tested by examples; a gate comparing them is part of R-0335)
- Evidence: tested

**Rationale.** OD150 gives recorded decisions precedence until the derived grammar is resynchronized (OD77); parser implementation and the EBNF are conformance targets, not competing authorities.

```con reject name=unbalanced-parenthesis-decided code=E0001 stage=parse pair=balanced-parenthesis pending=R-0555 now=reject:E0001:parse
fn example_value() -> i64 {
    return (40 + 2;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=balanced-parenthesis exit=42
fn example_value() -> i64 {
    return (40 + 2);
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

One disambiguation is contextual, so the productions alone do not show it. In the head of an
`if`, `while` or `match`  a `{` ends the head and opens its block,
whatever name precedes it: there is no case heuristic, and a struct literal in a head is written in
parentheses (SYN-4, owner decision OD16 of 2026-10-29). Elsewhere, after an `IDENT`, a `{` begins a
struct literal's field list (`ident_tail`) only when the name begins with an uppercase letter, the
type-name convention. The current compiler does not implement the head rule yet: it applies the case rule in heads
too, so an uppercase constant just before a head's block is read as the start of a struct literal
and the head does not parse, and the E0001 diagnostic suggests parentheses ([bug
212](../bugs/212_const_before_block_parses_as_struct_literal.md)'s provisional repair). Bug 318
replaces that with SYN-4, and the reject example below, which pins the current compiler's behaviour, is retired when
it lands. This is a FIRST/FOLLOW conflict between `ident_tail` and the block after a head, which
the head context resolves; the checkers report it, and `grammar/ll1_known_conflicts.tsv` lists it.

```con accept name=head-constant-decided exit=42 pending=R-0555 now=reject:E0001:parse
const LIMIT: i64 = 16;

fn example_value() -> i64 {
    let mut n: i64 = 20;
    if n > LIMIT { n = LIMIT; }
    return n + 26;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=head-constant-parenthesized exit=42
const LIMIT: i64 = 16;

fn example_value() -> i64 {
    let mut n: i64 = 20;
    if (n > LIMIT) { n = LIMIT; }
    return n + 26;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

The token forms (SYN-6), the statement `match` (SYN-8), the value `if` (SYN-9) and the separators
(SYN-10) are refused here when broken:

```con reject name=float-exponent-decided code=E0001 stage=parse pair=float-digits pending=R-0555 now=reject:E0001:parse
fn main() -> u8 {
    let f: f64 = 1e3;
    return 42;
}
```

```con accept name=float-digits exit=42
fn main() -> u8 {
    let f: f64 = 1000.0;
    if f == 1000.0 { return 42; } return 1;
}
```

```con reject name=block-comment-decided code=E0001 stage=parse pair=line-comment pending=R-0555 now=reject:E0001:parse
/* a block comment */
fn main() -> u8 {
    return 42;
}
```

```con accept name=line-comment exit=42
// a line comment
fn main() -> u8 {
    return 42;
}
```

```con reject name=statement-match-continued-decided code=E0001 stage=parse pair=value-match-continued pending=R-0555 now=reject:E0001:parse
fn main() -> u8 {
    let x: i64 = 1;
    match x { _ => 40 } + 2;
    return 42;
}
```

```con accept name=value-match-continued exit=42
fn example_value() -> i64 {
    let x: i64 = 1;
    let y: i64 = match x { _ => 40 } + 2;
    return y;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=value-if-without-else-decided code=E0001 stage=parse pair=value-if-else pending=R-0555 now=reject:E0001:parse
fn example_value() -> i64 {
    let c: bool = true;
    let x: i64 = if c { 42 };
    return x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=value-if-else exit=42
fn example_value() -> i64 {
    let c: bool = true;
    let x: i64 = if c { 42 } else { 0 };
    return x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=hash-variant-decided code=E0001 stage=parse pair=colon-variant pending=R-0555 now=reject:E0001:parse
fn example_value() -> i64 {
    let o: Option<i64> = Option::<i64>#Some { value: 42 };
    match o {
        Option::Some { value } => { return value; },
        Option::None => { return 0; }
    }
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=colon-variant exit=42
fn example_value() -> i64 {
    let o: Option<i64> = Option::<i64>::Some { value: 42 };
    match o {
        Option::Some { value } => { return value; },
        Option::None => { return 0; }
    }
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

Outside a head, a `{` after a lowercase name does not begin a struct literal:

```con reject name=lowercase-struct-literal-decided code=E0001 stage=parse pair=uppercase-struct-literal pending=R-0555 now=reject:E0001:parse
struct Copy p { v: i64 }

fn example_value() -> i64 {
    let q: p = p { v: 42 };
    return q.v;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=uppercase-struct-literal exit=42
struct Copy P { v: i64 }

fn example_value() -> i64 {
    let q: P = P { v: 42 };
    return q.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=range-pattern-removed code=E0001 stage=parse pair=balanced-parenthesis pending=R-0555 now=accept:42
fn main() -> u8 { let n: u8 = 3; match n { 1..=9 => { return 42; }, _ => { return 0; } } }
```

<a id="SYN-2"></a>
#### SYN-2. Operators group by a fixed precedence

**SYN-2@1.** Binary operators group left to right at the precedence levels the grammar lists before `expr`, from `||` (lowest) through `&&`, equality, comparison, `|`, `^`, `&`, shifts, `+ -` and `* / %` to `as`, and postfix operations bind tighter than prefix ones, which bind tighter than `as`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseExprPrec
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Bitwise operators bind tighter than comparisons, so `x & mask == 0` is
`(x & mask) == 0`, which is what C programmers mean and what C does not do. Postfix binds
tighter than prefix, so `-c.n` is `-(c.n)` (a workload bug once parsed it the other way), and a
prefix operator binds tighter than a cast, so `-x as i32` is `(-x) as i32`.

```con accept name=precedence-levels exit=42
struct Copy C { n: i64 }

fn example_value() -> i64 {
    let c: C = C { n: 2 };
    let a: i64 = -c.n * 3 + 50;
    let s: i64 = 1 << 2 + 1;
    let masked: bool = 6 & 1 == 0;
    if masked {
        return a + s - 10;
    }
    return 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="SYN-3"></a>
#### SYN-3. Compiler spellings are reserved

**SYN-3@1.** A `let` does not bind a name that begins with `__destr_`, the prefix of the bindings the compiler introduces when it lowers a struct destructuring.

- Kind: refusal
- Uses: none
- Codes: E0283 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The post-specialization reference check (REF-6) exempts the compiler's
destructuring temporaries by their prefix, so a source binding with that spelling would inherit
the exemption. Function names the compiler reserves (`__` names, `user_main` and the builtins)
are refused by MOD-3.

```con reject name=compiler-spelling code=E0283 stage=check pair=ordinary-spelling
fn example_value() -> i64 {
    let __destr_x: i64 = 42;
    return __destr_x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=ordinary-spelling exit=42
fn example_value() -> i64 {
    let destr_x: i64 = 42;
    return destr_x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-4"></a>
#### SYN-4. A struct literal in a condition head is parenthesized

**SYN-4@2.** A struct literal in the head of an `if`, `while` or `match` is refused unless enclosed in parentheses or brackets, including inside call arguments, and a `{` outside those delimiters ends the head and opens its block regardless of the preceding name.

- Kind: refusal
- Uses: none
- Codes: E0001 (parse)
- Status: pending R-0555
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD16 and OD34 fix head disambiguation without consulting name resolution; the legacy parser remains pending (bug 318).

```con reject name=bare-struct-literal-in-head code=E0001 stage=parse pair=parenthesized-struct-literal-in-head pending=bug-318 now=accept:42
struct Copy P { v: i64 }

fn main() -> u8 {
    if P { v: 1 }.v == 1 {
        return 42;
    }
    return 0;
}
```

```con accept name=parenthesized-struct-literal-in-head exit=42
struct Copy P { v: i64 }
fn one(p: P) -> bool { return p.v == 1; }
fn main() -> u8 { if one(P { v: 1 }) { return 42; } return 0; }
```

```con accept name=head-constant-unparenthesized exit=42 pending=bug-318 now=reject:E0001:parse
const LIMIT: i64 = 3;

fn example_value() -> i64 {
    let mut n: i64 = 0;
    while n < LIMIT {
        n = n + 1;
    }
    return n + 39;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="SYN-5"></a>
#### SYN-5. Source text and string literals are UTF-8

**SYN-5@3.** A source file is UTF-8 text, a string literal denotes the UTF-8 encoding of its characters, and its only escapes are `\n`, `\t`, `\r`, `\\`, `\"` and `\0`, so a file that is not valid UTF-8 and any other `\` sequence in a literal are not accepted, and every `String<A>` reachable by safe code holds valid UTF-8.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.lexStringLoop
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** One encoding for source and literals means a literal's bytes are known without
knowing the editor or the platform. Unknown escapes were once read as the character itself; the
lexer now refuses them, with E0001 tagged SYN-1 (`unknown string escape '\q'`). A file that is not
valid UTF-8 is refused when it is read, before lexing, but today by an uncaught exception
(`Tried to read file ... containing non UTF-8 data`) rather than a diagnostic. There is no `\xHH`
or `\u{...}` escape. OD127 guarantees valid UTF-8 in every `String<A>` reachable by safe code; safe constructors validate, and unchecked construction requires a trusted validity premise.






Static-text type and borrowing are [undecided: see the target Core proposal](#core-static-text); the legacy literal-library examples are retired.

<a id="SYN-6"></a>
#### SYN-6. Tokens

**SYN-6@3.** A token is an identifier (a letter or `_`, then letters, digits and `_`), an integer literal in decimal or with a `0x`, `0b` or `0o` prefix, a float literal written as digits, `.` and digits, a string literal, a char literal (one ASCII character, or one of the escapes `\n`, `\t`, `\r`, `\0`, `\\` and `\'`, between single quotes), a label (`'` then an identifier), a keyword, or an operator or punctuation mark, and `//` begins a comment that ends at the end of its line.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.tokenize
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD137 limits character literals to ASCII; char values remain bytes (TYPE-20).

```con accept name=token-forms exit=42
// a line comment
fn example_value() -> i64 {
    let a: i64 = 0o17;
    let b: i64 = 0x1F;
    let c: i64 = 0b101;
    let q: char = '\'';
    let f: f64 = 1.5;
    let mut n: i64 = 0;
    'outer: while n < 1 {
        n = n + 1;
        break 'outer;
    }
    discard(q); discard(f);
    return a + b + c + n - 10;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-7"></a>
#### SYN-7. `>>` closes two type-argument lists

**SYN-7@1.** Where a `>` closes a type-argument or type-parameter list, a `>>` token closes two nested lists, read as two `>`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.expect
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The lexer reads `>>` as the shift operator; the grammar's `type_args` and
`type_params` close with `>`, and the parser splits the token there, so `Option<Option<i64>>`
needs no space.

```con accept name=nested-type-arguments exit=42
fn example_value() -> i64 {
    let o: Option<Option<i64>> = Option::<Option<i64>>::Some { value: Option::<i64>::Some { value: 42 } };
    match o {
        Option::Some { value: inner } => {
            match inner {
                Option::Some { value } => { return value; },
                Option::None => { return 1; }
            }
        },
        Option::None => { return 2; }
    }
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-8"></a>
#### SYN-8. A `match` that begins a statement is a statement

**SYN-8@1.** A `match` that begins a statement is the whole statement, so no operator or postfix operation continues after its closing brace, while a `match` in a value position (the right-hand side of a `let` or an assignment, or a block's trailing value) is an expression.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseMatchStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The grammar's `stmt` production has a `match` alternative; the value form is
described in the comment before `primary_expr`, because the dispatch is by position, which the
LL(1) checkers cannot express. `match x { ... } + 2;` as a statement is a parse error (SYN-1).
STMT-8 says which expression of a value block is its value.

```con accept name=statement-and-value-match exit=42
fn example_value() -> i64 {
    let x: i64 = 1;
    let mut r: i64 = 0;
    match x {
        1 => { r = 40; },
        _ => { r = 0; }
    }
    let y: i64 = match x { _ => 2 } + r;
    return y;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-9"></a>
#### SYN-9. A value `if` has an `else`

**SYN-9@1.** An `if` in a value position has an `else` branch.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parsePrimary
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Without an `else` the `if` would have no value when its condition is false. The
grammar states this in the comment before `primary_expr`; `let x: i64 = if c { 42 };` is a parse
error (SYN-1).

```con accept name=value-if-with-else exit=42
fn example_value() -> i64 {
    let c: bool = true;
    let x: i64 = if c { 42 } else { 0 };
    return x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="SYN-10"></a>
#### SYN-10. The separators `.` and `::`

**SYN-10@1.** `.` selects a field or calls a method, `::` qualifies a member of a module, a variant of an enum or an associated function of a type and introduces explicit type arguments, and an import path separates its modules with `.`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parsePrimary, Concrete.parsePostfix
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** One qualification separator keeps the type or module visible at every use
(SYNTAX_FREEZE_REVIEW §1.1, archived). The older `E#V` variant spelling is a parse error in the current compiler
(SYN-1; the grammar's productions still spell it, which OPEN-77 resyncs), and a variant is always
named through its enum: a bare `Ok(x)` is an unknown function (E0101) and `Option.None` an unknown
variable (E0100), both under MOD-1.

The recorded answers in [OD139](#od139), [OD151](#od151), [OD152](#od152) govern this subject; the frozen compiler’s earlier behavior is not an alternative rule.

```con accept name=separators exit=42
// spec-file: main.con
mod m;
import m.{base};
struct Copy P { v: i64 }

impl P {
    fn make() -> P { return P { v: 1 }; }
    fn get(&self) -> i64 { return self.v; }
}

fn example_value() -> i64 {
    let p: P = P::make();
    return m::base() + (&p).get() + p.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}

// spec-file: m.con
pub fn base() -> i64 { return 40; }

```

### Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- SYN-6@2 (spec audits 6 and 10, 2026-10-30) adds the char-literal escapes to the sentence; the compiler accepted them before.

- SYN-1@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; SYN-1@1 is superseded by version.
- SYN-4@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; SYN-4@1 is superseded by version.
- SYN-6@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; SYN-6@2 is superseded by version.

- SYN-5@2 (2026-11-03) supersedes SYN-5@1 for decided-language conformance.


<a id="ch-modules"></a>
## Modules and Names

A program is a tree of modules, one source file per module, loaded by `mod name;`. Every
declaration is private to its module unless it is declared `pub`, and another module reaches it
only by importing it (`import a.{f, T as U};`). Names resolve once, before checking, to exactly
one declaration. This chapter covers name resolution (MOD-1), visibility (MOD-2), one binding
per name in a module (MOD-3), loading module files (MOD-4), the privacy of `std.libc` (MOD-6),
generated interfaces (MOD-7, pending), calls the compiler generates (MOD-8) and what an import binds
(MOD-9). There is no MOD-5: the number was never allocated, and IDs are not renumbered
([MAINTAINERS.md](MAINTAINERS.md) §2.1). Representation privacy (which fields and constructors another module
may use) is TYPE-11, and trait coherence is the traits chapter.

OD67 refuses locals that reuse a visible top-level function, import alias, constant or intrinsic name (SCOPE-1 and SCOPE-2). TIGHTENING_DECISIONS
D9 (one source file per module with `pub` as its interface, and interface artifacts the compiler
generates and checks) is decided and scheduled late; it is MOD-7, pending R-0556. The design record is
[VISIBILITY_AND_MODULE_HYGIENE.md](../archive/design-records/VISIBILITY_AND_MODULE_HYGIENE.md).

### Rules

<a id="MOD-1"></a>
#### MOD-1. Every name resolves to one declaration

**MOD-1@1.** Every name a program uses resolves to exactly one declaration visible where it is used (a local binding, a declaration of the module, an import, a type parameter or a builtin), and a name that resolves to none, a type alias that names itself, and `Self` outside an `impl` are refused.

- Kind: refusal
- Uses: none
- Codes: E0100 (resolve), E0101 (resolve), E0102 (resolve), E0103 (resolve), E0104 (resolve), E0105 (resolve), E0106 (resolve), E0107 (resolve), E0108 (resolve), E0109 (resolve), E0112 (resolve), E0125 (resolve), E0200 (check), E0201 (check), E0202 (check), E0263 (check), E0267 (check)
- Status: enforced
- Enforced by: Concrete.Pipeline.resolve
- Semantics: none
- Obligation: resolution by identity (`DefId`, `LocalId`), never by spelling (R-0492)
- Evidence: tested

**Rationale.** Resolution validates every name once, before checking. Later passes do not yet
work only with what a name denotes: Elab and CoreCheck still look some names up by spelling, and
R-0492 moves them to identities. Bugs 039 to 045 were names resolved more than once, differently; binders
are now keyed by identity (SCOPE-1, SCOPE-3), and declarations get the same treatment under R-0492.
The checker repeats some lookups (E0200 to E0202, E0263, E0267) as defense in depth.
The builtins include the enums `Option` and `Result`, which a module uses without an import; OD43 refuses user declarations that collide with these builtin type names. The types chapter's
[builtin catalog](#builtin-functions) records the current compiler's builtin functions,
their signatures, the capabilities it checks and how it handles their names.

```con reject name=undeclared-name code=E0100 stage=resolve pair=declared-name
fn example_value() -> i64 {
    return missing + 1;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=declared-name exit=42
fn example_value() -> i64 {
    let present: i64 = 41;
    return present + 1;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```



<a id="MOD-2"></a>
#### MOD-2. Only `pub` declarations cross a module boundary

**MOD-2@1.** A declaration is private to its module unless it is declared `pub`, and another module uses it only by importing it from a module that exists, where it is `pub`.

- Kind: refusal
- Uses: none
- Codes: E0110 (resolve), E0111 (resolve), E0126 (resolve), E0284 (check), E0285 (check), E0323 (check), E0418 (elab), E0419 (elab), E0423 (elab)
- Status: enforced
- Enforced by: Concrete.Pipeline.resolve, Concrete.importRefusal
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Private by default makes every public API intentional: a missing `pub` cannot
export a helper by accident. `pub` on a type exports its name, not its representation (TYPE-11).
There is one public level today; a package-internal `pub(pkg)` is designed for when packages
depend on packages. Resolve, Check and Elab each verify imports (E0111, E0285, E0419).

**Codes by case.** Each of the three refusals has one code per pass that can report it, Resolve
first and Check or Elab when they meet the import again (`importCode` and `unimportableCode` in
`Concrete/Resolve/FileSummary.lean`): an unknown module is E0110 (resolve), E0284 (check) or E0418
(elab); a declaration that is not `pub` is E0111, E0285 or E0419; and an import naming a public
trait, which no import binds, is E0126, E0323 or E0423. On a single file the import refusals are
reported at resolve (E0110, E0111, E0126); a qualified call to a private function there is E0101
(unknown function).

Visibility has two levels, `pub` and private, and private is the default, so a module's public
surface is explicit and nothing is exported by accident; a `pub(pkg)` level is deferred until
packages need it, and `pub(super)` or `pub(in path)` will not be added (restructure the modules
instead). There are no glob imports, permanently: every imported name is listed, so each module
begins with a manifest of what it uses and a name's origin is always visible (STDLIB design
principle 8). A module's non-public items are not in its `FileSummary.publicNames` and cannot be
named in an import list (E0111). Capabilities are not hidden by a module boundary: calling a
function needs the capabilities in its signature (TRUST-4), so a module cannot smuggle a `File`
call behind an `Alloc` signature, and each public function declares only what it needs; std contains
`Unsafe` behind `trusted` functions that expose domain capabilities (operations need their declared resource and permission).
`--report caps`, `--report authority` and `--report interface` show these surfaces, and a package
policy (`[policy] deny = ["Unsafe"]`) can refuse capabilities. The visibility design and its
pre-package hygiene checklist are the archived
[VISIBILITY_AND_MODULE_HYGIENE.md](../archive/design-records/VISIBILITY_AND_MODULE_HYGIENE.md)
(several of its first-release choices, such as all fields and variants visible, were since replaced
by TYPE-11 and R-0011).

An import binds a name in the importing module only and does not make it `pub` there, so a module
cannot re-export what it imports, and `pub import` is a parse error (SYN-1).

```con reject name=import-private code=E0111 stage=resolve pair=import-public
// spec-file: main.con
mod a;
import a.{hidden};
fn example_value() -> i64 {
    return hidden();
}
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: a.con
fn hidden() -> i64 {
    return 1;
}

```

```con reject name=submodule-imports-private code=E0285 stage=check pair=import-public
// spec-file: main.con
mod b;
mod a;
fn example_value() -> i64 {
    return b::g();
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: a.con
fn secret() -> i64 {
    return 42;
}

// spec-file: b.con
import a.{secret};
pub fn g() -> i64 {
    return secret();
}

```

```con reject name=reexport code=E0285 stage=check pair=import-public
// spec-file: main.con
mod c;
mod b;
mod a;
import c.{g};
fn example_value() -> i64 {
    return g();
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: a.con
pub fn f() -> i64 {
    return 42;
}

// spec-file: b.con
import a.{f};

// spec-file: c.con
import b.{f};
pub fn g() -> i64 {
    return f();
}

```

```con accept name=import-public exit=42
// spec-file: main.con
mod a;
import a.{shown};
fn example_value() -> i64 {
    return shown();
}
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}

// spec-file: a.con
pub fn shown() -> i64 {
    return 42;
}

```

<a id="MOD-3"></a>
#### MOD-3. A module binds each name once

**MOD-3@3.** A module binds each name to at most one declaration, whether by a declaration or an import, an import alias does not name a compiler intrinsic, and no function definition takes a builtin name or another name the compiler reserves, while an impl method does not bind a free-function name `Type_method`.

- Kind: refusal
- Uses: none
- Codes: E0299 (check), E0120 (resolve), E0578 (check, core-check)
- Status: pending R-0555
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A name bound twice in a module made the meaning of a call depend on which binding a
lookup met first (bug 082). Renaming on import (`import a.{f as g}`) resolves a clash. Importing
the same declaration twice binds the name to one declaration and is accepted. The reserved names
are the `__` prefix, `user_main` and the builtins a program may call but not redefine; they are
reserved for functions only, and a `let` spelling the compiler reserves is SYN-3. The
[builtin catalog](#builtin-functions) records which builtin names the current compiler
reserves. OD133 reserves `print`, `println` and `append`; OD153 refuses program functions named like other builtins. OD135 stops entering methods under `Type_method`, so a method does not occupy that free-function name.

```con accept name=method-name-independent-decided exit=7 pending=R-0555 now=reject:E0299:check
struct Copy P { x: i64 }
impl P {
    fn get(&self) -> i64 {
        return self.x;
    }
}
fn P_get(a: i64) -> i64 {
    return a;
}
fn example_value() -> i64 {
    let p: P = P { x: 3 };
    return (&p).get() + P_get(4);
}
fn main() -> u8 {
    if example_value() == 7 {
        return 7;
    }
    return 1;
}
```

```con accept name=method-beside-free-function exit=7
struct Copy P { x: i64 }
impl P {
    fn get(&self) -> i64 {
        return self.x;
    }
}
fn get_p(a: i64) -> i64 {
    return a;
}
fn example_value() -> i64 {
    let p: P = P { x: 3 };
    return (&p).get() + get_p(4);
}
fn main() -> u8 {
    if example_value() == 7 {
        return 7;
    }
    return 1;
}
```

```con reject name=import-clashes-with-local-decided code=E0299 stage=check pair=renamed-import pending=R-0555 now=reject:E0299:check
// spec-file: main.con
mod a;
import a.{f};
fn f() -> i64 {
    return 2;
}
fn example_value() -> i64 {
    return f();
}
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: a.con
pub fn f() -> i64 {
    return 40;
}

```

```con accept name=renamed-import exit=42
// spec-file: main.con
mod a;
import a.{f as g};
fn f() -> i64 {
    return 2;
}
fn example_value() -> i64 {
    return g() + f();
}
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}

// spec-file: a.con
pub fn f() -> i64 {
    return 40;
}

```

<a id="MOD-4"></a>
#### MOD-4. A `mod` declaration loads one module file

**MOD-4@2.** A `mod X;` declaration loads exactly one file `X.con` beside the declaring file, and inline modules, the `X/mod.con` fallback, a missing file and a file already loaded by another declaration are refused.

- Kind: refusal
- Uses: none
- Codes: new (parse), E0122 (resolve), E0123 (resolve), E0124 (resolve)
- Status: pending R-0555
- Enforced by: Concrete.Pipeline.resolveFiles
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD9, OD33, OD61 and OD142 give a single source file per module, only `X.con`, without inline modules or a directory fallback.

```con reject name=missing-module-file-decided code=E0122 stage=resolve pair=module-file pending=R-0555 now=reject:E0122:resolve
mod missing;

fn main() -> u8 {
    return 42;
}
```

```con reject name=inline-module-removed code=new stage=parse pair=module-file pending=R-0555 now=accept:42
mod present { pub fn answer() -> u8 { return 42; } }
fn main() -> u8 { return present::answer(); }
```

```con accept name=module-file exit=42
// spec-file: main.con
mod present;
import present.{answer};
fn example_value() -> i64 {
    return present::answer();
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}

// spec-file: present.con
pub fn answer() -> i64 {
    return 42;
}

```

<a id="MOD-6"></a>
#### MOD-6. The bindings of `std.libc` are private to std

**MOD-6@1.** The `extern` bindings of `std.libc` are imported only by modules of std, so no module of a program names them.

- Kind: refusal
- Uses: none
- Codes: E0326 (resolve, check)
- Status: enforced
- Enforced by: Concrete.importRefusal
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD20). std reaches the C library through
`std.libc`, whose `extern` declarations are not `pub`; std's own modules import them, and a program
uses std's wrappers instead. Until bug 226's fix the import rule of a submodule admitted a private
declaration, which is how std's modules reached `std.libc` and also how a program's submodule
could; one import rule (`importRefusal`) now gives MOD-2 to every module and keeps `std.libc`
visible to std alone, `pub` or not. A module belongs to std by its definition path (`std` and the
modules under it).

```con reject name=program-imports-libc code=E0326 stage=check pair=program-uses-std project=libcuser interp-exempt=project-needs-std
// spec-file: main.con
mod b;
fn example_value() -> i64 {
    return b::g();
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: b.con
import std.libc.{strlen};
pub fn g() -> i64 {
    return 42;
}

```

```con reject name=program-root-imports-libc code=E0326 stage=resolve pair=program-uses-std project=libcroot interp-exempt=project-needs-std
import std.libc.{strlen};

fn main() -> u8 {
    return 42;
}
```

```con accept name=program-uses-std exit=42 project=stduser interp-exempt=project-needs-std
import std.string.{String};

fn main() -> u8 {
    return 42;
}
```

<a id="MOD-7"></a>
#### MOD-7. A module's interface is its `pub` declarations, generated

**MOD-7@1.** A module is written in one source file with no handwritten interface file, its `pub` declarations are its interface, and the compiler generates the module's interface artifact, which is compared between commits so that every change to a module's public API is reviewed as an interface change.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0556
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD9). The generator of interface artifacts and
the gate that diffs them between commits are R-0556, built on the interface data of R-0492 and
R-0497; separate compilation (V6) reads the same artifacts. `mod X;` loads one module per file
(MOD-4). OD33 and OD61 refuse inline modules; one source file declares one module.

<a id="MOD-8"></a>
#### MOD-8. A generated call is bound by identity

**MOD-8@1.** A call the compiler generates, such as the runtime helpers `print`, `println` and `append` desugar into, is bound to its helper by identity and never to a declaration of the program that has the same name.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Symbols.runtimeFn
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** MOD-1 resolves the names a program writes; a generated call has no written name to resolve, so
its target is fixed when it is generated. The example uses checked integer addition, whose generated overflow-check calls retain their compiler identities.

```con accept name=generated-arithmetic-call exit=42
fn add(a: u8, b: u8) -> u8 { return a + b; }
fn main() -> u8 { return add(40, 2); }
```

<a id="MOD-9"></a>
#### MOD-9. An import binds the declaration it names

**MOD-9@2.** Importing a `pub` function, foreign binding, struct, enum, trait, type alias or constant binds that declaration itself, and an import alias of a struct or enum names the same type, with its methods and impls, in every pass.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.resolveImports
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A public trait is explicitly exported and imported like other declarations (owner, 2026-11-02); aliases preserve declaration identity, and removed newtypes introduce none.

```con accept name=import-constant-and-alias exit=42
// spec-file: main.con
mod user;
mod shapes;
import user.{run};
pub fn main() -> u8 { if run() == 42 { return 42; } return 1; }

// spec-file: shapes.con
pub const BASE: i64 = 38;
pub struct Copy P { pub v: i64 }
impl P {
    pub fn mk(n: i64) -> P { return P { v: n }; }
}

// spec-file: user.con
import shapes.{P as Q, BASE};
pub fn run() -> i64 {
    let a: Q = Q::mk(4);
    return a.v + BASE;
}

```


### Version history

This appendix records superseded versions and is not normative.

- MOD-4@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; MOD-4@1 is superseded by version.
- MOD-9@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; MOD-9@1 is superseded by version.


<a id="ch-scope"></a>
## Scope and Binding

A name inside a function body denotes at most one visible local value binding at every program
point (INV-SCOPE). A rule keyed by a name is therefore keyed by a binding. The reference rules
([references.md](#ch-references)) rely on this when they say that a reference's root "stays the
same binding". The design record is [MEMORY_SEMANTICS §15.4](../archive/design-records/MEMORY_SEMANTICS.md).

OD67 refuses a local that reuses a top-level declaration or import name; field labels remain distinct.

### Definitions

<a id="SCOPE-D1"></a>
#### SCOPE-D1. Visible

**SCOPE-D1@3.** A local binding is visible from its declaration to the end of its declaring block, nested blocks included regardless of ownership state, with parameters visible throughout the body, pattern binders in their arm, a counted loop index only in its body and a borrow-block binder only in its block, without a named region.

- Kind: definition
- Uses: REF-D3
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.scope1Diagnostics
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** Visibility is lexical and independent of ownership state; OD47 scopes a counted index to the body and OD57 removes named regions.

### Rules

<a id="SCOPE-1"></a>
#### SCOPE-1. One lexical name, one binding

**SCOPE-1@3.** No binder reuses the spelling of a visible local binding, top-level declaration or import, and a contract expression binds no name at all.

- Kind: refusal
- Uses: SCOPE-D1
- Codes: E0301 (check), E0305 (check), E0292 (check), E0214 (check), E0215 (check)
- Status: pending R-0555
- Enforced by: Concrete.scope1Diagnostics, Concrete.constInitDiagnostics
- Semantics: none
- Obligation: INV-SCOPE (R-0500)
- Evidence: tested

**Rationale.** Visible names are not rebound, including top-level declarations and imports (OD67); siblings may reuse names, and field labels remain separate (SCOPE-2). Contracts introduce no runtime binders.

```con reject name=block-let-decided code=E0301 stage=check pair=block-let-renamed pending=R-0555 now=reject:E0301:check
fn example_value() -> i64 {
    let x: i64 = 1;
    if true {
        let x: i64 = 100;
        if x != 100 { return 0; }
    }
    return x + 41;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=block-let-renamed exit=42
fn example_value() -> i64 {
    let x: i64 = 1;
    if true {
        let inner_x: i64 = 100;
        if inner_x != 100 { return 0; }
    }
    return x + 41;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_2i_scope1_const_initializer.con` | `reject:E0301:SCOPE-1@3` | `const-init` | `-` |
| `tests/programs/regress_2i_const_initializer_block.con` | `run:42` | `const-init` | `binary,interp-exempt:bug-117-the-interpreter-has-no-module-constants` |
| `tests/programs/error_2i_ref6_contract_ref_binding.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_scope1_contract_let.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_invariant_binds.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_variant_binds_ref.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_inferred_ref_alias.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_wrapped_binder.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_trait_impl_invariant.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_place_assignment.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_borrow_to_generic.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_generic_call.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_imported_generic.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_scope1_extern_duplicate_params.con` | `reject:E0301:SCOPE-1@3` | `scope1-duplicate_params` | `-` |
| `tests/programs/error_2i_scope1_trait_decl_duplicate_params.con` | `reject:E0301:SCOPE-1@3` | `scope1-duplicate_params` | `-` |
| `tests/programs/regress_2i_contract_aliased_ref_param.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/regress_2i_contract_ref_receiver.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/regress_2i_contract_static_nongeneric_method.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_spec_body_hidden_binder.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_spec_body_generic_call.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/regress_2i_contract_nongeneric_call_beside_generic_method.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_contract_generic_method_mangled.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_generic_method_static.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/regress_2i_contract_paren_ref_arg.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_scope1_contracted_fn_shadow.con` | `reject:E0301:SCOPE-1@3` | `scope1-param_in_block` | `-` |
| `tests/programs/error_2i_scope1_spec_fn_duplicate_params.con` | `reject:E0301:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/regress_2i_contract_and_spec_params.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_scope1_same_block_let.con` | `reject:E0301:SCOPE-1@3` | `scope1-same_block_let` | `-` |
| `tests/programs/regress_2i_scope1_same_block_let_renamed.con` | `run:42` | `scope1-same_block_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_block_let.con` | `reject:E0301:SCOPE-1@3` | `scope1-block_let` | `-` |
| `tests/programs/regress_2i_scope1_block_let_renamed.con` | `run:42` | `scope1-block_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_param_in_block.con` | `reject:E0301:SCOPE-1@3` | `scope1-param_in_block` | `-` |
| `tests/programs/regress_2i_scope1_param_in_block_renamed.con` | `run:42` | `scope1-param_in_block` | `binary,interp` |
| `tests/programs/error_2i_scope1_consuming_rebind.con` | `reject:E0301:SCOPE-1@3` | `scope1-consuming_rebind` | `-` |
| `tests/programs/regress_2i_scope1_consuming_rebind_renamed.con` | `run:42` | `scope1-consuming_rebind` | `binary,interp` |
| `tests/programs/error_2i_scope1_for_header.con` | `reject:E0301:SCOPE-1@3` | `scope1-for_header` | `-` |
| `tests/programs/regress_2i_scope1_for_header_renamed.con` | `run:42` | `scope1-for_header` | `binary,interp` |
| `tests/programs/error_2i_scope1_arm_binder.con` | `reject:E0301:SCOPE-1@3` | `scope1-arm_binder` | `-` |
| `tests/programs/regress_2i_scope1_arm_binder_renamed.con` | `run:42` | `scope1-arm_binder` | `binary,interp` |
| `tests/programs/error_2i_scope1_if_let.con` | `reject:E0301:SCOPE-1@3` | `scope1-if_let` | `-` |
| `tests/programs/regress_2i_scope1_if_let_renamed.con` | `run:42` | `scope1-if_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_while_let.con` | `reject:E0301:SCOPE-1@3` | `scope1-while_let` | `-` |
| `tests/programs/regress_2i_scope1_while_let_renamed.con` | `run:42` | `scope1-while_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_destructure.con` | `reject:E0301:SCOPE-1@3` | `scope1-destructure` | `-` |
| `tests/programs/regress_2i_scope1_destructure_renamed.con` | `run:42` | `scope1-destructure` | `binary,interp` |
| `tests/programs/error_2i_scope1_duplicate_params.con` | `reject:E0301:SCOPE-1@3` | `scope1-duplicate_params` | `-` |
| `tests/programs/regress_2i_scope1_duplicate_params_renamed.con` | `run:42` | `scope1-duplicate_params` | `binary,interp` |
| `tests/programs/error_2i_scope1_borrow_ref.con` | `reject:E0301:SCOPE-1@3` | `scope1-borrow_ref` | `-` |
| `tests/programs/regress_2i_scope1_borrow_ref_renamed.con` | `run:42` | `scope1-borrow_ref` | `binary,interp` |
| `tests/programs/error_2i_scope1_region_name.con` | `reject:E0301:SCOPE-1@3` | `scope1-region_name` | `-` |
| `tests/programs/regress_2i_scope1_region_name_renamed.con` | `run:42` | `scope1-region_name` | `binary,interp` |
| `tests/programs/error_borrow_shadow.con` | `reject:E0301:SCOPE-1@3` | `scope1-borrow_ref` | `-` |
| `tests/programs/error_093_shadowing_block_local_leak.con` | `reject:E0301:SCOPE-1@3` | `scope1-block_let` | `-` |
| `tests/programs/error_093_shadowing_match_binder_leak.con` | `reject:E0301:SCOPE-1@3` | `scope1-arm_binder` | `-` |
| `tests/programs/error_094_arm_binder_shadows_live.con` | `reject:E0301:SCOPE-1@3` | `scope1-arm_binder` | `-` |
| `tests/programs/error_094_destructure_shadows_live.con` | `reject:E0301:SCOPE-1@3` | `scope1-destructure` | `-` |
| `tests/programs/error_094_value_binder_shadows_live.con` | `reject:E0301:SCOPE-1@3` | `scope1-arm_binder` | `-` |
| `tests/programs/error_094_borrow_block_shadow_leak.con` | `reject:E0301:SCOPE-1@3` | `scope1-block_let` | `-` |
| `tests/programs/error_094_mut_ref_param_shadow_leak.con` | `reject:E0301:SCOPE-1@3` | `scope1-param_in_block` | `-` |
| `tests/programs/error_107_shadow_under_live_ref.con` | `reject:E0301:SCOPE-1@3` | `scope1-same_block_let` | `-` |
| `tests/programs/error_107_shadow_in_block_under_live_ref.con` | `reject:E0301:SCOPE-1@3` | `scope1-block_let` | `-` |
| `tests/programs/regress_093_shadowing_locals_consumed.con` | `reject:E0301:SCOPE-1@3` | `scope1-block_let` | `-` |
| `tests/programs/regress_094_shadowing_binders.con` | `reject:E0301:SCOPE-1@3` | `scope1-arm_binder` | `-` |
| `tests/programs/regress_094_shadow_state_by_identity.con` | `reject:E0301:SCOPE-1@3` | `scope1-same_block_let` | `-` |
| `tests/programs/regress_045_match_binder_shadow.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_087_let_block_shadow.con` | `reject:E0301:SCOPE-1@3` | `scope3-087` | `-` |
| `tests/programs/regress_087_borrow_shadow_uaf/` | `reject:E0301:SCOPE-1@3` | `scope3-087-borrow` | `-` |

<a id="SCOPE-2"></a>
#### SCOPE-2. What is not a collision

**SCOPE-2@3.** Bindings in sibling scopes, one declaration in a loop body executed many times, an assignment and a field label are not collisions under SCOPE-1, while a local may not reuse a visible top-level declaration or import name and types and values share one module namespace.

- Kind: guarantee
- Uses: SCOPE-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.scope1Diagnostics
- Semantics: none
- Obligation: INV-SCOPE (R-0500)
- Evidence: tested

**Rationale.** OD43 completes one module namespace; OD67 refuses a local that reuses a top-level name. Sibling lexical scopes and field labels do not introduce two visible bindings.

```con accept name=sibling-branches exit=42
fn pick(c: bool) -> i64 {
    let mut out: i64 = 0;
    if c {
        let x: i64 = 40;
        out = x;
    } else {
        let x: i64 = 2;
        out = x;
    }
    return out;
}

fn example_value() -> i64 {
    return pick(true) + pick(false);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=loop-body-let exit=42
fn example_value() -> i64 {
    let mut total: i64 = 0;
    let mut k: i64 = 0;
    while k < 6 {
        let step: i64 = 7;
        total = total + step;
        k = k + 1;
    }
    return total;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_2i_scope2_sibling_branches.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_sibling_arms.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_consecutive_loops.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_loop_body_let.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_assign_rebind.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_borrow_after_loop.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_087_sibling_promoted.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_087_unpromoted_sibling.con` | `run:42` | `-` | `binary,interp` |

<a id="SCOPE-3"></a>
#### SCOPE-3. Blocks are lexical on every consumer

**SCOPE-3@3.** A binding declared in a block is not visible after the block, and every consumer (the checker, elaboration, the interpreter, lowering, extraction and evidence frames) resolves a name to the innermost visible binding.

- Kind: guarantee
- Uses: SCOPE-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.scope1Diagnostics, Concrete.bindLocal, Concrete.enterBlock
- Semantics: none
- Obligation: INV-SCOPE (R-0500); every consumer agrees (PASS-1)
- Evidence: tested

**Rationale.** SCOPE-1 lets a name be reused only in a sibling scope. Every consumer must still
mean the lexical binding when that happens. Before exit 2i, shadowing inside a block, loop body
or borrow block miscompiled (bug 087): `let x: i64 = 1; if true { let x: i64 = 100; } return x
+ 41;` exited 141. Elaboration's alpha-renaming of block locals (`bindLocal`, `enterBlock`)
stays as defense in depth. One conformance row also checks the extracted model.

```con accept name=sibling-reuse-after-block exit=42
fn example_value() -> i64 {
    let mut out: i64 = 0;
    if true {
        let x: i64 = 100;
        out = out + x * 0;
    }
    if true {
        let x: i64 = 42;
        out = out + x;
    }
    return out;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_2i_scope1_087_renamed.con` | `run:42` | `scope3-087` | `binary,interp,model:087-renamed` |
| `tests/programs/regress_2i_scope1_087_borrow_renamed/` | `run:0` | `scope3-087-borrow` | `binary,interp-exempt:project-fixture-(the-interpreter-runs-single-files)` |

### Version history

This appendix records superseded versions and is not normative.

- SCOPE-D1@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; SCOPE-D1@1 is superseded by version.
- SCOPE-1@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; SCOPE-1@1 is superseded by version.
- SCOPE-2@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; SCOPE-2@1 is superseded by version.
- SCOPE-3@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; SCOPE-3@1 is superseded by version.


<a id="ch-types"></a>
## Types

Every expression has one type, and there are no implicit conversions. This chapter covers
callable values and dispatch (TYPE-1, TYPE-2), `Text` (TYPE-3), the typing of expressions and
operators (TYPE-4 to TYPE-10), construction rights (TYPE-11), type aliases (TYPE-12) and the
kinds of type parameters (TYPE-13, TYPE-14), generic arguments (TYPE-15), finite size (TYPE-16),
capability sets in types (TYPE-D1, TYPE-17, TYPE-18), the primitive numeric types (TYPE-19), `char`
(TYPE-20), the refusal of newtypes (TYPE-21), the builtin `Option` and `Result` (TYPE-23) and
the refusal of newtype conversions (TYPE-24). Copy itself is defined in the ownership chapter
(OWN-D1, OWN-14). What a capability means and when a function may use one are in the trusted
chapter (TRUST-D1, TRUST-4). The section "Builtin functions" below lists every builtin function
with its current signature.

The design records are [VALIDATED_WRAPPERS.md](../stdlib/VALIDATED_WRAPPERS.md) (a library pattern built on
TYPE-11, not a language rule), [VALUE_MODEL.md](../archive/design-records/VALUE_MODEL.md) (absorbed
into TYPE-21, FFI-7 and FFI-8 on 2026-10-30) and
[CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md).
Const generics are not part of the language. Their design is decided, and the build is
deferred until a workload needs it (see "No const generics" below).
Layout attributes (`#[repr(C)]`, `packed`, `align`) belong to the FFI chapter.

### Definitions

<a id="TYPE-D1"></a>
#### TYPE-D1. Capability set

**TYPE-D1@2.** A capability set is a set of capabilities (TRUST-D1) and capability variables, written as a comma-separated list in `with(...)` and as `{}` or names joined by `+` in a capability argument, and a function or a struct declares a capability variable as a parameter `<cap C>`.

- Kind: definition
- Uses: TRUST-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.CapSet
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** Capability sets are extensional and duplicate members are refused (OD108); capability aliases, including `Std`, are removed, and set variables are substituted exactly rather than unioned on inconsistency (authority rules 3–4). Ambiguous inference is [undecided: see the target Core proposal](#core-capability-inference).

### Rules

<a id="TYPE-1"></a>
#### TYPE-1. A callable value is a function pointer with explicit context

**TYPE-1@1.** A value of function type is a pointer to a named function, and any context the callee needs is passed to it as an explicit argument, so the language has no closures.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Ty
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A closure captures part of its environment. Capturing a reference would store it,
which REF-3 forbids, and capturing an owned value would need its own ownership and conversion
rules. A function pointer plus an explicit context argument keeps both in view: the context is an
ordinary parameter, borrowed or owned under the ordinary rules, and its capabilities are visible
at the call site (TYPE-17). VERIFIED_PIPELINE's first verified profile excludes closures for the
same reason. The type grammar (`Ty`) has function-pointer types and no closure type, and a nested
`fn` declaration is a parse error: it would add locality but no expressive power, and invite the
expectation that it captures. A function pointer is Copy (OWN-D1); its context is borrowed shared,
lent `&mut` for each call (REF-10) or moved into a single call (OWN-1). A callback cannot reach a
container its caller has borrowed for the call: a context cannot hold a reference (REF-3), and one
call's borrows of a root conflict (REF-11). A first-class value pairing a context with a function
is deferred until a workload needs to store one
([CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md) §4).

```con accept name=explicit-context exit=42
struct Ctx { add: i64 }

fn apply(ctx: &Ctx, x: i64, f: fn(&Ctx, i64) -> i64) -> i64 {
    return f(ctx, x);
}

fn plus(c: &Ctx, x: i64) -> i64 {
    return c.add + x;
}

fn example_value() -> i64 {
    let c: Ctx = Ctx { add: 40 };
    let r: i64 = apply(&c, 2, plus);
    let Ctx { add } = c;
    return r + add * 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-2"></a>
#### TYPE-2. No dynamic dispatch

**TYPE-2@1.** There are no trait-object types, so every method call through a trait bound resolves to one implementation at compile time, by monomorphization.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Ty, Concrete.monoProgram
- Semantics: none
- Obligation: the substitution and dispatch theorem of S8a (R-0510)
- Evidence: tested

**Rationale.** A call through a trait bound is specialized at each instantiation, so the code that
runs is known statically. Dynamic dispatch would add a vtable layout, an indirect-call rule and a
second dispatch semantics to the formal model. Indirect calls exist only through function-pointer
values (TYPE-1). The coherence rules for impls (TIGHTENING_DECISIONS D7) belong to the traits
chapter.

```con accept name=static-dispatch exit=42
trait Area {
    fn area(&self) -> i64;
}

struct Copy Sq { s: i64 }

impl Area for Sq {
    fn area(&self) -> i64 { return self.s * self.s; }
}

fn total<T: Area>(x: &T) -> i64 {
    return x.area();
}

fn example_value() -> i64 {
    let q: Sq = Sq { s: 6 };
    return total::<Sq>(&q) + 6;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-3"></a>
#### TYPE-3. `Text` is owned

**TYPE-3@2.** A `Text` owns an immutable copy of its bytes, never aliases another value's buffer, and is linear.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: std/src/text.con#Text
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Ownership of immutable text remains required, but the literal/static-text type and
its conversion to allocating strings are [undecided: see the target Core proposal](#core-static-text).
The old allocating library example is retired until it can be stated without choosing that API.

<a id="TYPE-4"></a>
#### TYPE-4. One type per expression, no implicit conversion

**TYPE-4@2.** Every expression has exactly one type, and every position a value flows into (a binding, an argument, a return, a field, a branch or arm of a value-producing `if` or `match`) expects that type exactly, with no implicit conversion, except that an expression that never completes (a diverging `return`, `break`, `continue` or call) fits any position other than a condition (TYPE-5).

- Kind: refusal
- Uses: none
- Codes: E0220 (check), E0222 (check), E0224 (check), E0225 (check), E0226 (check), E0500 (core-check), E0560 (core-check)
- Status: enforced
- Enforced by: Concrete.expectTy, Concrete.checkExprCore, Concrete.checkStmt, Concrete.ccCheckExpr, Concrete.ccCheckStmt
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** Conversions are explicit (`as`, or a named operation in the arithmetic chapter),
so the type of every value is visible in the source. An integer literal takes the type its
position expects. A literal that does not fit that type is an arithmetic rule. CoreCheck repeats
the variable and return checks on Core as defense in depth.
A `break` carries no value (STMT-6@3, OD27), so it is not one of the positions.
The current compiler still parses `break <expr>;`, evaluating and dropping the value (bug 323).

```con reject name=bool-for-int code=E0220 stage=check pair=one-type
fn example_value() -> i64 {
    let x: i64 = true;
    return x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con reject name=branches-disagree code=E0224 stage=check pair=one-type
fn example_value() -> i64 {
    let c: bool = true;
    let x: i64 = if c { 1 } else { false };
    return x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=one-type exit=42
fn example_value() -> i64 {
    let c: bool = true;
    let x: i64 = if c { 42 } else { 0 };
    return x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=capabilities-not-erased code=E0220 stage=check pair=same-capabilities
fn inc(x: i64) with(Fs) -> i64 {
    return x + 1;
}

fn example_value() with(Fs) -> i64 {
    let f: fn(i64) -> i64 = inc;
    return f(41);
}

fn main() with(Fs) -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=capability-lost-in-argument code=E0220 stage=check pair=same-capabilities
fn apply(f: fn(i64) -> i64, x: i64) -> i64 {
    return f(x);
}

fn inc(x: i64) with(Fs) -> i64 {
    return x + 1;
}

fn example_value() with(Fs) -> i64 {
    return apply(inc, 41);
}

fn main() with(Fs) -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=capability-lost-in-field code=E0220 stage=check pair=same-capabilities
struct Copy Box { f: fn(i64) -> i64 }

fn inc(x: i64) with(Fs) -> i64 {
    return x + 1;
}

fn example_value() with(Fs) -> i64 {
    let b: Box = Box { f: inc };
    let g: fn(i64) -> i64 = b.f;
    return g(41);
}

fn main() with(Fs) -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=same-capabilities exit=42
fn inc(x: i64) with(Fs) -> i64 {
    return x + 1;
}

fn example_value() with(Fs) -> i64 {
    let f: fn(i64) with(Fs) -> i64 = inc;
    return f(41);
}

fn main() with(Fs) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-5"></a>
#### TYPE-5. Conditions are `bool`

**TYPE-5@3.** The condition of an `if` or `while` and the operands of `&&`, `||` and `!` have type `bool`, and a comparison has type `bool`.

- Kind: refusal
- Uses: none
- Codes: E0223 (check), E0504 (core-check), E0505 (core-check), E0508 (core-check), E0540 (core-check), E0541 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt, Concrete.ccCheckExpr, Concrete.ccCheckStmt
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** OD55 removes guards; conditions and logical operands remain boolean.

```con reject name=int-condition-decided code=E0223 stage=check pair=bool-condition pending=R-0555 now=reject:E0223:check
fn main() -> u8 {
    let n: i64 = 1;
    if n {
        return 42;
    }
    return 0;
}
```

```con accept name=bool-condition exit=42
fn main() -> u8 {
    let n: i64 = 1;
    if n != 0 {
        return 42;
    }
    return 0;
}
```

<a id="TYPE-6"></a>
#### TYPE-6. Operators take their declared primitive types

**TYPE-6@3.** Arithmetic operators take two operands of one numeric type, bitwise and shift operators take integers, unary minus takes a number, comparisons take two operands of one numeric type or two `bool` or two `char` operands, and no operator takes a raw pointer.

- Kind: refusal
- Uses: none
- Codes: E0228 (check), E0229 (check), E0501 (core-check, post-mono), E0502 (core-check), E0503 (core-check), E0506 (core-check), E0507 (core-check), E0509 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: one `IntArith` authority for operand widths (R-0493)
- Evidence: tested

**Rationale.** OD42 admits bool and char comparisons; numeric operands still agree in type and raw-pointer operators remain refused.

```con reject name=compare-structs-decided code=E0229 stage=check pair=same-type-operands pending=R-0555 now=reject:E0229:check
struct Copy P { n: i64 }

fn main() -> u8 {
    let a: P = P { n: 1 };
    let b: P = P { n: 1 };
    if a == b {
        return 42;
    }
    return 0;
}
```

```con accept name=same-type-operands exit=42
struct Copy P { n: i64 }

fn example_value() -> i64 {
    let a: P = P { n: 21 };
    let b: P = P { n: 21 };
    if a.n == b.n {
        return a.n + b.n;
    }
    return 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=pointer-comparison code=E0229 stage=check pair=pointee-comparison pending=bug-322 now=accept:42
trusted fn same(p: *const i64, q: *const i64) -> bool {
    return p == q;
}
fn main() -> u8 {
    let x: i64 = 1;
    let p: *const i64 = &x as *const i64;
    if same(p, p) { return 42; }
    return 0;
}
```

```con accept name=pointee-comparison exit=42
trusted fn same(p: *const i64, q: *const i64) -> bool {
    return *p == *q;
}
fn main() with(Unsafe) -> u8 {
    let x: i64 = 1;
    if same(&x as *const i64, &x as *const i64) { return 42; }
    return 0;
}
```

<a id="TYPE-7"></a>
#### TYPE-7. Aggregates are built and read by their declarations

**TYPE-7@2.** A struct literal names a declared struct and gives each of its fields exactly once explicitly, with no struct update `..base`, a field access names a field of a struct, an enum literal or pattern names a variant of a declared enum, an array literal has at least one element, and only an array is indexed, by an integer.

- Kind: refusal
- Uses: none
- Codes: new (parse), E0250 (check), E0251 (check), E0252 (check), E0253 (check), E0254 (check), E0260 (check), E0261 (check), E0306 (check), E0416 (elab), E0550 (core-check), E0551 (core-check), E0552 (core-check), E0316 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt, Concrete.ccCheckExpr
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** OD53 removes functional update: every struct field is explicit. Patterns are governed by the patterns chapter and private representation by TYPE-11.

```con reject name=missing-field-decided code=E0252 stage=check pair=every-field pending=R-0555 now=reject:E0252:check
struct Copy P { a: i64, b: i64 }

fn example_value() -> i64 {
    let p: P = P { a: 42 };
    return p.a;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=unknown-field-decided code=E0253 stage=check pair=every-field pending=R-0555 now=reject:E0253:check
struct Copy P { a: i64, b: i64 }

fn example_value() -> i64 {
    let p: P = P { a: 42, b: 0, c: 1 };
    return p.a;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=duplicate-field-decided code=E0306 stage=check pair=every-field pending=R-0555 now=reject:E0306:check
struct Copy P { a: i64, b: i64 }

fn example_value() -> i64 {
    let p: P = P { a: 1, b: 2, a: 40 };
    return p.a + p.b;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```



```con accept name=every-field exit=42
struct Copy P { a: i64, b: i64 }

fn example_value() -> i64 {
    let p: P = P { a: 40, b: 2 };
    let arr: [i64; 2] = [p.a, p.b];
    return arr[0] + arr[1];
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=struct-update-removed code=new stage=parse pair=every-field pending=R-0555 now=accept:42
struct Copy P { a: u8, b: u8 }
fn main() -> u8 { let p: P = P { a: 1, b: 2 }; let q: P = P { a: 40, ..p }; return q.a + q.b; }
```

<a id="TYPE-8"></a>
#### TYPE-8. Only a reference or a raw pointer is dereferenced

**TYPE-8@1.** The operand of `*` is a reference, a raw pointer or an owned `Heap`, and an assignment through `*` needs a `&mut T` or `*mut T`.

- Kind: refusal
- Uses: none
- Codes: E0221 (check), E0554 (core-check), E0555 (core-check)
- Status: enforced
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr, Concrete.ccCheckStmt
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** A shared reference grants no write access, so writing through `&T` is refused. A
raw-pointer dereference also needs trusted or `Unsafe` code (TRUST-3). `*h` of an owned
`Heap` moves the value out of its cell (OWN-15, `deref`).

```con reject name=deref-int code=E0221 stage=check pair=deref-ref
fn example_value() -> i64 {
    let x: i64 = 42;
    return *x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=deref-ref exit=42
fn read(r: &i64) -> i64 {
    return *r;
}

fn example_value() -> i64 {
    let x: i64 = 42;
    return read(&x);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-9"></a>
#### TYPE-9. Calls match their signatures

**TYPE-9@2.** A call passes exactly as many arguments as the callee has parameters and, when it writes a type-argument list, exactly as many type arguments as the callee has type parameters, and a method call names a method of the receiver's named type or of a trait that bounds the receiver's type parameter.

- Kind: refusal
- Uses: none
- Codes: E0262 (check), E0264 (check), E0265 (check), E0266 (check), E0522 (core-check), E0313 (check), E0317 (check), E0322 (check)
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: progress and preservation for the Core type system (R-0507)
- Evidence: tested

**Rationale.** There are no default arguments, no variadic functions and no method lookup
through implicit conversions. A method of a type parameter comes only from its bounds, which is
what lets a generic body be checked once (TYPE-2, PASS-1). E0322 enforces the type-argument count (bug 221: an extra type argument was dropped silently). A body-less declaration (an `#[intrinsic]`) keeps no type-parameter
list, so the current compiler still skips the count there (bug 333), pinned by the pending example.

```con reject name=wrong-arity code=E0262 stage=check pair=right-arity
fn add(a: i64, b: i64) -> i64 {
    return a + b;
}

fn example_value() -> i64 {
    return add(42);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```



```con reject name=receiverless-trait-method code=E0317 stage=check pair=right-arity
trait Make { fn make() -> i64; }
fn call<T: Make>(x: &T) -> i64 { return x.make(); }
fn main() -> u8 { return 0; }
```

```con accept name=right-arity exit=42
fn add(a: i64, b: i64) -> i64 {
    return a + b;
}

fn example_value() -> i64 {
    return add(40, 2);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```





<a id="TYPE-10"></a>
#### TYPE-10. Builtins have fixed signatures

**TYPE-10@2.** A builtin is called with the argument count, type-argument count and argument types of its fixed signature, allocation takes an allocator and reports conserving failure, and the removed `free` and `unwrap` forms are refused.

- Kind: refusal
- Uses: none
- Codes: E0268 (check), E0269 (check), E0270 (check), E0271 (check), E0274 (check), E0421 (check, elab), E0314 (check), E0315 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Signatures and primitive effects are fixed by identity, not by declarations. MEM-5 and MEM-7 govern conserving allocation; OD59 removes `free` and OD155 removes `unwrap`. Complete failure payload contracts are [undecided: see the target Core proposal](#core-allocation-contracts).

```con reject name=wrapping-wrong-arity-decided code=E0268 stage=check pair=wrapping-valid-arguments pending=R-0555 now=reject:E0268:check
fn main() -> u8 {
    return wrapping_add(42);
}
```

```con reject name=wrapping-wrong-type-decided code=E0314 stage=check pair=wrapping-valid-arguments pending=R-0555 now=reject:E0314:check
fn main() -> u8 {
    return wrapping_add(true, false);
}
```

```con accept name=wrapping-valid-arguments exit=42
fn main() -> u8 {
    return wrapping_add(40, 2);
}
```

```con reject name=free-non-heap-decided code=E0274 stage=check pair=free-heap pending=R-0555 now=reject:E0274:check
fn main() with(Alloc) -> u8 {
    let x: i64 = 42;
    free(x);
    return 0;
}
```

```con reject name=wrapping-on-non-integers-decided code=E0314 stage=check pair=free-heap pending=R-0555 now=reject:E0314:check
fn main() -> u8 {
    let b: bool = wrapping_add(true, false);
    return 0;
}
```

```con reject name=print-without-arguments-decided code=E0315 stage=check pair=free-heap pending=R-0555 now=reject:E0315:check
fn main() with(Alloc) -> u8 {
    print();
    return 0;
}
```

```con accept name=free-heap exit=42
fn main() -> u8 { let n: u64 = sizeof::<u8>(); if n == 1 { return 42; } return 1; }
```

<a id="TYPE-11"></a>
#### TYPE-11. A module keeps its types' representation

**TYPE-11@2.** A struct field not marked `pub` is not named in a struct literal, read or written outside its declaring module, including by a parent module, and newtype declarations and conversions are refused.

- Kind: refusal
- Uses: none
- Codes: E0296 (check), E0297 (check), E0298 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD21 protects representation even from a parent module; OD54 removes newtypes rather than introducing conversion exceptions.



```con reject name=read-private-field-decided code=E0298 stage=check pair=public-constructor pending=R-0555 now=reject:E0298:check
// spec-file: main.con
mod box;
import box.{Bx, mk};
fn example_value() -> i64 {
    let b: Bx = mk();
    return b.v;
}
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: box.con
pub struct Copy Bx { v: i64 }
pub fn mk() -> Bx {
    return Bx { v: 42 };
}

```

```con reject name=destructure-private-field-decided code=E0298 stage=check pair=public-constructor pending=R-0555 now=reject:E0298:check
// spec-file: main.con
mod box;
import box.{Bx, mk};
fn example_value() -> i64 {
    let b: Bx = mk();
    let Bx { v } = b;
    return v;
}
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: box.con
pub struct Copy Bx { v: i64 }
pub fn mk() -> Bx {
    return Bx { v: 42 };
}

```





```con reject name=parent-reads-child-private-field-decided code=E0298 stage=check pair=public-constructor pending=R-0555 now=reject:E0298:check
// spec-file: main.con
mod kids;
import kids.{Box, mk};
fn example_value() -> i64 {
    let b: Box = mk();
    return b.v;
}
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: kids.con
pub struct Copy Box { v: i64 }
pub fn mk() -> Box {
    return Box { v: 42 };
}

```




```con accept name=public-constructor exit=42
// spec-file: main.con
mod box;
import box.{Bx, mk, get};
fn main() -> u8 { let b: Bx = mk(); return get(b); }
// spec-file: box.con
pub struct Copy Bx { v: u8 }
pub fn mk() -> Bx { return Bx { v: 42 }; }
pub fn get(b: Bx) -> u8 { return b.v; }
```

<a id="TYPE-12"></a>
#### TYPE-12. A type alias is another name for the same type

**TYPE-12@1.** `type A = T;` makes `A` another spelling of `T` in every position, with the same layout, extraction and proof identity, and an alias is never recursive.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.expandAliasDeep, Concrete.closeAliasMap
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** An alias is transparent, not nominal: for a distinct type with its own identity,
use a newtype. Aliases are expanded transitively when types are resolved, so every later stage
sees the underlying type. A program written with an alias and the same program written without
it have byte-identical layout and fingerprint reports. Aliases take no type parameters, but an
alias may name a generic instantiation (`type IntBox = Box<i32>;`). An unknown target is refused
at the declaration (E0108) and so is a cycle (E0112), both in the resolver.

An alias is written `type Name = <type>;` (`type Digest = [u8; 32];`, `type Point = P;`), and
`pub type Meters = i32;` is importable like any public item. Aliases compose: a chain
(`type C = B; type B = A; type A = i32`) resolves `C` to `i32`, and an alias nested in another type
(`type Arr = [E; 3]` with `type E = i32`) expands to `[i32; 3]`. The expansion is structural and
transitive (`closeAliasMap`, `expandAliasDeep`), so layout, the Copy and representation checks,
monomorphization and lowering all see the underlying type: `type Id = i32; struct Copy S { a: Id }`
is Copy. An alias never shifts a struct's size or offsets and never changes a function's semantic
body fingerprint, so it cannot affect proof or evidence identity; `scripts/tests/check_type_alias.sh`
(fixtures in `tests/programs/type_alias/`) asserts the byte-identical `--report layout` and
`--report fingerprints`. The unknown-target refusal holds even for an alias that is never used, and
the cycle refusal (`type A = A;`, or `type A = B; type B = A;`) is its own diagnostic rather than a
later type mismatch. Not provided in V1: alias type parameters (`type Pair<T> = ...`; target a
concrete instantiation instead) and opaque or distinct aliases (a newtype gives a type its own
identity). Implemented as ROADMAP old Phase 6 #3 (2026-06-22).

```con accept name=alias-is-transparent exit=12
type Id = i32;
fn want_underlying(x: i32) -> i32 { return x; }
fn want_alias(x: Id) -> Id { return x; }
fn example_value() -> i64 {
    let a: Id = 5;
    let b: i32 = a;
    let c: Id = want_underlying(b);
    let d: i32 = want_alias(7);
    return (c + d) as i64;
}
fn main() -> u8 {
    if example_value() == 12 {
        return 12;
    }
    return 1;
}
```

<a id="TYPE-13"></a>
#### TYPE-13. A bare type parameter is linear-capable

**TYPE-13@3.** A type parameter without a bound may be instantiated at any type, linear ones included, so a generic body uses each value of that parameter exactly once, and `T: Copy` is the only bound that lets a body duplicate one.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.isCopyType
- Semantics: none
- Obligation: binder and kind facts survive specialization (PASS-1, PASS-2)
- Evidence: tested

**Rationale.** Decided 2026-10-23 as the normative reading of today's behaviour
(TIGHTENING_DECISIONS D5): Copy is the only property that changes what a body may do, so a
mandatory kind annotation would add syntax without removing a semantic case. There is no
linear-only kind. Duplicating a value of a bare `T` is a use after move (OWN-1).

Decided by the owner on 2026-10-29 (OD5): a bare `<T>` means Any, and `T: Copy` is explicit.

```con accept name=bare-and-copy-parameters exit=42
struct R { v: i64 }

fn pass<T>(x: T) -> T {
    return x;
}

fn twice<T: Copy>(x: T, f: fn(T) -> i64) -> i64 {
    return f(x) + f(x);
}

fn half(n: i64) -> i64 {
    return n / 2;
}

fn example_value() -> i64 {
    let r: R = pass::<R>(R { v: 0 });
    let R { v } = r;
    return twice::<i64>(42, half) + v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-14"></a>
#### TYPE-14. Copy is declared, never derived

**TYPE-14@3.** Whether an instantiation of a struct or enum is Copy is decided from its declaration alone, so it is Copy exactly when the struct or enum is declared `Copy` and every type argument is Copy.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Layout.isCopyTyGeneric
- Semantics: none
- Obligation: Copy is decided from the declaration and the type arguments alone, with no fixpoint over the type graph (R-0510)
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS D6, option (a)) and enforced the same
day. Copy used to be derived from an instantiation's fields, which Mono computed as a fixpoint
over the type graph; the fixpoint is gone. A generic declared `Copy` behaves like a derived
`Copy`: `struct Copy Bx<T> { v: T }` is accepted, its declaration checked once with `T` taken
as Copy (OWN-14), and `Bx<Int>` is Copy while `Bx<String>` is linear. A parameter no field
mentions still counts. A generic not declared `Copy` is never Copy.

Decided by the owner on 2026-10-29 (OD5): Copy for a generic struct or enum is declared and checked, never derived, and a generic declared Copy is Copy when every argument is.

```con accept name=copy-over-bare-parameter-at-copy exit=42
struct Copy Bx<T> { v: T }

fn example_value() -> i64 {
    let b: Bx<i64> = Bx::<i64> { v: 21 };
    let c: Bx<i64> = b;
    return b.v + c.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=copy-over-copy-parameter exit=42
struct Copy Bx<T: Copy> { v: T }

fn example_value() -> i64 {
    let b: Bx<i64> = Bx::<i64> { v: 42 };
    let c: Bx<i64> = b;
    return b.v + c.v * 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-15"></a>
#### TYPE-15. A generic type takes the arguments it declares

**TYPE-15@1.** A named type takes exactly as many type arguments and capability arguments as its declaration has type parameters and capability parameters, each of the matching kind, so a type with capability parameters is never named bare.

- Kind: refusal
- Uses: none
- Codes: E0113 (resolve), E0114 (resolve), E0115 (resolve)
- Status: enforced
- Enforced by: Concrete.resolveProgram
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** An extra type argument was once dropped silently, so `Box1<i32, bool>` compiled
and ran (bug 073), and a missing one surfaced later as an unrelated error on the unresolved
parameter. A capability argument says what a handle may do (R-0484), so a bare `Writer` for a
`Writer<cap C>` would leave the authority to inference; it is refused instead. The parser
classifies each argument as a type or a capability by position, and resolution checks the kind
and the count in both directions. A generic type named bare, with no argument list, is refused
too (bug 200).

```con reject name=extra-type-argument code=E0113 stage=resolve pair=declared-type-arguments
struct Copy Box1<T> { v: T }

fn example_value() -> i64 {
    let b: Box1<i64, bool> = Box1::<i64, bool> { v: 42 };
    return b.v;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=declared-type-arguments exit=42
struct Copy Box1<T> { v: T }

fn example_value() -> i64 {
    let b: Box1<i64> = Box1::<i64> { v: 42 };
    return b.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=bare-generic-type code=E0113 stage=resolve pair=declared-type-arguments
struct Copy Box1<T> { v: i64 }

fn example_value() -> i64 {
    let b: Box1 = Box1 { v: 42 };
    return b.v;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=type-where-capability code=E0114 stage=resolve pair=capability-argument
struct Copy Sink<cap C> { f: fn(i64) with(C) -> i64 }

fn take(s: Sink<i64>) -> i64 {
    return 0;
}

fn main() -> u8 {
    return 42;
}
```

```con reject name=capability-arity code=E0115 stage=resolve pair=capability-argument
struct Copy Sink<cap C> { f: fn(i64) with(C) -> i64 }

fn take(s: Sink<{}, {}>) -> i64 {
    return 0;
}

fn main() -> u8 {
    return 42;
}
```

```con accept name=capability-argument exit=42
struct Copy Sink<cap C> { f: fn(i64) with(C) -> i64 }

fn take(s: Sink<{}>) -> i64 {
    discard(s);
    return 0;
}

fn main() -> u8 {
    return 42;
}
```

<a id="TYPE-16"></a>
#### TYPE-16. A struct has a finite size

**TYPE-16@1.** A struct does not contain itself by value, directly or through the fields of other structs and enums or of their instantiations, so a recursive type goes through a `Heap` or a raw pointer.

- Kind: refusal
- Uses: none
- Codes: E0583 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: every type has a layout (R-0502)
- Evidence: tested

**Rationale.** A value is laid out inline, so a struct that holds itself by value has no finite
size. CoreCheck refuses it before code generation, where it used to surface as an LLVM
"recursive structure type" error. A reference cannot be a field (REF-3), so it does not break a
cycle. A generic instantiation is followed with its type arguments substituted, so `W<Node>`
inside `Node` is refused too (bug 205; it used to reach `llvm-as` while `--interp` ran it).

```con reject name=struct-contains-itself code=E0583 stage=core-check pair=finite-struct
struct Copy Node { v: i64, next: Node }
fn main() -> u8 { return 0; }
```

```con reject name=through-instantiation code=E0583 stage=core-check pair=finite-struct
struct Copy W<T> { v: T }
struct Copy Node { a: i64, w: W<Node> }
fn main() -> u8 { return 42; }
```

```con accept name=finite-struct exit=42
struct Copy Node { v: i64, w: i64 }
fn example_value() -> i64 { let n: Node = Node { v: 40, w: 2 }; return n.v + n.w; }
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TYPE-17"></a>
#### TYPE-17. A function type carries its capability set

**TYPE-17@2.** The capability set of a function type `fn(..) with(C) -> R` is part of the type, compared without regard to the order of its capabilities, so a function value keeps its capabilities through every binding, argument, field and combinator.

- Kind: guarantee
- Uses: TYPE-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Ty, Concrete.expectTy
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Authority rule 4 is the canonical current rule (OD93 refers to it): function values
keep exact capability sets without widening. A combinator declares its callback's set plus its own
effects; capability arguments come from checked argument types, not result context (OD45), and
duplicate capabilities are refused (OD108). Ambiguous set inference is
[undecided: see the target Core proposal](#core-capability-inference).

```con accept name=set-order exit=42
fn inc(x: i64) with(Fs, Network) -> i64 {
    return x + 1;
}

fn example_value() with(Fs, Network) -> i64 {
    let f: fn(i64) with(Network, Fs) -> i64 = inc;
    return f(41);
}

fn main() with(Fs, Network) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=combinator-carries-capabilities exit=42
fn both<cap C, cap D>(f: fn(i64) with(C) -> i64, g: fn(i64) with(D) -> i64) with(C, D) -> i64 {
    return f(g(40));
}

fn inc(x: i64) with(Fs) -> i64 {
    return x + 1;
}

fn inc2(x: i64) with(Network) -> i64 {
    return x + 1;
}

fn example_value() with(Fs, Network) -> i64 {
    return both(inc, inc2);
}

fn main() with(Fs, Network) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=field-keeps-capabilities exit=42
struct Copy Sink<cap C> { f: fn(i64) with(C) -> i64 }

fn inc(x: i64) with(Fs, Network) -> i64 {
    return x + 1;
}

fn use_it(s: Sink<Network + Fs>) with(Fs, Network) -> i64 {
    let g: fn(i64) with(Network, Fs) -> i64 = s.f;
    return g(41);
}

fn example_value() with(Fs, Network) -> i64 {
    let s: Sink<Fs + Network> = Sink { f: inc };
    return use_it(s);
}

fn main() with(Fs, Network) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TYPE-18"></a>
#### TYPE-18. Capabilities have no run-time representation

**TYPE-18@3.** A capability set or capability argument has no run-time representation, so types that differ only in their capability sets or capability arguments have the same size and layout, but generic instantiations differing in a function-type argument’s capability set have distinct specialization symbols.

- Kind: guarantee
- Uses: TYPE-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign, Concrete.Layout.fieldOffset, Concrete.Layout.tyToLLVM
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A capability is a compile-time permission, checked and then erased; it matters for
handle types (TYPE-15) and for the C layout of a struct passed across FFI. Whether two capability
instantiations of one generic function share generated code is an implementation choice, not a
language rule; OD109 requires distinct specialization symbols when function-type arguments differ only in their capability sets.

```con accept name=same-layout exit=42



struct Copy Sink<cap C> { f: fn(i64) with(C) -> i64, n: i64 }

pub fn main() -> u8 {
    let a: u64 = sizeof::<Sink<{}>>();
    let b: u64 = sizeof::<Sink<Fs + Network>>();
    let c: u64 = sizeof::<fn(i64) -> i64>();
    let d: u64 = sizeof::<fn(i64) with(Fs) -> i64>();
    let e: u64 = alignof::<Sink<{}>>();
    let f: u64 = alignof::<Sink<Fs + Network>>();
    if a == b && c == d && e == f {
        return 42;
    }
    return 1;
}
```



<a id="TYPE-19"></a>
#### TYPE-19. The primitive numeric types

**TYPE-19@2.** The integer types are the signed `i8`, `i16`, `i32` and `i64` and unsigned `u8`, `u16`, `u32` and `u64`, each as many bits wide as its name says, the float types are `f32` and `f64`, and builtin types have only their canonical spellings, with `Int`, `Uint`, `Float64`, `Float32`, `Bool` and `Char` refused.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.parseType, Concrete.tyFromName, Concrete.IntArith.intBitWidth
- Semantics: none
- Obligation: one `IntArith` authority for widths and signedness (R-0493)
- Evidence: tested

**Rationale.** The alias decision and OD102 give each primitive exactly one spelling; source examples use canonical names.

```con accept name=two-spellings exit=42
fn wide(x: u64) -> u64 {
    return x;
}

fn half(x: f64) -> f64 {
    return x / 2.0;
}

fn example_value() -> i64 {
    let a: i64 = 40;
    let b: i64 = a;
    let c: u64 = 1;
    let d: u64 = wide(c);
    let e: f64 = half(3.0);
    if e > 1.0 {
        return b + (d as i64) + 1;
    }
    return 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=widths-and-signedness exit=42
fn main() -> u8 {
    let a: i8 = 127;
    let b: u8 = 255;
    let c: i16 = 32767;
    let d: u16 = 65535;
    let e: i32 = 2147483647;
    let f: u32 = 4294967295;
    let g: i64 = 9223372036854775807;
    let h: u64 = 18446744073709551615;
    if wrapping_add(a, 1) == -128 && wrapping_add(b, 1) == 0 && wrapping_add(c, 1) == -32768
        && wrapping_add(d, 1) == 0 && wrapping_add(e, 1) == -2147483648 && wrapping_add(f, 1) == 0
        && wrapping_add(g, 1) < 0 && wrapping_add(h, 1) == 0 {
        return 42;
    }
    return 0;
}
```

<a id="TYPE-20"></a>
#### TYPE-20. A `char` is one byte

**TYPE-20@1.** A `char` is a one-byte code unit, so `sizeof::<char>()` is 1 and every `u8` value converts with `as` to a `char` that converts back to the same `u8`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: partial R-0555
- Enforced by: Concrete.Layout.tySize, Concrete.ccCheckExpr, Concrete.lowerExpr, Concrete.Interp.evalCast
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Strings are bytes ([STRING_TEXT_CONTRACT.md](../stdlib/STRING_TEXT_CONTRACT.md) §2),
and `char` is the type of one of them, which is why ARITH-5 accepts `u8` to `char` and `char` to any
integer of at least 8 unsigned or 16 signed bits. A Unicode-scalar `char` would be a separate
proposal with its own conversions.

```con accept name=char-is-a-byte exit=42


fn example_value() -> i64 {
    let b: u8 = 255;
    let c: char = b as char;
    let d: u8 = c as u8;
    if sizeof::<char>() == 1 { return (d as i64) - 213; }
    return 0;
}

pub fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=every-byte-round-trips-decided exit=42 pending=R-0555 now=reject:E0101:resolve
fn example_value() -> i64 {
    let mut i: i64 = 0;
    let mut same: i64 = 0;
    while i < 256 {
        let b: u8 = truncate::<u8>(i);
        let c: char = b as char;
        if (c as u8) == b {
            same = same + 1;
        }
        i = i + 1;
    }
    return same - 214;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TYPE-21"></a>
#### TYPE-21. Newtype declarations are refused

**TYPE-21@2.** Newtype declarations are refused; a named struct is the product type and has the layout specified by FFI-7 and FFI-8.

- Kind: refusal
- Uses: none
- Codes: new (parse)
- Status: pending R-0555
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign, Concrete.Layout.resolveNewtype, Concrete.Layout.tyToLLVM
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** OD54 removes newtypes; nominal wrappers use named structs and explicit field destructuring.





```con reject name=newtype-removed code=new stage=parse pair=struct-wrapper pending=R-0555 now=accept:42
newtype N = u8;
fn main() -> u8 { return 42; }
```

```con accept name=struct-wrapper exit=42
struct N { v: u8 }
fn main() -> u8 { let n: N = N { v: 42 }; let N { v } = n; return v; }
```

<a id="TYPE-22"></a>
#### TYPE-22. A constant is evaluated where it is used

**TYPE-22@1.** A module constant's initializer is evaluated at each use of the constant, in a scope of its own that sees no local of the using function, and never at program start, so the initializer of a constant that is not used never runs.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.lowerExpr, Concrete.Interp.evalExpr
- Semantics: none
- Obligation: none
- Evidence: tested

**Revised by the owner 2026-11-05** ([row Q6.4](#decided-2026-11-05-revisions)): a constant whose initializer provably fails is refused at compile time whether or not it is used (OD118). Both examples below, `unused-constant-never-runs` and `constant-traps-at-use`, become reject examples when this rule is written in Lean; until then the revision governs over them. When an initializer runs (at each use, never at start) is unchanged.

**Rationale.** A constant has no storage: the binary inlines its initializer at each use and the
interpreter evaluates it there, so a trap in an initializer (ARITH-3) happens at the use. A constant
is immutable and holds no capability or token (TRUST-8). Absorbed from
[INTERPRETER_TRUST.md](../verification/INTERPRETER_TRUST.md) §2.

```con accept name=unused-constant-never-runs exit=42
const BIG: i32 = 2147483647 + 1;

fn main() -> u8 {
    return 42;
}
```

```con abort name=constant-traps-at-use trap=overflow
const BIG: i32 = 2147483647 + 1;

fn example_value() -> i64 {
    let x: i32 = BIG;
    return x as i64;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

<a id="TYPE-23"></a>
#### TYPE-23. The builtin `Option` and `Result`

**TYPE-23@1.** The compiler provides the generic enums `Option<T>`, whose variants are `Some { value: T }` and `None`, and `Result<T, E>`, whose variants are `Ok { value: T }` and `Err { error: E }`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.builtinOptionEnum, Concrete.builtinResultEnum
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Builtins that can fail, such as `vec_pop`, and std's error conventions return these
two types, so their shape is part of the language rather than of a library module. Both are declared `Copy`, so an instantiation is Copy exactly when its type
arguments are (TYPE-14). OD43 refuses declarations that collide with builtin type names.

```con accept name=option-and-result exit=42
fn example_value() -> i64 {
    let o: Option<i64> = Option::<i64>::Some { value: 40 };
    let r: Result<i64, i64> = Result::<i64, i64>::Err { error: 2 };
    let a: i64 = match o { Option::Some { value } => value, Option::None => 0 };
    let b: i64 = match r { Result::Ok { value } => value, Result::Err { error } => error };
    return a + b;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TYPE-24"></a>
#### TYPE-24. Newtype conversions are refused

**TYPE-24@2.** Newtype constructors, newtype `.0` projections, newtype `as` conversions and the builtin `unwrap` are refused; destructuring uses `match` or an irrefutable pattern.

- Kind: refusal
- Uses: none
- Codes: new (parse)
- Status: pending R-0555
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD54, OD123 and OD155 remove the feature and its associated conversions rather than retaining representation-changing aliases.



```con reject name=unwrap-removed code=new stage=parse pair=match-wrapper pending=R-0555 now=accept:42
newtype N = u8;
fn main() -> u8 { let n: N = N(42); return unwrap(n); }
```

```con accept name=match-wrapper exit=42
struct N { v: u8 }
fn main() -> u8 { let n: N = N { v: 42 }; let N { v } = n; return v; }
```

### Builtin functions

This catalog describes decided requirements, not the frozen compiler's builtin table. TYPE-10
checks signatures, TRUST-4 checks permissions, and FFI-6 fixes primitive effects by identity.

| operation | decided contract | governing rules |
|---|---|---|
| `alloc` | allocator plus input and `with(Alloc)`, returning `Result<Heap<T, A>, AllocFailure<T>>` without losing the input on failure | MEM-5, MEM-7 |
| `*h` | consumes `Heap<T, A>`, returns its payload and releases storage using recorded release information | MEM-5 |
| `destroy` | explicit consuming cleanup returning Unit, requiring at most Alloc; never implicit | OWN-13, TRAIT-11 |
| `discard` | eligible Copy values only, never Result or Option | OWN-5, OWN-7 |
| `free`, `unwrap` | refused | OD59, OD155, TYPE-10, TYPE-24 |
| `sizeof`, `alignof` | size and alignment in bytes as `u64` | FFI-7 |
| `read_owned` | explicit raw ownership transfer with trusted premises | TRUST-7 |
| `abort` | terminal trap with no permission or resource requirement | STMT-13 |
| allocating strings and containers | allocator value plus Alloc, conserving fallible result; `Vec<T, A>` and `String<A>` retain allocator identity | MEM-7 |
| filesystem operations | Directory/File resource plus Fs; lookup stays inside the directory and crosses no mount | authority and filesystem decisions |
| network operations | destination/endpoint/resolver grant or socket plus Network; DNS grants no connection | authority and filesystem decisions |
| `get_args` | Environment plus Env and an allocator plus Alloc when allocating | revised OD156 |
| clock operations | Clock grant plus Time | authority decision |

Complete operation contracts, failure payloads and container Destroy applicability are
[undecided: see the target Core proposal](#core-allocation-contracts). Static-text
literal typing and conversion are [undecided: see the target Core proposal](#core-static-text).
Target service schemas are [undecided: see the target Core proposal](#core-grants).

### Absent by decision

This section is explanatory: it records type-system features the language does not have, why, and
what would reopen them. The grammar (SYN-1) is what refuses them.

#### No anonymous tuples

Decided 2026-06-22 (ROADMAP old Phase 6 #5, which asked for tuple types or a recorded decision
against them), pinned by `scripts/tests/check_no_tuples.sh`. There are no tuple types `(A, B)`, no
tuple literals `(a, b)` and no positional tuple indexing; each is a parse error. The named struct is
the one product type, and a function that returns two values returns a small `struct Copy`:

```con pseudocode
struct Copy DivMod { quotient: i32, remainder: i32 }
fn divmod(a: i32, b: i32) -> DivMod {
    return DivMod { quotient: a / b, remainder: a % b };
}
```

`t.0` stays the representation projection of a newtype (`Port(u16)` gives `port.0 : u16`), a named
single-field wrapper, not a tuple. The reasons: field names flow into layout reports, obligation
messages, backend contracts and proofs, and `result.remainder` describes itself where `result.1`
does not; anonymous tuples and named structs overlap almost entirely, so carrying both would double
the surface (patterns, layout, code generation, formatting, documentation); the language is
explicit and named everywhere else (no implicit conversions, named capabilities, named fields); and
no workload needs them, so adding them would be symmetry-driven rather than workload-driven. Tuples
would be reconsidered only if a real workload shows named structs insufficient (for example heavy
generic plumbing where naming every intermediate pair is untenable).

#### No units of measure

Evaluated and deferred for v1 (ROADMAP old Phase 6 #15). Unit annotations (bytes or bits,
milliseconds or seconds, block counts or byte offsets, protocol lengths, memory sizes) would catch a
real class of systems mistakes, but they are a type-system feature with proof and report
consequences, not a local addition: a unit-tagged integer must extract to the same `PExpr` as its
untagged form, or units become a proof axis; unit erasure must be an audit fact, never silent, so no
proof status may depend on erasing a unit unless a contract or VC names the conversion; and done
poorly they add ceremony for little near-term value. Programs encode the unit in the name instead
(`len_bytes`, `timeout_ms`, `off_in_block`, `size_bits`, per [STYLE.md](../project/STYLE.md)), and today's checked arithmetic
(ARITH-3; OD10, decided 2026-10-29) removes the silent corruption unit
mix-ups often hide behind. If a real
workload keeps hitting unit bugs, the first step is a report-only prototype (`examples/units_probe/`
over the mistake classes above) with optional annotations erased only after the audit records the
conversion: no implicit unit conversion, no dependent numeric refinements.

#### No const generics

Designed and deferred (2026-06-21, ROADMAP old Phase 7 #8f). A type is not parameterized by a
compile-time value (`Buf<T, const N: u64>`). The V1 design is fixed so the feature is unambiguous
when a workload pulls it: integer parameters only, arguments that are literals or constants the
existing folder evaluates, one specialization per distinct value with `N` in its mangled name, and
`N` recorded concretely in the ledger, the layout report and every obligation (`index < 16`, never a
symbolic `index < N`); type-level arithmetic and dependent constraints are rejected. The forcing
probe found no workload that needs it, and it is built only when one does. The design, the probe,
the forcing conditions and the staged build are in
[research/language/const-generics-v1-decision.md](../../research/language/const-generics-v1-decision.md).

### Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- TYPE-4@2 drops the `break` value from the list of positions a value flows into, since a `break` carries no value (OD27).
- TYPE-6@1 admitted raw-pointer comparison and `p + k` in trusted or `Unsafe` code (bugs 134 and 140). TYPE-6@2 records OD26 (2026-10-29), which removes both, pointer arithmetic becoming `ptr::offset` (TRUST-1).
- TYPE-9@2 states the type-argument count that E0322 already enforced (bug 221: an extra type argument was dropped silently).
- TYPE-14@1 (pending, never enforced) required a `T: Copy` bound instead and was retired by the option (a) decision (TIGHTENING_DECISIONS D6, 2026-10-23).

- TYPE-D1@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-D1@1 is superseded by version.
- TYPE-5@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-5@2 is superseded by version.
- TYPE-6@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-6@2 is superseded by version.
- TYPE-7@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-7@1 is superseded by version.
- TYPE-10@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-10@1 is superseded by version.
- TYPE-11@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-11@1 is superseded by version.
- TYPE-13@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-13@2 is superseded by version.
- TYPE-14@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-14@2 is superseded by version.
- TYPE-17@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-17@1 is superseded by version.
- TYPE-18@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-18@1 is superseded by version.
- TYPE-19@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-19@1 is superseded by version.
- TYPE-21@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-21@1 is superseded by version.
- TYPE-24@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TYPE-24@1 is superseded by version.

- TYPE-3@2 (2026-11-03) supersedes TYPE-3@1; pending legacy conformance is explicit.


<a id="ch-arithmetic"></a>
## Arithmetic

The numeric types are TYPE-19's. Every ordinary integer operation is checked: it
traps on a result its type cannot represent, and the trap is an abort (STMT-2). Wrapping and
saturating arithmetic are named operations. There is no implicit numeric conversion (TYPE-4), and
which operand types an operator takes is TYPE-6. The design records are
[ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) (absorbed into ARITH-8 to
ARITH-11, TYPE-19 and TYPE-20 on 2026-10-30; it keeps the profile mapping, report classification and
proof boundaries) and NUMERIC_MODEL.md (absorbed into ARITH-2, ARITH-3 and ARITH-5 and deleted).

### Rules

<a id="ARITH-1"></a>
#### ARITH-1. Floating point is IEEE-754 with round-to-nearest-even

**ARITH-1@1.** `f32` and `f64` are IEEE-754 binary32 and binary64, every operation rounds to nearest with ties to even, and no operations are contracted (no fused multiply-add unless written).

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0493
- Semantics: none
- Obligation: a total formal reference model for floats, proved for the profile or labelled trusted (VERIFIED_PIPELINE, "Floats")
- Evidence: tested

**Rationale.** A fixed rounding mode and no contraction make float results deterministic across
the interpreter, the LLVM profile and the native profile, so the consumers can be compared and a
reference model can define them. NaN bits, subnormals, signed zero and conversions are specified
together with the reference model.

<a id="ARITH-2"></a>
#### ARITH-2. An integer literal fits its type

**ARITH-2@4.** An integer literal takes the integer type its position expects, an integer literal with no expected type is refused, and an integer literal whose value, negation included, is outside its type's range is refused rather than truncated.

- Kind: refusal
- Uses: none
- Codes: E0227 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: one `IntArith` authority for literal ranges (R-0493)
- Evidence: tested

**Rationale.** A literal must never silently become a different value: `let a: u8 = 300` used to
be 44. The negated value is what is range-checked, so `let a: i8 = -128` is accepted and
`-129` is not. Hex (`0xFF`) and binary (`0b1010`) literals are accepted; suffixes such as `7u8`
are not part of the language (an annotation or a conversion says the type).

A hint (an annotation, a parameter, a field, an arm) gives a literal the hinted type. Suffixes are
workload-gated: an
annotation already covers the need, so a suffix would be pure convenience, revisited if a real
workload finds annotations too noisy. Gated by `scripts/tests/check_numeric_literals.sh` (ROADMAP
old Phase 6 #6, 2026-06-23).

The recorded answers in [OD63](#od63), [OD138](#od138) govern this subject; the frozen compiler’s earlier behavior is not an alternative rule.

```con reject name=literal-out-of-range-decided code=E0227 stage=check pair=literal-in-range pending=R-0555 now=reject:E0227:check
fn example_value() -> i64 {
    let a: u8 = 300;
    return a as i64;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=literal-in-range exit=42
fn example_value() -> i64 {
    let a: i8 = -128;
    let b: u8 = 0xFF;
    return (a as i64) + (b as i64) - 85;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=unconstrained-literal code=new stage=check pair=constrained-literal pending=bug-327 now=accept:42
fn main() -> u8 {
    if 3 < 4 {
        return 42;
    }
    return 0;
}
```

```con accept name=constrained-literal exit=42
fn main() -> u8 {
    let a: i64 = 3;
    if a < 4 {
        return 42;
    }
    return 0;
}
```

<a id="ARITH-3"></a>
#### ARITH-3. Integer arithmetic is checked

**ARITH-3@1.** `+`, `-`, `*`, unary `-`, `/` and `%` on integers abort when the mathematical result is not representable in the operand type, when the divisor is zero and for `MIN / -1` and `MIN % -1`, and a shift aborts when its count is negative or at least the operand's width and otherwise discards the bits shifted out.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit, Concrete.Pipeline.lower
- Semantics: none
- Obligation: traps are behaviours of Core semantics with a ghost kind (R-0493, R-0496); a pass removes a trap only with a discharged-VC certificate
- Evidence: tested

**Rationale.** Silent wrapping is a correctness bug in almost all code, and signed overflow is
the case C leaves undefined. Concrete never emits LLVM's `nsw` or `nuw`: checked operations use
the overflow intrinsics and branch to an abort helper. This holds in every profile; a profile
changes reports, never code generation (OD97 keeps every runtime check and permits profiles to refuse undischarged operations). The
fill of a right shift that does not abort is ARITH-8; OD114 makes integer division truncate toward zero and the remainder take the dividend’s sign. ARITH-13 refuses an operation the compiler proves always overflows (E0902), but in the current
compiler no operation reaches that proof (bug 350), so E0902 currently provides no compile-time
refusal; the run-time overflow check is the guarantee. Array bounds are
checked the same way, an index out of bounds aborting.

For example `let c: u8 = 200 + 100` compiles and traps at run time: E0902 is never reported
(`provenViolationDiagnostics`, the path that also reports a proven out-of-bounds index, E0900, and
a proven zero divisor, E0901), so this proof path currently refuses no program for overflow; completing the folder
is ROADMAP #10.

Decided by the owner on 2026-10-29 (OD10): integer arithmetic is checked by default and the shift count is checked; wrapping and saturating arithmetic are named operations (ARITH-4), and an abort runs no deferred call (STMT-2).

```con accept name=checked-in-range exit=42
fn example_value() -> i64 {
    let a: u8 = 200;
    let s: u8 = a << 1;
    let q: i64 = -7 / 2;
    return (s as i64) - 99 + q;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con abort name=unsigned-overflow trap=arithmetic_overflow
fn example_value() -> i64 {
    let a: u8 = 200;
    let b: u8 = 100;
    let c: u8 = a + b;
    return c as i64;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con abort name=divide-by-zero trap=division_by_zero
fn example_value() -> i64 {
    let a: i64 = 42;
    let z: i64 = a - 42;
    return a / z;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con abort name=shift-count-too-large trap=shift_amount_out_of_range
fn example_value() -> i64 {
    let a: u8 = 1;
    let n: u8 = 8;
    let s: u8 = a << n;
    return s as i64;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

<a id="ARITH-4"></a>
#### ARITH-4. Wrapping and saturating arithmetic are named operations

**ARITH-4@1.** `wrapping_add`, `wrapping_sub` and `wrapping_mul` compute the two's-complement result modulo 2 to the operand's width, and `saturating_add`, `saturating_sub` and `saturating_mul` clamp it to the type's range, for every integer type and without a trap.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit
- Semantics: none
- Obligation: the named operations are typed operations of Core (R-0493)
- Evidence: tested

**Rationale.** Modular arithmetic (hashes, checksums, ring indices) and clamping (accumulators,
sensors) are intentional, so the program says so at the operation. There is no wrapping or
saturating division and no wrapping shift; division has no modular meaning, and a shift by
`n mod width` has had no use.

```con accept name=wrap-and-saturate exit=42
fn example_value() -> i64 {
    let a: u8 = 200;
    let w: u8 = wrapping_add(a, 98);
    let s: u8 = saturating_add(a, 100);
    let t: i8 = saturating_sub(-100, 100);
    return (w as i64) + (s as i64) - 255 + (t as i64) + 128;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="ARITH-5"></a>
#### ARITH-5. `as` converts without loss

**ARITH-5@4.** A numeric, `bool` or `char` conversion written with `as` is accepted only when every value of the source type converts to a value of the target type that means the same number, `bool` or byte, namely a type to itself, an integer to a wider integer of the same signedness, an unsigned integer to a wider signed one, `bool` to an integer, `char` to an unsigned integer of at least 8 bits or a signed one of at least 16, `u8` to `char`, an integer to a float that represents every value exactly, and `f32` to `f64` (the conversions outside this list that the current compiler still accepts, narrowing, a
sign change other than unsigned to wider signed, and float to integer, are this entry's partial part,
refused once R-0500 lands), and every other such conversion is written with a named operation (a checked conversion returning an error, `truncate`, `reinterpret`, or a named float rounding).

- Kind: refusal
- Uses: none
- Codes: E0553 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: one `IntArith` authority for conversions (R-0493); a lossless `as` needs no value-preservation obligation
- Evidence: tested

**Rationale.** Decided 2026-10-21 (ARITHMETIC_POLICY §8, exit 2k): `as` is total and
obligation-free, and each lossy conversion names what it does. Today `as` refuses conversions to and from `bool` other than `bool` to an integer
(integer, float and `char` to `bool`, and `bool` to `char`: E0553, bugs 108 and 130). Narrowing
and sign changes are still accepted and truncate or reinterpret, and float to integer truncates
toward zero and aborts on NaN, an infinity or a value outside the target's range (a checked
conversion), until the named operations land and the corpus migrates (about 500 to 700 sites). Pointer and reference casts are not
numeric conversions (TRUST-1 owns the pointer–integer case; OD98 requires trusted/Unsafe code for reference-to-pointer conversion). The named operations' results are ARITH-10, and their
spellings are fixed by OD99.

Until then, in the current compiler, narrowing truncates silently (`300 as u8` is 44, `-1 as u32` is
4294967295, `3.9 as i32` is 3). Even a widening needs `as`: `want_u64(my_u8)` is E0220 (TYPE-4),
so every conversion is visible at the call site.

Decided by the owner on 2026-10-29 (OD11): `as` only when lossless by value domain, and every lossy conversion a named function.

```con reject name=int-to-bool-decided code=E0553 stage=core-check pair=widening pending=R-0555 now=reject:E0553:core-check
fn main() -> u8 {
    let x: i64 = 1;
    let b: bool = x as bool;
    if b {
        return 1;
    }
    return 0;
}
```

```con accept name=widening exit=42
fn example_value() -> i64 {
    let a: u8 = 41;
    let b: i32 = a as i32;
    let t: bool = true;
    return (b as i64) + (t as i64);
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=narrowing code=E0553 stage=core-check pair=widening pending=R-0500 now=accept:44
fn example_value() -> i64 {
    let x: i64 = 300;
    let t: u8 = x as u8;
    return t as i64;
}
fn main() -> u8 {
    if example_value() == 44 {
        return 44;
    }
    return 1;
}
```

<a id="ARITH-6"></a>
#### ARITH-6. Comparisons follow their operand type

**ARITH-6@1.** An integer comparison is signed or unsigned as its operand type is, a float comparison follows IEEE 754 (every ordered comparison with a NaN operand is false, and `x == x` is false for a NaN), and a comparison reads its operands without moving them.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit, Concrete.checkExprCore
- Semantics: none
- Obligation: no pass folds a float self-comparison (R-0493)
- Evidence: tested

**Rationale.** `(200 : u8) < 100` is false; signedness is a property of the type, never of the
comparison. A comparison between a signed and an unsigned type is refused (TYPE-6), so it never
reinterprets. Which types have comparisons at all is TYPE-6.

```con accept name=unsigned-comparison exit=42
fn example_value() -> i64 {
    let a: u8 = 200;
    let b: u8 = 100;
    let c: i8 = -1;
    let d: i8 = 1;
    let mut out: i64 = 0;
    if a > b {
        out = out + 40;
    }
    if c < d {
        out = out + 2;
    }
    return out;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="ARITH-7"></a>
#### ARITH-7. A float literal is correctly rounded

**ARITH-7@2.** A float literal denotes its decimal value rounded to nearest, ties to even, at the float type it takes, and every consumer uses that value.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Pipeline.emit
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** This is ARITH-1's rounding applied to the literal itself: a literal printed with too few digits
in the emitted IR once changed `0.1` (bug 109). The first program below holds only if the literals
are the correctly rounded binary64 values. The current compiler rounds an `f32` literal twice, through binary64,
so a literal just above a binary32 midpoint takes the even neighbour (bug 332), pinned by the
pending example.

```con accept name=float-literal-rounding exit=42
fn main() -> u8 {
    let a: f64 = 0.1;
    let b: f64 = 0.2;
    let c: f64 = 0.30000000000000004;
    if a + b == c {
        return 42;
    }
    return 0;
}
```

```con accept name=f32-literal-rounded-once exit=42 pending=bug-332 now=accept:0
fn main() -> u8 {
    let a: f32 = 1.0000000596046447753906250001;
    let b: f32 = 1.00000011920928955078125;
    if a == b {
        return 42;
    }
    return 0;
}
```

<a id="ARITH-8"></a>
#### ARITH-8. The fill of a right shift

**ARITH-8@1.** A right shift that does not abort fills the vacated bits with the sign bit on a signed type and with zeros on an unsigned type.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.IntArith.evalIntShift, Concrete.Pipeline.emit
- Semantics: none
- Obligation: the shift results are `IntArith`'s, the one arithmetic authority (R-0493)
- Evidence: tested

**Rationale.** These are LLVM's `ashr` and `lshr` (the archived ARITHMETIC_POLICY §6). When a shift
aborts instead (a count outside the width) is ARITH-3. The interpreter and the constant folder
compute it in `IntArith`, and the binary through the abort helpers. OD114 requires integer division to truncate toward zero and the remainder to take the dividend’s sign.

```con accept name=shift-fill exit=42
fn example_value() -> i64 {
    let a: i8 = -128;
    let s: i8 = a >> 7;
    let u: u8 = 128;
    let t: u8 = u >> 7;
    return (s as i64) + (t as i64) + 42;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="ARITH-9"></a>
#### ARITH-9. No ambient arithmetic mode

**ARITH-9@1.** No attribute, `#[overflow_checked]` included, and no module or function setting changes what an integer operator or a named arithmetic operation computes, so an operation's mode is given by its spelling alone.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit, Concrete.Pipeline.lower
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A reader of `a + b` knows it is checked (ARITH-3) and of `wrapping_add(a, b)` that it
wraps (ARITH-4) without looking for an enclosing `#[wrapping]` attribute or module switch; none
exists. `#[overflow_checked]` asks the proof pipeline for a no-overflow obligation at each
fixed-width `+`, `-` and `*` of the function, which is evidence, not meaning: the operations still
abort on overflow. OD97 keeps every runtime check and permits a profile to refuse undischarged operations.

```con accept name=overflow-checked-in-range exit=42
#[overflow_checked]
fn add(a: u8, b: u8) -> u8 {
    return a + b;
}

fn example_value() -> i64 {
    return add(40, 2) as i64;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con abort name=overflow-checked-still-traps trap=arithmetic_overflow
#[overflow_checked]
fn add(a: u8, b: u8) -> u8 {
    return a + b;
}

fn example_value() -> i64 {
    return add(200, 100) as i64;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=named-operations-unchanged exit=42
#[overflow_checked]
fn named(a: u8, b: u8) -> i64 {
    let w: u8 = wrapping_add(a, b);
    let s: u8 = saturating_add(a, b);
    return (w as i64) + (s as i64);
}

fn example_value() -> i64 {
    return named(200, 100) - 257;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="ARITH-10"></a>
#### ARITH-10. The named conversions' results

**ARITH-10@2.** `truncate::<T>(x)` keeps the low-order bits of `x`'s two's-complement representation, as many as `T`'s width, `reinterpret::<T>(x)` changes the signedness of the integer `x` at the same width and keeps its bit pattern, and `try_convert::<T>(x)` returns `Result<T, OutOfRange>`, with success containing the value of `T` that means the same number as `x` when there is one and failure otherwise (an integer out of range, a float that is not integral, out of range, NaN or infinite, or a value a target float does not represent exactly).

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Semantics: none
- Obligation: the named conversions are typed operations of the one arithmetic authority (R-0493)
- Evidence: tested

**Rationale.** These are the conversions ARITH-5 sends every lossy `as` to, each with one stated
result, so that no consumer has to guess what a lossy conversion means (bugs 080 and 128).
`truncate::<u8>(300)` is 44 and `reinterpret::<u32>(-1)` is 4294967295, what a narrowing or
sign-changing `as` gives today. OD106 fixes `try_convert` as `Result<T, OutOfRange>`; OD99 fixes these builtin spellings and the float-rounding spellings `trunc`, `round`, `floor`, `ceil`. The float conversions are ARITH-11; implementation remains pending R-0500.

```con accept name=truncate-and-reinterpret exit=42 pending=R-0500 now=reject:E0101:resolve
fn example_value() -> i64 {
    let x: i64 = 300;
    let t: u8 = truncate::<u8>(x);
    let m: i32 = -1;
    let u: u32 = reinterpret::<u32>(m);
    if u == 4294967295 {
        return (t as i64) - 2;
    }
    return 0;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=checked-conversion-result exit=42 pending=R-0500 now=reject:E0108:resolve
fn convert(x: i64) -> Result<u8, OutOfRange> {
    return try_convert::<u8>(x);
}
fn main() -> u8 { return 42; }
```

<a id="ARITH-11"></a>
#### ARITH-11. The named float conversions round as stated

**ARITH-11@2.** A named conversion from an integer to a float, or from `f64` to `f32`, rounds to the nearest representable value with ties to even, and each float-to-integer rounding operation (`trunc`, `round`, `floor`, `ceil`) rounds as named and returns the resulting integer when representable, or an error as `try_convert` for NaN, infinities and results outside the target integer range.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0500
- Semantics: none
- Obligation: the named conversions are typed operations of the one arithmetic authority (R-0493)
- Evidence: tested

**Rationale.** These are the conversions ARITH-5 sends a rounding `as` to (`i32` to `f32`, `i64` to
`f64`, `f64` to `f32`, every float to an integer). Today `as` rounds an integer to a float natively
and is the identity in `--interp` (bug 128), and a float-to-integer `as` truncates toward zero and
aborts on NaN, an infinity or a value out of range. OD107 instead requires an error as `try_convert`; OD99 fixes the builtin spellings `trunc`, `round`, `floor`, `ceil`. Implementation remains pending R-0500.

```con accept name=round-result exit=42 pending=R-0500 now=reject:E0108:resolve
fn convert(x: f64) -> Result<u8, OutOfRange> {
    return trunc::<u8>(x);
}
fn main() -> u8 { return 42; }
```

<a id="ARITH-12"></a>
#### ARITH-12. Float remainder

**ARITH-12@1.** `x % y` on floats is C's `fmod(x, y)`: the exact value `x - n * y` for the integer `n` that is `x / y` truncated toward zero, which has the sign of `x`, and NaN when `y` is zero or `x` is infinite or either is NaN.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Interp.floatRem, Concrete.Pipeline.emit
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** It is LLVM's `frem`, which the binary emits, and the interpreter computes the same
value. Unlike IEEE 754's `remainder`, it truncates rather than rounding the quotient to nearest.
Absorbed from [INTERPRETER_TRUST.md](../verification/INTERPRETER_TRUST.md) §2.

```con accept name=float-remainder exit=42
fn main() -> u8 {
    let a: f64 = -7.5;
    let b: f64 = 2.0;
    let r: f64 = a % b;
    if r == -1.5 {
        return 42;
    }
    return 1;
}
```

<a id="ARITH-13"></a>
#### ARITH-13. A division by a proven zero or a proven overflow is refused

**ARITH-13@2.** An integer `/` or `%` whose divisor the compiler proves zero, and an integer operation the compiler proves always overflows its type, are refused in every function, including a `trusted` function or one declaring `Unsafe`.

- Kind: refusal
- Uses: none
- Codes: E0901 (runtime-safety), E0902 (runtime-safety)
- Status: pending R-0555
- Enforced by: Concrete.Report.provenViolationDiagnostics
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD118 applies the proven-violation refusal inside trusted code too. The mandatory proof criterion is [undecided: see the target Core proposal](#core-static-refusal).

```con accept name=divisor-not-zero exit=42
fn example_value() -> i64 {
    let n: i64 = 84;
    return n / 2;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=divisor-proven-zero-decided code=E0901 stage=runtime-safety pair=divisor-not-zero interp-exempt=interp-runs-proven-violations pending=R-0555 now=reject:E0901:runtime-safety
fn example_value() -> i64 {
    let n: i64 = 84;
    return n / 0;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=divisor-proven-zero-unrun code=E0901 stage=runtime-safety pair=divisor-not-zero pending=bug-340 now=reject:E0901:runtime-safety now-interp=accept:42
fn example_value() -> i64 {
    let n: i64 = 84;
    let c: bool = false;
    if c {
        return n / 0;
    }
    return 42;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```
### Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- ARITH-2@1 typed a literal with no expected type `Int` (`i64`), which the current compiler still does. ARITH-2@2 recorded OD31 (2026-10-29) for integer and float literals. ARITH-2@3 (spec audit 8, 2026-10-30) states the integer-literal rule only, the float literal being OPEN-63 and the positions OPEN-138.
- ARITH-5@1 listed the accepted conversions in its rationale; ARITH-5@2 (spec audit 12, 2026-10-30) lists them in the sentence. ARITH-5@3 (spec audit 20, 2026-10-31) says "the current compiler" where ARITH-5@2 said "main" for the conversions still accepted.

- ARITH-13@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; ARITH-13@1 is superseded by version.

- ARITH-2@4 (2026-11-03) supersedes ARITH-2@3; pending legacy conformance is explicit.

- ARITH-5@4 (2026-11-03) supersedes ARITH-5@3; pending legacy conformance is explicit.

- ARITH-7@2 (2026-11-03) supersedes ARITH-7@1; pending legacy conformance is explicit.


<a id="ch-statements"></a>
## Statements, Expressions and Execution

Statements execute in source order. A block's trailing expression without `;` is its value, and
`e;` is a statement whose value is discarded (OWN-5 to OWN-7 say which values may be). This
chapter covers how executions end (STMT-1 to STMT-3), evaluation order (STMT-4), where exits may
appear (STMT-5), loops (STMT-6), `main` (STMT-7), value blocks (STMT-8), returning a value on
every path (STMT-9), test functions (STMT-10), the value flow of statements (STMT-11), the counted
loop's measure (STMT-12) and the capability `abort()` needs (STMT-13). The design records are
[STATEMENT_EXPRESSION_MODEL.md](../archive/design-records/STATEMENT_EXPRESSION_MODEL.md)
and [MAIN_EXIT_MODEL.md](../platform/MAIN_EXIT_MODEL.md). [EXECUTION_MODEL.md](../platform/EXECUTION_MODEL.md)
keeps the runtime and platform description (hosted runtime, startup, allocation, targets).
The section "Program execution" at the end follows a program from its source files to a running
process and records what `--interp` does differently today.

### Rules

<a id="STMT-1"></a>
#### STMT-1. Exit, abort and divergence are distinct behaviours

**STMT-1@1.** Every execution that does not get stuck has exactly one behaviour, a trace of external events that ends in exit with a status, abort at a defined trap, silent divergence or infinite external interaction, and no behaviour is observed as another, while stuck execution (a violated premise of a trusted operation) is a distinct state that safe code never reaches.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: the behaviour definition of Core semantics (R-0496)
- Evidence: tested

**Rationale.** Correctness theorems quantify over behaviours. If a trap could look like a normal
exit, or a divergence like an exit, a refinement proof would equate different programs. The Core
semantics records the external-event trace together with its outcome (VERIFIED_PIPELINE,
"Semantic contract"). Clock exhaustion in the evaluator is inconclusive, not divergence.

<a id="STMT-2"></a>
#### STMT-2. Allocation errors conserve; traps abort

**STMT-2@2.** A trap or explicit abort ends the process immediately without unwinding, deferred calls, destructors or loan endings, while program allocation failure is an ordinary error that conserves inputs and ownership obligations.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Semantics: none
- Obligation: abort is a behaviour of Core semantics and every "exactly once" cleanup claim is qualified by "on every path that does not abort" (R-0496)
- Evidence: tested

**Rationale.** The Allocation decision distinguishes ordinary conserving allocation errors from terminal traps. Stack, runtime and environment exhaustion remain separately stated premises of VERIFIED_PIPELINE, not program OOM aborts.

<a id="STMT-3"></a>
#### STMT-3. Programs are single-threaded

**STMT-3@1.** A Concrete program has one thread of execution, so no memory is mutated concurrently with it.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: the first verified profile is sequential (R-0496)
- Evidence: tested

**Rationale.** Concurrency needs a memory model, ownership-transfer rules and new refinement
obligations, and none exists yet. Foreign threads, asynchronous shared-memory mutation, signals
and reentrant callbacks are outside the semantics unless the environment model covers them
(VERIFIED_PIPELINE, "Scope of the first verified profile"). Child processes are separate
programs, not threads.

<a id="STMT-4"></a>
#### STMT-4. Evaluation is left to right, assignment evaluates its value first

**STMT-4@2.** Subexpressions are evaluated left to right in source order, each completely before the next begins (a call's receiver, then its arguments; a binary operator's left operand, then its right, with `&&` and `||` short-circuiting; literal elements and fields in the order written; a base before its index), except that an assignment evaluates its right-hand side before its destination's subexpressions.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Pipeline.lower
- Semantics: none
- Obligation: V1a's evaluation-order obligation; the verified pipeline may not model a looser order than the compiler implements (R-0500, R-0496)
- Evidence: tested

**Rationale.** Evaluation remains left to right except that assignment computes its value before its destination; OD53 removes the functional-update evaluation case.

```con accept name=source-order exit=42
struct Copy P { a: i64, b: i64 }

fn tick(acc: &mut i64, k: i64) -> i64 {
    *acc = *acc * 10 + k;
    return k;
}

fn example_value() -> i64 {
    let mut acc: i64 = 0;
    let p: P = P { b: tick(&mut acc, 2), a: tick(&mut acc, 1) };
    let s: i64 = tick(&mut acc, 1) + tick(&mut acc, 2);
    return acc - 2112 + p.a + p.b + s * 0 + 39;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=enum-literal-source-order exit=21
enum Copy E { V { a: i64, b: i64 }, W }

fn tick(acc: &mut i64, k: i64) -> i64 {
    *acc = *acc * 10 + k;
    return k;
}

fn example_value() -> i64 {
    let mut acc: i64 = 0;
    let e: E = E::V { b: tick(&mut acc, 2), a: tick(&mut acc, 1) };
    discard(e);
    return acc;
}

fn main() -> u8 {

    if example_value() == 21 {

        return 21;

    }

    return 1;

}
```

<a id="STMT-5"></a>
#### STMT-5. No exit inside an operand

**STMT-5@2.** `return`, `break` and `continue` do not occur inside a call argument or receiver, a struct, enum or array literal element, or an operator operand, unless a `break` or `continue` belongs to a loop entered inside that operand.

- Kind: refusal
- Uses: none
- Codes: E0278 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: an operand already evaluated for a pending construct is a live owned temporary until it completes (R-0499)
- Evidence: tested

**Rationale.** Operand exits cannot bypass the surrounding operation; OD53 removes functional update and its former base-operand case.

```con reject name=return-in-argument-decided code=E0278 stage=check pair=exit-in-branch pending=R-0555 now=reject:E0278:check
fn add(a: i64, b: i64) -> i64 {
    return a + b;
}

fn f(c: bool) -> i64 {
    return add(1, if c { return 42; } else { 2 });
}

fn example_value() -> i64 {
    return f(true);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=exit-in-branch exit=42
fn f(o: Option<i64>) -> i64 {
    let v: i64 = match o {
        Option::Some { value } => value,
        Option::None => { return 42; }
    };
    return v;
}

fn example_value() -> i64 {
    return f(Option::None);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="STMT-6"></a>
#### STMT-6. Loops are statements; `break` and `continue` name a loop

**STMT-6@4.** The loops are `while cond { ... }` and the counted loop `for i from a until b { ... }`, both statements, where the counted loop evaluates `a` and then `b` once, both of one integer type, which the index `i` takes, before its first iteration, compares `i` with `b` before each iteration and advances it only after a comparison that succeeded, so it never advances `i` past `b`, and runs its body with `i`, immutable and scoped to the body, bound to each value from `a` up to but not including `b`, none when `a >= b`; `break` carries no value, and `break` and `continue` act on the innermost enclosing loop or, with a label, on the labeled loop and appear only inside a loop, `continue` running the body's exit sequence and then advancing only the loop it names, while `break` and `return` advance nothing.

- Kind: refusal
- Uses: none
- Codes: E0216 (check), E0245 (check), E0280 (check), E0281 (check), E0542 (core-check), E0543 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.checkStmt, Concrete.ccCheckStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** There is no bare `loop`; `while true` is the infinite loop. A loop produces no
value (the value `while … else` form was removed in Phase 6D); a result is a mutable local
assigned before `break`. In the current compiler's `for (init; cond; step)` loop, `continue` still runs `step`, and
E0245 refuses a `break` or `continue` in the step that targets its own loop (bug 330); E0245 belongs to
this entry only while the current compiler parses the step form, which this entry removes (OD47), and retires with it. A
`break` or `continue` leaves the loop body's scopes like any exit (OWN-2).

The current compiler's loop forms are `while cond { ... }` and the C-style forms that this entry removes (OD47, bug 331):
`for (init; cond; step) { ... }`, whose `init` is a `let`,
whose `cond` is the continuation test and whose `step` is an expression or an assignment
(`for (i = 0; ...)` is a parse error), `for (cond) { ... }` and the unconditional `for { ... }`
(`regress_176_loop_shadow_restores_outer.con`). An unlabeled `break` or
`continue` acts on the innermost enclosing loop; a label (`'outer: while ...`) is the only way to
act on an outer loop, and `break 'outer;` or `continue 'outer;` exits or continues every loop in
between. A `break` or `continue` that would leave a linear value live is refused (E0210, E0211;
OWN-2), while consuming the value before the jump is fine: the rule concerns values live at the
jump, as for an early `return`. A `break` or `continue` does not disable runtime-safety
obligations (an index in a loop with a `break` still gets its bounds obligation), and loop
contracts (`#[invariant]`, `#[variant]`) attach to the loop and are checked as usual.

Deliberately absent: a bare `loop { }` (write `while true`) and a `break` or `continue` out of a
non-loop block. `break <expr>;` is still parsed; its value is evaluated and discarded, and a
non-Copy value is refused (bug 149), since no loop produces a value. History: before Phase 6D a
`while` in value position took its value from `break <expr>` or from its `else` block's trailing
expression, with E0222 for a disagreement between the two (`while-expression break type 'T' does
not match else type 'U'`); a bug once stored that value at the default `i64` width into a narrower
slot, fixed by coercing it to the result type (`tests/programs/loop_control/while_expr_*_i32.con`).
Phase 6D item 2 removed the form: a loop result is a mutable local assigned before `break;`
(`tests/programs/loop_break_result_*.con`). The surface is pinned by
`scripts/tests/check_loop_control.sh` (fixtures in `tests/programs/loop_control/`; ROADMAP old
Phase 6 #4, 2026-06-21).

The owner decided on 2026-10-29 (OD27) that `break <expr>` is refused at parse.
The current compiler still parses a value after `break` inside a `while` or `for` statement and drops it (bug 323).


The owner decided on 2026-10-29 (OD47) that the loops are `while` and a counted
range loop, and the C-style `for` is removed in every form (`for (init; cond; step)`,
`for (cond)` and the unconditional `for`). The counted loop is half-open, `for i from a until b`
running `a`, `a + 1`, ..., `b - 1` and nothing when `a >= b`; its index cannot be assigned in the
body, and its scope is the body; both bounds must have one integer type, which the index takes.
`from` and `until` are reserved (OD50). The grammar is LL(1): `for` IDENT `from` expr `until` expr block, the upper bound
being a restricted head expression so that `{` ends the head (SYN-4, OD16). In Core the counted loop
desugars to the one `while` form, so proofs keep a single loop construct, and the counted form
carries its termination measure, `b - i`. Why: a counted loop terminates without a proof and gives
a bounds check over its index something to eliminate; the C-style step had its own control flow,
which bugs 306 and 330 got wrong; and the old header was an LL(1) conflict. Austral and Ada have a
counted loop (inclusive), and Swift removed the C-style loop (SE-0007); `until` is half-open to avoid
off-by-one errors and because `to` reads as exclusive to some and inclusive to others. The current compiler still has
only the C-style forms (bug 331 implements the decision and migrates the corpus under exit 2q). The
two-untyped-literal bounds are refused (OD48), the termination measure is mathematical (OD49), and reverse, stepped and collection iteration remain absent (OD51).

The reviews of 2026-10-29 (Codex, Kimi and Grok) fixed the details this version states: the bounds
are evaluated once, left to right, before the first iteration, so a side effect in `b` happens
once; the comparison comes before the increment, so a loop whose `b` is its type's maximum never
increments past it and cannot overflow; `continue` runs the body's exit sequence (STMT-11) and then
advances, a labeled `continue` advancing only the loop it names, while `break` and `return` leave
without advancing; and the index cannot be assigned and is not visible after the loop. STMT-12
gives the counted loop its termination measure. The examples of this specification that use the C-style form migrate with the
corpus.

```con reject name=break-outside-loop-decided code=E0280 stage=check pair=labeled-break pending=R-0555 now=reject:E0280:check
fn main() -> u8 {
    break;
    return 0;
}
```

```con accept name=labeled-break exit=42
fn example_value() -> i64 {
    let mut out: i64 = 0;
    let mut i: i64 = 0;
    'outer: while i < 10 {
        i = i + 1;
        let mut j: i64 = 0;
        while j < 10 {
            j = j + 1;
            if j == 3 {
                continue 'outer;
            }
            if i == 8 {
                break 'outer;
            }
            out = out + 3;
        }
    }
    return out;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=break-with-value code=new stage=parse pair=break-without-value pending=bug-323 now=accept:1
fn example_value() -> i64 {
    let mut i: i64 = 0;
    while i < 3 {
        i = i + 1;
        break 42;
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 1 {
        return 1;
    }
    return 2;
}
```

```con accept name=break-without-value exit=1
fn example_value() -> i64 {
    let mut i: i64 = 0;
    while i < 3 {
        i = i + 1;
        break;
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 1 {
        return 1;
    }
    return 2;
}
```

```con reject name=c-style-for code=new stage=parse pair=counted-loop pending=bug-331 now=accept:42
fn example_value() -> i64 {
    let mut s: i64 = 0;
    for (let mut i: i64 = 0; i < 7; i = i + 1) {
        s = s + i;
    }
    return s + 21;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=for-condition-only code=new stage=parse pair=while-loop pending=bug-331 now=accept:42
fn example_value() -> i64 {
    let mut i: i64 = 0;
    for (i < 42) {
        i = i + 1;
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=unconditional-for code=new stage=parse pair=while-loop pending=bug-331 now=accept:42
fn example_value() -> i64 {
    let mut i: i64 = 0;
    for {
        i = i + 1;
        if i == 42 { break; }
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=while-loop exit=42
fn example_value() -> i64 {
    let mut i: i64 = 0;
    while i < 42 {
        i = i + 1;
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: i64 = 0;
    let hi: i64 = 7;
    let mut s: i64 = 0;
    for i from lo until hi {
        s = s + i;
    }
    return s + 21;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop-empty exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: i64 = 5;
    let hi: i64 = 5;
    let mut s: i64 = 42;
    for i from lo until hi {
        s = 0;
    }
    return s;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop-bounds-once exit=42 pending=bug-331 now=reject:E0001:parse
fn bump(c: &mut i64) -> i64 {
    *c = *c + 1;
    return 3;
}

fn example_value() -> i64 {
    let mut calls: i64 = 0;
    let lo: i64 = 0;
    let mut s: i64 = 0;
    for i from lo until bump(&mut calls) {
        s = s + 10;
    }
    return s + calls + 11;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=counted-loop-at-type-max exit=5 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: u8 = 250;
    let hi: u8 = 255;
    let mut n: i64 = 0;
    for i from lo until hi {
        n = n + 1;
    }
    return n;
}
fn main() -> u8 {
    if example_value() == 5 {
        return 5;
    }
    return 1;
}
```

```con accept name=counted-loop-reversed-bounds-empty exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: i64 = 9;
    let hi: i64 = 3;
    let mut s: i64 = 42;
    for i from lo until hi {
        s = 0;
    }
    return s;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop-continue-and-break exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: i64 = 0;
    let hi: i64 = 100;
    let mut s: i64 = 0;
    for i from lo until hi {
        if i == 2 {
            continue;
        }
        if i == 10 {
            break;
        }
        s = s + i;
    }
    return s - 1;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop-labeled-continue exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let zero: i64 = 0;
    let three: i64 = 3;
    let mut s: i64 = 0;
    'outer: for i from zero until three {
        for j from zero until three {
            if j == 1 {
                continue 'outer;
            }
            s = s + 14;
        }
    }
    return s;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=counted-loop-index-is-immutable code=new stage=check pair=counted-loop pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: i64 = 0;
    let hi: i64 = 3;
    let mut s: i64 = 0;
    for i from lo until hi {
        i = 5;
        s = s + i;
    }
    return s;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

<a id="STMT-7"></a>
#### STMT-7. `main` declares target grants and returns u8

**STMT-7@4.** The entry point is one module-level `main` returning only `u8`, whose parameters declare a subset of the target grants and whose `with(...)` declares permitted effects, every declared grant being explicitly consumed before normal return and the returned `u8` being the process exit status.

- Kind: refusal
- Uses: none
- Codes: E0310 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: the exit status is the `exit` behaviour of STMT-1 (R-0496)
- Evidence: tested

**Rationale.** The main-result decision fixes `u8` only. Authority rule 12 and the audit permit only declared target grants and require their explicit consumption; undeclared grants are never minted. Target grant schemas and lifecycle APIs are [undecided: see the target Core proposal](#core-grants).

```con reject name=main-with-parameter-decided code=E0310 stage=check pair=main-exit-status pending=R-0555 now=reject:E0310:check
fn main(argc: i64) -> u8 {
    return 0;
}
```

```con accept name=main-exit-status exit=7
fn main() -> u8 {
    return 7;
}
```

```con reject name=main-returns-int code=new stage=check pair=main-returns-u8 pending=bug-320 now=accept:1
fn main() -> i64 {
    if 42 == 0 { return 0; } return 1;
}
```

```con accept name=main-returns-u8 exit=42
fn main() -> u8 {
    return 42;
}
```

<a id="STMT-8"></a>
#### STMT-8. A trailing expression is the block's value

**STMT-8@1.** In a block that produces a value (an `if` expression's branch, a value `match` arm or a `let` initializer's branch), the last expression written without `;` is the block's value, while a function body produces its result only through `return`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The `;` decides whether an expression is a value or a discarded statement, so a
statement `match` whose arms happen to end in expressions is not a value (the fixed E0225 class).
A trailing `if` or `match` inside a value block is itself value-bearing when its branches end in
values. A braced block as an arbitrary expression (`let x = { ...; v };`) and an implicit trailing
return in a function body are not in the language.

The parser records the distinction on the statement itself (`Stmt.expr` carries `isValue`, true
only for a value block's trailing expression with no `;`, set in `parseExprBlock` and for a bare
`=> expr` arm), and every stage computes a block's type and value the same way: a trailing value
expression gives its type and value, a final `return` or `break` diverges, and anything else is
`Unit`. Because every construction site must set the flag, no site can miss it, and Lower's
`lastExprVal` reads the value only from a value expression, which removed a class of `store void`
and wrong-slot hazards (ROADMAP #36, 2026-06-20; `regress_stmt_match_arm_unit.con`). The formatter
prints a value expression without `;` and a statement with one.

```con accept name=trailing-values exit=42
fn pick(c: bool, k: i64) -> i64 {
    let v: i64 = if c {
        let t: i64 = k * 2;
        match k {
            0 => 1,
            _ => t
        }
    } else {
        0
    };
    return v;
}

fn example_value() -> i64 {
    return pick(true, 21);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="STMT-9"></a>
#### STMT-9. A function with a result type returns on every path

**STMT-9@1.** A function whose result type is not `Unit` cannot reach the end of its body: every path ends in a `return`, an abort or a loop that never exits.

- Kind: refusal
- Uses: none
- Codes: E0307 (check)
- Status: enforced
- Enforced by: Concrete.checkFn
- Semantics: none
- Obligation: every reachable exit returns a value of the result type (R-0499)
- Evidence: tested

**Rationale.** Function bodies have no trailing value expression (STMT-8), so a reachable end has
no value to return. Until bug 194 nothing checked this: the backend failed in `llvm-as` (a `ret
void` in a non-void function) while `--interp` ran the program. Check now refuses it with E0307,
using a reachability analysis over every statement of a block.

```con reject name=one-branch-returns code=E0307 stage=check pair=every-path-returns
fn f(c: bool) -> i64 {
    if c {
        return 1;
    }
}

fn example_value() -> i64 {
    return f(true) + 41;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=every-path-returns exit=42
fn f(c: bool) -> i64 {
    if c {
        return 1;
    }
    return 0;
}

fn example_value() -> i64 {
    return f(true) + 41;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="STMT-10"></a>
#### STMT-10. A test function is an entry point

**STMT-10@2.** A test function marked `#[test]` returns only `u8`, with zero denoting success and a nonzero result denoting failure.

- Kind: refusal
- Uses: none
- Codes: E0319 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkFn
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Revised OD146 follows `main`: tests return only `u8`.

```con reject name=test-with-parameter-decided code=E0319 stage=check pair=test-shape-decided pending=R-0555 now=reject:E0319:check
#[test]
fn t(x: u8) -> u8 {
    return x;
}

fn main() -> u8 {
    return 42;
}
```

```con accept name=test-shape-decided exit=42 pending=R-0555 now=reject:E0319:check
#[test]
fn t() -> u8 {
    return 0;
}

fn main() -> u8 {
    return 42;
}
```

<a id="STMT-11"></a>
#### STMT-11. One exit sequence

**STMT-11@3.** Every non-abort scope exit first evaluates its exit value, then runs deferred calls in reverse registration order from innermost scope outward, then ends the loans and tears down local storage without invoking an implicit `Destroy`, while a trapping deferred call stops the sequence.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Semantics: none
- Obligation: STMT-1 (R-0496)
- Evidence: tested

**Rationale.** OD2 fixes the exit order, OD68 stops cleanup after a trapping deferred call, and OD69 makes the final step storage teardown rather than an implicit Destroy; all linear obligations were explicitly discharged already.

<a id="STMT-12"></a>
#### STMT-12. A counted loop carries its own measure

**STMT-12@2.** In Core a counted loop is the one `while` form, carrying the proof-only mathematical-integer termination measure `b - i` that the loop gives itself, refusing an explicit `#[variant]` and checking its invariant before the loop and after each iteration.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Semantics: none
- Obligation: the counted loop's measure decreases on every iteration (R-0496)
- Evidence: tested

**Rationale.** Decided with OD47 on 2026-10-29: proofs keep a single loop construct. The measure
proves only that the number of iterations is finite; the loop terminates when its bounds, its body
and its cleanup do, and the counted loop's proof rule (zero iterations, `break`, a labelled
`continue`) is stated over this desugared loop. OD104 refuses an explicit variant; OD105 checks invariants before the loop and after every iteration. Removing a bounds check needs an in-range certificate at the access (the bounds against the array's length), not only a length that cannot change. OD49 makes the measure a mathematical integer in the proof only, never computed at runtime.

```con accept name=counted-loop-own-measure exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: i64 = 0;
    let hi: i64 = 7;
    let mut s: i64 = 0;
    for i from lo until hi {
        s = s + i;
    }
    return s + 21;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="STMT-13"></a>
#### STMT-13. `abort()` needs `Process`

**STMT-13@2.** The builtin `abort()` and checked traps need neither a resource value nor a permission, while target diagnostics and output flushing are recorded in traces and target assumptions.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.IntrinsicId.capability
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The 2026-11-02 audit clarifies that pure computation and terminal traps are outside the resource-operation requirement; target-side observable effects remain accounted for.

```con accept name=abort-without-permission exit=42 pending=R-0555 now=reject:E0520:core-check
fn main() -> u8 {
    if 1 == 2 {
        abort();
    }
    return 42;
}
```

### Recoverable failure: explicit `Result` flow

This section is explanatory; it records a design decision and the shape of programs under it. A
recoverable failure is a `Result<T, E>` value. There are no exceptions, no unwinding, no implicit
error conversion and no propagation operator: every propagation edge is an exhaustive `match` whose
error arm contains the `return` that leaves the function. A fatal failure is an abort (STMT-2).

**Postfix `?` was removed (2026-09-08).** It compressed a match and an early return into
punctuation at an expression site, which saved characters but made a reviewer reconstruct which
call can exit the function, which error type leaves it, whether a conversion happens, which
deferred cleanup runs and whether a refactor moved or duplicated a linear value. For an
audit-first language that trade is not worth it: generated explicit arms are cheap, review is the
scarce resource. The lexer still recognizes `?`, only so the parser can refuse it with a migration
diagnostic, and the retired diagnostics stay reserved.

Same-error propagation:

```con pseudocode
fn parse_header(data: [i32; 8]) -> Result<Header, ParseError> {
    let version: i32 = match validate_version(data[0]) {
        Result::Ok { value } => value,
        Result::Err { error } => {
            return Result::<Header, ParseError>::Err { error: error };
        },
    };
    return Result::<Header, ParseError>::Ok { value: Header { version: version } };
}
```

Cross-error propagation converts visibly in the error arm
(`return Result::<Response, ServiceError>::Err { error: to_service_validation(error) };`). `map_err`
is an ordinary named function: it creates no hidden return, and its caller still matches the
mapped value.

An error arm's `return` is a normal return: the deferred calls of the scopes it leaves run in the
same order as for any return (OWN-12), while an abort runs none. Calling a fallible function uses
the capabilities it declares; propagating its result needs no capability, allocation or
conversion. A `Result` obeys the ordinary conservation rules: the match consumes it, the selected
payload becomes a new binding, neither arm may drop the other's linear payload, and returning a
payload moves it once; the checker and Core have no propagation case.

The library's helper floor is ordinary functions, not control flow: `is_ok`, `is_err` (borrowed
receiver), `unwrap_or_else`, `map`, `map_err`, `and_then` (explicit function pointers, since there
are no closures, implicit trait resolution or `From` conversion), and `unwrap_or`, `ok`, `err`
(Copy bounds where the other arm is discarded) on `Result`; `is_some`, `is_none`,
`unwrap_or_else`, `map`, `and_then` on `Option`. A helper that returns a `Result` never propagates it
out of its caller. Parsers keep a flat named error enum, with each failure next to its check;
services convert stage errors in a visible arm at the stage boundary. A long run of similar arms is
not evidence that syntax is needed: first prefer a narrower named validation function, a combinator
where it improves the data flow, one match around a multi-step function, or reviewed source
generated outside the language.

Rejected: postfix `?` (permanently), implicit error conversion (hidden conversion lookup),
exceptions and unwinding (excluded by STMT-2), try/catch blocks (a second failure-control model),
result-specific statement syntax (duplicates `match`), closure-taking combinators (closures are
excluded, TYPE-1). These hold only while: `?` is refused with its removal message; neither the
surface AST nor Core has a propagation node; no stage (Check, Elab, Mono, lowering, the
interpreter, proof extraction, evidence identity) has a propagation path; same-type and
cross-type explicit propagation compile and run; explicit error returns run `defer` cleanup in
nested scopes; ignored fallible results stay refused (OWN-6); and the documentation, the book, the
site, tests and examples show the exhaustive match and the explicit return.

### Abort and failure

STMT-2 distinguishes ordinary error returns from terminal traps. There is no exception unwinding.

| situation | response | deferred calls run? |
|---|---|---|
| normal return or explicit error return | ordinary control flow, preserving every ownership obligation | yes |
| break, continue or end of a block | exit the scopes left | yes |
| program allocation failure | ordinary conserving error; allocator valid and inputs returned | on subsequent structured exit |
| checked trap or explicit abort | terminal, no cleanup | no |
| stack, runtime or environment exhaustion | separately stated target premises | no language cleanup promise |

Every non-abort exit follows STMT-11: evaluate the exit value, run deferred calls innermost first
and last registered first, end loans, then tear down storage without implicit Destroy (OD69).
A trapping deferred call stops the sequence (OD68). A write through `&mut` already changes its
place, so there is no write-back step (REF-13). `abort()` needs no permission (STMT-13).
The legacy runtime's null-allocation abort is a disagreement with the Allocation decision, not
an alternative language rule or a workload-gated recovery feature.

**Traps in the current compiler.** These are non-normative observations of the run-time checks that
ended a program, on the native binary and under `--interp`, on 2026-10-31. In each probe the operands
reached the operation through a function parameter, so the compiler could not evaluate them. Every
native run below exited with status 134 (SIGABRT on Linux), which agrees with the paragraph above:
the trap helpers call libc `abort()`. The interpreter stops with a run-time error whose first line is
shown, without its `interp: ` prefix; the `trap=` attribute of an abort example (§2.2 of
[MAINTAINERS.md](MAINTAINERS.md)) is matched against that line.

| operation probed | operands | related rule | native | `--interp` stops with |
|---|---|---|---|---|
| `+` on `i32` | 2147483647 and 1 | ARITH-3 | status 134 | `arithmetic overflow (checked +)` |
| `*` on `u8` | 16 and 16 | ARITH-3 | status 134 | `arithmetic overflow (checked *)` |
| `-` on `i32` | 0 and the minimum, -2147483648 | ARITH-3 | status 134 | `arithmetic overflow (checked -)` |
| `/` on `i32` | 7 and 0 | ARITH-3 | status 134 | `division by zero` |
| `/` on `i32` | -2147483648 and -1 | ARITH-3 | status 134 | `arithmetic overflow (checked /)` |
| `%` on `i32` | 7 and 0 | ARITH-3 | status 134 | `modulo by zero` |
| `<<` on `i32` | 1 and -1 | ARITH-3 | status 134 | `shift amount out of range` |
| `<<` on `u8` | 1 and 8 | ARITH-3 | status 134 | `shift amount out of range` |
| `f64` `as i32` | NaN (0.0 / 0.0) | ARITH-5 | status 134 | `float-to-integer conversion of an infinity or NaN (the binary aborts)` |
| `f64` `as i32` | 100000000000.0 | ARITH-5 | status 134 | `float-to-integer conversion out of range for 'i32' (the binary aborts)` |
| `a[i]` on `[u8; 3]` | `i` = 5 | MEM-6 | status 134 | `array index 5 out of bounds (size 3)` |
| `abort()` in a `main` declared `with(Process)` | none | STMT-2 | status 134 | `undefined function 'abort'` |

The recorded answers in [OD97](#od97) govern this subject; the frozen compiler’s earlier behavior is not an alternative rule.

**Recoverable failures** use values and exhaustive matching. Allocation failure conserves its
input, failed growth preserves the container and returns the rejected element, and release retains
every surviving obligation. Exact failure payloads are
[undecided: see the target Core proposal](#core-allocation-contracts).

**Hardware traps** are outside the language model: a null dereference (only in trusted code) and a
stack overflow raise SIGSEGV, and an illegal instruction (SIGILL) should not come from correct code
generation. Checked arithmetic and safe indexing are not among them: the compiler inserts a
language-defined abort before an invalid operation. **Undefined behaviour** needs trusted or
`Unsafe` code or a dishonest foreign boundary (TRUST-2): a trusted unchecked pointer or index access
can read or write arbitrary memory, a dishonest `extern` signature can corrupt the ABI or memory, and
an invalid raw-pointer lifetime or alias causes use after free; the mitigation is a named, audited
boundary.

On every normal path (including explicit error returns) all deferred calls run, every linear value
is consumed (OWN-2), each scope's loans end and its local storage is torn down without implicit Destroy in STMT-11's order, and the frame is reclaimed; predictable
code has no heap, so the deferred call is its only cleanup. On an abort, the resources of the
aborting function and of its callers get no language cleanup.

At the foreign boundary, a linear value passed by value to an `extern fn` is consumed, a reference
argument is borrowed for the call, and a return value has its declared type, trusted and not
verified. Nothing checks that a foreign function follows its declared contract, nothing cleans up
when it calls `longjmp` or `exit` or raises a signal, raw pointers it returns are not tracked, and it
is not assumed deterministic or pure. If it aborts or raises a signal, no deferred call runs and the
linear values in scope are leaked to the OS; `longjmp` across Concrete frames is undefined (no
setjmp-safe code is emitted). A proved function calls no `extern fn` (the proof-eligibility FFI
gate), so foreign failure is outside the proof model; what proved code may and may not assume about
failure is in [PROOF_SEMANTICS_BOUNDARY.md](../verification/PROOF_SEMANTICS_BOUNDARY.md).

### Program execution

This section is explanatory and states no rule. It follows a program from its source files to a
running process and names, for each step, the entries that govern it; what those entries say is in
the entries, not here. Where this section reports what the current compiler does, it says so and
names the probe. The runtime and platform side (the generated C entry, libc, targets) is described
in [EXECUTION_MODEL.md](../platform/EXECUTION_MODEL.md).

| step | governing entries and what this section records | entries |
|---|---|---|
| source | MOD-4 covers how `mod` declarations load module files, SYN-5 the encoding of source text and SYN-1 the grammar a program is a sentence of. | MOD-4, SYN-5, SYN-1 |
| names | MOD-1 and MOD-3 cover name resolution and the names a module binds. The [builtin catalog](#builtin-functions) lists the current compiler's builtin functions, with the sources and probes of its observations; every probe there calls builtins with no import, and this section records no other probe of name resolution. | MOD-1, MOD-3 |
| compilation | [pipeline.md](#ch-pipeline) describes the stages: Parse, Resolve, Check, Elab, CoreCheck, Mono, Lower, Emit and link. The entries for the program after a stage are PASS-5 (Check), PASS-6 (Elab), PASS-7 (Mono), PASS-8 (CoreCheck), PASS-9 (Lower) and PASS-10 (Emit); PASS-1 and PASS-2 cover specialization and PASS-11 the backend IR. PASS-12 (partial) covers the compiling of a program every rule accepts. The current compiler can fail after Check on a checked program: the builtin catalog records such failures for `unwrap`, `abort` and `get_args` (bugs 345 to 347). | pipeline.md, PASS-1, PASS-2, PASS-5 to PASS-11, PASS-12 (partial) |
| entry point | STMT-7 (partial) covers `main` and its exit status, and PASS-10 an executable's entry point. | STMT-7 (partial), PASS-10 |
| start | TRUST-8 (pending) covers module-level state and TYPE-22 the evaluation of constants. [EXECUTION_MODEL.md](../platform/EXECUTION_MODEL.md) describes the generated C entry. This section records no probe of start-up, of the generated C entry or of constant evaluation. | TRUST-8 (pending), TYPE-22 |
| authority | TRUST-4 covers the capabilities of `main` on the same terms as every other function. TRUST-9 and TRUST-10 are pending, and authority rule 12 governs the target grants `main` declares and consumes; no Root type exists. STMT-7 (partial) states the entry-point signature. Every probe below uses a `main` with no parameters; this section records no probe of a `main` that declares parameters. | TRUST-4, TRUST-9 (pending), TRUST-10 (pending), OPEN-38, STMT-7 (partial) |
| running | STMT-3 and STMT-4 cover threads and evaluation order, and STMT-1 (pending) what a behaviour is. | STMT-3, STMT-4, STMT-1 (pending) |
| normal exit | STMT-11 (pending) covers the exit sequence of a scope, and STMT-7 (partial) the exit status. The paragraph below records the status the current compiler produces; the order of the exit sequence was not probed for this section. | STMT-11 (pending), STMT-7 (partial) |
| failure | STMT-2 (pending) covers failure, and STMT-13 `abort()`. "Abort and failure" above records the current compiler's traps and abort paths, and the table below what `--interp` does instead. | STMT-2 (pending), STMT-13 |
| tests | STMT-10 covers `#[test]` functions under `concrete test`. This section records no probe of `concrete test`. | STMT-10 |

**The exit status today.** STMT-7 is partial (bug 320): its pending example records that the
current compiler still accepts result types other than those STMT-7 names. Probes of the native
binary on Linux x86-64, at f17645a13 on 2026-10-31 and again at 904c7cc87: a `main` declared
`-> u8` that returns 7 exits with status 7; a `main` with no result type exits with status 0; a
`main` declared `-> i64` that returns 300 exits with status 44; a `main` declared `-> bool` that
returns `true` exits with status 1.

**What `--interp` does differently today.** PASS-13 is the entry for what every consumer gives a
checked program. The rows below are probes of single-file programs, run on 2026-10-31 at f17645a13
and again at 904c7cc87. "Status" is the process exit status as the shell
reported it on Linux x86-64, where a process ended by SIGABRT is reported as 134.

| situation | native binary | `--interp` |
|---|---|---|
| `main` returns a `u8` (7, after printing `hello`) | prints `hello`, status 7 | prints `hello`, then `7` as the last line, status 0; STMT-7 states that the exit status is that `u8`, with nothing written by the runtime, and PASS-13 that every consumer gives the same result, output and traps; no bug record covers the `u8` case, and bug 320 records the same printing for an `i64` result and names `Interp.interpret` as its site |
| `main` returns `i64` 300 | status 44 (bug 320) | prints `300`, status 0 (bug 320) |
| `main` returns `bool` `true` | status 1 (bug 320) | prints nothing (as bug 320's record reports for a `bool` result), status 0 |
| a division by zero at `i32`: `main` calls `fn d(a: i32, b: i32) -> i32 { return a / b; }` as `d(7, 0)` | ended by SIGABRT, status 134 | prints `interp: division by zero`, status 1 |
| `abort();` as a statement in a `main` declared `with(Process)` | ended by SIGABRT, status 134 | prints `interp: undefined function 'abort'`, status 1 (bug 344) |
| `clock_monotonic_ns()` in a `main` declared `with(Time)` | runs, status 42 | prints `interp: clock_monotonic_ns reads the system clock, a value the interpreter cannot know; run the compiled binary`, status 1 |
| `float_to_string(1.5)` | runs, status 42 | prints `interp: undefined function 'float_to_string'`, status 1 (bug 348) |

Whether `--interp` runs a program of several files or a project with dependencies was not probed
for this section. The traps the current compiler reports are listed under "Traps in the current
compiler" above.

### Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- STMT-6@2 records OD27 (2026-10-29): `break <expr>` is refused at parse. STMT-6@3 records OD47 (2026-10-29): the loops are `while` and the counted loop, and the C-style `for` is removed.
- STMT-7@2 masked an integer result to 0–255 and mapped `bool` (stage 1 of MAIN_EXIT_MODEL). STMT-7@3 records OD24 (2026-10-29): `main` returns `u8` or `Unit`.
- STMT-11@1 (retired before it was enforced, as the registry records) said of the C-style `for` step, which OD47 removes, what STMT-11@2 says of the counted loop of OD47.

- STMT-2@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; STMT-2@1 is superseded by version.
- STMT-4@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; STMT-4@1 is superseded by version.
- STMT-5@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; STMT-5@1 is superseded by version.
- STMT-7@4 (2026-11-03) brings the chapter into line with the recorded owner decisions; STMT-7@3 is superseded by version.
- STMT-10@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; STMT-10@1 is superseded by version.
- STMT-11@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; STMT-11@2 is superseded by version.

- STMT-13@2 (2026-11-03) supersedes STMT-13@1 to state the recorded decision.

- STMT-6@4 (2026-11-03) supersedes STMT-6@3; pending legacy conformance is explicit.


<a id="ch-patterns"></a>
## Patterns

Patterns destructure one level in explicit `match` arms and irrefutable destructuring `let`:
variants, structs, literals, bindings, `_` and alternatives. Guards, ranges, `if let`, `while let`
and `let ... else` are refused (OD52, OD55, OD56). Borrowed scrutinees follow REF-9 and ignored
fields follow OWN-5, including its Result/Option restrictions.

Two things are deliberately not in the language. **Nested patterns** (`E::Wrap { P { x, y } }`)
are deferred until a workload needs them; one level of destructuring plus a field access or an
inner `match` expresses the same programs ([No nested patterns](#no-nested-patterns),
`check_nested_patterns.sh`). **Tuples** do not exist; the named struct is the one product type
([types.md, "No anonymous tuples"](#no-anonymous-tuples), `check_no_tuples.sh`). The design records are
[PATTERN_ERGONOMICS.md](../archive/design-records/PATTERN_ERGONOMICS.md) and
[PATTERN_DESTRUCTURING.md](../archive/design-records/PATTERN_DESTRUCTURING.md) (absorbed into PAT-10
and PAT-11 on 2026-10-30).

### Rules

<a id="PAT-1"></a>
#### PAT-1. A pattern field may be bound under another name

**PAT-1@2.** A struct or enum pattern in a `match` or an irrefutable destructuring `let` may bind a field under another name with `field: binder`, and the formatter keeps the label.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.parseBindingList
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Renaming avoids shadowing under SCOPE-1; OD52 removes conditional and refutable destructuring sugar without changing field labels.

```con accept name=nested-option exit=42
fn nested(o: Option<Option<i64>>) -> i64 {
    match o {
        Option::Some { value: outer } => {
            match outer {
                Option::Some { value: inner } => { return inner; },
                Option::None => { return 0; }
            }
        },
        Option::None => { return 0; }
    }
}

fn example_value() -> i64 {
    return nested(Option::Some { value: Option::Some { value: 42 } });
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_2i_renamed_pattern_binder.con` | `run:42` | `-` | `binary,interp,fmt` |

<a id="PAT-2"></a>
#### PAT-2. A match is exhaustive

**PAT-2@3.** A `match` covers every value of its scrutinee: an enum covers every variant or has a catch-all, `bool` covers both literals or has a catch-all, and every other type requires a catch-all even when every integer literal is listed, with guards and range patterns refused.

- Kind: refusal
- Uses: none
- Codes: E0530 (core-check), E0534 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: a checked match never falls off its last arm (R-0507)
- Evidence: tested

**Rationale.** OD55 and OD56 remove guards and ranges; exhaustive enum and boolean coverage remains structural, and an integer match always needs a catch-all.

```con reject name=missing-variant-decided code=E0530 stage=core-check pair=every-variant pending=R-0555 now=reject:E0530:core-check
enum Copy E { A, B, C }

fn f(e: E) -> i64 {
    match e {
        E::A => { return 1; },
        E::B => { return 2; }
    }
}

fn example_value() -> i64 {
    return f(E::A);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=no-default-arm-decided code=E0534 stage=core-check pair=every-variant pending=R-0555 now=reject:E0534:core-check
fn main() -> u8 { let k: u8 = 3; match k { 1 => { return 1; }, 3 => { return 42; } } }
```

```con accept name=every-variant exit=42
enum Copy E { A, B, C }
fn f(e: E) -> u8 {
    match e { E::A => { return 42; }, E::B => { return 1; }, E::C => { return 0; } }
}
fn main() -> u8 { return f(E::A); }
```

<a id="PAT-3"></a>
#### PAT-3. Every arm fits its scrutinee

**PAT-3@4.** A variant pattern names a variant of the scrutinee’s enum and lists every field, ignoring one only explicitly with an eligible wildcard or `Variant { .. }`, a bare variant name is legal only for a fieldless variant, a literal pattern has the scrutinee’s type and no arm repeats a covered variant, while guards and ranges are refused.

- Kind: refusal
- Uses: none
- Codes: E0304 (check), E0309 (check), E0531 (core-check), E0532 (core-check), E0533 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: a checked match never inspects a value its pattern does not describe (R-0507)
- Evidence: tested

**Rationale.** OD25 requires explicit ignored fields; OD55 and OD56 remove guards and ranges. Only eligible values may be ignored (OWN-5).

```con reject name=duplicate-arm-decided code=E0532 stage=core-check pair=arms-fit pending=R-0555 now=reject:E0532:core-check
enum Copy E { A, B }

fn main() -> u8 {
    let e: E = E::A;
    match e {
        E::A => { return 1; },
        E::A => { return 2; },
        E::B => { return 0; }
    }
}
```

```con reject name=missing-payload-field-decided code=E0533 stage=core-check pair=arms-fit pending=R-0555 now=reject:E0533:core-check
enum Copy E { A { x: i64, y: i64 }, B }

fn example_value() -> i64 {
    let e: E = E::A { x: 1, y: 2 };
    match e {
        E::A { x } => { return x; },
        E::B => { return 0; }
    }
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=other-enum-variant-decided code=E0309 stage=check pair=arms-fit pending=R-0555 now=reject:E0309:check
enum Copy E { A, B }
enum Copy F { X, Y }

fn main() -> u8 {
    let e: E = E::A;
    match e {
        F::X => { return 1; },
        _ => { return 0; }
    }
}
```



```con reject name=same-name-other-enum-decided code=E0309 stage=check pair=arms-fit pending=R-0555 now=reject:E0309:check
enum Copy E { A, B }
enum Copy F { A, X }

fn main() -> u8 {
    let e: E = E::B;
    match e {
        F::A => { return 1; },
        _ => { return 42; }
    }
}
```

```con accept name=arms-fit exit=42
enum Copy E { A { x: i64, y: i64 }, B }

fn example_value() -> i64 {
    let e: E = E::A { x: 40, y: 2 };
    match e {
        E::A { x, y } => { return x + y; },
        E::B => { return 0; }
    }
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=bare-variant-with-payload code=new stage=check pair=variant-field-ignored pending=bug-321 now=accept:42
enum Copy E { A { v: i64 }, B }
fn main() -> u8 {
    let e: E = E::A { v: 1 };
    match e {
        E::A => { return 42; },
        E::B => { return 0; },
    }
}
```

```con accept name=variant-field-ignored exit=42
enum Copy E { A { v: i64 }, B }
fn main() -> u8 {
    let e: E = E::A { v: 1 };
    match e {
        E::A { v: _ } => { return 42; },
        E::B => { return 0; },
    }
}
```

<a id="PAT-4"></a>
#### PAT-4. Match guards are refused

**PAT-4@3.** Match guards are refused; a conditional test is written inside the matching arm or in an `if` chain.

- Kind: refusal
- Uses: OWN-D1
- Codes: new (parse)
- Status: pending R-0555
- Enforced by: Concrete.checkGuard
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** OD55 removes the guard form entirely, so there is no guard commitment or move rule.



```con accept name=guard-reads exit=42
fn peek(r: &i64) -> bool { return *r > 100; }
fn main() -> u8 {
 let k: i64 = 1;
 match k { 1 => { if peek(&k) { return 1; } return 42; }, _ => { return 0; } }
}
```

```con reject name=guard-removed code=new stage=parse pair=test-inside-arm pending=R-0555 now=accept:42
fn main() -> u8 { let n: u8 = 1; match n { 1 if true => { return 42; }, _ => { return 0; } } }
```

```con accept name=test-inside-arm exit=42
fn main() -> u8 { let n: u8 = 1; match n { 1 => { if true { return 42; } return 0; }, _ => { return 0; } } }
```

<a id="PAT-5"></a>
#### PAT-5. A destructuring `let` binds every field

**PAT-5@4.** A destructuring `let` consumes its scrutinee, names every field with ignored fields explicitly marked by eligible wildcards, and has an irrefutable pattern, while refutable destructuring and `let ... else` are refused and use `match` instead.

- Kind: refusal
- Uses: none
- Codes: new (parse), E0252 (check), E0308 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkStmt, Concrete.checkExprCore
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** OD52 and OD85 require an explicit match for refutable destructuring; OD25 requires all fields, with eligible ignored fields marked explicitly.

```con reject name=struct-field-missing-decided code=E0252 stage=check pair=destructure-all pending=R-0555 now=reject:E0252:check
struct P { a: i64, b: i64 }

fn example_value() -> i64 {
    let p: P = P { a: 1, b: 2 };
    let P { a } = p;
    return a;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```



```con accept name=destructure-all exit=42
struct P { a: i64, b: i64 }
fn main() -> u8 { let p: P = P { a: 40, b: 2 }; let P { a, b } = p; if a + b == 42 { return 42; } return 1; }
```

```con reject name=struct-let-omits-field code=E0252 stage=check pair=struct-let-ignores-field pending=bug-321 now=accept:42
struct Copy P { a: i64, b: i64 }
fn example_value() -> i64 {
    let p: P = P { a: 42, b: 1 };
    let P { a } = p;
    return a;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=struct-let-ignores-field exit=42
struct Copy P { a: i64, b: i64 }
fn example_value() -> i64 {
    let p: P = P { a: 42, b: 1 };
    let P { a, b: _ } = p;
    return a;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=refutable-let-else-removed code=new stage=parse pair=destructure-all pending=R-0555 now=accept:42
fn main() -> u8 { let o: Option<u8> = Option::Some { value: 42 }; let Option::Some { value } = o else { return 0; }; return value; }
```

<a id="PAT-6"></a>
#### PAT-6. Conditional destructuring sugar is refused

**PAT-6@3.** `if let` and `while let` are refused; conditional destructuring uses an explicit `match` and repetition uses `while`.

- Kind: refusal
- Uses: none
- Codes: new (parse)
- Status: pending R-0555
- Enforced by: Concrete.parseStmt
- Semantics: none
- Obligation: the desugaring is part of the elaboration relation (R-0496)
- Evidence: tested

**Rationale.** OD52 removes both forms; explicit matches expose every branch and its ownership obligations.


```con reject name=if-let-removed code=new stage=parse pair=explicit-match pending=R-0555 now=accept:42
fn main() -> u8 { let o: Option<u8> = Option::Some { value: 42 }; if let Option::Some { value } = o { return value; } return 0; }
```

```con accept name=explicit-match exit=42
fn main() -> u8 { let o: Option<u8> = Option::Some { value: 42 }; match o { Option::Some { value } => { return value; }, Option::None => { return 0; } } }
```

<a id="PAT-7"></a>
#### PAT-7. Alternatives remain; ranges and guards are refused

**PAT-7@2.** Alternative patterns `P1 | P2 => body` mean consecutive arms `P1 => body` and `P2 => body` in that order, while integer range patterns and match guards are refused.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.parseStmt, Concrete.ccCheckExpr
- Semantics: none
- Obligation: range and guard patterns are modelled in the proof path (they are reported unsupported today)
- Evidence: tested

**Rationale.** OD55 and OD56 remove guards and integer intervals; alternatives remain separate consecutive arms and must bind the names their body uses.

```con accept name=ranges-and-alternatives exit=42
fn main() -> u8 { let k: u8 = 7; match k { 1 | 7 => { return 42; }, _ => { return 1; } } }
```

<a id="PAT-8"></a>
#### PAT-8. C-style loop headers are refused

**PAT-8@2.** C-style `for` headers, including destructuring initializers, are refused; a counted loop binds its immutable index only in its body.

- Kind: refusal
- Uses: none
- Codes: new (parse)
- Status: pending R-0555
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD47 and OD61 remove the entire C-style header, rather than retaining a special rule about its initializer.



```con accept name=destructure-before-loop exit=42
enum Copy E { A { v: i64 } }
fn main() -> u8 { let e: E = E::A { v: 42 }; let E::A { v } = e; if v == 42 { return 42; } return 1; }
```

```con reject name=c-header-removed code=new stage=parse pair=while-replacement pending=R-0555 now=accept:42
fn main() -> u8 { for (let mut i: u8 = 0; i < 1; i = i + 1) { if i == 0 { return 42; } } return 0; }
```

```con accept name=while-replacement exit=42
fn main() -> u8 { let mut i: u8 = 0; while i < 1 { if i == 0 { return 42; } i = i + 1; } return 0; }
```

<a id="PAT-9"></a>
#### PAT-9. The first matching arm wins

**PAT-9@2.** A `match` selects the first matching arm in source order and refuses an arm covered by earlier arms, while every integer match requires a catch-all that listing integer literals never makes unreachable.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0555
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD18 fixes source order; OD40 and OD41 no longer need desugaring or interval exceptions because OD52 and OD56 removed those forms.

```con reject name=arm-after-catch-all code=new stage=check pair=catch-all-last pending=R-0500 now=accept:42
fn main() -> u8 {
    let x: i64 = 1;
    match x {
        _ => {
            return 1;
        },
        1 => {
            return 42;
        },
    }
}
```

```con accept name=catch-all-last exit=42
fn main() -> u8 {
    let x: i64 = 1;
    match x {
        1 => {
            return 42;
        },
        _ => {
            return 1;
        },
    }
}
```

<a id="PAT-10"></a>
#### PAT-10. Irrefutable enum destructuring matches the rest of its block

**PAT-10@2.** An irrefutable enum destructuring `let P = e;` means a one-arm `match` over the rest of its block, with binders visible only there, while a refutable pattern or `let ... else` is refused.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.desugarStmts
- Semantics: none
- Obligation: the desugaring is part of the elaboration relation (R-0496)
- Evidence: tested

**Rationale.** OD52 removes the fallback form; an irrefutable one-arm match preserves binder scope and explicit consumption.

```con accept name=let-else-and-irrefutable exit=42
enum Copy One { A { v: u8 } }
fn main() -> u8 { let one: One = One::A { v: 42 }; let One::A { v } = one; return v; }
```

<a id="PAT-11"></a>
#### PAT-11. `let mut` makes every binder of a pattern mutable

**PAT-11@2.** An irrefutable destructuring `let mut P = e` declares every binder of `P` mutable, and no destructuring `let` has an `else` block.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkStmt, Concrete.checkExprCore, Concrete.desugarStmts
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** One `mut` applies to each binder; OD52 removes the refutable else form, and immutable binders remain protected by OWN-10.

```con accept name=let-mut-pattern exit=42
struct Copy P { a: u8, b: u8 }
fn main() -> u8 { let p: P = P { a: 40, b: 1 }; let mut P { a, b } = p; a = a + 1; b = b + 0; return a + b; }
```

```con accept name=let-mut-variant-without-else exit=42
enum Copy One { A { v: i64 } }

fn example_value() -> i64 {
    let o: One = One::A { v: 41 };
    let mut One::A { v } = o;
    v = v + 1;
    return v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

### Absent by decision

This section is explanatory: it records pattern forms the language does not have, why, and what
would reopen them. The grammar (SYN-1) is what refuses them.

#### No nested patterns

Deferred, not in V1 (decided 2026-06-22, ROADMAP old Phase 6 #5, the last open pattern item),
pinned by `scripts/tests/check_nested_patterns.sh`. A pattern destructures exactly one level: an
arm binds each field of a variant or struct to one name, and the value at that field is bound
whole. `E::Wrap { P { x, y } }` and `Some(Some(n))` are parse errors. A deeper value is reached in a
second step, by a field access or an inner `match`:

```con pseudocode
match e { E::Wrap { p } => use(p.x, p.y) }

match outer {
    Option::Some { value } => match value {
        Option::Some { value: inner } => use(inner),
        Option::None => fallback(),
    },
    Option::None => fallback(),
}
```

This is more verbose and exactly as expressive: no program written with nested patterns lacks a
one-level equivalent. The feature is deferred because it is a large cross-cutting change for sugar
(the flat binder list would become a recursive pattern type, threaded through parsing, resolution,
checking, elaboration, lowering with recursive field extraction, and the proof path, the heaviest
change of the pattern-ergonomics work), and because it is workload-driven, not symmetry-driven: no
example or workload is blocked by the one-level limit, since decoders, parsers and interpreters are
served by explicit matches, comparisons, alternatives, matching on `&T` and one-level destructuring. The same call was made for const generics
and tuples ([types.md](#no-anonymous-tuples)). Nested patterns are built when a real
workload shows the one-level form untenable (for example deep protocol syntax trees where the
inner matches dominate the code), as a recursive pattern type with recursive destructuring in
check, elaboration and lowering.

#### No bare variant names and no parameter patterns

A variant is always written with its enum, `Option::Some { value }`, never `Some(x)`: a bare name
would make the parser depend on name resolution to know which enum it belongs to. A function
parameter is a name with a type, never a pattern; the body destructures it.

### Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- PAT-3@3 records OD25 (2026-10-29): ignoring a payload is always written.
- PAT-5@3 records OD25 (2026-10-29): a struct `let` names every field.

- PAT-1@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-1@1 is superseded by version.
- PAT-2@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-2@2 is superseded by version.
- PAT-3@4 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-3@3 is superseded by version.
- PAT-4@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-4@2 is superseded by version.
- PAT-5@4 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-5@3 is superseded by version.
- PAT-6@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-6@2 is superseded by version.
- PAT-7@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-7@1 is superseded by version.
- PAT-8@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-8@1 is superseded by version.
- PAT-9@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-9@1 is superseded by version.
- PAT-10@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-10@1 is superseded by version.
- PAT-11@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-11@1 is superseded by version.


<a id="ch-ownership"></a>
## Ownership

Concrete is **linear**: a non-Copy value is used exactly once and never silently disappears.
"Used" means moved, consumed, returned or handed off. There is no implicit destructor and no
catch-all discard. If a value is not Copy, the checker must see where it goes. Three concepts are
kept apart: **Copy** (may it be duplicated?), **linear**, meaning every non-Copy value (must it be
accounted for exactly once?), and **Destroy** (is there an explicit consuming cleanup verb?). A
permit or proof token owns nothing and has no `Destroy`, yet it is linear and must be handed off.
`&mut T` owns no resource, but its bound name must still be used or `_` (REF-10, OD62).

**Why linear, not affine** (decided 2026-07-01). The language had drifted into two predicates for
"may this disappear": a named binding used "not Copy" (linear), while the `_` wildcard used "owns a
resource" (affine), so `match e { _ => {} }` dropped an `Option<i32>` or a plain view struct that the
same value bound to a name had to consume. Of the three coherent fixes, Copy-centric (drop iff Copy)
needed conditional Copy for generics, and resource-centric (must consume iff it owns a resource)
could not express a token that owns no memory yet must not vanish, which a capability language
needs; the linear rule was chosen: a non-Copy value is used exactly once, `_` ignores only Copy
values, one law and no implicit weakening. In substructural terms Copy is contraction and
discardability is weakening, and a linear type permits neither; the precedents are Austral
(explicit linearity, no implicit `Drop`) and Linear Haskell. Unlike Rust, affine with an implicit
`Drop`, Concrete makes cleanup explicit, so it refuses silent disappearance instead of running a
destructor. Drop glue does not change this: when source consumes an owner (`v.drop()`,
`defer v.drop()`, `destroy(v)`), that operation may destroy the owner's live fields or elements, but
the compiler never inserts a scope-end destructor (OWN-13). Copy answers "may it be duplicated", not
"may it be dropped": a value that owns nothing and is safe to copy and drop is declared `Copy`
(OWN-D1, TYPE-14). The design record with the per-site history is the archived
[OWNERSHIP_MODEL.md](../archive/design-records/OWNERSHIP_MODEL.md). The decided `defer` and exit-order
refinements (TIGHTENING_DECISIONS §4.5) and Copy being explicit (D6) are not yet implemented.
Every claim of "exactly once" here holds on every path that does not abort (STMT-2).

### Definitions

<a id="OWN-D1"></a>
#### OWN-D1. Copy

**OWN-D1@3.** A type is Copy when it is a primitive, a shared reference, a raw pointer, a function pointer, an array of a Copy type, a type parameter bounded by `Copy`, or an instantiation of a struct or enum declared `Copy` whose type arguments are all Copy, and every other type is linear.

- Kind: definition
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.isCopyType
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** Copy determines duplication, not permission to silently discard fallible values; OD54 removes newtypes from the classification and OD62 restricts unused names and wildcard drops.

### Rules

<a id="OWN-1"></a>
#### OWN-1. A moved value is not used again

**OWN-1@3.** After a linear value is moved or consumed, the binding that held it is not used or borrowed again.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0205 (check), E0230 (check)
- Status: pending R-0555
- Enforced by: Concrete.consumeVar, Concrete.checkSharedAccess, Concrete.checkStmt
- Semantics: none
- Obligation: no use after move in safe Core (R-0507)
- Evidence: tested

**Rationale.** Every site a value flows into moves it: `let g = f;`, a by-value argument,
`return f;`, a match scrutinee, an assignment `a = f;`, a struct, enum or array literal,
(a `break` operand, which the current compiler still parses, is the `break_` row below)
and a store `*p = v` or `a[i] = v`. Using the source afterwards would use a value that now has
another owner.

```con reject name=use-after-move-decided code=E0205 stage=check pair=move-once pending=R-0555 now=reject:E0205:check
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn example_value() -> i64 {
    let r: R = R { v: 21 };
    let a: i64 = take(r);
    return a + take(r);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=borrow-after-move-decided code=E0230 stage=check pair=move-once pending=R-0555 now=reject:E0001:parse
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn example_value() -> i64 {
    let r: R = R { v: 21 };
    let a: i64 = take(r);
    let mut out: i64 = 0;
    borrow r as rr {
        out = rr.v;
    }
    return a + out;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=move-once exit=42
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn peek(r: &R) -> i64 {
    return r.v;
}

fn example_value() -> i64 {
    let r: R = R { v: 21 };
    let a: i64 = peek(&r);
    return a + take(r);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=trusted-use-after-move-decided code=E0205 stage=check pair=move-once pending=R-0555 now=reject:E0205:check
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

trusted fn twice(r: R) -> i64 {
    return take(r) + take(r);
}

fn example_value() -> i64 {
    return twice(R { v: 21 });
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

<a id="OWN-2"></a>
#### OWN-2. Every owned linear binding is consumed before every exit

**OWN-2@3.** Every owned linear binding, parameters and by-value `self` included, is consumed before every exit of its scope (falling off the end, `return`, `break` and `continue`), except where the end of the scope is unreachable.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0208 (check), E0210 (check), E0211 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkScopeExit, Concrete.checkBlockLocalsConsumed, Concrete.checkReturnPathConsumed, Concrete.checkStmt
- Semantics: none
- Obligation: linear conservation at reachable exits (R-0507; VERIFIED_PIPELINE, "Resources and failure")
- Evidence: tested

**Rationale.** Parameters are owned locals: the caller moved the value in, so the callee holds
the obligation. There is no terminal parameter sink, so `fn drop_it(f: File) {}` is refused. A
scope whose end is unreachable, such as an infinite `while true` with no `break` or a call to
`abort()`, need not consume its locals, because no exit follows. A pending `defer` counts as
consumption (OWN-12).

```con reject name=never-consumed-decided code=E0208 stage=check pair=consumed-on-every-exit pending=R-0555 now=reject:E0208:check
struct R { v: i64 }

fn main() -> u8 {
    let r: R = R { v: 42 };
    return 0;
}
```

```con reject name=early-return-leaks-decided code=E0208 stage=check pair=consumed-on-every-exit pending=R-0555 now=reject:E0208:check
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn f(c: bool) -> i64 {
    let r: R = R { v: 42 };
    if c {
        return 0;
    }
    return take(r);
}

fn example_value() -> i64 {
    return f(false);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=consumed-on-every-exit exit=42
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn f(c: bool) -> i64 {
    let r: R = R { v: 42 };
    if c {
        return take(r) * 0;
    }
    return take(r);
}

fn example_value() -> i64 {
    return f(false);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=trusted-never-consumed-decided code=E0208 stage=check pair=consumed-on-every-exit pending=R-0555 now=reject:E0208:check
struct R { v: i64 }

trusted fn drop_it(r: R) -> i64 {
    return 42;
}

fn example_value() -> i64 {
    return drop_it(R { v: 1 });
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

<a id="OWN-3"></a>
#### OWN-3. Branches agree on what they consume

**OWN-3@3.** Every path of an `if` or `match` that reaches the point after it has consumed the same linear values, while a path that diverges or leaves the function is exempt from that agreement.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0209 (check), E0212 (check), E0213 (check)
- Status: pending R-0555
- Enforced by: Concrete.mergeVarStates, Concrete.checkNoBranchConsumption, Concrete.checkExprCore
- Semantics: none
- Obligation: linear conservation at reachable exits (R-0507)
- Evidence: tested

**Rationale.** After a merge a value cannot be "maybe consumed". A branch that returns, breaks
out of the function or aborts never reaches the merge, so
`Some { v } => use(v), None => { drop(x); return 1; }` is sound. If every arm diverges, the code
after the match is unreachable.

```con reject name=one-branch-consumes-decided code=E0212 stage=check pair=both-branches-consume pending=R-0555 now=reject:E0212:check
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn example_value() -> i64 {
    let r: R = R { v: 42 };
    let mut out: i64 = 0;
    if out == 0 {
        out = take(r);
    } else {
        out = 1;
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=both-branches-consume exit=42
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn example_value() -> i64 {
    let r: R = R { v: 42 };
    let mut out: i64 = 0;
    if out == 0 {
        out = take(r);
    } else {
        out = take(r) * 0 + 1;
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-4"></a>
#### OWN-4. A loop body does not consume an outer value

**OWN-4@3.** A loop body does not consume a linear value declared outside the loop, except on a path that leaves the function or the loop before another iteration can run, or in an assignment that consumes a `mut` binding and gives it a new value in the same statement (`acc = f(acc)`).

- Kind: refusal
- Uses: OWN-D1
- Codes: E0207 (check)
- Status: pending R-0555
- Enforced by: Concrete.consumeVar
- Semantics: none
- Obligation: linear conservation at reachable exits (R-0507)
- Evidence: tested

**Rationale.** The next iteration would see a moved value.
`while c { if bad { s.drop(); return 1; } }` is sound, because no iteration follows the return.
A loop nested inside such a branch resets the exemption, because it iterates before the return.
A consequence for APIs: a generic function that returns one of two linear arguments always leaks
the other, so such helpers require `T: Copy`.

```con reject name=consume-outer-in-loop-decided code=E0207 stage=check pair=consume-after-loop pending=R-0555 now=reject:E0207:check
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn example_value() -> i64 {
    let r: R = R { v: 42 };
    let mut out: i64 = 0;
    let mut i: i64 = 0;
    while i < 2 {
        out = take(r);
        i = i + 1;
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=consume-after-loop exit=42
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn example_value() -> i64 {
    let r: R = R { v: 40 };
    let mut out: i64 = 0;
    let mut i: i64 = 0;
    while i < 2 {
        out = out + 1;
        i = i + 1;
    }
    return out + take(r);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-5"></a>
#### OWN-5. A linear value is never discarded

**OWN-5@3.** A linear value is never discarded, every bound name is used or is `_`, and `discard` and wildcard drops refuse `Result` and `Option` even when Copy, while other Copy values may be explicitly discarded and linear values must be consumed or handled.

- Kind: refusal
- Uses: OWN-D1
- Codes: new (check), E0287 (check), E0288 (check), E0289 (check), E0295 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkStmt, Concrete.checkExprCore
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** OD62 requires every bound name to be used or `_`; naming `_` never disposes of a linear value, Result or Option. Match fallible results explicitly even with Copy payloads.

```con reject name=wildcard-drops-linear-decided code=E0288 stage=check pair=account-for-it pending=R-0555 now=reject:E0288:check
struct R { v: i64 }

fn main() -> u8 {
    let o: Option<R> = Option::Some { value: R { v: 42 } };
    match o {
        _ => {}
    }
    return 0;
}
```

```con reject name=statement-drops-linear-decided code=E0287 stage=check pair=account-for-it pending=R-0555 now=reject:E0287:check
struct R { v: i64 }

fn mk() -> R {
    return R { v: 42 };
}

fn main() -> u8 {
    mk();
    return 0;
}
```

```con reject name=bare-variant-drops-payload-decided code=E0288 stage=check pair=account-for-it pending=R-0555 now=reject:E0288:check
struct R { v: i64 }
enum E { A { r: R }, B }

fn main() -> u8 {
    let e: E = E::A { r: R { v: 42 } };
    match e {
        E::A => { return 1; },
        E::B => { return 0; }
    }
}
```

```con reject name=discard-linear-decided code=E0295 stage=check pair=account-for-it pending=R-0555 now=reject:E0295:check
struct R { v: i64 }

fn main() -> u8 {
    let r: R = R { v: 42 };
    discard(r);
    return 0;
}
```

```con reject name=defer-drops-linear-decided code=E0287 stage=check pair=account-for-it pending=R-0555 now=reject:E0287:check
struct R { v: i64 }

fn mk() -> R {
    return R { v: 1 };
}

fn main() -> u8 {
    defer mk();
    return 42;
}
```

```con accept name=account-for-it exit=42
struct R { v: i64 }

fn example_value() -> i64 {
    let o: Option<R> = Option::Some { value: R { v: 42 } };
    let k: i64 = 3;
    discard(k);
    let mut out: i64 = 0;
    match o {
        Option::Some { value } => {
            let R { v } = value;
            out = v;
        },
        Option::None => {}
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=unused-name-refused code=new stage=check pair=account-for-it pending=R-0555 now=accept:42
fn ignore(x: u8) -> u8 { return 42; }
fn main() -> u8 { return ignore(1); }
```

<a id="OWN-6"></a>
#### OWN-6. A fallible result is used

**OWN-6@1.** A statement expression does not discard a value of type `Result` or `Option`.

- Kind: refusal
- Uses: none
- Codes: E0286 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** `Result` and `Option` exist to make the caller consider failure or absence.
Dropping one on the floor throws that away, even when the payload is Copy. The value is used by
matching it, propagating it or binding it (OWN-2 then requires a linear binding to be consumed). A
trailing value expression with no `;` is the block's value, not a discard. User enums are not
must-use.

The recorded answers in [OD62](#od62) govern this subject; the frozen compiler’s earlier behavior is not an alternative rule.

This entry and OWN-7 overlap: a statement expression of type `Result` or `Option` is also a non-`Unit` result under OWN-7. The current compiler reports such a statement as E0286 under this entry, whether it is a call (`get();`) or a literal (`Option::<i32>::Some { value: 1 };`); OWN-7's E0294 is reported for other syntactically pure values only. They stay two entries, with two codes, statuses and sets of examples; merging them would change which rule and code a diagnostic names. The future error-set report (`--report error-sets`,
ROADMAP old Phase 6 #14) must keep these diagnostics firing. Site: `mustUseEnumName?` and the
`Stmt.expr` discard check of `checkStmt` (`CheckError.discardedMustUse`); fixtures in
`tests/programs/ignored_result/`, gate `scripts/tests/check_ignored_result.sh`.

```con reject name=ignored-option code=E0286 stage=check pair=handled-option
fn find(k: i64) -> Option<i64> {
    if k > 0 {
        return Option::Some { value: k };
    }
    return Option::None;
}

fn main() -> u8 {
    find(42);
    return 0;
}
```

```con accept name=handled-option exit=42
fn find(k: i64) -> Option<i64> {
    if k > 0 {
        return Option::Some { value: k };
    }
    return Option::None;
}

fn example_value() -> i64 {
    let mut out: i64 = 0;
    match find(42) {
        Option::Some { value } => { out = value; },
        Option::None => {}
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-7"></a>
#### OWN-7. A result is not thrown away unwritten

**OWN-7@4.** A statement expression has type `Unit`, and a result of any other type is dropped only by writing `discard(e)`.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0294 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** `2 + 3;` or `f();` with a result is either a dead computation or a deliberate
discard, and the reader cannot tell which; `discard(e)` says it. The current compiler reports a statement expression of type `Result` or `Option` as OWN-6's E0286, not as E0294.

The owner decided on 2026-10-29 (OD29) that every unused non-Unit result needs `discard(e)`, with no syntactic purity heuristic. The current compiler still applies that heuristic: it refuses only a syntactically pure non-Unit Copy value (E0294), skipping calls and operations that can trap (`/`, `%`), and drops a call's result silently (bug 325). Its site is `exprPureDiscardable`, a local structural purity predicate in the `Stmt.expr` check (`CheckError.discardedPureValue`).


```con reject name=dead-computation-decided code=E0294 stage=check pair=used-computation pending=R-0555 now=reject:E0294:check
fn example_value() -> i64 {
    let a: i64 = 40;
    a + 2;
    return a;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=used-computation exit=42
fn example_value() -> i64 {
    let a: i64 = 40;
    discard(a + 1);
    return a + 2;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=unused-call-result code=E0294 stage=check pair=discarded-call-result pending=bug-325 now=accept:42
fn f() -> i64 {
    return 1;
}
fn main() -> u8 {
    f();
    return 42;
}
```

```con accept name=discarded-call-result exit=42
fn f() -> i64 {
    return 1;
}
fn main() -> u8 {
    discard(f());
    return 42;
}
```

<a id="OWN-8"></a>
#### OWN-8. A non-Copy part is not read out of a place

**OWN-8@3.** A non-Copy field, array element or heap interior is never read out of its place by value, so it is borrowed in place or its whole owner is destructured.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0290 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** A non-Copy field or element cannot be moved out by projection. OD32 requires an explicit reference receiver, written `(&s).m()` or `(&mut s).m()`, instead of auto-borrow.

```con reject name=field-read-by-value-decided code=E0290 stage=check pair=destructure-owner pending=R-0555 now=reject:E0290:check
struct R { v: i64 }
struct W { r: R }

fn example_value() -> i64 {
    let w: W = W { r: R { v: 42 } };
    let g: R = w.r;
    let R { v } = g;
    return v;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=destructure-owner exit=42
struct R { v: i64 }
struct W { r: R }

fn example_value() -> i64 {
    let w: W = W { r: R { v: 42 } };
    let W { r } = w;
    let R { v } = r;
    return v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-9"></a>
#### OWN-9. An assignment never overwrites a live linear value

**OWN-9@3.** An assignment never overwrites a live linear value, whether it is in a binding, a field, an array element or a place behind `&mut`, and a `mut` binding of linear type may be assigned again once its old value has been consumed.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0219 (check), E0291 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** Overwriting would leak the old value. The accumulate pattern `acc = f(acc, x)` is
legal, because the right-hand side consumed the old value first. Stores through a `*mut T` raw
pointer are exempt: trusted collection code writes uninitialized slots. The order in which an
assignment evaluates its parts, and the operations that return the displaced value, belong to
the statements chapter.

```con reject name=overwrite-live-decided code=E0219 stage=check pair=rebind-after-consume pending=R-0555 now=reject:E0219:check
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn example_value() -> i64 {
    let mut r: R = R { v: 1 };
    r = R { v: 42 };
    return take(r);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=overwrite-through-mut-ref-decided code=E0291 stage=check pair=rebind-after-consume pending=R-0555 now=reject:E0108:resolve
struct R { v: i64 }

fn replace(slot: &mut R, v: R) -> Unit {
    *slot = v;
}

fn main() -> u8 {
    return 0;
}
```

```con accept name=rebind-after-consume exit=42
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn bump(r: R) -> R {
    let R { v } = r;
    return R { v: v + 41 };
}

fn example_value() -> i64 {
    let mut r: R = R { v: 1 };
    r = bump(r);
    return take(r);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-10"></a>
#### OWN-10. Only a `mut` binding is assigned or mutably borrowed

**OWN-10@3.** A binding, or a field, element or heap cell of it, is assigned, or borrowed with `&mut`, only when the binding is declared `mut`.

- Kind: refusal
- Uses: none
- Codes: E0217 (check), E0233 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkStmt, Concrete.checkMutAccess
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Mutation is visible at the declaration. A `&mut` parameter is already a mutable
place, so writing through it needs no `mut` on the parameter. The owner decided on 2026-10-29 (OD17) that a field write requires a `mut` binding, as `&mut` of the field and a
direct array element write already did. The current compiler still accepts a write into a field, a nested array
element or a heap cell of a binding not declared `mut` (bug 216, which implements the rule), pinned
by the pending example below.

```con reject name=assign-immutable-decided code=E0217 stage=check pair=assign-mut-decided pending=R-0555 now=reject:E0217:check
fn example_value() -> i64 {
    let x: i64 = 1;
    x = 42;
    return x;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con reject name=mut-borrow-immutable-decided code=E0233 stage=check pair=assign-mut-decided pending=R-0555 now=reject:E0108:resolve
fn set(r: &mut i64) -> Unit {
    *r = 42;
}

fn example_value() -> i64 {
    let x: i64 = 1;
    set(&mut x);
    return x;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=field-write-immutable code=E0217 stage=check pair=assign-mut-decided pending=bug-216 now=accept:21
struct Copy P { x: i64, y: i64 }

fn example_value() -> i64 {
    let s: P = P { x: 1, y: 1 };
    s.y = 20;
    return s.x + s.y;
}

fn main() -> u8 {

    if example_value() == 21 {

        return 21;

    }

    return 1;

}
```

```con accept name=assign-mut-decided exit=42 pending=R-0555 now=reject:E0108:resolve
fn set(r: &mut i64) -> Unit {
    *r = 41;
}

fn example_value() -> i64 {
    let mut x: i64 = 1;
    set(&mut x);
    x = x + 1;
    return x;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-11"></a>
#### OWN-11. A move happens only where its code always runs

**OWN-11@1.** Nothing is moved by code that may not run when its effect is relied on: a proof-only statement (`assert`, `assume` or `ghost let`) changes the ownership state of no runtime binding, and the right operand of `&&` or `||` moves no value.

- Kind: refusal
- Uses: none
- Codes: E0243 (check), E0244 (check)
- Status: enforced
- Enforced by: Concrete.refuseErasedConsumption, Concrete.refuseConditionalConsumption
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** Proof-only code is erased before Core, so a move there would happen in the
checker's model and not in the binary (bug 147). The right operand of `&&` or `||` runs only
sometimes, so a move there would happen on only some paths (bug 148). A guard of a match arm is
read-only for the same reason; that rule (E0300) belongs to the patterns chapter. Ghost bindings exist only
in proof code, so proof code may consume them.

```con reject name=move-in-short-circuit code=E0244 stage=check pair=move-before-condition
struct R { v: i64 }

fn take(r: R) -> bool {
    let R { v } = r;
    return v > 0;
}

fn main() -> u8 {
    let r: R = R { v: 42 };
    let c: bool = true;
    if c && take(r) {
        return 42;
    }
    return 0;
}
```

```con accept name=move-before-condition exit=42
struct R { v: i64 }

fn take(r: R) -> bool {
    let R { v } = r;
    return v > 0;
}

fn main() -> u8 {
    let r: R = R { v: 42 };
    let t: bool = take(r);
    let c: bool = true;
    if c && t {
        return 42;
    }
    return 0;
}
```

<a id="OWN-12"></a>
#### OWN-12. `defer` reserves its arguments until the scope exits

**OWN-12@2.** A `defer` registers a function call whose arguments are local names, borrows of local names or constants, the call runs when its block exits on any path that does not abort, the block's deferred calls run in reverse order of registration, and until then a local passed by value is reserved and a local passed by borrow is pinned, so neither may be moved or consumed.

- Kind: refusal
- Uses: none
- Codes: E0206 (check), E0279 (check), E0282 (check)
- Status: pending R-0555
- Enforced by: Concrete.consumeVar, Concrete.checkStmt
- Semantics: none
- Obligation: deferred calls run exactly once on every exit that does not abort (R-0496, R-0501)
- Evidence: tested

**Rationale.** A deferred call reads its argument places at the exit, not at registration: a
Copy argument sees the value the local holds then. Arguments are bound by binding identity, so a
later `let` of the same name is a different binding. A reserved local may still be read,
borrowed and mutated in place. The deferred call discharges its obligation, and a later use of it
is a use after move (OWN-1). A pinned local must still be consumed, so a deferred observer goes
after the deferred consumer in source (`defer kill(t); defer peek(&t);` runs `peek` first). The
body is a function call. A method call, a block, a projection or a nested call is refused,
because it would be evaluated at the exit. The decided refinements are pending: loans retained by
a deferred call stay live until it runs, and the exit order (the exit value first, then each
scope's deferred calls innermost first, then its loans end) is TIGHTENING_DECISIONS §4.5.

`defer <call>;` schedules the call for the exit of the enclosing block: within a block the calls
run last registered first (`defer a(); defer b();` runs `b`, then `a`), on fall-through, on an early
`return` (an explicit `Result::Err` return included) and on a `break` or `continue` out of the block,
and each block runs its own list, inner blocks before outer ones (`Concrete/IR/Lower.lean`:
`emitFrameDeferredCalls` at a scope's end, `emitAllDeferredCalls` on `return`,
`emitDeferredUntilLoop` on `break` and `continue`). An argument is a local name, a shared or
exclusive borrow of a local name, or a constant (a non-string literal, a module constant, a function name); a
projection (`defer f(t.id)`, `defer f(&t.field)`), a nested call (`defer f(g(x))`) or a string literal
is E0279, so write a named cleanup function that takes the local or a borrow of it and computes
inside (decided 2026-10-21, bug 143). A by-value local is reserved, not taken: E0206 ("reserved by
defer") refuses a later move or consumption (`error_defer_move.con`,
`error_memory_edge_defer_then_move.con`, `pressure_err_defer_then_move.con`), and when the block ends
the reserved locals are consumed (a later use is E0205) and the pins end. A borrowed local is
pinned (E0206, "borrowed by a pending defer"), so `defer peek(&t); defer kill(t);` is refused and
`defer peek(&t);` alone leaves `t` unconsumed (E0208). A deferred call reads its arguments when it runs, so a registration-time value is
spelled `let x0 = x; defer show(x0);`. Rebinding the name in the same block is not a way around
this: a visible binding cannot be shadowed (SCOPE-1, E0301). A method call
`defer obj.method();` is E0282 (`deferBodyNotCall`; an earlier design note wrongly allowed it,
EVALUATION2 D7): defer the function the method desugars to, or a named cleanup function; `defer
{ ... }` is a parse error. A deferred call's arguments are typed by its parameters as in an ordinary
call (`defer f(1);` passes an `i32` to an `i32` parameter), but a string literal is not a deferred
argument (E0279), so `defer println("x")` is refused where `println("x")` compiles: defer a cleanup
function instead (an earlier design note blamed a missing argument coercion). OD68 stops cleanup if a deferred call traps or diverges; a trap runs no remaining deferred calls, and divergence never reaches them. An abort runs no deferred calls (OD10; STMT-2). There is no implicit scope-end drop to order
against `defer` (OWN-13): `destroy(x)`, `x.drop()` or a deferred explicit call consumes a value.
The surface is pinned by `scripts/tests/check_defer.sh` and `examples/defer/cleanup_order/`
(ROADMAP old Phase 6 #7, 2026-06-23).

```con reject name=move-reserved-decided code=E0206 stage=check pair=defer-consumes-decided pending=R-0555 now=reject:E0108:resolve
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn release(r: R) -> Unit {
    let R { v: _ } = r;
}

fn example_value() -> i64 {
    let r: R = R { v: 42 };
    defer release(r);
    return take(r);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=defer-projection-decided code=E0279 stage=check pair=defer-consumes-decided pending=R-0555 now=reject:E0108:resolve
struct Copy P { n: i64 }

fn show(n: i64) -> Unit {
}

fn main() -> u8 {
    let p: P = P { n: 42 };
    defer show(p.n);
    return 0;
}
```

```con accept name=defer-consumes-decided exit=42 pending=R-0555 now=reject:E0108:resolve
struct R { v: i64 }

fn release(r: R) -> Unit {
    let R { v: _ } = r;
}

fn peek(r: &R) -> i64 {
    return r.v;
}

fn example_value() -> i64 {
    let r: R = R { v: 42 };
    defer release(r);
    return peek(&r);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-13"></a>
#### OWN-13. Nothing is destroyed implicitly

**OWN-13@3.** No value is destroyed implicitly: a linear value is destroyed only by an explicit consuming call (`destroy(x)`, a consuming method or a deferred one), no type is both Copy and `Destroy`, and a `Destroy` implementation requires no capability beyond `Alloc`.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0570 (core-check), E0584 (core-check), E0272 (check), E0273 (check)
- Status: pending R-0555
- Enforced by: Concrete.ccCheckModuleDecls, Concrete.checkExprCore
- Semantics: none
- Obligation: ownership instrumentation with a proved erasure theorem (R-0496, R-0507)
- Evidence: tested

**Rationale.** OD69 distinguishes storage teardown after loans end from Destroy; no destructor runs implicitly. Cleanup needing operational effects is explicit, and container Destroy applicability is [undecided: see the target Core proposal](#core-allocation-contracts).

```con reject name=copy-and-destroy-decided code=E0570 stage=core-check pair=explicit-destroy-decided pending=R-0555 now=reject:E0108:resolve
struct Copy C { v: i64 }

impl Destroy for C {
    fn destroy(self) -> Unit {
        let C { v: _ } = self;
    }
}

fn main() -> u8 {
    return 0;
}
```

```con accept name=explicit-destroy-decided exit=42 pending=R-0555 now=reject:E0108:resolve
struct R { v: i64 }

impl Destroy for R {
    fn destroy(self) -> Unit {
        let R { v: _ } = self;
    }
}

fn example_value() -> i64 {
    let r: R = R { v: 42 };
    let out: i64 = r.v;
    destroy(r);
    return out;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-14"></a>
#### OWN-14. A `Copy` declaration has only Copy fields

**OWN-14@3.** A struct or enum declared `Copy` has only fields and payloads that are Copy when its type parameters are taken as Copy, so no Copy value ever contains a linear one.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0571 (core-check, post-mono)
- Status: pending R-0555
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: Copy is a fact of the declaration (TIGHTENING_DECISIONS D6, pending)
- Evidence: tested

**Rationale.** A Copy value is duplicated freely, so a linear field inside it would be duplicated
with it. A generic declaration is checked once, with its type parameters taken as Copy, so a
field that is linear whatever the arguments (`String`) is refused at the declaration.

```con reject name=copy-with-linear-field-decided code=E0571 stage=core-check pair=copy-with-copy-fields pending=R-0555 now=reject:E0571:core-check
struct S { x: i64 }
struct Copy T { s: S }

fn main() -> u8 {
    return 0;
}
```

```con accept name=copy-with-copy-fields exit=42
struct Copy S { x: i64 }
struct Copy T { s: S }

fn example_value() -> i64 {
    let t: T = T { s: S { x: 21 } };
    let u: T = t;
    return t.s.x + u.s.x;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=generic-with-linear-field-decided code=E0571 stage=core-check pair=copy-with-copy-fields pending=R-0555 now=reject:E0571:core-check
struct Copy S<T> { a: T, s: String }

fn main() -> u8 {
    return 0;
}
```

<a id="OWN-15"></a>
#### OWN-15. Every surface form has one value-flow behaviour

**OWN-15@5.** Every surface expression, match-arm and statement constructor creates, moves, borrows, copies, overwrites or refuses its operands as its row in this entry's value-flow tables states, and no method call borrows its receiver implicitly.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt
- Semantics: none
- Obligation: one value-flow rule per Core constructor in the Core checker (R-0499)
- Evidence: tested

**Rationale.** Value flow used to be spread over many syntax handlers, and new forms kept
landing without a conservation story (`break (value)` had no row for weeks). Now
`scripts/tests/check_value_flow_spec.sh` reads the constructors from `Concrete/Frontend/AST.lean`
and fails until each one has a row here, and its gate name, where there is one, must have a
fixture. Since the 2026-07-06 refactor, `checkExpr` decides consumption centrally by mode.
`callArg` is confined to argument checking, and the gate pins its use-site count.

The tables were introduced under ROADMAP 13c, the prevention program after the H13 – H17 sweep,
which named three failure classes, each with its gate: **discard** (a value vanishes,
`check_linear_discard.sh`), **conservation** (a value duplicates or aliases,
`check_linear_conservation.sh`) and **scope** (a value falls out of nested control flow,
`check_linear_nested_scope.sh`). The cross-cutting ruling H17 (2026-07-05) made parameters owned
locals, by-value `self` and `Destroy` bodies included.

The owner decided on 2026-10-29 (OD32) that there is no receiver auto-borrow; a
`&self` or `&mut self` method on an owned place is called as `(&s).m()` or `(&mut s).m()`, and a
receiver that is already a reference is unchanged. The current compiler still borrows an owned receiver implicitly
(bug 328); the `methodCall` row below states the decided form and notes the current compiler's behaviour. `break <expr>`
is refused at parse by the decided rule (STMT-6, OD27); the current compiler still parses it (bug 323).


```con accept name=flows-move-once exit=42
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

struct Two { a: R, b: R }

fn example_value() -> i64 {
    let a: R = R { v: 20 };
    let b: R = R { v: 22 };
    let two: Two = Two { a: a, b: b };
    let Two { a: x, b: y } = two;
    return take(x) + take(y);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

##### Value flow: Expr

| constructor | value flow | gate / errors |
|---|---|---|
| `intLit` `floatLit` `boolLit` `charLit` | creates a Copy value | — |
| `strLit` | static text needs no allocation; literal type and ownership are [undecided: see the target Core proposal](#core-static-text) | discard (E0208 via binding rules) |
| `ident` | in value position **moves** a non-Copy binding (consumed at the use site); a Copy one is copied | conservation (E0205 reuse after move) |
| `binOp` | reads Copy operands (non-Copy operands are type errors); `&&` and `\|\|` short-circuit control flow only (OWN-11) | — |
| `unaryOp` | reads a Copy operand | — |
| `call` | each ident argument **moves**; the result is an owned value the caller must account for | conservation (E0205); discard (E0286 / E0287 on a `;` discard) |
| `paren` | transparent: propagates the position (including place-ness) to the inner expression | — |
| `structLit` | each explicit field expression **moves** in; `..base` is refused (OD53) | conservation |
| `fieldAccess` | place projection: a Copy field is copied, a non-Copy field read by value is **refused** (**E0290**, OWN-8); as a projection base, borrow target or assignment target it is a place (no flow); newtypes and `.0` are refused (OD54) | conservation (H11 rows) |
| `enumLit` | payload expressions **move** in; creates an owned value | conservation |
| `match_` | an ident scrutinee **moves**; matching through `&` / `&mut` borrows payloads (REF-9); arms must agree on consumption (E0209, OWN-3) | conservation + discard (`_` rules E0288) |
| `borrow` | **borrows** the place (no move); the result `&T` is Copy | conservation (accept rows) |
| `borrowMut` | **exclusive borrow**; the result `&mut T` is not Copy but carries no consume obligation | conservation |
| `deref` | `*ref` reads (a Copy pointee is copied); `*heap_ident` **consumes** the `Heap` (load and free); `*rawptr` is the trusted idiom | conservation |
| `arrayLit` | **moves** each element in (H10) | conservation (E0205 rows) |
| `arrayIndex` | place projection: a Copy element is copied, a non-Copy element read by value is **refused** (**E0290**, OWN-8) | conservation (H11 rows) |
| `cast` | reads a Copy numeric | — |
| `methodCall` | the receiver is a place; a `&self` / `&mut self` method takes a reference receiver, written `(&s).m()` / `(&mut s).m()` for an owned place (the current compiler still borrows an owned receiver implicitly, bug 328); by-value `self` **moves** an ident receiver and **refuses** a projection receiver (**E0290**); ident arguments **move** | conservation (H11 rows) |
| `staticMethodCall` | ident arguments **move**; the result is owned | conservation |
| `fnRef` | creates a Copy function-pointer value | — |
| heap-shell `.fieldAccess` (the removed `->` form) | no implicit heap opening; borrow blocks open one heap layer, preserving root/lifetime; consuming extraction is `*h` (MEM-4, MEM-5) | projection |
| `allocCall` | allocator and input evaluated; returns `Result<Heap<T, A>, AllocFailure<T>>`, preserving input on failure (MEM-5) | discard |
| `ifExpr` | branch values **move**; branches must agree on consumption (OWN-3) | conservation |

##### Value flow: MatchArm

| constructor | value flow | gate / errors |
|---|---|---|
| `mk` (enum arm) | bindings **own** the payloads of an owned scrutinee (they must be consumed, E0208) and **borrow** them through a reference scrutinee; a `_` payload may ignore only a Copy value (**E0288**); guards are refused (OD55) | discard + scope |
| `litArm` | no bindings; a literal compare on a Copy scrutinee | — |
| `varArm` | the binding **owns** the whole scrutinee value (**moves** it; it must be consumed) | discard (E0205 / E0208 rows) |
| `rangeArm` | refused: range patterns are removed (OD56) | — |

##### Value flow: Stmt

| constructor | value flow | gate / errors |
|---|---|---|
| `letDecl` | **moves** an ident right-hand side (H9); reusing the name of any visible binding is **refused** (**E0301**, SCOPE-1); `_` is an unused binding only for eligible Copy values, never linear values, `Result` or `Option` (OD62) | conservation + discard |
| `assign` | the old value must already be consumed (otherwise **E0219**, OWN-9); an ident right-hand side **moves** (H13) | conservation (H13 rows) |
| `return_` | an ident value **moves**; every return path must leave no unconsumed linear value (OWN-2) | conservation + scope |
| `expr` | `e;` (not a value) **discards**: a fallible result is **refused** (**E0286**, whether or not it is Copy), a non-Copy value is **refused** (**E0287**); a trailing `e` (a value) is the block's value, never a discard; `free` is refused (OD59); eligible Copy results require explicit discard | discard |
| `ifElse` | branches must agree on consumption of outer linear values (E0209 / E0212, OWN-3); branch locals are checked at branch exit (H9) | scope |
| `while_` | the body may not consume an outer linear value (**E0207**, OWN-4) except in a branch that exits the function, a rebind, or a move next to a `break` (the H14 one-level exemption) | scope |
| `forLoop` | the same rules as `while_`; the counted loop of OD47 desugars to `while_` in Core, its index an immutable Copy binder of the body | scope |
| `fieldAssign` | an overwrite of a non-Copy field is **refused** (**E0219**); an ident right-hand side **moves**; the object is a place | conservation |
| `derefAssign` | through `&mut T`: an overwrite of a non-Copy pointee is **refused** (**E0291**, H15); through `*mut T`: a trusted uninitialized-slot store (exempt); an ident right-hand side **moves** | conservation (H15 rows) |
| `arrayIndexAssign` | an overwrite of a non-Copy element is **refused** (**E0291**, H15); an ident right-hand side **moves**; the array is a place | conservation (H15 rows) |
| `break_` | may not skip unconsumed loop-local linear values (E0210); the H14 one-level loop-depth exemption of the `while_` row applies (a break fires at most once per loop entry); `break` carries no value (STMT-6@3, OD27), and the current compiler, which still parses `break <expr>;` (bug 323), evaluates and drops it, refusing a non-Copy one (bug 149) | conservation (H14 rows) + scope |
| `continue_` | may not skip unconsumed loop-local linear values (E0211) | scope |
| `defer` | the deferred call **reserves** its operands, consumed at scope exit (OWN-12); only a Unit-returning call is accepted; other results use a named wrapper (OWN-17) | discard |
| `assert_` | proof-only, erased before Core; must not change the ownership state of a runtime binding (OWN-11) | — |
| `assume_` | proof-only, erased before Core; must not change the ownership state of a runtime binding (OWN-11) | — |
| `borrowIn` | a borrow block: the source is frozen for the block; the reference is scoped and may not escape (REF-6, REF-8) | conservation (escape rows) |
| `letDestructure` | irrefutable enum destructuring **consumes** the source; bindings **own** payloads; refutable let and let-else are refused (OD52) | conservation + discard |
| `letStructDestructure` | a whole-owner **move**: the source is consumed, all fields must be bound (a partial pattern is **E0252**), and each binding is owned (it must be consumed) | conservation |

##### Expression modes

The checker decides consumption by the position an expression is checked in (`UseMode` of
`checkExpr`). Every `Expr` constructor has a row.

| constructor | value | callArg | place |
|---|---|---|---|
| `intLit` `floatLit` `boolLit` `strLit` `charLit` | creates | same as value | n/a (not addressable) |
| `ident` | **moves** non-Copy (auto-consume; frozen check for all) | reads; the call site consumes according to the parameter type | reads; no consume |
| `binOp` `unaryOp` `cast` | reads Copy operands (a cast operand is checked as a place, so an illegal cast reports "cannot cast") | same | n/a |
| `call` `methodCall` `staticMethodCall` | the result is an owned value; arguments are checked in callArg | n/a (a call result is a value) | n/a |
| `paren` | transparent: propagates the surrounding mode | transparent | transparent |
| `structLit` `enumLit` `arrayLit` | fields and elements are checked in value mode (moved in); `..base` is refused (OD53) | same | n/a |
| `fieldAccess` `arrayIndex` | a Copy read copies; a non-Copy read is refused (E0290); the base is checked as a place | same as value | sub-place; no flow |
| `match_` `ifExpr` | scrutinee and arm values in value mode; branch environments merged | n/a | n/a |
| `borrow` `borrowMut` | the inner expression is checked as a place; the result `&T` is Copy and `&mut T` exclusive | same | n/a |
| `deref` | the inner expression is checked as a place (reading through a reference or pointer never consumes the binding); `*heap_ident` explicitly consumes | same | inner as place |
| `fnRef` | creates a Copy function pointer | same | n/a |
| `allocCall` | allocator and input evaluated; returns `Result<Heap<T, A>, AllocFailure<T>>`, preserving input on failure (MEM-5) | n/a | n/a |

<a id="OWN-16"></a>
#### OWN-16. A borrow given to a deferred call lives until the call runs

**OWN-16@1.** A borrow passed to a deferred call stays live until the deferred call runs, so a conflicting borrow or use of its place before then is refused.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0555
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD2, part 4), alongside OWN-12, which stands: a
deferred call's arguments are read at the exit, and a local passed by value is reserved and one
passed by borrow pinned. This entry adds the loan: after `defer log(&mut x)`, no borrow of `x` that
conflicts with that exclusive borrow (any borrow of the same root, REF-11) may be taken before the block exits. The current compiler
already holds a deferred `&mut` reference binding's loan (`heldByDefer`, `refuseDeferHeldRef`,
E0206), but it accepts a shared borrow of a local whose exclusive borrow a deferred call holds.

```con reject name=borrow-while-deferred code=new stage=check pair=deferred-borrow-alone-decided pending=R-0555 now=reject:E0108:resolve
fn log(r: &mut i64) -> Unit {
    *r = *r + 1;
}

fn peek(r: &i64) -> i64 {
    return *r;
}

fn example_value() -> i64 {
    let mut x: i64 = 41;
    let mut out: i64 = 0;
    if true {
        defer log(&mut x);
        out = peek(&x);
    }
    return out + 1;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=deferred-borrow-alone-decided exit=42 pending=R-0555 now=reject:E0108:resolve
fn log(r: &mut i64) -> Unit {
    *r = *r + 1;
}

fn example_value() -> i64 {
    let mut x: i64 = 41;
    if true {
        defer log(&mut x);
    }
    return x;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="OWN-17"></a>
#### OWN-17. A deferred call returns only Unit

**OWN-17@2.** A deferred call must return `Unit`; every other result is handled in a named wrapper returning `Unit` that consumes a linear result, handles a `Result` or `Option`, or explicitly discards an eligible Copy result.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The canonical Unit-only defer decision requires a named wrapper for other calls; it handles Result or Option, consumes linear results and explicitly discards only eligible Copy results (OD62).

```con accept name=defer-unit-call-decided exit=42 pending=R-0555 now=reject:E0108:resolve
struct R { v: i64 }

fn release(r: R) -> Unit {
    let R { v: _ } = r;
}

fn main() -> u8 {
    let r: R = R { v: 1 };
    defer release(r);
    return 42;
}
```

### Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- OWN-D1@1 looked at the fields after substitution, so a parameter no field mentions (`struct Copy P<T> { n: Int }`) made every instantiation Copy, and Mono needed a fixpoint over the type graph. OWN-D1@2 (TIGHTENING_DECISIONS D6, option (a), 2026-10-23) decides from the declaration alone.
- OWN-7@2 (CapabilityJudgment slice 5) flagged only syntactically pure values: it skipped calls, because an empty capability set does not prove a call pure (mutation through `&mut` and foreign effects are not capabilities), and operations that can trap (`/`, `%`). OWN-7@3 records OD29 (2026-10-29), which removes the heuristic.
- OWN-10@2 records OD17 (2026-10-29): a field write requires a `mut` binding.
- OWN-14@1 checked each instantiation and made a non-qualifying one linear; OWN-14@2 refuses the declaration.
- OWN-15@3 records OD32 (2026-10-29): no receiver auto-borrow.

- OWN-D1@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; OWN-D1@2 is superseded by version.
- OWN-1@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; OWN-1@2 is superseded by version.
- OWN-2@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; OWN-2@2 is superseded by version.
- OWN-3@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; OWN-3@2 is superseded by version.
- OWN-4@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; OWN-4@2 is superseded by version.
- OWN-5@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; OWN-5@2 is superseded by version.
- OWN-7@4 (2026-11-03) brings the chapter into line with the recorded owner decisions; OWN-7@3 is superseded by version.
- OWN-8@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; OWN-8@2 is superseded by version.
- OWN-9@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; OWN-9@2 is superseded by version.
- OWN-13@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; OWN-13@2 is superseded by version.
- OWN-14@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; OWN-14@2 is superseded by version.
- OWN-15@4 (2026-11-03) brings the chapter into line with the recorded owner decisions; OWN-15@3 is superseded by version.
- OWN-17@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; OWN-17@1 is superseded by version.

- OWN-10@3 (2026-11-03) supersedes OWN-10@2; pending legacy conformance is explicit.

- OWN-12@2 (2026-11-03) supersedes OWN-12@1; pending legacy conformance is explicit.


<a id="ch-references"></a>
## References

References in Concrete are **second-class**. A reference can be a parameter, a call argument,
a borrow-block binder, or a pattern binder scoped to one arm through a borrowed scrutinee. It is
never returned, stored, bound by `let` or assigned. Every reference is a root binding plus a
sequence of projections, and the root binding outlives the reference lexically. No
lifetime analysis is needed to show this, which is what makes the rules checkable by a small
lexical argument (INV-REF, the proof target of R-0500 and V1).

This chapter states the rules. For the design record behind them (probes, status history, gap
analysis G1 to G19), see [MEMORY_SEMANTICS §15](../archive/design-records/MEMORY_SEMANTICS.md). For the
decisions of 2026-10-23 (D1 to D4), see
[TIGHTENING_DECISIONS §4](../project/TIGHTENING_DECISIONS.md#4-decisions-of-2026-10-23). Those decisions enter this
chapter as pending examples and entries until they are implemented. Raw pointers
(`*const T`, `*mut T`) are not references, and these rules do not cover them. The trusted
boundary is in [ffi.md](#ch-ffi).

### Definitions

<a id="REF-D1"></a>
#### REF-D1. Contains a reference

**REF-D1@1.** A type contains a reference when a reference type (`&T` or `&mut T`, also when it is spelled through a type alias) occurs anywhere in it, including inside raw pointers, `Heap`, arrays, generic arguments and the parameters and result of a function type.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.tyContainsRef
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** This is the wide predicate. The rules about what may be returned use it (REF-1,
REF-2, REF-4), because a reference must not leave a frame by any route, including behind a
raw pointer or inside a callback's type.

<a id="REF-D2"></a>
#### REF-D2. Holds a reference

**REF-D2@1.** A type holds a reference when it is a reference type or an array, `Heap`, heap array or generic instantiation one of whose element or argument types holds a reference, where a function-pointer type and a raw pointer are not looked through.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.tyStoresRef, Concrete.tyHoldsRefInAggregate
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** This is the narrow predicate. The storage rules use it (REF-3, REF-4, REF-7). A
`fn(&T) -> U` value stores no reference, so a struct may hold a callback that takes a
reference. A named struct or enum can hold a reference only through its type arguments,
because REF-3 forbids reference fields. For a binding, the outer reference is looked through
(`tyHoldsRefInAggregate`): `& &T` and `&[i64; 2]` hold no reference in an aggregate.

<a id="REF-D3"></a>
#### REF-D3. Borrow block

**REF-D3@3.** A borrow block `borrow x as r { B }` freezes the local `x` for `B` and lends an ordinary `x: T` as `&T` (`borrow mut` as `&mut T`), but opens exactly one layer of `Heap<U, A>` (also through a shell reference) to its payload preserving the root and block lifetime and never upgrading shared access, while the exact heap-array view remains undecided (see [the target Core proposal](#core-heap-array-view)), and the block ends its local loans and restores the owner state.

- Kind: definition
- Uses: MEM-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** OD58 retains borrow blocks and OD57 removes the named `in R` region annotation; references retain the source root and block lifetime. Heap opening follows MEM-4. The exact heap-array view is [undecided: see the target Core proposal](#core-heap-array-view).

```con accept name=owner-restored-decided exit=42 pending=R-0555 now=reject:E0001:parse
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn peek(r: &R) -> i64 {
    return r.v;
}

fn example_value() -> i64 {
    let r: R = R { v: 42 };
    let mut seen: i64 = 0;
    borrow r as rr {
        seen = peek(rr);
    }
    return take(r) + seen * 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

### Rules

<a id="REF-1"></a>
#### REF-1. No function returns a reference

**REF-1@1.** No function, method or trait method, `trusted` ones included, has a return type that contains a reference.

- Kind: refusal
- Uses: REF-D1
- Codes: E0236 (check, post-mono)
- Status: enforced
- Enforced by: Concrete.checkFn, Concrete.verifyNoReturnedRefs, Concrete.checkModule
- Semantics: none
- Obligation: INV-REF (R-0500; V1, R-0507)
- Evidence: tested

**Rationale.** A returned reference would outlive the frame that made it. The escape for
low-level code is a raw pointer to the referent, returned from trusted code. A trait method
*declaration* is checked like a definition (gap G13, closed 2026-10-23). An `extern fn` whose
parameter or result contains a reference, also behind a raw pointer, is refused at the C boundary
by FFI-3 (gap G15, closed 2026-10-23). A generic reached at a
reference-returning instantiation only through another generic is refused after
monomorphization by the same judgment (gap G2).

Decided by the owner on 2026-10-29 (OD1): references are second-class, never returned (this entry) or stored (REF-3).

```con reject name=return-param code=E0236 stage=check pair=return-value
fn id(x: &i64) -> &i64 {
    return x;
}

fn main() -> u8 {
    let v: i64 = 42;
    return 0;
}
```

```con accept name=return-value exit=42
fn get(x: &i64) -> i64 {
    return *x;
}

fn example_value() -> i64 {
    let v: i64 = 42;
    return get(&v);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=trait-declaration code=E0236 stage=check pair=return-value
trait Get {
    fn get(&self) -> &i64;
}

fn main() -> u8 {
    return 42;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_escape_return.con` | `reject:E0236:REF-1@1` | `ref1` | `-` |
| `tests/programs/error_2i_ref1_return_bare.con` | `reject:E0236:REF-1@1` | `ref1` | `-` |
| `tests/programs/error_2i_ref1_return_option.con` | `reject:E0236:REF-1@1` | `ref1` | `-` |
| `tests/programs/error_2i_ref1_return_fnptr_ref_param.con` | `reject:E0236:REF-1@1` | `ref1` | `-` |
| `tests/programs/regress_2i_ref1_return_value.con` | `run:42` | `ref1` | `binary,interp` |
| `tests/programs/error_2i_ref1_trait_decl_returns_ref.con` | `reject:E0236:REF-1@1` | `ref1g13` | `-` |
| `tests/programs/regress_2i_ref1_trait_decl_value.con` | `run:42` | `ref1g13` | `binary,interp` |
| `tests/programs/error_2i_ref1_transitive_generic.con` | `reject:E0236:REF-1@1` | `ref1g2` | `json-exempt:post-mono` |
| `tests/programs/regress_2i_ref1_transitive_generic_value.con` | `run:42` | `ref1g2` | `binary,interp` |

<a id="REF-2"></a>
#### REF-2. No function type returns a reference

**REF-2@1.** No function type, wherever it is written, has a return type that contains a reference.

- Kind: refusal
- Uses: REF-D1
- Codes: E0236 (check)
- Status: enforced
- Enforced by: Concrete.resolveType, Concrete.checkModule
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** A callback that could return a reference could hand a borrowed element back
out of a scoped accessor such as `with_value`. Parameters of reference type are allowed. The
type is refused wherever it is written, also through a type alias, as a field type and in a
trait method signature (gap G16, closed 2026-10-23), although no value of it could be made in
safe code, because every function that could inhabit it is refused by REF-1.

Decided by the owner on 2026-10-29 (OD14): function types that return a reference are refused everywhere, and one structural well-formedness judgment runs before specialization (R-0499).

```con reject name=param-fn-type code=E0236 stage=check pair=callback-takes-ref
fn apply(x: &i64, f: fn(&i64) -> &i64) -> i64 {
    return 0;
}

fn main() -> u8 {
    return 0;
}
```

```con accept name=callback-takes-ref exit=42
fn apply(x: &i64, f: fn(&i64) -> i64) -> i64 {
    return f(x);
}

fn read(r: &i64) -> i64 {
    return *r;
}

fn example_value() -> i64 {
    let v: i64 = 42;
    return apply(&v, read);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=through-alias code=E0236 stage=check pair=callback-takes-ref
type Cb = fn(&i64) -> &i64;

fn apply(x: &i64, f: Cb) -> i64 {
    return 0;
}

fn main() -> u8 {
    return 0;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_2i_ref2_fn_type_returns_ref.con` | `reject:E0236:REF-2@1` | `ref2` | `-` |
| `tests/programs/regress_2i_ref2_callback_takes_ref.con` | `run:42` | `ref2` | `binary,interp` |
| `tests/programs/error_2i_ref2_alias_fn_type_returns_ref.con` | `reject:E0236:REF-2@1` | `ref2g16` | `-` |
| `tests/programs/error_2i_ref2_unused_alias_fn_type_returns_ref.con` | `reject:E0236:REF-2@1` | `ref2g16` | `-` |
| `tests/programs/error_2i_ref2_field_fn_type_returns_ref.con` | `reject:E0236:REF-2@1` | `ref2g16` | `-` |
| `tests/programs/regress_2i_ref2_callback_field.con` | `run:42` | `ref2g16` | `binary,interp` |
| `tests/programs/regress_2i_ref2_trait_generic_shadows_alias.con` | `run:42` | `ref2g16` | `binary,interp` |

<a id="REF-3"></a>
#### REF-3. No reference is stored

**REF-3@2.** No struct or enum field, struct or enum literal payload, array element, binding, parameter or constant has a type that holds a reference anywhere except as the bare outer type of a binding or parameter.

- Kind: refusal
- Uses: REF-D2
- Codes: E0239 (check, post-mono)
- Status: pending R-0555
- Enforced by: Concrete.checkModule, Concrete.verifyNoStoredRefs
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** References remain second-class; OD54 removes newtypes, so they are no longer an aggregate case.

```con reject name=struct-field-decided code=E0239 stage=check pair=fn-pointer-field pending=R-0555 now=reject:E0239:check
struct W { t: &i64 }

fn main() -> u8 {
    return 0;
}
```

```con accept name=fn-pointer-field exit=42
struct H { f: fn(&i64) -> i64 }

fn g(r: &i64) -> i64 {
    return *r + 1;
}

fn example_value() -> i64 {
    let h: H = H { f: g };
    let H { f } = h;
    let x: i64 = 41;
    return f(&x);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_092_ref_struct_field.con` | `reject:E0239:REF-3@2` | `ref3` | `-` |
| `tests/programs/error_092_ref_in_option.con` | `reject:E0239:REF-3@2` | `ref3` | `-` |
| `tests/programs/error_092_ref_in_array.con` | `reject:E0239:REF-3@2` | `ref3` | `-` |
| `tests/programs/error_092_newtype_ref.con` | `reject:E0239:REF-3@2` | `ref3` | `-` |
| `tests/programs/error_092_alias_newtype_ref.con` | `reject:E0239:REF-3@2` | `ref3` | `-` |
| `tests/programs/error_092_alias_struct_field_ref.con` | `reject:E0239:REF-3@2` | `ref3` | `-` |
| `tests/programs/error_092_const_ref_array.con` | `reject:E0239:REF-3@2` | `ref3` | `-` |
| `tests/programs/error_escape_field.con` | `reject:E0239:REF-3@2` | `ref3` | `-` |
| `tests/programs/error_123_const_ref_type.con` | `reject:E0239:REF-3@2` | `ref3` | `-` |
| `tests/programs/regress_2i_ref3_fnptr_field.con` | `run:42` | `ref3` | `binary,interp` |
| `tests/programs/regress_092_refs_flow_down.con` | `run:42` | `ref3-heap` | `binary,interp-exempt:no-heap-allocation-in-the-interpreter` |

<a id="REF-4"></a>
#### REF-4. Generic instantiation preserves REF-1 and REF-3

**REF-4@1.** A type parameter is not instantiated at a type that contains a reference when it occurs in the generic's return type or behind a reference or pointer in one of its parameter types (inside a function type's parameters too), and no instantiation makes a field, binding, parameter or constant hold a reference in an aggregate.

- Kind: refusal
- Uses: REF-D1, REF-D2
- Codes: E0236 (check), E0239 (post-mono), E0238 (check, mono)
- Status: enforced
- Enforced by: Concrete.refuseNestedRefInstantiation, Concrete.refuseNestedRefSpecialization, Concrete.verifyNoStoredRefs
- Semantics: none
- Obligation: INV-REF (R-0500); substitution preserves the rules (PASS-1)
- Evidence: tested

**Rationale.** A generic body is checked once, at its opaque type parameter. Instantiating that
parameter at a reference type must not create a returned or stored reference that the check of
the body never saw. A top-level `x: T` parameter and a by-value callback `fn(T) -> U` are not
nested positions, so `ignore::<&i64>(&v, k)` and `apply::<&i64>(&v, read)` with
`f: fn(T) -> i64` are allowed, while `f: fn(&mut T)` puts `T` behind a reference and is refused.

```con reject name=generic-return code=E0236 stage=check pair=top-level-ref-arg
fn wrap<R>(r: R) -> R {
    return r;
}

fn example_value() -> i64 {
    let x: i64 = 42;
    let y: i64 = *wrap::<&i64>(&x);
    return y;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=top-level-ref-arg exit=42
fn ignore<T: Copy>(x: T, k: i64) -> i64 {
    discard(x);
    return k;
}

fn apply<T>(x: T, f: fn(T) -> i64) -> i64 {
    return f(x);
}

fn read(r: &i64) -> i64 {
    return *r;
}

fn example_value() -> i64 {
    let v: i64 = 21;
    return ignore::<&i64>(&v, 21) + apply::<&i64>(&v, read);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_2i_ref4_generic_return.con` | `reject:E0236:REF-4@1` | `ref4` | `-` |
| `tests/programs/error_092_generic_box_ref.con` | `reject:E0239:REF-4@1` | `ref4` | `json-exempt:post-mono` |
| `tests/programs/error_092_generic_array_ref.con` | `reject:E0239:REF-4@1` | `ref4` | `json-exempt:post-mono` |
| `tests/programs/error_091_norm_generic_fn_mut_param.con` | `reject:E0238:REF-4@1` | `ref4` | `-` |
| `tests/programs/error_091_norm_generic_transitive_ref.con` | `reject:E0238:REF-4@1` | `ref4` | `json-exempt:mono` |
| `tests/programs/regress_2i_ref4_top_level_ref_arg.con` | `run:42` | `ref4` | `binary,interp` |

<a id="REF-5"></a>
#### REF-5. A reference is a root binding plus projections

**REF-5@3.** In safe code a reference is formed only by borrowing a local or projecting a field or index through a reference, with `&s.f` abbreviating the projection of `&s` and every projection base a local or projection through a place, while temporaries and call results are bound before projection and the type and borrowing of static text are undecided: see the target Core proposal.

- Kind: refusal
- Uses: none
- Codes: E0238 (check)
- Status: pending R-0555
- Enforced by: Concrete.normPlace, Concrete.refuseRefValuedBranch, Concrete.checkRefArg, Concrete.recordRefProvenance, Concrete.refuseTemporaryBorrow
- Semantics: none
- Obligation: INV-REF (R-0500): every reference has a binding root
- Evidence: tested

**Rationale.** OD32 removes auto-borrow receivers and OD121 requires projection bases to be places. Static-text typing and borrowing are [undecided: see the target Core proposal](#core-static-text).

```con reject name=borrow-operator-decided code=E0238 stage=check pair=root-plus-projections pending=R-0555 now=reject:E0238:check
fn read(r: &i64) -> i64 {
    return *r;
}

fn example_value() -> i64 {
    return read(&(40 + 2));
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=root-plus-projections exit=42
struct P { n: i64 }
struct W { p: P }

fn get(r: &P) -> i64 {
    return r.n;
}

fn read(r: &i64) -> i64 {
    return *r;
}

fn through(r: &W) -> i64 {
    return get(&r.p) + read(&r.p.n);
}

fn example_value() -> i64 {
    let w: W = W { p: P { n: 10 } };
    let a: [i64; 2] = [0, 12];
    let out: i64 = get(&w.p) + read(&a[1]) + through(&w);
    let W { p } = w;
    let P { n } = p;
    return out + n * 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=borrow-int-literal-decided code=E0238 stage=check pair=borrow-let-first pending=R-0555 now=reject:E0238:check
fn read(r: &i64) -> i64 {
    return *r;
}

fn example_value() -> i64 {
    return read(&42);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=borrow-call-result-decided code=E0238 stage=check pair=borrow-let-first pending=R-0555 now=reject:E0238:check
fn mk() -> i64 {
    return 42;
}

fn read(r: &i64) -> i64 {
    return *r;
}

fn example_value() -> i64 {
    return read(&mk());
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=borrow-let-first exit=42
fn mk() -> i64 {
    return 42;
}

fn read(r: &i64) -> i64 {
    return *r;
}

fn example_value() -> i64 {
    let v: i64 = mk();
    return read(&v);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=borrow-reference-binding exit=42
fn g(rr: & &i64) -> i64 { discard(rr);
    return 42;
}

fn f(r: &i64) -> i64 {
    return g(&r);
}

fn example_value() -> i64 {
    let v: i64 = 1;
    return f(&v);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=branch-value-base-decided code=E0238 stage=check pair=bind-branch-first pending=R-0555 now=reject:E0238:check
struct Copy P { n: i64 }

fn example_value() -> i64 {
    let a: P = P { n: 42 };
    let b: P = P { n: 1 };
    let c: bool = true;
    return (if c { a } else { b }).n;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=bind-branch-first exit=42
struct Copy P { n: i64 }

fn example_value() -> i64 {
    let a: P = P { n: 42 };
    let b: P = P { n: 1 };
    let c: bool = true;
    let w: P = if c { a } else { b };
    return w.n;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_105_ref_valued_if_arg.con` | `reject:E0238:REF-5@3` | `ref5` | `-` |
| `tests/programs/error_105_ref_valued_if_field.con` | `reject:E0238:REF-5@3` | `ref5` | `-` |
| `tests/programs/error_091_norm_temporaries.con` | `reject:E0238:REF-5@3` | `ref5` | `-` |
| `tests/programs/error_091_norm_if_local_binding.con` | `reject:E0238:REF-5@3` | `ref5` | `-` |
| `tests/programs/error_091_norm_double_deref_read.con` | `reject:E0238:REF-5@3` | `ref5` | `-` |
| `tests/programs/error_091_norm_nested_ref_binder.con` | `reject:E0238:REF-5@3` | `ref5` | `-` |
| `tests/programs/error_2i_ref5_borrow_temporary.con` | `reject:E0238:REF-5@3` | `ref5` | `-` |
| `tests/programs/error_123_const_borrow_in_body.con` | `reject:E0238:REF-5@3` | `ref5` | `-` |
| `tests/programs/regress_2i_ref5_canonical_places.con` | `run:42` | `ref5` | `binary,interp` |
| `tests/programs/error_091_norm_if_projection.con` | `reject:E0238:REF-5@3` | `ref5-branch` | `-` |
| `tests/programs/regress_2i_ref5_bind_branch_first.con` | `run:42` | `ref5-branch` | `binary,interp` |
| `tests/programs/regress_2i_ref5_heap_field_borrow.con` | `run:42` | `ref5-heap` | `binary,interp-exempt:no-heap-allocation-in-the-interpreter` |
| `tests/programs/error_2i_ref5_const_ref_through_raw_ptr.con` | `reject:E0238:REF-5@3` | `ref5` | `-` |
| `tests/programs/error_d2_borrow_literal.con` | `reject:E0238:REF-5@3` | `ref5d2` | `-` |
| `tests/programs/error_d2_borrow_field_of_temporary.con` | `reject:E0238:REF-5@3` | `ref5d2` | `-` |
| `tests/programs/error_d2_temporary_receiver.con` | `reject:E0238:REF-5@3` | `ref5d2` | `-` |
| `tests/programs/error_d2_mut_borrow_string_literal.con` | `reject:E0238:REF-5@3` | `ref5d2` | `-` |
| `tests/programs/error_d2_branch_receiver.con` | `reject:E0238:REF-5@3` | `ref5d2` | `-` |
| `tests/regressions/bug182_borrowed_call_result_temporary/repro.con` | `reject:E0238:REF-5@3` | `ref5d2` | `-` |
| `tests/regressions/bug182_borrowed_call_result_temporary/repro_linear_leak.con` | `reject:E0238:REF-5@3` | `ref5d2` | `-` |
| `tests/programs/regress_d2_roots_and_projections.con` | `run:42` | `ref5d2` | `binary,interp` |

<a id="REF-6"></a>
#### REF-6. Reference bindings are never introduced by `let` and never assigned

**REF-6@2.** No `let` binds a reference, no reference binding (a parameter, a borrow-block binder or an arm-scoped pattern binder) is ever the target of an assignment, no constant initializer borrows (a cast to a reference type counting as a borrow), and no generic instantiated inside an `assert`, an `assume` or a ghost initializer is instantiated at a type that could hold a reference under substitution.

- Kind: refusal
- Uses: none
- Codes: E0302 (check, post-mono), E0303 (check, post-mono), E0234 (check), E0238 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt, Concrete.verifyNoRefBindingsPostMono
- Semantics: none
- Obligation: INV-REF (R-0500): a reference binding's source is fixed at its introduction
- Evidence: tested

**Rationale.** With both halves, a reference binding's source is fixed when the binding is
introduced. Every form that introduces one is lexically enclosed by the scope of its source, so
the flow-sensitive half of the rules disappears. The rewrite is to borrow at each use
(`f(&w.p); g(&w.p);`) or to borrow the owner in a borrow block. Writing *through* a reference
(`*r = v`) assigns the referenced place, not the binding (REF-7).

This entry also states two refusals under its tag and code (E0302). A
constant initializer is checked as an expression of the constant's type and may not
borrow at all, a cast to a reference type counting as a borrow (`constBorrows`). A generic
instantiated inside an `assert`, an `assume` or a ghost initializer may not be instantiated at a
type that could hold a reference under substitution (`erasedRefInstantiation`), because erased code
is never monomorphized, so the post-monomorphization half of this rule never sees it.

```con reject name=let-binds-ref code=E0302 stage=check pair=borrow-each-use
struct P { n: i64 }

fn get(r: &P) -> i64 {
    return r.n;
}

fn example_value() -> i64 {
    let p: P = P { n: 21 };
    let r: &P = &p;
    let out: i64 = get(r) + get(r);
    let P { n } = p;
    return out + n * 0;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=param-assigned code=E0303 stage=check pair=borrow-each-use
fn f(r: &i64, q: &i64) -> i64 {
    r = q;
    return *r;
}

fn example_value() -> i64 {
    let a: i64 = 1;
    let b: i64 = 42;
    return f(&a, &b);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=borrow-each-use exit=42
struct P { n: i64 }

fn get(r: &P) -> i64 {
    return r.n;
}

fn example_value() -> i64 {
    let p: P = P { n: 21 };
    let out: i64 = get(&p) + get(&p);
    let P { n } = p;
    return out + n * 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=constant-borrows code=E0302 stage=check pair=constant-and-erased-value
struct Copy P { n: i64 }

const X: P = P { n: 42 };

fn read(r: &P) -> i64 {
    return r.n;
}

const C: i64 = read(&X);

fn example_value() -> i64 {
    return C;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=erased-generic-at-ref code=E0302 stage=check pair=constant-and-erased-value
fn same<T: Copy>(x: T) -> bool {
    return true;
}

fn example_value() -> i64 {
    let v: i64 = 42;
    assert same::<&i64>(&v);
    return v;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=constant-and-erased-value exit=42
struct Copy P { n: i64 }

const X: P = P { n: 42 };

fn example_value() -> i64 {
    let x: P = X;
    assert x.n == 42;
    return x.n;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_2i_ref6_let_shared_ref.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref6_let_inferred_alias.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref6_let_mut_ref.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref6_let_ref_to_ref.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/error_104_assigned_ref_heap_block.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/error_104_assigned_ref_stack_block.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/error_091_ref_repointed_in_branch.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/error_091_mut_receiver_reassigned_ref.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/error_borrow_escape.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref8_free_under_projected_borrow.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/error_2i_ref8_assign_under_shared.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/error_named_ref_mut_conflict.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/error_091_norm_copied_mut_ref.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/error_091_norm_assign_under_shared_ref.con` | `reject:E0302:REF-6@2` | `ref6-place` | `-` |
| `tests/programs/regress_2i_ref6_place_each_use.con` | `run:42` | `ref6-place` | `binary,interp` |
| `tests/programs/regress_2i_ref6_index_once.con` | `run:42` | `ref6-place` | `binary,interp` |
| `tests/programs/error_2i_ref6_borrow_block_copy.con` | `reject:E0302:REF-6@2` | `ref6-block` | `-` |
| `tests/programs/error_2i_ref6_borrow_block_paren_copy.con` | `reject:E0302:REF-6@2` | `ref6-block` | `-` |
| `tests/programs/error_2i_ref6_borrow_block_cast_copy.con` | `reject:E0302:REF-6@2` | `ref6-block` | `-` |
| `tests/programs/error_2i_ref6_borrow_block_reborrow_escape.con` | `reject:E0302:REF-6@2` | `ref6-block` | `-` |
| `tests/programs/regress_2i_ref6_borrow_block_projection.con` | `run:42` | `ref6-block` | `binary,interp` |
| `tests/programs/error_2i_ref6_generic_let_at_ref.con` | `reject:E0302:REF-6@2` | `ref6-generic` | `json-exempt:post-mono` |
| `tests/programs/error_2i_ref6_generic_assign_at_ref.con` | `reject:E0303:REF-6@2` | `ref6-generic` | `json-exempt:post-mono` |
| `tests/programs/regress_2i_ref6_generic_assign_at_value.con` | `run:42` | `ref6-generic` | `binary,interp` |
| `tests/programs/error_2i_ref6_param_repointed.con` | `reject:E0303:REF-6@2` | `ref6-use` | `-` |
| `tests/programs/error_2i_ref6_borrow_binder_assigned.con` | `reject:E0303:REF-6@2` | `ref6-use` | `-` |
| `tests/programs/error_2i_ref6_arm_binder_assigned.con` | `reject:E0303:REF-6@2` | `ref6-use` | `-` |
| `tests/programs/error_131_param_repointed_block_local.con` | `reject:E0303:REF-6@2` | `ref6-use` | `-` |
| `tests/programs/error_131_param_repointed_param.con` | `reject:E0303:REF-6@2` | `ref6-use` | `-` |
| `tests/programs/regress_2i_ref6_arm_binder_through_ref.con` | `run:42` | `ref6-use` | `binary,interp` |
| `tests/programs/error_2i_ref6_let_else_through_ref.con` | `reject:E0302:REF-6@2` | `ref6-let-else` | `-` |
| `tests/programs/regress_2i_ref6_method_let_else.con` | `run:42` | `ref6-let-else` | `binary,interp` |
| `tests/programs/error_2i_ref6_nested_let_else_through_ref.con` | `reject:E0302:REF-6@2` | `ref6-let-else` | `-` |
| `tests/programs/regress_2i_nested_let_destructure.con` | `run:42` | `ref6-let-else` | `binary,interp` |
| `tests/programs/error_2i_ref6_ghost_let_at_type_param.con` | `reject:E0302:REF-6@2` | `ref6-generic` | `-` |
| `tests/programs/error_2i_ref6_struct_destructure_through_ref.con` | `reject:E0302:REF-6@2` | `ref6-destructure` | `-` |
| `tests/programs/regress_2i_ref6_struct_fields_through_ref.con` | `run:42` | `ref6-destructure` | `binary,interp` |
| `tests/programs/error_2i_ref6_erased_assert_let.con` | `reject:E0302:REF-6@2` | `ref6-generic` | `-` |
| `tests/programs/error_2i_ref6_erased_assert_assign.con` | `reject:E0303:REF-6@2` | `ref6-generic` | `-` |
| `tests/programs/error_2i_ref6_ghost_initializer_let.con` | `reject:E0302:REF-6@2` | `ref6-generic` | `-` |
| `tests/programs/regress_2i_ref6_ghost_ptr_and_fn_types.con` | `run:42` | `ref6-generic` | `binary,interp` |
| `tests/programs/regress_2i_value_block_destructure.con` | `run:42` | `ref6-let-else` | `binary,interp,fmt` |
| `tests/programs/error_2i_ref6_const_initializer.con` | `reject:E0302:REF-6@2` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_branch_borrow.con` | `reject:E0302:REF-6@2` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_destructure_through_borrow.con` | `reject:E0302:REF-6@2` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_match_through_borrow.con` | `reject:E0302:REF-6@2` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_typed_ref_let.con` | `reject:E0302:REF-6@2` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_borrow_block.con` | `reject:E0302:REF-6@2` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_cast_to_ref.con` | `reject:E0302:REF-6@2` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_const_aliased_ref.con` | `reject:E0302:REF-6@2` | `const-init` | `-` |
| `tests/programs/error_2i_ref6_erased_forwarded_generic.con` | `reject:E0302:REF-6@2` | `ref6-erased-call` | `-` |
| `tests/programs/error_2i_ref6_erased_generic_call.con` | `reject:E0302:REF-6@2` | `ref6-erased-call` | `-` |
| `tests/programs/regress_2i_ref6_generic_call_outside_assert.con` | `run:42` | `ref6-erased-call` | `binary,interp` |
| `tests/programs/regress_2i_const_nested_if_statement.con` | `run:42` | `const-init` | `binary,interp-exempt:bug-117-the-interpreter-has-no-module-constants` |
| `tests/programs/error_2i_ref6_mut_param_repointed.con` | `reject:E0303:REF-6@2` | `ref6-write` | `-` |
| `tests/programs/regress_2i_ref6_write_through_param.con` | `run:42` | `ref6-write` | `binary,interp` |

<a id="REF-7"></a>
#### REF-7. No reference is written through a reference

**REF-7@2.** No assignment through a reference stores a value whose type holds a reference.

- Kind: refusal
- Uses: REF-D2
- Codes: E0238 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** `*slot = &local` with `slot: &mut &T` would leave the caller holding a reference
to the callee's local. A reference behind a reference (`& &T`) can exist as an argument, but
the inner reference is never read out of it (REF-5) and never written through it (this rule).

```con reject name=store-through-ref-decided code=E0238 stage=check pair=write-value-decided pending=R-0555 now=reject:E0001:parse
fn replace(slot: &mut &i64) -> Unit {
    let local: i64 = 42;
    *slot = &local;
}

fn example_value() -> i64 {
    let original: i64 = 0;
    borrow original as r {
        replace(&mut r);
    }
    return original;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=write-value-decided exit=42 pending=R-0555 now=reject:E0108:resolve
fn set(slot: &mut i64) -> Unit {
    *slot = 42;
}

fn example_value() -> i64 {
    let mut x: i64 = 0;
    set(&mut x);
    return x;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_091_norm_ref_write_through_ref.con` | `reject:E0238:REF-7@2` | `ref7` | `-` |
| `tests/programs/regress_2i_ref7_write_value_through_mut_ref.con` | `run:42` | `ref7` | `binary,interp` |

<a id="REF-8"></a>
#### REF-8. A reference's root stays the same live binding

**REF-8@3.** While a reference exists (a borrow-block binder for its block, an arm binder for its arm, a borrow argument for its call), its root binding is not moved, consumed, freed or shadowed and is not accessed in a mode that conflicts with the reference, and the borrows a single call makes do not conflict.

- Kind: refusal
- Uses: REF-D3
- Codes: E0203 (check), E0204 (check), E0218 (check), E0231 (check), E0232 (check), E0235 (check), E0293 (check)
- Status: pending R-0555
- Enforced by: Concrete.consumeVar, Concrete.checkCallBorrowConflicts, Concrete.checkWriteAccess
- Semantics: none
- Obligation: INV-REF (R-0500): the root is the same live binding while the reference lives
- Evidence: tested

**Rationale.** REF-6 fixes which binding a reference comes from. This rule keeps that binding
alive and unchanged for as long as the reference can be used. A parameter's source lives in a
caller frame, where the caller's call borrow protects it. Two borrows conflict when at least
one is exclusive. Since REF-11 the key is the root alone. Because REF-9 makes every arm binder
through a reference a reference, a Copy payload binder is a loan too, and writing its root
inside the arm is refused (the G17 Copy case, closed 2026-10-23).

**Codes by case** (the compiler's ledger descriptions). E0203: the root is used while a borrow
block has frozen it (read, freed or moved inside the block). E0204: a linear root is moved while it
is borrowed. E0218: a frozen root is assigned. E0231: a root already mutably borrowed is borrowed
again. E0232: a root already borrowed is mutably borrowed. E0235: a root already immutably borrowed
is mutably borrowed. E0293: two borrows that one call makes, at least one `&mut`, overlap as places,
one path a prefix of the other (`&mut w` with `&mut w.f`); disjoint projections that conflict only
through their root are E0293 under REF-11 (`checkCallBorrowConflicts`).

```con reject name=free-in-borrow-block-decided code=E0203 stage=check pair=free-after-block-decided pending=R-0555 now=reject:E0001:parse
struct R { v: u8 }
fn take(r: R) -> u8 { let R { v } = r; return v; }
fn main() -> u8 { let r: R = R { v: 42 }; let mut out: u8 = 0; borrow r as rr { out = take(r) + rr.v; } return out; }
```

```con reject name=read-owner-in-borrow-block-decided code=E0203 stage=check pair=free-after-block-decided pending=R-0555 now=reject:E0001:parse
fn example_value() -> i64 {
    let x: i64 = 42;
    let mut out: i64 = 0;
    borrow x as r {
        out = x;
    }
    return out;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con reject name=call-overlap-decided code=E0293 stage=check pair=free-after-block-decided pending=R-0555 now=reject:E0293:check
fn mix(a: &i64, b: &mut i64) -> i64 {
    *b = 1;
    return *a;
}

fn example_value() -> i64 {
    let mut x: i64 = 42;
    return mix(&x, &mut x);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=free-after-block-decided exit=42 pending=R-0555 now=reject:E0001:parse
struct R { v: u8 }
fn main() -> u8 {
 let r: R = R { v: 42 }; let mut out: u8 = 0;
 borrow r as rr { out = rr.v; }
 let R { v: _ } = r;
 return out;
}
```

```con reject name=trusted-frozen-owner-decided code=E0203 stage=check pair=free-after-block-decided pending=R-0555 now=reject:E0001:parse
struct R { v: i64 }

fn peek(r: &R) -> i64 {
    return r.v;
}

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

trusted fn use_frozen(r: R) -> i64 {
    let mut s: i64 = 0;
    borrow r as rr {
        s = peek(rr) + take(r);
    }
    return s;
}

fn example_value() -> i64 {
    return use_frozen(R { v: 21 });
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=copy-binder-root-written-decided code=E0232 stage=check pair=free-after-block-decided pending=R-0555 now=reject:E0232:check
struct Copy P { n: i64 }

fn example_value() -> i64 {
    let mut o: Option<P> = Option::Some { value: P { n: 42 } };
    let mut out: i64 = 0;
    match &o {
        Option::Some { value } => {
            o = Option::None;
            out = value.n;
        },
        Option::None => {}
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_borrow_frozen.con` | `reject:E0203:REF-8@3` | `ref8-block` | `-` |
| `tests/programs/error_2i_ref8_free_in_borrow_block.con` | `reject:E0203:REF-8@3` | `ref8-block` | `-` |
| `tests/programs/regress_2i_ref8_free_after_ref_block.con` | `run:42` | `ref8-block` | `binary,interp-exempt:no-heap-allocation-in-the-interpreter` |
| `tests/programs/error_2i_ref8_call_overlap.con` | `reject:E0293:REF-8@3` | `ref8-call` | `-` |
| `tests/programs/error_091_norm_cast_arg_overlap.con` | `reject:E0293:REF-8@3` | `ref8-call` | `-` |
| `tests/programs/error_od2_call_overlapping_path.con` | `reject:E0293:REF-8@3` | `ref8-call` | `-` |
| `tests/programs/regress_2i_ref8_call_disjoint.con` | `run:42` | `ref8-call` | `binary,interp` |
| `tests/programs/regress_2i_ref8_match_borrow_scrutinee.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/error_181_root_reassigned_match.con` | `reject:E0232:REF-8@3` | `ref8-arm` | `-` |
| `tests/programs/error_181_root_reassigned_guard.con` | `reject:E0232:REF-8@3` | `ref8-arm` | `-` |
| `tests/programs/error_181_root_reassigned_nested.con` | `reject:E0232:REF-8@3` | `ref8-arm` | `-` |
| `tests/programs/error_181_root_reassigned_while_let.con` | `reject:E0232:REF-8@3` | `ref8-arm` | `-` |
| `tests/programs/error_181_root_reassigned_whole_binder.con` | `reject:E0232:REF-8@3` | `ref8-arm` | `-` |
| `tests/programs/regress_181_borrowed_binders.con` | `run:42` | `ref8-arm` | `binary,interp` |

<a id="REF-9"></a>
#### REF-9. A binder through a reference is a reference

**REF-9@2.** Every pattern binder that binds through a scrutinee of reference type has reference type, `&T` for a payload of type `T` and the scrutinee's own reference type for a whole-value binder, whether or not `T` is Copy.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.payloadBinderTy, Concrete.wholeBinderTy
- Semantics: none
- Obligation: binder modes are fixed before specialization (PASS-2)
- Evidence: tested

**Rationale.** Borrowed scrutinees bind references without moving their payloads; OD56 removes range patterns.

```con accept name=copy-payload-is-ref exit=42
fn example_value() -> i64 {
    let o: Option<i64> = Option::Some { value: 42 };
    match &o {
        Option::Some { value } => { return *value; },
        Option::None => { return 0; }
    }
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="REF-10"></a>
#### REF-10. A `&mut` argument is an implicit reborrow

**REF-10@4.** An exclusive-reference binding is lent to a call until that call returns without a reborrow operation or copy-in/copy-out, retains its root and lifetime, and like every binding is used or named `_`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.consumeCallArgIdent
- Semantics: none
- Obligation: the reborrow is explicit in Core (R-0496)
- Evidence: tested

**Rationale.** OD2 gives lending without reborrow or write-back, and OD62 requires an exclusive-reference name to be used or `_` even though it owns no resource.

```con accept name=reborrow-in-borrow-block-decided exit=42 pending=R-0555 now=reject:E0001:parse
fn inc(r: &mut i64) -> Unit {
    *r = *r + 21;
}

fn example_value() -> i64 {
    let mut x: i64 = 0;
    borrow mut x as r {
        inc(r);
        inc(r);
    }
    return x;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="REF-11"></a>
#### REF-11. Loans are keyed by root binding

**REF-11@4.** A borrow of any part of a value is a loan on its root binding and freezes the whole owner while it lives, so within one call every argument, the receiver included and a re-lent `&mut` binding included, conflicts with any other argument that reads or borrows the same root when at least one of them is exclusive, whatever their projections.

- Kind: refusal
- Uses: none
- Codes: E0293 (check)
- Status: pending R-0555
- Enforced by: Concrete.pathsOverlap, Concrete.checkCallRootReads
- Semantics: none
- Obligation: INV-REF (R-0500); the loan checker needs no path-overlap analysis
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD15, part (a)): a live borrow of any part of a
value freezes the whole owner, and loans are keyed by the root binding, as OD1 put it and as the current compiler
already enforces. This tightens OD2's part 2 as first recorded the same day: an exclusive argument
conflicts with any other argument that borrows the same root, so even disjoint fields conflict and
`h(&mut a.f, &mut a.g)` is refused, as are `upd(&mut s.a, &s.b)` and `add(&mut s.a, s.b)` (E0293);
the version recorded earlier that day, which allowed disjoint siblings, was never published. Two
fields of one owner are lent one call at a time, or read into a `let` first. A conflict between a borrow and a later use of its
place, outside one call, is REF-8's. History: the rule dates from 2026-10-23 (TIGHTENING_DECISIONS
D2); the measured migration was one std site (`std/src/cli.con:77`) and this fixture, and
`error_d2_call_disjoint_fields.con`, `error_d2_call_field_read.con`,
`error_d2_receiver_projection_read.con`, `error_d2_call_field_read_in_branch.con` and
`error_d2_explicit_mut_receiver_read.con` pin it in run_tests.sh. No pass may assume or exploit this
exclusivity until it is proved (OD15, part (b), REF-14).

The owner decided OD23 on 2026-10-29: the overlap check covers every argument, the receiver and
re-lent `&mut` bindings included, so `r.add(r.step)` with `r: &mut S` is refused; the Copy field is
read into a `let` first. The current compiler still exempts a re-lent reference binding (bug 319), pinned by the
pending example below.


```con reject name=disjoint-fields-one-call-decided code=E0293 stage=check pair=fields-in-sequence-decided pending=R-0555 now=reject:E0108:resolve
struct Copy W { a: i64, b: i64 }

fn two(a: &mut i64, b: &mut i64) -> Unit {
    *a = 1;
    *b = 1;
}

fn example_value() -> i64 {
    let mut w: W = W { a: 0, b: 0 };
    two(&mut w.a, &mut w.b);
    return w.a + w.b;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=fields-in-sequence-decided exit=2 pending=R-0555 now=reject:E0108:resolve
struct Copy W { a: i64, b: i64 }

fn one(a: &mut i64) -> Unit {
    *a = 1;
}

fn example_value() -> i64 {
    let mut w: W = W { a: 0, b: 0 };
    one(&mut w.a);
    one(&mut w.b);
    return w.a + w.b;
}

fn main() -> u8 {

    if example_value() == 2 {

        return 2;

    }

    return 1;

}
```

```con reject name=relend-receiver-read code=E0293 stage=check pair=relend-read-first-decided pending=R-0555 now=reject:E0108:resolve
struct S { step: i64, acc: i64 }
impl S {
    fn add(&mut self, n: i64) -> Unit {
        self.acc = self.acc + n;
    }
}
fn bump(r: &mut S) -> Unit {
    r.add(r.step);
}
fn example_value() -> i64 {
    let mut s: S = S { step: 42, acc: 0 };
    bump(&mut s);
    let S { step, acc } = s;
    return acc;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=relend-read-first-decided exit=42 pending=R-0555 now=reject:E0108:resolve
struct S { step: i64, acc: i64 }
impl S {
    fn add(&mut self, n: i64) -> Unit {
        self.acc = self.acc + n;
    }
}
fn bump(r: &mut S) -> Unit {
    let n: i64 = r.step;
    r.add(n);
}
fn example_value() -> i64 {
    let mut s: S = S { step: 42, acc: 0 };
    bump(&mut s);
    let S { step, acc } = s;
    return acc;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_d2_call_disjoint_fields.con` | `reject:E0293:REF-11@4` | `ref11` | `-` |
| `tests/programs/error_d2_call_field_read.con` | `reject:E0293:REF-11@4` | `ref11` | `-` |
| `tests/programs/error_d2_receiver_projection_read.con` | `reject:E0293:REF-11@4` | `ref11` | `-` |
| `tests/programs/error_d2_call_field_read_in_branch.con` | `reject:E0293:REF-11@4` | `ref11` | `-` |
| `tests/programs/error_d2_explicit_mut_receiver_read.con` | `reject:E0293:REF-11@4` | `ref11` | `-` |
| `tests/programs/regress_2i_ref8_call_disjoint.con` | `run:42` | `ref11` | `binary,interp` |

<a id="REF-12"></a>
#### REF-12. Nothing is mutated through a shared reference

**REF-12@2.** No place reached through a shared reference is mutated: it is not borrowed with `&mut`, not the receiver of a `&mut self` method and not assigned.

- Kind: refusal
- Uses: REF-D1
- Codes: E0237 (elab, check)
- Status: pending R-0555
- Enforced by: Concrete.checkMutAccess, Concrete.checkWriteAccess
- Semantics: none
- Obligation: INV-REF (R-0500): a shared reference's target is not written while it lives
- Evidence: tested

**Rationale.** Shared access is never upgraded to mutable access; OD32 requires explicit reference receivers.

```con reject name=assign-through-shared-decided code=E0237 stage=check pair=assign-through-exclusive-decided pending=R-0555 now=reject:E0108:resolve
struct Copy P { n: i64 }

fn set(p: &P) -> Unit {
    p.n = 42;
}

fn example_value() -> i64 {
    let p: P = P { n: 0 };
    set(&p);
    return p.n;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=assign-through-exclusive-decided exit=42 pending=R-0555 now=reject:E0108:resolve
struct Copy P { n: i64 }

fn set(p: &mut P) -> Unit {
    p.n = 42;
}

fn example_value() -> i64 {
    let mut p: P = P { n: 0 };
    set(&mut p);
    return p.n;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```
<a id="REF-13"></a>
#### REF-13. A `&mut` refers to the place

**REF-13@1.** A `&mut` reference refers to the place it borrows, so a write through it is the write to that place at once, and no copy of the place is made at the borrow and written back at its end.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: INV-REF (R-0500)
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD2, part 3). Copy-in and copy-out is not part
of the language: a backend may copy only as an optimization proven equivalent, which REF-11's
exclusivity at a call makes possible. A borrow block's reference is the owner's own storage since
bug 305's fix, with no copy and no write-back on any exit; the current compiler still lowers a `&mut x` argument by
copying the owner into a slot and writing it back (bug 307, under repair), and the guarantee holds
once that lowering is replaced.

<a id="REF-14"></a>
#### REF-14. An alias derived from a loan lives no longer than the loan

**REF-14@1.** A pointer or other alias derived from a loan is valid only while that loan lives, and `&mut` exclusivity is a guarantee the checker enforces, which no compiler pass or optimization assumes or exploits until that guarantee is proved in Lean.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: the checker's exclusivity guarantee, proved over the Core semantics (R-0499, R-0496), before any pass relies on it
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD15, part (b)), the strict option. The language
guarantees `&mut` exclusivity and the checker enforces it (REF-8, REF-11), but a pass that exploits
it (alias metadata, reordering loads and stores across a call) would turn any checker bug into a
miscompile, so none may until the guarantee is a theorem (R-0499 with R-0496's Core semantics). Until
then the operational loan rules are the definition: a loan starts at its borrow and ends with the
borrow block, the arm or the call that holds it (REF-8), and a raw pointer or other alias derived
from it (G19) is valid only while it lives; using one afterwards is outside safe code's guarantee
(TRUST-3). The current compiler emits no alias assumption today (no `noalias` or alias metadata); the rule is
pending until a gate checks that and the Core semantics defines loan lifetimes (R-0496).

### Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- REF-6@2 (spec audit 12, 2026-10-30) states two refusals the compiler already enforced under this entry's tag and code (E0302): a borrowing constant initializer and an erased generic instantiated at a type that could hold a reference.
- REF-9@1 said a whole-value binder over `&mut T` is `&T` too; REF-9@2 makes it `&mut T`, the scrutinee reference itself.

- REF-D3@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; REF-D3@1 is superseded by version.
- REF-8@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; REF-8@1 is superseded by version.
- REF-10@4 (2026-11-03) brings the chapter into line with the recorded owner decisions; REF-10@3 is superseded by version.

- REF-3@2 (2026-11-03) supersedes REF-3@1 to state the recorded decision.

- REF-5@3 (2026-11-03) supersedes REF-5@2 to state the recorded decision.

- REF-7@2 (2026-11-03) supersedes REF-7@1; pending legacy conformance is explicit.

- REF-11@4 (2026-11-03) supersedes REF-11@3; pending legacy conformance is explicit.

- REF-12@2 (2026-11-03) supersedes REF-12@1; pending legacy conformance is explicit.


<a id="ch-memory"></a>
## Memory

Safe code owns memory through bindings and `Heap<T, A>` values and reaches other storage only through
references (the references chapter). Every local binding has an ownership state (MEM-D1). This
chapter covers initialization (MEM-3), field access through
references and heap shells (MEM-4), heap ownership (MEM-5) and its capability (MEM-7), array bounds (MEM-6) and an index proven out of bounds (MEM-9), indexing through a
reference (MEM-8) and the two
layers of the memory model the Core semantics defines (MEM-1, MEM-2, pending). The checker
tracks whole owners: a borrow freezes its whole root (REF-8, REF-11), with no field- or
element-granular borrows. The design records are [MEMORY_SEMANTICS.md](../archive/design-records/MEMORY_SEMANTICS.md)
and [MEMORY_GUARANTEES.md](../verification/MEMORY_GUARANTEES.md), which keeps the public safety claim and
its boundaries.

### Definitions

<a id="MEM-D1"></a>
#### MEM-D1. Ownership state

**MEM-D1@1.** At each program point a local binding is unconsumed, used (read or borrowed), consumed (moved), reserved (passed by value to a pending `defer`) or frozen (the owner of an enclosing borrow block, REF-D3), and reading or moving a Copy binding leaves its state unchanged, while a borrow block freezes it as any other.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.VarState
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** The rules key on these states: a consumed binding is not used again (OWN-1), a
linear binding is consumed or reserved before every exit (OWN-2), a reserved one is not moved
(OWN-12) and a frozen one is not accessed (REF-D3, REF-8). A Copy binding is frozen by a borrow
block like any other. A local passed by borrow to a pending `defer` is pinned, which is not a
state of its own: it stays unconsumed or used and may not be moved (OWN-12, OWN-16).

### Rules

<a id="MEM-1"></a>
#### MEM-1. Two-layer memory

**MEM-1@1.** Safe values live in allocations with identities, raw memory is CompCert's block-and-offset model of bytes and pointer fragments with permissions, and a representation relation connects the two layers.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0502
- Semantics: none
- Obligation: the typed-value and raw-memory abstraction lemma (R-0496, R-0502)
- Evidence: tested

**Rationale.** Safe code reasons about structured values and places. Raw operations and FFI need
bytes, offsets and pointer provenance. Keeping both layers, with one relation between them, lets
safe programs be verified without byte-level reasoning, while raw code still has an exact
semantics (VERIFIED_PIPELINE, "IR chain").

<a id="MEM-2"></a>
#### MEM-2. Addresses are quasi-concrete

**MEM-2@1.** A block has no numeric address until a pointer into it is exposed, and exposing a pointer is an operation with a semantic effect, never an erasable cast.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0502
- Semantics: none
- Obligation: a finite-address-space policy stated with the memory model (R-0502)
- Evidence: tested

**Rationale.** Treating every address as concrete makes optimizations unsound, and treating every
address as abstract makes integer-pointer round trips undefined. With quasi-concrete addresses,
only exposed blocks have numeric addresses, so unexposed blocks keep their freedom (VERIFIED_PIPELINE,
"Language decisions this requires"). The surface operations are TRUST-1's.

<a id="MEM-3"></a>
#### MEM-3. Every binding is initialized

**MEM-3@1.** A binding is always declared with a value (`let x: T = e`), so safe code never reads uninitialized memory.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseStmt
- Semantics: none
- Obligation: no uninitialized state in Core's value semantics (R-0496)
- Evidence: tested

**Rationale.** There is no deferred-initialization form: `let x: T;` is a syntax error, and an
assignment only replaces an already valid value, so no definite-assignment analysis is needed.
Indeterminate memory exists only behind trusted or `Unsafe` operations, as a named premise
(TRUST-2).

```con accept name=initialized-bindings exit=42
fn example_value() -> i64 {
    let mut x: i64 = 1;
    x = 42;
    return x;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="MEM-4"></a>
#### MEM-4. Field access dereferences at most one reference

**MEM-4@5.** A field access requires a struct or one reference to it and dereferences at most that reference; a borrow block opens exactly one heap layer without consuming it, preserving the root and lifetime and never upgrading shared access, while a `HeapArray` exposes only a bounded element view whose exact type is undecided: see the target Core proposal.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: one place representation in every consumer (R-0501, R-0504)
- Evidence: tested

**Rationale.** OD30 and OD36 require explicit heap opening by borrow block, including through a shell reference; the exact HeapArray view is [undecided: see the target Core proposal](#core-heap-array-view).



<a id="MEM-5"></a>
#### MEM-5. A `Heap` value owns one heap cell

**MEM-5@2.** `alloc(a: &mut Allocator<A>, v: T) with(Alloc)` returns `Result<Heap<T, A>, AllocFailure<T>>`, conserving `v` on failure and allocator validity in every outcome, while `*h` consumes a heap and releases its storage returning the payload, `destroy(h)` destroys it explicitly, and `free` is refused.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: allocation identities of Core's safe layer (MEM-1, R-0496)
- Evidence: tested

**Rationale.** The Allocation decision preserves inputs on failure, records allocator identity and release information without storing a safe allocator reference, and forbids giving up an allocator with live allocations; OD59 leaves consuming extraction and explicit destruction instead of `free`. Brand binders, dependency discipline and release metadata are [undecided: see the target Core proposal](#core-allocator-identities), and failure payloads are [undecided: see the target Core proposal](#core-allocation-contracts).



<a id="MEM-6"></a>
#### MEM-6. An array index is checked

**MEM-6@2.** Reading or writing `a[i]` aborts when `i` is negative or at least the array's length.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.lower
- Semantics: none
- Obligation: bounds are a trapping operation of Core; a pass removes the check only with a discharged-VC certificate (R-0493, R-0496)
- Evidence: tested

**Rationale.** One unsigned comparison catches both a negative index and one past the end, and the
abort is the same as an arithmetic trap (ARITH-3). There is no global way to turn bounds checks
off; an unchecked access would be a trusted operation with a named premise (TRUST-2).
Version 2 leaves the refusal of an index the compiler proves out of bounds to MEM-9, which names its
code (E0900).

```con accept name=index-in-bounds exit=42
fn example_value() -> i64 {
    let a: [i64; 3] = [10, 30, 2];
    let mut i: i64 = 0;
    let mut total: i64 = 0;
    while i < 3 {
        total = total + a[i];
        i = i + 1;
    }
    return total;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con abort name=index-out-of-bounds trap=out_of_bounds
fn example_value() -> i64 {
    let a: [i64; 3] = [1, 2, 3];
    let mut i: i64 = 0;
    i = i + 5;
    return a[i];
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

<a id="MEM-7"></a>
#### MEM-7. The heap builtins require `Alloc`

**MEM-7@2.** Allocation and allocating container or string operations require both an `Allocator<A>` value and `with(Alloc)` and report conserving failure, allocations retain their allocator identity in `Heap<T, A>`, `Vec<T, A>` or `String<A>`, and release through `*h` or `destroy` requires at most `Alloc` without an allocator argument.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.IntrinsicId.capability, Concrete.ccCheckExpr
- Semantics: none
- Obligation: capability containment over the call graph after specialization (R-0499, R-0510)
- Evidence: tested

**Rationale.** OD154 requires allocating string operations to declare Alloc; the Allocation decision also requires an allocator and conserving failure, and static text allocates nothing. Allocator reset consumes the old allocator with no live allocation or conflicting loan and returns a fresh identity; the discipline is [undecided: see the target Core proposal](#core-allocator-identities).



<a id="MEM-8"></a>
#### MEM-8. Indexing dereferences at most one reference

**MEM-8@2.** An index `a[i]`, read or written, whose base `a` is a `&[T; N]` or `&mut [T; N]` indexes the array the reference refers to, dereferencing that one reference.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: one place representation in every consumer (R-0501, R-0504)
- Evidence: tested

**Rationale.** This is MEM-4's one-hop rule for an index: the place is the array behind the
reference, bounds-checked as MEM-6 states, and a write through `&mut [T; N]` writes the caller's
array (REF-13). TYPE-7's "only an array is indexed" is read with this one dereference. Absorbed
from [KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) C10.

```con accept name=index-through-reference-decided exit=42 pending=R-0555 now=reject:E0108:resolve
fn get(a: &[i64; 3]) -> i64 {
    return a[1];
}

fn set(a: &mut [i64; 3]) -> Unit {
    a[2] = 38;
}

fn example_value() -> i64 {
    let mut arr: [i64; 3] = [1, 2, 3];
    set(&mut arr);
    return get(&arr) + arr[2] + 2;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="MEM-9"></a>
#### MEM-9. An index proven out of bounds is refused

**MEM-9@2.** An index that the compiler proves always out of bounds is refused in every function, including a `trusted` function or one declaring `Unsafe`.

- Kind: refusal
- Uses: none
- Codes: E0900 (runtime-safety)
- Status: pending R-0555
- Enforced by: Concrete.Report.provenViolationDiagnostics
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD118 applies the same proven-violation refusal in trusted and Unsafe functions. The proof criterion is [undecided: see the target Core proposal](#core-static-refusal); legacy interpreter behavior remains pinned where recorded.

```con accept name=index-proven-in-bounds exit=42
fn example_value() -> i64 {
    let a: [i64; 3] = [10, 30, 2];
    return a[0] + a[1] + a[2];
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=index-proven-out-of-bounds-decided code=E0900 stage=runtime-safety pair=index-proven-in-bounds interp-exempt=interp-runs-proven-violations pending=R-0555 now=reject:E0900:runtime-safety
fn example_value() -> i64 {
    let a: [i64; 3] = [10, 30, 2];
    return a[5];
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=index-proven-out-of-bounds-unrun code=E0900 stage=runtime-safety pair=index-proven-in-bounds pending=bug-340 now=reject:E0900:runtime-safety now-interp=accept:42
fn example_value() -> i64 {
    let a: [i64; 3] = [10, 30, 2];
    let c: bool = false;
    if c {
        return a[5];
    }
    return 42;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```
### Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- MEM-4@2 opened one `Heap` layer implicitly in a field access (a `HeapArray<T>` only directly, not through a reference), without consuming the heap pointer. MEM-4@3 recorded OD30 (2026-10-29) and was retired before it was enforced, as the registry records; MEM-4@4 (spec audit 8, 2026-10-30) states the same decision and names OPEN-36 for the spelling of the explicit form.

- MEM-4@5 (2026-11-03) brings the chapter into line with the recorded owner decisions; MEM-4@4 is superseded by version.
- MEM-5@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; MEM-5@1 is superseded by version.
- MEM-7@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; MEM-7@1 is superseded by version.
- MEM-9@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; MEM-9@1 is superseded by version.

- MEM-8@2 (2026-11-03) supersedes MEM-8@1; pending legacy conformance is explicit.


<a id="ch-traits"></a>
## Traits

Impl selection is a function. For a type and a trait there is at most one impl, and for a type and
a method name at most one method, and neither answer depends on the order in which impls, modules
or imports are written. The compiler judges this once, over the whole program, before it builds
any method table (`Concrete.Coherence.checkProgram`). TRAIT-1 to TRAIT-7 are what that judgment
refuses. They were filed as bug 186 ([record](../bugs/186_trait_impl_selection_is_a_search.md)).
TRAIT-8 to TRAIT-10 are the per-declaration checks Check and CoreCheck make after it: an impl
names a declared trait, conforms to its trait's signatures, and every bound a caller instantiates
is satisfied. TRAIT-11 states that destruction cannot fail.

An identity here is never a spelling. A trait is the compiler's builtin `Destroy` or a
declaration (its defining module and name; public traits are exported and explicitly imported). An
impl's type is the declaration its name resolves to at the impl site, through an import alias if
there is one, so `import m.{Foo as Bar}; impl Destroy for Bar` names `m`'s `Foo`.

Locality follows Austral's instance rule (Austral spec, "Instance Uniqueness") at module
granularity: an impl lives with its trait or with its type. Two consequences follow. Every impl of
one trait for one type is in one module, so uniqueness never needs the whole program. And a
module that defines neither cannot add an impl that changes what another module's code calls.

An ambiguous bound call uses `Trait::method(&x, ...)` (completed OD6, TRAIT-6).

### Definitions

<a id="TRAIT-D1"></a>
#### TRAIT-D1. Impl target

**TRAIT-D1@1.** The target of an impl is the type its type name resolves to at the impl site (a type declaration, a compiler enum, or a primitive by its canonical type), applied to the type arguments written after that name, where a name that is one of the impl's own type parameters is a blanket target.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Coherence.resolveHead, Concrete.Coherence.moduleImpls
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** Impl lookup uses declaration identity with explicit imports or qualification for child declarations (completed OD43); there is no sibling-submodule fallback.

### Rules

<a id="TRAIT-1"></a>
#### TRAIT-1. One impl of a trait per type

**TRAIT-1@1.** No two impls of one trait have targets that are equal up to the naming of their type parameters.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0590 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.pairwise
- Semantics: none
- Obligation: none
- Evidence: tested

**Revised by the owner 2026-11-05** ([row Q2.8](#decided-2026-11-05-revisions)): two impls with equal heads may coexist when Copy/Destroy disjointness (OD28) makes their domains disjoint, one bounded `T: Copy` and the other `T: Destroy`. No other coherence or orphan rule changes, and no disjunctive bound is added.

**Rationale.** Both impls used to enter the method tables under one spelling (`Foo_m`). Native
code then failed in `llvm-as` with a redefined function, and the interpreter refused only when
the call ran. Neither is a language rule; the refusal now happens at the impl.

Decided by the owner on 2026-10-29 (OD6): traits are coherent (this entry to TRAIT-7): one impl per trait and type, the orphan rule (TRAIT-3), an ambiguous method call qualified (TRAIT-6), and resolution by typed identity.

```con reject name=twice code=E0590 stage=check pair=once
trait Tr { fn m(&self) -> i64; }
struct Copy Foo { a: i64 }
impl Tr for Foo { fn m(&self) -> i64 { discard(self); return 42; } }
impl Tr for Foo { fn m(&self) -> i64 { discard(self); return 2; } }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=once exit=42
trait Tr { fn m(&self) -> i64; }
struct Copy Foo { a: i64 }
impl Tr for Foo { fn m(&self) -> i64 { discard(self); return 42; } }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_duplicate_impl.con` | `reject:E0590:TRAIT-1@1` | `coherent` | `-` |
| `tests/programs/error_186_duplicate_generic_impl.con` | `reject:E0590:TRAIT-1@1` | `coherent` | `-` |
| `tests/programs/error_186_duplicate_primitive_spelling.con` | `reject:E0590:TRAIT-1@1` | `primitive` | `-` |
| `tests/programs/regress_186_primitive_impl_spellings.con` | `run:42` | `primitive` | `binary,interp` |
| `tests/programs/regress_186_visible_targets.con` | `run:42` | `coherent` | `binary,interp` |
| `tests/programs/regress_186_coherent_impls.con` | `run:42` | `coherent` | `binary,interp` |

<a id="TRAIT-2"></a>
#### TRAIT-2. Impls of a trait do not overlap

**TRAIT-2@1.** No two impls of one trait have targets that unify when each impl's type parameters are read as variables.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0591 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.pairwise, Concrete.Coherence.unify
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A generic impl and a specialized one for the same type both apply to the
specialized instantiation. Before bug 186 the binary ran whichever body was emitted first. A
blanket impl (`impl<T> Tr for T`) unifies with every other impl of its trait. TRAIT-4 refuses
each of these impls on its own as well; TRAIT-2 names the pair.

```con reject name=generic-and-specialized code=E0591 stage=check pair=generic-only
trait Tr { fn m(&self) -> i64; }
struct Copy Box2<T> { v: T }
impl<T> Tr for Box2<T> { fn m(&self) -> i64 { discard(self); return 42; } }
impl Tr for Box2<i32> { fn m(&self) -> i64 { discard(self); return 2; } }
fn example_value() -> i64 { let b: Box2<i32> = Box2::<i32> { v: 0 }; return (&b).m(); }
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=generic-only exit=42
trait Tr { fn m(&self) -> i64; }
struct Copy Box2<T> { v: T }
impl<T> Tr for Box2<T> { fn m(&self) -> i64 { discard(self); return 42; } }
fn example_value() -> i64 { let b: Box2<i32> = Box2::<i32> { v: 0 }; return (&b).m(); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_overlap_generic_impl.con` | `reject:E0591:TRAIT-2@1` | `coherent` | `-` |
| `tests/programs/error_186_overlap_blanket_impl.con` | `reject:E0591:TRAIT-2@1` | `coherent` | `-` |

<a id="TRAIT-3"></a>
#### TRAIT-3. An impl lives with its trait or its type

**TRAIT-3@2.** An impl of a trait is declared in the module that declares the trait or in the module that declares its target's type, where the builtin `Destroy` is declared in no module.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0592 (check)
- Status: pending R-0555
- Enforced by: Concrete.Coherence.moduleImpls
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** This is Austral's instance locality: local trait for a local type, local trait
for a foreign type, or foreign trait for a local type, and never a foreign trait for a foreign
type. A submodule is its own module. Primitive types (`i64`, `u8`, …) are declared in no module,
so only a local trait can be implemented for them.

```con reject name=foreign-for-foreign-decided code=E0592 stage=check pair=with-its-type-decided pending=R-0555 now=reject:E0108:resolve
// spec-file: main.con
mod defs;
import defs.{Foo, make};
impl Destroy for Foo { fn destroy(self) -> Unit { let Foo { a: _ } = self; } }
pub fn main() -> u8 { let f: Foo = make(); f.destroy(); return 42; }

// spec-file: defs.con
pub struct Foo { pub a: i64 }
pub fn make() -> Foo { return Foo { a: 42 }; }

```

```con accept name=with-its-type-decided exit=42 pending=R-0555 now=reject:E0108:resolve
// spec-file: main.con
mod defs;
import defs.{Foo, make};
pub fn main() -> u8 { let f: Foo = make(); f.destroy(); return 42; }

// spec-file: defs.con
pub struct Foo { pub a: i64 }
pub fn make() -> Foo { return Foo { a: 42 }; }
impl Destroy for Foo { pub fn destroy(self) -> Unit { let Foo { a: _ } = self; } }

```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_orphan_impl.con` | `reject:E0592:TRAIT-3@2` | `coherent` | `-` |
| `tests/programs/error_186_orphan_sibling.con` | `reject:E0592:TRAIT-3@2` | `coherent` | `-` |

<a id="TRAIT-4"></a>
#### TRAIT-4. An impl covers every instantiation of its type

**TRAIT-4@1.** The target of an impl is a type that resolves to a single declaration, a compiler enum or a primitive, not a type parameter or an alias, applied to exactly the impl's own type parameters (and, for an inherent impl, then its capability parameters) in declaration order, as many as the declaration takes.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0593 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.moduleImpls
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Every pass after the parser keys an impl by its type's name alone, so an impl is
in fact an impl for every instantiation. TRAIT-4 makes the written impl say so. Before bug 186,
`impl Tr for Box2<i32>` let a `Box2<bool>` satisfy `T: Tr` and run the `i32` body: silent wrong
code, natively and under `--interp`. Specialized and blanket impls are not supported; adding
them needs targets keyed by more than the type's identity.

```con reject name=specialized code=E0593 stage=check pair=generic
trait Tr { fn m(&self) -> i64; }
struct Copy Box2<T> { v: i64 }
impl Tr for Box2<i32> { fn m(&self) -> i64 { discard(self); return 42; } }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let b: Box2<bool> = Box2::<bool> { v: 0 }; return call::<Box2<bool>>(&b); }
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=generic exit=42
trait Tr { fn m(&self) -> i64; }
struct Copy Box2<T> { v: i64 }
impl<T> Tr for Box2<T> { fn m(&self) -> i64 { discard(self); return 42; } }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let b: Box2<bool> = Box2::<bool> { v: 0 }; return call::<Box2<bool>>(&b); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_specialized_impl.con` | `reject:E0593:TRAIT-4@1` | `coherent` | `-` |
| `tests/programs/error_186_imported_alias_target.con` | `reject:E0593:TRAIT-4@1` | `coherent` | `-` |
| `tests/programs/error_186_builtin_arity.con` | `reject:E0593:TRAIT-4@1` | `coherent` | `-` |

<a id="TRAIT-5"></a>
#### TRAIT-5. One method of a name per type

**TRAIT-5@1.** No two impls whose targets name the same type declaration both declare a method of the same name, whether each impl is inherent or of a trait.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0594 (check)
- Status: enforced
- Enforced by: Concrete.Coherence.pairwise
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A method call on a known type (`x.m()`) has exactly one candidate, so the trait
is a function of the type and the method name. That is also why the compiler's internal method
spelling (`Type_method`) can omit the trait without two methods colliding. Before bug 186 two
traits of one type with a method `m` both became `Foo_m`: `--interp` called the one declared
first and native code failed in `llvm-as`.

```con reject name=two-traits code=E0594 stage=check pair=renamed
trait A { fn m(&self) -> i64; }
trait B { fn m(&self) -> i64; }
struct Copy Foo { a: i64 }
impl A for Foo { fn m(&self) -> i64 { discard(self); return 40; } }
impl B for Foo { fn m(&self) -> i64 { discard(self); return 2; } }
fn call<T: A>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f) + 2; }
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=renamed exit=42
trait A { fn m(&self) -> i64; }
trait B { fn n(&self) -> i64; }
struct Copy Foo { a: i64 }
impl A for Foo { fn m(&self) -> i64 { discard(self); return 40; } }
impl B for Foo { fn n(&self) -> i64 { discard(self); return 2; } }
fn call<T: A + B>(x: &T) -> i64 { return x.m() + x.n(); }
fn example_value() -> i64 { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_method_clash.con` | `reject:E0594:TRAIT-5@1` | `coherent` | `-` |
| `tests/programs/error_186_inherent_clash.con` | `reject:E0594:TRAIT-5@1` | `coherent` | `-` |

<a id="TRAIT-6"></a>
#### TRAIT-6. A bound call has one candidate

**TRAIT-6@2.** A method call on a type parameter with multiple bound traits declaring that method is refused unless written `Trait::method(&x, ...)`, resolved by the trait’s identity without relaxing coherence.

- Kind: refusal
- Uses: none
- Codes: E0595 (check)
- Status: pending R-0555
- Enforced by: Concrete.boundMethodCandidates, Concrete.checkExprCore
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The canonical OD6 rule is completed by the qualified `Trait::method(&x, ...)` syntax; public traits cross module boundaries by explicit import. Qualification does not weaken coherence.

```con reject name=two-bounds-decided code=E0595 stage=check pair=one-bound pending=R-0555 now=reject:E0595:check
trait A { fn m(&self) -> i64; }
trait B { fn m(&self) -> i64; }
fn call<T: A + B>(x: &T) -> i64 { return x.m(); }
pub fn main() -> u8 { return 42; }
```

```con accept name=one-bound exit=42
trait A { fn m(&self) -> i64; }
trait B { fn m(&self) -> i64; }
fn call<T: A>(x: &T) -> i64 { return x.m(); }
pub fn main() -> u8 { return 42; }
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_186_ambiguous_bound_method.con` | `reject:E0595:TRAIT-6@2` | `coherent` | `-` |

<a id="TRAIT-7"></a>
#### TRAIT-7. Impl selection does not depend on order

**TRAIT-7@1.** A program's meaning does not change when its modules, imports, impls or a function's bounds are written in another order.

- Kind: guarantee
- Uses: TRAIT-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.Coherence.checkProgram
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** TRAIT-1 to TRAIT-6 compare impls by identity and never take the first match, so
the order of declarations reaches no answer. The two fixtures are one program written in two
orders.

```con accept name=forward exit=42
trait A { fn m(&self) -> i64; }
trait B { fn n(&self) -> i64; }
struct Copy Foo { a: i64 }
impl A for Foo { fn m(&self) -> i64 { discard(self); return 40; } }
impl B for Foo { fn n(&self) -> i64 { discard(self); return 2; } }
fn call<T: A + B>(x: &T) -> i64 { return x.m() + x.n(); }
fn example_value() -> i64 { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=reversed exit=42
fn call<T: B + A>(x: &T) -> i64 { return x.m() + x.n(); }
impl B for Foo { fn n(&self) -> i64 { discard(self); return 2; } }
impl A for Foo { fn m(&self) -> i64 { discard(self); return 40; } }
struct Copy Foo { a: i64 }
trait B { fn n(&self) -> i64; }
trait A { fn m(&self) -> i64; }
fn example_value() -> i64 { let f: Foo = Foo { a: 0 }; return call::<Foo>(&f); }
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_186_coherent_impls.con` | `run:42` | `coherent` | `binary,interp` |
| `tests/programs/regress_186_coherent_impls_reversed.con` | `run:42` | `coherent` | `binary,interp` |

<a id="TRAIT-8"></a>
#### TRAIT-8. An impl names a declared trait

**TRAIT-8@1.** A trait impl names a trait the program declares or the builtin `Destroy`, and no program declares a trait of its own named `Destroy`.

- Kind: refusal
- Uses: none
- Codes: E0579 (core-check), E0580 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** `Destroy` is the one builtin trait; the compiler gives its impls their meaning
(the destructor), so a user declaration of the same name would be a second trait that the
destructor machinery takes for the builtin.

```con reject name=unknown-trait code=E0580 stage=core-check pair=declared-trait
struct Copy S { v: i64 }
impl Missing for S { fn m(&self) -> i64 { discard(self); return 1; } }
fn main() -> u8 { return 0; }
```

```con reject name=destroy-redeclared code=E0579 stage=core-check pair=declared-trait
trait Destroy { fn destroy(&self); }
fn main() -> u8 { return 0; }
```

```con accept name=declared-trait exit=42
trait Tr { fn m(&self) -> i64; }
struct Copy S { v: i64 }
impl Tr for S { fn m(&self) -> i64 { return self.v; } }
fn example_value() -> i64 { let s: S = S { v: 42 }; return (&s).m(); }
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TRAIT-9"></a>
#### TRAIT-9. An impl conforms to its trait

**TRAIT-9@2.** A trait impl defines every method of its trait with the trait method's receiver, parameter types and return type (reading `Self` as the impl's type), declares no type or capability parameters the trait method does not, and requires no capability the trait method does not declare, with `Destroy::destroy` requiring the one consuming receiver form `fn destroy(self)`.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0581 (core-check), E0582 (core-check), E0586 (core-check), E0587 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: a call through a bound is checked against the trait's signature and runs the impl's body (R-0499)
- Evidence: tested

**Rationale.** Generic code that calls a method through a bound is checked against the trait's
signature and then specialized to the impl's body without a second check, so any way an impl's
signature exceeds its trait's would pass something the generic caller never agreed to (bug 091).
Capabilities are the load-bearing case: a trait method with no `with(...)` admits only impls with
none (TRUST-4). OD60 requires the one consuming receiver form `fn destroy(self)`. Its capability bound is OWN-13's (E0584).

```con reject name=missing-method-decided code=E0581 stage=core-check pair=conforming-impl pending=R-0555 now=reject:E0581:core-check
trait Tr { fn m(&self) -> i64; fn n(&self) -> i64; }
struct Copy S { v: i64 }
impl Tr for S { fn m(&self) -> i64 { discard(self); return 1; } }
fn main() -> u8 { return 0; }
```

```con reject name=return-type-differs-decided code=E0582 stage=core-check pair=conforming-impl pending=R-0555 now=reject:E0582:core-check
trait Tr { fn m(&self) -> i64; }
struct Copy S { v: i64 }
impl Tr for S { fn m(&self) -> bool { return true; } }
fn main() -> u8 { return 0; }
```

```con reject name=receiver-differs-decided code=E0587 stage=core-check pair=conforming-impl pending=R-0555 now=reject:E0587:core-check
trait Tr { fn m(&self, n: i64) -> i64; }
struct Copy S { v: i64 }
impl Tr for S { fn m(&mut self, n: i64) -> i64 { return n; } }
fn main() -> u8 { return 0; }
```

```con reject name=capabilities-exceed-trait-decided code=E0586 stage=core-check pair=conforming-impl pending=R-0555 now=reject:E0586:core-check
trait Tr { fn m(&self) -> i64; }
struct Copy S { v: i64 }
impl Tr for S { fn m(&self) with(Fs) -> i64 { return self.v; } }
fn main() -> u8 { return 0; }
```

```con accept name=conforming-impl exit=42
trait Tr { fn m(&self, n: i64) -> i64; fn me(&self) -> Self; }
struct Copy S { v: i64 }
impl Tr for S {
    fn m(&self, n: i64) -> i64 { return self.v + n; }
    fn me(&self) -> S { return S { v: self.v }; }
}
fn example_value() -> i64 { let s: S = S { v: 40 }; let t: S = (&s).me(); return (&t).m(2); }
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TRAIT-10"></a>
#### TRAIT-10. A bound is satisfied by an impl

**TRAIT-10@3.** A type argument given for a type parameter with a bound satisfies every trait in the bound: `Copy` by being Copy, and `Destroy` and any other trait only by an impl of it, so a Copy type is not `Destroy`.

- Kind: refusal
- Uses: TRAIT-D1
- Codes: E0241 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkTraitBounds
- Semantics: none
- Obligation: every bound call has a candidate after specialization (TRAIT-6)
- Evidence: tested

**Rationale.** A bound is a promise the caller makes for the generic body: the body calls the
bound's methods on values of the parameter, and specialization then needs exactly one impl
(TRAIT-6). A missing impl is refused at the call that gives the type argument. A primitive type
argument needs an impl too, matched by its canonical type (`i64` and `i64` are one); before bug
204 it was not checked, and the program ran when the body called no bound method.

The recorded answers in [OD35](#od35) govern this subject; the frozen compiler’s earlier behavior is not an alternative rule.


```con reject name=bound-unsatisfied-decided code=E0241 stage=check pair=bound-satisfied pending=R-0555 now=reject:E0241:check
trait Tr { fn m(&self) -> i64; }
struct Copy S { v: i64 }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let s: S = S { v: 42 }; return call::<S>(&s); }
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con reject name=primitive-bound-unsatisfied-decided code=E0241 stage=check pair=bound-satisfied pending=R-0555 now=reject:E0241:check
trait Tr { fn m(&self) -> i64; }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let n: i64 = 42; return call::<i64>(&n); }
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=bound-satisfied exit=42
trait Tr { fn m(&self) -> i64; }
struct Copy S { v: i64 }
impl Tr for S { fn m(&self) -> i64 { return self.v; } }
fn call<T: Tr>(x: &T) -> i64 { return x.m(); }
fn example_value() -> i64 { let s: S = S { v: 42 }; return call::<S>(&s); }
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=copy-type-for-destroy-bound code=E0241 stage=check pair=unbounded-generic pending=bug-324 now=accept:42
fn id<T: Destroy>(x: T) -> T {
    return x;
}
fn example_value() -> i64 {
    return id(42);
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=unbounded-generic exit=42
fn id<T>(x: T) -> T {
    return x;
}
fn example_value() -> i64 {
    return id(42);
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="TRAIT-11"></a>
#### TRAIT-11. Destruction cannot fail

**TRAIT-11@2.** The method `destroy` of the builtin trait `Destroy` returns `Unit`, so destroying a value reports no failure.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Cleanup that can fail can be written as a consuming method that returns its outcome
(a `close(self) -> Result`); that is a library pattern, not part of this rule. A destructor runs from `destroy(x)`, from a deferred call and from a collection's
drop glue, none of which has a place to put an error, so an error there would be silently dropped
(OWN-6). An impl whose `destroy` returns anything else is refused by TRAIT-9 (E0582, `expected
return type ()`). Fallible cleanup, such as closing a file whose final flush can fail, is an
ordinary consuming method (`close(self) -> Result<Unit, E>`) that the caller matches. The receiver
form of `destroy` is the consuming `fn destroy(self)` (OD60), and the capabilities it may require are OWN-13's.

```con accept name=destroy-returns-unit-decided exit=42 pending=R-0555 now=reject:E0108:resolve
struct R { v: i64 }

impl Destroy for R {
    fn destroy(self) -> Unit {
        let R { v: _ } = self;
    }
}

fn main() -> u8 {
    let r: R = R { v: 1 };
    destroy(r);
    return 42;
}
```

### Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- TRAIT-10@2 records OD28 (2026-10-29): a `Destroy` bound is satisfied only by an impl.

- TRAIT-6@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRAIT-6@1 is superseded by version.

- TRAIT-3@2 (2026-11-03) supersedes TRAIT-3@1; pending legacy conformance is explicit.

- TRAIT-10@3 (2026-11-03) supersedes TRAIT-10@2; pending legacy conformance is explicit.

- TRAIT-11@2 (2026-11-03) supersedes TRAIT-11@1; pending legacy conformance is explicit.


<a id="ch-contracts"></a>
## Contracts

A contract is a claim, not a check the program runs: the compiler turns each claim into
obligations with stable identities and attaches evidence to each (proved by a Lean theorem or a
kernel decision procedure, assumed, or still open). Contracts and ghost code are erased before code
generation and never change what the program does. This chapter states what a contract means
(CON-1, CON-2), what a contract expression may contain (CON-3, pending its specification
judgment), how ghost code is kept apart from runtime code (CON-4), specification functions (CON-5),
`assert` and `assume` (CON-6), the scope of `result` (CON-7) and stacked clauses (CON-8). How obligations are
discharged and reported is the verification documentation:
[CONTRACTS_GUIDE.md](../verification/CONTRACTS_GUIDE.md) (now a usage guide),
[CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md) and
[EVIDENCE_CLASSES.md](../verification/EVIDENCE_CLASSES.md).

### Rules

<a id="CON-1"></a>
#### CON-1. Claims are partial correctness

**CON-1@1.** A contract claim states partial correctness, holding for every execution that terminates, and a function is claimed to terminate only when `decreasing` evidence for its loops and recursion is checked.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0473
- Semantics: none
- Obligation: claims are generated against Core semantics as partial-correctness statements, with total correctness only from checked `decreasing` measures (R-0473, R-0495)
- Evidence: tested

**Rationale.** Partial correctness is what a proof about a terminating execution can establish
without a termination argument. Termination is a separate, explicit claim, so a proof never
implies termination by accident. Divergence remains one of the three outcomes (STMT-1).

<a id="CON-2"></a>
#### CON-2. What `requires`, `ensures`, `invariant` and `variant` claim

**CON-2@1.** `#[requires(p)]` claims that `p` holds at every call (an obligation of each caller, assumed on entry), `#[ensures(q)]` claims that `q` holds at every return with `result` naming the returned value, and a loop's `#[invariant(i)]` and `#[variant(v)]` claim that `i` holds before the loop and after every iteration and that `v` is non-negative and decreases on every iteration.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Report.callSiteObligations, Concrete.Report.loopVCGoals
- Semantics: none
- Obligation: claims are generated against Core semantics with exact contract identity (R-0473, R-0495)
- Evidence: tested

**Rationale.** A claim is generated into obligations (a call site's precondition, a loop's
initialization, preservation, exit-to-postcondition, variant non-negativity and decrease), each
with its own evidence; nothing is reported green without it. Contracts never execute, so a program
with false contracts runs exactly as one without them; whether a claim is proved is a separate
question that `--report contracts` answers.

```con accept name=contracts-are-erased exit=42
#[requires(n >= 0)]
#[ensures(result == n)]
fn count(n: i64) -> i64 {
    let mut i: i64 = 0;
    #[invariant(0 <= i && i <= n)]
    #[variant(n - i)]
    while i < n {
        i = i + 1;
    }
    return i;
}

fn example_value() -> i64 {
    return count(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-3"></a>
#### CON-3. A contract expression is a pure specification

**CON-3@2.** Contract expressions are pure non-consuming specifications with mathematical integer arithmetic unless a machine operation is named; function clauses see parameters, permitted declarations and `result` only in `ensures`, postconditions read surviving parameter state after deferred cleanup without an implicit entry snapshot, and loop clauses and assertions use their own lexical scope including permitted ghost locals.

- Kind: refusal
- Uses: none
- Codes: E0320 (check), E0321 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: the specification judgment of contract expressions (R-0473)
- Evidence: tested

**Rationale.** OD143 and OD144 restrict function contracts to their specification scope and post-cleanup surviving state; the audit distinguishes loop and assertion lexical scope. No implicit old snapshot is introduced (OD117).

```con reject name=ill-typed-contract code=new stage=check pair=well-typed-contract pending=R-0473 now=accept:42
#[requires((x > 0) + 1 > 0)]
fn f(x: i32) -> i32 {
    return x;
}

fn example_value() -> i64 {
    return f(42) as i64;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=integer-requires-decided code=E0320 stage=check pair=well-typed-contract pending=R-0555 now=reject:E0320:check
#[requires(x + 1)]
fn f(x: i32) -> i32 {
    return x;
}

fn example_value() -> i64 {
    return f(42) as i64;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=spec-fn-calls-runtime-code-decided code=E0321 stage=check pair=well-typed-contract pending=R-0555 now=reject:E0321:check
fn twice(x: i32) -> i32 {
    return x + x;
}

spec fn big(x: i32) -> bool = twice(x) > 10;

fn main() -> u8 {
    return 42;
}
```

```con accept name=well-typed-contract exit=42
#[requires(x > 0)]
fn f(x: i32) -> i32 {
    return x;
}

fn example_value() -> i64 {
    return f(42) as i64;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-4"></a>
#### CON-4. Ghost code stays out of runtime code

**CON-4@2.** A `ghost let` binding is erased before code generation and may be read only by contracts and other ghost code, never by runtime code.

- Kind: refusal
- Uses: none
- Codes: E0420 (elab)
- Status: pending R-0555
- Enforced by: Concrete.throwElab
- Semantics: none
- Obligation: ghost erasure preserves runtime behaviour (R-0496)
- Evidence: tested

**Rationale.** A ghost value names something a proof needs and the program does not keep. Proof
code cannot change runtime ownership in the other direction either (OWN-11). Ghost destructuring
bindings are not supported. A ghost initializer is elaborated as runtime code today, so it cannot
read another ghost binding yet (the pending example below). E0420 names only this refusal; the
`append()` argument refusal that shared it is E0421 (TYPE-10, bug 203).

```con reject name=ghost-read-at-runtime-decided code=E0420 stage=elab pair=ghost-in-contract pending=R-0555 now=reject:E0420:elab
fn f() -> i64 {
    ghost let bound: i64 = 8;
    return bound;
}

fn example_value() -> i64 {
    return f();
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=ghost-in-contract exit=28
fn f() -> i64 {
    ghost let bound: i64 = 8;
    let mut acc: i64 = 0;
    let mut i: i64 = 0;
    #[invariant(0 <= i && i <= bound)]
    #[variant(bound - i)]
    while i < 8 {
        acc = acc + i;
        i = i + 1;
    }
    return acc;
}

fn example_value() -> i64 {
    return f();
}

fn main() -> u8 {

    if example_value() == 28 {

        return 28;

    }

    return 1;

}
```

```con accept name=ghost-reads-ghost exit=28 pending=R-0499 now=reject:E0420:elab
fn f() -> i64 {
    ghost let bound: i64 = 8;
    ghost let limit: i64 = bound;
    let mut acc: i64 = 0;
    let mut i: i64 = 0;
    #[invariant(0 <= i && i <= limit)]
    #[variant(limit - i)]
    while i < 8 {
        acc = acc + i;
        i = i + 1;
    }
    return acc;
}

fn example_value() -> i64 {
    return f();
}

fn main() -> u8 {

    if example_value() == 28 {

        return 28;

    }

    return 1;

}
```

<a id="CON-5"></a>
#### CON-5. A specification function is erased

**CON-5@2.** A `spec fn` requires a total body and is erased: no code is generated for it, and it is named only by contract expressions and the bodies of other `spec fn` declarations, never by runtime code.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.elabModule, Concrete.checkModule
- Semantics: none
- Obligation: ghost erasure preserves runtime behaviour (R-0496)
- Evidence: tested

**Rationale.** A specification function is a mathematical description that a claim names, as
`rotr_spec` names the function `rotr` must compute. Runtime code that names one is refused as an
unknown name (E0101, MOD-1), since only the contract tables resolve it; a `spec fn` body that calls
runtime code is E0321 (CON-3). OD115 requires a body and totality; the current compiler still accepts bodyless declarations and unchecked recursive bodies.
Absorbed from [CONTRACTS_GUIDE.md](../verification/CONTRACTS_GUIDE.md) and
[CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md).

```con accept name=spec-fn-in-contract exit=42
spec fn big(x: i64) -> bool = x > 10;

#[requires(big(x))]
fn f(x: i64) -> i64 {
    return x;
}

fn example_value() -> i64 {
    return f(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-6"></a>
#### CON-6. What `assert` and `assume` claim

**CON-6@1.** `assert e` claims that `e` holds where it is written, an obligation of the function, and `assume e` makes `e` a hypothesis of the obligations after it whose own evidence is `assumed`, never `proved`, and neither statement executes.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkStmt, Concrete.elabStmtEv, Concrete.Report.renderAssertAssume
- Semantics: none
- Obligation: claims are generated against Core semantics with exact contract identity (R-0473, R-0495)
- Evidence: tested

**Rationale.** `assume` is a proof-only trapdoor: a claim that leans on it is reported as depending
on an assumption, never as a clean proof, and a project policy may forbid it. An `assert` that is
never discharged stays `unproven`, and one whose condition is constant-false is reported as a
violation; neither stops the program, since both are erased (OWN-11 keeps them from changing
ownership). OD79 and OD80 include both statements in the grammar. Absorbed from
[CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md).

```con accept name=assert-and-assume-are-erased exit=42
fn f(x: i64) -> i64 {
    assume x > 0;
    assert x > 0;
    return x;
}

fn example_value() -> i64 {
    return f(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-7"></a>
#### CON-7. `result` is bound only in a postcondition

**CON-7@1.** `result` names the returned value in an `#[ensures]` clause and is bound in no other contract clause, so a `requires`, `invariant` or `variant` that names it names an unknown variable.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Only a postcondition is evaluated after a return. A precondition that names `result`
is refused as an undeclared variable (E0201, MOD-1). Outside contracts `result` is an ordinary
identifier. Absorbed from [KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) H27.

```con accept name=result-in-ensures exit=42
#[requires(x > 0)]
#[ensures(result == x)]
fn f(x: i64) -> i64 {
    let result: i64 = x;
    return result;
}

fn example_value() -> i64 {
    return f(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-8"></a>
#### CON-8. Stacked clauses are a conjunction

**CON-8@1.** Several `#[requires]` clauses on one function, or several `#[invariant]` clauses on one loop, claim their conjunction, each clause becoming an obligation of its own.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Report.callSiteObligations, Concrete.Report.loopVCGoals
- Semantics: none
- Obligation: claims are generated against Core semantics with exact contract identity (R-0473, R-0495)
- Evidence: tested

**Rationale.** CON-2 states one clause; writing two is the same claim as writing their `&&`, with
separate identities in the report (`R1`, `R2`). Absorbed from
[CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md).

```con accept name=stacked-requires exit=42
#[requires(x > 0)]
#[requires(x < 100)]
fn f(x: i64) -> i64 {
    return x;
}

fn example_value() -> i64 {
    return f(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

- CON-3@2 (2026-11-03) supersedes CON-3@1 to state the recorded decision.

- CON-4@2 (2026-11-03) supersedes CON-4@1; pending legacy conformance is explicit.


<a id="ch-trusted"></a>
## Trusted Code and Raw Pointers

Concrete's safety model has three parts. **Capabilities** (`with(Fs, Alloc)`) are the effects a
function may exercise, visible in its signature. **`trusted`** contains pointer-level implementation
techniques behind a safe API. **`Unsafe`** is the capability for crossing a foreign or raw-memory
boundary, and a `trusted` body absorbs it. No layer is exempt: compiler builtins, `std` and user
code follow the same rules. This chapter covers what a capability means (TRUST-D1), raw pointers (TRUST-1, TRUST-3), undefined
behaviour (TRUST-2), capabilities (TRUST-4), `trusted` (TRUST-5), the refusal of the trusted-module marker
(TRUST-6, pending), moving a value out of raw memory (TRUST-7), global state (TRUST-8 to TRUST-10), storing through a raw pointer (TRUST-11), the
ownership rules in trusted code (TRUST-12) and taking a reference's address (TRUST-13). The section
"Trusted premises" lists every premise the compiler relies on and does not check. Capability sets, capability parameters and the capabilities of function types and
handles (`Sink<cap C>`) are in the types chapter (TYPE-D1, TYPE-15, TYPE-17, TYPE-18); this specification governs them. The design records are [SAFETY.md](../verification/SAFETY.md),
[CAPABILITY_FACTS.md](../compiler/CAPABILITY_FACTS.md),
[HANDLE_CAPABILITIES.md](../archive/design-records/HANDLE_CAPABILITIES.md),
[CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md) and
[MEMORY_SEMANTICS.md](../archive/design-records/MEMORY_SEMANTICS.md) (§10 and §11, absorbed into
TRUST-11 and TRUST-12).

### Definitions

<a id="TRUST-D1"></a>
#### TRUST-D1. What a capability means

**TRUST-D1@2.** Capabilities are compile-time permissions with the reserved names `Fs`, `Network`, `Env`, `Time`, `Alloc`, `Device`, `Process` and `Unsafe`, distinct from resource values and never supplied by holding or returning a value, with no aliases or umbrella capabilities.

- Kind: definition
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.validCaps
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** Authority rules 1–2 and 13 distinguish permission sets from resource values. Fs covers filesystem operations on Directory/File, Network covers network grants and sockets, Env the Environment grant, Time the Clock grant, Alloc allocator operations, Device device access, Process process services, and Unsafe raw and foreign validity obligations. Holding a value grants no permission; exact target service contracts are [undecided: see the target Core proposal](#core-grants).

### Rules

<a id="TRUST-1"></a>
#### TRUST-1. Pointer arithmetic and address exposure are named operations

**TRUST-1@1.** Pointer arithmetic is written `ptr::offset`, and a conversion between a pointer and an integer is written `ptr::expose` or `ptr::from_exposed`, so `as` never converts between a pointer and an integer.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0555
- Semantics: none
- Obligation: offset, expose and from_exposed have their MEM-2 semantics (R-0502)
- Evidence: tested

**Rationale.** A pointer-to-integer `as` cast is lossy: it discards provenance and silently
exposes the block. Pointer arithmetic written as integer arithmetic on a cast loses which block
the result belongs to. Named operations make provenance and exposure explicit (MEM-2). std uses
the cast form today (`std/src/text.con`, `dst as u64 + i`), so the migration is std's
raw-pointer helpers.

```con reject name=pointer-to-integer-cast code=new stage=check pair=no-pointer-cast pending=R-0502 now=accept:0
trusted fn addr(p: *const i64) -> u64 {
    return p as u64;
}

fn main() -> u8 {
    return 0;
}
```

```con accept name=no-pointer-cast exit=0
fn main() -> u8 {
    return 0;
}
```

<a id="TRUST-2"></a>
#### TRUST-2. Safe code has no undefined behaviour

**TRUST-2@1.** Safe code has no undefined behaviour, and every undefined behaviour of the language is a named premise of a trusted operation that the trusted code's author discharges.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0506
- Semantics: none
- Obligation: every stuck state of Core semantics is a violated named premise (R-0496, R-0506)
- Evidence: tested

**Rationale.** A safe program either runs to a defined outcome (STMT-1) or is refused. Undefined
behaviour exists only where a trusted operation states a premise it cannot check, such as a raw
dereference needing an aligned, live and initialized pointer. Naming each premise makes the
trusted base a list that can be audited ([TRUSTED_COMPUTING_BASE.md](../verification/TRUSTED_COMPUTING_BASE.md)).

<a id="TRUST-3"></a>
#### TRUST-3. A reference from a raw pointer carries a premise

**TRUST-3@1.** In trusted or `Unsafe` code, `&*p` and `&mut *p` form a reference from a raw pointer `p` only under the premise that `p` is aligned, live and initialized for its type, and the reference then obeys every rule of the references chapter.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0506
- Semantics: none
- Obligation: the premise is a named premise of the trusted dereference (TRUST-2)
- Evidence: tested

**Rationale.** A raw-pointer dereference is not one of REF-5's two operations that form a
reference. Trusted code may form a reference this way, but only by discharging the validity
premise itself (TIGHTENING_DECISIONS D2, caveats). The resulting reference is second-class like
any other.

Decided by the owner on 2026-10-29 (OD1): a raw-pointer dereference needs a trusted validity premise.


<a id="TRUST-4"></a>
#### TRUST-4. A function exercises only the capabilities it declares

**TRUST-4@3.** Every body uses only its declared `with(...)` permissions and every call requires the callee’s set to be included in the caller’s across trait dispatch, specialization and cleanup, with `trusted` absorbing only `Unsafe` and all raw operations and reference-to-pointer conversions requiring `trusted` or `Unsafe`.

- Kind: refusal
- Uses: none
- Codes: E0121 (resolve), E0240 (check), E0242 (check), E0520 (core-check), E0521 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.ccCheckExpr, Concrete.checkExprCore
- Semantics: none
- Obligation: capability containment over the call graph after specialization (R-0499, R-0510)
- Evidence: tested

**Rationale.** Authority rules 5–7 require effect containment even inside trusted bodies; no Std or user umbrella alias exists. Intrinsic effects come from the primitive table (FFI-6); reference-to-pointer conversion is gated by OD98.

```con reject name=undeclared-capability-decided code=E0520 stage=core-check pair=declared-capability pending=R-0555 now=reject:E0520:core-check
fn needs() with(Network) -> i64 {
    return 1;
}

fn example_value() -> i64 {
    return needs();
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=raw-deref-needs-unsafe-decided code=E0521 stage=core-check pair=declared-capability pending=R-0555 now=reject:E0521:core-check
fn rd(p: *const i64) -> i64 {
    return *p;
}

fn main() -> u8 {
    return 0;
}
```

```con reject name=capability-variable-named-unsafe-decided code=E0121 stage=resolve pair=declared-capability pending=R-0555 now=reject:E0121:resolve
fn apply<cap Unsafe>(f: fn(i64) with(Unsafe) -> i64) with(Unsafe) -> i64 {
    return f(41);
}

fn add_one(x: i64) -> i64 {
    return x + 1;
}

fn example_value() -> i64 {
    return apply(add_one);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=declared-capability exit=42
fn needs() with(Network) -> i64 {
    return 41;
}

fn rd(p: *const i64) with(Unsafe) -> i64 {
    return *p;
}

fn example_value() with(Network, Unsafe) -> i64 {
    let x: i64 = 1;
    return needs() + rd(&x as *const i64);
}

fn main() with(Network, Unsafe) -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-5"></a>
#### TRUST-5. `trusted` absorbs `Unsafe` and nothing else

**TRUST-5@2.** Trust is declared per function, never on an `impl` block, and a `trusted fn` may perform raw-pointer operations and call an `extern fn` without declaring `Unsafe`, while every other capability it uses, those an `extern fn` declares included, still appears in its `with(...)` set and binds its callers, and a `trusted extern fn` is a foreign binding callable without `Unsafe`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: every trusted operation states its premises (TRUST-2, R-0506, R-0509)
- Evidence: tested

**Rationale.** `trusted` is an audited containment boundary: the code inside uses techniques safe
code cannot, and that use is concentrated where a reviewer looks for it. It vouches for memory
safety, so it absorbs `Unsafe` (R-0484); it never hides an effect a caller should see, and it
does not relax linearity. A `trusted extern fn` asserts that the foreign function is safe for every
argument its types permit (`sqrt`, `abs`); one whose safety depends on pointer validity, or that can
invalidate a resource another handle owns, stays a plain `extern fn`.

`Unsafe` used to conflate three facts, which are now separate: operational authority, declared
as `with(File, Alloc, ...)`, which the caller supplies; a caller safety obligation, declared as
`with(Unsafe)`, an invariant the language cannot establish; and trust provenance, derived, never
declared: the implementation depends on raw operations, FFI or an audited assertion. An API may be
safe to call while raw inside (`Vec::push`, `String::clone`), so provenance never propagates as a
caller requirement; the four combinations are a safe interface over a checked or a trusted body, and
an obligation-bearing interface over a checked or a trusted body. Per-method `trusted` makes a safe
interface over an audited method expressible, which a whole-impl modifier could not; the owner
decided on 2026-10-29 (OD22) that trust is per method only, so `trusted impl` stays refused (a parse
error, E0001; bug 189's repair, `error_189_trusted_impl_refused.con`), and the two axes stay independent: `pub trusted fn read_unchecked(..) with(Unsafe)` is an audited body whose
caller still owes an obligation. A trusted body discharges an `Unsafe` obligation at a call, scoped
three ways so that it is a discharge and not an erasure: only `Unsafe` (operational capabilities
still bind), `extern` callees included since R-0484 (every `extern` now declares its effects, E0116),
and only the call (the raw-operation gate, E0521, is untouched). The 2026-09-25 migration took std's
public `Unsafe` declarations from 154 to 21 with no operational capability set changed and no
corpus regression; it found the inverse defect too, 15 public std APIs that traffic raw pointers
and declare no obligation (`String::from_raw_unchecked` the worst), recorded with the measurements
in [research/language/two-axis-safety-migration.md](../../research/language/two-axis-safety-migration.md).

```con accept name=trusted-absorbs-unsafe exit=42
trusted fn rd(p: *const i64) -> i64 {
    return *p;
}

trusted fn example_value() -> i64 {
    let x: i64 = 42;
    return rd(&x as *const i64);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-6"></a>
#### TRUST-6. Trust is inventoried per declaration

**TRUST-6@3.** The `trusted mod` marker is refused; trust is declared per function and externs, intrinsics and minting wrappers are individually inventoried in the trusted base.

- Kind: refusal
- Uses: none
- Codes: E0001 (parse)
- Status: pending R-0555
- Semantics: none
- Obligation: the trusted base inventories primitive and minting identities (R-0509)
- Evidence: tested

**Rationale.** OD61 supersedes OD7’s historical marker. Libraries may declare inventoried externs, only the compiler introduces intrinsics, and only the target runtime mints initial grants.





```con reject name=marker-removed code=E0001 stage=parse pair=per-function-trust pending=R-0555 now=reject:E0001:parse
trusted mod m { fn main() -> u8 { return 42; } }
```

```con accept name=per-function-trust exit=42
trusted fn id(n: u8) -> u8 { return n; }
fn main() -> u8 { return id(42); }
```

<a id="TRUST-7"></a>
#### TRUST-7. Moving a value out of raw memory is explicit

**TRUST-7@2.** In trusted or `Unsafe` code, `*p` through a raw pointer `p` reads by value only a Copy pointee, and a non-Copy value is moved out of raw memory only by `read_owned(p)`.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0311 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: the slot's ownership transfer is a named premise of `read_owned` (TRUST-2, R-0506)
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS T-RAWDUP; bug 198). `trusted` does not
relax linearity. A by-value `*p` of a non-Copy pointee copied a value the slot still owned, so a
trusted helper could free one heap cell twice. `read_owned(p)` states the transfer; afterwards the
slot is logically uninitialized until a value is stored back, a premise the trusted code's author
discharges (TRUST-2), which no checker tracks yet. Place uses
(`(*p).f`, `&*p`), Copy pointees and stores through `*p` are unchanged.

Decided by the owner on 2026-10-29 (OD13): a value moves out of raw memory only through `read_owned(p)` or an explicit replace or take through `&mut T`, and a raw address carries a trusted ownership obligation; bug 198's repair is final.

```con reject name=raw-deref-moves-linear-decided code=E0311 stage=check pair=read-owned-moves pending=R-0555 now=reject:E0311:check
struct R { v: u8 }
trusted fn dup<K>(x: &mut K) -> K { let p: *mut K = x as *mut K; return *p; }
fn main() -> u8 { let mut r: R = R { v: 40 }; let r2: R = dup::<R>(&mut r); let R { v: a } = r2; let R { v: b } = r; return a + b; }
```

```con accept name=read-owned-moves exit=42
struct R { v: u8 }
trusted fn swap(slot: &mut R, fresh: R) -> R {
 let p: *mut R = slot as *mut R; let old: R = read_owned(p); *p = fresh; return old;
}
fn main() -> u8 { let mut r: R = R { v: 40 }; let old: R = swap(&mut r, R { v: 2 }); let R { v: a } = old; let R { v: b } = r; return a + b; }
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_rawdup_trusted_deref.con` | `reject:E0311:TRUST-7@2` | `trust7` | `-` |
| `tests/programs/regress_198_read_owned.con` | `run:42` | `trust7` | `binary,interp` |

<a id="TRUST-8"></a>
#### TRUST-8. A program has no mutable global state

**TRUST-8@2.** No module-level declaration holds mutable state: there is no `static mut` or global cell, and a constant is immutable, has no interior mutability and never contains a resource or handle, while effects are checked independently through `with(...)`.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0555
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD8’s constants-only rule survives; authority rules replace its token-only effect model with resource values plus independent permissions.

```con reject name=constant-holds-token code=new stage=check pair=constant pending=R-0557 now=accept:42
struct Token { v: i64 }

const T: Token = Token { v: 42 };

fn example_value() -> i64 {
    let t: Token = T;
    let Token { v } = t;
    return v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=constant exit=42
struct Copy P { v: i64 }

const C: P = P { v: 42 };

fn example_value() -> i64 {
    return C.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-9"></a>
#### TRUST-9. Resources originate in target grants

**TRUST-9@2.** Only the target runtime mints initial grants, `main` declares and consumes the offered grants it needs without a `Root` type or split, and subsequent resources arise only by checked acquisition or unforgeable constructors from existing grants, with every extern, compiler intrinsic and minting wrapper inventoried by identity.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Semantics: none
- Obligation: target grants and acquisition contracts (R-0557, R-0509)
- Evidence: tested

**Rationale.** Authority rules 11–12 replace the root-token model: a grant not declared by main is never minted, no user code runs at startup, and target startup/shutdown effects are explicit. Target grant schemas are [undecided: see the target Core proposal](#core-grants).

<a id="TRUST-10"></a>
#### TRUST-10. Explicit resources and exact callback permissions

**TRUST-10@2.** Handlers and callbacks receive resource values only by explicit installation, transfer or a lock granting a borrow, and function values retain exact capability sets without conveying permissions, while permissions for future foreign function values remain undecided: see the target Core proposal.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Authority rules separate explicit resource transfer from compile-time permission sets; the canonical exact callback-set rule is authority rule 4. Future foreign callback permissions are [undecided: see the target Core proposal](#core-grants).

<a id="TRUST-11"></a>
#### TRUST-11. A raw store neither reads nor destroys the old contents

**TRUST-11@2.** In trusted or `Unsafe` code, a store `*p = v` through a raw pointer `p: *mut T` moves `v` into the slot without reading or destroying what the slot held, whether or not `T` is Copy.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkStmt, Concrete.lowerStmt, Concrete.Interp.evalStmtCore
- Semantics: none
- Obligation: whether the slot held a live value is a named premise of the store (TRUST-2, R-0506)
- Evidence: tested

**Rationale.** This is how trusted collection code writes an uninitialized slot, or refills one that
`read_owned` emptied (TRUST-7). The same store through `&mut T` over a live non-Copy value is
refused (OWN-9), because that value would be lost; through a raw pointer the checker cannot know
whether the slot is live, so overwriting a live linear value leaks it, and avoiding that is the
trusted code author's premise.

```con accept name=refill-emptied-slot exit=42
struct R { v: u8 }
trusted fn swap(slot: &mut R, fresh: R) -> R {
 let p: *mut R = slot as *mut R; let old: R = read_owned(p); *p = fresh; return old;
}
fn main() -> u8 { let mut r: R = R { v: 40 }; let old: R = swap(&mut r, R { v: 2 }); let R { v: a } = old; let R { v: b } = r; return a + b; }
```

<a id="TRUST-12"></a>
#### TRUST-12. Trusted code keeps the move, cleanup and freeze rules

**TRUST-12@2.** In a `trusted` function, as in safe code, a moved linear value is not used again, a linear value is consumed on every exit, and an owner frozen by a borrow block is not accessed.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkFn
- Semantics: none
- Obligation: the trusted body is checked by the same judgment as safe code (R-0499)
- Evidence: tested

**Rationale.** A double move, a skipped cleanup or an access to a frozen owner in a `trusted fn` is
refused with the same code as in safe code: OWN-1's `trusted-use-after-move`, OWN-2's
`trusted-never-consumed` and REF-8's `trusted-frozen-owner`. These are the rules the design record
(MEMORY_SEMANTICS §11) listed; it also said that `trusted` relaxes no ownership rule, which the current compiler
contradicts twice: trusted code may destroy a value through a reference (E0290 under OWN-8
elsewhere) and release with `vec_free` a vector whose elements are not Copy (E0287 under OWN-5
elsewhere), the drop-glue cases of std's collections. OD113 refuses these ownership exceptions.
The checker's guarantees stop at
raw pointers: what lies behind `*mut T` or `*const T` is not tracked (OWN-D1), which is why a raw
store (TRUST-11) and a move out of raw memory (TRUST-7) carry premises. OD64 confines borrow-derived pointers to their borrow.

```con accept name=trusted-consumes-once-decided exit=42 pending=R-0555 now=reject:E0001:parse
struct R { v: i64 }

fn take(r: R) -> i64 {
    let R { v } = r;
    return v;
}

fn peek(r: &R) -> i64 {
    return r.v;
}

trusted fn use_once(r: R) -> i64 {
    let mut seen: i64 = 0;
    borrow r as rr {
        seen = peek(rr);
    }
    return take(r) + seen * 0;
}

fn example_value() -> i64 {
    return use_once(R { v: 42 });
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="TRUST-13"></a>
#### TRUST-13. Reference-to-pointer conversion is gated

**TRUST-13@2.** Reference-to-pointer conversions require `trusted` or `Unsafe`, and a pointer derived from a borrow stays inside that borrow, never escapes or is exposed as an integer, and is passed to externs only through call-only parameters.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD98 gates the conversion, and OD64 confines the derived pointer to its borrow, including copies, offsets and helper calls; shared provenance never yields writes. Call-only extern annotation syntax and complete unsafe contracts are [undecided: see the target Core proposal](#core-unsafe-premises).

```con accept name=reference-to-pointer-in-safe-code exit=42
trusted fn rd(p: *const i64) -> i64 {
    return *p;
}

trusted fn example_value() -> i64 {
    let x: i64 = 42;
    return rd(&x as *const i64);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

### Trusted premises

This section is explanatory and states no rule. It lists each place where the language relies on a
premise that the compiler does not check, who states it, and the entry that names it. Where an
entry states a premise, the table only collects it; where no entry states it yet, the row says so. TRUST-2 (pending) is the rule that every
undefined behaviour is such a named premise, and
[TRUSTED_COMPUTING_BASE.md](../verification/TRUSTED_COMPUTING_BASE.md) records the trusted base of
the compiler and toolchain.

| place | premise the compiler does not check | stated by | entries |
|---|---|---|---|
| a `trusted fn`, or a function with `Unsafe` in its `with(...)` set | the premises of every raw-pointer operation and foreign call in its body | the function's author, by writing `trusted` or `with(Unsafe)` | TRUST-5, TRUST-4, TRUST-2 (pending) |
| a raw-pointer read `*p`, a write `*p = v` or an offset (`ptr::offset`) | the validity premises of `*p`, `*p = v` and `ptr::offset`; TRUST-1 names `ptr::offset`, TRUST-2 (pending) is the rule that each undefined behaviour be a named premise, and no entry lists these premises yet (registry item R-0506) | the author of the trusted or `Unsafe` code | TRUST-4, TRUST-1, TRUST-2 (pending); element stride: FFI-12 |
| `&*p` and `&mut *p` | `p` is aligned, live and initialized for its type | the author of the trusted or `Unsafe` code | TRUST-3 (pending) |
| `read_owned(p)` | what the slot holds before and after the move-out; TRUST-7 makes the move-out explicit, and no entry states this premise yet | the author of the trusted or `Unsafe` code | TRUST-7 |
| a raw store `*p = v` | whether the slot held a live value, which the store does not destroy | the author of the trusted or `Unsafe` code | TRUST-11 |
| a raw pointer taken from a reference | what the pointer may be used for and for how long; REF-14 (pending) is the entry for an alias derived from a loan, and OD64 confines it to its borrow with call-only extern use | the code that uses the pointer | TRUST-13, REF-14 (pending), OPEN-64 |
| an `extern fn` | the declared parameter types, result type and `with(...)` set match the C symbol | the declaration's author | FFI-3, FFI-5; "Abort and failure" in [statements.md](#ch-statements) |
| a `trusted extern fn` | the foreign function is safe for every argument its types permit and has no effect beyond its `with(...)` set | the declaration's author | FFI-9, TRUST-5 |
| a resource acquisition or minting wrapper | its external-state and authority contract; listing it proves nothing | the wrapper's author; a gate lists the wrappers | TRUST-9 (pending), TRUST-10 (pending) |
| `assume e` in a contract | `e` holds, as a hypothesis whose evidence is `assumed` | the programmer | CON-6 |
| an entry whose `Evidence:` is `trusted: <ID>` | the assumption that ID names in TRUSTED_COMPUTING_BASE.md | the specification ([MAINTAINERS.md](MAINTAINERS.md) §2.1); no entry uses it today | MAINTAINERS.md §2.1 |
| the native profile | the stated premises of the trusted base under which the forward simulation is proved | the proof of PASS-3 | PASS-3 (pending) |
| the LLVM profile | the BIR-to-LLVM printer, LLVM and the linker are correct; the results are differential-tested, not proved | the compiler | PASS-4 (pending) |

TRUST-12 is the entry for the move, cleanup and freeze rules inside a `trusted fn`. The raw-memory
premises listed above (`read_owned`, raw stores) remain separate from those rules. OD113 grants no ownership exceptions to trusted code.

### Absent by decision

This section is explanatory: it records capability features the language does not have, why, and
what would reopen them. The grammar (SYN-1) is what refuses them.

#### No capability-set bounds, subtraction or capability values

Decided with the callable-values design (2026-06-12,
[CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md)
§6 and §10, after
[research/language/capability-polymorphism.md](../../research/language/capability-polymorphism.md)).
A capability parameter takes no bound (`where C ⊆ {File, Console}`), a capability set is never
written as a difference, and a capability is not a value that can be stored, passed or returned:
capabilities are permissions the checker compares as sets (TYPE-D1), not effects. So there are no
effect handlers and no row-polymorphic effect types either; a combinator's set is its callbacks'
sets, named by capability parameters (TYPE-17). None of these is needed by the higher-order std
surface. A bound would be reconsidered only if a workload needs a combinator that refuses some
capability its callback could carry; global resources are values (TRUST-9), distinct from permissions.

### Version history

This appendix records superseded versions and is not normative.

- TRUST-D1@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-D1@1 is superseded by version.
- TRUST-4@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-4@2 is superseded by version.
- TRUST-6@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-6@2 is superseded by version.
- TRUST-7@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-7@1 is superseded by version.
- TRUST-8@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-8@1 is superseded by version.
- TRUST-9@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-9@1 is superseded by version.
- TRUST-10@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-10@1 is superseded by version.
- TRUST-11@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-11@1 is superseded by version.
- TRUST-13@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; TRUST-13@1 is superseded by version.

- TRUST-12@2 (2026-11-03) supersedes TRUST-12@1; pending legacy conformance is explicit.


<a id="ch-ffi"></a>
## Foreign Functions

The C boundary is explicit: foreign functions are declared `extern fn`, raw pointers are their own
types, and crossing the boundary needs `Unsafe` outside `trusted` code (TRUST-4, TRUST-5). This
chapter covers foreign function values (FFI-1), the one layout authority (FFI-2, pending), extern
declarations (FFI-3), layout attributes (FFI-4), symbol names (FFI-5), compiler intrinsics (FFI-6) and
what layout is promised (FFI-7 to FFI-12). The design record is
[FFI.md](../platform/FFI.md), which keeps the wrapper patterns and the calling-convention design;
[LAYOUT_CONTRACT.md](../archive/design-records/LAYOUT_CONTRACT.md) keeps the layout reasoning.

### Rules

<a id="FFI-1"></a>
#### FFI-1. A foreign function is only called directly

**FFI-1@1.** No `extern fn` is used as a value, and an extern function is only ever called directly.

- Kind: refusal
- Uses: none
- Codes: E0588 (elab, core-check)
- Status: enforced
- Enforced by: Concrete.refuseForeignFnValue
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS D4). A Concrete function type and a C
function type use different calling conventions. A foreign function used as a Concrete
`fn(..)` value is called with the wrong one (bug 166). Scalars happen to be passed alike, but
the rule does not rest on that coincidence. The rewrite is a Concrete wrapper that calls the
extern directly and is passed instead. The long-term design puts the calling convention in the
function type (`extern "C" fn(A) -> R`, distinct from `fn(A) -> R`). It lifts this refusal when
the first workload passes a callback to C. Enforced 2026-10-23 where a global function becomes a
value, on the resolver's identity of the name, so an import alias, a constant initializer and a
shadowing local are all decided right; CoreCheck re-checks it.

Decided by the owner on 2026-10-29 (OD4): every foreign function value is refused now; typed `extern "C" fn` types may come later.

```con reject name=extern-as-value code=E0588 stage=elab pair=wrapper-as-value
trusted extern fn abs(x: i32) with() -> i32;
fn main() -> u8 { let f: fn(i32) -> i32 = abs; if f(0 - 5) == 5 { return 5; } return 1; }
```

```con accept name=wrapper-as-value exit=5
trusted extern fn abs(x: i32) with() -> i32;
fn abs_wrapper(x: i32) -> i32 { return abs(x); }
fn main() -> u8 { let f: fn(i32) -> i32 = abs_wrapper; if f(0 - 5) == 5 { return 5; } return 1; }
```

<a id="FFI-2"></a>
#### FFI-2. One Layout authority for AAPCS64

**FFI-2@1.** Every size, alignment, field offset and AAPCS64 argument and return classification comes from one `Layout` authority, which the checker, the lowering and the ABI adapter all consult.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0502
- Semantics: none
- Obligation: `Layout` is the single proved authority, and the representation certificates it emits are checked (R-0502)
- Evidence: tested

**Rationale.** When two components compute a layout separately, they can disagree, and the
disagreement shows up as wrong code at the boundary (structural classes 8 and 12). One authority,
used by every consumer and stated for the AAPCS64 calling convention of the native profile,
removes the second copy.

<a id="FFI-3"></a>
#### FFI-3. An extern declaration declares its effects and FFI-safe types

**FFI-3@1.** An `extern fn` has no body, declares its effects with `with(...)` (`with()` for none, never for a C symbol known to have effects), and takes and returns only FFI-safe types: integers, floats, `bool`, `char`, `Unit`, raw pointers to a type that contains no reference, and `#[repr(C)]` structs.

- Kind: refusal
- Uses: REF-D1
- Codes: E0116 (resolve), E0117 (resolve), E0574 (core-check), E0575 (core-check)
- Status: enforced
- Enforced by: Concrete.coreCheckProgram, Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: foreign calls are external events governed by their declared contracts (R-0496)
- Evidence: tested

**Rationale.** An undeclared binding is never read as having no effects (bug 113), and a reference
or a Concrete aggregate has no C meaning, so it cannot cross. An `#[intrinsic = "n"]` declaration
that the compiler implements is not a foreign binding and declares no effects. A raw pointer to a
reference is refused too: C could keep the reference it was handed, or hand back the address of a
reference slot, and nothing on the Concrete side could see it. E0574 and E0575 look through raw
pointers with the REF-D1 judgment (gap G15 of MEMORY_SEMANTICS §15.7, closed 2026-10-23); a raw
pointer to a value type stays FFI-safe.

```con reject name=undeclared-effects code=E0116 stage=resolve pair=declared-extern
extern fn abs(x: i32) -> i32;

fn main() -> u8 {
    return 0;
}
```

```con reject name=non-ffi-safe-parameter code=E0574 stage=core-check pair=declared-extern
struct S { v: i64 }

extern fn f(s: S) with() -> i32;

fn main() -> u8 {
    return 0;
}
```

```con accept name=declared-extern exit=42
trusted extern fn abs(x: i32) with() -> i32;

fn example_value() -> i64 {
    return abs(0 - 42) as i64;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=returns-pointer-to-ref code=E0575 stage=core-check pair=pointer-to-value
trusted extern fn f() with() -> *const &i64;

fn main() -> u8 {
    return 0;
}
```

```con reject name=param-pointer-to-ref code=E0574 stage=core-check pair=pointer-to-value
trusted extern fn keep(p: *const &i64) with();

fn main() -> u8 {
    return 0;
}
```

```con accept name=pointer-to-value exit=42
trusted extern fn f() with() -> *const i64;
trusted extern fn keep(p: *const i64) with();

fn main() -> u8 {
    return 42;
}
```

<a id="FFI-4"></a>
#### FFI-4. Layout attributes are well formed

**FFI-4@1.** A `#[repr(C)]` struct is not generic and has only FFI-safe fields, `#[repr(packed)]` and `#[repr(align(N))]` are not combined, and `N` is a power of two no larger than the struct's natural alignment.

- Kind: refusal
- Uses: none
- Codes: E0572 (core-check), E0573 (core-check), E0576 (core-check), E0577 (core-check), E0585 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** `repr(C)` promises C's layout, which a generic or a non-C field cannot have.
Over-alignment is refused because code generation does not support it yet (E0585). OD111 accepts misaligned packed fields and refuses references to them.

```con reject name=align-not-power-of-two code=E0577 stage=core-check pair=repr-c
#[repr(C, align(3))]
struct S { v: i64 }

fn main() -> u8 {
    return 0;
}
```

```con accept name=repr-c exit=42
#[repr(C)]
struct Copy S { a: i32, b: i32 }

fn example_value() -> i64 {
    let s: S = S { a: 40, b: 2 };
    return (s.a + s.b) as i64;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="FFI-5"></a>
#### FFI-5. Concrete symbols cannot collide with C symbols

**FFI-5@1.** Every Concrete-defined function is emitted under a mangled symbol in a namespace disjoint from C identifiers, and only an `extern fn` (which binds its `#[link_name]` or its declared name) and an `#[export_name]` function use a C name.

- Kind: refusal
- Uses: none
- Codes: E0118 (resolve), E0119 (resolve)
- Status: enforced
- Enforced by: Concrete.Pipeline.emit
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A Concrete function named `malloc` used to replace C's `malloc` (bug 129). Every
Concrete symbol contains `.` or `$`, which no C identifier does, so the namespaces are disjoint by
construction. An export must be a C identifier of a C-compatible shape that no `extern fn` imports
and that is not a runtime symbol the program uses; a missing foreign function is an error, never a
synthesized body.

```con reject name=bad-export-name code=E0119 stage=resolve pair=concrete-malloc
#[export_name = "main"]
fn entry() -> i32 {
    return 0;
}

fn main() -> u8 {
    return 0;
}
```

```con accept name=concrete-malloc exit=42
fn malloc(n: u8) -> u8 { return n; }
fn main() -> u8 { return malloc(42); }
```

<a id="FFI-6"></a>
#### FFI-6. A compiler intrinsic is not a foreign binding

**FFI-6@2.** Only the compiler introduces an intrinsic, identified by its primitive identity rather than a foreign symbol, and its effects are fixed by the exhaustive primitive table rather than chosen by its declaration, with compound effects checked by composition.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0555
- Enforced by: Concrete.Pipeline.resolve
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Authority rule 7 and the primitive-introduction audit require compiler-only intrinsic identity and an exhaustive effect table covering externs, generated operations, runtime entry, startup and shutdown. A source declaration cannot invent an intrinsic or understate its effects.



```con reject name=library-intrinsic-removed code=new stage=check pair=compiler-primitive pending=R-0555 now=accept:42
// spec-file: main.con
mod foo;
import foo.{sizeof};
fn main() -> u8 { if sizeof::<u8>() + sizeof::<u32>() == 5 { return 42; } return 0; }
// spec-file: foo.con
#[intrinsic = "sizeof"]
pub trusted fn sizeof<T>() -> u64;
```

```con accept name=compiler-primitive exit=42
fn main() -> u8 { if sizeof::<u8>() == 1 { return 42; } return 0; }
```

<a id="FFI-7"></a>
#### FFI-7. Layout is unspecified unless a `repr` fixes it

**FFI-7@1.** The field order, offsets, padding, size and alignment of a struct or enum are unspecified except as a struct's `#[repr]` attribute fixes them (FFI-8), and safe code observes them only through `sizeof::<T>()` and `alignof::<T>()`, which give `T`'s size and alignment in bytes as a `u64`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign, Concrete.checkExprCore
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** The promised layout is smaller than the implemented one, so the compiler may change
an unannotated layout and a program cannot come to depend on it. Safe code has no other window:
it cannot turn a pointer into an integer (TRUST-1) or read through a raw pointer without `Unsafe`
(TRUST-4's `raw-deref-needs-unsafe`). Trusted
code that walks a buffer may also rely on the stride of an array of `T` being `sizeof::<T>()`
(FFI-12). An enum takes no `repr`; OD110 refuses enums at the foreign boundary (E0574).

```con accept name=observed-through-sizeof exit=42



struct Copy S { a: u8, b: u32 }

pub fn main() -> u8 {
    if sizeof::<S>() >= 5 && alignof::<S>() >= 1 {
        return 42;
    }
    return 0;
}
```

<a id="FFI-8"></a>
#### FFI-8. What `repr(C)` and `repr(packed)` promise

**FFI-8@2.** `#[repr(C)]` lays a struct out as C does, each field in declaration order at the next offset aligned for its type and the size rounded up to the struct's alignment, and `#[repr(packed)]` places the fields in declaration order with no padding and gives the struct alignment 1, accepts misaligned fields but refuses references to them (also when combined with `repr(C)`).

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Layout.tySize, Concrete.Layout.tyAlign, Concrete.Layout.fieldOffset
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** `repr(C)` is what a struct needs to cross `extern fn` (FFI-3), and `repr(packed)`
describes wire formats. `#[repr(align(N))]` promises only a minimum alignment, and today only where
it changes nothing (FFI-4, E0585). Which combinations are well formed is FFI-4. OD111 accepts packed fields at misaligned offsets and refuses references to them; the current compiler still accepts such references.

```con accept name=c-and-packed exit=42



#[repr(C)]
struct Copy R { v: u8, l: u32 }
#[repr(C, packed)]
struct Copy H { l: u32, f: u16, v: u8 }
#[repr(packed)]
struct Copy P { l: u32, v: u8 }

pub fn main() -> u8 {
    if sizeof::<R>() == 8 && alignof::<R>() == 4 && sizeof::<H>() == 7 && alignof::<H>() == 1
        && sizeof::<P>() == 5 && alignof::<P>() == 1 {
        return 42;
    }
    return 0;
}
```



```con accept name=misaligned-packed-field-value exit=42
#[repr(packed)]
struct Copy P { tag: u8, value: u32 }
fn main() -> u8 {
    let p: P = P { tag: 0, value: 42 };
    if p.value == 42 { return 42; }
    return 0;
}
```

<a id="FFI-9"></a>
#### FFI-9. What `trusted extern fn` asserts

**FFI-9@1.** A `trusted extern fn` asserts, as an obligation of its author that the compiler does not check, that the foreign function is safe for every argument its parameter types permit and has no effect beyond those its `with(...)` declares.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: every trusted operation states its premises (TRUST-2, R-0506, R-0509)
- Evidence: tested

**Rationale.** The assertion is what makes the binding callable without `Unsafe` (TRUST-5). A
binding whose safety depends on pointer validity, or that can invalidate a resource another handle
owns, stays a plain `extern fn`. The effects half is FFI-3: every `extern fn` declares its effects.
Absorbed from [SAFETY.md](../verification/SAFETY.md), which records the assertion in the trusted
base ([TRUSTED_COMPUTING_BASE.md](../verification/TRUSTED_COMPUTING_BASE.md) §6).

```con accept name=trusted-extern-without-unsafe exit=42
trusted extern fn abs(x: i32) with() -> i32;

fn example_value() -> i64 {
    return abs(-42) as i64;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="FFI-10"></a>
#### FFI-10. The layout forms

**FFI-10@1.** The only layout attributes are `#[repr(C)]`, `#[repr(packed)]` and `#[repr(align(N))]`, alone or combined as FFI-4 allows and written only on a struct, so every other struct has the unspecified layout of FFI-7, and no other `repr` option, `transparent` included, and no `repr` on an enum is accepted.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseAttribute, Concrete.parseModuleBody
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The promised layout surface stays small: a `transparent` wrapper would promise both
the layout of its field and that calls pass it as the field, and the project has no workload that
needs either ([LAYOUT_CONTRACT.md](../archive/design-records/LAYOUT_CONTRACT.md) §4). The parser
refuses the other forms today with the syntax diagnostic E0001, tagged SYN-1 (`unknown repr option
'transparent'`, and `#[repr(...)] can only be applied to struct definitions` for an enum), so this
entry carries no code of its own. OD110 refuses enums at the foreign boundary.

```con accept name=four-forms exit=42
#[repr(C)]
struct Copy A { v: u8 }
#[repr(packed)]
struct Copy B { v: u8 }
#[repr(C, align(1))]
struct Copy C { v: u8 }
struct Copy D { v: u8 }

fn example_value() -> i64 {
    let a: A = A { v: 10 };
    let b: B = B { v: 10 };
    let c: C = C { v: 10 };
    let d: D = D { v: 12 };
    return (a.v + b.v + c.v + d.v) as i64;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="FFI-11"></a>
#### FFI-11. A `repr(C)` struct crosses an extern call by value

**FFI-11@2.** A `#[repr(C)]` struct passed to or returned from an `extern fn` crosses by value at every size according to the platform's C calling convention, classified by the `Layout` authority.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0502
- Semantics: none
- Obligation: the AAPCS64 classification comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** A C function that takes or returns a struct by value can be bound directly, with no
pointer wrapper. Internal Concrete calls pass aggregates by pointer, and that set is not promised.
OD122 promises returned structs at every size according to the platform C ABI via `Layout`. The current compiler (EmitSSA's extern-call path, not the `Layout` authority) classifies by size alone, as
AAPCS64 does for structs of integer fields: a struct of at most 8 bytes is passed as one 64-bit
integer, of 9 to 16 bytes as two, and a larger one by pointer, and a returned struct of at most 8
bytes comes back as one 64-bit integer while a larger one is returned as an LLVM aggregate. Structs
of floating-point fields are not classified as AAPCS64's homogeneous aggregates, the same rule is
applied on x86-64, and a returned struct smaller than 8 bytes is stored as 8 bytes into its slot. The example below runs only natively, since the
interpreter models no C function that takes or returns a struct.

```con accept name=struct-by-value exit=42 interp-exempt=interp-models-no-struct-c-function
#[repr(C)]
struct Copy D { quot: i32, rem: i32 }

trusted extern fn div(n: i32, d: i32) with() -> D;

#[repr(C)]
struct Copy Addr { s: u32 }

trusted extern fn htonl(x: u32) with() -> u32;
trusted extern fn inet_lnaof(a: Addr) with() -> u32;

fn example_value() -> i64 {
    let r: D = div(85, 2);
    let a: Addr = Addr { s: htonl(167772202) };
    if r.rem == 1 && inet_lnaof(a) == 42 {
        return r.quot as i64;
    }
    return 0;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="FFI-12"></a>
#### FFI-12. The stride of a buffer is `sizeof`

**FFI-12@2.** Consecutive elements of an array, a `HeapArray` or a raw buffer of `T` lie `sizeof::<T>()` bytes apart, which trusted code doing pointer arithmetic over the elements may rely on.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Layout.tySize
- Semantics: none
- Obligation: layout comes from the one `Layout` authority (FFI-2, R-0502)
- Evidence: tested

**Rationale.** The complete raw offset contracts are [undecided: see the target Core proposal](#core-unsafe-premises); the legacy pointer-arithmetic examples are retired until they can use the decided named operations.

 This is the one layout fact beyond FFI-7 and FFI-8 that trusted code needs to walk a
buffer, and the `Layout` authority states it: an array of `n` elements of `T` has size `n` times
`sizeof::<T>()`. The examples use the pointer arithmetic of the current compiler (`p + 1` and an
integer added to a `*const u8`), which TYPE-6@2 and TRUST-1 replace with `ptr::offset` (bug 322,
R-0502). The interpreter stores typed values, not bytes, so the byte-offset example runs only
natively.





### Version history

This appendix records superseded versions and is not normative.

- FFI-6@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; FFI-6@1 is superseded by version.

- FFI-12@2 (2026-11-03) supersedes FFI-12@1 for decided-language conformance.


<a id="ch-pipeline"></a>
## Pipeline Preservation

The compiler is a chain of stages (PIPELINE_CONTRACTS.md). Each stage's preservation entry
states the relation between the stage's input and its output, and the evidence for it. Under
VERIFIED_PIPELINE the target form of each statement is
`WF Γ p → pass p = .ok q → WF Γ q ∧ Refines Γ p q`. Until that is proved, the evidence is
`tested`: the examples are accepted programs, and every consumer must give each one the stated
result. A stage-boundary entry also has reject examples: a program that breaks an earlier
stage's contract must be refused by the first responsible stage (`stage=` pins its pass tag), and
no lower layer (`llvm-as`, the linker, a panic) may report it instead. The reject example's code
belongs to the chapter of the rule it enforces. This chapter replaces the boundary contracts of
[PIPELINE_CONTRACTS.md](../compiler/PIPELINE_CONTRACTS.md), which is now their design record, and
`check_spec.sh` runs their fixtures, which used to belong to `check_pipeline_contracts.sh`.

The stages are, in order: Parse, Resolve, Check, Elab (into Core), CoreCheck, Mono, Lower (to SSA,
then SSAVerify), Emit (LLVM IR) and link. PASS-5 to PASS-11 state each boundary. PASS-1 and
PASS-2 are about specialization, and PASS-3 and PASS-4 are about the end-to-end profiles.

### Rules

<a id="PASS-1"></a>
#### PASS-1. Specialization preserves meaning

**PASS-1@1.** Monomorphization refines its input: a call of a generic function at a type argument behaves as the generic body behaves at that type, on every consumer.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.monoProgram
- Semantics: none
- Obligation: the substitution and dispatch theorem of S8a (R-0510)
- Evidence: tested

**Rationale.** A generic body is checked once, at its opaque type parameters. Every property
the checker established there must survive substitution, and the code that runs is the
specialization. A rule keyed on a property that substitution changes, such as whether a type is
Copy, breaks this (bug 105's class, and bug 181, PASS-2). Today this is tested only. S8a
(R-0510) replaces Mono with a specialization search plus a total validator and proves the
substitution example.

```con accept name=generic-at-int exit=42
fn twice<T: Copy>(x: T, f: fn(T) -> i64) -> i64 {
    return f(x) + f(x);
}

fn half(n: i64) -> i64 {
    return n / 2;
}

fn example_value() -> i64 {
    return twice::<i64>(42, half);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```


<a id="PASS-2"></a>
#### PASS-2. Binder modes are fixed before specialization

**PASS-2@2.** Every pattern binder's mode (by reference or by value) is decided once by the checker and carried explicitly in Core as the binder's type, and it is the same in every specialization, except that a whole-value binder over an owned scrutinee instantiated at a reference type may change mode where both modes bind the same value.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.payloadBinderTy, Concrete.wholeBinderTy
- Semantics: none
- Obligation: CoreCheck re-checks the carried mode after Mono (R-0496, R-0510)
- Evidence: tested

**Rationale.** Bug 181: Check gave a payload binder through a reference the type `&T`, Elab
gave it `T`, and after specialization at `T := i64` the binary dereferenced the payload as an
address. The decision was made in one place and recomputed in another, and nothing compared
the two. REF-9 removes the Copy dependence from the rule. This entry requires the mode to be
data in Core rather than recomputed: since 2026-10-23 a binder's Core type is its mode (a
reference type is a borrowed binder), decided once from the scrutinee's type, and Lower and the
interpreter read it from there. The re-check after Mono is the open obligation. The carried type changes under substitution in one case: a
whole-value binder `n` over a scrutinee of type `T` at `T := &i64` gets a reference type, and
Lower and the interpreter bind the scrutinee itself in both modes.

```con accept name=binder-mode-survives-mono exit=42
fn get<T>(o: &Option<T>, k: fn(&T) -> i64) -> i64 {
    match o {
        Option::Some { value } => { return k(value); },
        Option::None => { return 0; }
    }
}

fn read(p: &i64) -> i64 {
    return *p;
}

fn example_value() -> i64 {
    let o: Option<i64> = Option::Some { value: 42 };
    return get::<i64>(&o, read);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PASS-3"></a>
#### PASS-3. The native profile is a forward simulation from checked Core to AArch64

**PASS-3@1.** For the native profile, every behaviour of the compiled AArch64 image is a behaviour of the checked Core program it was compiled from, proved as a forward simulation through each stage under the stated premises of the trusted base.

- Kind: preservation
- Uses: none
- Codes: none
- Status: pending R-0508
- Semantics: none
- Obligation: the composed compiler-correctness theorem (R-0508), with the native image checked by R-0505
- Evidence: tested

**Rationale.** This is the theorem the verified pipeline exists for (VERIFIED_PIPELINE, Part 2).
Forward simulation is turned into backward refinement only after its receptiveness and
determinacy hypotheses are proved. Each stage's own preservation entry in this chapter is one
link of the chain.

<a id="PASS-4"></a>
#### PASS-4. The LLVM profile is trusted and differential-tested

**PASS-4@1.** The LLVM profile is outside PASS-3: its binaries are labelled unverified, its BIR-to-LLVM printer is trusted, and its results are differential-tested against the interpreter and the native profile.

- Kind: preservation
- Uses: none
- Codes: none
- Status: pending R-0503
- Semantics: none
- Obligation: none (trusted; TRUSTED_COMPUTING_BASE section 4)
- Evidence: tested

**Rationale.** LLVM is the fast profile, not the verified one. Unverified does not mean
unspecified: the printer adds no poison- or UB-generating flags without their preconditions, and
an unsupported mapping fails explicitly (VERIFIED_PIPELINE, "IR chain").

<a id="PASS-5"></a>
#### PASS-5. After Check: linearity resolved, operand types agree

**PASS-5@2.** A program that Check accepts has every owned linear local, parameter and branch or arm local consumed on every normal exit and every arithmetic, bitwise or comparison operation on numeric operands at one numeric type, so no later stage receives either violation.

- Kind: preservation
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Pipeline.check
- Semantics: none
- Obligation: Check's judgment is the Core checker's (R-0499); width agreement comes from the one `IntArith` authority (R-0493)
- Evidence: tested

**Rationale.** Front end and back end share one width and signedness truth, so "Check says ok,
SSA-verify says no" cannot happen. A leak caught by Check is never left for a later pass to
notice.

```con reject name=unconsumed-linear-local-decided code=E0208 stage=check pair=linear-local-consumed pending=R-0555 now=reject:E0108:resolve
struct R { x: i32 }
fn f() -> Unit { let r: R = R { x: 1 }; }
fn main() -> u8 { return 0; }
```

```con reject name=mixed-width-binop-decided code=E0228 stage=check pair=linear-local-consumed
fn example_value() -> i64 {
    let a: i32 = 1;
    let b: i64 = 2;
    return (a + b) as i64;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=linear-local-consumed exit=42
struct R { x: i32 }
fn f() -> i32 {
    let r: R = R { x: 41 };
    let R { x } = r;
    return x;
}
fn example_value() -> i64 {
    let a: i64 = 1;
    let b: i64 = f() as i64;
    return (a + b) as i64;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="PASS-6"></a>
#### PASS-6. After Elab: desugaring adds nothing

**PASS-6@1.** Elaboration into Core introduces no ill-typed term, no mixed-width operation and no capability call hidden from the capability check, and CoreCheck refuses any Core in which it would have, so desugaring never smuggles an effect past the check.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.elaborate, Concrete.Pipeline.coreCheck
- Semantics: none
- Obligation: elaboration is proved against the declarative elaboration relation (VERIFIED_PIPELINE, "Passes"; R-0496)
- Evidence: tested

**Rationale.** Elaboration rewrites loops, patterns, method calls and borrow sugar into Core.
CoreCheck re-checks the result, so a desugaring that introduced a capability call or a
mixed-width operation is refused there, not compiled.

```con accept name=sugar-elaborates exit=42
struct Copy P { a: i64, b: i64 }
fn main() -> u8 {
 let p: P = P { a: 40, b: 0 };
 let q: P = P { a: p.a, b: 2 };
 let mut i: i64 = 0; let mut out: i64 = q.a;
 while i < q.b { out = out + 1; i = i + 1; }
 if out == 42 { return 42; } return 1;
}
```

<a id="PASS-7"></a>
#### PASS-7. After Mono: no type variable survives

**PASS-7@2.** After monomorphization no type variable and no unresolved generic declaration remains in the Core that later stages compile, and every Copy type's fields are Copy in each specialization, with growing polymorphic call cycles refused before Mono.

- Kind: preservation
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Pipeline.monomorphize, Concrete.verifyPostMono
- Semantics: none
- Obligation: specialization validated by S8a's total validator (R-0510)
- Evidence: tested

**Rationale.** Lowering assumes every type is concrete. `verifyPostMono` is a hard wall: a
specialization that left a type variable, or a Copy specialization with a non-Copy field, stops
the pipeline instead of reaching the backend.

```con accept name=generic-specialized exit=42
struct Copy Pair<T> { a: T, b: T }

fn sum<T: Copy>(p: Pair<T>, f: fn(T) -> i64) -> i64 {
    return f(p.a) + f(p.b);
}

fn id(x: i64) -> i64 {
    return x;
}

fn example_value() -> i64 {
    let p: Pair<i64> = Pair::<i64> { a: 40, b: 2 };
    return sum::<i64>(p, id);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PASS-8"></a>
#### PASS-8. After CoreCheck: no invalid Core proceeds

**PASS-8@1.** No Core program proceeds past CoreCheck unless its binary operations are width-compatible, every capability it uses is declared, every Copy type has Copy fields, no type has infinite size, and every unsafe or trusted operation carries its fact.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.coreCheckProgram
- Semantics: none
- Obligation: CoreCheck becomes the authoritative checker on Core (R-0499)
- Evidence: tested

**Rationale.** CoreCheck is the wall between the front end and code generation. A recursive type
used to reach `llvm-as` (bug 024). Each reject example pins the `core-check` boundary, so a
regression that moves a check later fails here even when the program is still refused.

```con reject name=undeclared-capability code=E0520 stage=core-check pair=valid-core
fn needs() with(Network) -> i64 { return 0; }
fn example_value() -> i64 { return needs(); }
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con reject name=copy-struct-non-copy-field code=E0571 stage=core-check pair=valid-core
struct S { x: i32 }
struct Copy T { s: S }
fn main() -> u8 { return 0; }
```

```con reject name=infinite-size-type code=E0583 stage=core-check pair=valid-core
struct S { x: S }
fn main() -> u8 { return 0; }
```

```con accept name=valid-core exit=42
fn needs() with(Network) -> i64 { return 42; }
struct Copy S { x: i32 }
struct Copy T { s: S }
fn example_value() with(Network) -> i64 {
    let t: T = T { s: S { x: 0 } };
    return needs() + (t.s.x as i64);
}
fn main() with(Network) -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="PASS-9"></a>
#### PASS-9. After Lower: verified SSA with every runtime check

**PASS-9@1.** Lowering produces SSA that passes the SSA verifier, with a runtime trap for every check the semantics requires (array bounds, integer overflow, division by zero and shift range) and with every defer and scope cleanup represented as edges of the control-flow graph.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.lower, Concrete.ssaVerifyProgram
- Semantics: none
- Obligation: Core to Place MIR to BIR as proved simulations or validated passes (R-0501, R-0503, R-0504)
- Evidence: tested

**Rationale.** Bounds, overflow and division are semantics, not static checks: they are trapping
operations in Core, and a pass may remove a trap only with a discharged-VC certificate naming
that operation (VERIFIED_PIPELINE, "Where each check lives"). The verifier runs before and after
SSA cleanup.

```con accept name=checked-operations-pass exit=42
fn example_value() -> i64 {
    let a: [i64; 4] = [10, 20, 5, 7];
    let mut total: i64 = 0;
    let mut i: i64 = 0;
    while i < 4 {
        total = total + a[i];
        i = i + 1;
    }
    return total / 1;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="PASS-10"></a>
#### PASS-10. After Emit: checked operations stay checked, executables have an entry point

**PASS-10@1.** Every checked operation is emitted through a named helper or a named proven elision and never as a raw wrapping or undefined LLVM operation, and an executable build without an entry point is refused with a `link` diagnostic before the system linker runs.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit, compileSSA
- Semantics: none
- Obligation: none (the LLVM profile is trusted, PASS-4)
- Evidence: tested

**Rationale.** Overflow checks lower to `@__cc_*` helpers built on LLVM's overflow intrinsics,
not to plain `add` with undefined wrap. A missing `main` used to leak from the system linker as
"Undefined symbols: _main" (bug 025). The `link` diagnostic has no code yet, and the
interpreter reports a missing `main` its own way.

```con reject name=no-entry-point code=none stage=link pair=entry-point interp-exempt=no-link-stage-in-the-interpreter
// spec-file: main.con
mod m;


// spec-file: m.con
fn helper() -> i64 { return 1; }

```

```con accept name=entry-point exit=42
fn helper() -> i32 { return 41; }
fn example_value() -> i64 { return (helper() + 1) as i64; }
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="PASS-11"></a>
#### PASS-11. BackendIR keeps backend intent structured

**PASS-11@1.** Runtime checks, trap and source spans, layout and ABI facts, helper calls, target constants and capability and trust labels stay structured through BackendIR, and `BackendIRVerify` refuses malformed backend intent before LLVM text is emitted.

- Kind: preservation
- Uses: none
- Codes: none
- Status: pending R-0503
- Semantics: none
- Obligation: BIR is the only input to LLVM and to the native adapter (R-0503)
- Evidence: tested

**Rationale.** Reports, audits and code generation must not reconstruct facts from LLVM text.
The boundary is useful even while LLVM is the only emitter.

<a id="PASS-12"></a>
#### PASS-12. An accepted program is compiled

**PASS-12@2.** A program that every rule of this specification accepts is compiled: no stage after Check refuses it, crashes on it or emits LLVM IR that fails validation.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Pipeline.lower, Concrete.Pipeline.emit
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The front end is where a program is judged; the later stages only translate it. A
generic function that calls itself at a growing type argument does not finish monomorphizing in the current compiler;
OD132 refuses growing call cycles before Mono. A refusal,
a crash or invalid IR after Check means the program met no rule and still did not compile, so the
user gets no diagnostic that names a rule. Bugs 005, 009, 014, 016, 029, 032, 035, 044, 055, 077,
108 and 116 were such failures. A refusal that a later stage needs (such as E0809, when a generated
specialization's name is already declared) is a gap in a rule, not a licence: the current compiler still refuses
the program below at monomorphization (bug 054, whose mangling is forgeable).

```con accept name=generic-beside-user-types exit=42
struct Copy Box<T> { v: T }
struct Copy Pair { a: i64, b: i64 }

fn example_value() -> i64 {
    let a: Box<i64> = Box::<i64> { v: 40 };
    let p: Pair = Pair { a: 1, b: 1 };
    return a.v + p.a + p.b;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=user-type-spells-a-specialization exit=42 pending=bug-054 now=reject:E0809:mono
struct Copy Box<T> { v: T }
struct Copy Box_Int { v: i64 }

fn example_value() -> i64 {
    let a: Box<i64> = Box::<i64> { v: 40 };
    let b: Box_Int = Box_Int { v: 2 };
    return a.v + b.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PASS-13"></a>
#### PASS-13. Every consumer gives the checked Core behaviour

**PASS-13@1.** The interpreter and every compiled profile give an accepted program the behaviour of its checked Core program: the same result, the same output and the same traps.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.lower, Concrete.Pipeline.emit, Concrete.Interp.interpret
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** PASS-3 states this for the native profile as a proof obligation (pending, R-0508) and
PASS-4 tests the LLVM profile against the interpreter; this entry states the agreement every consumer owes, which the differential gates
(native against `--interp`) check on every example of this specification. Bugs 004, 006, 019,
038, 102, 184 and 254 were disagreements between consumers.

```con accept name=consumers-agree exit=42
fn example_value() -> i64 {
    let mut s: i64 = 0;
    let mut i: i64 = 0;
    while i < 7 {
        s = s + i;
        i = i + 1;
    }
    return s + 21;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

### Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- PASS-2@1 was retired before it was enforced (the registry: the whole-value binder exception is stated in version 2); PASS-2@2, the first enforced version, names the one case where the carried type changes under substitution (a whole-value binder over a scrutinee of type `T` at `T := &Int`).

- PASS-5@2 (2026-11-03) supersedes PASS-5@1; pending legacy conformance is explicit.

- PASS-12@2 (2026-11-03) supersedes PASS-12@1; pending legacy conformance is explicit.


<a id="ch-decisions"></a>
## Owner Decisions

This chapter records the language questions that only the project owner decides, OD1 to OD157 of
[the decision record](#owner-decisions), and how each was answered. Every one is answered:
OD1 to OD32 and OD47 on 2026-10-29, the entries OPEN-33 to OPEN-157 on 2026-10-31 and 2026-11-01
(OD94 was withdrawn: MOD-1 already refuses its case, bug 337), and the questions raised while
recording them on 2026-11-02. The entry IDs are retired and never
reused; their anchors stay, so links to them land on the answer.

The answers of 2026-10-31, 2026-11-01 and 2026-11-02 are recorded here before the rules that carry them are written.
The chapters state these decided rules now (owner revision, 2026-11-03), before being rewritten as Lean, the only normative source of the language
([ROADMAP.md](../../ROADMAP.md#phases), phases B–D); until then, where a chapter still describes
the earlier behaviour, the answer below governs, and the code follows as its new-compiler slice lands.

The shape of Core was approved provisionally on 2026-11-05 ([stage A2a](#decided-2026-11-05)): it binds the design, and the owner freezes Core at stage A2c, after the four design theorems are proved against the proposed `Spec.lean`. The project and architecture decisions are recorded here too, [below](#decided-project).

<a id="rules-a-decision-changed"></a>
### Rules a decision has changed

The chapters were brought into line on 2026-11-03 under the owner's explicit prose-cleanup
revision of the Lean-only source decision. The table records the audit and its disposition; an
undecided dependency remains explicitly linked to the target Core proposal rather than designed here.

| Rules | What the audit found | Governing decision | Disposition |
|---|---|---|---|
| TRUST-9 | `main` receives a root resource, splits it, and resources descend from it. | authority rule 12; revised OD8/38 | Fixed in prose 2026-11-03. |
| TRUST-8/9/10 | Tokens model resources, effects appear as token parameters, and there is no separate effect system. | Authority introduction and rules 1–2, 8, 11 | Fixed in prose 2026-11-03. |
| TRUST-D1/4 | Capabilities include `File`, `Console`, and `Random`. | authority rule 13 | Fixed in prose 2026-11-03. |
| TRUST-4, TYPE-D1 rationale | `Std` expands to all operational capabilities. | authority rule 6 and introduction | Fixed in prose 2026-11-03. |
| Syntax introduction | Capability names are contextual; `ghost` is contextual. | authority rule 13; OD73/81/90; OD50 | Fixed in prose 2026-11-03. |
| MEM-5, OWN-15 tables | `alloc(v)` directly returns an owned heap value. | Allocation decision | Fixed in prose 2026-11-03. Allocator identities, dependency discipline and complete failure contracts remain [undecided: see the target Core proposal](#core-allocator-identities). |
| MEM-5 and resource descriptions | Heap/container types have no allocator identity: `Heap<T>`, `Vec<T>`, `String`. | Allocation decision | Fixed in prose 2026-11-03. Allocator identities, dependency discipline and complete failure contracts remain [undecided: see the target Core proposal](#core-allocator-identities). |
| STMT-2 | OOM is terminal; recovery is a workload-gated backlog item. | Allocation decision | Fixed in prose 2026-11-03. |
| MEM-5/7, TYPE-10, OWN-15 | `free(h)` releases storage and returns its payload; a Copy result can disappear as part of consumption. | OD59; clarified OD140 | Fixed in prose 2026-11-03. Allocator identities, dependency discipline and complete failure contracts remain [undecided: see the target Core proposal](#core-allocator-identities). |
| STMT-7 | `main` takes no parameters until an implementation task lands. | authority rule 12; revised OD38 | Fixed in prose 2026-11-03. |
| STMT-7 | `main` returns `u8` or `Unit`, with `Unit` mapped to zero. | 2026-11-02 main decision | Fixed in prose 2026-11-03. |
| STMT-10 | Tests return `i32`. | Revised OD146 | Fixed in prose 2026-11-03. |
| SCOPE-2 | Types, traits, and modules are categorically “another namespace” and therefore not collisions. | Completed OD43; OD67 | Fixed in prose 2026-11-03. |
| TRAIT-D1 rationale | Impl target lookup includes a sibling submodule’s declarations as an implicit fallback. | Explicit-child decision, completed OD43 | Fixed in prose 2026-11-03. |
| Modules introduction and MOD-7 rationale; accepted examples throughout modules/types/FFI | Inline `mod m { … }` is an available module form. | OD33/61 | Fixed in prose 2026-11-03. |
| MOD-4 | Loads `X.con` or `X/mod.con`. | OD142 | Fixed in prose 2026-11-03. |
| TRUST-6 | Trusted functions, externs, and raw operations require `trusted mod`. | OD61 | Fixed in prose 2026-11-03. |
| PAT-1/6, SYN-4, scope introduction | `if let` and `while let` are supported forms with specified semantics. | OD52 | Fixed in prose 2026-11-03. |
| PAT-5/10/11, OWN-5/15 | Refutable destructuring uses `let … else`; rules explain its fallback and mutability. | OD52/85 | Fixed in prose 2026-11-03. |
| PAT-2/3/4/7/9, TYPE-5, OWN-15 | Guards affect matching, coverage, commitment, and moves. | OD55 | Fixed in prose 2026-11-03. |
| PAT-3/7, OWN-15, REF-9 rationale | Range patterns match integer intervals. | OD56/41 | Fixed in prose 2026-11-03. |
| TYPE-7/11, STMT-4/5, OWN-15;  | Struct update `..base` supplies fields, has evaluation order, and copies fields. | OD53 | Fixed in prose 2026-11-03. |
| TYPE-11/21/24, OWN-D1/15, REF-3/5, MOD-9;  | Newtypes exist, have representation rules, constructors, `.0`, casts, and imports. | OD54/123/155 | Fixed in prose 2026-11-03. |
| REF-D3, SCOPE-D1;  | Borrow blocks bind a region via `in R`. | OD57 | Fixed in prose 2026-11-03. |
| OWN-8 rationale; REF-5/12 rationale | Auto-borrowed receivers are ordinary supported places; elaboration builds implicit receiver borrows. | OD32 | Fixed in prose 2026-11-03. |
| MEM-4 accepted example; OWN-15 heap-field row | `h.f` works directly through `&Heap<S>`/`&mut Heap<S>`. | OD30/36 | Fixed in prose 2026-11-03. Exact heap-array view remains [undecided: see the target Core proposal](#core-heap-array-view). |
| TRUST-13, TRUST-4 exception; accepted example  | Reference-to-pointer conversion is safe and needs neither trust nor `Unsafe`. | OD98 | Fixed in prose 2026-11-03. |
| MEM-9, ARITH-13 | Proven runtime violations are refused only outside trusted/`Unsafe` functions. | OD118 | Fixed in prose 2026-11-03. |
| CON-3 | Contracts read parameters and `result`. | OD143 | Fixed in prose 2026-11-03. |
| STMT-11 and execution prose; OWN-13 rationale | Remaining locals “are destroyed”; storage teardown versus `Destroy` remains open. | OD69 | Fixed in prose 2026-11-03. |
| OWN-17 | Deferred Copy results are implicitly dropped. | OD140; authority rule 9 | Fixed in prose 2026-11-03. |
| OWN-5; catalog | `discard` accepts any Copy value. | OD62 | Fixed in prose 2026-11-03. |
| REF-10 | An exclusive-reference binding may go unused. | OD62 | Fixed in prose 2026-11-03. |
| SYN-1 and introduction | The external EBNF governs acceptance exactly. | OD150/77 | Fixed in prose 2026-11-03. |
| SYN-6 | A char literal is any one character; non-ASCII behavior is left open. | OD137 | Fixed in prose 2026-11-03. |
| TYPE-6 | Comparisons take numbers only; bool/char support is unresolved. | OD42 | Fixed in prose 2026-11-03. |
| TRAIT-6 and introduction; rationale  | Ambiguous bound-method calls are refused and qualification does not exist. | OD6, completed 2026-11-02 by the qualified call `Trait::method(&x, ...)` | Fixed in prose 2026-11-03. |
| PAT-8 and accepted examples; PASS-6; CON-4 | Rules/examples still accept or regulate C-style `for` headers. | OD47/61 | Fixed in prose 2026-11-03. |
| Conformance examples across all 15 rule chapters; e.g. | Unqualified accepted fixtures use removed aliases/forms, wrong `main` results, or unused named bindings. | Alias/main decisions; OD33/52–57/62 and others above | Fixed in prose 2026-11-03. |
| README and overview authority description | Markdown sentences are normative except where semantics later lands; the written directory is the language source. | the Lean-only source decision (ROADMAP) | Fixed in prose 2026-11-03. |


<a id="target-core-proposal"></a>
### Target Core proposal: dependencies (provisionally decided 2026-11-05)

These items were undesigned when the chapters were cleaned up on 2026-11-03. **The owner approved their design provisionally on 2026-11-05** ([the decided rows](#decided-2026-11-05-core): allocator identities Q1, allocation contracts Q1–Q2, the heap-array view Q3, static text Q4, capability inference Q5, unsafe contracts Q3.7–Q3.12, static refusal Q6, grants Q7, semantic objects Q8–Q9). Where a chapter still says **undecided: see the target Core proposal**, read the decided row; the rule is rewritten in Lean at stage A2b and frozen at A2c. The proposal is owned by
[ROADMAP: Questions for the Core session](../../ROADMAP.md#questions-for-the-core-session).

- <a id="core-allocator-identities"></a>[Core-session question 1](../../ROADMAP.md#questions-for-the-core-session). **Allocator identities and dependencies:** introduction, kind,
  scope, equality, packaging and repeated reset, dependency tracking across calls, containers and
  callbacks, and lifetime-safe release metadata; the allocation decision's conservation and no-live-
  allocation requirements stand, but the discipline expressing them, undecided on 2026-11-03, is provisionally decided by rows Q1.1–Q1.13 ([rows](#decided-2026-11-05-core)).
- <a id="core-allocation-contracts"></a>[Core-session question 1–2](../../ROADMAP.md#questions-for-the-core-session). **Allocation contracts:** failure payload shapes and complete
  growth, reserve and multi-allocation transition contracts; applicability of container `Destroy`
  when payload cleanup needs effects beyond `Alloc`, undecided on 2026-11-03, are provisionally decided by rows Q1.8–Q1.12 and Q2.1–Q2.8 ([rows](#decided-2026-11-05-core)).
- <a id="core-heap-array-view"></a>[Core-session question 3](../../ROADMAP.md#questions-for-the-core-session). **Heap-array view:** exact bounded view type, operations, lifetime,
  mutability and permitted positions, undecided on 2026-11-03, are provisionally decided by rows Q3.1–Q3.6 and Q4.7 ([rows](#decided-2026-11-05-core)); opening never yields an element reference.
- <a id="core-static-text"></a>[Core-session question 4](../../ROADMAP.md#questions-for-the-core-session). **Static text:** literal type, ownership and conversion into
  allocator-branded strings, undecided on 2026-11-03, are provisionally decided by rows Q4.1–Q4.6 ([rows](#decided-2026-11-05-core)); static text requires no allocation.
- <a id="core-grants"></a>[Core-session question 7](../../ROADMAP.md#questions-for-the-core-session). **Target grants:** exhaustive schemas, multiplicity, lifecycle and narrowing
  APIs, including the resource backing process services, undecided on 2026-11-03, are provisionally decided by rows Q7.1–Q7.11 ([rows](#decided-2026-11-05-core); process spawning is deferred).
- <a id="core-capability-inference"></a>[Core-session question 5](../../ROADMAP.md#questions-for-the-core-session). **Capability inference:** decided 2026-11-05 (row Q5.1/Q5.4): there is
  none; capability arguments are written explicitly at every polymorphic call and checked, so no choice among
  solutions arises. Exact sets and refusal of inconsistent bindings stand.
- <a id="core-unsafe-premises"></a>[Core-session question 3, 9](../../ROADMAP.md#questions-for-the-core-session). **Unsafe contracts:** the complete validity, provenance, aliasing
  and foreign-mutation taxonomy and call-only annotation syntax, undecided on 2026-11-03, are provisionally decided by rows Q3.7–Q3.12 and Q9.7 ([rows](#decided-2026-11-05-core)); OD64's confinement
  requirement stands.
- <a id="core-static-refusal"></a>[Core-session question 6](../../ROADMAP.md#questions-for-the-core-session). **Static refusal:** the mandatory proof criterion and its relation
  to Core acceptance, undecided on 2026-11-03, are provisionally decided by rows Q6.1–Q6.7 ([rows](#decided-2026-11-05-core)); recorded proven-violation refusals apply in trusted code too.
- <a id="core-semantic-contract"></a>[Core-session question 8–9](../../ROADMAP.md#questions-for-the-core-session). **Semantic objects:** exact events, admissible allocation oracles,
  observations, resource relations and theorem endpoint, undecided on 2026-11-03, are provisionally decided by rows Q8.1–Q8.7 and Q9.1–Q9.19 ([rows](#decided-2026-11-05-core)); technical adequacy and
  refinement obligations remain binding.


#### Rules awaiting Lean formalization (listed 2026-11-03)

These rules depended on a design that was undecided on 2026-11-03. Their designs were provisionally
decided on 2026-11-05 ([decided rows](#decided-2026-11-05-core)); formalizing them in Lean remains
pending through A2b, and freezing them through A2c. This does not reopen the decisions already
recorded in their sentences.

| rules | reason |
|---|---|
| SYN-5, TYPE-3, OWN-15, REF-5 | [Static-text typing, ownership and borrowing](#core-static-text) |
| TYPE-D1, TYPE-17 | [Ambiguous set-variable inference](#core-capability-inference) |
| TYPE-10, OWN-13 | [Complete conserving failure contracts and container Destroy applicability](#core-allocation-contracts) |
| MEM-5, MEM-7 | [Allocator identity introduction, dependency discipline and release metadata](#core-allocator-identities); MEM-5 also needs [failure payload contracts](#core-allocation-contracts) |
| REF-D3, MEM-4 | [Exact bounded heap-array view](#core-heap-array-view) |
| STMT-7, TRUST-D1, TRUST-9, TRUST-10 | [Target grant schemas, lifecycle and future foreign callback permissions](#core-grants) |
| ARITH-13, MEM-9 | [Mandatory static-proof refusal criterion](#core-static-refusal) |
| TRUST-13, FFI-12 | [Complete unsafe primitive contracts and call-only annotation syntax](#core-unsafe-premises) |


<a id="decided-2026-11-05"></a>
### Owner decisions 2026-11-05: Core design (A2a provisional approval) and project rows

The owner answered every row of the 2026-11-05 decision page, in the terminal. The rows that revise a recorded rule were answered one by one: Q5.1/Q5.4, Q6.4, Q2.8, Q9.15 with P3, Q7.11 and Q4.7. Q3.12, Q9.19, Q7.10, Q1.6, P2, P4, P9, P13, P16 and P17a/b were also answered directly. The remaining Core rows were accepted as recommended, and the remaining project rows were approved on the owner's condition that Claude and Codex agree and that each is the best long-term design (both confirmed; every row carries Codex agreement). The full rows (question, options, reason, recommendation, agreement, revision and notes) are in [core_rows.json](../archive/decisions-2026-11-05/core_rows.json) and [other_rows.json](../archive/decisions-2026-11-05/other_rows.json); the concrete rules behind them are in [R4_RATIONALE.md](../archive/decisions-2026-11-05/R4_RATIONALE.md), and where R4 and a row differ, the row governs.

**The Core rows are a provisional approval (stage A2a).** They bind the design without freezing it. Stage A2b formalizes them in a proposed `ConcreteSpec/Spec.lean` and proves the four design theorems; any row the formalization changes returns to the owner; the owner freezes Core at A2c (row Q9.19). Until a rule is written in Lean, the answer below governs over any chapter text that differs.

<a id="decided-2026-11-05-revisions"></a>**Owner-approved revisions of recorded rules** (each also noted at the rule it revises):

- **OD45 and OD87** (rows Q5.1, Q5.4): capability arguments are written explicitly at every polymorphic call and checked against the callee; there is no capability inference and no inference solver. OD46 (type arguments from checked argument types) is unchanged.
- **TYPE-22@1's examples** (row Q6.4): a constant whose initializer provably fails is refused at compile time whether or not it is used (OD118); both examples, `unused-constant-never-runs` and `constant-traps-at-use`, become reject examples. When a constant is evaluated (at each use, never at start) is unchanged.
- **TRAIT-1@1** (row Q2.8): two impls with equal heads may coexist only when Copy/Destroy disjointness (OD28) makes their domains disjoint, as in `impl<T: Copy> Destroy for Vec<T, A>` beside `impl<T: Destroy> Destroy for Vec<T, A>`. No other coherence or orphan rule changes, and no disjunctive bound syntax is added.
- **The generated-chapter requirement** (rows Q9.15, P3): Lean is the specification, in one main normative file with doc comments and examples; no prose is generated from it. This confirms the [2026-11-04 decision](#decided-project-lean-is-the-spec).
- **Authority rules 13, 5 and 7** (row Q7.11): `Console` joins the reserved capability set, for operations on the `StdIn`, `StdOut` and `StdErr` grants; rule 5's operational list and rule 7's primitive table include it. `Device` stays for device grants.
- **The text-only slice of R4 F4.3** (row Q4.7) is replaced before it was recorded: one scoped form opens every view, `borrow [mut] source [from a until b] as view { … } [else { … }]`. Without `else` an invalid range traps, like indexing; with `else` it is handled; a proven invalid range without `else` is refused (OD118). Arrays need `a ≤ b ≤ len`; text also needs UTF-8 scalar boundaries at both byte offsets.
- <a id="decided-2026-11-05-q6-2-fields"></a>**Q6.2 and R4 F12, known values through structs** (owner-approved revision, 2026-11-05, made during A2b): known values propagate field by field through immutable struct construction and destructuring. The checker tracks an abstract value for each binding: unknown, a known scalar, or a constructor whose fields carry their own abstract values. Constructing a struct records each field's fact independently; an immutable binding keeps the fact; destructuring hands each field's fact, recursively, to the corresponding immutable binder; an unknown component stays unknown without discarding its known siblings; an ownership transfer keeps the facts for the receiving binding without permitting reuse of the consumed one. Q6.2's exclusions stay (mutable state, branches and paths, user-function calls, calls outside the table, elaborator temporaries). So `let S { a } = S { a: 200u8 }; a + 100` is refused, in trusted and unreachable code too (OD118 as revised by Q6.4). Before this revision a field bound by destructuring was not known and that program was accepted and trapped.
- <a id="decided-2026-11-05-q9-1-reads"></a>**Q9.1, read forms** (owner-confirmed, 2026-11-05): `copy x` is allowed only when `x` has a Copy type and `move x` only when it has a non-Copy (linear) type. Each read has one form; `move` of a Copy value and `copy` of a linear value are both refused.

<a id="decided-2026-11-05-new-rules"></a>**New rules** (P4): duplicate variant names in one enum and duplicate method names in one impl are refused, like duplicate fields (bug 313); the rule gets its ID and diagnostic when it is written in Lean.

<a id="decided-2026-11-05-core"></a>**Core rows** (the answer is the row's recommendation):

- **Q1.1** What is an allocator brand? — Separate erased `Brand` kind.
- **Q1.2** How are fresh brands introduced, including loop resets? — Generative binders; existential loop state.
- **Q1.3** How are packages formed and opened? — Explicit pack; lexical unpack; repackage escapes.
- **Q1.4** How are brands compared and supplied? — Nominal brand equality; explicit brand arguments.
- **Q1.5** How are allocator dependencies tracked across calls? — Structural dependencies; checked interface summaries.
- **Q1.6** Who owns a child allocator’s backing? — Child owns backing; parent brand exposed in type. *Revises or adds: Clarify/extend allocator interface.*
- **Q1.7** Where does release information live? — Stable release state and raw context; no safe references.
- **Q1.8** What if creating allocator state fails? — Fallible allocator construction conserving surviving inputs.
- **Q1.9** What do allocation failures contain? — `AllocFailure<T>` returns rejected `T`; operation-specific recovery.
- **Q1.10** Are capacity and addresses observable? — Observable capacity; explicit raw-address exposure.
- **Q1.11** What does “unchanged” require of allocator state? — Availability predicate plus four laws: progress, unchanged state on failure, release never reduces availability, a concrete success witness.
- **Q1.12** What rolls back around callbacks? — No rollback of callback effects; every element accounted for in a typed result.
- **Q1.13** Can resetting the system allocator return `Allocator<Sys>` again? — Fresh brand after any supported system reset.
- **Q2.1** Are storage release, destruction and resource cleanup separate? — Separate storage release, destruction and resource cleanup.
- **Q2.2** Which containers implement `Destroy`? — Exactly sixteen named Destroy impls: Array (T: Destroy); Heap, HeapArray and Vec (two each: T: Copy; T: Destroy); String (unconditional); Option (T: Destroy); Result (three: T: Destroy/E: Copy, T: Destroy/E: Destroy, T: Copy/E: Destroy); OrderedMap (four: each of K and V Copy or Destroy). HashMap excluded.
- **Q2.3** In what order are contents destroyed? — Index order 0..len−1, then the backing; OrderedMap in ascending key order, key then value, then the nodes.
- **Q2.4** How does a container of non-`Destroy` resources release its storage? — `drain(v, ctx, f)` with `f: fn(&mut C, T) with(K) -> Step<T, E>`; the only way to empty a container of non-Destroy elements.
- **Q2.5** What happens when drain processing fails? — On Stop, `Drained { at, survivor, rest, err }`; `map_into` returns `Stopped { done, survivor, rest, err }`.
- **Q2.6** Must `destroy` terminate and avoid traps? — Ordinary partial correctness; prove stronger guarantees separately.
- **Q2.7** Does user destruction have special execution semantics? — Ordinary consuming checked `destroy`; at most `Alloc`.
- **Q2.8** How is 'Copy or Destroy' written for container impls? — (a) separate impls for every admitted combination of Copy/Destroy payload bounds, no disjunctive-bound syntax: two each for Heap, HeapArray and Vec, three for Result, four for OrderedMap; coherence may use Copy/Destroy disjointness. *Revises or adds: TRAIT-1@1 (under OD6): equal generic impl heads allowed only when Copy/Destroy disjointness makes their domains disjoint; all other coherence and orphan rules unchanged. Option (b) would also add new bound syntax.*
- **Q3.1** What does opening a heap array produce? — Second-class shared/mutable array views; no element references.
- **Q3.2** How are views passed and lent? — Downward parameters; mutable lending freezes origin.
- **Q3.3** What is a view’s root identity? — Original owner identity plus projection path.
- **Q3.4** Which view accesses conflict? — Existing whole-root conflict rules.
- **Q3.5** What is the index type? — `u64` lengths/indices across profiles; checked target limits.
- **Q3.6** How are nested linear elements traversed without substitutes? — `drain` / `map_into` for consuming traversal; `swap_at(v, i, x) -> T` for in-place replacement.
- **Q3.7** What is the raw-memory model and validity taxonomy? — Blocks with provenance; pointer = (prov: Option<BlockId>, addr: u64); integers carry no provenance; every access needs explicit validity premises.
- **Q3.8** How do zero-sized elements behave? — Zero-sized slots own their obligations; they occupy no bytes and may share addresses.
- **Q3.9** What do pointer equality and integer round trips mean? — `expose` records the block; `from_exposed` picks in-bounds, then one-past, else no provenance.
- **Q3.10** What happens when address space runs out? — A collision means overlapping occupied bytes; address exhaustion is part of Avail.
- **Q3.11** How are typed values related to raw bytes? — Structured values and raw store joined by `Rep`.
- **Q3.12** How are extern pointer parameters marked, and how does C keep memory past the call? — (a) `kept` parameter marker; `Kept<H>` receipt ended only by the paired extern. *Revises or adds: New design + spelling: owner decision.*
- **Q4.1** What is a literal’s type? — Copy `StaticStr`; explicit fallible dynamic conversion.
- **Q4.2** Which encoding and NUL policy apply? — UTF-8 including embedded NUL.
- **Q4.3** What do length and comparison mean? — Byte length; content equality; lexicographic byte order.
- **Q4.4** Are literal addresses observable and literals mergeable? — Content semantics; pooling allowed; no safe address identity.
- **Q4.5** What is the FFI representation? — Explicit pointer/byte-length FFI adapter.
- **Q4.6** What are dynamic string views? — Byte offsets; `byte_at` traps out of range; subviews only through the scoped borrow form of Q4.7 (`borrow t from a until b as s { … } [else { … }]`).
- **Q4.7** How are scoped views and range subviews introduced? — (a) borrow [mut] source [from a until b] as view { … } [else { … }]; no else = trap (like indexing); proven invalid range without else refused (OD118). *Revises or adds: Replaces R4 F4.3 text-only mandatory-else form; extends Q3.1/Q3.2 with subview formation; preserves OD36/OD118.*
- **Q5.1** Should capability arguments ever be inferred? — Explicit capability arguments on every polymorphic call. *Revises or adds: OD45, OD87.*
- **Q5.2** May explicit arguments contain the caller’s capability variables? — Explicit symbolic capability expressions and union.
- **Q5.3** What is capability-set equality and normal form? — Canonical semantic equality; refuse repeated names.
- **Q5.4** Is a capability inference solver still normative? — Explicit substitution checking; no inference solver. *Revises or adds: OD45, OD87.*
- **Q6.1** What determines mandatory static refusal? — Fixed Lean-defined local refusal rules.
- **Q6.2** Is local constant propagation mandatory? — Fixed table of statically known values: literals, immutable lets, module constants, checked ops, six named wrapping/saturating ops, lossless `as`. *Revised 2026-11-05: facts propagate field by field through immutable struct construction and destructuring ([revision](#decided-2026-11-05-q6-2-fields)).*
- **Q6.3** Are violations rechecked after generic substitution? — Parametric checking plus substitution-time rechecking.
- **Q6.4** Does unreachable code receive static refusal checks? — Check all written code, including generic bodies. *Revises or adds: REVISES TYPE-22@1 examples.*
- **Q6.5** How do short-circuit operators interact with refusal? — Check both short-circuit operands; execute conditionally.
- **Q6.6** What happens when constant evaluation fails? — Separate invalid and unknown; no evaluator-resource failure.
- **Q6.7** What distinguishes traps, unsafe stuckness and exhaustion? — Separate traps, unsafe stuckness, OOM and fuel exhaustion.
- **Q7.1** How are target grant schemas and multiplicities specified? — One target, linux-x86_64-hosted (Linux ≥ 5.6), with a fixed grant table; main asks for grants as parameters.
- **Q7.2** How do narrowing, acquisition and splitting relate? — Narrowing consumes and intersects rights; children are acquired from a grant that stays.
- **Q7.3** What are close and close-error contracts? — close always consumes; failure is a Copy error holding no handle; listen failure returns its grant.
- **Q7.4** How does process inheritance work? — Deferred: this target offers no process spawning.
- **Q7.5** Which environment does a program observe? — Scoped immutable launch environment snapshot.
- **Q7.6** What does a clock grant specify? — Explicit clock identity, kind, resolution and guarantees.
- **Q7.7** What are network scopes? — Connect and listen grants with numeric scopes; a separate Resolver grant with explicit server, transport, suffixes and record types.
- **Q7.8** Is grant policy enforced statically or dynamically? — Static authority checks plus atomic dynamic scope checks.
- **Q7.9** How are blocking, terminal actions and delivery observed? — Split request/response events; logical terminal observations.
- **Q7.10** Do you approve the linux-x86_64-hosted grant table? — Approve R4 F6's complete grant table below as part of this row: every launch configuration, operation signature, permission, narrowing rule and ending contract. Standard-stream operations require Console (Q7.11), not Device; where the R4 F6 table says with(Device) for StdIn/StdOut/StdErr, read with(Console). A missing configured grant refuses startup with exit 125; a trap exits 134. *Revises or adds: New target contract: owner decision.*
- **Q7.11** Which permission do the standard streams use? — Add a Console capability for StdIn/StdOut/StdErr operations; keep Device for device grants. *Revises or adds: Authority rule 13 (adds Console); rules 5 and 7 lists include it.*
- **Q8.1** What is the first proof endpoint? — Proved source→BIR; qualified native; conditional image claim.
- **Q8.2** What exact source subset is admitted? — A1 minimal `u8`/linear-struct component subset.
- **Q8.3** What establishes frontend correspondence? — Independent byte-to-Core correspondence judgments.
- **Q8.4** Does the first artifact qualify as a complete executable? — Component plus bound launcher first.
- **Q8.5** What launcher ABI and observations are fixed? — Pinned launcher ABI, inputs, startup and trap mapping.
- **Q8.6** What artifacts does the claim bind? — Bind complete retained dependency and image closure.
- **Q8.7** What prevents vacuous correctness? — Coverage, realizable witnesses and rejection controls; a spec where every allocation fails fails qualification.
- **Q9.1** What is Core’s expression and binding form? — ANF; fresh identities; explicit moves/copies; separate places. *Confirmed 2026-11-05: `copy` only for Copy types, `move` only for non-Copy types ([read forms](#decided-2026-11-05-q9-1-reads)).*
- **Q9.2** What types and declaration identities must Core represent? — Kinded nominal inventory; ordinary owning container definitions.
- **Q9.3** What is the single loop form? — Core `while L { C } do { B }`: C is a condition region re-run every iteration; the counted loop advances its counter before the body.
- **Q9.4** How are data, matching and exits represented? — Flat data/matches; structured exits; checked stores.
- **Q9.5** Are heap opening and borrowing separate user mechanisms? — One surface borrow; distinct typed Core cases.
- **Q9.6** Which operations are primitives rather than ordinary definitions? — Irreducible primitives; checked destructor bodies.
- **Q9.7** How are primitive calls specified? — One exhaustive typed primitive-contract inventory.
- **Q9.8** Does `defer` survive in Core? — Nested deferred scopes; late reads; reverse cleanup.
- **Q9.9** What is evaluator state and execution style? — Total clock-indexed evaluator; explicit instrumented state.
- **Q9.10** How are ownership instrumentation and erasure organized? — Ghost ownership instrumentation with proved erasure.
- **Q9.11** What do contracts mean and when are they checked? — Erased logical contracts; ensures sees result and surviving parameters after cleanup; no snapshot construct.
- **Q9.12** What evidence crosses declaration/module interfaces? — Checked semantic interfaces with reviewable declarations.
- **Q9.13** Are judgments independent of the checker and elaborator? — Independent judgments; checker/elaborator correspondence.
- **Q9.14** What must pass before Core freezes? — Provisional approval, then four kernel-checked theorems against a proposed Spec.lean, then freeze.
- **Q9.15** Where do normative definitions and documentation live? — One main normative Lean file with doc comments. *Revises or adds: Generated-chapter requirement (philosophy rule 6 / earlier roadmap commitment): replaced by one normative Lean file with doc comments and examples, a browsable rendering of that Lean, and coverage-gated retirement of prose chapters; your decision of 2026-11-04.*
- **Q9.16** What Lean dependencies may the specification use? — Core Lean; supporting Batteries; owner exceptions for Mathlib.
- **Q9.17** May spec proofs use `native_decide`? — Kernel-only authoritative spec/A2 proofs.
- **Q9.18** How is the old compiler retained? — Frozen isolated `legacy/`; no normative fallback.
- **Q9.19** How do approval, formalization and Core freeze relate? — A2a → A2b → A2c; Spec.lean headed PROPOSED until freeze; R-0560 depends on the freeze. *Revises or adds: Process: owner decision.*

<a id="decided-2026-11-05-project"></a>**Project rows:**

- **P1** Where agent discretion ends — Claude and Codex reason every question from the philosophy and recommend; they never pick a language answer through code, fixtures, diagnostics or docs. 'Important' = anything that revises a decided rule, changes observable language meaning you have not ruled on, or that you reserved. Every important decision comes to you.
- **P2** Priority order when principles conflict — Soundness and provability > transparency (with auditability, honest trust boundaries and compiler-owned identity) > one authoritative definition (checked derived views allowed) > explicitness at the use site > brevity. The philosophy guides open choices and never silently reopens decided rules.
- **P3** Lean is the spec: confirm the amendment — Lean is the specification: confirmed together with Q9.15 (revision below).
- **P4** Duplicate enum variants or methods (bug 313) — Refuse duplicate variant names in one enum and duplicate method names in one impl, with a new rule and diagnostic, the same as duplicate fields.
- **P5** legacy/ is a byte-frozen snapshot, never patched or run — legacy/ is main's tree at the move commit, byte-frozen. A root check compares its tree hash, allowing only deletions a satisfied RETENTION.tsv entry authorizes. No legacy gate ever runs. The oracle is the tag-built artifact (P13), not the legacy/ copy.
- **P6** Retention manifest gates deleting legacy/ — RETENTION.tsv at the root gives every legacy asset (corpus, regressions, examples, std, bug ledger, site, docs, research) a destination and deletion criterion. B3 deletes only the old pipeline (compiler sources, their scripts, gates, build files). legacy/ disappears when RETENTION.tsv has no live entry, not at B3.
- **P7** Prove the new landing path before moving — T0: land beside the old tree the new root CI, required-checks.txt (enforced by train watch and land), RETENTION.tsv, the task disposition table, ORACLE.lock with the oracle artifact, and ISSUES.tsv. T1: rehearse the move on a throwaway branch: fresh clone, build, hook, exact-SHA CI, train --dry-run land; a broken build, a failing gate and a missing required check must each be refused. T2: the move in one train.
- **P8** Server services under scripts/ — Keep only landing services: train, review, hook, landing bot. The status page, work queue, fuzz farm, perf tracking and nightly campaign all stop and are recorded as archived in RETENTION.tsv; each returns only with a roadmap task.
- **P9** Website during the transition — The website goes down at the move and is redone from scratch with the new compiler; no frozen playground is served. Unpublishing the live site is an outward action whose moment the owner confirms.
- **P10** What 'closed' means for old-compiler bugs — Old-compiler defects become 'retired with the old implementation', never 'fixed'. Each keeps its reproducer, ruling and structural class; 106 reproducers feed the oracle comparisons. Bugs the frozen browser compiler still shows stay visible while the site serves it.
- **P11** Std bugs and rulings are carried — Add a 'std' class: 12 std bugs move with std as contracts plus regressions. The 112 language rulings (26 still open) keep their ruling and become conformance examples.
- **P12** Issue record from day one — From day one, ISSUES.tsv at the root: id, date, area (spec|proof|compiler|launcher|ci|migration), evidence, owner, severity, closure condition, status, with a small root check. Spec, proof, launcher, CI and migration defects go there. The full bug ledger and tool return with slice 1.
- **P13** concrete-ci-2 as the only CI host — CI runs on concrete-ci-2 (self-hosted) and on GitHub-hosted runners as a regular second pool, not only a fallback. Jobs that need ci-2 caches run there; the oracle is fetched as a release asset verified by digest; a check neither pool can run is incomplete and blocks landing. Self-hosted runners never run pull_request jobs.
- **P14** ci-3 handover: an exception to the archive requirement — Done by the owner on 2026-11-04: concrete-ci-3 was handed to stabileo; its fuzz corpus, perf history and leanprof data were not copied (an exception to the archive requirement).
- **P15** New roadmap: retiring obligations — Accept the new single-file ROADMAP.md (wip/new-roadmap, reconciled: prepare → rehearse → move, assurance wording per Q8.1, Phase E per P18, decides nothing itself): 79 tasks kept as rows, 345 folded into area tasks with their obligations, 109 deferred with triggers, 7 retired, 15 done.
- **P16** Status changes in the refusal list — Accept the new statuses in PHILOSOPHY.md. Now Permanent: operator overloading (OD26), capability hiding (authority rule 5), derived equality, mutable global state (OD8), umbrella capabilities and aliases, old/quantifiers in contracts (OD117), lexical shadowing (OD67), and ghost code that affects execution. Ghost code itself is admitted as decided (OD81). Foreign function values reopen when typed extern fn types are admitted (OD4); concurrency reopens on R-0399's trigger.
- **P17a** Default arguments — Default arguments: a Permanent refusal, reopened only by a new decision from you. They conflict with visible arguments.
- **P17b** Compile-time evaluation and broad #[cfg] — comptime and broad #[cfg]: keep evidence-based reopening, only with a workload, a proof story, phase separation and an audit report, and only by a new decision from you.
- **P18** Phase E: contracts and termination within OD115–OD117 — Phase E: no source old, quantifier, implication or snapshot construct (OD117); entry/exit and frame properties are Lean theorems (a source frame clause would be a new decision for you); spec functions have a body and are total (OD115); ordinary functions have no total category (OD116) and get per-function termination theorems where wanted. R-0524 becomes 'Frame and entry/exit theorems in Lean, no old'; R-0526 stays 'checked totality of spec functions'.
- **P19** Mutation testing for the new compiler — Retire the old mutation-harness defect work (R-0518). The new compiler gets its own mutation harness under R-0498, not a port of the old one.
- **P20** One home for binding decisions — Move the still-binding roadmap decisions (new compiler, retention contract, authority, allocation, Lean as the spec) into docs/decisions.md beside your language answers. The dated log becomes history; ROADMAP.md links to decisions.md and restates nothing.
- **P21** Retention records outlive legacy/ — RETENTION.tsv, ISSUES.tsv, required-checks.txt, scripts/oracle/ and docs/archive/roadmap-2026-11/ (with task_disposition.tsv) are part of the day-one root, outside legacy/, so they survive deleting it.
- **P22** The work-queue dispatcher — Stop the dispatcher for the lean start. Agents are started by hand from the roadmap; bring the dispatcher back only when there is enough parallel work to need it.
- **P23** Precedence between decisions, rows and drafts — Recorded decisions govern. An accepted row supersedes a recorded rule only through an explicitly identified, owner-approved revision (Q5.1/Q5.4 for OD45/OD87, Q6.4 for TYPE-22, Q2.8 for TRAIT-1, Q9.15/P3 for generated chapters). R4's replacements govern conflicting draft recommendations. Before your acceptance is recorded, every passage R4 F18 lists is reconciled (capability inference, generated chapters, roadmap authority, assurance wording, restart order and retention, Phase E, freeze dependencies, superseded Core recommendations), and historical conflicting text is marked superseded.

<a id="decided-project"></a>
### Project and architecture decisions

These are the binding project and architecture decisions, moved here from the archived roadmap
decision log (decision row P20, 2026-11-05) so that language and project decisions have one home.
Each entry summarizes the decision and links its verbatim record in the
[2026-11 archive](../archive/roadmap-2026-11/ROADMAP_DECISIONS.md), which stays as history; where an
entry and its archived record differ, the entry and its revision notes govern. The roadmap,
the philosophy and AGENTS.md cite these entries and restate none of them.

- <a id="decided-project-recording"></a>**Recording rule** (owner, 2026-11-02; extended 2026-11-05).
  The owner decides every language question and every important project question. Claude and Codex
  reason from the philosophy and recommend; neither chooses an answer through code, fixtures,
  diagnostics or documents (row P1). Where Claude and Codex agree that an answer is the best
  long-term decision, it is recorded as decided when the owner has given that rule for the question
  ("anything where you and codex agree that is the best decision, record the solution", 2026-11-02;
  the remaining rows of 2026-11-05 under the same condition). Every answer is written here with its
  date ([record](../archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-02--owner-recording-rule)).
- <a id="decided-project-precedence"></a>**Precedence** (row P23, 2026-11-05). Recorded decisions
  govern. A decided row supersedes a recorded rule only through a revision the owner approved and
  that is marked as such ([the 2026-11-05 revisions](#decided-2026-11-05-revisions)). Drafts such as
  the Core proposal and the R4 rationale are background.
- <a id="decided-project-priority"></a>**Priority when principles conflict** (row P2, 2026-11-05).
  Soundness and provability first; then transparency, with artifact-backed auditability, honest
  trust boundaries and compiler-owned identity; then one authoritative definition (checked derived
  views allowed); then explicitness at the use site; brevity last. The order guides open questions
  and never reopens a decided rule. Most code is expected to be written by language models, so the
  language is optimized for checking and reading, never for writing effort.
- <a id="decided-project-spec-first"></a>**Specification first** (owner, 2026-10-30). The spec is
  complete when it is the one normative, simple and checkable statement of the language: owner
  decisions recorded, every normative statement absorbed and audited, removals applied, one shape per
  rule, every language refusal mapped to a rule, a conformance suite, the code matching the spec,
  and the outside-reader test passed. The criteria are read through the Lean-is-the-specification
  entry below ([record](../archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-10-30--specification-first)).
- <a id="decided-project-docs-follow"></a>**Documentation follows the spec** (owner, 2026-10-30).
  Every other document cites the specification and loses any statement it contradicts; no document
  restates a rule the spec owns or describes a removed feature as current
  ([record](../archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-10-30--documentation-follows-the-spec)).
- <a id="decided-project-lean-is-the-spec"></a>**Lean is the specification** (owner, 2026-11-01,
  revised 2026-11-04, confirmed 2026-11-05 by rows Q9.15 and P3). The normative language is Lean:
  each rule is a readable definition with its rule ID, a plain-words doc comment and its conformance
  examples, in one main normative file (`ConcreteSpec/Spec.lean`); proofs and tooling import it. No
  prose is generated from Lean; the 2026-11-01 requirement to generate `SPEC.md` and the chapters is
  superseded. Derived machine views (rule registry, IDs, manifests) keep a staleness gate. Prose
  chapters retire slice by slice, each only after a browsable rendering of the Lean rules and a
  rule-by-rule coverage report show its rules in Lean. The user guide is non-normative. Core is a
  small Lean language and every surface feature is defined by its lowering into Core
  ([2026-11-01](../archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-01--lean-as-the-only-source),
  [2026-11-04](../archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-04--lean-is-the-specification)).
- <a id="decided-project-new-compiler"></a>**A new compiler from the Lean spec** (owner, 2026-11-01;
  architecture approved 2026-11-02). The old compiler is not migrated; it is frozen at the tag
  `legacy-compiler-final` and retired. `ConcreteSpec` imports nothing from any compiler and holds
  the grammar as data with its LL(1) check, Core with identities, the statics as inductive judgments
  (the only normative form) with a checker proved to correspond, the one elaborator, the
  clock-indexed evaluator (the reference interpreter) and the conformance examples. The new compiler
  imports it and builds a proved lowering chain below Core; slice 1 gets a BIR → LLVM printer
  labelled unverified, and the Lean Compiler Backend follows. Revised 2026-11-05 by
  [proof infrastructure](#decided-project-proof-infrastructure): slice 0 gets the first BIR → LLVM
  printer and slice 1 extends the same path. Std, examples and tests migrate to the
  decided language; the old pipeline is deleted after slice 1
  ([record](../archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)).
  The record's "the browser compiler keeps running from the tag" is revised by row P9 below.
- <a id="decided-project-restart"></a>**The restart and the `legacy/` folder** (owner, 2026-11-04;
  details rows P5–P8, P21, P22, 2026-11-05). Everything old moves under `legacy/` on main in one
  train. `legacy/` is a byte-frozen snapshot of main's tree at the move commit, never patched and
  never run; one root check refuses any change under it except a deletion its retention row allows,
  and no legacy gate runs (P5). Every legacy asset has a row in the root `RETENTION.tsv` with a
  destination and deletion condition; only the old pipeline (compiler, its gates and their scripts)
  is deleted after slice 1, and retained assets stay until their rows resolve (P6). The new root's
  infrastructure lands first beside the old tree (T0: new CI, `required-checks.txt` enforced by the
  train with an explicit old-to-new transition mapping, `RETENTION.tsv`, the oracle lock, `ISSUES.tsv`),
  the move is rehearsed on the exact candidate root (T1), and only then does it land (T2) (P7). Only
  the landing services keep running; the status page, work-queue dispatcher, fuzz farm, perf tracking
  and nightly campaign are archived and return only with a roadmap task (P8, P22). The retention
  records live outside `legacy/` and survive its deletion (P21). The differential oracle is built from
  the tag in an isolated checkout, published as a release artifact verified by digest, and never
  overrides the Lean semantics.
- <a id="decided-project-oracle-release"></a>**The oracle release** (owner-approved, 2026-11-05). The
  oracle is published as the GitHub release
  [oracle-legacy-compiler-final](https://github.com/unbalancedparentheses/concrete2/releases/tag/oracle-legacy-compiler-final):
  the frozen compiler at tag `legacy-compiler-final` (commit `066c9d9b8836a2b176aa37f6c8313195a4b4ee29`),
  asset `oracle-legacy-compiler-final-x86_64-linux.tar.gz` with SHA-256
  `74d8f1e3b5214d9c64c25736a24c19458736a2dacc7977ff784506839b9f26a2`. It predates the decisions of 2026-10-29 to 2026-11-05 and is
  not the current language.
- <a id="decided-project-site"></a>**The website** (owner, row P9, 2026-11-05). The website goes down
  at the move and is rebuilt from scratch with the new compiler. No frozen playground is served in
  between. Unpublishing the live site is an outward action; the owner confirms its moment. The owner
  confirmed on 2026-11-05 that the site is taken down at the move (T2).
- <a id="decided-project-ci"></a>**CI hosts** (owner, row P13, 2026-11-05). CI runs on concrete-ci-2
  (self-hosted) and on GitHub-hosted runners as a regular second pool. Landing and oracle runs use
  separate pools with per-pool CPU and memory limits and at most two heavy runs per machine. A check
  neither pool can run is incomplete and blocks landing; it is never reported as passed. Self-hosted
  runners never run pull-request jobs.
- <a id="decided-project-bugs"></a>**Bugs across the restart** (rows P10–P12, 2026-11-05). Old-compiler
  defects are retired with the old implementation, never marked fixed; each keeps its reproducer,
  ruling and structural class, and the reproducers feed the oracle comparisons (P10). Std defects
  move with std as contracts and regressions, and the language rulings become conformance examples
  or recorded owner questions (P11). From day one `ISSUES.tsv` at the root records spec, proof,
  launcher, CI and migration defects (id, date, area, evidence, owner, severity, closure condition,
  status) with a root check (P12).
- <a id="decided-project-roadmap"></a>**The roadmap** (row P15, 2026-11-05). `ROADMAP.md` is one file
  that plans and decides nothing; the previous roadmap is archived with a disposition for each of its
  555 tasks. Mutation testing for the new compiler is a new harness, not a port (P19).
- <a id="decided-project-proof-order"></a>**Compiler proof order** (2026-11-01). The semantic contract
  and the final theorem first, with values-to-bytes a named boundary; then executable Core semantics
  (fuel exhaustion distinct from divergence) with type, ownership and effect soundness; one minimal
  end-to-end path; functional models for safe code; open-module correctness and C contracts; ghost
  permissions for trusted code; SMT-checked lowering; proof-producing equality saturation. Parsing,
  linking and native-image assumptions stay explicit; only kernel-checked proofs count; items 4–7
  are proposed to the owner before each is built
  ([record](../archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-01--compiler-proof-order)).
  Strengthened 2026-11-05 by [proof infrastructure](#decided-project-proof-infrastructure): the
  minimal path is built in cumulative slices, each used lowering proved as it lands.
- <a id="decided-project-proof-infrastructure"></a>**Proof infrastructure from the first commit and
  cumulative compiler slices** (owner, 2026-11-05: "i want to be able to prove from one point to the
  other things from the beginning, everything should be designed to be proven right"; "we could start
  by simple additions and things but the infra should be there"; "yes first unverified printer to llvm
  and to lean backend we created right"; "and then we can do end to end proof"; Claude and Codex agree).
  - *Slice 0.* The first executable compiler slice is slice 0: one source unit, no imports or grants,
    parameterless `main` returning `u8`, explicitly typed immutable `u8` bindings, representable decimal
    literals, local reads, grouping, checked addition and final return; other forms are refused as
    unsupported. It traverses the actual frontend and each used lowering IR to BIR and an explicitly
    unverified LLVM native build. Its examples include exit 42 from `20 + 22`, exit 255 at the
    representable boundary, static overflow refusal, out-of-range literal refusal, unused-binding
    refusal and malformed-input rejection; checked addition is proved over arbitrary representable
    operands at the IR boundary, overflow included. Slice 0 work may start as a provisional skeleton
    before the Core freeze (A2c), but it makes no authoritative or normative claim before the freeze.
    Slice 0 is an engineering milestone: it does not replace slice 1's ownership, component/launcher,
    accepted-trap or legacy-retirement obligations, and it does not trigger legacy deletion.
  - *Proofs as each piece lands.* Every used IR has total executable Lean semantics and
    well-formedness conditions. Every implemented lowering lands with kernel-checked preservation and
    output-invariant proofs, directly or through a proved validator; unused future passes are not
    required to exist. Frontend correspondence is stated independently; every undischarged frontend
    obligation is explicit. The shipped compiler is these Lean definitions.
  - *The composed theorem from the first slice.* Slice 0 includes a kernel-checked composed theorem
    with separately named undischarged semantic premises (the holes), exact artifact binding, minimal
    provenance and an inventory distinguishing logical axioms, implementation assumptions, execution
    trust and model/environment contracts. Authoritative proofs use no `sorry`, `native_decide` or
    axioms outside `propext`, `Quot.sound` and `Classical.choice`. CI checks both transitive axioms
    and the registered semantic-premise closure, and fails when a stage is covered by neither a
    theorem nor a registered premise. Claims with undischarged compiler implementation obligations are
    labelled conditional.
  - *The holes only shrink.* For an unchanged supported subset and target profile, no undischarged
    obligation may be added or strengthened without owner approval. Splitting, renaming or
    discharging obligations preserves recorded lineage. Extending the subset, runtime, environment or
    target profile publishes an explicit assumption diff; new or strengthened assumptions need owner
    approval.
  - *Example-driven growth.* Features extend this path cumulatively, each with rule-indexed accepted
    and refusal examples, trapping examples where admitted, semantics, checker correspondence, the
    applicable soundness and preservation proofs, a program-specific correctness theorem for its
    motivating example and classified Lean/native differential tests. Programs grow from simple
    examples to file-copy (phase C's acceptance workload) and small real programs, ordered by the
    constructs they need, then to larger real software.
  - *Two kinds of proof.* Compiler correctness (the composed theorem: the executable does what the
    source means, under its named premises) and program correctness (Lean theorems about one program:
    its functional behaviour, failure behaviour and authority) are proved separately; a program is
    fully proven only with both. A postcondition does not establish termination or freedom from
    traps; capability checking establishes allowed authority use, not application correctness.
    Inspection comes from capabilities and proofs. The legacy oracle is a differential cross-check
    only and never overrides the Lean semantics.
  - *Backend order.* Unverified BIR → LLVM first, then BIR → LCB with its adapter proof, then native
    proof closure for the supported profile. LLVM and LCB claims have separate assumption
    inventories; proving the BIR → LCB adapter removes that adapter obligation only when composed with
    the applicable LCB theorems, and the LLVM profile stays unverified.
  - *What it revises.* It amends the new-compiler row (slice 0, not slice 1, gets the first
    BIR → LLVM printer; slice 1 extends the same path) and strengthens the proof-order row's minimal
    path (proofs as each used lowering lands). It adds an earlier subset before row Q8.2's slice-1
    subset and an earlier conditional image theorem before row Q8.1's slice-1 endpoint, neither of
    which upgrades native assurance; Q8.1's source-to-BIR endpoint for slice 1 stands. Rows Q8.3–Q8.7
    (independent frontend judgments, component/launcher separation, exact artifact closure,
    non-vacuity), the A2b → A2c freeze (Q9.14, Q9.19), P18 and OD115–OD117 are inherited unchanged;
    early example-specific Lean proofs need no new source contract forms. The arithmetic decisions,
    grant schema, target order and retention decisions are not reopened.
- <a id="decided-project-contracts"></a>**Contracts and termination in verified growth** (row P18,
  2026-11-05). No source `old`, quantifier, implication or snapshot construct (OD117); entry, exit and
  frame properties are stated as Lean theorems (a source frame clause would be a new owner decision);
  spec functions have a body and are total (OD115); an ordinary function can get its own termination
  proof, with no total-function category (OD116).
- <a id="decided-project-refusals"></a>**The refusal list** (rows P16, P17a, P17b, 2026-11-05). The
  statuses in `PHILOSOPHY.md` stand; default arguments are a permanent refusal; compile-time
  evaluation and broad `#[cfg]` reopen only with a workload, a proof story, phase separation, an audit
  report and a new owner decision.
- <a id="decided-project-review"></a>**Review** (owner, 2026-10-18, reaffirmed 2026-11-04: "always codex
  should check everything"). Codex reviews every change, plan, specification, decision row and piece
  of infrastructure before it is published or brought to the owner. A second, independent reviewer
  (Kimi, Grok or agy/Gemini) reviews as well when one is available, as the owner required for cleanup
  branches on 2026-10-28; when none is (quota, credit or sign-in), its absence is recorded with the
  review and never assumed. A review never replaces the gates.
- <a id="decided-project-throughput"></a>**Development throughput** (2026-10-22). The development
  feedback loop (landing, review, CI) is a priority track beside the main sequence
  ([record](../archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-10-22--development-throughput-priority)).
- <a id="decided-project-supersession"></a>**Editorial supersession of legacy task directions**
  (2026-11-03). The new-compiler and restructure decisions supersede in-place repairs and migration of
  frozen compiler internals; the archived task bodies keep their history and regression obligations
  ([record](../archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions)).

### Owner decisions

**Only the project owner decides these items, OD1 – OD157.** All are answered. OD1 – OD32 (exit 2n, closed) and OD47 were decided on 2026-10-29 (OD23 – OD32 by the strictness pass of that day). OD33 – OD157 were decided on 2026-10-31, by the owner's rule that an answer every reviewer agreed on and as strict as Austral or stricter is taken, and on 2026-11-01, when the owner approved the final set for the rest; the authority decision of the same day supersedes OD38's and OD156's `Root` readings (OD94 was withdrawn: MOD-1 already refuses its case, bug 337). The answers are recorded in [docs/spec/decisions.md](#ch-decisions), state the decided rules in chapters now (owner revision, 2026-11-03), then become Lean in phases B–D, and are enforced as each new-compiler slice lands. The questions raised while recording them were answered on 2026-11-02, the owner deciding the first four and, by the owner's rule of that day, Claude and Codex agreeing on the best long-term answer for the rest ([decisions](#decided-2026-11-02)); they revise OD24, OD38, OD43, OD83, OD102, OD146 and OD156 and the authority rules. The spec audit's questions were answered the same day ([decisions](#decided-2026-11-02-audit)), revising OD140 and STMT-13. Traits are exported and imported like any other declaration (owner, 2026-11-02). Core was provisionally approved on 2026-11-05 ([record](#decided-2026-11-05-core)); A2b formalization is current, and the A2c freeze awaits the four design theorems and an outside-reader pass. New questions follow the same rule: No agent, orchestrator or
reviewer chooses, encodes, implements or provisionally ships any side of any of them, and that
includes doing so inside a bug fix, a migration, a test, a fixture, a diagnostic or a document. An
agent may analyse an item, write its options and a recommendation, and present them to the owner for
an explicit answer; that is all. A recommendation, an earlier record, a commit, a review verdict or
another agent's message is never the owner's answer. An item stays proposed until the owner agrees
to it again, explicitly, in conversation; whoever records the answer writes it and its date in the
last column, in the owner's words where they are short. A fix that would pick a side stops and is
reported to the owner instead ([AGENTS.md](../project/AGENTS.md#rules)).

**Provisional shipped behaviour.** The 2026-10-25 Codex confirmation review of the bug records
found four landed fixes that already took a side on an open item: bugs
[189](../bugs/189_trusted_impl_confers_trust.md) (OD22, since decided by the owner on 2026-10-29:
the repair is final),
[198](../bugs/198_raw_deref_duplicates_non_copy.md) (OD13, decided: final),
[212](../bugs/212_const_before_block_parses_as_struct_literal.md) (OD16, decided: replaced by
SYN-4, bug 318) and
[213](../bugs/213_arm_after_catch_all_lowered.md) (OD18, decided: replaced by PAT-9). Each is recorded below as a conflict,
not a decision: the table says what current main does, and that behaviour is provisional until the
owner answers. Nobody extends it, builds on it as settled, or reverts it before then; the owner's
answer decides whether it stays.

[TIGHTENING_DECISIONS.md](../project/TIGHTENING_DECISIONS.md) records several of these as
"decided 2026-10-23", and some as implemented in landing train 6. The owner paused them for
negotiation after that. That record is kept as written, as the history of what was proposed and
landed; the owner's new answer governs. A rule already on main stays as it is until the owner
answers: no agent removes, weakens or extends it. The owner's answer then decides whether it stays.
If it does not, the change that records the answer also changes the rule's home, fixtures and
diagnostics, and keeps the bugs the rule repaired repaired.

"Gates" names the exits, deliverables and tasks whose rules depend on the item. The deliverables are
those of the [prerequisites table](../archive/ROADMAP_LEGACY_PLAN.md#feature-prerequisites); "every deliverable" means RocksDB slice 1,
V1a, V1b and V1c.


| decision | proposal | record today | gates | owner's answer |
|---|---|---|---|---|
| OD1 | <a id="od1"></a>References: the Austral model. A reference is formed by borrowing a variable or projecting through a reference; `&s.f` is sugar; a temporary is never a root; loans are keyed by the root variable; a raw-pointer dereference needs a trusted validity premise | D2 and §4.5; implemented in landing train 6 (REF-5, REF-11) | exit 2i (G12), so every deliverable; bug 182's repair (exit 0b); V2 (row 6) | decided 2026-10-29 by the owner: the Austral model: a reference is formed by borrowing a variable or projecting through a reference; `&s.f` is sugar; a temporary is never a root; references are second-class, never stored or returned; a raw-pointer dereference needs a trusted validity premise; loans are keyed by the root variable (confirmed by OD15 (a), decided 2026-10-29) |
| OD2 | <a id="od2"></a>`&mut` arguments: an implicit reborrow; overlapping exclusive arguments refused; a deferred call keeps its loan until it runs; one defined order for cleanup, loan end and write-back | D1 and §4.5; the reborrow implemented in landing train 6 (REF-10); the defer and exit-order rules not yet | exit 2i (G9, B1, B2), so every deliverable; R-0496 (Core reborrow form) | decided 2026-10-29 by the owner: (1) passing a `&mut` on is plain lending, each call's use ending when it returns, with no reborrow concept or syntax; (2) a call is refused when an exclusive argument overlaps any other argument (`h(r, r)`, `h(&mut x, &x)`, `h(&mut a, &mut a.f)` refused); as tightened by OD15 (a) the same day, two arguments that borrow the same root conflict whenever one is exclusive, so even `h(&mut a.f, &mut a.g)` is refused; (3) `&mut` refers to the place, a write through it is the write to that place at once, and copy-in/copy-out is not part of the language (only an optimization proven equivalent); (4) `defer` keeps OWN-12 and a borrow passed to a deferred call stays live until the call runs; (5) one exit sequence on every non-abort exit: the exit value, then the scope's deferred calls (last registered first, innermost scope first), then the scope's loans end, then the remaining locals are destroyed; an abort runs none of it **Canonical current root-based exclusivity rule: REF-11 and OD15(a); this historical overlap formulation is a reference to that rule.** |
| OD3 | <a id="od3"></a>Pattern binders through a borrowed scrutinee: the binder is always `&T`; its mode is stored in Core; the root cannot be reassigned while a binder's loan lives (the rule that reopened G17's Copy case) | D3 and §4.5; implemented in landing train 6 (REF-9), and with it the refusal of G17's Copy case (closed 2026-10-23 in the references chapter of the specification) | exits 2i (G17) and 2k (explicit match binders), so every deliverable; bug 181's repair (exit 0b); R-0496 | decided 2026-10-29 by the owner: the binder is always `&T`; its mode is stored in Core; the root cannot be reassigned while a binder's loan lives. Already enforced (REF-9, PASS-2, REF-8) |
| OD4 | <a id="od4"></a>Foreign function values: all refused now; typed `extern "C" fn` types later | D4; the refusal implemented in landing train 6 (FFI-1); the typed form not built | bug 166's repair (exit 0b), so every deliverable; R-0285's callback companion example; R-0283, R-0287 | decided 2026-10-29 by the owner: all foreign function values are refused now; typed `extern "C" fn` types may come later |
| OD5 | <a id="od5"></a>Generics: a bare `<T>` means Any; `T: Copy` is explicit; Copy for a generic struct is declared and checked, never derived | D5 (today's behaviour, made normative) and D6 with its refinement (a), under which a generic declared `Copy` is Copy when every argument is; D6 implemented 2026-10-23 (OWN-D1, TYPE-14) | R-0499 and R-0510 (the Core checker and checked specialization, row 4); V5 (row 6) | decided 2026-10-29 by the owner: a bare `<T>` means Any and `T: Copy` is explicit; Copy for a generic struct or enum is declared and checked, never derived, and a generic declared Copy is Copy when every argument is |
| OD6 | <a id="od6"></a>Traits are coherent: one impl per (trait, type), the orphan rule, an ambiguous method call must be qualified, resolution by typed identity | D7; implemented by bug 186's repair (TRAIT-1 – TRAIT-9) | bug 186's repair (exit 0b), so every deliverable; R-0548, R-0549; V5 | decided 2026-10-29 by the owner: traits are coherent: one impl per (trait, type), the orphan rule, an ambiguous method call must be qualified, resolution by typed identity. Completed 2026-11-02: qualified as `Trait::method(&x, ...)` ([decisions](#decided-2026-11-02-audit)) **Canonical current qualification rule: this OD6 as completed by the audit below; the audit supplies syntax, not a second rule.** |
| OD7 | <a id="od7"></a>An unsafe-module marker in source (for example `trusted mod`), which never confers trust by itself; a gate lists the marked modules | D10; pending (TRUST-6, R-0509) | R-0509's trusted-base inventory; RocksDB slice 1 (where its `extern` declarations live); V6 | decided 2026-10-29 by the owner: an unsafe-module marker in source (for example `trusted mod`) never confers trust by itself, a gate lists the marked modules, and trust stays per method (OD22) **Revised 2026-11-03: this historical module-marker requirement was superseded by OD61 (2026-11-01), which removed `trusted mod`; trust and inventory remain per declaration.** |
| OD8 | <a id="od8"></a>Global state: immutable globals everywhere; mutable state only as declared cells in unsafe-marked modules, reached through capabilities; no `static mut` | D8; pending (TRUST-6, R-0509) | R-0509, R-0496; V6 | decided 2026-10-29 by the owner: Austral's capability model, extended. (1) Constants only: no mutable global state of any kind (no `static mut`, no cells); a constant is truly immutable, has no interior mutability and never contains a capability, token or handle. (2) Global resources are linear tokens minted at startup: `main` receives a root resource value (in the spirit of Austral's `RootCapability`) and splits it; hardware and runtime singletons (allocator arena, UART and MMIO, panic buffer, errno, C libraries with internal state) are produced or taken only by `trusted fn` wrappers returning or taking linear tokens, and a gate lists those wrappers. (3) Tokens are ordinary values: every effect shows as a token parameter, under the existing loan and exclusivity rules (OD1, OD2, OD15), with no separate effect system. (4) Interrupt handlers, FFI callbacks and future threads get only the tokens explicitly given to them, by installation, transfer or a lock granting a borrow. (5) No life before `main`, no cleanup protocol for global state, and abort runs nothing (OD10). (6) An implicit-context mechanism, if ergonomics later demands one, is a new owner decision, never a hidden global. The cell variant was rejected after the Codex and Grok reviews (interrupts, FFI re-entrancy, linear cell contents, effect polymorphism, concurrency). Now TRUST-8 to TRUST-10 (pending, R-0557) Revised 2026-11-01 – 11-02: items 2 and 3 are superseded by the authority decision (grants handed to `main`; `with(...)` plus resource values); item 5 stands ([decisions](#decided-2026-11-02-authority)) |
| OD9 | <a id="od9"></a>Modules and interfaces: one file per module, `pub` as the interface, interface artifacts generated by the compiler, no handwritten interface files | D9; pending (TIGHTENING_DECISIONS §5, step 6) | R-0492 and R-0497 (interface data); separate compilation in V6; OD20 | decided 2026-10-29 by the owner: one file per module with `pub` as the interface; interface artifacts generated by the compiler, no handwritten interface files; a gate diffs each module's generated interface between commits so any public-API change shows in review (R-0556) |
| OD10 | <a id="od10"></a>Arithmetic: checked by default, named wrapping and saturating operations, shifts with a checked count; an abort runs no deferred call | T-ARITH, recorded as settled (today's behaviour), with §4.5's abort rule | exits 2h and 2j, so V1a, V1b and V1c (checked `+ - *` and their traps); R-0493 | decided 2026-10-29 by the owner: integer arithmetic is checked by default; wrapping and saturating arithmetic are named operations; the shift count is checked; an abort runs no deferred call |
| OD11 | <a id="od11"></a>Casts: `as` only when lossless by value domain; every lossy conversion is a named function | T-CAST: ARITHMETIC_POLICY §8 (2026-10-21) and §4.5's value-domain rule; not implemented | exit 2k; the V1 increment that admits casts | decided 2026-10-29 by the owner: `as` only when lossless by value domain; every lossy conversion is a named function |
| OD12 | <a id="od12"></a>Contracts (H27): a pure, non-consuming specification judgment, with machine and mathematical arithmetic kept distinct | T-H27 and §4.5; pending (R-0473) | R-0473 (row 12); any V1a or V1b claim stated with a contract | decided 2026-10-29 by the owner: a contract is a pure, non-consuming specification judgment, with machine and mathematical arithmetic kept distinct |
| OD13 | <a id="od13"></a>Raw-pointer ownership: `ptr::read_owned`, an explicit replace or take through `&mut T`, a trusted ownership obligation for a raw address | T-RAWDUP and §4.5; implemented in landing train 6 (TRUST-7). **Provisional (bug 198):** the current compiler refuses a by-value `*p` of a non-Copy pointee through a raw pointer (E0311) and moves the value out only with the intrinsic `read_owned(p)`, with 26 std sites migrated; provisional until the owner answers | RocksDB slice 1 (its trusted binding code); R-0509, R-0514; V6 | decided 2026-10-29 by the owner: a value moves out of raw memory only through `read_owned(p)` or an explicit replace or take through `&mut T`, and a raw address carries a trusted ownership obligation; bug 198's provisional repair is final |
| OD14 | <a id="od14"></a>Type validity (G2, G13, G15, G16): one structural well-formedness judgment before specialization; function types that return a reference refused everywhere | T-G2, T-G13, T-G15, T-G16; implemented 2026-10-23 (G13, G15 and G16 recorded closed in the references chapter of the specification) | exit 2i, so every deliverable | decided 2026-10-29 by the owner: one structural well-formedness judgment runs before specialization, and function types that return a reference are refused everywhere |
| OD15 | <a id="od15"></a>G10: whole-owner freezing under a shared borrow stays. G19: no pass assumes `&mut` exclusivity, and the operational loan rules are defined now | T-G10 (keep the freeze) and T-G19 (the policy sentence) | exit 2i (the G10 decision record), so every deliverable; R-0496, R-0505 | decided 2026-10-29 by the owner. (a) Whole-owner freezing: a live borrow of any part of a value freezes the whole owner, and loans are keyed by the root binding (REF-11 as in the current compiler); this tightens OD2's part 2: at a call, two arguments that borrow the same root conflict whenever one is exclusive, even disjoint fields (`h(&mut a.f, &mut a.g)` is refused, as in the current compiler); OD1's "keyed by the root variable" stands. (b) The strict option: the language guarantees `&mut` exclusivity, which the checker enforces (OD2, OD15a), and no compiler pass or optimization may assume or exploit it until the checker's guarantee is proven in Lean (R-0499 with the Core semantics R-0496); until then the operational loan rules are defined now, G19 included: a pointer or alias derived from a loan is valid only while that loan lives. Now REF-11@3 (partial, bug 319, which OD23 adds) and REF-14 (pending, R-0496) **Canonical current root-based exclusivity subject: REF-11 (including OD23’s re-lent case); OD2 and OD23 refer here rather than defining separate variants.** |
| OD16 | <a id="od16"></a>A struct literal in an `if`, `while` or `match` head is refused unless parenthesized | not in the record. **Provisional (bug 212):** the current compiler reads an uppercase name followed by `{` in an `if`, `while` or `match` head as the start of a struct literal, refuses `if n > LIMIT { … }` with an E0001 that suggests parentheses, and states that case rule in `grammar/concrete.ebnf` and SYN-1; provisional until the owner answers | V1c (V1's surface grammar has conditionals and loops); R-0500 | decided 2026-10-29 by the owner: refused unless parenthesized, with no exceptions; the uniform rule replaces the uppercase-name heuristic (bug 212's provisional behaviour). Now SYN-4 (pending, bug 318); whether a literal inside a call's argument list in a head counts as parenthesized is OD34 |
| OD17 | <a id="od17"></a>A field write requires a `mut` binding. Bug 216: `s.y = 1` is accepted on a binding not declared `mut`, while `&mut s.y` and `a[i] = v` are refused | not in the record; bug 216 open | bug 216's repair (class 5, R-0499); V2 (aggregates) | decided 2026-10-29 by the owner: a field write requires a `mut` binding. Now OWN-10@2 (partial; bug 216 implements it) |
| OD18 | <a id="od18"></a>Match arms: the first matching arm in source order wins; an unreachable arm is refused. Today an unguarded catch-all arm is moved to the end (bug 213) | not in the record. **Provisional (bug 213):** the current compiler first moves unguarded catch-all arms after the other arms (CoreCanonicalize), so a specific arm written after a catch-all moves before it and stays selectable; in that Core order, it accepts an arm after the first unguarded catch-all and does not lower it, and does not refuse it; provisional until the owner answers | exit 2k (the match rules) and exit 2c's differential legs; V2 (match) | decided 2026-10-29 by the owner: the first matching arm in source order wins and an unreachable arm is refused; this changes the current compiler, where CoreCanonicalize moves unguarded catch-all arms to the end (bug 213's provisional behaviour) |
| OD19 | <a id="od19"></a>Outside the language list: the scope of [PROVABLE_V1](../verification/PROVABLE_V1.md), that is, whether discarded statements, guards and ranges are admitted | open; it holds the model-coverage branch (`wip/model-coverage-3`) | that branch's landing; exit 2c's model-diff coverage | decided 2026-10-29 by the owner: the provable subset admits discarded statements, guards and ranges, each entering as its Core semantics lands (this unblocks `wip/model-coverage-3`) **Revised 2026-11-03: the guards and ranges fragments were superseded by OD55 and OD56 (2026-10-31), which removed those forms; discarded statements remain subject to the decided discard rules.** |
| OD20 | <a id="od20"></a>Outside the language list: the visibility of std's internal `std.libc` bindings | bug 226 open (a std API decision) | bug 226's repair (MOD-2); OD9 | decided 2026-10-29 by the owner: std's internal `std.libc` bindings are private to std; programs cannot see them |
| OD21 | <a id="od21"></a>Representation rights of a parent over its child module (added 2026-10-25, from bug 262): when a parent module imports its child's newtype or struct, every stage lets the parent construct the newtype and read and write the struct's private fields, because Check counts the children's declarations among the module's own for construction and field rights; Resolve also lets a parent name a child's newtype as a symbol without an import. Two options: enforce TYPE-11 as written (a newtype or private field is constructed or accessed only inside the module that declares it, so a child is another module), or amend TYPE-11 to grant a parent these rights over the declarations it imports from its children | the behaviour dates from construction rights slice 1 (2026-07-18); it contradicts TYPE-11@1's wording ("outside the module that declares it"); bug 262's record names `tests/programs/module_qualified_impl` as a program that applying TYPE-11 strictly would refuse; the wider impact is not yet measured (no `scripts/tools/rule_impact.sh` scan is recorded); bug 262's record (branch `wip/newtype-ctor-stage`) leaves the question to R-0011 and makes no recommendation | R-0011; the construction-rights fixtures (`module_qualified_impl` and bug 262's `error_262_*` and `regress_262_*` fixtures); bug 262's follow-up (whether Resolve binds a child's newtype in its parent) | decided 2026-10-29 by the owner: only the declaring module; a newtype is constructed, and a private field read or written, only inside the module that declares it, and a parent module gets no rights over its child (TYPE-11 enforced as written) |
| OD22 | <a id="od22"></a>Trust on an `impl` block (added 2026-10-25, from bug 189). Bug 189's repair made the parser refuse `trusted impl` (E0001) and moved trust onto each method (`trusted fn`, 175 methods marked), while TRUST-5@1 in [the specification](#TRUST-5) still says a `trusted fn` or `trusted impl` may perform raw-pointer operations. Two options: keep the refusal and amend TRUST-5 to drop `trusted impl`, or restore `trusted impl` as TRUST-5@1 states it and revert bug 189's refusal | **Provisional (bug 189):** the current compiler refuses `trusted impl` and trusts only methods declared `trusted fn`; the specification text is unchanged, so the current compiler and TRUST-5@1 disagree; bug 189's record cites the 2026-10-23 rule that a marker never confers trust (D10, itself OD7, proposed); provisional until the owner answers | bug 189's repair (exit 0b) and its pins (`error_189_trusted_impl_refused.con`, `check_trusted_method.sh`); TRUST-5; R-0509's trusted-base inventory; OD7 | decided 2026-10-29 by the owner: trust is per method, `trusted fn` only; `trusted impl` stays refused, so bug 189's provisional repair is final |
| OD23 | <a id="od23"></a>REF-11: every argument of a call, the receiver included, is overlap-checked against its root, re-lent `&mut` bindings included (added 2026-10-29, from a Grok review of the spec) | the current compiler exempts a re-lent reference binding (REF-11@2), so `r.add(r.step)` is accepted; bug 319 | exit 2q; R-0499 | decided 2026-10-29 by the owner ("strict across the board"): every argument of a call, the receiver included, is overlap-checked against the root, including re-lent `&mut` bindings, so `r.add(r.step)` is refused; read the Copy field into a `let` first. Now REF-11@3 (partial, bug 319) **Canonical current rule: REF-11 and OD15(a); this is the historical clarification of its re-lent case.** |
| OD24 | <a id="od24"></a>STMT-7: `main` returns `u8` or `Unit` (added 2026-10-29, from a Grok review of the spec) | the current compiler accepts any result type, masks an integer to 0–255 and maps `bool` natively; bug 320 | exit 2q; V1c | decided 2026-10-29 by the owner ("strict across the board"): `main` returns `u8` or `Unit` (exit 0); no masking, no bool mapping, no other result type. Now STMT-7@3 (partial, bug 320). Revised 2026-11-02 by the owner: `u8` only ([decisions](#decided-2026-11-02-main)) |
| OD25 | <a id="od25"></a>PAT-3/PAT-5: ignoring a payload or a field is written (added 2026-10-29, from a Grok review of the spec) | a bare variant name ignores a Copy payload and a Copy struct `let` may omit fields; bug 321 | exit 2q; exit 2k | decided 2026-10-29 by the owner ("strict across the board"): a bare variant name is legal only for a fieldless variant; ignoring a payload is written `Variant { .. }` or `field: _`; a struct `let` names every field or ignores one explicitly with `field: _`. Now PAT-3@3 and PAT-5@3 (partial, bug 321) |
| OD26 | <a id="od26"></a>TYPE-6 with TRUST-1: operators take numbers only (added 2026-10-29, from a Grok review of the spec) | trusted and `Unsafe` code may compare raw pointers and add an integer to one; bug 322 | exit 2q; R-0502 | decided 2026-10-29 by the owner ("strict across the board"): arithmetic, shifts and comparisons take numbers only; pointer offset, difference and ordering are `ptr::offset` and named functions. Now TYPE-6@2 (partial, bug 322) |
| OD27 | <a id="od27"></a>STMT-6/OWN-15: `break <expr>` is refused at parse time (added 2026-10-29, from a Grok review of the spec) | a value after `break` in a `while` or `for` statement is parsed and dropped; bug 323 | exit 2q; exit 2j | decided 2026-10-29 by the owner ("strict across the board"): `break <expr>` is refused at parse time. Now STMT-6@2 (partial, bug 323) |
| OD28 | <a id="od28"></a>TRAIT-10: a `Destroy` bound is satisfied only by an impl (added 2026-10-29, from a Grok review of the spec) | every Copy type and every array of destroyable elements counts as `Destroy`; bug 324 | exit 2q; R-0499 | decided 2026-10-29 by the owner ("strict across the board"): a `Destroy` bound is satisfied only by an impl; Copy types are not `Destroy` (discard them with `discard`); arrays get one written impl, or one language impl that is its own spec entry. Now TRAIT-10@2 (partial, bug 324); which array form is OD35 |
| OD29 | <a id="od29"></a>OWN-7: a statement expression has type `Unit` (added 2026-10-29, from a Grok review of the spec) | only a syntactically pure non-Unit Copy value is refused (E0294); a call's result is dropped silently; bug 325 | exit 2q; exit 2j | decided 2026-10-29 by the owner ("strict across the board"): a statement expression must have type `Unit`; every other unused result needs `discard(e)`; the syntactic purity heuristic is removed. Now OWN-7@3 (partial, bug 325) |
| OD30 | <a id="od30"></a>MEM-4: `e.f` requires the struct or a reference to it (added 2026-10-29, from a Grok review of the spec) | a field access opens one `Heap` layer implicitly; bug 326 | exit 2q; R-0499 | decided 2026-10-29 by the owner ("strict across the board"): `e.f` requires `e` to be the struct or a reference to it; opening a `Heap` or `HeapArray` uses an explicit non-consuming place form; `*` stays the consuming move-out. Now MEM-4@3 (pending, bug 326); the place form's spelling is OD36 |
| OD31 | <a id="od31"></a>ARITH-2: no `Int` fallback for an unconstrained literal (added 2026-10-29, from a Grok review of the spec) | an unconstrained integer literal is `Int` and a float literal `Float64`; bug 327 | exit 2q; exit 2j | decided 2026-10-29 by the owner ("strict across the board"): an integer literal takes its type from its context, as today; the fallback to `Int` where nothing is expected is removed, so an unconstrained literal is refused; suffixes are not required; float literals get an explicit rule under the same principle. Now ARITH-2@2 (partial, bug 327) |
| OD32 | <a id="od32"></a>OWN-15: no receiver auto-borrow (added 2026-10-29, from a Grok review of the spec) | a `&self` or `&mut self` method on an owned place borrows it implicitly; bug 328 | exit 2q; R-0499 | decided 2026-10-29 by the owner ("strict across the board"): no receiver auto-borrow: a method with `&self` or `&mut self` on an owned place is written `(&s).m()` or `(&mut s).m()`; a receiver that is already a reference is unchanged. Now OWN-15@3 (pending, bug 328) |
| OD33 | <a id="od33"></a>Inline submodules under OD9 (added 2026-10-29, from recording OD9): OD9's answer is one file per module, and today the language also accepts an inline submodule (`mod m { ... }` inside its parent's file, used by most specification examples and many fixtures). Two options: inline submodules stay (a module is one file or one inline block), or every module is its own file and the inline form is refused. Recommendation: every module its own file, as OD9's answer reads; the specification's examples and the fixtures move their inline modules into project examples when exit 2q migrates the corpus. Austral has one module per file pair and no inline modules Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): no inline `mod m { }`: one file per module | the current compiler accepts inline submodules; MOD-7 (pending, R-0556) records OD9 without settling this; OPEN-33 | R-0556; the specification's examples and the fixture corpus | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): No inline `mod`; one file per module |
| OD34 | <a id="od34"></a>A struct literal inside a call's argument list, an index or an array literal within an `if`, `while` or `match` head (added 2026-10-29, while recording OD16): whether it counts as parenthesized under SYN-4. Two readings: only a grouped expression `( expr )` parenthesizes, so `if same(p, P { v: 1 }) { ... }` is refused; or any enclosing bracket does, so it is accepted (Rust's reading). Recommendation: any enclosing bracket, since inside a call's parentheses the head's `{` cannot be meant, and refusing it would require parentheses inside parentheses Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): only an enclosing `(` or `[` parenthesizes, stated as one invariant: in a condition head, a `{` at bracket depth zero opens the block | not in the record; the current compiler accepts it (the uppercase heuristic reads a struct literal anywhere in a head) | bug 318's repair (SYN-4); V1c | decided 2026-11-01 by the owner (final set): Only enclosing `(`/`[` parenthesize; `{` at depth 0 opens the block |
| OD35 | <a id="od35"></a>How an array satisfies a `Destroy` bound (added 2026-10-29, while recording OD28, which allows either form): one impl written in std for `[T; N]` with `T: Destroy`, or one language impl that is its own specification entry. Recommendation: the language impl, since an array's destruction is its elements' in order and std cannot write an impl generic over the length without const generics Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): a language impl `[T; N]: Destroy` when `T: Destroy`, elements destroyed in index order, with its own identity in coherence | the current compiler counts an array of destroyable elements as `Destroy` with no impl (bug 324) | bug 324's repair (TRAIT-10) | decided 2026-11-01 by the owner (final set): Language impl `[T; N]: Destroy` when `T: Destroy`, index order |
| OD36 | <a id="od36"></a>The spelling of the explicit non-consuming place form that opens a `Heap` or `HeapArray` (added 2026-10-29, while recording OD30, which requires one and keeps `*` as the consuming move-out). Options: a borrow through the cell (`(&h).f`, `(&mut h).f`), a named projection (`h.open().f`), or a dedicated operator. Recommendation: the borrow form, which needs no new syntax and matches OD32's explicit receiver borrow Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): only through a borrow block (`borrow h as r { r.f }`), no `(&h).f`; specify the result type for `Heap<T>`, `&Heap<T>` and `&mut Heap<T>`, one heap layer per block (MEM-4), and `HeapArray` elements | the current compiler opens one heap layer implicitly (`h.f` on `Heap<S>`, bug 326) | bug 326's repair (MEM-4) | decided 2026-11-01 by the owner (final set): Only through a borrow block; no `(&h).f` |
| OD37 | <a id="od37"></a>The names of the pointer difference and ordering functions (added 2026-10-29, while recording OD26). Options: `ptr::diff(p, q) -> Int` and `ptr::cmp(p, q) -> Ordering` beside `ptr::offset`; or no ordering at all, only `ptr::addr(p)` (via `ptr::expose`) compared as integers. Recommendation: `ptr::diff` and `ptr::cmp`, both trusted, each with its provenance premise (same allocation) stated as an obligation; Austral has no named pointer-difference or ordering functions, and its `Pointer` and `Address` types are compared with the ordering operators Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): one trusted `ptr::diff` (same-allocation premise), ordering read from its sign, no `ptr::cmp`; specify the representability of its result, one-past pointers included (Q8 is marked agreed in the picks; ROADMAP records no owner answer for it) | trusted code compares raw pointers with `==` and `<` today (bug 322) | bug 322's repair (TYPE-6, TRUST-1) | decided 2026-11-01 by the owner (final set): One trusted `ptr::diff`, ordering from its sign, no `ptr::cmp` |
| OD38 | <a id="od38"></a>The root resource API (added 2026-10-29, while recording OD8). Options: `fn main(root: Root) -> u8` with trusted splitting functions (`root.split_alloc()`, `root.split_console()`, ...) returning linear tokens; or a fixed set of token parameters `main` may declare (`fn main(alloc: Alloc, io: Console)`). Recommendation: the single `Root` value, as Austral's `RootCapability`, split by trusted functions the wrapper gate lists Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): `fn main(root: Root) -> u8` (or Unit), `Root` split into linear tokens by trusted functions; specify the split functions and how leftover authority is consumed | `main` takes no parameters today (STMT-7) | R-0557 (TRUST-9) and STMT-7 | decided 2026-11-01 by the owner (final set): Superseded by the authority decision of 2026-11-01: there is no `Root` type; `main`'s parameters are the target's singletons, hosted `fn main() with(...) -> u8` and freestanding `fn main(uart: Uart, arena: Arena) with(Device, Alloc) -> u8`; revised 2026-11-02: `main` receives the target's grants and returns `u8` only ([decisions](#decided-2026-11-02-authority)) |
| OD39 | <a id="od39"></a>The interface artifact format (added 2026-10-29, while recording OD9). Options: a generated `.coni` text file per module beside the build output; a JSON interface in the build directory; or the `pub` declarations rendered as Concrete source. Recommendation: rendered Concrete source (declarations without bodies), deterministic and reviewable as a diff Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): the `pub` declarations rendered as Concrete source without bodies, generated by the compiler, with an API-diff gate; it also carries private layout, impl methods, attributes, constants, capabilities and generic bodies, and is not read as FFI-6 bodyless foreign bindings | the compiler has the declaration-level `FileSummary` and `--report interface`, but no persisted per-module interface artifact and no API-diff gate of the proposed form | R-0556 (MOD-7) | decided 2026-11-01 by the owner (final set): `pub` decls rendered as Concrete source, compiler-generated, API-diff gate |
| OD40 | <a id="od40"></a>PAT-9 and the fallback arm of a desugared `if let`, `let ... else` or `while let` (added 2026-10-29, while fixing bug 317). Options: the fallback arm is not a written arm and is not judged; or it is judged, so `if let` over a one-variant enum is refused as unreachable. Recommendation: not judged, since the source has no arm to delete Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): moot if OD52 removes the desugared forms | `if let` over a one-variant enum is accepted | bug 317's repair (PAT-9) | decided 2026-11-01 by the owner (final set): Moot: OD52 removed `if let`/`while let`/`let...else` |
| OD41 | <a id="od41"></a>PAT-9 and integer ranges that span their type (added 2026-10-29, while fixing bug 317). Options: a catch-all after ranges covering the whole integer type is not refused (PAT-2 still requires it); or it is refused and PAT-2 is relaxed to accept ranges that cover the type. Recommendation: not refused, keeping PAT-2 as decided Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): close as range-specific under OD56, recording in PAT-2 and PAT-9 that an integer match still needs a catch-all and that listing every literal is not coverage | such a match is accepted | bug 317's repair (PAT-9, PAT-2) | decided 2026-11-01 by the owner (final set): Close as range-specific; integer matches keep a required catch-all, listing every literal is not coverage |
| OD42 | <a id="od42"></a>Comparisons on `bool` and `char` under OD26 (added 2026-10-29, while recording OD26, whose answer says "arithmetic, shifts and comparisons take numbers only"). Two readings: numbers only, so `b == true` and `c < 'z'` are refused (a `bool` is tested directly, a `char` converted by a named function); or "numbers" excludes pointers only, and `bool` and `char` keep equality (and `char` ordering). Recommendation: keep `==` and `!=` on `bool` and `char` and ordering on `char`, and refuse only pointers, which is what OD26's alignment with TRUST-1 is about Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): keep `==` and `!=` on `bool` and `char` and ordering on `char`, refuse ordering on `bool`; amend TYPE-6 and OD26 explicitly | the current compiler compares `bool` and `char` | bug 322's repair (TYPE-6) | decided 2026-11-01 by the owner (final set): Keep `==`/`!=` on bool/char and `<` on char; refuse ordering on bool |
| OD43 | <a id="od43"></a>How a type position resolves a name (added 2026-10-29, from the spec gap bug 229's ruling found). Options: (a) the current compiler's rules: type positions search the type namespace only, a user `enum Option` is always the builtin, and a parent sees its child's newtypes; (b) a user declaration shadows a builtin of the same name, with a warning; (c) a user type may not take a builtin type's name (refused). Recommendation: (c), which removes the shadowing question entirely; Austral reserves its builtin type names the same way Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): (c) refused, for builtin-name collisions only, with the complete reserved set published (also for aliases and import renames); this answer would not retire OD43, whose type-versus-value lookup and child-module questions stay open in it. A user `struct Copy HeapArray` is treated as linear today | the current compiler follows (a) | MOD-1; bug 229's record | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refuse builtin-name collisions, publish reserved set. Completed 2026-11-02 by the owner: one namespace per module, and a child module's declarations only through `child::T` or an import ([decisions](#decided-2026-11-02-namespace)) |
| OD44 | <a id="od44"></a>Two packages of one program that define the same top-level module name (added 2026-10-29, from bug 074's ruling). Options: (a) refused when the project loads, naming both packages (the current compiler); (b) module paths are qualified by their package, so both coexist. Recommendation: (a) until packages depend on packages (R-0440), then revisit with (b) Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): (a) refused at load, naming both packages, until packages depend on packages (R-0440) | the current compiler refuses (bug 074) | MOD-1, MOD-4; R-0440 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refuse at load, naming both, until R-0440 |
| OD45 | <a id="od45"></a>Capability-variable inference (added 2026-10-29, from bug 063's ruling). Main today binds a callee's `cap C` variable from the capability set of an argument's function type when its type peek (`PeekType`) can compute that type without checking the argument, and refuses a variable no argument determines (E0242, `pick_generic::<i64>()`). Options: (a) specify the current compiler's peek-based rule, listing the argument forms it covers; (b) bind from the checked type of every argument, whatever its form; (c) require an explicit capability argument whenever an argument is not a name. Recommendation: (b), so whether a call infers does not depend on the argument's spelling Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): (b) bind from the checked type of every argument; refuse unresolved or inconsistent ones; no result-context inference | the current compiler is (a) without the list | TRUST-4, TYPE-1; bug 063's record | decided 2026-11-01 by the owner (final set): Bind from checked type of every argument; no result-context. **Revised by the owner 2026-11-05 (row Q5.1/Q5.4, [revision](#decided-2026-11-05-revisions)): capability arguments are written explicitly at every polymorphic call and checked; no capability inference** |
| OD46 | <a id="od46"></a>Generic type-argument inference (added 2026-10-29, from bug 308's ruling). Main today binds each type parameter from the types its peek (`PeekType`) computes for the arguments given for it, a value `if` or `match` included since bug 308, but answers `.placeholder` for a generic call's result and some arms with binders, and refuses a parameter no argument determines; it never infers from the expected result type. Options: (a) specify the current compiler's peek-based rule, listing the forms it covers; (b) bind from the checked type of every argument, whatever its form; (c) also infer from the expected type at the call site. Recommendation: (b), local and predictable, with explicit type arguments for the rest; Austral infers type parameters from the arguments, and a `let` declaration's type can disambiguate a return-type-polymorphic call Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): (b) from checked argument types only, never from the expected result; arguments that need an expected type are refused, not defaulted | the current compiler is (a) without the list | TYPE-13, TYPE-15; bug 308's record | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): From checked argument types only |
| OD47 | <a id="od47"></a>Loops: `while` and a counted range loop (added 2026-10-29) | the current compiler has `while` and the C-style `for (init; cond; step)`, `for (cond)` and `for { }`; bug 331 | exit 2q; R-0499; V1a's loops and exits | decided 2026-10-29 by the owner: (1) the C-style `for` is removed in every form; (2) loops are `while cond { ... }` and `for i from a until b { ... }`: half-open (`a`, ..., `b - 1`, empty when `a >= b`), the index immutable in the body, `a` and `b` evaluated once before the first iteration, the index of the bounds' integer type (both bounds of one type), `break` and `continue` as usual (`continue` advances to the next index), `from` and `until` reserved (whether reserved or contextual was reopened by the reviews of that day: OD50), LL(1) as `for` IDENT `from` expr `until` expr block with the restricted head expression for `b` (OD16); (3) in Core the counted loop desugars to the one `while` form, carrying its termination measure `b - i`. Now STMT-6@3, STMT-11@2 and STMT-12 (pending, bug 331) |
| OD48 | <a id="od48"></a>The index type of a counted loop over two literals (added 2026-10-29, while recording OD47). Options: (a) refused, as OD31 refuses any literal with no expected type (write `for i from 0 until n` with a typed bound, or annotate); (b) the index is `Int` there. Recommendation: (a), consistent with OD31 Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): (a) refused, as OD31 refuses any literal with no expected type; a typed bound supplies the other bound's type, in either order | the counted loop is not in the current compiler | bug 331's repair (STMT-6) | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refused, as OD31 |
| OD49 | <a id="od49"></a>The arithmetic of a counted loop's termination measure (added 2026-10-29, while recording OD47). Options: (a) the measure is a mathematical integer in the proof only, never computed at run time; (b) it is computed in the unsigned type of the bounds' width; (c) a counted loop whose `b - a` cannot be represented is refused. Recommendation: (a): the loop compares `i < b` and increments `i`, which cannot overflow below `b`, so the measure need not exist at run time Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): (a) a mathematical integer in the proof only, never computed at run time | the counted loop is not in the current compiler | bug 331's repair; the contracts chapter's loop variant | decided 2026-11-01 by the owner (final set): Mathematical integer in the proof only |
| OD50 | <a id="od50"></a>Reserved or contextual `from` and `until` (added 2026-10-29, from the reviews of OD47). Options: (a) reserved words everywhere, as OD47 first said: `let from: Int` is refused and the lexer has two more keywords; (b) contextual keywords, meaningful only between `for IDENT` and the block, so existing names stay legal. Recommendation: (a), one lexical rule and no context-dependent tokens, at the cost of two names; Austral reserves its loop words Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): (a) reserved everywhere | the current compiler has neither word (bug 331) | bug 331's repair; SYN-1 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Reserved everywhere |
| OD51 | <a id="od51"></a>Reverse, stepped and collection iteration (added 2026-10-29, from the reviews of OD47). Options: (a) none: the counted loop is ascending by one, everything else is a `while`; (b) add a descending form (`for i from a down until b`) and a step (`by k`) later, each its own decision; (c) add iteration over a collection. Recommendation: (a) now, revisiting (b) when a workload needs it; a `while` keeps the proof story at one loop construct Reviewed pick (2026-10-30, a recommendation that supersedes the recommendation above where they differ): (a) none now; each a later decision when a workload needs it | the current compiler has no counted loop (bug 331) | STMT-6 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): None now |
| OD52 | <a id="od52"></a>`if let`, `while let` and `let ... else` (added 2026-10-30, item S1 of the owner picks of that day). Question: Whether `if let`, `while let` and `let ... else` stay in the language or are removed in favour of `match`. Options: (a) keep the three forms (PAT-6, their desugaring to `match`); (b) remove them, `match` being the only refutable pattern form. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove. Austral: none of the three; `case` is the only refutable form | all three are accepted and desugar to a `match` with a catch-all arm (PAT-6) | PAT-6, PAT-8, PAT-9, OPEN-40, SYN-1 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Remove |
| OD53 | <a id="od53"></a>Struct update `..base` (added 2026-10-30, item S2 of the owner picks of that day). Question: Whether the struct update form `S { a: 1, ..base }` stays in the language. Options: (a) keep it; (b) remove it, every field written. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove. Austral: no struct update; every field is written | accepted (TYPE-7, OWN-15's value flow for `..base`) | TYPE-7, OWN-15 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Remove |
| OD54 | <a id="od54"></a>`newtype` and `.0` (added 2026-10-30, item S3 of the owner picks of that day). Question: Whether `newtype` declarations and the `.0` projection stay, or a one-field struct with a private field replaces them. Options: (a) keep `newtype` and `.0`; (b) remove both, using a one-field struct whose field is private to its module. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove. Austral: no newtype; a type is made opaque by the module interface hiding its definition (a record being one possible definition) | `newtype` declarations and `.0` are accepted; `unwrap(id)` is an intrinsic | TYPE-7, TYPE-11, ARITH-5 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Remove |
| OD55 | <a id="od55"></a>Match guards (added 2026-10-30, item S10 of the owner picks of that day). Question: Whether match guards (`pattern if cond =>`) stay in the language. Options: (a) keep them (PAT-4); (b) remove them, the condition written inside the arm. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove. Austral: no guards | guards are accepted and read-only (PAT-4) | PAT-4, PAT-2, PAT-9 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Remove |
| OD56 | <a id="od56"></a>Range patterns (added 2026-10-30, item S11 of the owner picks of that day). Question: Whether integer range patterns (`1..=9 =>`) stay in the language (this amends OD19). Options: (a) keep them (PAT-7); (b) remove them, the comparison written in the arm or an `if` chain. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) remove; then close OPEN-41 as range-specific, recording in PAT-2 and PAT-9 that an integer match still needs a catch-all and that listing every literal is not coverage, so that catch-all is never unreachable. Austral: no range patterns | range patterns are accepted (PAT-7) | PAT-7, PAT-2, PAT-9, OPEN-41, OD19 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Remove; close OD41 as range-specific |
| OD57 | <a id="od57"></a>The region name of a borrow block (added 2026-10-30, item S12b of the owner picks of that day). Question: Whether a borrow block keeps its region name (`borrow x as r in R { }`) or drops `in R`. Options: (a) keep `in R`; (b) drop it, the block itself being the region. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) drop `in R`. Austral: the borrow statement names its region (`borrow x as r in R do ... end`); anonymous borrows `&x` in call arguments name none | the region name is written and has no other use in the language | REF-8, REF-11, SCOPE-D1 | decided 2026-11-01 by the owner (final set): Drop `in R` |
| OD58 | <a id="od58"></a>Borrow blocks stay (added 2026-10-30, item S12 of the owner picks of that day). Question: Whether borrow blocks (`borrow x as r in R { }`, as written today) stay the way to hold a reference across statements. Options: (a) keep them; (b) remove them, references living only within one call's arguments. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) keep (recorded as already agreed in the picks; no owner record yet). Austral: the borrow statement is the way to hold a reference across statements | borrow blocks are accepted (REF-8, REF-11) | REF-6, REF-8, REF-11, OPEN-36 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Keep |
| OD59 | <a id="od59"></a>One way to consume a `Heap` (added 2026-10-30, item S7 of the owner picks of that day). Question: Which operations consume a `Heap<T>`: the move-out `*h` (OD30), `destroy(h)`, `free(h)` or a new `take(h)`. Options: (a) keep `*h` and `free(h)` as today; (b) `*h` as move-out-and-free and `destroy(h)` extended to a Heap, `free` deleted, no `take`; (c) `take(h)` replaces `*h` as the move-out, `destroy(h)` extended to a Heap, `free` deleted. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b): `*h` and a `destroy(h)` extended to a Heap, no `free`, no `take` (a `take` would be another spelling of `*h`). Austral: not checked | `*h` moves out and frees and `free(h)` frees the cell and returns its value (MEM-5); `destroy(h)` on a Heap is refused (E0272: destroy() requires a named type) | MEM-5, OWN-13, OPEN-36 | decided 2026-11-01 by the owner (final set): `*h` + `destroy(h)`, no `free`, no `take` |
| OD60 | <a id="od60"></a>One consuming receiver form for `Destroy` (added 2026-10-30, item S8 of the owner picks of that day). Question: Whether `Destroy` is implemented through one consuming receiver form only. Options: (a) keep the receiver forms the current compiler accepts; (b) one consuming form (`fn destroy(self)`) only. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b) one consuming form. Austral: no destructor trait; a linear type's module exports an ordinary consuming function | a `Destroy` impl may take `self` or `&self`; `&mut self` is refused (E0587) | OWN-13, TRAIT-9, TRAIT-10 | decided 2026-11-01 by the owner (final set): One consuming form |
| OD61 | <a id="od61"></a>One coordinated removal list (added 2026-10-30, item review new 7 of the owner picks of that day). Question: Whether the decided and proposed removals (inline `mod`, the C-style `for`, `if let` / `while let` / `let ... else`, `newtype`, `..base`, range patterns, guards, `in R`, `break <expr>` and the `trusted mod` marker) are migrated as one coordinated list. Options: (a) one list, migrated in exit 2q's single transition; (b) each removal on its own schedule. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) one list, including dropping TRUST-6's `trusted mod` marker, which S14 removes (S14 is marked agreed in the picks; ROADMAP records no owner answer for it). Austral: not applicable | TRUST-6 (pending) still requires the `trusted mod` marker, which S14's pick would drop (no owner answer is recorded for S14) | TRUST-6, STMT-6, PAT-6, PAT-7, PAT-4, TYPE-7, MOD-4, SYN-1 | decided 2026-11-01 by the owner (final set): One list (incl. dropping `trusted mod`) |
| OD62 | <a id="od62"></a>Unused values and variables (added 2026-10-30, item Q3 of the owner picks of that day). Question: Whether an unused local variable or parameter is refused (Go's rule) and whether an unused `Result` or `Option` may be dropped by `discard`. Options: (a) refuse only an unused `Result` or `Option` (OWN-6); (b) Go's rule: any unused local or parameter is refused (an intentionally unused parameter is named `_`), and `discard` refuses `Result` and `Option`. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (b), stated as one invariant: every name a body binds is used or is `_` (a bodyless declaration has no body, so needs no exception); `discard` and wildcard drops refuse `Result` and `Option`, Copy or not; to clarify: `_` as a loop index, and an unused `&mut` binding under REF-10. Austral: not checked | unused Copy locals and parameters are accepted; OWN-6 refuses an unused fallible result | OWN-5, OWN-6, OWN-7, REF-10 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Every bound name used or `_`; `discard` refuses Result/Option |
| OD63 | <a id="od63"></a>Float literals take their expected type (added 2026-10-30, item Q4 of the owner picks of that day). Question: Whether a float literal takes the float type its position expects and is refused where nothing is expected, as ARITH-2@2 states under OD31's note that float literals get an explicit rule under the same principle. Options: (a) yes, as ARITH-2@2 states for integers and floats alike; (b) a float literal defaults to `f64`. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a). Austral: not checked | `let x = 1.5;` is accepted as `Float64` (ARITH-2@2 is partial, bug 327) | ARITH-2, ARITH-7 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Expected type, refused where none |
| OD64 | <a id="od64"></a>Raw pointers derived from a borrow (added 2026-10-30, item G19 of the owner picks of that day). Question: What a raw pointer derived from a borrow may do: whether it is valid only while the borrow lives, may escape the borrow block, may be passed to C, and how extern parameters say so. Options: (a) valid only while the borrow lives and never escaping it; (b) the operational loan rules of REF-14 only. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a): no returning, no escaping storage (a local `let` inside the block stays allowed), no converting to an integer and no capturing in an outer `defer`; borrow-derived pointers go only to call-only `extern` parameters; C uses it only during a call (memory C keeps must come from an allocation with its own owner); `extern` parameters say call-only (the default) or kept; field pointers come from the same borrow of the whole owner; deferred calls in the block run before the borrow ends; provenance survives copies, offsets, field pointers, helpers and indirect calls; a shared borrow never yields write access; `ptr::expose` is refused on such a pointer; the annotation's syntax gets an LL(1) check. Austral: not checked | no pass assumes `&mut` exclusivity (OD15); REF-14 bounds aliases by their loan | REF-14, TRUST-1, TRUST-3, FFI-3, OWN-12 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Valid only inside the borrow; call-only extern params |
| OD65 | <a id="od65"></a>Empty structs (added 2026-10-30, item review new 1 of the owner picks of that day). Question: Whether a struct with no fields (`struct S {}`) is allowed, as a unit-like type for capability and root tokens, or refused. Options: (a) allowed; (b) refused. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Austral: not checked | the parser accepts `struct S {}` and `S {}`; the grammar requires at least one field | TYPE-7, TYPE-16, TRUST-9, OPEN-38 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Allow |
| OD66 | <a id="od66"></a>The unit literal `()` (added 2026-10-30, item review new 2 of the owner picks of that day). Question: Whether `()` is the unit value's literal, given that the grammar has no production for it. Options: (a) add `()` to the grammar; (b) no unit literal, Unit produced only by a statement or a Unit call. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Austral: the unit value is written `nil` | the grammar has no production for `()` | SYN-1, TYPE-4 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Add `()` |
| OD67 | <a id="od67"></a>Locals that reuse a top-level name (added 2026-10-30, item review new 4 (SCOPE-4) of the owner picks of that day). Question: Whether a local binding may reuse the name of a top-level function, an import alias or an intrinsic (MEMORY_SEMANTICS §15.4 also lists constants). Options: (a) allowed, as today; (b) refused, as SCOPE-1 refuses shadowing a local. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Austral: not checked | allowed | SCOPE-1, SCOPE-2, MOD-1 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refuse |
| OD68 | <a id="od68"></a>Failure inside a deferred call (added 2026-10-30, item review new 5 of the owner picks of that day). Question: What OWN-12's text says about a failure inside a deferred call, given that an abort runs no deferred call (STMT-1, STMT-2). Options: (a) OWN-12 states that a trap inside a deferred call ends the program and runs no further deferred call, as STMT-2 and OD10 say for every abort; (b) the remaining deferred calls still run after a trapping one, which would amend STMT-2 and OD10. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Austral: not checked | failure is abort only (STMT-2); OWN-12's rationale leaves the case open | OWN-12, STMT-1, STMT-2, STMT-11 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Trap ends the program, no further deferred call |
| OD69 | <a id="od69"></a>What "the remaining locals are destroyed" means (added 2026-10-30, item review new 6 of the owner picks of that day). Question: Whether STMT-11's last exit step ("the remaining locals are destroyed") means storage teardown only, with no synthesized `Destroy` call, as OWN-13 requires. Options: (a) storage teardown only, no call, as OWN-13 requires; (b) a remaining local whose type implements `Destroy` is destroyed by a synthesized call, which would amend OWN-13. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (raised by the review). Austral: no implicit destruction | nothing is destroyed implicitly (OWN-13) | STMT-11, OWN-13 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Storage teardown only |
| OD70 | <a id="od70"></a>Struct fields with no comma (added 2026-10-30, item P2 of the owner picks of that day). Question: Whether struct fields written without a separating comma (`struct S { a: i32 b: i32 }`) are refused. Options: (a) refused, the grammar wins; (b) the grammar adds the form. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse. Austral: separators are required | the parser accepts it | SYN-1 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refuse |
| OD71 | <a id="od71"></a>Enum variants with no comma (added 2026-10-30, item P3 of the owner picks of that day). Question: Whether enum variants written without a separating comma are refused. Options: (a) refused; (b) the grammar adds the form. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse. Austral: separators are required | the parser accepts it | SYN-1 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refuse |
| OD72 | <a id="od72"></a>An enum with no variants (added 2026-10-30, item P4 of the owner picks of that day). Question: Whether an empty enum `enum E { }` is refused or is the uninhabited type. Options: (a) refused; (b) the grammar adds it as the uninhabited type. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse until a workload needs an uninhabited type. Austral: not checked | the parser accepts it | SYN-1, PAT-2 | decided 2026-11-01 by the owner (final set): Refuse until a workload needs it |
| OD73 | <a id="od73"></a>Reserved words used as identifiers (added 2026-10-30, item P5 of the owner picks of that day). Question: Whether `cap` and `type`, which the lexer reserves, are refused as identifiers where the parser lets them through. Options: (a) refused, reserved words stay reserved; (b) the grammar lets them through there. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse. Austral: keywords are reserved | `expectIdent` accepts `cap` (and `type`) as a variant name | SYN-1 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refuse |
| OD74 | <a id="od74"></a>A raw pointer type with no qualifier (added 2026-10-30, item P6 of the owner picks of that day). Question: Whether a bare `*T` is refused or read as `*mut T`. Options: (a) refused, `*const` or `*mut` required; (b) the grammar adds `*T` as `*mut T`. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse; the qualifier is the safety-relevant part. Austral: not applicable | the parser reads `*T` as `*mut T` | SYN-1, TRUST-1 | decided 2026-11-01 by the owner (final set): Refuse; qualifier required |
| OD75 | <a id="od75"></a>An empty type parameter list (added 2026-10-30, item P7 of the owner picks of that day). Question: Whether `impl<> S` (an empty type parameter list) is refused. Options: (a) refused; (b) the grammar adds it. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse. Austral: not applicable | the parser accepts it | SYN-1 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refuse |
| OD76 | <a id="od76"></a>Unqualified struct patterns in `match` (added 2026-10-30, item G1-b of the owner picks of that day). Question: Whether an unqualified struct pattern is allowed in `match` (`match s { S { a } => ... }`), given that only `let` destructures structs today (the `if let` form follows OD52). Options: (a) refused in `match`, PAT-1's wording fixed to say so; (b) allowed. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) refuse in `match`; fix PAT-1's wording. Austral: not checked | the parser refuses it in `match` and `if let` and accepts it in `let` | PAT-1, PAT-5, SYN-1 | decided 2026-11-01 by the owner (final set): Refuse in `match`; fix PAT-1 |
| OD77 | <a id="od77"></a>Resync the grammar to the decided language (added 2026-10-30, item B of the owner picks of that day). Question: Whether `grammar/concrete.ebnf` is resynchronized for the items the language already decided: `E#V` becomes `E::V`, binding renaming, a function body with no trailing value and `;` for bodyless functions, no generic trait methods, assignment targets are places, unknown attribute names refused, and no `break <expr>`. Options: (a) approve the resync, as actual productions with a factored `place` prefix for assignment; (b) defer it, the divergences staying recorded in grammar/conformance_known.tsv (the decided language does not change either way). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) approve, with actual productions, a `place` prefix and `break <expr>` removed. Austral: not applicable | the grammar is stale on these items (grammar/conformance_known.tsv) | SYN-1, PAT-1, STMT-6, STMT-8, OWN-8, FFI-6 | decided 2026-11-01 by the owner (final set): Approve resync with actual productions |
| OD78 | <a id="od78"></a>`mod X;` in the grammar (added 2026-10-30, item C1 (review new 3) of the owner picks of that day). Question: Whether the grammar gains `mod name;` (a module in its own file), which the parser accepts and OD33's pick would make the only module form. Options: (a) add it to the grammar; (b) keep the recorded divergence for now. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) add, item by item. Austral: each module is its own pair of files | the parser accepts it; the grammar lacks it | MOD-4, OPEN-33, SYN-1 | decided 2026-11-01 by the owner (final set): Add `mod X;` item by item |
| OD79 | <a id="od79"></a>`assert` in the grammar (added 2026-10-30, item C2 of the owner picks of that day). Question: Whether the grammar gains the proof-annotation statement `assert expr;`. Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Austral: not checked | the parser accepts it | CON-2, CON-3, SYN-1 | decided 2026-11-01 by the owner (final set): Add `assert` |
| OD80 | <a id="od80"></a>`assume` in the grammar (added 2026-10-30, item C3 of the owner picks of that day). Question: Whether the grammar gains the proof-annotation statement `assume expr;`. Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Austral: not checked | the parser accepts it | CON-2, CON-3, SYN-1 | decided 2026-11-01 by the owner (final set): Keep `assume`, tracked: every claim depending on it is marked assumed |
| OD81 | <a id="od81"></a>`ghost let` in the grammar (added 2026-10-30, item C4 of the owner picks of that day). Question: Whether the grammar gains the proof-only binding `ghost let` (with `ghost` a contextual keyword). Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Austral: not applicable | the parser accepts it | CON-4, SYN-1 | decided 2026-11-01 by the owner (final set): Add, with `ghost` reserved |
| OD82 | <a id="od82"></a>Loop contract attributes and where attributes attach (added 2026-10-30, item C5 (R7) of the owner picks of that day). Question: Whether the grammar gains `#[invariant(e)]` and `#[variant(e)]` before a loop, and whether attributes attach by kind (declaration attributes to the next declaration, loop attributes to the next loop). Options: (a) add them to the grammar and attach attributes by kind; (b) remove them from the parser; (c) keep the recorded divergence for now. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a): attach by kind, declaration attributes to the next declaration, `#[invariant]` and `#[variant]` to the next loop. Austral: not checked | the parser accepts them | CON-2, STMT-12, SYN-1 | decided 2026-11-01 by the owner (final set): Add; attributes attach by kind |
| OD83 | <a id="od83"></a>The call-site allocator binding in the grammar (added 2026-10-30, item C6 of the owner picks of that day). Question: Whether the grammar gains the call-site allocator binding `f(...) with(Alloc = e)`. Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Austral: not checked | the parser accepts it | TRUST-4, MEM-5, SYN-1 | decided 2026-11-01 by the owner (final set): No call-site binding `f(...) with(Alloc = e)`. Under the authority decision, one allocator form per target: hosted, `with(Alloc)` and no allocator value; freestanding, an `Arena` parameter plus `with(Alloc)`. Revised 2026-11-02 by the owner's rule: one form on every target, an `Allocator<A>` value plus `with(Alloc)` ([decisions](#decided-2026-11-02-allocation)) |
| OD84 | <a id="od84"></a>Struct destructuring `let` in the grammar (added 2026-10-30, item C7 of the owner picks of that day). Question: Whether the grammar gains struct destructuring `let S { a } = e;`. Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): individual review required; no pick recorded. Austral: destructuring `let` of records exists (not checked) | the parser accepts it (PAT-5) | PAT-5, SYN-1 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Add struct-destructuring `let` |
| OD85 | <a id="od85"></a>Variant destructuring with `else` in the grammar (added 2026-10-30, item C8 of the owner picks of that day). Question: Whether the grammar gains `let E::V { a } = e else { ... };`, which OD52's pick would remove. Options: (a) add it to the grammar; (b) remove it from the parser, so the language loses the form; (c) keep the recorded divergence for now. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): follows OD52 (removed under its pick). Austral: no `let ... else` | the parser accepts it (PAT-8) | PAT-8, OPEN-52, SYN-1 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Remove (follows OD52) |
| OD86 | <a id="od86"></a>The body of an unbraced match arm (added 2026-10-30, item R1 of the owner picks of that day). Question: Whether the grammar narrows an unbraced match-arm body to an expression, a block or `return`, which is what the parser accepts (`continue;` as an unbraced body is refused). Options: (a) narrow the grammar; (b) widen the parser. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) narrow. Austral: not applicable | the parser accepts an expression, a block or `return` as an unbraced arm body and refuses other statements | SYN-1, PAT-3 | decided 2026-11-01 by the owner (final set): Narrow |
| OD87 | <a id="od87"></a>Capability arguments in an expression turbofish (added 2026-10-30, item R2/R3 of the owner picks of that day). Question: Whether capability arguments (`T + C`, `{}`) may be written in an expression or method turbofish. Options: (a) the grammar narrows to type arguments there until a workload needs them; (b) the parser adds them. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) narrow. Austral: not applicable | the parser refuses them; the grammar derives them | SYN-1, TRUST-4, OPEN-45 | decided 2026-11-01 by the owner (final set): Accept explicit capability arguments in a turbofish (inference fills omitted ones). **Revised by the owner 2026-11-05 (row Q5.1/Q5.4, [revision](#decided-2026-11-05-revisions)): explicit capability arguments are required; inference no longer fills omitted ones** |
| OD88 | <a id="od88"></a>The shape of capability arguments (added 2026-10-30, item R8 of the owner picks of that day). Question: Whether the grammar states the capability-argument shape rules the parser checks (a union of names only; type arguments before capability arguments). Options: (a) state them in the grammar; (b) leave them to the parser. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a). Austral: not applicable | the parser checks them; the grammar does not | SYN-1, TRUST-4 | decided 2026-11-01 by the owner (final set): State in grammar |
| OD89 | <a id="od89"></a>Capability parameters on a trait impl (added 2026-10-30, item R9 of the parser/grammar divergence report of 2026-10). Question: Whether a trait impl may declare capability parameters, which the grammar derives and the parser refuses. Options: (a) refused, the grammar narrowed; (b) allowed. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (in the divergence report, not the picks). Austral: not applicable | the parser refuses them (declare them on an inherent impl) | SYN-1, TRAIT-9, TRUST-4 | decided 2026-11-01 by the owner (final set): Refuse; declare on inherent impl |
| OD90 | <a id="od90"></a>`struct Copy <T>` and contextual keywords (added 2026-10-30, item Copy of the owner picks of that day). Question: Whether `Copy` stays a contextual keyword, with the LL(1) checkers taught to model it, or becomes a reserved word (the other unreserved words, `self`, `repr`, `packed` and `align`, stay as they are unless decided separately). Options: (a) reserve `Copy` as a keyword, refusing it as an identifier; (b) keep it contextual, the LL(1) checkers taught to model a quoted unreserved terminal as also IDENT, each conflict found listed for the owner (letters as in the divergence report). Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): (a) reserve `Copy`: one lexical rule, consistent with OD50 (reserved `from` and `until`) and OD43 (builtin names refused as user names). Austral: its keywords are reserved | contextual; the `struct Copy <T>` conflict is invisible to the checkers' token model | SYN-1, OWN-D1 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Reserve `Copy` |
| OD91 | <a id="od91"></a>A cast followed by `<` (added 2026-10-30, item LL(1) type\|5.2 of grammar/ll1_known_conflicts.tsv). Question: How `x as T < y` parses, given that `<` after a cast's type name may start type arguments or be the less-than operator. Options: (a) a cast's type takes no type arguments unless parenthesized (`x as (T<U>)`), so `<` is less-than; (b) the parser's reading: `<` always starts type arguments, so the comparison needs parentheses (`(x as T) < y`); (c) refuse `<` directly after a cast's type and require parentheses either way. Pick (the coordinator's pick after the Kimi, Codex and Grok review of 2026-10-30, a recommendation and not a decision): none given (in the LL(1) ledger, not the picks). Austral: a conversion is the cast expression `e : T`, which takes no type arguments after it, so the conflict does not arise | the parser always reads type arguments, so `x as T < y` is refused (grammar/ll1_known_conflicts.tsv, row type\|5.2) | SYN-1, SYN-2, ARITH-5 | decided 2026-11-01 by the owner (final set): `x as T < y` is less-than; a generic cast type needs parentheses `x as (Vec<T>)` |
| OD92 | <a id="od92"></a>A capability set inferred from callbacks (added 2026-10-30, raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES). Question: Whether a combinator may write its capability set as that of its callbacks (`with(f)`), the checker computing the union of the sets of the callbacks its body calls. Bears on TYPE-D1, TYPE-17, TRUST-4. The current compiler: no such form; `with(f)` reads `f` as a capability name (an unresolved one, which MOD-1 refuses and the current compiler accepts: bug 337), so a combinator names a capability parameter (`fn map<T, U, cap C>(.., f: fn(&T) with(C) -> U) with(C)`). The callable-values design (2026-06-12, [CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md) §6) recorded the shortcut as specified and not built, after [research/language/capability-polymorphism.md](../../research/language/capability-polymorphism.md); no roadmap item tracks it. Options: (a) no shortcut, combinators name capability parameters; (b) add it, as set propagation and not row unification. Austral: not applicable (capabilities are values). Pick: none given (raised by the absorption of 2026-10-30). | see OPEN-92 | see OPEN-92 | decided 2026-11-01 by the owner (final set): No shortcut; name a capability parameter |
| OD93 | <a id="od93"></a>Widening a function value's capability set (added 2026-10-30, raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES). Question: Whether a function value converts to a function type whose capability set contains its own, implicitly by subset or by an explicit conversion. Bears on TYPE-4, TYPE-17, TRUST-4. The current compiler: refused (E0220): `fn(Int) -> Int` is not a `fn(Int) with(Console) -> Int`, nor `with(File)` a `with(Std)`, and there is no explicit conversion; a capability-polymorphic parameter accepts either by instantiating its variable. The callable-values design (§10) says there is no implicit widening, and that "widening is explicit or via lattice subsumption only", with a lattice example (`Concurrent ⊇ Async`) whose names are not capabilities; HANDLE_CAPABILITIES R7 keeps handle types invariant, as TYPE-4 does. Options: (a) exact types, as in the current compiler; (b) subsumption into a larger set; (c) an explicit conversion. Austral: not applicable (capabilities are values). Pick: none given (raised by the absorption of 2026-10-30). | see OPEN-93 | see OPEN-93 | decided 2026-11-01 by the owner (final set): Exact types **Canonical current capability-set rule: authority rule 4; this earlier exact-types answer refers to it.** |
| OD94 | <a id="od94"></a>Names in `with(...)` that are not capabilities (added 2026-10-30, raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES). Question: Whether a name in `with(...)` that is not a capability, a capability alias or a capability variable in scope is refused. Bears on TRUST-D1, TYPE-D1, TRUST-4. The current compiler: accepted as a capability of its own: `fn main() with(Foo) -> Int` compiles and runs, and a call to a function that requires `Foo` is refused only in a caller that does not list it (E0520). The compiler's `validCaps`, TRUST-4's rationale and [SAFETY.md](../verification/SAFETY.md) name nine capabilities. Options: (a) refuse an unknown name; (b) an open vocabulary. Austral: not applicable (capabilities are values). Pick: none given (raised by the absorption of 2026-10-30). | [MOD-1](#MOD-1) refuses the name; the current compiler accepts it (bug 337) | MOD-1@1 | withdrawn 2026-10-30: not an owner question; MOD-1@1 refuses the name (bug 337) |
| OD95 | <a id="od95"></a>What `Time`, `Env`, `Process` and `Alloc` mean (added 2026-10-30, raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES). Question: What authority each of `Time`, `Env`, `Process` and `Alloc` names. Bears on TRUST-D1, TRUST-4. The current compiler: no document states them; HANDLE_CAPABILITIES §2 defined `Console`, `File` and `Network`, and SAFETY.md `Random` (TRUST-D1). std requires `Time` for its clock and sleep functions, `Env` for environment variables and for `std.net`'s `bind` and `connect`, `Process` for exit, spawn, wait and kill, and `Alloc` for heap allocation. Options: state each as std uses it, or narrow or split it (std's `Env` on socket functions is the case to settle). Austral: not applicable (capabilities are values). Pick: none given (raised by the absorption of 2026-10-30). | see OPEN-95 | see OPEN-95 | decided 2026-11-01 by the owner (final set): State each; move socket `bind`/`connect` from `Env` to `Network` |
| OD96 | <a id="od96"></a>Two foreign bindings of one C symbol (added 2026-10-30, raised while absorbing CALLABLE_VALUES and HANDLE_CAPABILITIES). Question: Whether two foreign bindings of one C symbol (`#[link_name]`) must agree on its C signature. Bears on FFI-3, FFI-5. The current compiler: accepted: `abs_a(x: i32) with() -> i32` and `abs_b(x: i64) with() -> i64`, both bound to `abs`, compile and the binary runs, while the interpreter stops at run time on the mismatched width. HANDLE_CAPABILITIES §5 (encoding A, one binding per effect for one C symbol) planned to refuse a mismatch with a diagnostic. Options: (a) refuse; (b) allow, each binding's signature trusted on its own. Austral: not applicable. Pick: none given (raised by the absorption of 2026-10-30). | see OPEN-96 | see OPEN-96 | decided 2026-11-01 by the owner (final set): Refuse |
| OD97 | <a id="od97"></a>A build profile that omits or demands an overflow check (added 2026-10-30, raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL). Question: Whether a build profile may omit the run-time check of an integer operation that no proof discharges, or refuse such an operation at compile time. Bears on ARITH-3, ARITH-9. The current compiler: every profile emits every check, and no option turns one off; ARITH-3's rationale says a profile changes reports, never code generation, and OD10 decided that arithmetic is checked "by default". The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §3.1 instead let a release profile omit a check that is "explicitly configured off and reported", and a provable or high-integrity profile refuse an unproved overflow site. Options: (a) every profile keeps every check that no discharged proof removes; (b) a release profile may turn checks off when configured and reported, never wrapping; (c) as (a), and a profile may also refuse an operation whose no-overflow obligation is not discharged. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30). | see OPEN-97 | see OPEN-97 | decided 2026-11-01 by the owner (final set): Keep every check; a profile may refuse undischarged ops |
| OD98 | <a id="od98"></a>Pointer and reference conversions written with `as` (added 2026-10-30, raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL). Question: Which conversions between raw pointers and references `as` writes (`&x as *const T`, `p as &T`, `p as &mut T`, `p as *mut U`). Bears on TRUST-1, TRUST-3, REF-5, ARITH-5. The current compiler: `&x as *const T` is accepted in safe code, `p as &T` from a raw pointer is accepted in trusted or `Unsafe` code (the binary and `--interp` agree), and a reference cast between different pointee types (`a as &Int` with `a: &A`) is refused (E0553). No entry states these: TRUST-3 forms a reference from a raw pointer with `&*p`, and REF-5 lists borrowing and projection as the ways a reference is formed. The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §8 kept `as` "the spelling of pointer and reference casts (`as *const T`, `as *mut T`, `as &K`, `as &mut V`)", 281 lines of std. Options: (a) `as` keeps every one of them; (b) `as` converts only a reference to a raw pointer, and a reference is formed from a raw pointer only by `&*p` (TRUST-3); (c) pointer conversions become named functions. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30). | see OPEN-98 | see OPEN-98 | decided 2026-11-01 by the owner (final set): Reference to raw pointer conversion only in trusted/Unsafe code (equal to Austral) |
| OD99 | <a id="od99"></a>The names of the named conversions (added 2026-10-30, raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL). Question: The spellings of the named conversions of ARITH-5, ARITH-10 and ARITH-11 (`try_convert`, `truncate`, `reinterpret`, the integer-to-float and `f64`-to-`f32` conversions, and the float roundings `trunc`, `round`, `floor`, `ceil`). Bears on ARITH-5, ARITH-10, ARITH-11. The current compiler: none of them exists (E0101). ARITH-5's sentence names `truncate` and `reinterpret`, while the archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §8.2 calls its spellings indicative, to be fixed by the API's owner; it names the integer-to-float and `f64`-to-`f32` conversions by their rounding only. Options: (a) the spellings above, as builtins; (b) other names, or std functions rather than builtins. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30). | see OPEN-99 | see OPEN-99 | decided 2026-11-01 by the owner (final set): These spellings, as builtins |
| OD100 | <a id="od100"></a>Comparisons on a type parameter (added 2026-10-30, raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL). Question: Whether a comparison on a value of a `Copy`-bounded type parameter is accepted and checked again at each instantiation, or refused in the generic body. Bears on TYPE-6, TYPE-9, PASS-1, OPEN-42. The current compiler: `a < b` with `a, b: T` and `T: Copy` is accepted, and an instantiation at a type with no comparison is refused after monomorphization (E0229, `post-mono`); without the `Copy` bound it is refused in the body (E0229, `check`). TYPE-9's rationale says a generic body is checked once, from its bounds, which a per-instantiation check departs from. Options: (a) keep the current compiler's per-instantiation check; (b) refuse a comparison on a type parameter until a bound states it (a comparison trait); (c) refuse it. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md §8a (2026-10-30). | see OPEN-100 | see OPEN-100 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refuse until a bound states it |
| OD101 | <a id="od101"></a>What a trap writes (added 2026-10-30, raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL). Question: Whether a run-time trap writes a message naming the operation, its operands and its source location before the process ends, and in what form. Bears on STMT-2, STMT-7, ARITH-3. The current compiler: the binary writes nothing and ends by `SIGABRT`, and `--interp` reports a run-time error naming the trap (`arithmetic overflow (checked +)`). The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §9.2 described a message `concrete: arithmetic overflow in function 'f' at file:line` with the operation, the operands and the policy. Options: (a) the binary writes nothing; (b) it writes one line naming the trap and its location; (c) the §9.2 form. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30). | see OPEN-101 | see OPEN-101 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): One line naming the trap and location |
| OD102 | <a id="od102"></a>`Float32` and `Char` as type spellings (added 2026-10-30, raised while absorbing ARITHMETIC_POLICY and VALUE_MODEL). Question: Whether `Float32` and `Char` are other spellings of `f32` and `char`, as `Float64` and `Bool` are of `f64` and `bool`. Bears on TYPE-19, SYN-3. The current compiler: the parser maps `Float64` and `Bool` to `f64` and `bool` but not `Float32` or `Char`, so `fn g(x: f32) -> Float32 { return x; }` is refused (E0220) while the resolver's `tyFromName` maps all four. The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §1 names `Float32`/`f32` as two spellings. Options: (a) `Float32` and `Char` become spellings of `f32` and `char`; (b) neither, keeping the current compiler's `Float64` and `Bool`; (c) `Float32` and `Char` decided separately. Whether every capitalized spelling (`Int`, `Uint`, `Float64`, `Bool`) is removed is a separate removal question, not asked here. Austral: not checked. Raised by the absorption of ARITHMETIC_POLICY.md (2026-10-30). | see OPEN-102 | see OPEN-102 | decided 2026-11-01 by the owner (final set): No `Float32` or `Char` aliases. Completed 2026-11-02 by the owner: the aliases `Int`, `Uint`, `Float64` and `Bool` are removed too ([decisions](#decided-2026-11-02-aliases)) |
| OD103 | <a id="od103"></a>`as` between reference types (added 2026-10-30, raised while absorbing MEMORY_SEMANTICS). Question: Whether `as` may convert a reference type to a reference type, given that REF-5 lists borrowing and projecting as the only operations that form a reference. Bears on REF-5, REF-6, ARITH-5, TRUST-4. The current compiler: an identity cast between equal reference types is accepted and denotes the operand's place (`read(r as &Int)` with `r: &Int`, and `read(&v as &Int)`, exit 42 on both consumers); every other cast between reference types, `&mut Int as &Int` included, is E0553 at core-check under ARITH-5's tag, whose sentence covers only numeric, `bool` and `char` conversions; a cast from a reference to a raw pointer is allowed (TRUST-4). The design record (MEMORY_SEMANTICS §15.3) listed a cast of a traced reference binding (`r as &U`) among the reference-forming operations. Options: (a) refuse `as` between reference types; (b) allow the identity cast as a no-op on the operand's place. Conversions between a reference and a raw pointer are OPEN-98's, not this question's. Austral: not checked. Pick: none given (raised by the MEMORY_SEMANTICS absorption, 2026-10-30). | see OPEN-103 | see OPEN-103 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refuse |
| OD104 | <a id="od104"></a>A `#[variant]` written on a counted loop (added 2026-10-30, from the Codex and Kimi review of the OD47 loop rules). Question: whether a `#[variant]` attribute written on a counted loop is refused, checked like any loop variant, or ignored. Options: (a) refused, the loop's own measure being its only variant; (b) allowed and checked as a loop variant (CON-2); (c) allowed and ignored. No pick given | the counted loop is not in the current compiler (bug 331) | STMT-12, CON-2 | decided 2026-11-01 by the owner (final set): Refuse |
| OD105 | <a id="od105"></a>Where a counted loop's `#[invariant]` is checked (added 2026-10-30, from the same review). Question: where an `#[invariant]` on a counted loop is checked, given that CON-2 checks a loop invariant before the loop and after every iteration. Options: (a) as CON-2 says for every loop; (b) at each entry to the body, so nothing is checked when the loop runs no iteration. No pick given; (b) needs proof requirement 2 | the counted loop is not in the current compiler (bug 331) | STMT-12, CON-2 | decided 2026-11-01 by the owner (final set): As CON-2: before the loop and after every iteration |
| OD106 | <a id="od106"></a>The result type of a checked conversion (added 2026-10-30, from the Codex and Kimi review of the absorption). Question: Whether a checked conversion (`try_convert`) reports a value it cannot preserve as `Option::None` or as a `Result` error carrying the reason. Bears on ARITH-5, ARITH-10. The current compiler: no checked conversion exists (E0101, R-0500). The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §8.2 wrote `try_convert::<T>(x) -> Option<T>` "(or a `Result` with a reason)" and left the choice to the API's owner. Options: (a) `Option<T>`; (b) a `Result` whose error names the reason (out of range, not integral, NaN, infinite, not representable). Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30). | no checked conversion exists (E0101, R-0500) | ARITH-5, ARITH-10 | decided 2026-11-01 by the owner (final set): `Result<T, OutOfRange>` (failure, not absence) |
| OD107 | <a id="od107"></a>What a float-to-integer rounding gives for NaN, infinities and out-of-range values (added 2026-10-30, from the Codex and Kimi review of the absorption). Question: Whether each float-to-integer rounding operation (`trunc`, `round`, `floor`, `ceil`) gives a value or an error for NaN, an infinity or a result outside the target type's range, and which value. Bears on ARITH-1, ARITH-11. The current compiler: the rounding operations do not exist (R-0500); a float-to-integer `as` truncates toward zero and aborts on NaN, an infinity or a value out of range, natively and in `--interp`. The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) §8.2 required a stated result for every input, "either as a value or as an error". Options: (a) an error, as `try_convert` reports; (b) a value: the nearest value of the type for an out-of-range input or an infinity, and 0 for NaN; (c) a trap, as `as` does today. Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30). | the rounding operations do not exist; a float-to-integer `as` truncates toward zero and aborts on NaN, an infinity or a value out of range | ARITH-1, ARITH-11 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): An error, as `try_convert` |
| OD108 | <a id="od108"></a>A capability listed twice in a set (added 2026-10-30, from the Codex and Kimi review of the absorption). Question: Whether a capability set that lists one capability twice (`with(Console, Console)`) is the same set as the one that lists it once, is refused, or is a different set. Bears on TYPE-D1, TYPE-17, TYPE-4. The current compiler: accepted where it is written, and a different type: `fn(Int) with(Console) -> Int` is not a `fn(Int) with(Console, Console) -> Int`, in either direction (E0220), because `CapSet.normalize` sorts a set and does not remove a repeated name. The callable-values design ([CALLABLE_VALUES_AND_CAPABILITIES.md](../archive/design-records/CALLABLE_VALUES_AND_CAPABILITIES.md)) calls the capabilities of a function type a set. Options: (a) the same set; (b) refuse a repeated capability where it is written; (c) a different set, as in the current compiler. Austral: not applicable (capabilities are values). Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30). | accepted, and a different set: `with(Console, Console)` is not `with(Console)` (E0220) | TYPE-D1, TYPE-17, TYPE-4 | decided 2026-11-01 by the owner (final set): Refuse a repeated capability |
| OD109 | <a id="od109"></a>Specializations that differ only in a function type's capabilities (added 2026-10-30, from the Codex and Kimi review of the absorption). Question: Whether the symbol of a generic function's instantiation includes the capability set of a function-type argument, so that two instantiations that differ only in that set have distinct symbols. Bears on TYPE-17, TYPE-18. The current compiler: one: `pick<T: Copy>` instantiated at `fn(Int) -> Int` and at `fn(Int) with(Console) -> Int` emits the single function `pick_for_Fn_T_Int_to_Int_E`, because Mono's `tyToSuffix` drops a function type's capability set while it keeps a struct's capability arguments (E0809's history), and the program runs correctly since capabilities have no run-time representation (TYPE-18). The archived [HANDLE_CAPABILITIES.md](../archive/design-records/HANDLE_CAPABILITIES.md) R8 required collision-free symbol identity across instantiations and left code sharing to its own argument. Options: (a) the set is part of the symbol, as for struct capability arguments; (b) it is not, one symbol serving both, stated as erasure with its correctness argument. Whether two symbols share one body stays an implementation choice (TYPE-18). Austral: not applicable (capabilities are values). Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30). | one specialization: Mono's symbol drops a function type's capability set | TYPE-17, TYPE-18 | decided 2026-11-01 by the owner (final set): Set is part of the symbol |
| OD110 | <a id="od110"></a>Enums at the foreign boundary (added 2026-10-30, from the Codex and Kimi review of the absorption). Question: Whether a value of enum type may cross an `extern fn` boundary, and if so in what representation. Bears on FFI-3, FFI-7. The current compiler: refused: `extern fn f(x: E)` with `enum Copy E { A, B }` is E0574 (a non-FFI-safe parameter); only scalars, raw pointers and `repr(C)` structs cross. The design record [LAYOUT_CONTRACT.md](../archive/design-records/LAYOUT_CONTRACT.md) says enums must not cross (§3.2); before it was archived it also said they cross as integers. Options: (a) refused, as in the current compiler; (b) a fieldless enum crosses as an integer of a stated width, which needs a stated discriminant for each variant. Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30). | refused (E0574) | FFI-3, FFI-7 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refused |
| OD111 | <a id="od111"></a>Misaligned fields of a packed struct (added 2026-10-30, from the Codex and Kimi review of the absorption). Question: Whether a `#[repr(packed)]` struct may hold a field whose alignment exceeds the alignment of its offset, and whether such a field may be borrowed. Bears on FFI-4, FFI-8, REF-5. The current compiler: both accepted: `#[repr(packed)] struct P { v: u8, l: u32 }` places `l` at offset 1, and `&p.l` is accepted and reads the field correctly in the binary and in `--interp`. [LAYOUT_CONTRACT.md](../archive/design-records/LAYOUT_CONTRACT.md) §2 restricts a packed struct to fields whose alignment does not exceed their position, without saying what enforces it. Options: (a) refuse such a field; (b) accept it and refuse a reference to it; (c) accept both, as in the current compiler. Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30). | both accepted on both consumers | FFI-4, FFI-8, REF-5 | decided 2026-11-01 by the owner (final set): Accept field, refuse a reference to it |
| OD112 | <a id="od112"></a>Mutation through a shared reference (added 2026-10-30, from the Codex and Kimi review of the absorption). Question: Whether a value reached through a shared reference `&T` may be changed, by a write through a raw pointer derived from the reference or by a cell type. Bears on REF-10, OWN-10, TRUST-2, TRUST-5. The current compiler: safe code changes a value only through a `mut` binding or a `&mut` reference, and there is no cell type; a trusted function may convert `r: &Int` to `*mut Int` (`r as *const Int as *mut Int`) and store through it: the binary does the store, and `--interp` stops ("deref-assign through immutable ref"). The archived [MEMORY_SEMANTICS.md](../archive/design-records/MEMORY_SEMANTICS.md) §12 said there is no `Cell`, `RefCell` or `UnsafeCell` equivalent and that mutation flows from `let mut` through `&mut`; the [overview](#ch-overview) lists no interior mutability in safe code, with a later `UnsafeCell<T>` gated by `Unsafe`. Options: (a) never: a write through a pointer derived from a `&T` is undefined behaviour, a premise trusted code keeps (TRUST-2); (b) as (a), with a cell type gated by `Unsafe` as the one exception; (c) a trusted write through such a pointer is allowed. Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30). | safe code cannot; a trusted write through `r as *const T as *mut T` runs in the binary and stops `--interp` | REF-10, OWN-10, TRUST-2, TRUST-5 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Never; UB premise trusted code keeps |
| OD113 | <a id="od113"></a>The ownership exceptions of trusted code (added 2026-10-30, from the Codex and Kimi review of the absorption). Question: Whether trusted code keeps its two exceptions to the ownership rules, destroying a value through a reference and releasing with `vec_free` a vector whose elements are not Copy. Bears on TRUST-5, TRUST-12, OWN-5, OWN-8. The current compiler: both are accepted in trusted code only (elsewhere E0290 under OWN-8 and E0287 under OWN-5): std's collections destroy an element through `(p as &T)` in drop glue after its owner was consumed, and a trusted function may free a buffer it has emptied. The archived [MEMORY_SEMANTICS.md](../archive/design-records/MEMORY_SEMANTICS.md) §11 said that `trusted` does not relax linearity, and TRUST-5's rationale says the same. Options: (a) keep both, each a named trusted operation with its premise (the owner was consumed; the vector is empty); (b) remove both, drop glue moving the element out of raw memory instead (TRUST-7); (c) keep only destruction through a reference; (d) keep only `vec_free` of an emptied vector. Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30). | both accepted in trusted code only (E0290, E0287 elsewhere) | TRUST-5, TRUST-12, OWN-5, OWN-8 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Remove both |
| OD114 | <a id="od114"></a>How integer division rounds (added 2026-10-30, from the Codex and Kimi review of the absorption). Question: How an integer division that does not abort rounds its quotient, and which sign a nonzero remainder takes. Bears on ARITH-3. The current compiler: the quotient is truncated toward zero and a nonzero remainder has the sign of the dividend, as C99 and LLVM's `sdiv` and `srem` give (`-7 / 2` is `-3`, `-7 % 2` is `-1`, `7 % -2` is `1`), in the binary, the interpreter and the constant folder (`IntArith.evalIntBinOp`). The archived [ARITHMETIC_POLICY.md](../archive/design-records/ARITHMETIC_POLICY.md) states when division aborts (§5) but not how it rounds. Options: (a) truncation toward zero, as in the current compiler; (b) floored division, the remainder taking the divisor's sign; (c) Euclidean division, the remainder never negative. Austral: not checked. Pick: none given (raised by the Codex and Kimi review of the absorption, 2026-10-30). | the quotient is truncated toward zero and a remainder has the dividend's sign (C99) | ARITH-3 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Truncation toward zero |
| OD115 | <a id="od115"></a>The body and totality of a specification function (added 2026-10-30, raised while absorbing the verification documents). Question: Whether a `spec fn` may be declared without a body, and whether its body must be shown total. Bears on CON-3, CON-5, CON-1. The current compiler: both `spec fn big(x: Int) -> bool;` and `spec fn big(x: Int) -> bool = x > 10;` are accepted, and so is a recursive body that never terminates (`spec fn loopy(x: Int) -> bool = loopy(x + 1);`), with no check. [CONTRACTS_GUIDE.md](../verification/CONTRACTS_GUIDE.md) calls a `spec fn` "an erased mathematical specification, no body"; contracts.md's examples (CON-3) give it a body; [CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md) (Soundness Bridge) requires it to be total. Options: (a) a body is required and must be total (structural recursion or a checked measure); (b) a body is optional, a body-less one an uninterpreted function a proof must constrain, and a body must be total; (c) as in the current compiler, no totality check. Austral: not applicable. Pick: none given (raised by the absorption of docs/verification, 2026-10-30). | see OPEN-115 | see OPEN-115 | decided 2026-11-01 by the owner (final set): Body required and total |
| OD116 | <a id="od116"></a>A checked total function category (added 2026-10-30, raised while absorbing the verification documents). Question: Whether the language has a `total fn` category, a runtime function checked to terminate without trapping that contracts may then call. Bears on CON-1, CON-3, CON-5. The current compiler: there is no such category (`total fn f(..)` is a parse error, E0001); a contract clause may still call a capability-free runtime function until CON-3's judgment lands. [CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md) (Design Principles) distinguishes `spec fn`, a checked `total fn`, ordinary and trusted functions, and admits a `total fn` into contracts once its obligations close. Options: (a) add the category; (b) no category: contracts call only `spec fn` (CON-3's full judgment). Austral: not checked. Pick: none given (raised by the absorption of docs/verification, 2026-10-30). | see OPEN-116 | see OPEN-116 | decided 2026-11-01 by the owner (final set): No category |
| OD117 | <a id="od117"></a>`old`, quantifiers and implication in contracts (added 2026-10-30, raised while absorbing the verification documents). Question: Whether contract expressions gain `old(x)`, quantifiers and an implication or equivalence operator, and with what syntax. Bears on CON-2, CON-3, SYN-1. The current compiler: `#[ensures(result == old(x))]` compiles and runs, and `--report contracts` marks the clause `invalid_contract_expression` (unknown function `old`); `forall i: Int, ...` is a parse error (E0001); there is no implication operator. [CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md) (Contract Expression V1) lists all three as deferred. Options: (a) add each, with a syntax decided per form; (b) keep them out, a claim needing one being a Lean theorem; (c) refuse `old(..)` now as an unknown name, deciding the rest later. Austral: not checked. Pick: none given (raised by the absorption of docs/verification, 2026-10-30). | see OPEN-117 | see OPEN-117 | decided 2026-11-01 by the owner (final set): No `old`, quantifiers or implication in contracts; state such properties as Lean theorems |
| OD118 | <a id="od118"></a>A proven run-time violation in trusted code (added 2026-10-30, raised while absorbing the verification documents). Question: Whether an index the compiler proves out of bounds, or a division it proves to be by zero, is refused in `trusted` and `with(Unsafe)` code as in safe code. Bears on MEM-6, ARITH-3, TRUST-5, TRUST-12. The current compiler: `a[5]` on a `[Int; 3]`, or `x / 0`, is refused in safe code (E0900, `error[runtime-safety]`) and compiles in a `trusted fn` (the index also in a function declaring `Unsafe`), where it aborts at run time on both consumers. MEM-6 states the refusal with no exemption; [KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) C7 records the exemption as intended. Options: (a) refuse everywhere, as MEM-6 reads; (b) keep the exemption and state it in MEM-6. Austral: not checked. Pick: none given (raised by the absorption of docs/verification, 2026-10-30). | see OPEN-118 | see OPEN-118 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refuse everywhere |
| OD119 | <a id="od119"></a>Buffered output when the process aborts (added 2026-10-30, raised while absorbing the verification documents). Question: Whether output the program wrote before a trap or abort is flushed before the process ends. Bears on STMT-1, STMT-2, PASS-13, OPEN-101. The current compiler: print builtins write through C's buffered stdio, and `abort` does not flush it on glibc (a binary that prints `hello` and then traps prints nothing on Linux) while it does on Darwin; the interpreter models the same buffer and the same platform difference. STMT-2 says a failure ends the process at once and is silent on output already written, so the observable trace of a trapping program differs by platform. Recorded by [INTERPRETER_TRUST.md](../verification/INTERPRETER_TRUST.md) §2. Options: (a) everything written before the trap is output (flush on abort, or unbuffered output); (b) what was written is unspecified on a trap, and PASS-13 compares output only for exiting runs. Austral: not checked. Pick: none given (raised by the absorption of docs/verification, 2026-10-30). | see OPEN-119 | see OPEN-119 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Everything written before the trap is output |
| OD120 | <a id="od120"></a>A vacuous contract (added 2026-10-30, raised while absorbing the verification documents). Question: Whether a contract whose precondition is unsatisfiable is refused by the language or only reported, under a project policy. Bears on CON-1, CON-2. The current compiler: `#[requires(x > 5 && x < 3)]` compiles; a decided-unsatisfiable precondition makes the claim `vacuous`, never `proved`, and E0613 refuses it only under the `forbid-vacuous` policy (`Concrete.Policy`), not in a plain build. [CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md) (Soundness Bridge) states the reporting rule. Options: (a) a language refusal in every build; (b) tooling only: always reported `vacuous`, refused under a policy, as in the current compiler. Austral: not checked. Pick: none given (raised by the absorption of docs/verification, 2026-10-30). | see OPEN-120 | see OPEN-120 | decided 2026-11-01 by the owner (final set): Tooling: reported `vacuous`, refused under policy |
| OD121 | <a id="od121"></a>Projections from a base that is not a traceable place (added 2026-10-30, raised while absorbing the verification documents). Question: Whether a field access, index or `*` whose base is not a traceable place (an `if` or `match` value, `**r`, a reference read out of storage) is refused, or accepted once the checker can trace it. Bears on REF-5, MEM-4, MEM-8, TYPE-7. The current compiler: refused (E0238, `untraceablePlace`), whether the projection reads a Copy value or forms a reference: `(if c { a } else { b }).f`, `(**r).f`, `*w.r`. No entry states the refusal: REF-5 forbids a branch value or a stored reference as a reference's root, and MEM-4, MEM-8 and TYPE-7 do not restrict a projection's base. [KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) H28 calls it a deliberate restriction until R-0499's checker on Core. Options: (a) state the restriction as a rule (a projection's base is a place: a binding, its fields and elements, or `*` of a named reference); (b) accept such projections of owned values and Copy reads once the checker traces them, refusing only what REF-5 refuses. Austral: not checked. Pick: none given (raised by the absorption of docs/verification, 2026-10-30). | see OPEN-121 | see OPEN-121 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): State the restriction as a rule |
| OD122 | <a id="od122"></a>A `repr(C)` struct returned from an extern function (added 2026-10-30, raised while absorbing the platform and stdlib documents). Question: Whether a `#[repr(C)]` struct returned from an `extern fn` is returned as the platform's C calling convention returns it, at every size. Bears on FFI-2, FFI-11. The current compiler: a returned struct of at most 8 bytes comes back as one 64-bit integer, stored as 8 bytes into the struct's slot even when the struct is smaller (a 4-byte `in_addr` returned by `inet_makeaddr` reads back wrong natively); a larger one is returned as an LLVM aggregate, which matches C on x86-64 and AArch64 only for some layouts. [ABI.md](../platform/ABI.md) promised returns of at most 8 bytes and listed larger ones as not promised. Options: (a) every size, as C, classified by the `Layout` authority (R-0502); (b) at most 8 bytes (or 16), a larger return refused in an `extern fn` signature; (c) none, a returned struct going through an out-pointer the binding declares. Austral: not checked. Pick: none given (raised by the absorption of docs/platform and docs/stdlib, 2026-10-30). | see OPEN-122 | see OPEN-122 | decided 2026-11-01 by the owner (final set): Every size per C ABI via `Layout` |
| OD123 | <a id="od123"></a>`as` on a newtype outside its module (added 2026-10-30, raised while absorbing the platform and stdlib documents). Question: Whether `v as N` and `n as T`, between a newtype `N` and its inner type `T`, are accepted outside the module that declares `N`, where TYPE-11 refuses `N(v)`. Bears on TYPE-11, TYPE-24, ARITH-5, OPEN-54. The current compiler: both accepted in any module, so `x as Id` builds a foreign newtype that `Id(x)` may not (E0296), and a validated wrapper's smart constructor can be bypassed. [VALIDATED_WRAPPERS.md](../stdlib/VALIDATED_WRAPPERS.md) §8 describes the cast rule as guarding the wrapper contract from an `as` bypass, which holds for casts to unrelated types only. Options: (a) accepted anywhere, as in the current compiler; (b) accepted only where `N(v)` and `.0` are (TYPE-11, with R-0500's follow-on for `.0`); (c) `as` never names a newtype, the conversions being `N(v)` and `.0`. Austral: no newtype (OPEN-54's comparison). Pick: none given (raised by the absorption of docs/platform and docs/stdlib, 2026-10-30). | see OPEN-123 | see OPEN-123 | decided 2026-11-01 by the owner (final set): `as` never names a newtype |
| OD124 | <a id="od124"></a>Profiles in the specification (added 2026-10-30, raised while absorbing the platform and stdlib documents). Question: Whether the profiles (the admission profiles Safe, Predictable and Proof, and the build profiles) belong to the language specification, in a chapter of their own, and whether a profile may change what an accepted program does. Bears on ARITH-9, OPEN-97, PASS-3, PASS-4. The current compiler: a profile only refuses or reports: `--check predictable` reports its five gates (no recursion, bounded loops, no allocation, no extern calls, no `File`, `Network` or `Process`), `[policy] predictable = true` in `Concrete.toml` turns a violation into E0610, and no profile changes the behaviour of a program it accepts; ARITH-9 states that for arithmetic. [PROFILES.md](../platform/PROFILES.md) says a profile never changes what a program means, while OPEN-97 asks whether a build profile may omit a run-time check. Options: (a) no profiles chapter: profiles are tooling, and the spec states only that no profile changes an accepted program's behaviour; (b) a profiles chapter whose entries state each profile's admission rules as refusals, with that same guarantee; (c) as (b), with build profiles allowed to differ as OPEN-97 decides. Austral: not checked. Pick: none given (raised by the absorption of docs/platform and docs/stdlib, 2026-10-30). | see OPEN-124 | see OPEN-124 | decided 2026-11-01 by the owner (final set): Profiles chapter of refusals; no behaviour change |
| OD125 | <a id="od125"></a>Contracts at run time (added 2026-10-30, raised while absorbing the platform and stdlib documents). Question: Whether a `#[requires]`, `#[ensures]` or loop `#[invariant]` that no proof discharges is checked at run time, trapping when it fails, or is only a claim. Bears on CON-1, CON-2, STMT-2. The current compiler: contracts are erased metadata: a call that violates `#[requires(x > 0)]` runs normally, in the binary and in `--interp`, and `assert` elaborates to proof evidence only (syntax chapter introduction). [ERROR_CONVENTIONS.md](../stdlib/ERROR_CONVENTIONS.md) lists contract-violation traps among the fatal failures. Options: (a) claims only, never checked at run time, as in the current compiler; (b) checked at run time and trapping unless discharged; (c) checked under a build profile only (OPEN-97, OPEN-124). Austral: not checked. Pick: none given (raised by the absorption of docs/platform and docs/stdlib, 2026-10-30). | see OPEN-125 | see OPEN-125 | decided 2026-11-01 by the owner (final set): Claims only |
| OD126 | <a id="od126"></a>Target assumptions (added 2026-10-30, raised while absorbing the platform and stdlib documents). Question: Which target facts are language rules: 64-bit pointers, little-endian byte order, POSIX hosts, and the sizes of `bool`, `Unit` and pointers. Bears on FFI-2, FFI-7, TYPE-19, TYPE-20. The current compiler: one target profile: `Layout` hardcodes 8-byte pointers and references, `bool` of 1 byte, `Unit` of 0 bytes, an `i32` enum tag, and assumes little-endian POSIX hosts; FFI-2 names AAPCS64 for the native profile, and the integer types' widths follow from their names (TYPE-19). [EXECUTION_MODEL.md](../platform/EXECUTION_MODEL.md) and [ABI.md](../platform/ABI.md) state these as platform facts. Options: (a) language rules, a 32-bit or big-endian target being another language profile; (b) target facts, observable only through `sizeof`, `alignof` (FFI-7) and named target constants; (c) as (b), with the sizes of `bool` and `Unit` fixed by the language. Austral: not checked. Pick: none given (raised by the absorption of docs/platform and docs/stdlib, 2026-10-30). | see OPEN-126 | see OPEN-126 | decided 2026-11-01 by the owner (final set): Language rules; other targets are other profiles |
| OD127 | <a id="od127"></a>UTF-8 in every `String` (added 2026-10-30, raised while absorbing the platform and stdlib documents). Question: Whether every `String` value that safe code can reach holds valid UTF-8, as a language guarantee. Bears on TYPE-3, SYN-5, TRUST-2. The current compiler: literals are UTF-8 (SYN-5), but nothing keeps a `String` valid: `push_char` appends a byte unchecked, `Bytes::to_string` transfers bytes without validation, and std's `read_to_string` does not validate. [STRING_TEXT_CONTRACT.md](../stdlib/STRING_TEXT_CONTRACT.md) §1 states the invariant as a target, and [UNICODE_POLICY.md](../stdlib/UNICODE_POLICY.md) has a validating `Bytes::to_string` returning `Option`. Options: (a) a language guarantee, every safe constructor validating and an unchecked one being a trusted operation with that premise; (b) a std convention only, `String` being bytes to the language. Austral: not checked. Pick: none given (raised by the absorption of docs/platform and docs/stdlib, 2026-10-30). | see OPEN-127 | see OPEN-127 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Language guarantee |
| OD128 | <a id="od128"></a>`->` between a match arm's pattern and body (added 2026-10-30, raised while absorbing the compiler documents and the grammar comments). Question: Whether a match arm may separate its pattern from its body with `->` as well as `=>`. Bears on SYN-1 and PAT-7. The current compiler: the parser accepts both (`parseMatchArms`; `2 -> { return 42; }` exits 42 on both consumers), and the grammar's `match_arm` has only `=>`. The syntax freeze review (2026-04, [SYNTAX_FREEZE_REVIEW.md](../archive/design-records/SYNTAX_FREEZE_REVIEW.md) §1.5) recommended `=>` as the canonical form with a warning for `->`, which also spells a return type. Options: (a) `=>` only, `->` refused; (b) both accepted; (c) both accepted, with a warning on `->`, as the freeze review recommended. Austral: not applicable. Pick: none given (raised by the absorption of docs/compiler, 2026-10-30). | see OPEN-128 | see OPEN-128 | decided 2026-11-01 by the owner (final set): `=>` only |
| OD129 | <a id="od129"></a>Type arguments of an enum literal taken from its expected type (added 2026-10-30, raised while absorbing the compiler documents and the grammar comments). Question: Whether an enum literal or variant written without type arguments (`Option::Some { value: 42 }`, `Option::None`) takes them from the type its position expects, and from which positions. Bears on TYPE-7, TYPE-15 and OPEN-46, which asks the same of a generic call's type arguments and whose pick excludes the expected result type. The current compiler: the type arguments come from the expected type of a `return` (the function's result type), of an annotated `let` and of a call argument (the parameter's type); all three exit 42 on both consumers, and spec examples rely on it (PAT-1). The syntax freeze review (§1.3, kill-list items 1, 4 and 5) proposed this inference for enum literals and for constructor calls such as `Vec::new()`. Options: (a) explicit type arguments always; (b) from the expected type in the positions the current compiler uses; (c) the answer to OPEN-46, applied to enum literals. Austral: type parameters are inferred from the arguments, and a declaration's type can disambiguate. Pick: none given (raised by the absorption of docs/compiler, 2026-10-30). | see OPEN-129 | see OPEN-129 | decided 2026-11-01 by the owner (final set): Same rule as OD46: type arguments from the arguments, otherwise explicit (`None::<i32>`) |
| OD130 | <a id="od130"></a>Unlabeled pattern binders: by position or by name (added 2026-10-30, raised while absorbing the compiler documents and the grammar comments). Question: Whether an unlabeled binder in a struct or variant pattern (`E::V { y, x }`) binds the field of its own name or the field at its position. Bears on PAT-1, PAT-3 and PAT-10. The current compiler disagrees with itself: with `enum E { V { x: Int, y: Int } }` and `E::V { x: 40, y: 2 }`, the arm `E::V { y, x } => y` gives 40 in `match` and in `if let` (binders zip the variant's fields in declaration order, `Concrete/Elab/Elab.lean` `bindings.zip ev.fields`), while the struct destructuring `let S { y, x } = s; y` gives 2 (by name); both consumers agree. PAT-1's `field: binder` form and the grammar's `binding_list` comment ("positional") point different ways. Options: (a) by name (field punning) everywhere; (b) by position everywhere; (c) refuse an unlabeled list whose order differs from the declaration. Austral: not applicable (no punning). Pick: none given (raised by the absorption of docs/compiler, 2026-10-30; the variant-pattern disagreement may also be filed as a bug). | see OPEN-130 | see OPEN-130 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): By name everywhere |
| OD131 | <a id="od131"></a>`union` declarations (added 2026-10-30, raised while absorbing the compiler documents and the grammar comments). Question: Whether the language has `union` declarations, and if so what reading a field other than the one last written means. Bears on SYN-1, TYPE-7, FFI-7 and FFI-8. The current compiler: `union U { a: Int, b: Uint }` parses (`Concrete/Frontend/Parser.lean`, as a struct whose fields share storage) and compiles; a value built as `U { a: 42 }` is linear, and `union` is neither a keyword nor in the grammar. No document specifies it. Options: (a) remove it; (b) keep it, for the foreign boundary only, with its layout and read rules specified. Austral: no unions. Pick: none given (raised by the absorption of docs/compiler, 2026-10-30). | see OPEN-131 | see OPEN-131 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Remove |
| OD132 | <a id="od132"></a>Polymorphic recursion (added 2026-10-30, raised while absorbing the compiler documents and the grammar comments). Question: Whether a generic function that calls itself at a strictly larger type argument (`f::<Box<T>>` inside `f<T>`) is refused, and by which check. Bears on TYPE-13, PASS-7 and PASS-12. The current compiler: such a program is not refused; compilation does not terminate (more than 200 s and 2 GB on 2026-10-30, natively and with `--interp`), since monomorphization specializes without a bound on function instances (the bound `closeInstances` has covers only generic type definitions). [PASSES.md](../compiler/PASSES.md) §7 lists unbounded polymorphic recursion as Mono's error. Options: (a) refuse a call cycle whose type arguments grow, before Mono; (b) refuse at Mono when specialization exceeds a stated limit. Austral: not checked. Pick: none given (raised by the absorption of docs/compiler, 2026-10-30; the non-termination may also be filed as a bug). | see OPEN-132 | see OPEN-132 | decided 2026-11-01 by the owner (final set): Refuse growing call cycles before Mono |
| OD133 | <a id="od133"></a>User functions named `print`, `println` or `append` (added 2026-10-30, raised while absorbing the compiler documents and the grammar comments). Question: Whether a program may declare a function named `print`, `println` or `append`, which then replaces the builtin of that name in calls the program writes. Bears on MOD-3, MOD-8 and TYPE-10. The current compiler: accepted; with `fn print(x: Int) -> Int`, `print(42)` calls it (exit 42 on both consumers), because Elab desugars the variadic builtins only when no function of that name is visible, and MOD-3's reserved names do not include them. MOD-8 already binds the calls the compiler itself generates by identity. Options: (a) reserve the three names (MOD-3); (b) allow the replacement, as the current compiler does. Austral: not applicable. Pick: none given (raised by the absorption of docs/compiler, 2026-10-30). | see OPEN-133 | see OPEN-133 | decided 2026-11-01 by the owner (final set): Reserve the three names |
| OD134 | <a id="od134"></a>The capabilities of a container's generated destruction (added 2026-10-30, raised while reviewing the absorption of docs/stdlib). Question: Whether the destruction a generic container generates for its elements may require the capabilities of those elements' `Destroy` implementations beyond `Alloc`, or every `Destroy` implementation stays limited to `Alloc`. Bears on OWN-13, TYPE-D1 and TRUST-4. The current compiler: OWN-13 is enforced, so a `Destroy` implementation requires no capability beyond `Alloc`, and a container's generated destruction is `with(Alloc)`. [RUNTIME_COLLECTIONS.md](../stdlib/RUNTIME_COLLECTIONS.md) rule 3 describes a different model, in which the generated destruction requires the union of its element, key and value destructors' capabilities, computed per instantiation, and calls today's limit a first-release restriction. Options: (a) keep OWN-13's limit, so destruction never needs more than `Alloc`; (b) lift it, so a `Destroy` implementation may require further capabilities and a container's destruction requires their union per instantiation. Austral: not assessed. Pick: none given (raised by the review of the absorption of docs/stdlib, 2026-10-30). | see OPEN-134 | see OPEN-134 | decided 2026-11-01 by the owner (final set): Keep `Alloc` limit |
| OD135 | <a id="od135"></a>A method's `Type_method` name in its module's namespace (added 2026-10-30, raised while reviewing the absorption of docs/compiler and docs/platform). Question: Whether an impl method `m` of a type `T` also occupies the name `T_m` in its module, so that a free function of that name is refused. Bears on MOD-3 and MOD-8. The current compiler: refused: `fn P_get(a: Int) -> Int` beside `impl P { fn get(&self) -> Int }` is E0299 under MOD-3@2 ("a function named 'P_get' is also declared in this module as an impl method"), on both consumers, because the method is entered under its mangled name. MOD-3's reserved names are the `__` prefix, `user_main` and the builtins, which do not include such names. Options: (a) keep it, listing `T_m` among the names MOD-3 reserves; (b) stop entering methods under `T_m`, so the free function is accepted. Austral: not applicable (no methods). Pick: none given (raised by the review of the absorption of docs/compiler, 2026-10-30). | see OPEN-135 | see OPEN-135 | decided 2026-11-01 by the owner (final set): Stop entering methods under `T_m` |
| OD136 | <a id="od136"></a>std's `process_exit` (added 2026-10-30, raised while reviewing the absorption of docs/compiler and docs/platform). Question: Whether std keeps `process_exit`, which ends the process through libc `exit` without running any deferred call or consuming the caller's owned values. Bears on STMT-7, OWN-2 and TRUST-4. The current compiler: `std.process.process_exit(code: i32) with(Process)` exists and calls libc `exit`; the language itself has no exit operation (STMT-7). The design record [MAIN_EXIT_MODEL.md](../platform/MAIN_EXIT_MODEL.md) calls an exit operation deliberately absent, since in a linear language it would bypass the proof that every owned value is consumed. Options: (a) keep it, stating that it skips cleanup and that `Process` is what grants it; (b) remove it, status flowing back through `main`'s return only. Austral: not checked. Pick: none given (raised by the review of the absorption of docs/platform, 2026-10-30). | see OPEN-136 | see OPEN-136 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Remove |
| OD137 | <a id="od137"></a>Character literals outside ASCII (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: Which byte a char literal denotes when its character is not ASCII. Bears on SYN-6, TYPE-20 and SYN-5. The current compiler: the literal silently takes the low byte of the character's code point: `'é'` (U+00E9) is 233, `'ā'` (U+0101) is 1 and `'€'` (U+20AC) is 172 as `u8`, on both consumers, and SYN-6 says only that a char literal is a token. Options: (a) refuse a char literal whose character is not ASCII, so every accepted literal is the one byte of its ASCII character; (b) accept U+0000 to U+00FF as that byte (Latin-1) and refuse anything above; (c) make `char` a Unicode scalar value, which changes TYPE-20's one-byte `char`. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-137 | see OPEN-137 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refuse non-ASCII |
| OD138 | <a id="od138"></a>Which positions give a literal its expected type (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: Which positions give an integer literal operand its expected type under ARITH-2@3 (and a float literal's, should OPEN-63 give floats the same rule). Bears on ARITH-2, TYPE-4 and TYPE-6. The current compiler: an unconstrained integer literal is typed `Int` and a float literal `Float64` (bug 327); with `x: u8`, a literal beside `x` takes `u8` as the other operand of `+`, `<` and `&` and as the count of `>>`: `x + 300`, `x < 300`, `x & 300` and `x >> 300` are each refused natively with E0227 (`integer literal 300 is out of range for type 'u8'`), and `300 as Int` is accepted, whether through `as` or the `Int` default the probe does not show. ARITH-2@3 refuses an integer literal with no expected type but does not list the positions that supply one. Options: (a) declared positions (an annotated binding, a parameter, a return, a field) and the other operand of a binary operator, as the probe shows the current compiler doing for `+`, `<`, `&` and a shift count (other operators were not probed); (b) declared positions only, so a literal beside a typed operand needs a typed binding; (c) option (a), plus the target type of an `as` for its literal operand. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-138 | see OPEN-138 | decided 2026-11-01 by the owner (final set): Declared positions + other operand of a binary op |
| OD139 | <a id="od139"></a>A qualified call to a function that is not imported (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: Whether `a::f()` may call a `pub` function of another module that the caller has not imported. Bears on MOD-2, SYN-10 and MOD-4. The current compiler: accepted: `mod a { pub fn answer() -> u8 { return 42; } }` and `a::answer()` in `main` exits 42 with no import, as MOD-4's and SYN-10's accepted examples also do, while MOD-2's sentence says another module uses a declaration only by importing it. Options: (a) a qualified path to a `pub` declaration is a use in its own right, and MOD-2 says so; (b) an import is required, and the unimported qualified call is refused. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-139 | see OPEN-139 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Import required |
| OD140 | <a id="od140"></a>Implicitly dropped results of `free` and of a deferred call (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: Whether the result of `free(h)` (the pointee, of type `T`) when `T` is Copy, and a deferred call's Copy result, may be dropped without `discard`, as exceptions to OWN-7@3 (OD29). Bears on OWN-7, OWN-17, MEM-5 and OWN-15. The current compiler: both accepted: `free(h);` on a `Heap<Int>` (whose result is the `Int` moved out of the cell: `let v: Int = free(h);` after `alloc(42)` gives 42 on both consumers) and `defer f();` with `fn f() -> u8` compile and run, while OWN-7@3 (decided OD29) drops a non-Unit result only through `discard(e)`. OWN-17 states the current compiler's drop of a deferred call's result, and MEM-5 and OWN-15 describe `free(h);` as a statement; neither is named as an exception in OWN-7. Options: (a) both are exceptions, stated in OWN-7; (b) neither is, so each needs `discard`; (c) `free` is an exception and a deferred call's result is not. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-140 | see OPEN-140 | decided 2026-11-01 by the owner (final set): Neither; each needs `discard`. Earlier clarification of 2026-11-02: `free` no longer exists (OD59); a deferred call's Copy result is dropped with `discard`, while a `Result` or `Option` cannot be discarded (OD62) and is handled by a named function that returns `Unit` ([decisions](#decided-2026-11-02-authority)). Revised 2026-11-02: `defer` takes a call whose result is `Unit` ([decisions](#decided-2026-11-02-audit)) **Canonical current deferred-result rule: the Unit-only audit rule below; the earlier discard-inside-defer fragment is superseded, and authority rule 9 refers to the Unit-only rule.** |
| OD141 | <a id="od141"></a>More than one `main` in a program (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: Which function is the entry point when more than one module of a program declares `main`. Bears on STMT-7, MOD-3 and PASS-10. The current compiler: `mod a { pub fn main() -> u8 { return 7; } }` beside `mod b { pub fn main() -> u8 { return 9; } }` fails natively inside LLVM (`redefinition of global '@__concrete_argc'`) and prints 7 under `--interp`, which runs the first, also when the second `main` is the root module's; MOD-3 makes a name unique within one module only. Options: (a) only the root module's `main` is the entry point, and others are ordinary functions; (b) a program with more than one `main` is refused; (c) a `main` outside the root module is refused. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-141 | see OPEN-141 | decided 2026-11-01 by the owner (final set): A `main` outside the root module is refused |
| OD142 | <a id="od142"></a>`X.con` beside `X/mod.con` (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: Which file `mod X;` loads when both `X.con` and `X/mod.con` exist. Bears on MOD-4. The current compiler: `X.con` is loaded and `X/mod.con` is ignored without a diagnostic (with `X.con` returning 7 and `X/mod.con` 9, the program exits 7 on both consumers). MOD-4 says one module is loaded from either file. Options: (a) refuse the pair; (b) `X.con` takes precedence, as the current compiler does; (c) `X/mod.con` takes precedence. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-142 | see OPEN-142 | decided 2026-11-01 by the owner (final set): One module layout: `X.con`, children in directory `X/`; no `mod.con` |
| OD143 | <a id="od143"></a>What a contract may read (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: Which names each contract position (`requires`, `ensures`, `invariant`, `variant`) may read. Bears on CON-2, CON-3 and CON-4. The current compiler: CON-3 says a contract reads parameters and `result`, CON-4 lets contracts read `ghost let` bindings, and CON-2's accepted loop invariant reads the local `i`; the checker accepts all three. Options: (a) per position: `requires` the parameters, `ensures` the parameters, `result` and ghost bindings, and a loop's `invariant` and `variant` every binding in scope, with CON-3 amended; (b) every position reads every binding in scope at that point; (c) CON-3 as written, with loop contracts and ghost reads refused. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-143 | see OPEN-143 | decided 2026-11-01 by the owner (final set): A contract reads exactly the names in scope at its position, plus `result` in `ensures` |
| OD144 | <a id="od144"></a>Postconditions and deferred calls (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: Whether an `ensures` clause is about the state before or after the function's deferred calls run. Bears on CON-2 and STMT-11. The current compiler: `#[ensures(result == *r)]` on a function whose `defer inc(r)` runs after the result is computed is accepted and runs (contracts are erased at run time); the probe does not show which state the postcondition's proof obligation reads, no rule says, and STMT-11 computes the result before the deferred calls run. Options: (a) after every deferred call, the state the caller sees; (b) when the result is computed, before deferred calls; (c) refuse an `ensures` in a function with a deferred call that may write what the clause reads. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-144 | see OPEN-144 | decided 2026-11-01 by the owner (final set): After every deferred call |
| OD145 | <a id="od145"></a>How a contract names machine arithmetic (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: How a contract expression names machine arithmetic, which CON-3 allows in place of its default mathematical arithmetic. Bears on CON-3 and ARITH-4. The current compiler: `#[ensures(result == wrapping_add(x, 1))]` is accepted and the program runs (contracts are erased at run time); what the proof pipeline makes of the named operation was not probed. CON-3 says a contract's integer arithmetic is mathematical unless it names a machine operation, and no rule or example names one. Options: (a) the named operations of ARITH-4 (`wrapping_add`, `saturating_add` and the rest) are the machine operations in a contract; (b) a contract-only spelling; (c) contracts have only mathematical arithmetic, and CON-3 drops the exception. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-145 | see OPEN-145 | decided 2026-11-01 by the owner (final set): ARITH-4 named operations |
| OD146 | <a id="od146"></a>The result type of a test function (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: Whether a `#[test]` function returns `i32` (STMT-10) or follows `main`'s decided `u8` or `Unit` (OD24). Bears on STMT-10 and STMT-7. The current compiler: `#[test] fn t() -> Int` is refused with E0319 (`#[test] function 't' must return i32`), and a test's result is its exit status under `concrete test`. Options: (a) keep `i32`; (b) `u8` or `Unit`, as `main` (STMT-7@3); (c) `Unit` only, a failing test aborting. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-146 | see OPEN-146 | decided 2026-11-01 by the owner (final set): `u8` or `Unit`, as `main`. Revised 2026-11-02: `u8` only, as `main` ([decisions](#decided-2026-11-02-main)) |
| OD147 | <a id="od147"></a>`repr(C, packed)` together (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: Whether `#[repr(C, packed)]` is accepted, and what layout it gives. Bears on FFI-4 and FFI-8. The current compiler: accepted: `#[repr(C, packed)] struct Copy P { a: u8, b: u32 }` has size 5, so packing overrides C alignment. FFI-4 refuses only `packed` with `align(N)`, and FFI-8 states C layout and packed layout separately. Options: (a) refuse the combination; (b) accept it as C field order with no padding and alignment 1, as the current compiler does, and FFI-8 says so. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-147 | see OPEN-147 | decided 2026-11-01 by the owner (final set): Accept: C order, no padding, align 1, stated in FFI-8 |
| OD148 | <a id="od148"></a>Type parameters on a trait (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: Whether a trait declaration may have type parameters, and if so what binds them at a bound and an impl. Bears on TRAIT-1, TRAIT-9 and SYN-1. The current compiler: `trait Tr<T> { fn get(&self, x: T) -> T; }` parses, but no bound or impl head can supply `T`, so `impl Tr for S { fn get(&self, x: u8) -> u8 ... }` is refused with E0582 and E0587 (`expected return type T, got u8`). OPEN-77 lists generic trait methods among the grammar's stale items; it covers only the productions, while this entry covers what the parameters mean and what binds them. Options: (a) refuse type parameters on a trait declaration; (b) allow them, with type arguments at bounds and impl heads (`impl Tr<u8> for S`). Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-148 | see OPEN-148 | decided 2026-10-31 by the owner's rule (every reviewer agreed, as strict as Austral or stricter): Refuse type parameters on a trait |
| OD149 | <a id="od149"></a>How `[e; N]` evaluates `e` (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: How many times an array repetition `[e; N]` evaluates `e`, and which `e` it accepts. Bears on STMT-4, OWN-15 and TYPE-7. The current compiler: `e` is evaluated once per element, in index order (`[bump(&mut n); 3]` gives `[1, 2, 3]` on both consumers), and whether a linear `e` is accepted was not shown (an array of a linear type that is never consumed is refused with E0208 either way). OWN-15 says only that an array literal moves each element in, and STMT-4 orders written elements. Options: (a) `e` is evaluated once and copied, so it must be Copy; (b) `e` is evaluated `N` times in index order, as the current compiler does; (c) `e` must be a constant expression. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-149 | see OPEN-149 | decided 2026-11-01 by the owner (final set): Evaluated once, must be Copy |
| OD150 | <a id="od150"></a>Which text governs the grammar until it is resynchronized (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: Whether, until OPEN-77's resynchronization, `grammar/concrete.ebnf` or the rule entries govern where they disagree. Bears on SYN-1 and OPEN-77. The current compiler: the parser accepts forms the EBNF does not derive (`import a.{f};`, `m::f()`, `f::<T>(x)`, value `if` and `match`, `pub` fields, bodyless declarations) and the EBNF keeps forms the entries removed (`E#V`, the C-style `for`, `break <expr>`); SYN-1 calls the EBNF the grammar, and the LL(1) checkers read it. Options: (a) the rule entries govern, and the EBNF is descriptive until the resync; (b) the EBNF governs, and an entry that disagrees is pending; (c) neither: each disagreement is decided one by one under OPEN-77. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-150 | see OPEN-150 | decided 2026-11-01 by the owner (final set): Rule entries govern |
| OD151 | <a id="od151"></a>Module paths of more than one level (added 2026-10-31, raised while absorbing SYNTAX_FREEZE_REVIEW §1.1). Question: Whether a qualified path may name a member of a module nested more than one level deep (`a::b::f()`). Bears on SYN-10, MOD-4 and OPEN-139. The current compiler: the parser's lowercase module-path branch accepts one qualification pair: `a::b::f()`, with `f` in `mod b` inside `mod a`, is a parse error on both consumers (E0001, `expected <, got ident(f)`), because after `a::b` it accepts only `::<` (explicit type arguments). SYNTAX_FREEZE_REVIEW §1.1 gives `std::fs::read_to_string` as an example of module qualification, which this parser does not accept. Options: (a) one qualification pair, as in the current compiler, and SYN-10 says so; (b) any depth, every segment before the last naming a module (a repetition, which stays LL(1)). Austral: not checked. Pick: none given (raised while absorbing SYNTAX_FREEZE_REVIEW §1.1, 2026-10-31). | see OPEN-151 | see OPEN-151 | decided 2026-11-01 by the owner (final set): Allow multi-level module paths `a::b::f` |
| OD152 | <a id="od152"></a>Whether a path's first letter decides its syntactic form (added 2026-10-31, raised while absorbing SYNTAX_FREEZE_REVIEW §1.1). Question: Whether the first letter of a path's first segment decides the syntactic form of `X::y`, or one path form leaves to name resolution whether `X` is a type or a module. Bears on SYN-10, SYN-1 and MOD-1. The current compiler: the parser chooses by the first letter of `X`: an uppercase `X` gives an associated-function call or an enum literal, and a lowercase `X` the flattened name `X_y`, which name resolution looks up as one identifier (bug 343); a module named with an uppercase letter goes through the associated-function form, and `M::f()` on `mod M` builds and runs on both consumers. SYNTAX_FREEZE_REVIEW §1.1 presents the case split as how the parse stays LL(1). Options: (a) the first letter decides the syntactic form, as in the current compiler, and SYN-10 states it; (b) the parser reads `X::y` as one path form, which is also LL(1), and name resolution decides whether `X` is a type or a module. Austral: not checked. Pick: none given (raised while absorbing SYNTAX_FREEZE_REVIEW §1.1, 2026-10-31). | see OPEN-152 | see OPEN-152 | decided 2026-11-01 by the owner (final set): Resolve `X::y` by declaration, not by the first letter |
| OD153 | <a id="od153"></a>Program functions named like a builtin (added 2026-10-31, raised while reviewing the builtins catalog of spec readability 2). Question: Whether a program may declare a function whose name is a builtin's, other than `print`, `println` and `append` (OPEN-133) and the names MOD-3 reserves, and which function the program's calls of that name reach. Bears on MOD-3, MOD-1, MOD-8 and OPEN-133. The current compiler: a program function `string_length` is called by the program's calls on both consumers, and so are program functions named `string_len` and `String_len` (`string_len` returns 7 and `String_len` returns 9; a program that calls both exits 79 on both consumers), while `destroy`, `discard`, `abort`, `alloc`, `free`, `alloc_array`, `free_array` and `realloc_array` are reserved (E0578). The calls the compiler itself generates are bound by identity (MOD-8), so only the program's own calls change. Options: (a) reserve every remaining builtin's name, aliases included, and MOD-3 lists them (`print`, `println` and `append` stay with OPEN-133); (b) allow the declaration, and the program's own calls reach it, as the current compiler does, and MOD-3 says so; (c) allow the declaration, and a program reaches the builtin by a qualified name. Austral: not checked. Pick: none given (raised while reviewing the builtins catalog of spec readability 2, 2026-10-31). | see OPEN-153 | see OPEN-153 | decided 2026-11-01 by the owner (final set): Refuse program functions named like builtins |
| OD154 | <a id="od154"></a>Whether the string builtins require `Alloc` (added 2026-10-31, raised while reviewing the builtins catalog of spec readability 2). Question: Whether `string_concat`, `string_slice`, `string_substr`, `string_trim`, `int_to_string`, `bool_to_string`, `float_to_string`, `string_push_char`, `string_append`, `string_append_int` and `string_append_bool` require the `Alloc` capability. Bears on MEM-7, TRUST-4 and OPEN-133. The current compiler: Check gives none of these builtins a capability, and a function with no capabilities calls `string_concat` and `string_append_int` and exits 42 on both consumers (the other names were not probed). MEM-7@1 lists `string_reserve` and `append` among the builtins that require `Alloc`; `append` is accepted without `Alloc` (bug 349), and it expands into `string_append`, `string_append_int`, `string_append_bool` and `string_push_char`. MEM-7's rationale says allocation is an effect a signature shows. Options: (a) each builtin named above requires `Alloc`, and MEM-7 lists it; (b) a stated subset of them requires `Alloc`, and MEM-7 lists that subset; (c) only the builtins MEM-7 already lists require `Alloc`, so `string_reserve` and `append` do and the names above do not, and MEM-7 says so; (d) no string builtin requires `Alloc`, `string_reserve` and `append` included, and MEM-7 drops those two. Austral: not checked. Pick: none given (raised while reviewing the builtins catalog of spec readability 2, 2026-10-31). | see OPEN-154 | see OPEN-154 | decided 2026-11-01 by the owner (final set): Allocating string builtins need `Alloc` (MEM-7; closes bug 349) |
| OD155 | <a id="od155"></a>`unwrap` beside TYPE-24's conversions (added 2026-10-31, raised while reviewing the builtins catalog of spec readability 2). Question: Whether `unwrap` stays an intrinsic beside the conversions TYPE-24 lists (`n.0` and `n as T`), becomes one of those conversions, or is removed. Bears on TYPE-24, TYPE-11, TYPE-10 and OPEN-54. The current compiler: Check gives `unwrap(m)` the newtype's inner type (E0270 when the argument is not a newtype); `return unwrap(m);` exits 42 on both consumers, and `let x: Int = unwrap(m);` followed by `if x == 42` fails the native build at SSA verification (E0715, bug 345). TYPE-24 lists `n.0` and `n as T` and does not list `unwrap`. OPEN-54 decides this only if its answer removes newtypes; while newtypes stay, this question stays. Options: (a) keep `unwrap` as an intrinsic and leave it off TYPE-24's list, as the current compiler and TYPE-24 do; (b) `unwrap(n)` is a conversion, TYPE-24 lists it, and TYPE-11 restricts it as it restricts `n.0`; (c) remove `unwrap`, leaving `n.0` and `n as T`. Austral: not checked. Pick: none given (raised while reviewing the builtins catalog of spec readability 2, 2026-10-31). | see OPEN-155 | see OPEN-155 | decided 2026-11-01 by the owner (final set): No builtin `unwrap`; use `match` |
| OD156 | <a id="od156"></a>The capability of `get_args` (added 2026-10-31, raised while reviewing the builtins catalog of spec readability 2). Question: Which capability the builtin `get_args` requires, or whether the builtin is removed. Bears on TRUST-4, STMT-7, TRUST-9 and OPEN-136. The current compiler: Check gives `get_args()` the result `HeapArray<String>` and requires `Process` (E0520 with `Env` only); the builtin has no implementation on either consumer (bug 347). STMT-7's rationale says command-line arguments come from `std.args` with `Env`, and OD8 gives `main` the root resource value (TRUST-9). Options: (a) `Env`, as `std.args` requires; (b) `Process`, as the current compiler's signature says; (c) another capability, or none; (d) remove the builtin, and command-line arguments come from `std.args`. Austral: not checked. Pick: none given (raised while reviewing the builtins catalog of spec readability 2, 2026-10-31). | see OPEN-156 | see OPEN-156 | decided 2026-11-01 by the owner (final set): Superseded by the authority decision of 2026-11-01: `get_args` requires `with(Env)`, and `with(Alloc)` when it allocates. Revised 2026-11-02 by the owner's rule: it also takes the `Environment` grant, and an allocator when it allocates ([decisions](#decided-2026-11-02-filesystem)) |
| OD157 | <a id="od157"></a>How the type `Unit` is written (added 2026-10-31, raised while reviewing the builtins catalog of spec readability 2). Question: Whether the type `Unit`, which STMT-7 names as a result type of `main` and which other rules and the builtins catalog also name, has a spelling in type positions, and which spelling. Bears on STMT-7, TYPE-4, SYN-1 and OPEN-66. The current compiler: a function with no result type returns no value, and `fn main() { return; }` exits 0; `fn main() -> Unit` and `let x: Unit = f();` are refused at resolve (E0108, unknown type 'Unit'); `fn main() -> ()` is a parse error (E0001, `expected type`). STMT-7@3 says `main`'s result type is `u8` or `Unit`. OPEN-66 concerns the unit value's literal; this entry concerns the type in type positions, and the two cross-reference each other. Options: (a) no spelling: the type is written only by omitting the result type, and STMT-7 says so; (b) `Unit` is a builtin type name, accepted wherever a type is; (c) `()` is the type's spelling; (d) both `Unit` and `()` are accepted. Austral: not checked. Pick: none given (raised while reviewing the builtins catalog of spec readability 2, 2026-10-31). | see OPEN-157 | see OPEN-157 | decided 2026-11-01 by the owner (final set): The type is written `Unit`, its value `()` |


### Decided

These entries were retired when the owner answered them; their IDs are never reused.

- **The authority design** (decided 2026-11-01, after a Codex and Grok review that concluded "adopt
  with changes"; its rules confirmed and completed on 2026-11-02, see
  [the authority rules](#decided-2026-11-02-authority)). `with(...)` is the only permission
  mechanism; it is compile-time information, not a resource. Linear values are the only resource
  mechanism: `File`, `Socket`, `Heap<T, A>`, and the grants the target hands to `main`. The concept
  "token" is dropped. An operation needs both the resource value (which thing) and the permission
  (what is allowed); in the owner's words, "one is the parameter to modify, the other the permission",
  and this is not duplication. `Std` is removed as a capability: every signature, `main` included,
  lists its real capabilities; std, examples and tests are to migrate under exit 2q, with the compiler
  suggesting the exact set needed. It supersedes OD38's and OD156's readings through a `Root` value
  and OD83's allocator token. OD8 stands except where this supersedes it: its root value and
  its split (item 2) give way to the grants the target hands to `main`, and its "every effect shows as a
  token parameter, with no separate effect system" (item 3) gives way to `with(...)` plus resource
  values. Its "no life before `main`" (item 5) stands: the target's startup and shutdown only create
  the grants, run no user code and do nothing else a program could observe beyond what the target's
  table states.

<a id="decided-2026-11-02"></a>
**Decided 2026-11-02 by the owner's rule** (the owner: "anything where you and codex agree that is the
best decision, record the solution"; each answer below is the one Claude and Codex both judged the best
long-term answer, Codex having read the repository; the owner answered the first four directly on
2026-11-01):

- <a id="decided-2026-11-02-aliases"></a>**Type-name aliases** (after OD102): the aliases `Int`,
  `Uint`, `Float64` and `Bool` are removed. A builtin type has one spelling (`i64`, `u64`, `f64`,
  `bool`, ...). Owner's answer, 2026-11-01.
- <a id="decided-2026-11-02-main"></a>**`main`'s result** (revises OD24): `main` returns `u8`, and
  only `u8`. Owner's answer, 2026-11-01. A test function's result follows it (revises OD146): `u8`
  only.
- <a id="decided-2026-11-02-namespace"></a>**One namespace per module** (completes OD43): a type and a
  value (a function or constant) in one module may not share a name. A struct literal `S { ... }` and
  a variant path `E::V` name the type itself and are not collisions. Owner's answer, 2026-11-01.
- <a id="decided-2026-11-02-child"></a>**A child module's declarations** (completes OD43) are not
  visible in its parent by bare name: the parent writes `child::T` or imports it. Owner's answer,
  2026-11-01.

<a id="decided-2026-11-02-authority"></a>**The authority rules** (they replace the twelve rules approved
in summary on 2026-11-01):

1. **A value is not a permission, and a permission is not a value.** Holding a value never satisfies
   a `with` requirement, and holding a permission never supplies a required value.
2. **Every operation needs both a resource value and a permission.** There are no permission-only
   operations: the target's services are values too (an `Allocator<A>`, a `Directory`, an
   `Environment`, a `Clock`, network endpoint grants, devices), each selecting what the operation may
   reach.
3. **Capability variables range over sets** of capabilities, the empty set included. A generic body is
   checked parametrically; at each instantiation the exact sets are substituted into its types and
   requirements and the instantiation is rechecked. Inconsistent bindings are refused, never merged
   into a union. No set-bound syntax is added.
4. **Function-pointer types keep their exact capability set** (canonical current rule; OD93 refers here). No conversion changes it. A function
   that calls a callback declares the callback's set plus its own effects, and a context value adds no
   authority. Foreign function values stay refused (OD4); the permissions of foreign callbacks are
   decided when they are admitted.
5. **`trusted` and `with(Unsafe)` never discharge an operational capability** (`Fs`, `Network`, `Env`,
   `Time`, `Alloc`, `Device`, `Console`, `Process`; `Console` added by the [owner revision, 2026-11-05](#decided-2026-11-05-revisions), row Q7.11). Effect checking applies inside trusted bodies and at foreign
   calls; no capability can be hidden. What native code actually does is a named assumption of the
   theorem, never an exemption.
6. **There are no umbrella capabilities** (`Std`, `Io`, `Block`). Behavioural classes such as "may
   block" are a conservative table over the capabilities, which also covers allocation, destruction
   and foreign calls; unknown foreign behaviour counts as "may block". The table never claims that a
   particular call does not block.
7. **Primitive effects come from one exhaustive table** (externs, intrinsics, compiler-generated
   operations, runtime entry, startup and shutdown); compound effects follow checked composition. A
   declaration may never understate the effects of what it calls. A body uses only its declared
   permissions, and a call requires the callee's set to be included in the caller's (TRUST-4), across
   trait dispatch, generic methods and generated cleanup alike; one Lean judgment states this. The table includes the `Console` operations on the standard-stream
   grants (row Q7.11, [owner revision, 2026-11-05](#decided-2026-11-05-revisions)).
8. **Returned values never supply permissions.** A returned `File` or function value still carries its
   ownership and its effect requirements, but no permission.
9. **Cleanup returns what survives it.** Cleanup with effects is explicit (`close(f) with(Fs)`), and
   `Destroy` requires at most `Alloc`. On every returning path, a consuming operation releases its
   input or returns every ownership obligation that remains: a cleanup that can leave a resource live
   returns it in its error. `close` on a `File` consumes the handle in every outcome only through a
   platform adapter that guarantees the descriptor is released (POSIX alone does not); a handle whose
   number may have been reused is never handed back. `defer close(f)` is written with a named function
   that returns `Unit` and handles the error (OD62, OD140, referring to the canonical Unit-only audit rule below); there is no new syntax for errors of
   deferred calls.
10. **Ownership on failure and exit.** Error branches and early returns keep every linear obligation.
    Abort runs no cleanup and a trapping deferred call stops the sequence (OD68, OD69); these are the
    only exceptions.
11. **Resources are introduced only by checked acquisition or unforgeable constructors**, and each
    grant the target creates is minted once. Every extern, intrinsic and minting wrapper is listed in
    the trusted base, with its identity preserved through interfaces, specialization and linking.
    Who may introduce them is decided [below](#decided-2026-11-02-audit).
12. **`main` receives the target's grants and consumes them.** There is no `Root` type. The target
    declares the grants it offers (its allocator, directories, environment, clock, devices); `main`'s
    parameters and `with(...)` must be within them, checked at compile time. `main` consumes every
    grant explicitly before it returns; abort is the exception. Effects outside ordinary calls (the
    trap message, flushing output, startup, shutdown) come from the target's table and appear in the
    semantics and its traces; no source code gains a hidden `Console` or `Alloc`.
13. **Capability names are reserved and differ from resource type names**: `with(Fs)` against a
    `File` value. The reserved set is `Fs`, `Network`, `Env`, `Time`, `Alloc`, `Device`, `Console`, `Process` and
    `Unsafe` (meanings per OD95; `Console` is the permission for operations on the `StdIn`, `StdOut` and
    `StdErr` grants, added by the [owner revision, 2026-11-05](#decided-2026-11-05-revisions), row Q7.11; there is no `Net` spelling and no alias). They are reserved everywhere,
    including import aliases, capability parameters and other binders.
14. **What the theorem claims.** Effect containment, resource safety and information-flow security are
    separate claims with separate assumptions; none implies another. The trusted base is an explicit
    list of assumptions, and being listed proves nothing.

<a id="decided-2026-11-02-allocation"></a>**Allocation** (revises OD83, the former authority rule 10
of 2026-11-01 "one allocator form per target", and the terminal out-of-memory failure of STMT-2):

- **One allocation form on every target**: an allocator value plus `with(Alloc)`. A hosted target hands
  `main` the system allocator; a freestanding target hands it its arenas. There is no allocation by
  permission alone.
- **The common type is `Allocator<A>`**, whose parameter `A` is a fresh, unforgeable identity of one
  allocator domain (`Sys` for the system allocator); an arena is one kind of allocator.
- **Allocations carry their allocator's identity in their type**: `Heap<T, A>`, `Vec<T, A>`, `String<A>`.
  Each records how to release its storage (never a reference to the allocator), so `*h` and
  `destroy(h)` (OD59) need no allocator argument and `Destroy` still requires at most `Alloc`.
  Moving a value to another allocator allocates there explicitly.
- **An allocator can be given up only when nothing allocated from it is alive.** This dependency is a
  language rule that holds across function boundaries, generics, containers and callbacks, and it is
  not implemented by reference counting.
- **Allocation is fallible and conserves its input**:
  `alloc(a: &mut Allocator<A>, v: T) with(Alloc) -> Result<Heap<T, A>, AllocFailure<T>>` returns the
  value on failure. The failure value itself never allocates, and the allocator stays valid after a
  failure. A container whose growth fails is unchanged and returns the rejected element; a failed
  reserve keeps the original storage; an operation that allocates several times accounts for what it
  had already built; a consuming container operation returns the container when an obligation
  survives. Running out of memory is an ordinary error, not a terminal failure. Allocating string operations
  (OD154) take an allocator and report failure; static text needs no allocation.

<a id="decided-2026-11-02-filesystem"></a>**Filesystem, environment, time and network** (revises
the former authority rule 9 of 2026-11-01 on `get_args`, and OD156):

- **Every filesystem namespace operation takes a `Directory` value plus `with(Fs)`**: opening,
  creating, listing, probing, removing, renaming, linking and metadata lookup. Renaming and linking need
  the authority of both directories involved. A file that is opened owns its own handle, which borrows
  nothing from the directory; operations on an open file take that file. `main` receives the
  directories the target grants; a directory value can be narrowed (for example to read-only) and
  handed to a library alone.
- **A path stays inside its directory**, enforced during lookup: absolute paths, `..` components and
  symbolic links anywhere in the path are refused, confinement is enforced by the lookup primitive
  itself (never by checking a path and then opening it), and the mount-crossing policy is stated. A
  target without such a primitive refuses filesystem access.
- **The environment and the clock are grants**: an `Environment` value selects which variables and
  arguments are visible, a `Clock` value which clock and what precision. `get_args` takes the
  `Environment` and `with(Env)`, and an allocator when it allocates.
- **Network**: connecting needs a grant naming the permitted destinations, and binding or listening a
  grant naming the endpoint, each plus `with(Network)`; accepting takes the listener, and communicating
  takes the connected socket, still with `with(Network)` (holding a socket supplies no permission).
  Name resolution (DNS) needs its own stated authority contract.

<a id="decided-2026-11-02-audit"></a>**Decided 2026-11-02, the questions the spec audit raised** (same rule: Claude's proposal
and Codex's review agreed, Codex's refinement adopted where it gave one):

- **Allocator reset** is allowed under the condition for giving an allocator up: it consumes the
  allocator, requires `with(Alloc)` and no live allocation or conflicting loan from it, and returns an
  allocator whose identity the operation generates fresh (never chosen by the caller). How fresh
  identities are introduced and scoped in Core, including repeated resets in a loop, is part of the
  target Core proposal.
- **Who introduces primitives.** Any library may declare externs, in trusted code, each listed in the
  trusted base with an accurate primitive contract. Only the compiler introduces intrinsics. Only the
  target runtime mints the initial grants; ordinary libraries may acquire or narrow authority only
  from grants they already hold, and an extern never bypasses this.
- **What "every operation" covers** (authority rule 2): the operations on resources. Pure computation
  and checked traps need neither a value nor a permission. `abort()` is the same terminal trap and
  needs neither (this revises STMT-13, which required `Process`). The target's diagnostics and output
  flushing stay accounted for in traces and target assumptions, and `Unsafe` keeps its own validity
  requirements.
- **`main` declares the grants it wants**, as parameters, out of those the target offers; a grant it
  does not declare is never minted, and it consumes every grant it declares.
- **Opening a heap.** `&h` gives `&Heap<T, A>`, a reference to the heap's shell. A borrow block opens
  exactly one heap layer; opening mutably needs exclusive access, and opening through a shell
  reference keeps that reference's root and lifetime and never upgrades shared access. A `HeapArray`
  exposes a bounded element view, never an element `&T` or an extra heap layer; the view's exact type
  is part of the target Core proposal.
- **Qualified trait calls** (syntax completion of the canonical [OD6](#od6) rule): an ambiguous method call is written `Trait::method(&x, ...)`,
  a path call resolved by the trait's identity, and is refused otherwise; qualification never relaxes
  coherence.
- **Traits cross modules like any other declaration** (owner, 2026-11-02): a `pub` trait is exported,
  and another module imports it explicitly (OD139) to implement it, bound by it or call
  it qualified as `Trait::method(&x, ...)`.
- **Mounts and name resolution.** A lookup that crosses a mount point is refused. Name resolution
  (DNS) takes a resolver grant naming the permitted service, query scope and transport, plus
  `with(Network)`, and an allocator when it allocates; resolving grants no connection, and connecting
  to a returned address still needs the destination grant.
- **`defer` takes a call whose result is `Unit`** (canonical current rule; revised OD140 and authority rule 9 refer here). Any other call is wrapped in a named
  function that returns `Unit` and consumes a linear result, handles a `Result` or `Option`, or
  discards an eligible Copy result; there is no discard form inside `defer`.
- **A projection base is a place**: a local, or a projection through a place. A call result is bound
  with `let` before it is projected.
- **Deferred arguments.** A by-value argument of a deferred call is reserved and read when the call runs
  (OWN-12, kept by OD2); a borrowed argument is a loan held until the call runs (OWN-16). Both
  restrictions apply together. Allocator dependencies are checked when the deferred call runs: an
  allocation may still be alive at registration only if the cleanup removes it before its allocator is
  consumed.
- **`_` names an unused binding.** It never discards a linear value or a `Result` or `Option` (OD62);
  those are consumed or handled explicitly.
- **Postconditions are partial correctness over normal returns.** A trap is not a return, so `ensures`
  says nothing about trapping or diverging executions, and a proved postcondition is never reported as
  freedom from traps.
- **What a function contract sees.** `requires` and `ensures` see the function's parameters, `result`
  (in `ensures` only) and the declarations they may name, such as specification functions; never the
  body's locals. `requires` reads the parameters at entry; `ensures` reads the parameters' surviving
  state after every deferred call (OD144), with no implicit entry snapshot, and a parameter the body
  consumed is not visible. Loop clauses and body assertions use their own lexical scope, permitted
  ghost locals included.
- **User capability aliases** (`cap IO = ...`) are removed: they are umbrella capabilities by another
  name. Capability parameters remain.

**Decided 2026-10-31 by the owner's rule** (every reviewer agreed, and the answer is as strict as
Austral or stricter):

- <a id="OPEN-33"></a>**OPEN-33** (OD33, Inline submodules under one file per module): See [OD33](#od33).
- <a id="OPEN-43"></a>**OPEN-43** (OD43, How a type position resolves a name): See [OD43](#od43).
- <a id="OPEN-44"></a>**OPEN-44** (OD44, Two packages with one module name): See [OD44](#od44).
- <a id="OPEN-46"></a>**OPEN-46** (OD46, Generic type-argument inference): See [OD46](#od46).
- <a id="OPEN-48"></a>**OPEN-48** (OD48, The index type of a counted loop over two literals): See [OD48](#od48).
- <a id="OPEN-50"></a>**OPEN-50** (OD50, Reserved or contextual `from` and `until`): See [OD50](#od50).
- <a id="OPEN-51"></a>**OPEN-51** (OD51, Reverse, stepped and collection iteration): See [OD51](#od51).
- <a id="OPEN-52"></a>**OPEN-52** (OD52, `if let`, `while let` and `let ... else`): See [OD52](#od52).
- <a id="OPEN-53"></a>**OPEN-53** (OD53, Struct update `..base`): See [OD53](#od53).
- <a id="OPEN-54"></a>**OPEN-54** (OD54, `newtype` and `.0`): See [OD54](#od54).
- <a id="OPEN-55"></a>**OPEN-55** (OD55, Match guards): See [OD55](#od55).
- <a id="OPEN-56"></a>**OPEN-56** (OD56, Range patterns): See [OD56](#od56).
- <a id="OPEN-58"></a>**OPEN-58** (OD58, Borrow blocks stay): See [OD58](#od58).
- <a id="OPEN-62"></a>**OPEN-62** (OD62, Unused values and variables): See [OD62](#od62).
- <a id="OPEN-63"></a>**OPEN-63** (OD63, Float literals take their expected type): See [OD63](#od63).
- <a id="OPEN-64"></a>**OPEN-64** (OD64, Raw pointers derived from a borrow): See [OD64](#od64).
- <a id="OPEN-65"></a>**OPEN-65** (OD65, Empty structs): See [OD65](#od65).
- <a id="OPEN-66"></a>**OPEN-66** (OD66, The unit literal `()`): See [OD66](#od66).
- <a id="OPEN-67"></a>**OPEN-67** (OD67, Locals that reuse a top-level name): See [OD67](#od67).
- <a id="OPEN-68"></a>**OPEN-68** (OD68, Failure inside a deferred call): See [OD68](#od68).
- <a id="OPEN-69"></a>**OPEN-69** (OD69, What "the remaining locals are destroyed" means): See [OD69](#od69).
- <a id="OPEN-70"></a>**OPEN-70** (OD70, Struct fields with no comma): See [OD70](#od70).
- <a id="OPEN-71"></a>**OPEN-71** (OD71, Enum variants with no comma): See [OD71](#od71).
- <a id="OPEN-73"></a>**OPEN-73** (OD73, Reserved words used as identifiers): See [OD73](#od73).
- <a id="OPEN-75"></a>**OPEN-75** (OD75, An empty type parameter list): See [OD75](#od75).
- <a id="OPEN-84"></a>**OPEN-84** (OD84, Struct destructuring `let` in the grammar): See [OD84](#od84).
- <a id="OPEN-85"></a>**OPEN-85** (OD85, Variant destructuring with `else` in the grammar): See [OD85](#od85).
- <a id="OPEN-90"></a>**OPEN-90** (OD90, `struct Copy <T>` and contextual keywords): See [OD90](#od90).
- <a id="OPEN-100"></a>**OPEN-100** (OD100, Comparisons on a type parameter): See [OD100](#od100).
- <a id="OPEN-101"></a>**OPEN-101** (OD101, What a trap writes): See [OD101](#od101).
- <a id="OPEN-103"></a>**OPEN-103** (OD103, `as` between reference types): See [OD103](#od103).
- <a id="OPEN-107"></a>**OPEN-107** (OD107, What a float-to-integer rounding gives for NaN, infinities and out-of-range values): See [OD107](#od107).
- <a id="OPEN-110"></a>**OPEN-110** (OD110, Enums at the foreign boundary): See [OD110](#od110).
- <a id="OPEN-112"></a>**OPEN-112** (OD112, Mutation through a shared reference): See [OD112](#od112).
- <a id="OPEN-113"></a>**OPEN-113** (OD113, The ownership exceptions of trusted code): See [OD113](#od113).
- <a id="OPEN-114"></a>**OPEN-114** (OD114, How integer division rounds): See [OD114](#od114).
- <a id="OPEN-118"></a>**OPEN-118** (OD118, A proven run-time violation in trusted code): See [OD118](#od118).
- <a id="OPEN-119"></a>**OPEN-119** (OD119, Buffered output when the process aborts): See [OD119](#od119).
- <a id="OPEN-121"></a>**OPEN-121** (OD121, Projections from a base that is not a traceable place): See [OD121](#od121).
- <a id="OPEN-127"></a>**OPEN-127** (OD127, UTF-8 in every `String`): See [OD127](#od127).
- <a id="OPEN-130"></a>**OPEN-130** (OD130, Unlabeled pattern binders: by position or by name): See [OD130](#od130).
- <a id="OPEN-131"></a>**OPEN-131** (OD131, `union` declarations): See [OD131](#od131).
- <a id="OPEN-136"></a>**OPEN-136** (OD136, std's `process_exit`): See [OD136](#od136).
- <a id="OPEN-137"></a>**OPEN-137** (OD137, Character literals outside ASCII): See [OD137](#od137).
- <a id="OPEN-139"></a>**OPEN-139** (OD139, A qualified call to a function that is not imported): See [OD139](#od139).
- <a id="OPEN-148"></a>**OPEN-148** (OD148, Type parameters on a trait): See [OD148](#od148).

**Decided 2026-11-01 by the owner** (the final set, reviewed by Claude, Codex and Grok, checked
against Austral and the no-duplication rule):

- <a id="OPEN-34"></a>**OPEN-34** (OD34, A struct literal inside brackets in a condition head): See [OD34](#od34).
- <a id="OPEN-35"></a>**OPEN-35** (OD35, How an array satisfies a `Destroy` bound): See [OD35](#od35).
- <a id="OPEN-36"></a>**OPEN-36** (OD36, The explicit place form that opens a heap cell): See [OD36](#od36).
- <a id="OPEN-37"></a>**OPEN-37** (OD37, The names of the pointer difference and ordering functions): See [OD37](#od37).
- <a id="OPEN-38"></a>**OPEN-38** (OD38, The root resource API): See [OD38](#od38).
- <a id="OPEN-39"></a>**OPEN-39** (OD39, The interface artifact format): See [OD39](#od39).
- <a id="OPEN-40"></a>**OPEN-40** (OD40, PAT-9 and the fallback arm of a desugared form): See [OD40](#od40).
- <a id="OPEN-41"></a>**OPEN-41** (OD41, PAT-9 and integer ranges that span their type): See [OD41](#od41).
- <a id="OPEN-42"></a>**OPEN-42** (OD42, Comparisons on `bool` and `char`): See [OD42](#od42).
- <a id="OPEN-45"></a>**OPEN-45** (OD45, Capability-variable inference): See [OD45](#od45).
- <a id="OPEN-49"></a>**OPEN-49** (OD49, The arithmetic of a counted loop's termination measure): See [OD49](#od49).
- <a id="OPEN-57"></a>**OPEN-57** (OD57, The region name of a borrow block): See [OD57](#od57).
- <a id="OPEN-59"></a>**OPEN-59** (OD59, One way to consume a `Heap`): See [OD59](#od59).
- <a id="OPEN-60"></a>**OPEN-60** (OD60, One consuming receiver form for `Destroy`): See [OD60](#od60).
- <a id="OPEN-61"></a>**OPEN-61** (OD61, One coordinated removal list): See [OD61](#od61).
- <a id="OPEN-72"></a>**OPEN-72** (OD72, An enum with no variants): See [OD72](#od72).
- <a id="OPEN-74"></a>**OPEN-74** (OD74, A raw pointer type with no qualifier): See [OD74](#od74).
- <a id="OPEN-76"></a>**OPEN-76** (OD76, Unqualified struct patterns in `match`): See [OD76](#od76).
- <a id="OPEN-77"></a>**OPEN-77** (OD77, Resync the grammar to the decided language): See [OD77](#od77).
- <a id="OPEN-78"></a>**OPEN-78** (OD78, `mod X;` in the grammar): See [OD78](#od78).
- <a id="OPEN-79"></a>**OPEN-79** (OD79, `assert` in the grammar): See [OD79](#od79).
- <a id="OPEN-80"></a>**OPEN-80** (OD80, `assume` in the grammar): See [OD80](#od80).
- <a id="OPEN-81"></a>**OPEN-81** (OD81, `ghost let` in the grammar): See [OD81](#od81).
- <a id="OPEN-82"></a>**OPEN-82** (OD82, Loop contract attributes and where attributes attach): See [OD82](#od82).
- <a id="OPEN-83"></a>**OPEN-83** (OD83, The call-site allocator binding in the grammar): See [OD83](#od83).
- <a id="OPEN-86"></a>**OPEN-86** (OD86, The body of an unbraced match arm): See [OD86](#od86).
- <a id="OPEN-87"></a>**OPEN-87** (OD87, Capability arguments in an expression turbofish): See [OD87](#od87).
- <a id="OPEN-88"></a>**OPEN-88** (OD88, The shape of capability arguments): See [OD88](#od88).
- <a id="OPEN-89"></a>**OPEN-89** (OD89, Capability parameters on a trait impl): See [OD89](#od89).
- <a id="OPEN-91"></a>**OPEN-91** (OD91, A cast followed by `<`): See [OD91](#od91).
- <a id="OPEN-92"></a>**OPEN-92** (OD92, A capability set inferred from callbacks): See [OD92](#od92).
- <a id="OPEN-93"></a>**OPEN-93** (OD93, Widening a function value's capability set): See [OD93](#od93).
- <a id="OPEN-95"></a>**OPEN-95** (OD95, What `Time`, `Env`, `Process` and `Alloc` mean): See [OD95](#od95).
- <a id="OPEN-96"></a>**OPEN-96** (OD96, Two foreign bindings of one C symbol): See [OD96](#od96).
- <a id="OPEN-97"></a>**OPEN-97** (OD97, A build profile that omits or demands an overflow check): See [OD97](#od97).
- <a id="OPEN-98"></a>**OPEN-98** (OD98, Pointer and reference conversions written with `as`): See [OD98](#od98).
- <a id="OPEN-99"></a>**OPEN-99** (OD99, The names of the named conversions): See [OD99](#od99).
- <a id="OPEN-102"></a>**OPEN-102** (OD102, `Float32` and `Char` as type spellings): See [OD102](#od102).
- <a id="OPEN-104"></a>**OPEN-104** (OD104, A `#[variant]` written on a counted loop): See [OD104](#od104).
- <a id="OPEN-105"></a>**OPEN-105** (OD105, Where a counted loop's `#[invariant]` is checked): See [OD105](#od105).
- <a id="OPEN-106"></a>**OPEN-106** (OD106, The result type of a checked conversion): See [OD106](#od106).
- <a id="OPEN-108"></a>**OPEN-108** (OD108, A capability listed twice in a set): See [OD108](#od108).
- <a id="OPEN-109"></a>**OPEN-109** (OD109, Specializations that differ only in a function type's capabilities): See [OD109](#od109).
- <a id="OPEN-111"></a>**OPEN-111** (OD111, Misaligned fields of a packed struct): See [OD111](#od111).
- <a id="OPEN-115"></a>**OPEN-115** (OD115, The body and totality of a specification function): See [OD115](#od115).
- <a id="OPEN-116"></a>**OPEN-116** (OD116, A checked total function category): See [OD116](#od116).
- <a id="OPEN-117"></a>**OPEN-117** (OD117, `old`, quantifiers and implication in contracts): See [OD117](#od117).
- <a id="OPEN-120"></a>**OPEN-120** (OD120, A vacuous contract): See [OD120](#od120).
- <a id="OPEN-122"></a>**OPEN-122** (OD122, A `repr(C)` struct returned from an extern function): See [OD122](#od122).
- <a id="OPEN-123"></a>**OPEN-123** (OD123, `as` on a newtype outside its module): See [OD123](#od123).
- <a id="OPEN-124"></a>**OPEN-124** (OD124, Profiles in the specification): See [OD124](#od124).
- <a id="OPEN-125"></a>**OPEN-125** (OD125, Contracts at run time): See [OD125](#od125).
- <a id="OPEN-126"></a>**OPEN-126** (OD126, Target assumptions): See [OD126](#od126).
- <a id="OPEN-128"></a>**OPEN-128** (OD128, `->` between a match arm's pattern and body): See [OD128](#od128).
- <a id="OPEN-129"></a>**OPEN-129** (OD129, Type arguments of an enum literal taken from its expected type): See [OD129](#od129).
- <a id="OPEN-132"></a>**OPEN-132** (OD132, Polymorphic recursion): See [OD132](#od132).
- <a id="OPEN-133"></a>**OPEN-133** (OD133, User functions named `print`, `println` or `append`): See [OD133](#od133).
- <a id="OPEN-134"></a>**OPEN-134** (OD134, The capabilities of a container's generated destruction): See [OD134](#od134).
- <a id="OPEN-135"></a>**OPEN-135** (OD135, A method's `Type_method` name in its module's namespace): See [OD135](#od135).
- <a id="OPEN-138"></a>**OPEN-138** (OD138, Which positions give a literal its expected type): See [OD138](#od138).
- <a id="OPEN-140"></a>**OPEN-140** (OD140, Implicitly dropped results of `free` and of a deferred call): See [OD140](#od140).
- <a id="OPEN-141"></a>**OPEN-141** (OD141, More than one `main` in a program): See [OD141](#od141).
- <a id="OPEN-142"></a>**OPEN-142** (OD142, `X.con` beside `X/mod.con`): See [OD142](#od142).
- <a id="OPEN-143"></a>**OPEN-143** (OD143, What a contract may read): See [OD143](#od143).
- <a id="OPEN-144"></a>**OPEN-144** (OD144, Postconditions and deferred calls): See [OD144](#od144).
- <a id="OPEN-145"></a>**OPEN-145** (OD145, How a contract names machine arithmetic): See [OD145](#od145).
- <a id="OPEN-146"></a>**OPEN-146** (OD146, The result type of a test function): See [OD146](#od146).
- <a id="OPEN-147"></a>**OPEN-147** (OD147, `repr(C, packed)` together): See [OD147](#od147).
- <a id="OPEN-149"></a>**OPEN-149** (OD149, How `[e; N]` evaluates `e`): See [OD149](#od149).
- <a id="OPEN-150"></a>**OPEN-150** (OD150, Which text governs the grammar until it is resynchronized): See [OD150](#od150).
- <a id="OPEN-151"></a>**OPEN-151** (OD151, Module paths of more than one level): See [OD151](#od151).
- <a id="OPEN-152"></a>**OPEN-152** (OD152, Whether a path's first letter decides its syntactic form): See [OD152](#od152).
- <a id="OPEN-153"></a>**OPEN-153** (OD153, Program functions named like a builtin): See [OD153](#od153).
- <a id="OPEN-154"></a>**OPEN-154** (OD154, Whether the string builtins require `Alloc`): See [OD154](#od154).
- <a id="OPEN-155"></a>**OPEN-155** (OD155, `unwrap` beside TYPE-24's conversions): See [OD155](#od155).
- <a id="OPEN-156"></a>**OPEN-156** (OD156, The capability of `get_args`): See [OD156](#od156).
- <a id="OPEN-157"></a>**OPEN-157** (OD157, How the type `Unit` is written): See [OD157](#od157).

**Decided 2026-10-29:**

- **OD47** (loops, decided 2026-10-29, never an open entry): `while` and the counted loop `for i from a
  until b`, the C-style `for` removed. Now STMT-6@3, STMT-11@2 and STMT-12 (pending, bug 331); OD47 left OPEN-48 to
  OPEN-51.
- **OD23 – OD32** (the strictness pass, decided 2026-10-29, never open entries): REF-11@3 (OD23),
  STMT-7@3 (OD24, revised 2026-11-02 to [`u8` only](#decided-2026-11-02-main)), PAT-3@3 and PAT-5@3 (OD25), TYPE-6@2 (OD26), STMT-6@2 (OD27), TRAIT-10@2 (OD28),
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
  tokens. Now TRUST-8, TRUST-9 and TRUST-10 of [trusted.md](#TRUST-8) (pending, R-0557).

- **OPEN-20** (OD20, the visibility of std's `std.libc` bindings): decided 2026-10-29, private to std.
  Now MOD-6 of [modules.md](#MOD-6) (enforced by bug 226's fix).
- **OPEN-21** (OD21, a parent module's rights over its child): decided 2026-10-29, only the declaring
  module. Now TYPE-11 of [types.md](#TYPE-11), enforced as written for a parent too (bug
  311's fix).
- **OPEN-22** (OD22, trust on an `impl` block): decided 2026-10-29, per method only. Now TRUST-5@2 of
  [trusted.md](#TRUST-5).
