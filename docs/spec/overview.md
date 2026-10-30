# Overview

Concrete is a small systems language for software that must be reviewed and proved. Its values are
linear: a value that owns a resource is consumed exactly once, and nothing is destroyed implicitly.
References are scoped access, never stored or returned. A function's authority is written in its
signature (`with(File, Console)`), arithmetic is checked, and failure is an abort. Its compiler is
written in Lean 4 and is being built toward an end-to-end verified pipeline. The design follows
Austral and departs from it only with a written reason.

## How to read this specification

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
(`Semantics:`); elsewhere the sentence is the rule until the definition lands. The owner's open
questions are the last chapter, in the order to read them. The whole specification is also one
generated page, [SPEC.md](SPEC.md), and how it is maintained (format, registry, gates) is
[MAINTAINERS.md](MAINTAINERS.md).

## Chapters

The chapters in reading order. The prefix is the one their entry IDs carry.

| chapter | prefix | covers |
|---|---|---|
| [Overview](overview.md) | `LANG-` | what Concrete is, how to read this specification, the language's invariants |
| [Syntax](syntax.md) | `SYN-` | the grammar, operator precedence, reserved spellings, struct literals in conditions |
| [Modules and Names](modules.md) | `MOD-` | name resolution, visibility, module files, imports, interfaces |
| [Scope and Binding](scope.md) | `SCOPE-` | where a binding is visible, no shadowing |
| [Types](types.md) | `TYPE-` | the types, typing of expressions and calls, aggregates, generics, capability sets in types |
| [Arithmetic](arithmetic.md) | `ARITH-` | integers and floats, literals, checked arithmetic, conversions, comparisons |
| [Statements, Expressions and Execution](statements.md) | `STMT-` | outcomes, evaluation order, loops, `main`, the exit sequence, failure |
| [Patterns](patterns.md) | `PAT-` | `match`, exhaustiveness, destructuring `let` |
| [Ownership](ownership.md) | `OWN-` | linearity, Copy, discards, assignment, `defer`, destruction |
| [References](references.md) | `REF-` | forming, passing and borrowing references, loans |
| [Memory](memory.md) | `MEM-` | ownership states, initialization, field access, `Heap`, array bounds |
| [Traits](traits.md) | `TRAIT-` | traits, impls, bounds, coherence |
| [Contracts](contracts.md) | `CON-` | `requires`, `ensures`, invariants, ghost code |
| [Trusted Code and Raw Pointers](trusted.md) | `TRUST-` | capabilities, `trusted`, raw pointers, global resources |
| [Foreign Functions](ffi.md) | `FFI-` | `extern`, layout, symbols |
| [Pipeline Preservation](pipeline.md) | `PASS-` | what each compiler stage preserves |
| [Open Owner Decisions](decisions.md) | `OPEN-` | the questions only the project owner decides |

## The language's invariants

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
| No exceptions | Errors are values, propagated by an exhaustive `match` and an explicit `return`; failure is abort only. | STMT-2, "Recoverable failure" in statements.md |
| No global mutable state | A program holds no mutable global state; global resources are linear tokens descending from the root value `main` receives (OD8, decided 2026-10-29). | TRUST-8, TRUST-9, TRUST-10 (pending) |
| No interior mutability in safe code | All mutation flows through `&mut`; a later low-level `UnsafeCell<T>` would be gated by `Unsafe`. | open: OPEN-112 |
| Local-only type inference | Signatures are fully annotated; inside a body a local's type may come from its initializer, right to left only, with no cross-statement constraint solving and no broad bidirectional inference. | TYPE-4 (in part); gap |
| LL(1) grammar | Every parsing decision takes one token of lookahead, with no ambiguity and no backtracking. | SYN-1 |
| `abort()` is terminal | No deferred call, `Destroy` implementation or drop glue runs after an abort; hosted execution ends the process (usually status 134 on POSIX, so tests check for a nonzero status), and a freestanding profile calls a declared non-returning handler (R-0223, R-0322). | STMT-2, "Abort and failure" in statements.md |

**Project and compiler commitments** that are not rules of the language but are kept with it:

- **Readability is part of correctness.** Surface syntax, std APIs and trusted boundaries stay
  explicit enough that ordinary code reads and audits predictably; a feature that hides low-level
  behaviour raises audit cost and is a correctness concern, not style.
- **New features justify their cost** in grammar, audit and proof; one that makes the language
  harder to parse, review or prove without a correspondingly large benefit is rejected or delayed
  ([design policy](../project/PROJECT.md#design-policy)).
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
