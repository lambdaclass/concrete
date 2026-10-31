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
The terms this specification uses are in the [glossary](#glossary) at the end of this chapter, and
the entries that govern each construct are in [rules by construct](#rules-by-construct).

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

## Glossary

This section is explanatory and states no rule. Each term points at the entry or section that fixes
it; where an entry defines the term, that entry is the definition and this table only finds it.

| term | meaning in this specification | defined or governed by |
|---|---|---|
| entry | an item with an ID, a versioned normative sentence and fields, of one of the kinds below | "How to read this specification" above, [MAINTAINERS.md](MAINTAINERS.md) §2.1 |
| refusal, guarantee, definition, preservation, open | the kinds an entry may have; the `Kind:` field of MAINTAINERS.md §2.1 defines each | MAINTAINERS.md §2.1 (`Kind:`) |
| open entry | an entry of kind `open`: a question only the project owner decides; it encodes no answer | [decisions.md](decisions.md), MAINTAINERS.md §2.1 |
| enforced, partial, pending | an entry's status: implemented; holding on its examples that are not pending examples, with the pending examples tracked separately; decided and not implemented | MAINTAINERS.md §2.1 (`Status:`) |
| pending example | an example of an entry not yet implemented, run with `now=` recording what the compiler does today; it never counts as a pass | MAINTAINERS.md §2.2 |
| consumer | a program that runs a checked program: the native binary, `--interp`, and Core's executable semantics once it exists | MAINTAINERS.md §2.2, PASS-13 |
| profile | a way the compiler produces an executable: the native profile and the LLVM profile | PASS-3, PASS-4, OPEN-124 |
| Core | the compiler's internal language after elaboration; MAINTAINERS.md ("Authority") states the standing of its Lean semantics | MAINTAINERS.md ("Authority"), [pipeline.md](pipeline.md) |
| Check | the stage that checks the surface program: types, ownership, references, capabilities | PASS-5 |
| Elab | elaboration: the stage that turns the checked program into Core, desugaring as it goes | PASS-6 |
| CoreCheck | the stage that checks Core before it proceeds | PASS-8 |
| Mono | monomorphization: the stage that specializes every generic at its type arguments | PASS-1, PASS-2, PASS-7 |
| Lower, Emit | the stages that turn Core into verified SSA and SSA into LLVM IR | PASS-9, PASS-10, PASS-11 |
| builtin | a function, type or enum the compiler provides with no declaration in the program | functions: the [builtin catalog](types.md#builtin-functions), TYPE-10; primitive types: TYPE-19, TYPE-20; `Option` and `Result`: TYPE-23; names: MOD-1 |
| intrinsic | an operation the compiler implements, named by a builtin or by `#[intrinsic = "n"]` | FFI-6, the builtin catalog |
| place | a root binding followed by field, index or newtype projections | REF-5 |
| owned value | a value a local binding holds by value rather than through a reference; MEM-D1 names its states | MEM-D1 |
| Copy | a type whose values may be duplicated | OWN-D1, TYPE-14, OWN-14 |
| linear | a type that is not Copy (OWN-D1); OWN-1 and OWN-2 govern how its values are used | OWN-D1, OWN-1, OWN-2 |
| consume, move | the uses of a value that OWN-15 lists as moves (passing it by value, returning it, moving it into a field, destructuring it and others); MEM-D1 and OWN-1 govern the binding's state afterwards, and reading or moving a Copy binding leaves that binding's state unchanged (MEM-D1) | OWN-15, MEM-D1, OWN-1 |
| reserved, frozen | two binding states MEM-D1 names: reserved is a binding passed by value to a pending `defer` (OWN-12), frozen is the owner of an enclosing borrow block (REF-D3) | MEM-D1, OWN-12, REF-D3 |
| reference | a value of type `&T` or `&mut T`; the entries cited govern where it may flow | REF-D1, REF-D2, REF-1, REF-3, REF-5 |
| borrow | forming a reference to a place (`&x`, `&mut x`), or a borrow block | REF-5, REF-D3 |
| borrow block | `borrow x as r in R { B }`, which lends `x` to `r` for `B` | REF-D3 |
| loan | a borrow seen from its root binding | REF-11, REF-8, STMT-11 |
| capability | a named logical authority, such as `Console` or `Alloc` | TRUST-D1, TRUST-4 |
| capability set | the capabilities and capability variables of a `with(...)` or a capability argument | TYPE-D1, TYPE-17, TYPE-18 |
| `Unsafe` | the capability for raw-memory and foreign operations | TRUST-D1, TRUST-4 |
| trusted | a `trusted fn` (TRUST-5, TRUST-12) or a `trusted mod` (TRUST-6, pending); "Trusted premises" collects what they leave unchecked | TRUST-5, TRUST-6, TRUST-12; "Trusted premises" in [trusted.md](trusted.md) |
| premise | a fact the compiler relies on and does not check; "Trusted premises" lists each and who states it | TRUST-2, "Trusted premises" in trusted.md |
| abort, trap | the ending of a process by `abort()` or by a failed run-time check; the entries cited govern both | STMT-2, STMT-13, ARITH-3, MEM-6 |
| behaviour | the trace of external events of one execution with its ending | STMT-1 |
| exit sequence | the sequence STMT-11 (pending) describes for a scope exit that does not abort | STMT-11 |
| visible | where a local binding may be named | SCOPE-D1 |
| impl target | the type an impl is for | TRAIT-D1 |
| ghost | code and bindings for contracts only, erased before code generation | CON-4, CON-5 |

## Rules by construct

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
| `borrow x as r in R { B }` | REF-D3, REF-8, OPEN-57, OPEN-58 |
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
| `alloc`, `free`, `Heap<T>` | MEM-5, MEM-7 |
| `destroy`, `Destroy` | OWN-13, TRAIT-8, TRAIT-11 |
| `discard(e)` | OWN-5, OWN-7 |
| `abort()` | STMT-13, STMT-2 |
| other builtins | TYPE-10 for argument checking; the [builtin catalog](types.md#builtin-functions) names the governing entries of each |
| `mod`, `import`, `pub` | MOD-2, MOD-4, MOD-7, MOD-9 |
| names and `::`, `.` | MOD-1, MOD-3, SYN-10, SCOPE-1 to SCOPE-3 |
| `#[requires]`, `#[ensures]`, `#[invariant]`, `#[variant]` | CON-1, CON-2, CON-3, CON-7, CON-8 |
| `assert`, `assume` | CON-6 |
| `ghost let`, `spec fn` | CON-4, CON-5 |

The explanatory section "Program execution" in [statements.md](statements.md) follows `fn main`
from source files to process exit and names the entries for each step.
