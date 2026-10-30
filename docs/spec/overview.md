# Overview: the language's invariants

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
