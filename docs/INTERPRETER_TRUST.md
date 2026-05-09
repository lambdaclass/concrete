# Interpreter trust boundary

Status: design + reference. Pinned to `Concrete/Interp.lean`.

This document defines the trust boundary of the source-level interpreter
that is invoked via `concrete <file.con> --interp` and used as the
oracle in the Phase A.1 differential harness (`tests/oracle/`).

It answers four questions, in order:

1. What does the interpreter run?
2. What does it explicitly refuse to run?
3. What memory / UB / arithmetic assumptions does it make?
4. Why is it intentionally smaller and more trustworthy than the full
   compiler, and what does that earn us?

The interpreter is **not** a reference implementation of the full
language. It is a small evaluator over validated Core IR that we trust
as the source-level intent for the predictable subset. Programs outside
that subset must be rejected by the interpreter with an explicit
`interp: ...` diagnostic, never silently approximated.

## 1. What the interpreter runs

The interpreter operates on the `CModule` / `CExpr` / `CStmt` IR
produced by the frontend after parsing, name resolution, type checking,
elaboration, and `CoreCheck`. It does not run monomorphization,
lowering, SSA emission, LLVM, clang, or any linker.

### Supported expression forms

- Integer literals, bool literals
- Identifiers (variable lookup against a flat `(name, value)` list)
- Binary operators: `+ - * / %`, `== != < > <= >=`, `&& ||`,
  bitwise `^ & | << >>`
- Unary operators: `-` (neg), `!` (not), `~` (bitnot)
- Casts between integer types and bool→int (`true→1`, `false→0`)
- Function calls (named, by-value or by-ref)
- Struct literals and field access (including auto-deref when the
  receiver is a borrow)
- Enum literals and enum match (with field bindings)
- Match expressions over int/bool literals, variable patterns, wildcard `_`,
  and enum variants
- Array literals, array index reads (with auto-deref when the receiver
  is a borrow)
- `if` / `else` expressions, with scope restoration so block-local
  `let` bindings do not leak past the branch
- `&` and `&mut` borrows of locals, fields, and array elements;
  borrow targets capture a path (base + field/index steps) plus the
  env frame depth at borrow time so callee shadowing cannot redirect
  a borrow to the wrong binding

### Supported statements

- `let` declarations (block-scoped)
- Assignment to a local
- Field assignment, when the receiver is a plain identifier (owned
  struct or borrow of a struct)
- Array index assignment, when the receiver is a plain identifier
  (owned array or borrow of an array)
- Deref assignment `*r = e` through a mutable borrow
- `return` (with or without value)
- `if` / `else` with branch-local scope and outer-variable mutation
  preserved
- `while` loops, with `break` and `continue`, bounded by a fuel counter
  of 10,000,000 iterations
- Statement-level `match` (mutations to outer variables persist; arm-local
  bindings are scoped out)
- `borrow var as ref in 'region { body }` — named borrow regions
  bind a borrow for the body and drop it on exit; mutations through
  the borrow survive

### Supported entry contract

The harness contract is `fn main() -> Int`. The interpreter:

- Looks up `main`, evaluates its body, expects a `return <int>`, and
  returns that integer to `Main.lean`.
- Other return shapes (`bool`, `unit`, struct, enum, array) currently
  collapse to exit code `0`. Programs whose `main` returns those types
  are not on the oracle harness contract and must not be added to
  `tests/oracle/vectors.txt` until the contract is widened
  deliberately.

## 2. What the interpreter refuses to run

These constructs raise an explicit `interp: ...` diagnostic. The
diagnostic is the contract — it is the marker the harness reads to
classify a vector as PENDING rather than FAIL.

| Construct | Diagnostic |
|---|---|
| String literal | `interp: string literals not yet supported` |
| Char literal | `interp: char literals not yet supported` |
| Float literal | `interp: float literals not yet supported` |
| Function reference | `interp: function references not yet supported` |
| `?` (try) | `interp: try expressions not yet supported` |
| Heap `alloc` | `interp: alloc expressions not yet supported` |
| `while` as expression | `interp: while expressions not yet supported` |
| `defer` | `interp: defer not yet supported` |
| Borrow target shape (literal-borrow etc.) | `interp: unsupported borrow target shape` |
| Deref-assign through immutable ref | `interp: deref-assign through immutable ref` |
| Field-assign on non-ident | `interp: field assign on non-ident expression` |
| Array index-assign on non-ident | `interp: array index assign on non-ident expression` |
| Negative array index | `interp: negative array index <i>` |
| Out-of-bounds index | `interp: array index <i> out of bounds (size <n>)` |
| Division / modulo by zero | `interp: division by zero` / `interp: modulo by zero` |
| Loop runaway (>10M iters) | `interp: loop exceeded maximum iterations (10000000)` |

Adding a new "not yet supported" branch is fine; **silently widening
support without the test corpus exercising the new construct is not**.
Every supported construct must have at least one passing oracle
vector.

## 3. Memory, UB, and arithmetic assumptions

The interpreter is a value-passing tree walker. It has no concept of
memory addresses, aliasing, lifetimes, or borrows. Every value is owned
and copied semantically.

### Values are flat ADTs

`IVal` is `int | bool | struct | enum | array | unit | ref`. Structs
and arrays are represented by their fields/elements directly. Refs
are paths (base name + field/index steps + the env frame depth at
borrow time + a mut flag) rather than addresses.

This means:

- Two locals never alias the same memory by accident — a borrow only
  participates in aliasing when there is an actual ref value.
- A ref always names a binding visible at borrow time. Same-named
  callee parameters cannot redirect a ref to the wrong binding because
  the ref skips frames pushed after its creation.
- Function calls share the env stack with the caller: parameters
  push on top, mutations through ref params reach into the caller's
  frame, and the local frame is dropped on return. A value passed by
  value is fully owned by the callee and the caller cannot observe
  internal callee state.

The interpreter's borrow model is weaker than the surface language's
borrow checker (it does not enforce lifetimes or alias exclusion —
the type checker already did, and the interpreter operates on
already-validated Core IR). It is strong enough to give the same
observable result as the compiled binary for the supported subset.

### Integer arithmetic is arbitrary-precision

All `int` values are Lean `Int`s. The interpreter does **not** model
integer width or wrapping:

- `i32 + i32` that would wrap in the compiled binary will not wrap in
  the interpreter.
- Shift amounts are unbounded; `1 << 100` produces `2^100`, not
  undefined behavior.
- `bitnot n` is computed as `-(n+1)` (Lean's two's-complement-on-Int
  identity) rather than over a fixed width.

This is a deliberate trade-off: the predictable subset deliberately
avoids relying on wrap behavior, and proof claims about the predictable
subset should hold over arbitrary-precision integers. Any oracle vector
whose result depends on wrapping is a sign that the program is outside
the predictable subset, not a bug in the interpreter — but the harness
will surface the disagreement and force the call to be made
explicitly.

For an explicit overflow policy, see
[ARITHMETIC_POLICY.md](ARITHMETIC_POLICY.md).

### Failure modes are explicit, not silent

The compiled binary inherits LLVM / target / OS behavior on division
by zero, out-of-bounds access, and stack overflow. The interpreter
turns each of these into a named `interp: ...` error. The oracle
contract treats these as runtime errors of the interpreter; the
compiled binary is allowed to behave differently (raise, abort, or
hardware-trap). When that divergence matters, the program is outside
the agreed-upon predictable subset.

### Loop fuel

`while` is bounded at 10,000,000 iterations. This guards the
interpreter (and the harness) from accidentally non-terminating
programs without claiming anything about the compiled binary's
termination.

### Stack

The interpreter recurses through Lean's stack. Concrete programs that
recurse deeply may hit the Lean stack limit before they hit the
predictable-stack-depth bound the compiler reports. Stack-bound claims
about a program belong on the compiled side, not the interpreter side;
see [PREDICTABLE_BOUNDARIES.md](PREDICTABLE_BOUNDARIES.md).

## 4. Why this earns trust

The interpreter is small on purpose:

- **~550 lines, single file, no transformation passes.** Reading it
  end-to-end fits in one sitting. Reading the full compiler does not.
- **Operates on validated Core IR.** Parsing, name resolution, type
  checking, elaboration, and `CoreCheck` already ran. The interpreter
  inherits their guarantees and adds no new ones.
- **No optimizer, no monomorphization, no lowering.** Every reduction
  rule is a direct match on the IR; there is no opportunity for an
  optimizer to rewrite a program into something equivalent-on-paper
  but distinct-in-practice.
- **No platform / target dependence.** The interpreter runs the same
  on every host where Lean runs. There is no clang version, no LLVM
  version, no libc, no linker.
- **No silent approximations.** Every unsupported construct hits an
  explicit `interp: ...` branch. The harness reads those exact strings
  to classify a vector as PENDING.
- **No external trust dependencies.** No FFI, no `unsafe`, no shelling
  out. The Lean kernel and standard library are the only trust roots.

Together, these mean: when the harness reports a mismatch, the simpler
side is the one we treat as ground truth by default. A mismatch is a
real disagreement, and at least one of the two sides has a bug. The
harness exists to surface those disagreements; this document exists so
we know where the line is.

The interpreter is **not** intended to grow into a fully verified
reference implementation. Phase G/I has a separate goal — a small
proof-relevant reference interpreter scoped to ProofCore — and that
work is gated on `Core → ProofCore` extraction being formal enough to
support it.

## See also

- `tests/oracle/README.md` — the harness, the vector format, and the
  PENDING-vs-FAIL contract
- [ARITHMETIC_POLICY.md](ARITHMETIC_POLICY.md) — arithmetic and
  overflow policy for predictable / proved profiles
- [PREDICTABLE_BOUNDARIES.md](PREDICTABLE_BOUNDARIES.md) — the
  compiled-side predictable subset
- [PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md) — how the
  proof workflow models program semantics over PExpr (a separate IR
  from the interpreter's `CExpr`)
- [TRUSTED_COMPUTING_BASE.md](TRUSTED_COMPUTING_BASE.md) — the broader
  trust map for Concrete
