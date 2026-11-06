# Public Guarantee Statement

Status: canonical reference — the single precise statement of what Concrete guarantees, at what evidence level, and where those guarantees end.

For the memory/ownership guarantee boundary, see [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md).
For the effect/trust proof boundary, see [EFFECT_PROOF_BOUNDARIES.md](EFFECT_PROOF_BOUNDARIES.md).
For the provable subset definition, see [PROVABLE_SUBSET.md](PROVABLE_SUBSET.md).
For the safety model, see [SAFETY.md](SAFETY.md).
For the language-semantics vs proof-semantics boundary, see [PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md).
For the proof-claim taxonomy, see [CLAIM_TAXONOMY.md](CLAIM_TAXONOMY.md).

---

## 1. What Safe Code Guarantees

**Safe code** = no `trusted` marker, no `with(Unsafe)` capability.

For safe Concrete code that passes the checker, the following properties hold at compile time:

### Memory safety (compiler-enforced)

| Property | Rule | Evidence level |
|----------|-----------|----------------|
| No use-after-move | [OWN-1](../spec/ownership.md#OWN-1) | Compiler-enforced |
| No double free | [OWN-1](../spec/ownership.md#OWN-1), [MEM-5](../spec/memory.md#MEM-5) | Compiler-enforced |
| No memory leak | [OWN-2](../spec/ownership.md#OWN-2) | Compiler-enforced |
| No dangling safe reference | [REF-6](../spec/references.md#REF-6), [REF-8](../spec/references.md#REF-8) | Compiler-enforced |
| No borrow conflict | [REF-11](../spec/references.md#REF-11) | Compiler-enforced |
| No frozen-variable access | [REF-8](../spec/references.md#REF-8) | Compiler-enforced |
| No live linear overwrite | [OWN-9](../spec/ownership.md#OWN-9) | Compiler-enforced |
| No `&mut T` aliasing | `&mut` is lent per call ([REF-10](../spec/references.md#REF-10)) and exclusive within the call ([REF-11](../spec/references.md#REF-11)) | Compiler-enforced |
| Deterministic cleanup order | [STMT-11](../spec/statements.md#STMT-11), [OWN-12](../spec/ownership.md#OWN-12) | Compiler-enforced |

### Capability discipline (compiler-enforced)

| Property | Rule | Evidence level |
|----------|-----------|----------------|
| No capability escalation | [TRUST-4](../spec/trusted.md#TRUST-4) | Compiler-enforced |
| No hidden effects | Capabilities in signatures; an empty set is an authority claim, not purity ([TRUST-4](../spec/trusted.md#TRUST-4)) | Compiler-enforced |
| No silent FFI | Plain `extern fn` calls need `Unsafe` outside a `trusted` body ([TRUST-4](../spec/trusted.md#TRUST-4), [TRUST-5](../spec/trusted.md#TRUST-5)), and every extern declares its effects ([FFI-3](../spec/ffi.md#FFI-3)) | Compiler-enforced |

### Control-flow safety (compiler-enforced)

| Property | Rule | Evidence level |
|----------|-----------|----------------|
| Branch agreement | [OWN-3](../spec/ownership.md#OWN-3) | Compiler-enforced |
| No cross-loop consumption | [OWN-4](../spec/ownership.md#OWN-4) | Compiler-enforced |
| No skip past linear | `break` and `continue` are scope exits under [OWN-2](../spec/ownership.md#OWN-2) | Compiler-enforced |
| No escape from a deferred call | `defer` registers one call, not a body ([OWN-12](../spec/ownership.md#OWN-12)) | Compiler-enforced |

### What safe code does NOT guarantee

- **No static proof that every array index is in range.** Safe indexing is checked at run time and
  aborts ([MEM-6](../spec/memory.md#MEM-6)); trusted/unchecked access remains outside the safe
  guarantee.
- **No static proof that ordinary arithmetic cannot overflow.** Integer arithmetic is checked and
  aborts ([ARITH-3](../spec/arithmetic.md#ARITH-3)); `wrapping_*` and `saturating_*` keep their named
  semantics ([ARITH-4](../spec/arithmetic.md#ARITH-4)).
- **No stack overflow protection.** Stack exhaustion is a failure ([STMT-2](../spec/statements.md#STMT-2)) that depends on the OS guard page. The compiler does not analyze stack usage.
- **No termination guarantee.** Divergence is a behaviour ([STMT-1](../spec/statements.md#STMT-1)), and a claim implies termination only with checked `decreasing` evidence ([CON-1](../spec/contracts.md#CON-1)).

---

## 2. What Proof-Backed Code Guarantees

**Proof-backed code** = functions that pass all ProofCore eligibility gates and have Lean 4 theorems attached via the proof registry.

For proof-backed code, the following additional properties hold:

### Functional correctness (proof-backed)

| Property | Mechanism | Evidence level |
|----------|-----------|----------------|
| Return value correctness | Lean 4 theorem proving `eval(body, env) = expected` | Proof-backed |
| Current stale detection | Stored body fingerprint invalidates links when the body changes; full proof-subject digest remains R-0004 | Compiler-enforced containment |
| Extraction/model fidelity | Differential/negative-control evidence for admitted fragments; no general source/Core-to-PExpr correspondence proof | Reported translation assurance, not established by theorem replay |

### What makes code proof-eligible

All of these must hold simultaneously:

1. **No capability** — empty capability set (an authority claim, not purity: [TRUST-4](../spec/trusted.md#TRUST-4))
2. **Not trusted** — no `trusted` marker on the function or method
3. **Not entry point** — not `main`
4. **Body extractable** — every construct is in the current `ProvableV1`
   surface, including its selected bounded-loop and functional-state forms
5. **Profile gates** (advisory) — no recursion, only admitted bounded loops, no allocation, no FFI, no blocking I/O

### What proof-backed code does NOT guarantee

- **No proof of the checker itself.** The Lean theorems prove properties of specific functions, not that the checker is sound.
- **No cross-function composition.** Proofs are per-function. A proof that `f` is correct and `g` is correct does not automatically prove `f(g(x))` is correct.
- **No proof of runtime behavior.** Proofs operate on the `PExpr` semantic model, not on the compiled binary. Backend correctness is a separate assumption.
- **No correspondence from identity alone.** Exact implementation identity
  prevents substitution and detects covered changes; it does not prove that the
  selected PExpr/model faithfully denotes that implementation. Model
  correspondence and logical validity are separate target evidence fields.
- **No coverage of all proof-eligible functions.** A function can be proof-eligible without having a proof attached. Eligibility is a necessary condition, not sufficient.

---

## 3. What Trusted Code Invalidates

**Trusted code** = functions or methods marked `trusted` (trust is never declared on an `impl`
block, [TRUST-5](../spec/trusted.md#TRUST-5)), or functions with `with(Unsafe)`.

Trusted code **retains** the ownership, linearity, borrow, scope-exit and branch-agreement rules of
safe code ([TRUST-12](../spec/trusted.md#TRUST-12)) and the capability discipline ([TRUST-5](../spec/trusted.md#TRUST-5)).

Trusted code **adds** exactly the raw-pointer operations and plain `extern fn` calls of
[TRUST-5](../spec/trusted.md#TRUST-5): raw dereference and assignment ([TRUST-7](../spec/trusted.md#TRUST-7),
[TRUST-11](../spec/trusted.md#TRUST-11)), pointer arithmetic, written `ptr::offset` ([TRUST-1](../spec/trusted.md#TRUST-1), pending
R-0502; main still accepts `p + n`), and pointer casts other than a reference to a pointer, which
needs no trust ([TRUST-13](../spec/trusted.md#TRUST-13)).

Trusted code **invalidates** these guarantees at the trust boundary:

| Guarantee | How trusted code can break it | Mitigation |
|-----------|-------------------------------|------------|
| No use-after-free | Raw pointer dereference after the pointee is freed | Audit responsibility; `--report unsafe` |
| No dangling reference | Raw pointer outlives its referent | Audit responsibility |
| No aliasing violation | Raw pointer aliasing bypasses borrow rules | Audit responsibility |
| No memory corruption | Raw pointer write to arbitrary memory | Audit responsibility |
| Proof eligibility | Trusted functions are excluded from proof extraction | By design |

**The trust boundary is contained.** Callers of trusted functions see a safe signature with declared
capabilities, and a trusted function's capabilities bind its callers like any other's
([TRUST-4](../spec/trusted.md#TRUST-4), [TRUST-5](../spec/trusted.md#TRUST-5)). The trust boundary does not propagate silently.

**`trusted extern fn` is a special case.** It is callable without `with(Unsafe)` on its author's
assertion, which the compiler does not verify ([FFI-9](../spec/ffi.md#FFI-9)): the binding is safe
for every argument its types permit and has no effect beyond its declared `with(...)` (e.g. `sqrt`,
`abs`). If the foreign function breaks that, the trust boundary is silently violated.

---

## 4. Backend and Target Assumptions

These properties are assumed, not verified by the compiler:

| Assumption | Depends on |
|------------|------------|
| LLVM IR semantics match Concrete's intended semantics | LLVM version, optimization level |
| `malloc`/`realloc`/`free` behave correctly | libc implementation |
| OS provides a guard page for stack overflow | OS and runtime |
| Explicit `wrapping_*` operations follow the declared fixed-width modular semantics | Target architecture and backend |
| Linking produces a correct executable | Linker, system libraries |
| `trusted extern fn` bindings are honest | Foreign library correctness |

The compiler does not verify any of these. They are outside the trust boundary entirely.

---

## 5. Summary: The Four Guarantee Tiers

```
┌─────────────────────────────────────────────────────────┐
│  Tier 1: Proof-backed                                   │
│  Pure functions with Lean 4 theorems.                   │
│  Evidence: formal proof of functional correctness.      │
│  Scope: small — integer/boolean arithmetic, comparisons,│
│          let bindings, if/then/else, non-recursive calls│
├─────────────────────────────────────────────────────────┤
│  Tier 2: Compiler-enforced                              │
│  All safe code (no trusted, no Unsafe).                 │
│  Evidence: mechanical checker enforcement.              │
│  Scope: ownership, linearity, borrows, capabilities,    │
│          control flow, escape analysis, cleanup.         │
├─────────────────────────────────────────────────────────┤
│  Tier 3: Compiler-reported                              │
│  All code covered by reports.                           │
│  Evidence: compiler analysis, not hard enforcement.     │
│  Scope: allocation sites, effect classification,        │
│          predictable profile, capability chains.         │
├─────────────────────────────────────────────────────────┤
│  Tier 4: Trusted assumption                             │
│  trusted code, extern fn, backend, target.              │
│  Evidence: programmer assertion + audit.                │
│  Scope: raw pointer ops, FFI correctness, libc,         │
│          LLVM, linker, OS.                               │
└─────────────────────────────────────────────────────────┘
```

Each tier strictly contains the ones above it. Tier 1 code also has Tier 2 guarantees. Tier 2 code also has Tier 3 reporting. Tier 4 assumptions underlie everything.

The compiler makes every tier boundary visible:
- `--report proof` / `--report eligibility` → Tier 1 boundary
- Checker errors → Tier 2 boundary
- `--report effects` / `--report caps` / `--report unsafe` → Tier 3 reporting
- `--report unsafe` → Tier 4 trust boundaries

---

## 6. What This Statement Does Not Cover

This document defines what the compiler guarantees today. It does not cover:

- **Future guarantees.** The proof-eligible subset will expand (structs, enums, match, recursion). The predictable profile may become enforced rather than advisory. These are tracked in the [roadmap](../../ROADMAP.md).
- **Formal proof of the checker.** The checker is tested by adversarial tests and code review. A formal soundness proof is future work.
- **Concurrency.** A program has one thread ([STMT-3](../spec/statements.md#STMT-3)). Concurrency interaction is an explicitly deferred boundary.
- **Cross-package guarantees.** The current model is single-compilation-unit. Package-level trust, capability, and proof propagation are future work.
