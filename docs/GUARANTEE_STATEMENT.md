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

| Property | Mechanism | Evidence level |
|----------|-----------|----------------|
| No use-after-move | Linear variable in `consumed` state cannot be read, borrowed, or moved | Compiler-enforced |
| No double free | Linear value consumed once; second consumption is use-after-move | Compiler-enforced |
| No memory leak | Every linear variable consumed or reserved by scope exit | Compiler-enforced |
| No dangling safe reference | Borrow-block scoping + escape analysis | Compiler-enforced |
| No borrow conflict | Mutable borrows are exclusive; shared borrows are incompatible with mutable borrows | Compiler-enforced |
| No frozen-variable access | Owner frozen during borrow block; no read, write, move, or re-borrow | Compiler-enforced |
| No linear reassignment | Linear variables cannot be reassigned, period | Compiler-enforced |
| No `&mut T` aliasing | Borrow-block exclusive refs consumed on function call; no double-use | Compiler-enforced |
| Deterministic cleanup order | `defer` runs LIFO at scope exit | Compiler-enforced |

### Capability discipline (compiler-enforced)

| Property | Mechanism | Evidence level |
|----------|-----------|----------------|
| No capability escalation | Callers must declare a superset of callee capabilities | Compiler-enforced |
| No hidden effects | Capabilities in signatures; `capSet.isEmpty` = pure | Compiler-enforced |
| No silent FFI | Extern calls require `with(Unsafe)` (except `trusted extern fn`) | Compiler-enforced |

### Control-flow safety (compiler-enforced)

| Property | Mechanism | Evidence level |
|----------|-----------|----------------|
| Branch agreement | If/else and match arms agree on linear consumption | Compiler-enforced |
| No cross-loop consumption | Linear outer-scope variable cannot be consumed in loop body | Compiler-enforced |
| No skip past linear | Break/continue cannot skip unconsumed linear variables | Compiler-enforced |
| No defer-body escape | Break/continue forbidden inside defer bodies | Compiler-enforced |

### What safe code does NOT guarantee

- **No runtime bounds checking by default.** Array access through checked APIs returns `Option`; unchecked access is UB. The checker does not verify array bounds.
- **No integer overflow protection.** Integer arithmetic wraps silently. This is a runtime property, not a checker property.
- **No stack overflow protection.** Stack depth depends on the OS guard page. The compiler does not analyze stack usage.
- **No termination guarantee.** The checker does not verify that loops terminate or that recursion bottoms out.

---

## 2. What Proof-Backed Code Guarantees

**Proof-backed code** = functions that pass all ProofCore eligibility gates and have Lean 4 theorems attached via the proof registry.

For proof-backed code, the following additional properties hold:

### Functional correctness (proof-backed)

| Property | Mechanism | Evidence level |
|----------|-----------|----------------|
| Return value correctness | Lean 4 theorem proving `eval(body, env) = expected` | Proof-backed |
| Stale detection | Body fingerprint invalidates proofs when source changes | Compiler-enforced |
| Extraction fidelity | `PExpr` normalized proof targets match Core IR semantics | Compiler-enforced |

### What makes code proof-eligible

All of these must hold simultaneously:

1. **Pure** — empty capability set (no effects)
2. **Not trusted** — no `trusted` marker, no `trusted impl` origin
3. **Not entry point** — not `main`
4. **Body extractable** — function body translatable to `PExpr` (integer/boolean arithmetic, comparisons, let bindings, if/then/else, non-recursive calls)
5. **Profile gates** (advisory) — no recursion, bounded loops, no allocation, no FFI, no blocking I/O

### What proof-backed code does NOT guarantee

- **No proof of the checker itself.** The Lean theorems prove properties of specific functions, not that the checker is sound.
- **No cross-function composition.** Proofs are per-function. A proof that `f` is correct and `g` is correct does not automatically prove `f(g(x))` is correct.
- **No proof of runtime behavior.** Proofs operate on the `PExpr` semantic model, not on the compiled binary. Backend correctness is a separate assumption.
- **No coverage of all proof-eligible functions.** A function can be proof-eligible without having a proof attached. Eligibility is a necessary condition, not sufficient.

---

## 3. What Trusted Code Invalidates

**Trusted code** = functions or impl blocks marked `trusted`, or functions with `with(Unsafe)`.

Trusted code **retains** all of these safe-code guarantees:
- Linear ownership and consumption rules
- Capability discipline (capabilities are not suppressed by `trusted`)
- Borrow-block and escape analysis rules
- Scope-exit linearity checks
- Branch agreement and control-flow safety

Trusted code **adds** these permissions:
- Pointer arithmetic (`*mut T + offset`)
- Raw pointer dereference (`*p`)
- Raw pointer assignment (`*p = value`)
- Pointer-involving casts (except `&x as *const T`, which is always safe)

Trusted code **invalidates** these guarantees at the trust boundary:

| Guarantee | How trusted code can break it | Mitigation |
|-----------|-------------------------------|------------|
| No use-after-free | Raw pointer dereference after the pointee is freed | Audit responsibility; `--report unsafe` |
| No dangling reference | Raw pointer outlives its referent | Audit responsibility |
| No aliasing violation | Raw pointer aliasing bypasses borrow rules | Audit responsibility |
| No memory corruption | Raw pointer write to arbitrary memory | Audit responsibility |
| Proof eligibility | Trusted functions are excluded from proof extraction | By design |

**The trust boundary is contained.** Callers of trusted functions see a safe signature with declared capabilities. The caller is not itself "infected" — a pure function can call a trusted function only if the trusted function has capabilities, which would make the caller non-pure. The trust boundary does not propagate silently.

**`trusted extern fn` is a special case.** It allows calling a foreign function without `with(Unsafe)`, based on the programmer's assertion that the binding is pure and safe (e.g., `sqrt`, `abs`). The compiler does not verify this assertion. If the foreign function has side effects or is unsafe, the trust boundary is silently violated.

---

## 4. Backend and Target Assumptions

These properties are assumed, not verified by the compiler:

| Assumption | Depends on |
|------------|------------|
| LLVM IR semantics match Concrete's intended semantics | LLVM version, optimization level |
| `malloc`/`realloc`/`free` behave correctly | libc implementation |
| OS provides a guard page for stack overflow | OS and runtime |
| Integer wrapping follows two's-complement semantics | Target architecture (universally true on supported targets) |
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

- **Future guarantees.** The proof-eligible subset will expand (structs, enums, match, recursion). The predictable profile may become enforced rather than advisory. These are tracked in the [roadmap](../ROADMAP.md).
- **Formal proof of the checker.** The checker is tested by adversarial tests and code review. A formal soundness proof is future work.
- **Concurrency.** The model assumes single-threaded execution. Concurrency interaction is an explicitly deferred boundary.
- **Cross-package guarantees.** The current model is single-compilation-unit. Package-level trust, capability, and proof propagation are future work.
