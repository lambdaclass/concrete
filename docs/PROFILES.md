# Profiles

Status: canonical reference

This document defines the four Concrete profiles, the exact gates/reports/evidence each provides today, and which are current, partial, or future-facing.

Concrete is one language. Profiles are stricter, compiler-visible ways to use it. They are not separate languages.

For the five claim classes, see [CLAIM_TAXONOMY.md](CLAIM_TAXONOMY.md).
For the public guarantee statement, see [GUARANTEE_STATEMENT.md](GUARANTEE_STATEMENT.md).
For the canonical "claims today" surface, see [CLAIMS_TODAY.md](CLAIMS_TODAY.md).

---

## Why Profiles Exist

Concrete supports several kinds of trust claims:

- ordinary safe systems code
- bounded and predictable code
- proof-backed code
- later, stricter high-integrity code

Those should not blur together. Profiles make the boundaries explicit.

---

## Profile 1: Safe

**Status: real, enforced today.**

This is the ordinary checked Concrete surface. All compiled code gets these guarantees unless it opts into `trusted` or `with(Unsafe)`.

### Gates (compiler-enforced)

| Gate | What it rejects |
|------|----------------|
| Ownership tracking | Use-after-move, double free, forgotten linear values |
| Borrow checking | Borrow conflicts, borrow escape, frozen-variable access |
| Linearity | Linear reassignment, cross-loop consumption, skip past linear |
| Capability containment | Callers must declare superset of callee capabilities |
| Control flow agreement | Branches must agree on linear variable consumption |
| `&mut T` tracking | Borrow-block exclusive refs consumed on call; no aliasing |
| Cleanup ordering | `defer` runs LIFO at scope exit |

All gates are hard errors. The program cannot compile and violate them.

### Reports

| Command | What it shows |
|---------|---------------|
| `--report effects` | Per-function evidence level (enforced/proved/reported/trusted) |
| `--report caps` | Per-function capability requirements |
| `--report unsafe` | Trust boundaries — what each `trusted fn` wraps |
| `--report authority` | Transitive capability propagation chains |
| `--report traceability` | Full per-function evidence and boundary classification |

### Evidence artifacts

- Checker errors are hard failures — the evidence is the fact that the program compiled
- `--report effects` JSON facts carry `"evidence": "enforced"` for safe code
- `--report traceability` JSON facts carry `"evidenceLevel": "enforced"` with boundary description

### What safe does NOT cover

- Runtime bounds checking (array access through checked APIs returns `Option`; unchecked is UB)
- Integer overflow (wraps silently — runtime property)
- Stack overflow (depends on OS guard page)
- Termination (no loop/recursion termination analysis)
- Formal proof of checker soundness (tested adversarially, not mechanized)
- Backend/runtime correctness (assumed, not verified)

### Claim class

Enforced. No trust required for safe code.

References: [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md), [GUARANTEE_STATEMENT.md](GUARANTEE_STATEMENT.md), [SAFETY.md](SAFETY.md)

---

## Profile 2: Predictable

**Status: partially real today. Reporting and checking exist. Not a complete enforced profile yet.**

This is the restricted execution-oriented profile aimed at code that should be easier to audit for execution shape: no unbounded allocation, no recursion, no hidden blocking.

### Gates (five gates, reporting + optional enforcement)

| Gate | What it checks | Violation reason |
|------|---------------|-----------------|
| No direct recursion | Function calls itself | `"direct recursion"` |
| No mutual recursion | Function in a call cycle | `"mutual recursion with ..."` |
| Bounded loops | All loops have explicit bounds | `"unbounded loops"` or `"mixed loop boundedness"` |
| No allocation | No `alloc`/`vec_new` calls, no `Alloc` capability | `"allocates (...)"` or `"has Alloc capability"` |
| No blocking | No extern calls, no `File`/`Network`/`Process` capabilities | `"calls extern (...)"` or `"may block (...)"` |

### Enforcement modes

| Mode | How | Effect |
|------|-----|--------|
| Advisory | `concrete input.con --check predictable` | Reports pass/fail, exit 0/1 — does not block compilation |
| Policy-enforced | `[policy] predictable = true` in `Concrete.toml` | Violations become hard errors (E0610) via `enforcePredictable` |

### Reports

| Command | What it shows |
|---------|---------------|
| `--check predictable` | Five-gate pass/fail per function with reasons and hints |
| `--report effects` | Effect classification including recursion, loop class, FFI crossings |
| `--report alloc` | Allocation site attribution (allocating, stack-only, cleanup patterns) |
| `--report eligibility` | Predictable gates as part of broader eligibility assessment |

### Evidence artifacts

- JSON facts with kind `"predictable_violation"` carry function name, state, reason, hint, location
- `--query predictable:FUNCTION` returns structured pass/fail with per-gate detail
- Policy violations produce diagnostics with code E0610

### What exists today vs what remains

| Aspect | Status |
|--------|--------|
| Five-gate check | Working |
| Advisory mode (`--check predictable`) | Working |
| Policy enforcement (`predictable = true`) | Working |
| JSON facts for violations | Working |
| Recursion detection (direct + mutual via SCC) | Working |
| Loop boundedness classification | Working |
| Allocation detection | Working |
| Blocking/FFI detection | Working |
| Stack-depth reporting | Working (`--report stack-depth`) |
| Failure discipline | Defined ([PREDICTABLE_FAILURE_DISCIPLINE.md](PREDICTABLE_FAILURE_DISCIPLINE.md)) |
| Checked indexing / slice-view contract | Not yet — indexing and views still need one explicit checked/unchecked surface across arrays and borrowed views |
| Overflow-mode visibility | Not yet — arithmetic still wraps silently today and reports do not expose a chosen source-level policy |
| No-std/freestanding split | Not yet as an implemented profile mode; design is documented in [FREESTANDING_SPLIT.md](FREESTANDING_SPLIT.md) |
| Full predictable profile enforcement as a first-class surface | Not yet — five gates exist but the complete bounded-execution discipline is still being tightened |

### Claim class

Reported (advisory mode). Enforced when policy-gated via `Concrete.toml`. The five gates are real compiler checks, not documentation-only.

References:
- [../research/predictable-execution/predictable-execution.md](../research/predictable-execution/predictable-execution.md)
- [../research/predictable-execution/effect-taxonomy.md](../research/predictable-execution/effect-taxonomy.md)

---

## Profile 3: Provable

**Status: real for the current proof subset.**

This is the proof-backed subset. Functions that pass all eligibility gates can have Lean 4 theorems attached via the proof registry, with automatic stale detection and Lean kernel verification.

### Gates (eligibility gates — reported, not hard errors)

| Gate | Excludes |
|------|----------|
| Not an entry point | `main` functions |
| No capabilities | Functions with `with(Console)`, `with(Net)`, etc. |
| Not `trusted` | Functions marked `trusted fn` |
| No trusted impl origin | Functions backed by trusted code |
| No recursion | Recursive functions |
| No loops | Functions containing `while` |
| No allocation | Functions using `new` |
| No FFI | Functions calling `extern` |
| No blocking I/O | Functions with blocking operations |
| No mutable assignment | Functions with `var` mutation in body |

Additionally, the function body must use only supported extraction constructs: integer/boolean arithmetic, comparisons, let bindings, if/then/else, non-recursive calls.

### Proof obligation states

Every function receives exactly one status:

| Status | Meaning |
|--------|---------|
| `proved` | Spec attached, fingerprint matches, extraction succeeded |
| `stale` | Spec attached, fingerprint changed — body was modified |
| `missing` | Eligible and extractable, no spec attached |
| `blocked` | Eligible but extraction failed (unsupported constructs) |
| `ineligible` | Fails eligibility gates |
| `trusted` | Marked trusted (proof bypassed) |

### Supported theorem shapes

| Shape | Purpose |
|-------|---------|
| Concrete test case | Fixed-input regression anchor via `native_decide` |
| Universal boundary theorem | Prove one branch/path for all inputs |
| Full contract | Complete input-output specification |

### Reports

| Command | What it shows |
|---------|---------------|
| `--report extraction` | Eligibility, extraction status, PExpr form, fingerprint |
| `--report lean-stubs` | Generated Lean 4 theorem stubs |
| `--report proof-status` | Per-function proof state with fingerprints |
| `--report check-proofs` | Lean kernel verification results |
| `--report proof-diagnostics` | Failure taxonomy (8 kinds, E0800–E0807) |
| `--report proof-deps` | Dependency graph with proved/stale edges |
| `--report proof-bundle` | JSON evidence bundle for review and CI |
| `--report obligations` | Obligation internals (spec source, deps) |
| `--report diagnostic-codes` | Error code registry |
| `concrete check` | Project-level proof status with actionable next steps |

### Evidence artifacts

- Lean-kernel-checked theorems (via `--report check-proofs` → `lake env lean`)
- Proof-bundle JSON with summary, assumptions, registry, dependency graph, facts
- JSON facts: `proof_status`, `obligation`, `extraction`, `proof_diagnostic`, `eligibility`
- CI proof gate: 20 checks across 8 sections (`scripts/ci/proof_gate.sh`)
- Proof registry (`proof-registry.json`) with function, fingerprint, proof name, spec

### What "proved" means and does not mean

**Means:** The function's PExpr representation satisfies the stated Lean 4 theorem. The body fingerprint matches. Stale detection is automatic.

**Does not mean:** Binary correctness, checker soundness, cross-function composition, coverage of all properties, or coverage of all eligible functions.

### Claim class

Proved (for functions with attached theorems). Reported (for eligibility, diagnostics, evidence). Enforced (for stale detection and fingerprint validation).

References: [PROOF_CONTRACT.md](PROOF_CONTRACT.md), [PROOF_THEOREM_SHAPES.md](PROOF_THEOREM_SHAPES.md), [PROVABLE_SUBSET.md](PROVABLE_SUBSET.md), [PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md), [PROOF_WORKFLOW.md](PROOF_WORKFLOW.md)

---

## Profile 4: High-Integrity

**Status: named direction, not implemented today.**

This is the stricter long-term profile for code that must be easier to review, constrain, and eventually certify.

### Intended shape

- Same language, tighter restrictions
- Stricter authority, allocation, FFI, and failure-path discipline
- Stronger evidence requirements at package and review boundaries
- Profile-aware compiler checks and reports
- Profile-aware package summaries

### Likely restrictions (not yet implemented)

| Restriction | Purpose |
|-------------|---------|
| No `with(Unsafe)` or tightly scoped | Smaller trusted surface |
| No unrestricted FFI | Approved wrappers only |
| No dynamic/unbounded allocation | Bounded memory footprint |
| Analyzable concurrency | Restricted concurrency model |
| Capability budgets | Explicit authority limits |

### What exists today

Nothing profile-specific. The high-integrity profile is a named design target that draws from safe (enforcement), predictable (bounded execution), and provable (proof-backed evidence) but adds its own restrictions.

### What does not exist yet

- Profile-specific compiler gates
- Profile-specific reports or evidence artifacts
- Package-level profile declarations
- Profile-aware review workflows

### Claim class

None yet. This is a documented design direction, not a claim.

Reference: [../research/language/high-integrity-profile.md](../research/language/high-integrity-profile.md)

---

## Relationship Between Profiles

```
┌──────────────────────────────────────────────┐
│  Safe (enforced)                              │
│  All compiled code without trusted/Unsafe     │
│                                               │
│  ┌──────────────────────────────────────────┐ │
│  │  Predictable (reported/policy-enforced)  │ │
│  │  Bounded execution, no recursion/alloc   │ │
│  └──────────────────────────────────────────┘ │
│                                               │
│  ┌──────────────────────────────────────────┐ │
│  │  Provable (proved)                       │ │
│  │  Lean-backed theorems over PExpr         │ │
│  └──────────────────────────────────────────┘ │
│                                               │
│  ┌──────────────────────────────────────────┐ │
│  │  High-integrity (future)                 │ │
│  │  Stricter synthesis of all three         │ │
│  └──────────────────────────────────────────┘ │
└──────────────────────────────────────────────┘
```

These overlap but are not subsets of each other:

- Predictable is not automatically proved (bounded execution ≠ Lean theorem)
- Proved is not automatically predictable (a proved function may allocate in callees)
- Proved is not automatically high-integrity (proof of one property ≠ full auditability)
- Safe is not automatically predictable (safe code can recurse and allocate freely)

High-integrity is intended to be the stricter synthesis: safe enforcement + predictable bounds + proof-backed evidence + tighter authority/trust boundaries.

---

## Practical Rule

When talking about Concrete publicly:

1. Say which profile you mean
2. Say whether the claim is enforced, proved, reported, or trusted
3. Say whether the profile is current, partial, or future-facing
4. Do not use "safe", "predictable", "proved", and "high-integrity" interchangeably
