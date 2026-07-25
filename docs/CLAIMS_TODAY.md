# Claims Today

Status: canonical public reference (Phase 2, item 20)

This page states what Concrete claims today, what it does not claim yet, and what each claim's evidence class is. It is intentionally short. For detail, follow the references.

For the five claim classes (enforced, proved, reported, trusted assumption, backend/target assumption), see [CLAIM_TAXONOMY.md](CLAIM_TAXONOMY.md).

---

## 1. Compiler-Enforced Guarantees (safe code)

Safe code = no `trusted` marker, no `with(Unsafe)` capability.

For safe code that passes the checker, the compiler enforces at compile time:

| Guarantee | Mechanism |
|-----------|-----------|
| No use-after-move | Linear ownership tracking |
| No double free | Second consumption is use-after-move |
| No memory leak | Every linear value consumed or reserved by scope exit |
| No dangling safe reference | References are second-class — never returned (no `&T`/`&mut T` in any safe return, directly/aggregate-wrapped/generic); borrow-block scoping + escape analysis |
| No borrow conflict | Exclusive mutable; shared precludes mutable |
| No frozen-variable access | Owner frozen during active borrow block |
| No live linear overwrite | Assignment cannot replace a live linear value; rebind after consumption is legal |
| No `&mut T` aliasing | Borrow-block exclusive refs consumed on call |
| Deterministic cleanup | `defer` runs LIFO at scope exit |
| No capability escalation | Caller must declare superset of callee capabilities — including calls through function pointers: calling `f: fn(i32) with(Network) -> i32` requires the caller to hold `Network` (E0240; `adversarial_neg_cap_fnptr_smuggle.con`) |
| No hidden effects | Capabilities visible in signatures |
| Branch agreement | If/else and match arms agree on linear consumption |

**Claim class:** Enforced. The program cannot violate these and compile.

**Returned-reference provenance (H1): CLOSED 2026-06-13.** Resolved by
subtraction — the language invariant *references are second-class, never
returned*: the checker rejects any safe-callable function or function type that
returns a reference, directly, nested in an aggregate (`Option<&T>`), or via
generic instantiation (`Concrete/Check/Check.lean`, `resolveType`; gate
`scripts/tests/check_returned_ref_provenance.sh`). The former aggregate-ref
accessors were migrated to the value model: `get -> Option<V>` (Copy),
`with_value` / `with_at` to borrow (scoped, the `&V` never escapes the callback),
`remove`/`pop` to move out, capability-polymorphic `fold`/`for_each`/`map` to
iterate. No lifetimes, regions, or `from()`. So "No dangling safe reference" now
covers the whole safe surface — borrow-block refs *and* the absence of any
returned reference. (Deferred, evidence-gated: `from(param)`, and the mutable
scoped callback `with_value_mut` — parked, nothing pulls it.)

**What enforced does NOT cover:**
- Static proof that every index is in range. Raw safe indexing is runtime-checked
  and traps on OOB; explicitly trusted/unchecked access remains outside the safe
  guarantee.
- Static proof that ordinary arithmetic cannot overflow. Ordinary integer
  arithmetic is runtime-checked and traps; explicit `wrapping_*` and
  `saturating_*` have their named meanings.
- Stack overflow (depends on OS guard page)
- Termination (no loop/recursion termination analysis)
- Formal proof of checker soundness (tested adversarially, not mechanized)

Legacy H-series and cross-cutting safety boundaries are indexed in
[KNOWN_HOLES.md](KNOWN_HOLES.md); numbered defects and their current status live
in [bugs/README.md](bugs/README.md).

References: [GUARANTEE_STATEMENT.md](GUARANTEE_STATEMENT.md), [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md), [SAFETY.md](SAFETY.md), [KNOWN_HOLES.md](KNOWN_HOLES.md)

---

## 2. Proof-Backed Guarantees (provable subset)

Proof-backed = function passes all eligibility/extraction gates, has a Lean 4
theorem attached through the proof registry, has a stored body fingerprint that
matches, and passes kernel replay.

### What "proved" means precisely

The function's PExpr representation, evaluated with its declared proof
semantics, satisfies the stated theorem. The stored body fingerprint matches the
current body. This is current containment, not a complete proof-subject binding:
until R-0004 lands, signature/types, contracts/spec identity, and transitive
dependencies are not all covered by freshness (bugs 059–060 plus the
dependency-closure leg). A source link without a stored fingerprint is unbound
and cannot report `proved`.

### Proof eligibility gates

A function must be authority-free (no capabilities), not trusted, not an entry
point, non-recursive, non-allocating, free of FFI/blocking I/O, and fully
translatable to the current ProofCore surface. The shipped surface includes
selected bounded-loop/state encodings and functional array update; arbitrary
mutation, unmodeled loop shapes, heap/reference state, and unsupported
expressions remain excluded. `PROVABLE_V1.md` is the canonical construct list.

### Supported theorem shapes

| Shape | Purpose | Example |
|-------|---------|---------|
| Concrete test case | Fixed-input regression anchor | `eval f args = some (.int 42)` via `native_decide` |
| Universal boundary theorem | Prove one branch for all inputs | `∀ x, x < 0 → eval f x = some (.int 2)` |
| Full contract | Complete input-output specification | `∀ x, eval f x = some (.int (spec x))` |

### Proof obligation states

Every function has exactly one status: `proved`, `unbound`, `stale`, `missing`,
`blocked`, `ineligible`, or `trusted`.

### What "proved" does NOT mean

- Does not mean the compiled binary is correct (proof is over PExpr with unbounded integers, not the binary)
- Does not mean the checker is sound (no formal checker soundness proof)
- Does not provide automatic cross-function composition; callees need a complete
  function table and an explicit composition/refinement argument
- Does not mean all properties are proved (only the stated theorem)
- Eligibility does not equal proved (many eligible functions have no proof attached)

**Claim class:** Proved. Lean 4 kernel has verified the theorem. Stale detection is compiler-enforced.

References: [PROOF_CONTRACT.md](PROOF_CONTRACT.md), [PROOF_THEOREM_SHAPES.md](PROOF_THEOREM_SHAPES.md), [PROVABLE_V1.md](PROVABLE_V1.md), [PROVABLE_SUBSET.md](PROVABLE_SUBSET.md), [PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md)

---

## 3. Proof Workflow and Evidence Pipeline

The proof workflow is operational today, not aspirational. It includes:

| Capability | Status | Command / artifact |
|------------|--------|--------------------|
| Extraction and eligibility inspection | Working | `--report extraction` |
| Lean theorem stub generation | Working | `--report lean-stubs` |
| In-source proof links with fingerprint binding | Working | `#[proof_by]` / `#[proof_fingerprint]` |
| Lean kernel verification | Working | `--report check-proofs` |
| Full proof-status report | Working | `--report proof-status` |
| Proof diagnostics (including unbound E0810) | Working | `--report proof-diagnostics` |
| Direct dependency graph with advisory stale-edge display | Working | `--report proof-deps`; caller verdict/closure containment remains R-0004 |
| JSON evidence bundle | Working | `--report proof-bundle` |
| Project-level check with scoped output | Working | `concrete check` |
| CI evidence gate (20 checks, 8 sections) | Working | `scripts/ci/proof_gate.sh` |

### Diagnostic taxonomy

Diagnostics distinguish stale proof (E0800), missing proof (E0801), ineligible
(E0802), unsupported construct (E0803), trusted (E0804), attachment integrity
(E0805), theorem lookup (E0806), Lean check failure (E0807), and an unbound
source proof link (E0810). Each carries a failure class and repair class.

### Evidence bundle

`--report proof-bundle` produces a single JSON artifact with: proof summary, explicit assumptions, registry entries, dependency graph, and all proof-related facts. Designed for CI gates, review, and release evidence.

### Rename and stale detection

Registry entries are validated against current source. Renamed functions are
detected by body-fingerprint matching. Body changes invalidate stored links;
comments and formatting do not. R-0004 records the current limitation: this is
body freshness, not yet a complete `ProofSubjectDigest` over signatures,
contracts/spec identity, and transitive dependencies.

**Claim class:** The workflow itself is compiler-enforced (fingerprinting, validation, stale detection) and reported (diagnostics, evidence bundle). Lean theorem checking is proved.

References: [PROOF_WORKFLOW.md](PROOF_WORKFLOW.md), [CLAIM_TAXONOMY.md](CLAIM_TAXONOMY.md)

---

## 4. Compiler-Reported Analysis

The compiler reports these properties without enforcing them as hard errors:

| Report | What it shows | Claim class |
|--------|---------------|-------------|
| `--report effects` | Per-function effect classification, evidence level | Reported |
| `--report alloc` | Allocation sites, stack-only vs heap | Reported |
| `--report caps` | Per-function capability requirements | Reported |
| `--report authority` | Transitive capability chains | Reported |
| `--report unsafe` | Trust boundaries, what trusted code wraps | Reported |
| `--report eligibility` | Proof eligibility with gate breakdown | Reported |
| `--check predictable` | Five-gate predictable profile check | Reported |
| `--report layout` | Type sizes, alignment, offsets | Reported |
| `--report traceability` | Per-function evidence + boundary | Reported |

**Claim class:** Reported. The compiler observed these properties but does not block compilation on them. Use for audit, not as enforcement.

---

## 5. Profile Status Today

| Profile | Status | What exists |
|---------|--------|-------------|
| **Safe** | Real, enforced | Ownership, linearity, borrows, capabilities, cleanup — all compiler-enforced |
| **Predictable** | Partially real | Reporting and checking for parts of bounded execution; not a complete enforced profile yet |
| **Provable** | Real for the current proof subset | Extraction, stubs, registry, Lean kernel checking, stale detection, diagnostics, evidence bundle, CI gate |
| **High-integrity** | Named direction | Explicit design target; stricter authority, allocation, FFI, failure-path discipline; not implemented yet |

Profiles are stricter ways to use the same language, not separate languages. They do not overlap: predictable is not automatically proved, proved is not automatically high-integrity, safe is not automatically predictable.

Reference: [PROFILES.md](PROFILES.md)

---

## 6. What Concrete Does Not Claim Yet

| Non-claim | Why not |
|-----------|---------|
| Whole-compiler correctness | No formal proof of checker or pipeline soundness |
| Verified code generation | Core IR → SSA → LLVM IR → binary chain is unverified |
| Verified backend/toolchain behavior | LLVM, linker, OS behavior is assumed, not proved |
| Complete proof of binary behavior | Proofs are over PExpr with unbounded integers, not the compiled binary |
| Cross-function proof composition | Proofs are per-function; dependency tracking exists but is not compositional verification |
| Finished high-integrity profile | Named direction, not a completed profile |
| Finished predictable profile | Partially real, still being tightened |
| Composite compile-time resource certificate | Planned as R-0445; no current composite resource-report command or source-step/space guarantee |
| Stabilized first public release surface | Still evolving |
| Concurrency safety | Single-threaded model assumed; concurrency is explicitly deferred. Async/concurrency capabilities, structured scopes, linear task handles, and simulation-backed evidence are research directions, not current claims. |
| Cross-package guarantees | Single-compilation-unit model today |

---

## 7. Trusted Computing Base

The strongest current claims depend on these trusted components:

| TCB layer | Trusted for | Verified? |
|-----------|-------------|-----------|
| Concrete checker and compiler | Parsing, checking, elaboration, lowering, reporting, artifact production | Adversarial tests + code review; no formal soundness proof |
| Lean kernel | Checking attached theorems and proof objects | Trusted as core proof anchor |
| Proof registry and fingerprint machinery | Binding functions to specs and detecting stale proofs | Compiler-enforced validation; still part of TCB |
| Backend and toolchain (LLVM, clang, linker) | IR handling, optimization, object generation, linking | Assumed correct; not verified by Concrete |
| Runtime / target / OS / hardware | ABI, calling conventions, allocator, guard pages | Assumed correct; outside compiler reach |
| Trusted and foreign code boundaries | Correctness of `trusted fn`, `extern fn`, FFI wrappers | Programmer assertion + audit; compiler tracks but does not verify |

Reference: [TRUSTED_COMPUTING_BASE.md](TRUSTED_COMPUTING_BASE.md)

---

## 8. How to Read Concrete Claims

Use these words precisely:

| Term | Meaning |
|------|---------|
| **Enforced** | Checker/compiler-enforced — program cannot violate and compile |
| **Proved** | Kernel-verified Lean theorem over PExpr with a matching stored body fingerprint; R-0004 records the remaining proof-subject scope |
| **Reported** | Compiler-observed and surfaced in reports; not a hard error |
| **Trusted assumption** | Programmer asserts correctness; compiler tracks but does not verify |
| **Backend/target assumption** | Below the compiler's reach; assumed correct |

Do not conflate these. Reported ≠ proved. Enforced ≠ Lean-proved. Proved ≠ binary-correct. Trusted ≠ verified.

Reference: [CLAIM_TAXONOMY.md](CLAIM_TAXONOMY.md)

---

## 9. Machine-Readable Evidence

Every claim above has a machine-readable surface:

| Evidence | Format | Command |
|----------|--------|---------|
| Proof status per function | JSON fact `proof_status` | `--report proof-status` |
| Obligation status per function | JSON fact `obligation` | `--report obligations` |
| Evidence level per function | JSON field `evidence` | `--report effects` |
| Eligibility per function | JSON fact `eligibility` | `--report eligibility` |
| Full evidence bundle | JSON document | `--report proof-bundle` |
| All facts as JSON | JSON array | `--report diagnostics-json` |

CI gates can consume these directly. The `proof-gate` CI job runs 20 checks across 8 sections automatically.
