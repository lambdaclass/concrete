# Trusted Computing Base

Status: canonical reference (Phase 2, item 22)

This document names the trusted components behind Concrete's strongest current claims, what each layer is and is not verified for, and how each profile's claims depend on the TCB.

The point is not to eliminate trust with one document. The point is to keep the trust boundary explicit and current.

For the five claim classes, see [CLAIM_TAXONOMY.md](CLAIM_TAXONOMY.md).
For the public guarantee statement, see [GUARANTEE_STATEMENT.md](GUARANTEE_STATEMENT.md).
For the canonical "claims today" surface, see [CLAIMS_TODAY.md](CLAIMS_TODAY.md).

---

## Why This Exists

Concrete makes stronger claims than an ordinary compiler:

- checked safe-code guarantees
- proof/evidence artifacts with Lean-backed theorems
- automatic stale detection and CI evidence gates
- trust-drift and consistency checks

Those claims are only honest if the trusted computing base stays visible.

---

## TCB Layers

### 1. Concrete checker and compiler

**Trusted for:**

- Parsing, resolution, checking, elaboration, core-checking, monomorphization, lowering, SSA verification, emission, and reporting
- Enforcing the documented safe-code rules (ownership, linearity, borrows, capabilities, control flow, `&mut T` tracking, cleanup ordering)
- Generating accurate proof/evidence artifacts (extraction, fingerprinting, obligation derivation, stale detection, eligibility classification)
- Registry validation (unknown functions, ineligible targets, duplicate entries, empty fields, rename detection)
- Predictable profile gate evaluation (recursion, loop bounds, allocation, blocking/FFI)
- Policy enforcement (`require-proofs`, `predictable`, `deny`)

**Actively verified by:**

- Adversarial test suite (1272 trust-gate checks)
- CI proof gate (20 checks across 8 sections)
- Determinism checks (extraction and fingerprints verified identical across runs)
- End-to-end regression coverage (positive, negative, integration, report, codegen)

**Not currently claimed:**

- Formal proof of compiler correctness
- Formal proof of checker soundness
- Proof of code-generation correctness (Core IR → SSA → LLVM IR)
- Proof that enforced properties are sound (the checker rejects violations, but there is no mechanized argument that it catches all violations)

### 2. Lean kernel

**Trusted for:**

- Checking attached theorems and proof objects
- Enforcing the theorem-level proof discipline Concrete relies on
- Verifying that `sorry`-free theorems actually hold

**Role in the TCB:**

This is the core trust anchor for the "proved" label. When `--report check-proofs` invokes `lake env lean` and the theorem compiles without `sorry`, the Lean kernel has verified it. The Concrete compiler does not re-verify the proof — it trusts the Lean kernel's verdict.

**Not currently claimed:**

- Lean kernel correctness itself (Lean's kernel is widely trusted but not formally verified against an independent specification in this project)

### 3. Proof attachment / registry / fingerprint machinery

**Trusted for:**

- Correctly binding source functions to specs and theorem identities via `proof-registry.json` or hardcoded `provedFunctions`
- Correctly computing body fingerprints from Core IR
- Correctly detecting stale or mismatched proof attachments
- Correctly resolving renamed functions by fingerprint matching
- Correctly scoping proof obligations to user packages (filtering out stdlib)

**Actively verified by:**

- Registry validation in compiler (`validateRegistry`): unknown functions, ineligible targets, extraction-blocked targets, empty fields, duplicates
- Fingerprint determinism checks (same source → same fingerprint across runs)
- Stale-repair cycle tests (mutate body → stale detected → update fingerprint → proved restored)
- Rename-detection tests (rename function → orphaned entry matched by fingerprint)
- Adversarial registry tests (fabricated entries, mismatched specs, invalid targets)

**Trust surface being reduced:**

Each Phase 2 item has tightened this layer. The registry is no longer a blind trust boundary — it is validated, and invalid entries produce explicit diagnostics with error codes (E0805 for attachment integrity, E0806 for theorem lookup, E0807 for Lean check failure).

**Remaining trust:**

- The fingerprint algorithm is assumed to be collision-free for practical purposes (structural hash, not cryptographic)
- The PExpr extraction is assumed to faithfully represent the Core IR semantics (no formal correspondence proof)
- The `eval` function in `Proof.lean` is assumed to correctly model PExpr evaluation (no formal soundness proof of the evaluator)

### 4. Backend and toolchain

**Trusted for:**

- LLVM IR semantic correctness
- Optimization correctness (LLVM passes do not introduce bugs)
- Object generation and linking
- Final binary behavior within normal toolchain assumptions

**Specific assumptions:**

| Assumption | Depends on |
|------------|------------|
| LLVM IR semantics match Concrete's intended semantics | LLVM version, optimization level |
| `malloc`/`realloc`/`free` behave correctly | libc implementation |
| Linking produces a correct executable | Linker, system libraries |
| Two's-complement integer wrapping | Target architecture (universally true on supported targets) |

**Not verified by Concrete:**

The Core IR → SSA → LLVM IR → binary chain is entirely unverified. This is the single largest gap between what the proof model covers (PExpr with unbounded integers) and what the binary does (fixed-width integers, real memory, real execution). The integer-representation gap is the most concrete instance: a proof that `f(x) = 42` holds for all mathematical integers, but the binary may produce a different result if intermediate computation overflows.

### 5. Runtime / target / OS / hardware

**Trusted for:**

- ABI behavior and calling conventions
- Allocator and libc behavior where used
- OS guard page for stack overflow
- Hardware instruction semantics
- OS and runtime behavior outside the source-language model

**This especially matters for:**

- FFI (foreign function behavior is entirely outside Concrete's model)
- Timing/stack/layout assumptions (predictable profile does not cover hardware timing)
- Hosted runtime behavior (I/O, filesystem, networking)

### 6. Trusted and foreign program boundaries

**Trusted for:**

- Correctness of code behind `trusted fn` / `trusted impl` boundaries
- Correctness of foreign code behind `extern fn` / `trusted extern fn` boundaries
- Correctness of wrappers that intentionally concentrate unsafety

**Compiler visibility:**

- `--report unsafe` shows trust boundaries and what each trusted function wraps
- Capabilities in signatures reveal which functions can reach trust boundaries
- `trusted extern fn` is a special case: allows calling foreign code without `with(Unsafe)` based on the programmer's assertion that the binding is pure and safe; the compiler does not verify this

**Concrete's goal is to keep this trust visible and narrow, not to pretend it disappears.**

---

## Profile-Specific TCB

Each profile's claims depend on different TCB layers with different weights:

### Safe profile TCB

| TCB layer | Role | Weight |
|-----------|------|--------|
| Concrete checker | Primary — enforces all safe-code guarantees | Critical |
| Backend/toolchain | Assumed correct for execution-level claims | Assumed |
| Runtime/target/OS | Assumed correct for ABI, stack, allocator | Assumed |
| Trusted/foreign code | Outside safe boundary — `trusted` and FFI code is explicitly excluded from safe guarantees | Boundary |

**Key dependency:** The safe profile's guarantees rest almost entirely on the Concrete checker. If the checker has a soundness bug, safe-code guarantees are violated. Adversarial tests and the no-codegen-crash rule mitigate this but do not eliminate it.

### Predictable profile TCB

| TCB layer | Role | Weight |
|-----------|------|--------|
| Concrete checker | Enforces safe-code guarantees (inherited) | Critical |
| Concrete report/analysis | Evaluates five predictable gates (recursion, loops, allocation, blocking, FFI) | Critical |
| Backend/toolchain | Assumed — optimization may affect execution shape | Assumed |
| Runtime/target/OS | Assumed — timing, stack depth, and memory behavior are outside the model | Assumed |

**Key dependency:** The predictable profile adds analysis-level claims (reported or policy-enforced) on top of the safe TCB. The five gates are compiler checks, but they do not cover: actual execution timing, stack depth, memory-layout effects, or LLVM optimization behavior. The predictable profile is about source-level execution shape, not binary-level timing guarantees.

### Provable profile TCB

| TCB layer | Role | Weight |
|-----------|------|--------|
| Concrete checker | Enforces safe-code guarantees (inherited) | Critical |
| Concrete extraction pipeline | Extracts PExpr from Core IR, computes fingerprints, derives obligations | Critical |
| Proof registry/fingerprint | Binds functions to specs and theorems, detects stale proofs | Critical |
| Lean kernel | Verifies the actual theorem | Critical — trust anchor |
| Backend/toolchain | NOT covered — proof is over PExpr, not the binary | Gap |
| Runtime/target/OS | NOT covered — proof uses unbounded integers, not fixed-width | Gap |

**Key dependency:** The provable profile has the deepest TCB but also the most explicit gap. Four layers are critical (checker, extraction, registry, Lean kernel), and two layers are explicitly not covered (backend, runtime). The proof-bundle evidence artifact (`--report proof-bundle`) documents these assumptions explicitly in its `assumptions` section.

### High-integrity profile TCB (future)

Not yet defined. The intended shape is to inherit all three TCBs (safe + predictable + provable) and add profile-specific restrictions that reduce the trust surface further: tighter authority budgets, restricted FFI, bounded allocation, and stronger evidence requirements. The TCB accounting for high-integrity will be defined when the profile is implemented.

---

## Strongest Current Claims and Their TCB

### Claim: "Safe code has no use-after-move, no leak, no borrow conflict"

| Layer | Status |
|-------|--------|
| Concrete checker | Enforces it — adversarial-tested |
| Backend | Assumed correct for execution |
| Formal proof | None — checker behavior, not mechanized soundness |

### Claim: "This function satisfies theorem T"

| Layer | Status |
|-------|--------|
| Concrete extraction | Trusted — PExpr faithfully represents Core IR |
| Proof registry | Validated — fingerprint match enforced, stale detection automatic |
| Lean kernel | Verified — theorem is kernel-checked |
| Backend | NOT covered — proof is over PExpr, not the binary |
| Integer model | Gap — proof uses unbounded Int, binary uses fixed-width |

### Claim: "This function is predictable (no recursion, no allocation, no blocking)"

| Layer | Status |
|-------|--------|
| Concrete analysis | Checked — five gates evaluated from source/Core IR |
| Backend | NOT covered — optimization may change execution shape |
| Runtime | NOT covered — actual timing/stack behavior is outside the model |

### Claim: "The proof workflow is deterministic and CI-gated"

| Layer | Status |
|-------|--------|
| Fingerprint determinism | Verified — same source produces same fingerprint across runs |
| Registry validation | Verified — invalid entries rejected with diagnostics |
| CI gate | Running — 20 checks across 8 sections |
| Lean checking | Invoked — `lake env lean` verifies theorems |

---

## Practical Rule

When a claim gets stronger, ask:

1. Which layer is enforcing it?
2. Which layer is proving it?
3. Which layer is still trusted?
4. Which layer is backend/target assumed?
5. Which profile does the claim belong to?

If those answers are unclear, the claim is too vague.

When the project evolves (new extraction constructs, new profile gates, new proof targets), update this document to keep the TCB accounting current.
