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

That verdict is conditional on the exact kernel implementation, version,
rule set, exporter, and retained environment. Kernel soundness defects can
admit axiom-free false theorems; `--trust=0` and an empty axiom inventory do not
detect that class. Checker identity is therefore versioned evidence, and
current acceptance is revocable policy rather than a timeless receipt fact.

**Not currently claimed:**

- Lean kernel correctness itself (Lean's kernel is widely trusted but not formally verified against an independent specification in this project)
- That attached proof modules or serialized environments are benign; they are adversarial checker input
- That two implementations are independent merely because their executable names differ

### 3. Proof attachment / registry / fingerprint machinery

**Trusted for:**

- Correctly binding source functions to specs and theorem identities via in-source proof links (the only binding since bug 083 deleted the name-keyed `provedFunctions` table)
- Correctly computing body fingerprints from Core IR
- Correctly detecting stale or mismatched proof attachments
- Correctly resolving renamed functions by fingerprint matching
- Correctly scoping proof obligations to user packages (filtering out stdlib)

**Actively verified by:**

- Registry validation in compiler (`validateRegistry`): unknown functions, ineligible targets, extraction-blocked targets, empty fields, duplicates
- Unbound-link containment: an in-source proof with no stored fingerprint is
  E0810/`unbound`, not proved
- Fingerprint determinism checks (same source → same fingerprint across runs)
- Stale-repair cycle tests (mutate body → stale detected → update fingerprint → proved restored)
- Rename-detection tests (rename function → orphaned entry matched by fingerprint)
- Adversarial registry tests (fabricated entries, mismatched specs, invalid targets)

**Trust surface being reduced:**

Each Phase 2 item has tightened this layer. The registry is no longer a blind trust boundary — it is validated, and invalid entries produce explicit diagnostics with error codes (E0805 for attachment integrity, E0806 for theorem lookup, E0807 for Lean check failure).

**Remaining trust:**

- The fingerprint algorithm is assumed to be collision-free for practical purposes (truncated SHA-256 over the structural encoding; hardened from the former 64-bit `String.hash`, which was craftable into a silent stale→proved upgrade)
- The current fingerprint does not yet cover every signature/type fact,
  contract, transitive dependency, or replay-context fact; R-0004 owns the
  versioned full-subject digest
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

- Correctness of code behind `trusted fn` boundaries (functions and methods)
- Correctness of foreign code behind `extern fn` / `trusted extern fn` boundaries
- Correctness of wrappers that intentionally concentrate unsafety

**Compiler visibility:**

- `--report unsafe` shows trust boundaries and what each trusted function wraps
- Capabilities in signatures reveal which functions can reach trust boundaries
- `trusted extern fn` is a special case: allows calling foreign code without `with(Unsafe)` based on the programmer's assertion that the binding is safe for every argument and has no undeclared effect ([FFI-9](../spec/ffi.md#FFI-9)); the compiler does not verify this

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
| Proof registry | Validated at the current body boundary; missing fingerprints fail closed, full-subject coverage remains R-0004 |
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

## What multi-kernel agreement does and does not remove from the TCB

`proved_by_two_kernels` / `proved_by_multi_kernel` move exactly one thing out of the
trusted base, and leave one conspicuous thing in it. Both directions belong here,
because the badge is easy to read as stronger than it is.

**Reduced: sole dependence on one kernel's soundness.** An obligation closed by Lean's
`omega` *and* by Rocq's `lia` (and/or Isabelle's `presburger`) no longer rests on any
single prover implementation being correct. With Isabelle among the attesters the
agreement also spans logics — HOL rather than a CIC-family type theory — so a
foundational error in one family does not silently carry the claim. Each attestation
is recorded as a receipt carrying the exact tool version, so a claim can be
re-audited, or invalidated, when a specific prover release is later found buggy.

This reduction is conditional on version currency and genuine independence.
Record code lineage, algorithm, logical foundation, parser, exporter, shared
libraries, and shared specification separately. Two ports of the same flawed
rule are not equivalent to two proof-system families, and an affected checker
is rejected wholesale unless an independently checked feature footprint safely
narrows the advisory's reach.

**NOT removed: the Core→obligation bridge.** Every kernel checks a lowering produced
by ONE shared bridge. If that bridge emits the wrong proposition, all kernels agree on
the wrong proposition and the badge still appears. Bridge trust is therefore
untouched by adding kernels, and stays untouched until the per-rule bridge register is
discharged (R-0460; **0 of 5 rows fully discharged today, 4 of 5 half** — the semantics
half of overflow/bounds/div/shift is proved and proved tight, while the lowering half, which
is precisely the trust this paragraph is about, is untouched) or realization proofs exist
(R-0449). The
obligation record states this structurally rather than in prose —
`independent_of.bridge` is `"no"` — so the disclaimer cannot quietly go stale.

The asymmetry is worth stating plainly, because it inverts the intuition the badge
creates: kernels are redundant checkers of the bridge's *output*, so they are strongest
exactly where failure was least likely (a prover implementation being unsound) and blind
exactly where our own code is (the lowering). Measured over this arc, kernel agreement
found no faults; the faults were found by differential tests and by reading reports. Read
the badge as reducing *whose* soundness you must assume, not as reducing *how much* is
assumed.

**ALSO NOT REMOVED, and currently unsound: the hypothesis set.** Every obligation is
discharged *under hypotheses*, and those hypotheses are part of the trusted base whenever
their own justification is weaker than the conclusion drawn from them. Today that is not
merely a boundary but a defect: a loop `#[invariant]` is attached as a hypothesis
regardless of whether its preservation VC is discharged, so a badge can rest on a fact
nothing established. H23 in [KNOWN_HOLES.md](KNOWN_HOLES.md) reproduces it — three
kernels, two logics, unanimous, on a program that aborts. Until R-0461 composes status
across the assumption edge, the TCB for any `proved` runtime-safety obligation on a
function containing a loop silently includes **that loop's invariants as unverified
assumptions**.

This is the clearest illustration of the section's point. Adding kernels did nothing about
it, because all three were handed the same hypothesis.

Two narrower guarantees, both gate-enforced rather than asserted:

- A kernel that closed a goal whose *lowering* does not denote the obligation is
  excluded from the badge and named in the report. Closing a different proposition is
  not evidence about this one.
- Agreement never launders past a `trusted` boundary. A trusted obligation stays
  trusted however many kernels run, so a consumer filtering for `proved_by_*` cannot
  mistake a conditional claim for an unconditional one.

Separately, the bit-blasting path's certificate check is corroborated by an
independently implemented checker (drat-trim) — see [AXIOMS.md](AXIOMS.md). That does
not remove the `Lean.trustCompiler` extension; it means a single checker bug cannot
carry an unsound bit-blasting claim unnoticed.

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
