# Effect and Trust Proof Boundaries

Status: canonical reference — defines exactly where proofs apply, where they weaken into trusted assumptions, and how each effect category interacts with the proof boundary.

For the safety model and three-way split, see [SAFETY.md](SAFETY.md).
For the provable subset definition, see [PROVABLE_SUBSET.md](PROVABLE_SUBSET.md).
For memory/ownership guarantees, see [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md).
For the execution model and runtime boundary, see [EXECUTION_MODEL.md](EXECUTION_MODEL.md).
For the language-semantics vs proof-semantics boundary, see [PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md).

---

## 1. The Central Claim

Concrete's proof story has a sharp boundary:

**Proved territory:** Pure functions with no capabilities, no `trusted` marker, no extern calls, and no entry-point obligations. These are extracted into ProofCore, given formal semantics in `Proof.lean`, and can carry Lean 4–backed theorems.

**Enforced territory:** All safe code (no `trusted`, no `with(Unsafe)`) is covered by the checker's ownership, linearity, borrow, and capability discipline. These properties are mechanically enforced but not yet formally proved.

**Trusted-assumption territory:** Code marked `trusted`, code behind `with(Unsafe)`, and code calling extern functions. The compiler tracks and reports these boundaries but does not verify the correctness of the code inside them.

Everything the compiler says about a program falls into exactly one of these three categories. The rest of this document maps each effect category onto this split.

---

## 2. Effect Categories and Their Proof Status

### Capabilities

Capabilities (`with(File, Network, Alloc, ...)`) are compile-time effect declarations in function signatures. The compiler enforces that callers possess a superset of callee capabilities (checked in both Check and CoreCheck).

| Capability state | Proof status | Evidence level |
|-----------------|--------------|----------------|
| No capabilities (pure) | **Proved** — eligible for ProofCore extraction and Lean theorems | Compiler-enforced + proof-backed |
| Has capabilities | **Enforced** — capability discipline is checked, but function is excluded from proof extraction | Compiler-enforced |
| Has `Unsafe` capability | **Trusted assumption** — `Unsafe` is required for FFI and raw pointer ops; code correctness depends on audit | Compiler-reported |

**Where proofs stop:** At any function with a non-empty capability set. A pure function that calls a function with capabilities cannot itself be proof-eligible — the capability requirement propagates to the caller's signature.

**What the compiler still guarantees:** Capability containment is enforced regardless of proof eligibility. A function cannot silently acquire capabilities it does not declare. `--report caps` and `--report authority` make the full capability graph visible.

### Allocation

Allocation is gated by the `Alloc` capability. Functions that allocate (`alloc`, `vec_new`, etc.) declare `with(Alloc)` and are excluded from proof extraction.

| Allocation state | Proof status | Evidence level |
|-----------------|--------------|----------------|
| No allocation (no `Alloc` cap) | **Proved** (if other gates pass) | Compiler-enforced + proof-backed |
| Allocates with `Alloc` | **Enforced** — linearity ensures allocated resources are consumed or deferred; not proof-eligible | Compiler-enforced |
| Allocation inside `trusted` code | **Trusted assumption** — the trusted wrapper is responsible for correctness | Compiler-reported |

**Where proofs stop:** At the `Alloc` capability boundary. Heap operations (`Heap<T>`, `HeapArray<T>`) are fundamentally effectful and may never enter the pure proof subset.

**What the compiler still guarantees:** Linear ownership of heap pointers. Every `Heap<T>` must be consumed (freed, dereferenced, or passed to an owner). The no-leak guarantee (see [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md)) is enforced by the checker, not by the proof pipeline.

### Blocking and Host Interaction

Host interaction is gated by capabilities: `File`, `Network`, `Process`, `Clock`, `Env`, `Random`, `Console`. The `--check predictable` profile additionally flags blocking I/O (`File`, `Network`, `Process`) as a separate concern.

| Host interaction state | Proof status | Evidence level |
|-----------------------|--------------|----------------|
| No host interaction | **Proved** (if other gates pass) | Compiler-enforced + proof-backed |
| Has host capabilities | **Enforced** — capability discipline checked; excluded from proof extraction | Compiler-enforced |
| Passes `--check predictable` | **Enforced** — no recursion, bounded loops, no alloc, no FFI, no blocking; not proved unless also pure | Compiler-enforced |

**Where proofs stop:** At any host capability. A function with `with(File)` cannot be proof-eligible regardless of what it does internally.

**What the compiler still guarantees:** Capability containment. A function declared `with(File)` cannot silently also use `Network`. The predictable profile gates (no recursion, bounded loops, no alloc, no FFI, no blocking) are enforced for functions that pass `--check predictable`, but this is a report-level check, not a proof-level one.

### FFI (Foreign Function Interface)

Extern functions are the boundary between Concrete and external code. Calling an untrusted extern function requires `with(Unsafe)`. `trusted extern fn` exempts specific audited pure foreign bindings (e.g., `sqrt`, `abs`) from the `Unsafe` requirement.

| FFI state | Proof status | Evidence level |
|-----------|--------------|----------------|
| No extern calls | **Proved** (if other gates pass) | Compiler-enforced + proof-backed |
| Calls `trusted extern fn` | **Trusted assumption** — the binding is audited as pure, but the compiler does not verify the foreign implementation | Compiler-reported |
| Calls untrusted `extern fn` | **Trusted assumption** — requires `with(Unsafe)`; correctness depends entirely on the external code | Compiler-reported |

**Where proofs stop:** At any extern call. Even `trusted extern fn` calls prevent proof extraction, because the compiler has no model of the foreign function's behavior. The `--report proof` heuristic additionally flags functions with extern calls in their bodies.

**What the compiler still guarantees:** FFI type safety — only FFI-safe types are allowed in extern function signatures (checked by `Layout.isFFISafe`). Capability discipline — untrusted extern calls require `with(Unsafe)`. `--report unsafe` shows trust boundaries and what trusted functions wrap.

### Trusted Code

`trusted fn`, `trusted impl`, and `trusted extern fn` contain pointer-level implementation techniques behind safe APIs. `trusted` permits pointer arithmetic, raw pointer dereference, and raw pointer assignment — but does not relax capabilities, linearity, or borrow checking.

| Trust state | Proof status | Evidence level |
|-------------|--------------|----------------|
| Not trusted | **Proved** or **Enforced** depending on other gates | Compiler-enforced (+ proof-backed if pure) |
| `trusted fn` / `trusted impl` | **Trusted assumption** — the code is outside the proof model; correctness depends on audit | Compiler-reported |
| `trusted extern fn` | **Trusted assumption** — audited pure foreign binding; stronger than untrusted extern but still not verified | Compiler-reported |
| Caller of trusted code | **Enforced** — callers see a safe signature; the trust boundary is contained at the declaration site | Compiler-enforced |

**Where proofs stop:** At the `trusted` marker. A trusted function is ineligible for proof extraction regardless of whether it is pure, because it uses pointer-level techniques outside the formal model. Functions from `trusted impl` blocks inherit this exclusion via `trustedImplOrigin`.

**What the compiler still guarantees:**
- Linearity is not relaxed inside trusted code. Linear values follow ownership rules everywhere.
- Capability discipline applies inside trusted code. `trusted` does not suppress capability requirements.
- The `trusted` boundary is contained: callers of trusted code see a normal safe signature and are not themselves excluded from proofs (unless they have other disqualifying properties).
- `--report unsafe` shows exactly which functions are trusted, what operations they contain, and what extern calls they wrap.

---

## 3. The Five Proof Eligibility Gates

ProofCore extraction requires all five gates to pass. Each gate corresponds to one of the effect categories above:

| Gate | Check | Effect category | ProofCore.lean location |
|------|-------|----------------|------------------------|
| Pure | `f.capSet.isEmpty` | Capabilities | `CFnDef.isProofEligible` |
| Not trusted | `!f.isTrusted && f.trustedImplOrigin.isNone` | Trusted code | `CFnDef.isProofEligible` |
| Not entry point | `!f.isEntryPoint` | Host interaction | `CFnDef.isProofEligible` |
| Body extractable | `cExprToPExpr body ≠ none` | All (construct support) | `extractProofCore` |
| Profile gates | No recursion, bounded loops, no alloc, no FFI, no blocking | All effects | `--report proof` heuristic |

Gates 1–3 are checked by the ProofCore extractor. Gate 4 determines whether the function body can be translated into the proof IR (`PExpr`). Gate 5 is the stricter `--report proof` heuristic that additionally flags operational concerns.

A function that passes gates 1–4 is extracted into ProofCore and can carry Lean theorems. Gate 5 is advisory — it identifies functions that are technically extractable but are poor proof targets in practice.

---

## 4. Evidence Levels

Every compiler statement about a program carries one of these evidence levels:

| Level | Meaning | Example |
|-------|---------|---------|
| **Proof-backed** | Property proved in Lean 4 with formal semantics | `abs(x) ≥ 0` for a pure `abs` function |
| **Compiler-enforced** | Property mechanically checked by the compiler (Check, CoreCheck, SSAVerify) but not formally proved | Linear variables consumed exactly once; capability containment |
| **Compiler-reported** | Property visible in compiler reports but not enforced as a hard error | Allocation sites in `--report alloc`; trust boundaries in `--report unsafe` |
| **Trusted assumption** | Property asserted by the programmer via `trusted` or `extern fn`; compiler cannot verify | `trusted extern fn sqrt(x: Float64) -> Float64` is pure |
| **Backend/target assumption** | Property that depends on the backend, linker, OS, or hardware | Stack overflow caught by guard page; `malloc` returns valid memory |

The proof pipeline produces **proof-backed** evidence. The checker produces **compiler-enforced** evidence. Reports produce **compiler-reported** evidence. Everything behind `trusted`/`extern fn` is a **trusted assumption**. Everything below the LLVM IR boundary is a **backend/target assumption**.

---

## 5. Boundary Interactions

### Pure function calling enforced code

A pure function cannot call a function with capabilities. Capabilities propagate — if `f` calls `g` and `g` requires `with(Alloc)`, then `f` must also declare `with(Alloc)`, making `f` ineligible for proof extraction.

This is by design. The proof boundary is a hard wall, not a soft gradient. There is no mechanism to "trust" that a capability-requiring function is "really pure."

### Enforced code calling trusted code

A non-trusted function can call a trusted function. The caller sees a safe signature with declared capabilities. The caller is not itself excluded from enforcement — it is still checked for linearity, capabilities, and borrow correctness. But if the caller is pure and the trusted function requires capabilities, the caller must declare those capabilities and loses proof eligibility.

### Trusted code wrapping FFI

The standard pattern is: `extern fn` (requires `Unsafe`) → `trusted fn` wrapper (audited, declares specific capabilities) → safe caller (sees only the declared capabilities). The trust boundary is contained at the wrapper. See [FFI.md](FFI.md) for wrapper patterns.

### Proof-eligible code inside a larger program

ProofCore is a window, not a wall. Most programs mix proof-eligible pure functions with capability-using, trusted, and FFI code. The `--report proof` tool shows what is eligible and what is excluded. Proofs target the eligible fragment; the rest is covered by testing, reports, and audits.

---

## 6. What the Compiler Reports

| Report | What it shows about proof boundaries |
|--------|--------------------------------------|
| `--report proof` | Which functions are proof-eligible, which are excluded, and why (source + profile reasons) |
| `--report eligibility` | Detailed eligibility assessment with source and profile gates broken out |
| `--report caps` | Per-function capability requirements with "why" traces — shows what makes functions non-pure |
| `--report authority` | Transitive capability chains — shows how capabilities propagate through the call graph |
| `--report unsafe` | Trust boundaries: trusted fn/impl/extern, Unsafe holders, what trusted functions wrap |
| `--report effects` | Combined per-function effect summary: caps, alloc class, recursion, loops, FFI, trusted, evidence level |
| `--check predictable` | Five-gate predictable profile check: no recursion, bounded loops, no alloc, no FFI, no blocking |
| `--report alloc` | Allocation/cleanup summaries: alloc sites, defer patterns, leak warnings |

The `evidence` field in `--report effects` uses the three-level classification from section 4: `"enforced"`, `"reported"`, or `"trusted-assumption"`.

---

## 7. Current Gaps

These are honest limitations, not bugs:

### The proof model covers a small expression subset

ProofCore currently formalizes: integer/boolean literals and arithmetic, comparison, let bindings, if/then/else, and non-recursive function calls. Structs, enums, match, while loops, recursion, and string operations are not yet formalized. This means many proof-eligible functions are extracted but cannot yet carry Lean theorems. See [PROVABLE_SUBSET.md](PROVABLE_SUBSET.md) for the full construct table.

### No cross-function proof composition

Proofs are per-function. There is no mechanism to compose proofs across function boundaries or to verify that a caller's proof assumptions about a callee hold. This is future work.

### No proof of checker soundness

The checker enforces ownership, linearity, borrow, and capability rules, but there is no formal proof that the checker is sound. The guarantees are validated by adversarial tests and code review.

### Predictable profile is report-only

`--check predictable` is a report-level check, not a type-system-level enforcement. A function can fail the predictable profile and still compile. The profile gates are not integrated into the proof pipeline.

### Evidence level is per-function, not per-statement

A function is either proof-eligible or not. There is no mechanism to prove a property about part of a function while trusting the rest. The granularity is the function boundary.

### `trusted extern fn` is audited, not verified

`trusted extern fn` allows calling a foreign function without `with(Unsafe)`, based on the programmer's assertion that the binding is pure. The compiler does not verify this assertion. If the foreign function has side effects or is not safe, the trust boundary is violated silently.

---

## 8. Design Rationale

### Why capabilities gate proof eligibility

Capabilities are the effect system. A function with effects cannot be reasoned about purely — its meaning includes what it does to the world, not just what it returns. Rather than building an effect-aware proof system, Concrete draws the proof boundary at purity: no capabilities, no effects, no proofs about effects.

### Why trusted code is excluded from proofs

Trusted code uses pointer-level techniques (arithmetic, dereference, assignment) that are outside the formal model. Including trusted code in proofs would require formalizing raw pointer semantics, which is a much harder problem. Instead, trusted code is treated as an opaque boundary — proofs stop at it, and its correctness is an audit responsibility.

### Why the boundary is a hard wall

A soft boundary ("this function is mostly pure") would make proof claims ambiguous. By making the boundary binary — either fully proof-eligible or not — the compiler can make unambiguous statements about what is proved and what is not. Users can always refactor code to move pure logic into proof-eligible functions.

### Why evidence levels are explicit

Every compiler output carries an evidence level so that users and auditors can distinguish between "the compiler proved this," "the compiler checked this," and "the programmer asserted this." Without explicit levels, it would be too easy to confuse enforced properties with trusted assumptions.
