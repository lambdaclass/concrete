# Effect and Trust Proof Boundaries

Status: canonical reference — defines the semantic scopes in which proof and
other evidence apply, and how effect/trust boundaries constrain those scopes.

For the safety model and three-way split, see [SAFETY.md](SAFETY.md).
For the provable subset definition, see [PROVABLE_V1.md](PROVABLE_V1.md).
For memory/ownership guarantees, see [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md).
For the execution model and runtime boundary, see [EXECUTION_MODEL.md](EXECUTION_MODEL.md).
For the language-semantics vs proof-semantics boundary, see [PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md).

---

## 1. The Central Claim

Concrete has a sharp **functional-semantics boundary**, but evidence is not a
three-rung ladder:

**ProofCore functional scope:** Authority-free functions with no `trusted` or
extern boundary and an entirely admitted body may be extracted into ProofCore.
They can carry Lean-backed theorems. Eligibility is not proof, and an empty
capability set does not by itself establish termination or absence of traps.

**Whole-program structural scope:** Capability containment, authority
reachability, allocation/trap reachability, ownership, and other compiler facts
apply to larger programs, including effectful functions. They may be enforced,
reported, tested, or—when an independent checker reconstructs the fact from a
canonical artifact—structurally certified. This does not give effectful
functions ProofCore functional semantics.

**Trusted boundaries:** `trusted`, `Unsafe`, extern code, unsupported indirect
ingress, backends, and targets remain explicit dependencies or completeness
boundaries for the relevant claim.

Per the ratified R-0440 model, a claim must record subject, semantic scope,
method, status, assumptions, producer, validator, trusted dependencies,
freshness, and replay. Its schema migration is pending; current reports still
contain legacy composites. The model can describe one function as
authority-free, compiler-enforced for ownership, kernel-proved for one
ProofCore contract, runtime-checked for overflow, and backend-trusted at the
same time. The rest of this document maps effect categories onto those
dimensions.

---

## 2. Effect Categories and Their Proof Status

### Capabilities

Capabilities (`with(File, Network, Alloc, ...)`) are compile-time effect declarations in function signatures. The compiler enforces that callers possess a superset of callee capabilities (checked in both Check and CoreCheck).

| Capability state | Proof status | Evidence level |
|-----------------|--------------|----------------|
| No capabilities (authority-free) | **Eligible** for ProofCore if every other gate passes; proved only when a linked theorem checks | Compiler-enforced authority fact; optional proof evidence |
| Has capabilities | **Enforced** — capability discipline is checked, but function is excluded from proof extraction | Compiler-enforced |
| Has `Unsafe` capability | **Trusted assumption** — `Unsafe` is required for FFI and raw pointer ops; code correctness depends on audit | Compiler-reported |

**Where functional ProofCore extraction stops:** At any function with a
non-empty capability set. An authority-free function that calls a function
with capabilities acquires that requirement and is no longer eligible.
Program-structure evidence about the larger call graph may still cross that
edge while naming its scope and trust boundary.

**What the compiler still guarantees:** Capability containment is enforced regardless of proof eligibility. A function cannot silently acquire capabilities it does not declare. `--report caps` and `--report authority` make the full capability graph visible.

### Allocation

Allocation is gated by the `Alloc` capability. Functions that allocate (`alloc`, `vec_new`, etc.) declare `with(Alloc)` and are excluded from proof extraction.

| Allocation state | Proof status | Evidence level |
|-----------------|--------------|----------------|
| No allocation (no `Alloc` cap) | **Eligible** if other ProofCore gates pass; not automatically proved | Compiler-enforced authority fact; optional proof evidence |
| Allocates with `Alloc` | **Enforced** — linearity ensures allocated resources are consumed or deferred; not proof-eligible | Compiler-enforced |
| Allocation inside `trusted` code | **Trusted assumption** — the trusted wrapper is responsible for correctness | Compiler-reported |

**Where functional ProofCore extraction stops:** At the `Alloc` capability
boundary. Heap operations (`Heap<T>`, `HeapArray<T>`) are outside the current
functional proof model; allocation authority and reachability remain valid
whole-program structural facts.

**What the compiler still guarantees:** Linear ownership of heap pointers. Every `Heap<T>` must be consumed (freed, dereferenced, or passed to an owner). The no-leak guarantee (see [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md)) is enforced by the checker, not by the proof pipeline.

### Blocking and Host Interaction

Host interaction is gated by capabilities: `File`, `Network`, `Process`, `Time`, `Env`, `Random`, `Console`. The `--check predictable` profile additionally flags blocking I/O (`File`, `Network`, `Process`) as a separate concern.

| Host interaction state | Proof status | Evidence level |
|-----------------------|--------------|----------------|
| No host interaction | **Eligible** if other ProofCore gates pass; not automatically proved | Compiler-enforced authority fact; optional proof evidence |
| Has host capabilities | **Enforced** — capability discipline checked; excluded from proof extraction | Compiler-enforced |
| Passes `--check predictable` | **Enforced** — no recursion, bounded loops, no alloc, no FFI, no blocking; not proof merely because it is authority-free | Compiler-enforced |

**Where functional ProofCore extraction stops:** At any host capability. A
function with `with(File)` is not currently ProofCore-eligible regardless of
what it does internally. Its authority/reachability properties remain eligible
for separately scoped structural evidence.

**What the compiler still guarantees:** Capability containment. A function declared `with(File)` cannot silently also use `Network`. The predictable profile gates (no recursion, bounded loops, no alloc, no FFI, no blocking) are enforced for functions that pass `--check predictable`, but this is a report-level check, not a proof-level one.

### FFI (Foreign Function Interface)

Extern functions are the boundary between Concrete and external code. Calling an untrusted extern function requires `with(Unsafe)`. `trusted extern fn` exempts specific audited pure foreign bindings (e.g., `sqrt`, `abs`) from the `Unsafe` requirement.

| FFI state | Proof status | Evidence level |
|-----------|--------------|----------------|
| No extern calls | **Eligible** if other ProofCore gates pass; not automatically proved | Compiler-enforced boundary fact; optional proof evidence |
| Calls `trusted extern fn` | **Trusted assumption** — the binding is audited as pure, but the compiler does not verify the foreign implementation | Compiler-reported |
| Calls untrusted `extern fn` | **Trusted assumption** — requires `with(Unsafe)`; correctness depends entirely on the external code | Compiler-reported |

**Where proofs stop:** At any extern call. Even `trusted extern fn` calls prevent proof extraction, because the compiler has no model of the foreign function's behavior. The `--report proof` heuristic additionally flags functions with extern calls in their bodies.

**What the compiler still guarantees:** FFI type safety — only FFI-safe types are allowed in extern function signatures (checked by `Layout.isFFISafe`). Capability discipline — untrusted extern calls require `with(Unsafe)`. `--report unsafe` shows trust boundaries and what trusted functions wrap.

### Trusted Code

`trusted fn`, `trusted impl`, and `trusted extern fn` contain pointer-level implementation techniques behind safe APIs. `trusted` permits pointer arithmetic, raw pointer dereference, and raw pointer assignment — but does not relax capabilities, linearity, or borrow checking.

| Trust state | Proof status | Evidence level |
|-------------|--------------|----------------|
| Not trusted | **Eligible** or **enforced** depending on other gates; never automatically proved | Compiler-enforced (+ separately attached proof evidence where present) |
| `trusted fn` / `trusted impl` | **Trusted assumption** — the code is outside the proof model; correctness depends on audit | Compiler-reported |
| `trusted extern fn` | **Trusted assumption** — audited pure foreign binding; stronger than untrusted extern but still not verified | Compiler-reported |
| Caller of trusted code | **Enforced** — callers see a safe signature; the trust boundary is contained at the declaration site | Compiler-enforced |

**Where functional ProofCore extraction stops:** At the `trusted` marker. A
trusted function is ineligible regardless of whether it has an empty capability
set, because its pointer-level behavior is outside the model. Functions from
`trusted impl` blocks inherit this exclusion via `trustedImplOrigin`.

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
| Authority-free | `f.capSet.isEmpty` | Capabilities | `CFnDef.isProofEligible` |
| Not trusted | `!f.isTrusted && f.trustedImplOrigin.isNone` | Trusted code | `CFnDef.isProofEligible` |
| Not entry point | `!f.isEntryPoint` | Host interaction | `CFnDef.isProofEligible` |
| Body extractable | `cExprToPExpr body ≠ none` | All (construct support) | `extractProofCore` |
| Profile gates | No recursion, bounded loops, no alloc, no FFI, no blocking | All effects | `--report proof` heuristic |

Gates 1–3 are checked by the ProofCore extractor. Gate 4 determines whether the function body can be translated into the proof IR (`PExpr`). Gate 5 is the stricter `--report proof` heuristic that additionally flags operational concerns.

A function that passes gates 1–4 is extracted into ProofCore and can carry Lean theorems. Gate 5 is advisory — it identifies functions that are technically extractable but are poor proof targets in practice.

---

## 4. Evidence Dimensions

The canonical evidence model uses orthogonal fields, not one total level.
R-0440 still owns migrating every current producer and renderer:

| Dimension | Examples |
|-----------|----------|
| Subject and claim | function contract, ownership judgment, authority path, trap reachability |
| Semantic scope | source, ProofCore, validated Core, SSA, native artifact, target |
| Method | Lean theorem, kernel decision procedure, compiler enforcement, oracle test, observation |
| Status | current, stale, missing, counterexample, unsupported |
| Trust and replay | producer, validator, trusted dependencies, freshness, replay receipt |

Friendly labels such as `proved_by_lean`, `compiler_enforced`,
`tested_by_oracle`, and `trusted-assumption` remain useful renderings. They do
not form a universal ordering: a narrow ProofCore theorem and a broad native
oracle test establish different claims at different scopes. See
`EVIDENCE_CLASSES.md`, `CLAIM_TAXONOMY.md`, and R-0440.

---

## 5. Boundary Interactions

### Authority-free function calling capability-bearing code

An authority-free function cannot call a function with capabilities.
Capabilities propagate—if `f` calls `g` and `g` requires `with(Alloc)`, then
`f` must also declare `with(Alloc)`, making `f` ineligible for ProofCore
extraction.

This is by design. The functional ProofCore boundary is a hard wall, not a soft
gradient. A separate whole-program theorem may reason about the existence of
that authority edge; it does not turn the effectful function into a ProofCore
function.

### Enforced code calling trusted code

A non-trusted function can call a trusted function. The caller sees a safe
signature with declared capabilities. The caller is not itself excluded from
enforcement—it is still checked for linearity, capabilities, and borrow
correctness. If an authority-free caller reaches a trusted function that
requires capabilities, the caller must declare them and loses ProofCore
eligibility. The trusted edge remains a dependency of any program-structure
claim.

### Trusted code wrapping FFI

The standard pattern is: `extern fn` (requires `Unsafe`) → `trusted fn` wrapper (audited, declares specific capabilities) → safe caller (sees only the declared capabilities). The trust boundary is contained at the wrapper. See [FFI.md](FFI.md) for wrapper patterns.

### Proof-eligible code inside a larger program

ProofCore is a window, not a wall. Most programs mix proof-eligible
authority-free functions with capability-using, trusted, and FFI code. The
`--report proof` tool shows what is eligible and what is excluded. Functional
theorems target the admitted fragment; larger program properties use their own
scope, method, and trust accounting.

---

## 6. What the Compiler Reports

| Report | What it shows about proof boundaries |
|--------|--------------------------------------|
| `--report proof` | Which functions are proof-eligible, which are excluded, and why (source + profile reasons) |
| `--report eligibility` | Detailed eligibility assessment with source and profile gates broken out |
| `--report caps` | Per-function capability requirements with "why" traces — shows what makes functions non-authority-free |
| `--report authority` | Transitive capability chains — shows how capabilities propagate through the call graph |
| `--report unsafe` | Trust boundaries: trusted fn/impl/extern, Unsafe holders, what trusted functions wrap |
| `--report effects` | Combined per-function effect summary: caps, alloc class, recursion, loops, FFI, trusted, legacy evidence summary |
| `--check predictable` | Five-gate predictable profile check: no recursion, bounded loops, no alloc, no FFI, no blocking |
| `--report alloc` | Allocation/cleanup summaries: alloc sites, defer patterns, leak warnings |

The current `evidence` field in `--report effects` is a legacy composite such
as `"enforced"`, `"reported"`, or `"trusted-assumption"`. R-0440 replaces that
single summary with orthogonal scope/method/status/trust fields while retaining
friendly rendering.

---

## 7. Current Gaps

These are honest limitations, not bugs:

### The proof model covers a named subset

`ProvableV1` now includes selected aggregates, arrays, matches, bounded loops,
functional state, and fixed-width operations in addition to scalar
expressions. It still excludes recursion, strings/text, references, allocation,
FFI, trusted code, and arbitrary unmodeled state/control forms. See
[PROVABLE_V1.md](PROVABLE_V1.md) for the canonical allowlist; do not infer
eligibility from an older blanket “no loops/no mutation” rule.

### No cross-function proof composition

Functional ProofCore claims are currently attached per function. There is no
general mechanism that composes arbitrary function contracts into a semantic
whole-program theorem. R-0443 is narrower: it adds an independently checked
program-structure predicate over conservative authority reachability. It must
not be presented as effectful functional correctness.

### No proof of checker soundness

The checker enforces ownership, linearity, borrow, and capability rules, but there is no formal proof that the checker is sound. The guarantees are validated by adversarial tests and code review.

### Predictable profile is report-only

`--check predictable` is a report-level check, not a type-system-level enforcement. A function can fail the predictable profile and still compile. The profile gates are not integrated into the proof pipeline.

### Functional proof attachment is per-function

A function is either proof-eligible or not. There is no mechanism to prove a property about part of a function while trusting the rest. The granularity is the function boundary.

### `trusted extern fn` is audited, not verified

`trusted extern fn` allows calling a foreign function without `with(Unsafe)`, based on the programmer's assertion that the binding is pure. The compiler does not verify this assertion. If the foreign function has side effects or is not safe, the trust boundary is violated silently.

---

## 8. Design Rationale

### Why capabilities gate functional ProofCore eligibility

Capabilities expose authority at the function boundary. The current ProofCore
evaluator does not model external effects, so a capability-bearing function is
outside its functional semantics. Concrete may still prove or independently
check structural facts about effectful programs—such as authority
reachability—without claiming to model the effects themselves.

### Why trusted code is excluded from proofs

Trusted code uses pointer-level techniques (arithmetic, dereference, assignment) that are outside the formal model. Including trusted code in proofs would require formalizing raw pointer semantics, which is a much harder problem. Instead, trusted code is treated as an opaque boundary — proofs stop at it, and its correctness is an audit responsibility.

### Why the boundary is a hard wall

A soft functional boundary (“this function is mostly modeled”) would make a
ProofCore theorem ambiguous. A body is either fully admitted by the named
ProofCore version or it is not. That binary extraction boundary coexists with
other claims at other scopes; it is not a universal evidence ladder.

### Why evidence dimensions are explicit

Every claim records enough dimensions for users and auditors to distinguish
“the kernel proved this ProofCore contract,” “the compiler enforced this source
rule,” “an independent checker validated this artifact predicate,” “an oracle
tested native behavior,” and “the programmer asserted this boundary.” A single
level cannot preserve those differences.
