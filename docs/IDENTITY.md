# Concrete Identity

Status: stable reference

This document states what Concrete is, what it optimizes for, and what it is not.

For feature admission criteria, see [DESIGN_POLICY.md](DESIGN_POLICY.md). For recorded "no" and "not yet" decisions, see [DECISIONS.md](DECISIONS.md). For long-term shape commitments, see [LANGUAGE_SHAPE.md](LANGUAGE_SHAPE.md).

## What Concrete Is

A small systems language for evidence-carrying software.

Concrete is written in Lean 4 and is designed so capability requirements, predictable execution, resource risk, trust boundaries, and proof evidence can become compiler facts. It is not a proof assistant; it is the no-GC systems language that Lean 4 reasons about.

The bet is that for high-consequence code (firmware, security boundaries, safety-critical components), being able to answer "what authority does this module have?", "how predictable is this core?", "which assumptions are trusted?", and "which claims are proved?" matters more than having a large ecosystem or maximal expressiveness.

The deeper thesis is that Concrete should make four things explicit enough to audit, restrict, and prove:

1. authority
2. operational behavior
3. trust boundaries
4. evidence level

That is the sense in which Concrete is trying to make operational power explicit, not just memory safety or proof objects in isolation.

Concrete is not "Lean without GC." The intended shape is narrower and more practical:

1. a minimalist systems language rather than a theorem-prover surface
2. linear/resource-aware ownership instead of a hidden runtime memory model
3. explicit capabilities and trust markers at boundaries
4. predictable core versus effectful shell as a normal design pattern
5. Lean 4 as both the implementation language of the compiler and the proof environment that can attach real theorems to Concrete code

The design rule is small analyzable core over feature growth. If a feature makes behavior less visible, muddies boundaries, or weakens the proof/audit story, it is suspect by default.

Concrete is not trying to stop at "the compiler can print interesting reports." The intended workflow is artifact-first:

1. the compiler emits stable facts, evidence, proof, and traceability artifacts
2. teams enforce policies from those artifacts
3. CI gates and AI tooling consume those artifacts directly
4. reviewers answer audit questions from those artifacts without reading compiler internals

If Concrete cannot support that workflow, the thesis is only partially successful.

That artifact-first workflow matters because Concrete is trying to be useful for real systems review, not just language research. The compiler should act like an audit machine over ordinary systems code:

1. what authority does this function or module have?
2. is this core predictable or not?
3. what is enforced, what is reported, what is proved, and what is merely trusted?
4. what changed since the last reviewed version?

Concrete is designed for humans first, not for LLMs. But the same explicitness also makes it unusually friendly to LLM-assisted coding, review, and optimization: authority, predictability, trust boundaries, and evidence are surfaced as compiler facts and queries instead of being left implicit in source code.

Short version:

1. Rust makes memory-safety discipline explicit.
2. Zig makes low-level control explicit.
3. SPARK/Ada makes specifications, obligations, and assurance workflow explicit.
4. Lean 4 makes proof a practical implementation and theorem-proving environment.
5. Concrete is trying to make operational power and evidence explicit in native systems code.

## Positioning Against Nearby Tools

Concrete's claim is not that no other tool can prove programs, write systems
code, or enforce safety. The claim is that these pieces are usually separated.

- **Rust / Zig / C / C++** are strong systems languages. Their proof story is
  usually external: tests, fuzzing, sanitizers, static analyzers, model
  checkers, or separate proof tools. The compiler does not normally maintain a
  source/spec/theorem attachment and downgrade evidence when it drifts.
- **SPARK / Ada, Dafny, F*, Why3** have mature verification workflows, but their
  center of gravity is high-assurance verification rather than a small
  C/Rust/Zig-shaped systems language implemented in Lean.
- **Lean / Coq / Isabelle** are excellent proof systems. They are not primarily
  normal low-level systems languages with explicit FFI, layout, authority, and
  no-GC runtime boundaries.
- **Austral** is close on linearity and capability-shaped safety, but it does
  not provide the same Lean-backed proof/evidence pipeline.

The intended Concrete workflow is: write ordinary systems-shaped code, keep
authority and trust visible, attach selected Lean theorems, and let compiler
artifacts catch body/spec/proof drift.

For the explicit map of what Concrete copies, adapts, or rejects from other
languages, see [INFLUENCES.md](INFLUENCES.md).

## The Three-Way Trust Split

Most systems languages have one escape hatch. Rust has `unsafe`. C has... everything. Concrete splits trust into three orthogonal mechanisms:

| Mechanism | What it covers | Visibility |
|-----------|---------------|------------|
| **Capabilities** (`with(File, Network)`) | Semantic effects visible to callers | In function signatures |
| **`trusted`** | Pointer-level implementation unsafety behind safe APIs | At declaration site only |
| **`with(Unsafe)`** | Authority to cross foreign boundaries (FFI, transmute) | In function signatures, even inside trusted code |

**Why this matters:** In Rust, `unsafe` covers raw pointer arithmetic, FFI calls, and transmute with identical syntax. You can't distinguish "contained implementation unsafety behind a safe interface" from "this function crosses a foreign boundary." Concrete makes these syntactically and semantically different, and the compiler reports on them separately.

**What `trusted` specifically permits:** pointer arithmetic, raw pointer dereference, raw pointer assignment, pointer-involving casts. Nothing else. It does NOT suppress capabilities, does NOT permit FFI without `with(Unsafe)`, does NOT relax linearity.

**Nine capabilities:** `File`, `Network`, `Clock`, `Env`, `Random`, `Process`, `Console`, `Alloc`, `Unsafe`. A function can only call functions whose capabilities are a subset of its own.

## The Compiler as Audit Machine

The compiler doesn't just accept or reject programs. It answers questions about them:

- `--report authority` — per-capability function lists with transitive call-chain traces. "Why does `serve` need `File`? Because `serve → log_request → append_file`."
- `--report unsafe` — trust boundary analysis: how many trusted functions, extern functions, unsafe crossings
- `--report alloc` — allocation and cleanup summaries, leak warnings
- `--report proof` — which functions are pure enough to be formally proved (no capabilities, not trusted, no extern calls)
- `--report layout` — struct/enum sizes, alignment, field offsets
- `--report mono` — what monomorphized code actually exists
- `--report interface` — public API surface
- `--report caps` — per-function capabilities
- `snapshot` / `diff` / semantic queries — stable artifacts for CI, review, and AI tools

These are structured compiler outputs, not linting. They derive from the same semantic analysis that type-checks the code. The goal is that a reviewer can understand a Concrete program's authority structure without reading the implementation.

## The Proof Story

The compiler is written in Lean 4. This enables two layers of proof:

**Layer 1 — prove the compiler:** type soundness, ownership/linearity coherence, capability/trust rule preservation, Core-to-SSA lowering correctness. These are proofs that the language rules work as intended.

**Layer 2 — prove user programs:** through formalized Core semantics, a Concrete function's behavior can be stated and proved as a Lean theorem. A hash function written in Concrete (real systems language, real FFI, real memory layout) could be proved correct in Lean (real theorem prover), with a well-defined semantic bridge between them.

The architecture keeps proof tooling separate from compilation. The compiler produces stable artifacts (`ValidatedCore`, `ProofCore`); proof tools consume them. `ProofCore` is the pure subset — functions with no capabilities, not trusted, no extern calls. This is the subset where formal proofs are tractable.

Currently: 17 proven theorems over a pure Core fragment. Narrow, but the architecture is designed to grow without contaminating the compile path.

The important distinction is:

1. Concrete is implemented in Lean 4
2. selected Concrete functions can already carry Lean 4-backed proofs
3. the long-term goal is to enlarge the useful provable subset without turning ordinary systems programming into theorem-prover syntax

## Thesis-Level Direction

Concrete's long-term claim is the combination of:

1. visible authority at the function boundary
2. reportable and enforceable operational behavior such as allocation, blocking, recursion, and boundedness
3. explicit trust boundaries
4. proof-backed evidence tied to compiler artifacts

The normal program shape this pushes toward is:

1. a small pure or tightly-bounded core
2. an effectful shell at the edge
3. explicit authority and trust markers at the seam
4. artifacts that make the seam visible to reviewers and tooling

If that combination works, a reviewer can ask far more precise questions about a function than most mainstream systems languages make practical.

Concrete's vision is only validated if this remains honest and usable in practice:

1. one flagship example demonstrates the full thesis end-to-end
2. bad changes cause visible drift in authority, predictability, trust, or proof status
3. a second example in a different domain also fits the model
4. ergonomics and performance stay acceptable for the target systems use case
5. reports state clearly where claims are enforced, analysis-only, proved, or still trusted at the backend/toolchain boundary

## Biggest Remaining Multipliers

The biggest remaining multipliers for Concrete are not random language features.
They are the parts that most increase the value of the thesis:

1. a stronger flagship systems example with real Lean-backed proof
2. an explicit `ProofCore` boundary as a real compiler artifact
3. module/package policy strong enough to enforce architecture
4. a usable bounded-capacity predictable subset between `NoAlloc` and unrestricted allocation
5. trust-drift and CI evidence gates as normal workflow
6. an AI-native fact interface over stable compiler artifacts
7. later, proof-aware package artifacts so dependencies can ship evidence and policy data as part of the build story

Those are the parts most likely to make Concrete feel qualitatively different
from ordinary systems languages rather than merely more explicit.

## Research Directions

These are the most developed ideas in [research/](../research/). None are implemented yet, but each is grounded in the current compiler architecture.

The current thesis-validation center of gravity is:

1. [../research/thesis-validation/core-thesis.md](../research/thesis-validation/core-thesis.md)
2. [../research/thesis-validation/objective-matrix.md](../research/thesis-validation/objective-matrix.md)
3. [../research/thesis-validation/thesis-validation.md](../research/thesis-validation/thesis-validation.md)

Those notes define the experimental target more directly than the older broad research notes do.

For the narrower question of which verification-language ideas actually fit
Concrete's philosophy, see
[../research/proof-evidence/verification-surface.md](../research/proof-evidence/verification-surface.md).

### Authority Budgets

Capabilities tell you what each function requires. Authority budgets extend this to modules and packages as enforceable constraints:

```con
#[authority(Alloc)]
mod Parser {
    // Any function here that transitively reaches Network or File
    // is a compile error
}
```

At the package level: "dependency `json_parser` may only use `Alloc`." If the next release adds logging, the build fails. The transitive capability set is already computed by `--report authority`; budget checking is set containment.

This is unusual among mainstream systems languages. Rust, for example, has no mechanism to declare that a crate is limited to specific side effects.

See [../research/packages-tooling/authority-budgets.md](../research/packages-tooling/authority-budgets.md).

### Allocation Budgets

`with(Alloc)` is currently binary. The proposal adds gradations: NoAlloc (already works), Bounded (allocates but provably bounded), Unbounded (current default). The first step is classification by call-graph analysis — no language change needed.

The intended direction is more compositional than the usual whole-program or whole-crate story. Ada/SPARK have pool-level bounds; Rust gets `no_std` by convention. Concrete's proposed version is per-function and compositional.

See [../research/stdlib/allocation-budgets.md](../research/stdlib/allocation-budgets.md).

### Execution Cost Tracking

Structural classification of functions: bounded or unbounded loops, recursive or not, max static call depth. For bounded functions, abstract instruction counts via IPET. Concrete is unusually tractable for this: no dynamic dispatch, no closures, no hidden allocation, clean SSA CFG.

See [../research/stdlib/execution-cost.md](../research/stdlib/execution-cost.md).

### Semantic Diff and Trust Drift

Diff trust-relevant properties across two versions — not source text, but semantic facts. Authority changes, new trusted boundaries, allocation shifts. The compiler already computes all these facts; semantic diff is structured comparison of report outputs.

See [../research/compiler/semantic-diff-and-trust-drift.md](../research/compiler/semantic-diff-and-trust-drift.md).

### Proof Addon Architecture

Proof tooling as a separate consumer of compiler artifacts, not fused into compilation. SMT discharge, symbolic execution, and Lean export would all work from the same stable `ValidatedCore`/`ProofCore` artifacts. This avoids making proof failure a compile failure.

See [../research/proof-evidence/proof-addon-architecture.md](../research/proof-evidence/proof-addon-architecture.md).

## Where Concrete Fits

Concrete is not trying to replace Rust, Zig, or C for general-purpose systems programming. Its case is narrower: software that must be small, explicit, reviewable, and honest about power.

Target use cases:

- boot, update, and artifact verification tools
- key-handling and cryptographic policy helpers
- safety/security guard processes with tightly bounded behavior
- industrial control safety interlocks, medical-device policy kernels
- audited wrappers around critical C libraries or hardware interfaces
- cross-domain or high-assurance data-release policy engines

### Competitive Stance

**vs. Rust:** not competing on ecosystem or macro power; competing on auditability, explicit authority, and trust boundaries.

**vs. Zig:** shares low-level explicitness, but pushes harder on ownership, capability tracking, and proof structure.

**vs. verification languages (F\*, SPARK/Ada):** wants the assurance discipline — specs, obligations, proof, trusted assumptions — while keeping low-level runtime, FFI, layout, explicit capabilities, and ownership first-class.

**vs. Lean:** Concrete is not a proof assistant. It is the low-level language that Lean 4 reasons about, and Lean 4 is also the implementation language of the compiler.

The intended relationship is:

1. Lean remains the proof language
2. Concrete remains the systems language
3. the interesting question is how much low-level, no-GC systems code can still carry useful proof-backed evidence

## Non-Goals

- feature-count competition
- hidden semantic behavior keyed off public names
- cleverness that makes auditability harder
- large convenience surfaces inside the compiler
- treating ecosystem size as more important than semantic clarity

## Design Filters

Every proposed feature must pass a checklist (see [DESIGN_POLICY.md](DESIGN_POLICY.md) and [../research/design-filters.md](../research/design-filters.md)):

- Does it make behavior more visible or less visible?
- Is dispatch still statically known?
- Can the compiler own it with one clear pass?
- Does it help or hurt the proof story?
- Is the benefit real for audited low-level code, or just ergonomics?

Guiding principle: "Copy constraints before copying features." The languages Concrete learns from most (Zig, Austral, SPARK) are most useful where they say "no."
