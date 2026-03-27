+++
title = "Identity"
+++

# Concrete Identity

Status: stable reference

This document states what Concrete is, what it optimizes for, and what it is not.

For feature admission criteria, see [DESIGN_POLICY](@/reference/DESIGN_POLICY.md). For recorded "no" and "not yet" decisions, see [DECISIONS](@/reference/DECISIONS.md). For long-term shape commitments, see [LANGUAGE_SHAPE](@/reference/LANGUAGE_SHAPE.md).

## What Concrete Is

A systems language where capability requirements, trust boundaries, and ownership are visible in every function signature — written in Lean 4 so the compiler itself can be a proof target.

The bet is that for high-consequence code (firmware, security boundaries, safety-critical components), being able to answer "what authority does this module have?" and "which functions are pure enough to prove?" matters more than having a large ecosystem or maximal expressiveness.

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

These are structured compiler outputs, not linting. They derive from the same semantic analysis that type-checks the code. The goal is that a reviewer can understand a Concrete program's authority structure without reading the implementation.

## The Proof Story

The compiler is written in Lean 4. This enables two layers of proof:

**Layer 1 — prove the compiler:** type soundness, ownership/linearity coherence, capability/trust rule preservation, Core-to-SSA lowering correctness. These are proofs that the language rules work as intended.

**Layer 2 — prove user programs:** through formalized Core semantics, a Concrete function's behavior can be stated and proved as a Lean theorem. A hash function written in Concrete (real systems language, real FFI, real memory layout) could be proved correct in Lean (real theorem prover), with a well-defined semantic bridge between them.

The architecture keeps proof tooling separate from compilation. The compiler produces stable artifacts (`ValidatedCore`, `ProofCore`); proof tools consume them. `ProofCore` is the pure subset — functions with no capabilities, not trusted, no extern calls. This is the subset where formal proofs are tractable.

Currently: 17 proven theorems over a pure Core fragment. Narrow, but the architecture is designed to grow without contaminating the compile path.

## Research Directions

These are the most developed ideas in [research/](../research/). None are implemented yet, but each is grounded in the current compiler architecture.

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

See [../research/packages-tooling/authority-budgets](https://github.com/unbalancedparentheses/concrete2/blob/main/research/packages-tooling/authority-budgets.md).

### Allocation Budgets

`with(Alloc)` is currently binary. The proposal adds gradations: NoAlloc (already works), Bounded (allocates but provably bounded), Unbounded (current default). The first step is classification by call-graph analysis — no language change needed.

The intended direction is more compositional than the usual whole-program or whole-crate story. Ada/SPARK have pool-level bounds; Rust gets `no_std` by convention. Concrete's proposed version is per-function and compositional.

See [../research/stdlib-runtime/allocation-budgets](https://github.com/unbalancedparentheses/concrete2/blob/main/research/stdlib-runtime/allocation-budgets.md).

### Execution Cost Tracking

Structural classification of functions: bounded or unbounded loops, recursive or not, max static call depth. For bounded functions, abstract instruction counts via IPET. Concrete is unusually tractable for this: no dynamic dispatch, no closures, no hidden allocation, clean SSA CFG.

See [../research/stdlib-runtime/execution-cost](https://github.com/unbalancedparentheses/concrete2/blob/main/research/stdlib-runtime/execution-cost.md).

### Semantic Diff and Trust Drift

Diff trust-relevant properties across two versions — not source text, but semantic facts. Authority changes, new trusted boundaries, allocation shifts. The compiler already computes all these facts; semantic diff is structured comparison of report outputs.

See [../research/compiler/semantic-diff-and-trust-drift](https://github.com/unbalancedparentheses/concrete2/blob/main/research/compiler/semantic-diff-and-trust-drift.md).

### Proof Addon Architecture

Proof tooling as a separate consumer of compiler artifacts, not fused into compilation. SMT discharge, symbolic execution, and Lean export would all work from the same stable `ValidatedCore`/`ProofCore` artifacts. This avoids making proof failure a compile failure.

See [../research/proof-evidence/proof-addon-architecture](https://github.com/unbalancedparentheses/concrete2/blob/main/research/proof-evidence/proof-addon-architecture.md).

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

**vs. Rust:** Not competing on ecosystem, borrow-checker polish, or macro power. Competing on auditability and explicit authority. Rust's `unsafe` covers everything Concrete splits into three checkable surfaces. Rust has no capability system for declaring that a crate may not do network I/O.

**vs. Zig:** Shares low-level explicitness. Concrete pushes harder on ownership, capability tracking, and proof structure.

**vs. verification languages (F\*, SPARK):** Keeps low-level runtime, FFI, layout, and ownership first-class. Verification-first languages treat these as escape hatches.

**vs. Lean:** Concrete is not a proof assistant. It is the low-level language that Lean 4 reasons about.

## Non-Goals

- feature-count competition
- hidden semantic behavior keyed off public names
- cleverness that makes auditability harder
- large convenience surfaces inside the compiler
- treating ecosystem size as more important than semantic clarity

## Design Filters

Every proposed feature must pass a checklist (see [DESIGN_POLICY](@/reference/DESIGN_POLICY.md) and [../research/design-filters](https://github.com/unbalancedparentheses/concrete2/blob/main/research/design-filters.md)):

- Does it make behavior more visible or less visible?
- Is dispatch still statically known?
- Can the compiler own it with one clear pass?
- Does it help or hurt the proof story?
- Is the benefit real for audited low-level code, or just ergonomics?

Guiding principle: "Copy constraints before copying features." The languages Concrete learns from most (Zig, Austral, SPARK) are most useful where they say "no."
