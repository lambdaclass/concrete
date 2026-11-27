**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27); decides no language rule. Maps to provisional task [R-0735](../../../ROADMAP.md#r-0735) (extending `R-0556`, `R-0149`, `R-0697`, and `R-0201`).**

# 05 — Bodyless `InterfaceSummary`, Content-Addressed `QueryDB`, and Semantic-Delta Auditing

## Problem Statement

Two scaling walls hit every real systems language and every formally verified codebase once it grows past toy programs:

1. **Compile-Time & Proof-Brittleness Explosion:** In mainstream systems compilers (`rustc`, `clang`), name resolution, macros, and cross-module monomorphization create coarse file-level rebuild cascades. In formally verified languages (Lean, Dafny, Verus, SPARK), changing one private line inside a helper function frequently invalidates or re-runs dozens of downstream proofs—which is the single largest reason teams abandon formal verification in production.
2. **Human Audit Bottleneck in the LLM Era:** When LLMs write and refactor Concrete code ([docs/PHILOSOPHY.md](../../PHILOSOPHY.md#what-concrete-is-for)), a textual `git diff` of 1,500 lines buries the only questions an auditor actually cares about: *Did a function gain a capability? Start allocating? Add `trusted`/`unsafe`/FFI? Lose a termination proof? Increase its stack or step bound?*

Concrete's existing architecture makes it uniquely easy to solve **both** problems with one shared data structure: a **canonical, content-addressed `InterfaceSummary`** backed by a **memoized `QueryDB`**.

---

## 1. Bodyless `InterfaceSummary` and Two-Level Merkle Identity

### Current Architecture on `main`
- [Concrete/Stages.lean](../../../Concrete/Stages.lean) runs a monolithic single-file `compile (input : Input) : Except Refusal Output` returning every intermediate stage (`tokens`, `ast`, `core`, `evidence`, `mir`, `memory`, `bir`) coupled to `fileId` and source spans.
- [Concrete/Artifact/Encoding.lean](../../../Concrete/Artifact/Encoding.lean) and [Concrete/Artifact/Sha256.lean](../../../Concrete/Artifact/Sha256.lean) already provide canonical binary encoding and SHA-256 digests.
- [LANGUAGE_AND_COMPILER_ENHANCEMENTS.md](../LANGUAGE_AND_COMPILER_ENHANCEMENTS.md#comp-merkle-incremental) already proposes two-level declaration Merkle hashing (`interface_hash` vs `body_hash`) for `R-0556` and `R-0149`.

### Architectural Upgrade: Make `InterfaceSummary` a First-Class Compiler Artifact

For every declaration `d` (function, struct, enum, trait, const), separate its **semantic interface** from its **private implementation body**:

```lean
structure InterfaceSummary where
  entityId : StableKey               -- compiler-owned semantic identity (Rule 13, C9)
  signature : CanonicalType          -- parameter/return types, generic bounds (no spans/whitespace)
  capabilities : CapabilitySet       -- exact `with(...)` set
  contracts : List CanonicalContract -- #[requires], #[ensures], struct #[invariant]
  layoutPolicy : LayoutSummary       -- fixed ABI or abstract internal layout summary
  facts : ExportedFacts              -- termination, may-abort, allocation, frames, stack/step bounds,
                                     -- trust/FFI surface, proof status + strength label
  deps : List (StableKey × Sha256)   -- `interface_hash` of every referenced declaration
```

From `InterfaceSummary`, compute two deterministic digests via `Artifact/Encoding.lean` + `Sha256.lean`:
1. **`interface_hash := sha256 (encode summary)`** — completely invariant to comments, whitespace, local variable spellings, ANF temporary numbering, and private body implementation!
2. **`body_hash := sha256 (encode canonicalCoreBody ++ interface_hash)`** — changes whenever the implementation body or its internal obligations change.

---

## 2. Query-Driven, Content-Addressed Compilation (`QueryDB`)

Replace monolithic file-at-a-time compilation in [Concrete/Stages.lean](../../../Concrete/Stages.lean) with a **pure, memoized, Salsa-style query graph** whose keys are semantic identities and content hashes rather than file paths and timestamps:

| Query | Depends On | Invalidated By |
|---|---|---|
| `parseFile (fileBytesHash)` | Raw file bytes only | Any edit to that file |
| `extractInterface (entityId)` | Declaration header + `interface_hash` of referenced types/traits | Signature/contract/fact change only |
| `checkBody (entityId)` | Own `body_hash` + callee **`interface_hash`es ONLY** | Own body change or callee **interface** change (NEVER callee body edits!) |
| `lowerFunction (entityId, target)` | Own `body_hash` + callee `interface_hash`es (or `body_hash` if inlined/monomorphized) | Own body change, callee interface change, or inlined callee body change |
| `lookupProof (obligationHash)` | Exact canonical proposition hash + dependency `interface_hash`es | Only when the proposition or an interface dependency changes |

### Soundness Guarantee (Preserving `R-0697` Kernel Checking)
A critical Concrete invariant (`R-0697` in [ROADMAP.md](../../../ROADMAP.md#e-verified-growth)): **caching accelerates lookup and skips redundant re-elaboration/codegen, and NEVER skips Lean kernel checking of proof obligations.**
- If a callee's body changes while its `interface_hash` stays identical:
  - Caller typechecking, ownership checking, fact derivation, and non-inlined separate-compilation codegen are **provably identical** (guaranteed by a **body-independence theorem** `checkBody_depends_only_on_interfaces`) and can be reused directly.
  - Body-dependent artifacts (monomorphized generics, inlined callers, whole-function benchmarks) explicitly record `body_hash` dependencies so stale specializations are **never** reused.
  - External Lean proof lookup matches by exact `(obligationHash, dependencyInterfaceHashes)` and still runs Lean kernel checking (with `R-0724`'s fast modular replay).

### Parallelism for Free
Because `checkBody`, `lowerFunction`, and `ValidatedPass` superoptimization ([doc 03](03-joint-solver-and-llm-verified-opt.md)) depend only on immutable `InterfaceSummary` snapshots, **every function in a project can be checked, lowered, validated, and superoptimized in parallel** across cores or build workers with deterministic, order-independent output.

---

## 3. Zero-Boilerplate Semantic-Delta Auditing (`concrete diff`)

With `InterfaceSummary` already canonical and compiler-owned, **`concrete diff <base> <head>`** (`R-0201`) becomes a tiny, exact, trustworthy structural diff over two `Map StableKey InterfaceSummary` tables:

```text
concrete diff origin/main HEAD

  fn parser::decode_frame (body_hash changed, interface_hash UNCHANGED)
    capabilities : unchanged  (none)
    allocations  : unchanged  (0 bytes)
    trusted/FFI  : unchanged  (none)
    termination  : proved     (unchanged)
    may-abort    : false      (unchanged)
    stack bound  : 192 B -> 128 B  (-64 B, improved)
    step bound   : 4*n + 12 -> 2*n + 8  (improved)
    proof status : proved (refines base: YES, verified via BIR validator)

  VERDICT: 0 regressions, 0 new capabilities, 0 new trust premises.
```

### Why This Changes Code Review for LLM-Written Code
- **Empty semantic delta + proved refinement:** If an LLM rewrites a 500-line kernel to be 3× faster and `concrete diff` confirms `interface_hash` unchanged, zero new capabilities/trust premises, improved stack/step bounds, and kernel-checked equivalence (`R-0732`), a human reviewer can audit the PR in **10 seconds** instead of manually tracing 500 lines of bit-twiddling.
- **Exact regression spotlight:** If a PR quietly adds `with(Fs)`, introduces a `may-abort` path, drops a theorem from `proved` to `not established`, or bumps a loop from `O(n)` to `O(n²)`, `concrete diff` (and `Concrete.toml` ratchets in CI) flags the exact `StableKey` and `FileSpan` immediately.

---

## Why This Will Work

- **Already aligned with Concrete's rules:** Concrete forbids macros, forbids mutable globals, enforces declaration-only dependence ([AT 5](../../PHILOSOPHY.md#admission-test)), owns stable semantic identities (`C9` `StableKey`, Rule 13), and already implements canonical encoding + SHA-256 in `Concrete/Artifact/`.
- **Clean composition with `R-0582` and `R-0734`:** `InterfaceSummary` is the exact interface object that per-module certificates (`R-0582`) and compositional linking (`R-0734` / `R-0705`) bind to—one structure serving incremental compilation, separate verification, and PR auditing ([Rule 5: every fact once](../../PHILOSOPHY.md#the-rules)).

## Slice Staging & Acceptance Gates

| Increment | Slice | Deliverable | Gate |
|---|---|---|---|
| **1. Single-function `InterfaceSummary` + `concrete diff` v0** | `B1` / Slice 3 (optional early) or Slice 8 | Canonical `InterfaceSummary` encoder & SHA-256 hash over existing `concrete facts`; minimal `concrete diff` comparing two `.con` files | Formatting/comment/local-renaming invariance test (identical `interface_hash`), planted capability/abort/stack regression caught |
| **2. Module-level `InterfaceSummary` & body-independence** | Slice 8 (`R-0556`) | Bodyless module interfaces emitting `interface_hash` + `body_hash`; proof that caller checking reads only callee `InterfaceSummary` | Body-only edit leaves downstream `interface_hash` invariant; signature/contract edit invalidates dependents |
| **3. Full `QueryDB` incremental build & audit dossier** | Slice 10 (`R-0201`) & Phase G (`R-0149`) | Memoized `QueryDB` cache with explicit `body_hash` tracking for inlining/monomorphization + `R-0697` mandatory kernel replay | Clean-vs-incremental equivalence matrix (`R-0149`), planted stale-inlined-body and corrupted-cache refusal controls |

**Spec impact:** None. Purely inside `Concrete/` and driver/audit tooling. No owner decision required to implement `R-0735`.
