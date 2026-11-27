**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27); decides no language rule. Maps to provisional task [R-0735](../../../ROADMAP.md#r-0735) (extending `R-0556`, `R-0149`, `R-0697`, and `R-0201`).**

# 05 — Bodyless `InterfaceSummary`, Content-Addressed `QueryDB`, and Semantic-Delta Auditing

## Problem Statement

Two scaling walls hit every real systems language and every formally verified codebase once it grows past toy programs:

1. **Compile-Time & Proof-Brittleness Explosion:** In mainstream systems compilers (`rustc`, `clang`), name resolution, macros, and cross-module monomorphization create coarse file-level rebuild cascades. In formally verified languages (Lean, Dafny, Verus, SPARK), changing one private line inside a helper function frequently invalidates or re-runs dozens of downstream proofs—which is the single largest reason teams abandon formal verification in production.
2. **Human Audit Bottleneck in the LLM Era:** When LLMs write and refactor Concrete code ([docs/PHILOSOPHY.md](../../PHILOSOPHY.md#what-concrete-is-for)), a textual `git diff` of 1,500 lines buries the only questions an auditor actually cares about: *Did a function gain a capability? Start allocating? Add `trusted`/`unsafe`/FFI? Lose a termination proof? Increase its stack or step bound?*

Concrete's existing architecture makes it uniquely easy to solve **both** problems with one shared data structure: a **canonical, content-addressed `InterfaceSummary`** backed by a **memoized `QueryDB`**.

---

## <a id="1-bodyless-interfacesummary-and-two-level-merkle-identity"></a>1. Bodyless `InterfaceSummary`, Derived Facts and Body Identity

### Current Architecture on `main`
- `Registry.pipeline` selects the indexed route and emitter. Its stage inputs are the starting point for query dependencies. [Pipeline](../../../Concrete/Interfaces/Pipeline.lean).
- The artifact modules frame records canonically and compute SHA-256. `Sha256.hash` returns `Option ByteArray`. [Encoding](../../../Concrete/Artifact/Encoding.lean) · [SHA-256](../../../Concrete/Artifact/Sha256.lean).
- [LANGUAGE_AND_COMPILER_ENHANCEMENTS.md](../LANGUAGE_AND_COMPILER_ENHANCEMENTS.md#comp-merkle-incremental) already proposes two-level declaration Merkle hashing (`interface_hash` vs `body_hash`) for `R-0556` and `R-0149`.

### Architectural Upgrade: Make `InterfaceSummary` a First-Class Compiler Artifact

For every declaration, distinguish three canonical values:

1. **Bodyless declaration header.** Record the stable entity key, signature, capabilities, contracts, layout policy and dependency headers. Hash recursive dependency groups together in canonical order so mutually referring declarations do not require each other's completed hash.
2. **Derived facts summary.** Derive or validate exported facts using their body dependencies. Collect termination, may-abort, allocation, frames, stack/step bounds, trust and evidence for each recursive group. The exported interface comprises its declaration header and exported facts.
3. **Canonical body.** Record the implementation and body dependencies independently. A private edit preserves interface identity only when every exported field stays equal.

A digest indexes candidate records. Validate the exact canonical values on a hit and handle hash failure. A body-independence proof uses equality of canonical inputs and injectivity of the encoding (`encode_injective` for its framed domain), without assuming SHA-256 is injective.

---

## 2. Query-Driven, Content-Addressed Compilation (`QueryDB`)

Build pure, memoized queries around `Registry.pipeline` in the indexed compiler. Record each input read by the query. [Pipeline](../../../Concrete/Interfaces/Pipeline.lean).

| Query | Depends On | Invalidated By |
|---|---|---|
| `parseFile` | Source bytes, file identity, grammar and versions | Source or parsing input changes |
| `extractInterface` | Bodyless declaration header and dependency header groups | Any canonical header dependency changes |
| `deriveFacts` / `checkBody` | Body, checked headers, facts and their recursive dependencies, policy and checker version | Any input read by this derivation changes |
| `lowerFunction` | Canonical body, specialized/inlined dependencies, target, profile, policy, codegen version and trap-span table | Any lowering input changes, including diagnostic coordinates |
| `lookupProof` | Exact proposition, environment and declarations read | Recheck the retrieved proof term in the current kernel environment |

### Soundness Guarantee (Preserving R-0697 Kernel Checking)
Reuse a query only after validating all inputs it reads, including versions, target, policy, specialized bodies and source coordinates where relevant. Recheck proof terms against the current declarations. A blank line may preserve typechecking while requiring new trap bytes. Bind BIR/codegen entries to the current trap-span table, or reattach and validate the current spans before emission.

### Parallelism
Independent recursive groups can run concurrently once their inputs are available. Check the result against a clean build, including emitted diagnostics.

---

## 3. Zero-Boilerplate Semantic-Delta Auditing (`concrete diff`)

With `InterfaceSummary` already canonical and compiler-owned, **`concrete diff <base> <head>`** (`R-0201`) becomes a tiny, exact, trustworthy structural diff over two `Map StableKey InterfaceSummary` tables:

```text
concrete diff origin/main HEAD

  fn parser::decode_frame (body hash changed, exported interface changed)
    declaration header hash unchanged; derived facts summary changed
    capabilities : unchanged  (none)
    allocations  : unchanged  (0 bytes)
    trusted/FFI  : unchanged  (none)
    termination  : proved     (unchanged)
    may-abort    : false      (unchanged)
    stack bound  : 192 B -> 128 B  (-64 B, improved)
    step bound   : 4*n + 12 -> 2*n + 8  (improved)
    proof status : callable refinement proved under the recorded premises

  VERDICT: 0 regressions, 0 new capabilities, 0 new trust premises.
```

### Why This Changes Code Review for LLM-Written Code
- **Checked semantic delta.** This example preserves the declaration header and improves two exported bounds. The audit names the changed facts, the proof of refinement and the inputs it covers.
- **Exact regression spotlight:** If a PR quietly adds `with(Fs)`, introduces a `may-abort` path, drops a theorem from `proved` to `not established`, or bumps a loop from `O(n)` to `O(n²)`, `concrete diff` (and `Concrete.toml` ratchets in CI) flags the exact `StableKey` and `FileSpan` immediately.

---

## Why This Will Work

- **Already aligned with Concrete's rules:** Concrete forbids macros, forbids mutable globals, enforces declaration-only dependence ([AT 5](../../PHILOSOPHY.md#admission-test)), owns stable semantic identities (`C9` `StableKey`, Rule 13), and already implements canonical encoding + SHA-256 in `Concrete/Artifact/`.
- **Clean composition with `R-0582` and `R-0734`:** `InterfaceSummary` is the exact interface object that per-module certificates (`R-0582`) and compositional linking (`R-0734` / `R-0705`) bind to—one structure serving incremental compilation, separate verification, and PR auditing ([Rule 5: every fact once](../../PHILOSOPHY.md#the-rules)).

## Slice Staging & Acceptance Gates

| Increment | Slice | Deliverable | Gate |
|---|---|---|---|
| **1. Canonical exported summaries** | Slice 8 (R-0556), subject to OD39 | Separate header, derived-facts and canonical-body identities | An unchanged header with a changed exported bound changes the exported interface |
| **2. Audit diff** | Slice 10 (R-0201) | Compare validated exported summaries, including evidence and bounds | A whitespace edit updates affected trap coordinates; changed capabilities and bounds appear in the diff |
| **3. Incremental cache** | Phase G (R-0149) | Record all inputs and replay proofs under R-0697 | Compare clean and incremental results; reject stale spans, specialized bodies and corrupted records |

**Spec impact:** The cache is compiler tooling. Public generic-body, layout and ABI guarantees remain subject to OD39 under R-0556.
