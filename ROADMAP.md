# Concrete Roadmap

This document is the active execution plan. It answers one question: **what should happen next, in what order?**

North star: **systems code with explicit authority, bounded behavior, small trusted boundaries, and a path from compiler-enforced properties to Lean-backed proof, while keeping backend, compiler, toolchain, and target assumptions honest.**

Active phases are numbered 1-13 in dependency order. Task numbering restarts inside each phase. For landed work, see [CHANGELOG.md](CHANGELOG.md). For detailed design, see `docs/` and `research/`.

## Current State

Concrete already has a real Lean 4 compiler pipeline:

`Parse -> Resolve -> Check -> Elab -> CoreCheck -> Mono -> Lower -> EmitSSA -> LLVM IR`

The core language, stdlib foundation, diagnostics, proof/evidence reports, project workflow, adversarial tests, trust-drift demos, and CI evidence gates are real. Recent adversarial compiler bugs are fixed and retained as regressions.

The remaining question is no longer "can Concrete compile programs?" It is:

**Can Concrete prove its thesis with real systems examples, honest trust boundaries, and a usable proof/evidence workflow?**

## First-Release Success Bar

Do not call the language releasable until these are true:

- A flagship example shows explicit authority, predictable/bounded code, at least one Lean-backed property, artifact-backed evidence, and drift detection.
- Bad changes are caught by normal tools: widened authority, new allocation/FFI/blocking, predictable-profile breaks, stale proofs, and obligation/evidence drift.
- Another engineer can audit a function without reading compiler internals.
- At least one second-domain example works, so the thesis is not only a packet-parser story.
- The supported stdlib/syntax/profile surface is narrow, documented, and stable enough for outsiders.
- Trust boundaries are explicit and never mixed.
- The release does not claim a fully verified compiler; it claims Lean-backed evidence for selected user-code properties under explicit assumptions.

## Priority Map

| # | Phase | Goal |
|---:|---|---|
| 1 | Hardening | Make compiler self-checks, test discipline, and bug-capture pipelines normal workflow. |
| 2 | Artifacts | Produce stable, diffable, machine-readable artifacts reviewers and tools can rely on. |
| 3 | Runtime | Define allocation, stack, failure, arithmetic, and concurrency profiles proof/release claims rest on. |
| 4 | Proof | Expand ProofCore coverage, obligation generation, and the provable-subset surface. |
| 5 | Backend | Stabilize backend contracts, target/toolchain model, and incremental build story. |
| 6 | Performance | Benchmark, profile, and budget compile-time and runtime costs. |
| 7 | Flagships | Build the canonical example set the language is defended with. |
| 8 | Release | Public positioning, distribution, supply chain, and the first public release. |
| 9 | Packages | Package manifest, dependency resolution, trust policy, registry. |
| 10 | Editor | LSP, refactoring, artifact viewer, compatibility regression. |
| 11 | Governance | Onboarding, evolution policy, stability boundary, decision process. |
| 12 | Verification | Selected compiler-correctness and pass-preservation proofs. |
| 13 | Research | Speculative ideas gated behind a concrete forcing example. |

## Active Dependency Order

Phases are listed in dependency order, but execution overlaps. The standing rules:

1. **The relevant slices of Phases 1-4 gate each flagship.** Hardening, artifacts, runtime profile, and proof workflow must be honest in the areas the flagship actually touches before it can claim them. The active candidate (rule 2) determines which runtime / proof / artifact surfaces are actually required; not every item in Phases 1-4 gates every flagship — for example, the concurrency sub-track in Phase 3 only gates a flagship that uses concurrency.
2. **Run examples through one linear ladder.** Examples start as inventory entries, then become candidates, then one active pull-through candidate at a time may force gaps in Phases 1-4, and only after its evidence holds does it graduate into Phase 7. `parse_validate` has completed that path and is now the first graduated Phase 7 entry. A second active pull-through candidate does not start until the current one graduates or is explicitly parked with reasons.
3. **Make the compiler artifact-first before broadening surface area.** The current architecture track is ProofCore as a first-class IR, stable artifact identities, pass contracts, and report generation from a shared fact layer. `fixed_capacity` is the forcing candidate for this work because it exposes the ProofCore gaps that block both bounded/no-alloc and real-crypto flagships.
4. **Phase 5 lands when backend honesty matters.** SSA contract, target model, and incremental boundaries before public packaging.
5. **Phase 6 is not on the critical path to the first release.** Benchmarks and budgets help but do not gate it.
6. **Phases 7-8 only after stable evidence.** Phase 7 grows by promotion from the candidate ladder, not by starting many public showcases at once. Release packaging rests on Phases 1-5 plus at least one graduated showcase, not the other way around.
7. **Phases 9-11 only after first-release surface holds.** Packages, editor UX, and public governance assume the artifact / proof / release contracts are stable.
8. **Phase 12 is a trust multiplier, not a prerequisite.** Compiler-correctness proofs come after Phases 4, 5, and the artifact contracts in 2 are stable.
9. **Phase 13 stays last** unless a current example forces a research topic forward.

## Operating Rules

- When a task is completed, move it to [CHANGELOG.md](CHANGELOG.md) and renumber the remaining tasks inside that phase.
- Close phases on concrete outputs (examples, reports, docs, tool surfaces) with explicit success bars, not abstract intent.
- Every flagship example must name its oracle or explicitly state none exists yet.
- Keep examples on the linear ladder: inventory entry -> candidate -> one active pull-through candidate -> graduated Phase 7 flagship; do not maintain several almost-flagships in parallel.
- Refactor compiler architecture only when a candidate makes the boundary undeniable; current forcing candidate is `fixed_capacity`, and the current boundary is `Core -> normalized Core -> ProofCore -> obligations/reports`.
- Judge new language features by grammar cost, audit cost, and proof cost, not expressiveness alone.
- Keep specs in Lean-attached / artifact-registry form until obligations and diagnostics support source-level contracts honestly.
- Build a local fact CLI before MCP / editor integrations.
- Keep QBE and other backend work waiting until proof/evidence attachment, optimization policy, and backend trust boundaries are trustworthy.
- Treat compiler verification as a long-term trust multiplier; do not promise full compiler correctness at first release.
- Parallelize only low-risk inventories and docs while the active implementation path is proof/diagnostic/compiler-contract work.

## Design Constraints

- Keep the parser LL(1).
- Keep SSA as the only backend boundary.
- Prefer stable storage for mutable aggregate loop state over phi transport.
- Avoid parallel semantic lowering paths.
- Keep builtins minimal and implementation-shaped; keep stdlib clean and user-facing.
- Keep trust, capability, and foreign boundaries explicit and auditable.
- Make serious errors and report failures explain themselves: violated rule, source location, why it matters, one plausible next action.

## Current Risks

- Mutable aggregate lowering can still be too backend-sensitive if promoted storage is incomplete.
- Formalization scope is still narrow.
- Type-coercion completeness is not proved, only hardened.
- The linearity checker is tested heavily but not formally audited.

---

## Phase 1: Hardening

Expected outcome: every checker, capability, ownership, predictable, proof, FFI, and concurrency rule has both a positive test and a named regression, and every new compiler bug enters the wrong-code corpus on the day it is found.

1. Wrong-code corpus maintenance: every new bug enters `tests/wrong-code/manifest.toml` with notes on discovery day (live; ongoing).
2. Reducer / minimizer workflow: keep `Concrete.Reduce` + `scripts/tests/minimize_wrong_code.sh` healthy as new predicate kinds arrive (live; ongoing).
3. Bug bundle export: keep `capture_wrong_code_bundle.sh` and `--bundle` integration in sync with new artifact surfaces (live; ongoing).
4. Negative examples as first-class regressions for every checker / capability / ownership / predictable / proof / FFI / concurrency rule (first pair live for parse_validate's pilot: `examples/parse_validate/catches/` + `CATCHES.md` + `make test-catches`; demonstrates capability discipline via authority-widening rejection).
5. Trusted-boundary stress harness: raw-pointer wrappers, FFI ownership transfer, abort/failure cleanup, layout-sensitive wrappers, capability narrowing.
6. Property-based tests for formatter/parser round-trips, stdlib containers, parser cores, report facts.
7. Fuzzing infrastructure where there is an oracle: grammar, structure-aware parser, coverage-guided.
8. Metamorphic differential testing: source-equivalent variants must produce identical outputs, facts, and diagnostics.
9. Sanitizer-backed generated-code validation (ASan/UBSan/LSan) for trusted/FFI/layout/pointer-heavy examples.
10. Targeted differential / codegen tests where there is an executable oracle and a known backend risk.
11. Semantic coverage dashboard: track language constructs, report surfaces, trusted/FFI edges, negative diagnostics, optimizer-sensitive cases, proof/evidence paths.
12. Coverage tooling over tests, report facts, policy checks, obligations, proof artifacts, doc tests.
13. Machine-readable example inventory and candidate pool: classify examples by thesis claim, oracle, proof status, policy / assumption coverage, negative pair, maturity level, and promotion blocker; generate lifecycle and no-duplicate docs from that data.
14. Docs/CLI truthfulness gates: every documented command has a smoke test or an explicit "design only" label.
15. Code formatter robust enough to be the default documentation / example workflow.
16. Documentation-comment extraction and API reference generation from source.
17. Doc tests so code examples in docs and generated reference compile or run as regressions.
18. Lightweight playground / instant-feedback path: single-file compile/run plus effects/predictable/proof-status summaries.
19. Local semantic query CLI over compiler facts (callers/callees, authority paths, proof status, obligation impact, traceability) before MCP/editor integration.
20. MCP server for Claude / ChatGPT / Codex / research agents after the local CLI is useful.
21. Quarantine known external toolchain bugs (e.g. `lli` crashes) from the default developer workflow while retaining named reproductions.
22. Structured logging / tracing / observability primitives for real services: leveled logs, structured fields, spans where justified.
23. Cleanup / destroy ergonomics when examples force it: unified `drop(x)` / Destroy-style API, scoped cleanup helpers, borrow-friendly owner APIs.
24. Widen the semantic-oracle harness to model print/IO only when a flagship needs stdout-level comparison.

## Phase 2: Artifacts

Expected outcome: a function ships with stable, diffable, fingerprinted artifacts that another engineer can audit without reading compiler internals.

1. Pass-by-pass verifier gates: extend the four downstream gates (live: post-elab, post-mono, post-lower, post-cleanup) to earlier boundaries (parsed / resolved / import-summary / post-check) when concrete bugs motivate them.
2. Pass contracts as first-class artifacts: every named compiler boundary states input invariants, output invariants, identity/fingerprint preservation rules, assumptions introduced, and the verifier/report that checks the boundary.
3. Compiler-debuggable dump modes for the important IR boundaries: checked program, Core, normalized Core, ProofCore, obligations, diagnostics, lowering, SSA.
4. Shared fact database for reports: `--report alloc`, `layout`, `caps`, `authority`, `proof`, `unsafe`, `mono`, policy, assumptions, snapshots, and showcase manifests should render from one canonical fact layer instead of recomputing truth independently.
5. Report-consistency checkers over that fact database: reports and interface outputs must agree with each other and with compiler state, with contradictions kept as regressions.
6. Carry the frozen arithmetic policy into reports, diagnostics, and proof/evidence artifacts.
7. End-to-end source traceability across IR boundaries (source → resolved names → checked/elaborated → Core → normalized Core → ProofCore → lowered/SSA/report).
8. Stable identities and deterministic fingerprints for modules, declarations, obligations, diagnostics, reports, proof subjects, policy subjects, assumption subjects, and release-bundle entries.
9. Deterministic artifact serialization and compatibility rules: canonical ordering, hashing scope, schema/version rules, reproducibility.
10. Module / interface artifacts: exported types, signatures, capabilities, proof expectations, policy requirements, fact schema version, dependency fingerprints.
11. Durable frontend artifact boundaries: parsed-file bundles, resolved program/import state, checked program state, normalized proof-target state, and per-stage contracts named explicitly.
12. `CheckedProgram` artifact between checking and elaboration if it materially simplifies plumbing; otherwise reject explicitly.
13. Authority / evidence diffing as a first-class artifact workflow.
14. Assumption files as machine-readable artifacts: target, compiler, backend, OS, toolchain, FFI contracts, trusted regions, proof/evidence assumptions, versioned and diffable (live for v1 — `docs/ASSUMPTION_FILES.md`, first instance at `examples/parse_validate/assumptions.toml`, drift-enforced by `make test-assumptions`).
15. Project policy files for enforceable authority and evidence budgets (no `Unsafe`, no `Alloc`, max stack, required proof/evidence levels) (live for v1 — `docs/POLICY_FILES.md`, schema extends `Concrete.toml` `[policy]`, first instance at `examples/parse_validate/Concrete.toml`, drift-enforced by `make test-policy`).
16. Warning / lint discipline: separate hard errors, warnings, deny-in-CI warnings, advisory lints.
17. ABI/layout round-trip checkers: generate C headers/stubs from public ABI surfaces, verify offsets/size/alignment/calling conventions cross-language.
18. Strengthen memory/layout audit reports with source locations, qualified names, repr/packed/align facts, trusted-pointer boundaries, backend/target caveats.
19. Compiler self-leak / resource soak harness: long-running compile/query/report/rebuild cycles assert no unbounded RSS, FD, temp-file, or subprocess growth.
20. Deepen leak-risk reporting: richer allocation/cleanup path explanations, trusted/FFI leak attribution, precise leak-risk classes.
21. Deepen allocation/leak regression coverage on larger examples; assert `--report alloc` consistency.

## Phase 3: Runtime

Expected outcome: a bounded queue or parser helper can carry claims like "no allocation," explicit overflow policy, and explicit failure-path assumptions, with allocation/stack/concurrency profiles backing them.

1. Arithmetic-overflow policy for predictable/proved profiles versus performance-oriented profiles.
2. Failure-path boundedness: abort, assertions, impossible branches, OOM-excluded profiles, `defer`, drops, cleanup paths.
3. Stack-boundedness reporting and enforcement boundaries.
4. Separate source-level stack-depth claims from backend/target stack claims.
5. Backend and target assumptions for timing, stack, calls, layout, undefined behavior, proof/evidence boundaries.
6. Strengthen `--report alloc` so every user-visible allocation is attributed to a source location and call path.
7. Structural bounded-allocation reports where the compiler can explain the bound.
8. `BoundedAlloc(N)` only where the bound is structurally explainable.
9. Tighter bounded-allocation profile between `NoAlloc` and unrestricted allocation.
10. Const-generics / comptime evaluation only when bounded capacity or artifact generation needs a narrow version of it.
11. Validate predictable execution with bounded examples (fixed-buffer parser, bounded-state controller, fixed-capacity ring buffer).
12. Explicitly defer inline assembly until the backend contract, target model, and trust-boundary story can contain it honestly.
13. Decide the analyzable-concurrency / predictable-execution subset (research anchors in `research/predictable-execution/` and `research/stdlib-runtime/`).
14. Concurrency pressure-test suite: design-only `.con` sketches + expected reports for overlap, required progress, race/select, bounded channels, scope return values, nested scopes, cancellation, FFI blocking, and rejected misuse.
15. Freeze the v1 concurrency surface: capability lattice, scope rules, spawn/join, linear handles, bounded channels, result flow, ownership transfer, rejected forms, `--report concurrency` schema.
16. Implement OS threads + structured scopes + typed bounded channels only after the v1 surface is frozen and pressure-tested.

### Concurrency track (sub-roadmap for items 13-16)

1. Build the design-only pressure-test suite with positive examples (optional-overlap I/O, required-progress producer/consumer, DNS-style race/select, bounded-channel pipelines, scope-returned values, nested scopes) and negative examples (handle leaks, cross-task borrows, detached spawn, scope escape, unbounded channels in bounded profiles, shared mutable state in v1).
2. Write expected `--report concurrency` output for every pressure-test example before implementation.
3. Pressure-test cancellation and trust boundaries separately: FFI blocking, trusted region without checkpoints, cancellable wrapper, cancellation latency reporting.
4. Settle terminology and the capability lattice: optional-overlap name (`Async` / `Overlap` / etc.), `Concurrent` for required progress; defer `Sync` / `Clock` / `Cancellable` / `Tasks(N)` / `Stack(N)` until checks exist.
5. Type-system prerequisites: capability polymorphism for higher-order functions and scope APIs; capability-set subset/union checks; diagnostics.
6. Specify v1 structured scopes: lexical/block-local only, not stored, not returned, not passed around; the only safe spawn context.
7. Specify v1 result and communication flow: parent-child results through `join`; handles cannot escape scope; sibling tasks communicate only through typed bounded channels; no direct scope-to-scope.
8. Specify v1 ownership transfer: owned linear values move into tasks; borrows do not cross task boundaries; channels move owned values; shared mutable state out of v1.
9. Specify the first channel model: `Channel<T, N>` typed bounded capacity, close, sender/receiver ownership, blocking semantics, SPSC/MPSC decision.
10. Mechanize the formal model for v1: scope intro/close, spawn, join, channel send/recv, capability containment, linear handle consumption, theorems for no-leaked-tasks and no-missing-concurrency.
11. Freeze v1 before implementation: capability lattice, scope syntax, spawn/join signatures, channel signatures, rejected forms, evidence terms, report schema.
12. Implement the v1 checker surface: concurrency capabilities, scope IR, scoped `spawn` / `join`, linear `Handle<T, E>`, no escape / cross-task borrow / shared mutable state.
13. Implement the v1 threaded backend: OS threads or small thread pool behind the scoped model; backend inability to provide `Concurrent` is a build error, not silent fallback.
14. Implement v1 typed bounded channels with scope ownership and checker integration.
15. Add the first `--report concurrency`: scope counts, spawn sites, joins, channel capacities, handle consumption, cross-task ownership transfer, backend requirements, evidence levels.
16. Define the analyzable-concurrent profile separately from the default: fixed task set, fixed-capacity channels, bounded waits, no shared mutable state.
17. Add v2 features after v1 is stable: linear-aware race/select, cooperative cancellation, deadlines, `Clock`, `Cancellable`, FFI cancellation-boundary reporting.
18. Add resource-bounded concurrency when the checker can explain the bound source: `Tasks(N)`, per-task `Stack(N)`, integration with heap budgets.
19. Deterministic simulation after threaded model is stable: virtual clock, seeded scheduler, simulated I/O, replayable schedule artifacts, `simulated(N)` evidence.
20. Add evented I/O last, after stack-bound analysis and backend assumptions are strong; must reuse the same scope / capability / channel / handle / evidence model.

## Phase 4: Proof

Expected outcome: helper composition like `fn validate_header(...) -> Bool { ... }` carries a Lean-backed claim that successful return implies multiple structural invariants.

1. Make `ProofCore` a first-class IR with a named compiler boundary, verifier, human-readable dump, regression tests, and a documented extension contract. This is the main compiler architecture track because it is the path from evidence demos to proofs over real systems code.
2. Split the proof path explicitly into `Core -> normalized Core -> ProofCore -> obligations/reports`: normalize early returns, simple control flow, field/array operations, enum construction, and other source conveniences once, then make ProofCore and report generation consume that stable shape.
3. Extend ProofCore in the order forced by `fixed_capacity` and the real-crypto slot: (a) **array indexing** — blocks `parse_header` on `data[i]`, blocks SHA-256 on block-byte access; (b) **bounded while loops** — blocks `compute_checksum`, `compute_tag`, and any real cryptographic round loop; (c) **struct value construction** — blocks `Header { ... }` and any hash-state struct; (d) **enum value construction** — blocks `Result::Ok { value: ... }`; (e) **pattern matching** — blocks `error_code` and tagged-union dispatch; (f) **casts** — blocks byte/int normalization; (g) **field-heavy code** — blocks struct/enum payload proofs. Items (a)–(c) together gate the next Phase 7 real-cryptography flagship slot (HMAC / Ed25519 / constant-time); see Phase 7 item 7.
4. Add ProofCore pass contracts and self-checks: every extraction rule states what Concrete construct it covers, what assumptions it introduces, why rejected constructs are excluded, and which example/regression forces it.
5. Broaden proof obligation generation beyond the first pipeline slice: loop, memory, contract obligations become mechanically inspectable.
6. Broaden the pure Core proof fragment after artifacts, diagnostics, ProofCore phase, normalization, and obligation generation are usable.
7. Deepen the memory / reference proof model: ownership, aliasing, mutation, pointer/reference, cleanup, layout reasoning where examples require.
8. Deepen the effect / trust proof boundaries: prove right up to capability, allocation, blocking, FFI, trusted edges without pretending they disappear.
9. Proof-regression test pipeline: `Core -> normalized Core -> ProofCore`, normalization stability, obligation generation, exclusion reasons, stale proof behavior, proof artifact drift.
10. Stabilize the provable subset as an explicit user-facing target.
11. Public release criteria for the provable subset: supported / unsupported constructs, trust assumptions, artifact stability, what users may rely on.
12. Small reference interpreter for the proof-relevant subset once `Core -> ProofCore` and the memory/UB model are precise.
13. Proof artifact / schema compatibility alongside fact/query schema: proof-status, obligations, extraction, traceability, fingerprints, spec/proof identifiers.
14. Scale proof extraction and obligation generation to larger projects: measure cost, identify bottlenecks, keep tractable.
15. AI-assisted proof repair and authoring on top of stable artifacts, explicit statuses, and kernel-checked validation.
16. Proof replay / caching on top of the artifact model.
17. Selected compiler-preservation proofs where they protect evidence claims.
18. Evaluate contracts / source-level preconditions only after Lean-attached specs, obligations, diagnostics, registry, ProofCore boundary, and proof workflow are real.
19. Evaluate loop invariants only after specs, obligations, and proof UX / repair loop are usable.
20. Evaluate ghost / proof-only code only when a proof-backed example needs it.
21. Pull research-gated language features into implementation only when a current example or proof needs them.

## Phase 5: Backend

Expected outcome: the same pure function and proof artifacts produce equivalent facts, obligations, diagnostics, and outputs across clean builds, incremental builds, and supported backend/target configurations.

1. Define optimization policy: allowed optimizations, evidence-preservation expectations, debug/release behavior, report/codegen validation.
2. Research miscompile-focused differential validation: trustworthy oracles, artifact/codegen consistency checks, backend sanity, smallest high-value wrong-code corpus.
3. Optimization differential runner: same corpus through interpreter, unoptimized/optimized backend paths, `lli`, native; require equal outputs and facts; mismatches become regressions.
4. Research optimization / debug transparency: which transformations need explainable dumps, which passes need validation hooks.
5. Stabilize SSA as the backend contract before experimenting with another backend.
6. Evaluate a normalized mid-level IR only after traceability and backend-contract reports expose a concrete gap.
7. Target / toolchain model: triple, data layout, linker, runtime/startup, libc expectation, clang/llc boundary, sanitizer/coverage hooks.
8. Host-boundary / platform-boundary design note: core / hosted / freestanding expectations, host responsibilities, artifact boundary assumptions.
9. SIMD / vector types and architecture-specific intrinsics only after backend contract and target model are explicit.
10. Sanitizer / source-coverage / LTO / toolchain-integrated optimization support only after the backend contract is explicit.
11. QBE as the first lightweight second backend once backend / source evidence boundaries and optimization policy are explicit; land a small path or record explicit rejection.
12. Cross-backend validation if a second backend lands.
13. Source-level debug-info support when codegen maturity becomes the bottleneck.
14. Explicit WASM target decision: narrow path with honest limits or recorded deferral.
15. Artifact driver layer above the passes: build-graph ownership, cache lookup/store, toolchain/config keys, invalidation policy, reused reports, honest rebuild explanations.
16. Implement incremental compilation artifacts: parsed/resolved/typed/lowered caches, dependency keys, invalidation rules, fact/proof invalidation.
17. Clean-build versus incremental-build equivalence checks: identical facts, obligations, diagnostics, reports, codegen.
18. Compiler-process resource-hygiene checks for long-running workflows: no leaked memory / FDs / temp artifacts / subprocesses.
19. Extend the reducer / minimizer: package-aware, multi-file, richer rewrites, wrong-code / artifact-mismatch predicates.
20. Canonical semantic test matrix: every important rule and guarantee maps to positive / negative / adversarial / artifact-level coverage.

## Phase 6: Performance

Expected outcome: a proof-bearing function ships with benchmark numbers, allocation/leak visibility, and enforceable budgets that CI and review can hold.

1. Stable benchmark harness: selected programs, repeatable runner, baseline artifacts, size/output checks, comparison metadata.
2. Explicit compiler performance budgets: acceptable compile-time regressions, artifact-generation overhead, memory-growth limits enforceable in CI.
3. Compile-time regression profiling: parse/check/elaboration/proof/report time, artifact-generation cost, baseline data.
4. Compiler memory profiling and scaling baselines: peak memory, growth characteristics on larger proof/fact workloads.
5. Runtime / allocation profiling workflow: profiler-friendly output, hot spots, source-location attribution, correlation with `--report alloc`.
6. Large-workspace and many-artifact scaling tests: many modules / facts / obligations / snapshots before package/editor depends on it.
7. Memory-profiler and leak-debug integration for user programs: heap snapshots, allocation tracing where allowed, correlation with `--report alloc`.
8. Agent-readable performance research packet from benchmark, report, proof/evidence, size, and guardrail facts.
9. Explicit AI optimization loop: generate packet, propose patch, run benchmarks, run evidence gates, reject patches that weaken proof/trust/predictability unless requested.

## Phase 7: Flagships

This phase is the **curated public showcase set**, opened only after Phases 1-5 are stable enough to back the claims (per Active Dependency Order rule 5). Examples do not start here; they enter through the Phase 1 inventory / candidate pool, run one at a time as pull-through candidates, and graduate here only after their gaps in 1-5 are closed and their evidence holds up to outside review.

Status: live. `tests/showcase/manifest.toml` is the curated registry; `make test-showcase` is the drift-enforced gate. Graduated entries:

1. `parse_validate` (2026-05-22) — packet/parser flagship. First pull-through pilot. See `examples/parse_validate/AUDIT.md` for the 10-bar contract every flagship must meet.
2. `crypto_verify` (2026-05-23) — toy authenticated-tag model. Graduates the proof scaffolding for authentication, NOT real cryptographic security. The tag function is invertible and the "key" offers no secrecy; the manifest's `limits.algorithm` field states this directly. A real HMAC / Ed25519 / constant-time flagship would be a sibling entry, gated on the Phase 4 ProofCore extensions named below.

Expected outcome: a flagship packet/header validator has explicit authority, one Lean-backed property, report/snapshot/diff coverage, and a release evidence bundle that tells an outsider exactly what is proved and what is assumed.

1. Maintain `parse_validate` as the first packet/parser flagship and use its 10-bar contract as the promotion template for every later showcase.
2. Promote the next packet/parser candidate only after it passes the same ladder (packet / HTTP / DNS / ELF); name the explicit canonical thesis demo with oracle-backed validation.
3. Promote an FFI candidate with a `trusted` wrapper and `with(Unsafe)` isolated at the boundary (libc, checksum/hash, OS call facade, C-ABI library).
4. Promote an ownership-heavy data-structure candidate with linear ownership and deterministic cleanup (ordered map, intrusive list, tree, arena-backed graph).
5. Promote a privilege-separated tool candidate where capability signatures prove the trusted core cannot touch files/network/processes.
6. Promote a fixed-capacity / no-alloc candidate proving the predictable subset is practical (ring buffer, bounded queue, bounded-state controller, fixed parser state machine). Suggested next pull-through candidate — its bounded loops + array indexing + struct construction surface forces the Phase 4 ProofCore extensions named in (7) below.
7. Promote a **real cryptography** candidate (HMAC-SHA256 verification, Ed25519 verification subset, or constant-time tag comparison) ONLY after Phase 4 ProofCore extends to (a) array indexing, (b) bounded while loops, (c) struct construction. Without those, the algorithm's inner loop cannot extract and the proof story would be no better than `crypto_verify`'s toy. The toy graduated 2026-05-23 specifically to register the proof scaffolding; this entry registers the real-algorithm slot the toy does NOT fill.
8. Grow the public showcase corpus linearly from graduated candidates shaped as small / medium / big programs (one property per small, composition per medium, scale per big); must include borrow/aliasing, cleanup/leak-boundary, ownership-heavy, bounded/no-alloc.
9. Keep the curated showcase set balanced: each graduated example proves a different thesis claim with honest framing, report/snapshot/diff coverage, "what the compiler catches," and an oracle when possible.
10. Require capability-shaped APIs in at least one flagship so authority is visible in source/API, not only reports.
11. Privilege-separated capability-first showcase as a core thesis example, not incidental.
12. Showcase maintenance policy: showcase examples are first-class regression targets; framing must stay honest; report/snapshot/diff coverage must be retained.
13. C-replacement examples as a named release bar: at least one C packet validator, one C state machine, one syscall wrapper, one checksum/length helper.
14. Big-workload flagship set: at minimum one protocol/parser security example, one crypto/security proof, one privilege-separated tool, one ownership-heavy medium, one bounded/no-alloc medium.
15. Big-workload flagship quality bar: each has honest proof/trust framing, report/snapshot/diff coverage, an oracle when possible.
16. Focused "Concrete catches this" showcase: examples where C/Rust/Zig/SPARK rely on convention while Concrete rejects or reports the boundary directly; include matching "why not" docs for rejected directions (GC, actors, STM, hidden async, effect handlers, broad inference, detached tasks).

## Phase 8: Release

Expected outcome: the first public language release ships with installable artifacts, a supported subset narrower than the full roadmap, and a public security/disclosure policy.

1. Supported-workload matrix: first-class / showcase-only / research-only workloads separated explicitly.
2. Semantic diff / trust-drift review as a first-class workflow over stable facts and release artifacts: proof-target drift, theorem/attachment drift, claim-scope drift, package-boundary evidence drift.
3. Positioning page against Rust, Zig, Lean 4, SPARK/Ada, Austral, Dafny, F*, Why3.
4. Migration / adoption playbook: what C/Rust/Zig moves first, how to wrap libraries honestly, what stays outside Concrete; C-header scaffolding and stale-proof repair guidance.
5. User-facing documentation set: FAQ for predictable/proof/capability questions, comparison guide, supporting material before the book can stop churning.
6. First public release criteria: supported subset, required examples (small/medium/big), required diagnostics, required proof workflow, stdlib/project UX, evidence/policy/tooling story.
7. Public security and soundness disclosure policy: compiler soundness bugs are security bugs; reporting process for miscompiles, stdlib safety, proof-evidence, trusted-boundary; triage and embargo.
8. Release / install distribution matrix: release binaries, supported host triples, checksums/signing, install paths, first-class vs deferred channels.
9. Reproducible release-build expectations for compiler and distribution artifacts: bit-for-bit scope, allowed variation, verification recipe.
10. Compiler-release supply-chain provenance: signed binaries, checksums, source commit identity, Lean/toolchain identity, build environment metadata.
11. Ship the first real public language release once those criteria are met: versioned honestly, narrower than the full roadmap.
12. Language book / tutorial path only after the first stable subset and release criteria are concrete.
13. REPL and lightweight playground workflow once parser/checker diagnostics and project UX are stable.

## Phase 9: Packages

Expected outcome: a package exporting `pub fn parse_version(...) -> Int` also exports package-level facts about proof status, trusted assumptions, and authority surface that downstream users can review before adoption.

1. Expand packaging / artifacts only after reports, registry, policies, interface artifacts, and CI gates have proved what artifacts must carry.
2. Proof-aware package artifacts: facts, obligations, proof status, trusted assumptions, policy declarations, package-boundary evidence summaries as normal build artifacts.
3. Split interface artifacts from body artifacts at package / workspace scale.
4. Research module-cycle and interface-hygiene enforcement before package-scale hardening.
5. Harden package-aware visibility and encapsulation before package management: public/internal/private API boundaries, exported field policy, sealed/internal modules, accidental-API-leakage diagnostics.
6. Design and parse the package manifest.
7. Build-script / custom-build-logic support only after the manifest is stable: code generation, C library compilation, resource embedding, environment detection — explicit and constrained.
8. Version constraints, dependency resolution, and a lockfile.
9. Workspace and multi-package support.
10. Package-aware test selection.
11. Generate C headers from public C-ABI-facing Concrete declarations.
12. Validate cross-target FFI / ABI from package boundaries.
13. Module / package authority budgets after package graphs are real.
14. Package / runtime boundary artifacts: packages declare what they require from host/runtime/platform.
15. Package discoverability and package-quality gates: docs, examples, trust summaries, compatibility surfaces as part of readiness.
16. Provenance-aware publishing before public distribution.
17. Package registry server protocol and trust model: upload/download, index/search, yanking/deprecation, checksums/signatures, authentication, provenance compatibility.
18. Package / dependency trust policy: how dependencies summarize trusted assumptions; trust widening across boundaries; review and inheritance.

## Phase 10: Editor

Expected outcome: hovering over `fn check_nonce(...) -> Bool` in an editor shows capability status, proof status, predictable status, and theorem/obligation links from the same artifact model.

1. Compiler-as-service / editor / LSP support after diagnostics and facts are structured: parser/checker/report/query entrypoints without forcing executable compilation.
2. LSP feature scope: go-to-definition, hover/type info, diagnostics, formatting, rename, code actions, fact/proof-aware features.
3. Refactoring as an explicit product goal: rename, move, extract-helper, interface extraction, dead-code cleanup preserving or updating facts/proofs.
4. Fact / proof-aware editor UX: capability/evidence hover, predictable/proof status per function, jump/link to obligations, extraction, traceability.
5. Small human-friendly artifact viewer (CLI/TUI/web) for facts, diff, evidence, proof state once schemas stabilize.
6. Dependency auditing for capability, allocation, FFI, trust, evidence, predictability, proof-obligation drift.
7. Docs / tooling UX bar for external users: generated docs, language-server quality, newcomer navigation, project-level discoverability.
8. Canonical "how to use Result well" docs-and-examples surface.
9. Release / compatibility discipline when external users depend on the language.
10. Backwards-compatibility regression corpus once public users exist: old programs / facts / proof artifacts / deprecated syntax remain testable across releases.
11. Explicit language / versioning / deprecation policy across syntax, stdlib, proof/fact artifacts.
12. Stdlib quality gates: API stability, allocation/capability discipline, proof/predictability friendliness; post-freeze Result/Option helper expansion only when function-pointer-in-generic validation justifies helpers like `map_err` / `and_then` / `unwrap_or_else` / `with_context`.
13. `concrete audit` bundle command: one human-readable + machine-readable package of capabilities, unsafe/trusted boundaries, allocation, stack, proof/evidence, FFI, backend/target assumptions, policy violations, authority/evidence drift.

## Phase 11: Governance

Expected outcome: a new user can install Concrete, run one proof-bearing example, inspect its evidence bundle, and understand what is proved / enforced / reported / trusted without reading compiler source.

1. Improve onboarding so a newcomer can build one small program without project-author help.
2. First real `concrete new` bootstrap path with predictable / library / FFI starter templates.
3. Implement or explicitly reject the standalone `--stdlib` bridge for single-file stdlib access.
4. Clean-room outsider workflow CI job: install compiler, create temp project, build/run/check, inspect one artifact without repo-local paths.
5. Audit design-only user-facing docs against the actual shipped CLI/runtime surface; implement or relabel each promised surface.
6. One roadmap / docs / changelog reference migration to the current numbering before consistency gates start enforcing.
7. Define the stability / experimental boundary for public users.
8. Language evolution policy on top of that boundary: edition/versioning rules, deprecation windows, breaking-change policy, graduation rules; post-freeze LL(1)-preserving syntax relief (typed-context generic-construction, modifier-order cleanup) proposed without reopening bare variants, contextual inference, or competing syntaxes.
9. Public governance and decision process: syntax changes, profile changes, stdlib stabilization, breaking changes, security-relevant decisions proposed/reviewed/accepted/documented.
10. Roadmap / docs reference-consistency checks so phase/task references in user-facing docs cannot silently rot.

## Phase 12: Verification

Expected outcome: a simple function like `fn add1(x: Int) -> Int { return x + 1; }` is not only user-proved but backed by proofs that `Core → ProofCore` extraction and selected normalization/preservation steps keep its intended pure meaning intact.

1. Define precisely what "compiler proof" means for Concrete: user-code property proofs, ProofCore semantic proofs, normalization/extraction proofs, pass-preservation proofs, future end-to-end correctness claims.
2. Separate public user-code proof claims from compiler-correctness claims so reports never imply the compiler itself is fully verified.
3. Inventory the compiler-verification trusted base and unproved assumptions: parser, checker, elaborator, CoreCheck, mono, lowering, SSA emission, LLVM/toolchain, runtime, target model, proof registry attachment.
4. Prove normalized Core preserves Core semantics for the supported expression fragment before relying on normalized proof targets as equivalent to source-derived Core.
5. Prove `normalized Core -> ProofCore` extraction sound for the supported constructs.
6. Prove selected checker / report / artifact facts agree with compiler state: proof eligibility, predictable status, capabilities, trusted boundaries, fingerprints, obligations, traceability.
7. Prove small internal compiler invariants: no post-mono type variables, well-formed qualified identities, well-formed SSA facts, consistent interface artifacts, stable diagnostic attachment.
8. Prove selected pass-preservation properties for a restricted pure subset, starting with transformations that directly affect proof/evidence claims.
9. Use the small reference interpreter as executable semantic oracle for the proof-relevant subset.
10. Decide whether full end-to-end compiler correctness is in scope; if yes, define the restricted source subset, target/backend assumptions, proof architecture, explicit non-goals.

## Phase 13: Research

Expected outcome: future ideas (typestate, arena proofs, richer timing models, Miri-style semantic checking) stay clearly gated until Concrete has a stable artifact/proof/evidence foundation.

1. Expand formalization only after obligations, extraction reports, proof diagnostics, attached specs, ProofCore boundary, and broader memory/effect model are artifact-backed.
2. Research typestate only if a current state-machine / protocol example needs it.
3. Research arena allocation after bounded-capacity and allocation-profile work exposes a concrete gap.
4. Research target-specific timing models after source-level predictability and backend boundaries are explicit.
5. Research exact WCET / runtime models only with a target/hardware model.
6. Research exact stack-size claims across optimized machine code only with deeper backend/target integration.
7. Research cache / pipeline behavior as target-level analysis, not a source-language promise.
8. Research binary-format DSLs only if packet/ELF examples show repeated parser boilerplate.
9. Research hardware capability mapping after source-level capabilities and package policies are stable.
10. Research capability sandbox profiles after authority reports and package policies are useful.
11. Broaden the small reference interpreter toward fuller Miri-style UB checking only if the proof-subset interpreter proves valuable.
12. Research persistent equality / rewrite state across phases after the backend contract, semantic diff workflow, and proof/evidence pipeline are stronger.

---

## Reference Map

Thesis: [core-thesis](research/thesis-validation/core-thesis.md), [objective-matrix](research/thesis-validation/objective-matrix.md), [thesis-validation](research/thesis-validation/thesis-validation.md), [validation-examples](research/thesis-validation/validation-examples.md), [predictable-execution](research/predictable-execution/predictable-execution.md), [effect-taxonomy](research/predictable-execution/effect-taxonomy.md), [diagnostic-ux](research/compiler/diagnostic-ux.md), [backend-traceability](research/compiler/backend-traceability.md).

Proof / evidence: [concrete-to-lean-pipeline](research/proof-evidence/concrete-to-lean-pipeline.md), [proving-concrete-functions-in-lean](research/proof-evidence/proving-concrete-functions-in-lean.md), [spec-attachment](research/proof-evidence/spec-attachment.md), [effectful-proofs](research/proof-evidence/effectful-proofs.md), [provable-systems-subset](research/proof-evidence/provable-systems-subset.md), [proof-addon-architecture](research/proof-evidence/proof-addon-architecture.md), [proof-ux-and-verification-influences](research/proof-evidence/proof-ux-and-verification-influences.md), [proof-ux-and-authoring-loop](research/proof-evidence/proof-ux-and-authoring-loop.md), [verification-product-model](research/proof-evidence/verification-product-model.md), [vericoding-and-evidence-product](research/proof-evidence/vericoding-and-evidence-product.md), [evidence-review-workflows](research/proof-evidence/evidence-review-workflows.md), [proof-evidence-artifacts](research/proof-evidence/proof-evidence-artifacts.md), [concurrency-evidence-example](research/proof-evidence/concurrency-evidence-example.md), [concurrency-formal-model](research/proof-evidence/concurrency-formal-model.md).

Language / runtime: [checked-indexing-and-slice-views](research/language/checked-indexing-and-slice-views.md), [arithmetic-overflow-policy](research/language/arithmetic-overflow-policy.md), [opaque-validated-types](research/language/opaque-validated-types.md), [layout-contract-surface](research/language/layout-contract-surface.md), [failure-semantics](research/language/failure-semantics.md), [high-integrity-profile](research/language/high-integrity-profile.md), [memory-ub-boundary](research/language/memory-ub-boundary.md), [trusted-code-policy](research/language/trusted-code-policy.md), [contracts-and-invariants-gating](research/language/contracts-and-invariants-gating.md), [interrupt-signal-model](research/language/interrupt-signal-model.md), [capability-polymorphism](research/language/capability-polymorphism.md), [allocation-budgets](research/stdlib-runtime/allocation-budgets.md), [arena-allocation](research/stdlib-runtime/arena-allocation.md), [execution-cost](research/stdlib-runtime/execution-cost.md), [long-term-concurrency](research/stdlib-runtime/long-term-concurrency.md), [async-concurrency-evidence](research/stdlib-runtime/async-concurrency-evidence.md), [channel-model](research/stdlib-runtime/channel-model.md), [ffi-cancellation-boundary](research/stdlib-runtime/ffi-cancellation-boundary.md), [concurrent-stack-analysis](research/predictable-execution/concurrent-stack-analysis.md).

Compiler / package: [semantic-diff-and-trust-drift](research/compiler/semantic-diff-and-trust-drift.md), [miri-style-interpreter](research/compiler/miri-style-interpreter.md), [persistent-equality-and-rewrite-state](research/compiler/persistent-equality-and-rewrite-state.md), [package-model](research/packages-tooling/package-model.md), [proof-aware-package-boundaries](research/packages-tooling/proof-aware-package-boundaries.md).

Tooling / backend / showcase: [artifact-driven-compiler](research/compiler/artifact-driven-compiler.md), [semantic-query-interface](research/compiler/semantic-query-interface.md), [performance-research-packets](research/compiler/performance-research-packets.md), [developer-tooling](research/packages-tooling/developer-tooling.md), [package-manager-design](research/packages-tooling/package-manager-design.md), [qbe-backend](research/compiler/qbe-backend.md), [qbe-in-concrete](research/compiler/qbe-in-concrete.md), [showcase-workloads](research/workloads/showcase-workloads.md), [adoption-strategy](research/workloads/adoption-strategy.md), [phase-h-findings](research/workloads/phase-h-findings.md).

Multipliers: [ten-x-improvements](research/meta/ten-x-improvements.md), [capability-sandboxing](research/language/capability-sandboxing.md), [trust-multipliers](research/proof-evidence/trust-multipliers.md), [ai-assisted-optimization](research/meta/ai-assisted-optimization.md).
