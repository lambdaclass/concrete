# Concrete Roadmap

This document is the active execution plan. It answers one question: **what should happen next, in what order?**

North star: **systems code with explicit authority, bounded behavior, small trusted boundaries, and a path from compiler-enforced properties to Lean-backed proof, while keeping backend, compiler, toolchain, and target assumptions honest.**

Read the active list with the explicit **Active Dependency Order** below rather than assuming phase letters are a strict execution sequence. The 14 phase letters below are thematic buckets for remaining work, and task numbering restarts inside each phase.

For landed work, see [CHANGELOG.md](CHANGELOG.md). For detailed design, see `docs/` and `research/`.

## Current State

Concrete already has a real Lean 4 compiler pipeline:

`Parse -> Resolve -> Check -> Elab -> CoreCheck -> Mono -> Lower -> EmitSSA -> LLVM IR`

The core language, stdlib foundation, diagnostics, proof/evidence reports, project workflow, adversarial tests, trust-drift demos, and CI evidence gates are real. Recent adversarial compiler bugs are fixed and retained as regressions: LLVM function-name collisions, generic `Copy` struct validation, top-level `main` after inline modules, and newtype-in-enum-payload layout resolution in native/SSA codegen.

The remaining question is no longer "can Concrete compile programs?" It is:

**Can Concrete prove its thesis with real systems examples, honest trust boundaries, and a usable proof/evidence workflow?**

## First-Release Success Bar

Do not call the language releasable until these are true:

- A flagship example shows explicit authority, predictable/bounded code, at least one Lean-backed property, artifact-backed evidence, and drift detection.
- Bad changes are caught by normal tools: widened authority, new allocation/FFI/blocking, predictable-profile breaks, stale proofs, and obligation/evidence drift.
- Another engineer can audit a function without reading compiler internals: what it can touch, whether it is predictable, whether it is proved, what is trusted, and what changed.
- At least one second-domain example works, so the thesis is not only a packet-parser story.
- The supported stdlib/syntax/profile surface is narrow, documented, and stable enough for outsiders.
- Trust boundaries are explicit: compiler-enforced, analysis-reported, Lean-proved, trusted-code, backend/toolchain, and target/runtime assumptions are not mixed.
- The release does not claim a fully verified compiler; it claims Lean-backed evidence for selected user-code properties under explicit compiler, registry, backend, toolchain, and target assumptions.

## Missing Feature Coverage

For fast scanning, the remaining major missing feature surfaces are:

- Language/stdlib pressure workloads before stdlib freeze: parser/decoder, ownership-heavy, borrow-heavy, trusted-wrapper/FFI, and cleanup-heavy pressure workloads.
- Stable stdlib and syntax definition: string/text contract, checked indexing and slice/view rules, opaque validated wrappers, fallible conversion conventions, core-vs-hosted split, Result helpers, endian-aware byte cursors, explicit visibility, arithmetic policy, explicit layout/ABI contracts, and LL(1)-safe destructuring / `let ... else`.
- Tooling, tests, and wrong-code hardening: formatter, doc extraction, doc tests, fuzzing, metamorphic differential testing, sanitizer-backed generated-code checks, trusted-boundary stress suites, reducer/minimizer workflow, crash bundles, named wrong-code corpus, semantic coverage dashboards, and a lightweight playground.
- Stronger runtime/allocation/performance surfaces: allocation reporting, stack reporting, profiling, benchmark harnesses/guardrails, per-pass verifier gates, report-consistency checks, ABI/layout round-trip checks, and compiler self-leak/resource soak harnesses.
- Editor and artifact UX: compiler-as-service, editor/LSP support, and a small human-friendly artifact viewer.
- Package and workspace support: package manifest, workspace/multi-package support, dependency resolution, and lockfile.
- Incremental compilation and equivalence checking: incremental artifacts plus clean-build versus incremental-build equivalence.
- Later-stage backend and trust-multiplier work: optimization differential runners, target decisions such as WASM, second-backend evaluation, and selected compiler verification/preservation proofs.

## Priority Map

| Phase | Tasks | Human goal |
|---|---:|---|
| A. Predictable Core | 1-3 | Make bounded, predictable, failure-aware code usable before broadening scope. |
| B. Pre-Stdlib Pressure Workloads | closed | Use real workloads to discover what the stdlib must actually provide. |
| C. Stdlib and Syntax Freeze | 1-3 | Define, build, polish, and freeze the first-release stdlib, visibility, error, binary parsing, and LL(1) syntax surface. |
| D. Tooling, Tests, and Wrong-Code Corpus | 1-22 | Make examples, docs, formatting, fuzzing, wrong-code capture, minimization, and instant feedback normal workflow. |
| E. Performance, Artifacts, and Contract Hardening | 1-25 | Put budgets, reports, artifacts, and explicit failure behavior around the compiler. |
| F. Release Credibility and Showcase | 1-24 | Prepare honest public positioning, C-replacement validation, and release packaging only after the broader proof/toolchain surface is stable enough to defend publicly. |
| G. Proof Expansion and Provable Subset | 1-25 | Grow ProofCore, obligations, and provable-subset claims only after the surrounding language, stdlib, tooling, and runtime-profile surface is stable enough that the new proofs mean something durable. |
| H. Backend, Target, and Incremental Pipeline | 1-20 | Stabilize backend contracts, targets, incremental builds, and semantic regression coverage. |
| I. Compiler Verification and Preservation Proofs | 1-10 | Scope and prove selected compiler properties only after the proof workflow, backend contract, and artifact semantics are stable enough to justify them. |
| J. Package System and Dependency Trust | 1-16 | Add packages only after artifacts, visibility, and trust summaries know what they must carry. |
| K. Editor, Artifact UX, and Compatibility | 1-13 | Expose facts, diagnostics, refactoring, proof state, artifacts, compatibility tests, and stability policy to users and tools. |
| L. Runtime Profiles, Allocation, and Predictability | 1-16 | Define concurrency, allocation, stack, failure, timing, and overflow boundaries early enough that proof and release claims do not outrun the runtime model. |
| M. Public Readiness and User Tooling | 1-13 | Make the language easier to adopt, audit, govern, and evolve publicly. |
| N. Long-Horizon Research Backlog | 1-12 | Keep speculative language/runtime/research ideas visible but clearly gated. |

## Active Dependency Order

Do not treat the remaining phase letters as a strict execution sequence. They are thematic buckets. Use this dependency order when deciding what to do next:

1. **Make the surrounding language stable enough to prove against**: Phases A and C remain active; Phase B is closed and recorded in the changelog. First finish the semantic-oracle/predictable-core follow-through, then keep stdlib/syntax follow-up work bounded to post-freeze polish.
2. **Make the workflow operationally trustworthy**: Phases D, E, and L. Before broadening proof claims, harden tests, wrong-code capture, artifacts, profiling, allocation/leak reporting, and runtime-profile boundaries.
3. **Broaden the provable subset on top of that stable surface**: Phases G and H. Once the language/runtime/toolchain surfaces stop drifting, expand ProofCore coverage, obligation generation, backend contracts, and incremental/equivalence guarantees.
4. **Only then package the public story**: Phase F, followed by Phases J, K, and M as needed. Public showcases, release criteria, package trust, editor UX, onboarding, and governance should rest on stable evidence rather than aspirational internals.
5. **Treat compiler verification as a later trust multiplier, not a prerequisite for the first credible release**: Phase I comes after the proof workflow, backend assumptions, and artifact contracts are already clear.
6. **Keep research-gated ideas fenced off**: Phase N stays last unless a concrete earlier example forces one of those topics forward.

## Operating Rules

- Follow the **Active Dependency Order** instead of treating phase letters as a strict queue.
- When a task is completed, move it to [CHANGELOG.md](CHANGELOG.md) and renumber the remaining tasks inside that phase.
- Close phases on concrete outputs: examples, reports, docs, or tool surfaces with explicit success bars, not only abstract intent.
- Every canonical or flagship example must name its oracle or explicitly state that no external oracle exists yet.
- Every active phase should have a short exit checklist or equivalent “phase closes when…” surface before it is called done.
- Judge new language features by grammar cost, audit cost, and proof cost, not just expressiveness.
- Do not start package management, new backends, concurrency, broad proof syntax, source-level contracts, package ecosystems, or showcase polish until earlier evidence/diagnostic/tooling steps make them concrete.
- Keep specs in Lean-attached / artifact-registry form until obligations and diagnostics are strong enough to support source-level contracts honestly.
- Build a normal fact CLI before MCP/editor integrations.
- Keep QBE and other backend work waiting until proof/evidence attachment, optimization policy, and backend trust boundaries are trustworthy.
- Treat compiler verification as a long-term trust multiplier after ProofCore, artifact schemas, reference-interpreter work, and backend/target assumptions are stable; do not make full compiler correctness a first-release promise.
- Parallelize only low-risk inventories and docs while the active implementation path is proof/diagnostic/compiler-contract work.

## Active List

The active roadmap starts after the completed compiler-integrity and proof-workflow foundation now recorded in [CHANGELOG.md](CHANGELOG.md). Former pre-letter roadmap phases 1 and 2 are historical; the active remaining work is organized below as Phases A-N, with task numbering restarting in each phase.

### Phase A: Predictable Core

Expected outcome example: a bounded helper like `fn sum4(a0: Int, a1: Int, a2: Int, a3: Int, len: Int) -> Int { ... }` can be shown to run without allocation, recursion, or blocking, with explicit failure and stack assumptions.

1. broaden the semantic-oracle differential harness across all canonical predictable examples; the harness and trust-boundary doc are live (see `tests/oracle/`, `make test-oracle`, `docs/INTERPRETER_TRUST.md`), and the interpreter now supports borrows / `borrowIn` / `?` / string literals / `string_length` / `drop_string`. The harness sits at 56 PASS / 0 FAIL / 1 PENDING. The remaining PENDING is `examples/fixed_capacity` blocked on `print` / `println` / `print_string` and friends — the next concrete interpreter gap is print/IO modeling so the interpreter can match compiled stdout for programs that interleave diagnostics with `fn main() -> Int`

### Phase B: Pre-Stdlib Pressure Workloads

Expected outcome example: real programs such as a JSON subset parser, DNS packet parser, ring buffer, and intrusive list compile cleanly enough to reveal exactly which stdlib, result/error, byte, and ownership APIs are still missing.

No active tasks remain in this phase. Completed pressure-workload history is recorded in [CHANGELOG.md](CHANGELOG.md).

### Phase C: Stdlib and Syntax Freeze

Expected outcome example: parser-facing code such as `let len = cur.read_u16_be();`, string-heavy code such as `grep`/policy output, and runtime-heavy code such as interpreter environments all exist in a stable stdlib/syntax surface that is explicit, teachable, workload-backed, and still LL(1).

Status: the Phase C exit checklist is now closed in `docs/PHASE_EXIT_CHECKLISTS.md` (19/19). Remaining work that still touches these surfaces is follow-up evidence or tooling/polish, not freeze-blocking stdlib/syntax definition.

1. follow-up after the Phase C freeze: carry the arithmetic policy all the way into reports, diagnostics, and proof/evidence artifacts so reviewers can see the active mode directly rather than inferring it from source and policy docs
2. follow-up after the Phase C freeze: expand the library helper layer beyond the shipped Tier 1 floor (`unwrap_or`, `ok`, `err`, `ok_or`) only when function-pointer-in-generic validation makes helpers like `map_err`, `and_then`, `unwrap_or_else`, or `with_context` worth freezing
3. follow-up after the Phase C freeze: revisit remaining syntax friction only through LL(1)-preserving, evidence-driven changes such as typed-context generic-construction relief or modifier-order cleanup; keep bare variants, contextual inference, and competing syntaxes explicitly out

### Phase D: Tooling, Tests, and Wrong-Code Corpus

Expected outcome example: a proved parser helper and an ownership-heavy negative test both live in normal workflows with formatter output, doc examples, fuzz/property hooks where relevant, and named wrong-code regressions if the compiler ever drifts.

1. continue cleanup/destroy ergonomics only when examples force it: unified `drop(x)` / Destroy-style API, scoped cleanup helpers, borrow-friendly owner APIs, and report coverage for cleanup paths
2. add structured logging/tracing/observability primitives for real services and tools: leveled logs, structured fields, spans/events where justified, and an honest split between minimal core APIs and hosted/runtime integrations
3. add a code formatter or make the existing formatter robust enough to be the default documentation/example workflow
4. add documentation-comment extraction and doc generation from source so API reference material is produced from canonical declarations/comments instead of drifting handwritten docs
5. add doc tests so code examples in docs and generated API reference can compile or run as regression tests rather than silently rotting
6. add property-based tests for formatter/parser round-trips, selected stdlib containers, and fixed traces over Vec, String/Text, HashMap, parser cores, and report facts
7. add dedicated fuzzing infrastructure where there is a real oracle: grammar fuzzing, structure-aware parser fuzzing, coverage-guided fuzzing for high-risk surfaces, and a path to keep discovered crashes/miscompiles as stable regressions
8. add targeted differential/codegen tests only where there is an executable oracle and a known backend risk
9. add metamorphic differential testing as a normal wrong-code finder instead of treating it as rare research work: systematically generate source-equivalent program variants (helper vs inlined form, reordered pure lets, match vs `let...else` desugaring, generic vs monomorphized spelling, and similar transformations) and require identical outputs, facts, and diagnostics wherever the language contract says semantics must match
10. add sanitized generated-runtime / generated-code validation modes where the toolchain permits them: run trusted/FFI/layout/pointer-heavy examples under ASan/UBSan/LSan-like configurations, keep target/platform caveats explicit, and turn sanitizer-found bugs into named regressions rather than one-off debugging wins
11. add a semantic coverage dashboard for the compiler corpus: track not only line coverage but language constructs, report surfaces, trusted/FFI edges, negative diagnostics, optimizer-sensitive cases, and proof/evidence paths so blind spots in the suite are visible instead of guessed at
12. add crash/miscompile bundle export as part of the reducer and wrong-code workflow: every crash, verifier failure, report contradiction, or miscompile should be capturable as a stable bundle containing minimized source, expected/actual behavior, flags, relevant dumps, and artifact/report outputs
13. add a dedicated trusted-boundary stress harness instead of letting trusted-wrapper regressions hide inside the general suite: raw-pointer wrappers, FFI ownership transfer, abort/failure cleanup edges, layout-sensitive wrappers, capability narrowing, and similar "tiny trusted shell around dangerous thing" patterns should have explicit adversarial coverage
14. make reducer/minimizer workflow part of normal compiler hardening earlier, not only late backend work: minimize SSA verifier failures, wrong-code cases, fact/report mismatches, and crashers into named regressions; examples should include the `parse_validate` E0703 dominator issue, fixed-capacity drift, and future proof/evidence inconsistencies
15. add a lightweight playground / instant-feedback path before the full REPL: support single-file compile/run plus effects/predictable/proof-status summaries for examples like `clamp_value`, `parse_validate`, and `fixed_capacity` so language shaping is not gated on full project setup
16. build and maintain a named wrong-code regression corpus: every discovered miscompile, codegen bug, obligation bug, checker soundness bug, and proof-pipeline regression should land as a stable reproduction, not just disappear into the general suite
17. add a boring local semantic query CLI over compiler facts before MCP/editor integrations: callers/callees, authority paths, predictable/proof status, obligation impact, artifact subject lookup, and traceability queries should be scriptable, stable enough for shell/CI use, and useful without an IDE
18. add an MCP server for Claude, ChatGPT, Codex, and research agents to query compiler facts after the normal fact CLI is useful
19. add docs/CLI truthfulness gates: every documented command in the book and top-level docs should have a smoke test or an explicit "design only" label, so surfaces like `concrete new` cannot be presented as real before implementation lands
20. quarantine known external toolchain/interpreter bugs from the default developer workflow while retaining named reproductions and native/compiler-level checks; external failures such as `lli` crashes should not make the normal fast path look like a Concrete miscompile
21. make example metadata machine-readable and generate the inventory/lifecycle/no-duplicate docs from it so example status, oracle strategy, promotion state, and phase ownership cannot drift across docs and roadmap
22. make negative examples first-class documentation and regression material: every major checker, capability, ownership, predictable, proof, FFI, and concurrency rule should have at least one small rejected example that explains what Concrete refuses and why

### Phase E: Performance, Artifacts, and Contract Hardening

Expected outcome example: a proof-bearing function ships with stable report artifacts, benchmark numbers, allocation/leak visibility, and compiler dumps that let a reviewer connect source behavior to evidence and performance.

1. define a stable benchmark harness before performance packets: selected benchmark programs drawn from the same small/medium/big workload ladder, repeatable runner, baseline artifacts, size/output checks, and enough metadata to compare patches honestly
2. add explicit compiler performance budgets on top of profiling: acceptable compile-time regressions, artifact-generation overhead, and memory-growth limits that CI and review can enforce
3. add compile-time regression profiling: parse/check/elaboration/proof/report time, artifact-generation cost, and enough baseline data to keep the compiler usable as the proof pipeline grows
4. add compiler memory profiling and scaling baselines: peak memory, artifact-generation overhead, and growth characteristics on larger proof/fact workloads
5. add runtime/allocation profiling workflow: profiler-friendly output, allocation hot spots, allocation-path visibility, source-location attribution, and a path to correlate profiling results with `--report alloc` / evidence artifacts
6. add large-workspace and many-artifact scaling tests: many modules, many facts, many obligations, repeated snapshot/report workflows, and enough volume to expose nonlinear behavior before package/editor use depends on it
7. deepen leak-risk reporting once the first no-leak boundary and leak reports exist: add richer allocation/cleanup path explanations, trusted/FFI leak attribution, and more precise leak-risk classes where the strong no-leak guarantee does not apply
8. deepen allocation/leak regression coverage once the first reporting surfaces exist: adversarial tests for cleanup-path classification, leak-risk classification, trusted/FFI leak boundaries, and `--report alloc` consistency on larger examples
9. define a real warning/lint discipline: separate hard errors, warnings, deny-in-CI warnings, and advisory lints so diagnostics can get stricter without turning every issue into a compile failure
10. add compiler-debuggable dump modes for the important IR boundaries: typed/core IR, ProofCore, obligations, diagnostics, lowering, and SSA should all have stable human-readable dumps suitable for debugging and regression review
11. add pass-by-pass verifier gates across the named compiler boundaries instead of trusting downstream failures to reveal upstream corruption: parsed/resolved/import summaries, checked/elaborated Core, lowered IR, and SSA should each have structural/self-consistency verifiers that run in normal development and CI so the compiler catches "it lied to itself" bugs as early as possible
12. add report-consistency checkers over the existing fact surfaces: `--report alloc`, `layout`, `caps`, `authority`, `proof`, `unsafe`, `mono`, and interface outputs should agree with each other and with the underlying compiler state, with named contradictions kept as regressions rather than debug folklore
13. add ABI/layout round-trip checkers before broader FFI and package claims: generate C headers/stubs from public ABI surfaces, compile tiny foreign callers, and verify field offsets, size/alignment, calling conventions, opaque-vs-guaranteed boundaries, and cross-language enum/newtype behavior on supported targets
14. add a compiler self-leak/resource soak harness before editor/package workflows depend on a long-lived compiler process: repeatedly compile/query/report/rebuild the same corpus and assert no unbounded RSS growth, file descriptor leaks, temp-file leaks, subprocess leaks, or artifact-cache drift
15. produce an agent-readable performance research packet from benchmark, report, proof/evidence, size, and guardrail facts
16. make the AI optimization loop explicit: generate packet, propose patch, run benchmarks, run evidence gates, reject patches that weaken proof/trust/predictability unless requested
17. define and check module/interface artifacts before package management: exported types, function signatures, capabilities, proof expectations, policy requirements, fact schema version, dependency fingerprints, and enough body/interface separation for later incremental compilation
18. define durable frontend artifact boundaries explicitly before more pipeline state gets smeared across passes: parsed-file bundles, resolved program/import state, and the minimum per-stage contracts should be named and stable enough that `ResolvedFilesProgram` or its equivalent is a real boundary instead of an architecture note
19. evaluate and, if it materially simplifies plumbing, caching, and report generation, land a `CheckedProgram` artifact between checking and elaboration; if the extra boundary does not pay for itself, reject it explicitly instead of leaving the shape implicit
20. give modules, declarations, obligations, diagnostics, reports, and proof subjects stable identities and deterministic fingerprints so cache keys, diffs, artifact viewers, and external tools are not forced to key off ephemeral pass-local structure
21. define deterministic artifact serialization and compatibility rules before dumps/JSON become product dependencies: canonical ordering, hashing scope, schema/version rules, reproducibility expectations, and explicit allowances for redacted or host-specific fields
22. add end-to-end source traceability across the important IR boundaries: source -> resolved names -> checked/elaborated forms -> Core/ProofCore -> lowered/SSA/report artifacts, with enough links that a reviewer can connect diagnostics, proof obligations, and codegen behavior back to source without guesswork
23. add authority/evidence diffing as a first-class artifact workflow: show when a function, module, package, or commit gains capabilities such as `File`, `Alloc`, `Unsafe`, trusted assumptions, allocation, proof obligations, or weaker evidence levels
24. add explicit assumption files as machine-readable artifacts: target, compiler, backend, OS, toolchain, FFI contracts, external libraries, trusted regions, and proof/evidence assumptions should be versioned and diffable
25. add project policy files for enforceable authority and evidence budgets: examples include no `Unsafe`, no `Alloc`, max stack, only these capabilities, no trusted functions outside `trusted/`, and required proof/evidence levels for selected modules

### Phase F: Release Credibility and Showcase

Expected outcome example: a flagship packet/header validator has explicit authority, one Lean-backed property, report/snapshot/diff coverage, and a release evidence bundle that tells an outsider exactly what is proved and what is assumed.

1. expand packaging/artifacts only after reports, registry, policies, interface artifacts, and CI gates have proved what artifacts must carry
2. define proof-aware package artifacts explicitly: packages should eventually ship facts, obligations, proof status, trusted assumptions, policy declarations, and package-boundary evidence summaries as normal build artifacts
3. build and curate a broader public showcase corpus after the thesis workflow is credible, and shape it deliberately as small, medium, and big programs rather than a pile of demos: small programs should isolate one property, medium programs should test composition, and a few bigger programs should prove the language survives scale; the corpus must include borrow/aliasing programs, cleanup/leak-boundary programs, at least one ownership-heavy medium example, and at least one bounded/no-alloc medium example, not only parsers and containers
4. turn the showcase corpus into a curated showcase set where each example proves a different thesis claim, each has honest framing, each has report/snapshot/diff coverage, each demonstrates at least one concrete thing the compiler catches, and each is chosen with an oracle in mind when possible: fuzzing, differential testing, round-trip properties, model-based tests, or comparison against another mature implementation/spec; include explicit borrow/aliasing, cleanup/leak-boundary, privilege/authority, ownership-heavy, and bounded/no-alloc examples in that quality bar
5. require capability-shaped APIs in at least one flagship example so authority passing and narrowing are visible in the source/API surface, not only in reports
6. require one privilege-separated capability-first showcase in the public corpus so capability discipline is demonstrated as a core thesis example rather than an incidental side property
7. publish a supported-workload matrix before the first major release: explicitly separate first-class supported workloads, showcase-only workloads, and research-only workloads so the public claim matches the actual language surface
8. harden semantic diff / trust-drift review into a first-class workflow over stable facts and package/release artifacts, not just a research note or one-off diff tool: this should grow to include proof-target drift, theorem/attachment drift, claim-scope drift, package-boundary evidence drift, and a reviewer-facing proof-diff story that answers what changed, what is still proved, and which assumptions moved
9. sharpen the positioning against Rust, Zig, Lean 4, SPARK/Ada, Austral, Dafny, F*, and Why3 into one short page
10. write the migration/adoption playbook: what C/Rust/Zig code should move first, how to wrap existing libraries honestly, how to introduce Concrete into an existing system, and what should stay outside Concrete; include C-header scaffolding and stale-proof repair suggestions before broader migration automation
11. build the user-facing documentation set deliberately: a FAQ for predictable/proof/capability questions, a Concrete comparison guide against Rust, Zig, SPARK/Ada, Lean 4, and related tools, and the supporting material needed before the language book can stop churning
12. define the showcase maintenance policy: showcase examples are first-class regression targets, must keep honest framing, must retain report/snapshot/diff coverage, and regressions in them count as serious thesis breaks; maintain the small/medium/big balance rather than letting the corpus collapse into only tiny demos
13. make mechanically auditable C-replacement examples a named release bar: replace at least one C packet validator, one C state machine, one syscall wrapper, and one checksum/length-check helper with Concrete examples that have a smaller trusted surface and richer report/evidence output than the original C versions
14. build a named big-workload flagship set, not just small pressure examples: at minimum one real protocol/parser security example, one crypto/security proof example, one privilege-separated tool, one ownership-heavy medium program, and one bounded/no-alloc medium program; the set should be explicit enough that no core thesis area is represented only by snippets
15. require each big-workload flagship to have honest proof/trust boundary framing, report/snapshot/diff coverage, and an oracle when possible (fuzzing, differential testing, round-trip properties, or model-based checks) so they function as real validation workloads instead of marketing demos
16. define first public release criteria: the first stable supported subset, required examples across small, medium, and big workloads, required diagnostics, required proof workflow, required stdlib/project UX, and the minimum evidence/policy/tooling story for outsiders; the example bar must explicitly include parser/decoder, ownership-heavy, borrow/aliasing, trusted-wrapper/FFI, fixed-capacity, cleanup/leak-boundary, the named big-workload programs, the named profile surfaces, and a release evidence contract for each flagship example (proof/report/diff artifacts plus explicit assumptions)
17. define a public security and soundness disclosure policy before first release: compiler soundness bugs are security bugs, and users need a clear process for reporting miscompiles, stdlib safety bugs, proof-evidence bugs, trusted-boundary issues, plus expected triage and embargo handling
18. define the release/install distribution matrix before the first real public release: release binaries, supported host triples, checksums/signing, install paths, and which distribution channels are first-class versus deferred
19. define reproducible release-build expectations for the compiler and distribution artifacts: what must be bit-for-bit reproducible, what may vary, how rebuilds are verified, and how non-reproducible components are documented
20. define compiler-release supply-chain provenance: signed release binaries, checksums, source commit identity, Lean/toolchain identity, build environment metadata, and verification instructions for users
21. ship the first real public language release once those criteria are actually met: version the release honestly, publish the supported subset and known limits, ship installable artifacts, and make the release promise narrower than the full roadmap
22. write the real language book/tutorial path only after the first stable supported subset and first public release criteria are concrete enough that teaching the language will not churn with every compiler refactor
23. add a REPL and lightweight playground workflow once the parser/checker diagnostics and project UX are stable enough that quick experimentation will reflect the real language instead of a toy front-end
24. add a focused "Concrete catches this" showcase: examples where C, Rust, Zig, or SPARK/Ada would rely on convention, review discipline, or external tooling, while Concrete rejects the program or reports the authority/trust/evidence boundary directly; include matching "why not" docs for rejected language directions such as GC, actors, STM, hidden async, effect handlers, broad inference, and detached tasks

### Phase G: Proof Expansion and Provable Subset

Expected outcome example: helper-composition code such as `fn validate_header(...) -> Bool { if !check_magic(...) { return false; } return check_class(c); }` can carry a Lean-backed claim that successful return implies multiple structural invariants, not just one tiny helper fact.

1. polish the packet/parser flagship example as the canonical thesis demo; likely candidates are the packet, HTTP, DNS, or ELF parser surfaces, but at least one must become the explicit flagship with oracle-backed validation
2. build an FFI showcase with a `trusted` wrapper and `with(Unsafe)` isolated at the boundary; good candidates are a libc wrapper, checksum/hash wrapper, OS call facade, or C-ABI library example
3. build an ownership-heavy data-structure showcase with linear ownership and deterministic cleanup; likely candidates are an ordered map, intrusive list, tree, or arena-backed graph
4. build a privilege-separated tool where capability signatures prove the trusted core cannot touch files/network/processes; it should be a real medium program, not only a tiny policy toy
5. build a fixed-capacity / no-alloc showcase that proves the predictable subset is practical for real bounded systems code; likely candidates are a ring buffer, bounded queue, bounded-state controller, or fixed parser state machine
6. build a real cryptography example only after the proof/artifact boundary is stronger: good candidates are constant-time equality + verification use, an HMAC verification core, an Ed25519 verification helper/core subset, or hash/parser/encoding correctness around a crypto-adjacent component; the goal is to prove the system is useful on security-sensitive code rather than only format parsing
7. refine and stabilize the explicit `Core -> ProofCore` phase after the flagship has forced it into the open: keep the extraction semantics small, testable, and shared by obligations, specs, proofs, and future proof tools
8. extend ProofCore and its semantics to cover more real Concrete constructs in a principled order: structs/fields, pattern matching, arrays/slices, borrows/dereferences, casts, cleanup/defer/drop behavior, and other constructs the flagship examples actually force into scope
9. broaden proof obligation generation beyond the first pipeline slice so loop-related, memory-related, and contract-related proof work becomes mechanically inspectable instead of ad hoc
10. broaden the pure Core proof fragment after proof artifacts, diagnostics, the explicit ProofCore phase, normalization, and obligation generation are usable
11. deepen the memory/reference model for proofs once the first explicit version exists: sharpen ownership, aliasing, mutation, pointer/reference, cleanup, and layout reasoning where real examples require it
12. deepen the effect/trust proof boundaries once the first explicit version exists: prove more right up to capability, allocation, blocking, FFI, and trusted edges without pretending the edges disappear
13. add a dedicated proof-regression test pipeline covering `Core -> ProofCore`, normalization stability, obligation generation, exclusion reasons, stale proof behavior, and proof artifact drift
14. stabilize the provable subset as an explicit user-facing target
15. define public release criteria for the provable subset: supported constructs, unsupported constructs, trust assumptions, proof artifact stability expectations, and what evidence claims users may rely on semantically
16. build a small reference interpreter for the proof-relevant subset once the `Core -> ProofCore` boundary and memory/UB model are precise enough: use it as a semantic oracle for the restricted subset, compare interpreter results against proof semantics and compiled behavior, and keep it intentionally smaller and more trustworthy than the full compiler
17. stabilize proof artifact/schema compatibility alongside the fact/query schema: proof-status, obligations, extraction, traceability, fingerprints, spec identifiers, and proof identifiers need explicit compatibility rules before external users or tools depend on them
18. make proof extraction and obligation generation scale to larger projects without collapsing usability: measure cost, identify bottlenecks, and keep the proof workflow tractable as the codebase grows
19. add AI-assisted proof repair and authoring support only on top of stable proof artifacts, explicit statuses, and kernel-checked validation: suggestions may help with stale-proof repair, attachment updates, and theorem scaffolding, but the trust anchor must remain Lean checking plus compiler artifact validation
20. add proof replay/caching on top of the artifact model so unchanged proof targets, fingerprints, and obligations do not have to be recomputed or revalidated from scratch in every workflow
21. push selected compiler-preservation proofs where they protect evidence claims
22. evaluate contracts / source-level preconditions only after Lean-attached specs, obligations, diagnostics, the registry work, the explicit ProofCore boundary, and the built-in proof workflow are real enough to support them honestly
23. evaluate loop invariants only after specs, proof obligations, and the proof UX/repair loop are real enough that users can diagnose failures without compiler-internal knowledge
24. evaluate ghost/proof-only code only after a proof-backed example needs it and the erasure story is explicit
25. pull research-gated language features into implementation only when a current example or proof needs them

### Phase H: Backend, Target, and Incremental Pipeline

Expected outcome example: the same pure function and proof artifacts produce equivalent facts, obligations, diagnostics, and outputs across clean builds, incremental builds, and supported backend/target configurations under documented assumptions.

1. define optimization policy before substantial backend work: allowed optimizations, evidence-preservation expectations, debug/release behavior, and report/codegen validation expectations
2. research miscompile-focused differential validation before implementing it broadly: identify trustworthy oracles, artifact/codegen consistency checks, backend sanity checks, and the smallest high-value wrong-code detection corpus
3. add an optimization differential runner once the first backend policy and oracles are clear: run the same corpus through the interpreter, unoptimized backend paths, optimized backend paths, `lli`, and native binaries where applicable, require equal outputs and equal fact/report surfaces wherever the contract says they should match, and keep every mismatch as a first-class wrong-code regression
4. research optimization/debug transparency before deeper backend work: which transformations need explainable dumps, which passes need validation hooks, and how optimized/unoptimized evidence should be related without overclaiming
5. stabilize SSA as the backend contract before experimenting with another backend
6. evaluate a normalized mid-level IR only after traceability and backend-contract reports expose a concrete gap between typed Core and SSA; do not add a Rust-MIR-sized layer by default
7. define a target/toolchain model before serious cross-compilation: target triple, data layout, linker, runtime/startup files, libc/no-libc expectation, clang/llc boundary, sanitizer/coverage hooks, and target assumptions
8. write a host-boundary / platform-boundary design note before broader runtime and package claims: make core vs hosted vs freestanding expectations, host/runtime responsibilities, and artifact boundary assumptions explicit without importing Roc’s managed-effect model
9. evaluate SIMD/vector types and architecture-specific intrinsics only after the backend contract and target/toolchain model are explicit: decide portable-vs-target-specific surface, proof/predictability implications, and whether the feature belongs in core language, stdlib, or trusted boundary
10. evaluate sanitizer, source-coverage, LTO, and toolchain-integrated optimization support only after the backend contract and target/toolchain model are explicit
11. evaluate QBE as the first lightweight second backend once backend/source evidence boundaries and optimization policy are explicit; either land a small path, record a clear rejection, or document why another backend would be warranted instead
12. add cross-backend validation if a second backend lands
13. add source-level debug-info support when codegen maturity becomes the bottleneck
14. make the target/toolchain model concrete enough to support an explicit WASM target decision: either land a narrow WASM path with honest runtime/tooling limits or record a clear deferral with reasons
15. build a real artifact driver layer above the passes before relying on incremental compilation broadly: explicit build-graph ownership, cache lookup/store, toolchain/config keys, invalidation policy, reused reports/artifacts, and honest rebuild explanations should live in the driver rather than as accidental pass plumbing
16. implement incremental compilation artifacts after report/proof/policy/interface artifacts are well-shaped: parsed/resolved/typed/lowered caches, dependency keys, invalidation rules, fact/proof invalidation, and clear rebuild explanations
17. add clean-build versus incremental-build equivalence checks: the same source and toolchain state must produce identical facts, obligations, diagnostics, reports, and codegen outputs whether built from scratch or through incremental caches
18. add compiler-process resource-hygiene checks for long-running workflows: repeated report/query/snapshot/incremental runs should not leak memory, file descriptors, temp artifacts, or subprocess state
19. extend the first reducer/minimizer into a broader workflow: add package-aware and multi-file reduction, richer syntax-aware rewrites, and wrong-code / artifact-mismatch predicates on top of the landed single-file crash/verifier/consistency reducer
20. define a canonical semantic test matrix: every important language rule and artifact guarantee should map to positive, negative, adversarial, and artifact-level regression coverage

### Phase I: Compiler Verification and Preservation Proofs

Expected outcome example: a simple function like `fn add1(x: Int) -> Int { return x + 1; }` is not only user-proved for a property, but also backed by proofs that `Core -> ProofCore` extraction and selected normalization/preservation steps keep its intended pure meaning intact.

1. define precisely what “compiler proof” means for Concrete: distinguish user-code property proofs, ProofCore semantic proofs, pass-preservation proofs, and any future end-to-end compiler correctness claim
2. separate public user-code proof claims from compiler-correctness claims so reports never imply the compiler itself is fully verified when only selected user properties are Lean-backed
3. inventory the compiler-verification trusted base and unproved assumptions: parser, checker, elaborator, CoreCheck, monomorphization, lowering, SSA emission, LLVM/toolchain, runtime, target model, and proof registry attachment
4. prove ProofCore normalization preserves ProofCore semantics for the supported expression fragment before relying on normalized proof targets as equivalent to extracted targets
5. prove `Core -> ProofCore` extraction sound for the supported constructs: if extraction succeeds, the ProofCore expression represents the intended pure Core meaning under the documented assumptions
6. prove selected checker/report/artifact facts agree with compiler state: proof eligibility, predictable status, capabilities, trusted boundaries, fingerprints, obligations, and traceability should not drift from the data that produced them
7. prove small internal compiler invariants before broad pass preservation: no post-mono type variables, well-formed qualified identities, well-formed SSA facts, consistent module/interface artifacts, and stable diagnostic attachment where applicable
8. prove selected pass-preservation properties for a restricted pure subset, starting with transformations that directly affect proof/evidence claims rather than trying to verify the whole compiler at once
9. use the small reference interpreter as an executable semantic oracle for the proof-relevant subset and compare interpreter behavior against ProofCore semantics and compiled behavior where the target model permits
10. decide whether full end-to-end compiler correctness is in scope; if yes, define the restricted source subset, target/backend assumptions, proof architecture, and explicit non-goals before implementation begins

### Phase J: Package System and Dependency Trust

Expected outcome example: a package exporting `pub fn parse_version(...) -> Int` also exports package-level facts about proof status, trusted assumptions, and authority surface so downstream users can review trust widening before adoption.

1. split interface artifacts from body artifacts at package/workspace scale
2. research module-cycle and interface-hygiene enforcement before hardening it at package scale: import-cycle policy, interface/body mismatch handling, invalidation boundaries, and package-facing visibility rules
3. harden package-aware visibility and encapsulation before package management: public/internal/private API boundaries, exported field policy, sealed/internal modules where needed, and diagnostics for accidental API leakage should be explicit before package graphs are trusted
4. design and parse the package manifest
5. add build-script/custom-build-logic support only after the package manifest is stable enough to host it: code generation, C library compilation, resource embedding, and environment detection should be explicit and constrained rather than arbitrary hidden shelling-out
6. add version constraints, dependency resolution, and a lockfile
7. add workspace and multi-package support
8. add package-aware test selection
9. generate C headers from public C-ABI-facing Concrete declarations so library-grade `extern \"C\"` / `repr(C)` surfaces do not require manually maintained `.h` files
10. validate cross-target FFI/ABI from package boundaries
11. add module/package authority budgets after package graphs are real
12. define package/runtime boundary artifacts explicitly: packages should declare what they require from host/runtime/platform surfaces, not only what they export at the source level
13. add package discoverability and package-quality gates so docs, examples, trust summaries, and compatibility surfaces are part of package readiness rather than an afterthought
14. define provenance-aware publishing before public package distribution
15. define package registry server protocol and trust model before a public ecosystem push: upload/download, index/search, yanking/deprecation, checksums/signatures, authentication, and compatibility with provenance/evidence artifacts
16. define package/dependency trust policy explicitly: how dependencies summarize trusted assumptions, how trust widens across package boundaries, how package-level evidence is reviewed, and how trust inheritance is made visible

### Phase K: Editor, Artifact UX, and Compatibility

Expected outcome example: hovering over `fn check_nonce(...) -> Bool` in an editor shows capability status, proof status, predictable status, and theorem/obligation links from the same underlying artifact model.

1. add compiler-as-service / editor / LSP support after diagnostics and facts are structured; expose parser/checker/report/query entrypoints without forcing full executable compilation
2. define the LSP/editor feature scope explicitly: go-to-definition, hover/type info, diagnostics, formatting, rename, code actions, and fact/proof-aware language features
3. treat refactoring support as an explicit product goal, not an incidental side effect of LSP work: rename, move, extract-helper, interface extraction, and dead-code cleanup should preserve or clearly update facts/proofs where possible; examples should include moving a parser helper between modules, renaming a proof-attached validator, and extracting a capability-free core from an effectful shell
4. add fact/proof-aware editor UX: capability/evidence hover, predictable/proof status per function, and jump/link surfaces for obligations, extraction, and traceability
5. add a small human-friendly artifact viewer UX (CLI/TUI/web) for facts, diff, evidence, and proof state once the JSON/schema surfaces stabilize; reports must be readable enough for a security reviewer or systems engineer, not only JSON for tools
6. add dependency auditing for capability, allocation, FFI, trust, evidence, predictability, and proof-obligation drift
7. set a stronger docs/tooling UX bar for external users: generated docs, language-server quality, newcomer navigation, and project-level discoverability must be treated as first-class deliverables rather than polish work
8. add one canonical “how to use Result well” docs-and-examples surface so explicit error handling stays teachable, consistent, and visible in user-facing tooling
9. add release / compatibility discipline when external users depend on the language
10. build a backwards-compatibility regression corpus once public users exist: old accepted programs, old facts/reports, old proof artifacts, deprecated syntax/API examples, and expected migration diagnostics should remain testable across releases
11. define explicit language/versioning/deprecation policy across syntax, stdlib APIs, and proof/fact artifacts so users know what stability guarantees exist and how removals happen
12. add stdlib quality gates for the bounded systems surface: API stability expectations, allocation/capability discipline, proof/predictability friendliness for core modules, and compatibility rules for example-grade helper APIs
13. add a tiny `concrete audit` bundle command after the underlying reports are stable: produce one human-readable and machine-readable package of capabilities, unsafe/trusted boundaries, allocation, stack, proof/evidence, FFI, backend/target assumptions, policy violations, and authority/evidence drift

### Phase L: Runtime Profiles, Allocation, and Predictability

Expected outcome example: a bounded queue or parser helper can carry claims like “no allocation” or “bounded allocation,” explicit overflow policy, and explicit failure-path assumptions rather than hand-wavy runtime promises.

1. decide the analyzable-concurrency / predictable-execution subset before implementing general concurrency; current research anchors are `research/predictable-execution/analyzable-concurrency.md`, `research/predictable-execution/concurrent-stack-analysis.md`, `research/stdlib-runtime/async-concurrency-evidence.md`, `research/stdlib-runtime/channel-model.md`, `research/stdlib-runtime/ffi-cancellation-boundary.md`, `research/language/capability-polymorphism.md`, `research/proof-evidence/concurrency-formal-model.md`, and `research/proof-evidence/concurrency-evidence-example.md`
2. build the concurrency pressure-test suite before implementation: design-only `.con` sketches plus expected reports for optional overlap, required concurrent progress, race/select, bounded channels, scope return values, nested scopes, cancellation edges, FFI blocking edges, and rejected misuse cases such as leaked handles, borrows crossing task boundaries, detached spawn, unbounded channels in bounded profiles, and `Concurrent` use without required authority; use this suite to push the proposed language model before adding compiler support
3. freeze the v1 concurrency surface before implementation: capability names/lattice, lexical scope rules, spawn/join signatures, linear handle rules, bounded channel shape, result-flow rules, ownership-transfer rules, rejected forms, and `--report concurrency` schema
4. implement OS threads + structured scopes + typed bounded channels only after the concurrency stance is documented, pressure-tested, and formalized enough to support evidence claims

Concurrency track, in dependency order:

  1. Build the design-only pressure-test suite first. Include positive examples for optional-overlap file/network work, required-progress producer/consumer, DNS-style race/select, bounded-channel pipelines, scope-returned values, and nested scopes; include negative examples for handle leaks, cross-task borrows, detached spawn, scope escape, unbounded channels in bounded profiles, and shared mutable state in v1.
  2. For every pressure-test example, write the expected `--report concurrency` output before implementation. The report should show capabilities, scope boundaries, spawn sites, joins, channel capacities, handle consumption, backend requirements, and evidence levels.
  3. Pressure-test cancellation and trust boundaries separately: FFI blocking edge, trusted region without checkpoints, cancellable wrapper with explicit interrupt, and cancellation latency reporting.
  4. Settle terminology and the capability lattice: choose `Async`, `Overlap`, or another name for optional overlap; define required concurrent progress as `Concurrent`; defer `Sync`, `Clock`, `Cancellable`, `Tasks(N)`, and `Stack(N)` until their checks/reports exist; enforce `Concurrent` implies optional overlap.
  5. Finish type-system prerequisites: capability polymorphism for higher-order functions and scope APIs, capability-set subset/union checks, and diagnostics explaining why a scope/function requires optional overlap, `Concurrent`, or a later capability.
  6. Specify v1 structured scopes: lexical/block-local only, not stored, not returned, not passed around, and the only safe spawn context.
  7. Specify v1 result and communication flow: parent-child results flow through `join`; handles cannot escape their scope; sibling tasks communicate only through typed bounded channels; nested scopes return values normally to parent scopes; there is no direct scope-to-scope communication outside handles and bounded channels.
  8. Specify v1 ownership transfer: owned linear values may move into tasks; borrowed references may not cross task boundaries; channels move owned values, not borrows; shared mutable state is out of v1.
  9. Specify the first channel model: `Channel<T, N>` or equivalent typed bounded capacity, close behavior, sender/receiver ownership, send/receive blocking behavior, SPSC/MPSC decision, and whether `Channel<T, Unbounded>` exists only as a loud later `Alloc`-requiring form.
  10. Write/mechanize the formal model for the v1 fragment before relying on evidence claims: scope introduction/close, spawn, join, channel send/recv, capability containment, linear handle consumption, and theorems for no leaked tasks and no missing-concurrency-from-sequential-fallback.
  11. Freeze v1 before implementation. The frozen surface must include capability names/lattice, scope syntax, spawn/join signatures, channel signatures, rejected forms, evidence terms, and report schema. Changes after this point require explicit roadmap/design updates.
  12. Implement the v1 checker surface: concurrency capabilities, scope syntax/IR representation, scoped `spawn`, scoped `join`, linear `Handle<T, E>` checking, no handle escape, no cross-task borrows, and no shared mutable state.
  13. Implement the v1 threaded backend: OS threads or a small thread pool behind the scoped model; source semantics stay scope-owned; backend inability to provide `Concurrent` is a build/config error, not silent sequential fallback.
  14. Implement v1 typed bounded channels and integrate them with scope ownership, task argument transfer, and the concurrency checker.
  15. Add the first `--report concurrency`: scope counts, spawn sites, joins, channel capacities, handle consumption, cross-task ownership transfer, backend requirements, and evidence levels (`enforced`, `reported`, `trusted-assumption`, backend assumption).
  16. Define the analyzable-concurrent profile separately from the default model: fixed task set, fixed-capacity channels, bounded waits, no shared mutable state, explicit scheduler/backend assumptions, and no evented backend unless its scheduler is part of the profile.
  17. Add v2 features only after v1 is stable: linear-aware race/select, cooperative cancellation, deadlines, `Clock`, `Cancellable`, and explicit FFI cancellation-boundary reporting. Preemptive cancellation stays out of safe Concrete.
  18. Add resource-bounded concurrency only when the checker/report can explain the bound source: `Tasks(N)`, per-task `Stack(N)`, and integration with heap/allocation budgets.
  19. Add deterministic simulation after the threaded model and reports are stable: virtual clock, seeded scheduler, simulated I/O boundaries, replayable schedule artifacts, CI seed reporting, and `simulated(N)` evidence.
  20. Add evented I/O last, after stack-bound analysis and backend assumptions are strong enough; evented I/O must reuse the same structured scope, capability, channel, handle, and evidence model rather than introducing a second async culture or global executor.

5. explicitly defer inline assembly until the backend contract, target/toolchain model, and trust-boundary story are strong enough to contain it honestly
6. strengthen `--report alloc` so every user-visible allocation is attributed to a source location and call path
7. add structural bounded-allocation reports where the compiler can explain the bound
8. add `BoundedAlloc(N)` only where the bound is structurally explainable
9. evaluate const-generics / comptime only when bounded capacity or artifact generation needs a narrow version of it
10. define a tighter bounded-allocation profile between `NoAlloc` and unrestricted allocation
11. define stack-boundedness reporting and enforcement boundaries
12. separate source-level stack-depth claims from backend/target stack claims
13. define backend and target assumptions for timing, stack, calls, layout, undefined behavior, and proof/evidence boundaries
14. define failure-path boundedness: abort, assertions, impossible branches, OOM-excluded profiles, `defer`, drops, and cleanup paths
15. define arithmetic-overflow policy for predictable/proved profiles versus performance-oriented profiles
16. validate predictable execution with bounded examples: fixed-buffer parser, bounded-state controller, fixed-capacity ring buffer, or equivalent

### Phase M: Public Readiness and User Tooling

Expected outcome example: a new user can install Concrete, run one proof-bearing example, inspect its evidence bundle, and understand what is proved, enforced, reported, or trusted without reading the compiler source.

1. strengthen memory/layout audit reports with source locations, qualified names, repr/packed/align facts, trusted-pointer boundaries, and backend/target caveats
2. add coverage tooling over tests, report facts, policy checks, obligations, proof artifacts, and doc tests
3. add memory-profiler and leak-debug integration for user programs once runtime/allocation profiling exists: heap snapshots or allocation tracing where the target allows it, leak-focused workflows, and a path to correlate runtime findings with `--report alloc`
4. improve onboarding so a newcomer can build one small program without project-author help
5. implement the first real `concrete new` bootstrap path with at least the documented predictable, library, and FFI starter templates; design-only docs are not enough for outsider onboarding
6. implement or explicitly reject the standalone `--stdlib` bridge for single-file stdlib access; if the bridge does not land, remove the proposal from user-facing docs and make project-only stdlib access the unambiguous supported path
7. add a clean-room outsider workflow CI job: install the compiler, create a temp project, build it, run it, check it, and inspect one artifact without relying on repo-local paths or maintainer-only setup
8. audit design-only user-facing docs and workflows against the actual shipped CLI/runtime surface and either implement or explicitly relabel each promised surface before public-facing material pretends it is real
9. perform one roadmap/docs/changelog reference migration to the current numbering and phase structure before the new consistency gates start enforcing future drift prevention
10. define the stability / experimental boundary for public users
11. define the language evolution policy on top of that boundary: edition/versioning rules, deprecation windows, breaking-change policy, and how experimental features graduate into the supported subset
12. define public governance and decision process for language evolution: how syntax changes, profile changes, stdlib stabilization, breaking changes, and security-relevant decisions are proposed, reviewed, accepted, and documented
13. add roadmap/docs reference-consistency checks so phase/task references in user-facing docs cannot silently rot when the roadmap is renumbered, regrouped, or partially completed

### Phase N: Long-Horizon Research Backlog

Expected outcome example: future ideas such as typestate, arena proofs, richer timing models, or Miri-style semantic checking stay clearly gated until Concrete already has a stable artifact/proof/evidence foundation.

1. expand formalization only after obligations, extraction reports, proof diagnostics, attached specs, the explicit ProofCore boundary, and the broader memory/effect model are artifact-backed
2. research typestate only if a current state-machine/protocol example needs it
3. research arena allocation after bounded-capacity and allocation-profile work exposes a concrete gap
4. research target-specific timing models after source-level predictability and backend boundaries are explicit
5. research exact WCET / runtime models only with a target/hardware model
6. research exact stack-size claims across optimized machine code only with deeper backend/target integration
7. research cache / pipeline behavior as target-level analysis, not a source-language promise
8. research binary-format DSLs only if the packet/ELF examples show repeated parser boilerplate
9. research hardware capability mapping after source-level capabilities and package policies are stable
10. research capability sandbox profiles after authority reports and package policies are useful
11. broaden the small reference interpreter toward fuller Miri-style UB checking only if the first proof-subset interpreter proves valuable and the memory/UB model can support the added operational complexity
12. research persistent equality / rewrite state across phases only after the backend contract, semantic diff workflow, and proof/evidence pipeline are stronger; use [persistent-equality-and-rewrite-state](research/compiler/persistent-equality-and-rewrite-state.md) as the starting point
## Reference Map

The thesis references are [core-thesis](research/thesis-validation/core-thesis.md), [objective-matrix](research/thesis-validation/objective-matrix.md), [thesis-validation](research/thesis-validation/thesis-validation.md), [validation-examples](research/thesis-validation/validation-examples.md), [predictable-execution](research/predictable-execution/predictable-execution.md), [effect-taxonomy](research/predictable-execution/effect-taxonomy.md), [diagnostic-ux](research/compiler/diagnostic-ux.md), and [backend-traceability](research/compiler/backend-traceability.md).

The proof/evidence references are [concrete-to-lean-pipeline](research/proof-evidence/concrete-to-lean-pipeline.md), [proving-concrete-functions-in-lean](research/proof-evidence/proving-concrete-functions-in-lean.md), [spec-attachment](research/proof-evidence/spec-attachment.md), [effectful-proofs](research/proof-evidence/effectful-proofs.md), [provable-systems-subset](research/proof-evidence/provable-systems-subset.md), [proof-addon-architecture](research/proof-evidence/proof-addon-architecture.md), [proof-ux-and-verification-influences](research/proof-evidence/proof-ux-and-verification-influences.md), [proof-ux-and-authoring-loop](research/proof-evidence/proof-ux-and-authoring-loop.md), [verification-product-model](research/proof-evidence/verification-product-model.md), [vericoding-and-evidence-product](research/proof-evidence/vericoding-and-evidence-product.md), [evidence-review-workflows](research/proof-evidence/evidence-review-workflows.md), [proof-evidence-artifacts](research/proof-evidence/proof-evidence-artifacts.md), [concurrency-evidence-example](research/proof-evidence/concurrency-evidence-example.md), and [concurrency-formal-model](research/proof-evidence/concurrency-formal-model.md).

The language/runtime references are [checked-indexing-and-slice-views](research/language/checked-indexing-and-slice-views.md), [arithmetic-overflow-policy](research/language/arithmetic-overflow-policy.md), [opaque-validated-types](research/language/opaque-validated-types.md), [layout-contract-surface](research/language/layout-contract-surface.md), [failure-semantics](research/language/failure-semantics.md), [high-integrity-profile](research/language/high-integrity-profile.md), [memory-ub-boundary](research/language/memory-ub-boundary.md), [trusted-code-policy](research/language/trusted-code-policy.md), [contracts-and-invariants-gating](research/language/contracts-and-invariants-gating.md), [interrupt-signal-model](research/language/interrupt-signal-model.md), [capability-polymorphism](research/language/capability-polymorphism.md), [allocation-budgets](research/stdlib-runtime/allocation-budgets.md), [arena-allocation](research/stdlib-runtime/arena-allocation.md), [execution-cost](research/stdlib-runtime/execution-cost.md), [long-term-concurrency](research/stdlib-runtime/long-term-concurrency.md), [async-concurrency-evidence](research/stdlib-runtime/async-concurrency-evidence.md), [channel-model](research/stdlib-runtime/channel-model.md), [ffi-cancellation-boundary](research/stdlib-runtime/ffi-cancellation-boundary.md), and [concurrent-stack-analysis](research/predictable-execution/concurrent-stack-analysis.md).

The compiler/package references are [semantic-diff-and-trust-drift](research/compiler/semantic-diff-and-trust-drift.md), [miri-style-interpreter](research/compiler/miri-style-interpreter.md), [persistent-equality-and-rewrite-state](research/compiler/persistent-equality-and-rewrite-state.md), [package-model](research/packages-tooling/package-model.md), and [proof-aware-package-boundaries](research/packages-tooling/proof-aware-package-boundaries.md).

The tooling/package/backend/showcase references are [artifact-driven-compiler](research/compiler/artifact-driven-compiler.md), [semantic-query-interface](research/compiler/semantic-query-interface.md), [performance-research-packets](research/compiler/performance-research-packets.md), [developer-tooling](research/packages-tooling/developer-tooling.md), [package-model](research/packages-tooling/package-model.md), [package-manager-design](research/packages-tooling/package-manager-design.md), [qbe-backend](research/compiler/qbe-backend.md), [qbe-in-concrete](research/compiler/qbe-in-concrete.md), [showcase-workloads](research/workloads/showcase-workloads.md), [adoption-strategy](research/workloads/adoption-strategy.md), and [phase-h-findings](research/workloads/phase-h-findings.md).

---

## Design Constraints

- keep the parser LL(1)
- keep SSA as the only backend boundary
- prefer stable storage for mutable aggregate loop state over phi transport
- avoid parallel semantic lowering paths
- keep builtins minimal and implementation-shaped; keep stdlib clean and user-facing
- keep trust, capability, and foreign boundaries explicit and auditable
- make serious errors and report failures explain themselves: a user should know the violated rule, the source location, the reason it matters, and one plausible next action

## Current Risks

- mutable aggregate lowering can still be too backend-sensitive if promoted storage is incomplete
- formalization scope is still narrow
- type-coercion completeness is not proved, only hardened
- the linearity checker is tested heavily but not formally audited

## Longer-Horizon Multipliers

- proof-backed trust claims
- stronger audit outputs
- a smaller trusted computing base
- a better capability/sandboxing story

**References:** [ten-x-improvements](research/meta/ten-x-improvements.md), [capability-sandboxing](research/language/capability-sandboxing.md), [trust-multipliers](research/proof-evidence/trust-multipliers.md), [ai-assisted-optimization](research/meta/ai-assisted-optimization.md)
