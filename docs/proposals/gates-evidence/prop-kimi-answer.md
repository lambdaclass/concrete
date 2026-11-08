  ITEM C1: AGREE-WITH-CHANGES  
  why: Wall-clock minima remain noisy even with interleaving; use per-stage structural work counters as the primary PR signal, with `perf stat` instruction counts on concrete-ci-2 as corroboration. Lean heartbeats measure elaboration, not arbitrary compiled-stage work, so they are suitable only for proof/elaboration budgets.  
  Keep these measurement counters distinct from the language’s normative step-cost model; wall time remains a safety bound and diagnostic.  
  where it belongs: C/gate 13; backend stages now, frontend stages when they land in slice 1.  
  size: LARGE  
  exists already?: C currently times stages with `IO.monoNanosNow` (`branches/C-gate-scaling-determinism.diff:368-397`); whole-replay seconds exist at `scripts/proofs/kernel_replay.sh:84-94`. No deterministic counter framework.

  ITEM C2: AGREE  
  why: One family per dimension identifies the stage and input shape responsible for growth much better than a single “bigger program” family. Each family must remain well formed, non-dead, deterministic, and have a checked expected result and output digest.  
  Scale exactly one dimension at a time and derive the family inventory from the supported fragment rather than duplicating a hand-written construct list.  
  where it belongs: C/gate 13; BIR blocks, locals, expression size, and call-graph-independent shapes now; source/Core nesting in slice 1; functions, structs, and enums with their respective slices.  
  size: LARGE  
  exists already?: Only one mixed BIR family exists (`branches/C-gate-scaling-determinism.diff:330-350`).

  ITEM C3: AGREE-WITH-CHANGES  
  why: Declared complexity should live in the future `Concrete/Stages.lean` registry, not in a second stage inventory consumed only by C. Fit at least four sizes with range/resolution checks and a planted quadratic fit; an undeclared or unmeasured stage fails closed.  
  A margin applies to measurement/fit uncertainty, not as permission to hide a clear quadratic trend.  
  where it belongs: C/gate 13 plus the stage registry; backend stages now, all registered stages in slice 1.  
  size: LARGE  
  exists already?: C has only three sizes and one global exponent threshold (`branches/C-gate-scaling-determinism.diff:831-860`). The planned stage registry is described at `Concrete/README.md:468-502`.

  ITEM C4: AGREE  
  why: Exact emitted-byte counts are deterministic and can be checked cheaply on PRs. Peak RSS belongs in the scheduled scaling report, measured through the common bounded runner so every stage uses the same accounting and a missing measurement is not a pass.  
  Output size includes text, objects, executables, and intermediate representations, not only final binaries.  
  where it belongs: C/gate 13, with gate 11’s existing per-example reports retained; now.  
  size: LARGE  
  exists already?: Gate 11 records executable bytes/text and wall time but no scaling or RSS history (`scripts/checks/compiler/slice0_artifacts.sh:71-78`, `:153-170`); `bounded_exec.py` does not collect RSS.

  ITEM C5: AGREE-WITH-CHANGES  
  why: Keep exact-SHA history on concrete-ci-2 and enforce absolute budgets for deterministic counters and output sizes. Wall-time budgets should initially remain report-only until a stable baseline exists, consistent with the owner’s performance-noise rule (`docs/decisions.md:961-969`).  
  Use reference workloads and trend alarms for unchanged-slope regressions; weekly review handles sustained wall-time drift.  
  where it belongs: C/gate 13 and the scheduled results store; now.  
  size: LARGE  
  exists already?: Gate 11 records per-example time/size but not historical comparison (`scripts/checks/compiler/slice0_artifacts.sh:71-78`). Exact-SHA performance history is planned by `ROADMAP.md:475`.

  ITEM C6: AGREE-WITH-CHANGES  
  why: The quadratic plant exists, but it is not run automatically: `scaling.sh` only executes a plant when explicitly passed flags, while CI invokes it without them (`branches/C-gate-scaling-determinism.diff:518-581`, `:79-80`).  
  Every normal scaling run must execute linear-pass and quadratic-fail controls, requiring the named planted stage and approximately quadratic exponent; a setup failure is not a successful control.  
  where it belongs: C/gate 13 fix round; now.  
  size: SMALL  
  exists already?: Plant implementations are at `branches/C-gate-scaling-determinism.diff:356-362`; only manual invocation exists.

  ITEM D1: AGREE  
  why: C varies paths, CWD, HOME, TMPDIR, and assignment order but fixes locale and timezone (`branches/C-gate-scaling-determinism.diff:213-227`). Add the full perturbation matrix, including input ordering where semantically irrelevant, available locales, umask, hash seeds, and repeated ASLR-enabled runs.  
  Every comparison remains byte-exact, and a perturbation that cannot be applied is reported rather than skipped silently.  
  where it belongs: C/gate 14 with gate 8’s independent rebuild; current backend artifacts now, all frontend stages in slice 1.  
  size: LARGE  
  exists already?: Partial coverage exists in C/gate 14 and gate 8 (`scripts/checks/compiler/slice0_repro.sh:53-83`).

  ITEM D2: AGREE-WITH-CHANGES  
  why: Gate 8 already compares every currently emitted backend artifact (`scripts/checks/lib/repro_compare.py:14-15`). Extend the same ordered comparator to canonical serializations of every `Output` field and name the first differing stage in pipeline order.  
  B0’s U5/U11 printers and Core/MIR/Memory artifacts do not exist in this export, so they must be added with those stages rather than simulated now.  
  where it belongs: Gate 8 and C/gate 14, driven by the stage registry; backend stages now, frontend stages in slice 1.  
  size: LARGE  
  exists already?: All current backend stages are compared by `scripts/checks/compiler/slice0_repro.sh:81-101`; no frontend intermediate printers exist.

  ITEM D3: AGREE-WITH-CHANGES  
  why: The current matrix uses `ubuntu-24.04` for both Linux reproducibility legs, so it does not implement the decided GitHub-versus-concrete-ci-2 comparison (`.github/workflows/lean_action_ci.yml:458-465`).  
  Use two GitHub runners for untrusted PRs, but require GitHub plus concrete-ci-2 for landing dispatches, with runner identity in each manifest. This preserves the rule that self-hosted runners never execute PR code.  
  where it belongs: Gate 8 / ISS-0029; now.  
  size: SMALL  
  exists already?: Cross-runner comparison exists at `.github/workflows/lean_action_ci.yml:480-502`, but not between the required hosts.

  ITEM A1: AGREE-WITH-CHANGES  
  why: Generate well-scoped programs directly from the spec’s typed constructors, and add deterministic shrinking that preserves the same mismatch. Persist every minimized finding as a manifest regression with its original seed and shrink path.  
  Generators belong in test tooling, not the production `Concrete` import closure.  
  where it belongs: A/gates 5–6; BIR generation/shrinking can start now, source/Core generation requires slice-1 `compileCore`.  
  size: LARGE  
  exists already?: A has seeded weighted generation but no shrinking (`branches/A-gate-spec-diff.diff:267-349`); its Core hook is unwired (`:432-438`).

  ITEM A2: AGREE-WITH-CHANGES  
  why: Coverage must be derived from the Lean rule registry, with each rule declaring its required accepted, refused, trapping, and/or evaluation examples. Merely counting constructor names or generator productions would not establish rule coverage.  
  Fail on an unregistered rule, missing obligation, or fixture referencing an unknown/retired rule incorrectly.  
  where it belongs: A/gate 5 and B’s manifest; partial BIR construct coverage now, real rule coverage after R-0603/R-0500 and slice-1 frontend support.  
  size: LARGE  
  exists already?: A checks generator productions only (`branches/A-gate-spec-diff.diff:820-839`); rule-indexed conformance is planned at `ROADMAP.md:432`.

  ITEM A3: AGREE  
  why: Compare acceptance and refusal in both directions, including crashes, timeouts, and unexpected success as mismatches. Compare canonical rule-ID sets/revisions, not diagnostic text.  
  This must use the spec’s typed refusal result rather than inferring a reason from `check = false`.  
  where it belongs: A/gate 5 and B’s `con:` runner; slice 1, after R-0592, R-0603, R-0593, and `compileCore`.  
  size: LARGE  
  exists already?: B parses `refuse RULE-ID` (`branches/B-gates-manifest.diff:763-778`), but A filters for accepted programs only (`branches/A-gate-spec-diff.diff:655-661`).

  ITEM A4: AGREE  
  why: Use targeted semantic mutations per stage and require the expected comparison leg to kill each mutant for the stated reason. Record survivors with their seed/mutant and add minimized regressions; do not port the frozen legacy harness.  
  This is precisely the sort of expensive campaign that belongs weekly on concrete-ci-2.  
  where it belongs: A/gates 5–6 under R-0498; BIR/backend mutations now, frontend mutations in slice 1.  
  size: LARGE  
  exists already?: Only individual planted controls exist in A/C/D; the new mutation harness is planned at `ROADMAP.md:473`.

  ITEM A5: AGREE  
  why: Record that differential testing is transitional evidence, not the final assurance argument. As each stage gains a theorem, the meta-gate should require its registered proof/premise evidence while retaining differentials for native trust boundaries and regression testing.  
  This is a roadmap and evidence-model clarification, not a new runtime gate.  
  where it belongs: `ROADMAP.md` R-0508/R-0583 and the gate manifest; documentation now, proof-driven transitions per stage.  
  size: SMALL  
  exists already?: The proof-first direction exists at `ROADMAP.md:98-113` and `Concrete/README.md:496-502`; the explicit gate-transition note is absent.

  ITEM T1: AGREE-WITH-CHANGES  
  why: Do not pretend the entire list is derivable: trust rationale, semantic premises, and retirement paths are intentional Lean registry data. Derive the mechanical closure—axioms, trusted declarations, resolved executable identities/hashes, toolchain pins—and require exact agreement in both directions with the committed registry/golden.  
  Growth or strengthening needs an owner-approved decision record with quoted provenance; basename-only `strace` matching is insufficient identity.  
  where it belongs: D/gate 9; now, with richer source premises in slice 1.  
  size: LARGE  
  exists already?: D derives environment axioms/escape hatches (`branches/D-trusted-base.diff:232-291`) and exec names (`:459-510`), but retains a hand-written component list and no executable-content hashes.

  ITEM G1: AGREE-WITH-CHANGES  
  why: Introduce one gate manifest containing property, script, tier, budget, runner, planted control, expected failure signature, and required-check names. Validate it against CI and `required-checks.txt`; do not create a third unsynchronized job inventory.  
  PR-tier controls run on PRs; scheduled controls need a current exact-SHA record, since missing or cancelled evidence is never a pass.  
  where it belongs: New root/meta check, with B’s test manifest remaining a separate test-data manifest; now.  
  size: LARGE  
  exists already?: `required-checks.txt:1-23` lists only job names; `scripts/checks/run.sh:5-7` prevents forgotten root scripts but declares no properties or controls.

  ITEM G2: AGREE  
  why: A differing repeated compilation or observation must remain a failure, and no later attempt may convert it to a pass. The existing retry is specifically for timeouts and judges a completed retry normally, so this proposal should not ban that separate INCOMPLETE-recovery mechanism.  
  Encode the no-retry-on-disagreement rule in shared differential/determinism helpers with a planted flip-then-clean sequence.  
  where it belongs: Shared comparator used by gates 5, 8, and 14; now.  
  size: SMALL  
  exists already?: Timeout-only retry semantics are documented at `scripts/checks/lib/bounded.sh:6-10`; no disagreement retry currently exists.

  ITEM G3: AGREE  
  why: Add declared budgets and exact-SHA duration records for every gate, enforced by an external bounded wrapper. The meta-gate fails a PR-tier gate that exceeds its budget as INCOMPLETE/failed, never silently passes it.  
  Use daily history and weekly trend review to keep the landing tier affordable.  
  where it belongs: G1’s manifest and the shared gate runner; now.  
  size: SMALL  
  exists already?: Root checks print individual durations (`scripts/checks/run.sh:21-23`), and CI has coarse job timeouts, but there is no gate-level budget/history contract.

  ## Group 2 — candidate new gates

  ITEM N1: AGREE-WITH-CHANGES  
  why: Requiring a refusal fixture for every spec rule would be meaningless for evaluation, representation, and acceptance-only rules. Instead, every emitted refusal must name a live spec rule/revision, and every rule classified as refusal-bearing needs at least one refusal fixture; orphan IDs fail both directions.  
  Keep semantic-example obligations in A2 rather than manufacturing irrelevant refusals.  
  where it belongs: B’s manifest plus a root rule-registry check; slice 1 after R-0603/R-0593.  
  size: LARGE  
  exists already?: Rule-ID/revision syntax is decided at `docs/decisions.md:821-827`; B accepts refusal rows but has no registry bidirectionality.

  ITEM N2: AGREE-WITH-CHANGES  
  why: A true spec law must not be “refuted” in the same consistent spec. Give each law a satisfiable non-trivial witness and a premise-violating negative witness or deliberately strengthened/mutated proposition that the kernel/checker rejects for the intended reason.  
  Reuse the existing proof-build machinery rather than adding another independent spec build.  
  where it belongs: `scripts/proofs/`, linked to the Lean law registry; initial laws now where enumerable, systematic coverage with A2i/slice 1.  
  size: LARGE  
  exists already?: Examples are axiom-audited at `scripts/proofs/trust_ban.sh:134-157`; general non-vacuity witnesses are designed at `Concrete/README.md:411-432`, but per-law planted counterexamples are absent.

  ITEM N3: AGREE-WITH-CHANGES  
  why: Scope the forward direction to landed rules: every Lean rule cites its decision row, and every decided language row either links its Lean definition/fixtures or names its future slice. Process/project rows and not-yet-landed language rows must not be forced into fake fixtures.  
  Derive the mapping from the Lean registry and check decision anchors, revisions, and retired IDs mechanically.  
  where it belongs: Root traceability check; A2i/R-0500/R-0603, with initial landed modules as soon as the registry exists.  
  size: LARGE  
  exists already?: Free-form citations exist at `ConcreteSpec/Spec.lean:39-40`; no bidirectional semantic traceability gate exists.

  ITEM N4: AGREE-WITH-CHANGES  
  why: Fuzz lexer/parser termination, crash freedom, bounded resource use, and typed refusal IDs. For round-trip testing, use a canonical test serializer derived from the AST/grammar; do not prematurely decide the user-facing formatter, which is scheduled separately.  
  Timeouts are INCOMPLETE/failures, never evidence of termination; minimized cases become manifest fixtures.  
  where it belongs: New frontend fuzz gate sharing A’s generator/shrinker; slice 1 for lexer/parser and refusals, canonical printer when available.  
  size: LARGE  
  exists already?: Only generic LL(1) machinery and the planned independent judgments exist (`Concrete/README.md:206-220`); there is no frontend fuzzer.

  ITEM N5: AGREE  
  why: B already runs every `bir:` manifest row through LLVM-native and LCB-native and compares full observations against the manifest expectation. The emitter also checks BIR evaluation and LCB’s evaluator, so this needs no second backend-agreement gate.  
  Future `con:` rows must use the same runner and compare status, stdout, stderr, trap/startup classification, and later event traces.  
  where it belongs: B/`scripts/checks/compiler/tests.sh`, gate 4; now.  
  size: SMALL  
  exists already?: Yes: `branches/B-gates-manifest.diff:507-515`, `:531-546`, and `:581-590`.

  ITEM N6: AGREE-WITH-CHANGES  
  why: BIR already carries kind, typed site, span, and location (`Concrete/BIR.lean:76-84`), but artifact-level trap-code presence is explicitly unestablished (`Concrete/README.md:526-531`).  
  Map emitted trap paths back to source obligations and trap names, distinguish explicit `abort()` from implicit traps, and fail on an unaccounted artifact trap without claiming that unreachable runtime trap code is necessarily absent.  
  where it belongs: New artifact/facts gate, using N7’s binary inventory; BIR accounting now, source obligations in slice 1.  
  size: LARGE  
  exists already?: Runtime naming exists at `Concrete/Backend/Runtime.lean:28-41`; structural trap inspection is only a follow-up (`Concrete/README.md:635-639`).

  ITEM N7: AGREE  
  why: Audit final objects/executables on ELF and Mach-O: symbols, sections, undefined imports, and runtime/libc dependencies against a target-specific allowlist derived from the trusted registry. Unexpected imports fail, including those introduced by the linker rather than the printer.  
  Reuse artifacts already built by gates 4/10/12 rather than linking again.  
  where it belongs: Gate 12/artifact checks, coordinated with D/gate 9; now.  
  size: LARGE  
  exists already?: Gate 12 disassembles and records external calls (`scripts/checks/compiler/slice0_artifacts.sh:117-150`), but does not enforce a symbol/section/import allowlist.

  ITEM N8: AGREE  
  why: Validate Lean, LLVM, Nix, and Lake pins on every PR and hash the actual resolved executables, not merely version strings. Daily perform a clean rebuild/hash check; weekly perform the from-scratch Nix rebuild and supply-chain audit.  
  Keep the legacy oracle’s independent weekly rebuild; this proposal covers the new compiler’s toolchain.  
  where it belongs: Gate 9/build identity plus scheduled supply-chain workflows; now.  
  size: LARGE  
  exists already?: Pins and dependency cleanliness are checked at `scripts/checks/compiler/build_identity.sh:27-45`; only the legacy oracle has a weekly fresh rebuild (`.github/workflows/oracle.yml:1-8`).

  ITEM N9: AGREE-WITH-CHANGES  
  why: Check explicitly marked Concrete examples with declared accepted/refused outcomes, not every Lean, shell, or illustrative code block. Spec examples remain in Lean and kernel-checked; documentation examples should reference the same manifest/run machinery rather than duplicate semantics.  
  There is currently no `docs/book`, so the gate starts with marked current documentation and expands when the book exists.  
  where it belongs: B’s manifest plus a documentation-example runner; slice 1 when source compilation exists.  
  size: LARGE  
  exists already?: Spec examples exist at `ConcreteSpec/Spec/Slice0.lean:77-132`; no docs/book or documentation-example runner exists.

  ITEM N10: AGREE-WITH-CHANGES  
  why: Prefer deterministic elaboration heartbeat/work limits and a hard overall proof-job cap over flaky per-theorem wall-time failures on PRs. Record per-theorem/module elaboration and kernel-replay costs by exact SHA, with daily history and weekly trend review.  
  This needs profiler/toolchain support; the existing replay only measures whole legs.  
  where it belongs: `scripts/proofs/` and the performance history; now.  
  size: LARGE  
  exists already?: Whole replay timings exist at `scripts/proofs/kernel_replay.sh:84-94`; CI job caps are 30/90 minutes (`.github/workflows/lean_action_ci.yml:170-175`, `:280-285`). No per-theorem history.

  ITEM N11: AGREE  
  why: A currently imports `Concrete.SpecDiffRandom`, `SpecDiffSpecGen`, and `SpecDiffBirGen` from the production library root (`branches/A-gate-spec-diff.diff:47-57`). Move them to test tooling and enforce directional imports: `ConcreteSpec` imports no compiler, `Concrete` imports no test generators, and test drivers may import either.  
  Plant a production-root test import and require the root check to reject it.  
  where it belongs: A fix plus a root import-layering check; now, before A lands.  
  size: SMALL  
  exists already?: Main’s root is clean (`Concrete.lean:1-8`), but no import-layering gate exists; A’s first draft violates it.

  ITEM N12: AGREE-WITH-CHANGES  
  why: Derive the stage inventory from `Concrete/Stages.lean` and require each stage to map to at least one gate property and planted control in G1’s manifest. Do not maintain another handwritten list that can drift when a pass is added.  
  Proof/evidence registration and behavioural gate coverage are separate rows: a theorem alone does not claim a runtime differential, and a test alone does not claim proof coverage.  
  where it belongs: G1 plus the stage registry; backend stages now, full pipeline in slice 1.  
  size: LARGE  
  exists already?: Stage-call/evidence enumeration is designed at `Concrete/README.md:496-502`; broader generated coverage is planned at `ROADMAP.md:471`.

  ## Additional recommendations

  ITEM X1: AGREE  
  why: Add incremental-versus-clean build equivalence with cache-identity and stale-artifact controls. ISS-0016 demonstrated that restored build directories can contain orphaned modules; final artifacts should be byte-identical and a planted stale dependency must invalidate the cache.  
  This complements, rather than duplicates, gate 8’s independent-source reproducibility.  
  where it belongs: Scheduled build/cache gate under R-0546/R-0149; now for Lean artifacts, native outputs as available.  
  size: LARGE  
  exists already?: Orphan pruning exists from ISS-0016 (`scripts/proofs/lib.sh:36-51`); no general incremental/clean equivalence gate exists.

  ITEM X2: AGREE  
  why: Mechanize the owner’s regression rule: every daily/weekly defect must produce a minimized, replayable case and an `ISSUES.tsv` link, then a PR manifest fixture before the issue closes.  
  A scheduled failure lacking its regression promotion remains visibly open rather than disappearing when the original campaign seed changes.  
  where it belongs: Shared campaign result tooling and B’s manifest; now for BIR, slice 1 for source cases.  
  size: LARGE  
  exists already?: A prints regression instructions (`branches/A-gate-spec-diff.diff:842-859`), but there is no issue-to-fixture closure check.

  ITEM X3: AGREE  
  why: Add curated hostile resource fixtures—deep nesting, huge literals, excessive locals/blocks, oversized artifacts—and require bounded termination with a typed refusal rather than OOM, crash, or stack overflow.  
  These are deterministic PR fixtures; generated resource attacks and larger limits run daily under the common process/memory bounds.  
  where it belongs: R-0528 resource-limit gate, sharing frontend/BIR generators; BIR now, parser/source in slice 1.  
  size: LARGE  
  exists already?: Resource limits are planned at `ROADMAP.md:456`; bounded execution helpers exist, but no hostile fixture inventory.

  ITEM X4: AGREE  
  why: Add a native runtime fault matrix for short writes, write errors/EINTR, closed fd 2, invalid result words, exit 125/134 versus genuine refusal/trap, and unexpected signals.  
  This tests the actual observation boundary and prevents status-only misclassification while keeping environment assumptions explicit in `P_launch`/`P_rt`.  
  where it belongs: B/gate 4 and the runtime controls; Linux now, macOS matrix where supported.  
  size: LARGE  
  exists already?: B has forged status/newline/stdout controls (`branches/B-gates-manifest.diff:597-614`); the runtime loops on partial writes but ignores errors (`Concrete/Backend/Runtime.lean:107-125`), and no full fault matrix exists.

  ## TIERS

  ### Per-item placement and estimated added PR cost

  Costs are incremental seconds on the shared x86-64 PR path, assuming the compiler and native artifacts are built once. “0” means reuse of an existing required job’s output.

  | Item | Recommended tier | PR seconds |
  |---|---|---:|
  | C1 | split: small structural counters PR; full counters/`perf stat` daily | 20 |
  | C2 | split: small dimension smoke PR; full families daily, largest sizes weekly | 15 |
  | C3 | split: declaration/schema/control PR; ≥4-size fitting daily | 3 |
  | C4 | split: exact output-size checks PR; RSS and scaling daily | 2 |
  | C5 | daily absolute budgets/history; weekly trend review | 0 |
  | C6 | split: automatic synthetic controls PR; full scaling plants daily | 5 |
  | D1 | split: small env/CWD double emission PR; full perturbation matrix daily | 30 |
  | D2 | split: second emission/all current printers PR; full native-stage comparison daily | 45 |
  | D3 | PR on two hosted runners; GitHub+ci-2 required for landing dispatch | 5 |
  | A1 | split: small seeded typed sample PR after slice 1; bounded daily and overnight weekly | 30 |
  | A2 | PR over the rule registry and manifest | 5 |
  | A3 | PR over all refusal fixtures after slice 1 | 15 |
  | A4 | weekly mutation campaign | 0 |
  | A5 | PR documentation/meta validation | 1 |
  | T1 | split: derived registry equality and representative tool hashes PR; full trace daily | 45 |
  | G1 | PR manifest/controls validation; scheduled-control freshness checked in its tier | 10 |
  | G2 | PR comparator policy and flip-then-clean control | 1 |
  | G3 | PR timing enforcement; daily history and weekly trends | 2 |
  | N1 | PR after the typed rule registry | 5 |
  | N2 | split: changed-law controls PR; complete law-mutation sweep daily | 45 |
  | N3 | PR traceability | 5 |
  | N4 | split: curated parser cases PR; 1-hour daily fuzz, overnight weekly | 10 |
  | N5 | PR as part of B’s existing Tests job | 0 |
  | N6 | PR using already-emitted IR/artifacts | 15 |
  | N7 | PR using existing native artifacts | 10 |
  | N8 | split: pin validation PR; clean rebuild daily; from-scratch/supply-chain weekly | 2 |
  | N9 | PR after source compilation exists | 20 |
  | N10 | split: existing-build hard cap PR; daily history, weekly trend review | 0 |
  | N11 | PR import-layering check | 2 |
  | N12 | PR stage/gate-manifest coverage | 3 |
  | X1 | daily incremental/clean build equivalence; weekly cold-cache variant | 0 |
  | X2 | split: issue/fixture schema PR; shrinking and regression generation daily | 2 |
  | X3 | split: curated hostile fixtures PR; generated resource campaign daily | 15 |
  | X4 | PR runtime fault controls; daily cross-host/runtime matrix | 20 |

  The total is about **388 seconds (6.5 minutes)** on the shared critical path, leaving roughly 3.5 minutes for orchestration and variation. Costs for slice-1 items do not apply until their dependencies exist.

  ### Is the 10-minute PR budget realistic?

  **Not if each item becomes an independent required job.** Existing required jobs already include a 90-minute external re-check and 60-minute compiler/trusted-base/reproducibility jobs (`.github/workflows/lean_action_ci.yml:280-285`, `:356-478`), while `required-checks.txt:6-23` makes all 18 current jobs mandatory. A’s differential alone defaults to 600 seconds per architecture and C’s scaling command to 600 seconds.

  It is realistic only with consolidation:

  - Build the compiler once and share one manifest emission/native-artifact set across B, A fixture legs, determinism, trusted-base, trap, and binary audits.
  - Reuse the existing proof builds for N2/N10 rather than elaborating `ConcreteSpec` again.
  - Move C’s full 1×/10×/100× wall-time scaling, full native double builds, full manifest `strace`, random A1 campaigns, and parser fuzzing to daily.
  - On PRs, run only a second emission for D2, deterministic small counters for C1/C3, representative trusted-tool tracing for T1, and cheap planted controls.
  - Preserve gate 8’s independent rebuild as the heavyweight reproducibility leg; do not duplicate it inside C/gate 14.
  - Count shared setup separately in the report, but enforce the sum of added per-gate budgets at 600 seconds on the critical path.

  The existing landing target remains ≤40 minutes (`ROADMAP.md:595-603`); the new 10-minute allowance should be treated as added critical-path work, not as permission to add ten minutes to every matrix leg.

  ### Scheduling and exact-SHA records

  Use **GitHub Actions `schedule` workflows routed to concrete-ci-2**, not a new systemd scheduler. This follows `.github/workflows/oracle.yml:13-25`, keeps execution under the checked workflow YAML, and uses the existing rule that scheduled/self-hosted jobs never run PR code (`docs/SELF_HOSTED_RUNNERS.md:125-129`).

  Recommended structure:

  - `daily.yml` and `weekly.yml`, plus `workflow_dispatch`, with `concurrency.cancel-in-progress: false`.
  - Each run resolves main once, prints that full SHA everywhere, checks out exactly it, and never substitutes the SHA of a later main commit.
  - Full logs and TSV histories upload as artifacts named with tier, SHA, and run ID.
  - A small exact-SHA summary is published through the existing `refs/notes/concrete-gates` mechanism (`scripts/land/notes.sh:5-19`): run ID, tier, verdict, timestamp, controls, and principal counter summary. Large time series stay in artifacts.
  - Record steps use `if: always()` and record `passed`, `failed`, or `incomplete`; cancelled jobs cannot write a success. Missing notes are not interpreted as passes.

  Add a separate lightweight scheduled **schedule-audit** on a GitHub-hosted runner. For every expected daily/weekly window it queries the workflow run and exact-SHA note; absent, skipped, cancelled, stale, or artifact-less evidence becomes `missing`, and the audit fails with the expected window and last known run. The next PR meta-gate should also *report* stale scheduled evidence without blocking that unrelated landing.

  A daily/weekly failure or missing result becomes a stop-the-line issue against the exact tested main SHA. The reproducer and closure condition are filed in `ISSUES.tsv` through the normal train—never by an automated push to main—and every such defect gains a PR regression fixture. This matches the issue schema enforced by `scripts/checks/issues.sh:19-40`.

  ### Build order

  Each numbered step is a separate small branch with its own planted control:

  1. **Land B/N5 first** (already accepted): manifest inventory, backend agreement, wrong-expectation and inventory controls.
  2. **N11 + A layering fix:** move generators out of `Concrete.lean`; production-root test-import control. This blocks A’s landing.
  3. **G1 gate manifest:** missing property/tier/control and workflow/required-check drift controls.
  4. **G3 gate budgets:** over-budget gate is INCOMPLETE/fails; exact-SHA duration record control.
  5. **C6:** invoke linear/quadratic controls automatically; assert exact planted stage/reason.
  6. **D3/ISS-0029:** real GitHub/ci-2 landing matrix and runner-identity manifests; PR remains hosted-only.
  7. **G2, then D2:** disagreement can never retry-pass; planted first-stage difference names the correct stage.
  8. **Scheduled-tier runner and missing-evidence audit:** cancelled/skipped/artifact-less controls.
  9. **T1, then N8:** derived trusted closure and owner-approved growth diff; resolved-binary hash and clean-rebuild controls.
  10. **C1, then C3:** deterministic counters and declared-complexity registry; counter drift and undeclared-stage controls.
  11. **C2, then C4/C5:** per-dimension families, RSS/output accounting, exact-SHA history; non-dead family and planted quadratic controls.
  12. **Rule-registry prerequisites R-0603/R-0592/R-0593**, then **N1, N2, N3, A2**: orphan IDs, vacuous law, missing decision link, and missing rule-obligation controls.
  13. **Slice-1 frontend/Core→BIR prerequisites**, then **A3, A1, N4, N9**: refusal mismatch, shrink-to-minimal, parser crash/nontermination, and wrong documented outcome controls.
  14. **N7, then N6:** unexpected import/section control, followed by an unmapped artifact-trap control.
  15. **N12:** derived stage-to-gate mapping; unregistered stage and proof-vs-test conflation controls.
  16. **N10, X1, X2, X3, X4:** proof-cost history, stale-cache control, missing regression-fixture control, hostile resource fixture, and runtime-fault controls.
  17. **A4 and weekly gate mutation:** survivor reporting and proof that each campaign catches its planted mutants.

  Explicitly slice-1-blocked or dependent: **A1 source generation, A2 full rule coverage, A3, N1, N2 systematic law registry, N3 full traceability, N4, N9, N6 source obligations, C2 source/front-end families, and N12 full-pipeline coverage.** BIR/backend increments for A1, A4, C2, D2, N6, N12, X2, and X3 can proceed now.

  ## Top 5 by payoff/cost for the next two weeks

  1. **N11** — tiny change that prevents test generators becoming part of the compiler’s trusted production library; prerequisite for a sound A landing.
  2. **C6** — small fix closing a real vacuity hole: the advertised quadratic control currently never runs in CI.
  3. **N5/B** — backend agreement over the whole manifest is already implemented and accepted; landing it gives immediate broad wrong-code coverage at no new design cost.
  4. **D3** — small workflow correction delivering the already-decided GitHub/ci-2 reproducibility guarantee instead of two equivalent hosted legs.
  5. **G1 + G3** — the largest item in the five, but the structural foundation for tiers, mandatory planted controls, budgets, and missing-evidence handling; landing it early prevents every later gate inventing its own conventions.

  DONE.

