The tier split is sound, with changes to the gate designs and their execution contracts. Approval to add every item does not make an unwired comparison, a declared-but-unrun control, or an invalid semantic transformation acceptable.

The largest corrections are:

- Keep complete fixture coverage, automatic planted controls, and the required GitHub↔concrete-ci-2 reproducibility comparison in PR.
- Split expensive exploration from deterministic regression checks; move campaigns to scheduled tiers without weakening them.
- Replace universal “counterexamples to every law” with explicit non-vacuity obligations.
- Distinguish unavailable compiler capabilities from passed checks.
- Treat ten minutes as an unvalidated **added-cost target**, not an established CI duration.

References to `branches/*.diff` describe proposals, not landed code. This was a static, offline review; no files were changed and no builds were run.

## TIERS

PR costs below are **planning estimates in seconds**, including the item’s automatic controls but excluding compiler/toolchain setup and builds. They assume shared, immutable build artifacts where independence is unnecessary. They are not measured results, and overlapping rows must not be added twice.

“Now” means implementable against the present slice-0 backend. Future capability rows must name their blocking dependency; their absence must never appear as successful execution.

### Existing gate improvements

| Item / design verdict | Recommended tier; estimated PR cost | Design, home, slice, size, and existing evidence |
|---|---|---|
| **C1 AGREE-WITH-CHANGES** | **Split:** PR deterministic work counters, **10–30s** shared with C2/C3; daily allocations, instructions and time | Instrument actual stage work. Elaborator heartbeats do not measure arbitrary native compiler execution; hardware counters are supporting, host-specific evidence. **C; now**, extend per stage. **LARGE.** Existing measurement is elapsed time: `branches/C-gate-scaling-determinism.diff:371`. |
| **C2 AGREE-WITH-CHANGES** | **Split:** PR small independent families within C1’s cost; daily every supported family at large sizes | Scale one dimension at a time and report actual input measures. Unsupported dimensions await their feature slices. **C; backend families now; source nesting/locals/front-end families blocked. LARGE.** Existing coupled family: `branches/C-gate-scaling-determinism.diff:345`. |
| **C3 AGREE-WITH-CHANGES** | **Split:** PR declaration/completeness and small counter checks, **2–5s** beyond C1; daily ≥4-size fits | Declare size measures and growth envelopes. Fit `n log n` as that model; a finite exponent fit is evidence, not a Big-O proof. Missing stages or samples fail. **C; now. LARGE.** Existing three sizes/common threshold: `branches/C-gate-scaling-determinism.diff:423,529`. |
| **C4 AGREE-WITH-CHANGES** | **Split:** PR exact output-growth checks, **2–10s**; daily isolated-process RSS and large output growth | Attribute memory to individual stages/processes. Keep output-growth enforcement distinct from the existing report-only final binary-size record. **C/artifact gate; now. LARGE**, output portion SMALL. Existing forcing sizes: `branches/C-gate-scaling-determinism.diff:371`; final binary reporting: `Concrete/README.md:585`. |
| **C5 AGREE-WITH-CHANGES** | **Daily** reference workloads, absolute budgets and history; weekly trend review | Record SHA, workload, target, toolchain and host profile. Calibrate before enforcing noisy thresholds; retain raw samples and approved baseline changes. **C/history reporting; now. LARGE.** Existing per-example reporting: `scripts/checks/compiler/slice0_artifacts.sh:74`; historical enforcement: **no**. |
| **C6 AGREE** | **PR, 2–10s** with counter-based plants; daily also exercise the full measurement path | Automatically require linear acceptance and quadratic rejection, naming the planted stage and reason. A real compiler failure cannot substitute for catching the plant. **C; now. SMALL.** Plants exist but are opt-in: `branches/C-gate-scaling-determinism.diff:360,520–525`. |
| **D1 AGREE-WITH-CHANGES** | **Split:** PR two distinct executions, cost shared with D2; daily perturbation matrix | Vary locale, TZ, paths, environment, umask and supported seed controls. Reorder only semantically unordered inputs; compare within identical target/toolchain profiles. **C/gate 8; now, extend per slice. LARGE.** Partial perturbations: `branches/C-gate-scaling-determinism.diff:214–226`. |
| **D2 AGREE** | **PR, 60–180s** for the combined D1/D2 incremental execution work | Compare the complete registered artifact inventory and name the earliest differing stage. Missing printers fail once the stage exists. **C/gate 8; backend now; front-end printers with their implementation. SMALL per stage.** Backend comparator: `scripts/checks/lib/repro_compare.py:14,35`; future printers: **no**. |
| **D3 AGREE-WITH-CHANGES** | **PR required; 5–15s comparison**, plus an independent concrete-ci-2 build/execution leg whose cost is unmeasured | Bind both sides to the exact SHA, pins, target and artifact inventory. Preserve independence; do not share compiled outputs between these two legs. **Gate 8; now. SMALL alignment change.** Current workflow compares two GitHub Linux runners: `.github/workflows/lean_action_ci.yml:459–464,480–502`; requested server leg absent from export. |
| **A1 AGREE-WITH-CHANGES** | **Split:** PR fixtures plus short fixed-seed generated differential, **20–60s** incremental; daily bounded ~1h campaign; weekly overnight | Add validity-preserving shrinking and exact replay. Do not duplicate the checker’s admission rules. **A; BIR generator/shrinking now; actual Core/native differential blocked on slice-1 lowering; source generation also needs front end. LARGE.** Typed generation exists, shrinking does not: `branches/A-gate-spec-diff.diff:296,437`. |
| **A2 AGREE-WITH-CHANGES** | **PR, 2–10s** beyond fixture execution | Derive the implemented rule inventory from Lean; require deterministic fixture witnesses. Random generation supplements coverage. Track unsupported rules explicitly. **A+B; slice-scoped now, richer semantic instrumentation as stages land. LARGE.** Existing production-label coverage: `branches/A-gate-spec-diff.diff:1009`; rule coverage: **no**. |
| **A3 AGREE-WITH-CHANGES** | **PR, 5–20s** beyond fixture execution | Distinguish semantic refusal, unsupported capability and infrastructure failure. Compare versioned rule IDs using the specified diagnostic-selection contract. **A+B/gate 4; blocked on diagnostics and lowering/front end. LARGE.** Obligation: `Concrete/README.md:574`; current A discards rejected candidates: `branches/A-gate-spec-diff.diff:658`. |
| **A4 AGREE** | **Split:** targeted controls PR, **5–20s** shared with A; full compiler mutation weekly | Require realistic mutants to be killed by the intended comparison, in private copies; report survivors and invalid mutants separately. **A; backend now, lowering mutants after slice 1. LARGE.** Targeted controls: `branches/A-gate-spec-diff.diff:1022`; broad campaign: **no**. |
| **A5 AGREE-WITH-CHANGES** | **PR** proof-coverage linkage, **1–3s** once evidence exists | Link the gate to universal per-pass proofs and exact artifact binding. Retain executable differentials across residual printer/toolchain/runtime assumptions. **A+pipeline coverage; roadmap linkage now; enforcement with proofs. SMALL.** Existing proof goal: `ROADMAP.md:180–182`; executable trust boundary: `Concrete/README.md:73`. |
| **T1 AGREE-WITH-CHANGES** | **Split:** PR derived declarations/axioms/consumed identities = approved registry, **10–30s**; daily broader execution traces; weekly supply-chain audit | Derive observable facts; retain explicit approved premises and explanations. An execution trace cannot prove absence of an unused tool. Compare executable identities, not basenames. Trust growth requires recorded owner approval. **D/gate 9; now, extend with executable pipeline. LARGE.** Derivation: `branches/D-trusted-base.diff:243,272`; basename weakness: `:451–462`. |
| **G1 AGREE-WITH-CHANGES** | **PR, 3–10s** inventory/evidence validation; owning gates run their controls; weekly additional mutations | Each gate declares property, scope, tier, invocation, budgets and controls. Require current-run evidence of the expected control failure; a declaration alone is insufficient. **Root check + owning gates; now. LARGE.** Obligation exists at `ROADMAP.md:473`; complete implementation: **no**. |
| **G2 AGREE-WITH-CHANGES** | **All tiers; PR 1–3s** result-policy checks | Completed semantic disagreement stays failed even if a subsequent attempt passes. Preserve all attempts; classify timeout recovery separately, never erase its evidence. **Shared result handling/A/C/gate 8; now. SMALL.** Byte disagreements already fail: `scripts/checks/lib/repro_compare.py:35`; timeout policy: `scripts/checks/lib/bounded.sh:9`. |
| **G3 AGREE-WITH-CHANGES** | **PR, 1–3s** accounting; daily history; weekly trend review | Measure setup, builds, controls, gate body, runner occupancy and critical path separately. An exceeded PR budget fails; it must not truncate work and report success. **Workflow + shared reporting; now. SMALL initial slice.** Root durations: `scripts/checks/run.sh:21–23`; complete accounting: **no**. |

### New gates

| Item / design verdict | Recommended tier; estimated PR cost | Design, home, slice, size, and existing evidence |
|---|---|---|
| **N1 AGREE-WITH-CHANGES** | **PR, 2–5s** beyond refusal execution | Require valid IDs on language refusals and fixtures for implemented **refusal-producing** rules. Positive semantic rules need witnesses, not invented refusal cases. **B/gate 4; blocked on R-0593/R-0603 diagnostics. LARGE.** Manifest syntax: `branches/B-gates-manifest.diff:776`; enforcement: **no**. |
| **N2 DISAGREE** | Replacement **PR, 5–20s**; expensive witness searches daily | Build explicit non-vacuity obligations: inhabited hypotheses, feasible contexts, boundary witnesses and claim-strength checks. True unconditional laws need no counterexample; plant weakened/vacuous claims to test detection. **Spec/proof checks, R-0585; now and per slice. LARGE.** Examples exist: `ConcreteSpec/README.md:12–15`; universal proposed gate: **no**. |
| **N3 AGREE-WITH-CHANGES** | **PR, 2–5s** | Trace language decisions bidirectionally with many-to-many links, supersession and explicit scoped exceptions. Process decisions do not require language definitions. Lean owns semantic provenance. **Spec-site/root check; now, incremental. LARGE.** Existing citations/site links: `ConcreteSpec/README.md:19–27`; completeness check: **no**. |
| **N4 AGREE-WITH-CHANGES** | **Split:** PR lexical/parser conformance and fixed hostile corpus, **10–30s**; daily ~1h fuzz; weekly overnight | Byte fuzzing checks crashes/resources; valid-AST fuzzing checks normalized parse/print correspondence. Lexer starts against landed lexical rules; parser awaits owner-approved grammar. **A/front-end gate; R-0620/R-0622, not necessarily slice 1. LARGE.** Planned at `ROADMAP.md:459–460`; implemented fuzz gate: **no**. |
| **N5 AGREE-WITH-CHANGES** | **PR, 30–90s** manifest execution; replaces existing backend work | Compare complete observations against the independent oracle, including trap identity, not only backend equality. Run every supported row; report pending source/refusal capabilities explicitly. **B/gate 4; BIR now, source after front end/lowering. SMALL current integration; LARGE source extension.** Already in B: `branches/B-gates-manifest.diff:513,530–548`. |
| **N6 AGREE-WITH-CHANGES** | **PR, 10–30s** | Account structurally for emitted failure paths and bind intended trap sites to obligations. Runtime/toolchain failures remain separately classified. **Artifact gate + pipeline provenance; BIR now; source obligations after lowering/positions. LARGE.** Site design: `Concrete/README.md:70`; complete binary trap audit: **no**. |
| **N7 AGREE-WITH-CHANGES** | **PR, 5–20s** | Audit owned objects and final-image symbols/imports/sections against target-specific approved policy. Intentional libc calls remain explicit. Plant an unexpected import on each supported format. **Artifact gate + D registry; now. LARGE** across ELF/Mach-O. Partial external analysis: `scripts/checks/compiler/slice0_artifacts.sh:139`. |
| **N8 AGREE-WITH-CHANGES** | **Split:** PR consumed-tool identity/pin checks, **2–10s** shared with T1; daily clean project rebuild and hashes; weekly full toolchain rebuild including Nix inputs and supply-chain audit | Specify precisely what is rebuilt. A daily cache-free project build using pinned toolchains is different from rebuilding Lean/LLVM themselves. Compare independent outputs and provenance. **Gate 8+D; identity now, rebuild infrastructure staged. LARGE.** Existing toolchain/pin inventory: `scripts/checks/compiler/slice0_trusted_base.sh:40`; full rebuild audit: **no**. |
| **N9 AGREE-WITH-CHANGES** | **PR, 5–20s** incremental with shared fixtures | Render examples from executable Lean/manifest sources; classify historical and future examples explicitly. Do not silently skip live examples. **B+spec-site; current examples now; surface examples after compiler support. LARGE.** Lean examples exist: `ConcreteSpec/README.md:12–15`; active `docs/book` gate: **no**. |
| **N10 AGREE-WITH-CHANGES** | **Split:** PR deterministic resource limits and generous wall-clock watchdog, **1–3s** accounting; daily proof history; weekly trends | Separate elaboration, kernel checking and external replay. A tight wall-clock cap is not deterministic. Measure first, then calibrate theorem/module budgets without reducing proof coverage. **Existing proof jobs; now. LARGE** overall. Replay timing: `scripts/proofs/kernel_replay.sh:81`; per-theorem history: **no**. |
| **N11 AGREE** | **PR, 1–3s** | Move generators out of production imports and enforce one-way layering with a forbidden-import plant. **A + root check; now. SMALL.** Current violation: `branches/A-gate-spec-diff.diff:47`; enforcing gate: **no**. |
| **N12 AGREE-WITH-CHANGES** | **PR, 2–5s** | Derive stage/edge identity from compiler contracts. Require property-specific execution and control evidence, not merely a gate name per stage. **Pipeline coverage + G1; backend now, extend per slice. LARGE.** Existing obligations: `ROADMAP.md:471–473`; complete enforcement: **no**. |

### Group 3 — additional test types

| Item / verdict | Tier and estimated PR cost | Slice, size, existing evidence, and home |
|---|---|---|
| **T-a AGREE-WITH-CHANGES** | **Split:** PR deterministic applicable transformations, **10–30s**; daily generated combinations | Require each rewrite’s semantic preconditions. Reordering can change effects; dead code can change admission/ownership; alpha-renaming must avoid capture and exclude externally visible names. Compare source observations while mapping diagnostic locations. **A spec-diff; Core subset now, shadowing/source tests after front end, more rewrites with their features. LARGE.** Generator scope exists at `branches/A-gate-spec-diff.diff:296`; metamorphic suite: **no**. |
| **T-b AGREE-WITH-CHANGES** | **Split:** PR boundary matrix, **10–30s**; daily exhaustive u8/i8 operand pairs for every supported operator/backend | Batch compilation and result transport, preserving identification of every case. Compare full arithmetic results, traps and names; exit codes alone lose wider results. Unary operators have one operand; shifts and other operators need their specified domains. **A+B; BIR now, actual spec→native path after lowering; operators as their slices land. LARGE.** Existing arithmetic fixtures: `Concrete/BIR.lean:553–580,656–663`; exhaustive suite: **no**. |
| **T-c AGREE-WITH-CHANGES** | **PR**, every exercised pass output, **5–20s** initially; also enabled in daily/weekly campaigns | Give each real IR a total checker and soundness proposition, integrated at the pass boundary. Name the first invalid output and producing pass. Avoid inventing six parallel representations merely to match the list. **Pipeline/root compiler checks + A/B; BIR now, other IRs with their passes. LARGE.** BIR `wf`/`WF`: `Concrete/BIR.lean:430–504`; progress connection: `Concrete/BIR/Progress.lean:353–359`; all-pass integration: **no**. |
| **T-d AGREE-WITH-CHANGES** | **Split:** retain PR ASan and add native memory-check smoke on both backends, **20–60s**; daily full manifest under native memory checking | Check unsanitized emitted machine code from both backends; ASan currently covers LLVM only. Classify intentional trap termination and runtime ownership; suppressions must be precise and reviewed. Unsupported platform coverage is explicit. **Artifact/sanitizer gate 10; slice-0 Linux now, equivalent target tooling as available. LARGE.** Partial: `Concrete/README.md:584`; LLVM ASan/control: `scripts/checks/compiler/slice0_artifacts.sh:1–11`; both-backend memcheck: **no**. |
| **T-e AGREE-WITH-CHANGES** | **Split:** PR deterministic ABI conformance matrix, **10–40s** when enabled; daily generated combinations | Compare size/alignment/offsets and calls in both directions on every supported ABI. Use clang as an independent target-profile reference, not the language specification. Include aggregates, padding and register/stack boundaries as supported. **New ABI gate, invoked through B where useful; later, blocked on decided repr(C)/extern support. LARGE. Exists: no.** |
| **T-f AGREE** | **PR, 5–15s** beyond refusal execution | Pin exact human text and structured diagnostics: span, rule revision, ordering, schema and exit code. Updates require reviewed diffs; do not normalize away meaningful differences. **B/gate 4; first front-end output, R-0593/R-0603. LARGE** initial integration. Already required by `ROADMAP.md`, R-0593; implemented suite: **no**. |
| **T-g AGREE-WITH-CHANGES** | **PR, 3–10s** | Fingerprint canonical elaborated theorem types with readable statement diffs, universe parameters and relevant dependency-definition identities. Type hashes alone miss weakened predicates under unchanged names. Plant both direct statement weakening and changed referenced definitions. **Proof checks + D; now. LARGE** for the structural version. Axiom inventories exist at `scripts/proofs/axioms.sh:2–17`; rendered statements at `ConcreteSpec/README.md:19–23`; weakening detection: **no**. |
| **T-h AGREE-WITH-CHANGES** | **Split:** PR fixed boundary/hostile corpus, **10–30s**; daily 10k-function/large limits and scaling; weekly combinations via fuzzing | Every run must succeed, produce a specified refusal, or report infrastructure INCOMPLETE. Limits must be explicit; a watchdog kill is not a language refusal. **B + C + front-end fuzz gate; BIR limits now, lexical/parser limits with front end, function limits with functions. LARGE.** Resource obligation exists at `ROADMAP.md`, R-0528; requested comprehensive suite: **no**. |
| **T-i DISAGREE** | Replacement **PR, 5–15s**; daily generated control-flow cases | Test the actual decided termination/permission/resource policy. Do not impose universal refusal of every possibly diverging program: existing decisions allow named permissions and conditional facts. Evaluator fuel exhaustion is not proof of divergence. Include refusal cases, permitted unresolved cases, truthful facts and hidden-trap accounting. **B/refusal gate + facts + N6; spec cases now, compiler diagnostics after front end, recursion/loops with their features. LARGE.** Policy recorded under `ROADMAP.md` R-0575/R-0576; runtime nonreturning control at `scripts/checks/compiler/slice0_backends.sh:44–46`; proposed universal refusal suite: **no**. |
| **T-j AGREE-WITH-CHANGES** | **PR, 2–5s** metadata; fixture execution charged to its owning gate | Every fixed issue needs a failing-before/passing-after regression and an executable coverage reference. Compiler defects use B’s manifest; CI/proof/tooling defects use their owning control suites. A source program cannot reproduce every infrastructure bug. **Root issue check + B/owning gates; now, source fixtures as supported. LARGE** ledger integration. Issue criteria exist in `ISSUES.tsv`; universal fixture-link verification: **no**. |

For **T-i**, the quoted slogan cannot silently replace the more detailed decided permission policy. If the owner intends that language change, it needs an explicit decision before implementation.

### Is the ten-minute PR budget realistic?

**It is not established by this export.** A ten-minute *total* PR duration is especially unsupported.

`required-checks.txt:6–23` lists **18 required job instances**. The workflow independently builds the specification in the spec/proof jobs, runs two native backend/artifact/trust profiles, runs three reproducibility profiles, and then compares Linux outputs. External re-check alone has a 90-minute timeout; that is a bound, not a duration measurement (`.github/workflows/lean_action_ci.yml`, `external-recheck`). Reproducibility has a dependent comparison job (`:480–502`).

A’s proposed generated differential already has a default 600-second bound including its shared build (`branches/A-gate-spec-diff.diff:90–110`). Adding that configuration unchanged, plus C and D, cannot substantiate a ten-minute total added budget.

The estimates above put warm incremental PR work in the **several-minute to over-ten-minute range**, before independent builds, target duplication, queueing and setup. A measured representative baseline is necessary.

To pursue ten minutes of added work:

- **Daily:** move large scaling/RSS measurements, perturbation matrices, exhaustive arithmetic, full native memcheck, long generated comparisons, large limit workloads and proof history.
- **Weekly:** keep broad compiler/gate mutation, overnight fuzzing, full toolchain rebuilds and supply-chain audits.
- **PR:** retain the complete supported fixture corpus, rule/refusal coverage, structural audits, validators, automatic controls, short deterministic generated tests, theorem statements, and required reproducibility.
- Reuse exact-SHA build products for ordinary checks where this preserves the property. **Independent rebuild and cross-machine checks must remain independent.**

Do not move existing required proof checks to scheduled tiers merely to claim the target was met. Measure cold/warm builds, runner contention and each target separately; if required work still exceeds ten minutes, report that evidence and revise execution architecture or the budget.

### Scheduling, recording, and missing runs

**Use a separate GitHub scheduled workflow on self-hosted concrete-ci-2 runners**, with manual dispatch for replay. This best preserves the existing exact-SHA GitHub/train conventions. Use a host-specific runner label; the current generic `concrete` label does not by itself establish machine identity.

Keep required landing job names in `required-checks.txt`. Scheduled jobs belong in the gate manifest with their tier and invocation, **not** in that required-job list. Its current “root has no campaign tier” comment must change alongside workflow checks. Schedule entry points belong below `scripts/checks/`; placing a heavy standalone `.sh` at its root would accidentally run it through `run.sh:17`.

For each scheduled occurrence:

1. Resolve main once and record the full SHA before checkout. Keep daily and weekly occurrences identifiable even when they target the same SHA.
2. Run in a private checkout and `nix develop`, using `bounded_run`. Honour the host-wide limit of two heavy runs, including trusted landing/reproducibility work.
3. Record occurrence ID, expected window, full SHA, gate revision, tier/profile, tool identities, seeds, commands, resource measurements, control outcomes and final status.
4. Upload logs, samples, minimized reproducers and a machine-readable result index even on failure. Preserve history beyond ordinary short-lived artifact retention.
5. Link confirmed defects from `ISSUES.tsv` to the exact failing SHA, reproducer, structural cause and structural owner. Keep run history in result artifacts/indexes; the issue ledger is not a row-per-run database.

Use explicit **PASS / FAIL / INCOMPLETE / MISSING** distinctions. A cancelled, skipped, queued-too-long or absent run is never green. A replay at the same SHA records another attempt; it does not overwrite the original failure.

**Missing-run detection needs an observer outside the scheduled worker.** A GitHub-hosted watchdog should compare expected windows from the manifest with completed result records, including control evidence. Also run that freshness check during normal CI so a missed watchdog schedule becomes visible. A small systemd watchdog on concrete-ci-2 can provide additional notification, but should not become a second campaign scheduler.

The observer reports overdue occurrences through a failing freshness check and visible summary. Persistent scheduling defects become ISSUES rows. A later successful run resolves freshness only according to the recorded window/recovery policy; it does not retroactively turn the missed occurrence into a pass.

“Stop the line” and “never block an unrelated landing” need precise scope: **a confirmed scheduled failure triggers immediate triage and structural repair through the train, and blocks work that depends on the broken property. It does not add a blanket failed required check to every unrelated candidate.** Silent defects receive the repository’s immediate response. Missing coverage remains visibly incomplete.

Every scheduled discovery gets a PR regression, but in the **appropriate fixture domain**: compiler program, proof mutation, malformed gate record, binary artifact or scheduler control.

For D3, preserve the existing rule that self-hosted runners do not execute arbitrary PR events (`.github/workflows/lean_action_ci.yml:114–116`). Use a trusted exact-SHA dispatch for the server leg and bind its result into PR/landing qualification.

### Build order: small branches, controls included

Each numbered entry is a separate branch. Shared references below express dependencies, not permission to bundle unrelated gates.

1. **N11 import layering:** relocate A’s generators; plant a forbidden production import.
2. **B manifest landing/N5:** retain complete BIR inventory and observations; existing missing-row, wrong-outcome and stray-output controls.
3. **G1 gate inventory:** property, tier, command, scope and control schema; plant an unregistered gate and missing tier.
4. **G2 result semantics:** preserve disagreement and attempts; plant fail-then-pass evidence.
5. **G3 cost accounting:** measure complete gate execution; plant an over-budget PR result.
6. **Scheduled execution:** exact-SHA private runs and durable results; plant wrong-SHA and absent-result records.
7. **Scheduled freshness:** independent observer; plant cancelled, skipped and overdue occurrences.
8. **N12 pipeline coverage:** registry-derived property coverage; plant a missing stage/edge.
9. **C sample completeness:** reject wholly missing stages and duplicate samples—the prior X1 finding.
10. **C6 automatic controls:** linear passes, quadratic fails for the intended reason.
11. **C1 counters:** instrument actual backend work; plant omitted/inaccurate accounting.
12. **C2 independent families:** **one family per branch**; plant coupled or missing dimension coverage.
13. **C3 growth contracts:** ≥4 sizes, declared measures/models; plant undeclared stage and excessive growth.
14. **C4 output growth:** plant duplicated output.
15. **C4 isolated RSS:** daily measurement; plant retained allocation.
16. **C5 history/budgets:** baseline identities and daily reference workloads; plant constant-factor regression.
17. **D2 artifact completeness:** register current printers; plant a missing and changed intermediate.
18. **D1 perturbations:** add perturbations in separate small branches; plant locale/path/seed dependence.
19. **D3 server comparison:** integrate the required repro-ci2 leg; plant differing bytes and wrong identity.
20. **T1 derived trusted declarations:** extend D; plant unlisted axiom/escape hatch.
21. **T1 executable identities:** remove basename-only acceptance; plant a different executable named `clang`.
22. **N7 binary audit:** target-format branches separately; plant unexpected imports/sections.
23. **N6 backend trap accounting:** plant an unaccounted failure path. Source mapping waits for lowering/provenance.
24. **T-g statement/dependency pinning:** plant direct weakening, then changed predicate definition.
25. **N2 non-vacuity:** explicit claim obligations under R-0585; plant impossible hypotheses and overstated point evidence.
26. **N3 decision traceability:** plant broken provenance and missing reverse coverage.
27. **N9 executable documentation:** plant an omitted live example and incorrect expected result.
28. **T-j issue regression links:** plant a closed issue with no executable regression.
29. **T-c validators:** **one implemented IR/pass boundary per branch**; plant malformed output. BIR now; future boundaries wait for their passes.
30. **T-b arithmetic:** boundary matrix first, then daily exhaustive runner; plant overflow/trap-name and result corruption.
31. **T-d native memory checks:** both-backend smoke, then full daily manifest; plant invalid access and owned leak.
32. **A1 generator replay/shrinking:** BIR now; plant a mismatch whose reduction must preserve failure.
33. **T-a metamorphic checks:** **one justified transformation per branch**; plant capture/effect-order corruption.
34. **A4 weekly compiler mutation:** reuse exact comparison infrastructure; plant a known killable mutant and a surviving-mutant reporting control.
35. **G1 weekly gate mutation:** bypass detectors in private copies; require controls to expose the bypass.
36. **N8 consumed identities:** share T1’s inventory; plant substituted tool.
37. **N8 daily clean rebuild:** plant stale/corrupt build reuse.
38. **N8 weekly full rebuild/audit:** plant altered input identity and unreproducible output.
39. **N10 proof accounting:** PR resource limits, daily history and weekly trends; plant accounting omission and controlled resource excess.
40. **A5 proof linkage:** tie implemented stage claims to actual proof/artifact evidence; plant unrelated evidence substitution.

The dependent front-end/lowering branches then proceed as capabilities arrive:

41. **N4 lexer corpus/fuzzing:** against landed Lean lexical rules; plant crash/nontermination/token corruption. **Front-end blocked; lexer need not wait for the parser or slice 1.**
42. **N4 parser round-trip/fuzzing:** after **R-0622 owner-approved grammar and R-0620 parser**; plant a changed normalized AST.
43. **T-f diagnostic goldens:** first diagnostic output, after **R-0593/R-0603**; plant changed span, ID, order and exit code.
44. **N1 refusal inventory:** after rule IDs exist; plant orphan IDs and a missing refusal witness.
45. **A2 rule witnesses:** one supported rule family per branch; plant an unexercised rule. Backend coverage can start earlier.
46. **A Core→native activation:** after **slice-1 Core→BIR lowering**; require every supported accepted candidate to compile and compare. Plant a zero/partial compilation bypass—the prior X3 finding.
47. **A3 refusal differential:** after diagnostic records and relevant checker/front-end interfaces; plant accept/refuse and rule-ID disagreements.
48. **T-h source limits:** one supported input class per branch; plant crash/hang or misclassified watchdog expiry. BIR limits start earlier.
49. **T-i decided termination-policy tests:** functions/loops/recursion and facts interfaces as implemented; plant false termination evidence, hidden traps and wrongly refused permitted cases.
50. **T-e ABI conformance:** later, with approved extern/layout support; separate target branches, planted layout and calling-convention mismatches.

All staged LARGE items need a roadmap row naming the complete design, dependencies and closure control. Every review prompt should quote the owner’s long-term rule and require reviewers to report shortcuts as findings, with the structural replacement.

DONE.