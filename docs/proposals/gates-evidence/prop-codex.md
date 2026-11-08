Reviewed statically; no files changed, builds run, or network used. References to `branches/*.diff:line` identify lines in the supplied patches, not landed implementations. ISS-0029/0030 and `repro-ci2` are absent from this export; I treat your descriptions as supplied context.

**ITEM C1: AGREE-WITH-CHANGES**  
Why: Use counters that measure actual stage execution. Elaborator heartbeats do not automatically measure compiled compiler work; allocations and hardware instructions are implementation/platform dependent. “Noise-free” is too strong. C currently measures monotonic time (`branches/C-gate-scaling-determinism.diff:371`).  
Where: C; prototype backend counters now, extend with stages in slice 1; substantial measurements on concrete-ci-2.  
Size: LARGE.  
Exists already?: Time measurement only, `branches/C-gate-scaling-determinism.diff:371`.

**ITEM C2: AGREE-WITH-CHANGES**  
Why: Independent dimensions expose problems that C’s coupled block/instruction/site family cannot isolate. Generate only supported constructs; functions, aggregates and call graphs must wait for their slices (`Concrete/README.md`, slice order).  
Where: C; blocks and instructions now, nesting/locals with the front end, remaining dimensions with their slices.  
Size: LARGE.  
Exists already?: One coupled family, `branches/C-gate-scaling-determinism.diff:345`.

**ITEM C3: AGREE-WITH-CHANGES**  
Why: Require every measured stage to declare its input-size measure and growth envelope. A finite fitted exponent cannot establish Big-O, and an `n log n` envelope needs its actual model rather than a universal exponent threshold. C currently uses three sizes and one margin.  
Where: C; now for backend stages, extend from the pipeline registry as stages land.  
Size: LARGE.  
Exists already?: Three sizes at `branches/C-gate-scaling-determinism.diff:423`; common threshold at line 529.

**ITEM C4: AGREE-WITH-CHANGES**  
Why: Output growth and memory growth can regress independently of elapsed time. Start with exact output bytes; measure RSS in isolated processes because a shared process’s peak cannot attribute memory to individual stages. C already computes forcing sizes but discards them in reporting.  
Where: C; output-size checks now, isolated RSS campaigns on concrete-ci-2 subsequently.  
Size: LARGE overall; output-size portion SMALL.  
Exists already?: Forcing size at `branches/C-gate-scaling-determinism.diff:371`; ignored sample field at line 811.

**ITEM C5: AGREE-WITH-CHANGES**  
Why: A constant-factor regression leaves slope unchanged. Record reference workloads by SHA, toolchain and host configuration; establish history before enforcing calibrated budgets. Existing compile-time reporting is explicitly report-only (`Concrete/README.md:587`).  
Where: C plus existing artifact reporting; now on concrete-ci-2, without another landing job.  
Size: LARGE.  
Exists already?: Per-example time/budget reports, `scripts/checks/compiler/slice0_artifacts.sh:74`; no historical regression policy.

**ITEM C6: AGREE**  
Why: C implements both linear and quadratic plants, but normal `scaling.sh` and the workflow never invoke them automatically. Add automatic assertions that linear passes and quadratic fails naming the planted stage and exponent, independently of real-stage failures.  
Where: C fix round; now.  
Size: SMALL.  
Exists already?: Quadratic implementation, `branches/C-gate-scaling-determinism.diff:360`; opt-in invocation at lines 520–525; normal CI command at line 68.

**ITEM D1: AGREE-WITH-CHANGES**  
Why: C already varies cwd, path length, HOME, TMPDIR and environment ordering, but fixes locale and TZ identically. Add supported perturbations incrementally; preserve semantically significant input ordering and compare bytes only within the same target/toolchain profile.  
Where: C determinism and gate 8; small locale/TZ/umask additions now, larger matrices periodically on concrete-ci-2.  
Size: LARGE for the full proposal.  
Exists already?: Existing perturbations, `branches/C-gate-scaling-determinism.diff:214–226`; no full matrix.

**ITEM D2: AGREE**  
Why: Stage-local comparison makes failures actionable. Existing comparison already names the first differing backend artifact; extend that same inventory to U5/U11 printers when those outputs exist, rather than creating another comparator.  
Where: Gate 8 and C; backend coverage now, front-end intermediates with their B0/slice-1 implementations.  
Size: SMALL per stage.  
Exists already?: `scripts/checks/lib/repro_compare.py:14` and `:35`; front-end intermediates absent.

**ITEM D3: AGREE-WITH-CHANGES**  
Why: Keep identical revision, toolchain identity, artifact inventory and target profile across machines. Main already compares two GitHub Linux runners, but neither its matrix nor supplied patches establish concrete-ci-2 participation; ISS-0029 is absent here.  
Where: Existing gate 8 workflow; now, coordinated with the described `repro-ci2` work.  
Size: SMALL for workflow alignment.  
Exists already?: `.github/workflows/lean_action_ci.yml:446` and `:481`; not the requested server comparison.

**ITEM A1: AGREE-WITH-CHANGES**  
Why: A already has scoped, typed slice-0/BIR generation and replay, but no shrinker. Extend it with validity-preserving shrinking that retains the same mismatch; do not reproduce admission semantics inside the generator.  
Where: A; shrinking now, richer type-directed generation as slice 1 adds types and lowering.  
Size: LARGE.  
Exists already?: Generator production table, `branches/A-gate-spec-diff.diff:296`; no shrinking.

**ITEM A2: AGREE-WITH-CHANGES**  
Why: Count supported semantic rules, not merely generator production labels. Derive the rule inventory from Lean and use deterministic witnesses for mandatory coverage; random coverage alone creates seed-dependent failures. Whole-spec coverage is inappropriate before corresponding compiler slices exist.  
Where: A plus B’s fixtures; slice-scoped coverage now, expanded with each slice.  
Size: LARGE.  
Exists already?: Construct-label checks, `branches/A-gate-spec-diff.diff:1009`; no semantic-rule metric.

**ITEM A3: AGREE-WITH-CHANGES**  
Why: Agreement must distinguish semantic rejection, unsupported fragment and infrastructure failure. Today A discards rejected candidates, and the spec checker returns a Boolean; exact rule attribution needs the planned Lean diagnostic records. Multiple violations also need an existing specified selection policy.  
Where: A and gate 4/B; slice 1 after Core lowering and R-0593/R-0603 diagnostic interfaces.  
Size: LARGE.  
Exists already?: Planned agreement, `Concrete/README.md:574`; rejected candidates discarded at `branches/A-gate-spec-diff.diff:658`.

**ITEM A4: AGREE**  
Why: Broader compiler mutation campaigns test whether independent comparison detects realistic wrong-code changes. Keep targeted plants in every normal A run, with additional stage-specific mutants in private copies on concrete-ci-2.  
Where: A and existing coverage work R-0498; backend mutants now, lowering mutants once lowering exists.  
Size: LARGE.  
Exists already?: Automatic overflow/off-by-one controls, `branches/A-gate-spec-diff.diff:1022`; no broad campaign.

**ITEM A5: AGREE-WITH-CHANGES**  
Why: The theorem goal is already explicit in ROADMAP’s B0/B1/LP0 and `Concrete/README.md` proof strategy. Link A to that goal, but retain executable differentials: proofs remain conditional on printers, toolchains, runtime and artifact binding.  
Where: A documentation and existing roadmap rows; now, proof-coverage enforcement as stage evidence lands.  
Size: SMALL.  
Exists already?: `ROADMAP.md:180` and `:182`; `Concrete/README.md:73`.

**ITEM T1: AGREE-WITH-CHANGES**  
Why: Derive observable facts and compare them against an explicitly approved policy. Discovery cannot infer why an OS, runtime or printer is trusted, nor prove absence from one execution trace. Preserve one Lean registry for those explanations and derive its rendered inventories.  
Where: D/gate 9; now for declarations, later for complete executable identities. Growth requires owner approval, not merely updating a golden.  
Size: LARGE for the full proposal.  
Exists already?: Derived axiom/trusted-declaration audits, `branches/D-trusted-base.diff:243` and `:272`; toolchain/Lake recording, `scripts/checks/compiler/slice0_trusted_base.sh:40`.

**ITEM G1: AGREE-WITH-CHANGES**  
Why: A declaration that a control exists does not demonstrate that it ran or failed correctly. A small inventory linking properties to executable controls is useful; a universal new gate framework has insufficient payoff under the tooling freeze.  
Where: Existing root checks/R-0498; inventory now, execution in each owning gate.  
Size: LARGE as proposed.  
Exists already?: Obligation recorded in `ROADMAP.md:473`; no universal gate manifest.

**ITEM G2: AGREE-WITH-CHANGES**  
Why: Completed runs that disagree must fail, and a semantic failure must never become green through retry. Distinguish this from the existing bounded runner’s explicit timeout recovery policy; changing that policy globally needs a separate justified proposal.  
Where: A/C and gate 8 now; preserve disagreement evidence locally in those gates.  
Size: SMALL.  
Exists already?: Byte disagreement fails at `scripts/checks/lib/repro_compare.py:35`; timeout retry policy at `scripts/checks/lib/bounded.sh:9`.

**ITEM G3: AGREE**  
Why: Record gate duration and critical-path cost against exact SHAs before adding required jobs. C adds independent builds and a required scaling job, so gate cost must include setup/build time, not just measured backend stages.  
Where: Existing workflow/artifact reporting; now, with heavy campaigns on concrete-ci-2.  
Size: SMALL for measurement; historical enforcement belongs with C5.  
Exists already?: Compiler-step timings, `scripts/checks/compiler/slice0_artifacts.sh:74`; no complete gate-cost history.

**ITEM N1: AGREE-WITH-CHANGES**  
Why: Every language refusal should name a valid, versioned Lean rule. Not every spec rule admits a refusal example: positive semantic rules and theorems do not. Require witnesses for refusal-producing rules in the implemented fragment, and reject stale fixture IDs.  
Where: B/gate 4, not a separate gate; with R-0593/R-0603 and the front end.  
Size: LARGE.  
Exists already?: Manifest refusal syntax, `branches/B-gates-manifest.diff:776`; intended refusal leg, `Concrete/README.md:574`.

**ITEM N2: DISAGREE**  
Why: “Every law has a counter-example” is not meaningful for unconditional true laws, and generic non-triviality is not mechanically decidable. Require explicit inhabited hypotheses and boundary witnesses for important conditional claims, while mutation controls test the checking machinery separately.  
Where: Existing Lean spec/proof work, now and per slice; no universal new gate.  
Size: LARGE as proposed.  
Exists already?: Examples and executable checks described in `ConcreteSpec/README.md:12`; no universal vacuity checker.

**ITEM N3: AGREE-WITH-CHANGES**  
Why: Traceability is useful, but project/process decisions have no language definition, and some definitions implement several decisions. Use Lean-held provenance with generated links, explicit supersession and scoped exceptions; do not create another semantic registry.  
Where: Existing spec-site generation and root link checks; A2i now, incremental with subsequent slices.  
Size: LARGE.  
Exists already?: Decision citations, `ConcreteSpec/README.md:24`; declaration/proof links at `:20`; no bidirectional completeness check.

**ITEM N4: AGREE-WITH-CHANGES**  
Why: Fuzz bytes for crashes and bounded resource use, and fuzz valid ASTs for normalized parse/print agreement. Printing may omit spans or normalize identities, so raw AST equality needs qualification. Do not encode undecided grammar spellings.  
Where: Gate 4/A front-end leg; after R-0622 owner-approved grammar and R-0620 implementation.  
Size: LARGE.  
Exists already?: Front-end obligations, `ROADMAP.md:459–460`; no implemented fuzz gate.

**ITEM N5: AGREE-WITH-CHANGES**  
Why: B’s runner already compares both native backends against the same complete observation, giving agreement transitively. It currently selects only `bir:` rows, so “whole manifest” overstates coverage: source/refusal rows need the front end.  
Where: B/gate 4; BIR now, source rows with front-end compilation.  
Size: SMALL for retaining current coverage; source integration is LARGE.  
Exists already?: `branches/B-gates-manifest.diff:530–548`; exact status/stdout/stderr contract at line 513.

**ITEM N6: AGREE-WITH-CHANGES**  
Why: Named BIR traps already carry sites, but emitted machine instructions can introduce additional failure paths. Combine structural artifact inspection with the existing obligation/site graph; distinguish intended traps from toolchain/runtime failures. Any new permission rule must return to the owner.  
Where: Artifact gate plus R-0118/R-0498; backend accounting now, source obligations when provenance/lowering exists.  
Size: LARGE.  
Exists already?: Source/site design, `Concrete/README.md:70`; structural trap inspection remains a follow-up in that README.

**ITEM N7: AGREE-WITH-CHANGES**  
Why: Audit owned objects and final-image imports against a target-specific policy. A blanket “no libc” rule would reject the existing runtime’s intentional `write`, `_exit` and `abort`; audit unexpected imports instead and plant one.  
Where: Existing artifact gate, linked to D’s trust registry; slice 0 now.  
Size: LARGE for robust ELF/Mach-O coverage.  
Exists already?: External-call analysis, `scripts/checks/compiler/slice0_artifacts.sh:139`; broader inventory planned in `Concrete/README.md`, “Artifact facts”.

**ITEM N8: AGREE-WITH-CHANGES**  
Why: Pins and identities are necessary, but rebuilding LLVM/Lean reproducibly is a substantial supply-chain project. First verify actual consumed tool identities against lock-derived expectations; reserve full toolchain rebuilds for periodic server work. A matching hash does not prove trustworthiness.  
Where: Gate 8 plus D/gate 9; identity checks now, full rebuild verification later.  
Size: LARGE.  
Exists already?: Toolchain/Lake inventory, `scripts/checks/compiler/slice0_trusted_base.sh:40`; independent rebuild gate, `Concrete/README.md:583`.

**ITEM N9: AGREE-WITH-CHANGES**  
Why: There is no active `docs/book` in this export. Keep executable examples in Lean or B’s manifest and render documentation from them. Historical or future-slice examples must be explicitly classified rather than forced through today’s compiler.  
Where: B and spec-site generation; current examples now, book integration when an active book exists.  
Size: LARGE as an extraction/coverage system.  
Exists already?: Lean example model, `ConcreteSpec/README.md:12`; no active book gate.

**ITEM N10: AGREE-WITH-CHANGES**  
Why: Proof-resource creep matters, but elaboration, kernel checking and independent replay are different costs. Start with module-level measurements and expensive-theorem diagnostics; per-theorem history and thresholds need calibrated server baselines.  
Where: Existing proof jobs, sharing C5’s history mechanism; now for reports, later for thresholds.  
Size: LARGE.  
Exists already?: Replay timing, `scripts/proofs/kernel_replay.sh:81`; no per-theorem historical budget.

**ITEM N11: AGREE**  
Why: A’s current patch imports test generators through `Concrete.lean`, expanding the production library and D’s audit population. Move generators into a test-owned namespace/root with one-way imports, then plant a forbidden reverse import.  
Where: A fix round plus a light root import-layer check; now.  
Size: SMALL.  
Exists already?: The unwanted imports are in `branches/A-gate-spec-diff.diff:47`; no enforcing check.

**ITEM N12: AGREE-WITH-CHANGES**  
Why: A stage-to-gate map should identify properties and actual exercised edges; one superficial test is insufficient coverage. Derive stage identity from the compiler registry, and require a missing-stage control rather than another hand-maintained pipeline list.  
Where: Existing R-0118/R-0498 and gate 1 infrastructure; backend entries now, extend as stages land.  
Size: LARGE.  
Exists already?: Explicit roadmap requirements, `ROADMAP.md:471–473`; no complete implemented enforcement.

**ITEM X1: AGREE**  
Why: C validates sample completeness only for stages that appear. If every `lcb_emit` sample disappears, remaining stages can still pass. Require the expected stage set, unique sample keys and complete repetitions; plant a wholly missing stage and duplicate key.  
Where: C report parser; now.  
Size: SMALL.  
Exists already?: Partial check at `branches/C-gate-scaling-determinism.diff:845`; expected-stage enforcement absent.

**ITEM X2: AGREE-WITH-CHANGES**  
Why: D reduces successful `execve` paths to basenames, so a different executable named `clang` passes its tool check. Compare resolved executable identity with lock-derived expectations; retain basename only for diagnostics. Dynamic libraries remain a separately stated trust boundary.  
Where: D/gate 9; Linux identity check now, other platforms when equivalent evidence is available.  
Size: SMALL for the Linux extension.  
Exists already?: Basename extraction, `branches/D-trusted-base.diff:451–462`; identity enforcement absent.

**ITEM X3: AGREE**  
Why: A permits zero compiled spec programs intentionally, and later only requires some judged spec output when compilation becomes nonzero. Add an explicit capability transition requiring all supported accepted candidates to compile and a meaningful comparison floor, so partial wiring cannot look complete.  
Where: A now for the capability contract; activate enforcement with slice-1 Core→BIR lowering.  
Size: SMALL.  
Exists already?: Unwired hook at `branches/A-gate-spec-diff.diff:437`; weak nonzero comparison check at line 1058.

**ITEM X4: AGREE**  
Why: Route C’s heavy scaling measurements to concrete-ci-2 and replace the new workflow commands’ GNU `timeout` with the repository’s bounded runner. The patch currently selects `ubuntu-latest` for scaling and uses `timeout` on the macOS determinism leg, contrary to the stated workflow constraints.  
Where: C workflow fix round; now.  
Size: SMALL.  
Exists already?: Problematic commands/routing, `branches/C-gate-scaling-determinism.diff:46–68`; portable runner at `scripts/checks/lib/bounded.sh:22`.

My ranked top five for the next two weeks:

1. **C6** — Automatically validate the scaling detector with its existing plants.
2. **X1** — Prevent missing measurement stages from producing green results.
3. **N11** — Remove test-generator imports from the production compiler library.
4. **X4** — Correct heavy-job placement and portable execution bounds.
5. **X3** — Make the future Core differential’s activation enforce meaningful coverage.

DONE.