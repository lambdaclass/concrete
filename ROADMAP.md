# Concrete Roadmap

**What Concrete is aiming at.** Concrete is a systems language with linear ownership, second-class
references and explicit capabilities whose compiler is to be verified end to end: CompCert/CakeML-class
theorems that accepted programs are memory- and ownership-safe, that the native binary only does what
the program's Core semantics allows, and that supported claims about Core transport to the native image.
Its evidence (proofs, receipts, trust and assumptions) is to be compositional, transferable and
independently replayable ([verified-compiler decision](docs/project/ROADMAP_DECISIONS.md#decisions), [product north star](docs/project/ROADMAP_DIRECTION.md#product-north-star-compositional-transferable-evidence)).

**Where we are (2026-10-21).** Phase 1 (Stabilize) is in progress: the soundness repairs for bugs
079 – 101 are landing (queue row 0) and R-0482's P1 closures follow (row 1). Probes added since
2026-10-19 found bugs 102 – 120 in about two days, nearly all of them new instances of the recurring
[structural classes](docs/bugs/STRUCTURAL_CLASSES.md): point repairs are not converging (the
[bug dashboard](#bug-dashboard)'s discovery-rate signal measures this, and Phase 1 exit 2m closes
only when, in every structural class, three consecutive probe batches find about nothing), which is why
the [priority decision of 2026-10-21](docs/project/ROADMAP_DECISIONS.md#decisions) makes the verified compiler's structural work the
primary track. The next release is the experimental systems preview; its
[blockers](#next-release-blockers) are listed under [Releases](#releases). Fixes implemented on
branches but not yet accepted on main are listed under
[Implemented, not yet accepted](#implemented-not-yet-accepted).

**Active phases:** 1

**Next phases:** 2, 3

**What is next.** The verified compiler is the primary engineering track. The
[capacity allocation](#capacity-allocation) protects it with a minimum share of effort and two
integration slots of its own; queue order alone does not. Its order:

1. land the in-flight soundness repairs (row 0) and fix the semantics the proofs rely on: one
   `IntArith` and the written rules for references, binding, literals, statement position, evaluation
   order and casts (exits 2h – 2k);
2. in the migration slot, make verified-pipeline Part 1's structure the shared compiler (row 4) once
   row 0's in-flight candidates are accepted: `DefId`/`LocalId` identities, the authoritative checker
   on Core for the whole language with checked specialization, then one `Layout` authority and Place
   MIR in the existing profile;
3. in the proof slot, V1a: checked Core → BIR preservation (row 3);
4. V1b: the same subset through the backend to the identified native image, with traps, calls, memory
   and the runtime boundary (row 3); and V1c: restricted source → checked Core correspondence. No V1
   result is called source-to-native before V1c holds;
5. grow the subset slice by slice, choosing what to admit next from real programs (row 6).

**Development throughput comes first among the process work (decided 2026-10-22).** A finished
branch took 6 – 10 h to reach main for about 1 h of work. The [development
throughput](#development-throughput) track (R-0539 – R-0547, with R-0513's pre-push selection) brings
in landing trains, CI tiers, generated ledgers, impact-selected pre-push gates, load control, and
review and branch discipline. It is ordered ahead of the next feature wave, and its targets are median
times of under 2 h from Codex-clean to main, ≤ 40 min of landing CI and ≤ 10 min of pre-push.

Evidence integrity (one verdict authority, the axiom policy, minimal task identity, independent replay,
and evidence bound through linking to the final image and its dependency versions) is a second
protected track, and adversarial validation runs continuously. RocksDB slice 1 (row 5) is the one
bounded executable workload alongside. The ethlambda port, broad library expansion and polished
tooling stay bounded until V1b holds. New defects follow the [defect disposition
rule](#the-current-execution-queue): silent ones (memory-safety violations, wrong code, false proofs)
are stopped at once, and the rest are filed with their structural owner and contained fail-closed
until that owner removes their class. Implementation and experiments on every track may proceed now,
within the rules as they are on main (no work chooses, encodes or implements an unanswered
[owner decision](#owner-decisions)):
early executable feedback is how defects surface (the RocksDB groundwork already found bugs 102 and
103). What a deliverable's [prerequisite checklist](#feature-prerequisites) controls is
**acceptance**: RocksDB slice 1, V1a, V1b and V1c are not accepted, and their safety is not
advertised, until their prerequisite exits hold. The remaining exits continue alongside and still
close Phase 1. Every bug record states the design fact behind it and its structural owner.

**Where things live.** This file answers only *what happens next, in what order*: the active and
next phases, the one execution queue with its exits, the owner decisions, the feature prerequisites,
the capacity allocation and the release map. Everything else has its own home:

- [Task register](docs/project/ROADMAP_TASKS.md): the full text of every task a queue row names or an
  active or next phase schedules, filed by area.
- [Roadmap backlog](docs/project/ROADMAP_BACKLOG.md): every other task, each with its pull trigger
  ([pull rules](#backlog-and-pull-rules)).
- [Decision log](docs/project/ROADMAP_DECISIONS.md): the dated decisions that shape the queue.
- [Direction and standing reference](docs/project/ROADMAP_DIRECTION.md): the
  [capability unlocks](docs/project/ROADMAP_DIRECTION.md#capability-unlocks--the-stable-product-map),
  the [north star](docs/project/ROADMAP_DIRECTION.md#north-star-compositional-fail-visible-verification),
  the definition of done, the gate-writing checklist and the [glossary](docs/project/ROADMAP_DIRECTION.md#glossary).
- [Validation and promotion](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline): which checks a
  change passes during development, before merge and before qualification or release, the CI tiers and
  the result vocabulary.
- [Verified-compiler plan](docs/compiler/VERIFIED_PIPELINE.md): Phases 2 and 4 in detail.
- Normative architecture: [Evidence Architecture](docs/verification/EVIDENCE_ARCHITECTURE.md) defines
  the five authoritative evidence objects and [Verification IR](docs/verification/VERIFICATION_IR.md)
  the target portable proposition boundary. This roadmap sequences their implementation and must not
  invent competing definitions.
- Language rules: the language documents under `docs/language/`. The rule-defining sections this file
  used to carry (the [language baseline](docs/archive/roadmap-language-scope.md#language-baseline-and-bounded-verification-milestone),
  the cross-cutting language decisions, the linearity and stdlib-boundary decisions) are in the
  [archived language scope](docs/archive/roadmap-language-scope.md), pending absorption into the
  language specification.
- Defects: the [bug dashboard](#bug-dashboard) below and the [bug records](docs/bugs/README.md).
- Records: the [reconciliation record](docs/project/ROADMAP_RECONCILIATION.md), the
  [roadmap history](docs/archive/ROADMAP_HISTORY.md), the
  [retired task bodies](docs/archive/ROADMAP_RETIRED_TASKS.md) and [CHANGELOG.md](CHANGELOG.md).

## Bug dashboard

Generated by `scripts/gen/bug_dashboard.py --write` from the [bug records](docs/bugs/README.md)
(status), [STRUCTURAL_CLASSES.md](docs/bugs/STRUCTURAL_CLASSES.md) (classes; a bug's primary class
here is the first class row that lists it), the [discovery record](docs/bugs/discovery.tsv) (which
probe batch found each bug, and which are silent) and the [Phase 1 exits](#phase-1-exits) table.
`check_docs_drift.sh` fails when it differs from those inputs, so it is never edited by hand: whoever
files, fixes or reclassifies a bug, or records a probe batch, regenerates it in the same commit.
Silent means a memory-safety violation in accepted code, silent wrong code or value, a false proof
or claim, an authority violation or a vacuous gate; loud means a rejected valid program, invalid IR,
a link failure, an interpreter refusal or an unrejected leak.

<!-- BEGIN GENERATED: bug dashboard (scripts/gen/bug_dashboard.py; edit docs/bugs records and docs/bugs/discovery.tsv) -->

**Open bugs: 10** (1 silent, 4 loud, 5 filed before the discovery record) of 281 bug records.

| open bugs by primary structural class | open | silent | bugs |
|---|---|---|---|
| 1. Declaration identity by spelled name, not a resolved `DefId` | 4 | 1 | 054, 077, 114, 122 |
| 5. Borrow tracking by syntax instead of normalized places | 1 | 0 | 216 |
| 11. Absence read as a fact (fail-open defaults) | 2 | 0 | 049, 058 |
| 12. One fact kept in two places | 2 | 0 | 064, 226 |
| 17. Pass state in association lists and per-query recomputation (superlinear compile time) | 1 | 0 | 270 |

| open bugs by Phase 1 exit | open | silent | bugs |
|---|---|---|---|
| exit 0b | 1 | 1 | 122 |
| exit 2c | 1 | 0 | 114 |
| exit 2n | 2 | 0 | 216, 226 |
| no exit names it | 6 | 0 | 049, 054, 058, 064, 077, 270 |

| open bugs by spec ruling ([RULINGS.md](docs/bugs/RULINGS.md)) | open | silent | bugs |
|---|---|---|---|
| violates FFI-2@1 | 1 | 1 | 122 |
| violates FFI-5@1 | 1 | 0 | 114 |
| violates MOD-2@1 | 1 | 0 | 226 |
| spec gap (PASS) | 2 | 0 | 054, 077 |
| open OPEN-17 | 1 | 0 | 216 |
| outside the spec (performance) | 1 | 0 | 270 |
| outside the spec (tooling) | 3 | 0 | 049, 058, 064 |

| discovery rate: counting probe batch | date | effort | found | silent | loud | signal |
|---|---|---|---|---|---|---|
| workload: Phase 7 workload 9 lzpack (LZSS + PackBits codec, CRC container) and nine probe programs on its shapes (fixed-array hash chains through &mut, ring buffer, Huffman tree, trait codecs, fn values in structs, shift and cast edges, nested linear Result/Option, borrow and defer forms), plus a 400-case corrupt-stream fuzz against a Python reference | 2026-10-23 | 10 programs | 2 | 0 | 2 | found bugs |
| fuzz: fuzz farm mutation generator findings open at eb122a01 (13 crash signatures re-run on the OVH server, interpreter vs binary) plus about 100 hand probes after R-0492 steps 1-3 (shadowing, fn-pointer locals, generics, match through references, defer exits, nested modules, diagnostics) | 2026-10-23 | 113 programs | 8 | 5 | 3 | found bugs |
| workload: Phase 7 workload 10 stackvm (assembler, bytecode codec and load-time verifier, interpreter and disassembler over a 23-variant enum, a generic bounded stack, two traits and defer cleanup) with a differential gate against an independent Python implementation, and 25 probe programs on its shapes (imported enums with impls and trait bounds through each import path, import aliases, linear values in loops, Destroy impls, temporaries as receivers, borrow conflicts, diagnostics) | 2026-10-23 | 26 programs | 5 | 0 | 5 | found bugs |
| hunt: differential hunt (interpreter vs native binary) on main 9622f7e0: 3500 fuzz_differential.py cases (seeds 102501, 102502) and hand probes on module constants, enum payloads across modules, generics and traits across module boundaries, nested submodules, defer and loops under --interp | 2026-10-24 | 60 programs | 5 | 0 | 5 | found bugs |
| hunt: Lower borrows and address-taken locals: hand probes of every CExpr and CStmt constructor, and the differential fuzzer's borrow mode (fuzz_differential.py --mode borrow) | 2026-10-26 | 3000 programs | 7 | 3 | 4 | found bugs |
| hunt: the differential fuzzer's borrow mode with for loops (fuzz_differential.py --mode borrow) on a build with bugs 301 - 305 fixed: every remaining mismatch | 2026-10-27 | 1000 programs | 1 | 1 | 0 | found bugs |

| discovery rate by primary structural class | quiet counting batches in a row | holds |
|---|---|---|
| 1. Declaration identity by spelled name, not a resolved `DefId` | 4 | yes |
| 2. Binder identity by name, not a `LocalId` | 9 | yes |
| 3. Destruction dispatched by synthesized `<Type>_destroy` names | 10 | yes |
| 4. No reference-provenance model in the source checker | 10 | yes |
| 5. Borrow tracking by syntax instead of normalized places | 10 | yes |
| 6. Capability facts not carried across dispatch, specialization or package boundaries | 10 | yes |
| 7. Semantics implemented several times (interpreter, proof model, folder, backend disagree) | 6 | yes |
| 8. Storage, places and control flow decided ad hoc in `Lower` | 1 | no |
| 9. Evidence verdicts computed per surface and joined to their subject by name | 10 | yes |
| 10. Evidence identity computed from a partial subject | 10 | yes |
| 11. Absence read as a fact (fail-open defaults) | 10 | yes |
| 12. One fact kept in two places | 0 | no |
| 13. Documented rules with no enforcing site | 4 | yes |
| 14. Trusted `std` and FFI code with no checked contract | 10 | yes |
| 15. Ownership state not modeled along control-flow paths | 7 | yes |
| 16. Artifacts at paths no run owns | 10 | yes |
| 17. Pass state in association lists and per-query recomputation (superlinear compile time) | 2 | no |
| 18. Pipeline artifacts not bound to their payload | 10 | yes |
| 19. A mention read as a use (tool facts inferred from where a name is spelled) | 10 | yes |
| no class yet | 10 | yes |

Discovery signal (exit 2m): 17 of 20 classes have 3 consecutive eligible quiet counting batches (for a class: no silent bug of that class, at most one loud, effort at least 4 hours or 10 programs); the exit needs every class. 18 counting batches recorded. 71 attribution-only batches (reviews, follow-ons, CI, controls, development) found 117 further bugs.

<!-- END GENERATED: bug dashboard -->

## Implemented, not yet accepted

Work implemented on a branch (or landed without acceptance) but not yet accepted on main: bug fixes
keyed by bug number, and feature deliverables. Each row is one candidate: its branch, its exact
candidate SHA, the Phase 1 exits it contributes to or is blocked by, its [promotion-pipeline](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline)
results (each at that SHA, or not run), what its acceptance unlocks and its remaining step. The table is
generated from the candidate record [`docs/project/candidates.tsv`](docs/project/candidates.tsv) by
`scripts/gen/candidate_table.py --write`; whoever lands, re-points or validates a candidate updates its
row and regenerates in the same commit, and `check_docs_drift.sh` fails when the table differs from the
record. A row leaves when its work is accepted on main and, for bug fixes, the
[bug ledger](docs/bugs/README.md) records it fixed; `scripts/tests/check_roadmap_coverage.sh` fails if a
listed bug is already recorded fixed there, or if a row lacks a branch or a remaining step. A stage (c)
pass is the [one qualification verdict](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline) over every required job at the candidate
SHA, never the mutation aggregate alone.

<!-- BEGIN GENERATED: candidate record (scripts/gen/candidate_table.py; edit docs/project/candidates.tsv) -->
| candidate | bugs | deliverable | branch | candidate SHA | exits | stage (a) | stage (b) | stage (c) | unlocks | remaining acceptance step |
|---|---|---|---|---|---|---|---|---|---|---|
| rocksdb-slice1 | — | RocksDB slice 1 groundwork (R-0285) | `rocksdb-slice1` | `094e51bf3258` | blocked by 0b (incl. bug 102), 2i | not run | not run | not run | RocksDB slice 1 acceptance, then the four-adapter checkpoint | landed on main at 094e51bf but not accepted: exits 0b (bug 102) and 2i; G1 (declaring the native library, so it builds with no manual flags) closed by `[native]` ([NATIVE_LIBRARIES.md](docs/platform/NATIVE_LIBRARIES.md)); G3 closed (bug 103 fixed on main by `0c507fb9`, verified 2026-10-21) |
| v1-design-examples | — | V1 design examples (R-0511, row 3) | `v1-design-examples` | `03d2edec1d21` | none (isolated proof development) | not run | not run | not run | V1a's Core interfaces freeze | landed on main at 03d2edec but not accepted: review of the design questions, then stages (b) and (c) |
<!-- END GENERATED: candidate record -->

## Development throughput

**Decision (2026-10-22): the development cycle's feedback loop is a priority track, ordered ahead of
the next feature wave.** Its tasks are R-0539 – R-0547, with R-0513 for pre-push gate selection; their
bodies are under [Area: Development Throughput](docs/project/ROADMAP_TASKS.md#area-development-throughput). The track is charged to
track C of the [capacity allocation](#capacity-allocation). It does not preempt defect repairs, but
no new track D checkpoint starts after RocksDB slice 1 until R-0547's measurements are recorded and
landing trains (R-0539) on the landing CI tier (R-0540) are in use. The validation semantics do not
change. The [promotion pipeline](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline)'s stages, its exact-SHA binding and its
passed / failed / skipped / cancelled / incomplete vocabulary bind every item below. A faster path
never reports a check it did not run as passed.

The measurements behind the decision and the parts already in use on main are recorded in the
[roadmap history](docs/archive/ROADMAP_HISTORY.md#moved-at-the-2026-10-28-roadmap-compaction).

**Targets.**

| measure | target |
|---|---|
| Codex-clean to main | median under 2 h |
| landing CI, dispatch to conclusion, including queueing | median ≤ 40 min |
| local pre-push | median ≤ 10 min, and no gate failure first seen in CI over 20 consecutive landings |
| ledger conflicts on rebase | zero |
| review rounds per slice | median ≤ 5, and a slice past 5 is split |

**What remains.** Build and compile caches (R-0546: the slow-gate profile first, then the shared
compiled-artifact and Lean build caches), and the measurement the targets are read from (R-0547:
per-branch stage timestamps and the generated median time to land on this roadmap;
`scripts/land/report.sh` computes the medians, but none is recorded here yet). Until R-0547's
median exists, no item whose exit is a median (R-0539, R-0540, R-0513) can be shown to hold.

**Items, in dependency order.** This is the track's internal order under the queue's
[development-process exception](#the-current-execution-queue). It is not a second queue for compiler
or evidence work. Each item's owner holds its exit, and items with no dependency may start now. The
two in-progress branches are referenced here, not restated.

| order | item | owner | exit criterion | depends on |
|---|---|---|---|---|
| 1 | Measurement: per-branch stage timestamps (started, Codex-clean, CI started, landed) and median time to land shown on this roadmap | R-0547 | every branch landed after the recorder exists has all four timestamps, and the generated median covers them | — |
| 2 | Review discipline: review per slice; after the first full pass, review only the delta since the previous round; neutral prompts and piecewise diffs for ownership work | R-0544 | median ≤ 5 review rounds per slice over a month of recorded slices, and every slice past 5 rounds was split | — |
| 3 | Branch discipline: one bug or one rule per branch; rule-changing branches land first and small | R-0545 | over a month of landings, no branch carries more than one bug or rule without a recorded reason, and no rule change breaks another branch's fixture after that branch was ready | — |
| 4 | Load control: at most 2 heavy local runs per machine (a machine-wide semaphore); agents run only fast and impact-selected gates; full sets run on servers or in CI | R-0543 | a third concurrent heavy run waits for a slot (control), and a recorded week has no timeout failure caused by load | — |
| 5 | Generated ledgers: bug records are the single source, CHANGELOG entries are `changelog.d` fragments, drift gates (branch `ledgers`) | R-0542 | a scripted control lands one of two branches that each add a bug, a changelog entry and an example, rebases the other onto the result with zero textual conflicts, and finds both branches' records in the regenerated ledgers; a hand edit to a generated ledger fails its drift gate | — |
| 6 | CI tiers: a landing tier required for merge; the mutation campaign on schedule or dispatch, gating campaign qualification and not merges; the language-surface job split; candidate CI on lambdaclass/concrete (branch `ci-tiering`) | R-0540 | median landing-tier wall clock ≤ 40 min over 10 consecutive runs; stage (c) still requires the campaign, and `check_qualification_record.sh` refuses a landing-only run | — |
| 7 | Landing trains: merge every ready branch into one candidate, run one CI, bisect only on red; a landing script builds the train, regenerates generated files, runs the fast gates, pushes `ci/`, dispatches, watches and lands on both remotes | R-0539 | three consecutive trains of at least two branches land through the script, and median Codex-clean → main is under 2 h over them | 5, 6 |
| 8 | Self-hosted or larger runners on the maintainers' servers, never for `pull_request` (the repository is public); heavy local gate runs execute remotely on the servers | R-0541 | a workflow check fails when a self-hosted job can run for `pull_request` (with its negative control); a full local gate set runs on a server and records its result against the SHA | 4, 6 |
| 9 | Impact-based gate selection for pre-push: map source files and fixtures to the gates that exercise them, so a change runs every gate it can break | R-0513 | interim checkpoint before exit 2f's five-minute budget: pre-push median ≤ 10 min, and over 20 consecutive landings no gate failure is first seen in CI | 1 |
| 10 | Build and compile caches: profile the 20 slowest gates first; then a shared compiled-artifact cache keyed by compiler identity and source hash, and a shared Lean build cache across worktrees | R-0546 | the profile is recorded at a SHA; with the compiler unchanged, a second run of those gates takes at most half the first run's time; changing the compiler identity or a source invalidates the cache (control) | 1 |

## Phases and releases

Seven phases each group rows of the [execution queue](#the-current-execution-queue), have an exit, and
produce a release scope (see [Releases](#releases)). After Phase 1, Phase 3 runs alongside the proof
track, which progresses from Phase 2 into Phase 4; the shared-front-end work overlaps proof development.
Effort is divided between tracks by the [capacity allocation](#capacity-allocation), in which the
verified compiler holds the majority; new soundness defects take priority over every phase and are
disposed of by the [defect disposition rule](#the-current-execution-queue).

| phase | goal | queue rows | exit | release |
|---|---|---|---|---|
| **Phase 1 — Stabilize** | Land the 2026-10-18 soundness repairs, close R-0482, put the immediate protections in place | 0 – 2 | every [Phase 1 exit](#phase-1-exits) (0a – 2p): repairs gated; R-0482 qualified; one `Verdict` authority; language rules answered by the project owner and enforced | 0.1 Experimental systems preview |
| **Phase 2 — Verified foundation** | First proved end-to-end slice (V1a, V1b, V1c) and the shared compiler structure for the whole language: identities, the authoritative Core checker, one `Layout`, Place MIR | 3 – 4 | V1a and V1b theorems and V1c's source correspondence; Core checker authoritative; one `Layout` and Place MIR in the existing profile | 0.2 Verified preview |
| **Phase 3 — Usable language** | Interop, application workloads and the usability track | 5, 7, 8 | four-adapter interop checkpoint; usability checkpoints | none of its own; feeds every release |
| **Phase 4 — Verified growth** | Cumulative slices V2 – V7 and claim migration to Core | 6 | each slice's theorem; V7 covers the feature inventory | 0.3 – 0.8 Verified-subset milestones |
| **Phase 5 — Evidence as a product** | Compiler provenance, canonical task, independent consumer, receipt v1 | 9 – 11 | receipt v1 frozen | 0.9 Verified-component alpha (with Phases 4 and 6) |
| **Phase 6 — Richer verification** | Typed contracts, `old`/frames, ninth table, totality and spec library | 12 – 15 | typed contracts with exact identity; `old`/frames; ninth table converted; checked totality | 0.9 alpha increments |
| **Phase 7 — Release and ecosystem** | External-user workflow, three flagships, package evidence and policy | 16 – 17 | non-author replay and upgrade; flagships graduate; package evidence composes | 0.10 Project beta; 1.0 Stable; 1.x |

| row | tasks | goal |
|---|---|---|
| 0 | R-0482 | Land the active soundness repairs (bugs 079 – 101) and qualify R-0482 (exits 0a – 0c) |
| 1 | R-0482 | Make R-0482's cross-package (P1) controls permanent (exit 1a) |
| 2 | R-0491, R-0493, R-0494, R-0498, R-0500, R-0506, R-0513, R-0550, R-0553, R-0554 | Immediate protections (exits 2a – 2p): one verdict authority, prevention harness, trustworthy gates, axiom policy, one `IntArith`, language rules, demonstrated enforcement, the owner's language decisions, compile-time scaling, compiler totality, then the repository cleanup |
| 3 | R-0511, R-0517 | First cumulative verified releases: design proofs, V1a, V1b and V1c, each accepted when its [prerequisites](#feature-prerequisites) hold |
| 4 | R-0492, R-0497, R-0499, R-0514, R-0502, R-0501, R-0355 | Shared compiler structure (verified-pipeline Part 1): identities, the authoritative Core checker, one `Layout` and Place MIR for the whole language, measured against a development-speed baseline |
| 5 | R-0285, R-0283, R-0286, R-0287 | Bounded C/Rust/Zig interop: RocksDB slice 1 (work proceeds now; accepted when its [prerequisites](#feature-prerequisites) hold), then the four-adapter checkpoint |
| 6 | R-0491–R-0510, R-0512 | Slices V2 – V7 and evidence migration to generated Core statements |
| 7 | R-0490 | Continuous application feedback toward the ethlambda port |
| 8 | R-0515 | Usability track alongside the proofs: onboarding, pilots, diagnostics, editor, performance |
| 9 | R-0521 | Compiler provenance and one canonical `VerificationTask` |
| 10 | R-0353 | Independent evidence consumer and schema conformance |
| 11 | R-0338, R-0353, R-0521 | Receipt v1 freeze |
| 12 | R-0473, R-0474 | Typed contracts and exact contract identity |
| 13 | R-0524 | Two-state mutation contracts (`old`, frames, ghosts) |
| 14 | R-0525 | Ninth-table conversion of the fail-closed `proofFnsExt` links |
| 15 | R-0526, R-0270, R-0060 | Totality and the specification library |
| 16 | R-0486, R-0119 | External-user workflow and the three public flagships |
| 17 | R-0440 | Package evidence and typed policy |

The detailed exits are in [the current execution queue](#the-current-execution-queue); the summary
above names the same task IDs per row (checked by `scripts/tests/check_roadmap_coverage.sh`).

### Phase details

- **Phase 1 — Stabilize** (rows 0 – 2). **Goal:** Land the 2026-10-18 soundness repairs (080 – 101), close the R-0482 identity migration, and put the immediate protections in place: one verdict authority, the prevention harness, demonstrated enforcement, the language decisions (the project owner's answers, exit 2n), one `IntArith`, the axiom policy and trustworthy gates. **Exit:** every [Phase 1 exit](#phase-1-exits): each repair's reproducer is a permanent gate; R-0482 qualified and its P1 controls permanent; one `Verdict` authority and the written language rules enforced. Feature work starts now and is accepted when its [prerequisites](#feature-prerequisites) hold. **Release:** **0.1 Experimental systems preview**, with the usability onboarding path and first pilot.
- **Phase 2 — Verified foundation** (rows 3 – 4). **Goal:** The first proved end-to-end slice (V1a Core → BIR, V1b native AArch64 through the Lean Compiler Backend, V1c restricted source → checked Core correspondence, with design proofs for ownership, borrows and memory first), and verified-pipeline Part 1's structure for the whole language: identities, the authoritative checker on Core, one `Layout` and Place MIR in the existing profile. **Exit:** V1a and V1b theorems with explicit boundaries and V1c's correspondence theorem; the Core checker authoritative for every program; the existing profile's layout from one `Layout` and its lowering jobs in Place MIR passes. **Release:** **0.2 Verified preview** (with Phase 3's tooling checkpoints); the candidate states where the theorem begins.
- **Phase 3 — Usable language** (rows 5, 7, 8). **Goal:** Bounded C/Rust/Zig interop, real application workloads leading to the ethlambda port, and the usability track: onboarding, external pilots, diagnostics, editor and debugging, published performance. **Exit:** the four-adapter interop checkpoint; continuing workloads and pilots; each usability checkpoint's exit. **Release:** no release of its own: it runs alongside and feeds every release from 0.1 on (onboarding, pilots, tooling, the minimal editor path for beta).
- **Phase 4 — Verified growth** (row 6). **Goal:** Cumulative slices V2 – V7 (aggregates and references, heap and linear resources, generics and capabilities, raw pointers and FFI, the rest of the language) each with its theorem, and eligible claims migrated onto compiler-generated Core statements independently of backend coverage, with explicit deprecation for retired claims and historical replay preserved. **Exit:** each slice's cumulative theorem; V7 covers the maintained feature inventory. **Release:** **0.3 – 0.8 Verified-subset milestones** (a slice may span several releases); V7 is required for 1.0.
- **Phase 5 — Evidence as a product** (rows 9 – 11). **Goal:** Compiler provenance and one canonical verification task, the independent evidence consumer, and the receipt v1 freeze. **Exit:** receipt v1 frozen after producer and independent consumer agree on hostile fixtures. **Release:** **0.9 Verified-component alpha**, together with Phases 4 and 6 for the admitted component.
- **Phase 6 — Richer verification** (rows 12 – 15). **Goal:** Typed contracts and exact contract identity, two-state (`old`, frames) contracts, the ninth-table conversion, and totality with a specification library. **Exit:** typed contracts with exact identity, `old`/frames, the ninth table converted, checked totality and the spec library. **Release:** **0.9 Verified-component alpha** (its typed-contract, two-state and required totality increments).
- **Phase 7 — Release and ecosystem** (rows 16 – 17). **Goal:** External-user workflow and three graduated flagships, then package-level evidence and typed policy. **Exit:** a non-author replays and upgrades with exact diffs; three flagships graduate; package evidence composes under typed policy. **Release:** **0.10 Project beta**, then **1.0 Stable supported subset** (V7 with source correspondence, Phases 5 – 7's acceptance, release-bar compatibility and post-release readiness) and **1.x** feature releases.

The [verified-compiler plan](docs/compiler/VERIFIED_PIPELINE.md) holds Phases 2 and 4 in detail; the
[task register](docs/project/ROADMAP_TASKS.md) holds every row's task and its exit.

### Estimated plan to a proven compiler

Estimates, recorded 2026-10-25 with the project owner's approval; they are revised as slices land and
are not commitments or release dates. Each duration counts from **D**, the date the owner answers
OD1 – OD3 and OD10 – OD11 (references, `&mut` arguments, pattern binders, arithmetic and casts; see
[owner decisions](#owner-decisions)), because V1a and the slices after it are checked under those
rules. Until D, proof development proceeds within the rules as they are on main and does not choose
those answers. The milestones follow the slices of the
[verified-compiler plan](docs/compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices), in
this order:

| milestone | what it proves | estimate |
|---|---|---|
| V1a | checked Core → BIR refinement for the minimal scalar subset (queue row 3) | about 3 months after D |
| V2, V3 | aggregates, bounds checks and downward references; then owned heap, linear resources, destruction and `defer` (queue row 6) | between V1a and V4 |
| V4 | the admitted reference, reborrow and resource interactions complete: a proved checked Core → BIR refinement for that language, emitted as LLVM IR by the BIR → LLVM printer, which stays trusted and unverified, with LLVM itself trusted (the LLVM profile) | V1a through V4 about 9 – 18 months after D |
| V5, V6, native backend | generics, traits and capability polymorphism (V5); raw pointers, FFI, callbacks and separate compilation (V6); the native AArch64 path through the Lean Compiler Backend (V1b onward, R-0517) with per-compilation certificates (R-0552); then V7 | the whole, backend included, a further 1 – 2 years |

## The current execution queue

This is the only current queue and the authoritative scheduling section; execution starts here. It
is intentionally not padded to a fixed number: rows exist only for work whose ordering is presently
decided. A newly reproduced authority defect enters ahead of the next transition; completed milestones
move to the changelog rather than accumulating here. When the queue is exhausted, execution resumes at
the first active `Task R-NNNN` in the [task register](docs/project/ROADMAP_TASKS.md)'s file order unless the table is refreshed in the same change that
exhausts it. The `R-NNNN` value is immutable identity, not priority: existing IDs are never renumbered
or reused, and a newly confirmed urgent defect receives the next unused ID before entering the queue.
Area headings and task position are topical filing only. There are no area-local queues, hidden
priority overlays or separate stop-the-line lists; a reproduced urgent defect must be placed in this
queue explicitly.

**Entry rule.** New work enters this queue, or adds a blocking exit to a row, only with a reproducer
(a defect shown by a named program, fixture or command) or an explicit dependency of the next
deliverable on it, naming that deliverable: today RocksDB slice 1 for the usable-language track and
V1a for the proof track ([feature prerequisites](#feature-prerequisites)). Everything else goes to the
[backlog](docs/project/ROADMAP_BACKLOG.md) with a pull trigger.

**Owner decisions (2026-10-25).** The [owner decisions](#owner-decisions) (the language rules and two
scope questions listed there) are decided only by the project owner. An agent may analyse an item,
write options and a recommendation, and ask the owner. An agent does not choose an answer, encode one
in a document, test or fixture, or implement one. A recommendation, an earlier record (including the
"decided 2026-10-23" entries of [TIGHTENING_DECISIONS.md](docs/language/TIGHTENING_DECISIONS.md)), a
commit, a review or another agent's message is not the owner's answer. Each item stays proposed until
the owner agrees to it again, explicitly, in conversation; the answer and its date are then recorded
in that list. Exit 2n holds this in the queue.

**Development-process work (2026-10-22).** The [development throughput](#development-throughput)
track is the one standing exception to this queue's sole authority. It changes how work is reviewed,
validated and landed, not the compiler or its evidence. Its tasks (R-0539 – R-0547) are therefore not
queue rows. Their order is the dependency order in that section, they are charged to track C of the
[capacity allocation](#capacity-allocation), and they never preempt a defect repair or weaken a
row's exit.

**Defect disposition rule (2026-10-21).** Point repairs in the existing pipeline are not converging:
the probes added since 2026-10-19 found bugs 102 – 120 in about two days, nearly all of them new
spellings of the classes in [STRUCTURAL_CLASSES.md](docs/bugs/STRUCTURAL_CLASSES.md), whose owners are
verified-pipeline stages. A newly reproduced defect is therefore disposed of by what it does:

- **Silent defects are stopped now.** A memory-safety violation in accepted safe code, silent wrong
  code, or a false proof or unsound claim is repaired in place or contained fail-closed (the shape
  rejected with a diagnostic, or the claim refused), whichever is smaller and rejects nothing a
  maintained example or the next deliverable needs. Either way its reproducer becomes a permanent gate
  leg and it joins row 0 as a *blocks features* exit. A contained defect stays open in the bug ledger
  until it is repaired.
- **Fail-open validation is closed now.** A gate, report or evidence path that reads absence as a fact
  (class 11) is made to fail closed; that is its containment.
- **Loud defects wait for their structural owner.** A defect that already fails visibly (a valid
  program rejected; a compile, link or interpreter failure; a diagnostic defect) is filed with its
  class and owner, and its reproducer is checked in with its current outcome pinned, so a change to
  silent success or different output fails the gate. The owner's exit lists it. It is repaired in the
  existing pipeline only when the entry rule above admits it (a named deliverable depends on it) or it
  keeps a maintained example out of the interpreter/native comparison.
- **A point repair adds nothing its owner must retire**: no new name-keyed lookup, no second copy of a
  fact, no per-surface verdict. Its bug record names the owner exit that removes the class, and that
  exit lists the bug.

Repair effort for an existing Phase 1 exit is charged to that exit's track in the
[capacity allocation](#capacity-allocation). For a new defect, its row-0 exit records its primary
structural class, its accountable structural owner and its capacity track when it is added; the
capacity table is authoritative. Exits assigned before 2026-10-21 keep their
obligations, including exit 2c's repairs of loud failures.

**Example rule.** Every example has a gate: besides its entry in `scripts/tests/example_manifest.txt`, a
new example has a gate leg that asserts its behaviour (expected output and exit status, an expected
rejection with its diagnostic code, or its claims' statuses); a compile-only manifest entry does not
count, and there are no ungated demos. An expected output counts only with an independent check:
every `@run` rule declares its oracle — `interp` (the interpreter agrees, enforced on every run),
`spec:<pointer>` (derived by hand from the program's specification or by a reference tool, the
derivation recorded at the pointer, [GOLDEN_ORACLES.md](docs/project/GOLDEN_ORACLES.md)) or
`unverified:<reason>` (a tracked gap the gate counts and lists); `check_example_outputs.sh` fails on an
undeclared oracle. A golden copied from the binary's own output checks nothing: bug 121's `conlog`
golden blessed a miscompiled answer. New examples are workload-driven and targeted: each serves a bug
class (R-0491's [adversarial example suite](docs/project/ROADMAP_TASKS.md#task-r-0491)), an upcoming deliverable (the
[RocksDB slice 1 companions](docs/project/ROADMAP_TASKS.md#task-r-0285), [V1a's target programs](docs/project/ROADMAP_TASKS.md#task-r-0511)) or an application
workload (R-0490). All example work follows R-0135's lifecycle and owner policy: user-facing examples
record their lifecycle and owner, while pure regression and rejection fixtures live in the owning
test or bug corpus and are invoked by the example's gate.

| order | work | exit before advancing |
|---|---|---|
| 0 | **Active soundness repairs and R-0482 qualification** | close bug 079's subject/model/spec binding at status, issuance and receipt consumption; land the bug 080 cast model and the 081 – 095 repairs (evidence, model/runtime, name-resolution and checker memory/capability-safety defects from the 2026-10-18 hunts), each with its reproducer as a permanent gate leg; repair campaign isolation and retain a qualified result naming its exact commit before closing the migration. Closes through exits 0a, 0b and 0c ([Phase 1 exits](#phase-1-exits)) |
| 1 | **R-0482 P1 cross-package closures** | promote the manifestful std/dependency controls into permanent gates, retain D3's manifestless distinction and negative controls, and publish only after the subject-binding repair. Closes through exit 1a |
| 2 | **Immediate protections and semantic decisions (R-0491, R-0493, R-0494, R-0498, R-0500, R-0506, R-0513, R-0550, R-0553, R-0554)** | closes through the individually closable exits listed in [Phase 1 exits](#phase-1-exits), each with its owner, acceptance test and class: one `Verdict` authority renders every evidence surface (2a); permanent reproducer controls for 080 – 101 (2b) and the prevention harness (2c); trustworthy gate execution — checked inventories instead of incidental count pins (2d), minimum completed comparisons (2e), a fast push tier separate from full qualification (2f); the axiom-policy gate on today's evidence (2g); one `IntArith` for the existing pipeline (2h); the rules the next slice needs written down and enforced with versioned migration diagnostics — strictly second-class references and no block `let` shadowing (2i); typed literals, literal range, statement vs value position, sequential assignment within a loop iteration, evaluation order and short-circuiting (2j); explicit match binders and cast semantics per width (2k); demonstrated enforcement (2l); the bug-discovery rate at about zero in every structural class across its three latest probe batches (2m); the project owner's answers to the [owner decisions](#owner-decisions) (2n); a compile-time scaling gate (2o); compiler totality, starting with a ratchet gate that lets the count of non-total compiler declarations only go down (2p), ahead of the verified-pipeline pass proofs of rows 3, 4 and 6. After it comes the repository cleanup programme (R-0554), queued at the project owner's request as an explicit exception to the entry rule and charged to track C; it has no exit of its own, and its sub-items continue alongside and are recorded in the changelog as they land. Known authority and safety defects are repaired under row 0. An exit classed *blocks features* holds back the acceptance of only the deliverables whose [prerequisites](#feature-prerequisites) name it; the others continue alongside feature work. Broader prevention coverage continues throughout and does not block isolated proof development |
| 3 | **First cumulative verified releases (R-0511, with R-0517)** | reviewed machine-checked design examples for move/destroy, borrows, reborrowing, cleanup and typed-value memory representation before Core's interfaces freeze; V1a (checked Core → BIR refinement for the minimal scalar subset) and V1b (native AArch64 through the Lean Compiler Backend, R-0517, with final-image correspondence; LCB's Stage 1 exit is necessary but insufficient — V1b needs a pinned certified LCB domain covering every reachable V1 operation and behaviour, including checked-arithmetic trap paths, direct calls and `print_int`, plus Concrete's proved adapter and semantic bridges; a smaller leaf-function intersection ships only as a separately named intermediate theorem; R-0517's explicitly selected fallback does not complete this row) with explicit theorem boundaries; checker soundness, capability containment, resource and target-memory-validity obligations and one transported claim for the admitted subset; V1c, restricted source → checked Core correspondence for V1's surface (syntax, resolution and elaboration judgments specified independently of their implementations, then proved or certificate-checked; constructs outside the subset rejected explicitly), before any V1 result is called source-to-native; retained artifacts with minimal task identity and independent replay. The design examples and isolated proof development start now; V1a, V1b and V1c are accepted only when their prerequisite exits hold (prerequisite exits: 0a, 0b, 2a, 2g, 2h, 2i, 2j, 2n; the programs each must compile and the independently checked artifact are under [feature prerequisites](#feature-prerequisites)). See [the plan](docs/compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices) |
| 4 | **Shared compiler structure: identities, the authoritative checker on Core, one `Layout` and Place MIR for the whole language (R-0492, R-0497, R-0499, R-0514, R-0502, R-0501, with R-0355's baseline)** | verified-pipeline Part 1, in the primary track's migration slot ([capacity allocation](#capacity-allocation)), alongside row 3: before the first whole-language change of R-0492, R-0499/R-0514, R-0502 or R-0501 lands on main, R-0355's [development-speed baseline](docs/project/ROADMAP_TASKS.md#task-r-0355) is recorded, so each structural change's cost to build time, rebuild time, compiler memory and test turnaround is visible; the parts V1a's subset exercises proceed with V1a, and the whole-language rollout starts once row 0's in-flight candidates (batch 2, type-identity, generic-destroy) are accepted on main, which edit the same checker code; it implements the rules as exits 2i – 2k state them and does not wait for row 2's other exits, except that it never chooses, encodes, implements or extends a rule that an unanswered [owner decision](#owner-decisions) covers (exit 2n): it keeps such a rule as it is on main; the front end both profiles share uses `LocalId`/`DefId` from Resolve on, including the source checker's environments, with immutable payload-bound artifacts; one checker over Core implements every ownership, borrow, reference-escape, capability and `Unsafe` rule for the whole language, emits payload-bound `CheckedCore` consumed by checked specialization in both profiles; specialization produces payload-bound `CheckedMonoCore`, the only input to lowering; the source-tree checker holds no such rule; the 087 borrow-block case and 090 – 094 are rejected by it; capabilities are re-checked after Mono. Then, in the same slot: one `Layout` authority (R-0502) supplies every size, alignment, offset and enum representation the existing lowering and LLVM emission use, with a gate comparing it with the emitted types (class 12 of the [structural classes](docs/bugs/STRUCTURAL_CLASSES.md)); and `Lower`'s sequencing, match compilation, cleanup/`defer` and check expansion move into Place MIR passes (R-0501) that consume `CheckedMonoCore`, as shadow paths with disagreement gates against the existing lowering (class 8). Their semantics and proofs still grow per slice (rows 3 and 6), and the old `Lower` retires only under the plan's retirement conditions. The slices prove this checker's rules as they reach them (V1 – V7, beginning with scalar typing and basic capabilities in V1, downward borrows in V2, and extending through resources, polymorphism and unsafe/FFI interactions); this row makes it authoritative for every user now |
| 5 | **Bounded interop track (R-0285, with R-0283/R-0286/R-0287)** | the one bounded executable workload beside the verified compiler, in the single feature slot ([capacity allocation](#capacity-allocation)): first deliver **RocksDB slice 1**, the first executable feature deliverable: implementation and experiments proceed now (its groundwork on main already found bugs 102 and 103), and it is accepted, and its safety advertised, only when its prerequisite exits hold (prerequisite exits: 0b, 2i, 2n; checklist and acceptance under [feature prerequisites](#feature-prerequisites)); then deliver the four-adapter checkpoint, after R-0515's practical debugging checkpoint and with the clean-machine upgrade-and-roll-back exercise (RocksDB C, Rust crypto/networking, an upstream Zig library; no application bridge/build scripts, generated-file edits or manual flags) on the supported existing profile alongside verified-pipeline work; foreign implementations remain named trust dependencies; interop neither waits for nor blocks the first native theorem |
| 6 | **Cumulative slice growth and evidence migration (R-0512, R-0491–R-0510 by slice)** | extend the proved language and frontend correspondence slice by slice (V2 aggregates and downward references, V3 heap and linear resources, V4 full reference/resource interactions, V5 generics/traits/capabilities, V6 raw pointers/FFI/separate compilation, V7 the maintained feature inventory); migrate eligible claims to generated Core statements independently of backend coverage; ship supported releases and preserve historical replay throughout |
| 7 | **Continuous application feedback and eventual ethlambda completion (R-0490)** | begin bounded client-owned workloads after the relevant interop support, using the existing profile where necessary; until V1b's composed native exit holds these are bounded component workloads (a Concrete component called from the Rust client through R-0286) inside the feature slot, starting after R-0515's practical debugging checkpoint, and the bulk port starts after V1b, or earlier only by a dated decision recorded when V1b is blocked on LCB's external milestones; measured language, library, compile-time and runtime feedback informs slice priorities; full mixed-client node completion keeps its existing acceptance criteria and is not gated on V7 |
| 8 | **Usability track (R-0515), alongside the proof track** | bounded checkpoints within the feature track of the [capacity allocation](#capacity-allocation), advancing the tasks that already own each concern: a first-project onboarding path before the first pilot; quarterly external pilots that record author assistance and require participant-validated fixes; a diagnostics quality bar gated on failing inputs and valid repairs; editor (types, diagnostics, navigation, proof/trust/capability status) and debugging checkpoints at V1a/V1b; published, regression-gated performance comparisons; the stability policy, `std` coverage inventory and named reviewers before public release. Pilots are a continuing obligation, not a gate for rows 9 – 17 |
| 9 | **Compiler provenance and canonical `VerificationTask` (R-0521)** | separate source-build provenance from executable identity; establish one production task constructor binding the exact subject, proposition, contract state, rule set, dependency material and environment |
| 10 | **R-0353 independent consumer and schema conformance** | publish experimental task/receipt encodings plus hostile conformance fixtures; independently parse, account and replay without importing the compiler/report implementation |
| 11 | **Receipt v1 freeze (R-0338, with R-0353/R-0521)** | freeze only after producer and independent consumer agree on canonical fixtures and Slice 8's permanent attacks cover the finalized schema; incompatible changes require a version bump |
| 12 | **R-0473/R-0474 typed contracts and exact contract identity** | replace conservative implementation-bound contract witnessing with typed contracts, imported hypotheses, exact `ContractIdentity`, and contract-preserving/body-changing controls |
| 13 | **Two-state mutation contracts (R-0524)** | add a narrow record/array state model, `old`, `modifies`/frames, then ghost locals and parameters; unsupported heap/reborrow shapes refuse explicitly |
| 14 | **Ninth-table conversion (R-0525)** | use the narrow mutable-state model to extract and replay the three currently fail-closed `proofFnsExt` links; until then they remain unable to provide authority |
| 15 | **Totality and specification library (R-0526, R-0270, R-0060)** | add checked `#[decreases]`/total functions, then canonical `int`, list, map, set and bitvector theories before richer or relational logic |
| 16 | **External-user workflow and productization (R-0486, R-0119)** | a non-author receives evidence, replays it, upgrades a dependency, and sees exact machine/human diffs; graduate exactly three deep public flagships—HMAC-SHA256, a secure update-bundle verifier that absorbs bounded parsing and file-integrity work, and a small protocol state machine—plus IDE/CI lenses and recurring ergonomics audits. Compiler regressions, hostile inputs and benchmarks remain fixtures, adversarial cases and workloads rather than additional public flagships |
| 17 | **R-0440/package evidence and typed policy** | compose partial dependency evidence, revocation/advisories, trust and release requirements without turning receipt validity into policy acceptance |

### Phase 1 exits

Rows 0 – 2 close through these exits, each closable on its own. The **class** says what an exit holds
back:

- **blocks features**: the exit repairs or permanently guards a known false-proof, unsound-claim or
  memory-safety defect, or a feature deliverable names it as a dependency. It holds back the acceptance of exactly the
  deliverables whose [prerequisites](#feature-prerequisites) list it.
- **continues alongside**: consolidation or prevention without a reproduced defect or a named
  dependency. It proceeds next to feature work, with a finite acceptance test, and still closes Phase 1.

Consolidation blocks only when it fixes a reproduced defect or a named dependency. A newly reproduced
authority or safety defect joins row 0 as a new *blocks features* exit. Phase 1, and the experimental
systems preview's advertised scope, still require every exit. `check_roadmap_coverage.sh` checks that
every exit has an owner from its row, an acceptance test and a class, and that the prerequisite lists
agree with the classes.

| exit | owner | closes when | acceptance test | class | why this class |
|---|---|---|---|---|---|
| 0a | R-0482 | bug 079's subject/model/spec binding is closed at status, issuance and receipt consumption, and the evidence and model repairs land: the bug 080 cast model, 081, 083 – 086, 089, 101 and the 065 stack-depth policy verdict | each bug's reproducer is a permanent gate leg (the `check_model_diff.sh` legs for 080, 084 – 086 and 101; the 079, 081, 083 and 089 reproducers in the evidence gates; 065's in the stack-depth policy gates), and the [bug ledger](docs/bugs/README.md) records each fixed on main | blocks features | known false proofs and unsound claims |
| 0b | R-0482 | the wrong-code and memory/capability-safety repairs land: <!-- BEGIN GENERATED: exit 0b bugs (scripts/gen/bug_ledger.py; edit the records' exit lines) -->052, 082, 087, 088, 090 – 094, 096 – 100, 102 – 107, 109, 111, 113, 115, 121 – 123, 126 – 129, 131 – 133, 135 – 137, 140, 142 – 152, 165, 166, 176, 177, 180 – 183, 185, 186, 188 – 190, 192 – 198, 200, 204, 205, 208, 210, 215, 218, 220, 221, 238, 239, 247, 251, 253, 271, 272, 285, 287, 301, 302, 305, 306<!-- END GENERATED: exit 0b bugs -->, 095 (no record yet). The list is generated from the bug records' `exit: 0b` lines: every silent bug the [discovery record](docs/bugs/discovery.tsv) lists, except the false proofs and unsound claims exit 0a names, carries one, as do the memory-safety repairs it does not mark silent or that predate it (052, 093 – 095, 099, 100, 142, 145 – 148), and a silent bug filed later gets one in its record when it is filed. Among them is the silent defect found while writing the [2026-10-21 semantics decisions](docs/project/ROADMAP_DECISIONS.md#decisions) into the docs: the binary evaluating a dereference assignment's destination before its right-hand side while the interpreter does the reverse ([evaluation order](docs/language/STATEMENT_EXPRESSION_MODEL.md#evaluation-order-and-assignment); filed as bug 183). The other one recorded then, an arm binder through a borrowed scrutinee whose root is moved inside the arm ([MEMORY_SEMANTICS §15.7](docs/language/MEMORY_SEMANTICS.md) G17), is refused on main (E0204, re-probed 2026-10-23) and pinned by rejection fixtures; bugs 181 and 182 close under the [2026-10-23 tightening decisions](docs/language/TIGHTENING_DECISIONS.md#4-decisions-of-2026-10-23) D3 and D2, and bug 166 under D4's refusal; whether those rules stay is the project owner's answer to [owner decisions](#owner-decisions) OD1, OD3 and OD4 (exit 2n), and a different answer must still leave each of those bugs repaired | each bug's reproducer is a permanent gate leg, and the bug ledger records each fixed on main | blocks features | use-after-free, double free, skipped destruction and capability escape in safe code (052, 087, 090 – 093, 099 – 100, 091, and 104 and 131: a reference assigned from an inner block outlives its source, to a local or to a reference parameter), a safe user function named like a C symbol replacing it for the whole program (129: `fn malloc` becomes the allocator `alloc` calls), a comparison operator consuming its linear operands (140: `Heap == Heap` compared addresses and leaked both cells), a `return` inside a call argument or literal field leaking the values already moved into the construct (142), a deferred call reading a local freed after its `defer` was registered (143: `defer pb(&h); free(h);`), heap-shell interiors exempt from linearity (144: a field read twice double-frees, `free(h)` drops a linear interior), the raw `vec_set`/`vec_free` intrinsics losing non-Copy elements (145), `*h` never freeing its cell (146), a move inside an erased `assert`/`ghost let` counted as a release that never runs (147), a move inside the right operand of `&&`/`||` counted although it may not run (148), loop conditions, returns in nested blocks and labelled exits not checked for owned values (149), double free through `vec_get` taking a non-Copy element by value (115), silent wrong code (082, 088, 096 – 098, 102, 107, 111, and 123: a borrow of a module constant reads an uninitialized slot; 127: struct-literal initializers run in declaration order natively and in the ratified source order in the interpreter; 128 (fixed 2026-10-21): the interpreter, the differential reference, modelled an integer-to-float cast as the identity; 150 (fixed 2026-10-21): SSA cleanup folded `x != x` to false for a float `x`, so a NaN passed every such check; 152 (fixed 2026-10-21): `string_trim("")` returned a byte past its source; 151 (contained by E0299): `a.b_f` and `a_b.f` flattened to one name and the binary's `a_b::f()` ran `a.b_f`; 133: a write through `&mut (*p).f` lands in a copy of `*p`; 136: functional update `S { .., ..base }` evaluates its base once per omitted field; 137: enum-variant literal fields run in declaration order in both consumers; 176: a loop body's shadowing `let` outlived the loop in the checker, so a `u8` binding held 300 natively and in the interpreter; 105: a reference-valued `if`/`match` reached through a generic instantiated at a reference type, which the checker containment did not cover, segfaulted natively), a silent semantic change by a tool (135: `concrete fmt` rewrote float literals to six decimals, so `0.0000000001` became `0.000000`), missing extern effect declarations accepted in nested submodules (113; calls still charged `Unsafe`), a foreign destructor silently replaced by an empty body (103, a leak of every C resource released through a `*_destroy` function), and LLVM type layouts that disagree with the compiler's own, so aggregate copies move fields (121: conlog freed a corrupted String pointer; 122: the canonical builtin `Option`/`Result` union, including a stack overrun); the defects found on 2026-10-21 are a read through an arm binder after its source moved, and an interpreter/native disagreement on dereference-assignment order; and heap-slot corruption through a `&mut Heap<T>` argument (126: the callee treats the caller's heap-pointer slot as the struct, so a field write overwrites the slot and `free` receives a corrupted pointer) |
| 0c | R-0482 | campaign isolation is repaired and a qualified campaign result naming its exact commit is retained before the identity migration closes | a [stage (c)](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline) qualification record with `verdict=qualified` at that commit: every required job succeeded there, the mutation aggregate's `qualified=1` among them | continues alongside | qualification of the identity migration; no deliverable depends on it |
| 1a | R-0482 | the manifestful std/dependency controls are permanent gates, D3's manifestless distinction and its negative controls are retained, and the result is published only after 0a's subject-binding repair | the P1 controls run as permanent legs of the full gate set, and the manifestless negative controls fail when the D3 distinction is collapsed | continues alongside | cross-package claims are not a dependency of RocksDB slice 1 or V1 |
| 2a | R-0494 | one `Verdict` authority renders every evidence surface (contracts, vcs, obligation-ledger, audit, check-proofs text and JSON, proof-status, issuance) | R-0494's exit: status spellings only in the renderer table; bug 089's reproducers (`rt_ens_only`, `rt_ens`, `rt_ens_bogus`, `rt_ens_noens`) read not-proved on every surface; a cross-surface gate (new) pins the expected positive and negative judgment for every claim and clause | blocks features | the proof surface is feature-frozen until it lands ([verified-compiler decision](docs/project/ROADMAP_DECISIONS.md#decisions)), so V1's published claim needs it |
| 2b | R-0491 | permanent reproducer controls for 080 – 101: each bug has a checked-in record, reproducer, expected outcome and pre-fix/fixed SHA pair | each assigned control fails at its pre-fix SHA and passes at its fix; `check_bug_filing_complete.sh` and `audit_bug_corpus.sh` fail on a missing inventory entry | continues alongside | the reproducers already block as gate legs under 0a and 0b; this exit proves each one load-bearing |
| 2c | R-0491 | the prevention harness (S1): the extracted model, then Core's semantics, as a third side of the differential fuzzer over every shape that broke, outcomes classified and never skipped, and model-diff for zero-argument, `bool` and aggregate signatures plus a binary leg with a pinned claimed-function inventory; and the adversarial example suite for today's bug classes; and closing the loud failures that keep valid examples out of the interpreter/native comparison (108 – 110, 112, 114, 116, 124, 130, 134, and interpreter gaps 117 – 120, all four fixed by 2026-10-21; the interpreter-coverage pass also closed D4 (`return` in value position) and D13 (output before a trap)); permanent differential legs for struct- and enum-literal field order, functional-update evaluation, assignment order and integer → float conversion ([2026-10-21 semantics decisions](docs/project/ROADMAP_DECISIONS.md#decisions)); and every example golden independently checked: each `@run` rule declares its oracle (`interp`, `spec:<pointer>` or `unverified:<reason>`, [GOLDEN_ORACLES.md](docs/project/GOLDEN_ORACLES.md)), and the unverified goldens the interpreter gaps leave are retired by interpreter agreement or a recorded derivation | R-0491's detection map: the deterministic differential suite detects 080 and 084 – 088, name-binding controls 081 – 082, evidence and cross-surface controls 083 and 089, checker mutants 090 – 094 and the binary leg 095 (`check_model_diff.sh`, `fuzz_differential.py`); and each of the eight examples of R-0491's [adversarial example suite](docs/project/ROADMAP_TASKS.md#task-r-0491) (`linear_flow`, `generic_destroy`, `nested_names`, `import_aliases`, `casts_widths`, `mut_receivers`, `match_and_fnptrs`, `statement_sequencing`) passes its own leg of `scripts/tests/check_adversarial_examples.sh` (new, in the full gate set), comparing interpreter output, native output and the expected output, plus the proof model and claim statuses where the example carries claims; each bug assigned to this exit (108 – 110, 112, 114, 116, 117 – 120, 124, 130, 134) has its reproducer permanently checking the corrected expected outcome, and the bug ledger records each fixed on main; `check_example_outputs.sh` fails on an undeclared oracle (with negative controls) and reports zero unverified goldens | continues alongside | prevention plus repairs of loud compilation/link failures and interpreter gaps, without a demonstrated authority or safety defect; the generator coverage 2i – 2k need is part of their own acceptance; the examples add prevention coverage, while existing repairs and containment controls keep their owners and acceptance; a defect an example newly reproduces enters the queue under the entry rule, as a new row-0 exit when it is an authority or safety defect |
| 2d | R-0513 | checked inventories replace incidental count pins: every expected claim and comparison is accounted for, exclusions carry reasons, and missing entries, skipped work or unexpected eligibility shrinkage fail | adding a legitimate proved example needs no incidental count edit; controls with a deleted inventory entry, a disabled generator, an empty suite and altered eligibility each fail | continues alongside | gate affordability; no reproduced soundness defect |
| 2e | R-0513, R-0491 | minimum completed comparisons by semantic feature, with permanent positive and negative controls | every generated feature has a pinned minimum completed-comparison count, and a run below it fails | continues alongside | prevention; no reproduced defect |
| 2f | R-0513 | a fast push tier separate from full qualification | the fast tier meets its recorded budget (target five minutes); full results bind exact commits, toolchains and artifacts; a fast-tier pass is never presented as full qualification ([promotion pipeline](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline)) | continues alongside | validation cost; no deliverable depends on it |
| 2g | R-0506 | the axiom-policy gate on today's authoritative evidence at status, issuance and consumption: `sorryAx` and unregistered axioms rejected transitively, kernel-only evidence limited to `propext`, `Classical.choice` and `Quot.sound`, registered native-evaluation and float axioms under distinct trust labels | positive controls and forbidden-transitive-dependency mutations, with `check_axiom_inventory.sh` | blocks features | V1's evidence must pass the declared axiom policy (R-0506 is a stage V1 exercises) |
| 2h | R-0493 | one `IntArith` is the arithmetic authority for the existing pipeline's evaluation, folding and arithmetic-model generation, with emitted operations checked against the same semantics | `check_single_arithmetic.sh` (new) and differential controls over every supported width and operation family, including casts and defined traps | blocks features | V1a's checked `+ - *` and their traps are stated through it (R-0493 is a stage V1 exercises) |
| 2i | R-0500 | the safety rules of [MEMORY_SEMANTICS §15](docs/language/MEMORY_SEMANTICS.md) are written down and enforced: strictly second-class references — they exist only as parameters, call arguments, borrow-block binders and arm-scoped pattern binders through a borrowed scrutinee; no reference binding is introduced by `let` or assigned; none is stored in an aggregate or a constant, and constants are values, never places — and one binding per visible scope chain (SCOPE-1: no binder reuses a visible name; sibling scopes may), with the design-example decisions on `&mut` arguments and unused exclusive references (B1, B2) and owner reads under a shared borrow (B7) recorded | per rule: a normative paragraph in `docs/language/` with accepted and rejected examples, cited fixtures, S1 generator coverage meeting its minima and a versioned migration diagnostic; the 092 and 093 reproducers stay rejected, 087's shadowing reproducers, accepted today, become rejected by the shadowing rule, and the `let`-bound and assigned reference shapes (REF-6), accepted today, become rejected. Delivered 2026-10-21: the renamed pattern binder (G4), SCOPE-1@1 (E0301; bug 124 closed), REF-6@1 (E0302 / E0303, at Check and after monomorphization) and the versioned migration diagnostic of every rule, with the acceptance manifest `tests/rules/exit_2i.tsv` checked by `scripts/tests/check_rules_2i.sh`; what remains before this exit closes: S1 generator coverage and its minima (G8, with R-0491 and 2e), the B1/B2/B7 decision records (G9, G10), the mutation rows (R-0498), and the unenforced shapes G12, G13, G15, G16 and G17 (MEMORY_SEMANTICS §15.7) | blocks features | their violations were use-after-free and leaks in safe code (087, 092, 093); RocksDB handles and V1 locals are checked under them |
| 2j | R-0500 | the rules V1a's subset needs are written down and enforced: typed literals, literal range (signed minima preserved), statement vs value position, sequential assignment within a loop iteration, evaluation order and short-circuiting ([the evaluation-order rule](docs/language/STATEMENT_EXPRESSION_MODEL.md#evaluation-order-and-assignment): left to right in source order, struct and enum literal fields in source order, a functional-update base evaluated once after the explicit fields, an assignment evaluating its right-hand side before its destination), and what an assignment does to the old value (MEMORY_SEMANTICS §10) | the per-rule acceptance of 2i | blocks features | named dependency of V1a: the verified pipeline never models looser rules than the compiler enforces ([plan](docs/compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices)) |
| 2k | R-0500 | the remaining disputed rules are written down and enforced: explicit match binders and cast semantics per width pair, as [ARITHMETIC_POLICY §8](docs/language/ARITHMETIC_POLICY.md#8-numeric-conversions-as) decided it on 2026-10-21: `as` accepts only the always-lossless numeric, `bool` and `char` conversions of its class table, and every lossy conversion is a compile error that uses named truncation, reinterpretation, rounding or checked conversion; numeric scope only (pointer and reference casts unchanged); staged: the named operations, then the rejection with versioned migration diagnostics, then the migration | the per-rule acceptance of 2i | continues alongside | their defects (080, 084) are repaired under 0a; casts enter at a later V1 increment and match at V2, so neither is a dependency of RocksDB slice 1 or V1a |
| 2l | R-0498 | enforcement is demonstrated (S3): a claim → gate-leg → targeted-mutation manifest over the verification and language docs, enforcement-bearing comments in `Concrete/Proof`, `Check` and `Backend`, and emitted contract text, with R-18 – R-20 relabelled shape-only until proved | every enforcement phrase in that scope at the commit that opens this exit has a manifest row whose mutation its leg kills in a qualified ([stage (c)](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline)) campaign; later phrases join row 6's per-slice growth | continues alongside | prevention; no reproduced defect |
| 2m | R-0491 | the bug-discovery rate has fallen to about zero in every structural class. For each class of [STRUCTURAL_CLASSES.md](docs/bugs/STRUCTURAL_CLASSES.md) (a bug's primary class is the first class row that lists it; bugs in no class form one more bucket), its last three eligible *counting* probe batches in the [discovery record](docs/bugs/discovery.tsv) (new workloads, fuzzing campaigns, new examples, hunts, semantics probes, specifications, design proofs, interpreter coverage) each found no silent bug of that class and at most one loud bug of it; the exit closes when every class does. A batch that probes a structural migration's own new code carries the tag `class N: ` at the start of its probe cell: it counts toward that migration's class only, and every bug it finds counts against that class, but a silent bug it finds still ends the streak of the bug's own class. Each batch records its effort, and a quiet batch counts only with a recorded effort of at least four hours or ten probe programs, examples or cases (a smaller, token batch is recorded and shown but does not count). Reviews, fix follow-ons, CI and development attribute their bugs but are not probe rounds | the [bug dashboard](#bug-dashboard)'s per-class discovery table, generated by `scripts/gen/bug_dashboard.py` and checked by `check_docs_drift.sh`, reads three quiet batches for every class at the closing commit, and every bug record from 079 on is attributed to one batch (the generator refuses an unattributed record, and a tag that names no class or marks an attribution-only batch) | continues alongside | a convergence measure: it says whether the point repairs and the structural work have stopped the classes of [STRUCTURAL_CLASSES.md](docs/bugs/STRUCTURAL_CLASSES.md) from producing new bugs; it is kept per class because a migration in progress (R-0492's, whose probes filed about 15 bugs on 2026-10-25) finds bugs in its own class while it lands, and one global count could not close before every migration had finished; no reproduced defect and no deliverable depends on it, while Phase 1 and the preview's advertised stability do |
| 2n | project owner (the answers are recorded under R-0500) | the project owner has answered every item of the [owner decisions](#owner-decisions) list explicitly in conversation, each answer is recorded in that list with its date, and each rule's home, fixtures and enforcement agree with the answer; for a rule already on main, the answer decides whether it stays. The open bugs waiting on an answer are 216 (OD17) and 226 (OD20); the landed fixes of bugs 189, 198, 212 and 213 are provisional under OD22, OD13, OD16 and OD18 | every row of the owner-decisions list carries a dated answer given by the owner; an answer that changes or drops a rule already on main lands with that rule's specification entry, fixtures and diagnostics changed to match, and with the bugs that rule repaired still repaired by some rule; no row is filled from a recommendation, an earlier record, a commit or an agent's message | blocks features | the rules these items settle are the rules RocksDB slice 1 and V1a – V1c are checked under; the list names, per item, the exits and deliverables it gates, and a deliverable waits only for the items named against it |
| 2o | R-0550 | compile time is gated for scaling, with R-0422 repairing the class (structural class 17): generated inputs of 10^4 – 10^5 elements or statements in each shape (array literals, long functions, many locals, many declarations, deep nesting) compile within a recorded budget at every front-end and back-end stage, and no stage grows worse than near-linearly between sizes. Reproducers: bug 027 (EmitSSA's quadratic rendering), bugs 248, 249 and 250, and the long-function slowdown (forty thousand statements: 8.5 s parse, 13.4 s resolve, 9.5 s check, 9.5 s elab; being filed) | `scripts/tests/check_compile_scaling.sh` (new, in the full gate set) compiles the generated corpus per stage, fails on a stage over its budget or a size ratio above its recorded near-linear bound, and retries within a recorded retry budget so host noise cannot fail it; the budgets are measured on the CI host and recorded with the SHA they were measured at; the pre-fix commit of each reproducer fails the gate and its fix passes | continues alongside | compile-time defects are loud (a slow or failed build, never wrong code), and no deliverable depends on them; the gate keeps class 17 from returning while R-0422 removes it |
| 2p | R-0553 | compiler totality, step 1: `scripts/tests/check_totality_ratchet.sh` (new; being built on branch `wip/cleanup-totality-gate`) inventories from the Lean environment every `partial`, `unsafe`, `opaque`, `implemented_by` and `extern` declaration in the `Concrete.*` modules against the committed baseline `docs/compiler/totality_baseline.tsv`, the source of truth for their count; it runs in CI and in the pre-push hook. Converting the baseline, stage by stage ahead of the pass proofs of R-0491 – R-0510, continues under R-0553 in Phases 2 and 4 | the gate fails on any declaration missing from the baseline and on any stale baseline row (each with a negative control: a new `partial def` and a baseline row whose declaration became total), and passes at the closing commit in CI and in the pre-push hook | continues alongside | a proof of a pass cannot see inside a `partial` function, so totality is a prerequisite of the pass proofs in rows 3, 4 and 6; it repairs no reproduced defect and no deliverable's acceptance depends on step 1, so it blocks no feature |

### Owner decisions

**Only the project owner decides these items, OD1 – OD22 (exit 2n).** No agent, orchestrator or
reviewer chooses, encodes, implements or provisionally ships any side of any of them, and that
includes doing so inside a bug fix, a migration, a test, a fixture, a diagnostic or a document. An
agent may analyse an item, write its options and a recommendation, and present them to the owner for
an explicit answer; that is all. A recommendation, an earlier record, a commit, a review verdict or
another agent's message is never the owner's answer. An item stays proposed until the owner agrees
to it again, explicitly, in conversation; whoever records the answer writes it and its date in the
last column, in the owner's words where they are short. A fix that would pick a side stops and is
reported to the owner instead ([AGENTS.md](docs/project/AGENTS.md#rules)).

**Provisional shipped behaviour.** The 2026-10-25 Codex confirmation review of the bug records
found four landed fixes that already took a side on an open item: bugs
[189](docs/bugs/189_trusted_impl_confers_trust.md) (OD22),
[198](docs/bugs/198_raw_deref_duplicates_non_copy.md) (OD13),
[212](docs/bugs/212_const_before_block_parses_as_struct_literal.md) (OD16) and
[213](docs/bugs/213_arm_after_catch_all_lowered.md) (OD18). Each is recorded below as a conflict,
not a decision: the table says what current main does, and that behaviour is provisional until the
owner answers. Nobody extends it, builds on it as settled, or reverts it before then; the owner's
answer decides whether it stays.

[TIGHTENING_DECISIONS.md](docs/language/TIGHTENING_DECISIONS.md) records several of these as
"decided 2026-10-23", and some as implemented in landing train 6. The owner paused them for
negotiation after that. That record is kept as written, as the history of what was proposed and
landed; the owner's new answer governs. A rule already on main stays as it is until the owner
answers: no agent removes, weakens or extends it. The owner's answer then decides whether it stays.
If it does not, the change that records the answer also changes the rule's home, fixtures and
diagnostics, and keeps the bugs the rule repaired repaired.

"Gates" names the exits, deliverables and tasks whose rules depend on the item. The deliverables are
those of the [prerequisites table](#feature-prerequisites); "every deliverable" means RocksDB slice 1,
V1a, V1b and V1c.

| decision | proposal | record today | gates | owner's answer |
|---|---|---|---|---|
| OD1 | References: the Austral model. A reference is formed by borrowing a variable or projecting through a reference; `&s.f` is sugar; a temporary is never a root; loans are keyed by the root variable; a raw-pointer dereference needs a trusted validity premise | D2 and §4.5; implemented in landing train 6 (REF-5, REF-11) | exit 2i (G12), so every deliverable; bug 182's repair (exit 0b); V2 (row 6) | proposed |
| OD2 | `&mut` arguments: an implicit reborrow; overlapping exclusive arguments refused; a deferred call keeps its loan until it runs; one defined order for cleanup, loan end and write-back | D1 and §4.5; the reborrow implemented in landing train 6 (REF-10); the defer and exit-order rules not yet | exit 2i (G9, B1, B2), so every deliverable; R-0496 (Core reborrow form) | proposed |
| OD3 | Pattern binders through a borrowed scrutinee: the binder is always `&T`; its mode is stored in Core; the root cannot be reassigned while a binder's loan lives (the rule that reopened G17's Copy case) | D3 and §4.5; implemented in landing train 6 (REF-9), and with it the refusal of G17's Copy case (closed 2026-10-23 in the references chapter of the specification) | exits 2i (G17) and 2k (explicit match binders), so every deliverable; bug 181's repair (exit 0b); R-0496 | proposed |
| OD4 | Foreign function values: all refused now; typed `extern "C" fn` types later | D4; the refusal implemented in landing train 6 (FFI-1); the typed form not built | bug 166's repair (exit 0b), so every deliverable; R-0285's callback companion example; R-0283, R-0287 | proposed |
| OD5 | Generics: a bare `<T>` means Any; `T: Copy` is explicit; Copy for a generic struct is declared and checked, never derived | D5 (today's behaviour, made normative) and D6 with its refinement (a), under which a generic declared `Copy` is Copy when every argument is; D6 implemented 2026-10-23 (OWN-D1, TYPE-14) | R-0499 and R-0510 (the Core checker and checked specialization, row 4); V5 (row 6) | proposed |
| OD6 | Traits are coherent: one impl per (trait, type), the orphan rule, an ambiguous method call must be qualified, resolution by typed identity | D7; implemented by bug 186's repair (TRAIT-1 – TRAIT-9) | bug 186's repair (exit 0b), so every deliverable; R-0548, R-0549; V5 | proposed |
| OD7 | An unsafe-module marker in source (for example `trusted mod`), which never confers trust by itself; a gate lists the marked modules | D10; pending (TRUST-6, R-0509) | R-0509's trusted-base inventory; RocksDB slice 1 (where its `extern` declarations live); V6 | proposed |
| OD8 | Global state: immutable globals everywhere; mutable state only as declared cells in unsafe-marked modules, reached through capabilities; no `static mut` | D8; pending (TRUST-6, R-0509) | R-0509, R-0496; V6 | proposed |
| OD9 | Modules and interfaces: one file per module, `pub` as the interface, interface artifacts generated by the compiler, no handwritten interface files | D9; pending (TIGHTENING_DECISIONS §5, step 6) | R-0492 and R-0497 (interface data); separate compilation in V6; OD20 | proposed |
| OD10 | Arithmetic: checked by default, named wrapping and saturating operations, shifts with a checked count; an abort runs no deferred call | T-ARITH, recorded as settled (today's behaviour), with §4.5's abort rule | exits 2h and 2j, so V1a, V1b and V1c (checked `+ - *` and their traps); R-0493 | proposed |
| OD11 | Casts: `as` only when lossless by value domain; every lossy conversion is a named function | T-CAST: ARITHMETIC_POLICY §8 (2026-10-21) and §4.5's value-domain rule; not implemented | exit 2k; the V1 increment that admits casts | proposed |
| OD12 | Contracts (H27): a pure, non-consuming specification judgment, with machine and mathematical arithmetic kept distinct | T-H27 and §4.5; pending (R-0473) | R-0473 (row 12); any V1a or V1b claim stated with a contract | proposed |
| OD13 | Raw-pointer ownership: `ptr::read_owned`, an explicit replace or take through `&mut T`, a trusted ownership obligation for a raw address | T-RAWDUP and §4.5; implemented in landing train 6 (TRUST-7). **Provisional (bug 198):** current main refuses a by-value `*p` of a non-Copy pointee through a raw pointer (E0311) and moves the value out only with the intrinsic `read_owned(p)`, with 26 std sites migrated; provisional until the owner answers | RocksDB slice 1 (its trusted binding code); R-0509, R-0514; V6 | proposed |
| OD14 | Type validity (G2, G13, G15, G16): one structural well-formedness judgment before specialization; function types that return a reference refused everywhere | T-G2, T-G13, T-G15, T-G16; implemented 2026-10-23 (G13, G15 and G16 recorded closed in the references chapter of the specification) | exit 2i, so every deliverable | proposed |
| OD15 | G10: whole-owner freezing under a shared borrow stays. G19: no pass assumes `&mut` exclusivity, and the operational loan rules are defined now | T-G10 (keep the freeze) and T-G19 (the policy sentence) | exit 2i (the G10 decision record), so every deliverable; R-0496, R-0505 | proposed |
| OD16 | A struct literal in an `if`, `while` or `match` head is refused unless parenthesized | not in the record. **Provisional (bug 212):** current main reads an uppercase name followed by `{` in an `if`, `while` or `match` head as the start of a struct literal, refuses `if n > LIMIT { … }` with an E0001 that suggests parentheses, and states that case rule in `grammar/concrete.ebnf` and SYN-1; provisional until the owner answers | V1c (V1's surface grammar has conditionals and loops); R-0500 | proposed |
| OD17 | A field write requires a `mut` binding. Bug 216: `s.y = 1` is accepted on a binding not declared `mut`, while `&mut s.y` and `a[i] = v` are refused | not in the record; bug 216 open | bug 216's repair (class 5, R-0499); V2 (aggregates) | proposed |
| OD18 | Match arms: the first matching arm in source order wins; an unreachable arm is refused. Today an unguarded catch-all arm is moved to the end (bug 213) | not in the record. **Provisional (bug 213):** current main first moves unguarded catch-all arms after the other arms (CoreCanonicalize), so a specific arm written after a catch-all moves before it and stays selectable; in that Core order, it accepts an arm after the first unguarded catch-all and does not lower it, and does not refuse it; provisional until the owner answers | exit 2k (the match rules) and exit 2c's differential legs; V2 (match) | proposed |
| OD19 | Outside the language list: the scope of [PROVABLE_V1](docs/verification/PROVABLE_V1.md), that is, whether discarded statements, guards and ranges are admitted | open; it holds the model-coverage branch (`wip/model-coverage-3`) | that branch's landing; exit 2c's model-diff coverage | proposed |
| OD20 | Outside the language list: the visibility of std's internal `std.libc` bindings | bug 226 open (a std API decision) | bug 226's repair (MOD-2); OD9 | proposed |
| OD21 | Representation rights of a parent over its child module (added 2026-10-25, from bug 262): when a parent module imports its child's newtype or struct, every stage lets the parent construct the newtype and read and write the struct's private fields, because Check counts the children's declarations among the module's own for construction and field rights; Resolve also lets a parent name a child's newtype as a symbol without an import. Two options: enforce TYPE-11 as written (a newtype or private field is constructed or accessed only inside the module that declares it, so a child is another module), or amend TYPE-11 to grant a parent these rights over the declarations it imports from its children | the behaviour dates from construction rights slice 1 (2026-07-18); it contradicts TYPE-11@1's wording ("outside the module that declares it"); bug 262's record names `tests/programs/module_qualified_impl` as a program that applying TYPE-11 strictly would refuse; the wider impact is not yet measured (no `scripts/tools/rule_impact.sh` scan is recorded); bug 262's record (branch `wip/newtype-ctor-stage`) leaves the question to R-0011 and makes no recommendation | R-0011; the construction-rights fixtures (`module_qualified_impl` and bug 262's `error_262_*` and `regress_262_*` fixtures); bug 262's follow-up (whether Resolve binds a child's newtype in its parent) | proposed |
| OD22 | Trust on an `impl` block (added 2026-10-25, from bug 189). Bug 189's repair made the parser refuse `trusted impl` (E0001) and moved trust onto each method (`trusted fn`, 175 methods marked), while TRUST-5@1 in [the specification](docs/spec/trusted.md#TRUST-5) still says a `trusted fn` or `trusted impl` may perform raw-pointer operations. Two options: keep the refusal and amend TRUST-5 to drop `trusted impl`, or restore `trusted impl` as TRUST-5@1 states it and revert bug 189's refusal | **Provisional (bug 189):** current main refuses `trusted impl` and trusts only methods declared `trusted fn`; the specification text is unchanged, so main and TRUST-5@1 disagree; bug 189's record cites the 2026-10-23 rule that a marker never confers trust (D10, itself OD7, proposed); provisional until the owner answers | bug 189's repair (exit 0b) and its pins (`error_189_trusted_impl_refused.con`, `check_trusted_method.sh`); TRUST-5; R-0509's trusted-base inventory; OD7 | proposed |

### Feature prerequisites

Implementation and experiments on the first feature deliverables proceed now, within the rules as they
are on main; no early work, proof development included, chooses, encodes or implements an unanswered
[owner decision](#owner-decisions) (exit 2n). Early executable
feedback is how defects surface, and the RocksDB slice 1 groundwork already found bugs 102 and 103. The
exits listed against a deliverable are required before it is **accepted** or its safety is advertised;
none of them is removed or weakened by starting early, and a defect the early work finds enters the
queue under the entry rule. Isolated proof development (the reviewed design examples, proof libraries)
waits for none. Every *blocks features* exit appears in
at least one *blocked by* list and no *continues alongside* exit appears in any. For exit 2n a
deliverable waits only for the [owner decisions](#owner-decisions) whose "gates" cell names it or an
exit it is blocked by.

| deliverable | blocked by | not blocked by |
|---|---|---|
| RocksDB slice 1 (row 5, R-0285) | 0b, 2i, 2n | 0a, 0c, 1a, 2a, 2b, 2c, 2d, 2e, 2f, 2g, 2h, 2j, 2k, 2l, 2m, 2o, 2p |
| V1a (row 3, R-0511) | 0a, 0b, 2a, 2g, 2h, 2i, 2j, 2n | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l, 2m, 2o, 2p |
| V1b (row 3, R-0511 with R-0517) | 0a, 0b, 2a, 2g, 2h, 2i, 2j, 2n | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l, 2m, 2o, 2p |
| V1c (row 3, R-0511) | 0a, 0b, 2a, 2g, 2h, 2i, 2j, 2n | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l, 2m, 2o, 2p |

- **RocksDB slice 1** (R-0285). Declare the RocksDB C library as a dependency, import it with a normal
  Concrete import, open a database, write a key and read it back, handle a failure path (opening an
  invalid path returns a typed error, not an abort), and release every resource: each linear handle is
  destroyed exactly once. It builds and runs from a clean checkout with the normal commands and no
  manual flags. **Acceptance:** the example `examples/rocksdb_kv` (new) and its gate
  `scripts/tests/check_rocksdb_slice1.sh` (new, in the full gate set) build and run it from a fresh
  clone, assert the written value, the typed error and one destruction per handle, and keep two
  compile-fail fixtures rejected: a handle never destroyed and a handle used after it is closed. **Why
  these prerequisites:** the slice depends on linear handles being destroyed exactly once and used at
  most once, on capability containment at the foreign boundary and on correct code from the shared
  front end, which 0b's defects broke; 2i states the reference and scoping rules its handles and
  borrows are checked under. Evidence, proof-surface and gate-infrastructure exits do not affect it.
  It is the first acceptance point of R-0285's
  [first executable slice](docs/project/ROADMAP_TASKS.md#task-r-0285); the four-adapter checkpoint follows. R-0285's four
  companion FFI and resource examples (a file-handle wrapper, a C library holding a callback and its
  context, a resource pool, multi-step error paths that release everything) are accepted with or after
  this slice; they do not block it.
- **V1a** (R-0511). Must compile, from checked Core to executable BIR, every program of the V1a subset:
  `i64`, `u64`, `bool`, locals and assignment, conditionals, a general loop with structured exits,
  non-recursive direct calls, comparisons, checked addition, subtraction and multiplication, and one
  capability-authorized `print_int` event, with terminating, trapping and divergent behaviours. Other
  widths, casts, division, shifts, recursion and further loop forms are later V1 increments. R-0511's
  [V1a target programs](docs/project/ROADMAP_TASKS.md#task-r-0511) are the named instances of this subset and V1a's regression set;
  they do not widen it.
  **Independently checked artifact:** the Lean proof of checked Core → BIR refinement for that subset
  and its retained artifacts with a minimal canonical task identity, replayed by the independent
  retained-artifact consumer
  ([plan, V1 milestones](docs/compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices)).
- **V1b** (R-0511 with R-0517). Must compile the same programs to an identified native AArch64 image
  through Concrete's proved BIR → LCB-IR adapter and a pinned certified LCB domain that covers every
  reachable V1 operation and behaviour, including checked-arithmetic trap paths, direct calls and
  `print_int`. **Independently checked artifact:** the composed theorem (V1a, the adapter's refinement
  and LCB's preservation theorem, with LCB's trust boundaries as explicit premises), whose proof closure
  is re-checked by con-leche and nanoda and bound to the exact retained image and artifacts, with
  final-image correspondence checked by R-0505 (the plan's native exit). A smaller leaf-function
  intersection is a separately named intermediate theorem and does not complete V1b.
- **V1c** (R-0511). Restricted source → checked Core correspondence for V1's surface: for a single
  source unit with fixed imports, successful parsing, resolution, desugaring and elaboration are proved
  to implement syntax, resolution and elaboration judgments specified independently of their
  implementations (a relation defined as whatever the elaborator produces does not count), or proved
  certificate checkers cover those boundaries. Surface constructs outside the admitted subset are
  rejected with a diagnostic, never elaborated unchecked. **Independently checked artifact:** the
  correspondence theorem and its retained artifacts with a minimal canonical task identity, replayed by
  the independent retained-artifact consumer. Once V1b and V1c both hold, their independently replayed
  composition is required before the source-to-native statement for the subset is published. Until V1c holds, every V1 result states that its guarantee begins at
  retained `CheckedCore`. V1c may be accepted before or after V1b; the source-to-native statement needs
  both. **Why these prerequisites:** the same as V1a's: the judgments state 2i's and 2j's rules and
  2h's literal typing, and 0b's name-resolution defects (096 – 098) are front-end defects.

### Capacity allocation

Queue order says what comes first within a track; this table says how effort is divided between the
tracks, so that the verified compiler cannot be displaced by the work beside it. It replaces the earlier
limit (one shared-compiler migration checkpoint and one proof checkpoint, with interop, application,
usability and release work "under the same limit"), which did not say whether feature work could take
the migration slot. `check_roadmap_coverage.sh` checks that every queue row after Phase 1 and every
Phase 1 exit belongs to exactly one track, that the primary track holds the majority and the rows of
V1 and the shared compiler structure, and that the feature track is capped with a single slot.

| track | queue rows | Phase 1 exits | share per cycle | integration slots | scope |
|---|---|---|---|---|---|
| **A. Verified compiler** (primary) | 3, 4, 6 | 0b, 2h, 2i, 2j, 2k, 2n, 2p | at least 55% | 2: one shared-compiler migration checkpoint (row 4, then each slice's migrations) and one proof checkpoint (rows 3 and 6) | the semantics the proofs rely on (overflow, casts, evaluation order, traps, divergence, memory, with executable examples that agree: R-0493, R-0496, R-0500); the shared compiler structure; ownership and capability soundness, preserved through specialization (R-0499, R-0507, R-0510); source → Core correspondence (V1c, then each slice's frontend obligation); V1a – V7 |
| **B. Evidence integrity** (protected) | 9, 10, 11, 12, 13, 14, 15, 17 | 0a, 1a, 2a, 2g | at least 15% | 1 | specification and claim correctness (the claimed property is the theorem's proposition, typed contracts); one verdict authority; the axiom policy; task identity and evidence bound through linking to the final image and its dependency versions; independent checking. Before rows 9 – 11, the bounded increments every published V1 claim needs: R-0521's minimal task identity and R-0353's minimal independent retained-artifact consumer |
| **C. Adversarial validation and gate health** (continuous) | — | 0c, 2b, 2c, 2d, 2e, 2f, 2l, 2m, 2o | at least 10% | 1 | the prevention harness, scheduled hunts and fuzz campaigns, the adversarial example suite, demonstrated enforcement, trustworthy gates and qualification (R-0491, R-0498, R-0513); the compile-time scaling gate (R-0550, exit 2o); the repository cleanup programme (R-0554), queued in row 2 without an exit |
| **D. Bounded features** (capped) | 5, 7, 8, 16 | — | at most 20% | 1 | interop, application workloads, usability, release preparation and the external-user workflow, one checkpoint at a time |

- **Cycle.** Shares are measured over a planning cycle (a calendar month until release trains start,
  then one train) as the share of integration and review effort spent on each track's checkpoints.
  They are minimums and one maximum, not quotas: capacity a track of A – C does not use may go to
  another of A – C, never to D beyond its maximum. A cycle that misses a minimum or exceeds the maximum
  records the split, the reason and the recovery as a dated note under [Decisions](docs/project/ROADMAP_DECISIONS.md#decisions).
- **Slots.** At most one checkpoint per slot is active for integration, each with its accountable
  maintainer, required reviewer and exit recorded before it starts. D's single slot means feature work
  grows by sequence, not by parallel checkpoints: RocksDB slice 1 holds it until it is accepted, and
  the next D checkpoint (R-0515's preview checkpoints, the four-adapter checkpoint, release
  preparation) is chosen at a cycle boundary by what the next release needs. Heavy validation runs
  are limited to two per machine at a time (R-0543, 2026-10-22); a further one queues behind them
  whatever its track.
- **Defects.** Repairs preempt every track and are charged to their exit's track in this table, under
  the [defect disposition rule](#the-current-execution-queue); a new defect's exit names its primary
  class in [STRUCTURAL_CLASSES.md](docs/bugs/STRUCTURAL_CLASSES.md) and its track when it is added. When repairs keep A below its minimum for
  two consecutive cycles, the structural owner of the class causing most of them moves to the front of
  A's migration slot.
- **Development throughput.** The [development throughput](#development-throughput) track (R-0539 –
  R-0547, with R-0513) is charged to C. After RocksDB slice 1, the next D checkpoint waits until
  R-0547's measurements are recorded and landing trains (R-0539) run on the landing tier (R-0540).
- **Scope bounds until V1b's composed native exit holds.** RocksDB slice 1 and its four companions are
  the one executable workload; the four-adapter checkpoint follows its acceptance. R-0490 runs only
  bounded component workloads (row 7). Workloads expand beyond RocksDB slice 1 only after R-0515's
  practical debugging checkpoint. `std` work is limited to R-0021's usable subset for the first two
  workloads and recorded asks (R-0031's broader APIs wait); tutorial expansion stays pull-gated (R-0515);
  concurrency, a public registry and additional backends stay in the backlog behind their workload
  triggers; and
  editor, debugger and performance work goes no further than R-0515's V1 tooling checkpoints and the
  preview's release bar. Foreign-code and runtime boundaries grow with interop, not ahead of it: each
  foreign function or runtime facility a workload admits is recorded in R-0509's trusted-base
  inventory with its ownership, error and authority contract when it is admitted (R-0283/R-0287 check
  the C ABI), and V6 owns their proofs.

## Releases

This is the one place that describes how releases happen and what each milestone accepts; other
sections link here. R-0333 owns this section's release map and each candidate checklist; the
release-bar area retains the detailed release bars.

### Release rhythm

Proposed 2026-10-19:

- **Nightlies** publish the newest fully qualified main commit with its qualification record and no
  support promise.
- **Release trains** target a release every 4–6 weeks after the systems preview, when the applicable
  candidate acceptance passes. Security and soundness fixes may ship out of band; a skipped train
  records its reason. [Patch releases](#release-increments-and-candidate-records) repair an already
  published scope.
- **Milestones are acceptance scopes**, not dates and not a fixed allocation of version numbers: the
  experimental systems preview, the verified preview, the verified-subset milestones (V2 – V7), the
  verified-component alpha, the project beta and the 1.0 stable supported subset. The
  [phase table](#phases-and-releases) names the phase that produces each; its version labels are
  illustrative and are not phase numbers. A milestone ships when its scope's acceptance holds, not on a
  date. Allowing alpha before V7 would require an explicit queue amendment pulling its bounded
  evidence/contract prerequisites alongside slice growth.
- Release automation (R-0516) is a prerequisite for automated trains and nightlies. R-0333 records
  compatibility and numbering policy for each release.

### Validation and promotion

How a change is validated before it merges, how a candidate is qualified, the CI tiers, nightly
qualification, triage and the result vocabulary (passed, failed, skipped, cancelled, incomplete) are
in [Validation and promotion](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline). Its stages (a),
(b) and (c) are the ones the exits, the candidate table and the release checklists name.

## Release map

Release scopes below are provisional, not dates or claims that a release is ready. The execution queue
remains the sole scheduling authority. No release requires finishing the entire task catalog. This
subsection holds each release scope's acceptance; the candidate-record rules follow.

| release scope | user-visible outcome | required increments / owners | explicitly deferred |
|---|---|---|---|
| Experimental systems preview | Install Concrete and build, run and test a small program with explicit ownership and external authority, with honest reports of limitations. | Rows 0–2 qualified for the advertised scope; R-0333's exact candidate/subset record; R-0335/R-0336/R-0337 reference and honest claims; bounded R-0334/R-0346/R-0352/R-0354 installation/tutorial/distribution acceptance; R-0515 onboarding, first pilot and before-public-release checkpoint; R-0343/R-0345 security and threat disclosures; R-0357 supply-chain lock/license inventory; R-0358 compatibility/deprecation policy; preview-scoped R-0426/R-0427/R-0428 support, repair and response readiness. The preview checklist explicitly states which evidence-verifier/replay artifacts ship; unsupported proof replay cannot satisfy or advertise a verification release claim. | General verification claims, stable APIs, broad platform/backend coverage and the verified-component alpha promise. |
| Verified preview | Obtain V1a checked Core → BIR proof evidence. Native-image proof evidence is available only when the separately recorded V1b gate passes; otherwise native output is explicitly tested and `backend_trusted`, and the candidate ships under R-0333’s narrower recorded scope without graduating this one (R-0517’s fallback). The authoritative Core checker guards every program. | R-0511’s V1a exit and R-0514’s whole-language checker; R-0515’s applicable tooling checkpoints; R-0333’s candidate record states the theorem start and endpoint, V1b and V1c status, assumptions and fallback mode; without V1c the theorem is stated from retained `CheckedCore` and no source-to-native claim is made. This scope is narrower than Phase 2's exit: V1c, one `Layout` and Place MIR in the existing profile close Phase 2 but are not required to ship the verified preview, so the first verified release does not wait for the whole-language lowering migration. Native claims additionally require the complete R-0517/R-0505/R-0508 gate. | Everything outside the certified subset; native transport in fallback mode; claims beyond the transported example. |
| Verified-subset milestones | Each slice V2 – V7 enlarges the subset the `verified` profile accepts, with its cumulative theorem. | The verified-pipeline plan's cumulative V2–V7 exits, delivered by R-0491–R-0510 as coordinated under queue row 6; R-0512 migration for eligible claims; R-0515 checkpoints for the same window. | Features of later slices; unsafe/FFI until V6. |
| Verified-component alpha | A non-author changes a useful bounded component, repairs its contracts/evidence and independently replays the result. | R-0486's change-and-repair scenario, R-0150 measurements, the initial R-0182/R-0183 diff, R-0353 replay, and the queued qualification, identity and contract prerequisites; all existing release-bar-area alpha criteria. | General heap proofs, concurrency, whole-stdlib verification and proving every flagship completely. |
| Project beta | Maintain a multi-package project and review a dependency upgrade with inherited assumptions, evidence and policy decisions visible. | All existing release-bar-area beta criteria; bounded local-package increments from R-0363–R-0383, R-0440 policy, compatibility work R-0338–R-0341, and the minimal editor and external-user workflow. | Public registry, hosted publishing, remote caches, advanced editor features and additional production backends. |
| Stable supported subset | Rely on a named, versioned language/std/platform subset with compatibility, reproducible distribution and maintenance commitments. | R-0335/R-0425 reference and conformance, R-0337 claim freeze, R-0338–R-0341 compatibility, R-0351 migration, R-0352/R-0356/R-0357 distribution and provenance, R-0358 deprecation, and R-0426–R-0429 support/repair readiness. | Stability or verification guarantees for anything outside the published subset. |

The systems preview is a separate, narrower deliverable, **not** a weakening or
renaming of the existing verification alpha. It may expose experimental proof
features only with their actual evidence classes, assumptions and coverage.
The preview cannot advertise alpha graduation. Each later scope inherits the
applicable earlier acceptance checks, rerun on its own candidate.

### Next-release blockers

The next proposed release is the experimental systems preview. This is an
acceptance checklist, not an alternate priority list. It does not move release
packaging ahead of current queue work or mark any in-flight branch as complete.

- R-0333 names the exact preview subset, supported target/OS/toolchain matrix,
  known limitations and the validation required for every advertised feature.
- R-0335/R-0336/R-0337 publish a versioned description and consistent claims for
  that subset; examples and migration guidance are checked against the candidate.
- R-0334/R-0346/R-0352/R-0354 demonstrate clean installation, build/run/test and
  the tutorial on every claimed platform. Record external-user failures and fixes.
- R-0515's onboarding path, first pilot and before-public-release checkpoint
  (published inventory, named reviewers) are complete for the preview scope; the first
  pilot does not wait for R-0515's two-quarter continuing-feedback checkpoint.
- R-0343/R-0345 publish the security disclosure policy and threat model; R-0357
  locks the toolchain/dependencies and publishes the license inventory; R-0358 publishes
  the compatibility/deprecation policy; R-0426/R-0427/R-0428 are ready at preview scope.
- The preview checklist states exactly which evidence-verifier/replay artifacts ship
  (bounded R-0354 distribution); unsupported replay advertises no verification claim.
- R-0333 records a reproducible candidate and its validation result using the
  common candidate record below. Unresolved queue prerequisites remain blockers
  unless an explicit, scoped release disposition permits a narrower claim.

R-0484's and R-0483's closures, the two items already met, are recorded in the
[roadmap history](docs/archive/ROADMAP_HISTORY.md#next-release-blockers-met-before-2026-10-28).
Meeting those alone does not ship the preview. The mutation
campaign exception that permitted a development checkpoint does not authorize a
release exception. Qualification remains owned by its existing queue milestone;
a candidate must satisfy its applicable requirements or obtain a separately
recorded release decision that narrows the claims without concealing failures.

### Release increments and candidate records

Assign a **bounded increment** of a task to a release, not the whole task when its
objective spans several releases. Each R-0333 checklist entry names its existing
owner ID, delivered behavior, acceptance artifact and deferred remainder. For
example, alpha needs the three-family R-0182/R-0183 change-and-repair diff; their
broader reporting dimensions can remain open. Beta needs local package evidence,
not the whole package ecosystem. Completing an increment does not close its owner.
Research and workload-gated expansions remain in their existing sections until
pulled. Historical execution logs belong in the archive, not candidate checklists.

For each candidate, retain one versioned record under R-0333 containing:

- Exact source commit, compiler/toolchain identity and released artifact identities;
  supported language/std subset, platforms, backends and runtime profile.
- Required checks and retained results for that exact candidate, including positive
  and rejecting controls, applicable mutation qualification and clean installation.
  A timeout, skipped job or incomplete report is never a passing result.
- Source, std API/ABI and evidence-schema compatibility stated separately; migration,
  invalidation and revalidation requirements for changes to any of them.
- Open blockers, accepted limitations and deferred increments as separate lists,
  with owners and rationale. An accepted limitation cannot contradict an advertised
  guarantee; unresolved soundness failures inside the claimed scope block release.
- Matching reference, claims matrix, tutorial/examples and release notes. Distinguish
  enforced properties, checked proofs, assumptions, tests and incomplete coverage.
- Supported lifetime and patch procedure, with security reporting, rollback and
  evidence revocation/rechecking where affected (R-0343, R-0426–R-0429).

Patch releases repair an already published scope without waiting for the next
feature release. They have their own candidate commit and validation record;
compatibility breaks or changed evidence identities require an explicit migration
or invalidation notice. Stabilization must not silently expand the claimed subset.

## Backlog and pull rules

ROADMAP.md holds the phases and the queue; the [task register](docs/project/ROADMAP_TASKS.md) holds
every task that a queue row names or whose **Phase:** line includes an active or next phase (as
named at the top of this file). Every other task lives, with
its full text, ID and **Phase:** line, in the [roadmap backlog](docs/project/ROADMAP_BACKLOG.md), which
owns the pull rules:

- **Pull triggers.** Each backlog task carries a **Pull trigger:** line: the workload, bug or user need
  that hits it. A trigger firing, a phase that depends on the task becoming active or next, or a release
  checkpoint needing a bounded increment of it moves the task to the task register.
- **Phase entry checkpoint.** When the **Active phases:** line changes, every backlog task is reviewed and
  either pulled into the task register, kept with a recorded **Kept in backlog:** reason, or retired to the archive with a
  reason.
- **Per-release review.** Each release candidate reviews the backlog the same way.
- **Enforcement.** `scripts/tests/check_roadmap_coverage.sh` gates one home per task, pull triggers,
  placement against the active and next phases, and a backlog review for the current active phases.

## Decisions

The dated decisions that shape this queue (priority, semantics, tightening, owner-decision, delivery,
soundness, verified-compiler, usability, vertical-slice and behavioural-contract decisions) are in the
[decision log](docs/project/ROADMAP_DECISIONS.md#decisions). The language rules the project owner
reserved are not decided there: they are the [owner decisions](#owner-decisions) above.

## Reconciliation record

The [2026-10-19 reconciliation](docs/project/ROADMAP_RECONCILIATION.md) records historical task dispositions and the four prose inventories, including their source revision. The current queue and live task records own scheduling and acceptance. Transferred obligations remain binding unless explicitly retired with a recorded reason.

Every `### Task R-NNNN` carries a **Phase:** line; a superseded, merged, delegated, retired or done task also carries a **Disposition:** line naming its owner, and its row in the record's task-disposition table. `scripts/tests/check_roadmap_coverage.sh` reads that table and checks that every task has a phase or a disposition, that every disposition row and queue row names an existing task, that every queue row names an owner, and that no task's phase contradicts its queue row.

The record's [binding reconciliation dispositions](docs/project/ROADMAP_RECONCILIATION.md#binding-reconciliation-dispositions)
replace conflicting implementation directions in the named tasks; existing regression cases and
acceptance obligations survive unless explicitly retired with a reason.

## Archived history

Dated execution records, superseded status boards and merged-branch snapshots are archived in
[docs/archive/ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md), moved verbatim from this file at `a159c489`: the 2026-08-31
execution state (post-R-0004 mutation qualification, R-0004 closure, evidence-producer and gate-health
records, the multi-prover merge), the 2026-08-08 status board, the `spike/non-arithmetic-multi-kernel`
and `spike/multi-prover-evidence` records, the 2026-08-04 outstanding-work list, the superseded
sequencing notes and R-0004's detailed slice history. Every live remainder of those sections is
mapped to an owning task in the [reconciliation record](docs/project/ROADMAP_RECONCILIATION.md);
nothing in the archive schedules work or overrides the queue. The bodies of done, superseded, merged,
retired and delegated tasks are archived in
[docs/archive/ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md); each keeps a stub in its
area with its **Phase:**, **Disposition:** and **Objective:** lines.

The 2026-10-28 compaction cut this file down to its control plane. It moved the task bodies to the
[task register](docs/project/ROADMAP_TASKS.md), the decision log, direction and validation process to
their own documents (listed at the top), the language rules to the
[archived language scope](docs/archive/roadmap-language-scope.md), and completed records to the
[roadmap history](docs/archive/ROADMAP_HISTORY.md#moved-at-the-2026-10-28-roadmap-compaction).
