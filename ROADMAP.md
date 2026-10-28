# Concrete Roadmap

**What Concrete is aiming at.** Concrete is a systems language with linear ownership, second-class
references and explicit capabilities whose compiler is to be verified end to end: CompCert/CakeML-class
theorems that accepted programs are memory- and ownership-safe, that the native binary only does what
the program's Core semantics allows, and that supported claims about Core transport to the native image.
Its evidence (proofs, receipts, trust and assumptions) is to be compositional, transferable and
independently replayable ([verified-compiler decision](#decisions), [product north star](#product-north-star-compositional-transferable-evidence)).

**Where we are (2026-10-21).** Phase 1 (Stabilize) is in progress: the soundness repairs for bugs
079 – 101 are landing (queue row 0) and R-0482's P1 closures follow (row 1). Probes added since
2026-10-19 found bugs 102 – 120 in about two days, nearly all of them new instances of the recurring
[structural classes](docs/bugs/STRUCTURAL_CLASSES.md): point repairs are not converging (the
[bug dashboard](#bug-dashboard)'s discovery-rate signal measures this, and Phase 1 exit 2m closes
only when, in every structural class, three consecutive probe batches find about nothing), which is why
the [priority decision of 2026-10-21](#decisions) makes the verified compiler's structural work the
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

**Where things live.** This file answers *what happens next, in what order*: the phases, the one
execution queue, the release plan, the standing decisions, and the tasks a queue row names or an active
or next phase schedules.

- [Roadmap backlog](docs/project/ROADMAP_BACKLOG.md): every other task, each with its pull trigger
  ([pull rules](#backlog-and-pull-rules)).
- [Verified-compiler plan](docs/compiler/VERIFIED_PIPELINE.md): Phases 2 and 4 in detail.
- [Reconciliation record](docs/project/ROADMAP_RECONCILIATION.md): the 2026-10-19 task dispositions and
  transferred obligations.
- Archives: [roadmap history](docs/archive/ROADMAP_HISTORY.md) and
  [retired task bodies](docs/archive/ROADMAP_RETIRED_TASKS.md); completed work is in
  [CHANGELOG.md](CHANGELOG.md).
- Normative architecture: [Evidence Architecture](docs/verification/EVIDENCE_ARCHITECTURE.md) defines
  the five authoritative evidence objects and [Verification IR](docs/verification/VERIFICATION_IR.md)
  the target portable proposition boundary. This roadmap sequences their implementation and must not
  invent competing definitions.
- Direction and scope: [Capability unlocks](#capability-unlocks--the-stable-product-map),
  [North star](#north-star-compositional-fail-visible-verification) and the
  [language baseline](#language-baseline-and-bounded-verification-milestone).
- Validation: the [promotion pipeline](#promotion-pipeline) says which checks a change passes during
  development, before merge and before qualification or release, and what invalidates a recorded result.
- Defects: the [bug dashboard](#bug-dashboard) shows open bugs by structural class and exit, silent
  and loud, and the discovery-rate trend that Phase 1 exit 2m reads.
- Terms: [Glossary](#glossary).

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

**Open bugs: 10** (1 silent, 4 loud, 5 filed before the discovery record) of 279 bug records.

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

| discovery rate: counting probe batch | date | effort | found | silent | loud | signal |
|---|---|---|---|---|---|---|
| spec: language-specification examples (docs/spec: pilot, pipeline, ownership and types chapters, both consumers, and their reviews) | 2026-10-23 | 116 programs | 2 | 2 | 0 | found bugs |
| workload: Phase 7 workload 9 lzpack (LZSS + PackBits codec, CRC container) and nine probe programs on its shapes (fixed-array hash chains through &mut, ring buffer, Huffman tree, trait codecs, fn values in structs, shift and cast edges, nested linear Result/Option, borrow and defer forms), plus a 400-case corrupt-stream fuzz against a Python reference | 2026-10-23 | 10 programs | 2 | 0 | 2 | found bugs |
| fuzz: fuzz farm mutation generator findings open at eb122a01 (13 crash signatures re-run on the OVH server, interpreter vs binary) plus about 100 hand probes after R-0492 steps 1-3 (shadowing, fn-pointer locals, generics, match through references, defer exits, nested modules, diagnostics) | 2026-10-23 | 113 programs | 8 | 5 | 3 | found bugs |
| workload: Phase 7 workload 10 stackvm (assembler, bytecode codec and load-time verifier, interpreter and disassembler over a 23-variant enum, a generic bounded stack, two traits and defer cleanup) with a differential gate against an independent Python implementation, and 25 probe programs on its shapes (imported enums with impls and trait bounds through each import path, import aliases, linear values in loops, Destroy impls, temporaries as receivers, borrow conflicts, diagnostics) | 2026-10-23 | 26 programs | 5 | 0 | 5 | found bugs |
| hunt: differential hunt (interpreter vs native binary) on main 9622f7e0: 3500 fuzz_differential.py cases (seeds 102501, 102502) and hand probes on module constants, enum payloads across modules, generics and traits across module boundaries, nested submodules, defer and loops under --interp | 2026-10-24 | 60 programs | 5 | 0 | 5 | found bugs |
| hunt: Lower borrows and address-taken locals: hand probes of every CExpr and CStmt constructor, and the differential fuzzer's borrow mode (fuzz_differential.py --mode borrow) | 2026-10-26 | 3000 programs | 6 | 2 | 4 | found bugs |

| discovery rate by primary structural class | quiet counting batches in a row | holds |
|---|---|---|
| 1. Declaration identity by spelled name, not a resolved `DefId` | 3 | yes |
| 2. Binder identity by name, not a `LocalId` | 8 | yes |
| 3. Destruction dispatched by synthesized `<Type>_destroy` names | 9 | yes |
| 4. No reference-provenance model in the source checker | 9 | yes |
| 5. Borrow tracking by syntax instead of normalized places | 9 | yes |
| 6. Capability facts not carried across dispatch, specialization or package boundaries | 9 | yes |
| 7. Semantics implemented several times (interpreter, proof model, folder, backend disagree) | 5 | yes |
| 8. Storage, places and control flow decided ad hoc in `Lower` | 0 | no |
| 9. Evidence verdicts computed per surface and joined to their subject by name | 9 | yes |
| 10. Evidence identity computed from a partial subject | 9 | yes |
| 11. Absence read as a fact (fail-open defaults) | 9 | yes |
| 12. One fact kept in two places | 0 | no |
| 13. Documented rules with no enforcing site | 3 | yes |
| 14. Trusted `std` and FFI code with no checked contract | 9 | yes |
| 15. Ownership state not modeled along control-flow paths | 6 | yes |
| 16. Artifacts at paths no run owns | 9 | yes |
| 17. Pass state in association lists and per-query recomputation (superlinear compile time) | 1 | no |
| 18. Pipeline artifacts not bound to their payload | 9 | yes |
| 19. A mention read as a use (tool facts inferred from where a name is spelled) | 9 | yes |
| no class yet | 9 | yes |

Discovery signal (exit 2m): 17 of 20 classes have 3 consecutive eligible quiet counting batches (for a class: no silent bug of that class, at most one loud, effort at least 4 hours or 10 programs); the exit needs every class. 17 counting batches recorded. 71 attribution-only batches (reviews, follow-ons, CI, controls, development) found 117 further bugs.

<!-- END GENERATED: bug dashboard -->

## Implemented, not yet accepted

Work implemented on a branch (or landed without acceptance) but not yet accepted on main: bug fixes
keyed by bug number, and feature deliverables. Each row is one candidate: its branch, its exact
candidate SHA, the Phase 1 exits it contributes to or is blocked by, its [promotion-pipeline](#promotion-pipeline)
results (each at that SHA, or not run), what its acceptance unlocks and its remaining step. The table is
generated from the candidate record [`docs/project/candidates.tsv`](docs/project/candidates.tsv) by
`scripts/gen/candidate_table.py --write`; whoever lands, re-points or validates a candidate updates its
row and regenerates in the same commit, and `check_docs_drift.sh` fails when the table differs from the
record. A row leaves when its work is accepted on main and, for bug fixes, the
[bug ledger](docs/bugs/README.md) records it fixed; `scripts/tests/check_roadmap_coverage.sh` fails if a
listed bug is already recorded fixed there, or if a row lacks a branch or a remaining step. A stage (c)
pass is the [one qualification verdict](#promotion-pipeline) over every required job at the candidate
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
change. The [promotion pipeline](#promotion-pipeline)'s stages, its exact-SHA binding and its
passed / failed / skipped / cancelled / incomplete vocabulary bind every item below. A faster path
never reports a check it did not run as passed.

**Why (measured 2026-10-22).**

- **Time to land.** A finished, Codex-clean branch took 6 – 10 h of wall clock to reach main for about
  1 h of real work.
- **Review loops.** Codex took 8 – 26 rounds per branch (interp-coverage 26, exit-2i 17, validation
  12), and each round re-reviewed the whole diff. The cybersecurity filter blocked rounds on ownership
  work.
- **Big branches.** safety-eval2 carried 10 bugs and 23 test migrations, and exit-2i touched about 100
  files. Rule-changing branches broke other branches' new fixtures (symbols' mangling, exit-2i's REF-6,
  safety's heap rule).
- **Late failures.** The pre-push hook runs about 92 of 255 gates, chosen by area name. Stack-budget,
  trust-gate, float round-trip and linear-discard failures were first seen in CI or on later pushes,
  at about 1.5 h per round.
- **Serial landing.** Candidates landed one at a time. Each landing made every other branch rebase and
  re-run its local gates and CI. Hand-edited shared ledgers conflicted on every rebase: CHANGELOG, the
  bugs README, STRUCTURAL_CLASSES, the audit corpus, ROADMAP rows, the example manifest, the gate
  count and BuildIdentity.
- **CI wall time.** A run has about 35 jobs against roughly 20 concurrent jobs on the free plan, so
  runs queued. One waited 45 min, and its shards started an hour late. The language-surface job and
  the mutation shards take about 80 min each. lambdaclass/concrete is on GitHub Enterprise, with about
  500 concurrent jobs.
- **One laptop.** 5 – 7 agents plus gate runs pushed the load average to 140 – 240, which caused
  timeout false failures and 90-minute local full runs.
- **Infrastructure defects.** The hooks pointed at a stale pre-push without the wip tier. GIT_DIR
  leaked into gates under the hook (repaired in `run_ci_gates_local.sh` on branch `bug175`).
  Automatic git maintenance repacked during runs.
- **No measurement.** Stage timestamps were not recorded.

**Targets.**

| measure | target |
|---|---|
| Codex-clean to main | median under 2 h |
| landing CI, dispatch to conclusion, including queueing | median ≤ 40 min |
| local pre-push | median ≤ 10 min, and no gate failure first seen in CI over 20 consecutive landings |
| ledger conflicts on rebase | zero |
| review rounds per slice | median ≤ 5, and a slice past 5 is split |

**Working on main (2026-10-25).** These parts of the track are in use; each item still closes only
by its exit criterion in the table below.

- **Landing trains (R-0539).** `scripts/land/train.sh` builds a train, runs its gates, dispatches CI
  and lands the exact SHA on both remotes ([LANDING.md](docs/project/LANDING.md)). Recent trains
  landed this way, among them train 22 (`1b69ed0e`).
- **Landing CI tier (R-0540).** A candidate's CI run is the landing tier; the mutation campaign, the
  fuzz campaigns and proof replay are the campaign tier, run on dispatch with `campaign=true` or on
  schedule (`.github/workflows/lean_action_ci.yml`). `check_qualification_record.sh` refuses a
  landing-only run as a qualification.
- **Self-hosted runners (R-0541).** CI jobs run on runners on concrete-ci-2 when the repository
  variable `CONCRETE_SELF_HOSTED` is set, and never for `pull_request`
  ([SELF_HOSTED_RUNNERS.md](docs/project/SELF_HOSTED_RUNNERS.md)).
- **Load control (R-0543).** A heavy run takes a machine-wide slot (`scripts/tests/lib/heavy_slots.sh`,
  gated by `check_heavy_slots.sh`).
- **Gate-run cleanup on kill** ([bug 244](docs/bugs/244_killed_gate_run_wedges_lock.md)). A gate run
  that is killed no longer leaves gate processes running and the repository lock held.
- **Documentation-link gate** ([bug 252](docs/bugs/252_doc_link_check_unchecked_and_unrun.md)).
  `check_doc_links.sh` checks heading anchors and runs in CI and the local gate set.
- **Impact gate selection (R-0513).** The selector judges the text in a keyed array as text, not as a
  call ([bug 237](docs/bugs/237_selector_reads_map_text_as_calls.md)), so a docs-only change no
  longer selects unrelated gates.
- **Generated ledgers (R-0542).** Each bug record holds its facts in a ledger block (migrated by
  `scripts/gen/migrate_ledgers.sh`), CHANGELOG entries are `changelog.d/` fragments, and the bug
  lists, the class table, exit 0b's bug list, the bug dashboard and the candidate table are generated,
  each with a drift gate.

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
| **Phase 1 — Stabilize** | Land the 2026-10-18 soundness repairs, close R-0482, put the immediate protections in place | 0 – 2 | every [Phase 1 exit](#phase-1-exits) (0a – 2o): repairs gated; R-0482 qualified; one `Verdict` authority; language rules answered by the project owner and enforced | 0.1 Experimental systems preview |
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
| 2 | R-0491, R-0493, R-0494, R-0498, R-0500, R-0506, R-0513, R-0550 | Immediate protections (exits 2a – 2o): one verdict authority, prevention harness, trustworthy gates, axiom policy, one `IntArith`, language rules, demonstrated enforcement, the owner's language decisions, compile-time scaling |
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
task sections below hold every row's exit.

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
the first active `Task R-NNNN` in file order unless the table is refreshed in the same change that
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
| 2 | **Immediate protections and semantic decisions (R-0491, R-0493, R-0494, R-0498, R-0500, R-0506, R-0513, R-0550)** | closes through the individually closable exits listed in [Phase 1 exits](#phase-1-exits), each with its owner, acceptance test and class: one `Verdict` authority renders every evidence surface (2a); permanent reproducer controls for 080 – 101 (2b) and the prevention harness (2c); trustworthy gate execution — checked inventories instead of incidental count pins (2d), minimum completed comparisons (2e), a fast push tier separate from full qualification (2f); the axiom-policy gate on today's evidence (2g); one `IntArith` for the existing pipeline (2h); the rules the next slice needs written down and enforced with versioned migration diagnostics — strictly second-class references and no block `let` shadowing (2i); typed literals, literal range, statement vs value position, sequential assignment within a loop iteration, evaluation order and short-circuiting (2j); explicit match binders and cast semantics per width (2k); demonstrated enforcement (2l); the bug-discovery rate at about zero in every structural class across its three latest probe batches (2m); the project owner's answers to the [owner decisions](#owner-decisions) (2n); a compile-time scaling gate (2o). Known authority and safety defects are repaired under row 0. An exit classed *blocks features* holds back the acceptance of only the deliverables whose [prerequisites](#feature-prerequisites) name it; the others continue alongside feature work. Broader prevention coverage continues throughout and does not block isolated proof development |
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
| 0b | R-0482 | the wrong-code and memory/capability-safety repairs land: <!-- BEGIN GENERATED: exit 0b bugs (scripts/gen/bug_ledger.py; edit the records' exit lines) -->052, 082, 087, 088, 090 – 094, 096 – 100, 102 – 107, 109, 111, 113, 115, 121 – 123, 126 – 129, 131 – 133, 135 – 137, 140, 142 – 152, 165, 166, 176, 177, 180 – 183, 185, 186, 188 – 190, 192 – 198, 200, 204, 205, 208, 210, 215, 218, 220, 221, 238, 239, 247, 251, 253, 271, 272, 285, 287, 301, 302<!-- END GENERATED: exit 0b bugs -->, 095 (no record yet). The list is generated from the bug records' `exit: 0b` lines: every silent bug the [discovery record](docs/bugs/discovery.tsv) lists, except the false proofs and unsound claims exit 0a names, carries one, as do the memory-safety repairs it does not mark silent or that predate it (052, 093 – 095, 099, 100, 142, 145 – 148), and a silent bug filed later gets one in its record when it is filed. Among them is the silent defect found while writing the [2026-10-21 semantics decisions](#decisions) into the docs: the binary evaluating a dereference assignment's destination before its right-hand side while the interpreter does the reverse ([evaluation order](docs/language/STATEMENT_EXPRESSION_MODEL.md#evaluation-order-and-assignment); filed as bug 183). The other one recorded then, an arm binder through a borrowed scrutinee whose root is moved inside the arm ([MEMORY_SEMANTICS §15.7](docs/language/MEMORY_SEMANTICS.md) G17), is refused on main (E0204, re-probed 2026-10-23) and pinned by rejection fixtures; bugs 181 and 182 close under the [2026-10-23 tightening decisions](docs/language/TIGHTENING_DECISIONS.md#4-decisions-of-2026-10-23) D3 and D2, and bug 166 under D4's refusal; whether those rules stay is the project owner's answer to [owner decisions](#owner-decisions) OD1, OD3 and OD4 (exit 2n), and a different answer must still leave each of those bugs repaired | each bug's reproducer is a permanent gate leg, and the bug ledger records each fixed on main | blocks features | use-after-free, double free, skipped destruction and capability escape in safe code (052, 087, 090 – 093, 099 – 100, 091, and 104 and 131: a reference assigned from an inner block outlives its source, to a local or to a reference parameter), a safe user function named like a C symbol replacing it for the whole program (129: `fn malloc` becomes the allocator `alloc` calls), a comparison operator consuming its linear operands (140: `Heap == Heap` compared addresses and leaked both cells), a `return` inside a call argument or literal field leaking the values already moved into the construct (142), a deferred call reading a local freed after its `defer` was registered (143: `defer pb(&h); free(h);`), heap-shell interiors exempt from linearity (144: a field read twice double-frees, `free(h)` drops a linear interior), the raw `vec_set`/`vec_free` intrinsics losing non-Copy elements (145), `*h` never freeing its cell (146), a move inside an erased `assert`/`ghost let` counted as a release that never runs (147), a move inside the right operand of `&&`/`||` counted although it may not run (148), loop conditions, returns in nested blocks and labelled exits not checked for owned values (149), double free through `vec_get` taking a non-Copy element by value (115), silent wrong code (082, 088, 096 – 098, 102, 107, 111, and 123: a borrow of a module constant reads an uninitialized slot; 127: struct-literal initializers run in declaration order natively and in the ratified source order in the interpreter; 128 (fixed 2026-10-21): the interpreter, the differential reference, modelled an integer-to-float cast as the identity; 150 (fixed 2026-10-21): SSA cleanup folded `x != x` to false for a float `x`, so a NaN passed every such check; 152 (fixed 2026-10-21): `string_trim("")` returned a byte past its source; 151 (contained by E0299): `a.b_f` and `a_b.f` flattened to one name and the binary's `a_b::f()` ran `a.b_f`; 133: a write through `&mut (*p).f` lands in a copy of `*p`; 136: functional update `S { .., ..base }` evaluates its base once per omitted field; 137: enum-variant literal fields run in declaration order in both consumers; 176: a loop body's shadowing `let` outlived the loop in the checker, so a `u8` binding held 300 natively and in the interpreter; 105: a reference-valued `if`/`match` reached through a generic instantiated at a reference type, which the checker containment did not cover, segfaulted natively), a silent semantic change by a tool (135: `concrete fmt` rewrote float literals to six decimals, so `0.0000000001` became `0.000000`), missing extern effect declarations accepted in nested submodules (113; calls still charged `Unsafe`), a foreign destructor silently replaced by an empty body (103, a leak of every C resource released through a `*_destroy` function), and LLVM type layouts that disagree with the compiler's own, so aggregate copies move fields (121: conlog freed a corrupted String pointer; 122: the canonical builtin `Option`/`Result` union, including a stack overrun); the defects found on 2026-10-21 are a read through an arm binder after its source moved, and an interpreter/native disagreement on dereference-assignment order; and heap-slot corruption through a `&mut Heap<T>` argument (126: the callee treats the caller's heap-pointer slot as the struct, so a field write overwrites the slot and `free` receives a corrupted pointer) |
| 0c | R-0482 | campaign isolation is repaired and a qualified campaign result naming its exact commit is retained before the identity migration closes | a [stage (c)](#promotion-pipeline) qualification record with `verdict=qualified` at that commit: every required job succeeded there, the mutation aggregate's `qualified=1` among them | continues alongside | qualification of the identity migration; no deliverable depends on it |
| 1a | R-0482 | the manifestful std/dependency controls are permanent gates, D3's manifestless distinction and its negative controls are retained, and the result is published only after 0a's subject-binding repair | the P1 controls run as permanent legs of the full gate set, and the manifestless negative controls fail when the D3 distinction is collapsed | continues alongside | cross-package claims are not a dependency of RocksDB slice 1 or V1 |
| 2a | R-0494 | one `Verdict` authority renders every evidence surface (contracts, vcs, obligation-ledger, audit, check-proofs text and JSON, proof-status, issuance) | R-0494's exit: status spellings only in the renderer table; bug 089's reproducers (`rt_ens_only`, `rt_ens`, `rt_ens_bogus`, `rt_ens_noens`) read not-proved on every surface; a cross-surface gate (new) pins the expected positive and negative judgment for every claim and clause | blocks features | the proof surface is feature-frozen until it lands ([verified-compiler decision](#decisions)), so V1's published claim needs it |
| 2b | R-0491 | permanent reproducer controls for 080 – 101: each bug has a checked-in record, reproducer, expected outcome and pre-fix/fixed SHA pair | each assigned control fails at its pre-fix SHA and passes at its fix; `check_bug_filing_complete.sh` and `audit_bug_corpus.sh` fail on a missing inventory entry | continues alongside | the reproducers already block as gate legs under 0a and 0b; this exit proves each one load-bearing |
| 2c | R-0491 | the prevention harness (S1): the extracted model, then Core's semantics, as a third side of the differential fuzzer over every shape that broke, outcomes classified and never skipped, and model-diff for zero-argument, `bool` and aggregate signatures plus a binary leg with a pinned claimed-function inventory; and the adversarial example suite for today's bug classes; and closing the loud failures that keep valid examples out of the interpreter/native comparison (108 – 110, 112, 114, 116, 124, 130, 134, and interpreter gaps 117 – 120, all four fixed by 2026-10-21; the interpreter-coverage pass also closed D4 (`return` in value position) and D13 (output before a trap)); permanent differential legs for struct- and enum-literal field order, functional-update evaluation, assignment order and integer → float conversion ([2026-10-21 semantics decisions](#decisions)); and every example golden independently checked: each `@run` rule declares its oracle (`interp`, `spec:<pointer>` or `unverified:<reason>`, [GOLDEN_ORACLES.md](docs/project/GOLDEN_ORACLES.md)), and the unverified goldens the interpreter gaps leave are retired by interpreter agreement or a recorded derivation | R-0491's detection map: the deterministic differential suite detects 080 and 084 – 088, name-binding controls 081 – 082, evidence and cross-surface controls 083 and 089, checker mutants 090 – 094 and the binary leg 095 (`check_model_diff.sh`, `fuzz_differential.py`); and each of the eight examples of R-0491's [adversarial example suite](docs/project/ROADMAP_TASKS.md#task-r-0491) (`linear_flow`, `generic_destroy`, `nested_names`, `import_aliases`, `casts_widths`, `mut_receivers`, `match_and_fnptrs`, `statement_sequencing`) passes its own leg of `scripts/tests/check_adversarial_examples.sh` (new, in the full gate set), comparing interpreter output, native output and the expected output, plus the proof model and claim statuses where the example carries claims; each bug assigned to this exit (108 – 110, 112, 114, 116, 117 – 120, 124, 130, 134) has its reproducer permanently checking the corrected expected outcome, and the bug ledger records each fixed on main; `check_example_outputs.sh` fails on an undeclared oracle (with negative controls) and reports zero unverified goldens | continues alongside | prevention plus repairs of loud compilation/link failures and interpreter gaps, without a demonstrated authority or safety defect; the generator coverage 2i – 2k need is part of their own acceptance; the examples add prevention coverage, while existing repairs and containment controls keep their owners and acceptance; a defect an example newly reproduces enters the queue under the entry rule, as a new row-0 exit when it is an authority or safety defect |
| 2d | R-0513 | checked inventories replace incidental count pins: every expected claim and comparison is accounted for, exclusions carry reasons, and missing entries, skipped work or unexpected eligibility shrinkage fail | adding a legitimate proved example needs no incidental count edit; controls with a deleted inventory entry, a disabled generator, an empty suite and altered eligibility each fail | continues alongside | gate affordability; no reproduced soundness defect |
| 2e | R-0513, R-0491 | minimum completed comparisons by semantic feature, with permanent positive and negative controls | every generated feature has a pinned minimum completed-comparison count, and a run below it fails | continues alongside | prevention; no reproduced defect |
| 2f | R-0513 | a fast push tier separate from full qualification | the fast tier meets its recorded budget (target five minutes); full results bind exact commits, toolchains and artifacts; a fast-tier pass is never presented as full qualification ([promotion pipeline](#promotion-pipeline)) | continues alongside | validation cost; no deliverable depends on it |
| 2g | R-0506 | the axiom-policy gate on today's authoritative evidence at status, issuance and consumption: `sorryAx` and unregistered axioms rejected transitively, kernel-only evidence limited to `propext`, `Classical.choice` and `Quot.sound`, registered native-evaluation and float axioms under distinct trust labels | positive controls and forbidden-transitive-dependency mutations, with `check_axiom_inventory.sh` | blocks features | V1's evidence must pass the declared axiom policy (R-0506 is a stage V1 exercises) |
| 2h | R-0493 | one `IntArith` is the arithmetic authority for the existing pipeline's evaluation, folding and arithmetic-model generation, with emitted operations checked against the same semantics | `check_single_arithmetic.sh` (new) and differential controls over every supported width and operation family, including casts and defined traps | blocks features | V1a's checked `+ - *` and their traps are stated through it (R-0493 is a stage V1 exercises) |
| 2i | R-0500 | the safety rules of [MEMORY_SEMANTICS §15](docs/language/MEMORY_SEMANTICS.md) are written down and enforced: strictly second-class references — they exist only as parameters, call arguments, borrow-block binders and arm-scoped pattern binders through a borrowed scrutinee; no reference binding is introduced by `let` or assigned; none is stored in an aggregate or a constant, and constants are values, never places — and one binding per visible scope chain (SCOPE-1: no binder reuses a visible name; sibling scopes may), with the design-example decisions on `&mut` arguments and unused exclusive references (B1, B2) and owner reads under a shared borrow (B7) recorded | per rule: a normative paragraph in `docs/language/` with accepted and rejected examples, cited fixtures, S1 generator coverage meeting its minima and a versioned migration diagnostic; the 092 and 093 reproducers stay rejected, 087's shadowing reproducers, accepted today, become rejected by the shadowing rule, and the `let`-bound and assigned reference shapes (REF-6), accepted today, become rejected. Delivered 2026-10-21: the renamed pattern binder (G4), SCOPE-1@1 (E0301; bug 124 closed), REF-6@1 (E0302 / E0303, at Check and after monomorphization) and the versioned migration diagnostic of every rule, with the acceptance manifest `tests/rules/exit_2i.tsv` checked by `scripts/tests/check_rules_2i.sh`; what remains before this exit closes: S1 generator coverage and its minima (G8, with R-0491 and 2e), the B1/B2/B7 decision records (G9, G10), the mutation rows (R-0498), and the unenforced shapes G12, G13, G15, G16 and G17 (MEMORY_SEMANTICS §15.7) | blocks features | their violations were use-after-free and leaks in safe code (087, 092, 093); RocksDB handles and V1 locals are checked under them |
| 2j | R-0500 | the rules V1a's subset needs are written down and enforced: typed literals, literal range (signed minima preserved), statement vs value position, sequential assignment within a loop iteration, evaluation order and short-circuiting ([the evaluation-order rule](docs/language/STATEMENT_EXPRESSION_MODEL.md#evaluation-order-and-assignment): left to right in source order, struct and enum literal fields in source order, a functional-update base evaluated once after the explicit fields, an assignment evaluating its right-hand side before its destination), and what an assignment does to the old value (MEMORY_SEMANTICS §10) | the per-rule acceptance of 2i | blocks features | named dependency of V1a: the verified pipeline never models looser rules than the compiler enforces ([plan](docs/compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices)) |
| 2k | R-0500 | the remaining disputed rules are written down and enforced: explicit match binders and cast semantics per width pair, as [ARITHMETIC_POLICY §8](docs/language/ARITHMETIC_POLICY.md#8-numeric-conversions-as) decided it on 2026-10-21: `as` accepts only the always-lossless numeric, `bool` and `char` conversions of its class table, and every lossy conversion is a compile error that uses named truncation, reinterpretation, rounding or checked conversion; numeric scope only (pointer and reference casts unchanged); staged: the named operations, then the rejection with versioned migration diagnostics, then the migration | the per-rule acceptance of 2i | continues alongside | their defects (080, 084) are repaired under 0a; casts enter at a later V1 increment and match at V2, so neither is a dependency of RocksDB slice 1 or V1a |
| 2l | R-0498 | enforcement is demonstrated (S3): a claim → gate-leg → targeted-mutation manifest over the verification and language docs, enforcement-bearing comments in `Concrete/Proof`, `Check` and `Backend`, and emitted contract text, with R-18 – R-20 relabelled shape-only until proved | every enforcement phrase in that scope at the commit that opens this exit has a manifest row whose mutation its leg kills in a qualified ([stage (c)](#promotion-pipeline)) campaign; later phrases join row 6's per-slice growth | continues alongside | prevention; no reproduced defect |
| 2m | R-0491 | the bug-discovery rate has fallen to about zero in every structural class. For each class of [STRUCTURAL_CLASSES.md](docs/bugs/STRUCTURAL_CLASSES.md) (a bug's primary class is the first class row that lists it; bugs in no class form one more bucket), its last three eligible *counting* probe batches in the [discovery record](docs/bugs/discovery.tsv) (new workloads, fuzzing campaigns, new examples, hunts, semantics probes, specifications, design proofs, interpreter coverage) each found no silent bug of that class and at most one loud bug of it; the exit closes when every class does. A batch that probes a structural migration's own new code carries the tag `class N: ` at the start of its probe cell: it counts toward that migration's class only, and every bug it finds counts against that class, but a silent bug it finds still ends the streak of the bug's own class. Each batch records its effort, and a quiet batch counts only with a recorded effort of at least four hours or ten probe programs, examples or cases (a smaller, token batch is recorded and shown but does not count). Reviews, fix follow-ons, CI and development attribute their bugs but are not probe rounds | the [bug dashboard](#bug-dashboard)'s per-class discovery table, generated by `scripts/gen/bug_dashboard.py` and checked by `check_docs_drift.sh`, reads three quiet batches for every class at the closing commit, and every bug record from 079 on is attributed to one batch (the generator refuses an unattributed record, and a tag that names no class or marks an attribution-only batch) | continues alongside | a convergence measure: it says whether the point repairs and the structural work have stopped the classes of [STRUCTURAL_CLASSES.md](docs/bugs/STRUCTURAL_CLASSES.md) from producing new bugs; it is kept per class because a migration in progress (R-0492's, whose probes filed about 15 bugs on 2026-10-25) finds bugs in its own class while it lands, and one global count could not close before every migration had finished; no reproduced defect and no deliverable depends on it, while Phase 1 and the preview's advertised stability do |
| 2n | project owner (the answers are recorded under R-0500) | the project owner has answered every item of the [owner decisions](#owner-decisions) list explicitly in conversation, each answer is recorded in that list with its date, and each rule's home, fixtures and enforcement agree with the answer; for a rule already on main, the answer decides whether it stays. The open bugs waiting on an answer are 216 (OD17) and 226 (OD20); the landed fixes of bugs 189, 198, 212 and 213 are provisional under OD22, OD13, OD16 and OD18 | every row of the owner-decisions list carries a dated answer given by the owner; an answer that changes or drops a rule already on main lands with that rule's specification entry, fixtures and diagnostics changed to match, and with the bugs that rule repaired still repaired by some rule; no row is filled from a recommendation, an earlier record, a commit or an agent's message | blocks features | the rules these items settle are the rules RocksDB slice 1 and V1a – V1c are checked under; the list names, per item, the exits and deliverables it gates, and a deliverable waits only for the items named against it |
| 2o | R-0550 | compile time is gated for scaling, with R-0422 repairing the class (structural class 17): generated inputs of 10^4 – 10^5 elements or statements in each shape (array literals, long functions, many locals, many declarations, deep nesting) compile within a recorded budget at every front-end and back-end stage, and no stage grows worse than near-linearly between sizes. Reproducers: bug 027 (EmitSSA's quadratic rendering), bugs 248, 249 and 250, and the long-function slowdown (forty thousand statements: 8.5 s parse, 13.4 s resolve, 9.5 s check, 9.5 s elab; being filed) | `scripts/tests/check_compile_scaling.sh` (new, in the full gate set) compiles the generated corpus per stage, fails on a stage over its budget or a size ratio above its recorded near-linear bound, and retries within a recorded retry budget so host noise cannot fail it; the budgets are measured on the CI host and recorded with the SHA they were measured at; the pre-fix commit of each reproducer fails the gate and its fix passes | continues alongside | compile-time defects are loud (a slow or failed build, never wrong code), and no deliverable depends on them; the gate keeps class 17 from returning while R-0422 removes it |

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
| RocksDB slice 1 (row 5, R-0285) | 0b, 2i, 2n | 0a, 0c, 1a, 2a, 2b, 2c, 2d, 2e, 2f, 2g, 2h, 2j, 2k, 2l, 2m, 2o |
| V1a (row 3, R-0511) | 0a, 0b, 2a, 2g, 2h, 2i, 2j, 2n | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l, 2m, 2o |
| V1b (row 3, R-0511 with R-0517) | 0a, 0b, 2a, 2g, 2h, 2i, 2j, 2n | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l, 2m, 2o |
| V1c (row 3, R-0511) | 0a, 0b, 2a, 2g, 2h, 2i, 2j, 2n | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l, 2m, 2o |

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
| **A. Verified compiler** (primary) | 3, 4, 6 | 0b, 2h, 2i, 2j, 2k, 2n | at least 55% | 2: one shared-compiler migration checkpoint (row 4, then each slice's migrations) and one proof checkpoint (rows 3 and 6) | the semantics the proofs rely on (overflow, casts, evaluation order, traps, divergence, memory, with executable examples that agree: R-0493, R-0496, R-0500); the shared compiler structure; ownership and capability soundness, preserved through specialization (R-0499, R-0507, R-0510); source → Core correspondence (V1c, then each slice's frontend obligation); V1a – V7 |
| **B. Evidence integrity** (protected) | 9, 10, 11, 12, 13, 14, 15, 17 | 0a, 1a, 2a, 2g | at least 15% | 1 | specification and claim correctness (the claimed property is the theorem's proposition, typed contracts); one verdict authority; the axiom policy; task identity and evidence bound through linking to the final image and its dependency versions; independent checking. Before rows 9 – 11, the bounded increments every published V1 claim needs: R-0521's minimal task identity and R-0353's minimal independent retained-artifact consumer |
| **C. Adversarial validation and gate health** (continuous) | — | 0c, 2b, 2c, 2d, 2e, 2f, 2l, 2m, 2o | at least 10% | 1 | the prevention harness, scheduled hunts and fuzz campaigns, the adversarial example suite, demonstrated enforcement, trustworthy gates and qualification (R-0491, R-0498, R-0513); the compile-time scaling gate (R-0550, exit 2o) |
| **D. Bounded features** (capped) | 5, 7, 8, 16 | — | at most 20% | 1 | interop, application workloads, usability, release preparation and the external-user workflow, one checkpoint at a time |

- **Cycle.** Shares are measured over a planning cycle (a calendar month until release trains start,
  then one train) as the share of integration and review effort spent on each track's checkpoints.
  They are minimums and one maximum, not quotas: capacity a track of A – C does not use may go to
  another of A – C, never to D beyond its maximum. A cycle that misses a minimum or exceeds the maximum
  records the split, the reason and the recovery as a dated note under [Decisions](#decisions).
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

### Promotion pipeline

A change is promoted through three stages. Each result is recorded against the exact commit SHA it
ran on, with passed, failed, skipped, cancelled and incomplete kept distinct: a timeout, skipped job or
cancelled run is never a pass. A result is reused for a later commit only with an explicit account of
why the difference cannot affect it.

| stage | when | what runs | recorded as | invalidated by |
|---|---|---|---|---|
| **(a) fast affected checks** | every push, during development | the [pre-push hook](.githooks/pre-push) (`make pre-push`): gate hygiene always; for a docs-only push (every changed path under `docs/` or ending `.md`) the docs-drift gate (which runs `check_roadmap_coverage.sh`) and `check_roadmap_linear.sh`; otherwise the fast surface gates, the bug-filing check when bug files change, and the gates and CI jobs of the touched areas, resolved by `run_ci_gates_local.sh`. Branch pushes also run the fast surface gates in the branch-health workflow | the hook's validation record (`scripts/tests/lib/prepush_record.sh`), bound to the pushed commit, a clean tree, untracked files, the toolchain and compiler binary, the gate definitions, the selection and the changed-file list | any change to one of those inputs; the hook reruns rather than reuse a mismatched record. A stage (a) pass never stands for (b) or (c) |
| **(b) integrated full gate set** | on the integrated candidate before it merges to main, and on main after each push | the CI workflow's ([`lean_action_ci.yml`](.github/workflows/lean_action_ci.yml)) push/pull-request jobs: CI on pushes to main and pull requests, or `gh workflow run CI --ref <branch>` for a branch (a dispatch runs the landing tier only; `-f campaign=true` adds the campaign tier, see [CI tiers](#ci-tiers)). This includes the Phase 6C observability capstone (`check_phase6c_observability.sh`) and, on pushes to main, the multi-kernel evidence gate. Locally, `scripts/tests/run_ci_gates_local.sh` extracts the same gate commands but excludes the mutation campaign, its aggregator and `checker_acceptance_evidence.sh`, and does not reproduce step-level environments; `make pre-push-full` runs it in parallel for feedback only. A local completion record needs closure mode (`CONCRETE_CLOSURE_RUN=1`, which forces `JOBS=1`) on a clean candidate and covers only that locally runnable population | the CI run ID and its head SHA; or a local closure-mode summary with `completed=1` and that head, which never stands in for the CI-only steps | any change outside documentation (compiler, `std`, examples, proofs, gate scripts, workflow, toolchain, lake or nix inputs) reruns the whole set. A docs-only change reruns only the gates that read the changed documents (docs drift and roadmap gates, links, and the hole-status, claims and axiom-inventory gates when those documents change) |
| **(c) full qualification** | on an immutable candidate: before a release candidate or nightly, and before closing an exit that names a qualified result | stage (b) on the same SHA, plus the jobs that run only on schedule or dispatch: the gate-mutation campaign (`check_gate_mutation_coverage.sh`, sharded by CI's mutation plan, shard and aggregate jobs on the nightly schedule on `main` of `lambdaclass/concrete` or a `workflow_dispatch` with `campaign=true`; it refuses a dirty tree and a git worktree, so a local campaign runs in a fresh clone of the candidate), the weekly clean-checkout proof replay, the nightly differential fuzz campaigns and the scheduled multi-kernel mutation coverage (`MULTI_KERNEL_MUTATE=1`); the con-leche external re-check already runs in (b) | one qualification verdict over **every** required job at the same candidate SHA: `scripts/ci/qualification_record.py` reads the record of exactly one run: the first attempt of a `workflow_dispatch` run of the candidate dispatched with `campaign=true` (the only event that starts every job; a dispatch without it skips the campaign tier and is refused by those skipped jobs; scheduled runs skip each other's jobs, partial runs are never combined, and a rerun could hide a failure) and emits the candidate SHA, each job of the candidate commit's own `lean_action_ci.yml` (read from that commit and parsed structurally; a workflow shape it does not understand is refused) with its run, head SHA, status and conclusion (each mutation shard 1..N individually), and `verdict=qualified` only when the run and every required job succeeded at that SHA and every unconditional step of each job, plus the conditional multi-kernel mutation coverage, ran exactly once and succeeded. The mutation aggregate's `qualified=1` (every planned shard completed) is one component of it, never the verdict: the aggregate succeeds while proof replay, fuzzing, the multi-kernel and checker checks or the regression suites fail. A missing, cancelled, skipped, failed or in-progress job or required step, a missing, duplicate or unexpected shard, a run at another SHA or with a non-success conclusion, or a failure hidden behind a successful rerun at the same SHA prevents qualification, and a workflow the parser does not understand is refused rather than read as fewer jobs (`check_qualification_record.sh` attacks the verdict with these records). A local closure-mode summary cannot qualify. The record, with its run IDs, is kept in R-0333's candidate record for a release and in the [candidate record](#implemented-not-yet-accepted) for an exit | any change outside documentation, including a rebase onto a new base; a docs-only change invalidates neither (b) nor (c) except their document-reading gates |

#### CI tiers

The CI workflow's jobs fall in two tiers. Every job runs somewhere; none was dropped.

- The **landing tier** is what a merge candidate needs (stage (b)). It runs on every push to `main`,
  every pull request, every `workflow_dispatch` and both crons. Its jobs: `LL(1) grammar check`,
  `Build`, `Main test suite`, `Build + main tests (macOS)`, `SSA test suite`, `Trust gate`, `Proof
  evidence gate`, `External proof re-check (con-leche)`, the four language-surface jobs
  (`Language-surface gates (Phase 5-6)`, `Receipt-path gates`, `Checker-identity gates`,
  `Evidence-identity gates`; one ~80-minute job until 2026-10-22, split by measured step time into
  four of ~20 minutes), `Compiler-infra gates`, the two `Campaign gates` jobs (they test the campaign
  tooling with fixtures, not the campaign), `Thesis evidence smoke test`, `Examples`, `Extra gates`,
  and `Multi-kernel evidence` (on push, dispatch and schedule, and on a pull request labelled
  `multi-kernel`). The critical path is now `Build` then `Proof evidence gate` (~60 minutes), down
  from `Build` then the unsplit language job (~85 minutes).
- The **campaign tier** is stage (c) evidence and gates qualification, not merges. It runs only in a
  dispatch with `campaign=true`: `gh workflow run CI --ref <ref> -f campaign=true` for a candidate,
  and every night on `main` (below). Its jobs: `Gate mutation plan`, the 24 `Gate mutation shard N`
  jobs, `Gate mutation coverage (aggregate verdict)`, `Nightly differential fuzz`, `Weekly
  clean-checkout proof replay` (now nightly; the job keeps its name) and the `Multi-kernel mutation
  coverage` step of the multi-kernel job. The CI workflow has no cron of its own.

#### Nightly qualification, triage and flakes

- **Nightly.** [`nightly.yml`](.github/workflows/nightly.yml) dispatches CI on `main` with
  `campaign=true` at 03:23 UTC, on `lambdaclass/concrete`, and on `unbalancedparentheses/concrete2`
  when the repository variable `CONCRETE_NIGHTLY_QUALIFICATION` is `true` (R-0468's opt-in). It
  reuses CI's jobs, so nothing runs twice, and a campaign dispatch is the one event
  `scripts/ci/qualification_record.py` accepts: every night yields a stage (c) verdict for `main`'s
  tip. When the run completes, the report job (`scripts/ci/nightly_report.py`) writes to its job
  summary the verdict, the gates that failed tonight but not last night, the triage of the run and
  the mutation-coverage diff against the previous night (`scripts/ci/coverage_diff.py`: a killed
  family that survives, or any family dropped, is a regression). The report is red unless the run
  qualified with no new failure and no lost coverage. Its jobs route to self-hosted runners under
  `CONCRETE_SELF_HOSTED_CAMPAIGN`, like the campaign jobs.
- **Triage.** `scripts/ci/triage.sh <run-id>` classifies each failed job as infra (runner lost, a
  cancellation, a network, apt or cache failure, or a bounded run's `INCOMPLETE` marker with no
  failed assertion), infra-defect (our tooling cannot build a pinned tool, e.g. lean4export under a
  leaked `GIT_DIR`: reported, never rerun, never a flake), known-flaky (a ledger row whose signature
  matches) or real. It reruns the failed jobs once (`gh run rerun --failed`) only when every failure
  is infra or known-flaky, never on attempt 2 and never for a run cancelled as a whole; otherwise it
  writes `.triage/<run>.md` with the gate, its FAIL lines, a local reproduction, the impact-selected
  command, the suspected commits and, when `scripts/gen/new_bug.sh` exists, the command to file a bug
  skeleton (printed, not run). A rerun is diagnostic: qualification reads only a first attempt.
- **Flakes.** `scripts/ci/flake_history.py` derives per-gate outcomes (passed, failed, skipped,
  cancelled, incomplete) from GitHub's run records on demand, so nothing is committed per run; the
  nightly keeps each reading as the `flake-history-<run>` artifact. A gate that failed and passed on
  one SHA is a flake candidate (infra and infra-defect failures excluded). The ledger
  [`docs/project/flaky_gates.tsv`](docs/project/flaky_gates.tsv) is committed and human-curated:
  `known-flaky` and `quarantined` need an owner, a quarantine needs an expiry, and a quarantined
  gate still runs and still fails its job; triage reports it separately as failed and reruns it
  once, and after the expiry its failures are real again. `check_ci_triage.sh` pins all of this.

Runners: every Ubuntu job runs on GitHub-hosted `ubuntu-latest` unless the repository variable
`CONCRETE_SELF_HOSTED` (landing tier) or `CONCRETE_SELF_HOSTED_CAMPAIGN` (campaign tier) is `true`;
a `pull_request` event never runs on a self-hosted runner. The macOS job stays on `macos-latest`.
Setup and security rules: [SELF_HOSTED_RUNNERS.md](docs/project/SELF_HOSTED_RUNNERS.md).

**Result vocabulary.** Every check reports one of five results, and they are never merged:

- **passed**: the check ran to completion and every assertion held;
- **failed**: it ran to completion and observed a wrong result;
- **incomplete**: it did not finish, so nothing was observed. A timeout is incomplete, not failed and
  not passed. A test that exceeds its budget is re-run once, alone, with a larger budget: a
  completed re-run is judged normally and recorded *recovered*; a second timeout is reported
  `INCOMPLETE`, and the timed-out run's partial output is discarded, so no output check can pass on
  it. `scripts/tests/lib/bounded.sh` implements this (a command's own exit 124 is told apart from a
  timeout; its behaviour is pinned by controls in `check_example_outputs.sh`) for the per-test
  budgets of `check_example_outputs.sh`, `check_callable_values.sh`,
  `check_hashmap_probe_invariants.sh`, `check_model_diff.sh` and `run_tests.sh`'s project fixtures,
  which count incomplete results apart and list recovered ones, and as the watchdog wrapper of the
  gates that bound single commands; `test_ssa.sh` applies the same rule to its parallel 5-second
  alarms (re-run alone with 30 s). In the seventeen watchdog-form gates a second timeout gives the
  wrapped command no output and status 124, and the gate's last line (`bounded_gate_status`) lists
  recovered and incomplete runs and fails the gate on any incomplete one, whatever its own checks
  concluded; their per-check tallies still count such a command among their failures, and R-0513
  owns moving them to per-check accounting. A timed-out command is killed with its whole process
  group (TERM, then KILL) before the retry starts. Fuzzers are the exception by
  design: a hang on a generated input is their finding, triaged like a crash;
- **skipped**: deliberately not run, with a recorded reason (a platform without the tool, an opt-in
  leg). A judgment a host cannot make soundly is skipped there, not approximated: the gate reports it
  NOT-JUDGED and exits 77, and `run_ci_gates_local.sh` records that as *deferred to CI* only for a
  gate `scripts/tests/lib/host_tools.sh` declares judged on CI (its `judged_on`) and CI runs
  unconditionally; CI itself must be able to judge it, or the gate fails there (bug 315: the scaling
  gate's wall verdicts on a host without instruction counters);
- **cancelled**: the run was stopped from outside (a superseded CI run).

A gate with an incomplete result exits nonzero, so incomplete never reads as green; its log names
the result for what it is, so it is not investigated as a wrong answer. A **qualifying run**
(stage (b) or (c), or a local closure-mode record) contains no failed, incomplete or cancelled
result, and a skipped result only where the stage's definition allows it. Machine load is a reason
to re-run a check, never to accept an incomplete one.

**How the stages run today.** CI is the validator; local runs give feedback.

1. **Development.** Work happens on a candidate branch in its own worktree
   ([CONCURRENT_WORK.md](docs/project/CONCURRENT_WORK.md)). Work in progress is backed up to
   `wip/<name>` every few minutes; the pre-push hook's wip tier checks only for conflict markers, so
   a usage limit or crash never loses work, and a `wip/*` ref is never a candidate.
2. **Stage (a), the fast local tier.** The pre-push hook (`.githooks/pre-push`) runs the selected
   fast gates on every push of a non-`wip` ref and records its result against the pushed commit.
3. **Stage (b) and (c), CI.** The candidate is pushed to `ci/<name>` (full hook) and the CI workflow
   is dispatched on it (`gh workflow run CI --ref ci/<name>`). A dispatched run starts every job,
   the qualification-only jobs included, so one run yields both the regular verdict and the
   qualification record. Failures are diagnosed from the run's logs and repaired on the candidate
   branch; the corrected SHA is pushed and dispatched again.
4. **Landing.** When every regular job of the run is green at the candidate SHA, main is
   fast-forwarded to exactly that SHA on both remotes (`origin` and `lambdaclass`). A candidate that
   is not a fast-forward of main is rebased, which is a new SHA and needs its own run. A
   qualification job that fails or does not finish blocks the closures that need a qualified
   result, not the landing of a change whose regular jobs are green.
5. **Heavy campaigns.** Fuzzing hours, mutation campaigns and other long runs go to remote servers
   when they are available, and otherwise queue behind each other locally; their results are
   recorded against the exact SHA they ran on, like any other.

R-0513 owns making stage (a) a dependency-closure-selected fast tier within its budget and keeping
stage (c) bound to exact commits, toolchains and artifacts (Phase 1 exit 2f); R-0516 automates (c)'s
record for releases (today `qualification_record.py` is run by hand against the run IDs).

**Trains and tiers (2026-10-22, [development throughput](#development-throughput)).** Ready branches
are meant to land as one **train**: one integrated candidate, one CI run, and bisection by member only
when that run is red (R-0539). CI is split into two tiers that follow the stages above:

- the **landing tier** is stage (b)'s required job set for merge, with a target median of ≤ 40 min
  (R-0540, branch `ci-tiering`);
- the **campaign tier** is the set of jobs that run only on schedule or dispatch, the mutation
  campaign among them. It is part of stage (c), so it gates qualification but not merges.

Every result is recorded against the SHA of the candidate that actually ran, which is the train or a
bisection candidate. Attribution of a failure to a member is a separate field of that record. A
member's own SHA receives a result only from a run at that SHA, and a landed member's record names
the train candidate whose result it relies on. A member removed by bisection keeps the failure that
was attributed to it. The landing tier never stands in for stage
(c), and a stage (c) verdict still needs every job of one dispatch run. Passed, failed, skipped,
cancelled and incomplete stay distinct throughout: a cancelled or incomplete train run is not a pass
for any member.

### Browser compiler and explorer — bounded website delivery

**2026-10-22: implemented, validated, and delivered on main.** This is a small website feature using
existing compiler passes, not a new backend or a replacement for the execution
queue above. Implementation and build instructions: [browser compiler](scripts/browser/README.md).

- Run the existing compiler as WebAssembly in a browser worker, for self-contained
  files; show real diagnostics and Core → specialized Core → SSA → LLVM.
- Explain the stages, expose source-declaration links where available, retain
  completed stages on later failure, and export the result with compiler identity
  and the actual LLVM target. Successful checking is never labeled proof verification.
- Include representative checked examples, on-demand download progress, versioned
  HTTP caching, cancellation/retry, and stale-result invalidation after source edits.
- Validation: 33 native/Wasm comparisons; loader progress, integrity, five failure
  modes and retry; UI cancellation, stale messages, source invalidation and partial
  stage controls; successful site build. The local browser prototype was exercised
  and the redesigned layout reviewed from browser screenshots. Versioned compiler
  assets are included in ordinary Pages builds; deployment follows the existing site
  workflow. All four selected pre-push gates passed (not the full CI suite).
- Explicit non-goals: editor upgrades, program execution/linking, standard-library
  or multi-file loading, a Concrete-to-Wasm backend, and independent verification.

**Verifier: separate planned project.** Reuse R-0275 (independent Core-certificate
checker) and R-0353 (independent evidence consumer/schema conformance); do not
create a second overlapping checker initiative. Start from their named predicates,
versioned artifact contract, dependency firewall, trust boundaries, and hostile
fixtures. Browser hosting of such a checker is a later interface decision, not a
prerequisite for this explorer. Compiler output and a successful compile are not
certificates of semantic preservation or functional correctness.

**Maintenance:** update this entry when scope, status, dependencies, or acceptance
criteria change. Record completed delivery in the changelog; keep remaining work
and the separate verifier status here.

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

- [x] R-0484 meets its closure criteria (closed 2026-10-08 at `245cd51c`, main CI 37771982899);
  its accepted limits are listed in its task section and its mutation campaign is
  incompletely qualified at that revision; the campaign later qualified on `670ec85a`
  (2026-10-11, run 38104755470).
- [x] R-0483 rejects mismatched owners for parsed results with retained controls (closed
  2026-10-09 at `4f005715`; `check_view_lifetime.sh` 28/0 in main CI 37917463687).
- [ ] R-0333 names the exact preview subset, supported target/OS/toolchain matrix,
  known limitations and the validation required for every advertised feature.
- [ ] R-0335/R-0336/R-0337 publish a versioned description and consistent claims for
  that subset; examples and migration guidance are checked against the candidate.
- [ ] R-0334/R-0346/R-0352/R-0354 demonstrate clean installation, build/run/test and
  the tutorial on every claimed platform. Record external-user failures and fixes.
- [ ] R-0515's onboarding path, first pilot and before-public-release checkpoint
  (published inventory, named reviewers) are complete for the preview scope; the first
  pilot does not wait for R-0515's two-quarter continuing-feedback checkpoint.
- [ ] R-0343/R-0345 publish the security disclosure policy and threat model; R-0357
  locks the toolchain/dependencies and publishes the license inventory; R-0358 publishes
  the compatibility/deprecation policy; R-0426/R-0427/R-0428 are ready at preview scope.
- [ ] The preview checklist states exactly which evidence-verifier/replay artifacts ship
  (bounded R-0354 distribution); unsupported replay advertises no verification claim.
- [ ] R-0333 records a reproducible candidate and its validation result using the
  common candidate record below. Unresolved queue prerequisites remain blockers
  unless an explicit, scoped release disposition permits a narrower claim.

Finishing the first two items alone does not ship the preview. The mutation
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

## Glossary

Each term is defined once, here; the linked documents hold the full definitions.

- **Phase / area / milestone / release.** A *phase* (1 – 7) groups queue rows, has an exit and
  schedules work. An *area* (`## Area: …`) is a topical filing heading for tasks and schedules nothing.
  A *milestone* is an acceptance scope (systems preview, verified preview, verified-subset milestones,
  component alpha, project beta, 1.0 stable). A *release* is a published candidate (nightly, train or
  milestone) that passes its scope's acceptance. See [Releases](#releases).
- **Active / next phases; backlog.** The phases in progress and those that start next, named at the top
  of this file. Tasks neither scheduled in an active or next phase nor named by a queue row live in the
  [backlog](docs/project/ROADMAP_BACKLOG.md) until pulled.
- **Core.** The language definition: the typed IR elaboration produces, with an executable semantics.
  Claims and the authoritative checker work on it.
- **CheckedCore.** The immutable artifact the authoritative Core checker emits, bound to the exact
  program and its imported interfaces (R-0499, R-0514).
- **CheckedMonoCore.** CheckedCore after checked specialization (monomorphization), bound to its
  CheckedCore; the only input to lowering.
- **Place MIR.** The first lowering IR: a typed non-SSA control-flow graph with fixed evaluation order,
  match decision trees, cleanup edges and explicit runtime checks (R-0501).
- **Memory CFG.** The lowering IR where layout, offsets, loads, stores and allocation become explicit
  over block–offset memory (R-0502).
- **BIR.** The backend IR: a control-flow graph with block arguments and overflow-flagged operations.
  LLVM is printed from it and the LCB adapter consumes it (R-0503).
- **LCB.** The Lean Compiler Backend: a separately developed Lean 4 port of QBE 1.3 with a certified
  path to AArch64; Concrete's native backend and QBE path, reached through a proved adapter (R-0517).
- **Slices V1a, V1b, V1c, V2 – V7.** Cumulative verified subsets carried through the whole compiler, each
  with its own theorem. V1a: checked Core → BIR for a minimal scalar subset. V1b: that subset to native
  AArch64 through LCB. V1c: restricted source → checked Core correspondence for that subset's surface;
  V1b and V1c together are the first source-to-native theorem. V2 – V7 add aggregates and downward references, heap and linear resources, full
  reference/resource interactions, generics/traits/capabilities, raw pointers/FFI/separate compilation,
  and the rest of the maintained feature inventory. *S1 – S16* are the verified-compiler plan's
  implementation stages.
- **Assurance profile and mode.** `verified` is a versioned admission and assurance profile: a stated
  language subset, target, environment contracts and theorem boundary, rejecting unsupported code and
  never silently falling back. The existing LLVM profile stays supported and unverified. A native
  *assurance mode* is selected explicitly: certified output, or R-0517's experimental fallback.
- **Claim.** A proposition about a program's behaviour, stated over Core semantics from a typed
  `ClaimSpec`, with its scope, evidence, assumptions and status.
- **Subject binding.** What ties a claim to the exact body and parameters its theorem evaluates. Since
  bug 079 a claim is proved only when its subject is the specification its theorem is about.
- **Receipt.** A `ReplayReceipt`: an immutable record that exact retained material (subject,
  proposition, checker, dependencies, toolchain) was replayed. It is distinct from the time-relative
  policy decision that interprets it.
- **Proof model / PExpr.** The legacy proof-model expression language (`PExpr`, evaluated by
  `Proof.eval`) that claims were stated over. It is being retired in favour of claims on Core (R-0495,
  R-0496).

## Backlog and pull rules

ROADMAP.md holds the phases, the queue, and every task that a queue row names or whose **Phase:** line
includes an active or next phase (the **Active phases:** and **Next phases:** lines at the top of
this file). Every other task lives, with its full text, ID and
**Phase:** line, in the [roadmap backlog](docs/project/ROADMAP_BACKLOG.md), which owns the pull rules:

- **Pull triggers.** Each backlog task carries a **Pull trigger:** line: the workload, bug or user need
  that hits it. A trigger firing, a phase that depends on the task becoming active or next, or a release
  checkpoint needing a bounded increment of it moves the task here.
- **Phase entry checkpoint.** When the **Active phases:** line changes, every backlog task is reviewed and
  either pulled here, kept with a recorded **Kept in backlog:** reason, or retired to the archive with a
  reason.
- **Per-release review.** Each release candidate reviews the backlog the same way.
- **Enforcement.** `scripts/tests/check_roadmap_coverage.sh` gates one home per task, pull triggers,
  placement against the active and next phases, and a backlog review for the current active phases.

## Decisions

The dated decisions that shape the queue. Each names the tasks that carry it; none creates another
queue.

**Priority decision, 2026-10-21:** the verified compiler is the primary engineering track, protected
by an explicit [capacity allocation](#capacity-allocation) rather than by queue numbering. Two inputs
were weighed. An external review asked for exactly that, in the order: close known soundness defects
and fix the semantics proofs rely on; make the Core checker authoritative with explicit identities and
ownership/capability rules; V1a; V1b through the backend to the native image; then grow the subset
from real programs, with one small executable workload alongside and evidence integrity as a second
protected track. Separately, point repairs are not converging: probes found bugs 102 – 120 in about
two days, nearly all of them new instances of [structural classes](docs/bugs/STRUCTURAL_CLASSES.md)
whose owners are verified-pipeline Part 1 stages. Adopted: the capacity table (verified compiler at
least 55%, evidence integrity at least 15%, adversarial validation at least 10%, features at most 20%
in a single slot); the [defect disposition rule](#the-current-execution-queue); Part 1's structure as
row 4's migration track once row 0's in-flight candidates are accepted, extended from identities and
the checker to one `Layout` (R-0502) and Place MIR (R-0501) in the existing profile, because classes 8
and 12 produce wrong code in the profile users run today; V1c as a named deliverable, so that a
Core-to-native theorem is never presented as source-to-native; bounded scope for the ethlambda port,
`std` breadth and tooling until V1b. Not adopted, with reasons: (1) repairing only memory-safety and
wrong-code defects: false proofs and unsound claims are as silent, and the evidence is the product, so
they are stopped at once too; and a silent defect may be contained fail-closed instead of repaired
when that is smaller. (2) Moving rows 9 – 11 ahead of the slices: every published V1 claim already
requires their bounded increments (minimal task identity, the independent retained-artifact consumer,
final-image binding), and the complete cross-profile consumer and the receipt freeze have no consumer
before V1b; track B's protected share keeps those increments from being starved instead. (3)
Whole-language Place MIR and layout proofs now: the structure moves now, and its semantics and proofs
stay per slice, as the vertical-slice decision requires. (4) Pausing feature work: RocksDB slice 1
continues in its slot, because executable workloads are how defects surface. A second round of
external advice, on day-to-day engineering, is attached to existing owners as acceptance milestones.
Adopted: a development-speed baseline (clean build, one-function-edit rebuild, peak compiler memory,
test turnaround, recorded per main commit) that must exist before row 4's first structural change lands
(R-0355, now named by row 4); a clean-machine package exercise with dependency upgrade and roll-back
(R-0285, R-0365); a shared fault-injection checkpoint for runtime, `std` and FFI, exercised composed in
RocksDB slice 1's companions (R-0030); a practical debugging checkpoint, in which a non-author diagnoses
a runtime trap and a failing foreign call, required before workloads expand beyond RocksDB slice 1
(R-0515 with R-0281); a usable `std` subset named from the first two workloads and validated for
ownership, errors and the foreign boundary together (R-0021, R-0488); and pilots that measure
maintenance across a compiler and a dependency upgrade (R-0515). Two placements differ from the advice.
The upgrade-and-roll-back half of the package exercise closes the four-adapter checkpoint rather than
RocksDB slice 1, so the slice stays small; slice 1's acceptance already covers install to test. The
debugging checkpoint does not gate slice 1, as the advice itself says. Concurrency, a public registry
and additional backends already wait in the backlog behind workload triggers (R-0399, the registry
task, the Wasm and freestanding targets); broad `std` expansion in this file (R-0031) is now bounded
the same way. No new task ID was needed. No exit or acceptance criterion is removed; exits assigned
before this date keep their obligations.

**Semantics decisions, 2026-10-21:** a list of ten language-semantics proposals tied to bug records and
four items of external advice were evaluated against probes of main (both consumers) and corpus
counts, and every recommendation was adopted as evaluated, each written into its canonical document
with its status on main, enforcing site, the bugs it prevents and its owning exit. **Adopted:**
references exist only as parameters, call arguments, borrow-block binders and arm-scoped pattern
binders, no reference binding is introduced by `let` and none is ever assigned (needed because
reference parameters are assignable today), and constants are values (MEMORY_SEMANTICS §15; exit
2i); one binding per visible scope chain, as SCOPE-1 already specified (exit 2i); the heap one-hop
field rule as a representation rule (MEMORY_SEMANTICS §5; bug 126's repair in exit 0b, R-0501);
evaluation strictly left to right in source order, with struct and enum literal fields in source order
and assignment evaluating its right-hand side first as the one exception (STATEMENT_EXPRESSION_MODEL;
exit 2j); explicit consumption before replacing a live resource (MEMORY_SEMANTICS §10); layout
unspecified unless a `repr` guarantees it, observed only through `sizeof` / `alignof`, with one
`Layout` authority (LAYOUT_CONTRACT §3.3; R-0502); every Concrete-defined function in a mangled
namespace disjoint from C, with `#[link_name]` for imports and an export attribute for the reverse
(FFI.md; R-0285, R-0283, R-0286, R-0510); ownership on failure stated by every consuming fallible
`std` and foreign-boundary API (ERROR_CONVENTIONS; R-0285, R-0283); recoverable versus terminal
failure restated (FAILURE_STRATEGY). **Adopted, staged:** `Option` and `Result` laid out per
instantiation now (bug 122, exit 0b) and referred to only by resolved identity later (R-0492,
R-0514; LAYOUT_CONTRACT §3.4); lossy numeric `as` casts become **compile errors** — the user's choice
over a trapping `as` — reached through named operations first (checked conversion returning an error,
or truncation, reinterpretation and rounding with exact bit semantics), then the rejection with
versioned migration diagnostics, then the corpus migration (ARITHMETIC_POLICY §8; exit 2k, with exit
2h centralizing today's cast semantics without waiting). **`main`:** `fn main() -> Int` stays, and
parameters on `main` are banned now (bug 111, exit 0b). **Open, not adopted:** whether `main`'s
return type narrows to MAIN_EXIT_MODEL's `fn main()` / `fn main() -> u8`; that end state stays a
recorded preference until a later surface pass decides exit codes. **Stance:** reference restrictions
are not relaxed and nothing is destroyed implicitly; the relaxations the decision names (immutable
scoped `let` references or place-based borrow blocks; owner reads under a shared borrow) wait for a
workload and would be new rule versions. Two items are pull-gated with no owner yet: fallible
allocation (`try_*` collection operations and a non-aborting allocator path) and a general place
`replace` / `swap`. The evaluation's probes also found defects, filed as bugs 127 – 131 (exits 0b and 2c), and
showed stale documentation (the wrapping-overflow text in NUMERIC_MODEL, `--report profile`'s
wrap-gap line, the removed `->` heap syntax), corrected with the decisions. No exit or acceptance
criterion is removed; exit 2i's acceptance and exits 2j and 2k's scopes are made precise.

**Tightening decisions, 2026-10-23:** the user approved selective tightening (a rule lands when it
closes a soundness hole or removes a case from the semantics the verified pipeline must formalize;
style-only rules wait for a workload) and answered the four open questions of
[TIGHTENING_DECISIONS.md](docs/language/TIGHTENING_DECISIONS.md) with "do the best long term
solution for this problem and every problem". **Adopted:** every `&mut` argument is an implicit
reborrow and an unused `&mut` may lapse, as an exclusive scoped loan that suspends its parent (D1,
G9 / B1 / B2; R-0496 with exit 2i); the Austral model, "ok do the austral model with `&s.f`
sugar": only a variable (or a string literal, a static root) is borrowed, a reference to a part is
a projection through a reference, `&s.f` is front-end sugar, and loans are keyed by root, so a
borrow of a temporary is refused and disjoint field borrows wait for a workload (D2, REF-5, G12;
closes bug 182; 2 std sites and 1 train3 fixture migrate); every binder through a scrutinee of
reference type is a reference, Copy or not, carried explicitly through Core (D3; closes bug 181; 2
fixtures migrate; G17's Copy case reopens under it); every foreign function value is refused now
(bug 166), and function types will carry calling convention, capabilities and safety obligations
when the first callback workload needs them (D4, [FFI.md](docs/platform/FFI.md)). G17's moving
shapes are closed (refused on main, pinned by fixtures). The ordered implementation plan is TIGHTENING_DECISIONS §5; exit 2i owns D1 –
D3's checker rules and exit 0b the bug repairs.

**Owner decisions, 2026-10-25:** the project owner paused the 2026-10-23 tightening decisions for
negotiation, together with further language questions raised since (eighteen in all, OD21 from bug 262 and OD22 from bug 189), the
PROVABLE_V1 scope and bug 226's std visibility question. They are listed under
[owner decisions](#owner-decisions) and held by exit 2n. Only the owner decides them: agents analyse,
recommend and ask, and do not choose, encode or implement an answer. The entry above and
TIGHTENING_DECISIONS.md stay as the record of what was proposed and landed; the owner's new answer
governs. Rules already on main stay as they are until the owner answers, and the answer decides
whether they stay.

**Delivery decision, 2026-10-17** (amended 2026-10-19: interop runs alongside the verified-pipeline track after the immediate protections; see the vertical-slice decision below; amended 2026-10-20: RocksDB slice 1's work proceeds now and is accepted once its [prerequisite exits](#feature-prerequisites) hold, not after all of rows 0 – 2)**:** comfortable C/Rust/Zig interop is the next delivery investment
after the active soundness/identity work. R-0285 owns the integration workflow; the existing ABI
and export tasks supply the tested subset it needs. R-0490 then uses a complete ethlambda port
to find language, library and tooling gaps through execution. Bound the interop checkpoint to
its four acceptance adapters; further dependencies are integrated when a port slice needs them.
The application contract is dependency -> ordinary Concrete import -> normal build/test.
Reusable binding packages describe ownership, errors and authority once; generation and native
builds are tooling responsibilities. The [interop design](docs/platform/FFI_GLUE.md) defines the
shared boundary description, language-specific front ends, cache/upgrade behavior and exit bars.
Compiler safety, protocol conformance and regression testing remain continuous. Functional
proofs of client components follow the working port, through the evidence and contract owners
below. New soundness defects still enter at the front. The port is an engineering workload,
not a fourth verified flagship or a claim that foreign libraries have been proved.

**Soundness track, 2026-10-18 (now queue rows 2, 3, 4 and 6):** the evidence layer is now
strong — every theorem is re-checked outside Lean by con-leche and nanoda, and since bug 079 a claim is
proved only when its body and parameters are the specification its theorem evaluates. The gaps that
remain are recorded here so the queue does not read as if rows 0–1 settle soundness:

- **Core→VC soundness (H19, R-0460).** Each obligation-generation rule, including the soundness of
  its hypotheses, requires the named evidence recorded in `VC_BRIDGE_REGISTER.md`; proving claims
  on Core does not discharge these rows.
- **Model and compiler correspondence ([verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md)).**
  `PExpr` is retired in favour of claims on Core's semantics (R-0495, R-0496); the lower pipeline
  gets a semantics per IR (R-0501 – R-0503); translation validation of the LLVM profile is pulled
  forward (R-0504 with R-0299/R-0300); compiler correctness and proof transport to native binaries
  are R-0508. Until then each relation reports its admitted subset, and unvalidated backend and
  toolchain edges remain trusted.
- **The checker's own rules** — linear ownership, second-class references, capabilities, the Unsafe
  boundary — are enforced and gated but have no soundness proof; R-0499 moves them onto Core as
  declarative rules and R-0507 proves them.
- **Coverage classification and loop binding:** R-0495 owns checked proposition coverage and exact
  checked-Core loop binding. Unsupported classification or missing loop binding cannot grant proof
  authority.
- **Trusted std boundaries** (`trusted impl Bytes`, IO and others) rest on human audit; claims reaching
  them say "carries trust".
- **The bug ledger's open list** has entries later work appears to have closed (R-0010 owns
  mechanically checked per-bug states).

**Verified-compiler decision, 2026-10-18:** the goal is an end-to-end verified compiler —
CompCert/CakeML-class theorems that accepted programs are memory- and ownership-safe, that the
native binary only does what the program's Core semantics allows, and that supported Core claims
transport to the native image through proved transport lemmas under recorded premises. Hunting after bugs 079 and 080 found fifteen more (081 – 095) in an
afternoon, and they trace to the pipeline's structure: the checker runs on the source tree and
returns nothing the compiler uses, the language's meaning is implemented several times, names are
strings after resolution, evidence is computed per surface, and SSA and the backend have no
semantics. The [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md) (R-0491 – R-0510)
reconciles two independent architecture designs and Codex's reviews into one target: Core as the
language definition with an executable semantics, the authoritative checker on Core, `PExpr`
retired in favour of claims on Core, a Place MIR → Memory CFG → BIR lower pipeline each with a
semantics, LLVM behind a specified, unverified BIR-to-LLVM adapter, and native AArch64 code generation
delegated to the separately developed Lean Compiler Backend through a proved adapter (R-0517). Part 1 establishes semantic interfaces, states obligations and proves
representative cases before dependent implementations freeze; Part 2 completes proofs for the
admitted profile (delivered as vertical slices since 2026-10-19: rows 2, 3, 4 and 6). Every
slice is reviewed by Codex before publication, the proof surface is feature-frozen until the
verdict authority and claims-on-Core land, and the fixes for 081 – 101 land first (row 0).

**Usability decision, 2026-10-19:** a great language needs real users, good errors, tooling, a
reliable `std` and published performance, not only a sound compiler. Usability moves from the end of
the queue to a track beside the proofs (row 8, R-0515), in waves under the capacity rule (since 2026-10-21 the
feature track of the [capacity allocation](#capacity-allocation)); the external
user pilot that row 16 required at the end now starts after rows 0–2 qualify and the onboarding path is ready.

**Vertical-slice decision, 2026-10-19:** deliver the verified compiler as cumulative proved subsets
through the whole compiler, with explicit intermediate and source-to-native theorem boundaries, rather
than layer by layer, which would postpone any end-to-end guarantee — and the first real test of the
architecture — by years. Before shared interfaces freeze, representative ownership, borrowing and
memory-representation cases are proved. Frontend correspondence and safety proofs grow with the
admitted language. One converging compiler architecture carries distinct assurance profiles (`verified`
and the existing one), and the LLVM profile stays supported for production use. The proof track, the
shared front end (row 4), and the bounded interop and application-feedback tracks proceed alongside one
another after the immediate protections (amended 2026-10-20: each first deliverable waits only for its
[prerequisite exits](#feature-prerequisites), not for all of rows 0 – 2), with explicit capacity for each
(amended 2026-10-21: the [capacity allocation](#capacity-allocation));
new authority defects take priority. Legacy claims migrate when their Core semantics and evidence boundaries are ready, preserving
supported replay and exact assurance labels. Property gates replace brittle corpus cardinalities while
keeping independent coverage inventories and minimum completed comparisons. No multi-year proof
milestone suspends supported releases or external-user feedback. The [plan](docs/compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices)
holds the milestones and the discipline (proof engineering, Lean scalability, compiler-execution trust,
ownership and specialist review, specification validation, release strategy).

**Implementation and feedback decision, 2026-10-17** (amended 2026-10-19 and 2026-10-20)**:** once RocksDB slice 1's prerequisite exits hold ([feature prerequisites](#feature-prerequisites); the rest of rows 0 – 2 continues alongside), start R-0285 with a working RocksDB C-API consumer and the
minimal integrated package/build code it needs. Prepare concrete manifest/lockfile
examples and only the resolution decisions needed by that slice, then implement,
run and measure it. Each later adapter repeats that feedback loop. Full registry,
range-resolution, schema breadth and functional-proof work are not entrance
requirements for this tested slice; their existing acceptance bars remain owned.
R-0285's four-adapter exit still precedes the bulk R-0490 port.

**Behavioral-contract decision, 2026-10-18 (planned):** make failure states,
interface laws and the exact meaning of proof claims explicit through the existing
API, contract and evidence machinery. R-0285 exercises resource recovery and
partial progress in the first applicable foreign-library slice; R-0488 carries
the same questions into ordinary library APIs. R-0031 owns a bounded equality/hash
law example, R-0271 owns its generic/instance proof meaning, and R-0482 owns checking
the claimed property against the actual theorem proposition. Their acceptance
criteria below extend those owners; they create no additional execution queue.
Start with ordinary types, explicit contracts and selected proofs. Add syntax or
type-system machinery only after a real workload establishes the need. Tested API
behavior can ship with the interop slices; formal error-state and generic-law
composition follow the queued typed-contract and evidence prerequisites. These
increments do not expand rows 0–1's recorded exits; newly reproduced false
guarantees still enter through the existing soundness-repair rule.

**Emitted-code validation analysis, 2026-10-18 (planned):** the
[Sparkle review](research/compiler/sparkle-emitted-code-validation.md) records
the existing SSA/LLVM foundations and a bounded check of actual emitted LLVM,
beyond optional `llvm-as` well-formedness checks. R-0299/R-0300 own that additional
semantic edge and its independent replay; their existing BackendIR V1 relation
keeps its scope. The small Lean 4 native backend this analysis anticipated is now
the separate Lean Compiler Backend, which Concrete consumes under R-0517 (R-0297
delegated); R-0413 keeps only the later Concrete-hosted comparison. Implementation
language alone supplies no correctness guarantee. Fresh-consumer checks join R-0285; deeper backend research does not
reorder the queue or delay its first executable dependency slice.

R-0484 (closed 2026-10-08 at `245cd51c`), R-0483 (closed 2026-10-09 at `4f005715`) and R-0208
(closed 2026-10-13 at `2b570c5a`) moved to [CHANGELOG.md](CHANGELOG.md); their accepted limits stay recorded in their task sections.

`ProofCache` remains performance-pulled. A second proof-producing kernel remains research-gated and
is not part of this strict queue. Why3 remains comparative prior art, never a backend.

## Reconciliation record

The [2026-10-19 reconciliation](docs/project/ROADMAP_RECONCILIATION.md) records historical task dispositions and the four prose inventories, including their source revision. The current queue and live task records own scheduling and acceptance. Transferred obligations remain binding unless explicitly retired with a recorded reason.

Every `### Task R-NNNN` carries a **Phase:** line; a superseded, merged, delegated, retired or done task also carries a **Disposition:** line naming its owner, and its row in the record's task-disposition table. `scripts/tests/check_roadmap_coverage.sh` reads that table and checks that every task has a phase or a disposition, that every disposition row and queue row names an existing task, that every queue row names an owner, and that no task's phase contradicts its queue row.

### Binding reconciliation dispositions

The following dispositions replace conflicting implementation directions in the named tasks.
Existing regression cases and acceptance obligations survive unless explicitly retired with a reason.

- R-0114 merges into R-0492/R-0514; R-0116 merges into R-0497/R-0510/R-0514.
- R-0123, R-0163 and R-0164 target checked Core, generated ClaimSpec statements and
  Core-based composition under R-0495/R-0512. They add no new PExpr/FnTable mechanism.
- R-0176's forward ProofCore architecture is retired. R-0512 owns any isolation needed
  to preserve historical replay.
- R-0202, R-0262 and R-0269 retain legacy correctness maintenance only. New dependency
  closure and statement-generation work belongs to R-0495/R-0509; a PExpr bridge is
  permitted only as an explicitly proved R-0512 migration bridge.
- R-0268's architecture decision is settled by R-0496.
- R-0274 preserves a single authoritative checker over Core under R-0499/R-0514.
  Source diagnostics and elaboration do not replace its typing, ownership or capability rules.
- R-0278/R-0288 supply the validated BIR contract under R-0503, not an additional
  competing IR. R-0295's decision is settled by R-0501/R-0502.
- R-0464 retains the shared trap-definition and fail-closed analysis requirements.
  R-0460 owns VC generation proved sound against Core. Any SSA-based analysis needs
  an explicit proved relation to Core and must account for stores, aliases and calls.
- R-0408/R-0409 evaluate instrumentation and proof-engineering improvements to normative
  Core semantics under R-0496; they do not establish another language evaluator.
- R-0446's generated trap/fuzzer/consumer coverage is an increment of R-0491/R-0493.
- R-0297 is delegated to the Lean Compiler Backend; R-0505 integrates LCB and builds no Concrete
  machine backend; R-0113 records that LCB is Concrete's QBE path (R-0517).

R-0460, R-0299/R-0300 and R-0353 are not redundant with compiler correctness: VC soundness,
LLVM-profile validation and independent evidence consumption retain distinct obligations.

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

## How To Read This Roadmap

Decision and delivery status use this lightweight key. These labels describe lifecycle, not proof
strength, and therefore do not form another assurance ladder:

- **[ratified]** — the design decision is made; implementation may still be absent;
- **[shipped]** — the implementation is complete through its stated production gate;
- **[pending]** — active or queued work whose exit condition is not yet met; and
- **[historical]** — superseded material retained only for rationale or traceability.

### Capability Unlocks — The Stable Product Map

The task catalog below is deliberately detailed and the execution queue changes as defects are
found. This table is the stable layer above both. It answers a different question: **which task
clusters combine into a user-visible category change?** Area headings remain topical filing
labels; this table is neither a second queue nor permission to skip prerequisites. When queue,
area position and this map appear to disagree, the current execution queue decides *when*, and
the exit artifact here decides *what the work must ultimately unlock*.

| unlock | principal owners | exit artifact / user-visible boundary | weaker path retired | state |
|---|---|---|---|---|
| Trustworthy `proved` | R-0004, R-0440 | every friendly verdict requires a current exact subject, complete dependency root, explicit trust/assumptions/environment and successful replay receipt; migrated corpus survives independent red-team pressure | V1/body-only friendly status, advisory dependency freshness, receipt-free `proved` | **R-0004 CLOSED 2026-08-20** (205/205 protected serial run; see the closure record below) |
| Typed evidence and revocable policy | R-0440, R-0208, R-0343 | source correspondence, logical validity, dependency closure, immutable replay receipt and current policy decision are distinct typed facts; checker advisories degrade only the dimensions they can honestly scope | flat status/digest strings, advisory state baked into historical evidence, whole-report reminting | **ratified / implementation pending** |
| Typed compositional contracts | R-0473, R-0474, R-0477; the proof-authoring area frame/composition work | a nontrivial multi-function program is proved without callee inlining; caller establishes preconditions and imports postconditions; mutation is stated with `old` plus frame/`modifies`; one contract crosses an interface artifact | erased/name-keyed contracts, textual substitution, per-call inlining as the only composition story | **queued after identity/task/verifier stabilization** |
| Total contract language | R-0473, R-0474; the proof-authoring area termination/spec-library work | `spec fn` and checked `total fn` are the only contract-callable definitions; effects, traps, unchecked recursion and trusted execution refuse explicitly | purity-shaped admission of executable functions whose totality/meaning is unestablished | **design required** |
| One faithful verification pipeline | R-0450, R-0454–R-0456, R-0460; the soundness-bridge area | one typed neutral obligation IR and semantics feeds every prover; Core→obligation register rows are discharged or explicitly trusted; independent evaluator/certificate checks cover the admitted boundary | duplicated walkers/renderers and backend-specific meanings of one obligation | **partial / H19 open** |
| Portable multi-kernel evidence | R-0448, R-0454–R-0456, R-0440 | Lean/Rocq/Isabelle receipts bind one neutral obligation identity and report checker, foundation, independence and unsupported fragments separately | coordinator-emitted aggregate badge and agreement over unbound/mismatched propositions | **experimental until prerequisites graduate** |
| Practical proof authoring | The proof-authoring area | stable obligation artifacts, minimization, usable stubs/scaffolds, reusable lemmas, structural/operational automation, bounded specs and replay-checked repair on a real workload | hero proofs reconstructed manually from compiler internals | **planned** |
| Auditable safety and resource guarantees | R-0445; the audit-commands, proof-status, provable-subsets and runtime-safety areas | a named profile gates runtime failures, authority, stack/allocation/resource facts and assumptions through one ledger and audit bundle | reports that describe risks without controlling release policy | **partial** |
| Independently checkable compiler/evidence artifacts | The soundness-bridge area, R-0275, R-0353 | an offline checker validates a versioned Core/evidence predicate and evidence DAG while naming every remaining compiler/backend-trusted boundary | “trust the compiler report” as the only verification route | **planned** |
| Non-vacuous assurance operations | R-0208, R-0343; testing/diagnostic hardening tasks | shared assertion primitives, exact declaration denominators, advisory/revocation drills and production-consumer mutations make false-green gates and unmeasured regions visible | hand-copied counts, absent anchors passing, registered-but-unrun mutations, first-error measurement masking | **partial / consolidation required** |
| Usable systems platform | The stdlib area, the QBE backend-validation area, the backend-contracts through freestanding areas | real stdlib workloads, independent backend validation, explicit FFI/Unsafe boundaries and a freestanding target retain the same evidence vocabulary | proof demos that cannot build ordinary or low-level programs | **broad foundation partial** |
| Proof-aware local packages | The incremental-driver area plus the pre-release core of the packages area | manifest, lockfile/workspace, interface/body split and locally verified dependency certificate reject evidence/trust/authority weakening before artifact formats freeze publicly | single-repository proof attachment and source-private dependency inspection | **must precede beta/release** |
| Narrow defensible release | The release-bar area | a fresh user installs Concrete, builds a useful project, inspects and independently replays its bundle, and can read the exact supported subset/compatibility promise | repository-local success and claims that require project-author interpretation | **planned** |
| Ecosystem packages and editor experience | remainder of the packages and editor areas | evidence-typed imports, package publishing/provenance and an incremental editor expose the same facts as CLI/CI without a second truth source | manual dependency audits and batch-only proof feedback | **post-foundation** |
| Relational/concurrent/research extensions | The research-extensions area and explicit forcing tasks | only a workload-backed semantics graduates; constant-time/non-interference use paired execution, concurrency uses a mechanized memory model, and every unsupported observation remains named | treating source-shape checks as relational proof or adding concurrency before its evidence model | **research-gated, not a near-term promise** |

The long-term product chain is therefore:

```text
trustworthy local evidence
    → typed compositional contracts
    → one faithful verification pipeline
    → independently replayable artifacts
    → proof-aware package boundaries
    → CI/editor enforcement of behavioral compatibility
```

The cross-cutting dependency order inside those unlocks is:

1. qualify the stronger post-R-0004 mutation campaign without treating interruption, inapplicability,
   build failure, or a pre-existing red gate as a kill;
2. run the Lean #14576 upgrade/revocation fire drill without blessing unexplained proof drift;
3. preserve the Concrete-owned separation between verification task, proof attempt, mutable proof
   session, immutable replay receipt, time-relative policy decision, and disposable proof cache;
4. introduce the remaining typed digest/identity boundaries and five evidence objects in shadow;
5. version and freeze the canonical `VerificationTask` envelope and the canonical VIR proposition
   bytes it names for their explicitly admitted fragment, not the compiler's internal `TermIR`;
   unsupported contract/state constructs refuse or require a new schema rather than freezing an
   incomplete internal representation;
6. keep evidence-affecting language features behind the post-R-0004 identity/task/verifier seam;
   then land the typed contract substrate, `old`/frame/`modifies` semantics for a narrow two-state
   record and array model, ghost locals and parameters, `#[decreases]` and the checked
   total-function category, the spec standard library, and only then richer/relational logic;
7. land declaration-isolated measurement and the shared non-vacuous assertion library;
8. build R-0353 Layer 0, then add certificate profiles only as their probes succeed;
9. shadow-compare compiler and independent verifier, export proof-aware package interfaces, and
   complete independent hostile review before the verifier becomes authoritative; and
10. expand heap, resource, and relational support only on those foundations.

This is a dependency chain, not a second execution queue: urgent correctness defects may insert
ahead of it, and the current execution queue remains the owner of current work.

### Ratified evidence ownership and backend decisions

- **[ratified] Concrete owns the evidence lifecycle formats.** `VerificationTask`, `ProofAttempt`,
  `ProofSession`, `ReplayReceipt`, `PolicyDecision`, and `ProofCache` remain distinct. A session is
  mutable development history; a receipt is an immutable statement that exact retained material was
  successfully replayed; a policy decision is the current interpretation of that receipt against
  freshly recomputed source/dependency/advisory facts; a cache is disposable acceleration and never
  evidence. R-0004 need not ship a general session UI, but it must not collapse these authority
  stages while wiring replay-backed receipts.
- **[ratified] Why3 is a comparative reference, not a supported backend.** Why3 remains useful prior
  art for VC generation, transformations, prover dispatch, obsolete attempts, and replay. Concrete
  will not promise WhyML export, consume Why3 sessions as authority, add a Why3 build/runtime
  dependency, or make Why3 drivers/results part of a friendly verdict. Concrete's evidence model
  requires ownership of task, attempt, receipt, and policy formats; outsourcing them to Why3
  sessions would reopen the portability and trust-collapsing boundary R-0004 exists to close.
- **[ratified] The public replay/evidence compatibility boundary is a versioned canonical
  verification task, whose proposition boundary is canonical VIR—not internal `TermIR`.** Internal
  compiler representations may evolve while producing equivalent canonical tasks and VIR. Task
  identity binds the exact subject and proposition, admitted contract/hypothesis and state-model
  identities, and the generator/rule-set identity.
- **[ratified] Receipt transport does not own receipt semantics.** Concrete defines typed receipt
  fields and canonical semantic bytes. The intended external wrapper is an in-toto Statement and
  the intended signature envelope is DSSE; neither JSON field order nor the wrapper becomes an
  identity producer. Signing is not an R-0004 closure requirement. A SLSA-style verification
  summary corresponds to a retained `PolicyDecision`, not to the immutable `ReplayReceipt`.

### Architecture-preparation closure

The architecture package is not prepared merely because its two normative documents exist. Before
implementation relies on them, close these documentation and drift boundaries:

1. **Ratified decisions:** add durable `docs/project/DECISIONS.md` entries for the five identity
   domains and the separate evidence dimensions and lifecycle artifacts; the task/attempt/session/
   receipt/policy/cache separation; immutable receipt versus time-relative policy decision; Concrete
   ownership of the evidence formats and Why3 non-goal;
   whole-checker fail-closed revocation; VIR as the portable proposition inside the canonical
   verification-task envelope; in-toto/DSSE as transport rather than semantic owner; explicit
   proof-authoring modes; and the checked `total fn` contract-callable category. Each entry names
   rejected alternatives and what evidence would justify reopening it.
2. **Historical record:** add a `CHANGELOG.md` entry labelled design/roadmap only. It must state
   that typed evidence, VIR v1, proof modes, the standalone verifier, total functions, and advisory
   policy are targets rather than shipped guarantees.
3. **Contradiction audit:** search all docs, examples, generated help, snapshots, and release copy
   for claims equivalent to “kernel checking adds no trust,” “two kernels remove kernel trust,”
   “a certificate makes another checker unnecessary,” “advisory state is a receipt fact,” “pure is
   sufficient for contract calls,” TermIR and VIR described as parallel semantic owners, or an
   unscoped `verified`/`proved`. Every live hit is corrected, explicitly historical, or linked to a
   current-schema boundary.
4. **Mechanically executable architecture drift:** add source/docs gates for the subset a machine
   can decide: normative links name TermIR as VIR's semantic owner; `CLAIMS_TODAY` target sections
   retain direction/not-shipped markers; replay-receipt types contain no advisory disposition;
   retained policy decisions bind policy/advisory identities; authoritative digest families use
   distinct types at migrated boundaries; and R-0004/R-0353 retain producer/consumer links. Each
   rule has a concrete source/doc predicate, a positive control, and a run mutation. Do not claim
   this proves semantic single ownership.
5. **Manual architecture audit:** review module imports, constructors, call sites, codec ownership,
   and deletion plans for duplicate semantics that textual gates cannot decide. Record reviewer,
   commit, inspected owner set, conflicts/refusals, and follow-up task. A judgment-heavy statement
   such as “there is one semantic owner” belongs here, not inside a grep gate whose acceptance can
   be argued until disabled.
6. **No prose-only completion:** closing this block updates the decision log, changelog, drift gate,
   mutation registry, documentation index, and roadmap together. It does not advance any
   implementation authority state.

After the post-R-0004 mutation checkpoint and R-0208 fire drill, implementation proceeds in bounded
migrations: remaining typed identity wrappers; five evidence objects in shadow; structured old/new
comparison; the already-ratified
receipt/policy separation; versioned canonical `VerificationTask` and VIR types/bytes; explicit
proof-authoring modes; checked total contract functions; then R-0353 Layer 0. Each increment
migrates one real consumer, kills a production mutation, records the corpus denominator, and
deletes its flattened predecessor only after shadow agreement.

**Release/evidence blocker lifted (2026-10-13):** R-0208's Lean #14576 fire drill closed at
`2b570c5a`. New evidence is minted only under an accepted measured build of Lean 4.34.1; native-trust
receipts stay withheld (a deferred feature, not a closure criterion).

### Standard Graduation Contract

Every major unlock above uses the same graduation shape. A task may specialize it, but may not
silently omit a category:

1. **Positive control** — the intended case succeeds through the production consumer.
2. **Negative control** — the nearest invalid or unsupported case fails/downgrades precisely.
3. **Non-vacuity control** — the fixture, anchor, denominator and produced artifact are present.
4. **Class-level regression** — the gate covers the bug class, not only its first witness.
5. **Production mutation** — weakening/removing the property is killed where the user-visible
   verdict is decided; a helper-only mutation is necessary at most, never sufficient.
6. **Exact denominator and named holes** — skipped, unsupported, refused and unmeasured cases are
   counted rather than disappearing from the numerator.
7. **Reproducibility** — repository/project-root, clean-checkout and relevant cache/toolchain
   variations either agree or produce an explicit non-current disposition.
8. **Independent hostile review** — at least one non-author attack packet for evidence-critical
   unlocks; every successful attack becomes a permanent fixture.
9. **Claim/docs convergence** — capability map, public claim, known-hole disposition and
   shipped/experimental label change together.
10. **Old-path deletion** — the task names the weaker implementation/status/schema it retires.
    Two paths may coexist only during an explicit shadow comparison with an owner and exit gate.

Evidence-critical gates use one shared assertion library. Absence checks require a present anchor;
negative suites require a positive control; joins report exact denominators; mutation state is one
of `registered`, `executed`, `killed`, `survived`, `inconclusive`, or `vacuous`. Canonical status
counts are generated or checked from machine output, never accepted because a commit message copied
them. Audit remaining bespoke gates—including `test_prove_cli.sh`—before calling this contract
repository-wide.

Progress is recorded by stage, not a single percentage:

```text
representation → shadow-measured → production-consumed → corpus-migrated → red-teamed
```

“The type/helper exists” never implies “the product guarantee exists.”

### Area Index — Where Detailed Work Lives

Use this index to navigate by concern. It summarizes ownership; it does not duplicate task state
or create another queue.

| area | detailed home | capability it serves |
|---|---|---|
| Evidence integrity, receipts and status composition | R-0004, R-0440, the proof-status area | trustworthy `proved`, replay and policy |
| Checker advisories, revocation and hostile evidence | R-0208, R-0343, R-0353 | current policy acceptance without rewriting replay history |
| Contract language, identities and VC calculus | R-0473, R-0474, R-0477, R-0450 | typed compositional contracts and one obligation meaning |
| Canonical VIR and certificate profiles | R-0450, R-0451, R-0463, R-0353 | portable propositions and small independent replay |
| Diagnostic isolation and non-vacuous gates | R-0473, testing hardening tasks | truthful denominators and assurance controls |
| Language/stdlib usability and workload pull | The stdlib area | real programs without hidden authority or ownership shortcuts |
| Independent backend and differential validation | The QBE backend-validation area (reconciled), R-0517, R-0491 | LLVM/LCB-native independence and miscompile detection |
| Flagships and cross-feature forcing examples | The flagships area | proof that features survive realistic composition |
| Incrementality, artifact store and query graph | The incremental-driver area | scalable builds, proof caching, packages and editor service |
| Proof authoring, automation, frames and spec library | The proof-authoring area | affordable compositional proof work |
| Audit artifacts, semantic diffs and trust gates | The audit-commands and proof-status areas | reviewable assurance and release enforcement |
| Named subsets and runtime/resource obligations | The provable-subsets and runtime-safety areas | precise `ProvableV1`/`PredictableV1` and absence-of-runtime-error claims |
| Compiler/VC soundness and independent certificates | The soundness-bridge area | reduction of the source/Core→claim trusted bridge |
| ABI, FFI, unsafe islands, optimization and targets | The backend-contracts through freestanding areas | low-level, embedded and mixed-language systems |
| Public compatibility, distribution and operations | The release-bar and post-release areas | a supportable external promise |
| Package/dependency evidence | The packages area | behavioral compatibility across dependency boundaries |
| Editor, agent and migration tooling | The editor area | the same evidence facts where developers work |
| Concurrency and semantic research | The research-extensions area | workload-gated extensions without premature claims |

### Decision And Status Vocabulary

The roadmap has one authority ordering:

1. **Safety/claim invariants and completion gates** decide what may ship.
2. **The current execution queue** decides what work happens next.
3. **Capability unlocks** define the user-visible destination and deletion boundary.
4. **Phase/task position** is topical organization only.
5. **Historical notes** explain why a decision exists but never override a newer dated status.

Use these state words consistently:

| state | precise meaning |
|---|---|
| `planned` | scoped, not implemented |
| `blocked` | exit requires a named external prerequisite; other useful work is exhausted |
| `active / representation` | types or algorithms exist, with no production consumer claim |
| `active / shadow` | production facts are measured but forbidden from improving a verdict |
| `active / production` | the real consumer uses the fact; migration or hostile validation remains |
| `migrated` | the exact existing corpus/artifact population has moved with no silent skips |
| `closed` | production-consumed, migrated where applicable, graduation contract satisfied, weaker path retired |
| `experimental` | useful spike whose result must not appear as a supported claim |
| `research-gated` | no implementation commitment until the recorded forcing trigger fires |

`landed`, `implemented`, `green`, and `can mint/render/compute` are observations, not completion
states. When used, they must be qualified by one of the stages above.

### Cross-Phase Forcing Artifact

The roadmap needs one workload that pressures the whole chain rather than letting every phase
choose its easiest example. Select the final artifact through a cheap forcing probe; the current
preferred shape is a small cryptographic or binary-protocol module exported through the C ABI and
used by an existing host project. It must combine functional contracts, bounded arrays/loops,
bit-level reasoning, no-allocation/resource policy, an explicit constant-time boundary,
dependency receipts, package/interface evidence, clean replay and an external consumer.

This is not permission to claim machine constant-time from source-shape checks. Ordinary contracts
remain single-execution properties. True constant-time/non-interference requires a separately
versioned paired/self-composed execution semantics, public/secret classification and declared
observation/machine model; until a forcing consumer pulls that work, the roadmap records it as a
research boundary rather than a scheduled implementation promise.

The forcing artifact graduates only when a non-author can build it, import its certified
interface, replay its evidence independently, perturb a dependency and see the right invalidation,
and use it from the host without inspecting private Concrete bodies.

### Active Work vs Historical Record

- **Active roadmap:** future work and pull-gated deferred work that can still
  affect the stdlib area+ execution.
- **Historical record:** completed phases 1-6 item bodies, implementation notes,
  and detailed gate transcripts live in [CHANGELOG.md](CHANGELOG.md) and the
  linked design docs.
- **Done items:** do not remain here as task text. Keep only a dependency or
  invariant that still constrains future work; put the completion record in
  the changelog.

**Task-state compaction rule.** Each active task gets one dated canonical status block containing
current stage, measured facts, blockers, next transition and exit gate. When a later measurement
supersedes it, replace that block; do not append a second headline below it. Investigation history,
wrong hypotheses, gate transcripts and completed increments move to the changelog, bug record or a
linked decision note. Keep a historical paragraph here only when its reasoning still constrains a
future choice, and label it `history — non-authoritative`. A reader must never need to reconcile two
present-tense answers inside one task.

**Schema ownership rule.** Subject, obligation, receipt, audit, package and editor artifacts each
name one canonical schema owner. Later tasks extend or consume that schema; they do not define a
parallel representation. A task proposing a second schema must state the independent trust
boundary that requires it, its correspondence check and which representation is eventually
retired.

### Definition Of Done

A roadmap item is done only when its behavior is protected by checked evidence:
positive fixtures, negative fixtures, regression fixtures, and a red-team or
mutation-style guard for the failure mode that would be most damaging. If a task
cannot yet have that gate, the item must say why and name the trigger that makes
it testable.

**When a fix addresses a bug CLASS, "done" means a gate that catches every
instance of the class, not only the reported one.** This project has repeatedly
fixed one instance — a decimal parser while `hex`/`bin`/`oct` still trapped, one
snapshot while sibling ledgers still drifted, one fixture main while the corpus
still failed — and had the siblings resurface a CI run later. The fix that holds
is a class-level gate (a grep/lint over the shape, or a corpus check), not a
per-instance patch.

**Large semantic migrations land in a worktree/branch and merge green — never
incrementally on `main`.** A change to a cross-cutting contract (e.g. the
main-return / exit-code model across the whole fixture corpus) done commit-by-
commit on `main` has produced multi-commit red streaks; isolate it, finish it to
green, and merge once.

### Evidence Debt Rule

**Every new evidence surface must retire a register row or close a hole.**

The failure this rule exists to prevent is not any single defect — it is that evidence
infrastructure grows faster than the debt it accounts for, because infrastructure is more
fun to build than proofs are. The 2026-07-31 state that prompted the rule: four kernels,
five report surfaces and a receipt schema, sitting above a Register A with **0 of 4 rows
discharged** (as of that date; 2026-08-04: **0 of 5 fully discharged, 4 of 5 half** — the
count was also wrong, there are five lowering rows plus a projection) — and, as H23 showed, above an obligation layer that could hand all four
kernels a hypothesis nothing had established.

Two corollaries, both measured rather than asserted:

- **No further prover before the first Register A row is discharged.** Kernel agreement
  has surfaced zero real defects on this arc; the faults came from differential tests and
  from reading reports. A discharged row moves the ceiling further than a fourth kernel.
- **A surface that only renders is not a payment.** Register C counts because it made a
  bug class unrepresentable; a new report view does not, however useful it is.

The rule is deliberately cheap to satisfy and hard to ignore: it does not forbid building
surfaces, it forbids building them *for free*.

### Pull-Gated Work

Deferred work stays in the phase where its trigger lives. Do not build machinery
because the roadmap can imagine it; build it when a workload, proof, failing gate,
or public API forces it.

### Cross-Document Consistency (pull-gated note, 2026-07-31)

**Trigger to build on: a second instance of two documents contradicting each other where
both pass their gates.** There is one instance so far, and it now has a specific gate; do
not generalize until a second one bites.

The instance: three files said "H23 closed as a compile-time fact" while <!-- HOLE-STATUS-OK: quoting the wrong claim -->
[docs/verification/KNOWN_HOLES.md](docs/verification/KNOWN_HOLES.md) said OPEN and
`check_known_wrong_corpus.sh` asserted it still reproduces. All green. `check_docs_drift.sh`
verifies that referenced artifacts *exist*, not that statements *agree* — a limitation its
own commit named (`5a0c4e3e`) before the contradiction recurred. Closed for hole status by
`check_hole_status_consistency.sh`.

The template, if a second instance appears: name ONE authority file, give the fact a
machine-readable form in it, and check that every reference agrees — plus, where possible,
tie the authority to *observed behaviour* rather than to other prose, which is the check
with teeth. Candidate pairs already visible: `CLAIMS_TODAY.md` against the report
vocabulary in `ObligationCore.statusVocabulary`, and `VC_BRIDGE_REGISTER.md`'s rows against
the family generators (the register gate checks generators have rows, not that the rows
describe what the generators emit — H24's insufficient div row passed it).

Why this is a note and not a task: a general "documents must agree" checker greps English
and would be mostly false positives, which is how a gate becomes something people learn to
ignore. The hole-status gate is narrow enough to be exact about the part that matters and
heuristic-with-an-escape-hatch about the rest, and that ratio is what a second instance
should be judged against before any machinery is built.

### Spike-First / Kill Criteria

Large investments are validated before they are built. Any task whose cost is
measured in weeks or more should name a cheap falsification probe — an
afternoon-to-days experiment whose result decides whether the task proceeds —
and its kill criteria: the outcome that stops the work. Precedents: the
combinator count before the parser arc (seven shapes, not forty, made the
EverParse economics real), the cap-inference probes before the capability
verdict, the step-count correlation experiment before any timing claim. The
roadmap is a hypothesis list, not a build list; a task that survives its probe
is built with evidence, and a task killed by one is a success recorded in the
changelog, not a failure.

## Product north star: compositional, transferable evidence

The canonical product direction is
[docs/verification/VERIFICATION_CHARTER.md](docs/verification/VERIFICATION_CHARTER.md): important behavior
is specified at boundaries, composed across calls and packages, carried in
independently re-checkable artifacts, and assigned explicit evidence. Anything
outside the supported verification surface is named rather than silently
omitted.

This combines three pieces that must not be sequenced as rival strategies:

1. **Compositional contracts are the content.** Concrete already ships much of
   the local surface—requirements, ensures clauses, call-site obligations, loop
   invariant/variant obligations, ghost code, proof attachment, and evidence
   classes. The delta is call- and package-closed reasoning, old-state/frame
   semantics, wider data and state models, termination, resources, relational
   contracts, and publishing.
2. **Portable artifacts are the product.** A consumer who does not trust or use
   Concrete can inspect the exact claim and replay its evidence with an accepted
   kernel. Package upgrades can then be gated on behavioral-contract weakening,
   authority/trust widening, resource budgets, and proof-coverage regression.
3. **Faithfulness joins them.** Independent kernels do not repair a shared
   mistranslation. Every graduated contract family therefore advances its
   structured IR, lowering validation or proof, artifact schema, and replay path
   together. Multi-kernel evidence is not postponed to a final rung, nor is
   kernel count used as a substitute for bridge assurance.

Progress is measured by meaningful real-program behavior covered through that
whole path, not by construct count or prover count alone. Routine obligations
should require as few annotations as sound inference permits, but automatic
specification discovery is not a completeness claim. Strict coverage is also a
policy choice: support module/property-specific requirements and explicit hole
budgets so incremental adopters can ship without turning named debt into green.

### Delivery order implied by the charter

This direction does not replace the execution queue below. It supplies the
ordering rule for work entering that queue:

1. fail-visible coverage accounting and negative controls;
2. a typed verification IR with executable semantics and practical differential
   validation, strengthened by proofs incrementally rather than blocking all
   downstream delivery;
3. compositional function contracts—callee postconditions, call-site
   preconditions, old state, frame conditions, and ghost state;
4. total correctness—complete the existing operational loop-preservation path,
   then variants and recursive decreases; this proves termination, not general
   temporal liveness;
5. enums, structured state, and a declared memory model;
6. resource contracts and graduated release policies;
7. a separate paired-execution path for constant-time and non-interference;
8. proof-aware package interfaces and standalone consumer-side replay.

Each slice must exercise a real workload and ship its evidence artifact and
honest bridge status with it. Avoid demonstration contracts whose specification
merely restates the implementation, and avoid using quantified flagship examples
whose prover-library cost obscures the smaller capability actually being built.

**Delivery focus (2026-10-04): make a useful change cheaper to trust.** R-0486 is
the shared acceptance workload for contracts, review diffs, policy and package
evidence. Each prerequisite increment names the user-visible operation it unlocks,
the smallest sufficient artifact/schema, and its bounded exit. Keep the required
qualification, identity and independent-checking gates; defer broader schemas,
backend breadth and proof automation until the workload demonstrates a need.
This changes milestone emphasis, not the current execution queue.

Three adoption tracks run across those slices rather than waiting behind them:

- **one-module adoption:** a supported C ABI path for introducing one
  evidence-bearing Concrete component into an existing C or Rust build;
- **one forcing domain:** carry a narrow constant-time/secret-flow or similarly
  consequential claim through contracts, bridge status, artifact replay, and
  package policy without overstating machine-level guarantees;
- **one feedback surface:** CLI, CI, agent, and LSP explanations over the same
  evidence facts, including actionable reasons for every proof or coverage gap.

The package milestone is complete only when an independent standalone checker,
not the producing compiler's report renderer, can validate the versioned
artifact and a dependency upgrade can be rejected for a policy-relevant
behavioral regression.

### Verification-document convergence required by every graduation

[docs/verification/VERIFICATION_STATUS.md](docs/verification/VERIFICATION_STATUS.md) is the canonical
cross-axis capability map. `CLAIMS_TODAY` owns exact public guarantees,
`KNOWN_HOLES` owns defect status, the verification charter owns direction, and
this ROADMAP owns ordering and acceptance criteria. A feature is not graduated
when only its implementation lands: the same change must update the capability
matrix, claim boundary, known-hole disposition, and shipped/experimental label.

Missing documentation infrastructure, now explicit roadmap work:

1. extend documentation drift checks so a closed hole cannot remain a live
   overriding caveat in `CLAIMS_TODAY`;
2. reject shipped syntax described as absent or planned in canonical proof docs;
3. require every verification-status row to name its proof subject, evidence
   class, support state, and important boundary;
4. require experimental branch capabilities to remain labeled experimental
   until code, negative controls, policy, and documentation land together;
5. generate the stable portions of the capability matrix from compiler
   vocabularies and evidence registries once those schemas can express all
   dimensions without losing information;
6. reconcile the multi-kernel and VC-calculus branch documents against the
   charter before either branch merges, including termination versus general
   liveness and evidence-vector terminology.

Until these checks are automated, `make test-docs-drift` plus review of
`VERIFICATION_STATUS`, `CLAIMS_TODAY`, and `KNOWN_HOLES` is a required
graduation gate.

### Language baseline and bounded verification milestone

**Direction ratified 2026-09-15; implementation and verification remain incomplete.**
Concrete's systems-language shape is provisionally settled. The next phase implements
and validates that shape, while completing the contract semantics needed for one useful,
compositional verification subset.

Linear ownership, scoped references, explicit capabilities and cleanup, exhaustive
`match` plus explicit error returns, static or explicitly indirect dispatch,
monomorphization, no GC and no unwinding are the baseline. This is a planning constraint,
not a claim of soundness or a public compatibility freeze. Safety repairs, consistent
diagnostics and library API corrections remain admissible. New ordinary-language syntax
or abstractions require a concrete workload demonstrating why the existing model is
insufficient, plus a stated semantic rule and verification boundary. Postfix `?` remains
excluded. Broader language expressiveness is not the next milestone.

The remaining design work has existing owners:

| area | owner | decision or completion required |
|---|---|---|
| Safe library abstractions | R-0483, with R-0485 for review clarity | Make views, cursors, text and resource APIs preserve the existing ownership rules; distinguish reusable coordinates from access tied to an encapsulated owner, and preserve validated content against mutation. This is primarily library/API work, not a presumption that a new lifetime system is needed. |
| Authority and effects | R-0484 | **Decided 2026-09-29/30:** a header's `with(...)` is the complete list of the function's external authority. Handles carry their capability in their type (`Writer<C>`) and using one requires `with(C)`; `trusted` absorbs only `Unsafe`. Implemented; closed 2026-10-08 (see the task). |
| Compositional contract semantics | R-0473/R-0474/R-0477, the proof-authoring area and the existing VC bridge tasks | Complete typed contracts, binding/substitution, call composition, narrow mutation/frame semantics and checked totality. This is the largest remaining language-design area; R-0486 supplies the forcing workload. |
| Tuples and destructuring | R-0515 coordinates the decision record | Before language-surface freeze, document whether named records remain the supported product type or a workload justifies tuples. Include a multi-value-return example, ownership/cleanup implications, and acceptance or rejection examples. A deliberate deferral with a reopening trigger closes the decision; implementation is not presumed. |

**First bounded verification milestone — planned, not shipped:** one sequential
parser/state-machine component, drawn from the already selected flagships, with contracts
over scalars, records and fixed arrays. R-0486 owns the workload; the existing typed-contract,
two-state, totality and VC tasks own the semantics. Graduation requires:

1. A stated admitted subset with bounded iteration and explicit failure behavior;
   contract-callable helpers have a checked logical interpretation and required totality.
   An empty `with(...)` is not sufficient for that: it still permits mutation through
   `&mut` parameters, so contract evaluation needs its own purity and admissibility rule
   (recorded 2026-09-30 by R-0484's design, `HANDLE_CAPABILITIES.md` §1).
2. Calls establish preconditions and use exported postconditions without callee inlining;
   `old` and frame/`modifies` conditions describe the admitted record/array mutations.
3. Bounds, arithmetic traps, assertions and call obligations have explicit evidence or
   gaps. Unsupported constructs refuse or are classified visibly; runtime checks and
   trusted FFI/backend boundaries do not become proofs by association.
4. A caller consumes the component's interface without inspecting private bodies;
   contract-preserving implementation changes preserve caller evidence after the required
   implementation recheck, while contract/authority/trust changes produce precise diffs.
5. The migrated component has functioning safety regression controls and measured proof
   effort, review friction and runtime costs. Its results determine subsequent feature work.
6. A non-author changes the component or one dependency, sees which guarantees the
   change affects, repairs the affected obligations, and independently checks the
   resulting evidence within the admitted scope. R-0486 retains the before/change/repair
   artifacts; R-0150 measures effort from the first baseline, and R-0182/R-0183 supply
   the initial authority/assumption/contract-evidence diff. Compiler-author intervention
   is recorded as an unresolved usability limitation, not counted as independent success.

Every feature contributing to this milestone includes actionable failure feedback in
its exit: source location, relevant call/dependency path, unmet permission or obligation,
and a valid next action (or an explicit unsupported boundary). R-0137/R-0466 own the
diagnostic criteria; consumers reuse the same facts rather than inventing new verdicts.

General heap proofs, concurrency, relational verification and general resource-bound proofs
are not prerequisites for this milestone. Their existing research and later-phase owners
remain in place. Fixed-capacity enforcement and resource reporting must state their actual
evidence strength. This milestone narrows the first useful verification target; it does not
waive the current queue's evidence-integrity prerequisites or the broader release gates.

### Design-review priorities (ratified 2026-09-15)

R-0483 (sound, usable zero-copy parsing) closed 2026-10-09 with owner-bound views, and
R-0484's authority semantics closed 2026-10-08; both are in the changelog. R-0485 improves review clarity within the existing
error model. R-0486 carries the same workload into the typed-contract and external-user
rows above; it is not an additional public flagship or a parallel execution queue.
The ByteCursor defect and Text/ByteView lifetime, identity and content-validity
classification are recorded under R-0483 with ten retained fixtures. The additive-change
experiment and raw-constructor inspection are also recorded there. The remaining
questions concern safe migration of the affected attestation references and the actual
proof/evidence path for the Writer helper: its reporting and eligibility misclassification
is now measured under R-0484. Repairs and target guarantees are not yet shipped.

- **Keep explicit error propagation.** Exhaustive `match` plus explicit `return`
  remains the model. Postfix `?` stays rejected; this review does not reopen it.
  Improve diagnostics, `defer` consistency and library APIs within that constraint.
- **Prioritize semantic correspondence and composition over backend breadth.**
  R-0473/R-0474/R-0477 own typed contracts, resolved lexical identities, capture-safe
  substitution and call composition. R-0450/R-0454–R-0456/R-0460 own the single typed
  obligation pipeline and H19 bridge work. Complete a useful fragment with explicit
  trap semantics, call preconditions and unsupported coverage before expanding
  proof backends. Agreement among kernels does not establish source correspondence.
- **Double down on the compiler as an audit tool.** Authority paths, resource and
  failure summaries, named trust boundaries, contract-preserving change detection,
  and independently degradable evidence remain the product. Existing evidence schema
  owners supply these facts; new renderers must not invent another source of truth.
- **Retain the restricted systems core and evaluate its costs on workloads.** No GC,
  no unwinding, explicit indirect calls, scoped references, monomorphization and
  explicit cleanup remain the baseline. No closures or macros are proposed here.
  Explain those exclusions through semantic complexity, allocation visibility and
  analysis cost: finite callable enumeration alone does not require either ban.
  Evaluate explicit function-plus-context callbacks on real composition patterns;
  record awkward workarounds before considering any separately justified change.

### Unblocked repairs, off the strict queue (recorded 2026-09-15)

Small, owned, and independent of the sequence above. Each was reproduced during the postfix-`?`
removal; none blocks a queue item, and none should be allowed to become furniture.

| # | repair | why it matters | exit |
|---|---|---|---|
| a | **`attestation_refs.sh` canonical order** | the generator never sorts, so a byte-for-byte gate can call a correct tree stale and advise a "fix" that breaks CI — reproduced, see checklist item 9 | emit sorted under `LC_ALL=C` as `build_identity.sh` already does, regenerate once, and add a gate asserting two differing enumeration orders produce identical output |
| b | ~~**Diff-to-gate mapping for pre-push**~~ | **DONE 2026-09-18.** `scripts/tests/gates_for_diff.sh` selects the gates a change can break and runs them with `--run`. The map is READ OUT OF THE GATES rather than curated — gates name the files they check, so it cannot drift the way a hand-kept list would, and gate-to-gate references come along for free (`check_mutation_anchors.sh` names the mutation driver, so editing the driver implicates the anchor gate). Umbrella directories are excluded, because nearly every gate mentions `scripts/tests/` to reach a helper and a list that is always long is ignored as fast as one that is always empty; broad trees are covered by a curated ALWAYS set carrying a reason per entry. **Validated against the diff that motivated it:** for the R-0483 stdlib repair it selects 25 of 226 gates and names all five that actually went red — stdlib surface manifest, shadow-body, mutation anchors, ByteView, std compiled coverage — which took four separate CI rounds to discover. `check_gates_for_diff.sh` 11/0 pins that, asserts the answer is a selection rather than the whole suite, and carries a control that the selector can answer NO. It is stated in its own output as a MAPPING and not a proof of completeness: a gate can be broken by a change it never names |
| c | ~~**TCP fixtures are broken on Linux**~~ | **DONE 2026-09-15.** Ported, not made conditional: the language has no `cfg`, and the two ABIs disagree at byte zero (`[2,0]` against `[16,2]`), so no single pattern satisfies both. The fixtures now try the Linux layout and fall back to BSD — the assertion is that a socket binds and a roundtrip completes, not that one ABI is in force, so discovering the layout by attempting it is honest and exercises the real syscalls on both platforms. Coverage went from **zero to both**: they were `--full`-only *and* quarantined while CI runs `fast`, so neither ever executed anywhere. `SKIP_FLAKY_TCP_TEST` is renamed `SKIP_NETWORK_TESTS`; `tcp_basic` was never flaky and the false label is what stopped anyone reading the failure |

Repair (c) carries a second finding worth stating separately: `tcp_basic.con` is labelled flaky and
is not flaky. It fails deterministically on one platform, and the label is what kept anyone from
looking. A skip whose reason is wrong is a defect with a lid on it.

### Open holes, owners, acceptance tests

| Hole | Owner | Reproduce | Done when |
|---|---|---|---|
| **H24** obligation generation restates trap rules, weaker | **R-0464** | `examples/trap_semantics_gap/` — div reports proved and aborts on `MIN / -1`; shift generates no obligation | div obligation covers signed `MIN / -1`, a shift family exists, both derived from `IntArith` |
| **H19** the Core→obligation bridge is unproven | **R-0460** | — (structural; 0 of 5 register rows fully discharged, 4 of 5 half — the *semantics* half of each is proved, the *lowering* half IS H19) | rows in `VC_BRIDGE_REGISTER.md` discharged, named individually |
| **H20** `bv_decide`'s certificate check runs as native code | unowned; see [docs/verification/AXIOMS.md](docs/verification/AXIOMS.md) | `make test-bv-certificates` | a verified checker, or decomposition until kernel-reduction LRAT is practical |
| **H21** nonlinear SMT results cannot be certificate-replayed | **R-0451** (upstream-blocked) | gate assertion in `check_multi_kernel.sh` | upstream reconstruction support, or a certified nonlinear checker |

**No reproduced unsoundness remains** (2026-08-03). H23 and H24 are both closed and both
fixtures are retained as regression guards. The scope limits that remain are *structural*
rather than reproduced — H19 (the Core→obligation bridge is unproven), H20, H21 — and they
are bounded by Register A, not by anything a fixture currently demonstrates.

Read that precisely: R-0464 proves every trap condition in the semantics has an obligation
family, **not** that the families' propositions are strong enough. Sufficiency is Register A, where as of
2026-08-04 **4 of 5 rows have their semantics half proved and proved tight** — overflow,
bounds, div/mod and shift. That is genuine progress and it does not touch H19: every one of
those theorems says the CONDITIONS suffice, never that the emitted obligation denotes them.
`VC_BRIDGE_REGISTER.md`'s rows are the list of what is still owed, and the remaining row
(`callSiteObligations`) is different in kind — its content is substitution correctness.

**Correction worth carrying (2026-08-03).** Four places recorded that Lean's rendering
cannot be validated because "its rendering IS the reference the others are validated
against, so there is nothing to validate it with". That is false. The lowering-agreement
check validates a rendering against the reference **evaluator** (`safeOn`/`evalBoolEnv`,
walking the AST) — Lean's rendering is not the yardstick, it is simply the one never
measured. The actual obstacle was that Lean's lowering was not expressible as a driver, so
the existing machinery could not be pointed at it; R-0450's slice removed that. The gap is a
TODO, not an asymmetry that cannot be closed, and the difference matters for planning: it
was being treated as unfixable-in-principle.

H23 no longer needs such a caveat: an obligation resting on an unproved invariant now
reports `assumed` on every surface and fails `[policy] forbid-assume` with `E0617`. What it
leaves behind is a **method**, recorded here because R-0464 and R-0460 will hit it too:

- *Fixing the derivation is not fixing the report.* The ledger and the multi-kernel report
  each computed the class, so capping the ledger left the badge surface still printing
  `proved_by_multi_kernel`. Any status change must be checked on **every** surface that
  renders it, and R-0465's firewall is what would make that structural rather than diligent.
- *Display is not enforcement.* The cap read `assumed` while `concrete check` exited 0,
  because the existing `forbid-assume` gate keys on the `assume(...)` construct, not on the
  status. A status is only load-bearing once a policy fails on it.
- *Renaming a gate block can silently drop its coverage.* Relabelling the corpus heading to
  `=== H23 (CLOSED …)` removed H23 from `check_hole_status_consistency.sh`'s checked set —
  it went green by no longer looking. That gate now rejects an unclassifiable heading
  instead of skipping it, and the mutation is verified to be caught.

### Gate inventory — what was added, and what each actually guards

| Gate | Guards | Build? | CI job |
|---|---|---|---|
| `check_evidence_algebra.sh` | Register C rows present, no `sorry`/`native_decide`, one construction site per badge string, all consumers share the derivation | no | `grammar` |
| `check_known_wrong_corpus.sh` | H24's fixture still reproduces; H23's now guards its fix (`assumed` on both surfaces + `E0617`) — a counterexample that stops demonstrating its hole is worse than none | **yes** | `extra-gates` |
| `check_hole_status_consistency.sh` | no document contradicts KNOWN_HOLES; every fixtured hole's corpus heading declares reproduces-vs-guards and matches its documented status (an unclassifiable heading FAILS rather than skipping — see R-0461's note) | no | `grammar` |
| `check_multi_kernel.sh` | 14 assertions bare, 74 with provers, 82 with `MULTI_KERNEL_MUTATE=1` | yes | `multi-kernel` (opt-in — **R-0467**) |
| `check_vc_bridge_register.sh` | every family *generator* has a register row — **cannot detect a missing family**, which is how H24's shift gap hid | no | `grammar` |

### Mutation coverage: 10 of 10 families KILLED (2026-08-01, this repository)

Every gate in `check_gate_mutation_coverage.sh` detects removal of the property it guards.
The run was manual — the scheduled job is pinned to `lambdaclass/concrete` and cannot fire
here (**R-0468**).

This retires the standing suspicion R-0468 recorded ("assume more H22s until the gate has
run clean once"). It has run clean.

**But note precisely what a KILLED verdict means, because it is narrower than it feels.**
Family 9 `fact-invalidation` is KILLED — removing fact invalidation turns
`check_scoped_collector.sh` red — and H23 lives in that exact machinery, untouched. The
gate guards *staleness* (does a mutated variable drop its facts?); H23 is *composition* (is
the invariant that supplied the fact itself established?). A fully load-bearing gate,
adjacent to the bug, blind to it.

> **Mutation coverage proves a gate detects the removal of what it tests. It never shows
> the gate tests the right thing.** Both H22 and H23 were invisible to gate suites that
> were green — H22 because its gate was decorative, H23 because no gate had the property
> in scope at all. The second failure mode is not detectable by mutation testing and needs
> counterexample fixtures instead, which is what `check_known_wrong_corpus.sh` is for.

### Running things

```
lake build                                            # required by any gate marked Build? yes
bash scripts/tests/check_known_wrong_corpus.sh        # both live holes, ~1 min
bash scripts/tests/check_hole_status_consistency.sh   # doc/behaviour agreement, instant
bash scripts/tests/check_multi_kernel.sh              # 14 assertions without provers
nix develop .#provers --command bash scripts/tests/check_multi_kernel.sh    # 74
MULTI_KERNEL_MUTATE=1 nix develop .#provers --command bash scripts/tests/check_multi_kernel.sh   # 82, ~40 min
FAMILY=<n> bash scripts/tests/check_gate_mutation_coverage.sh   # one family, ~15 min if it rebuilds
bash scripts/tests/run_ci_gates_local.sh <area>       # the gates CI runs for an area
```

`check_gate_mutation_coverage.sh` mutates tracked source. It refuses to start on a dirty
tree and restores on EXIT/INT/TERM — but bash defers a trap until the current foreground
command returns, so a kill during `lake build` is not prompt. **After interrupting it, run
`git status` before doing anything else.**

### External review

`.claude/codex-review-prompt.md` holds the review criteria as a versioned file — its seven
checks each correspond to a defect this codebase actually shipped, so it is project
knowledge rather than a one-off prompt. Run it read-only:

```
codex exec --sandbox read-only -c model_reasoning_effort="xhigh" \
  "$(cat .claude/codex-review-prompt.md)" < /dev/null > review.md
```

Three things that cost a run each: `codex exec` hangs without `< /dev/null`; the default
reasoning effort is `none`, which produces a near-worthless review; and `codex resume`
needs a TTY, so a truncated non-interactive run must be restarted rather than continued —
give the prompt a tool-call budget and pre-seed established findings instead.

`--sandbox read-only` is deliberate: the reviewer reports, it does not edit. The cost is
that it cannot mutation-test, which is where its sharpest finding came from — it deleted a
filter and showed the gate stayed green. Preserve that by asking it to NAME the mutation
and its prediction; apply and verify the mutation yourself.

Its 2026-08-01 findings landed in `0281890d` and `037e9616`; two structural ones are filed
under R-0465, and one reported divergence was a false positive (it compared different
inputs) and is recorded as such rather than left to be re-found.

> Moved to [ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md#rationale-notes-the-prover-neutral-arc-r-0004-r-0450-r-0448-r-0454r-0456-r-0458r-0464-r-0467-r-0468) (historical record; nothing in it schedules work).

## Standing reference: open holes, register status, and how to write a gate

_Extracted from the `spike/multi-prover-evidence` pre-merge record after that merge landed._
_The merge-specific parts are dropped; what follows outlives it._

### Known holes still open (from `docs/verification/KNOWN_HOLES.md`)

- **H19 — the Core→obligation bridge is unproven.** Listed above; repeated here because it is the
  largest single gap in the trust story.
- **H20 — `bv_decide`'s certificate check runs as native code.** The bitvector decision procedure
  is trusted at the level of its checker binary, not the Lean kernel.
- **H21 — nonlinear SMT results cannot be certificate-replayed** (upstream limitation). A
  nonlinear goal closed by an SMT solver stays `solver_trusted`; only linear goals reach
  `proved_by_lean_replay`.

Related ceiling, measured and documented rather than assumed: **SMT datatype goals are provable
but not Alethe-certifiable.** cvc5 proves a ground datatype goal and cannot emit a replayable
certificate for it, so non-arithmetic obligations are kernel-proved rather than
certificate-replayed. This is what a non-arithmetic multi-prover example (item 6 above) would run
into, and it is a property of the tooling, not of this repo.

### Register status, plainly

- **Register A** (obligation sufficiency): 0 of 5 discharged, 4 half-discharged. Half means the
  row is still *trusted*.
- **Register B** (transformation soundness): 1 of 3 rows discharged (`eliminate_tmod`).
- **Register C** (evidence composition): **DISCHARGED by compile-time rows** — C1-C4 are theorems
  in `Concrete/Report/Evidence.lean` ("a green build is the proof"), with 59 structural/wiring
  assertions in `check_evidence_algebra.sh` on top, no `sorry`/`admit`/`native_decide`. Earlier
  text here said it was "gated rather than expressed as a row
  table; no per-row discharge tracking exists for it yet, which is itself a gap.
- **Discovery completeness** (added 2026-08-05): 4 of 4 runtime-safety families. This register did
  not exist before and answers the question the other three structurally cannot — whether an
  obligation is generated at all.

### The composition test is opt-in

`check_multi_kernel.sh`'s final section — a disagreeing lowering must not earn the two-kernel
badge or pass the release gate — is **skipped unless `MULTI_KERNEL_MUTATE=1`**, because it mutates
tracked source and needs two rebuilds. It therefore does NOT run in the normal 78/78 pass. Any
claim that composition is gated should say "when that variable is set".

### Writing gates that actually bite — checklist earned the hard way

Recorded because this session produced eight concrete failures of gate quality, and the pattern
is consistent enough to be a rule rather than an anecdote.

1. **Assert the property, not the code.** Six assertions of mine fired on non-regressions. Every
   one quoted something syntactic — an argument list, a call site verbatim, a mechanism ("the
   access is NAMED") — and broke when the code improved. Twice the *same* assertion broke twice.
2. **A check that greps one error class treats every other failure as success.** A sweep reported
   GREEN from a loop whose exit condition looked only for termination errors, so
   `cannot mix partial and non-partial definitions` read as a pass.
3. **Never interrogate a binary you did not just build.** An end-to-end assertion passed while the
   fix was reverted, because the mutated source failed to compile and `lake` left the old
   executable in place. An mtime comparison is also wrong — `lake` does not relink on a cache hit.
   Build inside the gate and fail if the build fails.
4. **A section guarded on a temp dir skips itself when the build breaks.** Guard on failure, not on
   the presence of a variable an unbuildable tree never sets.
5. **Mutations must COMPILE to count.** A mutation rejected by the unused-variable lint looks
   killed and tests nothing. Classify those INVALID; do not report them as kills.
6. **Distinguish an equivalent mutant from a survivor.** Making a declaration visible to its own
   statement is unobservable (a `let` initialiser cannot index the name being declared) — that is
   INVALID, not a gate weakness.
7. **A completeness theorem needs three things**, or it is true and worthless: a satisfiable
   antecedent (non-vacuity), a negative case (the conclusion discriminates), and its boundary
   pinned so `∀ e, P e → Q e` is not read as global completeness when `P` is narrow.
8. **A false number is worse than a missing one**, because it is quotable. `--report stack-depth`
   stated `Max stack bound: 32 bytes` for arbitrarily deep recursion.
9. **A byte-for-byte comparison needs a canonical order, or it compares environments rather than
   content.** `scripts/gen/attestation_refs.sh` emits in manifest order and never sorts —
   `grep`/`awk` preserve input order — so `check_attestation_manifest.sh` reported a correct tree
   as STALE and advised regenerating it. Following that advice would have committed one machine's
   enumeration order and broken CI, which agreed with the checked-in file. Its sibling
   `scripts/gen/build_identity.sh` already states the rule it needs: emit sorted, "so the digest is
   a function of content and not of directory order." A generator feeding a byte comparison is not
   finished until its order is a function of content alone.
10. **A change that moves a digest is a change to everything derived from it.** Removing postfix
    `?` altered the evidence encoding and left two derived artifacts stale; each had a freshness
    gate, each gate lived in a different CI job, and they reported hours apart. Treat the
    generators as the unit of change: when a digest moves, re-run every `scripts/gen/*` producer
    and the freshness gates in one pass before pushing, rather than discovering the set one CI job
    at a time.
11. **A gate the local suite cannot run is a gate that only fails after you push.** CI invokes 204
    distinct `scripts/tests/check_*.sh` gates; `scripts/tests/run_ci_gates_local.sh` invokes three,
    and `run_tests.sh --full` covers a different axis entirely. The gap is not that the gates are
    expensive — most are not — but that nothing maps a diff to the gates it can break.
12. **A golden that records workspace cleanliness can only be verified from a clean tree.**
    `campaign-summary.golden` carries `tracked_sha` over modified tracked files, and the baseline
    holds the empty-string digest. Regenerate, commit, *then* verify: checking it while the
    regenerated file is still uncommitted fails on `tracked_sha` and points at the wrong field.

## Design honesty: this is largely Why3's architecture, and one layer is worse than Why3's

Worth writing down, because it reframes what is valuable to build next.

### The convergence is real

| Why3 | Here |
|---|---|
| driver files per prover | `ProverLowering` records — "the tactic is DATA, not a template" |
| VC transformations (`eliminate_*`) | Register B rows, same names and same idea |
| many provers per goal | the multi-kernel tiers |
| `variant` clause for termination | the `#[decreases]` design (rung 8) |
| theory **realizations** | **missing** — see below |

Arriving independently at Why3's shape is reassuring: it is the state of the art for this problem.

### Two things here are genuinely better

**The reference-evaluator agreement check.** Why3 TRUSTS its drivers — if a driver prints the
wrong thing, the prover closes the wrong goal and nothing notices. Every lowering here is checked
against an independent evaluator instead: exhaustively for booleans, sampled for arithmetic. A
kernel that "proved" a mis-rendered obligation does not earn the badge.

**Discovery completeness.** Why3 generates VCs by weakest-precondition calculus, so "was an
obligation emitted at all" is not a question anyone needs to ask. Here it was, and asking it found
seven defects, two of them unsound.

### One thing here is worse, and it is the layer that produced this year's bugs

**Why3 generates VCs by a CALCULUS. This generates them with hand-written AST walkers, one per
family.**

That is exactly where every discovery bug came from — `(a)[i]` missed, `b.data[i]` missed,
unannotated array sizes missed, shadowed bindings sized from the wrong declaration. A
weakest-precondition generator makes most of those **impossible by construction**: you cannot
forget a case when you are not enumerating cases. Four walkers were patched to fix symptoms whose
common cause is the absence of a calculus.

**The design is written: [docs/verification/VC_GENERATOR_DESIGN.md](docs/verification/VC_GENERATOR_DESIGN.md).** One
traversal plus a per-constructor rule table replaces four walkers; the `if`/`while` rules subsume
`scopedWalk`'s hand-threaded guards, and the transformer's environment is the `ScopeDecls` work
already done. It recommends running over **Core rather than the surface AST**, which is the one
change that attacks H19 structurally. Migration is differential against the existing walkers —
they are a usable oracle now that their seven defects are fixed and gated, and every diff is a
finding either way. It also makes Register A statable as ONE theorem ("if every obligation from
`wp(body, True)` holds, the body cannot trap") instead of five half-discharged rows.

**Consequence for planning:** adding more tiers (rungs) grows what CAN be expressed, while the
generator remains the part most likely to be silently wrong. A principled VC generator is
therefore plausibly worth more than the next two rungs combined, and it subsumes H19 (the
Core→obligation bridge) — Why3's answer to that is realizations, where the axiomatized theory is
proved in Coq/Isabelle rather than assumed.

This is not an argument to stop; the tiers are real and verified. It is an argument about ORDER,
and it is recorded here because "add another rung" is the more interesting work and therefore the
easier trap.

## Why3 as a comparative reference, not a backend target

**[ratified] Concrete does not support Why3 as a backend.** This inventory records useful prior art
and missing verification-language capabilities; it is not a WhyML compatibility plan, a promise to
consume Why3 sessions, or permission for Why3 results to enter Concrete authority. Concrete owns
the canonical task, attempt, session, receipt, policy, and cache formats described above. Where a
Why3 mechanism is useful, reproduce the property through those Concrete-owned types.

The table is therefore a comparative checklist rather than a parity target: what Why3 demonstrates,
what exists here, and which gaps matter independently for Concrete's own product.

### What Why3 has, and where this stands

| Why3 capability | Here | To close |
|---|---|---|
| WP calculus for VC generation | ✗ four hand-written walkers | `docs/verification/VC_GENERATOR_DESIGN.md` |
| driver files per prover | ✓ `ProverLowering` | — |
| transformation library (~100) | ~1 discharged row (`eliminate_tmod`) | Register B rows 2–3, then breadth |
| `requires` / `ensures` | ✓ | — |
| `variant` (termination measure) | ✗ | `#[decreases]`, rung 8 |
| `invariant` (loop) | ✗ | loop invariants as obligations |
| `old(x)` / two-state postconditions | ✗ | **essential and missing** — see below |
| ghost code | partial (`spec fn`, erased) | ghost *variables* and ghost state |
| algebraic datatypes in specs | ✗ | the enum rung |
| pattern matching in specs | ✗ | follows enums |
| exceptions / error paths | ✗ | Concrete uses `Result`; specs cannot mention it yet |
| mutable records & arrays in specs | reads only | needs `old` + a memory model |
| theory/module system, cloning | ✗ | a spec stdlib with refinement |
| standard theories (int, list, map, set, bitvector) | ✗ | a spec stdlib — nothing exists |
| counterexamples from models | ✓ `counterexample` status with source-mapped values | — |
| developer fingerprints / staleness / VC replay command | partial: present, but not one typed theorem-replay producer | extract the typed producer and keep rendering outside it |
| authoritative theorem replay receipts | ✗ no production minting, storage, migration, or consumer | R-0004 Package 3; Why3 sessions are not the format |
| mutable proof-session history and obsolete attempts | ✗ | Concrete-owned `ProofSession`, after the receipt authority path; not an R-0004 UI blocker |
| **realizations** (theories proved in Coq/Isabelle) | ✗ | H19 is the same gap wearing a different name |
| extraction to OCaml/C | ✓ compiles to LLVM — see below | — |

### The single biggest missing feature: `old`

Nothing here can state a postcondition that RELATES the state before a call to the state after it.

```
#[ensures(len(v) == old(len(v)) + 1)]     // not expressible today
fn push(v: &mut Vec<i32>, x: i32)
```

Every specification of a mutating operation needs this, so its absence quietly restricts specs to
pure functions — which is why the corpus's only real contracts are refinement of pure helpers.
It is a prerequisite for mutable records, arrays-as-state, and most of a spec stdlib. **It should
rank alongside `#[decreases]`, not below it.**

### Three places this is already ahead of Why3

Not incidental — each is a deliberate consequence of targeting a compiler rather than a spec
language, and each should be defended when the parity items above get built.

**1. Drivers are VALIDATED, not trusted.** Why3 trusts its drivers: if a driver prints the wrong
thing, the prover closes the wrong goal and nothing notices. Every lowering here is checked
against an independent reference evaluator — exhaustively for booleans (`2^n` rows), sampled for
arithmetic. A kernel that closes a mis-rendered obligation does not earn the badge. As transformations
and drivers multiply, this is the property that keeps the count meaningful.

**2. Evidence is a taxonomy, not a boolean.** Why3 reports "proved by Alt-Ergo". Here:
`runtime-checked → solver_trusted (SMT in the TCB) → proved_by_lean_replay (certificate
reconstructed) → proved_by_kernel_decision → N independent kernels agreeing`, with axiom-freedom
gated via `Print Assumptions` and a documented native-code exception list. That distinction is the
difference between "a tool said yes" and "a kernel checked it, and here is which kernel and what
it trusted".

**3. Obligations are about the code that RUNS.** Why3 verifies WhyML and extracts to OCaml/C;
the extracted program is a translation of the verified one. Here the same source is compiled to
LLVM, so the obligations concern the artifact itself. That advantage is currently **claimed rather
than proved** — H19 says nothing connects surface-AST obligations to the compiled form — which is
precisely why the VC generator should run over Core.

A fourth, with no Why3 analogue: **discovery completeness** — proving the obligation is generated
at all.

**Scheduled for demotion, and marked so deliberately.** Why3 does not need this register because a
WP calculus cannot forget a case. That means discovery completeness is *compensating for a design
flaw*, not adding a guarantee Why3 lacks — so once the generator lands it should be re-labelled a
redundancy check, not defended as permanent scaffolding. Recording the expected demotion now,
because a register that quietly outlives its cause is how a metric turns into a ritual.

### Ordering for Concrete's own verification stack

1. **VC generator** (design written) — closes the largest structural gap and subsumes H19.
2. **`old` / two-state postconditions** — a ceiling, not a feature: without it specs cannot
   describe mutation at all.
3. **spec standard library** (int, array, list, map) — **moved up from sixth.** A calculus with
   nothing to say about lists does not get used; much of Why3's practical value is its theory
   library, not its WP engine. Nothing exists here today, so even after items 1–2 land there is
   little to *say*. It was ranked sixth by how interesting it is to build, which is the wrong axis.
4. **`#[decreases]`** — unlocks recursion and liveness at once.
5. **loop invariants** — safety and functional-correctness machinery; variants supply the
   separate termination half of total correctness.
6. **enums + pattern matching in specs** — last quantifier-free construct.
7. **realizations** — prove the axiomatized theories in the kernels rather than assuming them.
8. **heap / separation** — largest remaining, needs design before code.

Items 1–3 are where the leverage is.

## North star: compositional, fail-visible verification

Concrete succeeds when meaningful behavior can be specified at a public boundary, relied upon
compositionally across calls and packages, and traced to explicit, independently replayable
evidence. Any behavior, dependency, assumption or translation step outside the supported surface
must remain named and policy-visible.

The unit of progress is therefore not construct count, prover count or the number of green local
goals. It is meaningful real-program behavior carried through the complete chain:

```text
source + typed contract
    → exact semantic subject and claim
    → faithful obligation
    → complete dependency/trust/environment closure
    → replayed evidence receipt
    → caller/package policy
```

Multi-kernel replay remains a defining evidence advantage: it lets a consumer choose and combine
independent proof foundations. It is one dimension of the result, not the product objective and
not a substitute for checking that every kernel received the intended proposition. The three axes
below are a coverage inventory subordinate to this chain, not three competing roadmaps.

### Axis 1 — language constructs (what a proof is ABOUT)

| Construct | Obligations today | To reach the kernels |
|---|---|---|
| integer arithmetic | overflow, div, shift | **done** (linear); nonlinear is 2-of-3 |
| array indexing | bounds | **done**; arrays lower as total `index → value` |
| booleans / conditionals | — | **done** (rung 2) |
| structs / field access | — | **done** (rung 6) |
| uninterpreted spec fns | — | **done** (rung 5) |
| defined spec fns | refinement | **done** — the `spec fn … = expr` body |
| **enums / sum types** | — | constructor injectivity + disjointness, emitted per kernel |
| **loops** | safety plus invariant/variant obligation families | typed resolved invariants, production total-correctness consumption and stronger loop-state models |
| **recursion** | none — profile-banned | `#[decreases]`; see rung 8 |
| **heap / references** | linear types (static) | a memory model; separation-logic shaped |
| **generics / traits** | monomorphised away | provable per instantiation; the general statement needs polymorphic lowering |
| **closures / fn pointers** | refused by the profile | target-set analysis, or higher-order lowering |
| **capabilities / effects** | statically enforced | sound by construction; not obligation-shaped |

### Axis 2 — property kinds (what a proof SAYS)

This axis matters more than the first and is easier to overlook.

| Property | Shape | Status |
|---|---|---|
| **Safety** ("this bad event never happens") | predominantly quantifier-free per site | the broadest automatic obligation pipeline today |
| **Termination / total correctness** | variants and well-founded measures | loop variant obligations exist; production completeness and recursive `#[decreases]` remain. This is not general temporal liveness |
| **General liveness** ("this good event eventually happens") | progress, fairness, eventual response | unsupported; requires an explicit transition/observation model rather than ordinary per-site VCs |
| **Functional correctness** | refinement against a spec | rung 5 + defined specs; reaches only single-`return` bodies |
| **Relational / hyperproperties** (constant-time, non-interference) | two executions compared | **not expressible** as a per-site obligation at all |
| **Resource bounds** (stack, alloc, time) | quantitative | stack reported not gated; alloc capability-gated; time untouched |

### Axis 3 — evidence dimensions

Evidence is a vector, not a single strength ladder. Record at least disposition (proved,
enforced, runtime-checked, tested, assumed, missing), semantic coverage, checker trust,
translation assurance, independence, composition closure and freshness. Several independent
kernels agreeing strengthens the checker-foundation dimension; it does not erase a shared
lowering, subject-binding or dependency-closure gap.

### The hard limits

**1. General automation has mathematical limits.** Termination and non-trivial semantic
properties are undecidable for arbitrary programs, although useful fragments admit sound
inference. Author-supplied invariants and well-founded measures are part of the interface, not an
admission of failure. The reachable promise is not automatic discovery of every true property;
it is an attempt for every well-formed property in a declared logic, with an explicit disposition
when inference, proof or support ends.

**2. Stronger logics make translation assurance harder, not inherently weaker.** Exhaustive
agreement is practical for small finite boolean domains; arithmetic and unbounded structures need
sampling, executable reference semantics, proof of selected transformations, certificate
checking or an explicitly trusted bridge. Kernel breadth and arrow assurance must advance
together, and the artifact must state what kind of assurance each arrow actually has.

**3. Backend-surface fidelity is not formally established today.** Renderer tests and target
parsing validate it. A formal target fragment and verified printer/parser correspondence could
strengthen that boundary later, so the roadmap must describe the current absence rather than call
the property unprovable.

### Honest scope of the destination

The charter means:

1. every admitted construct has a typed semantic representation or a precise refusal;
2. every well-formed property in a declared supported logic receives an explicit disposition;
3. callers and packages may rely only on exported contracts whose subject, dependencies, trust,
   environment and replay state satisfy their policy; and
4. unsupported constructs, observations and proof fragments remain named rather than silently
   disappearing.

This section does not own execution order. The [current execution queue](#the-current-execution-queue)
does; the [capability unlock map](#capability-unlocks--the-stable-product-map) states the durable
dependency chain and exit artifacts. The rung material below remains a feasibility record for
multi-kernel theory coverage, not a second priority list.

## Inherited Scheduling Constraints From Earlier Phases

These are not a second task queue. They constrain the stable-ID task
that owns each trigger; they remain here so moving completed phase bodies to the
changelog does not erase a still-relevant condition:

- **6C V1 hardening:** add remaining telemetry fields, pass hashes, and
  incremental-shadow manifest/edit coverage when a workload needs them.
- **ProofCore partial-def reduction:** moved to the soundness-bridge area, pulled only by
  preservation proofs.
- **CompilerDB / interned fact store:** moved to the incremental-driver area, pulled by the real
  incremental driver and relational fact consumers.
- **Consume-then-break linear restoration:** after value `while...else` removal,
  consume-then-break on an outer linear remains rejected; add a narrow exemption
  only if a workload needs that control-flow shape.
- **Qualified names/import aliases, FFI surface, target-profile roots, target
  constants, lint/vet, doc, bench, trace/debug, eval/inspect, LSP:** future
  usability/tooling work should be pulled by the stdlib area+ workloads or the later
  phase that owns the corresponding user surface.
- **Build-output convention:** R-0447 makes `.build/` (or the one equivalent
  project-local artifact root it ratifies) the only ordinary home for generated
  binaries, IR, reports, reductions, and proof artifacts. New source-directory
  outputs are forbidden immediately; legacy producers migrate when touched.

## Cross-Cutting Decisions

### Global Sequence And Evidence Discipline

The current execution queue is the sole scheduling authority. Task IDs identify
obligations; file order and area headings do not schedule them. Phases summarize
dependencies and permitted overlap, not additional integration capacity. The
[capacity allocation](#capacity-allocation) divides effort and integration slots between
the tracks: the verified compiler holds the majority and two slots, evidence integrity and
adversarial validation have protected minimums, and usability, interop, application,
incremental-driver and release work share one capped feature slot.
Every activated checkpoint records its accountable maintainer, required reviewer and exit.

Completed work moves to [CHANGELOG.md](CHANGELOG.md) under a `### Completed Task R-NNNN`
heading so the gate reserves that identity permanently.

Every user-facing claim should have a replay command, checked report, gate, or
proof. Unsupported cases must fail closed and loud. A feature that passes only
because no one tried to break its gate is not done.

### No Semantically Dark Constructs

Every construct is `proved`, `enforced`, `reported`, `assumed`, or `trusted`.
Authority, allocation, trust, runtime failure, byte/text/path boundaries, and
proof class must remain visible in source or audit output. Do not hide them
behind inference-heavy abstractions, implicit conversions, ambient lookup,
metaprogramming, or broad convenience APIs.

### Capability And Authority Visibility

Capabilities are part of the language's audit story. Imports do not grant
authority. Trusted/Unsafe internals may exist, but public wrappers must expose
the domain capability (`File`, `Env`, `Time`, `Network`, `Process`, `Console`,
`Alloc`, `Unsafe`, etc.) in signatures and reports.

### Allocation Visibility

`with(Alloc)` is allocation authority; allocator values name allocator identity.
The stdlib area allocator decision is two-tier: tier-1 defaults remain simple,
while allocator-specific `*_in` APIs use explicit allocator values when they are
pulled. Do not add an ambient implicit allocator.

### Scoped References, Not User-Managed Lifetimes

Safe references are second-class scoped access paths, not ordinary returned or
stored values. They may flow down into calls, callbacks, and borrow blocks, but
safe APIs should not return `&T` / `&mut T` or hide lifetime relationships in
data structures. This is the Hylo/Val-style value-semantics choice Concrete
adopts: users get in-place mutation, while the compiler manages the short-lived
borrow reasoning locally instead of exposing a Rust-style lifetime language.

API consequence: prefer operation APIs, owned values/views, handles/indices, or
scoped callbacks such as `with_value` / `with_value_mut`. Returning safe
references stays deeply deferred and evidence-gated; raw pointers are the
trusted/Unsafe escape hatch when low-level code must return an address.

> Moved to [ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md#compiler-pipeline-spine) (historical record; nothing in it schedules work).

### Future Compiler-Pipeline Refactor Contract

> **Reconciled 2026-10-19:** where this contract names QBE as a backend or a forcing consumer, read the
> LCB-native profile behind Concrete's BIR → LCB-IR adapter (R-0517); its QBE-first sequencing is
> superseded by the current queue and the verified-pipeline IR chain (Place MIR → Memory CFG → BIR).
> The refactor acceptance requirements remain in force; each refactor's owner is recorded in the
> [reconciliation record](docs/project/ROADMAP_RECONCILIATION.md).

The pipeline works, but several bug classes came from boundaries represented by
convention rather than types: raw module lists reused after different validation
stages, ordered first-match lookup tables standing in for scoped identity,
reports rescanning artifacts, duplicated CLI compile/link paths, and backend
emitters rediscovering program facts. Refactor these incrementally as real
consumers pull them—the QBE backend-validation area is the first forcing consumer—not as a flag-day
rewrite.

#### A. Make Successful Phase Boundaries Type-Safe

Introduce thin opaque wrappers whose constructors live with the pass that earns
the invariant. The intended ladder is:

```text
SourceInputs
  -> ParsedProgram
  -> ResolvedProgram
  -> CheckedProgram
  -> ElaboratedCore
  -> CanonicalCore
  -> MonomorphizedCore
  -> LoweredSSA
  -> ValidatedSSA
  -> CleanedSSA
  -> CodegenInput
```

Lean-like API sketch (the implementation should enforce opacity with module
boundaries/private constructors or an equivalent Lean mechanism; names may
adapt to the existing pipeline, but invariant ownership must not):

```lean
structure LoweredSSA where
  modules : List SModule

structure ValidatedSSA where
  modules : List SModule

structure CleanedSSA where
  modules : List SModule

def lower (mono : MonomorphizedCore) : Except Diagnostics LoweredSSA
def verifySSA (ssa : LoweredSSA) : Except Diagnostics ValidatedSSA
def cleanupSSA (ssa : ValidatedSSA) : Except Diagnostics CleanedSSA
def prepareCodegen (ssa : CleanedSSA) : Except Diagnostics CodegenInput
```

The wrappers are not proof theater: APIs that require verified SSA accept only
`ValidatedSSA`; emitters accept only `CodegenInput`; test helpers that
deliberately inspect invalid/raw SSA name that escape hatch explicitly. Do not add
`unsafeCastValidated`, public constructors, or ubiquitous `.modules` extraction
that immediately erases the boundary.

Example failure prevented: a new CLI path may no longer call cleanup/emission
on raw `SModule` output while silently skipping `SSAVerify`. The code should
fail to typecheck before a runtime gate is needed.

Gate: `scripts/tests/check_pipeline_types.sh` scans public signatures and runs
negative compile fixtures proving raw/lowered SSA cannot reach cleanup or
codegen, cleaned SSA cannot be fabricated outside its owning module, and the
normal file/build/test/debug paths traverse the complete typed ladder.

#### B. Give Passes One Observable Contract

Keep `Except Diagnostics` for fail-fast transformations where appropriate, but
standardize the metadata every pass can publish instead of rebuilding it in
CLI/report/debug code:

```lean
structure PassTrace where
  passName       : String
  inputHash      : String
  outputHash     : String
  elapsedMicros  : Nat
  counters       : List (String × Nat)
  changed        : Bool

structure PassFacts where
  schemaVersion  : Nat
  invariantNames : List String
  artifactKind   : String

structure PassSuccess (α : Type) where
  output : α
  facts  : PassFacts
  trace  : PassTrace
```

This does not require every pass to accumulate recoverable diagnostics or use
one giant generic monad. The contract is the observable success envelope:
diagnostics remain structured on failure; output, facts, trace, counters, and
fingerprints come from the pass once on success.

Before/after example:

```text
BEFORE: `--report verify`, debug-bundle, and `--trace-pipeline` each infer
        whether SSA verification ran by inspecting different output.
AFTER:  `verifySSA` publishes one `PassSuccess ValidatedSSA`; all three views
        render its versioned facts/trace without rerunning verification.
```

Gate: snapshot the pass sequence, schema versions, input/output hash chaining,
and counters for one success and one failure at every phase. Mutation tests must
make the trace/report/debug views fail together when a pass is skipped or its
artifact hash is swapped.

#### C. Derive Program Facts Once, With Stable Identity

Add one immutable, typed program-fact index before codegen. It should include:

```lean
structure FunctionId where
  moduleId : ModuleId
  localId  : Nat

structure CodegenFacts where
  functions       : Std.HashMap FunctionId FunctionFacts
  symbolOwners    : Std.HashMap LinkSymbol FunctionId
  moduleAliases   : Std.HashMap ModuleId AliasScope
  layouts         : Std.HashMap TypeId LayoutFacts
  strings         : Std.HashMap StringId StringFacts
  builtins        : Std.HashMap BuiltinId BuiltinCodegenFacts
  entryPoint      : Option FunctionId
  testEntries     : Array FunctionId
```

Use stable IDs for semantic identity and strings only for display/link symbols.
Ordered lists may preserve deterministic presentation order, but semantic lookup
must not mean “first equal bare name in program order.” Module-local aliases are
resolved in their owning scope before any program-wide fallback.

Bug-class examples this closes:

- bug 039: `std.env.get` cannot be rebound to `std.args.get` because both have
  distinct `FunctionId`s and aliases are keyed by importing `ModuleId`;
- duplicate basenames cannot collide merely because their generated strings
  match before qualification;
- builtin recognition uses `BuiltinId`, not a raw function name that user code
  can shadow;
- entry/test discovery is computed once and cannot differ between LLVM, QBE,
  `concrete test`, reports, and debug bundles.

Gate: generated programs permute module order, repeat local names/import aliases,
and create basename/link-symbol pressure. All permutations must produce the
same fact index hash, semantic observations, and deterministic emitted symbol
set. Inject duplicate semantic IDs and duplicate link symbols to prove the
builder rejects them with a diagnostic rather than selecting one.

#### D. Transform Once; Render Many Views

Compiler transformations own canonical artifacts and facts. Reports, audit,
queries, LSP, agent JSON, and debug bundles are views over those artifacts—not
partial compiler passes hidden behind output commands.

```text
canonical pass artifact
  + versioned facts
  + stable source identities
        |- diagnostics renderer
        |- `--report ...`
        |- audit JSON
        |- query API
        |- debug bundle
        `- LSP/agent views
```

Concrete example: layout is computed once into `LayoutFacts { size, alignment,
fieldOffsets, enumTag, payloadOffset, passMode }`. LLVM prints LLVM types from
it, QBE prints QBE aggregate declarations from it, `--report layout` renders it,
and the ABI gate serializes it. None independently walks `Ty` to rediscover
offsets.

Another example: proof/obligation status is read from the canonical obligation
ledger; `audit`, reports, diagnostics JSON, and editor output must not derive a
second status by scanning proof files.

Gate: an import-firewall/source-scan rejects report/query modules importing
transformation internals that are not declared artifact APIs. For selected
facts, mutate the canonical artifact and prove every view changes together;
mutate a renderer and prove compiler semantics do not change.

#### E. Centralize Semantic Legalization Ownership

Add a machine-readable table covering every Core/SSA operation and answering:

1. Where are source types and failure semantics fixed?
2. Where is the operation canonicalized?
3. Does SSA already express exact runtime semantics?
4. Is target-neutral legalization required?
5. What remains backend-specific instruction selection?
6. Which interpreter/external/mutation oracle covers it?

Example rows:

| Family | Exact semantics owned by | Shared legalization | Backend responsibility |
| --- | --- | --- | --- |
| checked `i32` add | integer-semantics facts | overflow result + trap edge | LLVM intrinsic/CFG; QBE explicit CFG/helper |
| signed right shift | typed Core/SSA signedness | normalized count policy | LLVM `ashr`; QBE signed word/long sequence |
| float-to-int cast | cast policy | finite/range guard + trap class | target conversion after guard |
| enum payload copy | layout + SSA copy intent | size/alignment/payload facts | independent LLVM/QBE memory operations |
| indirect call | resolved function type/caps | canonical signature/callee identity | target ABI call sequence |

Do not “single-source” backend instruction sequences: that would correlate LLVM
and QBE failures and weaken the oracle. Single-source language semantics and
facts; independently realize them in each backend.

Gate: generate the table from `SInst`, terminator, `Ty`, builtin, and trap
constructors. A new constructor fails CI until it names its semantic owner,
legalization class, interpreter status, LLVM/QBE status, and at least one test.

#### F. Finish Panic-To-Diagnostic Hygiene By Boundary

Sequence note: land 0d's split (LLVM spellings out of `Concrete/Check/Layout.lean`
into a backend-neutral `LayoutFacts` home) BEFORE converting the Layout panics —
converting them in the old location churns the same functions twice (review
2026-07-18).

The Layout conversion is the first slice, not the whole audit. Inventory and
remove compiler-owned panics/unreachable assumptions in:

- resolver and symbol/alias lookup;
- monomorphization specialization lookup/merge;
- Core canonicalization and CoreCheck environment lookup;
- lowering place/type/function lookup;
- SSA verification and cleanup rewrite assumptions;
- layout and aggregate field/tag/payload lookup;
- LLVM/QBE emission and builtin/runtime manifests;
- report/query handling of unknown/new constructors.

Internal invariant failures should report:

```text
error[internal:E9xxx]: codegen requested layout for unknown TypeId 41
  phase: prepare-codegen
  invariant: CODEGEN-TYPE-KNOWN
  function: app.parse_config
  source: src/main.con:27:14
  artifact: <debug-bundle path>
```

Do not turn impossible states into ordinary user errors. Classify them as
internal diagnostics, retain relevant artifacts, fail the command, and keep CI
leak gates rejecting `panic`, raw Lean exceptions, LLVM/QBE parser errors, or
host stack traces as the first diagnosis.

Gate: one controlled invariant-break fixture per boundary plus a static panic
inventory with an allowlist containing only process-fatal bootstrap/runtime
cases justified by owner and removal condition.

#### G. Make SSA Cleanup A Named, Testable Rule Pipeline

Replace opaque fixpoint behavior with an ordered registry of small rules while
preserving the existing semantics:

```lean
structure CleanupRule where
  name       : String
  apply      : SFunction -> CleanupResult
  invariants : List String

structure CleanupResult where
  function : SFunction
  changed  : Bool
  rewrites : Nat
```

Example rule sequence (adapt to the actual implementation): trivial-copy
forwarding, dead pure instruction removal, constant branch simplification,
unreachable-block removal, phi simplification, block merge, strength reduction.
Rules that require a fixpoint declare it; the driver records iteration and
rewrite counts and enforces a termination/fuel bound as an internal diagnostic.

Each rule needs:

- a minimal positive before/after golden;
- a negative fixture where it must not fire;
- verifier-before/verifier-after checks;
- interpreter/native observation preservation where executable;
- a mutation demonstrating the gate catches an unsound rewrite;
- deterministic output under block/function/module order perturbation.

Do not begin by rewriting all of `SSACleanup.lean`. Extract one existing rule at
a time with byte-identical cleaned SSA over the full corpus, then delete the old
branch in the same slice.

#### H. Unify All Compilation Entry Points

File compile, project build, `concrete run`, `concrete test`, emit-only modes,
and debug-bundle capture should configure one driver:

```text
prepare inputs
  -> frontend
  -> canonical Core
  -> monomorphize
  -> lower
  -> verify SSA
  -> cleanup SSA
  -> prepare codegen
  -> emit selected backend
  -> validate backend artifact
  -> assemble/codegen
  -> link
  -> optionally execute
```

Illustrative configuration:

```lean
structure CompileRequest where
  mode          : CompileMode       -- file | project | test | run | emitOnly
  backend       : BackendKind       -- llvm | qbe
  target        : TargetSpec
  optimization  : OptimizationMode
  retain        : ArtifactPolicy
  testFilter    : Option TestFilter
```

Modes may stop at a named boundary or substitute the test entry wrapper; they
must not reproduce the passes. `--emit-ssa` stops after cleaned SSA,
`--emit-qbe` stops after QBE validation, normal build links, and `run` executes
the linked artifact. Debug-bundle capture subscribes to artifacts already
produced by the driver rather than rerunning the pipeline.

Gate: a matrix covers file/project × normal/test/run/emit × LLVM/QBE-supported
subset. It verifies identical frontend/SSA hashes for equivalent inputs,
consistent diagnostics, artifact retention on each injected boundary failure,
and absence of duplicated direct emitter/tool invocations outside the driver.

#### I. Preserve Stable Source And Transformation Identity

Carry identity needed for diagnostics/evidence without attaching large source
objects to every instruction:

```lean
structure OriginId where
  sourceFile : SourceFileId
  nodeId     : SyntaxNodeId

structure TransformOrigin where
  primary    : OriginId
  family     : SemanticFamilyId
  parents    : SmallArray OriginId
```

Core/SSA operations that can trap, create obligations, cross capabilities,
materialize memory, or become backend calls retain an origin. Cleanup rewrites
preserve the primary origin and record combined parents where necessary. LLVM
and QBE runtime checks/debug bundles report the same source construct and
semantic family.

Example: a checked addition lowered into compare + branch + trap must keep one
`checked_add` family ID and source span through cleanup and both backends; a QBE
mismatch bundle should not point only to a generated temporary or `.qbe` line.

Gate: source-span/identity goldens for direct instructions, inlined helpers,
phi/block merges, aggregate copies, generated runtime checks, and
monomorphized functions. Moving a source file must not change semantic IDs that
are defined as position-independent; editing the owning construct must change
its freshness/fingerprint where required.

#### J. Generate Consumer Coverage From IR Constructors

Maintain a compiler-generated matrix, not a manually curated checklist:

```text
constructor | Check | CoreCheck | Interp | Lower | Verify | Cleanup
            | LLVM | QBE | reports | source identity | tests
```

“Handled” must distinguish `implemented`, `intentionally_noop`,
`rejected_before_here`, `runtime_helper`, `pending`, and `not_applicable`.
Catch-all pattern matches do not count as coverage unless the constructor is
explicitly classified.

Example: adding a new `SInst.atomicRmw` in the future should immediately fail
the matrix because interpreter semantics, capability/memory-model facts,
cleanup side-effect classification, LLVM, QBE, reports, and tests have no rows.
It must not silently inherit `_ => unchanged` in cleanup and `_ => panic` in an
emitter.

Gate: constructor extraction is fail-closed and mutation-tested by adding a
synthetic constructor in a fixture copy or removing one coverage row. Publish
the matrix through agent features/debug bundles so CI and external tooling use
the same inventory.

#### Scheduling Map And Definition Of Done

The long-term contract is scheduled, but it is not one prerequisite chain and
must not delay the stdlib area exit. Implement each item as a separately green,
runnable slice in these phase-owned sequences:

- **The QBE backend-validation area backend sequence:** extract target-neutral `LayoutFacts`; define
   the typed deterministic QBE fragment model; establish the minimal immutable
   `CodegenInput`/backend-neutral driver required by the first `main -> 42`
   vertical slice; then grow the real QBE backend and generated capability
   matrix. Convert Layout panics after the ownership split. An LLVM fragment
   migration is optional and remains bug-027 profile/pull-gated—building typed
   QBE fragments does not itself fix LLVM's concatenation cost.
- **The flagships area architecture sequence:** introduce stable semantic identities;
   build immutable indexed program facts; add thin typed pass-boundary wrappers;
   replace the temporary manifest scanner with compiler-derived interface
   facts under a comparison gate; then generate the complete consumer/coverage
   matrix from those facts. Alpha-renaming remains the completed fix for bug
   045; IDs prevent future name-as-identity failures rather than relabeling that
   fix as temporary.
- **Incremental consumers:** move reports/debug bundles to canonical pass
   artifacts, carry source/semantic identity into runtime checks and backend
   bundles, extract SSA cleanup rules one at a time, and finish remaining
   panic-to-internal-diagnostic boundaries as their owning facts stabilize.
- **The backend-contracts area:** add translation validation and only then consider owning a
   target MIR/native encoder beyond the real QBE backend.

For every refactor slice:

- name the bug class or immediate consumer;
- preserve byte-identical LLVM/Core/SSA artifacts by default;
- if bytes change, explain the exact delta and prove behavior/evidence parity;
- run verifier-before/after and the relevant full/differential/fuzz gates;
- add a mutation or negative fixture that fails without the new invariant;
- delete the superseded path in the same slice or give it a dated retirement
  gate;
- record compile-time/memory impact and reject unbounded regressions;
- keep unrelated feature work out of the refactor commit.

The refactor program is complete when all production entry points traverse the
typed ladder and unified driver; semantic lookup uses stable identity; canonical
facts feed every view/backend; every constructor has fail-closed consumer
coverage; cleanup rules are named and mutation-sensitive; compiler-owned
invariant failures are structured diagnostics; LLVM remains green; and the QBE
backend consumes the same immutable codegen input without importing LLVM logic
while retaining its independent-oracle test duty.

Explicit non-goals: no wholesale rewrite, no universal pass monad, no dynamic
backend plugin framework, no new source language, no duplicate ZIR/AIR layer,
no target MIR/register allocator/object writer before a direct-native forcing
case, no report-specific compiler pipeline, and no claim that typed wrappers or
backend agreement constitute a proof of compiler correctness.

### Incremental / Certificate Trust Split

Cache hits are optimizations, not proofs, and cannot upgrade evidence classes.
The incremental-driver area starts as conservative cache reuse over successful artifacts. The soundness-bridge area
adds an independent Core certificate checker for named predicates; the backend-contracts area
extends the statement only through supported backend-translation slices. Parser,
resolver, unproved source-to-Core correspondence, LLVM, linking, runtime, OS,
and hardware remain trusted until separately checked.

> Moved to [ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md#external-validation-gate) (historical record; nothing in it schedules work).

### Early Risk Probes

Four verdict-deciding risks get narrow early probes:

- **External credibility:** deterministic `diff-caps` style authority-diff
  artifact before the full flagships-area trial.
- **Proof cost:** the proof-authoring area proof-effort telemetry on the first real proof slice.
- **Backend soundness:** narrow the backend-contracts area translation-validation slice over a
  small supported subset.
- **Pipeline scale:** old phase 6C and the flagships area shadow invalidation transcript before real
  artifact reuse.

### Floats And Proof Profiles

`f32`/`f64` are usable runtime types but outside `ProvableV1` unless a future
explicit float profile is selected. Float proofs require named IEEE semantics,
backend fast-math restrictions, and an honest trusted/checked primitive layer.

### Embedded / Freestanding Direction

Embedded and freestanding support remain later-phase work. Inline asm is
trusted/Unsafe; MMIO/volatile access needs explicit capabilities; `Device` is
reserved for real freestanding/MMIO code and should not be added before a
consumer exists.

### Target-Conditional Code

Prefer target/profile-selected source roots and modules in `Concrete.toml` over
in-source conditionals. If narrow `cfg` attributes are ever admitted, they must
be audit-visible target/profile facts, not hidden preprocessor state.

> Moved to [ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md#native-debug-info) (historical record; nothing in it schedules work).

### Contract / Spec Trust

Specs and contracts carry evidence classes too. The system must avoid vacuous
proofs, impossible preconditions, partial ghost/spec functions, and generic
proofs whose scope is unclear. Proof evidence is relative to the toolchain that
checked it and must detect drift.

### Stdlib Direction

The stdlib should be small, explicit, and auditable: Gleam/Roc-sized coherence,
Hare-style restrained systems modules, Zig-style allocator/authority
explicitness, Rust-style `Option`/`Result` and layering where useful, Ada/SPARK
evidence discipline, Go-style `Reader`/`Writer` simplicity, and Odin-style
pragmatic breadth only after workload pressure.

## Architecture Decision Index (Non-Executable Context)

This preserves the conclusions of the 2026-07-18 18-item architecture review.
It is not a task list and does not define execution order; every open conclusion
is implemented by one of the stable-ID tasks below. The unifying
rule: **`pub` is the ONLY visibility keyword — everything is module-private by
default, `pub` exports a declaration/field/variant, and exporting a type does
NOT export its representation.** No `private`/`sealed`/`opaque`/`transparent`.
Every decision below is carried by a numbered task; these topic labels do not
create ordering or an alternate queue.

### Stdlib Boundary Decisions
- **Construction rights:** enum-variant privacy is a separate scheduled slice:
   it must define
   construction, matching, imports, exhaustiveness with inaccessible variants,
   and migration diagnostics. Syntax (D2): keep `pub newtype N = u32` (NO
   parentheses); variant marker is `pub Ok { value: T }`. `pub` stays the only
   visibility word.
- **Borrow versus ownership:** split `BytesRaw` into `Slice<u8>` (borrow) and
   `BytesParts` (ownership transfer); a borrowed view and owned allocation parts
   must not share an ambiguous destruction contract. Inventory
   producers/consumers first, then split with destruction-counter tests.
- **One IO spine:** Files/streams/sockets/buffers on shared Reader/Writer;
   keep parser-oriented `read_all -> Bytes`, ADD `read_to_end -> Result<Bytes,_>`
   for error-preserving completion. Migrate vertically through workloads.
- **Boundary validation:** validate UTF-8 ONLY where an API promises text
    (CORRECTION: network/process expose `Bytes` by DEFAULT; validate only in
    explicit text adapters). args/env already validated.
- **Checked C-string boundary:** `to_cstr -> Result<CString, CStringError>`;
    interior NUL must not silently truncate. Land it in vertical slices:
    add checked conversion + CStringError, keep a narrow internal `_unchecked`,
    migrate all FFI sites, test interior-NUL/empty/alloc-fail/exactly-once,
    then remove/privatize the infallible public conversion.
- **Hosted adapters absorb unsafe:** callers request domain caps, not `Unsafe`;
    raw-pointer APIs stay private/explicitly-unsafe. Unblocked now that manifest
    facts come from the repaired balanced scanner. This is the
    raw-ptr⇒Unsafe gate (the hosted-adapter rule).

### Linearity Decisions
- **Centralized linear conservation:** one checker enforces exactly-once across
   branches/matches/returns/loops/callbacks/destructuring; consuming
   Option/Result combinators in Task R-0012 use this machinery, with no special-case rules
   (verified). Add an assertion fixture.
- **Trusted is not implicit linear copy:** local/binding copies are rejected (verified
   E0205; pin permanently), but raw-pointer dereference can still duplicate a
   non-Copy owned value: bug 046's `*(p : *mut K)` shape compiles today. Schedule
   a checker slice that rejects dereference-as-owned-duplication unless code uses
   an explicit audited ownership-transfer primitive such as `read_owned`; test
   both paths rather than claiming trusted linearity already holds generally.

### Backend-Separation Decisions
> **Reconciled 2026-10-19:** LayoutFacts is now R-0502's single `Layout` authority and typed emission
> is R-0503's BIR; the forcing consumers are the LLVM adapter and the LCB adapter (R-0517), and the
> "the QBE backend-validation area order" below is historical.

- **Backend-neutral LayoutFacts** (size/align/offsets/tags/ABI/legal scalar),
    separate from LLVM spelling — the opening slice; QBE is the forcing consumer.
- **Typed emitter fragments** (structured defs/blocks/instrs/terminators, rendered
    once); LLVM and QBE share rendering + neutral facts, NOT instruction
    selection. CORRECTION: this does NOT fix bug 027 in the LLVM emitter — that
    happens only if LLVM later adopts the fragment model (profile/pull-gated).
- **Structured diagnostics only** (panics→diagnostics) — the Layout panic
    conversion, reordered behind the LayoutFacts split (do the split once).
    the QBE backend-validation area order: LayoutFacts → QBE fragment model → QBE backend → Layout
    panic-to-diagnostic → optional LLVM fragment migration (only when pulled).

### Compiler-Architecture Decisions

Each migration is independently runnable, never a compiler-wide rewrite; the
compiler stays runnable and tested after every slice.
- **Stable semantic IDs:** alpha-renaming remains the demonstrated binder fix,
   while the larger 039–055 audit family justifies retiring name-as-identity:
   import/alias capture (039, 044, 055), binder/type/import tables keyed by text
   or first-match position (040–042, 045), indirect-call hijacking (050),
   generic-enum declaration/layout identity failure (051), and non-injective
   monomorphized names/layout lookup (054). Their narrow fixes remain necessary;
   stable IDs prevent the family from recurring at the next boundary. Bug 054's
   immediate collision/layout fix lands in Task R-0007; the flagships area retires semantic
   identity-by-name across the pipeline.
- **Compiler-derived interface facts:** supersede the repaired manifest scanner.
   Keep the scanner until Task R-0117; the compiler artifact becomes
   authoritative, compared against the scanner during migration before deletion.
- **Typed pass boundaries:** ParsedProgram → … → VerifiedSSAProgram.
- **Immutable indexed program facts:** modules/defs/layouts/builtin identities/
    target) consumed by passes instead of re-scanning / first-match lookup.
- **Self-enforcing coverage matrix:** derive it from compiler facts.

Their execution order is expressed only by Tasks R-0114–R-0118: stable IDs → immutable
indexed facts → typed pass boundaries → compiler-derived interfaces → generated
coverage matrix.

### Slice Decision
- **Slices as the safe buffer interface:** `&Slice<u8>`/`&mut MutSlice<u8>` on
   hosted read/write; raw-pointer forms private) — the `&Bytes` net-I/O pull
   (currently 2nd ask).


The corresponding local/CI gate work is scheduled as a stable-ID task
below; this index carries no executable work.
