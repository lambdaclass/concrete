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
only when three consecutive probe batches find about nothing), which is why
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
until that owner removes their class. Implementation and experiments on every track may proceed now:
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

**Open bugs: 17** (6 silent, 5 loud, 6 filed before the discovery record) of 138 bug records.

| open bugs by primary structural class | open | silent | bugs |
|---|---|---|---|
| 1. Declaration identity by spelled name, not a resolved `DefId` | 6 | 2 | 054, 077, 114, 118, 122, 165 |
| 2. Binder identity by name, not a `LocalId` | 1 | 0 | 124 |
| 7. Semantics implemented several times (interpreter, proof model, folder, backend disagree) | 5 | 3 | 117, 128, 134, 136, 137 |
| 11. Absence read as a fact (fail-open defaults) | 2 | 0 | 049, 058 |
| 12. One fact kept in two places | 1 | 0 | 064 |
| 14. Trusted `std` and FFI code with no checked contract | 1 | 1 | 166 |
| no class yet | 1 | 0 | 027 |

| open bugs by Phase 1 exit | open | silent | bugs |
|---|---|---|---|
| exit 0b | 6 | 6 | 122, 128, 136, 137, 165, 166 |
| exit 2c | 5 | 0 | 114, 117, 118, 124, 134 |
| no exit names it | 6 | 0 | 027, 049, 054, 058, 064, 077 |

| discovery rate: counting probe batch | date | effort | found | silent | loud | signal |
|---|---|---|---|---|---|---|
| hunt: name-resolution, collision, model-vs-runtime, scope, literal, checker-ownership and red-team hunts; type-identity and generic-destroy review probes | 2026-10-18 | unrecorded | 19 | 15 | 4 | found bugs |
| workload: RocksDB slice 1 groundwork | 2026-10-20 | unrecorded | 2 | 2 | 0 | found bugs |
| design-proofs: V1 design examples (design question B6) | 2026-10-20 | unrecorded | 1 | 1 | 0 | found bugs |
| examples: examples-health pass: port and output-test every example | 2026-10-20 | unrecorded | 13 | 4 | 9 | found bugs |
| spec: exit 2i specification (MEMORY_SEMANTICS section 15) | 2026-10-21 | unrecorded | 3 | 2 | 1 | found bugs |
| semantics: semantics probes N1-N4 and reference rules | 2026-10-21 | unrecorded | 5 | 4 | 1 | found bugs |

Discovery signal (exit 2m): 0 consecutive eligible counting batches are quiet (no silent bug, at most one loud, effort at least 4 hours or 10 programs); the exit needs 3. 6 counting batches recorded. 11 attribution-only batches (reviews, follow-ons, CI, controls, development) found 17 further bugs.

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
bodies are under [Area: Development Throughput](#area-development-throughput). The track is charged to
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
| **Phase 1 — Stabilize** | Land the 2026-10-18 soundness repairs, close R-0482, put the immediate protections in place | 0 – 2 | every [Phase 1 exit](#phase-1-exits) (0a – 2m): repairs gated; R-0482 qualified; one `Verdict` authority; language rules enforced | 0.1 Experimental systems preview |
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
| 2 | R-0491, R-0493, R-0494, R-0498, R-0500, R-0506, R-0513 | Immediate protections (exits 2a – 2m): one verdict authority, prevention harness, trustworthy gates, axiom policy, one `IntArith`, language rules, demonstrated enforcement |
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

- **Phase 1 — Stabilize** (rows 0 – 2). **Goal:** Land the 2026-10-18 soundness repairs (080 – 101), close the R-0482 identity migration, and put the immediate protections in place: one verdict authority, the prevention harness, demonstrated enforcement, the language decisions, one `IntArith`, the axiom policy and trustworthy gates. **Exit:** every [Phase 1 exit](#phase-1-exits): each repair's reproducer is a permanent gate; R-0482 qualified and its P1 controls permanent; one `Verdict` authority and the written language rules enforced. Feature work starts now and is accepted when its [prerequisites](#feature-prerequisites) hold. **Release:** **0.1 Experimental systems preview**, with the usability onboarding path and first pilot.
- **Phase 2 — Verified foundation** (rows 3 – 4). **Goal:** The first proved end-to-end slice (V1a Core → BIR, V1b native AArch64 through the Lean Compiler Backend, V1c restricted source → checked Core correspondence, with design proofs for ownership, borrows and memory first), and verified-pipeline Part 1's structure for the whole language: identities, the authoritative checker on Core, one `Layout` and Place MIR in the existing profile. **Exit:** V1a and V1b theorems with explicit boundaries and V1c's correspondence theorem; the Core checker authoritative for every program; the existing profile's layout from one `Layout` and its lowering jobs in Place MIR passes. **Release:** **0.2 Verified preview** (with Phase 3's tooling checkpoints); the candidate states where the theorem begins.
- **Phase 3 — Usable language** (rows 5, 7, 8). **Goal:** Bounded C/Rust/Zig interop, real application workloads leading to the ethlambda port, and the usability track: onboarding, external pilots, diagnostics, editor and debugging, published performance. **Exit:** the four-adapter interop checkpoint; continuing workloads and pilots; each usability checkpoint's exit. **Release:** no release of its own: it runs alongside and feeds every release from 0.1 on (onboarding, pilots, tooling, the minimal editor path for beta).
- **Phase 4 — Verified growth** (row 6). **Goal:** Cumulative slices V2 – V7 (aggregates and references, heap and linear resources, generics and capabilities, raw pointers and FFI, the rest of the language) each with its theorem, and eligible claims migrated onto compiler-generated Core statements independently of backend coverage, with explicit deprecation for retired claims and historical replay preserved. **Exit:** each slice's cumulative theorem; V7 covers the maintained feature inventory. **Release:** **0.3 – 0.8 Verified-subset milestones** (a slice may span several releases); V7 is required for 1.0.
- **Phase 5 — Evidence as a product** (rows 9 – 11). **Goal:** Compiler provenance and one canonical verification task, the independent evidence consumer, and the receipt v1 freeze. **Exit:** receipt v1 frozen after producer and independent consumer agree on hostile fixtures. **Release:** **0.9 Verified-component alpha**, together with Phases 4 and 6 for the admitted component.
- **Phase 6 — Richer verification** (rows 12 – 15). **Goal:** Typed contracts and exact contract identity, two-state (`old`, frames) contracts, the ninth-table conversion, and totality with a specification library. **Exit:** typed contracts with exact identity, `old`/frames, the ninth table converted, checked totality and the spec library. **Release:** **0.9 Verified-component alpha** (its typed-contract, two-state and required totality increments).
- **Phase 7 — Release and ecosystem** (rows 16 – 17). **Goal:** External-user workflow and three graduated flagships, then package-level evidence and typed policy. **Exit:** a non-author replays and upgrades with exact diffs; three flagships graduate; package evidence composes under typed policy. **Release:** **0.10 Project beta**, then **1.0 Stable supported subset** (V7 with source correspondence, Phases 5 – 7's acceptance, release-bar compatibility and post-release readiness) and **1.x** feature releases.

The [verified-compiler plan](docs/compiler/VERIFIED_PIPELINE.md) holds Phases 2 and 4 in detail; the
task sections below hold every row's exit.

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
class (R-0491's [adversarial example suite](#task-r-0491)), an upcoming deliverable (the
[RocksDB slice 1 companions](#task-r-0285), [V1a's target programs](#task-r-0511)) or an application
workload (R-0490). All example work follows R-0135's lifecycle and owner policy: user-facing examples
record their lifecycle and owner, while pure regression and rejection fixtures live in the owning
test or bug corpus and are invoked by the example's gate.

| order | work | exit before advancing |
|---|---|---|
| 0 | **Active soundness repairs and R-0482 qualification** | close bug 079's subject/model/spec binding at status, issuance and receipt consumption; land the bug 080 cast model and the 081 – 095 repairs (evidence, model/runtime, name-resolution and checker memory/capability-safety defects from the 2026-10-18 hunts), each with its reproducer as a permanent gate leg; repair campaign isolation and retain a qualified result naming its exact commit before closing the migration. Closes through exits 0a, 0b and 0c ([Phase 1 exits](#phase-1-exits)) |
| 1 | **R-0482 P1 cross-package closures** | promote the manifestful std/dependency controls into permanent gates, retain D3's manifestless distinction and negative controls, and publish only after the subject-binding repair. Closes through exit 1a |
| 2 | **Immediate protections and semantic decisions (R-0491, R-0493, R-0494, R-0498, R-0500, R-0506, R-0513)** | closes through the individually closable exits listed in [Phase 1 exits](#phase-1-exits), each with its owner, acceptance test and class: one `Verdict` authority renders every evidence surface (2a); permanent reproducer controls for 080 – 101 (2b) and the prevention harness (2c); trustworthy gate execution — checked inventories instead of incidental count pins (2d), minimum completed comparisons (2e), a fast push tier separate from full qualification (2f); the axiom-policy gate on today's evidence (2g); one `IntArith` for the existing pipeline (2h); the rules the next slice needs written down and enforced with versioned migration diagnostics — strictly second-class references and no block `let` shadowing (2i); typed literals, literal range, statement vs value position, sequential assignment within a loop iteration, evaluation order and short-circuiting (2j); explicit match binders and cast semantics per width (2k); demonstrated enforcement (2l); the bug-discovery rate at about zero across three consecutive probe batches (2m). Known authority and safety defects are repaired under row 0. An exit classed *blocks features* holds back the acceptance of only the deliverables whose [prerequisites](#feature-prerequisites) name it; the others continue alongside feature work. Broader prevention coverage continues throughout and does not block isolated proof development |
| 3 | **First cumulative verified releases (R-0511, with R-0517)** | reviewed machine-checked design examples for move/destroy, borrows, reborrowing, cleanup and typed-value memory representation before Core's interfaces freeze; V1a (checked Core → BIR refinement for the minimal scalar subset) and V1b (native AArch64 through the Lean Compiler Backend, R-0517, with final-image correspondence; LCB's Stage 1 exit is necessary but insufficient — V1b needs a pinned certified LCB domain covering every reachable V1 operation and behaviour, including checked-arithmetic trap paths, direct calls and `print_int`, plus Concrete's proved adapter and semantic bridges; a smaller leaf-function intersection ships only as a separately named intermediate theorem; R-0517's explicitly selected fallback does not complete this row) with explicit theorem boundaries; checker soundness, capability containment, resource and target-memory-validity obligations and one transported claim for the admitted subset; V1c, restricted source → checked Core correspondence for V1's surface (syntax, resolution and elaboration judgments specified independently of their implementations, then proved or certificate-checked; constructs outside the subset rejected explicitly), before any V1 result is called source-to-native; retained artifacts with minimal task identity and independent replay. The design examples and isolated proof development start now; V1a, V1b and V1c are accepted only when their prerequisite exits hold (prerequisite exits: 0a, 0b, 2a, 2g, 2h, 2i, 2j; the programs each must compile and the independently checked artifact are under [feature prerequisites](#feature-prerequisites)). See [the plan](docs/compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices) |
| 4 | **Shared compiler structure: identities, the authoritative checker on Core, one `Layout` and Place MIR for the whole language (R-0492, R-0497, R-0499, R-0514, R-0502, R-0501, with R-0355's baseline)** | verified-pipeline Part 1, in the primary track's migration slot ([capacity allocation](#capacity-allocation)), alongside row 3: before the first whole-language change of R-0492, R-0499/R-0514, R-0502 or R-0501 lands on main, R-0355's [development-speed baseline](#task-r-0355) is recorded, so each structural change's cost to build time, rebuild time, compiler memory and test turnaround is visible; the parts V1a's subset exercises proceed with V1a, and the whole-language rollout starts once row 0's in-flight candidates (batch 2, type-identity, generic-destroy) are accepted on main, which edit the same checker code; it implements the rules as exits 2i – 2k state them and does not wait for row 2's other exits; the front end both profiles share uses `LocalId`/`DefId` from Resolve on, including the source checker's environments, with immutable payload-bound artifacts; one checker over Core implements every ownership, borrow, reference-escape, capability and `Unsafe` rule for the whole language, emits payload-bound `CheckedCore` consumed by checked specialization in both profiles; specialization produces payload-bound `CheckedMonoCore`, the only input to lowering; the source-tree checker holds no such rule; the 087 borrow-block case and 090 – 094 are rejected by it; capabilities are re-checked after Mono. Then, in the same slot: one `Layout` authority (R-0502) supplies every size, alignment, offset and enum representation the existing lowering and LLVM emission use, with a gate comparing it with the emitted types (class 12 of the [structural classes](docs/bugs/STRUCTURAL_CLASSES.md)); and `Lower`'s sequencing, match compilation, cleanup/`defer` and check expansion move into Place MIR passes (R-0501) that consume `CheckedMonoCore`, as shadow paths with disagreement gates against the existing lowering (class 8). Their semantics and proofs still grow per slice (rows 3 and 6), and the old `Lower` retires only under the plan's retirement conditions. The slices prove this checker's rules as they reach them (V1 – V7, beginning with scalar typing and basic capabilities in V1, downward borrows in V2, and extending through resources, polymorphism and unsafe/FFI interactions); this row makes it authoritative for every user now |
| 5 | **Bounded interop track (R-0285, with R-0283/R-0286/R-0287)** | the one bounded executable workload beside the verified compiler, in the single feature slot ([capacity allocation](#capacity-allocation)): first deliver **RocksDB slice 1**, the first executable feature deliverable: implementation and experiments proceed now (its groundwork on main already found bugs 102 and 103), and it is accepted, and its safety advertised, only when its prerequisite exits hold (prerequisite exits: 0b, 2i; checklist and acceptance under [feature prerequisites](#feature-prerequisites)); then deliver the four-adapter checkpoint, after R-0515's practical debugging checkpoint and with the clean-machine upgrade-and-roll-back exercise (RocksDB C, Rust crypto/networking, an upstream Zig library; no application bridge/build scripts, generated-file edits or manual flags) on the supported existing profile alongside verified-pipeline work; foreign implementations remain named trust dependencies; interop neither waits for nor blocks the first native theorem |
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
| 0b | R-0482 | the wrong-code and memory/capability-safety repairs land: 052, 082, 087, 088, 090 – 095, 096 – 100, 102 – 105, 107, 111, 113, 115, 121 – 123, 126 – 129, 131 – 133, 135 – 137, 165, 166, and the silent defects found while writing the [2026-10-21 semantics decisions](#decisions) into the docs, once they are filed in the bug ledger: an arm binder through a borrowed scrutinee whose root is moved inside the arm ([MEMORY_SEMANTICS §15.7](docs/language/MEMORY_SEMANTICS.md) G17), the binary evaluating a dereference assignment's destination before its right-hand side while the interpreter does the reverse ([evaluation order](docs/language/STATEMENT_EXPRESSION_MODEL.md#evaluation-order-and-assignment)) | each bug's reproducer is a permanent gate leg, and the bug ledger records each fixed on main | blocks features | use-after-free, double free, skipped destruction and capability escape in safe code (052, 087, 090 – 093, 099 – 100, 091, and 104 and 131: a reference assigned from an inner block outlives its source, to a local or to a reference parameter), a safe user function named like a C symbol replacing it for the whole program (129: `fn malloc` becomes the allocator `alloc` calls), double free through `vec_get` taking a non-Copy element by value (115), silent wrong code (082, 088, 096 – 098, 102, 107, 111, and 123: a borrow of a module constant reads an uninitialized slot; 127: struct-literal initializers run in declaration order natively and in the ratified source order in the interpreter; 128: the interpreter, the differential reference, models an integer-to-float cast as the identity; 133: a write through `&mut (*p).f` lands in a copy of `*p`; 136: functional update `S { .., ..base }` evaluates its base once per omitted field; 137: enum-variant literal fields run in declaration order in both consumers; 105: a reference-valued `if`/`match` reached through a generic instantiated at a reference type, which the checker containment did not cover, segfaulted natively), a silent semantic change by a tool (135: `concrete fmt` rewrote float literals to six decimals, so `0.0000000001` became `0.000000`), missing extern effect declarations accepted in nested submodules (113; calls still charged `Unsafe`), a foreign destructor silently replaced by an empty body (103, a leak of every C resource released through a `*_destroy` function), and LLVM type layouts that disagree with the compiler's own, so aggregate copies move fields (121: conlog freed a corrupted String pointer; 122: the canonical builtin `Option`/`Result` union, including a stack overrun); the defects found on 2026-10-21 are a read through an arm binder after its source moved, and an interpreter/native disagreement on dereference-assignment order; and heap-slot corruption through a `&mut Heap<T>` argument (126: the callee treats the caller's heap-pointer slot as the struct, so a field write overwrites the slot and `free` receives a corrupted pointer) |
| 0c | R-0482 | campaign isolation is repaired and a qualified campaign result naming its exact commit is retained before the identity migration closes | a [stage (c)](#promotion-pipeline) qualification record with `verdict=qualified` at that commit: every required job succeeded there, the mutation aggregate's `qualified=1` among them | continues alongside | qualification of the identity migration; no deliverable depends on it |
| 1a | R-0482 | the manifestful std/dependency controls are permanent gates, D3's manifestless distinction and its negative controls are retained, and the result is published only after 0a's subject-binding repair | the P1 controls run as permanent legs of the full gate set, and the manifestless negative controls fail when the D3 distinction is collapsed | continues alongside | cross-package claims are not a dependency of RocksDB slice 1 or V1 |
| 2a | R-0494 | one `Verdict` authority renders every evidence surface (contracts, vcs, obligation-ledger, audit, check-proofs text and JSON, proof-status, issuance) | R-0494's exit: status spellings only in the renderer table; bug 089's reproducers (`rt_ens_only`, `rt_ens`, `rt_ens_bogus`, `rt_ens_noens`) read not-proved on every surface; a cross-surface gate (new) pins the expected positive and negative judgment for every claim and clause | blocks features | the proof surface is feature-frozen until it lands ([verified-compiler decision](#decisions)), so V1's published claim needs it |
| 2b | R-0491 | permanent reproducer controls for 080 – 101: each bug has a checked-in record, reproducer, expected outcome and pre-fix/fixed SHA pair | each assigned control fails at its pre-fix SHA and passes at its fix; `check_bug_filing_complete.sh` and `audit_bug_corpus.sh` fail on a missing inventory entry | continues alongside | the reproducers already block as gate legs under 0a and 0b; this exit proves each one load-bearing |
| 2c | R-0491 | the prevention harness (S1): the extracted model, then Core's semantics, as a third side of the differential fuzzer over every shape that broke, outcomes classified and never skipped, and model-diff for zero-argument, `bool` and aggregate signatures plus a binary leg with a pinned claimed-function inventory; and the adversarial example suite for today's bug classes; and closing the loud failures that keep valid examples out of the interpreter/native comparison (108 – 110, 112, 114, 116, 124, 130, 134, and interpreter gaps 117 – 120); permanent differential legs for struct- and enum-literal field order, functional-update evaluation, assignment order and integer → float conversion ([2026-10-21 semantics decisions](#decisions)); and every example golden independently checked: each `@run` rule declares its oracle (`interp`, `spec:<pointer>` or `unverified:<reason>`, [GOLDEN_ORACLES.md](docs/project/GOLDEN_ORACLES.md)), and the unverified goldens the interpreter gaps leave are retired by interpreter agreement or a recorded derivation | R-0491's detection map: the deterministic differential suite detects 080 and 084 – 088, name-binding controls 081 – 082, evidence and cross-surface controls 083 and 089, checker mutants 090 – 094 and the binary leg 095 (`check_model_diff.sh`, `fuzz_differential.py`); and each of the eight examples of R-0491's [adversarial example suite](#task-r-0491) (`linear_flow`, `generic_destroy`, `nested_names`, `import_aliases`, `casts_widths`, `mut_receivers`, `match_and_fnptrs`, `statement_sequencing`) passes its own leg of `scripts/tests/check_adversarial_examples.sh` (new, in the full gate set), comparing interpreter output, native output and the expected output, plus the proof model and claim statuses where the example carries claims; each bug assigned to this exit (108 – 110, 112, 114, 116, 117 – 120, 124, 130, 134) has its reproducer permanently checking the corrected expected outcome, and the bug ledger records each fixed on main; `check_example_outputs.sh` fails on an undeclared oracle (with negative controls) and reports zero unverified goldens | continues alongside | prevention plus repairs of loud compilation/link failures and interpreter gaps, without a demonstrated authority or safety defect; the generator coverage 2i – 2k need is part of their own acceptance; the examples add prevention coverage, while existing repairs and containment controls keep their owners and acceptance; a defect an example newly reproduces enters the queue under the entry rule, as a new row-0 exit when it is an authority or safety defect |
| 2d | R-0513 | checked inventories replace incidental count pins: every expected claim and comparison is accounted for, exclusions carry reasons, and missing entries, skipped work or unexpected eligibility shrinkage fail | adding a legitimate proved example needs no incidental count edit; controls with a deleted inventory entry, a disabled generator, an empty suite and altered eligibility each fail | continues alongside | gate affordability; no reproduced soundness defect |
| 2e | R-0513, R-0491 | minimum completed comparisons by semantic feature, with permanent positive and negative controls | every generated feature has a pinned minimum completed-comparison count, and a run below it fails | continues alongside | prevention; no reproduced defect |
| 2f | R-0513 | a fast push tier separate from full qualification | the fast tier meets its recorded budget (target five minutes); full results bind exact commits, toolchains and artifacts; a fast-tier pass is never presented as full qualification ([promotion pipeline](#promotion-pipeline)) | continues alongside | validation cost; no deliverable depends on it |
| 2g | R-0506 | the axiom-policy gate on today's authoritative evidence at status, issuance and consumption: `sorryAx` and unregistered axioms rejected transitively, kernel-only evidence limited to `propext`, `Classical.choice` and `Quot.sound`, registered native-evaluation and float axioms under distinct trust labels | positive controls and forbidden-transitive-dependency mutations, with `check_axiom_inventory.sh` | blocks features | V1's evidence must pass the declared axiom policy (R-0506 is a stage V1 exercises) |
| 2h | R-0493 | one `IntArith` is the arithmetic authority for the existing pipeline's evaluation, folding and arithmetic-model generation, with emitted operations checked against the same semantics | `check_single_arithmetic.sh` (new) and differential controls over every supported width and operation family, including casts and defined traps | blocks features | V1a's checked `+ - *` and their traps are stated through it (R-0493 is a stage V1 exercises) |
| 2i | R-0500 | the safety rules of [MEMORY_SEMANTICS §15](docs/language/MEMORY_SEMANTICS.md) are written down and enforced: strictly second-class references — they exist only as parameters, call arguments, borrow-block binders and arm-scoped pattern binders through a borrowed scrutinee; no reference binding is introduced by `let` or assigned; none is stored in an aggregate or a constant, and constants are values, never places — and one binding per visible scope chain (SCOPE-1: no binder reuses a visible name; sibling scopes may), with the design-example decisions on `&mut` arguments and unused exclusive references (B1, B2) and owner reads under a shared borrow (B7) recorded | per rule: a normative paragraph in `docs/language/` with accepted and rejected examples, cited fixtures, S1 generator coverage meeting its minima and a versioned migration diagnostic; the 092 and 093 reproducers stay rejected, 087's shadowing reproducers, accepted today, become rejected by the shadowing rule, and the `let`-bound and assigned reference shapes (REF-6), accepted today, become rejected | blocks features | their violations were use-after-free and leaks in safe code (087, 092, 093); RocksDB handles and V1 locals are checked under them |
| 2j | R-0500 | the rules V1a's subset needs are written down and enforced: typed literals, literal range (signed minima preserved), statement vs value position, sequential assignment within a loop iteration, evaluation order and short-circuiting ([the evaluation-order rule](docs/language/STATEMENT_EXPRESSION_MODEL.md#evaluation-order-and-assignment): left to right in source order, struct and enum literal fields in source order, a functional-update base evaluated once after the explicit fields, an assignment evaluating its right-hand side before its destination), and what an assignment does to the old value (MEMORY_SEMANTICS §10) | the per-rule acceptance of 2i | blocks features | named dependency of V1a: the verified pipeline never models looser rules than the compiler enforces ([plan](docs/compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices)) |
| 2k | R-0500 | the remaining disputed rules are written down and enforced: explicit match binders and cast semantics per width pair, as [ARITHMETIC_POLICY §8](docs/language/ARITHMETIC_POLICY.md#8-numeric-conversions-as) decided it on 2026-10-21: `as` accepts only the always-lossless numeric, `bool` and `char` conversions of its class table, and every lossy conversion is a compile error that uses named truncation, reinterpretation, rounding or checked conversion; numeric scope only (pointer and reference casts unchanged); staged: the named operations, then the rejection with versioned migration diagnostics, then the migration | the per-rule acceptance of 2i | continues alongside | their defects (080, 084) are repaired under 0a; casts enter at a later V1 increment and match at V2, so neither is a dependency of RocksDB slice 1 or V1a |
| 2l | R-0498 | enforcement is demonstrated (S3): a claim → gate-leg → targeted-mutation manifest over the verification and language docs, enforcement-bearing comments in `Concrete/Proof`, `Check` and `Backend`, and emitted contract text, with R-18 – R-20 relabelled shape-only until proved | every enforcement phrase in that scope at the commit that opens this exit has a manifest row whose mutation its leg kills in a qualified ([stage (c)](#promotion-pipeline)) campaign; later phrases join row 6's per-slice growth | continues alongside | prevention; no reproduced defect |
| 2m | R-0491 | the bug-discovery rate has fallen to about zero: the last three consecutive *counting* probe batches in the [discovery record](docs/bugs/discovery.tsv) (new workloads, fuzzing campaigns, new examples, hunts, semantics probes, specifications, design proofs, interpreter coverage) each found no silent bug and at most one loud bug. Each batch records its effort, and a quiet batch counts only with a recorded effort of at least four hours or ten probe programs, examples or cases (a smaller, token batch is recorded and shown but does not count). Reviews, fix follow-ons, CI and development attribute their bugs but are not probe rounds | the [bug dashboard](#bug-dashboard)'s discovery signal, generated by `scripts/gen/bug_dashboard.py` and checked by `check_docs_drift.sh`, reads three quiet counting batches in a row at the closing commit, and every bug record from 079 on is attributed to one batch (the generator refuses an unattributed record) | continues alongside | a convergence measure: it says whether the point repairs and the structural work have stopped the classes of [STRUCTURAL_CLASSES.md](docs/bugs/STRUCTURAL_CLASSES.md) from producing new bugs; no reproduced defect and no deliverable depends on it, while Phase 1 and the preview's advertised stability do |

### Feature prerequisites

Implementation and experiments on the first feature deliverables proceed now: early executable
feedback is how defects surface, and the RocksDB slice 1 groundwork already found bugs 102 and 103. The
exits listed against a deliverable are required before it is **accepted** or its safety is advertised;
none of them is removed or weakened by starting early, and a defect the early work finds enters the
queue under the entry rule. Isolated proof development (the reviewed design examples, proof libraries)
waits for none. Every *blocks features* exit appears in
at least one *blocked by* list and no *continues alongside* exit appears in any.

| deliverable | blocked by | not blocked by |
|---|---|---|
| RocksDB slice 1 (row 5, R-0285) | 0b, 2i | 0a, 0c, 1a, 2a, 2b, 2c, 2d, 2e, 2f, 2g, 2h, 2j, 2k, 2l, 2m |
| V1a (row 3, R-0511) | 0a, 0b, 2a, 2g, 2h, 2i, 2j | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l, 2m |
| V1b (row 3, R-0511 with R-0517) | 0a, 0b, 2a, 2g, 2h, 2i, 2j | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l, 2m |
| V1c (row 3, R-0511) | 0a, 0b, 2a, 2g, 2h, 2i, 2j | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l, 2m |

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
  [first executable slice](#task-r-0285); the four-adapter checkpoint follows. R-0285's four
  companion FFI and resource examples (a file-handle wrapper, a C library holding a callback and its
  context, a resource pool, multi-step error paths that release everything) are accepted with or after
  this slice; they do not block it.
- **V1a** (R-0511). Must compile, from checked Core to executable BIR, every program of the V1a subset:
  `i64`, `u64`, `bool`, locals and assignment, conditionals, a general loop with structured exits,
  non-recursive direct calls, comparisons, checked addition, subtraction and multiplication, and one
  capability-authorized `print_int` event, with terminating, trapping and divergent behaviours. Other
  widths, casts, division, shifts, recursion and further loop forms are later V1 increments. R-0511's
  [V1a target programs](#task-r-0511) are the named instances of this subset and V1a's regression set;
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
| **A. Verified compiler** (primary) | 3, 4, 6 | 0b, 2h, 2i, 2j, 2k | at least 55% | 2: one shared-compiler migration checkpoint (row 4, then each slice's migrations) and one proof checkpoint (rows 3 and 6) | the semantics the proofs rely on (overflow, casts, evaluation order, traps, divergence, memory, with executable examples that agree: R-0493, R-0496, R-0500); the shared compiler structure; ownership and capability soundness, preserved through specialization (R-0499, R-0507, R-0510); source → Core correspondence (V1c, then each slice's frontend obligation); V1a – V7 |
| **B. Evidence integrity** (protected) | 9, 10, 11, 12, 13, 14, 15, 17 | 0a, 1a, 2a, 2g | at least 15% | 1 | specification and claim correctness (the claimed property is the theorem's proposition, typed contracts); one verdict authority; the axiom policy; task identity and evidence bound through linking to the final image and its dependency versions; independent checking. Before rows 9 – 11, the bounded increments every published V1 claim needs: R-0521's minimal task identity and R-0353's minimal independent retained-artifact consumer |
| **C. Adversarial validation and gate health** (continuous) | — | 0c, 2b, 2c, 2d, 2e, 2f, 2l, 2m | at least 10% | 1 | the prevention harness, scheduled hunts and fuzz campaigns, the adversarial example suite, demonstrated enforcement, trustworthy gates and qualification (R-0491, R-0498, R-0513) |
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
  `multi-kernel`). `External proof re-check` skips the nightly cron. The critical path is now `Build` then `Proof evidence gate` (~60 minutes), down
  from `Build` then the unsplit language job (~85 minutes).
- The **campaign tier** is stage (c) evidence and gates qualification, not merges. It runs on its
  cron on `main` of `lambdaclass/concrete` only, or on any ref with
  `gh workflow run CI --ref <ref> -f campaign=true`. Its jobs: `Gate mutation plan`, the 24 `Gate
  mutation shard N` jobs, `Gate mutation coverage (aggregate verdict)` and `Nightly differential
  fuzz` (nightly cron), `Weekly clean-checkout proof replay` (weekly cron), and the `Multi-kernel
  mutation coverage` step of the multi-kernel job (either cron, on `main` of `lambdaclass/concrete`, or a dispatch with `campaign=true`).

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
  leg);
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

## Where every area fits

The task catalog below is filed by topical **area**; areas are filing locations, not scheduling units.
Until 2026-10-19 they were numbered "Phase 7" to "Phase 20"; the *old phase* column maps those numbers,
which older documents and the changelog still use, to the area. Old Phases 1 – 6E, and the completed stdlib-area foundations and
workloads 1 – 8, are complete and recorded in [CHANGELOG.md](CHANGELOG.md). Task counts include retained completed and retired stubs; they
are not counts of unfinished work. The second number counts the tasks of that area in the
[backlog](docs/project/ROADMAP_BACKLOG.md): tasks no active or next phase schedules (see
[Backlog and pull rules](#backlog-and-pull-rules)). Each task increment follows its explicit queue assignment or the
reconciliation ledger. Unscheduled work remains pull-gated or deferred with a recorded trigger. An
area-level assignment does not override an individual task's prerequisites.

| area | old phase | tasks (ROADMAP + backlog) | phases (principal, non-exhaustive, except the two rows marked exhaustive) | how it fits |
|---|---|---|---|---|
| Standard library and core APIs | 7 | 90 + 6 | 1–7, plus after-1.0/research (exhaustive) — **reconcile** | Includes identity/evidence infrastructure and typed contracts as well as stdlib work: R-0482 follows rows 0–1; R-0473/R-0474/R-0477 follow Phase 6; R-0440 follows its evidence/package checkpoints; R-0486 graduates through Phases 6–7 |
| QBE backend and independent validation | 7.5 | 33 + 9 | 2–4 — **reconciled 2026-10-19** | R-0113's decision: the Lean Compiler Backend (R-0517) is the QBE path; Concrete builds no QBE backend. Each task is kept, merged or retired with its controls transferred (reconciliation ledger) |
| Flagship depth and examples | 8 | 18 + 7 | 1–4, then 7 — **reconcile** | R-0114–R-0118 supply shared compiler foundations; workloads run in Phase 3; the three public flagships graduate in Phase 7 |
| Incremental compiler driver | 8.5 | 12 + 0 | 3 | Prerequisite for the editor (R-0385) and the compile-time budgets |
| Proof authoring and automation | 9 | 14 + 14 | 2, 4, 6 — **reconcile** | Core proof authoring grows with slices; specification libraries, quantified contracts and frame work follow their Phase 6 prerequisites; legacy PExpr/FnTable objectives require explicit disposition |
| Audit commands and review artifacts | 10 | 10 + 15 | 2–5, then 7 | Minimal profile, provenance and replay surfaces accompany public slices; the complete evidence product follows Phase 5; package consumers follow Phase 7 |
| Proof status and trust gates | 11 | 16 + 4 | 1, 5 — **reconcile** | Overlaps the single verdict authority and axiom policy (R-0494, R-0506) |
| Provable and predictable subsets | 12 | 12 + 10 | 2, 4 — **reconcile** | The verified slices define the admitted subsets from now on |
| Runtime safety obligations | 13 | 15 + 12 | 1, 2, 4, 6 — **reconcile** | Immediate soundness repairs precede release; R-0460 proves Core-to-VC rules; contract-dependent obligations follow Phase 6 |
| Compiler soundness bridge | 14 | 18 + 4 | 2, 4–6 — **reconcile** | Refinement and evidence soundness follow the verified plan; spec/ghost totality follows its contract prerequisites; R-0460, filed in the runtime-safety area, remains the Core-to-VC owner |
| Verified-compiler pipeline, stabilization and usability | — | 38 + 10 | 1–7 and every release, plus after-1.0/research (exhaustive) | R-0491–R-0517 follow their queue, slice and release checkpoints. R-0518–R-0538 follow their individual Phase lines: stabilization, provenance and tooling, Phase 6 contracts/totality/equivalence, compatibility, and explicitly deferred research. |
| Backend, target and stdlib contracts | 15 | 24 + 4 | 2–4 and every release — **reconcile** | Backend/target contracts reconcile with S10 – S13; interop (R-0285, ABI/export tasks) and applications (R-0490) follow Phase 3; QBE-derived tasks follow R-0113's decision |
| WebAssembly | 15.25 | 0 + 7 | after 1.0 | A later target profile; research-gated. The whole area is in the backlog |
| Unsafe Concrete and trusted systems profile | 15.5 | 6 + 4 | 4 | V6 (raw pointers, `Unsafe`, FFI) and the trusted-base inventory |
| Performance and optimization evidence | 15.75 | 8 + 0 | 3–4 and every release | R-0416–R-0424 provide the shared measurements consumed by R-0355/R-0515; optimization work is measurement-pulled and its certificates follow S12 |
| Freestanding and embedded target | 16 | 0 + 12 | after 1.0 | A later target profile. The whole area is in the backlog |
| Public release bar | 17 | 29 + 2 | 7 and every milestone | The release map's scopes draw their acceptance from here |
| Post-release operations | 17.5 | 7 + 1 | from 0.1 | Security response, support and patch procedure start with the first release |
| Packages and dependency evidence | 18 | 8 + 13 | 3, then 7 | Local packages with interop (Phase 3); package evidence and policy (R-0440) in Phase 7 |
| Editor and human tooling | 19 | 11 + 4 | 3, then 6–7 and later | R-0385–R-0388 provide the V1 checkpoint subject to the incremental-driver area; richer-contract and dependency views follow their facts; advanced tooling remains pull-gated |
| Concurrency and research-gated extensions | 20 | 4 + 16 | research, with operational exceptions — **reconcile** | Concurrency and other research retain their triggers; R-0446 joins the prevention/typed-operation inventory; R-0487 joins staged authority reporting; R-0413 remains gated compiler-TCB research |

R-0113 owns the QBE reconciliation decision before a QBE implementation checkpoint is
activated. The decision records keep, merge or retire for R-0072–R-0113 and updates
R-0296, R-0305–R-0311, R-0331 and R-0424 plus the Wasm entrance criteria and all
mandatory QBE comparisons. Useful ABI, differential, legality and reproducibility
controls transfer to named owners even if QBE is retired. QBE is not a prerequisite
for R-0505's native backend. Any retained backend has an explicit maintenance budget.
**Decision recorded 2026-10-19:** the Lean Compiler Backend, a separate Lean 4 port of QBE 1.3, is
Concrete's QBE path and native backend (R-0517); R-0505 integrates it and R-0297 is delegated to it.
The QBE backend that the QBE backend-validation area specified was never started.

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

## Area: Development Throughput

These tasks carry the [development throughput](#development-throughput) track, which was decided on
2026-10-22. The measurements behind them, the targets and the dependency order are in that section.
Pre-push gate selection belongs to R-0513 (exit 2f), so it is not restated here. None of these tasks
changes what a validation result means. Every result stays bound to its exact SHA under the
[promotion pipeline](#promotion-pipeline).

### Task R-0539

**Phase:** 1

**Objective:** Landing trains. Merge every ready branch into one candidate, run one CI, and bisect only
on red.

**Work:** a landing script (new) that takes the ready branches and does the following:

- builds the train;
- regenerates the generated files (R-0542's ledgers, `Concrete/BuildIdentity.lean`, the candidate
  table) once on the train rather than on each branch;
- runs the fast gates and pushes the train to `ci/`;
- dispatches CI and watches it to its conclusion;
- on green, lands the train on both remotes.

On red, it bisects the train by member and rebuilds it without the offending branch. Every result is
recorded against the SHA of the candidate that actually ran: the train, or the bisection candidate.
The attribution to a member is a separate field of that record. A member's own SHA receives a result
only from a run at that SHA. A rerun never stands in for the failed result. The train's record lists
each member's SHA, so a landed member can be traced to the train candidate whose result it relies
on.

**Exit:** three consecutive trains of at least two branches land through the script with no manual
step except approval. Median Codex-clean → main is under 2 h over those trains (R-0547's record). A
seeded red member is identified by bisection, and the remaining members land without it.

**Depends on:** R-0542 (a train whose ledgers conflict cannot be built mechanically) and R-0540 (the
landing tier).

### Task R-0540

**Phase:** 1

**Objective:** CI tiers. A landing tier is required for merge, with a target of ≤ 40 min. The mutation
campaign runs on schedule or dispatch and gates campaign qualification ([stage (c)](#promotion-pipeline)),
not merges.

**Work:** in progress on branch `ci-tiering`, which edits the workflow and the promotion-pipeline tier
text. It moves the mutation campaign out of the landing tier, splits the language-surface job and
routes jobs to runners. Candidate CI runs on lambdaclass/concrete, whose GitHub Enterprise plan allows
about 500 concurrent jobs against the free plan's roughly 20.

**Exit:** median landing-tier wall clock, from dispatch to conclusion including queueing, is ≤ 40 min
over 10 consecutive runs. No landing-tier job takes more than 40 min. Stage (c) still requires every
job, the campaign included, and `check_qualification_record.sh` refuses a landing-only run.

### Task R-0541

**Phase:** 1

**Objective:** Self-hosted or larger runners on the maintainers' servers, and remote execution of
heavy local gate runs on the same servers.

**Work:** register the runners with labels and route only `push`, `schedule` and `workflow_dispatch`
jobs to them. They are never used for `pull_request`, because the repository is public and a fork's
pull request would run untrusted code on them. The runbook is `docs/project/SELF_HOSTED_RUNNERS.md` on
branch `ci-tiering`. Heavy local gate runs (`make pre-push-full`, closure-mode runs and local mutation
campaigns) run remotely on a server against an exact SHA, so they keep the laptop's load low.

**Exit:** a workflow check fails when a job with a self-hosted label can run for `pull_request`, and
its negative control proves the check bites. A full local gate set runs on a server and records its
result against the candidate SHA in the same form as a local closure-mode summary.

**Depends on:** R-0540 (routing), R-0543 (where heavy runs go).

### Task R-0542

**Phase:** 1

**Objective:** Generated ledgers. Every shared fact has one owner that is edited by hand, and every
other copy is generated and drift-gated. This is the "one owner per fact" rule of the approved
repository reorganization ([R-0480](#task-r-0480)).

**Work:** in progress on branch `ledgers`, which edits the bug README, the audit corpus and the
CHANGELOG mechanics:

- bug records are the single source for the bug README's lists, STRUCTURAL_CLASSES' instances and the
  audit corpus;
- CHANGELOG entries are `changelog.d` fragments;
- the example manifest's and gate inventory's counts are derived;
- each generated copy has a drift gate.

**Exit:** a scripted control starts two branches from one base, each adding a bug record, a changelog
entry and an example. It lands the first, rebases the second onto the result with zero textual
conflicts, and finds both branches' records in the regenerated ledgers. A hand edit to a generated
ledger fails its drift gate.

### Task R-0543

**Phase:** 1

**Objective:** Load control. At most 2 heavy local runs per machine.

**Work:**

- a machine-wide semaphore in a directory shared by every worktree and clone, which every heavy runner
  acquires and releases even when it is interrupted;
- agents run only the fast gates and the impact-selected gates (R-0513);
- full sets run on the servers (R-0541) or in CI.

**Exit:** a third concurrent heavy run waits for a slot (control), and a stale slot from a killed run is
reclaimed. A recorded week of runs has no timeout failure attributed to load.

### Task R-0544

**Phase:** 1

**Objective:** Review discipline. The target is ≤ 5 review rounds per slice.

**Work:**

- review per slice;
- after the first full pass, each round reviews only the delta since the previous round;
- a slice still not clean after 5 rounds is split;
- ownership and memory-safety work is reviewed with neutral prompts and piecewise diffs, so a filter
  does not block the round.

**Exit:** R-0547's record shows a median of ≤ 5 rounds per slice over a month, and every slice past 5
rounds has a recorded split.

### Task R-0545

**Phase:** 1

**Objective:** Branch discipline. One bug or one rule per branch, and rule-changing branches land first
and small.

**Work:**

- a branch that changes a language or checker rule lands before branches that add fixtures under the
  old rule;
- the rule change carries the migrations of main's fixtures, and nothing else;
- a branch that needs two rules is two branches.

**Exit:** over a month of landings, no branch carries more than one bug or rule without a recorded
reason. No rule change breaks a fixture of a branch that was already ready.

### Task R-0546

**Phase:** 1

**Objective:** Build and compile caches.

**Work:**

- first, profile the 20 slowest gates;
- then a shared compiled-artifact cache keyed by compiler identity and source hash;
- a shared Lean build cache across worktrees.

**Exit:**

- the profile is recorded at a named SHA;
- with the compiler unchanged, a second run of those gates takes at most half the first run's time;
- a new worktree's first build reuses the shared Lean cache;
- changing the compiler identity or a source file invalidates the affected entries (negative control).

**Depends on:** R-0547 (the profile is recorded with the measurement).

### Task R-0547

**Phase:** 1

**Objective:** Measurement. Record per-branch stage timestamps (started, Codex-clean, CI started,
landed) and show median time to land on this roadmap.

**Work:**

- one record per branch, so recording never conflicts on rebase;
- review rounds per slice and the place each gate failure was first seen (pre-push or CI) are recorded
  with the timestamps;
- a generated median time to land next to the candidate table under
  [Implemented, not yet accepted](#implemented-not-yet-accepted), regenerated like that table.

**Exit:** every branch landed after the recorder exists has all four timestamps. The generated median
covers them, and `check_docs_drift.sh` fails when the median is stale.

## Area: Verified-Compiler Pipeline, Stabilization And Usability

These tasks carry the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md)
(R-0491 – R-0512), the gate and shared-front-end work it depends on (R-0513, R-0514), the usability
and release-automation tracks (R-0515, R-0516), the Lean Compiler Backend dependency (R-0517) and the
open work promoted from roadmap prose by the 2026-10-19 reconciliation. They were filed under the backend-contracts area
until 2026-10-19; their queue rows, not this position, schedule them.

### Task R-0491

**Phase:** 1, 2, 4

**Reconciliation (2026-10-19):** Keep; extend explicit defect inventory through 101. Extend the permanent defect inventory and per-surface expected results through bug 101.

**Objective:** Prevention harness — stage S1 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; starts now.**

Depends on bug 080 landing (`--report model-diff`).
The extracted model, then Core's semantics, as a third side of the differential fuzzer over a
grammar covering every shape that broke (loop-body sequencing, `let` after assignment, `return` in
loops, if-without-else fall-through, discarded `expr;`, shadowing, renamed match binders, literals
at 2^63/2^64, every cast width pair). Outcomes classified (value, trap, rejection, unsupported,
exhaustion, crash), never skipped. model-diff for zero-argument, `bool` and aggregate signatures
plus a binary leg, with a pinned claimed-function inventory. The four 2026-10-18 hunts and the
checker hunt run before every release candidate.

**Structural classes (2026-10-20).** Prevention covers causes, not only spellings. This task keeps
[docs/bugs/STRUCTURAL_CLASSES.md](docs/bugs/STRUCTURAL_CLASSES.md): every bug record from 102 on
states its structural cause and owner (`check_bug_filing_complete.sh` refuses one without them), and
each new bug joins a class there, with the owning task, exit or verified-pipeline stage, or adds a
class with a proposed owner in the same change. A class whose owner is not yet scheduled is raised in
the queue under the entry rule when a new bug of that class is reproduced.

**Exit:** each of 080 – 095 has a checked-in bug record, reproducer, expected outcome and
pre-fix/fixed SHA pair. The deterministic differential suite detects 080 and 084 – 088;
name-binding controls detect 081 – 082; evidence and cross-surface controls detect 083 and 089;
checker mutants detect 090 – 094; the binary leg detects 095. Each assigned control fails at its
pre-fix SHA and passes at its fix; generated features have pinned minimum completed-comparison
counts; missing inventory entries fail. The adversarial example suite below passes every leg (exit 2c).

**Independent golden oracles (exit 2c; 2026-10-22).** An expected output is evidence only when
something other than the compiler under test agrees with it. Bug 121's `conlog` golden was recorded
from the miscompiled binary and blessed its wrong count, because the interpreter could not run
`conlog` and nothing else checked it. Every `@run` rule of `scripts/tests/example_manifest.txt`
therefore declares its oracle, enforced by `check_example_outputs.sh` with negative controls:
`interp` (interpreter agreement, which requires `interp=yes`), `spec:<path>[#anchor]` (a hand
derivation from the program's specification or a reference tool's output, recorded in
[GOLDEN_ORACLES.md](docs/project/GOLDEN_ORACLES.md)), or `unverified:<reason>` (allowed only when the
interpreter does not run the case). The gate prints the number of unverified goldens and lists
them; each one is a tracked gap, retired by an interpreter repair (exit 2c's interpreter gaps,
projects that import `std` modules, argv) or a recorded derivation. The same rule applies to the
adversarial examples below and to every new expected result.

**Adversarial example suite (exit 2c; 2026-10-20; planned).** Phase 7's workloads (`base64_cli`,
`png_chunks`, `wordfreq`, `httpget`, `tcpserve`) and interpreter-vs-compiled probing found many of the
compiler's wrong-code bugs; generated programs are bounded by their grammar. This suite writes small
hand-targeted programs for each current bug class, under `examples/adversarial/`:

| example | exercises | classes |
|---|---|---|
| `linear_flow` | linear handles moved, consumed and destroyed across `if`/`else` and `match` arms, early `return` from inside loops and branches, `break`/`continue` with a live handle, consume-then-exit; rejection legs for a handle leaked on one arm and one moved by a match guard | 078, 087, 090, 093 |
| `generic_destroy` | generic `Destroy` through trait bounds and impls, collections of arrays and of generic structs, nested composition; a rejection leg for an impl's capability hidden behind generic dispatch | 052, 091, 099, 100 |
| `nested_names` | same-named types, functions and methods in nested and sibling modules and against generated specialization names, type-argument arity, relative calls into a nested submodule (its leg asserts today's refusal until 077 is fixed); builtin versus local and imported user-defined `Result` | 054, 070, 073, 077, 096 – 098 |
| `import_aliases` | function, type, method and sibling-module aliases, an alias against a same-named local definition, alias order, colliding aliases rejected | 039, 055, 069, 082, the E0264 aliased-method case |
| `casts_widths` | every cast width pair (narrowing, widening, sign change) at boundary values, literals at each type's minimum and maximum, checked negation; claims on cast functions | 053, 080, 088 |
| `mut_receivers` | `&mut self` methods called through `&mut` parameters, struct fields and array elements and in both arms of a branch; stack-array storage mutated through a borrowed raw pointer (018), mutation of an array extracted from a struct field (019), and receiver borrowing on the right of `&&`/`||` (034), with mutations observed afterwards | 018, 019, 031, 034 |
| `match_and_fnptrs` | match guards with effects, renamed and positional binders, function pointers reassigned across branches and read from struct fields, with their capabilities | 056, 063, 066, 084, 090 |
| `statement_sequencing` | discarded `expr;`, `let` after assignment, loop-step sequencing, branch-local `let`, `if` without `else` falling through; claims on each shape | 085, 086, 101 |

Bug 063's fixed derived-function-pointer cases compile and run through both the function-call and the
method-call path, and its generic-return controls keep E0242 as in `check_cap_inference.sh`. For cases
still unresolved (054, 055, 077 and the E0264 aliased-method case), separate fixtures pin
the documented collision diagnostic, rejection or containment result until the owning repair lands;
runnable cases still require interpreter, native and expected agreement. The suite does not make those
repairs prerequisites of any deliverable. The number-to-shape mappings for 082, 087, 088 and 090 – 100
are provisional until their checked-in records and reproducers land under 2b, and are reconciled
against those records before the suite is accepted.

Each example has its own leg in `scripts/tests/check_adversarial_examples.sh` (new, in the full gate
set): interpreter and native observable output and normalized outcomes must agree with a checked-in
expected result, where normal exits keep their exit status and traps keep their identity rather than
counting every trap as equivalent; a rejection leg asserts its diagnostic code; where the example carries claims, its leg also
runs `--report model-diff` on the claimed functions and asserts each claim's status. The gate fails on a
missing example or leg, and a perturbed expected result must fail its leg (positive control). The
evidence and report cases 058 – 062, 064, 065, 067, 068, 075, 076, 079, 081, 083 and 089 keep their
existing controls (exit 2b covers only 080 – 101). Capability case 071 keeps
`check_cross_package_caps.sh` and `check_cap_sibling_module.sh`; runtime case 072 keeps
`check_spawn_exit.sh`; package-name collision 074 keeps its rejection controls; collection case 057
keeps its collection controls; 092 keeps the 2i controls; 094 and 095 keep the checker and binary
controls of the detection map above. A defect an example newly reproduces is a
reproducer: it enters the queue under the [entry rule](#the-current-execution-queue), as a new row-0
exit when it is an authority or safety defect, and the example keeps its leg as that defect's gate.

**Scaled differential campaigns (2026-10-20; planned).** The nightly `fuzz-campaign` job in
`.github/workflows/lean_action_ci.yml` runs `fuzz_differential.py` at 2,000 depth-3 and 500 depth-4
programs and `fuzz_linearity.py` at 400 per date-derived seed (R-0272's seed rotation, merged here).
Once the third side exists, the same generators compare interpreter, native binary and proof model
(model-diff on generated claimed functions) at campaign scale: at least a hundred times the nightly
counts per campaign, sharded by seed range. This CPU-bound work runs first on short-lived remote build
servers once they are available (R-0513 owns the remote runner infrastructure); no provider
credentials live in the repository, and the GitHub nightly keeps its current counts meanwhile. Each
campaign records its commit SHA, toolchain, seed ranges, outcome classification and per-feature
completed comparisons against exit 2e's minima; failures are minimized automatically and checked in as
reproducers, which enter the queue under the entry rule. This campaign is not part of exit 2c's
acceptance and no deliverable waits for it; it is accepted when one campaign at that scale completes
with every outcome classified and every per-feature minimum met, and recurs before each release
candidate with the hunts above.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0092** (superseded): superseded — the three-way differential is Core semantics / LLVM profile / LCB-native under R-0491. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0092)
- **R-0093** (merged): duplicate of R-0092: merge; labels are hypotheses, not proof of faulty component. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0093)
- **R-0094** (superseded): superseded by R-0491: merge; retain hand/reference/metamorphic controls.
- **R-0095** (superseded): superseded by R-0491/R-0118: merge manifests and minimum completed comparisons.
- **R-0096** (superseded): superseded by R-0513/R-0491: merge; remove mandatory QBE availability.
- **R-0105** (superseded): superseded by R-0491/R-0504: merge debug/release and sanitizer controls. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0105)
- **R-0106** (merged): duplicate of R-0009/R-0491: merge new backend predicates into shared reducer/harness.
- **R-0273** (superseded): superseded by R-0491/R-0498: merge; map every listed family to a retained control. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0273)
- **R-0290** (merged): Duplicate of R-0491: merge into R-0491, retaining backend/profile matrix.

### Task R-0492

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep under R-0514; avoid unnecessary legacy migration before retirement.

**Objective:** Resolved identities — the identity half of S5; R-0497 owns immutable, payload-bound artifacts — stage S5 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; starts now.**

`LocalId` for every binder, allocated at
Resolve so the source checker and Elab both consume it; `DefId` as a compilation-scoped declaration
handle allocated before body elaboration, preserving callable namespaces, specialization arguments
and extern/intrinsic distinctions, with a validated `DefId → DefinitionIdentity` mapping at evidence
boundaries; owner-checked `FieldIdx`/`VariantIdx`; `RegId`/`BlockId` in SSA. No `String → identity`
coercions: identities come from scope-aware environments through checked adapters;
capture-preserving alpha-canonicalization before hashing, frozen identity bytes preserved or
explicitly versioned. Pipeline artifacts (owned by R-0497) become immutable and payload-bound (`ValidatedCore` is
publicly constructible today, `Pipeline.lean` ~:48), with diagnostic artifacts typed separately.

**Semantics decisions (2026-10-21):** the compiler refers to `Option` and `Result` only by resolved
identity, never by spelling, so a user enum named `Result` is a distinct type (LAYOUT_CONTRACT §3.4);
mangled symbols for every Concrete-defined function take their disambiguator from `DefId` (FFI.md,
"Symbol Names").

Language: no block `let` shadowing a visible name; colliding import aliases rejected; match binders
equal or name their field.

**Exit:** `check_ir_no_string_identity.sh` rejects string identity joins in resolved tables, the
source checker's environments, Core, SSA and `PExpr`; 081, 082, 087, 093 and 094 reject or bind
correctly; targeted mutations show checker consumption/block-local accounting, `ElabVerify` and the
theorem-table identity checks load-bearing; compile-fail controls cover artifact forgery and
diagnostic-path bypass.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0007** (superseded): superseded by R-0492/R-0510: merge, preserving function-symbol and metamorphic controls. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0007)
- **R-0008** (superseded): superseded by R-0492: merge reproducer and alias permutations into identity migration.
- **Aliased imported method (E0264)**, from the archived 2026-08 gate-health record: a method call on an
  aliased imported type is rejected today; the identity migration resolves it and adds an exact
  alias-method fixture (reconciliation prose item, merged through R-0008).
- **R-0103** (superseded): superseded by R-0492/R-0505: merge; general separate compilation arrives in V6.
- **R-0114** (superseded): superseded by R-0492: merge; distinguish compilation-local IDs from persistent evidence identity. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0114)

### Task R-0493

**Phase:** 1, 2, 4

**Reconciliation (2026-10-19):** Keep immediate and slice checkpoints separate.

**Objective:** Typed operations — stage S6 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; the immediate checkpoint is queue row 2.**

**Immediate row-2 checkpoint:** make `IntArith` the arithmetic authority for the existing pipeline's
evaluation, folding and arithmetic-model generation, with emitted operations checked against the same
specified semantics. `check_single_arithmetic.sh` and differential controls cover every supported width
and operation family, including casts and defined traps. This checkpoint does not wait for S5; the
typed-operation migration below does.

Depends on S5. Integer ops typed by `IntTy` with
checked/wrapping/saturating as distinct op kinds and signedness on the op; pointer arithmetic its
own op; boolean, float and pointer ops distinct; signed literals canonicalized before range
checking; contextual literal types resolved before construction and re-validated after Mono; typed
casts; `IntArith` the only arithmetic (`check_single_arithmetic.sh`).

**Exit:** every integer op in Core and SSA carries its kind and width; the arithmetic lint passes;
signed-minimum fixtures pass; 088's reproducers agree on every side.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0079** (superseded): superseded by R-0493/R-0501/03: merge into typed operations and pass obligations. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0079)
- **R-0221** (superseded): superseded by R-0493/S6: merge semantic ownership into R-0493. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0221)
- **R-0464** (superseded): superseded by R-0493/R-0495/R-0460: merge; archive completed H24 fix and retire SSA-first prescription. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0464)

### Task R-0494

**Phase:** 1, 4

**Reconciliation (2026-10-19):** Keep; row 6 extends it per slice. Hypothesis dependencies: cycles (O1 needs O2 needs O1), self-support and missing references are rejected or handled by a stated well-founded/SCC rule, with hostile cycle fixtures (with R-0460). The structured subject-facts schema replaces the scraped human report here (with R-0481).

**Objective:** One verdict authority — stage S2 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; starts now.**

`Verdict` with a private discharging constructor;
`Verdict.decide` consumes checked evidence binding proposition, clause, subject, coverage,
dependency closure and environment; one `ClaimJudgment` per claim and contract clause; receipt
decoding and currency checks feed it; contracts, vcs, obligation-ledger, audit, check-proofs (text
and JSON), proof-status and issuance render it. Delete `Proof.provedFunctions` and name-substring
replay checks; split `Report.lean` (6,405 lines) and `Main.lean` (4,194) by surface.

**Exit:** status spellings only in the renderer table; 089's reproducers read not-proved
everywhere; a cross-surface gate pins the expected positive and negative judgment for every claim
and clause.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0171** (superseded): superseded by R-0494: merge into R-0494; preserve route-specific attacks. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0171)
- **R-0194** (merged): duplicate of R-0209: merge into R-0209.
- **R-0204** (superseded): superseded by R-0494: merge into R-0494.
- **R-0205** (superseded): superseded by R-0494: merge into R-0494’s evidence-class controls.
- **R-0209** (superseded): superseded by R-0494: merge into R-0494; retain audit/diff tests.
- **R-0218** (superseded): superseded by R-0494: merge into R-0494.
- **R-0264** (superseded): superseded by R-0494/R-0509: merge implementation into R-0494, soundness proof into S16.
- **R-0465** (done): done for its enumerated wiring; the remaining universal-verdict-authority requirement merges into R-0494. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0465)

### Task R-0495

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; coverage and loop binding are essential exits.

**Objective:** Claims on Core — stage S9 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned.**

Depends on S2, S7, S8. Typed `ClaimSpec`; statements generated against
Core's semantics on the exact checked term; ProofKit's symbolic-evaluation simp set; existing
`PExpr` theorems ported (through an explicitly proved bridge where cheaper), keeping weaker coverage
where that is what was proved; Core-proved and binary-transported status remain distinct. Legacy
issuance and replay retirement follow R-0512's inventory, compatibility and deprecation conditions.

R-0495 checks coverage relation and domain against the exact generated proposition; author
annotations confer no authority. Loop claims bind the exact checked Core loop and its
initialization, preservation and exit obligations; missing binding remains `spec_unbound`.
**Exit for each migration cohort:** every migrated positive claim replays against its exact generated
statement; negative fixtures refuse; wrong-coverage, narrowed-domain and substituted-loop controls are
refused through status, issuance, consumption and independent replay. Unmigrated supported claims
retain their declared maintenance and replay path. Legacy issuance code is removed only under R-0512's
final retirement conditions; historical replay remains available.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0123** (superseded): superseded by R-0495/R-0512: retire new legacy-model semantics; add admitted Core features/proof lemmas instead.
- **R-0176** (superseded): superseded by R-0495/R-0512: retire proposed legacy architecture; retain ownership goals in R-0151. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0176)
- **R-0202** (superseded): superseded by R-0495/R-0512: merge dependency-closure requirement into Core migration; maintain legacy checks until retirement.
- **R-0229** (superseded): superseded by R-0495 and V2–V4: merge into slice admission/claim coverage.
- **R-0235** (superseded): superseded by R-0496/R-0495: retire PVal/PBinOp expansion; implement Core float semantics. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0235)
- **R-0262** (superseded): superseded by R-0495/R-0512: retire blanket legacy bridge; retain only migration-pulled bridges. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0262)
- **R-0265** (superseded): superseded by R-0495/R-0509: merge into statement/binding/aggregation soundness.
- **R-0267** (superseded): superseded by R-0499/R-0507/R-0495: merge; capabilities and purity remain distinct.
- **R-0269** (superseded): superseded by R-0495: merge composition needs into R-0164; retire PExpr table architecture.
- **R-0464** (superseded): superseded by R-0493/R-0495/R-0460: merge; archive completed H24 fix and retire SSA-first prescription. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0464)

### Task R-0496

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; state implementation versus proved-subset coverage separately.

**Objective:** Core semantics — stage S7 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned.**

Depends on S5, S6. The semantic contract above for Core: value
semantics with allocation identities, capability-tagged external events, defined traps,
behaviours from clocks, an instrumented mode with ghost ownership (moved places, loans, frames,
capability use); FFI and OS as external-call contracts. The interpreter becomes this semantics plus
an IO driver. It is the oracle for S1 from here on. S4 fixes the relevant language rules before
this stage freezes their semantics. Before freezing Core, the raw-memory interface is specified
jointly with S11 (initialization, alignment, pointer fragments, allocation lifetime, address
exposure, foreign mutation), and a representative typed-value/raw-memory abstraction lemma is
proved. Representative proofs of clock stability and partial-trace monotonicity land with the
evaluator.

**Exit:** covers every construct the checker accepts; the interpreter is the semantics; the
differential fuzzer compares it with the binary; S4's paragraphs link bidirectionally to their
clauses; the representative lemmas are proved.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0223** (superseded): superseded by R-0496/S7: merge into R-0496’s semantic contract.
- **R-0234** (superseded): superseded by R-0496/S7, V7: merge into V7 float-profile work. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0234)
- **R-0235** (superseded): superseded by R-0496/R-0495: retire PVal/PBinOp expansion; implement Core float semantics. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0235)
- **R-0261** (merged): duplicate of R-0496/R-0460: merge; already explicitly reconciled.
- **R-0268** (superseded): superseded by S5–S8: retire undecided architecture question; plan has chosen the chain.
- **R-0276** (superseded): superseded by R-0506/R-0496: retire general legacy rewrite; retain migration-needed wrappers only. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0276)
- **R-0408** (superseded): Superseded in core scope by R-0496/S7: merge required instrumentation into R-0496; separately specify any extra research.
- **R-0409** (superseded): Superseded by R-0496/R-0512: retire legacy redesign; carry demonstrated proof debt into Core proof engineering.
- **R-0456** (superseded): superseded by R-0496 for proposed PExpr port: retire PExpr-port design; reconsider only a measured Core-metatheory proposal. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0456)

### Task R-0497

**Phase:** 2, 4

**Objective:** Immutable, payload-bound pipeline artifacts — the artifact half of stage S5 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md); R-0492 owns the identity half.

**Status (2026-10-18): planned; starts with R-0492.**

Each stage's output is an immutable artifact bound to its exact payload, constructible only by its producer or verifier (`ValidatedCore` is publicly constructible today, `Pipeline.lean` ~:48); any mutation yields an unverified artifact; diagnostic artifacts (including `lowerUnverified`) have separate types that downstream stages and evidence APIs reject. The stage sequence follows the target IR chain (HIR → Core → `CheckedCore` → `CheckedMonoCore` → Place MIR → Memory CFG → BIR → LLVM printer or Machine IR), extended as each IR lands. Private constructors are API discipline; soundness comes from the stated theorems.

**Exit:** compile-fail controls cover artifact forgery, certificate reuse after mutation and the diagnostic-path bypass; `Pipeline.lean` exposes only typed stages.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0073** (superseded): superseded by R-0497/R-0503: merge into payload-bound validated BIR artifacts.
- **R-0082** (superseded): superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0082)
- **R-0116** (superseded): superseded by R-0497/R-0510/R-0514: merge; use payload binding and new IR chain. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0116)

### Task R-0498

**Phase:** 1, 4

**Reconciliation (2026-10-19):** Keep; grow manifest with new boundaries.

**Objective:** Enforcement demonstrated — stage S3 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; starts now.**

A claim → gate-leg → targeted-mutation manifest over
verification and language docs, enforcement-bearing comments in `Concrete/Proof`, `Check`,
`Backend`, and emitted contract text; the backend contract is generated from BIR op semantics once
S11 lands. R-18 – R-20 relabelled shape-only until their proofs exist.

**Exit:** every in-scope enforcement phrase has a manifest row whose mutation its leg kills in a
qualified campaign.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0273** (superseded): superseded by R-0491/R-0498: merge; map every listed family to a retained control. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0273)

### Task R-0499

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; R-0514 owns whole-language rollout.

**Objective:** Checker rules, authoritative on Core — stage S8 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned.**

Depends on S5, S7. Declarative judgments in
`docs/language/CHECKER_RULES.md` for use-once, conservation at reachable exits, borrow validity
(a source may not be moved, destroyed or incompatibly mutated while a reference to it is live;
shared/exclusive aliasing), no escaping or stored reference, capability containment (a call's
resolved required capabilities, after removing `Unsafe` only at a `trusted` call site, are a subset
of the caller's declared set, through pointers, generics and FFI), raw dereference only in a
`trusted` body or an `Unsafe`-covered context, and bounds (an access establishes an in-range index
or executes the bounds trap). A Core checker implementing them runs as a shadow beside the source
checker with a disagreement gate, then becomes authoritative and emits `CheckedCore` plus its
derivation; capabilities are re-checked after Mono. A rule-violation fuzzer applies one
rule-breaking mutation to accepted programs (including destroying a source while a
permitted local or downward reference is live, conflicting borrows, reaching a capability through a pointer or
generic, raw dereference outside both `trusted` and `Unsafe`); valid programs that must trap are
not checker rejections. Positive controls: trusted raw-pointer operations and trusted absorption of
`Unsafe` accepted; trusted absorption of any other effect rejected. The instrumented semantics is
the runtime witness.

**Exit:** every rule has an enforcing site, an S3 manifest row and a mutation class; the CI seed
set rejects every mutant and accepts every well-formed program with minimum completed counts per
rule; the instrumented semantics runs the corpus and fuzzer programs with zero violations; 090 –
094 and the 087 borrow-block case are rejected by the Core checker; the source checker holds no
ownership, borrow or capability rule. Shadow gates compare the two checkers on the common supported
subset; intended acceptance changes (such as references in aggregates, now rejected) have normative
expected results in an explicit migration manifest. The historical 092/095 aggregate-reference
programs remain as rejection fixtures, with versioned historical binary checks, and an accepted
loop-mutation/downward-borrow fixture detects stale storage.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0227** (superseded): superseded by R-0499/R-0507: merge into checker rules and soundness.
- **R-0228** (superseded): superseded by R-0499/S8 and V6: merge enforcement into R-0499; raw-operation extensions into V6. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0228)
- **R-0267** (superseded): superseded by R-0499/R-0507/R-0495: merge; capabilities and purity remain distinct.
- **R-0274** (superseded): superseded by R-0499/R-0514: retire source-checker/CompilerDB authority prescription. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0274)

### Task R-0500

**Phase:** 1, 4

**Reconciliation (2026-10-19):** Keep; row 6 adds the rules each slice admits.

**Objective:** Written language rules — stage S4 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; starts now.**

Normative paragraphs in `docs/language/` for block scoping
and shadowing, match-binding positions and names, statement vs value position, sequential
assignment within a loop iteration, literal typing and range (signed minima preserved), cast
semantics per width pair, evaluation order, short-circuiting, and second-class references; each
with accepted/rejected examples and fixtures.

**Exit before advancing past queue row 2:** every rule has a normative paragraph, examples, cited
fixtures and S1 generator coverage meeting its minima. S7's exit later adds links from each
paragraph to its semantic clause.

**Semantics decisions (2026-10-21):** the rules this task enforces are now written in their
canonical homes ([Decisions](#decisions)): references and scoping in MEMORY_SEMANTICS §15 (REF-6
rewritten: no `let`-bound reference, no assignment to a reference binding, proposed E0302 / E0303;
constants are values; SCOPE-1 staged behind the renamed pattern binder), evaluation order and
assignment in STATEMENT_EXPRESSION_MODEL and MEMORY_SEMANTICS §10 (exit 2j), and casts in
ARITHMETIC_POLICY §8 (exit 2k: the lossless class table, the three named-operation families and the
three stages). Each records its status on main and its open gaps.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0435** (superseded): superseded by R-0500: merge migration, renamed-pattern syntax and controls. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0435)

### Task R-0501

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep by slice.

**Objective:** Place MIR — stage S10 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18; amended 2026-10-21): planned.** Queue row 4 carries a whole-language structural
checkpoint for the existing profile after R-0514's checker is authoritative: `Lower`'s sequencing,
match compilation, cleanup/`defer` and check expansion move into Place MIR passes consuming
`CheckedMonoCore`, as shadow paths with disagreement gates against the existing lowering (structural
class 8). The Place MIR semantics, the per-pass differential against Core's semantics and the proofs
grow per slice (rows 3 and 6).

Depends on S7, S8a (consumes `CheckedMonoCore`). Carve sequencing, match compilation, cleanup/`defer`
edges and check expansion out of `Lower.lean` into Core → Place MIR passes, one job each, with a
Place MIR semantics; per-pass differential against Core's semantics.

**Exit:** `Lower` no longer performs these jobs; each pass is total and differentially tested with
minimum counts; cleanup expansion is proved for one representative function with early exits and
`defer`.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0079** (superseded): superseded by R-0493/R-0501/03: merge into typed operations and pass obligations. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0079)
- **R-0082** (superseded): superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0082)
- **R-0295** (superseded): Superseded by R-0501/S10: retire workload gate; MIR is already an architectural decision.

### Task R-0502

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; early design obligations precede full implementation.

**Objective:** Memory model, layout and Memory CFG — stage S11 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18; amended 2026-10-21): planned.** Queue row 4 carries a whole-language checkpoint
for the existing profile: one `Layout` authority supplies every size, alignment, offset and enum
representation the existing lowering and LLVM emission use, with a gate comparing it with the emitted
types (structural class 12). Representation certificates, Memory CFG and the proofs grow per slice.

Owns the shared raw-memory specification with S7, block–offset/provenance memory, `Layout`, representation certificates and Place MIR → Memory CFG lowering.

**Semantics decisions (2026-10-21):** layout is unspecified unless a `repr` guarantees it, each `repr`
guarantees only its documented properties, code relies on nothing but `sizeof`, `alignof` and array
stride = `sizeof`, and one `Layout` owns every fact; `Option` and `Result` are laid out per
instantiation like every generic enum ([LAYOUT_CONTRACT §3.3 – §3.4](docs/platform/LAYOUT_CONTRACT.md)).
Bug 122's per-instantiation repair is not deferred to this task; it lands under exit 0b.

**Exit:** Memory CFG has executable semantics and a verifier; `Layout` emits checked
representation certificates; Place MIR → Memory CFG comparisons cover related values and stores,
traces, traps, allocation and alignment cases; a representative memory-injection lemma is proved.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0076** (superseded): superseded by R-0502: merge into single proved Layout authority. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0076)
- **R-0085** (merged): duplicate of R-0076: merge through R-0076 into R-0502.
- **R-0088** (superseded): superseded — aggregate/memory ABI realization moves to R-0502 (layout) and the adapter boundary (R-0505/R-0517) across V2–V6.
- **R-0434** (superseded): superseded by R-0502: merge into layout certificates and Memory CFG verification. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0434)

### Task R-0503

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; absorb R-0278/R-0288.

**Objective:** BIR, the LLVM adapter and the rt module — stage S11 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned.**

Depends on R-0502. Owns Memory CFG → BIR, BIR semantics and verification, LLVM adapter legalization, the `rt` migration, generated backend contracts and replacement-path qualification.

**Exit (depends on R-0502):** LLVM is emitted only from validated BIR, under the adapter
contract above; BIR semantics and differential gates run; `EmitBuiltins.lean`'s LLVM-AST helpers
are replaced by `rt`; profile labels are emitted; the old `Lower` is retired only after the complete
replacement path meets its gates.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0073** (superseded): superseded by R-0497/R-0503: merge into payload-bound validated BIR artifacts.
- **R-0074** (superseded): superseded — merge into validated BIR (R-0503) and R-0517; the closed `llvm`/`qbe` backend set becomes the LLVM and LCB-native profiles. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0074)
- **R-0077** (superseded): superseded by R-0503/R-0509: merge with `rt` contracts, support inventory and trust accounting.
- **R-0079** (superseded): superseded by R-0493/R-0501/03: merge into typed operations and pass obligations. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0079)
- **R-0082** (superseded): superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0082)
- **R-0090** (superseded): superseded by R-0503: retire preferred-C-runtime prescription; implement Concrete `rt`, named libc FFI.
- **R-0109** (superseded): superseded by R-0503/R-0146: merge; remove assumption helpers must be C.
- **R-0278** (superseded): Superseded by R-0503/S11: merge into R-0503; BIR replaces this architecture.
- **R-0288** (superseded): Superseded by R-0503/S11: merge inspection, verification and parity requirements into R-0503. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0288)

### Task R-0504

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; retain R-0299/R-0300’s distinct assurance scopes.

**Objective:** Validators — stage S12 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned.**

The validator interface; validators for SSA
construction and for optimization certificates, each with a stated soundness `Prop` and one
representative proved instance. R-0299/R-0300 validate the LLVM profile: a pinned inventory of
proved functions records validation status per semantic edge and call closure; unsupported closures
stay `translation_trusted`/`blocked`; mismatches are `translation_mismatch` and never grant
native-code evidence.

**Exit:** SSA and optimization validators reject targeted semantic substitutions and unsupported
certificates; R-0299's BackendIR V1 validation and R-0300's independent replay pass their
admitted-subset, mismatch, unsupported and substitution controls; the emitted-LLVM profile checks
the retained `.ll` bytes including helper bodies; LLVM optimization, emission, linking and
runtime/ABI behaviour remain explicitly trusted for that profile.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0105** (superseded): superseded by R-0491/R-0504: merge debug/release and sanitizer controls. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0105)

### Task R-0505

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Re-pointed 2026-10-19: integrate the Lean Compiler Backend (R-0517); no Concrete machine backend. Absorbs R-0297's scheduling.

**Objective:** Native AArch64 profile: LCB integration, linking and image — stage S13 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design. Concrete integrates the Lean Compiler Backend (R-0517) and builds no machine backend of its own.

**Status (2026-10-19): planned; re-pointed from an owned Concrete machine backend to LCB integration.**

For the full first profile (AArch64, Apple ARM64 ABI, macOS, Mach-O): LCB supplies machine IR,
instruction selection, frame construction, register allocation, assembly, encoding and their proofs
(LCB's C01 – C03 and V01 – V05, external dependencies under R-0517). Concrete owns the profile around
it: the BIR → LCB-IR adapter (R-0517); **separate compilation and linking** — module interfaces,
compatible linking, symbol resolution and imported-contract satisfaction, source-module linking
related to independently compiled object linking, and compiler, profile, LCB version, interface,
runtime and dependency identities bound into the image; the `rt` module and startup; FFI ABI
realization at the adapter boundary (callbacks, external memory, aggregate argument/return
classification, as LCB's P07 ABI coverage admits them); and Concrete-side object/image validation.
R-0508 owns the semantic composition theorem.

**Exit:** corpus execution and performance against the LLVM profile are intermediate milestones.
The stage closes only when final-image validation checks actual instruction and data bytes, symbols,
relocations, entry state and startup against the recorded adapter output, LCB artifact and profile;
wrong-instruction, wrong-relocation, wrong-symbol, ABI and startup mutations are rejected; every
assembler, linker or loader behaviour LCB still assumes is named in the profile and theorem premises
and shrinks only as LCB's V04/V05 milestones land.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0081** (merged): merged — emitter independence between the LLVM adapter and the LCB adapter, and shared-helper disclosure, move to R-0505/R-0517.
- **R-0088** (superseded): superseded — aggregate/memory ABI realization moves to R-0502 (layout) and the adapter boundary (R-0505/R-0517) across V2–V6.
- **R-0089** (superseded): superseded — call/FFI/ABI matrices apply at the LCB adapter boundary (R-0505/R-0517); LCB's own ABI coverage (its P07) is an external dependency. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0089)
- **R-0099** (retired): retired — graduation of the native path is R-0517/R-0505's LCB-native profile; no QBE graduation exists. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0099)
- **R-0103** (superseded): superseded by R-0492/R-0505: merge; general separate compilation arrives in V6.
- **R-0296** (retired): retired — see the disposition in the task body. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0296)
- **R-0297** (delegated): delegated — see the disposition in the task body. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0297)

### Task R-0506

**Phase:** 1, 2, 4

**Objective:** Stated obligations and totality — stage S12a of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; the immediate checkpoint is queue row 2.**

**Immediate row-2 checkpoint:** enforce the transitive axiom policy on today's authoritative evidence
at status, issuance and consumption. Reject `sorryAx` and unregistered axioms; kernel-only evidence
permits only `propext`, `Classical.choice` and `Quot.sound`; registered native-evaluation and float
axioms require distinct trust labels. Positive controls and forbidden-transitive-dependency mutations
qualify this checkpoint independently of S5 and the new pass ledger.

Starts with S5 and grows as each boundary lands
— including S13 and linking — rather than after the lower pipeline: every pass's `Refines` and
every validator's soundness are type-checked Lean `Prop`s in `PassObligations.lean`, with explicit
premises and a ledger (stated / differentially tested / proved); `sorryAx` and unregistered axioms
are rejected transitively, kernel-only evidence permits only `propext`, `Classical.choice` and
`Quot.sound`, and registered native-evaluation and float axioms require separate trust labels; a totality lint covers semantics, checkers, validators and proved
passes (untrusted search may stay partial).

**Exit:** every registered boundary, including the native backend and linking, has a type-checked
obligation with explicit premises and status; evidence dependencies pass the declared axiom policy;
semantics, checkers, validators and proved passes pass the totality gate. Before each boundary is frozen, its
semantic relations and obligations type-check and a representative preservation or
validator-soundness lemma is proved for its motivating feature, including native encoding and
linking.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0102** (superseded): superseded — the operation gap ledger becomes typed pass obligations (R-0506) and the adapter's obligations (R-0517).
- **R-0236** (superseded): superseded by R-0506/R-0509: merge axiom classification and inventory.
- **R-0260** (superseded): superseded by R-0506/S12a: merge into pass/validator obligation ledger; preserve legacy historical view. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0260)
- **R-0276** (superseded): superseded by R-0506/R-0496: retire general legacy rewrite; retain migration-needed wrappers only. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0276)

### Task R-0507

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; cumulative proofs, not a deferred final phase.

**Objective:** Type and ownership soundness — stage S14 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned (Part 2; starts when its prerequisites exist).**

Progress and preservation for the Core checker over
Core's instrumented semantics; ghost erasure; trusted and FFI code enter as their contracts. Can
start as soon as S7 and S8 exist.

**Exit:** proved for the full checked subset; S8's hunt finds nothing the theorem does not exclude.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0227** (superseded): superseded by R-0499/R-0507: merge into checker rules and soundness.
- **R-0267** (superseded): superseded by R-0499/R-0507/R-0495: merge; capabilities and purity remain distinct.

### Task R-0508

**Phase:** 2, 4

**Objective:** Compiler correctness and proof transport — stage S15 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned (Part 2; starts when its prerequisites exist).**

Prove every ledger
obligation by simulation or validator soundness. Define refinement over maximal behaviours under
related initial states and admissible environments, with the explicit observation relation for
values, external memory and ghost erasure, and a preserved target-memory validity invariant; prove
native-to-Core refinement for executions satisfying the stated contracts, composed across separate
compilation and linking; state the resource relation and prove the sufficient-resource theorem.
Prove frontend correspondence: source bytes and import roots → HIR → `CheckedCore` against
independently specified syntax, resolution and elaboration judgments (a relation defined as
"whatever the elaborator produces" does not count). Prove VC generation against Core (R-0460).
Each binary-transported claim cites a theorem that its proposition is preserved by the refinement
relation; receipts record the theorem, profile and compiler build.

**Exit:** the composed source-bytes-and-import-roots-to-native-image theorem, including
independently specified frontend correspondence, is proved and re-checked by con-leche and nanoda.
HIR-only and `CheckedCore`-only theorems are intermediate milestones. Every binary-transported claim
cites its preservation theorem.

### Task R-0509

**Phase:** 1, 2, 4, 5

**Reconciliation (2026-10-19):** Keep; inventory starts now, minimal consumer at V1. The trusted list is non-increasing for a fixed supported scope; additions for newly admitted scope are recorded and reviewed separately. A separate compiler/checker execution-trust reduction plan with milestones is recorded here; R-0413 explores only the Concrete-hosted path.

**Objective:** Trusted base and independent evidence check — stage S16 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; the trusted-base inventory starts now, proofs in Part 2.**

`std`'s trusted
implementations verified against contracts or listed with audit (the list only shrinks); the
published trusted-base inventory. R-0353 independently checks retained evidence — decoding,
authenticated roots, subject/dependency binding, exact propositions, replay and policy — without
trusting stored verdicts; it does not by itself establish source-to-model or native-code
correspondence, which carry their own recorded assurance. Codec and hostile-input prototypes may proceed beside queue rows 3 – 8. R-0353's complete cross-profile
consumer remains at row 10, after row 9 and R-0004's prerequisites; a minimal independently checked
retained-artifact consumer is required for every publicly authoritative V1 claim. Prove soundness of `ClaimSpec` statement generation, evidence-to-subject/proposition
binding, authoritative verdict aggregation and retained-artifact checking; audit exact elaborated
axiom dependencies, so kernel-only verdicts admit only the named standard logical axioms while
native-evaluation and float axioms stay separately labelled. R-0413 dogfoods the evidence model onto
the compiler.

**Exit:** the inventory names every remaining assumption and owner; R-0353's acceptance criteria
pass with every 079 – 095 fixture pinning its expected result vector, and no envelope or replay
success upgrades an unchecked semantic edge. Statement generation, subject/proposition binding, verdict aggregation and
retained-artifact checking have proved soundness, with transitive assumptions classified by the
axiom-policy gate.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0077** (superseded): superseded by R-0503/R-0509: merge with `rt` contracts, support inventory and trust accounting.
- **R-0211** (superseded): superseded by R-0509/S16: merge into R-0509; retain audit command acceptance. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0211)
- **R-0230** (superseded): superseded by R-0509/S16: merge into R-0509 and slice profiles.
- **R-0236** (superseded): superseded by R-0506/R-0509: merge axiom classification and inventory.
- **R-0264** (superseded): superseded by R-0494/R-0509: merge implementation into R-0494, soundness proof into S16.
- **R-0265** (superseded): superseded by R-0495/R-0509: merge into statement/binding/aggregation soundness.
- **R-0266** (superseded): superseded by R-0509/S16: merge; distinguish trusted assumptions from ordinary proof dependencies. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0266)

### Task R-0510

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep whole-language implementation in R-0514’s checkpoint.

**Objective:** Checked specialization — stage S8a of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned.**

Depends on S8. Core semantics under type/capability
substitutions and trait evidence; Mono as specialization search plus a total validator checking
substitution, implementation identity, reachable-instance closure, signatures, Copy constraints,
literal ranges and capability requirements; produces `CheckedMonoCore` bound to `CheckedCore` and
its interfaces. Unbounded specialization or exhausted search produces an explicit incomplete
result.

**Exit:** every lowering entry point consumes `CheckedMonoCore`; missing or substituted instances,
wrong impls, invalid conditional Copy and effect-erased callbacks are rejected by targeted controls
(091's impl-capability case among them); direct and indirect generic calls are covered by
differential tests and a proved substitution/dispatch example.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0007** (superseded): superseded by R-0492/R-0510: merge, preserving function-symbol and metamorphic controls. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0007)
- **R-0116** (superseded): superseded by R-0497/R-0510/R-0514: merge; use payload binding and new IR chain. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0116)
- **R-0271** (superseded): superseded by R-0510/S8a, V5: merge into R-0510, retaining explicit law-evidence acceptance. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0271)
- **R-0437** (superseded): superseded by R-0514/R-0510: merge declaration and specialization checks; preserve distinct diagnostics. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0437)

### Task R-0511

**Phase:** 2

**Reconciliation (2026-10-19):** Proof-library engineering (refinement, clock, memory-injection, substitution and ownership libraries) has bounded deliverables recorded with each V1 increment.

**Objective:** The first cumulative verified releases — a proved path from checked Core to a native
image for a small language, carried through every stage of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices).

**Status (2026-10-19; amended 2026-10-20 and 2026-10-21): planned; queue row 3.** The design examples and isolated proof development start now; V1a, V1b and V1c are accepted only when their [prerequisite exits](#feature-prerequisites) hold, together with the subset obligations below. V1c names the restricted-source correspondence below as its own deliverable (2026-10-21).

Work (details and premises in the plan's *V1 milestones*): reviewed machine-checked design examples for
move/destroy, downward exclusive and shared borrows, reborrowing, structured cleanup and typed-value
representation in byte/provenance memory, before Core's interfaces freeze; **V1a**, checked Core →
executable BIR refinement for `i64`, `u64`, `bool`, locals and assignment, conditionals, a general loop
with structured exits, non-recursive direct calls, comparisons, checked `+ - *` and one
capability-authorized `print_int` event, with terminating, trapping and divergent behaviours; subsequent
V1 increments for the remaining widths, casts, division, shifts, recursion and loop forms; **V1b**,
native AArch64 through Concrete’s proved BIR → LCB-IR adapter and an applicable pinned LCB preservation theorem (R-0517). C01–C03 establish only the certified typed-assembly endpoint; rendering and encoding remain named assumptions until separately covered. R-0505 supplies final-image correspondence, with R-0508 proving the composition through the exact image, runtime and startup. Any assumed rendering, assembler, linker, loader or ISA correspondence remains explicit in the claim; mutation-tested image checks alone do not establish validator soundness; **V1c**, restricted-source correspondence (syntax,
resolution, elaboration judgments specified independently, then proved or certificate-checked); a
minimal canonical task identity and an independent retained-artifact consumer for every published slice
claim. Stages exercised, restricted to the admitted subset: R-0492 – R-0493, R-0495 – R-0497, R-0499,
R-0501 – R-0510 and R-0517, with minimal task identity and independent replay delivered under the V1 requirements.

**Exit:** for successful compilation of an admitted `CheckedCore` program to an identified image, under
related initial states, admissible environments and recorded resource premises, a Lean proof of S15's
maximal-behaviour refinement through the explicit observation relation, the preserved target-memory
validity invariant and the sufficient-resource theorem; checker soundness and basic capability
containment for the subset; every used pass or validator proved; one nontrivial Core claim transported
to its binary; the closure re-checked by con-leche and nanoda and bound to the exact retained artifacts.
V1 is described as source-to-native only once its frontend correspondence (V1c) is proved; until then the
published theorem states that it starts at `CheckedCore`. An outside expert's review of Core's V1
semantics and the design examples is recorded.

**V1a target programs (2026-10-20; planned).** Small programs inside V1a's scalar subset as the
[plan](docs/compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices) defines it, under
`examples/v1a/`, with `#[proof_by]` claims proved through today's evidence pipeline on the four
non-divergent targets:

- `sum_to`: an `i64` loop with a structured exit accumulating with checked `+` and printing its result
  through `print_int`; a claim relates the result to a Lean reference function.
- `max3`: `bool` conditions, comparisons and non-recursive direct calls; a claim that the result is one
  of the arguments and no smaller than any.
- `checked_trap`: checked `*` and `-` on `i64` and `u64` reaching overflow and underflow; the trapping
  behaviour is asserted, with a claim on the non-trapping inputs.
- `counter_u64`: `u64` locals and assignment in a loop with comparisons; a claim on the final value.
- `diverge`: a loop with no exit after a `print_int` prefix; its regression leg checks the prefix and
  nontermination within a fixed bound, recording exhaustion rather than claiming that this finite
  observation proves divergence; it carries no claim.

`scripts/tests/check_v1a_targets.sh` (new, in the full gate set) gives each program a leg: terminating
and trapping runs compare interpreter and native output and normalized outcomes with a checked-in
expected result, and `diverge` uses the bounded prefix and exhaustion check above; `--report model-diff`
agrees on every claimed function and each claim reads proved; and a subset check fails when a program
steps outside V1a's subset (another width, a cast, division, a shift, recursion, another loop form, an
aggregate or a reference). Once V1a's pipeline exists these programs are its acceptance programs and its
regression set: each compiles from checked Core to BIR and its BIR behaviour matches the recorded one.
They add nothing to V1a's theorem: its scope stays the subset the plan defines, and their claims are
candidates for this task's one transported claim, not new theorem statements.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0083** (retired): retired — there is no Concrete QBE backend to select; the no-silent-fallback rule moves to R-0511/R-0517. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0083)
- **R-0086** (superseded): superseded — its semantic vectors become adapter test vectors for V1 (R-0511/R-0517); later widths, casts, memory and floats follow the V-slices. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0086)
- **R-0097** (superseded): superseded by R-0511/R-0512: merge profile admission; retain no silent fallback. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0097)
- **R-0111** (superseded): superseded by R-0511/R-0137: merge reusable exits; retire required QBE showcase. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0111)
- **R-0231** (superseded): superseded by R-0511/R-0512: merge into each slice’s admission controls.
- **R-0238** (superseded): superseded by R-0511/R-0512: merge into cumulative slice matrices; retain orthogonal policies.
- **R-0263** (superseded): superseded by R-0511/R-0512: merge into V1–V7 slice ordering.
- **R-0277** (superseded): superseded by R-0511/R-0512: merge into each slice’s cumulative acceptance.

### Task R-0512

**Phase:** 4

**Reconciliation (2026-10-19):** Keep; enumerate V2–V7 delivery ownership explicitly. Delivery ownership for V2 – V7: each slice's cumulative exit from the plan is recorded as a milestone record under this task, including the maintained V7 feature inventory.

**Objective:** Migrate every claim onto compiler-generated Core statements and retire the legacy
evidence implementation, keeping its responsibilities.

**Status (2026-10-19): planned; proceeds with the slices (queue row 6).** The legacy implementation
(`PExpr` and `Proof.eval`, name-keyed tables, the theorem-type classification generator, the frozen
identity rows and amendments, attestations, per-surface status logic) receives soundness, compatibility
and security maintenance only, with no new semantic claim mechanisms.

Work (the plan's *Evidence migration*): an inventory of every supported claim and consumer with its exact
subject, proposition, coverage, transitive dependencies, assumptions and replay version; a claim migrates
once its complete semantic dependency closure has supported Core statements and sound statement
generation, binding and evidence interpretation — native compilation coverage is not required —
re-proved or carried by a proved bridge, with any change in domain, coverage or assumptions recorded for
review; each migration ships user-facing assurance and compatibility diffs; historical artifacts and a
versioned replay path are preserved; a superseded or invalidated claim never silently stays
authoritative. Subject identity, dependency binding and provenance remain required in the replacement.

**Exit:** every supported claim replays against a generated Core statement or has completed explicit
deprecation; legacy issuance code is deleted with its bug-class controls transferred to the replacement;
historical replay remains available; no name-keyed lookup grants current authority.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0087** (superseded): superseded by VP V7/R-0512: merge with total float model and profile-specific trust labels. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0087)
- **R-0097** (superseded): superseded by R-0511/R-0512: merge profile admission; retain no silent fallback. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0097)
- **R-0112** (superseded): superseded by R-0512: merge; retirement requires compatibility and rollback, not parity alone. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0112)
- **R-0123** (superseded): superseded by R-0495/R-0512: retire new legacy-model semantics; add admitted Core features/proof lemmas instead.
- **R-0176** (superseded): superseded by R-0495/R-0512: retire proposed legacy architecture; retain ownership goals in R-0151. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0176)
- **R-0202** (superseded): superseded by R-0495/R-0512: merge dependency-closure requirement into Core migration; maintain legacy checks until retirement.
- **R-0231** (superseded): superseded by R-0511/R-0512: merge into each slice’s admission controls.
- **R-0238** (superseded): superseded by R-0511/R-0512: merge into cumulative slice matrices; retain orthogonal policies.
- **R-0262** (superseded): superseded by R-0495/R-0512: retire blanket legacy bridge; retain only migration-pulled bridges. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0262)
- **R-0263** (superseded): superseded by R-0511/R-0512: merge into V1–V7 slice ordering.
- **R-0277** (superseded): superseded by R-0511/R-0512: merge into each slice’s cumulative acceptance.
- **R-0409** (superseded): Superseded by R-0496/R-0512: retire legacy redesign; carry demonstrated proof debt into Core proof engineering.

### Task R-0513

**Phase:** 1

**Objective:** Trustworthy, affordable gates: checked inventories instead of incidental counts, and a
fast push tier separate from full qualification.

**Status (2026-10-19): planned; queue row 2.** On 2026-10-18 – 19 every legitimate corpus change tripped
several count pins (the corpus split, the convergence inventory, the identity freeze), and the full set
took hours under load, then an hour of CI per push.

Work: replace incidental corpus cardinality pins with independently checked inventory and coverage
properties — every expected claim and comparison accounted for, exclusions with explicit reasons, and
missing entries, skipped work or unexpected eligibility shrinkage failing; keep minimum completed
comparisons by semantic feature and permanent positive/negative controls; test the gate machinery with
deleted inventory entries, disabled generators, empty suites and altered eligibility. A fast push tier
targeting five minutes, selected by the dependency closure of changed semantics, implementations and
proof infrastructure; the full tier on the exact candidate commit before qualification or release, and
nightly; semantic, checker, evidence and trusted-base changes run their complete affected proof and
regression closure; remote runners for the full tier with local reproduction kept; the Linux-only
claim-inventory step in the local full tier.

**Impact-based selection (2026-10-22, [development throughput](#development-throughput) item 9).** Today the
pre-push hook selects about 92 of 255 gates by area name, and stack-budget, trust-gate, float
round-trip and linear-discard failures were first seen in CI. The fast tier is to select by an
impact map from source files and fixtures to the gates that exercise them, so a change runs every
gate it can break. A path the map does not cover is to select the full set, so the map fails closed.
Interim checkpoint before the five-minute budget: pre-push median ≤ 10 min, and over 20 consecutive
landings no gate failure is first seen in CI. **Depends on:** R-0547, which records where each
failure was first seen.

**Exit:** adding a legitimate proved example requires no incidental count edit; deliberate coverage loss
fails; the fast tier meets its recorded budget; full results bind exact commits, toolchains and
artifacts; a fast-tier pass is never presented as full qualification.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0096** (superseded): superseded by R-0513/R-0491: merge; remove mandatory QBE availability.

### Task R-0514

**Phase:** 2

**Reconciliation (2026-10-19):** Keep; broader proof coverage remains Phase 4.

**Objective:** Make the shared front end sound for every user now: identities from Resolve on and one
authoritative checker over Core for the whole language, consumed by both the existing lowering and the
`verified` profile.

**Status (2026-10-19; amended 2026-10-20): planned; queue row 4, alongside R-0511.** The parts V1a's subset exercises proceed with V1a now and are accepted with it once its [prerequisite exits](#feature-prerequisites) hold; the whole-language rollout starts once row 0's in-flight candidates (batch 2, type-identity, generic-destroy) are accepted on main, implementing the rules as exits 2i – 2k state them, without waiting for row 2's other exits (amended 2026-10-21, [priority decision](#decisions)). Neither task waits for the other's complete exit; both use the same identities, Core representation, checker implementation and checked-artifact interfaces. The source-tree checker returns `Unit`
and elaboration rebuilds the program independently, so the program checked is not the program compiled;
bugs 087 (borrow-block use-after-free), 090 and 092 – 094 lived in that gap, and name-keyed
environments produced 082, 087, 093, 094 and 096 – 098. The slices prove these rules only as they reach
them (V1 – V7, beginning with scalar typing and basic capabilities in V1, downward borrows in V2, and extending through resources, polymorphism and unsafe/FFI interactions), years out; this task makes the checker authoritative for the whole language before then.

Work: deliver R-0492's identities and R-0497's immutable, payload-bound artifacts across the shared
front end. Implement R-0499's declarative rules once over Core, extending coverage to the whole
supported language through shadow checking, qualified disagreement gates and an explicit migration
manifest. Both profiles consume the exact `CheckedCore` payload; specialization consumes that artifact
and produces payload-bound `CheckedMonoCore`, which is the only input to lowering. R-0510 supplies
whole-language specialization validation and post-Mono capability checks. V1 uses the same
implementation restricted by its admission predicate, with a proved trivial specialization instance
where applicable. R-0514 depends on the Core representation and rule specifications needed for its
checks, not on whole-language compiler-correctness proofs; S7/S14/S15 proof coverage stays recorded per
subset.

**Exit:** `check_ir_no_string_identity.sh` covers the shared front end and the source checker; the Core
checker is authoritative for the whole language and the source checker holds no ownership, borrow,
reference, capability or `Unsafe` rule; 087's borrow-block case, 090 – 094 and 096 – 098 are rejected or
compiled correctly through it; R-0499's rule-violation fuzzer rejects every rule-breaking mutant in the
qualified suite and accepts its well-formed positive controls, with minimum completed counts per rule
and feature combination.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0116** (superseded): superseded by R-0497/R-0510/R-0514: merge; use payload binding and new IR chain. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0116)
- **R-0274** (superseded): superseded by R-0499/R-0514: retire source-checker/CompilerDB authority prescription. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0274)
- **R-0437** (superseded): superseded by R-0514/R-0510: merge declaration and specialization checks; preserve distinct diagnostics. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0437)

### Task R-0515

**Phase:** 3

**Reconciliation (2026-10-19):** Keep coordination; existing tasks retain implementation ownership.

**Objective:** Make Concrete a language people choose to use, measured continuously alongside the proof
track rather than after it, by advancing bounded checkpoints of the tasks that already own each concern.

**Status (2026-10-19; amended 2026-10-21): planned; queue row 8, delivered through bounded checkpoints
within the feature track of the [capacity allocation](#capacity-allocation).** After rows 0 – 2 qualify,
each checkpoint records its scope, accountable maintainer, reviewer, acceptance evidence and integration
slot before starting. Usability, interop and application work share the feature track's single slot and
capped share; the verified compiler's share and slots are reserved. Quarterly pilots are a
continuing feedback obligation, not a prerequisite for advancing rows 9 – 17; missed pilots and their
recovery dates are recorded rather than counted as completed.

R-0515 owns pilot coordination and acceptance evidence. R-0485/R-0488 retain ownership of ergonomics
changes and their prerequisites; row 7 supplies application workloads; row 16 retains
dependency-upgrade/replay acceptance and flagship graduation. R-0385–R-0388 own editor implementation,
R-0281 owns native debugging, and R-0416–R-0424 own generated-code performance evidence. R-0355 consumes
that evidence and owns release budgets. R-0515 coordinates their bounded user-facing checkpoints; it
creates no separate debugger or benchmark infrastructure. R-0358 owns stability and deprecation, R-0346/R-0347 onboarding, R-0359 contribution and
governance, R-0399 – R-0401 concurrency. A shared workload may supply evidence to several tasks, but does
not close their distinct exits.

Work, in bounded checkpoints:
- **Before the first pilot.** Pull forward the minimal R-0346/R-0347 onboarding path: install on a named
  supported host, create, build, run and test a project, use one dependency, and repair one
  representative error. Retain a non-author transcript, elapsed time and all author assistance; keep
  tutorial examples executable in CI.
- **Now.** (1) *External pilot:* each quarter, a non-author attempts a bounded program with a stated user
  goal and executable acceptance checks on the supported existing profile. Record installation and
  first-success time, failed attempts, author assistance, error-recovery effort, missing APIs and
  build/runtime measurements. Publish a consented or redacted friction log. Each blocking finding
  receives an owner and disposition; implemented fixes retain regression evidence and are retried by the
  participant. Reuse row 7 workloads where suitable without requiring the full application port.
  (2) *Diagnostics quality bar:* inventory diagnostic families encountered by the pilot and maintain a
  reviewed coverage list. New or changed diagnostics, and existing diagnostics blocking the pilot, must
  identify the violated rule, semantic consequence, relevant source or input location, and a valid next
  action or explicit unsupported boundary. Reuse R-0137/R-0466 and R-0488's structured-path criteria.
  Test representative failing programs and repaired positive controls; suggested repairs must preserve
  the intended ownership and authority policy. Snapshots supplement these checks
  ([DIAGNOSTIC_UX](docs/compiler/DIAGNOSTIC_UX.md)).
- **Practical debugging (2026-10-21; before workloads expand beyond RocksDB slice 1).** A non-author
  diagnoses two injected failures in a program built on RocksDB slice 1 (a runtime trap, and a
  foreign call that fails) with source locations, stack traces through Concrete, runtime and foreign
  frames, and inspected values, on the supported existing profile. R-0281 owns the implementation.
  The transcript, elapsed time and author assistance are retained. This checkpoint is a prerequisite
  for the four-adapter checkpoint and for R-0490's component workloads, not for RocksDB slice 1.
- **Continuing maintenance (2026-10-21).** The pilots measure keeping a program working, not only
  the first build: a participant other than the authors maintains a small project across one
  compiler upgrade and one dependency upgrade, recording the breakage, migration diagnostics, effort
  and author assistance. Row 16 still owns the external-user dependency-upgrade and replay acceptance.
- **V1 tooling checkpoints.** At V1a, qualify the minimal R-0385 – R-0388 editor path over the shared
  compiler session: types, diagnostics, definition navigation, proof/trust/capability status and replay
  links. At V1b, qualify DWARF line mappings and source-level trap traces for the admitted native subset
  on each declared supported profile/target. These checkpoints retain the incremental-driver area prerequisites; any
  unresolved dependency and its effect on the release schedule is recorded before the checkpoint is
  committed. R-0355 owns performance baselines and regression gates, extended with equivalent-workload
  comparisons against pinned C, Rust and Zig toolchains and the verified-versus-LLVM comparison.
- **Before public release.** R-0358 owns the language/stdlib stability and deprecation policy. Publish a
  supported/missing `std` coverage inventory and assign owners to pilot-blocking gaps. Begin contributor
  onboarding and succession planning under R-0359 and the verified-pipeline ownership discipline; name
  critical-area reviewers and retain an independently completed review or release-replay exercise.
- **Pull-gated.** Expand `std` and the tutorial into a book when recorded workloads justify them. Each
  activated item records an owner, bounded scope and executable acceptance criteria before work starts.
  R-0399 – R-0401 retain concurrency design and implementation ownership; record a dated decision or an
  explicit deferral with a reopening trigger.

**Exit (per checkpoint):** *onboarding* — a non-author completes the first-project path with the
transcript, time and assistance retained, and its examples run in CI; *now* — two consecutive quarterly
pilots retain their program, acceptance results and assistance record, each with a participant-validated
improvement to a recorded obstacle (author-completed steps remain unresolved usability limitations;
pilots continue quarterly after this checkpoint), and the diagnostic coverage list has no unowned
pilot-blocking gaps, with gates exercising failing inputs and valid repairs for each selected family;
*V1 tooling* — scripted editor tests exercise every promised operation and verify CLI/editor agreement
after source, dependency and profile changes, including stale and unsupported evidence; a debugger
resolves known source lines and nested trap fixtures produce expected source frames on every declared
supported profile/target; benchmark sources, correctness checks, hardware, toolchain versions, build
flags, safety settings, repeated measurements and numeric regression thresholds are published, cold
builds, incremental edits and runtime are measured separately, CI rejects a deliberately injected
threshold violation, and unsupported comparisons are reported explicitly and do not count as covered;
*practical debugging* — a non-author diagnoses the injected trap and the failing foreign call from
source locations, stack traces and inspected values, with the transcript, time and assistance retained,
and scripted fixtures check that the trap and the foreign failure each resolve to their source frame;
*continuing maintenance* — one non-author maintenance record across a compiler upgrade and a dependency
upgrade is retained, each blocking finding has an owner and disposition, and the record repeats with
later pilots; *before public release* — the stability policy, `std` coverage inventory and named reviewers are
published.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0108** (merged): duplicate of R-0416/R-0515: merge profile-labelled budgets and measurements. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0108)

### Task R-0516

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep; required after preview, not only at final ecosystem stage.

**Objective:** Release automation, so that nightly builds, the release train and milestone releases cost
minutes of attention instead of days.

**Status:** planned; Phase 3 release-preparation checkpoint coordinated by R-0333/R-0515
within the feature track of the capacity allocation. Its nightly-publication subset precedes the first
automated nightly; its complete exit precedes the first automated train or milestone.

Work: one command produces, from an exact main commit whose full tier passed (R-0513), the R-0333
candidate record (source and toolchain identities, artifact identities, supported subset and profiles,
retained check results, compatibility statements, open blockers and accepted limitations), the release
artifacts for every declared platform, a changelog section and migration notes drafted from merged
changes, and the claims matrix for the release. Nightly builds publish automatically with their records;
train and milestone releases add the human review of notes and claims.

**Exit:** from immutable source, toolchain, policy and reviewed release-input records,
produce the candidate record and packaged artifacts without manual edits to generated
outputs. Full qualification must match the exact source/toolchain identities, and
candidate installation, claims and artifact checks run against the artifacts to be
published. Demonstrate rejection of failed, missing or wrong-commit qualification,
artifact substitution and unapproved milestone publication. Demonstrate one reviewed
train/milestone publication rehearsal, retry after partial failure without replacing
an existing release identity, and the applicable R-0427 rollback procedure. Two
successive scheduled nightlies publish unattended with retained records. Semantic,
compatibility and limitation decisions remain explicit reviewed inputs.

### Task R-0517

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Coordinated under queue row 3 (V1b) with R-0505.

**Objective:** Lean Compiler Backend as Concrete's native backend. The native AArch64 code
generation of the `verified` profile (V1b onward) is the separately developed, general-purpose
[Lean Compiler Backend](https://github.com/unbalancedparentheses/lean-compiler-backend) (LCB), a Lean 4 port of QBE 1.3 whose correctness is
developed alongside its implementation. Concrete **uses** LCB; it does not build it, and it builds no
machine backend of its own.

**Status (2026-10-19): planned; coordinated under queue row 3 (V1b) and R-0505.** The existing
restricted adapter is [Concrete draft PR #8](https://github.com/unbalancedparentheses/concrete2/pull/8)
(branch `feat/lean-backend-integration`): an experimental `--emit-qbe` path from today's checked SSA,
covering wrapping arithmetic, comparisons, branches and loops and rejecting checked arithmetic, calls
and memory. It is a consumer experiment, not the adapter this task delivers.

**Dependency contract.**
- LCB is a pinned dependency: an exact LCB version (commit and release identity) enters through a
  lake/nix input whose lock records LCB's own `upstream-lock.json` hash, so the QBE 1.3 and Zig
  reference sources LCB ports are part of Concrete's recorded build identity.
- Concrete and LCB build with the same Lean toolchain (4.34.1 today); an LCB upgrade that changes the
  toolchain is a Concrete toolchain upgrade under R-0302/R-0357's procedure.
- Concrete imports LCB’s versioned source and target semantics, initial/final-state relations, behaviours, ABI projection, certified-domain predicates, admission soundness and preservation/safety theorem statements. Each theorem must identify the actual compilation function and exact checked input/output artifacts, its direction, premises and transitive axiom dependencies. Compatibility-mode support is not certified-domain membership.
- Before freezing the adapter, Concrete states type-checked bridge obligations from BIR to these exported interfaces and proves them for each admitted slice. They relate signed/unsigned values and widths; checked overflow and division failures; defined traps versus stuckness, undefined behaviour and compiler rejection; and external traces, capability tags and ghost erasure. No defined Concrete behaviour may be discarded by an LCB domain restriction without a proved admission precondition.
- Memory bridges relate Concrete’s block/provenance, permissions, initialization, lifetime and address-exposure semantics to LCB’s memory and target frames, preserving target-memory validity as well as observations. Call bridges establish the exact Apple ARM64 argument/result layout, stack alignment, clobbers, saved registers, external-memory effects and imported contracts. Features without these bridges remain outside the certified profile.
- Composition proves compatibility of related initial states, environments and observation relations; adequacy between Concrete’s clock semantics and LCB’s execution relations; preservation of maximal behaviours, including divergence; and well-founded administrative stuttering. Forward-to-backward conversion requires proved determinacy/receptiveness hypotheses or an applicable oracle-based alternative. Leaf-source determinism alone does not discharge external-interaction hypotheses.
- The resource bridge distinguishes compile-time limits and evaluator exhaustion from program outcomes, establishes available frame space, address non-wrap and other target premises, and discharges Concrete’s sufficient-resource obligation. LCB admission limits are not runtime stack-safety proofs.
- Every dependency upgrade records semantic, domain, ABI, artifact-schema, theorem and axiom changes alongside Lean/toolchain changes. CI rebuilds the imported proofs and complete composition, replays retained evidence, and runs admission, adapter, ABI and image mutation controls before promotion. Changed definitions require reviewed proof migration and versioned claim boundaries; cached evidence cannot cross identities. Preserve a qualified pin and rollback path when upstream changes cannot yet be adopted.

**Ownership.** Concrete owns: the BIR → LCB-IR adapter, preserving Concrete's semantics — checked
overflow is lowered to explicit checks or to LCB's checked operations once LCB's E02 Concrete
adapter work provides them (wrapping LCB arithmetic does not implement checked operations by itself),
and traps, capability-tagged effects and the runtime ABI are preserved; the adapter's refinement
proof (BIR semantics to LCB-IR semantics under the explicit observation relation); version pinning
and upgrade qualification; Concrete-side final-image checks (with R-0505); and the composition of
theorems (with R-0508). LCB owns its IR semantics, lowering, register allocation, encoding and their
proofs. LCB's milestones — C01 – C03 (first composed preservation result), V01 – V05 (semantics,
pass preservation, allocation, encoding, composition) and E02 (Concrete-facing adapter support) —
are tracked here as external dependencies, never as Concrete work; Concrete records their status
from LCB's published reports and does not schedule them.

**V1b theorem.** V1b's native theorem is the composition: Concrete's checked Core → BIR refinement
(V1a), the adapter's BIR → LCB-IR refinement, and LCB's preservation theorem for its exact certified
domain. It is V1b only when that pinned domain covers V1's declared subset (see the release gate); it carries
every trust boundary LCB names (today: rendering, ISA model, assembler, linker, host OS and the Lean
kernel, until LCB's V04/V05 remove them) as explicit premises in Concrete's theorem and profile.

**Release gate.** LCB’s Stage 1 exit is necessary but insufficient for V1b. V1b requires a pinned certified LCB domain covering every reachable operation and behaviour of Concrete’s declared V1 subset, including checked-arithmetic trap paths, direct calls and the `print_int` external-event contract, together with Concrete’s proved adapter, semantic bridges, final-image correspondence and native-exit obligations. A smaller leaf-function intersection may ship as a separately named intermediate theorem with explicit admission restrictions; it does not complete V1b.

**Fallback:** whenever any V1b obligation remains unmet — LCB’s Stage 1, a certified LCB domain covering V1, or Concrete’s composed V1b exit — a candidate may ship V1a’s proved CheckedCore → BIR result and an explicitly selected experimental LCB-native path, with native execution tested and labelled `tested`/`backend_trusted`. That native path does not satisfy the `verified` profile, complete V1b or queue row 3, or graduate the verified-preview acceptance scope. R-0333 records the narrower delivered scope and outstanding native obligations; no source-to-native or native-transport claim is issued. Fallback requires an explicitly selected native assurance mode distinct from a request for certified native output. A request for certified native output fails when V1b evidence is unavailable; it never silently downgrades. Here “validated” means only the listed structural or differential checks unless a named validator-soundness theorem establishes a stronger result.

**Transferred controls.** The useful controls of the retired QBE tasks (R-0072 – R-0112, R-0296) apply
at the adapter: emitter independence from the LLVM adapter, a generated adapter support matrix with
explicit unsupported diagnostics and no silent fallback to LLVM, three-way differential observations
(Core semantics, LLVM profile, LCB-native) with classified disagreements and retained reductions, ABI
classification against native C round trips, deterministic emission and full toolchain identity
(including the LCB lock), and performance comparison between the LLVM and LCB-native profiles under
R-0416 – R-0424.

**Exit:** a pinned LCB version builds inside Concrete's Nix/lake build with the shared toolchain and its
upstream lock recorded in the candidate identity; the adapter covers V1's admitted subset with checked
arithmetic, traps, `print_int` and direct calls, and rejects everything else with a stable
diagnostic; the adapter refinement is proved against LCB's imported semantics; the composed V1b
statement type-checks with LCB's trust boundaries as named premises; wrong-check, dropped-trap,
wrong-width and substituted-callee mutations of the adapter are rejected by the proof or by the
differential suite; and the release record states which LCB milestones the shipped claim depends on.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0072** (retired): retired — LCB pins QBE 1.3 in its own upstream lock; Concrete pins the LCB version under R-0517. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0072)
- **R-0074** (superseded): superseded — merge into validated BIR (R-0503) and R-0517; the closed `llvm`/`qbe` backend set becomes the LLVM and LCB-native profiles. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0074)
- **R-0081** (merged): merged — emitter independence between the LLVM adapter and the LCB adapter, and shared-helper disclosure, move to R-0505/R-0517.
- **R-0083** (retired): retired — there is no Concrete QBE backend to select; the no-silent-fallback rule moves to R-0511/R-0517. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0083)
- **R-0084** (delegated): delegated — a typed QBE-style IR and printer are LCB's input layer; Concrete's adapter constructs LCB-IR under R-0517.
- **R-0086** (superseded): superseded — its semantic vectors become adapter test vectors for V1 (R-0511/R-0517); later widths, casts, memory and floats follow the V-slices. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0086)
- **R-0088** (superseded): superseded — aggregate/memory ABI realization moves to R-0502 (layout) and the adapter boundary (R-0505/R-0517) across V2–V6.
- **R-0089** (superseded): superseded — call/FFI/ABI matrices apply at the LCB adapter boundary (R-0505/R-0517); LCB's own ABI coverage (its P07) is an external dependency. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0089)
- **R-0091** (merged): merged — native test-mode entry generation joins R-0075's driver and the LCB-native profile (R-0517).
- **R-0099** (retired): retired — graduation of the native path is R-0517/R-0505's LCB-native profile; no QBE graduation exists. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0099)
- **R-0101** (delegated): delegated — legality verification of the QBE-style IR is LCB's (its F04/V02); Concrete validates adapter output under R-0517. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0101)
- **R-0102** (superseded): superseded — the operation gap ledger becomes typed pass obligations (R-0506) and the adapter's obligations (R-0517).
- **R-0104** (merged): merged — deterministic emission and toolchain identity join R-0146; the LCB version and upstream lock are pinned by R-0517.
- **R-0296** (retired): retired — see the disposition in the task body. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0296)
- **R-0297** (delegated): delegated — see the disposition in the task body. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0297)

### Promoted from roadmap prose (2026-10-19 reconciliation)

The tasks below give an owner and an exit to open work that was previously stated only in prose
(the source lines, at commit `7b134645`, are listed in the [reconciliation record](docs/project/ROADMAP_RECONCILIATION.md)).
Each records what it absorbs; none deletes the prose it came from.

### Task R-0518

**Phase:** 1

**Objective:** Close the mutation-harness lifecycle defects H-1, H-4, H-7 and H-9 recorded under the
post-R-0004 qualification checkpoint, each independently.

**Status (2026-10-19): planned; under R-0513's gate-health checkpoint.** H-2 and H-6 merge into
R-0513 directly; H-3 and H-5 are closed. H-7 carried a "before the first qualified campaign"
deadline that the 2026-10-11 qualification passed without meeting; that deadline is recorded as
missed, and the published sound-by-argument invariant (one authority consumer, inside
`candidate_incoherent`'s qualification branch, reached only when `SCOPE_NOTES` is empty) is gated
until the fields are separated.

**Exit:** H-1 — a supervisor signalled during launch keeps the lock while any campaign child
survives, with a signal-injection control; H-4 — the golden copy re-checks its own `run_id` after
copying, with a concurrent-replacement control; H-7 — integrity refusals and scope notes are
published as separate fields and the authority consumer reads only the former; H-9 — lock ownership
records a namespace-independent identity so a sandboxed run's lock is reclaimable, with an
`--unshare-pid` control. Each item closes separately with its control and commit.

### Task R-0519

**Phase:** 1

**Objective:** Finish the mutation campaign's remaining completeness work: the mutation-52 survivor,
full worktree support and measured shard balancing.

**Status (2026-10-19): planned.** Mutation 52 ("Elab V2 input: resolved field use omitted") survived
on main and on the R-0484 branch; the later sharded campaign does not establish its closure, because
it ran a different inventory. Worktree support has only its early refusal (2026-10-03). Shard
balancing is performance-pulled.

**Exit:** mutation 52 is killed by a named gate leg in a qualified run, or retired with a recorded
reason; the harness discovers checkout-local and shared Git state with `git rev-parse` and passes a
two-worktree lock test; shard assignment uses measured durations once a campaign exceeds its job
budget, with serial/sharded verdict agreement checked.

### Task R-0520

**Phase:** 3, 5

**Objective:** Produce `solver_error`, so that an absent or broken solver is distinguishable from a
proof-difficulty `unproven`.

**Status (2026-10-19): planned.** Absent solver, garbage solver and the negatives fixture all report
`unproven` today. That is fail-closed, so nothing over-claims, but consumers cannot tell an
environment defect from a proof result.

**Exit:** the three gate sites that expected `solver_error` restore their strict assertions; absent,
crashing and malformed-output solvers produce `solver_error` on every surface R-0494 renders; a
genuine `unproven` control is unchanged.

### Task R-0521

**Phase:** 2, 5

**Objective:** Compiler provenance and one canonical production `VerificationTask` (queue row 9).
Separate source-build provenance from the identity of the executable bytes that actually ran, and
build every verification task through one constructor binding the exact subject, proposition,
contract state, rule set, dependency material and environment.

**Status (2026-10-19): planned.** A minimal form is required with V1 (every publicly authoritative V1
claim needs minimal task identity and executable provenance); the cross-profile form completes at
row 9. Receipt `importsId` and proof-workspace binding gaps merge here.

**Exit:** a receipt names the executable digest it was produced by, not only the source and build
recipe; substituting a differently built compiler with the same source identity is detected; every
production task is built by the one constructor, and a hand-built task is refused by status,
issuance and R-0353's consumer.

### Task R-0523

**Phase:** 1, 4

**Objective:** Explain the unclassified `fixed_capacity` obligation that both the old and new
obligation paths drop, and make that class of drop visible.

**Status (2026-10-19): planned.** One obligation is rejected by both paths without a recorded reason.

**Exit:** the dropped obligation is classified (generated, unsupported with a reason, or a defect
with a bug record); an unclassified drop fails the coverage report; a control reintroducing an
unexplained drop is rejected.

### Task R-0524

**Phase:** 6

**Objective:** Two-state mutation contracts (queue row 13): a narrow record/array state model, `old`,
`modifies`/frames, then ghost locals and parameters with erasure and admissibility rules.

**Status (2026-10-19): planned; after R-0473/R-0474.** Links R-0175 (frame inference) and R-0239
(frame/flow contracts). Unsupported heap and reborrow shapes refuse explicitly.

**Exit:** `old` and frame clauses on records and arrays are typed, generated against Core's
two-state semantics and replayed; a contract omitting a mutated field is refused; ghost locals and
parameters erase with a proved erasure lemma for the admitted forms; unsupported shapes report
`unsupported` rather than `proved`.

### Task R-0525

**Phase:** 6

**Objective:** Ninth-table conversion (queue row 14): extract and replay the three currently
fail-closed `proofFnsExt` links using R-0524's mutable-state model.

**Status (2026-10-19): planned; after R-0524.** Retargeted to Core migration (R-0512): the conversion
produces generated Core statements, not new legacy table rows.

**Exit:** the three links replay against generated Core statements; until then they provide no
authority, which a control asserts; no-authority exclusions remain gated during migration.

### Task R-0526

**Phase:** 6

**Objective:** Checked recursion and total functions (queue row 15, with R-0270's totality requirement
and R-0060's specification library): `#[decreases]`, well-founded descent VCs over every recursive
edge including mutual recursion, recursive definitions extracted to each admitted proof host, and
induction-capable automation; only checked-total definitions are callable from contracts.

**Status (2026-10-19): planned.** A decrease of `n - 1 < n` alone is not well-foundedness: lower
bounds, every recursive edge and combined measures across cycles are required. Termination is not
liveness. Symbolic stack bounds from admitted measures need the frame/resource correspondence of
R-0224/R-0248 before they become byte bounds.

**Exit:** recursive functions with checked measures are admitted, including one mutually recursive
pair; a measure that fails to decrease on one edge, or is unbounded below, is refused; a contract
calling a non-total helper is refused; one recursive claim is proved by induction and replayed.

### Task R-0528

**Phase:** 3, 5

**Objective:** Cross-command resource limits: bound input size, memory, time, import closure and
diagnostic volume for every command that reads untrusted inputs (receipts, tasks, manifests,
policies, packages, source).

**Status (2026-10-19): planned.** Each limit names its owning command; a limit is a refusal with a
diagnostic, never a silent truncation.

**Exit:** each listed input class has a documented limit and a hostile fixture exceeding it that is
refused with a stable diagnostic; inputs at the limit are accepted.

### Task R-0530

**Phase:** 3, 4

**Objective:** Typed descriptors and `dup`/`dup2`/`fdopen` alias ownership — the "second step" left
open when R-0484 closed.

**Status (2026-10-19): planned; conditional on a workload that needs descriptor duplication.**
R-0484's closure stands; this is new work, not a reopening.

**Exit:** descriptors are typed handles; duplication produces an explicitly owned second handle;
closing one handle does not invalidate the other's capability accounting; double-close and
use-after-close of an alias are rejected or trap with a regression fixture.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0484** (done): done — closed 2026-10-08 at `245cd51c`; the body is retained for its accepted limits; the typed-descriptor second step is R-0530. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0484)

### Task R-0536

**Phase:** 3, 4

**Objective:** Explicit enum discriminant values: honour explicit tags, reject collisions, and
validate representation/FFI agreement.

**Status (2026-10-19): planned (previously prose item "7a" without an R-id).** The representation
proof enters V2/V6 through R-0502's layout certificates.

**Exit:** explicit tags are emitted as written; duplicate or out-of-range tags are rejected; a C round
trip confirms ABI agreement for a `repr`-annotated enum.

## Area: Standard Library And Core APIs

Goal: build the small standard library people need before real workloads,
packages, editor tooling, freestanding targets, and release work can be honest.

Done when: a normal C/Rust-style Concrete program can use documented core APIs
for errors, bytes/text/path, collections, formatting/parsing, I/O capabilities,
tests, and oracle helpers, with every public stdlib item carrying an evidence
class and authority/allocation story.

**Excellence exit contract.** the stdlib area is judged on *honesty, not breadth* — the
one stdlib where every public API's authority, allocation, failure, ownership,
and evidence are visible and gated, over a proven pure core. Breadth is copied
only after that holds. Four properties gate the phase, each backed by a check,
not prose:

- **Capability-complete.** Every hosted operation carries its exact domain
   capability — `fs`→`File`, `env`→`Env`, `time`→`Time`, `process`→`Process`,
   `net`→`Network`, `console`/`io`→`Console`. `trusted` internals may use
   `Unsafe`, but the public wrapper must re-expose the real cap (a `trusted`
   extern requires *no* capability, so authority collapses into `Unsafe` unless
   re-added by hand). The stdlib manifest gate fails if a hosted op is behind
   `Unsafe` alone.
- **One IO spine.** All output targets a single `io.Writer` (fn-pointer handle,
   authority at acquisition, no `dyn`), and there is one `File` type: `fmt`,
   files, console, `std.test` output, logs/progress, and future sockets are all
   `Writer` producers/consumers. No second sink, no duplicate `Writer`/`File`.
- **Linearity-real.** Collections destroy live non-`Copy` elements on
   `drop`/`clear`/`remove`/overwrite (not merely reclaim the buffer); no hidden
   `Clone`, no implicit move-out from indexed containers; deterministic map/set
   traversal. Until non-`Copy` element drop is wired, the limitation is recorded,
   not silent.
- **Self-enforcing + selectively proven.** The five-fact manifest (allocate / consume /
   fail / capability / proof-class), derived from compiler facts and gated
   (the stdlib manifest gate); the strict error rule (`Option` for absence, `Result` for
   recoverable domain/environment failure, trap/abort for invariants / OOM /
   bounds / arithmetic, explicit cross-module wrapping, no implicit conversion
   web); and the stable, workload-pulled pure core (`option`/`result`, selected
   `bytes`/`numeric` helpers, and checked conversion boundaries) carrying Lean
   evidence where the API has stopped moving. Do not try to prove all stdlib
   APIs before workloads validate their usefulness; every API needs an evidence
   class, but only stable central pure APIs should be upgraded to `proved`.

These enforce a three-layer shape: a **pure core** (no capabilities, no
allocation unless explicit), an **alloc layer** (`with(Alloc)`, later explicit
allocator values), and a **hosted authority layer** (every API names its domain
cap). Iteration is internal only (`for_each`/`fold`/`_ctx`, optional
`Continue | Break`, no iterator trait or lazy adapter tower). Hold breadth —
broad crypto/compression/networking/threading and framework-style APIs — until
the four properties hold and a workload pulls it; the pure core is the priority,
not the periphery.

### Confirmed Stdlib Boundary Corrections

This is a specification set for verified drift in the
stdlib that already exists. It is not a coordinated parity migration and does
not block unrelated workload-pulled the stdlib area slices. Only confirmed contract
violations and the narrow infrastructure needed to prevent their recurrence
are immediate; broader API shapes below remain owned by the workload that
demonstrates the need.

Canonical rule:

> A public ordinary stdlib API exposes typed values, ownership, its exact
> domain capability, allocation, and recoverable failure. Raw pointers,
> platform sentinels, C strings, ABI structs, and `Unsafe` stop at a narrow
> trusted adapter boundary. Explicitly raw modules and `_unchecked` functions
> are the visible exception.

Execution rule: land each confirmed correction as its own green vertical slice
with a discriminating fixture, mutation check, manifest/API-snapshot update,
and migrated consumers. Do not build Command/Rng/SystemTime/digest abstractions,
unify every handle, or redesign every error type merely for symmetry. Deferred
notes become work only when their named workload or failing gate pulls them.

> Moved to [ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md#global-task-sequence) (historical record; nothing in it schedules work).

### Task R-0004

**Phase:** 1

**Disposition (2026-10-19):** done — closed 2026-08-20; the body is retained for its explicit follow-ups and historical boundary; physical move to the archive under R-0481.

**[shipped] Objective:** Fix proof-subject freshness and fail closed. R-0004 closed on 2026-08-20;
its closure result and accepted limits follow.

**Closed 2026-08-20** on a protected serial closure run at `a839d519`: 205 of 205 gates discovered,
executed and passed, clean tree, `HEAD == origin/main`. `proved` now requires a current exact subject,
a complete dependency root, explicit trust and assumptions, and a receipt only a successful kernel
replay can mint. The closure record is archived in
[ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md#r-0004--closed-2026-08-20).

**Accepted limits at closure (each now owned by a live task):**

- `PackageIdentity` over-binds (whole dependency closure; definitions scoped by package content) —
  fail-safe demotion and renaming, never a misattributed proof; the five-way identity redesign is R-0482.
- Post-build executable bytes are unclaimed: the compiler identity names the build, not the bytes —
  R-0521 (compiler provenance).
- Ten v1 proof links stay pinned because the structural producer refuses intrinsic casts; they migrate
  with claims on Core (R-0495/R-0512).
- The closure run did not include the mutation campaign; the post-closure campaign qualified on
  `670ec85a` (2026-10-11, run 38104755470).

The **Post-R-0004 milestones and longer-term research** subsection below remains current future work.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0004).

### Post-R-0004 milestones and longer-term research

These are recorded so they remain visible without distracting from the strict critical path below.
They are not R-0004 blockers. This is the canonical sequencing/decision map, not a second copy of the
detailed task specifications: R-0482, R-0353, R-0473/R-0474, R-0440 and the later phase tasks own
their implementation detail and gates.

**Evidence architecture**
- Five-way identity migration under R-0482: `PackageScopeIdentity` names declared publisher/package
  scope without dependency content; `PackageArtifactIdentity` binds canonical `(module identity,
  content digest)` pairs for the package's own artifact; `ResolutionContextIdentity` binds the
  resolved dependency/toolchain context; `DefinitionIdentity` binds scope, module, declaration and
  implementation; claim dependency roots carry reachable dependency content. Add declared
  origin/namespace and bootstrap attestations, generated references, receipts and consumers
  atomically. No mixed identity-version fallback. Fix duplicate enumeration at its producer; never
  erase legitimate duplicate-content modules to stabilize a digest.
- Compiler provenance hardening: retain the portable source-build identity, then bind source/build
  provenance, build recipe/toolchain and final executable bytes as distinct facts through a portable
  streaming digest or signed build manifest. The current build identity deliberately does not detect
  post-build binary patching.
- Canonical production `VerificationTask`: one authoritative constructor binding exact subject,
  proposition, contract state, rule set, dependency material, and environment. Complete before
  freezing receipt v1.
- R-0353/schema conformance: publish experimental canonical task/receipt encodings and hostile
  conformance fixtures, implement a consumer independently of the compiler pipeline, and freeze
  receipt v1 only after both implementations agree. Receipt validity remains distinct from policy
  acceptance.
- Model-faithfulness evidence: explicit relation kinds and independently checked
  `CorrespondenceEvidence` under R-0482.
- Package-level evidence composition: how dependencies consume partial evidence, how upgrades
  invalidate claims, and how absent dependency evidence is represented.
- Typed `PolicyDecision` under R-0440: policy consumes fresh receipt facts, trust, assumptions,
  advisories, and release requirements; a valid receipt is not automatically a release acceptance.
- Axiom and assumption inventories: record Lean axioms, trusted declarations, runtime assumptions,
  and imported hypotheses per claim.
- Evidence revocation and rollback: withdrawn proofs, compromised toolchains, revoked package
  versions, rolled-back receipt stores.
- Signed transport without authority inflation: DSSE/in-toto envelope authenticates bytes, never
  upgrades the evidence class.
- Schema lifecycle and migration: compatibility windows, unknown-field handling, canonical decoding,
  schema bumps, migration tooling, and controlled downgrade of old receipts to `needs_recheck`.

**Trust and compiler boundaries**
- Source-to-VIR bridge audit: keep it small, versioned, and independently reviewable; gradually
  prove or translation-validate critical portions.
- Toolchain-upgrade migration: a complete recheck plan for Lean, compiler, proof-library, and
  rule-set upgrades.
- Reproducible release artifacts: compiler source, dependencies, build recipe, target, linker/
  runtime inputs, executable digest; separate from ordinary proof receipts.
- FFI, `Unsafe`, runtime, and backend boundaries: explicit dispositions for claims crossing foreign
  code, trusted runtime, platform adapters, or backend assumptions.
- Platform qualification: exercise receipt production and consumption on every supported host,
  including filesystem semantics, process interruption, and line endings.

**Language and specification work**
- Exact contract witnesses under R-0473/R-0474: canonical typed `ContractIdentity`, theorem-to-spec
  binding and imported hypotheses distinguish contract dependence from implementation dependence.
- Narrow two-state mutation follows that substrate: `old`, `modifies`/frames, record/array state,
  then ghost locals/parameters/state. Only afterward does the narrow mutable-state extraction convert
  the ninth table; its existing fail-closed/no-authority disposition remains until then.
- Checked total functions and `#[decreases]`, followed by the spec standard library, precede richer
  or relational logic.
- Specification adequacy and vacuity controls: policy-visible adequacy checks, contradiction
  detection, and permanent vacuity controls.
- Relational properties: noninterference, constant-time behavior, equivalence, refinement.
- Closed-world library evolution: enumerable callable sets, compatible upgrades, and escape hatches.

**Robustness and operations**
- Parser and decoder fuzzing: receipt, task, manifest, policy, and package decoders.
- Resource limits and denial-of-service behavior: bounded input sizes, memory, time, import closure,
  and diagnostic output.
- Atomic evidence persistence: temporary files, fsync, rename, permissions, concurrent readers,
  interrupted writes, crash recovery.
- Gate ownership and reachability: every gate names its owner, freshness producer, mutation
  coverage, CI lane, and last successful execution.
- Long-running stability and nondeterminism: scheduling, locale, directory, process-count, and
  filesystem variations.
- Performance baselines: replay time, receipt size, dependency-closure cost, memory use,
  invalidation breadth.
- `ProofCache` remains performance-pulled rather than early post-R-0004 work. When a measured
  bottleneck justifies it, cache-off/on, cold/warm, missing, stale and corrupt cases must agree on
  authority before the cache graduates; cache contents never mint or upgrade evidence.

**Product and governance**
- External-user dependency-upgrade workflow: receive package/evidence, replay independently, replace
  a dependency, and obtain an exact machine-readable and human explanation of every claim that stayed
  current or moved without access to the producer checkout.
- IDE/CI evidence lenses and a recurring non-author ergonomics audit follow that vertical workflow
  rather than substituting for it. The public example portfolio is capped at three deep flagships:
  HMAC-SHA256, a secure update-bundle verifier that combines bounded parsing with file integrity,
  rollback policy and transferable evidence, and a small protocol state machine. New compiler
  behaviours use dedicated fixtures; they do not create another showcase by default.
- Human-readable evidence explanations: which exact subject, theorem, dependency, trust boundary,
  or environment field moved.
- Evidence-diff format: stable machine-readable diff for upgrades, toolchain changes, trust widening,
  schema migration, and proof replacement.
- Independent implementation and security review of R-0353, schema, verifier, and hostile corpus.
- Release assurance bundles: claims, receipts, policy decisions, assumptions, dependency evidence,
  toolchains, replay commands, and unresolved refusals.
- Privacy and disclosure policy: avoid exposing checkout paths, usernames, private dependency
  locations, or confidential proof material.
- Ecosystem compatibility policy: compatible package upgrades from the evidence perspective.

**Longer-term research**
- Second proof-producing kernel (Rocq or another kernel) in shadow first; Why3 remains a comparative
  reference only.
- Verified or translation-validated compiler bridges.
- Certificate-producing automation; keep non-certificate SMT results explicitly `solver_trusted`.

**Language posture after the evidence-and-contract sequence**

Concrete should be described as an **evidence-bearing sequential-language core with stated,
bounded trust assumptions and an explicit, shrinking TCB**. Do not call the language or compiler
"verified" while any of these boundaries remains:

- **H19:** the Core-to-obligation bridge is unproven. Four of five Register A rows have their
  semantics half discharged, but none proves that the emitted obligation denotes that condition.
- **H20:** `bv_decide` certificate checking still runs as native code, so bitvector evidence trusts
  the checker binary at that boundary.
- **H21:** nonlinear SMT evidence cannot yet be certificate-replayed and therefore remains
  `solver_trusted` by an upstream limitation.

Keep three categories distinct when describing what follows:

1. **Philosophical-core gaps.** Model correspondence is first: exact identity prevents
   substitution, but it does not prove that a Lean model describes the selected implementation.
   R-0482 owns independent model identity, explicit relation kinds, and independently checked
   `CorrespondenceEvidence`. H19-H21 are the other named boundaries that the word "verified" waits
   on.
2. **Scheduled critical-path work.** Package evidence and typed policy (the packages area/R-0440 and the
   five-way identity migration), developer tooling (the editor area after schema stability), and resource
   bounds (the provable-subsets area profiles plus the runtime-safety area obligations) are owned and sequenced work, not unowned
   language holes.
3. **Optional, workload-pulled research.** Typestate remains research-gated unless the
   `protocol_state_machine` flagship demonstrates that ordinary contracts cannot express the
   required compile-time state discipline. If that pressure appears, pull forward only the smallest
   explicit mechanism that preserves auditability.

Permanent exclusions remain design assets rather than missing features: garbage collection,
exceptions/unwinding, hidden async runtimes, trait-object dispatch, capturing closures,
source-generating macros, implicit conversions, ambient authority, and broad invisible `unsafe`.
Postfix `?` joins this list: recoverable failure uses `Result`, exhaustive `match`, and explicit
`return Result::Err ...`. Concrete does not hide a function exit behind expression punctuation;
there is no implicit conversion, cleanup, drop, or capability use during propagation.

> Moved to [ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md#strict-post-r-0004-critical-path) (historical record; nothing in it schedules work).

No evidence-affecting language feature crosses the initial identity/task/verifier stabilization seam
unless its change includes the corresponding identity version, atomic migration, replay behavior,
consumer update and adversarial controls. Why3 remains a comparative reference and never enters this
chain as a backend. A second proof-producing kernel remains separately research-gated; it does not
block this product path.

> Moved to [ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md#historical-record-and-detailed-slice-traceability) (historical record; nothing in it schedules work).

> Moved to [ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md#binder-frames-position-and-meaning-are-separate-obligations) (historical record; nothing in it schedules work).

> Moved to [ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md#multi-kernel-where-the-ladder-actually-stands-2026-08-08) (historical record; nothing in it schedules work).

### Test-harness principles (2026-08-08) — rules, not tasks

Extracted from a review of the contract-validation work, and written here rather than in a commit
message because they are reusable and were each paid for once. R-0476 is the task that drains the
backlog they describe; these are the rules that stop it refilling.

**1. Every universal assertion needs a non-emptiness witness.** The hazard is a theorem, not a
style preference:

> all properties hold over the empty collection

`all(P(x) for x in coll)` is TRUE when `coll` is empty. So is "the forbidden string does not
appear in this output" when there is no output. Both report a pass having checked nothing. Unless
emptiness is the intended case, a universal assertion must be paired with a witness that its
collection is inhabited.

**2. A helper that asserts something about a SELECTED BLOCK must first prove the block exists.**
Empty selection must never mean success. `assert_block_absent` should distinguish three distinct
failures, which are not the same event:

1. missing anchor — the thing being inspected is not there;
2. malformed or unterminated block — it is there but cannot be parsed;
3. forbidden content present — the actual property failed.

Implemented today: 1 and 3. Case 2 is deliberately not implemented, because no report shape was
found that reproduces it, and an untested branch inside an assertion helper is worse than a named
gap. Recorded so the omission is a decision rather than an oversight.

**3. A fatal negative must not share a fixture with an unrelated positive.** Several positives may
live together, and several *accumulating* diagnostics may live together — but a negative that
becomes fatal erases the positive's subject, and the positive then passes for want of anything to
inspect. This is not hypothetical: promoting the impure-call defect from a report line to a check
error made `spec_ghost_totality`'s pure-helper control pass BECAUSE the file was rejected
wholesale and its report was empty. The control was green and proved nothing. The positive now
lives in `examples/contract_positive/pure_contract_calls`, where nothing else can fail first.

**4. Audit the shape, not the instance.** When one of these is found, the same shape is usually
live elsewhere. Fixing `assert_block_absent` left the identical defect in two `assert_absent`
helpers and in 23 universal JSON assertions. The places to look: `sed`/`awk` block extraction,
`grep` pipelines accepting empty input, assertions using `! grep`, command substitutions hiding a
failed producer, section checks where an earlier fatal diagnostic removed the section, and JSON
filters where an empty list makes `all(...)` true.

**5. Never describe a check as more than it enforces.** The impure-call check rejects
capability-requiring calls and reuses a diagnostic saying "spec/ghost must be pure and total" —
which claims more than it does. Capability-free does not mean valid in a proposition: such a
function may still trap, diverge, allocate through a badly summarised path, depend on runtime
state the contract semantics do not model, call trusted or foreign code, or have no total logical
interpretation. The rule that would be correct — *contract calls must resolve to admitted spec fn
declarations, or to executable functions with an explicit, checked logical interpretation* — needs
a notion that does not exist yet. Until it does, this is containment of EFFECTFUL calls and is
described that way (H27, R-0473).

**6. A failed search is not evidence of absence.** I reported "Register C has no row table" after
grepping `docs/verification/VC_BRIDGE_REGISTER.md` and finding nothing. Register C's rows were never in that
file — C1-C4 are compile-time theorems in `Concrete/Report/Evidence.lean`, the module they
constrain, which is the better place for them. The claim reached this roadmap and two status
summaries before an external audit caught it. Before recording an absence, establish where the
thing WOULD be if it existed; a grep that returns nothing has told you about the file you
grepped, and nothing else.

**7. A comment that claims a gate exists must be checked like any other claim.** While writing
`Check.impureFnsIn` I wrote that it and `ReportVC.impureFnNames` "are held together by a shared
gate assertion". No such assertion existed. The comment was load-bearing — it is the reason a
reader would accept the duplication — and it was false from the moment it was written. A claim
about the test suite, made inside the code the suite is supposed to constrain, is exactly as
checkable as a claim about behaviour, and nothing was checking it. The gate now exists
(`check_contract_negatives.sh`), which is the right repair: make the sentence true rather than
delete it.

**8. Resolved typed records must never be built on names.** Storing a name now with a promise to
replace it by an identity later puts the name in the record and in every consumer built against
it. This is why the identity substrate moved into R-0473 rather than waiting for R-0474, and why
`ResolvedContractRef` is a sum rather than a string with a companion type table.

### Task R-0480

**Phase:** 1

**Reconciliation (2026-10-19):** Keep remaining lifecycle/authority gates; archive completed physical migration.

**Documentation information architecture.**

**Scheduling: incremental; physical moves wait for a stable integration boundary.** The ownership
and lifecycle rules may land immediately. Do not bulk-move files while R-0004 and its evidence
modules are changing: link churn, merge conflicts and stale references would obscure the work the
reorganization is meant to clarify. Move one topic family at a time, with link and drift gates
green before and after.

The physical migration of the stable documents into the `docs/` families landed 2026-08-14 (record
archived in [ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md#r-0480-physical-documentation-migration-2026-08-14)). Phase A's full lifecycle-header inventory and Phase B's
one-owner/duplicate-current-status controls remain open; physical neatness is not completion of the
authority audit.

**Objective:** make the repository answer three questions without requiring institutional memory:

1. Which document is authoritative for this fact?
2. Is this file normative, an active design, a stable reference, historical, or exploratory?
3. Where should a change to this subject be made so that a second source of truth is not created?

The canonical ownership map and precedence order live in `docs/README.md`; the research promotion
and retirement workflow lives in `research/README.md`. This task implements and enforces those
rules. It must not create a second roadmap, a second current-status dashboard, or a second copy of
normative prose under a cleaner directory name.

**Phase A — inventory before movement:**

- inventory every file under `docs/` and `research/` with lifecycle status, topic, canonical owner
  and—where applicable—the roadmap task or decision that consumes it;
- identify duplicate present-tense claims, stale status snapshots, adopted research that still
  reads as a competing proposal, and active designs with no named normative destination;
- add or normalize visible status/owner headers; dated measurements must name the commit,
  toolchain and denominator they describe;
- keep `CLAIMS_TODAY.md` as the shipped-claims authority and this file as the sole canonical task
  status. Changelogs and research notes may link to those facts but may not restate them as current.

**Phase B — executable information architecture:**

- gate internal Markdown links and repository-relative file references;
- gate the mechanically checkable subset of document ownership: required status headers, canonical
  owner links, no advisory state represented as a receipt fact, and no known duplicate current-
  status headings;
- require every `adopted` or `closed` research note to link to its normative owner; retain rejected
  hypotheses and dated measurements as history rather than silently rewriting them;
- distinguish the manual contradiction audit from automated checks. Prose judgment is not made
  trustworthy by putting an arguable heuristic in a shell gate.

**Phase C — staged physical layout:** use the target families documented in `docs/README.md`:
`docs/language/`, `docs/compiler/`, `docs/verification/`, and `docs/platform/`, while retaining the
existing `docs/stdlib/`, `docs/bugs/`, `docs/book/`, and `docs/NOTES/` roles. `research/` already has
topic directories; reorganize its contents by promotion/retirement state, not by duplicating the
normative hierarchy. For every moved family:

1. choose and record its canonical owner;
2. update all inbound links and code comments in the same commit;
3. leave a small pointer only where an external or durable path requires compatibility;
4. run documentation, reference and roadmap-drift gates before and after;
5. do not mix the move with semantic compiler changes.

**Acceptance:**

- every normative document has one lifecycle status and one owner entry;
- every adopted/closed research note points to the normative result, and exploratory notes contain
  no undated claim of current implementation status;
- one subject has one present-tense authority; contradiction searches name concrete forbidden
  formulations rather than relying on reviewer memory;
- broken-link and ownership gates fail non-vacuously under a removed owner, stale link and duplicate
  current-status mutation;
- at least one document family completes the staged move with zero broken references before the
  remaining families are scheduled;
- repository-root navigation remains small: `README.md`, `ROADMAP.md` and `CHANGELOG.md` point into
  the hierarchy instead of accumulating detailed design prose themselves.

### Task R-0481

**Phase:** 1, 2, 3, 4, 5

**Reconciliation (2026-10-19):** Keep canonical owner; absorb overlapping R-0447/R-0478 work.

**Final repository architecture: temporal honesty, generated facts, and enforced dependency
boundaries.**

**Status and scheduling:** RATIFIED TARGET; implement in vertical slices. R-0480's subject-based
documentation move is complete and is NOT to be undone. Journal/status infrastructure and the gate
library may proceed independently. The `Concrete/` module moves wait until R-0004 finishes scoped
definition identity and the affected evidence boundaries are stable; then align them with
R-0114–R-0118 rather than moving the same modules twice. VIR moves when TermIR promotion begins.

**Why this task exists.** Repository disorder is primarily informational, not cosmetic. Four kinds
of material have repeatedly occupied the same documents and data structures:

1. what is normatively true now;
2. what is measured now;
3. what happened at a dated point in history; and
4. what the project intends to build.

Moving files without separating those temporal roles only relocates drift. The durable design uses
subject-oriented directories for discovery, explicit lifecycle/owner metadata for authority, a
journal for dated narrative, canonical machine artifacts for measured state, and compiler module
boundaries that mirror producer/consumer trust boundaries.

#### Canonical target layout

```text
/
├── Concrete/                 compiler implementation
│   ├── Frontend/
│   ├── Resolve/
│   ├── Check/
│   ├── Core/
│   ├── Proof/                proof/extraction producer machinery
│   ├── Evidence/             portable typed evidence objects
│   ├── VIR/                  canonical proposition language and certificates
│   ├── Report/               projections only; never reconstructs evidence
│   └── Backend/
│
├── docs/                     maintained documentation, organized by subject
│   ├── language/
│   ├── compiler/
│   ├── verification/
│   ├── platform/
│   ├── project/
│   ├── stdlib/
│   ├── bugs/
│   └── book/
│
├── research/                 non-normative exploration, organized by subject
├── journal/                  dated historical records
│   ├── incidents/
│   ├── audits/
│   ├── decisions/
│   └── milestones/
│
├── artifacts/                schemas and checked/generated repository facts
│   ├── schemas/
│   ├── status/
│   └── manifests/
│
├── proofs/                   attached proof developments
├── std/                      Concrete standard-library source
├── examples/                 user-facing programs and evidence workloads
├── tests/                    compiler, oracle, adversarial and negative corpora
├── scripts/
│   ├── tests/
│   │   └── lib/              shared non-vacuous assertion vocabulary
│   ├── generators/
│   └── tooling/
├── site/
├── paper/
├── editor/
└── bench/
```

The root remains conventional and small. `README.md`, `ROADMAP.md`, `CHANGELOG.md`, `LICENSE`,
`Makefile`, Lean/Lake/Nix manifests and toolchain files, `Concrete.lean`, `Main.lean`, and the shared
logo are legitimate root entry points. Ignored `.lake`, `.cache`, `.build`, and `.concrete-cache`
directories are generated local state, not repository information architecture, and are not moved
merely to make a checkout look emptier.

#### Axis 1 — subject location and temporal authority are separate

Do NOT replace the subject hierarchy with `docs/normative/` and `docs/reference/`. A file's subject
determines its directory; its lifecycle and authority are typed metadata. Otherwise all normative
material becomes a second flat miscellaneous directory and cross-subject navigation degrades.

Every maintained document uses a visible lifecycle from R-0480 and, where it is not itself the
owner, names the canonical owner. The intended header model is:

```text
Status: normative contract | stable reference | active design | historical
Owner: docs/<subject>/<owner>.md
Current implementation: ROADMAP R-NNNN
Historical record: journal/<class>/<dated-entry>.md
```

`CLAIMS_TODAY.md` owns shipped public claims. Normative design documents own semantics and trust
contracts. `ROADMAP.md` owns active sequencing and qualitative transition state. `CHANGELOG.md`
owns landed milestone history. Research is never normative merely because an idea was adopted.

#### Axis 2 — journal for dated narrative

Create `journal/` for material whose value is historical context rather than current authority:

- `journal/incidents/` — failures, regressions, security advisories and fire drills;
- `journal/audits/` — dated corpus measurements, red-team reports and architecture reviews;
- `journal/decisions/` — dated decision rationale and supersession records;
- `journal/milestones/` — implementation narratives and release retrospectives.

Migrate `docs/NOTES/`, dated roadmap narratives and bug write-ups selectively, not mechanically.
An active reproducible defect specification remains in `docs/bugs/`; an incident narrative moves
to the journal and links the living bug/contract. A journal entry records date, commit, toolchain,
denominator, conclusion-at-the-time, current normative owner and supersession state.

"Append-only" means historical claims are never silently rewritten to appear prescient. It does
NOT forbid correction: an erratum or superseding entry explicitly preserves the original record
and links the correction. Journal text cannot define current status, policy or shipped claims.

#### Axis 3 — canonical generated status and artifacts

Add an `artifacts/` ownership layer. Canonical machine data does not live conceptually inside docs;
human documentation and CI render projections of it.

At minimum, generate and schema-version:

- `artifacts/status/corpus.json` — every discovered example/test/proof input, its stable identity,
  classification, eligibility, measured/unmeasured disposition and named refusal;
- `artifacts/status/verification.json` — subjects, stored links, current digests, correspondence,
  dependency roots, receipts, assumptions, trust and multidimensional evidence state;
- `artifacts/status/mutations.json` — mutation families, attacked invariant, production consumer,
  killing gate, positive control, last verified commit and toolchain;
- `artifacts/manifests/` — package, proof-link, implementation, theorem, table and dependency
  inventories consumed by migration/replay tooling;
- `artifacts/schemas/` — canonical wire/status schemas and compatibility/revocation rules.

`make status` (or its eventual equivalent) reads authoritative compiler/gate outputs and emits the
canonical structured facts plus an optional generated Markdown view. It never scrapes decorative
report prose when typed or structured facts exist. Generation must fail on missing denominators,
duplicate identities, unknown classifications, partial joins, failed producer commands and schema
mismatch.

Generated counts replace hand-copied numbers in status summaries: discovered files, measurable
files, proof subjects, stored links, root coverage, correspondence coverage, receipt migration and
mutation coverage each have one denominator-owning producer. A dashboard with an incomplete input
inventory is a refusal, not a smaller confident number.

The roadmap keeps qualitative status that cannot be safely inferred from counts: shadow-only,
blocked on a decision, implemented without a production consumer, acceptance condition open, or
authority transition forbidden. It LINKS generated quantitative facts rather than copying them.

#### Axis 4 — one gate assertion library and one mutation registry

Consolidate gate helpers into `scripts/tests/lib/`. The shared vocabulary must encode the failure
shapes repeatedly found in this repository, including:

- presence before content checks;
- absence only after proving the anchor exists;
- universal assertions only with an explicit non-empty witness/denominator;
- exact denominator and exact join checks;
- paired moving/non-moving controls;
- positive controls for negative validators;
- producer exit-status validation before stdout interpretation;
- typed/named refusal preservation;
- mutation registration and killed/survived/inconclusive dispositions.

Required conceptual helpers include `assert_present`, `assert_absent_after_anchor`,
`assert_nonempty_then_all`, `assert_exact_denominator`, `assert_join_complete`,
`assert_positive_and_negative_control`, `assert_command_succeeded`, and
`assert_mutation_killed`; exact names may follow existing shell conventions. Private copies retire
only after the shared helper has discriminating self-tests, including deliberate vacuity attacks.

Create one machine-readable mutation registry. Every security/evidence mutation records its stable
family ID, invariant, subsystem, production path, killing gate, positive control, authority state,
and last execution identity. "Which boundaries lack negative controls?" and "which mutations were
not run under this toolchain?" become queries, not manual audits. A registered-but-never-run or
synthetic-only mutation cannot be reported as production coverage.

#### Axis 5 — one corpus manifest

Generate a repository corpus manifest spanning `examples/`, `tests/`, `proofs/`, proof tables and
stored proof links. Each row has a stable owner and classification such as positive fixture,
negative fixture, showcase, workload, oracle, performance case, migration link or hostile input.
The manifest records whether the item reached the relevant phase and, if not, its typed refusal.

The manifest is also the prerequisite for reducing `examples/`. Classify before moving or deleting:

- `examples/` ultimately contains the three public flagships, one deliberately small onboarding
  sequence and only complete user-facing workflows; prefer documentation snippets over another
  standalone tutorial project;
- minimal compiler/language regressions belong under `tests/fixtures/` or the existing equivalent;
- hostile and substitution inputs belong under `tests/adversarial/`;
- performance inputs belong with benchmarks; and
- larger compatibility/porting programs are workloads, not additional flagships.

Existing paths are not renamed merely to make the tree look tidy. A shallow example moves only
after its regression value has a manifest row, an owning gate and—when needed—a replacement fixture.
This keeps fewer examples from becoming fewer tests.

This manifest owns denominators such as discovered/measured/unmeasured files and prevents two gates
from using identical-looking but independently maintained regexes. Per-file and per-subject joins
must be explicit; equal totals are not evidence that two inventories describe the same population.

#### Axis 6 — Proof, Evidence, VIR and Report are different layers

Keep `Concrete/Proof/` and `Concrete/Evidence/` separate:

- `Proof/` owns compiler-side extraction, theorem attachment, prover integration, classification
  production and proof replay orchestration;
- `Evidence/` owns portable typed subjects, identity wrappers, digests, theorem/table bindings,
  dependency material, correspondence results, roots, assumptions, trust propagation, receipts and
  policy inputs;
- `VIR/` owns the promoted TermIR proposition language, closed fragment, canonical codec,
  structural evaluator and certificate attachment/checking interfaces;
- `Report/` owns human/JSON views over validated objects only.

The intended dependency direction is:

```text
Frontend -> Resolve -> Check -> typed Core -> Proof producer
                                      |             |
                                      v             v
                                     VIR -------> Evidence -> Report
                                                     |
                                                     v
                                          standalone verifier artifacts
```

This diagram is a constraint, not permission for every displayed arrow. In particular:

1. `Evidence` MUST NOT import `Report`.
2. `Report` may render validated evidence but MUST NOT mint receipts, recompute roots, classify
   theorem dependencies, reconstruct identity from strings, or independently derive evidence.
3. `VIR` MUST NOT import frontend parsing, project loading, compiler reporting or ambient prover
   elaboration. Its consumer grammar and evaluator remain small and structural.
4. The standalone verifier shares canonical schemas/VIR semantics, not compiler implementation or
   report code.
5. Proof producers may emit candidate objects; only closed constructors/validators in the owning
   evidence layer produce authoritative values.
6. Unknown imports, constructors, schema fields and evidence dispositions fail non-current by
   default. Success is enumerated; catch-alls do not grant it.

Enforce these rules through generated import-graph gates, private constructors, compile-failure
probes and production-consumer mutations—not comments alone.

#### Concrete/Evidence target ownership

After R-0004 stabilizes its identities, migrate by semantic owner rather than by today's filenames:

```text
Concrete/Evidence/
├── Identity/        PackageIdentity, DefinitionIdentity, ModelIdentity,
│                    ContractIdentity, PropositionIdentity, typed digest domains
├── Subject/         implementation and proof-subject preimages/digests
├── Attestation/     implementation references, models, relation/scope/evidence
├── Contract/        exported behavior/effect/frame/resource interfaces
├── Dependency/      typed edges, classification, table bindings, correspondence, roots
├── Receipt/         envelope, replay binding, environment identities, serialization
└── Policy/          assumptions, trust/advisories, multidimensional acceptance
```

Do not collapse all existing Proof modules into Evidence. PExpr/ProofCore extraction, theorem
metaprograms and backend proof production remain producer machinery. A relocation lands only when
the semantic owner is clear, imports remain acyclic, behavior is unchanged, and every code/comment/
gate reference moves in the same commit.

#### Concrete/VIR promotion

Do not invent a parallel VIR. Promote and extend `Concrete/Semantics/TermIR.lean` when its roadmap
work begins. The target adds canonical serialization and domain-separated proposition identity,
the closed declared fragment needed by obligations, a total structural consumer evaluator,
adversarial parsing, and certificate bindings for Farkas/linear and DRAT/bitvector witnesses.

The fragment permanently excludes ambient metaprogramming and proof-kernel features irrelevant to
Concrete claims. Extensions are explicit trust-surface decisions with semantics, codec, evaluator,
negative controls, compatibility/versioning and independent-parser implications reviewed together.

#### Sequencing — one move per stable boundary

1. **Preserve the completed subject-based docs migration.** Finish R-0480 lifecycle/owner headers
   and duplicate-current-status gates; do not create `docs/normative/` or `docs/reference/`.
2. **Introduce journal conventions and a pilot migration.** Move one dated audit/incident family,
   preserve errata/supersession semantics, and gate that journal entries cannot become current
   authorities.
3. **Create canonical corpus/status schemas and generators.** Establish exact denominators before
   deleting hand-maintained numbers. Keep old/new shadow comparison until they agree by identity,
   not merely total.
4. **Consolidate gate assertions and mutation registration.** Migrate one gate family at a time;
   keep behavior and deliberate failure controls green before deleting local helpers.
5. **Finish R-0004 scoped identity migration.** `PackageIdentity` and `DefinitionIdentity` become
   mandatory at all evidence boundaries; correspondence reaches its honest closure; roots and
   receipts become authoritative only afterward.
6. **Perform the Evidence module split with R-0114–R-0118.** Move stabilized identity/subject/
   dependency/receipt/policy objects once, while typed ProgramFacts/pass boundaries remove the
   remaining reverse dependencies. No path-only compiler reshuffle beforehand.
7. **Promote TermIR into VIR when the codec/evaluator/certificate work begins.** Preserve one
   proposition authority; do not duplicate semantics under a new name.
8. **Build the standalone verifier from canonical artifacts.** It checks envelope/accounting and
   replayable certificates without importing the compiler; it never claims to verify source-to-VIR
   correspondence.
9. **Shrink historical roadmap prose only after replacement authorities exist.** Move dated
   narratives to the journal, quantitative snapshots to generated artifacts, and completed work to
   the changelog. The roadmap retains the capability map, execution order, acceptance conditions,
   blockers and qualitative authority states.

Each step is independently green and reviewable. Do not combine semantic compiler edits with broad
path moves. Do not delete an old status/count producer until its replacement is denominator-complete
and mutation-tested.

#### Explicit non-goals

- no second repository or monorepo split—the compiler, proofs, corpus and gates deliberately
  cross-validate one another;
- no deep decorative taxonomies beyond the named ownership boundaries;
- no grand renaming campaign and no path churn without a semantic owner;
- no relocation of `research/`, `paper/`, `site/`, `std/`, examples or tests merely for symmetry;
- no machine-generated replacement for decisions that require qualitative judgment;
- no journal entry, generated Markdown page, changelog entry or research note as a competing source
  of current truth;
- no certificate/verifier claim that closes source-to-VIR correspondence—the producer-side semantic
  bridge remains separately measured and explicitly weaker;
- no Evidence/Report cycle, no report-side fact reconstruction and no raw string digest crossing an
  evidence module boundary where a typed digest domain exists.

#### Completion gates

R-0481 is complete only when all of the following are true:

1. the directory layout above exists for every component whose implementation phase has begun, and
   deferred directories remain named targets rather than empty scaffolding;
2. every maintained doc has lifecycle/owner metadata, every adopted research note names its
   normative owner, and every journal entry is dated and linked forward;
3. local-link, owner, contradiction and journal-authority gates have non-vacuous negative controls;
4. canonical corpus/status/mutation artifacts reproduce across checkout paths and reject missing,
   duplicate, partial, unknown and failed-producer inputs;
5. every hand-copied canonical denominator has been replaced by one generated owner or explicitly
   marked historical with its commit/toolchain;
6. shared assertion helpers replace private copies and their vacuity/self-test suite is green;
7. the mutation registry answers coverage by invariant and production consumer, with no
   registered-but-unexecuted mutation counted as killed;
8. import gates prove `Evidence !-> Report`, `VIR !-> Frontend/Report`, and the standalone verifier
   imports neither compiler nor report implementation;
9. reports are projections: removing any report-side reconstruction cannot change authoritative
   evidence, while mutating the owning evidence object changes every projection consistently;
10. Proof/Evidence relocation is behavior-preserving and happens after R-0004 identity closure;
11. TermIR/VIR has one semantics, one canonical codec and one proposition identity;
12. ROADMAP contains no duplicated quantitative status board, while retaining the qualitative
    state needed to understand blockers and authority transitions;
13. build, docs drift, local links, roadmap linearity, import direction, corpus/status generation,
    gate vacuity and mutation-registry gates all pass from a clean checkout.

**The governing rule:** organize by semantic subject, represent temporal authority explicitly, and
make every structural boundary executable. A cleaner tree without unique fact ownership and gates
is not completion; it is future archaeology with better folder names.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0447** (merged): duplicate of R-0481: merge unique operational controls; retire conflicting directory blueprint. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0447)
- **R-0478** (merged): duplicate of R-0481 / R-0474: merge each half into its actual owner. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0478)

### Task R-0482

**Phase:** 1, 2, 3, 4, 5, 6, 7

**Reconciliation (2026-10-19):** Keep P1 qualification; split later increments among R-0495, R-0473/74 and R-0440.

**Identity separation, model attestations, exported interfaces, and proof-aware package linking.**

**Scheduling:** R-0004's prerequisite is satisfied. Production work starts after the post-closure
91-family qualification checkpoint and R-0208's toolchain/revocation fire drill. This task is
deliberately separate so R-0004 remains finite rather than becoming the entire future verification
language.

**Current entrance state:** `IDENTITY-FREEZE` (11/0) pins the pre-migration populations and
`IDENTITY-OVERBINDING` (6/0) reproduces the fail-safe defect: an unused std-content change moves 7
of 21 package identities. Those count gates are entrance evidence, not a migration map. Before any
identity bytes change, freeze the canonical full rows so a rewrite cannot preserve every `sort -u`
count while silently changing associations. **Rows frozen (2026-10-13):** `IDENTITY-ROWS` (19/0) pins 323 canonical rows
(`docs/verification/identity-rows-pre-r0482.tsv`, from `scripts/tools/identity_rows.sh`) with package
digests replaced by their producing fixture, plus the raw baseline with every digest byte
(`docs/verification/identity-raw-baseline-pre-r0482.tsv`, tied to `61b2a261`) that the migration map
is written against; a model swap that keeps every count moves the rows, and an unused std edit moves
none. **Definitions RATIFIED 2026-10-13** (D1–D7 with amendments):
`docs/verification/IDENTITY_SEPARATION.md` — tagged scope origins (declared / undeclared / standalone /
reserved toolchain), version excluded from scope, content-addressed standalone scope, std as its own
scope, ambiguous undeclared same-named packages refused at existing boundaries, and context changes
distinguished from root changes. **Migration landed** (plan `docs/verification/IDENTITY_MIGRATION_PLAN.md`,
P1–P4 accepted 2026-10-13; see the changelog): the three identities below replace `PackageIdentity`,
the old-to-new map is total (323/323, 0 shared, 0 refused) and the normalized freeze is unchanged;
`IDENTITY-SEPARATION` holds the migration's controls and the over-binding and rows gates assert their
positive forms. **Remaining from the migration (P1):** claims whose closure reaches std or another
package stay fail-closed (`unscopedEdge`); admitting such nodes into report roots is its own slice,
with a claim that calls std and the reachable-std and cross-package sensitivity controls.

#### Five identities that must not substitute for one another

- **`PackageScopeIdentity`** names declared publisher/origin, package namespace/name and the
  manifestless-scope rule. It does not bind dependency names or module content.
- **`PackageArtifactIdentity`** binds the package's own canonical `(module identity, content digest)`
  pairs. Pairing is mandatory: independently sorted names and contents lose the association.
- **`ResolutionContextIdentity`** binds the resolved dependency graph, toolchain and other ambient
  inputs that can change how the same package artifact is interpreted.
- **`DefinitionIdentity`** binds package scope, module, declaration and implementation. Legitimate
  many-to-one attestation rows mean package-level injectivity is not the migration criterion;
  collisions and ambiguity are decided at the complete definition/row key.
- **Claim dependency root** binds only proof-reachable contract/body/trusted/missing dependencies.
  It is what must move when a reachable dependency changes; package scope must not be used as a
  substitute.

The old-to-new migration map is typed and row-complete. It proves total accounting, records
many-to-one cases and named refusals, and bootstraps generated symbols, attestations, receipts and
all production consumers atomically. Two controls graduate together: editing an unused std module
does not remap unrelated scope/attestations, while editing a reachable definition invalidates the
affected dependency roots. Either control alone permits a vacuous or over-bound implementation.

**Objective:** make the relationship among a mathematical model, an exact implementation, an
exported behavioral interface, a proved proposition and an accepting policy explicit and
machine-checkable. The authoring promise is:

> Proof authors define or select a mathematical model, select an opaque generated implementation
> reference, state the exact relation and claim scope, and provide correspondence evidence. The
> compiler recomputes implementation identity, validates the attachment, keeps contract/model/body
> dependencies separate, and refuses every missing, stale, ambiguous, downgraded or cross-package
> association.

#### Two arrows, neither substituting for the other

```text
PFnDef / ModelIdentity
       |  model-correspondence evidence
       v
DefinitionIdentity / exact source implementation

PFnDef / canonical proposition
       |  certificate or kernel/solver proof
       v
claimed logical property
```

The generated implementation reference identifies the endpoint of the first arrow. It does not
prove the arrow. A kernel-valid theorem over a model proves the second arrow only. Reports,
receipts and policy retain both results independently.

#### Required typed objects

**`ModelIdentity`:** identity of the mathematical semantics being used, distinct from both the
implementation and the theorem proposition. It binds model schema, canonical model digest,
parameterization, logical dependencies and model codec/toolchain identity. A proof-script-only
change does not move it; a semantic model change does.

**`ImplementationReference`:** opaque generated selector for an exact `DefinitionIdentity`:
package, module, declaration and authoritative implementation digest. Its constructor is private.
Authors select a generated typed symbol; they never transcribe its components. Compiler
recomputation is mandatory and a mismatch refuses.

**`ModelAttestation`:** the complete attachment, containing implementation reference, model
identity, relation kind, claim scope, provenance and correspondence disposition. Required relation
kinds are at least:

```text
implements
refines
equivalent
abstracts
contract_only
```

These are not aliases. A one-direction refinement cannot be rendered or consumed as equivalence.
Trust is an evidence mode/disposition, not a semantic relation kind; a trusted model remains an
assumption even when the implementation reference is exact.

**Exact property acceptance (2026-10-18; planned):** bug 079's body/spec binding
does not establish that the theorem states the annotated `#[proof_coverage]`
(`iff`, `one_direction`, etc.). For every
supported claim family, construct the expected typed proposition from the selected
subject/model, parameters, domain, relation and scope, then check the attached
theorem's type against it under the admitted normalization rules. A theorem about
the right body is insufficient if it proves a different property. An arbitrary
theorem that cannot be matched remains explicitly unclassified/unsupported for
that coverage; its author's annotation cannot grant a stronger verdict.

Retain a sorting example whose properties are distinct: preserving length,
producing sorted output, and preserving the input elements. A length-only theorem
must never authorize either of the latter claims. Gate a matching theorem, a wrong
property over the same body, a narrower input domain, and one-direction evidence
relabeled as equivalence. Drive those controls through status, receipt issuance,
receipt consumption and independent replay as each consumer graduates. Bind the
checked proposition, relation, domain, assumptions and exact implementation/model
and dependency identities in the existing canonical evidence objects. Tests,
assumptions and kernel proofs retain their separate dispositions; proposition
matching does not establish model-to-machine correspondence or that the chosen
specification captures the user's intent. No new coverage label becomes
authoritative before its matching rule and rejecting controls exist.
Until this lands, `#[ensures]` obligations have no discharge path at all: bug 089
found every report printing `proved_by_lean` from an `#[ensures_proof]` link or an
`iff` label alone, and `Report.ensuresVerdictOf` now reports every linked
postcondition `unproven`. The matcher plugs in there and nowhere else.

**Total-model admission is a semantic prerequisite, not merely sequencing.** A model used as
logical contract semantics may carry a current `implements`, `refines`, `equivalent`, or
`contract_only` attestation only when it is effect-free, terminating, free of unhandled traps, and
has a complete logical interpretation over the claimed domain. Recursive models require accepted
`#[decreases]`/well-foundedness evidence. Missing or invalid totality evidence leaves correspondence
`missing`/`unsupported`, or explicitly `assumed` through the trusted surface. Exact selection,
kernel-valid proof over the model, or finite differential agreement cannot manufacture totality.

**`ExportedContract`:** the package/caller interface, with a distinct `ContractIdentity`, covering:

```text
functional behavior
effects and capabilities
reads set
modifies/frame set
failure behavior
termination disposition
resource contract
assumptions and trusted boundaries
```

Every component records whether it is authored, inferred-exact, inferred-conservative,
runtime-checked, assumed or missing. Conservative inference is safe but weaker and must never
render identically to an exact or proved fact.

#### Attachment cardinality

Concrete deliberately does not impose F*'s one-interface/one-implementation restriction. One
implementation may have several legitimate claim scopes; one model may be reused across several
implementations; platform implementations may share one contract; and several proof modes may be
possible. The authoritative key is:

```text
(DefinitionIdentity, ModelIdentity, ClaimScope)
```

Relation kind and evidence mode are payload under a coherence rule, not key fields. Otherwise an
`equivalent` and `refines` row—or trusted and proved rows—could silently coexist as two answers for
one attachment. Zero matches is missing; more than one active row for the key is ambiguous—even if
byte-identical—and refuses. A stronger relation explicitly supersedes or retires the weaker active
row; it does not shadow it. The relation algebra records only valid entailments (at minimum,
`equivalent` entails its directional refinements, and a suitable refinement may satisfy a selected
`contract_only` claim). `implements`/`abstracts` direction and contract satisfaction remain explicit
rather than being forced into an unsound total order. Evidence-mode downgrade is trust
widening/laundering and requires an explicit transition, never coexistence.

#### Authoring surface and evidence modes

The intended source/proof distinction is explicit at authorship rather than reconstructed by a
report:

```text
certificate_by      independently checkable certificate
proof_object_by     retained/replayable proof object
trusted_proof_by    explicit trusted proof attachment
assume              explicit assumption
```

Exact syntax is a language-design decision, but these modes remain distinct types/dispositions.
No general `proof_by` fallback may silently choose among them. Generated reference use is
conceptually:

```lean
def model : PFnDef := { ... }
def table : FnTable :=
  FnTable.ofAttested [AttestedPFnDef.of model GeneratedImplementations.package_module_fn]
```

Raw package names, paths, digests and display strings are not author inputs. Failure to generate or
resolve the symbol is a typed refusal, never a placeholder authors repair by copying hex.

#### Correspondence dispositions

Model correspondence is independently retained as:

```text
proved
checked
assumed
missing
stale
ambiguous
unsupported
```

Attaching a model creates a named correspondence obligation whose default state is `missing`; it
does not discharge one and silence is never `assumed`. Following Why3's safe default, abstract
assumptions become obligations unless the author explicitly uses the assumption/trusted surface.
Runtime checks or finite exhaustive validation may earn `checked` only under their declared domain.
Signature agreement, identity agreement and proof validity do not earn `proved`.

**Near-term evidence path:** reuse the existing differential-lowering/reference-evaluator machinery
to compare the PFnDef model with implementation semantics. Exhaustive comparison earns `checked`
only over a declared finite domain; sampled comparison over infinite domains records its sample
strategy, seed/domain and translation-assurance strength and remains weaker. This is correspondence
evidence, not a kernel proof, and producer/shared-evaluator trust remains visible. A mismatch emits a
counterexample and refuses current correspondence. This turns the new axis into evidence rather
than bookkeeping without pretending sampling proves universal equivalence.

#### Three dependency strengths

Replace the eventual two-way contract/body simplification with three exact dependency kinds:

```text
contract edge        depends on ContractIdentity and exported evidence
model edge           depends on ModelIdentity and correspondence evidence
implementation edge  depends on exact DefinitionIdentity/body semantics
```

`trusted` and non-current `missing`/`unclassified` remain explicit. A contract-preserving body
change does not stale contract callers. It does stale implementation-edge callers. A model change
stales model-edge callers even when the executable body is unchanged. Edge kind is derived from
what the theorem/consumer actually uses, never selected as a freshness optimization flag.

#### Proof-aware package linking is partial

Define package evidence composition as a checked partial operation:

```text
linkEvidence : EvidenceBundle -> EvidenceBundle -> Except LinkRefusal EvidenceBundle
```

The linker refuses at least:

- duplicate or colliding definition identities;
- incompatible exported contracts or claim scopes;
- schema/codec/rule-set incompatibility;
- trust widening or assumption deletion/conflict;
- model/implementation substitution;
- ambiguous attestations;
- missing, stale or incomplete dependency closure;
- contract evidence weaker than the consumer policy requires.

Composition is not list concatenation followed by hashing. It preserves identity uniqueness,
propagates trust/assumptions monotonically, recomputes dependency closure/root, and records which
contract/model/implementation relation justified each imported claim. The package manager rejects
an upgrade that weakens a required contract even if ABI and names are unchanged.

#### Receipt and artifact bindings

Extend the canonical evidence schema and receipt with independently typed fields for:

```text
implementationReference
modelIdentity
relationKind
claimScope
correspondenceEvidence
contractIdentity
propositionIdentity
policyIdentity / rule-set identity
```

Receipts continue to bind theorem artifact, tables, environment/toolchain, dependency root, trust
and assumptions. A valid certificate copied to another implementation, model, proposition,
contract or policy refuses. Advisory/revocation state remains a validation-time registry input,
not a retroactively mutable receipt field.

#### Required adversarial matrix

Every item below has a positive control and a production-consumer mutation; a synthetic validator
probe alone is insufficient:

1. correct implementation, wrong model;
2. correct model, wrong implementation;
3. same callable/module name in another package, and the same declared package name/version from a
   different registry, repository, source namespace or origin identity;
4. same body with changed signature, capability, generic or contract facts;
5. same implementation with narrower claim scope;
6. refinement relabeled as equivalence;
7. trusted attachment relabeled as proved;
8. model changed while proof artifact remains fixed;
9. proof artifact changed while proposition/model remains fixed;
10. contract preserved while body changes—contract callers remain current;
11. model changed while contract remains equivalent—model callers stale;
12. one implementation with two legitimate claim scopes—both remain representable;
13. duplicate attachment for the same full key—ambiguous refusal;
14. package link with conflicting exported contracts;
15. certificate/receipt copied under another policy or rule set;
16. forged or manually constructed implementation reference—construction failure;
17. inferred-conservative dependency/effect presented as exact;
18. vacuous or empty exported contract receiving a strong proved presentation;
19. package scope removed from identity;
20. implementation scope removed from identity;
21. stale generated reference with a fresh proof;
22. model/body correspondence removed while logical proof remains valid;
23. implementation/model/contract identities swapped independently;
24. duplicate, surplus or unmatched package-link evidence discarded by filtering.
25. termination, trap-freedom or logical-interpretation evidence removed from a contract-level
    model attestation—the attestation and every dependent contract claim refuse or downgrade.

#### Documentation and reporting requirements

The normative object model lives in `docs/verification/EVIDENCE_ARCHITECTURE.md`; status vocabulary
in `VERIFICATION_STATUS.md`; author meaning in `PROOF_CONTRACT.md`; author procedure in
`PROOF_WORKFLOW.md`; hostile cases in `THREAT_MODEL.md`; product invariant in
`VERIFICATION_CHARTER.md`; proposition boundary in `VERIFICATION_IR.md`; shipped limitations in
`CLAIMS_TODAY.md`; theorem categories in `PROOF_THEOREM_SHAPES.md`; feature admission in
`docs/project/DESIGN_POLICY.md`. No roadmap-only architecture counts as implemented or normative.

Reports expose at least these independent dimensions:

```text
implementation_selection
model_correspondence
logical_validity
dependency_closure
policy_acceptance
```

No single `proved`/green value may erase a weaker dimension. A kernel CVE can degrade logical
validity without rewriting selection/correspondence/closure; a source change can stale selection or
correspondence without claiming the old theorem became logically false.

#### Sequencing and exit gates

1. finish R-0004 exact implementation-reference migration and reach identity-safe 10/10;
2. make correspondence completeness mandatory for root usability and connect roots to verdicts;
3. finish the total contract-callable model fragment; an attested model that may diverge, trap, or
   invoke trusted execution cannot serve as logical contract semantics without an explicit weaker
   disposition;
4. introduce `ModelIdentity` with canonical sensitivity/invariance gates;
5. introduce `ModelAttestation`, explicit relation algebra, named default-missing correspondence
   obligations and differential checked correspondence;
6. generate author-facing typed references and remove raw attachment inputs;
7. complete R-0473/R-0474 typed contract records, identity-safe substitution, `old`, and frame/
   `modifies`; `ExportedContract` cannot honestly precede that substrate;
8. introduce `ExportedContract`/`ContractIdentity` with functional/effect/frame/failure/termination/
   resource components and provenance;
9. migrate dependency material to contract/model/implementation kinds;
10. implement partial package-evidence linking and behavioral compatibility checks;
11. extend canonical artifacts, receipts and standalone verifier;
12. run and mutation-kill the complete adversarial matrix;
13. publish a multi-implementation package whose body can change without staling contract-only
    consumers, while model/body consumers stale exactly as specified.

**Done when:** all identities are typed and domain-separated; authors write no raw identity/digest;
every attachment states relation/scope/mode; correspondence and logical validity are independently
replayable; exported contracts compose across packages; the partial linker refuses every unmatched
or weakened case; dependency strength produces the intended freshness behavior; receipts bind
code/model/proposition/contract/policy separately; and all twenty-five attacks are killed by the
production path without collapsing evidence into one status.

### Task R-0479

**Phase:** 1

**Reconciliation (2026-10-19):** Merge evidence-surface cases into R-0494; retain broader diagnostic audit.

**Scheduling: opportunistic.** Nothing blocks on it, and each instance is a one-line change. The
survey is the work, not the fix.

**Objective:** make "this was not computed here" unrepresentable as a valid fact value.

**The defect, from a real failure.** `shadowEdgeKinds` reported edge kinds as

```
if e.fn.isTrusted then "trusted" else "unclassified"
```

having consulted no classification table. `unclassified` is a LEGITIMATE `DependencyEdge`
constructor, so the output was indistinguishable from a real classification of an unclassified
edge. That is why it did not read as a bug: it read as a finding. On 2026-08-13 it caused a
coverage fail-open to be reported that did not exist, with a prediction that root coverage would
fall 62/64 → ~53/64, committed and then retracted. Had the stub returned `NOT-CLASSIFIED-HERE`, the
staleness would have been visible on the first line of output.

**Why this is not the same task as one-producer-per-fact.** `check_one_producer.sh` (landed
2026-08-13) would now catch a second producer of edge kinds. It would NOT catch a producer that
computes the right fact from too little information and reports a plausible default — that is a
separate failure mode, and the more deceptive of the two, because a wrong value that is in-domain
survives every type check and every set-comparison gate.

**The rule:** a function that may not have looked returns a value OUTSIDE the fact's domain —
`ABSENT`, `NOT-COMPUTED-HERE`, `Option`, a distinct constructor — never a domain value that also
means something specific. This is already the discipline for
`ProofEvidenceReceipt` (private constructor, no `Inhabited`), `ImplementationManifest` (private
`ofRows`), and `ManifestRefusal` (named reasons rather than a smaller count). Diagnostics were
exempt because they "only report", which is exactly the assumption that made this one costly.

**The rule continues through every projection.** Replacing an in-domain default with an honest
marker is not complete if a later string catch-all maps that marker to success. The typed result,
rendered text, JSON/artifact disposition and process exit code must preserve the same distinction.
Unknown or newly added refusal constructors default to non-success; success is enumerated rather
than supplied by `_ => ok`. Report gates must likewise validate the producer's exit status before
interpreting its stdout, so a failed command cannot contribute apparently valid evidence.

**PROGRESS 2026-08-13 — the `.getD` half is DONE and ratcheted; the `| _ =>` half is MEASURED.**

Five sites fixed, all of them a failed read rendering as a real value of the fact:

- `Report.lean:4203`, `Main.lean:1559`, `Main.lean:1722` — a failed obligation lookup defaulting to
  `"missing"`, which is a genuine canonical status (`.notProved => "missing"`), so "no obligation
  record" was indistinguishable from "an obligation exists and is missing". Now
  `no_obligation_record`.
- `Report.lean` capability-origin, two sites — `| _ => "transitive"` served as BOTH the real fourth
  origin and the fallback for an absent `origin` key or a malformed trace, so an unreadable origin
  became a positive claim that the capability arrived transitively. `transitive` is now matched
  explicitly and the fallback is `origin_unavailable`.

Two of the three obligation sites were found BY THE RATCHET after I believed the sweep was
complete, which is the argument for the ratchet over a one-time survey.

**Ratcheted:** `check_one_producer.sh` refuses any `.getD` default drawn from a closed vocabulary of
12 (canonical statuses, dependency-edge kinds), reading code rather than prose via
`lib/code_refs.sh`, with a non-vacuity probe on the same path the assertion uses.

**REMAINING, and smaller than expected.** Measured: of 36 `| _ =>` branches returning a string
literal across `Concrete/` and `Main.lean`, **zero** return a value from the status or edge-kind
vocabularies — that half was already clean for the closed sets the ratchet knows. What remains is
the vocabularies the ratchet does NOT yet know: the 36 include `"source"`, `"self"`,
`"missing_proof_link"` and 15 bare `""`. Each needs the same question asked — is this literal a
meaningful value of the fact being reported? — and the ratchet's vocabulary extended per closed set
found. `""` is the ambiguous case and deserves a decision: it is out-of-domain for a digest but
reads as "empty" rather than "not computed".

**NOT a violation, and written into the gate so it is not "fixed" later:**
`if dep.isEmpty then ("none", "missing", "")` is a COMPUTED determination that the obligation is
missing, not a failed lookup papered over. The rule targets the failed-read shape.

**RULE REFINED 2026-08-13, after `docs/verification/EVIDENCE_ARCHITECTURE.md` landed.** The test is NOT "is the
default a member of the vocabulary". It is **"does the default ASSERT something the failed read did
not establish"**. Those come apart at the bottom of a ladder:

- `transitive` has positive content — it claims the capability arrived by a particular route — so a
  parse failure rendering as `transitive` fabricates provenance. Violation.
- obligation `missing` describes an EXISTING record whose proof is absent, so using it for "no
  record at all" conflates two different facts. Violation.
- `SourceCorrespondence.missing` means exactly "no correspondence evidence established" — the ⊥ of
  its ladder. A failed read defaulting to it asserts nothing and is fail-CLOSED. **Legitimate.**

**Consequence for the ratchet, and it is over-strict as written.** `check_one_producer.sh` blacklists
`.getD "missing"` wholesale, so it would reject a correct `SourceCorrespondence` default once the
typed-evidence objects are implemented. When that happens the fix is an explicit allowlist entry
naming the type and stating why its ⊥ is a safe default — NOT deleting the check, and NOT renaming
the value to evade it. A ladder's ⊥ is the one value a failed read may legitimately produce, and it
must be justified per type rather than assumed per name, since `missing` means different things in
the two vocabularies that already use it.

**Scope:** survey `Concrete/Report/Report.lean` for defaults standing in for "did not look".
Known-good examples to preserve, since they already follow the rule: `ABSENT (no structural body
threaded)`, `NO-SUBJECT`, `UNPINNED`, `none (no call-graph entry)`. The suspects are any branch
returning a domain value on a `none` or a fallback.

**Acceptance:** each converted site returns an out-of-domain marker; at least one gate asserts that
a report line for an uncomputed fact does NOT render as a valid value; and the `| _ =>` /
`.getD <domain value>` pattern is counted so the number cannot silently rise.

### Task R-0478

**Phase:** 1, 6

**Disposition (2026-10-19):** duplicate of R-0481 / R-0474: merge each half into its actual owner.

**Objective:** two refactors that are cheap individually and keep producing defects because they
are not done — recorded together because both are "the same thing exists in N places and nothing
holds them together".

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0478).

### Task R-0477

**Phase:** 2, 3, 4

**Reconciliation (2026-10-19):** Keep disposition task; retarget useful calculus work to R-0460/Core.

**Objective:** bring `vcgen/calculus` back onto current `main`, or retire it — it is tracked
nowhere in this file, which is how a branch quietly becomes a fork.

Four commits sit off `main`, based at `b97dfdf0`, which `main` has since passed by 80 commits:

* `66c56883` enroll obligation discovery in constructor coverage, with its measured limit;
* `d17be8a1` per-walker constructor coverage — found an eighth discovery defect immediately;
* `43e15a6c` obligations inside `assert`/`assume` were lost entirely, plus a false green made
  en route;
* `eaa3cf9b` split the VC calculus onto its own branch, off `main`, with no language change.

**Its earlier differential result must not be assumed to still hold.** That run reported the
calculus agreeing with the walkers on 41,807 obligations across 902 files with zero
disagreements — measured against a tree that predates contract validation, the source-order
language decision, and the evidence merges. Re-run the structured differential and the
merged-tree gates before treating that number as current.

**The hand-written walkers remain authoritative.** The differential has caught 3 defects in the
calculus and 0 in the walkers, which is exactly why the walkers are the oracle rather than the
thing being replaced. That does not change until a family graduates with its own negative
controls and a Register A row.

Sequencing: AFTER R-0473/R-0474, not before. Rebasing onto a foundation that is about to change
means rebasing twice, and the calculus is the natural consumer of resolved typed contract records
rather than of the AST it reads today.

### Task R-0476

**Phase:** 1

**Reconciliation (2026-10-19):** Merge gate-health checkpoint into R-0513; retain standalone freshness guards.

**Objective:** drain two ratchets to zero, so neither becomes a permanent debt marker that
everyone learns to read past.

**23 universal assertions have no non-emptiness witness.** `all(P(x) for x in coll)` is true when
`coll` is empty, so each passes when its collection is empty — proving nothing while reporting a
pass. `check_gate_vacuity.sh` counts them and fails if the count RISES, which stops the population
growing but drains none of it. A ratchet with no drain task is a debt marker, and debt markers
stop being read.

The fix per assertion is small (a `len(...) > 0` conjunct, or an explicit witness), so this is
volume rather than difficulty. Lower `BASELINE_UNIVERSAL` with each batch; the gate prints a NOTE
whenever the count drops below the baseline, so the ratchet tightens as a side effect of the work
rather than needing a separate step.

This matters more than its size suggests. The class is not hypothetical: a positive control in
`check_contract_negatives.sh` went green while proving nothing, because the fixture it inspected
was rejected wholesale and its report was empty. Two absence helpers had the same defect. Each of
these 23 is a place the same thing can happen silently.

**A third shape was added 2026-08-08, after an audit found a live instance**: unintended command
substitution inside a diagnostic string. `check_transform_register.sh` had backticks around
`partial` in an `echo`, so every run executed it, printed `partial: command not found`, and still
reported PASS=80 FAIL=0 — these scripts do not use `set -e`, so nothing counted the failure. The
vacuity shapes could not see it: the ASSERTIONS genuinely passed, and what was unclean was the
HARNESS. A harness that runs stray commands can also swallow the output an assertion depends on.
Six gates put backticks in diagnostic strings; five escape them, so this was the only live one.

**Shapes the gate STILL does not count**, listed so their absence is not read as their
non-existence: `sed`/`awk` block extraction over a missing block; `grep` pipelines accepting empty
input; command substitutions hiding a failed producer; section checks where a fatal earlier
diagnostic removes the section. `assert_block_absent` distinguishes a missing anchor from
forbidden content but not a malformed or unterminated block — left deliberately, because no report
shape was found that reproduces it, and an untested branch inside an assertion helper is worse
than a known gap.

**Centralise the freshness build.** `require_fresh_binary` runs `lake build` at the top of each of
145 gates. A no-op build is ~130ms, so a full suite pays roughly 19 seconds of redundant builds.
Correct but wasteful: the runner should build ONCE and hand the binary to N gates, with each gate
KEEPING its guard for standalone execution. Deleting the per-gate guard would remove the
protection exactly where it earned its place — running one gate directly while iterating is how
four stale-binary incidents happened in a single session.

### Task R-0473

**Phase:** 6

**Reconciliation (2026-10-19):** Keep; consume Resolve-owned identities; archive unrelated R-0004 history.

**typed contract records, and contract type checking that earns the name**

**Objective:** Replace the seven consumers' independent re-reading of raw `List Expr` contract
metadata with one checked, typed contract record, and close the H27 gaps that make the current
validation name-and-sort only.

Contracts were erased metadata that no pass validated. `#[requires(nosuchvar > 0)]` was accepted
and `nosuchvar` reached the HYPOTHESES of every obligation in the function, including ones
reported `proved_by_kernel_decision`. That failed closed — a hypothesis over a fresh universally
quantified variable is unprovable at the call site — but the failure surfaced in a *different*
function than the typo, and a precondition nobody can establish read exactly like one nobody had
established yet.

Name-and-sort validation now runs (`Concrete/Check/Check.lean`) and is deliberately not called
type checking. H27 records what remains accepted, each measured against the compiler and each
pinned by a gate assertion written to flip when the gap closes:

* ~~a boolean `#[variant]`~~ — **CLOSED 2026-08-07**. It was accepted because the proposition
  check *skipped* variants rather than holding them to their own rule; absence of a rule read as
  absence of a requirement. `i < n` does not decrease, it flips;
* ~~a contract calling an IMPURE (capability-requiring) function~~ — **CLOSED 2026-08-07**. Its
  meaning would depend on runtime effects, so it is not a proposition. Pure executable calls stay
  legal — purity is the property that matters, not spec-only;
* operand type compatibility — `#[requires(x < 9999999999)]` on an `i32`;
* a bool as an arithmetic operand — `#[requires((x > 0) + 1 > 0)]`;
* `result`'s type (its scope is enforced; its type is not);
* precise loop-binder scope — the bound set is every name bound anywhere in the function, so a
  name bound only *after* the loop is admitted and reaches the VC.

**Language boundary added:** purity alone is not the final contract-callable
criterion. Define `spec fn`, checked `total fn`, ordinary runtime functions,
and trusted functions. A `total fn` enters contracts only after effect freedom,
termination, trap obligations, logical interpretation, and partial-operation
preconditions close. Until that type exists, executable calls accepted by the
current narrow checker must not be documented as a general total fragment.
Coordinate with `#[decreases]`, recursion/loop total correctness, `old`, frame/
`modifies`, and the spec library rather than adding another call-name filter.

**Compiler measurement boundary:** declaration-level isolation must report
`valid`, `locally_invalid`, `blocked_by_invalid_dependency`,
`parse_unreachable`, `timed_out`, and `unmeasured`. Continue through independent
declarations using poisoned summaries; do not accumulate speculative cascades.
The current first-error population remains a named denominator until this
lands.

**The four that remain share one root cause, and it is the argument for doing this properly.**
Each needs the per-function environment: widths and operand domains need the types of locals,
`result`'s type needs the signature at the annotation site, and scope needs the bindings live at
the loop rather than "bound somewhere in the function". The two that closed did not need it, which
is exactly why they closed cheaply — and why the remaining four should not be attacked the same
way. Faking an environment in this module-level pass would be the same shape of mistake as the
walkers keeping their own weaker copy of the trap rules (R-0464): a second, weaker answer to a
question that already has an authoritative one.

The R-0004 Slice-6 record that accumulated here — migration-manifest closure (44/44), dependency-root
integration steps 1–8, the Slice-6 acceptance boundary and the three-digest boundary — closed with R-0004
and is archived in [ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md#r-0473-the-embedded-r-0004-slice-6-record-2026-08-07--2026-09-02). Its one live remainder is this task's and R-0474's:
contract dependency edges are witnessed conservatively today; exact contract witnesses replace them
(queue row 12).

**Acceptance boundary, from review and worth restating because it is easy to satisfy the letter
of this task and miss it:** the record must retain resolved identities and types. Rejecting the
six fixtures while still storing the original name-based AST expression would let VC generation
reconstruct exactly the unsafe information the record exists to eliminate — and R-0474 would
inherit the problem intact.

**Dependency correction (2026-08-07).** The plan as first written was circular: this task's record
"must carry resolved identities", while binding identities were assigned to R-0474. A record
cannot honestly retain identities that do not exist yet, and storing names now with a promise to
swap them later is exactly the failure the boundary above forbids — the names would be in the
record, and every consumer built against it.

So **the identity substrate begins here**, even though capture-safe substitution stays in R-0474:

1. introduce the internal lexical `BindingId`;
2. assign it to parameters, locals, ghost bindings, loop variables, and `result`;
3. resolve contract identifiers to a `BindingId` or a `CallableId`;
4. build `CheckedContract` over resolved, typed expressions;
5. *(R-0474)* implement identity-based substitution;
6. *(R-0474)* delete name-based substitution once the evaluation-law gate passes.

The resolved reference must be a SUM, not a string with a companion type table — a string field
is how the name survives in practice:

```
ResolvedContractRef
  = local     BindingId
  | parameter BindingId
  | ghost     BindingId
  | result    BindingId
  | constant  ConstId
  | spec      CallableId
```

`BindingId` here stays the *internal lexical* identity from `docs/language/BINDING_IDENTITY_DESIGN.md` —
assigned during elaboration, regenerated every build, never serialized. It must not be conflated
with stable evidence identity, which has to survive harmless source edits.

**Boundary caveat on the closed gap 6.** It rejects capability-requiring calls, which is narrower
than purity or totality: a capability-free function that traps, diverges, or is `#[trusted]` is
still admitted (all three measured). The rule that would be right — "resolves to a spec fn, or to
an executable function with an explicit, checked logical interpretation" — needs that second
notion to exist, and it does not. It belongs with the typed record.

**Diagnostic accumulation, narrowly.** The corpus denominator problem does not justify a
compiler-wide error-accumulation refactor before typed contracts. Enough: contract validation
collects all contract diagnostics for a declaration, runs once resolution and type information
exist, and lets one invalid contract not mask its siblings. Runtime-body errors may stay
first-error-wins. Reports must not be emitted from invalid typed records.

**Also in scope:** a diagnostic-accumulation or contract-only analysis mode. Checking stops at the
first error, so for 285 of 1249 corpus files the effect of contract validation is *unmeasured* —
a contract defect can sit behind an unrelated earlier error. Until that mode exists, the
defensible claim is "no unexpected rejection among the 964 files that reached contract checking",
which is materially weaker than "the corpus is compatible".

**Depends on:** nothing. **Blocks:** R-0474.

### Task R-0474

**Phase:** 6

**Reconciliation (2026-10-19):** Keep substitution work; move identity foundation to R-0492.

**lexical binding identities and capture-safe substitution**

**Objective:** Retire by-name substitution as proof infrastructure, and graduate H25 and H27's
shadowing restriction.

`refineObligations` instantiates a spec body by substituting parameters via `substExpr`, which
replaces by NAME with no capture avoidance. A binder in the body has the binding substituted away.
No unsound evidence escaped — the corrupted obligation reports `outside the fragment` because
`renderTerm` has no `.ifExpr` case — and *that is the finding*: soundness rested on an unrelated
downstream renderer's narrowness, and adding `.ifExpr` (an obvious extension the refinement tier
invites) would have made the corruption live with no test failing.

**The invariant:** refinement substitution may act only on expressions whose free-variable
identities are distinguished from all locally bound identities.

**Graduation condition:** substitution over identities, with the evaluation law
`eval(subst(Q, x, e), rho) = eval(Q, rho[x |-> eval(e, rho)])` proved or exhaustively gated over
the supported fragment.

Scope is fixed in `docs/language/BINDING_IDENTITY_DESIGN.md` before any editing, because this refactor
becomes invasive quietly. The load-bearing decision there is to keep **two** notions apart rather
than build one identifier for both: a *lexical binding ID* (internal, unique within one elaborated
function, regenerated every build, never serialized) and a *stable evidence identity* (canonical
across harmless edits, appearing in artifacts and fingerprints). Their requirements contradict,
and a lexical ID reaching stored evidence is a defect — a rebuild would read as a different
program. Only the lexical ID is in scope here. Monomorphization and inlining are explicitly out of
scope, with the point named at which the answer becomes mandatory.

**Currently contained, both temporary and both saying so in their diagnostics:** binder-bearing
spec bodies are rejected, and contracts naming a parameter shadowed by a local are rejected.
Shadowing is the gap unknown-name rejection cannot close by construction — every name resolves, no
diagnostic fires, and nothing records *which* binding the contract means.

**Depends on:** R-0473. **Blocks:** `old(...)`, frame/`modifies` conditions, reliable loop-state
reasoning, call-site contract instantiation, and quantified predicates with binders.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0478** (merged): duplicate of R-0481 / R-0474: merge each half into its actual owner. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0478)

### Task R-0475

**Phase:** 3

**Reconciliation (2026-10-19):** Archive shipped snapshot fix; retain origin-labelled reporting increment.

**decouple fixture snapshots from the stdlib**

**Objective:** Stop a standard-library contract edit from silently invalidating a negative
fixture's golden file, and stop the resulting failure from pointing at the wrong place.

The H26 investigation resolved the immediate drift: `assume_taint` is one of 3 of 21
contract-negative fixtures carrying a `Concrete.toml`, so it compiles as a *project* against the
stdlib, and `a3ba761b` (R-0464) added contracts to `std/src/sha256.con` after the snapshot was
last written. The diff was 94 additions, 0 removals, nothing touching the fixture's own module —
verified before regenerating.

The defect that remains is structural: **a negative fixture's golden file transitively depends on
every contract in the standard library.** The next stdlib contract edit drifts it again, and the
failure presents as a regression in `assume_taint`, which is not where anything changed. The first
attribution in this investigation was wrong for exactly that reason.

Two workable fixes: scope the contracts snapshot to the fixture's own modules, or report "N stdlib
entries changed" as a separate line from fixture content so the next occurrence names the stdlib.
The second is cheaper and keeps stdlib coverage visible.

The 2026-08-07 exposure audit (exactly one project-mode snapshot was coupled) and the shipped fix
(`31710d4e`: the contracts comparison is scoped to modules the fixture defines, and that invariant is
asserted on every run) are archived in [ROADMAP_HISTORY.md](docs/archive/ROADMAP_HISTORY.md#r-0475-exposure-audit-and-shipped-snapshot-fix-2026-08-07).

**What remains, and why it was not done here.** The *compiler-side* improvement is to label the
report's sections by origin — "Source Contracts (this project)" versus "(dependencies)" — so the
structure carries the distinction instead of the test harness reconstructing it. That needs module
origin recorded where modules ENTER THE PROJECT GRAPH — not, as first written here, plumbed
through the parser. Whether a module is project-owned or dependency-owned is project-resolution
knowledge; the parser's job is spans and paths, and teaching it package policy would put a
resolution decision in the wrong pass. The shape is

```
ModuleOrigin { package/project identity; module identity; source path;
               local | dependency | stdlib | generated }
```

classified at project assembly and carried through Core into obligation records. What is missing
today is any distinction at all: `buildFnLocMap` stamps ONE file across all modules, so nothing
downstream can tell a project module from a stdlib one. Worth doing, and deliberately
not bundled into a snapshot fix. Note the design constraint discovered while scoping this: simply
dropping dependency contracts from the report would LOSE safety-relevant information, because an
undischarged obligation inside stdlib code still affects the user's program. The right shape is
separate labelled sections, not a filter.

### Task R-0472

**Phase:** 2, 3, 4, 5, 6; after-1.0/research

**Reconciliation (2026-10-19):** Split into existing Core/VC, totality and research owners; remove obsolete absence claims.

**what could be provable but is not yet, by reason**

Numbered R-0472 because R-0470 and R-0471 were already allocated — the first draft of this
section collided with the existing R-0470 (capability-variable inference), caught by grepping the
heading list rather than by anything failing.

Placed after R-0004 because it is the map of where proving goes next, and grouped by the REASON
each item is blocked — the reason determines the cost far more than the feature does.

#### Trivially close: the obligation kind simply does not exist

```
let x: i8 = -128;  return -x;      // traps at runtime. No obligation generated.
let f: f64 = 1e300; return f as i32;  // traps. No obligation generated.
```

Both trap in the compiled binary, both are expressible in all three kernels today, and nobody
wrote the rule. Under `docs/verification/VC_GENERATOR_DESIGN.md` these are **two table rows**. That the current
four-walker design made them expensive enough to skip is itself an argument for that design.

#### Blocked by one rung, already unblocked

Bitwise reasoning — `(x & y) ^ (~x & z)`, i.e. SHA-256's `Ch`. The model bridge is proved in Lean
against `BitVec 32`, the shared type is confirmed in all three (`Nat`/`N`/`nat`, the last needing
`unbundle bit_operations_syntax`); only the tier is unbuilt. **This is the one that unlocks real
corpus coverage**, because `hmac_sha256`'s meaningful property is exactly this shape.

#### Blocked by one missing rung: enums

Any property mentioning which variant a sum type holds. Needs constructor injectivity and
disjointness emitted per kernel — the same shape as the struct rung already built, and the last
quantifier-free construct missing.

#### Blocked by the recursion ban

```
fn sum(n: i32) -> i32 { if n <= 0 { return 0; } return n + sum(n - 1); }
```

Not provable, not even *eligible* — recursion is banned from the profile. With `#[decreases(n)]`
it becomes provable, and its termination obligation is **linear arithmetic**, which the existing
three-kernel machinery already discharges.

Everything downstream is gated on this: a sort is ordered, a parser terminates, an output is a
permutation of its input. All need induction; induction needs recursion admitted.

#### Blocked by a missing design

- **Loop-carried properties.** Bounds *inside* a loop are proved today; "this sum equals the total
  of `a[0..n]`" is not expressible, because there are no invariants. WP forces this question
  rather than answering it.
- **Heap and pointers** — needs a memory model, separation-logic shaped. Largest single item.
- **Constant-time / non-interference** — "runtime does not depend on `secret`" is a RELATIONAL
  property comparing two executions, and cannot be a per-site obligation at all. Handled today as
  a capability discipline, not a proof. Genuinely different machinery.

#### Never automatic

Termination in general (halting problem — hence author-supplied measures), and **print fidelity**:
that emitted Rocq text *denotes* the intended obligation needs a formal semantics of Rocq's
surface syntax, which is not ours. Validated, never proved.

#### Shape of the list

The nearest wins — `MIN` negation, casts, bitwise, enums — are small and mechanical. The big jump
is `#[decreases]`, because it converts an entire category from "not eligible" to "provable", and
it is cheaper than it sounds since its own obligation is linear arithmetic. The genuinely hard
ones are heap and relational properties, and both need design before code, as the VC generator
did.


### Task R-0466

**Phase:** 3

**Reconciliation (2026-10-19):** Keep under R-0515; do not subordinate human pilots to it.

**Objective:** Measure whether the language is legible to the consumer that
actually writes it — a language model — by gating two numbers in CI: can a model
author correct Concrete from the documentation alone, and can it repair a
rejected program from the diagnostic alone.

The premise, stated because it reorders other tasks: Concrete's code is written
by models, so the ergonomic cost the external-validation trial was designed to
price is largely not charged. A model does not resent `defer x.drop()` or
`with(Console)` — keystrokes are the cheapest thing it spends, and linearity plus
capability headers are close to free on the consumer side. What a model cannot do
is guess. So explicitness is not one virtue among several here; it is the whole
adoption surface, and every rule that lives only in a maintainer's head, and
every rejection that does not name its own repair, is a defect rather than
friction.

Two measurements, both regression-tracked so a change can be shown to move them:

- **Authoring.** Give a model the language reference and stdlib manifest and
  nothing else; ask for N programs against fixed prompts; record the fraction
  that compile and, for each failure, which invariant was violated. The output
  that matters is not the score but the histogram: an invariant that models
  breach repeatedly is one the documentation does not state, and that is a
  reference defect with a named location.
- **Repair.** For each rejected program, feed back ONLY the diagnostic — never
  the fix — and measure rounds to green. One round is the bar. A diagnostic that
  cannot close the loop at all is the strongest possible finding, and bug 064 is
  the existence proof: it names a token unknown while displaying that token among
  the known set, so no repair is derivable from it in any number of rounds.

Seed the corpus from what exists rather than inventing one, and it is already
large enough to start: measured 2026-07-31, `tests/wrong-code/cases/` holds 23
documented WC cases and `tests/invalid_programs/` holds 15 programs, plus the
`docs/bugs/` repros — roughly 38 rejected programs with known intended causes
before anything is written. Report that denominator with both numbers; a repair
rate without its corpus size is the single-badge failure this project refuses
everywhere else.

Why this sits here, ahead of the prover-neutral arc: it is the cheap empirical
check on the language surface, in the same relation to R-0137 that R-0462 has to
R-0460 — an afternoon-scale continuous measurement standing in for an argument
that would otherwise be settled by assertion. It also front-runs R-0137's three
human sessions without replacing them (a diagnostic that defeats a model defeats
a newcomer), and unlike recruiting, it runs per commit. R-0137 owns the
diagnostics bar itself and the measured 12-of-15 empty-hint baseline; this task
owns the harness and the two numbers.

Kill criterion, per the spike-first rule: if authoring failures cluster on
prompt-shape or harness artifacts rather than on identifiable language or
documentation facts, the trial is measuring the harness and should be stopped
rather than tuned.

Definition of done: the harness is deterministic enough to gate (pinned model
version and prompts recorded in the artifact, so a number is comparable across
runs or is explicitly marked incomparable); the repair number is reported per
diagnostic code, not as one average, so a regression names its own code; and a
mutation that blanks a populated `hint` must make the repair number visibly
worse. Report both numbers with the corpus size, and never a single green badge —
same no-erased-dimensions rule as R-0440.

### Task R-0471

**Phase:** 3

**Reconciliation (2026-10-19):** Keep bounded diagnostics increment under R-0137/R-0515.

**Objective:** Close the diagnostic-legibility defects the 2026-07-31 surface
audit reproduced, as the early slice of R-0137's diagnostics bar rather than
behind its flagships-area dashboard.

R-0137 owns the full bar and the measured baseline (12 of 15 rejected programs in
`tests/invalid_programs/` emit an empty `hint`; ~25 `hint :=` sites against 215
`E0NNN` codes). But R-0137 is a flagships-area validation artifact, and these three are
hours of work that R-0466 needs in place to have anything to measure. Pulling
them forward is the point of this task; it does not narrow R-0137.

- **Bug 064 — the rejection that cannot be acted on.** Every bare semantic
  `--query KIND` exits 1 with `unknown query kind 'predictable'` and then lists
  `predictable` first among known kinds. The arity is wrong, not the spelling:
  the bare-form branch validates against `knownFactKinds`
  (`Concrete/Report/Report.lean:4430`) while the message renders both lists
  (`:4383`). Split the two cases, and give `traceability` — currently a fact
  filter AND a separate backend path under one name — either one meaning or two
  names. The gate is class-level: every member of both kind lists, in bare and
  argument form, asserting the message never lists the rejected token as known.
  That catches the next kind added to one list and not the other, which is the
  mechanism that produced this bug.
- **Retired syntax gets a generic parse error.**
  `tests/invalid_programs/missing_variant.con` uses the DELETED `#` enum
  qualifier and gets `E0001 expected ;, got #`, with no statement that the
  separator was retired or that `Option::<i32>::Some` replaces it. This is the
  error a model trained on older material will hit most. The class gate is a
  corpus grep for fixtures exercising retired surface forms — the class size is
  unmeasured, so measure it before fixing instances, and the fixture has also
  drifted from its own name.
- **Populate `hint` where the candidate set is already in hand.** `E0100`
  prints no candidates although the resolver holds the in-scope set; the
  field-not-found and missing-variant codes likewise know the legal set they
  choose from. The field is already wired end to end and reaches
  `--diagnostics-json`, so this is population, not plumbing. Gate that every code
  reachable from the corpus carries a non-empty next action, with a mutation
  blanking one hint.

### Task R-0469

**Phase:** 1

**Reconciliation (2026-10-19):** Keep immediate fail-closed repair; native resource proof remains separate.

**Objective:** Fix bug 065 — make stack unboundedness propagate through the call
graph, and give `--report stack-depth` and `--report recursion` the gate neither
has.

`computeCallDepths` filters recursive callees out of every caller's callee set
(`Concrete/Report/Report.lean:496`), so a `main` calling recursive code reports
`depth: 0  stack: 8 bytes` and the summary prints `Max stack bound: 8 bytes` as
the program maximum. A function is unbounded if it is itself recursive OR reaches
anything unbounded; render the reached callee by name, and report the summary
maximum as `unbounded` rather than maximizing over the bounded subset.

**The number is a policy verdict and a published evidence field, which is why
this is not cosmetic.** `scripts/tests/check_policy.sh:161` greps
`Max stack bound:` and enforces `[policy] max_stack_bytes`;
`check_assumptions.sh:128` does the same for `[allocation] stack_max_bytes`; and
`capture_release_bundle.sh:128` publishes it as `evidence.max_stack_bytes`. The
fail-closed branch that should catch this cannot fire — it triggers when the
report has NO numeric max, and the defect guarantees a finite one is always
printed. So a budget would pass on unbounded stack rather than refuse it.

Not live: all five budgeted regression/evidence projects (`crypto_verify`, `parse_validate`,
`fixed_capacity`, `hmac_sha256`, `constant_time_tag`; only HMAC remains in the ratified public
flagship portfolio) are
recursion-free, so every enforced number is currently correct. That is a property
of the corpus, not the mechanism, and it is the reason this sits after R-0471 and
R-0470 rather than at the front: lowest urgency of the three, but an unpredictable
trigger, so not late either. **It must precede any work that implements or
tightens a stack budget.** If a recursive function lands in one of those five
first, this becomes an H-numbered evidence hole with no further investigation
needed.

Gates, since `scripts/tests/check_stack_depth.sh` does not exist and that absence
is how the defect shipped: a fixture with direct recursion, mutual recursion, and
a bounded caller of each; a class-level assertion that no function reported with a
finite `stack:` can reach one reported `unbounded`, cross-checked against
`--report recursion` — which is itself currently ungated, has zero references in
`scripts/tests/`, and is folded in here because it is the same script and the
cross-check needs both reports; a negative budgeted project with recursion that
`check_policy.sh` and `check_assumptions.sh` must REFUSE; and confirmation that
bundle consumers tolerate a non-numeric `evidence.max_stack_bytes` — the fallback
is `"?"` today, so prefer an explicit `"unbounded"`. A mutation restoring the
`!isRecursive c` filter must fail the gate.

The summary and its consumers are fixed (bug 065, 2026-10-18): `Max stack bound:` reads
`unbounded` whenever any listed function has no bound, the policy gate, the assumptions gate and
the release bundle read it through `scripts/tests/lib/stack_bound.sh` (an unbounded stack refuses
every budget; the bundle publishes `"unbounded"`), and `check_stack_budget.sh` gates that with a
recursive budgeted fixture and a bounded positive control. What remains of this task is the
class-level reachability assertion cross-checked against `--report recursion`, and a gate for
`--report recursion` itself.

### Task R-0450

**Phase:** 2, 3, 4, 5

**Reconciliation (2026-10-19):** Keep; generate obligations against Core, not a new SSA proof authority.

**Objective:** Unify obligation lowering into one prover-neutral IR with per-backend drivers (Why3 shape) — implementation already in progress off-repo; land it in a worktree and merge once green per the operating rules. The problem is duplicated expression lowerings of the same obligations: several answers to one question, free to drift — and drift here means the Lean proof and the external check silently prove DIFFERENT obligations, an evidence-integrity defect in the system R-0004 is hardening. Define one typed obligation IR (linear integer arithmetic, bitvectors, bools, arrays: the deliberate intersection fragment; everything else `not_supported`), one semantics (a single `eval` in Lean, the IntArith single-source discipline), lowering as small named transforms, and per-backend drivers that select which transforms run.

**Ownership decision:** promote `Concrete.Semantics.TermIR`; do not mint a
parallel VIR. TermIR owns internal proposition syntax and semantics. VIR is the
validated, versioned public codec/digest boundary described in
`docs/verification/VERIFICATION_IR.md`. ObligationCore owns provenance/status and references
the canonical proposition identity. Receipts and policy remain separate.

Current TermIR is a seed, not yet a typed authority: `Srt` exists beside an
unindexed `Term`, and variables/symbols are strings. Add `RawTerm`/`RawGoal` to
closed `ValidatedTerm`/`ValidatedGoal` validation (or an equivalent
intrinsically sorted representation), stable identities and symbol signatures,
then permit only validated goals into renderers, profiles, digests, and receipts.

**Promotion slices:** inventory current obligation shapes/refusals; validate
sorts/arity/width/context; define canonical context and full proposition digest;
specify hostile canonical bytes; implement a second parser; attach exact
certificate profiles; migrate one family in structured shadow comparison; make
all renderers consume validated goals; bind ObligationCore/receipts; delete
direct `Expr -> prover string` paths. Arrays and records are reviewed semantic
extensions, not incidental constructor additions.

Acceptance includes alpha/order invariance, semantic-change sensitivity,
malformed/ambiguous context refusal, digest context binding, parser agreement,
wrong-goal certificate swaps, renderer constructor totality, and a mutation
showing no legacy string path can retain a friendly verdict.

This task's pulled-forward position is conditional: it may land representation,
semantics, and differential gates after R-0004 only when the existing branch is
merge-ready. It does not issue receipts or upgrade any verdict to authoritative
evidence; if it is not ready, R-0006 proceeds rather than waiting for it.

   **The count has grown since this task was written, and the growth is the
   argument.** It was two lowerings (`toLeanProp`, `exprToSmt`). Today there are
   three expression lowerings — `toLeanProp`
   (`Concrete/Report/ReportVC.lean:592`), `exprToSmt`
   (`ReportObligations.lean:791`), `exprToLeanProp` (`:863`) — plus
   `exprToProver` (`:930`) with four driver instances (`rocqLowering`,
   `isabelleLowering`, `rocqNiaLowering`, `isabelleSmtReplayLowering`).
   `exprToProver`'s own comment reads "Same linear fragment as
   `exprToLeanProp`": the fragment is now *defined twice*, in prose, in two
   functions that must agree. That is the exact drift this task exists to
   remove, and every kernel added before it lands adds another copy.

   **The generator feeding this IR may change under it (R-0464).** That task moves
   obligation generation from the surface AST to SSA. It does not alter this
   task's shape — the IR still receives an obligation and lowers it — but the
   terms arriving will be over SSA names, so do not bake source-level naming
   assumptions into the transforms. Landing this IR first is preferable, so the
   SSA generator emits into it rather than into today's ad-hoc lowerings.

   **Point the agreement technique at the older paths.** R-0448 invented a way to test
   that a rendering denotes the obligation without writing a parser per target: pin the
   driver's own output to ground assignments, make the target decide it, compare against
   `evalBoolEnv`. Status 2026-08-03: `exprToSmt` IS now checked this way
   (`smtAgreementGoals`, wired at all three consumers) — an earlier version of this
   paragraph said it was not, and that stale claim misdirected a round of prioritisation.
   Lean's own rendering is still unchecked and is the only remaining place validation is
   asserted by construction rather than minted.

   The reason once given for that — "its rendering is the reference the others are
   validated against, so nothing exists to validate it with" — is FALSE. The agreement
   check validates a rendering against the reference EVALUATOR (`safeOn`/`evalBoolEnv`,
   which walk the AST); Rocq and Isabelle are not compared to Lean, they are compared to
   what the expression means. The real obstacle was that Lean's lowering was not
   expressible as a driver, which this task's 2026-08-03 slice removed: the linear fragment
   is lowered by one parameterised function (`exprToProverU`) and `exprToLeanProp`
   delegates to it. What remains is to run the agreement scripts through a Lean driver and
   mint its witness.

   Slice 1 unifies the *existing* backends only — no further provers; the diff
   should be net-negative in lowering code. Note the original "two existing
   backends" framing was overtaken by R-0448's spike, which landed four prover
   drivers ahead of the IR: absorbing them is now part of slice 1's scope, not
   a follow-on. Gates: constructor coverage
   (every transform/emitter handles or explicitly rejects every IR
   constructor, fail-closed), Lean/SMT verdict agreement on generated
   obligations including identical `unsupported`s, and identity carried as
   resolved callable references (R-0442's output), never name strings.
   R-0170's auto-discharge must generate INTO the IR, not the ad-hoc
   lowerings. Version-stamp the IR in every emitted artifact so R-0004
   receipts record what produced them.

### Task R-0006

**Phase:** 1, 4

**Reconciliation (2026-10-19):** Keep containment now; integrate proof obligations into V3/V4.

**Objective:** Fix bug 052 — array element destruction becomes a no-op Immediate containment: reject `T: Destroy` for arrays/unnamed element types whose drop glue cannot be resolved; never synthesize an empty destroy function from “name lookup missed.” Root fix: structural, type-directed drop-glue identity for arrays and nested aggregates, independent of `tyName`. Gate destructor counters for arrays of linear values in Vec/Deque/heap containers, nesting, partial construction/failure, clear/remove/drop, exactly-once behavior, and a mutation replacing structural glue with the old no-op fallback.

   The bug 052 fix landed the containment and the array root fix: Mono
   decides a missing `_destroy` from the argument type (per-element glue for
   `[E; N]`, nested and generic elements included; the empty no-op only for
   Copy; E0814 for unresolved or destructor-less types), Check's
   `destroyable` lost its accept-anything catch-all, and the interpreter
   destroys array elements too. Gated by six legs in
   `check_collections_copy_only.sh` (Vec drop/clear, nesting, `[Vec<T>; N]`,
   Deque, compiled vs `--interp`, two refusal controls). What remains here:
   BinaryHeap and keyed-container legs with array elements, partial
   construction/failure, the mutation that restores the no-op fallback, and
   retiring the name-based `{Type}_destroy` rewrite for named types in favour
   of type identity.

### Task R-0007

**Phase:** 1, 2, 4

**Disposition (2026-10-19):** superseded by R-0492/R-0510: merge, preserving function-symbol and metamorphic controls.

**Objective:** Fix bug 054 — non-injective monomorphized names First fail closed on every generated/user symbol or type-name collision. Then separate semantic `TypeId`/`FunctionId` from link/display symbols and use an injective, versioned mangling encoding that user identifiers cannot forge. Layout lookup is by identity and missing fields diagnose instead of returning a past-end offset. Gate ambiguous type-argument boundaries, user names resembling specializations, module/basename pressure, deterministic symbols, and collision mutations.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0007).

### Task R-0008

**Phase:** 1, 2

**Disposition (2026-10-19):** superseded by R-0492: merge reproducer and alias permutations into identity migration.

**Objective:** Fix bug 055 — sibling renamed import emits an undefined callee Resolve an import once to canonical definition identity and carry that identity through Mono/codegen; do not repair only one alias-string orientation. Gate plain and generic sibling imports, qualified/unqualified and renamed forms, duplicate basenames, module-order permutations, and undefined-symbol failure injection. This is a rejected-valid-program bug unless a wrong-code witness appears.

### Task R-0434

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** superseded by R-0502: merge into layout certificates and Memory CFG verification.

**Objective:** Verify that every aggregate payload write fits its emitted declaration Bug 051's defect was a payload written through a declaration too small to hold it. R-0001 fixed the cause (one declaration per enum instantiation) and closed the forged-name variant fail-closed (E0809), so no known program reaches the shape today. What is still missing is the general invariant: nothing checks, for an arbitrary program, that a store through an aggregate actually lands inside the aggregate that was declared.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0434).

### Task R-0009

**Phase:** 1

**Reconciliation (2026-10-19):** Keep; remove unusable predicate until implemented.

**Objective:** Fix bug 049 — vacuous `reduce --predicate crash` Remove the predicate from help/CLI immediately unless the same slice evaluates candidates in an isolated subprocess with bounded time/memory/output and preserves the original crash boundary/class. Parse success is never a crash predicate.

    The reducer must verify the final candidate still satisfies the predicate,
    report stage/error-class drift, and say explicitly when a multi-module
    input has no supported reductions. Gate a healthy program (must not reduce
    as crash), signal/tool/compiler/runtime crash classes, timeout, changed
    failure class, empty candidate, and reducer self-failure.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0106** (merged): duplicate of R-0009/R-0491: merge new backend predicates into shared reducer/harness.

### Task R-0010

**Phase:** 1

**Reconciliation (2026-10-19):** Keep; connect to R-0491/R-0513 inventories.

**Objective:** Make the bug-corpus truth gate honest Replace the current skip-based audit summary with an explicit per-bug state:


```text
fixed_with_regression
open_with_reproducer
documented_no_fixture
missing
```

Only `fixed_with_regression` contributes to “regression coverage.” An open bug
requires a checked-in minimal reproducer plus exact expected bad observation,
watchdog/resource limits where needed, owner/priority/containment, and replay
command; it remains red or explicitly quarantined and can never be summarized
as covered/fixed. Mutation-sensitive class gates replace individual reproducers
when the fix lands. The reverse inventory fails on undocumented `bug_*.con`
files and on numbered bug docs absent from the state table.

**Untracked audit corpus prerequisite.** Before the reverse inventory becomes
enforcing, classify every file under `.audit_me/`: promote a minimal reproducer
with its control, expected observation, and case manifest into
`tests/bugs/<id>/`, or delete it deliberately after recording why it is
superseded. Existing `tests/wrong-code`/fixture paths may migrate when touched,
but no second bug registry is created. The directory currently contains the
bug-050–055 audit probes, four mini-projects, and generated binaries/LLVM files;
leaving them untracked makes `git clean` a data-loss event and makes the future
reverse inventory fail for reasons no owner has adjudicated. Generated
artifacts are never promoted as source fixtures, and new `.audit_me/` files are
forbidden once this gate lands.

**Publication discipline.** A milestone is not “pushed everywhere” until the
same intended tip and bug/roadmap artifacts are present on every declared
remote and required CI conclusions are recorded. Remote parity is process
state, not a compiler bug; report it separately and do not let a green origin
implicitly describe a lagging mirror.

The publication state machine is fixed: run the required local gates; push the
primary; wait for the named remote-CI conclusions; verify the primary ref still
equals the intended tip; then fast-forward the mirror and verify both refs.
The mirror may never lead the primary, and local hook success is not a synonym
for hosted CI success. A divergence requiring anything other than a
fast-forward stops publication for an explicit reconciliation; the helper may
not hide it with a force push.

### Task R-0439

**Phase:** 1

**Reconciliation (2026-10-19):** Keep bounded gate; merge hook-budget implementation with R-0513.

**Objective:** Gate work that is finished but not landed — the local counterpart
of publication discipline.

The Definition of Done covers *done*. Nothing represents "evidence exists, not
merged", and that state has now cost real work twice. R-0001's root fix sat as
uncommitted working-tree state in a locked worktree for a day while three
documents on `main` described the pre-fix language. `worktree-h18-drop-glue`
still holds three commits — a bug reproducer, a `MonoVerify` residual verifier,
and classified fuzzer legs — that no merged branch contains, on a branch named
after unrelated work.

Do not merge that branch wholesale: R-0001's enum-monomorphization
implementation already landed independently. Review and promote unique hunks by
owner: `MonoVerify` type/layout invariants to R-0434;
callable/application-identity material to R-0442; classified generator cases to
R-0272/R-0095; and an independently reproduced user-Option defect to a newly
numbered bug record. Discard superseded R-0001 implementation hunks only after
that inventory is recorded. This selective review is the required disposition
for the named worktree, not permission to mix implementation into a
documentation slice.

This is not a request for task/branch status tags in this file: those are
forbidden, and for good reason. A task-internal slice table such as R-0004's may
record landed versus pending acceptance boundaries while that one migration is
active, but it cannot represent unmerged work as landed. R-0439 is a gate —
scoped in three levels, because a pre-push hook
that fails whenever *any* local branch has unmerged commits would fire on
ordinary feature work, depend on unrelated local state, and behave differently
from CI. An opt-out registry under that design becomes mandatory bookkeeping.

1. **Pre-push, narrow:** only the branch being published. Dirty tracked state,
   commits that belong to the milestone but are not in the push, and the required
   gate evidence for what is being published. This level must remain routinely
   usable: target under five minutes. If measurement shows the full CI gate set
   exceeds that budget or regularly hits the hook timeout, run
   `make test-fast-surface-gates` plus path-matched load-bearing gates locally
   and leave the exhaustive `make test-ci-gates` run mandatory in CI. Do not
   preserve a predictably bypassed hook merely because it is stricter on paper.
2. **Repository audit, explicit command:** report worktrees and branches with no
   upstream and no recorded parked status past an age threshold. This is where
   `worktree-h18-drop-glue` should surface — reported, not blocking.
3. **Milestone gate:** every slice a milestone claims is merged or explicitly
   retired. This is the level that would have caught R-0001's root fix sitting
   uncommitted while three documents described the pre-fix language.

Gate: fixtures for a dirty publishing branch (must fail at level 1), an unrelated
unmerged feature branch (must NOT fail level 1, must appear at level 2), an aged
branch with no upstream, a parked branch with its recorded status, a clean tree,
and a milestone with an unmerged owned slice; plus a mutation removing the
dirty-state leg. Cheap to run — level 1 must not need a compiler build.

### Task R-0435

**Phase:** 1

**Disposition (2026-10-19):** superseded by R-0500: merge migration, renamed-pattern syntax and controls.

**Objective:** Ban lexical value shadowing while preserving explicit assignment
rebind.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0435).

### Task R-0440

**Phase:** 1, 5, 7

**Reconciliation (2026-10-19):** Split immediate verdict honesty from later package-policy productization.

**Objective:** Make evidence multidimensional instead of a ladder, and record
compiler trust per claim.

**Decision status:** ratified 2026-07-25; implementation pending. A ladder may
remain as a policy preference among claims with the same subject and semantic
scope, but it is not a universal ordering of unlike evidence.

`docs/verification/EVIDENCE_CLASSES.md` and `docs/verification/CLAIM_TAXONOMY.md` mix categories that are
not comparable. `proved`, `tested`, `enforced`, and `trusted` are evidence
*methods*; `stale`, `partial`, `missing`, and `counterexample` are *statuses*;
`reported` is an observation, not evidence at all; `runtime_checked` is a
disposition; and source / Core / SSA / native / target are *scopes*. Ranking them
on one axis produces false comparisons: a native differential-oracle test covers
the backend and runtime, while a ProofCore theorem reasons more strongly over a
narrower semantic layer. Neither is "below" the other — they are incomparable
evidence about different scopes, and the current model cannot say so.

Represent a claim as orthogonal fields — subject binding, claim kind, semantic
scope, evidence method, status, assumptions, producer, validated_by,
trusted_dependencies, freshness, replay receipt — and let the CLI keep rendering
friendly composites like `proved_by_lean`. The scope axis is the one genuinely
missing today, and it is the axis where the compiler already has the facts.

The normative target is now
`docs/verification/EVIDENCE_ARCHITECTURE.md`: five objects—`SourceCorrespondence`,
`LogicalValidity`, `DependencyClosure`, immutable `ReplayReceipt`, and
time-relative `PolicyDecision`. Implement them as a shared envelope plus typed
variants and authority-state constructors, not one record of optional fields.
Reports and package formats are projections of these objects.

Replace interchangeable digests at authoritative boundaries with
domain-separated types (`SubjectDigest`, `ImplementationDigest`,
`ContractDigest`, `PropositionDigest`, `TheoremArtifactDigest`,
`CertificateDigest`, `DependencyRoot`, and `ReceiptDigest`). Gate that swapping
two well-formed digest families fails at compile time. Migration inventories
every raw `String`/tuple boundary, runs old/new structured facts in shadow, and
deletes the old path after corpus conversion.

Advisory disposition is deliberately **not** a replay-receipt field. A receipt
records the checker/rule-set/artifact/environment that was replayed; current
acceptance is computed later from that receipt, the current advisory registry,
authentication, and consumer policy. A retained policy-decision attestation
binds its policy and advisory-registry identities separately.

Move evidence honesty to authorship as the language surface matures:
`#[certificate_by(...)]`, `#[proof_object_by(...)]`,
`#[trusted_proof_by(...)]`, and `#[assume(...)]` select distinct variants.
Existing `#[proof_by]` migrates conservatively and cannot silently inherit the
strongest class. Gate each mode through the production report, receipt, package,
and policy consumers; syntax alone earns no authority.

Trust is three fields, not one, because "who emitted the artifact", "who checked
it", and "what must be trusted for the claim to hold" are different questions.
The compiler may *produce* a Lean proof term that the kernel independently
replays: the producer need not be trusted for proof-term validity, while the
compiler is still trusted for the source-to-ProofCore correspondence. Collapsing
those into one `producer_trust` would lose exactly the distinction that makes
independent checking worth building.

What must stop being elided is that "compiler-enforced" reads as trust-free and
is not: until checker soundness and artifact production are independently
verified, enforcement depends on this compiler being correct. Being implemented in
Lean does not remove that dependency; mutation testing and duplicate boundary
checks are strong engineering evidence, not a soundness proof.
`docs/compiler/ARCHITECTURE.md` already admits `ValidatedCore` does not remove the
compiler from the TCB and `docs/verification/TRUSTED_COMPUTING_BASE.md` holds the accounting —
the change is that the dependency appears per claim.

Migration scope is larger than the two evidence documents. Residual total-ladder
language elsewhere in this file recreates the ordering this task rejects —
`tested_by_property` described as "always below proof and below solver evidence",
and a cross-checked solver result as "stronger than" another method. A
policy-specific partial order is fine, but it may only compare claims of
equivalent scope and subject; rewrite those sites as part of this task rather than
leaving a third vocabulary.

Gate: a fixture set covering one claim per (scope × method) cell that exists
today, a negative fixture proving two incomparable claims cannot be ordered by
the renderer, a fixture proving a claim without the producer/validator/trusted-
dependency accounting required by its method is rejected, and a red-team case
proving no rendering path can present a weaker-scope claim under a stronger
composite name. Reconcile both documents against the implemented model rather
than leaving a third vocabulary.

Also gate graceful degradation: revoking one checker changes logical validity
and policy acceptance while leaving independently established source binding
and dependency closure intact. A renderer that collapses those facts into one
stored `proved` field must fail.

ProofCore callable identity is pulled forward as R-0442 in the global sequence.
Every proof-status-area dependency/completeness task consumes that direct-call versus
callable-value distinction; none may recover callable identity from a source
spelling or reintroduce the old ambiguous `PExpr.call` representation.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0059** (merged): duplicate of R-0021/R-0440: merge; replace single-class taxonomy with orthogonal facts.

### Task R-0437

**Phase:** 1, 2, 4

**Disposition (2026-10-19):** superseded by R-0514/R-0510: merge declaration and specialization checks; preserve distinct diagnostics.

**Objective:** Give generic bounds a closed, validated vocabulary — an unknown
bound name is currently a silent no-op.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0437).

### Task R-0447

**Phase:** 1, 2, 3

**Disposition (2026-10-19):** duplicate of R-0481: merge unique operational controls; retire conflicting directory blueprint.

**Objective:** Freeze repository ownership and layout before broad stdlib,
test, proof, and documentation growth multiplies the current ambiguity.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0447).

### Task R-0444

**Phase:** 3

**Reconciliation (2026-10-19):** Keep implementation workload; proofs follow Core/VC prerequisites.

**Objective:** Pull the fired parser trigger: build a combinator core for binary/text parsing and port the duplicated spellings onto it. R-0406's gate was "binary-format DSLs only if packet/ELF examples show repeated parser boilerplate" — the 2026-07-25 audit count found it already fired: seven combinator shapes spelled ~45–50 times across std.numeric, std.parse, std.base64, std.hex, std.cli, std.text, and five example parsers, with `numeric.con` writing the same bounds-guard-plus-endian-fold sixteen times. The seven: `take(n)` bounds-guard-then-advance, `tag` literal match, length-prefixed consume, bounded repeat over remaining, `take_until`/`skip_while` scan, byte→`Option` table map, verify-over-region — plus one pure fixed-width assembly fold.

   Slice 1 (no Lean): the seven combinators + assembly fold as a
   `std.parse` module; port `numeric.con` onto it (deletes more code than
   it adds). Gate against the existing ByteCursor/byte_view fixtures. The
   length-prefix overflow obligation (attacker-controlled length in offset
   arithmetic) lives in the length-prefixed combinator ONCE, checked,
   instead of being re-derived per parser by width accident — PNG's
   `off + 12 + dlen` is unreachable today only because `be32` caps the
   length, not by construction.

   Slice 2: port png_chunks and elf_header. elf_header is the demo: its
   validation core reads via trusted pointer reads with no bounds relation
   to the bytes requested (it predates ByteCursor); after the port the
   trusted boundary shrinks to fopen/fread and the core is
   combinator-built — same parser, smaller boundary, one diff that tells
   the whole story.

   Proof handoff, deliberately outside this early task: R-0170 proves the
   seven combinators + fold once after operational auto-discharge has an exact
   semantic foundation, so parsers built from them can inherit
   memory-safety/non-malleability obligations instead of accumulating
   hand-written bridge theorems. R-0244 owns the DNS message parser and its
   compression-pointer bound: cycle-prone backward offsets must show “cannot
   loop more than N times the input length,” not merely reuse an index-loop
   argument. Claims apply only to parsers built from the combinator core;
   Concrete remains a general language and nothing stops hand-rolled pointer
   arithmetic outside that discipline. Never claim “Concrete parsers are
   safe.”

   Composes with: R-0244 (bounded repeat exposes its symbolic iteration
   bound as `complexity_guarded`), R-0443 (a combinator-built parser is the
   first real customer for whole-program authority certificates), and R-0442
   (proof links survive extraction). DSL generation proper (EverParse-style
   spec→source) stays research-gated in R-0406 until the R-0170 combinator
   proof leg exists.

   Immediate handoff after slice 2: the numeric, PNG, and ELF ports are the
   first forcing corpus for R-0445's source-level resource certificate. The
   task is complete at that handoff; do not hold the early resource report
   behind R-0170/R-0244 proof work. The ports must expose bounded repetition
   and allocation facts through the shared schema rather than grow
   parser-private cost annotations.

### Task R-0445

**Phase:** 3

**Reconciliation (2026-10-19):** Keep source scope; do not substitute for native sufficient-resource theorem.

**Objective:** Ship a compile-time source-resource certificate early, before
broad stdlib expansion. For each monomorphized function and selected program
root, `--report resources` (human and deterministic JSON) reports a conservative
upper bound or an explicit `unknown` for:

- source-model execution steps, as a constant or symbolic expression over
  named input bounds;
- maximum call depth;
- source-model stack bytes under a stated source layout model;
- cumulative allocation count and allocated bytes;
- peak live allocated bytes when the source ownership/lifetime facts suffice,
  otherwise an explicit `unknown`; and
- blocking, extern, trusted, or opaque boundaries that prevent a complete
  bound.

Every fact carries its subject identity, inputs, assumptions, provenance,
completeness, a versioned cost/layout-model identifier,
`semantic_scope = source`, and—when incomplete—a stable blocker code plus a
remedy-oriented explanation. V1 publishes the small weighting table that
defines a source step. Its stack value is the maximum sum of source-frame
footprints along a call path under the named source layout/target-width model;
it excludes backend spills, red zones, prologues, and ABI frame growth. A
missing fact is never converted to zero. Cumulative allocated bytes and peak
live allocated bytes are distinct facts and must never share a label or value
by convenience. A source-step or source-stack bound is never presented as
elapsed time, cycles, native frame size, or a hardware WCET claim.

Every symbolic input bound names its origin: a checked syntactic bound, a
validated contract/obligation, a profile/policy fact, or an explicit trusted
assumption. An arbitrary annotation or unchecked caller promise cannot become
a usable bound; it remains `unknown` or is surfaced as an assumption with its
scope and trust dependency. Substitution must preserve this provenance through
the call graph.

Build the certificate from canonical loop, call-edge, target-set, layout,
allocation, and stack facts. Do not add a private AST walk or second cost truth
source. Pull forward only the minimal shared resource-fact family that later
R-0178 will stabilize. Conditionals compose by conservative maximum, sequential
work by checked sum, direct calls by substitution, and indirect calls over the
conservative union/maximum of possible targets. Recursion, an unbounded loop,
an incomplete indirect target set, FFI/trusted code without a resource
contract, overflow in the bound expression, or an opaque intrinsic produces
`unknown` with the exact reason.

Use R-0444's fixed-width assembly, numeric parser, PNG, and ELF paths as the
first real corpus. Include one non-author session focused on whether the report
and its `unknown` remedies can be understood and acted on; record it as one
attempt in R-0137, but do not let it replace the later three-user validation
trial or its completion criterion.

Gate constant, linear, and nested-loop bounds; branch maximum; direct-call
composition; indirect-target maximum; zero-, fixed-, and symbolic cumulative
allocation; peak-live reuse versus simultaneous-live allocation; bound
provenance and rejection of an unchecked bound; stack/call-depth composition;
and honest unknowns for recursion, unbounded iteration, incomplete lifetime
facts, FFI/trusted boundaries, and opaque calls. Mutations that drop a loop,
call, stack, cumulative-allocation, or peak-live term must fail. Report
formatting and JSON must be deterministic, and adding the report may not change
interpreter or native behavior.

This is the early usability and design-pressure layer. R-0244 later turns the
same symbolic bounds into dischargeable obligations, R-0224/R-0248 bridge and
enforce target stack budgets, R-0416–R-0419 measure generated code, and R-0405
researches hardware WCET under a fixed target model. Backend cost preservation,
formal discharge, benchmark infrastructure, and hardware timing are explicit
non-goals here.

   Falsification probes before the machinery: (1) hand-write the symbolic
   bound for five real functions (HashMap.find_slot, one parse loop, one
   Vec grow, two others) — if the bounds cannot be stated cleanly for real
   std code, the cost model is wrong; (2) the step-count vs measured-time
   correlation experiment on the R-0416–R-0419 workloads — poor correlation
   kills every timing-adjacent reading early, by design.

### Task R-0011

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; enforce through authoritative Core/front-end boundaries.

**Objective:** Finish construction rights with private-by-default enum variants Struct-field privacy and direct-newtype construction are historical milestones, recorded in the changelog and `docs/language/CONSTRUCTION_RIGHTS.md`. Finish the same one-keyword model for the still-open construction paths: `pub` remains the only visibility word; exporting a type never implicitly exports its variants or raw representation.


- `pub enum T` exports the type name. A variant is externally constructible and
  matchable only when marked `pub`, for example `pub Ok { value: T }`. The
  marker governs the whole variant payload in v1; do not add payload-field
  visibility or a second `private`/`sealed`/`opaque` vocabulary.
- Preserve the existing newtype spelling (`pub newtype N = u32`) and make raw
  payload projection/unwrap obey the same defining-module rule as construction.
  Public checked constructors and accessors are the ordinary cross-module path.
- Representation privilege follows the exact defining module. External impls,
  parent/child/sibling modules, `trusted`, `Unsafe`, direct/renamed/wildcard
  imports, and umbrella re-exports do not widen variant or constructor access.
- Define exhaustiveness before migration: a client unable to see all variants
  must use a wildcard, and diagnostics must not reveal inaccessible payload
  structure or synthesize private patterns.
- Public interfaces may not transitively leak a module-private type through a
  public variant, callback, generic argument, capability signature, alias, or
  nested composite. ABI/layout knowledge remains separate from source access.
- Compiler-generated Copy/Destroy/equality/formatting/serialization/layout,
  manifests, debug views, and backend glue may inspect representation without
  exposing it. Visibility changes must update interface hashes, API snapshots,
  manifests, dependency invalidation, and docs.
- Mechanically mark intentionally public stdlib variants, audit invariant and
  state-machine enums rather than exporting them reflexively, and apply the
  same result in Resolve, Check, Elab, projects, interpreter, LLVM, and QBE
  artifacts.

Acceptance: positive public construction/matching and negative private
construction/matching fixtures through direct, renamed, wildcard, and re-export
paths; wildcard/exhaustiveness and stable diagnostic/span fixtures; external-
impl and `trusted` non-bypass tests; interface-leak tests; interpreter/compiled
parity; API/manifest snapshots; and mutations that accidentally export a
variant or omit one enforcement path.

### Task R-0012

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; use ordinary ownership rules and V3/V5 proof coverage.

**Objective:** Generalize workload-pulled Option/Result combinators for linear payloads This is an ergonomics/ownership blocker, not a current soundness defect, but workloads already pay the manual-match tax. Do not merely delete Copy bounds: prove arm-wise conservation and explicitly dispose of every unused owned default/error.


- Reuse the checker's existing branch-join, divergence, return-path, and linear-
  conservation machinery. Combinators are ordinary functions and must not gain
  callback- or Option/Result-specific ownership exceptions.

- Support at least `Option<String>.map/and_then` and
  `Result<String,E>.map/and_then/map_err` where each present payload is consumed
  or passed through exactly once.
- Design `unwrap_or`, `ok`, `err`, and `ok_or` separately because the unused
  default or opposite payload may require `Destroy`; do not hide disposal
  behind Clone, leaks, or broad Copy bounds.
- Gate transformed, passed-through, short-circuit, callback-failure where
  expressible, and unused-payload paths with destructor counters; add negative
  fixtures for leak/double-consume callbacks and open match arms.
- Require interpreter/LLVM agreement, proof fingerprint/spec updates, and
  representative kernel proofs to remain alpha-invariant after generalization.

### Task R-0013

**Phase:** 1, 3

**Reconciliation (2026-10-19):** Keep confirmed contract defects ahead of API breadth.

**Objective:** Close confirmed boundary correctness holes

1. Validate UTF-8 in `std.fs.read_to_string`, just as `env::get`, argv, and
   `Bytes::to_string` do. Prefer `Result<String, ReadTextError>` with distinct
   filesystem and `InvalidUtf8` cases. A file containing invalid bytes must
   remain available through `read_file -> Bytes` and must never become a
   contract-violating `String`.
2. Replace unchecked public C-string conversion at hosted boundaries with an
   owning checked `CStr`/`CString` adapter that rejects interior NUL and always
   terminates. Translate `InteriorNul` into `FsError`, `EnvError`, `NetError`,
   or `ProcessError` at the owning boundary. Keep raw conversion private or
   explicitly `_unchecked`. Gate all 13 C-string FFI consumers with empty,
   exact-capacity, embedded-NUL, invalid-UTF-8 where relevant, and normal cases.
3. Audit every external return value before constructing a typed success, but
   promote a site only with a concrete failure witness or a normative contract
   that already requires propagation:
   `fseek`/`ftell`/`fread`/`fwrite`/`fclose`/`fflush`, `clock_gettime`,
   `nanosleep`, socket operations, environment mutation, and process calls.
   Negative sizes must never cast to huge allocation sizes; zeroed FFI storage
   must never be returned as a fabricated `Instant` or address after failure.
4. Correct two already-ratified hosted contracts: `env.set`/`env.unset` return a
   typed environmental failure instead of unconditional Unit, and `args::get`
   no longer maps both out-of-range and invalid OS bytes to the valid empty
   String. Preserve a simple UTF-8 accessor; add a raw-bytes argv API only when
   a workload actually needs non-UTF-8 arguments.
5. Make `random_range(lo, hi)` enforce its documented half-open-range
   precondition by trapping when `lo >= hi`; the current `hi == lo -> lo`
   special case contradicts the bucket-2 invariant policy. Pin equality,
   reversed bounds, ordinary ranges, and overflow edges. A recoverable
   `checked_range`/`try_range` remains a separate workload-pulled API, as does
   any explicit Rng/system-entropy redesign.

**Additional input (2026-10-02, owner confirmed 2026-10-04):** R-0484's
construction/caller audit (`docs/language/HANDLE_CAPABILITIES_AUDIT.md`) assigns F7/F8 to
item 3 here: ignored write/close results (`console_err_write`, `TextFile::write`,
`TextFile::close`, `fs.File::close`, `TcpListener::close`, `TcpStream::close`) and unchecked
`ftell` failure in `fs.read_file`/`fs.read_to_string`. Repair each with an injected-failure
regression and a successful-operation control; preserve explicit cleanup and report any
failure-path limitations. **Validation is R-0030's:** the injected failures run through its
deterministic fault schedules at the `Writer`/`File` seam, so R-0013 owns the repair and
R-0030 owns the dynamic evidence that the error now surfaces — neither duplicates the
other, and R-0485 does not own these.

Gate: `scripts/tests/check_stdlib_boundary_correctness.sh`, mutation-tested at
each discarded-error/unchecked-conversion site. Add a source inventory that
fails when a hosted module introduces unchecked `String` construction from
external bytes or an ignored classified FFI result without an explicit
justification row.

Boundary rule: validate UTF-8 only when an API promises text. File, process,
environment, argument, and network byte surfaces preserve `Bytes`; explicit
String/Text adapters validate and return the declared absence/error shape.
Never force arbitrary process output, argv bytes, paths, or network payloads
through UTF-8 merely to make adjacent APIs symmetric.

`std.io.read_all` is explicitly not a confirmed hole. Its workload-2 parser
contract intentionally ends the drain on a Reader error and returns partial
bytes so the parser reports a short/invalid payload. Preserve that behavior and
its documentation. Revisit only when a socket-to-Reader workload demonstrates
that EOF, partial data, and mid-stream transport failure need a different API;
then choose an explicit split such as fallible `read_to_end` versus a named
partial-read result rather than retroactively calling the existing contract a
bug.

### Task R-0014

**Phase:** 1, 2, 3, 4

**Reconciliation (2026-10-19):** Keep library migration; merge checker rules into R-0514/V6.

**Objective:** Put Unsafe behind domain-capability wrappers and close trusted dereference duplication Split hosted modules into ordinary public wrappers and private trusted/raw adapters:


```text
public typed API: with(File | Env | Time | Network | Process | Console | Random)
        -> private trusted adapter: with(Unsafe), pointers, ABI/platform values
        -> std.libc/raw platform declarations
```

Ordinary `File::open`, `env::get`, `Instant::now`, `TcpStream::connect`,
`TcpListener::accept`, `Child::wait`, and random acquisition must not require
callers to add `Unsafe`. Allocation remains visible when the returned operation
allocates. Keep `std.alloc`, `std.ptr`, raw buffer constructors, FFI helpers,
and explicitly named `_raw`/`_unchecked` operations capability-visible and out
of the ordinary prelude.

Building on the repaired manifest derivation, extend the stdlib manifest with
`public_safety = safe | raw | unchecked`
and an owning trusted-boundary id. Start by migrating `std.net`, where compiled
workloads already exercise the boundary and `std.io`/`std.fmt` demonstrate that
trusted adapters can absorb `Unsafe` today. Ratchet the remaining reviewed
legacy rows with an owner and workload trigger; fail CI on any new ordinary
hosted API that exposes `Unsafe`, a raw pointer, or a platform sentinel. Raw
allowlist entries require a narrow purpose and no safer claimed surface. Audit
output shows both the public domain capability and the underlying trusted
assumption.

`trusted` may authorize raw implementation operations, but it is not intended
to suspend linearity. The implemented guarantee is currently narrower: E0205
(or its stable successor) rejects ordinary local/binding duplication inside
trusted functions/impls, while bug 046 proved that dereferencing
`*(p : *mut K)` can still duplicate a non-Copy owned value. Pin the working
local-copy path, and schedule a checker slice for the dereference path. Its
negative fixture must include the bug-046 shape; taking ownership through a raw
pointer is accepted only through an explicit audited primitive such as
`read_owned` whose contract consumes/invalidates the source ownership. Add
mutations for both local-copy and dereference paths. Until that slice lands,
reports describe trusted pointer ownership as an open assumption rather than an
enforced linearity property.

The raw-pointer rule is semantic, not decorative: a public operation
that accepts caller-controlled memory must take a provenance-safe Slice, be
private, or be explicitly raw/`_unchecked` with `Unsafe`. Inventory and migrate
`Vec::get_mut`, String/Text raw constructors, numeric cursor/writer `from_raw`,
IO `read`/`write_raw`, test sink pointers, and similar signatures. Conversely,
do not make callers request `Unsafe` merely because HashMap/HashSet or another
trusted implementation manipulates its own private storage; normalize the
current set/map capability mismatch according to public authority.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0051** (merged): duplicate of R-0014: merge wrappers, documentation and boundary fixtures. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0051)

### Task R-0015

**Phase:** 3

**Reconciliation (2026-10-19):** Keep workload-gated; use capability-parameterized handles.

**Objective:** Converge File and Reader/Writer when its recorded trigger fires Direction only; not a stdlib boundary-corrections-area closure condition. Pull this when a real socket, file, or buffered-IO workload otherwise needs a second implementation of short-read/write, close, or ownership behavior.


Workload 8 executed the public listener/stream path but left borrowed net-buffer
overloads at two independent asks, so it does not by itself authorize a full
socket Reader/Writer adapter or File/TextFile migration. Preserve the existing
parser-oriented `read_all` contract; a future socket adapter must state whether
mid-stream transport failure needs fallible `read_to_end`, an explicit partial
result, or a distinct operation.

Remove the overlapping `std.fs.File` versus `std.io.TextFile` split. Choose one
linear `File` handle and adapt it into the existing `Reader`/`Writer` contracts:

```concrete
File::open(path: &Path, options: OpenOptions) with(File)
  -> Result<File, FsError>
File::reader(self) -> Reader
File::writer(self) -> Writer
read_file(path: &Path) with(File, Alloc) -> Result<Bytes, FsError>
read_to_string(path: &Path) with(File, Alloc) -> Result<String, ReadTextError>
write_file(path: &Path, data: Slice<u8>) with(File) -> Result<Unit, FsError>
```

All reads distinguish `Ok(0)` EOF from `Err`; writes expose short writes;
flush/seek/tell/close return typed results. Define `SeekFrom` rather than public
integer `whence`, and never encode EOF/error as `-1`. Formatting, files,
console, fixed buffers, tests, and sockets reuse the same read-all/write-all and
fault-simulation logic. When a bidirectional resource creates adapters, use
borrowed adapters or one explicit ownership split so two independently owned
handles cannot close the same resource.

Gate: `scripts/tests/check_io_spine.sh` inventories exactly one public File,
Reader, and Writer contract; runs file/fixed-buffer/console/socket adapters
through identical short-read, short-write, EOF, flush-failure, close-failure,
and exactly-once-close schedules; and forbids a new public sink/source loop.
The forcing inventory must retain the specific legacy discrepancies rather
than hiding them under “unify IO”: TextFile write/flush/close discard
`fwrite`/`fflush`/`fclose` results; `read_byte` uses `-1` for EOF or failure;
`read_line` cannot distinguish EOF from an empty line; and console stdout/stderr
writers do not apply the same external-write result policy. Fix each when its
vertical consumer moves to the common spine.

### Task R-0016

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; retire already-fixed R-0470 prerequisite. The R-0470 prerequisite is retired (R-0470 done).

**Objective:** Add provenance-safe scoped Slice access Promote the currently exempt `std.slice` surface because existing public raw accessors already need a safe counterpart and the compiled-coverage exemption is concrete evidence. V1 deliberately uses scoped access rather than escapable slice returns:


```concrete
Vec::with_slice<R, cap C>(&self, f: fn(&Slice<T>) with(C) -> R) with(C) -> R
Vec::with_mut_slice<R, cap C>(&mut self, f: fn(&mut MutSlice<T>) with(C) -> R) with(C) -> R
Bytes::with_slice<R, cap C>(&self, f: fn(&Slice<u8>) with(C) -> R) with(C) -> R
Bytes::with_mut_slice<R, cap C>(&mut self, f: fn(&mut MutSlice<u8>) with(C) -> R) with(C) -> R
```

**Confirmed prerequisite (2026-07-25): derived callable capability
inference.** Capability checking fails closed today, but a callback reached
through a struct field, array element, or call result cannot be passed to a
`cap C` parameter even though its function type carries the exact capability
set. It is rejected as E0220; bare function identifiers and locals infer and
enforce their capabilities correctly. This is not an escalation hole, but it
blocks the API above. R-0016 must make `cap C` unify from any checked function
value origin, diagnose a genuinely underconstrained variable with
`.cannotInferCapVariable`, and retain E0240 when the caller lacks the inferred
authority. Gate direct/local/field/array/call-result callbacks, exact cap-set
preservation, and a mutation that widens or erases the inferred set.

`Slice`/`MutSlice` are private-field, non-Copy view tokens; the callback receives
only a second-class reference to the token, not an owned pointer pair it can
return. This adds no user-visible lifetime or region syntax. The callback scope
is the lifetime: the existing reference-escape rules keep the view inside the
call while the owner remains borrowed. Explicit non-goal:
do not add Rust-style lifetime annotations, escapable borrows, or a region
language for this slice.

An ordinary `as_slice(&self) -> Slice<T>`/`as_mut_slice` return remains private,
trusted-internal, or explicitly `_unchecked` until a workload demonstrates that
scoped callbacks are insufficient. A non-Copy linear Slice alone is not a
solution: exactly-once consumption does not stop the owner from being dropped
while the view exists without a checkout/give-back protocol. A future escapable
slice therefore requires a separately reviewed owner/region mechanism or an
explicitly documented unsafe/hazard contract; it is not smuggled into S3.

Convert `Vec::get_mut -> *mut T` into the existing scoped `with_at_mut` ordinary
path; if the pointer escape remains for trusted internals, name it
`get_mut_ptr_unchecked`. Raw pointer+length Slice constructors remain private or
`from_raw_unchecked`. Preserve owner provenance through the existing scoped-
reference rules—do not make slices forgeable or escapable Copy pointer pairs
without an owning borrow contract. Reader/Writer/File/TcpStream/numeric/crypto
migration is workload-pulled after these scoped constructors exist, not an all-
at-once mandate.

Gate: convert the compiled-coverage `std.slice` exemption into positive and
negative fixtures; prove the scoped callback cannot return/store/capture the
slice, the owner cannot be freed/reallocated/mutated incompatibly during the
callback, checked access rejects OOB, mutation is visible only through
`MutSlice`, and the ordinary Vec/Bytes access path no longer returns a raw
pointer or escapable Slice. Track remaining pointer-length hosted signatures in
the manifest rather than forcing an unpulled whole-stdlib migration in this
slice.

### Task R-0017

**Phase:** 3

**Reconciliation (2026-10-19):** Keep unpulled API work; merge confirmed defects into R-0013.

**Objective:** Normalize hosted failure shapes as their recorded triggers fire The normative direction below follows `ERROR_CONVENTIONS.md`, but each signature change is pulled by a workload or a failing manifest leg. It is not a demand to migrate every hosted module in one campaign.


Converge domain surfaces on explicit errors:

- `net`: `read/write/write_all/read_all/close -> Result`; `Ok(0)` alone means
  EOF; add at least `ListenFailed`, `ReadFailed`, `WriteFailed`, and
  `CloseFailed`; retain portable categories while optionally attaching reviewed
  platform detail.
- `fs/io`: eliminate raw counts/status sentinels and discarded flush/close
  failures; distinguish open, metadata/seek, read, write, flush, close, invalid
  path/C-string, and invalid text boundaries where a caller can react.
- `env`: `set`/`unset -> Result<Unit, EnvError>`; provide a representation that
  distinguishes absent from malformed. Keep invalid-value fallback policy in
  the workload, not irreversibly inside the only stdlib accessor.
- `args`: stop mapping both out-of-range and invalid OS bytes to `""`. Make the
  UTF-8 accessor optional/fallible and add a raw-bytes accessor only when a
  workload needs non-UTF-8 argv.
- `time`: `Instant::now`, `sleep`, and wall-clock acquisition surface platform
  failure; validate normalized nonnegative Duration values; keep monotonic
  `Instant` distinct from wall-clock `SystemTime`.
- `process`: typed spawn/wait/kill/close outcomes; do not expose argv pointer
  arrays or raw errno/signal conventions as the primary portable API.
- `rand`: define `random_range(lo, hi)` over the half-open range `[lo, hi)` and
  trap when `lo >= hi`, because invalid bounds are a bucket-2 caller-invariant
  failure under `ERROR_CONVENTIONS.md`. If a workload needs recoverable range
  validation, add a separately named `checked_range`/`try_range` returning
  `Result`/`Option`; never soften the existing API with a mode flag. Separately,
  distinguish deterministic generator state from hosted OS entropy and
  document modulo-bias policy when those shapes are pulled.

Use `Option` only for ordinary absence, `Result` for recoverable malformed or
environmental failure, and trap for documented invariants/OOM. Add
`scripts/tests/check_stdlib_error_contracts.sh` with one success, absence/EOF,
recoverable error, cleanup error, and invalid-input leg per hosted module.

### Task R-0018

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; update stale borrowed-Text description after R-0483. The borrowed-Text description is updated to R-0483's owner-backed Text.

**Objective:** Stabilize boundary type roles under pull discipline Freeze and gate these roles:


```text
Bytes / Slice<u8>       owned / borrowed arbitrary bytes
String / Text           owned / borrowed valid UTF-8
PathBuf / Path          owned / borrowed OS path bytes
CString / CStr          owned / borrowed checked FFI string
```

Filesystem APIs accept `Path`, not UTF-8 `String`; display conversion is
fallible. Keep operations semantically distinct and do not inflate pull counts:
`index_of(byte, from)`, future `find_subslice(needle, from)`, and future
`starts_with_at(offset, expected)` are different APIs. If a workload needs
recoverable endian bounds, add checked readers and name trapping forms
`_unchecked`; do not churn them without that consumer. `fmt::write_char` is
correct under Concrete's current byte-sized `char` contract and is not a stdlib
defect. Revisit its name/encoding only if the language character model changes.
Raw views and ownership-taking raw-parts APIs remain explicitly trusted and
document their lifetime/allocation handoff.

`BytesRaw` currently names both a non-owning result of `raw_view` and the
ownership handoff from `into_raw_parts`; audit this immediately because borrowed
and owning values cannot share an ambiguous destruction contract. Split the
owning form into `BytesParts` (pointer/length/capacity plus explicit allocator/
handoff rules) and keep the borrowed form provenance-bound, unless a narrower
design proves the same distinction. Do not collapse Slice, Text, ByteView,
ByteCursor, and the text parser Cursor merely to reduce the type count: record
their owner, validation, mutability, position, and destruction roles, then let
parser workloads decide whether ByteView is a refined Slice and whether cursors
share implementation. Move binary cursor APIs out of `numeric` only with that
consumer-backed module decision.

Gate: a generated conversion matrix covers valid/invalid UTF-8, arbitrary path
bytes, embedded NUL, empty buffers, multibyte scalars, split code points, and
owned/borrowed round trips. The API snapshot fails if a checked boundary loses
its fallibility or an unchecked operation loses the suffix.

### Task R-0019

**Phase:** 3

**Disposition (2026-10-19):** duplicate of R-0031: merge with explicit pull counts and safety requirements.

**Objective:** Add collection ergonomics as recorded workloads pull them Preserve the strong existing decisions: scoped callback access instead of escaping references, `Destroy` for live non-Copy contents, capability- polymorphic traversal, and ascending traversal only for ordered containers.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0019).

### Task R-0020

**Phase:** 3

**Reconciliation (2026-10-19):** Keep trigger ledger; assign each candidate to module owner below.

**Objective:** Add remaining module shapes as recorded workloads pull them The following are design candidates, not scheduled the stdlib boundary-corrections area migrations. Each requires independent workload evidence or a confirmed contract failure before implementation.


1. Replace public `spawn(cmd, *const *const u8)` with a typed `Command` builder
   over `Path`/owned arguments. Put fork and Unix signals in an explicitly Unix
   or raw module; generic `std.process` must not imply portability it lacks.
2. Store Duration canonically and define overflow/negative policy. Remove
   hard-coded clock ids from generic code; target facts choose the platform
   implementation. Add deterministic clock/failure adapters for tests.
3. If a workload pulls it, introduce explicit deterministic `Rng` state and
   reserve `with(Random)` for hosted entropy. Gate reproducibility, range
   bounds, invariant-trap behavior for `lo >= hi`, any separately named
   recoverable range API, and bias claims. Never describe libc `rand` as
   cryptographic.
4. Keep scalar parsing's `Option` surface; use positional `ParseError` results
   when cursor/structured-parser workloads need diagnostics. Do not create an
   implicit conversion web between module errors.
5. Keep Writer-first formatting and allocation-visible String wrappers. Test
   all Writer failures; do not use `unwrap_or` to erase a formatting sink error.
6. Make test assertion messages borrowed/static where possible, add typed
   byte/String/path assertions, and keep exit status/stdout/stderr distinct.
7. Replace `sha256::hash_raw(*mut u8) -> Vec<u32>` with a borrowed byte input
   and nominal/fixed 32-byte digest once fixed-array ergonomics support it; input
   is not mutable. Apply the same Slice input policy to checksum/base64/hex.
   Streaming crypto/encoding remains workload-pulled.
8. Replace `std.cli`'s untyped `u64` flag ids and unchecked lookups with nominal
   declaration handles, reject duplicate declarations, keep unknown/missing/
   malformed/arity errors distinct, and define repeated-flag policy. Argument
   acquisition must carry its authority consistently (`count` and `get` may not
   disagree), while pure token parsing remains capability-free.
9. A3 owns the already-pulled Option/Result consuming-combinator slice. Any
   additional convenience combinator remains pull-gated; extend `std.test`
   helpers alongside A3 so linear Option/Result values can be asserted without
   becoming unconsumable.
10. Audit pure scalar modules (`ascii`, `math`, `parse`, `numeric`, `hash`) for
    totality and naming: invalid clamp/range bounds, `abs(MIN)`, float domain/
    NaN behavior, cursor bounds, integer overflow, endian width, and hash
    stability must be documented as checked result, trap, or defined value and
    pinned by edge vectors. Avoid generic APIs whose ordering/arithmetic
    constraints are only implicit in the body.
11. Keep the remaining workload-gap ledger evidence-counted: `String::cmp` at
    its recorded ask count; `Vec::with_capacity`; SHA/hex borrowed-Bytes input;
    stdin Reader; and safe process argv construction. Promote only at the pull
    threshold or on a confirmed contract defect. Do not combine them into a
    polish release merely because neighboring modules already have symmetric
    names.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0025** (merged): duplicate of R-0047/R-0020: merge; no separate umbrella implementation task.

### Task R-0021

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; R-0117 supplies producer, R-0056 supplies diff.

**Objective:** Complete the mechanical manifest and fail-closed coverage gate Extend `concrete std snapshot --json` and the compiled-coverage inventory so every public symbol records: canonical signature, safety class, capabilities, allocation, ownership/consumption, success/absence/error/trap behavior, platform availability, evidence class, examples, and trusted-boundary id.

Generate rather than hand-count the inventory.

The inventory is fail-closed over every `std/src/*.con` module, including the
low-level `alloc`, `ptr`, and `libc` modules and umbrella `lib`. A module may be
classified pure, alloc-backed, hosted, raw/unsafe, umbrella, or intentionally
surface-less, but none may disappear because the focused migration examples do
not mention it. Adding a module or public symbol without a compiled behavioral
fixture or reasoned exemption, API snapshot entry, and evidence/authority/
failure classification fails CI. This extends the existing five-fact manifest
and compiled-coverage machinery; do not create a parallel review database.

**Usable `std` subset (2026-10-21; graduation criterion).** Name, in a checked list, the `std` APIs the
first two workloads need: RocksDB slice 1 with its companions, and the first R-0490 component workload
(or the first pilot's program if it comes first). Validate those APIs' ownership, error and foreign
boundary behaviour together, in one composed program fixture per workload rather than one fixture per
API. This subset and its fixtures are a graduation criterion for this inventory and for R-0488's
library-usability checkpoint; `std` work beyond it before V1b follows recorded workload asks
([capacity allocation](#capacity-allocation)).

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0059** (merged): duplicate of R-0021/R-0440: merge; replace single-class taxonomy with orthogonal facts.
- **R-0062** (merged): duplicate of R-0021/R-0056: merge behavioral regression gates.

### Task R-0438

**Phase:** 1, 2, 3, 4

**Reconciliation (2026-10-19):** Merge enforcement rows into R-0498; share inventory with R-0118/V7.

**Objective:** Generate the semantically-dark-construct inventory from compiler
constructors, and make the docs-drift gate check claims rather than paths.
Also expand the gate's coverage: `PRESENT_DOCS` currently lists five files,
while `docs/NOTES/` and `research/` accumulate claim-bearing design notes
outside any drift check — bring both under the path check at minimum.

`docs/verification/PROOF_STORY_MATRIX.md` is what `docs/README.md` calls "the 'no semantically
dark constructs' inventory": every construct must be `proved`, `enforced`,
`reported`, `assumed`, or `trusted`. It has 30 hand-written rows, and grep counts
zero occurrences of `defer`, `newtype`, `function pointer`, `monomorph`,
`indirect`, `generic`, `import`, and `module`.

Line that up against the defect corpus. Bug 050 was indirect calls through
function pointers; 051 was generic-enum monomorphization; 054 is monomorphized
name collision; 044 was renamed generic imports; 055 is sibling import aliases;
039 and 045 were import-alias and match-binder identity. Every one lived in a row
the inventory does not have. The constructs the language added on top of the
proved fragment — generics, indirect dispatch, module/import identity — are
exactly the ones never enumerated. A hand-maintained inventory of a growing
language lags, and this one lagged by seven constructs and seven bugs.

**Immediate stopgap, before generation lands:** add explicit hand-maintained
rows for `defer`, newtypes, generics, monomorphization, function-pointer values,
indirect calls, modules, and imports/aliases. Each row names current status,
evidence, gap, and roadmap owner. Mark the block as temporary so it is replaced,
not duplicated, when constructor-derived generation lands.

Two mechanisms, deliberately separate — they share a motivation but not an
implementation, and merging them would produce a gate that greps English prose.

**Mechanism 1 — constructor coverage, generated.** Derive the row set from
constructors the way the backend capability matrix is keyed on
`SInst`/`STerm`/`Ty`, so adding a construct fails the gate until its story,
evidence, and named gap are declared. AST and Core constructors alone are not
enough: imports, modules, manifests, and target/module selection live in the
resolve and project models, and those are precisely the identity-bearing surfaces
the missing rows cover. Take the inventory from all three.

**Mechanism 2 — structured claim records, not prose analysis.** A gate cannot
infer from English whether a normative sentence is still true. Give each
claim-bearing statement a record — claim id, `current` or `planned`, owning gate,
fixture, the compiler fact it rests on, and the commit where it was last verified
— and have the docs gate check the records and their backing, not the sentences.
`planned` is what makes the "no aspirational behavior in the present tense" rule
mechanical rather than stylistic.

Second leg motivation: extend
`scripts/tests/check_docs_drift.sh` past path resolution to claim checking. It is
91 lines and 3 checks (paths resolve, `--report` kinds exist, roadmap IDs are
well-formed) over 126 docs of which 106 assert a `Status:`. Nothing checks
whether a normative claim is still true, which is why in one session
`LANGUAGE_INVARIANTS.md` #4 asserted a rule the checker does not enforce,
`KNOWN_HOLES.md`'s OPEN section read empty while seven defects were open,
`LANGUAGE_GAPS.md`'s two "remaining findings" were both already shipped,
`DEFER.md` called move-after-defer "not yet specified or gated" when it is
diagnosed as E0206, `LANGUAGE_SHAPE.md:47` contradicted
`OWNERSHIP_MODEL.md:92-95` on linear rebind, and `FAILURE_STRATEGY.md:74` said
"there are no destructors" while `impl Destroy` and generated drop glue ship. A
`Status: stable reference` doc must cite the gate or fixture backing its
normative claims, and aspirational behavior may not appear in the present tense
beside a current guarantee.

The one-time truth repair is part of this task's migration input, not another
roadmap task. It aligns the canonical arithmetic/bounds and proof-eligibility
claims; rewrites `INTERPRETER_TRUST.md` around a second semantics tested for
divergence rather than a fictional tiny width-blind oracle; narrows
`KNOWN_HOLES.md` to an index over the numbered corpus; and corrects stale
`DEFER.md`, `LANGUAGE_GAPS.md`, `IDENTITY.md`, error-convention, and anti-feature
statements. The repair may land as documentation commits before the generated
claim records exist; R-0438 owns preventing recurrence. The same repair removes
stale hand-counted theorem totals, replaces “empty capability set means pure”
with the narrower “authority-free” claim, and makes proof-boundary documents
use R-0440’s orthogonal evidence dimensions instead of assigning every fact to
one total tier. Stable documents link to generated proof coverage rather than
freezing a count that changes when a theorem lands.

Consume R-0447's repository ownership manifest rather than inventing a second
file registry. Constructor/claim records may point to test-case manifests,
example lifecycle records, normative-document owners, and generated artifacts;
those references must resolve through the shared ownership map. R-0438
generates semantic and claim coverage, while R-0447 classifies where the
supporting artifacts belong—the two schemas join by stable IDs and neither
copies the other's facts.

An `owning_gate` in a claim record is one of R-0447's stable gate IDs, not a
workflow filename or extracted shell fragment. Claim validation consumes the
registered verdict and liveness record for that ID. This keeps the claims gate
from blessing an assertion because a permanently failing command extractor was
mistaken for a compiler/test failure.

Gate: the generated matrix fails on a newly added constructor with no declared
story, in each of the AST, Core, resolve, and project inventories; the claim gate
fails on each of the six stale claims above, reproduced as records whose backing
no longer holds; a `planned` record asserted in the present tense fails; and a
mutation removing a row or a claim record proves both go red.

### Stdlib Area Completion Contract

The final stdlib-area task may close only when:

- Tasks R-0001–R-0010 are fixed with committed
  load-bearing regression/class gate, the bug-corpus report distinguishes open
  reproducers from fixed coverage, and declared remotes contain the intended
  milestone artifacts before publication is claimed;
- private-by-default enum variants and raw newtype payload access complete the
  one-keyword construction-rights model without weakening the shipped struct-field gate;
- A3 supports the workload-pulled linear combinators with exactly-once
  conservation and updated proof evidence;
- `fs::read_to_string` rejects invalid UTF-8 without losing the Bytes path;
- every current String-to-C consumer rejects interior NUL through one checked
  adapter and the unchecked primitive is explicitly named/private;
- the public-safety rule is documented and mechanically gated, `std.net` no
  longer makes ordinary callers request `Unsafe`, and every remaining legacy
  exception is a ratcheted manifest row rather than an invisible exemption;
- Vec/Bytes expose provenance-preserving Slice/MutSlice constructors,
  `Vec::get_mut` is scoped or explicitly `_unchecked`, and the compiled
  `std.slice` exemption is gone;
- the API snapshot, compiled behavioral inventory, docs/examples, authority/
  allocation/failure manifest, and evidence report agree;
- borrowed Bytes views and owning raw-parts handoff are different typed
  contracts with mutation-tested destruction/provenance behavior; and
- the documented `io::read_all` partial-parser contract remains pinned rather
  than being silently changed under the cleanup.

S2 and S4–S7 are trigger records, not the stdlib boundary-corrections area closure criteria. Move their work
into the earliest honest global task position only with the named workload,
failing gate, or already-ratified contract violation. Do not wait for broad File/process/time/
random/crypto redesign or JSON/DNS/compression/threading breadth to close this
correction slice.

### Task R-0023

**Phase:** 3

**Reconciliation (2026-10-19):** Keep versioned language change; coordinate startup with R-0505.

**Objective:** Retire the remaining entry-point tails Deliberately decoupled from the completed exit-model migration: retire Int-main in a later surface pass by narrowing the entry signature to `fn main() -> u8 | Unit`; workload-gate any `main -> Result<Unit, E>` form and nominal error rendering to stderr.


### Task R-0024

**Phase:** 1, 2, 3

**Reconciliation (2026-10-19):** Split concrete residuals into owners; remove obsolete phase-number priority rule.

**Objective:** Finish compiler hygiene and known-defect tails Each item lands as its own evidence-ranked slice with the battery:

   - Layout panic hygiene: convert `Concrete/Check/Layout.lean` `panic!`
     paths (unknown struct/named type in fieldOffset/tySize) into
     structured internal-error diagnostics — bug 035's fix made them
     unreachable for well-formed programs, which is exactly when a panic
     should become a diagnostic. SCOPED (2026-07-17): ~69 call sites
     (tyAlign 21, tySize 27, isPassByPtr 12, fieldOffset 8, tyToLLVM 2
     incl. EmitSSA), all cascading through pure code — no small slice;
     it is an Except-threading grind that must land green in ONE piece,
     with the full battery as backstop. Dedicated fresh session.
   - Known-defects ledger (each owns a slice and battery when its stated trigger
     fires):
     These are historical/pull-gated tails inside this task, not another source
     of priority; do not select one while an earlier-numbered task lacks
     containment.
     1. Bug 039 residual (docs/bugs/039): two SUBMODULES of one top-level
        module importing different same-named fns still share a flat
        alias list (collectAllLinkerAliases). Pull-gated: fix when a
        workload or fixture hits it.
     2. `std.slice` has no public constructor — user code cannot obtain a
        provenance-backed `Slice` at all (coverage-gate exemption records it).
        the stdlib boundary-corrections area S3 owns the narrow fix because existing Vec/Bytes raw access
        already needs a safe counterpart; convert the exemption into positive
        and negative compiled fixtures. Migration of IO/fs/net/numeric
        signatures remains workload-pulled after the constructor exists.
     3. Bug 027 (EmitSSA O(n^2) rendering) — open perf item, not
        correctness; profile-driven fix when compile times matter.
     4. Differential fuzzer/oracle residuals: heap/alloc/IO shapes still
        compiled-only, trap class not compared interp-vs-compiled, no
        float support in interp comparisons.
     5. Check-side divergence/capability classifiers still match raw Core names
        after intrinsic identity was fixed in Elab/Lower. The remaining
        direction is conservative misclassification only; pull with Task R-0274's
        typing-truth work or a failing user-shadowing fixture.
     6. Non-finite float literals leak an external tool error (audit
        2026-07-16): a literal with a ≥309-digit integer part lexes to +inf,
        and EmitLLVM prints `double inf`, which llvm-as rejects — an
        external-tool diagnostic reaching the user instead of a compiler one.
        Fix fail-closed: reject non-finite literals at lex time with a
        spanned diagnostic (or emit the LLVM hex-float form); add a corpus
        leg to check_float_literals.sh.
     7. Silent defaults sweep (audit 2026-07-16; adjacent to §F's panic
        inventory but distinct — these fabricate rather than crash):
        `sizeof` with no type args defaults to `Ty.int` (Lower),
        `vec_pop`'s element type defaults to `.placeholder` (Elab), an
        unknown enum's payload offset defaults to 8 (Lower). Each becomes a
        structured internal-error diagnostic with a sweep proving no current
        program reaches them.
     8. Bug-043 class rule is prose-only (docs/bugs/043): nothing stops a
        new FFI site handing a raw `.ptr` to a NUL-delimited C API again.
        Add a std-hardening-style grep/lint leg over trusted FFI sites
        (no bare `.ptr` argument to a C string function; ptr+len sites
        exempt).
     9. printf|grep -q pipefail flake class (10b9a776 fixed 123 sites, 43
        files): add a grep-lint to gate-hygiene so the pattern cannot be
        reintroduced.
### Task R-0025

**Phase:** 3

**Disposition (2026-10-19):** duplicate of R-0047/R-0020: merge; no separate umbrella implementation task.

**Objective:** CLI/env/process helpers for real tools (stdlib APIs, not compiler CLI).

### Task R-0026

**Phase:** 3

**Reconciliation (2026-10-19):** Merge concrete exits into R-0515’s debugging/diagnostics checkpoints.

**Objective:** Finish trap/debug UX and verified-profile/ proof-obligation UX.

### Task R-0030

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; F7/F8 already provide a forcing trigger.

**Objective:** Deterministic capability-fault simulation (pull-gated; the *dynamic* complement to H3). H3's manifest gate proves an IO error *can* be surfaced (static shape); this proves it *is* surfaced under real failure (dynamic).

  Because every effect flows through an explicit capability/handle value — e.g.
  `Writer`'s `write_fn: fn(...) -> Result<u64, IoError>` — faults are injected by
  swapping the backend at the seam, with no whole-world simulator; the capability
  model makes fault injection cheap by construction, which is why Concrete's
  version is far smaller than Turso-style DST. Deterministic failure schedules
  drive properties static analysis cannot reach: `write_all` never `Ok` after a
  short/failed write; close-fail-after-flush is reported; allocation #N fails;
  `Reader` treats `Ok(0)` as EOF only at the right boundary. First consumers:
  R-0013's F7/F8 repairs (ignored write/close results, unchecked `ftell`), whose
  injected-failure controls run here. Do NOT re-litigate
  already-static properties (capability erasure = manifest gate; exit-status ≠
  stdout = MAIN_EXIT_MODEL). Proof stays separate — the pure core's first line;
  simulation covers only the effectful failure interleavings proofs do not, and
  upgrades no evidence class to `proved`. Pull-gated: pulled when H3 lands or an IO
  bug appears; run against `base64_cli`/`png_chunks`/workload 3, starting at the
  `Writer`/`File` seam and expanding only as each capability (`Alloc`, `Time`)
  gains an injectable backend. Design record:
  [`docs/project/DETERMINISTIC_SIMULATION.md`](docs/project/DETERMINISTIC_SIMULATION.md), which
  refines the tier-J+ "deterministic simulation backend" earmark in
  `docs/language/EXECUTION_MODEL.md`. Gate: `scripts/tests/check_effect_simulation.sh`
  replays a fixed fault schedule and asserts each effect-boundary property.

**Shared fault-injection checkpoint (2026-10-21; pulled by RocksDB slice 1's companions).** One
seam-based failure-schedule mechanism serves the runtime, `std` and FFI: allocation failure where the
allocator supports injection, short reads and writes, failed initialization, failed cleanup and
interrupted persistence. It is exercised composed in real programs, not only per API: R-0285's
`ffi_error_release` and `ffi_resource_pool` companions and the RocksDB slice 1 consumer run under
schedules that fail each acquisition, write, flush and release in turn. **Checkpoint exit:** each
failure kind has a deterministic schedule that a composed program runs, asserting the typed error,
exactly-once release of everything acquired and no use of a released resource; a control with the
injected failure ignored by the program under test fails; unsupported injection points (an allocator
without a seam) are listed, not counted as covered. It does not block RocksDB slice 1 itself.

### Task R-0031

**Phase:** 3, 4, 6

**Reconciliation (2026-10-19):** Keep; separate research extensions listed below. Hash-flood policy: a fixed deterministic hasher gives collision correctness, not denial-of-service resistance. Choose and document bounded input/work, a collision-resistant strategy for attacker-controlled keys, or an explicit non-adversarial-use limitation (with R-0036).

**Objective:** Build the remaining collection APIs across `std.vec`, `std.map`, `std.set`, `std.ordered_map`, `std.ordered_set`, `std.deque`, `std.heap`, `std.bitset`, and `std.slice`: fixed arrays/slices, `Vec<T>`, maps, sets, buffers, parser cursors, and capacity-aware helpers. Add the ordinary APIs C/Rust users expect: `contains`, `remove`, `iter` for maps and sets;

`insert`, `remove`, `retain`, `sort`, `search` for vectors and slices;
stable ordering helpers for ordered collections. Each API must state whether
it requires `with(Alloc)`, whether it can fail, and which runtime
obligations it creates.

**Scope until V1b (2026-10-21):** new collection APIs are added for a recorded workload ask or R-0021's
usable `std` subset, not for completeness ([capacity allocation](#capacity-allocation)); the list above
stays this task's eventual scope.

Every new collection API must preserve the H18 ownership contract documented
in [CHANGELOG.md](CHANGELOG.md) and `docs/language/RUNTIME_COLLECTIONS.md`: a
collection owns its live elements until they are explicitly moved out.
`clear`, `drop`, overwrite,
set/replacement, and compaction paths destroy displaced live elements;
`pop`, `remove`, and `swap_remove` transfer ownership to the caller.
Disposal stays forced-explicit (`x.drop()` / `defer x.drop()`), never silent
scope-end auto-drop. `get -> Option<T>` remains `T: Copy`; non-`Copy` indexed
access remains scoped callback / borrow / explicit move-out, with no returned
mutable references. The remaining pull-gated v1 fence is associated
destructor-capability machinery, pulled by the first infallible
non-`Alloc` destructor, and raw `*_unchecked` overwrite escapes remain
documented trusted escapes.

Traversal must be **deterministic but not ordered**:
`for_each`/`fold` over `std.map`/`std.set` need a fixed hasher and no per-run
random seed (so the oracle/differential story is stable given the same
operations), but the visitation order is deliberately **not** a defined key
order — `HashMap`/`HashSet` are permanently unordered (a map is unordered in
the proof model, so this costs the evidence story nothing). A defined key
order is `OrderedMap`/`OrderedSet`'s job; their `for_each`/`fold` APIs have
landed and must remain ascending-key traversal. Do not add insertion-order
tracking to the hash collections.
- 1a. Keep `Clone` and indexed move/swap as **workload-gated value-model
  research**, separate from H1 and not assumed inevitable. If admitted,
  `Clone` is explicit semantic duplication (capability-visible, usually
  `with(Alloc)`, audit-visible, and not silently proof-eligible), not a
  convenience patch for borrowed reads. The sibling move-out primitive for
  indexed containers is `swap(i, new) -> V`, which transfers ownership out
  while preserving the linear one-value-per-slot invariant. Build either only
  after a real workload repeatedly needs owned duplication or indexed
  ownership-out and the existing `Copy` / move / borrow APIs are the wrong
  fit.
- 1b. Design arena/index safety before any arena-backed structure becomes a
  flagship or stable stdlib API. Array-backed linked structures use indices,
  and a stale index into a removed/reused slot is a logic-level dangling
  pointer that ordinary ownership does not see. V1 answer should compare
  typed index newtypes (`NodeId`, not `u64`), generation-counted slots,
  arena validity invariants, and proof obligations that inject newtype
  invariants as scoped hypotheses. Public stdlib arenas remain workload-
  gated: do not add a generic `Arena<T>` until a parser/graph/runtime
  workload needs it, and never expose raw integer indices as the stable API.
  Any admitted arena-backed container must preserve linear destruction,
  allocator visibility, and stale-index rejection or explicit trust. Add
  `docs/ARENA_INDEX_SAFETY.md`, `examples/arena_indices/{stale_index,generation_checked,typed_node_id}/`,
  and `scripts/tests/check_arena_index_safety.sh`; the gate must prove stale
  index reuse is rejected, trapped, or explicitly trusted, never silently
  treated as ordinary memory-safe access.
- 1c. Decide explicit-dictionary coherence before binary collection
  operations harden. `HashMap<K, V>` values built with different `hash_fn` /
  `eq_fn`, or ordered maps/sets built with different comparators, have the
  same type but incompatible semantics; `union`, `merge`, `intersection`,
  equality, and bulk transfer across them can silently produce nonsense.
  Choose one v1 rule: canonical registered dictionary per key type,
  dictionary/brand carried in the collection type or value, debug/runtime
  identity checks on binary operations, or a documented sharp edge that
  forbids binary ops across explicitly-different dictionaries. Add
  `docs/COLLECTION_COHERENCE.md`,
  `examples/collection_coherence/{same_dictionary_merge,different_eq_rejected,different_cmp_rejected}/`,
  and `scripts/tests/check_collection_coherence.sh`; the gate must show the
  chosen verdict for incompatible dictionaries and prove the API cannot
  silently combine them.
- 1d. Future collection APIs must preserve the `with_value_mut` / `modify`
  `E0293`
  container-not-in-context guard: the callback's `&mut ctx` may not alias the
  `&mut self` container. Gated in `check_callable_values.sh`.
- 1e. Keep scalar `from(param)` returned references deeply deferred and
  evidence-gated. If ever added, they stay flat and scalar: no `Option`,
  `Result`, structs, arrays, containers, callback contexts, or generic
  wrappers. This is a research escape valve only if real workloads prove
  operation APIs, owned views, and scoped callbacks insufficient.
- 1f. Pull narrow const generics forward only when fixed-capacity stdlib APIs
  need reusable capacities. Follow `docs/language/CONST_GENERICS_V1.md`; implementation
  is deferred until a real stdlib-area consumer
  appears (`BoundedVec<T, N>`, `RingBuffer<T, N>`, `PacketBuf<N>`, fixed hash
  table, parser scratch buffer, freestanding reusable buffer, or a
  capacity-indexed proof API). When triggered, implement the staged V1 from
  that doc and wire `scripts/tests/check_const_generics_v1.sh` to prove
  distinct capacities specialize separately, layout is capacity-specific,
  runtime-safety obligations name the instantiated size, and unsupported
  comptime/reflection/runtime-bound forms are rejected.
- 1g. Research **allocator-as-value** before allocator-backed collections,
  arenas, or freestanding APIs harden. Keep the distinction sharp:
  `with(Alloc)` is permission to allocate; an allocator value names which
  allocator/arena/pool is used. Do not replace capabilities with allocator
  parameters, and do not add an ambient implicit allocator. The research
  note should compare Zig-style explicit allocators, arena/test allocators,
  embedded/freestanding pools, hot-reload/plugin ownership, allocation
  failure policy, and audit output that records both the capability and the
  allocator identity/strategy. If admitted, every allocator-taking API must
  keep allocation authority visible in function headers and reports. This
  item should move ahead of allocator-heavy collection stabilization if the
  validation project needs arenas, test allocators, reload-safe allocation,
  or freestanding pools.
- 1h. Pilot **behavioral laws for explicit interfaces** (planned 2026-10-18).
  R-0031 owns the API example; R-0271 owns its proof/instantiation semantics,
  using R-0473/R-0474's typed contracts and R-0482's evidence identities when
  formal composition is admitted. Start with the existing explicit equality
  and hash functions: state the equality laws and that equal keys have equal
  hashes. This complements 1c's dictionary compatibility: two maps can use
  the same dictionary while that dictionary's own functions disagree.
  Retain a matching pair and a deliberately inconsistent pair that treats
  `"Hello"` and `"hello"` as equal while hashing their original case
  differently; exercise insertion and lookup to expose the bad behavior.
  Finite tests report their tested domain; they do not prove the universal law.
  Bind each law's exact proposition and assumptions to the selected callable
  implementations, their dictionary and the admitted generic instance. An
  implementation change invalidates dependent evidence. Missing evidence
  withholds the corresponding functional guarantee; an explicit assumption
  remains visible through callers and package boundaries. A safe API must
  preserve memory/resource safety even when an unproved behavioral law fails.
  Keep ordinary programs usable without proofs. Begin with normal declarations,
  existing static/explicit dispatch and selected proof attachments; this does
  not admit new trait syntax, implicit instance search or blanket proof
  requirements. Use the example and measured authoring/review friction to decide
  whether to expand to comparator ordering or codec round trips. Pull this
  bounded pilot when collection/generic-proof work reaches it; it is not a
  prerequisite for the first R-0285 consumer or the R-0490 executable port.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0019** (merged): duplicate of R-0031: merge with explicit pull counts and safety requirements. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0019)

### Task R-0032

**Phase:** 3

**Reconciliation (2026-10-19):** Keep workload-pulled; no external iterator/lifetime expansion.

**Objective:** Build internal-iteration and builder APIs in proposed `std.iter` and `std.builder` after the collection shape is known. This is NOT Rust's external-iterator / adapter-tower model: `research/stdlib/iterators.md` resolved the v1 design as per-container internal traversal (`for_each`, `fold`, context-threaded callbacks, and optional early-exit via an explicit `Continue | Break` tag), with no iterator trait, no lazy adapter chain, and no cursor/lifetime model. Add known-length reporting and reverse traversal (`rev_fold`/`rev_for_each` — today every backwards walk is a manual index-decrement loop; extend `docs/project/ITERATION_PROTOCOL.md` when these land),

plus byte/text builders and tree/buffer builders inspired by Gleam's
`BytesTree` and `StringTree`. Do not hide allocation; builder APIs either carry
`with(Alloc)` or operate over fixed buffers.
### Task R-0033

**Phase:** 3

**Reconciliation (2026-10-19):** Keep over one capability-preserving spine.

**Objective:** Complete the Hare/Zig-shaped IO adapter layer on top of the one `std.io.Reader` / `std.io.Writer` spine: buffered reader/writer helpers (`bufio` shape), fixed-memory readers/writers (`memio` shape), byte-counting and tee/discard adapters, and small copy/drain helpers. These are adapters, not a second IO abstraction. Every adapter must preserve the underlying capability story, keep allocation explicit (`with(Alloc)` or caller-provided buffers), and be covered by `std.test` sink/source oracles.

### Task R-0034

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; consume the single `IntArith` semantics.

**Objective:** Build numeric helper APIs in `std.numeric`, `std.math`, and `std.mem`:

checked/wrapping/saturating arithmetic helpers,
narrowing/conversion helpers, endian conversions, byte/word packing, and
evidence classes for each helper.
### Task R-0035

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; bind coherence/laws to R-0031.

**Objective:** Build sorting and searching primitives in proposed `std.sort` and `std.search`: comparison conventions, stable/unstable sort decision, binary search, min/max helpers, and evidence/oracle tests over edge cases.

### Task R-0036

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; distinguish entropy authority and security claims.

**Objective:** Build hashing, checksums, and deterministic random helpers in `std.hash`, proposed `std.checksum`, and `std.rand`: stable hash APIs for maps/sets, non-cryptographic checksums, seeded deterministic RNG for tests/oracles, and a clear split from cryptographic randomness. Any OS entropy source is hosted-only and capability-visible.

### Task R-0037

**Phase:** 3; after-1.0/research

**Reconciliation (2026-10-19):** Keep source-shape claims only; no automatic binary timing transport.

**Objective:** Build constant-time helper APIs in proposed `std.ct` only for narrow, auditable cases: equality/compare over fixed-size bytes, no secret-dependent branches or early exits, source-shape audit evidence, and clear machine-level timing assumptions. This belongs to the narrow security surface, not broad crypto.

### Task R-0038

**Phase:** 3

**Reconciliation (2026-10-19):** Keep workload-pulled.

**Objective:** Build time and duration helpers in `std.time`: monotonic versus wall-clock distinction, timestamp formatting/parsing if admitted, timeout helpers, and explicit hosted authority for reading the clock.

### Task R-0039

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; R-0444 owns combinator core.

**Objective:** Build formatting and parsing helpers in `std.fmt` and `std.parse`:

 integer/text formatting, simple
 scanners, structured parse results, error-set reports, and oracle-friendly
 output conventions. R-0444 owns the shared parser-combinator and fixed-width
 assembly core; this task consumes that core for the remaining formatting and
 text-parsing surface rather than creating a second parser vocabulary.
### Task R-0040

**Phase:** 3

**Reconciliation (2026-10-19):** Keep unique adapters; merge overlapping scanner work into R-0039.

**Objective:** Complete the scanner/parser surface left after R-0444 over
`std.bytes.Bytes` and `std.text.Text`: span/position tracking, structured error
reporting, and streaming/text adapters, with no hidden allocation unless the
API carries `with(Alloc)`. R-0444 owns the reusable combinator core (`peek`,
`advance`, bounded take/consume/repeat/scan shapes); do not duplicate it here.
Retire any originally listed row that R-0444 demonstrably ships rather than
keeping two nominal owners.

### Task R-0041

**Phase:** 3

**Reconciliation (2026-10-19):** Keep pull gate.

**Objective:** Extend `std.base64` only when a workload pulls streaming encode/decode; preserve canonical padding-bit strictness and the args -> bytes/text -> parse/errors -> Writer oracle path.

### Task R-0042

**Phase:** 3

**Reconciliation (2026-10-19):** Keep pull gate and syntax/authority separation.

**Objective:** Add `std.uri` parsing/formatting after the byte/text/path split is stable:

 component accessors, percent encoding/decoding, normalization policy, and
 clear distinction between syntax validation and network authority.
### Task R-0043

**Phase:** 3

**Reconciliation (2026-10-19):** Keep workload-pulled.

**Objective:** Add `std.json` as the first structured data module: tokenization, string/number handling, error spans, bounded recursion policy, optional DOM-like representation only if the allocation story is explicit, and oracle tests against a reference implementation.

### Task R-0044

**Phase:** 3

**Reconciliation (2026-10-19):** Keep only with a forcing consumer.

**Objective:** Add a small typed decoding layer in proposed `std.decode` after `std.json`: dynamic value decoding, field access, error paths, and examples comparable to Gleam's `dynamic/decode`, without broad reflection or hidden runtime typing.

### Task R-0045

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; reuse R-0444/R-0034.

**Objective:** Add binary serialization helpers in proposed `std.bin`: endian-aware reading/writing, fixed-width integers, length-prefixed fields only with explicit bounds, byte-span diagnostics, and no hidden allocation.

### Task R-0046

**Phase:** 3

**Reconciliation (2026-10-19):** Keep workload-pulled.

**Objective:** Add semantic-version and config-format helpers in proposed `std.semver` and `std.config` if package/build work starts depending on them:

 `SemVer`, INI/TOML-style scanner, and manifest parsing support. Prefer a
 small Hare-like `format/ini`-class module before any broad configuration
 framework; keep TOML/YAML/package manifests workload-gated. These are
 stdlib/package-boundary helpers, not general metaprogramming.
### Task R-0047

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; absorb R-0025’s CLI portion.

**Objective:** Add command-line parser helpers in proposed `std.cli`: flags, positional arguments, usage text, typed parse errors, no ambient environment access except through `std.args`, and examples that keep authority visible. This is for user programs. The `concrete` compiler's own command taxonomy and help behavior are old phase 6E, and `std.cli` should learn from that surface without coupling to compiler internals.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0025** (merged): duplicate of R-0047/R-0020: merge; no separate umbrella implementation task.

### Task R-0048

**Phase:** 3

**Reconciliation (2026-10-19):** Keep narrow workload gate.

**Objective:** Add narrow shell/path helper modules only as tool workloads demand them:

  `std.shlex` for shell-word splitting/quoting, `std.glob` / `std.fnmatch`
  for file-pattern matching, and no `wordexp`-style command/variable
  expansion in the core. These APIs are byte/path aware, carry no hidden
  filesystem authority, and must keep expansion separate from matching so a
  pure parser cannot accidentally become a hosted operation.
### Task R-0049

**Phase:** 3

**Reconciliation (2026-10-19):** Keep workload-pulled.

**Objective:** Add simple logging/diagnostic output APIs in proposed `std.log`: levels, writers, formatting integration, capability requirements, and policy for release builds. Keep this small; it is not a tracing framework.

### Task R-0050

**Phase:** 3

**Reconciliation (2026-10-19):** Keep named consumer trigger.

**Objective:** Add progress/status output helpers for CLI tools in proposed `std.progress` only after `examples/daily/word_count` and `examples/base64_cli` need visible progress output. V1 surface: `ProgressWriter`, `quiet`, `verbose`, `set_total`, `advance`, `finish`, terminal detection through an explicit console handle, and no ambient terminal authority. Wire `scripts/tests/check_stdlib_progress.sh` when the module is admitted.

### Task R-0051

**Phase:** 3

**Disposition (2026-10-19):** duplicate of R-0014: merge wrappers, documentation and boundary fixtures.

**Objective:** Build capability-scoped console, file, network, process, and time APIs in `std.io`, `std.fs`, `std.env`, `std.args`, `std.process`, `std.net`, and `std.time`. Authority must be visible in function types and audit reports; no API may smuggle ambient authority through a convenience wrapper. New output/input APIs target the `std.io.Reader` / `std.io.Writer` spine; a separate public `std.writer` sink surface must not reappear.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0051).

### Task R-0052

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; include external-call assumptions in V6/V7.

**Objective:** Build handle-relative filesystem APIs in `std.fs` as the preferred file/path shape: directory/file handles carry authority; operations are relative to handles for `open`, `create`, `read`, `write`, `metadata`, `remove`, `rename`, and `list`. Ambient absolute-path helpers must be hosted-only convenience wrappers with explicit authority. Temp-file, symlink, path-normalization, and TOCTOU behavior must appear in `docs/stdlib/STDLIB_GUIDE.md` and `scripts/tests/check_stdlib_fs.sh`.

### Task R-0053

**Phase:** 3

**Reconciliation (2026-10-19):** Keep behind R-0052.

**Objective:** Add small `std.temp` and `std.dirs` helpers after the handle-relative filesystem shape is gated. `std.temp` owns temporary files/directories with explicit cleanup (`drop`/`defer`, no implicit scope-end delete) and visible `with(File)` authority; `std.dirs` exposes only conservative hosted directory discovery needed by tools. Do not add XDG/platform policy sprawl until package/build workloads pull it.

### Task R-0054

**Phase:** 3

**Reconciliation (2026-10-19):** Keep workload-pulled; no general networking expansion.

**Objective:** Build handle-based network surface in `std.net` only as far as the validation workloads require: address parsing, socket handle wrappers, HTTP header parsing as a pure parser first, and no full HTTP client/server until package/workload evidence demands it.

### Task R-0055

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; share public-symbol denominator with R-0021.

**Objective:** Build stdlib test/oracle helpers in `std.test`: expected failures, capability-scoped fixtures, temp directories, oracle vector runners, interpreter-vs-compiled helpers, and report snapshots. Stdlib tests must also serve as runnable API documentation: every public stdlib type/function gets a tiny positive usage test and, where meaningful, a negative or edge case. Public docs should link to or quote those tests rather than carrying stale hand-written examples. This is the Zig stdlib lesson adapted to Concrete: usage examples should be executable fixtures, not prose that can rot. Add a gate that fails when a new public stdlib symbol lacks a test/doc

 example, when a referenced example no longer compiles, or when a doc claims
 an evidence class/capability/allocation behavior that the test/report does
 not produce.
### Task R-0056

**Phase:** 3

**Reconciliation (2026-10-19):** Keep consumer of R-0021/R-0117, not a second scanner.

**Objective:** Add a public stdlib API snapshot/diff immediately after the first real workload (`base64_cli`) and before broad URI/JSON/CLI/log/progress breadth.


  Deliverable: `concrete std snapshot --json` (or an equivalent test helper)
  and `scripts/tests/check_stdlib_api_snapshot.sh`.

  Snapshot fields: module path, public names, signatures, `Copy`/linear
  requirements, capabilities, allocation behavior, trap/recoverable-failure
  profile, evidence class, deprecation status, and linked tests/docs.

  Done when adding/removing a public function, changing a signature, widening
  a capability, changing allocation/trap behavior, or changing an evidence
  class fails the gate until the snapshot and docs are updated. This is the
  stdlib-local version of the audit-commands area's proof/capability diff and the packages area's
  package API artifacts; it prevents accidental drift while the library is
  still small.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0062** (merged): duplicate of R-0021/R-0056: merge behavioral regression gates.

### Task R-0057

**Phase:** 3

**Reconciliation (2026-10-19):** Keep enforcement/documentation remainder; avoid duplicating settled policy.

**Objective:** Define stdlib error-handling conventions: when APIs return `Result`, `Option`, panic/abort, or require a policy gate; how ignored-result diagnostics apply; and how accumulating error sets are reported. Split recoverable domain/environment failures from fatal invariant failures:

 `Result`/`Option` for user, file, network, parse, and domain errors that a
 caller can handle; abort/trap for OOM, bounds, arithmetic traps, impossible
 invariants, and explicitly unrecoverable runtime failures. Audit output
 should identify which public APIs can recover, which can trap, and which
 require a policy gate. Do not add a second Zig-style error-union mechanism;
 improve the `Result`/`Option` surface instead. Define how a module error
 composes into a caller's error across a boundary (`FsError` -> `ServiceError`)
 WITHOUT Rust's `From`/`Into` trait web: an explicit conversion function or a
 small wrapping variant named at the call site, not an implicit trait
 conversion. State the pattern once so error-heavy code does not each invent
 its own.
### Task R-0058

**Phase:** 3

**Reconciliation (2026-10-19):** Keep checked onboarding material.

**Objective:** Define the canonical **consume / destroy / handoff** conventions for linear code. The guide must distinguish explicit cleanup (`destroy(x)` or the type's consuming `.drop()`/Destroy verb), ownership transfer by by-value call, ownership transfer by return, destructuring into owned fields, `defer` as explicit scheduled cleanup, and the forbidden cases (bare non-`Copy` statement, `_` over non-`Copy`, `let _`, non-`Copy` sub-place projection by value). This is documentation plus examples and diagnostics, not automatic `Drop`: no hidden cleanup and no hidden control flow. Include a short **Copy and Linear Values** guide: when to mark a type

 `Copy`, why `Option<i32>` is `Copy` but `Option<String>` is linear, how
 value-mode reads move non-`Copy` bindings by default, why `_` ignores only
 `Copy`, how params are owned locals unless they are borrows, and the
 ordinary consume paths (`destroy`, handoff, return, destructure, `defer`).
 This guide is the user-facing explanation of the conditional-`Copy` and
 mode-based-checker refactors; it should be validated by small runnable
 examples and linked from README/site docs.
### Task R-0059

**Phase:** 3, 4, 5

**Disposition (2026-10-19):** duplicate of R-0021/R-0440: merge; replace single-class taxonomy with orthogonal facts.

**Objective:** Define stdlib evidence classes per public API: `proved`, `enforced`, `reported`, `tested_by_oracle`, `assumed`, or `trusted`. The evidence class must appear in docs and audit artifacts, not just implementation comments.

### Task R-0060

**Phase:** 6

**Reconciliation (2026-10-19):** Keep after totality; rational theory remains separately pull-gated. Absorbs the canonical int/list/map/set/bitvector theory modules, spec enums (constructor injectivity/disjointness) and spec pattern matching as scoped modules.

**Objective:** Add proof-facing formal stdlib models only when a proof or contract workload pulls them. These are not runtime containers: `std.formal_vec`, `std.formal_map`, and `std.formal_set` model mathematical sequences, maps, and sets for contracts, loop invariants, and Lean obligations; `std.bigint` models unbounded mathematical integers for specs before any runtime big-number API exists; `std.rational` stays deferred until exact-ratio specs need it. Each formal module must state its erasure/runtime story, its evidence class, the Lean artifact it lowers to, and the refinement relation to runtime containers such as `Vec`, `HashMap`, `OrderedMap`, and `HashSet`.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0028** (merged): duplicate of R-0060: merge.

### Task R-0062

**Phase:** 3

**Disposition (2026-10-19):** duplicate of R-0021/R-0056: merge behavioral regression gates.

**Objective:** Add stdlib authority/allocation/runtime-obligation gates so core helpers cannot silently widen capabilities, allocation behavior, trusted assumptions, or runtime-risk obligations.

### Task R-0063

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; feed VP S7’s instrumented-semantics validation.

**Objective:** Add sanitizer/runtime-instrumentation hooks as debug and validation surfaces, not proof evidence. Inspired by Odin's sanitizer-facing base surface, Concrete should expose named hooks for bounds, overflow, use-after-free-like trusted/FFI probes where applicable, allocator checks, and generated-code validation. Audit output must classify this as `runtime_checked`, `tested`, or `instrumented`, never `proved`.

### Task R-0064

**Phase:** 3

**Reconciliation (2026-10-19):** Keep API inventory now; freestanding implementation after 1.0.

**Objective:** Split hosted versus freestanding-ready stdlib modules at the API level:

 no-alloc/no-OS core modules, allocator-backed modules, hosted OS modules,
 and modules that are explicitly unavailable under freestanding profiles.
 The freestanding target implementation still lands in the freestanding area.
### Task R-0066

**Phase:** 3

**Reconciliation (2026-10-19):** Keep under onboarding; no additional flagship gallery.

**Objective:** Add stdlib documentation and checked recipes for C/Rust users without creating a
gallery of standalone public examples:

 `docs/stdlib/STDLIB_GUIDE.md` contains `bytes_text`, `path_fs`, `result_errors`, `vec_map`,
 `parser_cursor`, `json_scan`, `base64_cli`, `uri_parse`, `checksum`, `deterministic_rand`,
 `time_log`, and `capability_io` snippets sourced from an owning test-fixture corpus so they compile
 rather than rot. Each recipe must show the exact `std.*` imports,
 capability set, allocation behavior, and expected audit line.
 Add one blessed systems-program shape, inspired by Elm's success at
 teaching a single architecture but adapted to Concrete's domain:
 **read/acquire -> parse -> validate -> transform -> emit/release**. This is
 not a framework and not hidden control flow; it is an example/documentation
 convention for ordinary tools and protocol handlers. The guide must show
 which stages are pure, which carry capabilities, where `Result` flows, where
 runtime obligations attach, and which report proves each claim. Exercise the pipeline shape in the
 minimal onboarding sequence and later in `secure_update_verifier`/`protocol_state_machine`; its
 tiny byte-oriented CLI and no-allocation parser fixtures must include the source and expected
 `--report caps`, `--report contracts`, and `--report profile` snippets.
 Include a small **stdlib style-coherence pass** in this item: each module
 must choose and document when an operation is a method versus a free
 function. Do not mix `s.len()`, `string_length(s)`, and
 `string_push_char(&mut s, c)` arbitrarily in the same public surface. The
 rule should serve explicitness and capability visibility: methods are fine
 for operations whose receiver clearly owns/borrows the authority; free
 functions are fine for cross-type operations or functions whose capability
 story would be clearer outside a receiver. Add examples that teach the
 chosen style instead of preserving accidental historical names. These are checked documentation
 snippets and fixtures, not additional flagships.
### Task R-0067

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; preserve oracle mode per vector.

**Objective:** Add a stdlib compatibility/oracle corpus under `tests/compat/stdlib/` (or the corpus
manifest's canonical compatibility home): `fmt_parse_vectors`, `bytes_text_vectors`, `path_vectors`,
`collection_vectors`, `base64_vectors`, `uri_vectors`, `json_vectors`, `semver_vectors`,
`sort_search_vectors`, `checksum_vectors`, `rand_vectors`, and `cli_io_vectors`. Wire it with
`scripts/tests/check_stdlib_compat.sh`; every vector must declare exactly one mode in `manifest.toml`:
`oracle_python`, `oracle_system_tool`, `interp_vs_compiled`, `audit_only`, or
`negative_expected_failure`. Compatibility vectors are not public examples.

### Task R-0068

**Phase:** 3

**Reconciliation (2026-10-19):** Keep selected workloads, not the entire candidate list.

**Objective:** Add real stdlib workload checks before the flagships area relies on the library.

 Start with the existing `base64_cli`, classifying it as a workload rather than a flagship. Then
 select forcing workloads from `json_validator`, `ini_parser`, `checksum_cli`, `http_headers`,
 `path_normalizer`, and `lru_cache` or `ring_buffer`; do not instantiate the whole list when the
 three flagships or an existing workload already exercise the same surface. Keep them in the corpus
 manifest's workload home and wire them with
 `scripts/tests/check_stdlib_workloads.sh`. Each workload must build, run,
 compare interpreter-vs-compiled output, and report authority/allocation/
 evidence classes. The oracle requirement is explicit per workload:
 `base64_cli` against Python `base64`, `json_validator` against Python
 `json`, `ini_parser` against a checked-in Python reference,
 `checksum_cli` against Python/hashlib or a checked-in reference,
 `http_headers` against checked-in vectors, `path_normalizer` against
 checked-in platform-specific vectors, and `lru_cache`/`ring_buffer` against
 a checked-in reference model.
### Task R-0069

**Phase:** 1, 2, 3

**Reconciliation (2026-10-19):** Keep confirmed wrong behavior urgent; use `IntArith` contract.

**Objective:** Add a stdlib sentinel/arithmetic audit before broadening hosted APIs. The checked-arithmetic flip exposed syscall/sentinel-style code that relied on silent wrap (`-1 as unsigned` followed by `+ 1`, size/error sentinels, bit-packed flags, checksum/hash arithmetic). Add `scripts/tests/check_stdlib_sentinel_arithmetic.sh` or fold the checks into `check_stdlib_workloads.sh`: every intentional modular/sentinel operation in `std.fs`, `std.process`, `std.net`, `std.io`, hash/checksum code, and binary parsers must use explicit `wrapping_*`/bit operations with a comment or a report-visible classification; every non-intentional trap must be fixed as a

 real bug, not papered over. The gate should include at least one non-
 arithmetic workload path (text/path/UTF-8, fs/process/env capabilities,
 maps/iterators, formatter/parser round-trip, or proof/report views) so the
 post-arithmetic validation does not overfit to numeric examples.
### Task R-0070

**Phase:** 3

**Reconciliation (2026-10-19):** Merge coverage requirements into R-0068 if existing workloads suffice.

**Objective:** Add the stdlib area validation workload under the corpus manifest's workload home (for
example `tests/workloads/stdlib_client/`) plus `scripts/tests/check_phase6_stdlib.sh`. It validates
the library and is not a fourth public flagship.

 The client must use `std.option`, `std.result`, `std.bytes`, `std.text`,
 `std.path`, `std.vec`, `std.map`, `std.fs`, `std.io`, `std.fmt`,
 `std.parse`, either `std.json` or `std.base64`, deterministic RNG or
 checksums, sanitizer/runtime-instrumentation hooks where available, and
 `std.test`. CI must build, run, test, audit authority/allocation/evidence
 classes, and compare interpreter-vs-compiled behavior.

### Task R-0071

**Phase:** 3

**Reconciliation (2026-10-19):** Objective revised by the audit: closing the stdlib area no longer activates QBE; per-release std acceptance and compaction stay with R-0333/R-0515.

**Objective:** Close the stdlib area against its Completion Contract, archive completed work
while preserving future-relevant invariants, and select the next checkpoint from the
current execution queue. Phase closure does not automatically activate QBE.


### Task R-0483

**Phase:** 3

**Disposition (2026-10-19):** done — closed 2026-10-09 at `4f005715`; the body is retained for its accepted coordinate/provenance limit.

**Objective:** Make safe zero-copy parsing preserve owner lifetime through every
stdlib wrapper, not only through syntactic `&T` references.

**Status (2026-10-09): CLOSED at `4f005715`** (main CI
[37917463687](https://github.com/unbalancedparentheses/concrete2/actions/runs/37917463687)
passed; `check_view_lifetime.sh` 28/0 there). Owner-bound results landed as
`std.numeric.BoundView`, which owns the `Bytes` it describes; design, fixtures and measured
cost are in [tests/regressions/owner_bound/README.md](tests/regressions/owner_bound/README.md).
Wrong-buffer substitution is rejected statically: `BoundView::byte` takes no buffer
argument (`bound_no_buffer_argument`, E0262). The owner is a private field, so it cannot be
mutated while bound (`bound_no_owner_mutation`, E0298), and the view is linear, so it must
be released (`bound_must_release`, E0208). Positive controls cover the zero-copy read, the
moved owner, the cross-package case and rebinding after release. `examples/packet` reads
its payload through a `BoundView` with no copy and no `Alloc`; its predictable profile is
1 failed (`main`, I/O) / 16 passed.

**Accepted limit:** binding re-validates coordinates against the owner's bounds, not
their provenance — coordinates computed from a different buffer of sufficient length still
bind. The guarantee is that reads go to the owner held. `ByteView` deliberately stays
reusable coordinates, and the survey rows asserting that remain in the gate.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0483).

### Task R-0484

**Phase:** 1

**Disposition (2026-10-19):** done — closed 2026-10-08 at `245cd51c`; the body is retained for its accepted limits; the typed-descriptor second step is R-0530.

**Objective:** Give capability headers, resource handles and operational effects
one coherent meaning that checking, reports, proof eligibility and policy share.

**Status (2026-10-08): CLOSED at `245cd51c`** (fast-forward from `8187a2f7`; main CI
[37771982899](https://github.com/unbalancedparentheses/concrete2/actions/runs/37771982899)
passed every job). Closure covered proof admission on the shared assumption summary
(bug 076), named trusted boundaries with the obligations they absorb, descriptor-audit
coverage complete over std (audit F6 carried out, `_exit` audited, inventory drift
gated), cross-dependency capability explanations, `std.mem.sizeof` classified as a
compiler intrinsic, and the docs/authority audit. Per-surface status (checked, human
assumption, incomplete coverage, unsupported) is in
[HANDLE_CAPABILITIES.md](docs/language/HANDLE_CAPABILITIES.md#implementation-status-2026-10-07).

**Accepted limits, each gated rather than hidden:**

- single-file mode and `--query` do not analyse dependencies and say so
  (`dependencies_analysed=false`); `--query` refuses a project file
  (`check_capability_explanations.sh`);
- bug 077 (relative call into a nested submodule) is contained: the build refuses it
  (`check_bug077_contained.sh`);
- descriptor classification is a human assumption covered only by the construction/caller
  audit; typed descriptors remain the second step;
- a bodiless user `fn` declaration cannot carry `with(...)`, so user code cannot declare
  an intrinsic;
- reports still compute the older indirect-call opacity analysis beside the summary and
  treat a function as opaque if either says so.

**Not qualified:** the full mutation campaign never completed on a closure candidate —
the Nightly job's mutation-coverage step was cancelled at GitHub's 6-hour limit on both
dispatched runs (37464836373 on `8187a2f7`, 37707710553 on `245cd51c`). R-0484 closed
under the checkpoint exception, which does not certify the campaign or carry over to a
release; the campaign qualified on `670ec85a` (2026-10-11, run 38104755470). F7/F8 stay with R-0013, F9's runtime contract
with spawn's stated assumptions, typed descriptors with the second step.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0484).

### Task R-0485

**Phase:** 2, 3, 4

**Reconciliation (2026-10-19):** Keep; refresh obsolete “during R-0483” scheduling.

**Objective:** Improve review clarity without changing explicit error propagation
or implicit-drop policy.

**Status (2026-09-15): planned; apply during R-0483's parser migration.** Keep
`match`/`return`, explicit error conversion and explicit cleanup. Align deferred-call
argument coercions and diagnostics with ordinary calls; specify failure/divergence
during cleanup and test normal-exit cleanup ordering. Improve named library helpers
and ownership/error diagnostics where parser and CLI workloads expose friction.

**Exit:** compare the migrated parser and base64 CLI for nesting, duplicated cleanup,
missed I/O errors and reviewer ability to locate ownership transfers and exits.
Retain concrete before/after examples and regression controls for any implemented
change. Do not equate generated verbosity with human reviewability. `?`, implicit
error conversion, implicit drop and unwinding remain excluded.

### Task R-0486

**Phase:** 3, 4, 5, 6, 7

**Reconciliation (2026-10-19):** Keep workload now; formal graduation after Phase 5/6 prerequisites.

**Objective:** Use one bounded parser/state-machine component to validate compositional
contracts, audit diffs and the external-consumer workflow end to end.

**Status (2026-09-15): planned; workload selection begins with R-0483, proof graduation
depends on the existing identity/task, typed-contract and two-state queue milestones.**
Reuse the parser in the secure update-bundle verifier or protocol-state-machine
flagship already selected in the queue's external-user workflow row. Expose a small C interface and exercise
malformed input, bounded storage, state mutation, zero-copy access, recoverable
failure, resource limits and named FFI assumptions. No fourth public flagship.

**Shared change-and-repair scenario (2026-10-04):** prefer the bounded parser/state
machine in `secure_update_verifier`; use the existing protocol flagship if its admitted
fragment provides the smaller forcing case. Do not require the complete update verifier
or all its cryptographic claims for this milestone. Implement the component, establish
a useful behavioral contract that is not merely a restatement of its body, and retain
baseline evidence. Change a dependency in separate controlled variants: add authority,
introduce/widen an assumption, and invalidate contract/proof evidence. Show the precise
diff and applicable policy rejection, perform an explicit repair, then replay the
resulting evidence with the independent consumer. A contract-preserving body edit is
the positive control for preservation after implementation revalidation. Missing facts
or incomplete analysis produce an explicit incomplete result, never a false-clean diff.

Use this one scenario across typed contracts, R-0182/R-0183 review, existing policy and
package tasks, and R-0353 replay. R-0150 records annotation and manual proof burden,
feedback latency and repair effort from the initial baseline; set improvement targets
after observing that baseline. Measure the non-author session, including failed attempts
and required author assistance. The scenario does not reorder qualification, identity,
contract or independent-consumer prerequisites, or require a registry, new backend or
general heap model.

**Exit:** a consumer relies on a public contract without inspecting private bodies;
calls establish preconditions and use exported postconditions without inlining;
frame/`old` semantics cover the admitted mutation fragment. A contract-preserving
body change refreshes implementation evidence while leaving contract consumers
current; behavior/authority/resource/trust changes produce precise policy and audit
diffs. A non-author builds and consumes the component and replays its admitted
evidence. Measure proof effort, source review clarity, callback/context friction and
runtime costs; use those observations to prioritize subsequent features. Unsupported
heap, relational, resource-proof and backend claims remain explicit.

### Task R-0488

**Phase:** 1, 3

**Reconciliation (2026-10-19):** Keep safety audit early; later contract/diff additions stay with existing owners.

**Objective:** After R-0484, audit parameter-mutation guarantees, then pressure-test
the library model before adding more language machinery.

**Status (2026-09-30): planned; depends on R-0484, not yet scheduled in the execution queue.**
Preserve R-0483 and the existing safety priorities; schedule this checkpoint explicitly
after those priorities are reviewed. Qualify
existing `&T`, `&mut T` and ownership semantics before proposing new syntax. This is
distinct from the post-R-0004 **mutation-testing qualification** campaign. The
bounded audit does not bypass identity, receipt or typed-contract prerequisites.

1. **Parameter mutation first.** Inventory mutation through parameters, nested
   fields, aliases, callbacks, trusted wrappers and imported APIs. Verify that safe
   read-only access cannot become mutable and that intended mutation is visible in
   parameter types. Retain an accepted mutable-buffer update, a rejected read-only
   write, and an indirect/cross-package laundering case. An empty `with(...)` may
   coexist with parameter mutation; reports and contract admission must not infer
   purity from it. Repair concrete violations, with causal mutation tests where
   they demonstrate enforcement.
2. **Library usability next.** Exercise buffered I/O, fixed and growable buffers,
   sockets, stored callbacks, explicit cleanup and package boundaries in existing
   libraries/examples. Include closing an owned handle versus transferring it, and
   `Writer<{}>` versus `Writer<Alloc>`. Record annotation propagation, diagnostic
   clarity, duplicated cleanup, missed errors and audit effort. Reuse R-0485's
   review criteria and R-0486's flagship component; no new public flagship. Require
   repeated workload evidence before introducing more expressive language features.
   Graduation also needs R-0021's usable `std` subset (2026-10-21): the APIs the first two workloads
   need, validated for ownership, errors and the foreign boundary together.
   Include one fallible writer or owned-handle operation with R-0285's failure
   contract: record partial progress, ownership and usable state on error, and
   whether retry is valid. Retain a successful operation and a controlled failure
   after partial progress, checking explicit cleanup and preventing a retry from
   silently duplicating completed work. Reuse the existing effect-simulation seam
   when applicable. State guarantees first with ordinary types and API contracts;
   formal before/after and `Result`-case specifications follow R-0473/R-0474 and
   the queued two-state mutation work. Fatal abort/OOM remains a separate boundary.
3. **Optional read/write footprints — existing contract owners.** Feed a concrete
   preservation need into R-0473/R-0474, the queue's two-state mutation contracts,
   and R-0175. Start with bounded records/fixed arrays: update a cache or counter
   while preserving configuration. Specify permitted reads and writes, with a valid
   update control and a rejected out-of-footprint access. Keep footprints distinct
   from external authority and input/output dependencies. Preserve R-0175's workload
   gate; this does not authorize a general heap or separation-logic implementation.
4. **Auditable dependency upgrades — R-0182/R-0183/R-0184/R-0372.** Review diffs must
   expose added `Network`, new foreign assumptions, and widened mutation contracts
   once supported. Include an unchanged-contract control and a transitive dependency
   change. Configured policies reject forbidden widening. Reports retain assumption
   provenance and distinguish checked declarations from audited foreign behavior.
   Reuse package/evidence infrastructure and its existing prerequisites.
5. **Explanations at failure sites — R-0137/R-0466.** Missing-capability diagnostics
   trace the source-located path from caller through helper to `Writer<Console>` and
   name the required permission. Assumption reports identify the declaration or
   conversion where trust entered, including across packages. Gate structured paths
   and valid remedies, not exact prose; do not suggest broad authority simply to
   silence an error.

**Exit:** the parameter audit and bounded library pass have reproducible results,
positive/negative controls and owned follow-ups. Items 3–5 strengthen their existing
owners' acceptance criteria; their later implementation is not required to close this
checkpoint and does not advance ahead of the strict queue's prerequisites. Capability
completeness alone does not establish that everything touched is known.

## Area: Usable QBE Backend And Independent Validation

**Reconciled 2026-10-19 (R-0113):** the Lean Compiler Backend (R-0517) is Concrete's QBE path; Concrete builds
no QBE backend. The goal, contract and tasks below are retained as historical rationale and as the source of
controls transferred to R-0517, R-0505 and the named owners; each task's **Phase** line and the
[reconciliation record](docs/project/ROADMAP_RECONCILIATION.md) give its current disposition.

Goal: add QBE as a real, user-selectable native backend for ordinary Concrete
programs and use its independent implementation to find shared lowering,
interpreter, ABI/layout, and LLVM-emission bugs before the flagships area flagship
workloads deepen the public claims. Differential validation is a required duty
of the backend, not its sole purpose. QBE need not replace LLVM or prove that
native execution preserves source semantics to be useful and genuinely usable.

Done when: a QBE backend consumes verified/cleaned Concrete SSA directly,
supports a documented useful subset with no silent LLVM fallback, and works
through the normal `build`, `run`, `test`, project, artifact-retention, and
diagnostic workflows—not only a special differential-test harness. At least one
The flagships area-class program must be buildable and runnable by a user with
`--backend qbe`. CI must also classify interpreter/LLVM/QBE disagreements over
an edge-heavy corpus by stdout, stderr, exit status, and failure class. The
audit/evidence surface labels QBE results `tested` or `backend_trusted`; this
phase cannot upgrade a source/Core proof to native-code evidence. Release-grade
target breadth, translation certificates, full debug information, optimization
policy, and long-running platform qualification remain owned by the backend-contracts area.

### Product Contract: A Backend, Not A Test Adapter

- `--backend qbe` is accepted anywhere `--backend llvm` is accepted: single
  file compilation, project builds, `run`, `test`, examples, retained
  artifacts, debug bundles, and reproducible replay commands.
- A successful QBE build produces a normal native executable with an explicit
  target/toolchain/link manifest. Users do not need to invoke the differential
  harness or understand QBE IL to run it.
- Supported programs are defined by a generated capability matrix. Unsupported
  operations, ABI shapes, targets, or builtins receive a stable diagnostic
  before partial code generation; they never fall back to LLVM silently.
- QBE owns real backend concerns: instruction selection, QBE IL legality,
  runtime-helper selection, object/link inputs, entry and test wrappers,
  platform ABI realization, artifact naming, diagnostics, and toolchain facts.
- The backend ships initially as `experimental`, which describes compatibility
  and release guarantees—not a toy implementation. Its documented supported
  subset must work end to end and remain protected by ordinary user-workflow
  tests in addition to differential tests.

### Preflight Refactors: Make The Backend Boundary Real

These are enabling refactors, not a new optimizer or a premature second middle
end. Zig's useful lesson is the stable analyzed input shared by every backend;
Concrete already has that candidate in verified/cleaned SSA. Make the boundary
explicit before duplicating LLVM-era assumptions in QBE.

Opening dependency order: provision and pin the QBE toolchain on eligible CI
hosts; extract backend-neutral `LayoutFacts`; define the typed QBE IL/fragment
model; introduce only the `CodegenInput` and driver surface needed to run the
first vertical slice; then expand QBE coverage. The numbered preflight inventory
below records all obligations, not permission to complete every abstraction
before `main -> 42`. Layout panic-to-diagnostic conversion follows the layout
split. Existing LLVM emission moves to fragments only when a shared consumer or
profiled bug 027 justifies it.

The correspondence is architectural, not literal:

| Zig role | Concrete role in the QBE backend-validation area |
| --- | --- |
| AST / ZIR | parsed syntax / checked Core |
| AIR | verified and cleaned Concrete SSA |
| isolated codegen-job input | immutable `CodegenInput` + `CodegenFacts` |
| LLVM backend | structured LLVM emitter + existing toolchain |
| self-hosted native backend | QBE emitter + pinned QBE toolchain |
| target MIR / encoder | deliberately delegated to QBE; not built in the QBE backend-validation area |
| linker job | backend-neutral Concrete driver invoking the declared linker |

Hard boundary: do not rename Concrete IRs to imitate Zig, introduce ZIR/AIR as
extra layers, or build target MIR/register allocation/object writers merely for
architectural symmetry. Copy the separation of responsibilities, immutable
job shape, coverage discipline, and gradual backend enablement. QBE provides a
useful native compilation path while letting Concrete defer target MIR,
register allocation, and object writing until owning those layers is justified.

Execution discipline: build this phase as thin executable vertical slices, not
as a backend-framework rewrite followed by a distant first program. Every slice
must end with an eligible fixture running through interpreter, LLVM, and QBE;
the same commit adds its capability row, differential observation, negative or
mutation case, and retained failure artifacts. A refactor-only slice is allowed
only when an existing LLVM behavior/IR parity gate consumes it immediately and
the next QBE slice names it as a prerequisite. Do not land unused interfaces,
generic plugin machinery, speculative target abstractions, or a second
representation merely because a future backend might need them.

First executable milestone (before broad preflight completion): compile
`fn main() -> Int { return 42; }` through cleaned SSA into typed QBE IL, validate
it, invoke the pinned QBE toolchain, assemble/link it, observe clean stdout and
exit status 42, and compare that structured observation with interpreter and
LLVM. `--emit-qbe`, `--backend qbe`, unavailable-tool diagnostics, no-fallback
behavior, and a debug bundle containing source/Core/SSA/QBE IL/tool facts must
all work for this one program. Then expand in this order unless a failing
backend or differential case forces a different dependency: branches/phi -> direct calls -> integer widths
and casts -> checked arithmetic/traps -> stack memory -> f32/f64 arithmetic,
comparisons, and checked conversions -> globals/strings -> aggregates/enums ->
indirect/extern calls -> builtins -> test mode. Each step must stay runnable;
no "all operations implemented, testing later" batch.

LLVM-refactor rule: moving existing code behind `CodegenInput`, layout facts,
the runtime manifest, or the backend driver must initially produce
byte-identical LLVM IR for the gated corpus. If byte identity is impossible or
undesirable, the slice must state the exact textual change, prove equal native
observations in debug/release modes, run LLVM validation, and add a regression
for the changed invariant. QBE progress cannot be used to waive LLVM evidence.

### Task R-0073

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** superseded by R-0497/R-0503: merge into payload-bound validated BIR artifacts.

**Objective:** Introduce an immutable `CodegenInput` wrapper around cleaned `ValidatedSSA`, plus `CodegenFacts` derived once per program: target-neutral type/layout facts, function signatures, symbol identities, module-local aliases, entry/test metadata, string/global inventory, builtin identities, runtime-helper requirements, and source-span mappings. This is not a new IR and must not rewrite instructions. Both LLVM and QBE consume the identical value; a hash of its canonical form appears in backend artifacts and differential bundles. Eliminate emitter-owned rescans that can disagree by program/module order (the bug-039 class).

### Task R-0074

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** superseded — merge into validated BIR (R-0503) and R-0517; the closed `llvm`/`qbe` backend set becomes the LLVM and LCB-native profiles.

**Objective:** Replace `Pipeline.emit : SSAProgram -> String` with a statically dispatched backend driver returning a structured `CodegenArtifact`:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0074).

### Task R-0075

**Phase:** 3

**Reconciliation (2026-10-19):** Keep the shared prepare/emit/verify/assemble/link driver; the LCB-native profile (R-0517) is its forcing consumer.

**Objective:** Consolidate the duplicated file/build/test compilation paths in `Main.lean` behind one backend-neutral driver:

 `prepare -> emit -> verify -> assemble/codegen -> link`. Backend-specific
 code supplies artifact extension, verifier/tool invocation, target flags,
 and link inputs; CLI code owns diagnostics, artifact retention, and process
 policy once. Add failure-injection tests at every boundary so a QBE error
 cannot take a less structured path than an LLVM error.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0091** (merged): merged — native test-mode entry generation joins R-0075's driver and the LCB-native profile (R-0517).
- **R-0107** (merged): duplicate of R-0075: merge bounded external-tool execution contract. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0107)
- **R-0110** (merged): merged — profile naming and evidence attribution join R-0075/R-0333; the profiles are `llvm` and the LCB-native `verified` profile. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0110)

### Task R-0076

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** superseded by R-0502: merge into single proved Layout authority.

**Objective:** Split target-neutral layout computation from LLVM rendering. Rename or move `tyToLLVM`, LLVM type-definition construction, target triple/data-layout text, and LLVM ABI spellings out of `Concrete/Check/Layout.lean`; retain one checked `LayoutFacts` source for size, alignment, field offsets, enum tag/payload placement, pass-by-pointer decisions, and canonical builtin aggregates. LLVM and QBE render those facts independently. No emitter may recompute layout from syntax or host assumptions.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0076).

### Task R-0077

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** superseded by R-0503/R-0509: merge with `rt` contracts, support inventory and trust accounting.

**Objective:** Extract a versioned `RuntimeABI`/`BuiltinCodegenManifest` from `EmitSSA.lean`: symbol names, signatures, ownership/capability class, failure/trap behavior, required helper, and backend support status. Keep builtin semantic behavior detection in the existing interpreter/compiled gate; this manifest owns codegen availability and linkage, not a second signature or semantic truth source. A new builtin must fail closed until both backend rows are classified.

### Task R-0078

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; share scheduling/reproducibility with R-0146/47.

**Objective:** Make backend emission function-local and deterministic where possible.

 Each function job receives immutable `CodegenInput`/target facts and returns
 a function fragment plus declarations/helper requirements; it may not
 mutate program-wide alias, type, string, or symbol state. Merge fragments in
 canonical symbol/module order and deduplicate declarations through typed
 keys. Start sequentially, add a serial-versus-parallel byte-identity gate,
 and only then allow parallel codegen. This follows Zig's successful isolated
 codegen-job shape without pulling in its linker architecture.
### Task R-0079

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** superseded by R-0493/R-0501/03: merge into typed operations and pass obligations.

**Objective:** Define a legalization responsibility table before QBE instruction coverage:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0079).

### Task R-0080

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** duplicate of R-0118: merge backend/profile dimensions into one inventory.

**Objective:** Add a generated backend capability matrix keyed by the constructors of `SInst`, `STerm`, `Ty`, ABI shape, builtin, target, and mode (normal/test/debug/release). Coverage derives from compiler constructors, not a hand-maintained prose list, so adding an IR operation makes the gate fail until LLVM/QBE support is declared and tested. Expose the matrix in `concrete agent features --json`, debug bundles, and the QBE backend-validation area capstone.

### Task R-0081

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** merged — emitter independence between the LLVM adapter and the LCB adapter, and shared-helper disclosure, move to R-0505/R-0517.

**Objective:** Preserve oracle independence with a review/firewall gate. QBE modules may import SSA, target-neutral layout/codegen facts, diagnostics, and semantic identities, but not LLVM AST/printer/emitter modules. LLVM and QBE may share runtime helpers only when the manifest discloses the correlation. Add an import-direction test and a source scan preventing calls from one emitter into the other.

### Architecture And Independence Rules

### Task R-0082

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR.

**Objective:** Preserve one frontend and one lowering spine:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0082).

### Task R-0085

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** duplicate of R-0076: merge through R-0076 into R-0502.

**Objective:** Keep layout facts backend-neutral. Split LLVM spelling out of `Concrete/Check/Layout.lean` where necessary, but do not create a second size/alignment calculator. Add a gate proving LLVM and QBE consume the same Concrete layout decisions while independently realizing them.

### Staged Implementation

### Task R-0086

**Phase:** 2

**Disposition (2026-10-19):** superseded — its semantic vectors become adapter test vectors for V1 (R-0511/R-0517); later widths, casts, memory and floats follow the V-slices.

**Objective:** Land a scalar smoke slice first: integer and boolean constants, arithmetic, comparisons, casts, direct calls, returns, blocks, branches, phi/copy lowering, stack slots, loads/stores, globals, string data, and the Phase 2 main wrapper. Map Concrete `i8`/`i16`/`i32`/`bool` temporaries deliberately onto QBE word operations with explicit truncation and signed/unsigned extension; map `i64`/native pointers to long and `f32`/`f64` to single/double.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0086).

### Task R-0090

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** superseded by R-0503: retire preferred-C-runtime prescription; implement Concrete `rt`, named libc FFI.

**Objective:** Port the compiler-emitted builtin/runtime surface needed by the validation corpus. Prefer a small shared C runtime for allocation, IO, traps, and other platform services when that keeps semantics explicit; keep pure scalar builtins in the emitter when independence is more valuable. Extend the builtin-semantics inventory so every builtin is one of `qbe_equal`, `qbe_runtime_helper`, or `qbe_pending`; adding a builtin without an explicit class fails CI.

### Task R-0091

**Phase:** 3

**Disposition (2026-10-19):** merged — native test-mode entry generation joins R-0075's driver and the LCB-native profile (R-0517).

**Objective:** Add QBE test-mode entry generation so `concrete test` can execute the same discovered tests and module filter under both native backends. Preserve the test runner's result accounting and output exactly; do not special-case a smaller, silently different QBE test semantics.

### Differential Validation And Failure Localization

### Task R-0092

**Phase:** 1, 2, 3, 4

**Disposition (2026-10-19):** superseded — the three-way differential is Core semantics / LLVM profile / LCB-native under R-0491.

**Objective:** Add `scripts/tests/check_qbe_backend.sh` and a reusable differential driver that runs each fixture through the source interpreter, LLVM, and QBE and compares a structured observation:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0092).

### Task R-0093

**Phase:** 1, 2, 3, 4

**Disposition (2026-10-19):** duplicate of R-0092: merge; labels are hypotheses, not proof of faulty component.

**Objective:** Classify every mismatch rather than printing a generic wrong-code failure:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0093).

### Task R-0094

**Phase:** 1, 2, 3, 4

**Disposition (2026-10-19):** superseded by R-0491: merge; retain hand/reference/metamorphic controls.

**Objective:** Require an independent expected result for cases capable of exposing a shared lowering error: a hand-authored expectation, checked-in reference model, system-tool/Python oracle, or metamorphic property. Two backends agreeing is corroboration, not correctness, because both consume the same lowered SSA.

### Task R-0095

**Phase:** 1, 2, 3, 4

**Disposition (2026-10-19):** superseded by R-0491/R-0118: merge manifests and minimum completed comparisons.

**Objective:** Build the validation corpus by semantic family, not file count:
every `SInst`, terminator, scalar width, signedness-sensitive operation,
aggregate/layout class, direct/indirect call shape, generic specialization
shape, builtin class, distinct trap identity, and main/test wrapper path needs a
manifest row. Seed it with the existing arithmetic, cast,
enum-union-layout, nested-place, callable-value, fuzz, workload, wrong-code, and
exit-model corpora. Add mutation checks proving the differential suite notices
at least one injected fault in lowering, LLVM emission, QBE emission,
interpreter semantics, layout, and builtin behavior.

### Task R-0096

**Phase:** 1

**Disposition (2026-10-19):** superseded by R-0513/R-0491: merge; remove mandatory QBE availability.

**Objective:** Run a deterministic quick differential matrix on every CI change
and a larger rotating fuzz/differential matrix on schedule. Before QBE is
available, the presubmit smoke compares interpreter and LLVM over a fixed,
replayable seed set; once QBE lands it compares all three. Job-level workflow
conditions must be honored by local workflow replay so a local “all gates” run
does not silently execute or omit a different campaign. QBE-unavailable CI is a
hard configuration failure for the QBE job, not a skip. Keep LLVM-only release
jobs intact until the backend-contracts area graduates QBE; report QBE flakes separately and
retain their artifacts.

### Phase Boundary And Graduation

### Task R-0097

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** superseded by R-0511/R-0512: merge profile admission; retain no silent fallback.

**Objective:** the QBE backend-validation area may close with a declared subset, but that subset must be honest:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0097).

### Task R-0098

**Phase:** 3, 4, 5, 6, 7

**Disposition (2026-10-19):** duplicate of R-0137: merge profile-aware workload matrix and owned exclusions.

**Objective:** Exercise the completed subset immediately in the flagships area: every new flagship workload that stays inside it runs interpreter/LLVM/QBE differential checks;

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0098).

### Task R-0102

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** superseded — the operation gap ledger becomes typed pass obligations (R-0506) and the adapter's obligations (R-0517).

**Objective:** Write a QBE semantic-gap ledger before broad coverage. For every Concrete SSA operation, record the QBE instruction sequence and the assumptions that make it equivalent: signed division overflow and divide-by-zero, shift counts, float-to-int out-of-range behavior, NaNs and signed zero, sub-word truncation/extension, uninitialized bytes, invalid pointers, alignment, aliasing, aggregate padding, unreachable paths, traps, and integer/pointer conversions. Where LLVM uses poison/undef, flags, or intrinsics that have no direct QBE analogue, lower to explicit checks/helpers or mark the operation pending; never inherit accidental host behavior as Concrete semantics.

### Task R-0103

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** superseded by R-0492/R-0505: merge; general separate compilation arrives in V6.

**Objective:** Make symbol and module composition a first-class gate. Test duplicate basenames, import aliases, colliding private names, generic specializations, extern/defined-symbol collisions, linker aliases, string and type-name escaping, multi-module declaration deduplication, visibility, and deterministic ordering. Compile both one combined QBE unit and, once supported, separate units linked together; their observations and exported symbol sets must agree.

### Task R-0104

**Phase:** 3

**Disposition (2026-10-19):** merged — deterministic emission and toolchain identity join R-0146; the LCB version and upstream lock are pinned by R-0517.

**Objective:** Add deterministic-emission and reproducibility checks: identical cleaned SSA plus an identical target/toolchain configuration must produce byte-identical QBE IL, assembly where the pinned toolchain permits it, link manifests, and backend observation records. Debug paths, temporary paths, hash-map order, host locale, parallel test scheduling, and environment variables must not perturb semantic artifacts. Record the QBE version, target, assembler, C compiler/linker, runtime-helper hash, and flags in the build/debug bundle and cache key.

### Task R-0105

**Phase:** 1, 2, 3, 4

**Disposition (2026-10-19):** superseded by R-0491/R-0504: merge debug/release and sanitizer controls.

**Objective:** Add optimization-sensitive triangulation. For every eligible fixture, compare interpreter, LLVM at the supported debug/release optimization levels, and QBE followed by the supported assembler/linker configurations.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0105).

### Task R-0106

**Phase:** 1, 2, 3

**Disposition (2026-10-19):** duplicate of R-0009/R-0491: merge new backend predicates into shared reducer/harness.

**Objective:** Extend fuzzing and reduction for the new path. Add backend-aware predicates for QBE compile crash/rejection, QBE runtime mismatch, three-way semantic split, LLVM-only mismatch, shared-compiled mismatch, optimization mismatch, and nondeterministic QBE emission. Reducers must preserve the mismatch classification and relevant target/tool versions, save the smallest reproducer plus replay command, and prove by mutation tests that a reduced case is not accepted merely because one backend stopped compiling.

### Task R-0107

**Phase:** 3

**Disposition (2026-10-19):** duplicate of R-0075: merge bounded external-tool execution contract.

**Objective:** Separate compiler defects from toolchain defects operationally. The bundle and CI report must identify the first failing boundary:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0107).

### Task R-0108

**Phase:** 3

**Disposition (2026-10-19):** duplicate of R-0416/R-0515: merge profile-labelled budgets and measurements.

**Objective:** Establish performance guardrails without making QBE win benchmarks. Track compiler wall time, peak memory where available, emitted IL/assembly/object size, final binary size, and runtime for a small stable corpus against LLVM.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0108).

### Task R-0109

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** superseded by R-0503/R-0146: merge; remove assumption helpers must be C.

**Objective:** Define runtime-helper ownership and versioning. Shared C helpers must have a narrow, documented ABI; checked-in source; deterministic build; symbol namespace; target/feature manifest; sanitizer tests; and interpreter/LLVM/ QBE semantic vectors. Decide explicitly which helpers are common semantic infrastructure and which remain independently emitted oracle logic. A helper change invalidates both backend cache keys and reruns every builtin/ ABI consumer; sharing a helper must be disclosed when it reduces oracle independence.

### Task R-0110

**Phase:** 3

**Disposition (2026-10-19):** merged — profile naming and evidence attribution join R-0075/R-0333; the profiles are `llvm` and the LCB-native `verified` profile.

**Objective:** Add release-surface and compatibility discipline even while experimental:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0110).

### Task R-0111

**Phase:** 2, 3

**Disposition (2026-10-19):** superseded by R-0511/R-0137: merge reusable exits; retire required QBE showcase.

**Objective:** Add a QBE backend-validation-area capstone artifact:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0111).

### Task R-0112

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** superseded by R-0512: merge; retirement requires compatibility and rollback, not parity alone.

**Objective:** Add a refactor-retirement gate. Once LLVM runs through `CodegenInput`, the structured backend driver, and the extracted layout/runtime facts with byte/behavior parity, delete the old direct `Pipeline.emit -> String` path, duplicated CLI compile/link branches, and emitter-local program-wide fact discovery. Do not carry two LLVM pipelines through the QBE implementation;

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0112).

### Task R-0113

**Phase:** 2, 3, 4

**Reconciliation (2026-10-19):** Kept as the QBE reconciliation decision record; decided 2026-10-19 that the Lean Compiler Backend is the QBE path (R-0517).

**Decision (2026-10-19):** the Lean Compiler Backend is Concrete's QBE path. LCB is a Lean 4 port of QBE 1.3
developed as a separate general-purpose project; Concrete consumes it through R-0517's adapter and builds
no QBE backend of its own. Each the QBE backend-validation area task carries its disposition (kept, merged with its new owner,
or retired with its useful controls transferred to R-0517 or a named owner) in its **Phase** line and the
[reconciliation record](docs/project/ROADMAP_RECONCILIATION.md). R-0296, R-0305 – R-0311, R-0331, R-0424 and the Wasm
entrance criteria were updated in the same change. This task remains the decision record and owns any
reopening, which requires measured evidence that a separate QBE path finds bugs or serves users LCB does not.

**Objective:** Keep a QBE backend-validation-area decision log tied to implementation evidence. For every vertical slice record: the supported semantic families, newly shared facts, deliberately independent logic, LLVM textual changes (if any), three-way result counts, mutations caught, performance deltas, and remaining blocker.

 If the first three executable slices show that `CodegenInput`, the typed QBE
 AST, or the driver boundary creates duplication without improving failure
 localization, revise or delete that abstraction before expanding coverage;
 the roadmap is a hypothesis, not permission to preserve a bad framework.

## Area: Flagship Depth And Examples

Goal: produce examples that outside systems engineers find impressive, not only
internally coherent.

Done when: the showcase set is capped at **three deep flagships** — one
cryptographic correctness example, one systems-security verifier, and one
temporal/stateful protocol example — each with proof/evidence strong enough to
anchor the public pitch.

Maintain only those flagships, deepen theorem coverage where it strengthens
public claims, and add examples only when they force a named surface or claim.
Everything else moves to the owning fixture, adversarial, benchmark, workload or proof-pattern
corpus rather than becoming a user-facing example.

This phase is also the external-credibility probe for the compiler/evidence
pipeline: old phase 6B's `diff-caps` artifact gives the first narrow reviewable
delta, and this phase turns that style of replayable evidence into a non-author
example or workload transcript before the larger package/release machinery
depends on it.

### Incremental Architecture Track

Execute the following migrations after the QBE boundary has supplied concrete
consumers. Each is an independently green vertical slice, never a compiler-wide
rewrite; production entry points remain runnable and LLVM output is byte-stable
unless the task records and gates an intentional delta.

These tasks execute the compiler-code half of R-0447's repository contract.
Move a module only when its semantic owner and typed boundary are established;
in the same slice, update the generated import graph and remove the corresponding
baseline exception. The target ownership is frontend/resolve/check/typed Core/
mono/SSA/backend, with shared Semantics owning operation meaning, Proof/Evidence
consuming typed artifacts, Report rendering rather than deriving facts, and
Driver orchestrating rather than becoming a second pipeline. Do not precede
these migrations with a path-only `Concrete/` reshuffle.

### Task R-0114

**Phase:** 1, 2

**Disposition (2026-10-19):** superseded by R-0492: merge; distinguish compilation-local IDs from persistent evidence identity.

**Objective:** Introduce stable semantic identities at definition boundaries.
Give modules, definitions, fields, variants, binders, monomorphized instances,
blocks, values, runtime checks, and proof subjects stable typed identities where
text names or list position still act as identity. Treat bugs 039–045 and
050/051/054/055 as the forcing family: alias capture, first-match binder/type
tables, indirect-call hijacking, generic-enum layout identity, non-injective
monomorphized names, and renamed-import callee identity. Alpha-renaming remains
the completed local fix for bug 045; this task eliminates the recurring
architectural precondition without relabeling narrow fixes as temporary.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0114).

### Task R-0115

**Phase:** 2, 3

**Reconciliation (2026-10-19):** Keep canonical facts; share indexes with R-0140.

**Objective:** Build one immutable indexed ProgramFacts store Derive module/definition indexes, signatures, layouts, builtin identities, aliases, target/runtime facts, call relationships, and source/semantic identity once per program. Passes query typed keys rather than rescanning lists or building private first-match tables. Facts are immutable within a compilation;

transformations produce a new version with an explicit invalidation boundary.

Acceptance: compare old and new derivations over the corpus before cutover,
fail on duplicate/missing keys, record deterministic hashes and construction
cost, demonstrate linear-time builders, and gate that two consumers cannot
derive conflicting values for the same fact.

### Task R-0116

**Phase:** 2

**Disposition (2026-10-19):** superseded by R-0497/R-0510/R-0514: merge; use payload binding and new IR chain.

**Objective:** Make successful compiler pass boundaries type-safe Introduce thin opaque results such as `ParsedProgram`, `ResolvedProgram`, `CheckedProgram`, `ElaboratedCore`, `CanonicalCore`, `MonomorphizedCore`, `LoweredSSA`, `ValidatedSSA`, `CleanedSSA`, and `CodegenInput`. Constructors live with the pass that earns each invariant; reports, backends, tests, and projects cannot call a later consumer with an earlier-stage value.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0116).

### Task R-0117

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; source of R-0021/R-0056 facts.

**Objective:** Replace the temporary stdlib scanner with compiler-derived interface facts Emit canonical public symbol/signature, visibility/construction rights, capabilities, allocation, ownership, failure, trust, platform, and evidence facts from the compiler's resolved/checked interface artifact. Compare them against the repaired balanced scanner until every row agrees, then delete the scanner rather than retaining two authorities.


Acceptance: nested generics/function pointers, externs, trusted impls,
overloads/renames/re-exports, private representation, duplicate names, and all
public std modules are covered; mutations of each fact class fail closed; API
snapshots, docs, audit output, incremental keys, and backend manifests consume
the same artifact.

### Task R-0118

**Phase:** 1, 2, 3, 4

**Reconciliation (2026-10-19):** Keep one inventory; integrate R-0438/R-0080 and V7 coverage.

**Objective:** Generate a self-enforcing IR/pass/consumer coverage matrix Generate, from compiler constructors and interface facts, the matrix spanning Check, CoreCheck, Interp, Lower, Verify, Cleanup, LLVM, QBE, reports, source identity, obligations/proofs, and tests. Every constructor is explicitly classified as implemented, intentionally no-op, rejected-before-here, runtime-helper, pending, or not-applicable; catch-all matches do not count.


Acceptance: adding a synthetic constructor or deleting any consumer row makes
the gate fail; every row links to its implementation and discriminating test;
the matrix is published through debug/agent artifacts; and the former manual
inventories are deleted or reduced to generated snapshots.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0080** (merged): duplicate of R-0118: merge backend/profile dimensions into one inventory.
- **R-0095** (superseded): superseded by R-0491/R-0118: merge manifests and minimum completed comparisons.

### Task R-0119

**Phase:** 3, 4, 5, 6, 7

**Reconciliation (2026-10-19):** Keep workload/graduate distinction.

**Objective:** Graduate and maintain exactly three public flagships, keeping their evidence bundles
green:

1. **`hmac_sha256`** — cryptographic correctness and exact extraction/spec-drift refinement.
2. **`secure_update_verifier`** — systems-security showcase that absorbs the parser/serializer and
   file-integrity stories: canonical manifest decoding, target identity, segment
   bounds/overlap/overflow, payload hash or HMAC, version monotonicity/rollback prevention,
   trusted-key/advisory changes, exact manifest-to-payload binding, receipt currency and R-0353
   independent verification.
3. **`protocol_state_machine`** — temporal/stateful correctness with a canonical bounded packet
   codec, explicit state transitions, invalid-input/transition refusal, bounded resources and a
   replayable safety claim.

`parse_validate`, `crypto_verify`, `fixed_capacity`, and `constant_time_tag`
are demoted from flagship status; they may remain as tests, fixtures, proof
patterns, or components absorbed into the three flagships above, but they do
not expand the public example set.

Do not add a new public flagship before R-0353 supplies an independently implemented consumer.
Deepen HMAC first. Build the secure-update evidence workflow after R-0353, add its bounded mutable
parser after two-state contracts, and add richer rollback/chunk/Merkle properties only after the
specification library supports them. Graduate the protocol state machine after the same
identity/task/verifier seam and when its state/totality requirements are supported rather than
simulated.

A public showcase is admitted only when it has a distinct user/security purpose, README and threat
model, positive and negative workflows, evidence production and independent consumption,
stale/revoked/substitution demonstrations, permanent CI ownership and a maintenance owner. A
proposed fourth showcase must replace or merge with an existing role. This cap does not limit
regression, adversarial, compatibility or performance coverage.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0131** (merged): duplicate of R-0119: merge as portfolio acceptance criterion.

### Task R-0124

**Phase:** 1, 2, 3, 4

**Reconciliation (2026-10-19):** Keep controls; replace fixed count and migrate under R-0512.

**Objective:** Keep `hmac_sha256` as the regression anchor for exact-extraction, spec-drift-tied refinement: source perturbations must make the registered proof stale, and ProofKit refactors must keep the 11 proof checks green.

### Task R-0125

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep ongoing claim alignment; retire pre-R-0004 caveat.

**Objective:** Keep the paper, website, README, and showcase manifest aligned
with HMAC's actual claim: the stored-fingerprint extracted source body refines
an independent SHA-256/HMAC spec under named assumptions and trusted backend
boundaries. Until R-0004 lands its full `ProofSubjectDigest`, do not broaden
“body-fresh” into “every signature, contract, dependency, and replay-context
fact is pinned.”

### Task R-0126

**Phase:** 2, 3, 4

**Reconciliation (2026-10-19):** Keep in Core-oriented ProofKit; require real reuse.

**Objective:** Use HMAC-derived proof patterns only after they move into ProofKit or an explicit example guide; do not let future flagships copy private `Sha256Refine` scaffolding as hidden infrastructure.

### Task R-0128

**Phase:** 3, 4, 5, 6, 7

**Reconciliation (2026-10-19):** Keep; use authoritative capability rules/certificates.

**Objective:** Satisfy the authority/capability claim through the `secure_update_verifier` flagship: a privilege-separated verification core that cannot touch files/network/processes except through named wrappers, and whose authority boundary is independently auditable.

### Task R-0129

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep bounded interop and named trust assumptions.

**Objective:** Demonstrate one trusted C/FFI boundary as part of the `secure_update_verifier` or as a standalone evidence-class fixture. Do not promote it to a fourth flagship; keep the public example set capped at three.

### Task R-0130

**Phase:** 3, 4, 5, 6, 7

**Reconciliation (2026-10-19):** Keep; normal-exit conservation, not cleanup-on-abort promise.

**Objective:** Satisfy the ownership-heavy resource claim through the `secure_update_verifier` or
`protocol_state_machine` flagship: explicit cleanup, borrow-heavy APIs, no leaks/double-use and
evidence explaining why. If a separate resource demo is needed, place it in the owning test or
benchmark corpus, not `examples/`.

### Task R-0132

**Phase:** 3

**Reconciliation (2026-10-19):** Keep byte/path/exit/error oracle coverage.

**Objective:** Add Unix-tool/protocol compatibility as a workload and fold the relevant behavior
into `secure_update_verifier` or `protocol_state_machine`: byte-preserving I/O, path/OS-string
handling, handle-relative filesystem authority, exit-code/error compatibility, ignored-result
diagnostics and oracle tests. This work does not create a fourth public flagship.

### Task R-0133

**Phase:** 3, 4, 5, 6, 7

**Reconciliation (2026-10-19):** Keep implementation early; independent consumer before public graduation.

**Objective:** Add a thin end-to-end credibility slice before the larger workload ladder, so skeptical users can replay one compelling artifact before the full old phases 5/6 and the provable-subsets and runtime-safety areas surface is complete. This is the entrance version of `protocol_state_machine`, not a temporary fourth showcase. Target:

 `examples/showcase/protocol_state_machine/`, beginning with a no-allocation
 packet-window parser/verifier (WebSocket frame decoder, TLS record-header parser,
 DNS packet parser, or HTTP/1 header parser) and growing in place. The slice must demonstrate the
 actual production-readiness claim, not a toy proof: fixed buffers, `Bytes`,
 `ByteView`, arrays, `Result`, pattern matching, explicit capabilities,
 value-model accessors / scoped borrows, no returned references, and bounded
 allocation or no allocation. It must include positive and negative corpus
 fixtures, fuzz/property-style generated cases with persisted
 counterexamples, an oracle or interpreter-vs-compiled check, one
 runtime-safety obligation, one source contract, one Lean-checked proof, one
 `proved_by_kernel_decision` discharge, and one audit/release bundle. Wire
 `scripts/tests/check_credibility_slice.sh`; the gate must compile and run
 the example, replay the proof/evidence checks, emit `--report contracts`,
 `--report obligation-ledger`, `--report caps`, `--report layout`, and
 `--report fingerprints`, persist any counterexample as a regression, and
 produce a README transcript that a non-author can run without understanding
 compiler internals. This is intentionally a vertical slice, not a new
 parallel track; it exists to validate the bet before the later 10k-line
 workload ladder. Do not start this slice until the immediate old phase 6
 blockers that directly affect parser/buffer code are either done or
 explicitly deferred with examples: the historical phase 6 array-literal
 element-inference item, match guards / OR patterns / match-on-reference item,
 and `defer`/cleanup item if the chosen slice owns resources. It also does not graduate or become
 public before R-0353 supplies the independent-consumer workflow required by R-0119.
### Task R-0134

**Phase:** 3

**Reconciliation (2026-10-19):** Keep bounded selection; align large-port priority with R-0490.

**Objective:** Add a graduated real-workload ladder. The goal is to make sure Concrete builds real things that can be checked against references, not only tiny proof demos. Each workload must name the surface or public claim it forces;

 otherwise it does not belong in this phase. Do not jump straight to multiple
 10k-line ports before the old phase 5 core slab, the stdlib area stdlib, and daily
 workflow can support them; that would mostly test missing ergonomics.
 Each accepted workload and port tier must leave behind a checked gap
 report (`WORKLOAD_REPORT.md` or manifest fields) naming: missing stdlib
 APIs, painful syntax, ownership/copy friction, error-handling friction,
 proof/contract friction, runtime trap/debug friction, capability/authority
 surprises, cold and representative edit/rebuild latency, predicted old phase 6C
 invalidation fanout, docs/tooling gaps, and which later roadmap item should absorb
 each lesson. A workload is not complete if it only builds; it must either
 feed the linear roadmap or prove that no new item is needed.
 Sequence:
 - **Main compiler repo:** keep tiny proof patterns
   (`examples/proof_patterns/` until the corpus-manifest migration assigns their final home),
   evidence-class fixtures, small real programs that gate the compiler, and the three showcase
   flagships here. These protect compiler/proof correctness and should stay close to the tests, but
   only the three portfolio programs are presented as public flagships.
 - **Medium in-repo real programs after the old phase 5 core slab and the stdlib area
   stdlib:** maintain a forcing-surface candidate list—`mini_toml`, `http_headers`, `tar_index`,
   `bytecode_vm`, `lru_cache`, and `checksum_cli`—but do not pre-commit to six new public examples.
   Select at most two active medium workloads whose surfaces are not already exercised deeply by
   `secure_update_verifier` or `protocol_state_machine`, keep them under a workload-owned path such
   as `tests/workloads/medium/`, and wire `scripts/tests/check_medium_workloads.sh` from the corpus
   manifest. `mini_toml` forces
   bytes/text/path diagnostics and parse errors; `http_headers` forces
   ignored-result diagnostics and byte-preserving parsing; `tar_index`
   forces path/OS-string boundaries, archive offsets, overflow/cast
   obligations; `bytecode_vm` forces modules, dispatch, bounded loops;
   `lru_cache` forces collections, ownership, frame facts; `checksum_cli`
   forces project model, `concrete test`, and oracle comparison. A candidate already covered by a
   flagship is absorbed there rather than duplicated. The tier exits only when the selected medium
   workloads build, run, pass
   interpreter-vs-compiled checks, carry full evidence/trust classification,
   and are covered by runtime-obligation audit.
   The selected workloads must follow the stdlib area pipeline recipe
   (`read/acquire -> parse -> validate -> transform -> emit/release`) and
   include a short transcript showing where capabilities enter and where the
   pure core starts. This borrows the teaching value of Elm's one clear app
   shape and Roc's host/application split, while keeping Concrete's
   authority/evidence boundaries explicit.
 - **Separate workload repo later** (`concrete-workloads`): use it for
   larger ports, 2k-10k line
   programs, compatibility suites, external-user programs, benchmark
   workloads, and reference/oracle data that would bloat the compiler repo.
   Do not create this repo until modules/imports, `Concrete.toml`,
   `concrete test`, bytes/text/path, collections, stdlib core APIs, and basic
   diagnostics are usable. The first external-user workload in this repo is the
   external-validation-gate trial from the cross-cutting checkpoint. The repo
   must pin a Concrete compiler/toolchain version in
   `concrete-workloads/Concrete.lock`, include `workloads.toml` with
   `name`, `size_lines`, `source_language`, `oracle`, `evidence_class`, and
   `required_concrete_version`, and be validated by release CI through
   `scripts/tests/check_external_workloads_repo.sh` so it does not silently
   rot as the language changes.
 - **Ported compatibility examples after the daily workflow is stable:**
   `wc`/`cat`/`sha256sum`-style tools, QOI image decoder, base64 library,
   INI parser, tiny glob/regex matcher, arena allocator demo, MMIO-mock
   driver, or protocol codec. Annotate forcing surfaces: glob/regex must
   stay bounded or explicitly outside `PredictableV1`; arena allocator demo
   waits for allocation-profile work that needs it; MMIO-mock driver waits
   for the freestanding area `with(Device)`/MMIO evidence decision or explicitly
   pulls that decision forward.
 - **10k-line stress ports** only after daily workflow is stable enough that
   the port tests Concrete rather than fighting missing basics. First
   accepted port set: `inih`, `cJSON`, `qoi`, `tiny_regex`, and `tiny_tar`.
   Each port must live under `concrete-workloads/ports/<name>/` and include
   `PORT.toml` with `upstream_url`, `upstream_commit`, `license`,
   `ported_subset`, `line_count`, `unsupported_features`, `oracle_command`,
   `known_trust_boundaries`, and `lessons_for_concrete`. Wire
   `scripts/tests/check_10k_ports.sh`. Tier exits only when at least one
   large port has a pinned reference/oracle suite, interpreter-vs-compiled
   differential coverage, runtime-obligation audit, and release-CI replay.
 Each workload must have a check story: oracle/reference comparison,
 interpreter-vs-compiled differential tests, runtime-obligation audit, and
 explicit evidence/trust classification for what is proved, tested, assumed,
 or trusted.

### Task R-0416

**Phase:** 3

**Reconciliation (2026-10-19):** Keep early under R-0515; remove QBE dependency.

**Objective:** Establish the first generated-code performance evidence corpus
from the flagships area flagships and workloads, before public comparison pressure turns
informal speed impressions into claims.

**Decision status:** the project affirms predictable generated-code performance
as an evidence-bearing claim; `docs/project/DECISIONS.md` records the posture.
Individual comparisons remain `performance_not_claimed` until this task supplies
their workload-scoped evidence.

Select deterministic kernels and end-to-end programs from the workload ladder,
pair each with a pinned C reference and, only where maintenance is affordable,
a Rust/Zig reference implementing the same algorithm, allocation strategy,
input validation, and I/O boundary. Record source/toolchain/profile/target,
warmup and sample policy, input hashes, wall time distribution, peak memory,
allocation count/bytes, output hash, binary size, and emitted-code size. Publish
raw samples and replay commands; never collapse them into a single “fast” badge.
Carry the matching R-0445 resource certificate and its unknown reasons beside
each workload as source-scope evidence. Correlation with measured time or
memory is an experiment, not a reinterpretation of the certificate.

Define the pull trigger for the performance area optimization work: a stable workload
shows a material, reproduced generated-code gap against its comparable
reference; a backend optimization level regresses beyond its noise budget; or a
release-facing performance claim needs evidence. Until then, report
`performance_not_claimed` rather than extrapolating from microbenchmarks.

Acceptance: committed benchmark manifests and inputs, clean-host replay,
calibrated noise/variance handling, debug/release and LLVM/QBE attribution,
result-equivalence checks before timing, a deliberately slowed mutation that
the comparison detects, and no gating threshold derived from a single sample.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0108** (merged): duplicate of R-0416/R-0515: merge profile-labelled budgets and measurements. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0108)

### Task R-0135

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep candidate checks; use release checkpoints instead of phase linearity.

**Objective:** Do not run broad examples cleanup/polish sweeps. Clean examples opportunistically when a roadmap task touches them. Improve examples only when they serve proof-link migration, `concrete prove` authoring, external validation, or a release-facing tutorial. Add an example-refresh checkpoint at every phase closure, and after every two substantial Phase-6/7 usability increments. The checkpoint is not a broad rewrite; it is a small, gate-backed audit that asks whether the three graduated flagships, minimal onboarding sequence and release-facing docs still teach the current language. It must remove stale "deferred" language for newly landed features; update examples that should

 now use the preferred surface (`if let` / `while let`, range patterns,
 future guards/OR patterns, value-model collection access, `ByteView`,
 explicit capabilities); keep legacy examples only when they intentionally
 demonstrate the old/low-level form; and record any skipped update with a
 reason. At the next release-facing checkpoint, judge rather than assume three
 concrete candidates: whether `crypto_verify`'s `Int` 0/1 API is an intentional
 C-compatible boundary or stale truthiness teaching; whether `grep` should use
 the shipped `String::to_lower`; and whether integer-tag/string-pool
 representations in JSON/TOML/Lox are workload-appropriate runtime models or
 obsolete pre-enum workarounds. Add `scripts/tests/check_example_refresh.sh`
 once the first refresh
 has enough concrete assertions; until then, each phase closure commit must
 name the examples/docs it checked and why no refresh was needed. The goal is
 to prevent tutorial/showcase drift without turning every feature into a
 repo-wide churn pass.

Apply R-0447's lifecycle classification whenever an example is touched:
`tutorial`, `showcase`, `workload`, `evidence`, or `known_hole`. A file whose
purpose is compiler regression or bug reproduction moves to the owning
test/bug corpus instead of remaining user-facing by accident. Do not bulk-move
the examples tree for taxonomy alone; ratchet the unclassified count to zero
through these refresh checkpoints, and fail new examples that omit their
lifecycle/owner record.
### Task R-0137

**Phase:** 3, 4, 5, 6, 7

**Reconciliation (2026-10-19):** Keep; split early usability checkpoint from flagship graduation.

**Objective:** Add the flagships area validation artifact: a showcase/workload dashboard that proves every flagship and graduated workload has a check story, evidence bundle, oracle or reference when appropriate, interpreter-vs-compiled coverage, property-test/counterexample-regression coverage where relevant, runtime-obligation audit, trust/assumption classification, and release-CI replay. The first external-user workload in this dashboard is the external-validation-gate trial. Also publish representative cold pipeline timings and old phase 6C shadow invalidation traces for no-op, private-body, public-interface, proof/contract, policy, and target edits; those traces are

 the workload-derived input to the incremental-driver area, not a cache implementation here.
 The external-validation trial recruits three non-authors and records every
 attempted session, including completion, abandonment point, time to first
 useful result, diagnostics encountered, and whether the proof/evidence
 discipline paid for its cost. Alpha still requires at least one non-author
 to complete a useful workflow; the other attempts are evidence even when
 they stop early and may not be omitted from the verdict. Run the trial once
 the minimum teaching path and remedy-oriented diagnostics are usable, before
 the large proof-automation investment, so external evidence can redirect
 that investment rather than merely evaluate it afterward.

 **Amended 2026-07-31: the authoring consumer is a language model, and the
 primary trial is R-0466's, not this one.** Concrete's code is written by
 models, so the ergonomic cost this trial was designed to price is largely not
 charged: a model does not mind `defer x.drop()` or `with(Console)`, because
 keystrokes are the cheapest thing it spends. Explicitness is what it cannot
 substitute for — it cannot fall back on guessing, so any rule not stated in the
 source, and any rejection that does not name its own repair, is a defect rather
 than friction. That reorders this task's own contents: the diagnostics bar below
 stops being an adoption prerequisite and becomes the primary correctness
 surface, because a diagnostic is the model's only feedback channel.

 This does **not** retire the three human sessions or the alpha bar's one
 completed non-author workflow; a diagnostic that defeats a model defeats a
 newcomer too, so R-0466 front-runs the human trial rather than replacing it,
 and it is cheap enough to run per commit where recruiting is not.

 This task owns the minimum diagnostics bar rather than depending on an
 unowned notion of “usable.” For the first-session high-friction cases—an
 unconsumed linear value (E0208), a missing capability declaration (E0240), an
 ignored `Result`, and one proof/evidence failure—the diagnostic must preserve
 the rule explanation and add source-located next actions using facts the
 compiler already knows (for example consume/return/destroy sites or the exact
 `with(Capability)` addition). A suggestion is offered only when its edit is
 semantically valid; otherwise present ranked alternatives and the command
 that explains the obligation. Gate the checked message structure and spans,
 not one brittle prose sentence. This is the bounded adoption prerequisite;
 general IDE quick-fixes remain later tooling work.

 **Measured 2026-07-31, so the bar has a starting number rather than a
 judgement.** Over `tests/invalid_programs/`, 12 of 15 rejected programs emit an
 EMPTY `hint` in `--diagnostics-json`; roughly 25 `hint :=` sites exist against
 215 `E0NNN` codes. The field is already wired end-to-end and reaches the JSON,
 so this is population work, not plumbing — and the facts are in hand at every
 empty site measured: `E0100 undeclared variable 'b'` prints no candidates
 although the resolver holds the in-scope set, and the field/variant cases
 likewise know the legal set they are choosing from. Two findings from the same
 pass belong to this bar: bug 064 (a `--query` rejection that lists the rejected
 kind as known — a message from which no repair is derivable at all, the limiting
 case of a missing next action), and `tests/invalid_programs/missing_variant.con`,
 which exercises the DELETED `#` enum qualifier and gets a generic
 `E0001 expected ;, got #` — the migration case a model trained on older material
 will hit most, with no statement that the separator was retired or that
 `Option::<i32>::Some` replaces it. That fixture has also drifted from its own
 name. Fix the class, not the instance: a gate asserting every code carries a
 non-empty next action, and a corpus grep for fixtures testing retired syntax.

 R-0445's focused non-author resource-report session counts as one recorded
 attempt here. It is an early pressure test, not a substitute for recruiting
 three non-authors or for the alpha requirement that one non-author complete a
 useful workflow.
 If the external-validation trial finds manual proof authoring too costly,
 this artifact must also run the exact narrow Task R-0167 synthesis probe
 selected by the gate, preserve its transcript and review-cost measurements,
 and record the final GO/NO decision consumed by the incremental-driver area trigger. It
 may not silently broaden that probe into the general synthesis roadmap.

 Candidate workloads for the dashboard, each admitted with the claim it
 would falsify on arrival (the envcfg→039 / httpget→045 precedent):
 a capability-firewall proxy — "no path from `main` reaches `Network`
 except through `proxy_send`" (R-0443's first real certificate customer);
 a kv store with a write-ahead log — abort-only failure semantics, where
 `defer` not running on abort forces correctness on every log prefix;
 a naive-vs-bounded regex matcher pair — the `complexity_guarded` gate
 must reject the first and accept the second, the diff being the demo;
 an editor buffer core (gap buffer + owned undo stack) — drop-glue and
 linearity composition under heavy nested non-Copy mutation (the bug 052
 shape); and a job runner / mini-shell — `with(Process)`, argv/env, the
 external-user ergonomics probe. The DNS parser is R-0244's
 pointer-chasing-bound target over R-0444's combinator core, not a separate
 entry here. Workloads are admitted one at a time, when their named claim is
 the thing that needs testing.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0098** (merged): duplicate of R-0137: merge profile-aware workload matrix and owned exclusions. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0098)
- **R-0111** (superseded): superseded by R-0511/R-0137: merge reusable exits; retire required QBE showcase. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0111)

## Area: Incremental Compiler Driver And Artifact Reuse

Goal: make unchanged compiler, proof, report, and backend work reusable without
creating a second semantic pipeline or letting stale evidence remain green.

Trigger: start implementation only after the flagships area external-validation gate
returns **GO**, old phase 6B has stable identities/dependency/invalidation contracts
and structured validation records, old phase 6C's shadow edit corpus is green, and
at least one medium the flagships area workload is large enough to choose granularity from
measurements rather than toy programs.

Its position before later external-user phases is a dependency statement, not a
reason to build caching ahead of demand. Without the measured workload and GO
record above, the pull-gated doctrine requires an explicit defer/reject decision
rather than speculative implementation.

**Conditional phase rule.** the incremental-driver area retains its measured-workload and external-validation
GO prerequisites. A NO verdict records rejection or deferral and its effect on dependent
editor/package releases. Independent proof work may proceed under the current queue;
no dependent editor or package checkpoint bypasses the required architecture.

Prerequisites: old phase 6B determinism, one composed pipeline, stable fact IDs,
dependency graph, pass locality/invalidation, and validation-record contract;
Old phase 6C telemetry/replay/shadow manifests; the flagships area workload edit traces. The
general engine must land before the proof-authoring area's proof cache so proof reuse does not
become a second private database.

Non-goals for V1: no per-token/per-expression query graph, no Datalog or
user-facing rule language, no public long-term compatibility promise for the
opaque internal cache encoding, no remote cache, and no evidence upgrade from a
cache hit or hash match. Start serial and conservatively module/function-grained;
refine granularity and add parallelism only after measurements and equivalence
gates justify them.

Done when: clean, incremental-off, incremental-on, and verify/dual-run modes
produce the same facts, diagnostics, obligations, reports, Core/SSA/LLVM
behavior, and native result; the edit matrix proves conservative bounded
invalidation; no-op and leaf edits reuse semantic and object artifacts; cache
corruption/schema/tool drift recompute or fail closed; and no cached partial or
stale artifact can fabricate a validated boundary or stronger evidence class.

Timing of query-shaping (a deliberate decision, not an omission): the
query/`CompilerSession` form below is built HERE, as part of this
validation-gated driver — it is NOT pulled forward as a pre-GO "query monad
seam." Because this whole engine is gated on the external-validation GO,
threading a query monad through the pipeline earlier would be speculative
insurance against a build that may never happen, plus churn against the
still-moving old phase 6B fact graph. The bounded call-site conversion from the
batch/eager pipeline into queries is accepted as part of this build, post-GO.
Do not re-open a "do the seam early" debate: the seam's only payoff is avoiding
that conversion, and that payoff exists only if the bet validates.

### Task R-0138

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; align artifact binding with R-0497.

**Objective:** Freeze every build revision into an immutable `ProjectInputSnapshot` before evaluating queries.

The snapshot owns canonical project/file/module discovery and exact file
bytes; manifest/lock/policy/profile/target; stdlib and dependency roots;
configured cwd/path normalization, symlink and case policy; SDK/sysroot and
runtime/startup inputs; allowlisted environment variables; and compiler,
checker, solver, clang/LLVM, linker, and other tool identities. Watch events
only trigger resnapshot/content hashing. Query evaluation may perform no
ambient filesystem/environment/tool discovery outside declared input queries,
and an edit during a build belongs to the next revision rather than mutating
the active snapshot.

Give source-bearing artifacts two identities where justified: an exact-source
revision digest for bytes/spans/diagnostics/provenance, and a canonical
semantic AST/interface digest for semantics-preserving reuse. Reusing a
semantic result across whitespace/comment/span edits requires a checked
semantic-hash equivalence rule plus a refreshed revision/span map; span-bearing
output always depends on the exact revision. Gate changed environment/std
path, SDK/sysroot/tool binary, symlink/case alias, add/delete/rename, and
edit-during-build snapshots.

### Task R-0139

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; replace old Check→Elab/SSA-only stage list with current architecture.

**Objective:** Define one typed query/artifact contract and one `CompilerSession` driver.

Add a closed `QueryKey` family and an explicit typed `CompilerDB` fact/edge
store owned by the session. Split every result into (a) a canonical hashed
payload + semantic dependency manifest and (b) non-hashed operational/view
metadata. Only normalized semantic fields, schema, subject identity, and
declared dependency digests enter `output_digest`/the dependency root.
Timings, hit/miss state, cache path/location, absolute/local paths, rendered
replay commands, redaction/display choices, and other operational metadata do
not. Exact-revision diagnostics/provenance use their own revision-bound
digest.

Keep orthogonal status dimensions: the existing proof/evidence class;
`pipeline_validation_status` (`unvalidated | compiler_validated`);
independent `certificate_status`; and `replay_status`. Cache/integrity/
certificate states may not become new proof-evidence classes. The result
envelope also carries dependency keys/digests, producer validation-record id,
fact/diagnostic ids, provenance roots, cacheability, and replay-plan identity.
Required initial families:

- project input: snapshot, loaded-file/module graph, manifest/dependencies;
- file: source/lex/parse/interface-summary/body;
- module: resolve, desugar, check, elaborate, Core canonicalize, CoreCheck;
- function or explicit SCC: calls, ownership/capabilities/effects, ProofCore,
  obligations, discharge inputs, and typed report facts;
- monomorphized instance: stable definition id + canonical type arguments,
  monomorphization and post-Mono verification;
- codegen unit: lower, raw-SSA verify, SSA cleanup, post-cleanup verify, LLVM
  emission/object from `ValidatedSSAUnit` in V1;
- project: module graph, reachability, policy aggregation, link plan, and
  release/bundle view.

If several production stages remain one composite query, its contract must
name every enclosed stage and verifier so `runFrontend`, desugar,
canonicalization, cleanup, or post-pass verification cannot disappear from
the incremental path. The backend-contracts area schema-bumps codegen inputs from validated SSA
to `ValidatedBackendIR`; the incremental-driver area does not depend on future BackendIR.

Expose `--incremental=off|on|verify` (or equivalent internal modes). Strict,
tolerant, audit, proof, test, report, and codegen operations are demand sets
over the same stage functions, never alternate pipelines. Formatting,
redaction, proof-tool, target, profile, and policy inputs invalidate only
query families that declare them. The eager `off` mode remains the oracle
and debugging path.

Add a query-effect firewall: filesystem reads/discovery, environment
variables, locale/timezone/time, tool/solver discovery and identity, target
data, policy, and external process results enter computation only through
declared input/query APIs. Dynamic dependency recording cannot see ambient
inputs read behind the query engine, so bypassing the firewall is a cache
correctness bug and fails a mutation gate.

### Task R-0140

**Phase:** 3

**Reconciliation (2026-10-19):** Merge common indexing with R-0115; retain cache-specific checks.

**Objective:** Add deterministic indexed compiler data and linear-time builders before persisting today's hot whole-program scans.

Introduce measured `ProgramIndex`/`OrderedIndex`-style structures for modules,
symbols, types, functions, layouts, obligations, stable-id lookup, reverse
imports/calls, and monomorphized users. Maps/intern tables may serve lookup;
canonically ordered arrays/lists own iteration, serialization, and output so
hash-table order never leaks. Use dense arrays where stable ids permit them
and builders/reverse accumulation for large strings and collections.
Index-heavy compiler artifacts must carry table/arena identity in the type
or validated artifact schema (`FunctionId`, `TypeId`, `BlockId`,
`ObligationId`, not raw integers). Serialized artifact bytes are hostile
until decoded and validated: cross-table IDs, stale generation IDs where
slots can be reused, and out-of-range IDs must be rejected before they become
semantic facts or cache hits.

This is profiler-driven, not a blanket ban on `List`: replace hot repeated
`find?`/`contains`, `acc ++ [x]`, growing-string append, and repeated global
scans selected by old phase 6C telemetry and bug 027's corpus. Add duplicate-key
and index-finalization verifiers, plus mutations that reintroduce a
quadratic builder and that swap two incompatible typed IDs; both must fail
their gates.

### Task R-0141

**Phase:** 3

**Reconciliation (2026-10-19):** Keep reuse contract; checker-return-artifact prerequisite belongs to R-0514.

**Objective:** Make reusable frontend artifact boundaries operational.

Add or validate durable `ParsedFile`, split `InterfaceSummary` and body
summary, `ResolvedModule`, checked module/program artifact, elaborated module,
and validated Core-module boundaries. Check may no longer return only `Unit`
if downstream reuse needs its ownership/capability/value-flow decisions; it
must produce the durable facts or typed artifact that Elab and later queries
consume. Initially keep imported body digests in the conservative dependency
envelope anywhere resolution, trait/method lookup, Check, or Elab can still
inspect bodies. A module may depend on imported **interface-only** digests—and
therefore survive a dependency-private body edit—only after the interface
completeness gate proves every consumer uses the split artifact rather than
a body side channel. Public API identity and implementation-evidence identity
remain separate. Even then, the unchanged interface permits reuse only for
importer frontend/interface queries: referenced generic bodies,
call/effect/proof facts, monomorphized instances, implementation evidence,
and codegen still depend on the applicable body digest. Gate both an unused
nongeneric private-body edit whose importer frontend remains a hit and a
referenced generic-body edit that invalidates the affected mono/object path.

Strict and tolerant/partial artifacts use different result types. A partial,
recovered, cancelled, or internal-error artifact cannot satisfy strict
codegen, proof, policy, package, or release queries. Introduce only boundaries
demonstrated useful by the workload; `CheckedModule` is a candidate name,
not permission to add a redundant typed tree.

### Task R-0142

**Phase:** 3

**Objective:** Implement the serial in-memory query engine and conservative reverse-dependency invalidation.

Record every actual dependency read while evaluating a query and maintain
reverse edges. Initial recomputation units are file, module, function/SCC,
mono instance, codegen unit, and project as listed above; do not create a
query per expression merely because an id exists. Call/import/recursive
cycles are explicit SCC or fixed-point query families, not accidental query
recursion.

V1 uses a **complete conservative dependency envelope**. A result may be
reused only when that whole envelope is unchanged. A digest authenticates
declared inputs; it cannot detect an omitted semantic dependency. Missing a
reuse opportunity is acceptable; stale reuse is not. Narrow an envelope only
after a gate or theorem demonstrates completeness for that query family.

### Task R-0143

**Phase:** 3

**Objective:** Define deterministic internal codecs before persisting an artifact family.

 Each cacheable type needs a compiler-versioned canonical encoder/decoder,
 explicit ordering and normalization, `decode(encode(x))` semantic equality,
 canonical re-encoding, malformed-input/size-limit fuzzing, and a round-trip
 boundary verifier. Summary hashes from old phase 6C are not a serialization
 format. Internal codecs may invalidate wholesale across compiler versions;
 the packages area separately owns stable public package encodings.

### Task R-0144

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; integrate payload-bound CheckedCore/CheckedMonoCore validation.

**Objective:** Add an opaque compiler-versioned local content-addressed store under `.build/concrete-cache/` (or an equivalent project-local path).

Separate the memo/action table from the content store:
`QueryKey + canonical input/dependency fingerprint -> ResultManifest +
output digest`, then `output digest -> canonical artifact bytes`. Keys include
compiler build/internal schema, semantic input/dependency digests,
invariant-set version, target/data layout, build/test profile, policy only
where consumed, and relevant proof/backend/tool identity. Add
atomic writes, locking, checksums, interrupted-write recovery, size/GC policy,
`concrete cache status --json`, and `concrete clean --cache --dry-run`.

Serialized cache entries are untrusted bytes for decoding/integrity purposes.
Loading must follow an explicit `UntrustedCached* ->
UntrustedDecodedArtifact` API after schema/digest/dependency/subject checks;
those checks yield only `cache_integrity_checked` bytes bound to the
**original** producer validation record; they cannot reconstruct
`ValidatedCore`, `ValidatedSSA`, a proof result, or a release fact. Before
The soundness-bridge and backend-contracts areas, rerun the current
semantic boundary verifier over every loaded strict Core/SSA artifact before
constructing its in-process validated token and fresh producer record. If no verifier can reconstruct
a boundary, that artifact is not safely cacheable for strict codegen, proof,
policy, package, or release use. This still trusts the compiler's verifier—it
is not independent certification.

Treat the project-local store as inside the same user's integrity boundary:
hashes/checksums detect accidental corruption and substitution relative to a
trusted action mapping, not a malicious same-user rewrite of both mapping and
result. Unknown schema/compiler/invariant versions, truncated content,
conflicting ids, or mismatched dependencies are quarantined and treated as
ordinary cache misses whenever source inputs allow recomputation. Fail only
in an explicit artifact-only/offline operation where recomputation is
impossible. A coordinated malicious rewrite of action mapping, result,
manifest, and internal hashes is outside the local-cache threat model until
an authenticated expected action/root exists; tests must not misdescribe
internal consistency checks as detecting it.

### Task R-0145

**Phase:** 3, 4, 5

**Reconciliation (2026-10-19):** Keep; migrate ProofCore references and preserve R-0155 ownership.

**Objective:** Turn proof, obligation, and report work into queries rather than cached rendered strings.

Cache per-function ProofCore extraction, call/SCC classification,
stable normalized obligations, typed proof inputs, and typed evidence/report
rows whose schemas already exist. Project
reports are deterministic views/joins over those typed facts. A cache hit
preserves the original proof/evidence class and replay provenance; pipeline
validation remains a separate dimension. It never turns `tested`,
`solver_trusted`, `assumed`, or `trusted` into proof. Lean, ProofKit, solver,
synthesis-model, toolchain, spec, theorem, dependency-fact, and policy
identities affect only the query families that consume them.

Do not restore `proved_by_lean`, `proved_by_kernel_decision`, or
`kernel_replayed` from an untrusted cached verdict alone. Cache proof inputs,
normalized obligations, generated artifacts, exact proof-workspace/import
dependency digests, and replay plans; replay the
kernel before emitting the current strong status unless a separately
authenticated replay receipt is explicitly admitted by policy and named in
the TCB. Solver/LLM results retain their weaker class and replay provenance.

The incremental-driver area supplies the query namespace and store; Task R-0155 owns the
shared proof-artifact schema and activation of reusable Lean/solver verdicts,
rather than creating a second `.build/concrete-proof-cache/` database. Cache
only successful strict non-verdict artifacts first. Negative/tolerant
diagnostics, filesystem discovery, environment-dependent operations, solver
results, and cancelled work need explicit cache contracts before reuse.

### Task R-0146

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep LLVM increment; native reuse must retain image/certificate bindings.

**Objective:** Add stable codegen units, an object cache, and deterministic relinking.

First define `ToolchainIdentityV1` / `BackendInvocationV1`: exact clang/linker
executable digests (not version strings alone), SDK/sysroot, allowlisted
environment, target/data layout and ABI, optimization/debug/sanitizer flags,
runtime/startup objects, and every effective tool flag. These inputs come
from `ProjectInputSnapshot` and participate in action keys.

Then add a deterministic `ProgramCodegenPlan` / global ABI-symbol index that
owns reachability, symbol naming/collisions, whole-program signature/layout
facts, builtin/helper/type universe, global/string ownership, externs,
monomorphized-instance ownership, unit imports/exports, and common-versus-test
partitioning. The current whole-program Mono/SSA/emitter decisions must become
explicit plan inputs before a module can emit safely in isolation. V1 unit
input is `ValidatedSSAUnit`; the backend-contracts area migrates it to validated BackendIR.
Start with one unit per source module plus stable runtime/test-harness units
only if the partitionability gate passes. Later subdivision uses deterministic
stable-id buckets or explicit mono SCCs, never traversal-order chunks.

The object action key includes validated-SSA unit + codegen-plan digest,
backend schema, and `BackendInvocationV1`. The link action key includes
canonically ordered object digests plus linker/startup/runtime identities.
Cache/materialize the final linked artifact by link key if a retained-output
warm no-op is expected to skip the linker; otherwise a missing final artifact
may relink while still reusing every object.

Gate isolated unit emission, duplicate/orphan mono ownership, missing/
colliding symbols, common/test partitioning, target/toolchain invalidation,
corrupt local objects/action mappings, and requested-output materialization.
A leaf edit recompiles only its measured affected units plus link; a retained
warm no-op invokes neither clang nor linker. The backend-contracts area later adds independent
BackendIR relation checking. Before a native/object validator, an object hit
remains backend/compiler-trusted and its hash proves identity only relative
to the trusted local action mapping—not that LLVM produced correct code.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0104** (merged): merged — deterministic emission and toolchain identity join R-0146; the LCB version and upstream lock are pinned by R-0517.
- **R-0109** (superseded): superseded by R-0503/R-0146: merge; remove assumption helpers must be C.

### Task R-0147

**Phase:** 3

**Reconciliation (2026-10-19):** Keep editor prerequisite; parallel scheduling follows serial correctness.

**Objective:** Add a long-lived service/watch surface over the same session, then deterministic independent-query scheduling.

The initial engine is serial. Once serial reuse is correct, allow independent
file/module/codegen-unit queries to execute in parallel with cancellation and
bounded resources. Serial and parallel **Concrete-owned canonical**
artifacts/facts must be byte- and schema-equivalent; external LLVM/object
nondeterminism follows the explicit the backend-contracts area policy and must not change
compiler facts or native behavior. Cancellation or a worker crash must not
commit partial cache state. Canonically sort diagnostics/events whose order
is otherwise scheduling-dependent. The internal service exists to preserve a
session across CLI or scripted edits; the editor area's LSP must wrap this exact
session rather than a batch `runFrontend` or editor-only cache.

### Task R-0148

**Phase:** 3

**Objective:** Add cache/query observability and a conservative rollout.

Extend pipeline events with `executed | memory_hit | disk_hit | invalidated |
uncacheable`, reason, dependency ids, bytes, time, validation-record id, and
the separate evidence/pipeline-validation/certificate/replay statuses. Roll
out `shadow -> opt-in -> CI verify/dual-run -> default-on`
only after each stage's corpus is green. `verify` recomputes selected hits and
compares canonical results; any false hit disables that query family and
persists a minimized regression. Audit/why/diff output may explain reuse but
may not cite reuse as correctness evidence.

### Task R-0149

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; move validation program to fixture/workload home.

**Objective:** Add the incremental-driver area validation artifact.

 Create `examples/incremental_driver/`,
 `tests/perf/compiler_pipeline/incremental_driver/edits.json`,
 `scripts/tests/check_incremental_driver.sh`,
 `scripts/tests/check_codegen_object_cache.sh`,
 `scripts/tests/check_incremental_failure_recovery.sh`, and the umbrella
 `scripts/tests/check_phase8_5_incremental.sh`.

 Cover no-op; whitespace/comment; private leaf body; public
 signature/type/capability/contract; dependency private body versus
 interface; generic body/new instantiation; proof link/theorem/spec; policy;
 target/profile; file add/delete/rename; syntax/type error then repair;
 cancellation; cache truncation/corruption; schema/compiler/checker/tool
 drift; coordinated action-map/artifact/dependency rewrite (recorded as
 outside the unauthenticated local-cache threat model, never falsely
 "detected"); object
 substitution; ambient-input firewall bypass; and serial/parallel modes. Compare
 incremental-off/on/verify facts, diagnostics, obligations, reports,
 Concrete-owned canonical Core/SSA/LLVM and manifests byte-for-byte; compare
 normalized external-object metadata and native behavior unless the pinned
 toolchain is separately proved reproducible. Assert query execution sets,
 named unaffected sentinel modules/functions/objects that must remain hits,
 affected outputs that must change, and maximum query-family execution counts
 per edit—not only wall time or the tautology "declared dependencies rebuilt."
 On the flagships area large workload, a warm no-op must execute zero **producer
 recomputation** beyond required decode/integrity/boundary-validation queries,
 and a leaf edit must rebuild only declared reverse
 dependencies within the stated count bound. Publish observed/baseline-derived
 no-op and leaf-edit speedups (20x/5x are investigation targets, not phase
 gates or public claims); the release-bar area owns platform-specific release performance
 budgets.

## Area: Proof Authoring And Automation

Goal: make flagship proofs a repeatable engineering workflow, not a collection
of one-off `simp` scripts.

Done when: new flagship proofs can start from useful generated stubs, standard
lemmas, and actionable failure diagnostics.

This phase owns the proof-cost probe from the roadmap spine: before assuming the
proof discipline scales, measure manual proof effort on real flagships, then
measure review effort for the LLM-guided synthesis loop if manual authoring is
too expensive.

### Task R-0150

**Phase:** 3, 4, 7

**Reconciliation (2026-10-19):** Keep; start measurements before automation.

**Objective:** Instrument the flagships with PROOF-EFFORT AND COVERAGE TELEMETRY
before investing in automation, so the external-validation gate's “was the
proof discipline worth the cost?” question has data instead of anecdotes.

**First shared consumer (2026-10-04): R-0486.** Begin measurements with its first
bounded component, before proof scaffolding or automation is expanded. Retain source
annotation size, manually authored proof/bridge work, time to actionable feedback,
repair time after each controlled change, and compiler-author interventions. Record
the revision, toolchain and measurement conditions; establish targets from the observed
baseline rather than inventing speedup claims. The existing HMAC and coverage series
below remain useful comparisons, not prerequisites to recording this workload.

Measure both sides of the investment:

- **cost:** per proved function, Lean proof lines, tactic depth,
  solver/`bv_decide` time, replay time, and the source complexity covered
  (loops, branches, width casts); and
- **coverage:** the fraction of the existing eligibility report's pure-Core
  denominator closed without a hand-written Lean bridge, classified by
  obligation class, discharge route (`structural`, `omega`, `bv_decide`,
  linked Lean, or unsupported), and stable blocker reason, including the share
  blocked by absent operational or fixed-width semantics.

Never invent a telemetry-only definition of “eligible”; use the compiler's
canonical eligibility facts so the denominator cannot be improved by excluding
hard cases. Seed the coverage side from R-0004's honest post-migration baseline.
Publish a small table for the HMAC flagship and the `constant_time_tag` evidence workload, refresh
it per relevant portfolio release, and retain comparable history rather than replacing the previous
number. R-0438's generated claim records eventually own the durable trend; this
task owns the measurement definition and initial series. The result may justify
changing the later R-0169/R-0170 investment, but it must not move automation
ahead of proof-subject identity, exact semantic coverage, or the R-0137
external-user trial. Cheap to collect now; impossible to reconstruct later.

### Task R-0151

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; remove dependence on a future PExpr layering split.

**Objective:** Move example Lean proofs physically next to their Concrete examples. Target layout:

`examples/<name>/src/main.con`, `examples/<name>/proofs/Proofs.lean`,
`examples/<name>/snapshot/...`. Configure Lake/module discovery so these
files are importable by the proof checker (for example as
`Examples.<Name>.Proofs` or an equivalent stable namespace), update
`Concrete.lean`/proof umbrellas so `check-proofs` can see them, retarget
source `#[proof_by]` / `#[ensures_proof]` links, and update
`concrete prove --emit-lean` / `--workspace` to prefer the
`examples/<name>/proofs/` destination. Update the proof-namespace guard to
reject new example proof modules under `Concrete/Examples/`. Pilot on one
small example, then migrate all current example proof modules. Keep registered
spec PExprs in `Concrete.Proof` until the later `ProofCore` /
`SpecRegistry` split. This executes R-0447's proof-ownership rule: after each
example migrates, delete its old top-level `proofs/Examples` copy and make the
layout manifest reject a second owner. Reusable lemmas stay in ProofKit;
example-specific theorem code, fixtures, snapshots, and replay metadata stay
with the example.
### Task R-0153

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; names remain display conventions, not identity.

**Objective:** Define and document stable theorem naming conventions in tool output:

 `<fn>_refines_spec`, `<fn>_<obligation>_proved`,
 `<fn>_loop_<name>_preserves`, and
 `<fn>_call_<callee>_discharges_requires`. `concrete prove` should suggest
 these names instead of leaving agents to invent them.
### Task R-0154

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep, incrementally with each authoring surface.

**Objective:** Add CI gates for the agent-facing proof surfaces: snapshot representative `--json` output, validate schema versioning, ensure generated Lean stubs parse/check up to the intended placeholder boundary, assert replay JSON reports the same statuses as human replay, and assert proof-check JSON maps a failing Lean proof back to the intended obligation id.

### Task R-0155

**Phase:** 2, 5

**Reconciliation (2026-10-19):** Keep; derive from canonical task/evidence objects.

**Objective:** Define one proof artifact schema shared by obligations, minimization, `--why`, generated stubs, synthesis attempts, repair plans, stale-proof reports, proof-cache entries, and replay bundles.


 Deliverable: `docs/PROOF_ARTIFACT_SCHEMA.md` plus a schema-versioned JSON
 fixture directory. Required common fields: `schema_version`,
 `obligation_id`, source fingerprint, source span, policy id, evidence
 class, replay command, tool versions, assumptions/trust deltas, and final
 status.

 Done when `--emit-lean`, `--minimize`, `--why`, `--synthesize`,
 `--repair-plan`, proof-cache status, and stale-proof reports all emit this
 shared envelope, and a gate fails if any command invents a private JSON
 dialect.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0241** (merged): duplicate of R-0155/R-0178: merge obligation payload into those canonical schemas.

### Task R-0156

**Phase:** 3, 4

**Objective:** Add human docs only after the binary path exists:

 `docs/AGENT_PROOF_AUTHORING.md` and an optional repo-root `AGENTS.md`
 should summarize the binary workflow and point to the ProofKit guide, but
 they must not be the source of truth for agents using only an installed
 binary.
### Task R-0157

**Phase:** 3, 4, 6

**Reconciliation (2026-10-19):** Keep as continuing acceptance requirement.

**Objective:** Keep AI/agent assurance guidance aligned with the implemented proof surface.

 `docs/verification/SPARK_CLASS_ASSURANCE.md` is the current design-target guide for
 agents: it tells Claude/Codex-style tools which assurance annotations are
 implemented today, which are future-only, and which replay commands must
 validate a claim. When loop invariants, frame/dependency contracts,
 ghost/spec code, or package evidence land, update this guide in the same
 commit as the feature and add at least one agent-facing example of the
 intended suggestion pattern.
### Task R-0160

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; target Core loops and clock semantics.

**Objective:** Build reusable lemmas for loop-carried state and `while_step`.

### Task R-0161

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep, driven by admitted widths/operators.

**Objective:** Build reusable lemmas for BitVec operations used by flagships.

### Task R-0163

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; replace PExpr/FnTable output with exact Core statements.

**Objective:** Upgrade generated proof stubs for real shapes: arrays, structs, enums, fixed buffers, Result/Option, loops, source contracts, and refinement composition. Stubs should emit spec target, `PExpr` body, FnTable skeleton, expected theorem statement, common imports/tactics, and TODO blocks for loop invariants. These items enrich what `--emit-lean` produces; they do not introduce a second stub generator. Provide a friendly alias such as `concrete prove <file> <fn> --stub` only if it is the same artifact path and schema as `--emit-lean`, not a second proof surface.

### Task R-0164

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; replace manually assembled FnTables.

**Objective:** Add generated composition scaffolds: FnTable entries, call lemmas, callee refinement dependencies, and composed theorem skeletons.

### Task R-0166

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; share persistence with R-0191.

**Objective:** Improve failed-proof diagnostics after `--json`, failed artifacts, and `--minimize` exist: classify common failures into actionable categories such as missing callee theorem, stale source link, missing table entry, failed arithmetic bridge, insufficient frame fact, and spec/extraction mismatch. Add `concrete prove <file> <fn> --why <obligation_id>` (or an equivalent `--show-obligation --why` form) to explain why the obligation exists, which source span generated it, which facts are in scope, what evidence classes are allowed, and why automation did not close it.

 Diagnostics should point to the already-generated artifact or next action
 instead of introducing another parallel proof surface. Where a failed proof,
 failed contract, or solver counterexample has concrete inputs or a finite
 witness, emit a runnable `.con` counterexample fixture plus a replay command.
 This is the Dafny/SPARK/F* lesson in Concrete's terms: proof failure should
 feel like debugging a minimized program, not reading a wall of theorem-state
 text. The fixture is evidence of failure only; it must never upgrade a claim
 to proof.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0206** (merged): duplicate of R-0166: merge into R-0166.
- **R-0253** (merged): duplicate of R-0166: merge into R-0166.

### Task R-0171

**Phase:** 1, 4

**Disposition (2026-10-19):** superseded by R-0494: merge into R-0494; preserve route-specific attacks.

**Objective:** Add an automation trust-upgrade firewall for every proof automation path.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0171).

### Task R-0177

**Phase:** 2, 4, 6

**Reconciliation (2026-10-19):** Keep as cumulative checkpoints, not one late capstone.

**Objective:** Add the proof-authoring area validation artifact: a proof-authoring project that exercises `--json`, `--show-obligation`, `--emit-lean`, `--emit-artifacts`, `--workspace`, `--check`, `--nearest-lemmas`, `--minimize`, and source-linked proof attachment across straight-line, array update, loop copy, fold, composition, bounded quantified specs, ghost, stale, missing, partial, and repair cases. The gate must typecheck generated stubs, reject any `proof-registry.json`, and verify that failing Lean proofs map back to stable obligation ids. If the incremental-driver area completed, it must also exercise proof-query memory/disk hits, stale dependency invalidation, proof-tool/policy drift,

corrupt-entry recovery, and cache-off/on/verify equivalence without treating
a hit as proof evidence. If the incremental-driver area was rejected/deferred, the validation
artifact runs eagerly and does not create a private proof cache.

## Area: Audit Commands And Review Artifacts

Goal: let a reviewer answer "what can this program do, what is proved, what is
assumed, and what changed?" without reading compiler internals.

Done when: `concrete audit`, semantic diff, and an artifact viewer cover the
three graduated flagships and one package-scale workload.

### Task R-0180

**Phase:** 3, 5

**Objective:** Add `concrete explain <function>`: capabilities, proof status,
assumptions, obligations, trusted callees, direct and transitively reachable
runtime-trap kinds, evidence dimensions, and why each status is what it is.
Trap explanations reuse R-0178’s site/edge facts and show the path or
completeness boundary rather than deriving reachability again.

### Task R-0181

**Phase:** 3, 5

**Reconciliation (2026-10-19):** Keep; use shared conservative call facts.

**Objective:** Add `concrete why <capability>`: explain why a function needs `File`, `Network`, `Alloc`, `Unsafe`, etc., including transitive call chains.

### Task R-0184

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep; separate policy acceptance from compiler verification.

**Objective:** Add early **capability budget files** before full package facts exist.

 Deliverable: a project-local policy file (for example
 `concrete.policy.toml`) with a minimal V1 surface:
 `forbidden_caps = ["Network", "Unsafe"]`, `allowed_caps = ["Alloc"]`, and
 optional `forbid_trusted = true`.

 Done when `concrete check` / `concrete audit` fails a fixture that widens
 beyond the budget, reports the exact function/import that caused the
 widening, and reuses the same fact vocabulary as the packages area import fact
 constraints so the policy graduates cleanly to dependency/package
 boundaries.

The budget verdict must retain a canonical reachability witness: stable
function identities, direct edges, conservative possible-target sets for
indirect calls, the entry roots, the path introducing each reachable
capability, and every trusted/extern boundary where completeness stops.
Compiler-produced witnesses remain `compiler_validated`; they do not become
proof merely because the same compiler generated both the graph and verdict.
R-0443 later makes the narrow whole-program authority property independently
checkable from the canonical artifact.

### Task R-0441

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep; documentation repair is already complete.

**Objective:** Decide how `with(Std)` behaves under an authority budget. The
canonical-list documentation half is complete.

**Decision status:** pending. Current profiles continue to accept `with(Std)`;
the recommendation below is not a shipped restriction. Ratification must choose
between rejecting the alias in high-integrity profiles and requiring an exact
transitive-use budget before any gate changes program acceptance.

`with(Std)` expands at parse time to every standard capability except `Unsafe`
(`Concrete/Frontend/Parser.lean` "Expand \"Std\" to the full set";
`docs/language/SAFETY.md:61`). It is used in `docs/language/DEFER.md`, `docs/language/SAFETY.md`, and
several `adversarial_cap_*` fixtures. The canonical identity docs now list
`Std` beside the nine capabilities and accurately mark it as an alias over the
eight non-`Unsafe` capabilities. That documentation repair does not decide the
policy below.

The audit consequence is specific and it is why this sits beside the budget task.
Adding `Network` or `Process` use inside a `with(Std)` function produces **no
declaration widening** — the ceiling was already maximal — so a budget checked
against declared capabilities measures something that cannot change. It can still
appear in a *reachable-authority* diff, since that is a call-graph fact. So the
precise statement is: declaration-ceiling budgets over `Std` are meaningless,
reachable-authority budgets remain meaningful, and the two must not be conflated
in reports.

Decide one rule and gate it: either audited profiles reject broad aliases
outright, or a `Std` declaration must carry an exact transitive-use budget that
the compiler checks against reachable authority, so the alias stays a
convenience for scripts while audited code states what it actually reaches.
Related and worth stating in the same decision: a declared capability is an
authority *ceiling*, not evidence of use, so reports should distinguish declared
from reachable authority regardless of how the ceiling was spelled.

The list repair is complete; the task remains open for the audited-profile
policy and its gate.

Gate: a fixture where `Std`-declared code gains `Network` use, proving both
surfaces at once — declared ceiling unchanged, reachable authority changed — and
failing under the chosen rule; the same code under an explicit cap set (must
pass); a declared-vs-reachable report fixture; and a mutation removing the alias
leg from whichever check is chosen. For high-integrity profiles, rejecting `Std`
outright is likely the cleanest rule and should be the default proposal.

### Task R-0189

**Phase:** 3, 5

**Objective:** Make `tested_by_oracle` evidence structured and diffable:

 - add an oracle manifest naming reference, seeds, case count, input model,
   comparison mode, and coverage kind;
 - split cases into boundary, known-vector, random, adversarial, and
   regression buckets;
 - report case counts, seeds, reference identity, comparison mode, and
   `not_proof` evidence level in audit output;
 - save failing cases as reproducible fixtures;
 - optionally cross-check reference, interpreter, and compiled binary;
 - support metamorphic tests where no complete reference exists;
 - flag oracle evidence weakening in `concrete diff` when cases, seeds,
   reference, comparison mode, or boundary coverage shrink.
### Task R-0191

**Phase:** 1, 3, 5

**Reconciliation (2026-10-19):** Keep; minimum persistence joins R-0491 immediately.

**Objective:** Add counterexample-to-regression persistence for obligation witnesses from SMT, property tests, oracle failures, fuzzers, differential mismatches, runtime traps, proof failures, and future fuzzed contracts. Command surface: `concrete counterexample save <obligation_id> --out tests/counterexamples/<name>.con` plus JSON mode. The saved fixture must include source inputs, expected failing obligation id, expected status (`counterexample`, `tested_by_property_failure`, `oracle_failure`, etc.), replay command, and the original tool provenance. Wire `scripts/tests/check_counterexample_regressions.sh` with one SMT overflow

 witness, one property-test contract witness, one oracle mismatch, one
 fuzzer/differential mismatch, one runtime-trap witness, and one proof
 failure/minimized obligation witness. The
 gate must fail if a future refactor turns the same counterexample into
 `proved_*` without changing the checked fixture expectation.
### Task R-0193

**Phase:** 2, 5

**Objective:** Add spec provenance and adequacy facts to audit/release bundles: spec name, source standard or paper, independent reference if any, test-vector set, reviewer, review date, assumptions, and evidence class (`spec_trusted`, `spec_reviewed`, `tested_by_oracle`, or future `spec_refines_standard`). Do not let a source-to-spec proof imply the spec itself is adequate.

### Task R-0194

**Phase:** 1, 5

**Disposition (2026-10-19):** duplicate of R-0209: merge into R-0209.

**Objective:** Add evidence-level monotonicity checks to audit/diff output.

### Task R-0198

**Phase:** 1, 5

**Disposition (2026-10-19):** duplicate of R-0178: merge schema work into R-0178; verdict enforcement into R-0494.

**Objective:** Keep audit, contracts, obligations, assumptions, policies, manifests, and proof-status output on one shared vocabulary. Do not let each artifact grow its own mini-language for the same evidence classes.

### Task R-0199

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep as release acceptance.

**Objective:** Keep public-facing docs and website copy grounded in the same evidence vocabulary. Use `docs/project/WHY_CONCRETE.md` as the source for a C/Rust-oriented "why this exists" page: small systems code, explicit authority, visible evidence classes, spec-drift-tied proofs, named trust boundaries, and what Concrete deliberately avoids. The website should show the end goal and the current honest status, not catchy slogans or one-badge proof claims.

## Area: Proof Status And Trust Gates

Goal: make every green proof/evidence status precise, traceable, and hard to
misread.

Done when: all existing production proof specs are directly and transitively
FnTable-complete, proof dependencies and provenance are visible, assumptions
and trust boundaries have lifecycle reports, and weaker evidence cannot appear
under a stronger badge.

### Task R-0202

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0495/R-0512: merge dependency-closure requirement into Core migration; maintain legacy checks until retirement.

**Objective:** Add transitive FnTable completeness: walk registered spec call graphs, not only direct call sites, and fail or flag missing callees before theorem authors hit confusing `none` evaluations.

### Task R-0203

**Phase:** 1, 2, 5

**Reconciliation (2026-10-19):** Keep; use canonical dependencies and R-0494.

**Objective:** Add proof dependency tracking: if proof/spec for `f` depends on `g`, drift in `g` must affect `f`'s proof/evidence status or surface an explicit dependency warning.

### Task R-0204

**Phase:** 1

**Disposition (2026-10-19):** superseded by R-0494: merge into R-0494.

**Objective:** Add per-obligation proof/evidence status. Function-level status is only a summary; padding, block fold, digest serialization, final composition, contract clauses, runtime obligations, oracle checks, and assumptions each carry their own evidence class.

### Task R-0205

**Phase:** 1

**Disposition (2026-10-19):** superseded by R-0494: merge into R-0494’s evidence-class controls.

**Objective:** Keep oracle-tested evidence separate from Lean/spec refinement. Oracles are implementation sanity and regression evidence, not proof completion.

### Task R-0206

**Phase:** 3, 4

**Disposition (2026-10-19):** duplicate of R-0166: merge into R-0166.

**Objective:** Add proof debugging output for failed/stale proofs: extracted spec, current fingerprint, registered fingerprint, expected theorem shape, missing callee facts, likely missing lemma class.

### Task R-0207

**Phase:** 2, 5

**Reconciliation (2026-10-19):** Keep; canonical task owns shared fields.

**Objective:** Add evidence provenance to proof/evidence facts: source file/span, compiler commit, theorem name, spec name, policy file, assumption file, tool version, and replay command where available.

### Task R-0208

**Phase:** 1

**Disposition (2026-10-19):** done — closed 2026-10-13 at `2b570c5a`; the body is retained for its accepted limits, now owned by R-0532.

**Objective:** Add tool-version drift checks: proof/evidence facts record the Lean version, Concrete compiler commit, ProofKit hash, extraction version, decision procedure version, and solver version where relevant. A toolchain bump marks affected evidence `needs_recheck` until replayed; old green badges are never silently reused across a proof-tool upgrade.

This task also owns a machine-readable checker-advisory register. Key entries by checker/kernel
implementation, exact version or commit, rule-set digest and upstream advisory/reproducer id; do
not rely on an unverified CVE label or a prose version range. Each entry records
`accepted | affected | revoked | unknown`, the affected evidence classes and fragments, replacement
versions, replay requirements and claim indexes. Release policy must refuse or downgrade evidence
whose checker state is affected, revoked or unknown. A changed advisory register is evidence input
and therefore changes the policy result even when the proof artifact is byte-identical.

**CLOSED 2026-10-13 at `2b570c5a`.** Push CI
[38327442096](https://github.com/unbalancedparentheses/concrete2/actions/runs/38327442096) green;
campaign [38327460557](https://github.com/unbalancedparentheses/concrete2/actions/runs/38327460557):
116 discovered = killed, 0 invalid, 0 survived, 0 could-not-apply, `completed=1`, `integrity_ok=1`,
`qualified=1`, the local aggregator re-run identical to CI. The Linux artifact `checker-acceptance-Linux`
from that push run is the self-contained evidence for the registered build `3eb21141…` on the closure
commit: register `registered`, identity 15/0, kernel re-check 10/0 and policy 24/0 with logs tied to
that identity, the accepted-build consumer re-check leg (5 of 5 only after re-check), axiom inventory
7/0, claim inventory IDENTICAL; the Lean #14576 reproducer is rejected there too. The earlier artifact
and control logs stay as supporting evidence. Criteria → evidence: `docs/verification/R-0208-closure.md`.
Still open by decision: native-trust receipts (withheld), limited rollback protection, correlated
not authenticated checker self-report.

Fragment-scoped narrowing is tested only after the broad fail-closed path. Surface VIR lacking the
affected construct is not sufficient because attached metaprograms can add unrelated hostile
declarations; an accepted narrowing needs an independently checked retained-environment footprint.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0208).

### Task R-0209

**Phase:** 1, 5

**Disposition (2026-10-19):** superseded by R-0494: merge into R-0494; retain audit/diff tests.

**Objective:** Add evidence monotonicity checks: a refactor cannot silently present a weaker claim as if it were still stronger (`proved` cannot degrade to `reported` while retaining the same badge/summary).

### Task R-0210

**Phase:** 2, 5, 7

**Objective:** Add assumption lifecycle checks: every assumption has an owner, scope, rationale, review date, affected claims, and a diff gate when it widens.

### Task R-0211

**Phase:** 2, 4, 5

**Disposition (2026-10-19):** superseded by R-0509/S16: merge into R-0509; retain audit command acceptance.

**Objective:** Add a trust-boundary inventory report: all `trusted`, `Unsafe`, extern, backend, runtime, and target assumptions in one machine-readable list.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0211).

### Task R-0213

**Phase:** 1, 2, 5

**Reconciliation (2026-10-19):** Keep; specify decidable scope and unknown outcomes. Vacuity results form an explicit algebra (`nonvacuous` / `vacuous` / `unknown`) with timeout behaviour, policy treatment and witness scope; the admitted decision fragment is stated. A valid implication and a useful specification are different judgments.

**Objective:** Add vacuity gates to proof status: `proved` summaries must be downgraded or blocked when the proof depends on an unsatisfiable precondition, contradictory assumptions, unreachable code path, or invariant `false`.

### Task R-0215

**Phase:** 1, 4

**Reconciliation (2026-10-19):** Merge common infrastructure into R-0491/R-0498; retain workload mutations.

**Objective:** Add spec/proof mutation testing to prove evidence is load-bearing. Command surface: `concrete mutate-evidence --target <example> --json` creates controlled mutants: change a function body under a proof link, weaken or delete an `#[ensures]` clause, strengthen an impossible `#[requires]`, remove a loop invariant, alter a spec PExpr/table entry, change a theorem name, and perturb a trusted assumption. Expected outcomes must be explicit:

 stale, missing, vacuous, partial, failed proof, widened trust, or unchanged
 only when the mutation is semantically irrelevant and justified. Wire
 `scripts/tests/check_evidence_mutation.sh` over `hmac_sha256`,
 `constant_time_tag`, `proof_patterns`, and one contract-negative example.
 The gate must fail if a mutated proof/spec still reports the original green
 evidence class without an allowed explanation. This is evidence about the
 evidence: proofs must constrain the implementation, not merely decorate it.
### Task R-0216

**Phase:** 2, 5

**Reconciliation (2026-10-19):** Keep; R-0208’s one upgrade does not supply the general command.

**Objective:** Add proof-corpus migration across toolchain upgrades, the active half of the §7 drift story. Detection marks evidence `needs_recheck`; migration must turn a bumped corpus green again without hand-walking every obligation. Command surface: `concrete prove --recheck-corpus [--json]` re-runs every linked proof/evidence check under the current toolchain and triages each into `still_proved`, `replayed_clean`, `broke_needs_repair`, or `unavailable_dependency`; `concrete prove --recheck <obligation_id>` does one.

 Pin the external Lean proof-library surface the corpus depends on (Lean
 stdlib / Batteries / Mathlib lemmas and tactics actually cited) in a checked
 `proofs/lean-deps.lock` with versions, so a renamed or relocated upstream
 lemma is reported as `unavailable_dependency`, never a silent break. Wire
 `scripts/tests/check_proof_corpus_migration.sh`: it must prove the flagship
 corpus (`hmac_sha256`, `constant_time_tag`) re-greens under a simulated
 toolchain bump, that a removed/renamed pinned lemma surfaces as
 `unavailable_dependency`, and that no `needs_recheck` obligation can reach a
 green badge without an actual kernel re-check.
### Task R-0217

**Phase:** 1, 2

**Reconciliation (2026-10-19):** Merge axiom policy into R-0506; keep clean-replay increment.

**Objective:** Add an axiom-inventory and clean-checkout proof replay gate. Every `proved_by_lean` / `proved_by_kernel_decision` fact must record the Lean axioms its theorem transitively depends on (the `#print axioms` walk):

 `propext`, `Classical.choice`, and `Quot.sound` are the expected baseline;
 any `sorryAx`, `ofReduceBool`/`native_decide`, or user-declared axiom must
 downgrade the evidence class or surface as an explicit trusted-boundary
 fact in the §10 inventory — never sit silently under a kernel-evidence
 badge. Pair it with clean-checkout replay: the proof corpus must re-green
 from a fresh checkout in a clean temp directory, proving no green badge
 depends on stale `.lake` build artifacts, uncommitted files, or local
 machine state. Wire `scripts/tests/check_axiom_inventory.sh` (red-team
 case: a spec theorem patched to use `sorry` or a new `axiom` must lose its
 kernel-evidence class) and
 `scripts/tests/check_clean_checkout_replay.sh` (the flagship corpus —
 `hmac_sha256`, `constant_time_tag` — must replay green from a pristine
 copy, and a deliberately corrupted source-vs-artifact mismatch must fail
 loudly, not reuse the stale artifact). Seed the incremental-driver area's local cache with a
 stale proof result, wrong dependency root, truncated entry, and valid entry
 from another compiler/toolchain; clean-checkout replay must reject/recompute
 all four before any evidence becomes green.
### Task R-0218

**Phase:** 1

**Disposition (2026-10-19):** superseded by R-0494: merge into R-0494.

**Objective:** [historical origin: closed phase 3 items 15/16] Convert `--report proof-status` and `concrete prove`'s obligation facts from consistency-gated recompute to literal `ObligationCore`-ledger views — the last old phase 3 surfaces that recompute (sound today because consistency-gated). They are the proof-status / trust surfaces, so they belong in this phase.

### Task R-0219

**Phase:** 1, 5

**Reconciliation (2026-10-19):** Keep as cumulative gate suite.

**Objective:** Add the proof-status area validation artifact: a trust-gate pressure project that includes transitive proof dependencies, stale dependency propagation, tool-version drift, proof-corpus migration across a simulated toolchain bump, assumption widening, spec-adequacy policy, vacuity downgrade, solver portfolio / disagreement handling, evidence mutation testing, axiom inventory and clean-checkout replay, weaker-evidence monotonicity, and a release gate proving each status cannot be silently presented as stronger evidence. Include cache-off/on/verify parity and stale/corrupt-cache negatives so the trust gates cover the operational path

 users actually run.

## Area: Provable And Predictable Subsets

Goal: give users a named small subset they can rely on for serious
proof/evidence work.

Done when: the subset family has public names, allowed constructs, rejected
constructs, arithmetic-site policy, runtime-error policy, and compatibility
promises.

### Task R-0221

**Phase:** 1, 2

**Disposition (2026-10-19):** superseded by R-0493/S6: merge semantic ownership into R-0493.

**Objective:** Freeze arithmetic-site semantics for subset claims. This task is reconciled with historical phase 6 item 10 and `docs/language/ARITHMETIC_POLICY.md`: build/profile names are policy bundles, not arithmetic modes, and the same source expression must not mean wrap in one profile and checked in another. Ordinary `+ - *` are checked in every profile; intentional modular arithmetic is written as `wrapping_*`; intentional clamping is written as `saturating_*`. Subset reports must classify arithmetic sites as `checked`, `proved`, `runtime- checked`, `explicit-wrapping`, or `explicit-saturating`; they must never infer an ambient arithmetic mode from `debug`, `release`, `predictable`, or `proof`.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0221).

### Task R-0222

**Phase:** 1, 2, 4

**Reconciliation (2026-10-19):** Keep; replace obsolete operator-coverage snapshot.

**Objective:** Carry arithmetic-site facts into diagnostics, reports, assumptions, proof obligations, release bundles, and backend contracts. A theorem about checked arithmetic is not a theorem about modular arithmetic; a function using `wrapping_*` is either proved against the proof model's explicit modular operator (today only `wrapping_add@u32 → addw 32`, as SHA-256 uses) or, for the not-yet-modeled forms (`wrapping_sub`/`wrapping_mul`, other widths, saturating), outside the default proof subset. Add `docs/ARITHMETIC_SITE_EVIDENCE.md`, `examples/arithmetic_site_evidence/{checked,wrapping,saturating,profile_invariant}/`,

and `scripts/tests/check_arithmetic_site_evidence.sh`; the gate must prove
profile-invariance (the same expression has the same semantics under
different build profiles), explicit classification of every arithmetic site,
and rejection/downgrade of proof claims that confuse checked and wrapping
semantics.
### Task R-0223

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0496/S7: merge into R-0496’s semantic contract.

**Objective:** Define a first runtime failure model: abort, assertion failure,
OOM, stack overflow, checked arithmetic/bounds traps, `defer`/cleanup,
impossible branches, and what each does to proof/resource claims. Abort is a
profile-dependent terminal operation: hosted execution terminates the process;
freestanding execution invokes a declared non-returning handler. Neither form
promises unwinding, `defer`, destruction, or recovery.

### Task R-0224

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; integrate S13/S15 resource relation.

**Objective:** Define source-level stack-depth versus backend/target stack
claims, including how a source estimate, ABI/backend frame growth, and a named
target budget combine before the predictable profile may call stack use
bounded. Consume R-0445's source-stack and call-depth facts as the source
layer, then define the checked bridge to backend frame facts. Never relabel the
source estimate as target stack bytes.

### Task R-0227

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0499/R-0507: merge into checker rules and soundness.

**Objective:** Define source-level memory-safety claims precisely: what linearity, borrows, cleanup, trusted code, raw pointers, and FFI do and do not guarantee.

### Task R-0228

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0499/S8 and V6: merge enforcement into R-0499; raw-operation extensions into V6.

**Objective:** Define the **Unsafe island** rule for unchecked operations. Any operation that bypasses a safe runtime check, borrow/linearity rule, raw-pointer restriction, layout proof, or FFI ownership boundary must be exposed through a deliberately small surface requiring `with(Unsafe)` or an explicitly named `trusted` boundary. Examples include future `get_unchecked`, raw pointer place writes, unchecked casts/conversions, inline asm, and unchecked FFI wrappers. There is no global unsafe mode and no silent trusted wrapper that hides authority: audit output must show the safe wrapper, the underlying trusted/Unsafe operation, and the assumption being accepted.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0228).

### Task R-0230

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0509/S16: merge into R-0509 and slice profiles.

**Objective:** Define the v1 threat model: adversary, trusted base, proof scope, backend scope, side-channel scope, dependency scope, and what remains out of model.

### Task R-0231

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0511/R-0512: merge into each slice’s admission controls.

**Objective:** Add negative examples for every `ProvableV1` and `PredictableV1` exclusion.

### Task R-0232

**Phase:** 2, 4, 7

**Reconciliation (2026-10-19):** Keep; use versioned V-slice/profile names.

**Objective:** Update `CLAIMS_TODAY.md`, README, showcase docs, and release bundles to use the frozen subset names consistently.

### Task R-0233

**Phase:** 1

**Disposition (2026-10-19):** done — the unprofiled-float exclusion shipped (`ProofCore.lean`, `Report.lean`); profiled floats remain V7 (R-0496/R-0495).

**Objective:** Close the unprofiled-float proof hole before any float proof claim:

 float-typed params/returns/locals/literals/ops are excluded from ProofCore
 extraction unless an explicit float profile is active. Audit output must
 say `float semantics: unprofiled` and `proof eligibility: excluded` rather
 than reporting a float operation as extracted through integer `PBinOp.add`.
### Task R-0236

**Phase:** 1, 4

**Disposition (2026-10-19):** superseded by R-0506/R-0509: merge axiom classification and inventory.

**Objective:** Classify the first float semantics layer honestly. Until Concrete imports or proves a checked IEEE-754 semantics library, primitive float operations are `float_semantics_trusted`; proofs over profiled float code are refinements to an explicit bit-level IEEE spec under that named trusted primitive layer, not `proved_by_lean` from first principles.

### Task R-0238

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0511/R-0512: merge into cumulative slice matrices; retain orthogonal policies.

**Objective:** Add the provable-subsets area validation artifact: a profile matrix project covering `PredictableV1`, `ProvableV1`, unprofiled-float exclusion, profiled-float admission, borrow/reference proof-class decisions, constant-time source shape, stack/runtime-failure assumptions, and negative examples for every exclusion. The gate must prove reports never call excluded code proof eligible.

## Area: Runtime Safety Obligations

Goal: generate SPARK-like obligations for boring runtime failures instead of
relying only on examples and prose.

Done when: parser/security examples can show obligations for bounds, div/mod
zero, overflow profile, casts, and loop bounds with statuses
`proved`, `enforced`, `assumed`, `missing`, or `blocked`.

This phase is the **absence of runtime errors** story, stated honestly. A bounds
check, overflow trap, cast check, or loop-bound assertion may be enforced at
runtime, proved statically, assumed by policy, or still missing; those are
different evidence classes and reports must not collapse them. The SPARK/Ada
lesson to keep is the framing: runtime safety is not just "the program probably
doesn't crash"; it is a set of named obligations with source spans, replay
commands, and visible proof/enforcement status.

### Task R-0241

**Phase:** 2, 5

**Disposition (2026-10-19):** duplicate of R-0155/R-0178: merge obligation payload into those canonical schemas.

**Objective:** Define stable obligation schema v1: id, kind, source span, function, expression, dependencies, evidence status, discharging theorem/check/ assumption, and replay command.

### Task R-0242

**Phase:** 2, 3

**Reconciliation (2026-10-19):** Keep; align terminal semantics with R-0496.

**Objective:** Define the user-level error model: `Result`, `Option`, assertion failure, abort/panic, recoverable errors, test failures, and how error flow interacts with capabilities, proofs, runtime obligations, and audit output.

### Task R-0243

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Merge semantic/generator proof ownership into R-0460 and typed-operation slices.

**Objective:** Generate narrowing/invalid-cast obligations.

### Task R-0248

**Phase:** 2, 4, 6

**Objective:** Generate stack/recursion obligations where the profile claims
boundedness and enforce the configured target/profile stack budget rather than
only reporting it. Gate just-under/just-over programs, deep call chains with
large frames, report/gate accounting agreement, and a mutation that removes
enforcement while leaving the report. Enforce over R-0445's source facts only
after R-0224 supplies the named backend/ABI/target bridge; keep source and
target byte claims distinct in the resulting evidence.

### Task R-0251

**Phase:** 1, 4

**Reconciliation (2026-10-19):** Keep; integrate R-0491.

**Objective:** Add a runtime-error regression corpus: invalid cast, loop-bound violation, lossy byte/text conversion, ignored fallible result, unwrap-like failure, panic/abort profile mismatch, and release-policy rejection for missing `#[overflow_checked]` evidence where required.

### Task R-0253

**Phase:** 3, 4

**Disposition (2026-10-19):** duplicate of R-0166: merge into R-0166.

**Objective:** Add high-quality diagnostics for obligation failures: violated obligation, source expression, required evidence, current status, and next action.

### Task R-0254

**Phase:** 1, 5, 7

**Reconciliation (2026-10-19):** Keep; route through R-0494 and policy.

**Objective:** Add obligation suppression only through explicit assumptions or policy waivers, never comments or hidden allowlists.

### Task R-0255

**Phase:** 2, 4

**Disposition (2026-10-19):** duplicate of R-0460: merge into R-0460.

**Objective:** Prove or validate obligation-generation soundness for the first obligation kinds through the compiler soundness bridge.

### Task R-0259

**Phase:** 2, 4, 6

**Reconciliation (2026-10-19):** Keep as cumulative corpus.

**Objective:** Add the runtime-safety area validation artifact: a runtime-safety corpus covering bounds, div/mod-zero, overflow, casts, panic/abort/assert, byte/text/path boundaries, stack/recursion, inferred invariant candidates, newtype invariant hypotheses, arithmetic-site evidence mismatches, and obligation suppression. Each case must show one of `proved`, `enforced`, `assumed`, `missing`, or `blocked`, include a negative variant, and run through policy gates plus human/JSON reports.


### Task R-0461

**Phase:** 1, 2

**Reconciliation (2026-10-19):** Live; the H23 fix is done — merge the remaining generalization into R-0494/R-0460 and archive the H23 implementation narrative.

**Objective:** Give hypotheses provenance, and compose obligation status across the
assumption edge. **Closes H23, which is live: a guaranteed out-of-bounds access currently
reports `proved_by_multi_kernel (3: lean, rocq, isabelle)`.**

Do this before any further evidence-surface work. It is not a refinement of the
multi-kernel story; it is the defect that story is currently decorating.

**The substrate now exists.** Register C shipped 2026-07-31 as
`Concrete/Report/Evidence.lean`: `Evidence` carries an assumption set, `underHypotheses`
folds a claim together with its hypotheses' debt, and `HypOrigin` already encodes the four
sources below with `debt` total over them. C2′ and C3 are compile-time theorems, so once
this task populates `assumes` the cap applies **by construction** — R-0461 is now wiring
provenance into the generators, not designing a mechanism.

The mechanism, not just the symptom. `hyps : List Expr` erases where a fact came from,
and the three sources have entirely different justification status:

| Origin | What justifies it |
|---|---|
| control-flow guard | sound by construction (the branch was taken) |
| `#[requires]` | discharged at every call site by `callSiteObligations` |
| loop `#[invariant]` | owes O1 (init) **and** O2 (preservation) for that loop |
| explicit `#[assume]` | nothing — must downgrade the conclusion, permanently |

So hypotheses become records carrying `origin` and an optional `justifiedBy` obligation
reference, and status composes:

```
status(O)  ≤  min( status of O's own proof,
                   min over h ∈ hyps(O) of status(justifiedBy(h)) )
```

This is the same discipline already applied to proof dependencies (`staleDeps`) and to the
`trusted` boundary. Hypotheses are simply a dependency edge that was never modelled — which
is why the fix is a record change plus a fold, not new proof machinery.

**Reporting: DECIDED 2026-07-31, do not re-open when implementing.** The capped status is
the EXISTING `assumed`, plus a structured `conditions : [{ref, status}]` naming what is
outstanding. Not a new conditional badge, and specifically **no status string containing
`proved` may be emitted for a claim with an undischarged condition** — that rule is the
gate. Rationale in [docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md](docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md):
a conditional badge still containing `proved` reproduces H23 for every consumer that
pattern-matches the status, which is how H23 fooled the policy gate and the report reader
alike. The precedent exists twice already (`assumed` for `#[assume]`, `assumed_at_entry`
for `#[requires]`), and `assumed` is already gate-forbiddable via
`ProjectPolicy.forbidAssume`, so the cap makes H23 catchable by machinery that is already
wired. Independence is orthogonal and unchanged: the receipts still record that three
kernels closed the goal.

Gate over `examples/unsound_hypothesis/src/main.con`: the bounds obligation must NOT read
proved while O2 is unproven, the fixture must stay non-vacuous (assert O2 really is
undischarged, so the gate cannot pass by the invariant becoming provable), and a mutation
must show the gate detects removal of the composition.

Widen beyond loops once the mechanism exists: any obligation resting on an `#[assume]`,
and any call-site precondition whose caller-side discharge is itself conditional.

### Task R-0462

**Phase:** 1, 4

**Reconciliation (2026-10-19):** Live; the initial gate is done — broader coverage merges into R-0491; the "missing" claim is historical.

**Objective:** Fuzz the *compiled binary* against the safety claims — the empirical shadow
of Register A.

Register A asserts *if the obligation holds, the runtime property holds*. That is a
proof obligation, and R-0460 discharges it row by row over years. There is a cheap
continuous check of the same statement available now, and it is missing: **for every
function whose runtime-safety obligations all read `proved`, generate inputs satisfying
its `#[requires]`, run the COMPILED binary, and assert it does not trap.** Any trap is a
counterexample to Register A, to the obligation generator, or to the lowering — and it
is found in seconds rather than argued about.

Distinct from `--report bridge-check`, which fuzzes concrete inputs against the
*obligation* as evaluated by the interpreter. That tests the obligation against a model.
This tests the *claim* against the artifact that ships, and so also crosses the
surface→Core→SSA→LLVM lowering that no register row covers today.

H23 is the existence proof: a program whose bounds obligation reads
`proved_by_multi_kernel` aborts on the first run. Any fuzzer pointed at the binary would
have caught it immediately; every kernel-side surface reported success.

This is the highest fault-finding value per unit of work available, and it is the
project's own doctrine applied consistently — differential against the real artifact
outranks agreement between checkers of a model.

### Task R-0464

**Phase:** 1, 2, 4

**Disposition (2026-10-19):** superseded by R-0493/R-0495/R-0460: merge; archive completed H24 fix and retire SSA-first prescription.

**Objective:** ONE trap-semantics definition, consumed by SSA construction,
interpretation, optimization, obligation generation and the backends — and, second,
generate obligations from SSA rather than the surface AST.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0464).

### Task R-0465

**Phase:** 1

**Disposition (2026-10-19):** done for its enumerated wiring; the remaining universal-verdict-authority requirement merges into R-0494.

**Objective:** Extend the discharge-adapter firewall to the assumption axis, and bring the
multi-kernel path inside it.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0465).

### Task R-0468

**Phase:** 1

**Reconciliation (2026-10-19):** Merge into R-0513.

**Objective:** Make the nightly-pinned gates reachable in this repository, and fix the
hole-closure conditions that currently depend on them.

The scheduled jobs are gated on `github.repository == 'lambdaclass/concrete'`. This
repository is `unbalancedparentheses/concrete2`, so **the scheduled path can never fire
here** — `proof-replay`, the fuzz campaigns and `check_gate_mutation_coverage.sh` are
reachable only by manual `workflow_dispatch`. The pin is correct in intent (a fork should
not run the canonical nightly) and wrong in effect (the fork where the work happens gets
no nightly at all).

Two consequences, the second worse than the first:

- **H22's closure condition is unsatisfiable as written.** It says the entry can be
  deleted "once the nightly confirms the `checked-arith-trap` family reports KILLED". No
  nightly will run. Either dispatch `check_gate_mutation_coverage.sh` manually and close
  it, or restate the condition against something that can actually happen here. Corrected
  in `KNOWN_HOLES.md` to say "manual dispatch" pending this task.
- **Nobody knows what else the nightly would have caught.** `check_gate_mutation_coverage.sh`
  is the gate that finds decorative gates — it is how H22 was found in the first place, on
  a run someone did by hand. It has run in this repository approximately never. Assume
  more H22s until it has run clean once.

Minimum fix: make the repository pin a variable, or add this fork to it. Better fix: run
the mutation-coverage gate on merges to `main` here, since it is the gate whose absence is
self-concealing — a decorative gate stays green, so nothing else will report its absence.

### Task R-0460

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; reconcile register with exact checked Core. The register count is five lowering rows, none fully discharged and four half discharged (stale "four rows" text corrected).

**Objective:** Discharge the obligation-sufficiency register — *if the flat goal holds,
the runtime property holds* — rule by rule.

[docs/verification/VC_BRIDGE_REGISTER.md](docs/verification/VC_BRIDGE_REGISTER.md) enumerates the `Core ->
obligation` lowering as four rows with the theorem each owes. Zero are discharged. This
is the ceiling on every runtime-safety claim and every multi-kernel badge: adding kernels
cannot detect a fault here, because all of them check the *same* lowered proposition, so
a mis-lowering yields unanimous agreement on the wrong formula.

Distinct from R-0455. That register asserts *transformed goal implies input goal*; this
one asserts *flat goal implies runtime property*. A transformation register cannot
discharge a row here, and conflating them loses the distinction the architecture rests
on.

Choose the first row on lemma shape rather than conclusion shape. `d /= 0` is the
simplest conclusion to state but its lemma must relate the flat fact to the interpreter's
division semantics, where truncation-versus-flooring and trap-on-zero live. Bounds has a
duller conclusion but a cleaner definedness step, and far more real instances — though it
carries an extra premise about `arraySizeMap` soundness, which is a fact about name
resolution rather than semantics. Sketch both before committing.

Exit criterion names rows, not a count: a bare "two rows discharged" is satisfiable by
two rewriting arguments while the non-arithmetic ceiling does not move at all.

**Priority argument, from the only fault this arc has actually caught.** R-0448 added two
independent kernels and, to date, kernel agreement has surfaced zero defects — every
disagreement observed was synthetic, injected by the gate's own mutation. The one real
fault the work found was `Z.div` vs `Z.quot` disagreeing at `(-7)/2`, and it was found by
`--report core-semantics-diff`: a *differential* test against an independent evaluator, not
by a second kernel. That is the expected result rather than bad luck. Adding kernels buys
independence on kernel implementation and kernel foundations — the axes where failure was
already least likely — and buys nothing on the bridge, where our own code is the thing
that can be wrong. So the differential surfaces (`core-semantics-diff`, `bridge-check`,
`lowering-agreement`) and the rows in this register are where fault-finding actually lives,
and they should outrank prover count N+1 when the two compete for the same week. Kernel
diversity is for the auditor who does not trust Lean; it is not a bug-finding strategy.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0255** (merged): duplicate of R-0460: merge into R-0460.
- **R-0261** (merged): duplicate of R-0496/R-0460: merge; already explicitly reconciled.
- **R-0464** (superseded): superseded by R-0493/R-0495/R-0460: merge; archive completed H24 fix and retire SSA-first prescription. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0464)

## Area: Compiler Soundness Bridge

Goal: move the flagship-used `Core -> ProofCore` rules from "extracts to the
expected ProofCore shape" toward source-semantics agreement and checked
trust-gate correctness.

Prior-art peer group for this phase: CompCert for proved compiler passes,
CakeML for verified bootstrap discipline, Cogent for linear systems code with
refinement proof artifacts, F*/Low* for obligation-to-SMT-to-extraction
workflow, and Alive2 for SMT-checked LLVM IR equivalence/optimization
validation. Keep these as reference points for the shape of evidence, not as
marketing claims that Concrete already matches them.

Done when: each flagship-used ProofCore construct is classified as
shape-preserved, eval/source-semantics-preserved, or still trusted; proof-report
facts agree with compiler state; a small independent checker re-derives the
named `CoreCertificateV1` predicate from canonical artifacts rather than
trusting producer summaries; and the remaining trusted base is machine-readable.

**Endgame (north star): the compiler becomes a mechanically-verified artifact,
out of the trusted base.** The per-rule discharge below (R-01..R-21), the
`CoreCertificateV1` independent checker, and Task R-0299 translation validation
are not the finish line — they are the incremental ladder toward proving each
pass semantics-preserving in Lean against the interpreter reference, so the
compiler leaves the TCB entirely. Concrete's intended differentiator is that its
Lean-hosted compiler-correctness work and selected user-program proofs can share
one kernel and one evidence pipeline. CompCert and CakeML remain peer references
for verified-compilation and bootstrap discipline, not projects this roadmap
claims to surpass. The 6B fact-centralized IR and interpreter-as-oracle are the
runway: each pass is a clean function over committed facts with a reference
semantics to prove against, so passes should be built PROVABLE-FIRST, not merely
consistent. This names the endpoint so intermediate design choices bend toward
it; it does not schedule a multi-year proof now, and it must not starve the
near-term or the external trial. The complementary rewrite-passes-in-Concrete
route is Task R-0413; the shorter path is proving the existing Lean-hosted
passes here.

### Task R-0260

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0506/S12a: merge into pass/validator obligation ledger; preserve legacy historical view.

**Objective:** Add a compiler-soundness rule-status dashboard over R-01..R-21:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0260).

### Task R-0261

**Phase:** 2, 4

**Disposition (2026-10-19):** duplicate of R-0496/R-0460: merge; already explicitly reconciled.

**Objective:** Discharge the soundness-bridge area semantic obligations through R-0496's normative Core semantics; do not introduce a separate subset interpreter. (Superseded objective, 2026-10-18: Build source semantics for the provable subset as needed by rule discharge, not as a speculative full-language semantics.)

### Task R-0263

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0511/R-0512: merge into V1–V7 slice ordering.

**Objective:** Prioritize the hard eval/source rules in dependency order: direct calls, structs/fields, enums/match, arrays, casts, arraySet, flat bounded `while`, and `while_step`.

### Task R-0264

**Phase:** 1, 2, 4

**Disposition (2026-10-19):** superseded by R-0494/R-0509: merge implementation into R-0494, soundness proof into S16.

**Objective:** Prove selected proof-report facts agree with compiler state: `proved`, `stale`, `blocked`, `missing`, `ineligible`, `trusted`.

### Task R-0265

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0495/R-0509: merge into statement/binding/aggregation soundness.

**Objective:** Prove or mechanically validate trust-gate correctness: body fingerprint determinism, spec-drift completeness, proof attachment lookup, FnTable completeness, eligibility classification.

### Task R-0266

**Phase:** 2, 4, 5

**Disposition (2026-10-19):** superseded by R-0509/S16: merge; distinguish trusted assumptions from ordinary proof dependencies.

**Objective:** Record a machine-readable trusted computing base for proof/evidence claims:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0266).

### Task R-0267

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0499/R-0507/R-0495: merge; capabilities and purity remain distinct.

**Objective:** Prove selected checker/report agreement for authority and purity facts used by proof eligibility, so a function cannot be called proof-eligible while secretly requiring capabilities.

### Task R-0268

**Phase:** 2

**Disposition (2026-10-19):** superseded by S5–S8: retire undecided architecture question; plan has chosen the chain.

**Objective:** Decide whether deeper source-semantics proofs require a normalized Core layer. Add the layer only if the direct rule proofs show repeated semantic duplication across at least two forcing examples.

### Task R-0269

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0495: merge composition needs into R-0164; retire PExpr table architecture.

**Objective:** Automate dependency-ordered spec/table generation. Function specs and PExpr bodies should be collected/generated in dependency order together with FnTable completeness and call dependencies, so proof authors do not hand-relocate definitions above tables like `shaFns`.

### Task R-0270

**Phase:** 6

**Reconciliation (2026-10-19):** Keep; distinguish user-spec totality from Phase 2 compiler totality.

**Objective:** Prove or mechanically validate spec/ghost totality reporting: every contract-referenced `spec fn` or ghost computation is either backed by Lean termination, accepted by a Concrete totality check, or rejected with a `totality_obligation_missing` status. A contract may not depend on a partial or non-terminating spec expression silently.

### Task R-0272

**Phase:** 1, 4

**Reconciliation (2026-10-19):** Merge shared harness into R-0491; split obsolete reference-return repair.

**Objective:** [historical origin: old phase 4 items 44f/44g] Finish the remaining compiler-correctness hardening:

 - extend the differential generator with string, heap/allocation,
   linear-value, effectful-I/O, generic-function, generic-struct, and
   multi-instantiation shapes; generate division/modulo including `MIN / -1`,
   shifts at and around the width boundary, and arithmetic that can actually
   reach checked overflow rather than bounding every expression back into a
   safe range. Preserve the rule that an E07xx/panic on a generated well-typed
   program is a compiler bug, and compare the identity of traps rather than
   treating any trap as equivalent to every other trap;
 - rotate seeds in a nightly CI campaign and automatically minimize
   failures rather than relying only on the fixed presubmit seeds; and
 - repair the dead-path ref-return lowering case where a reference-typed
   return materialized from a ref identifier or `&place` emits a spurious
   extra load. This path is unreachable from source while reference returns
   are rejected, so its regression fixtures must separately preserve the
   accepted trusted raw-pointer (`*const`/`*mut`) return cases;
 - scale R-0007's metamorphic identity oracle across generated and corpus
   programs. Its strict alpha leg performs capture-avoiding fresh renames in
   the correct lexical namespace and requires identical acceptance, exit,
   output, trap identity, interpreter behavior, and compiled behavior modulo
   honest display/span facts. Its distinct hostile-spelling leg pressures
   generated mangles, builtin/intrinsic names, import aliases, and
   local/global collisions; that transform may preserve behavior or fail
   closed with an allowed collision diagnostic, but may never silently
   diverge or trigger an internal error. Bugs 044/050/051/054 were
   collision-sensitive, so random fresh renaming alone is not coverage.
   Seed the historical witnesses as fixed legs, and keep IR-only spellings
   such as `@fnref.*` in lowering mutation fixtures rather than the source
   renamer.
### Task R-0273

**Phase:** 1, 4

**Disposition (2026-10-19):** superseded by R-0491/R-0498: merge; map every listed family to a retained control.

**Objective:** Add a semantic-darkness audit and red-team gate. The goal is to catch the checked-arithmetic class of bug before it repeats: a construct looks ordinary in source, but its real behavior depends on width, profile, target, allocation, authority, runtime checks, or an outdated proof/interpreter model. Add `docs/SEMANTIC_DARKNESS_AUDIT.md` and `scripts/tests/check_semantic_darkness.sh`; wire the gate into CI or the soundness-bridge area validation artifact. The audit must cover:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0273).

### Task R-0274

**Phase:** 2

**Disposition (2026-10-19):** superseded by R-0499/R-0514: retire source-checker/CompilerDB authority prescription.

**Objective:** **Preserve the one source of typing truth through proofs.** Every 2026-07 front-end bug was two passes holding different opinions about the same program: Check typed literals from the hint while Elab typed them from the sibling operand;

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0274).

### Task R-0275

**Phase:** 2, 4, 5

**Reconciliation (2026-10-19):** Keep; align with checked-Core derivations and R-0353.

**Objective:** Add an independent Core-certificate checker for a narrow, explicit validated-Core predicate.


 Define a versioned canonical `CoreCertificateV1` and a small separate
 `concrete-cert` library/executable target. Enforce a dependency firewall: it
 may import the canonical artifact/schema decoder, hash/id datatypes,
 **independent declarative specifications and shared data types**, and checker
 theorems; it may
 not import Parser, Check, Elab, Mono, Lower, Report, CLI, project loader, the incremental-driver area scheduler/cache, producer executable judgments such as
 `TypeJudgment`/Layout/fingerprint helpers, or producer verifier
 implementations. Publish its
 source/binary hash, import inventory, `partial`/unsafe/axiom inventory,
 rule-set version, and size so "small checker" remains measurable.

 V1 independently derives its verdict from the canonical Core artifact and
 small specifications. It checks at least: canonical/schema validity;
 artifact, subject, predecessor, and dependency-root binding; unique stable
 identities and referenced-node/provenance existence; constructor-local type
 consistency; direct-call arity and argument/result agreement; absence of
 unresolved placeholders/type variables where the Core boundary forbids
 them; direct-call capability containment; proof-target/body-fingerprint and
 obligation dependency binding; and the old phase 6B validation-chain shape.
 Proof-target/fingerprint/obligation/provenance checks establish identity and
 binding only; they do not establish extraction correctness, theorem/spec
 adequacy, or source correspondence. It
 must not merely compare a producer-emitted fact table with a second
 producer-emitted summary.

 Ownership/borrow/value-flow claims enter V1 only where the artifact carries
 enough independently checkable decisions and path edges to validate them.
 Checking that an ownership field exists is not ownership-checker soundness.
 Extend the predicate one rule family at a time with a mutation fixture. Give
 every predicate an explicit checker-proof status:
 `checker_trusted` for executable checking without a soundness theorem,
 `checker_soundness_kernel_proved` only when Lean proves a theorem such as
 `checkCoreCertificate a c = true -> CoreCertificateV1.Valid a c`, and
 `kernel_replayed` only for the artifact-specific theorem. An unproved checker
 may not appear under the same badge as a proved checker.

 Emit `certificate_structurally_checked`, `certificate_mismatch`, or
 `certificate_unsupported`; never `certified` or `proved_by_lean` merely
 because the structural checker passed. An artifact-specific preservation
 theorem actually replayed by Lean may separately emit `kernel_replayed`.
 State the remaining TCB precisely: V1 does **not** prove parsing,
 resolution, Check/source-to-Core correspondence, ownership/capability
 soundness beyond named local rules, spec adequacy, ProofCore extraction
 beyond discharged soundness-bridge-area rules, native-code behavior, or dependency
 completeness beyond the conservative the incremental-driver area envelope. Its own TCB still
 includes adequacy of the named predicate, canonical decoder, hash algorithm
 and collision assumption, checker implementation unless covered by the
 stated soundness theorem, Lean compiler/runtime and host execution when
 running a compiled checker, and every imported trusted/axiomatic primitive.
 `kernel_replayed` is reserved for an artifact-specific theorem actually
 checked by the kernel; a theorem about the checker does not make execution
 of the compiled checker kernel-only.

 Bind the checker to the incremental-driver area query DAG: an **independent consumer** may
 distrust cached canonical bytes and the producing compiler for the named V1
 predicate only after that consumer reruns the checker. A receipt rendered by
 the producer remains producer output until external replay. A verification
 receipt names artifact digest,
 dependency root, checker/rule-set version, checked and unsupported
 predicates, theorem/replay identity where any, and exact trust boundary.

 Gate altered expression types, wrong call signatures/results, missing
 capabilities, unresolved type variables, duplicate ids, missing provenance,
 stale dependency roots, artifact/certificate swapping, fabricated proof
 links, mismatched obligation subjects, malformed/noncanonical encodings,
 unsupported constructs mislabeled checked, and a producer record that
 agrees with itself about a false artifact fact. Break a producer judgment/
 layout/fingerprint helper and prove the checker still rejects the bad
 artifact without importing that helper. Wire
 `scripts/tests/check_core_certificates.sh` and a checker import-firewall gate.

### Task R-0276

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0506/R-0496: retire general legacy rewrite; retain migration-needed wrappers only.

**Objective:** Reduce ProofCore partial-def opacity only where proof preservation needs it.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0276).

### Task R-0277

**Phase:** 2, 4

**Disposition (2026-10-19):** superseded by R-0511/R-0512: merge into each slice’s cumulative acceptance.

**Objective:** Add the soundness-bridge area validation artifact: a compiler-soundness dashboard with one witness program per shipped ProofCore construct, one status per R-rule, replay commands for proved/mechanically-validated facts, and regressions proving report facts (`proved`, `stale`, `blocked`, `missing`, `ineligible`, `trusted`) agree with compiler state. Include the `CoreCertificateV1` predicate/rule-set version, checker binary/source hash, soundness theorem names, independent receipt per artifact, mutation corpus, cache-off/on receipt parity, and a machine-readable list of every boundary V1 still leaves producer/compiler-trusted.

### Task R-0454

**Phase:** 2, 5

**Reconciliation (2026-10-19):** Merge implementation ownership into R-0450; retain identity invariants. Alpha-normalization alone does not make a proposition digest migration-neutral across SSA conversion; same-proposition criteria, version changes, compatibility mapping and negative controls are explicit (with R-0450/R-0512).

**Objective:** Encode obligations in a neutral term form and compute
`ProofSubjectDigest` over that neutral term rather than over any host AST.

**This belongs immediately after R-0004's remaining slices, and before the prover-neutral
evidence work merges.** The reason is a closing window, not importance: digests are
*stored*. Once artifacts carry a digest computed over a Lean AST, changing the basis
invalidates every stored claim simultaneously and forces migrating every fingerprint in
`examples/`. No `subjectDigest` field exists in `Concrete/` yet, so the migration cost is
currently zero and only grows.

It is R-0004's decoupling and is valuable even if no second prover ever ships: freshness
becomes host-independent by construction, so every prover agrees on staleness without any
of them seeing another's AST. It also closes a live defect — the multi-kernel fold matches
kernels on the obligation *id*, not on a subject digest, which is precisely the gap the
graduation criteria named.

Scope: the neutral term encoding (`PExpr`/`PVal`/`PMatchPat` are already prover-agnostic;
serialize versioned), the `NeutralObligation` and `HostAttestation` records, and
`checkedAgainstDigest` such that a mismatch reads `stale`. Three encoding invariants must
survive serialization: `.call` and `.applyVar` stay distinct (the two-namespace
resolution that closed bug 061); `displayName` is excluded from the digest because
identity is `CallableId`; and — **added 2026-07-31 from the R-0464 cross-check** —
**variable naming must not affect the digest**, so the term is alpha-normalized (de Bruijn
or a canonical renaming) before hashing.

That third invariant is this task's own closing-window argument turned on itself. R-0464
moves obligation generation from the surface AST to SSA, where the same obligation is
spelled over `i₁` instead of `i`. If names are in the digest basis, R-0464 invalidates
every stored digest — precisely the mass migration this task exists to avoid, arriving one
task later. Alpha-normalizing now makes the AST→SSA move digest-neutral and costs nothing
today. Getting this wrong is not detectable until R-0464 lands, which is exactly when it
is expensive.

Do not widen R-0004 to absorb this. That task is mid-flight; this is its successor.

Normative rule and record shapes in
[docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md](docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md).

### Task R-0455

**Phase:** 2, 4; after-1.0/research

**Reconciliation (2026-10-19):** Merge typed-IR implementation into R-0450; keep Register B obligations. The paragraph proposing to delete the validation layer is historical and void; rendering validation remains required.

**Objective:** Replace per-prover string printing with a transformation pipeline, and
give each transformation a soundness register row.

The current lowering fuses three jobs into string templates plus an exit code: how to say
the obligation, how to try to prove it, and how to believe the answer. The defects follow
from that fusion.

`div`/`mod` sub-terms are dropped from the prover lowering (`exprToProver`,
`Concrete/Report/ReportObligations.lean:930`, "`div`/`mod` dropped as in `toLeanProp`" —
the comment states the fact without a reason). The operator table is infix-only, which is
why a prefix form has nowhere to go; note this is a limit of the *table*, not of the
targets, since `CoreExtract.lean:96` renders the same operators as prefix `Z.quot`/`Z.rem`
in the extraction path. The divisor-nonzero *obligation* is unaffected and does flow
through — only div/mod appearing inside a larger expression is lost.

Spec-function calls are dropped because there are no uninterpreted symbols.

The tactic is hardcoded **per driver**, so reaching a different one costs a cloned driver.
This branch measured that cost rather than predicting it: `rocqNiaLowering`
(`ReportObligations.lean:1277`) exists solely to reach `nia`, duplicating the whole
`ProverLowering` record to change one word in a proof script, and
`isabelleSmtReplayLowering` (`:1344`) is a third clone. Four drivers now differ mostly in
a binop column and a tactic name. That is the argument for `tactics` (ordered, budgeted)
being a driver *field* instead of a template literal.

Adopt the Why3 shape: a typed term IR (sorts including bitvectors, operators carrying
arity *and* fixity, binders, uninterpreted symbols) plus goal-to-goal transformations
selected by declarative per-prover drivers. A driver states what a target *cannot*
express so the pipeline transforms instead of silently dropping. Backends become
`print` / `tactics` (ordered, budgeted) / `attest` / `keep`, with a batch interface so a
session-building prover amortizes startup instead of paying it per goal.

Concrete's improvement over Why3, and the reason this is a register and not just a
refactor: Why3's transformations are trusted. Each pass here owes *transformed goal
implies input goal*, recorded per row with a fingerprint of the transformation it
justifies so a discharged row fails loudly rather than going vacuous when its subject
changes.

Difficulty is skewed and the register must record it: `eliminate_div_mod` is a rewriting
argument, while `eliminate_algebraic` requires the axiomatization be conservative over the
datatype theory, which is model-theoretic.

**REGISTER B COVERS TRANSFORMATIONS, NOT THE PRINT STEP — and that distinction is the
point of this task.** An earlier version of this section claimed a printer soundness row
"deletes the validation layer" and ranked four options linearly. Both were wrong, corrected
2026-08-03, and the correction changes what this task should promise.

Proving "this printer emits syntax denoting the same proposition" requires a formal
semantics of the TARGET syntax. There is none for SMT-LIB or Coq's parser, and neither is
ours; formalising them means trusting that our formalisation matches the real parser — the
same trust relocated. Why3 does not prove its printers either. So the honest split is:

| step | ours? | status |
|---|---|---|
| obligation → transformed goal | both sides | **provable** — this is Register B's real scope |
| transformed goal → target text | target is not | **validated, plausibly forever** |

That inverts how to treat the validation cost. It is not a temporary embarrassment to be
proved away; it is a permanent cost whose only lever is having ONE printer instead of N. A
standard interchange format is therefore not a convenience but the architecture: SMT-LIB
reaches z3, cvc5, veriT, Alt-Ergo and Yices from a single printer, so there is one thing to
validate rather than one per target.

**And the earlier ranking conflated two orthogonal axes.** Print fidelity and answer trust
remove different things from the trusted path — a verified printer feeding a trusted solver
still trusts the solver; a replayed certificate from a mis-rendered goal still proves the
wrong thing. The best position is both, not one ranked above the other. See the corrected
boundary model in
[docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md](docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md).

What this task should therefore deliver, in order of value:

1. **A typed IR and Register B rows over the TRANSFORMATIONS** — genuinely provable,
   because both sides are ours, and far more tractable than Register A: syntactic claims
   relating two representations of the same term over a small fragment, not semantic claims
   about program execution.
2. **Collapse N printers to one standard-format printer** where the fragment allows,
   because the print step's validation cost is per-printer and permanent. `exprToSmt`
   already is that printer for the arithmetic tier, and its rendering is validated as of
   2026-08-03 (`smtAgreementGoals`, 46/46 on the families fixture, mutation-verified).
3. **Keep per-obligation print validation and make it non-vacuous**, rather than treating
   it as debt. Gate the instance count; a `0/0` "all agree" is the failure mode.

The consequence for kernel diversity, which belongs here because this is where the cost
lives: a native driver per kernel is another printer to validate forever. Kernels address
answer trust only, and only where certificates are impossible, while multiplying print
validation. That is the case against adding a fifth prover, stated as a cost rather than a
preference.

The spike built option 4 because it was reachable without an IR, which was the right call
for a spike and is not the destination. Recording the ranking here so the printer row is
scheduled as *deleting the validation layer* rather than as bookkeeping — and so a future
reader does not add a fifth prover under option 4 believing the marginal cost is a driver,
when it is a driver plus a validator plus a prover run per obligation.

## Area: Backend, Target, And Stdlib Contracts

Goal: make backend/toolchain/stdlib assumptions explicit, and state exactly
where source-level proof stops.

Done when: SSA, target/toolchain, optimization, ABI/layout, stdlib evidence,
and the incremental-driver area incremental build contracts are explicit enough for release
evidence; the supported SSA-to-BackendIR translation slice is independently
replayable; and every boundary after that slice remains explicitly trusted.

### Task R-0278

**Phase:** 2, 4

**Disposition (2026-10-19):** Superseded by R-0503/S11: merge into R-0503; BIR replaces this architecture.

**Objective:** Stabilize SSA as the only backend contract.

### Task R-0279

**Phase:** 2, 3, 4

**Reconciliation (2026-10-19):** Keep; define separate LLVM/native profiles.

**Objective:** Document target/toolchain model: triple, data layout, linker, runtime/startup, libc expectation, clang/llc boundary, sanitizer/coverage hooks.

### Task R-0280

**Phase:** 2, 3, 4

**Disposition (2026-10-19):** Duplicate of R-0421: merge into R-0421; connect certificate obligations to R-0504.

**Objective:** Define optimization policy: allowed optimizations, evidence preservation, debug/release behavior, report/codegen validation. Follow the QBE-style "small auditable backend contract first" principle: prefer a stable, inspectable subset with explicit assumptions over early attempts to match LLVM-level optimization coverage.

### Task R-0281

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; split V1b debugging from workload-triggered production features.

**Objective:** Add native compiled-program debugging and production-crash
diagnosis: DWARF/source-map emission, source-mapped backtraces for runtime
failures, debug-vs-release behavior, optimized-code caveats, and diagnostics
that distinguish source-level runtime checks from trusted backend/OS crashes.
Runtime traps inserted by Concrete (bounds, arithmetic, cast/profile checks,
failed runtime obligations) should carry enough source span and check-class
information that a real application can debug them without reverse-engineering
the abort site. This is not proof evidence; it is usability for
enforced/runtime-checked facts.

Trigger the production half when the first flagships-area flagship or external user
needs to diagnose a deployed binary. Add deterministic symbolication from a
release binary plus its exact debug/evidence bundle, stable build IDs, stripped
binary plus separate-symbol support, stack traces through Concrete/runtime/FFI
frames, core-dump or platform crash-report instructions, and a privacy-aware
redaction mode. A crash report must identify target/toolchain/backend/profile,
binary and source roots, trap/signal class, and whether each frame is exact,
inlined/optimized, runtime helper, foreign, or unavailable.

Gate a source-level trap, foreign crash, stripped release binary, mismatched
symbol bundle, optimized/inlined frame, and missing source checkout. The
mismatched bundle must fail loudly rather than symbolize against the wrong
program; no stack trace upgrades a trusted crash into checked or proved
evidence.

### Task R-0282

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; replace old artifact names with actual IR chain.

**Objective:** Extend the incremental-driver area clean-build versus incremental-build equivalence across backend variants: facts, obligations, diagnostics, reports, BackendIR, codegen units/objects, link manifests, and native behavior must agree under target, ABI/layout, optimization, sanitizer, debug, and toolchain changes.

### Task R-0283

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; testing supports interop, proofs enter V6.

**Objective:** Add ABI/layout round-trip checks: C headers/stubs, offsets, size, alignment and
calling conventions, including the C-compatible adapters generated for Rust and Zig packages.
Execute native/Concrete round trips on every advertised target; type layout calculations alone
do not qualify a target or an aggregate calling shape.

**Semantics decisions (2026-10-21):** with R-0285, this task owns the foreign-name rule of
[FFI.md, "Symbol Names"](docs/platform/FFI.md) (every Concrete-defined function mangled into a
namespace disjoint from C; `#[link_name]` on `extern fn`) and the ownership-on-failure
classification for foreign-boundary APIs ([ERROR_CONVENTIONS](docs/language/ERROR_CONVENTIONS.md)).

### Task R-0285

**Phase:** 3

**Reconciliation (2026-10-19):** Keep four-adapter exit and incremental delivery. Foreign builds run under an execution policy: filesystem, network, inherited environment/secrets, subprocess authority and undeclared generated inputs are controlled, with a hostile-build test (with R-0364). Dependency hashes establish identity, not safe execution.

**Objective:** Make C, Rust and Zig libraries usable as normal Concrete dependencies, with generated
bindings and one build command, before the bulk ethlambda port.

**Semantics decisions (2026-10-21):** every RocksDB slice-1 handle API states what happens to the
handle when it fails (returned, consumed or retained;
[ERROR_CONVENTIONS](docs/language/ERROR_CONVENTIONS.md)), and slice 1 binds C symbols through
`extern fn` (with `#[link_name]` where the C name is not a Concrete identifier) under the
foreign-name rule of [FFI.md](docs/platform/FFI.md), owned with R-0283.

**Scheduling (2026-10-17; amended 2026-10-20):** promoted into the current execution queue after the
active soundness and R-0482 work. Its first deliverable is **RocksDB slice 1**, defined with its
acceptance and its prerequisite checklist under [feature prerequisites](#feature-prerequisites): its
implementation and experiments proceed now, and it is accepted, and its safety advertised, when Phase 1
exits 0b and 2i hold, not when all of row 2 closes. This task owns the user workflow. R-0279 owns target/runtime facts;
R-0283/R-0287 own ABI checks, R-0286 owns exports/library artifacts and R-0289 owns sanitizer
validation. The later full BackendIR, independent verifier and functional-proof milestones are
not prerequisites for a tested foreign-library integration. Changes to evidence formats still
obey their existing owners and compatibility rules.

The outcome is a developer declaring a foreign dependency, using a normal Concrete import and
running the normal build/test commands. Binding selection and generation are automatic for the
supported interface; applications do not write bridge declarations or configure linker flags.
Tooling invokes Cargo, the C build/compiler or Zig's build/compiler, generates bindings, links
native artifacts and reuses unchanged outputs. Record exact dependency revisions/lockfiles,
selected features, bridge inputs, compiler/linker flags, target and artifact
identities. A changed header, bridge, feature or target invalidates the affected output. Failed
generation, unsupported ABI shapes and failed foreign builds produce source-located diagnostics;
they never reuse an older artifact as if the new build succeeded. A public registry is not
required: local binding packages with pinned dependencies establish the first workflow.

**First executable slice and feedback (2026-10-17; planned):**

1. Pin the RocksDB source, target/toolchain and selected C API. Retain the exact
   application/binding manifests, expected lockfile, ordinary imports and commands
   with R-0364/R-0365. Decide the exact-pin, override and native-link conflict
   behavior needed here. Draft workspace and Rust/Zig binding examples alongside
   it; those examples do not require implementing every adapter before this slice.
2. Implement the compiler-owned path from manifest parsing and dependency
   resolution through reviewed binding generation, foreign build/link and the
   runnable Concrete consumer. Exercise open/read/write/close and library-owned
   buffer cleanup through normal build/run/test. Keep the application executable
   throughout; include clean, unchanged warm and populated-cache offline rebuilds.
3. Retain controls for missing toolchains, changed archives, failed generation or
   foreign builds, unsupported targets and incompatible upgrades. Each failure
   names the relevant dependency/input and a valid next action; old generated or
   linked output cannot stand in for the failed new build.
4. Obtain a non-author trial as soon as this slice works. Record setup and manual
   intervention, maintained binding code, build/cache and copy/allocation costs,
   diagnostic quality and concrete friction. Turn each blocker into a minimal
   reproducer, owned fix and regression; use those results to adjust the shared
   implementation before adding Rust crypto, the selected upstream Zig operation
   and the bounded libp2p runtime adapter. Retain the same feedback for each slice.

Automate a fresh-consumer CI case outside the compiler checkout as soon as the
first slice runs. Fetch the exact candidate origin/revision and toolchain,
including fork-PR origins; use only declared dependencies and the installed
workflow. Compare actual results and expected failures, including resource
cleanup, rather than accepting a build or output marker alone. Record setup,
build and run timings on each claimed platform, with clean/warm/offline checks
as applicable. R-0352/R-0354 reuse this consumer for distribution acceptance.

Report the scope as each slice becomes usable. The first consumer supplies
engineering feedback; it does not close the four-adapter checkpoint or prove
foreign behavior. Broader package work grows from the observed requirements.

**Companion FFI and resource examples (2026-10-20; planned).** Four examples exercise linear handles
and exit 2i's rules (no reference stored in an aggregate, no block `let` shadowing a visible name) at
the foreign boundary, each with its own leg in `scripts/tests/check_ffi_companions.sh` (new, in the full
gate set):

- `examples/ffi_file_handle`: a linear file handle wrapping the C `open`/`read`/`write`/`close` calls,
  consumed by a `close` that returns its `Result`; a written file is read back, and a missing path
  returns a typed error.
- `examples/ffi_callback_context`: a small vendored C library that stores a callback and its context
  pointer and calls them later; the registration is a linear handle whose release unregisters it, and
  the context outlives the registration (the binding contract's callback retention).
- `examples/ffi_resource_pool`: a fixed-capacity pool of foreign handles checked out and returned as
  linear values; exhaustion returns a typed error, and shutdown releases every handle exactly once.
- `examples/ffi_error_release`: a multi-step acquisition (two foreign handles and a foreign buffer)
  in which every step can fail on request; every failure path releases exactly what was acquired.

Each leg compares the native output with a checked-in expected result, and the interpreter's where it
supports the foreign calls (otherwise the leg records that it is native-only); the foreign side counts
acquisitions and releases, and the leg asserts one release per handle on every path. Compile-fail
fixtures per example stay rejected: a handle not released on one error path, a handle used after
release, a reference stored in a handle aggregate, a block `let` shadowing a live handle, and a
context destroyed while its callback registration remains active. Their failure paths also run under
R-0030's [shared fault-injection checkpoint](#task-r-0030) (2026-10-21). The examples use today's explicit foreign
declarations and move to generated bindings when this task's workflow covers them. They are
companions accepted with or after RocksDB slice 1, not prerequisites of it; their 2i rejection
fixtures join when exit 2i lands.

**Failure and resource contract (2026-10-18; planned):** for each selected
fallible operation, state what success and each recoverable error mean for
ownership, partial progress, observable state and retry. An owned input is
returned with a stated resource state, retained by a named owner, or released;
ownership never disappears behind an error code. Borrowed mutable state has a
stated preservation or partial-update rule. Use
ordinary `Result`, records/enums, scoped borrows and explicit cleanup first.
An error does not imply rollback, and an external operation whose outcome is
unknown must say so rather than authorize a blind retry. Retain upstream
guarantees and reviewed assumptions with their provenance; generated wrappers
cannot manufacture atomicity or recovery guarantees absent from the foreign API.

In the first RocksDB slice, exercise a real recoverable error and the exact
ownership/release behavior of its results and handles. Where the selected API
supports partial progress or retry, retain an independent observable control;
otherwise state that limit and use the first applicable writer/runtime adapter
for that case. Gate successful use, controlled failure, exactly-once cleanup,
and safe resource recovery or explicit consumption. R-0488 reuses the contract
for ordinary stdlib operations. More expressive `Result`-case, `old` and frame
proofs belong to R-0473/R-0474 and the queued two-state contract work; they are
not entrance requirements for the tested consumer. Fatal abort, OOM and foreign
unwinding retain their existing separate policies. No automatic rollback,
scope-end destruction or general exception machinery is introduced.

**Annotation reuse and generated checks (2026-10-18; planned):**

- Reuse supported upstream bounds, nullability, no-escape and lifetime annotations
  before requesting new maintainer facts. Keep bounds and lifetime facts separate;
  map them only to relationships Concrete can express and check today. Upstream
  assertions retain their source and assumption status rather than becoming proofs.
- Store missing facts in the reusable binding package's external metadata, without
  editing upstream sources. Bind each reviewed fact to its selected declaration,
  signature, source revision, target/configuration and relevant input hashes.
  Conflicting annotations or overlays require an explicit maintainer resolution;
  a declaration name alone never identifies a reusable ownership contract.
- Generate only the selected public API and its required type/header closure.
  Feed extracted annotations and reviewed facts into the existing shared boundary
  description; derive native adapters, Concrete wrappers, docs and conformance
  harnesses together. Avoid importing an entire dependency's unsupported internals.
- Validate generated ABI claims against the actual foreign compiler and native
  calls, alongside Concrete callers. Retain independent behavior/error vectors and
  ownership rejection cases; generated tests do not prove the metadata they consume.
- Diagnostics identify the exact missing fact, such as whether an argument is
  retained or which function releases a returned buffer. Save the reviewed answer;
  ordinary application builds remain noninteractive. An upgrade requests review
  for affected facts and reports API/ownership/effect changes, while a stable
  signature alone does not establish unchanged foreign behavior.

Implement the metadata subset needed by the first RocksDB slice, then expand it
using that slice's feedback. A complete foreign-annotation vocabulary is not an
entrance gate. [FFI_GLUE.md](docs/platform/FFI_GLUE.md) records the Swift/D importer,
CXX/UniFFI generation and bindgen API-selection precedents and acceptance controls.

**Ease-of-use acceptance:** the application workflow is dependency -> ordinary import -> build.
Prefer a reviewed binding package automatically when one is available. For an uncovered library,
discover its C headers, supported Rust package/API metadata or selected Zig module metadata and
generate the supported native adapter, Concrete wrappers and boundary documentation together.
Use one versioned internal boundary description for generation, checks, docs and harnesses;
retain whether each fact was compiler-derived, checked or reviewed as an assumption. Validate
Rust/Zig adapters against the actual upstream package with its pinned compiler. An explicit
bridge interface remains an advanced escape hatch, not a mandatory handwritten step for every
application. Report the supported/unsupported API surface
and retain original documentation/source links for completion and diagnostics.

Ownership, effects or lifetime facts absent from an upstream interface are binding-maintainer
questions. Infer only established facts; generate specific requests or a draft description for
the missing ones, then retain the reviewed description for every downstream application. Name
patterns alone cannot establish pointer ownership or authorize a capability-free declaration.
Already reviewed facts are not requested again per application or build. Upgrade review is
limited to affected interfaces and facts; a changed declaration cannot silently inherit an old
ownership assumption. Never silently fall back to unsafe raw calls or serialize/copy every
buffer to make generation appear successful;
record required conversions and copies and support call-scoped borrowing where its contract is
known.

The non-author trial starts from a clean application and exercises initial setup, normal imports,
build/run/test, warm and populated-cache offline rebuilds, and a dependency upgrade. For the four
acceptance dependencies, require zero handwritten application FFI/bridge code, zero application
build scripts, zero edits to generated files and zero manual compiler/linker flags. Keep any
maintained custom adapter code counted in its binding package and in the port inventory.
Cached binding/native artifacts must match their pinned inputs;
an API/ABI change produces an actionable source-located error before it becomes a runtime call.

**Binding contract:**

- C headers supply declarations and target-specific layout facts. An explicit binding description
  supplies allocation/free pairs, owned versus call-scoped borrowed values, buffer lengths,
  retained pointers, errors, blocking/threading requirements and external authority. Unknown
  ownership or effects do not become safe, capability-free declarations through generation.
  The binding package records the supported upstream build recipe once when C package metadata
  is insufficient; applications do not maintain native build instructions.
- A narrow typed Rust bridge selects concrete functions/types and generates Rust C-ABI shims
  plus Concrete declarations from one interface. Check the shim against the real Rust API.
  Begin with scalars, records/enums, `Option`/`Result`, bytes/UTF-8 and opaque objects through
  explicitly defined ABI representations; arbitrary Rust ABI, generic APIs and trait objects
  do not cross the boundary directly. Unsupported forms refuse with an adapter diagnostic.
- Zig packages reuse existing C-compatible exports or generate exported adapters around
  selected public operations. Use supported compiler/comptime metadata with concrete
  instantiations; compile against the pinned module and resolve its `build.zig`/`build.zig.zon`
  graph. Lower slices, error unions, optional values and allocator-backed objects to explicit
  ABI representations; keep native allocator and internal layout details inside Zig. A public
  declaration alone does not establish safe ownership. No manual Zig glue is required in an
  application using a supported binding package.
- Generated safe APIs use Concrete's existing ownership and capability model:
  explicit consuming close/drop, scoped borrows, matching library deallocation and capability-
  bearing handles. Fallible cleanup retains an explicit result. Generation does not introduce
  implicit scope-end destruction, returned references or undeclared authority. Foreign behavior
  and ownership assertions remain named assumptions unless independently checked.
- Start the shared ABI subset with scalars, pointer/length buffers, opaque handles and
  status/out-parameter calls. Advertise aggregate forms only after target-specific round trips.
  Record Rust panic policy and Zig panic/trap/OOM behavior; ordinary errors become `Result`,
  while process aborts and unsupported unwinding do not become recoverable errors by generation.
- Networking adapters may keep their runtime in Rust or Zig and expose typed events and bounded
  queues with poll/wait, cancellation and shutdown. Specify full-queue behavior, thread affinity,
  callback retention and completion/cancellation lifetimes. This bounded bridge does not require
  or establish a general Concrete concurrency memory model.
- Generated headers, imported declarations, host stubs, symbol names, calling conventions,
  ownership rules, capability labels and trust assumptions round-trip through C/Rust/Zig harnesses
  and Concrete callers. Check ABI size/alignment/offsets and observable calls on each claimed
  target. Keep generated output deterministic and source-linked, with its reviewed inputs and
  native dependencies visible in build/audit metadata. Reference decision:
  [FFI glue](docs/platform/FFI_GLUE.md).
- Integrate transitive libraries and runtime/deployment requirements into the normal build graph.
  Cache the dependency/header closure, toolchains/sysroot, target/CPU, features, build mode,
  generator version, reviewed description and declared build inputs. Unchanged warm builds do
  no binding regeneration or foreign recompilation. Missing inputs cannot justify a reusable
  artifact; failed builds never fall back to stale linked output. Show upgrade diffs for APIs,
  effects, reviewed facts and copy/allocation behavior, with source-located errors.

**Acceptance workload:** use RocksDB's C API for open/read/write/close and buffer ownership; a real
operation from ethlambda's Rust crypto stack for typed input/results and errors; the smallest
libp2p adapter needed to exchange messages between a Concrete caller and a Rust peer; and one
pinned upstream Zig library for a real compression or hashing operation plus allocator-backed
output/cleanup. Select and record the Zig upstream/revision before claiming its gate; a local
arithmetic shim does not qualify. Retain clean, warm and offline builds, ABI/ownership and
malformed-input controls, queue saturation, cancellation and
shutdown controls, and applicable sanitizers. A non-author must build and run these through the
normal project workflow without hand-editing generated glue or invoking native build tools manually.
Measure setup steps, binding/adapter code maintained by hand, build cost and actionable diagnostic
quality. Register mutations for the authority, ownership and build invalidation rules the gates claim to
enforce. Include wrong-deallocator, use-after-close, retained-borrow, missing-capability,
native-build failure, stale-cache and incompatible-upgrade controls. Retain the selected boundary
format and supported API/target matrix with the acceptance record. This bounded exit unlocks
R-0490; it does not require wrapping every dependency in advance. Implementation remains pending.

**Clean-machine package workflow (2026-10-21; graduation criterion).** On a clean machine (a fresh
runner or container with only the documented installer), one scripted exercise installs the toolchain,
resolves the pinned dependencies, builds the native libraries the manifest declares (the `[native]`
section of `Concrete.toml`, in progress on branch `native-libs`), runs the tests, upgrades one
dependency, and rolls the upgrade back to the previous lock, with the build and tests passing at both
ends and the rolled-back lock reproducing the original artifacts. RocksDB slice 1's acceptance already
covers the install-to-test half (a fresh clone, normal commands, no manual flags); the upgrade and
roll-back half is required before the four-adapter checkpoint closes, and R-0365's lockfile exit
reuses the same exercise.

### Task R-0490

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; distinguish bounded feedback from full-port completion. Functional proofs of serialization, state transitions, fork choice and signing are bounded later increments with their own owners and exits; the Silver shared-state workload is a separate deferred record under R-0415.

**Objective:** Port the complete client-owned ethlambda implementation to Concrete, get it running
and tested, and use concrete blockers to improve the language before proving client components.

**Status (2026-10-17; amended 2026-10-21): planned; follows the bounded R-0285 interop checkpoint.**
Until V1b's composed native exit holds, this task runs bounded component workloads inside the feature
track's single slot ([capacity allocation](#capacity-allocation)); the bulk port starts after V1b, or
earlier only by a dated decision recorded when V1b is blocked on LCB's external milestones. The exit
below is unchanged. The initial
reference is [ethlambda at afc491b2](https://github.com/lambdaclass/ethlambda/tree/afc491b28d12de8c161fb5cd140b160ebf6d965d).
Pin its protocol/specification and fixture revisions before measuring parity. Advancing a pin is
an explicit compatibility change, with differences retained rather than absorbed into new goldens.

The port covers client-owned protocol and application logic: types/serialization, consensus state
transitions, fork choice, storage orchestration, networking orchestration, validator duties and
operational build/run/test tooling. Existing crypto, storage and transport/runtime dependencies may
remain behind R-0285 bindings. Inventory every retained native dependency and handwritten shim;
client consensus logic left in a foreign helper is unported work, not a completed Concrete component.
Reimplementing dependency internals is not part of initial parity.

Keep the node executable through successive replacements. Start with a Concrete component called
from the Rust client through R-0286, then move consensus processing and surrounding application
logic into Concrete. The existing Rust client supplies differential executions while pinned
protocol fixtures supply independent expected outcomes. For every completed slice retain accepted
and rejected inputs, state/root and error behavior, resource/ownership checks and appropriate
latency/allocation measurements. State precisely what happens to owned state after an error or
cancellation. Test bounded work and malformed peer inputs rather than accepting process aborts as
ordinary protocol rejection.

Each language/library gap found in the port gets a minimal reproducer, an owned fix and a permanent
regression. Preserve awkward-but-working cases in the friction record, including annotations,
callbacks/context passing, cleanup, diagnostics, compilation and debugging. R-0485/R-0488 own
related ergonomics and parameter-mutation work; the port supplies their forcing cases. Add SSZ,
bounded collections, narrow const generics or other language support only as actual cases require.
General concurrency remains research-gated; bounded foreign-runtime adapters have their own
explicit contracts. Record protocol parity, resource costs and open defects rather than treating
translated lines or a successful compilation as completion.

**Exit:** a Concrete node clean-builds and runs, syncs from peers, proposes, attests and finalizes
in a retained mixed-client devnet against the pinned reference. Preserve conformance/differential
results, restart and bounded-queue/failure scenarios, native dependency inventory, operational
instructions and measured resource costs. All remaining client-owned Rust logic and unsupported
scenarios are accounted for before calling the full port complete.

Full functional proofs are a subsequent increment, starting with serialization, state transitions,
fork-choice invariants and signing constraints once their models are supported. During the port,
ownership/capability enforcement, compiler regressions, protocol tests and honest evidence labels
remain mandatory. Existing verification release criteria and the three verified public flagships
are unchanged. Silver is a later, separate systems pressure test; its shared-state and thread
architecture does not expand this task's exit.

### Task R-0286

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; distinguish task “V1” from verified slice V1.

**Objective:** Add outbound FFI / embeddability as a first-class backend/ABI contract.

**Semantics decisions (2026-10-21):** the export attribute is the only way a Concrete-defined
function gets an unmangled C name ([FFI.md, "Symbol Names"](docs/platform/FFI.md)); every other
Concrete function is mangled.

 Concrete's most likely early adoption path is not replacing a whole
 process; it is exporting a small verified parser, protocol checker,
 constant-time helper, or evidence-bearing subsystem into an existing
 C/C++/Rust/Zig/Go host. Today the FFI story is mostly inbound
 (Concrete calling libc or trusted host functions). The roadmap must also
 cover Concrete code callable from outside.

 V1 deliverables:
 - `#[export]` or equivalent stable export annotation with explicit symbol
   naming / no-mangle policy;
 - `staticlib` and/or `sharedlib` output mode, with the supported target
   triples named;
 - generated C header for the exported ABI subset;
 - exported-function ABI classification: scalar, pointer, small/large
   aggregate, enum/result, string/bytes/path, ownership transfer, and
   unsupported forms;
 - generated boundary docs that expose capabilities, allocation authority,
   failure mode, trusted/Unsafe dependencies, and evidence class for each
   export;
 - evidence bundle next to the library artifact, so a host build can audit
   what the embedded Concrete component claims.

 Hard rule: exported functions must not hide Concrete's thesis at the C
 boundary. If an export allocates, needs a capability, can fail, depends on a
 trusted wrapper, or has only tested/runtime-checked evidence, the generated
 header/docs/report must say so. Gate: one C harness calls a pure exported
 parser/checker, one harness calls an exported fallible API, and one negative
 fixture proves an unsupported export shape fails with a diagnostic rather
 than silently choosing an ABI.
### Task R-0287

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep as R-0283’s concrete classification suite.

**Objective:** Add a C ABI classification suite, inspired by QBE's explicit ABI contract:

scalar parameters and returns, small and large structs, arrays, nested
aggregates, alignment/padding, by-value versus by-reference lowering,
varargs policy, calling convention labels, symbol names, and ownership/
capability/trust annotations. Wire `scripts/tests/check_c_abi_matrix.sh`
with generated C callers and Concrete callers, plus Rust/Zig adapters and callers for the
package interop subset. Keep target-specific positive and unsupported-shape controls.
### Task R-0288

**Phase:** 2, 4

**Disposition (2026-10-19):** Superseded by R-0503/S11: merge inspection, verification and parity requirements into R-0503.

**Objective:** Add `BackendIR` as a structured backend contract between `ValidatedSSA` and LLVM/native emission. This is useful even if Concrete never ships its own native backend: it gives the compiler a typed, inspectable place to preserve runtime checks, trap/source-span facts, layout/ABI decisions, target constants, trusted/runtime helper calls, and capability/trust labels before they disappear into LLVM text or target code. The intended ladder is `ValidatedSSA -> BackendIR -> ValidatedBackendIR -> EmitLLVM` first; later emitters (native, C, WASM, QBE-style) may consume the same `ValidatedBackendIR` only after the contract is stable.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0288).

### Task R-0289

**Phase:** 3, 4

**Objective:** Add sanitizer-backed generated-code validation for trusted/FFI/layout/ pointer-heavy examples, plus the stdlib sanitizer/runtime hooks from the proof-status area. Sanitizer findings are `runtime_checked` / `tested` evidence, not proof.
### Task R-0290

**Phase:** 1, 2, 4

**Disposition (2026-10-19):** Duplicate of R-0491: merge into R-0491, retaining backend/profile matrix.

**Objective:** Add backend/codegen differential validation where executable oracles exist.

### Task R-0291

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; distinct from generated-program performance.

**Objective:** Add compiler self-leak/resource soak harness for long-running workflows.

### Task R-0292

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; link R-0358 policy and R-0509 verification/audits.

**Objective:** Harden stdlib stability and evidence policy from the stdlib area: which stdlib functions are trusted, proved, enforced, allocation-free, capability-free, or assumption carriers.

### Task R-0293

**Phase:** 3, 4, 6

**Reconciliation (2026-10-19):** Keep practical contracts early; richer specifications later.

**Objective:** Define stdlib contracts for allocators, I/O handles, directory/file/path handles, byte/text/path conversion APIs, and fallible return discipline.

 Each public stdlib function must state allocation behavior, OS authority,
 failure mode, trusted platform assumptions, and evidence class. This item
 also owns the release-facing unsafe/trusted boundary UX: a safe wrapper may
 narrow authority, but reports must still show the underlying trusted,
 extern, raw-pointer, or `with(Unsafe)` operation and the assumption it
 depends on.
### Task R-0294

**Phase:** 3, 4

**Objective:** Add stdlib evidence gates so core helpers cannot silently widen authority, allocation, proof assumptions, or runtime-error obligations.

### Task R-0295

**Phase:** 2, 4

**Disposition (2026-10-19):** Superseded by R-0501/S10: retire workload gate; MIR is already an architectural decision.

**Objective:** Evaluate a normalized mid-level IR only when traceability/backend-contract reports expose a concrete gap.

### Task R-0298

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; make assurance-profile selection explicit. Core claim validity is backend-independent; binary-transport assurance necessarily depends on the profile/backend.

**Objective:** Define backend-default policy explicitly if multiple production backends graduate: defaults are a versioned `(target, build_mode) -> backend` table, visible in help/audit/build receipts and overridable by an explicit flag. Changing a default requires clean-build/incremental equivalence, behavior and optimization matrices, performance data, a rollback switch, and release notes. Backend availability must never change source-language semantics, proof status, diagnostics before codegen, or package resolution.

### Task R-0299

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; retarget old SSA/BackendIR language to S12.

**Pulled forward 2026-10-18:** this task joins R-0504 in stage S12 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), validating the LLVM profile; its ordering there takes precedence over the backend-contracts area placement below.

**Objective:** Add translation validation for codegen as the path out of a fully trusted backend. V1 should validate a narrow `ValidatedSSA -> BackendIR -> ValidatedBackendIR` subset per compile:

 integer arithmetic, fixed arrays, structs, direct calls, branches, bounded
 loops, runtime checks, capability calls, and source-map annotations. The
 validator compares checked Core / typed IR / SSA facts against BackendIR
 facts and reports one of: `translation_compiler_validated_v1`, `translation_trusted`,
 `translation_blocked`, or `translation_mismatch`. This is not a promise to
 prove the whole LLVM/native stack; it is a per-artifact check that the
 Concrete lowering into the backend contract preserves the facts Concrete
 claims. Add `examples/translation_validation/` with straight-line,
 branch/loop, struct/array, runtime-check, and deliberate mismatch fixtures.
 This is the roadmap's early backend-soundness probe: start with the small
 subset as soon as SSA/backend facts are stable enough, rather than waiting
 for a full native-backend story before learning whether Concrete's evidence
 can cross the backend boundary.
 Prior art: CompCert for proved compilation/pass-correctness discipline and
 Alive2 for SMT-checked LLVM IR equivalence/optimization validation. The
 concrete threat model here is the class differential fuzzing has already
 exposed: source/Core facts can be true while a backend lowering silently
 emits a different native artifact unless that boundary has its own check.
 Wire `scripts/tests/check_translation_validation.sh`; the gate must prove a
 Lean-proved/Core-level claim cannot be presented as native-code evidence
 unless the backend artifact is either translation-validated or explicitly
 backend-trusted in the audit bundle.

**Actual emitted LLVM increment (planned 2026-10-18):** extend validation beyond
the existing `ValidatedBackendIR` boundary with a separate, explicitly scoped
relation to the emitted `.ll` bytes. Follow the
[analysis and acceptance experiment](research/compiler/sparkle-emitted-code-validation.md#bounded-emitted-llvm-experiment):
retain checked SSA, generate LLVM normally, parse the actual output with a
separate reader and compare independent source/LLVM semantics for every input
in the admitted domain. The first case is checked `u8` increment: `10 -> 11`,
`254 -> 255`, and `255 -> required overflow failure`. A silent wrap to zero must
refuse. Start with integer/boolean functions and acyclic branches, and include
the real checked-arithmetic helper bodies and their required call closure;
checking helper names or the producer's pre-render AST alone is insufficient.

Retain controls for a corrupted helper, wrong width/signedness, changed branch
or return, missing failure edge, new LLVM poison/undefined behavior, malformed
output and unsupported instructions. Unsupported shapes, timeout and checker
errors remain distinct incomplete results and cannot grant a validated verdict.
Keep finite-domain checking, solver trust and kernel-checked proof separate.
Parser/reflection soundness and model-to-source correspondence remain explicit
assurance obligations. A direct probe of the current SSA emitter is permitted;
the full BackendIR migration is not a prerequisite for learning from it.
LLVM optimization and LLVM-to-machine-code correctness are separate edges.

### Task R-0300

**Phase:** 2, 4, 5

**Reconciliation (2026-10-19):** Keep distinct relation versions and call-closure accounting.

**Pulled forward 2026-10-18:** this task joins R-0504 in stage S12 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md), validating the LLVM profile; its ordering there takes precedence over the backend-contracts area placement below.

**Objective:** Make the backend-contracts area translation-validation slice independently replayable.

 Extend the soundness-bridge area `concrete-cert` target with a versioned canonical
 `BackendTranslationCertificateV1` over hash-bound
 `ValidatedSSA -> BackendIR -> ValidatedBackendIR` artifacts. The checker
 validates the explicit input/output relation; it does not trust
 producer-emitted `preserved` flags or compare two summaries produced by the
 same lowering pass. Its operator, trap, intrinsic/helper, ABI-layout, and
 target specifications must be independent declarative definitions; it may
 not reuse Lower/Emit/Layout decision helpers whose common-mode bug it is
 intended to catch.

 V1 covers integer width/signedness and operator selection, bools, direct
 calls and signatures, returns, branches, fixed-layout structs/arrays,
 required arithmetic/bounds/runtime traps, layout/target constants,
 source-map identities, helper/intrinsic calls, and capability/trust labels
 for the subset admitted by Task R-0299. V1 admission is whole-function: if an
 unsupported operation can influence control flow, memory, calls/arguments,
 or the return value, that entire function is `translation_trusted` or
 `translation_blocked`. Region-level validation is forbidden until explicit
 compositional boundaries and a composition theorem exist. Track call-closure
 separately: a caller may satisfy the local V1 lowering relation while a
 reachable callee remains backend-trusted, so that is not end-to-end validated
 function behavior.

 Bind the incremental-driver area codegen-unit manifests and object/link identities into the
 dependency root with explicit edge status. The independent V1 **relation**
 check stops at `ValidatedBackendIR`: `BackendIR -> object` and
 `object -> binary` are `identity_bound_only` / `backend_trusted`, not
 semantically checked. LLVM optimization, LLVM text-to-object
 translation, object writer, linker, ABI/runtime behavior, and native
 execution remain trusted/tested unless a later validator explicitly covers
 them. Object and binary hashes prove identity, not semantics.

 Mutate signed versus unsigned division, integer width, required overflow or
 bounds trap, branch target, field offset/layout, direct-call target/signature,
 capability/trust label, source-map id, target/profile, codegen-unit owner,
 object/link manifest, and certificate/build binding. Include an unsupported
 instruction mislabeled as validated. Independent replay must reject each
 checked-relation mutation without calling the producer backend. Also combine
 BackendIR A with object B and consistently rehash a fresh internal graph: in
 the absence of an externally expected root the checker must report an
 unvalidated backend edge, not claim semantic mismatch detection. Wire
 `scripts/tests/check_backend_translation_certificates.sh`.

**Emitted-LLVM replay increment (planned 2026-10-18):** R-0299's emitted-text
relation is distinct from the V1 relation above and must not reinterpret an old
certificate as covering output it never checked. Use the existing canonical
task/evidence objects with a separately versioned relation/profile. Bind the
exact checked input, emitted bytes, selected function and actual helper/callee
closure, target/arithmetic semantics, domain, checker/rules, proof artifacts and
assumptions. Independent replay parses the retained output and checks the
relation without calling producer Emit/Lower helpers. Gate a valid bundle,
wrong/stale output, wrong target/profile, a modified helper and a consistently
rehashed but semantically wrong bundle. Incomplete parsing or checking cannot
be hidden by a matching hash or a partial set of obligations.

Report the independently checked edge precisely: checked SSA/BackendIR to
emitted LLVM, followed separately by LLVM optimization, machine-code emission,
object/linking and runtime/ABI boundaries. LLVM transformation checking such as
Alive2 keeps its supported scope and solver trust; it does not become a Lean
proof or a Concrete-to-LLVM proof by relabeling. Object/binary identities remain
identity-only until their own semantic edge is checked. Extend hostile schema
fixtures and the R-0353 consumer when this profile graduates; do not freeze
producer-only bytes or mint stronger receipts during the development probe.

### Task R-0301

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep as bounded checkpoints, not a monolithic phase gate.

**Objective:** Add the backend-contracts area validation artifact: a backend/std-lib contract project with ABI/layout C round trips, C/ABI glue generation/import checks, the C ABI classification matrix, alignment-fact fixtures if Task R-0284 is pulled into implementation, backend-IR emission/verifier checks, producer translation-validation checks, independent `BackendTranslationCertificateV1` replay and mutations, sanitizer runs, compiled-oracle differential tests, native debug/source-map smoke tests, clean-vs-incremental fact equivalence, cached-object substitution negatives, and stdlib

 authority/allocation/evidence gates. The artifact must show the exact point
 at which independent checking stops and backend/toolchain trust begins.
 Include R-0299/R-0300's actual emitted-LLVM fixture and independent replay once
 that profile graduates, with overflow-helper and output-substitution negatives;
 keep its coverage separate from the BackendIR V1 and native-code edges.
### Task R-0302

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; connect candidate identity to R-0357.

**Objective:** Pin the supported LLVM/clang version and gate version drift. The backend today targets whatever `clang` defaults to on the host (`docs/language/EXECUTION_MODEL.md`), yet the TCB already conditions backend correctness on "LLVM version" — so a floating toolchain leaves the trusted boundary a moving, un-audited target and makes native output non-reproducible across hosts (`docs/compiler/DETERMINISM.md` already disclaims binary reproducibility for exactly this reason). Declare one supported LLVM version (or a narrow tested range) as part of the backend contract, and make an upgrade a deliberate, gated event: on a version bump the wrong-code corpus, the backend-contract gate, and IR golden tests must re-pass before the new version is

 declared supported. This is the cheap control that turns the moving boundary into
 a fixed one; the structured `BackendIR` contract (Task R-0288) and a future hand-written
 LLVM bitcode emitter (the Zig/Roc decoupling technique) are the heavier long-term
 options, pulled only if version pinning proves insufficient. Wire the check into
 `scripts/tests/check_backend_contracts.sh`: the compiler refuses or warns on an
 unsupported LLVM version, and the drift corpus re-runs on a bump.
 - 20a. LLVM bitcode emitter — the heavy decoupling rung (pull-gated, deliberately
   NOT its own phase). *What it is:* emit LLVM **bitcode** (LLVM IR's stable
   serialized binary format) from a hand-written serializer that never links or
   calls the churning LLVM C++ API. Zig's self-hosted compiler is the reference
   implementation — it writes bitcode in Zig and hands it to whatever LLVM is
   present, making it immune to the C++-API breakage that costs most frontends an
   upgrade tax; Roc reused Zig's bitcode writer wholesale in its 2026 Rust→Zig
   rewrite. Concrete today emits *textual* LLVM IR to `clang`
   (`Concrete/Backend/EmitLLVM.lean`), which already avoids C++-API linkage but
   stays coupled to textual-IR *syntax* drift across LLVM versions (the
   opaque-pointer migration was one such event). A bitcode writer would remove
   that coupling and enable `clang`-free, bit-identical cross-host emission.
   *Why an item and not a phase, and why deferred:* (1) it is a sub-component of the
   backend contract that gates nothing downstream; (2) it is *less* proof-relevant
   than translation validation (Task R-0299) — it buys decoupling/reproducibility,
   not correctness — so elevating it above Task R-0299 would invert priorities for
   a proof-carrying compiler; (3) the cheap version pin (Task R-0302) already fixes the moving
   trusted boundary, and LLVM churn has not been shown intolerable (one migration
   eaten so far), so a phase-sized commitment now would be speculative scheduling.
   *Pull-trigger* — promote to scheduled work when EITHER a freestanding / no-`clang`
   target is pursued (the freestanding area), where self-contained emission stops being optional,
   OR LLVM version churn recurs intolerably despite Task R-0302's pin. Same ladder as
   `BackendIR` (Task R-0288): `ValidatedSSA → BackendIR → ValidatedBackendIR → EmitLLVM`
   today; bitcode / own-backend later. Prior art: Zig (hand-written bitcode writer,
   self-hosted backend), Roc (reused it in the Zig rewrite).
### Task R-0304

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep as shared acceptance constraint.

**Objective:** Keep inbound FFI, outbound FFI, and unsafe-profile work connected but distinct. Inbound FFI says how Concrete safely calls host code; outbound FFI says how host code safely calls Concrete exports; the unsafe/trusted-profile area says how the trusted/raw-pointer implementation islands remain auditable. Release-facing docs must not collapse these into a vague "FFI supported" claim. Each side needs its own examples, ABI restrictions, ownership-transfer rules, capability/evidence report, and red-team negatives.


## Area: Unsafe Concrete And Trusted Systems Profile

Goal: make deliberately unsafe Concrete code boring to write, easy to audit, and
hard to misuse. This phase borrows the useful Zig lesson: low-level programs
need excellent tools for pointers, slices, allocators, debug allocators, and
runtime instrumentation; the answer is not a global unsafe mode or Rust-style
lifetime algebra, but small explicit unsafe islands with visible authority and
replayable checks.

Done when: every raw-pointer, unchecked, trusted, allocator, and FFI escape hatch
has a named safe wrapper pattern, audit/report output shows the underlying
assumption, debug/profile instrumentation can catch representative misuse, and a
validation project proves safe code cannot reach these operations accidentally.

This phase does **not** weaken Concrete's philosophy. Safe Concrete remains
linear, capability-visible, second-class-reference, and no-hidden-cleanup.
Unsafe Concrete is a deliberately marked profile for the code that must cross
those boundaries: runtimes, allocators, FFI, VM/interpreter internals,
freestanding/MMIO, sanitizer probes, and backend/ABI glue. Its evidence class is
`trusted`, `runtime_checked`, `tested_by_oracle`, or `assumed` unless a later
proof actually discharges the exact claim.

Reference languages and reading:
- Zig for pointer/slice taxonomy, allocator-passing, arena/fixed-buffer/debug
  allocators, and the "prefer slices over raw many-item pointers when length is
  known" rule:
  `https://ziglang.org/documentation/master/` and
  `https://zig.guide/standard-library/allocators/`.
- Hare for checked slices by default, unbounded arrays as an explicit escape
  hatch, small Unix-shaped wrappers, and honest docs about what remains manual:
  `https://harelang.org/blog/2022-06-21-safety-features/` and
  `https://harelang.org/blog/2021-02-09-hare-advances-on-c/`.
- Odin for tracking allocators, sanitizer/runtime-instrumentation pragmatism,
  foreign-system ergonomics, layout/endian control, and fine-grained
  bounds-check controls:
  `https://odin-lang.org/docs/overview/` and
  `https://odin-lang.org/docs/faq/`.
- Rust/Rustonomicon as cautionary input: unsafe reference/aliasing validity is
  subtle, pervasive unsafe can become harder to audit than raw pointer code, and
  Concrete should avoid silently upgrading raw pointers into ordinary safe
  references:
  `https://dev-doc.rust-lang.org/beta/nomicon/what-unsafe-does.html` and
  `https://doc.rust-lang.org/stable/reference/behavior-considered-undefined.html`.
- Counterpoint reading, to keep the claims honest:
  `https://rustmagazine.org/issue-3/is-zig-safer-than-unsafe-rust`.

Borrow the techniques, not the philosophy. Concrete must not adopt hidden
authority, implicit allocation, implicit cleanup, lifetime syntax, trait-object
unsafe abstractions, ambient context, or a broad "unsafe mode." Debug allocators,
sanitizers, and runtime checks are `runtime_checked` / `tested` evidence, never
proof. Unsafe ergonomics are allowed only when they make the trusted assumption
more legible in source and reports.

### Task R-0312

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep core decisions before memory interfaces freeze; surface breadth in V6.

**Objective:** Freeze the unsafe-surface taxonomy:

- safe references remain second-class and may not be returned;
- raw single pointers, many-item pointers, fixed-size array pointers, and
  slices are distinct source/report concepts;
- nullable pointers are explicit (`Option<Ptr>`-style), not the default;
- every raw pointer/slice carries mutability, provenance/trust class,
  alignment fact where known, and optional length/bounds facts where known.
Add `docs/UNSAFE_CONCRETE.md` and make it point back to the provable-subsets area's
`UNSAFE_ISLAND.md`, not duplicate it.
The design note must include a comparison table mapping Zig's `*T`, `[*]T`,
`[*c]T`, `?*T`, slices, fixed array pointers, Hare unbounded arrays/slices,
and Odin pointers/slices onto Concrete's chosen surface. Every row must name
whether bounds/null/alignment/provenance are statically known, runtime-
checked, trusted, or unavailable.
### Task R-0313

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; obey prohibition on returned/stored safe references. Safe wrappers use scoped/downward construction or owned wrappers; no recipe returns or stores a safe reference.

**Objective:** Add safe-wrapper recipes for the common unsafe shapes: checked slice from raw pointer + length, bounds-checked indexed access, non-null construction, alignment-refined pointer construction, FFI-owned handle wrappers, allocator-backed buffers, and raw-place writes. Each recipe must show the public safe wrapper, the private trusted/Unsafe operation, the accepted assumption, and the report line that exposes it.

### Task R-0314

**Phase:** 3, 4

**Objective:** Add a debug allocator / checked allocator profile inspired by Zig's development allocators: allocation/free stack identity where available, double-free detection, use-after-free-like sentinel checks where applicable, leak reporting for tests, red-zone or canary checks where practical, and allocator-name reporting in audit output. This is `runtime_checked`, not proof evidence. It must work with Concrete's allocator-as-value direction:

`with(Alloc)` grants authority; allocator values name strategy/identity.
Include the Odin tracking-allocator lesson, but reject Odin's implicit
`context` as hidden authority: Concrete allocator identity is an explicit
value or report fact, and allocation permission remains visible as
`with(Alloc)`.
### Task R-0316

**Phase:** 2, 4

**Reconciliation (2026-10-19):** Keep; settle with R-0496/R-0502 before interface freeze.

**Objective:** Add an unsafe-aliasing policy that avoids Rust's hidden reference-UB trap:

safe references keep Concrete's ordinary borrow/linearity rules; raw pointers
do not silently become safe references. Any operation that temporarily
upgrades raw memory to a safe borrow must state its exclusivity, lifetime, and
frame assumptions in the wrapper contract/report. No "trust me, this raw
pointer is an ordinary `&mut` now" shortcut.
### Task R-0317

**Phase:** 3, 4

**Objective:** Add allocator-strategy examples: global allocator, arena/bump allocator, fixed-buffer allocator, debug allocator, and test allocator. Every example must show allocation authority, allocator identity, cleanup path, and failure behavior. Do not add ambient allocation.

### Task R-0319

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep; bind results to R-0498’s enforcement manifest.

**Objective:** Add FFI/trusted-boundary red-team tests: wrong length, stale pointer, null pointer, alignment mismatch, double free, missing capability, hidden allocation, and wrapper that forgets to report the underlying trusted call.

Safe code must fail to reach the operation; unsafe code must appear in
`concrete audit --trust-boundaries`.
## Area: Performance And Optimization Evidence

Goal: make generated-code speed, size, memory, and optimization claims follow
the same evidence discipline as correctness claims. This phase owns measured
program performance; compiler throughput/caching remains primarily the incremental-driver area,
semantic equivalence remains the backend/differential gates, and release budgets
consume rather than reinvent these facts.

Trigger: start after Task R-0416 has a stable comparable corpus. R-0417–R-0419
are required evidence infrastructure for the affirmed project claim; actual
optimization work remains conditional on a reproduced gap, regression, or
release-facing comparison. If no such gap exists, retain measured baselines and
do not build speculative optimizers.

Done when: every public performance claim names its workload/input, comparable
reference, backend/target/profile/toolchain, metric and uncertainty, raw data,
replay command, and evidence class; generated-code regressions are caught within
calibrated noise; optimization work is pulled by measured gaps; and the release-bar area's
release budgets consume the same ledger.

### Task R-0417

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; source-cost bounds remain distinct.

**Objective:** Define performance evidence classes and claim grammar.

Use `measured_replayable`, `measured_nonreplayable`, `estimated`,
`complexity_guarded`, and `not_claimed`; never reuse `proved`, `enforced`, or
`tested_by_oracle` as a speed claim. A measurement states machine/OS, CPU
governor and load controls, toolchains, target, backend, optimization/profile,
input, repetitions, estimator, dispersion, outlier policy, and comparison
fairness. Complexity theorems or O(n²) ratchets describe growth, not wall-clock
performance. Add `docs/PERFORMANCE_EVIDENCE.md` and machine-readable schema.

`complexity_guarded` may carry compositional symbolic bounds on steps,
allocated bytes, or stack bytes produced by R-0244/R-0248 and the allocation
budget work. Such a claim names its source-level cost model and inputs and
never implies cycles or elapsed time. Start with the std container bounds
already forced by bug 048 plus one workload budget; do not build a parallel
cost-semantics task: R-0445 establishes the early schema and source model,
R-0244/R-0248 add discharge and target-budget enforcement, and this task
generalizes their evidence grammar without changing their meaning.

If a timing interpretation of `complexity_guarded` is ever entertained, run
the correlation experiment first: cost-model step counts vs. measured time on
the R-0416–R-0419 corpus (base64_cli, png_chunks, wordfreq). Poor correlation
is expected on memory-bound workloads and invalidates nothing about the
complexity claims; only a strong result licenses even speaking about time.

### Task R-0418

**Phase:** 3

**Objective:** Turn the flagships area corpus into a reproducible benchmark harness and
committed baseline ledger.

Add `concrete bench --manifest ... --json` or an equally stable script surface;
pin inputs and reference implementations; verify output/status before timing;
retain raw samples; separate compilation, link, startup, steady-state runtime,
and end-to-end measurements; and record cold/warm/cache state. Baselines are
reviewed artifacts with provenance, not an uncommitted `tests/perf` file. A
baseline update requires an explanatory diff and replay transcript.

### Task R-0419

**Phase:** 3

**Objective:** Add statistically honest generated-code regression gates.

Calibrate per-host noise from repeated controls; use minimum effect sizes plus
confidence/dispersion rules rather than one fixed percentage; rerun suspected
regressions on an isolated runner; distinguish flaky infrastructure from a
confirmed regression; and retain both sample sets. Per-change CI runs a small
deterministic sentinel set, scheduled CI runs the full corpus, and release CI
replays the declared budgets. Inject CPU work, allocation, and code-size
mutations to prove the corresponding gates are load-bearing.

### Task R-0420

**Phase:** 3, 4

**Objective:** Measure codegen quality directly, not only elapsed time.

For selected hot paths record dynamic/static instruction counts where stable,
branches, calls, stack usage, allocation count/bytes, bounds/runtime-check
sites, helper calls, vectorization/inlining facts where observable, emitted IR
and object/binary size. Add structural budgets for accidental allocation,
duplicate checks, missed constant folding, helper-call explosions, and loop-body
growth. These are target/backend-specific measured or reported facts, never
source-semantics proof.

### Task R-0421

**Phase:** 2, 3, 4

**Reconciliation (2026-10-19):** Keep; absorb R-0280 and require S12 certificates where proved.

**Objective:** Define the optimization ownership and evidence-preservation
contract across Concrete SSA, backend IR, LLVM, and QBE.

Every optimization has a named owner, preconditions, affected operations and
failure semantics, debug/source-identity policy, verifier before/after,
interpreter/differential/mutation coverage, and performance witness. Prefer
target-neutral canonicalization/legalization only when semantics require it;
keep target instruction selection backend-specific. Do not add a pass because
another compiler has it. Optimization levels select documented pass sets and
must not change observable checked arithmetic, traps, capabilities, ownership,
or evidence status.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0280** (merged): Duplicate of R-0421: merge into R-0421; connect certificate obligations to R-0504.

### Task R-0422

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; do not equate throughput with program speed.

**Objective:** Close measured compiler and emitter performance defects under one
owned queue, beginning with bug 027's quadratic emission class.

Profile before refactoring; retain adversarial scaling inputs; measure time and
peak memory across increasing program sizes; require an explicit expected
complexity envelope; and gate the class rather than one file. Typed emitter
fragments may be adopted when the profile pulls them, but QBE's fragment model
does not by itself prove LLVM emission fixed. Keep compiler-throughput facts
separate from generated-program runtime facts in reports and release claims.

### Task R-0423

**Phase:** 3

**Reconciliation (2026-10-19):** Keep under R-0515/R-0355 checkpoint.

**Objective:** Publish fair comparative performance reports without turning
benchmarks into marketing badges.

Compare identical algorithms, validation, integer semantics, allocation/I/O
boundaries, optimization intent, and outputs; disclose handwritten/reference
advantages, excluded workloads, confidence intervals, and backend/toolchain
trust. Report wins, losses, and inconclusive results. Ban geometric-mean or
headline claims that cannot be reconstructed from published rows, and ban
cross-language comparisons that silently omit safety/runtime checks present in
only one implementation.

### Task R-0424

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; compares the LLVM and LCB-native (`verified`) profiles; no QBE rows.

**Objective:** Add the performance area validation artifact and graduate its facts
into release budgets.

Ship a pinned mixed corpus containing compute, allocation, parsing, collection,
I/O, and binary-size workloads; replay LLVM and LCB-native (`verified` profile) rows on named
hosts; demonstrate one caught runtime, allocation, code-size, and scaling
regression; emit the performance evidence ledger and raw samples; and feed the
same schema into Task R-0355 rather than creating a release-only benchmark
database. The capstone states explicitly which performance claims are measured,
estimated, complexity-only, or not made.

## Area: Public Release Bar

Goal: make Concrete understandable and usable by someone who did not build the
compiler.

Done when: a fresh user can install Concrete, run a proof-bearing example,
inspect its audit bundle, independently verify its evidence root offline, and
understand the claim matrix and remaining trust boundary in under ten minutes.

**Language graduation is a gate, not a date.** Concrete can graduate individual
examples before the language graduates. The language reaches **alpha** only
after the source-contract/proof-link path is usable outside flagship hero work;
it reaches **beta/release** only after ordinary project workflow and external
validation are in place.

The [release map](#release-map) separates the systems preview, verified-component
alpha, project beta and stable supported subset. The preview makes no alpha
claim; the alpha and beta bars below remain required for their respective scopes.
Stable release additionally requires the named compatibility and maintenance bars.

**Alpha bar (verified-component alpha):**
- At least one non-author writes, proves, or contract-annotates a useful
  Concrete program and reports that ProofKit + contracts + `concrete prove`
  were worth the discipline.
- Diagnostics teach the discipline: on the linearity, capability, and
  must-use surfaces, errors name the remedy (consume / drop / return /
  declare the capability), not only the rule. A first-user session stalling
  on an undiagnosed E-code is filed as a compiler bug, not a user error —
  this is measured through the external-validation transcript, not
  self-assessed.
- Source contracts are primary for at least one flagship, with source-linked
  proofs and no JSON dependency for that flagship's main proof surface.
- `concrete prove` guides a user through contract -> obligation -> Lean/omega/
  `bv_decide` evidence -> source link -> audit.
- Small evidence examples exist for every public claim class:
  `proved_by_lean`, `proved_by_kernel_decision`, `tested_by_oracle`,
  `assumed`, `trusted`, `partial`, and `stale`.
- Core language surface blockers are settled or explicitly deferred with
  examples: pattern cleanup, bytes/text/path, iteration, collections,
  ignored-result diagnostics, and capability polymorphism.
- Runtime safety obligations have defined evidence classes for bounds,
  div-zero, overflow, casts, panic/abort, and unchecked conversion behavior.
- README, website, papers, examples, and roadmap agree and do not outclaim
  audit evidence.

**Beta/release bar (language can be released to users beyond the thesis
audience):**
- `concrete fmt`, `concrete test`, and a minimal project model (`Concrete.toml`)
  exist.
- The **minimal local package/evidence foundation** has exercised the public artifact boundary
  before it freezes: manifest, lockfile, workspace, interface/body split, local dependency
  certificate verification, trust/assumption inheritance and evidence-schema compatibility. A
  public registry, hosted publishing and remote cache are not beta prerequisites.
- A **minimal editor path** exists for incremental structured diagnostics, capability/proof-status
  hover, obligation navigation and replay commands. It consumes the compiler session/query facts;
  advanced refactoring, dependency dashboards, migration UI and playground work remain later.
- In-source proof links are the only proof path; `proof-registry.json` support
  must remain absent.
- Release bundles have stable schemas, replay commands, assumption/trust
  reports, and proof-link provenance.
- An installed independent `concrete-cert verify-bundle` path can validate a
  release evidence root offline from untrusted bundle bytes, report the precise
  Core/backend predicates it checked, replay kernel evidence where claimed, and
  keep every unsupported/native boundary visibly trusted.
- A verified-profile policy gate exists and is used by every release-facing
  proof/audit claim; public examples cannot bypass it with stale proofs,
  unapproved assumptions, or hidden trusted/Unsafe boundaries.
- The stdlib/runtime boundary is stable enough for daily examples.
- At least one external user completes the first-user tutorial and a useful
  audit/proof workflow without compiler-author intervention.

### Task R-0333

**Phase:** 1, 2, 3, 7

**Reconciliation (2026-10-19):** Keep; repair release-map inconsistencies.

**Objective:** Define first public release criteria: supported subset, required examples, required diagnostics, proof workflow, stdlib/project UX, evidence/policy/ tooling story.

Own the [release map](#release-map) and one bounded checklist/candidate record per
release scope. Assign existing task increments with explicit acceptance artifacts
and deferred remainders; do not create a second queue or require completion of all
phases. Record separate source/std/evidence compatibility and patch-release rules.
The systems preview does not satisfy or replace the verification alpha bar.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0110** (merged): merged — profile naming and evidence attribution join R-0075/R-0333; the profiles are `llvm` and the LCB-native `verified` profile. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0110)

### Task R-0334

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; share one tutorial with R-0346/R-0347.

**Objective:** Add the first-user teaching path as a release-bar artifact, not marketing copy. Deliverable: `docs/LEARN_CONCRETE.md` plus a checked tutorial transcript that covers install/build/run/test, `Copy` vs linear values, consume/destroy/handoff, capabilities in function headers, `Result` / ignored-result diagnostics, stdlib basics, one runtime trap, one audit report, and the five evidence classes. The tutorial must use runnable snippets covered by the doc-snippet gate and must not claim proof where the compiler reports only testing, runtime checking, assumption, or trust.

### Task R-0335

**Phase:** 1, 3, 7

**Reconciliation (2026-10-19):** Keep; Phase 1 settles rules, each release publishes its subset.

**Objective:** Publish a versioned authoritative language reference before any public release claim. The tutorial/book is not the reference. The current normative material is scattered across grammar, value-flow, execution, invariants, and evidence docs; release users need one versioned document whose sections define the language surface, not merely explain it.


 Deliverable: `docs/LANGUAGE_REFERENCE_V0.md` (or generated equivalent)
 covering syntax, lexical forms, type/value model, Copy vs linear values,
 explicit destroy/defer, capabilities, references and borrow blocks,
 statements/expressions, modules/imports/visibility, generics/monomorphization,
 trusted/unsafe/raw pointers, runtime checks/traps, main/exit semantics,
 stdlib boundary expectations, and evidence taxonomy. Every section links to
 its machine-checked gate, diagnostic, or "not yet supported" rejection.
 Gate: `scripts/tests/check_language_reference.sh` verifies every construct
 named in the grammar has a reference section, every anti-feature/rejected
 construct has a diagnostic or policy citation, and README/site/book links
 point to this reference as the normative source.
### Task R-0425

**Phase:** 1, 2, 3, 4

**Reconciliation (2026-10-19):** Keep; grow alongside normative semantics.

**Objective:** Publish a versioned language conformance suite derived from the
normative language reference, runnable by every backend and any future
independent implementation.

Do not make the current compiler's output the expected answer. For each
reference rule classify fixtures as required behavior, required rejection,
implementation-defined behavior with a declared choice, target/profile-
conditional behavior, or unsupported extension. Name the interpreter as the
executable reference only for the subset whose interpreter semantics are
specified and independently checked against normative expected observations;
interpreter/backend agreement alone remains corroboration, not definition.

Version the corpus with the language reference. Each case records source,
reference-section/rule ID, expected stdout/stderr/exit/trap or stable diagnostic
class, capabilities/profile/target, permitted variation, and minimum language
version. Run it through interpreter, LLVM, QBE, WebAssembly when supported, and
future backends without silent fallback. Backend-specific exclusions require a
declared capability-matrix row and cannot make the language case disappear.

Acceptance: coverage mapping from every normative construct and rejection rule
to at least one positive/negative case or explicit untestable rationale;
mutation tests for wrong value, missing trap, wrong rejection, backend drift,
and stale reference links; an external runner with JSON results; deterministic
fixtures independent of host locale/time/network unless those facts are the
declared subject; and a release gate that refuses a claimed-supported backend or
language version with an unexplained conformance failure.

### Task R-0336

**Phase:** 1, 3, 7

**Objective:** Publish a public claim matrix: what Concrete proves, enforces, reports, assumes, and trusts.

### Task R-0337

**Phase:** 1, 3, 7

**Objective:** Add release claim freeze: README, `CLAIMS_TODAY.md`, roadmap, showcase manifest, and release bundles must agree.

### Task R-0338

**Phase:** 2, 5, 7

**Reconciliation (2026-10-19):** Keep; minimal V1 versions precede final schema freeze.

**Objective:** Add compatibility policy for proof artifacts and fact schemas.

Own queue row 11’s receipt-v1 freeze decision: producer and independent consumer agree on canonical fixtures, the permanent schema attacks pass, and incompatible changes require a version bump.

### Task R-0339

**Phase:** 2, 5, 6

**Reconciliation (2026-10-19):** Keep; distinguish display stability from semantic identity.

**Objective:** Add compatibility policy for generated contract/VC obligation IDs:

obligation IDs should stay stable across harmless formatting and local
refactors, and any unavoidable churn should be reported as artifact churn,
not hidden under ordinary proof drift.
### Task R-0340

**Phase:** 3, 7

**Objective:** Add public API compatibility checking before release:

`concrete api-diff old/ new/ --json` compares exported modules, function
signatures, capabilities, allocation behavior, evidence classes, stdlib API,
package interface artifacts, and generated docs. This is the Swift
API-digester lesson adapted to Concrete: API drift is a reportable fact,
not a release-note afterthought.
### Task R-0341

**Phase:** 1, 5, 7

**Reconciliation (2026-10-19):** Keep; use typed policy rather than scalar evidence ranking.

**Objective:** Define release/showcase evidence policy by class:

`proved_by_lean` and `proved_by_kernel_decision` are strong evidence;
`tested_by_oracle` is supporting evidence; `proved_by_smt` /
`solver_trusted` require explicit policy approval; `open` and unreviewed
`assumed` are forbidden for release claims. The policy must be executable by
The audit-commands area verified-profile command, not just written in prose.
### Task R-0342

**Phase:** 3, 7

**Objective:** Add public examples policy: public-facing examples, website copy, README snippets,
paper examples and showcase manifests must not outclaim their proof status. The public flagship
portfolio is exactly HMAC-SHA256, `secure_update_verifier` and `protocol_state_machine`; active
candidates can be shown as active work, but cannot be presented as proved or graduated until their
bars land. Other fixtures/workloads may be documented without being marketed as additional
flagships.

### Task R-0343

**Phase:** 1, 2, 5, 7

**Reconciliation (2026-10-19):** Keep; current validity and historical replay must remain separate. Revocation rejects current policy acceptance while preserving accurately labelled historical replay (R-0430).

**Objective:** Add public security/soundness disclosure policy: compiler/proof pipeline bugs are security-relevant.

- 9a. Publish `SECURITY.md`: how to report a compiler/proof-system bug
  privately, response-time commitment, advisory format, and severity
  classes — a soundness bug that lets a false claim go green
  (checker hole, extraction bug, stale-evidence upgrade) is treated as
  highest severity even when nothing crashes. The policy must name scope:
  compiler crashes on valid input, silent miscompiles, verifier/proof
  unsoundness, stale-proof upgrades, hidden capability/allocation/trusted
  boundaries, package-evidence forgery, and stdlib/runtime safety holes are
  all security-relevant for Concrete. Advisories enumerate affected claims by
  reading the obligation ledger of affected releases, not by prose
  recollection. Gate: `scripts/tests/check_security_policy.sh` verifies
  `SECURITY.md` exists, is linked from README/site/release bundle docs, and
  names the supported versions/reporting channel before any public release
  target can pass.
- 9b. Define the proof-revocation procedure BEFORE the first release that
  carries proof claims: when a shipped theorem/evidence class is
  invalidated (proof bug, axiom-inventory violation, solver unsoundness,
  spec error), there must be a mechanical path that (1) republishes the
  release manifest with the affected facts downgraded to their honest
  class (`open` / `needs_recheck` / `trusted`), (2) emits an advisory
  listing every downgraded claim id, and (3) makes the old bundle
  fail verification loudly rather than silently coexist. Red-team gate:
  `scripts/tests/check_proof_revocation.sh` simulates revoking one
  flagship theorem (e.g. remove it from the axiom-inventory allowlist or
  break its fingerprint) and must show the release bundle flips to
  failing with the downgraded claim named — no false green on a revoked
  proof. Task R-0353's independent verifier must reject the old evidence
  root and any cached receipt that still names the revoked theorem.
- 9c. Treat upstream checker/kernel soundness advisories as Concrete security inputs. Attached
  `#[proof_by]` / `#[ensures_proof]` modules, generated proof objects and serialized checker input
  are adversarial even when they use an ordinary checked API and report no axioms. Axiom inventory
  does not detect an axiom-free kernel exploit. For each advisory, retain the exact reproducer,
  enumerate affected receipts/claims, exercise R-0208's version downgrade and R-0353's independent
  replay, and publish which evidence dimensions remain intact. The threat model must say plainly
  that Concrete cannot detect a kernel implementation bug from inside that same kernel; it can
  contain the consequence through version binding, revocation, replay and genuinely different
  checker families.
### Task R-0345

**Phase:** 1, 3, 7

**Reconciliation (2026-10-19):** Keep before relevant public claims.

**Objective:** Publish `THREAT_MODEL.md` and keep it linked from README, release bundles, showcase manifests, and assumptions docs.

### Task R-0346

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; reuse R-0285 consumer.

**Objective:** Add first-user workflow CI: install compiler, create/run one example, inspect one audit bundle without repo-local assumptions.

### Task R-0347

**Phase:** 3

**Reconciliation (2026-10-19):** Keep as tutorial content, not another onboarding project.

**Objective:** Improve onboarding, tutorial, and docs around `proved` / `enforced` / `reported` / `assumed` / `trusted`.

### Task R-0348

**Phase:** 3

**Disposition (2026-10-19):** merged — the formatting decision is settled; residual documentation cleanup moves to R-0349.

**Objective:** Resolve string interpolation / formatting as an explicit release-facing decision, not an orphaned gap. V1 rejects string interpolation syntax, format macros, and trait-object-style `Display` because they hide allocation, dispatch, and formatting authority. The supported path is `std.fmt` over `std.io.Writer`, with any string-returning helper carrying `with(Alloc)` and any future convenience helper pulled by workloads.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0348).

### Task R-0349

**Phase:** 1, 3

**Objective:** Refresh or archive stale gap/backlog documents before release. Documents such as `docs/language/LANGUAGE_GAPS.md`, root-level idea piles, and old stdlib reviews may be useful historically, but they cannot sit next to the roadmap making claims that contradict closed known holes or current semantics. Gate: `scripts/tests/check_gap_docs_current.sh` rejects stale "true blocker" / "largest gap" claims unless they map to an open roadmap item or known-hole id; otherwise the material moves to CHANGELOG/archive with a date and status.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0348** (merged): merged — the formatting decision is settled; residual documentation cleanup moves to R-0349. — [archived body](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0348)

### Task R-0351

**Phase:** 3, 7

**Objective:** Add migration/adoption playbook: what C/Rust/Zig code moves first, how to wrap libraries honestly, what stays outside Concrete.

### Task R-0352

**Phase:** 3

**Objective:** Add release/install distribution matrix: host triples, checksums/signing, install paths, supported/deferred channels.

### Task R-0353

**Phase:** 2, 5

**Reconciliation (2026-10-19):** Keep; fix freeze ordering and profile-specific native edges.

**Linked 2026-10-18:** this task is part of stage S16 of the [verified-compiler pipeline](docs/compiler/VERIFIED_PIPELINE.md): it consumes R-0494's `ClaimJudgment` encoding and R-0495's generated statements without trusting stored verdicts, does not by itself establish source-to-model or native-code correspondence, and pins every 079 – 095 fixture's expected result vector. Codec and hostile-input prototypes may proceed beside queue rows 3 – 8. R-0353's complete cross-profile consumer remains at row 10, after row 9 and R-0004's prerequisites; a minimal independently checked retained-artifact consumer is required for every publicly authoritative V1 claim.

**Objective:** Define the canonical release-evidence DAG and offline independent bundle verifier before packaging or benchmarking it.

**Strategic role and dependency.** This is the standalone consumer for R-0004's stabilized
subjects, exact dependency roots and replay receipts. It is a separate product boundary, not an
extra R-0004 slice: R-0004 makes evidence production honest; R-0353 lets a stranger check the
result without trusting the Concrete compiler or its report pipeline. Codec, hostile-decoder and
root-recomputation prototypes may start earlier in shadow. R-0353's complete consumer becomes
authoritative only after R-0004's replay, migration and adversarial boundaries close. The minimal V1
consumer has its own explicitly qualified subset and cannot claim authority over unsupported legacy or
cross-profile evidence.

**Trust promise.** Given hostile bundle bytes, an externally supplied expected root or an accepted
signing key, and a verification policy, independently decide whether the bundle is internally
complete, dependency-closed, replay-bound, authenticated and policy-compliant. Do not compile
Concrete source, regenerate proofs or trust stored `proved` strings. The compiler proposes the
evidence graph; the verifier recomputes whether it is closed and properly bound.

**Small-verifier architecture.** Keep the deterministic core dependency-light and auditable:

- `concrete-cert-core`: strict versioned decoding, canonical encoding, domain-separated digest
  recomputation, identity uniqueness, graph closure/root recomputation, receipt validation,
  trust/assumption propagation and policy evaluation. No filesystem, network, subprocesses,
  compiler imports or ambient environment reads;
- `concrete-cert`: hostile file loading, CLI, expected-root/key inputs, resource limits, structured
  and human output, and kernel-adapter orchestration;
- kernel adapters: separately versioned Lean, Rocq, Isabelle and certificate-checker replay paths.
  Adapter success is evidence input, never authority to bypass core graph/policy checks.

The governing rule is **check retained producer outputs; never rerun a producer derivation and call
that independent verification**. Re-elaboration, VC generation, extraction, SAT solving or tactic
search may be useful diagnostics, but they do not substitute for checking the exact artifact that
minted the receipt. The verifier has three explicit layers:

1. **Envelope/accounting:** authenticate the expected root/signature; strictly decode canonical
   bytes; recompute identities, dependency closure and roots; validate receipts; propagate trust,
   assumptions and advisory state; apply policy. This proves internal binding and completeness only
   relative to the declared graph. It cannot discover an arbitrary omitted semantic dependency
   unless the bundle also carries an independently checkable completeness relation or an externally
   pinned expected root.
2. **Certificate profiles:** check the exact retained certificate against the exact retained
   canonical proposition. Every profile defines its supported fragment, proposition encoding,
   certificate encoding, checker implementation/version/rule set, checking relation, resource
   limits, deterministic failure taxonomy and evidence class. A checker must not invoke the solver
   or tactic that generated its certificate.
3. **Translation assurance:** validate and report source/Core-to-VIR correspondence evidence without
   claiming that the small verifier generally proves this semantic arrow. It may check future narrow
   translation certificates or finite-fragment relations, but otherwise reports the producer's
   named assurance (`producer_validated`, `sampled`, `exhaustive`, `formally_justified`, or
   `missing`). Layer 1 truth about a VIR proposition does not imply that the proposition faithfully
   describes the source program.

**Initial certificate profiles and honest probes:**

- Linear arithmetic follows R-0463: first determine whether stable micromega/Farkas or
  Positivstellensatz witnesses can be exported with enough semantic detail for an independent
  checker. Measure eligible obligations and distinguish integer reasoning from rational/polynomial
  witness semantics before estimating coverage or checker size. Failure to export a stable witness
  is the probe's named kill condition, not a reason to reconstruct the proof by rerunning tactics.
- Boolean/bit-vector evidence follows R-0451, but the authoritative chain is exact
  `VIR proposition -> CNF bytes/digest -> original retained DRAT/LRAT certificate -> checker result`.
  Re-solving captured CNF to manufacture a second certificate is corroboration, not replay of the
  certificate that supported the original claim.
- Nonlinear evidence remains `solver_trusted` while H21/R-0451 lacks a replayable proof object. The
  verifier names that boundary rather than converting solver success into certificate evidence.

Certificate replay does not automatically make a second kernel/checker worthless. Whether one
small checked certificate is sufficient is an explicit policy decision informed by checker
maturity, independent reproduction and adversarial results. Record independence along separate
axes: implementation/code lineage, algorithm and proof-system family, shared exporter/serializer,
shared libraries and shared specification. Different executable names are not independent evidence.

**Build sequence:** finish R-0004's exact correspondence and replay-receipt schema; freeze canonical
receipt/bundle bytes; ship Layer 0 accounting with hostile fixtures; run the R-0463 extraction probe;
bind exact CNF and retained certificates before the DRAT/LRAT checker; add kernel proof-object replay
profiles; mint R-0004 slice-7 receipts only for defined profiles; shadow-compare the verifier with
Concrete; complete R-0004/R-0353 adversarial exercises; then make the external verifier
authoritative behind expected-root/signature authentication and revocation policy.

From the first checker commit, each profile needs a valid positive certificate, malformed and
truncated inputs, a corrupted certificate, a valid certificate paired with the wrong goal, empty
material, duplicate/surplus material, resource-exhaustion limits, deterministic replay, and a
production-consumer mutation proving that bypassing the checker cannot retain a friendly verdict.

The hashed representation is a canonical binary format (deterministic CBOR or a deliberately
smaller specified codec); JSON is display/debug output only. Unknown critical fields and unknown
schema/rule-set versions refuse. Canonical decoding must reject duplicate keys, alternate integer
encodings, ambiguous ordering, trailing data and resource-exhaustion shapes.

**Lean kernel incident requirement (2026-07, issue #14576).** Treat proof kernels as versioned,
revocable dependencies rather than timeless oracles. The incident allowed the ordinary checked
Lean `addDecl` path to admit a malformed nested-inductive declaration and derive axiom-free
`False`; `--trust=0` and axiom printing did not detect it. The original artifact also passed an
outdated independent checker because that checker had a separate projection bug. Therefore:

- every replay receipt binds exact kernel implementation, commit/release, rule-set digest,
  exporter/serializer version and proof-artifact digest;
- policy maintains explicit accepted, vulnerable and revoked checker-version sets; a receipt from
  a newly revoked kernel becomes `needs_recheck` or rejected, never remains `proved` because it was
  green when minted;
- advisory status remains external validation-time input. A replay receipt binds immutable replay
  facts; a retained policy-decision attestation binds the advisory-registry and policy identities
  it evaluated. Neither artifact pretends a future advisory was known at minting;
- high-assurance policy requires current independent replay through structurally different
  implementations, not merely two invocations or two wrappers around shared kernel code;
- checker independence is recorded by implementation lineage and shared code/theory, not inferred
  from different executable names. A port of the affected kernel logic does not count as a fully
  independent defense against that defect class;
- the #14576 exploit, the independent-checker projection exploit, and every future upstream kernel
  soundness reproducer enter a permanent checker-arena corpus. Each accepted checker version must
  reject every applicable historical exploit before it can satisfy policy;
- successful replay is re-executed from the exact retained proof artifact in an isolated process.
  A receipt that merely states replay succeeded is insufficient;
- revocation enumerates every affected Concrete claim through receipt indexes and invalidates
  cached roots/receipts mechanically. Public advisories must name the downgraded claims.

Fragment-scoped revocation is an optimization, not the default. An affected checker version is
downgraded wholesale unless an independently checked feature footprint proves the vulnerable
mechanism absent from the retained environment. Surface VIR lacking the construct is insufficient:
an attached metaprogram may introduce a hostile declaration unrelated to the visible proposition,
and a footprint self-reported only by the affected checker cannot clear itself.

Do not respond by banning Lean metaprogramming. The elaborator is untrusted by design, and hostile
proof terms or serialized artifacts can bypass it. Defense belongs at the kernel/checker boundary,
in independent replay, version binding, exploit regression, revocation and multidimensional
policy.


 Move the common public/verifier codecs here: versioned canonical encoders
 for source/lock inputs, typed-Core artifacts and the soundness-bridge area receipts,
 obligations/proof bundles, SSA/BackendIR artifacts and the backend-contracts area receipts,
 codegen-unit/object/link manifests, binary identity, policy/profile,
 assumptions/trust facts, checker/toolchain identities, and proof replay
 payloads. Task R-0381 later wraps these in package interface/body formats;
 it does not first define encoders required by this release gate.

 Every edge declares its relation and status (`identity_bound_only`,
 `producer_validated`, or a named independently checked relation), and the
 graph schema names mandatory edges. The bundle exposes one computed evidence
 root, but self-consistency is not authenticity: verification requires an
 externally supplied pinned expected root or a policy-accepted signature/
 builder attestation over that root. A root/signature authenticates identity
 and provenance, not semantic correctness.

 Define `concrete-cert verify-bundle <bundle> --expected-root <root>` plus a
 `concrete verify-bundle` alias only if it invokes the same checker. It runs
 without repository/network state, treats bundle bytes as hostile, recomputes
 canonical nodes/edges/root, reruns the soundness-bridge and backend-contracts areas certificate predicates, and
 validates checker/rule-set/toolchain/target/profile/policy binding. Output
 separate dimensions: `internally_consistent`, `root_matches_expected`,
 `provenance_authenticated`, `core_certificate`, `backend_translation`,
 `proof_replay`, `policy`, `assumptions`, and `remaining_trust`—never one
 ambiguous `verified` badge.

 The minimum result vector includes `internally_consistent`, `root_matches_expected`,
 `signature_accepted`, `provenance_authenticated`, `dependencies_complete`, `kernel_replay_valid`,
 `checker_set_current`, `policy_satisfied`, `assumptions`, `trusted_boundaries` and
 `remaining_trust`. A bundle can be internally consistent yet describe the wrong project or rely
 on a revoked checker; those states must remain independently visible.

 The schema can reject a missing mandatory edge; the Merkle graph cannot
 discover an arbitrary omitted semantic dependency unless completeness is
 separately proved. Proof/Core and SSA/BackendIR substitutions may fail their
 independently checked relations. Object/binary substitutions fail only the
 expected-root/provenance binding; `BackendIR -> object -> binary` remains
 backend-trusted. A fresh internally consistent root over different native
 bytes is not a semantic mismatch.

 Specify a self-contained offline proof-replay payload and runtime: theorem
 sources or checked proof objects, generated workspace/manifest, exact Lean/
 ProofKit dependencies, kernel/tool versions and hashes, and licenses. If a
 platform bundle cannot provide that replay surface, report
 `proof_replay=unavailable`; it may not satisfy the beta/release bar for a
 claim advertised as offline kernel-replayable.

 Gate expected-root mismatch, accepted/bad signature, proof from build A with
 Core from B, SSA/BackendIR substitution, object/binary substitution,
 schema-required edge deletion, declared dependency-root mismatch, revoked
 proof, unsupported relation mislabeled checked, malformed cycles/duplicate
 ids, and missing checker/theorem identity. A byte mutation must produce a
 different root and fail against the pinned expected root (or lose accepted
 authentication), not necessarily fail internal-consistency parsing. Wire
 `scripts/tests/check_verify_bundle.sh`.

 Also gate omitted/duplicated/surplus dependencies; swapped subject/receipt/table material; stale
 theorem, implementation and checker identities; trust/assumption deletion; replay success without
 retained replay material; revoked checker versions; one current plus one vulnerable checker;
 purportedly independent adapters sharing the same implementation lineage; the Lean #14576
 reproducer; the historical independent-checker projection reproducer; canonical-encoding
 ambiguity; unknown critical fields; cyclic, oversized and deeply nested hostile bundles; and
 deterministic verification across clean machines and checkout paths.

**Authority progression:** `designed -> prototype -> negatively controlled -> mutation-killed ->
shadow-compared with Concrete -> independently reproduced -> authoritative consumer ->
adversarially closed`. No stage may be skipped, and internal consistency alone never satisfies the
release policy.
### Task R-0354

**Phase:** 3, 5

**Reconciliation (2026-10-19):** Keep; scope preview separately from replay-bearing releases.

**Objective:** Add concrete distribution UX:

 `concrete --version --json`, `scripts/release/build_dist.sh`,
 `dist/concrete-<version>-<target>.tar.gz`, `dist/SHA256SUMS`,
 `dist/SIGNATURES` if signing is enabled, `INSTALL.md`,
 `RELEASE_NOTES.md`, and uninstall/upgrade notes. Nix/Homebrew or similar
 channels may be added only if they can be kept reproducible and
 version-pinned. The archive must ship the independently built
 `concrete-cert` verifier, expose `concrete-cert --version --json`, record its
 source/binary/rule-set hashes separately from the producer compiler, and let
 `verify-bundle` run without repository state or network access. It must also
 ship Task R-0353's pinned proof-replay runtime/payload dependencies—or report
 `proof_replay=unavailable` and fail any release profile that requires offline
 kernel replay. Include every shipped Lean/ProofKit/runtime component in
 checksums, the supply-chain lock, and `THIRD_PARTY.md`.
### Task R-0355

**Phase:** 2, 3

**Reconciliation (2026-10-19):** Keep; consume performance ledger and add proof scalability metrics. Before large R-0512 migrations, budgets include proof scalability: elaboration/recheck memory, proof-term size, artifact size and validator scaling.

**Objective:** Add release performance budgets:

 `scripts/tests/check_release_performance.sh` must measure compiler startup,
 cold small-project/stdlib builds, the incremental-driver area warm no-op, private-leaf and
 public-interface edits, proof-query/object-cache hits, relink latency,
 `concrete test`, audit/report generation, proof-check latency, and
 independent bundle-verification latency against
 `release/perf-baseline.json`. Done when
 release CI blocks unexplained regressions and prints the regressed command.

**Development-speed baseline (2026-10-21; checkpoint of queue row 4, precedes its structural
changes).** Before the first whole-language change of R-0492, R-0499/R-0514, R-0502 or R-0501 lands on
main, record a baseline over a small named representative corpus (the compiler, `std`, one large
example and one multi-module program, listed in `release/dev-speed-corpus.txt` (new)): clean build
time of the compiler and of each corpus program, rebuild time after a one-function edit in the
compiler and in a Concrete program, peak compiler memory on each corpus program, and the fast push
tier's turnaround (R-0513). Each measurement is recorded per main commit with its SHA, host class and
toolchain, and has a budget with a stated noise tolerance; `scripts/tests/check_dev_speed.sh` (new)
fails a commit that exceeds a budget beyond its tolerance, and a budget is raised only by a commit that
states the reason. **Checkpoint exit:** the baseline exists at a named main commit before that first
structural change lands; a deliberately injected regression beyond tolerance fails the gate; the
recorded series shows each structural change's effect. The release budgets above remain this task's
exit; this checkpoint is their first, smaller corpus.

### Task R-0356

**Phase:** 2, 3, 5

**Reconciliation (2026-10-19):** Keep; preserve historical replay instead of blanket old-format rejection. Current issuance may reject old formats; retained historical verification stays supported through its versioned path (R-0512).

**Objective:** Add reproducible release artifact hashes:

 `concrete release --manifest --json` must record source tree hash,
 compiler commit/version, schema versions, target triple, build profile,
 dependency lock hash, stdlib hash, canonical query/artifact dependency root,
 the soundness-bridge and backend-contracts areas checker and rule-set identities, typed-Core/BackendIR/codegen-unit
 roots, emitted object/binary identity, release-bundle evidence root, and
 replay command. V1 does not support old release manifests; schema
 mismatches fail loudly with a regeneration command. Wire
 `scripts/tests/check_reproducible_release_hash.sh`; the gate must build the
 same release artifact twice from a clean tree and compare all hashes.
### Task R-0357

**Phase:** 2, 3

**Objective:** Add a toolchain/dependency supply-chain lock and license inventory before any release claim:

 - The Lean toolchain (`lean-toolchain`), every Lake dependency
   (`lake-manifest.json` pinned revisions), the SMT solver binary
   (name + version + hash, per the SMT replay metadata), and clang/LLVM
   versions are part of the evidence chain — a claim is only as
   trustworthy as the unpinned link. Task R-0356's release manifest must
   record all of them; drift between the lock and the build environment
   fails the release gate, mirroring the Task R-0216
   `proofs/lean-deps.lock` recheck trigger.
 - Publish `docs/THIRD_PARTY.md`: every shipped dependency (Lean
   toolchain, Lake packages, solver, runtime libs linked into emitted
   binaries) with license and role; release claims are blocked while any
   shipped dependency's license is unreviewed or incompatible.
 - Red-team gate: `scripts/tests/check_supply_chain_lock.sh` must fail
   when (a) `lake-manifest.json` and the recorded release manifest
   disagree on any pinned revision, (b) a dependency is present in the
   build but absent from `THIRD_PARTY.md`, or (c) the solver version
   recorded in replay metadata differs from the one in the lock.
### Task R-0358

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep before first public release.

**Objective:** Add the language/stdlib/artifact deprecation policy (moved up from the packages area — users of a released language need the policy BEFORE the release, not after): what stability the first release promises for syntax, stdlib API, proof/fact schemas, and obligation ids; how deprecations are announced (diagnostic with a replacement, minimum deprecation window measured in releases); and what may never break silently (anything that would flip an evidence class without `needs_recheck`). The editor area migration tooling (`concrete migrate`) implements this policy; the policy itself is a release-bar document. Gate: Task R-0340's `concrete api-diff` must classify every

 public-surface change as `compatible`, `deprecated(window)`, or
 `breaking`, and the release gate fails on `breaking` without a recorded
 policy exception.
### Task R-0359

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep; bound checklist to implemented gates. Gates every implemented or activated requirement, with explicit dispositions for future tasks.

**Objective:** Add the external-contributor and public-platform surface before release.

 Write `CONTRIBUTING.md` as an operational checklist, not generic etiquette:
 how to add a language feature, which compiler modules usually change, which
 gates to run, which docs must be updated, how to add positive/negative/
 adversarial fixtures, and how to update
 `scripts/tests/example_manifest.txt` in the same commit as any new
 `examples/` `.con` file (this broke CI twice on 2026-06-09/10). Add
 `scripts/tests/check_contributing_contract.sh`; the gate must prove every
 example project has a manifest entry, every roadmap-required gate is named
 in either `Makefile` or CI, and contributor docs mention source changes,
 tests, docs, roadmap/changelog, claims, and release-bundle impacts. Publish
 a public platform-support statement (`docs/PLATFORM_SUPPORT.md`) naming
 Tier 1/Tier 2/unsupported hosts and targets, including Linux/macOS
 x86_64/aarch64 and Windows status. Windows must not remain ambient: either
 it is a named supported target with CI and hosted-capability semantics, or
 it is explicitly "not v1" with a pull trigger (external adopter, workload,
 or release requirement) and a list of blocked surfaces: paths, process,
 env, console, file locking, line endings, and toolchain/LLVM availability.
 Add performance-claim discipline: public copy may not imply "fast systems
 language" unless `concrete bench` and release performance gates publish
 replayable numbers; otherwise docs must say performance claims are not made
 yet.
**Governance (added 2026-10-19 via R-0515):** publish a lightweight governance document naming
language-design and release decision owners, the proposal/comment/decision process, how disagreements
and conflicts of interest are handled, and how maintainer authority is delegated or transferred. Retain
a public decision record for the first externally proposed change; record rejected proposals and their
rationale as well as accepted ones.

### Task R-0360

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep; replace “all above” with scoped R-0333 candidate criteria. Ships against R-0333's bounded candidate checklist, not "all of the above".

**Objective:** Ship the first narrow public release only after the above are green.

### Task R-0361

**Phase:** 3, 5

**Reconciliation (2026-10-19):** Keep; historical origin does not make remainder complete.

**Objective:** [relocated from closed phase 4] Artifact and docs stability hardening:

 schema-version rejection gates (refuse to silently misread an artifact whose
 schema version differs from the compiler's), source-location privacy /
 redaction modes for emitted diagnostics and artifacts (historical phase 4 item 38), and
 docs-drift SEMANTIC checks beyond the artifact-existence gate
 (`check_docs_drift.sh`) — e.g. `Status:`/`Verified:` metadata and stale-claim
 marker detection. (Found not mechanically robust as a default during historical item 44
 work; they belong here, gated for the release bar.)
### Task R-0362

**Phase:** 3, 5, 7

**Reconciliation (2026-10-19):** Keep; branch native versus LLVM assurance.

**Objective:** Add the release-bar area validation artifact:

 `scripts/tests/check_release_candidate.sh` installs the dist archive into a
 clean temp prefix on every supported host, runs `concrete --version --json`,
 builds one example, runs one proof/audit workflow, verifies checksums and
 signatures if enabled, checks `release/perf-baseline.json`, runs
 `concrete api-diff` against the previous public interface snapshot, and
 confirms the bundle contains the claim matrix, threat model, public
 examples policy, replay commands, schemas, assumptions/trust reports,
 Task R-0343's `SECURITY.md`, the supply-chain lock and `THIRD_PARTY.md` from
 Task R-0357, the deprecation policy and clean `api-diff` classification from
 Task R-0358, and
 the tutorial transcript from someone who did not build the compiler. It
 must also run Task R-0343's proof-revocation drill against the candidate
 bundle, verify the evidence DAG offline through Task R-0353 from the installed
 independent checker, and run tamper cases for source/Core/proof/BackendIR/
 object/binary/dependency-edge substitution. The report must distinguish
 Core/proof or SSA/BackendIR relation failures from object/binary failures
 against the expected root/authentication; native semantic edges remain
 explicitly backend-trusted. Release replay begins with a
 clean cache or `--incremental=off`; a local cache cannot be an undeclared
 release input.

## Area: Post-Release Operations And Stewardship

Goal: keep Concrete's claims, users, artifacts, and dependency evidence
trustworthy after the first release. This phase starts only after Task R-0360
ships a public release; it is operational stewardship, not a pre-release
community-building program.

Done when: supported versions and response windows are public; releases can be
reproduced, rolled back, revoked, and repaired; security and evidence incidents
have rehearsed procedures; current/previous-version compatibility is gated;
user failures become permanent corpus evidence; and ecosystem-health claims use
opt-in or public data rather than hidden telemetry.

### Task R-0426

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep readiness before 0.1; operate from 0.1.

**Objective:** Define the supported-version, maintenance, and end-of-life
policy.

Name stable/preview channels, support windows, which release lines receive
security versus correctness fixes, toolchain/platform support, evidence/schema
compatibility, and the notice required before end of life. Publish a
machine-readable support matrix consumed by installers, `doctor`, release
bundles, package tooling, and documentation. Unsupported must mean no claim, not
silent best effort.

### Task R-0427

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep; rehearse before first supported release.

**Objective:** Make release rebuild, hotfix, rollback, and revocation procedures
replayable.

From the source tag, locks, builder/toolchain identities, and evidence root,
reproduce every supported artifact and compare declared hashes. Define signed
hotfix provenance, rollback selection, mirror/CDN correction, compromised-key
rotation, and how installers refuse a revoked release. Drill a bad binary, bad
metadata, failed rollout, and lost build host; retain timings and manual steps.

### Task R-0428

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep; share policy with R-0343.

**Objective:** Operate a security and soundness response process.

Publish reporting channels, acknowledgement/triage/remediation targets,
embargo and coordinated-disclosure rules, severity criteria for compiler wrong
code, proof/evidence overclaim, stdlib memory safety, package compromise, and
toolchain/supply-chain defects, plus advisory/CVE policy where applicable. A
fix includes containment, minimized reproducer, affected-version matrix,
revocation/recheck consequences, class-level gate, disclosure, and backport or
explicit non-backport decision.

### Task R-0429

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep; seed fixtures before second supported version.

**Objective:** Gate cross-version language, stdlib, package, and evidence
compatibility across the current and previous supported releases.

Test new compiler/old source, old compiler/newly declared source rejection,
old/new package interfaces, lockfiles, proof subjects, certificates, audit
bundles, diagnostics/API snapshots, and migration tooling according to the
published compatibility policy. Unknown future schemas fail loudly; migrations
preserve provenance and downgrade to `needs_recheck` whenever evidence cannot
be retained honestly.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0391** (merged): Duplicate of R-0429: merge into R-0429; intake through R-0431.

### Task R-0431

**Phase:** 3

**Reconciliation (2026-10-19):** Keep from first pilot, not only after release.

**Objective:** Turn every qualifying external failure into durable evidence.

Provide an intake/reduction template that records affected version/target,
minimal source/project, expected and observed behavior, failure boundary,
replay command, privacy/license permission, and disposition. Wrong code, crash,
hang, misleading evidence, compatibility break, diagnostic failure, and severe
performance regression each enter the appropriate permanent corpus or a
documented no-fixture state. Closure requires a class-level prevention gate and
links the advisory/issue, fix, regression, and released versions.

### Task R-0432

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep; reuse pilot evidence and disclose sampling limits.

**Objective:** Measure post-release ecosystem health without hidden telemetry.

Use opt-in diagnostics bundles, explicit surveys, public issue data, CI from
consenting ecosystem projects, reproducible installation probes, and published
compatibility runs. Track install success, first-project completion, upgrade
friction, diagnostic usefulness, real-project compatibility, proof replay,
performance regressions, and time-to-resolution with collection bias and sample
size disclosed. The compiler sends nothing by default; absence of telemetry is
not evidence of success.

### Task R-0433

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep bounded rehearsal before support promises.

**Objective:** Add the post-release area operational validation drill.

Rebuild a release from clean inputs; detect a substituted artifact; issue and
consume a signed revocation; produce a hotfix and rollback; migrate a previous-
version project/evidence bundle; process a simulated wrong-code or false-proof
report through containment, regression, advisory, backport, and release; test
offline behavior; and publish the postmortem with response-time and manual-step
budgets. The drill must distinguish practiced automation from policy that has
never been exercised.

## Area: Packages And Dependency Evidence

Goal: let package users inspect proof, trust, capability, and assumption facts
before adopting a dependency.

Done when: packages have manifests, lockfiles, package-aware facts, trust
policies, provenance, independently checked interface/evidence receipts,
The incremental-driver area-aware reuse/invalidation, and registry protocol.

Prerequisite: the incremental-driver area completed. If it was rejected/deferred, the packages area may
start only after an explicit roadmap amendment replaces these artifact/query
dependencies; package work may not grow a private cache/driver as a workaround.

**Sequencing split — local foundation before beta, ecosystem scale after.** Phase numbering is
topical, not permission to freeze a public release artifact before its primary dependency consumer
exists. The beta/release bar pulls forward only the minimal local foundation: R-0364–R-0369's
manifest/lockfile/workspace/interface split and certificate codec/verification boundary, plus the
narrow trust/assumption/evidence compatibility needed to prove an upgrade cannot silently weaken a
consumer. Public registry/publishing, hosted docs, remote reuse and ecosystem-scale policy remain
post-foundation work. This split avoids both bad extremes: releasing a public evidence schema never
tested across a dependency boundary, or blocking a narrow release on operating a package registry.

**Integrated package management (decided 2026-10-17; implementation planned).**
Package management belongs to the Concrete language toolchain and ships inside
the `concrete` executable. `Concrete.toml`, `Concrete.lock` and source-level
imports describe one package model. The driver owns add/remove, resolution,
fetching, updates, workspaces and native builds/linking; the compiler checks
imports, visibility, capabilities, contracts and dependency evidence against
that resolved graph. CLI, compiler sessions, reports and editor tooling share
the same graph and identities. Reading an import never implicitly downloads a
package or runs dependency code; the frontend receives resolved driver inputs.

Primary references are Cargo's project workflow, Go modules' explicit resolution
discipline, Zig's integrated compiler/build/package experience and Nix's declared
inputs and reproducibility. Decisions and upstream references live in
[package-manager-design.md](research/packages-tooling/package-manager-design.md).

R-0285's queued interop checkpoint pulls forward the minimal local manifest,
lockfile and native-build graph needed by its automatic application workflow;
it does not wait for the full packages-area package protocol or registry. R-0364–R-0367
own the shared package model, explicit updates, locked/offline builds, workspace
lockfile, host-versus-target separation and dependency-path explanations.
Reuse the existing compiler graph as incremental/package infrastructure lands;
do not introduce a separate resolver or cache for foreign bindings. Normal
builds preserve selected versions; explicit package commands own manifest/lock
changes. R-0182/R-0183 own the authority/assumption/evidence upgrade diff.
Public publishing and a separate package-manager executable are outside this
local foundation. This decision scopes existing tasks without reordering the
current execution queue or reducing R-0285's automatic generation/build exit.

### Task R-0363

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep; JSON summaries must not replace canonical authority bytes.

**Objective:** Expand package artifacts only after reports, policies, assumptions, interface artifacts, and CI gates prove what packages must carry. The first package artifact refactor must define exact files:

`Concrete.package.json` (manifest summary), `Concrete.lock`,
`.concrete/interfaces/<module>.json`, `.concrete/facts/<module>.json`,
`.concrete/evidence/<module>.json`, `.concrete/certificates/<module>.json`,
optional `.concrete/verifier-inputs/<module>.car`, and
`.concrete/docs/<module>.json`.
Each artifact must name compiler version, schema version, package id,
source hash, interface/body hash, dependency/evidence root, the soundness-bridge and backend-contracts areas
checker/rule-set identities and receipts where applicable, authority budget,
assumptions, proof/evidence summaries, and replay commands. Add
`scripts/tests/check_package_artifacts.sh`; the gate must prove package
consumers read these artifacts rather than source-private side channels.
This phase is the main 20k+ LOC scale boundary: large Concrete programs need
package/workspace boundaries, import hygiene, visibility/API stability,
versioning, generated docs, and evidence summaries before the language can
claim to support multi-team codebases rather than only single-repo examples.
### Task R-0364

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep minimal exact-pin schema first. Build-time authority is enforced by R-0285's foreign-build execution policy, not only declared.

**Objective:** Design and parse package manifest.

Use one compiler-owned model for Concrete source packages and R-0285's reusable
C/Rust/Zig binding packages. Record upstream selections and locks, reviewed
boundary inputs, toolchain/sysroot, target/CPU/profile, generated binding and
native artifact identities, ordered transitive link inputs and runtime/system
libraries. Applications declare dependencies; the integrated driver selects
bindings and invokes supported foreign build tools without application scripts
or manual compiler/linker flags. Keep normal, test-only and build-tool edges
distinct; build tools target the host while linked libraries target the program.
Build-time tool authority and linked-library runtime authority are separate facts.
Imports resolve only declared dependencies against this same graph.

**Proof-authoring edges (planned 2026-10-18):** record heavy proof/tactic
dependencies separately from runtime, host build-tool and test edges within this
same model, following the [Sparkle sidecar lesson](research/compiler/sparkle-emitted-code-validation.md#additional-workflow-lessons).
Ordinary builds need not fetch or build unused proof-authoring packages. A build
requiring evidence must still validate its retained bundle or replay with the
required dependencies; absence cannot excuse missing/stale proof evidence.
Keep proof dependencies/toolchains in evidence roots even when they do not enter
the executable link graph. Implement this schema increment when a proof-bearing
package needs it; it does not delay the first exact-pin native dependency slice.

R-0285's first slice supplies exact application/binding manifest examples and
expected lock output; include workspace and C/Rust/Zig package cases. Label draft
fields until implemented, then keep the examples in executable fixtures so docs
and parser behavior move together. Expand the schema when a working slice needs
the next field, preserving the shared graph and identity contracts.

### Task R-0365

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep; ranges follow specified conflict behavior. Before automatic fetching is supported: path traversal, symlink/hardlink escapes, decompression limits, interrupted acquisition and atomic installation into the dependency store each have a hostile fixture.

**Objective:** Add version constraints, dependency resolution, and lockfile.

Specify deterministic version selection and permitted duplicate versions before
admitting ranges. Cover diamond graphs, cycles, incompatible requirements,
root-only overrides and native link-identity conflicts; explain the introducing
dependency path and remedy. Lock exact source/header/binding/native input
identities, std/toolchain, target/profile and transitive link requirements.
Paths alone are insufficient: replacing an archive at the same path rejects a
locked build or requires an explicit refresh and dependent revalidation.

Planned `--locked` forbids lock changes, `--offline` forbids network access and
`--frozen` requires both. Explicit add/remove/update/lock commands own resolution
changes; upgrades preview R-0182's API, authority, assumptions and evidence facts.
Fetch and verify locked content before an offline build. A changed native input
invalidates dependent implementation evidence even when its public header stays
unchanged; identity/integrity never supplies behavioral trust or proof validity.
Use existing package/evidence roots and invalidation rules throughout.

Define how one workspace lock represents multiple host/target configurations and
conditional dependencies, so switching targets preserves unrelated selections.
Retain expected outcomes for duplicate-version/native-library conflicts and
root-only overrides. The first R-0285 slice can use exact pins; introduce ranges
only after their selection/compatibility rules and conflict fixtures are decided.

**Graduation criterion (2026-10-21):** R-0285's clean-machine package workflow exercise (install,
resolve pinned dependencies, build native libraries, test, upgrade one dependency, roll back to the
previous lock and reproduce the original artifacts) passes with this task's lockfile and commands.

### Task R-0366

**Phase:** 3

**Objective:** Add workspace and multi-package support.

Use one root, shared lockfile, dependency graph and compiler session. Root-only
overrides and policy apply consistently to every member and command. Gate member
selection, local path edits and host/target separation with matching package
facts and native link inputs under build, test, check and report.

### Task R-0367

**Phase:** 3

**Objective:** Add package-aware test selection.

### Task R-0368

**Phase:** 3, 7

**Objective:** Split interface artifacts from body artifacts at package/workspace scale.

Promote the incremental-driver area internal split and stable ids into versioned public
package artifacts; do not expose or rename the opaque internal cache format
as a package protocol.
Interface artifacts expose public names, types, capabilities, contracts,
allocation/effect summaries, deprecation/version facts, and evidence classes.
Body artifacts contain implementation fingerprints, private obligations,
proof links, emitted IR hashes, and private diagnostics. A dependent package
may compile against interface artifacts without seeing private bodies, but
audit/release bundles must still show when a public claim depends on a
private body proof, trusted boundary, or assumption.
### Task R-0377

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep; local docs before hosting.

**Objective:** Add API docs publishing for packages and stdlib:

 `concrete doc --format json`, `concrete doc --format html`,
 `docs/api/std/<version>/`, and package docs under
 `docs/api/packages/<name>/<version>/`. Generated docs must carry version,
 module path, capabilities, allocation behavior, evidence class,
 deprecation status, and source links where available. Published docs must
 be reproducible from the package artifact.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0378** (merged): Duplicate of R-0377 in export scope: merge export requirements into R-0377; retain hosting as deferred increment.

### Task R-0383

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep separate local-foundation and evidence-policy checkpoints.

**Objective:** Add the packages area validation artifact: a multi-package workspace project with dependency resolution, lockfile, package-aware tests, interface/body artifact split, dependency trust policy, assumption inheritance, authority budgets, provenance, independently checked dependency certificates, evidence-typed imports, published docs, downstream reuse after a dependency private-body edit, invalidation after a public-interface edit, and release-bundle evidence for every dependency. Include remote-cache hostile input/offline fallback if Task R-0382 is implemented. Wire it as `examples/package_workspace/` plus

 `scripts/tests/check_phase18_packages.sh`.

## Area: Editor And Human Tooling

Goal: make evidence visible where developers work.

Done when: editor/LSP/tooling exposes the same facts as CI and command-line
reports through the incremental-driver area session/query graph, with bounded edit
invalidation and certificate freshness, without inventing a second truth source.

Prerequisite: the incremental-driver area completed. If it was rejected/deferred, the editor area needs a
recorded replacement architecture before starting; an editor-only fact database
or cache remains forbidden.

**Minimal pre-release slice, advanced tooling later.** The beta bar pulls forward the smallest
honest editor consumer of the incremental-driver area session: compiler-as-service entrypoint, incremental
structured diagnostics, capability/proof-status hover, obligation navigation and replay-command
links. It must reuse CLI/CI facts and may not wrap batch compilation as its normal edit path.
Proof-aware refactoring, dependency dashboards, migration/deprecation UI, hosted playground and
other breadth stay in the editor area after the core path proves useful. First users should not meet a
proof language exclusively through batch reports, but release is not blocked on a complete IDE.

### Task R-0384

**Phase:** 3, 5

**Objective:** Add artifact viewer integration for proof/evidence facts.

### Task R-0385

**Phase:** 3

**Reconciliation (2026-10-19):** Keep V1a checkpoint.

**Objective:** Add compiler-as-service / LSP entrypoints after diagnostics and facts are structured. The service must host the incremental-driver area `CompilerSession`; it may not wrap batch `runFrontend` as its normal edit path.

The basic editor checkpoint also provides scope/type-aware name and member completion,
call signature help, and find-references for resolved symbols. These operations consume
the shared compiler session and respect visibility and current project revision.
Scripted incomplete-source and edit/repair cases verify useful results without stale
facts or changes to proof authority.

### Task R-0386

**Phase:** 3

**Objective:** Reuse the incremental query/certificate graph as the only editor fact engine.

 LSP/editor requests use the same typed query keys, dependency edges, local
 store, invalidation rules, validation records, independently checked package
 receipts, and evidence views as CLI/CI. Tag each response with project
 revision, subject/dependency root, completeness (`strict | partial`),
 freshness, checker/rule-set version where applicable, and cache/query
 provenance kept separate from evidence status. Cancelled or tolerant
 partial computations cannot enter complete caches or answer proof/release
 queries.

 Gate a scripted edit sequence covering comment/span-only, private body,
 public signature/capability/contract, target/profile, policy, proof, and
 dependency-certificate changes. Assert the bounded the incremental-driver area query execution
 set as well as CLI/LSP fact equality; an editor-only database, stale hover,
 or batch whole-project rebuild for a private leaf edit fails the gate.
### Task R-0387

**Phase:** 3

**Objective:** Add hover/type info for capability status, proof status, predictable status, assumptions, obligations, and trusted boundaries.

### Task R-0388

**Phase:** 3

**Objective:** Add obligation navigation: jump from source contract/index/mod/loop to the generated obligation and discharging theorem.

### Task R-0391

**Phase:** 3, 7

**Disposition (2026-10-19):** Duplicate of R-0429: merge into R-0429; intake through R-0431.

**Objective:** Add backwards-compatibility regression corpus once public users exist.

### Task R-0392

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep minimal diagnostics for early breaking rules; broader automation later.

**Objective:** Add migration/deprecation tooling after the policy exists (Task R-0358):

`concrete migrate --check`, `concrete migrate --apply`, diagnostics for
deprecated syntax/APIs, suggested replacements, edition/version notes where
needed, and mechanical rewrites only for transformations that preserve
evidence facts or explicitly mark them `needs_recheck`. The tool must read
`concrete api-diff --json`, `concrete doc --format json`, and the final
obligation ledger rather than scraping text.
### Task R-0393

**Phase:** 3, 7

**Objective:** Add API-docs/editor integration: LSP go-to-docs for stdlib/package APIs, evidence/capability/deprecation badges in hover, and diagnostic links to `concrete doc` output. The LSP payload must reuse the same doc/evidence JSON as the CLI.

### Task R-0395

**Phase:** 3

**Reconciliation (2026-10-19):** Keep; V1 editor is now the forcing consumer.

**Objective:** [historical origin: closed phase 4 item 11 tail] Route obligation / proof / policy facts into the structured `Diagnostic` record / `--diagnostics-json` channel so LSP/editor and CI-JSON consumers see them (array-bounds, solver-policy, vacuous-contract, stale-proof, …) — without duplicating the `ObligationCore`/report model. Includes interpreter structured diagnostics (historical phase 4 item 18a). Pull when a real consumer (LSP / CI JSON parser) needs machine-readable obligation diagnostics; see LANGUAGE_GAPS for the frontend-vs-obligation diagnostic split.

### Task R-0397

**Phase:** 3

**Objective:** Add installed-binary feature discovery for agents and editor tooling.

  Deliverable: extend the old phase 6E `concrete help --json` / command catalog
  with `concrete agent features --json` only if agent-specific fields need a
  separate view. The catalog must describe supported
  commands, accepted inputs, emitted artifacts, schema versions, evidence
  classes, replay commands, policy gates, forbidden actions, and examples of
  valid next steps.

  Rule: this catalog is the source of truth that MCP, LSP, docs, and LLM
  prompts wrap; they must not duplicate a stale list of Concrete features.
  Done when golden JSON snapshots exist and one agent workflow discovers
  proof synthesis, replay, capability diffs, and counterexample saving from
  the installed binary alone.
### Task R-0398

**Phase:** 3, 7

**Reconciliation (2026-10-19):** Keep minimal V1 checkpoint; defer advanced-feature exit.

**Objective:** Add the editor area validation artifact:

`scripts/tests/check_phase19_editor.sh` runs a scripted LSP/editor session
or golden transcript over one real project, proving hover, diagnostics,
obligation navigation, proof/evidence facts, dependency audit UI, refactor
behavior, docs integration, deprecation diagnostics, and playground output
match CLI facts rather than inventing a second truth source. It also checks
revision/freshness tags, cancellation safety, minimal incremental
recomputation, independently checked dependency receipt changes, and
cache-off/on equivalence.

## Area: Concurrency And Research-Gated Extensions

Goal: keep speculative ideas gated until Concrete's proof/evidence foundation
can contain them honestly.

Done when: each research idea is either pulled into an earlier phase by a
forcing example, explicitly deferred, or rejected.

### Task R-0408

**Phase:** 2, 4

**Disposition (2026-10-19):** Superseded in core scope by R-0496/S7: merge required instrumentation into R-0496; separately specify any extra research.

**Objective:** Broaden the proof-relevant interpreter toward Miri-style UB checking only if the proof-subset interpreter proves valuable.

### Task R-0409

**Phase:** 2, 4

**Disposition (2026-10-19):** Superseded by R-0496/R-0512: retire legacy redesign; carry demonstrated proof debt into Core proof engineering.

**Objective:** Investigate a sized/indexed ProofCore evaluator only if the current fuel-indexed evaluator remains repeated proof debt after HMAC and at least one other substantial loop/composition proof. This is ProofCore v2 research, not a migration commitment; see [research/proof-evidence/SIZED_EVALUATOR_INVESTIGATION.md](research/proof-evidence/SIZED_EVALUATOR_INVESTIGATION.md).

### Task R-0446

**Phase:** 1, 2

**Reconciliation (2026-10-19):** Keep; misplaced in the research phase — its generated trap/fuzzer/consumer coverage is an increment of R-0491/R-0493.

**Objective:** Generate the operation-semantics/trap inventory rather than
hand-maintaining it, and extend its consumer set.

R-0005 removed the recurrence mechanism for bug 053: `IntArith` now answers
whether and how an operation traps, for the unary family as it already did for
the binary one, and folding, DCE, the interpreter and EmitSSA read that one
answer (`scripts/tests/check_trap_inventory.sh`, mutations #25-#27). Any retained
backend consumes the shared semantic inventory through backend-neutral
interfaces; its emitter-independence requirements remain in force.

What R-0005 asked for and did not build is the *generated* form. Two consumers
remain outside the inventory entirely: **fuzz generation**, which would emit
trapping cases per constructor and is the thing that would surface the next
trapping operation without an audit, and the **capability matrix**. A
hand-written inventory still requires someone to remember to extend it when a
constructor is added; a generated one does not, and generation is what makes
the consumer list checkable rather than conventional.

Gate: a new trapping constructor added to the inventory must appear in
generated fuzz cases without editing the generator, and every declared consumer
must be shown to read the generated artifact rather than a local copy.

---

### Task R-0487

**Phase:** 3, 4

**Reconciliation (2026-10-19):** Keep reporting analysis separate from mandatory capability containment.

**Objective:** Infer the authority each function's body MAY use, and use that to check and
explain capability declarations — never to replace them. Every inferred fact is a potential
effect, not guaranteed execution: a reachable network call means "may use `Network`", not
"uses the network".

**Status (2026-10-04): rewritten around R-0484's accepted semantics; planned, not scheduled.**
R-0484's partial checkpoint (`a7c9cf1c`) made the header authoritative: `with(...)` is the
complete list of a function's external authority, handles carry their capability in their
type (`Writer<C>`) and using one requires `with(C)`, `trusted` absorbs only `Unsafe`, and
every foreign binding declares its effects as an audited assumption. The task's original
framing (2026-09-26: split `with(...)` into separate `requires`/`carries`/`performs` facts,
with authority travelling unannounced inside handle values) is superseded on both counts:
`carries` is now expressed in types, and the declared header is the primary fact, not an
inferred one. What survives is inference as a CHECK over declarations and as the basis for
negative answers.

1. **Potential authority per function.** Compute the capabilities a body may use through
   direct calls, handle use at each handle's instantiated capability, function-pointer
   values with a resolved target set, and foreign bindings' declared effects. Potential
   use beyond the declaration is already a checker error. A declaration broader than the
   potential set is reported as an over-declaration — an advisory least-authority hint,
   issued only when the analysis is complete (no unresolved edge), never an error:
   conservative declarations remain valid, for instance to keep an interface stable.
2. **Instantiation-level reports.** A capability-generic helper reports its variable
   (`print_bytes<cap C>` may use `C`), and each call site reports the instantiation
   (`Console` where `base64_cli` calls it), so a reviewer sees concrete authority where it
   is chosen.
3. **Negative questions only under completeness.** "Does this touch the network?" is an
   absence, so it is answered only when the potential set has no unresolved edge — a
   function-pointer value whose targets are unknown, or a dependency summarised without
   capability facts. An unresolved edge yields `unknown`, naming the edge. An answer that
   passes through a foreign binding is conditional on that binding's declared effects and
   names the assumption (R-0484 R10). See `docs/project/ABSENCE_IS_NOT_A_FACT.md`.
4. **Out of scope:** what is mutated through arguments, termination, and which inputs
   influence which outputs. Those are separate contracts (R-0488's parameter-mutation audit;
   R-0473/R-0474 typed contracts), and `with(...)` does not claim them.

**Exit:** `print_bytes` reports "may use `C`" generically and "may use `Console`" at its
`base64_cli` call site; a function declaring `with(Network)` whose completely-analysed body
may use nothing network-capable gets an advisory over-declaration note, and the same
function with an unresolved edge gets none; a negative claim is refused where the potential
set has an unresolved function-pointer edge, naming it; a negative claim that depends on a
foreign binding names the assumption. Gate: a fixture pair in which the same helper is called through a resolved
and through an unresolved function pointer yields `none` and `unknown` respectively, plus a
positive control where a `Writer<{}>` helper may use no external authority.
