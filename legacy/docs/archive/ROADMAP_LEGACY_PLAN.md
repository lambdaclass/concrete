# Legacy Roadmap Execution Plan

Status: reference. Moved on 2026-11-02 from ROADMAP.md; schedules nothing.
Task and exit IDs remain citable here. The [current roadmap](../../ROADMAP.md) owns scheduling.
The notice below records the state at the move. Owner decisions live in
[decisions.md](../spec/decisions.md#owner-decisions).

> **Not in force (2026-11-02).** From here to the end of this file, the execution plan for the
> current compiler (where we are, what is next, phases, queue rows and their exits, feature
> prerequisites, capacity, estimates and releases) is kept for reference until the restructure
> replaces it; [the interim queue](roadmap-2026-11/ROADMAP.md#the-queue) is the only queue. Still in force here: the
> [owner decisions](../spec/decisions.md#owner-decisions) and their rule, the [bug dashboard](../bugs/DASHBOARD.md#bug-dashboard) as a record,
> and the [development throughput](roadmap-2026-11/ROADMAP.md#development-throughput) process work.

**Where we are (2026-10-21).** Phase 1 (Stabilize) is in progress: the soundness repairs for bugs
079 – 101 are landing (queue row 0) and R-0482's P1 closures follow (row 1). Probes added since
2026-10-19 found bugs 102 – 120 in about two days, nearly all of them new instances of the recurring
[structural classes](../bugs/STRUCTURAL_CLASSES.md): point repairs are not converging (the
[bug dashboard](../bugs/DASHBOARD.md#bug-dashboard)'s discovery-rate signal measures this, and Phase 1 exit 2m closes
only when, in every structural class, three consecutive probe batches find about nothing), which is why
the [priority decision of 2026-10-21](roadmap-2026-11/ROADMAP_DECISIONS.md#decisions) makes the verified compiler's structural work the
primary track. The next release is the experimental systems preview; its
[blockers](#next-release-blockers) are listed under [Releases](roadmap-2026-11/ROADMAP.md#releases). Fixes implemented on
branches but not yet accepted on main are listed under
[Implemented, not yet accepted](roadmap-2026-11/ROADMAP.md#implemented-not-yet-accepted).

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
throughput](roadmap-2026-11/ROADMAP.md#development-throughput) track (R-0539 – R-0547, with R-0513's pre-push selection) brings
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
[owner decision](../spec/decisions.md#owner-decisions)):
early executable feedback is how defects surface (the RocksDB groundwork already found bugs 102 and
103). What a deliverable's [prerequisite checklist](#feature-prerequisites) controls is
**acceptance**: RocksDB slice 1, V1a, V1b and V1c are not accepted, and their safety is not
advertised, until their prerequisite exits hold. The remaining exits continue alongside and still
close Phase 1. Every bug record states the design fact behind it and its structural owner.

**Where things live.** This file answers only *what happens next, in what order*: the active and
next phases, the one execution queue with its exits, the owner decisions, the feature prerequisites,
the capacity allocation and the release map. Everything else has its own home:

- [Task register](roadmap-2026-11/ROADMAP_TASKS.md): the full text of every task a queue row names or an
  active or next phase schedules, filed by area.
- [Roadmap backlog](roadmap-2026-11/ROADMAP_BACKLOG.md): every other task, each with its pull trigger
  ([pull rules](roadmap-2026-11/ROADMAP.md#backlog-and-pull-rules)).
- [Decision log](roadmap-2026-11/ROADMAP_DECISIONS.md): the dated decisions that shape the queue.
- [Direction and standing reference](roadmap-2026-11/ROADMAP_DIRECTION.md): the
  [capability unlocks](roadmap-2026-11/ROADMAP_DIRECTION.md#capability-unlocks--the-stable-product-map),
  the [north star](roadmap-2026-11/ROADMAP_DIRECTION.md#north-star-compositional-fail-visible-verification),
  the definition of done, the gate-writing checklist and the [glossary](roadmap-2026-11/ROADMAP_DIRECTION.md#glossary).
- [Validation and promotion](../project/PROMOTION_PIPELINE.md#promotion-pipeline): which checks a
  change passes during development, before merge and before qualification or release, the CI tiers and
  the result vocabulary.
- [Verified-compiler plan](../compiler/VERIFIED_PIPELINE.md): Phases 2 and 4 in detail.
- Normative architecture: [Evidence Architecture](../verification/EVIDENCE_ARCHITECTURE.md) defines
  the five authoritative evidence objects and [Verification IR](../verification/VERIFICATION_IR.md)
  the target portable proposition boundary. This roadmap sequences their implementation and must not
  invent competing definitions.
- Language rules: the language documents under `docs/language/`. The rule-defining sections this file
  used to carry (the [language baseline](roadmap-language-scope.md#language-baseline-and-bounded-verification-milestone),
  the cross-cutting language decisions, the linearity and stdlib-boundary decisions) are in the
  [archived language scope](roadmap-language-scope.md), pending absorption into the
  language specification.
- Defects: the [bug dashboard](../bugs/DASHBOARD.md#bug-dashboard) below and the [bug records](../bugs/README.md).
- Records: the [reconciliation record](roadmap-2026-11/ROADMAP_RECONCILIATION.md), the
  [roadmap history](ROADMAP_HISTORY.md), the
  [retired task bodies](ROADMAP_RETIRED_TASKS.md) and [CHANGELOG.md](../../CHANGELOG.md).

## Phases and releases

Seven phases each group rows of the [execution queue](#the-current-execution-queue), have an exit, and
produce a release scope (see [Releases](roadmap-2026-11/ROADMAP.md#releases)). After Phase 1, Phase 3 runs alongside the proof
track, which progresses from Phase 2 into Phase 4; the shared-front-end work overlaps proof development.
Effort is divided between tracks by the [capacity allocation](#capacity-allocation), in which the
verified compiler holds the majority; new soundness defects take priority over every phase and are
disposed of by the [defect disposition rule](#the-current-execution-queue).

| phase | goal | queue rows | exit | release |
|---|---|---|---|---|
| **Phase 1 — Stabilize** | Land the 2026-10-18 soundness repairs, close R-0482, put the immediate protections in place | 0 – 2 | every [Phase 1 exit](#phase-1-exits) (0a – 2q): repairs gated; R-0482 qualified; one `Verdict` authority; language rules answered by the project owner and enforced | 0.1 Experimental systems preview |
| **Phase 2 — Verified foundation** | First proved end-to-end slice (V1a, V1b, V1c) and the shared compiler structure for the whole language: identities, the authoritative Core checker, one `Layout`, Place MIR | 0s, 3 – 4 | V1a and V1b theorems and V1c's source correspondence; Core checker authoritative; one `Layout` and Place MIR in the existing profile | 0.2 Verified preview |
| **Phase 3 — Usable language** | Interop, application workloads and the usability track | 5, 7, 8 | four-adapter interop checkpoint; usability checkpoints | none of its own; feeds every release |
| **Phase 4 — Verified growth** | Cumulative slices V2 – V7 and claim migration to Core | 6 | each slice's theorem; V7 covers the feature inventory | 0.3 – 0.8 Verified-subset milestones |
| **Phase 5 — Evidence as a product** | Compiler provenance, canonical task, independent consumer, receipt v1 | 9 – 11 | receipt v1 frozen | 0.9 Verified-component alpha (with Phases 4 and 6) |
| **Phase 6 — Richer verification** | Typed contracts, `old`/frames, ninth table, totality and spec library | 12 – 15 | typed contracts with exact identity; `old`/frames; ninth table converted; checked totality | 0.9 alpha increments |
| **Phase 7 — Release and ecosystem** | External-user workflow, three flagships, package evidence and policy | 16 – 17 | non-author replay and upgrade; flagships graduate; package evidence composes | 0.10 Project beta; 1.0 Stable; 1.x |

| row | tasks | goal |
|---|---|---|
| 0 | R-0482 | Land the active soundness repairs that exits 0a – 0c list (exit 0b's list is generated from the bug records) and qualify R-0482 |
| 0s | R-0555 | Complete, mechanized language specification (the WebAssembly model): the first row after row 0, on track A in the active proof slot, never paused except for row-0 repairs |
| 1 | R-0482 | Make R-0482's cross-package (P1) controls permanent (exit 1a) |
| 2 | R-0491, R-0493, R-0494, R-0498, R-0500, R-0506, R-0513, R-0550, R-0553, R-0554 | Immediate protections (exits 2a – 2q): one verdict authority, prevention harness, trustworthy gates, axiom policy, one `IntArith`, language rules, demonstrated enforcement, the owner's language decisions (answered), compile-time scaling, compiler totality, enforcing the decided rules with one corpus migration, then the repository cleanup |
| 3 | R-0511, R-0517 | First cumulative verified releases: design proofs, V1a, V1b and V1c, each accepted when its [prerequisites](#feature-prerequisites) hold |
| 4 | R-0492, R-0497, R-0556, R-0499, R-0514, R-0502, R-0501, R-0355 | Shared compiler structure (verified-pipeline Part 1): identities, the authoritative Core checker, one `Layout` and Place MIR for the whole language, measured against a development-speed baseline |
| 5 | R-0285, R-0283, R-0286, R-0287 | Bounded C/Rust/Zig interop: RocksDB slice 1 (work proceeds now; accepted when its [prerequisites](#feature-prerequisites) hold), then the four-adapter checkpoint |
| 6 | R-0491–R-0510, R-0512, R-0557 | Slices V2 – V7, global state as linear tokens, and evidence migration to generated Core statements |
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
- **Phase 2 — Verified foundation** (rows 0s, 3 – 4). **Goal:** The first proved end-to-end slice (V1a Core → BIR, V1b native AArch64 through the Lean Compiler Backend, V1c restricted source → checked Core correspondence, with design proofs for ownership, borrows and memory first), and verified-pipeline Part 1's structure for the whole language: identities, the authoritative checker on Core, one `Layout` and Place MIR in the existing profile. **Exit:** V1a and V1b theorems with explicit boundaries and V1c's correspondence theorem; the Core checker authoritative for every program; the existing profile's layout from one `Layout` and its lowering jobs in Place MIR passes. **Release:** **0.2 Verified preview** (with Phase 3's tooling checkpoints); the candidate states where the theorem begins.
- **Phase 3 — Usable language** (rows 5, 7, 8). **Goal:** Bounded C/Rust/Zig interop, real application workloads leading to the ethlambda port, and the usability track: onboarding, external pilots, diagnostics, editor and debugging, published performance. **Exit:** the four-adapter interop checkpoint; continuing workloads and pilots; each usability checkpoint's exit. **Release:** no release of its own: it runs alongside and feeds every release from 0.1 on (onboarding, pilots, tooling, the minimal editor path for beta).
- **Phase 4 — Verified growth** (row 6). **Goal:** Cumulative slices V2 – V7 (aggregates and references, heap and linear resources, generics and capabilities, raw pointers and FFI, the rest of the language) each with its theorem, and eligible claims migrated onto compiler-generated Core statements independently of backend coverage, with explicit deprecation for retired claims and historical replay preserved. **Exit:** each slice's cumulative theorem; V7 covers the maintained feature inventory. **Release:** **0.3 – 0.8 Verified-subset milestones** (a slice may span several releases); V7 is required for 1.0.
- **Phase 5 — Evidence as a product** (rows 9 – 11). **Goal:** Compiler provenance and one canonical verification task, the independent evidence consumer, and the receipt v1 freeze. **Exit:** receipt v1 frozen after producer and independent consumer agree on hostile fixtures. **Release:** **0.9 Verified-component alpha**, together with Phases 4 and 6 for the admitted component.
- **Phase 6 — Richer verification** (rows 12 – 15). **Goal:** Typed contracts and exact contract identity, two-state (`old`, frames) contracts, the ninth-table conversion, and totality with a specification library. **Exit:** typed contracts with exact identity, `old`/frames, the ninth table converted, checked totality and the spec library. **Release:** **0.9 Verified-component alpha** (its typed-contract, two-state and required totality increments).
- **Phase 7 — Release and ecosystem** (rows 16 – 17). **Goal:** External-user workflow and three graduated flagships, then package-level evidence and typed policy. **Exit:** a non-author replays and upgrades with exact diffs; three flagships graduate; package evidence composes under typed policy. **Release:** **0.10 Project beta**, then **1.0 Stable supported subset** (V7 with source correspondence, Phases 5 – 7's acceptance, release-bar compatibility and post-release readiness) and **1.x** feature releases.

The [verified-compiler plan](../compiler/VERIFIED_PIPELINE.md) holds Phases 2 and 4 in detail; the
[task register](roadmap-2026-11/ROADMAP_TASKS.md) holds every row's task and its exit.

### Estimated plan to a proven compiler

Estimates, recorded 2026-10-25 with the project owner's approval; they are revised as slices land and
are not commitments or release dates. Each duration counts from **D**, the date the owner answers
OD1 – OD3 and OD10 – OD11 (references, `&mut` arguments, pattern binders, arithmetic and casts; see
[owner decisions](../spec/decisions.md#owner-decisions)), because V1a and the slices after it are checked under those
rules. Until D, proof development proceeds within the rules as they are on main and does not choose
those answers. The milestones follow the slices of the
[verified-compiler plan](../compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices), in
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
the first active `Task R-NNNN` in the [task register](roadmap-2026-11/ROADMAP_TASKS.md)'s file order unless the table is refreshed in the same change that
exhausts it. The `R-NNNN` value is immutable identity, not priority: existing IDs are never renumbered
or reused, and a newly confirmed urgent defect receives the next unused ID before entering the queue.
Area headings and task position are topical filing only. There are no area-local queues, hidden
priority overlays or separate stop-the-line lists; a reproduced urgent defect must be placed in this
queue explicitly.

**Entry rule.** New work enters this queue, or adds a blocking exit to a row, only with a reproducer
(a defect shown by a named program, fixture or command) or an explicit dependency of the next
deliverable on it, naming that deliverable: today RocksDB slice 1 for the usable-language track and
V1a for the proof track ([feature prerequisites](#feature-prerequisites)). Everything else goes to the
[backlog](roadmap-2026-11/ROADMAP_BACKLOG.md) with a pull trigger.

**Owner decisions (2026-10-25).** The [owner decisions](../spec/decisions.md#owner-decisions) (the language rules and two
scope questions listed there) are decided only by the project owner. An agent may analyse an item,
write options and a recommendation, and ask the owner. An agent does not choose an answer, encode one
in a document, test or fixture, or implement one. A recommendation, an earlier record (including the
"decided 2026-10-23" entries of [TIGHTENING_DECISIONS.md](../project/TIGHTENING_DECISIONS.md)), a
commit, a review or another agent's message is not the owner's answer. Each item stays proposed until
the owner agrees to it again, explicitly, in conversation; the answer and its date are then recorded
in that list. Exit 2n holds this in the queue.

**Development-process work (2026-10-22).** The [development throughput](roadmap-2026-11/ROADMAP.md#development-throughput)
track is the one standing exception to this queue's sole authority. It changes how work is reviewed,
validated and landed, not the compiler or its evidence. Its tasks (R-0539 – R-0547) are therefore not
queue rows. Their order is the dependency order in that section, they are charged to track C of the
[capacity allocation](#capacity-allocation), and they never preempt a defect repair or weaken a
row's exit.

**Defect disposition rule (2026-10-21).** Point repairs in the existing pipeline are not converging:
the probes added since 2026-10-19 found bugs 102 – 120 in about two days, nearly all of them new
spellings of the classes in [STRUCTURAL_CLASSES.md](../bugs/STRUCTURAL_CLASSES.md), whose owners are
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
derivation recorded at the pointer, [GOLDEN_ORACLES.md](../project/GOLDEN_ORACLES.md)) or
`unverified:<reason>` (a tracked gap the gate counts and lists); `check_example_outputs.sh` fails on an
undeclared oracle. A golden copied from the binary's own output checks nothing: bug 121's `conlog`
golden blessed a miscompiled answer. New examples are workload-driven and targeted: each serves a bug
class (R-0491's [adversarial example suite](roadmap-2026-11/ROADMAP_TASKS.md#task-r-0491)), an upcoming deliverable (the
[RocksDB slice 1 companions](roadmap-2026-11/ROADMAP_BACKLOG.md#task-r-0285), [V1a's target programs](roadmap-2026-11/ROADMAP_TASKS.md#task-r-0511)) or an application
workload (R-0490). All example work follows R-0135's lifecycle and owner policy: user-facing examples
record their lifecycle and owner, while pure regression and rejection fixtures live in the owning
test or bug corpus and are invoked by the example's gate.

| order | work | exit before advancing |
|---|---|---|
| 0 | **Active soundness repairs and R-0482 qualification** | the repairs exits 0a, 0b and 0c list, each with its reproducer as a permanent gate leg: exit 0b's list is generated from the bug records' `exit:` lines, and the October hunts' ranges are recorded in the changelog. Bugs 305 and 306 are landing and bug 307 is under review against OD15 (b). Repair campaign isolation and retain a qualified result naming its exact commit before closing the migration. Closes through exits 0a, 0b and 0c ([Phase 1 exits](#phase-1-exits)) |
| 0s | **Complete, mechanized language specification: the WebAssembly model (R-0555)** | **the top priority (owner decision, 2026-10-30): until its steps 1 – 6 at the top of this file hold, only row-0 silent soundness repairs and the landing of already-validated branches run beside it, and no other row starts; this overrides the capacity allocation for that period.** Its order is that nine-step list. Its exits are R-0555's workstreams: (a) every chapter's Lean `Semantics:` link required; (b) every `docs/language` file and the archived language-scope text absorbed, and the missing chapters written; (c) the conformance suite, which is the existing rule-ID fixture index, not a second corpus; (d) bugs cite the rule they violate (new bugs now; older records are backfilled inside the chapter that absorbs their rule, which blocks no deliverable). Each chapter closes independently: its prose absorbed, then its accept and reject examples under rule-ID fixtures, then its Lean definition and `Semantics:` link landed together. Absorbing the remaining `docs/language` files and the language-scope text is a near-term milestone that completes before exit 2q's migration of std, examples and tests starts, so the migration targets one source. The spec does not gate R-0496 as a whole: R-0496 lands each Core definition with the links of the chapter it defines; R-0499 implements the answered prose rules construct by construct, refusing anything unspecified explicitly; and the soundness proofs of those rules wait, slice by slice, for their link. Chapter gates per deliverable are under [feature prerequisites](#feature-prerequisites) |
| 1 | **R-0482 P1 cross-package closures** | promote the manifestful std/dependency controls into permanent gates, retain D3's manifestless distinction and negative controls, and publish only after the subject-binding repair. Closes through exit 1a |
| 2 | **Immediate protections and semantic decisions (R-0491, R-0493, R-0494, R-0498, R-0500, R-0506, R-0513, R-0550, R-0553, R-0554)** | closes through the individually closable exits listed in [Phase 1 exits](#phase-1-exits), each with its owner, acceptance test and class: one `Verdict` authority renders every evidence surface (2a); permanent reproducer controls for 080 – 101 (2b) and the prevention harness (2c); trustworthy gate execution — checked inventories instead of incidental count pins (2d), minimum completed comparisons (2e), a fast push tier separate from full qualification (2f); the axiom-policy gate on today's evidence (2g); one `IntArith` for the existing pipeline (2h); the rules the next slice needs written down and enforced with versioned migration diagnostics — strictly second-class references and no block `let` shadowing (2i); typed literals, literal range, statement vs value position, sequential assignment within a loop iteration, evaluation order and short-circuiting (2j); explicit match binders and cast semantics per width (2k); demonstrated enforcement (2l); the bug-discovery rate at about zero in every structural class across its three latest probe batches (2m); the project owner's answers to the [owner decisions](../spec/decisions.md#owner-decisions) (2n, closed: every one answered on 2026-10-29, the questions raised since tracked under 2q); the decided rules enforced and std, examples and tests migrated to them once (2q); a compile-time scaling gate (2o); compiler totality, starting with a ratchet gate that lets the count of non-total compiler declarations only go down (2p), ahead of the verified-pipeline pass proofs of rows 3, 4 and 6. After it comes the repository cleanup programme (R-0554), queued at the project owner's request as an explicit exception to the entry rule and charged to track C; it has no exit of its own, and its sub-items continue alongside and are recorded in the changelog as they land. Known authority and safety defects are repaired under row 0. An exit classed *blocks features* holds back the acceptance of only the deliverables whose [prerequisites](#feature-prerequisites) name it; the others continue alongside feature work. Broader prevention coverage continues throughout and does not block isolated proof development |
| 3 | **First cumulative verified releases (R-0511, with R-0517)** | reviewed machine-checked design examples for move/destroy, borrows, reborrowing, cleanup and typed-value memory representation before Core's interfaces freeze; V1a (checked Core → BIR refinement for the minimal scalar subset) and V1b (native AArch64 through the Lean Compiler Backend, R-0517, with final-image correspondence; LCB's Stage 1 exit is necessary but insufficient — V1b needs a pinned certified LCB domain covering every reachable V1 operation and behaviour, including checked-arithmetic trap paths, direct calls and `print_int`, plus Concrete's proved adapter and semantic bridges; a smaller leaf-function intersection ships only as a separately named intermediate theorem; R-0517's explicitly selected fallback does not complete this row) with explicit theorem boundaries; checker soundness, capability containment, resource and target-memory-validity obligations and one transported claim for the admitted subset; V1c, restricted source → checked Core correspondence for V1's surface (syntax, resolution and elaboration judgments specified independently of their implementations, then proved or certificate-checked; constructs outside the subset rejected explicitly), before any V1 result is called source-to-native; retained artifacts with minimal task identity and independent replay. The design examples and isolated proof development start now; V1a, V1b and V1c are accepted only when their prerequisite exits hold (prerequisite exits: 0a, 0b, 2a, 2g, 2h, 2i, 2j; V1c also waits for exit 2q from the moment its correspondence proof is built, since that proof is stated against the decided rules; the programs each must compile and the independently checked artifact are under [feature prerequisites](#feature-prerequisites)). See [the plan](../compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices) |
| 4 | **Shared compiler structure: identities, the authoritative checker on Core, one `Layout` and Place MIR for the whole language (R-0492, R-0497, R-0556, R-0499, R-0514, R-0502, R-0501, with R-0355's baseline)** | verified-pipeline Part 1, in the primary track's migration slot ([capacity allocation](#capacity-allocation)), alongside row 3: before the first whole-language change of R-0492, R-0499/R-0514, R-0502 or R-0501 lands on main, R-0355's [development-speed baseline](roadmap-2026-11/ROADMAP_TASKS.md#task-r-0355) is recorded, so each structural change's cost to build time, rebuild time, compiler memory and test turnaround is visible; the parts V1a's subset exercises proceed with V1a, and the whole-language rollout starts once row 0's in-flight candidates (batch 2, type-identity, generic-destroy) are accepted on main, which edit the same checker code; it implements the rules as exits 2i – 2k state them and does not wait for row 2's other exits, and it implements the recorded [owner decisions](../spec/decisions.md#owner-decisions) by stable rule ID, starting without waiting for exit 2q (all of OD1 – OD157 are answered); the front end both profiles share uses `LocalId`/`DefId` from Resolve on, including the source checker's environments, with immutable payload-bound artifacts; one checker over Core implements every ownership, borrow, reference-escape, capability and `Unsafe` rule for the whole language, emits payload-bound `CheckedCore` consumed by checked specialization in both profiles; specialization produces payload-bound `CheckedMonoCore`, the only input to lowering; the source-tree checker holds no such rule; the 087 borrow-block case and 090 – 094 are rejected by it; capabilities are re-checked after Mono. Then, in the same slot: one `Layout` authority (R-0502) supplies every size, alignment, offset and enum representation the existing lowering and LLVM emission use, with a gate comparing it with the emitted types (class 12 of the [structural classes](../bugs/STRUCTURAL_CLASSES.md)); and `Lower`'s sequencing, match compilation, cleanup/`defer` and check expansion move into Place MIR passes (R-0501) that consume `CheckedMonoCore`, as shadow paths with disagreement gates against the existing lowering (class 8). Their semantics and proofs still grow per slice (rows 3 and 6), and the old `Lower` retires only under the plan's retirement conditions. The slices prove this checker's rules as they reach them (V1 – V7, beginning with scalar typing and basic capabilities in V1, downward borrows in V2, and extending through resources, polymorphism and unsafe/FFI interactions); this row makes it authoritative for every user now |
| 5 | **Bounded interop track (R-0285, with R-0283/R-0286/R-0287)** | the one bounded executable workload beside the verified compiler, in the single feature slot ([capacity allocation](#capacity-allocation)): first deliver **RocksDB slice 1**, the first executable feature deliverable: implementation and experiments proceed now (its groundwork on main already found bugs 102 and 103), and it is accepted, and its safety advertised, only when its prerequisite exits hold (prerequisite exits: 0b, 2i, 2q; checklist and acceptance under [feature prerequisites](#feature-prerequisites)); then deliver the four-adapter checkpoint, after R-0515's practical debugging checkpoint and with the clean-machine upgrade-and-roll-back exercise (RocksDB C, Rust crypto/networking, an upstream Zig library; no application bridge/build scripts, generated-file edits or manual flags) on the supported existing profile alongside verified-pipeline work; foreign implementations remain named trust dependencies; interop neither waits for nor blocks the first native theorem |
| 6 | **Cumulative slice growth and evidence migration (R-0512, R-0491–R-0510 by slice, R-0557)** | extend the proved language and frontend correspondence slice by slice (V2 aggregates and downward references, V3 heap and linear resources, V4 full reference/resource interactions, V5 generics/traits/capabilities, V6 raw pointers/FFI/separate compilation, with global resources as linear tokens and the token-wrapper gate (R-0557, OD8), V7 the maintained feature inventory); migrate eligible claims to generated Core statements independently of backend coverage; ship supported releases and preserve historical replay throughout |
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
| 0a | R-0482 | bug 079's subject/model/spec binding is closed at status, issuance and receipt consumption, and the evidence and model repairs land: the bug 080 cast model, 081, 083 – 086, 089, 101 and the 065 stack-depth policy verdict | each bug's reproducer is a permanent gate leg (the `check_model_diff.sh` legs for 080, 084 – 086 and 101; the 079, 081, 083 and 089 reproducers in the evidence gates; 065's in the stack-depth policy gates), and the [bug ledger](../bugs/README.md) records each fixed on main | blocks features | known false proofs and unsound claims |
| 0b | R-0482 | the wrong-code and memory/capability-safety repairs land: <!-- BEGIN GENERATED: exit 0b bugs (scripts/gen/bug_ledger.py; edit the records' exit lines) -->052, 082, 087, 088, 090 – 094, 096 – 100, 102 – 107, 109, 111, 113, 115, 121 – 123, 126 – 129, 131 – 133, 135 – 137, 140, 142 – 152, 165, 166, 176, 177, 180 – 183, 185, 186, 188 – 190, 192 – 198, 200, 204, 205, 208, 210, 215, 218, 220, 221, 238, 239, 247, 251, 253, 271, 272, 285, 287, 301, 302, 305, 306, 311, 332, 333, 340<!-- END GENERATED: exit 0b bugs -->, 095 (no record yet). The list is generated from the bug records' `exit: 0b` lines: every silent bug the [discovery record](../bugs/discovery.tsv) lists, except the false proofs and unsound claims exit 0a names, carries one, as do the memory-safety repairs it does not mark silent or that predate it (052, 093 – 095, 099, 100, 142, 145 – 148), and a silent bug filed later gets one in its record when it is filed. Among them is the silent defect found while writing the [2026-10-21 semantics decisions](roadmap-2026-11/ROADMAP_DECISIONS.md#decisions) into the docs: the binary evaluating a dereference assignment's destination before its right-hand side while the interpreter does the reverse ([evaluation order](design-records/STATEMENT_EXPRESSION_MODEL.md#evaluation-order-and-assignment); filed as bug 183). The other one recorded then, an arm binder through a borrowed scrutinee whose root is moved inside the arm ([MEMORY_SEMANTICS §15.7](design-records/MEMORY_SEMANTICS.md) G17), is refused on main (E0204, re-probed 2026-10-23) and pinned by rejection fixtures; bugs 181 and 182 close under the [2026-10-23 tightening decisions](../project/TIGHTENING_DECISIONS.md#4-decisions-of-2026-10-23) D3 and D2, and bug 166 under D4's refusal; whether those rules stay is the project owner's answer to [owner decisions](../spec/decisions.md#owner-decisions) OD1, OD3 and OD4 (exit 2n), and a different answer must still leave each of those bugs repaired | each bug's reproducer is a permanent gate leg, and the bug ledger records each fixed on main | blocks features | use-after-free, double free, skipped destruction and capability escape in safe code (052, 087, 090 – 093, 099 – 100, 091, and 104 and 131: a reference assigned from an inner block outlives its source, to a local or to a reference parameter), a safe user function named like a C symbol replacing it for the whole program (129: `fn malloc` becomes the allocator `alloc` calls), a comparison operator consuming its linear operands (140: `Heap == Heap` compared addresses and leaked both cells), a `return` inside a call argument or literal field leaking the values already moved into the construct (142), a deferred call reading a local freed after its `defer` was registered (143: `defer pb(&h); free(h);`), heap-shell interiors exempt from linearity (144: a field read twice double-frees, `free(h)` drops a linear interior), the raw `vec_set`/`vec_free` intrinsics losing non-Copy elements (145), `*h` never freeing its cell (146), a move inside an erased `assert`/`ghost let` counted as a release that never runs (147), a move inside the right operand of `&&`/`||` counted although it may not run (148), loop conditions, returns in nested blocks and labelled exits not checked for owned values (149), double free through `vec_get` taking a non-Copy element by value (115), silent wrong code (082, 088, 096 – 098, 102, 107, 111, and 123: a borrow of a module constant reads an uninitialized slot; 127: struct-literal initializers run in declaration order natively and in the ratified source order in the interpreter; 128 (fixed 2026-10-21): the interpreter, the differential reference, modelled an integer-to-float cast as the identity; 150 (fixed 2026-10-21): SSA cleanup folded `x != x` to false for a float `x`, so a NaN passed every such check; 152 (fixed 2026-10-21): `string_trim("")` returned a byte past its source; 151 (contained by E0299): `a.b_f` and `a_b.f` flattened to one name and the binary's `a_b::f()` ran `a.b_f`; 133: a write through `&mut (*p).f` lands in a copy of `*p`; 136: functional update `S { .., ..base }` evaluates its base once per omitted field; 137: enum-variant literal fields run in declaration order in both consumers; 176: a loop body's shadowing `let` outlived the loop in the checker, so a `u8` binding held 300 natively and in the interpreter; 105: a reference-valued `if`/`match` reached through a generic instantiated at a reference type, which the checker containment did not cover, segfaulted natively), a silent semantic change by a tool (135: `concrete fmt` rewrote float literals to six decimals, so `0.0000000001` became `0.000000`), missing extern effect declarations accepted in nested submodules (113; calls still charged `Unsafe`), a foreign destructor silently replaced by an empty body (103, a leak of every C resource released through a `*_destroy` function), and LLVM type layouts that disagree with the compiler's own, so aggregate copies move fields (121: conlog freed a corrupted String pointer; 122: the canonical builtin `Option`/`Result` union, including a stack overrun); the defects found on 2026-10-21 are a read through an arm binder after its source moved, and an interpreter/native disagreement on dereference-assignment order; and heap-slot corruption through a `&mut Heap<T>` argument (126: the callee treats the caller's heap-pointer slot as the struct, so a field write overwrites the slot and `free` receives a corrupted pointer) |
| 0c | R-0482 | campaign isolation is repaired and a qualified campaign result naming its exact commit is retained before the identity migration closes | a [stage (c)](../project/PROMOTION_PIPELINE.md#promotion-pipeline) qualification record with `verdict=qualified` at that commit: every required job succeeded there, the mutation aggregate's `qualified=1` among them | continues alongside | qualification of the identity migration; no deliverable depends on it |
| 1a | R-0482 | the manifestful std/dependency controls are permanent gates, D3's manifestless distinction and its negative controls are retained, and the result is published only after 0a's subject-binding repair | the P1 controls run as permanent legs of the full gate set, and the manifestless negative controls fail when the D3 distinction is collapsed | continues alongside | cross-package claims are not a dependency of RocksDB slice 1 or V1 |
| 2a | R-0494 | one `Verdict` authority renders every evidence surface (contracts, vcs, obligation-ledger, audit, check-proofs text and JSON, proof-status, issuance) | R-0494's exit: status spellings only in the renderer table; bug 089's reproducers (`rt_ens_only`, `rt_ens`, `rt_ens_bogus`, `rt_ens_noens`) read not-proved on every surface; a cross-surface gate (new) pins the expected positive and negative judgment for every claim and clause | blocks features | the proof surface is feature-frozen until it lands ([verified-compiler decision](roadmap-2026-11/ROADMAP_DECISIONS.md#decisions)), so V1's published claim needs it |
| 2b | R-0491 | permanent reproducer controls for 080 – 101: each bug has a checked-in record, reproducer, expected outcome and pre-fix/fixed SHA pair | each assigned control fails at its pre-fix SHA and passes at its fix; `check_bug_filing_complete.sh` and `audit_bug_corpus.sh` fail on a missing inventory entry | continues alongside | the reproducers already block as gate legs under 0a and 0b; this exit proves each one load-bearing |
| 2c | R-0491 | the prevention harness (S1): the extracted model, then Core's semantics, as a third side of the differential fuzzer over every shape that broke, outcomes classified and never skipped, and model-diff for zero-argument, `bool` and aggregate signatures plus a binary leg with a pinned claimed-function inventory; and the adversarial example suite for today's bug classes; and closing the loud failures that keep valid examples out of the interpreter/native comparison (108 – 110, 112, 114, 116, 124, 130, 134, and interpreter gaps 117 – 120, all four fixed by 2026-10-21; the interpreter-coverage pass also closed D4 (`return` in value position) and D13 (output before a trap)); permanent differential legs for struct- and enum-literal field order, functional-update evaluation, assignment order and integer → float conversion ([2026-10-21 semantics decisions](roadmap-2026-11/ROADMAP_DECISIONS.md#decisions)); and every example golden independently checked: each `@run` rule declares its oracle (`interp`, `spec:<pointer>` or `unverified:<reason>`, [GOLDEN_ORACLES.md](../project/GOLDEN_ORACLES.md)), and the unverified goldens the interpreter gaps leave are retired by interpreter agreement or a recorded derivation | R-0491's detection map: the deterministic differential suite detects 080 and 084 – 088, name-binding controls 081 – 082, evidence and cross-surface controls 083 and 089, checker mutants 090 – 094 and the binary leg 095 (`check_model_diff.sh`, `fuzz_differential.py`); and each of the eight examples of R-0491's [adversarial example suite](roadmap-2026-11/ROADMAP_TASKS.md#task-r-0491) (`linear_flow`, `generic_destroy`, `nested_names`, `import_aliases`, `casts_widths`, `mut_receivers`, `match_and_fnptrs`, `statement_sequencing`) passes its own leg of `scripts/tests/check_adversarial_examples.sh` (new, in the full gate set), comparing interpreter output, native output and the expected output, plus the proof model and claim statuses where the example carries claims; each bug assigned to this exit (108 – 110, 112, 114, 116, 117 – 120, 124, 130, 134) has its reproducer permanently checking the corrected expected outcome, and the bug ledger records each fixed on main; `check_example_outputs.sh` fails on an undeclared oracle (with negative controls) and reports zero unverified goldens | continues alongside | prevention plus repairs of loud compilation/link failures and interpreter gaps, without a demonstrated authority or safety defect; the generator coverage 2i – 2k need is part of their own acceptance; the examples add prevention coverage, while existing repairs and containment controls keep their owners and acceptance; a defect an example newly reproduces enters the queue under the entry rule, as a new row-0 exit when it is an authority or safety defect |
| 2d | R-0513 | checked inventories replace incidental count pins: every expected claim and comparison is accounted for, exclusions carry reasons, and missing entries, skipped work or unexpected eligibility shrinkage fail | adding a legitimate proved example needs no incidental count edit; controls with a deleted inventory entry, a disabled generator, an empty suite and altered eligibility each fail | continues alongside | gate affordability; no reproduced soundness defect |
| 2e | R-0513, R-0491 | minimum completed comparisons by semantic feature, with permanent positive and negative controls | every generated feature has a pinned minimum completed-comparison count, and a run below it fails | continues alongside | prevention; no reproduced defect |
| 2f | R-0513 | a fast push tier separate from full qualification | the fast tier meets its recorded budget (target five minutes); full results bind exact commits, toolchains and artifacts; a fast-tier pass is never presented as full qualification ([promotion pipeline](../project/PROMOTION_PIPELINE.md#promotion-pipeline)) | continues alongside | validation cost; no deliverable depends on it |
| 2g | R-0506 | the axiom-policy gate on today's authoritative evidence at status, issuance and consumption: `sorryAx` and unregistered axioms rejected transitively, kernel-only evidence limited to `propext`, `Classical.choice` and `Quot.sound`, registered native-evaluation and float axioms under distinct trust labels | positive controls and forbidden-transitive-dependency mutations, with `check_axiom_inventory.sh` | blocks features | V1's evidence must pass the declared axiom policy (R-0506 is a stage V1 exercises) |
| 2h | R-0493 | one `IntArith` is the arithmetic authority for the existing pipeline's evaluation, folding and arithmetic-model generation, with emitted operations checked against the same semantics | `check_single_arithmetic.sh` (new) and differential controls over every supported width and operation family, including casts and defined traps | blocks features | V1a's checked `+ - *` and their traps are stated through it (R-0493 is a stage V1 exercises) |
| 2i | R-0500 | the safety rules of [MEMORY_SEMANTICS §15](design-records/MEMORY_SEMANTICS.md) are written down and enforced: strictly second-class references — they exist only as parameters, call arguments, borrow-block binders and arm-scoped pattern binders through a borrowed scrutinee; no reference binding is introduced by `let` or assigned; none is stored in an aggregate or a constant, and constants are values, never places — and one binding per visible scope chain (SCOPE-1: no binder reuses a visible name; sibling scopes may), with the design-example decisions on `&mut` arguments and unused exclusive references (B1, B2) and owner reads under a shared borrow (B7) recorded | per rule: a normative paragraph in `docs/language/` with accepted and rejected examples, cited fixtures, S1 generator coverage meeting its minima and a versioned migration diagnostic; the 092 and 093 reproducers stay rejected, 087's shadowing reproducers, accepted today, become rejected by the shadowing rule, and the `let`-bound and assigned reference shapes (REF-6), accepted today, become rejected. Delivered 2026-10-21: the renamed pattern binder (G4), SCOPE-1@1 (E0301; bug 124 closed), REF-6@1 (E0302 / E0303, at Check and after monomorphization) and the versioned migration diagnostic of every rule, with the acceptance manifest `tests/rules/exit_2i.tsv` checked by `scripts/tests/check_rules_2i.sh`; what remains before this exit closes: S1 generator coverage and its minima (G8, with R-0491 and 2e), the B1/B2/B7 decision records (G9, G10), the mutation rows (R-0498), and the unenforced shapes G12, G13, G15, G16 and G17 (MEMORY_SEMANTICS §15.7) | blocks features | their violations were use-after-free and leaks in safe code (087, 092, 093); RocksDB handles and V1 locals are checked under them |
| 2j | R-0500 | the rules V1a's subset needs are written down and enforced: typed literals, literal range (signed minima preserved), statement vs value position, sequential assignment within a loop iteration, evaluation order and short-circuiting ([the evaluation-order rule](design-records/STATEMENT_EXPRESSION_MODEL.md#evaluation-order-and-assignment): left to right in source order, struct and enum literal fields in source order, a functional-update base evaluated once after the explicit fields, an assignment evaluating its right-hand side before its destination), and what an assignment does to the old value (MEMORY_SEMANTICS §10) | the per-rule acceptance of 2i | blocks features | named dependency of V1a: the verified pipeline never models looser rules than the compiler enforces ([plan](../compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices)) |
| 2k | R-0500 | the remaining disputed rules are written down and enforced: explicit match binders and cast semantics per width pair, as [ARITHMETIC_POLICY §8](design-records/ARITHMETIC_POLICY.md#8-numeric-conversions-as) decided it on 2026-10-21: `as` accepts only the always-lossless numeric, `bool` and `char` conversions of its class table, and every lossy conversion is a compile error that uses named truncation, reinterpretation, rounding or checked conversion; numeric scope only (pointer and reference casts unchanged); staged: the named operations, then the rejection with versioned migration diagnostics, then the migration | the per-rule acceptance of 2i | continues alongside | their defects (080, 084) are repaired under 0a; casts enter at a later V1 increment and match at V2, so neither is a dependency of RocksDB slice 1 or V1a |
| 2l | R-0498 | enforcement is demonstrated (S3): a claim → gate-leg → targeted-mutation manifest over the verification and language docs, enforcement-bearing comments in `Concrete/Proof`, `Check` and `Backend`, and emitted contract text, with R-18 – R-20 relabelled shape-only until proved | every enforcement phrase in that scope at the commit that opens this exit has a manifest row whose mutation its leg kills in a qualified ([stage (c)](../project/PROMOTION_PIPELINE.md#promotion-pipeline)) campaign; later phrases join row 6's per-slice growth | continues alongside | prevention; no reproduced defect |
| 2m | R-0491 | the bug-discovery rate has fallen to about zero in every structural class. For each class of [STRUCTURAL_CLASSES.md](../bugs/STRUCTURAL_CLASSES.md) (a bug's primary class is the first class row that lists it; bugs in no class form one more bucket), its last three eligible *counting* probe batches in the [discovery record](../bugs/discovery.tsv) (new workloads, fuzzing campaigns, new examples, hunts, semantics probes, specifications, design proofs, interpreter coverage) each found no silent bug of that class and at most one loud bug of it; the exit closes when every class does. A batch that probes a structural migration's own new code carries the tag `class N: ` at the start of its probe cell: it counts toward that migration's class only, and every bug it finds counts against that class, but a silent bug it finds still ends the streak of the bug's own class. Each batch records its effort, and a quiet batch counts only with a recorded effort of at least four hours or ten probe programs, examples or cases (a smaller, token batch is recorded and shown but does not count). Reviews, fix follow-ons, CI and development attribute their bugs but are not probe rounds | the [bug dashboard](../bugs/DASHBOARD.md#bug-dashboard)'s per-class discovery table, generated by `scripts/gen/bug_dashboard.py` and checked by `check_docs_drift.sh`, reads three quiet batches for every class at the closing commit, and every bug record from 079 on is attributed to one batch (the generator refuses an unattributed record, and a tag that names no class or marks an attribution-only batch) | continues alongside | a convergence measure: it says whether the point repairs and the structural work have stopped the classes of [STRUCTURAL_CLASSES.md](../bugs/STRUCTURAL_CLASSES.md) from producing new bugs; it is kept per class because a migration in progress (R-0492's, whose probes filed about 15 bugs on 2026-10-25) finds bugs in its own class while it lands, and one global count could not close before every migration had finished; no reproduced defect and no deliverable depends on it, while Phase 1 and the preview's advertised stability do |
| 2n | project owner (the answers are recorded under R-0500) | the project owner answered OD1 – OD32 explicitly in conversation and each answer is recorded in the [owner decisions](../spec/decisions.md#owner-decisions) list with its date; closed on 2026-10-29. Enforcing the answers, and the questions raised while recording them (OD33 – OD46 and OD48 – OD93 and OD95 – OD157), are exit 2q's | each of OD1 – OD32 carries a dated answer given by the owner; no row is filled from a recommendation, an earlier record, a commit or an agent's message | continues alongside | closed: no deliverable waits for it any more |
| 2o | R-0550 | compile time is gated for scaling, with R-0422 repairing the class (structural class 17): generated inputs of 10^4 – 10^5 elements or statements in each shape (array literals, long functions, many locals, many declarations, deep nesting) compile within a recorded budget at every front-end and back-end stage, and no stage grows worse than near-linearly between sizes. Reproducers: bug 027 (EmitSSA's quadratic rendering), bugs 248, 249 and 250, and the long-function slowdown (forty thousand statements: 8.5 s parse, 13.4 s resolve, 9.5 s check, 9.5 s elab; being filed) | `scripts/tests/check_compile_scaling.sh` (new, in the full gate set) compiles the generated corpus per stage, fails on a stage over its budget or a size ratio above its recorded near-linear bound, and retries within a recorded retry budget so host noise cannot fail it; the budgets are measured on the CI host and recorded with the SHA they were measured at; the pre-fix commit of each reproducer fails the gate and its fix passes | continues alongside | compile-time defects are loud (a slow or failed build, never wrong code), and no deliverable depends on them; the gate keeps class 17 from returning while R-0422 removes it |
| 2p | R-0553 | compiler totality, step 1: `scripts/tests/check_totality_ratchet.sh` (landed 675871b4) inventories from the Lean environment every `partial`, `unsafe`, `opaque`, `implemented_by` and `extern` declaration in the `Concrete.*` modules against the committed baseline `docs/compiler/totality_baseline.tsv`, the source of truth for their count; it runs in CI and in the pre-push hook. Converting the baseline, stage by stage ahead of the pass proofs of R-0491 – R-0510, continues under R-0553 in Phases 2 and 4 | the gate fails on any declaration missing from the baseline and on any stale baseline row (each with a negative control: a new `partial def` and a baseline row whose declaration became total), and passes at the closing commit in CI and in the pre-push hook | continues alongside | a proof of a pass cannot see inside a `partial` function, so totality is a prerequisite of the pass proofs in rows 3, 4 and 6; it repairs no reproduced defect and no deliverable's acceptance depends on step 1, so it blocks no feature |
| 2q | R-0500 | the decided rules are enforced and std, examples and tests are migrated to them once, in one user-visible transition: OD16, OD17, OD18, OD21, OD23 – OD32, OD47 and OD8's root and resource tokens, with the spelling questions its rows name (OD33 – OD46 and OD48 – OD93 and OD95 – OD157), all answered by 2026-11-02. Its bugs: 216, 226, 311, 317, 318, 319 – 328 and 331. The rules of V1a's subset fold into their existing exits instead (no `Int` fallback, statement versus value position and the loop-exit form into 2j; arithmetic and abort into 2h and 2j; casts and match into 2k). Only minimal temporary enforcement goes into the existing pipeline where an acceptance needs it sooner, each piece naming its retirement point; the permanent implementation lands once, in the Core checker (row 4), by stable rule ID. The migration starts after the specification's phase-1 absorption is complete (row 0s), so it targets one source | a rule-by-rule manifest names each rule's enforcement stage, fixtures, migration status and owning exit; every listed rule's pending examples in `docs/spec` are enforced, and its bug (or R-0557's work) is fixed on main with std, examples and tests migrated in the same transition | blocks features | RocksDB slice 1 is checked under these rules, and V1c's correspondence proof is stated against them from the moment it is built; it does not block Core-to-BIR proof work (V1a, V1b) or row 4 |

### Feature prerequisites

Implementation and experiments on the first feature deliverables proceed now, within the rules as they
are on main; no early work, proof development included, chooses, encodes or implements an
[owner decision](../spec/decisions.md#owner-decisions) that is still open (all of OD1 – OD157 are answered). Early executable
feedback is how defects surface, and the RocksDB slice 1 groundwork already found bugs 102 and 103. The
exits listed against a deliverable are required before it is **accepted** or its safety is advertised;
none of them is removed or weakened by starting early, and a defect the early work finds enters the
queue under the entry rule. Isolated proof development (the reviewed design examples, proof libraries)
waits for none. Every *blocks features* exit appears in
at least one *blocked by* list and no *continues alongside* exit appears in any. Exit 2q blocks V1c
from the moment V1c's correspondence proof is built (queue row 3 says so by name); it does not
block V1a or V1b.

**Chapter gates (row 0s).** Each deliverable also waits for the specification chapters it is stated
against, each linked to the Lean definitions it uses: V1a and V1b for the Core chapters of the admitted
subset (evaluation, literals, arithmetic, traps, divergence, loops and exits, calls, `print_int`); V1c
for the source chapters of V1's surface (the lexical subset, names, statements, elaboration and
abort), linked to independently specified judgments, with constructs outside the subset refused;
RocksDB slice 1 for the normative rules, with enforcement and conformance cases, of its resource,
borrow, capability and FFI behaviour; and any whole-language claim for the complete specification.
V1a's design examples start now and wait for no chapter.

| deliverable | blocked by | not blocked by |
|---|---|---|
| RocksDB slice 1 (row 5, R-0285) | 0b, 2i, 2q | 0a, 0c, 1a, 2a, 2b, 2c, 2d, 2e, 2f, 2g, 2h, 2j, 2k, 2l, 2m, 2n, 2o, 2p |
| V1a (row 3, R-0511) | 0a, 0b, 2a, 2g, 2h, 2i, 2j | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l, 2m, 2n, 2o, 2p, 2q |
| V1b (row 3, R-0511 with R-0517) | 0a, 0b, 2a, 2g, 2h, 2i, 2j | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l, 2m, 2n, 2o, 2p, 2q |
| V1c (row 3, R-0511) | 0a, 0b, 2a, 2g, 2h, 2i, 2j, 2q | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l, 2m, 2n, 2o, 2p |

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
  [first executable slice](roadmap-2026-11/ROADMAP_BACKLOG.md#task-r-0285); the four-adapter checkpoint follows. R-0285's four
  companion FFI and resource examples (a file-handle wrapper, a C library holding a callback and its
  context, a resource pool, multi-step error paths that release everything) are accepted with or after
  this slice; they do not block it.
- **V1a** (R-0511). Must compile, from checked Core to executable BIR, every program of the V1a subset:
  `i64`, `u64`, `bool`, locals and assignment, conditionals, a general loop with structured exits,
  non-recursive direct calls, comparisons, checked addition, subtraction and multiplication, and one
  capability-authorized `print_int` event, with terminating, trapping and divergent behaviours. Other
  widths, casts, division, shifts, recursion and further loop forms are later V1 increments. R-0511's
  [V1a target programs](roadmap-2026-11/ROADMAP_TASKS.md#task-r-0511) are the named instances of this subset and V1a's regression set;
  they do not widen it.
  **Independently checked artifact:** the Lean proof of checked Core → BIR refinement for that subset
  and its retained artifacts with a minimal canonical task identity, replayed by the independent
  retained-artifact consumer
  ([plan, V1 milestones](../compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices)).
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

**Suspended while row 0s runs its steps 1 – 6** (owner decision, 2026-10-30, at the [top of this file](roadmap-2026-11/ROADMAP.md#concrete-roadmap)). The shares and slots below resume when those steps hold.

Queue order says what comes first within a track; this table says how effort is divided between the
tracks, so that the verified compiler cannot be displaced by the work beside it. It replaces the earlier
limit (one shared-compiler migration checkpoint and one proof checkpoint, with interop, application,
usability and release work "under the same limit"), which did not say whether feature work could take
the migration slot. `check_roadmap_coverage.sh` checks that every queue row after Phase 1 and every
Phase 1 exit belongs to exactly one track, that the primary track holds the majority and the rows of
V1 and the shared compiler structure, and that the feature track is capped with a single slot.

| track | queue rows | Phase 1 exits | share per cycle | integration slots | scope |
|---|---|---|---|---|---|
| **A. Verified compiler** (primary) | 0s, 3, 4, 6 | 0b, 2h, 2i, 2j, 2k, 2n, 2p, 2q | at least 55% | 2: one shared-compiler migration checkpoint (row 4, then each slice's migrations) and one proof checkpoint (rows 3 and 6), which row 0s shares: the specification is not a third slot, and it is paused only for row-0 repairs and never preempts tracks B or C | the semantics the proofs rely on (overflow, casts, evaluation order, traps, divergence, memory, with executable examples that agree: R-0493, R-0496, R-0500); the shared compiler structure; ownership and capability soundness, preserved through specialization (R-0499, R-0507, R-0510); source → Core correspondence (V1c, then each slice's frontend obligation); V1a – V7 |
| **B. Evidence integrity** (protected) | 9, 10, 11, 12, 13, 14, 15, 17 | 0a, 1a, 2a, 2g | at least 15% | 1 | specification and claim correctness (the claimed property is the theorem's proposition, typed contracts); one verdict authority; the axiom policy; task identity and evidence bound through linking to the final image and its dependency versions; independent checking. Before rows 9 – 11, the bounded increments every published V1 claim needs: R-0521's minimal task identity and R-0353's minimal independent retained-artifact consumer |
| **C. Adversarial validation and gate health** (continuous) | — | 0c, 2b, 2c, 2d, 2e, 2f, 2l, 2m, 2o | at least 10% | 1 | the prevention harness, scheduled hunts and fuzz campaigns, the adversarial example suite, demonstrated enforcement, trustworthy gates and qualification (R-0491, R-0498, R-0513); the compile-time scaling gate (R-0550, exit 2o); the repository cleanup programme (R-0554), queued in row 2 without an exit |
| **D. Bounded features** (capped) | 5, 7, 8, 16 | — | at most 20% | 1 | interop, application workloads, usability, release preparation and the external-user workflow, one checkpoint at a time |

- **Cycle.** Shares are measured over a planning cycle (a calendar month until release trains start,
  then one train) as the share of integration and review effort spent on each track's checkpoints.
  They are minimums and one maximum, not quotas: capacity a track of A – C does not use may go to
  another of A – C, never to D beyond its maximum. A cycle that misses a minimum or exceeds the maximum
  records the split, the reason and the recovery as a dated note under [Decisions](roadmap-2026-11/ROADMAP_DECISIONS.md#decisions).
- **Slots.** At most one checkpoint per slot is active for integration, each with its accountable
  maintainer, required reviewer and exit recorded before it starts. D's single slot means feature work
  grows by sequence, not by parallel checkpoints: RocksDB slice 1 holds it until it is accepted, and
  the next D checkpoint (R-0515's preview checkpoints, the four-adapter checkpoint, release
  preparation) is chosen at a cycle boundary by what the next release needs. Heavy validation runs
  are limited to two per machine at a time (R-0543, 2026-10-22); a further one queues behind them
  whatever its track.
- **Defects.** Repairs preempt every track and are charged to their exit's track in this table, under
  the [defect disposition rule](#the-current-execution-queue); a new defect's exit names its primary
  class in [STRUCTURAL_CLASSES.md](../bugs/STRUCTURAL_CLASSES.md) and its track when it is added. When repairs keep A below its minimum for
  two consecutive cycles, the structural owner of the class causing most of them moves to the front of
  A's migration slot.
- **Development throughput.** The [development throughput](roadmap-2026-11/ROADMAP.md#development-throughput) track (R-0539 –
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


## Development throughput

**Decision (2026-10-22): the development cycle's feedback loop is a priority track, ordered ahead of
the next feature wave.** Its tasks are R-0539 – R-0547, with R-0513 for pre-push gate selection; their
bodies are under [Area: Development Throughput](roadmap-2026-11/ROADMAP_TASKS.md#area-development-throughput). The track is charged to
track C of the [capacity allocation](#capacity-allocation). It does not preempt defect repairs, but
no new track D checkpoint starts after RocksDB slice 1 until R-0547's measurements are recorded and
landing trains (R-0539) on the landing CI tier (R-0540) are in use. The validation semantics do not
change. The [promotion pipeline](../project/PROMOTION_PIPELINE.md#promotion-pipeline)'s stages, its exact-SHA binding and its
passed / failed / skipped / cancelled / incomplete vocabulary bind every item below. A faster path
never reports a check it did not run as passed.

The measurements behind the decision and the parts already in use on main are recorded in the
[roadmap history](ROADMAP_HISTORY.md#moved-at-the-2026-10-28-roadmap-compaction).

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
[roadmap history](ROADMAP_HISTORY.md#next-release-blockers-met-before-2026-10-28).
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


## Area scheduling before the restructure

## Where every area fits

The task catalog below is filed by topical **area**; areas are filing locations, not scheduling units.
Until 2026-10-19 they were numbered "Phase 7" to "Phase 20"; the *old phase* column maps those numbers,
which older documents and the changelog still use, to the area. Old Phases 1 – 6E, and the completed stdlib-area foundations and
workloads 1 – 8, are complete and recorded in [CHANGELOG.md](../../CHANGELOG.md). Task counts include retained completed and retired stubs; they
are not counts of unfinished work. The second number counts the tasks of that area in the
[backlog](roadmap-2026-11/ROADMAP_BACKLOG.md): tasks no active or next phase schedules (see
[Backlog and pull rules](roadmap-2026-11/ROADMAP.md#backlog-and-pull-rules)). Each task increment follows its explicit queue assignment or the
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

## Task directions before the coherent plan

These bodies preserve superseded implementation directions. They schedule no work.
The live register carries the approved surviving requirements. Regression duties and
acceptance controls remain binding as constructs land, except for frozen-internal repairs
and migration procedures superseded by the new-compiler decision.

<a id="coherent-plan-r-0491"></a>

### Historical task R-0491

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The body combines extracted-model and old report machinery with explicit source, evidence, binary and inventory regression duties. surviving requirement: Retain the defect inventory, independently justified expected outcomes, classified spec/native differential comparisons, evidence and consumer controls, and non-vacuous campaign coverage as new slices admit their constructs. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; extend explicit defect inventory through 101. Extend the permanent defect inventory and per-surface expected results through bug 101.

**Objective:** Prevention harness — stage S1 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

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
[docs/bugs/STRUCTURAL_CLASSES.md](../bugs/STRUCTURAL_CLASSES.md): every bug record from 102 on
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
[GOLDEN_ORACLES.md](../project/GOLDEN_ORACLES.md)), or `unverified:<reason>` (allowed only when the
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
reproducer: it enters the queue under the [entry rule](roadmap-2026-11/ROADMAP.md#the-queue), as a new row-0
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

- **R-0092** (superseded): superseded — the three-way differential is Core semantics / LLVM profile / LCB-native under R-0491. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0092)
- **R-0093** (merged): duplicate of R-0092: merge; labels are hypotheses, not proof of faulty component. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0093)
- **R-0094** (superseded): superseded by R-0491: merge; retain hand/reference/metamorphic controls.
- **R-0095** (superseded): superseded by R-0491/R-0118: merge manifests and minimum completed comparisons.
- **R-0096** (superseded): superseded by R-0513/R-0491: merge; remove mandatory QBE availability.
- **R-0105** (superseded): superseded by R-0491/R-0504: merge debug/release and sanitizer controls. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0105)
- **R-0106** (merged): duplicate of R-0009/R-0491: merge new backend predicates into shared reducer/harness.
- **R-0273** (superseded): superseded by R-0491/R-0498: merge; map every listed family to a retained control. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0273)
- **R-0290** (merged): Duplicate of R-0491: merge into R-0491, retaining backend/profile matrix.


<a id="coherent-plan-r-0492"></a>

### Historical task R-0492

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The remaining migration names old Elab, SSA and PExpr, but the exit preserves identity and forgery invariants. surviving requirement: Use scope-resolved identities throughout the new compiler, distinguish lexical from persistent evidence identity, and retain alias, symbol, binding, substitution and artifact-forgery controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep under R-0514; avoid unnecessary legacy migration before retirement.

**Objective:** Resolved identities — the identity half of S5; R-0497 owns immutable, payload-bound artifacts — stage S5 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-23): slice 1 landed on branch `vp-r0492`; the slice contract is S5 slice
contract 2 in [VERIFIED_PIPELINE.md](../compiler/VERIFIED_PIPELINE.md).** `LocalId` is minted by
the one lexical walk (`Resolve/Locals.lean`), which runs inside `Pipeline.resolve`. The checker's
environment is keyed by `LocalId`, and E0301 is rendered from the same walk. Checks are in
`check_local_identity.sh`, plus mutation family 98. The slice found bug 202: project builds
skipped desugaring. Remaining: Elab, Core, SSA and `PExpr` binders; `DefId`, `FieldIdx` and
`VariantIdx`; contract expressions; `ElabVerify`.
**Slice 6 (2026-10-25, branch `wip/r0492-decl-identity`):** declaration identity for module-level
types and constants, design in [DECLARATION_IDENTITY.md](../compiler/DECLARATION_IDENTITY.md).
Steps 1 – 2 landed: one type environment per module in Resolve (bug 230 fixed) and Core spells a
user `Result` apart from the builtin (bug 227 fixed). Step 3 landed on `wip/r0492-steps34`: types
are compared, and methods and trait impls looked up, by `TypeId` in Resolve, Check and Elab (bug
223 fixed; the inherited aliased-imported-method obligation is met by its fixtures). Step 4 landed
there too: a public constant is imported by its `ConstId`, referenced in Core by its defining module
(`cfg::BASE`) and inlined by Lower and the interpreter in that module's scope (bug 233 fixed). The
probes filed bugs 242 (imported signature types read by spelling) and 243 (aliased newtype import).

`LocalId` for every binder, allocated at
Resolve so the source checker and Elab both consume it; `DefId` as a compilation-scoped declaration
handle allocated before body elaboration, preserving callable namespaces, specialization arguments
and extern/intrinsic distinctions, with a validated `DefId → DefinitionIdentity` mapping at evidence
boundaries; owner-checked `FieldIdx`/`VariantIdx`; `RegId`/`BlockId` in SSA. No `String → identity`
coercions: identities come from scope-aware environments through checked adapters;
capture-preserving alpha-canonicalization before hashing, frozen identity bytes preserved or
explicitly versioned. Pipeline artifacts (owned by R-0497) become immutable and payload-bound (slice 1 sealed `ValidatedCore`,
`MonomorphizedProgram` and `SSAProgram`, 2026-10-23), with diagnostic artifacts typed separately.

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

- **R-0007** (superseded): superseded by R-0492/R-0510: merge, preserving function-symbol and metamorphic controls. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0007)
- **R-0008** (superseded): superseded by R-0492: merge reproducer and alias permutations into identity migration.
- **Aliased imported method (E0264)**, from the archived 2026-08 gate-health record: a method call on an
  aliased imported type is rejected today; the identity migration resolves it and adds an exact
  alias-method fixture (reconciliation prose item, merged through R-0008).
- **R-0103** (superseded): superseded by R-0492/R-0505: merge; general separate compilation arrives in V6.
- **R-0114** (superseded): superseded by R-0492: merge; distinguish compilation-local IDs from persistent evidence identity. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0114)


<a id="coherent-plan-r-0493"></a>

### Historical task R-0493

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The immediate checkpoint repairs the existing pipeline, while typed operations and boundary-value agreement survive. surviving requirement: Define typed arithmetic and casts from one specified semantics, preserving widths, signedness, operation modes, literal ranges and defined-trap regression controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep immediate and slice checkpoints separate.

**Objective:** Typed operations — stage S6 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

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

- **R-0079** (superseded): superseded by R-0493/R-0501/03: merge into typed operations and pass obligations. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0079)
- **R-0221** (superseded): superseded by R-0493/S6: merge semantic ownership into R-0493. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0221)
- **R-0464** (superseded): superseded by R-0493/R-0495/R-0460: merge; archive completed H24 fix and retire SSA-first prescription. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0464)


<a id="coherent-plan-r-0496"></a>

### Historical task R-0496

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The old interpreter replacement retires, while the body requires the semantic contract and raw-memory interface before Core freezes. surviving requirement: Specify the new Core semantic contract and values-to-bytes boundary, then implement its clock-indexed evaluator and instrumented semantics with representative memory, clock and trace proofs. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; state implementation versus proved-subset coverage separately.

**Objective:** Core semantics — stage S7 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

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
- **R-0234** (superseded): superseded by R-0496/S7, V7: merge into V7 float-profile work. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0234)
- **R-0235** (superseded): superseded by R-0496/R-0495: retire PVal/PBinOp expansion; implement Core float semantics. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0235)
- **R-0261** (merged): duplicate of R-0496/R-0460: merge; already explicitly reconciled.
- **R-0268** (superseded): superseded by S5–S8: retire undecided architecture question; plan has chosen the chain.
- **R-0276** (superseded): superseded by R-0506/R-0496: retire general legacy rewrite; retain migration-needed wrappers only. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0276)
- **R-0408** (superseded): Superseded in core scope by R-0496/S7: merge required instrumentation into R-0496; separately specify any extra research.
- **R-0409** (superseded): Superseded by R-0496/R-0512: retire legacy redesign; carry demonstrated proof debt into Core proof engineering.
- **R-0456** (superseded): superseded by R-0496 for proposed PExpr port: retire PExpr-port design; reconsider only a measured Core-metatheory proposal. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0456)


<a id="coherent-plan-r-0497"></a>

### Historical task R-0497

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The landed wrappers belong to the old pipeline, but payload binding and diagnostic-path exclusion are explicit exits. surviving requirement: Make new pipeline artifacts immutable and bound to exact certified payloads, with separate diagnostic types and compile-fail controls for forgery, mutation and bypass. This line governs over older implementation directions for the legacy compiler.

**Objective:** Immutable, payload-bound pipeline artifacts — the artifact half of stage S5 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md); R-0492 owns the identity half.

**Status (2026-10-23): slice 1 landed on branch `verified-pipeline`; the slice contract is in
[VERIFIED_PIPELINE.md](../compiler/VERIFIED_PIPELINE.md) (S5 slice contract 1).** `ValidatedCore`,
`MonomorphizedProgram` and `SSAProgram` are sealed and carry erased certificates about their exact
payload. `ElaboratedProgram` is sealed. `UnverifiedSSA` and `CoreSubset` are separate uncertified
types. Compile-fail controls are in `check_pipeline_artifacts.sh`. The slice found bug 180: a project
report replaced a certificate's payload and ran Mono on the result. Remaining: the stage types
after SSA as each IR lands; binding `ElaboratedProgram` to its input; payload-bound evidence
records (class 10); the identity half under R-0492.

Each stage's output is an immutable artifact bound to its exact payload, constructible only by its producer or verifier (slice 1, 2026-10-23, sealed `ValidatedCore`, `MonomorphizedProgram` and `SSAProgram` in `Pipeline.lean`); any mutation yields an unverified artifact; diagnostic artifacts (including `lowerUnverified`) have separate types that downstream stages and evidence APIs reject. The stage sequence follows the target IR chain (HIR → Core → `CheckedCore` → `CheckedMonoCore` → Place MIR → Memory CFG → BIR → LLVM printer or Machine IR), extended as each IR lands. Private constructors are API discipline; soundness comes from the stated theorems.

**Exit:** compile-fail controls cover artifact forgery, certificate reuse after mutation and the diagnostic-path bypass; `Pipeline.lean` exposes only typed stages.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0073** (superseded): superseded by R-0497/R-0503: merge into payload-bound validated BIR artifacts.
- **R-0082** (superseded): superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0082)
- **R-0116** (superseded): superseded by R-0497/R-0510/R-0514: merge; use payload binding and new IR chain. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0116)


<a id="coherent-plan-r-0498"></a>

### Historical task R-0498

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old Proof, Check and Backend inventory retires, while the exit requires demonstrated enforcement rather than comments. surviving requirement: Map every new enforcement claim to its production gate leg and targeted mutation, and label unproved shape checks honestly. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; grow manifest with new boundaries.

**Objective:** Enforcement demonstrated — stage S3 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; starts now.**

A claim → gate-leg → targeted-mutation manifest over
verification and language docs, enforcement-bearing comments in `Concrete/Proof`, `Check`,
`Backend`, and emitted contract text; the backend contract is generated from BIR op semantics once
S11 lands. R-18 – R-20 relabelled shape-only until their proofs exist.

**Exit:** every in-scope enforcement phrase has a manifest row whose mutation its leg kills in a
qualified campaign.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0273** (superseded): superseded by R-0491/R-0498: merge; map every listed family to a retained control. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0273)


<a id="coherent-plan-r-0499"></a>

### Historical task R-0499

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: Shadowing the old source checker is obsolete, but declarative rules and their discriminating controls remain required. surviving requirement: Define independent Core judgments and a corresponding authoritative checker, preserving ownership, authority, bounds and reference regressions with positive and rule-breaking mutation controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; R-0514 owns whole-language rollout.

**Objective:** Checker rules, authoritative on Core — stage S8 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

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
- **R-0228** (superseded): superseded by R-0499/S8 and V6: merge enforcement into R-0499; raw-operation extensions into V6. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0228)
- **R-0267** (superseded): superseded by R-0499/R-0507/R-0495: merge; capabilities and purity remain distinct.
- **R-0274** (superseded): superseded by R-0499/R-0514: retire source-checker/CompilerDB authority prescription. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0274)


<a id="coherent-plan-r-0500"></a>

### Historical task R-0500

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The body’s prose-first homes and old implementation checkpoints are superseded, while its rule and example obligations survive. surviving requirement: Dispose of the listed language rules under the approved decisions and give each its Lean judgment, semantic clause and accepted, refused or trapping conformance examples as slices land. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; row 6 adds the rules each slice admits.

**Objective:** Written language rules — stage S4 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; starts now.** 2026-10-21: exit 2i's SCOPE-1, REF-6, renamed pattern binder and versioned diagnostics are enforced (see the [2i row](../archive/ROADMAP_LEGACY_PLAN.md#phase-1-exits) for what remains).

Normative paragraphs in `docs/language/` for block scoping
and shadowing, match-binding positions and names, statement vs value position, sequential
assignment within a loop iteration, literal typing and range (signed minima preserved), cast
semantics per width pair, evaluation order, short-circuiting, and second-class references; each
with accepted/rejected examples and fixtures.

**Exit before advancing past queue row 2:** every rule has a normative paragraph, examples, cited
fixtures and S1 generator coverage meeting its minima. S7's exit later adds links from each
paragraph to its semantic clause.

**Semantics decisions (2026-10-21):** the rules this task enforces are now written in their
canonical homes ([Decisions](roadmap-2026-11/ROADMAP_DECISIONS.md#decisions)): references and scoping in MEMORY_SEMANTICS §15 (REF-6
rewritten: no `let`-bound reference, no assignment to a reference binding, E0302 / E0303, enforced since 2026-10-21;
constants are values; SCOPE-1 staged behind the renamed pattern binder), evaluation order and
assignment in STATEMENT_EXPRESSION_MODEL and MEMORY_SEMANTICS §10 (exit 2j), and casts in
ARITHMETIC_POLICY §8 (exit 2k: the lossless class table, the three named-operation families and the
three stages). Each records its status on main and its open gaps.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0435** (superseded): superseded by R-0500: merge migration, renamed-pattern syntax and controls. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0435)


<a id="coherent-plan-r-0501"></a>

### Historical task R-0501

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: Carving jobs out of old Lower and shadow comparison retire, but pass semantics and cleanup preservation remain explicit exits. surviving requirement: Build proved new lowering passes with executable semantics for sequencing, matching, cleanup and check expansion, retaining differential and early-exit cleanup controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep by slice.

**Objective:** Place MIR — stage S10 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

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

- **R-0079** (superseded): superseded by R-0493/R-0501/03: merge into typed operations and pass obligations. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0079)
- **R-0082** (superseded): superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0082)
- **R-0295** (superseded): Superseded by R-0501/S10: retire workload gate; MIR is already an architectural decision.


<a id="coherent-plan-r-0502"></a>

### Historical task R-0502

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The existing-profile layout repair retires, while the body places shared memory design before implementation and requires representation proofs. surviving requirement: Specify the shared value/raw-memory boundary and one layout authority, then deliver checked representations and memory lowering with allocation, alignment, store and memory-injection proofs. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; early design obligations precede full implementation.

**Objective:** Memory model, layout and Memory CFG — stage S11 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18; amended 2026-10-21): planned.** Queue row 4 carries a whole-language checkpoint
for the existing profile: one `Layout` authority supplies every size, alignment, offset and enum
representation the existing lowering and LLVM emission use, with a gate comparing it with the emitted
types (structural class 12). Representation certificates, Memory CFG and the proofs grow per slice.

Owns the shared raw-memory specification with S7, block–offset/provenance memory, `Layout`, representation certificates and Place MIR → Memory CFG lowering.

**Semantics decisions (2026-10-21):** layout is unspecified unless a `repr` guarantees it, each `repr`
guarantees only its documented properties, code relies on nothing but `sizeof`, `alignof` and array
stride = `sizeof`, and one `Layout` owns every fact; `Option` and `Result` are laid out per
instantiation like every generic enum ([LAYOUT_CONTRACT §3.3 – §3.4](../archive/design-records/LAYOUT_CONTRACT.md)).
Bug 122's per-instantiation repair is not deferred to this task; it lands under exit 0b.

**Exit:** Memory CFG has executable semantics and a verifier; `Layout` emits checked
representation certificates; Place MIR → Memory CFG comparisons cover related values and stores,
traces, traps, allocation and alignment cases; a representative memory-injection lemma is proved.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0076** (superseded): superseded by R-0502: merge into single proved Layout authority. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0076)
- **R-0085** (merged): duplicate of R-0076: merge through R-0076 into R-0502.
- **R-0088** (superseded): superseded — aggregate/memory ABI realization moves to R-0502 (layout) and the adapter boundary (R-0505/R-0517) across V2–V6.
- **R-0434** (superseded): superseded by R-0502: merge into layout certificates and Memory CFG verification. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0434)


<a id="coherent-plan-r-0503"></a>

### Historical task R-0503

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: EmitBuiltins migration and old Lower retirement disappear, but validated BIR, runtime semantics and honest profile labels survive. surviving requirement: Emit the new compiler’s explicitly unverified LLVM path from validated BIR with executable semantics, runtime contracts, support labels and differential qualification. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; absorb R-0278/R-0288.

**Objective:** BIR, the LLVM adapter and the rt module — stage S11 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned.**

Depends on R-0502. Owns Memory CFG → BIR, BIR semantics and verification, LLVM adapter legalization, the `rt` migration, generated backend contracts and replacement-path qualification.

**Exit (depends on R-0502):** LLVM is emitted only from validated BIR, under the adapter
contract above; BIR semantics and differential gates run; `EmitBuiltins.lean`'s LLVM-AST helpers
are replaced by `rt`; profile labels are emitted; the old `Lower` is retired only after the complete
replacement path meets its gates.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0073** (superseded): superseded by R-0497/R-0503: merge into payload-bound validated BIR artifacts.
- **R-0074** (superseded): superseded — merge into validated BIR (R-0503) and R-0517; the closed `llvm`/`qbe` backend set becomes the LLVM and LCB-native profiles. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0074)
- **R-0077** (superseded): superseded by R-0503/R-0509: merge with `rt` contracts, support inventory and trust accounting.
- **R-0079** (superseded): superseded by R-0493/R-0501/03: merge into typed operations and pass obligations. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0079)
- **R-0082** (superseded): superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0082)
- **R-0090** (superseded): superseded by R-0503: retire preferred-C-runtime prescription; implement Concrete `rt`, named libc FFI.
- **R-0109** (superseded): superseded by R-0503/R-0146: merge; remove assumption helpers must be C.
- **R-0278** (superseded): Superseded by R-0503/S11: merge into R-0503; BIR replaces this architecture.
- **R-0288** (superseded): Superseded by R-0503/S11: merge inspection, verification and parity requirements into R-0503. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0288)


<a id="coherent-plan-r-0504"></a>

### Historical task R-0504

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old SSA and BackendIR paths retire, while the exit preserves validator soundness and substitution controls. surviving requirement: Give new validators explicit soundness propositions and reject semantic substitutions, unsupported certificates and wrong retained LLVM artifacts without inflating native assurance. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; retain R-0299/R-0300’s distinct assurance scopes.

**Objective:** Validators — stage S12 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

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

- **R-0105** (superseded): superseded by R-0491/R-0504: merge debug/release and sanitizer controls. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0105)


<a id="coherent-plan-r-0506"></a>

### Historical task R-0506

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: Maintenance of today’s authoritative evidence retires, but stated obligations and axiom policy begin before pass proofs. surviving requirement: State each new proof boundary and validator obligation with explicit premises and status, enforce transitive axiom classification and totality, and prove representative lemmas before freezing boundaries. This line governs over older implementation directions for the legacy compiler.

**Objective:** Stated obligations and totality — stage S12a of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

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
- **R-0260** (superseded): superseded by R-0506/S12a: merge into pass/validator obligation ledger; preserve legacy historical view. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0260)
- **R-0276** (superseded): superseded by R-0506/R-0496: retire general legacy rewrite; retain migration-needed wrappers only. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0276)


<a id="coherent-plan-r-0508"></a>

### Historical task R-0508

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The compiler theorem survives, while the old VC-generation implementation is replaced by proofs over new Core functional models. surviving requirement: State and progressively prove the new compiler’s maximal-behaviour refinement, independent frontend correspondence, resource adequacy and exact-image claim transport with explicit residual assumptions. This line governs over older implementation directions for the legacy compiler.

**Objective:** Compiler correctness and proof transport — stage S15 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

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


<a id="coherent-plan-r-0509"></a>

### Historical task R-0509

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The body names old ProofCore controls but explicitly starts the trusted inventory immediately and requires a minimal independent consumer. surviving requirement: Maintain the new compiler’s explicit trusted-base inventory and prove evidence binding and interpretation soundness, retaining independent-kernel replay and hostile consumer controls from the first authoritative claims. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; inventory starts now, minimal consumer at V1. The trusted list is non-increasing for a fixed supported scope; additions for newly admitted scope are recorded and reviewed separately. A separate compiler/checker execution-trust reduction plan with milestones is recorded here; R-0413 explores only the Concrete-hosted path.

**Objective:** Trusted base and independent evidence check — stage S16 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

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

**The trusted-base inventory (listed 2026-10-25).** Each entry is trusted until its mitigation
removes or narrows it; the published inventory carries the same rows with their current state.

| trusted | why it is trusted | mitigation | owner |
|---|---|---|---|
| Lean's code generator and runtime | the Concrete compiler, its checkers and validators run as code Lean compiled, and the kernel never sees that code | per-compilation certificates checked in a kernel, so a checked artifact does not depend on the binary that produced it; development builds keep trusting the binary | R-0552 |
| the C compiler that builds Lean's output | Lean's generated C and its runtime are compiled by a C compiler, and its output is run, not checked | the same certificates (R-0552); until then it is pinned with the Lean toolchain (`lean-toolchain`, the Nix build) | R-0552 |
| `@[implemented_by]` and `@[extern]` in proved code | the kernel reasons about the stated definition while execution runs the replacement | an inventory of every such attribute reachable from proved code, each with a stated equivalence or listed as trusted; `check_proofcore_callable_identity.sh` already refuses an `@[implemented_by]` or `@[csimp]` replacement on the ProofCore wrappers it pins | R-0509, with R-0506 |
| native-trust axioms (`bv_decide`, `native_decide`) | they rest on compiled evaluation (`Lean.ofReduceBool`), not on the kernel alone | every use registered and labelled (`check_axiom_inventory.sh`), never an unqualified kernel-only verdict; replace a use with a kernel proof where the cost allows | R-0506 (exit 2g) |
| the Lean kernel | every proof is accepted by it | independent re-checking by nanoda and con-leche (`check_kernel_recheck.sh`, `check_multi_kernel.sh`) | R-0509 |
| the assembler and linker (the QBE path) | LCB emits assembly text that an external assembler and linker turn into an image | LCB's V04/V05 milestones (encoding and composition) remove them; until then they are named premises of the native theorem, and R-0505 checks the final image | R-0517, R-0505 |
| the machine (ISA) model | the native theorem is stated against a model of AArch64, not the hardware | a named premise of every native claim; the model is LCB's and is differentially tested against the hardware | R-0517 |
| the semantics' fidelity to intent | a proof shows the compiler matches the formal semantics, not that the semantics says what the language means | an outside expert review of Core's V1 semantics, source judgments specified independently of the implementation (V1c), and differential testing of the semantics against the interpreter and the compiled output | R-0511 |
| libc, the OS, FFI and `trusted` code | their behaviour is assumed through external-call contracts and audited trusted code, not proved | stated contracts for every foreign call; `std`'s trusted implementations verified against contracts or listed with audit, and the list only shrinks; the trusted-code obligations formalized in V6 | R-0509, R-0514 |

For the LLVM profile, the BIR → LLVM IR printer, LLVM and the system toolchain are also trusted (R-0505). Whether a module marker
or a block can confer trust is an open owner decision (OD7, OD22); the inventory lists today's
behaviour and does not settle it.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0077** (superseded): superseded by R-0503/R-0509: merge with `rt` contracts, support inventory and trust accounting.
- **R-0211** (superseded): superseded by R-0509/S16: merge into R-0509; retain audit command acceptance. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0211)
- **R-0230** (superseded): superseded by R-0509/S16: merge into R-0509 and slice profiles.
- **R-0236** (superseded): superseded by R-0506/R-0509: merge axiom classification and inventory.
- **R-0264** (superseded): superseded by R-0494/R-0509: merge implementation into R-0494, soundness proof into S16.
- **R-0265** (superseded): superseded by R-0495/R-0509: merge into statement/binding/aggregation soundness.
- **R-0266** (superseded): superseded by R-0509/S16: merge; distinguish trusted assumptions from ordinary proof dependencies. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0266)


<a id="coherent-plan-r-0511"></a>

### Historical task R-0511

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: Old V1 staging and today’s evidence path retire, while design proofs, target regressions and minimal claim consumers survive. surviving requirement: Review representative Core design proofs before interface freeze and deliver cumulative new-compiler slices with honest theorem scopes, retained target programs, exact artifact binding and independent checking. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Proof-library engineering (refinement, clock, memory-injection, substitution and ownership libraries) has bounded deliverables recorded with each V1 increment.

**Objective:** The first cumulative verified releases — a proved path from checked Core to a native
image for a small language, carried through every stage of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices).

**Status (2026-10-19; amended 2026-10-20 and 2026-10-21): planned; queue row 3.** The design examples and isolated proof development start now; V1a, V1b and V1c are accepted only when their [prerequisite exits](../archive/ROADMAP_LEGACY_PLAN.md#feature-prerequisites) hold, together with the subset obligations below. V1c names the restricted-source correspondence below as its own deliverable (2026-10-21).

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
[plan](../compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices) defines it, under
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

- **R-0083** (retired): retired — there is no Concrete QBE backend to select; the no-silent-fallback rule moves to R-0511/R-0517. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0083)
- **R-0086** (superseded): superseded — its semantic vectors become adapter test vectors for V1 (R-0511/R-0517); later widths, casts, memory and floats follow the V-slices. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0086)
- **R-0097** (superseded): superseded by R-0511/R-0512: merge profile admission; retain no silent fallback. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0097)
- **R-0111** (superseded): superseded by R-0511/R-0137: merge reusable exits; retire required QBE showcase. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0111)
- **R-0231** (superseded): superseded by R-0511/R-0512: merge into each slice’s admission controls.
- **R-0238** (superseded): superseded by R-0511/R-0512: merge into cumulative slice matrices; retain orthogonal policies.
- **R-0263** (superseded): superseded by R-0511/R-0512: merge into V1–V7 slice ordering.
- **R-0277** (superseded): superseded by R-0511/R-0512: merge into each slice’s cumulative acceptance.


<a id="coherent-plan-r-0514"></a>

### Historical task R-0514

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old whole-language shadow rollout retires, but the body’s checked-program identity and rejection regressions remain essential. surviving requirement: Make the new compiler consume the exact identity-resolved Core payload checked against independent judgments, expanding by slice while retaining checker, capability and artifact-substitution controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; broader proof coverage remains Phase 4.

**Objective:** Make the shared front end sound for every user now: identities from Resolve on and one
authoritative checker over Core for the whole language, consumed by both the existing lowering and the
`verified` profile.

**Status (2026-10-19; amended 2026-10-20): planned; queue row 4, alongside R-0511.** The parts V1a's subset exercises proceed with V1a now and are accepted with it once its [prerequisite exits](../archive/ROADMAP_LEGACY_PLAN.md#feature-prerequisites) hold; the whole-language rollout starts once row 0's in-flight candidates (batch 2, type-identity, generic-destroy) are accepted on main, implementing the rules as exits 2i – 2k state them, without waiting for row 2's other exits (amended 2026-10-21, [priority decision](roadmap-2026-11/ROADMAP_DECISIONS.md#decisions)). Neither task waits for the other's complete exit; both use the same identities, Core representation, checker implementation and checked-artifact interfaces. The source-tree checker returns `Unit`
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

- **R-0116** (superseded): superseded by R-0497/R-0510/R-0514: merge; use payload binding and new IR chain. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0116)
- **R-0274** (superseded): superseded by R-0499/R-0514: retire source-checker/CompilerDB authority prescription. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0274)
- **R-0437** (superseded): superseded by R-0514/R-0510: merge declaration and specialization checks; preserve distinct diagnostics. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0437)


<a id="coherent-plan-r-0553"></a>

### Historical task R-0553

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: Converting the old 664-definition baseline retires, while proof visibility and executed-definition correspondence remain required. surviving requirement: Require new semantics, checkers, validators and proved passes to be total and executed as proved, with an environment inventory and controls for opaque or replacement execution gaps. This line governs over older implementation directions for the legacy compiler.

**Objective:** Compiler totality: every pipeline function provable (no `partial`, `unsafe` or opaque
definitions). A proof about a compiler pass cannot see inside a `partial` function: Lean compiles it
to an opaque constant, so a theorem about the pass can only treat it as an unknown. The same holds for
`unsafe` and `opaque` declarations. `@[implemented_by]` and `@[extern]` are a different case: the
definition keeps its Lean body, so proofs can unfold it, but the compiled compiler runs the replacement
instead, so a theorem about the Lean body says nothing about the executed code until the replacement's
correspondence is checked or the attribute is removed. The inventory tracks all five so that both gaps
(no proof possible, proof about code that does not run) only shrink. Proving the verified-pipeline
passes (R-0491 – R-0510, queue rows 3, 4 and 6) therefore needs the functions those passes run to be
total and executed as proved first, which is why this task sits ahead of them in the queue (row 2,
exit 2p).

**Scope and owner.** This task builds the mechanism behind the totality gate that R-0506 (stage S12a)
owns: R-0506 states which code must be total (semantics, checkers, validators and proved passes) and
keeps its exit; R-0553 supplies the environment inventory, the ratchet and the conversions. As the
[verified-compiler plan](../compiler/VERIFIED_PIPELINE.md) allows, untrusted search (heuristics,
elaboration search, register-allocation search) may stay partial: its result is checked by a total
validator, and its baseline row records that reason instead of being converted.

**Queue entry.** It enters under the entry rule as an explicit dependency of V1a, the next proof
deliverable: V1a proves checked Core → BIR refinement for the passes its subset runs, and that proof
cannot be completed while those passes' functions are `partial`. Making V1a's own stages total is part
of R-0511's V1a work and is supplied by step 2 below; step 1, the ratchet that exit 2p closes on, is a
prevention gate that no deliverable's acceptance waits for, so exit 2p continues alongside.

**Depends on:** nothing. **Blocks:** the pass proofs of R-0491 – R-0510 for each stage it has not yet
made total.

1. **Step 1, the ratchet gate, has landed** (675871b4; see CHANGELOG.md):
   `scripts/tests/check_totality_ratchet.sh` inventories, from the Lean environment, every `partial`,
   `unsafe`, `opaque`, `implemented_by` or `extern` declaration in the `Concrete.*` modules against
   the committed baseline `docs/compiler/totality_baseline.tsv`, in CI and in the pre-push hook, so the
   count only goes down. What remains is steps 2 and 3: converting the declarations the baseline lists
   (664 `partial` definitions when step 1 landed; the baseline file is the current count).
2. **Step 2: convert the baseline's `partial` definitions in batches.** Batches go in pipeline-stage
   order, closest-to-proven stage first, guided by the per-stage inventory
   `research/compiler/partial-def-inventory-2026-10.md`. Each definition falls in one of three classes:
   - *structural*: the recursion is already structural, and the change is often only deleting the
     keyword;
   - *measure-based*: the parser, worklists and fixpoints, given a `termination_by` measure and its
     decreasing proof;
   - *fuel*: the interpreter, which runs programs that may diverge, takes an explicit fuel argument
     and reports running out of fuel as its own outcome.
   Each batch removes its rows from the baseline in the same commit, and the ratchet gate checks that.
   For an `implemented_by` or `extern` row, the batch removes the attribute, or records why the
   replacement must stay and what checks its agreement with the Lean body.
3. **Exit:** for the stages the verified pipeline proves (R-0491 – R-0510), the baseline holds no
   semantics, checker, validator or proved-pass declaration; the only rows left in those stages are
   untrusted search whose result a total validator checks, each with its reason. From then on the same
   gate refuses outright any other non-total declaration in those stages, in CI and in the pre-push
   hook. Later, under this task, the same requirement extends to the whole compiler: every remaining
   row is converted or is genuine untrusted search with its reason.

Exit 2p closed on step 1; steps 2 and 3 continue through Phases 2 and 4, stage by stage ahead of the
pass proofs that need them.


<a id="coherent-plan-r-0554"></a>

### Historical task R-0554

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The cleanup list mixes surviving repository controls with old Report splitting, ProofCore overlap and string-identity repairs. surviving requirement: Keep unique specification and document authority, manifest-driven tests, drift and dead-code controls, generated-artifact discipline and branch hygiene, applying structural boundaries to the new compiler as components appear. This line governs over older implementation directions for the legacy compiler.

**Objective:** Repository cleanup: one owner per fact, document and test, so the repository says each
thing once and the gates read manifests instead of hand-kept lists (the repository reorganization plan
approved 2026-07-28). It is queued in row 2 after R-0553 and has no Phase 1 exit of its own: its
sub-items continue alongside the queue, each landing on its own branch under the normal promotion
pipeline, and none may choose, encode or implement a side of an [owner decision](../spec/decisions.md#owner-decisions).
Completed sub-items are recorded only in [CHANGELOG.md](../../CHANGELOG.md): the ROADMAP compaction
(sub-item 3), the grep benchmark data out of git (part of 12), the grammar three-way cross-check (13)
and the compile-scaling gate's fix (15, bug 315) have landed. The list below keeps what remains, with
its owner and branch.

**Queue entry and capacity.** The task names no reproducer and no dependency of RocksDB slice 1 or
V1a, so the queue's entry rule alone would send it to the backlog. It is in row 2 because the project
owner asked, on 2026-10-28, for the whole cleanup programme to be queued after R-0553; that request is
its admission, recorded here as an explicit exception like the development-process exception. Its
integration effort is charged to track C (adversarial validation and gate health) of the
[capacity allocation](../archive/ROADMAP_LEGACY_PLAN.md#capacity-allocation), since most sub-items consolidate gates,
tests and the documents gates read, and it never preempts a defect repair or weakens an exit.

Owners: the cleanup agent (branches `wip/cleanup-*`) and the spec agent (`wip/spec`).

1. One normative language specification, `docs/spec/` (kept as chapters rather than one SPEC.md file,
   so no rule is stated twice), in progress: the S12 and S13 gates and most absorptions have landed;
   the remaining `docs/language/*.md` files (listed in `docs/spec/unabsorbed.tsv`) are absorbed, then
   deleted. Its full target is [R-0555](#coherent-plan-r-0555). Owner: the spec agent, `wip/spec`.
2. The book's language chapters (`docs/book/src/language/`) teach by example and link to SPEC rule IDs
   instead of restating rules. Owner: the spec agent, `wip/spec`.
4. Consolidate `docs/project/` (36 documents) into `PROJECT.md` plus the contributor documents. Owner:
   the cleanup agent, `wip/cleanup-*`.
5. Consolidate `docs/verification/` (59 documents) into one normative verification specification plus
   its rationale. Owner: the cleanup agent, `wip/cleanup-*`.
6. Move `docs/NOTES/` into `research/`. Owner: the cleanup agent, `wip/cleanup-*`.
7. Gate and test manifest: about 284 `check_*` gates; `run_tests.sh` (686 KB), the CI workflow (2,556
   lines) and the Makefile become manifest-driven; duplicate gates merge; a new `tests/` taxonomy.
   Owner: the cleanup agent, `wip/cleanup-*`.
8. Split the report code (`Report.lean`, 6.5k lines, and `ReportObligations`). Owner: the cleanup
   agent, `wip/cleanup-*`.
9. Proof-module overlap audit (`Proof.lean` against `ProofCore.lean`), research only. Owner: the
   cleanup agent, `wip/cleanup-*`.
10. Replace string-encoded identities (184 `== "literal"` comparisons). Owner: the cleanup agent,
    `wip/cleanup-*`.
11. A dead-code finder with a ratchet gate. Owner: the cleanup agent, `wip/cleanup-*`.
12. Generated artifacts out of git: `concrete.wasm` is no longer committed; the site workflow
    builds it from the published commit (`scripts/browser/build-site-compiler.sh`). Owner: the
    cleanup agent, `wip/cleanup-*`.
14. Branch hygiene: merged remote branches are deleted (192 on 2026-10-28), and merged and dead
    worktrees are pruned periodically. Owner: the cleanup agent.


<a id="coherent-plan-r-0555"></a>

### Historical task R-0555

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The body’s old semantic modules and prose-link workflow change, but complete mechanization and conformance obligations survive. surviving requirement: Complete audit disposition and maintain one Lean-generated normative specification with independent judgments, rule-indexed conformance, validated bug-rule references and chapter acceptance artifacts through the language slices. This line governs over older implementation directions for the legacy compiler.

**Objective:** Language spec: the WebAssembly model. `docs/spec/` becomes a specification in the sense
of the WebAssembly specification: every rule is mechanized against a formal definition, the language
is covered completely, a conformance suite indexed by rule ID runs on any implementation, and the
spec decides what is a bug. It continues R-0554 sub-item 1 and is placed ahead of the
verified-pipeline proof rows it serves (R-0496, R-0499): those proofs are stated against the rules
this task makes exact. No workstream may choose, encode or implement a side of an
[owner decision](../spec/decisions.md#owner-decisions); a question one of them surfaces becomes an `open`
entry (`docs/spec/decisions.md`) for the owner. Owner: the spec agent, `wip/spec`.

**Queue entry.** Queue row 0s, the first row after row 0, approved by the project owner on
2026-10-29 as an explicit exception to the entry rule, like R-0554. It is charged to track A and
shares the active proof slot (not a third integration slot); it proceeds continuously and is paused
only for row-0 soundness repairs, and it never preempts tracks B or C. It is not a predecessor of
rows 1 or 2. Each chapter closes independently: its prose absorbed, then its accept and reject
examples under rule-ID fixtures, then its Lean definition and `Semantics:` link landed together.
Absorbing the remaining `docs/language` files and the archived language-scope text completes before
exit 2q's migration starts. The chapter gates each deliverable waits for are under the ROADMAP's
feature prerequisites; the spec does not gate R-0496 as a whole (R-0496 lands each Core definition
with its chapter's links, R-0499 implements the answered rules construct by construct and refuses
anything unspecified, and the soundness proofs wait for each link).

(a) **Mechanized.** Every entry names the Lean definition of the rule (`Semantics:`), and the spec is
checked against that formal definition, not only against the implementation. The link becomes
required chapter by chapter (check_spec.sh S7), so a linked chapter never regresses. It starts with
the definitions that already exist in `Concrete/Semantics/`: `IntArith` (the ARITH chapter),
`TypeJudgment` (the TYPE typing rules), `RefWellFormed` (REF), `BindingMode` (pattern binder modes),
`Capabilities` and `TrustEdges` (TRUST and capabilities). Exit: every chapter has a required link.

(b) **Complete.** Every `docs/language/*.md` file is absorbed (rules and rationale into the spec,
tracking material to ROADMAP, KNOWN_HOLES or research, the file deleted; S13), and so is the archived
language-scope text (`docs/archive/roadmap-language-scope.md`). The areas the spec covers thinly get
chapters: expression evaluation (the dynamic semantics beyond statement outcomes), generics and
monomorphization, lexical structure, the built-ins the compiler treats specially, and abort and
failure semantics. Exit: `docs/language/` holds no document, and each listed area has a chapter whose
refusals all have reject examples.

(c) **Conformance suite.** Accept and reject cases indexed by rule ID, runnable by any implementation
(today the native compiler and `--interp`), in the style of the WebAssembly spec tests. The suite is
the existing rule-ID fixture index, not a second corpus. Exit: the gate reports every rule that forbids something and has no reject
case, and that list is empty or each entry is an `open` entry.

(d) **The spec decides what is a bug** (owned by a separate agent, not the spec agent; new bugs cite
rule IDs now, and older records are backfilled inside the chapter that absorbs their rule, which
blocks no deliverable). Every bug record names the rule it violates
(`Violates: <RULE-ID>@<version>`), or records `Spec gap` (no rule decides the behaviour: the fix starts
by writing the rule, filed as spec work) or `Open: OPEN-n` (the behaviour waits on an owner question:
the fix is parked until the owner decides, as bugs 305 – 307 and 311 are). A not-a-bug verdict cites
the rule that permits the behaviour ("permitted by RULE-ID", as the class-13 audit's G5 did). This is
enforced, not a convention: `scripts/gen/new_bug.sh` requires one of `--violates RULE-ID`,
`--spec-gap` or `--open OPEN-n`; the bug ledger and dashboard count bugs by the rule violated and by
spec gap and open; a gate checks that every cited rule ID resolves in `docs/spec/ids.tsv`. The open
records and the 2026-10 records are backfilled; older records may say `Violates: unrecorded`, and the
share of those only shrinks. Exit: the gate holds on every record, and no record filed after the gate
lands says `unrecorded`.

Measured state (2026-10-29, after the owner decided OD1 – OD32): 0 of 161 entries link a
Lean definition; 138 of 161 have `Evidence: tested`; 18 open entries (OD33 to OD46 and OD48 to OD51, raised while
recording those answers). `docs/spec/MAINTAINERS.md` ("Path to the complete spec") keeps the current figures.


<a id="coherent-plan-r-0481"></a>

### Historical task R-0481

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The old module relocation and TermIR promotion retire, but the completion gates retain corpus, authority and gate-integrity requirements. surviving requirement: Preserve unique temporal authority, canonical generated inventories, shared non-vacuous assertions and mutation registration, and enforce producer/consumer boundaries as new compiler and evidence components are built. This line governs over older implementation directions for the legacy compiler.

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

Migrate dated roadmap narratives and bug write-ups selectively, not mechanically. (The four
`docs/NOTES/` notes already moved to `research/proof-evidence/` on 2026-10-29; they are research
records, not journal entries.)
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

- **R-0447** (merged): duplicate of R-0481: merge unique operational controls; retire conflicting directory blueprint. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0447)
- **R-0478** (merged): duplicate of R-0481 / R-0474: merge each half into its actual owner. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0478)


<a id="coherent-plan-r-0479"></a>

### Historical task R-0479

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The remaining survey targets old Report defaults, but the body states a general fact and refusal preservation invariant. surviving requirement: Represent unavailable or failed computations without fabricating facts, and preserve refusals through typed results, text, JSON and exit codes with production controls. This line governs over older implementation directions for the legacy compiler.

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


<a id="coherent-plan-r-0471"></a>

### Historical task R-0471

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old query/report and resolver repairs retire, but the body specifies diagnostic-legibility regressions beyond those sites. surviving requirement: Make new diagnostics distinguish invalid command forms, identify retired syntax and provide valid next actions or candidates, retaining class-level and blank-hint mutation controls. This line governs over older implementation directions for the legacy compiler.

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


<a id="coherent-plan-r-0009"></a>

### Historical task R-0009

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old reducer defect retires, while the body defines reusable crash-predicate correctness requirements. surviving requirement: Provide crash reduction only with isolated bounded execution, preserved failure class, final-predicate validation and healthy-program, timeout and self-failure controls. This line governs over older implementation directions for the legacy compiler.

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


<a id="coherent-plan-r-0440"></a>

### Historical task R-0440

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old evidence migration and PExpr callable prescription retire, while the body’s multidimensional assurance requirement applies to initial claims. surviving requirement: Represent claim scope, method, validity, binding, dependencies and policy independently, retaining typed digest separation and controls against scope inflation or trust laundering. This line governs over older implementation directions for the legacy compiler.

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


<a id="coherent-plan-r-0438"></a>

### Historical task R-0438

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The body combines old constructor inventories and interpreter repairs with enduring coverage and claim-integrity duties. surviving requirement: Generate construct coverage and validate structured documentation claims against live gates, retaining missing-row and stale-claim negative controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Merge enforcement rows into R-0498; share inventory with R-0118/V7.

**Objective:** Generate the semantically-dark-construct inventory from compiler
constructors, and make the docs-drift gate check claims rather than paths.
Also expand the gate's coverage: `PRESENT_DOCS` currently lists five files,
while `docs/NOTES/` and `research/` accumulate claim-bearing design notes
outside any drift check — bring both under the path check at minimum.
(2026-10-29: `docs/NOTES/` no longer exists; its notes moved to `research/proof-evidence/`,
which stays outside the gate by the convention in `research/README.md`. What remains here is
the claim check over gated `docs/`.)

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
`DEFER.md`, `LANGUAGE_GAPS.md`, `IDENTITY.md` (now the identity section of `PROJECT.md`), error-convention, and anti-feature
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


<a id="coherent-plan-r-0075"></a>

### Historical task R-0075

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The Main.lean consolidation is legacy-specific, but its operational and inherited release/test controls survive. surviving requirement: Use a shared native compilation driver with structured diagnostics, retained artifacts, bounded tools and failure-injection controls at every boundary. This line governs over older implementation directions for the legacy compiler.

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
- **R-0107** (merged): duplicate of R-0075: merge bounded external-tool execution contract. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0107)
- **R-0110** (merged): merged — profile naming and evidence attribution join R-0075/R-0333; the profiles are `llvm` and the LCB-native `verified` profile. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0110)


<a id="coherent-plan-r-0115"></a>

### Historical task R-0115

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: Old/new derivation cutover retires, while canonical facts and their integrity requirements remain. surviving requirement: Derive immutable indexed canonical program facts with typed keys, explicit invalidation and duplicate, missing-key and conflicting-consumer controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep canonical facts; share indexes with R-0140.

**Objective:** Build one immutable indexed ProgramFacts store Derive module/definition indexes, signatures, layouts, builtin identities, aliases, target/runtime facts, call relationships, and source/semantic identity once per program. Passes query typed keys rather than rescanning lists or building private first-match tables. Facts are immutable within a compilation;

transformations produce a new version with an explicit invalidation boundary.

Acceptance: compare old and new derivations over the corpus before cutover,
fail on duplicate/missing keys, record deterministic hashes and construction
cost, demonstrate linear-time builders, and gate that two consumers cannot
derive conflicting values for the same fact.


<a id="coherent-plan-r-0118"></a>

### Historical task R-0118

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The named legacy passes retire, but self-enforcing coverage and inherited differential controls survive. surviving requirement: Generate exhaustive new-IR pass and consumer coverage with implementation/test links and missing-row or synthetic-constructor negative controls. This line governs over older implementation directions for the legacy compiler.

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


<a id="coherent-plan-r-0222"></a>

### Historical task R-0222

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The evidence requirement survives while the old wrapping_add-to-addw coverage snapshot does not. surviving requirement: Arithmetic-site facts must distinguish checked, wrapping and saturating semantics across new-compiler proofs and outputs, with profile-invariance and confusion-rejection controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; replace obsolete operator-coverage snapshot.

**Objective:** Carry arithmetic-site facts into diagnostics, reports, assumptions, proof obligations, release bundles, and backend contracts. A theorem about checked arithmetic is not a theorem about modular arithmetic; a function using `wrapping_*` is either proved against the proof model's explicit modular operator (today only `wrapping_add@u32 → addw 32`, as SHA-256 uses) or, for the not-yet-modeled forms (`wrapping_sub`/`wrapping_mul`, other widths, saturating), outside the default proof subset. Add `docs/ARITHMETIC_SITE_EVIDENCE.md`, `examples/arithmetic_site_evidence/{checked,wrapping,saturating,profile_invariant}/`,

and `scripts/tests/check_arithmetic_site_evidence.sh`; the gate must prove
profile-invariance (the same expression has the same semantics under
different build profiles), explicit classification of every arithmetic site,
and rejection/downgrade of proof claims that confuse checked and wrapping
semantics.


<a id="coherent-plan-r-0272"></a>

### Historical task R-0272

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The body combines a dead legacy reference-return lowering repair with explicit surviving test obligations. surviving requirement: Retain differential generation, trap-identity checks, nightly minimization, alpha and hostile-spelling controls, historical witnesses and accepted raw-pointer-return regressions as new-compiler slices admit them. This line governs over older implementation directions for the legacy compiler.

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


<a id="coherent-plan-r-0302"></a>

### Historical task R-0302

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The body mixes essential version-drift control with legacy emitter references and a deferred bitcode option. surviving requirement: Pin the new compiler's supported LLVM toolchain and require wrong-code, contract and output controls on upgrades, retaining bitcode emission as conditional work. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; connect candidate identity to R-0357.

**Objective:** Pin the supported LLVM/clang version and gate version drift. The backend today targets whatever `clang` defaults to on the host (`docs/platform/EXECUTION_MODEL.md`), yet the TCB already conditions backend correctness on "LLVM version" — so a floating toolchain leaves the trusted boundary a moving, un-audited target and makes native output non-reproducible across hosts (`docs/compiler/DETERMINISM.md` already disclaims binary reproducibility for exactly this reason). Declare one supported LLVM version (or a narrow tested range) as part of the backend contract, and make an upgrade a deliberate, gated event: on a version bump the wrong-code corpus, the backend-contract gate, and IR golden tests must re-pass before the new version is

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


<a id="coherent-plan-r-0421"></a>

### Historical task R-0421

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The obligation survives, but its Concrete SSA, old backend IR and QBE ownership map is obsolete. surviving requirement: Define each new-compiler optimization's owner, semantic preconditions, identity policy, validation controls and performance witness without changing observable behavior or evidence. This line governs over older implementation directions for the legacy compiler.

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


<a id="coherent-plan-r-0422"></a>

### Historical task R-0422

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The body mixes legacy quadratic-emitter repairs with lasting class-level scaling controls. surviving requirement: Use profiling and adversarial scaling inputs to prevent superlinear compiler work in the new pipeline with explicit complexity and memory budgets. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; do not equate throughput with program speed.

**Objective:** Close measured compiler and emitter performance defects under one
owned queue, beginning with bug 027's quadratic emission class.

Profile before refactoring; retain adversarial scaling inputs; measure time and
peak memory across increasing program sizes; require an explicit expected
complexity envelope; and gate the class rather than one file. Typed emitter
fragments may be adopted when the profile pulls them, but QBE's fragment model
does not by itself prove LLVM emission fixed. Keep compiler-throughput facts
separate from generated-program runtime facts in reports and release claims. The class's scaling
gate is R-0550 (Phase 1 exit 2o), and its continuous profiling is R-0551.


<a id="coherent-plan-r-0550"></a>

### Historical task R-0550

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The body combines repairs of frozen EmitSSA, Lower and frontend defects with a reusable load-bearing scaling gate. surviving requirement: Retain the scaling corpus and controlled complexity, instruction, allocation and host-normalized timing gates for new-compiler stages as their constructs land. This line governs over older implementation directions for the legacy compiler.

**Objective:** A compile-time scaling gate for structural class 17 (pass state in association lists
and per-query recomputation), owned with R-0422, which repairs the class. It closes Phase 1 exit 2o.

It enters the queue under the entry rule with these reproducers: bug 027 (EmitSSA's quadratic
rendering), bugs 248, 249 and 250, and the long-function front-end slowdown (forty thousand
statements: 8.5 s parse, 13.4 s resolve, 9.5 s check and 9.5 s elab; being filed). leanprof profiles
add two hot spots in an `examples/lox --emit-llvm` build: EmitSSA's scan for generic arguments (13.6%
of the build) and a linear search of the pointer-register list (6.8%).

The gate generates inputs of 10^4 to 10^5 elements or statements in each shape (array literals, long
functions, many locals, many declarations, deep nesting), compiles them, and times each front-end and
back-end stage. It fails when a stage exceeds its recorded budget, or when the ratio between two sizes
is above the recorded near-linear bound. The budgets are measured on the CI host and recorded with the
SHA they were measured at. A retry budget keeps host noise from failing the gate; it never lets the
pre-fix code pass. Since bug 294 the gate separates the compiler's work from the host's speed: where
the CI host can count retired instructions (perf), each run's instruction count and each stage's
allocations are held to exact budgets, growth between sizes is judged on the instruction counts
against the linear expectation (a wall-time ratio between 10^4 and 10^5 measures the caches as much
as the work: linear stages grew x45 – x60, close to the bound a quadratic had to exceed), and wall
time is judged only against budgets normalized to the host speed measured at `--measure` (capped
at 2x); a host that cannot count keeps the wall bounds. **Exit:** `scripts/tests/check_compile_scaling.sh` (new, in the full gate set) passes
at the closing commit, and each reproducer's pre-fix commit fails it.

The gate is in the full gate set (2026-10-26), with budgets measured on concrete-ci-2 and recorded
in `scripts/tests/lib/compile_scaling_budgets.tsv`. Building it found ten more superlinear stages
over declaration count and nesting depth (bugs 273 – 282, open); they and Lower's loop header phis
(bug 270; bugs 265 – 267 are fixed and their shapes judged) are named exclusions the gate reports on
every run. What remains for the exit: each fix removes its exclusion, and the declaration and
nesting shapes move to 10^4 – 10^5 elements.


<a id="coherent-plan-r-0551"></a>

### Historical task R-0551

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The objective's profiling process survives, but the named legacy workloads and compiler stages must be replaced. surviving requirement: Continuously profile supported new-compiler workloads at exact landed SHAs and prove hotspot detection with a deliberate scaling regression. This line governs over older implementation directions for the legacy compiler.

**Objective:** Continuous compiler profiling with leanprof, the project owner's Lean profiler, owned
by R-0422 with the performance track.

A scheduled job on the CI server (concrete-ci-2) runs leanprof on fixed compiler workloads at every
commit that lands on main: `examples/lox --emit-llvm`, R-0550's scaling corpus and the full std
build. It publishes the top hot spots and the time per compiler stage, compares them with the
previous landed commit, and opens a tracked finding when a hot spot grows, or a new one appears,
above a recorded threshold. It complements the instruction-count tracker and its alarm of about 1.5%
([PERF_TRACKING.md](../project/PERF_TRACKING.md)) and does not replace it: the
alarm says that a commit got slower, and the profile says where. Server provisioning lives in the
owner's private dotfiles (`servers/`); this repository holds only the portable scripts.
**Exit:** for ten consecutive landed commits the job has published a profile and a comparison at
each commit's SHA, and a control that adds a known hot spot (a deliberate quadratic loop in one
stage) opens a finding.


<a id="coherent-plan-r-0355"></a>

### Historical task R-0355

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The performance obligations survive, but deadlines tied to old structural migrations do not. surviving requirement: Establish exact-SHA development-speed baselines for the new compiler and grow release budgets to cover compilation, proofs, reports and independent verification with regression controls. This line governs over older implementation directions for the legacy compiler.

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


<a id="coherent-plan-r-0446"></a>

### Historical task R-0446

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: IntArith and EmitSSA are legacy machinery, but generated coverage and consumer-integrity gates remain required. surviving requirement: Generate the new Core operation and trap inventory for backend-neutral consumers, fuzzing and capability reporting, with controls proving automatic constructor coverage and actual consumer use. This line governs over older implementation directions for the legacy compiler.

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
