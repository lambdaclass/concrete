# Roadmap restructure around the new compiler

Status: approved by the owner on 2026-11-02, after Codex's review. This is the plan for item 1 of
[the interim queue](../../../legacy/docs/archive/roadmap-2026-11/ROADMAP.md#the-queue). It describes the migration once it has
landed and schedules nothing beside the interim queue.

## Why

ROADMAP.md still holds the current compiler's execution plan below its "Not in force" notice. That
plan covers Phase 1 – 7, rows 0 – 17, the Phase 1 exits, feature prerequisites, the capacity
allocation and the release map. Gates enforce it, and 546 tasks name its phases. The new-compiler
decision of 2026-11-01 replaced that plan, so the roadmap is rebuilt around the new compiler.

## Phases

| phase | goal | done when |
|---|---|---|
| A. Core and theorem | the spec audit of 2026-11-02 is disposed of (its gaps and duplications assigned to owners); the target Core proposal is decided by the owner, the whole shape at once and before implementation; the semantic contract with the named values-to-bytes boundary; the end-to-end theorem stated in Lean with its holes; the trusted base listed | Core approved; the theorem statement and trusted base on main |
| B. Slice 1 | `ConcreteSpec` and the new compiler for slice 1: scalars, `let`, the move and use-after-move refusal of an empty struct without `Copy` consumed by a struct-destructuring `let`, one checked operation, return or a defined trap, from parsing through native execution | the slice's rules are judgments, independent of the checker and elaborator, whose correspondence is proved; fuel exhaustion is distinct from divergence; LLVM output is labelled unverified, and the lexer, model-translation and native-image obligations are explicit; its examples pass through the spec and the native build; its theorem is proved; its chapter is generated; the outside-reader test passes. Then the old pipeline is deleted from main under the retention contract of the new-compiler decision |
| C. File-copy | slices ordered by the file-copy program's dependencies: directories, `File`, `with(Fs)`, fallible operations, explicit cleanup on every error path, `defer`; chapters retired as Lean replaces them; std, examples and tests migrate as their constructs land | the file-copy program, its failure paths included, builds natively from the new compiler with its theorems; **0.1 is cut here**, with the supported subset, the unsupported constructs and the LLVM assumptions stated |
| D. Whole language | the remaining constructs as slices; every chapter generated; the conformance suite complete; each paused workload (RocksDB slice 1, the ethlambda workloads, the usability track) resumes when the constructs it needs are supported | every construct of the decided language is a slice, and no prose chapter is left |
| E. Verified growth | the proof order's later items: functional models, open modules and the C boundary, ghost permissions for trusted code, SMT-checked lowering, proof-producing equality saturation, the Lean Compiler Backend native path, the verified parser (lexer, semantic actions and parser execution included); richer verification: typed contracts, exact contract identity, two-state contracts and frames, checked totality, the specification library | each item's theorem with its residual assumptions stated |
| F. Evidence as a product | provenance, the canonical verification task, receipts and the independent consumer, rebuilt on the new Core (minimal provenance and task identity already accompany the first authoritative proof claims) | receipt v1 frozen |
| G. Release and ecosystem | external users, flagships, packages, 1.0 | as the old Phase 7, rebased |

Development throughput (T: R-0539 – R-0547 and R-0513) runs beside the queue, along with the
landing of already-validated branches. That is the whole scheduling rule: one compiler sequence,
with no capacity shares and no slots. The eight-item proof order already on ROADMAP.md is the
internal order of B – E.

Every criterion of the specification's steps 1 – 9 maps to a task and an acceptance artifact in
A – D. Those steps are: owner decisions, absorption and coverage audit, simplification, reading as
one document, nothing outside the spec, conformance suite, code matching the spec, Lean links, and
the outside-reader test.

## What ROADMAP.md keeps

- **Aim.**
- **Where we are.** The legacy compiler is frozen at `legacy-compiler-final`; the language decisions
  are recorded; the shape of Core is open.
- **The phases and the queue.** The queue rows are A1 … with a done-when and an owning task each;
  later phases are summarized until they become next. The interim queue is absorbed into them.
- **The owner's rule**, with a link to the decision record.
- **Development throughput**, its scheduling text rebased.
- **The candidate table.**
- **Releases.** The rhythm, the R-0333 candidate records and the compatibility policy are kept; the
  release map is rebased (0.1 at the end of C), and the promotion stages (a), (b) and (c), exact-SHA
  binding and result vocabulary are unchanged.
- **Pointers** to the backlog (the one home of the pull rules), the decision log, the
  reconciliation record and the archive.

## What leaves ROADMAP.md

- **The old execution plan** moves verbatim to `docs/archive/ROADMAP_LEGACY_PLAN.md`: Where we are
  2026-10-21, What is next, Phases 1 – 7, the estimates, rows 0 – 17, the Phase 1 exits with their
  bug spans, the feature prerequisites, the capacity allocation, the old release map and the
  next-release blockers. Task and exit IDs stay citable there, and it schedules nothing.
- **The owner-decision table** is merged into [decisions.md](../../decisions.md). IDs, dates and
  rationale are preserved, with no second copy. `spec.py`'s OD resolver, `review.sh`'s prompts,
  AGENTS.md and the roadmap-item command change with it.
- **The bug dashboard** moves to `docs/bugs/`, generated, with views scoped by compiler. Bug records
  gain a validated `compiler:` field, backfilled to `legacy`. Legacy bugs are recorded and never
  dispatched for repair (`build_queue.py`, with a control). Discovery statistics are never mixed
  across compilers. Historical exit membership stays in the archive.
- **The two legacy candidate rows** get a dated disposition with their SHAs: `rocksdb-slice1` is
  paused and `v1-design-examples` becomes research input for phase A.

## Tasks

Every task gets one class:
- **carried**, to a phase A – G or T;
- **split**, where the old-compiler machinery is retired and the surviving requirement is stated
  and carried. This includes regression duties that are not source programs: evidence-substitution
  controls, consumer tests and gate integrity;
- **retired** with the legacy compiler, its body archived;
- **backlog**, with a pull trigger phrased for the new compiler.

The classification is an immutable migration record, `docs/project/roadmap_restructure_2026-11-02.tsv`.
Task bodies carry the live result, and the reconciliation record gets narrow supersessions, so
retired migration instructions stop binding.

## Gates and consumers

These change with the restructure:
- `check_roadmap_coverage.sh` uses phases A – G and T and their queue mapping. Its Phase 1 exits,
  feature-prerequisite and capacity checks go with their sections, replaced by: every queue row has
  a done-when and an owning task.
- The bug tooling: `bug_dashboard.py`, `bug_ledger.py`, `check_ledgers.sh`, `new_bug.sh`,
  `check_new_bug.sh`, `refresh_generated.sh`.
- The work queue: `build_queue.py`, `check_work_queue.sh`, `WORK_QUEUE.md`.
- The decision references: `spec.py`, `review.sh`, `.claude/commands/roadmap-item.md`, `AGENTS.md`.
- `check_doc_links.sh` for the removed anchors, including the incoming links from
  `ROCKSDB_SLICE1.md`, `PROMOTION_PIPELINE.md` and `ROADMAP_TASKS.md`.
- `PHASE_EXIT_CHECKLISTS.md`, which is marked historical.

Each gate keeps negative controls.

## Landing

1. **A preparation branch.** It adds backward-compatible support to the consumers, the schemas and
   the controls, and the reviewed classification record. Behaviour is unchanged.
2. **One atomic switch.** It replaces the control plane and applies the task classes and placement,
   with the phase-entry backlog review. It also moves the generated views, retargets the links and
   removes the compatibility support.

Each landed state has one meaningful queue, working bug filing and regeneration, and intact
promotion semantics. Both branches go through Codex review, the pre-push hook and branch CI.

The freeze is made real by tagging `legacy-compiler-final` on both remotes.
