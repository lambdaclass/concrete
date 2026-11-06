# Concrete Roadmap

Concrete is a systems language with linear ownership, second-class references and explicit
capabilities. Its specification is a Lean library, `ConcreteSpec`: readable definitions with
plain-words doc comments and conformance examples. A new compiler is built from that library, proof
first, one slice of the language at a time, and each slice states the exact scope of its theorem.
The first executable milestone is slice 0, a closed `u8` arithmetic subset traversing the actual
compiler to an unverified LLVM native build. Each used lowering is proved as it lands. A composed
conditional image theorem and its explicit assumptions accompany it. Slice 1 extends this path with
its approved linear component and launcher obligations
([decision](docs/decisions.md#decided-project-proof-infrastructure)). Slice 1's theorem covers
its exact source down to BIR. Its native output is qualified by tests and labelled unverified; any
claim about the native image is conditional on the LLVM, linker and image assumptions it lists. A proof that reaches native code is later work (phase E), not the first
milestone. The old compiler is frozen as `legacy-compiler-final`. A pinned build of that tag is the
differential oracle, and the old pipeline is deleted at B3.

This file is the plan: the phases, the queue and one row per live task. It decides nothing. The
rules below summarize decisions recorded elsewhere, and the linked record governs where they differ.
The [2026-11 archive](docs/archive/roadmap-2026-11/README.md) holds the previous roadmap, its task
register, its backlog and its decision log, all unchanged. Its
[task disposition table](docs/archive/roadmap-2026-11/task_disposition.tsv) says what happened to
each of the 555 old task IDs: retained here, transferred with its obligations to a task here,
deferred until a trigger, retired with a reason, or done. Archived IDs are never reused.
`scripts/tests/check_roadmap.sh` checks this file against that table.

## Rules in force

Every task follows these rules. Each is a summary that links the dated record holding the rule and
its reasons, or says which part still awaits the owner; this file holds no rule of its own. Language rules are not decided here: they are the owner decisions in [decisions.md](docs/decisions.md#owner-decisions), and the project and architecture decisions are [there too](docs/decisions.md#decided-project) (decision row P20, 2026-11-05).
The design rules (one form per situation, one owner per fact, the most transparent long-term option,
verbosity that buys clarity) get one home in `PHILOSOPHY.md` when the `wip/philosophy` branch lands.

1. **The owner decides the language.** No agent, orchestrator or reviewer chooses, encodes or ships
   any side of an open language question. That includes doing so inside a fix, a test or a document.
   Agents bring options and a recommendation. An answer counts only when the owner gives it
   explicitly, and whoever records it writes its date in [decisions.md](docs/decisions.md)
   ([recording rule](docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-02--owner-recording-rule),
   [AGENTS.md](AGENTS.md#rules)).
2. **Lean is the specification.** Each rule is a Lean definition with its rule ID, a doc comment in
   plain words and its conformance examples. The normative definitions form one authoritative library
   in cohesive modules, with `ConcreteSpec/Spec.lean` as its public import, and proofs and tooling
   live in files that import it (R-0560); a browsable owner view is generated from the same sources
   ([revised 2026-11-06](docs/decisions.md#decided-project-long-term-2026-11-06)). No prose is generated from Lean. Machine
   views (the rule registry, IDs and manifests) are derived and have a staleness gate. The user guide
   teaches and links the rules, and states none of its own. Prose chapters are retired as their rules
   land in Lean, and a chapter is retired only after a browsable rendering of the Lean rules and a
   rule-by-rule coverage report show that every rule it states has a Lean definition (R-0555)
   ([2026-11-01](docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-01--lean-as-the-only-source),
   [revised 2026-11-04](docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-04--lean-is-the-specification)).
3. **The new compiler is built from the spec.** `ConcreteSpec` imports nothing from any compiler. It
   holds:
   - the grammar as data, with its LL(1) check;
   - Core with identities from the start;
   - the statics as inductive judgments, the only normative form, with a checker proved to
     correspond;
   - the one elaborator;
   - the clock-indexed evaluator, which is the reference interpreter;
   - the conformance examples.

   The compiler imports it and builds a proved lowering chain below Core. Slice 0 gets a BIR → LLVM
   printer, labelled unverified; slice 1 extends the same path. The Lean Compiler Backend follows, with a
   bounded BIR → LCB → native proof pilot right after slices 0 and 1 (LP0,
   [2026-11-06](docs/decisions.md#decided-project-long-term-2026-11-06))
   ([decision](docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract),
   [revised 2026-11-05](docs/decisions.md#decided-project-proof-infrastructure)).
4. **The old tree is legacy.** The owner chose a `legacy/` folder on main on 2026-11-04 and approved
   the restart details below on 2026-11-05 (decision rows P5–P8, P21, P22;
   [record](docs/decisions.md#decided-project-restart)).
   In one train, everything old moves under `legacy/`: the compiler, its scripts, tests, examples, std, proofs, gates, docs, research, site
   and bug ledger. `legacy/` is a byte-frozen snapshot of main's tree at the move commit. It is never
   patched and never run. One root check refuses any modification or addition under it, and any
   deletion except one its retention row (below) allows. Its gates are not
   re-run: they were qualified at `legacy-compiler-final`, and its evidence stays replayable from that
   tag. The differential oracle is built from the tag in its own isolated checkout, not from
   `legacy/` (R-0564). The infrastructure for the new root is prepared beside the old tree (T0,
   R-0568) and rehearsed on the exact candidate root (T1, R-0563), and the move comes after (T2,
   R-0558). The root starts minimal. A file comes back from
   `legacy/` only when a slice needs it, in that slice's change. Live tooling stays live: landing,
   review, the hooks, CI and the changelog. At B3 only the old pipeline (the compiler, its gates and
   their scripts) is deleted. Retained assets (corpora, regressions, examples, std, the bug ledger,
   the site and docs) stay archived in `legacy/` until they migrate. Each one leaves only when its
   row in the root retention manifest `RETENTION.tsv` (R-0562) is resolved. That manifest and the
   [task disposition table](docs/archive/roadmap-2026-11/task_disposition.tsv) live outside `legacy/`
   and survive B3.
5. **Authority uses permissions and values.** `with(...)` is compile-time permission, and linear values
   are the resources. An operation needs both. `main` consumes the target's grant values. Allocation
   takes an allocator value and can fail. Filesystem access goes through directory values
   ([decision](docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--authority-design)).
6. **The proof grows in a fixed order.**
   1. State the semantic contract and the final theorem first, with values-to-bytes as a named
      boundary.
   2. Executable Core semantics, where running out of fuel is distinct from divergence, with type,
      ownership and effect soundness.
   3. One minimal end-to-end path, built in cumulative slices from slice 0: each used lowering lands
      with its preservation and output-invariant proofs, and the composed theorem, with its named
      premises, is checked from the first slice
      ([decision](docs/decisions.md#decided-project-proof-infrastructure)).
   4. Functional models for safe code.
   5. Open-module correctness and C contracts.
   6. Ghost permissions for trusted code.
   7. SMT-checked lowering.
   8. Proof-producing equality saturation.

   Parsing, linking and native-image assumptions stay explicit, and only kernel-checked proofs count.
   Before items 4–7 are built, their supported subsets, representations and residual assumptions are
   proposed to the owner
   ([proof order](docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-01--compiler-proof-order)).
7. **The specification criteria still bind.** These are owner decisions, audit, simplification,
   refusal coverage, conformance, code matching the spec, Lean links and the outside-reader test. They
   are read through rule 2
   ([criteria](docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-10-30--specification-first)).
   Other documentation cites the Lean rules and loses any statement they contradict
   ([documentation rule](docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-10-30--documentation-follows-the-spec)).
8. **Validation semantics do not change.** Every result is bound to an exact SHA. Every result is one
   of passed, failed, skipped, cancelled or incomplete
   ([promotion pipeline](legacy/docs/project/PROMOTION_PIPELINE.md#promotion-pipeline)). A faster path never
   reports a check it did not run as passed.
9. **Codex reviews everything.** Every change, plan, specification, decision row and piece of
   infrastructure is reviewed by Codex before it is published or brought to the owner. A second,
   independent reviewer (Kimi, Grok or agy/Gemini) reviews as well when one is available; when none
   is, its absence is recorded with the review, never assumed
   ([record](docs/decisions.md#decided-project-review)).

### Scheduling rule

There is one compiler sequence, A → G. Development throughput (T) runs beside it, together with
the landing of already-validated branches, with no capacity shares. The proof order (rule 6) is the
internal order of B–E. RocksDB slice 1, the ethlambda workloads and the usability track are paused
until the new compiler supports their constructs.

### Defect disposition rule

From the first day of the new root, a minimal issue record holds every defect: ID, reproducer or
evidence, owner, severity and closure condition. Spec, proof, launcher, CI and migration defects
are filed there before slice 1 exists. Legacy-compiler defects are recorded with `compiler: legacy`
and never repaired. When the old pipeline is deleted at B3 they become *retired with the old
implementation*, never *fixed* (R-0561). Each keeps its reproducer, ruling and structural class.
A defect the frozen browser compiler still shows stays visible while the site serves it. Std
defects move with std as contracts and regressions. A legacy ruling that states language behaviour
becomes a conformance example when its construct's slice lands. A new compiler's silent defect (wrong code, an
unsound claim) is stopped at once by a repair or fail-closed containment. A loud one is filed, pinned
and owned by the task that removes its structural class.

## Phases

| phase | goal | done when |
|---|---|---|
| A. Core and theorem | Dispose of the spec audit. The owner decides the whole Core shape before implementation, in four stages: provisional approval (A2a), a proposed `Spec.lean` with four checked design theorems (A2b), integration of its parts with their interaction proofs, the modular layout and the browsable owner view (A2i), and the owner's freeze of the integrated scopes (A2c). State the semantic contract and the end-to-end theorem in Lean, and list the trusted base. | Core frozen by the owner after the four design theorems and the integration proofs are kernel-checked. The theorem, its holes and the trusted base on main. |
| B. Cumulative slices 0 and 1 | Slice 0 establishes the running proof infrastructure: a closed `u8` arithmetic subset through every used IR to an unverified LLVM native build, with per-lowering proofs and the composed conditional theorem. Then `ConcreteSpec` and the new compiler for slice 1, extending the same path, from parsing to a native build: scalars, `let`, the move and use-after-move refusal of an empty struct without `Copy` consumed by a struct-destructuring `let`, one checked operation, and a return or a defined trap. | The slice's rules are inductive judgments, independent of the checker and the elaborator, with proved correspondence. Every judgment, semantics and pass is total. Fuel exhaustion is distinct from divergence. The theorem, exact-source component correctness down to BIR, is proved. The native build is qualified by tests, not proved: the LLVM output is labelled unverified, and its assumptions are listed with the lexer, model-translation and native-image obligations. A native-image claim, if any, is stated separately as conditional on those assumptions. Minimal provenance accompanies the first authoritative proof claims. The examples pass through the Lean semantics and the native build. The outside-reader test passes on the Lean rules. Phase B closes only after slice 1 and B2/B3 acceptance; then the old pipeline is deleted from `legacy/`. |
| C. File-copy | Workloads grow from simple proved examples to file-copy and then supported real programs; each records program correctness, authority, failure behaviour and residual assumptions. Slices ordered by what a file-copy program needs: directories, `File`, `with(Fs)`, fallible operations, explicit cleanup on every error path, `defer`. Std, examples and tests come back as their constructs land. | The file-copy program, failure paths included, builds natively with its theorems. **0.1 is cut** with the supported subset, the unsupported constructs and the LLVM assumptions stated. |
| D. Whole language | The remaining constructs as slices. The paused workloads resume as their constructs land. | Every construct of the decided language is a slice with Lean rules, and the conformance suite is complete. |
| E. Verified growth | Proof-order items 4–8, the Lean Compiler Backend native path, the verified parser, typed contracts, frame reasoning, termination proofs and the proof library. Earlier slices already contain example-specific program theorems; E expands reusable models and proof support. Contracts have no source `old` and no snapshot construct (OD117): a postcondition reaches the entry state only through an explicit Lean theorem. Spec functions are total (OD115). An ordinary function can get its own termination proof, and there is no total-function category (OD116). | Each item's theorem holds, with its residual assumptions stated. |
| F. Evidence as a product | Provenance, the canonical verification task, receipts and an independent consumer, all on the new Core. | Receipt v1 frozen. |
| G. Release and ecosystem | External users, flagships, packages, editor, incremental builds, 1.0. | External-user acceptance, package evidence, conformance, compatibility and maintenance commitments hold for the released subset. |
| T. Development throughput | Beside the sequence: the minimal root, then faster landing, review and CI. | The [targets](#development-throughput) hold. |

## The queue

**Active:** T0, A1 and A2b. **Next:** T1, T2, A2i and A2c. A2a (provisional approval of the Core shape, decision row Q9.19) is done: the owner approved every Core row on 2026-11-05 ([record](docs/decisions.md#decided-2026-11-05)); the approval binds the design without freezing it.

| row | goal | tasks | done when |
|---|---|---|---|
| T0 | Prepare the restart beside the old tree (decision row P7, approved 2026-11-05) | R-0568, R-0564 | Checklist, each item on main before T1: (1) the new root's CI jobs; (2) `required-checks.txt`, enforced by `train.sh watch` and `train.sh land`; (3) the required-check transition mechanism, an explicit old-to-new job mapping that is validated and needs the replacement checks to pass at the exact candidate SHA, never marking a dropped legacy check passed; (4) `ISSUES.tsv` and its validator; (5) `RETENTION.tsv` and the task disposition table; (6) `scripts/oracle/ORACLE.lock` with the oracle release artifact and its digest; (7) the archive records of the stopped services. |
| T1 | Rehearse the move on the candidate root | R-0563 | On a throwaway branch holding the exact proposed day-one root, a fresh clone builds the candidate, hooks, runs exact-SHA CI and passes a dry-run landing, and the negative controls are refused for the expected reason. |
| T2 | Move the old tree to `legacy/` in one train, after T0 and the T1 rehearsal | R-0558, R-0565 | The root holds only the day-one set. `legacy/` is byte-frozen and checked. The move itself lands on both remotes. The website is down. |
| A1 | Dispose of the spec audit and draft the boundaries Core must satisfy | R-0555, R-0312, R-0316 | Each audit finding, gap and duplication has an owner and an acceptance artifact. The [boundary drafts](legacy/docs/compiler/A1_BOUNDARIES.md) exist. |
| A2b | Formalize the approved design in a proposed `Spec.lean` (current work) | R-0511 | Exit to A2i: `ConcreteSpec/Spec.lean`, aligned with the decided rows, is headed "PROPOSED: not normative until Core freeze"; the four design theorems are kernel-checked. The four design theorems (`reset_fresh`, `defer_cleanup_releases`, `open_mut_freezes_root`, `rep_roundtrip`) and their full obligations are kernel-checked without `native_decide`. Every row the formalization changes returns to the owner. This is separate from slice 1's soundness theorems. |
| A2i | Integrate the proposed spec before the freeze ([decision](docs/decisions.md#decided-project-long-term-2026-11-06)) | R-0571, R-0496, R-0560, R-0555, R-0559 | The separately proved parts share Core interfaces and one integrated semantics. Interaction proofs hold for a borrow across deferred cleanup, an allocator reset with outstanding loans and cleanup that needs authority, and the README obligations O7 and O9 are discharged. The semantic contract is stated. The normative definitions are one library in cohesive modules with `Spec.lean` as the public import. The browsable owner view exists. An outside reader has read the integrated spec through that view. |
| A2c | Core freeze | R-0567 | The owner approves the freeze of the integrated scopes after A2b's four theorems, A2i's integration proofs and an outside-reader pass. Only then is PROPOSED removed. A3 sends Core back only as a recovery when it exposes a substantive change. |
| A3 | State the theorem and the trusted base in Lean | R-0502, R-0506, R-0508, R-0509, R-0500 | The contract and the modular layout from A2i are on main. The theorem, with its holes, the named values-to-bytes boundary and the trusted-base inventory, is on main. The theorem skeleton uses concrete slice interfaces; its semantic-premise inventory is separate from the axiom audit. Before slice 0's authoritative claim, its actual Core fragment is connected to the frozen definitions and any instrumentation erasure it uses is proved; later features require integration proofs into that same Core. Slice 0 starts after A2c and the applicable A3 foundations for its authoritative claims; a provisional skeleton may start earlier and claims nothing normative. |
| B0 | Build and qualify slice 0, the proof spine ([decision](docs/decisions.md#decided-project-proof-infrastructure)) | R-0501, R-0503, R-0425, R-0499, R-0493, R-0507, R-0521, R-0552, R-0118, R-0498, R-0553, R-0573 | Slice 0 runs through every used IR to unverified LLVM native execution, with per-lowering proofs, independent supported-subset coverage, the composed conditional image theorem, artifact binding, example correctness, including transport to the exact native artifact under the named premises, and classified differential and refusal controls. This does not trigger legacy deletion. |
| B1 | Build `ConcreteSpec` and slice 1 with its theorem | R-0425, R-0499, R-0492, R-0493, R-0514, R-0501, R-0503, R-0497, R-0504, R-0507, R-0521, R-0552, R-0528, R-0479, R-0118, R-0573 | Slice 1 runs from parsing to native code. Its judgments are independent, its correspondence and soundness are proved, its obligations are explicit and its provenance is minimal. Its conformance examples pass on both implementations. |
| LP0 | The LCB pilot: bound the LLVM hole ([decision](docs/decisions.md#decided-project-long-term-2026-11-06)) | R-0572 | After B1 and before C and D grow the language: the slice-0 and slice-1 subset compiles through BIR → LCB → native code with its adapter proof composed with the slice theorem under named premises. Every remaining backend obligation is a task with a milestone. |
| B2 | Qualify slice 1 | R-0491, R-0498, R-0272, R-0550 | Differential (Lean semantics, native build, legacy oracle), refusal and gate-integrity controls hold. The fuzzers target the new compiler. The outside-reader test passes. |
| B3 | Audit totality, then delete the old pipeline | R-0553, R-0562, R-0561 | The totality audit finds no partial definition. Every `RETENTION.tsv` row for the old pipeline is resolved. Legacy defects are retired with the old implementation. The old pipeline is gone from `legacy/`; retained assets stay there until their rows resolve. |

Tasks shared by B0 and later rows deliver their slice-0 increment at B0; their remaining acceptance obligations stay with the later row. Dependencies between cumulative tasks apply to the corresponding increment.

<a id="questions-for-the-core-session"></a>
### Questions for the Core session

The owner answered these provisionally on 2026-11-05 (A2a; [the decided rows](docs/decisions.md#decided-2026-11-05-core)) and freezes them at A2c. The [Core proposal](legacy/docs/compiler/CORE_PROPOSAL.md) is the background; where it and a decided row differ, the row governs:

1. allocator discipline;
2. `destroy` bounds;
3. the `HeapArray` element view and the raw-memory taxonomy;
4. the type of static text;
5. whether capability and type arguments are inferred or written (decided: capability arguments are written, a revision of OD45 and OD87);
6. static refusal versus traps;
7. target grant schemas and narrowing;
8. slice 1's assurance scope;
9. the whole Core interface.

<a id="targets"></a>
### Targets

The hosted grant table decided in row Q7.10 is for `linux-x86_64-hosted`. The first native **proof**
profile is AArch64 on macOS through the Lean Compiler Backend
([VERIFIED_PIPELINE.md](legacy/docs/compiler/VERIFIED_PIPELINE.md)). These are separate: a target's grant
table states what a program may be given there; a proof profile states which native path carries
the theorem. The plan is Linux x86-64 and macOS AArch64 first, then FreeBSD and OpenBSD as
additional target tables, each added when a workload needs it and each a decision for the owner.
FreeBSD's Capsicum and OpenBSD's `pledge` and `unveil` fit the capability model: the operating
system can enforce what a program's declared capabilities and grants already promise. A target
without a primitive that confines path lookup refuses filesystem access.

<a id="target-theorem-shape"></a>
### The shape of target behaviour theorems

Ideas taken from the silmaril-lean development (no dependency, no vendored code; reviewed
2026-11-05). A theorem about a program's behaviour on a target (R-0496, R-0508, A3) is stated in
this shape; which parts a given theorem proves, and under which premises, is stated with it:

- **Observable events.** Behaviour is a set of traces of events an outside observer sees (reads and
  writes on grants, the exit code with the final state of the granted filesystem, a trap with its
  location), plus errors and an "unmodelled" flag; a claim requires the program never to step
  outside the model (fail closed).
- **Safety in general.** Every trace is a prefix of an allowed trace. Where the environment is
  nondeterministic, the allowed behaviour is a set of ideal traces, not one. Safety is partial
  correctness: it needs no termination premise.
- **Termination and completeness under stated premises.** That every run exits (or traps at a stated
  point) and that an ideal trace occurs are proved only under explicit premises about the program,
  its resources (for example enough memory and budget) and the environment (for example a bound on
  spurious refusals), following the decided partial-correctness contracts and per-function
  termination theorems (row P18).
- **A hostile environment.** Within its premises, a claim holds for every input, every spurious
  refusal or short transfer by the operating system and every launch layout; a premise that rules
  out part of the environment is stated, never implicit.
- **Non-vacuity.** Each theorem comes with a witness that its premises can hold.
- **Exact artifacts.** A claim is about the exact source and the exact bytes produced, never about a
  name.
- **An explicit trusted base.** The machine, loader and operating-system models the claim relies on
  are listed as assumptions.
- **Authority** (Concrete's own clause). Every event in every trace uses a grant the program holds,
  through an operation its declared capabilities allow.

## Tasks

One row per live task: what it delivers, when it is done, and what it waits for. "Depends on" names
tasks (R-NNNN), queue rows (A2b: every task of that row), whole phases (C: every task of phase C) or
an owner decision (`owner: <decision>`), comma-separated; `—` is nothing. A task transferred
into one of these carries the obligations recorded in its row of the
[disposition table](docs/archive/roadmap-2026-11/task_disposition.tsv). The full old body stays in the
archive. Rows for C–G are summaries. Each is refined when its phase becomes next.

### A. Core and theorem

| task | objective | done when | depends on |
|---|---|---|---|
| R-0555 | Dispose of the 2026-11-02 spec audit, then keep one normative spec in Lean with independent judgments and rule-indexed conformance | every audit row has an owner; each slice's Lean rules replace the prose they cover, and bug-to-rule references resolve; a browsable rendering of the Lean rules and a rule-by-rule coverage report exist, and no prose chapter is retired before the report shows each of its rules in Lean; the browsable owner view (each rule's English doc comment, examples, dependencies and proof links, generated from the same sources and stating no rule of its own) exists before the freeze (A2c, revised 2026-11-06) | — |
| R-0312 | The unsafe-surface and raw-memory taxonomy: validity, provenance, aliasing, ownership transfer | taxonomy decided in the Core session (question 3) and stated in Lean | R-0555 |
| R-0316 | An unsafe-aliasing policy without hidden reference UB | the policy is part of the approved Core | R-0312 |
| R-0559 | Outside-reader pass on the approved Core design, then on each feature | a reader new to Concrete reads the decided rows and the integrated spec through the browsable owner view; each confusion is recorded and resolved before the freeze (A2c); the check repeats for each feature as it lands | R-0555 |
| R-0511 | Representative design proofs before Core freezes, then cumulative slices with honest theorem scopes | the four A2b theorems are kernel-checked against the proposed `Spec.lean`, without `native_decide`, with their full obligations (decision row Q9.14): `reset_fresh` (a reset gives a brand distinct from every live brand, resets in a loop give pairwise-distinct brands, and the new allocator's operations refuse values of an old brand); `defer_cleanup_releases` (every structured exit runs the deferred calls in reverse registration order, releases each held loan after its call, and preserves the allocator-dependency check); `open_mut_freezes_root` (a mutable heap open excludes every other access to its root, and a shared open never grants a write); `rep_roundtrip` (encode then decode is the identity for `u8` to `u64`, `bool` and the empty struct); each later slice states its theorem scope (A2a was approved 2026-11-05) | — |
| R-0567 | Core freeze (A2c) | the owner approves the freeze of the integrated scopes after A2i's complete acceptance (the four A2b theorems, the integration proofs, the semantic contract, the modular layout, the browsable owner view and the outside-reader pass); PROPOSED is removed from its header in the freeze change | R-0571, R-0574, R-0496, R-0560, R-0555, R-0559 |
| R-0566 | One live home for binding decisions (decision row P20, approved 2026-11-05; the decisions are [recorded](docs/decisions.md#decided-project) in `docs/decisions.md`, which the move places at `docs/decisions.md`) | the still-binding architecture and process decisions of the archived roadmap log (new compiler and retention contract, authority, allocation, Lean as the specification, the owner-recording rule) are in `docs/decisions.md` with their dates and revision links; the owner-approved revisions (OD45/OD87, TYPE-22, TRAIT-1 and any other accepted row) are recorded there; ROADMAP.md, PHILOSOPHY.md and AGENTS.md cite it and restate nothing; a gate refuses a roadmap rule link into the archived log once the move lands | — |
| R-0560 | One authoritative spec library: the normative definitions in cohesive modules under `ConcreteSpec/`, with `ConcreteSpec/Spec.lean` as the public import; proofs, tooling and derived views in files that import it (revised 2026-11-06) | the layout exists before the freeze; a gate refuses a normative definition outside the library and a proof inside it, each with a negative control | A2b |
| R-0571 | Integrate the proposed spec before the freeze (A2i): shared Core interfaces, one integrated semantics and interaction proofs (decided 2026-11-06) | the parts share Core interfaces and one semantics; kernel-checked proofs cover a borrow across deferred cleanup, an allocator reset with outstanding loans and cleanup that needs authority; README obligations O7 and O9 are discharged; no `sorry`, no `native_decide`, only the three standard axioms | R-0511, R-0560 |
| R-0574 | No hidden traps and bounded execution ([decision](docs/decisions.md#decided-project-no-hidden-traps), 2026-11-06): the Core rules in the spec, then the checker | the owner picks the three pending spellings (checked operations, the Lean discharge reference, the termination permission marker); within A2i, before the freeze, the proposed spec has the admission rule with `checked_no_implicit_traps`, the octagon fact language and its transfers, the loop rule, closed contract evidence, the termination rule with source permissions, and the per-function execution facts with `bounded` derived, each with accepted and refused examples (`add100` refused, with guarded variants); the checker's implementation, its Lean soundness proof and the per-function facts report follow with the slices (phase B), slice 0 first; each guarantee is published only when its evidence lands; the stack target contract (claim 1, then claim 2) is target work | R-0511 |
| R-0496 | The semantic contract (outcomes, traces, authority, resources, the error model) and the clock-indexed evaluator | the contract is stated in Lean; the evaluator runs; fuel exhaustion is distinct from divergence; trap location and message, the exit-code mapping and the target observations are part of the contract, and until they are, every claim about executable behaviour is qualified by the model's abstract observations | R-0560 |
| R-0502 | One value/raw-memory boundary and one layout authority | the boundary is named in the theorem; layout has one owner with its proofs | R-0496 |
| R-0506 | Every proof boundary and validator obligation stated with its premises and status | obligations have explicit premises; transitive axiom classification is enforced; CI audits semantic-premise closure separately from logical axioms and rejects unapproved assumption additions or strengthening | R-0496 |
| R-0508 | The end-to-end theorem: maximal-behaviour refinement, frontend correspondence, resource adequacy, image transport | stated in Lean with its holes; proved progressively per slice; every target behaviour theorem has the [target-theorem shape](#target-theorem-shape); the concrete slice-0 theorem is stated and composed before B0 closes, retaining the maximal-behaviour, resource, observation and image-transport obligations | R-0496, R-0502 |
| R-0509 | The trusted-base inventory, evidence binding, assumption lifecycle and axiom replay | the inventory is on main; every claim's axioms are recorded and replayed from a clean checkout; CI audits semantic-premise closure separately from logical axioms and rejects unapproved assumption additions or strengthening | R-0506 |
| R-0500 | Each decided language rule gets its Lean judgment, semantic clause and accepted, refused or trapping examples | each rule lands with its slice; every rule of the decided language has one by the end of D | R-0560 |

### B. Cumulative slices 0 and 1

| task | objective | done when | depends on |
|---|---|---|---|
| R-0425 | The conformance format and suite: each example names its rule, source and outcome (result, refusal code or trap), and one runner per implementation runs it through the Lean semantics and through the compiler | slice 1's examples pass both runners; the suite is versioned and published at 0.1, and complete at D; first applicable increment at B0; the approved slice-1 obligations remain due at B1 | R-0560 |
| R-0499 | Independent Core judgments and an authoritative checker proved to correspond | correspondence proved for slice 1, with positive and rule-breaking mutation controls; first applicable increment at B0; the approved slice-1 obligations remain due at B1 | R-0500 |
| R-0492 | Scope-resolved identities throughout; lexical identity distinct from persistent evidence identity | alias, symbol, binding and forgery controls hold | R-0499 |
| R-0493 | Typed arithmetic and casts from one semantics, with arithmetic-site facts | widths, signedness, modes and the defined trap match the Lean semantics; first applicable increment at B0; the approved slice-1 obligations remain due at B1 | R-0496 |
| R-0514 | The compiler consumes the exact identity-resolved Core payload checked against the judgments | substitution controls hold | R-0499 |
| R-0501 | Proved lowering passes for sequencing, matching, cleanup and check expansion | each pass preserves behaviour and re-establishes invariants; the slice-0 instances are delivered at B0 and extended cumulatively at B1; every used lowering has preservation and output-invariant proofs | R-0514 |
| R-0503 | The unverified BIR → LLVM path, the native driver and the pinned LLVM toolchain | output labelled unverified; LLVM, linker and runtime assumptions listed; the driver bounds external tools; the slice-0 instance is delivered at B0 and extended cumulatively at B1, labelled unverified | R-0501 |
| R-0497 | Immutable pipeline artifacts bound to exact payloads | forgery, mutation and bypass compile-fail controls | R-0514 |
| R-0504 | Validators with explicit soundness propositions | substitutions and wrong artifacts are rejected without inflating native assurance | R-0497 |
| R-0507 | Type and ownership soundness over the executable semantics | proved for slice 1; first applicable increment at B0; the approved slice-1 obligations remain due at B1 | R-0496, R-0499 |
| R-0521 | Minimal provenance and one canonical verification task for the first authoritative claims | a claim names its compiler, source, theorem and executable digest; minimal provenance and kernel-checked per-compilation evidence accompany the first authoritative slice-0 claim; the evidence connects the exact input to the exact emitted BIR without trusting native evaluation, the backend input is bound to that BIR, and the Lean compiler, runtime and toolchain are listed as execution assumptions wherever the evidence does not close the gap (2026-11-06) | R-0509 |
| R-0552 | Per-compilation certificates (owner-approved 2026-10-25) | a build emits a certificate per output, and the kernel checks it; minimal provenance and kernel-checked per-compilation evidence accompany the first authoritative slice-0 claim; each certificate connects the exact input to the exact emitted BIR through proved judgments or validators, checked by the kernel without trusting native evaluation; the backend input is bound to that BIR (a digest is identity, not correspondence); residual Lean compiler, runtime and toolchain trust is listed as explicit execution assumptions; LCB verification of compiled programs is not counted as verification of the compiler's execution (2026-11-06) | R-0504 |
| R-0528 | Resource limits on input size, memory, time and import closure | each input class has a limit and a hostile fixture | R-0503 |
| R-0573 | Small adversarial interaction workloads from the first slices on, alongside file-copy (decided 2026-11-06) | each slice adds programs that combine its features with earlier ones in difficult ways (for example borrows across cleanup, authority in cleanup, nested views), each with its expected outcome and Lean/native differential check | R-0425, R-0503 |
| R-0479 | Failures and refusals are never turned into facts; diagnostics name the valid next action | refusals survive through results, text, JSON and exit codes | R-0499 |
| R-0118 | Generated coverage of passes, consumers, constructs, operations and traps | a missing row or a synthetic constructor fails the gate; B0 includes pipeline-edge coverage and controls for missing proofs, unregistered premises, wrong artifact substitution and reject-all compilation | R-0501 |
| R-0491 | The defect inventory and the classified differential (Lean semantics, native build, legacy oracle) | comparisons are classified and campaigns are non-vacuous | R-0425, R-0564 |
| R-0498 | Every enforcement claim maps to its gate leg and a targeted mutation | an unmapped claim fails; shape checks are labelled unproved; B0 includes pipeline-edge coverage and controls for missing proofs, unregistered premises, wrong artifact substitution and reject-all compilation | R-0118 |
| R-0272 | Restart fuzzing against the new compiler on concrete-ci-2, seeded from the regressions and examples that survive in the repository and from newly generated programs (the ci-3 corpus and findings were not copied before the handover, decision row P14, and are lost), with reduction and counterexample-to-regression persistence | the generators emit slice-supported programs, the oracle is Lean semantics against native code, and findings become regressions; legacy fuzzing stopped at the move | B1 |
| R-0550 | Compiler scaling and speed baselines at exact SHAs | scaling gates hold for each stage, and a planted regression is caught | B1 |
| R-0553 | Totality audit: every semantics, checker, validator and pass is total and runs as proved | no partial definition; opaque execution gaps controlled; totality and execution-gap checks apply from B0; B3 performs the cumulative audit after B2 | R-0501 |
| R-0562 | The retention manifest `RETENTION.tsv` at the root, written before the move and checked before any deletion under `legacy/` | every legacy asset (corpora, regressions, examples, std, the bug ledger, the site, docs and the archived services) has a row with a destination and a deletion condition; deleting a path under `legacy/` whose row is unresolved is refused (control); the manifest and the task disposition table stay at the root and survive B3 | — |
| R-0561 | Bug ledger by compiler | legacy defects are marked retired with the old implementation, never fixed, each keeping its reproducer, ruling and structural class; std defects move with std as contracts and regressions; every legacy ruling that states language behaviour becomes a conformance example or a recorded owner question; the minimal issue record of the day-one root becomes the new ledger with the first slice | R-0425 |
| R-0569 | A new website, built from scratch with the new compiler (decision row P9) | the site is designed anew around the decided language; its playground runs the new compiler; it teaches and links the Lean rules and states no rule of its own; no page describes the legacy compiler or the language before the 2026-11 decisions as current | B1 |

### LP0. The LCB pilot

| task | objective | done when | depends on |
|---|---|---|---|
| R-0572 | The LCB pilot (LP0): a tiny BIR → LCB → native proof on the slice-0 and slice-1 subset, before substantial feature growth (decided 2026-11-06) | the subset compiles through the LCB with its adapter proof composed with the slice theorem under named premises; every remaining backend obligation has its own task and milestone | B1 |

### C. File-copy and 0.1

| task | objective | done when | depends on |
|---|---|---|---|
| R-0052 | Handle-relative filesystem: directories and files carry authority | file-copy opens, reads, writes and closes through directory values | B, LP0 |
| R-0557 | Global state as capabilities: no duplicable linear constants; `main` receives explicit target grants | grants reach `main`; constant linearity is enforced | B, LP0 |
| R-0023 | The decided entry-point signatures and explicit exit and error behaviour | implemented and tested in the new compiler | R-0557 |
| R-0071 | Std on the new compiler: modules return when their constructs land, against the completion contract | each returned module has its manifest, coverage and error conventions | R-0052 |
| R-0556 | Generated module interfaces and the interface-diff gate | every public API change shows in the diff | R-0071 |
| R-0510 | Checked specialization | specialization is validated against the generic definition | B, LP0 |
| R-0162 | Reusable lemmas for structs, enums, matching, `Result` and `Option` | file-copy's theorems use them | B, LP0 |
| R-0301 | Backend contracts: ABI, layout, sanitizer and FFI controls | the validation project passes for the 0.1 subset | R-0503 |
| R-0334 | The first-user teaching path (the guide) | a checked tutorial covers install to file-copy | R-0071 |
| R-0516 | Release automation for nightlies and trains | a nightly publishes from a qualified SHA | T |
| R-0333 | Release criteria and candidate records, including claims, security and soundness disclosure, compatibility and operations | the 0.1 candidate record is complete | C |

### D. Whole language

| task | objective | done when | depends on |
|---|---|---|---|
| R-0137 | Workload validation: each workload records its attempts, diagnostics and measurements | the dashboard covers every supported workload; workloads grow from simple proved examples to file-copy and then supported real programs, each recording program correctness, authority, failure behaviour and residual assumptions | C |
| R-0321 | The unsafe and trusted profile, with its VM pressure workload | the workload runs in the normal and checked profiles | C |
| R-0285 | RocksDB slice 1 (paused) | open, read, write and close with linear handles and typed errors | C |
| R-0490 | The ethlambda port (paused) | runs with its interop checkpoint | R-0285 |
| R-0515 | The usability track (paused): non-author pilots and legibility measures | a non-author pilot builds a project | C |

### E. Verified growth

| task | objective | done when | depends on |
|---|---|---|---|
| R-0495 | Claims on Core: exact claims, coverage, solver errors and external-proof interfaces | wrong propositions and substitutions are rejected through independent replay | D |
| R-0460 | Obligation sufficiency, with hypothesis provenance | each admitted obligation is proved to suffice | R-0495 |
| R-0473 | One typed contract record with capture-safe substitution | all consumers read the one record | R-0495 |
| R-0482 | Separate identities for model, implementation, proposition and contract | the relation and the substitution matrix are checked | R-0473 |
| R-0524 | Frames and entry-state reasoning without `old` (OD117) | frames are proved on the model; a postcondition reaches the entry state only through an explicit Lean theorem; no source `old` or snapshot construct exists | R-0473 |
| R-0526 | Total spec functions (OD115) and per-function termination proofs, with no total-function category (OD116) | every recursive edge of a contract-referenced spec function is well-founded or refused; an ordinary function's termination proof is a theorem about that function | R-0496 |
| R-0172 | The proof library and formal std models | used by the flagship proofs | R-0162 |
| R-0177 | Proof authoring: stubs, minimization, diagnostics, automation | the validation project passes | R-0172 |
| R-0259 | Runtime-safety obligations: casts, loops, stack, panics, byte, text and path boundaries | the safety corpus is discharged or reported | R-0495 |
| R-0299 | Translation validation of lowering and emitted LLVM, independently replayable | named relations are validated, with corruption controls | R-0503 |
| R-0517 | The Lean Compiler Backend as the native backend | the native path holds its theorem with residual assumptions | R-0572 |
| R-0505 | The native AArch64 profile: linking and image | the image theorem holds | R-0517 |
| R-0424 | Performance evidence: corpus, codegen quality, fair comparisons, optimization ownership | the mixed corpus runs on LLVM and LCB-native | R-0517 |

### F. Evidence as a product

| task | objective | done when | depends on |
|---|---|---|---|
| R-0494 | One verdict authority for every rendered claim | every surface derives its verdict from one judgment | E |
| R-0512 | Claims on compiler-generated Core statements; no authority from legacy evidence | identity, binding and provenance are rebuilt on new Core | R-0494 |
| R-0178 | Stable machine-readable fact schemas and compatibility policy | the schemas are versioned and frozen | R-0494 |
| R-0219 | Proof status: vacuity, spec adequacy, evidence mutation, toolchain migration | the validation project passes | R-0178 |
| R-0201 | Audit commands: `audit`, `explain`, `why`, `diff` and trust-diff gates | the package-scale audit bundle passes | R-0178 |
| R-0353 | The release-evidence graph and an offline independent bundle verifier | a hostile bundle is rejected; receipt v1 frozen | R-0201 |

### G. Release and ecosystem

| task | objective | done when | depends on |
|---|---|---|---|
| R-0119 | Exactly three public flagships with their evidence | graduated and maintained | F |
| R-0383 | Packages: manifest, resolution, interfaces, provenance, trust policy | the multi-package workspace passes | F |
| R-0149 | Incremental builds equivalent to clean builds | the edit, corruption and substitution matrix passes | R-0383 |
| R-0398 | Editor service over the shared session | the editor validation passes | R-0149 |

### T. Development throughput

| task | objective | done when | depends on |
|---|---|---|---|
| R-0568 | Day-one root infrastructure (T0) | on main beside the old tree: the new root's CI jobs; `required-checks.txt` enforced by `train.sh watch` and `land`; a required-check transition mechanism (an explicit, validated old-to-new job mapping; replacement checks must pass at the exact candidate SHA; a control that dropping a requirement without a mapping is refused; no legacy check is ever marked passed); `ISSUES.tsv` (ID, evidence, owner, severity, closure condition) with its validator and controls; the archive records of the stopped services | R-0562, R-0564 |
| R-0558 | Move the old tree to `legacy/` in one train and set up the minimal root. Day one, the root holds the README, this file, AGENTS.md, an empty CHANGELOG, LICENSE, the new build files, `ConcreteSpec/Spec.lean`, `docs/PHILOSOPHY.md`, `docs/decisions.md`, the landing, review and hook scripts, `scripts/oracle/`, `required-checks.txt` and its job mapping, `RETENTION.tsv`, the minimal issue record `ISSUES.tsv` with its validator, and the [2026-11 roadmap archive](docs/archive/roadmap-2026-11/README.md) with its task disposition table. Everything else comes back from `legacy/` when a slice needs it. The status page, the work-queue dispatcher, the fuzz farm, perf tracking and the nightly campaign stop, and each is listed in `RETENTION.tsv` as archived; each returns only with a roadmap task. The ci-3 fuzz corpus, perf history and leanprof data were not copied before the handover (decision row P14) and are not archived. | the root holds only that set; a root check refuses any modification or addition under `legacy/`, and any deletion its `RETENTION.tsv` row does not allow (controls for each); no legacy gate runs; the move lands on both remotes | T0, R-0563, R-0565, owner: approval of the move inventory |
| R-0565 | Take the website down at the move (decision row P9, 2026-11-05) | the site's build and Pages deployment stop in the move's change; the published site is unpublished as a documented manual step whose moment the owner confirms (an outward action, never automatic); no frozen playground is served; the old site's sources stay archived under `legacy/` with a `RETENTION.tsv` row | R-0564 |
| R-0563 | Operational migration acceptance test (T1), before the move | T0 is on main; on a throwaway branch holding the exact proposed day-one root, a fresh clone builds the candidate (after `train.sh build`, so the candidate tree is what builds) with the candidate's own landing tooling, hooks, runs exact-SHA CI and passes a dry-run landing; negative controls (a broken build, a failing hook gate, a failed required CI job, a missing, skipped or cancelled required check, a SHA mismatch) are each refused for the expected reason at the expected SHA; a check that cannot run on a fallback runner is reported incomplete and blocks landing, never passed; the two-remote landing is proved by the move itself | R-0568, owner: approval of the move inventory |
| R-0564 | A pinned, reproducible legacy oracle, independent of `legacy/` | `legacy-compiler-final` builds in an isolated checkout of the tag from a pinned toolchain (`scripts/oracle/ORACLE.lock`) into a recorded digest; the build is published as a release artifact whose identity and SHA-256 are in the lock, fetched and verified by digest on any runner, cached on concrete-ci-2 and rebuildable from the tag alone with a hash check; a missing or mismatched artifact makes the oracle check incomplete, which blocks landing; runner criteria (decision row P13, owner 2026-11-05: concrete-ci-2 and GitHub-hosted runners as a regular second pool): the lock records the release tarball's SHA-256 with the toolchain and flake-lock hashes; landing and oracle runs use separate pools with per-pool CPU and memory limits and at most two heavy runs per machine; jobs that need ci-2's caches run there, and the build, the spec, the gates and the oracle diff also run on the GitHub-hosted x86_64-linux pool, which takes them over when ci-2 is down; a check neither pool can run is incomplete and blocks landing, and a heavy campaign the GitHub pool cannot run is recorded as skipped, never passed; self-hosted runners never run pull-request jobs; a weekly from-scratch rebuild matches the digest; the differential harness invokes it by digest when the new compiler changes; an unexplained difference fails, and the oracle's agreement never overrides the Lean semantics | — |
| R-0547 | Measurement: per-branch stage timestamps and the median time to land | every landing has its four timestamps, and the median shows here | — |
| R-0544 | Review discipline: per slice, delta after the first pass | median ≤ 5 rounds; a slice past 5 is split | — |
| R-0545 | Branch discipline: one bug or one rule per branch | a month of landings with no unrecorded bundling | — |
| R-0543 | Load control: at most 2 heavy local runs per machine | a third heavy run waits (control) | — |
| R-0542 | Generated ledgers with a single hand-edited source | two branches adding records rebase without conflicts | — |
| R-0540 | CI tiers: a landing tier for merge, campaigns on schedule | landing tier median ≤ 40 min over 10 runs | — |
| R-0539 | Landing trains through `scripts/land/train.sh` | three trains of two or more branches land, median under 2 h | R-0542, R-0540 |
| R-0541 | Self-hosted runners never for `pull_request`; heavy runs on the servers | the workflow check and its negative control | R-0543, R-0540 |
| R-0513 | Gates that are trustworthy and affordable: inventories over counts, impact-selected pre-push | pre-push median ≤ 10 min, and no gate failure first seen in CI over 20 landings | R-0547 |
| R-0546 | Build and compile caches, after profiling the slowest gates | a second run takes at most half the first; changing an identity invalidates the cache (control) | R-0547 |

## Development throughput

| measure | target |
|---|---|
| Codex-clean to main | median under 2 h |
| landing CI, dispatch to conclusion | median ≤ 40 min |
| local pre-push | median ≤ 10 min, and no gate failure first seen in CI over 20 landings |
| ledger conflicts on rebase | zero |
| review rounds per slice | median ≤ 5 |

## Implemented, not yet accepted

Generated from [`docs/project/candidates.tsv`](legacy/docs/project/candidates.tsv) by
`scripts/gen/candidate_table.py --write`; `check_docs_drift.sh` fails when they differ.

<!-- BEGIN GENERATED: candidate record (scripts/gen/candidate_table.py; edit docs/project/candidates.tsv) -->
| candidate | bugs | deliverable | branch | candidate SHA | exits | stage (a) | stage (b) | stage (c) | unlocks | remaining acceptance step |
|---|---|---|---|---|---|---|---|---|---|---|
| rocksdb-slice1 | — | RocksDB slice 1 groundwork (R-0285) | `rocksdb-slice1` | `094e51bf3258` | blocked by 0b (incl. bug 102), 2i, 2q | not run | not run | not run | RocksDB slice 1 acceptance, then the four-adapter checkpoint | 2026-11-02: paused until its constructs are supported by the new compiler. landed on main at 094e51bf but not accepted: exits 0b (bug 102), 2i and 2q; G1 (declaring the native library, so it builds with no manual flags) closed by `[native]` ([NATIVE_LIBRARIES.md](legacy/docs/platform/NATIVE_LIBRARIES.md)); G3 closed (bug 103 fixed on main by `0c507fb9`, verified 2026-10-21) |
| v1-design-examples | — | V1 design examples (R-0511, row 3) | `v1-design-examples` | `03d2edec1d21` | none (isolated proof development) | not run | not run | not run | V1a's Core interfaces freeze | 2026-11-02: research input for phase A, not an accepted compiler deliverable. landed on main at 03d2edec but not accepted: review of the design questions, then stages (b) and (c) |
<!-- END GENERATED: candidate record -->

## Releases

Nightlies publish the newest fully qualified main commit with no support promise. After 0.1, release
trains aim for one release every 4–6 weeks when the candidate passes. A security or soundness fix may
ship out of band, and a skipped train records its reason. Milestones are acceptance scopes, not
dates. A task is assigned to a release as a bounded increment, and completing that increment does
not close the task.

R-0333 keeps one versioned record per candidate. The record holds:

- the exact source commit and the compiler, toolchain and released-artifact identities;
- the supported language and std subset, platforms, backends and runtime profile;
- the required checks and their results at that SHA, with positive and rejecting controls,
  applicable mutation qualification and a clean installation;
- source, std API and ABI, and evidence-schema compatibility, each stated separately, with the
  migration, invalidation and revalidation each change needs;
- open blockers, accepted limitations and deferred increments, as three separate lists with owners;
- the matching reference, claims matrix, tutorial and release notes, which distinguish enforced
  properties, checked proofs, assumptions, tests and incomplete coverage;
- the supported lifetime, the patch procedure, security reporting, rollback, and evidence
  revocation and rechecking.

The following rules apply to every candidate:

- A timeout, a skipped job or an incomplete report is never a passing result.
- An accepted limitation never contradicts an advertised guarantee.
- An unresolved soundness failure inside the claimed scope blocks the release.
- A patch release has its own candidate and record, and repairs an already published scope.
- Stabilization never silently widens the claimed subset.
- Each later scope inherits the earlier acceptance checks that apply, rerun on its own candidate.
  It never inherits the legacy compiler's release scope.

### Release map

| release | phase | acceptance |
|---|---|---|
| 0.1 | C | file-copy and its failure paths build natively with their theorems; subset, exclusions and LLVM assumptions published |
| verified preview | E | each advertised theorem holds with its residual assumptions; native claims use the LCB path |
| verified-component alpha | E–F | a non-author repairs a bounded component and replays its evidence independently |
| project beta | G | multi-package use exposes inherited assumptions, evidence and compatibility |
| 1.0 | G | whole-language slices, E–F acceptance, conformance, the reference, compatibility, distribution and maintenance commitments hold for the named subset |
