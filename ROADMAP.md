# Concrete Roadmap

Concrete is a systems language with linear ownership, second-class references and explicit
capabilities. Its specification is a Lean library, `ConcreteSpec`: readable definitions with
plain-words doc comments and conformance examples. A new compiler is built from that library, proof
first, one slice of the language at a time, and each slice states the exact scope of its theorem.
Slice 1's theorem covers its exact source down to BIR. Its native output is qualified by tests and
labelled unverified; any claim about the native image is conditional on the LLVM, linker and image
assumptions it lists. A proof that reaches native code is later work (phase E), not the first
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
its reasons, or says which part still awaits the owner; this file holds no rule of its own. Language rules are not decided here: they are the owner decisions in [decisions.md](docs/spec/decisions.md#owner-decisions).
The design rules (one form per situation, one owner per fact, the most transparent long-term option,
verbosity that buys clarity) get one home in `PHILOSOPHY.md` when the `wip/philosophy` branch lands.

1. **The owner decides the language.** No agent, orchestrator or reviewer chooses, encodes or ships
   any side of an open language question. That includes doing so inside a fix, a test or a document.
   Agents bring options and a recommendation. An answer counts only when the owner gives it
   explicitly, and whoever records it writes its date in [decisions.md](docs/spec/decisions.md)
   ([recording rule](docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-02--owner-recording-rule),
   [AGENTS.md](docs/project/AGENTS.md#rules)).
2. **Lean is the specification.** Each rule is a Lean definition with its rule ID, a doc comment in
   plain words and its conformance examples. The normative definitions live in one main file, and
   proofs and tooling live in files that import it (R-0560). No prose is generated from Lean. Machine
   views (the rule registry, IDs and manifests) are derived and have a staleness gate. The user guide
   teaches and links the rules, and states none of its own. Prose chapters are retired as their rules
   land in Lean
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

   The compiler imports it and builds a proved lowering chain below Core. Slice 1 gets a BIR → LLVM
   printer, labelled unverified. The Lean Compiler Backend follows
   ([decision](docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)).
4. **The old tree is legacy.** The owner chose a `legacy/` folder on main on 2026-11-04; that
   choice is recorded with this roadmap in the changelog. The details below are the restart
   proposal (decision rows P5–P7 and P21), pending the owner's approval, and change if it changes.
   In one train, everything old moves under `legacy/`: the compiler, its scripts, tests, examples, std, proofs, gates, docs, research, site
   and bug ledger. `legacy/` is a byte-frozen snapshot of main's tree at the move commit. It is never
   patched and never run. One root check refuses any modification or addition under it, and any
   deletion except one its retention row (below) allows. Its gates are not
   re-run: they were qualified at `legacy-compiler-final`, and its evidence stays replayable from that
   tag. The differential oracle is built from the tag in its own isolated checkout, not from
   `legacy/` (R-0564). The infrastructure for the new root is prepared and rehearsed before the move
   (R-0563), and the move comes after (R-0558). The root starts minimal. A file comes back from
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
   3. One minimal end-to-end path, each pass preserving behaviour and invariants.
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
   ([promotion pipeline](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline)). A faster path never
   reports a check it did not run as passed.

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
| A. Core and theorem | Dispose of the spec audit. The owner decides the whole Core shape before implementation. State the semantic contract and the end-to-end theorem in Lean, and list the trusted base. | Core approved. Representative design proofs checked in Lean before Core freezes. The theorem, its holes and the trusted base on main. |
| B. Slice 1 | `ConcreteSpec` and the new compiler for slice 1, from parsing to a native build: scalars, `let`, the move and use-after-move refusal of an empty struct without `Copy` consumed by a struct-destructuring `let`, one checked operation, and a return or a defined trap. | The slice's rules are inductive judgments, independent of the checker and the elaborator, with proved correspondence. Every judgment, semantics and pass is total. Fuel exhaustion is distinct from divergence. The theorem, exact-source component correctness down to BIR, is proved. The native build is qualified by tests, not proved: the LLVM output is labelled unverified, and its assumptions are listed with the lexer, model-translation and native-image obligations. A native-image claim, if any, is stated separately as conditional on those assumptions. Minimal provenance accompanies the first authoritative proof claims. The examples pass through the Lean semantics and the native build. The outside-reader test passes on the Lean rules. Then the old pipeline is deleted from `legacy/`. |
| C. File-copy | Slices ordered by what a file-copy program needs: directories, `File`, `with(Fs)`, fallible operations, explicit cleanup on every error path, `defer`. Std, examples and tests come back as their constructs land. | The file-copy program, failure paths included, builds natively with its theorems. **0.1 is cut** with the supported subset, the unsupported constructs and the LLVM assumptions stated. |
| D. Whole language | The remaining constructs as slices. The paused workloads resume as their constructs land. | Every construct of the decided language is a slice with Lean rules, and the conformance suite is complete. |
| E. Verified growth | Proof-order items 4–8, the Lean Compiler Backend native path, the verified parser, typed contracts, frame reasoning, termination proofs and the proof library. Contracts have no source `old` and no snapshot construct (OD117): a postcondition reaches the entry state only through an explicit Lean theorem. Spec functions are total (OD115). An ordinary function can get its own termination proof, and there is no total-function category (OD116). | Each item's theorem holds, with its residual assumptions stated. |
| F. Evidence as a product | Provenance, the canonical verification task, receipts and an independent consumer, all on the new Core. | Receipt v1 frozen. |
| G. Release and ecosystem | External users, flagships, packages, editor, incremental builds, 1.0. | External-user acceptance, package evidence, conformance, compatibility and maintenance commitments hold for the released subset. |
| T. Development throughput | Beside the sequence: the minimal root, then faster landing, review and CI. | The [targets](#development-throughput) hold. |

## The queue

**Active:** T1 and A1. **Next:** T2 and A2.

| row | goal | tasks | done when |
|---|---|---|---|
| T1 | Prepare and rehearse the restart before anything moves | R-0563, R-0564 | The new root's CI and `required-checks.txt` are on main. On a throwaway branch holding the day-one root, a fresh clone builds, hooks, runs exact-SHA CI and passes a dry-run landing, and the negative controls are refused. The legacy oracle is pinned and reproducible from the tag. |
| T2 | Move the old tree to `legacy/` in one train | R-0558 | The root holds only the day-one set. `legacy/` is byte-frozen and checked. The move itself lands on both remotes. |
| A1 | Dispose of the spec audit and draft the boundaries Core must satisfy | R-0555, R-0312, R-0316 | Each audit finding, gap and duplication has an owner and an acceptance artifact. The [boundary drafts](docs/compiler/A1_BOUNDARIES.md) exist. |
| A2 | Decide the whole target Core shape | R-0511, R-0559 | An outside reader has read the [Core proposal](docs/compiler/CORE_PROPOSAL.md) and their confusions are resolved. The design proofs are checked in Lean. The owner approves the proposal. A3 sends it back if it exposes a substantive change. |
| A3 | State the contract, the theorem and the trusted base in Lean | R-0560, R-0496, R-0502, R-0506, R-0508, R-0509, R-0500 | The one-main-file layout and its gate exist. The theorem, with its holes, the named values-to-bytes boundary and the trusted-base inventory, is on main. |
| B1 | Build `ConcreteSpec` and slice 1 with its theorem | R-0425, R-0499, R-0492, R-0493, R-0514, R-0501, R-0503, R-0497, R-0504, R-0507, R-0521, R-0552, R-0528, R-0479, R-0118 | Slice 1 runs from parsing to native code. Its judgments are independent, its correspondence and soundness are proved, its obligations are explicit and its provenance is minimal. Its conformance examples pass on both implementations. |
| B2 | Qualify slice 1 | R-0491, R-0498, R-0272, R-0550 | Differential (Lean semantics, native build, legacy oracle), refusal and gate-integrity controls hold. The fuzzers target the new compiler. The outside-reader test passes. |
| B3 | Audit totality, then delete the old pipeline | R-0553, R-0562, R-0561, R-0565 | The totality audit finds no partial definition. Every `RETENTION.tsv` row for the old pipeline is resolved. Legacy defects are retired with the old implementation. The browser compiler is served from a tag-built bundle until the new compiler covers it. The old pipeline is gone from `legacy/`; retained assets stay there until their rows resolve. |

<a id="questions-for-the-core-session"></a>
### Questions for the Core session

The owner decides these in the A2 session. The [Core proposal](docs/compiler/CORE_PROPOSAL.md) gives
each one options and a recommendation, and no work encodes an answer first:

1. allocator discipline;
2. `destroy` bounds;
3. the `HeapArray` element view and the raw-memory taxonomy;
4. the type of static text;
5. whether capability and type arguments are inferred or written (a proposed revision of OD45 and OD87);
6. static refusal versus traps;
7. target grant schemas and narrowing;
8. slice 1's assurance scope;
9. the whole Core interface.

## Tasks

One row per live task: what it delivers, when it is done, and what it waits for. "Depends on" names
tasks (R-NNNN), queue rows (A2: every task of that row), whole phases (C: every task of phase C) or
an owner decision (`owner: <decision>`), comma-separated; `—` is nothing. A task transferred
into one of these carries the obligations recorded in its row of the
[disposition table](docs/archive/roadmap-2026-11/task_disposition.tsv). The full old body stays in the
archive. Rows for C–G are summaries. Each is refined when its phase becomes next.

### A. Core and theorem

| task | objective | done when | depends on |
|---|---|---|---|
| R-0555 | Dispose of the 2026-11-02 spec audit, then keep one normative spec in Lean with independent judgments and rule-indexed conformance | every audit row has an owner; each slice's Lean rules replace the prose they cover, and bug-to-rule references resolve | — |
| R-0312 | The unsafe-surface and raw-memory taxonomy: validity, provenance, aliasing, ownership transfer | taxonomy decided in the Core session (question 3) and stated in Lean | R-0555 |
| R-0316 | An unsafe-aliasing policy without hidden reference UB | the policy is part of the approved Core | R-0312 |
| R-0559 | Outside-reader dry run on the Core proposal | a reader new to Concrete reads the proposal; each confusion is recorded and resolved before the owner's decision | R-0555 |
| R-0511 | Representative design proofs before Core freezes, then cumulative slices with honest theorem scopes | Lean proofs checked for allocator dependencies with reset generativity, deferred cleanup with allocators and loans, heap opening and mutable lending, and one values-to-bytes lemma; each later slice states its theorem scope | R-0559 |
| R-0560 | One-main-file spec layout: every normative definition in `ConcreteSpec/Spec.lean`; proofs, tooling and derived views in files that import it | the layout exists; a gate refuses a normative definition outside the main file, with a negative control | A2 |
| R-0496 | The semantic contract (outcomes, traces, authority, resources, the error model) and the clock-indexed evaluator | the contract is stated in Lean; the evaluator runs; fuel exhaustion is distinct from divergence | R-0560 |
| R-0502 | One value/raw-memory boundary and one layout authority | the boundary is named in the theorem; layout has one owner with its proofs | R-0496 |
| R-0506 | Every proof boundary and validator obligation stated with its premises and status | obligations have explicit premises; transitive axiom classification is enforced | R-0496 |
| R-0508 | The end-to-end theorem: maximal-behaviour refinement, frontend correspondence, resource adequacy, image transport | stated in Lean with its holes; proved progressively per slice | R-0496, R-0502 |
| R-0509 | The trusted-base inventory, evidence binding, assumption lifecycle and axiom replay | the inventory is on main; every claim's axioms are recorded and replayed from a clean checkout | R-0506 |
| R-0500 | Each decided language rule gets its Lean judgment, semantic clause and accepted, refused or trapping examples | each rule lands with its slice; every rule of the decided language has one by the end of D | R-0560 |

### B. Slice 1

| task | objective | done when | depends on |
|---|---|---|---|
| R-0425 | The conformance format and suite: each example names its rule, source and outcome (result, refusal code or trap), and one runner per implementation runs it through the Lean semantics and through the compiler | slice 1's examples pass both runners; the suite is versioned and published at 0.1, and complete at D | R-0560 |
| R-0499 | Independent Core judgments and an authoritative checker proved to correspond | correspondence proved for slice 1, with positive and rule-breaking mutation controls | R-0500 |
| R-0492 | Scope-resolved identities throughout; lexical identity distinct from persistent evidence identity | alias, symbol, binding and forgery controls hold | R-0499 |
| R-0493 | Typed arithmetic and casts from one semantics, with arithmetic-site facts | widths, signedness, modes and the defined trap match the Lean semantics | R-0496 |
| R-0514 | The compiler consumes the exact identity-resolved Core payload checked against the judgments | substitution controls hold | R-0499 |
| R-0501 | Proved lowering passes for sequencing, matching, cleanup and check expansion | each pass preserves behaviour and re-establishes invariants | R-0514 |
| R-0503 | The unverified BIR → LLVM path, the native driver and the pinned LLVM toolchain | output labelled unverified; LLVM, linker and runtime assumptions listed; the driver bounds external tools | R-0501 |
| R-0497 | Immutable pipeline artifacts bound to exact payloads | forgery, mutation and bypass compile-fail controls | R-0514 |
| R-0504 | Validators with explicit soundness propositions | substitutions and wrong artifacts are rejected without inflating native assurance | R-0497 |
| R-0507 | Type and ownership soundness over the executable semantics | proved for slice 1 | R-0496, R-0499 |
| R-0521 | Minimal provenance and one canonical verification task for the first authoritative claims | a claim names its compiler, source, theorem and executable digest | R-0509 |
| R-0552 | Per-compilation certificates (owner-approved 2026-10-25) | a build emits a certificate per output, and the kernel checks it | R-0504 |
| R-0528 | Resource limits on input size, memory, time and import closure | each input class has a limit and a hostile fixture | R-0503 |
| R-0479 | Failures and refusals are never turned into facts; diagnostics name the valid next action | refusals survive through results, text, JSON and exit codes | R-0499 |
| R-0118 | Generated coverage of passes, consumers, constructs, operations and traps | a missing row or a synthetic constructor fails the gate | R-0501 |
| R-0491 | The defect inventory and the classified differential (Lean semantics, native build, legacy oracle) | comparisons are classified and campaigns are non-vacuous | R-0425, R-0564 |
| R-0498 | Every enforcement claim maps to its gate leg and a targeted mutation | an unmapped claim fails; shape checks are labelled unproved | R-0118 |
| R-0272 | Restart fuzzing against the new compiler on concrete-ci-2, seeded from the archived ci-3 corpus, with reduction and counterexample-to-regression persistence | the generators emit slice-supported programs, the oracle is Lean semantics against native code, and findings become regressions; legacy fuzzing stopped at the move | B1 |
| R-0550 | Compiler scaling and speed baselines at exact SHAs | scaling gates hold for each stage, and a planted regression is caught | B1 |
| R-0553 | Totality audit: every semantics, checker, validator and pass is total and runs as proved | no partial definition; opaque execution gaps controlled | B2 |
| R-0562 | The retention manifest `RETENTION.tsv` at the root, written before the move and checked before any deletion under `legacy/` | every legacy asset (corpora, regressions, examples, std, the bug ledger, the site, docs and the archived services) has a row with a destination and a deletion condition; deleting a path under `legacy/` whose row is unresolved is refused (control); the manifest and the task disposition table stay at the root and survive B3 | — |
| R-0561 | Bug ledger by compiler | legacy defects are marked retired with the old implementation, never fixed, each keeping its reproducer, ruling and structural class; those the browser compiler still shows stay visible while the site serves it; std defects move with std as contracts and regressions; every legacy ruling that states language behaviour becomes a conformance example or a recorded owner question; the minimal issue record of the day-one root becomes the new ledger with the first slice | R-0425 |
| R-0565 | Website transition | before the site stops rebuilding, a browser bundle built and verified from `legacy-compiler-final` (parity with the native oracle, loader and UI checks) is deployed, labelled as the frozen compiler for the language before the 2026-10-29 to 2026-11-01 decisions, with a documented redeploy; links to main paths point at the tag; the site later builds from the new tree and switches when the new compiler covers it; no page describes the legacy compiler as current | R-0564 |

### C. File-copy and 0.1

| task | objective | done when | depends on |
|---|---|---|---|
| R-0052 | Handle-relative filesystem: directories and files carry authority | file-copy opens, reads, writes and closes through directory values | B |
| R-0557 | Global state as capabilities: no duplicable linear constants; `main` receives explicit target grants | grants reach `main`; constant linearity is enforced | B |
| R-0023 | The decided entry-point signatures and explicit exit and error behaviour | implemented and tested in the new compiler | R-0557 |
| R-0071 | Std on the new compiler: modules return when their constructs land, against the completion contract | each returned module has its manifest, coverage and error conventions | R-0052 |
| R-0556 | Generated module interfaces and the interface-diff gate | every public API change shows in the diff | R-0071 |
| R-0510 | Checked specialization | specialization is validated against the generic definition | B |
| R-0162 | Reusable lemmas for structs, enums, matching, `Result` and `Option` | file-copy's theorems use them | B |
| R-0301 | Backend contracts: ABI, layout, sanitizer and FFI controls | the validation project passes for the 0.1 subset | R-0503 |
| R-0334 | The first-user teaching path (the guide) | a checked tutorial covers install to file-copy | R-0071 |
| R-0516 | Release automation for nightlies and trains | a nightly publishes from a qualified SHA | T |
| R-0333 | Release criteria and candidate records, including claims, security and soundness disclosure, compatibility and operations | the 0.1 candidate record is complete | C |

### D. Whole language

| task | objective | done when | depends on |
|---|---|---|---|
| R-0137 | Workload validation: each workload records its attempts, diagnostics and measurements | the dashboard covers every supported workload | C |
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
| R-0517 | The Lean Compiler Backend as the native backend | the native path holds its theorem with residual assumptions | R-0508 |
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
| R-0558 | Move the old tree to `legacy/` in one train and set up the minimal root. Day one, the root holds the README, this file, AGENTS.md, an empty CHANGELOG, LICENSE, the new build files, `ConcreteSpec/Spec.lean`, `docs/PHILOSOPHY.md`, `docs/decisions.md`, the landing, review and hook scripts, `required-checks.txt`, `RETENTION.tsv`, the minimal issue record and the [2026-11 roadmap archive](docs/archive/roadmap-2026-11/README.md) with its task disposition table. Everything else comes back from `legacy/` when a slice needs it. The status page, the work-queue dispatcher, the fuzz farm, perf tracking and the nightly campaign stop, and each is listed in `RETENTION.tsv` as archived; each returns only with a roadmap task. | the root holds only that set; a root check refuses any modification or addition under `legacy/`, and any deletion its `RETENTION.tsv` row does not allow (controls for each); no legacy gate runs; the move lands on both remotes | R-0563, R-0562, R-0565, owner: approval of the move inventory |
| R-0563 | Operational migration acceptance test, before the move | the new root's CI and `required-checks.txt` land first, and both `train.sh watch` and `train.sh land` refuse a run that lacks a named check; on a throwaway branch holding the day-one root, a fresh clone builds, hooks, runs exact-SHA CI and passes a dry-run landing; negative controls (a broken build, a failing hook gate, a red CI, a missing or skipped required check) are each refused; a check that cannot run on a fallback runner is reported incomplete and blocks landing, never passed; the two-remote landing is proved by the move itself | owner: approval of the move inventory |
| R-0564 | A pinned, reproducible legacy oracle, independent of `legacy/` | `legacy-compiler-final` builds in an isolated checkout of the tag from a pinned toolchain (`scripts/oracle/ORACLE.lock`) into a recorded digest; the build is cached on concrete-ci-2 and rebuildable from the tag alone with a hash check, independently of ci-2; a weekly from-scratch rebuild matches the digest; the differential harness invokes it by digest when the new compiler changes; an unexplained difference fails, and the oracle's agreement never overrides the Lean semantics | — |
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

Generated from [`docs/project/candidates.tsv`](docs/project/candidates.tsv) by
`scripts/gen/candidate_table.py --write`; `check_docs_drift.sh` fails when they differ.

<!-- BEGIN GENERATED: candidate record (scripts/gen/candidate_table.py; edit docs/project/candidates.tsv) -->
| candidate | bugs | deliverable | branch | candidate SHA | exits | stage (a) | stage (b) | stage (c) | unlocks | remaining acceptance step |
|---|---|---|---|---|---|---|---|---|---|---|
| rocksdb-slice1 | — | RocksDB slice 1 groundwork (R-0285) | `rocksdb-slice1` | `094e51bf3258` | blocked by 0b (incl. bug 102), 2i, 2q | not run | not run | not run | RocksDB slice 1 acceptance, then the four-adapter checkpoint | 2026-11-02: paused until its constructs are supported by the new compiler. landed on main at 094e51bf but not accepted: exits 0b (bug 102), 2i and 2q; G1 (declaring the native library, so it builds with no manual flags) closed by `[native]` ([NATIVE_LIBRARIES.md](docs/platform/NATIVE_LIBRARIES.md)); G3 closed (bug 103 fixed on main by `0c507fb9`, verified 2026-10-21) |
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
