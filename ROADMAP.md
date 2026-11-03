# Concrete Roadmap

**What Concrete is aiming at.** Concrete is a systems language with linear ownership, second-class
references and explicit capabilities whose compiler is to be verified end to end: CompCert/CakeML-class
theorems that accepted programs are memory- and ownership-safe, that the native binary only does what
the program's Core semantics allows, and that supported claims about Core transport to the native image.
Its evidence (proofs, receipts, trust and assumptions) is to be compositional, transferable and
independently replayable ([verified-compiler decision](docs/project/ROADMAP_DECISIONS.md#decisions), [product north star](docs/project/ROADMAP_DIRECTION.md#product-north-star-compositional-transferable-evidence)).

**The specification comes first (owner decision, 2026-10-30).** The criteria below are delivered in phases A–D under [the scheduling rule](#scheduling-rule). The spec is complete when it
is the one normative, simple and checkable statement of the language. In order:

1. **Owner decisions.** Every open question is answered. They are the OPEN entries in
   [docs/spec/decisions.md](docs/spec/decisions.md), and the ones the spec work has raised since land
   with it. Each carries a reviewed recommendation, the principle it rests on and its proof impact.
   Until it is answered, the spec says "undecided" there. **Done when:** no OPEN entry remains, except
   ones the owner defers explicitly.
2. **Absorb and audit.** Every language-normative statement in the repository moves into `docs/spec`,
   tracked by the absorption inventory, so there is one source. Coverage is then audited against the
   implementation, because absorbing only finds rules someone wrote down: every construct the parser
   and checker accept has a rule. Project policy, process and research stay where they are.
   **Done when:** the inventory has no unabsorbed row, and the coverage audit lists no construct
   without a rule.
3. **Simplify.** The decided removals are applied to the text, and the rules they make obsolete
   are retired by version, not silently deleted.
4. **Read as one document.** One generated single-page spec, a short overview, one shape for every
   rule (statement and examples, plus its diagnostic code where the rule is a refusal), the grammar
   in the spec, a glossary, and an index from diagnostic codes to rules.
5. **Nothing outside the spec.** Every diagnostic code that reports a language refusal maps to a rule,
   and every compiler site that enforces one cites it. A blocking gate, defined over the diagnostic
   ledger and the rule-site table, fails on an unmapped language refusal. Semantic guarantees and
   operational failures are classified separately, as `docs/spec/README.md` already does.
6. **Conformance suite.** Every rule's examples state their expected outcome: accepted with a result,
   refused with a code, or trapping. They are published as a suite another implementation could run.
   **Done when:** every rule has its examples. Pending rules are expected to fail until step 7.
7. **The code matches the spec.** Std, examples and tests migrate as their constructs land, with an owning task for every pending rule.
8. **Lean links.** Each chapter's rules link their Lean definition (`Semantics:`), and the definition
   governs. They arrive chapter by chapter with the Core semantics (R-0496), each together with its
   implementation, and prose, Lean and the implementation must agree on the chapter's examples.
9. **Outside-reader test.** Someone new to Concrete writes a working program from the spec alone,
   and whatever confused them is fixed.

**Authority design (owner decision, 2026-11-01; rules completed 2026-11-02).** `with(...)` is the
only permission mechanism; it is compile-time information, not a resource. Linear values are the only
resource mechanism. Every operation needs both: the value says which thing, and `with` says what is
allowed. The target's services are values too: `main` receives the target's grants (its allocator,
directories, environment, clock, devices) and consumes them. There is no `Root` type, no permission
token and no `Std` capability; every signature lists its real capabilities. Allocation takes an
`Allocator<A>` value on every target and is fallible; allocations carry their allocator's identity;
filesystem access goes through `Directory` values confined at lookup. The fourteen authority rules and
the allocation, filesystem, environment, time and network decisions are in
[docs/spec/decisions.md](docs/spec/decisions.md#decided-2026-11-02). The spec's trusted, types, memory
and ownership chapters are to be rewritten to them in Lean, and std, examples and tests are to
migrate to them.

**Then the documentation follows the spec.** Once the spec is rewritten, every other document (the
book, the site, the README, the project, compiler and platform docs, and the research notes that state
language facts) is updated to cite the new spec and lose any statement it now contradicts. The
absorption inventory and the doc-links and docs-drift gates track this: no document may restate a rule
the spec owns, or describe a removed feature as current.

**Lean is the only source of the language (owner decision, 2026-11-01).** This is the governing rule for phases A–D. It changes how steps 3, 4, 6 and 8 are carried out, not what they require:
every done-when criterion of steps 1 – 9 still holds. It supersedes the rewrite plan recorded earlier the
same day, which kept a prose source beside Lean.

- **Core in Lean from the start.** The normative language is a small Core, roughly 20 – 30
  constructs. Its syntax, its typing, ownership and effect rules, and its execution semantics are Lean
  definitions, each carrying its rule ID and a doc comment. There is no Markdown-first draft: existing
  chapter text is rewritten as Lean, not edited as prose. Rules stated on surface syntax today, such as
  the ownership checks, become Core rules (R-0499's checker on Core).
- **Desugaring in Lean.** Every surface feature (counted loops, method calls, `defer`, pattern forms,
  and so on) is defined by its lowering into Core, written as Lean functions, with no semantics of its
  own. Core's metatheory is shared by every feature, and each lowering carries its own correctness
  obligation.
- **Everything else is generated from Lean** (the WebAssembly SpecTec model). A Lean program generates
  `SPEC.md`, the chapters and the rule registry; a blocking gate fails when they are stale. The
  reference interpreter is the Lean semantics itself, compiled and run. It replaces `--interp` once it
  passes the existing interpreter-versus-compiled differential corpus. The conformance
  examples live next to their rule and run through both the Lean semantics and the compiler.
- **The compiler is proved against the same definitions.** R-0496's Core semantics is this Lean Core,
  so the spec, the reference interpreter, the conformance oracle and the verified-compiler theorems
  share one semantic definition. What remains are explicit correctness obligations at the translation
  and generation boundaries: desugaring, the generator, the compiler and linking.
- **Documentation follows from Lean.** The book, the site, the README and the compiler and platform
  docs cite the generated spec and state no rule of their own.
- **Implementation sequence**, one step at a time:
  1. record the owner decisions (step 1);
  2. Core in Lean: syntax, typing, ownership and effects, execution semantics;
  3. the desugaring of each surface feature in Lean;
  4. the generator and its staleness gate;
  5. retire the old chapters area by area, each replaced by its generated output;
  6. the documentation follows ("Then the documentation follows the spec", above), area by area as
     each chapter is replaced;
  7. the migration of std, examples and tests (step 7).

  Proposals 1 and 3 of [SPEC_STRUCTURE_PROPOSALS.md](docs/project/SPEC_STRUCTURE_PROPOSALS.md)
  (grammar productions in the entries; the conformance suite) are folded into steps 2 – 4.

**A new compiler, written from the Lean spec (owner decision, 2026-11-01).** The current compiler
is not migrated. It implements the language before the decisions of 2026-10-29 – 11-01, and its
recurring defect classes (spelling-based names, places mixed with values, checks spread across
passes) are structural. A new compiler is written from the Lean definition, proof-first, and the
current one is retired. The owner: "i don't care about the current pipeline, we could remove it and start
from scratch", then approved the transition below (freeze, tag, oracle, deletion after slice 1, the target
Core proposal first). This changes the implementation strategy only: the Lean-only source decision above
still governs, and no pending language question is settled by it. The architecture of `ConcreteSpec` and
the new compiler (below) was approved on 2026-11-02 under the owner's rule of that day
(Claude and Codex agreeing); the conditions are in phase B's done-when.

- **`ConcreteSpec`** is a new Lean library that imports nothing from any compiler. It holds the
  grammar as data (with its LL(1) check), Core with identities from the start, the semantic contract
  sized to each slice, the statics as inductive judgments (the only normative form; a decision
  procedure is an implementation, proved equivalent once a subset's rules stop changing), the one
  elaborator from surface to Core, the clock-indexed evaluator (the reference interpreter, which
  replaces `--interp`), the conformance examples and the generator of `SPEC.md`. The rule text is
  rendered from the judgment clause; doc comments are commentary and examples.
- **The new compiler** imports `ConcreteSpec` and calls its elaborator. Below Core it is a new
  lowering chain, built pass by pass with its proofs. A simple BIR → LLVM printer, labelled
  unverified, gives native code from the first slice; the Lean Compiler Backend follows. A
  conformance package above both runs every example through the spec and through the compiler.
- **When slice 1 works end to end, the old pipeline is deleted from main** in one change, together
  with the gates that test its internals. Kept: the test programs and bug regressions (re-run as
  slices cover them), examples and std (migrated to the decided language), the CI and gate framework,
  the docs and research, and the external native backend. Dropped: the old Core, checker,
  elaborator, lowering, SSA and LLVM emission, the old interpreter, the `PExpr`/ProofCore proofs, and
  the evidence, receipt and report subsystems, which are rebuilt on the new Core when proofs need
  them. Evidence the old compiler produced stays replayable from the tag; its
  issuance ends at the freeze and none of it is migrated (the
  [verified-compiler plan](docs/compiler/VERIFIED_PIPELINE.md)'s supersession notice). The browser compiler keeps running from the tag until the new compiler covers it.
- **What it supersedes:** the in-place migration of the current compiler in
  [VERIFIED_PIPELINE.md](docs/compiler/VERIFIED_PIPELINE.md) (its target architecture, semantic contract
  and proof strategy still apply, now to the new compiler); the migration of the compiler,
  which becomes its retirement (std, examples and tests still migrate); the old-pipeline items of
  the [legacy plan](docs/archive/ROADMAP_LEGACY_PLAN.md); and the unlanded cleanup branches of the old pipeline, which are to be
  closed.
**How the compiler proof is built (research reviews, 2026-11-01).** Two surveys of verified-language and
verified-compiler projects, the first reviewed by Codex (adopt with changes), fix the order in which the
[verified-compiler plan](docs/compiler/VERIFIED_PIPELINE.md) is carried out for the new compiler. Every
source is in [INFLUENCES.md](docs/project/INFLUENCES.md#verification-and-compiler-influences-research-of-2026-11-01):

1. **The semantic contract first.** The final theorem is stated before any pass is proved: which
   outcomes a program has (return, abort, observable I/O, divergence), what a trace is, how authority
   and resources appear in it, and the memory abstraction. Values-to-bytes is a named proof boundary,
   not an implicit step.
2. **Executable Core semantics.** The planned form is a step function, so it runs as the reference
   interpreter. Running out of fuel is an execution-budget result of the interpreter, distinct from
   every program outcome; its relation to divergence is stated with the theorem. Type, ownership and effect
   soundness are proved over it.
3. **One minimal end-to-end path.** Each pass preserves behaviour and re-establishes the next IR's
   ownership and authority invariants, so no later pass relies on a check an earlier one skipped
   (Linear Core). Safety is split as in Owlang: the checker proves the ownership invariant and
   compilation preserves it, the ownership conditions at module boundaries are stated as interface
   contracts, and every check runs before optimization.
4. **Functional models for safe code**, in the style of Aeneas: a supported safe function is
   translated to a pure Lean function, with `&mut` arguments passed in and returned, and failure,
   non-termination and effects explicit. Because a Concrete reference is never returned or stored
   (Hylo's mutable value semantics), the translation is simpler, and the supported subset may need
   none of Aeneas's backward functions; that each model is exact (effects, divergence, allocation
   failure and trusted boundaries included) is a theorem to prove. Either the translator is verified
   or every model carries its own proof. Contracts are stated over these models; the plan is to
   discharge them with Lean's own program-verification tools (`Std.Do`, `mvcgen`, `grind`) instead of
   building a project verification-condition generator (the old TermIR generator retires with the old
   compiler). This replaces ProofCore.
5. **Open-module correctness and the C boundary**, in the style of CompCertO: per-module theorems
   that compose into a whole-program theorem, with explicit contracts for C code.
6. **Ghost permissions for trusted code**, in the style of Verus, so selected `trusted` functions
   discharge their trust assumptions instead of carrying them.
7. **SMT-checked lowering rules** where they pay off, later.
8. **Later, an optimizer by equality saturation**: an e-graph search whose rewrite rules are proved Lean
   theorems and whose every result carries a kernel-checked proof that the output equals its input
   (as the Lean egg tactic reconstructs), so the search and the extraction stay outside the trusted
   base.

Three more choices follow from the surveys:
- **The parser is generated from the grammar** (Lean data, which must pass the LL(1) check) by a
  verified LL(1) generator in the style of Vermillion. The lexer, the semantic actions and running the
  generated parser remain obligations before the parser leaves the trusted base.
- **Capability variables** ([authority rule 3](docs/spec/decisions.md#decided-2026-11-02-authority))
  take Scala's System Capless, whose metatheory is mechanized in Lean, and Effekt's
  effects-as-capabilities as metatheory references; callbacks that carry capabilities follow once
  foreign function values are admitted (OD4).
- **Agent-written proofs, kernel-checked**, as in Axon: agents may write proofs and translation-
  validation certificates, and only the Lean kernel's check counts. Trivet does this for LLVM's own
  optimization passes; for Concrete it could cover a named edge such as the BIR → LLVM printer, with
  LLVM, the backend and the linker still assumptions.

The plan also covers the front end (parsing and desugaring into Core) and the final linked artifact,
and the spec is tested independently of the compiler, through the conformance suite run on the Lean
semantics. Items 4 – 7 are the adopted direction; their supported subsets, representations and
residual assumptions are proposed to the owner before each is built.

The criteria above map to acceptance artifacts as follows. R-0555 owns the spec criteria;
R-0515 owns the outside-reader test as workloads become supported.

| criteria | phases | acceptance artifact |
|---|---|---|
| 1–2: decisions, absorption and audit | A | decisions and audit disposition in the target Core proposal |
| 3–6: simplification, one document, refusal coverage and conformance | B–D | generated chapters, rule-site inventory and conformance examples per slice |
| 7–8: matching code and Lean links | B–D | migrated corpus and correspondence proofs per slice |
| 9: outside-reader test | B–D | recorded reader program and resolved confusion per slice |

## Where we are

2026-11-02: the legacy compiler is frozen at `legacy-compiler-final` at commit 066c9d9b8. The language decisions
are recorded; the shape of Core is open for the owner's target Core proposal decision.

**Where things live.** The [task register](docs/project/ROADMAP_TASKS.md) holds A, B and T tasks;
the [backlog](docs/project/ROADMAP_BACKLOG.md) holds later tasks and the pull rules.
The [spec](docs/spec/README.md) owns language rules; the
[verified-pipeline plan](docs/compiler/VERIFIED_PIPELINE.md) holds the target proof architecture.
The [direction](docs/project/ROADMAP_DIRECTION.md) holds standing reference and product goals.

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
| T. Development throughput | R-0539 – R-0547 and R-0513, beside the compiler sequence | the targets and item exits in [Development throughput](#development-throughput) hold |

## The queue

| row | goal | owning tasks | done when |
|---|---|---|---|
| A1 | Dispose of the spec audit and draft the boundaries Core must satisfy | R-0555, R-0481, R-0010, R-0447, R-0554, R-0511, R-0211, R-0230 | Each changed rule, gap and duplication has an owner and acceptance artifact; defect views distinguish compilers; drafts exist of the semantic contract's objects (outcomes, events and their payloads, resource and grant identities, admissible environments and allocation behaviour, observation equivalence, stuckness, trap erasure), the theorem's signature (acceptance and coverage premises so that refusing every program proves nothing, frontend correspondence, initial and final state relations, exact artifact binding), the assumption and trusted-base inventory, and slice 1's manifest (admitted syntax and Core forms, the checked operation, an accepted trap witness, observations, the proof endpoint and the LLVM assumption). |
| A2 | Decide the whole target Core shape before implementation | R-0312, R-0316 | The owner approves the target Core proposal, drafted against A1's artifacts, including identities, memory and interface boundaries; before Core is frozen, representative design proofs are checked in Lean for allocator dependencies with reset generativity, deferred cleanup with allocators and loans, heap opening and mutable lending, and one values-to-bytes lemma. If A3 exposes a substantive change, A2 is revisited. |
| A3 | Formalize the approved semantic contract and theorem in Lean | R-0496, R-0500, R-0502, R-0506, R-0508, R-0509, R-0480, R-0223, R-0227, R-0242, R-0464, R-0266 | The theorem with its holes, named values-to-bytes boundary and trusted-base inventory is on main. |
| B1 | Build ConcreteSpec and slice 1 with its theorem | R-0491, R-0492, R-0493, R-0497, R-0498, R-0499, R-0501, R-0503, R-0504, R-0507, R-0514, R-0552, R-0521, R-0528, R-0004, R-0435, R-0440, R-0058, R-0073, R-0074, R-0075, R-0076, R-0077, R-0079, R-0080, R-0082, R-0085, R-0090, R-0107, R-0114, R-0115, R-0116, R-0191, R-0203, R-0205, R-0207, R-0210, R-0217, R-0221, R-0222, R-0231, R-0260, R-0272, R-0273, R-0274, R-0278, R-0279, R-0280, R-0288, R-0290, R-0302, R-0421, R-0422, R-0446 | The phase B slice passes from parsing to native execution with independent judgments, correspondence proofs, explicit obligations and minimal proof provenance; every judgment, semantics and pass is total as it lands. |
| B2 | Generate the chapter and qualify slice 1 | R-0479, R-0471, R-0009, R-0438, R-0092, R-0093, R-0094, R-0095, R-0105, R-0106, R-0118, R-0550, R-0551, R-0425, R-0355 | Conformance and native examples pass; refusal, differential and gate-integrity controls hold; the outside-reader test passes. |
| B3 | Audit compiler totality and delete the old pipeline under the retention contract | R-0553 | The end-to-end slice is accepted, the totality audit finds no partial definition, the fuel-budget distinctions are established, and the old pipeline and its internal gates are deleted together; retained programs, regressions, examples, std, evidence replay and browser service follow the retention contract above; CI builds `legacy-compiler-final` as the differential oracle and the site's browser compiler is built from that tag until the new compiler covers it. |

<a id="questions-for-the-core-session"></a>
**Questions for the Core session (design review, 2026-11-03).** The
[design review](research/process/design-review-2026-11-03.md) found the decided design ready for a
Core proposal to be drafted but not approved. These are decided by the owner in the session of A2;
the proposal brings each with options and a recommendation, and no work encodes an answer first:

1. allocator discipline: what a brand is, fresh identities and reset in a loop, how a dependency on
   an allocator is tracked through nested containers, returned values and callbacks, where a heap
   records how to release its storage, and what `AllocFailure<T>` holds and "unchanged" means;
2. `destroy` bounds: storage release, payload destruction and resource cleanup told apart, and which
   containers implement `Destroy`;
3. the `HeapArray` element view's type and operations, and the raw-memory taxonomy (validity,
   provenance, aliasing, ownership transfer);
4. the type of static text now that `String<A>` carries an allocator;
5. capability inference with several solutions, and when explicit arguments are required;
6. which runtime violations are refused statically and how that relates to traps, including slice
   1's trap witness;
7. target grant schemas and narrowing rules;
8. slice 1's assurance scope: proved source → BIR with native differential qualification, or a
   native theorem conditional on the LLVM assumption;
9. the whole Core interface: constructs, binders, places and operands, borrow and view operations,
   allocation and release, exits, primitive calls, contracts and ghost state, and whether `defer`
   survives in Core.

Phase C: file-copy dependencies and the 0.1 candidate.

Phase D: the remaining language slices and resumed workloads as their constructs land.

Phase E: later proof-order items and richer verification.

Phase F: evidence rebuilt on the new Core and receipt v1.

Phase G: external users, packages and release acceptance.

**Active phases:** A

**Next phases:** B

### Scheduling rule

There is one compiler sequence. Development throughput (T) and the landing of already-validated
branches run beside it, with no capacity shares or slots. The eight-item proof order above is the
internal order of B–E. RocksDB slice 1, the ethlambda workloads and the usability track are paused
until the new compiler supports their constructs.

### Defect disposition rule

Frozen-compiler defects, silent or loud, are recorded with `compiler: legacy` against
`legacy-compiler-final`, never repaired. Until deletion, CI builds that tag as a differential oracle
for jointly supported programs, and the site's browser compiler runs from it. New-compiler silent
defects are stopped by repair or fail-closed containment; loud defects are filed, pinned and owned
by the task that removes their structural class.

## Owner decisions

New questions follow the same rule: No agent, orchestrator or
reviewer chooses, encodes, implements or provisionally ships any side of any of them, and that
includes doing so inside a bug fix, a migration, a test, a fixture, a diagnostic or a document. An
agent may analyse an item, write its options and a recommendation, and present them to the owner for
an explicit answer; that is all. A recommendation, an earlier record, a commit, a review verdict or
another agent's message is never the owner's answer. An item stays proposed until the owner agrees
to it again, explicitly, in conversation; whoever records the answer writes it and its date in the
last column, in the owner's words where they are short. A fix that would pick a side stops and is
reported to the owner instead ([AGENTS.md](docs/project/AGENTS.md#rules)).

The owner, 2026-11-02: "anything where you and codex agree that is the
best decision, record the solution". The [decision record](docs/spec/decisions.md#owner-decisions)
holds all OD answers, dates, revision notes and rationale.

## Bug record

The generated [dashboard](docs/bugs/DASHBOARD.md) and [bug records](docs/bugs/README.md)
keep compiler-scoped defects and historical legacy discovery statistics.

## Implemented, not yet accepted

Work implemented on a branch (or landed without acceptance) but not yet accepted on main: bug fixes
keyed by bug number, and feature deliverables. Each row is one candidate: its branch, its exact
candidate SHA, its acceptance scope, its [promotion-pipeline](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline)
results (each at that SHA, or not run), what its acceptance unlocks and its remaining step. The table is
generated from the candidate record [`docs/project/candidates.tsv`](docs/project/candidates.tsv) by
`scripts/gen/candidate_table.py --write`; whoever lands, re-points or validates a candidate updates its
row and regenerates in the same commit, and `check_docs_drift.sh` fails when the table differs from the
record. Dated dispositions remain visible in the table with their original SHAs and validation history;
they schedule no work. A row leaves when its work is accepted on main and, for bug fixes, the
[bug ledger](docs/bugs/README.md) records it fixed; `scripts/tests/check_roadmap_coverage.sh` fails if a
listed bug is already recorded fixed there, or if a row lacks a branch or a remaining step. A stage (c)
pass is the [one qualification verdict](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline) over every required job at the candidate
SHA, never the mutation aggregate alone.

<!-- BEGIN GENERATED: candidate record (scripts/gen/candidate_table.py; edit docs/project/candidates.tsv) -->
| candidate | bugs | deliverable | branch | candidate SHA | exits | stage (a) | stage (b) | stage (c) | unlocks | remaining acceptance step |
|---|---|---|---|---|---|---|---|---|---|---|
| rocksdb-slice1 | — | RocksDB slice 1 groundwork (R-0285) | `rocksdb-slice1` | `094e51bf3258` | blocked by 0b (incl. bug 102), 2i, 2q | not run | not run | not run | RocksDB slice 1 acceptance, then the four-adapter checkpoint | 2026-11-02: paused until its constructs are supported by the new compiler. landed on main at 094e51bf but not accepted: exits 0b (bug 102), 2i and 2q; G1 (declaring the native library, so it builds with no manual flags) closed by `[native]` ([NATIVE_LIBRARIES.md](docs/platform/NATIVE_LIBRARIES.md)); G3 closed (bug 103 fixed on main by `0c507fb9`, verified 2026-10-21) |
| v1-design-examples | — | V1 design examples (R-0511, row 3) | `v1-design-examples` | `03d2edec1d21` | none (isolated proof development) | not run | not run | not run | V1a's Core interfaces freeze | 2026-11-02: research input for phase A, not an accepted compiler deliverable. landed on main at 03d2edec but not accepted: review of the design questions, then stages (b) and (c) |
<!-- END GENERATED: candidate record -->

## Development throughput

**Decision (2026-10-22): the development cycle's feedback loop is a priority track.**
Its tasks are R-0539 – R-0547, with R-0513 for pre-push gate selection; their bodies are under
[Area: Development Throughput](docs/project/ROADMAP_TASKS.md#area-development-throughput).
It runs beside the compiler sequence under the [scheduling rule](#scheduling-rule).
The validation semantics do not
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
[scheduling rule](#scheduling-rule). It is not a second queue for compiler
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
| 9 | Impact-based gate selection for pre-push: map source files and fixtures to the gates that exercise them, so a change runs every gate it can break | R-0513 | interim checkpoint for the pre-push budget: pre-push median ≤ 10 min, and over 20 consecutive landings no gate failure is first seen in CI | 1 |
| 10 | Build and compile caches: profile the 20 slowest gates first; then a shared compiled-artifact cache keyed by compiler identity and source hash, and a shared Lean build cache across worktrees | R-0546 | the profile is recorded at a SHA; with the compiler unchanged, a second run of those gates takes at most half the first run's time; changing the compiler identity or a source invalidates the cache (control) | 1 |

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
- **Milestones are acceptance scopes**, not dates or phase numbers. The release map below names
  their producing phases; R-0333 records each candidate's actual supported scope.
- Release automation (R-0516) is a prerequisite for automated trains and nightlies. R-0333 records
  compatibility and numbering policy for each release.

### Validation and promotion

How a change is validated before it merges, how a candidate is qualified, the CI tiers, nightly
qualification, triage and the result vocabulary (passed, failed, skipped, cancelled, incomplete) are
in [Validation and promotion](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline). Its stages (a),
(b) and (c) are the ones the exits, the candidate table and the release checklists name.

### Release map

| release | phase | acceptance |
|---|---|---|
| 0.1 | C | The file-copy program and failure paths build natively from the new compiler with their theorems. Publish the supported subset, unsupported constructs and LLVM assumptions. |
| Verified preview and verified growth | E | Each advertised theorem and its endpoint holds, with residual assumptions stated; native proof claims require the Lean Compiler Backend path. |
| Verified-component alpha | E–F | A non-author repairs a bounded component and independently replays its evidence; applicable typed contracts, identity, qualification and receipt checks hold. |
| Project beta | G | Multi-package use and dependency upgrades expose inherited assumptions, evidence, policy and compatibility. |
| 1.0 stable supported subset | G | Whole-language slices, E–F acceptance, conformance, reference, compatibility, distribution and maintenance commitments hold for the named subset. |

The 0.1 blocker list is re-derived when C becomes next. Each later scope inherits applicable earlier
acceptance checks, rerun on its own candidate; it does not inherit the legacy compiler's release scope.

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

The [backlog](docs/project/ROADMAP_BACKLOG.md#pull-rules) is the one home of pull rules and phase-entry reviews.

## Decisions

The dated decisions that shape this queue (priority, semantics, tightening, owner-decision, delivery,
soundness, verified-compiler, usability, vertical-slice and behavioural-contract decisions) are in the
[decision log](docs/project/ROADMAP_DECISIONS.md#decisions). The language rules the project owner
reserved are not decided there: they are the [owner decisions](docs/spec/decisions.md#owner-decisions).

## Reconciliation record

The [2026-10-19 reconciliation](docs/project/ROADMAP_RECONCILIATION.md) records historical task dispositions and the four prose inventories, including their source revision. The current queue and live task records own scheduling and acceptance. Transferred obligations remain binding unless explicitly retired with a recorded reason.

Every `### Task R-NNNN` carries a **Phase:** line; a superseded, merged, delegated, retired or done task also carries a **Disposition:** line naming its owner, and its row in the record's task-disposition table. `scripts/tests/check_roadmap_coverage.sh` reads that table and checks that every task has a phase or a disposition, that every disposition row and queue row names an existing task, that every queue row names an owner, and that no task's phase contradicts its queue row.

The record's [binding reconciliation dispositions](docs/project/ROADMAP_RECONCILIATION.md#binding-reconciliation-dispositions)
replace conflicting implementation directions in the named tasks; existing regression cases and
acceptance obligations survive unless explicitly retired with a reason.

## Archived history

The [legacy execution plan](docs/archive/ROADMAP_LEGACY_PLAN.md) preserves the old phases, queue,
exits, estimates, capacity and release map; it schedules nothing.

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
