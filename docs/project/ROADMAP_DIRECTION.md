# Roadmap Direction and Standing Reference

Status: reference — direction, standing rules and vocabulary that frame the roadmap. It schedules
nothing: [ROADMAP.md](../../ROADMAP.md) owns what happens next and in what order, through
[the current execution queue](../../ROADMAP.md#the-current-execution-queue). Where this file and the
queue disagree, the queue decides *when*.

Moved verbatim from ROADMAP.md on 2026-10-28 (the roadmap compaction); only heading levels and relative links were adjusted. Language rules that stood in the same
sections are in [roadmap-language-scope.md](../archive/roadmap-language-scope.md), pending absorption
into the language specification; completed records went to
[ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md#moved-at-the-2026-10-28-roadmap-compaction).

## Verifier for the browser explorer

From the browser compiler and explorer entry (delivered 2026-10-22; its record is in
[ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md#browser-compiler-and-explorer--bounded-website-delivery)):

**Verifier: separate planned project.** Reuse R-0275 (independent Core-certificate
checker) and R-0353 (independent evidence consumer/schema conformance); do not
create a second overlapping checker initiative. Start from their named predicates,
versioned artifact contract, dependency firewall, trust boundaries, and hostile
fixtures. Browser hosting of such a checker is a later interface decision, not a
prerequisite for this explorer. Compiler output and a successful compile are not
certificates of semantic preservation or functional correctness.

## Glossary

Each term is defined once, here; the linked documents hold the full definitions.

- **Phase / area / milestone / release.** A *phase* (1 – 7) groups queue rows, has an exit and
  schedules work. An *area* (`## Area: …`) is a topical filing heading for tasks and schedules nothing.
  A *milestone* is an acceptance scope (systems preview, verified preview, verified-subset milestones,
  component alpha, project beta, 1.0 stable). A *release* is a published candidate (nightly, train or
  milestone) that passes its scope's acceptance. See [Releases](../../ROADMAP.md#releases).
- **Active / next phases; backlog.** The phases in progress and those that start next, named at the top
  of [ROADMAP.md](../../ROADMAP.md). Tasks neither scheduled in an active or next phase nor named by a queue row live in the
  [backlog](ROADMAP_BACKLOG.md) until pulled.
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

1. **Ratified decisions:** add durable `docs/project/PROJECT.md` language-decision entries for the five identity
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
  and detailed gate transcripts live in [CHANGELOG.md](../../CHANGELOG.md) and the
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
[docs/verification/KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) said OPEN and
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
[docs/verification/VERIFICATION_CHARTER.md](../verification/VERIFICATION_CHARTER.md): important behavior
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

[docs/verification/VERIFICATION_STATUS.md](../verification/VERIFICATION_STATUS.md) is the canonical
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
| **H20** `bv_decide`'s certificate check runs as native code | unowned; see [docs/verification/AXIOMS.md](../verification/AXIOMS.md) | `make test-bv-certificates` | a verified checker, or decomposition until kernel-reduction LRAT is practical |
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

> Moved to [ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md#rationale-notes-the-prover-neutral-arc-r-0004-r-0450-r-0448-r-0454r-0456-r-0458r-0464-r-0467-r-0468) (historical record; nothing in it schedules work).

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

**The design is written: [docs/verification/VC_GENERATOR_DESIGN.md](../verification/VC_GENERATOR_DESIGN.md).** One
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

This section does not own execution order. The [current execution queue](../../ROADMAP.md#the-current-execution-queue)
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
[capacity allocation](../../ROADMAP.md#capacity-allocation) divides effort and integration slots between
the tracks: the verified compiler holds the majority and two slots, evidence integrity and
adversarial validation have protected minimums, and usability, interop, application,
incremental-driver and release work share one capped feature slot.
Every activated checkpoint records its accountable maintainer, required reviewer and exit.

Completed work moves to [CHANGELOG.md](../../CHANGELOG.md) under a `### Completed Task R-NNNN`
heading so the gate reserves that identity permanently.

Every user-facing claim should have a replay command, checked report, gate, or
proof. Unsupported cases must fail closed and loud. A feature that passes only
because no one tried to break its gate is not done.

### Future Compiler-Pipeline Refactor Contract

> **Reconciled 2026-10-19:** where this contract names QBE as a backend or a forcing consumer, read the
> LCB-native profile behind Concrete's BIR → LCB-IR adapter (R-0517); its QBE-first sequencing is
> superseded by the current queue and the verified-pipeline IR chain (Place MIR → Memory CFG → BIR).
> The refactor acceptance requirements remain in force; each refactor's owner is recorded in the
> [reconciliation record](ROADMAP_RECONCILIATION.md).

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

> Moved to [ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md#external-validation-gate) (historical record; nothing in it schedules work).

### Early Risk Probes

Four verdict-deciding risks get narrow early probes:

- **External credibility:** deterministic `diff-caps` style authority-diff
  artifact before the full flagships-area trial.
- **Proof cost:** the proof-authoring area proof-effort telemetry on the first real proof slice.
- **Backend soundness:** narrow the backend-contracts area translation-validation slice over a
  small supported subset.
- **Pipeline scale:** old phase 6C and the flagships area shadow invalidation transcript before real
  artifact reuse.

## Architecture Decision Index: backend and compiler architecture

The [Architecture Decision Index](../archive/roadmap-language-scope.md#architecture-decision-index-non-executable-context)'s
introduction (the `pub`-only visibility rule) and its stdlib, linearity and slice decisions are language
scope and moved to the archived language scope; its backend and compiler-architecture decisions stay here.

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
