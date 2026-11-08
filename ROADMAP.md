# Concrete Roadmap

Concrete is a systems language with linear ownership, second-class references and explicit
capabilities. Its specification is a Lean library, `ConcreteSpec`: readable definitions with
plain-words doc comments and conformance examples. A new compiler is built from that library, proof
first, one slice of the language at a time, and each slice states the exact scope of its theorem.
The first executable milestone is slice 0, a closed `u8` arithmetic subset traversing the actual
compiler to an unverified LLVM native build. Each used lowering is proved as it lands. A composed
conditional image theorem and its explicit assumptions accompany it. The language then grows in thin
vertical slices, each fully proven end to end before the next starts
([vertical slices](#vertical-slices); [decision](docs/decisions.md#decided-project-vertical-slices-2026-11-06)). Slice 1 is the approved linear component
with its launcher obligations ([decision](docs/decisions.md#decided-project-proof-infrastructure));
LP0 follows it, and integers are complete at slice 2. Each slice's theorem covers
its exact source down to BIR. Its native output is qualified by tests and labelled unverified; any
claim about the native image is conditional on the LLVM, linker and image assumptions it lists. Beyond the bounded LCB pilot (LP0, for the slice-0 and slice-1 subset), a proof that reaches native
code is later work (phase E), not the first milestone. The old compiler is frozen as `legacy-compiler-final`. A pinned build of that tag is the
differential oracle. `legacy/` stays, byte-frozen, as the reference for ideas the new design may have
forgotten; it is deleted, with the owner's approval, once the new compiler implements a full real program such as RocksDB, ethrex or similar (LD, R-0590).

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

   The compiler imports it and builds a proved lowering chain below Core. Slice 0 gets two native
   builds, a BIR → LLVM printer and BIR → LCB-IR → the Lean Compiler Backend, both labelled
   unverified, and CI compares their observations; later slices extend the same paths. The BIR → LCB-IR
   adapter is the first backend proof, and a bounded BIR → LCB → native proof pilot follows
   before the language grows large (LP0, [2026-11-06](docs/decisions.md#decided-project-long-term-2026-11-06))
   ([decision](docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract),
   [revised 2026-11-05 and 2026-11-06](docs/decisions.md#decided-project-proof-infrastructure)).
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
   review, the hooks, CI and the changelog. `legacy/` is kept much longer than first planned, as the
   reference for ideas the new design may have forgotten (owner, 2026-11-06): nothing is deleted at
   B3. A short ideas sweep of `legacy/` covers each of phases C, D and E's areas, at the latest at that phase's start
   (R-0591, R-0597, R-0598). `legacy/`, the old pipeline included, is deleted in one change once the
   new compiler implements a full real program such as RocksDB, ethrex or similar (LD, R-0590; in the
   slice plan the earliest is the RocksDB acceptance program after slice 25), and only when a final ideas sweep has dispositioned every idea (carried, planned,
   deferred or dropped on purpose), every row of the root retention manifest `RETENTION.tsv`
   (R-0562) is resolved, and the owner explicitly approves. Retained assets (corpora, regressions,
   examples, std, the bug ledger, the site and docs) migrate as slices need them. That manifest and
   the [task disposition table](docs/archive/roadmap-2026-11/task_disposition.tsv) live outside
   `legacy/` and survive its deletion.
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
and never repaired. At B3, when the new compiler has replaced the old pipeline, they become *retired with the old
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
| B. Cumulative slices 0 and 1, then LP0 | Slice 0 establishes the running proof infrastructure: a closed `u8` arithmetic subset through every used IR to two unverified native builds, with per-lowering proofs and the composed conditional theorem. Then `ConcreteSpec` and the new compiler for slice 1 (the approved linear component, row Q8.2), on the same path; then LP0. See [vertical slices](#vertical-slices). | Each slice meets the [slice shape](#vertical-slices). Every judgment, semantics and pass is total. Fuel exhaustion is distinct from divergence. The native builds are qualified by tests, not proved: the LLVM output is labelled unverified, and its assumptions are listed with the lexer, model-translation and native-image obligations; a native-image claim, if any, is stated separately as conditional on those assumptions. Minimal provenance accompanies the first authoritative proof claims. The outside-reader test passes on the Lean rules. Phase B closes after slice 1, LP0 and B2/B3 acceptance; `legacy/` stays until LD. |
| C. File-copy | Workloads grow from simple proved examples to file-copy and then supported real programs; each records program correctness, authority, failure behaviour and residual assumptions. [Vertical slices](#vertical-slices) 2 to 18, each meeting the slice shape: integers, control flow, loops, functions, authority, secrets, the proof surface, structs, enums, generics, `Option`/`Result`, references, traits and `defer`, arrays, text and the console, modules, and files, ending at file-copy. Std, examples and tests come back as their constructs land. | The file-copy program, failure paths included, builds natively with its theorems. **0.1 is cut** at slice 18 with the supported subset, the unsupported constructs, the LLVM assumptions and the proven zero-heap facts stated. |
| D. Whole language | [Vertical slices](#vertical-slices) 19 to 28: allocation, function values, heap arrays, `Vec` and `String`, maps, raw pointers, FFI (then RocksDB slice 1), floats, clock, network and devices, and concurrency as the final slice (then ethlambda and the encrypted tunnel). | Every construct of the decided language is a slice with Lean rules, and the conformance suite is complete. |
| E. Verified growth | Proof-order items 4–8, the Lean Compiler Backend native path, the verified parser, typed contracts, frame reasoning, termination proofs and the proof library. Earlier slices already contain example-specific program theorems; E expands reusable models and proof support. Contracts have no source `old` and no snapshot construct (OD117): a postcondition reaches the entry state only through an explicit Lean theorem. Spec functions are total (OD115). An ordinary function can get its own termination proof, and there is no total-function category (OD116). | Each item's theorem holds, with its residual assumptions stated. |
| F. Evidence as a product | Provenance, the canonical verification task, receipts and an independent consumer, all on the new Core. | Receipt v1 frozen. |
| G. Release and ecosystem | External users, flagships, packages, editor, incremental builds, 1.0. | External-user acceptance, package evidence, conformance, compatibility and maintenance commitments hold for the released subset. |
| T. Development throughput | Beside the sequence: the minimal root, then faster landing, review and CI. | The [targets](#development-throughput) hold. |

## The queue

**Active:** A2i (integrating the proposed spec), B0 (the slice-0 design is done; building it is next) and folding the decided diagnostics, memory and information-flow interfaces into A2i (R-0592). What waits for the owner is [listed in the decisions](docs/decisions.md#waiting-for-the-owner). **Next:** A2c (the owner's spec read and freeze), then B1. The legacy move (T0–T2) has landed; A1 and A2b feed A2i. A2a (provisional approval of the Core shape, decision row Q9.19) is done: the owner approved every Core row on 2026-11-05 ([record](docs/decisions.md#decided-2026-11-05)); the approval binds the design without freezing it.

| row | goal | tasks | done when |
|---|---|---|---|
| T0 | Prepare the restart beside the old tree (decision row P7, approved 2026-11-05) | R-0568, R-0564 | Checklist, each item on main before T1: (1) the new root's CI jobs; (2) `required-checks.txt`, enforced by `train.sh watch` and `train.sh land`; (3) the required-check transition mechanism, an explicit old-to-new job mapping that is validated and needs the replacement checks to pass at the exact candidate SHA, never marking a dropped legacy check passed; (4) `ISSUES.tsv` and its validator; (5) `RETENTION.tsv` and the task disposition table; (6) `scripts/oracle/ORACLE.lock` with the oracle release artifact and its digest; (7) the archive records of the stopped services. |
| T1 | Rehearse the move on the candidate root | R-0563 | On a throwaway branch holding the exact proposed day-one root, a fresh clone builds the candidate, hooks, runs exact-SHA CI and passes a dry-run landing, and the negative controls are refused for the expected reason. |
| T2 | Move the old tree to `legacy/` in one train, after T0 and the T1 rehearsal | R-0558, R-0565 | The root holds only the day-one set. `legacy/` is byte-frozen and checked. The move itself lands on both remotes. The website is down. |
| A1 | Dispose of the spec audit and draft the boundaries Core must satisfy | R-0555, R-0312, R-0316 | Each audit finding, gap and duplication has an owner and an acceptance artifact. The [boundary drafts](legacy/docs/compiler/A1_BOUNDARIES.md) exist. |
| A2b | Formalize the approved design in a proposed `Spec.lean` (current work) | R-0511 | Exit to A2i: `ConcreteSpec/Spec.lean`, aligned with the decided rows, is headed "PROPOSED: not normative until Core freeze"; the four design theorems are kernel-checked. The four design theorems (`reset_fresh`, `defer_cleanup_releases`, `open_mut_freezes_root`, `rep_roundtrip`) and their full obligations are kernel-checked without `native_decide`. Every row the formalization changes returns to the owner. This is separate from slice 1's soundness theorems. |
| A2i | Integrate the proposed spec before the freeze ([decision](docs/decisions.md#decided-project-long-term-2026-11-06)) | R-0571, R-0574, R-0496, R-0560, R-0555, R-0559, R-0588, R-0592, R-0602, R-0603, R-0607, R-0608, R-0609, R-0610, R-0611, R-0612, R-0613, R-0614, R-0615, R-0616, R-0626 | The shared foundation, contracts C1–C10 ([decision](docs/decisions.md#decided-2026-11-08-foundation)), is written in Lean with its laws proved, before slice 1 builds on it; each later slice adds its rules without rewriting it ([foundation first](docs/decisions.md#decided-2026-11-07-foundation-first)). The separately proved parts share Core interfaces and one integrated semantics. Interaction proofs hold for a borrow across deferred cleanup, an allocator reset with outstanding loans and cleanup that needs authority, and the README obligations [O7 and O9](ConcreteSpec/README.md) are discharged. The semantic contract is stated. The normative definitions are one library in cohesive modules with `Spec.lean` as the public import. The browsable owner view exists. The pre-freeze part of R-0574 (no hidden traps and bounded execution) is in the proposed spec. An outside reader has read the integrated spec through that view. |
| A2c | Core freeze | R-0567 | The owner approves the freeze of the integrated scopes after A2b's four theorems, A2i's integration proofs and an outside-reader pass. Only then is PROPOSED removed. A3 sends Core back only as a recovery when it exposes a substantive change. |
| A3 | State the theorem and the trusted base in Lean | R-0502, R-0506, R-0508, R-0509, R-0500, R-0588 | The contract and the modular layout from A2i are on main. The theorem, with its holes, the named values-to-bytes boundary and the trusted-base inventory, is on main. The theorem skeleton uses concrete slice interfaces; its semantic-premise inventory is separate from the axiom audit. Before slice 0's authoritative claim, its actual Core fragment is connected to the frozen definitions and any instrumentation erasure it uses is proved; later features require integration proofs into that same Core. Slice 0 starts after A2c and the applicable A3 foundations for its authoritative claims; a provisional skeleton may start earlier and claims nothing normative. |
| B0 | Build and qualify slice 0, the proof spine ([decision](docs/decisions.md#decided-project-proof-infrastructure)) | R-0630, R-0501, R-0581, R-0580, R-0583, R-0503, R-0578, R-0579, R-0425, R-0499, R-0493, R-0507, R-0521, R-0552, R-0118, R-0498, R-0553, R-0573, R-0575, R-0585, R-0588, R-0593, R-0594, R-0577, R-0272, R-0301, R-0550, R-0620 | Slice 0 runs through every used IR to two unverified native builds, LLVM and the LCB, whose full observations CI compares with each other and with the expected outcome, with per-lowering proofs, independent supported-subset coverage, the composed conditional image theorem, artifact binding, example correctness, including transport to the exact native artifact under the named premises, and classified differential and refusal controls. The per-function facts report (R-0575) is produced for every slice-0 function, each fact and obligation with its source location. The BIR → LCB-IR adapter proof (R-0579) is the first backend proof. Reproducible builds, final-artifact stack facts, the trusted-base statement, the front-end differential fuzzer, ASan runs (UBSan dropped, [decision](docs/decisions.md#decided-2026-11-08-ubsan)), binary-size tracking and the frozen, versioned JSON schema start here. This does not trigger legacy deletion. |
| B1 | Build `ConcreteSpec` and slice 1 with its theorem | R-0425, R-0499, R-0492, R-0493, R-0514, R-0501, R-0503, R-0497, R-0504, R-0507, R-0521, R-0552, R-0528, R-0479, R-0118, R-0573, R-0576, R-0577, R-0579, R-0580, R-0582, R-0586, R-0588, R-0589, R-0599, R-0600 | [Slice 1](#vertical-slices) meets the slice shape: it runs from parsing to both native builds, its judgments are independent, its correspondence and soundness are proved, its obligations are explicit and its provenance is minimal. |
| LP0 | The LCB pilot: bound the LLVM hole ([decision](docs/decisions.md#decided-project-long-term-2026-11-06)) | R-0572 | The LCB already runs unverified from slice 0 (R-0578) and its adapter is proved first (R-0579). After slice 1 (B1) and before any growth: the slice-0 and slice-1 subset compiles through BIR → LCB → native code with the adapter proof composed with the LCB's theorems and the slice theorem under named premises, kept separate from the LLVM premises. Every remaining backend obligation is a task with a milestone. |
| B2 | Qualify slices 0 and 1 | R-0491, R-0498, R-0272, R-0550 | Differential (Lean semantics, native build, legacy oracle), refusal and gate-integrity controls hold. The fuzzers target the new compiler. The outside-reader test passes. |
| B3 | Audit totality and retire legacy defects (revised 2026-11-06: nothing is deleted here) | R-0553, R-0561 | The totality audit finds no partial definition. Legacy defects are retired with the old implementation. `legacy/` stays byte-frozen; its deletion is LD. |
| LD | Delete `legacy/` once the new compiler implements a full real program ([decision](docs/decisions.md#decided-project-restart), revised 2026-11-06) | R-0590 | The new compiler implements a full real program such as RocksDB, ethrex or similar (in the slice plan the earliest is the RocksDB acceptance program after slice 25). A final ideas sweep of `legacy/` dispositions every idea (carried, planned, deferred or dropped on purpose), every `RETENTION.tsv` row is resolved, and the owner explicitly approves; then `legacy/`, the old pipeline included, is deleted in one change. |

Tasks shared by B0 and later rows deliver their slice-0 increment at B0; their remaining acceptance obligations stay with the later row. Dependencies between cumulative tasks apply to the corresponding increment.

<a id="vertical-slices"></a>
### Vertical slices

The language grows in thin vertical slices, each fully proven end to end before the next starts ([decision](docs/decisions.md#decided-project-vertical-slices-2026-11-06)).

**The slice shape** (the same `done when` for every slice): the slice's rules are Lean judgments
with accepted, refused and trapping conformance examples; its programs run from source through
every stage to BIR and both backends, each stage proved or checked by a proved validator; the
composed theorem covers the slice under named premises, LLVM and LCB premises separate; the facts
report and diagnostics cover its constructs; its examples pass on the Lean semantics and on both
backends, and CI compares their full observations.

**The order** (agreed unanimously by Claude, Codex, Kimi and agy; owner-approved 2026-11-06,
revised 2026-11-07). Later slices extend earlier constructs without reintroducing them. Not
scheduled: `free` (OD59), `trusted mod` (OD61), mutable global cells (OD8), `process_exit` (OD136),
process spawning (Q7.4), foreign function values (OD4). Concurrency is the final language slice;
its model remains to be decided when that slice comes.

| slice | adds | phase |
|---|---|---|
| 0 | `main -> u8`, immutable typed `u8` lets, decimal literals (leading zero refused), reads, parentheses, checked `+`, return; `concrete build` and `concrete facts`; the diagnostics record; both backends; reproducible builds; each program's maximum stack use, measured from the final binary on both backends, with the `Concrete.toml` stack budget; the trusted-base statement in facts; the random-program differential fuzzer; AddressSanitizer; binary-size tracking and the compile-speed budget; the frozen, versioned JSON schema for diagnostics and facts from the first front-end output; the `overflow` trap kind | B0 |
| 1 | the approved component (row Q8.2): an empty non-`Copy` struct, its literal, a move, the use-after-move refusal, a struct-destructuring `let`, one checked operation, a return; explicit `abort()` as the runtime trap example and the refused `200 + 56` control; the component and bound launcher; the predictable policy; `Concrete.toml` `[policy]` and `[assumptions]`; the Lean I/O driver infrastructure; the `abort` trap kind | B1 |
| LP0 | BIR → LCB → native proof for slices 0 and 1 (R-0572); no syntax | LP0 |
| 2 | all integer widths, literal forms, unary `-`, `- * / %`, bitwise operations, shifts, the named lossless conversion (no `as`), `wrapping_*`, `saturating_*`, the named lossy conversions (OD99); all values known; the `division_by_zero` and `invalid_shift` trap kinds | C |
| 3 | `bool`, comparisons (OD42), `&&`, `\|\|`, `!` (Q6.5), `Unit`, value `if`/`else`, trailing value expressions and the Unit statement rule (OD29), the head rule (OD16, OD34), `let mut`, assignment, no shadowing (OD67), `discard` (OD62); the octagon checker through branches and joins; `checked_no_implicit_traps`; the may-abort fact; a dead store to a `mut` local refused (SPARK 4(c), extending OD62) | C |
| 4 | `while`, `for i from a until b` (OD47–OD49), labels, `break`, `continue`, `#[invariant]`, `#[variant]`, `#[may_not_terminate("reason")]` on loops; the loop rule; termination facts; the step-counting cost model for loops | C |
| 5 | functions, parameters, calls, recursion with one function `#[variant]` or permission per recursive component (OD82), `#[requires]`, `#[ensures]`, literal expected types (OD31, OD138); closed call evidence; stack and depth facts from final artifacts; proven stack bounds for recursion; the step-counting cost model extended through calls; the explicit mathematical-arithmetic boundary in contracts (spelling from R-0625) | C |
| 6 | authority: `with(...)` over the reserved set (no spawning), the inclusion judgment (TRUST-4), the primitive effect table, `main` grant parameters, `give_up` and `close_dir`, launcher minting, startup refusal 125, the start and exit trace; capability and reachable-grant facts; the entropy capability granted to `main` | C |
| 7 | `Secret<T>` on scalars: `classify` (of a value passed in), `declassify(x, "reason")`, `ct_select`, secret comparisons giving `Secret<bool>`, the closed constant-time operation table; plain arithmetic, secret control flow, divisors and shift counts refused; noninterference and source constant-time theorems; release sites traced; secret policies; backend constant time and wiping as named premises; entropy output as `Secret<T>`, with its source named as a trusted premise in facts | C |
| 8 | the proof surface: `assert`, `assert(e) by thm(...)`, `assume` (never admits safety; removed only after R-0627), `ghost let`, total `spec fn`, `#[proof(path)]`, vacuity refusal and strength labels (OD120, R-0585), `#[test]`; proofs of step bounds; the test runner | C |
| 9 | structs with named fields, declared `Copy`, `e.f`, field writes on `mut`, destructuring, field-wise known values, type aliases; constructors written `field: value` and patterns `field: binder`; every local has a written type | C |
| 10 | enums, `match` (first arm wins, exhaustive, unreachable arms refused, OD41), kept patterns | C |
| 11 | generics, `T: Copy`, type-argument inference (OD46, OD129), capability parameters and arguments (Q5.1–Q5.4), OD132; Mono Core, its validator and re-admission (Q6.3) | C |
| 12 | builtin `Option` and `Result`, `checked_*` with `ArithmeticError`, `try_convert` with `OutOfRange` | C |
| 13 | second-class `&T` and `&mut T`, `*r`, borrow blocks, projections, borrowed-scrutinee binders (OD3), lending, overlap and freezing (OD1, OD2, OD15, OD23); borrow blocks written `borrow [mut] v = source [from a until b] { … }` (Q4.7 and Q9.5 as revised; no `as`) | C |
| 14 | traits, impls, calls written `Type::f(x)` or `Trait::f(x)` with a typed first parameter (no method calls or receivers), `Destroy`, `defer` (OD69, OD140) | C |
| 15 | arrays, indexing with evidence, range views, `swap_at`, array `Destroy`; `ct_eq` on arrays and views; linear secret arrays wiped on destroy; entropy fill-bytes; the `out_of_bounds` and `invalid_range` trap kinds | C |
| 16 | `StaticStr`, UTF-8 operations on `u8` (no `char`), UTF-8 text views, `byte_at`; the console streams; integers printed by user code; grant-backed secret sources from the console; no `print`, `println` or `append` builtins: output is ordinary calls that need `Console` | C |
| 17 | modules, `pub`, imports, paths (OD43, OD21), constants (Q6.4), generated interfaces and their diff gate (R-0556), DWARF on LLVM (R-0587); `#[invariant(...)]` on structs with private fields; the test runner extended to modules; the formatter; imports `import a::b::{x};`, every imported item used qualified as `b::x` (item 11(b), option 3); constants computed at compile time with `Copy` values and a closed initializer subset (Q6.4 and TYPE-22 as revised); compiler-generated interfaces keep bodyless declarations, and handwritten bodyless functions are refused | C |
| 18 | files and directories (`Fs`), narrowing, typed errors, failure-path handle conservation, environment reads; grant-backed secret sources from files; **file-copy and 0.1** (zero heap) | C |
| 19 | allocation: brands, `Allocator<A>`, arenas, reset, `Heap<T, A>`, fallible `alloc`, heap borrows (OD36), `AllocFailure<T>`; heap facts and memory policies (R-0595); heap secrets wiped; the named heap move-out replaces the consuming `*h` (OD59 as revised); existential `unpack <brand A> x = p { … }` | D |
| 20 | function values (function pointer plus explicit context, exact capability sets), indirect calls, the `#[may_not_terminate]` expression prefix; accepted only with function-type contracts (R-0601) | D |
| 21 | `HeapArray`, heap-array views, `drain`, `map_into` | D |
| 22 | `Vec`, `String<A>`, allocating text APIs (OD154); string append as an ordinary allocating call under `with(Alloc)` | D |
| 23 | `OrderedMap`; `HashMap` only when the key-value store's measurements need it | D |
| 24 | raw pointers and the raw memory model (Q3.7–Q3.11), `trusted fn`, `with(Unsafe)` (OD64, OD98); the no-libc runtime on the proven path later, around slices 24–25 (R-0621) | D |
| 25 | FFI: `extern fn`, `#[link_name("sym")]` and `#[export_name("sym")]`, `repr` forms without packed (OD147 as revised), `Kept<H>`; then **RocksDB slice 1** as an acceptance workload (R-0604) | D |
| 26 | `f32` and `f64` (OD63), float conversions (OD99, OD107) | D |
| 27 | clock and time, network grants with `with(Network)`, device grants; grant-backed secret sources from the network; **httpget, tcpserve and the sequential static web server** | D |
| 28 | concurrency (R-0399), the final language slice; then **ethlambda** (R-0604) and the encrypted tunnel after the randomness/nonce audit | D |

Step bounds count abstract operations in input and pattern size. Their model starts with loops at
slice 4 and calls at slice 5, and their proofs are available at slice 8; wall-clock claims need
separate backend evidence ([decision](docs/decisions.md#decided-2026-11-07-step-bounds)); home: spec
(the cost model and the step-bound theorems); gate: the acceptance gates of slices 4, 5 and 8. The test
runner starts at slice 8 and extends to modules at 17, the formatter lands at 17, and the LSP comes
later ([decision](docs/decisions.md#decided-2026-11-07-tools)); home: tooling; gate: the test-runner
and formatter checks of slices 8 and 17, and R-0398 for the LSP ([tracking](docs/decisions.md#decided-2026-11-07-tracking)). Struct invariants in source at
slice 17: home: spec (the invariant and its obligations, defined before the freeze); gate: slice 17's
acceptance. Flagship runtime is tracked from
slice 2, and compiler wall time, per-stage time and memory from slice 0. Measurements are recorded
by exact SHA, backend and target and are report-only until their noise is understood
([decision](docs/decisions.md#decided-2026-11-07-performance)). Reproducible builds start at slice
0, with byte-identical BIR, LLVM IR text, LCB job and output, and final binaries checked in different
directories and on a GitHub runner and concrete-ci-2
([decision](docs/decisions.md#decided-project-reproducible-builds)).

Stack bounds start at slice 0: `concrete facts` reports each program's maximum stack use, measured
from the final binary on both backends, and an unknown bound or one over the `Concrete.toml` stack
budget is refused; slice 5 extends it with proven bounds. Entropy is a capability at 6, yields
`Secret<T>` at 7 and gains fill-bytes at 15; its source is a named trusted premise in facts
([decision](docs/decisions.md#decided-2026-11-07-stack-randomness)). The trusted-base statement,
sanitizers and report-only binary-size tracking start at slice 0, the differential fuzzer starts
with the slice-0 front end and grows one feature per slice, and the frozen, versioned JSON schema
starts with the first front-end output
([decision](docs/decisions.md#decided-2026-11-07-early-evidence)).

**Showcases** (all seven adopted by the owner on 2026-11-07;
[decision](docs/decisions.md#decided-2026-11-07-showcases)):

| showcase | slices |
|---|---|
| `grep` with a linear-time regex | 16 → 18 → 22 for dynamic patterns |
| image decoder | QOI at 19 → 21; PNG with DEFLATE at 21–22 |
| EVM interpreter | 15 → 21 → 23 |
| embedded key-value store | 23, with durability "not established"; RocksDB comparison at 25 |
| WebAssembly sandbox | 21 → 23 → 26 |
| crypto | 7 → 15 → 18 file encryptor; encrypted tunnel after 28 and the randomness/nonce audit |
| static web server | sequential at 27; concurrent version after 28 |

Each showcase states its functional, resource, authority and backend claims separately. Home:
tooling (the example programs), with their claims from the spec; gate: each version's claims
checked at its slice (its theorems, its `concrete facts` lines and its runs on the Lean evaluator,
LLVM and LCB) ([tracking](docs/decisions.md#decided-2026-11-07-tracking)). Phase-E
proof items (functional models, frames, the verified parser, LCB native closure, the direct-syscall
launcher) are tasks beside these slices, not language slices. In phase E, the optional checked
`#[depends(result <- a, b)]` (SPARK 4(b)) arrives in source; it is checked against the derived fact
and never trusted ([decision](docs/decisions.md#decided-2026-11-08-language) item 28). Lexicographic measures for
`#[variant]` come when a workload needs them (R-0605).

### Std per slice

One review round settles where each std item lives ([decision](docs/decisions.md#decided-2026-11-08-process), item 3); its proposal revises the placements below to the 2026-11-08 language decisions (for example, `HashMap` now comes only when the key-value store's measurements need it), and until then they stand as recorded. The owner approved this std-per-slice table on 2026-11-08 ("ok let's do 2", [decision](docs/decisions.md#decided-2026-11-08-std-table)); placements below follow the existing slice and decision rows. Every new-root std module is **not started**: each needs a spec contract, kernel-checked laws, accepted/refused conformance examples, and the program theorems named below. Source paths are planned destinations, not existing implementations; a language substrate arriving does not by itself schedule every legacy API built on it.

| module | arrives at slice | spec home | std source (legacy idea) | evidence due | needed by |
|---|---|---|---|---|---|
| bytes | not placed; fixed-byte substrate at 15, heap arrays at 21, growable storage at 22 | C2 scalar operations; C4 `Store`; C5 range/ownership contracts | `std/src/bytes.con` ← `legacy/std/src/bytes.con`; carry byte operations, not its raw allocating representation | Not started; bounds, byte equality and copying laws; codec/interpreter program theorems | EVM 15/21/23; QOI 19/21; PNG 21–22; crypto 15/18; Wasm 21; store 23; server 27 |
| fixed arrays and range views | 15 | C4 `Store`, C5 `Borrow`/`Ownership`; `Primitives` | `std/src/slice.con` ← `legacy/std/src/slice.con`; arrays are language forms | Not started; indexing, overlap, permutation, destruction and view laws | EVM 15; crypto 15/18; grep 16/18; all byte workloads |
| text / UTF-8 | 16 (static text and views); allocating APIs 22 | C2 `Scalar`, C5 `Borrow`; `Primitives` UTF-8 contracts | `std/src/text.con` ← `legacy/std/src/text.con` | Not started; validation, boundary/split and invalid-input laws; grep matching theorem | grep 16/18/22; CLI workloads; server 27 |
| String | 22 | C4/C5 storage, brands and views; C7 accounting | `std/src/string.con` ← `legacy/std/src/string.con` | Not started; UTF-8 preservation, growth/failure and destruction laws | dynamic grep 22; later CLI/server; legacy args dependency conflicts below |
| Heap and allocators | 19 | C4 `Store` availability laws; C5 brands/dependencies; C7 accounting | `std/src/heap.con`, `std/src/alloc.con` ← corresponding files in `legacy/std/src/` | Not started; failure conservation, reset freshness, release and heap bounds | QOI 19/21; later image/EVM/Wasm/store; RocksDB after 25; ethlambda after 28 |
| HeapArray | 21 | C4/C5 heap storage, views, `drain` and `map_into`; C7 accounting | Planned source home within `std/src/heap.con`; legacy `heap.con`/`slice.con` ideas, not a claimed legacy HeapArray implementation | Not started; length, view, transfer and cleanup laws | QOI 21; PNG 21–22; EVM 21; Wasm 21 |
| Vec | 22 | C4/C5 container ownership; C7 growth bounds | `std/src/vec.con` ← `legacy/std/src/vec.con` | Not started; contents, growth, failure conservation and destruction laws | dynamic grep 22; legacy CLI; later interpreter/store storage |
| OrderedMap | 23 | C4/C5 ownership; ordered destruction contract; C7 accounting | `std/src/ordered_map.con` ← `legacy/std/src/ordered_map.con` | Not started; lookup/update, ordering and ascending destruction laws | store 23; EVM 23; Wasm host tables 23 |
| HashMap | 23 | C4/C5 ownership; hashing/equality contract; C7 accounting | `std/src/map.con` ← `legacy/std/src/map.con` | Not started; lookup/update, collision and failure laws; no invented Destroy impl | store 23; EVM 23; Wasm host tables 23 |
| hash | not placed | C2 pure scalar/sequence contract; C9 `Primitives` where primitive | `std/src/hash.con` ← `legacy/std/src/hash.con` | Not started; specified hash/equality consistency; distinguish FNV from cryptographic hashes | HashMap 23; does not supply EVM Keccak |
| io / console | 16 console; file streams 18 | C5 authority; C6 `Contract` events; C9 `Primitives` | `std/src/io.con` ← `legacy/std/src/io.con` | Not started; partial I/O, errors, grant confinement and handle conservation | grep 16/18; file encryptor 18; store 23; server 27 |
| fs | 18 | C5 Fs grants/narrowing; C6 external-response contract | `std/src/fs.con` ← `legacy/std/src/fs.con` | Not started; typed errors, narrowing and cleanup on every path; file-copy theorem | grep 18; encryptor 18; store log 23; server 27; RocksDB after 25 |
| args | not placed; Environment substrate at 18 | C5 Environment grant; C6 launch/input contract; OD156 | `std/src/args.con` ← `legacy/std/src/args.con` | Not started; argument access, invalid UTF-8 and zero-heap input contract for early programs | `grep PATTERN FILE` 18; later command-line showcases |
| cli | not placed; legacy implementation needs Vec/String at 22 | C2 parsing; C5 Environment authority; C6 launch contract | `std/src/cli.con` ← `legacy/std/src/cli.con` | Not started; flag/positional parse laws and errors; command-line program theorems | grep 18/22; file encryptor if command-line driven; server launcher |
| env | 18 reads; other operations not placed | C5 Environment authority; C6 environment-response contract | `std/src/env.con` ← `legacy/std/src/env.con` | Not started; read/error/encoding laws; no implicit allocating String requirement at 18 | command-line environment boundary; acceptance-program needs subject to R-0604 |
| time / clock | 27 | C5 clock identity/kind/resolution grant; C6 time responses | `std/src/time.con` ← `legacy/std/src/time.con` | Not started; only grant-promised clock laws; no unconditional monotonicity or deadlines | network programs 27 and later when they use time; acceptance audits |
| entropy / random | 6 grant; 7 secret scalar output; 15 fill-bytes; other random APIs not placed | C5 grant (shape deferred to 6); C6 source responses; C7 leakage | `std/src/rand.con` ← `legacy/std/src/rand.con`; retain source premise explicitly | Not started; secret-flow/fill laws, source premise; randomness/nonce audit before tunnel | crypto 7/15/18; encrypted tunnel after 28 |
| net | 27 | C5 Network grants; C6 network events; C9 primitive contracts | `std/src/net.con` ← `legacy/std/src/net.con` | Not started; partial reads/writes, errors, cleanup and authority containment | sequential server/httpget/tcpserve 27; tunnel and ethlambda after 28 |
| crc32 | not placed | C2 bitwise/sequence contract; C9 if registered as primitive | `std/src/checksum.con` ← `legacy/std/src/checksum.con` | Not started; CRC32 ISO-HDLC recurrence and chunk/incremental laws; PNG integrity theorem | PNG 21–22 |
| endian / numeric byte codecs | not placed; integer substrate 2, array/view substrate 15 | C2 `Scalar`; C3 representation contract, separate from native layout | `std/src/numeric.con` ← `legacy/std/src/numeric.con`, plus endian ideas in `bytes.con` | Not started; encode/decode round trips, widths and bounds | QOI 19/21; PNG 21–22; EVM 15/21; Wasm 21; crypto 15 |
| base64 | not placed (slice-17 example is a proposal) | C2 sequence/codec contract | `std/src/base64.con` ← `legacy/std/src/base64.con` | Not started; round trip, invalid-input and output-size laws | proposed base64 example 17; no adopted showcase requires base64 explicitly |
| crypto primitives | 7 scalar stage → 15 array stage → 18 file encryptor; complete primitive-module placements not placed | C2 arithmetic; C7 `Leakage` noninterference/constant time/wiping; C9 contracts | Named ChaCha20, Poly1305, X25519 and EVM Keccak source filenames not fixed; `std/src/sha256.con` ← legacy `sha256.con` is a separate supported idea, not a substitute | Not started; primitive functional laws, source constant time and release/wipe theorems; backend premises separate; SHA-256 placement not placed | crypto 7/15/18; EVM Keccak by 23; Noise tunnel after 28 and nonce audit |
| path | not placed | C5 Fs confinement/narrowing; C2 path operation contracts | `std/src/path.con` ← `legacy/std/src/path.con` | Not started; path operations and confined-resolution laws | filesystem showcases, especially static server 27; exact helper use not fixed |
| ascii / parse / fmt / hex | not placed | C2 scalar/text contracts; C6 for actual output | `std/src/ascii.con`, `parse.con`, `fmt.con`, `hex.con` ← corresponding legacy files | Not started; classification, parse/format and hex round trips | grep text handling; HTTP parsing; diagnostic/hex output where selected; helper dependencies not fixed |
| Option / Result | 12 (builtin types) | `Types`, `Primitives`, `Admission`; C2 failures, C5 linear payloads | `std/src/option.con`, `std/src/result.con` ← corresponding legacy files; no duplicate type authority | Not started; case/propagation and payload conservation laws | typed-error stages of every later showcase |
| ptr / mem | 24 raw operations; helper-module APIs not placed | C3 layout, C4 `Store` provenance/validity, C10 frames | `std/src/ptr.con`, `std/src/mem.con` ← corresponding legacy files | Not started; safe-wrapper refinement, initialized bytes and provenance laws | FFI acceptance after 25; earlier safe containers cannot depend on source-level raw operations |
| libc / process | 25 FFI substrate; wrapper/process modules not placed | C5 authority; C6 external events; C10 ABI and retained-handle contracts | `std/src/libc.con`, `std/src/process.con` ← corresponding legacy files | Not started; foreign premises, ABI and handle lifecycle laws; R-0604 program audit | RocksDB slice 1 after 25; ethlambda after 28; legacy runtime calls are not an early std dependency license |
| test | 8 test runner; module support 17; legacy helper API not placed | C9 evidence; `Examples` conformance; theorems in `Proofs/` | `std/src/test.con` ← `legacy/std/src/test.con` | Not started; conformance examples and kernel-checked program theorems, not tests alone | every showcase and both acceptance programs |

**Stages that need a module before it arrives** (flagged for the owner; the table chooses none of them):

- **grep at 18: command-line input gap.** The adopted `grep PATTERN FILE` stage needs argument access (the [showcase plan](docs/proposals/EXAMPLES.md#showcases)), but args/cli are not placed. The legacy accessor returns an allocating String (`legacy/std/src/args.con` line 22); legacy CLI stores Vec/String (`legacy/std/src/cli.con` line 27, `legacy/std/src/cli.con` line 50). Allocation arrives at 19 and Vec/String at 22 (slices 19 and 22 of the [slice table](#vertical-slices)), after zero-heap slice 18 (slice 18 of the [slice table](#vertical-slices)). A nonallocating argument contract is unresolved; this table does not choose one. The same gap applies to the file encryptor at 18 if it takes command-line paths, which its adopted row does not specify.
- **All early byte consumers: legacy Bytes cannot be carried unchanged.** EVM 15, crypto 15/18, grep fixed buffers 16/18, QOI 19/21, PNG 21 and Wasm 21 need byte operations before growable Vec storage at 22; Bytes itself is unplaced. Legacy Bytes owns raw, allocating storage (`legacy/std/src/bytes.con` line 7, `legacy/std/src/bytes.con` line 30); allocation is 19 and source raw pointers are 24 (slices 19 and 24 of the [slice table](#vertical-slices)). Fixed arrays/views at 15 and HeapArray at 21 support earlier safe implementations; they do not establish a Bytes module placement. The stage rows are the [showcase plan](docs/proposals/EXAMPLES.md#showcases).
- **EVM 21 growable memory: potential container conflict.** The showcase asks for growable memory in its 21 → 23 expansion (the [showcase plan](docs/proposals/EXAMPLES.md#showcases)), while HeapArray at 21 is fixed-length and Vec arrives at 22 (slices 21 and 22 of the [slice table](#vertical-slices)). If growth is required at 21, its implementation contract is missing; the row does not establish that Vec arrives early.
- **Unplaced codec/crypto dependencies.** QOI 19/21, PNG 21–22, EVM 15/21/23 and Wasm 21 need endian byte operations; PNG also needs CRC32, EVM's full version needs Keccak (the [showcase plan](docs/proposals/EXAMPLES.md#showcases)), and the HashMap at 23 behind the KV store, EVM 23 and the Wasm host tables needs a `hash` module, also unplaced (as are the optional `path` and text helpers). Their module arrivals are not placed, so these are scheduling gaps, not demonstrable later-slice conflicts. DEFLATE is an adopted PNG program requirement, not an independently scheduled std module. The crypto 7 stage is scalar only: arrays arrive at 15, so complete array-based ChaCha20/Poly1305/X25519 implementations cannot be claimed at 7 (slices 7 and 15 of the [slice table](#vertical-slices); the [showcase plan](docs/proposals/EXAMPLES.md#showcases)).
- **Wide-integer arithmetic is unplaced.** EVM 15 needs u256 arithmetic and X25519 and Poly1305 at 15/18 need arithmetic wider than u64 (the [showcase plan](docs/proposals/EXAMPLES.md#showcases)); the integer types stop at 64 bits and no limb or wide-integer module is placed, so these stages have no module contract yet.
- **Proposed base64 at 17: legacy implementation conflicts.** This is not an adopted placement (the [showcase plan](docs/proposals/EXAMPLES.md#showcases)). Legacy encode/decode allocate Bytes (`legacy/std/src/base64.con` line 40, `legacy/std/src/base64.con` line 62), before allocation at 19 and growable containers at 22; a fixed-buffer version has no decided API.
- No other definite stage-before-module conflict follows from the adopted rows: maps arrive with store/EVM/Wasm stage 23; network/time arrive with the sequential server at 27. RocksDB acceptance is **after 25**, and ethlambda **after 28**, both subject to the R-0604 dependency audit (R-0604, R-0285, R-0490); their complete module requirements must not be inferred as already audited. No concurrency model is chosen here.

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
OS-level sandboxing (seccomp/landlock, Capsicum, pledge/unveil) is outside the compiler's scope;
users may apply it themselves (owner, 2026-11-06). A target
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
| R-0559 | Outside-reader pass on the approved Core design, then on each feature ([readers](docs/decisions.md#decided-project-outside-readers)) | Codex, agy and Kimi read the decided rows and the integrated spec through the browsable owner view first; every unclear point and confusion is resolved before the owner's formal read and before the freeze (A2c), while real design questions still go to the owner; Kimi's absence is recorded, not waited on; the check repeats for each feature as it lands ([reading order](docs/decisions.md#outside-readers-ordering), owner, 2026-11-07); Home: tooling (the owner view) and the recorded reads. Gate: each participating reader's read recorded, Kimi's recorded absence accepted in its place when it is unavailable, and every confusion resolved before the freeze ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0555, R-0571, R-0496, R-0560, R-0574 |
| R-0511 | Representative design proofs before Core freezes, then cumulative slices with honest theorem scopes | the four A2b theorems are kernel-checked against the proposed `Spec.lean`, without `native_decide`, with their full obligations (decision row Q9.14): `reset_fresh` (a reset gives a brand distinct from every live brand, resets in a loop give pairwise-distinct brands, and the new allocator's operations refuse values of an old brand); `defer_cleanup_releases` (every structured exit runs the deferred calls in reverse registration order, releases each held loan after its call, and preserves the allocator-dependency check); `open_mut_freezes_root` (a mutable heap open excludes every other access to its root, and a shared open never grants a write); `rep_roundtrip` (encode then decode is the identity for `u8` to `u64`, `bool` and the empty struct); each later slice states its theorem scope (A2a was approved 2026-11-05) | — |
| R-0567 | Core freeze (A2c) | the owner approves the freeze of the integrated scopes after A2i's complete acceptance (the four A2b theorems, the integration proofs, the semantic contract, the modular layout, the browsable owner view and the outside-reader pass); the frozen scope is the foundation C1–C10, accepted after its Lean implementation, kernel proofs, interaction tests, review by the four models and the owner ([question 1](docs/decisions.md#foundation-q1)); afterwards a change to a frozen foundation definition is an owner decision, re-runs the earlier proofs and is recorded in `docs/decisions.md`; PROPOSED is removed from its header in the freeze change | R-0571, R-0574, R-0496, R-0560, R-0555, R-0559, R-0607, R-0608, R-0609, R-0610, R-0611, R-0612, R-0613, R-0614, R-0615, R-0616, R-0626 |
| R-0566 | One live home for binding decisions (decision row P20, approved 2026-11-05; the decisions are [recorded](docs/decisions.md#decided-project) in `docs/decisions.md`, which the move places at `docs/decisions.md`) | the still-binding architecture and process decisions of the archived roadmap log (new compiler and retention contract, authority, allocation, Lean as the specification, the owner-recording rule) are in `docs/decisions.md` with their dates and revision links; the owner-approved revisions (OD45/OD87, TYPE-22, TRAIT-1 and any other accepted row) are recorded there; ROADMAP.md, PHILOSOPHY.md and AGENTS.md cite it and restate nothing; a gate refuses a roadmap rule link into the archived log once the move lands | — |
| R-0560 | One authoritative spec library: the normative definitions in cohesive modules under `ConcreteSpec/`, with `ConcreteSpec/Spec.lean` as the public import; proofs, tooling and derived views in files that import it (revised 2026-11-06) | the layout exists before the freeze; a gate refuses a normative definition outside the library and a proof inside it, each with a negative control | A2b |
| R-0571 | Integrate the proposed spec before the freeze (A2i): shared Core interfaces, one integrated semantics and interaction proofs (decided 2026-11-06) | the parts share Core interfaces and one semantics; kernel-checked proofs cover a borrow across deferred cleanup, an allocator reset with outstanding loans and cleanup that needs authority; README obligations [O7 and O9](ConcreteSpec/README.md) are discharged; no `sorry`, no `native_decide`, only the three standard axioms | R-0511, R-0560, R-0607, R-0608, R-0609, R-0610, R-0611, R-0612, R-0613, R-0614, R-0615, R-0616, R-0626 |
| R-0588 | Fact and capability preservation across indirect calls, specialization and packages | at A2i, the integrated spec states that capability, allocation, termination, may-abort and evidence facts are preserved through indirect calls, monomorphization and package boundaries; at A3, the theorem is stated with its holes; at B0 and B1, it is proved for each slice's fragment and the facts query reports through these boundaries; later fragments prove it as they land | R-0571 |
| R-0592 | Fix the checker result type, the leakage-trace interface and the byte-accounting instrumentation before the freeze ([diagnostics](docs/decisions.md#decided-project-diagnostics-2026-11-06), [memory](docs/decisions.md#decided-project-memory-2026-11-06), [information flow](docs/decisions.md#decided-project-infoflow-2026-11-06), 2026-11-06) | the integrated spec's checker returns a typed result with one refusal constructor per premise; the semantics carries the leakage-trace interface (branches, addresses, allocation sizes, loop counts, variable-time operands) and sized-allocation accounting per allocator lineage; each is kernel-checked against its examples; changing them after the freeze would be a Core change | R-0571 |
| R-0602 | An extensible observable-event kind in Core | before the freeze, Core has an observable-event kind that later events extend; secret wipes are protected as observable; device semantics come later | R-0612 |
| R-0603 | Rule revisions, the retired-ID registry and `spec_version` | each Lean rule has its own revision, bumped when its meaning changes, including through dependencies, and checked; retired rule IDs stay in a registry; every output carries one `spec_version` header covering every record (0.x before the freeze, 1.0 at A2c, bumped by every normative change), checked in CI and distinct from the product version | R-0571 |
| R-0607 | Foundation C1: execution, calls and cleanup ([contract](docs/proposals/FOUNDATION.md#c1-execution-calls-and-cleanup), [decision](docs/decisions.md#decided-2026-11-08-foundation)) | machine configurations, the labeled single-step relation and its results (completion, return, labeled break and continue, abort) are in Lean; left-to-right evaluation, call by typed identity (`Type::f(x)` or `Trait::f(x)`, with an ordinary typed first parameter and no receivers, [decision](docs/decisions.md#decided-2026-11-08-language) item 7), and cleanup on every structured exit (inner scope first, reverse registration order, abort runs none) are rules with accepted and refused examples; sequencing, scope restoration and cleanup preservation are kernel-checked, and the old `Cleanup` fragment is ported and deleted; Home: spec (`Syntax`, `Semantics`). Gate: Spec build, the rule gate and the proof-integrity jobs over its theorems | R-0610, R-0612 |
| R-0608 | Foundation C2: scalars, failure and arithmetic ([contract](docs/proposals/FOUNDATION.md#c2-scalars-failure-and-arithmetic)) | the mathematical scalar values, their bit representations, every operation's contract and the error classification are in Lean, with the correspondence laws; traps, unsafe stuckness, allocation failure and fuel exhaustion stay separate outcomes (Q6.7); the trap kinds are `overflow`, `division_by_zero`, `invalid_shift`, `out_of_bounds`, `invalid_range` and `abort`, diagnostic categories distinct from the checked operations' error types ([question 9](docs/decisions.md#foundation-q9), [decision](docs/decisions.md#decided-2026-11-08-process) item 4); contracts mark exact mathematical arithmetic explicitly ([decision](docs/decisions.md#decided-2026-11-08-language) item 10, spelling in R-0625); Home: spec (`Types`, `Scalar`). Gate: Spec build, the rule gate (each operation's accepted and failing examples) and the proof-integrity jobs | — |
| R-0609 | Foundation C3: target, layout and representation ([contract](docs/proposals/FOUNDATION.md#c3-target-layout-and-representation)) | the read-only target profile (address width, pointer storage, length width, byte order, address spaces, maximum object sizes) and the layout interface are in Lean, with the existing 64-bit little-endian profile as an instance ([question 3](docs/decisions.md#foundation-q3)); the layout laws (alignment, size, in-bounds non-overlapping fields, unique discriminants, valid tag decoding) are proved for the native and C algorithms (packed only if a demonstrated packed foreign ABI reopens it, [decision](docs/decisions.md#decided-2026-11-08-language) item 23); `i32` enum tags, no niches, `Secret<T>` with `T`'s layout; `rep_roundtrip` is restated over it and `Rep` is deleted; Home: spec (`Target`, `Layout`, `Values`). Gate: Spec build, the rule gate and the proof-integrity jobs | R-0608 |
| R-0610 | Foundation C4: byte memory and value validity ([contract](docs/proposals/FOUNDATION.md#c4-byte-memory-and-value-validity)) | the hybrid state (structured places, byte and provenance blocks, the partial projection and `Rep`, materialization on raw address-taking, `expose` or foreign use) is in Lean ([question 2](docs/decisions.md#foundation-q2)), with the typed-write store rule, partial decoding, access validity, provenance and `from_exposed` (Q3.9), zero-size slots and the allocator availability laws (Q1.11); round-trip and decode-validity are kernel-checked; Home: spec (`Store`). Gate: Spec build, the rule gate and the proof-integrity jobs | R-0609 |
| R-0611 | Foundation C5: dynamic borrow, brands and authority ([contract](docs/proposals/FOUNDATION.md#c5-dynamic-borrow-brands-and-authority)) | the preservation invariant over owner slots, moves, roots and projections, loans, brands, dependencies and grants is in Lean and proved preserved by each step and across calls; whole-root conflicts, generative brands and grant narrowing are kept; release-context erasure only with its proof ([question 6](docs/decisions.md#foundation-q6)); the old `Borrow`, `Brand` and `Authority` fragments are ported with `open_mut_freezes_root` and `reset_fresh`, and deleted; Home: spec (`Ownership`, `Borrow`, `Brand`, `Authority`). Gate: Spec build, the rule gate and the proof-integrity jobs | R-0607, R-0610, R-0612 |
| R-0612 | Foundation C6: observations, environment and the concurrency extension ([contract](docs/proposals/FOUNDATION.md#c6-observations-environment-and-concurrency-extension)) | extensible typed event signatures, request and response matching, finite and infinite traces, terminal observations and the environment relation are in Lean; the trap line, exits 134 and 125, and byte columns with a tab counting one byte ([question 10](docs/decisions.md#foundation-q10)) are rules with examples; the sequential-fragment preservation law for a later concurrency extension is stated; this delivers R-0602's extensible event kind; Home: spec (`Contract`). Gate: Spec build, the rule gate (trap-line conformance examples) and the proof-integrity jobs | R-0608 |
| R-0613 | Foundation C7: secret leakage and accounting ([contract](docs/proposals/FOUNDATION.md#c7-secret-leakage-and-accounting)) | the leakage trace, the public-observation relation, release records, the release-policy interface and byte accounting per allocator lineage are in Lean, with the noninterference and constant-time statements as separate theorems over equal release traces; wipe events are observable; Home: spec (`Leakage`). Gate: Spec build, the rule gate and the proof-integrity jobs | R-0607 |
| R-0614 | Foundation C8: evaluator adequacy and termination ([contract](docs/proposals/FOUNDATION.md#c8-evaluator-adequacy-and-termination)) | Q9.9's total fuel-indexed evaluator over the hybrid state, with oracle choices checked against operation contracts, is proved sound, complete, monotone in fuel and prefix-consistent against C1's step relation, and adequate for infinite behaviour; `outOfFuel` is only an insufficient budget; constants are computed at compile time, their values are `Copy`, and the closed total initializer subset is defined, with the decided reopening conditions checked, before Q6.4 and TYPE-22 are revised ([decision](docs/decisions.md#decided-2026-11-08-language) item 26); Home: spec (`Semantics`). Gate: Spec build, the rule gate and the proof-integrity jobs | R-0607 |
| R-0615 | Foundation C9: identities, primitive contracts, evidence and ghost erasure ([contract](docs/proposals/FOUNDATION.md#c9-identities-primitive-contracts-evidence-and-ghost-erasure)) | the typed identities and rule ids are in Lean (`Ident`, `Rules`, PLAN step 2, on `wip/spec-core`); one primitive-contract interface with a lawful extensible registry refuses unknown operations ([question 16](docs/decisions.md#foundation-q16)); that interface is written early (SPEC_UNIFICATION step 6) and C1 and the later contracts build on it, while this task's acceptance comes last, with evidence and erasure (step 19); the typed evidence graph and the one versioned diagnostics and facts schema; a dead store to a `mut` local refused, extending OD62, and the optional `#[depends(result <- a, b)]` checked against the derived fact, never trusted ([decision](docs/decisions.md#decided-2026-11-08-language) items 28 and 29); the struct-invariant exposure definition ([question 15](docs/decisions.md#foundation-q15)); erasure of ownership, brand, loan, accounting and ghost instrumentation is kernel-checked (O3); Home: spec (`Ident`, `Rules`, `Primitives`, `Evidence`, `Facts`). Gate: Spec build, the rule gate, the proof-integrity jobs, and the one schema's Lean types; the JSON generated from them with goldens is R-0593's, after integration | R-0607, R-0611, R-0613 |
| R-0616 | Foundation C10: frames and ABI ([contract](docs/proposals/FOUNDATION.md#c10-frames-and-abi)) | logical footprints and the frame law, call-frame ownership and restoration, and ABI signatures (registers, stack slots, preserved state, return convention, clobbers, stack-accounting handoff, extern validity and retention, launcher entry and exit) are in Lean; the frame law is kernel-checked for the foundation's operations; Home: spec (`Frame`). Gate: Spec build, the rule gate and the proof-integrity jobs | R-0610, R-0607 |
| R-0626 | Audit every foreign declaration for the `Process` capability ([decision](docs/decisions.md#decided-2026-11-08-language) item 20) | every foreign declaration is listed with whether it needs `Process`; `Process` is removed from the reserved set only if none does; Home: tooling (the audit record), then spec (the authority rules) if it is removed. Gate: the recorded audit, and a refusal control for `Process` if it is removed ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | — |
| R-0627 | Replace local `assume` only after its replacement is shown, from slice 8 on and not a prerequisite of the freeze ([decision](docs/decisions.md#decided-2026-11-08-language) item 24) | named trusted contracts with evidence identity are shown on real examples, point-local conditional facts included; only then is `assume` removed; until then it stays explicit, tracked and never admits a safety obligation; Home: spec (`Evidence`). Gate: the worked examples reviewed, then the owner's removal approval ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0615 |
| R-0574 | No hidden traps and bounded execution ([decision](docs/decisions.md#decided-project-no-hidden-traps), 2026-11-06): the Core rules in the spec, then the checker | the three spellings are decided (`checked_*` with `ArithmeticError`, `#[proof(...)]`, `#[may_not_terminate("reason")]`); within A2i, before the freeze, the grammar data gains the attribute-prefix production for expression attributes, kept LL(1), and the proposed spec has the admission rule with `checked_no_implicit_traps`, the checker as an executable Lean definition with its correspondence and soundness proofs, the octagon fact language and its transfers, the loop rule, closed contract evidence, the termination rule with source permissions, and the per-function execution facts with `bounded` derived, each with accepted and refused examples (`add100` refused, with guarded variants); the compiler's use of that checker at each stage, the per-function facts report and the per-compilation evidence follow with the slices (phase B), slice 0 first; each guarantee is published only when its evidence lands; the stack target contract (claim 1, then claim 2) is target work | R-0511 |
| R-0496 | The semantic contract (outcomes, traces, authority, resources, the error model) and the clock-indexed evaluator | the contract is stated in Lean; the evaluator runs; fuel exhaustion is distinct from divergence; trap location and message (the line `concrete: trap: <kind> at <path>:<line>:<col> (site <id>)`, the path relative to the project root with forward slashes, line and column 1-based), the exit-code mapping and the target observations are part of the contract, and until they are, every claim about executable behaviour is qualified by the model's abstract observations | R-0560 |
| R-0502 | One value/raw-memory boundary and one layout authority | the boundary is named in the theorem; layout has one owner with its proofs; the default struct layout is declaration order, natural alignment defined exactly per target, size rounded up to the alignment, zero-sized fields taking no bytes, with no C ABI promise | R-0496 |
| R-0506 | Every proof boundary and validator obligation stated with its premises and status | obligations have explicit premises; transitive axiom classification is enforced; CI audits semantic-premise closure separately from logical axioms and rejects unapproved assumption additions or strengthening | R-0496 |
| R-0508 | The end-to-end theorem: maximal-behaviour refinement, frontend correspondence, resource adequacy, image transport | stated in Lean with its holes; proved progressively per slice; every target behaviour theorem has the [target-theorem shape](#target-theorem-shape); the concrete slice-0 theorem is stated and composed before B0 closes, retaining the maximal-behaviour, resource, observation and image-transport obligations | R-0496, R-0502 |
| R-0509 | The trusted-base inventory, evidence binding, assumption lifecycle and axiom replay | the inventory is on main; from slice 0 `concrete facts` states what is not proven: the Lean kernel, LLVM printer and LLVM, LCB's MIR→assembly step, linker, OS and named runtime premises ([early decision](docs/decisions.md#decided-2026-11-07-early-evidence)); every claim's axioms are recorded and replayed from a clean checkout; CI audits semantic-premise closure separately from logical axioms and rejects unapproved assumption additions or strengthening; Home: spec (the trusted-premise list). Gate: a trusted-base golden ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0506 |
| R-0500 | Each decided language rule gets its Lean judgment, semantic clause and accepted, refused or trapping examples | each rule lands with its slice; every rule of the decided language has one by the end of D | R-0560 |

### B. Cumulative slices up to LP0

| task | objective | done when | depends on |
|---|---|---|---|
| R-0425 | The conformance format and suite: each example names its rule, source and outcome (result, refusal code or trap), and one runner per implementation runs it through the Lean semantics and through the compiler | slice 1's examples pass both runners; the suite is versioned and published at 0.1, and complete at D; first applicable increment at B0; the approved slice-1 obligations remain due at B1 | R-0560 |
| R-0499 | Independent Core judgments and an authoritative checker proved to correspond | correspondence proved for slice 1, with positive and rule-breaking mutation controls; first applicable increment at B0; the approved slice-1 obligations remain due at B1 | R-0500 |
| R-0492 | Scope-resolved identities throughout; lexical identity distinct from persistent evidence identity | alias, symbol, binding and forgery controls hold | R-0499 |
| R-0493 | Typed arithmetic and casts from one semantics, with arithmetic-site facts | widths, signedness, modes and the defined trap match the Lean semantics; first applicable increment at B0; the approved slice-1 obligations remain due at B1 | R-0496 |
| R-0514 | The compiler consumes the exact identity-resolved Core payload checked against the judgments | substitution controls hold | R-0499 |
| R-0501 | Proved lowering passes for sequencing, matching, cleanup and check expansion | each pass preserves behaviour and re-establishes invariants; the slice-0 instances are delivered at B0 and extended cumulatively at B1; every used lowering has preservation and output-invariant proofs | R-0514 |
| R-0503 | The unverified BIR → LLVM path, the native driver and the pinned LLVM toolchain | output labelled unverified; LLVM, linker and runtime assumptions listed in their own inventory, separate from the LCB premises; the driver bounds external tools; the slice-0 instance is delivered at B0 and extended cumulatively at B1, labelled unverified | R-0501, R-0581 |
| R-0581 | BIR as a complete backend contract shared by LLVM and the LCB ([decision](docs/decisions.md#decided-project-proof-infrastructure), 2026-11-06) | BIR's Lean definition fixes integer semantics, layout and endianness, alignment, zero-sized values, internal and foreign calling conventions, external events, trap realization (kind and site) and a frame-accounting handoff; no BIR construct, invariant or lowering choice depends on one backend; stack bytes are never derived from BIR slot counts, only from allocated frames in the final artifact; the slice-0 instance is delivered at B0 | R-0496, R-0502 |
| R-0580 | A Memory CFG stage (layout and ABI lowering) between Place MIR and BIR | one layout authority (R-0502) assigns offsets, alignment, loads and stores over a block–offset memory model, consuming the spec's byte encoding; layout and provenance appear nowhere in SSA construction or either printer; the stage may be the identity in slice 0, with its preservation theorem stated, and is real by slice 9 (structs) | R-0502, R-0501 |
| R-0583 | The per-pass proof-or-validator table | each pass is classified as proved directly (the stable, structural ones: parsing, sequencing, matching, cleanup, representation lowering, BIR → LCB-IR) or checked by a proved validator (the search-heavy or changing ones: specialization, SSA construction and destruction, optimizations, register allocation, scheduling, linking), with the reason; the lexer is proved against an independent byte/token judgment; an unclassified pass fails the gate; the slice-0 rows are delivered at B0 | R-0501, R-0504 |
| R-0578 | The unverified LCB build from slice 0, and the cross-backend comparison ([decision](docs/decisions.md#decided-project-proof-infrastructure), 2026-11-06) | every supported example is built through BIR → LCB-IR → the Lean Compiler Backend, labelled unverified, beside the LLVM build; CI runs both and compares their full observations (exit status, the fd-2 trap line, startup refusal, later the event traces and external state), never exit codes alone, with each other and with the expected outcome, and any mismatch fails; each native trace is also checked as a refinement of Core's behaviour ([question 17](docs/decisions.md#foundation-q17); allocation-dependent observations are decided at slice 24); the LCB output runs natively on a GitHub-hosted AArch64 macOS runner, the first native proof profile's platform; a planted mismatch in either backend is caught | R-0581, R-0503 |
| R-0579 | The BIR → LCB-IR adapter refinement proof, the first backend proof target | the adapter's refinement against LCB's imported semantics (not a restatement) is kernel-checked for the slice-0 subset right after the front-end and lowering proofs, at B0, and extended to slice 1 at B1; the theorem is complete: an adapter error produces no artifact, and success implies correctness, so no adapter premise remains ([decision](docs/decisions.md#decided-2026-11-08-adapter)); the end-to-end theorem names the LLVM and LCB premises separately | R-0578, R-0501 |
| R-0497 | Immutable pipeline artifacts bound to exact payloads | forgery, mutation and bypass compile-fail controls | R-0514 |
| R-0504 | Validators with explicit soundness propositions | substitutions and wrong artifacts are rejected without inflating native assurance | R-0497 |
| R-0507 | Type and ownership soundness over the executable semantics | proved for slice 1; first applicable increment at B0; the approved slice-1 obligations remain due at B1 | R-0496, R-0499 |
| R-0521 | Minimal provenance and one canonical verification task for the first authoritative claims | a claim names its compiler, source, theorem and executable digest; minimal provenance and kernel-checked per-compilation evidence accompany the first authoritative slice-0 claim; the evidence connects the exact input to the exact emitted BIR without trusting native evaluation, the backend input is bound to that BIR, and the Lean compiler, runtime and toolchain are listed as execution assumptions wherever the evidence does not close the gap (2026-11-06) | R-0509 |
| R-0552 | Per-compilation certificates (owner-approved 2026-10-25) | a build emits a certificate per output, and the kernel checks it; minimal provenance and kernel-checked per-compilation evidence accompany the first authoritative slice-0 claim; each certificate connects the exact input to the exact emitted BIR through proved judgments or validators, checked by the kernel without trusting native evaluation; the backend input is bound to that BIR (a digest is identity, not correspondence); residual Lean compiler, runtime and toolchain trust is listed as explicit execution assumptions; LCB verification of compiled programs is not counted as verification of the compiler's execution (2026-11-06) | R-0504 |
| R-0582 | Per-pass certificates in place of the whole-compiler `decide` certificate | after slice 0, a per-compilation certificate composes universal per-pass theorems with small per-edge witnesses, linear in program size, instead of making the kernel re-run the whole compiler by reduction; slice 0 may keep `decide`; a planted wrong witness at any edge is rejected | R-0552, R-0583 |
| R-0528 | Resource limits on input size, memory, time and import closure | each input class has a limit and a hostile fixture | R-0503 |
| R-0573 | Small adversarial interaction workloads from the first slices on, alongside file-copy (decided 2026-11-06) | each slice adds programs that combine its features with earlier ones in difficult ways (for example borrows across cleanup, authority in cleanup, nested views), each with its expected outcome and Lean/native differential check | R-0425, R-0503 |
| R-0575 | The per-function facts report and query from slice 0 ([decision](docs/decisions.md#decided-project-facts-report), 2026-11-06) | every function of each slice has a report derived from the Lean rules, with a versioned schema: capabilities and the grants and resources actually reachable, allocation and allocator, trusted, foreign and opaque boundaries with their assumptions, each operation that could fail and how it is handled, termination (evidence with its named premises, or the responsible source permission and the call path to it), recursion depth and stack bound or why none is established, proof status per obligation, and completeness, each with its reason, evidence and source location; a permission that justifies nothing is reported as a fact, which a `Concrete.toml` policy may refuse (the decided refusals of an unmatched `#[proof]` and a duplicate `#[may_not_terminate]` stay) (carried from slice 0 into CheckedCore and the schema, spec obligations O1 and O4); the schema already has fields for what memory a function writes (frames) and what its outputs depend on (dependency flow), reported "not established" until derived: definite, maybe and never writes per `&mut` place and the output-to-input dependencies (control dependence, cleanup and opaque calls included) are derived into these fields as soon as `&mut` lands, not in E–G, while R-0524, R-0473 and R-0383 keep source frames, contracts and package summaries; facts are preserved across indirect calls, specialization and package boundaries (R-0588); from slice 0 the compiler answers a query about one function or module from the same facts, as readable text and in the machine format (the command is `concrete facts`); the report never claims what the rules do not establish; every fact is a typed judgment in Lean with a derivation-soundness theorem (and obligation-discovery completeness where it claims absence), one derivation feeds the readable view, the machine format and every policy, each fact has a closed status (established, conditional, refuted, not established, not applicable) with per-fact coverage, unknown never satisfies a policy, and answers carry typed identities, versioned schema and semantics and artifact digests; from slice 0 the report also carries the build identity (compiler SHA, toolchain pins and target), each program's maximum stack use, measured from the final binary on both backends, and the trusted-base statement naming the Lean kernel, LLVM printer and LLVM, LCB's MIR→assembly step, linker, OS and named runtime premises as not proven ([early decisions](docs/decisions.md#decided-2026-11-07-early-evidence)); the diagnostics and facts JSON schema is frozen and versioned from the first front-end output, with golden tests; a planted wrong fact and a planted unknown version are caught; Home: spec (the stack-bound claim and, from slice 0, the refusal of an unknown or over-budget bound against the `Concrete.toml` stack budget) and tooling (the report). Gate: stack, the final-artifact bound checked against the claim and the budget ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0499 |
| R-0625 | The spelling round ([decision](docs/decisions.md#decided-2026-11-08-spelling-round), items 55 and 59) | the exact names and signatures of the lossless conversion, the move-out, the mathematical-arithmetic boundary, output and append, the typing rules for destructuring and ghost bindings, every spelling from the 2026-11-08 items 6–29 and 40–53, and the remaining grammar sub-rules of item 59 are shown to the owner as before/after code and approved before the Lean grammar is written; Home: a review sheet, then spec (`Grammar`). Gate: the owner's recorded approval ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | owner: the spelling round |
| R-0622 | The written grammar before the parser ([decision](docs/decisions.md#decided-2026-11-08-written-grammar)) | the full surface grammar is written down for the owner's review, every construct with its decided row and the open spellings flagged; the owner approves it; it is encoded in Lean as the independent grammar judgment (row Q8.3, SPEC_UNIFICATION step 8's `Grammar`); only then is the parser built against it; Home: spec (`Grammar`), after a review sheet outside the repository. Gate: the owner's recorded approval, then the grammar's conformance examples and `ll1_sound` | R-0625, owner: the grammar review |
| R-0620 | The lexer and the LL(1) parser, started now in parallel with the foundation ([decision](docs/decisions.md#decided-2026-11-08-front-end)) | the lexer for slice 0's surface runs against the spec's `Lexical` rules as they land (SPEC_UNIFICATION step 8) and does not wait for R-0622, which only the parser and so this task's completion wait for; and the parser is built against the Lean grammar judgment once the owner has approved the written grammar (R-0622), with refusals as diagnostics records (R-0593, which this task feeds rather than waits for); correspondence to the grammar is proved as part of B0; Home: tooling (the compiler's front end), against the spec. Gate: the conformance examples of slice 0's lexical and grammar rules, accepted and refused | R-0622 |
| R-0593 | The diagnostic and facts records, their frozen, versioned JSON schema, goldens and the parser's diagnostics from the first front-end output ([diagnostics decision](docs/decisions.md#decided-project-diagnostics-2026-11-06), [early decision](docs/decisions.md#decided-2026-11-07-early-evidence)) | every slice-0 refusal is one typed record rendered as human text and JSON lines (`--format=human|json`), with rule IDs from the Lean rule name and that rule's own revision (R-0603); the diagnostics and facts JSON schema is frozen and versioned from the first front-end output, and golden tests cover it and both diagnostic renderings; the LL(1) parser fills its expected-token set from the grammar data; B0's classified refusal controls use the records; Home: spec (the diagnostics and facts types are the schema source). Gate: the JSON schema generated from the Lean types, with goldens ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0592, R-0575 |
| R-0594 | Zero-heap facts proved in slice 0 ([decision](docs/decisions.md#decided-project-memory-2026-11-06)) | every slice-0 function's heap facts are 0, established by the derivation (no allocator is reachable), not by schema defaults; a planted reachable allocator flips them | R-0575, R-0592 |
| R-0585 | Vacuity checks and contract-strength labels from the first proof slice | no fact reads "proved" when its theorem holds only under an unsatisfiable precondition or premise set, from the first proof slice on; the checker reports a context as infeasible wherever its octagon is empty, and a discharge inside an infeasible context is marked vacuous, never established, unless the infeasibility itself is the claim (a dead branch), applying OD120 from B1 rather than F; each proved claim carries its strength (a point instance, one direction, an equivalence or a full contract), so a single-input proof never reads as a universal contract; planted vacuous and point-only proofs are caught; obligation sufficiency stays with R-0460 and the phase-F validation project with R-0219 | R-0575 |
| R-0599 | Replayed counterexamples for "not established", without SMT | a deterministic bounded search evaluates a failed obligation's path constraints on boundary points (type extremes and the octagon's constants) and replays a candidate through the Lean reference evaluator with the exact semantics, inputs, resources and environment, satisfying the preconditions; a replayed witness reports "refuted, replayed", otherwise "no witness within N cases", never "safe"; the search never affects admission; due at B1 | R-0575, R-0586 |
| R-0600 | A derived "always aborts" fact (SPARK `No_Return`) | a function whose every path aborts gets a derived fact, with no annotation and no function category (OD116); the facts report shows it, and the checker uses it to prune paths after a call to an abort wrapper within the decided divergence typing (TYPE-4@2); any change to the join rule goes to the owner; due with the functions-and-calls slice | R-0575 |
| R-0589 | Declared assumptions in `Concrete.toml`, with a change-detecting check ([decision](docs/decisions.md#decided-project-concrete-toml), 2026-11-06) | a project declares its target, heap, stack, foreign-function and trusted-surface assumptions in its `Concrete.toml` only where the facts cannot derive them; the section is typed, versioned data and an unknown key or version is refused; the facts query reports each assumption with the facts that rely on it; any change to the declared set is reported as an assumption diff and fails CI until the diff is recorded with the change; the sections are the typed, versioned `[policy]` and `[assumptions]` tables | R-0575 |
| R-0586 | The I/O driver for the Lean reference evaluator | the Lean evaluator runs programs that read and write their grants, so I/O examples get expected outcomes independent of the compiler; its infrastructure lands with slice 1 (B1), and from slice 16 (the console) it runs every supported I/O example and produces its expected observations independently of the compiler, and R-0491 uses them | R-0496, R-0425 |
| R-0576 | The predictable policy ([decision](docs/decisions.md#decided-project-predictable-policy), 2026-11-06) | an optional module or project policy, built on the per-function facts (R-0575), refuses at compile time any function in its scope that allocates, blocks or calls foreign code, and, when the policy demands it, any function without termination evidence or a stack bound; admission requires established evidence of no allocation, no blocking and no foreign calls, so an unknown or incomplete fact is refused; the policy explicitly states whether named trusted assumptions are acceptable (Q13); a violation names the function, the fact and its source; the query reports which policy covers each function; a planted violation and a planted unknown fact are both refused; the policy is declared in the project's `Concrete.toml` ([decision](docs/decisions.md#decided-project-concrete-toml)) as typed, versioned data, a module may tighten it but never relax it, and dependencies are checked against the consumer's policy transitively and fail-closed; Home: tooling (the optional `Concrete.toml` policy over the facts; the mandatory slice-0 stack-budget refusal is R-0575's). Gate: the policy's refusal controls ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0575 |
| R-0577 | Reproducible builds checked in CI from slice 0 ([decision](docs/decisions.md#decided-project-reproducible-builds), 2026-11-06, revised 2026-11-07) | the same source, compiler revision, toolchain pin and target give byte-identical BIR, LLVM IR text, LCB job and output, and final binary; pins cover the nix flake, LCB revision and LLVM/clang version; artifacts contain no timestamps, absolute paths or host names (`-ffile-prefix-map`, `SOURCE_DATE_EPOCH`, deterministic link order), and output never depends on hash-map iteration order; CI builds every slice-0 example twice in different directories and on a GitHub runner and concrete-ci-2 and compares SHA-256, failing on any difference; `concrete facts` carries the compiler SHA, toolchain pins and target; work started on `wip/slice0-repro`; Home: tooling (build pins and CI). Gate: reproducibility, every slice-0 example built in two directories and on two machines (a GitHub runner and concrete-ci-2), SHA-256 compared ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0503 |
| R-0479 | Failures and refusals are never turned into facts; diagnostics name the valid next action ([decision](docs/decisions.md#decided-project-diagnostics-2026-11-06)) | refusals survive through results, text, JSON and exit codes; fixes are offered only rechecked and labelled, and applied automatically only when a lemma proves them meaning-preserving; there are no warnings | R-0499 |
| R-0118 | Generated coverage of passes, consumers, constructs, operations and traps | a missing row or a synthetic constructor fails the gate; B0 includes pipeline-edge coverage and controls for missing proofs, unregistered premises, wrong artifact substitution and reject-all compilation | R-0501 |
| R-0491 | The defect inventory and the classified differential (Lean semantics, native build, legacy oracle) | comparisons are classified and campaigns are non-vacuous; its check refuses any expected outcome recorded from compiler output (I/O expectations come from the Lean evaluator's driver, R-0586) | R-0425, R-0564, R-0586 |
| R-0498 | Every enforcement claim maps to its gate leg and a targeted mutation | an unmapped claim fails; shape checks are labelled unproved; B0 includes pipeline-edge coverage and controls for missing proofs, unregistered premises, wrong artifact substitution and reject-all compilation | R-0118 |
| R-0272 | Restart fuzzing against the new compiler on concrete-ci-2, including the random-program differential against the Lean evaluator from the slice-0 front end ([early decision](docs/decisions.md#decided-2026-11-07-early-evidence)), seeded from the regressions and examples that survive in the repository and from newly generated programs (the ci-3 corpus and findings were not copied before the handover, decision row P14, and are lost), with reduction and counterexample-to-regression persistence | the random-program differential starts with the slice-0 front end and grows one feature per slice; the generators emit slice-supported programs, the oracle is Lean semantics against native code, and findings become regressions; legacy fuzzing stopped at the move; Home: tooling. Gate: the fuzzer, short in CI and long nightly on concrete-ci-2 ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0425 |
| R-0550 | Compiler speed, scaling and binary-size tracking from slice 0 ([speed decision](docs/decisions.md#decided-2026-11-07-performance), [early decision](docs/decisions.md#decided-2026-11-07-early-evidence)) | wall time, per-stage time and memory are recorded by exact SHA, backend and target from slice 0; binary size is tracked per example per commit next to speed tracking and is report-only; a compile-speed budget is tracked per example, report-only like binary size ([decision](docs/decisions.md#decided-2026-11-08-compile-budget)); speed measurements are report-only until their noise is understood, then scaling gates hold for each stage and a planted regression is caught; Home: tooling. Gate: speed and binary size, report-only ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0503 |
| R-0630 | Gate 2 for `Concrete/`: axioms and trust over the compiler | over every constant of `Concrete/`: only the three axioms, no `sorry`, no `native_decide`/`ofReduceBool`, the totality ban (no `partial`, `unsafe`, `@[implemented_by]`, `@[extern]` or `opaque` outside `Premises.lean`) and the external-checker replay; today only the trusted-base golden covers the headline theorems' axioms (slice-0 worker, 2026-11-08); Home: tooling (CI). Gate: the job itself, with a planted `sorry`, a planted `partial` and a planted axiom refused ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0553 |
| R-0553 | Totality audit: every semantics, checker, validator and pass is total and runs as proved | no partial definition; opaque execution gaps controlled; totality and execution-gap checks apply from B0; B3 performs the cumulative audit after B2 | R-0501 |
| R-0562 | The retention manifest `RETENTION.tsv` at the root, written before the move and checked before any deletion under `legacy/` | every legacy asset (corpora, regressions, examples, std, the bug ledger, the site, docs and the archived services) has a row with a destination and a deletion condition; deleting a path under `legacy/` whose row is unresolved is refused (control); the manifest and the task disposition table stay at the root and survive the deletion of `legacy/` (LD) | — |
| R-0561 | Bug ledger by compiler | legacy defects are marked retired with the old implementation, never fixed, each keeping its reproducer, ruling and structural class; std defects move with std as contracts and regressions; every legacy ruling that states language behaviour becomes a conformance example or a recorded owner question; the minimal issue record of the day-one root becomes the new ledger with the first slice | R-0425 |
| R-0569 | A new website, built from scratch with the new compiler (decision row P9) | the site is designed anew around the decided language; its playground runs the new compiler; it teaches and links the Lean rules and states no rule of its own; no page describes the legacy compiler or the language before the 2026-11 decisions as current | B1 |

### LP0. The LCB pilot

| task | objective | done when | depends on |
|---|---|---|---|
| R-0572 | The LCB pilot (LP0): a BIR → LCB → native proof on the subset of the slices before LP0, before substantial feature growth (decided 2026-11-06, revised the same day: the LCB joins slice 0) | the subset, already built unverified through the LCB since slice 0 (R-0578), carries a native proof: the adapter proof (R-0579) composed with the LCB's theorems for its exact domain and with the slice theorem under named premises, kept separate from the LLVM premises; every remaining backend obligation has its own task and milestone; native proof closure for the profile (R-0517) follows | B1, R-0579 |

### C. File-copy and 0.1

| task | objective | done when | depends on |
|---|---|---|---|
| R-0052 | Handle-relative filesystem: directories and files carry authority | file-copy opens, reads, writes and closes through directory values | B, LP0 |
| R-0557 | Global state as capabilities: no duplicable linear constants; `main` receives explicit target grants | grants reach `main`; constant linearity is enforced | B, LP0 |
| R-0023 | The decided entry-point signatures and explicit exit and error behaviour | implemented and tested in the new compiler | R-0557 |
| R-0071 | Std on the new compiler: modules return when their constructs land, against the completion contract | each returned module has its manifest, coverage and error conventions | R-0052 |
| R-0587 | Debug info on the LLVM path | unverified DWARF output on the LLVM profile, with a check that enabling it changes no generated code | R-0503 |
| R-0556 | Generated module interfaces and the interface-diff gate | every public API change shows in the diff | R-0071 |
| R-0510 | Checked specialization | specialization is validated against the generic definition | B, LP0 |
| R-0162 | Reusable lemmas for structs, enums, matching, `Result` and `Option` | file-copy's theorems use them | B, LP0 |
| R-0301 | Backend contracts: ABI, layout, sanitizer and FFI controls | from slice 0, every example's LLVM output is also built and run under AddressSanitizer in CI ([early decision](docs/decisions.md#decided-2026-11-07-early-evidence)); UBSan is not run, since it instruments C at clang's front end and checks nothing on LLVM IR input ([decision](docs/decisions.md#decided-2026-11-08-ubsan)); the validation project passes for the 0.1 subset; Home: tooling. Gate: ASan runs of every example ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0503 |
| R-0334 | The first-user teaching path (the guide) | a checked tutorial covers install to file-copy | R-0071 |
| R-0516 | Release automation for nightlies and trains | a nightly publishes from a qualified SHA | T |
| R-0333 | Release criteria and candidate records, including claims, security and soundness disclosure, compatibility and operations | the 0.1 candidate record is complete | C |
| R-0591 | A short ideas sweep of `legacy/` for phase C's area, at the latest at C's start (owner, 2026-11-06) | at the latest before C's first slice lands (it may run earlier), the ideas of `legacy/` in C's area are listed and each is dispositioned (carried, planned, deferred or dropped on purpose) in a recorded sweep | R-0562 |
| R-0596 | `Secret<T>` with its noninterference and constant-time proofs, from slice 7 ([decision](docs/decisions.md#decided-project-infoflow-2026-11-06)) | at slice 7, scalar `Secret<T>`, `classify` (of a value passed in), `declassify(x, "reason")`, `ct_select` and secret comparisons land; secret control flow, secret indices and secret lengths are refused; scalar secrets are `Copy`; under the randomness decision, the entropy capability arrives at slice 6, its output is `Secret<T>` at slice 7 and fill-bytes arrives at slice 15, with its source a named trusted premise in facts ([decision](docs/decisions.md#decided-2026-11-07-stack-randomness)); later increments add `ct_eq` on arrays and views and linear, wiped secret arrays at slice 15, wiped heap secrets at slice 19, and grant-backed sources at slices 16, 18 and 27; noninterference and constant time are proved for the type system under the baseline observer model; each `declassify` is traced in the facts; the legacy `constant_time_tag` is rewritten with `ct_eq`; the LCB plan gains constant-time and wipe preservation, and CI checks the binary constant-time premise; Home: spec (the entropy capability and its trusted premise). Gate: the slice's own acceptance gates ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0592, R-0575 |

### D. Whole language

| task | objective | done when | depends on |
|---|---|---|---|
| R-0137 | Workload validation: each workload records its attempts, diagnostics and measurements | the dashboard covers every supported workload; workloads grow from simple proved examples to file-copy and then supported real programs, each recording program correctness, authority, failure behaviour and residual assumptions; every port ships paired rejected-violation examples showing what Concrete refuses | C |
| R-0321 | The unsafe and trusted profile, with its VM pressure workload | the workload runs in the normal and checked profiles | C |
| R-0623 | A performance target against C, set at the first real showcase ([decision](docs/decisions.md#decided-2026-11-08-speed-target)) | speed and size stay report-only (R-0550) until grep at slice 16; then a target against C is set from measured data, by the owner; Home: tooling (the measurements). Gate: until the owner sets it, none beyond R-0550's report | R-0550, owner: the target |
| R-0621 | A no-libc runtime on the proven path ([decision](docs/decisions.md#decided-2026-11-08-no-libc)) | a later slice, around slices 24–25, replaces libc on the proven path with a runtime of its own; until then libc is a named premise in the trusted-base statement (R-0509); R-0584's direct-syscall launcher removes libc start-up, and this task removes the rest of libc from the proven path; the runtime is written around slices 24–25, and the no-libc claim closes only once R-0584 has landed too; Home: tooling (the runtime) and spec (its contract). Gate: the trusted-base golden no longer lists libc for the proven path | R-0509, R-0584 |
| R-0285 | RocksDB slice 1 (paused) | open, read, write and close with linear handles and typed errors; accepted only after slice 25 (FFI) and the audit of its program (R-0604) | C, R-0604 |
| R-0399 | Concurrency, the final language slice (slice 28; [decision](docs/decisions.md#decided-2026-11-07-final-slices)) | its model is decided by the owner first (the reviewers suggested structured, scoped spawn-join with linear ownership transfer and no shared mutable state, which is not a decision); then the slice meets the slice shape; Home: spec. Gate: slice 28's acceptance ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0604, owner: the concurrency model |
| R-0490 | The ethlambda port (paused) | runs with its interop checkpoint; accepted only after slice 28 (concurrency) and the audit of its program (R-0604), as revised by the [2026-11-07 decision](docs/decisions.md#decided-2026-11-07-final-slices); Home: tooling (the acceptance program). Gate: its interop checkpoint and the R-0604 audit record ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0399, R-0285, R-0604 |
| R-0515 | The usability track (paused): non-author pilots and legibility measures | a non-author pilot builds a project | C |
| R-0597 | A short ideas sweep of `legacy/` for phase D's area, at the latest at D's start (owner, 2026-11-06) | as R-0591, for D's area, at the latest before D's first slice lands (it may run earlier) | R-0591 |
| R-0595 | Heap bounds with the allocation slice (slice 19) ([decision](docs/decisions.md#decided-project-memory-2026-11-06)) | when allocation lands, each function reports requested, net and peak logical bytes and footprint per allocator lineage as polynomial bounds with `max`, or "not established"; std containers state per-operation growth bounds; budgets in `Concrete.toml` policies are enforced; "allocation cannot fail" is established only from an allocator success theorem; acceptance examples: a counted push loop into a reserved `Vec` with an exact bound, an arena parser proved infallible, an unbounded `while` giving "not established", branching recursion with only a depth bound giving "not established" for requested bytes, a reset loop accounted by lineage | R-0594, R-0589 |
| R-0624 | Whether destroying a collection needs `with(Alloc)` once its release metadata travels with it, decided with the allocation slice (slice 19) ([decision](docs/decisions.md#decided-2026-11-08-capabilities-kept)) | before slice 19's destroy rule is written, the owner decides whether destroying a collection whose release metadata travels with it still requires `with(Alloc)`, with options from the reviewers (raised by agy); until then the decided [authority rules](docs/decisions.md#decided-2026-11-02-authority) apply unchanged; Home: spec (the authority rules, C5). Gate: the owner's answer recorded in `docs/decisions.md` before slice 19's destroy rule lands | R-0611, owner: the answer |
| R-0601 | Function-type contracts, including trait methods (owner, 2026-11-06) | a binding design lets a function whose `#[requires]` is a safety precondition become a function value only under a matching function-type contract; until then such functions are refused as function values, including through wrappers and specialization; it never broadens to non-safety contracts; a prerequisite for accepting slice 20, or earlier if a workload pulls it | R-0571 |
| R-0604 | Audit the RocksDB and ethlambda acceptance programs before accepting them (owner, 2026-11-06; revised 2026-11-07) | each program's needs are listed against the decided language; concurrency is now final slice 28, with its model still to be decided, and ethlambda follows it ([decision](docs/decisions.md#decided-2026-11-07-final-slices)); foreign function values (OD4) stay refused, and any real need for them goes to the owner before acceptance; Home: tooling (the audit record). Gate: each program's needs listed against the decided language before acceptance ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | — |

### LD. Delete `legacy/` once a full real program runs

| task | objective | done when | depends on |
|---|---|---|---|
| R-0590 | Delete `legacy/` (LD) once the new compiler implements a full real program ([decision](docs/decisions.md#decided-project-restart), revised 2026-11-06) | the new compiler implements a full real program such as RocksDB, ethrex or similar (in the slice plan the earliest is the RocksDB acceptance program after slice 25); the sweeps for the areas of C, D and E (R-0591, R-0597, R-0598) have all run, each possibly earlier than its phase, and a final ideas sweep of `legacy/` dispositions every idea (carried, planned, deferred or dropped on purpose); every `RETENTION.tsv` row is resolved; the owner explicitly approves at the time; `legacy/`, the old pipeline included, is then deleted in one change, and the retention records at the root survive it | R-0591, R-0597, R-0598, R-0562, R-0561, owner: approval at the time |

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
| R-0505 | The native AArch64 profile: linking and final-image validation | the image theorem holds; the final image's bytes, relocations, entry state and startup are validated against LCB's ISA model and the recorded adapter output, so assembler, linker and loader behaviour leaves the trusted base except where named in the profile | R-0517 |
| R-0584 | A minimal direct-syscall launcher written in Concrete | the launcher that mints `main`'s grants is a small Concrete program compiled through the verified path and making system calls directly, so no C runtime or libc start-up stays in the trusted base; OS-level sandboxing (seccomp/landlock, Capsicum, pledge/unveil) is outside the compiler's scope; users may apply it themselves. | R-0505 |
| R-0424 | Performance evidence: flagship runtime tracking from slice 2, then corpus, codegen quality, fair comparisons and optimization ownership ([early tracking decision](docs/decisions.md#decided-2026-11-07-performance)) | flagship runtime is recorded by exact SHA, backend and target from slice 2 and is report-only until its noise is understood; the mixed corpus runs on LLVM and LCB-native; Home: tooling. Gate: flagship speed, report-only ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0517 |
| R-0598 | A short ideas sweep of `legacy/` for phase E's area, at the latest at E's start (owner, 2026-11-06) | as R-0591, for E's area, at the latest before E's first item lands (it may run earlier) | R-0591 |
| R-0605 | Lexicographic measures for `#[variant]` | added when a workload needs one, with its proofs | workload |

### F. Evidence as a product

| task | objective | done when | depends on |
|---|---|---|---|
| R-0494 | One verdict authority for every rendered claim | every surface derives its verdict from one judgment | E, R-0575 |
| R-0512 | Claims on compiler-generated Core statements; no authority from legacy evidence | identity, binding and provenance are rebuilt on new Core | R-0494 |
| R-0178 | Stable machine-readable fact schemas and compatibility policy for the frozen, versioned diagnostics and facts JSON schema introduced from the first front-end output ([early decision](docs/decisions.md#decided-2026-11-07-early-evidence)) | the schemas remain versioned and frozen, and the compatibility policy holds; Home: spec (the schema source) and tooling (the compatibility check). Gate: the generated JSON schema, with goldens ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0494 |
| R-0219 | Proof status: vacuity, spec adequacy, evidence mutation, toolchain migration | the validation project passes | R-0178 |
| R-0201 | Audit commands over the slice-0 facts query (R-0575): `audit`, `explain`, `why`, `diff` and trust-diff gates | the package-scale audit bundle passes; the per-function query itself is R-0575 (B0/B1), not this task | R-0178 |
| R-0353 | The release-evidence graph and an offline independent bundle verifier | a hostile bundle is rejected; receipt v1 frozen; a receipt distinguishes "was valid when produced" from current acceptance and revocation (the obligations of legacy R-0430), over the per-compilation certificates of R-0552 | R-0201 |
| R-0606 | Named assurance properties per module (owner, 2026-11-06) | each property is separate, named and versioned, with its scope, evidence and coverage; there is no ladder of levels | R-0575 |

### G. Release and ecosystem

| task | objective | done when | depends on |
|---|---|---|---|
| R-0119 | Exactly three public flagships with their evidence | graduated and maintained | F |
| R-0383 | Packages: manifest, resolution, interfaces, provenance, trust policy | the multi-package workspace passes | F |
| R-0149 | Incremental builds equivalent to clean builds | the edit, corruption and substitution matrix passes | R-0383 |
| R-0398 | LSP and editor service later, over the shared session ([timing decision](docs/decisions.md#decided-2026-11-07-tools)) | the editor validation passes; Home: tooling. Gate: the editor validation ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0149 |

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
| R-0617 | Smaller landing scripts ([decision](docs/decisions.md#decided-2026-11-08-landing-shrink)) | `scripts/land/` keeps the train, its review leg, the gates and the `legacy/` protection; the landing bot, notify, install-bot, the systemd units and duplicate helpers are removed and their old copies moved to `legacy/` as byte-identical additions, the one kind of addition `legacy_frozen` accepts once legacy sweep question 1 ("ok to 1", [sweep row](docs/decisions.md#decided-2026-11-08-legacy-sweep)) lands; Home: tooling. Gate: the root checks (`landing_script`, `legacy_frozen`, the root inventory) pass without them | — |
| R-0618 | Review rounds capped by risk ([decision](docs/decisions.md#decided-2026-11-08-review-caps)) | `scripts/review/review.sh` derives the round cap from what the diff touches: docs and roadmap 2, scripts 3, compiler and spec 5; a diff touching several classes takes the highest cap, classified by the files it changes, and at the cap the owner may sign off one exact SHA that Codex, Kimi and agy have all reviewed, with the approval and every outstanding finding recorded and never replacing a gate or proof obligation ([decision](docs/decisions.md#decided-2026-11-08-process), items 1 and 2); AGENTS.md and REVIEW.md state it; Home: tooling. Gate: `review_tool.sh` with a control per class | R-0544 |
| R-0619 | One clones folder ([decision](docs/decisions.md#decided-2026-11-08-clones)) | every branch's full clone is `~/projects/clones/<branch>`, made by one script and removed after the branch lands; AGENTS.md states it; Home: tooling. Gate: the script's negative controls (a reused branch name, a clone outside the folder) | — |
| R-0628 | Server-side protection of `main` ([decision](docs/decisions.md#decided-2026-11-08-landing-redesign)) | after the landing-script shrink (R-0617) and one repository (`wip/one-repository`) land, GitHub rulesets or branch protection let only a train merge with the required checks green update `main`; this is the real boundary; Home: repository settings, recorded as tooling. Gate: a refused direct push and a refused merge with a required check missing, recorded as controls ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0617 |
| R-0629 | A much smaller local guard, as a convenience ([decision](docs/decisions.md#decided-2026-11-08-landing-redesign)) | after R-0628, a small local guard warns before a push that the server would refuse; it is not the boundary; `wip/remote-guard` and `wip/acceptance-shim` are superseded; Home: tooling. Gate: its controls in the landing-script checks ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | R-0628 |
| R-0631 | Move all of `docs/archive/` under `legacy/` ([decision](docs/decisions.md#decided-2026-11-08-archive-move)) | every file under `docs/archive/` is added under `legacy/` by the byte-for-byte sweep ([sweep row](docs/decisions.md#decided-2026-11-08-legacy-sweep)) and removed from the root; P21, `RETENTION.tsv` and the links that cite these files are updated; a path that already exists under `legacy/` with other content is reported, never overwritten; Home: tooling. Gate: `legacy_frozen.sh` with its sweep controls, and the link check ([tracking](docs/decisions.md#decided-2026-11-07-tracking)) | the byte-for-byte sweep check |
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
