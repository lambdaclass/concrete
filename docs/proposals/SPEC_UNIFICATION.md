# A2i: unifying the Concrete Lean spec into one Core (plan, draft 3)

Status: proposal, accepted by Codex and Kimi in review round 3 (agy accepted round 2; its round-3 rerun on this text is pending); the spec worker carries it out as stage A2i (R-0571). The Lean spec stays the only home of the rules: this file is the work plan and is retired when A2i is done. Draft 3 folds in
the Codex, agy and Kimi delta reviews of draft 2 (section 7), the agreed slice order
(the [vertical slices](../../ROADMAP.md#vertical-slices), all four reviewers, 2026-11-06) and the owner decisions of 2026-11-06 relayed by
the coordinator (section 4), and the coordinator's answers to draft 3's questions S1–S3 (section
5.1). Drafts 1 and 2, the archive copies of the inputs and the raw review outputs are not kept in
the repository; the exact revisions below are the inputs. I checked each kept reviewer claim against the files.

**Inputs read (exact revisions):**

- `origin/wip/spec-root` 6f80b9c9d0: `ConcreteSpec.lean`, `ConcreteSpec/Spec.lean`, `ConcreteSpec/Spec/*.lean`, `ConcreteSpec/Proofs/*.lean`, `ConcreteSpec/README.md`.
- `origin/wip/spec-nht` a5e982a6c4: A2i no-hidden-traps M1 (`atomBounds`, `admitted`, `checked_no_implicit_traps`, `add100` refused, `add0` accepted).
- `origin/main` ab537833d7 and `origin/wip/decisions-followup` 4e75883d91: `docs/decisions.md` and `ROADMAP.md`. The three decided proposals are in [`docs/archive/crucial-2026-11-06/`](../archive/crucial-2026-11-06/).
- `origin/wip/slice0` 53f9a21798: `Concrete/README.md`, the compiler's view of the spec. It records the owner's slice order of 2026-11-06 at lines 612-622.
- The pipeline review [`MERGED.md`](MERGED.md) (item 11).

**Path conventions.** A file:line reference is to `origin/wip/spec-root` 6f80b9c9d0
unless it is marked `nht:`. The declaration name next to each line number is the anchor: the
spec-root head that lands (3968ada60c) moved lines in `Borrow`, `Cleanup` and the scripts, so the
implementation finds each item by name, never by line.
`Spec/X.lean` means `ConcreteSpec/Spec/X.lean`, and `Proofs/X.lean` means `ConcreteSpec/Proofs/X.lean`.
`dec:N` is line N of `decisions.md` and `RM:N` is line N of `ROADMAP.md`, both at
`decisions-followup` 4e75883d91.

---

## 0. Summary

The spec header calls its parts "small Core fragment[s]" (`Spec.lean:41-42`). Each fragment
re-invents the same things:

- 8 keyed-lookup functions plus 4 update and 2 remove functions;
- 6 statement representations;
- 7 result or outcome types;
- 5 evaluators with 3 fuel conventions;
- 6 "Ok" judgments;
- 2 operation tables;
- 2 loan models;
- 2 static-fact models;
- 2 encodings of allocator dependency.

There are also four instrumentation domains with one erasure proof between them.

Nothing joins these fragments:

- `Cleanup`, `Borrow`, `Brand` and `Authority` import nothing.
- `Semantics` imports the static `Ownership` (`Spec/Semantics.lean:1`).
- Three normative modules import the examples module: `Rep`, `Observation` and `Slice0`.

**The plan.** It replaces the fragments with one Core:

- one syntax, with a `SiteId` on every operation;
- typed identities;
- one `Ty` and one `Val`;
- one primitive-contract table (Q9.7);
- one clock-indexed semantics over explicit instrumented state (Q9.9);
- one outcome and event model;
- one evidence domain;
- one context with derived locks;
- one admission judgment with named premises;
- one checker returning `CheckResult`;
- one grammar as data;
- one facts vocabulary;
- one semantic contract.

The four design theorems keep their names and their full conclusions.

**Build order.** Foundations first, with no change of meaning (phase A). Then the Core grows slice
by slice in the agreed order of the [vertical slices](../../ROADMAP.md#vertical-slices) (
[row](../decisions.md#decided-project-vertical-slices-2026-11-06)): slice 0, slice 1 (the Q8.2 linear
empty struct, with explicit `abort()` and the refused `200 + 56` control), slice 2 (all integers),
slice 3 (bool, `if`, `let mut`, the octagon checker), slice 4 (loops and termination), slice 5
(functions), slice 6 (authority), and onward. An old fragment is ported, whole, at the slice that
introduces its feature, and deleted in that commit; the one exception is `Borrow`, ported in two
steps (its loans at slice 13, its views at slice 15). The library builds and passes the proof checks
after every commit.

**A2i integrates the whole decided Core before the A2c freeze** (the decided A2i/A2c rule). The
slice order governs the order of the spec work inside A2i and the compiler's implementation order;
it does not let any fragment stay unintegrated. Proposed (the coordinator's S1 answer, not an owner decision, and wider
than the A2i acceptance in ROADMAP.md, so it needs the owner's approval): every slice's spec side, 0 to
27, and every old fragment are in the one Core before A2c; slice 28's concurrency model is not yet
decided. None remains "not yet integrated" at the freeze.

The owner questions of draft 2 (layout, rule-ID spelling, spec version) and the slice plan's owner
questions are decided (sections 4 and 5.1).

---

## 1. Inventory: concepts defined more than once

### 1.1 Keyed lookup, update and removal

| Copy | Where |
|---|---|
| `lookupF` (generic) | `Spec/Syntax.lean:23` |
| `envFind` | `Spec/Semantics.lean:62` |
| `ctxFind` (by `Entry.name`) | `Spec/Ownership.lean:35` |
| `Borrow.find` (`Nat` keys, `Place` values) | `Spec/Borrow.lean:42` |
| `Borrow.getN` (`Nat` keys, any value) — the same pattern, polymorphic, in the same file | `Spec/Borrow.lean:404` |
| `Brand.findL` | `Spec/Brand.lean:32` |
| `findDecl`, `Prog.find` (via `List.find?`) | `Spec/Syntax.lean:87`, `Spec/Authority.lean:151` |
| updates: `envMove`, `ctxMark`, `putN`, `mapPlaces` | `Spec/Semantics.lean:67`, `Spec/Ownership.lean:40`, `Spec/Borrow.lean:409,46` |
| removals: `Borrow.remove`, `Brand.removeL` | `Spec/Borrow.lean:48`, `Spec/Brand.lean:36` |
| lemmas proved per copy | `Proofs/Basics.lean:11,20` (`ctxFind_mem`, `lookupF_map`); `Proofs/Borrow.lean:11,21,40,450` (`find_mem` and the `getN`/`putN` lemmas); `Proofs/Brand.lean:11,20,31,372,378`; also `tys_lookup` and `ctxMark_tys` (`Proofs/Slice1.lean`) |

**Unified:** one `AList` API (`lookup`, `update`, `erase`, `map`) in `Spec/Basics.lean`, with its lemmas
once in `Proofs/Basics.lean`. Every map is keyed by a typed identity (1.2).

### 1.2 Identities and sites

| Copy | Where |
|---|---|
| `Var` (hint + number): the only typed identity | `Spec/Syntax.lean:15` |
| struct and field names as `String` | `Spec/Syntax.lean:11,78-81` |
| places and roots as `Nat` | `Spec/Borrow.lean:33,40` |
| allocator variables, heap values and brands as `Nat` | `Spec/Brand.lean:44,56-74` |
| resources, loans and loop labels as `Nat` | `Spec/Cleanup.lean:31,34,60` |
| functions as `String`, with `"main"` looked up by string | `Spec/Authority.lean:140,151,206` |
| `"main"` refused as a struct name, by string | `Spec/Ownership.lean:201` |
| grants as `Nat` positions and launcher numbers | `Spec/Authority.lean:131,221,237` |
| operation sites: none in Core | README O1/O4; slice 0 mints `SiteId` beside Core (`slice0-README.md:80`) |

The facts decision requires typed identities: module, function and instance, with no bare-name
collisions (`dec:729`). Rule 13 requires that `#[proof]` resolve to a typed theorem identity
(`dec:640`).

**Unified:** `Spec/Ident.lean` with these identities:

- `Var`, `DeclId`, `FieldId`, `FnId`, `ModuleId`, `InstanceId`;
- `LoopId` (labels, Q9.3);
- `BrandId` and `LineageId` (the memory decision's allocator lineage);
- `LoanId`, `GrantId`, `BlockId` (Q3.7);
- `CapVarId` (capability parameters, Q5.1–Q5.4; used from slice 11);
- `TheoremId`;
- `SiteId`, one per operation and per refusable statement.

Every operation node carries a `SiteId`; elaboration (O1) maps it to a span. Diagnostics, facts,
traps, leakage records and discharge tokens all key on it.

### 1.3 Types and linearity

| Copy | Where | Notes |
|---|---|---|
| `IntTy`, `STy`, `Ty` | `Spec/Syntax.lean:31,64,70` | struct fields are scalars only |
| `Brand.STy` (`alloc β`, `heap β`) | `Spec/Brand.lean:44` | the same name as `ConcreteSpec.STy`, with a different meaning |
| `Authority.GrantTy` | `Spec/Authority.lean:96` | |
| `Borrow.Place` flags `mutable`, `owner` | `Spec/Borrow.lean:32-36` | stand in for a type |
| `Borrow.Source` | `Spec/Borrow.lean:133-136` | run-time data (array length, text bytes), which `RangeFacts` then also uses as a static fact (`:177`) |
| linearity | `Ty.isCopy` `Spec/Syntax.lean:91`; `Val.isCopy` `Spec/Semantics.lean:72` (the same declaration lookup again); prose `Spec/Brand.lean:42` | three encodings. `Place.owner` (`Spec/Borrow.lean:35`) is root-versus-view ownership metadata, not linearity; it becomes the entry's `root` field (1.12) |

**Unified:** one `Ty` in `Spec/Types.lean` with these constructors:

- `scalar`;
- `struct` and `enum`, the flat nominal data of Q9.2 and Q9.4, whose fields are any `Ty`;
- `array T n` and `text`;
- `alloc β` and `heap β T`, with brands as a separate erased kind (Q1.1);
- `view m T`, second-class (Q3.1–Q3.3);
- `grant g`;
- `Result` and `Option` as ordinary enum declarations;
- `secret T` on scalars (R-0596), from slice 7, within A2i.

`Ty` is complete for every type the old fragments already use (alloc, heap, views, grants, text)
from phase A, ahead of the slices that expose them in source: `Fragment.surface` gates the source,
not the Core.

There is one `Ty.isCopy`. One recursive `Ty.brands`, over struct and enum fields, gives the
allocator dependencies of Q1.5 and Q1.6. `Val.isCopy` goes away: the run-time environment keeps
each binding's declared type as instrumentation.

### 1.4 Values and stores

| Copy | Where |
|---|---|
| `Scalar`, `Val` (struct of scalars) | `Spec/Syntax.lean:98`, `Spec/Semantics.lean:15` |
| `Brand.Val`, `Brand.EVal` | `Spec/Brand.lean:103,196` |
| `Borrow.Mem` (untyped `List Nat` arrays), `VInfo`, `Views` | `Spec/Borrow.lean:414,420,427` |
| text as `List Nat` | `Spec/Borrow.lean:136` |
| `Rep.MByte` (not used by any evaluator) | `Spec/Rep.lean:28` |
| grant handles as `List Nat` | `Spec/Authority.lean:237` |

Value typing is restated per fragment: `SVTy` (`Spec/Semantics.lean:201`) and `RepVal`
(`Spec/Rep.lean`) say a value fits a type. `ViewsOk` (`Proofs/Borrow.lean:467`, views against
memory) and `Brand.Rel` (`Proofs/Brand.lean:198`, a context and environment against a brand
realization) are state invariants built on value typing.

**Unified:** one recursive `Val` in `Spec/Values.lean` and one `Val.HasTy`. `SVTy` and `RepVal`
become it; `ViewsOk` and `Rel` become invariants of the one semantic state, stated with `Val.HasTy`
(they are not replaced by value typing alone). Atom typing and evaluation stay subcases of
expression typing and evaluation, each defined once. One store of blocks with provenance
(Q3.7) in the semantic state. `Layout` relates values to bytes (Q3.11). Brands are ghost data on
handles (1.9).

### 1.5 Statement languages and programs

There are six statement representations:

| Statements | Where |
|---|---|
| slice-1 `Atom`, `Expr`, `Binder`, `Stmt`, `Program` | `Spec/Syntax.lean:112-147` |
| slice-0 `Atom`, `Expr`, `Stmt`, `Program`, with `toCore` and `InSlice0 := ∃ q, toCore q = p` | `Spec/Slice0.lean:34-75` |
| `Borrow.Stmt` | `Spec/Borrow.lean:69` |
| `Borrow.VStmt`, a second language in the same file | `Spec/Borrow.lean:433` |
| `Brand.Stmt` | `Spec/Brand.lean:56` |
| `Authority.Stmt`, `Fn`, `Prog` | `Spec/Authority.lean:129,139,148` |

`Cleanup` has no statements at all: it models `Deferred` records and `Exit` values
(`Spec/Cleanup.lean:48,80`).

**Unified:** one ANF Core syntax (Q9.1, Q9.3–Q9.5, Q9.8) in `Spec/Syntax.lean`. It has:

- atoms, and operation nodes through the primitive table, each carrying a `SiteId`;
- `let`, `let _` and destructuring;
- `match` on flat data;
- place writes;
- `borrow [mut] p [from a until b] as v { … } [else { … }]`, and the distinct heap-open case (Q9.5);
- `defer` calls, whose by-value arguments are read when the call runs (`dec:1250`);
- nested scopes;
- `while L { C } do { B }` (Q9.3), with the attributes as data: `#[invariant]`, `#[variant]`,
  `#[may_not_terminate("…")]`;
- `if`;
- `break L`, `continue L` and `return`;
- calls with explicit capability arguments (Q5.1);
- brand binders, explicit `pack` and lexical `unpack` (Q1.2, Q1.3);
- `abort`.

`Fn` carries its parameters, grant parameters, `with(...)`, `trusted`, `#[variant]` and
`#[proof(TheoremId)]`. A `Program` is its declarations plus its functions. `main` declares its grants
as parameters (STMT-7, `dec:1227`). `Spec/Syntax.lean:140` ("may declare no grants") should be
reworded: it allows a grant-free slice, but it reads like a prohibition.

**The slice-0 syntax.** It stops being a second Core syntax. A slice is two decidable predicates:

- `Fragment.surface`: the grammar productions the slice supports;
- `Fragment.core`: the Core constructors the slice supports.

Both live in `Spec/Fragments.lean`. The decided "not supported in this slice" refusal is a
surface-level classification: a decided form whose production lies outside `Fragment.surface`
(`dec:829-835`). An invalid program is refused by the grammar or by admission. The Core predicate
scopes the slice theorems.

The `wip/slice0` design kept `toCore`, retargeted at the integrated Core
(`slice0-README.md:615`). Decided internally (I3, 2026-11-06): one Core syntax; slice 0 uses the
fragment predicates, and `Spec/Slice0.lean`'s syntax and `toCore` are deleted.

### 1.6 Outcomes, results, traps and failure

| Copy | Where | Notes |
|---|---|---|
| `R` (ok, trap, stuck) | `Spec/Semantics.lean:46` | |
| `Outcome` (exit, trap, stuck, outOfFuel) | `Spec/Semantics.lean:54` | |
| `Trap` (`arithmeticOverflow` only), `Stuck` | `Spec/Semantics.lean:32,38` | no site |
| `Brand.GR`, `Brand.ER` | `Spec/Brand.lean:147,210` | |
| `Borrow.VR` | `Spec/Borrow.lean:447` | `trap` has no kind |
| `Borrow.RangeStep.trap` | `Spec/Borrow.lean:158` (the constructor at `:164`) | |
| `Cleanup.Result`, plus the oracle `ok : Deferred → Bool` | `Spec/Cleanup.lean:111,119` | trap kind lost |
| `Authority.Trace.complete : Bool` | `Spec/Authority.lean:228` | conflates out-of-fuel with stuck |
| `checkedAdd` | `Spec/Semantics.lean:115` | |
| `Observation.trapped k` | `Spec/Observation.lean:43` | no site (O4) |

**Unified** in `Spec/Semantics.lean`, following Q6.7, `dec:1224` and the no-hidden-traps decision:

```
inductive TrapKind | arith (e : ArithmeticError) | index | range | byteAt | abort | …
def TrapKind.implicit : TrapKind → Bool          -- every kind except `abort`
inductive Outcome
  | exit (code : Nat) (h : code < 256)   -- or Fin 256; keep what wip/slice0 consumes
  | trap (k : TrapKind) (s : SiteId)     -- status 134 for every kind (Q7.10); abort is a trap (dec:1224)
  | exhausted (r : TargetResource)       -- target-only (stack, Q14): a named premise, never an allocation failure
  | stuck (w : Stuck)                    -- unsafe stuckness, ruled out by admission
  | outOfFuel                            -- the clock ran out; inconclusive, not divergence
```

`ArithmeticError { Overflow, DivisionByZero, InvalidShift }` is defined once, in `Spec/Types.lean`.
It is the error payload of the `checked_*` operations and the payload of the `arith` trap kind.
Allocation failure is a value, `AllocFailure<T>` (Q1.8, Q1.9), not an outcome.

One result type replaces `R`, `GR`, `ER`, `VR` and `Cleanup.Result`. Primitives return a
primitive-local result (1.11), which the semantics maps to sited outcomes.

### 1.7 Events, traces and observations

| Copy | Where |
|---|---|
| `Cleanup.Event` (a call, or the release of a loan) | `Spec/Cleanup.lean:105` |
| `Authority.Event` (an operation on a grant), `Trace` | `Spec/Authority.lean:219,228` |
| `vrun`'s ad-hoc output list `out` | `Spec/Borrow.lean:457` |
| `Observation` | `Spec/Observation.lean:37` |

**Unified:** two channels, kept apart.

- **Public events.** One `Event` type in Core with an explicit observable-event kind (decided
  2026-11-06): `start`, `exit`, and the operations on grants (with Q7.9's request/response split when
  blocking arrives). Only these, with the outcome, feed `Observation`. The Start/Exit trace of
  slice 6 is these kinds.
- **The ghost log** (1.9). It records cleanup calls, loan ends, alloc and destroy (each with lineage,
  size and call frame), release sites and leakage records. It is instrumentation, never observed.

The event kind is extensible (SPARK row). A secret wipe is a **public** event kind (R-0602), added
at slice 7: erasure keeps it and every later preservation theorem must preserve it, which is what
protects a wipe from being optimized away. It is not in the ghost log.

Making alloc, free or cleanup a public observation would change what programs observably do, so
the plan does not do it.

### 1.8 Evaluators and fuel

| Evaluator | Where | Fuel |
|---|---|---|
| `runBody`, `run`, and the relational `Exec` | `Spec/Semantics.lean:176-188` | one unit per statement |
| `Authority.exec`, `runMain` | `Spec/Authority.lean:237,257` | recursion depth |
| `Brand.ghost`, `Brand.erased`, and the relational `Run` | `Spec/Brand.lean:126,163,217` | depth; loop tests come from an oracle `σ` |
| `Borrow.vrun` | `Spec/Borrow.lean:457` | depth |
| `Cleanup.RunCleanup` | `Spec/Cleanup.lean:119` | relational; call results come from an oracle `ok` |

**Unified:** one total clock-indexed evaluator (Q9.9) with one fuel convention, and one relational
`Eval`. Today `Proofs/Slice1.lean` proves `exec_deterministic`, but its adequacy is specific to
straight-line code: `exec_complete` uses the fuel `ss.length + 1`, and `exec_total` holds because
slice 1 has no loops or calls. Neither generalizes. They are replaced by:

- `run_sound`: `run n p = o` with `o ≠ outOfFuel` gives `Eval p o`;
- `run_complete`: `Eval p o` gives `∃ n, run n p = o` (existential fuel, no bound);
- `run_mono`: more fuel never changes a non-`outOfFuel` outcome;
- `eval_deterministic`;
- totality (`Eval p o` for some `o`) only under a termination fact (3.1.10), never unconditionally.

Each old evaluator is migrated with a correspondence lemma, proved in the commit that moves it and
deleted with the old evaluator. For an oracle-free evaluator it relates the old run to the new run
of the embedded program. For an oracle-driven one (`Brand`, `Cleanup`) it goes only in the
realizable direction: every integrated run determines an oracle under which the old run agrees with
it (section 6, phase C). The oracles `σ` and
`ok` disappear: conditions and deferred calls actually run. Every fuel-relating lemma is stated once,
at the evaluator.

### 1.9 Instrumentation and erasure

There are four instrumentation domains, with one erasure proved:

| Domain | Where | State |
|---|---|---|
| ownership ghost (`Slot.moved`; `Entry.used`, `live`, `known`, `kfields`) | `Spec/Semantics.lean:23`, `Spec/Ownership.lean:17` | erasure stated as intent only (O3) |
| brands | `Spec/Brand.lean:196-244` | proved (`brand_erasure`) |
| per-lineage byte accounting | none | due in A2i (R-0592) |
| leakage | none | due in A2i (R-0592) |

**Unified:** the semantic state is `State × Ghost`. `Ghost` holds the slot states, brands and
lineages, and the ghost log (1.7). The byte quantities are not stored counters. They are functions
of the log:

- requested bytes and allocation count per lineage;
- live bytes;
- peak above a call's entry baseline;
- signed net change per call frame.

A single source then answers the memory decision's quantities (`../archive/crucial-2026-11-06/MEMORY.md:19-31`).
Leakage records are branch decision, address, allocation size, loop count and variable-time
operand, each with its `SiteId`.

The mode checks of variable reads (`wrongMode`, `movedRead`) inspect `Ghost` only; in the erased run
they cannot fire, which is what `checked_never_stuck` plus erasure guarantees.

There is one `erase` and one theorem, `instrumentation_erasure`. It replaces `brand_erasure` and
discharges O3 and the "removing it changes no run" obligations of the memory and information-flow
decisions.

### 1.10 Judgments and the checker

| Copy | Where |
|---|---|
| `HasType`, `UseAtom(s)`, `Uses`, `StmtOk`, `BodyOk`, `DeclsOk`, `Checked` | `Spec/Statics.lean:39`, `Spec/Ownership.lean:49-205` |
| `Borrow.Ok`, `RangeOk` | `Spec/Borrow.lean:83,217` |
| `Brand.Ok` | `Spec/Brand.lean:76` |
| `Cleanup.Reg` | `Spec/Cleanup.lean:147` |
| `Authority.PrimOk`, `CallOk`, `StmtOk`, `FnOk`, `ProgOk` (reusing the name `StmtOk`) | `Spec/Authority.lean:156-204` |
| `check : Program → Bool` | `Spec/Checker.lean:130` |

Authority has `Decidable` instances and no checker. Borrow, Brand and Cleanup have neither; their
examples are proved by `cases`.

**Unified:** one judgment family in `Spec/Admission.lean` (`StmtOk Σ Γ s Γ'`, `BlockOk`, `FnOk`,
`Admitted`). Source-level `Checked` (lexing, parsing, the fragment predicate, then `Admitted`) is
defined in `Checker`. Every premise is named, because the rule ID is the Lean rule name plus that
rule's own revision (1.10). Feature modules define auxiliary predicates; the admission judgment
is the one authority.

`Spec/Checker.lean` holds the checker, following `../archive/crucial-2026-11-06/DIAGNOSTICS.md:13-27`:

```
-- in Ident (phase A step 2), with no dependency on any rule's module:
structure RuleId where                -- decided: the Lean rule name plus that rule's own revision
  name : Lean.Name                    -- the declaration (constructor or premise) that states the rule
  rev  : Nat
-- in Checker (step 9), downstream of every rule's module:
inductive Rule                        -- every rule a diagnostic can cite, typed
  | lexical (r : LexRule)             -- e.g. the leading-zero rule
  | grammar (r : GrammarRule)
  | fragment (prod : Production) (s : SliceId)   -- "not supported in this slice"
  | admission (p : Premise)           -- a constructor and named premise of Admission
def Rule.id : Rule → RuleId           -- a gate checks each name is the rule's own declaration
inductive Subject                     -- what a refusal is about
  | site (s : SiteId)                 -- an operation or refusable statement
  | span (sp : Span)                  -- source text with no site yet (lexical, grammar)
inductive CheckResult
  | accepted
  | refused (rs : List Refusal) (ne : rs ≠ [])   -- deterministic order
  | incomplete (reason : Incomplete)             -- fixed analysis limit or tool failure; neither acceptance nor refusal
structure Refusal where
  rule    : RuleId
  kind    : false | notEstablished | unsupported
  subject : Subject
  blockedBy : List RuleId
```

The rule and version lifecycle (runtime row; R-0593, R-0603):

- every output (diagnostics, facts, certificates, JSON lines included) carries one `spec_version`, in
  one header covering every record: 0.x before the freeze, 1.0 at A2c, bumped by every normative
  change;
- a rule's revision is bumped when its meaning changes, including a change that reaches it through a
  definition it depends on;
- a retired rule ID is kept in a registry in `Checker` and never reused;
- CI checks all three: a normative diff without a version bump, a changed rule without a revision
  bump, and a reused retired ID each fail (step 9, with negative controls).

Its theorems are `check_correspond` (`accepted ↔ Checked p`, where `Checked` includes lexing,
parsing and the fragment predicate) and `check_refused` (`refused rs → ¬ Checked p`, and each refusal
names a rule that is false, not established or unsupported for its subject). A gate checks that
rules and refusals correspond one to one.

A single fact that is "not established" is a refusal of kind `notEstablished`. `incomplete` is
reserved for whole-checker limits.

### 1.11 Operation tables

| Copy | Where |
|---|---|
| `checkedAdd` | `Spec/Semantics.lean:115` |
| `Authority.Op`, `Op.grant`, `Op.caps` (with `alloc`) | `Spec/Authority.lean:101-127` |
| allocator statements, again without capabilities | `Spec/Brand.lean:56` |
| element access `get`/`set`, and range-index arithmetic twice (`viewIndex`, `elemIdx`) | `Spec/Borrow.lean:172,431-443` |
| `validRange`, `openRange` | `Spec/Borrow.lean:153,168` |

**Unified:** Q9.7's one exhaustive, typed primitive-contract inventory in `Spec/Primitives.lean`. It
imports `Types` and `Values` only, so `Syntax` can name a `PrimOp` (the module order is
`Types → Primitives → Syntax`, 2.2). It holds two things.

**Per primitive:**

- its `PrimOp` identifier and signature;
- grant type and capabilities (rule 7);
- whether it is **pure** or **stateful**. A pure primitive (integer and bitwise operations,
  conversions, `ct_select`) has an evaluation into a primitive-local result `ok v | fail f`. A
  stateful primitive (`alloc`, `reset`, `release`, view `get`/`set`, `byte_at` on a store, grant
  I/O) has only its contract here: the transition on the semantic state is a rule in `Semantics`,
  which reads the same failure condition;
- the failure condition, as a predicate on the arguments. Overflow, zero divisor, invalid shift,
  index out of range and `byte_at` offset out of range are the cases. `byte_at` needs only an
  in-range offset (Q4.6); UTF-8 boundaries belong to range subviews (Q4.7);
- whether its time depends on its operands;
- its allocation size.

**The range test.** One `validRange` and one index map serve both views and element access.

Semantics, admission, authority, the facts and the leakage records all read this one table. An
operation's evidence premise is "the facts entail the negation of its failure condition". The trap
and the refusal are two readings of one definition.

### 1.12 Contexts, ownership, loans and dependencies

| Copy | Where | What "owned" means |
|---|---|---|
| `TyCtx` | `Spec/Statics.lean:15` | |
| `Entry`, `Ctx` | `Spec/Ownership.lean:17,29` | `live`, `used`, known facts |
| `Borrow.Ctx` of `Place` | `Spec/Borrow.lean:32,40` | `owner`; a moved place is removed |
| `Brand.Ctx` and the brand set `B` | `Spec/Brand.lean:49,76` | removed on free and release |
| `live : Res → Bool` and `Stack` | `Spec/Cleanup.lean:147` | a predicate |
| loans | `Cleanup.Loan` (`Spec/Cleanup.lean:34`): an identity, no mode; `Borrow.Lock`, `lockFor` (`Spec/Borrow.lean:24,54`): a mode, no identity | two loan models |
| allocator dependency | `Deferred.deps`, declared (`Spec/Cleanup.lean:48`); the brand of `heap β`, derived (`Spec/Brand.lean:52`) | two encodings |
| brand freshness | static: `β ∉ B` in `Brand.Ok` (`Spec/Brand.lean:73-99`); run-time: the counter invariant `Inv` (`Proofs/Brand.lean:88-91`) | two definitions |

**Unified:** one context. Each entry is `{ var, ty, state, used, root, mutable }`. Alongside the
entries the context holds:

- the active loans `{ LoanId, root, mode, range?, holder : block | deferred call }`;
- the cleanup frames with their registered calls;
- the brands in scope.

A place's lock is derived from the active loans. A dependency is derived from `Ty.brands`. Brand
freshness is one definition, `BrandId` not among the brands in scope; the run-time counter invariant
is a lemma about the generator, proved against that definition.

Two rules are decided and must hold together:

- A deferred call's loans end right after that call (R-0511, `RM:261`; OD2 (4)).
- A `reset` or `release` of brand `β` needs no live allocation and no conflicting loan from it
  (`dec:1214-1216`). Values consumed by a pending `defer` still count as live allocations of `β`
  (`dec:1250`, last sentence).

Static facts are not in the entry (1.13).

### 1.13 Static facts and evidence

| Copy | Where |
|---|---|
| `Entry.known`, `kfields`, `knownAtom`, `knownExpr`, `knownFields`, `FieldsHold`, `knownViolates` | `Spec/Ownership.lean:22-125` |
| `atomBounds`, `admitted` (interval admission) | `nht:Spec/Ownership.lean` |
| `RangeFacts`, `knownInvalid`, `Agrees` | `Spec/Borrow.lean:177,195,207` |
| `Known` (the slice-0 proof invariant) | `Proofs/Slice0.lean:47` |

**Unified:** `Spec/Evidence.lean`. It holds:

- the evidence graph: nodes are sites, edges say which facts a discharge used. Diagnostics read it
  for `blockedBy`, and facts read it for dependency flow;

- the octagon fact language over typed place versions;
- its transfers, including the field facts of the Q6.2 revision;
- `entails`, with the fixed limits as spec constants;
- evidence sources: proved, guard, precondition discharged at every call, named theorem.
  `assume` is excluded;
- the discharge token, keyed by `SiteId`, which the compiler carries down to BIR
  (`slice0-README.md:74`).

The token's validity (its satisfaction relation) is defined in `Spec/EvidenceSem.lean`, after
`Semantics`.

`knownViolates`, `knownInvalid` and `Agrees` are subsumed by admission on evidence. `admitted` moves
here.

### 1.14 Knowledge kept in prose that gets old

| Copy | Where | Stale today |
|---|---|---|
| module table and reading order | README table; `Spec.lean:27-29` | |
| proof table | README "Proofs/" table | `knownViolation_traps` and `add100_safe` are already gone on `nht` |
| seven-parts list and slice-1 description | `Spec.lean:41-123` and README | "Unifying them … is work for A3 (R-0560)" (`Spec.lean:42`, README): the work moved to A2i (R-0571) on 2026-11-06 |
| "Formalized at A2c" | README | that work was A2b |
| "revision pending" note | `Spec/Borrow.lean:216` (and longer on `nht`) | |

**Unified:** the owner view (`scripts/spec/lean_site.sh`, R-0555) already generates the module list,
each declaration's uses and its proofs. The README keeps only how to read and the open obligations,
and links the view. `Spec.lean` keeps a short header and the imports. A decision row is cited once,
on the definition it governs.

### 1.15 Grammar

There is no grammar data in `ConcreteSpec` today. R-0574 requires, within A2i, the
attribute-prefix production, kept LL(1) and checked mechanically (`RM:268`, `dec:667-668`). The
slice-0 design plans `Spec/Surface0.lean` with `grammar0` and `ll1` (`slice0-README.md:194-217`).

**Unified:** one `Spec/Grammar.lean` holding the grammar as data, `ll1` with `ll1_sound`, and the
attribute-prefix production. `grammar0` is `Fragment.surface` of slice 0: a restriction of the one
grammar, not a second grammar. The lexical specification (`Lexes`, the leading-zero refusal of the
slice-0 surface decision) lives in the same module or beside it in `Spec/Lexical.lean`.

---

## 2. The integrated Core

### 2.1 Shared interfaces

1. **Syntax** (1.5), with **grammar** (1.15) and **fragments**.
2. **Typing and admission** (1.10): one judgment with named premises, one checker returning
   `CheckResult`.
3. **One semantics** (1.8). Structured exits run the cleanup frames in OD2 (5)'s order: exit value,
   then deferred calls (each releasing its own loans), then the scope's own loans end, then the
   remaining locals are destroyed. Loans end with their block or deferred call.
4. **Observations** (1.7, in `Contract.lean`). `observe : Launch → Events × Outcome → Observation`
   gives status 125, the returned code, or 134. The trap line is decided
   ([row](../decisions.md#decided-project-runtime-2026-11-06)): `concrete: trap: <kind> at
   <path>:<line>:<col> (site <id>)`. It is a function of the kind and the
   site's span; the span comes from elaboration (O1). This closes README O4.
5. **The failure and trap model** (1.6).
6. **Resource and allocation accounting** (1.9): lineages, sized allocation, and log-derived
   quantities. Allocator footprint is an allocator-contract function, not Core. `Layout.size` is the
   one size authority. The default struct layout is decided
   ([row](../decisions.md#decided-project-runtime-2026-11-06)): declaration order, natural alignment
   defined exactly per target (a target parameter of `Layout`), size rounded up to the alignment,
   zero-sized fields take no bytes; not a C ABI promise. Slice 1's empty
   struct is the first zero-sized type.
7. **Site identity** (1.2), throughout.
8. **Facts** (`Facts.lean`). Statuses: established, conditional, refuted, not established, not
   applicable. Coverage is per fact. Each established or conditional fact also carries the strength
   of its claim (R-0585) and its vacuity-check status (OD120). Each fact records its dependency flow
   (which inputs and facts it rests on, `RM:300`), read from the evidence graph. A kind whose slice
   has not landed is `notApplicable`, so the vocabulary is complete from the start. The fact kinds
   are typed judgments:
   - capabilities used;
   - grants and resources reachable;
   - memory reads and writes;
   - heap requested, net and peak, and footprint, per lineage;
   - allocation infallibility;
   - may block;
   - foreign, trusted and opaque boundaries;
   - each implicit-trap site with how it is handled;
   - `Result` errors, allocation failure and abort, kept separate;
   - termination, recursion depth, and stack (artifact scope);
   - proof status;
   - termination permissions (`#[may_not_terminate("reason")]` sites), the derived `bounded`
     fact (R-0574), may-abort, and always-aborts with path pruning after an always-aborting call
     (R-0600, slice 5);
   - unused permissions: a permission that justifies nothing (SPARK row; a `Concrete.toml` policy
     may refuse it);
   - `reads_secret`, `declassifies`, `ct_source`, `ct_binary`, `wipes` (from slice 7).

   `Fact.Holds` gives each judgment's semantic meaning. The derivation comes with each slice, in the
   compiler, importing these definitions, with two theorems: soundness (a reported fact holds) and
   absence completeness (a fact reported absent, such as "no capability X", truly has no witness in
   any run). The
   compiler's `Facts.lean` (`slice0-README.md:190`) must not redefine the vocabulary.
9. **The semantic contract** (`Contract.lean`, R-0496), stated first and refined per feature. It
   names the predicates the theorems prove; the theorems use these names and do not restate them:
   - `Safe`;
   - `NoImplicitTraps`;
   - `AuthorityContained`;
   - `InstrumentationErasure`;
   - `TerminatesUnder` (the evidence);
   - the observation mapping (item 4);
   The statements that also need admission or facts (the R-0588 preservation statement for indirect
   calls, specialization and packages; admission re-checked after Mono, Q6.3) are not here: their
   one home is `Facts` (2.2).

### 2.2 Modules and dependencies

There is no folder below `Spec/`. Arrows are imports, and the graph is acyclic:

```
Basics                                     (AList)
Basics → Ident                             (typed identities, SiteId, SliceId, RuleId, spec_version)
Ident → Types                              (Ty, ArithmeticError, TrapKind, Cap, GrantTy)
Types → Outcome                            (the sited outcome)
Types → Values                             (Val, Val.HasTy, Layout)
Values → Primitives                        (PrimOp, signatures, contracts, pure evaluation)
Primitives → Syntax                        (one ANF Core; attributes as data)
Syntax → Lexical → Grammar                 (lexical rules; grammar data, ll1, attribute prefix)
Grammar → Fragments                        (Fragment.surface, Fragment.core per SliceId)
Syntax, Outcome → Contract                 (Event, Behaviour; Safe, NoImplicitTraps, …, observe)
Contract, Values, Primitives → Semantics   (State × Ghost, stateful transitions, run, Eval, erase)
Syntax → Evidence                          (octagon facts, entails, sources, tokens, evidence graph)
Evidence → Obligations                     (generated obligation schemas; #[proof] roles; TheoremId targets)
Semantics, Obligations → EvidenceSem       (satisfaction of evidence facts, tokens and obligations)
Obligations → Typing → Ownership           (static feature modules; each also imports Evidence)
Typing → Termination, Authority, Borrow, Memory, Cleanup
all static feature modules, Fragments → Admission
Admission → Checker
Admission, EvidenceSem, Contract → Facts    (Fact.Holds; post-Mono re-admission and R-0588 as statements)
everything → Examples                      (cross-module conformance examples only)
```

The import rules:

- `Semantics` and `Contract` import no static module.
- Static modules may import `Primitives`, but not `Semantics`.
- Only `EvidenceSem` and `Facts` join statics and semantics.
- `Contract` comes before `Semantics` (the roadmap's contract-then-evaluator order, `RM:97-105`): it
  defines `Event`, a program's `Behaviour` (the set of event traces with outcomes), `observe` and the
  run predicates (`Safe`, `NoImplicitTraps`, …) over behaviours. `Semantics` then defines a
  program's behaviour. Theorems relating `Checked` to the predicates live in `Proofs/`. The
  statements that need admission or facts (post-Mono re-admission, R-0588 preservation, `Fact.Holds`
  soundness) are in `Facts`, the one module that sees both.
- A `Program` carries its site table: each `SiteId` with its source `Span` (path, line, column). The
  trap line reads it. Elaboration, which must produce a correct table, stays README obligation O1:
  the compiler's elaborator owns it, with a theorem per slice.
- `#[proof]` admission (roles (a), (b) and (d), `dec:650-657`) reads obligation schemas from
  `Obligations`, which precedes `Admission`; their semantic meaning is in `EvidenceSem`.
- No normative module imports `Examples`.
- `Proofs/` mirrors the modules and adds `Proofs/Interactions.lean`.
- `ConcreteSpec/Spec.lean` remains the public import (R-0560).

These are 26 modules, replacing 13. agy recommended about 17; the plan keeps one module per concept
the owner reads, and the proposed front-end-first order (section 6, phase A) names `Ident`, `Outcome`
and `Lexical` as their own steps, so they stay separate modules. Folded: `Layout` into `Values`,
observation into `Contract`.

---

## 3. Proofs

### 3.1 Interaction and integration proofs for A2i

1. **Borrow across deferred cleanup** (`defer_holds_loan`). Covered cases:
   - A deferred call's borrowed arguments keep their root locked from registration until the call
     runs. Its by-value arguments are read when it runs.
   - On a **successful** unwind (fall-through, `break L`, `continue L` or `return`): the exit value
     is evaluated first; each call comes before its own loan ends; the scope's loans end after its
     deferred calls; locals are destroyed last.
   - A `defer` inside `borrow mut x as v { … }` may use `v`, and runs before `v`'s loan ends.
   - Same-scope dependency order holds.
   - Postconditions (`#[ensures]`) see the state after returning cleanup (Q9.11).

   On a trapping, aborting or permitted-nonterminating cleanup, the theorem states the terminal
   prefix only.
2. **Allocator reset with outstanding loans** (`reset_no_outstanding_loans`). An accepted `reset` or
   `release` of brand `β` happens only when no live value, no active loan or view, and no pending
   deferred call's target or loan has `β` in `Ty.brands`. In the run, no handle or view of the old
   brand is used afterwards. `reset_fresh`'s freshness conclusions are kept whole.
3. **Cleanup that needs authority** (`authority_contained`, restated). Every public event, including
   operations performed by cleanup on unwinding and by allocator primitives, acts on a grant whose
   provenance traces to one of `main`'s launch grants: the grant itself, or one acquired or
   narrowed from it (Q7.2). The event's capabilities are declared by `main`. A deferred call's
   capabilities are checked where it is registered (rule 7); a user `destroy` needs at most `Alloc`
   (Q2.7). This discharges README O7.
4. **O9** (`checked_views_safe`). The view theorems hold for the integrated run of every accepted
   program, not for `vrun` from an assumed `ViewsOk`.
5. **`checked_never_stuck`**, extended at every feature step.
6. **`checked_no_implicit_traps`** for every failing primitive, ranges and `byte_at` included. It is
   extended at every feature step.
7. **A trap during cleanup** ends the run with that trap: no later cleanup runs and its loans are
   not released. `abort` runs no cleanup.
8. **`instrumentation_erasure`** (1.9).
9. **Ghost-log adequacy** (`ghost_log_adequate`):
   - live bytes per lineage equal the sum of `Layout.size` over live blocks;
   - the log-derived requested bytes and count match the alloc events;
   - net and peak per call frame are measured from the frame's entry baseline, including failure
     paths and transfers.

   The analysis theorems (`heap_peak_sound` and the rest) belong to R-0595.
10. **Termination under evidence** (`checked_terminates`). A function whose termination fact is
    established reaches a non-`outOfFuel` outcome for some fuel. It promises no fuel bound:
    quantitative bounds are separate facts (`../archive/crucial-2026-11-06/MEMORY.md:56`). A permission is named in the
    fact.
11. **Checker theorems:** `check_correspond` and `check_refused` (1.10), and `ll1_sound` for the
    grammar.
12. **R-0588**: stated in `Facts`, not proved, at A2i.
13. **O8 and O10.** Deferred calls run in the integrated semantics, so the `ok` oracle of part 3 goes
    (O8, discharged at slice 14). Loop conditions are evaluated, so the `σ` oracle of part 5 goes
    (O10, discharged when the allocation fragment is ported at slice 19; loops exist from slice 4).
14. **Examples:** the leakage and accounting interfaces are kernel-checked against examples (R-0592), from phase B, before the slices that give them semantics.

### 3.2 Existing theorems restated on the integrated Core

| Theorem | File | On the integrated Core |
|---|---|---|
| `rep_roundtrip` (design 1) | `Proofs/Rep.lean:57` | same name; unified `Ty`/`Val` for the types `Layout` covers |
| `defer_cleanup_releases` (design 2) | `Proofs/Cleanup.lean:225` | same name and all five conclusions, including registration and dependency; over the integrated run, without the `ok` oracle |
| `open_mut_freezes_root` (design 3), `shared_open_grants_no_write` | `Proofs/Borrow.lean:132,217` | same names; over `Admission` |
| `reset_fresh` (design 4) | `Proofs/Brand.lean:150` | same name and all three conclusions; over the integrated run, without the `σ` oracle |
| `checked_never_stuck`, `checked_no_use_after_move` | `Proofs/Slice1.lean:567,571` | integrated |
| `checked_exits_or_traps` / `nht:checked_no_implicit_traps`, `nht:checked_exits` | `Proofs/Slice1.lean:577` | integrated; `checked_exits` gains the termination-evidence premise |
| `nht:add0_ok`, `add0_never_stuck`, `add0_never_traps` | `nht:Proofs/Examples.lean` | kept as examples, with guarded `add100` variants |
| `check_iff` | `Proofs/Checker.lean:285` | becomes `check_correspond`, plus `check_refused` |
| `slice0_exits`, `slice0_never_traps` | `Proofs/Slice0.lean:144,151` | over `Fragment.core s0 p ∧ Admitted p` (step 12); `toCore_injective` is retired with `toCore` (I3) |
| `view_not_moved`, `view_not_used_after`, `root_restored`, `range_open_mut_freezes_root`, `enter_in_bounds` | `Proofs/Borrow.lean:380-433` | integrated |
| `range_can_open`, `range_known_opens` | `Proofs/Borrow.lean:336,349` | become "every run-time source agreeing with the evidence opens" |
| `knownInvalid_iff`, `knownInvalid_traps` | `Proofs/Borrow.lean:265,329` | retired: subsumed by admission on evidence |
| `view_in_source`, `view_get`, `view_set`, `viewsOk_*`, `view_write_exact`, `range_block_frame` | `Proofs/Borrow.lean:471-593` | integrated (3.1.4) |
| `utf8_split`, `text_view_utf8` | `Proofs/Borrow.lean:639,676` | unchanged (pure); move with text |
| `brand_mismatch_refused`, `reset_requires_no_old`, `reset_refuses_old`, `checked_passes_brand_checks` | `Proofs/Brand.lean:41-64,367` | integrated |
| `brand_erasure`, `ghost_run` | `Proofs/Brand.lean:490,508` | `instrumentation_erasure` and the run's adequacy |
| `authority_contained` | `Proofs/Authority.lean:174` | integrated with grant provenance (3.1.3); `sub_iff`, `equiv_iff` unchanged |
| `checked_observation`, `checked_observable`, `refused_start` | `Proofs/Observe.lean:48,61,69` | over the new `Outcome`, strengthened by no hidden traps: an accepted program's observation is its exit status, or 134 with a trap line of kind `abort` at an explicit `abort()` site, never an implicit trap; 125 only at a refused start; `exhausted` only under its named target premise |
| `abort_no_cleanup` | `Proofs/Cleanup.lean:264` | integrated (3.1.7) |

---

## 4. Decisions to place in the spec, and where each goes

Every decision is recorded once, as a row in `docs/decisions.md`. The spec cites the row in the doc
comment of the definition it governs; the README and `Spec.lean` cite nothing and restate nothing.
The rows of 2026-11-06 are recorded on `wip/decisions-followup` a0e588f59: [vertical
slices](../decisions.md#decided-project-vertical-slices-2026-11-06) (with the slice plan's six owner
answers), [SPARK](../decisions.md#decided-project-spark-2026-11-06) (function values, struct
invariants, `assert … by thm`, observable events) and [trap line, layout and spec
version](../decisions.md#decided-project-runtime-2026-11-06); rule revisions are in R-0593/R-0603. A
step cites the row once it is on main; a step that needs a row not yet on main waits for it.

| # | Decision | Module | Definition | Slice |
|---|---|---|---|---|
| D1 | No hidden traps (Q1; OD10, ARITH-3, MEM-6, OD118 revised) | `Primitives`, `Evidence`, `Admission` | failure condition per primitive; evidence premise per operation; an executable checker with `check_correspond`; `checked_no_implicit_traps` | 0, extended per slice |
| D2 | The octagon checker, its limits and closed evidence (Q2, Q4, Q7–Q9) | `Evidence` | fact language, transfers, limits as spec constants, evidence sources, evidence graph; a fact not established at a site is a refusal of kind `notEstablished` | intervals at 0; octagons at 3 |
| D3 | Loops (Q5; OD47–49, OD105) | `Admission`, `Evidence` | footprint forgotten at the head, bounds snapshot, invariant before the loop and after every iteration, exit join | 4 |
| D4 | Termination (Q11–Q13; OD82 extended) | `Syntax`, `Termination`, `Obligations` | `#[variant]` on loops and functions (one measure per recursive cycle); `#[may_not_terminate("reason")]` with a non-empty reason, on loops at 4, on one designated function per recursive component at 5, on extern and blocking declarations at the slices that introduce them, as an expression prefix on indirect calls at 20; `checked_terminates`; termination facts | 4, 5, 20 |
| D5 | Trap sites and the trap line ([row](../decisions.md#decided-project-runtime-2026-11-06); R-0496: `concrete: trap: <kind> at <path>:<line>:<col> (site <id>)`, path relative to the project root with forward slashes, 1-based) | `Semantics`, `Contract` | `Outcome.trap k site`; the trap line as a function of kind and span; closes README O4 | 1 |
| D6 | `checked_*` with `ArithmeticError` | `Types`, `Primitives` | `ArithmeticError` from phase A (it is also the `arith` trap payload); the `checked_*` operations returning `Result<T, ArithmeticError>`; signed `MIN / -1` and `MIN % -1` give `Overflow` | type at A; operations at 12 |
| D7 | `#[proof(path)]`, four roles | `Syntax`, `Obligations`, `Admission`, `EvidenceSem` | a `TheoremId`; obligation schemas generated before admission; roles (a), (b), (d) are admission discharges and refusals, role (c) a fact; one discharge edge per obligation | 8 |
| D8 | The attribute-prefix production, LL(1) (R-0574) | `Grammar`, `Syntax` | the production in the grammar data; `ll1_sound`; attachment by kind (OD82) | 0 (grammar), used from 4 |
| D9 | Facts vocabulary, statuses, coverage, strength, vacuity, dependency flow (facts decision; R-0585) | `Facts` | 2.1 item 8; every kind present from phase B, `notApplicable` until its slice | B |
| D10 | Diagnostics are typed checker results; rule ID = Lean rule name plus that rule's own revision (R-0593, R-0603); one `spec_version` per output, 0.x before the freeze and 1.0 at A2c ([runtime row](../decisions.md#decided-project-runtime-2026-11-06)) | `Ident`, `Checker` | `RuleId` and `spec_version` (step 2); `Rule`, `Subject`, `CheckResult`, the retired-ID registry and the lifecycle checks (1.10) with `incomplete`, `check_correspond`, `check_refused` | A (steps 2, 9) |
| D11 | Byte accounting (R-0592) | `Ident`, `Values`, `Semantics` | `LineageId`, the ghost-log alloc and destroy records, the derived quantities, with examples over hand-written logs at B; the semantics and `ghost_log_adequate` at 19 | B, 19 |
| D12 | The leakage-trace interface (R-0592): `Secret<T>`, `classify`, `declassify`, `ct_select`, `ct_eq` | `Types`, `Primitives`, `Semantics` | leakage records in the ghost log and a variable-time flag per primitive at B (with examples); `Secret<T>` on scalars, `classify` (the pure classify of a value passed in, no wrapper duplicating it), the entropy capability's `Secret<T>` output from slice 7 ([decision](../decisions.md#decided-2026-11-07-stack-randomness)), `declassify(x, "reason")`, `ct_select`, the closed CT operation table and the Q2.2-revised scalar Copy rule at 7; fill-bytes from entropy at 15; grant-backed sources at 16, 18, 27 | B, 6, 7, 15 |
| D13 | Q4.6 and Q4.7 revised | `Primitives`, `Borrow`, `Evidence` | `byte_at` needs an in-range offset; a range without `else` needs evidence of validity | 15, 16 |
| D14 | Closed evidence (Q9.11, OD125 qualified, CON-2); `assume` never admits safety (OD80, CON-6); vacuous preconditions (OD120) | `Evidence`, `Obligations`, `Admission` | evidence sources; `assume` excluded; vacuity refusal | 5, 8 |
| D15 | Policies and `Concrete.toml` | not Core, except the stack budget | compiler data over `Facts`; the spec defines `Fact.Holds` and the mandatory stack-budget refusal (D32) | — |
| D16 | The semantic contract (R-0496) | `Contract` | 2.1 item 9, stated before `Semantics` (step 10) and refined per slice | B |
| D17 | Q6.7, Q9.9, Q9.10 | `Semantics` | 1.6, 1.8, 1.9 | B |
| D18 | Q9.4 flat data and matches; Q9.5 heap open; Q1.3 pack and unpack; Q9.7 one inventory | `Types`, `Syntax`, `Primitives` | 1.3, 1.5, 1.11 | 9, 10, 19 |
| D19 | Slice-0 surface: a distinct "not supported in this slice" refusal; leading zeros refused | `Lexical`, `Grammar`, `Fragments` | `Rule.fragment`, kind `unsupported`; the lexical rule | A (steps 6, 7) |
| D20 | Thin vertical slices, integers first ([row](../decisions.md#decided-project-vertical-slices-2026-11-06)); the agreed order ([vertical slices](../../ROADMAP.md#vertical-slices)) | `Fragments`; the step order (6) | one `SliceId` per slice; each slice a pair of fragment predicates | all |
| D21 | Abort is the same terminal trap (`dec:1224`); may-abort is a separate fact | `Semantics`, `Facts` | `TrapKind.abort`, not `implicit`; explicit `abort()` in slice 1 (slice plan owner question 6) | 1 |
| D22 | Allocation failure is a value (Q1.8, Q1.9) | `Primitives` | `AllocFailure<T>`; `exhausted` is target-only | 19 |
| D23 | Default struct layout (SPARK or runtime row, 2026-11-06): declaration order, natural alignment, size rounded up to the alignment, a zero-sized type has no bytes | `Values` (`Layout`) | `Layout.size`, `Layout.align`, field offsets; the empty struct of slice 1 has size 0 | 1 (ZST), 9 |
| D24 | An extensible observable-event kind in Core ([SPARK row](../decisions.md#decided-project-spark-2026-11-06); R-0602: it also protects secret wipes) | `Semantics`, `Contract` | `Event.kind`; only these feed `observe` (1.7); wipe events made observable-protected at 7 | B |
| D25 | A function whose `#[requires]` is a safety precondition is refused as a function value, including through wrappers and specialization ([SPARK row](../decisions.md#decided-project-spark-2026-11-06); R-0601: the function-type-contract design, trait methods included, is a prerequisite for accepting slice 20; under that design, a function value with a matching function-type contract is accepted) | `Admission` | an admission premise on function-value formation without a matching function-type contract, re-checked after Mono | 20, after R-0601 |
| D26 | `#[invariant(...)]` on a struct with private fields holds at every exposure ([SPARK row](../decisions.md#decided-project-spark-2026-11-06)) | `Syntax`, `Obligations`, `Admission` | the struct invariant and its obligations (established at construction, preserved by field writes inside the declaring module, assumed at every exposure outside it); defined in A2i | spec in A2i; source feature at 17 (S2) |
| D27 | `assert(e) by thm(...)`, an extension of OD79 ([SPARK row](../decisions.md#decided-project-spark-2026-11-06)) | `Syntax`, `Obligations`, `EvidenceSem` | one fact at one point, the theorem's hypotheses fully discharged; a role kept separate from `#[proof]` (which discharges the function's obligations) | 8 |
| D29 | `#[overflow_checked]` (ARITH-9) retired | `Grammar` | no production; the attribute is refused as unknown | 0 |
| D30 | `Concrete.toml`: typed, versioned `[policy]` and `[assumptions]` tables, unknown keys and versions refused | not Core, except the stack budget (D32) | compiler data; the spec defines the facts a policy reads | — |
| D32 | Stack bounds ([decision](../decisions.md#decided-2026-11-07-stack-randomness)) | `Facts`, `Admission` | each program's maximum stack use, measured from the final binary on both backends, as a fact; an unknown bound or one over the `Concrete.toml` stack budget refused; proven bounds for recursion at 5 | 0, 5 |
| D33 | Step bounds ([decision](../decisions.md#decided-2026-11-07-step-bounds)) | `Semantics`, `Facts` | a step-counting cost model in abstract operations over input and pattern size, built with loops and extended through calls; step-bound proofs; wall-clock claims stay with separate backend evidence | 4, 5, 8 |
| D31 | R-0595 heap bounds at the allocation slice; RocksDB and ethlambda programs audited before acceptance (R-0604) | not Core | the facts kinds D9 already holds | 19, 25, after 28 ([decision](../decisions.md#decided-2026-11-07-final-slices)) |
| D28 | One Core syntax; slice 0 by fragment predicates (I3, decided internally 2026-11-06) | `Syntax`, `Fragments` | `Spec/Slice0.lean`'s syntax and `toCore` deleted at step 12, when the new admission and semantics can carry the slice-0 theorems | B (step 12) |

---

## 5. Questions and internal choices

### 5.1 Answered scheduling questions (coordinator, 2026-11-06)

Draft 2's owner questions are decided: O-1 by D23, O-2 and O-3 by D10. Draft 3's scheduling
questions were answered by the coordinator:

**S1. When A2i closes.** A2i integrates the whole decided Core before the A2c freeze. Inside A2i,
fragments are ported in slice order, but every old fragment is folded into the one Core, and every
slice's spec side (0–27) is written, before A2c. This is a proposal waiting for the owner, since
it widens A2i beyond its ROADMAP acceptance. Step 29 is the gate: no old fragment module
remains.

**S2. D26 and visibility.** `#[invariant]` on private-field structs needs field privacy, which
arrives with modules. The spec definition is written in A2i, with the structs (step 20); the source
feature is scheduled at slice 17 (modules). The owner approved this on 2026-11-07 ("ok go ahead all
yes then as you said right"), and the SPARK row records the revision.

**S3. the slice plan's owner questions.** All six recommended answers are approved ("ok do everything",
2026-11-06) and recorded on `wip/decisions-followup` a0e588f59: the pure `classify` is slice 7's
secret source (D12; since 2026-11-07 the entropy output is a second source); ARITH-9 `#[overflow_checked]` is retired (D29); `Concrete.toml` has typed,
versioned `[policy]` and `[assumptions]` tables (D30); R-0595 lands at slice 19 (D31); RocksDB and
ethlambda are audited before acceptance (D31); `abort()` is in slice 1 (D21).

### 5.2 Internal (decided rows; no owner question)

- **I1.** Everything in sections 1.1–1.15.
- **I2.** The fuel convention, and the adequacy lemmas of 1.8.
- **I3.** One Core syntax; the slice-0 syntax and `toCore` are deleted (D28).
- **I4.** `ArithmeticError` shared by `checked_*` and the `arith` trap kind.
- **I5.** Retiring `knownViolates`, `RangeFacts`, `knownInvalid` and `Agrees`.
- **I6.** A deferred call's loans end after the call (R-0511, OD2 (4)).
- **I7.** Abort is a trap with status 134 (`dec:1224`).
- **I8.** Allocator release expressed as `give_up` of the allocator value (Q2.1).
- **I9.** Examples in `Spec/` are the documentation form: `Spec/` may hold `example`s (checked by
  the trust ban's leg X), never a `theorem`. The R-0560 gate states exactly this, so
  `Spec/Examples.lean` needs no exception.

No step should change what an example accepts or what a program observes beyond the decided rows.
If one does, it goes to the owner (P1).

---

## 6. Step order

**Every commit:**

- `lake build ConcreteSpec` with `warningAsError`;
- no `sorry` and no `native_decide`; axioms only `propext`, `Quot.sound`, `Classical.choice`;
- `scripts/proofs` green: the axiom golden updated in the same commit, the trust ban, the kernel
  replay and the external checker;
- Codex review per step or per small group, until ACCEPT; pushed to lambdaclass/concrete.
- phase A steps land on main through the train (review leg on) as soon as they pass; later steps
  land in small groups.

**Every slice step (phase C)** extends, in the same commit:

- `checked_never_stuck`, `checked_no_implicit_traps`, `checked_terminates` (from slice 4);
- `check_correspond` and `check_refused`;
- `run_sound`, `run_complete`, `eval_deterministic`;
- `instrumentation_erasure`;
- accepted and refused examples for every new rule, with its `RuleId`;
- the fact kinds the slice makes applicable.

One concept changes per commit. An old definition is deleted in the commit that moves its last
user, together with a correspondence lemma when an evaluator moves (1.8). Steps that edit shared
foundations are sequential.

**Phase A: slice 0's front end first, each step landed on main**

The owner asked to unify first (owner, 2026-11-06: "yes let's make the spec the best and unify things we have to unify" and "we should unifyt things before going forward right?"). This plan's proposed order for that: what slice 0's front end needs lands first, as its own early steps,
each landed on main through the train (review leg on) as soon as it passes, so the slice-0 worker
builds elaboration and admission against them.

**Coexistence with the old kernel.** The new Core is declared in the namespace `ConcreteSpec.Core`
from its first step, so its `Ty`, `Val`, `Program` and `Outcome` never collide with the old
kernel's names in `ConcreteSpec`. Old modules whose file names the new Core takes (`Syntax`,
`Semantics`, `Checker`, `Ownership`) are renamed in step 0 to `Slice1Syntax`, `Slice1Semantics`,
`Slice1Checker` and `Slice1Ownership`, a rename with no change of meaning. The new `Syntax` covers every construct of the
old kernel (structs with scalar fields included) as well as slices 0 and 1, so one translation
`embed : old Program → Core.Program` is total. Each bridge lemma is stated over `embed` and deleted
with the last old proof that uses it (step 12). No old proof is deleted before its restatement
builds.

0. Land `wip/spec-root`, then `wip/spec-nht` M1 (merged with spec-root, checks green). Small
   commits: doc hygiene (1.14); the `Examples` imports of `Rep`, `Observation` and `Slice0` replaced
   by direct imports of the old kernel modules they use, and the declarations they took from
   `Examples` moved to the module that uses them (or their examples moved to the end of
   `Examples.lean`); the `Ownership` import dropped from the old semantics if unused (otherwise the shared
   definitions move to the old `Syntax`); the four renames above.
1. `Basics`: `AList` (`lookup`, `update`, `erase`, `map`) and its lemmas. Migrate the lookups one
   fragment per commit and delete the duplicate lemmas.
2. `Ident`: the typed identities, `SiteId`, `Span`, `SliceId`, `RuleId` (a Lean name and a revision,
   1.10) and `spec_version`. No rule type lives here, so `Ident` depends on nothing downstream.
3. `Types`: `Ty` (complete for every type the old fragments use, with `Ty.brands`),
   `ArithmeticError`, `TrapKind`, `Cap`, `GrantTy`. `Outcome`, in its own module. `Values` with
   `Val.HasTy` and `Layout` (D23), porting `Spec/Rep.lean`: `rep_roundtrip` keeps its name and
   statement over `Layout`, and `Rep` is deleted in this step.
4. `Primitives`, as far as slices 0 and 1 and the old kernel need: `PrimOp`, signatures and failure
   conditions. A prerequisite of `Syntax` (2.2), not in the owner's list.
5. `Syntax`: the one Core syntax, with `SiteId`s, a site table and attributes as data, covering
   slices 0 and 1 and the old kernel's constructs; `embed`. Later constructs are added by their
   slice; old feature fragments keep their own statement types until ported.
6. `Lexical` (the lexical rules, leading zeros refused, D19) and `Grammar` (the grammar as data,
   `ll1`, `ll1_sound`, the attribute-prefix production, D8, ARITH-9's attribute absent, D29).
7. `Fragments`: `Fragment.surface` and `Fragment.core` for slices 0 and 1, and the "not supported in
   this slice" rule (D19). The old slice-0 syntax, `toCore` and its proofs stay until step 12; a
   lemma relates `Fragment.core s0` to the image of `toCore`. The `check`/`run` examples of
   `Spec/Slice0.lean` move to `Spec/Examples.lean` here.
8. `Evidence`, as far as slices 0 and 1 need (interval admission moved from `nht:Ownership`,
   evidence sources, the evidence graph), `Obligations` with no schemas yet, `Typing` (from
   `Statics`) and the new `Ownership` (the old `Ownership` rules restated over Core syntax), so the
   static chain of 2.2 exists before `Admission`. The old one stays, as `Slice1Ownership`
   (step 0), until step 12.
9. `Admission`: the judgment with named premises for slices 0 and 1 and the old kernel's constructs,
   and `Checker` with `Rule`, `Subject`, `CheckResult`,
   `Refusal`, `check_correspond` and `check_refused` (replacing `check_iff`), the rule-to-refusal gate
   and the rule/version lifecycle checks (1.10). Slice 1's refusals, including the refused
   `200 + 56` control. Two levels are kept apart. Core admission (`Core.Admitted`, no lexing,
   parsing or fragment) is bridged: `Core.Admitted (embed p) ↔` the old `Checked p` for every old program,
   abort-free since the old kernel has no `abort`. Source-level `Checked` (lexing, parsing, the
   fragment predicate, then admission) is what `check_correspond` is about; it is not bridged.
   `abort()` is admitted at step 12, with the semantics that gives it meaning.

**Every phase-A step, like every later one,** gives each new rule a readable doc comment citing its
decision row and at least one accepted and one refused example (`RM:331`). The per-rule gate lands
with `Ident` (step 2) and works on `RuleId`s directly: every declaration registered as a rule (a
`RuleId` naming it) has a doc comment and examples that cite that `RuleId`. Step 9 adds the typed
check that every `Rule` constructor's `Rule.id` names a registered rule.

**Phase B: the kernel's contract and semantics (slices 0 and 1)**

10. `Contract` before the evaluator (`RM:97-105`): `Event` with its kinds (D24), `Behaviour`,
    the new `observe` over the sited `Outcome`, the trap line (D5), and the run predicates. The old
    `Observation` and its proofs (`checked_observation`, `checked_observable`, `refused_start`)
    stay until step 12.
11. `Semantics`: `State × Ghost`, `run`, `Eval`, the adequacy lemmas, `erase` and
    `instrumentation_erasure` (O3); the ghost-log record kinds for accounting and leakage with their
    derived quantities and examples (D11, D12 interfaces); the rest of `Primitives` (every existing
    operation's contract, pure and stateful). The bridge `Core.run (embed p) n ≈ run p n`.
    `EvidenceSem`, and `Facts` with every kind (D9).
12. Slice 1's semantics and theorems (Q8.2): the zero-sized empty struct, move, explicit `abort()`
    with the trap line (D5, D21), `checked_never_stuck`, `checked_no_implicit_traps`,
    `checked_exits`, over the new admission and semantics. The slice-0 theorems over
    `Fragment.core s0`, and the slice-0 stack fact and its budget refusal (D32). The old kernel (`Slice1Syntax`, `Statics`, `Slice1Semantics`, `Slice1Checker`,
    `Slice1Ownership`, `Slice0`, `Observation`) and every bridge lemma are deleted;
    the observation theorems are restated over the new `observe` in the same commit. The old kernel's struct-field
    rules stay in Core, outside `Fragment.core s1` until slice 9.

Old feature fragments keep their own result types until their slice; the sited `Outcome` is used by
the kernel, not migrated across every fragment at once.

**Porting a feature fragment** (phase C). When an old fragment's run takes an oracle (`σ` for loop
tests in `Brand`, `ok` for deferred calls in `Cleanup`), the correspondence goes in the realizable
direction: every integrated run determines an oracle (its actual condition outcomes and call
results) under which the old run agrees with it. The old theorems, which hold for every oracle, then
apply to the integrated run, and are restated without the oracle in the same commit.

**Phase C: slices, in the agreed order ([vertical slices](../../ROADMAP.md#vertical-slices))**

Each step adds the slice's syntax, premises, semantics, facts and examples, and ports any old
fragment whose feature the slice introduces, deleting it in the same commit. Where an old fragment
already covers more than its slice (capability arguments in `Authority`, text in `Borrow`), the port
brings that Core ahead of its source slice with `Fragment.surface` excluding it, so no old proof is
dropped and no source feature arrives early. `Borrow` is the one fragment ported in two steps
(22, 24), stated as such.

13. Slice 2: all integer widths, literal forms, unary `-`, `- * / %`, bitwise operations, shifts,
    lossless `as`, `wrapping_*`, `saturating_*`, named lossy conversions; each with its failure
    condition.
14. Slice 3: `bool`, comparisons, `&& || !`, `Unit`, value `if`, trailing values, `let mut`,
    assignment, no shadowing, `discard`; the octagon checker through branches and joins (D2); the
    may-abort fact.
15. Slice 4: `while`, `for … from … until`, labels, `break`, `continue`, `#[invariant]`,
    `#[variant]`, `#[may_not_terminate("r")]` on loops; the loop rule (D3); `Termination` and
    `checked_terminates` (D4); the step-counting cost model for loops (D33).
16. Slice 5: functions, parameters, calls, recursion with one `#[variant]` or permission per
    recursive component, `#[requires]` and `#[ensures]` with call-site obligation schemas, closed
    call evidence (D14); the always-aborts fact and path pruning after an always-aborting call
    (R-0600); proven stack bounds for recursion (D32) and the cost model through calls (D33).
17. Slice 6: authority. Port `Authority` whole (calls exist): `with(...)`, inclusion, the primitive
    effect table, `main`'s grant parameters, launcher minting, startup refusal 125, Start/Exit
    events, and the entropy capability granted to `main` with its source as a named trusted
    premise ([decision](../decisions.md#decided-2026-11-07-stack-randomness)). `authority_contained` with grant provenance; `sub_iff` and `equiv_iff` move unchanged.
    The old fragment's explicit capability arguments and capability-variable substitution come into
    Core here, ahead of slice 11, with the source forms outside `Fragment.surface` until slice 11.
    Delete `Authority.*`.
18. Slice 7: `Secret<T>` on scalars and the leakage semantics (D12), with entropy output as
    `Secret<T>`; the wipe event kind (R-0602).
19. Slice 8: `assert`, `assume`, `ghost let`, `spec fn`, `#[proof(path)]` (D7),
    `assert(e) by thm(...)` (D27), vacuity refusal and strength labels, `#[test]`; step-bound
    proofs (D33).
20. Slice 9: structs with named fields of any type, declared `Copy`, field access and writes,
    destructuring, field facts, type aliases. The struct-invariant definition and its obligations
    (D26) are written here, with every field treated as module-private; the source feature is
    enabled in `Fragment.surface` at slice 17 (S2).
21. Slice 10: enums and `match`. Slice 11: generics, `CapVarId`, Mono re-admission as a statement.
    Slice 12: `Option`, `Result`, `checked_*` (D6), `try_convert`. One step per slice.
22. Slice 13: references and borrow blocks. Port the loan half of `Borrow`: locks derived from loans,
    `open_mut_freezes_root`, `shared_open_grants_no_write`. The view half that remains in `Borrow`
    (`RangeOk` and the view proofs) is retargeted to the Core's loans and access rules in the same
    commit, so the old `Place`, `Lock` and `Ok` are deleted here.
23. Slice 14: traits, `Destroy`, `defer`. Port `Cleanup` whole: frames, exits, late reads,
    unwinding in the run; `defer_cleanup_releases` with all five conclusions (its dependency order
    uses `Ty.brands`, present since step 3), `abort_no_cleanup`; 3.1.1, 3.1.7, 3.1.3 for cleanup
    (O7), O8. Delete `Cleanup.*`.
24. Slice 15: arrays and range views, with entropy fill-bytes (D12). Port the view half of `Borrow` (`VStmt`, `vrun` deleted);
    3.1.4 (O9); D13. Text sources and views come into Core here, ahead of slice 16 (surface
    excluded), and `utf8_split` and `text_view_utf8` move to `Proofs/` beside them. Delete
    `Borrow.*`.
25. Slices 16–18: text and Console, modules and constants (with `pub`, field privacy and the
    struct-invariant source feature, D26), Fs and Environment; one step per slice.
26. Slice 19: allocation (with R-0595's heap facts, D31). Port `Brand` whole: brands, lineages, `pack`/`unpack`, child backing
    dependencies, reset and release, heap open, `AllocFailure<T>`; `reset_fresh` and the brand
    refusals; `ghost_log_adequate`; 3.1.2 complete; O10. The old fragment's `.free` statement and its
    examples map to `destroy(h)` (a consuming `Destroy`, OD59): no source `free` is revived. Delete
    `Brand.*`. No old fragment remains.
27. Slices 20–27, one step each. Function values (slice 20) come after R-0601's function-type
    contract design, with D25.

**Phase D: closure, all before A2c (S1)**

28. `Facts` and `Contract` completed, including the R-0588 statement (in `Facts`).
29. The integration gate (S1): no old fragment module remains. It names each: `Rep`, `Observation`,
    `Statics`, `Slice1Ownership`, `Slice1Syntax`, `Slice1Semantics`, `Slice1Checker`, `Slice0`,
    `Authority`, `Borrow`, `Cleanup`, `Brand`, and fails with a negative control (a stub of any of
    them). Every slice's `Fragment.surface` and `Fragment.core` are defined.
30. The R-0560 gate (no normative definition outside the library; no `theorem` in `Spec/`, per I9;
    each with a negative control). Regenerate the owner view. README obligations O3, O4, O7, O8,
    O9 and O10 discharged.
31. The outside-reader pass (R-0559), then the owner's read (A2c).

---

## 7. Delta reviews of draft 2

Codex: ACCEPT-WITH-CHANGES. agy: ACCEPT-WITH-CHANGES. Kimi: REVISE. The raw outputs are not kept in the repository.
Every finding below was checked against the files.

**Accepted and fixed.**

- Step order (all three). Phase C now follows the agreed slice order: `if` and octagon evidence
  (slice 3) before `while` and termination (4), functions (5) before authority (6), and `Authority`
  ported whole at slice 6, so call and substitution theorems never disappear mid-sequence. Heap open
  arrives with allocation (19), after its types.
- Horizontal outcome migration (agy). Old fragments keep their result types until their slice.
- Stateful primitives (all three). `Primitives` holds contracts for all; pure ones also evaluate,
  stateful transitions are rules in `Semantics` (1.11).
- `Syntax` before `Primitives` (all three). The order is now `Primitives → Syntax`.
- `#[proof]` obligations after admission (all three). New `Obligations` module before `Admission`;
  semantics in `EvidenceSem`; D7 maps to `Obligations` and `Admission`.
- `Contract` before `Admission` while importing it (Kimi). `Contract` now states predicates over runs
  only; the theorems are in `Proofs/`.
- `EvidenceSem` had no step (Kimi): step 11.
- False determinism/adequacy claim (Codex): 1.8 now names what exists and the replacement lemmas.
- Unsupported-slice refusals not representable (Kimi): `Rule.fragment` and kind `unsupported`.
- `Spec/Examples.lean` against the no-proofs gate (Kimi): I9.
- Counts: eight lookups; three linearity encodings, `Place.owner` reclassified (agy, Kimi).
- §7 of draft 2 claimed nobody asked for fewer modules; agy asked for about 17 (Kimi). Corrected
  in 2.2, with two folds made (`Layout` into `Values`, observation into `Contract`).
- `checked_terminates` missing from the per-step gates (Kimi): added.
- O4 naming (Codex): the trap line is now decided (D5) and closes README O4; owner questions are
  numbered S1–S3 to avoid the clash.
- Duplicated decision record (Codex): each decision is one `decisions.md` row, cited on its
  definition (section 4 preamble).
- Absence completeness and the evidence graph (Codex): 2.1 item 8 and 1.13.
- Dependency-flow fact fields (Kimi), vacuity status (agy): 2.1 item 8.
- O8 and O10 (agy): 3.1 item 13 and steps 23, 26, 30.
- `checked_observation` restated precisely under no hidden traps (agy): 3.2.
- Atom/expression and value-correspondence duplicates, the named lookup lemmas, `CapVarId` and the
  ghost read checks (agy): 1.1, 1.2, 1.4, 1.9.
- Line offsets (agy): declaration names are the anchors; `RangeStep.trap`'s constructor line added.
  agy's `Brand.Stmt :71` does not match the reviewed revision (`:56`), so that one is kept.
- The `Secret<T>` Q2.2 revision (agy): D12 at slice 7.
- Child allocator backing dependencies (agy): step 26.

**Not adopted.**

- agy's 17-module target: see 2.2.
- Duplicate conformance examples across `Spec/Cleanup.lean`, `Spec/Brand.lean` and
  `Proofs/Examples.lean` (agy) are consolidated as each fragment is ported, not in a separate step.

**Owner request folded in** (owner, 2026-11-06: "yes let's make the spec the best and unify things we have to unify" and "we should unifyt things before going forward right?"): the plan proposes that phase A land slice 0's
front-end prerequisites first, each on main as soon as it passes (section 6).

**Coordinator answers folded in** (S1–S3, section 5.1): whole-Core integration before A2c, D26's
source feature at slice 17 with the spec in A2i, all slice-plan owner answers (D12, D21, D29–D31).

### 7.1 First review round of draft 3

Codex: REVISE. agy: ACCEPT-WITH-CHANGES. Kimi: REVISE. The raw outputs are not kept in the repository. All three
confirmed S1–S3 are folded in. Fixed, after checking each against the files:

- `Rule` in `Ident` was a cycle (all three): `Ident` holds only `RuleId` (a Lean name and a
  revision); the typed `Rule` is in `Checker` (1.10).
- Step 7 deleted the slice-0 syntax before its replacements existed (all three): the old kernel and
  its proofs stay until step 12; phase A states the coexistence strategy (namespace, four renames,
  a total `embed`, bridges over it, deleted at step 12).
- The bridge could not cover `abort()` (agy): it is stated for abort-free programs and `abort()` is
  admitted at step 12.
- The old kernel's broader syntax (Codex): the new `Syntax` covers it, so `embed` is total.
- `Outcome` collision (agy): the namespace `ConcreteSpec.Core`.
- `Rep` and `Observation` had no deletion (agy, Kimi): steps 3 and 12; the integration gate names
  every old module and has a negative control.
- `Obligations` and `Typing` came after `Admission` (agy): step 8.
- The contract came after the evaluator (Kimi): `Contract` (with `Event` and `Behaviour`) is step 10,
  `Semantics` step 11; statements needing admission or facts are in `Facts` (2.2).
- `EvidenceSem` claimed fact satisfaction before `Facts` (Kimi): it covers evidence facts, tokens and
  obligations; `Fact.Holds` is in `Facts`.
- Refusals of malformed text had no subject (Kimi): `Subject.span`; programs carry a site table;
  elaboration stays O1, owned by the compiler.
- The per-rule doc-and-examples gate applied only from phase C (Codex, Kimi): every step.
- The rule and version lifecycle (Codex, Kimi): the retired-ID registry, dependency-induced
  revisions, one header, CI checks with controls (1.10).
- Missing facts (Kimi, agy): `bounded`, unused permissions, always-aborts with pruning (R-0600).
- R-0601 before slice 20 (Kimi): step 27 and D25.
- Authority crossing slice 11, Borrow's text proofs, Borrow split, Cleanup's dependency order
  (Codex, agy, Kimi): Core ahead of surface with `Fragment.surface` excluding the forms; the
  `Borrow` two-step exception stated; `Ty.brands` from step 3.
- Oracle correspondence (Codex): stated in the realizable direction (phase C preamble).
- Secret wipes (Codex): a public event kind, not ghost (1.7).
- Brand's `.free` (Codex): mapped to `destroy(h)` (OD59).
- Stale or wrong text (all three): `Secret` "not in A2i"; correspondence-relation locations and
  roles (1.4); "if I3 is agreed"; "four folds"; D10, D11, D16, D19, D28 modules and phases.

### 7.2 Second review round of draft 3

Codex: REVISE. agy: ACCEPT. Kimi: REVISE. Codex: steps 0–2 can start now. Fixed:

- Step 10 deleted `Observation` before its proofs could move (Codex, Kimi): kept until step 12.
- R-0588 and post-Mono re-admission had two homes (Codex, Kimi): `Facts` only (2.1 item 9, 3.1
  item 12, step 28).
- The unconditional old-to-new correspondence in 1.8 (Codex): qualified to the realizable direction
  for oracle-driven evaluators.
- The per-rule gate before typed rules (Codex): it works on `RuleId`s from step 2; step 9 adds the
  typed check.
- The bridge used source-level `Checked` (Kimi): the bridge is over Core admission only.
- The new `Ownership` was missing from step 8 (Kimi): added; the old one is renamed until step 12.
- Step 0's `Examples` imports (Kimi): replaced by direct imports, with the declarations moved.
- Brand freshness (Kimi, agy): one definition (1.12).

### 7.3 Third review round of draft 3

Codex: ACCEPT-WITH-CHANGES. Kimi: ACCEPT-WITH-CHANGES. Both: phase A, steps 0–9, can start now.
agy produced no output: its headless mode refused a tool it needed (`command`), so this round has
two reviewers, recorded rather than waited on. Fixed: step 12 no longer deletes the new
`Ownership` (Codex, Kimi); the summary states `Borrow`'s two-step exception (Codex, Kimi); 1.10
names the admission judgment `Admitted`, with source-level `Checked` in `Checker` (Codex).
