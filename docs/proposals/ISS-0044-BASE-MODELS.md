# ISS-0044: rebase the base models onto Core

Status: design for review, not an implementation or a language decision.
Baseline: `9e48fdb06` (current base-migration lane). Owner: base-migration lane,
ROADMAP A2i (R-0571, R-0555). ISS-0044 remains open until the implementation
and its correspondence evidence land.

Evidence: `_review/AUDIT-s5-base-models-agy-aa44140a3.md` (whole audit) and
`_review/CONFIRM-s5-codex-aa44140a3.md` (VERDICT-1 through VERDICT-8).
The numbered parts below follow the **confirmation**, not the audit's inventory.
The structural cause is that the base models preceded the Core foundation and
were never rebased onto it. Per `docs/PHILOSOPHY.md:69` (every fact once),
retire redundant definitions; retain only abstractions with an independently
proved interpretation. Renaming or import substitution alone cannot close this issue.

## Reference discipline and scope

The evidence describes integration head `aa44140a3`. Paths and line numbers
explicitly marked **integration** below refer to that head, as recorded in the
confirmation. In particular `ConcreteSpec/Spec/Own.lean` and
`ConcreteSpec/Spec/Grant.lean` are absent from this checkout; do not invent local
replacements. Their accepted integration versions are dependencies of parts 6–8.
Current Core locations differ: `ConcreteSpec/Spec/Exec.lean:172` (Deferred),
`:243` (Final), `:468` (plan), `:499` (Cleans), and
`ConcreteSpec/Spec/Observe.lean:139` (Terminal), `:150` (status), `:176` (launch).
Revalidate all integration interfaces when the foundation is available.

No Lean changes are authorized by this note. All code blocks are proposed
signatures, not checked declarations. New adapter/proof names belong under
`ConcreteSpec.Core.BaseMigration`; existing base syntax may remain an input
language, but never an independent source of Core facts.

For compact citations, unqualified module names such as `Scalar` and
`Slice1Syntax` mean `ConcreteSpec/Spec/<name>.lean`; `Proofs/<name>` means
`ConcreteSpec/Proofs/<name>.lean`. A trailing `:line` uses that complete path.

## Decisions

| Part | Decision | Single owner / retained abstraction |
| --- | --- | --- |
| 1 | RETIRE | Core Var and IntTy; no conversion between duplicate identities |
| 2 | RETIRE | Core CTy/CVal; slice admission excludes unit explicitly |
| 3 | VIEW | Target-aware partial representation of slice values |
| 4 | VIEW | Slice execution and diagnostic stuckness over Core results/execution |
| 5 | RETIRE | Core hosted observations, startup and statuses |
| 6 | VIEW | Static cleanup dependency analysis interpreted in Core execution |
| 7 | VIEW | Static ownership checking and range/text views over Core ownership |
| 8 | VIEW | Static brand checking and loop abstraction over Generative |

## 1. Variables and integer universe — RETIRE

Exact pairs: `ConcreteSpec/Spec/Slice1Syntax.lean:87-90` versus
`ConcreteSpec/Spec/Ident.lean:41-44` (Var); Slice1Syntax `:96-122` versus
`ConcreteSpec/Spec/Scalar.lean:58-87` (IntTy, widths, signedness, bounds, range).
The slice contributes only a Boolean range interface; Core exposes a Prop.
Neither universe adds an integer type or a different bound to the other.
Delete the slice structures and arithmetic definitions. Use Core names directly;
where executable admission needs a Bool, use `decide (t.InRange n)`, not a second
bounds formula. Any Boolean/Prop bridge is a representation lemma, not soundness.

Migrate users: Spec/{Slice1Syntax,Statics,Slice1Semantics,Slice1Ownership,
Slice1Checker,Slice0,Rep,Examples}.lean and Proofs/{Slice1,Checker,Slice0,Rep,
Examples,Observe}.lean. Audit transitive imports in ConcreteSpec/Spec.lean and
ConcreteSpec.lean. Preserve Var identity semantics, including hint and id; no
freshening conversion is needed. No remaining independent subject needs a law.

## 2. Scalar types, values and typing — RETIRE

Exact pairs: Slice1Syntax `:129-132,163-166` (STy/Scalar) versus Scalar
`:395-406` (CTy/CVal); Slice1Semantics `:193-195` (SVTy) versus Scalar
`:409-417` (type equality plus validity). Base adds an int/bool-only interface;
Core adds unit and separates validity from expected type. These are not
interchangeable interfaces.

Delete STy/Scalar and the independent SVTy constructors. Use CTy/CVal directly;
express slice scalar admission as `v.ty = t ∧ v.Valid ∧ t ≠ .unit`.
Keep unit outside the existing slice syntax/checker admission, not outside Core.
Do not use Hybrid's zero size sentinel to give unit storage meaning
(`ConcreteSpec/Spec/Hybrid.lean:39-66`). Retain struct Ty/Val constructors: Core
scalar values are not a replacement for aggregate syntax.

Change the part-1 users plus Hybrid-facing Rep adapters. Reprove existing
checker equivalences and progress/preservation against the shared predicates in
Proofs/{Checker,Slice1,Rep,Examples}.lean. Predicate expansion/constructor
transport is representation evidence, not a new Law. No new scalar semantics.

## 3. Representation — VIEW

Exact pairs: `ConcreteSpec/Spec/Rep.lean:28-31` / Memory `:78-81` (bytes);
Rep `:34-36` / Hybrid `:48-51` (size); Rep `:65-66` / Scalar `:102-103`
(two's complement); Rep `:47-55` / Memory `:240-247` (radix); Rep `:59-75` /
Memory `:260-283` and Hybrid `:57-66` (scalar codec).
Proofs/Rep `:13-27,55-72` overlaps Proofs/Memory `:24-27,68-74,111-123`
and Proofs/Hybrid `:18-28`. These integration references are also present in
this checkout's scalar/memory/Hybrid interfaces.

Base adds declared-empty-struct representation and aggregate type selection.
Core adds provenance-bearing bytes and Target-dependent order. Delete the base
byte/radix/bit codecs and their independent proofs. New byte lists are lists of
Mem.MByte; scalar encoding/decoding delegates to Hybrid.encode/decode, hence
Mem.encodeInt/decodeInt and encodeBool/decodeBool. Every codec takes Target.

Use a partial encoder, including a declaration lookup:

```lean
encode : Target → Decls → Val → Option (List Mem.MByte)
decode : Target → Decls → Ty → List Mem.MByte → Option Val
-- Subject: encode. Proposed law rep_roundtrip:
RepVal Δ v t → (encode tgt Δ v).bind (decode tgt Δ t) = some v
-- Subject: encode. Proposed rejection statement (representation lemma):
findDecl Δ s = some d → d.fields ≠ [] → encode tgt Δ (.struct s fs) = none
```

Encode a struct only when its declaration exists, its fields are empty AND its
value fields are empty. Unknown declarations and every unsupported aggregate
return none. Keep the existing declared-empty-struct rule; do not choose padding
or nonempty struct layout. A malformed value must not acquire an encoding just
because the theorem's RepVal premise excludes it. Scalar validity admission is
explicit; decode must retain Core's initialization/length/bool checks.

For rep_roundtrip, the closed instance is u16 258 on the big-endian update of
linuxX8664, with bytes `[.init 1 none, .init 2 none]`. The mutant has **exactly** the type of encode and changes scalar byte order
to little endian while leaving decode fixed. The law has no premise fixing
the encoded byte list; its sole RepVal premise is unchanged by this mutation.

The positive and planted case both use the same Target, Δ, v, t and RepVal
proof. The mutant decodes to 513, refuting the conclusion. For the rejection statement,
use the existing pair declaration (`Examples.lean:31`) and a populated pair;
mutant `fun _ _ _ => some []` has the same encoder type and preserves the
lookup/nonempty premises while violating rejection. Also test unknown and
malformed empty structs. Round-trip proofs must derive Core codec laws; they
must not unfold two home-grown codecs.

Change Spec/Rep, Proofs/Rep and their examples in Spec/Examples and
Proofs/Examples. Add target cases in both orders, undef/provenance/invalid-bool
cases and empty-struct cases; retire old little-endian-only claims.

## 4. Results, outcomes and addition — VIEW

Exact pairs: Slice1Semantics `:33-60` / Outcome `:45-75` and **integration**
Exec `:245-257,828-839`; checkedAdd at Slice1Semantics `:105-108` /
Scalar `:183-195,215-219`. Base adds explanatory Stuck reasons and a small
expression interpreter; Core adds trap kinds, sites, configurations and more
endings. Arithmetic behavior already corresponds to **plain**, not checked.

Remove the slice Trap and duplicate operand result. Use Res/TrapKind directly;
keep stuck reasons as an optional diagnostic attached to a stuck derivation,
not a second semantic result. Retain fuel exhaustion only as inconclusive bounded
execution evidence, not divergence. Rename checkedAdd to plainAdd and define it
by mapping `Core.plain t .add a b` to the slice scalar Val. Do not change Scalar,
Outcome or Exec. Final outcomes become views of Core execution with explicit
site maps and exit-value admission, not a map inventing source sites.

```lean
plainAdd : IntTy → Int → Int → Res Val
-- Subject plainAdd; law add_agrees (all t, a, b, including overflow):
plainAdd t a b = (Core.plain t .add a b).map (fun n => Val.scalar (.int t n))
```

This equality is a representation lemma when the implementation delegates
exactly as specified; do not mislabel it a Law. The substantive law is:

```lean
-- Subject evalExpr, with its existing interpreter type after result migration:
IntTy.InRange t a → IntTy.InRange t b →
 evalExpr Δ env (.add (.int t a) (.int t b)) =
   ((Core.plain t .add a b).map (fun n => (Val.scalar (.int t n), env)))
```

Δ/env are closed empty inputs in the witnesses. Mutant of evalExpr's exact full function type: at an overflowing add,
return a wrapped successful scalar, otherwise behave as evalExpr. Closed positive
and planted arguments: u8 200 and 56, both in range. The Core result is
`.trap .overflow`; the mutant succeeds. Add the nonoverflow u8 2+3 instance.
No hypothesis assumes the arithmetic conclusion or excludes overflow.

A second simulation law must relate each admitted slice step to a nonempty Core
step sequence under its operand instance, preserving environments and traps;
reverse simulation covers Core steps at slice control boundaries. Site maps
are explicit input data. Do not demand equal fuel counts. Extend this to final
outcomes before claiming whole-program correspondence. The scalar expression
law alone does not close the entire part.

Change Spec/{Slice1Semantics,Slice1Syntax,Slice1Checker,Examples},
Proofs/{Slice1,Checker,Examples,Observe}; retain existing progress and soundness
strength. Migration must account for every use of checkedAdd, including
Proofs/Slice1.lean:394,396,446,578.

## 5. Hosted observation — RETIRE

Exact pairs: Observation `:47-50` / **integration** Observe `:322-325`
(status); Observation `:54-61` / integration Observe `:348-351` (launch).
Current owners are Observe `:150-177`. Base adds no hosted behavior: it
explicitly omits diagnostics (`Observation.lean:28-33`). Core retains trap
kind, site, span, rendered line and event histories. The omission is disclosed,
not evidence of a false full-diagnostic theorem.

Delete Spec/Observation.lean and its import in Spec.lean. Migrate
Spec/Examples.lean:464-474, Proofs/Observe.lean and its consumers in
Proofs/Examples and ConcreteSpec.lean to Core.Observe. Use actual site tables
and trapTerminal; no fabricated location, and no second status function.
Use part 4's execution embedding where examples originate as slice programs.
Bounded out-of-fuel/stuck results cannot be recast as hosted divergence.
No view needs to survive solely for status 125/134; test Core observations
including the distinction between normal return 134 and a trap.

## 6. Cleanup — VIEW

Exact pairs (base / **integration**): Cleanup `:48-73` / Exec
`:174-180,206-212` (deferred calls/frames); Cleanup `:75-77` / Exec `:699-704`
(order); Cleanup `:80-106,123-127` / Exec `:183-193,333-350,722-744`
(unwinding/execution); Cleanup `:135-158` / Grant `:63-84` and Own `:268-279`
(dependency admission/loan release). Current Exec owners are cited above.
Base adds static resource dependency summaries and a Boolean call-success
abstraction; Core adds executable functions, arguments, sites, stores and
actual control/loan transitions. Neither is an interchangeable implementation.

Retain dependency summaries as annotations on Core Deferred identities. Replace
base frames/exits/unwound/pending with Core frames and control projections;
remove the independent cleanup execution machine. The analysis computes
registration admissibility over Core register/plan and Core dependency state.
Interpret the old Boolean oracle only through a separately verified executable
call instance, never as a premise that the desired cleanup trace already holds.
Core call events and Own loan-release transitions justify summary events;
loan release is an internal proof event unless an existing observation rule
makes it external. Do not add hosted events.

Proposed analysis interface and law (all types in BaseMigration):

```lean
cleanupCheck : CleanupInput → Bool
-- CleanupInput contains a Core program, initial configuration, registrations,
-- dependency metadata and an exit; it does NOT contain a correctness certificate.
-- Subject cleanupCheck; law cleanup_sound:
RealizesMetadata i → CoreInitialWellFormed i → cleanupCheck i = true →
 ∀ tr c', Exec.Steps i.program i.start tr c' →
   CleanupSafePrefix i tr c'
```

`RealizesMetadata` relates each function's individual behavior/resources to its
summary, not global unwinding/order; `CoreInitialWellFormed` checks identities,
loans and dependencies before registration. `CleanupSafePrefix` requires exact
LIFO calls in the exited scopes, each loan kept until its call returns and
released at that activation's exit, no resource used after consumption, and no
cleanup after a trap. For completed successful exits add the full-plan equality
and resulting ownership-state correspondence (via Exec.Cleans). This is proved
from Exec step/exit laws plus analysis induction, not by runtime rechecking.

The acceptance premise prevents using an always-accept checker at the same
positive instance to expose bad ordering. Use a separate operational
correspondence law with a semantic subject and subject-independent premises:

```lean
cleanupTrace : CleanupInput → Nat → CleanupReport
-- executable Core instance, bounded result plus internal ownership events
-- law cleanup_correspondence (final law subject: cleanupTrace):
RealizesMetadata i → CoreInitialWellFormed i →
 ∀ n, ReportRelated (cleanupTrace i n) (coreCleanupReport i n)
```

`coreCleanupReport` is the Core runner on the translated input, with a specified
step budget; the adapter budget is the same runner budget, not old abstract
recursion fuel. ReportRelated preserves exact call identities/order, loan state,
terminal trap/site and inconclusive status, erasing only explicitly internal
steps. Translation is structural; its correctness is not a hypothesis.
Mutant of the **exact** `CleanupInput → Nat → CleanupReport` type reverses
pending call order before running, leaving calls/stores/sites unchanged.
Instance and Planted: well-formed allocator a, dependent v, registration a then
v, both calls return; sufficient closed fuel, trace v then a. The mutant
runs a first, violating trace/state correspondence with exactly the same
premises. A second same-type mutant continues after a first-call trap; use the
same valid registration with v trapping, and refute the extra call. Prove the
analysis soundness result separately; its evidence must use a semantic operation
parameter if a checker-acceptance premise would make the planted case invalid.

Change Spec/Cleanup and Proofs/Cleanup; migrate their examples in
Spec/Examples and Proofs/Examples. Own/Generative integration is mandatory for
loan/dependency claims; Exec-only trace order is insufficient for closure.

## 7. Ownership, borrows and range views — VIEW

Exact pairs (base / **integration**): Slice1Ownership `:19-50` / Own
`:43-48,150-154` (live slots/moves); Borrow `:27-87` / Own
`:82-95,136-147,165-174` (whole-root locks/access/borrow). The checker gap is
Own `:690-699`; README `:72-76` explicitly leaves its discharge open in the
integration evidence. Range traps: Borrow `:151-162,427-431,440-479` versus
Outcome `:45-51`.

Base adds static contexts, range facts, UTF-8 boundaries and range/text views;
Core adds runtime loan identities, parents, activation scope and permission
transitions. Static acceptance cannot be replaced by a runtime transition and
called soundness. Retain syntax-directed checking and unique range/text
abstractions. Context realization maps each live/moved variable, root, view,
loan parent and lexical scope to Core state. Whole-root permission queries use
Own, not copied lock arithmetic. Derive static transfer preservation for each
constructor, then executable-checker correspondence and reachable-step safety.

```lean
borrowStep : BorrowInput → BorrowResult
-- BorrowInput: source/view descriptor, range/index operation, Core Own state,
-- memory, lexical scope; BorrowResult: Core Res with updated state/view/value.
-- Subject borrowStep; law borrow_correspondence:
Own.WellFormed i.state → ContextRealizes i.context i.state →
 RangeFactsRealize i.facts i.memory →
 BorrowResultRelated (borrowStep i) (interpretBorrowInCore i)
```

`interpretBorrowInCore` uses Own.borrow/reborrowTo/endLoan/leave for ownership
and the single retained range/UTF-8 rules for view validity. It is not another
range implementation. Relation compares resulting permission state and values,
including exact TrapKind. No `Borrow.Ok` or permission-success premise is
needed for this runtime law: refused operations are part of its conclusion.
Mutant has **exactly** type `BorrowInput → BorrowResult` and changes only a
failed range opening from `.invalidRange` to `.outOfBounds`. Instance and
Planted share a well-formed root of length 2, truthful range facts and attempted
range [0,3); correct result invalidRange, mutant outOfBounds. Second instance:
valid range [0,2) then index 2, yielding outOfBounds. A same-type mutant
allowing a mutable reborrow of a shared loan is refuted at a well-formed shared
root/view, with no permission-success premise. Retain UTF-8 boundary examples.

The essential static law is also required:

```lean
-- Subject sliceStep : Program → SliceConfig → StepReport
-- StepReport contains the actions attempted, result and next configuration.
-- emittedActions preserves failed attempts; it does not filter by permission.
Checked p → InitialContextRealizes p c → Reachable p c c' →
  ∀ a ∈ emittedActions (sliceStep p c'), Own.StepPermitted c'.ownership a
```

Reachable uses a fixed, subject-independent Core execution relation; it must
not assume permission or use the candidate checker soundness theorem. The
mutant has exactly type `Program → SliceConfig → StepReport` and emits an owner write during a valid shared borrow in a checked
program; the fixed Core reachable state, acceptance and realization premises
are unchanged, and Own denies its emitted action. Closed witness program:
open shared view, read through it, close it, consume the root. This discharges
the checker obligation, not just a projection of an assumed CheckerAccepted.
Unbounded loop preservation needs induction/coinduction over reachability;
bounded examples alone cannot establish it.

Change Spec/{Borrow,Slice1Ownership,Slice1Checker,Slice1Semantics},
Proofs/{Borrow,Slice1,Checker,Examples} and affected examples/imports. Do not
copy Own into this branch while its foundation is unavailable.

## 8. Allocator brands — VIEW

Exact pairs (base / **integration**): Brand `:111-119` / Grant
`:36-40,54-60` (state/generation); Brand `:123-140,176-180` / Grant `:77-102`
(release/reset); Brand `:162-187,195-243` / Grant `:104-132` (ghost/erased runs).
Base adds a static checker and an abstract loop-test sequence; Core adds
lineage, lexical scope, phantom dependencies and concrete allocator callbacks.
Base release/reset relies on static admission while Core enforces dependency
guards. Do not remove Core guards or claim all untyped base runs correspond.

Retain the static syntax/checker and loop abstraction; translate operations to
Generative.LifetimeOp/Command and execute with Generative.run/runErased.
Replace State.bind/fresh counters, Run/ghost/erased transitions and brand erasure
with projections of that execution. Keep source-variable-to-Core-binder and
source-heap-to-Core-dependency maps; generated labels correspond via an injective
renaming, not equal numeric counters. Realization includes scope, lineage,
phantom dependencies and callback contracts, not just an environment map.

```lean
brandRun : BrandInput → Nat → BrandReport
-- BrandInput includes source program, initial Core state, loop-test stream,
-- binder map and concrete callbacks; report includes done/stuck/inconclusive.
-- Subject brandRun; law brand_correspondence:
Brand.Ok i.usedBrands i.context i.program i.finalContext →
 BrandContextRealizes i.context i.state i.binders →
 Generative.WellFormed i.state → CallbacksAdmissible i.callbacks →
 BrandReportRelated (brandRun i fuel) (coreBrandReport i fuel)
```

ReportRelated requires Core guard
success for admitted release/reset, matching allocation/free behavior, fresh
binder injection after reset, and erasure agreement with runErased; it must not
carry these facts as fields of the input realization. Prove simulation at loop
iteration boundaries for all test streams, including zero iterations. Run fuel
is the Core runner budget; old recursion fuel is not equated to Core steps.
Derive erasure from Core's erasure theorem plus simulation; delete the second
erasure proof over independent counters. A wrapper equality alone is a
representation lemma; the substantive law connects static Brand.Ok to guard
success and fresh correspondence through execution.

Mutant has **exactly** type `BrandInput → Nat → BrandReport` and reuses the
old binder on reset instead of the freshly generated one, otherwise preserving
Core callback actions. Closed Instance and Planted: new allocator a; allocate h;
free h; reset a to a'; release a', empty starting state, constant-false loop
stream and admissible successful callbacks, sufficient fuel. All typing,
realization and callback premises remain identical; fresh-binder injection
fails for the mutant. Also test a single true iteration followed by false to
exercise repeated freshness. Separately test that a live dependent heap blocks
release/reset at the runtime boundary (a runtime test, not a fake well-typed
instance with an impossible Ok premise).

Change Spec/Brand, Proofs/Brand and their examples/imports. This part depends
on the actual integrated Generative semantics and its erasure proof; do not
fill the missing dependency with another local brand machine.

## Evidence and implementation sequence

Every proposed signature above must become a fully elaborated, fully qualified
statement before implementation review. Schematic names such as ReportRelated
must have explicit definitions satisfying the constraints stated here, not
existential obligations that assume correspondence. The whole-program laws
must cover both directions at observation boundaries, successful/refused
operations, trap sites and inconclusive reports. If an interface cannot
express these obligations, revise the adapter design rather than weaken a law.

For each semantic law: name the fixed operation as subject; write a closed
Instance as a full application with all hypotheses; write a same-typed mutant
and a Planted example conjoining the **same** hypotheses at the **same**
arguments with the negated mutant conclusion. The concrete cases above are
construction requirements, not claimed compiled evidence. Follow
`_review/VACUITY_CONVENTION_FINAL.md` exactly when writing Lean; helpers and
representation lemmas receive their own single class tag and approval PENDING.
Do not promote definitional delegation, constructor transport or projections
to Laws. Keep correspondence premises structural and independently satisfiable;
never put correctness, successful rechecking or soundness inside realization.

Order of landable implementation slices:

1. Retire identities/integers (1), then scalar duplicates (2), preserving slice
   admission and migrating all users and existing proofs in each slice.
2. Part 3 follows 1–2. Part 4 follows 1–2 and can proceed independently of 3.
   Part 5's deletion follows 4's execution/site translation; its Core-only
   example migration can be prepared independently.
3. Integrate accepted Own/Generative foundations before 6–8. Part 7 depends on
   shared identities/scalars and part 4's traps. Part 8's brand translation is
   independent of range/text work once Generative is present. Part 6 depends
   on both ownership loan-release and generative dependencies; its Exec trace
   work can proceed earlier, but that alone cannot close it.
4. Recheck imports through Spec.lean/ConcreteSpec.lean and all named consumers;
   retire obsolete proof modules only after migrating their useful claims and
   examples. Preserve checker equivalence, progress/preservation, dependency
   safety, range/text validity and erasure claims. No duplicate compatibility
   implementation may remain at the end.

Each implementation slice needs the prescribed build, impact checks and tagged
non-vacuity evidence; later independent review is coordinator work. This
filing/design slice runs rules.sh and the mandated slotted ConcreteSpec build,
changes no Lean, runs no reviewers and writes no git notes. Closing ISS-0044
requires all eight parts retired or linked as above, not merely this note.

## QUESTIONS

No language answer is chosen by this design. Known decided constraints are
binding: use Core plain addition, Target byte order and existing empty-struct
support; preserve Core guards and diagnostic distinctions.

- **Nonempty aggregate storage:** implementing it would require a decided
  layout/padding/representation contract beyond Hybrid's current scalar scope
  (`Hybrid.lean:12-19`). Options for future owner scope: retain explicit
  unsupported rejection, or authorize a separate aggregate-layout extension.
  This design retains rejection and leaves the extension out; it does not
  choose a layout. The current struct encoder's accidental `[]` behavior is
  not a layout rule to preserve.
- **Whole-pointer representation:** Hybrid explicitly awaits its owner answer
  (`Hybrid.lean:18-19`). Options remain the owner's representation alternatives;
  this task does not select one or extend scalar adapters to pointers.

Foundation availability, exact adapter signatures, fuel accounting and source
site maps are implementation obligations, not new language questions. If the
integration versions reveal an unrecorded semantic disagreement (especially
activation loan release, reset lineage or cleanup trapping), stop that part,
quote the conflicting definitions/decision, and obtain an owner answer rather
than encode either side. No change to decisions.md or ROADMAP.md is proposed.
