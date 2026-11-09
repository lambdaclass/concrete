# ISS-0044: rebase the base models onto Core

Status: design for review, not an implementation or a language decision.
Baseline: `811ec53a1` (current base-migration lane). Owner: base-migration lane,
ROADMAP A2i (R-0571, R-0555). ISS-0044 remains open until the implementation
and its correspondence evidence land.

Evidence is the tracked definitions and proofs cited in each part below. The
structural cause is that the base models preceded Core and were never rebased
onto it. Follow `docs/PHILOSOPHY.md:69`: every fact has one owner.

## Reference discipline and scope

All numbered citations below refer to the current baseline `811ec53a1`.
Own and Grant are unavailable in this checkout. Parts 6–8 require their accepted
integration versions; names describing their interfaces below are requirements,
not claims that absent declarations have been checked here. Revalidate those
interfaces before implementation; do not implement replacement foundations.

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
| 4 | VIEW | Slice translation and diagnostic stuckness over Core execution |
| 5 | RETIRE | Core hosted observations, startup and statuses |
| 6 | VIEW | Static cleanup dependency analysis interpreted in Core execution |
| 7 | VIEW | Static ownership checking and range/text views over Core ownership |
| 8 | VIEW | Static brand checking and loop abstraction over Generative |

## 1. Variables and integer universe — RETIRE

Exact pairs: `ConcreteSpec/Spec/Slice1Syntax.lean:87-90` versus
`ConcreteSpec/Spec/Ident.lean:37-40` (Var); Slice1Syntax `:96-122` versus
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
This preserves acceptance: STy has only int/bool constructors
(`Slice1Syntax.lean:129-132`); StructDecl.fields stores STy
(`Slice1Syntax.lean:143-148`), and Ty.scalar stores STy
(`Slice1Syntax.lean:135-139`). Thus no current declaration or context can contain
unit, including an unused field. Require non-unit fields in DeclsOk, non-unit
scalar entries in context well-formedness, and non-unit scalar results in
AtomTy/HasType and atomTy/tyOf (including copy/move lookup). Preserve arbitrary
existing int/bool field declarations; add no validity restriction on their names
or usage. Prove embedding equivalence for all old declarations, contexts and
expressions, in both directions. The widened Core carrier is not new slice syntax.
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
Proofs/Rep `:13-27,55-72` overlaps Proofs/Memory `:22-25,36-44,56-64,94-95`
and Proofs/Hybrid `:18-28`. These are current scalar/memory/Hybrid interfaces.

Base adds declared-empty-struct representation and aggregate type selection.
Core adds provenance-bearing bytes and Target-dependent order. Delete the base
byte/radix/bit codecs and their independent proofs. New byte lists are lists of
Mem.MByte; scalar encoding/decoding delegates to Hybrid.encode/decode, hence
Mem.encodeInt/decodeInt and encodeBool/decodeBool. Every codec takes Target.

Use a partial encoder, including a declaration lookup:

```lean
encode : Target → Decls → Val → Option (List Mem.MByte)
decode : Target → Decls → Ty → List Mem.MByte → Option Val
-- Subjects encode, decode, Ty.size; law rep_roundtrip:
RepVal Δ v t →
 (encode tgt Δ v).map List.length = Ty.size Δ t ∧
 (encode tgt Δ v).bind (decode tgt Δ t) = some v
-- Subjects encode, decode; scalar_core:
v.Valid → v.ty ≠ .unit →
 encode tgt Δ (.scalar v) = some (Hybrid.encode tgt v) ∧
 decode tgt Δ (.scalar v.ty) (Hybrid.encode tgt v) = some (.scalar v)
-- Subject decode; scalar_decode_core (all bytes, including malformed bytes):
st ≠ .unit → decode tgt Δ (.scalar st) bs =
 (Hybrid.decode tgt st bs).map Val.scalar
-- Subject encode; encode_domain:
(encode tgt Δ v).isSome = decide (∃ t, RepVal Δ v t)
-- Subject decode; decode_valid:
match decode tgt Δ t bs with
| some v => RepVal Δ v t
| none => True
-- Subject encode; aggregate_refusal:
findDecl Δ s = some d → d.fields ≠ [] → encode tgt Δ (.struct s fs) = none
```

Ty.size explicitly returns none for scalar unit and some (Hybrid.sizeof st)
for non-unit scalars; its declared-empty-struct branch stays unchanged. Encoder
and decoder both refuse unit. Encode a struct only if its declaration exists,
its declared fields are empty and its value fields are empty; otherwise none.
Scalar encoding requires Valid and non-unit. Decode preserves Core byte checks.
No nonempty aggregate layout is chosen.

Witness requirements (each mutant has the exact full type of its subject):
- rep_roundtrip: big-endian linuxX8664, empty Δ, u16 258; encode mutant
  reverses its two bytes, decode mutant returns 513, size mutant returns 1.
  All three keep RepVal fixed and fail the corresponding conjunct.
- scalar_core: same u16 case; a *paired* little-endian encode/decode mutation
  still fails equality to the fixed Core bytes `[.init 1 none, .init 2 none]`.
  Test each subject separately as well. This prevents self-consistent wrong codecs.
- scalar_decode_core: big-endian u16 bytes `[.init 1 none, .init 2 none]`,
  mutant decodes little endian; also bool byte 2 and uninitialized input,
  mutant accepts them as false. Core is unchanged in every planted case.
- encode_domain: declared populated pair, mutant returns some []; also unit,
  unknown struct and malformed empty struct. Positive supported scalar/empty
  struct cases and a constant-none mutant test the opposite direction.
- decode_valid: bool byte 1; mutant returns an out-of-range u8 256 at the
  same arguments. The conclusion tests the candidate result, with no success
  premise mentioning decode. Add malformed-byte acceptance cases.
- aggregate_refusal: existing pair declaration (`Examples.lean:31`), populated
  value; constant some [] mutant preserves lookup/nonempty premises.

Round-trip proofs derive Core codec theorems, including byte length, rather
than unfold independent codecs. Add both endian orders and provenance cases.
Delegation equalities are Representation lemmas when definitional; the
substantive adapter laws additionally establish partial-domain and aggregate
selection facts. Do not call a scalar delegation equality soundness evidence.

Change Spec/Rep, Proofs/Rep and their examples in Spec/Examples and
Proofs/Examples. Add target cases in both orders, undef/provenance/invalid-bool
cases and empty-struct cases; retire old little-endian-only claims.

## 4. Results, outcomes and addition — VIEW

Exact pairs: Slice1Semantics `:33-60` / Outcome `:45-75` and
Exec `:243-256,578-589`; checkedAdd at Slice1Semantics `:107-108` /
Scalar `:194-219`. Base adds explanatory Stuck reasons and a small expression
interpreter; Core adds trap kinds, sites, configurations and more endings.
Arithmetic behavior already corresponds to plain, not checked.

Remove the slice Trap and independent R evaluator. Preserve Stuck reasons with
one Core operand instance and a local failure-classification relation. Lower
each atom read, mode check, field selection and arithmetic operation to a separate
A-normal operand boundary. The failing Core configuration retains its input
locals, ownership store and operand syntax. A diagnostic witness classifies that
single boundary: missing local gives unbound, moved slot gives movedRead,
incompatible copy/move gives wrongMode, wrong value shape gives typeError.
It does not execute any previous or subsequent operand or source statement.
Prove classification complete and unique for stuck states reachable from the
translation, and absence on successful/trapping boundaries. Successful moves
update Core instrumentation once. This respects Exec.Operands.eval's signature
(`Exec.lean:123-138`): Res.stuck carries no updated store; do not pretend a
failure can write a diagnostic field through that signature. The witness is
derived from the retained failing input configuration, not a parallel runner.
A source Outcome projection combines Core stuckness with this witness.
This diagnostic classifier inspects one failed operation only: it cannot step,
mutate state, select a source branch or produce a successful/trapping outcome. Core
itself remains payload-free. Erasure of failure witnesses is representation
evidence. No change to Scalar, Outcome or Exec is authorized.

Keep checked_never_stuck and checked_no_use_after_move at their existing
strength: the former excludes all Core stuck endings and the latter excludes
any failure witness of movedRead. Preserve refused examples' exact movedRead
and wrongMode observations, and add0_never_stuck. If the accepted Core interface
cannot carry the witness without another evaluator or a Core change, block this
part for the owner question below, rather than erase reasons provisionally.
Fuel exhaustion stays inconclusive. Rename checkedAdd to plainAdd, delegating
to Core.plain. Final outcomes project Core execution with explicit source site
maps and u8 return admission; no invented trap locations.

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

Translate syntax structurally into Core; retire readVar/evalAtom/evalFields/
evalExpr/destructure/execStmt/retOutcome/runBody/run as independent interpreters.
Compatibility names, if needed during migration, only project the one Core run.
Their delegation equations are Representation lemmas. Substantive laws test the
translation, whose function types are fixed before implementation:

```lean
lowerStmt : StmtInput → CoreFragment
lowerProgram : ProgramInput → CoreProgramInstance
-- stmt_simulation, subject lowerStmt:
TypedOwnershipConfig i →
 StmtContract i (BoundarySemantics (lowerStmt i))
-- program_simulation, subject lowerProgram:
TypedOwnershipConfig i.initial →
 ProgramContract i (BoundaryTraces (lowerProgram i))
```

StmtContract/ProgramContract are proof predicates on the *one Core execution*,
not source evaluation relations: constructor obligations require each source read
at its mapped boundary, each binding/destructure at the mapped local, each
successful move exactly once and each terminal outcome at its mapped return or
failing operation. Arithmetic requirements use Core.plain directly. They contain
no recursive source-run judgment or second evaluator, executable or relational.
BoundarySemantics/BoundaryTraces use Core Steps and terminal/infinite execution,
projected only at source boundaries. Require both coverage (every source action
has its mapped boundary unless an earlier terminal operation stops it) and
reflection (every projected Core action belongs to that source boundary),
environments, ownership changes, exact trap/site, diagnostic witness and status; internal stuttering is permitted, source progress
requires nonempty Core progress. Bounded projections distinguish exhausted
budgets from divergence; do not equate old recursion fuel to Core step counts.
TypedOwnershipConfig uses HasType, Uses and Corr with structural type/declaration
admission only, never admitted/StmtOk/BodyOk/Checked or no-trap assumptions.

Closed witnesses for both laws: empty declarations/environment, u8 2+3 bound
then returned, and u8 200+56 bound then returned. Both operands are in range;
the latter traps at its supplied addition site. Same-type lowerStmt/lowerProgram
mutants replace that addition with wrapping arithmetic or swap operands of
sequential move-dependent field evaluation. The overflow mutant succeeds and
violates each law with unchanged premises. Cover struct construction/destruction,
copy/move and early trap suppressing later statements. These contracts must not be defined as equality with the candidate translation
or by projecting an assumed translation-correctness certificate.
The existing evalExpr arithmetic law is retained as an adapter law, but its
implementation is a Core projection, never a retained interpreter.

Change Spec/{Slice1Semantics,Slice1Syntax,Slice1Checker,Slice0,Examples},
Proofs/{Slice1,Checker,Slice0,Examples,Observe}. checkedAdd definition/call are
Slice1Semantics `:107,119`; proof uses include Proofs/Slice1
`:394,396,446,578`. Part 5 is atomic with this slice: retire Observation and
migrate its consumers before removing Trap/Outcome dependencies.

## 5. Hosted observation — RETIRE

Exact pairs: Observation `:47-50` / Observe `:150-153` (status);
Observation `:54-61` / Observe `:176-177` (launch).
Current owners are Observe `:150-177`. Base adds no hosted behavior: it
explicitly omits diagnostics (`Observation.lean:28-33`). Core retains trap
kind, site, span, rendered line and event histories. The omission is disclosed,
not evidence of a false full-diagnostic theorem.

Delete Spec/Observation.lean and its import in Spec.lean. Migrate
Spec/Examples.lean:464-474, Proofs/Observe.lean and its consumers in
Proofs/Slice0.lean:1,153-157 and ConcreteSpec.lean to Core.Observe. Use actual site tables
and trapTerminal; no fabricated location, and no second status function.
Use part 4's execution embedding where examples originate as slice programs.
Bounded out-of-fuel/stuck results cannot be recast as hosted divergence.
No view needs to survive solely for status 125/134; test Core observations
including the distinction between normal return 134 and a trap.

## 6. Cleanup — VIEW

Exact current pairs: Cleanup `:48-77` / Exec `:172-212,468-472`
(deferred calls, frames, order); Cleanup `:80-131` / Exec `:333-413,499-511`
(unwinding/execution). Cleanup `:135-158` dependency admission must be
interpreted using the unavailable Own loan lifecycle and Generative dependency
interfaces, not another resource machine. Their exact integration signatures
are a prerequisite to implementation.
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

Execute only through Core Exec.plan/Cleans/runFrom. Delete RunCleanup and
its Boolean-oracle evaluator, not merely rename it to cleanupTrace. Retain
registration dependency summaries keyed by Core deferred-call identities.

```lean
cleanupCheck : CleanupInput → Bool
-- subject cleanupCheck; cleanup_sound:
RealizesMetadata i → CoreInitialWellFormed i →
 (cleanupCheck i = true → ∀ tr c',
   Exec.Steps i.program i.start tr c' → CleanupSafePrefix i tr c')
-- subject cleanupCheck; cleanup_refuses:
RealizesMetadata i → CoreInitialWellFormed i →
 UnsafeRegistration i → cleanupCheck i = false
```

The outer premises are fixed structural facts. RealizesMetadata certifies each
individual executable call's resources and callbacks, not global cleanup order.
CoreInitialWellFormed describes the state *before attempted registration*, so
unsafe proposed registrations are possible. UnsafeRegistration is an independent
witness of duplicate consumption, self dependency, unavailable resource or a
pending call needing the proposed target (including loan source). It is not
negated checker acceptance. CleanupSafePrefix uses Core plan and ownership
transitions: exact LIFO calls, loans live until call return then released on
activation exit, dependencies unconsumed until last use, no cleanup after trap.
For successful complete exits derive full plan/state equality via Cleans; for a
trapping call prove only its executed prefix. No second trace runner is needed.

Safe soundness instance: register allocator a then dependent v; both callbacks
return, Core executes v then a. The paired soundness Instance/Planted uses an
unsafe registration (the actual checker refuses and the implication holds):
register loan-holding v then consume its source b, with Core executing b before v.
The actual checker refuses; an always-true mutant makes the implication false,
with the same outer premises and a concrete unsafe Core prefix. Use that same
unsafe case as the refusal instance and planted always-true mutant. This follows
the convention: the implication is inside the conclusion, not an acceptance
hypothesis. Also give a closed safe accepted application of cleanup_sound to
show its safety branch is exercised. The current loan-source conflict is
Cleanup `:178-179`. A checker dropping only the pending loan-source guard must
fail at this case too. Metadata must include an executable source access after
consumption; arbitrary oracle Booleans cannot establish this instance.

Change Spec/Cleanup and Proofs/Cleanup; migrate their examples in
Spec/Examples and Proofs/Examples. Own/Generative integration is mandatory for
loan/dependency claims; Exec-only trace order is insufficient for closure.

## 7. Ownership, borrows and range views — VIEW

The base live-state/move definitions are Slice1Ownership `:19-63`;
whole-root locks/access/borrows are Borrow `:27-108`. Their counterparts are the
accepted integration Own slots/move, owner/loan access, borrow/reborrow and
activation-exit operations. That file is unavailable here; confirm its exact
interfaces and discharge the static-checker obligation when integrated. Current
range traps are Borrow `:151-162,427-479` versus Outcome `:45-51`.

Base adds static contexts, range facts, UTF-8 boundaries and range/text views;
Core adds runtime loan identities, parents, activation scope and permission
transitions. Static acceptance cannot be replaced by a runtime transition and
called soundness. Retain syntax-directed checking and unique range/text
abstractions. Context realization maps each live/moved variable, root, view,
loan parent and lexical scope to Core state. Whole-root permission queries use
Own, not copied lock arithmetic. Derive static transfer preservation for each
constructor, then executable-checker correspondence and reachable-step safety.

Keep the three source languages distinct: Checked concerns slice-1 Program;
Borrow.Ok concerns scoped Borrow.Stmt; RangeOk additionally checks RangeFacts
and both branches. Never use a borrow program as a Checked witness.

For each predicate K with its **existing full curried type**, require these
separate law schemas instantiated as checked_sound/refuses, borrow_sound/refuses
and range_sound/refuses (K is the named subject in each conclusion):

```lean
-- Structural i fixes all arguments of K, the initial state and source syntax.
Realizes i → (K i.args → AllCorePrefixesSafe i)
Realizes i → ForbiddenSourcePath i → ¬ K i.args
```

Here `K i.args` schematically denotes full application, not a new common checker.
For executable check, also retain check_iff, with check and Checked named as
subjects if classified as a Law. Realizes has no acceptance/permission premise.
AllCorePrefixesSafe states Core ownership permission for every source-attempted
action (including failed attempts), preservation of realization, and range/UTF-8
validity where facts promise it. ForbiddenSourcePath is a finite source control
path to an independently denied action or invalid unhandled range, derived from
syntax and truthful facts, never a second checker or negated K. Prove by static
transfer induction and Core reachable-state induction, including loop invariants.

Per-law closed cases and exact-type mutants:
- Checked: accepted add0/move-once program; refused double move of the linear
  empty struct in Examples `:95-106`. At the double move, a constant-True
  predicate mutant makes soundness false and refusal false with identical
  realization arguments. The existing checker refusal is the refusal instance;
  the soundness instance is the same refused program's implication plus a
  separate accepted move-once closed application to exercise the safety branch.
- Borrow.Ok: shared borrow/read/close is accepted; shared borrow containing
  owner write is refused (Proofs/Borrow `:219-227`). Mutant adds precisely that
  write admission; the independently denied Own action refutes both laws.
- RangeOk: source length 2, facts [0,3), no else, empty body/rest is refused;
  remove knownValid guard mutant. Fixed truthful facts imply invalidRange, so
  both laws fail for the mutant. Add accepted [0,2), invalid-with-else, and
  unknown-facts refusal instances. Keep facts realizable in both applications.
For soundness's unsafe instance the conclusion contains the K implication;
K is never moved into the law's outer hypotheses. Each refusal has its own
closed law application and planted example, not a reused positive-only test.

Replace vrun with translation plus projection of Core execution; no second
view interpreter. Required translation law:

```lean
lowerBorrow : BorrowProgramInput → CoreProgramInstance
-- borrow_program, subject lowerBorrow:
StructuralBorrowConfig i →
 BorrowProgramContract i (BoundaryTraces (lowerBorrow i))
```

BorrowProgramContract checks mapped Core boundaries using the single retained
range/UTF-8 rules and Core permission actions. It requires coverage and reflection
of body sequencing, restoration of outer views, else paths, whole memory and
output; no Ok/RangeOk premise. It is neither a recursive source evaluation
relation nor defined from lowerBorrow. Instance: memory [10,20], shared
range [0,3), else reads source index 0, then read source index 1: output [10,20].
Same-type mutant drops else or keeps the inner view alive after body; refute
with the same inputs (add a post-block inner-view access case for lifetime).
Other closed cases: [0,3) without else gives invalidRange; valid [0,2) then
index 2 gives outOfBounds. Same-type mutant conflates these traps. Prove frame
preservation and lexical restoration for nested bodies, not only one operation.
Retain text_view_utf8 and its split-boundary refused examples. Runtime permission
refusals are included without a static acceptance guard.

Change Spec/{Borrow,Slice1Ownership,Slice1Checker,Slice1Semantics},
Proofs/{Borrow,Slice1,Checker,Examples} and affected examples/imports. Do not
copy Own into this branch while its foundation is unavailable.

## 8. Allocator brands — VIEW

Base state/generation is Brand `:111-119`, release/reset is
Brand `:123-140,176-180`, ghost/erased runs are Brand `:162-243`.
The counterpart is the accepted integration Generative state/fresh/dependency/
release/reset/run/runErased interface. Grant is unavailable here; verify its
exact types and guard/erasure theorems before implementation.
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

There is no brandRun/coreBrandReport pair. Translate Brand.Stmt to Core
Generative commands, run only Generative.run/runErased, project their reports.
Retire State/Run/ghost/erased and the independent erasure induction.

```lean
lowerBrand : BrandInput → GenerativeCommandInstance
-- brand_runtime, subject lowerBrand:
BrandContextRealizes i → CoreWellFormed i → CallbacksAdmissible i →
 RuntimeBrandMeaning (Generative.run (lowerBrand i) fuel) i fuel
-- brand_sound, subject Brand.Ok (existing full predicate type):
BrandContextRealizes i → CoreWellFormed i → CallbacksAdmissible i →
 (Brand.Ok i.used i.context i.program i.finalContext →
  AllBrandPrefixesSafe (lowerBrand i))
-- brand_refuses, subject Brand.Ok:
BrandContextRealizes i → ForbiddenBrandPath i →
 ¬ Brand.Ok i.used i.context i.program i.finalContext
```

RuntimeBrandMeaning states guard refusal with live dependencies, fresh binder
injection after successful reset, no old handle use, exact callback allocation/
free behavior and erasure agreement from Core's theorem. It does not restate
lowerBrand or check the same invariant during translation. Context realization
contains scope, lineage, phantom dependencies and binder maps only. Runtime law
has no Ok premise, including at refused source programs. Quantify all loop-test
streams and prove at iteration boundaries, with Core step budget, not equal old
recursion fuel. Erasure projection equalities are Representation lemmas.

Runtime instance/planted: empty initial state, new a; allocate h; free h;
reset a to a'; release a', successful concrete callbacks, constant-false tests,
sufficient fixed fuel. Mutant of exact lowerBrand type reuses old binder at
reset: injection fails. Also use live h at reset and release: Core refuses;
a mutant translation consumes h silently before reset/release fails the law
because callback/guard behavior changes. These cases have no Ok premise.
Keep reset_fresh_brand/nodup/refuses (Proofs/Brand `:123-140`) as Core-derived
claims, not facts about a retained local counter. Add one-iteration-then-false
and repeated-reset cases.

AllBrandPrefixesSafe includes successful dependency guards, nominal handle
matching and fresh scopes for every test stream, not merely a callback report.
ForbiddenBrandPath is a finite source path to a live-heap reset/release or
mismatched allocator operation. Soundness unsafe instance: new a; allocate h;
reset a with h live (source is refused); mutant Brand.Ok drops NoHeap. It
falsifies the implication conclusion under unchanged structural premises.
Refusal instance uses the same source and mutant. Add separate accepted closed
soundness application of the allocate/free/reset/release program. Also give
soundness/refusal instances with heap brand 10 used through allocator brand 11,
and a same-type predicate mutant dropping nominal equality. Predicates are
mutated with their full four-argument types, not by changing realization.

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
construction requirements, not claimed compiled evidence. The self-contained convention is: every authored theorem gets exactly one
fully qualified doc-comment tag `Law (subject …).`, `Helper
(inversion|projection|congruence|arithmetic|computation).` or `Representation
lemma.` Helpers/representation lemmas state a reason and `(approved: PENDING)`;
the writer cannot approve them. Obligations are defs of Prop or structure fields,
never theorems. Each Law has `Instance (law L, case c)` on a full closed law
application, including all implicit arguments and hypothesis proofs, with its
exact instantiated type. `Planted (law L, case c, subject S, mutant M)` has the
same arguments and type: mutated hypotheses conjoined with the negated mutated
conclusion (only negation when no hypotheses). M must be a different definition
of S's exact type. Every name must resolve fully qualified. No impossible
hypothesis, runtime invariant recheck, returned hypothesis, projection, constructor
repackaging or definitional reflexivity counts as a Law. A law without a meaningful
mutable subject must be reformulated over an operation or reclassified. Existing
Accepted/Refused tags alone are not law evidence. All cases specified here are
construction requirements for later Lean evidence, not compiled examples.
Do not promote definitional delegation, constructor transport or projections
to Laws. Keep correspondence premises structural and independently satisfiable;
never put correctness, successful rechecking or soundness inside realization.

Order of landable implementation slices:

1. Retire identities/integers (1), then scalar duplicates (2), preserving slice
   admission and migrating all users and existing proofs in each slice.
2. Part 3 follows 1–2. Part 4 follows 1–2 and can proceed independently of 3.
   Parts 4–5 land atomically, including Slice0 and Observation consumers;
   Core-only observation examples can be prepared independently.
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

No language answer is chosen. Core plain addition, Target byte order, existing
empty-struct support, guards and diagnostic distinctions remain binding.

- **Reason-bearing stuck outcomes:** the design preserves existing reasons by
  classifying the retained failing Core operand boundary, without rerunning it.
  If integration cannot preserve this observation with the current Core interface,
  stop part 4. Owner options are authorize a Core diagnostic-witness interface,
  or retire reason-bearing slice outcomes and migrate refused examples to
  payload-free stuck (moving use-after-move specificity to ownership action
  soundness). Recommendation: preserve the reasons with a single-boundary
  witness; no reason erasure or Core change is authorized by this note.

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

## Review disposition

All ten findings were checked against the current tracked code; none is disputed.
1. **Fixed:** preserve all representation subjects/length, explicit Core codec
   agreement, domain and validity laws, and unit size rejection (part 3).
2. **Fixed:** exclude unit at declarations, contexts, declarative and executable
   typing; current STy has no unit, so no existing program is removed (part 2).
3. **Fixed:** translation subjects and declarative statement/program trace laws
   cover overflow without admitted/Checked guards (part 4).
4. **Fixed:** one instrumented Core evaluation carries failure witnesses; retain
   reason-specific claims. Any inability to carry them blocks implementation
   for the owner question, rather than choosing reason erasure (part 4).
5. **Fixed:** include Slice0, actual checkedAdd call, correct observation consumer;
   parts 4–5 are atomic.
6. **Fixed:** no cleanup runner; soundness implication and refusal conclusions
   test cleanupCheck, including pending-loan-source guard mutation (part 6).
7. **Fixed:** distinct Checked/Ok/RangeOk laws and witnesses, whole borrow-program
   translation with else/restoration/frame behavior (part 7).
8. **Fixed:** one Core brand runner, unguarded runtime law and separate static
   soundness/refusal subjects with NoHeap/equality mutants (part 8).
9. **Fixed:** tracked evidence only; non-vacuity convention is stated here.
10. **Fixed:** current baseline references replace integration line numbers;
    unavailable foundations are explicit dependencies without fabricated citations.
