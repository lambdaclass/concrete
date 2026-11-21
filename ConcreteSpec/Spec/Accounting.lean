import ConcreteSpec.Spec.Ident

/-!
# C7 logical-byte accounting

PROPOSED: not normative until the Core freeze. Authority:
docs/decisions.md#decided-2026-11-08-foundation (C7) and
docs/decisions.md#decided-project-memory-2026-11-06.

This projection records per-lineage flows and historical and function-local peaks. It does not
define execution, allocation layout or allocator footprint; the allocator supplies its own contract.
The C7 scope here is byte and memory accounting. The November 21 decision
(docs/decisions.md#decided-2026-11-21-secrets-after-1-0) defers security, leakage, release
and wiping until after 1.0 and closes ISS-0072.
-/

namespace ConcreteSpec.Core.Leak

/-! ## Logical byte accounting -/

/-- The live logical-byte accounting of one allocator lineage. `requested` is cumulative, `live` is
the current logical bytes, `peak` is the greatest live value reached by the whole execution, and
`functionPeak` is the greatest live value reached since the most recent `Account.enter`. -/
structure Account where
  requested : Nat
  live : Nat
  peak : Nat
  functionPeak : Nat
  deriving DecidableEq, Repr

/-- No allocation and no history. -/
def Account.empty : Account := ⟨0, 0, 0, 0⟩

/-- A well-formed account has already reached at least its current live bytes, both historically
and since function entry.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-2026-11-08-foundation (C7);
docs/decisions.md#decided-project-memory-2026-11-06. -/
def Account.WF (a : Account) : Prop := a.live ≤ a.peak ∧ a.live ≤ a.functionPeak

/-- Mark a function entry. This starts its local peak at the bytes already live on entry without
changing ownership, cumulative requests or the whole-execution historical peak. -/
def Account.enter (a : Account) : Account :=
  { a with functionPeak := a.live }

/-- Account for a successful request of `bytes`: cumulative requested and live bytes increase, and
both peaks move only if the new live value exceeds them. -/
def Account.alloc (a : Account) (bytes : Nat) : Account where
  requested := a.requested + bytes
  live := a.live + bytes
  peak := max a.peak (a.live + bytes)
  functionPeak := max a.functionPeak (a.live + bytes)

/-- Account for a successful free. Cumulative history and both peaks are conserved. The abstract
machine admits the action only when `bytes ≤ a.live`. -/
def Account.free (a : Account) (bytes : Nat) : Account :=
  { a with live := a.live - bytes }

/-- Account for a reset: all live bytes are released, while cumulative history and both peaks
remain. -/
def Account.reset (a : Account) : Account :=
  { a with live := 0 }

/-- An abstract C4 accounting action. A reset names only its lineage: the amount released is a fact
of the transition, not a caller-supplied field that the transition could be asked to confirm. -/
inductive Action where
  | alloc (lineage : LineageId) (bytes : Nat)
  | free (lineage : LineageId) (bytes : Nat)
  | reset (lineage : LineageId)
  deriving DecidableEq, Repr

/-- Abstract memory is only the accounting projection of C4 state. `Memory.empty` starts every
lineage empty; function-entry projections may already contain live allocations. -/
abbrev Memory := LineageId → Account

/-- Replace one lineage's account, leaving every other lineage unchanged. -/
def Memory.set (m : Memory) (lineage : LineageId) (a : Account) : Memory :=
  fun l => if l = lineage then a else m l

/-- The empty accounting projection. -/
def Memory.empty : Memory := fun _ => Account.empty

/-- Apply an action. Freeing more than the lineage's live bytes has no transition. A reset releases
exactly the lineage's current live bytes by setting that account's live field to zero. -/
def apply (m : Memory) : Action → Option Memory
  | .alloc lineage bytes => some (Memory.set m lineage ((m lineage).alloc bytes))
  | .free lineage bytes =>
      if bytes ≤ (m lineage).live then some (Memory.set m lineage ((m lineage).free bytes)) else none
  | .reset lineage => some (Memory.set m lineage (m lineage).reset)

/-- A checked view of the one accounting transition: allocation/reset succeed and free refuses
outflow above the actual live account.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-2026-11-08-foundation
(C7) and docs/decisions.md#decided-project-memory-2026-11-06. -/
def ActionPermitted (m : Memory) (a : Action) : Prop := (apply m a).isSome = true

/-- A successful sequence of abstract C4 accounting actions. Every step uses `apply`; refusal stops the sequence. -/
def run (m : Memory) (actions : List Action) : Option Memory :=
  match actions with
  | [] => some m
  | a :: rest => (apply m a).bind fun m' => run m' rest

/-- A checked view of the one accounting fold; a refused action refuses the sequence.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-2026-11-08-foundation (C7). -/
def SequencePermitted (m : Memory) (actions : List Action) : Prop := (run m actions).isSome = true

/-- Bytes allocated for `lineage` by a sequence. -/
def allocated (lineage : LineageId) : List Action → Nat
  | [] => 0
  | .alloc l bytes :: rest => (if l = lineage then bytes else 0) + allocated lineage rest
  | _ :: rest => allocated lineage rest

/-- Bytes explicitly freed for `lineage` by a sequence. -/
def freed (lineage : LineageId) : List Action → Nat
  | [] => 0
  | .free l bytes :: rest => (if l = lineage then bytes else 0) + freed lineage rest
  | _ :: rest => freed lineage rest

/-- The reset outflow of one action, read from the account immediately before that action. -/
def resetOutflow (lineage : LineageId) (m : Memory) : Action → Nat
  | .reset l => if l = lineage then (m l).live else 0
  | _ => 0

/-- Bytes released by resets of `lineage` while executing a sequence from `m`. Each outflow comes
from the actual intermediate state. An unsuccessful suffix contributes nothing. -/
def resetBytes (lineage : LineageId) (m : Memory) (actions : List Action) : Nat :=
  match actions with
  | [] => 0
  | action :: rest =>
      match apply m action with
      | none => 0
      | some m' => resetOutflow lineage m action + resetBytes lineage m' rest

/-- Independent conservation obligation for a reset. The recorded outflow is not an input to the
account transition, so a wrong record makes this predicate false rather than making the reset
inadmissible. Cumulative history and both peaks must be conserved.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-2026-11-08-foundation (C7);
docs/decisions.md#decided-project-memory-2026-11-06. -/
def ResetConservation (before after : Account) (recordedOutflow : Nat) : Prop :=
  after.requested = before.requested ∧
    after.live = 0 ∧ after.live + recordedOutflow = before.live ∧
    after.peak = before.peak ∧ after.functionPeak = before.functionPeak

/-- Independent maximum of live bytes at entry and every successful intermediate state.
This observer never reads either recorded peak field. A failed suffix has no successor state. -/
def observedPeak (lineage : LineageId) (m : Memory) (actions : List Action) : Nat :=
  match actions with
  | [] => (m lineage).live
  | action :: rest =>
      match apply m action with
      | none => (m lineage).live
      | some m' => max (m lineage).live (observedPeak lineage m' rest)

/-- The named allocator boundary defines footprint from the lineage, entry memory and actions.
No logical-byte theorem invents a footprint for an allocator that does not provide one. -/
structure AllocatorContract where
  footprint : LineageId → Memory → List Action → Option Nat

/-- Evidence for a reported bound. A bound is either established, with its stated limit, or not
established. The latter never turns an unknown into a reassuring numeric claim. -/
inductive BoundStatus where
  | boundEstablished
  | notEstablished
  deriving DecidableEq, Repr

/-- A reported bound and its independent evidence status. -/
structure Bound (α : Type) where
  limit : α
  status : BoundStatus
  deriving DecidableEq, Repr

/-- An established bound is sound for `actual`; `notEstablished` asserts no bound.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-2026-11-08-foundation (C7);
docs/decisions.md#decided-project-memory-2026-11-06. -/
def Bound.Sound {α : Type} [LE α] (actual : α) (b : Bound α) : Prop :=
  b.status = .boundEstablished → actual ≤ b.limit

/-- An established footprint bound needs an allocator-defined footprint; no contract-defined value
means that footprint evidence cannot be established.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-2026-11-08-foundation (C7);
docs/decisions.md#decided-project-memory-2026-11-06. -/
def Bound.SoundOptional {α : Type} [LE α] (actual : Option α) (b : Bound α) : Prop :=
  b.status = .boundEstablished → ∃ a, actual = some a ∧ a ≤ b.limit

/-- A signed interval for net live-byte change, with an independent evidence status.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-project-memory-2026-11-06, adopting MEMORY §1. -/
structure SignedInterval where
  lo : Int
  hi : Int
  status : BoundStatus
  deriving DecidableEq, Repr

/-- Both endpoints bound an established net change; unknown status asserts neither endpoint.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-project-memory-2026-11-06, adopting MEMORY §1. -/
def SignedInterval.Sound (actual : Int) (b : SignedInterval) : Prop :=
  b.status = .boundEstablished → b.lo ≤ actual ∧ actual ≤ b.hi

/-- Per-lineage facts for one function: bytes requested by that function, its net live-byte change,
its peak logical bytes since entry, and footprint when the allocator contract defines one. Each has
its own status. -/
structure LineageReport where
  lineage : LineageId
  requested : Bound Nat
  netChange : SignedInterval
  peak : Bound Nat
  footprint : Bound Nat

/-- Soundness of a lineage report against explicitly identified entry and exit accounts.
The identity must agree with the report; numeric bounds cannot be relabelled. Requested bytes exclude
requests made before entry. The peak is the maximum live bytes above the entry level, floored at zero.
The internal local high-water mark is measured in absolute live bytes. The caller applies `Account.enter`
at function entry
and does not reset the local peak again before the function exits.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-2026-11-08-foundation (C7);
docs/decisions.md#decided-project-memory-2026-11-06. -/
def LineageReport.Sound (lineage : LineageId) (entry exit : Account) (footprint : Option Nat)
    (r : LineageReport) : Prop :=
  r.lineage = lineage ∧
    Bound.Sound (exit.requested - entry.requested) r.requested ∧
    SignedInterval.Sound ((exit.live : Int) - (entry.live : Int)) r.netChange ∧
    Bound.Sound (exit.functionPeak - entry.live) r.peak ∧ Bound.SoundOptional footprint r.footprint

/-- Instance. Concrete execution check evaluating `run` on an allocate/free/reset sequence from `Memory.empty`.

Accepted: a valid allocate/free/reset sequence. Refused: freeing a byte not owned by the
lineage has no accounting transition. -/
theorem example_execution_check_evaluating_run_on_allocate : (run Memory.empty
    [.alloc ⟨0⟩ 8, .free ⟨0⟩ 3, .reset ⟨0⟩]).map (· ⟨0⟩) = some ⟨8, 0, 8, 8⟩ := by decide

/-- Instance. Concrete refusal check showing `apply` returns `none` when freeing more bytes than are live. -/
theorem example_refusal_check_showing_apply_returns_none : apply Memory.empty (.free ⟨0⟩ 1) = none := by decide


/-- A finite accounting projection. Later entries replace earlier ones; absent lineages are empty. -/
def accountMemory (entries : List (LineageId × Account)) : Memory :=
  entries.foldl (fun m e => Memory.set m e.1 e.2) Memory.empty

/-- Finite-data instance of the accounting transition judgment.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-2026-11-08-foundation (C7). -/
def FiniteActionPermitted (d : List (LineageId × Account) × Action) : Prop :=
  ActionPermitted (accountMemory d.1) d.2

/-- Finite-data instance of the accounting sequence judgment.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-2026-11-08-foundation (C7). -/
def FiniteSequencePermitted (d : List (LineageId × Account) × List Action) : Prop :=
  SequencePermitted (accountMemory d.1) d.2

/-- Natural-byte instance of the generic bound judgment; its ordering is fixed by the adapter.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-2026-11-08-foundation (C7). -/
def ByteBoundSound (d : Nat × Bound Nat) : Prop := Bound.Sound d.1 d.2

/-- Natural-byte instance of optional footprint evidence, retaining the same absence rule.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-2026-11-08-foundation (C7). -/
def ByteBoundSoundOptional (d : Option Nat × Bound Nat) : Prop := Bound.SoundOptional d.1 d.2

/-- Monomorphic data view of reset conservation; delegates to the owning judgment.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-2026-11-08-foundation (C7). -/
def ResetConserved (d : Account × Account × Nat) : Prop := ResetConservation d.1 d.2.1 d.2.2

/-- Monomorphic data view of lineage report soundness; delegates to the owning judgment.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-2026-11-08-foundation (C7). -/
def ReportSound (d : LineageId × Account × Account × Option Nat × LineageReport) : Prop :=
  let (lineage, entry, exit, footprint, report) := d
  LineageReport.Sound lineage entry exit footprint report

/-- Concrete signed-net interval evidence, delegated to its owning judgment.
Authority: R-0613 (C7 accounting); docs/decisions.md#decided-project-memory-2026-11-06. -/
def SignedNetSound (d : Int × SignedInterval) : Prop := SignedInterval.Sound d.1 d.2


end ConcreteSpec.Core.Leak
