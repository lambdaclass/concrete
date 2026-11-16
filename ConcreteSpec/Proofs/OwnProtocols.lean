import ConcreteSpec.Proofs.Own
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Own
import ConcreteSpec.Spec.Scalar

/-! # C5 scoped ownership protocols and loan histories (O3)

Authority: docs/decisions.md#decided-2026-11-08-foundation (C5), #decided-2026-11-05-core
(Q3.1–Q3.4, Q3.8), #od15/#od23 (whole-root exclusion), #od60/#od69
(explicit linear consumption), #decided-2026-11-08-recovered-d3 (downward lending,
call duration and no implicit ending), and #decided-2026-11-13-lifecycle-refusal.
The classification and control conventions are #decided-2026-11-13-proof-labels and
#decided-2026-11-14-trap-sites-and-law-tests. Only Laws count as evidence.

Source disposition: the O3 optional-result restatements of O1's owned-root, moved-root,
birth/supply, adoption, current-destination and passOwner results add no independent fact;
reuse their existing result helpers and preservation Laws. Likewise the instrument, life,
callLife, readOwner/writeOwner/readLoan/writeLoan erasure restatements remain owned by O2
and O1's result helpers. The generic instrument preservation wrapper remains O1's helper.
The registration Law reuses O1's defer_keeps_store_result inversion helper; its planted
control changes only the dispatcher's registration branch, evaluating arguments too early.
The source deferredStep alias and silentPrefix driver are retired: protocolOps, Exec.step and
Exec.run supply the actual execution and uniform lifecycle substitution. Prefix Laws instead
use an unbounded inductive certificate of actual machine steps and admitted ownership effects.
Source run-then-overwrite,
whole-function-refusal and whole-function-no-op mutants are replaced by one-rule body mutants.
All O3 declarations live here. O1's existing slot/loan uniqueness, fold projection,
entry-loan, leave inversion, ending-frame and lifecycle-subset Helpers are public so this
module can reuse their private dependencies without copying their proofs. Their statements
and proof ownership stay in O1; no O3 declaration remains beside a private helper.
No duplicate declaration or inherited APPR receipt is imported.

Held: ISS-0064/ISS-0065, ISS-0068–ISS-0073 and ISS-0076/ISS-0080 retain their existing scope
(source policy, constants, codecs, wiping, startup/ABI and aggregate representation).
No new language choice is made. References remain second-class: these protocols manipulate
loan identities, never stored or returned reference values (docs/PHILOSOPHY.md §§5–10).
-/
namespace ConcreteSpec.Core.Own

/-- A reborrow protocol permits a child in the current activation, refuses the caller as a
new destination, and restores the previous loan table when that child ends. The endpoint
comparison includes every pre-existing loan; a caller loan is never consumed implicitly. -/
def DownwardProtocol (lend : State → LoanId → List Projection → Mode → Nat → Option State)
    (s : State) (i : LoanId) (path : List Projection) (m : Mode) (current caller : Nat) : Prop :=
  lend s i path m caller = none ∧
    ∃ t, lend s i path m current = some t ∧
      endLoan t ⟨s.nextLoan⟩ = some { s with nextLoan := s.nextLoan + 1 } ∧
      endLoan t i = none ∧ accessLoan t ⟨s.nextLoan⟩ m = some t ∧
      accessLoan t i .mutable = none ∧
      (m = .mutable → accessLoan t i .shared = none)

/-- Helper (induction on birth order). Cached ancestors predate their descendant. -/
private theorem ancestor_older (s : State) (hs : WellFormed s) (a : Loan)
    (ha : a ∈ s.loans) (k : LoanId) (hk : k ∈ a.ancestors) : k.id < a.id.id := by
  generalize hn : a.id.id = n
  induction n using Nat.strongRecOn generalizing a with
  | ind n ih =>
    have hp := (hs.2.2.2.1 a ha).2.2.2.1
    unfold ParentValid at hp
    cases he : a.parent with
    | none => simp [he] at hp; simp [hp] at hk
    | some i =>
      rw [he] at hp
      obtain ⟨b, hb, _, _, older, chain, _, _⟩ := hp
      rw [chain] at hk
      rcases List.mem_cons.mp hk with rfl | hk
      · omega
      · have := ih b.id.id (by omega) b hb hk rfl
        omega

/-- Law (authority: docs/decisions.md#decided-2026-11-08-recovered-d3;
#decided-2026-11-05-core Q3.2–Q3.4; #od15; #od23). (subject `ConcreteSpec.Core.Own.reborrowTo`).
Downward lending creates a child in the current frame
and ends it without consuming any caller loan, while lending back to the caller is refused
(owner Q1, 2026-11-08). -/
theorem downward_reborrow_protocol (s : State) (i : LoanId) (path : List Projection)
    (m : Mode) (f : Activation) (caller : Nat) (a : Loan) (hs : WellFormed s)
    (hf : s.frames.head? = some f) (different : f.id ≠ caller)
    (found : loan s i = some a) (allowed : loanAccess s i m = true) :
    DownwardProtocol reborrowTo s i path m f.id caller := by
  have fm : f ∈ s.frames := List.mem_of_head? hf
  have issued := (hs.2.2.2.2.2.2.1 f fm).1
  have am : a ∈ s.loans := List.mem_of_find?_eq_some found
  have ai : a.id = i := by simpa using List.find?_some found
  have fresh : ∀ l ∈ s.loans, l.id ≠ LoanId.mk s.nextLoan := by
    intro l hl he
    have hb := (hs.2.2.2.1 l hl).1
    rw [he] at hb
    exact Nat.lt_irrefl _ hb
  have ine : i ≠ LoanId.mk s.nextLoan := by simpa [ai] using fresh a am
  have noChild : ∀ l ∈ s.loans, l.parent ≠ some (LoanId.mk s.nextLoan) := by
    intro l hl he
    have hp := (hs.2.2.2.1 l hl).2.2.2.1
    unfold ParentValid at hp
    rw [he] at hp
    obtain ⟨b, hb, hid, _⟩ := hp
    exact fresh b hb hid
  have children : s.loans.any (·.parent == some (LoanId.mk s.nextLoan)) = false := by
    apply Bool.eq_false_iff.mpr
    intro h
    obtain ⟨l, hl, he⟩ := List.any_eq_true.mp h
    exact noChild l hl (by simpa using he)
  have unchanged : s.loans.filter (·.id != LoanId.mk s.nextLoan) = s.loans := by
    apply List.filter_eq_self.mpr
    intro l hl
    simpa using fresh l hl
  have foundRaw := found
  unfold loan at foundRaw
  have permission := allowed
  simp only [loanAccess, found, Option.any_some, Bool.and_eq_true, Bool.not_eq_true',
    Bool.or_eq_true, beq_iff_eq] at permission
  have newNotAncestor : LoanId.mk s.nextLoan ∉ a.ancestors := by
    intro h
    have older := ancestor_older s hs a am _ h
    change s.nextLoan < a.id.id at older
    have := (hs.2.2.2.1 a am).1
    omega
  refine ⟨?_, ?_⟩
  · simp [reborrowTo, found, allowed, hf, different]
  · refine ⟨{ s with
        nextLoan := s.nextLoan + 1
        loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ path, m, some i,
          i :: a.ancestors, f.id, false, false⟩ :: s.loans }, ?_, ?_⟩
    · simp [reborrowTo, found, allowed, hf, issued]
    · refine ⟨?_, ?_, ?_, ?_, ?_⟩
      · simp [endLoan, loan, ine, children, unchanged]
      · simp [endLoan, loan]
      · simp only [accessLoan, loanAccess, loan, List.find?_cons]
        simp only [beq_self_eq_true, Option.any_some,
          Bool.not_false, Bool.true_and, List.all_cons]
        simp only [Bool.true_or]
        have hc : s.loans.all (fun b => b.id == LoanId.mk s.nextLoan ||
            decide (b.id ∈ i :: a.ancestors) || b.root != a.root ||
            (m == Mode.shared && b.mode == Mode.shared)) = true := by
          apply List.all_eq_true.mpr
          intro b hb
          have h := List.all_eq_true.mp permission.2 b hb
          simp only [Bool.or_eq_true, beq_iff_eq, decide_eq_true_eq, bne_iff_ne,
            Bool.and_eq_true] at h ⊢
          rcases h with ((h | h) | h) | h
          · exact Or.inl (Or.inl (Or.inr (by simp [h])))
          · exact Or.inl (Or.inl (Or.inr (List.mem_cons_of_mem _ h)))
          · exact Or.inl (Or.inr h)
          · exact Or.inr h
        cases m <;> simp_all
      · simp [accessLoan, loanAccess, loan, Ne.symm ine, foundRaw, newNotAncestor]
      · intro hm
        subst m
        simp [accessLoan, loanAccess, loan, Ne.symm ine, foundRaw, newNotAncestor]

namespace DownwardProtocolEvidence
open Examples

/-- The populated mutable child received by activation 1. -/
def source : Loan := ⟨⟨1⟩, ⟨0⟩, [.index 0], .mutable, some ⟨0⟩, [⟨0⟩], 1, false, false⟩
/-- Current callee frame, above caller activation 0. -/
def calleeFrame : Activation := ⟨1, [], [(y, ⟨1⟩)]⟩

/-- Mutant: retain ordinary permissions, but allow a new child to target the active caller. -/
def allowUpward (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, m, some i,
        i :: a.ancestors, activation, false, false⟩ :: s.loans }
  else none

/-- Instance (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`). Current activation 1 refuses caller activation 0. -/
example : DownwardProtocol reborrowTo LifecycleWitnesses.called ⟨1⟩ [] .mutable 1 0 :=
  @downward_reborrow_protocol LifecycleWitnesses.called ⟨1⟩ [] .mutable calleeFrame 0 source
    (by decide) rfl (by decide) rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant
`ConcreteSpec.Core.Own.DownwardProtocolEvidence.allowUpward`). The exact upward request is now admitted. -/
example : WellFormed LifecycleWitnesses.called ∧
    LifecycleWitnesses.called.frames.head? = some calleeFrame ∧ calleeFrame.id ≠ 0 ∧
    loan LifecycleWitnesses.called ⟨1⟩ = some source ∧
    loanAccess LifecycleWitnesses.called ⟨1⟩ .mutable = true ∧
    ¬ DownwardProtocol allowUpward LifecycleWitnesses.called ⟨1⟩ [] .mutable calleeFrame.id 0 := by
  refine ⟨by decide, rfl, by decide, rfl, by decide, ?_⟩
  intro h
  have hn := h.1
  cases hn

/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`). Reuse O1's body mutant
that removes only the caller's parent when creating a child. Exact restoration detects the loss. -/
example : WellFormed LifecycleWitnesses.called ∧
    LifecycleWitnesses.called.frames.head? = some calleeFrame ∧
    calleeFrame.id ≠ 0 ∧
    loan LifecycleWitnesses.called ⟨1⟩ = some source ∧
    loanAccess LifecycleWitnesses.called ⟨1⟩ .mutable = true ∧
    ¬ DownwardProtocol PreservationEvidence.reborrowToDropParent
      LifecycleWitnesses.called ⟨1⟩ [] .mutable calleeFrame.id 0 := by
  refine ⟨by decide, rfl, by decide, rfl, by decide, ?_⟩
  rintro ⟨_, t, ht, he, _⟩
  cases ht
  revert he
  decide

/-- Instance (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`). The actual child
can be accessed while its parent is suspended; ending it restores that parent's access. -/
example : ∃ t u,
    reborrowTo LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 = some t ∧
    accessLoan t ⟨2⟩ .mutable = some t ∧
    accessLoan t ⟨1⟩ .mutable = none ∧
    endLoan t ⟨2⟩ = some u ∧
    accessLoan u ⟨1⟩ .mutable = some u := by
  obtain ⟨_, t, ht, he, _, usable, suspended, _⟩ :=
    @downward_reborrow_protocol LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable
      calleeFrame 0 source (by decide) rfl (by decide) rfl (by decide)
  exact ⟨t, _, ht, usable, suspended, he, by decide⟩


def sharedUp (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      (m = .shared ∨ s.frames.head?.map Activation.id = some activation)) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, m, some i,
        i :: a.ancestors, activation, false, false⟩ :: s.loans }
  else none

def deadChild (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, m, some i,
        i :: a.ancestors, activation, true, false⟩ :: s.loans }
  else none


/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`,
mutant `PreservationEvidence.reborrowParentlessTo`). A populated request isolates the missing protocol clause. -/
example : ¬ DownwardProtocol PreservationEvidence.reborrowParentlessTo LifecycleWitnesses.called ⟨1⟩ [] .mutable 1 0 := by
  rintro ⟨up, t, ht, rest⟩
  cases ht
  have bad := rest.2.1
  cases bad
/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`,
mutant `sharedUp`). A populated request isolates the missing protocol clause. -/
example : ¬ DownwardProtocol sharedUp LifecycleWitnesses.called ⟨1⟩ [] .shared 1 0 := by
  rintro ⟨up, t, ht, rest⟩
  cases up
/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`,
mutant `deadChild`). A populated request isolates the missing protocol clause. -/
example : ¬ DownwardProtocol deadChild LifecycleWitnesses.called ⟨1⟩ [] .mutable 1 0 := by
  rintro ⟨up, t, ht, rest⟩
  cases ht
  have bad := rest.2.2.1
  cases bad
/-- Instance (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`). Shared lending has the same downward boundary. -/
example : DownwardProtocol reborrowTo LifecycleWitnesses.called ⟨1⟩ [] .shared 1 0 :=
  @downward_reborrow_protocol LifecycleWitnesses.called ⟨1⟩ [] .shared calleeFrame 0 source
    (by decide) rfl (by decide) rfl (by decide)
end DownwardProtocolEvidence


/-- Linear consumption denies both owner accesses until reinitialization restores the exact
original state. Unrelated owners, loans and activation indices survive the whole protocol. -/
def MoveLifecycle (consume replace : State → Hybrid.Place → Option State) (s : State) (p : Hybrid.Place) : Prop :=
  ∃ t, consume s p = some t ∧ accessOwner t p .shared = none ∧
    accessOwner t p .mutable = none ∧ consume t p = none ∧
    replace s p = none ∧ replace t p = some s

/-- Law (authority: docs/decisions.md#decided-2026-11-08-foundation, C5;
#decided-2026-11-05-core Q3.8; #od15; #od60; #od69). (subject `ConcreteSpec.Core.Own.move`).
A discharged linear owner is unusable between consumption and reinitialization, and replacement restores all
previous state. -/
theorem move_reinitialize_protocol (s : State) (p : Hybrid.Place) (x : Slot)
    (hs : WellFormed s) (found : slot s p = some x) (linear : x.desc.copy = false)
    (owned : x.status = .owned) (clear : ∀ l ∈ s.loans, l.root ≠ p) :
    MoveLifecycle move reinitialize s p := by
  have xp : x.place = p := by simpa using List.find?_some found
  have xm : x ∈ s.slots := List.mem_of_find?_eq_some found
  let change := fun a : Slot => if a.place = p then { a with status := .moved } else a
  have place (a : Slot) : (change a).place = a.place := by simp [change]; split <;> rfl
  let t : State := { s with slots := s.slots.map change }
  have sf : slot t p = some { x with status := .moved } := by
    unfold slot at found ⊢
    simp only [t, List.find?_map, Function.comp_def, place, found, Option.map_some]
    simp [change, xp]
  have clearBool : s.loans.any (·.root == p) = false := by
    apply Bool.eq_false_iff.mpr
    intro h
    obtain ⟨l, hl, he⟩ := List.any_eq_true.mp h
    exact clear l hl (by simpa using he)
  have allClear : s.loans.all (·.root != p) = true := by
    apply List.all_eq_true.mpr
    intro l hl
    simpa using clear l hl
  have restoration :
      (s.slots.map change).map (fun a => if a.place = p then { a with status := .owned } else a) = s.slots := by
    rw [List.map_map]
    calc
      _ = s.slots.map id := by
        apply List.map_congr_left
        intro a ha
        dsimp only [Function.comp_def]
        rw [place]
        by_cases hp : a.place = p
        · have ax := slot_unique s.slots hs.1 a x ha xm (hp.trans xp.symm)
          subst a
          simp only [change, xp, ↓reduceIte]
          rw [← xp]
          cases x
          cases owned
          rfl
        · simp [change, hp]
      _ = s.slots := List.map_id _
  refine ⟨t, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · simp [move, found, linear, owned, clearBool, t, change]
  · simp [accessOwner, ownerAccess, sf]
  · simp [accessOwner, ownerAccess, sf]
  · simp [move, sf, linear]
  · simp [reinitialize, found, owned, linear]
  · simp [reinitialize, sf, t, allClear, restoration]

namespace MoveLifecycleEvidence
open Examples DirectArgumentWitnesses

/-- Two owned locals and an unrelated live mutable loan. -/
def source : State := caller.own
/-- The second local is linear, owned and unborrowed. -/
def selected : Slot := ⟨⟨1⟩, 0, ⟨.int .i32, false, true⟩, .owned⟩

/-- Mutant: consumption falsely reports success while retaining the owned value. -/
def keepOwned (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if x.desc.copy || x.status != .owned || s.loans.any (·.root == p) then none else
    some { s with slots := s.slots.map fun x =>
      if x.place = p then { x with status := .owned } else x }

/-- Instance (law `ConcreteSpec.Core.Own.move_reinitialize_protocol`, case
`ConcreteSpec.Core.Own.MoveLifecycleEvidence.source`). -/
example : MoveLifecycle move reinitialize source ⟨1⟩ :=
  @move_reinitialize_protocol source ⟨1⟩ selected (by decide) rfl rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.move_reinitialize_protocol`, case
`ConcreteSpec.Core.Own.MoveLifecycleEvidence.source`, subject `ConcreteSpec.Core.Own.move`, mutant
`ConcreteSpec.Core.Own.MoveLifecycleEvidence.keepOwned`). -/
example : WellFormed source ∧ slot source ⟨1⟩ = some selected ∧ selected.desc.copy = false ∧
    selected.status = .owned ∧
    (∀ l ∈ source.loans, l.root ≠ Hybrid.Place.mk 1) ∧
    ¬ MoveLifecycle keepOwned reinitialize source ⟨1⟩ := by
  refine ⟨by decide, rfl, rfl, rfl, by decide, ?_⟩
  rintro ⟨t, ht, denied, _⟩
  cases ht
  cases denied


def doubleMove (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if x.desc.copy || s.loans.any (·.root == p) then none else
    some { s with slots := s.slots.map fun x =>
      if x.place = p then { x with status := .moved } else x }

def overwrite (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if (x.status == .moved || x.desc.mutable) &&
      s.loans.all (·.root != p) then
    some { s with slots := s.slots.map fun x =>
      if x.place = p then { x with status := .owned } else x }
  else none

/-- Planted (law `ConcreteSpec.Core.Own.move_reinitialize_protocol`, case
`ConcreteSpec.Core.Own.MoveLifecycleEvidence.source`, subject `ConcreteSpec.Core.Own.move`,
mutant `ConcreteSpec.Core.Own.MoveLifecycleEvidence.doubleMove`).
An added transition violates linearity even though ordinary post-move access is refused. -/
example : ¬ MoveLifecycle doubleMove reinitialize source ⟨1⟩ := by
  rintro ⟨t, ht, rest⟩
  cases ht
  have bad := rest.2.2.1
  cases bad
/-- Planted (law `ConcreteSpec.Core.Own.move_reinitialize_protocol`, case
`ConcreteSpec.Core.Own.MoveLifecycleEvidence.source`, subject `ConcreteSpec.Core.Own.reinitialize`,
mutant `ConcreteSpec.Core.Own.MoveLifecycleEvidence.overwrite`).
An added transition violates linearity even though ordinary post-move access is refused. -/
example : ¬ MoveLifecycle move overwrite source ⟨1⟩ := by
  rintro ⟨t, ht, rest⟩
  cases ht
  have bad := rest.2.2.2.1
  cases bad
end MoveLifecycleEvidence

/-- Law (authority: docs/decisions.md#decided-2026-11-08-foundation, C5;
#od15, loan-derived validity ends with its loan; #decided-2026-11-05-core Q3.2–Q3.3).
(subject `ConcreteSpec.Core.Own.loanAction`). Every
single loan-table transition
(`borrow`, `reborrowTo`, or `endLoan`) preserves `RetiredLoan` for any already retired
loan identity. -/
theorem loanSequence_retired_step (s : State) (op : LoanAction) (i : LoanId)
    (hr : RetiredLoan s i) :
    ResultSatisfies (fun t => RetiredLoan t i) (loanAction s op) := by
  cases h : loanAction s op with
  | none => trivial
  | some t =>
    have bound := hr.1
    cases op with
    | borrow p path mode =>
      change borrow s p path mode = some t at h
      unfold borrow at h
      obtain ⟨_, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h
        refine ⟨Nat.lt_succ_of_lt bound, ?_⟩
        intro l hl
        rcases List.mem_cons.mp hl with rfl | hl
        · intro he
          have hid := congrArg LoanId.id he
          dsimp at hid
          omega
        · exact hr.2 l hl
      · contradiction
    | reborrow i' path mode activation =>
      change reborrowTo s i' path mode activation = some t at h
      unfold reborrowTo at h
      obtain ⟨_, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h
        refine ⟨Nat.lt_succ_of_lt bound, ?_⟩
        intro l hl
        rcases List.mem_cons.mp hl with rfl | hl
        · intro he
          have hid := congrArg LoanId.id he
          dsimp at hid
          omega
        · exact hr.2 l hl
      · contradiction
    | endLoan i' =>
      change endLoan s i' = some t at h
      unfold endLoan at h
      obtain ⟨_, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · contradiction
      · cases h
        exact ⟨bound, fun l hl => hr.2 l (List.mem_filter.mp hl).1⟩

/-- Law (authority: docs/decisions.md#decided-2026-11-08-foundation, C5;
#od15, loan-derived validity ends with its loan; #decided-2026-11-05-core Q3.2–Q3.3).
(subject `ConcreteSpec.Core.Own.runLoanSequence`).
An ended loan identity cannot
reappear through any finite sequence of owner borrowing, projected reborrowing and ending.
The original roots may remain live and be borrowed again; their new loans have new identities. -/
theorem ended_loans_never_revive (s : State) (ops : List LoanAction) (i : LoanId)
    (hr : RetiredLoan s i) :
    ResultSatisfies (fun t => RetiredLoan t i) (runLoanSequence s ops) := by
  cases h : runLoanSequence s ops with
  | none => trivial
  | some t =>
    unfold runLoanSequence at h
    induction ops generalizing s with
    | nil => simp at h; subst t; exact hr
    | cons op ops ih =>
      simp only [List.foldlM_cons] at h
      obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
      have hstep := loanSequence_retired_step s op i hr
      rw [hu] at hstep
      exact ih u hstep h

namespace LoanHistoryEvidence
open Examples

/-- A retired child, its live parent, and an unrelated owned root for an eventful history. -/
def endedChild : State := { (endLoan child ⟨1⟩).getD initial with
  nextPlace := 2
  slots := (DirectArgumentWitnesses.caller.own.slots.filter (·.place == ⟨1⟩)) ++ initial.slots
  frames := [⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩] }
/-- Open another root, end it, then create a fresh projected child of the retained parent. -/
def actions : List LoanAction :=
  [.borrow ⟨1⟩ [] .shared, .endLoan ⟨2⟩,
    .reborrow ⟨0⟩ [.index 3] .mutable 0]

/-- A body mutation issues identity 1 instead of the fresh counter, retaining every guard,
projection, parent, ancestor, destination and supply update. -/
def recycleReborrow (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨1⟩, a.root, a.path ++ suffix, m, some i,
        i :: a.ancestors, activation, false, false⟩ :: s.loans }
  else none

/-- Substitute the changed constructor in the public action dispatcher. -/
def recycleStep (s : State) : LoanAction → Option State
  | .borrow p path m => borrow s p path m
  | .reborrow i path m a => recycleReborrow s i path m a
  | .endLoan i => endLoan s i

/-- Substitute the same action at every transition of the existing monadic history fold. -/
def recycle (s : State) (ops : List LoanAction) : Option State :=
  ops.foldlM recycleStep s

/-- Instance (law `ConcreteSpec.Core.Own.loanSequence_retired_step`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`). -/
example : ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
    (loanAction endedChild (.reborrow ⟨0⟩ [.index 2] .mutable 0)) :=
  @loanSequence_retired_step endedChild (.reborrow ⟨0⟩ [.index 2] .mutable 0) ⟨1⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.loanSequence_retired_step`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.loanAction`, mutant
`ConcreteSpec.Core.Own.LoanHistoryEvidence.recycleStep`). -/
example : RetiredLoan endedChild ⟨1⟩ ∧
    ¬ ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
      (recycleStep endedChild (.reborrow ⟨0⟩ [.index 2] .mutable 0)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.ended_loans_never_revive`). The populated state is
well formed; the history succeeds with a live parent and retires its intermediate loan. -/
example : WellFormed endedChild ∧
    (runLoanSequence endedChild actions).isSome = true ∧
    (loan endedChild ⟨0⟩).isSome = true ∧
    ResultSatisfies (fun t => RetiredLoan t ⟨1⟩ ∧ RetiredLoan t ⟨2⟩ ∧
      (loan t ⟨3⟩).isSome = true) (runLoanSequence endedChild actions) := by
  refine ⟨by decide, by decide, by decide, ?_⟩
  exact ⟨ended_loans_never_revive endedChild actions ⟨1⟩ (by decide),
    by decide, by decide⟩

/-- Instance (law `ConcreteSpec.Core.Own.ended_loans_never_revive`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`). -/
example : ResultSatisfies (fun t => RetiredLoan t ⟨1⟩) (runLoanSequence endedChild actions) :=
  @ended_loans_never_revive endedChild actions ⟨1⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.ended_loans_never_revive`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.runLoanSequence`, mutant
`ConcreteSpec.Core.Own.LoanHistoryEvidence.recycle`). -/
example : RetiredLoan endedChild ⟨1⟩ ∧
    ¬ ResultSatisfies (fun t => RetiredLoan t ⟨1⟩) (recycle endedChild actions) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.loanSequence_retired_step`). Both other action arms
succeed on real loans; retirement survives a birth on another root and ending the live parent. -/
example : RetiredLoan endedChild ⟨1⟩ ∧
    (loanAction endedChild (.borrow ⟨1⟩ [] .shared)).isSome = true ∧
    (loanAction endedChild (.endLoan ⟨0⟩)).isSome = true ∧
    ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
      (loanAction endedChild (.borrow ⟨1⟩ [] .shared)) ∧
    ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
      (loanAction endedChild (.endLoan ⟨0⟩)) := by
  exact ⟨by decide, by decide, by decide,
    loanSequence_retired_step endedChild (.borrow ⟨1⟩ [] .shared) ⟨1⟩ (by decide),
    loanSequence_retired_step endedChild (.endLoan ⟨0⟩) ⟨1⟩ (by decide)⟩

/-- Change only the new owner's loan identity, leaving its supply advance and permissions intact. -/
def recycleBorrow (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode) :
    Option State := do
  let f ← s.frames.head?
  if ownerAccess s p m && decide (f.id < s.nextActivation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨1⟩, p, path, m, none, [], f.id, false, false⟩ :: s.loans }
  else none

/-- Uniformly substitute the borrowed identity mutation in the action dispatcher. -/
def recycleBorrowStep (s : State) : LoanAction → Option State
  | .borrow p path m => recycleBorrow s p path m
  | .reborrow i path m a => reborrowTo s i path m a
  | .endLoan i => endLoan s i

/-- The same fold with the substituted action, including every earlier history transition. -/
def recycleBorrowHistory (s : State) (ops : List LoanAction) : Option State :=
  ops.foldlM recycleBorrowStep s

/-- End a fresh child before opening an independent owner loan. -/
def borrowActions : List LoanAction :=
  [.reborrow ⟨0⟩ [.index 2] .mutable 0, .endLoan ⟨2⟩, .borrow ⟨1⟩ [] .shared]

/-- Planted (law `ConcreteSpec.Core.Own.loanSequence_retired_step`). The retired identity is
reissued by an owner borrow, independently of the projected-reborrow mutation. -/
example : RetiredLoan endedChild ⟨1⟩ ∧
    ¬ ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
      (recycleBorrowStep endedChild (.borrow ⟨1⟩ [] .shared)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.ended_loans_never_revive`). Both kinds of birth
remain fresh after an intermediate child ends. -/
example : (runLoanSequence endedChild borrowActions).isSome = true ∧
    ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
      (runLoanSequence endedChild borrowActions) := by
  exact ⟨by decide, ended_loans_never_revive endedChild borrowActions ⟨1⟩ (by decide)⟩

/-- Planted (law `ConcreteSpec.Core.Own.ended_loans_never_revive`). The borrow mutation is
substituted in every action, and the successful multi-action history revives identity 1. -/
example : RetiredLoan endedChild ⟨1⟩ ∧
    (recycleBorrowHistory endedChild borrowActions).isSome = true ∧
    ¬ ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
      (recycleBorrowHistory endedChild borrowActions) := by decide

/-- Change only the ending operation's supply retention, resetting it after removing the loan. -/
def rewindEnd (s : State) (i : LoanId) : Option State := do
  let _ ← loan s i
  if s.loans.any (·.parent == some i) then none else
    some { s with nextLoan := 0, loans := s.loans.filter (·.id != i) }

/-- Substitute the changed ending arm; the other loan actions are unchanged. -/
def rewindStep (s : State) : LoanAction → Option State
  | .borrow p path m => borrow s p path m
  | .reborrow i path m a => reborrowTo s i path m a
  | .endLoan i => rewindEnd s i

/-- Planted (law `ConcreteSpec.Core.Own.loanSequence_retired_step`). Ending a real parent
must retain the fact that the retired child's identity was already issued. -/
example : RetiredLoan endedChild ⟨1⟩ ∧
    ¬ ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
      (rewindStep endedChild (.endLoan ⟨0⟩)) := by decide

end LoanHistoryEvidence

/-- A place identity was issued and its storage has been retired. This is independent of
whether another slot happens to carry equal concrete bytes. -/
def RetiredPlace (s : State) (p : Hybrid.Place) : Prop :=
  p.id < s.nextPlace ∧ ∀ x ∈ s.slots, x.place ≠ p
instance (s : State) (p : Hybrid.Place) : Decidable (RetiredPlace s p) := by
  unfold RetiredPlace
  infer_instance

/-- Law (authority: docs/decisions.md#foundation-q1 C5; #decided-2026-11-05-core Q3.3;
#od69). (subject `ConcreteSpec.Core.Own.bind`). Rebinding allocates a new identity or
changes an existing slot's status; neither branch revives retired storage. -/
theorem bind_retired_place (types : Var → Option Descriptor) (s : State) (x : Var)
    (p : Hybrid.Place) (hr : RetiredPlace s p) :
    ResultSatisfies (fun t => RetiredPlace t p) (bind types x s) := by
  have bound := hr.1
  cases h : bind types x s with
  | none => trivial
  | some t =>
    unfold bind at h
    obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · unfold reinitialize at h
      obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h
        refine ⟨hr.1, ?_⟩
        intro a ha
        obtain ⟨b, hb, rfl⟩ := List.mem_map.mp ha
        split <;> exact hr.2 b hb
      · contradiction
    · obtain ⟨d, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h
        refine ⟨Nat.lt_succ_of_lt hr.1, ?_⟩
        intro a ha
        rcases List.mem_cons.mp ha with rfl | ha
        · intro he
          have := congrArg Hybrid.Place.id he
          dsimp at this
          omega
        · exact hr.2 a ha
      · contradiction

/-- Law (authority: docs/decisions.md#foundation-q1 C5; #od69).
(subject `ConcreteSpec.Core.Own.drop`). Storage teardown retains the issuance history,
including identities no longer represented by any live slot. -/
theorem drop_retired_place (s : State) (xs : List Var) (p : Hybrid.Place)
    (hr : RetiredPlace s p) : ResultSatisfies (fun t => RetiredPlace t p) (drop xs s) := by
  cases h : drop xs s with
  | none => trivial
  | some t =>
    unfold drop at h
    obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
    dsimp at h
    split at h
    · cases h
      exact ⟨hr.1, fun a ha => hr.2 a (List.mem_filter.mp ha).1⟩
    · contradiction

/-- Law (authority: docs/decisions.md#foundation-q1 C5; #od15;
#decided-2026-11-08-recovered-d3, call-duration loans).
(subject `ConcreteSpec.Core.Own.passOwner`). Call-issued loans use the same fresh supply
as ordinary loans; an ended identity cannot be issued again by direct argument evaluation. -/
theorem call_birth_retired (s : CallState) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (x : Var) (i : LoanId) (hr : RetiredLoan s.own i) :
    ResultSatisfies (fun t => RetiredLoan t.own i) (passOwner s p path m x) := by
  have bound := hr.1
  cases h : passOwner s p path m x with
  | none => trivial
  | some t =>
    unfold passOwner at h
    split at h
    · contradiction
    · obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h
        refine ⟨Nat.lt_succ_of_lt hr.1, ?_⟩
        intro a ha
        rcases List.mem_cons.mp ha with rfl | ha
        · intro he
          have := congrArg LoanId.id he
          dsimp at this
          omega
        · exact hr.2 a ha
      · contradiction

namespace RetirementEvidence
open Examples

/-- A successful teardown has already retired place zero. -/
def retired : State := (drop [x] LifecycleWitnesses.moved).getD initial

/-- Mutant: only the teardown supply is rewound. A subsequent bind can recycle place zero. -/
def rewindDrop (xs : List Var) (s : State) : Option State := do
  let f ← s.frames.head?
  let ps := List.map Prod.snd (f.locals.filter fun e => decide (e.1 ∈ xs))
  if s.slots.all (fun x => decide (x.place ∉ ps) || x.desc.copy || x.status == .moved) &&
      s.loans.all (fun l => decide (l.root ∉ ps)) then
    some { s with
      nextPlace := 0
      slots := s.slots.filter (fun x => decide (x.place ∉ ps))
      frames := { f with locals := f.locals.filter (fun e => decide (e.2 ∉ ps)) } :: s.frames.tail }
  else none

/-- Mutant: a newly allocated slot reuses zero, leaving the admission guards unchanged. -/
def recycleBind (types : Var → Option Descriptor) (x : Var) (s : State) : Option State := do
  let f ← s.frames.head?
  match AList.lookup f.locals x with
  | some p => reinitialize s p
  | none =>
    let d ← types x
    if f.id < s.nextActivation then some { s with
      nextPlace := s.nextPlace + 1
      slots := ⟨⟨0⟩, f.id, d, .owned⟩ :: s.slots
      frames := { f with locals := (x, ⟨0⟩) :: f.locals } :: s.frames.tail }
    else none

/-- Mutant: direct argument evaluation reissues loan one, including its pending index.
No other constructor field or guard changes. -/
def recyclePass (s : CallState) (root : Hybrid.Place) (path : List Projection)
    (mode : Mode) (parameter : Var) : Option CallState := do
  if parameter ∈ s.pending.map Prod.fst then none else do
  let f ← s.own.frames.head?
  if ownerAccess s.own root mode && decide (f.id < s.own.nextActivation) then
    pure ⟨{ s.own with
      nextLoan := s.own.nextLoan + 1
      loans := ⟨⟨1⟩, root, path, mode, none, [], f.id, true, true⟩ :: s.own.loans },
      s.pending ++ [(parameter, ⟨1⟩)]⟩
  else none

/-- Instance (law `ConcreteSpec.Core.Own.bind_retired_place`, case
`ConcreteSpec.Core.Own.RetirementEvidence.retired`). -/
example : ResultSatisfies (fun t => RetiredPlace t ⟨0⟩) (bind types x retired) :=
  bind_retired_place types retired x ⟨0⟩ (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.bind_retired_place`, case
`ConcreteSpec.Core.Own.RetirementEvidence.retired`, subject `ConcreteSpec.Core.Own.bind`,
mutant `ConcreteSpec.Core.Own.RetirementEvidence.recycleBind`). -/
example : RetiredPlace retired ⟨0⟩ ∧
    ¬ ResultSatisfies (fun t => RetiredPlace t ⟨0⟩) (recycleBind types x retired) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.drop_retired_place`, case
`ConcreteSpec.Core.Own.RetirementEvidence.retired`). -/
example : ResultSatisfies (fun t => RetiredPlace t ⟨0⟩) (drop [] retired) :=
  drop_retired_place retired [] ⟨0⟩ (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.drop_retired_place`, case
`ConcreteSpec.Core.Own.RetirementEvidence.retired`, subject `ConcreteSpec.Core.Own.drop`,
mutant `ConcreteSpec.Core.Own.RetirementEvidence.rewindDrop`). -/
example : RetiredPlace retired ⟨0⟩ ∧
    ¬ ResultSatisfies (fun t => RetiredPlace t ⟨0⟩) (rewindDrop [] retired) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.call_birth_retired`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`). -/
example : ResultSatisfies (fun t => RetiredLoan t.own ⟨1⟩)
    (passOwner ⟨LoanHistoryEvidence.endedChild, []⟩ ⟨1⟩ [] .mutable DirectArgumentWitnesses.z) :=
  call_birth_retired _ _ _ _ _ _ (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.call_birth_retired`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.passOwner`,
mutant `ConcreteSpec.Core.Own.RetirementEvidence.recyclePass`). -/
example : RetiredLoan LoanHistoryEvidence.endedChild ⟨1⟩ ∧
    ¬ ResultSatisfies (fun t => RetiredLoan t.own ⟨1⟩)
      (recyclePass ⟨LoanHistoryEvidence.endedChild, []⟩ ⟨1⟩ [] .mutable DirectArgumentWitnesses.z) := by decide
end RetirementEvidence

/-- The existing call operands, with one uniformly substituted lifecycle callback. -/
abbrev protocolOps (lifecycle : Exec.Life → CallState → Option CallState) : Exec.Operands :=
  { Examples.callOps with life := lifecycle }

/-- A prefix certificate relates the actual before/after states to ownership operations.
The protected call loan cannot be explicitly ended, and a nested return must leave a different
activation. These are checker admission premises; this is not a checker implementation. -/
inductive CalleeEffect (lifecycle : Exec.Life → CallState → Option CallState)
    (held : Loan) : CallState → CallState → Prop where
  | quiet {s t} (loans : t.own.loans = s.own.loans) : CalleeEffect lifecycle held s t
  | borrow {s t p path m} (h : borrow s.own p path m = some t) :
      CalleeEffect lifecycle held s { s with own := t }
  | reborrow {s t i path m a} (h : reborrowTo s.own i path m a = some t) :
      CalleeEffect lifecycle held s { s with own := t }
  | endOther {s t i} (different : held.id ≠ i) (h : endLoan s.own i = some t) :
      CalleeEffect lifecycle held s { s with own := t }
  | enter {s t types params} (h : enter types params s.own = some t) :
      CalleeEffect lifecycle held s { s with own := t }
  | adopt {s t args} (different : held.id ∉ args.map Prod.snd)
      (h : adoptDown s.own args = some t) : CalleeEffect lifecycle held s { s with own := t }
  | ownerArgument {s t p path m x} (h : passOwner s p path m x = some t) :
      CalleeEffect lifecycle held s t
  | viewArgument {s t i x} (h : passDown s i x = some t) : CalleeEffect lifecycle held s t
  | compose {s u t} (first : CalleeEffect lifecycle held s u)
      (wf : WellFormed u.own) (second : CalleeEffect lifecycle held u t) :
      CalleeEffect lifecycle held s t
  | life {s t e} (h : lifecycle e s = some t)
      (notReturn : e = .leave → ∃ f, s.own.frames.head? = some f ∧ held.activation ≠ f.id)
      (notEntry : ∀ ps, e ≠ .enter ps) : CalleeEffect lifecycle held s t

/-- An arbitrary finite prefix of the real machine, with explicit ownership-effect and
well-formedness certificates at each transition. No fuel bound, skip-body assumption, or
loan-liveness/freeze conclusion is among the premises. Nested entries compose actual entry
and adoption effects; births and endings have separate constructors. Argument adoption must
exclude the already adopted protected loan identity. The checker must supply the certificates. -/
inductive AdmittedCalleePrefix (lifecycle : Exec.Life → CallState → Option CallState)
    (P : Exec.Program (protocolOps lifecycle)) (held : Loan) :
    Exec.Config (protocolOps lifecycle) → Exec.Config (protocolOps lifecycle) → Prop where
  | refl (c) : AdmittedCalleePrefix lifecycle P held c c
  | next {a b c} (trace : AdmittedCalleePrefix lifecycle P held a b)
      (wf : WellFormed b.store.own)
      (step : Exec.step P b = some (.next c))
      (effect : CalleeEffect lifecycle held b.store c.store) :
      AdmittedCalleePrefix lifecycle P held a c

/-- Helper (inversion). A lifecycle event other than entry preserves a caller's protected
loan: local storage removal never edits loans, and nested leave retains caller loans. -/
private theorem callee_life_retains (types : Var → Option Descriptor) (s t : CallState)
    (held : Loan) (hs : WellFormed s.own) (hl : held ∈ s.own.loans) (e : Exec.Life)
    (h : callLife types e s = some t)
    (notReturn : e = .leave → ∃ f, s.own.frames.head? = some f ∧ held.activation ≠ f.id)
    (notEntry : ∀ ps, e ≠ .enter ps) : held ∈ t.own.loans := by
  cases e with
  | enter ps => exact False.elim (notEntry ps rfl)
  | leave =>
    obtain ⟨f, hf, different⟩ := notReturn rfl
    simp only [callLife] at h
    split at h
    · obtain ⟨u, hu, h⟩ := Option.map_eq_some_iff.mp h
      cases h
      have keep := leave_retains_caller s.own f held hs hf hl different
      change leave s.own = some u at hu
      rwa [hu] at keep
    · contradiction
  | bind x =>
    simp only [callLife] at h
    split at h
    · obtain ⟨u, hu, h⟩ := Option.map_eq_some_iff.mp h
      cases h
      change bind types x s.own = some u at hu
      unfold bind at hu
      obtain ⟨f, _, hu⟩ := Option.bind_eq_some_iff.mp hu
      split at hu
      · unfold reinitialize at hu
        obtain ⟨a, _, hu⟩ := Option.bind_eq_some_iff.mp hu
        split at hu
        · cases hu; exact hl
        · contradiction
      · obtain ⟨d, _, hu⟩ := Option.bind_eq_some_iff.mp hu
        split at hu
        · cases hu; exact hl
        · contradiction
    · contradiction
  | drop xs =>
    simp only [callLife] at h
    split at h
    · obtain ⟨u, hu, h⟩ := Option.map_eq_some_iff.mp h
      cases h
      change drop xs s.own = some u at hu
      unfold drop at hu
      obtain ⟨f, _, hu⟩ := Option.bind_eq_some_iff.mp hu
      dsimp at hu
      split at hu
      · cases hu; exact hl
      · contradiction
    · contradiction

/-- Helper (inversion). Loan creation retains all pre-existing records, while ending another
loan filters only that identity. This proves preservation, rather than requiring it as a trace premise. -/
private theorem callee_effect_retains (types : Var → Option Descriptor) (s t : CallState)
    (held : Loan) (hs : WellFormed s.own) (hl : held ∈ s.own.loans)
    (effect : CalleeEffect (callLife types) held s t) : held ∈ t.own.loans := by
  induction effect with
  | quiet loans => rwa [loans]
  | borrow h =>
    unfold borrow at h
    obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h; exact List.mem_cons_of_mem _ hl
    · contradiction
  | reborrow h =>
    unfold reborrowTo at h
    obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h; exact List.mem_cons_of_mem _ hl
    · contradiction
  | endOther different h =>
    unfold endLoan at h
    obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · contradiction
    · cases h; exact List.mem_filter.mpr ⟨hl, by simpa using different⟩
  | enter h =>
    have same := enter_loans _ _ _ _ h
    rwa [same]
  | adopt different h =>
    unfold adoptDown at h
    obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h
      apply List.mem_map.mpr
      exact ⟨held, hl, by simp [different]⟩
    · contradiction
  | ownerArgument h =>
    unfold passOwner at h
    split at h
    · contradiction
    · obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h; exact List.mem_cons_of_mem _ hl
      · contradiction
  | viewArgument h =>
    have bound := (hs.2.2.2.1 held hl).1
    have different : held.id.id ≠ _ := Nat.ne_of_lt bound
    unfold passDown at h
    split at h
    · contradiction
    · obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
      obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
      cases h
      unfold reborrow at hu
      obtain ⟨f, _, hu⟩ := Option.bind_eq_some_iff.mp hu
      unfold reborrowTo at hu
      obtain ⟨b, _, hu⟩ := Option.bind_eq_some_iff.mp hu
      split at hu
      · cases hu
        apply List.mem_map.mpr
        exact ⟨held, List.mem_cons_of_mem _ hl, by simp [different]⟩
      · contradiction
  | compose first wf second ih₁ ih₂ => exact ih₂ wf (ih₁ hs hl)
  | life h notReturn notEntry => exact callee_life_retains types _ _ held hs hl _ h notReturn notEntry

/-- Lifetime conclusion at a callee trace. All accesses that conflict with the mutable call
loan are frozen. Usability of a suspended parent is intentionally not promised while it has
lent another child; C5's downward protocol governs that suspension. -/
def HeldAt (s : CallState) (held : Loan) : Prop :=
  held ∈ s.own.loans ∧ ownerAccess s.own held.root .mutable = false ∧
    ownerAccess s.own held.root .shared = false

/-- The universal trace/freeze contract, with uniformly substituted lifecycle and permission
operations for independent negative controls. -/
def DeferredLifetime (lifecycle : Exec.Life → CallState → Option CallState)
    (permit : State → Hybrid.Place → Mode → Bool) : Prop :=
  ∀ (P : Exec.Program (protocolOps lifecycle)) (held : Loan)
    (a b : Exec.Config (protocolOps lifecycle)), held.mode = .mutable →
    held ∈ a.store.own.loans → AdmittedCalleePrefix lifecycle P held a b →
    held ∈ b.store.own.loans ∧ permit b.store.own held.root .mutable = false ∧
      permit b.store.own held.root .shared = false

/-- Helper (trace induction). Proves the loan and freeze postconditions from actual effects. -/
private theorem deferred_prefix_result (types : Var → Option Descriptor)
    (P : Exec.Program (protocolOps (callLife types))) (held : Loan)
    (a b : Exec.Config (protocolOps (callLife types)))
    (mutable : held.mode = .mutable) (live : held ∈ a.store.own.loans)
    (trace : AdmittedCalleePrefix (callLife types) P held a b) : HeldAt b.store held := by
  have retained : held ∈ b.store.own.loans := by
    induction trace with
    | refl => exact live
    | next trace wf _ effect ih => exact callee_effect_retains types _ _ held wf ih effect
  have blocks (m : Mode) : (b.store.own.loans.all (fun l =>
      l.root != held.root || (m == Mode.shared && l.mode == Mode.shared))) = false := by
    apply List.all_eq_false.mpr
    exact ⟨held, retained, by cases m <;> simp [mutable]⟩
  refine ⟨retained, ?_, ?_⟩
  all_goals
    cases found : slot b.store.own held.root with
    | none => simp only [ownerAccess, found, Option.any_none]
    | some x => simp only [ownerAccess, found, Option.any_some, blocks, Bool.and_false]

/-- Law (authority: docs/decisions.md#decided-2026-11-08-recovered-d3, C5 question 2;
#od2; #od15; #od69). (subject `ConcreteSpec.Core.Own.callLife`). Every arbitrary admitted
callee trace retains its mutable call loan and freezes the owner. The actual machine body
and trace length are unrestricted; the explicit effect certificates exclude ending that
loan or leaving its activation before the boundary. This conditional Law does not claim
that arbitrary unchecked bodies satisfy the certificates. -/
theorem deferred_call_lifetime (types : Var → Option Descriptor) :
    DeferredLifetime (callLife types) ownerAccess :=
  deferred_prefix_result types

/-- Law (authority: docs/decisions.md#decided-2026-11-05-core Q9.8;
#decided-2026-11-08-recovered-d3 C5 question 2; #od2).
(subject `ConcreteSpec.Core.Exec.step`). Registering a deferred call does not evaluate any
operand, stage any pending argument, or change any ownership fact, for any operand list. -/
theorem deferred_registration (O : Exec.Operands) (P : Exec.Program O)
    (f : FnId) (args : List O.Expr) (site : SiteId) (L : AList Var O.Val)
    (s : O.Store) (k : List (Exec.Frame O)) (c : Exec.Config O)
    (h : Exec.step P ⟨.exec (.defer f args site), L, s, k⟩ = some (.next c)) : c.store = s :=
  defer_keeps_store_result O P f args site L s k c h

/-- Helper (list induction). Filtering cannot change a successful search when its selected
record passes the filter. Earlier rejected search candidates stay rejected. -/
private theorem find_filter_kept {α : Type} (xs : List α) (p q : α → Bool) (x : α)
    (found : xs.find? p = some x) (kept : q x = true) : (xs.filter q).find? p = some x := by
  induction xs with
  | nil => simp at found
  | cons a xs ih =>
    by_cases hp : p a = true
    · simp [hp] at found
      subst x
      simp [kept, hp]
    · have found' : xs.find? p = some x := by simpa [hp] using found
      have := ih found'
      cases hq : q a <;> simp [hq, hp, this]

/-- Helper (projection). Ending any set of local loans cannot change storage records. -/
private theorem endLocals_slots (a : Nat) (s t : State) (h : endLocals a s = some t) :
    t.slots = s.slots := by
  apply fold_projection State.slots _ ?_ _ s t h
  intro u l v hv
  unfold endLoan at hv
  obtain ⟨b, _, hv⟩ := Option.bind_eq_some_iff.mp hv
  split at hv
  · contradiction
  · cases hv; rfl

/-- The complete return-boundary postcondition, independent of the callee's body length. -/
def ReturnedCall (depart : State → Option State) (s : State) (f : Activation)
    (held : Loan) (root : Hybrid.Place) : Prop :=
  ResultSatisfies (fun t =>
    (∀ l ∈ t.loans, l.activation ≠ f.id) ∧
    (∀ l ∈ s.loans, l.activation ≠ f.id → l ∈ t.loans) ∧
    loan t held.id = none ∧ ownerAccess t root .mutable = true) (depart s)

/-- Law (authority: docs/decisions.md#decided-2026-11-08-recovered-d3 C5 question 2;
#od2; #od15; #od69). (subject `ConcreteSpec.Core.Own.leave`). For any admitted callee
endpoint, successful return ends the callee's loans, preserves every caller loan and restores
a caller-owned mutable root. The explicit premises require the root still owned outside the
callee and no other activation's loan on that root. Refusal is not normal return. -/
theorem deferred_return_lifetime (s : State) (f : Activation) (held : Loan) (x : Slot)
    (hs : WellFormed s) (hf : s.frames.head? = some f)
    (live : held ∈ s.loans) (activation : held.activation = f.id)
    (root : held.root = x.place) (found : slot s x.place = some x)
    (outer : x.activation ≠ f.id) (owned : x.status = .owned) (mutable : x.desc.mutable = true)
    (exclusive : ∀ l ∈ s.loans, l.root = x.place → l.activation = f.id) :
    ReturnedCall leave s f held held.root := by
  rw [root]
  unfold ReturnedCall
  cases h : leave s with
  | none => trivial
  | some t =>
    have removed := leave_no_activation_loan s f hf
    rw [h] at removed
    have subset := life_subset (fun _ => none) .leave s t h
    have rootClear : ∀ l ∈ t.loans, l.root ≠ x.place := by
      intro l hl he
      exact removed l hl (exclusive l (subset l hl) he)
    have dead : loan t held.id = none := by
      apply List.find?_eq_none.mpr
      intro l hl he
      have hid : l.id = held.id := by simpa using he
      have eq := loan_unique s.loans hs.2.1 l held (subset l hl) live hid
      subst l
      exact removed held hl activation
    obtain ⟨u, v, hu, hv, ht⟩ := leave_parts s t f hf h
    have uslots := endLocals_slots f.id s u hu
    have uframes : u.frames = s.frames :=
      fold_projection State.frames _ (fun a l b hb => endLoan_frames a b l.id hb) _ s u hu
    have pclear : x.place ∉ List.map Prod.snd
        (f.locals.filter fun e => decide (e.1 ∈ List.map Prod.fst f.locals)) := by
      intro hm
      obtain ⟨e, he, hep⟩ := List.mem_map.mp hm
      have em := (List.mem_filter.mp he).1
      obtain ⟨a, ha, ap, act⟩ := hs.2.2.2.2.2.2.2.2 f (List.mem_of_head? hf) e.2
        (List.mem_map.mpr ⟨e, em, rfl⟩)
      have xm := List.mem_of_find?_eq_some found
      have eq := slot_unique s.slots hs.1 a x ha xm (ap.trans hep)
      subst a
      exact outer act
    have preserved : slot t x.place = some x := by
      unfold drop at hv
      rw [uframes, hf] at hv
      dsimp at hv
      split at hv
      · cases hv
        rw [ht]
        unfold slot
        apply find_filter_kept
        · simpa only [uslots] using (show s.slots.find? (·.place == x.place) = some x from found)
        · simpa using pclear
      · contradiction
    refine ⟨removed, ?_, dead, ?_⟩
    · intro l hl different
      have keep := leave_retains_caller s f l hs hf hl different
      rwa [h] at keep
    · simpa [ownerAccess, preserved, owned, mutable] using rootClear

/-- Entry must install a usable view, rather than merely retain an unusable loan record. -/
def AdoptedView (adopt : State → List (Var × LoanId) → Option State)
    (s : State) (parameter : Var) (i : LoanId) : Prop :=
  ∃ t, adopt s [(parameter, i)] = some t ∧ loanAccess t i .mutable = true ∧
    t.frames.head?.bind (fun f => AList.lookup f.views parameter) = some i

/-- Law (authority: docs/decisions.md#decided-2026-11-08-recovered-d3 C5 question 2;
#decided-2026-11-05-core Q3.2; #od23). (subject `ConcreteSpec.Core.Own.adoptDown`).
A pending direct mutable argument becomes a usable callee view. The explicit premises
state the issued destination, unique fresh loan, free binder and whole-root exclusion;
no body or execution fuel is fixed. -/
theorem adopted_view_usable (s : State) (f : Activation) (l : Loan) (old : List Loan)
    (parameter : Var) (hf : s.frames.head? = some f) (issued : f.id < s.nextActivation)
    (table : s.loans = l :: old) (pending : l.pendingArgument = true)
    (mutable : l.mode = .mutable) (free : AList.lookup f.locals parameter = none)
    (unique : ∀ b ∈ old, b.id ≠ l.id) (clear : ∀ b ∈ old, b.root ≠ l.root) :
    AdoptedView adoptDown s parameter l.id := by
  have oldMap : (old.map fun b => if b.id = l.id then
      { b with activation := f.id, pendingArgument := false } else b) = old := by
    calc
      _ = old.map id := by
        apply List.map_congr_left
        intro b hb
        simp [unique b hb]
      _ = old := List.map_id _
  have clearBool : old.all (fun b => b.id == l.id || decide (b.id ∈ l.ancestors) ||
      b.root != l.root || (Mode.mutable == Mode.shared && b.mode == Mode.shared)) = true := by
    apply List.all_eq_true.mpr
    intro b hb
    simp [clear b hb]
  refine ⟨{ s with
    loans := { l with activation := f.id, pendingArgument := false } :: old
    frames := { f with views := [(parameter, l.id)] ++ f.views } :: s.frames.tail }, ?_, ?_, ?_⟩
  · simp [adoptDown, hf, issued, table, pending, free, oldMap]
  · simp only [loanAccess, loan, List.find?_cons, beq_self_eq_true, Option.any_some]
    simp [mutable, clearBool]
  · simp [AList.lookup]

/-- Law (authority: docs/decisions.md#od69; #od2;
#decided-2026-11-08-recovered-d3 C5 question 2).
(subject `ConcreteSpec.Core.Own.callLife`). Successful storage-only cleanup preserves
all loans and has no pending argument buffer. This applies to the final enclosing scope,
not just callee prefixes, and prevents a normal-completion test from stopping before teardown. -/
theorem scope_cleanup_loans (types : Var → Option Descriptor) (s t : CallState) (xs : List Var)
    (h : callLife types (.drop xs) s = some t) : t.own.loans = s.own.loans ∧ t.pending = [] := by
  simp only [callLife] at h
  split at h
  · rename_i empty
    obtain ⟨u, hu, h⟩ := Option.map_eq_some_iff.mp h
    cases h
    change drop xs s.own = some u at hu
    unfold drop at hu
    obtain ⟨f, _, hu⟩ := Option.bind_eq_some_iff.mp hu
    dsimp at hu
    split at hu
    · cases hu; exact ⟨rfl, empty⟩
    · contradiction
  · contradiction

/-- The callee uses its view. A dead-on-arrival adopted loan therefore refuses execution. -/
def heldProgram (lifecycle : Exec.Life → CallState → Option CallState)
    (parameter : Var) : Exec.Program (protocolOps lifecycle) :=
  ⟨[(Examples.callee, ⟨[(parameter, ())], (), ⟨1⟩,
    .cond (.useView parameter) .skip .skip ⟨0⟩⟩)]⟩

/-- Real late operand evaluation: registration starts with no pending arguments and an
ordinary direct-owner operand. The enclosing scope also executes its final storage drop. -/
def heldRegistration (lifecycle : Exec.Life → CallState → Option CallState)
    (s : CallState) (root : Hybrid.Place) (parameter : Var) : Exec.Config (protocolOps lifecycle) :=
  ⟨.exec (.defer Examples.callee [.ownerArgument root .mutable parameter] ⟨0⟩), [], s,
    [.scope ⟨0⟩ [] []]⟩

/-- The complete fixture contract is an Instance, not a general lifetime Law. It checks actual
late argument evaluation, a usable view, normal completion of the enclosing scope, restoration
of caller loans and slots, an empty pending buffer, and the owner's restored permission. -/
def DeferredExecution (lifecycle : Exec.Life → CallState → Option CallState)
    (s : CallState) (root : Hybrid.Place) (parameter : Var) : Prop :=
  let r := Exec.run (heldProgram lifecycle parameter) (fun _ p => nomatch p) 30
    (heldRegistration lifecycle s root parameter)
  r.ending = .final .normal ∧ r.residual.store.own.slots = s.own.slots ∧
    r.residual.store.own.loans = s.own.loans ∧ r.residual.store.pending = [] ∧
    ownerAccess r.residual.store.own root .mutable = true

namespace DeferredLifetimeEvidence
open Examples DirectArgumentWitnesses

/-- The adopted call loan; its id, root and mode were allocated by real argument evaluation. -/
def held : Loan := ⟨⟨1⟩, ⟨1⟩, [], .mutable, none, [], 1, false, true⟩
/-- The actual callee entry reached after registration, cleanup start and argument evaluation. -/
def entry : Exec.Config (protocolOps (callLife types)) :=
  (Exec.run (heldProgram (callLife types) z) (fun _ p => nomatch p) 3
    (heldRegistration (callLife types) caller ⟨1⟩ z)).residual

/-- Instance (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.entry`). The view is live in a real call. -/
example : HeldAt entry.store held :=
  deferred_call_lifetime types (heldProgram (callLife types) z) held entry entry rfl
    (by decide) (.refl entry)

/-- Instance (law `ConcreteSpec.Core.Own.deferred_registration`, case
`ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`). Registration has no pending argument. -/
example : ∃ c, Exec.step (heldProgram (callLife types) z)
    (heldRegistration (callLife types) caller ⟨1⟩ z) = some (.next c) ∧
    c.store = caller ∧ c.store.pending = [] := by
  refine ⟨_, rfl, ?_, rfl⟩
  exact deferred_registration _ (heldProgram (callLife types) z) callee
    [.ownerArgument ⟨1⟩ .mutable z] ⟨0⟩ [] caller [.scope ⟨0⟩ [] []] _ rfl

/-- Instance (execution). Real view use and all enclosing cleanup reach normal finality;
the unrelated caller loan remains live. This replaces the old skip/fuel-6 observation. -/
example : DeferredExecution (callLife types) caller ⟨1⟩ z := by
  exact ⟨rfl, rfl, rfl, rfl, rfl⟩

/-- Mutant: retain every admission check but fail to clear the adopted argument's pending bit. -/
def deadAdopt (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then { l with activation := f.id, pendingArgument := true } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none

/-- Substitute a one-rule adoption or return mutation through every lifecycle event. -/
def callLifeWith (depart : State → Option State) (adopt : State → List (Var × LoanId) → Option State)
    (types : Var → Option Descriptor) : Exec.Life → CallState → Option CallState
  | .enter params, s => do
    if s.pending.all (fun e => decide (e.1 ∈ params)) then
      let own ← enter types (params.filter fun x => decide (x ∉ s.pending.map Prod.fst)) s.own
      let own ← adopt own s.pending
      pure ⟨own, []⟩
    else none
  | e, s => if s.pending = [] then
      ((match e with
        | .leave => depart s.own
        | _ => life types e s.own)).map fun own => { s with own := own }
    else none

/-- Planted (execution, subject `ConcreteSpec.Core.Own.adoptDown`, mutant
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.deadAdopt`). A pending view cannot be used. -/
example : ¬ DeferredExecution (callLifeWith leave deadAdopt types) caller ⟨1⟩ z := by
  intro h
  cases h.1

/-- The loan table before adoption, reached by real argument evaluation and ordinary entry. -/
def beforeAdopt : State :=
  (enter types [] ((passOwner caller ⟨1⟩ [] .mutable z).getD caller).own).getD caller.own
/-- The fresh pending argument before the callee claims it. -/
def pendingLoan : Loan := { held with activation := 0, pendingArgument := true }

/-- Instance (law `ConcreteSpec.Core.Own.adopted_view_usable`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.beforeAdopt`). -/
example : AdoptedView adoptDown beforeAdopt z ⟨1⟩ :=
  adopted_view_usable beforeAdopt ⟨1, [], []⟩ pendingLoan caller.own.loans z
    rfl (by decide) rfl rfl rfl rfl (by decide) (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.adopted_view_usable`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.beforeAdopt`, subject `ConcreteSpec.Core.Own.adoptDown`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.deadAdopt`). Only usability fails. -/
example : ¬ AdoptedView deadAdopt beforeAdopt z ⟨1⟩ := by
  rintro ⟨t, ht, usable, _⟩
  cases ht
  cases usable

/-- Mutant: remove the loans on an empty scope drop, retaining all other lifecycle cases.
This corrupts both callee-prefix cleanup and enclosing-scope cleanup. -/
def dropWipe : Exec.Life → CallState → Option CallState
  | .drop [], s => some { s with own := { s.own with loans := [] } }
  | e, s => callLife types e s

/-- A real cleanup prefix with an adopted callee view and an empty storage scope. -/
def cleanup (lifecycle : Exec.Life → CallState → Option CallState) : Exec.Config (protocolOps lifecycle) :=
  ⟨.done, [], entry.store, [.scope ⟨2⟩ [] []]⟩
/-- The result of the mutated scope-drop branch. -/
def wiped : Exec.Config (protocolOps dropWipe) :=
  ⟨.done, [], { entry.store with own := { entry.store.own with loans := [] } }, []⟩

/-- Planted (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.cleanup`, subject `ConcreteSpec.Core.Own.callLife`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.dropWipe`). A genuine pre-return
cleanup transition ends the call loan prematurely. No final-state comparison is needed. -/
example : ¬ DeferredLifetime dropWipe ownerAccess := by
  intro law
  have bad := law (heldProgram dropWipe z) held (cleanup dropWipe) wiped rfl (by decide)
    (.next (.refl _) (by decide) rfl (.life (e := .drop []) rfl (by intro h; cases h) (by intro ps h; cases h)))
  have missing := bad.1
  cases missing

/-- Mutant: owner access checks the owned slot but ignores the live loan table. -/
def ignoreLoans (s : State) (p : Hybrid.Place) (m : Mode) : Bool :=
  (slot s p).any fun x => x.status == .owned && (m == .shared || x.desc.mutable)

/-- Planted (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.entry`, subject `ConcreteSpec.Core.Own.ownerAccess`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.ignoreLoans`). Loan membership
remains true, but the independent freeze clause fails at the entry prefix. -/
example : ¬ DeferredLifetime (callLife types) ignoreLoans := by
  intro law
  have bad := law (heldProgram (callLife types) z) held entry entry rfl (by decide) (.refl _)
  cases bad.2.1

/-- Instance (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.cleanup`). The real cleanup step retains the loan. -/
example : HeldAt (Exec.run (heldProgram (callLife types) z) (fun _ p => nomatch p) 1
    (cleanup (callLife types))).residual.store held := by
  apply deferred_call_lifetime types (heldProgram (callLife types) z) held (cleanup (callLife types))
  · rfl
  · decide
  · exact .next (.refl _) (by decide) rfl
      (.life (e := .drop []) rfl (by intro h; cases h) (by intro ps h; cases h))

/-- Instance (law `ConcreteSpec.Core.Own.deferred_return_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.entry`). The callee endpoint can be any
admitted state; this concrete one retains the unrelated caller loan. -/
example : ReturnedCall leave entry.store.own ⟨1, [], [(z, ⟨1⟩)]⟩ held ⟨1⟩ :=
  deferred_return_lifetime entry.store.own ⟨1, [], [(z, ⟨1⟩)]⟩ held
    ⟨⟨1⟩, 0, ⟨.int .i32, false, true⟩, .owned⟩ (by decide) rfl
    (by decide) rfl rfl rfl (by decide) rfl rfl (by decide)


def leaveRetain (s : State) : Option State := do
  let f ← s.frames.head?
  let s ← endLocals s.nextActivation s
  let s ← drop (List.map Prod.fst f.locals) s
  some { s with frames := s.frames.tail }

def endAllLocals (_activation : Nat) (s : State) : Option State :=
  (s.loans.filter (fun _ => true)).foldlM (fun s l => endLoan s l.id) s

def leaveAll (s : State) : Option State := do
  let f ← s.frames.head?
  let s ← endAllLocals f.id s
  let s ← drop (List.map Prod.fst f.locals) s
  some { s with frames := s.frames.tail }

/-- Planted (law `ConcreteSpec.Core.Own.deferred_return_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.entry`, subject `ConcreteSpec.Core.Own.leave`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.leaveRetain`). The return boundary
must end exactly the callee's loans while retaining caller loans. -/
example : ¬ ReturnedCall leaveRetain entry.store.own ⟨1, [], [(z, ⟨1⟩)]⟩ held ⟨1⟩ := by
  intro h
  have bad := h.1 held (by decide)
  revert bad
  decide
/-- Planted (law `ConcreteSpec.Core.Own.deferred_return_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.entry`, subject `ConcreteSpec.Core.Own.leave`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.leaveAll`). The return boundary
must end exactly the callee's loans while retaining caller loans. -/
example : ¬ ReturnedCall leaveAll entry.store.own ⟨1, [], [(z, ⟨1⟩)]⟩ held ⟨1⟩ := by
  intro h
  have bad := h.2.1 (caller.own.loans.headD held) (by decide) (by decide)
  revert bad
  decide
/-- Mutant: only the enclosing caller scope wipes loans; callee execution and return remain
unchanged. The old fuel-6 observation never reached this mutation. -/
def outerWipe : Exec.Life → CallState → Option CallState
  | .drop [], s => if s.own.frames.length = 1 then
      some { s with own := { s.own with loans := [] } } else callLife types (.drop []) s
  | e, s => callLife types e s

/-- Instance (law `ConcreteSpec.Core.Own.scope_cleanup_loans`, case
`ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`). Final enclosing cleanup keeps caller loans. -/
example : caller.own.loans = caller.own.loans ∧ caller.pending = [] :=
  scope_cleanup_loans types caller caller [] rfl
/-- Planted (law `ConcreteSpec.Core.Own.scope_cleanup_loans`, case
`ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.callLife`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.outerWipe`). The final scope cannot
silently end a caller loan after the deferred callee has returned. -/
example : ∃ t, outerWipe (.drop []) caller = some t ∧ t.own.loans ≠ caller.own.loans := by
  exact ⟨_, rfl, by decide⟩
/-- Planted (execution, subject `ConcreteSpec.Core.Own.callLife`, mutant
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.outerWipe`). Normal enclosing completion
exposes the late corruption that the old out-of-fuel endpoint missed. -/
example : ¬ DeferredExecution outerWipe caller ⟨1⟩ z := by
  intro h
  have bad := h.2.2.1
  cases bad

/-- Mutant: evaluate deferred operands during registration. Only the defer branch changes;
all other statements continue through the real dispatcher. -/
def eagerRegistration (P : Exec.Program (protocolOps (callLife types)))
    (c : Exec.Config (protocolOps (callLife types))) : Option (Exec.Out (protocolOps (callLife types))) :=
  match c.ctrl with
  | .exec (.defer f args site) =>
    match Exec.evalArgs (protocolOps (callLife types)) c.locals c.store args with
    | .ok (_, st) => (Exec.register ⟨f, args, site⟩ c.frames).map fun frames =>
        .next ⟨.done, c.locals, st, frames⟩
    | _ => none
  | _ => Exec.step P c

/-- Planted (law `ConcreteSpec.Core.Own.deferred_registration`, case
`ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Exec.step`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.eagerRegistration`). The registered
call has evaluated its owner argument, leaving a pending loan across ordinary scope statements. -/
example : ∃ c, eagerRegistration (heldProgram (callLife types) z)
    (heldRegistration (callLife types) caller ⟨1⟩ z) = some (.next c) ∧
    c.store ≠ caller ∧ c.store.pending ≠ [] ∧ callLife types (.drop []) c.store = none := by
  exact ⟨_, rfl, by decide, by decide, rfl⟩

/-- Mutant: only mutable owner access ignores active loans. Shared access is unchanged. -/
def unfreezeMutable (s : State) (p : Hybrid.Place) : Mode → Bool
  | .mutable => ignoreLoans s p .mutable
  | .shared => ownerAccess s p .shared
/-- Mutant: only shared owner access ignores a mutable loan. Mutable access is unchanged. -/
def unfreezeShared (s : State) (p : Hybrid.Place) : Mode → Bool
  | .mutable => ownerAccess s p .mutable
  | .shared => ignoreLoans s p .shared
/-- Planted (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.entry`, subject `ConcreteSpec.Core.Own.ownerAccess`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.unfreezeMutable`). Isolate the mutable
freeze clause; shared access is still frozen. -/
example : ¬ DeferredLifetime (callLife types) unfreezeMutable ∧
    unfreezeMutable entry.store.own held.root .shared = false := by
  refine ⟨?_, rfl⟩
  intro law
  have bad := law (heldProgram (callLife types) z) held entry entry rfl (by decide) (.refl _)
  cases bad.2.1
/-- Planted (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.entry`, subject `ConcreteSpec.Core.Own.ownerAccess`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.unfreezeShared`). Isolate the shared
freeze clause; mutable access is still frozen. -/
example : ¬ DeferredLifetime (callLife types) unfreezeShared ∧
    unfreezeShared entry.store.own held.root .mutable = false := by
  refine ⟨?_, rfl⟩
  intro law
  have bad := law (heldProgram (callLife types) z) held entry entry rfl (by decide) (.refl _)
  cases bad.2.2

/-- The former empty-callee execution is retained only as an Instance. Registration is
corrected to evaluate the actual owner argument late, with an empty pending buffer. -/
def skipProgram : Exec.Program (protocolOps (callLife types)) :=
  ⟨[(callee, ⟨[(z, ())], (), ⟨1⟩, .skip⟩)]⟩
/-- Instance (execution). The original skip callee now runs through normal enclosing-scope
completion, retaining all caller loans rather than stopping at an intermediate fuel budget. -/
example : let r := (Exec.run skipProgram (fun _ p => nomatch p) 30
    (heldRegistration (callLife types) caller ⟨1⟩ z));
  r.ending = .final .normal ∧ r.residual.store.own.loans = caller.own.loans ∧
      r.residual.store.pending = [] := by exact ⟨rfl, rfl, rfl⟩
/-- Mutant: adoption removes the call's arguments instead of transferring them. -/
def adoptDropArguments (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.filter (fun l => decide (l.id ∉ args.map Prod.snd))
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none

/-- Planted (law `ConcreteSpec.Core.Own.adopted_view_usable`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.beforeAdopt`, subject `ConcreteSpec.Core.Own.adoptDown`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.adoptDropArguments`). Early release
at entry is rejected separately from a retained but unusable view. -/
example : ¬ AdoptedView adoptDropArguments beforeAdopt z ⟨1⟩ := by
  rintro ⟨t, ht, usable, _⟩
  cases ht
  cases usable

end DeferredLifetimeEvidence
/-- Helper (inversion). Binding can allocate a place but never changes the loan supply. -/
private theorem bind_loan_supply (types : Var → Option Descriptor) (s t : State) (x : Var)
    (h : bind types x s = some t) : t.nextLoan = s.nextLoan := by
  unfold bind at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · unfold reinitialize at h
    obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h; rfl
    · contradiction
  · obtain ⟨d, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h; rfl
    · contradiction

/-- Helper (inversion). Storage teardown retains both identity supplies. -/
private theorem drop_supplies (s t : State) (xs : List Var) (h : drop xs s = some t) :
    t.nextPlace = s.nextPlace ∧ t.nextLoan = s.nextLoan := by
  unfold drop at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  dsimp at h
  split at h
  · cases h; exact ⟨rfl, rfl⟩
  · contradiction

/-- Helper (projection). No lifecycle event issues or rewinds loan identities. -/
private theorem life_loan_supply (types : Var → Option Descriptor) (s t : State) (e : Exec.Life)
    (h : life types e s = some t) : t.nextLoan = s.nextLoan := by
  cases e with
  | bind x => exact bind_loan_supply types s t x h
  | drop xs => exact (drop_supplies s t xs h).2
  | enter ps =>
    change ps.foldlM (fun s x => bind types x s) _ = some t at h
    exact fold_projection State.nextLoan _ (fun s x t h => bind_loan_supply types s t x h) ps
      { s with nextActivation := s.nextActivation + 1, frames := ⟨s.nextActivation, [], []⟩ :: s.frames } t h
  | leave =>
    unfold life leave at h
    obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
    obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
    obtain ⟨v, hv, h⟩ := Option.bind_eq_some_iff.mp h
    cases h
    exact (drop_supplies u v _ hv).2.trans
      (fold_projection State.nextLoan _ (fun s l t h => endLoan_supply_result s t l.id h) _ s u hu)

/-- Helper (induction/inversion). Places remain retired through entry, binding, teardown
and return; ending local loans changes neither the slots nor the place supply. -/
private theorem life_place_history (types : Var → Option Descriptor) (s : State)
    (e : Exec.Life) (p : Hybrid.Place) (hr : RetiredPlace s p) :
    ResultSatisfies (fun t => RetiredPlace t p) (life types e s) := by
  cases e with
  | bind x => exact bind_retired_place types s x p hr
  | drop xs => exact drop_retired_place s xs p hr
  | enter ps =>
    change ResultSatisfies (fun t => RetiredPlace t p) (enter types ps s)
    cases h : enter types ps s with
    | none => trivial
    | some t =>
      change ps.foldlM (fun s x => bind types x s) _ = some t at h
      have fold (xs : List Var) (a b : State) (ha : RetiredPlace a p)
          (h : xs.foldlM (fun s x => bind types x s) a = some b) : RetiredPlace b p := by
        induction xs generalizing a with
        | nil => simp at h; subst b; exact ha
        | cons x xs ih =>
          simp only [List.foldlM_cons] at h
          obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
          have keep := bind_retired_place types a x p ha
          rw [hu] at keep
          exact ih u keep h
      exact fold ps
        { s with nextActivation := s.nextActivation + 1, frames := ⟨s.nextActivation, [], []⟩ :: s.frames }
        t hr h
  | leave =>
    change ResultSatisfies (fun t => RetiredPlace t p) (leave s)
    cases h : leave s with
    | none => trivial
    | some t =>
      unfold leave at h
      obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
      obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
      obtain ⟨v, hv, h⟩ := Option.bind_eq_some_iff.mp h
      cases h
      have places : u.nextPlace = s.nextPlace := by
        apply fold_projection State.nextPlace _ ?_ _ s u hu
        intro a l b hb
        unfold endLoan at hb
        obtain ⟨a, _, hb⟩ := Option.bind_eq_some_iff.mp hb
        split at hb
        · contradiction
        · cases hb; rfl
      have slots := endLocals_slots f.id s u hu
      have ur : RetiredPlace u p := ⟨by simpa [places] using hr.1, by simpa [slots] using hr.2⟩
      have vr := drop_retired_place u (List.map Prod.fst f.locals) p ur
      rw [hv] at vr
      exact vr

/-- The lifecycle adapter cannot rewind an issued identity or revive absent storage/loans. -/
def LifecycleNonrevival (lifecycle : Exec.Life → CallState → Option CallState)
    (s : CallState) (e : Exec.Life) (p : Hybrid.Place) (i : LoanId) : Prop :=
  ResultSatisfies (fun t => RetiredPlace t.own p ∧ RetiredLoan t.own i) (lifecycle e s)

/-- Law (authority: docs/decisions.md#foundation-q1 C5; #decided-2026-11-05-core Q3.3;
#od15; #od69). (subject `ConcreteSpec.Core.Own.callLife`). Retired places and loans,
including identities originally issued to calls, cannot revive through any lifecycle event.
Entry/adoption can change loan destinations but cannot issue a retired identity. -/
theorem lifecycle_nonrevival (types : Var → Option Descriptor) (s : CallState) (e : Exec.Life)
    (p : Hybrid.Place) (i : LoanId) (hp : RetiredPlace s.own p) (hi : RetiredLoan s.own i) :
    LifecycleNonrevival (callLife types) s e p i := by
  unfold LifecycleNonrevival
  cases h : callLife types e s with
  | none => trivial
  | some t =>
    cases e with
    | enter ps =>
      simp only [callLife] at h
      split at h
      · obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
        obtain ⟨v, hv, h⟩ := Option.bind_eq_some_iff.mp h
        cases h
        have ur := life_place_history types s.own
          (.enter (ps.filter fun x => decide (x ∉ s.pending.map Prod.fst))) p hp
        change ResultSatisfies (fun t => RetiredPlace t p) (enter types _ s.own) at ur
        rw [hu] at ur
        have supply := life_loan_supply types s.own u (.enter _) hu
        have loans := enter_loans types s.own u _ hu
        unfold adoptDown at hv
        obtain ⟨f, _, hv⟩ := Option.bind_eq_some_iff.mp hv
        split at hv
        · cases hv
          refine ⟨ur, ?_, ?_⟩
          · simpa [supply] using hi.1
          · intro a ha
            obtain ⟨b, hb, rfl⟩ := List.mem_map.mp ha
            have hb' : b ∈ s.own.loans := by rwa [loans] at hb
            split <;> exact hi.2 b hb'
        · contradiction
      · contradiction
    | bind x =>
      simp only [callLife] at h
      split at h
      · obtain ⟨u, hu, h⟩ := Option.map_eq_some_iff.mp h
        cases h
        have pr := life_place_history types s.own (.bind x) p hp
        rw [hu] at pr
        exact ⟨pr, by simpa [life_loan_supply types s.own u (.bind x) hu] using hi.1,
          fun l hl => hi.2 l (life_subset types (.bind x) s.own u hu l hl)⟩
      · contradiction
    | drop xs =>
      simp only [callLife] at h
      split at h
      · obtain ⟨u, hu, h⟩ := Option.map_eq_some_iff.mp h
        cases h
        have pr := life_place_history types s.own (.drop xs) p hp
        rw [hu] at pr
        exact ⟨pr, by simpa [life_loan_supply types s.own u (.drop xs) hu] using hi.1,
          fun l hl => hi.2 l (life_subset types (.drop xs) s.own u hu l hl)⟩
      · contradiction
    | leave =>
      simp only [callLife] at h
      split at h
      · obtain ⟨u, hu, h⟩ := Option.map_eq_some_iff.mp h
        cases h
        have pr := life_place_history types s.own (.leave) p hp
        rw [hu] at pr
        exact ⟨pr, by simpa [life_loan_supply types s.own u (.leave) hu] using hi.1,
          fun l hl => hi.2 l (life_subset types (.leave) s.own u hu l hl)⟩
      · contradiction

namespace LifecycleHistoryEvidence
open Examples
/-- A populated state with both an absent issued place and an ended child loan. -/
def source : CallState := ⟨{ LoanHistoryEvidence.endedChild with nextPlace := 4 }, []⟩
/-- Mutant: scope teardown rewinds only the place supply in the adapter's result constructor. -/
def rewindPlace : Exec.Life → CallState → Option CallState
  | .drop xs, s => if s.pending = [] then (drop xs s.own).map fun own =>
      ⟨{ own with nextPlace := 0 }, []⟩ else none
  | e, s => callLife types e s
/-- Mutant: scope teardown rewinds only the loan supply in the adapter's result constructor. -/
def rewindLoan : Exec.Life → CallState → Option CallState
  | .drop xs, s => if s.pending = [] then (drop xs s.own).map fun own =>
      ⟨{ own with nextLoan := 0 }, []⟩ else none
  | e, s => callLife types e s
/-- Instance (law `ConcreteSpec.Core.Own.lifecycle_nonrevival`, case
`ConcreteSpec.Core.Own.LifecycleHistoryEvidence.source`). Ordinary entry preserves both histories. -/
example : LifecycleNonrevival (callLife types) source (.enter []) ⟨3⟩ ⟨1⟩ :=
  lifecycle_nonrevival types source (.enter []) ⟨3⟩ ⟨1⟩ (by decide) (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.lifecycle_nonrevival`, case
`ConcreteSpec.Core.Own.LifecycleHistoryEvidence.source`, subject `ConcreteSpec.Core.Own.callLife`,
mutant `ConcreteSpec.Core.Own.LifecycleHistoryEvidence.rewindPlace`). -/
example : ¬ LifecycleNonrevival rewindPlace source (.drop []) ⟨3⟩ ⟨1⟩ := by
  intro h
  have bad := h.1.1
  revert bad
  decide
/-- Planted (law `ConcreteSpec.Core.Own.lifecycle_nonrevival`, case
`ConcreteSpec.Core.Own.LifecycleHistoryEvidence.source`, subject `ConcreteSpec.Core.Own.callLife`,
mutant `ConcreteSpec.Core.Own.LifecycleHistoryEvidence.rewindLoan`). -/
example : ¬ LifecycleNonrevival rewindLoan source (.drop []) ⟨3⟩ ⟨1⟩ := by
  intro h
  have bad := h.2.1
  revert bad
  decide
end LifecycleHistoryEvidence

end ConcreteSpec.Core.Own
