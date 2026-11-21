import ConcreteSpec.Proofs.Basics
import ConcreteSpec.Proofs.Exec
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
The Law, Helper, Instance, Planted and Regression tags identify the roles of the declarations below.

Source disposition: the O3 optional-result restatements of O1's owned-root, moved-root,
birth/supply and passOwner results add no independent fact;
reuse their existing result helpers and preservation Laws. Likewise the instrument, life,
callLife, readOwner/writeOwner/readLoan/writeLoan erasure restatements remain owned by O2
and O1's result helpers. The generic instrument preservation wrapper remains O1's helper.
Single-step Laws with independent admission require progress as well as preservation.
The exact loan rules and laws below support the history proofs. OwnLegalStep adds the
composed dispatcher Laws. The original protocol witnesses use progress and preservation
projections.
Lifecycle, loan and issuance admission enumerate their event constructors. Admitted traces
compose ledger rules so earlier events can change the permissions of later events.
Nonrevival over arbitrary event lists and loan retention along certified prefixes remain
safety properties of produced transitions.
The registration Law reuses Exec's independent registration rule and dispatcher soundness.
Its planted controls evaluate arguments too early, either retaining their store effects or
refusing registration when evaluation fails.
The source deferredStep alias and silentPrefix driver are retired: protocolOps, Exec.step and
Exec.run supply the actual execution and uniform lifecycle substitution. Prefix Laws instead
use an unbounded inductive certificate of actual machine steps and admitted ownership effects.
Source run-then-overwrite,
whole-function-refusal and whole-function-no-op mutants are replaced by one-rule body mutants.
O3 protocol witnesses live here. O1's existing slot/loan uniqueness, fold projection,
entry-loan, leave inversion, ending-frame, removed-loan and lifecycle-subset Helpers are public so this
module can reuse their private dependencies without copying their proofs. Their statements
and proof ownership stay in O1; no O3 declaration remains beside a private helper.
No duplicate declaration or inherited APPR receipt is imported.

Held: ISS-0064/ISS-0065, ISS-0068–ISS-0073 and ISS-0076/ISS-0080 retain their existing scope
(source policy, constants, codecs, wiping, startup/ABI and aggregate representation).
No new language choice is made. References remain second-class: these protocols manipulate
loan identities, never stored or returned reference values (docs/PHILOSOPHY.md §§5–10).
-/
namespace ConcreteSpec.Core.Own

namespace LegalStep

def OwnerPermission (s : State) (p : Hybrid.Place) (m : Mode) : Prop :=
  ∃ a, slot s p = some a ∧ a.status = .owned ∧
    (m = .shared ∨ a.desc.mutable = true) ∧
    ∀ l ∈ s.loans, l.root ≠ p ∨ (m = .shared ∧ l.mode = .shared)

def LoanPermission (s : State) (i : LoanId) (m : Mode) : Prop :=
  ∃ a, loan s i = some a ∧ a.pendingArgument = false ∧
    (m = .shared ∨ a.mode = .mutable) ∧
    ∀ l ∈ s.loans, l.id = i ∨ l.id ∈ a.ancestors ∨ l.root ≠ a.root ∨
      (m = .shared ∧ l.mode = .shared)

/-- Law (authority: docs/decisions.md#od15). Owner permission obeys whole-root exclusion. -/
theorem ownerAccess_iff (s : State) (p : Hybrid.Place) (m : Mode) :
    ownerAccess s p m = true ↔ OwnerPermission s p m := by
  cases h : slot s p <;>
    simp [ownerAccess, OwnerPermission, h, Bool.or_eq_true, Bool.and_eq_true,
      List.all_eq_true, and_assoc, bne_iff_ne]

/-- Law (authority: docs/decisions.md#od15; #od23). A loan exempts its ancestors from exclusion. -/
theorem loanAccess_iff (s : State) (i : LoanId) (m : Mode) :
    loanAccess s i m = true ↔ LoanPermission s i m := by
  cases h : loan s i <;>
    simp [loanAccess, LoanPermission, h, Bool.or_eq_true, Bool.and_eq_true,
      List.all_eq_true, and_assoc, or_assoc, bne_iff_ne]

instance (s : State) (p : Hybrid.Place) (m : Mode) : Decidable (OwnerPermission s p m) :=
  decidable_of_iff (ownerAccess s p m = true) (ownerAccess_iff s p m)

instance (s : State) (i : LoanId) (m : Mode) : Decidable (LoanPermission s i m) :=
  decidable_of_iff (loanAccess s i m = true) (loanAccess_iff s i m)

def EndLeafAtBoundary (s : State) (i : LoanId) (t : State) : Prop :=
  (∃ a, loan s i = some a) ∧ (∀ l ∈ s.loans, l.parent ≠ some i) ∧
    t = { s with loans := s.loans.filter (·.id != i) }

def EndLoan (s : State) (i : LoanId) (t : State) : Prop :=
  OrdinaryLoanEnd s i ∧ EndLeafAtBoundary s i t

theorem endLeafAtBoundary_iff (s : State) (i : LoanId) (t : State) :
    endLeafAtBoundary s i = some t ↔ EndLeafAtBoundary s i t := by
  cases h : loan s i <;>
    simp [endLeafAtBoundary, EndLeafAtBoundary, h, List.any_eq_true, eq_comm (a := t)]

def Borrow (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode)
    (t : State) : Prop :=
  OwnerPermission s p m ∧ ∃ f, s.frames.head? = some f ∧ f.id < s.nextActivation ∧
    t = { s with nextLoan := s.nextLoan + 1
                 loans := ⟨⟨s.nextLoan⟩, p, path, m, none, [], f.id, false, false⟩ :: s.loans }

/-- The child grant keeps the source's root and path prefix, extends its ancestry, and
allocates one identity. Buffered and immediate grants differ only in their phase and destination. -/
def ChildGrant (s t : State) (source : Loan) (i : LoanId) (suffix : List Projection)
    (mode : Mode) (activation : Nat) (pending : Bool) : Prop :=
  t = { s with nextLoan := s.nextLoan + 1
               loans := ⟨⟨s.nextLoan⟩, source.root, source.path ++ suffix, mode, some i,
                 i :: source.ancestors, activation, pending, false⟩ :: s.loans }

def ReborrowTo (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (activation : Nat) (t : State) : Prop :=
  LoanPermission s i m ∧ activation < s.nextActivation ∧
    s.frames.head?.map Activation.id = some activation ∧
    ∃ a, loan s i = some a ∧
      ChildGrant s t a i path m activation false

def LoanStep (s : State) (e : LoanAction) (t : State) : Prop :=
  match e with
  | .borrow p path m => Borrow s p path m t
  | .reborrow i path m a => ReborrowTo s i path m a t
  | .endLoan i => EndLoan s i t

/-- Law (authority: docs/decisions.md#od15). Ending a leaf removes that loan alone. -/
theorem endLoan_iff (s : State) (i : LoanId) (t : State) :
    endLoan s i = some t ↔ EndLoan s i t := by
  by_cases allowed : OrdinaryLoanEnd s i <;>
    simp [endLoan, EndLoan, allowed, endLeafAtBoundary_iff]

/-- Law (authority: docs/decisions.md#od15). Borrow allocates the next loan identity. -/
theorem borrow_iff (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode)
    (t : State) : borrow s p path m = some t ↔ Borrow s p path m t := by
  cases h : s.frames.head? <;>
    simp [borrow, Borrow, h, Bool.and_eq_true, ownerAccess_iff, and_assoc, eq_comm (a := t)]

/-- Law (authority: docs/decisions.md#od15; #od23). Reborrow issues a downward child. -/
theorem reborrowTo_iff (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (a : Nat) (t : State) : reborrowTo s i path m a = some t ↔ ReborrowTo s i path m a t := by
  cases h : loan s i with
  | none => simp [reborrowTo, ReborrowTo, ChildGrant, h]
  | some parent =>
    simp [reborrowTo, ReborrowTo, ChildGrant, h, Bool.and_eq_true, loanAccess_iff,
      and_assoc, eq_comm (a := t)]

def Reborrow (s : State) (i : LoanId) (path : List Projection) (m : Mode) (t : State) : Prop :=
  ∃ f, s.frames.head? = some f ∧ ReborrowTo s i path m f.id t

/-- Law (authority: docs/decisions.md#od23). Reborrow targets the current frame. -/
theorem reborrow_iff (s : State) (i : LoanId) (path : List Projection) (m : Mode) (t : State) :
    reborrow s i path m = some t ↔ Reborrow s i path m t := by
  simp [reborrow, Reborrow, Option.bind_eq_some_iff, reborrowTo_iff]

/-- Law (authority: docs/decisions.md#od15). Each event realizes the corresponding loan rule. -/
theorem loanAction_iff (s : State) (e : LoanAction) (t : State) :
    loanAction s e = some t ↔ LoanStep s e t := by
  cases e <;> first | apply borrow_iff | apply reborrowTo_iff | apply endLoan_iff

/-- Helper (preservation). Legal loan actions keep the ledger well formed. -/
theorem loanStep_wellFormed {s t : State} {e : LoanAction}
    (h : LoanStep s e t) (hs : WellFormed s) : WellFormed t := by
  have step := (loanAction_iff s e t).mpr h
  cases e with
  | borrow p path m =>
    have safe := borrow_preserves s p path m hs
    change borrow s p path m = some t at step
    rwa [step] at safe
  | reborrow i path m a =>
    have safe := reborrowTo_preserves s i path m a hs
    change reborrowTo s i path m a = some t at step
    rwa [step] at safe
  | endLoan i =>
    have safe := endLoan_preserves s i hs
    change endLoan s i = some t at step
    rwa [step] at safe

end LegalStep

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
      (m = .mutable → accessLoan t i .shared = none) ∧
      (m = .shared → accessLoan t ⟨s.nextLoan⟩ .mutable = none ∧
        accessLoan t i .shared = some t) ∧
      ∃ l ∈ t.loans, l.id = ⟨s.nextLoan⟩ ∧ l.activation = current ∧
        ∃ a, loan s i = some a ∧ l.root = a.root ∧ l.path = a.path ++ path ∧
          l.parent = some i ∧ a ∈ t.loans ∧ l.mode = m

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
Downward lending creates a child in the current frame with the requested mode.
Shared children permit shared access through the parent while refusing child mutation.
Ending the child restores the previous loan table. Lending back to the caller is refused
(owner Q1, 2026-11-08). -/
theorem downward_reborrow_protocol (s : State) (i : LoanId) (path : List Projection)
    (m : Mode) (f : Activation) (caller : Nat) (a : Loan) (hs : WellFormed s)
    (hf : s.frames.head? = some f) (different : f.id ≠ caller)
    (found : loan s i = some a) (allowed : loanAccess s i m = true)
    (freshBinding : ∀ g ∈ s.frames, LoanId.mk s.nextLoan ∉ List.map Prod.snd g.views) :
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
    · refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
      · simp [endLoan, OrdinaryLoanEnd, endLeafAtBoundary, loan, ine, children, unchanged]
        refine ⟨fun b hb eq => (fresh b hb eq).elim, ?_⟩
        intro g hg x hx
        exact freshBinding g hg (List.mem_map.mpr ⟨(x, ⟨s.nextLoan⟩), hx, rfl⟩)
      · simp [endLoan, OrdinaryLoanEnd, endLeafAtBoundary, loan]
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
      · intro hm
        subst m
        constructor
        · simp [accessLoan, loanAccess, loan]
        · simp [loanAccess, loan, foundRaw] at allowed
          simp [accessLoan, loanAccess, loan, Ne.symm ine, foundRaw]
          refine ⟨allowed.1, ?_⟩
          intro b hb hi ha hr
          rcases allowed.2 b hb with ((h | h) | h) | h
          · exact False.elim (hi h)
          · exact False.elim (ha h)
          · exact False.elim (h hr)
          · exact h
      · exact ⟨_, List.mem_cons_self, rfl, rfl, a, found, rfl, rfl, rfl,
          List.mem_cons_of_mem _ am, rfl⟩

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
theorem downward_reborrow_protocol_instance_1 : DownwardProtocol reborrowTo LifecycleWitnesses.called ⟨1⟩ [] .mutable 1 0 :=
  @downward_reborrow_protocol LifecycleWitnesses.called ⟨1⟩ [] .mutable calleeFrame 0 source
    (by decide) rfl (by decide) rfl (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant
`ConcreteSpec.Core.Own.DownwardProtocolEvidence.allowUpward`). The exact upward request is now admitted. -/
theorem downward_reborrow_protocol_planted_1 : WellFormed LifecycleWitnesses.called ∧
    LifecycleWitnesses.called.frames.head? = some calleeFrame ∧ calleeFrame.id ≠ 0 ∧
    loan LifecycleWitnesses.called ⟨1⟩ = some source ∧
    loanAccess LifecycleWitnesses.called ⟨1⟩ .mutable = true ∧
    ¬ DownwardProtocol allowUpward LifecycleWitnesses.called ⟨1⟩ [] .mutable calleeFrame.id 0 := by
  refine ⟨by decide, rfl, by decide, rfl, by decide, ?_⟩
  intro h
  have hn := h.1
  cases hn

/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant
`ConcreteSpec.Core.Own.PreservationEvidence.reborrowToDropParent`). Reuse O1's body mutant
that removes only the caller's parent when creating a child. Exact restoration detects the loss. -/
theorem downward_reborrow_protocol_planted_2 : WellFormed LifecycleWitnesses.called ∧
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

/-- Instance (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`). The actual child
can be accessed while its parent is suspended; ending it restores that parent's access. -/
theorem downward_reborrow_protocol_instance_2 : ∃ t u,
    reborrowTo LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 = some t ∧
    accessLoan t ⟨2⟩ .mutable = some t ∧
    accessLoan t ⟨1⟩ .mutable = none ∧
    endLoan t ⟨2⟩ = some u ∧
    accessLoan u ⟨1⟩ .mutable = some u := by
  obtain ⟨_, t, ht, he, _, usable, suspended, _⟩ :=
    @downward_reborrow_protocol LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable
      calleeFrame 0 source (by decide) rfl (by decide) rfl (by decide) (by decide)
  exact ⟨t, _, ht, usable, suspended, he, by decide⟩


/-- Mutant: shared children alone may target an active caller. -/
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

/-- Mutant: a new child retains its pending bit and cannot be used. -/
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
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant
`PreservationEvidence.reborrowParentlessTo`). A populated request isolates the missing protocol clause. -/
theorem downward_reborrow_protocol_planted_3 : ¬ DownwardProtocol PreservationEvidence.reborrowParentlessTo
    LifecycleWitnesses.called ⟨1⟩ [] .mutable 1 0 := by
  rintro ⟨up, t, ht, rest⟩
  cases ht
  obtain ⟨l, member, id, _, _, _, _, _, parent, _, _⟩ := rest.2.2.2.2.2.2
  rcases List.mem_cons.mp member with rfl | old
  · contradiction
  · exact (show ∀ a ∈ LifecycleWitnesses.called.loans,
      a.id ≠ LoanId.mk LifecycleWitnesses.called.nextLoan from by decide) l old id
/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant
`sharedUp`). A populated request isolates the missing protocol clause. -/
theorem downward_reborrow_protocol_planted_4 : ¬ DownwardProtocol sharedUp LifecycleWitnesses.called ⟨1⟩ [] .shared 1 0 := by
  rintro ⟨up, t, ht, rest⟩
  cases up
/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant
`deadChild`). A populated request isolates the missing protocol clause. -/
theorem downward_reborrow_protocol_planted_5 : ¬ DownwardProtocol deadChild LifecycleWitnesses.called ⟨1⟩ [] .mutable 1 0 := by
  rintro ⟨up, t, ht, rest⟩
  cases ht
  have bad := rest.2.2.1
  cases bad
/-- Instance (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`). Shared lending has the same downward boundary. -/
theorem downward_reborrow_protocol_instance_3 : DownwardProtocol reborrowTo LifecycleWitnesses.called ⟨1⟩ [] .shared 1 0 :=
  @downward_reborrow_protocol LifecycleWitnesses.called ⟨1⟩ [] .shared calleeFrame 0 source
    (by decide) rfl (by decide) rfl (by decide) (by decide)
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
theorem move_reinitialize_protocol_instance_1 : MoveLifecycle move reinitialize source ⟨1⟩ :=
  @move_reinitialize_protocol source ⟨1⟩ selected (by decide) rfl rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.move_reinitialize_protocol`, case
`ConcreteSpec.Core.Own.MoveLifecycleEvidence.source`, subject `ConcreteSpec.Core.Own.move`, mutant
`ConcreteSpec.Core.Own.MoveLifecycleEvidence.keepOwned`). -/
theorem move_reinitialize_protocol_planted_1 : WellFormed source ∧ slot source ⟨1⟩ = some selected ∧ selected.desc.copy = false ∧
    selected.status = .owned ∧
    (∀ l ∈ source.loans, l.root ≠ Hybrid.Place.mk 1) ∧
    ¬ MoveLifecycle keepOwned reinitialize source ⟨1⟩ := by
  refine ⟨by decide, rfl, rfl, rfl, by decide, ?_⟩
  rintro ⟨t, ht, denied, _⟩
  cases ht
  cases denied


/-- Mutant: allow consumption a second time without reinitialization. -/
def doubleMove (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if x.desc.copy || s.loans.any (·.root == p) then none else
    some { s with slots := s.slots.map fun x =>
      if x.place = p then { x with status := .moved } else x }

/-- Mutant: permit replacing an owned mutable value without consumption. -/
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
theorem move_reinitialize_protocol_planted_2 : ¬ MoveLifecycle doubleMove reinitialize source ⟨1⟩ := by
  rintro ⟨t, ht, rest⟩
  cases ht
  have bad := rest.2.2.1
  cases bad
/-- Planted (law `ConcreteSpec.Core.Own.move_reinitialize_protocol`, case
`ConcreteSpec.Core.Own.MoveLifecycleEvidence.source`, subject `ConcreteSpec.Core.Own.reinitialize`,
mutant `ConcreteSpec.Core.Own.MoveLifecycleEvidence.overwrite`).
An added transition violates linearity even though ordinary post-move access is refused. -/
theorem move_reinitialize_protocol_planted_3 : ¬ MoveLifecycle move overwrite source ⟨1⟩ := by
  rintro ⟨t, ht, rest⟩
  cases ht
  have bad := rest.2.2.2.1
  cases bad
end MoveLifecycleEvidence

/-- Helper (inversion). Each successful loan action preserves retired identities. -/
private theorem loanAction_retains_retirement (s : State) (op : LoanAction) (i : LoanId)
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
      split at h <;> try contradiction
      unfold endLeafAtBoundary at h
      obtain ⟨_, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · contradiction
      · cases h
        exact ⟨bound, fun l hl => hr.2 l (List.mem_filter.mp hl).1⟩

/-- Static and ledger conditions for each loan action, independent of its dispatcher. -/
def AdmittedLoanAction (s : State) : LoanAction → Prop
  | .borrow p _ m => ∃ f, s.frames.head? = some f ∧
      f.id < s.nextActivation ∧ LegalStep.OwnerPermission s p m
  | .reborrow i _ m a => ∃ parent, loan s i = some parent ∧
      a < s.nextActivation ∧ s.frames.head?.map Activation.id = some a ∧ LegalStep.LoanPermission s i m
  | .endLoan i => ∃ a, loan s i = some a ∧ OrdinaryLoanEnd s i ∧ ∀ b ∈ s.loans, b.parent ≠ some i

/-- The history corollary exposes progress, retirement, and well-formedness alongside
the exact single-step Law. -/
def RetiringLoanAction (step : State → LoanAction → Option State)
    (s : State) (op : LoanAction) : Prop :=
  (∀ i, RetiredLoan s i → ResultSatisfies (fun t => RetiredLoan t i) (step s op)) ∧
  (AdmittedLoanAction s op → ∃ t, step s op = some t) ∧
  (∀ t, step s op = some t ↔ LegalStep.LoanStep s op t) ∧
  (WellFormed s → StateResult (step s op))

/-- Law (authority: docs/decisions.md#decided-2026-11-08-foundation, C5;
#od15; #decided-2026-11-05-core Q3.2–Q3.3). (subject `ConcreteSpec.Core.Own.loanAction`).
An admitted loan action completes. All produced results preserve retired identities. -/
theorem loanSequence_retired_step (s : State) (op : LoanAction) :
    RetiringLoanAction loanAction s op := by
  refine ⟨loanAction_retains_retirement s op, ?_, LegalStep.loanAction_iff s op, ?_⟩
  · intro admitted
    cases op with
    | borrow p path m =>
      obtain ⟨f, frame, issued, allowed⟩ := admitted
      exact borrow_progress s p path m f frame issued ((LegalStep.ownerAccess_iff ..).mpr allowed)
    | reborrow i path m a =>
      obtain ⟨parent, found, issued, current, allowed⟩ := admitted
      exact reborrowTo_progress s i path m a parent found issued current ((LegalStep.loanAccess_iff ..).mpr allowed)
    | endLoan i =>
      obtain ⟨a, found, allowed, children⟩ := admitted
      exact endLoan_progress s i a found allowed children
  · intro hs
    cases step : loanAction s op with
    | none => trivial
    | some t => exact LegalStep.loanStep_wellFormed ((LegalStep.loanAction_iff s op t).mp step) hs

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
      have hstep := (loanSequence_retired_step s op).1 i hr
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
theorem loanSequence_retired_step_instance_1 : RetiringLoanAction loanAction endedChild (.reborrow ⟨0⟩ [.index 2] .mutable 0) :=
  loanSequence_retired_step endedChild (.reborrow ⟨0⟩ [.index 2] .mutable 0)

/-- Mutant: refuse ending a loan while preserving both borrowing rules. -/
private def refuseEndLoan (s : State) : LoanAction → Option State
  | .endLoan _ => none
  | op => loanAction s op

/-- Planted (law `ConcreteSpec.Core.Own.loanSequence_retired_step`, case `endedChild`,
subject `ConcreteSpec.Core.Own.loanAction`, mutant `refuseEndLoan`). -/
theorem loanSequence_retired_step_planted_1 : AdmittedLoanAction endedChild (.endLoan ⟨0⟩) ∧
    ¬ RetiringLoanAction refuseEndLoan endedChild (.endLoan ⟨0⟩) := by
  have admitted : AdmittedLoanAction endedChild (.endLoan ⟨0⟩) :=
    ⟨_, rfl, by decide⟩
  refine ⟨admitted, ?_⟩
  intro contract
  obtain ⟨t, step⟩ := contract.2.1 admitted
  cases step

/-- Planted (law `ConcreteSpec.Core.Own.loanSequence_retired_step`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.loanAction`, mutant
`ConcreteSpec.Core.Own.LoanHistoryEvidence.recycleStep`). -/
theorem loanSequence_retired_step_planted_2 : ¬ RetiringLoanAction recycleStep endedChild (.reborrow ⟨0⟩ [.index 2] .mutable 0) := by
  intro law
  have safe := law.1 ⟨1⟩ (by decide)
  exact (by decide : ¬ ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
    (recycleStep endedChild (.reborrow ⟨0⟩ [.index 2] .mutable 0))) safe

/-- Instance (law `ConcreteSpec.Core.Own.ended_loans_never_revive`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.runLoanSequence`). The populated
state is well formed; the history succeeds with a live parent and retires its intermediate loan. -/
theorem ended_loans_never_revive_instance_1 : WellFormed endedChild ∧
    (runLoanSequence endedChild actions).isSome = true ∧
    (loan endedChild ⟨0⟩).isSome = true ∧
    ResultSatisfies (fun t => RetiredLoan t ⟨1⟩ ∧ RetiredLoan t ⟨2⟩ ∧
      (loan t ⟨3⟩).isSome = true) (runLoanSequence endedChild actions) := by
  refine ⟨by decide, by decide, by decide, ?_⟩
  exact ⟨ended_loans_never_revive endedChild actions ⟨1⟩ (by decide),
    by decide, by decide⟩

/-- Instance (law `ConcreteSpec.Core.Own.ended_loans_never_revive`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`). -/
theorem ended_loans_never_revive_instance_2 : ResultSatisfies (fun t => RetiredLoan t ⟨1⟩) (runLoanSequence endedChild actions) :=
  @ended_loans_never_revive endedChild actions ⟨1⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.ended_loans_never_revive`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.runLoanSequence`, mutant
`ConcreteSpec.Core.Own.LoanHistoryEvidence.recycle`). -/
theorem ended_loans_never_revive_planted_1 : RetiredLoan endedChild ⟨1⟩ ∧
    ¬ ResultSatisfies (fun t => RetiredLoan t ⟨1⟩) (recycle endedChild actions) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.loanSequence_retired_step`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.loanAction`). Both other
action arms succeed on real loans; retirement survives a birth on another root and ending the live parent. -/
theorem loanSequence_retired_step_instance_2 : RetiredLoan endedChild ⟨1⟩ ∧
    (loanAction endedChild (.borrow ⟨1⟩ [] .shared)).isSome = true ∧
    (loanAction endedChild (.endLoan ⟨0⟩)).isSome = true ∧
    ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
      (loanAction endedChild (.borrow ⟨1⟩ [] .shared)) ∧
    ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
      (loanAction endedChild (.endLoan ⟨0⟩)) := by
  exact ⟨by decide, by decide, by decide,
    (loanSequence_retired_step endedChild (.borrow ⟨1⟩ [] .shared)).1 ⟨1⟩ (by decide),
    (loanSequence_retired_step endedChild (.endLoan ⟨0⟩)).1 ⟨1⟩ (by decide)⟩

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

/-- Planted (law `ConcreteSpec.Core.Own.loanSequence_retired_step`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.loanAction`,
mutant `ConcreteSpec.Core.Own.LoanHistoryEvidence.recycleBorrowStep`). The retired identity is
reissued by an owner borrow, independently of the projected-reborrow mutation. -/
theorem loanSequence_retired_step_planted_3 : ¬ RetiringLoanAction recycleBorrowStep endedChild (.borrow ⟨1⟩ [] .shared) := by
  intro law
  have safe := law.1 ⟨1⟩ (by decide)
  exact (by decide : ¬ ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
    (recycleBorrowStep endedChild (.borrow ⟨1⟩ [] .shared))) safe

/-- Instance (law `ConcreteSpec.Core.Own.ended_loans_never_revive`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.runLoanSequence`). Both kinds
of birth remain fresh after an intermediate child ends. -/
theorem ended_loans_never_revive_instance_3 : (runLoanSequence endedChild borrowActions).isSome = true ∧
    ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
      (runLoanSequence endedChild borrowActions) := by
  exact ⟨by decide, ended_loans_never_revive endedChild borrowActions ⟨1⟩ (by decide)⟩

/-- Planted (law `ConcreteSpec.Core.Own.ended_loans_never_revive`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.runLoanSequence`,
mutant `ConcreteSpec.Core.Own.LoanHistoryEvidence.recycleBorrowHistory`). The borrow mutation is
substituted in every action, and the successful multi-action history revives identity 1. -/
theorem ended_loans_never_revive_planted_2 : RetiredLoan endedChild ⟨1⟩ ∧
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

/-- Planted (law `ConcreteSpec.Core.Own.loanSequence_retired_step`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.loanAction`,
mutant `ConcreteSpec.Core.Own.LoanHistoryEvidence.rewindStep`). Ending a real parent
must retain the fact that the retired child's identity was already issued. -/
theorem loanSequence_retired_step_planted_4 : ¬ RetiringLoanAction rewindStep endedChild (.endLoan ⟨0⟩) := by
  intro law
  have safe := law.1 ⟨1⟩ (by decide)
  exact (by decide : ¬ ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
    (rewindStep endedChild (.endLoan ⟨0⟩))) safe

end LoanHistoryEvidence

/-- A place identity was issued and its storage has been retired. This is independent of
whether another slot happens to carry equal concrete bytes. -/
def RetiredPlace (s : State) (p : Hybrid.Place) : Prop :=
  p.id < s.nextPlace ∧ ∀ x ∈ s.slots, x.place ≠ p
instance (s : State) (p : Hybrid.Place) : Decidable (RetiredPlace s p) := by
  unfold RetiredPlace
  infer_instance

/-- Helper (inversion). Successful fresh binding and reinitialization preserve retirement. -/
private theorem bind_retains_retirement (types : Var → Option Descriptor) (s : State) (x : Var)
    (p : Hybrid.Place) (hr : RetiredPlace s p) :
    ResultSatisfies (fun t => RetiredPlace t p) (bind types x s) := by
  have bound := hr.1
  cases h : bind types x s with
  | none => trivial
  | some t =>
    unfold bind at h
    obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h <;> try contradiction
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

/-- Independent binding admission. Fresh locals need a descriptor and an issued frame.
Existing locals need an available slot with no live loans into it. -/
def AdmittedBinding (types : Var → Option Descriptor) (s : State) (x : Var)
    (f : Activation) : Prop :=
  s.frames.head? = some f ∧ ParameterAvailable f.views x ∧
    match AList.lookup f.locals x with
    | none => f.id < s.nextActivation ∧ ∃ d, types x = some d
    | some p => ∃ a, slot s p = some a ∧
        (a.status = .moved ∨ (a.desc.copy = true ∧ a.desc.mutable = true)) ∧
        ∀ l ∈ s.loans, l.root ≠ p

/-- A binding completes and keeps a retired place unavailable. -/
def FreshBinding (bindStep : Var → State → Option State) (s : State) (x : Var) : Prop :=
  ∃ t, bindStep x s = some t ∧ ∀ p, RetiredPlace s p → RetiredPlace t p

/-- Law (authority: docs/decisions.md#foundation-q1 C5; #decided-2026-11-05-core Q3.3, Q3.8;
#od69; #decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.bind`).
An admitted binding succeeds and preserves every retired place through allocation or replacement. -/
theorem bind_retired_place (types : Var → Option Descriptor) (s : State) (x : Var)
    (f : Activation) (admitted : AdmittedBinding types s x f) :
    FreshBinding (bind types) s x := by
  obtain ⟨frame, freeView, admissibleSlot⟩ := admitted
  have progress : ∃ t, bind types x s = some t := by
    cases found : AList.lookup f.locals x with
    | none =>
      rw [found] at admissibleSlot
      obtain ⟨issued, d, typed⟩ := admissibleSlot
      exact bind_fresh_progress types s x f d frame issued found freeView typed
    | some q =>
      rw [found] at admissibleSlot
      obtain ⟨a, current, available, clear⟩ := admissibleSlot
      have free : x ∉ List.map Prod.fst f.views := freeView
      simpa [bind, frame, found, free] using reinitialize_progress s q a current available clear
  obtain ⟨t, step⟩ := progress
  refine ⟨t, step, fun p hr => ?_⟩
  have keep := bind_retains_retirement types s x p hr
  rw [step] at keep
  exact keep

/-- Storage selected from the current activation by the scope's departing binders. -/
def departingPlaces (f : Activation) (xs : List Var) : List Hybrid.Place :=
  (f.locals.filter (fun e => decide (e.1 ∈ xs))).map Prod.snd

/-- The current frame's selected storage can be removed once discharged and loan-free. -/
def AdmittedDrop (s : State) (xs : List Var) (f : Activation) : Prop :=
  s.frames.head? = some f ∧
  (∀ a ∈ s.slots, a.place ∈ departingPlaces f xs →
    a.desc.copy = true ∨ a.status = .moved) ∧
  (∀ l ∈ s.loans, l.root ∉ departingPlaces f xs)

instance (s : State) (xs : List Var) (f : Activation) : Decidable (AdmittedDrop s xs f) := by
  unfold AdmittedDrop departingPlaces
  infer_instance

/-- Helper (inversion). Teardown cannot revive a retired place. -/
private theorem drop_retains_retirement (s : State) (xs : List Var) (p : Hybrid.Place)
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

/-- Teardown succeeds and keeps the retired place unavailable. -/
def RetiringDrop (remove : List Var → State → Option State) (s : State) (xs : List Var) : Prop :=
  ∃ t, remove xs s = some t ∧ ∀ p, RetiredPlace s p → RetiredPlace t p

/-- Law (authority: docs/decisions.md#foundation-q1 C5; #od69;
#decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.drop`).
Admitted teardown succeeds and preserves retired storage identities. -/
theorem drop_retired_place (s : State) (xs : List Var)
    (f : Activation) (admitted : AdmittedDrop s xs f) : RetiringDrop drop s xs := by
  obtain ⟨frame, discharged, clear⟩ := admitted
  obtain ⟨t, step⟩ := drop_progress s xs f frame discharged clear
  refine ⟨t, step, fun p hr => ?_⟩
  have keep := drop_retains_retirement s xs p hr
  rw [step] at keep
  exact keep

/-- Helper (inversion). Direct argument evaluation cannot recycle a retired identity. -/
private theorem passOwner_retains_retirement (s : CallState) (p : Hybrid.Place) (path : List Projection)
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

/-- Independent admission for a direct owner argument. -/
def AdmittedOwnerArgument (s : CallState) (p : Hybrid.Place) (m : Mode) (x : Var) : Prop :=
  x ∉ s.pending.map Prod.fst ∧ LegalStep.OwnerPermission s.own p m ∧
  ∃ f, s.own.frames.head? = some f ∧ f.id < s.own.nextActivation

/-- Direct lending produces a result which keeps the retired identity unavailable. -/
def FreshOwnerArgument (pass : CallState → Hybrid.Place → List Projection → Mode → Var →
    Option CallState) (s : CallState) (p : Hybrid.Place) (path : List Projection) (m : Mode)
    (x : Var) : Prop :=
  ∃ t, pass s p path m x = some t ∧ ∀ i, RetiredLoan s.own i → RetiredLoan t.own i

/-- Law (authority: docs/decisions.md#foundation-q1 C5; #od2; #od15;
#decided-2026-11-08-recovered-d3, call-duration loans).
(subject `ConcreteSpec.Core.Own.passOwner`). Every admitted direct owner argument
succeeds and leaves each retired loan identity retired. -/
theorem call_birth_retired (s : CallState) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (x : Var) (admitted : AdmittedOwnerArgument s p m x) :
    FreshOwnerArgument passOwner s p path m x := by
  obtain ⟨free, access, f, frame, issued⟩ := admitted
  have progress : ∃ t, passOwner s p path m x = some t := by
    simp [passOwner, free, (LegalStep.ownerAccess_iff ..).mpr access, frame, issued]
  obtain ⟨t, step⟩ := progress
  refine ⟨t, step, fun i hr => ?_⟩
  have keep := passOwner_retains_retirement s p path m x i hr
  rw [step] at keep
  exact keep

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

/-- Instance (admission for the corresponding retirement fixture). -/
private theorem retiredBindingAdmitted :
    AdmittedBinding types retired x (retired.frames.headD ⟨0, [], []⟩) :=
  ⟨rfl, by decide, by exact ⟨by decide, _, rfl⟩⟩

/-- Instance (law `ConcreteSpec.Core.Own.bind_retired_place`, case `retired`). -/
theorem bind_retired_place_instance_1 : FreshBinding (bind types) retired x :=
  bind_retired_place types retired x _ retiredBindingAdmitted

/-- Planted (law `ConcreteSpec.Core.Own.bind_retired_place`, case `retired`,
subject `ConcreteSpec.Core.Own.bind`, mutant `recycleBind`). -/
theorem bind_retired_place_planted_1 : ¬ FreshBinding (recycleBind types) retired x := by
  rintro ⟨t, step, keep⟩
  cases step
  have retiredZero := keep ⟨0⟩ (by decide)
  revert retiredZero
  decide

/-- Mutant: refuse fresh bindings while allowing reinitialization. -/
private def refuseFreshBind (types : Var → Option Descriptor) (x : Var)
    (s : State) : Option State := do
  let f ← s.frames.head?
  if (AList.lookup f.locals x).isSome then bind types x s else none

/-- Planted (law `ConcreteSpec.Core.Own.bind_retired_place`, case `retired`,
subject `ConcreteSpec.Core.Own.bind`, mutant `refuseFreshBind`). -/
theorem bind_retired_place_planted_2 : AdmittedBinding types retired x (retired.frames.headD ⟨0, [], []⟩) ∧
    ¬ FreshBinding (refuseFreshBind types) retired x := by
  refine ⟨retiredBindingAdmitted, ?_⟩
  rintro ⟨t, step, _⟩
  cases step

/-- Instance (law `ConcreteSpec.Core.Own.drop_retired_place`, case
`ConcreteSpec.Core.Own.RetirementEvidence.retired`). -/
theorem drop_retired_place_instance_1 : RetiringDrop drop retired [] :=
  drop_retired_place retired [] _ (by decide :
    AdmittedDrop retired [] (retired.frames.headD ⟨0, [], []⟩))

/-- Planted (law `ConcreteSpec.Core.Own.drop_retired_place`, case
`ConcreteSpec.Core.Own.RetirementEvidence.retired`, subject `ConcreteSpec.Core.Own.drop`,
mutant `ConcreteSpec.Core.Own.RetirementEvidence.rewindDrop`). -/
theorem drop_retired_place_planted_1 : ¬ RetiringDrop rewindDrop retired [] := by
  rintro ⟨t, step, keep⟩
  cases step
  have retiredZero := keep ⟨0⟩ (by decide)
  revert retiredZero
  decide

private def departingLocal := LifecycleWitnesses.moved

/-- Mutant: decline a teardown of nonempty storage. -/
private def refuseStorageDrop : List Var → State → Option State
  | [], s => drop [] s
  | _ :: _, _ => none

/-- Planted (law `ConcreteSpec.Core.Own.drop_retired_place`, case `departingLocal`,
subject `ConcreteSpec.Core.Own.drop`, mutant `refuseStorageDrop`). -/
theorem drop_retired_place_planted_2 : AdmittedDrop departingLocal [x] (departingLocal.frames.headD ⟨0, [], []⟩) ∧
    ¬ RetiringDrop refuseStorageDrop departingLocal [x] := by
  refine ⟨by decide, ?_⟩
  rintro ⟨t, step, _⟩
  cases step

private def ownerArgumentSource : CallState := ⟨LoanHistoryEvidence.endedChild, []⟩

/-- Instance (admission for the corresponding retirement fixture). -/
private theorem ownerArgumentAdmitted :
    AdmittedOwnerArgument ownerArgumentSource ⟨1⟩ .mutable DirectArgumentWitnesses.z :=
  ⟨by decide, ⟨_, rfl, rfl, Or.inr rfl, by decide⟩, _, rfl, by decide⟩

/-- Instance (law `ConcreteSpec.Core.Own.call_birth_retired`, case `ownerArgumentSource`). -/
theorem call_birth_retired_instance_1 : FreshOwnerArgument passOwner ownerArgumentSource ⟨1⟩ [] .mutable
    DirectArgumentWitnesses.z :=
  call_birth_retired _ _ _ _ _ ownerArgumentAdmitted

/-- Planted (law `ConcreteSpec.Core.Own.call_birth_retired`, case `ownerArgumentSource`,
subject `ConcreteSpec.Core.Own.passOwner`, mutant `recyclePass`). -/
theorem call_birth_retired_planted_1 : ¬ FreshOwnerArgument recyclePass ownerArgumentSource ⟨1⟩ [] .mutable
    DirectArgumentWitnesses.z := by
  rintro ⟨t, step, keep⟩
  cases step
  have retiredOne := keep ⟨1⟩ (by decide)
  revert retiredOne
  decide

/-- Mutant: refuse direct mutable lending while preserving direct shared lending. -/
private def refuseMutableOwnerArgument (s : CallState) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (x : Var) : Option CallState :=
  match m with
  | .mutable => none
  | .shared => passOwner s p path m x

/-- Planted (law `ConcreteSpec.Core.Own.call_birth_retired`, case `ownerArgumentSource`,
subject `ConcreteSpec.Core.Own.passOwner`, mutant `refuseMutableOwnerArgument`). -/
theorem call_birth_retired_planted_2 : AdmittedOwnerArgument ownerArgumentSource ⟨1⟩ .mutable DirectArgumentWitnesses.z ∧
    ¬ FreshOwnerArgument refuseMutableOwnerArgument ownerArgumentSource ⟨1⟩ [] .mutable
      DirectArgumentWitnesses.z := by
  refine ⟨ownerArgumentAdmitted, ?_⟩
  rintro ⟨t, step, _⟩
  cases step
end RetirementEvidence

/-- The existing call operands, with one uniformly substituted lifecycle callback. -/
abbrev protocolOps (lifecycle : Exec.Life → CallState → Option CallState) : Exec.Operands :=
  { Examples.callOps with life := lifecycle }

/-- A prefix certificate relates the actual before/after states to ownership operations.
The protected call loan cannot be explicitly ended, and a nested return must leave a different
activation. These are checker admission premises; this is not a checker implementation. -/
inductive CalleeEffect (lifecycle : Exec.Life → CallState → Option CallState)
    (held : Loan) : CallState → CallState → Prop where
  | quiet {s} : CalleeEffect lifecycle held s s
  | consume {s t p} (h : move s.own p = some t) :
      CalleeEffect lifecycle held s { s with own := t }
  | replace {s t p} (h : reinitialize s.own p = some t) :
      CalleeEffect lifecycle held s { s with own := t }
  | borrow {s t p path m} (h : borrow s.own p path m = some t) :
      CalleeEffect lifecycle held s { s with own := t }
  | reborrow {s t i path m a} (h : reborrowTo s.own i path m a = some t) :
      CalleeEffect lifecycle held s { s with own := t }
  | endOther {s t i} (different : held.id ≠ i) (h : endLoan s.own i = some t) :
      CalleeEffect lifecycle held s { s with own := t }
  | ownerArgument {s t p path m x} (h : passOwner s p path m x = some t) :
      CalleeEffect lifecycle held s t
  | viewArgument {s t i x} (h : passDown s i x = some t) : CalleeEffect lifecycle held s t
  | compose {s u t} (first : CalleeEffect lifecycle held s u)
      (wf : WellFormed u.own) (second : CalleeEffect lifecycle held u t) :
      CalleeEffect lifecycle held s t
  | life {s t e} (h : lifecycle e s = some t)
      (notReturn : e = .leave → ∃ f, s.own.frames.head? = some f ∧ held.activation ≠ f.id)
      (nestedEntry : ∀ ps, e = .enter ps → held.id ∉ s.pending.map Prod.snd) : CalleeEffect lifecycle held s t

/-- An arbitrary finite prefix of the real machine, with explicit ownership-effect and
well-formedness certificates at each transition. No fuel bound, skip-body assumption, or
loan-liveness/freeze conclusion is among the premises. Nested entries use the substituted
lifecycle callback and clear pending arguments. Owner consumption/reinitialization use
the actual operations; quiet effects change no store and cannot bypass lifecycle admission.
Births and endings have separate constructors. Nested entry must not adopt the already
adopted protected loan identity. The checker must supply the certificates. -/
inductive AdmittedCalleePrefix (lifecycle : Exec.Life → CallState → Option CallState)
    (P : Exec.Program (protocolOps lifecycle)) (held : Loan) :
    Exec.Config (protocolOps lifecycle) → Exec.Config (protocolOps lifecycle) → Prop where
  | refl (c) : AdmittedCalleePrefix lifecycle P held c c
  | next {a b c} (trace : AdmittedCalleePrefix lifecycle P held a b)
      (wf : WellFormed b.store.own)
      (step : Exec.step P b = some (.next c))
      (effect : CalleeEffect lifecycle held b.store c.store) :
      AdmittedCalleePrefix lifecycle P held a c

/-- Helper (inversion). Every admitted lifecycle event preserves a caller's protected
loan: local storage removal never edits loans, and nested leave retains caller loans. -/
private theorem callee_life_retains (types : Var → Option Descriptor) (s t : CallState)
    (held : Loan) (hs : WellFormed s.own) (hl : held ∈ s.own.loans) (e : Exec.Life)
    (h : callLife types e s = some t)
    (notReturn : e = .leave → ∃ f, s.own.frames.head? = some f ∧ held.activation ≠ f.id)
    (nestedEntry : ∀ ps, e = .enter ps → held.id ∉ s.pending.map Prod.snd) : held ∈ t.own.loans := by
  cases e with
  | enter ps =>
    simp only [callLife] at h
    split at h
    · obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
      obtain ⟨v, hv, h⟩ := Option.bind_eq_some_iff.mp h
      cases h
      have hlu : held ∈ u.loans := by rwa [enter_loans _ _ _ _ hu]
      unfold adoptDown at hv
      obtain ⟨f, _, hv⟩ := Option.bind_eq_some_iff.mp hv
      split at hv
      · cases hv
        exact List.mem_map.mpr ⟨held, hlu, by simp [nestedEntry ps rfl]⟩
      · contradiction
    · contradiction
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
      split at hu <;> try contradiction
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
  | quiet => exact hl
  | consume h =>
    unfold move at h
    obtain ⟨x, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · contradiction
    · cases h; exact hl
  | replace h =>
    unfold reinitialize at h
    obtain ⟨x, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h; exact hl
    · contradiction
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
    split at h <;> try contradiction
    unfold endLeafAtBoundary at h
    obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · contradiction
    · cases h; exact List.mem_filter.mpr ⟨hl, by simpa using different⟩
  | ownerArgument h =>
    unfold passOwner at h
    split at h
    · contradiction
    · obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h; exact List.mem_cons_of_mem _ hl
      · contradiction
  | viewArgument h =>
    unfold passDown at h
    split at h
    · contradiction
    · obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
      obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h; exact List.mem_cons_of_mem _ hl
      · contradiction
  | compose first wf second ih₁ ih₂ => exact ih₂ wf (ih₁ hs hl)
  | life h notReturn nestedEntry =>
    exact callee_life_retains types _ _ held hs hl _ h notReturn nestedEntry

/-- Lifetime conclusion at a callee trace. Mutable owner access is frozen for every loan;
shared owner access is frozen when the loan is mutable. A parent may be suspended while it
has lent another child; C5's downward protocol governs that suspension. -/
def HeldAt (s : CallState) (held : Loan) : Prop :=
  held ∈ s.own.loans ∧ ownerAccess s.own held.root .mutable = false ∧
    (held.mode = .mutable → ownerAccess s.own held.root .shared = false)

/-- The universal trace/freeze contract, with uniformly substituted lifecycle and permission
operations for independent negative controls. -/
def DeferredLifetime (lifecycle : Exec.Life → CallState → Option CallState)
    (permit : State → Hybrid.Place → Mode → Bool) : Prop :=
  ∀ (P : Exec.Program (protocolOps lifecycle)) (held : Loan)
    (a b : Exec.Config (protocolOps lifecycle)),
    held ∈ a.store.own.loans → AdmittedCalleePrefix lifecycle P held a b →
    held ∈ b.store.own.loans ∧ permit b.store.own held.root .mutable = false ∧
      (held.mode = .mutable → permit b.store.own held.root .shared = false)

/-- Helper (trace induction). Proves the loan and freeze postconditions from actual effects. -/
private theorem deferred_prefix_result (types : Var → Option Descriptor)
    (P : Exec.Program (protocolOps (callLife types))) (held : Loan)
    (a b : Exec.Config (protocolOps (callLife types)))
    (live : held ∈ a.store.own.loans)
    (trace : AdmittedCalleePrefix (callLife types) P held a b) : HeldAt b.store held := by
  have retained : held ∈ b.store.own.loans := by
    induction trace with
    | refl => exact live
    | next trace wf _ effect ih => exact callee_effect_retains types _ _ held wf ih effect
  have blocks (m : Mode) (conflict : m = .mutable ∨ held.mode = .mutable) :
      (b.store.own.loans.all (fun l =>
        l.root != held.root || (m == Mode.shared && l.mode == Mode.shared))) = false := by
    apply List.all_eq_false.mpr
    refine ⟨held, retained, ?_⟩
    rcases conflict with rfl | h
    · simp
    · cases m <;> simp [h]
  have freeze (m : Mode) (conflict : m = .mutable ∨ held.mode = .mutable) :
      ownerAccess b.store.own held.root m = false := by
    cases found : slot b.store.own held.root with
    | none => simp only [ownerAccess, found, Option.any_none]
    | some x => simp only [ownerAccess, found, Option.any_some, blocks m conflict, Bool.and_false]
  exact ⟨retained, freeze .mutable (Or.inl rfl), fun h => freeze .shared (Or.inr h)⟩

/-- Law (authority: docs/decisions.md#decided-2026-11-08-recovered-d3, C5 question 2;
#od2; #od15; #od69). (subject `ConcreteSpec.Core.Own.callLife`). Every arbitrary admitted
callee trace retains its shared or mutable call loan and freezes the owner. The actual machine body
and trace length are unrestricted; the explicit effect certificates exclude ending that
loan or leaving its activation before the boundary. This conditional Law does not claim
that arbitrary unchecked bodies satisfy the certificates. -/
theorem deferred_call_lifetime (types : Var → Option Descriptor) :
    DeferredLifetime (callLife types) ownerAccess :=
  deferred_prefix_result types

/-- Law (authority: docs/decisions.md#decided-2026-11-05-core Q9.8;
#foundation-q1 C1). (subject `ConcreteSpec.Core.Exec.step`). Whenever the independent
registration rule admits a defer, the dispatcher queues the original operands in the
innermost scope, leaving locals and store intact and deferring operand reads to cleanup. -/
theorem deferred_registration (O : Exec.Operands) (P : Exec.Program O)
    (f : FnId) (args : List O.Expr) (site : SiteId) (L : AList Var O.Val)
    (s : O.Store) (k k' : List (Exec.Frame O))
    (h : Exec.Registered ⟨f, args, site⟩ k k') :
    Exec.step P ⟨.exec (.defer f args site), L, s, k⟩ = some (.next ⟨.done, L, s, k'⟩) := by
  cases Exec.rules_sound P _ none .silent _ (.defer h) with
  | silent hs => exact hs

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
  unfold endLeafAtBoundary at hv
  obtain ⟨b, _, hv⟩ := Option.bind_eq_some_iff.mp hv
  split at hv
  · contradiction
  · cases hv; rfl

/-- Helper (list induction). A closed, newest-first loan graph can be unwound. -/
private theorem fold_end_progress (todo : List Loan) (s : State)
    (present : ∀ a ∈ todo, a ∈ s.loans)
    (order : todo.Pairwise (fun a b => b.id.id < a.id.id))
    (closed : ∀ a ∈ todo, ∀ b ∈ s.loans, b.parent = some a.id →
      a.id.id < b.id.id ∧ b ∈ todo) :
    ∃ t, todo.foldlM (fun s a => endLeafAtBoundary s a.id) s = some t := by
  induction todo generalizing s with
  | nil => exact ⟨s, rfl⟩
  | cons a rest ih =>
    have ord := List.pairwise_cons.mp order
    have children : ∀ b ∈ s.loans, b.parent ≠ some a.id := by
      intro b hb he
      obtain ⟨older, mem⟩ := closed a (by simp) b hb he
      rcases List.mem_cons.mp mem with eq | mem
      · subst b; omega
      · have := ord.1 b mem; omega
    have found : ∃ b, loan s a.id = some b := by
      cases h : loan s a.id with
      | some b => exact ⟨b, rfl⟩
      | none =>
        have := List.find?_eq_none.mp h a (present a (by simp))
        simp at this
    obtain ⟨b, hb⟩ := found
    have noChild : s.loans.any (·.parent == some a.id) = false := by
      apply Bool.eq_false_iff.mpr
      intro h
      obtain ⟨c, hc, he⟩ := List.any_eq_true.mp h
      exact children c hc (by simpa using he)
    let u := { s with loans := s.loans.filter (·.id != a.id) }
    have step : endLeafAtBoundary s a.id = some u := by simp [endLeafAtBoundary, hb, noChild, u]
    have retained : ∀ c ∈ rest, c ∈ u.loans := by
      intro c hc
      have lower := ord.1 c hc
      have ne : c.id ≠ a.id := by intro h; rw [h] at lower; omega
      exact List.mem_filter.mpr ⟨present c (by simp [hc]), by simpa using ne⟩
    obtain ⟨t, ht⟩ := ih u retained ord.2 (by
      intro c hc d hd he
      have hd' := (List.mem_filter.mp hd).1
      obtain ⟨older, mem⟩ := closed c (by simp [hc]) d hd' he
      refine ⟨older, ?_⟩
      rcases List.mem_cons.mp mem with eq | mem
      · subst d
        have bad := (List.mem_filter.mp hd).2
        simp at bad
      · exact mem)
    exact ⟨t, by simp [List.foldlM_cons, step, ht]⟩
/-- Structural return admission: birth order, no caller child of a callee loan,
linear locals discharged, and surviving loans do not point into callee storage.
These are input graph/storage properties, not assumptions of successful execution. -/
def ValidReturn (s : State) (f : Activation) : Prop :=
  s.loans.Pairwise (fun a b => b.id.id < a.id.id) ∧
  (∀ a ∈ s.loans, a.activation = f.id → ∀ b ∈ s.loans,
    b.parent = some a.id → b.activation = f.id) ∧
  (∀ x ∈ s.slots, x.place ∈ List.map Prod.snd f.locals →
    x.desc.copy = true ∨ x.status = .moved) ∧
  (∀ b ∈ s.loans, b.activation ≠ f.id → b.root ∉ List.map Prod.snd f.locals)

instance (s : State) (f : Activation) : Decidable (ValidReturn s f) := by
  unfold ValidReturn
  infer_instance

/-- Non-vacuous boundary progress, shared by both return restoration Laws. -/
def ReturnProgress (depart : State → Option State) (s : State) : Prop :=
  ∃ t, depart s = some t

/-- Law (authority: docs/decisions.md#od2; #decided-2026-11-13-lifecycle-refusal;
#decided-2026-11-08-recovered-d3 C5 Q2). (subject `ConcreteSpec.Core.Own.leave`).
Every structurally valid return succeeds, including plain lending with arbitrarily many
local descendants. Both return-restoration Laws use this progress result. -/
theorem return_progress (s : State) (f : Activation) (hs : WellFormed s)
    (hf : s.frames.head? = some f) (valid : ValidReturn s f) : ReturnProgress leave s := by
  let todo := s.loans.filter (·.activation == f.id)
  obtain ⟨u, hu⟩ := fold_end_progress todo s
    (fun a ha => (List.mem_filter.mp ha).1) (valid.1.filter _) (by
      intro a ha b hb he
      have ham := (List.mem_filter.mp ha).1
      have haa : a.activation = f.id := by simpa using (List.mem_filter.mp ha).2
      have hp := (hs.2.2.2.1 b hb).2.2.2.1
      unfold ParentValid at hp
      rw [he] at hp
      obtain ⟨p, hm, hid, _, older, _⟩ := hp
      have eq := loan_unique s.loans hs.2.1 p a hm ham hid
      subst p
      exact ⟨older, List.mem_filter.mpr ⟨hb, by simpa using valid.2.1 a ham haa b hb he⟩⟩)
  have slots := endLocals_slots f.id s u hu
  have frames : u.frames = s.frames :=
    fold_projection State.frames _ (fun a l b hb => endLeafAtBoundary_frames a b l.id hb) _ s u hu
  have remaining := endLoans_removed todo s u hu
  obtain ⟨v, hv⟩ := drop_progress u (List.map Prod.fst f.locals) f (by rwa [frames]) (by
    intro x hx hp
    apply valid.2.2.1 x (by rwa [slots] at hx)
    exact List.mem_map.mpr (by
      obtain ⟨e, he, heq⟩ := List.mem_map.mp hp
      exact ⟨e, (List.mem_filter.mp he).1, heq⟩)) (by
    intro b hb
    obtain ⟨old, absent⟩ := remaining b hb
    have outer : b.activation ≠ f.id := by
      intro he
      exact absent b (List.mem_filter.mpr ⟨old, by simpa using he⟩) rfl
    intro hp
    apply valid.2.2.2 b old outer
    obtain ⟨e, he, heq⟩ := List.mem_map.mp hp
    exact List.mem_map.mpr ⟨e, (List.mem_filter.mp he).1, heq⟩)
  have hu' : endLocals f.id s = some u := hu
  exact ⟨{ v with frames := v.frames.tail }, by simp [leave, hf, hu', hv]⟩

/-- The complete return-boundary postcondition, independent of the callee's body length. -/
def ReturnedCall (depart : State → Option State) (s : State) (f : Activation)
    (held : Loan) (root : Hybrid.Place) : Prop :=
  ∃ t, depart s = some t ∧
    (∀ l ∈ t.loans, l.activation ≠ f.id) ∧
    (∀ l ∈ s.loans, l.activation ≠ f.id → l ∈ t.loans) ∧
    loan t held.id = none ∧ ownerAccess t root .mutable = true

/-- Law (authority: docs/decisions.md#decided-2026-11-08-recovered-d3 C5 question 2;
#od2; #od15; #od69). (subject `ConcreteSpec.Core.Own.leave`). For any admitted callee
endpoint satisfying `ValidReturn`, return succeeds and ends the callee's loans, preserves
every caller loan and restores a caller-owned mutable root. The explicit premises require the root still owned outside the
callee and no other activation's loan on that root. Refusal is not normal return. -/
theorem deferred_return_lifetime (s : State) (f : Activation) (held : Loan) (x : Slot)
    (hs : WellFormed s) (hf : s.frames.head? = some f) (valid : ValidReturn s f)
    (live : held ∈ s.loans) (activation : held.activation = f.id)
    (root : held.root = x.place) (found : slot s x.place = some x)
    (outer : x.activation ≠ f.id) (owned : x.status = .owned) (mutable : x.desc.mutable = true)
    (exclusive : ∀ l ∈ s.loans, l.root = x.place → l.activation = f.id) :
    ReturnedCall leave s f held held.root := by
  rw [root]
  obtain ⟨t, h⟩ := return_progress s f hs hf valid
  refine ⟨t, h, ?_⟩
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
    fold_projection State.frames _ (fun a l b hb => endLeafAtBoundary_frames a b l.id hb) _ s u hu
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

/-- A successful callee return preserves every caller loan, ends every callee loan, and
restores access through a caller view, including a parent with the same root. -/
def ReturnedView (depart : State → Option State) (s : State) (f : Activation)
    (parent : Loan) (m : Mode) : Prop :=
  ∃ t, depart s = some t ∧ (∀ l ∈ t.loans, l.activation ≠ f.id) ∧
    (∀ l ∈ s.loans, l.activation ≠ f.id → l ∈ t.loans) ∧
    parent ∈ t.loans ∧ loanAccess t parent.id m = true

/-- Law (authority: docs/decisions.md#od2; #od15; #od23;
#decided-2026-11-08-recovered-d3 C5 Q2). (subject `ConcreteSpec.Core.Own.leave`).
An admitted return satisfying `ValidReturn` ends plain lending and restores the caller's
parent view. Other surviving loans may be ancestors or compatible shared loans.
The premises describe an available caller view with all conflicting children in the callee. -/
theorem deferred_return_view (s : State) (f : Activation) (parent : Loan) (m : Mode)
    (hs : WellFormed s) (hf : s.frames.head? = some f) (valid : ValidReturn s f) (live : parent ∈ s.loans)
    (outer : parent.activation ≠ f.id) (ready : parent.pendingArgument = false)
    (mode : m = .shared ∨ parent.mode = .mutable)
    (clear : ∀ b ∈ s.loans, b.activation ≠ f.id → b.id = parent.id ∨
      b.id ∈ parent.ancestors ∨ b.root ≠ parent.root ∨ (m = .shared ∧ b.mode = .shared)) :
    ReturnedView leave s f parent m := by
  obtain ⟨t, h⟩ := return_progress s f hs hf valid
  refine ⟨t, h, ?_⟩
  have removed := leave_no_activation_loan s f hf
  rw [h] at removed
  have keep (l : Loan) (hl : l ∈ s.loans) (ha : l.activation ≠ f.id) : l ∈ t.loans := by
    have k := leave_retains_caller s f l hs hf hl ha
    rwa [h] at k
  have retained := keep parent live outer
  have subset := life_subset (fun _ => none) .leave s t h
  have found : loan t parent.id = some parent := by
    cases he : loan t parent.id with
    | none =>
      have absent := List.find?_eq_none.mp he parent retained
      simp at absent
    | some b =>
      have hb := List.mem_of_find?_eq_some he
      have hid := List.find?_some he
      have eq := loan_unique s.loans hs.2.1 b parent (subset b hb) live (by simpa using hid)
      simp [eq]
  refine ⟨removed, keep, retained, ?_⟩
  have compatible : t.loans.all (fun b => b.id == parent.id ||
      decide (b.id ∈ parent.ancestors) || b.root != parent.root ||
      (m == .shared && b.mode == .shared)) = true := by
    apply List.all_eq_true.mpr
    intro b hb
    rcases clear b (subset b hb) (removed b hb) with h | h | h | ⟨h, hb⟩ <;> simp_all
  simp only [loanAccess, found, Option.any_some, ready, Bool.not_false, Bool.true_and,
    compatible, Bool.and_true]
  rcases mode with h | h <;> simp [h]

/-- The entire admitted argument list is installed in the input callee frame. Every loan
outside the list is unchanged; every loan inside it becomes a non-pending callee loan. -/
def AdoptedView (adopt : State → List (Var × LoanId) → Option State)
    (s : State) (f : Activation) (args : List (Var × LoanId)) : Prop :=
  ∃ t, adopt s args = some t ∧
    t.frames = { f with views := args ++ f.views } :: s.frames.tail ∧
    t.loans = s.loans.map (fun b => if b.id ∈ args.map Prod.snd then
      { b with activation := f.id, pendingArgument := false } else b) ∧
    ∀ e ∈ args, t.frames.head?.bind (fun g => AList.lookup g.views e.1) = some e.2 ∧
      ∀ a, loan s e.2 = some a →
        (∀ b ∈ s.loans, b.id = e.2 ∨ b.id ∈ a.ancestors ∨ b.root ≠ a.root ∨
          (a.mode = .shared ∧ b.mode = .shared)) →
        loanAccess t e.2 a.mode = true

/-- Law (authority: docs/decisions.md#decided-2026-11-08-recovered-d3;
#decided-2026-11-05-core Q3.2; #od2; #od23). (subject `ConcreteSpec.Core.Own.adoptDown`).
Adoption transfers every admitted argument to the input callee, including calls with multiple
arguments. The premises describe issued destinations, distinct binders and loans, pending
arguments and free local names. Each root-compatible argument is usable in its original mode. -/
theorem adopted_view_usable (s : State) (f : Activation) (args : List (Var × LoanId))
    (hf : s.frames.head? = some f) (issued : f.id < s.nextActivation)
    (names : (args.map Prod.fst).Nodup) (ids : (args.map Prod.snd).Nodup)
    (pending : ∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true)
    (free : ∀ e ∈ args, AList.lookup f.locals e.1 = none ∧ ParameterAvailable f.views e.1) :
    AdoptedView adoptDown s f args := by
  let transfer (b : Loan) : Loan :=
    if b.id ∈ args.map Prod.snd then { b with activation := f.id, pendingArgument := false } else b
  have id_eq (b : Loan) : (transfer b).id = b.id := by simp [transfer]; split <;> rfl
  have root_eq (b : Loan) : (transfer b).root = b.root := by simp [transfer]; split <;> rfl
  have mode_eq (b : Loan) : (transfer b).mode = b.mode := by simp [transfer]; split <;> rfl
  refine ⟨{ s with
    frames := { f with views := args ++ f.views } :: s.frames.tail
    loans := s.loans.map transfer }, ?_, rfl, rfl, ?_⟩
  · simp [adoptDown, hf, issued, names, ids, transfer]
    exact ⟨fun a b h => pending (a, b) h, fun a b h => free (a, b) h⟩
  · intro e he
    refine ⟨AList.lookup_append_some (AList.lookup_of_mem names he), ?_⟩
    intro a found clear
    have ea : a.id = e.2 := by simpa using List.find?_some found
    have inArgs : a.id ∈ args.map Prod.snd := ea ▸ List.mem_map_of_mem he
    have raw : s.loans.find? (·.id == e.2) = some a := found
    have ready : (s.loans.map transfer).find? (·.id == e.2) =
        some { a with activation := f.id, pendingArgument := false } := by
      simp [List.find?_map, Function.comp_def, id_eq, raw, transfer, inArgs]
    have compatible : (s.loans.map transfer).all (fun b =>
        b.id == e.2 || decide (b.id ∈ a.ancestors) || b.root != a.root ||
          (a.mode == .shared && b.mode == .shared)) = true := by
      rw [List.all_map]
      apply List.all_eq_true.mpr
      intro b hb
      change ((transfer b).id == e.2 || decide ((transfer b).id ∈ a.ancestors) ||
        (transfer b).root != a.root || (a.mode == .shared && (transfer b).mode == .shared)) = true
      rw [id_eq, root_eq, mode_eq]
      rcases clear b hb with h | h | h | ⟨h, hm⟩ <;> simp [*]
    simp only [loanAccess, loan, ready, Option.any_some, Bool.not_false, Bool.true_and, compatible,
      Bool.and_true]
    cases a.mode <;> rfl

/-- Admission for a storage-only scope exit, including the adapter's argument boundary. -/
def AdmittedCleanup (s : CallState) (xs : List Var) (f : Activation) : Prop :=
  s.pending = [] ∧ AdmittedDrop s.own xs f

instance (s : CallState) (xs : List Var) (f : Activation) : Decidable (AdmittedCleanup s xs f) := by
  unfold AdmittedCleanup
  infer_instance

/-- The adapter completes teardown, removing the selected slots and their bindings. -/
def ScopeCleanup (lifecycle : Exec.Life → CallState → Option CallState)
    (s : CallState) (xs : List Var) (f : Activation) : Prop :=
  ∃ t, lifecycle (.drop xs) s = some t ∧
    t.own.slots = s.own.slots.filter (fun a => decide (a.place ∉ departingPlaces f xs)) ∧
    t.own.frames = { f with locals := f.locals.filter (fun e => decide (e.2 ∉ departingPlaces f xs)) } :: s.own.frames.tail ∧
    t.own.loans = s.own.loans ∧ t.pending = []

/-- Law (authority: docs/decisions.md#od69; #od2;
#decided-2026-11-08-recovered-d3 C5 question 2; #decided-2026-11-13-lifecycle-refusal).
(subject `ConcreteSpec.Core.Own.callLife`). Every admitted storage-only cleanup completes,
including the final enclosing scope. It removes the departing storage and preserves all loans. -/
theorem scope_cleanup_loans (types : Var → Option Descriptor) (s : CallState) (xs : List Var)
    (f : Activation) (admitted : AdmittedCleanup s xs f) :
    ScopeCleanup (callLife types) s xs f := by
  obtain ⟨empty, frame, discharged, clear⟩ := admitted
  obtain ⟨u, hu⟩ := drop_progress s.own xs f frame discharged clear
  have step : callLife types (.drop xs) s = some { s with own := u } := by
    simp [callLife, life, empty, hu]
  refine ⟨_, step, ?_⟩
  simp only [drop, frame] at hu
  dsimp at hu
  split at hu
  · cases hu
    exact ⟨rfl, rfl, rfl, empty⟩
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
theorem deferred_call_lifetime_instance_1 : HeldAt entry.store held :=
  deferred_call_lifetime types (heldProgram (callLife types) z) held entry entry
    (by decide) (.refl entry)

/-- Instance (law `ConcreteSpec.Core.Own.deferred_registration`, case
`ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`). Registration has no pending argument. -/
theorem deferred_registration_instance_1 : ∃ c, Exec.step (heldProgram (callLife types) z)
    (heldRegistration (callLife types) caller ⟨1⟩ z) = some (.next c) ∧
    c.store = caller ∧ c.store.pending = [] := by
  exact ⟨_, deferred_registration _ (heldProgram (callLife types) z) callee
    [.ownerArgument ⟨1⟩ .mutable z] ⟨0⟩ [] caller _ _ .scope, rfl, rfl⟩

/-- Instance (execution, case `heldRegistration`, subject `ConcreteSpec.Core.Exec.run`). Real view use and all
enclosing cleanup reach normal finality;
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

/-- Planted (execution, case `heldRegistration`, subject `ConcreteSpec.Core.Own.adoptDown`, mutant
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.deadAdopt`). A pending view cannot be used. -/
example : ¬ DeferredExecution (callLifeWith leave deadAdopt types) caller ⟨1⟩ z := by
  intro h
  cases h.1

/-- The loan table before adoption, reached by real argument evaluation and ordinary entry. -/
def beforeAdopt : State :=
  (enter types [] ((passOwner caller ⟨1⟩ [] .mutable z).getD caller).own).getD caller.own
/-- Instance (law `ConcreteSpec.Core.Own.adopted_view_usable`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.beforeAdopt`). -/
theorem adopted_view_usable_instance_1 : AdoptedView adoptDown beforeAdopt ⟨1, [], []⟩ [(z, ⟨1⟩)] :=
  adopted_view_usable beforeAdopt _ _ rfl (by decide) (by decide) (by decide)
    (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.adopted_view_usable`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.beforeAdopt`, subject `ConcreteSpec.Core.Own.adoptDown`, mutant
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.deadAdopt`). Leaving the loan pending
breaks the transfer and prevents use. -/
theorem adopted_view_usable_planted_1 : ¬ AdoptedView deadAdopt beforeAdopt ⟨1, [], []⟩ [(z, ⟨1⟩)] := by
  rintro ⟨t, ht, _, table, _⟩
  cases ht
  cases table

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
theorem deferred_call_lifetime_planted_1 : ¬ DeferredLifetime dropWipe ownerAccess := by
  intro law
  have bad := law (heldProgram dropWipe z) held (cleanup dropWipe) wiped (by decide)
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
theorem deferred_call_lifetime_planted_2 : ¬ DeferredLifetime (callLife types) ignoreLoans := by
  intro law
  have bad := law (heldProgram (callLife types) z) held entry entry (by decide) (.refl _)
  cases bad.2.1

/-- Instance (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.cleanup`). The real cleanup step retains the loan. -/
theorem deferred_call_lifetime_instance_2 : HeldAt (Exec.run (heldProgram (callLife types) z) (fun _ p => nomatch p) 1
    (cleanup (callLife types))).residual.store held := by
  apply deferred_call_lifetime types (heldProgram (callLife types) z) held (cleanup (callLife types))
  · decide
  · exact .next (.refl _) (by decide) rfl
      (.life (e := .drop []) rfl (by intro h; cases h) (by intro ps h; cases h))

/-- Instance (law `ConcreteSpec.Core.Own.deferred_return_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.entry`). The callee endpoint can be any
admitted state; this concrete one retains the unrelated caller loan. -/
theorem deferred_return_lifetime_instance_1 : ReturnedCall leave entry.store.own ⟨1, [], [(z, ⟨1⟩)]⟩ held ⟨1⟩ :=
  deferred_return_lifetime entry.store.own ⟨1, [], [(z, ⟨1⟩)]⟩ held
    ⟨⟨1⟩, 0, ⟨.int .i32, false, true⟩, .owned⟩ (by decide) rfl (by decide)
    (by decide) rfl rfl rfl (by decide) rfl rfl (by decide)


/-- Mutant: end loans at an unissued activation, retaining callee loans. -/
def leaveRetain (s : State) : Option State := do
  let f ← s.frames.head?
  let s ← endLocals s.nextActivation s
  let s ← drop (List.map Prod.fst f.locals) s
  some { s with frames := s.frames.tail }

/-- Mutant: end every loan regardless of its activation. -/
def endAllLocals (_activation : Nat) (s : State) : Option State :=
  (s.loans.filter (fun _ => true)).foldlM (fun s l => endLeafAtBoundary s l.id) s

/-- Mutant: return ends caller loans as well as callee loans. -/
def leaveAll (s : State) : Option State := do
  let f ← s.frames.head?
  let s ← endAllLocals f.id s
  let s ← drop (List.map Prod.fst f.locals) s
  some { s with frames := s.frames.tail }

/-- Planted (law `ConcreteSpec.Core.Own.deferred_return_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.entry`, subject `ConcreteSpec.Core.Own.leave`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.leaveRetain`). The return boundary
must end exactly the callee's loans while retaining caller loans. -/
theorem deferred_return_lifetime_planted_1 : ¬ ReturnedCall leaveRetain entry.store.own ⟨1, [], [(z, ⟨1⟩)]⟩ held ⟨1⟩ := by
  rintro ⟨t, h, removed, _⟩
  cases h
  have bad := removed held (by decide)
  revert bad
  decide
/-- Planted (law `ConcreteSpec.Core.Own.deferred_return_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.entry`, subject `ConcreteSpec.Core.Own.leave`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.leaveAll`). The return boundary
must end exactly the callee's loans while retaining caller loans. -/
theorem deferred_return_lifetime_planted_2 : ¬ ReturnedCall leaveAll entry.store.own ⟨1, [], [(z, ⟨1⟩)]⟩ held ⟨1⟩ := by
  rintro ⟨t, h, _, keep, _⟩
  cases h
  have bad := keep (caller.own.loans.headD held) (by decide) (by decide)
  revert bad
  decide
/-- Mutant: refuse the return rule for a live direct owner argument. -/
private def refuseDirectReturn (s : State) : Option State :=
  if s.loans.any (fun l => l.callRoot && !l.pendingArgument) then none else leave s

/-- Planted (law `ConcreteSpec.Core.Own.deferred_return_lifetime`, case `entry`,
subject `ConcreteSpec.Core.Own.leave`, mutant `refuseDirectReturn`). -/
theorem deferred_return_lifetime_planted_3 : ValidReturn entry.store.own ⟨1, [], [(z, ⟨1⟩)]⟩ ∧
    ¬ ReturnedCall refuseDirectReturn entry.store.own ⟨1, [], [(z, ⟨1⟩)]⟩ held ⟨1⟩ := by
  refine ⟨by decide, ?_⟩
  rintro ⟨t, step, _⟩
  cases step

/-- Mutant: only the enclosing caller scope wipes loans; callee execution and return remain
unchanged. The old fuel-6 observation never reached this mutation. -/
def outerWipe : Exec.Life → CallState → Option CallState
  | .drop [], s => if s.own.frames.length = 1 then
      some { s with own := { s.own with loans := [] } } else callLife types (.drop []) s
  | e, s => callLife types e s

/-- A moved linear local beside a live shared caller loan. -/
def cleanupSource : CallState := ⟨(move caller.own ⟨1⟩).getD caller.own, []⟩
/-- The actual non-empty storage teardown result. -/
def cleanupResult : CallState := (callLife types (.drop [y]) cleanupSource).getD caller
/-- Instance (law `ConcreteSpec.Core.Own.scope_cleanup_loans`, case `cleanupSource`,
subject `ConcreteSpec.Core.Own.callLife`). The moved local satisfies the independent admission. -/
theorem scope_cleanup_loans_instance_1 : ScopeCleanup (callLife types) cleanupSource [y] (cleanupSource.own.frames.headD ⟨0, [], []⟩) :=
  scope_cleanup_loans types cleanupSource [y] _ (by decide)

/-- A normally completed scope whose entry domain did not contain the moved local. -/
private def movedLocalExit (lifecycle : Exec.Life → CallState → Option CallState) :
    Exec.Config (protocolOps lifecycle) :=
  ⟨.done, [(y, 0)], cleanupSource, [.scope ⟨0⟩ [] []]⟩

/-- Instance (law `ConcreteSpec.Core.Own.scope_cleanup_loans`, case `movedLocalExit`,
subject `ConcreteSpec.Core.Exec.step`). The dispatcher completes scope exit with the live loan intact. -/
theorem scope_cleanup_loans_instance_2 : Exec.step (heldProgram (callLife types) z) (movedLocalExit (callLife types)) =
      some (.next ⟨.done, [], cleanupResult, []⟩) ∧
    (slot cleanupSource.own ⟨1⟩).map Slot.status = some .moved ∧
    slot cleanupResult.own ⟨1⟩ = none ∧
    cleanupResult.own.loans = cleanupSource.own.loans ∧ cleanupSource.own.loans ≠ [] := by
  exact ⟨rfl, rfl, rfl, rfl, by decide⟩

/-- Mutant: decline scope exit whenever there is a local to remove. -/
private def refuseNonemptyDrop : Exec.Life → CallState → Option CallState
  | .drop (_ :: _), _ => none
  | e, s => callLife types e s

/-- Planted (law `ConcreteSpec.Core.Own.scope_cleanup_loans`, case `movedLocalExit`,
subject `ConcreteSpec.Core.Own.callLife`, mutant `refuseNonemptyDrop`).
An admitted scope exit stalls under the mutant. -/
theorem scope_cleanup_loans_planted_1 : AdmittedCleanup cleanupSource [y] (cleanupSource.own.frames.headD ⟨0, [], []⟩) ∧
    ¬ ScopeCleanup refuseNonemptyDrop cleanupSource [y]
      (cleanupSource.own.frames.headD ⟨0, [], []⟩) ∧
    Exec.step (heldProgram refuseNonemptyDrop z) (movedLocalExit refuseNonemptyDrop) = none :=
  ⟨by decide, (by rintro ⟨t, h, _⟩; cases h), rfl⟩

/-- Planted (law `ConcreteSpec.Core.Own.scope_cleanup_loans`, case
`ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.callLife`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.outerWipe`). The final scope cannot
silently end a caller loan after the deferred callee has returned. -/
theorem scope_cleanup_loans_planted_2 : AdmittedCleanup caller [] (caller.own.frames.headD ⟨0, [], []⟩) ∧
    ¬ ScopeCleanup outerWipe caller [] (caller.own.frames.headD ⟨0, [], []⟩) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨t, step, _, _, loans, _⟩
  cases step
  revert loans
  decide
/-- Planted (execution, case `heldRegistration`, subject `ConcreteSpec.Core.Own.callLife`, mutant
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
theorem deferred_registration_planted_1 : ∃ c, eagerRegistration (heldProgram (callLife types) z)
    (heldRegistration (callLife types) caller ⟨1⟩ z) = some (.next c) ∧
    c.store ≠ caller ∧ c.store.pending ≠ [] ∧ callLife types (.drop []) c.store = none := by
  exact ⟨_, rfl, by decide, by decide, rfl⟩

/-- Planted (law `ConcreteSpec.Core.Own.deferred_registration`, mutant
`eagerRegistration`). Independent registration succeeds using the original store. -/
theorem deferred_registration_planted_2 :
    let c := heldRegistration (callLife types) caller ⟨1⟩ z
    let d : Exec.Deferred (protocolOps (callLife types)) :=
      ⟨callee, [.ownerArgument ⟨1⟩ .mutable z], ⟨0⟩⟩
    let k' := [.scope ⟨0⟩ [] [d]]
    c.ctrl = .exec (.defer d.f d.args d.site) ∧ Exec.Registered d c.frames k' ∧
      ¬ (eagerRegistration (heldProgram (callLife types) z) c =
        some (.next ⟨.done, c.locals, c.store, k'⟩)) := by
  exact ⟨rfl, .scope, by intro h; cases h⟩

/-- Mutant: evaluate deferred operands at registration, discarding their store effects
but refusing registration if evaluation fails. -/
private def eagerGate {O : Exec.Operands} (P : Exec.Program O)
    (c : Exec.Config O) : Option (Exec.Out O) :=
  match c.ctrl with
  | .exec (.defer _ args _) =>
    match Exec.evalArgs O c.locals c.store args with
    | .ok _ => Exec.step P c
    | _ => none
  | _ => Exec.step P c

/-- The owner used by the deferred argument is moved until the following assignment. -/
private def repairAfterRegistration : Exec.Config (protocolOps (callLife types)) :=
  ⟨.exec (.seq (.defer callee [.ownerArgument ⟨1⟩ .mutable z] ⟨0⟩)
    (.assign y (.value (.lit 7)) ⟨1⟩)), [(y, 0)], cleanupSource, [.scope ⟨0⟩ [y] []]⟩

/-- Instance (execution, case `repairAfterRegistration`, subject `ConcreteSpec.Core.Exec.run`).
The assignment reinitializes the owner in time for the deferred call to use it at cleanup. -/
example :
    let run := fun fuel => Exec.run (heldProgram (callLife types) z) (fun _ p => nomatch p) fuel
      repairAfterRegistration
    (slot (run 1).residual.store.own ⟨1⟩).map Slot.status = some .moved ∧
    (slot (run 4).residual.store.own ⟨1⟩).map Slot.status = some .owned ∧
    (run 30).ending = .final .normal :=
  ⟨rfl, rfl, rfl⟩

/-- Planted (law `ConcreteSpec.Core.Own.deferred_registration`, case
`repairAfterRegistration`, subject `ConcreteSpec.Core.Exec.step`, mutant `eagerGate`).
The scope admits registration, but an eager read of the moved owner prevents the next
statement from reinitializing it. -/
theorem deferred_registration_planted_3 :
    let d : Exec.Deferred (protocolOps (callLife types)) :=
      ⟨callee, [.ownerArgument ⟨1⟩ .mutable z], ⟨0⟩⟩
    let next : Exec.Stmt (protocolOps (callLife types)) := .assign y (.value (.lit 7)) ⟨1⟩
    let k' := [.seq next, .scope ⟨0⟩ [y] [d]]
    let P := heldProgram (callLife types) z
    let c := (Exec.run P (fun _ p => nomatch p) 1 repairAfterRegistration).residual
    c.ctrl = .exec (.defer d.f d.args d.site) ∧ Exec.Registered d c.frames k' ∧
      ¬ (eagerGate P c = some (.next ⟨.done, c.locals, c.store, k'⟩)) :=
  ⟨rfl, .seq .scope, by intro h; cases h⟩

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
theorem deferred_call_lifetime_planted_3 : ¬ DeferredLifetime (callLife types) unfreezeMutable ∧
    unfreezeMutable entry.store.own held.root .shared = false := by
  refine ⟨?_, rfl⟩
  intro law
  have bad := law (heldProgram (callLife types) z) held entry entry (by decide) (.refl _)
  cases bad.2.1
/-- Planted (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.entry`, subject `ConcreteSpec.Core.Own.ownerAccess`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.unfreezeShared`). Isolate the shared
freeze clause; mutable access is still frozen. -/
theorem deferred_call_lifetime_planted_4 : ¬ DeferredLifetime (callLife types) unfreezeShared ∧
    unfreezeShared entry.store.own held.root .mutable = false := by
  refine ⟨?_, rfl⟩
  intro law
  have bad := law (heldProgram (callLife types) z) held entry entry (by decide) (.refl _)
  cases bad.2.2 rfl

/-- The former empty-callee execution is retained only as an Instance. Registration is
corrected to evaluate the actual owner argument late, with an empty pending buffer. -/
def skipProgram : Exec.Program (protocolOps (callLife types)) :=
  ⟨[(callee, ⟨[(z, ())], (), ⟨1⟩, .skip⟩)]⟩
/-- Instance (execution, case `skipProgram`, subject `ConcreteSpec.Core.Exec.run`). The original skip callee
now runs through normal enclosing-scope
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
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.beforeAdopt`, subject `ConcreteSpec.Core.Own.adoptDown`, mutant
`ConcreteSpec.Core.Own.DeferredLifetimeEvidence.adoptDropArguments`). Early release
at entry is rejected separately from a retained but unusable view. -/
theorem adopted_view_usable_planted_2 : ¬ AdoptedView adoptDropArguments beforeAdopt ⟨1, [], []⟩ [(z, ⟨1⟩)] := by
  rintro ⟨t, ht, _, table, _⟩
  cases ht
  cases table

end DeferredLifetimeEvidence
/-- Helper (inversion). Binding can allocate a place but never changes the loan supply. -/
private theorem bind_loan_supply (types : Var → Option Descriptor) (s t : State) (x : Var)
    (h : bind types x s = some t) : t.nextLoan = s.nextLoan := by
  unfold bind at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h <;> try contradiction
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
    change enter types ps s = some t at h
    unfold enter at h
    split at h <;> try contradiction
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
      (fold_projection State.nextLoan _ (fun s l t h => endLeafAtBoundary_supply_result s t l.id h) _ s u hu)

/-- Helper (induction/inversion). Places remain retired through entry, binding, teardown
and return; ending local loans changes neither the slots nor the place supply. -/
private theorem life_place_history (types : Var → Option Descriptor) (s : State)
    (e : Exec.Life) (p : Hybrid.Place) (hr : RetiredPlace s p) :
    ResultSatisfies (fun t => RetiredPlace t p) (life types e s) := by
  cases e with
  | bind x => exact bind_retains_retirement types s x p hr
  | drop xs => exact drop_retains_retirement s xs p hr
  | enter ps =>
    change ResultSatisfies (fun t => RetiredPlace t p) (enter types ps s)
    cases h : enter types ps s with
    | none => trivial
    | some t =>
      unfold enter at h
      split at h <;> try contradiction
      change ps.foldlM (fun s x => bind types x s) _ = some t at h
      have fold (xs : List Var) (a b : State) (ha : RetiredPlace a p)
          (h : xs.foldlM (fun s x => bind types x s) a = some b) : RetiredPlace b p := by
        induction xs generalizing a with
        | nil => simp at h; subst b; exact ha
        | cons x xs ih =>
          simp only [List.foldlM_cons] at h
          obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
          have keep := bind_retains_retirement types a x p ha
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
        unfold endLeafAtBoundary at hb
        obtain ⟨a, _, hb⟩ := Option.bind_eq_some_iff.mp hb
        split at hb
        · contradiction
        · cases hb; rfl
      have slots := endLocals_slots f.id s u hu
      have ur : RetiredPlace u p := ⟨by simpa [places] using hr.1, by simpa [slots] using hr.2⟩
      have vr := drop_retains_retirement u (List.map Prod.fst f.locals) p ur
      rw [hv] at vr
      exact vr

/-- Helper (lifecycle inversion). Adoption changes destinations but not identities;
all other lifecycle events preserve the supply and only remove loan records. -/
private theorem callLife_retired_loan (types : Var → Option Descriptor) (s : CallState)
    (e : Exec.Life) (i : LoanId) (hr : RetiredLoan s.own i) :
    ResultSatisfies (fun t => RetiredLoan t.own i) (callLife types e s) := by
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
        have supply := life_loan_supply types s.own u (.enter _) hu
        have loans := enter_loans types s.own u _ hu
        unfold adoptDown at hv
        obtain ⟨f, _, hv⟩ := Option.bind_eq_some_iff.mp hv
        split at hv
        · cases hv
          refine ⟨by simpa [supply] using hr.1, ?_⟩
          intro a ha
          obtain ⟨b, hb, rfl⟩ := List.mem_map.mp ha
          have hb' : b ∈ s.own.loans := by rwa [loans] at hb
          split <;> exact hr.2 b hb'
        · contradiction
      · contradiction
    | bind x | drop x | leave =>
      all_goals
        simp only [callLife] at h
        split at h
        · obtain ⟨u, hu, h⟩ := Option.map_eq_some_iff.mp h
          cases h
          exact ⟨by simpa [life_loan_supply types _ _ _ hu] using hr.1,
            fun l hl => hr.2 l (life_subset types _ _ _ hu l hl)⟩
        · contradiction

/-- The lifecycle adapter cannot rewind an issued identity or revive absent storage/loans. -/
def LifecycleNonrevival (lifecycle : Exec.Life → CallState → Option CallState)
    (s : CallState) (e : Exec.Life) (p : Hybrid.Place) (i : LoanId) : Prop :=
  (RetiredPlace s.own p → ResultSatisfies (fun t => RetiredPlace t.own p) (lifecycle e s)) ∧
  (RetiredLoan s.own i → ResultSatisfies (fun t => RetiredLoan t.own i) (lifecycle e s))

/-- Helper (inversion). Call lifecycle independently preserves retired places. -/
theorem callLife_retired_place (types : Var → Option Descriptor) (s : CallState)
    (e : Exec.Life) (p : Hybrid.Place) (hp : RetiredPlace s.own p) :
    ResultSatisfies (fun t => RetiredPlace t.own p) (callLife types e s) := by
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
        unfold adoptDown at hv
        obtain ⟨f, _, hv⟩ := Option.bind_eq_some_iff.mp hv
        split at hv
        · cases hv
          exact ur
        · contradiction
      · contradiction
    | bind x =>
      simp only [callLife] at h
      split at h
      · obtain ⟨u, hu, h⟩ := Option.map_eq_some_iff.mp h
        cases h
        have pr := life_place_history types s.own (.bind x) p hp
        rw [hu] at pr
        exact pr
      · contradiction
    | drop xs =>
      simp only [callLife] at h
      split at h
      · obtain ⟨u, hu, h⟩ := Option.map_eq_some_iff.mp h
        cases h
        have pr := life_place_history types s.own (.drop xs) p hp
        rw [hu] at pr
        exact pr
      · contradiction
    | leave =>
      simp only [callLife] at h
      split at h
      · obtain ⟨u, hu, h⟩ := Option.map_eq_some_iff.mp h
        cases h
        have pr := life_place_history types s.own (.leave) p hp
        rw [hu] at pr
        exact pr
      · contradiction

/-- Law (authority: docs/decisions.md#foundation-q1 C5; #decided-2026-11-05-core Q3.3;
#od15; #od69). (subject `ConcreteSpec.Core.Own.callLife`). Retired places and loans,
including identities originally issued to calls, cannot revive through any lifecycle event.
Entry/adoption can change loan destinations but cannot issue a retired identity. -/
theorem lifecycle_nonrevival (types : Var → Option Descriptor) (s : CallState) (e : Exec.Life)
    (p : Hybrid.Place) (i : LoanId) :
    LifecycleNonrevival (callLife types) s e p i :=
  ⟨callLife_retired_place types s e p, callLife_retired_loan types s e i⟩

/-- The existing loan-issuing operations, including both call-boundary interfaces.
This is a history alphabet over the specification operations, not a second execution relation. -/
inductive IssuanceAction where
  | ordinary (action : LoanAction)
  | owner (p : Hybrid.Place) (path : List Projection) (m : Mode) (x : Var)
  | view (i : LoanId) (x : Var)
  | entry (args : List (Var × LoanId))
  | lifecycle (event : Exec.Life)

/-- Ordinary issuance leaves the pending-call buffer intact. Its end event can
therefore remove only a loan outside that buffer. -/
def OrdinaryIssuanceAllowed (s : CallState) : LoanAction → Prop
  | .borrow .. | .reborrow .. => True
  | .endLoan i => i ∉ s.pending.map Prod.snd

instance (s : CallState) (op : LoanAction) : Decidable (OrdinaryIssuanceAllowed s op) := by
  cases op <;> unfold OrdinaryIssuanceAllowed <;> infer_instance

/-- Dispatch each history event through its actual specification operation. -/
def issuanceStep (types : Var → Option Descriptor) (s : CallState) : IssuanceAction → Option CallState
  | .ordinary op => if OrdinaryIssuanceAllowed s op then
      (loanAction s.own op).map fun own => { s with own := own }
    else none
  | .owner p path m x => passOwner s p path m x
  | .view i x => passDown s i x
  | .entry args => if s.pending = [] then
      (enterDown s.own args).map fun own => { s with own := own } else none
  | .lifecycle e => callLife types e s

/-- Helper (inversion). Passing an existing view issues a fresh child and changes only
that child's pending bit; no previously retired identity can reappear. -/
private theorem passDown_retired (s : CallState) (source i : LoanId) (x : Var)
    (hr : RetiredLoan s.own i) :
    ResultSatisfies (fun t => RetiredLoan t.own i) (passDown s source x) := by
  cases h : passDown s source x with
  | none => trivial
  | some t =>
    unfold passDown at h
    split at h
    · contradiction
    · obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
      obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h
        refine ⟨by have := hr.1; dsimp; omega, ?_⟩
        intro b hb
        rcases List.mem_cons.mp hb with rfl | hb
        · intro bad
          have eq : s.own.nextLoan = i.id := congrArg LoanId.id bad
          have := hr.1
          omega
        · exact hr.2 b hb
      · contradiction

/-- Helper (fold induction). Standalone downward entry uses the same fresh supply for
each parameter, so an arbitrary argument list preserves retired identities. -/
private theorem enterDown_retired (s : State) (args : List (Var × LoanId)) (i : LoanId)
    (hr : RetiredLoan s i) : ResultSatisfies (fun t => RetiredLoan t i) (enterDown s args) := by
  cases h : enterDown s args with
  | none => trivial
  | some t =>
    unfold enterDown at h
    split at h <;> try contradiction
    obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
    have initial : RetiredLoan u i := by
      have loans := enter_loans (fun _ => none) s u [] hu
      have supply := life_loan_supply (fun _ => none) s u (.enter []) hu
      exact ⟨by simpa [supply] using hr.1, by simpa [loans] using hr.2⟩
    rename_i available original
    clear original hu hr available s
    induction args generalizing u with
    | nil => simp at h; subst t; exact initial
    | cons arg args ih =>
      simp only [List.foldlM_cons] at h
      obtain ⟨v, hv, h⟩ := Option.bind_eq_some_iff.mp h
      unfold enterDownParameter at hv
      obtain ⟨a, _, hv⟩ := Option.bind_eq_some_iff.mp hv
      obtain ⟨f, _, hv⟩ := Option.bind_eq_some_iff.mp hv
      split at hv <;> try contradiction
      obtain ⟨w, hw, hv⟩ := Option.bind_eq_some_iff.mp hv
      cases hv
      have keep := loanAction_retains_retirement u (.reborrow arg.2 [] a.mode f.id) i initial
      change ResultSatisfies _ (reborrowTo u arg.2 [] a.mode f.id) at keep
      rw [hw] at keep
      exact ih _ h keep

/-- All finite histories of a uniformly substituted issuance dispatcher preserve retirement. -/
def IssuanceNonrevival (step : CallState → IssuanceAction → Option CallState) : Prop :=
  ∀ (s : CallState) (ops : List IssuanceAction) (i : LoanId), RetiredLoan s.own i →
    ResultSatisfies (fun t => RetiredLoan t.own i) (ops.foldlM step s)

/-- Law (authority: docs/decisions.md#foundation-q1 C5; #od15; #od23;
#decided-2026-11-08-recovered-d3). (subject `ConcreteSpec.Core.Own.issuanceStep`).
No issued identity revives across any finite interleaving of ordinary lending/ending,
owner arguments, view arguments, standalone entry, and lifecycle entry/adoption/return.
The two call interfaces share the ordinary loan supply rather than recycling identities. -/
theorem issuance_history_nonrevival (types : Var → Option Descriptor) :
    IssuanceNonrevival (issuanceStep types) := by
  intro s ops i hr
  have step (s : CallState) (op : IssuanceAction) (hr : RetiredLoan s.own i) :
      ResultSatisfies (fun t => RetiredLoan t.own i) (issuanceStep types s op) := by
    cases op with
    | ordinary op =>
      have keep := loanAction_retains_retirement s.own op i hr
      by_cases allowed : OrdinaryIssuanceAllowed s op <;>
        cases h : loanAction s.own op <;> simp_all [issuanceStep, ResultSatisfies]
    | owner p path m x => exact passOwner_retains_retirement s p path m x i hr
    | view source x => exact passDown_retired s source i x hr
    | entry args =>
      have keep := enterDown_retired s.own args i hr
      by_cases empty : s.pending = [] <;>
        cases h : enterDown s.own args <;> simp_all [issuanceStep, ResultSatisfies]
    | lifecycle e => exact callLife_retired_loan types s e i hr
  cases h : ops.foldlM (issuanceStep types) s with
  | none => trivial
  | some t =>
    induction ops generalizing s with
    | nil => simp at h; subst t; exact hr
    | cons op ops ih =>
      simp only [List.foldlM_cons] at h
      obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
      have keep := step s op hr
      rw [hu] at keep
      exact ih u keep h


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
theorem lifecycle_nonrevival_instance_1 : LifecycleNonrevival (callLife types) source (.enter []) ⟨3⟩ ⟨1⟩ :=
  lifecycle_nonrevival types source (.enter []) ⟨3⟩ ⟨1⟩
/-- A retired place with no issued loans, so the loan premise is false. -/
def retiredPlaceOnly : CallState := ⟨RetirementEvidence.retired, []⟩
/-- Instance (law `ConcreteSpec.Core.Own.lifecycle_nonrevival`, case `retiredPlaceOnly`). -/
theorem lifecycle_nonrevival_instance_2 : ¬ RetiredLoan retiredPlaceOnly.own ⟨0⟩ ∧
    ResultSatisfies (fun t => RetiredPlace t.own ⟨0⟩) (callLife types (.drop []) retiredPlaceOnly) :=
  ⟨by decide, (lifecycle_nonrevival types retiredPlaceOnly (.drop []) ⟨0⟩ ⟨0⟩).1 (by decide)⟩
/-- Planted (law `ConcreteSpec.Core.Own.lifecycle_nonrevival`, case `retiredPlaceOnly`,
subject `ConcreteSpec.Core.Own.callLife`, mutant `ConcreteSpec.Core.Own.LifecycleHistoryEvidence.rewindPlace`). -/
theorem lifecycle_nonrevival_planted_1 : ¬ LifecycleNonrevival rewindPlace retiredPlaceOnly (.drop []) ⟨0⟩ ⟨0⟩ := by
  intro h
  have bad := (h.1 (by decide)).1
  revert bad
  decide
/-- Planted (law `ConcreteSpec.Core.Own.lifecycle_nonrevival`, case
`ConcreteSpec.Core.Own.LifecycleHistoryEvidence.source`, subject `ConcreteSpec.Core.Own.callLife`,
mutant `ConcreteSpec.Core.Own.LifecycleHistoryEvidence.rewindLoan`). -/
theorem lifecycle_nonrevival_planted_2 : ¬ LifecycleNonrevival rewindLoan source (.drop []) ⟨3⟩ ⟨1⟩ := by
  intro h
  have bad := (h.2 (by decide)).1
  revert bad
  decide
end LifecycleHistoryEvidence


/-- Argument-time plain lending succeeds and immediately suspends conflicting parent use. -/
def PassedView (pass : CallState → LoanId → Var → Option CallState)
    (s : CallState) (i : LoanId) (x : Var) (m : Mode) : Prop :=
  ∃ t, pass s i x = some t ∧ loanAccess t.own i .mutable = false ∧
    (m = .mutable → loanAccess t.own i .shared = false) ∧
    t.pending = s.pending ++ [(x, ⟨s.own.nextLoan⟩)] ∧
    ∃ a l, loan s.own i = some a ∧ loan t.own ⟨s.own.nextLoan⟩ = some l ∧
      l.pendingArgument = true ∧ l.root = a.root ∧ l.path = a.path ∧
      l.mode = a.mode ∧ l.mode = m ∧ l.parent = some i

/-- Law (authority: docs/decisions.md#od2; #od23;
#decided-2026-11-08-recovered-d3 C5 Q2). (subject `ConcreteSpec.Core.Own.passDown`).
Every available view argument succeeds before the next argument is evaluated, including
when other arguments are already pending. The child keeps the source root, projection and
mode, is queued under the fresh parameter binding, and suspends conflicting parent access. -/
theorem passDown_protocol (s : CallState) (i : LoanId) (x : Var) (f : Activation) (a : Loan)
    (hs : WellFormed s.own) (hf : s.own.frames.head? = some f)
    (issued : f.id < s.own.nextActivation) (found : loan s.own i = some a)
    (allowed : loanAccess s.own i a.mode = true) (free : x ∉ s.pending.map Prod.fst) :
    PassedView passDown s i x a.mode := by
  have am := List.mem_of_find?_eq_some found
  have ai : a.id = i := by simpa using List.find?_some found
  have fresh (b : Loan) (hb : b ∈ s.own.loans) : b.id.id ≠ s.own.nextLoan := by
    have := (hs.2.2.2.1 b hb).1
    omega
  have ine : i ≠ LoanId.mk s.own.nextLoan := by
    intro h
    have := fresh a am
    rw [ai, h] at this
    exact this rfl
  have newNotAncestor : LoanId.mk s.own.nextLoan ∉ a.ancestors := by
    intro h
    have older := ancestor_older s.own hs a am _ h
    change s.own.nextLoan < a.id.id at older
    have := (hs.2.2.2.1 a am).1
    omega
  have raw : s.own.loans.find? (·.id == i) = some a := found
  let child : Loan :=
    ⟨⟨s.own.nextLoan⟩, a.root, a.path, a.mode, some i, i :: a.ancestors, f.id, true, false⟩
  let waiting : CallState :=
    ⟨{ s.own with nextLoan := s.own.nextLoan + 1, loans := child :: s.own.loans },
      s.pending ++ [(x, child.id)]⟩
  refine ⟨waiting, ?_, ?_, ?_, rfl, a, child, found, ?_, rfl, rfl, rfl, rfl, rfl, rfl⟩
  · simp [waiting, child, passDown, free, found, allowed, hf, issued]
  · simp [waiting, child, loanAccess, loan, Ne.symm ine, raw, newNotAncestor]
  · intro hm
    simp [waiting, child, loanAccess, loan, Ne.symm ine, raw, newNotAncestor, hm]
  · simp [waiting, child, loan]

/-- Plain lending pins the argument-time child before checking the complete call roundtrip.
The operation parameter is substituted in both obligations by every planted control. -/
def PlainLendingRoundtrip (pass : CallState → LoanId → Var → Option CallState)
    (s : State) (i : LoanId) (x : Var) (m : Mode) (types : Var → Option Descriptor) : Prop :=
  PassedView pass ⟨s, []⟩ i x m ∧
    (do let waiting ← pass ⟨s, []⟩ i x
        let called ← callLife types (.enter [x]) waiting
        callLife types .leave called) =
      some ⟨{ s with nextLoan := s.nextLoan + 1, nextActivation := s.nextActivation + 1 }, []⟩

/-- Law (authority: docs/decisions.md#od2; #od23;
#decided-2026-11-08-recovered-d3 C5 Q2). (subject `ConcreteSpec.Core.Own.passDown`).
Plain lending of either mode supplies the exact pending child at argument evaluation
and returns the caller state after the call with both identity supplies advanced. -/
theorem plain_lending_roundtrip (s : State) (i : LoanId) (x : Var)
    (types : Var → Option Descriptor) (f : Activation) (a : Loan)
    (hs : WellFormed s) (hf : s.frames.head? = some f)
    (issued : f.id < s.nextActivation) (found : loan s i = some a)
    (allowed : loanAccess s i a.mode = true) :
    PlainLendingRoundtrip passDown s i x a.mode types := by
  refine ⟨passDown_protocol ⟨s, []⟩ i x f a hs hf issued found allowed (by simp), ?_⟩
  have fresh : ∀ l ∈ s.loans, l.id ≠ LoanId.mk s.nextLoan := by
    intro l hl he
    have hb := (hs.2.2.2.1 l hl).1
    rw [he] at hb
    exact Nat.lt_irrefl _ hb
  have oldAct : ∀ l ∈ s.loans, l.activation ≠ s.nextActivation := by
    intro l hl he
    have hb := (hs.2.2.2.1 l hl).2.1
    omega
  have oldSlots : ∀ a ∈ s.slots, a.activation ≠ s.nextActivation := by
    intro a ha he
    have hb := (hs.2.2.1 a ha).2
    omega
  have noChild : ∀ l ∈ s.loans, l.parent ≠ some (LoanId.mk s.nextLoan) := by
    intro l hl he
    have hp := (hs.2.2.2.1 l hl).2.2.2.1
    unfold ParentValid at hp
    rw [he] at hp
    obtain ⟨a, ha, hid, _⟩ := hp
    exact fresh a ha hid
  have noChildBool : s.loans.any (·.parent == some (LoanId.mk s.nextLoan)) = false := by
    apply Bool.eq_false_iff.mpr
    intro h
    obtain ⟨l, hl, he⟩ := List.any_eq_true.mp h
    exact noChild l hl (by simpa using he)
  have filterId : s.loans.filter (·.id != LoanId.mk s.nextLoan) = s.loans := by
    apply List.filter_eq_self.mpr
    intro l hl
    simpa using fresh l hl
  have filterAct : s.loans.filter (·.activation == s.nextActivation) = [] := by
    apply List.filter_eq_nil_iff.mpr
    intro l hl
    simpa using oldAct l hl
  have slotsBool : s.slots.any (·.activation == s.nextActivation) = false := by
    apply Bool.eq_false_iff.mpr
    intro h
    obtain ⟨a, ha, he⟩ := List.any_eq_true.mp h
    exact oldSlots a ha (by simpa using he)
  have mapOld : (s.loans.map fun l => if l.id = LoanId.mk s.nextLoan then
      { l with activation := s.nextActivation, pendingArgument := false } else l) = s.loans := by
    calc
      _ = s.loans.map id := by
        apply List.map_congr_left
        intro l hl
        simp [fresh l hl]
      _ = s.loans := List.map_id _
  have noChildExists : ¬ ∃ l ∈ s.loans, l.parent = some (LoanId.mk s.nextLoan) := by
    rintro ⟨l, hl, he⟩
    exact noChild l hl he
  have raw : s.loans.find? (·.id == i) = some a := found
  have ine : i ≠ LoanId.mk s.nextLoan := by
    have ai : a.id = i := by simpa using List.find?_some found
    simpa [ai] using fresh a (List.mem_of_find?_eq_some found)
  simp [passDown, raw, hf, issued, allowed, callLife, enter, adoptDown, life, leave,
    endLocals, endLeafAtBoundary, drop, loan, ParameterAvailable, AList.lookup, mapOld, filterAct, noChildExists,
    filterId, ine]


namespace Round3Evidence
open Examples DirectArgumentWitnesses DeferredLifetimeEvidence
/-- Mutant: the child inherits its parent activation, pointing back into the caller. -/
def inheritActivation (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, m, some i,
        i :: a.ancestors, a.activation, false, false⟩ :: s.loans }
  else none

/-- Mutant: adoption leaves shared arguments pending. -/
def sharedDeadAdopt (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then { l with activation := f.id, pendingArgument := l.mode == .shared } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none

/-- Mutant: adoption leaves an argument pending when another loan shares its root. -/
def ancestorDeadAdopt (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then
          { l with
            activation := f.id
            pendingArgument := s.loans.any (fun b => b.id != l.id && b.root == l.root) } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none

/-- Mutant: a view argument recycles issued identity 1 instead of the fresh supply. -/
def recyclePassDown (s : CallState) (source : LoanId) (parameter : Var) : Option CallState := do
  if parameter ∈ s.pending.map Prod.fst then none else do
  let l ← loan s.own source
  let own ← reborrow s.own source [] l.mode
  let own := { own with loans := own.loans.map fun l =>
    if l.id.id = s.own.nextLoan then { l with id := ⟨1⟩, pendingArgument := true } else l }
  pure ⟨own, s.pending ++ [(parameter, ⟨1⟩)]⟩

/-- A populated caller lending its existing exclusive view. -/
def relendCaller : CallState := ⟨exclusive, []⟩
/-- The before-adoption state for a call lending the caller's existing mutable view. -/
def relendBefore : State :=
  (enter types [] ((passDown relendCaller ⟨0⟩ z).getD relendCaller).own).getD relendCaller.own
/-- The pending child issued while evaluating that existing view argument. -/
def relendPending : Loan := ⟨⟨1⟩, ⟨0⟩, [.index 0], .mutable, some ⟨0⟩, [⟨0⟩], 0, true, false⟩
/-- The child after adoption in callee activation 1. -/
def relendHeld : Loan := { relendPending with activation := 1, pendingArgument := false }
/-- The adopted state shares its root with the caller's retained parent. -/
def relendEntry : State := (adoptDown relendBefore [(z, ⟨1⟩)]).getD relendCaller.own

/-- Instance (law `ConcreteSpec.Core.Own.adopted_view_usable`, case
`relendBefore`). OD2 plain lending admits a same-root parent and produces a usable child. -/
theorem adopted_view_usable_instance_2 : AdoptedView adoptDown relendBefore ⟨1, [], []⟩ [(z, ⟨1⟩)] :=
  adopted_view_usable relendBefore _ _ rfl (by decide) (by decide) (by decide)
    (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.adopted_view_usable`, case `relendBefore`,
subject `ConcreteSpec.Core.Own.adoptDown`, mutant `ancestorDeadAdopt`). The caller parent
shares the root; it must not cause the adopted child to remain unusable. -/
theorem adopted_view_usable_planted_3 : ¬ AdoptedView ancestorDeadAdopt relendBefore ⟨1, [], []⟩ [(z, ⟨1⟩)] := by
  rintro ⟨t, ht, _, table, _⟩
  cases ht
  cases table

/-- Instance (law `ConcreteSpec.Core.Own.deferred_return_view`, case `relendEntry`).
Return preserves the parent record and restores mutable access through it. -/
theorem deferred_return_view_instance_1 : ReturnedView leave relendEntry ⟨1, [], [(z, ⟨1⟩)]⟩
    (relendCaller.own.loans.headD held) .mutable :=
  deferred_return_view relendEntry ⟨1, [], [(z, ⟨1⟩)]⟩ (relendCaller.own.loans.headD held)
    .mutable (by decide) rfl (by decide) (by decide) (by decide) rfl (by decide) (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.deferred_return_view`, case `relendEntry`,
subject `ConcreteSpec.Core.Own.leave`, mutant `DeferredLifetimeEvidence.leaveAll`).
Ending a relendCaller parent at return violates both preservation and restored access. -/
theorem deferred_return_view_planted_1 : ¬ ReturnedView leaveAll relendEntry ⟨1, [], [(z, ⟨1⟩)]⟩
    (relendCaller.own.loans.headD held) .mutable := by
  rintro ⟨t, h, _, _, retained, _⟩
  cases h
  revert retained
  decide
/-- Mutant: refuse to return while a callee loan descends from a caller view. -/
private def refuseViewReturn (s : State) : Option State :=
  if s.loans.any (fun l => l.parent.isSome && !l.pendingArgument) then none else leave s

/-- Planted (law `ConcreteSpec.Core.Own.deferred_return_view`, case `relendEntry`,
subject `ConcreteSpec.Core.Own.leave`, mutant `refuseViewReturn`). -/
theorem deferred_return_view_planted_2 : ValidReturn relendEntry ⟨1, [], [(z, ⟨1⟩)]⟩ ∧
    ¬ ReturnedView refuseViewReturn relendEntry ⟨1, [], [(z, ⟨1⟩)]⟩
      (relendCaller.own.loans.headD held) .mutable := by
  refine ⟨by decide, ?_⟩
  rintro ⟨t, step, _⟩
  cases step

/-- A current callee frame above an existing caller loan. -/
def upSource : State := (enter types [] exclusive).getD initial
/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case `upSource`,
subject `ConcreteSpec.Core.Own.reborrowTo`, mutant `inheritActivation`).
The child must target the current frame, never its caller. -/
theorem downward_reborrow_protocol_planted_6 : ¬ DownwardProtocol inheritActivation upSource ⟨0⟩ [] .mutable 1 0 := by
  have noActivation : ∀ l ∈ (inheritActivation upSource ⟨0⟩ [] .mutable 1).getD initial |>.loans,
      l.activation ≠ 1 := by decide
  rintro ⟨_, t, ht, _, _, _, _, _, _, l, hl, hid, ha, _⟩
  cases ht
  exact noActivation l hl ha

/-- A complete deferred call passing an existing mutable view, evaluated late. -/
def relendRegistration (life : Exec.Life → CallState → Option CallState) :
    Exec.Config (protocolOps life) :=
  ⟨.exec (.defer callee [.argument ⟨0⟩ z] ⟨0⟩), [], relendCaller, [.scope ⟨0⟩ [] []]⟩
/-- Instance (execution, case `relendRegistration`, subject `ConcreteSpec.Core.Exec.run`).
Same-root lending reaches normal enclosing completion and restores the caller parent. -/
example : let r := (Exec.run (heldProgram (callLife types) z) (fun _ p => nomatch p) 30
    (relendRegistration (callLife types)));
    r.ending = .final .normal ∧ r.residual.store.own.loans = relendCaller.own.loans ∧
      loanAccess r.residual.store.own ⟨0⟩ .mutable = true := by exact ⟨rfl, rfl, rfl⟩
/-- Planted (execution, case `relendRegistration`, subject `ConcreteSpec.Core.Own.adoptDown`, mutant
`ancestorDeadAdopt`). The one-rule mutant gets stuck through the actual dispatcher. -/
example : let life := callLifeWith leave ancestorDeadAdopt types;
    let r := Exec.run (heldProgram life z) (fun _ p => nomatch p) 30 (relendRegistration life);
    Ending.IsStuck r.ending = true := by decide

/-- A relending chain with both a parent and a grandparent on the same root. -/
def ancestorCaller : CallState := ⟨LifecycleWitnesses.called, []⟩
/-- The next nested callee, before adopting the newly issued child. -/
def ancestorBefore : State :=
  (enter types [] ((passDown ancestorCaller ⟨1⟩ z).getD ancestorCaller).own).getD initial
/-- Instance (law `ConcreteSpec.Core.Own.adopted_view_usable`, case `ancestorBefore`,
subject `ConcreteSpec.Core.Own.adoptDown`). Both parent and ancestor are admitted. -/
theorem adopted_view_usable_instance_3 : AdoptedView adoptDown ancestorBefore ⟨2, [], []⟩ [(z, ⟨2⟩)] :=
  adopted_view_usable ancestorBefore _ _ rfl (by decide) (by decide) (by decide)
    (by decide) (by decide)

/-- A real shared owner argument before adoption. -/
def sharedBefore : State :=
  (enter types [] ((passOwner caller ⟨1⟩ [] .shared z).getD caller).own).getD caller.own
/-- Instance (law `ConcreteSpec.Core.Own.adopted_view_usable`, case `sharedBefore`).
Shared arguments also become usable callee views. -/
theorem adopted_view_usable_instance_4 : AdoptedView adoptDown sharedBefore ⟨1, [], []⟩ [(z, ⟨1⟩)] :=
  adopted_view_usable sharedBefore _ _ rfl (by decide) (by decide) (by decide)
    (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.adopted_view_usable`, case `sharedBefore`,
subject `ConcreteSpec.Core.Own.adoptDown`, mutant `sharedDeadAdopt`). Isolate shared adoption. -/
theorem adopted_view_usable_planted_4 : ¬ AdoptedView sharedDeadAdopt sharedBefore ⟨1, [], []⟩ [(z, ⟨1⟩)] := by
  rintro ⟨t, ht, _, table, _⟩
  cases ht
  cases table

/-- A shared adopted loan in a populated callee. -/
def sharedHeld : Loan := { held with mode := .shared }
/-- The real adopted shared-call store. -/
def sharedStore : CallState := ⟨(adoptDown sharedBefore [(z, ⟨1⟩)]).getD caller.own, []⟩
/-- An actual storage-cleanup site with that shared call loan. -/
def sharedCleanup (life : Exec.Life → CallState → Option CallState) : Exec.Config (protocolOps life) :=
  ⟨.done, [], sharedStore, [.scope ⟨2⟩ [] []]⟩
/-- Mutant: owner writes ignore active shared loans when all loans are shared. -/
def allowWriteUnderShared (s : State) (p : Hybrid.Place) (m : Mode) : Bool :=
  if m = .mutable ∧ s.loans.all (fun l => l.mode == .shared) then ignoreLoans s p m
  else ownerAccess s p m
/-- Mutant: an empty cleanup drops shared loans alone. -/
def dropShared : Exec.Life → CallState → Option CallState
  | .drop [], s => (callLife types (.drop []) s).map fun t =>
      { t with own := { t.own with loans := t.own.loans.filter (·.mode != .shared) } }
  | e, s => callLife types e s
/-- Instance (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case `sharedStore`).
A shared call freezes owner mutation for an arbitrary admitted prefix. -/
theorem deferred_call_lifetime_instance_3 : HeldAt (sharedCleanup (callLife types)).store sharedHeld :=
  deferred_call_lifetime types (heldProgram (callLife types) z) sharedHeld
    (sharedCleanup (callLife types)) (sharedCleanup (callLife types)) (by decide) (.refl _)
/-- Planted (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case `sharedStore`,
subject `ConcreteSpec.Core.Own.ownerAccess`, mutant `allowWriteUnderShared`). -/
theorem deferred_call_lifetime_planted_5 : ¬ DeferredLifetime (callLife types) allowWriteUnderShared := by
  intro law
  have bad := law (heldProgram (callLife types) z) sharedHeld
    (sharedCleanup (callLife types)) (sharedCleanup (callLife types)) (by decide) (.refl _)
  cases bad.2.1
/-- Planted (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case `sharedCleanup`,
subject `ConcreteSpec.Core.Own.callLife`, mutant `dropShared`). A real cleanup step may
not silently retire a shared call loan. -/
theorem deferred_call_lifetime_planted_6 : ¬ DeferredLifetime dropShared ownerAccess := by
  intro law
  let after : Exec.Config (protocolOps dropShared) :=
    ⟨.done, [], { sharedStore with own := { sharedStore.own with loans := [] } }, []⟩
  have bad := law (heldProgram dropShared z) sharedHeld (sharedCleanup dropShared) after
    (by decide) (.next (.refl _) (by decide) rfl
      (.life (e := .drop []) rfl (by intro h; cases h) (by intro ps h; cases h)))
  cases bad.1

/-- A callee-owned linear local beside the protected call loan, ready for consumption. -/
def movingStore : CallState := { entry.store with own := { entry.store.own with
  nextPlace := 3
  slots := ⟨⟨2⟩, 1, ⟨.int .i32, false, true⟩, .owned⟩ :: entry.store.own.slots
  frames := ⟨1, [(y, ⟨2⟩)], [(z, ⟨1⟩)]⟩ :: entry.store.own.frames.tail } }
/-- A real expression step consuming that unrelated local. -/
def movingStart : Exec.Config (protocolOps (callLife types)) :=
  ⟨.exec (.ret (.value (.move y)) ⟨0⟩), [(y, 7)], movingStore, []⟩
/-- Its endpoint from the actual dispatcher. -/
def movingEnd : Exec.Config (protocolOps (callLife types)) :=
  (Exec.run (heldProgram (callLife types) z) (fun _ p => nomatch p) 1 movingStart).residual
/-- Instance (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case `movingStart`,
subject `ConcreteSpec.Core.Exec.step`). Restricting quiet effects does not exclude ordinary
owner consumption: its explicit operation certificate preserves the protected loan. -/
theorem deferred_call_lifetime_instance_4 : HeldAt movingEnd.store held :=
  deferred_call_lifetime types (heldProgram (callLife types) z) held movingStart movingEnd
    (by decide) (.next (.refl _) (by decide) rfl (.consume (p := ⟨2⟩) rfl))

/-- Mutant: nested entry alone wipes the pre-existing loans. -/
def nestedWipe : Exec.Life → CallState → Option CallState
  | .enter ps, s => (callLife types (.enter ps) s).map fun t =>
      if s.own.frames.length ≥ 2 then { t with own := { t.own with loans := [] } } else t
  | e, s => callLife types e s
/-- A nested function with a real body and normal return. -/
def nestedProgram (life : Exec.Life → CallState → Option CallState) : Exec.Program (protocolOps life) :=
  ⟨[(callee, ⟨[], (), ⟨3⟩, .skip⟩)]⟩
/-- A call instruction executing within the deferred callee while its argument is live. -/
def nestedStart (life : Exec.Life → CallState → Option CallState) : Exec.Config (protocolOps life) :=
  ⟨.exec (.call none callee [] ⟨0⟩), [], entry.store, []⟩
/-- The actual nested entry endpoint, with the substituted callback. -/
def nestedEntered (life : Exec.Life → CallState → Option CallState) : Exec.Config (protocolOps life) :=
  (Exec.run (nestedProgram life) (fun _ p => nomatch p) 1 (nestedStart life)).residual
/-- Instance (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case `nestedStart`).
Two actual steps, nested call entry and its body, retain the deferred callee's loan. -/
theorem deferred_call_lifetime_instance_5 : HeldAt (Exec.run (nestedProgram (callLife types)) (fun _ p => nomatch p) 2
    (nestedStart (callLife types))).residual.store held := by
  apply deferred_call_lifetime types (nestedProgram (callLife types)) held (nestedStart (callLife types))
  · decide
  · exact .next (.next (.refl _) (by decide) rfl
      (.life (e := .enter []) rfl (by intro h; cases h) (by intro ps h; decide))) (by decide) rfl .quiet
/-- Planted (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case `nestedStart`,
subject `ConcreteSpec.Core.Own.callLife`, mutant `nestedWipe`). The actual nested call
must use the substituted lifecycle operation rather than an unmutated entry bypass. -/
theorem deferred_call_lifetime_planted_7 : ¬ DeferredLifetime nestedWipe ownerAccess := by
  intro law
  have bad := law (nestedProgram nestedWipe) held (nestedStart nestedWipe) (nestedEntered nestedWipe)
    (by decide) (.next (.refl _) (by decide) rfl
      (.life (e := .enter []) rfl (by intro h; cases h) (by intro ps h; decide)))
  cases bad.1

/-- Substitute the recycled view-argument rule in the complete history dispatcher. -/
def recycleViewStep (s : CallState) : IssuanceAction → Option CallState
  | .view i x => recyclePassDown s i x
  | op => issuanceStep types s op
/-- Mutant: standalone entry reuses retired identity 1 in each new child constructor. -/
def recycleEnterDown (s : State) (args : List (Var × LoanId)) : Option State := do
  let s ← enter (fun _ => none) [] s
  args.foldlM (fun s (x, i) => do
    let a ← loan s i
    let f ← s.frames.head?
    let s ← LoanHistoryEvidence.recycleReborrow s i [] a.mode f.id
    some { s with frames := { f with views := (x, ⟨1⟩) :: f.views } :: s.frames.tail }) s
/-- Substitute standalone entry in the complete history dispatcher. -/
def recycleEntryStep (s : CallState) : IssuanceAction → Option CallState
  | .entry args => (recycleEnterDown s.own args).map fun own => { s with own := own }
  | op => issuanceStep types s op
/-- Instance (law `ConcreteSpec.Core.Own.issuance_history_nonrevival`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`). The unified history includes
ordinary actions, both argument forms, standalone entry, adoption and return. -/
theorem issuance_history_nonrevival_instance_1 : IssuanceNonrevival (issuanceStep types) := issuance_history_nonrevival types
/-- A successful eventful history visiting every issuing interface and retiring
intermediate owner/view arguments at their actual return boundaries. -/
def issuanceActions : List IssuanceAction :=
  [.ordinary (.borrow ⟨1⟩ [] .shared), .ordinary (.endLoan ⟨2⟩),
   .owner ⟨1⟩ [] .mutable z, .lifecycle (.enter [z]), .lifecycle .leave,
   .view ⟨0⟩ z, .lifecycle (.enter [z]), .lifecycle .leave,
   .entry [(z, ⟨0⟩)], .lifecycle .leave, .ordinary (.reborrow ⟨0⟩ [] .mutable 0)]
/-- Instance (law `ConcreteSpec.Core.Own.issuance_history_nonrevival`, case `issuanceActions`,
subject `ConcreteSpec.Core.Own.issuanceStep`). Every arm succeeds in this populated history. -/
theorem issuance_history_nonrevival_instance_2 : (issuanceActions.foldlM (issuanceStep types) ⟨LoanHistoryEvidence.endedChild, []⟩).isSome = true ∧
    ResultSatisfies (fun t => RetiredLoan t.own ⟨1⟩)
      (issuanceActions.foldlM (issuanceStep types) ⟨LoanHistoryEvidence.endedChild, []⟩) :=
  ⟨by decide, issuance_history_nonrevival types _ issuanceActions ⟨1⟩ (by decide)⟩
/-- Planted (law `ConcreteSpec.Core.Own.issuance_history_nonrevival`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.passDown`, mutant
`recyclePassDown`). A successful, well-formed call argument revives retired identity 1. -/
theorem issuance_history_nonrevival_planted_1 : ¬ IssuanceNonrevival recycleViewStep := by
  intro law
  have bad := law ⟨LoanHistoryEvidence.endedChild, []⟩ [.view ⟨0⟩ z] ⟨1⟩ (by decide)
  revert bad
  decide
/-- Planted (law `ConcreteSpec.Core.Own.issuance_history_nonrevival`, case
`ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.enterDown`,
mutant `recycleEnterDown`). Standalone entry must also issue a fresh identity. -/
theorem issuance_history_nonrevival_planted_2 : ¬ IssuanceNonrevival recycleEntryStep := by
  intro law
  have bad := law ⟨LoanHistoryEvidence.endedChild, []⟩ [.entry [(z, ⟨0⟩)]] ⟨1⟩ (by decide)
  revert bad
  decide
end Round3Evidence

namespace Round4Evidence
open Examples DirectArgumentWitnesses DeferredLifetimeEvidence Round3Evidence
/-- Mutant: relending retains the caller activation; direct-owner arguments adopt normally. -/
def keepActivation (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then
          { l with
            activation := if l.callRoot then f.id else l.activation
            pendingArgument := false } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none

/-- Mutant: retain argument loans and their ancestors, silently deleting other caller loans. -/
def dropUnrelated (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := (s.loans.filter fun b => args.any fun e =>
        e.2 == b.id || (loan s e.2).any (fun a => decide (b.id ∈ a.ancestors))).map fun l =>
        if l.id ∈ args.map Prod.snd then { l with activation := f.id, pendingArgument := false } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none

/-- Mutant: downward lending discards the requested suffix. -/
def discardSuffix (s : State) (i : LoanId) (_ : List Projection) (m : Mode) (a : Nat) :=
  reborrowTo s i [] m a
/-- Mutant: return refuses any callee loan lent from a caller view. -/
def refuseRelend (s : State) : Option State := do
  let f ← s.frames.head?
  if s.loans.any (fun b => b.activation == f.id && b.parent.isSome) then none else leave s
/-- Mutant: unwind callee loans oldest first, attempting parents before their children. -/
def reverseLeave (s : State) : Option State := do
  let f ← s.frames.head?
  let t ← (s.loans.filter (·.activation == f.id)).reverse.foldlM (fun t b => endLoan t b.id) s
  let u ← drop (List.map Prod.fst f.locals) t
  pure { u with frames := u.frames.tail }
/-- Mutant: plain lending refuses mutable views while leaving shared lending unchanged. -/
def refuseMutable (s : CallState) (i : LoanId) (x : Var) : Option CallState := do
  let b ← loan s.own i
  if b.mode == .mutable then none else passDown s i x

/-- A valid return with a local child of the adopted view, newest first. -/
def localChild : State := (reborrow relendEntry ⟨1⟩ [.index 1] .mutable).getD initial
/-- Instance (law `ConcreteSpec.Core.Own.return_progress`, case `localChild`, subject `ConcreteSpec.Core.Own.leave`).
Both a local child and the adopted child end before the caller parent resumes. -/
theorem return_progress_instance_1 : WellFormed localChild ∧ ValidReturn localChild ⟨1, [], [(z, ⟨1⟩)]⟩ ∧
    ReturnProgress leave localChild :=
  ⟨by decide, by decide, return_progress localChild _ (by decide) rfl (by decide)⟩
/-- Planted (law `ConcreteSpec.Core.Own.return_progress`, case `relendEntry`, subject `ConcreteSpec.Core.Own.leave`,
mutant `refuseRelend`). Refusal of plain lending violates progress, not just an Instance. -/
theorem return_progress_planted_1 : WellFormed relendEntry ∧ ValidReturn relendEntry ⟨1, [], [(z, ⟨1⟩)]⟩ ∧
    ¬ ReturnProgress refuseRelend relendEntry := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨t, ht⟩
  cases ht
/-- Planted (law `ConcreteSpec.Core.Own.return_progress`, case `localChild`, subject `ConcreteSpec.Core.Own.leave`,
mutant `reverseLeave`). Parent-before-child unwinding refuses this structurally valid return. -/
theorem return_progress_planted_2 : WellFormed localChild ∧ ValidReturn localChild ⟨1, [], [(z, ⟨1⟩)]⟩ ∧
    ¬ ReturnProgress reverseLeave localChild := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨t, ht⟩
  cases ht
/-- Planted (law `ConcreteSpec.Core.Own.adopted_view_usable`, case `relendBefore`, subject
`ConcreteSpec.Core.Own.adoptDown`, mutant `keepActivation`). All old premises still hold; the callee activation
output fails. -/
theorem adopted_view_usable_planted_5 : ¬ AdoptedView keepActivation relendBefore ⟨1, [], []⟩ [(z, ⟨1⟩)] := by
  rintro ⟨t, ht, _, table, _⟩
  cases ht
  cases table

/-- Planted (law `ConcreteSpec.Core.Own.adopted_view_usable`, case `beforeAdopt`, subject
`ConcreteSpec.Core.Own.adoptDown`, mutant `dropUnrelated`). Adoption must preserve the unrelated caller borrow
as well as ancestors. -/
theorem adopted_view_usable_planted_6 : ¬ AdoptedView dropUnrelated beforeAdopt ⟨1, [], []⟩ [(z, ⟨1⟩)] := by
  rintro ⟨t, ht, _, table, _⟩
  cases ht
  cases table

/-- Instance (law `ConcreteSpec.Core.Own.passDown_protocol`, case `relendCaller`, subject
`ConcreteSpec.Core.Own.passDown`). -/
theorem passDown_protocol_instance_1 : PassedView passDown relendCaller ⟨0⟩ z .mutable :=
  passDown_protocol relendCaller ⟨0⟩ z _ _ (by decide) rfl (by decide) rfl rfl (by decide)
/-- Instance (law `ConcreteSpec.Core.Own.plain_lending_roundtrip`, case `relendCaller`, subject
`ConcreteSpec.Core.Own.passDown`). -/
theorem plain_lending_roundtrip_instance_1 : (do
      let waiting ← passDown relendCaller ⟨0⟩ z
      let called ← callLife types (.enter [z]) waiting
      callLife types .leave called) =
    some ⟨{ relendCaller.own with nextLoan := 2, nextActivation := 2 }, []⟩ :=
  (plain_lending_roundtrip relendCaller.own ⟨0⟩ z types _ _ (by decide) rfl (by decide) rfl rfl).2
/-- Planted (law `ConcreteSpec.Core.Own.passDown_protocol`, case `relendCaller`, subject
`ConcreteSpec.Core.Own.passDown`, mutant `refuseMutable`). OD2 plain mutable lending cannot be refused. -/
theorem passDown_protocol_planted_1 : ¬ PassedView refuseMutable relendCaller ⟨0⟩ z .mutable := by
  rintro ⟨t, ht, _⟩
  cases ht
/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case `upSource`, subject
`ConcreteSpec.Core.Own.reborrowTo`, mutant `discardSuffix`). A supplied projection must be appended, rather than
discarded. -/
theorem downward_reborrow_protocol_planted_7 : ¬ DownwardProtocol discardSuffix upSource ⟨0⟩ [.index 1] .mutable 1 0 := by
  rintro ⟨_, t, ht, _, _, _, _, _, _, l, hl, hid, _, a, found, _, path, _⟩
  cases ht
  cases found
  change l ∈ [_, _] at hl
  rcases List.mem_cons.mp hl with he | hl
  · subst l
    cases path
  · simp only [List.mem_singleton] at hl
    subst l
    cases hid

/-- Planted (law `ConcreteSpec.Core.Own.plain_lending_roundtrip`, case `relendCaller`, subject
`ConcreteSpec.Core.Own.passDown`, mutant `refuseMutable`). The same refusal also breaks the complete call
composition. -/
theorem plain_lending_roundtrip_planted_1 : ¬ PlainLendingRoundtrip refuseMutable relendCaller.own ⟨0⟩ z .mutable types := by
  rintro ⟨_, returned⟩
  cases returned

/-- Extra activation mutant: adoption assigns the next, unissued frame rather than the callee. -/
def futureActivation (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then { l with activation := s.nextActivation, pendingArgument := false } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none

/-- Planted (law `ConcreteSpec.Core.Own.adopted_view_usable`, case `relendBefore`, subject
`ConcreteSpec.Core.Own.adoptDown`, mutant `futureActivation`). Independent activation-family attempt,
kernel-rejected. -/
theorem adopted_view_usable_planted_7 : ¬ AdoptedView futureActivation relendBefore ⟨1, [], []⟩ [(z, ⟨1⟩)] := by
  rintro ⟨t, ht, _, table, _⟩
  cases ht
  cases table

/-- Extra path mutant: use only the suffix, discarding the parent's projection. -/
def replacePath (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, suffix, m, some i,
        i :: a.ancestors, activation, false, false⟩ :: s.loans }
  else none

/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case `upSource`, subject
`ConcreteSpec.Core.Own.reborrowTo`, mutant `replacePath`). Independent path-family attempt, kernel-rejected. -/
theorem downward_reborrow_protocol_planted_8 : ¬ DownwardProtocol replacePath upSource ⟨0⟩ [.index 1] .mutable 1 0 := by
  rintro ⟨_, t, ht, _, _, _, _, _, _, l, hl, hid, _, a, found, _, path, _⟩
  cases ht
  cases found
  change l ∈ [_, _] at hl
  rcases List.mem_cons.mp hl with he | hl
  · subst l
    cases path
  · simp only [List.mem_singleton] at hl
    subst l
    cases hid
/-- Extra progress mutant: refuse shared call returns alone. -/
def refuseShared (s : State) : Option State := do
  let f ← s.frames.head?
  if s.loans.any (fun b => b.activation == f.id && b.mode == .shared) then none else leave s
/-- Instance (law `ConcreteSpec.Core.Own.return_progress`, case `sharedStore`, subject `ConcreteSpec.Core.Own.leave`).
-/
theorem return_progress_instance_2 : ReturnProgress leave sharedStore.own :=
  return_progress sharedStore.own ⟨1, [], [(z, ⟨1⟩)]⟩ (by decide) rfl (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.return_progress`, case `sharedStore`, subject `ConcreteSpec.Core.Own.leave`,
mutant `refuseShared`). Independent progress-family attempt in the other mode, kernel-rejected. -/
theorem return_progress_planted_3 : WellFormed sharedStore.own ∧ ValidReturn sharedStore.own ⟨1, [], [(z, ⟨1⟩)]⟩ ∧
    ¬ ReturnProgress refuseShared sharedStore.own := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨t, ht⟩
  cases ht

/-- Extra path mutant: adoption clears the child's existing projection during transfer. -/
def forgetAdoptionPath (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then { l with path := [], activation := f.id, pendingArgument := false } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none


/-- Planted (law `ConcreteSpec.Core.Own.adopted_view_usable`, case `relendBefore`, subject
`ConcreteSpec.Core.Own.adoptDown`, mutant `forgetAdoptionPath`). This fourth extra mutant survived the
intermediate outputs;
the exact transfer-table output now rules it out without an additional premise. -/
theorem adopted_view_usable_planted_8 : ¬ AdoptedView forgetAdoptionPath relendBefore ⟨1, [], []⟩ [(z, ⟨1⟩)] := by
  rintro ⟨t, ht, _, table, _⟩
  cases ht
  cases table
end Round4Evidence

namespace Round5Evidence
open Examples DirectArgumentWitnesses DeferredLifetimeEvidence Round3Evidence
/-- Mutant: inherit the parent mode instead of the requested mode. -/
def inheritMode (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, a.mode, some i,
        i :: a.ancestors, activation, false, false⟩ :: s.loans }
  else none
/-- Mutant: silently upgrade a shared request when mutable parent access is available. -/
def upgradeModeWhenClear (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, (if loanAccess s i .mutable then .mutable else m), some i,
        i :: a.ancestors, activation, false, false⟩ :: s.loans }
  else none
/-- Mutant: append a bogus index while passing an existing view. -/
def passDownBogusSuffix (s : CallState) (source : LoanId) (parameter : Var) : Option CallState := do
  if parameter ∈ s.pending.map Prod.fst then none else do
  let l ← loan s.own source
  let own ← reborrow s.own source [.index 0] l.mode
  let own := { own with loans := own.loans.map fun l =>
    if l.id.id = s.own.nextLoan then { l with pendingArgument := true } else l }
  pure ⟨own, s.pending ++ [(parameter, ⟨s.own.nextLoan⟩)]⟩
/-- Mutant: transfer only the first pending argument to the callee activation. -/
def adoptFirstActivation (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then
          { l with
            activation := if args.head?.map Prod.snd == some l.id then f.id else l.activation
            pendingArgument := false } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none
/-- Mutant: install the arguments in the caller frame after ordinary admission. -/
def adoptCallerSwap (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    let g := s.frames.tail.headD f
    let tail := if s.frames.tail.isEmpty then [] else f :: s.frames.tail.tail
    some { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then { l with activation := g.id, pendingArgument := false } else l
      frames := { g with views := args ++ g.views } :: tail }
  else none

/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant
`ConcreteSpec.Core.Own.Round5Evidence.inheritMode`). The shared request cannot become mutable. -/
theorem downward_reborrow_protocol_planted_9 : WellFormed LifecycleWitnesses.called ∧
    loanAccess LifecycleWitnesses.called ⟨1⟩ .shared = true ∧
    ¬ DownwardProtocol inheritMode LifecycleWitnesses.called ⟨1⟩ [] .shared 1 0 := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨_, t, ht, _, _, _, _, _, _, l, hl, hid, _, a, found, _, _, _, _, mode⟩
  cases ht
  cases found
  change l ∈ [_, _, _] at hl
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hl
  rcases hl with h | h | h <;> subst l
  · cases mode
  · cases hid
  · cases hid
/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant
`ConcreteSpec.Core.Own.Round5Evidence.upgradeModeWhenClear`). The shared request cannot become mutable. -/
theorem downward_reborrow_protocol_planted_10 : WellFormed LifecycleWitnesses.called ∧
    loanAccess LifecycleWitnesses.called ⟨1⟩ .shared = true ∧
    ¬ DownwardProtocol upgradeModeWhenClear LifecycleWitnesses.called ⟨1⟩ [] .shared 1 0 := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨_, t, ht, _, _, _, _, _, _, l, hl, hid, _, a, found, _, _, _, _, mode⟩
  cases ht
  cases found
  change l ∈ [_, _, _] at hl
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hl
  rcases hl with h | h | h <;> subst l
  · cases mode
  · cases hid
  · cases hid
/-- Planted (law `ConcreteSpec.Core.Own.passDown_protocol`, case `relendCaller`,
subject `ConcreteSpec.Core.Own.passDown`, mutant `ConcreteSpec.Core.Own.Round5Evidence.passDownBogusSuffix`).
The exact child projection rejects a corruption that successful return would erase. -/
private theorem bogusSuffix_rejected : WellFormed relendCaller.own ∧
    loanAccess relendCaller.own ⟨0⟩ .mutable = true ∧
    ¬ PassedView passDownBogusSuffix relendCaller ⟨0⟩ z .mutable := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨t, ht, _, _, _, a, l, found, child, _, _, path, _⟩
  cases ht
  cases found
  cases child
  cases path
/-- Planted (law `ConcreteSpec.Core.Own.plain_lending_roundtrip`, case `relendCaller`,
subject `ConcreteSpec.Core.Own.passDown`, mutant `ConcreteSpec.Core.Own.Round5Evidence.passDownBogusSuffix`).
Substitution in both the child obligation and the full call rejects the same mutant. -/
theorem plain_lending_roundtrip_planted_2 : ¬ PlainLendingRoundtrip passDownBogusSuffix relendCaller.own ⟨0⟩ z .mutable types := by
  intro h
  exact bogusSuffix_rejected.2.2 h.1

/-- A caller with two distinct owned roots and an existing mutable view of the first root. -/
def twoCaller : CallState := ⟨{ caller.own with loans := exclusive.loans }, []⟩
/-- The pending buffer contains a re-lent mutable view and a direct-owner mutable argument. -/
def twoWaiting : CallState :=
  (passOwner ((passDown twoCaller ⟨0⟩ y).getD twoCaller) ⟨1⟩ [] .mutable z).getD twoCaller
/-- Ordinary entry makes activation 1 current before the entire pending buffer is adopted. -/
def twoBefore : State := (enter types [] twoWaiting.own).getD initial
/-- Instance (law `ConcreteSpec.Core.Own.adopted_view_usable`, case `twoBefore`,
subject `ConcreteSpec.Core.Own.adoptDown`). Both arguments satisfy every admission premise. -/
theorem adopted_view_usable_instance_5 : twoWaiting.pending = [(y, ⟨1⟩), (z, ⟨2⟩)] ∧
    WellFormed twoBefore ∧ AdoptedView adoptDown twoBefore ⟨1, [], []⟩ twoWaiting.pending :=
  ⟨rfl, by decide, adopted_view_usable twoBefore _ _ rfl (by decide) (by decide) (by decide)
    (by decide) (by decide)⟩
/-- Planted (law `ConcreteSpec.Core.Own.adopted_view_usable`, case `relendBefore`,
subject `ConcreteSpec.Core.Own.adoptDown`, mutant `ConcreteSpec.Core.Own.Round5Evidence.adoptCallerSwap`).
The input callee frame cannot be replaced by the caller, even when the resulting loan is usable. -/
theorem adopted_view_usable_planted_9 : ¬ AdoptedView adoptCallerSwap relendBefore ⟨1, [], []⟩ [(z, ⟨1⟩)] := by
  rintro ⟨t, ht, frames, _, _⟩
  cases ht
  cases frames
/-- Planted (law `ConcreteSpec.Core.Own.adopted_view_usable`, case `twoBefore`,
subject `ConcreteSpec.Core.Own.adoptDown`, mutant `ConcreteSpec.Core.Own.Round5Evidence.adoptFirstActivation`).
Every pending argument, including the second one, must enter the callee activation. -/
theorem adopted_view_usable_planted_10 : ¬ AdoptedView adoptFirstActivation twoBefore ⟨1, [], []⟩ twoWaiting.pending := by
  rintro ⟨t, ht, _, table, _⟩
  cases ht
  cases table
end Round5Evidence


/-! ## Dispatcher admission and progress -/

/-- An admitted event must produce a result. -/
def Advances {α : Type} (admitted : Prop) (result : Option α) : Prop :=
  admitted → ∃ t, result = some t

/-- A dispatcher must complete admitted events and preserve safety on every returned result. -/
def Dispatches {α : Type} (admitted : Prop) (safe : α → Prop) (result : Option α) : Prop :=
  Advances admitted result ∧ ResultSatisfies safe result

/-- Owning parameters designate distinct fresh locals with checked descriptors. -/
def AdmittedEntry (types : Var → Option Descriptor) (params : List Var) : Prop :=
  params.Nodup ∧ ∀ x ∈ params, ∃ d, types x = some d

/-- The departing activation must discharge storage and close its local loan graph. -/
def AdmittedLeave (s : State) : Prop :=
  WellFormed s ∧ ∃ f, s.frames.head? = some f ∧ ValidReturn s f

/-- Exhaustive admission for the ownership-only lifecycle interface. -/
def AdmittedLife (types : Var → Option Descriptor) (s : State) : Exec.Life → Prop
  | .bind x => ∃ f, AdmittedBinding types s x f
  | .enter params => AdmittedEntry types params
  | .leave => AdmittedLeave s
  | .drop xs => ∃ f, AdmittedDrop s xs f

/-- Pending view operands must match the signature, be distinct and designate pending loans.
Only the remaining parameters allocate owner storage. -/
def AdmittedCallEntry (types : Var → Option Descriptor) (s : CallState)
    (params : List Var) : Prop :=
  params.Nodup ∧
  (∀ x ∈ params, x ∉ s.pending.map Prod.fst → ∃ d, types x = some d) ∧
  (s.pending.map Prod.fst).Nodup ∧ (s.pending.map Prod.snd).Nodup ∧
  ∀ e ∈ s.pending, e.1 ∈ params ∧
    ∃ l ∈ s.own.loans, l.id = e.2 ∧ l.pendingArgument = true

/-- Pending argument evaluation can cross only the matching call entry. -/
def AdmittedCallLife (types : Var → Option Descriptor) (s : CallState) : Exec.Life → Prop
  | .enter params => AdmittedCallEntry types s params
  | .bind x => s.pending = [] ∧ AdmittedLife types s.own (.bind x)
  | .leave => s.pending = [] ∧ AdmittedLife types s.own .leave
  | .drop xs => s.pending = [] ∧ AdmittedLife types s.own (.drop xs)

/-- Helper (authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal).
The ownership-only dispatcher completes every admitted lifecycle event. -/
theorem life_dispatch (types : Var → Option Descriptor) (s : State) (e : Exec.Life) :
    Dispatches (AdmittedLife types s e) (fun t => WellFormed s → WellFormed t)
      (life types e s) := by
  constructor
  · intro admitted
    cases e with
    | bind x =>
      obtain ⟨f, hf⟩ := admitted
      obtain ⟨t, ht, _⟩ := bind_retired_place types s x f hf
      exact ⟨t, ht⟩
    | enter params => exact enter_progress types s params admitted.1 admitted.2
    | leave =>
      obtain ⟨hs, f, hf, valid⟩ := admitted
      exact return_progress s f hs hf valid
    | drop xs =>
      obtain ⟨f, hf⟩ := admitted
      obtain ⟨t, ht, _⟩ := drop_retired_place s xs f hf
      exact ⟨t, ht⟩
  · cases h : life types e s with
    | none => trivial
    | some t =>
      intro hs
      have safe := life_preserves types e s hs
      rwa [h] at safe

/-- Helper (fold induction). Binding the owned parameters leaves all other names free in
the fresh activation and preserves its issued identity. -/
private theorem entry_frame (types : Var → Option Descriptor) (xs : List Var)
    (s t : State) (f : Activation) (hf : s.frames.head? = some f)
    (issued : f.id < s.nextActivation)
    (h : xs.foldlM (fun s x => bind types x s) s = some t) :
    ∃ g, t.frames.head? = some g ∧ g.id < t.nextActivation ∧
      g.views = f.views ∧
      ∀ x, x ∉ xs → AList.lookup f.locals x = none → AList.lookup g.locals x = none := by
  induction xs generalizing s f with
  | nil =>
    simp at h
    subst t
    exact ⟨f, hf, issued, rfl, fun _ _ free => free⟩
  | cons x xs ih =>
    simp only [List.foldlM_cons] at h
    obtain ⟨u, hu, ht⟩ := Option.bind_eq_some_iff.mp h
    unfold bind at hu
    rw [hf] at hu
    dsimp at hu
    split at hu <;> try contradiction
    split at hu
    · unfold reinitialize at hu
      obtain ⟨a, _, hu⟩ := Option.bind_eq_some_iff.mp hu
      split at hu
      · cases hu
        obtain ⟨g, hg, hi, views, free⟩ := ih _ f (by exact hf) (by exact issued) ht
        exact ⟨g, hg, hi, views, fun y hy => free y (fun hm => hy (List.mem_cons_of_mem _ hm))⟩
      · contradiction
    · obtain ⟨d, _, hu⟩ := Option.bind_eq_some_iff.mp hu
      simp at hu
      subst u
      obtain ⟨g, hg, hi, views, free⟩ := ih _ { f with locals := (x, ⟨s.nextPlace⟩) :: f.locals } (by rfl) (by exact issued) ht
      refine ⟨g, hg, hi, views, fun y hy old => free y (fun hm => hy (List.mem_cons_of_mem _ hm)) ?_⟩
      have ne : x ≠ y := fun eq => hy (eq ▸ List.mem_cons_self)
      simp [AList.lookup, ne, old]

/-- Preserve ownership, the pending argument buffer invariant, and retired identities. -/
def CallLifeSafe (s t : CallState) : Prop :=
  (WellFormed s.own → WellFormed t.own) ∧
  (s.WellFormed → t.WellFormed) ∧
  (∀ p, RetiredPlace s.own p → RetiredPlace t.own p) ∧
  (∀ i, RetiredLoan s.own i → RetiredLoan t.own i)

/-- Helper (authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal; #od23).
The C1 adapter completes every admitted lifecycle event, including mixed-parameter calls. -/
theorem callLife_dispatch (types : Var → Option Descriptor) (s : CallState) (e : Exec.Life) :
    Dispatches (AdmittedCallLife types s e)
      (CallLifeSafe s) (callLife types e s) := by
  constructor
  · intro admitted
    have plain (e : Exec.Life) (h : AdmittedLife types s.own e)
        (adapt : callLife types e s = (life types e s.own).map
          (fun own => { s with own := own })) : ∃ t, callLife types e s = some t := by
      obtain ⟨u, hu⟩ := (life_dispatch types s.own e).1 h
      exact ⟨{ s with own := u }, by simp [adapt, hu]⟩
    cases e with
    | bind x => exact plain _ admitted.2 (by simp [callLife, admitted.1])
    | leave => exact plain _ admitted.2 (by simp [callLife, admitted.1])
    | drop xs => exact plain _ admitted.2 (by simp [callLife, admitted.1])
    | enter params =>
      obtain ⟨unique, typed, names, ids, pending⟩ := admitted
      let owned := params.filter fun x => decide (x ∉ s.pending.map Prod.fst)
      obtain ⟨u, hu⟩ := enter_progress types s.own owned (unique.filter _) (by
        intro x hx
        have hm := List.mem_filter.mp hx
        exact typed x hm.1 (of_decide_eq_true hm.2))
      have loans := enter_loans types s.own u owned hu
      have fold := hu
      unfold enter at fold
      split at fold <;> try contradiction
      change owned.foldlM (fun s x => bind types x s)
        { s.own with nextActivation := s.own.nextActivation + 1
                     frames := ⟨s.own.nextActivation, [], []⟩ :: s.own.frames } = some u at fold
      obtain ⟨f, frame, issued, views, free⟩ := entry_frame types owned _ u
        ⟨s.own.nextActivation, [], []⟩ rfl (Nat.lt_succ_self _) fold
      obtain ⟨v, hv, _⟩ := adopted_view_usable u f s.pending frame issued names ids
        (fun e he => by rw [loans]; exact (pending e he).2)
        (fun e he => ⟨free e.1 (by simp [owned, List.mem_map_of_mem he]) rfl,
          by simp [ParameterAvailable, views]⟩)
      have supplied : params.Nodup ∧ ∀ e ∈ s.pending, e.1 ∈ params :=
        ⟨unique, fun e he => (pending e he).1⟩
      refine ⟨⟨v, []⟩, ?_⟩
      simp only [callLife]
      split
      all_goals first | exact False.elim (‹¬ _› supplied) | skip
      change (enter types owned s.own >>= fun own =>
        adoptDown own s.pending >>= fun own => some (CallState.mk own [])) = some (CallState.mk v [])
      simp [hu, hv]
  · cases h : callLife types e s with
    | none => trivial
    | some t =>
      refine ⟨?_, ?_, ?_, ?_⟩
      · intro hs
        have safe := callLife_preserves types e s hs
        rwa [h] at safe
      · intro hs
        have safe := callLife_call_preserves types e s hs
        rwa [h] at safe
      · intro p hp
        have safe := callLife_retired_place types s e p hp
        rwa [h] at safe
      · intro i hi
        have safe := callLife_retired_loan types s e i hi
        rwa [h] at safe

/-- Helper (authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal).
Transporting the lifecycle through a concrete store preserves its admission and safety. -/
theorem callLifeStore_dispatch {σ : Type} (types : Var → Option Descriptor)
    (s : σ × CallState) (e : Exec.Life) :
    Dispatches (AdmittedCallLife types s.2 e)
      (fun t => t.1 = s.1 ∧ CallLifeSafe s.2 t.2) (callLifeStore types e s) := by
  have law := callLife_dispatch types s.2 e
  constructor
  · intro h
    obtain ⟨t, ht⟩ := law.1 h
    exact ⟨(s.1, t), by simp [callLifeStore, ht]⟩
  · unfold callLifeStore
    cases h : callLife types e s.2 with
    | none => trivial
    | some t => exact ⟨rfl, by simpa only [h, ResultSatisfies] using law.2⟩


/-- A finite protocol is admitted when its independent ledger rules compose in order. -/
inductive AdmittedTrace {α β : Type} (rule : α → β → α → Prop) : α → List β → Prop
  | nil (s) : AdmittedTrace rule s []
  | cons {s e u es} : rule s e u → AdmittedTrace rule u es → AdmittedTrace rule s (e :: es)

/-- Helper (induction). A fold completes whenever each ledger rule has its required successor. -/
private theorem admitted_fold {α β : Type} {rule : α → β → α → Prop}
    (step : α → β → Option α)
    (implements : ∀ s e t, rule s e t → step s e = some t)
    {s : α} {es : List β} (h : AdmittedTrace rule s es) :
    ∃ t, es.foldlM step s = some t := by
  induction h with
  | nil s => exact ⟨s, rfl⟩
  | cons h _ ih =>
    obtain ⟨t, ht⟩ := ih
    exact ⟨t, by simp [List.foldlM_cons, implements _ _ _ h, ht]⟩

/-- The loan ledger adds a fresh grant or removes an ended leaf. The graph premises are
the same single-event admission used by the retirement Law. -/
inductive LoanLedger : State → LoanAction → State → Prop
  | borrow {s p path m f}
      (h : AdmittedLoanAction s (.borrow p path m))
      (frame : s.frames.head? = some f) :
      LoanLedger s (.borrow p path m) { s with
        nextLoan := s.nextLoan + 1
        loans := ⟨⟨s.nextLoan⟩, p, path, m, none, [], f.id, false, false⟩ :: s.loans }
  | reborrow {s i path m activation a}
      (h : AdmittedLoanAction s (.reborrow i path m activation))
      (parent : loan s i = some a) :
      LoanLedger s (.reborrow i path m activation) { s with
        nextLoan := s.nextLoan + 1
        loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ path, m, some i, i :: a.ancestors,
          activation, false, false⟩ :: s.loans }
  | endLoan {s i} (h : AdmittedLoanAction s (.endLoan i)) :
      LoanLedger s (.endLoan i) { s with loans := s.loans.filter (·.id != i) }

/-- Helper (computation). Loan actions realize the independent ledger rules. -/
private theorem loanLedger_step (s : State) (e : LoanAction) (t : State)
    (h : LoanLedger s e t) : loanAction s e = some t := by
  cases h with
  | borrow admitted frame =>
    obtain ⟨_, hf, issued, allowed⟩ := admitted
    rw [frame] at hf
    cases hf
    simp [loanAction, borrow, frame, issued, (LegalStep.ownerAccess_iff ..).mpr allowed]
  | reborrow admitted parent =>
    obtain ⟨_, _, issued, current, allowed⟩ := admitted
    simp [loanAction, reborrowTo, parent, issued, current, (LegalStep.loanAccess_iff ..).mpr allowed]
  | @endLoan i admitted =>
    obtain ⟨a, found, allowed, children⟩ := admitted
    have clear : s.loans.any (·.parent == some i) = false := by
      apply Bool.eq_false_iff.mpr
      intro h
      obtain ⟨b, hb, eq⟩ := List.any_eq_true.mp h
      exact children b hb (by simpa using eq)
    simp [loanAction, endLoan, allowed, endLeafAtBoundary, found, clear]

/-- Helper (authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal; #od15).
Every admitted loan trace completes and all completed traces preserve retirement. -/
theorem runLoanSequence_dispatch (s : State) (ops : List LoanAction) :
    Dispatches (AdmittedTrace LoanLedger s ops)
      (fun t => ∀ i, RetiredLoan s i → RetiredLoan t i) (runLoanSequence s ops) := by
  refine ⟨admitted_fold loanAction loanLedger_step, ?_⟩
  cases h : runLoanSequence s ops with
  | none => trivial
  | some t =>
    intro i hr
    have safe := ended_loans_never_revive s ops i hr
    rwa [h] at safe

/-- A downward parameter lends at the source mode into the new current activation.
The next argument sees the extended loan graph and therefore the earlier argument's freeze. -/
inductive DownwardParameter : State → Var × LoanId → State → Prop
  | lend {s x i a f u}
      (frame : s.frames.head? = some f)
      (source : loan s i = some a)
      (available : AList.lookup f.locals x = none ∧ ParameterAvailable f.views x)
      (grant : LoanLedger s (.reborrow i [] a.mode f.id) u) :
      DownwardParameter s (x, i)
        { u with frames := { f with views := (x, ⟨s.nextLoan⟩) :: f.views } :: s.frames.tail }

/-- Standalone entry introduces a fresh activation followed by its downward grants. -/
def AdmittedDownwardEntry (s : State) (args : List (Var × LoanId)) : Prop :=
  CallerSources s args ∧ AdmittedTrace DownwardParameter
    { s with nextActivation := s.nextActivation + 1
             frames := ⟨s.nextActivation, [], []⟩ :: s.frames } args

/-- Helper (authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal;
#decided-2026-11-08-recovered-d3). Every admitted standalone downward entry completes. -/
theorem enterDown_dispatch (s : State) (args : List (Var × LoanId)) :
    Dispatches (AdmittedDownwardEntry s args)
      (fun t => (WellFormed s → WellFormed t) ∧
        ∀ i, RetiredLoan s i → RetiredLoan t i) (enterDown s args) := by
  constructor
  · intro h
    unfold enterDown
    split
    all_goals first | exact False.elim (‹¬ _› h.1) | skip
    simp only [enter, List.nodup_nil, not_true_eq_false, ite_false, List.foldlM_nil]
    dsimp
    apply admitted_fold _ _ h.2
    intro v e t rule
    cases rule with
    | lend frame source available grant =>
      have hg := loanLedger_step _ _ _ grant
      cases grant
      simp [loanAction] at hg
      simp [enterDownParameter, frame, source, available]
      simp [hg]
  · cases h : enterDown s args with
    | none => trivial
    | some t =>
      constructor
      · intro hs
        have safe := enterDown_preserves s args hs
        rwa [h] at safe
      · intro i hr
        have safe := enterDown_retired s args i hr
        rwa [h] at safe

/-- View operands need an available source grant and an unsupplied parameter. -/
def AdmittedViewArgument (s : CallState) (i : LoanId) (x : Var) : Prop :=
  x ∉ s.pending.map Prod.fst ∧ ∃ a f, loan s.own i = some a ∧
    s.own.frames.head? = some f ∧ f.id < s.own.nextActivation ∧
    LegalStep.LoanPermission s.own i a.mode

/-- Helper (authority: docs/decisions.md#od23; #decided-2026-11-13-lifecycle-refusal).
View argument evaluation completes at every available source, including with earlier pending arguments. -/
theorem passDown_dispatch (s : CallState) (i : LoanId) (x : Var) :
    Dispatches (AdmittedViewArgument s i x)
      (fun t => (WellFormed s.own → WellFormed t.own) ∧ (s.WellFormed → t.WellFormed))
      (passDown s i x) := by
  constructor
  · rintro ⟨free, a, f, found, frame, issued, access⟩
    exact ⟨_, by simp [passDown, free, found, frame, issued,
      (LegalStep.loanAccess_iff ..).mpr access] <;> rfl⟩
  · cases h : passDown s i x with
    | none => trivial
    | some t =>
      constructor
      · intro hs
        have safe := passDown_preserves s i x hs
        rwa [h] at safe
      · intro hs
        have safe := passDown_call_preserves s i x hs
        rwa [h] at safe

/-- Exhaustive admission for the mixed issuance history alphabet. -/
def AdmittedIssuance (types : Var → Option Descriptor) (s : CallState) :
    IssuanceAction → Prop
  | .ordinary op => OrdinaryIssuanceAllowed s op ∧ AdmittedLoanAction s.own op
  | .owner p _ m x => AdmittedOwnerArgument s p m x
  | .view i x => AdmittedViewArgument s i x
  | .entry args => s.pending = [] ∧ AdmittedDownwardEntry s.own args
  | .lifecycle e => AdmittedCallLife types s e

/-- Ownership well-formedness and identity retirement at the mixed issuance boundary. -/
def IssuanceSafe (s t : CallState) : Prop :=
  (WellFormed s.own → WellFormed t.own) ∧ ∀ i, RetiredLoan s.own i → RetiredLoan t.own i

/-- Helper (authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal; #od15; #od23).
The mixed dispatcher completes every admitted issuance event and preserves retirement. -/
theorem issuanceStep_dispatch (types : Var → Option Descriptor) (s : CallState)
    (e : IssuanceAction) :
    Dispatches (AdmittedIssuance types s e)
      (IssuanceSafe s) (issuanceStep types s e) := by
  constructor
  · intro h
    cases e with
    | ordinary op =>
      obtain ⟨t, ht⟩ := (loanSequence_retired_step s.own op).2.1 h.2
      exact ⟨_, by simp [issuanceStep, h.1, ht] <;> rfl⟩
    | owner p path m x =>
      obtain ⟨t, ht, _⟩ := call_birth_retired s p path m x h
      exact ⟨t, ht⟩
    | view i x => exact (passDown_dispatch s i x).1 h
    | entry args =>
      obtain ⟨t, ht⟩ := (enterDown_dispatch s.own args).1 h.2
      exact ⟨_, by simp [issuanceStep, h.1, ht] <;> rfl⟩
    | lifecycle e => exact (callLife_dispatch types s e).1 h
  · cases h : issuanceStep types s e with
    | none => trivial
    | some t =>
      constructor
      · intro hs
        cases e with
        | ordinary op =>
          have safe : StateResult (loanAction s.own op) := by
            cases op with
            | borrow p path m => exact borrow_preserves s.own p path m hs
            | reborrow i path m a => exact reborrowTo_preserves s.own i path m a hs
            | endLoan i => exact endLoan_preserves s.own i hs
          simp only [issuanceStep] at h
          split at h
          · obtain ⟨u, hu, rfl⟩ := Option.map_eq_some_iff.mp h
            rwa [hu] at safe
          · contradiction
        | owner p path m x =>
          have safe := passOwner_preserves s p path m x hs
          change passOwner s p path m x = some t at h
          rwa [h] at safe
        | view i x =>
          have safe := passDown_preserves s i x hs
          change passDown s i x = some t at h
          rwa [h] at safe
        | entry args =>
          have safe := enterDown_preserves s.own args hs
          simp only [issuanceStep] at h
          split at h
          · obtain ⟨u, hu, rfl⟩ := Option.map_eq_some_iff.mp h
            rwa [hu] at safe
          · contradiction
        | lifecycle e =>
          have safe := callLife_preserves types e s hs
          change callLife types e s = some t at h
          rwa [h] at safe
      · intro i hr
        have safe := issuance_history_nonrevival types s [e] i hr
        simpa [List.foldlM_cons, h, ResultSatisfies] using safe

/-- Helper (constructor enumeration). Each match is exhaustive without a fallback,
so extending any event alphabet requires an explicit admission obligation. -/
theorem admission_cases (types : Var → Option Descriptor) (s : CallState) :
    (∀ e, AdmittedLife types s.own e ↔ match e with
      | .bind x => ∃ f, AdmittedBinding types s.own x f
      | .enter ps => AdmittedEntry types ps
      | .leave => AdmittedLeave s.own
      | .drop xs => ∃ f, AdmittedDrop s.own xs f) ∧
    (∀ e, AdmittedCallLife types s e ↔ match e with
      | .bind x => s.pending = [] ∧ AdmittedLife types s.own (.bind x)
      | .enter ps => AdmittedCallEntry types s ps
      | .leave => s.pending = [] ∧ AdmittedLeave s.own
      | .drop xs => s.pending = [] ∧ AdmittedLife types s.own (.drop xs)) ∧
    (∀ e, AdmittedLoanAction s.own e ↔ match e with
      | .borrow p _ m => ∃ f, s.own.frames.head? = some f ∧
          f.id < s.own.nextActivation ∧ LegalStep.OwnerPermission s.own p m
      | .reborrow i _ m a => ∃ parent, loan s.own i = some parent ∧
          a < s.own.nextActivation ∧ s.own.frames.head?.map Activation.id = some a ∧
          LegalStep.LoanPermission s.own i m
      | .endLoan i => ∃ a, loan s.own i = some a ∧ OrdinaryLoanEnd s.own i ∧ ∀ b ∈ s.own.loans, b.parent ≠ some i) ∧
    (∀ e, AdmittedIssuance types s e ↔ match e with
      | .ordinary op => OrdinaryIssuanceAllowed s op ∧ AdmittedLoanAction s.own op
      | .owner p _ m x => AdmittedOwnerArgument s p m x
      | .view i x => AdmittedViewArgument s i x
      | .entry args => s.pending = [] ∧ AdmittedDownwardEntry s.own args
      | .lifecycle e => AdmittedCallLife types s e) :=
  ⟨fun _ => Iff.rfl, by intro e; cases e <;> rfl, fun _ => Iff.rfl, fun _ => Iff.rfl⟩

/-- Adoption uses a fresh issued destination and distinct pending loans and names. -/
def AdmittedAdoption (s : State) (args : List (Var × LoanId)) : Prop :=
  ∃ f, s.frames.head? = some f ∧ f.id < s.nextActivation ∧
    (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
    (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
    (∀ e ∈ args, AList.lookup f.locals e.1 = none ∧ ParameterAvailable f.views e.1)

/-- Helper (authority: docs/decisions.md#od23; #decided-2026-11-13-lifecycle-refusal).
Every admitted adoption completes and preserves ownership. -/
theorem adoptDown_dispatch (s : State) (args : List (Var × LoanId)) :
    Dispatches (AdmittedAdoption s args) (fun t => WellFormed s → WellFormed t)
      (adoptDown s args) := by
  constructor
  · rintro ⟨f, frame, issued, names, ids, pending, free⟩
    obtain ⟨t, ht, _⟩ := adopted_view_usable s f args frame issued names ids pending free
    exact ⟨t, ht⟩
  · cases h : adoptDown s args with
    | none => trivial
    | some t =>
      intro hs
      have safe := adoptDown_preserves s args hs
      rwa [h] at safe

/-- Only an unborrowed linear owner may be consumed. -/
def AdmittedMove (s : State) (p : Hybrid.Place) : Prop :=
  ∃ a, slot s p = some a ∧ a.desc.copy = false ∧ a.status = .owned ∧
    ∀ l ∈ s.loans, l.root ≠ p

/-- Helper (authority: docs/decisions.md#od60; #od62).
Every admitted move completes and preserves ownership. -/
theorem move_dispatch (s : State) (p : Hybrid.Place) :
    Dispatches (AdmittedMove s p) (fun t => WellFormed s → WellFormed t) (move s p) := by
  constructor
  · rintro ⟨a, found, linear, owned, clear⟩
    exact move_progress s p a found linear owned clear
  · cases h : move s p with
    | none => trivial
    | some t =>
      intro hs
      have safe := move_preserves s p hs
      rwa [h] at safe

namespace DispatchEvidence
open Examples DirectArgumentWitnesses

/-- The fixture lends a mutable view into a fresh standalone activation. -/
def singletonEntry : List (Var × LoanId) := [(y, ⟨0⟩)]

/-- Regression (admission for `enterDown_dispatch` and `issuanceStep_dispatch`). -/
private theorem singletonAdmitted : AdmittedDownwardEntry exclusive singletonEntry := by
  refine ⟨by decide, ?_⟩
  apply AdmittedTrace.cons
  · apply DownwardParameter.lend rfl (a := exclusive.loans.head (by decide)) rfl (by decide)
    exact LoanLedger.reborrow ⟨_, rfl, by decide, rfl, ⟨_, rfl, rfl, Or.inr rfl, by decide⟩⟩ rfl
  · exact .nil _

/-- Mutant: accept an empty standalone entry only. -/
def refuseStandaloneEntry (s : State) (args : List (Var × LoanId)) : Option State :=
  if args.isEmpty then enterDown s args else none

/-- Mutant: refuse nested zero-argument calls at the adapter. -/
def refuseEmptyCallEntry (e : Exec.Life) (s : CallState) : Option CallState :=
  match e with
  | .enter [] => if s.own.frames.length ≥ 2 then none else callLife types e s
  | _ => callLife types e s

/-- Mutant: refuse result binding at the call adapter. -/
def refuseCallBinding (e : Exec.Life) (s : CallState) : Option CallState :=
  match e with
  | .bind _ => none
  | _ => callLife types e s

/-- Mutant: refuse entry with multiple pending view arguments. -/
def refuseMixedCallEntry (e : Exec.Life) (s : CallState) : Option CallState :=
  match e with
  | .enter _ => if s.pending.length > 1 then none else callLife types e s
  | _ => callLife types e s

/-- Mutant: refuse return with allocated local storage. -/
def refuseNonemptyCallReturn (e : Exec.Life) (s : CallState) : Option CallState :=
  match e with
  | .leave =>
    if s.own.frames.head?.any (fun f => !f.locals.isEmpty) then none else callLife types e s
  | _ => callLife types e s

/-- Mutant: refuse return when the frame has multiple loans to end. -/
def refuseMultipleLoanReturn (e : Exec.Life) (s : CallState) : Option CallState :=
  match e with
  | .leave =>
    if s.own.frames.head?.any (fun f => (s.own.loans.filter (·.activation == f.id)).length > 1)
    then none else callLife types e s
  | _ => callLife types e s

/-- Mutant: refuse binding in the ownership-only dispatcher. -/
def refuseLifeBind (e : Exec.Life) (s : State) : Option State :=
  match e with
  | .bind _ => none
  | _ => life types e s

/-- Mutant: refuse entry in the ownership-only dispatcher. -/
def refuseLifeEnter (e : Exec.Life) (s : State) : Option State :=
  match e with
  | .enter _ => none
  | _ => life types e s

/-- Mutant: refuse adoption after the first argument. -/
def refuseMultiAdopt (s : State) (args : List (Var × LoanId)) : Option State :=
  if args.length > 1 then none else adoptDown s args

/-- Mutant: refuse consumption outside place zero. -/
def refuseNonzeroMove (s : State) (p : Hybrid.Place) : Option State :=
  if p.id = 0 then move s p else none

/-- Mutant: refuse a view if an earlier view argument is pending. -/
def refuseSecondPendingView (s : CallState) (i : LoanId) (x : Var) : Option CallState :=
  if s.pending.isEmpty then passDown s i x else none

/-- Helper (inversion). A refused admitted event contradicts progress. -/
private theorem refuses {α : Type} {admitted : Prop} {result : Option α}
    (h : admitted) (hr : result = none) : ¬ Advances admitted result := by
  intro advance
  obtain ⟨t, ht⟩ := advance h
  rw [hr] at ht
  cases ht

/-- Regression (for `enterDown_dispatch`, case `singletonEntry`). -/
example : Advances (AdmittedDownwardEntry exclusive singletonEntry)
    (enterDown exclusive singletonEntry) :=
  (enterDown_dispatch _ _).1

/-- Regression (for `enterDown_dispatch`, mutant `refuseStandaloneEntry`). -/
example : ¬ Advances (AdmittedDownwardEntry exclusive singletonEntry)
    (refuseStandaloneEntry exclusive singletonEntry) :=
  refuses singletonAdmitted rfl

/-- The nested call starts inside the active deferred callee. -/
def nestedCaller := (Round3Evidence.nestedStart (callLife types)).store

/-- Instance (admission for a zero-argument call). -/
private theorem emptyEntry (s : CallState) (h : s.pending = []) :
    AdmittedCallEntry types s [] := by
  simp [AdmittedCallEntry, h]

/-- Regression (for `callLife_dispatch`, case `nestedCaller`). -/
example : Advances (AdmittedCallLife types nestedCaller (.enter []))
    (callLife types (.enter []) nestedCaller) := (callLife_dispatch _ _ _).1

/-- Regression (for `callLife_dispatch`, mutant `refuseEmptyCallEntry`).
Actual machine entry stalls when the adapter refuses the nested call. -/
example : ¬ Advances (AdmittedCallLife types nestedCaller (.enter []))
      (refuseEmptyCallEntry (.enter []) nestedCaller) ∧
    (Exec.step (Round3Evidence.nestedProgram (callLife types))
      (Round3Evidence.nestedStart (callLife types))).isSome = true ∧
    Exec.step (Round3Evidence.nestedProgram refuseEmptyCallEntry)
      (Round3Evidence.nestedStart refuseEmptyCallEntry) = none :=
  ⟨refuses (emptyEntry _ rfl) rfl, rfl, rfl⟩

/-- Instance (admission for binding a fresh local). -/
private theorem freshBind : AdmittedLife types caller.own (.bind z) :=
  ⟨caller.own.frames.head (by decide), rfl, by decide, by decide, _, rfl⟩

/-- A callee with a moved linear local still has storage to tear down. -/
def localReturn : CallState :=
  let entered := (enter types [z] initial).getD initial
  ⟨(move entered ⟨1⟩).getD entered, []⟩

/-- Instance (admission for return with a nonempty local table). -/
private theorem localReturnAdmitted : AdmittedCallLife types localReturn .leave :=
  ⟨rfl, by decide, ⟨1, [(z, ⟨1⟩)], []⟩, rfl, by decide⟩

/-- Instance (admission for return with a callee descendant loan). -/
private theorem descendantReturnAdmitted :
    AdmittedCallLife types ⟨Round4Evidence.localChild, []⟩ .leave :=
  ⟨rfl, by decide, ⟨1, [], [(z, ⟨1⟩)]⟩, rfl, by decide⟩

/-- Instance (admission for two adopted views and a fresh owning parameter). -/
private theorem mixedEntry : AdmittedCallEntry types Round5Evidence.twoWaiting [x, y, z] := by
  refine ⟨by decide, fun _ _ _ => ⟨_, rfl⟩, by decide, by decide, ?_⟩
  intro e he
  change e ∈ [(y, ⟨1⟩), (z, ⟨2⟩)] at he
  simp only [List.mem_cons, List.not_mem_nil, or_false] at he
  rcases he with rfl | rfl <;> decide

/-- Regression (for `callLife_dispatch`, mutants `refuseCallBinding`, `refuseMixedCallEntry`, `refuseNonemptyCallReturn`). -/
example :
    ¬ Advances (AdmittedCallLife types caller (.bind z))
      (refuseCallBinding (.bind z) caller) ∧
    ¬ Advances (AdmittedCallLife types Round5Evidence.twoWaiting (.enter [x, y, z]))
      (refuseMixedCallEntry (.enter [x, y, z]) Round5Evidence.twoWaiting) ∧
    ¬ Advances (AdmittedCallLife types localReturn .leave)
      (refuseNonemptyCallReturn .leave localReturn) ∧
    ¬ Advances (AdmittedCallLife types ⟨Round4Evidence.localChild, []⟩ .leave)
      (refuseMultipleLoanReturn .leave ⟨Round4Evidence.localChild, []⟩) :=
  ⟨refuses ⟨rfl, freshBind⟩ rfl, refuses mixedEntry rfl,
    refuses localReturnAdmitted rfl, refuses descendantReturnAdmitted rfl⟩

/-- Regression (for `life_dispatch`, case `caller`). -/
example : Advances (AdmittedLife types caller.own (.bind z)) (life types (.bind z) caller.own) :=
  (life_dispatch _ _ _).1

/-- Regression (for `life_dispatch`, mutants `refuseLifeBind` and `refuseLifeEnter`). -/
example : ¬ Advances (AdmittedLife types caller.own (.bind z))
      (refuseLifeBind (.bind z) caller.own) ∧
    ¬ Advances (AdmittedLife types caller.own (.enter []))
      (refuseLifeEnter (.enter []) caller.own) :=
  ⟨refuses freshBind rfl, refuses ⟨by decide, by simp⟩ rfl⟩

/-- Regression (for `adoptDown_dispatch`, case `Round5Evidence.twoBefore`). -/
example : Advances (AdmittedAdoption Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending)
    (adoptDown Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending) :=
  (adoptDown_dispatch _ _).1

/-- Regression (for `adoptDown_dispatch`, mutant `refuseMultiAdopt`). -/
example : ¬ Advances (AdmittedAdoption Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending)
    (refuseMultiAdopt Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending) :=
  refuses ⟨⟨1, [], []⟩, rfl, by decide, by decide, by decide, by decide, by decide⟩ rfl

/-- Regression (for `move_dispatch`, case `MoveLifecycleEvidence.source`). -/
example : Advances (AdmittedMove MoveLifecycleEvidence.source ⟨1⟩)
    (move MoveLifecycleEvidence.source ⟨1⟩) := (move_dispatch _ _).1

/-- Regression (for `move_dispatch`, mutant `refuseNonzeroMove`). -/
example : ¬ Advances (AdmittedMove MoveLifecycleEvidence.source ⟨1⟩)
    (refuseNonzeroMove MoveLifecycleEvidence.source ⟨1⟩) :=
  refuses ⟨MoveLifecycleEvidence.selected, rfl, rfl, rfl, by decide⟩ rfl

/-- Regression (for `issuanceStep_dispatch`, case `PendingEvidence.down`). -/
example : Advances (AdmittedIssuance types PendingEvidence.down (.view ⟨0⟩ y))
    (issuanceStep types PendingEvidence.down (.view ⟨0⟩ y)) := (issuanceStep_dispatch _ _ _).1

/-- Regression (for `passDown_dispatch`, case `PendingEvidence.down`). -/
example : Advances (AdmittedViewArgument PendingEvidence.down ⟨0⟩ y)
    (passDown PendingEvidence.down ⟨0⟩ y) := (passDown_dispatch _ _ _).1

/-- Regression (for `passDown_dispatch`, mutant `refuseSecondPendingView`). -/
example : ¬ Advances (AdmittedViewArgument PendingEvidence.down ⟨0⟩ y)
    (refuseSecondPendingView PendingEvidence.down ⟨0⟩ y) :=
  refuses ⟨by decide, shared.loans.head (by decide), ⟨0, [(x, ⟨0⟩)], []⟩,
    rfl, rfl, by decide, by decide⟩ rfl

/-- Regression (for `runLoanSequence_dispatch`, a live parent is ended before a fresh loan issues). -/
example :
    AdmittedTrace LoanLedger exclusive [.endLoan ⟨0⟩, .borrow ⟨0⟩ [] .mutable] ∧
    Advances (AdmittedTrace LoanLedger exclusive [.endLoan ⟨0⟩, .borrow ⟨0⟩ [] .mutable])
      (runLoanSequence exclusive [.endLoan ⟨0⟩, .borrow ⟨0⟩ [] .mutable]) := by
  refine ⟨?_, (runLoanSequence_dispatch _ _).1⟩
  apply AdmittedTrace.cons (LoanLedger.endLoan ⟨_, rfl, by decide⟩)
  exact .cons (LoanLedger.borrow ⟨_, rfl, by decide, by decide⟩ rfl) (.nil _)

/-- Mutant: the trace wrapper refuses even the empty trace. -/
def refuseRunLoanSequence (s : State) (es : List LoanAction) : Option State :=
  if es.isEmpty then none else runLoanSequence s es

/-- Mutant: the mixed dispatcher refuses standalone entry. -/
def refuseIssuanceStep (s : CallState) (e : IssuanceAction) : Option CallState :=
  match e with
  | .entry _ => none
  | _ => issuanceStep types s e

/-- Regression (for `runLoanSequence_dispatch`, empty trace). -/
example : Advances (AdmittedTrace LoanLedger initial []) (runLoanSequence initial []) :=
  (runLoanSequence_dispatch _ _).1

/-- Regression (for `runLoanSequence_dispatch`, mutant `refuseRunLoanSequence`). -/
example : ¬ Advances (AdmittedTrace LoanLedger initial []) (refuseRunLoanSequence initial []) :=
  refuses (.nil _) rfl

/-- Regression (for `issuanceStep_dispatch`, mutant `refuseIssuanceStep`). -/
example : ¬ Advances (AdmittedIssuance types ⟨exclusive, []⟩ (.entry singletonEntry))
    (refuseIssuanceStep ⟨exclusive, []⟩ (.entry singletonEntry)) :=
  refuses ⟨rfl, singletonAdmitted⟩ rfl

end DispatchEvidence

namespace DispatchEvidence
open Examples

/-- Mutant: storage teardown omits only the discharge check. -/
def dropWithoutDischarge (s : State) (xs : List Var) : Option State := do
  let f ← s.frames.head?
  let ps := departingPlaces f xs
  if s.loans.all (fun l => decide (l.root ∉ ps)) then
    some { s with
      slots := s.slots.filter (fun a => decide (a.place ∉ ps))
      frames := { f with locals := f.locals.filter (fun e => decide (e.2 ∉ ps)) } :: s.frames.tail }
  else none

/-- Mutant: return omits discharge during teardown, after ending local loans normally. -/
def acceptLiveReturn (e : Exec.Life) (s : State) : Option State :=
  match e with
  | .leave => do
    let f ← s.frames.head?
    let u ← endLocals f.id s
    let v ← dropWithoutDischarge u (List.map Prod.fst f.locals)
    some { v with frames := v.frames.tail }
  | _ => life types e s

/-- Mutant: teardown omits only the loan exclusion check. -/
def acceptBorrowedDrop (e : Exec.Life) (s : State) : Option State := do
  match e with
  | .drop xs =>
    let f ← s.frames.head?
    let ps := departingPlaces f xs
    if s.slots.all (fun a => decide (a.place ∉ ps) || a.desc.copy || a.status == .moved) then
      some { s with
        slots := s.slots.filter (fun a => decide (a.place ∉ ps))
        frames := { f with locals := f.locals.filter (fun e => decide (e.2 ∉ ps)) } :: s.frames.tail }
    else none
  | _ => life types e s

def borrowedCopy : State :=
  { exclusive with slots := exclusive.slots.map fun a => { a with desc.copy := true } }

/-- Helper (inversion). Well-formed inputs rule out success with a malformed ledger. -/
private theorem unsafeSuccess {s : State} {r : Option State}
    (hs : WellFormed s) (bad : ¬ StateResult r) :
    ¬ ResultSatisfies (fun t => WellFormed s → WellFormed t) r := by
  cases r with
  | none => exact False.elim (bad trivial)
  | some t => exact fun safe => bad (safe hs)

/-- Mutant: allow an absent owner slot, keeping the issued-frame and root-conflict checks. -/
def acceptMissingRoot (s : State) (e : LoanAction) : Option State :=
  match e with
  | .borrow p path mode => do
    let f ← s.frames.head?
    let owned := (slot s p).all fun a => a.status == .owned && (mode == .shared || a.desc.mutable)
    if owned && decide (f.id < s.nextActivation) &&
        s.loans.all (fun l => l.root != p || (mode == .shared && l.mode == .shared)) then
      some { s with
        loans := ⟨⟨s.nextLoan⟩, p, path, mode, none, [], f.id, false, false⟩ :: s.loans
        nextLoan := s.nextLoan + 1 }
    else none
  | _ => loanAction s e

/-- Mutant: omit the issued bound while retaining the current-destination and access checks. -/
def acceptUnissuedDestination (s : State) (e : LoanAction) : Option State :=
  match e with
  | .reborrow i path mode a => do
    let parent ← loan s i
    if loanAccess s i mode && s.frames.head?.map Activation.id == some a then
      some { s with
        loans := ⟨⟨s.nextLoan⟩, parent.root, parent.path ++ path, mode, some i,
          i :: parent.ancestors, a, false, false⟩ :: s.loans
        nextLoan := s.nextLoan + 1 }
    else none
  | _ => loanAction s e

def unissuedCurrent : State := { exclusive with nextActivation := 0 }

/-- Mutant: omit the child check when ending an existing loan. -/
def acceptParentEnd (s : State) (e : LoanAction) : Option State :=
  match e with
  | .endLoan i => (loan s i).map fun _ => { s with loans := s.loans.filter (·.id != i) }
  | _ => loanAction s e

/-- Mutant: omit the root-conflict check for owner arguments. -/
def acceptFrozenOwner (s : CallState) (p : Hybrid.Place) (path : List Projection)
    (mode : Mode) (v : Var) : Option CallState := do
  if v ∈ s.pending.map Prod.fst then none else do
  let f ← s.own.frames.head?
  let a ← slot s.own p
  if a.status == .owned && (mode == .shared || a.desc.mutable) && decide (f.id < s.own.nextActivation) then
    some ⟨{ s.own with
      loans := ⟨⟨s.own.nextLoan⟩, p, path, mode, none, [], f.id, true, true⟩ :: s.own.loans
      nextLoan := s.own.nextLoan + 1 }, s.pending ++ [(v, ⟨s.own.nextLoan⟩)]⟩
  else none

/-- Mutant: omit only the whole-root exclusion guard on reborrowing. -/
def reborrowIgnoringConflict (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (a : Nat) : Option State := do
  let parent ← loan s i
  if !parent.pendingArgument && (m == .shared || parent.mode == .mutable) &&
      decide (a < s.nextActivation ∧ s.frames.head?.map Activation.id = some a) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, parent.root, parent.path ++ path, m, some i,
        i :: parent.ancestors, a, false, false⟩ :: s.loans }
  else none

/-- Mutant: use the conflict-blind reborrow when evaluating a view argument. -/
def acceptFrozenView (s : CallState) (i : LoanId) (v : Var) : Option CallState := do
  if v ∈ s.pending.map Prod.fst then none else do
  let parent ← loan s.own i
  let f ← s.own.frames.head?
  let own ← reborrowIgnoringConflict s.own i [] parent.mode f.id
  let child ← own.loans.head?
  let own := { own with loans := { child with pendingArgument := true } :: s.own.loans }
  pure ⟨own, s.pending ++ [(v, child.id)]⟩

/-- Mutant: use the conflict-blind reborrow for standalone downward parameters. -/
def acceptFrozenDownward (s : State) (args : List (Var × LoanId)) : Option State := do
  let u ← enter (fun _ => none) [] s
  args.foldlM (fun v (x, i) => do
    let parent ← loan v i
    let f ← v.frames.head?
    if ParameterAvailable f.views x then
      let w ← reborrowIgnoringConflict v i [] parent.mode f.id
      some { w with frames := { f with views := (x, ⟨v.nextLoan⟩) :: f.views } :: v.frames.tail }
    else none) u

/-- Helper (inversion). Project the ownership invariant from issuance safety. -/
private theorem unsafeIssuance (s : CallState) (result : Option CallState)
    (hs : WellFormed s.own) (bad : ¬ CallOwnResult result) :
    ¬ ResultSatisfies (IssuanceSafe s) result := by
  cases h : result with
  | none => simp [ResultSatisfies, h] at bad
  | some t => exact fun safe => bad (by rw [h]; exact safe.1 hs)

/-- Regression (for owner admission, mutant `acceptFrozenOwner`). -/
example : ¬ ResultSatisfies (IssuanceSafe ⟨exclusive, []⟩)
    (acceptFrozenOwner ⟨exclusive, []⟩ ⟨0⟩ [] .mutable y) :=
  unsafeIssuance _ _ (by decide) (by decide)

/-- Mutant: a binding may allocate storage but cannot reinitialize a moved local. -/
def refuseReinitBind (e : Exec.Life) (s : State) : Option State :=
  match e with
  | .bind v =>
    if (s.frames.head?.bind (fun f => AList.lookup f.locals v)).isSome then none
    else life types e s
  | _ => life types e s

/-- Regression (for `life_dispatch`, mutant `refuseReinitBind`). -/
example : ¬ Advances (AdmittedLife types localReturn.own (.bind DirectArgumentWitnesses.z))
    (refuseReinitBind (.bind DirectArgumentWitnesses.z) localReturn.own) :=
  refuses ⟨⟨1, [(DirectArgumentWitnesses.z, ⟨1⟩)], []⟩, rfl, by decide,
    localReturn.own.slots.head (by decide), rfl, Or.inl rfl, by decide⟩ rfl

/-- Mutant: move a linear owner even though an active loan points to it. -/
def acceptBorrowedMove (s : State) (p : Hybrid.Place) : Option State :=
  do
    let a ← slot s p
    if a.desc.copy || a.status != .owned then none else
      some { s with slots := s.slots.map fun a =>
        if a.place = p then { a with status := .moved } else a }

/-- Regression (for `move_dispatch`, mutant `acceptBorrowedMove`). -/
example : ¬ ResultSatisfies (fun t => WellFormed exclusive → WellFormed t)
    (acceptBorrowedMove exclusive ⟨0⟩) := unsafeSuccess (by decide) (by decide)

/-- Regression (for `adoptDown_dispatch`, mutant `PreservationEvidence.adoptDownUnissuedActivation`).
An argument cannot be transferred into an activation beyond the current supply. -/
example : ¬ ResultSatisfies (fun t => WellFormed Round5Evidence.twoBefore → WellFormed t)
    (PreservationEvidence.adoptDownUnissuedActivation Round5Evidence.twoBefore
      Round5Evidence.twoWaiting.pending) := unsafeSuccess (by decide) (by decide)

/-- Regression (for `callLife_dispatch`, mutant `PreservationEvidence.callLifeKeepPending`).
Successful adoption must retire the pending entries in the call buffer. -/
example : ¬ ResultSatisfies (CallLifeSafe Round5Evidence.twoWaiting)
    (PreservationEvidence.callLifeKeepPending types (.enter [y, DirectArgumentWitnesses.z])
      Round5Evidence.twoWaiting) := by
  intro safe
  have bad := safe.2.1 (by decide)
  revert bad
  decide

/-- Regression (for `callLifeStore_dispatch`, case `nestedCaller`). -/
example : Advances (AdmittedCallLife types nestedCaller (.enter []))
    (callLifeStore types (.enter []) (7, nestedCaller)) :=
  (callLifeStore_dispatch types (7, nestedCaller) (.enter [])).1

/-- Mutant: the store wrapper resets the concrete store after a successful transition. -/
def resetStore (e : Exec.Life) (s : Nat × CallState) : Option (Nat × CallState) :=
  (callLife types e s.2).map fun t => (0, t)

/-- Regression (for `callLifeStore_dispatch`, mutant `resetStore`). -/
example : ¬ ResultSatisfies
    (fun t : Nat × CallState => t.1 = 7 ∧ CallLifeSafe nestedCaller t.2)
    (resetStore (.enter []) (7, nestedCaller)) := by
  intro safe
  exact (by decide : ¬ (0 : Nat) = 7) safe.1

/-- Regression (for `passDown_dispatch`, mutant `acceptFrozenView`). -/
example :
    passDown ⟨child, []⟩ ⟨0⟩ y = none ∧
    ¬ ResultSatisfies (fun t => (WellFormed child → WellFormed t.own) ∧
      ((⟨child, []⟩ : CallState).WellFormed → t.WellFormed))
      (acceptFrozenView ⟨child, []⟩ ⟨0⟩ y) := by
  refine ⟨rfl, ?_⟩
  intro safe
  have owned := safe.1 (by decide)
  revert owned
  decide

/-- Regression (for `runLoanSequence_dispatch`, mutant `LoanHistoryEvidence.recycle`). -/
example :
    ¬ ResultSatisfies (fun t => ∀ i, RetiredLoan LoanHistoryEvidence.endedChild i → RetiredLoan t i)
      (LoanHistoryEvidence.recycle LoanHistoryEvidence.endedChild LoanHistoryEvidence.actions) := by
  intro safe
  have retired := safe ⟨1⟩ (by decide)
  revert retired
  decide

end DispatchEvidence
end ConcreteSpec.Core.Own
