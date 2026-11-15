import ConcreteSpec.Proofs.Own
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Ident
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
The defer_keeps_store restatement similarly reuses O1's defer_keeps_store_result helper;
registration has no evaluator callback, so no second dispatcher is introduced to mutate it.
The source deferredStep alias and silentPrefix driver are retired: protocolOps, Exec.step and
Exec.run supply the actual execution and uniform lifecycle substitution. Source run-then-overwrite,
whole-function-refusal and whole-function-no-op mutants are replaced by one-rule body mutants.
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
    (s : State) (i : LoanId) (path : List Projection) (current caller : Nat) : Prop :=
  lend s i path .mutable caller = none ∧
    ∃ t, lend s i path .mutable current = some t ∧
      endLoan t ⟨s.nextLoan⟩ = some { s with nextLoan := s.nextLoan + 1 }

/-- Law (authority: docs/decisions.md#decided-2026-11-08-recovered-d3;
#decided-2026-11-05-core Q3.2–Q3.4; #od15; #od23). (subject `ConcreteSpec.Core.Own.reborrowTo`).
Downward lending creates a child in the current frame
and ends it without consuming any caller loan, while lending back to the caller is refused
(owner Q1, 2026-11-08). -/
theorem downward_reborrow_protocol (s : State) (i : LoanId) (path : List Projection)
    (f : Activation) (caller : Nat) (a : Loan) (hs : WellFormed s)
    (hf : s.frames.head? = some f) (different : f.id ≠ caller)
    (found : loan s i = some a) (allowed : loanAccess s i .mutable = true) :
    DownwardProtocol reborrowTo s i path f.id caller := by
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
  refine ⟨?_, ?_⟩
  · simp [reborrowTo, found, allowed, hf, different]
  · refine ⟨{ s with
        nextLoan := s.nextLoan + 1
        loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ path, .mutable, some i,
          i :: a.ancestors, f.id, false, false⟩ :: s.loans }, ?_, ?_⟩
    · simp [reborrowTo, found, allowed, hf, issued]
    · simp [endLoan, loan, ine, children, unchanged]

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
example : DownwardProtocol reborrowTo LifecycleWitnesses.called ⟨1⟩ [] 1 0 :=
  @downward_reborrow_protocol LifecycleWitnesses.called ⟨1⟩ [] calleeFrame 0 source
    (by decide) rfl (by decide) rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case
`ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant
`ConcreteSpec.Core.Own.DownwardProtocolEvidence.allowUpward`). The exact upward request is now admitted. -/
example : WellFormed LifecycleWitnesses.called ∧
    LifecycleWitnesses.called.frames.head? = some calleeFrame ∧ calleeFrame.id ≠ 0 ∧
    loan LifecycleWitnesses.called ⟨1⟩ = some source ∧
    loanAccess LifecycleWitnesses.called ⟨1⟩ .mutable = true ∧
    ¬ DownwardProtocol allowUpward LifecycleWitnesses.called ⟨1⟩ [] calleeFrame.id 0 := by
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
      LifecycleWitnesses.called ⟨1⟩ [] calleeFrame.id 0 := by
  refine ⟨by decide, rfl, by decide, rfl, by decide, ?_⟩
  rintro ⟨_, t, ht, he⟩
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
  exact ⟨_, _, rfl, rfl, rfl, rfl, rfl⟩

end DownwardProtocolEvidence

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
example : MoveLifecycle move source ⟨1⟩ :=
  @move_reinitialize_protocol source ⟨1⟩ selected (by decide) rfl rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.move_reinitialize_protocol`, case
`ConcreteSpec.Core.Own.MoveLifecycleEvidence.source`, subject `ConcreteSpec.Core.Own.move`, mutant
`ConcreteSpec.Core.Own.MoveLifecycleEvidence.keepOwned`). -/
example : WellFormed source ∧ slot source ⟨1⟩ = some selected ∧ selected.desc.copy = false ∧
    selected.status = .owned ∧
    (∀ l ∈ source.loans, l.root ≠ Hybrid.Place.mk 1) ∧
    ¬ MoveLifecycle keepOwned source ⟨1⟩ := by
  refine ⟨by decide, rfl, rfl, rfl, by decide, ?_⟩
  rintro ⟨t, ht, denied, _⟩
  cases ht
  cases denied

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
      (loan t ⟨3⟩).isSome = true) (runLoanSequence endedChild actions) := by decide

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
      (loanAction endedChild (.endLoan ⟨0⟩)) := by decide

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
      (runLoanSequence endedChild borrowActions) := by decide

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

/-- Helper (inversion). A view-only entry preserves every loan identity and root; its
empty owning-local drop leaves the callee state unchanged. -/
private theorem pending_enter_view (s t : CallState) (parameter : Var) (i : LoanId)
    (pending : s.pending = [(parameter, i)])
    (h : callLife Examples.types (.enter [parameter]) s = some t) :
    callLife Examples.types (.drop []) t = some t ∧
      t.own.loans.map (fun l => (l.id, l.root)) = s.own.loans.map (fun l => (l.id, l.root)) := by
  have check : s.pending.all (fun e => decide (e.1 ∈ [parameter])) = true := by simp [pending]
  have he : enter Examples.types ([parameter].filter fun x => decide (x ∉ s.pending.map Prod.fst)) s.own =
      some { s.own with
        nextActivation := s.own.nextActivation + 1
        frames := ⟨s.own.nextActivation, [], []⟩ :: s.own.frames } := by
    simp [pending, enter]
  simp only [callLife] at h
  dsimp at h
  rw [check] at h
  dsimp at h
  rw [he] at h
  dsimp at h
  unfold adoptDown at h
  dsimp at h
  split at h
  · cases h
    constructor
    · simp [callLife, life, drop]
    · simp only [List.map_map, Function.comp_def]
      apply List.map_congr_left
      intro l _
      split <;> rfl
  · contradiction

/-- The existing call operand instance with a substituted lifecycle callback. Every dispatcher,
driver, argument evaluation and cleanup uses this same instance. -/
abbrev protocolOps (lifecycle : Exec.Life → CallState → Option CallState) : Exec.Operands :=
  { Examples.callOps with life := lifecycle }

/-- A view-only deferred callee has no owned locals to discharge. -/
def heldProgram (lifecycle : Exec.Life → CallState → Option CallState)
    (parameter : Var) : Exec.Program (protocolOps lifecycle) :=
  ⟨[(Examples.callee, ⟨[(parameter, ())], (), ⟨1⟩, .skip⟩)]⟩

/-- Registration starts with a populated pending direct-root argument and an enclosing scope. -/
def heldRegistration (lifecycle : Exec.Life → CallState → Option CallState)
    (waiting : CallState) : Exec.Config (protocolOps lifecycle) :=
  ⟨.exec (.defer Examples.callee [.value (.lit 0)] ⟨0⟩), [], waiting,
    [.scope ⟨0⟩ [] []]⟩

/-- Snapshot of resource obligations, active loans and unadopted arguments. -/
def callSnapshot (s : CallState) : List Slot × List Loan × List (Var × LoanId) :=
  (s.own.slots, s.own.loans, s.pending)

/-- Uses Exec's sole fueled driver. A held loan survives registration, cleanup start and
every callee prefix before leaving. Six transitions invoke, execute, leave and return from the call.
The final snapshot includes every pre-existing caller loan, with no leaked call loan.
The lifecycle parameter is substituted uniformly throughout the actual execution. -/
def DeferredLifetime (lifecycle : Exec.Life → CallState → Option CallState)
    (s : State) (root : Hybrid.Place) (path : List Projection) (parameter : Var) : Prop :=
  ∃ waiting registered,
    passOwner ⟨s, []⟩ root path .mutable parameter = some waiting ∧
    Exec.step (heldProgram lifecycle parameter) (heldRegistration lifecycle waiting) =
      some (.next registered) ∧
    (∀ n, n ≤ 4 →
      let r := Exec.run (heldProgram lifecycle parameter) (fun _ p => nomatch p) n registered
      (∃ l ∈ r.residual.store.own.loans, l.id = ⟨s.nextLoan⟩ ∧ l.root = root) ∧
      ownerAccess r.residual.store.own root .mutable = false ∧
      r.ending = .outOfFuel) ∧
    let r := Exec.run (heldProgram lifecycle parameter) (fun _ p => nomatch p) 6 registered
    callSnapshot r.residual.store = (s.slots, s.loans, []) ∧ r.ending = .outOfFuel

/-- Law (authority: docs/decisions.md#decided-2026-11-08-recovered-d3, C5 question 2;
#od2; #od69; #decided-2026-11-13-lifecycle-refusal).
(subject `ConcreteSpec.Core.Own.callLife`). A held direct-root loan freezes
its owner until the deferred call runs, ends at actual return, and preserves caller loans. -/
theorem deferred_call_lifetime (s : State) (root : Hybrid.Place) (path : List Projection)
    (parameter : Var) (f : Activation) (hs : WellFormed s)
    (hf : s.frames.head? = some f) (allowed : ownerAccess s root .mutable = true) :
    DeferredLifetime (callLife Examples.types) s root path parameter := by
  have issued := (hs.2.2.2.2.2.2.1 f (List.mem_of_head? hf)).1
  obtain ⟨waiting, hpass⟩ : ∃ waiting, passOwner ⟨s, []⟩ root path .mutable parameter = some waiting := by
    simp [passOwner, hf, allowed, issued]
  have hpending : waiting.pending = [(parameter, ⟨s.nextLoan⟩)] := by
    unfold passOwner at hpass
    simp [hf, allowed, issued] at hpass
    subst waiting
    rfl
  obtain ⟨held, hheld, hid, hroot, _⟩ :=
    passOwner_birth_result ⟨s, []⟩ waiting root path .mutable parameter hpass
  have freeze : ownerAccess waiting.own root .mutable = false := by
    have hf0 := passOwner_freezes ⟨s, []⟩ root path .mutable parameter
    rw [hpass] at hf0
    exact hf0.1
  have roundtrip := directOwner_roundtrip s root path .mutable parameter Examples.types f hs hf issued allowed
  rw [hpass] at roundtrip
  dsimp at roundtrip
  obtain ⟨called, henter, hleave⟩ := Option.bind_eq_some_iff.mp roundtrip
  obtain ⟨hdrop, hkeys⟩ := pending_enter_view waiting called parameter ⟨s.nextLoan⟩ hpending henter
  have hmem : (held.id, held.root) ∈ called.own.loans.map (fun l => (l.id, l.root)) := by
    rw [hkeys, hheld]
    exact List.mem_cons_self
  obtain ⟨live, hlive, he⟩ := List.mem_map.mp hmem
  have liveId : live.id = LoanId.mk s.nextLoan := (congrArg Prod.fst he).trans hid
  have liveRoot : live.root = root := (congrArg Prod.snd he).trans hroot
  have calledFreeze : ownerAccess called.own root .mutable = false := by
    have blocked : called.own.loans.all (fun l => l.root != root) = false := by
      apply List.all_eq_false.mpr
      exact ⟨live, hlive, by simp [liveRoot]⟩
    simp [ownerAccess, show (Mode.mutable == Mode.shared) = false from rfl, blocked]
  let registered : Exec.Config (protocolOps (callLife Examples.types)) :=
    ⟨.done, [], waiting, [.scope ⟨0⟩ [] [⟨Examples.callee, [.value (.lit 0)], ⟨0⟩⟩]]⟩
  refine ⟨waiting, registered, hpass, rfl, ?_, ?_⟩
  · intro n hn
    have hc : n = 0 ∨ n = 1 ∨ n = 2 ∨ n = 3 ∨ n = 4 := by omega
    rcases hc with rfl | rfl | rfl | rfl | rfl
    · refine ⟨?_, freeze, rfl⟩
      exact ⟨held, by change held ∈ waiting.own.loans; rw [hheld]; exact List.mem_cons_self, hid, hroot⟩
    · refine ⟨?_, freeze, rfl⟩
      exact ⟨held, by change held ∈ waiting.own.loans; rw [hheld]; exact List.mem_cons_self, hid, hroot⟩
    all_goals
      simp [Exec.run, Exec.runFrom.eq_def, registered, Exec.Config.final?, Exec.step,
        Exec.startCleanup, Exec.Deferred.stmt, heldProgram, protocolOps, Exec.evalArgs,
        Exec.onRes, Examples.callEval, Examples.eval, Res.map, Res.bind, AList.lookup,
        Exec.dropOut, Exec.dead, Exec.dom, Exec.restrict, henter, hdrop]
      exact ⟨⟨live, hlive, liveId, liveRoot⟩, calledFreeze⟩
  · simp [Exec.run, Exec.runFrom.eq_def, Exec.Config.final?, registered, heldProgram, protocolOps,
      Exec.step, Exec.startCleanup, Exec.Deferred.stmt, Exec.evalArgs, Exec.onRes,
      Examples.callEval, Examples.eval, Res.map, Res.bind, AList.lookup,
      henter, Exec.dropOut, Exec.dead, Exec.dom, Exec.restrict, hdrop, hleave,
      Exec.bindLife, Exec.bindDst, callSnapshot]

namespace DeferredLifetimeEvidence
open Examples DirectArgumentWitnesses

/-- Skip ending the departing activation's loans by selecting the unissued next activation.
This changes only the endLocals destination in leave's body. -/
def leaveRetain (s : State) : Option State := do
  let f ← s.frames.head?
  let s ← endLocals s.nextActivation s
  let s ← drop (List.map Prod.fst f.locals) s
  some { s with frames := s.frames.tail }

/-- Propagate the changed leave through the sole ownership lifecycle adapter. -/
def lifeWithLeave (depart : State → Option State) (types : Var → Option Descriptor) : Exec.Life → State → Option State
  | .bind x, s => bind types x s
  | .enter ps, s => enter types ps s
  | .leave, s => depart s
  | .drop xs, s => drop xs s

/-- Propagate the same mutation through mixed owning/view call entry and return. -/
def callLifeWith (depart : State → Option State) (adopt : State → List (Var × LoanId) → Option State)
    (types : Var → Option Descriptor) :
    Exec.Life → CallState → Option CallState
  | .enter params, s => do
    if s.pending.all (fun e => decide (e.1 ∈ params)) then
      let own ← enter types (params.filter fun x => decide (x ∉ s.pending.map Prod.fst)) s.own
      let own ← adopt own s.pending
      pure ⟨own, []⟩
    else none
  | e, s => if s.pending = [] then
      (lifeWithLeave depart types e s.own).map fun own => { s with own := own }
    else none

/-- Instance (law `ConcreteSpec.Core.Own.deferred_call_lifetime`). The caller already has
an unrelated live mutable loan. Actual deferred execution must retain it at return. -/
example : DeferredLifetime (callLife types) caller.own ⟨1⟩ [.index 3] z :=
  @deferred_call_lifetime caller.own ⟨1⟩ [.index 3] z
    ⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩ (by decide) rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, subject `ConcreteSpec.Core.Own.leave`,
mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.leaveRetain`). All premises hold;
substitution through the lifecycle, operands, dispatcher and driver leaks the direct-root loan. -/
example : WellFormed caller.own ∧
    caller.own.frames.head? = some (⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩ : Activation) ∧
    ownerAccess caller.own ⟨1⟩ .mutable = true ∧
    ¬ DeferredLifetime (callLifeWith leaveRetain adoptDown types) caller.own ⟨1⟩ [.index 3] z := by
  refine ⟨by decide, rfl, by decide, ?_⟩
  rintro ⟨w, r, hp, hr, _, restore⟩
  cases hp
  cases hr
  have snapshot := restore.1
  revert snapshot
  decide

/-- Change only endLocals' activation filter to accept all loans. Every original loan is
otherwise ended through the real endLoan operation, in the original newest-first order. -/
def endAllLocals (_activation : Nat) (s : State) : Option State :=
  (s.loans.filter (fun _ => true)).foldlM (fun s l => endLoan s l.id) s

/-- Propagate the activation-filter mutation through the unchanged leave body. -/
def leaveAll (s : State) : Option State := do
  let f ← s.frames.head?
  let s ← endAllLocals f.id s
  let s ← drop (List.map Prod.fst f.locals) s
  some { s with frames := s.frames.tail }

/-- Planted (law `ConcreteSpec.Core.Own.deferred_call_lifetime`). The call returns after
ending its direct-root loan, but also silently ends the caller's unrelated existing loan. -/
example : WellFormed caller.own ∧
    caller.own.frames.head? = some (⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩ : Activation) ∧
    ownerAccess caller.own ⟨1⟩ .mutable = true ∧
    ¬ DeferredLifetime (callLifeWith leaveAll adoptDown types) caller.own ⟨1⟩ [.index 3] z := by
  refine ⟨by decide, rfl, by decide, ?_⟩
  rintro ⟨w, r, hp, hr, _, restore⟩
  cases hp
  cases hr
  have snapshot := restore.1
  revert snapshot
  decide

/-- Change only adoption's loan-table update: remove the argument loans instead of transferring
their activation and clearing the pending flag. Every admission check and frame update remains. -/
def adoptDropArguments (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.filter (fun l => decide (l.id ∉ args.map Prod.snd))
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none

/-- Planted (law `ConcreteSpec.Core.Own.deferred_call_lifetime`). Registration and final
restoration still agree, but entry prematurely ends the held loan. The callee-prefix
obligation, with all original law premises populated, catches the early release. -/
example : WellFormed caller.own ∧
    caller.own.frames.head? = some (⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩ : Activation) ∧
    ownerAccess caller.own ⟨1⟩ .mutable = true ∧
    ¬ DeferredLifetime (callLifeWith leave adoptDropArguments types) caller.own ⟨1⟩ [.index 3] z := by
  refine ⟨by decide, rfl, by decide, ?_⟩
  rintro ⟨w, r, hp, hr, keep, _⟩
  cases hp
  cases hr
  have held := (keep 2 (by decide)).1
  revert held
  decide

end DeferredLifetimeEvidence
end ConcreteSpec.Core.Own
