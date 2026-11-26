import ConcreteSpecEvidence.OwnExamples
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Own
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Proofs.Own

namespace ConcreteSpec.Core.Own

namespace PrimitiveWitnesses
open ConcreteSpec.Core.Own.Examples
/-- Instance: Positive preservation/progress instances use live, nonempty owner/loan states. -/
theorem example_Positive_preservation_progress_instances_use_live : WellFormed initial ∧ (move initial ⟨0⟩).isSome = true ∧
    WellFormed ((move initial ⟨0⟩).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation : WellFormed shared ∧ (borrow shared ⟨0⟩ [] .shared).isSome = true ∧
    WellFormed ((borrow shared ⟨0⟩ [] .shared).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation_2 : WellFormed exclusive ∧ (reborrowTo exclusive ⟨0⟩ [] .mutable 0).isSome = true ∧
    WellFormed child := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation_3 : accessOwner shared ⟨0⟩ .shared = some shared ∧ WellFormed shared := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation_4 : accessLoan child ⟨1⟩ .mutable = some child ∧ WellFormed child := by decide
/-- Planted: Malformed move/access observations destroy an owned root under a live loan. -/
theorem planted_Malformed_move_access_observations_destroy_owned : ¬ WellFormed { shared with slots := shared.slots.map fun x =>
    { x with status := .moved } } := by decide
/-- Planted: concrete ownership calculation. -/
theorem planted_ownership_calculation : ¬ WellFormed { child with slots := [] } := by decide
/-- Planted: A malformed borrow reuses the supply; a malformed reborrow loses its parent. -/
theorem planted_malformed_borrow_reuses_supply_malformed_reborrow : ¬ WellFormed { shared with nextLoan := 0 } := by decide
/-- Planted: concrete ownership calculation. -/
theorem planted_ownership_calculation_2 : ¬ WellFormed { child with loans := child.loans.filter (·.id != ⟨0⟩) } := by decide
/-- Planted: Negated success with each operational permission violated. -/
theorem planted_Negated_success_each_operational_permission_violated : ¬ (move shared ⟨0⟩).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
theorem planted_ownership_calculation_3 : ¬ (borrow exclusive ⟨0⟩ [] .shared).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
theorem planted_ownership_calculation_4 : ¬ (reborrowTo shared ⟨0⟩ [] .mutable 0).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
theorem planted_ownership_calculation_5 : ¬ (reborrow shared ⟨0⟩ [] .mutable).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
theorem planted_ownership_calculation_6 : ¬ (accessOwner shared ⟨0⟩ .mutable).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
theorem planted_ownership_calculation_7 : ¬ (accessLoan child ⟨0⟩ .mutable).isSome = true := by decide
end PrimitiveWitnesses

namespace LifecycleWitnesses
open ConcreteSpec.Core.Own.Examples

def moved : State := (move initial ⟨0⟩).getD initial
def entered : State := (enter types [y] shared).getD initial
def called : State := (enterDown exclusive [(y, ⟨0⟩)]).getD initial
/-- Instance: Each lifecycle/loan-ending law has a live-state success instance. -/
theorem example_Each_lifecycle_loan_ending_law_has : WellFormed moved ∧ reinitialize moved ⟨0⟩ = some initial := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation : WellFormed child ∧ (endLoan child ⟨1⟩).isSome = true ∧
    WellFormed ((endLoan child ⟨1⟩).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation_2 : WellFormed shared ∧ (bind types y shared).isSome = true ∧
    WellFormed ((bind types y shared).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation_3 : WellFormed moved ∧ (drop [x] moved).isSome = true ∧
    WellFormed ((drop [x] moved).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation_4 : WellFormed entered ∧ (enter types [y] shared).isSome = true := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation_5 : WellFormed called ∧ (endLocals 1 called).isSome = true ∧
    WellFormed ((endLocals 1 called).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation_6 : (leave called).isSome = true ∧ WellFormed ((leave called).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation_7 : life types (.bind y) shared = bind types y shared := by rfl
/-- Planted: Malformed reinitialization/drop/leave observations lose a borrowed root. -/
theorem planted_Malformed_reinitialization_drop_leave_observations_lose : ¬ WellFormed { shared with slots := [] } := by decide
/-- Planted: A malformed bind reuses the place identity. -/
theorem planted_malformed_bind_reuses_place_identity : ¬ WellFormed { initial with slots := initial.slots ++ initial.slots } := by decide
/-- Planted entry/adoption mutation assigns an unissued activation. -/
theorem planted_entry_adoption_mutation_assigns_unissued_activation : ¬ WellFormed { shared with loans := shared.loans.map fun l =>
    { l with activation := shared.nextActivation } } := by decide
/-- Planted endLoan/endLocals mutation removes the parent before the child. -/
theorem planted_endLoan_endLocals_mutation_removes_parent_before : ¬ WellFormed { child with loans := child.loans.filter (·.id != ⟨0⟩) } := by decide
/-- Planted: Progress counterexamples: live linear replacement, a child and a live local obligation. -/
theorem planted_Progress_counterexamples_live_linear_replacement_child : ¬ (reinitialize initial ⟨0⟩).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
theorem planted_ownership_calculation : ¬ (endLoan child ⟨0⟩).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
theorem planted_ownership_calculation_2 : ¬ (bind (fun _ => none) y shared).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
theorem planted_ownership_calculation_3 : ¬ (drop [x] initial).isSome = true := by decide
end LifecycleWitnesses

namespace CallWitnesses
open ConcreteSpec.Core.Own.Examples

def waiting : CallState := (passDown ⟨exclusive, []⟩ ⟨0⟩ y).getD ⟨initial, []⟩
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation : CallState.WellFormed ⟨exclusive, []⟩ ∧ CallState.WellFormed waiting := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation_2 : (passDown waiting ⟨0⟩ y) = none := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation_3 : callLife types .leave waiting = none := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation_4 : (callLife types (.enter [y]) waiting).isSome = true := by decide
/-- Instance: concrete ownership calculation. -/
theorem example_ownership_calculation_5 : CallState.WellFormed ((callLife types (.enter [y]) waiting).getD waiting) := by decide
/-- Planted: Malformed bookkeeping observations: duplicate parameters and a dangling identity. -/
theorem planted_Malformed_bookkeeping_observations_duplicate_parameters_and : ¬ CallState.WellFormed { waiting with pending := waiting.pending ++ waiting.pending } := by decide
/-- Planted: concrete ownership calculation. -/
theorem planted_ownership_calculation : ¬ CallState.WellFormed { waiting with own := exclusive } := by decide
/-- Planted leave mutation: discard loans but retain the pending buffer. -/
theorem planted_leave_mutation_discard_loans_but_retain : ¬ CallState.WellFormed { waiting with own := initial } := by decide
end CallWitnesses

namespace RoundThreeWitnesses
open ConcreteSpec.Core.Own.Examples
/-- Instance: Observation. A projected owner loan issues identity zero. -/
theorem example_Observation_projected_owner_loan_issues_identity : borrow initial ⟨0⟩ [.index 0] .shared = some shared ∧
    shared.nextLoan = initial.nextLoan + 1 ∧ ∃ l ∈ shared.loans,
      l.id = LoanId.mk initial.nextLoan ∧ l.root = ⟨0⟩ ∧ l.path = [.index 0] := by decide
/-- Planted: Rejection observation. Wrong projection falsifies the existential birth conclusion. -/
theorem planted_Rejection_observation_Wrong_projection_falsifies_existential : ¬ (∃ l ∈ shared.loans, l.id = LoanId.mk initial.nextLoan ∧
    l.root = ⟨0⟩ ∧ l.path = [.index 99]) := by decide
/-- Instance: Observation. The child retains its parent's root, extended path and parent identity. -/
theorem example_Observation_child_retains_its_parent_s : reborrowTo exclusive ⟨0⟩ [.index 1] .mutable 0 = some child ∧
    child.nextLoan = exclusive.nextLoan + 1 ∧ ∃ a l, loan exclusive ⟨0⟩ = some a ∧
      l ∈ child.loans ∧ l.id = LoanId.mk exclusive.nextLoan ∧ l.root = a.root ∧
      l.path = a.path ++ [.index 1] ∧ l.parent = some ⟨0⟩ := by
  simp [Examples.exclusive, Examples.child, Examples.initial, reborrow, reborrowTo, borrow,
    ownerAccess, loanAccess, slot, loan]
/-- Planted: Rejection observation. Removing the child's parent field falsifies the birth conclusion itself. -/
theorem planted_Rejection_observation_Removing_child_s_parent : ¬ (∃ a l, loan exclusive ⟨0⟩ = some a ∧
    l ∈ (child.loans.map fun l => { l with parent := none }) ∧
    l.id = LoanId.mk exclusive.nextLoan ∧ l.root = a.root ∧
    l.path = a.path ++ [.index 1] ∧ l.parent = some ⟨0⟩) := by
  simp [Examples.exclusive, Examples.initial, borrow, ownerAccess, slot, loan]
/-- Instance: Observation. Ending a child preserves the nonzero supply. -/
theorem example_Observation_Ending_child_preserves_nonzero_supply : ∃ t, endLoan child ⟨1⟩ = some t ∧ t.nextLoan = child.nextLoan := by decide
/-- Planted: Rejection observation. A mutated end decrements the supply. -/
theorem planted_Rejection_observation_mutated_end_decrements_supply : ¬ (({ child with nextLoan := child.nextLoan - 1 } : State).nextLoan =
    child.nextLoan) := by decide

/-- A callee owns a Copy local and borrows it; an unrelated caller loan survives its exit. -/
def localFrame : State := (enter (fun _ => some ⟨.int .i32, true, true⟩) [y] shared).getD initial
def localLoan : State := (borrow localFrame ⟨1⟩ [] .shared).getD initial
def exited : State := (leave localLoan).getD initial
/-- Instance: Observation. The departing local had a live loan; the caller's loan survives. -/
theorem example_Observation_departing_local_had_live_loan : WellFormed localLoan ∧ leave localLoan = some exited ∧
    (y, Hybrid.Place.mk 1) ∈ (localLoan.frames.headD ⟨99, [], []⟩).locals ∧
    (∃ l ∈ localLoan.loans, l.root = ⟨1⟩) ∧
    (∃ l ∈ exited.loans, l.root = ⟨0⟩) ∧
    (∀ l ∈ exited.loans, l.root ≠ Hybrid.Place.mk 1) ∧
    (∀ x ∈ exited.slots, x.activation ≠ 1) := by decide
/-- Planted: Rejection observation. Exit retaining the local's loan fails the root exclusion conclusion. -/
theorem planted_Rejection_observation_Exit_retaining_local_s : ¬ (∀ l ∈ localLoan.loans, l.root ≠ Hybrid.Place.mk 1) := by decide
/-- Planted: Rejection observation. Exit retaining a departing slot fails activation exclusion. -/
theorem planted_Rejection_observation_Exit_retaining_departing_slot : ¬ (∀ x ∈ localLoan.slots, x.activation ≠ 1) := by decide
/-- Instance: Observation. A child marked during argument evaluation can be adopted. -/
theorem example_Observation_child_marked_during_argument_evaluation : (callLife types (.enter [y]) CallWitnesses.waiting).isSome = true ∧
    ∃ l ∈ CallWitnesses.waiting.own.loans, l.id = LoanId.mk 1 ∧
      l.pendingArgument = true := by decide
/-- Planted: Rejection observation. Adopting a normal caller loan lacks the required pending witness. -/
theorem planted_Rejection_observation_Adopting_normal_caller_loan : ¬ (∃ l ∈ exclusive.loans, l.id = LoanId.mk 0 ∧
    l.pendingArgument = true) := by decide
/-- Planted: Rejection observation. An untyped parameter refuses entry. -/
theorem planted_Rejection_observation_untyped_parameter_refuses_entry : ¬ (enter (fun _ => none) [y] shared).isSome = true := by decide
/-- Instance: Observation. A typed fresh parameter enters over a borrowed caller. -/
theorem example_Observation_typed_fresh_parameter_enters_over : (enter types [y] shared).isSome = true := by decide
/-- Instance: Observation. Lending downward after entry targets the newly active callee. -/
theorem example_Observation_Lending_downward_after_entry_targets : (reborrowTo LifecycleWitnesses.called ⟨1⟩ [] .shared 1).isSome = true ∧
    ∃ f ∈ LifecycleWitnesses.called.frames, f.id = 1 ∧
      LifecycleWitnesses.called.frames.head? = some f := by decide
/-- Planted: Refused: activation 1 cannot lend upward into its caller, activation 0. -/
theorem planted_Refused_activation_cannot_lend_upward_into : reborrowTo LifecycleWitnesses.called ⟨1⟩ [] .shared 0 = none := by decide
/-- Planted: Rejection observation. The former arbitrary-destination rule admits upward lending, whose destination is not current. -/
theorem planted_Rejection_observation_former_arbitrary_destination_rule : ¬ (∃ f ∈ LifecycleWitnesses.called.frames, f.id = 0 ∧
    LifecycleWitnesses.called.frames.head? = some f) := by decide
/-- Planted: Rejection observation. A rule that issues the wrong birth counter fails the supply conclusion. -/
theorem planted_Rejection_observation_rule_issues_wrong_birth : ¬ (({ shared with nextLoan := 7 } : State).nextLoan = initial.nextLoan + 1) := by decide
/-- Planted: Rejection observation. Failing to append the requested suffix falsifies the path conclusion. -/
theorem planted_Rejection_observation_Failing_append_requested_suffix : ¬ (∃ l ∈ child.loans, l.id = LoanId.mk exclusive.nextLoan ∧
    l.path = [.index 0, .index 99]) := by decide
/-- Instance: Observation. Reborrowing retains the original parent record. -/
theorem example_Observation_Reborrowing_retains_original_parent_record : ∃ a, loan exclusive ⟨0⟩ = some a ∧ a ∈ child.loans := by
  simp [Examples.exclusive, Examples.child, Examples.initial, reborrow, reborrowTo,
    borrow, ownerAccess, loanAccess, slot, loan]
/-- Planted: Rejection observation. Removing the parent now falsifies the strengthened retention conclusion. -/
theorem planted_Rejection_observation_Removing_parent_now_falsifies : ¬ (∃ a, loan exclusive ⟨0⟩ = some a ∧
    a ∈ child.loans.filter (·.id != LoanId.mk 0)) := by
  simp [Examples.exclusive, Examples.child, Examples.initial, reborrow, reborrowTo,
    borrow, ownerAccess, loanAccess, slot, loan]
end RoundThreeWitnesses

namespace DirectArgumentWitnesses
open ConcreteSpec.Core.Own.Examples

def z : Var := ⟨2⟩
def caller : CallState := ⟨{ shared with
  slots := ⟨⟨1⟩, 0, ⟨.int .i32, false, true⟩, .owned⟩ :: shared.slots
  frames := [⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩]
  nextPlace := 2 }, []⟩
def waiting : CallState := (passOwner caller ⟨1⟩ [.index 3] .mutable z).getD caller
def called : CallState := (callLife types (.enter [z]) waiting).getD caller
def returned : CallState := (callLife types .leave called).getD caller

/-- Instance: Observation. A direct mutable argument coexists with an unrelated caller loan. -/
theorem example_Observation_direct_mutable_argument_coexists_unrelated : CallState.WellFormed caller ∧ passOwner caller ⟨1⟩ [.index 3] .mutable z = some waiting ∧
    CallState.WellFormed waiting := by decide
/-- Planted: Rejection observation. A mutated direct loan reuses the birth supply. -/
theorem planted_Rejection_observation_mutated_direct_loan_reuses : ¬ WellFormed { waiting.own with nextLoan := 1 } := by decide
/-- Instance: Observation. Direct lending has no caller-side parent. -/
theorem example_Observation_Direct_lending_has_no_caller : ∃ l, waiting.own.loans = l :: caller.own.loans ∧ l.id = LoanId.mk caller.own.nextLoan ∧
    l.root = Hybrid.Place.mk 1 ∧ l.path = [.index 3] ∧ l.mode = .mutable ∧
    l.parent = none ∧ l.pendingArgument = true ∧ l.callRoot = true := by
  exact passOwner_birth_result caller waiting ⟨1⟩ [.index 3] .mutable z (by decide)
/-- Planted: Rejection observation. A child of the caller loan cannot be the required parentless root. -/
theorem planted_Rejection_observation_child_caller_loan_cannot : ¬ (∃ l ∈ child.loans, l.id = LoanId.mk 1 ∧ l.parent = none) := by decide
/-- Instance: Observation. Argument evaluation freezes both reads and writes before entry. -/
theorem example_Observation_Argument_evaluation_freezes_both_reads : ownerAccess waiting.own ⟨1⟩ .mutable = false ∧
    ownerAccess waiting.own ⟨1⟩ .shared = false := by decide
/-- Planted: Rejection observation. Removing the loan prematurely fails the freeze conclusion. -/
theorem planted_Rejection_observation_Removing_loan_prematurely_fails : ¬ (ownerAccess { waiting.own with loans := caller.own.loans } ⟨1⟩ .mutable = false) := by decide
/-- Instance: Observation. Entry adopts the direct root and return ends it; its owner is immediately writable. -/
theorem example_Observation_Entry_adopts_direct_root_and : callLife types (.enter [z]) waiting = some called ∧
    callLife types .leave called = some returned ∧
    (∃ l ∈ called.own.loans, l.callRoot = true ∧ l.activation = 1) ∧
    (∀ l ∈ returned.own.loans, l.activation ≠ 1) ∧
    ownerAccess returned.own ⟨1⟩ .mutable = true := by decide
/-- Planted: Rejection observation. A return that never ends its direct argument retains a callee loan and freezes the owner. -/
theorem planted_Rejection_observation_return_never_ends_its : ¬ (∀ l ∈ called.own.loans, l.activation ≠ 1) := by decide
/-- Instance: Observation. The original caller loan is unchanged after return. -/
theorem example_Observation_original_caller_loan_is_unchanged : WellFormed called.own ∧
    (∀ l ∈ caller.own.loans, l ∈ called.own.loans ∧ l ∈ returned.own.loans) := by decide
/-- Planted: Rejection observation. Ending every loan would destroy the pre-existing caller loan. -/
theorem planted_Rejection_observation_Ending_every_loan_would : ¬ (∀ l ∈ caller.own.loans, l ∈ ([] : List Loan)) := by decide

/-- Instance: Observation. Registering a deferred call keeps a live direct root loan unchanged. -/
theorem example_Observation_Registering_deferred_call_keeps_live : ∃ c : Exec.Config callOps,
    Exec.step callProgram ⟨.exec (.defer callee [.ownerArgument ⟨1⟩ .mutable z] ⟨0⟩),
      [], waiting, [.scope ⟨0⟩ [] []]⟩ = some (.next c) ∧ c.store = waiting := by
  exact ⟨_, rfl, rfl⟩
/-- Planted: Rejection observation. Ending the call loan at registration changes the store and unfreezes the owner prematurely. -/
theorem planted_Rejection_observation_Ending_call_loan_at : ¬ ({ waiting with own := { waiting.own with loans := caller.own.loans } } = waiting) := by decide

/-- Instance: Accepted: a real C1 direct owner call returns, and the next operand reads the owner. -/
theorem example_Accepted_real_C1_direct_owner_call : (match (Exec.run callProgram (fun _ p => nomatch p) 100
    ⟨.exec (.seq (.call none callee [.ownerArgument ⟨0⟩ .mutable y] ⟨0⟩)
      (.cond (.value (.read x)) .skip .skip ⟨0⟩)), [(x, 7)], ⟨initial, []⟩, []⟩).ending with
    | .final .normal => true
    | _ => false) = true := by decide
/-- Instance: Accepted: deferred direct lending uses late-read arguments; its call loan ends only when
that call returns, and the caller reads its owner after cleanup. -/
theorem example_Accepted_deferred_direct_lending_uses_late : (match (Exec.run callProgram (fun _ p => nomatch p) 100
    ⟨.exec (.seq (.block ⟨0⟩ (.defer callee [.ownerArgument ⟨0⟩ .mutable y] ⟨0⟩))
      (.cond (.value (.read x)) .skip .skip ⟨0⟩)), [(x, 7)], ⟨initial, []⟩, []⟩).ending with
    | .final .normal => true
    | _ => false) = true := by decide
/-- Instance: Observation. A call restores two caller owners and a pre-existing caller loan. -/
theorem example_Observation_call_restores_two_caller_owners : (do
    let waiting ← passOwner caller ⟨1⟩ [.index 3] .mutable z
    let called ← callLife types (.enter [z]) waiting
    callLife types .leave called) = some returned ∧
    returned = ⟨{ caller.own with nextLoan := caller.own.nextLoan + 1, nextActivation := caller.own.nextActivation + 1 }, []⟩ := by decide
/-- Planted: Rejection observation. Keeping the call loan forever fails exact restoration of the caller tables. -/
theorem planted_Rejection_observation_Keeping_call_loan_forever : ¬ (called.own.loans = caller.own.loans) := by decide
end DirectArgumentWitnesses

namespace LiveDestinationWitnesses
open ConcreteSpec.Core.Own.Examples
/-- Instance: Observation. A mutable loan and child target their active caller frame. -/
theorem example_Observation_mutable_loan_and_child_target : LiveDestinations exclusive ∧ LiveDestinations child ∧
    reborrowTo exclusive ⟨0⟩ [.index 1] .mutable 0 = some child := by decide
/-- Planted: Rejection observation. Targeting an inactive but previously issued activation breaks actual liveness. -/
theorem planted_Rejection_observation_Targeting_inactive_but_previously : ¬ LiveDestinations { child with
    nextActivation := 3
    loans := child.loans.map fun l => { l with activation := 2 } } := by decide
/-- Instance: Observation. A pending direct root becomes a loan of the active callee. -/
theorem example_Observation_pending_direct_root_becomes_loan : LiveDestinations DirectArgumentWitnesses.waiting.own ∧
    LiveDestinations DirectArgumentWitnesses.called.own := by decide
/-- Planted: Rejection observation. Adoption into an inactive destination violates the conclusion. -/
theorem planted_Rejection_observation_Adoption_into_inactive_destination : ¬ LiveDestinations { DirectArgumentWitnesses.called.own with
    loans := DirectArgumentWitnesses.called.own.loans.map fun l => { l with activation := 7 } } := by decide
/-- Instance: Observation. Return retains the caller loan and its live destination. -/
theorem example_Observation_Return_retains_caller_loan_and : LiveDestinations DirectArgumentWitnesses.called.own ∧
    LiveDestinations DirectArgumentWitnesses.returned.own := by decide
/-- Planted: Rejection observation. Popping the callee without ending its root loan strands that loan. -/
theorem planted_Rejection_observation_Popping_callee_without_ending : ¬ LiveDestinations { DirectArgumentWitnesses.called.own with
    frames := DirectArgumentWitnesses.called.own.frames.tail } := by decide
end LiveDestinationWitnesses

namespace PreservationEvidence
open Examples
/-- A fresh callee with a pending direct owner argument, before adoption. -/
def adoptionInput : State :=
  (enter Examples.types [] DirectArgumentWitnesses.waiting.own).getD Examples.initial
/-- Planted: move appends a moved slot record without replacing the existing record for `p`,
breaking unique slot place identities (`(s.slots.map Slot.place).Nodup`). -/
def moveDuplicateSlot (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if x.desc.copy || x.status == .moved || s.loans.any (·.root == p) then none else
    some { s with slots := { x with status := .moved } :: s.slots }
/-- Planted: reinitialization assigns an unissued place identity `⟨s.nextPlace⟩` without advancing
`s.nextPlace`, breaking `x.place.id < s.nextPlace` and frame-local completeness. -/
def reinitializeUnissuedPlace (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if (x.status == .moved || (x.desc.copy && x.desc.mutable)) &&
      s.loans.all (·.root != p) then
    some { s with slots := s.slots.map fun x =>
      if x.place = p then { x with place := ⟨s.nextPlace⟩, status := .owned } else x }
  else none
/-- Planted: borrow issues `⟨s.nextLoan⟩` without advancing `nextLoan`, breaking `l.id.id < s.nextLoan`. -/
def borrowNoAdvanceSupply (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode) :
    Option State := do
  let f ← s.frames.head?
  if ownerAccess s p m && decide (f.id < s.nextActivation) then
    some { s with
      nextLoan := s.nextLoan
      loans := ⟨⟨s.nextLoan⟩, p, path, m, none, [], f.id, false, false⟩ :: s.loans }
  else none
/-- Planted: reborrowTo removes the parent loan `i` when inserting its child, breaking `ParentValid`. -/
def reborrowToDropParent (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, m, some i,
        i :: a.ancestors, activation, false, false⟩ :: s.loans.filter (·.id != i) }
  else none
/-- Planted: reborrow clears the cached ancestor chain while retaining `parent = some i`,
breaking `ParentValid`'s ancestor-chain agreement. -/
def reborrowDropAncestorsTo (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, m, some i,
        [], activation, false, false⟩ :: s.loans }
  else none
/-- Planted: Substitute the cleared-ancestor constructor through reborrow. -/
def reborrowDropAncestors (s : State) (i : LoanId) (path : List Projection) (m : Mode) : Option State := do
  let f ← s.frames.head?
  reborrowDropAncestorsTo s i path m f.id
/-- Planted: endLoan removes the parent of `i` instead of `i`, stranding child `i` and breaking `ParentValid`. -/
def endLoanRemoveParent (s : State) (i : LoanId) : Option State := do
  let a ← loan s i
  if s.loans.any (·.parent == some i) then none else
    some { s with loans := s.loans.filter (fun l => some l.id != a.parent) }
/-- Planted: bind allocates `⟨s.nextPlace⟩` without advancing `nextPlace`, breaking `x.place.id < s.nextPlace`. -/
def bindNoAdvancePlace (types : Var → Option Descriptor) (x : Var) (s : State) : Option State := do
  let f ← s.frames.head?
  match AList.lookup f.locals x with
  | some p => reinitialize s p
  | none =>
    let d ← types x
    if f.id < s.nextActivation then some { s with
      nextPlace := s.nextPlace
      slots := ⟨⟨s.nextPlace⟩, f.id, d, .owned⟩ :: s.slots
      frames := { f with locals := (x, ⟨s.nextPlace⟩) :: f.locals } :: s.frames.tail }
    else none
/-- Planted: drop removes the variable from `f.locals` while retaining its slot in `s.slots`,
breaking the frame-local completeness clause of `WellFormed`. -/
def dropLocalOnly (xs : List Var) (s : State) : Option State := do
  let f ← s.frames.head?
  let ps := List.map Prod.snd (f.locals.filter fun e => decide (e.1 ∈ xs))
  if s.slots.all (fun x => decide (x.place ∉ ps) || x.desc.copy || x.status == .moved) &&
      s.loans.all (fun l => decide (l.root ∉ ps)) then
    some { s with
      slots := s.slots
      frames := { f with locals := f.locals.filter (fun e => decide (e.2 ∉ ps)) } :: s.frames.tail }
  else none
/-- Planted: enter reuses the caller's activation ID `0` for the new frame instead of `s.nextActivation`,
breaking `(s.frames.map Activation.id).Nodup`. -/
def enterDuplicateFrameId (types : Var → Option Descriptor) (params : List Var) (s : State) : Option State := do
  let s := { s with
    nextActivation := s.nextActivation + 1
    frames := ⟨s.nextActivation - 1, [], []⟩ :: s.frames }
  params.foldlM (fun s x => bind types x s) s
/-- Planted: endLoan omits only the child-exclusion guard, allowing a parent to end first. -/
def endLoanAllowChildren (s : State) (i : LoanId) : Option State := do
  let _ ← loan s i
  some { s with loans := s.loans.filter (·.id != i) }
/-- Planted: substitute the child-guard omission throughout the original cleanup fold. -/
def endLocalsAllowChildren (activation : Nat) (s : State) : Option State :=
  (s.loans.filter (·.activation == activation)).foldlM (fun s l => endLoanAllowChildren s l.id) s
/-- Planted: leave ends local loans but omits dropping the departing frame’s slots. -/
def leaveKeepSlots (s : State) : Option State := do
  let f ← s.frames.head?
  let t ← endLocals f.id s
  some { t with frames := t.frames.tail }
/-- Planted: enterDown reuses the parent loan's ID `⟨0⟩` for the new child loan, breaking `(s.loans.map Loan.id).Nodup`. -/
def reborrowReuseId (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨i, a.root, a.path ++ suffix, m, some i,
        i :: a.ancestors, activation, false, false⟩ :: s.loans }
  else none
/-- Planted: downward entry uses the ID-reusing child constructor at every lending site. -/
def enterDownReuseParentId (s : State) (args : List (Var × LoanId)) : Option State := do
  let s ← enter (fun _ => none) [] s
  args.foldlM (fun s (x, i) => do
    let a ← loan s i
    let f ← s.frames.head?
    let s ← reborrowReuseId s i [] a.mode f.id
    some { s with frames := { f with views := (x, ⟨s.nextLoan - 1⟩) :: f.views } :: s.frames.tail }) s
/-- Planted: life on `.bind` adds the new slot to `s.slots` without recording it in `f.locals`,
breaking the frame-local completeness clause of `WellFormed`. -/
def bindUnindexed (types : Var → Option Descriptor) (x : Var) (s : State) : Option State := do
  let f ← s.frames.head?
  match AList.lookup f.locals x with
  | some p => reinitialize s p
  | none =>
    let d ← types x
    if f.id < s.nextActivation then some { s with
      nextPlace := s.nextPlace + 1
      slots := ⟨⟨s.nextPlace⟩, f.id, d, .owned⟩ :: s.slots }
    else none
/-- Planted: entry substitutes the unindexed binder at every parameter-binding site. -/
def enterBindUnindexed (types : Var → Option Descriptor) (params : List Var) (s : State) : Option State := do
  let s := { s with
    nextActivation := s.nextActivation + 1
    frames := ⟨s.nextActivation, [], []⟩ :: s.frames }
  params.foldlM (fun s x => bindUnindexed types x s) s
/-- Planted: substitute the unindexed binder in both direct binding and derived entry. -/
def lifeBindUnindexed (types : Var → Option Descriptor) : Exec.Life → State → Option State
  | .bind x, s => bindUnindexed types x s
  | .enter ps, s => enterBindUnindexed types ps s
  | .leave, s => leave s
  | .drop xs, s => drop xs s
/-- Planted: adoptDown assigns the unissued activation ID `s.nextActivation` to adopted loans,
breaking `l.activation < s.nextActivation`. -/
def adoptDownUnissuedActivation (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then { l with activation := s.nextActivation, pendingArgument := false } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none
/-- Planted: passDown clears `parent` on the new non-root pending loan (`callRoot = false`),
breaking `l.pendingArgument = true → (l.parent.isSome = true ∨ l.callRoot = true)`. -/
def reborrowParentlessTo (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, m, none,
        i :: a.ancestors, activation, false, false⟩ :: s.loans }
  else none
/-- Planted: Substitute the parentless constructor through reborrow. -/
def reborrowParentless (s : State) (i : LoanId) (path : List Projection) (m : Mode) : Option State := do
  let f ← s.frames.head?
  reborrowParentlessTo s i path m f.id
/-- Planted: Substitute the parentless reborrow through downward argument preparation. -/
def passDownClearParent (s : CallState) (source : LoanId) (parameter : Var) : Option CallState := do
  if parameter ∈ s.pending.map Prod.fst then none else do
  let l ← loan s.own source
  let own ← reborrowParentless s.own source [] l.mode
  let own := { own with loans := own.loans.map fun l =>
    if l.id.id = s.own.nextLoan then { l with pendingArgument := true } else l }
  pure ⟨own, s.pending ++ [(parameter, ⟨s.own.nextLoan⟩)]⟩
/-- Planted: passDown marks the new loan `pendingArgument = true` in `own` without appending it to
`s.pending`, breaking `CallState.WellFormed`'s pending-flag synchronization. -/
def passDownOmitPending (s : CallState) (source : LoanId) (parameter : Var) : Option CallState := do
  if parameter ∈ s.pending.map Prod.fst then none else do
  let l ← loan s.own source
  let own ← reborrow s.own source [] l.mode
  let own := { own with loans := own.loans.map fun l =>
    if l.id.id = s.own.nextLoan then { l with pendingArgument := true } else l }
  pure ⟨own, s.pending⟩
/-- Planted: callLife adopts pending loans into the unissued activation `own.nextActivation`,
breaking `l.activation < s.nextActivation`. -/
def callLifeUnissuedActivation (types : Var → Option Descriptor) : Exec.Life → CallState → Option CallState
  | .enter params, s => do
    if s.pending.all (fun e => decide (e.1 ∈ params)) then
      let own ← enter types (params.filter fun x => decide (x ∉ s.pending.map Prod.fst)) s.own
      let own ← adoptDownUnissuedActivation own s.pending
      pure ⟨own, []⟩
    else none
  | e, s => if s.pending = [] then
      (life types e s.own).map fun own => { s with own := own }
    else none
/-- Planted: callLife adopts pending loans (clearing their `pendingArgument` flags) without clearing
`s.pending`, breaking `CallState.WellFormed`'s pending-flag synchronization. -/
def callLifeKeepPending (types : Var → Option Descriptor) : Exec.Life → CallState → Option CallState
  | .enter params, s => do
    if s.pending.all (fun e => decide (e.1 ∈ params)) then
      let own ← enter types (params.filter fun x => decide (x ∉ s.pending.map Prod.fst)) s.own
      let own ← adoptDown own s.pending
      pure ⟨own, s.pending⟩
    else none
  | e, s => if s.pending = [] then
      (life types e s.own).map fun own => { s with own := own }
    else none
/-- Shared guarded direct-owner argument fixture. Each control changes exactly one supplied
field: identity increment, mode, call-root flag, or pending-list insertion. -/
def ownerArgument (advance : Nat) (loanMode : Mode → Mode) (callRoot appendPending : Bool)
    (s : CallState) (root : Hybrid.Place) (path : List Projection)
    (mode : Mode) (parameter : Var) : Option CallState := do
  if parameter ∈ s.pending.map Prod.fst then none else do
  let f ← s.own.frames.head?
  if ownerAccess s.own root mode && decide (f.id < s.own.nextActivation) then
    pure ⟨{ s.own with
      nextLoan := s.own.nextLoan + advance
      loans := ⟨⟨s.own.nextLoan⟩, root, path, loanMode mode, none, [], f.id, true, callRoot⟩ :: s.own.loans },
      if appendPending then s.pending ++ [(parameter, ⟨s.own.nextLoan⟩)] else s.pending⟩
  else none
/-- Planted: a parentless pending loan incorrectly lacks its call-root flag. -/
def passOwnerClearCallRoot := ownerArgument 1 id false true
/-- Planted: omit only pending-list insertion, leaving its synchronization flag set. -/
def passOwnerOmitPending := ownerArgument 1 id true false
/-- Planted: accessOwner consumes the accessed root slot (marking it `.moved`) while a shared loan
remains active on `p`, breaking `∃ x ∈ s.slots, x.place = l.root ∧ x.status = .owned`. -/
def accessOwnerConsumeRoot (s : State) (p : Hybrid.Place) (m : Mode) : Option State :=
  if ownerAccess s p m then some { s with slots := s.slots.map fun x =>
    if x.place == p then { x with status := .moved } else x } else none
/-- Planted: accessLoan drops the parent of loan `i` while `i` remains active, breaking `ParentValid`. -/
def accessLoanDropParent (s : State) (i : LoanId) (m : Mode) : Option State := do
  let a ← loan s i
  if loanAccess s i m then
    some { s with loans := s.loans.filter fun l => some l.id != a.parent }
  else none
/-- Instance (law `ConcreteSpec.Core.Own.move_preserves`, case `ConcreteSpec.Core.Own.Examples.initial`). -/
theorem move_preserves_example_initial : StateResult (move (initial) (⟨0⟩)) :=
  @move_preserves initial ⟨0⟩ (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.move_preserves`, case `ConcreteSpec.Core.Own.Examples.initial`, subject `ConcreteSpec.Core.Own.move`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.moveDuplicateSlot`). -/
theorem move_preserves_planted_initial_moveDuplicateSlot : (WellFormed (initial)) ∧ ¬ StateResult (moveDuplicateSlot (initial) (⟨0⟩)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.reinitialize_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.moved`). -/
theorem reinitialize_preserves_example_moved : StateResult (reinitialize (LifecycleWitnesses.moved) (⟨0⟩)) :=
  @reinitialize_preserves LifecycleWitnesses.moved ⟨0⟩ (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.reinitialize_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.moved`, subject `ConcreteSpec.Core.Own.reinitialize`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.reinitializeUnissuedPlace`). -/
theorem reinitialize_preserves_planted_moved_reinitializeUnissuedPlace : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (reinitializeUnissuedPlace (LifecycleWitnesses.moved) (⟨0⟩)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.borrow_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
theorem borrow_preserves_example_shared : StateResult (borrow (shared) (⟨0⟩) ([.index 1]) (.shared)) :=
  @borrow_preserves shared ⟨0⟩ [.index 1] .shared (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.borrow_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.borrow`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.borrowNoAdvanceSupply`). -/
theorem borrow_preserves_planted_shared_borrowNoAdvanceSupply : (WellFormed (shared)) ∧ ¬ StateResult (borrowNoAdvanceSupply (shared) (⟨0⟩) ([.index 1]) (.shared)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.reborrowTo_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
theorem reborrowTo_preserves_example_exclusive : StateResult (reborrowTo (exclusive) (⟨0⟩) ([.index 1]) (.mutable) (0)) :=
  @reborrowTo_preserves exclusive ⟨0⟩ [.index 1] .mutable 0 (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.reborrowTo_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.reborrowToDropParent`). -/
theorem reborrowTo_preserves_planted_exclusive_reborrowToDropParent : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowToDropParent (exclusive) (⟨0⟩) ([.index 1]) (.mutable) (0)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.reborrow_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
theorem reborrow_preserves_example_exclusive : StateResult (reborrow (exclusive) (⟨0⟩) ([.index 1]) (.mutable)) :=
  @reborrow_preserves exclusive ⟨0⟩ [.index 1] .mutable (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.reborrow_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.reborrow`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.reborrowDropAncestors`). -/
theorem reborrow_preserves_planted_exclusive_reborrowDropAncestors : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowDropAncestors (exclusive) (⟨0⟩) ([.index 1]) (.mutable)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.endLoan_preserves`, case `ConcreteSpec.Core.Own.Examples.child`). -/
theorem endLoan_preserves_example_child : StateResult (endLoan (child) (⟨1⟩)) :=
  @endLoan_preserves child ⟨1⟩ (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.endLoan_preserves`, case `ConcreteSpec.Core.Own.Examples.child`, subject `ConcreteSpec.Core.Own.endLoan`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.endLoanRemoveParent`). -/
theorem endLoan_preserves_planted_child_endLoanRemoveParent : (WellFormed (child)) ∧ ¬ StateResult (endLoanRemoveParent (child) (⟨1⟩)) := by decide
/-- Instance: Computation observation. -/
theorem example_Computation_observation : StateResult (accessOwner (shared) (⟨0⟩) (.shared)) :=
  @accessOwner_preserves shared ⟨0⟩ .shared (by decide)
/-- Planted: Computation observation. -/
theorem planted_Computation_observation : (WellFormed (shared)) ∧ ¬ StateResult (accessOwnerConsumeRoot (shared) (⟨0⟩) (.shared)) := by decide
/-- Instance: Computation observation. -/
theorem example_Computation_observation_2 : StateResult (accessLoan (child) (⟨1⟩) (.mutable)) :=
  @accessLoan_preserves child ⟨1⟩ .mutable (by decide)
/-- Planted: Computation observation. -/
theorem planted_Computation_observation_2 : (WellFormed (child)) ∧ ¬ StateResult (accessLoanDropParent (child) (⟨1⟩) (.mutable)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.bind_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
theorem bind_preserves_example_shared : StateResult (bind (types) (y) (shared)) :=
  @bind_preserves types shared y (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.bind_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.bind`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.bindNoAdvancePlace`). -/
theorem bind_preserves_planted_shared_bindNoAdvancePlace : (WellFormed (shared)) ∧ ¬ StateResult (bindNoAdvancePlace (types) (y) (shared)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.drop_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.moved`). -/
theorem drop_preserves_example_moved : StateResult (drop ([x]) (LifecycleWitnesses.moved)) :=
  @drop_preserves LifecycleWitnesses.moved [x] (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.drop_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.moved`, subject `ConcreteSpec.Core.Own.drop`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.dropLocalOnly`). -/
theorem drop_preserves_planted_moved_dropLocalOnly : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (dropLocalOnly ([x]) (LifecycleWitnesses.moved)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.enter_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
theorem enter_preserves_example_shared : StateResult (enter (types) ([y]) (shared)) :=
  @enter_preserves types shared [y] (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.enter_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.enter`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.enterDuplicateFrameId`). -/
theorem enter_preserves_planted_shared_enterDuplicateFrameId : (WellFormed (shared)) ∧ ¬ StateResult (enterDuplicateFrameId (types) ([y]) (shared)) := by decide
/-- Instance: endLocals_preserves also covers refusal while the caller loan has a child. -/
theorem example_endLocals_preserves_also_covers_refusal_while_caller : endLocals 0 LifecycleWitnesses.called = none ∧
    StateResult (endLocals 0 LifecycleWitnesses.called) :=
  ⟨by decide, @endLocals_preserves LifecycleWitnesses.called 0 (by decide)⟩
/-- Instance: successful cleanup preserves the populated caller ledger after ending the callee loan. -/
theorem example_successful_cleanup_preserves_populated_caller_ledger : (endLocals 1 LifecycleWitnesses.called).isSome = true ∧
    StateResult (endLocals 1 LifecycleWitnesses.called) :=
  ⟨by decide, endLocals_preserves LifecycleWitnesses.called 1 (by decide)⟩
/-- Planted: endLocalsAllowChildren violates endLocals_preserves by ending a parent before its child. -/
theorem planted_endLocalsAllowChildren_violates_endLocals_preserves_ending_parent_before : WellFormed LifecycleWitnesses.called ∧
    ¬ StateResult (endLocalsAllowChildren 0 LifecycleWitnesses.called) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.leave_preserves`, case `ConcreteSpec.Core.Own.RoundThreeWitnesses.localLoan`). -/
theorem leave_preserves_example_localLoan : StateResult (leave (RoundThreeWitnesses.localLoan)) :=
  @leave_preserves RoundThreeWitnesses.localLoan (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.leave_preserves`, case `ConcreteSpec.Core.Own.RoundThreeWitnesses.localLoan`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.leaveKeepSlots`). -/
theorem leave_preserves_planted_localLoan_leaveKeepSlots : WellFormed RoundThreeWitnesses.localLoan ∧
    ¬ StateResult (leaveKeepSlots RoundThreeWitnesses.localLoan) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.enterDown_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
theorem enterDown_preserves_example_exclusive : StateResult (enterDown (exclusive) ([(y, ⟨0⟩)])) :=
  @enterDown_preserves exclusive [(y, ⟨0⟩)] (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.enterDown_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.enterDown`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.enterDownReuseParentId`). -/
theorem enterDown_preserves_planted_exclusive_enterDownReuseParentId : (WellFormed (exclusive)) ∧ ¬ StateResult (enterDownReuseParentId (exclusive) ([(y, ⟨0⟩)])) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.life_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
theorem life_preserves_example_shared : StateResult (life (types) ((.bind y)) (shared)) :=
  @life_preserves types (.bind y) shared (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.life_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.life`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.lifeBindUnindexed`). -/
theorem life_preserves_planted_shared_lifeBindUnindexed : (WellFormed (shared)) ∧ ¬ StateResult (lifeBindUnindexed (types) ((.bind y)) (shared)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.adoptDown_preserves`, case `ConcreteSpec.Core.Own.PreservationEvidence.adoptionInput`). -/
theorem adoptDown_preserves_example_adoptionInput : StateResult (adoptDown (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) :=
  @adoptDown_preserves adoptionInput DirectArgumentWitnesses.waiting.pending (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.adoptDown_preserves`, case `ConcreteSpec.Core.Own.PreservationEvidence.adoptionInput`, subject `ConcreteSpec.Core.Own.adoptDown`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.adoptDownUnissuedActivation`). -/
theorem adoptDown_preserves_planted_adoptionInput_adoptDownUnissuedActivation : (WellFormed (adoptionInput)) ∧ ¬ StateResult (adoptDownUnissuedActivation (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.passDown_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
theorem passDown_preserves_example_exclusive : CallOwnResult (passDown (⟨exclusive, []⟩) (⟨0⟩) (y)) :=
  @passDown_preserves ⟨exclusive, []⟩ ⟨0⟩ y (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.passDown_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.passDown`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.passDownClearParent`). -/
theorem passDown_preserves_planted_exclusive_passDownClearParent : (WellFormed (⟨exclusive, []⟩ : CallState).own) ∧ ¬ CallOwnResult (passDownClearParent (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.passDown_call_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
theorem passDown_call_preserves_example_exclusive : CallResult (passDown (⟨exclusive, []⟩) (⟨0⟩) (y)) :=
  @passDown_call_preserves ⟨exclusive, []⟩ ⟨0⟩ y (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.passDown_call_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.passDown`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.passDownOmitPending`). -/
theorem passDown_call_preserves_planted_exclusive_passDownOmitPending : (CallState.WellFormed (⟨exclusive, []⟩)) ∧ ¬ CallResult (passDownOmitPending (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.callLife_preserves`, case `ConcreteSpec.Core.Own.CallWitnesses.waiting`). -/
theorem callLife_preserves_example_waiting : CallOwnResult (callLife (types) ((.enter [y])) (CallWitnesses.waiting)) :=
  @callLife_preserves types (.enter [y]) CallWitnesses.waiting (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.callLife_preserves`, case `ConcreteSpec.Core.Own.CallWitnesses.waiting`, subject `ConcreteSpec.Core.Own.callLife`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.callLifeUnissuedActivation`). -/
theorem callLife_preserves_planted_waiting_callLifeUnissuedActivation : (WellFormed (CallWitnesses.waiting).own) ∧ ¬ CallOwnResult (callLifeUnissuedActivation (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.callLife_call_preserves`, case `ConcreteSpec.Core.Own.CallWitnesses.waiting`). -/
theorem callLife_call_preserves_example_waiting : CallResult (callLife (types) ((.enter [y])) (CallWitnesses.waiting)) :=
  @callLife_call_preserves types (.enter [y]) CallWitnesses.waiting (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.callLife_call_preserves`, case `ConcreteSpec.Core.Own.CallWitnesses.waiting`, subject `ConcreteSpec.Core.Own.callLife`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.callLifeKeepPending`). -/
theorem callLife_call_preserves_planted_waiting_callLifeKeepPending : (CallState.WellFormed (CallWitnesses.waiting)) ∧ ¬ CallResult (callLifeKeepPending (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.passOwner_preserves`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`). -/
theorem passOwner_preserves_example_caller : CallOwnResult (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) :=
  @passOwner_preserves DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.passOwner_preserves`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.passOwner`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.passOwnerClearCallRoot`). -/
theorem passOwner_preserves_planted_caller_passOwnerClearCallRoot : (WellFormed (DirectArgumentWitnesses.caller).own) ∧ ¬ CallOwnResult (passOwnerClearCallRoot (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.passOwner_call_preserves`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`). -/
theorem passOwner_call_preserves_example_caller : CallResult (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) :=
  @passOwner_call_preserves DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.passOwner_call_preserves`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.passOwner`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.passOwnerOmitPending`). -/
theorem passOwner_call_preserves_planted_caller_passOwnerOmitPending : (CallState.WellFormed (DirectArgumentWitnesses.caller)) ∧ ¬ CallResult (passOwnerOmitPending (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide
end PreservationEvidence

namespace LifetimeEvidence
open Examples
/-- Planted: omit only the root-loan exclusion guard in drop. Linear consumption still applies. -/
def dropWithLoans (xs : List Var) (s : State) : Option State := do
  let f ← s.frames.head?
  let ps := List.map Prod.snd (f.locals.filter fun e => decide (e.1 ∈ xs))
  if s.slots.all (fun x => decide (x.place ∉ ps) || x.desc.copy || x.status == .moved) then
    some { s with
      slots := s.slots.filter (fun x => decide (x.place ∉ ps))
      frames := { f with locals := f.locals.filter (fun e => decide (e.2 ∉ ps)) } :: s.frames.tail }
  else none
/-- Planted: substitute the root-guard mutation at the local teardown site. -/
def leaveWithLoans (s : State) : Option State := do
  let f ← s.frames.head?
  let s ← endLocals f.id s
  let s ← dropWithLoans (List.map Prod.fst f.locals) s
  some { s with frames := s.frames.tail }
/-- Planted: omit only activation-local loan cleanup; drop retains all its guards. -/
def leaveWithoutEnding (s : State) : Option State := do
  let f ← s.frames.head?
  let s ← drop (List.map Prod.fst f.locals) s
  some { s with frames := s.frames.tail }
/-- Planted: cleanup indiscriminately ends caller loans too. -/
def endEveryActivation (_activation : Nat) (s : State) : Option State :=
  s.loans.foldlM (fun s l => endLoan s l.id) s
/-- Planted: substitute the activation-selector mutation into leave. -/
def leaveAllLoans (s : State) : Option State := do
  let f ← s.frames.head?
  let s ← endEveryActivation f.id s
  let s ← drop (List.map Prod.fst f.locals) s
  some { s with frames := s.frames.tail }
/-- Planted: mutable argument lending issues a shared loan; all bookkeeping still runs. -/
def passOwnerNoFreeze := PreservationEvidence.ownerArgument 1 (fun _ => .shared) true true
/-- Planted: only the fresh child targets an inactive activation. -/
def reborrowToDead (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, m, some i,
        i :: a.ancestors, s.nextActivation, false, false⟩ :: s.loans }
  else none
/-- A Copy local with a loan targeting the still-active caller; only the root guard prevents exit. -/
def outerLoan : State := { RoundThreeWitnesses.localLoan with
  loans := RoundThreeWitnesses.localLoan.loans.map fun l => { l with activation := 0 } }
/-- Instance: leave_no_local_loan also covers refusal when a caller loan still refers to the local. -/
theorem example_leave_no_local_loan_also_covers_refusal_when_caller : ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ⟨1⟩) (leave outerLoan) :=
  @leave_no_local_loan outerLoan ⟨1, [(y, ⟨1⟩)], []⟩ (y, ⟨1⟩) rfl (by decide) (.head [])
/-- Instance: exit ends the live local loan and retains the unrelated caller loan. -/
theorem example_exit_ends_live_local_loan_and : (leave RoundThreeWitnesses.localLoan).isSome = true ∧
    ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ⟨1⟩)
      (leave RoundThreeWitnesses.localLoan) :=
  ⟨by decide, leave_no_local_loan RoundThreeWitnesses.localLoan
    ⟨1, [(y, ⟨1⟩)], []⟩ (y, ⟨1⟩) rfl (by decide) (.head [])⟩
/-- Planted: leaveWithLoans violates leave_no_local_loan by omitting only the root-loan guard. -/
theorem planted_leaveWithLoans_violates_leave_no_local_loan_omitting_only_root : outerLoan.frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation) ∧
    WellFormed outerLoan ∧
    (y, Hybrid.Place.mk 1) ∈ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).locals ∧
    ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ⟨1⟩) (leaveWithLoans outerLoan) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.leave_no_activation_slot`, case `ConcreteSpec.Core.Own.RoundThreeWitnesses.localLoan`). -/
theorem leave_no_activation_slot_example_localLoan : ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leave (RoundThreeWitnesses.localLoan)) :=
  @leave_no_activation_slot RoundThreeWitnesses.localLoan ⟨1, [(y, ⟨1⟩)], []⟩ (by decide) (rfl)
/-- Planted (law `ConcreteSpec.Core.Own.leave_no_activation_slot`, case `ConcreteSpec.Core.Own.RoundThreeWitnesses.localLoan`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.leaveKeepSlots`). -/
theorem leave_no_activation_slot_planted_localLoan_leaveKeepSlots : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (PreservationEvidence.leaveKeepSlots (RoundThreeWitnesses.localLoan)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.passOwner_freezes`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`). -/
theorem passOwner_freezes_example_caller : ResultSatisfies (fun t : CallState => ownerAccess t.own (⟨1⟩) .mutable = false ∧ ((Mode.mutable) = .mutable → ownerAccess t.own (⟨1⟩) .shared = false)) (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (Mode.mutable) (DirectArgumentWitnesses.z)) :=
  @passOwner_freezes DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z
/-- Planted (law `ConcreteSpec.Core.Own.passOwner_freezes`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.passOwner`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.passOwnerNoFreeze`). -/
theorem passOwner_freezes_planted_caller_passOwnerNoFreeze : ¬ ResultSatisfies (fun t : CallState => ownerAccess t.own (⟨1⟩) .mutable = false ∧ ((Mode.mutable) = .mutable → ownerAccess t.own (⟨1⟩) .shared = false)) (passOwnerNoFreeze (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (Mode.mutable) (DirectArgumentWitnesses.z)) := by decide
/-- The pre-existing caller loan shared by the retention instance and its control. -/
def callerLoan : Loan := Examples.shared.loans.headD
  ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩
/-- Observe the exact caller loan, including its identity and all bookkeeping fields. -/
def retainsCaller (t : State) : Prop := callerLoan ∈ t.loans
instance (t : State) : Decidable (retainsCaller t) :=
  inferInstanceAs (Decidable (callerLoan ∈ t.loans))
/-- Instance: leave_retains_caller preserves this loan across successful callee exit. -/
theorem example_leave_retains_caller_preserves_this_loan_across_successful : ResultSatisfies retainsCaller (leave RoundThreeWitnesses.localLoan) :=
  leave_retains_caller RoundThreeWitnesses.localLoan ⟨1, [(y, ⟨1⟩)], []⟩ callerLoan
    (by decide) rfl (.tail _ (.head [])) (by decide)
/-- Planted: leaveAllLoans changes only cleanup selection and loses the caller loan. -/
theorem planted_leaveAllLoans_changes_only_cleanup_selection_and : WellFormed RoundThreeWitnesses.localLoan ∧
    RoundThreeWitnesses.localLoan.frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation) ∧
    callerLoan ∈ RoundThreeWitnesses.localLoan.loans ∧
    callerLoan.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id ∧
    ¬ ResultSatisfies retainsCaller (leaveAllLoans RoundThreeWitnesses.localLoan) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.leave_no_activation_loan`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.called.own`). -/
theorem leave_no_activation_loan_example_own : ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ 1) (leave DirectArgumentWitnesses.called.own) :=
  @leave_no_activation_loan DirectArgumentWitnesses.called.own ⟨1, [], [(DirectArgumentWitnesses.z, ⟨1⟩)]⟩ rfl
/-- Planted (law `ConcreteSpec.Core.Own.leave_no_activation_loan`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.called.own`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveWithoutEnding`). -/
theorem leave_no_activation_loan_planted_own_leaveWithoutEnding : DirectArgumentWitnesses.called.own.frames.head? = some (⟨1, [], [(DirectArgumentWitnesses.z, ⟨1⟩)]⟩ : Activation) ∧
    ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ 1) (leaveWithoutEnding DirectArgumentWitnesses.called.own) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.reborrowTo_live`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
theorem reborrowTo_live_example_exclusive : ResultSatisfies (fun t : State => LiveDestinations t) (reborrowTo (exclusive) (⟨0⟩) ([.index 1]) (Mode.mutable) (0)) :=
  @reborrowTo_live exclusive ⟨0⟩ [.index 1] .mutable 0 (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.reborrowTo_live`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.reborrowToDead`). -/
theorem reborrowTo_live_planted_exclusive_reborrowToDead : (LiveDestinations (exclusive)) ∧ ¬ ResultSatisfies (fun t : State => LiveDestinations t) (reborrowToDead (exclusive) (⟨0⟩) ([.index 1]) (Mode.mutable) (0)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.adoptDown_live`, case `ConcreteSpec.Core.Own.PreservationEvidence.adoptionInput`). -/
theorem adoptDown_live_example_adoptionInput : ResultSatisfies (fun t : State => LiveDestinations t) (adoptDown (PreservationEvidence.adoptionInput) (DirectArgumentWitnesses.waiting.pending)) :=
  @adoptDown_live PreservationEvidence.adoptionInput DirectArgumentWitnesses.waiting.pending (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.adoptDown_live`, case `ConcreteSpec.Core.Own.PreservationEvidence.adoptionInput`, subject `ConcreteSpec.Core.Own.adoptDown`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.adoptDownUnissuedActivation`). -/
theorem adoptDown_live_planted_adoptionInput_adoptDownUnissuedActivation : (LiveDestinations (PreservationEvidence.adoptionInput)) ∧ ¬ ResultSatisfies (fun t : State => LiveDestinations t) (PreservationEvidence.adoptDownUnissuedActivation (PreservationEvidence.adoptionInput) (DirectArgumentWitnesses.waiting.pending)) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.leave_live`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.called.own`). -/
theorem leave_live_example_own : ResultSatisfies LiveDestinations (leave DirectArgumentWitnesses.called.own) :=
  @leave_live DirectArgumentWitnesses.called.own ⟨1, [], [(DirectArgumentWitnesses.z, ⟨1⟩)]⟩ (by decide) rfl
/-- Planted (law `ConcreteSpec.Core.Own.leave_live`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.called.own`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveWithoutEnding`). -/
theorem leave_live_planted_own_leaveWithoutEnding : LiveDestinations DirectArgumentWitnesses.called.own ∧ DirectArgumentWitnesses.called.own.frames.head? = some (⟨1, [], [(DirectArgumentWitnesses.z, ⟨1⟩)]⟩ : Activation) ∧
    ¬ ResultSatisfies LiveDestinations (leaveWithoutEnding DirectArgumentWitnesses.called.own) := by decide
/-- A state whose active frame omits a slot of its activation from `f.locals` violates
the frame-local completeness clause of `WellFormed`. -/
def unindexedCopy : State :=
  { initial with
    slots := initial.slots.map fun x => { x with desc := { x.desc with copy := true } }
    frames := [⟨0, [], []⟩] }
/-- Planted: concrete ownership calculation. -/
theorem planted_ownership_calculation : ¬ WellFormed unindexedCopy := by decide
/-- Planted: concrete ownership calculation. -/
theorem planted_ownership_calculation_2 : ¬ WellFormed { initial with frames := [⟨0, [], []⟩] } := by decide
/-- Planted: An issued but inactive activation cannot retain an owner obligation. -/
theorem planted_issued_but_inactive_activation_cannot_retain : ¬ WellFormed { initial with frames := [⟨1, [], []⟩], nextActivation := 2 } := by decide
/-- Planted: A callee cannot index a caller's Copy owner as its own local. -/
theorem planted_callee_cannot_index_caller_s_Copy : ¬ WellFormed { initial with
    slots := initial.slots.map fun x => { x with desc := { x.desc with copy := true } }
    frames := [⟨1, [(y, ⟨0⟩)], []⟩, ⟨0, [(x, ⟨0⟩)], []⟩]
    nextActivation := 2 } := by decide
/-- Instance: Cleanup removes every alias of a removed place, so no local index dangles. -/
theorem example_Cleanup_removes_every_alias_removed_place : let s : State := { LifecycleWitnesses.moved with
    frames := [⟨0, [(x, ⟨0⟩), (y, ⟨0⟩)], []⟩] }
    WellFormed s ∧ (drop [x] s).isSome = true ∧
      WellFormed ((drop [x] s).getD initial) := by decide
/-- Planted: the root-guard mutation is used at every direct drop and leave site. -/
def lifeWithLoans (types : Var → Option Descriptor) : Exec.Life → State → Option State
  | .drop xs, s => dropWithLoans xs s
  | .leave, s => leaveWithLoans s
  | e, s => life types e s
/-- Planted: every non-entry call event uses the substituted lifecycle. -/
def callWithLoans (types : Var → Option Descriptor) : Exec.Life → CallState → Option CallState
  | .enter ps, s => callLife types (.enter ps) s
  | e, s => if s.pending = [] then
      (lifeWithLoans types e s.own).map fun own => { s with own := own }
    else none
/-- Planted: the same omitted root check adds a transition and breaks each derived preservation
postcondition, while every preservation premise holds. -/
theorem planted_same_omitted_root_check_adds_transition : WellFormed outerLoan ∧ CallState.WellFormed ⟨outerLoan, []⟩ ∧
    drop [y] outerLoan = none ∧ leave outerLoan = none ∧
    ¬ StateResult (dropWithLoans [y] outerLoan) ∧
    ¬ StateResult (leaveWithLoans outerLoan) ∧
    ¬ StateResult (lifeWithLoans types .leave outerLoan) ∧
    ¬ CallOwnResult (callWithLoans types .leave ⟨outerLoan, []⟩) ∧
    ¬ CallResult (callWithLoans types .leave ⟨outerLoan, []⟩) := by decide
end LifetimeEvidence

namespace FinalEvidence
open Examples
/-- Planted: entry wrongly forbids parameter shadowing of caller locals. -/
def enterRefuse (types : Var → Option Descriptor) (params : List Var) (s : State) : Option State := do
  let caller := s.frames.head?
  let s := { s with
    nextActivation := s.nextActivation + 1
    frames := ⟨s.nextActivation, [], []⟩ :: s.frames }
  params.foldlM (fun s x => do
    if (caller.bind fun f => AList.lookup f.locals x).isSome then none
    else bind types x s) s
/-- Instance (law `ConcreteSpec.Core.Own.enter_progress`, case `ConcreteSpec.Core.Own.Examples.shared`). Two parameters may shadow caller locals. -/
theorem enter_progress_example_shared : ∃ t, enter types [x, y] shared = some t :=
  @enter_progress types shared [x, y] (by decide) (fun _ _ => ⟨_, rfl⟩)
/-- Planted (law `ConcreteSpec.Core.Own.enter_progress`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.enter`, mutant `ConcreteSpec.Core.Own.FinalEvidence.enterRefuse`). -/
theorem enter_progress_planted_shared_enterRefuse : [x, y].Nodup ∧ (∀ v ∈ [x, y], ∃ d, types v = some d) ∧
    ¬ (∃ t, enterRefuse types [x, y] shared = some t) :=
  ⟨by decide, (fun _ _ => ⟨_, rfl⟩), by change ¬ (∃ t, (none : Option State) = some t); simp⟩
/-- Planted: every leave site uses the same cleanup omission; other events are unchanged. -/
def callLifeLeaked (types : Var → Option Descriptor) (e : Exec.Life) (s : CallState) : Option CallState :=
  match e with
  | .leave => if s.pending = [] then
      (LifetimeEvidence.leaveWithoutEnding s.own).map fun own => { s with own := own }
    else none
  | _ => callLife types e s
/-- Planted: advance only the loan supply by two; retain all argument bookkeeping. -/
def passOwnerWrongSupply := PreservationEvidence.ownerArgument 2 id true true
/-- Caller frame shared by the exact roundtrip observations. -/
def callerFrame : Activation := ⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩
/-- Expected caller ledger after one fresh direct-argument call. -/
def restoredCaller : CallState := ⟨{ DirectArgumentWitnesses.caller.own with
  nextLoan := DirectArgumentWitnesses.caller.own.nextLoan + 1
  nextActivation := DirectArgumentWitnesses.caller.own.nextActivation + 1 }, []⟩
/-- The same populated admission premises apply to both roundtrip substitutions. -/
def callerReady : Prop := WellFormed DirectArgumentWitnesses.caller.own ∧
  DirectArgumentWitnesses.caller.own.frames.head? = some callerFrame ∧
  callerFrame.id < DirectArgumentWitnesses.caller.own.nextActivation ∧
  ownerAccess DirectArgumentWitnesses.caller.own ⟨1⟩ .mutable = true
instance : Decidable callerReady := inferInstanceAs (Decidable (_ ∧ _ ∧ _ ∧ _))
/-- Instance (law `ConcreteSpec.Core.Own.directOwner_roundtrip`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`). -/
theorem directOwner_roundtrip_example_caller : (do
    let waiting ← passOwner DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z
    let called ← callLife types (.enter [DirectArgumentWitnesses.z]) waiting
    callLife types .leave called) =
    some restoredCaller :=
  @directOwner_roundtrip DirectArgumentWitnesses.caller.own ⟨1⟩ [.index 3] .mutable
    DirectArgumentWitnesses.z types callerFrame (by decide) rfl (by decide) (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.directOwner_roundtrip`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.callLife`, mutant `ConcreteSpec.Core.Own.FinalEvidence.callLifeLeaked`). -/
theorem directOwner_roundtrip_planted_caller_callLifeLeaked : callerReady ∧
    ¬ ((do
      let waiting ← passOwner DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z
      let called ← callLifeLeaked types (.enter [DirectArgumentWitnesses.z]) waiting
      callLifeLeaked types .leave called) =
      some restoredCaller) := by decide
/-- Planted (law `ConcreteSpec.Core.Own.directOwner_roundtrip`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.passOwner`, mutant `ConcreteSpec.Core.Own.FinalEvidence.passOwnerWrongSupply`). -/
theorem directOwner_roundtrip_planted_caller_passOwnerWrongSupply : callerReady ∧
    ¬ ((do
      let waiting ← passOwnerWrongSupply DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z
      let called ← callLife types (.enter [DirectArgumentWitnesses.z]) waiting
      callLife types .leave called) =
      some restoredCaller) := by decide
/-- Instance: The roundtrip mutant succeeds through `passOwner`, `enter` and `leave` (`isSome = true`),
so the planted refutation above fails because the returned state leaks a loan, not because a step
returns `none`. -/
theorem example_roundtrip_mutant_succeeds_through_passOwner_enter : (do
    let waiting ← passOwner DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z
    let called ← callLifeLeaked types (.enter [DirectArgumentWitnesses.z]) waiting
    callLifeLeaked types .leave called).isSome = true := by decide
end FinalEvidence

namespace SuccessfulInstances
open Examples
/-- Instance: preservation and lifetime fixtures actually take their successful branches.
In particular, optional postconditions cannot hide a vacuous refusal in their tests. -/
theorem example_preservation_and_lifetime_fixtures_actually_take : (move initial ⟨0⟩).isSome = true ∧
    (reinitialize LifecycleWitnesses.moved ⟨0⟩).isSome = true ∧
    (borrow shared ⟨0⟩ [.index 1] .shared).isSome = true ∧
    (reborrowTo exclusive ⟨0⟩ [.index 1] .mutable 0).isSome = true ∧
    (endLoan child ⟨1⟩).isSome = true ∧
    (bind types y shared).isSome = true ∧
    (drop [x] LifecycleWitnesses.moved).isSome = true ∧
    (enter types [y] shared).isSome = true ∧
    (endLocals 1 LifecycleWitnesses.called).isSome = true ∧
    (leave DirectArgumentWitnesses.called.own).isSome = true ∧
    (reborrow exclusive ⟨0⟩ [.index 1] .mutable).isSome = true ∧
    (enterDown exclusive [(y, ⟨0⟩)]).isSome = true ∧
    (life types (.bind y) shared).isSome = true ∧
    (leave RoundThreeWitnesses.localLoan).isSome = true ∧
    (adoptDown PreservationEvidence.adoptionInput DirectArgumentWitnesses.waiting.pending).isSome = true ∧
    (passDown ⟨exclusive, []⟩ ⟨0⟩ y).isSome = true ∧
    (callLife types (.enter [y]) CallWitnesses.waiting).isSome = true ∧
    (passOwner DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable
      DirectArgumentWitnesses.z).isSome = true := by decide
end SuccessfulInstances

namespace RoundTwoEvidence
open Examples
/-- Two moved slots, including a nonzero target; frame indices are fully populated. -/
def twoMoved : State :=
  { LifecycleWitnesses.moved with
    nextPlace := 2
    slots := ⟨⟨1⟩, 0, ⟨.int .i32, false, true⟩, .moved⟩ :: LifecycleWitnesses.moved.slots
    frames := [⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩] }
/-- Planted: only the update selector restores place zero instead of the requested place. -/
def restoreZero (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if (x.status == .moved || (x.desc.copy && x.desc.mutable)) &&
      s.loans.all (·.root != p) then
    some { s with slots := s.slots.map fun x =>
      if x.place = ⟨0⟩ then { x with status := .owned } else x }
  else none
/-- Planted: reinitialization restores every moved slot instead of only its target. -/
def restoreAll (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if (x.status == .moved || (x.desc.copy && x.desc.mutable)) &&
      s.loans.all (·.root != p) then
    some { s with slots := s.slots.map fun x =>
      { x with status := .owned } }
  else none
/-- Instance: a nonzero target is restored and the other moved obligation remains moved.
This pins the selected-place behavior in addition to the preservation law's weaker invariant. -/
theorem example_nonzero_target_is_restored_and_other : WellFormed twoMoved ∧ StateResult (reinitialize twoMoved ⟨1⟩) ∧
    ((reinitialize twoMoved ⟨1⟩).getD initial).slots.map Slot.status = [.owned, .moved] := by decide
/-- Planted: each wrong selector fails the same populated selected-place observation. -/
theorem planted_each_wrong_selector_fails_same_populated : ((restoreZero twoMoved ⟨1⟩).getD initial).slots.map Slot.status ≠ [.owned, .moved] ∧
    ((restoreAll twoMoved ⟨1⟩).getD initial).slots.map Slot.status ≠ [.owned, .moved] := by decide
/-- Instance: a shared direct argument freezes owner writes while preserving shared reads. -/
theorem example_shared_direct_argument_freezes_owner_writes : (passOwner DirectArgumentWitnesses.caller ⟨1⟩ [] .shared y).isSome = true ∧
    ResultSatisfies (fun t : CallState => ownerAccess t.own ⟨1⟩ .mutable = false ∧
      ownerAccess t.own ⟨1⟩ .shared = true)
      (passOwner DirectArgumentWitnesses.caller ⟨1⟩ [] .shared y) := by decide
/-- Instance: return ends a populated callRoot loan, not just local non-call loans. -/
theorem example_return_ends_populated_callRoot_loan_not : DirectArgumentWitnesses.called.own.loans.any Loan.callRoot = true ∧
    (leave DirectArgumentWitnesses.called.own).isSome = true ∧
    ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ 1)
      (leave DirectArgumentWitnesses.called.own) := by decide
/-- Planted: the callRoot survives the omitted cleanup and targets a departed activation. -/
theorem planted_callRoot_survives_omitted_cleanup_and_targets : (LifetimeEvidence.leaveWithoutEnding DirectArgumentWitnesses.called.own).isSome = true ∧
    ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ 1)
      (LifetimeEvidence.leaveWithoutEnding DirectArgumentWitnesses.called.own) ∧
    ¬ ResultSatisfies LiveDestinations
      (LifetimeEvidence.leaveWithoutEnding DirectArgumentWitnesses.called.own) := by decide
/-- Planted: omit only the current-frame destination guard; retain the issued-ID bound. -/
def reborrowBypass (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, m, some i,
        i :: a.ancestors, activation, false, false⟩ :: s.loans }
  else none
/-- Instance: an issued but departed activation refuses reborrowing. -/
theorem example_issued_but_departed_activation_refuses_reborrowing : LiveDestinations DirectArgumentWitnesses.returned.own ∧
    reborrowTo DirectArgumentWitnesses.returned.own ⟨0⟩ [] .shared 1 = none := by decide
/-- Planted: the same destination and arguments strand a child in the departed activation. -/
theorem planted_same_destination_and_arguments_strand_child : LiveDestinations DirectArgumentWitnesses.returned.own ∧
    ¬ ResultSatisfies LiveDestinations
      (reborrowBypass DirectArgumentWitnesses.returned.own ⟨0⟩ [] .shared 1) := by decide
end RoundTwoEvidence

namespace PendingEvidence
open Examples
/-- Planted: omit only the pending-parameter admission guard. -/
def passDownDuplicate (s : CallState) (source : LoanId) (parameter : Var) : Option CallState := do
  let l ← loan s.own source
  let own ← reborrow s.own source [] l.mode
  let own := { own with loans := own.loans.map fun l =>
    if l.id.id = s.own.nextLoan then { l with pendingArgument := true } else l }
  pure ⟨own, s.pending ++ [(parameter, ⟨s.own.nextLoan⟩)]⟩
/-- Planted: omit only the pending-parameter admission guard. -/
def passOwnerDuplicate (s : CallState) (root : Hybrid.Place) (path : List Projection)
    (mode : Mode) (parameter : Var) : Option CallState := do
  let f ← s.own.frames.head?
  if ownerAccess s.own root mode && decide (f.id < s.own.nextActivation) then
    pure ⟨{ s.own with
      nextLoan := s.own.nextLoan + 1
      loans := ⟨⟨s.own.nextLoan⟩, root, path, mode, none, [], f.id, true, true⟩ :: s.own.loans },
      s.pending ++ [(parameter, ⟨s.own.nextLoan⟩)]⟩
  else none
/-- Planted: omit only the pending-parameter admission guard. -/
def callLifePending (types : Var → Option Descriptor) : Exec.Life → CallState → Option CallState
  | .enter params, s => do
    if s.pending.all (fun e => decide (e.1 ∈ params)) then
      let own ← enter types (params.filter fun x => decide (x ∉ s.pending.map Prod.fst)) s.own
      let own ← adoptDown own s.pending
      pure ⟨own, []⟩
    else none
  | e, s => (life types e s.own).map fun own => { s with own := own }
/-- Two shared arguments can share a root but must have distinct parameter identities. -/
def first : CallState := (passOwner DirectArgumentWitnesses.caller ⟨1⟩ [] .shared x).getD DirectArgumentWitnesses.caller
/-- A pending shared child permits another shared downward argument. -/
def down : CallState := (passDown ⟨shared, []⟩ ⟨0⟩ x).getD ⟨initial, []⟩
/-- Instance (law `ConcreteSpec.Core.Own.passOwner_call_preserves`, case `ConcreteSpec.Core.Own.PendingEvidence.first`). Duplicate parameters are refused. -/
theorem passOwner_call_preserves_example_first : CallResult (passOwner first ⟨1⟩ [] .shared x) := @passOwner_call_preserves first ⟨1⟩ [] .shared x (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.passOwner_call_preserves`, case `ConcreteSpec.Core.Own.PendingEvidence.first`, subject `ConcreteSpec.Core.Own.passOwner`, mutant `ConcreteSpec.Core.Own.PendingEvidence.passOwnerDuplicate`). -/
theorem passOwner_call_preserves_planted_first_passOwnerDuplicate : CallState.WellFormed first ∧ passOwner first ⟨1⟩ [] .shared x = none ∧
    ¬ CallResult (passOwnerDuplicate first ⟨1⟩ [] .shared x) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.passDown_call_preserves`, case `ConcreteSpec.Core.Own.PendingEvidence.down`). Duplicate parameters are refused. -/
theorem passDown_call_preserves_example_down : CallResult (passDown down ⟨0⟩ x) := @passDown_call_preserves down ⟨0⟩ x (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.passDown_call_preserves`, case `ConcreteSpec.Core.Own.PendingEvidence.down`, subject `ConcreteSpec.Core.Own.passDown`, mutant `ConcreteSpec.Core.Own.PendingEvidence.passDownDuplicate`). -/
theorem passDown_call_preserves_planted_down_passDownDuplicate : CallState.WellFormed down ∧ passDown down ⟨0⟩ x = none ∧
    ¬ CallResult (passDownDuplicate down ⟨0⟩ x) := by decide
/-- Instance (law `ConcreteSpec.Core.Own.passDown_call_preserves`, case `ConcreteSpec.Core.Own.PendingEvidence.down`). Distinct numeric parameters can share a shared source. -/
theorem passDown_call_preserves_example_down_2 : CallResult (passDown down ⟨0⟩ y) :=
  @passDown_call_preserves down ⟨0⟩ y (by decide)
/-- Instance (law `ConcreteSpec.Core.Own.passOwner_call_preserves`, case `ConcreteSpec.Core.Own.PendingEvidence.first`). -/
theorem passOwner_call_preserves_example_first_2 : CallResult (passOwner first ⟨1⟩ [] .shared y) :=
  @passOwner_call_preserves first ⟨1⟩ [] .shared y (by decide)
/-- Instance: both shared argument paths accept a second distinct parameter. -/
theorem example_both_shared_argument_paths_accept_second : (passDown down ⟨0⟩ y).isSome = true ∧
    (passOwner first ⟨1⟩ [] .shared y).isSome = true := by decide
/-- Pending call state with a moved owner can exit only after its pending list is discharged. -/
def exiting : CallState := { first with own := { first.own with
  slots := first.own.slots.map fun s => { s with desc := { s.desc with copy := true } } } }
/-- Instance (law `ConcreteSpec.Core.Own.callLife_call_preserves`, case `ConcreteSpec.Core.Own.PendingEvidence.exiting`). -/
theorem callLife_call_preserves_example_exiting : CallResult (callLife types .leave exiting) :=
  @callLife_call_preserves types .leave exiting (by decide)
/-- Planted (law `ConcreteSpec.Core.Own.callLife_call_preserves`, case `ConcreteSpec.Core.Own.PendingEvidence.exiting`, subject `ConcreteSpec.Core.Own.callLife`, mutant `ConcreteSpec.Core.Own.PendingEvidence.callLifePending`). -/
theorem callLife_call_preserves_planted_exiting_callLifePending : CallState.WellFormed exiting ∧ callLife types .leave exiting = none ∧
    ¬ CallResult (callLifePending types .leave exiting) := by decide
end PendingEvidence

end ConcreteSpec.Core.Own
