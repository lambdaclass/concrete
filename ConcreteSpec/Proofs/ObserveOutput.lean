import ConcreteSpec.Proofs.Observe2

/-! # Independent physical-output correspondence and output-before-trap laws

`project_physical_outputs` and `output_before_trap_kept` are stated as general C6 operational laws
over `Exec.runFrom`, `ExecAdapter.project`, and `reportObservation`. `OutputDemo` supplies their
closed `Instance` and `Planted` witnesses using an adapter whose events distinguish non-output
exchange records (`n < 0`) from physical output records (`0 ≤ n`), so `OutputCorrespondence`
is checked against independently specified primitive effects.
-/
namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core

/-- Helper (computation). By induction on an event list `history`, `publicOutputs (A.project history)`
equals `history.flatMap physical` whenever `A` satisfies `ExecAdapter.OutputCorrespondence A physical`;
feeds the operational laws `project_physical_outputs` and `output_before_trap_kept`.
(approved: PENDING) -/
theorem project_physical_outputs_list {O : Exec.Operands} {S : EventSignature}
    (A : ExecAdapter O S) (physical : O.Event → List S.Output)
    (corresponds : ExecAdapter.OutputCorrespondence A physical) (history : List O.Event) :
    publicOutputs (A.project history) = history.flatMap physical := by
  induction history with
  | nil => rfl
  | cons event history ih =>
    change publicOutputs (A.events event ++ A.project history) =
      physical event ++ history.flatMap physical
    have append : publicOutputs (A.events event ++ A.project history) =
        publicOutputs (A.events event) ++ publicOutputs (A.project history) := by
      simp only [publicOutputs, List.filterMap_append]
    rw [append, corresponds event, ih]

/-- Helper (computation). Every fuel-driver report has a physical execution prefix. This is
an evaluator-to-relation proof, not a second implementation of the evaluator. (approved: PENDING) -/
theorem physical_run_execution {O : Exec.Operands} {Output : Type}
    (F : PhysicalEffects O Output) (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) (c : Exec.Config O) :
    ∃ outputs finalConfig, F.Execution P ρ hist c outputs
      (Exec.runFrom P ρ fuel hist c).events finalConfig := by
  induction fuel generalizing hist c with
  | zero =>
    unfold Exec.runFrom
    cases c.final? <;> exact ⟨[], c, .refl hist c⟩
  | succ fuel ih =>
    unfold Exec.runFrom
    cases hf : c.final? with
    | some f => exact ⟨[], c, .refl hist c⟩
    | none =>
      cases hs : Exec.step P c with
      | none => exact ⟨[], c, .refl hist c⟩
      | some out =>
        cases out with
        | next next =>
          obtain ⟨outputs, finalConfig, execution⟩ := ih hist next
          exact ⟨outputs, finalConfig, .silent hs execution⟩
        | request p vs site store k =>
          dsimp only
          cases ha : ρ hist p vs site store with
          | none => exact ⟨[], c, .refl hist c⟩
          | some a =>
            dsimp only
            by_cases allowed : O.allows p vs store a = true
            · simp only [allowed, ↓reduceIte]
              cases hk : k a with
              | none => exact ⟨[], c, .refl hist c⟩
              | some next =>
                obtain ⟨outputs, finalConfig, execution⟩ := ih (hist ++ a.event.toList) next
                exact ⟨_, finalConfig, .answer hs ha allowed hk execution⟩
            · simp only [allowed, Bool.false_eq_true, ↓reduceIte]
              exact ⟨[], c, .refl hist c⟩

/-- Helper (computation). Primitive faithfulness accumulates along an independently specified
physical execution, preserving effect order. (approved: PENDING) -/
theorem physical_execution_outputs {O : Exec.Operands} {Output : Type}
    (F : PhysicalEffects O Output) (P : Exec.Program O) (ρ : Exec.Oracle O)
    (faithful : F.OracleFaithful ρ) {hist : List O.Event} {c : Exec.Config O}
    {outputs : List Output} {finalHist : List O.Event} {finalConfig : Exec.Config O}
    (execution : F.Execution P ρ hist c outputs finalHist finalConfig) :
    finalHist.flatMap F.past = hist.flatMap F.past ++ outputs := by
  induction execution with
  | refl => simp
  | silent _ _ ih => exact ih
  | @answer hist c p vs site store k a next outputs finalHist finalConfig step answer allowed continued rest ih =>
    rw [ih, List.flatMap_append, faithful hist p vs site store a answer allowed,
      List.append_assoc]

/-- Law (subject `ConcreteSpec.Core.Observe.ExecAdapter.project`).
An actual evaluator run admits an independently specified physical execution, and its public
outputs are exactly the incoming effects followed by that execution's primitive effects. The
physical interface fixes effects from primitive arguments, store and answer, independently of
the observation adapter. Erasing public output cannot satisfy this law against unchanged effects. -/
theorem project_physical_outputs {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (ρ : Exec.Oracle O)
    (physical : PhysicalEffects O S.Output)
    (corresponds : ExecAdapter.OutputCorrespondence A physical.past)
    (faithful : physical.OracleFaithful ρ)
    (fuel : Nat) (hist : List O.Event) (c : Exec.Config O) :
    ∃ outputs finalConfig, physical.Execution P ρ hist c outputs
      (Exec.runFrom P ρ fuel hist c).events finalConfig ∧
      publicOutputs (A.project (Exec.runFrom P ρ fuel hist c).events) =
        hist.flatMap physical.past ++ outputs := by
  obtain ⟨outputs, finalConfig, execution⟩ := physical_run_execution physical P ρ fuel hist c
  refine ⟨outputs, finalConfig, execution, ?_⟩
  rw [project_physical_outputs_list A physical.past corresponds]
  exact physical_execution_outputs physical P ρ faithful execution

/-- Fault: history projection erases all projected events. -/
def eraseProjection {O : Exec.Operands} {S : EventSignature}
    (_A : ExecAdapter O S) (_history : List O.Event) : List (Event S) := []

/-- Fault: history projection reverses the order of primitive events before projecting them. -/
def reverseProjection {O : Exec.Operands} {S : EventSignature}
    (A : ExecAdapter O S) (history : List O.Event) : List (Event S) :=
  history.reverse.flatMap A.events

/-- Law (subject `ConcreteSpec.Core.Observe.reportObservation`, `ConcreteSpec.Core.Observe.ExecAdapter.project`).
An actual trapped run reports its full history, diagnostic and final external state. That
history retains precisely the independently specified physical execution's ordered outputs. -/
theorem output_before_trap_kept {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (ρ : Exec.Oracle O) (physical : PhysicalEffects O S.Output)
    (corresponds : ExecAdapter.OutputCorrespondence A physical.past)
    (faithful : physical.OracleFaithful ρ)
    (fuel : Nat) (hist : List O.Event) (c : Exec.Config O)
    (permitted : ExecAdapter.BlockingPermitted P A C input (Exec.runFrom P ρ fuel hist c))
    (kind : TrapKind) (site : SiteId) (store : O.Store) (sp : Span)
    (ending : (Exec.runFrom P ρ fuel hist c).ending = .final (.trapped kind site) store)
    (resolved : AList.lookup A.sites site = some sp) (pathOk : sp.PathOk) :
    reportObservation P A C input (Exec.runFrom P ρ fuel hist c) permitted =
      some (.terminal ⟨A.project (Exec.runFrom P ρ fuel hist c).events,
        .trapped kind site sp (trapLine kind site sp), A.state store⟩) ∧
    ∃ outputs finalConfig, physical.Execution P ρ hist c outputs
      (Exec.runFrom P ρ fuel hist c).events finalConfig ∧
      publicOutputs (A.project (Exec.runFrom P ρ fuel hist c).events) =
        hist.flatMap physical.past ++ outputs := by
  refine ⟨?_, project_physical_outputs P A ρ physical corresponds faithful fuel hist c⟩
  simp [reportObservation, ending, ExecAdapter.terminal?, trapTerminal, resolved, pathOk]

/-- Fault: drops all events emitted before a trap when reporting a trapped terminal state. -/
def dropTrapHistoryReport {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
    Option (FullObservation S) :=
  match reportObservation P A C input r permitted with
  | some (.terminal ⟨_, .trapped kind site sp line, state⟩) =>
    some (.terminal ⟨[], .trapped kind site sp line, state⟩)
  | other => other

namespace OutputDemo

/-- Span used for `Exec.Demo.s` in `OutputDemo`. -/
def trapSpan : Span := sourceSpan "src/main.con" (by decide) 1 (by decide) "a\té"

/-- Adapter distinguishing negative C1 events (projected to a request/response exchange with no
public output) from non-negative C1 events (projected to public text output). -/
def adapter : ExecAdapter Exec.Demo.ops Demo.signature where
  events := fun n =>
    if n < 0 then
      [Demo.request n.natAbs, Demo.response (n.natAbs * 2)]
    else
      [.output (toString n)]
  request := fun _ _ _ _ => none
  status := fun _ => none
  state := fun st => st.counter
  sites := [(Exec.Demo.s, trapSpan)]

/-- Physical public outputs of a C1 event in `OutputDemo`: negative exchange events produce `[]`,
while non-negative logging events produce `[toString n]`. -/
def physical (n : Int) : List String :=
  if n < 0 then [] else [toString n]

/-- Physical semantics are fixed by primitive arguments, independently of the adapter. -/
def effects : PhysicalEffects Exec.Demo.ops String where
  past := physical
  emit := fun p vs _ _ => match p, vs with
    | .log, [n] => if n < 0 then [] else [toString n]
    | _, _ => []

/-- Helper (computation). The actual C1 oracle records the argument-derived physical effect,
including counter updates with no output. (approved: PENDING) -/
theorem oracle_faithful : effects.OracleFaithful Exec.Demo.env := by
  intro hist p vs site store a answer allowed
  cases p with
  | log =>
    cases vs with
    | nil => simp [Exec.Demo.env] at answer
    | cons n ns =>
      cases ns with
      | nil =>
        simp only [Exec.Demo.env, Option.some.injEq] at answer
        subst a
        simp [effects, physical]
      | cons _ _ => simp [Exec.Demo.env] at answer
  | bump =>
    cases vs with
    | nil =>
      simp only [Exec.Demo.env, Option.some.injEq] at answer
      subst a
      rfl
    | cons _ _ => simp [Exec.Demo.env] at answer

/-- Helper (computation). Proves `ExecAdapter.OutputCorrespondence adapter physical` by case-splitting
on `n < 0` so exchange events (`[request, response]`) filter to `[]` while output events filter to
`[toString n]`; feeds the closed `Instance` and `Planted` witnesses of `project_physical_outputs`
and `output_before_trap_kept`. (approved: PENDING) -/
theorem adapter_correspondence : ExecAdapter.OutputCorrespondence adapter physical := by
  intro n
  unfold adapter physical
  by_cases h : n < 0
  · simp [h, publicOutputs, Demo.request, Demo.response]
  · simp [h, publicOutputs]

def program : Exec.Program Exec.Demo.ops := ⟨[]⟩

/-- Emit each integer as physical output before taking an unconditional trap. -/
def writeThenTrap : List Int → Exec.Stmt Exec.Demo.ops
  | [] => .abort Exec.Demo.s
  | n :: ns => .seq (.prim none .log [.lit n] Exec.Demo.s) (writeThenTrap ns)

def initial (values : List Int) : Exec.Config Exec.Demo.ops :=
  ⟨.exec (writeThenTrap values), [], .init, []⟩

/-- Helper (computation). Induction over executed logging operations establishes their actual
C1 events before the final trap, including a nonempty incoming history; feeds `permitted` and
`trap_ending` for the closed `output_before_trap_kept` instances. (approved: PENDING) -/
theorem run_writes (values : List Int) (history : List Int) :
    Exec.runFrom program Exec.Demo.env (3 * values.length + 1) history (initial values) =
      ⟨history ++ values, .final (.trapped .abort Exec.Demo.s) .init⟩ := by
  induction values generalizing history with
  | nil =>
    simp only [List.length_nil, Nat.mul_zero, Nat.zero_add, List.append_nil]
    unfold Exec.runFrom
    rfl
  | cons n ns ih =>
    have fuel : 3 * (n :: ns).length + 1 = 3 * ns.length + 1 + 3 := by simp; omega
    rw [fuel]
    conv => lhs; unfold Exec.runFrom
    dsimp [initial, writeThenTrap, Exec.Config.final?, Exec.step]
    conv => lhs; unfold Exec.runFrom
    simp [Exec.Config.final?, Exec.step, Exec.onRes, Exec.evalArgs, Exec.Demo.ops,
      Exec.Demo.evalE, Exec.Demo.env, Res.bind, Res.map, Exec.bindLife, Exec.bindDst]
    conv => lhs; unfold Exec.runFrom
    change Exec.runFrom program Exec.Demo.env (3 * ns.length + 1) (history ++ [n]) (initial ns) = _
    rw [ih]
    simp [List.append_assoc]

/-- Helper (computation). A completed concrete run cannot have a pending request; supplies the
`BlockingPermitted` argument for the closed `output_before_trap_kept` instances.
(approved: PENDING) -/
theorem permitted (values : List Int) (history : List Int) :
    ExecAdapter.BlockingPermitted program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env (3 * values.length + 1) history (initial values)) := by
  intro c p vs site store k q ending pending typed
  rw [run_writes] at ending
  cases ending

/-- Helper (computation). Any physical execution to the actual history has a nonempty effect
list, as forced by primitive semantics and the unchanged incoming effects. (approved: PENDING) -/
theorem cannot_erase {outputs : List String} {finalConfig : Exec.Config Exec.Demo.ops}
    (execution : effects.Execution program Exec.Demo.env [-2, 11] (initial [3, 4]) outputs
      (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events finalConfig) :
    ¬ ([] = [-2, 11].flatMap effects.past ++ outputs) := by
  have physical := physical_execution_outputs effects program Exec.Demo.env oracle_faithful execution
  intro erased
  rw [← physical] at erased
  change ([] : List String) = ["11", "3", "4"] at erased
  cases erased

/-- Instance (law `ConcreteSpec.Core.Observe.project_physical_outputs`, case `ordered_physical_outputs_on_run`). -/
example : ∃ outputs finalConfig, effects.Execution program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events finalConfig ∧
    publicOutputs (adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) = [-2, 11].flatMap effects.past ++ outputs :=
  project_physical_outputs program adapter Exec.Demo.env effects adapter_correspondence oracle_faithful 7 [-2, 11] (initial [3, 4])

/-- Planted (law `ConcreteSpec.Core.Observe.project_physical_outputs`, case `ordered_physical_outputs_on_run`,
subject `ConcreteSpec.Core.Observe.ExecAdapter.project`, mutant `ConcreteSpec.Core.Observe.eraseProjection`). -/
example : ExecAdapter.OutputCorrespondence adapter effects.past ∧
    effects.OracleFaithful Exec.Demo.env ∧
    ¬ (∃ outputs finalConfig, effects.Execution program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events finalConfig ∧
    publicOutputs (eraseProjection adapter (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) = [-2, 11].flatMap effects.past ++ outputs) := by
  refine ⟨adapter_correspondence, oracle_faithful, ?_⟩
  rintro ⟨outputs, finalConfig, execution, equal⟩
  exact cannot_erase execution equal

/-- Instance (law `ConcreteSpec.Core.Observe.project_physical_outputs`, case `reverse_projected_outputs_on_run`). -/
example : ∃ outputs finalConfig, effects.Execution program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events finalConfig ∧
    publicOutputs (adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) = [-2, 11].flatMap effects.past ++ outputs :=
  project_physical_outputs program adapter Exec.Demo.env effects adapter_correspondence oracle_faithful 7 [-2, 11] (initial [3, 4])

/-- Planted (law `ConcreteSpec.Core.Observe.project_physical_outputs`, case `reverse_projected_outputs_on_run`,
subject `ConcreteSpec.Core.Observe.ExecAdapter.project`, mutant `ConcreteSpec.Core.Observe.reverseProjection`). -/
example : ExecAdapter.OutputCorrespondence adapter effects.past ∧
    effects.OracleFaithful Exec.Demo.env ∧
    ¬ (∃ outputs finalConfig, effects.Execution program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events finalConfig ∧
    publicOutputs (reverseProjection adapter (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) = [-2, 11].flatMap effects.past ++ outputs) := by
  refine ⟨adapter_correspondence, oracle_faithful, ?_⟩
  rintro ⟨outputs, finalConfig, execution, equal⟩
  have physical := physical_execution_outputs effects program Exec.Demo.env oracle_faithful execution
  rw [← physical] at equal
  change (["4", "3", "11"] : List String) = ["11", "3", "4"] at equal
  exact (by decide : (["4", "3", "11"] : List String) ≠ ["11", "3", "4"]) equal

/-- Instance (law `ConcreteSpec.Core.Observe.output_before_trap_kept`, case `two_outputs_then_trap_report`). -/
example : reportObservation program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) (permitted [3, 4] [-2, 11]) =
      some (.terminal ⟨adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events,
        .trapped .abort Exec.Demo.s trapSpan (trapLine .abort Exec.Demo.s trapSpan), adapter.state .init⟩) ∧
    ∃ outputs finalConfig, effects.Execution program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events finalConfig ∧
    publicOutputs (adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) = [-2, 11].flatMap effects.past ++ outputs :=
  output_before_trap_kept program adapter Demo.contract () Exec.Demo.env effects
    adapter_correspondence oracle_faithful 7 [-2, 11] (initial [3, 4])
    (permitted [3, 4] [-2, 11]) .abort Exec.Demo.s .init trapSpan rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.output_before_trap_kept`, case `two_outputs_then_trap_report`,
subject `ConcreteSpec.Core.Observe.reportObservation`, mutant `ConcreteSpec.Core.Observe.dropTrapHistoryReport`). -/
example : ExecAdapter.OutputCorrespondence adapter effects.past ∧
    effects.OracleFaithful Exec.Demo.env ∧
    (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).ending =
      .final (.trapped .abort Exec.Demo.s) .init ∧
    AList.lookup adapter.sites Exec.Demo.s = some trapSpan ∧ trapSpan.PathOk ∧
    ¬ (dropTrapHistoryReport program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) (permitted [3, 4] [-2, 11]) =
      some (.terminal ⟨adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events,
        .trapped .abort Exec.Demo.s trapSpan (trapLine .abort Exec.Demo.s trapSpan), adapter.state .init⟩) ∧
      ∃ outputs finalConfig, effects.Execution program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events finalConfig ∧
    publicOutputs (adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) = [-2, 11].flatMap effects.past ++ outputs) := by
  refine ⟨adapter_correspondence, oracle_faithful, rfl, rfl, by decide, ?_⟩
  intro invalid
  have equal := congrArg (fun r : Option (FullObservation Demo.signature) =>
    match r with | some (.terminal obs) => obs.events.length | _ => 0) invalid.1
  change 0 = 5 at equal
  omega

/-- Instance (law `ConcreteSpec.Core.Observe.output_before_trap_kept`, case `two_outputs_then_trap_project`). -/
example : reportObservation program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) (permitted [3, 4] [-2, 11]) =
      some (.terminal ⟨adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events,
        .trapped .abort Exec.Demo.s trapSpan (trapLine .abort Exec.Demo.s trapSpan), adapter.state .init⟩) ∧
    ∃ outputs finalConfig, effects.Execution program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events finalConfig ∧
    publicOutputs (adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) = [-2, 11].flatMap effects.past ++ outputs :=
  output_before_trap_kept program adapter Demo.contract () Exec.Demo.env effects
    adapter_correspondence oracle_faithful 7 [-2, 11] (initial [3, 4])
    (permitted [3, 4] [-2, 11]) .abort Exec.Demo.s .init trapSpan rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.output_before_trap_kept`, case `two_outputs_then_trap_project`,
subject `ConcreteSpec.Core.Observe.ExecAdapter.project`, mutant `ConcreteSpec.Core.Observe.eraseProjection`). -/
example : ExecAdapter.OutputCorrespondence adapter effects.past ∧
    effects.OracleFaithful Exec.Demo.env ∧
    (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).ending =
      .final (.trapped .abort Exec.Demo.s) .init ∧
    AList.lookup adapter.sites Exec.Demo.s = some trapSpan ∧ trapSpan.PathOk ∧
    ¬ (reportObservation program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) (permitted [3, 4] [-2, 11]) =
      some (.terminal ⟨eraseProjection adapter (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events,
        .trapped .abort Exec.Demo.s trapSpan (trapLine .abort Exec.Demo.s trapSpan), adapter.state .init⟩) ∧
      ∃ outputs finalConfig, effects.Execution program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events finalConfig ∧
    publicOutputs (eraseProjection adapter (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) = [-2, 11].flatMap effects.past ++ outputs) := by
  refine ⟨adapter_correspondence, oracle_faithful, rfl, rfl, by decide, ?_⟩
  intro invalid
  obtain ⟨outputs, finalConfig, execution, erased⟩ := invalid.2
  exact cannot_erase execution erased

/-- Refused: physical semantics stay fixed when the adapter erases outputs. -/
example : ¬ ExecAdapter.OutputCorrespondence { adapter with events := fun _ => [] } effects.past := by
  intro correspondence
  have output := correspondence 3
  change [] = ["3"] at output
  cases output

end OutputDemo
end ConcreteSpec.Core.Observe
