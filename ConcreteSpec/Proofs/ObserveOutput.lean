import ConcreteSpec.Proofs.Observe2

/-! # Independent physical-output correspondence and output-before-trap laws

`project_physical_outputs` and `output_before_trap_kept` are stated as general C6 operational laws
over `Exec.runFrom`, `ExecAdapter.project`, and `reportObservation`. `OutputDemo` supplies their
closed `Instance` and `Planted` witnesses using an adapter whose events distinguish non-output
exchange records (`n < 0`) from physical output records (`0 ≤ n`), so `OutputCorrespondence`
requires case analysis rather than `fun _ => rfl`.
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

/-- Law (subject `ConcreteSpec.Core.Observe.ExecAdapter.project`).
For any evaluator run `Exec.runFrom P ρ fuel hist c` and adapter `A` satisfying the per-event
`ExecAdapter.OutputCorrespondence A physical`, the public outputs extracted from the projected run
history `A.project (Exec.runFrom P ρ fuel hist c).events` equal the ordered physical outputs
`(Exec.runFrom P ρ fuel hist c).events.flatMap physical` emitted by the run. -/
theorem project_physical_outputs {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (ρ : Exec.Oracle O)
    (physical : O.Event → List S.Output)
    (corresponds : ExecAdapter.OutputCorrespondence A physical)
    (fuel : Nat) (hist : List O.Event) (c : Exec.Config O) :
    publicOutputs (A.project (Exec.runFrom P ρ fuel hist c).events) =
      (Exec.runFrom P ρ fuel hist c).events.flatMap physical :=
  project_physical_outputs_list A physical corresponds (Exec.runFrom P ρ fuel hist c).events

/-- Fault: history projection erases all projected events. -/
def eraseProjection {O : Exec.Operands} {S : EventSignature}
    (_A : ExecAdapter O S) (_history : List O.Event) : List (Event S) := []

/-- Fault: history projection reverses the order of primitive events before projecting them. -/
def reverseProjection {O : Exec.Operands} {S : EventSignature}
    (A : ExecAdapter O S) (history : List O.Event) : List (Event S) :=
  history.reverse.flatMap A.events

/-- Law (subject `ConcreteSpec.Core.Observe.reportObservation`, `ConcreteSpec.Core.Observe.ExecAdapter.project`).
When an evaluator run ends in `.final (.trapped kind site) store` at a valid resolved span `sp`,
`reportObservation` produces the trapped terminal observation retaining the full projected event
history `A.project (Exec.runFrom P ρ fuel hist c).events` emitted before the trap, the diagnostic
line `trapLine kind site sp`, and the final external state `A.state store`; moreover, under
`ExecAdapter.OutputCorrespondence A physical`, the public outputs in that reported history are
exactly `(Exec.runFrom P ρ fuel hist c).events.flatMap physical`. -/
theorem output_before_trap_kept {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (ρ : Exec.Oracle O) (physical : O.Event → List S.Output)
    (corresponds : ExecAdapter.OutputCorrespondence A physical)
    (fuel : Nat) (hist : List O.Event) (c : Exec.Config O)
    (permitted : ExecAdapter.BlockingPermitted P A C input (Exec.runFrom P ρ fuel hist c))
    (kind : TrapKind) (site : SiteId) (store : O.Store) (sp : Span)
    (ending : (Exec.runFrom P ρ fuel hist c).ending = .final (.trapped kind site) store)
    (resolved : AList.lookup A.sites site = some sp) (pathOk : sp.PathOk) :
    reportObservation P A C input (Exec.runFrom P ρ fuel hist c) permitted =
      some (.terminal ⟨A.project (Exec.runFrom P ρ fuel hist c).events,
        .trapped kind site sp (trapLine kind site sp), A.state store⟩) ∧
    publicOutputs (A.project (Exec.runFrom P ρ fuel hist c).events) =
      (Exec.runFrom P ρ fuel hist c).events.flatMap physical := by
  refine ⟨?_, project_physical_outputs P A ρ physical corresponds fuel hist c⟩
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

/-- Instance (law `ConcreteSpec.Core.Observe.project_physical_outputs`, case `ordered_physical_outputs_on_run`). -/
example : publicOutputs (adapter.project
      (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events) =
    (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events.flatMap
      physical :=
  project_physical_outputs program adapter Exec.Demo.env physical adapter_correspondence
    (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])

/-- Planted (law `ConcreteSpec.Core.Observe.project_physical_outputs`, case `ordered_physical_outputs_on_run`,
subject `ConcreteSpec.Core.Observe.ExecAdapter.project`, mutant `ConcreteSpec.Core.Observe.eraseProjection`). -/
example : ExecAdapter.OutputCorrespondence adapter physical ∧
    ¬ (publicOutputs (eraseProjection adapter
        (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events) =
      (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events.flatMap
        physical) := by
  refine ⟨adapter_correspondence, ?_⟩
  decide

/-- Instance (law `ConcreteSpec.Core.Observe.project_physical_outputs`, case `reverse_projected_outputs_on_run`). -/
example : publicOutputs (adapter.project
      (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events) =
    (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events.flatMap
      physical :=
  project_physical_outputs program adapter Exec.Demo.env physical adapter_correspondence
    (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])

/-- Planted (law `ConcreteSpec.Core.Observe.project_physical_outputs`, case `reverse_projected_outputs_on_run`,
subject `ConcreteSpec.Core.Observe.ExecAdapter.project`, mutant `ConcreteSpec.Core.Observe.reverseProjection`). -/
example : ExecAdapter.OutputCorrespondence adapter physical ∧
    ¬ (publicOutputs (reverseProjection adapter
        (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events) =
      (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events.flatMap
        physical) := by
  refine ⟨adapter_correspondence, ?_⟩
  decide

/-- Fault: an adapter drops every physical output while retaining request and trap projection. -/
def erasingAdapter : ExecAdapter Exec.Demo.ops Demo.signature :=
  { adapter with events := fun _ => [] }

/-- Refused: erasing physical output violates the independently supplied correspondence
obligation, even though an empty projected history could pass request/response trace checks. -/
example : ¬ ExecAdapter.OutputCorrespondence erasingAdapter physical := by
  intro correspondence
  have output := correspondence 3
  change [] = ["3"] at output
  cases output

/-- Instance (law `ConcreteSpec.Core.Observe.output_before_trap_kept`, case `two_outputs_then_trap_report`). -/
example : reportObservation program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4]))
      (permitted [3, 4] [-2, 11]) =
      some (.terminal ⟨adapter.project
          (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events,
        .trapped .abort Exec.Demo.s trapSpan (trapLine .abort Exec.Demo.s trapSpan),
        adapter.state .init⟩) ∧
    publicOutputs (adapter.project
      (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events) =
      (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events.flatMap
        physical :=
  output_before_trap_kept program adapter Demo.contract () Exec.Demo.env physical
    adapter_correspondence (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])
    (permitted [3, 4] [-2, 11]) .abort Exec.Demo.s .init trapSpan rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.output_before_trap_kept`, case `two_outputs_then_trap_report`,
subject `ConcreteSpec.Core.Observe.reportObservation`, mutant `ConcreteSpec.Core.Observe.dropTrapHistoryReport`). -/
example : ExecAdapter.OutputCorrespondence adapter physical ∧
    (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).ending =
      .final (.trapped .abort Exec.Demo.s) .init ∧
    AList.lookup adapter.sites Exec.Demo.s = some trapSpan ∧
    trapSpan.PathOk ∧
    ¬ (dropTrapHistoryReport program adapter Demo.contract ()
        (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4]))
        (permitted [3, 4] [-2, 11]) =
        some (.terminal ⟨adapter.project
            (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events,
          .trapped .abort Exec.Demo.s trapSpan (trapLine .abort Exec.Demo.s trapSpan),
          adapter.state .init⟩) ∧
      publicOutputs (adapter.project
        (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events) =
        (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events.flatMap
          physical) := by
  refine ⟨adapter_correspondence, rfl, rfl, by decide, ?_⟩
  intro invalid
  have heq := invalid.1
  have hlen := congrArg (fun r : Option (FullObservation Demo.signature) =>
    match r with | some (.terminal obs) => obs.events.length | _ => 0) heq
  change 0 = 5 at hlen
  omega

/-- Instance (law `ConcreteSpec.Core.Observe.output_before_trap_kept`, case `two_outputs_then_trap_project`). -/
example : reportObservation program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4]))
      (permitted [3, 4] [-2, 11]) =
      some (.terminal ⟨adapter.project
          (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events,
        .trapped .abort Exec.Demo.s trapSpan (trapLine .abort Exec.Demo.s trapSpan),
        adapter.state .init⟩) ∧
    publicOutputs (adapter.project
      (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events) =
      (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events.flatMap
        physical :=
  output_before_trap_kept program adapter Demo.contract () Exec.Demo.env physical
    adapter_correspondence (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])
    (permitted [3, 4] [-2, 11]) .abort Exec.Demo.s .init trapSpan rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.output_before_trap_kept`, case `two_outputs_then_trap_project`,
subject `ConcreteSpec.Core.Observe.ExecAdapter.project`, mutant `ConcreteSpec.Core.Observe.eraseProjection`). -/
example : ExecAdapter.OutputCorrespondence adapter physical ∧
    (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).ending =
      .final (.trapped .abort Exec.Demo.s) .init ∧
    AList.lookup adapter.sites Exec.Demo.s = some trapSpan ∧
    trapSpan.PathOk ∧
    ¬ (reportObservation program adapter Demo.contract ()
        (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4]))
        (permitted [3, 4] [-2, 11]) =
        some (.terminal ⟨eraseProjection adapter
            (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events,
          .trapped .abort Exec.Demo.s trapSpan (trapLine .abort Exec.Demo.s trapSpan),
          adapter.state .init⟩) ∧
      publicOutputs (eraseProjection adapter
        (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events) =
        (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [-2, 11] (initial [3, 4])).events.flatMap
          physical) := by
  refine ⟨adapter_correspondence, rfl, rfl, by decide, ?_⟩
  intro invalid
  have hout := invalid.2
  change ([] : List String) = ["11", "3", "4"] at hout
  cases hout

end OutputDemo
end ConcreteSpec.Core.Observe
