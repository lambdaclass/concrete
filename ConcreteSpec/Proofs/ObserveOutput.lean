import ConcreteSpec.Proofs.Observe2

/-! # Independent physical-output correspondence and output-before-trap laws

`project_physical_outputs` and `output_before_trap_kept` are stated as general C6 operational laws
over `Exec.runFrom`, `ExecAdapter.project`, and `reportObservation`, parameterized by the operand
language's authoritative `PhysicalOperands` bundle and reaching the exact
`Exec.Ending O` of `Exec.runFrom`. `OutputDemo` supplies their closed `Instance` and `Planted`
witnesses using an adapter whose events distinguish non-output exchange records (`n < 0`) from
physical output records (`0 ≤ n`), so `OutputCorrespondence` is checked against independently
specified primitive effects.
-/
namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core

/-- Helper (computation). By induction on an event list `history`, `publicOutputs (A.project history)`
equals `history.flatMap F.past` whenever `A` satisfies `ExecAdapter.OutputCorrespondence A`;
feeds the operational laws `project_physical_outputs` and `output_before_trap_kept`.
(approved: PENDING) -/
theorem project_physical_outputs_list {O : Exec.Operands} {S : EventSignature}
    [F : PhysicalEffects O S.Output]
    (A : ExecAdapter O S) (corresponds : A.OutputCorrespondence) (history : List O.Event) :
    publicOutputs (A.project history) = history.flatMap F.past := by
  induction history with
  | nil => rfl
  | cons event history ih =>
    change publicOutputs (A.events event ++ A.project history) =
      F.past event ++ history.flatMap F.past
    have append : publicOutputs (A.events event ++ A.project history) =
        publicOutputs (A.events event) ++ publicOutputs (A.project history) := by
      simp only [publicOutputs, List.filterMap_append]
    rw [append, corresponds event, ih]

/-- Helper (computation). Every fuel-driver report has a physical execution reaching its exact
`Exec.Ending O` through guarded terminal/stopped constructors. This is an evaluator-to-relation
proof, not a second implementation of the evaluator. (approved: PENDING) -/
theorem physical_run_execution {O : Exec.Operands} (Output : Type) [F : PhysicalEffects O Output]
    (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) (c : Exec.Config O) :
    ∃ outputs, PhysicalEffects.Execution Output P ρ hist c outputs
      (Exec.runFrom P ρ fuel hist c).events (Exec.runFrom P ρ fuel hist c).ending := by
  induction fuel generalizing hist c with
  | zero =>
    unfold Exec.runFrom
    cases hf : c.final? with
    | some f => exact ⟨[], .final hf⟩
    | none => exact ⟨[], .outOfFuel hf⟩
  | succ fuel ih =>
    unfold Exec.runFrom
    cases hf : c.final? with
    | some f => exact ⟨[], .final hf⟩
    | none =>
      cases hs : Exec.step P c with
      | none => exact ⟨[], .stuckStep hf hs⟩
      | some out =>
        cases out with
        | next next =>
          obtain ⟨outputs, execution⟩ := ih hist next
          exact ⟨outputs, .silent hf hs execution⟩
        | request p vs site store k =>
          dsimp only
          cases ha : ρ hist p vs site store with
          | none => exact ⟨[], .noAnswer hf hs ha⟩
          | some a =>
            dsimp only
            by_cases allowed : O.allows p vs store a = true
            · simp only [allowed, ↓reduceIte]
              cases hk : k a with
              | none => exact ⟨[], .stuckCont hf hs ha allowed hk⟩
              | some next =>
                obtain ⟨outputs, execution⟩ := ih (hist ++ a.event.toList) next
                exact ⟨_, .answer hf hs ha allowed hk execution⟩
            · have disallowed : O.allows p vs store a = false :=
                Bool.eq_false_iff.mpr allowed
              simp only [allowed, Bool.false_eq_true, ↓reduceIte]
              exact ⟨[], .badAnswer hf hs ha disallowed⟩

/-- Helper (computation). Primitive faithfulness accumulates along an independently specified
physical execution to any guarded ending, preserving effect order. (approved: PENDING) -/
theorem physical_execution_outputs {O : Exec.Operands} (Output : Type) [F : PhysicalEffects O Output]
    (P : Exec.Program O) (ρ : Exec.Oracle O)
    (faithful : PhysicalEffects.OracleFaithful Output ρ) {hist : List O.Event} {c : Exec.Config O}
    {outputs : List Output} {finalHist : List O.Event} {ending : Exec.Ending O}
    (execution : PhysicalEffects.Execution Output P ρ hist c outputs finalHist ending) :
    finalHist.flatMap F.past = hist.flatMap F.past ++ outputs := by
  induction execution with
  | final => simp
  | outOfFuel => simp
  | stuckStep => simp
  | stuckCont => simp
  | noAnswer => simp
  | badAnswer => simp
  | silent _ _ _ ih => exact ih
  | @answer hist c p vs site store k a next outputs finalHist ending _ step answer allowed continued rest ih =>
    rw [ih, List.flatMap_append, faithful hist p vs site store a answer allowed,
      List.append_assoc]

/-- Helper (computation). Whenever an event history has a nonempty physical output interpretation
under the fixed `[PhysicalEffects O S.Output]` instance, an output-erasing adapter cannot satisfy
`ExecAdapter.OutputCorrespondence`. (approved: PENDING) -/
theorem no_erasing_correspondence {O : Exec.Operands} {S : EventSignature}
    [F : PhysicalEffects O S.Output] (A : ExecAdapter O S)
    {events : List O.Event} (nonempty : events.flatMap F.past ≠ []) :
    ¬ ExecAdapter.OutputCorrespondence (S := S) { A with events := fun _ => [] } := by
  intro erased
  apply nonempty
  clear nonempty
  induction events with
  | nil => rfl
  | cons ev rest ih =>
    have hev : F.past ev = [] := by
      have h := erased ev
      simpa [publicOutputs] using h.symm
    rw [List.flatMap_cons, hev, List.nil_append]
    exact ih

/-- Law (subject `ConcreteSpec.Core.Observe.ExecAdapter.project`).
An actual evaluator run admits an independently specified physical execution reaching its exact
`Exec.Ending O`, its public outputs are the incoming effects followed by that execution's primitive
effects, and any nonempty emitted output refutes an output-erasing adapter against the operand
language's authoritative physical operand bundle. -/
theorem project_physical_outputs {S : EventSignature}
    (M : PhysicalOperands S.Output)
    (P : Exec.Program M.operands) (A : ExecAdapter M.operands S)
    (corresponds : M.Admits A)
    (fuel : Nat) (hist : List M.operands.Event) (c : Exec.Config M.operands) :
    letI := M.effects
    ∃ outputs, PhysicalEffects.Execution S.Output P M.oracle hist c outputs
      (M.runFrom P fuel hist c).events (M.runFrom P fuel hist c).ending ∧
      publicOutputs (A.project (M.runFrom P fuel hist c).events) =
        hist.flatMap M.effects.past ++ outputs ∧
      (outputs ≠ [] → ¬ ExecAdapter.OutputCorrespondence (S := S) { A with events := fun _ => [] }) := by
  letI := M.effects
  obtain ⟨outputs, execution⟩ := physical_run_execution S.Output P M.oracle fuel hist c
  have haccum := physical_execution_outputs S.Output P M.oracle M.faithful execution
  refine ⟨outputs, execution, ?_, ?_⟩
  · rw [project_physical_outputs_list A corresponds]
    exact haccum
  · intro hne
    apply no_erasing_correspondence A (events := (M.runFrom P fuel hist c).events)
    rw [haccum]
    intro hempty
    exact hne (List.append_eq_nil_iff.mp hempty).2

/-- Fault: history projection erases all projected events. -/
def eraseProjection {O : Exec.Operands} {S : EventSignature}
    (_A : ExecAdapter O S) (_history : List O.Event) : List (Event S) := []

/-- Fault: history projection reverses the order of primitive events before projecting them. -/
def reverseProjection {O : Exec.Operands} {S : EventSignature}
    (A : ExecAdapter O S) (history : List O.Event) : List (Event S) :=
  history.reverse.flatMap A.events

/-- Law (subject `ConcreteSpec.Core.Observe.reportObservation`, `ConcreteSpec.Core.Observe.ExecAdapter.project`).
An actual trapped run reports its full history, diagnostic and final external state. That
history retains precisely the independently specified physical execution's ordered outputs up to
the trapped `.final` ending, and any nonempty emitted output refutes an output-erasing adapter. -/
theorem output_before_trap_kept {S : EventSignature}
    (M : PhysicalOperands S.Output)
    (P : Exec.Program M.operands) (A : ExecAdapter M.operands S) (C : OperationContract S) (input : C.Input)
    (corresponds : M.Admits A)
    (fuel : Nat) (hist : List M.operands.Event) (c : Exec.Config M.operands)
    (permitted : ExecAdapter.BlockingPermitted P A C input (M.runFrom P fuel hist c))
    (kind : TrapKind) (site : SiteId) (store : M.operands.Store) (sp : Span)
    (ending : (M.runFrom P fuel hist c).ending = .final (.trapped kind site) store)
    (resolved : AList.lookup A.sites site = some sp) (pathOk : sp.PathOk) :
    letI := M.effects
    reportObservation P A C input (M.runFrom P fuel hist c) permitted =
      some (.terminal ⟨A.project (M.runFrom P fuel hist c).events,
        .trapped kind site sp (trapLine kind site sp), A.state store⟩) ∧
    ∃ outputs, PhysicalEffects.Execution S.Output P M.oracle hist c outputs
      (M.runFrom P fuel hist c).events (.final (.trapped kind site) store) ∧
      publicOutputs (A.project (M.runFrom P fuel hist c).events) =
        hist.flatMap M.effects.past ++ outputs ∧
      (outputs ≠ [] → ¬ ExecAdapter.OutputCorrespondence (S := S) { A with events := fun _ => [] }) := by
  letI := M.effects
  obtain ⟨outputs, execution, hproj, herase⟩ :=
    project_physical_outputs M P A corresponds fuel hist c
  rw [ending] at execution
  refine ⟨?_, outputs, execution, hproj, herase⟩
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

/-- Independently specified primitive effects; installed into `OutputDemo.model` with the
operands and oracle, before adapter admission. The local instance supports relation notation. -/
instance effects : PhysicalEffects Exec.Demo.ops String where
  past := physical
  emit := fun p vs _ _ => match p, vs with
    | .log, [n] => if n < 0 then [] else [toString n]
    | _, _ => []

/-- Helper (computation). The actual C1 oracle records the argument-derived physical effect,
including counter updates with no output. (approved: PENDING) -/
theorem oracle_faithful : PhysicalEffects.OracleFaithful (O := Exec.Demo.ops) String Exec.Demo.env := by
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
        change physical n ++ [] = if n < 0 then [] else [toString n]
        simp [physical]
      | cons _ _ => simp [Exec.Demo.env] at answer
  | bump =>
    cases vs with
    | nil =>
      simp only [Exec.Demo.env, Option.some.injEq] at answer
      subst a
      rfl
    | cons _ _ => simp [Exec.Demo.env] at answer

/-- The concrete logging language owns its effects and primitive implementation. Adapters
for this language must retain this bundle when choosing a different event projection. -/
abbrev model : PhysicalOperands String := ⟨Exec.Demo.ops, effects, Exec.Demo.env, oracle_faithful⟩

/-- Helper (computation). Proves `ExecAdapter.OutputCorrespondence adapter` by case-splitting
on `n < 0` so exchange events (`[request, response]`) filter to `[]` while output events filter to
`[toString n]`; feeds the closed `Instance` and `Planted` witnesses of `project_physical_outputs`
and `output_before_trap_kept`. (approved: PENDING) -/
theorem adapter_correspondence : adapter.OutputCorrespondence := by
  intro n
  change publicOutputs (if n < 0 then [Demo.request n.natAbs, Demo.response (n.natAbs * 2)]
    else [.output (toString n)]) = physical n
  unfold physical
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
C1 events before the final trap, including a nonempty incoming history; feeds the permission witness and trap-ending premises for the closed `output_before_trap_kept` instances. (approved: PENDING) -/
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
theorem cannot_erase {outputs : List String} {ending : Exec.Ending Exec.Demo.ops}
    (execution : PhysicalEffects.Execution String program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events ending) :
    ¬ ([] = [-2, 11].flatMap (PhysicalEffects.past (O := Exec.Demo.ops) (Output := String)) ++ outputs) := by
  have hphys := physical_execution_outputs String program Exec.Demo.env oracle_faithful execution
  intro erased
  rw [← hphys] at erased
  change ([] : List String) = ["11", "3", "4"] at erased
  cases erased

/-- Instance (law `ConcreteSpec.Core.Observe.project_physical_outputs`, case `ordered_physical_outputs_on_run`). -/
example : ∃ outputs, PhysicalEffects.Execution String program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events
      (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).ending ∧
    publicOutputs (adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) =
      [-2, 11].flatMap (PhysicalEffects.past (O := Exec.Demo.ops) (Output := String)) ++ outputs ∧
    (outputs ≠ [] → ¬ ExecAdapter.OutputCorrespondence (S := Demo.signature) { adapter with events := fun _ => [] }) :=
  project_physical_outputs (S := Demo.signature) model program adapter adapter_correspondence 7 [-2, 11] (initial [3, 4])

/-- Planted (law `ConcreteSpec.Core.Observe.project_physical_outputs`, case `ordered_physical_outputs_on_run`,
subject `ConcreteSpec.Core.Observe.ExecAdapter.project`, mutant `ConcreteSpec.Core.Observe.eraseProjection`). -/
example : adapter.OutputCorrespondence ∧
    ¬ (∃ outputs, PhysicalEffects.Execution String program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events
      (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).ending ∧
    publicOutputs (eraseProjection adapter (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) =
      [-2, 11].flatMap (PhysicalEffects.past (O := Exec.Demo.ops) (Output := String)) ++ outputs ∧
    (outputs ≠ [] → ¬ ExecAdapter.OutputCorrespondence (S := Demo.signature) { adapter with events := fun _ => [] })) := by
  refine ⟨adapter_correspondence, ?_⟩
  rintro ⟨outputs, execution, equal, _⟩
  exact cannot_erase execution equal

/-- Planted (law `ConcreteSpec.Core.Observe.project_physical_outputs`, case `ordered_physical_outputs_on_run`,
subject `ConcreteSpec.Core.Observe.ExecAdapter.project`, mutant `ConcreteSpec.Core.Observe.reverseProjection`). -/
example : adapter.OutputCorrespondence ∧
    ¬ (∃ outputs, PhysicalEffects.Execution String program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events
      (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).ending ∧
    publicOutputs (reverseProjection adapter (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) =
      [-2, 11].flatMap (PhysicalEffects.past (O := Exec.Demo.ops) (Output := String)) ++ outputs ∧
    (outputs ≠ [] → ¬ ExecAdapter.OutputCorrespondence (S := Demo.signature) { adapter with events := fun _ => [] })) := by
  refine ⟨adapter_correspondence, ?_⟩
  rintro ⟨outputs, execution, equal, _⟩
  have hphys := physical_execution_outputs String program Exec.Demo.env oracle_faithful execution
  rw [← hphys] at equal
  change (["4", "3", "11"] : List String) = ["11", "3", "4"] at equal
  exact (by decide : (["4", "3", "11"] : List String) ≠ ["11", "3", "4"]) equal

/-- Instance (law `ConcreteSpec.Core.Observe.output_before_trap_kept`, case `two_outputs_then_trap_report`). -/
example : reportObservation program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) (permitted [3, 4] [-2, 11]) =
      some (.terminal ⟨adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events,
        .trapped .abort Exec.Demo.s trapSpan (trapLine .abort Exec.Demo.s trapSpan), adapter.state .init⟩) ∧
    ∃ outputs, PhysicalEffects.Execution String program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events
      (.final (.trapped .abort Exec.Demo.s) .init) ∧
    publicOutputs (adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) =
      [-2, 11].flatMap (PhysicalEffects.past (O := Exec.Demo.ops) (Output := String)) ++ outputs ∧
    (outputs ≠ [] → ¬ ExecAdapter.OutputCorrespondence (S := Demo.signature) { adapter with events := fun _ => [] }) :=
  output_before_trap_kept (S := Demo.signature) model program adapter Demo.contract ()
    adapter_correspondence 7 [-2, 11] (initial [3, 4])
    (permitted [3, 4] [-2, 11]) .abort Exec.Demo.s .init trapSpan rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.output_before_trap_kept`, case `two_outputs_then_trap_report`,
subject `ConcreteSpec.Core.Observe.reportObservation`, mutant `ConcreteSpec.Core.Observe.dropTrapHistoryReport`). -/
example : adapter.OutputCorrespondence ∧
    ExecAdapter.BlockingPermitted program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) ∧
    (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).ending =
      .final (.trapped .abort Exec.Demo.s) .init ∧
    AList.lookup adapter.sites Exec.Demo.s = some trapSpan ∧ trapSpan.PathOk ∧
    ¬ (dropTrapHistoryReport program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) (permitted [3, 4] [-2, 11]) =
      some (.terminal ⟨adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events,
        .trapped .abort Exec.Demo.s trapSpan (trapLine .abort Exec.Demo.s trapSpan), adapter.state .init⟩) ∧
      ∃ outputs, PhysicalEffects.Execution String program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events
      (.final (.trapped .abort Exec.Demo.s) .init) ∧
    publicOutputs (adapter.project (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) =
      [-2, 11].flatMap (PhysicalEffects.past (O := Exec.Demo.ops) (Output := String)) ++ outputs ∧
    (outputs ≠ [] → ¬ ExecAdapter.OutputCorrespondence (S := Demo.signature) { adapter with events := fun _ => [] })) := by
  refine ⟨adapter_correspondence, permitted [3, 4] [-2, 11],
    rfl, rfl, by decide, ?_⟩
  intro invalid
  have equal := congrArg (fun r : Option (FullObservation Demo.signature) =>
    match r with | some (.terminal obs) => obs.events.length | _ => 0) invalid.1
  change 0 = 5 at equal
  omega

/-- Planted (law `ConcreteSpec.Core.Observe.output_before_trap_kept`, case `two_outputs_then_trap_report`,
subject `ConcreteSpec.Core.Observe.ExecAdapter.project`, mutant `ConcreteSpec.Core.Observe.eraseProjection`). -/
example : adapter.OutputCorrespondence ∧
    ExecAdapter.BlockingPermitted program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) ∧
    (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).ending =
      .final (.trapped .abort Exec.Demo.s) .init ∧
    AList.lookup adapter.sites Exec.Demo.s = some trapSpan ∧ trapSpan.PathOk ∧
    ¬ (reportObservation program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])) (permitted [3, 4] [-2, 11]) =
      some (.terminal ⟨eraseProjection adapter (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events,
        .trapped .abort Exec.Demo.s trapSpan (trapLine .abort Exec.Demo.s trapSpan), adapter.state .init⟩) ∧
      ∃ outputs, PhysicalEffects.Execution String program Exec.Demo.env [-2, 11] (initial [3, 4])
      outputs (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events
      (.final (.trapped .abort Exec.Demo.s) .init) ∧
    publicOutputs (eraseProjection adapter (Exec.runFrom program Exec.Demo.env 7 [-2, 11] (initial [3, 4])).events) =
      [-2, 11].flatMap (PhysicalEffects.past (O := Exec.Demo.ops) (Output := String)) ++ outputs ∧
    (outputs ≠ [] → ¬ ExecAdapter.OutputCorrespondence (S := Demo.signature) { adapter with events := fun _ => [] })) := by
  refine ⟨adapter_correspondence, permitted [3, 4] [-2, 11],
    rfl, rfl, by decide, ?_⟩
  intro invalid
  obtain ⟨outputs, execution, erased, _⟩ := invalid.2
  exact cannot_erase execution erased

/-- Refused: an erasing adapter is not admitted for the unchanged concrete semantic operand
bundle. No ambient physical-effects instance is used by this admission proposition. -/
example : ¬ PhysicalOperands.Admits (S := Demo.signature) model { adapter with events := fun _ => [] } := by
  intro correspondence
  have output := correspondence 3
  change [] = ["3"] at output
  cases output

end OutputDemo
end ConcreteSpec.Core.Observe
