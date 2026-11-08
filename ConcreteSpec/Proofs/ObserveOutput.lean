import ConcreteSpec.Proofs.Observe2

/-! # Independent physical-output correspondence

C1's logging primitive emits the integer it receives. The adapter below represents that public
output as text. The operational law executes a list of those primitives and then traps; it does
not assume a projected history or an output-preservation invariant.
-/
namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core
/-- Law (subject `ConcreteSpec.Core.Observe.ExecAdapter.project`).
A projection satisfying the independently supplied physical-output interpretation for each
primitive event preserves those physical outputs across the entire ordered history. -/
theorem project_physical_outputs {O : Exec.Operands} {S : EventSignature}
    (A : ExecAdapter O S) (physical : O.Event → List S.Output)
    (corresponds : ExecAdapter.OutputCorrespondence A physical) (history : List O.Event) :
    publicOutputs (A.project history) = history.flatMap physical := by
  induction history with
  | nil => rfl
  | cons event history ih =>
    change publicOutputs (A.events event ++ A.project history) = physical event ++ history.flatMap physical
    have append : publicOutputs (A.events event ++ A.project history) =
        publicOutputs (A.events event) ++ publicOutputs (A.project history) := by
      simp only [publicOutputs, List.filterMap_append]
    rw [append, corresponds event, ih]

/-- Fault: history projection erases all projected events. -/
def eraseProjection {O : Exec.Operands} {S : EventSignature}
    (_A : ExecAdapter O S) (_history : List O.Event) : List (Event S) := []

namespace OutputDemo

/-- A concrete public output instance, independent of request/response admission. -/
def adapter : ExecAdapter Exec.Demo.ops Demo.signature where
  events := fun n => [.output (toString n)]
  request := fun _ _ _ _ => none
  status := fun _ => none
  sites := [(Exec.Demo.s, sourceSpan "src/main.con" 1 (by decide) "a\té")]

def program : Exec.Program Exec.Demo.ops := ⟨[]⟩

/-- Emit each integer as physical output before taking an unconditional trap. -/
def writeThenTrap : List Int → Exec.Stmt Exec.Demo.ops
  | [] => .abort Exec.Demo.s
  | n :: ns => .seq (.prim none .log [.lit n] Exec.Demo.s) (writeThenTrap ns)

def initial (values : List Int) : Exec.Config Exec.Demo.ops :=
  ⟨.exec (writeThenTrap values), [], .init, []⟩

/-- Helper (computation). Induction over executed logging operations establishes their actual
C1 events before the final trap, including a nonempty incoming history. It supports the adapter
law below rather than assuming its result. (approved: PENDING) -/
theorem run_writes (values : List Int) (history : List Int) :
    Exec.runFrom program Exec.Demo.env (3 * values.length + 1) history (initial values) =
      ⟨history ++ values, .final (.trapped .abort Exec.Demo.s)⟩ := by
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

/-- Helper (computation). A completed concrete run cannot have a pending request. This is
bookkeeping for the reporter, not blocking-policy correctness. (approved: PENDING) -/
theorem permitted (values : List Int) (history : List Int) :
    ExecAdapter.BlockingPermitted program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env (3 * values.length + 1) history (initial values)) := by
  intro c p vs site store k q ending pending typed
  rw [run_writes] at ending
  cases ending

/-- Law (subject `ConcreteSpec.Core.Observe.OutputDemo.adapter`).
Physical logging output is retained in order before an actual trap. Both the emitted history
and the exact rendered diagnostic are independently expected, rather than expressed through the
adapter's projection. An output-erasing adapter violates this result. -/
theorem output_then_trap (values : List Int) (history : List Int) :
    reportObservation program adapter Demo.contract ()
      (Exec.runFrom program Exec.Demo.env (3 * values.length + 1) history (initial values))
      (permitted values history) =
      some (.terminal ⟨(history ++ values).map (fun n => .output (toString n)),
        .trapped .abort Exec.Demo.s (sourceSpan "src/main.con" 1 (by decide) "a\té")
          "concrete: trap: abort at src/main.con:1:5 (site 0)"⟩) := by
  have executed := run_writes values history
  simp only [reportObservation, executed]
  change (some (FullObservation.terminal ⟨(history ++ values).flatMap (fun n => [Event.output (S := Demo.signature) (toString n)]),
    .trapped .abort Exec.Demo.s (sourceSpan "src/main.con" 1 (by decide) "a\té")
      "concrete: trap: abort at src/main.con:1:5 (site 0)"⟩) : Option (FullObservation Demo.signature)) = _
  congr 3
  generalize history ++ values = all
  induction all with
  | nil => rfl
  | cons n ns ih =>
    simpa only [List.flatMap_cons, List.map_cons, List.singleton_append] using
      congrArg (List.cons (Event.output (S := Demo.signature) (toString n))) ih

/-- Instance (law `ConcreteSpec.Core.Observe.project_physical_outputs`, case `ordered_physical_outputs`). -/
example : publicOutputs (adapter.project [11, 3, 4]) =
    ([11, 3, 4] : List Int).flatMap (fun n => [toString n]) :=
  project_physical_outputs adapter (fun n => [toString n]) (fun _ => rfl) [11, 3, 4]

/-- Planted (law `ConcreteSpec.Core.Observe.project_physical_outputs`, case `ordered_physical_outputs`,
subject `ConcreteSpec.Core.Observe.ExecAdapter.project`, mutant `ConcreteSpec.Core.Observe.eraseProjection`). -/
example : ExecAdapter.OutputCorrespondence adapter (fun n => [toString n]) ∧
    ¬ (publicOutputs (eraseProjection adapter [11, 3, 4]) =
      ([11, 3, 4] : List Int).flatMap (fun n => [toString n])) := by
  exact ⟨fun _ => rfl, by decide⟩

/-- Fault: an adapter drops every physical output while retaining request and trap projection. -/
def erasingAdapter : ExecAdapter Exec.Demo.ops Demo.signature :=
  { adapter with events := fun _ => [] }

/-- Refused: erasing physical output violates the independently supplied correspondence
obligation, even though an empty projected history could pass request/response trace checks. -/
example : ¬ ExecAdapter.OutputCorrespondence erasingAdapter (fun n => [toString n]) := by
  intro correspondence
  have output := correspondence 3
  change [] = ["3"] at output
  cases output

/-- Instance (law `ConcreteSpec.Core.Observe.OutputDemo.output_then_trap`, case `two_outputs_then_trap`). -/
example : reportObservation program adapter Demo.contract ()
    (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [11] (initial [3, 4]))
    (permitted [3, 4] [11]) =
    some (.terminal ⟨[.output "11", .output "3", .output "4"],
      .trapped .abort Exec.Demo.s (sourceSpan "src/main.con" 1 (by decide) "a\té")
        "concrete: trap: abort at src/main.con:1:5 (site 0)"⟩) :=
  output_then_trap [3, 4] [11]

/-- Planted (law `ConcreteSpec.Core.Observe.OutputDemo.output_then_trap`, case `two_outputs_then_trap`,
subject `ConcreteSpec.Core.Observe.OutputDemo.adapter`, mutant `ConcreteSpec.Core.Observe.OutputDemo.erasingAdapter`). -/
example : ¬ (reportObservation program erasingAdapter Demo.contract ()
    (Exec.runFrom program Exec.Demo.env (3 * [3, 4].length + 1) [11] (initial [3, 4]))
    (permitted [3, 4] [11]) =
    some (.terminal ⟨[.output "11", .output "3", .output "4"],
      .trapped .abort Exec.Demo.s (sourceSpan "src/main.con" 1 (by decide) "a\té")
        "concrete: trap: abort at src/main.con:1:5 (site 0)"⟩)) := by
  intro equal
  have lengths := congrArg (fun r : Option (FullObservation Demo.signature) =>
    match r with | some (.terminal obs) => obs.events.length | _ => 0) equal
  change 0 = 3 at lengths
  contradiction

end OutputDemo
end ConcreteSpec.Core.Observe
