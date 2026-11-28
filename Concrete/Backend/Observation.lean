import Concrete.Contracts
import LeanCompilerBackend.Verified.Source

namespace Concrete.Backend

open Contracts
open LeanCompilerBackend.Verified

/-- Project the actual imported return through the runtime ABI for this input. -/
def lcbTerminalFor {A : Type} (body : A → Source.Function)
    (project : A → Source.Value → Option Observation) (input : A)
    (q : Option Source.State) : Option Observation := do
  let state ← q
  match Source.step (body input) state with
  | .ok (.returned value) => project input value
  | _ => none

/-- Imported initial states and small steps determine prefixes, returns, faults and divergence.
The body is silent and the runtime projection supplies the terminal diagnostic. -/
def lcbInvocationSemantics {A E : Type} (body : A → Source.Function)
    (arguments : A → Array Source.Value)
    (project : A → Source.Value → Option Observation) : Semantics A E where
  State := Option Source.State
  initial i q := q = (Source.initial (body i) (arguments i)).toOption
  initialized _ := ⟨_, rfl⟩
  step i q q' := ∃ s t, q = some s ∧ q' = some t ∧ Source.Step (body i) s t
  result i q o := lcbTerminalFor body project i q = some o
  terminal _ _ _ := by
    rintro h q' ⟨s, t, rfl, _, hstep⟩
    unfold Source.Step at hstep
    simp [lcbTerminalFor, hstep] at h
  safe i q := (∃ q', ∃ s t, q = some s ∧ q' = some t ∧ Source.Step (body i) s t) ∨
    (lcbTerminalFor body project i q).isSome = true
  trace i q := ((lcbTerminalFor body project i q).map fun o => o.stderr.data.toList.map ProcessEvent.stderr).getD []
  permitted _ := fun
    | .stderr _ => True
    | .effect _ => False
  resultTrace i q o h := by
    simp only [h, Option.map_some, Option.getD_some]
    generalize o.stderr.data.toList = bytes
    induction bytes with
    | nil => rfl
    | cons b rest ih =>
      change b :: stderrBytes (rest.map ProcessEvent.stderr) = b :: rest
      rw [ih]
  traceGrows i q q' := by
    rintro ⟨s, t, rfl, rfl, hstep⟩
    refine ⟨((lcbTerminalFor body project i (some t)).map fun o => o.stderr.data.toList.map ProcessEvent.stderr).getD [], ?_⟩
    unfold Source.Step at hstep
    rw [show lcbTerminalFor body project i (some s) = none by simp [lcbTerminalFor, hstep]]
    rfl

def lcbSemanticsFor {A E : Type} (body : A → Source.Function)
    (project : A → Source.Value → Option Observation) : Semantics A E :=
  lcbInvocationSemantics body (fun _ => #[]) project

end Concrete.Backend
