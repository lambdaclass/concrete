import Concrete.Contracts
import Concrete.Backend.LCB
import ConcreteSpec.Spec.Observation

namespace Concrete.Backend

open Contracts
open LeanCompilerBackend.Verified

/-- Source byte offsets are erased at this boundary. The runtime prints the diagnostic ID,
path, line and column. -/
def trapObservation (entry : BIR.SiteEntry) : Observation :=
  let kind := match entry.kind with | .arithmeticOverflow => ConcreteSpec.Core.TrapKind.overflow
  let status := ConcreteSpec.Observe.Observation.status (.trapped entry.kind)
  let loc := entry.loc
  let line := s!"concrete: trap: {kind.name} at {loc.path}:{loc.line}:{loc.col} (site {entry.site.id})\n"
  .trapped (UInt8.ofNat status) kind ⟨entry.site, loc⟩ line.toUTF8

def birObservation (sites : BIR.SiteTable) : BIR.Outcome → Option Observation
  | .exited status =>
    if status < 256 then some (.returned (UInt8.ofNat status) ByteArray.empty) else none
  | .trapped index => sites[index]?.map trapObservation
  | .stuck | .outOfFuel => none

def birEvaluation (p : BIR.Program) (fuel : Nat) : Evaluation :=
  match BIR.run p fuel with
  | .outOfFuel => .running
  | outcome => match birObservation p.sites outcome with
    | some o => .finished o
    | none => .stuck

/-- BIR has no external effects while the body runs. These are its exact block-budget
prefixes, with the terminal result projected by the slice-0 runtime shim. -/
def birSemantics : Semantics BIR.Program :=
  evaluatorSemantics birEvaluation

abbrev LCBInput := Source.Function × BIR.SiteTable × ByteArray

/-- The ABI projection of the returned word, including the diagnostic looked up by its index. -/
def lcbResult (sites : BIR.SiteTable) : Source.Value → Option Observation
  | .word word =>
    if word.toNat < 256 then birObservation sites (.exited word.toNat)
    else birObservation sites (.trapped (word.toNat - 256))
  | _ => none

def lcbTerminal (i : LCBInput) (q : Option Source.State) : Option Observation := do
  let state ← q
  match Source.step i.1 state with
  | .ok (.returned value) => lcbResult i.2.1 value
  | _ => none

/-- Imported initial states and small steps determine prefixes, returns, faults and divergence.
The body is silent and the runtime projection supplies the terminal diagnostic. -/
def lcbSemantics : Semantics LCBInput where
  State := Option Source.State
  initial i q := q = (Source.initial i.1 #[]).toOption
  initialized _ := ⟨_, rfl⟩
  step i q q' := ∃ s t, q = some s ∧ q' = some t ∧ Source.Step i.1 s t
  result i q o := lcbTerminal i q = some o
  terminal _ _ _ := by
    rintro h q' ⟨s, t, rfl, _, hstep⟩
    unfold Source.Step at hstep
    simp [lcbTerminal, hstep] at h
  safe i q := (∃ q', ∃ s t, q = some s ∧ q' = some t ∧ Source.Step i.1 s t) ∨
    (lcbTerminal i q).isSome = true
  trace i q := ((lcbTerminal i q).map Observation.events).getD []
  permitted _ := runtimePermitted
  resultTrace i q o h := by
    simp only [h, Option.map_some, Option.getD_some]
    exact o.stderr_events
  traceGrows i q q' := by
    rintro ⟨s, t, rfl, rfl, hstep⟩
    refine ⟨((lcbTerminal i (some t)).map Observation.events).getD [], ?_⟩
    unfold Source.Step at hstep
    rw [show lcbTerminal i (some s) = none by simp [lcbTerminal, hstep]]
    rfl

end Concrete.Backend
