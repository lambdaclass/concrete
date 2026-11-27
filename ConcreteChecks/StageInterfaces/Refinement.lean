import ConcreteChecks.StageInterfaces.Models

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

def terminalModel (A : Type) (observed : Observation) (runtime : Concrete.Contracts.Observation)
    (same : observed.SameRuntimeResult runtime) : Model A Nat where
  execution := {
    State := Unit
    initial := fun _ _ => True
    initialized := fun _ => ⟨(), trivial⟩
    step := fun _ _ _ => False
    result := fun _ _ o => o = runtime
    terminal := fun _ _ _ _ _ h => h
    safe := fun _ _ => True
    trace := fun _ _ => runtime.stderr.data.toList.map .stderr
    permitted := fun _ _ => True
    resultTrace := by
      intro _ _ result equals
      subst result
      generalize runtime.stderr.data.toList = bytes
      induction bytes with
      | nil => rfl
      | cons byte rest ih =>
        change byte :: Concrete.Contracts.stderrBytes (rest.map .stderr) = byte :: rest
        rw [ih]
    traceGrows := fun _ _ _ h => False.elim h
  }
  result := fun _ _ o => o = observed
  projects := fun _ o legacy => o = observed ∧ legacy = runtime
  terminalExact := by
    intro _ _ _
    exact ⟨fun h => ⟨_, rfl, rfl, h⟩, fun ⟨_, _, _, h⟩ => h⟩
  completeProjection := fun _ _ _ h => ⟨_, h, rfl⟩
  runtimeExact := by
    rintro _ _ _ ⟨rfl, rfl⟩
    exact same

def returnsStatus (A : Type) (status : UInt8) : Model A Nat :=
  terminalModel A (.returned status ByteArray.empty) (.returned status ByteArray.empty) ⟨rfl, rfl⟩

theorem terminalRefines {A B : Type} {a : A} {b : B} {o runtime}
    {same : o.SameRuntimeResult runtime} :
    (terminalModel A o runtime same).Refines (terminalModel B o runtime same) a b := by
  intro _
  exact ⟨fun _ _ _ h => ⟨(), ⟨(), trivial, .nil ()⟩, h, rfl⟩,
    fun _ h => (h.2 0).elim, fun _ h => h.2.2 runtime rfl⟩

theorem reachableTerminal {A E : Type} (s : Concrete.Contracts.Semantics A E)
    (a : A) (entry : s.State) (o : Concrete.Contracts.Observation)
    (initial : ∀ p, s.initial a p → p = entry) (terminal : s.result a entry o)
    (p : s.State) (reach : s.finite a p) : p = entry := by
  obtain ⟨start, starts, path⟩ := reach
  have atEntry := initial start starts
  subst start
  cases path with
  | nil => rfl
  | cons tick _ => exact (s.terminal a entry o terminal _ tick).elim

theorem stationaryRefines {A B E O : Type} {a : A} {b : B}
    (s : Concrete.Contracts.Semantics A E) (t : Concrete.Contracts.Semantics B E)
    (sr : A → s.State → O → Prop) (tr : B → t.State → O → Prop)
    (p : s.State) (q : t.State) (terminal : Concrete.Contracts.Observation) (raw : O)
    (sInitial : ∀ x, s.initial a x ↔ x = p) (tInitial : ∀ x, t.initial b x ↔ x = q)
    (_sResult : s.result a p terminal) (tResult : t.result b q terminal)
    (sRaw : sr a p raw) (tRaw : ∀ o, tr b q o ↔ o = raw)
    (sameTrace : s.trace a p = t.trace b q) :
    BehaviorRefinement s t sr tr a b := by
  have ps : s.finite a p := ⟨p, (sInitial p).mpr rfl, .nil _⟩
  have onlyT := reachableTerminal t b q terminal (fun x => (tInitial x).mp) tResult
  intro _
  refine ⟨?_, ?_, ?_⟩
  · intro x o reach result
    obtain rfl := onlyT x reach
    obtain rfl := (tRaw o).mp result
    exact ⟨p, ps, sRaw, sameTrace⟩
  · intro run infinite
    exact (t.terminal b q terminal tResult _
      ((tInitial _).mp infinite.1 ▸ infinite.2 0)).elim
  · intro x stuck
    obtain rfl := onlyT x stuck.1
    exact stuck.2.2 terminal tResult

theorem stationarySafe {A E : Type} (s : Concrete.Contracts.Semantics A E)
    (a : A) (q : s.State) (o : Concrete.Contracts.Observation)
    (initial : ∀ p, s.initial a p → p = q) (result : s.result a q o) :
    ProgramSafe s a := by
  intro state stuck
  obtain rfl := reachableTerminal s a q o initial result state stuck.1
  exact stuck.2.2 o result

theorem stationaryRejects {A B E : Type} {a : A} {b : B}
    (s : Concrete.Contracts.Semantics A E) (t : Concrete.Contracts.Semantics B E)
    (p : s.State) (q : t.State) (expected actual : Concrete.Contracts.Observation)
    (initial : ∀ x, s.initial a x → x = p) (sResult : s.result a p expected)
    (tInitial : t.initial b q) (tResult : t.result b q actual)
    (wrong : ¬ s.result a p actual) :
    ¬ BehaviorRefinement s t s.result t.result a b := by
  intro preserves
  obtain ⟨state, finite, observation, _⟩ :=
    (preserves (stationarySafe s a p expected initial sResult)).1 q actual
      ⟨q, tInitial, .nil _⟩ tResult
  obtain rfl := reachableTerminal s a p expected initial sResult state finite
  exact wrong observation

theorem refinesToTerminal {A B E : Type} {a : A} {b : B}
    (s : Concrete.Contracts.Semantics A E) (t : Concrete.Contracts.Semantics B E)
    (p : s.State) (q : t.State) (result : Concrete.Contracts.Observation)
    (reached : s.finite a p) (sResult : s.result a p result) (tResult : t.result b q result)
    (tInitial : ∀ x, t.initial b x → x = q)
    (onlyResult : ∀ o, t.result b q o → o = result)
    (trace : s.trace a p = t.trace b q) :
    BehaviorRefinement s t s.result t.result a b := by
  have onlyT := reachableTerminal t b q result tInitial tResult
  intro _
  refine ⟨?_, ?_, ?_⟩
  · intro x o path observed
    obtain rfl := onlyT x path
    obtain rfl := onlyResult o observed
    exact ⟨p, reached, sResult, trace⟩
  · intro run infinite
    exact (t.terminal b q result tResult _ (tInitial _ infinite.1 ▸ infinite.2 0)).elim
  · intro x stuck
    obtain rfl := onlyT x stuck.1
    exact stuck.2.2 result tResult

-- The finite phase analysis below certifies the registered executions. It
-- covers every reachable machine state, including every legal allocation.
theorem reachedPhase {A E P : Type} (exec : Concrete.Contracts.Semantics A E)
    (input : A) (state : P → exec.State) (start : P)
    (initial : ∀ s, exec.initial input s → state start = s)
    (closed : ∀ p s, exec.step input (state p) s → ∃ q, state q = s)
    {s : exec.State} (finite : exec.finite input s) : ∃ p, state p = s := by
  obtain ⟨first, begins, steps⟩ := finite
  have fromPhase {a b : exec.State} (path : Concrete.Contracts.Steps (exec.step input) a b) :
      (∃ p, state p = a) → ∃ p, state p = b := by
    induction path with
    | nil => exact id
    | cons step _ ih =>
      rintro ⟨p, rfl⟩
      exact ih (closed p _ step)
  exact fromPhase steps ⟨start, initial first begins⟩

theorem finiteProgressRefines {A B E : Type} (source : Concrete.Contracts.Semantics A E)
    (target : Concrete.Contracts.Semantics B E) (input : A) (output : B)
    (result : Concrete.Contracts.Observation)
    (p : source.State) (pReach : source.finite input p) (pResult : source.result input p result)
    (progress : ∀ q, target.finite output q → (∃ q', target.step output q q') ∨ target.result output q result)
    (exactResult : ∀ q o, target.finite output q → target.result output q o →
      o = result ∧ source.trace input p = target.trace output q)
    (rank : target.State → Nat)
    (decrease : ∀ q q', target.finite output q → target.step output q q' → rank q' < rank q) :
    BehaviorRefinement source target source.result target.result input output := by
  intro _
  refine ⟨?_, ?_, ?_⟩
  · intro q o finite observes
    obtain ⟨rfl, trace⟩ := exactResult q o finite observes
    exact ⟨p, pReach, pResult, trace⟩
  · intro run diverges
    have extend {a b c : target.State}
        (path : Concrete.Contracts.Steps (target.step output) a b)
        (tick : target.step output b c) : Concrete.Contracts.Steps (target.step output) a c := by
      induction path with
      | nil => exact .cons tick (.nil _)
      | cons first _ ih => exact .cons first (ih tick)
    have reach (i : Nat) : target.finite output (run i) := by
      induction i with
      | zero => exact ⟨_, diverges.1, .nil _⟩
      | succ i ih =>
        obtain ⟨entry, begins, path⟩ := ih
        exact ⟨entry, begins, extend path (diverges.2 i)⟩
    have bound (i : Nat) : rank (run i) + i ≤ rank (run 0) := by
      induction i with
      | zero => simp
      | succ i ih =>
        have strictly := decrease _ _ (reach i) (diverges.2 i)
        omega
    have impossible := bound (rank (run 0) + 1)
    omega
  · intro q stuck
    rcases progress q stuck.1 with ⟨next, tick⟩ | terminal
    · exact stuck.2.1 next tick
    · exact stuck.2.2 result terminal

end StageInterfaceControls
