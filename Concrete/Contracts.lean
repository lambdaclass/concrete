import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome

namespace Concrete.Contracts

/-- Source path and 1-based line and column reported by the runtime diagnostic boundary. -/
structure DiagnosticLocation where
  path : String
  line : Nat
  col : Nat
  deriving DecidableEq, Repr

/-- The diagnostic identity carried by the runtime. Source byte ranges belong to provenance. -/
structure DiagnosticSite where
  id : ConcreteSpec.Core.SiteId
  location : DiagnosticLocation
  deriving DecidableEq, Repr

/-- Terminal observations preserve the distinction between normal exit and a runtime trap. -/
inductive Observation where
  | returned (status : UInt8) (stderr : ByteArray)
  | trapped (status : UInt8) (kind : ConcreteSpec.Core.TrapKind) (site : DiagnosticSite)
      (stderr : ByteArray)
  | startupRefused (status : UInt8) (stderr : ByteArray)
  /-- A physical lifetime could not reserve backing in any target address space. -/
  | resourceExhausted

/-- Stderr is observable on every profile. The extension carries additional
events, including the grant and operation information used by its authority judgment. -/
inductive ProcessEvent (Extension : Type) where
  | stderr (byte : UInt8)
  | effect (event : Extension)

abbrev Event := ProcessEvent Empty

def Observation.stderr : Observation → ByteArray
  | .returned _ bytes | .trapped _ _ _ bytes | .startupRefused _ bytes => bytes
  | .resourceExhausted => ByteArray.empty

def stderrBytes {E : Type} (events : List (ProcessEvent E)) : List UInt8 :=
  events.filterMap fun
    | .stderr byte => some byte
    | .effect _ => none

/-- Reflexive transitive closure of a small-step relation, including the empty path. -/
inductive Steps {S : Type} (step : S → S → Prop) : S → S → Prop where
  | nil (s : S) : Steps step s s
  | cons {a b c : S} : step a b → Steps step b c → Steps step a c

/-- All execution judgments use the same initial states and transitions. States carry the
cumulative visible trace. Terminal states have no successors. -/
structure Semantics (A : Type) (E : Type := Empty) where
  State : Type
  initial : A → State → Prop
  initialized : ∀ a, ∃ q, initial a q
  step : A → State → State → Prop
  result : A → State → Observation → Prop
  terminal : ∀ a q o, result a q o → ∀ q', ¬ step a q q'
  safe : A → State → Prop
  trace : A → State → List (ProcessEvent E)
  permitted : A → ProcessEvent E → Prop
  resultTrace : ∀ a q o, result a q o → stderrBytes (trace a q) = o.stderr.data.toList
  traceGrows : ∀ a q q', step a q q' → ∃ suffix, trace a q ++ suffix = trace a q'

def Semantics.onInput {A B E : Type} (s : Semantics A E) (f : B → A) : Semantics B E where
  State := s.State
  initial b := s.initial (f b)
  initialized b := s.initialized (f b)
  step b := s.step (f b)
  result b := s.result (f b)
  terminal b := s.terminal (f b)
  safe b := s.safe (f b)
  trace b := s.trace (f b)
  permitted b := s.permitted (f b)
  resultTrace b := s.resultTrace (f b)
  traceGrows b := s.traceGrows (f b)

def Semantics.finite {A E : Type} (s : Semantics A E) (a : A) (q : s.State) : Prop :=
  ∃ start, s.initial a start ∧ Steps (s.step a) start q

def Semantics.observes {A E : Type} (s : Semantics A E) (a : A) (o : Observation) : Prop :=
  ∃ q, s.finite a q ∧ s.result a q o

def Semantics.diverges {A E : Type} (s : Semantics A E) (a : A) (run : Nat → s.State) : Prop :=
  s.initial a (run 0) ∧ ∀ i, s.step a (run i) (run (i + 1))

def Semantics.stuck {A E : Type} (s : Semantics A E) (a : A) (q : s.State) : Prop :=
  s.finite a q ∧ (∀ next, ¬ s.step a q next) ∧ (∀ o, ¬ s.result a q o)

def TracePrefix {E : Type} (left right : List E) : Prop := ∃ suffix, left ++ suffix = right

/-- Equality of the projected traces of two infinite executions, allowing different timings. -/
def SameInfiniteTrace {E : Type} (left right : Nat → List E) : Prop :=
  ∀ events, (∃ i, TracePrefix events (left i)) ↔ (∃ j, TracePrefix events (right j))

def PrefixSafety {A B E : Type} (s : Semantics A E) (t : Semantics B E) (a : A) (b : B) : Prop :=
  ∀ q, t.finite b q → t.safe b q ∧
    (∀ e ∈ t.trace b q, s.permitted a e ∧ t.permitted b e) ∧
    ∃ p, s.finite a p ∧ s.safe a p ∧ TracePrefix (t.trace b q) (s.trace a p)

def MaximalRefinement {A B E : Type} (s : Semantics A E) (t : Semantics B E) (a : A) (b : B) : Prop :=
  (∀ q o, t.finite b q → t.result b q o →
    ∃ p, s.finite a p ∧ s.result a p o ∧ s.trace a p = t.trace b q) ∧
  (∀ run, t.diverges b run → ∃ source, s.diverges a source ∧
    SameInfiniteTrace (fun i => s.trace a (source i)) (fun i => t.trace b (run i))) ∧
  (∀ q, ¬ t.stuck b q)

def Refines {A B E : Type} (s : Semantics A E) (t : Semantics B E) (a : A) (b : B) : Prop :=
  PrefixSafety s t a b ∧ MaximalRefinement s t a b

/-- Evidence used to discharge a stage obligation. A named premise remains visible until replaced
by direct proof or a validator soundness theorem. -/
inductive EvidenceKind where
  | directProof
  | provedValidator
  | namedPremise (name : String)

/-- The common contract for a production edge. The relation is supplied by the caller, typically
the graph of the actual function. The correspondence can be a declarative judgment or refinement. -/
structure EdgeContract {A B : Type} (runs : A → B → Prop) where
  kind : EvidenceKind
  inputInvariant : A → Prop
  outputInvariant : B → Prop
  correspondence : A → B → Prop

/-- A successful invocation on an input satisfying its invariant establishes both obligations. -/
def EdgeContract.sound {A B : Type} {runs : A → B → Prop} (c : EdgeContract runs) : Prop :=
  ∀ a b, runs a b → c.inputInvariant a → c.outputInvariant b ∧ c.correspondence a b

/-- The invariant established by one edge suffices for the next edge. -/
def Compatible {A B C : Type} {run₁ : A → B → Prop} {run₂ : B → C → Prop}
    (first : EdgeContract run₁) (second : EdgeContract run₂) : Prop :=
  ∀ b, first.outputInvariant b → second.inputInvariant b

/-- The success graph of the actual executable function. -/
def Graph {A B E : Type} (f : A → Except E B) (a : A) (b : B) : Prop :=
  f a = .ok b

/-- The graph of a total pass. -/
def TotalGraph {A B : Type} (f : A → B) (a : A) (b : B) : Prop :=
  f a = b

/-- Simulation obligations for one lowering on exact input and output. A target step is matched
by a nonempty source path or stutters while a natural-valued measure strictly decreases.
Initialization and observation matching are supplied by the enclosing simulation. -/
structure StutteringSimulation {S T : Type} (source : S → S → Prop) (target : T → T → Prop) where
  stateRel : S → T → Prop
  measure : S → T → Nat
  progress : ∀ s t t', stateRel s t → target t t' →
    (∃ next s', source s next ∧ Steps source next s' ∧ stateRel s' t') ∨
    (stateRel s t' ∧ measure s t' < measure s t)

/-- Simulation of the same states that define reachability and maximal behavior.
Divergent trace reflection is explicit: forward prefix inclusion alone permits loss of events. -/
structure Simulates {A B E : Type} (s : Semantics A E) (t : Semantics B E) (a : A) (b : B) where
  simulation : StutteringSimulation (s.step a) (t.step b)
  initialMatch : ∀ q, t.initial b q → ∃ p, s.initial a p ∧ simulation.stateRel p q
  prefixMatch : ∀ p q, s.finite a p → t.finite b q → simulation.stateRel p q →
    s.safe a p ∧ t.safe b q ∧
    (∀ e ∈ t.trace b q, s.permitted a e ∧ t.permitted b e) ∧ TracePrefix (t.trace b q) (s.trace a p)
  resultMatch : ∀ p q o, s.finite a p → t.finite b q → simulation.stateRel p q →
    t.result b q o → ∃ p', Steps (s.step a) p p' ∧ s.result a p' o ∧ s.trace a p' = t.trace b q
  progress : ∀ q, t.finite b q → (∃ q', t.step b q q') ∨ (∃ o, t.result b q o)
  divergentTrace : ∀ target, t.diverges b target →
    ∃ source, s.diverges a source ∧
      SameInfiniteTrace (fun i => s.trace a (source i)) (fun i => t.trace b (target i))

private theorem Steps.append {S : Type} {step : S → S → Prop} {a b c : S}
    (left : Steps step a b) (right : Steps step b c) : Steps step a c := by
  induction left with
  | nil => exact right
  | cons h _ ih => exact .cons h (ih right)

theorem Simulates.refines {A B E : Type} {s : Semantics A E} {t : Semantics B E}
    {a : A} {b : B} (sim : Simulates s t a b) : Refines s t a b := by
  have matchReachable (q : t.State) (h : t.finite b q) :
      ∃ p, s.finite a p ∧ sim.simulation.stateRel p q := by
    obtain ⟨q₀, initial, path⟩ := h
    obtain ⟨p₀, starts, related⟩ := sim.initialMatch q₀ initial
    have follow {q₁ q₂ : t.State} (steps : Steps (t.step b) q₁ q₂) :
        ∀ p, s.finite a p → sim.simulation.stateRel p q₁ →
          ∃ p', s.finite a p' ∧ sim.simulation.stateRel p' q₂ := by
      induction steps with
      | nil => exact fun p reachable matched => ⟨p, reachable, matched⟩
      | cons tick _ ih =>
        intro p reachable matched
        rcases sim.simulation.progress _ _ _ matched tick with advance | pause
        · obtain ⟨_, p', tick, steps, matched⟩ := advance
          obtain ⟨start, initial, history⟩ := reachable
          exact ih p' ⟨start, initial, history.append (.cons tick steps)⟩ matched
        · exact ih p reachable pause.1
    exact follow path p₀ ⟨p₀, starts, .nil p₀⟩ related
  refine ⟨?_, ?_, sim.divergentTrace, ?_⟩
  · intro q reachable
    obtain ⟨p, sourceReachable, related⟩ := matchReachable q reachable
    obtain ⟨sourceSafe, targetSafe, authority, trace⟩ :=
      sim.prefixMatch p q sourceReachable reachable related
    exact ⟨targetSafe, authority, p, sourceReachable, sourceSafe, trace⟩
  · intro q o reachable result
    obtain ⟨p, sourceReachable, related⟩ := matchReachable q reachable
    obtain ⟨p', tail, outcome, trace⟩ := sim.resultMatch p q o sourceReachable reachable related result
    obtain ⟨start, initial, history⟩ := sourceReachable
    exact ⟨p', ⟨start, initial, history.append tail⟩, outcome, trace⟩
  · intro q stuck
    rcases sim.progress q stuck.1 with ⟨next, h⟩ | ⟨o, h⟩
    · exact stuck.2.1 next h
    · exact stuck.2.2 o h

structure LoweringContract {A B E : Type} (runs : A → B → Prop)
    (source : Semantics A E) (target : Semantics B E) where
  kind : EvidenceKind
  inputInvariant : A → Prop
  outputInvariant : B → Prop

/-- The semantic correspondence of a lowering edge is simulation at its exact handoff. -/
def LoweringContract.edge {A B E : Type} {runs : A → B → Prop}
    {s : Semantics A E} {t : Semantics B E} (c : LoweringContract runs s t) : EdgeContract runs where
  kind := c.kind
  inputInvariant := c.inputInvariant
  outputInvariant := c.outputInvariant
  correspondence a b := Nonempty (Simulates s t a b)

def LoweringContract.sound {A B E : Type} {runs : A → B → Prop}
    {s : Semantics A E} {t : Semantics B E} (c : LoweringContract runs s t) : Prop :=
  c.edge.sound

/-- Fuel exhaustion describes a prefix. Its universal persistence characterizes divergence. -/
inductive Evaluation where
  | running
  | finished (o : Observation)
  | stuck

def Observation.events (o : Observation) : List Event :=
  o.stderr.data.toList.map ProcessEvent.stderr

theorem Observation.stderr_events (o : Observation) : stderrBytes o.events = o.stderr.data.toList := by
  unfold Observation.events
  generalize o.stderr.data.toList = bytes
  induction bytes with
  | nil => rfl
  | cons b rest ih => simp [stderrBytes] at ih ⊢; exact ih

/-- Slice 0 grants stderr to the runtime diagnostic boundary. -/
def runtimePermitted : Event → Prop
  | .stderr _ => True
  | .effect impossible => nomatch impossible

theorem runtimePermitted_all (event : Event) : runtimePermitted event := by
  cases event with
  | stderr => trivial
  | effect impossible => cases impossible

def Evaluation.events : Evaluation → List Event
  | .finished o => o.events
  | _ => []

/-- Exact prefixes of a deterministic evaluator. Exhaustion advances the budget by one.
This lift is for evaluators with no visible effects before their final outcome. -/
def evaluatorSemantics {A : Type} (evaluate : A → Nat → Evaluation) : Semantics A where
  State := Nat
  initial _ q := q = 0
  initialized _ := ⟨0, rfl⟩
  step a q q' := evaluate a q = .running ∧ q' = q + 1
  result a q o := evaluate a q = .finished o
  terminal _ _ _ h _ hstep := by rw [h] at hstep; exact Evaluation.noConfusion hstep.1
  safe a q := evaluate a q ≠ .stuck
  trace a q := (evaluate a q).events
  permitted _ := runtimePermitted
  resultTrace _ _ o h := by
    simp only [h, Evaluation.events]
    exact o.stderr_events
  traceGrows a q q' h := by
    refine ⟨(evaluate a q').events, ?_⟩
    rw [h.1]
    rfl

def EvaluatorAdequacyAt {A : Type} (s : Semantics A)
    (evaluate : A → Nat → Evaluation) (a : A) : Prop :=
  (∀ events, (∃ q, s.finite a q ∧ s.trace a q = events) ↔
    (∃ q, (evaluatorSemantics evaluate).finite a q ∧ (evaluate a q).events = events)) ∧
  (∀ o, s.observes a o ↔ (evaluatorSemantics evaluate).observes a o) ∧
  ((∃ q, s.stuck a q) ↔ ∃ q, (evaluatorSemantics evaluate).stuck a q) ∧
  ((∃ run, s.diverges a run) ↔ ∀ fuel, evaluate a fuel = .running)

def EvaluatorAdequacy {A : Type} (s : Semantics A)
    (evaluate : A → Nat → Evaluation) : Prop :=
  ∀ a, EvaluatorAdequacyAt s evaluate a

/-- Every infinite evaluator run must exhaust each of its successive fuel budgets. -/
theorem evaluatorDiverges {A : Type} {evaluate : A → Nat → Evaluation}
    {a : A} {run : Nat → Nat} (h : (evaluatorSemantics evaluate).diverges a run) :
    ∀ fuel, evaluate a fuel = .running := by
  have index (fuel : Nat) : run fuel = fuel := by
    induction fuel with
    | zero => exact h.1
    | succ n ih => exact (h.2 n).2.trans (congrArg (· + 1) ih)
  intro fuel
  exact index fuel ▸ (h.2 fuel).1

theorem evaluatorAdequacy {A : Type} (evaluate : A → Nat → Evaluation) :
    EvaluatorAdequacy (evaluatorSemantics evaluate) evaluate := by
  intro a
  refine ⟨fun _ => Iff.rfl, fun _ => Iff.rfl, Iff.rfl, ⟨?_, ?_⟩⟩
  · rintro ⟨run, h⟩
    exact evaluatorDiverges h
  · intro running
    exact ⟨id, rfl, fun n => ⟨running n, rfl⟩⟩

/-- Match evaluator clocks that advance together or have a ranked target-only tick. -/
def evaluatorSimulation {A B : Type} {left : A → Nat → Evaluation} {right : B → Nat → Evaluation}
    {a : A} {b : B} (align rank : Nat → Nat) (start : align 0 = 0)
    (agree : ∀ n, left a (align n) = right b n)
    (tick : ∀ n, align (n + 1) = align n + 1 ∨
      (align (n + 1) = align n ∧ rank (n + 1) < rank n))
    (cofinal : ∀ i, ∃ j, i ≤ align j)
    (neverStuck : ∀ n, right b n ≠ .stuck) :
    Simulates (evaluatorSemantics left) (evaluatorSemantics right) a b := by
  refine ⟨⟨fun p q => p = align q, fun _ q => rank q, ?_⟩, ?_, ?_, ?_, ?_, ?_⟩
  · intro p q q' eq h
    change Nat at q q'
    change right b q = .running ∧ q' = q + 1 at h
    obtain ⟨running, rfl⟩ := h
    change p = align q at eq
    subst p
    rcases tick q with advance | wait
    · exact Or.inl ⟨align (q + 1), align (q + 1), ⟨(agree q).trans running, advance⟩, .nil _, rfl⟩
    · exact Or.inr ⟨wait.1.symm, wait.2⟩
  · intro q h
    change Nat at q
    change q = 0 at h
    subst q
    exact ⟨(0 : Nat), rfl, start.symm⟩
  · intro p q _ _ eq
    change p = align q at eq
    subst p
    exact ⟨fun bad => neverStuck q ((agree q).symm.trans bad), neverStuck q,
      fun e _ => ⟨runtimePermitted_all e, runtimePermitted_all e⟩,
      [], by simp only [List.append_nil]; exact congrArg Evaluation.events (agree q).symm⟩
  · intro p q o _ _ eq result
    change p = align q at eq
    subst p
    exact ⟨align q, .nil _, (agree q).trans result, congrArg Evaluation.events (agree q)⟩
  · intro q _
    change Nat at q
    cases h : right b q with
    | running => exact Or.inl ⟨q + 1, h, rfl⟩
    | finished o => exact Or.inr ⟨o, h⟩
    | stuck => exact (neverStuck q h).elim
  · intro target ht
    have hit (j i : Nat) (le : i ≤ align j) : ∃ k, align k = i := by
      induction j with
      | zero => exact ⟨0, by omega⟩
      | succ j ih =>
        by_cases prev : i ≤ align j
        · exact ih prev
        · refine ⟨j + 1, ?_⟩
          rcases tick j with h | ⟨h, _⟩ <;> omega
    have hTarget : ∀ n, right b n = .running := evaluatorDiverges ht
    have hSource (i : Nat) : left a i = .running := by
      obtain ⟨j, bound⟩ := cofinal i
      obtain ⟨k, eq⟩ := hit j i bound
      rw [← eq, agree, hTarget]
    refine ⟨id, ⟨rfl, fun n => ⟨hSource n, rfl⟩⟩, ?_⟩
    have rightRunning (n : Nat) : right b (target n) = .running := (ht.2 n).1
    change SameInfiniteTrace (fun i => (left a i).events) (fun i => (right b (target i)).events)
    simp only [hSource, rightRunning]
    exact fun _ => Iff.rfl

end Concrete.Contracts
