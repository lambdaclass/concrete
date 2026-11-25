import Concrete.BIR
import ConcreteSpec.Spec.Surface0
import ConcreteSpec.Spec.Outcome

namespace Concrete.Contracts

/-- The diagnostic identity carried by the runtime. Source byte ranges belong to provenance. -/
structure DiagnosticSite where
  id : BIR.SiteId
  location : BIR.Loc
  deriving DecidableEq

/-- Terminal observations preserve the distinction between normal exit and a runtime trap. -/
inductive Observation where
  | returned (status : UInt8) (stderr : ByteArray)
  | trapped (status : UInt8) (kind : ConcreteSpec.Core.TrapKind) (site : DiagnosticSite)
      (stderr : ByteArray)
  | startupRefused (status : UInt8) (stderr : ByteArray)

/-- The slice-0 visible event alphabet. Extend this at the observation boundary as effects grow. -/
inductive Event where
  | stderr (byte : UInt8)
  deriving DecidableEq

/-- Reflexive transitive closure of a small-step relation, including the empty path. -/
inductive Steps {S : Type} (step : S → S → Prop) : S → S → Prop where
  | nil (s : S) : Steps step s s
  | cons {a b c : S} : step a b → Steps step b c → Steps step a c

/-- All execution judgments use the same initial states and transitions. States carry the
cumulative visible trace. Terminal states have no successors. -/
structure Semantics (A : Type) where
  State : Type
  initial : A → State → Prop
  initialized : ∀ a, ∃ q, initial a q
  step : A → State → State → Prop
  result : A → State → Observation → Prop
  terminal : ∀ a q o, result a q o → ∀ q', ¬ step a q q'
  safe : A → State → Prop
  trace : A → State → List Event
  traceGrows : ∀ a q q', step a q q' → ∃ suffix, trace a q ++ suffix = trace a q'

def Semantics.onInput {A B : Type} (s : Semantics A) (f : B → A) : Semantics B where
  State := s.State
  initial b := s.initial (f b)
  initialized b := s.initialized (f b)
  step b := s.step (f b)
  result b := s.result (f b)
  terminal b := s.terminal (f b)
  safe b := s.safe (f b)
  trace b := s.trace (f b)
  traceGrows b := s.traceGrows (f b)

def Semantics.finite {A : Type} (s : Semantics A) (a : A) (q : s.State) : Prop :=
  ∃ start, s.initial a start ∧ Steps (s.step a) start q

def Semantics.observes {A : Type} (s : Semantics A) (a : A) (o : Observation) : Prop :=
  ∃ q, s.finite a q ∧ s.result a q o

def Semantics.diverges {A : Type} (s : Semantics A) (a : A) (run : Nat → s.State) : Prop :=
  s.initial a (run 0) ∧ ∀ i, s.step a (run i) (run (i + 1))

def Semantics.stuck {A : Type} (s : Semantics A) (a : A) (q : s.State) : Prop :=
  s.finite a q ∧ (∀ next, ¬ s.step a q next) ∧ (∀ o, ¬ s.result a q o)

def TracePrefix (left right : List Event) : Prop := ∃ suffix, left ++ suffix = right

/-- Equality of the projected traces of two infinite executions, allowing different timings. -/
def SameInfiniteTrace (left right : Nat → List Event) : Prop :=
  ∀ events, (∃ i, TracePrefix events (left i)) ↔ (∃ j, TracePrefix events (right j))

def PrefixSafety {A B : Type} (s : Semantics A) (t : Semantics B) (a : A) (b : B) : Prop :=
  ∀ q, t.finite b q → t.safe b q ∧
    ∃ p, s.finite a p ∧ s.safe a p ∧ TracePrefix (t.trace b q) (s.trace a p)

def MaximalRefinement {A B : Type} (s : Semantics A) (t : Semantics B) (a : A) (b : B) : Prop :=
  (∀ o, t.observes b o → s.observes a o) ∧
  (∀ run, t.diverges b run → ∃ source, s.diverges a source ∧
    SameInfiniteTrace (fun i => s.trace a (source i)) (fun i => t.trace b (run i))) ∧
  (∀ q, ¬ t.stuck b q)

def Refines {A B : Type} (s : Semantics A) (t : Semantics B) (a : A) (b : B) : Prop :=
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
structure Simulates {A B : Type} (s : Semantics A) (t : Semantics B) (a : A) (b : B) where
  simulation : StutteringSimulation (s.step a) (t.step b)
  initialMatch : ∀ q, t.initial b q → ∃ p, s.initial a p ∧ simulation.stateRel p q
  prefixMatch : ∀ p q, s.finite a p → t.finite b q → simulation.stateRel p q →
    s.safe a p ∧ t.safe b q ∧ TracePrefix (t.trace b q) (s.trace a p)
  resultMatch : ∀ p q o, s.finite a p → t.finite b q → simulation.stateRel p q →
    t.result b q o → ∃ p', Steps (s.step a) p p' ∧ s.result a p' o
  progress : ∀ q, t.finite b q → (∃ q', t.step b q q') ∨ (∃ o, t.result b q o)
  divergentTrace : ∀ source target, s.diverges a source → t.diverges b target →
    (∃ align : Nat → Nat, (∀ i, ∃ j, i ≤ align j) ∧
      ∀ j, simulation.stateRel (source (align j)) (target j)) →
    SameInfiniteTrace (fun i => s.trace a (source i)) (fun i => t.trace b (target i))

structure LoweringContract {A B : Type} (runs : A → B → Prop)
    (source : Semantics A) (target : Semantics B) where
  kind : EvidenceKind
  inputInvariant : A → Prop
  outputInvariant : B → Prop

/-- The registry proves simulation on the exact successful invocation. Refinement is its
semantic consequence, with an explicit divergence and trace obligation. -/
def LoweringContract.sound {A B : Type} {runs : A → B → Prop} {s : Semantics A} {t : Semantics B}
    (c : LoweringContract runs s t) : Prop :=
  ∀ a b, runs a b → c.inputInvariant a → c.outputInvariant b ∧ Nonempty (Simulates s t a b)

/-- Fuel exhaustion describes a prefix. Its universal persistence characterizes divergence. -/
inductive Evaluation where
  | running
  | finished (o : Observation)
  | stuck

def Observation.events : Observation → List Event
  | .returned _ stderr | .trapped _ _ _ stderr | .startupRefused _ stderr =>
    stderr.data.toList.map Event.stderr

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
  traceGrows a q q' h := by
    refine ⟨(evaluate a q').events, ?_⟩
    rw [h.1]
    rfl

def EvaluatorAdequacy {A : Type} (s : Semantics A)
    (evaluate : A → Nat → Evaluation) : Prop :=
  (∀ a events, (∃ q, s.finite a q ∧ s.trace a q = events) ↔
    (∃ q, (evaluatorSemantics evaluate).finite a q ∧ (evaluate a q).events = events)) ∧
  (∀ a o, s.observes a o ↔ ∃ fuel, evaluate a fuel = .finished o) ∧
  (∀ a, (∃ q, s.stuck a q) ↔ ∃ fuel, evaluate a fuel = .stuck) ∧
  (∀ a, (∃ run, s.diverges a run) ↔ ∀ fuel, evaluate a fuel = .running)

end Concrete.Contracts
