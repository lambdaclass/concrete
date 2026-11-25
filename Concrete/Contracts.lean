import Concrete.BIR
import ConcreteSpec.Spec.Surface0
import ConcreteSpec.Spec.Outcome

namespace Concrete.Contracts

/-- Lexical identity in the exact input, with the location used for diagnostic rendering. -/
structure LexicalSite where
  span : ConcreteSpec.Surface0.Span
  location : BIR.Loc

/-- Terminal process observations. The constructors distinguish a trap from an ordinary
return of 134, even though their exit statuses can agree. -/
inductive Observation where
  | returned (status : UInt8) (stderr : ByteArray)
  | trapped (status : UInt8) (kind : ConcreteSpec.Core.TrapKind) (site : LexicalSite)
      (stderr : ByteArray)
  | startupRefused (status : UInt8) (stderr : ByteArray)

/-- A semantic interface over complete executions and their reachable finite prefixes.
An execution identity can later carry events and authority. The projection is the explicit
observation boundary. Divergence denotes an infinite execution; stuckness and exhausted
evaluation budgets confer neither classification. -/
structure Semantics (A : Type) where
  Execution : Type
  Prefix : Type
  finite : A → Prefix → Prop
  safe : Prefix → Prop
  prefixStderr : Prefix → List UInt8
  terminates : A → Execution → Prop
  diverges : A → Execution → Prop
  stuck : A → Execution → Prop
  project : Execution → Observation

/-- An observed terminal execution of this exact input. -/
def Semantics.observes {A : Type} (s : Semantics A) (a : A) (o : Observation) : Prop :=
  ∃ run, s.terminates a run ∧ s.project run = o

/-- Refinement of reachable finite prefixes independently of any maximal execution.
Each target prefix must be safe and its visible bytes must be a prefix of allowed source bytes. -/
def PrefixSafety {A B : Type} (s : Semantics A) (t : Semantics B) (a : A) (b : B) : Prop :=
  ∀ q, t.finite b q → t.safe q ∧
    ∃ p suffix, s.finite a p ∧ s.safe p ∧ t.prefixStderr q ++ suffix = s.prefixStderr p

/-- Every admitted maximal execution refines an allowed maximal execution. Termination preserves
the projected observation, and divergence requires source divergence. -/
def MaximalRefinement {A B : Type} (s : Semantics A) (t : Semantics B) (a : A) (b : B) : Prop :=
  (∀ o, t.observes b o → s.observes a o) ∧
  (∀ run, t.diverges b run → ∃ source, s.diverges a source) ∧
  (∀ run, ¬ t.stuck b run)

/-- Common semantic obligations at one successful lowering or tool handoff. -/
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

/-- Reflexive transitive closure of a small-step relation, including the empty path. -/
inductive Steps {S : Type} (step : S → S → Prop) : S → S → Prop where
  | nil (s : S) : Steps step s s
  | cons {a b c : S} : step a b → Steps step b c → Steps step a c

/-- Simulation obligations for one lowering on exact input and output. A target step is matched
by a nonempty source path or stutters while a natural-valued measure strictly decreases.
Initialization and observations are discharged by the edge's invariant and refinement obligations. -/
structure StutteringSimulation {S T : Type} (source : S → S → Prop) (target : T → T → Prop) where
  stateRel : S → T → Prop
  measure : S → T → Nat
  progress : ∀ s t t', stateRel s t → target t t' →
    (∃ next s', source s next ∧ Steps source next s' ∧ stateRel s' t') ∨
    (stateRel s t' ∧ measure s t' < measure s t)

/-- Uniform lowering contract with the simulation slot indexed by the exact pass invocation. -/
structure LoweringContract {A B : Type} (runs : A → B → Prop)
    (source : Semantics A) (target : Semantics B) where
  kind : EvidenceKind
  inputInvariant : A → Prop
  outputInvariant : B → Prop
  SourceState : Type
  TargetState : Type
  sourceStep : A → SourceState → SourceState → Prop
  targetStep : B → TargetState → TargetState → Prop
  sourceInitial : A → SourceState → Prop
  targetInitial : B → TargetState → Prop
  simulation : ∀ a b, runs a b → inputInvariant a →
    StutteringSimulation (sourceStep a) (targetStep b)
  initialMatch : ∀ a b (h : runs a b) (hi : inputInvariant a) t, targetInitial b t →
    ∃ s, sourceInitial a s ∧ (simulation a b h hi).stateRel s t

/-- A lowering exposes the same obligations and compatibility interface as other stages. -/
def LoweringContract.edge {A B : Type} {runs : A → B → Prop} {s : Semantics A} {t : Semantics B}
    (c : LoweringContract runs s t) : EdgeContract runs :=
  ⟨c.kind, c.inputInvariant, c.outputInvariant, Refines s t⟩

/-- Fuel appears only when connecting an executable evaluator to terminal semantics. A successful
evaluator observation is sound at every budget, and each semantic observation has some budget. -/
def EvaluatorAdequacy {A : Type} (s : Semantics A)
    (evaluate : A → Nat → Option Observation) : Prop :=
  (∀ a fuel o, evaluate a fuel = some o → s.observes a o) ∧
  (∀ a o, s.observes a o → ∃ fuel, evaluate a fuel = some o)

end Concrete.Contracts
