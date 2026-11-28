import Concrete.Contracts

namespace Concrete.Interfaces

open Concrete.Contracts

/-- A program is safe when none of its reachable states can go wrong. -/
def ProgramSafe {A E : Type} (semantics : Semantics A E) (program : A) : Prop :=
  ∀ state, ¬ semantics.stuck program state

/-- Safe-source backward behavior refinement. Termination compares the full result
and finite trace. Infinite executions compare all finite visible prefixes, so
silent divergence and divergence with infinitely many events are both covered.
A safe source has no wrong behavior for a stuck target execution to match. -/
def BehaviorRefinement {A B E O : Type} (source : Semantics A E) (target : Semantics B E)
    (sourceResult : A → source.State → O → Prop) (targetResult : B → target.State → O → Prop)
    (input : A) (output : B) : Prop :=
  ProgramSafe source input →
    (∀ q result, target.finite output q → targetResult output q result →
      ∃ p, source.finite input p ∧ sourceResult input p result ∧
        source.trace input p = target.trace output q) ∧
    (∀ run, target.diverges output run →
      ∃ matching, source.diverges input matching ∧
        SameInfiniteTrace (fun i => source.trace input (matching i))
          (fun i => target.trace output (run i))) ∧
    ProgramSafe target output

/-- Safe-source backward behavior refinement is reflexive for every program,
introducing `ProgramSafe` internally. -/
theorem behaviorRefl {A E O : Type} {S : Semantics A E}
    {R : A → S.State → O → Prop} (a : A) :
    BehaviorRefinement S S R R a a := by
  intro safe
  exact ⟨fun q result reached finished => ⟨q, reached, finished, rfl⟩,
    fun run infinite => ⟨run, infinite, fun _ => Iff.rfl⟩, safe⟩

/-- Safe-source backward behavior refinement is transitive across stages. -/
theorem behaviorTrans {A B C E O : Type}
    {S : Semantics A E} {T : Semantics B E} {V : Semantics C E}
    {R : A → S.State → O → Prop} {U : B → T.State → O → Prop} {W : C → V.State → O → Prop}
    {a : A} {b : B} {c : C}
    (left : BehaviorRefinement S T R U a b)
    (right : BehaviorRefinement T V U W b c) :
    BehaviorRefinement S V R W a c := by
  intro safeA
  obtain ⟨leftTerm, leftDiv, safeB⟩ := left safeA
  obtain ⟨rightTerm, rightDiv, safeC⟩ := right safeB
  refine ⟨?_, ?_, safeC⟩
  · intro q result reachedC finishedC
    obtain ⟨m, reachedB, finishedB, traceBC⟩ := rightTerm q result reachedC finishedC
    obtain ⟨p, reachedA, finishedA, traceAB⟩ := leftTerm m result reachedB finishedB
    exact ⟨p, reachedA, finishedA, traceAB.trans traceBC⟩
  · intro run divC
    obtain ⟨mid, divB, traceBC⟩ := rightDiv run divC
    obtain ⟨src, divA, traceAB⟩ := leftDiv mid divB
    exact ⟨src, divA, fun events => (traceAB events).trans (traceBC events)⟩

end Concrete.Interfaces
