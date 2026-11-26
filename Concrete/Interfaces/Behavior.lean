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

end Concrete.Interfaces
