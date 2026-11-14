import ConcreteSpec.Spec.Leak

/-! C7 release admission over the transitive call graph, including deferred calls.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7).
This is static reachability, conservatively including both branches and loop regions. -/
namespace ConcreteSpec.Core.Leak
open Exec

/-- Policy interface: permitted functions and permitted typed sites. Empty lists forbid release.
The function list authorizes callers as well as the function containing a release. -/
structure ReleaseInterface (Ty : Type) where
  functions : List FnId
  sites : ReleasePolicy Ty

/-- Calls and explicitly declared release sites extracted from function bodies. -/
structure ReleaseCalls (Ty : Type) where
  calls : List (FnId × FnId)
  releases : List (FnId × ReleaseSite Ty)

/-- All ordinary and deferred calls in a statement, including nested control regions. -/
def statementCalls {O : Operands} : Stmt O → List FnId
  | .call _ f _ _ | .defer f _ _ => [f]
  | .seq s t | .cond _ s t _ | .loop _ s _ t _ => statementCalls s ++ statementCalls t
  | .block _ s => statementCalls s
  | _ => []

/-- Explicit release primitives supply their declared type; other primitives supply none.
The primitive contract owns this classification. No release is inferred from diagnostic metadata. -/
def statementReleases {O : Operands} (declared : O.Prim → Option O.Ty) :
    Stmt O → List (ReleaseSite O.Ty)
  | .prim _ p _ site => (declared p).toList.map fun ty => ⟨site, ty⟩
  | .seq s t | .cond _ s t _ | .loop _ s _ t _ =>
      statementReleases declared s ++ statementReleases declared t
  | .block _ s => statementReleases declared s
  | _ => []

/-- Extract call edges and release sites from C1's function bodies; there is no second call syntax. -/
def ReleaseCalls.ofProgram {O : Operands} (declared : O.Prim → Option O.Ty)
    (p : Program O) : ReleaseCalls O.Ty where
  calls := p.fns.flatMap fun (f, body) => (statementCalls body.body).map fun g => (f, g)
  releases := p.fns.flatMap fun (f, body) => (statementReleases declared body.body).map fun r => (f, r)

/-- Reflexive transitive call reachability. Recursion and arbitrary finite call depth need no fuel. -/
inductive ReleaseCalls.Reaches {Ty : Type} (g : ReleaseCalls Ty) : FnId → FnId → Prop where
  | self (f) : g.Reaches f f
  | call {f middle last} : (f, middle) ∈ g.calls → g.Reaches middle last → g.Reaches f last

/-- A release is permitted only if every caller that can reach it is allowlisted and its exact
 typed site is listed. This delegates site membership to ReleasePolicy.Permits.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
def ReleaseInterface.Admitted {Ty : Type} (policy : ReleaseInterface Ty) (g : ReleaseCalls Ty) : Prop :=
  ∀ caller owner site, g.Reaches caller owner → (owner, site) ∈ g.releases →
    caller ∈ policy.functions ∧ policy.sites.Permits (⟨site, ()⟩ : Release Ty Unit)

/-- Program admission uses the extracted C1 call graph and declared primitive release sites.
The classifier is part of the primitive interface; clients cannot replace call reachability.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
def ReleaseInterface.ProgramAdmitted {O : Operands} (policy : ReleaseInterface O.Ty)
    (declared : O.Prim → Option O.Ty) (p : Program O) : Prop :=
  policy.Admitted (ReleaseCalls.ofProgram declared p)

end ConcreteSpec.Core.Leak
