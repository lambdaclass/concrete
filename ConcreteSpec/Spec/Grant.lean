import ConcreteSpec.Spec.Own
import ConcreteSpec.Spec.Authority

/-!
# C5: generative brands and linear grant transitions

PROPOSED, before Core freeze. Brands use `BrandId`, lineage uses `LineageId`, lexical
scopes use `BlockId`, and resource obligations use existing typed place/grant identities.
Dependencies distinguish lexical brand occurrence (including phantom tags) from live allocator
use (Q1.3, Q1.5). A reset changes generation, never lineage. This is the generative binder
transition; existential package syntax and structural dependency extraction remain C9 duties.
The C9 ownership/brand integration slice must tie `Dependency.place` registration and
consumption to the Own slot lifecycle (`move`, `drop`, `leave`); this standalone state
does not establish that correspondence.

Grant rights below are abstract policy atoms, not a replacement target/network grant table.
Concrete resource identity/state is an opaque parameter and is retained, never erased as a
permission. Capability checks and a dynamic policy check are separate inputs (Q7.8).
-/
namespace ConcreteSpec.Core.Generative

structure Binder where
  brand : BrandId
  lineage : LineageId
  scope : BlockId
  allocatorLive : Bool
  deriving DecidableEq, Repr

/-- Phantom occurrences constrain scope but need not keep allocator storage alive (Q1.3). -/
structure Dependency where
  place : Hybrid.Place
  brands : List BrandId
  allocations : List BrandId
  deriving DecidableEq, Repr

structure State where
  binders : List Binder := []
  dependencies : List Dependency := []
  nextBrand : Nat := 0
  nextLineage : Nat := 0
  deriving DecidableEq, Repr

/-- Brand witnesses are nominal identities. Allocations require a still-live allocator;
phantom occurrences only require the binder to remain scoped (Q1.1–Q1.5). -/
def WellFormed (s : State) : Prop :=
  ((s.binders.map Binder.brand).Nodup ∧ (s.dependencies.map Dependency.place).Nodup) ∧
  (∀ b ∈ s.binders, b.brand.id < s.nextBrand ∧ b.lineage.id < s.nextLineage) ∧
  (∀ d ∈ s.dependencies,
    (∀ i ∈ d.brands, ∃ b ∈ s.binders, b.brand = i) ∧
    (∀ i ∈ d.allocations, i ∈ d.brands ∧ ∃ b ∈ s.binders, b.brand = i ∧ b.allocatorLive = true))
instance (s : State) : Decidable (WellFormed s) := by unfold WellFormed; infer_instance


/-- A generative binder allocates a new nominal identity, even at the same lexical loop site
(Q1.1, Q1.2). The allocator starts a new lineage. -/
def fresh (s : State) (scope : BlockId) : Option State :=
  some { s with
    binders := ⟨⟨s.nextBrand⟩, ⟨s.nextLineage⟩, scope, true⟩ :: s.binders
    nextBrand := s.nextBrand + 1
    nextLineage := s.nextLineage + 1 }

/-- Dependency registration is explicit and refuses missing brand/lifetime witnesses. -/
def depend (s : State) (d : Dependency) : Option State :=
  if s.dependencies.all (·.place != d.place) ∧
      (∀ i ∈ d.brands, ∃ b ∈ s.binders, b.brand = i) ∧
      (∀ i ∈ d.allocations, i ∈ d.brands ∧
        ∃ b ∈ s.binders, b.brand = i ∧ b.allocatorLive = true) then
    some { s with dependencies := d :: s.dependencies }
  else none

/-- Discharge the dependent resource obligation, without implicitly cleaning up its allocator. -/
def consume (s : State) (p : Hybrid.Place) : Option State :=
  if s.dependencies.any (·.place == p) then
    some { s with dependencies := s.dependencies.filter (·.place != p) }
  else none

/-- Release/reset cannot invalidate a live allocation dependency. Lexical phantom occurrences
remain valid until their brand binder ends (Q1.3, Q1.5). -/
def release (s : State) (i : BrandId) : Option State :=
  if s.binders.any (fun b => b.brand == i && b.allocatorLive) &&
      s.dependencies.all (fun d => decide (i ∉ d.allocations)) then
    some { s with binders := s.binders.map fun b =>
      if b.brand = i then { b with allocatorLive := false } else b }
  else none

/-- A supported reset is generative on every execution, including loop iterations (Q1.2).
The old brand stays nominally scoped, but its allocator is consumed. -/
def reset (s : State) (i : BrandId) : Option State := do
  let b ← s.binders.find? (·.brand == i)
  let t ← release s i
  some { t with
    binders := ⟨⟨s.nextBrand⟩, b.lineage, b.scope, true⟩ :: t.binders
    nextBrand := s.nextBrand + 1 }

/-- A lexical binder cannot escape its scope through any dependent value, even a zero-byte
phantom tag. No resource is implicitly consumed at scope exit (Q1.3, Q3.8). -/
def endScope (s : State) (scope : BlockId) : Option State :=
  if s.binders.all (fun b => b.scope != scope || !b.allocatorLive) &&
      s.dependencies.all (fun d => s.binders.all (fun b =>
        b.scope != scope || decide (b.brand ∉ d.brands))) then
    some { s with binders := s.binders.filter (·.scope != scope) }
  else none

/-- Couple a brand transition to its concrete allocator operation. Brand witnesses and
lexical dependencies erase; allocator state and failures remain concrete (Q1.1, Q9.10). -/
def instrument {σ α : Type} (transition : State → Option State)
    (operation : σ → Option (α × σ)) (s : σ × State) : Option (α × (σ × State)) := do
  let ghost ← transition s.2
  let (result, concrete) ← operation s.1
  pure (result, (concrete, ghost))

/-- One allocator instruction couples its concrete effect with its brand transition.
A run carries the same concrete operations in the same order after erasure. -/
structure Command (σ α : Type) where
  transition : State → Option State
  operation : σ → Option (α × σ)

/-- Execute a finite allocator command stream, retaining all results in order. -/
def run {σ α : Type} : List (Command σ α) → σ × State → Option (List α × (σ × State))
  | [], s => some ([], s)
  | c :: cs, s => do
    let (a, t) ← instrument c.transition c.operation s
    let (as, u) ← run cs t
    pure (a :: as, u)

/-- The uninstrumented allocator stream forgets brand transitions only. -/
def runErased {σ α : Type} : List (Command σ α) → σ → Option (List α × σ)
  | [], s => some ([], s)
  | c :: cs, s => do
    let (a, t) ← c.operation s
    let (as, u) ← runErased cs t
    pure (a :: as, u)

/-- The executable brand operations; arbitrary allocator callbacks cannot certify a lifetime
transition. The finite interpreter below uses precisely these six public operations. -/
inductive LifetimeOp where
  | fresh (scope : BlockId)
  | depend (dependency : Dependency)
  | consume (place : Hybrid.Place)
  | release (brand : BrandId)
  | reset (brand : BrandId)
  | endScope (scope : BlockId)

def lifetimeStep (s : State) : LifetimeOp → Option State
  | .fresh b => fresh s b
  | .depend d => depend s d
  | .consume p => consume s p
  | .release i => release s i
  | .reset i => reset s i
  | .endScope b => endScope s b

def runLifetime (s : State) (ops : List LifetimeOp) : Option State :=
  ops.foldlM lifetimeStep s

/-- A scoped identity has ended, but remains below the issued generation supply. -/
def Retired (s : State) (i : BrandId) : Prop :=
  i.id < s.nextBrand ∧ ∀ b ∈ s.binders, b.brand ≠ i
instance (s : State) (i : BrandId) : Decidable (Retired s i) := by
  unfold Retired
  infer_instance


namespace Examples

def initial : State := (fresh {} ⟨0⟩).getD {}
def heap : Dependency := ⟨⟨0⟩, [⟨0⟩], [⟨0⟩]⟩
def phantom : Dependency := ⟨⟨1⟩, [⟨0⟩], []⟩

/-- Nonempty well-formed dependency table; duplicating its place is a planted bad state. -/
example : WellFormed { initial with dependencies := [heap, phantom] } := by decide
example : ¬ WellFormed { initial with dependencies := [heap, heap] } := by decide

/-- Accepted (rule `ConcreteSpec.Core.Generative.reset`): repeated resets at the same site produce distinct nominal generations. -/
example : (do
    let s ← reset initial ⟨0⟩
    let s ← reset s ⟨1⟩
    pure (s.binders.map Binder.brand, s.binders.map Binder.lineage)) =
      some ([⟨2⟩, ⟨1⟩, ⟨0⟩], [⟨0⟩, ⟨0⟩, ⟨0⟩]) := by decide
/-- Refused (rule `ConcreteSpec.Core.Generative.reset`): a live allocation prevents its allocator's reset. -/
example : (depend initial heap).bind (fun s => reset s ⟨0⟩) = none := by decide
/-- Accepted: a phantom tag does not prevent reset, but still constrains lexical scope. -/
example : ((depend initial phantom).bind (fun s => reset s ⟨0⟩)).isSome = true := by decide
/-- Refused (rule `ConcreteSpec.Core.Generative.endScope`): zero representation bytes do not end the phantom's linear obligation. -/
example : (do
    let s ← depend initial phantom
    let s ← release s ⟨0⟩
    endScope s ⟨0⟩) = none := by decide
/-- Accepted (rule `ConcreteSpec.Core.Generative.endScope`): explicitly consume the dependent, then release the allocator and end its binder. -/
example : (do
    let s ← depend initial heap
    let s ← consume s ⟨0⟩
    let s ← release s ⟨0⟩
    endScope s ⟨0⟩).isSome = true := by decide

end Examples
end ConcreteSpec.Core.Generative

namespace ConcreteSpec.Core.Grants
open ConcreteSpec.Authority (Cap)

/-- The runtime resource/identity is opaque; rights are abstract policy atoms. Permissions are
separate erased capability sets. No inference about Entropy or a network schema is made. -/
structure Resource (Handle : Type) where
  handle : Handle
  rights : List Nat
  permission : Cap
  live : Bool
  deriving DecidableEq, Repr

structure State (Handle : Type) where
  resources : AList GrantId (Resource Handle) := []
  next : Nat := 0
  deriving DecidableEq, Repr

/-- A successful lookup also checks linear availability. A consumed identity is never usable. -/
def usable {H : Type} (s : State H) (i : GrantId) : Option (Resource H) := do
  let r ← AList.lookup s.resources i
  if r.live then some r else none

/-- Extensional permission of a live grant, independent of the representation/order of rights. -/
def Permits {H : Type} (s : State H) (i : GrantId) (right : Nat) : Prop :=
  ∃ r, usable s i = some r ∧ right ∈ r.rights


/-- Mark consumption without removing the historical identity. -/
def consumed {H : Type} (s : State H) (i : GrantId) : State H :=
  { s with resources := AList.update s.resources i (fun r => { r with live := false }) }

/-- Issuing a new identity never reuses a previous one; launch supplies the initial resources. -/
def issue {H : Type} (s : State H) (r : Resource H) : State H :=
  ⟨(⟨s.next⟩, r) :: s.resources, s.next + 1⟩

/-- Narrowing consumes the parent and gives precisely the intersection of rights (Q7.2).
It is total for a live input; no dynamic policy failure is introduced. -/
def narrow {H : Type} (s : State H) (i : GrantId) (requested : List Nat) : Option (State H) := do
  let r ← usable s i
  pure (issue (consumed s i) { r with rights := r.rights.filter (fun x => decide (x ∈ requested)) })

/-- Acquisition keeps its parent, checks the declared capability and atomic dynamic policy,
and accepts only a child policy contained in parent ∩ request (Q7.2, Q7.8). The supplied handle
is the successful acquisition result; an external failure has no such transition. -/
def acquire {H : Type} (s : State H) (i : GrantId) (requested selected : List Nat)
    (permissions : List Cap) (policyAllowed : Bool) (handle : H) : Option (State H) := do
  let r ← usable s i
  if r.permission ∈ permissions ∧ policyAllowed = true ∧
      ∀ right ∈ selected, right ∈ r.rights ∧ right ∈ requested then
    some (issue s ⟨handle, selected, r.permission, true⟩)
  else none

/-- Close errors carry only a Copy error code, never a handle. -/
abbrev CloseResult := Except Nat Unit

/-- Close/give-up consumes on every specified outcome, including failure (Q7.3).
This contract assumes the platform adapter's release guarantee; it does not model syscalls. -/
def close {H : Type} (s : State H) (i : GrantId) (outcome : CloseResult) :
    Option (CloseResult × State H) := do
  let _ ← usable s i
  pure (outcome, consumed s i)

/-- A launch budget bounds every resource's rights, including consumed history. -/
def Bounded {H : Type} (budget : List Nat) (s : State H) : Prop :=
  ∀ e ∈ s.resources, ∀ right ∈ e.2.rights, right ∈ budget
instance {H : Type} (budget : List Nat) (s : State H) : Decidable (Bounded budget s) := by
  unfold Bounded; infer_instance

/-- Historical identities remain below the monotonically increasing supply. -/
def Fresh {H : Type} (s : State H) : Prop := ∀ e ∈ s.resources, e.1.id < s.next
instance {H : Type} (s : State H) : Decidable (Fresh s) := by
  unfold Fresh; infer_instance

/-- One permitted grant operation (narrowing, child acquisition, or close). -/
inductive Op (H : Type) where
  | narrow (i : GrantId) (requested : List Nat)
  | acquire (i : GrantId) (requested selected : List Nat) (permissions : List Cap)
      (allowed : Bool) (handle : H)
  | close (i : GrantId) (outcome : CloseResult)
  deriving DecidableEq, Repr

/-- Execute one grant operation. -/
def stepOp {H : Type} (s : State H) : Op H → Option (State H)
  | .narrow i requested => narrow s i requested
  | .acquire i requested selected permissions allowed handle =>
    acquire s i requested selected permissions allowed handle
  | .close i outcome => (close s i outcome).map Prod.snd

/-- Execute a finite sequence of permitted grant operations, without a minting operation. -/
def runOps {H : Type} : State H → List (Op H) → Option (State H)
  | s, [] => some s
  | s, op :: ops => (stepOp s op).bind fun t => runOps t ops

/-- Any finite sequence of the permitted grant transitions, without a minting operation. -/
inductive Steps {H : Type} : State H → State H → Prop where
  | refl (s) : Steps s s
  | narrow {s t u i requested} : Steps s t → narrow t i requested = some u → Steps s u
  | acquire {s t u i requested selected permissions allowed handle} : Steps s t →
      acquire t i requested selected permissions allowed handle = some u → Steps s u
  | close {s t u i result} : Steps s t → close t i result = some (result, u) → Steps s u

namespace Examples

def initial : State Nat := ⟨[(⟨0⟩, ⟨42, [0, 1], .fs, true⟩)], 1⟩
/-- Accepted (rule `ConcreteSpec.Core.Grants.narrow`): narrowing consumes the parent and intersects rather than replacing rights. -/
example : ((narrow initial ⟨0⟩ [1, 2]).map fun s =>
    ((usable s ⟨0⟩).isNone, (usable s ⟨1⟩).map Resource.rights)) = some (true, some [1]) := by decide
/-- Accepted (rule `ConcreteSpec.Core.Grants.acquire`): acquisition preserves its parent. -/
example : ((acquire initial ⟨0⟩ [1] [1] [.fs] true 99).map fun s =>
    ((usable s ⟨0⟩).isSome, (usable s ⟨1⟩).map Resource.handle)) = some (true, some 99) := by decide
/-- Refused (rule `ConcreteSpec.Core.Grants.acquire`): neither a new writable right nor a missing capability may be manufactured. -/
example : acquire initial ⟨0⟩ [2] [2] [.fs] true 99 = none := by decide
example : acquire initial ⟨0⟩ [1] [1] [] true 99 = none := by decide
/-- Refused: static permission cannot bypass an atomic dynamic scope check. -/
example : acquire initial ⟨0⟩ [1] [1] [.fs] false 99 = none := by decide
/-- Accepted (rule `ConcreteSpec.Core.Grants.close`): a failed close still consumes the handle. -/
example : ((close initial ⟨0⟩ (.error 5)).map fun (_, s) => (usable s ⟨0⟩).isNone) =
    some true := by decide
/-- Refused (rule `ConcreteSpec.Core.Grants.narrow`): a consumed resource cannot be used, narrowed or closed again. -/
example : (do
    let (_, s) ← close initial ⟨0⟩ (.ok ())
    narrow s ⟨0⟩ [0]) = none := by decide

/-- Refused (rule `ConcreteSpec.Core.Grants.close`): double close cannot revive a handle. -/
example : (do
    let (_, s) ← close initial ⟨0⟩ (.error 5)
    close s ⟨0⟩ (.ok ())) = none := by decide

end Examples
end ConcreteSpec.Core.Grants
