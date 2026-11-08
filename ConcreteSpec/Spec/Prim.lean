import ConcreteSpec.Spec.Exec

/-!
# Identities, primitive contracts, evidence and ghost erasure

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`. Foundation contract C9 (the foundation proposal approved by the owner
on 2026-11-08, "ok all recommended").

C1 leaves its operand expressions abstract. The first section below therefore supplies a minimal
explicit term language, including a binder, for the identity laws. Its `copy` and `move` forms have
the same value semantics here; their ownership distinction belongs to C5. The adapter `operands`
plugs this language into C1 without changing C1.

A renaming is a bijection of numeric binder identities. It changes binders and all their occurrences
together, so injectivity prevents capture. Source-name hints are presentation only and are treated
by a separate transformation.
-/

namespace ConcreteSpec.Core
namespace Prim

/-- A bijection of numeric binder identities, with both inverse laws available to proofs. -/
structure Renaming where
  toFun : Nat → Nat
  inv : Nat → Nat
  toFun_inv : ∀ n, toFun (inv n) = n
  inv_toFun : ∀ n, inv (toFun n) = n

/-- The identity renaming. -/
def Renaming.id : Renaming where
  toFun := _root_.id
  inv := _root_.id
  toFun_inv := fun _ => rfl
  inv_toFun := fun _ => rfl

/-- The inverse renaming. -/
def Renaming.symm (r : Renaming) : Renaming where
  toFun := r.inv
  inv := r.toFun
  toFun_inv := r.inv_toFun
  inv_toFun := r.toFun_inv

/-- The minimal operand language used by the identity laws: integer literals, local reads, explicit
`copy`/`move`, addition, and a lexically scoped binder. -/
inductive Term where
  | lit (n : Int)
  | var (x : Var)
  | copy (x : Var)
  | move (x : Var)
  | add (a b : Term)
  | bind (x : Var) (value body : Term)
  deriving DecidableEq, Repr

/-- Rename a binder identity without changing its presentation hint. -/
def renameVar (r : Renaming) (x : Var) : Var := { x with id := r.toFun x.id }

/-- Rename every binder and occurrence. Because the same injective map is used everywhere, a free
occurrence cannot be captured by a renamed binder. -/
def renameTerm (r : Renaming) : Term → Term
  | .lit n => .lit n
  | .var x => .var (renameVar r x)
  | .copy x => .copy (renameVar r x)
  | .move x => .move (renameVar r x)
  | .add a b => .add (renameTerm r a) (renameTerm r b)
  | .bind x value body => .bind (renameVar r x) (renameTerm r value) (renameTerm r body)

/-- Rename the keys of a local environment, preserving entry order and values. -/
def renameLocals (r : Renaming) (L : AList Var Int) : AList Var Int :=
  List.map (fun e => (renameVar r e.1, e.2)) L

/-- Change a local's presentation hint without changing its numeric identity. -/
def rehintVar (h : Var → String) (x : Var) : Var := { x with hint := h x }

/-- Change every presentation hint in a term, leaving its binders' numeric identities untouched. -/
def rehintTerm (h : Var → String) : Term → Term
  | .lit n => .lit n
  | .var x => .var (rehintVar h x)
  | .copy x => .copy (rehintVar h x)
  | .move x => .move (rehintVar h x)
  | .add a b => .add (rehintTerm h a) (rehintTerm h b)
  | .bind x value body => .bind (rehintVar h x) (rehintTerm h value) (rehintTerm h body)

/-- Rehint every key of a local environment, preserving entry order and values. -/
def rehintLocals (h : Var → String) (L : AList Var Int) : AList Var Int :=
  List.map (fun e => (rehintVar h e.1, e.2)) L

/-- Look up a numeric local identity. Presentation hints are not consulted. -/
def lookupId : AList Var Int → Nat → Option Int
  | [], _ => none
  | (x, v) :: L, n => if x.id = n then some v else lookupId L n

/-- Evaluate a term against locals. An unbound local is unsafe stuckness; this fragment adds no
failure of its own. -/
def Term.eval (L : AList Var Int) : Term → Res Int
  | .lit n => .ok n
  | .var x | .copy x | .move x => match lookupId L x.id with
    | some v => .ok v
    | none => .stuck
  | .add a b => (eval L a).bind fun x => (eval L b).map fun y => x + y
  | .bind x value body => (eval L value).bind fun v => eval ((x, v) :: L) body

/-- The minimal terms as C1 operands. There is no primitive operation, instrumentation or event in
this fragment; C9's registry below supplies primitive identities to richer operand languages. -/
def operands : Exec.Operands where
  Val := Int
  Expr := Term
  Ty := Unit
  Store := Unit
  Prim := Empty
  Event := Empty
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => true
  eval := fun L st e => (Term.eval L e).map fun v => (v, st)
  life := fun _ st => some st
  test := fun v => some (v ≠ 0)
  allows := fun _ _ _ _ => false

/-! ### Examples -/

/-- Accepted: a bound local evaluates to its value, including through the C1 adapter. -/
example : Term.eval [(⟨"x", 0⟩, 3)] (.var ⟨"x", 0⟩) = .ok 3 := rfl

example : operands.eval ([(⟨"x", 0⟩, 3)] : AList Var Int) () (.var ⟨"x", 0⟩) =
    (.ok ((3 : Int), ()) : Res (Int × Unit)) := rfl

/-- Refused: reading an unbound local is stuck, never a fabricated zero. -/
example : Term.eval [(⟨"x", 0⟩, 3)] (.var ⟨"y", 1⟩) = .stuck := rfl

/-- A binder scopes its body and does not erase the outer local after the body finishes. -/
example : Term.eval [(⟨"x", 0⟩, 1)]
    (.bind ⟨"y", 1⟩ (.lit 2) (.add (.var ⟨"x", 0⟩) (.var ⟨"y", 1⟩))) = .ok 3 := rfl

end Prim
end ConcreteSpec.Core
