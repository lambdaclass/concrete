import Concrete.Frontend.AST

/-!
Pattern binding mode (decision D3, TIGHTENING_DECISIONS; MEMORY_SEMANTICS §15.3; closes bug 181).

ONE function decides a pattern binder's type, and Check and Elab both call it, so the checker's
binder and the Core binder cannot disagree again (bug 181: Check said `&T`, Elab said `T`, and the
specialization at `T := Int` passed an integer where a pointer was expected).

The rule: a binder that binds THROUGH a scrutinee of reference type (`match r`, `match &o`,
`if let`, `while let`) has reference type: a payload field of type `T` binds as `&T`, and a
whole-value binder over `&T` or `&mut T` binds as `&T`, whether or not `T` is Copy. A binder over
an owned scrutinee binds the value (a whole-value binder over `&mut T` is that `&mut T`). A Copy
value behind the reference is read with `*value`.

The mode is STORED in Core as the binder's type (`CMatchArm.enumArm` bindings, `varArm`'s type):
a reference-typed binder is a borrowed binder. What substitution preserves: Mono substitutes type
variables only, so it maps `&T` to `&σ(T)` and a non-reference type to a non-reference type
(a type variable never stands for the scrutinee's reference constructor, which is written in the
scrutinee's type). And a payload field can never itself be a reference (REF-3), so "the binder's
type is a reference" holds before Mono exactly when it holds after, and it means "borrowed binder"
in both. Lower and the interpreter read the mode from the binder's type (`binderIsBorrow`); none
of them re-derives it from Copy-ness, which substitution DOES change.
-/

namespace Concrete

/-- Does a pattern over a scrutinee of this type bind through a reference? -/
def bindsThroughReference : Ty → Bool
  | .ref _ | .refMut _ => true
  | _ => false

/-- The type of a binder for a payload field of type `fieldTy`, under a scrutinee of type
    `scrutTy` (D3). -/
def payloadBinderTy (scrutTy fieldTy : Ty) : Ty :=
  if bindsThroughReference scrutTy then .ref fieldTy else fieldTy

/-- The type of a whole-value binder (`n => ...`, a catch-all arm) over a scrutinee of type
    `scrutTy` whose referent type is `innerTy` (D3): the scrutinee's own reference type (`&T`
    over `&T`, `&mut T` over `&mut T`: the binder IS the reference), the value otherwise. -/
def wholeBinderTy (scrutTy innerTy : Ty) : Ty :=
  match scrutTy with
  | .ref _ => .ref innerTy
  | .refMut _ => .refMut innerTy
  | _ => innerTy

/-- A Core binder of reference type is a BORROWED binder: it names the scrutinee's storage
    (the payload field, or the whole referent), it is not a copy of it. Payload fields cannot be
    references (REF-3), so this reads the mode exactly, before and after monomorphization. -/
def binderIsBorrow : Ty → Bool
  | .ref _ | .refMut _ => true
  | _ => false

end Concrete
