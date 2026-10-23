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
a reference-typed binder is a borrowed binder. What substitution preserves:

- A PAYLOAD binder: Mono substitutes type variables only, so it maps `&T` to `&σ(T)`; and a
  payload field can never itself be a reference, not even through a type parameter (REF-3,
  checked before specialization and re-checked after it by `verifyNoStoredRefs`), so a by-value
  payload binder's type stays a non-reference. "The binder's type is a reference" therefore holds
  before Mono exactly when it holds after, and it means "borrowed payload binder" in both.
- A WHOLE-VALUE binder: its type is the scrutinee's own type (the reference over a reference, the
  value otherwise). An owned scrutinee of type `T` can specialize to a reference (`T := &Int`), and
  the binder's type with it; then binding "the scrutinee reference" and binding "the scrutinee
  value" are the same value, so Lower and the interpreter bind the same thing whichever they read
  the mode as. The mode is irrelevant there, which is why it may change under substitution. Lower and the interpreter read the mode from the binder's type (`binderIsBorrow`); none
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
