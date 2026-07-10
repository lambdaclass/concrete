import Concrete.Resolve.Shared

/-!
# TypeJudgment — the one source-expression type judgment (Phase 6.5 #9 ≡ #13b)

Concrete's front-end bug cluster (E0228, H12) was Check and Elab each running
their *own* source-type inference and disagreeing — the same "N implementations
of one fact" shape that `IntArith` (#1) and `Capabilities` (#5) already fixed by
centralization. This module is the type-axis sibling: the ONE judgment for a
source expression's type, called by Check (for its type-dependent checks) and by
Elab (to stamp `CExpr.ty`). A gate proves the two cannot disagree.

There is NO new typed source IR: Core `CExpr` is already the typed carrier (every
node has `ty : Ty`), so the fix is a shared judgment, not a second AST. The
family migration goes literals → binops → casts → calls → …; each family, once
routed here, has a single type answer by construction.

## Decision records, not bare `Ty`

Following the `IntArith` lesson, each judgment returns the WHOLE decision, not
just the resulting type: an integer literal's decision is its adopted type AND
the range obligation the value must satisfy. Both consumers read the entire
record — Elab stamps `.ty` onto `CExpr`; Check stamps `.ty` AND enforces
`.range` — so neither re-derives any field. If the record carried only `ty`,
drift would simply relocate from "disagree on the type" to "agree on the type,
disagree on the range/coercion." The record is the natural unit that later
carries a `proofArtifact` when preservation proofs (Phase 14) are pulled.

Scope is deliberately the type axis only — type / defaulting / coercion /
range-obligation / rejection. Ownership and capability decisions live in their
own axes' modules (`Capabilities`, the ownership checker); this module does not
accrete them.

Leaf over `Resolve.Shared` (resolved-type predicates + `intTyRange`, whose
numeric range is itself `IntArith.intRange`).
-/

namespace Concrete
namespace TypeJudgment

/-- Is `ty` a float type? (Integer-ness is `Resolve.Shared.isInteger`; float has
    no width table, so the one-line predicate lives here.) -/
def isFloatTy : Ty → Bool
  | .float32 | .float64 => true
  | _ => false

/-- The committed typing decision for an integer literal: the adopted type, plus
    the range obligation the literal value must satisfy (present only when the
    *hint* was a concrete integer type — a defaulted `Int` from a non-integer
    hint is NOT range-checked, matching the historical Check behavior). Both
    Check and Elab consume the whole record; neither re-derives a field. -/
structure IntLitDecision where
  ty : Ty
  range : Option (Int × Int × String)

/-- Canonical typing decision for an integer literal given its (already-resolved)
    hint. An integer or `char` hint is adopted (and carries its range
    obligation); a type-variable hint is adopted (accepts integer literals, no
    range); anything else defaults to `Int` with no range obligation. This is the
    one `intLit` decision Check and Elab share, so they cannot disagree. -/
def intLitDecision (hint : Option Ty) : IntLitDecision :=
  match hint with
  | some t =>
    if isInteger t || t == .char then
      { ty := t, range := intTyRange t }
    else
      { ty := (match t with | .typeVar _ => t | _ => .int), range := none }
  | none => { ty := .int, range := none }

/-- Convenience: the adopted type of an integer literal (the `.ty` field). -/
@[inline] def intLitType (hint : Option Ty) : Ty := (intLitDecision hint).ty

/-- Canonical type of a float literal given its type hint: a float hint is
    adopted, otherwise `Float64`. (Float literals carry no range obligation, so
    the decision is just the type.) -/
def floatLitType (hint : Option Ty) : Ty :=
  match hint with
  | some t => if isFloatTy t then t else .float64
  | none => .float64

end TypeJudgment
end Concrete
