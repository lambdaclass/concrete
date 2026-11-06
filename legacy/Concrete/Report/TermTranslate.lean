/-
# Obligation `Expr` → `TermIR.Term` (R-0455)

The producer for the term IR. Split out from the measurement report so that
`ReportObligations` can import it: `evalIntEnv`/`evalBoolEnv` are now thin wrappers over
`ofExpr` + `TermIR.eval*`, which is what removes a `partial def` from the path every
lowering-agreement check measures against.

Imports the surface AST and `TermIR` and nothing else. `TermIR` itself must not import the
frontend — a term language that does cannot be the neutral middle of a pipeline — so the
dependency points this way round.
-/
import Concrete.Semantics.TermIR
import Concrete.Frontend.AST

namespace Concrete
namespace Report

open Concrete.TermIR

/-- Map a surface binary operator onto the IR's. `none` for operators outside the fragment,
    which is a REJECTION rather than an approximation — an operator silently mapped to a
    near-neighbour is the failure mode this whole IR exists to remove. -/
def irBinOp : BinOp → Option TermIR.Op
  | .add => some .add | .sub => some .sub | .mul => some .mul
  -- Carried, not dropped. This is the defect: `exprToProver` returns `none` here.
  | .div => some .tdiv | .mod => some .tmod
  | .leq => some .le | .lt => some .lt | .eq => some .eq
  | .and_ => some .and_ | .or_ => some .or_
  -- geq/gt/neq are absent HERE because they are CANONICALISED in `ofExpr` rather than
  -- rejected: `a >= b` becomes `b <= a`, `a > b` becomes `b < a`, `a != b` becomes
  -- `not (a = b)`. One direction per relation means every transformation has half as many
  -- cases to be sound for, and nothing is lost.
  --
  -- Bit ops ARE rejected: the bv sort exists but no transformation targets it, and admitting
  -- terms no pass can handle would make effect locks like `hasTmod` unprovable for them.
  | _ => none

mutual
  /-- Translate an obligation expression into the IR. Structural and total: every unhandled
      construct yields `none`, never a guess.

      A `call` becomes an UNINTERPRETED symbol. That is the point — the translation does not
      need to know what a spec function means, and a transformation must be sound for every
      interpretation of it (`TermIR.SymEnv` is quantified over in the row-1 theorems). -/
  def ofExpr : Expr → Option TermIR.Term
    | .intLit _ v => some (.lit v)
    | .boolLit _ b => some (.blit b)
    | .ident _ n => some (.var n)
    | .paren _ e => ofExpr e
    | .unaryOp _ .neg e => (ofExpr e).map (fun t => .bin .sub (.lit 0) t)
    | .unaryOp _ .not_ e => (ofExpr e).map (.un .not_)
    -- CANONICAL FORMS. `evalBoolEnv` (which this replaces) handled `geq`/`gt`/`neq`
    -- directly; rejecting them here would have silently narrowed what the agreement checks
    -- can evaluate. Swapping operands preserves meaning exactly and keeps the IR's relation
    -- set minimal.
    | .binOp _ .geq l r => do let a ← ofExpr l; let b ← ofExpr r; some (.bin .le b a)
    | .binOp _ .gt  l r => do let a ← ofExpr l; let b ← ofExpr r; some (.bin .lt b a)
    | .binOp _ .neq l r => do let a ← ofExpr l; let b ← ofExpr r; some (.un .not_ (.bin .eq a b))
    | .binOp _ op l r => do
      let o ← irBinOp op
      let a ← ofExpr l
      let b ← ofExpr r
      some (.bin o a b)
    | .call _ f _ args => (ofExprs args).map (.sym f)
    -- Casts are CARRIED now, as a wrap at the target's width — not dropped, and not treated
    -- as identity. A cast whose target has no fixed width (`Int`/`Uint`, whose overflow is
    -- profile-dependent) is still rejected: there is no width to wrap at, so modelling it
    -- would mean inventing one.
    | .cast _ e ty => do
      let (w, signed) ← IntArith.intBitWidth ty
      let t ← ofExpr e
      some (.cast w signed t)
    | _ => none

  def ofExprs : List Expr → Option (List TermIR.Term)
    | [] => some []
    | e :: es => do
      let t ← ofExpr e
      let ts ← ofExprs es
      some (t :: ts)
end


end Report
end Concrete
