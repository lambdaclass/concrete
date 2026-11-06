/-
# Obligation `Expr` → `TermIR.Term` (R-0455, slice 1c)

`TermIR` gave the pipeline a typed IR and a proved transformation. Nothing produced a `Term`
from a real obligation, which made Register B row 1 a true theorem about a transformation the
compiler never ran. This module is the missing producer.

It lives here rather than in `Concrete/Semantics/` because it depends on the surface AST, and
`TermIR` must not: a term language that imports the frontend cannot be the neutral middle of
a pipeline.

## What this measures, immediately

The string lowering (`exprToProver`) DROPS two things, and R-0455 names both:

* `div`/`mod` inside a larger expression — the operator table is infix-only, so a prefix form
  has nowhere to go. The divisor-nonzero *obligation* is unaffected; it is `a / b` appearing
  as a subterm that is lost.
* spec-function calls — there are no uninterpreted symbols to carry them.

`ofExpr` carries both. So the two functions disagree on exactly the obligations the string
layer loses, and `droppedByStringLayer` below turns that into a number rather than a claim in
a roadmap entry.
-/
import Concrete.Report.TermTranslate
import Concrete.Report.ReportObligations

namespace Concrete
namespace Report

open Concrete.TermIR

/-- Obligations whose expression the IR can carry but the STRING layer drops — the defect
    R-0455 describes, as a count.

    `exprToProver` returns `none` for a term containing `div`/`mod` or a call; `ofExpr` returns
    `some`. Every key here is an obligation the prover path silently loses today. Reported by
    `--report term-ir` so the number is observable instead of asserted. -/
def droppedByStringLayer (modules : List Module) : List (String × String) :=
  (multiKernelObligations modules (requireLeanGoal := false)).filterMap fun o =>
    match exprToProver rocqBinOp o.mainExpr, ofExpr o.mainExpr with
    | none, some _ => some (o.key, Concrete.fmtExpr o.mainExpr)
    | _, _ => none

/-- Obligations both layers carry. -/
def carriedByBoth (modules : List Module) : List String :=
  (multiKernelObligations modules (requireLeanGoal := false)).filterMap fun o =>
    match exprToProver rocqBinOp o.mainExpr, ofExpr o.mainExpr with
    | some _, some _ => some o.key
    | _, _ => none

/-- Obligations NEITHER layer carries — and the reason this is reported rather than left
    implicit.

    A `0 dropped` result is easy to read as "the string layer loses nothing". Measured on the
    corpus it means something narrower: the obligations that DO contain a division subterm
    (e.g. `arr[(a / b) as Int]`) also contain a `cast`, which neither layer models, so they are
    dropped by both and never counted as an IR win.

    `ofExpr` rejects casts deliberately. A cast changes value semantics — truncation — so
    carrying it as if transparent would be a silent misinterpretation, which is the failure
    this IR exists to remove. Modelling casts is future work; pretending they are identity
    would be worse than dropping them.

    So the IR's advantage on div/mod is **latent on this corpus, not live**. Reporting all
    three buckets is what makes that visible instead of hidden behind a zero. -/
def droppedByBoth (modules : List Module) : List String :=
  (multiKernelObligations modules (requireLeanGoal := false)).filterMap fun o =>
    match exprToProver rocqBinOp o.mainExpr, ofExpr o.mainExpr with
    | none, none => some o.key
    | _, _ => none

/-! ### Behavioural locks

`ofExpr` handles what the string layer drops. Pinned by `rfl` rather than asserted in a gate,
so weakening the translation is a build failure. -/

private def sp : Span := default
private def eSymR : TermIR.SymEnv := fun _ _ => none

-- `a / b` as a SUBTERM: dropped by the string layer, carried here. The obligation this
-- appears in is exactly the case R-0455 names.
example : ofExpr (.binOp sp .lt (.binOp sp .div (.ident sp "a") (.ident sp "b")) (.intLit sp 10))
        = some (.bin .lt (.bin .tdiv (.var "a") (.var "b")) (.lit 10)) := rfl
-- The counterpart, and it is a `rfl` example NOW. This comment previously said it could not
-- be one, because `exprToProver` was a `partial def` and the kernel could not reduce it.
-- It turned out not to need `partial` at all — nor did `exprToSmt`, `exprToLeanProp`,
-- `arithToBVW` or three helpers; all seven recurse only on direct subterms and were marked
-- partial by habit. Removing the keyword was the whole fix.
--
-- So the drop the IR exists to repair is now pinned at COMPILE TIME rather than only counted
-- at runtime:
example : exprToProver rocqBinOp
    (.binOp sp .lt (.binOp sp .div (.ident sp "a") (.ident sp "b")) (.intLit sp 10)) = none := rfl
example : exprToProver rocqBinOp (.call sp "f" [] [.ident sp "x"]) = none := rfl
-- ...while a term inside the fragment still renders, so the lock above is not vacuous.
example : exprToProver rocqBinOp
    (.binOp sp .lt (.ident sp "a") (.intLit sp 10)) = some "a < 10" := rfl

-- A spec-function call becomes an uninterpreted symbol rather than being dropped.
example : ofExpr (.call sp "f" [] [.ident sp "x"]) = some (.sym "f" [.var "x"]) := rfl

-- Unary negation is normalised to `0 - x`, so the IR needs no unary-minus operator and every
-- transformation has one fewer case to be sound for.
example : ofExpr (.unaryOp sp .neg (.ident sp "x")) = some (.bin .sub (.lit 0) (.var "x")) := rfl

-- CASTS: carried as a wrap at the target width, matching `Interp.evalCast`'s
-- `IntArith.wrapToType`. This is what unblocks the div/mod case measured as latent: an
-- obligation like `arr[(a / b) as Int]` was previously dropped by the IR too.
example : ofExpr (.cast sp (.ident sp "x") .i32) = some (.cast 32 true (.var "x")) := rfl
example : ofExpr (.cast sp (.ident sp "x") .u8) = some (.cast 8 false (.var "x")) := rfl
-- The wrap is the REFERENCE's, not a re-derivation: truncation at i8 agrees with
-- `IntArith.wrapToType`, checked on a value that actually wraps.
example : TermIR.evalInt [("x", 200)] eSymR (.cast 8 true (.var "x"))
        = some (IntArith.wrapToType .i8 200) := rfl
example : TermIR.evalInt [("x", 200)] eSymR (.cast 8 true (.var "x")) = some (-56) := rfl
-- A cast is NOT identity, and that is pinned so nobody "simplifies" it away.
example : TermIR.evalInt [("x", 200)] eSymR (.cast 8 true (.var "x"))
        ≠ TermIR.evalInt [("x", 200)] eSymR (.var "x") := by decide
-- Widths the IR cannot model are still rejected rather than guessed.
example : ofExpr (.cast sp (.ident sp "x") .bool) = none := rfl

-- RELATIONS ARE CANONICALISED, not rejected. `irBinOp` has no `geq`, but `ofExpr` rewrites
-- `a >= b` to `b <= a` — meaning preserved, relation set minimal. Rejecting them outright
-- (which an earlier version did) would have silently narrowed what the reference evaluator
-- can evaluate the moment `evalBoolEnv` started routing through this translation, because
-- the old evaluator handled geq/gt/neq directly.
example : irBinOp .geq = none := rfl   -- not in the IR's operator set...
example : ofExpr (.binOp sp .geq (.ident sp "a") (.ident sp "b"))
        = some (.bin .le (.var "b") (.var "a")) := rfl   -- ...but carried by swapping
example : ofExpr (.binOp sp .gt (.ident sp "a") (.ident sp "b"))
        = some (.bin .lt (.var "b") (.var "a")) := rfl
example : ofExpr (.binOp sp .neq (.ident sp "a") (.ident sp "b"))
        = some (.un .not_ (.bin .eq (.var "a") (.var "b"))) := rfl
-- Bit ops ARE rejected: the bv sort exists but no transformation targets it.
example : irBinOp .bitand = none := rfl
example : ofExpr (.binOp sp .bitand (.ident sp "a") (.ident sp "b")) = none := rfl

-- And the translation composes with row 1: a carried `mod` subterm is then eliminated.
example : (ofExpr (.binOp sp .lt (.binOp sp .mod (.ident sp "a") (.ident sp "b"))
              (.intLit sp 10))).map TermIR.hasTmod = some true := rfl
example : (ofExpr (.binOp sp .lt (.binOp sp .mod (.ident sp "a") (.ident sp "b"))
              (.intLit sp 10))).map (TermIR.hasTmod ∘ TermIR.elimTmod) = some false := rfl

end Report
end Concrete
