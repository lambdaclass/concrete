-- Bug 219: the extraction rules exist once. Run by scripts/tests/check_proofcore_callable_identity.sh
-- (`lake env lean`); prints one OK/FAIL line per check. Reads the ELABORATED environment, so
-- comments, spellings, `open`s and formatting cannot hide a copy or invent one.
import Concrete
open Lean Meta

namespace SingleCopyProbe

/-- How many times `c` occurs as a constant in `e` (not deduplicated). -/
partial def countConst (c : Name) : Expr → Nat
  | .const n _ => if n == c then 1 else 0
  | .app f a => countConst c f + countConst c a
  | .lam _ t b _ | .forallE _ t b _ => countConst c t + countConst c b
  | .letE _ t v b _ => countConst c t + countConst c v + countConst c b
  | .mdata _ e | .proj _ _ e => countConst c e
  | _ => 0

/-- `none` when `pub` is exactly `impl` (its elaborated value eta-reduces to the constant) and no
    compiler redirection is attached to either; otherwise the reason. -/
def delegation (pub impl : Name) : MetaM (Option String) := do
  let env ← getEnv
  let some (.defnInfo d) := env.find? pub | return some s!"{pub} is not a definition"
  unless d.value.eta == .const impl [] do
    return some s!"{pub} is not a bare delegation to {impl}"
  for n in [pub, impl] do
    if (Compiler.implementedByAttr.getParam? env n).isSome then
      return some s!"{n} carries @[implemented_by]"
    if (Compiler.CSimp.ext.getState env).map.contains n then
      return some s!"{n} is replaced by a @[csimp] lemma"
  return none

/-- The declaration a compiler-generated constant belongs to (`f._unsafe_rec` -> `f`). -/
def root : Name → Name
  | .str p s => if s.startsWith "_" then root p else .str p s
  | n => n

/-- Construction sites of `PExpr.applyVar` among the definitions `inScope` selects: each owning
    declaration with its count. Patterns do not count (they compile to matchers / casesOn, skipped). -/
def sites (inScope : Name → Bool) : MetaM (List (Name × Nat)) := do
  let env ← getEnv
  let mut out : List (Name × Nat) := []
  for (n, ci) in env.constants.toList do
    unless inScope n do continue
    if ci.isTheorem || isMatcherCore env n then continue
    let s := n.toString
    if (s.splitOn "_sparseCasesOn").length > 1 || (s.splitOn "_mutual").length > 1 then continue
    if let .defnInfo d := ci then
      let k := countConst ``Concrete.Proof.PExpr.applyVar d.value
      if k > 0 then out := (root n, k) :: out
  -- a recursive definition appears twice (itself, and its compiled `_unsafe_rec`): keep one entry
  return out.foldl (fun acc (r, k) => if acc.any (·.1 == r) then acc else acc ++ [(r, k)]) []

end SingleCopyProbe

namespace SingleCopyProbe.Control
open Concrete
/-- A wrapper with an arm of its own (the pre-bug-219 shape). -/
def copyWrapper : CExpr → Option Proof.PExpr
  | .intLit n _ => some (.lit (.int n))
  | e => cExprToPExprImpl e
def copyImpl (e : CExpr) : Option Proof.PExpr := cExprToPExprImpl e
/-- A delegation whose COMPILED code is redirected to a copy. -/
@[implemented_by copyImpl] def redirected (e : CExpr) : Option Proof.PExpr := cExprToPExprImpl e
/-- A second construction site, fully qualified. -/
def extraSite (b : String) : Option Proof.PExpr := some (Concrete.Proof.PExpr.applyVar b [])
/-- A pattern on the constructor, not a construction. -/
def patternOnly : Proof.PExpr → Option String
  | .applyVar m _ => some m
  | _ => none
end SingleCopyProbe.Control

open SingleCopyProbe in
#eval show MetaM Unit from do
  let env ← getEnv
  let say (ok : Bool) (msg : String) : IO Unit := IO.println s!"{if ok then "OK" else "FAIL"} {msg}"
  for (p, i) in [(``Concrete.cExprToPExpr, ``Concrete.cExprToPExprImpl),
                 (``Concrete.cStmtsToPExprK, ``Concrete.cStmtsToPExprKImpl)] do
    match ← delegation p i with
    | none => say true s!"{p} is exactly {i} (bare delegation, no compiler redirection)"
    | some why => say false why
  let some mod := env.getModuleIdx? `Concrete.Proof.ProofCore | throwError "no ProofCore module"
  let real ← sites (fun n => env.getModuleIdxFor? n == some mod)
  say (real == [(``Concrete.cExprToPExprImpl, 1)])
    s!"PExpr.applyVar is constructed at exactly one site, in cExprToPExprImpl (found {real})"
  -- controls: each must be caught, for its own reason
  say ((← delegation ``Control.copyWrapper ``Concrete.cExprToPExprImpl).isSome)
    "CONTROL: a wrapper with an arm of its own is not a bare delegation"
  say ((← delegation ``Control.redirected ``Concrete.cExprToPExprImpl).any (·.endsWith "@[implemented_by]"))
    "CONTROL: an @[implemented_by] redirection is refused"
  let ctl ← sites (fun n => (`SingleCopyProbe.Control).isPrefixOf n)
  say (ctl == [(``Control.extraSite, 1)])
    s!"CONTROL: a qualified extra construction is found and a pattern is not (found {ctl})"
