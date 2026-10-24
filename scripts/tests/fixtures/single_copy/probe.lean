-- Bug 219: the extraction rules exist once. Run by scripts/tests/check_proofcore_callable_identity.sh
-- (`lake env lean`); prints one OK/FAIL line per check. Reads the ELABORATED environment, so
-- comments, spellings, `open`s and formatting cannot hide a copy or invent one.
import Concrete
open Lean Meta

namespace SingleCopyProbe

/-- Occurrences of constant `c` in EXECUTABLE positions of `e`: binder types are not visited, and
    an argument that is a type or a proof is skipped, so `h : e = .applyVar b xs` (a `match h :`
    equation) or a cast's motive does not count; only a term that builds a value does. -/
partial def countCtor (c : Name) (e : Expr) : MetaM Nat := do
  match e with
  | .const n _ => return if n == c then 1 else 0
  | .app .. =>
    let mut k ← countCtor c e.getAppFn
    for a in e.getAppArgs do
      if (← isProof a) || (← isType a) then continue
      k := k + (← countCtor c a)
    return k
  | .lam .. => lambdaTelescope e fun _ b => countCtor c b
  | .letE n t v b _ =>
    let kv ← countCtor c v
    withLetDecl n t v fun x => return kv + (← countCtor c (b.instantiate1 x))
  | .mdata _ b | .proj _ _ b => countCtor c b
  | _ => return 0

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

/-- Lean's generated body of a mutual well-founded block: `X._mutual`, exactly. -/
def isMutualBody : Name → Bool
  | .str _ "_mutual" => true
  | _ => false
/-- A per-function compiled copy: `X._unsafe_rec`, exactly. -/
def unsafeOwner? : Name → Option Name
  | .str p "_unsafe_rec" => some p
  | _ => none

/-- Construction counts of `c`, per declaration, among the definitions in `scope` (theorems and
    matchers excluded). A function and its own `_unsafe_rec` copy are one declaration (their counts
    must agree when both are non-zero). A `_mutual` body is not a declaration; instead its count must
    equal the sum over the block's members' `_unsafe_rec` copies, so nothing in the block hides. -/
def sites (c : Name) (scope : List Name) : MetaM (List (Name × Nat) × List String) := do
  let env ← getEnv
  let mut per : Std.HashMap Name Nat := {}
  let mut problems : List String := []
  let mut mutuals : List (Name × Nat) := []
  for n in scope do
    let some ci := env.find? n | continue
    if ci.isTheorem || isMatcherCore env n then continue
    let .defnInfo d := ci | continue
    let k ← countCtor c d.value
    if isMutualBody n then mutuals := (n, k) :: mutuals; continue
    if k == 0 then continue
    let owner := (unsafeOwner? n).getD n
    match per[owner]? with
    | some k' => if k' != k then problems := s!"{owner}: {k'} vs {k} in its compiled copy" :: problems
    | none => per := per.insert owner k
  for (m, k) in mutuals do
    -- members of the block: declarations whose value mentions the body
    let mem := scope.filter fun n => match env.find? n with
      | some (.defnInfo d) => (unsafeOwner? n).isNone && !isMutualBody n
          && (d.value.find? (·.isConstOf m)).isSome
      | _ => false
    let expected := mem.foldl (fun acc n => acc + (per[n]?.getD 0)) 0
    if k != expected then
      problems := s!"{m}: {k} constructions, but its members' compiled copies account for {expected}" :: problems
  return (per.toList, problems)

/-- Every definition declared under `pfx` (private ones included) reachable from `roots` through
    the values of definitions (and each one's compiled `_unsafe_rec` copy). -/
partial def reachable (pfx : Name) (roots : List Name) : MetaM (List Name) := do
  let env ← getEnv
  let rec go (todo : List Name) (seen : Std.HashSet Name) : Std.HashSet Name :=
    match todo with
    | [] => seen
    | n :: rest =>
      -- a private name is mangled (`_private.<module>.0.<name>`): test the name it was declared as
      let declared := (privateToUserName? n).getD n
      if seen.contains n || !pfx.isPrefixOf declared || (env.find? n).isNone then go rest seen else
      let next := match env.find? n with
        | some (.defnInfo d) => d.value.getUsedConstants.toList
        | _ => []
      go (next ++ [Name.str n "_unsafe_rec"] ++ rest) (seen.insert n)
  return (go roots {}).toList

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
/-- Two sites a name-based filter used to hide: a helper NAMED like generated code, and one under
    the extractor's own namespace. Both are reached from `extractor`. -/
def hidden_mutual_helper (b : String) : Proof.PExpr := Concrete.Proof.PExpr.applyVar b []
def extractor._extra (b : String) : Proof.PExpr := .applyVar b []
/-- A PRIVATE helper: its name is mangled, so a prefix test on the raw name would miss it. -/
private def privHelper (b : String) : Proof.PExpr := .applyVar b []
def extractor (b : String) : List Proof.PExpr :=
  [hidden_mutual_helper b, extractor._extra b, privHelper b]
/-- Patterns, including a `match h :` equation, are not constructions. -/
def patternOnly (e : Proof.PExpr) : String :=
  match _h : e with
  | .applyVar m _ => m
  | _ => ""
end SingleCopyProbe.Control

open SingleCopyProbe in
#eval show MetaM Unit from do
  let env ← getEnv
  let say (ok : Bool) (msg : String) : IO Unit := IO.println s!"{if ok then "OK" else "FAIL"} {msg}"
  let ctor := ``Concrete.Proof.PExpr.applyVar
  for (p, i) in [(``Concrete.cExprToPExpr, ``Concrete.cExprToPExprImpl),
                 (``Concrete.cStmtsToPExprK, ``Concrete.cStmtsToPExprKImpl)] do
    match ← delegation p i with
    | none => say true s!"{p} is exactly {i} (bare delegation, no compiler redirection)"
    | some why => say false why
  -- the scope: every definition of the extractor's module, and every Concrete definition the
  -- extractor can reach, in any module
  let some mod := env.getModuleIdx? `Concrete.Proof.ProofCore | throwError "no ProofCore module"
  let inModule := env.constants.toList.filterMap fun (n, _) =>
    if env.getModuleIdxFor? n == some mod then some n else none
  let reach ← reachable `Concrete [``Concrete.cExprToPExprImpl, ``Concrete.cStmtsToPExprKImpl]
  let scope := (inModule ++ reach).eraseDups
  let (real, probs) ← sites ctor scope
  say (probs.isEmpty) s!"every construction in a mutual block is attributed to a member ({probs})"
  say (real == [(``Concrete.cExprToPExprImpl, 1)])
    s!"PExpr.applyVar is constructed at exactly one site, once, in cExprToPExprImpl, across ProofCore and everything the extractor reaches ({reach.length} definitions) (found {real})"
  -- controls: each must be caught, for its own reason
  say ((← delegation ``Control.copyWrapper ``Concrete.cExprToPExprImpl).isSome)
    "CONTROL: a wrapper with an arm of its own is not a bare delegation"
  say ((← delegation ``Control.redirected ``Concrete.cExprToPExprImpl).any (·.endsWith "@[implemented_by]"))
    "CONTROL: an @[implemented_by] redirection is refused"
  let creach ← reachable `SingleCopyProbe.Control [``Control.extractor]
  let (ctl, _) ← sites ctor creach
  let ctlNames := (ctl.map fun (n, k) => ((privateToUserName? n).getD n, k)).toArray.qsort
    (fun a b => a.1.toString < b.1.toString) |>.toList
  say (ctlNames == [(``Control.extractor._extra, 1), (``Control.hidden_mutual_helper, 1),
                    (``Control.privHelper, 1)])
    s!"CONTROL: reachable helpers are found, whatever their names, private ones included (found {ctlNames})"
  let (pat, _) ← sites ctor [``Control.patternOnly]
  say pat.isEmpty s!"CONTROL: a `match h :` pattern is not a construction (found {pat})"
