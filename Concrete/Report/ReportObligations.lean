import Concrete.Elab.Core
import Concrete.Check.Layout
import Concrete.Resolve.FileSummary
import Concrete.Frontend.AST
import Concrete.Resolve.Intrinsic
import Concrete.Proof.Proof
import Concrete.Proof.ProofCore
import Concrete.IR.SSA
import Concrete.Report.Diagnostic
import Concrete.Frontend.Format
import Concrete.Report.ReportBase
import Concrete.Semantics.IntArith
import Concrete.Semantics.Capabilities
-- Obligation collectors need only the contract/VC helper cluster, not the
-- capability/arith/unsafe/layout report renderers (pipeline #34).
import Concrete.Report.ReportVC

namespace Concrete
namespace Report

-- Runtime-safety obligations: array bounds
-- ============================================================
-- A runtime-error class. Every `arr[idx]` into a fixed-size array generates the
-- obligation `0 ≤ idx < N`. Constant indices are evaluated (in-bounds / VIOLATION);
-- variable indices are discharged by `omega` under the function's #[requires]
-- (a kernel decision procedure — statically in bounds, no runtime check needed),
-- or left `unproven` (needs a precondition or a runtime check). This is the
-- runtime_checked evidence class.

mutual
/-- `(arrayName, indexExpr)` for every `arr[idx]` with an identifier base. -/
partial def collectIndexUsesE : Expr → List (String × Expr)
  | .arrayIndex _ (.ident _ arr) idx => (arr, idx) :: collectIndexUsesE idx
  | .arrayIndex _ a idx => collectIndexUsesE a ++ collectIndexUsesE idx
  | .binOp _ _ l r => collectIndexUsesE l ++ collectIndexUsesE r
  | .unaryOp _ _ x | .paren _ x | .borrow _ x | .borrowMut _ x | .deref _ x
  | .try_ _ x | .cast _ x _ | .fieldAccess _ x _ => collectIndexUsesE x
  | .arrayLit _ es => es.flatMap collectIndexUsesE
  | .call _ _ _ args => args.flatMap collectIndexUsesE
  | .methodCall _ o _ _ args => collectIndexUsesE o ++ args.flatMap collectIndexUsesE
  | .staticMethodCall _ _ _ _ args => args.flatMap collectIndexUsesE
  | .structLit _ _ _ fs base => fs.flatMap (fun (_, fe) => collectIndexUsesE fe) ++ (base.map collectIndexUsesE).getD []
  | .enumLit _ _ _ _ fs => fs.flatMap (fun (_, fe) => collectIndexUsesE fe)
  | .allocCall _ x a => collectIndexUsesE x ++ collectIndexUsesE a
  | .ifExpr _ c t el =>
      collectIndexUsesE c ++ t.flatMap collectIndexUsesS ++ el.flatMap collectIndexUsesS
  | .match_ _ s _ => collectIndexUsesE s
  | _ => []
partial def collectIndexUsesS : Stmt → List (String × Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v => collectIndexUsesE v
  | .return_ _ (some v) => collectIndexUsesE v
  | .ifElse _ c t el => collectIndexUsesE c ++ t.flatMap collectIndexUsesS ++ (el.getD []).flatMap collectIndexUsesS
  | .while_ _ c b _ => collectIndexUsesE c ++ b.flatMap collectIndexUsesS
  | .forLoop _ init c step b _ =>
      (init.map collectIndexUsesS).getD [] ++ collectIndexUsesE c
        ++ (step.map collectIndexUsesS).getD [] ++ b.flatMap collectIndexUsesS
  | .fieldAssign _ o _ v | .derefAssign _ o v => collectIndexUsesE o ++ collectIndexUsesE v
  | .arrayIndexAssign _ (.ident _ arr) idx v => (arr, idx) :: (collectIndexUsesE idx ++ collectIndexUsesE v)
  | .arrayIndexAssign _ a i v => collectIndexUsesE a ++ collectIndexUsesE i ++ collectIndexUsesE v
  | _ => []
end

-- ============================================================
-- Loop-invariant scope for runtime-safety obligations
-- ============================================================
-- A runtime-safety obligation that occurs inside a loop body may ASSUME the
-- loop's invariant and guard: at the top of the body the invariant holds and
-- the guard was just taken. Feeding those facts to omega lets a body access
-- like `a[i]` discharge from `#[invariant(0 <= i && i <= N)]` + guard `i < N`,
-- instead of demanding a `#[requires]`. SOUNDNESS: the invariant only provably
-- holds until the body mutates a variable it mentions, so the ordered walk
-- below DROPS a hypothesis as soon as a statement assigns to one of its
-- variables (array-element / field / deref stores touch no integer counter, so
-- they invalidate nothing; the canonical `a[i] = …; i = i + 1` therefore keeps
-- the bound at the access and loses it only for statements after the `i = …`).

/-- Loop invariants + guard for the loop whose statement begins on `line`
    (matched against `FnDef.loopContracts` by source line). The facts assumable
    for an obligation in that loop's body. -/
def loopHypsAt (lcs : List LoopContract) (line : Nat) : List Expr :=
  match lcs.find? (·.line == line) with
  | some lc => lc.invariants ++ lc.guard.toList
  | none    => []

/-- Scalar variables a statement assigns to. Mutating one invalidates any
    in-scope hypothesis that mentions it. Array-element / field / deref stores
    assign no integer counter (the domain our invariants range over), so they
    return `[]`; a nested loop invalidates whatever its body assigns. -/
partial def assignedScalarsS : Stmt → List String
  | .assign _ n _ => [n]
  | .letDecl _ n _ _ _ _ => [n]
  | .ifElse _ _ t el => t.flatMap assignedScalarsS ++ (el.getD []).flatMap assignedScalarsS
  | .while_ _ _ b _ => b.flatMap assignedScalarsS
  | .forLoop _ init _ step b _ =>
      (init.map assignedScalarsS).getD [] ++ (step.map assignedScalarsS).getD []
        ++ b.flatMap assignedScalarsS
  | _ => []

/-- Drop every in-scope hypothesis that mentions a just-assigned variable. -/
def dropStaleHyps (scope : List Expr) (assigned : List String) : List Expr :=
  scope.filter fun h => (collectIdents h).all (fun v => !assigned.contains v)

-- ────────────────────────────────────────────────────────────────────────────
-- The ONE scoped context collector (ROADMAP Phase 3 #3).
--
-- Shared engine for every scoped obligation family (call-site preconditions,
-- array bounds, div/mod, overflow, asserts, …). Migrated families walk the body
-- once with the SAME scope-threading discipline, parameterised only by a
-- per-statement `leaf` extractor. The walker threads the full fact set the
-- roadmap requires:
--   • enclosing `if`-guards          → the then-branch assumes `c`,
--   • negated guards                 → the else-branch assumes `¬c`,
--   • early-return fall-through       → after `if c { …return… }`, assume `¬c`,
--   • loop invariants + guards        → the body assumes `loopHypsAt`,
--   • one shared invalidation rule    → `dropStaleHyps` after each assignment.
-- `leaf scope s` sees the hypotheses in scope at `s` and returns this statement's
-- own (non-recursive) obligations; the walker owns ALL recursion into branches,
-- loop bodies, and for-loop init/step, so no family can drift in how it threads
-- scope. Migrated families instantiate this with their own leaf (Phase 3 #4-9).
mutual
partial def scopedWalkS {α} (leaf : List Expr → Stmt → List α)
    (lcs : List LoopContract) (scope : List Expr) : Stmt → List α
  | s@(.ifElse _ c t el) =>
      leaf scope s
        ++ scopedWalkB leaf lcs (scope ++ [c]) t
        ++ scopedWalkB leaf lcs (scope ++ (negateGuard c).toList) (el.getD [])
  | s@(.while_ sp _ b _) =>
      leaf scope s ++ scopedWalkB leaf lcs (scope ++ loopHypsAt lcs sp.line) b
  | s@(.forLoop sp init _ step b _) =>
      -- init, then this statement's own leaves (the loop condition), then step,
      -- then body — the traversal ORDER every family's old walker used, so the
      -- positional `#idx`/`#pre`/… keys are preserved across the migration.
      ((init.map (scopedWalkS leaf lcs scope)).getD [])
        ++ leaf scope s
        ++ ((step.map (scopedWalkS leaf lcs scope)).getD [])
        ++ scopedWalkB leaf lcs (scope ++ loopHypsAt lcs sp.line) b
  | s@(.borrowIn _ _ _ _ _ b) => leaf scope s ++ scopedWalkB leaf lcs scope b
  | s => leaf scope s
partial def scopedWalkB {α} (leaf : List Expr → Stmt → List α)
    (lcs : List LoopContract) (scope : List Expr) : List Stmt → List α
  | [] => []
  | s :: rest =>
      let restScope := match s with
        | .ifElse _ c t none => if blockTerminates t then scope ++ (negateGuard c).toList
                                else dropStaleHyps scope (assignedScalarsS s)
        | _ => dropStaleHyps scope (assignedScalarsS s)
      scopedWalkS leaf lcs scope s ++ scopedWalkB leaf lcs restScope rest
end

/-- Array-index leaf: the index uses in a statement's OWN expression positions
    (the walker owns recursion into branches/loops/init/step, so
    `.ifElse`/`.while_`/`.forLoop` contribute only their condition's index uses).
    A store `a[idx] = v` carries its target bound `(a, idx)` FIRST, matching the
    old walker's ordering exactly. -/
def boundsLeaf (scope : List Expr) : Stmt → List (String × Expr × List Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v =>
      (collectIndexUsesE v).map fun (a, i) => (a, i, scope)
  | .return_ _ (some v) => (collectIndexUsesE v).map fun (a, i) => (a, i, scope)
  | .ifElse _ c _ _ => (collectIndexUsesE c).map fun (a, i) => (a, i, scope)
  | .while_ _ c _ _ => (collectIndexUsesE c).map fun (a, i) => (a, i, scope)
  | .forLoop _ _ c _ _ _ => (collectIndexUsesE c).map fun (a, i) => (a, i, scope)
  | .fieldAssign _ o _ v | .derefAssign _ o v =>
      (collectIndexUsesE o ++ collectIndexUsesE v).map fun (a, i) => (a, i, scope)
  | .arrayIndexAssign _ (.ident _ arr) idx v =>
      (arr, idx, scope) :: (collectIndexUsesE idx ++ collectIndexUsesE v).map fun (a, i) => (a, i, scope)
  | .arrayIndexAssign _ a i v =>
      (collectIndexUsesE a ++ collectIndexUsesE i ++ collectIndexUsesE v).map fun (x, j) => (x, j, scope)
  | _ => []

/-- Index uses paired with the hypotheses in scope at the access (Phase 3 #5 —
    migrated onto the unified `scopedWalk`). Like the call-site migration, the
    collector now threads enclosing `if`-guards (then assumes `c`, else assumes
    `¬c`), early-return fall-through, and loop invariants/guards — strictly more
    sound context than the old bounds walker, so a bounds obligation can only move
    `unproven → proved_by_kernel_decision` (e.g. `if 0 ≤ i && i < n { a[i] }`),
    never the reverse, and a reassigned index still drops its stale guard. -/
def scopedBoundsB (lcs : List LoopContract) (scope : List Expr) (body : List Stmt) :
    List (String × Expr × List Expr) :=
  scopedWalkB boundsLeaf lcs scope body

/-- Call-site leaf: the calls in a statement's OWN expression positions (the
    walker owns recursion into branches, loop bodies, and for-loop init/step, so
    `.ifElse`/`.while_`/`.forLoop` contribute only their condition's calls). -/
def callLeaf (scope : List Expr) : Stmt → List (String × List Expr × List Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v =>
      (collectCallsE v).map fun (_, fn, args) => (fn, args, scope)
  | .return_ _ (some v) => (collectCallsE v).map fun (_, fn, args) => (fn, args, scope)
  | .ifElse _ c _ _ => (collectCallsE c).map fun (_, fn, args) => (fn, args, scope)
  | .while_ _ c _ _ => (collectCallsE c).map fun (_, fn, args) => (fn, args, scope)
  | .forLoop _ _ c _ _ _ => (collectCallsE c).map fun (_, fn, args) => (fn, args, scope)
  | .fieldAssign _ o _ v | .derefAssign _ o v =>
      (collectCallsE o ++ collectCallsE v).map fun (_, fn, args) => (fn, args, scope)
  | .arrayIndexAssign _ a i v =>
      (collectCallsE a ++ collectCallsE i ++ collectCallsE v).map fun (_, fn, args) => (fn, args, scope)
  | _ => []

/-- Calls paired with the hypotheses in scope at the call (Phase 3 #4 — migrated
    onto the unified `scopedWalk`). The collector threads the FULL fact set:
    enclosing `if`-guards (then assumes `c`, else assumes `¬c`), early-return
    fall-through (`¬c` after `if c { …return… }`), loop invariants/guards, and one
    shared stale-hypothesis rule. This is strictly more (sound) context than the
    old call walker threaded, so a call precondition can only move `unproven →
    proved`, never the reverse; it also closes the old gap of skipping calls
    inside `borrow … in { }` bodies. -/
def scopedCallsB (lcs : List LoopContract) (scope : List Expr) (body : List Stmt) :
    List (String × List Expr × List Expr) :=
  scopedWalkB callLeaf lcs scope body

/-- `assert`/`assume` leaf: the only own-obligation statements are `assert`/
    `assume` themselves; everything else is pure recursion the walker owns. -/
def assertLeaf (scope : List Expr) : Stmt → List (Bool × Expr × List Expr)
  | .assert_ _ c => [(false, c, scope)]
  | .assume_ _ c => [(true, c, scope)]
  | _ => []

/-- `assert`/`assume` with the path conditions in scope at each one (the first
    family on the unified collector — Phase 3 #3). Enclosing `if`-guards (the
    guard on the then-branch, its negation on the else-branch), loop invariants in
    the body, and `¬c` for the fall-through of an early-return `if c { …return… }`.
    Mirrors `collectAssertAssumeS`'s traversal ORDER exactly, so the `i`-th item
    keeps the same `#aa<i>` key the renderer uses. -/
def scopedAssertsB (lcs : List LoopContract) (scope : List Expr) (body : List Stmt) :
    List (Bool × Expr × List Expr) :=
  scopedWalkB assertLeaf lcs scope body

/-- Omega goals for `assert(e)` obligations: `∀ vars, (path conditions) → (e)`.
    The hypotheses are the function's `#[requires]` PLUS the path conditions in
    scope at the assert (if-guards, negated guards, loop invariants) — threaded
    like the call-site/bounds/div VCs. Discharged by the same omega backend; a
    success means the assert holds. `assume` produces no goal (trusted, not
    proved). Keyed `<fq>#aa<i>` by position in the assert/assume stream, matching
    `renderAssertAssume`. -/
def assertGoals (modules : List Module) : List (String × String) := Id.run do
  let mut goals : List (String × String) := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    let mut i := 0
    for (isAssume, cond, scope) in scopedAssertsB f.loopContracts [] f.body do
      if !isAssume then
        let hyps := f.requires ++ scope
        let nn := nonNegFromHyps hyps
        match toLeanPropSound nn cond with
        | some p =>
          let hypProps := hyps.filterMap (toLeanPropSound nn)
          let vars := (collectIdents cond ++ hyps.flatMap collectIdents).eraseDups
          let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
          let hyp := if hypProps.isEmpty then "" else s!"({" ∧ ".intercalate (hypProps.map (fun q => s!"({q})"))}) → "
          goals := goals ++ [(s!"{fq}#aa{i}", s!"{binder}{hyp}({p})")]
        | none => pure ()
      i := i + 1
  return goals

/-- `assume(e)` facts with the path conditions in scope (Phase 3 #8). An `assume`
    is NOT an obligation to discharge — it is a trusted assumption fact that the
    audit ledger must surface loudly: it carries status `assumed` (never a proof),
    is a policy input (`forbid-assume`, E0614), and crucially produces no goal, so
    it can never launder trust into kernel evidence for a later assert. Keyed
    `<fq>#aa<i>` by the SAME position scheme as `assertGoals` (assume and assert
    occupy disjoint positions in the shared stream). -/
def assumeFacts (modules : List Module) : List (String × String × List String) := Id.run do
  let mut facts : List (String × String × List String) := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    let mut i := 0
    for (isAssume, cond, scope) in scopedAssertsB f.loopContracts [] f.body do
      if isAssume then
        let hyps := f.requires ++ scope
        let nn := nonNegFromHyps hyps
        let concl := (toLeanPropSound nn cond).getD (Concrete.fmtExpr cond)
        facts := facts ++ [(s!"{fq}#aa{i}", concl, hyps.filterMap (toLeanPropSound nn))]
      i := i + 1
  return facts

/-- Build the ordered list of call-site obligations across all callers. The fast
    constant folder classifies the literal/arithmetic cases; an obligation that
    stays non-constant carries a `bv_decide` `leanGoal` (when closed after
    let-const subst) and the `hyps` in scope at the call so `callPrecondGoals`
    can try to discharge it with `omega` from the caller's `#[requires]` /
    enclosing guards / loop invariants. -/
def callSiteObligations (modules : List Module) : List CallObligation := Id.run do
  let fns := modules.flatMap allFunctions
  let reqMap : List (String × (List Param × List Expr)) :=
    fns.filterMap (fun (_, f) => if f.requires.isEmpty then none else some (f.name, (f.params, f.requires)))
  if reqMap.isEmpty then return []
  let mut obs : List CallObligation := []
  let mut gi := 0
  for (pfx, f) in fns do
    let lets := letConstMap f.body
    for (fn, args, scope) in scopedCallsB f.loopContracts [] f.body do
      match reqMap.find? (·.1 == fn) with
      | none => pure ()
      | some (_, (params, reqs)) =>
        let argSubst := (params.zip args).map (fun (p, a) => (p.name, a))
        let callStr := s!"{fn}({", ".intercalate (args.map Concrete.fmtExpr)})"
        for r in reqs do
          let spec := substContract argSubst r
          let (baseStatus, leanGoal) := match cEvalBool spec with
            | some true  => ("proved_at_callsite", none)
            | some false => ("failed_at_callsite", none)
            | none =>
              let spec2 := substContract lets spec
              match cEvalBool spec2 with
              | some false => ("failed_at_callsite", none)
              | _ => if isClosed spec2 then ("unproven", toLeanBV spec2)
                     else ("unproven", none)
          obs := obs ++ [{ caller := pfx ++ f.name, callStr, specExpr := spec, baseStatus, leanGoal
                         , key := s!"{pfx ++ f.name}#pre{gi}", hyps := f.requires ++ scope }]
          gi := gi + 1
  return obs

/-- Omega goals for call-site preconditions that are not constant-decidable:
    `∀ (vars : Int), (caller hyps) → precondition`. Discharged by the same
    `omega` backend as the bounds/div goals; a success means the caller's
    `#[requires]` / guards / loop invariants establish the callee's precondition. -/
def callPrecondGoals (modules : List Module) : List (String × String) :=
  (callSiteObligations modules).filterMap fun o =>
    if o.baseStatus != "unproven" then none
    else match toLeanProp o.specExpr with
      | none => none
      | some specStr =>
        let vars := (collectIdents o.specExpr ++ o.hyps.flatMap collectIdents).eraseDups
        let reqs := o.hyps.filterMap toLeanProp
        let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
        let hyp := if reqs.isEmpty then "" else s!"({" ∧ ".intercalate reqs}) → "
        some (o.key, s!"{binder}{hyp}({specStr})")

/-- One array-bounds obligation. `closedVerdict` is set when the index is a
    compile-time constant; otherwise `leanGoal` is the omega goal (if lowerable). -/
structure BoundsObl where
  fnQual        : String
  key           : String
  arrName       : String
  idxExpr       : Expr
  size          : Nat
  closedVerdict : Option Bool
  leanGoal      : Option String

/-- Identifier → fixed-array size, from array-typed params and annotated lets. -/
def arraySizeMap (f : FnDef) : List (String × Nat) :=
  let ps := f.params.filterMap fun p => match p.ty with | .array _ n => some (p.name, n) | _ => none
  let ls := f.body.filterMap fun s => match s with
    | .letDecl _ nm _ (some (.array _ n)) _ _ => some (nm, n) | _ => none
  ps ++ ls

/-- Generate array-bounds obligations for every indexed access into a known
    fixed-size array. -/
def boundsObligations (modules : List Module) : List BoundsObl := Id.run do
  let mut out : List BoundsObl := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    let sizes := arraySizeMap f
    let mut i := 0
    for (arr, idx, scope) in scopedBoundsB f.loopContracts [] f.body do
      match sizes.find? (·.1 == arr) with
      | none => pure ()
      | some (_, n) =>
        let key := s!"{fq}#bounds{i}"
        let cv : Option Bool × Option String := match cEvalInt idx with
          | some k => (some (decide (0 ≤ k) && decide (k < (Int.ofNat n))), none)
          | none => match toLeanProp idx with
            | none => (none, none)
            | some idxStr =>
              let hyps := f.requires ++ scope
              let vars := (collectIdents idx ++ hyps.flatMap collectIdents).eraseDups
              let reqs := hyps.filterMap toLeanProp
              let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
              let hyp := if reqs.isEmpty then "" else s!"({" ∧ ".intercalate reqs}) → "
              (none, some s!"{binder}{hyp}(0 ≤ {idxStr} ∧ {idxStr} < {n})")
        out := out ++ [{ fnQual := fq, key, arrName := arr, idxExpr := idx, size := n
                        , closedVerdict := cv.1, leanGoal := cv.2 }]
        i := i + 1
  return out

/-- Lean goals for the non-constant bounds obligations, for omega discharge. -/
def boundsGoals (modules : List Module) : List (String × String) :=
  (boundsObligations modules).filterMap fun o => o.leanGoal.map (fun g => (o.key, g))

/-- Render the array-bounds section. `provedKeys` are the omega-discharged keys. -/
def renderBounds (obls : List BoundsObl) (provedKeys : List String) : String := Id.run do
  if obls.isEmpty then return ""
  let mut out := "\n\n=== Runtime-safety obligations (array bounds) ==="
  let mut cur := ""
  for o in obls do
    if o.fnQual != cur then out := out ++ s!"\n\n{o.fnQual}"; cur := o.fnQual
    let status := match o.closedVerdict with
      | some true  => "checked: in bounds (constant index)"
      | some false => "VIOLATION: index out of bounds (constant index)"
      | none => match o.leanGoal with
        | some _ =>
          if provedKeys.contains o.key
          then "proved_by_kernel_decision (omega) — statically in bounds, no runtime check needed"
          else "unproven — bound the index with a #[requires], or insert a runtime check"
        | none => "unproven — index not statically analyzable; needs a runtime check"
    out := out ++ s!"\n  {o.arrName}[{Concrete.fmtExpr o.idxExpr}]  (array size {o.size})\n    status: {status}"
  return out ++ "\n"

-- ============================================================
-- Runtime-safety obligations: division by zero
-- ============================================================
-- Every `/` and `%` generates the obligation `divisor ≠ 0`. Same shape as array
-- bounds: constant divisors are evaluated; variable divisors discharge by omega
-- under the function's #[requires] (statically nonzero), or stay `unproven`.

mutual
/-- `(isMod, divisorExpr)` for every `/` and `%` in an expression. -/
partial def collectDivisorsE : Expr → List (Bool × Expr)
  | .binOp _ .div l r => (false, r) :: (collectDivisorsE l ++ collectDivisorsE r)
  | .binOp _ .mod l r => (true, r) :: (collectDivisorsE l ++ collectDivisorsE r)
  | .binOp _ _ l r => collectDivisorsE l ++ collectDivisorsE r
  | .unaryOp _ _ x | .paren _ x | .borrow _ x | .borrowMut _ x | .deref _ x
  | .try_ _ x | .cast _ x _ | .fieldAccess _ x _ => collectDivisorsE x
  | .arrayLit _ es => es.flatMap collectDivisorsE
  | .arrayIndex _ a i => collectDivisorsE a ++ collectDivisorsE i
  | .call _ _ _ args => args.flatMap collectDivisorsE
  | .methodCall _ o _ _ args => collectDivisorsE o ++ args.flatMap collectDivisorsE
  | .staticMethodCall _ _ _ _ args => args.flatMap collectDivisorsE
  | .structLit _ _ _ fs base => fs.flatMap (fun (_, fe) => collectDivisorsE fe) ++ (base.map collectDivisorsE).getD []
  | .enumLit _ _ _ _ fs => fs.flatMap (fun (_, fe) => collectDivisorsE fe)
  | .allocCall _ x a => collectDivisorsE x ++ collectDivisorsE a
  | .ifExpr _ c t el =>
      collectDivisorsE c ++ t.flatMap collectDivisorsS ++ el.flatMap collectDivisorsS
  | .match_ _ s _ => collectDivisorsE s
  | _ => []
partial def collectDivisorsS : Stmt → List (Bool × Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v => collectDivisorsE v
  | .return_ _ (some v) => collectDivisorsE v
  | .ifElse _ c t el => collectDivisorsE c ++ t.flatMap collectDivisorsS ++ (el.getD []).flatMap collectDivisorsS
  | .while_ _ c b _ => collectDivisorsE c ++ b.flatMap collectDivisorsS
  | .forLoop _ init c step b _ =>
      (init.map collectDivisorsS).getD [] ++ collectDivisorsE c
        ++ (step.map collectDivisorsS).getD [] ++ b.flatMap collectDivisorsS
  | .fieldAssign _ o _ v | .derefAssign _ o v => collectDivisorsE o ++ collectDivisorsE v
  | .arrayIndexAssign _ a i v => collectDivisorsE a ++ collectDivisorsE i ++ collectDivisorsE v
  | _ => []
end

/-- Divisor leaf: the `/`/`%` divisors in a statement's OWN expression positions
    (the walker owns recursion into branches/loops/init/step, so
    `.ifElse`/`.while_`/`.forLoop` contribute only their condition's divisors).
    Each item is `(isMod, divisorExpr, scope)`. -/
def divLeaf (scope : List Expr) : Stmt → List (Bool × Expr × List Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v =>
      (collectDivisorsE v).map fun (m, e) => (m, e, scope)
  | .return_ _ (some v) => (collectDivisorsE v).map fun (m, e) => (m, e, scope)
  | .ifElse _ c _ _ => (collectDivisorsE c).map fun (m, e) => (m, e, scope)
  | .while_ _ c _ _ => (collectDivisorsE c).map fun (m, e) => (m, e, scope)
  | .forLoop _ _ c _ _ _ => (collectDivisorsE c).map fun (m, e) => (m, e, scope)
  | .fieldAssign _ o _ v | .derefAssign _ o v =>
      (collectDivisorsE o ++ collectDivisorsE v).map fun (m, e) => (m, e, scope)
  | .arrayIndexAssign _ a i v =>
      (collectDivisorsE a ++ collectDivisorsE i ++ collectDivisorsE v).map fun (m, e) => (m, e, scope)
  | _ => []

/-- Divisor uses paired with the hypotheses in scope at the `/`/`%` (Phase 3 #6 —
    migrated onto the unified `scopedWalk`). The collector threads enclosing
    guards / negated guards / fall-through / loop invariants, so a `divisor ≠ 0`
    obligation can only move `unproven → proved` (e.g. `if d != 0 { n / d }`),
    never the reverse. The SOUND division/modulo lowering is unchanged: it still
    flows through `divSound`/`toLeanPropSound`, which lower `/`/`%` to Lean
    E-division ONLY when the dividend is provably non-negative — keeping Concrete's
    truncating semantics from being confused with Lean's floor division. -/
def scopedDivB (lcs : List LoopContract) (scope : List Expr) (body : List Stmt) :
    List (Bool × Expr × List Expr) :=
  scopedWalkB divLeaf lcs scope body

/-- One division-by-zero obligation. -/
structure DivObl where
  fnQual        : String
  key           : String
  divExpr       : Expr
  isMod         : Bool
  closedVerdict : Option Bool
  leanGoal      : Option String

/-- Generate `divisor ≠ 0` obligations for every `/` and `%`. -/
def divObligations (modules : List Module) : List DivObl := Id.run do
  let mut out : List DivObl := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    let mut i := 0
    for (isMod, dv, scope) in scopedDivB f.loopContracts [] f.body do
      let key := s!"{fq}#div{i}"
      let cv : Option Bool × Option String := match cEvalInt dv with
        | some k => (some (decide (k ≠ 0)), none)
        | none => match toLeanProp dv with
          | none => (none, none)
          | some dStr =>
            let hyps := f.requires ++ scope
            let vars := (collectIdents dv ++ hyps.flatMap collectIdents).eraseDups
            let reqs := hyps.filterMap toLeanProp
            let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
            let hyp := if reqs.isEmpty then "" else s!"({" ∧ ".intercalate reqs}) → "
            (none, some s!"{binder}{hyp}({dStr} ≠ 0)")
      out := out ++ [{ fnQual := fq, key, divExpr := dv, isMod, closedVerdict := cv.1, leanGoal := cv.2 }]
      i := i + 1
  return out

/-- Lean goals for the non-constant divisor obligations, for omega discharge. -/
def divGoals (modules : List Module) : List (String × String) :=
  (divObligations modules).filterMap fun o => o.leanGoal.map (fun g => (o.key, g))

/-- Render the division-by-zero section. -/
def renderDiv (obls : List DivObl) (provedKeys : List String) : String := Id.run do
  if obls.isEmpty then return ""
  let mut out := "\n\n=== Runtime-safety obligations (division: non-zero divisor) ==="
  let mut cur := ""
  for o in obls do
    if o.fnQual != cur then out := out ++ s!"\n\n{o.fnQual}"; cur := o.fnQual
    let opname := if o.isMod then "%" else "/"
    let status := match o.closedVerdict with
      | some true  => "checked: divisor is a nonzero constant"
      | some false => "VIOLATION: division by zero (constant divisor)"
      | none => match o.leanGoal with
        | some _ =>
          if provedKeys.contains o.key
          then "proved_by_kernel_decision (omega) — divisor nonzero, no runtime check needed"
          else "unproven — require the divisor nonzero (#[requires]), or insert a runtime check"
        | none => "unproven — divisor not statically analyzable; needs a runtime check"
    out := out ++ s!"\n  {opname} divisor {Concrete.fmtExpr o.divExpr}\n    status: {status}"
  return out ++ "\n"

-- ============================================================
-- Runtime-safety obligations: integer overflow (opt-in)
-- ============================================================
-- Under `#[overflow_checked]`, each fixed-width `+`/`-`/`*` generates
-- `MIN ≤ result ≤ MAX` for that width. Opt-in, because Concrete's default
-- integer overflow semantics are profile-dependent and emitting this for every
-- arithmetic op would flood the audit. Same disposition shape as bounds/div.

/-- Inclusive value range of a *fixed-width* integer type (none = arbitrary/
    `Int`). The range values come from the arithmetic reference
    (`IntArith.intRange`); this deliberately keeps `Int`/`Uint` as `none` (an
    audit choice — their overflow is profile-dependent, per the note above), so
    it is not a blind alias of `IntArith.intRange`, which does give them ranges. -/
def intRange : Ty → Option (Int × Int)
  | .int | .uint => none
  | ty => IntArith.intRange ty

/-- Best-effort fixed-width int type of an expression, from a var→type map. -/
partial def exprIntTy (vt : List (String × Ty)) : Expr → Option Ty
  | .ident _ n => vt.lookup n
  | .paren _ e => exprIntTy vt e
  | .unaryOp _ _ e => exprIntTy vt e
  | .cast _ _ t => some t
  | .binOp _ _ l r => match exprIntTy vt l with | some t => some t | none => exprIntTy vt r
  | _ => none

mutual
/-- Every `+`/`-`/`*` binop node in an expression (the whole `a op b`). -/
partial def collectArithE : Expr → List Expr
  | e@(.binOp _ op l r) =>
    let here := match op with | .add | .sub | .mul => [e] | _ => []
    here ++ collectArithE l ++ collectArithE r
  | .unaryOp _ _ x | .paren _ x | .borrow _ x | .borrowMut _ x | .deref _ x
  | .try_ _ x | .cast _ x _ | .fieldAccess _ x _ => collectArithE x
  | .arrayLit _ es => es.flatMap collectArithE
  | .arrayIndex _ a i => collectArithE a ++ collectArithE i
  | .call _ _ _ args => args.flatMap collectArithE
  | .methodCall _ o _ _ args => collectArithE o ++ args.flatMap collectArithE
  | .staticMethodCall _ _ _ _ args => args.flatMap collectArithE
  | .structLit _ _ _ fs base => fs.flatMap (fun (_, fe) => collectArithE fe) ++ (base.map collectArithE).getD []
  | .enumLit _ _ _ _ fs => fs.flatMap (fun (_, fe) => collectArithE fe)
  | .allocCall _ x a => collectArithE x ++ collectArithE a
  | .ifExpr _ c t el =>
      collectArithE c ++ t.flatMap collectArithS ++ el.flatMap collectArithS
  | .match_ _ s _ => collectArithE s
  | _ => []
partial def collectArithS : Stmt → List Expr
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v => collectArithE v
  | .return_ _ (some v) => collectArithE v
  | .ifElse _ c t el => collectArithE c ++ t.flatMap collectArithS ++ (el.getD []).flatMap collectArithS
  | .while_ _ c b _ => collectArithE c ++ b.flatMap collectArithS
  | .forLoop _ init c step b _ =>
      (init.map collectArithS).getD [] ++ collectArithE c
        ++ (step.map collectArithS).getD [] ++ b.flatMap collectArithS
  | .fieldAssign _ o _ v | .derefAssign _ o v => collectArithE o ++ collectArithE v
  | .arrayIndexAssign _ a i v => collectArithE a ++ collectArithE i ++ collectArithE v
  | _ => []
end

/-- Arithmetic-op leaf: the `+`/`-`/`*` op nodes in a statement's OWN expression
    positions (the walker owns recursion into branches/loops/init/step, so
    `.ifElse`/`.while_`/`.forLoop` contribute only their condition's op nodes). -/
def arithLeaf (scope : List Expr) : Stmt → List (Expr × List Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v =>
      (collectArithE v).map fun e => (e, scope)
  | .return_ _ (some v) => (collectArithE v).map fun e => (e, scope)
  | .ifElse _ c _ _ => (collectArithE c).map fun e => (e, scope)
  | .while_ _ c _ _ => (collectArithE c).map fun e => (e, scope)
  | .forLoop _ _ c _ _ _ => (collectArithE c).map fun e => (e, scope)
  | .fieldAssign _ o _ v | .derefAssign _ o v =>
      (collectArithE o ++ collectArithE v).map fun e => (e, scope)
  | .arrayIndexAssign _ a i v =>
      (collectArithE a ++ collectArithE i ++ collectArithE v).map fun e => (e, scope)
  | _ => []

/-- Arithmetic-op nodes paired with the hypotheses in scope (Phase 3 #7 —
    migrated onto the unified `scopedWalk`). The collector now threads enclosing
    guards / negated guards / fall-through / loop invariants into the in-scope
    facts, so an overflow obligation's interval/`bv_decide`/SMT discharge sees
    strictly MORE sound bounds — proofs can only get stronger
    (`unproven → proved`), never weaker. The three-route discharge downstream is
    unchanged: omega/interval/`bv_decide` are kernel-owned, external SMT remains
    opt-in (`--smt`) and may only touch obligations the kernel tiers left
    unproved; stale bounds are still dropped by the shared invalidation rule. -/
def scopedArithB (lcs : List LoopContract) (scope : List Expr) (body : List Stmt) :
    List (Expr × List Expr) :=
  scopedWalkB arithLeaf lcs scope body

/-- One integer-overflow obligation. -/
structure OverflowObl where
  fnQual        : String
  key           : String
  opExpr        : Expr
  lo            : Int
  hi            : Int
  closedVerdict : Option Bool
  leanGoal      : Option String           -- omega goal (linear)
  bvGoal        : Option String := none    -- widened bv_decide goal (nonlinear, interval-gated)
  hyps          : List Expr := []          -- the in-scope #[requires]/guards (for the SMT query)

/-- All annotated `let` bindings in a statement tree, including those declared
    inside loop inits/steps/bodies (so a loop counter `i` from a `for`-init is
    typed for the overflow check). -/
partial def collectLetTys : Stmt → List (String × Ty)
  | .letDecl _ n _ (some t) _ _ => [(n, t)]
  | .ifElse _ _ t el => t.flatMap collectLetTys ++ (el.getD []).flatMap collectLetTys
  | .while_ _ _ b _ => b.flatMap collectLetTys
  | .forLoop _ init _ step b _ =>
      (init.map collectLetTys).getD [] ++ (step.map collectLetTys).getD [] ++ b.flatMap collectLetTys
  | _ => []

/-- Var→type map from params and annotated lets (recursively, see
    `collectLetTys`). -/
def varTyMap (f : FnDef) : List (String × Ty) :=
  f.params.map (fun p => (p.name, p.ty)) ++ f.body.flatMap collectLetTys

-- ============================================================
-- Nonlinear overflow discharge: interval analysis + bv_decide
-- ============================================================
-- omega is LINEAR, so a product of two variables (`sample * gain`) is left
-- `unproven` by the omega path. When every operand has a non-negative, bounded
-- range (from #[requires] / loop invariants), interval analysis computes the
-- result range; if it fits the type, we emit a WIDENED unsigned `bv_decide`
-- goal so the no-overflow fact is KERNEL-checked (not merely computed here).
-- Restricted to +/* of non-negative bounded operands so the unsigned model is
-- sound (no underflow); anything else stays honestly `unproven`.

/-- Flatten an `&&`-conjunction into its conjuncts. -/
partial def conjuncts : Expr → List Expr
  | .binOp _ .and_ l r => conjuncts l ++ conjuncts r
  | .paren _ e => conjuncts e
  | e => [e]

/-- Per-variable integer bounds `[lo,hi]` from hypothesis conjuncts of the form
    `k <= v`, `v <= k`, `k < v`, `v < k` (and ≥/> mirrors), merged to the
    tightest known bound. Only variables with BOTH a lower and upper bound. -/
def varBoundsFromHyps (hyps : List Expr) : List (String × (Int × Int)) := Id.run do
  let mut los : List (String × Int) := []
  let mut his : List (String × Int) := []
  let upd := fun (l : List (String × Int)) (v : String) (k : Int) (f : Int → Int → Int) =>
    match l.lookup v with
    | some old => (l.filter (·.1 != v)) ++ [(v, f old k)]
    | none => l ++ [(v, k)]
  for c in hyps.flatMap conjuncts do
    match c with
    | .binOp _ .leq (.intLit _ k) (.ident _ v) => los := upd los v k max
    | .binOp _ .leq (.ident _ v) (.intLit _ k) => his := upd his v k min
    | .binOp _ .lt  (.intLit _ k) (.ident _ v) => los := upd los v (k+1) max
    | .binOp _ .lt  (.ident _ v) (.intLit _ k) => his := upd his v (k-1) min
    | .binOp _ .geq (.ident _ v) (.intLit _ k) => los := upd los v k max
    | .binOp _ .geq (.intLit _ k) (.ident _ v) => his := upd his v k min
    | .binOp _ .gt  (.ident _ v) (.intLit _ k) => los := upd los v (k+1) max
    | .binOp _ .gt  (.intLit _ k) (.ident _ v) => his := upd his v (k-1) min
    | _ => pure ()
  return los.filterMap fun (v, l) => (his.lookup v).map fun h => (v, (l, h))

/-- Conservative interval `(lo, hi)` of an arithmetic expr plus the maximum
    magnitude seen across the whole subtree (to choose a wrap-free bit width).
    `none` if any operand is unbounded or the op is outside `+`/`-`/`*`. -/
partial def exprIntervalMax (bounds : List (String × (Int × Int))) : Expr → Option (Int × Int × Nat)
  | .intLit _ k => some (k, k, k.natAbs)
  | .paren _ e => exprIntervalMax bounds e
  | .ident _ v => (bounds.lookup v).map fun (l, h) => (l, h, max l.natAbs h.natAbs)
  | .binOp _ op l r => do
    let (la, lb, lm) ← exprIntervalMax bounds l
    let (ra, rb, rm) ← exprIntervalMax bounds r
    let sub := max lm rm
    let mk := fun (a b : Int) => some (a, b, max sub (max a.natAbs b.natAbs))
    match op with
    | .add => mk (la + ra) (lb + rb)
    | .sub => mk (la - rb) (lb - ra)
    | .mul =>
      let ps := [la*ra, la*rb, lb*ra, lb*rb]
      mk (ps.foldl min (la*ra)) (ps.foldl max (la*ra))
    | _ => none
  | _ => none

/-- Lower an arithmetic expr to a `BitVec w` term (`+`/`*` of vars and
    non-negative literals only; `-` is excluded so the unsigned model can't
    underflow). -/
partial def arithToBVW (w : Nat) : Expr → Option String
  | .intLit _ k => if k < 0 then none else some s!"({k}#{w})"
  | .paren _ e => arithToBVW w e
  | .ident _ v => some v
  | .binOp _ op l r => do
    let L ← arithToBVW w l
    let R ← arithToBVW w r
    match op with
    | .add => some s!"({L} + {R})"
    | .mul => some s!"({L} * {R})"
    | _ => none
  | _ => none

/-- A widened unsigned `bv_decide` goal proving `e` cannot overflow `[lo,hi]`,
    when interval analysis shows the result is non-negative and in range and the
    operands are `+`/`*` of non-negative bounded vars. `none` otherwise (→ the
    obligation stays `unproven`). -/
def overflowBVGoal (e : Expr) (lo hi : Int) (hyps : List Expr) : Option String := do
  let bounds := varBoundsFromHyps hyps
  let (elo, ehi, maxMag) ← exprIntervalMax bounds e
  guard (lo ≤ elo)          -- lower bound holds by interval
  guard (ehi ≤ hi)          -- upper bound holds by interval
  guard (0 ≤ elo)           -- non-negative result → unsigned model is sound
  let w ← if maxMag < 2147483648 then some 32
          else if maxMag < 9223372036854775808 then some 64 else none
  let eBV ← arithToBVW w e
  let vars := (collectIdents e).eraseDups
  guard (!vars.isEmpty)
  -- every operand needs a non-negative upper bound to model it as unsigned BitVec
  let varHyps ← vars.mapM fun v => (bounds.lookup v).bind fun (vl, vh) =>
    if vl < 0 then none else some s!"BitVec.ule {v} ({vh}#{w})"
  some s!"∀ ({" ".intercalate vars} : BitVec {w}), {" → ".intercalate varHyps} → BitVec.ule {eBV} ({hi}#{w})"

/-- Generate no-overflow obligations for `#[overflow_checked]` functions. -/
def overflowObligations (modules : List Module) : List OverflowObl := Id.run do
  let mut out : List OverflowObl := []
  for (pfx, f) in modules.flatMap allFunctions do
    if !f.overflowChecked then continue
    let fq := pfx ++ f.name
    let vt := varTyMap f
    let mut i := 0
    for (e, scope) in scopedArithB f.loopContracts [] f.body do
      match (exprIntTy vt e).bind intRange, toLeanProp e with
      | some (lo, hi), some eStr =>
        let key := s!"{fq}#ovf{i}"
        let hyps := f.requires ++ scope
        let cv : Option Bool × Option String := match cEvalInt e with
          | some k => (some (decide (lo ≤ k ∧ k ≤ hi)), none)
          | none =>
            let vars := (collectIdents e ++ hyps.flatMap collectIdents).eraseDups
            let reqs := hyps.filterMap toLeanProp
            let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
            let hyp := if reqs.isEmpty then "" else s!"({" ∧ ".intercalate reqs}) → "
            (none, some s!"{binder}{hyp}({lo} ≤ {eStr} ∧ {eStr} ≤ {hi})")
        -- nonlinear/bv fallback: interval-gated widened unsigned goal (only when
        -- omega's linear goal won't close it — i.e. the constant case is skipped).
        let bvGoal := if cv.1.isSome then none else overflowBVGoal e lo hi hyps
        out := out ++ [{ fnQual := fq, key, opExpr := e, lo, hi
                       , closedVerdict := cv.1, leanGoal := cv.2, bvGoal, hyps }]
        i := i + 1
      | _, _ => pure ()
  return out

/-- Lean goals for the non-constant overflow obligations, for omega discharge. -/
def overflowGoals (modules : List Module) : List (String × String) :=
  (overflowObligations modules).filterMap fun o => o.leanGoal.map (fun g => (o.key, g))

/-- Widened unsigned `bv_decide` goals for nonlinear overflow obligations (the
    interval-gated `var * var` cases omega cannot close). Run by Main after omega,
    only for obligations omega left unproven. -/
def overflowBVGoals (modules : List Module) : List (String × String) :=
  (overflowObligations modules).filterMap fun o => o.bvGoal.map (fun g => (o.key, g))

/-! ## External-SMT path (Phase 2 #8) — narrow first slice

The kernel-checked tiers (constant fold → omega → `bv_decide`) are exhausted
first. What genuinely remains outside them is *nonlinear* integer arithmetic —
a product of two program variables that interval analysis cannot bound. For that
one narrow VC class we can emit a standard SMT-LIB query and hand it to an
external solver. The result is NEVER a kernel-checked class: it is
`solver_trusted` / `proved_by_smt` (the solver enters the TCB), kept distinct
from `proved_by_kernel_decision`, and only ever produced behind an explicit flag.
The translation targets structured `Expr`s (not the Lean-syntax strings), so the
emitted SMT-LIB is well-formed by construction. -/

/-- Lower a contract `Expr` to an SMT-LIB (QF_NIA) s-expression. Same fragment as
    `toLeanProp`: integer literals/vars, add/sub/mul, comparisons, and/or/not. -/
partial def exprToSmt : Expr → Option String
  | .intLit _ v => some (if v < 0 then s!"(- {-v})" else s!"{v}")
  | .ident _ n => some n
  | .paren _ e => exprToSmt e
  | .unaryOp _ op e => do
    let E ← exprToSmt e
    match op with
    | .neg  => some s!"(- {E})"
    | .not_ => some s!"(not {E})"
    | _     => none
  | .binOp _ op l r => do
    let L ← exprToSmt l
    let R ← exprToSmt r
    match op with
    | .neq => some s!"(not (= {L} {R}))"
    | _ => (obBinOpSmt op).map fun s => s!"({s} {L} {R})"
  | _ => none

/-- True when `e` contains a multiplication of two non-constant operands — the
    genuinely nonlinear shape `omega` cannot own and interval `bv_decide` may miss. -/
partial def exprHasNonlinMul : Expr → Bool
  | .binOp _ .mul l r => (cEvalInt l).isNone && (cEvalInt r).isNone
      || exprHasNonlinMul l || exprHasNonlinMul r
  | .binOp _ _ l r => exprHasNonlinMul l || exprHasNonlinMul r
  | .paren _ e => exprHasNonlinMul e
  | _ => false

/-- The SMT-eligible VC class (v1): `#[overflow_checked]` obligations whose operand
    is genuinely nonlinear (a product of variables), not constant, and not already
    closed by the interval `bv_decide` path. Returns `(vcKey, smtlibScript)`. The
    script asserts the in-scope hypotheses and the NEGATION of the range goal:
    `unsat` ⇒ no overflow (solver-proved); `sat` ⇒ a counterexample exists. -/
def overflowSmtGoals (modules : List Module) : List (String × String) := Id.run do
  let mut out : List (String × String) := []
  for o in overflowObligations modules do
    if o.closedVerdict.isSome then continue          -- constant tier owns it
    if o.bvGoal.isSome then continue                 -- interval bv_decide owns it
    if !exprHasNonlinMul o.opExpr then continue      -- omega owns the linear case
    -- soundness: require the operand AND every hypothesis to lower. If any
    -- #[requires]/guard falls outside the SMT fragment, DROP the whole query
    -- rather than emit one missing a constraint (which could read as a spurious
    -- counterexample). Never emit an unsound query.
    match exprToSmt o.opExpr, o.hyps.mapM exprToSmt with
    | some eSmt, some hypSmts =>
      let vars := (collectIdents o.opExpr ++ o.hyps.flatMap collectIdents).eraseDups
      let decls := vars.map (fun v => s!"(declare-const {v} Int)")
      let hypAsserts := hypSmts.map (fun s => s!"(assert {s})")
      let neg := s!"(assert (not (and (<= {o.lo} {eSmt}) (<= {eSmt} {o.hi}))))"
      let script := "\n".intercalate
        (["; VC " ++ o.key, "; no-overflow of an operand in [" ++ toString o.lo ++ ", " ++ toString o.hi ++ "]",
          "(set-logic QF_NIA)"] ++ decls ++ hypAsserts ++ [neg, "(check-sat)", "(get-model)"])
      out := out ++ [(o.key, script)]
    | _, _ => pure ()
  return out

/-! ## Lean replay artifact (Phase 2 #12)

For each SMT VC we also emit a standalone Lean theorem that states the SAME
obligation — hypotheses as binders, the range goal as the conclusion — with an
in-toolchain proof attempt (`by omega`). If a kernel-checked tactic closes it, the
claim no longer depends on the external solver and graduates `solver_trusted` →
`proved_by_lean_replay` (a Lean/kernel class, no solver in the TCB). The bounded
*nonlinear* fragment we route to SMT is, by construction, outside `omega`'s reach
(and `nlinarith` is Mathlib, which is deliberately NOT a dependency), so today the
attempt does not close and the VC honestly stays `solver_trusted`. The artifact is
still emitted so a reviewer — or a Mathlib-enabled build that swaps `omega` for
`nlinarith` — can check it and graduate the evidence. -/

/-- Lower a contract `Expr` to Lean `Prop`/`Int` syntax for the replay theorem.
    Same fragment as `toLeanProp` but ALSO handles unary negation (`-30000`) — the
    signed bounds that make a VC SMT-eligible in the first place. Kept local to the
    replay path so `toLeanProp`'s callers are unaffected. -/
partial def exprToLeanProp : Expr → Option String
  | .intLit _ v => some s!"{v}"
  | .ident _ n => some n
  | .paren _ e => exprToLeanProp e
  | .unaryOp _ op e => do
    let E ← exprToLeanProp e
    match op with | .neg => some s!"(-{E})" | .not_ => some s!"(¬ {E})" | _ => none
  | .binOp _ op l r => do
    let L ← exprToLeanProp l; let R ← exprToLeanProp r
    match op with
    | .div | .mod => none
    | _ => leanBinOp op L R
  | _ => none

/-- `(vcKey, leanTheoremSource)` for each SMT-eligible VC: a self-contained Lean
    theorem restating the obligation, with a `by omega` proof attempt. Same
    selection as `overflowSmtGoals`. -/
def leanReplayGoals (modules : List Module) : List (String × String) := Id.run do
  let mut out : List (String × String) := []
  for o in overflowObligations modules do
    if o.closedVerdict.isSome then continue
    if o.bvGoal.isSome then continue
    if !exprHasNonlinMul o.opExpr then continue
    match exprToLeanProp o.opExpr, o.hyps.mapM exprToLeanProp with
    | some eProp, some hypProps =>
      let vars := (collectIdents o.opExpr ++ o.hyps.flatMap collectIdents).eraseDups
      let binders := if vars.isEmpty then "" else s!"({" ".intercalate vars} : Int) "
      let hypBinders := " ".intercalate
        ((List.range hypProps.length).zip hypProps |>.map (fun (i, h) => s!"(h{i} : {h})"))
      let concl := s!"({o.lo} ≤ {eProp}) ∧ ({eProp} ≤ {o.hi})"
      -- `by omega` is the in-toolchain attempt; a Mathlib build swaps in `nlinarith`.
      let src := s!"theorem vc_replay {binders}{hypBinders} : {concl} := by omega"
      out := out ++ [(o.key, src)]
    | _, _ => pure ()
  return out

/-- Render the integer-overflow section. `provedKeys` are omega-discharged;
    `bvProvedKeys` are discharged by the widened `bv_decide` (nonlinear) path. -/
def renderOverflow (obls : List OverflowObl) (provedKeys : List String)
    (bvProvedKeys : List String := []) : String := Id.run do
  if obls.isEmpty then return ""
  let mut out := "\n\n=== Runtime-safety obligations (integer overflow, #[overflow_checked]) ==="
  let mut cur := ""
  for o in obls do
    if o.fnQual != cur then out := out ++ s!"\n\n{o.fnQual}"; cur := o.fnQual
    let status := match o.closedVerdict with
      | some true  => "checked: result in range (constant)"
      | some false => "VIOLATION: constant overflows the type"
      | none =>
        if provedKeys.contains o.key then
          "proved_by_kernel_decision (omega) — cannot overflow, no runtime check needed"
        else if bvProvedKeys.contains o.key then
          "proved_by_kernel_decision (bv_decide) — bounded operands cannot overflow (interval + bitvector), no runtime check needed"
        else match o.leanGoal with
          | some _ => "unproven — bound the operands (#[requires]), or use a wrapping/checked profile"
          | none => "unproven — operands not statically analyzable"
    out := out ++ s!"\n  {Concrete.fmtExpr o.opExpr}  (range [{o.lo}, {o.hi}])\n    status: {status}"
  return out ++ "\n"

/-- Runtime-safety obligations that have already been discharged to FALSE.
    These are compile-time proofs that the safe program is wrong, not merely
    `unproven` obligations. Build/check paths use this as a hard-error gate;
    report paths still render the obligations so users can inspect them. -/
def provenViolationDiagnostics (modules : List Module) : Diagnostics := Id.run do
  -- Safe-code only: functions marked `trusted` or holding the `Unsafe`
  -- capability carry audit responsibility and are exempt (ROADMAP Phase 12 #0).
  let unsafeQuals : List String := (modules.flatMap allFunctions).filterMap fun (pfx, f) =>
    if f.isTrusted || Capabilities.capSetHasUnsafe f.capSet then some (pfx ++ f.name) else none
  let mut ds : Diagnostics := []
  for o in boundsObligations modules do
    if o.closedVerdict == some false && !unsafeQuals.contains o.fnQual then
      let d : Diagnostic := {
        severity := .error,
        message := s!"proven runtime-safety violation: {o.arrName}[{Concrete.fmtExpr o.idxExpr}] is always out of bounds for array size {o.size}",
        pass := "runtime-safety",
        span := some o.idxExpr.getSpan,
        hint := some "fix the index, change the array size, or move this behind an explicit trusted/Unsafe boundary",
        code := "E0900",
        evidence := [("obligation", o.key), ("status", "violation"), ("kind", "array_bounds")]
      }
      ds := ds ++ [d]
  for o in divObligations modules do
    if o.closedVerdict == some false && !unsafeQuals.contains o.fnQual then
      let opname := if o.isMod then "%" else "/"
      let d : Diagnostic := {
        severity := .error,
        message := s!"proven runtime-safety violation: {opname} divisor {Concrete.fmtExpr o.divExpr} is always zero",
        pass := "runtime-safety",
        span := some o.divExpr.getSpan,
        hint := some "require/prove a nonzero divisor, use a checked API, or move this behind an explicit trusted/Unsafe boundary",
        code := "E0900",
        evidence := [("obligation", o.key), ("status", "violation"), ("kind", "division_nonzero")]
      }
      ds := ds ++ [d]
  for o in overflowObligations modules do
    if o.closedVerdict == some false && !unsafeQuals.contains o.fnQual then
      let d : Diagnostic := {
        severity := .error,
        message := s!"proven runtime-safety violation: {Concrete.fmtExpr o.opExpr} always overflows range [{o.lo}, {o.hi}]",
        pass := "runtime-safety",
        span := some o.opExpr.getSpan,
        hint := some "widen the type, bound the operands, or use an explicit wrapping/checked arithmetic profile",
        code := "E0900",
        evidence := [("obligation", o.key), ("status", "violation"), ("kind", "integer_overflow")]
      }
      ds := ds ++ [d]
  return ds

/-- Stable identity for one loop obligation, shared by the goal collector and
    the renderer so discharge results map back to the right line. -/
def loopVCKey (fnQual : String) (line : Nat) (obl : String) : String :=
  s!"{fnQual}@{line}#{obl}"

/-- Collect the loop VCs that a kernel decision procedure can discharge:
    `invariant_init` (O1), `variant_nonnegative` (O4), `variant_decreases` (O5).
    Each is `(key, leanGoal)` where the goal is the same string the report
    shows — it is already valid Lean (`∀ (i : Int), … `) provable by
    `intros; omega`. Preservation (O2) stays hand-linked; exit-link (O3) needs a
    postcondition. -/
def loopVCGoals (modules : List Module) : List (String × String) := Id.run do
  let withLoops := (modules.flatMap allFunctions).filter (fun (_, f) => !f.loopContracts.isEmpty)
  let mut out : List (String × String) := []
  for (pfx, f) in withLoops do
    let extraLets := letConstMap f.body
    let fq := pfx ++ f.name
    let retExpr := loopExitReturn f.body
    -- Phase 3 #9: thread the function's `#[requires]` into every loop obligation
    -- (the unified scoped context). Adding hypotheses is monotonic — it can only
    -- turn an `unproven` loop VC `proved` (e.g. an init `0 ≤ n` from a precondition),
    -- never the reverse.
    let outer := f.requires
    for lc in f.loopContracts do
      if let some g := genInitVC lc extraLets outer then
        out := out ++ [(loopVCKey fq lc.line "O1", g)]
      -- O2 arithmetic half: invariant is inductive (omega); operational half
      -- still needs Lean (genPreservationShape).
      if let some g := genPreservationGoal lc outer then
        out := out ++ [(loopVCKey fq lc.line "O2", g)]
      if let some g := genExitVC lc f.ensures retExpr outer then
        out := out ++ [(loopVCKey fq lc.line "O3", g)]
      if lc.variant.isSome then
        if let some g := genVariantNonneg lc outer then
          out := out ++ [(loopVCKey fq lc.line "O4", g)]
        if let some g := genVariantDecreases lc outer then
          out := out ++ [(loopVCKey fq lc.line "O5", g)]
  return out

/-- The loop-contract section: for each `#[invariant]`/`#[variant]`-annotated
    loop, enumerate the verification obligations it induces. Every obligation now
    carries a compiler-generated VC shape. Discharge: preservation (O2) is
    hand-linked via a `coverage: invariant` registry entry; init/variant
    (O1/O4/O5) are kernel-discharged by `omega` when their key appears in
    `provedVCs`; the rest are `planned`. -/
def loopContractSection (modules : List Module) (registry : ProofRegistry)
    (provedVCs : List String := []) (provedVacuous : List String := []) : String := Id.run do
  let withLoops := (modules.flatMap allFunctions).filter (fun (_, f) => !f.loopContracts.isEmpty)
  if withLoops.isEmpty then return ""
  let callables := callableContractNames modules
  let consts := contractConstNames modules
  let impures := impureFnNames modules
  let mut out := "\n\n=== Loop contracts ==="
  for (pfx, f) in withLoops do
    -- a registered `coverage: invariant` proof discharges invariant_preservation
    let preserveProof : Option String :=
      match registry.find? (fun e => e.function == pfx ++ f.name) with
      | some e => if e.coverage == "invariant" && !e.proof.isEmpty then some e.proof else none
      | none => none
    let extraLets := letConstMap f.body
    let fq := pfx ++ f.name
    let outer := f.requires  -- Phase 3 #9: function preconditions are in scope for loop VCs
    let contractVars := (f.params.map (·.name) ++ localNamesB f.body ++ consts).eraseDups
    for lc in f.loopContracts do
      out := out ++ s!"\n\n{pfx}{f.name}  (loop @ line {lc.line})"
      let mut invIdx := 0
      for inv in lc.invariants do
        let issues := validateContractExpr contractVars callables inv
          ++ (contractImpureCalls impures inv).map (fun fn => s!"impure call '{fn}' — spec/ghost must be pure and total (no capabilities)")
        let vac := cEvalBool inv == some false || provedVacuous.contains s!"{fq}@{lc.line}#inv_vac{invIdx}"
        let st :=
          if !issues.isEmpty then contractIssueStatus issues
          else if vac then "\n     status:  invalid/vacuous (unsatisfiable invariant — the loop obligations below are meaningless)"
          else ""
        out := out ++ s!"\n  invariant {Concrete.fmtExpr inv}{st}"
        invIdx := invIdx + 1
      match lc.variant with
      | some v => out := out ++ s!"\n  variant   {Concrete.fmtExpr v}{contractIssueStatus (validateContractExpr contractVars callables v)}"
      | none => pure ()
      out := out ++ "\n  obligations:"
      let planned := "planned (no discharge backend linked yet)"
      let vc := fun (g : Option String) => match g with | some s => s!"\n       generated VC:  {s}" | none => ""
      -- Kernel-decision status for an obligation: omega-discharged when its key
      -- is in `provedVCs`, else planned. `omega` is a kernel decision procedure
      -- (linear integer arithmetic), no external SMT in the TCB.
      let kstat := fun (obl : String) =>
        if provedVCs.contains (loopVCKey fq lc.line obl)
        then "proved_by_kernel_decision\n                                engine:  omega"
        else planned
      -- O1 invariant_init — generated shape, kernel-discharged
      out := out ++ s!"\n    O1 invariant_init          status:  {kstat "O1"}{vc (genInitVC lc extraLets outer)}"
      -- O2 invariant_preservation — split into the two things it actually
      -- requires: (1) the arithmetic step (invariant is inductive), now
      -- auto-discharged by omega; (2) the operational step (the extracted body
      -- realizes the substitution), which still needs Lean. (1) removes most of
      -- the hand-linking; (2) points to the theorem shape when unproved.
      let arithStep :=
        if provedVCs.contains (loopVCKey fq lc.line "O2")
        then "proved_by_kernel_decision (omega)" else planned
      let opStep := match preserveProof with
        | some thm => s!"proved_by_lean\n                          theorem: {thm}"
        | none => match genPreservationShape lc fq with
          | some shape => s!"planned — needs Lean (operational realization), shape:\n           {shape}"
          | none => planned
      out := out ++ s!"\n    O2 invariant_preservation"
      out := out ++ s!"\n       arithmetic step:   {arithStep}"
      out := out ++ s!"\n       operational step:  {opStep}{vc (genPreservationVC lc)}"
      -- O3 exit_implies_post: bridges loop exit facts (invariant ∧ ¬guard) to
      -- the function #[ensures]. Generated only when there is an ensures and a
      -- clean loop-exit return; kernel-discharged by omega like O1/O4/O5.
      let exitVC := genExitVC lc f.ensures (loopExitReturn f.body) outer
      let o3status :=
        if exitVC.isSome then kstat "O3"
        else if f.ensures.isEmpty then "n/a (no #[ensures] postcondition)"
        else planned
      out := out ++ s!"\n    O3 loop_exit_post_link     status:  {o3status}{vc exitVC}"
      match lc.variant with
      | some _ =>
        out := out ++ s!"\n    O4 variant_nonnegative     status:  {kstat "O4"}{vc (genVariantNonneg lc outer)}"
        out := out ++ s!"\n    O5 variant_decreases       status:  {kstat "O5"}{vc (genVariantDecreases lc outer)}"
      | none => pure ()
  return out ++ "\n"

partial def contractsReport (modules : List Module) (registry : ProofRegistry)
    (provedVCs : List String := []) (provedVacuous : List String := []) : String := Id.run do
  let callables := callableContractNames modules
  let consts := contractConstNames modules
  let impures := impureFnNames modules
  -- Discharge status for an `#[ensures]` on `qual`. Tiers, honest about partial
  -- coverage: a registered `ensures_proof` → fully proved_by_lean. Otherwise, a
  -- registered `proof` with directional coverage (`one_direction`) discharges
  -- ONE direction of the postcondition — shown as partial, with the converse
  -- still outstanding — rather than collapsing to a bare "missing".
  let discharge (qual : String) : String :=
    match registry.find? (fun e => e.function == qual) with
    | some e => match e.ensuresProof with
      | some thm =>
        -- `iff` coverage with both a `proof` and an `ensures_proof` means the
        -- two directions of an iff postcondition are each kernel-checked.
        if e.coverage == "iff" && !e.proof.isEmpty then
          s!"\n     status:  proved_by_lean (full iff)\n     forward direction:  {e.proof}\n     converse direction: {thm}"
        else s!"\n     status:  proved_by_lean\n     theorem: {thm}"
      | none =>
        if !e.proof.isEmpty && e.coverage == "one_direction" then
          s!"\n     status:  partial — one direction proved_by_lean, converse outstanding\n     theorem: {e.proof}  (coverage: one_direction)\n     note:    full postcondition not yet discharged; the converse is the next obligation"
        else "\n     status:  missing (registry entry has no ensures_proof)"
    | none => "\n     status:  missing (no in-source proof link for this function)"
  let rec go (m : Module) (acc : String) : String := Id.run do
    let mut out := acc
    let pfx := if m.name.isEmpty then "" else m.name ++ "."
    for sf in m.specFns do
      let ps := ", ".intercalate (sf.params.map (fun p => s!"{p.name}: {Concrete.fmtTy p.ty}"))
      out := out ++ s!"\nspec fn {pfx}{sf.name}({ps}) -> {Concrete.fmtTy sf.retTy}"
    for f in m.functions do
      if !f.ensures.isEmpty || !f.requires.isEmpty then
        out := out ++ s!"\n\n{pfx}{f.name}"
        let paramVars := f.params.map (·.name)
        let postVars := (paramVars ++ ["result"] ++ localNamesB f.body ++ consts).eraseDups
        let preVars := (paramVars ++ consts).eraseDups
        -- vacuity: an unsatisfiable precondition makes every #[ensures] hold
        -- trivially — a misleading green. Caught by the constant folder
        -- (#[requires(false)]) or by omega refuting the conjunction (x>0 && x<0).
        let vacuous := !f.requires.isEmpty
          && (f.requires.any (fun r => cEvalBool r == some false)
              || provedVacuous.contains s!"{pfx ++ f.name}#requires_vac")
        if vacuous then
          out := out ++ "\n  ⚠ VACUOUS — preconditions are unsatisfiable; any #[ensures] holds trivially (NOT a real proof)"
        -- preconditions: assumed on entry here; each call site is checked
        -- separately (see the "Call-site obligations" section).
        let mut ri := 1
        for r in f.requires do
          let issues := validateContractExpr preVars callables r
            ++ (contractImpureCalls impures r).map (fun fn => s!"impure call '{fn}' — spec/ghost must be pure and total (no capabilities)")
          let st :=
            if !issues.isEmpty then contractIssueStatus issues
            else if vacuous then "\n     status:  vacuous (unsatisfiable precondition)"
            else "\n     status:  assumed_at_entry (each call site checked separately)"
          out := out ++ s!"\n  R{ri}  requires {Concrete.fmtExpr r}{st}"
          ri := ri + 1
        -- postconditions: vacuous if the precondition is unsatisfiable, else
        -- discharged by a registered ensures_proof, or missing.
        let mut i := 1
        for e in f.ensures do
          let issues := validateContractExpr postVars callables e
            ++ (contractImpureCalls impures e).map (fun fn => s!"impure call '{fn}' — spec/ghost must be pure and total (no capabilities)")
          let st :=
            if !issues.isEmpty then contractIssueStatus issues
            else if vacuous then "\n     status:  vacuous (precondition unsatisfiable — postcondition holds trivially, NOT proved)"
            else discharge (pfx ++ f.name)
          out := out ++ s!"\n  O{i}  ensures {Concrete.fmtExpr e}{st}"
          i := i + 1
    for sub in m.submodules do
      out := go sub out
    return out
  let body := modules.foldl (fun acc m => go m acc) ""
  let body := if body.isEmpty then "\n(no spec fns or #[ensures] contracts found)" else body
  return s!"=== Source Contracts ==={body}\n{loopContractSection modules registry provedVCs provedVacuous}"

/-- Whether any module (or submodule) carries a source contract — a `spec fn`
    or an `#[ensures(...)]`. Used to decide whether `audit` appends the
    contracts section. -/
partial def hasContracts (modules : List Module) : Bool :=
  modules.any fun m =>
    !m.specFns.isEmpty
    || m.functions.any (fun f => !f.ensures.isEmpty || !f.requires.isEmpty || !f.loopContracts.isEmpty
        || !(f.body.flatMap collectAssertAssumeS).isEmpty)
    || hasContracts m.submodules

def interfaceReport (summaryTable : List (String × FileSummary)) : String :=
  let header := "=== Interface Summary ==="
  let body := summaryTable.map fun (name, fs) => interfaceModule name fs
  let totalExports := summaryTable.foldl (fun acc (_, fs) =>
    let pubCount := (fs.functions.filter fun (n, _) => fs.publicNames.contains n).length +
      (fs.externFns.filter (·.isPublic)).length +
      (fs.structs.filter (·.isPublic)).length +
      (fs.enums.filter (·.isPublic)).length +
      (fs.traits.filter (·.isPublic)).length +
      (fs.constants.filter (·.isPublic)).length +
      (fs.typeAliases.filter (·.isPublic)).length +
      (fs.newtypes.filter (·.isPublic)).length
    acc + pubCount) 0
  let summary := s!"\nTotals: {summaryTable.length} modules, {totalExports} public exports"
  s!"{header}\n\n{"\n\n".intercalate body}\n{summary}\n"

-- ============================================================

end Report
end Concrete
