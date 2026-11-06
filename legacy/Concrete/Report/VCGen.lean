/-
# Obligation generation as a calculus — slice 1

Implements the structure in `docs/verification/VC_GENERATOR_DESIGN.md`: **one traversal plus a per-constructor
rule table**, replacing four hand-written walkers over the same grammar.

## Why, in one number

Nine discovery defects were found in 2026-08. Every one was "an obligation in syntactic position X
is lost", and every one existed because the same traversal is written eight times (four expression
walkers, four leaves) and nothing forces them to agree. Two examples, both found mechanically in
the last hour rather than by review:

* a shift inside an `if`-EXPRESSION produced no obligation — three sibling walkers had the case,
  the fourth never did;
* a division inside `assert(a / b > 0)` produced none — all eight lacked it.

Neither is a discipline failure. Keeping eight copies of one traversal consistent by hand is a
design problem, and this file is the design fix: **there is one recursion, and adding a family is
a row rather than a walker.**

## Status: SHADOW ONLY

Nothing consumes this yet. It exists to be diffed against the existing walkers
(`--report vcgen-diff`), which are a usable oracle now that their nine defects are fixed and
gated. Every difference is a finding either way — the walker missed something, or this did.
Switching consumers over comes only when the diff is empty or every remaining entry is explained
in writing.
-/
import Concrete.Report.ReportObligations

namespace Concrete
namespace Report
namespace VCGen

/-- What a single node requires, independent of where it sits. -/
inductive Requirement where
  /-- `divisor ≠ 0`, carrying the dividend so the signed `MIN / -1` bound stays expressible. -/
  | divisorNonZero (isMod : Bool) (dividend divisor : Expr)
  /-- `0 ≤ amount < width(shifted)`. -/
  | shiftInRange (shifted amount : Expr)
  /-- `0 ≤ index < len(array)`. -/
  | indexInRange (array index : Expr)
  /-- the whole `a op b` node is in range for its type. -/
  | arithInRange (node : Expr)
  /-- `-x` traps when `x` is the type's MIN. No obligation kind exists for this anywhere in the
      pipeline today, and codegen traps on it — so it is a runtime abort with nothing proved. -/
  | negNotMin (operand : Expr)
  /-- a float→int cast traps when the value is out of the target's range. Same story: a
      `checkedF2IHelperDefs` trap at runtime, and no obligation. -/
  | castRepresentable (operand : Expr) (target : Ty)

/-- The rule table: what THIS node requires, with no recursion in it.

    This is the whole point of the design. A walker mixes "what does this node need" with "how do
    I reach the next node", so a family is a traversal and eight of them drift. Here the first
    question is a flat table and the second is `vcExpr` below, written once.

    Two obligation kinds that do not exist anywhere today are one line each when they are wanted:
    `MIN` negation (`unaryOp .neg`) and float→int casts. Both trap at runtime with nothing
    generated, and that they were skipped is evidence about the old structure's cost, not about
    their difficulty. -/
def requires : Expr → List Requirement
  | .binOp _ .div l r => [.divisorNonZero false l r]
  | .binOp _ .mod l r => [.divisorNonZero true l r]
  | .binOp _ .shl l r => [.shiftInRange l r]
  | .binOp _ .shr l r => [.shiftInRange l r]
  | e@(.binOp _ .add _ _) => [.arithInRange e]
  | e@(.binOp _ .sub _ _) => [.arithInRange e]
  | e@(.binOp _ .mul _ _) => [.arithInRange e]
  | .arrayIndex _ a i => [.indexInRange a i]
  -- TWO KINDS THAT DID NOT EXIST BEFORE THIS FILE, one line each -- which is the design claim
  -- being tested rather than asserted. Both trap in the compiled binary today with nothing
  -- generated, so they were runtime aborts the proof layer could not see. Adding them to a
  -- walker would have meant a fifth and sixth traversal; here they are rows.
  -- Constant operands are excluded: `-5` cannot trap, and counting it would inflate the number
  -- of genuinely unguarded sites. An unfiltered first measurement said 14,139 across the corpus;
  -- most were literals. Reporting the smaller true figure matters more than the larger one.
  | .unaryOp _ .neg (.intLit _ _) => []
  | .unaryOp _ .neg e => [.negNotMin e]
  | .cast _ (.intLit _ _) _ => []
  | .cast _ e t => [.castRepresentable e t]
  | _ => []

mutual
/-- THE traversal. One recursion over the expression grammar, calling `requires` at every node.

    Exhaustive by construction: every constructor appears once, and a new one forces a case here
    and nowhere else. Compare the eight places a new constructor must currently be added, of which
    the per-walker coverage gate can only check that they exist — not that they agree. -/
partial def vcExpr (e : Expr) : List Requirement :=
  requires e ++
    match e with
    | .paren _ x | .unaryOp _ _ x | .borrow _ x | .borrowMut _ x | .deref _ x
    | .cast _ x _ | .fieldAccess _ x _ => vcExpr x
    | .binOp _ _ l r => vcExpr l ++ vcExpr r
    | .arrayIndex _ a i => vcExpr a ++ vcExpr i
    | .allocCall _ x a => vcExpr x ++ vcExpr a
    | .arrayLit _ es => es.flatMap vcExpr
    | .call _ _ _ args => args.flatMap vcExpr
    | .methodCall _ o _ _ args => vcExpr o ++ args.flatMap vcExpr
    | .staticMethodCall _ _ _ _ args => args.flatMap vcExpr
    | .structLit _ _ _ fs base => fs.flatMap (fun (_, fe) => vcExpr fe) ++ (base.map vcExpr).getD []
    | .enumLit _ _ _ _ fs => fs.flatMap (fun (_, fe) => vcExpr fe)
    | .ifExpr _ c t el => vcExpr c ++ t.flatMap vcStmt ++ el.flatMap vcStmt
    | .match_ _ s _ => vcExpr s
    | _ => []

/-- Statement traversal. One recursion, and every statement carrying an expression reaches
    `vcExpr` — including the two that cost obligations this month: `assert_`/`assume_` conditions
    and destructuring scrutinees. -/
partial def vcStmt (s : Stmt) : List Requirement :=
  match s with
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v => vcExpr v
  | .return_ _ (some v) => vcExpr v
  | .return_ _ none => []
  | .assert_ _ c | .assume_ _ c => vcExpr c
  | .ifElse _ c t el => vcExpr c ++ t.flatMap vcStmt ++ (el.getD []).flatMap vcStmt
  | .while_ _ c b _ => vcExpr c ++ b.flatMap vcStmt
  | .forLoop _ init c step b _ =>
      (init.map vcStmt).getD [] ++ vcExpr c ++ (step.map vcStmt).getD [] ++ b.flatMap vcStmt
  | .fieldAssign _ o _ v | .derefAssign _ o v => vcExpr o ++ vcExpr v
  -- The WRITE target needs its own bounds check. `requires` fires on `.arrayIndex`
  -- EXPRESSIONS, i.e. reads; `a[i] = v` is a statement, so without this line the store's
  -- bounds obligation is never emitted -- and a store is the more safety-critical direction.
  -- Found by the differential against the walkers, not by review.
  | .arrayIndexAssign _ a i v => .indexInRange a i :: (vcExpr a ++ vcExpr i ++ vcExpr v)
  | .borrowIn _ _ _ _ _ b => b.flatMap vcStmt
  | .letDestructure _ _ _ _ v _ => vcExpr v
  | .letStructDestructure _ _ _ v => vcExpr v
  | _ => []
end

/-! ## `wp` over statements — the path condition, accumulated rather than threaded

`scopedWalk` carries the enclosing hypotheses by hand: it appends the guard on the then-branch,
`negateGuard c` on the else-branch, loop hypotheses in bodies, and calls `dropStaleHyps` when a
statement reassigns a variable a hypothesis mentions.

Those are the `if` and `while` rules of a weakest-precondition calculus, written out longhand. Here
they fall out of the recursion, which is the second half of the design's claim: **one mechanism
replaces two.**

Reassignment is where this got a claim wrong, and the correction is worth keeping. `scopedWalk`
DROPS any hypothesis mentioning a reassigned variable; WP would SUBSTITUTE
(`wp(x := e, Q) = Q[x := e]`), which is strictly stronger — after `d = d - 1` the fact `d != 0`
becomes `d - 1 != 0` and survives, where dropping loses it.

An earlier version of this comment concluded that the calculus should therefore RETAIN the
hypothesis, and that a divergence from the walker would be the calculus being right. That was
wrong: substitution is stronger, but retaining WITHOUT substituting is neither — it keeps a fact
about the old value and calls it a fact about the new one. On
`div_scope_adversarial.stale` — `if d != 0 { d = d - 1; return n / d; }` — it proves a division
safe that is not. The differential found it on the single fixture built for the purpose.

So this slice DROPS, matching the walker. Substitution is the principled replacement and is future
work; until it exists, dropping is the sound choice and the stronger-sounding option is a bug. -/

/-- A requirement together with the path condition holding where it arises. -/
abbrev Guarded := Requirement × List Expr

mutual
/-- Expression requirements, all under the same path condition. -/
partial def vcExprG (gs : List Expr) (e : Expr) : List Guarded :=
  (vcExpr e).map (fun r => (r, gs))

/-- Statement traversal accumulating the path condition, exactly as WP's rules prescribe. -/
partial def vcStmtG (gs : List Expr) (lcs : List LoopContract) (s : Stmt) : List Guarded :=
  match s with
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v => vcExprG gs v
  | .return_ _ (some v) => vcExprG gs v
  | .assert_ _ c | .assume_ _ c => vcExprG gs c
  | .ifElse _ c t el =>
      -- the two WP branch rules: `c` holds in the then-branch, `¬c` in the else-branch.
      vcExprG gs c
        ++ vcStmtsG (gs ++ [c]) lcs t
        ++ vcStmtsG (gs ++ (negateGuard c).toList) lcs (el.getD [])
  | .while_ sp c b _ =>
      vcExprG gs c ++ vcStmtsG (gs ++ loopHypsAt lcs sp.line) lcs b
  | .forLoop sp init c step b _ =>
      ((init.map (vcStmtG gs lcs)).getD []) ++ vcExprG gs c
        ++ ((step.map (vcStmtG gs lcs)).getD [])
        ++ vcStmtsG (gs ++ loopHypsAt lcs sp.line) lcs b
  | .fieldAssign _ o _ v | .derefAssign _ o v => vcExprG gs o ++ vcExprG gs v
  | .arrayIndexAssign _ a i v =>
      (.indexInRange a i, gs) :: (vcExprG gs a ++ vcExprG gs i ++ vcExprG gs v)
  | .borrowIn _ _ _ _ _ b => vcStmtsG gs lcs b
  | .letDestructure _ _ _ _ v _ => vcExprG gs v
  | .letStructDestructure _ _ _ v => vcExprG gs v
  | _ => []

/-- WP's SEQUENTIAL rule, and the reason a per-statement `flatMap` is not enough.

    After `if c { return … }` the remaining statements run only when `¬c`, so the path condition
    for the tail must gain `negateGuard c`. `scopedWalk` implements this as its fall-through case;
    without it the calculus loses a hypothesis the walker has, which the differential caught on
    `fmt.format_int` (`10 | value != 0` there, `10 |` here).

    Guards accumulate left-to-right across the list — that IS `wp(s₁; s₂, Q)` unrolled. -/
partial def vcStmtsG (gs : List Expr) (lcs : List LoopContract) : List Stmt → List Guarded
  | [] => []
  | s :: rest =>
      let here := vcStmtG gs lcs s
      -- a then-branch that always returns means the tail is reached only under ¬c
      -- A statement that REASSIGNS a variable invalidates every hypothesis mentioning it. This
      -- is not optional bookkeeping: on `if d != 0 { d = d - 1; return n / d; }` the guard is
      -- about the OLD `d`, and retaining it proves a division safe that is not.
      --
      -- I had written the opposite into this file an hour earlier -- that keeping the hypothesis
      -- would be "the calculus being right" -- reasoning that WP's substitution rule is stronger
      -- than dropping. Substitution IS stronger, but I implemented neither substitution nor
      -- dropping, and the combination is UNSOUND rather than strong. The differential caught it
      -- on the one fixture built for exactly this (`div_scope_adversarial.stale`).
      --
      -- Dropping matches the walker and is sound. Substitution (`wp(x := e, Q) = Q[x := e]`) is
      -- the principled replacement and would RETAIN the fact as `d - 1 != 0`; until it exists,
      -- this must drop.
      let afterAssign := dropStaleHyps gs (assignedScalarsS s)
      let tailGs := match s with
        | .ifElse _ c t none =>
            if blockTerminates t then gs ++ (negateGuard c).toList else afterAssign
        | _ => afterAssign
      here ++ vcStmtsG tailGs lcs rest
end

/-! ### Differential against the existing walkers

The walkers are the oracle: nine defects found, fixed and gated, so they are the best available
statement of what SHOULD be generated. Agreement is evidence for this file; disagreement is a
finding on one side or the other, and either is worth having. -/

/-- Divisor requirements from this calculus, as comparable strings. -/
def divisorsHere (f : FnDef) : List String :=
  (f.body.flatMap vcStmt).filterMap fun r =>
    match r with
    | .divisorNonZero isMod _ d => some s!"{if isMod then "mod" else "div"}:{Concrete.fmtExpr d}"
    | _ => none

/-- The same, from the existing walker, via the same statement traversal it already uses. -/
def divisorsThere (f : FnDef) : List String :=
  (f.body.flatMap collectDivisorsS).map fun (isMod, _, d) =>
    s!"{if isMod then "mod" else "div"}:{Concrete.fmtExpr d}"

/-- Shift requirements, both sides. -/
def shiftsHere (f : FnDef) : List String :=
  (f.body.flatMap vcStmt).filterMap fun r =>
    match r with
    | .shiftInRange sv amt => some s!"{Concrete.fmtExpr sv}<<{Concrete.fmtExpr amt}"
    | _ => none

def shiftsThere (f : FnDef) : List String :=
  (f.body.flatMap collectShiftsS).map fun (l, r) =>
    s!"{Concrete.fmtExpr l}<<{Concrete.fmtExpr r}"

/-- Bounds requirements, both sides. `collectIndexUsesE` records the array EXPRESSION (not a
    name) since 2026-08-05, so the two are directly comparable without peeling parens here. -/
def boundsHere (f : FnDef) : List String :=
  (f.body.flatMap vcStmt).filterMap fun r =>
    match r with
    | .indexInRange a i => some s!"{Concrete.fmtExpr a}[{Concrete.fmtExpr i}]"
    | _ => none

def boundsThere (f : FnDef) : List String :=
  (f.body.flatMap collectIndexUsesS).map fun (a, i) =>
    s!"{Concrete.fmtExpr a}[{Concrete.fmtExpr i}]"

/-- Overflow requirements, both sides. Both collect the whole `a op b` node for `+`/`-`/`*`. -/
def arithHere (f : FnDef) : List String :=
  (f.body.flatMap vcStmt).filterMap fun r =>
    match r with
    | .arithInRange e => some (Concrete.fmtExpr e)
    | _ => none

def arithThere (f : FnDef) : List String :=
  (f.body.flatMap collectArithS).map Concrete.fmtExpr

/-- Div requirements WITH their path condition, from the calculus. -/
def divGuardedHere (f : FnDef) : List String :=
  (vcStmtsG [] f.loopContracts f.body).filterMap fun (r, gs) =>
    match r with
    | .divisorNonZero _ _ d =>
        some s!"{Concrete.fmtExpr d} | {" & ".intercalate (gs.map Concrete.fmtExpr)}"
    | _ => none

/-- The same from `scopedWalk`, whose `scope` is the hand-threaded equivalent. -/
def divGuardedThere (f : FnDef) : List String :=
  (scopedDivB f.loopContracts [] (paramDecls f) f.body).map fun (_, _, d, scope, _) =>
    s!"{Concrete.fmtExpr d} | {" & ".intercalate (scope.map Concrete.fmtExpr)}"

/-- Per-function diff: `(qualName, family, onlyHere, onlyThere)`, empty when they agree.

    Multiset-shaped rather than set-shaped: a duplicated obligation is a real difference, and
    comparing sets would hide it. -/
def diff (modules : List Module) : List (String × String × List String × List String) := Id.run do
  let mut out := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    for (fam, here, there) in
        [("div", divisorsHere f, divisorsThere f), ("shift", shiftsHere f, shiftsThere f),
         ("bounds", boundsHere f, boundsThere f), ("overflow", arithHere f, arithThere f),
         ("div+guards", divGuardedHere f, divGuardedThere f)] do
      let onlyHere := here.filter (fun x => (here.count x) > (there.count x))
      let onlyThere := there.filter (fun x => (there.count x) > (here.count x))
      if !onlyHere.isEmpty || !onlyThere.isEmpty then
        out := out ++ [(fq, fam, onlyHere.eraseDups, onlyThere.eraseDups)]
  return out

/-- Requirements the calculus finds that NO walker has a counterpart for.

    Reported separately and never folded into the differential: comparing them against a walker
    that cannot produce them would show a permanent false disagreement, and silently dropping
    them would hide the only place the calculus is currently AHEAD. -/
def calculusOnly (modules : List Module) : List (String × String) := Id.run do
  let mut out := []
  for (pfx, f) in modules.flatMap allFunctions do
    for r in f.body.flatMap vcStmt do
      match r with
      | .negNotMin e => out := out ++ [(pfx ++ f.name, s!"neg-not-MIN: -({Concrete.fmtExpr e})")]
      | .castRepresentable e t =>
          out := out ++ [(pfx ++ f.name, s!"cast-representable: {Concrete.fmtExpr e} as {tyToString t}")]
      | _ => pure ()
  return out.eraseDups

/-- Totals both sides found, so AGREEMENT can be distinguished from BOTH-FOUND-NOTHING.

    Two checks today passed while ranging over an empty set, so a differential that reports
    "agree" without saying how much it compared is not evidence. -/
def diffTotals (modules : List Module) : Nat × Nat := Id.run do
  let mut here := 0
  let mut there := 0
  for (_, f) in modules.flatMap allFunctions do
    here := here + (divisorsHere f).length + (shiftsHere f).length
              + (boundsHere f).length + (arithHere f).length
    there := there + (divisorsThere f).length + (shiftsThere f).length
              + (boundsThere f).length + (arithThere f).length
  return (here, there)

end VCGen
end Report
end Concrete
