/-
# Non-arithmetic multi-kernel evidence: boolean postconditions

Every kernel-agreement demo before this one is **linear integer arithmetic** — `omega`, `lia`,
`presburger`. Three kernels agreeing on arithmetic is a real result, but it exercises one theory,
and each prover's decision procedure for it is mature and well-trodden.

This module carries the first NON-arithmetic obligation to all three kernels: a boolean
postcondition, discharged by case analysis rather than by an arithmetic decision procedure.

## Why booleans first, and not merely because they are easy

The multi-kernel claim does not rest on "three provers said yes". It rests on: each kernel closed
a lowering **whose truth table matches the reference evaluator**, which is what prevents a kernel
from proving a *different* proposition and having that counted. For a boolean goal over `n`
variables that check is **exhaustive** — all `2^n` assignments, no sampling. Arithmetic
obligations can only be checked on chosen instances, and an inductive property over unbounded
inputs could not be checked at all.

So booleans are the one rung of the ladder where faithfulness is *complete* rather than sampled.
That is a stronger agreement argument than the arithmetic tier has, on a weaker theory.

## Why Concrete `bool` maps to Rocq `bool`, not `Prop`

`Prop` would be unfaithful and would drag in axioms. Concrete's `bool` is two-valued and
decidable; Rocq's `Prop` is intuitionistic, so De Morgan's `~(a /\ b) -> ~a \/ ~b` is **not
provable** there without classical logic. Lowering to `Prop` would therefore either fail or
require `Classical`, which `Print Assumptions` would then report — turning an axiom-free
attestation into an axiom-bearing one for no reason. Over `bool` the same fact is
`destruct a, b; reflexivity`: constructive, and verified axiom-free
("Closed under the global context") against real `coqc`.

Isabelle/HOL is classical, so `by simp` closes it either way; Lean's `decide` needs the
decidable `Bool` for the same reason Rocq does. The three lowerings therefore agree on `bool`
and would NOT agree on `Prop` — which is itself a finding about cross-logic portability that
only a non-arithmetic obligation could surface.
-/
import Concrete.Report.ReportObligations

namespace Concrete
namespace Report

open Concrete.TermIR

/-- One boolean postcondition obligation: `bodyExpr = postExpr`, over boolean variables.

    Restricted to a function whose body is a single `return <expr>`, because substituting the
    body into the postcondition is only sound when there is nothing else to execute — no
    assignment, no branch, no call with an effect. A wider body needs the operational step that
    the `refinement` profile names and this does not attempt. -/
structure BoolObl where
  fnQual   : String
  key      : String
  /-- Boolean parameter names, in declaration order. -/
  vars     : List String
  /-- The returned expression — the left side of the equivalence. -/
  bodyExpr : Expr
  /-- The postcondition's right side, from `#[ensures(result == …)]`. -/
  postExpr : Expr

/-- Is every parameter boolean, and is the return type boolean? -/
private def allBoolParams (f : FnDef) : Bool :=
  f.retTy == .bool && !f.params.isEmpty && f.params.all (fun p => p.ty == .bool)

/-- `#[ensures(result == <expr>)]` → `<expr>`. Only this exact shape; anything else is left
    alone rather than guessed at. -/
private def resultEquivOf : Expr → Option Expr
  | .binOp _ .eq (.ident _ "result") rhs => some rhs
  | .binOp _ .eq lhs (.ident _ "result") => some lhs
  | .paren _ e => resultEquivOf e
  | _ => none

/-- The single returned expression of a one-statement body. -/
private def soleReturnOf : List Stmt → Option Expr
  | [.return_ _ (some e)] => some e
  | _ => none

/-- Collect boolean postcondition obligations.

    Deliberately narrow: all-boolean signature, a single `return`, and an `#[ensures]` of the
    form `result == <expr>`. Everything outside that shape is skipped, and `boolKernelSkipped`
    reports the count so "0 obligations" cannot be read as "nothing to prove here". -/
def boolPostObligations (modules : List Module) : List BoolObl := Id.run do
  let mut out : List BoolObl := []
  for (pfx, f) in modules.flatMap allFunctions do
    if allBoolParams f then
      match soleReturnOf f.body with
      | none => pure ()
      | some body =>
        let mut i := 0
        for ens in f.ensures do
          match resultEquivOf ens with
          | none => pure ()
          | some post =>
            out := out ++ [{ fnQual := pfx ++ f.name, key := s!"{pfx}{f.name}#boolpost{i}"
                           , vars := f.params.map (·.name), bodyExpr := body, postExpr := post }]
            i := i + 1
  return out

/-- Functions with an all-boolean signature that were SKIPPED, and why — so coverage is stated
    rather than implied. -/
def boolKernelSkipped (modules : List Module) : List (String × String) := Id.run do
  let mut out : List (String × String) := []
  for (pfx, f) in modules.flatMap allFunctions do
    if allBoolParams f then
      if (soleReturnOf f.body).isNone then
        out := out ++ [(pfx ++ f.name, "body is not a single `return` (needs the operational step)")]
      else if f.ensures.all (fun e => (resultEquivOf e).isNone) then
        out := out ++ [(pfx ++ f.name, "no `#[ensures(result == …)]` postcondition")]
  return out

/-! ## Exhaustive agreement with the reference evaluator

The arithmetic tier checks its lowerings on chosen instances. Over booleans every assignment can
be enumerated, so agreement is **decided**, not sampled. -/

/-- All `2^n` boolean assignments to `vars`. -/
def allAssignments : List String → List BoolEnv
  | [] => [[]]
  | v :: rest => (allAssignments rest).flatMap (fun a => [(v, true) :: a, (v, false) :: a])

/-- The obligation's truth value under one assignment: does the body agree with the
    postcondition? `none` if either side is not evaluable, which must NOT be read as `true`. -/
def BoolObl.evalUnder (o : BoolObl) (benv : BoolEnv) : Option Bool := do
  let l ← evalBoolEnv [] o.bodyExpr benv
  let r ← evalBoolEnv [] o.postExpr benv
  some (l == r)

/-- Is the obligation TRUE on every assignment? `none` if any assignment fails to evaluate —
    an unevaluable row makes the answer unknown rather than false. -/
def BoolObl.holdsEverywhere (o : BoolObl) : Option Bool := Id.run do
  let mut sawUnknown := false
  let mut allTrue := true
  for benv in allAssignments o.vars do
    match o.evalUnder benv with
    | none => sawUnknown := true
    | some b => if !b then allTrue := false
  return if sawUnknown then none else some allTrue

/-! ## The three lowerings

Each kernel gets a boolean rendering with its OWN decision procedure — not one shared arithmetic
tactic. That is the point of a non-arithmetic tier: `lia`/`presburger`/`omega` are useless here,
so agreement between the kernels is not three invocations of the same idea.

| kernel | sort | tactic | refusal marker (already known to the classifier) |
|--------|------|--------|--------------------------------------------------|
| Rocq | `bool` | `destruct …; reflexivity` | `Unable to unify` |
| Isabelle/HOL | `bool` | `auto` (classical; `simp` is too weak — see below) | `Failed to finish proof` |
| Lean | `Bool` | `decide` | `decide` reports the proposition FALSE |

All three were validated against the real tools before any of this was wired up, including the
negative controls: a false boolean claim is REFUSED by each, with the marker above. -/

/-- Boolean operator spelling per kernel: `(and, or, not, eq)`. Rocq and Lean use the decidable
    `bool`/`Bool` operators; Isabelle's inner syntax is classical HOL but agrees on `bool`. -/
private structure BoolOps where
  and_ : String
  or_  : String
  not_ : String
  /-- TOP-LEVEL equivalence: the lemma's own `=`, at proposition level. -/
  eqv  : String
  /-- NESTED boolean equality, which is NOT the same thing. In Rocq `a = b` on `bool` is a
      `Prop`, so `negb (a = b)` is a type error — the emitted `xor` script failed exactly that
      way, while the `rfl` locks passed. Isabelle's HOL `=` on `bool` is already boolean, and
      Lean needs `==` because `=` yields a `Prop` there too. Three kernels, three answers to one
      question the renderer had been treating as uniform. -/
  eqNested : String → String → String

private def rocqBoolOps : BoolOps :=
  { and_ := "&&", or_ := "||", not_ := "negb", eqv := "="
  , eqNested := fun a b => s!"(eqb {a} {b})" }
private def isaBoolOps : BoolOps :=
  { and_ := "&", or_ := "|", not_ := "~", eqv := "="
  , eqNested := fun a b => s!"({a} = {b})" }
private def leanBoolOps : BoolOps :=
  { and_ := "&&", or_ := "||", not_ := "!", eqv := "="
  , eqNested := fun a b => s!"({a} == {b})" }

/-- Render a boolean expression with one kernel's spellings. `none` outside the fragment —
    an integer literal or a call has no boolean meaning here and must not be invented. -/
private def renderBool (ops : BoolOps) : Expr → Option String
  | .ident _ n => some n
  | .boolLit _ b => some (if b then "true" else "false")
  | .paren _ e => renderBool ops e
  | .unaryOp _ .not_ e => do let x ← renderBool ops e; some s!"({ops.not_} {x})"
  | .binOp _ .and_ l r => do
      let a ← renderBool ops l; let b ← renderBool ops r; some s!"({a} {ops.and_} {b})"
  | .binOp _ .or_ l r => do
      let a ← renderBool ops l; let b ← renderBool ops r; some s!"({a} {ops.or_} {b})"
  | .binOp _ .eq l r => do
      let a ← renderBool ops l; let b ← renderBool ops r; some (ops.eqNested a b)
  | .binOp _ .neq l r => do
      let a ← renderBool ops l; let b ← renderBool ops r
      some s!"({ops.not_} {ops.eqNested a b})"
  | _ => none

/-- The obligation as one kernel's proposition: `body = post`. -/
def BoolObl.prop (o : BoolObl) (ops : BoolOps) : Option String := do
  let l ← renderBool ops o.bodyExpr
  let r ← renderBool ops o.postExpr
  some s!"{l} {ops.eqv} {r}"

/-- Rocq script. `destruct` on every boolean variable then `reflexivity` — constructive, so
    `Print Assumptions` reports "Closed under the global context" rather than a classical axiom. -/
def BoolObl.rocqScript (o : BoolObl) : Option String := do
  let p ← o.prop rocqBoolOps
  let binder := if o.vars.isEmpty then "" else s!"forall ({" ".intercalate o.vars} : bool), "
  let dest := if o.vars.isEmpty then "" else s!"destruct {", ".intercalate o.vars}; "
  some <| "\n".intercalate
    [ "(* non-arithmetic multi-kernel: a BOOLEAN postcondition, closed by case analysis. *)"
    -- REQUIRED. `&&` and `||` live in `bool_scope`; without this the script fails with
    -- `Unknown interpretation for notation "_ || _"`. The `rfl` locks below pinned the
    -- rendering and passed while the emitted script did not compile at all -- caught only by
    -- running `coqc` on the generated output, which is why the gate does that rather than
    -- trusting the locks.
    -- `eqb` (boolean equality) comes from `Bool`; `&&`/`||` from `bool_scope`.
    , "From Stdlib Require Import Bool."
    , "Open Scope bool_scope."
    , s!"Lemma boolpost : {binder}{p}."
    , s!"Proof. {dest}reflexivity. Qed."
    , "Print Assumptions boolpost." ]

/-- Isabelle/HOL theory. HOL is classical, but the goal is over `bool` either way, so `simp`
    closes it without the lowering depending on classicality. -/
def BoolObl.isabelleTheory (o : BoolObl) (thyName : String) : Option String := do
  let p ← o.prop isaBoolOps
  let binder := if o.vars.isEmpty then "" else s!"ALL {" ".intercalate o.vars}::bool. "
  some <| "\n".intercalate
    [ s!"theory {thyName}", "imports Main", "begin"
    , s!"lemma boolpost: \"{binder}{p}\""
    -- `auto`, not `simp`. `simp` is pure rewriting and could not finish `xor`: it reduced the
    -- goal to a TRUE residual (`(a | b) & (a --> ~b)) = (a = ~b)`) and stopped. Propositional
    -- tautologies need classical reasoning, which is Isabelle's own answer to what Rocq does
    -- with `destruct` -- so each kernel still uses its native procedure, none of them
    -- arithmetic. Found by running the generated theory, not by reading it.
    , "  by auto", "end" ]

/-- Lean goal, closed by `decide` — which is exactly why the sort must be the decidable `Bool`. -/
def BoolObl.leanGoal (o : BoolObl) : Option String := do
  let p ← o.prop leanBoolOps
  let binder := if o.vars.isEmpty then "" else s!"∀ {" ".intercalate o.vars} : Bool, "
  some s!"{binder}{p}"

/-! ### Locks

The renderings are pinned here rather than only exercised end-to-end, so changing a spelling is a
build failure and not a silently different proposition sent to a kernel. -/

private def spB : Span := default
private def nandBody : Expr :=
  .binOp spB .or_ (.unaryOp spB .not_ (.ident spB "a")) (.unaryOp spB .not_ (.ident spB "b"))
private def nandPost : Expr :=
  .unaryOp spB .not_ (.binOp spB .and_ (.ident spB "a") (.ident spB "b"))
private def nandObl : BoolObl :=
  { fnQual := "d.nand", key := "d.nand#boolpost0", vars := ["a", "b"]
  , bodyExpr := nandBody, postExpr := nandPost }

-- De Morgan, in each kernel's spelling. These exact strings were run through `coqc`,
-- `isabelle build` and `lean` before being written down.
example : nandObl.prop rocqBoolOps = some "((negb a) || (negb b)) = (negb (a && b))" := rfl
example : nandObl.prop isaBoolOps  = some "((~ a) | (~ b)) = (~ (a & b))" := rfl
example : nandObl.leanGoal = some "∀ a b : Bool, ((! a) || (! b)) = (! (a && b))" := rfl

-- Rocq's proof is `destruct a, b; reflexivity` — constructive on `bool`. Over `Prop` this
-- direction of De Morgan needs classical logic, which is why the sort choice is not cosmetic.
-- Only renderability is pinned here: whether the SCRIPT is correct is settled by `coqc`
-- accepting it in the gate, not by a substring check that would pass on a malformed proof.
example : nandObl.rocqScript.isSome = true := rfl
example : (nandObl.isabelleTheory "BoolPost").isSome = true := rfl

-- EXHAUSTIVE agreement: De Morgan holds on all four assignments, decided rather than sampled.
example : nandObl.holdsEverywhere = some true := rfl
example : (allAssignments ["a", "b"]).length = 4 := rfl

-- And the check discriminates: `a || b` is NOT `!(a && b)`, so a wrong postcondition is caught
-- by the reference evaluator before any kernel is invoked.
private def badObl : BoolObl :=
  { nandObl with bodyExpr := .binOp spB .or_ (.ident spB "a") (.ident spB "b") }
example : badObl.holdsEverywhere = some false := rfl

-- Outside the fragment renders as `none` rather than being guessed at.
example : renderBool rocqBoolOps (.intLit spB 3) = none := rfl

/-! # Rung 5 — uninterpreted functions (EUF)

A `spec fn f(x: i32) -> i32;` is body-less, pure and erased: an **uninterpreted function**. The
string lowering drops calls entirely today (`exprToProver` returns `none`), which is the
documented gap this closes.

## Two design decisions, both load-bearing

**The symbol becomes a QUANTIFIED FUNCTION VARIABLE, not a declared constant.** `Parameter f : Z
-> Z` in Rocq, or an `axiom` in Lean, would put a declaration into the trusted base and
`Print Assumptions` would report it — turning an axiom-free attestation into an axiom-bearing one.
Quantifying instead (`forall (f : Z -> Z), …`) proves the goal for EVERY interpretation of `f`,
which is both stronger and axiom-free. Verified: both Rocq lemmas report
"Closed under the global context".

**This tier has a SEPARATE renderer from the boolean one, deliberately.** EUF goals live at
proposition level (`(t = f m) <-> (f m = t)`); boolean postconditions live at `bool` level
(`(!a || !b) = !(a && b)`). Forcing one renderer to do both is exactly what produced the `xor`
type error — nested `a = b` emitted as a `bool` when it was a `Prop`. Two sorts, two renderers.

## What DEGRADES here, stated plainly

The boolean tier's agreement check is **exhaustive**: `2^n` assignments decide whether a lowering
means the same proposition. That is impossible for EUF — an uninterpreted symbol has infinitely
many interpretations, so there is no finite table to enumerate.

What replaces it is **propositional abstraction**: each distinct comparison containing a spec call
becomes a boolean atom, and the goal is checked as a propositional tautology over all assignments
to those atoms. That is **sound** — a propositional tautology holds under every interpretation of
the symbols — and **incomplete**: `x = y -> f x = f y` is EUF-valid but NOT a propositional
tautology over the atoms `x = y` and `f x = f y`, because the abstraction forgets that `f` is a
function. So a goal the reference check cannot confirm may still be closed by the kernels, and
this tier reports `abstraction: inconclusive` rather than claiming agreement.

That is the first rung where the reference check stops being decisive, and it is the same
trade the roadmap records for induction — one rung earlier and milder.
-/

/-- One EUF obligation: `bodyExpr <-> postExpr` at proposition level, over quantified
    integer variables and quantified uninterpreted symbols. -/
structure EufObl where
  fnQual   : String
  key      : String
  /-- Uninterpreted symbols used, as `(name, arity)`. Quantified, never declared. -/
  symbols  : List (String × Nat)
  /-- Integer variables to quantify. -/
  intVars  : List String
  /-- `#[requires]` clauses — the hypotheses. -/
  hyps     : List Expr
  /-- The `#[ensures]` clause — the conclusion. -/
  concl    : Expr

/-- Every spec-function application in an expression, as `(name, arity)`. -/
partial def specCallsOf (specNames : List String) : Expr → List (String × Nat)
  | .call _ f _ args =>
      (if specNames.contains f then [(f, args.length)] else [])
        ++ args.flatMap (specCallsOf specNames)
  | .paren _ e | .unaryOp _ _ e | .cast _ e _ => specCallsOf specNames e
  | .binOp _ _ l r => specCallsOf specNames l ++ specCallsOf specNames r
  | _ => []

/-- Collect EUF obligations as `requires -> ensures`.

    NOT from the function body: a `spec fn` is **erased**, so it cannot appear in executable
    code at all (`E0101: unknown function` if you try). Uninterpreted symbols live only in
    contracts, which is why this tier reads the contract rather than substituting a body the way
    the boolean tier does.

    Clauses mentioning `result` are skipped: relating `result` to anything needs the operational
    step, which is the `refinement` profile's job and not attempted here. -/
def eufObligations (modules : List Module) : List EufObl := Id.run do
  let specNames := modules.flatMap (fun m => m.specFns.map (·.name))
  let mut out : List EufObl := []
  for (pfx, f) in modules.flatMap allFunctions do
    if !f.params.isEmpty && f.params.all (fun p => p.ty != .bool) then
      let mentionsResult : Expr → Bool := fun e =>
        ((Concrete.fmtExpr e).splitOn "result").length > 1
      let hyps := f.requires.filter (fun h => !mentionsResult h)
      let mut i := 0
      for ens in f.ensures do
        if !mentionsResult ens then
          let syms := ((hyps.flatMap (specCallsOf specNames))
                        ++ specCallsOf specNames ens).eraseDups
          if !syms.isEmpty then
            out := out ++ [{ fnQual := pfx ++ f.name, key := s!"{pfx}{f.name}#euf{i}"
                           , symbols := syms, intVars := f.params.map (·.name)
                           , hyps := hyps, concl := ens }]
            i := i + 1
  return out

/-! ### Proposition-level rendering, one spelling set per kernel -/

private structure PropOps where
  and_  : String
  or_   : String
  not_  : String
  /-- Integer comparison at Prop level. -/
  cmp   : BinOp → String → String → Option String
  /-- Bi-implication. -/
  iff   : String
  /-- `f a b` application. Also used for ARRAY INDEXING: with the bounds family already proving
      every index in range, a fixed-size array is a total `index -> value` function, so it needs
      no new theory — it is rung 5's machinery reused (rung 7). -/
  app   : String → List String → String
  /-- Field projection. Rocq and Isabelle put the field first (`magic h`), Lean puts it last
      (`h.magic`) — the one place the three genuinely disagree on syntax. -/
  proj  : String → String → String

private def rocqCmp : BinOp → String → String → Option String
  | .eq, a, b => some s!"({a} = {b})"
  | .neq, a, b => some s!"({a} <> {b})"
  | .lt, a, b => some s!"({a} < {b})"
  | .leq, a, b => some s!"({a} <= {b})"
  | .gt, a, b => some s!"({a} > {b})"
  | .geq, a, b => some s!"({a} >= {b})"
  | _, _, _ => none

private def isaCmp : BinOp → String → String → Option String
  | .eq, a, b => some s!"({a} = {b})"
  | .neq, a, b => some s!"({a} ~= {b})"
  | .lt, a, b => some s!"({a} < {b})"
  | .leq, a, b => some s!"({a} <= {b})"
  | .gt, a, b => some s!"({a} > {b})"
  | .geq, a, b => some s!"({a} >= {b})"
  | _, _, _ => none

private def leanCmp : BinOp → String → String → Option String
  | .eq, a, b => some s!"({a} = {b})"
  | .neq, a, b => some s!"({a} ≠ {b})"
  | .lt, a, b => some s!"({a} < {b})"
  | .leq, a, b => some s!"({a} ≤ {b})"
  | .gt, a, b => some s!"({a} > {b})"
  | .geq, a, b => some s!"({a} ≥ {b})"
  | _, _, _ => none

private def rocqPropOps : PropOps :=
  { and_ := "/\\", or_ := "\\/", not_ := "~", cmp := rocqCmp, iff := "<->"
  , app := fun f as => s!"({f} {" ".intercalate as})"
  , proj := fun fld o => s!"({fld} {o})" }
private def isaPropOps : PropOps :=
  { and_ := "&", or_ := "|", not_ := "~", cmp := isaCmp, iff := "="
  , app := fun f as => s!"({f} {" ".intercalate as})"
  , proj := fun fld o => s!"({fld} {o})" }
private def leanPropOps : PropOps :=
  { and_ := "∧", or_ := "∨", not_ := "¬", cmp := leanCmp, iff := "↔"
  , app := fun f as => s!"({f} {" ".intercalate as})"
  , proj := fun fld o => s!"{o}.{fld}" }

/-- Integer-level term (inside a comparison): variables, literals, spec applications. -/
private partial def renderTerm (ops : PropOps) : Expr → Option String
  | .ident _ n => some n
  | .intLit _ v => some (toString v)
  | .paren _ e => renderTerm ops e
  | .call _ f _ args => do
      let as ← args.mapM (renderTerm ops)
      some (ops.app f as)
  | .binOp _ .add l r => do let a ← renderTerm ops l; let b ← renderTerm ops r; some s!"({a} + {b})"
  | .binOp _ .sub l r => do let a ← renderTerm ops l; let b ← renderTerm ops r; some s!"({a} - {b})"
  | .binOp _ .mul l r => do let a ← renderTerm ops l; let b ← renderTerm ops r; some s!"({a} * {b})"
  -- rung 6: struct field projection.
  | .fieldAccess _ o f => do let x ← renderTerm ops o; some (ops.proj f x)
  -- rung 7: array indexing as function application. Sound because the bounds family already
  -- proves the index in range, so the model needs no partiality.
  | .arrayIndex _ a i => do
      let arr ← renderTerm ops a; let idx ← renderTerm ops i; some (ops.app arr [idx])
  | _ => none

/-- Proposition-level formula: boolean structure over integer comparisons. -/
private partial def renderProp (ops : PropOps) : Expr → Option String
  | .paren _ e => renderProp ops e
  | .unaryOp _ .not_ e => do let x ← renderProp ops e; some s!"({ops.not_} {x})"
  | .binOp _ .and_ l r => do
      let a ← renderProp ops l; let b ← renderProp ops r; some s!"({a} {ops.and_} {b})"
  | .binOp _ .or_ l r => do
      let a ← renderProp ops l; let b ← renderProp ops r; some s!"({a} {ops.or_} {b})"
  | .binOp _ op l r => do
      let a ← renderTerm ops l; let b ← renderTerm ops r; ops.cmp op a b
  | _ => none

/-- Atom key for the abstraction, with EQUALITY NORMALISED.

    `tag_of(m) = t` and `t = tag_of(m)` are the same atom, and rendering them as different
    strings made symmetry-through-an-opaque-term look falsifiable: the abstraction counted one
    atom as two independent ones. Equality and disequality are symmetric, so the two sides are
    sorted; the ordering comparisons are not, so they are left alone. -/
private partial def atomKey (ops : PropOps) : Expr → Option String
  | .paren _ e => atomKey ops e
  | .binOp _ op l r =>
      if op == .eq || op == .neq then do
        let a ← renderTerm ops l
        let b ← renderTerm ops r
        let (x, y) := if a ≤ b then (a, b) else (b, a)
        ops.cmp op x y
      else do
        let a ← renderTerm ops l; let b ← renderTerm ops r; ops.cmp op a b
  | _ => none

/-- Operand pairs of EQUALITY atoms, for decidable case analysis. Normalised the same way as
    `atomKey` so `a = b` and `b = a` yield one split. -/
private partial def eqPairs (ops : PropOps) : Expr → List (String × String)
  | .paren _ e => eqPairs ops e
  | .unaryOp _ .not_ e => eqPairs ops e
  | .binOp _ .and_ l r => eqPairs ops l ++ eqPairs ops r
  | .binOp _ .or_ l r => eqPairs ops l ++ eqPairs ops r
  | .binOp _ op l r =>
      if op == .eq || op == .neq then
        match renderTerm ops l, renderTerm ops r with
        | some a, some b => [if a ≤ b then (a, b) else (b, a)]
        | _, _ => []
      else []
  | _ => []

/-- The obligation as one kernel's proposition: `h1 -> h2 -> … -> concl`. -/
def EufObl.prop (o : EufObl) (ops : PropOps) (arrow : String) : Option String := do
  let hs ← o.hyps.mapM (renderProp ops)
  let c ← renderProp ops o.concl
  some (String.join (hs.map (fun h => s!"({h}) {arrow} ")) ++ s!"({c})")

/-- Rocq: symbols and variables quantified, closed by `congruence` — which reasons about
    equality with uninterpreted functions and nothing more. -/
def EufObl.rocqScript (o : EufObl) : Option String := do
  let p ← o.prop rocqPropOps "->"
  let pairs := ((o.hyps.flatMap (eqPairs rocqPropOps)) ++ eqPairs rocqPropOps o.concl).eraseDups
  let eqDestructs := pairs.map fun (a, b) => s!"destruct (Z.eq_dec {a} {b})"
  let symBinders := o.symbols.map fun (n, ar) =>
    s!"({n} : {" -> ".intercalate (List.replicate ar "Z")} -> Z)"
  let varBinder := if o.intVars.isEmpty then "" else s!"({" ".intercalate o.intVars} : Z) "
  some <| "\n".intercalate
    [ "(* EUF: the spec function is a QUANTIFIED variable, not a Parameter — so"
    , "   Print Assumptions stays clean and the result holds for every interpretation. *)"
    , "From Stdlib Require Import ZArith."
    , "Open Scope Z_scope."
    , s!"Lemma eufgoal : forall {" ".intercalate symBinders} {varBinder}, {p}."
    -- `congruence` alone is not enough. The De Morgan instance `~(A /\ B) -> ~A \/ ~B` is
    -- CLASSICALLY valid and not intuitionistically provable, and Rocq's `tauto` is
    -- intuitionistic. Importing `Classical` would close it and put an axiom in the
    -- attestation, which `Print Assumptions` would then report. Instead: case-split on the
    -- decidable equality atoms (`Z.eq_dec`), which is constructive and keeps the proof
    -- "Closed under the global context". Same reason Lean needs `by_cases` rather than `simp_all`.
    , "Proof."
    , "  intros."
    , s!"  {String.join (eqDestructs.map (fun d => d ++ "; "))}solve [ tauto | congruence ]."
    , "Qed."
    , "Print Assumptions eufgoal." ]

/-- Isabelle/HOL: `ALL f::int=>int. …`, closed by `auto`. -/
def EufObl.isabelleTheory (o : EufObl) (thyName : String) : Option String := do
  let p ← o.prop isaPropOps "-->"
  let symBinders := o.symbols.map fun (n, ar) =>
    s!"({n}::{" => ".intercalate (List.replicate ar "int")} => int)"
  let varBinders := o.intVars.map (fun v => s!"({v}::int)")
  some <| "\n".intercalate
    [ s!"theory {thyName}", "imports Main", "begin"
    , s!"lemma eufgoal: \"ALL {" ".intercalate (symBinders ++ varBinders)}. {p}\""
    , "  by auto", "end" ]

/-- Lean goal. Function variables are quantified for the same reason. -/
def EufObl.leanGoal (o : EufObl) : Option String := do
  let p ← o.prop leanPropOps "→"
  let symBinders := o.symbols.map fun (n, ar) =>
    s!"({n} : {" → ".intercalate (List.replicate ar "Int")} → Int)"
  let varBinder := if o.intVars.isEmpty then "" else s!"({" ".intercalate o.intVars} : Int) "
  some s!"∀ {" ".intercalate symBinders} {varBinder}, {p}"

/-! ### Propositional abstraction — sound, and explicitly incomplete -/

/-- Distinct comparison atoms, keyed by their Lean rendering. -/
private partial def atomsOf (ops : PropOps) : Expr → List String
  | .paren _ e => atomsOf ops e
  | .unaryOp _ .not_ e => atomsOf ops e
  | .binOp _ .and_ l r => atomsOf ops l ++ atomsOf ops r
  | .binOp _ .or_ l r => atomsOf ops l ++ atomsOf ops r
  | e => match atomKey ops e with | some a => [a] | none => []

/-- Truth of a formula under an assignment to its atoms. -/
private partial def evalAbstract (ops : PropOps) (asg : List (String × Bool)) :
    Expr → Option Bool
  | .paren _ e => evalAbstract ops asg e
  | .unaryOp _ .not_ e => (evalAbstract ops asg e).map (! ·)
  | .binOp _ .and_ l r => do
      let a ← evalAbstract ops asg l; let b ← evalAbstract ops asg r; some (a && b)
  | .binOp _ .or_ l r => do
      let a ← evalAbstract ops asg l; let b ← evalAbstract ops asg r; some (a || b)
  | e => do let a ← atomKey ops e; asg.lookup a

/-- Is the obligation a propositional TAUTOLOGY over its atoms?

    `some ()` — a tautology, therefore valid under EVERY interpretation of the uninterpreted
    symbols. This direction is sound.

    `none` — **inconclusive, and nothing more.** Propositional abstraction is sound in one
    direction only: a tautology implies validity, but a falsifying assignment implies NOTHING,
    because the abstraction forgets that `f` is a function. `m = t -> tag_of m = tag_of t` is
    EUF-valid and has a falsifying assignment under abstraction, since `m = t` and
    `tag_of m = tag_of t` are treated as unrelated atoms.

    An earlier version of this returned a third verdict, "FALSE under some assignment — no
    kernel should close it", which is simply wrong: it labelled congruence — the defining
    property of an uninterpreted function — as something no kernel should prove. Only the
    kernels can refute an EUF goal; the abstraction can only ever confirm. -/
def EufObl.abstractionVerdict (o : EufObl) : Option Unit := Id.run do
  let atoms := ((o.hyps.flatMap (atomsOf leanPropOps)) ++ atomsOf leanPropOps o.concl).eraseDups
  if atoms.length > 12 then return none   -- 2^12 rows; refuse rather than hang
  let mut tautology := true
  for bits in allAssignments atoms do
    let hs := o.hyps.map (evalAbstract leanPropOps bits)
    match evalAbstract leanPropOps bits o.concl with
    | none => tautology := false
    | some c =>
      if hs.any (·.isNone) then tautology := false
      else if (hs.all (· == some true)) && !c then tautology := false
  return if tautology then some () else none

/-- Lean SCRIPT, with named binders and explicit case analysis.

    Three things forced this shape, each found by running it:

    * `intros` produces INACCESSIBLE names (`tag_of✝`), so the atoms cannot be named in
      `by_cases` afterwards. The generator knows the binder names, so it introduces them
      explicitly.
    * `simp_all` alone does not close these. The De Morgan instance is
      `(A → ¬B) → ¬A ∨ ¬B`, which is **classically** valid and not constructively provable — and
      this repo has no Mathlib, so no `tauto`.
    * `by_cases` nonetheless needs no classical axiom here, because the atoms are `Int`
      equalities and therefore DECIDABLE. Case analysis on a decidable proposition is
      constructive.

    Honest cross-kernel difference: `#print axioms` reports `propext` for these (from
    `simp_all`), whereas Rocq reports "Closed under the global context". Lean's proofs here are
    not axiom-free in the strict sense; `propext` is a standard Lean axiom, not classical choice. -/
def EufObl.leanScript (o : EufObl) : Option String := do
  let g ← o.leanGoal
  let names := o.symbols.map (·.1) ++ o.intVars ++ (List.range o.hyps.length).map (fun i => s!"h{i}")
  let atoms := ((o.hyps.flatMap (atomsOf leanPropOps)) ++ atomsOf leanPropOps o.concl).eraseDups
  let cases := atoms.zipIdx.map fun (a, i) =>
    if i == 0 then s!"  by_cases c{i} : {a}" else s!"  all_goals by_cases c{i} : {a}"
  some <| "\n".intercalate
    ([ s!"theorem eufgoal : {g} := by"
     , s!"  intro {" ".intercalate names}" ] ++ cases ++ [ "  all_goals simp_all" ])



/-! # Rungs 6 + 7 — algebraic datatypes and arrays

Nothing currently tells any prover that a Concrete struct exists: `CoreExtract` emits Gallina
`Definition`s and no `Inductive`/`Record` at all. This tier emits the DECLARATION alongside the
lemma, so a structural obligation can be stated.

**Arrays come free.** A fixed-size Concrete array with every index already proved in range by the
bounds family is a total `index -> value` function, so it is rung 5's uninterpreted-symbol
machinery with no new theory. That is why rungs 6 and 7 land together: the array half needs a
renderer case, not a theory.

**Where the three kernels genuinely differ:** field projection. Rocq and Isabelle put the field
first (`magic h`), Lean puts it last (`h.magic`), and the declaration syntax differs three ways
(`Record` / `record` / `structure`). The proposition itself is otherwise shared.
-/

/-- A struct obligation: `requires -> ensures` over field projections and array indexing, with
    the struct declarations the goal needs. -/
structure StructObl where
  fnQual  : String
  key     : String
  /-- Struct declarations to emit: `(name, [(field, isInt)])`. Only integer fields are modelled;
      a struct with a field this tier cannot render is skipped rather than half-declared. -/
  structs : List (String × List String)
  /-- Array parameters, modelled as uninterpreted `index -> value` functions (rung 7). -/
  arrays  : List String
  /-- Struct-typed variables to quantify, as `(name, structName)`. -/
  objs    : List (String × String)
  intVars : List String
  hyps    : List Expr
  concl   : Expr

/-- Struct-typed and array-typed parameters of a function, with their integer fields. -/
private def structParamsOf (structs : StructFieldEnv) (f : FnDef) :
    List (String × String) × List String :=
  let objs := f.params.filterMap fun p =>
    match namedStructOf p.ty with
    | some sn => if (structs.lookup sn).isSome then some (p.name, sn) else none
    | none => none
  let arrs := f.params.filterMap fun p =>
    match p.ty with | .array _ _ => some p.name | _ => none
  (objs, arrs)

/-- Collect datatype/array obligations from contracts, same `requires -> ensures` shape as EUF. -/
def structObligations (modules : List Module) : List StructObl := Id.run do
  let senv := structFieldEnv modules
  let mut out : List StructObl := []
  for (pfx, f) in modules.flatMap allFunctions do
    let (objs, arrs) := structParamsOf senv f
    if !objs.isEmpty || !arrs.isEmpty then
      let mentionsResult : Expr → Bool := fun e =>
        ((Concrete.fmtExpr e).splitOn "result").length > 1
      let hyps := f.requires.filter (fun h => !mentionsResult h)
      let mut i := 0
      for ens in f.ensures do
        if !mentionsResult ens then
          -- integer fields only: a field this tier cannot render must not be declared as if it
          -- could, or the emitted declaration would not match the obligation.
          let decls := objs.map (fun (_, sn) =>
            (sn, ((senv.lookup sn).getD []).filterMap (fun (fn, ty) =>
                    if (IntArith.intBitWidth ty).isSome || ty == .int then some fn else none)))
          out := out ++ [{ fnQual := pfx ++ f.name, key := s!"{pfx}{f.name}#struct{i}"
                         , structs := decls.eraseDups, arrays := arrs, objs := objs
                         , intVars := f.params.filterMap (fun p =>
                             match p.ty with
                             | .array _ _ => none
                             | t => if (namedStructOf t).isSome then none else some p.name)
                         , hyps := hyps, concl := ens }]
          i := i + 1
  return out

/-- Same `h1 -> … -> concl` shape as EUF. Separate from `EufObl.prop` because the two carry
    different binder sets, not because the proposition differs. -/
def StructObl.prop (o : StructObl) (ops : PropOps) (arrow : String) : Option String := do
  let hs ← o.hyps.mapM (renderProp ops)
  let c ← renderProp ops o.concl
  some (String.join (hs.map (fun h => s!"({h}) {arrow} ")) ++ s!"({c})")

/-- Rocq: `Record` declarations, then the lemma. Decidable case analysis keeps it axiom-free. -/
def StructObl.rocqScript (o : StructObl) : Option String := do
  let p ← o.prop rocqPropOps "->"
  let decls := o.structs.map fun (sn, flds) =>
    let fieldsStr := " ; ".intercalate (flds.map (fun f => s!"{f} : Z"))
    -- built separately: Rocq's record braces need literal `{`/`}`, and nesting an
    -- interpolation inside escaped braces does not parse.
    "Record " ++ sn ++ " : Set := mk" ++ sn ++ " { " ++ fieldsStr ++ " }."
  let arrBinders := o.arrays.map (fun a => s!"({a} : Z -> Z)")
  let objBinders := o.objs.map (fun (n, sn) => s!"({n} : {sn})")
  let varBinder := if o.intVars.isEmpty then "" else s!"({" ".intercalate o.intVars} : Z) "
  let pairs := ((o.hyps.flatMap (eqPairs rocqPropOps)) ++ eqPairs rocqPropOps o.concl).eraseDups
  let dests := pairs.map fun (a, b) => s!"destruct (Z.eq_dec {a} {b})"
  some <| "\n".intercalate
    ([ "(* rungs 6+7: struct declarations emitted with the goal; arrays are total"
     , "   index -> value functions, sound because bounds are proved elsewhere. *)"
     , "From Stdlib Require Import ZArith."
     , "Open Scope Z_scope." ] ++ decls ++
     [ s!"Lemma structgoal : forall {" ".intercalate (arrBinders ++ objBinders)} {varBinder}, {p}."
     , "Proof."
     , "  intros."
     , s!"  {String.join (dests.map (fun d => d ++ "; "))}solve [ tauto | congruence ]."
     , "Qed."
     , "Print Assumptions structgoal." ])

/-- Isabelle/HOL: `record` declarations, then the lemma. -/
def StructObl.isabelleTheory (o : StructObl) (thyName : String) : Option String := do
  let p ← o.prop isaPropOps "-->"
  let decls := o.structs.map fun (sn, flds) =>
    s!"record {sn} = {"  ".intercalate (flds.map (fun f => s!"{f} :: int"))}"
  let binders := o.arrays.map (fun a => s!"({a}::int => int)")
                  ++ o.objs.map (fun (n, sn) => s!"({n}::{sn})")
                  ++ o.intVars.map (fun v => s!"({v}::int)")
  some <| "\n".intercalate
    ([ s!"theory {thyName}", "imports Main", "begin" ] ++ decls ++
     [ s!"lemma structgoal: \"ALL {" ".intercalate binders}. {p}\""
     , "  by auto", "end" ])

/-- Lean: `structure` declarations, then the theorem. -/
def StructObl.leanScript (o : StructObl) : Option String := do
  let p ← o.prop leanPropOps "→"
  let decls := o.structs.map fun (sn, flds) =>
    s!"structure {sn} where\n" ++ "\n".intercalate (flds.map (fun f => s!"  {f} : Int"))
  let binders := o.arrays.map (fun a => s!"({a} : Int → Int)")
                  ++ o.objs.map (fun (n, sn) => s!"({n} : {sn})")
                  ++ (if o.intVars.isEmpty then [] else [s!"({" ".intercalate o.intVars} : Int)"])
  let names := o.arrays ++ o.objs.map (·.1) ++ o.intVars
                ++ (List.range o.hyps.length).map (fun i => s!"h{i}")
  let atoms := ((o.hyps.flatMap (atomsOf leanPropOps)) ++ atomsOf leanPropOps o.concl).eraseDups
  let cases := atoms.zipIdx.map fun (a, i) =>
    if i == 0 then s!"  by_cases c{i} : {a}" else s!"  all_goals by_cases c{i} : {a}"
  some <| "\n".intercalate
    (decls ++
     [ s!"theorem structgoal : ∀ {" ".intercalate binders} , {p} := by"
     , s!"  intro {" ".intercalate names}" ] ++ cases ++ [ "  all_goals simp_all" ])

/-- Propositional-abstraction verdict, same soundness caveat as EUF: it can confirm, never
    refute. Field projections and array reads are atoms like any other opaque term. -/
def StructObl.abstractionVerdict (o : StructObl) : Option Unit := Id.run do
  let atoms := ((o.hyps.flatMap (atomsOf leanPropOps)) ++ atomsOf leanPropOps o.concl).eraseDups
  if atoms.length > 12 then return none
  let mut taut := true
  for bits in allAssignments atoms do
    let hs := o.hyps.map (evalAbstract leanPropOps bits)
    match evalAbstract leanPropOps bits o.concl with
    | none => taut := false
    | some c =>
      if hs.any (·.isNone) then taut := false
      else if (hs.all (· == some true)) && !c then taut := false
  return if taut then some () else none


/-! # Refinement against a DEFINED spec — the coverage fix

The measured problem: these tiers reach **zero** of the corpus's real contracts, because those are
refinement obligations (`result == spec(args)`) and a body-less `spec fn` is uninterpreted — so the
claim "this body equals that spec" is unprovable by construction. The repo discharges them by
linking to a Lean proof where the spec has a definition, which means Rocq and Isabelle can never
see the spec at all.

A `spec fn` may now carry a definitional body in Concrete itself:

```
spec fn double_spec(x: i32) -> i32 = x + x;
```

Then a refinement obligation becomes an ordinary equation between two expressions of this
language — `2 * x == x + x` — and every kernel can prove it. The spec's meaning stops being a name
pointing into one prover.

**What this does and does not change.** It makes refinement PORTABLE, not easier: the obligation is
still exactly "body equals spec", and if the two genuinely differ every kernel refuses it (gated).
A spec left body-less keeps its old uninterpreted meaning, so nothing existing shifts. -/

/-- Substitute `params := args` throughout an expression. -/
partial def substExpr (env : List (String × Expr)) : Expr → Expr
  | .ident sp n => match env.lookup n with | some e => e | none => .ident sp n
  | .paren sp e => .paren sp (substExpr env e)
  | .unaryOp sp op e => .unaryOp sp op (substExpr env e)
  | .binOp sp op l r => .binOp sp op (substExpr env l) (substExpr env r)
  | .cast sp e t => .cast sp (substExpr env e) t
  | .call sp f ts args => .call sp f ts (args.map (substExpr env))
  | .fieldAccess sp o f => .fieldAccess sp (substExpr env o) f
  | .arrayIndex sp a i => .arrayIndex sp (substExpr env a) (substExpr env i)
  | e => e

/-- `body == spec(args)` with the spec's definition substituted in. -/
structure RefineObl where
  fnQual   : String
  key      : String
  specName : String
  intVars  : List String
  /-- The function's returned expression. -/
  lhs      : Expr
  /-- The spec's body with its parameters replaced by the call's arguments. -/
  rhs      : Expr
  /-- The spec RESTATES the implementation: after substitution the two sides are syntactically
      identical, so the obligation is `body = body` and proves nothing.

      This is not hypothetical. `hmac_sha256`'s registered spec for `ch` is
      `(x AND y) XOR ((x XOR 0xFFFFFFFF) AND z)` — character for character the function body.
      Giving that spec a definitional body would make its refinement obligation a tautology and
      the coverage number would rise while nothing new was proved. Detected and reported instead,
      because manufactured coverage is worse than an honest zero. -/
  trivial  : Bool

/-- Collect refinement obligations whose spec has a DEFINITION. A body-less spec is skipped and
    reported, since substituting nothing would silently produce a different obligation. -/
def refineObligations (modules : List Module) : List RefineObl := Id.run do
  let specs := modules.flatMap (·.specFns)
  let mut out : List RefineObl := []
  for (pfx, f) in modules.flatMap allFunctions do
    match soleReturnOf f.body with
    | none => pure ()
    | some body =>
      let mut i := 0
      for ens in f.ensures do
        match resultEquivOf ens with
        | some (.call _ sn _ args) =>
          match specs.find? (fun sd => sd.name == sn) with
          | some sd =>
            match sd.body with
            | some sb =>
              let env := (sd.params.map (·.name)).zip args
              let rhs := substExpr env sb
              out := out ++ [{ fnQual := pfx ++ f.name, key := s!"{pfx}{f.name}#refine{i}"
                             , specName := sn, intVars := f.params.map (·.name)
                             , lhs := body, rhs := rhs
                             , trivial := Concrete.fmtExpr body == Concrete.fmtExpr rhs }]
              i := i + 1
            | none => pure ()
          | none => pure ()
        | _ => pure ()
  return out

/-- Refinement obligations skipped because the spec has no body — the coverage gap, named. -/
def refineSkipped (modules : List Module) : List (String × String) := Id.run do
  let specs := modules.flatMap (·.specFns)
  let mut out : List (String × String) := []
  for (pfx, f) in modules.flatMap allFunctions do
    for ens in f.ensures do
      match resultEquivOf ens with
      | some (.call _ sn _ _) =>
        if (specs.find? (fun sd => sd.name == sn && sd.body.isSome)).isNone then
          out := out ++ [(pfx ++ f.name, s!"spec `{sn}` has no definitional body — meaning lives outside this language, so only Lean can discharge it")]
      | _ => pure ()
  return out

/-- The equation, at proposition level. -/
def RefineObl.prop (o : RefineObl) (ops : PropOps) : Option String := do
  let l ← renderTerm ops o.lhs
  let r ← renderTerm ops o.rhs
  some s!"({l}) = ({r})"

/-- Rocq: `lia` closes a linear equation; `ring` would too but `lia` is already the repo's
    arithmetic tactic and keeps the tactic set small. -/
def RefineObl.rocqScript (o : RefineObl) : Option String := do
  let p ← o.prop rocqPropOps
  let vb := if o.intVars.isEmpty then "" else s!"forall ({" ".intercalate o.intVars} : Z), "
  some <| "\n".intercalate
    [ "(* refinement against a DEFINED spec: an equation between two expressions of the"
    , "   source language, so every kernel can see it. *)"
    , "From Stdlib Require Import ZArith.", "From Stdlib Require Import Lia."
    , "Open Scope Z_scope."
    , s!"Lemma refines : {vb}{p}."
    , "Proof. intros; lia. Qed."
    , "Print Assumptions refines." ]

/-- Isabelle/HOL. -/
def RefineObl.isabelleTheory (o : RefineObl) (thyName : String) : Option String := do
  let p ← o.prop isaPropOps
  let vb := if o.intVars.isEmpty then "" else s!"ALL {" ".intercalate (o.intVars.map (fun v => s!"({v}::int)"))}. "
  some <| "\n".intercalate
    [ s!"theory {thyName}", "imports Main", "begin"
    , s!"lemma refines: \"{vb}{p}\""
    , "  by (simp add: algebra_simps)", "end" ]

/-- Lean: `omega` handles linear integer equations and is in core (no Mathlib `ring` here). -/
def RefineObl.leanScript (o : RefineObl) : Option String := do
  let p ← o.prop leanPropOps
  let vb := if o.intVars.isEmpty then "" else s!"∀ ({" ".intercalate o.intVars} : Int), "
  some s!"theorem refines : {vb}{p} := by\n  intro {" ".intercalate o.intVars}\n  omega"

/-! ## Coverage, as a number rather than a claim

These tiers are real capability with **no instances in the existing corpus**, which is easy to
forget and easy to overstate — I overstated it in the roadmap twice before measuring. So the report
counts it per file: how many contract clauses exist, how many reached a tier, and how many are
REFINEMENT obligations (`result == spec(…)`) that no tier here can discharge because an
uninterpreted spec makes them unprovable by construction. -/

/-- `(clauses, refinementClauses, reachedTiers)` for the PARSED PROGRAM, imports included.

    Not "for this file": `elf_header`'s own source has no contracts at all, yet this counts 2,
    because its imports have them. Labelling it per-file was wrong and would have made the
    coverage number quietly meaningless for any file that imports anything. -/
def coverageSummary (modules : List Module) : Nat × Nat × Nat :=
  let fns := modules.flatMap allFunctions
  let clauses := fns.foldl (fun n (_, f) => n + f.requires.length + f.ensures.length) 0
  -- Refinement obligations that remain UNREACHABLE, i.e. whose spec has no definitional body.
  -- Counting all refinement clauses here was wrong once the refinement tier existed: three of the
  -- four in the demo do reach a tier, and the metric still reported "0 reached, 4 refinement".
  let refinement := (refineSkipped modules).length
  -- A TRIVIAL refinement (spec restates the implementation) is not coverage and must not be
  -- counted as such, or the metric can be inflated by writing specs that copy the body.
  let reached := (boolPostObligations modules).length + (eufObligations modules).length
                   + (structObligations modules).length
                   + ((refineObligations modules).filter (fun o => !o.trivial)).length
  (clauses, refinement, reached)


end Report
end Concrete
