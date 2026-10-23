import Concrete.Elab.Core

namespace Concrete.CoreExtract

open Concrete

/-! # Core → Rocq extraction: a DIFFERENTIAL TEST of Core semantics

What this is, stated narrowly on purpose. It extracts a Core function into a Rocq
(Gallina) computation and cross-checks it against Concrete's Lean interpreter:

  * the interpreter evaluates `f(args)` — the existing oracle, already trusted enough
    to be the differential-testing reference for codegen;
  * the extracted definition is evaluated by Rocq's kernel, by asking it to check
    `Goal f_ext args = <interpreter's result>. Proof. reflexivity. Qed.`

`reflexivity` on closed integer arithmetic is decided by Rocq's own kernel reduction,
so agreement means two independently-implemented evaluators computed the same value,
and disagreement is a defect in one of them.

## What it does NOT establish

It is NOT bridge diversity, and earlier drafts of this module oversold it as such.
`--report multi-kernel` declines to attest that the **Core→obligation lowering** is
faithful — that lowering turns a Core body into a proposition like
`a+b ∈ [-2³¹, 2³¹-1]`, including collecting hypotheses. Nothing here checks that the
emitted obligation is the right safety condition for that site, or that its
hypotheses were gathered correctly. This module cross-checks Core's SEMANTICS; the
obligation-derivation step is attacked separately, and only partially, by the
`bridge-check` fuzzer (does a *proved* obligation get refuted by concrete execution).
Together they narrow the gap from two sides; neither proves faithfulness.

Nor is it a second bridge whose agreement reduces trust: two unverified translators
agreeing is evidence, not proof, and this one is a real component that can itself be
wrong — two bugs were found in it during development (definitions referencing
unextractable callees; and, in the sibling agreement check, Isabelle inferring a
fresh type variable per numeral). The sanctioned path to bridge trust is realization
proofs / a discharged bridge register, not extraction.

What it IS worth: a cheap, falsifiable net that catches semantic modelling errors no
amount of kernel agreement can, because every kernel would agree on a consistently
mis-lowered obligation. Demonstrated: swapping `Z.quot` for `Z.div` (truncating →
flooring division) yields `DISAGREE — Unable to unify "-3" with "div_safe (-7) 2"`.
It covers the sampled inputs only, and it can be demoted or deleted once bridge
register rows are discharged.

## Deliberate semantic care

Two places where a naive extraction is silently wrong, and where this differential
test earns its keep:

* **Division truncates toward zero.** Concrete's `/` and `%` follow `Int.tdiv` /
  `Int.tmod`. Rocq's `Z.div`/`Z.modulo` are FLOOR division, which differs on
  negative operands (`(-7)/2` is `-3` truncating, `-4` flooring). We emit
  `Z.quot`/`Z.rem`, which truncate.
* **Fixed-width arithmetic is checked, and traps.** The interpreter traps on
  overflow exactly as the compiled binary does; Gallina `Z` is unbounded. Rather
  than model traps, the harness only compares inputs on which the interpreter
  SUCCEEDS. An input that traps is skipped, never counted as agreement.

## Fragment

Supported: integer and boolean locals, `let`/assignment, `if`/`else` (statement and
expression form), `return`, direct calls to other extractable functions, and the
arithmetic/comparison/logical operators below.

Not supported, and reported rather than silently dropped: loops (Gallina needs a
termination argument — a fuel-parameterised encoding is the natural next step),
structs, enums, arrays, matches, borrows, casts, generics, floats, strings.
-/

/-- Rocq type for a Concrete type, within the extractable fragment. Every integer
    width maps to `Z`: the extraction models the VALUE, and width-dependent
    trapping is handled by only comparing non-trapping inputs (see the module
    docstring). `none` for anything outside the fragment. -/
def extractTy : Ty → Option String
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => some "Z"
  | .bool => some "bool"
  | _ => none

/-- Is this type an integer type in the fragment? Needed because Gallina is typed
    and comparisons must produce `bool`, not `Prop`. -/
def isIntTy : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | _ => false

/-- Gallina spelling of a binary operator, as a prefix application over `Z`/`bool`.

    Comparisons use the BOOLEAN-valued operations (`Z.leb`, not `Z.le`), because the
    extracted function is a computation whose `if` conditions must reduce, not a
    proposition. `div`/`mod` use `Z.quot`/`Z.rem` (truncating), NOT `Z.div`/`Z.modulo`
    (flooring) — see the module docstring. -/
def extractBinOp : BinOp → Option String
  | .add => some "Z.add"   | .sub => some "Z.sub"   | .mul => some "Z.mul"
  -- truncating division, matching Int.tdiv / Int.tmod
  | .div => some "Z.quot"  | .mod => some "Z.rem"
  | .eq  => some "Z.eqb"   | .neq => some "negb_Z_eqb"
  | .lt  => some "Z.ltb"   | .gt  => some "Z.gtb"
  | .leq => some "Z.leb"   | .geq => some "Z.geb"
  | .and_ => some "andb"   | .or_ => some "orb"
  | _ => none

/-- A Gallina preamble defining the few helpers the operator table needs that the
    Rocq stdlib does not provide directly. Emitted once per extracted file. -/
def extractPreamble : String :=
  "\n".intercalate
    [ "(* Core -> Rocq extraction (Core-semantics differential test). Values are Z; division",
      "   truncates toward zero (Z.quot/Z.rem), matching Concrete's Int.tdiv/Int.tmod",
      "   rather than Coq's flooring Z.div/Z.modulo. *)",
      "From Stdlib Require Import ZArith.",
      "Open Scope Z_scope.",
      "Definition negb_Z_eqb (a b : Z) : bool := negb (Z.eqb a b).",
      "" ]

/-- Does this block always leave the function (its last reachable statement is a
    `return`, or an `if`/`else` whose branches both do)? Required before modelling a
    guard as a Gallina `if`: `if c { return a; } rest` is `if c then a else rest` ONLY
    when the `then` branch cannot fall through into `rest`. If it could, the two paths
    would have to merge with mutated state, which the `let`-chain encoding cannot
    express — and assuming otherwise would silently extract the WRONG function. -/
partial def blockTerminates : List CStmt → Bool
  | [] => false
  | [.return_ _ _] => true
  | [.ifElse _ t (some e)] => blockTerminates t && blockTerminates e
  | _ :: rest => blockTerminates rest

mutual
/-- Extract a Core expression to a Gallina term. `none` if outside the fragment —
    callers must DROP the whole function rather than emit a partial body, so a
    construct we cannot model never silently becomes a passing test. -/
partial def extractExpr : CExpr → Option String
  | .intLit v _ => some (if v < 0 then s!"({v})" else s!"{v}")
  | .boolLit b => some (if b then "true" else "false")
  | .ident n _ => some n.name
  | .unaryOp op e _ => do
    let E ← extractExpr e
    match op with
    | .neg => some s!"(Z.opp {E})"
    | .not_ => some s!"(negb {E})"
    | _ => none
  | .binOp op l r _ => do
    let L ← extractExpr l
    let R ← extractExpr r
    let f ← extractBinOp op
    some s!"({f} {L} {R})"
  | .call callee _ args _ => do
    -- Only DIRECT calls: an indirect call through a function pointer has no
    -- statically-known callee to name in Gallina.
    let name ← callee.directName?
    let as ← args.mapM extractExpr
    some (if as.isEmpty then name else s!"({name} {" ".intercalate as})")
  | .ifExpr c t e _ => do
    let C ← extractExpr c
    let T ← extractStmts t
    let E ← extractStmts e
    some s!"(if {C} then {T} else {E})"
  | _ => none

/-- Extract a statement list to a single Gallina term. Statements become nested
    `let`s ending in the returned value, which is why the fragment excludes loops
    and early `return` from the middle of a block: both need control flow Gallina
    cannot express as a `let` chain.

    Accepts a block that ends in `return e` or a trailing value expression. -/
partial def extractStmts : List CStmt → Option String
  | [] => none
  | [.return_ (some e) _] => extractExpr e
  | [.expr e true] => extractExpr e
  -- A `return` mid-block makes everything after it dead code.
  | .return_ (some e) _ :: _ => extractExpr e
  -- GUARD / early-return: `if c { return a; } rest`. Extremely common in
  -- verification code (`if !ok { return 0; }` chains), and the single construct that
  -- kept crypto_verify's verify_message outside the fragment. Sound only when the
  -- `then` branch always leaves the function, so `rest` really is the else-path —
  -- hence the blockTerminates guard rather than an assumption.
  | .ifElse c t none :: rest => do
    if !blockTerminates t then none else do
      let C ← extractExpr c
      let T ← extractStmts t
      let R ← extractStmts rest
      some s!"(if {C} then {T} else {R})"
  -- Both branches leave the function, so `rest` is unreachable.
  | .ifElse c t (some e) :: rest => do
    let C ← extractExpr c
    if blockTerminates t && blockTerminates e then do
      let T ← extractStmts t
      let E ← extractStmts e
      some s!"(if {C} then {T} else {E})"
    else if rest.isEmpty then do
      let T ← extractStmts t
      let E ← extractStmts e
      some s!"(if {C} then {T} else {E})"
    else none
  | .letDecl n _ _ v :: rest => do
    let V ← extractExpr v
    let R ← extractStmts rest
    some s!"(let {n} := {V} in {R})"
  -- An assignment to an existing local is a rebinding in a functional encoding.
  -- Sound here only because the fragment has no loops and no aliasing: each
  -- assignment dominates every later read of that name in this block.
  | .assign n v :: rest => do
    let V ← extractExpr v
    let R ← extractStmts rest
    some s!"(let {n} := {V} in {R})"
  | _ => none
end

mutual
/-- Names of every DIRECT callee in an expression. Needed for closure: Gallina has
    no notion of an undefined reference, so a function is only extractable if every
    function it calls is extractable too. -/
partial def calleesExpr : CExpr → List String
  | .call callee _ args _ =>
    (callee.directName?.toList) ++ args.flatMap calleesExpr
  | .unaryOp _ e _ => calleesExpr e
  | .binOp _ l r _ => calleesExpr l ++ calleesExpr r
  | .ifExpr c t e _ => calleesExpr c ++ calleesStmts t ++ calleesStmts e
  | _ => []

/-- Names of every direct callee in a statement list. -/
partial def calleesStmts : List CStmt → List String
  | [] => []
  | s :: rest =>
    let here := match s with
      | .letDecl _ _ _ v => calleesExpr v
      | .assign _ v => calleesExpr v
      | .return_ (some e) _ => calleesExpr e
      | .expr e _ => calleesExpr e
      | .ifElse c t e => calleesExpr c ++ calleesStmts t ++ (e.map calleesStmts).getD []
      | _ => []
    here ++ calleesStmts rest
end

/-- Extract a whole function to a Gallina `Definition`. `none` if any parameter
    type, the return type, or the body falls outside the fragment. -/
def extractFn (f : CFnDef) : Option String := do
  let params ← f.params.mapM (fun (n, t) => (extractTy t).map (fun T => s!"({n} : {T})"))
  let retTy ← extractTy f.retTy
  let body ← extractStmts f.body
  let ps := if params.isEmpty then "" else " " ++ " ".intercalate params
  some s!"Definition {f.name}{ps} : {retTy} :=\n  {body}."

/-- Names of the functions that are extractable AND CLOSED under calling: every
    direct callee is itself extractable and closed.

    Syntactic extractability is not enough. A function whose body is entirely inside
    the fragment can still call one that is not — `extractFn` happily emits a
    `Definition` referring to a name no emitted definition binds, and Rocq then
    fails with "The reference <callee> was not found in the current environment".
    That is our bug surfacing as a broken script, and it would otherwise be counted
    as a checked function. Computed as a fixpoint: start from the syntactically
    extractable set and repeatedly drop anything with a callee outside it. Bounded by
    the number of functions, which is also the longest possible call chain. -/
def closedExtractableNames (fns : List CFnDef) : List String :=
  let syntactic := (fns.filter (fun f => (extractFn f).isSome)).map (·.name)
  let rec prune (candidates : List String) (fuel : Nat) : List String :=
    match fuel with
    | 0 => candidates
    | fuel' + 1 =>
      let next := candidates.filter (fun n =>
        match fns.find? (fun f => f.name == n) with
        | none => false
        | some f => (calleesStmts f.body).all (fun c => candidates.contains c))
      if next.length == candidates.length then next else prune next fuel'
  prune syntactic (fns.length + 1)

/-- Every function the fragment can extract, with its Gallina definition. Order is
    preserved so a callee is defined before a caller when the source is ordered that
    way (Gallina requires definition before use). Only call-closed functions are
    returned — see `closedExtractableNames`. -/
def extractableFns (fns : List CFnDef) : List (CFnDef × String) :=
  let ok := closedExtractableNames fns
  fns.filterMap (fun f =>
    if ok.contains f.name then (extractFn f).map (fun src => (f, src)) else none)

/-- Functions NOT bridge-checked, so the report can name them instead of quietly
    testing a subset and calling it coverage. Includes both the syntactically
    unextractable and those excluded for calling something unextractable. -/
def unextractableFns (fns : List CFnDef) : List String :=
  let ok := closedExtractableNames fns
  (fns.filter (fun f => !ok.contains f.name)).map (·.name)

end Concrete.CoreExtract
