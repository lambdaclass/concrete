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
    the range obligation the literal value must satisfy. EVERY concrete integer
    type the literal ends up with carries its range — the defaulted `Int` too.
    Only a type-variable hint has no range here (the literal's width is not known
    until monomorphization). Both Check and Elab consume the whole record;
    neither re-derives a field.

    Bug 088: the defaulted `Int` used to carry NO range ("matching the historical
    Check behavior"), so `let x = 18446744073709551615;` was accepted as an `Int`
    holding 2^64-1. The interpreter and the proof model kept the mathematical
    value; the binary emitted `i64 18446744073709551615`, which LLVM truncates to
    -1. A literal must fit the type it is given, whichever path gave it. -/
structure IntLitDecision where
  ty : Ty
  range : Option (Int × Int × String)

/-- Canonical typing decision for an integer literal given its (already-resolved)
    hint. An integer or `char` hint is adopted (and carries its range
    obligation); a type-variable hint is adopted (accepts integer literals, no
    range); anything else defaults to `Int` WITH `Int`'s range obligation. This is the
    one `intLit` decision Check and Elab share, so they cannot disagree. -/
def intLitDecision (hint : Option Ty) : IntLitDecision :=
  match hint with
  | some t =>
    if isInteger t || t == .char then
      { ty := t, range := intTyRange t }
    else
      match t with
      | .typeVar _ => { ty := t, range := none }
      | _ => { ty := .int, range := intTyRange .int }
  | none => { ty := .int, range := intTyRange .int }

/-- The hint an integer-literal CAST OPERAND is typed under. A cast operand has
    no type context (`as` exists to convert, so neither the target nor the outer
    hint may leak in), so a bare literal would default to `Int`. That is right
    for every literal `Int` can hold; a literal in `(Int max, Uint max]` — the
    bit-pattern idiom `0xFFFF_FFFF_FFFF_FFFF as i64` — is typed `Uint` instead,
    so the cast reinterprets a well-typed `u64` (giving -1 in the interpreter, the
    folder and the binary alike) rather than smuggling an out-of-range `Int`
    through. Anything wider than `Uint` stays `Int` and is rejected by its range.
    Parentheses are looked through. Check and Elab both call this (bug 088). -/
partial def castOperandHint : Expr → Option Ty
  | .paren _ inner => castOperandHint inner
  | .intLit _ n =>
    match IntArith.intRange .int with
    | some (_, hi) => if n > hi && IntArith.fitsType .uint n then some .uint else none
    | none => none
  | _ => none

/-- Convenience: the adopted type of an integer literal (the `.ty` field). -/
@[inline] def intLitType (hint : Option Ty) : Ty := (intLitDecision hint).ty

/-- Canonical type of a float literal given its type hint: a float hint is
    adopted, otherwise `Float64`. (Float literals carry no range obligation, so
    the decision is just the type.) -/
def floatLitType (hint : Option Ty) : Ty :=
  match hint with
  | some t => if isFloatTy t then t else .float64
  | none => .float64

/-- A literal-only expression is FLEXIBLE: it adopts its type from context (the
    other binop operand, or the type hint), mirroring Core elaboration. Covers
    bare int/float literals, negated or parenthesized ones, and arithmetic over
    literals only (`(8 + 9) % 1000`). The recursion through `binOp`/`paren`/`neg`
    is load-bearing: it lets a nested literal tree like `(8 + 5) + x` (x: i32)
    type top-down from the concrete operand, which is exactly what subsumes the
    old bottom-up re-elaborate repair Elab used to carry for the E0715 case. -/
partial def isFlexibleLit : Expr → Bool
  | .intLit _ _ => true
  | .floatLit _ _ => true
  | .paren _ inner => isFlexibleLit inner
  | .unaryOp _ .neg inner => isFlexibleLit inner
  | .binOp _ _ l r => isFlexibleLit l && isFlexibleLit r
  | _ => false

/-- Bug 280: `isFlexibleLit` of every node of a literal region (the parentheses, negations and
    binary operators `isFlexibleLit` recurses through), computed in one walk: `flex` is
    `isFlexibleLit` of the node, `kids` the trees of the operands it recurses into (one for a
    parenthesis or a negation, two for a binary operator). Check and Elab hand each operand its
    subtree, so a chain of nested operators is walked once instead of once per level. -/
structure FlexTree where
  flex : Bool
  kids : Array FlexTree

instance : Inhabited FlexTree := ⟨{ flex := false, kids := #[] }⟩

partial def flexTree : Expr → FlexTree
  | .intLit _ _ | .floatLit _ _ => { flex := true, kids := #[] }
  | .paren _ inner | .unaryOp _ .neg inner =>
    let t := flexTree inner
    { flex := t.flex, kids := #[t] }
  | .binOp _ _ l r =>
    let tl := flexTree l
    let tr := flexTree r
    { flex := tl.flex && tr.flex, kids := #[tl, tr] }
  | _ => { flex := false, kids := #[] }

/-- The tree of an operand: `i`-th of `kids`, when the node has one. -/
def FlexTree.kid (t : FlexTree) (i : Nat) : Option FlexTree := t.kids[i]?

/-- Whether `isFlexibleLit` recurses into `e` (a parenthesis, a negation, a binary operator). An
    operand that does not is a leaf of the region: its `isFlexibleLit` is answered without a walk,
    so a binary operator over two leaves (`s + t`) builds no tree. With a tree built at every
    operator, the later monomorphize stage of 10^5 assignments ran about 13% slower on the dev host
    and train 37's CI failed its budget; the slowdown went away once these allocations were
    avoided. Its instruction counts were nearly unchanged; the mechanism was not established. -/
def opensFlexRegion : Expr → Bool
  | .paren _ _ | .unaryOp _ .neg _ | .binOp _ _ _ _ => true
  | _ => false

/-- The trees of a binary operator's two operands: the parent's subtrees when it handed one down,
    none when both operands are leaves (their flexibility is `isFlexibleLit`, O(1)), otherwise
    from one walk of the region (`flexTree`). -/
@[inline] def binOpFlexTrees (flex : Option FlexTree) (e lhs rhs : Expr) :
    Option FlexTree × Option FlexTree :=
  match flex with
  | some ft => (ft.kid 0, ft.kid 1)
  | none =>
    if opensFlexRegion lhs || opensFlexRegion rhs then
      let ft := flexTree e
      (ft.kid 0, ft.kid 1)
    else (none, none)

/-- An operand's flexibility: from its tree when there is one, else `isFlexibleLit` (a leaf). -/
@[inline] def operandFlex (t : Option FlexTree) (e : Expr) : Bool :=
  match t with
  | some t => t.flex
  | none => isFlexibleLit e

/-- Which binop operand to type first. -/
inductive OperandOrder where
  /-- Flexible lhs, concrete rhs: type rhs first, then hint lhs with its type. -/
  | rhsFirst
  /-- Default: type lhs first, then hint rhs with its type. -/
  | lhsFirst
  deriving Repr, DecidableEq

/-- The ONE operand-order decision Check and Elab share for a binary operator:
    when one side is a flexible literal tree and the other is concrete, type the
    concrete side first so its type becomes the flexible side's hint
    (`inner[64 + i]`, i: i32 → 64 elaborates as i32). Both front-end passes route
    through this, so Elab's old bottom-up re-elaborate repair is deleted in favor
    of the single top-down rule. -/
def binOpOperandOrder (lhsFlexible rhsFlexible : Bool) : OperandOrder :=
  if lhsFlexible && !rhsFlexible then .rhsFirst else .lhsFirst

/-- Bug 304: the hint a binary operator passes on to the operand it types first, shared by Check
    and Elab. An arithmetic, bitwise or logical operator's result has its operands' type, so its
    hint is theirs. A comparison's hint is its RESULT type (`Bool`) and says nothing about its
    operands: passed on, it made Elab stamp a value `if` / `match` operand `Bool`
    (`(if c { 1 } else { 2 }) >= 2`), and CoreCheck refused the program Check had accepted. -/
def binOpOperandHint (op : BinOp) (hint : Option Ty) : Option Ty :=
  match op with
  | .eq | .neq | .lt | .gt | .leq | .geq => none
  | .add | .sub | .mul | .div | .mod | .and_ | .or_
  | .bitand | .bitor | .bitxor | .shl | .shr
  | .wrappingAdd | .wrappingSub | .wrappingMul
  | .saturatingAdd | .saturatingSub | .saturatingMul => hint

end TypeJudgment
end Concrete
