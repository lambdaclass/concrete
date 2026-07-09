import Concrete.Frontend.AST
import Concrete.Semantics.FactLedger

/-!
# Identified AST — stable node identity, minted post-desugar (Phase 6.5 #9 ≡ #13b)

The certificate substrate needs every typecheckable node to have a stable
`ExprId` so the fact ledger can key facts by identity. Rather than push an id
field onto every `Expr` constructor across the whole compiler (permanent walker
churn) or recompute ids independently in Check and Elab (traversal-order keying
= span-in-disguise = silent desync), identity lives in a **thin id-carrying IR
minted once, post-desugar, that Check and Elab both consume.**

`IdExpr` is a shape-preserving mirror of `Frontend.AST.Expr` with one addition —
an `ExprId` (and the original `span`) on every node — and NO facts (types live
in the ledger, not the syntax). It is ephemeral: it exists only between the mint
pass and Elab.

Design: `research/compiler/fact-ledger-design.md`. This module is sub-step B of
the fresh run: the IR + the mint. Sub-step C rewrites Check to consume it and
commit facts; until then this is additive.
-/

namespace Concrete

mutual

/-- A typed-pipeline expression node: stable identity + source span + shape.
    No `ty` field — the type is a ledger fact keyed by `id`, committed by Check. -/
structure IdExpr where
  id   : ExprId
  span : Span
  kind : IdExprKind

/-- Expression shapes, mirroring `Expr`; sub-expressions are `IdExpr`. -/
inductive IdExprKind where
  | intLit (val : Int)
  | floatLit (val : Float)
  | boolLit (val : Bool)
  | strLit (val : String)
  | charLit (val : Char)
  | ident (name : String)
  | fnRef (name : String)
  | binOp (op : BinOp) (lhs rhs : IdExpr)
  | unaryOp (op : UnaryOp) (operand : IdExpr)
  | call (fn : String) (typeArgs : List Ty) (args : List IdExpr)
  | paren (inner : IdExpr)
  | structLit (name : String) (typeArgs : List Ty) (fields : List (String × IdExpr)) (base : Option IdExpr)
  | fieldAccess (obj : IdExpr) (field : String)
  | enumLit (enumName variant : String) (typeArgs : List Ty) (fields : List (String × IdExpr))
  | match_ (scrutinee : IdExpr) (arms : List IdMatchArm)
  | borrow (inner : IdExpr)
  | borrowMut (inner : IdExpr)
  | deref (inner : IdExpr)
  | try_ (inner : IdExpr)
  | arrayLit (elems : List IdExpr)
  | arrayIndex (arr : IdExpr) (index : IdExpr)
  | cast (inner : IdExpr) (targetTy : Ty)
  | methodCall (obj : IdExpr) (method : String) (typeArgs : List Ty) (args : List IdExpr)
  | staticMethodCall (typeName method : String) (typeArgs : List Ty) (args : List IdExpr)
  | arrowAccess (obj : IdExpr) (field : String)
  | allocCall (inner : IdExpr) (allocExpr : IdExpr)
  | whileExpr (cond : IdExpr) (body : List IdStmt) (elseBody : List IdStmt)
  | ifExpr (cond : IdExpr) (then_ : List IdStmt) (else_ : List IdStmt)

/-- Match arm, mirroring `MatchArm`. -/
inductive IdMatchArm where
  | mk (span : Span) (enumName variant : String) (bindings : List String) (guard : Option IdExpr) (body : List IdStmt)
  | litArm (span : Span) (value : IdExpr) (guard : Option IdExpr) (body : List IdStmt)
  | varArm (span : Span) (binding : String) (guard : Option IdExpr) (body : List IdStmt)
  | rangeArm (span : Span) (lo hi : IdExpr) (inclusive : Bool) (guard : Option IdExpr) (body : List IdStmt)

/-- A statement node: identity + span + shape (mirrors `Stmt`). -/
structure IdStmt where
  id   : ExprId
  span : Span
  kind : IdStmtKind

inductive IdStmtKind where
  | letDecl (name : String) (mutable : Bool) (ty : Option Ty) (value : IdExpr) (isGhost : Bool)
  | assign (name : String) (value : IdExpr)
  | return_ (value : Option IdExpr)
  | expr (e : IdExpr) (isValue : Bool)
  | ifElse (cond : IdExpr) (then_ : List IdStmt) (else_ : Option (List IdStmt))
  | while_ (cond : IdExpr) (body : List IdStmt) (label : Option String)
  | forLoop (init : Option IdStmt) (cond : IdExpr) (step : Option IdStmt) (body : List IdStmt) (label : Option String)
  | fieldAssign (obj : IdExpr) (field : String) (value : IdExpr)
  | derefAssign (target : IdExpr) (value : IdExpr)
  | arrayIndexAssign (arr : IdExpr) (index : IdExpr) (value : IdExpr)
  | break_ (value : Option IdExpr) (label : Option String)
  | continue_ (label : Option String)
  | defer (body : IdExpr)
  | assert_ (cond : IdExpr)
  | assume_ (cond : IdExpr)
  | borrowIn (var ref region : String) (isMut : Bool) (body : List IdStmt)
  | arrowAssign (obj : IdExpr) (field : String) (value : IdExpr)
  | letDestructure (enumName variant : String) (bindings : List String) (value : IdExpr) (elseBody : Option (List IdStmt))
  | letStructDestructure (structName : String) (bindings : List String) (value : IdExpr)

end

-- Inhabited witnesses (via non-recursive constructors) so the recursive `mint`
-- functions may be `partial`.
instance : Inhabited IdExprKind := ⟨.intLit 0⟩
instance : Inhabited IdExpr := ⟨{ id := default, span := default, kind := .intLit 0 }⟩
instance : Inhabited IdStmtKind := ⟨.continue_ none⟩
instance : Inhabited IdStmt := ⟨{ id := default, span := default, kind := .continue_ none }⟩
instance : Inhabited IdMatchArm := ⟨.varArm default "" none []⟩

/-- A function with an id-carrying body (signature stays as AST facts). -/
structure IdFnDecl where
  fn   : FnDef          -- the original decl (signature, contracts, flags)
  body : List IdStmt    -- id-minted body

/-- A module whose function bodies have been minted to `IdStmt`. Non-function
    declarations keep their AST types (their expression-level nodes are minted
    when those families are migrated). -/
structure IdModule where
  mod       : Module            -- original module (all non-body metadata)
  functions : List IdFnDecl
  submodules : List IdModule := []

/-- A program whose typecheckable nodes carry stable identity, ready for Check to
    commit facts against. Minted once, post-desugar. -/
structure IdentifiedProgram where
  modules : List IdModule

/-- `Nonempty` witness so the recursive (over submodules) mint may be `partial`. -/
instance : Nonempty IdModule :=
  ⟨{ mod := { name := "", structs := [], enums := [], functions := [] },
     functions := [], submodules := [] }⟩

-- ============================================================
-- Mint pass: assign ExprIds deterministically (post-desugar)
-- ============================================================

namespace Mint

/-- Fresh-id counter, per module. -/
abbrev MintM := StateM Nat

def fresh (mid : String) : MintM ExprId := do
  let n ← get
  set (n + 1)
  return { moduleId := mid, localId := n }

mutual

/-- Mint an expression: assign this node's id (pre-order), then its children. The
    order is fixed, so ids are deterministic run-to-run; and because Check and
    Elab consume the SAME minted tree, they agree by construction. -/
partial def mintExpr (mid : String) : Expr → MintM IdExpr
  | .intLit sp v => do return { id := ← fresh mid, span := sp, kind := .intLit v }
  | .floatLit sp v => do return { id := ← fresh mid, span := sp, kind := .floatLit v }
  | .boolLit sp v => do return { id := ← fresh mid, span := sp, kind := .boolLit v }
  | .strLit sp v => do return { id := ← fresh mid, span := sp, kind := .strLit v }
  | .charLit sp v => do return { id := ← fresh mid, span := sp, kind := .charLit v }
  | .ident sp n => do return { id := ← fresh mid, span := sp, kind := .ident n }
  | .fnRef sp n => do return { id := ← fresh mid, span := sp, kind := .fnRef n }
  | .binOp sp op l r => do
    let id ← fresh mid; return { id, span := sp, kind := .binOp op (← mintExpr mid l) (← mintExpr mid r) }
  | .unaryOp sp op e => do
    let id ← fresh mid; return { id, span := sp, kind := .unaryOp op (← mintExpr mid e) }
  | .call sp fn tas args => do
    let id ← fresh mid; return { id, span := sp, kind := .call fn tas (← args.mapM (mintExpr mid)) }
  | .paren sp e => do
    let id ← fresh mid; return { id, span := sp, kind := .paren (← mintExpr mid e) }
  | .structLit sp n tas fields base => do
    let id ← fresh mid
    let fields' ← fields.mapM (fun (f, e) => do return (f, ← mintExpr mid e))
    let base' ← base.mapM (mintExpr mid)
    return { id, span := sp, kind := .structLit n tas fields' base' }
  | .fieldAccess sp o f => do
    let id ← fresh mid; return { id, span := sp, kind := .fieldAccess (← mintExpr mid o) f }
  | .enumLit sp en v tas fields => do
    let id ← fresh mid
    let fields' ← fields.mapM (fun (f, e) => do return (f, ← mintExpr mid e))
    return { id, span := sp, kind := .enumLit en v tas fields' }
  | .match_ sp s arms => do
    let id ← fresh mid
    return { id, span := sp, kind := .match_ (← mintExpr mid s) (← arms.mapM (mintArm mid)) }
  | .borrow sp e => do let id ← fresh mid; return { id, span := sp, kind := .borrow (← mintExpr mid e) }
  | .borrowMut sp e => do let id ← fresh mid; return { id, span := sp, kind := .borrowMut (← mintExpr mid e) }
  | .deref sp e => do let id ← fresh mid; return { id, span := sp, kind := .deref (← mintExpr mid e) }
  | .try_ sp e => do let id ← fresh mid; return { id, span := sp, kind := .try_ (← mintExpr mid e) }
  | .arrayLit sp es => do
    let id ← fresh mid; return { id, span := sp, kind := .arrayLit (← es.mapM (mintExpr mid)) }
  | .arrayIndex sp a i => do
    let id ← fresh mid; return { id, span := sp, kind := .arrayIndex (← mintExpr mid a) (← mintExpr mid i) }
  | .cast sp e t => do let id ← fresh mid; return { id, span := sp, kind := .cast (← mintExpr mid e) t }
  | .methodCall sp o m tas args => do
    let id ← fresh mid; return { id, span := sp, kind := .methodCall (← mintExpr mid o) m tas (← args.mapM (mintExpr mid)) }
  | .staticMethodCall sp tn m tas args => do
    let id ← fresh mid; return { id, span := sp, kind := .staticMethodCall tn m tas (← args.mapM (mintExpr mid)) }
  | .arrowAccess sp o f => do
    let id ← fresh mid; return { id, span := sp, kind := .arrowAccess (← mintExpr mid o) f }
  | .allocCall sp e a => do
    let id ← fresh mid; return { id, span := sp, kind := .allocCall (← mintExpr mid e) (← mintExpr mid a) }
  | .whileExpr sp c b e => do
    let id ← fresh mid; return { id, span := sp, kind := .whileExpr (← mintExpr mid c) (← b.mapM (mintStmt mid)) (← e.mapM (mintStmt mid)) }
  | .ifExpr sp c t e => do
    let id ← fresh mid; return { id, span := sp, kind := .ifExpr (← mintExpr mid c) (← t.mapM (mintStmt mid)) (← e.mapM (mintStmt mid)) }

partial def mintArm (mid : String) : MatchArm → MintM IdMatchArm
  | .mk sp en v bs g body => do
    return .mk sp en v bs (← g.mapM (mintExpr mid)) (← body.mapM (mintStmt mid))
  | .litArm sp val g body => do
    return .litArm sp (← mintExpr mid val) (← g.mapM (mintExpr mid)) (← body.mapM (mintStmt mid))
  | .varArm sp b g body => do
    return .varArm sp b (← g.mapM (mintExpr mid)) (← body.mapM (mintStmt mid))
  | .rangeArm sp lo hi inc g body => do
    return .rangeArm sp (← mintExpr mid lo) (← mintExpr mid hi) inc (← g.mapM (mintExpr mid)) (← body.mapM (mintStmt mid))

partial def mintStmt (mid : String) : Stmt → MintM IdStmt
  | .letDecl sp n mut_ ty v g => do
    let id ← fresh mid; return { id, span := sp, kind := .letDecl n mut_ ty (← mintExpr mid v) g }
  | .assign sp n v => do
    let id ← fresh mid; return { id, span := sp, kind := .assign n (← mintExpr mid v) }
  | .return_ sp v => do
    let id ← fresh mid; return { id, span := sp, kind := .return_ (← v.mapM (mintExpr mid)) }
  | .expr sp e isV => do
    let id ← fresh mid; return { id, span := sp, kind := .expr (← mintExpr mid e) isV }
  | .ifElse sp c t e => do
    let id ← fresh mid
    let e' ← e.mapM (fun ss => ss.mapM (mintStmt mid))
    return { id, span := sp, kind := .ifElse (← mintExpr mid c) (← t.mapM (mintStmt mid)) e' }
  | .while_ sp c b l => do
    let id ← fresh mid; return { id, span := sp, kind := .while_ (← mintExpr mid c) (← b.mapM (mintStmt mid)) l }
  | .forLoop sp init c step b l => do
    let id ← fresh mid
    let init' ← init.mapM (mintStmt mid)
    let step' ← step.mapM (mintStmt mid)
    return { id, span := sp, kind := .forLoop init' (← mintExpr mid c) step' (← b.mapM (mintStmt mid)) l }
  | .fieldAssign sp o f v => do
    let id ← fresh mid; return { id, span := sp, kind := .fieldAssign (← mintExpr mid o) f (← mintExpr mid v) }
  | .derefAssign sp t v => do
    let id ← fresh mid; return { id, span := sp, kind := .derefAssign (← mintExpr mid t) (← mintExpr mid v) }
  | .arrayIndexAssign sp a i v => do
    let id ← fresh mid; return { id, span := sp, kind := .arrayIndexAssign (← mintExpr mid a) (← mintExpr mid i) (← mintExpr mid v) }
  | .break_ sp v l => do
    let id ← fresh mid; return { id, span := sp, kind := .break_ (← v.mapM (mintExpr mid)) l }
  | .continue_ sp l => do
    let id ← fresh mid; return { id, span := sp, kind := .continue_ l }
  | .defer sp e => do
    let id ← fresh mid; return { id, span := sp, kind := .defer (← mintExpr mid e) }
  | .assert_ sp c => do
    let id ← fresh mid; return { id, span := sp, kind := .assert_ (← mintExpr mid c) }
  | .assume_ sp c => do
    let id ← fresh mid; return { id, span := sp, kind := .assume_ (← mintExpr mid c) }
  | .borrowIn sp var ref region isMut body => do
    let id ← fresh mid; return { id, span := sp, kind := .borrowIn var ref region isMut (← body.mapM (mintStmt mid)) }
  | .arrowAssign sp o f v => do
    let id ← fresh mid; return { id, span := sp, kind := .arrowAssign (← mintExpr mid o) f (← mintExpr mid v) }
  | .letDestructure sp en v bs val elseB => do
    let id ← fresh mid
    let elseB' ← elseB.mapM (fun ss => ss.mapM (mintStmt mid))
    return { id, span := sp, kind := .letDestructure en v bs (← mintExpr mid val) elseB' }
  | .letStructDestructure sp sn bs val => do
    let id ← fresh mid; return { id, span := sp, kind := .letStructDestructure sn bs (← mintExpr mid val) }

end

partial def mintModule (m : Module) : IdModule :=
  let mid := m.name
  let (fns, _) := m.functions.foldl (fun (acc : List IdFnDecl × Nat) f =>
    let (body, st') := (f.body.mapM (mintStmt mid)).run acc.2
    (acc.1 ++ [{ fn := f, body }], st')) (([], 0) : List IdFnDecl × Nat)
  { mod := m, functions := fns, submodules := m.submodules.map mintModule }

end Mint

/-- Mint a whole program to identified form. Run post-desugar, before Check. -/
def IdentifiedProgram.mint (modules : List Module) : IdentifiedProgram :=
  { modules := modules.map Mint.mintModule }

end Concrete
