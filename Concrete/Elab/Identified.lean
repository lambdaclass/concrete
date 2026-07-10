import Concrete.Frontend.AST
import Concrete.Semantics.CompilerDB

/-!
# Identified AST — stable node identity, minted post-desugar (Phase 6.5 #9 ≡ #13b)

The certificate substrate needs every typecheckable node to have a stable
`ExprId` so the fact ledger can key facts by identity. Identity lives in a thin
id-carrying IR minted once, post-desugar, that Check and Elab both consume — not
pushed onto every `Expr` constructor (permanent churn) and not recomputed
independently per pass (traversal-order = span-in-disguise = silent desync).

`IdExpr` is `{ id, kind }` where `IdExprKind` mirrors `Frontend.AST.Expr`
**exactly** (spans kept in the constructors, sub-expressions are `IdExpr`) — so
converting a pass from `Expr` to `IdExpr` changes only the scrutinee/param types,
not the match arms. No `ty`/fact fields: types live in the ledger, keyed by `id`.

Design: `research/compiler/fact-ledger-design.md`. Sub-step B of the fresh run.
-/

namespace Concrete

mutual

/-- A typecheckable expression node: stable identity + shape. The shape mirrors
    `Expr` (spans in the kind ctors); the only addition is `id`. -/
structure IdExpr where
  id   : ExprId
  kind : IdExprKind

/-- Mirrors `Expr` exactly, but sub-expressions are `IdExpr`. -/
inductive IdExprKind where
  | intLit (span : Span) (val : Int)
  | floatLit (span : Span) (val : Float)
  | boolLit (span : Span) (val : Bool)
  | strLit (span : Span) (val : String)
  | charLit (span : Span) (val : Char)
  | ident (span : Span) (name : String)
  | binOp (span : Span) (op : BinOp) (lhs rhs : IdExpr)
  | unaryOp (span : Span) (op : UnaryOp) (operand : IdExpr)
  | call (span : Span) (fn : String) (typeArgs : List Ty) (args : List IdExpr)
  | paren (span : Span) (inner : IdExpr)
  | structLit (span : Span) (name : String) (typeArgs : List Ty) (fields : List (String × IdExpr)) (base : Option IdExpr)
  | fieldAccess (span : Span) (obj : IdExpr) (field : String)
  | enumLit (span : Span) (enumName variant : String) (typeArgs : List Ty) (fields : List (String × IdExpr))
  | match_ (span : Span) (scrutinee : IdExpr) (arms : List IdMatchArm)
  | borrow (span : Span) (inner : IdExpr)
  | borrowMut (span : Span) (inner : IdExpr)
  | deref (span : Span) (inner : IdExpr)
  | try_ (span : Span) (inner : IdExpr)
  | arrayLit (span : Span) (elems : List IdExpr)
  | arrayIndex (span : Span) (arr : IdExpr) (index : IdExpr)
  | cast (span : Span) (inner : IdExpr) (targetTy : Ty)
  | methodCall (span : Span) (obj : IdExpr) (method : String) (typeArgs : List Ty) (args : List IdExpr)
  | staticMethodCall (span : Span) (typeName method : String) (typeArgs : List Ty) (args : List IdExpr)
  | fnRef (span : Span) (name : String)
  | arrowAccess (span : Span) (obj : IdExpr) (field : String)
  | allocCall (span : Span) (inner : IdExpr) (allocExpr : IdExpr)
  | whileExpr (span : Span) (cond : IdExpr) (body : List IdStmt) (elseBody : List IdStmt)
  | ifExpr (span : Span) (cond : IdExpr) (then_ : List IdStmt) (else_ : List IdStmt)

/-- Match arm, mirroring `MatchArm`. -/
inductive IdMatchArm where
  | mk (span : Span) (enumName variant : String) (bindings : List String) (guard : Option IdExpr) (body : List IdStmt)
  | litArm (span : Span) (value : IdExpr) (guard : Option IdExpr) (body : List IdStmt)
  | varArm (span : Span) (binding : String) (guard : Option IdExpr) (body : List IdStmt)
  | rangeArm (span : Span) (lo hi : IdExpr) (inclusive : Bool) (guard : Option IdExpr) (body : List IdStmt)

/-- A statement node: stable identity + shape (mirrors `Stmt`). -/
structure IdStmt where
  id   : ExprId
  kind : IdStmtKind

inductive IdStmtKind where
  | letDecl (span : Span) (name : String) (mutable : Bool) (ty : Option Ty) (value : IdExpr) (isGhost : Bool)
  | assign (span : Span) (name : String) (value : IdExpr)
  | return_ (span : Span) (value : Option IdExpr)
  | expr (span : Span) (e : IdExpr) (isValue : Bool)
  | ifElse (span : Span) (cond : IdExpr) (then_ : List IdStmt) (else_ : Option (List IdStmt))
  | while_ (span : Span) (cond : IdExpr) (body : List IdStmt) (label : Option String)
  | forLoop (span : Span) (init : Option IdStmt) (cond : IdExpr) (step : Option IdStmt) (body : List IdStmt) (label : Option String)
  | fieldAssign (span : Span) (obj : IdExpr) (field : String) (value : IdExpr)
  | derefAssign (span : Span) (target : IdExpr) (value : IdExpr)
  | arrayIndexAssign (span : Span) (arr : IdExpr) (index : IdExpr) (value : IdExpr)
  | break_ (span : Span) (value : Option IdExpr) (label : Option String)
  | continue_ (span : Span) (label : Option String)
  | defer (span : Span) (body : IdExpr)
  | assert_ (span : Span) (cond : IdExpr)
  | assume_ (span : Span) (cond : IdExpr)
  | borrowIn (span : Span) (var ref region : String) (isMut : Bool) (body : List IdStmt)
  | arrowAssign (span : Span) (obj : IdExpr) (field : String) (value : IdExpr)
  | letDestructure (span : Span) (enumName variant : String) (bindings : List String) (value : IdExpr) (elseBody : Option (List IdStmt))
  | letStructDestructure (span : Span) (structName : String) (bindings : List String) (value : IdExpr)

end

-- Inhabited witnesses (non-recursive ctors) so the recursive mint may be partial.
instance : Inhabited IdExprKind := ⟨.intLit default 0⟩
instance : Inhabited IdExpr := ⟨{ id := default, kind := .intLit default 0 }⟩
instance : Inhabited IdStmtKind := ⟨.continue_ default none⟩
instance : Inhabited IdStmt := ⟨{ id := default, kind := .continue_ default none }⟩
instance : Inhabited IdMatchArm := ⟨.varArm default "" none []⟩

/-- Source span of an identified expression (mirrors `Expr.getSpan`). -/
def IdExpr.getSpan (e : IdExpr) : Span :=
  match e.kind with
  | .intLit sp _ | .floatLit sp _ | .boolLit sp _ | .strLit sp _ | .charLit sp _ => sp
  | .ident sp _ | .fnRef sp _ => sp
  | .binOp sp _ _ _ | .unaryOp sp _ _ | .paren sp _ => sp
  | .call sp _ _ _ | .structLit sp _ _ _ _ | .enumLit sp _ _ _ _ => sp
  | .fieldAccess sp _ _ | .arrowAccess sp _ _ => sp
  | .match_ sp _ _ | .borrow sp _ | .borrowMut sp _ | .deref sp _ | .try_ sp _ => sp
  | .arrayLit sp _ | .arrayIndex sp _ _ | .cast sp _ _ => sp
  | .methodCall sp _ _ _ _ | .staticMethodCall sp _ _ _ _ => sp
  | .allocCall sp _ _ | .whileExpr sp _ _ _ | .ifExpr sp _ _ _ => sp

/-- A function with an id-carrying body (signature stays as AST facts). -/
structure IdFnDecl where
  fn   : FnDef
  body : List IdStmt

/-- A module whose function bodies have been minted to `IdStmt`. -/
structure IdModule where
  mod        : Module
  functions  : List IdFnDecl
  submodules : List IdModule := []

/-- A program whose typecheckable nodes carry stable identity. Minted once,
    post-desugar, before Check. -/
structure IdentifiedProgram where
  modules : List IdModule

instance : Nonempty IdModule :=
  ⟨{ mod := { name := "", structs := [], enums := [], functions := [] },
     functions := [], submodules := [] }⟩

-- ============================================================
-- Mint pass: assign ExprIds deterministically (post-desugar)
-- ============================================================

namespace Mint

abbrev MintM := StateM Nat

def fresh : MintM Nat := do
  let n ← get; set (n + 1); return n

/-- Wrap: assign this node's id (pre-order), then build the kind (children get
    ids as they are minted). `mid` is the module id. -/
def mk (mid : String) (kind : IdExprKind) : MintM IdExpr := do
  return { id := { moduleId := mid, localId := ← fresh }, kind }

def mkS (mid : String) (kind : IdStmtKind) : MintM IdStmt := do
  return { id := { moduleId := mid, localId := ← fresh }, kind }

mutual

partial def mintExpr (mid : String) : Expr → MintM IdExpr
  | .intLit sp v => mk mid (.intLit sp v)
  | .floatLit sp v => mk mid (.floatLit sp v)
  | .boolLit sp v => mk mid (.boolLit sp v)
  | .strLit sp v => mk mid (.strLit sp v)
  | .charLit sp v => mk mid (.charLit sp v)
  | .ident sp n => mk mid (.ident sp n)
  | .fnRef sp n => mk mid (.fnRef sp n)
  | .binOp sp op l r => do mk mid (.binOp sp op (← mintExpr mid l) (← mintExpr mid r))
  | .unaryOp sp op e => do mk mid (.unaryOp sp op (← mintExpr mid e))
  | .call sp fn tas args => do mk mid (.call sp fn tas (← args.mapM (mintExpr mid)))
  | .paren sp e => do mk mid (.paren sp (← mintExpr mid e))
  | .structLit sp n tas fields base => do
    let fields' ← fields.mapM (fun (f, e) => do return (f, ← mintExpr mid e))
    mk mid (.structLit sp n tas fields' (← base.mapM (mintExpr mid)))
  | .fieldAccess sp o f => do mk mid (.fieldAccess sp (← mintExpr mid o) f)
  | .enumLit sp en v tas fields => do
    let fields' ← fields.mapM (fun (f, e) => do return (f, ← mintExpr mid e))
    mk mid (.enumLit sp en v tas fields')
  | .match_ sp s arms => do mk mid (.match_ sp (← mintExpr mid s) (← arms.mapM (mintArm mid)))
  | .borrow sp e => do mk mid (.borrow sp (← mintExpr mid e))
  | .borrowMut sp e => do mk mid (.borrowMut sp (← mintExpr mid e))
  | .deref sp e => do mk mid (.deref sp (← mintExpr mid e))
  | .try_ sp e => do mk mid (.try_ sp (← mintExpr mid e))
  | .arrayLit sp es => do mk mid (.arrayLit sp (← es.mapM (mintExpr mid)))
  | .arrayIndex sp a i => do mk mid (.arrayIndex sp (← mintExpr mid a) (← mintExpr mid i))
  | .cast sp e t => do mk mid (.cast sp (← mintExpr mid e) t)
  | .methodCall sp o m tas args => do mk mid (.methodCall sp (← mintExpr mid o) m tas (← args.mapM (mintExpr mid)))
  | .staticMethodCall sp tn m tas args => do mk mid (.staticMethodCall sp tn m tas (← args.mapM (mintExpr mid)))
  | .arrowAccess sp o f => do mk mid (.arrowAccess sp (← mintExpr mid o) f)
  | .allocCall sp e a => do mk mid (.allocCall sp (← mintExpr mid e) (← mintExpr mid a))
  | .whileExpr sp c b e => do mk mid (.whileExpr sp (← mintExpr mid c) (← b.mapM (mintStmt mid)) (← e.mapM (mintStmt mid)))
  | .ifExpr sp c t e => do mk mid (.ifExpr sp (← mintExpr mid c) (← t.mapM (mintStmt mid)) (← e.mapM (mintStmt mid)))

partial def mintArm (mid : String) : MatchArm → MintM IdMatchArm
  | .mk sp en v bs g body => do return .mk sp en v bs (← g.mapM (mintExpr mid)) (← body.mapM (mintStmt mid))
  | .litArm sp val g body => do return .litArm sp (← mintExpr mid val) (← g.mapM (mintExpr mid)) (← body.mapM (mintStmt mid))
  | .varArm sp b g body => do return .varArm sp b (← g.mapM (mintExpr mid)) (← body.mapM (mintStmt mid))
  | .rangeArm sp lo hi inc g body => do return .rangeArm sp (← mintExpr mid lo) (← mintExpr mid hi) inc (← g.mapM (mintExpr mid)) (← body.mapM (mintStmt mid))

partial def mintStmt (mid : String) : Stmt → MintM IdStmt
  | .letDecl sp n mut_ ty v g => do mkS mid (.letDecl sp n mut_ ty (← mintExpr mid v) g)
  | .assign sp n v => do mkS mid (.assign sp n (← mintExpr mid v))
  | .return_ sp v => do mkS mid (.return_ sp (← v.mapM (mintExpr mid)))
  | .expr sp e isV => do mkS mid (.expr sp (← mintExpr mid e) isV)
  | .ifElse sp c t e => do
    let e' ← e.mapM (fun ss => ss.mapM (mintStmt mid))
    mkS mid (.ifElse sp (← mintExpr mid c) (← t.mapM (mintStmt mid)) e')
  | .while_ sp c b l => do mkS mid (.while_ sp (← mintExpr mid c) (← b.mapM (mintStmt mid)) l)
  | .forLoop sp init c step b l => do
    let init' ← init.mapM (mintStmt mid)
    let step' ← step.mapM (mintStmt mid)
    mkS mid (.forLoop sp init' (← mintExpr mid c) step' (← b.mapM (mintStmt mid)) l)
  | .fieldAssign sp o f v => do mkS mid (.fieldAssign sp (← mintExpr mid o) f (← mintExpr mid v))
  | .derefAssign sp t v => do mkS mid (.derefAssign sp (← mintExpr mid t) (← mintExpr mid v))
  | .arrayIndexAssign sp a i v => do mkS mid (.arrayIndexAssign sp (← mintExpr mid a) (← mintExpr mid i) (← mintExpr mid v))
  | .break_ sp v l => do mkS mid (.break_ sp (← v.mapM (mintExpr mid)) l)
  | .continue_ sp l => do mkS mid (.continue_ sp l)
  | .defer sp e => do mkS mid (.defer sp (← mintExpr mid e))
  | .assert_ sp c => do mkS mid (.assert_ sp (← mintExpr mid c))
  | .assume_ sp c => do mkS mid (.assume_ sp (← mintExpr mid c))
  | .borrowIn sp var ref region isMut body => do mkS mid (.borrowIn sp var ref region isMut (← body.mapM (mintStmt mid)))
  | .arrowAssign sp o f v => do mkS mid (.arrowAssign sp (← mintExpr mid o) f (← mintExpr mid v))
  | .letDestructure sp en v bs val elseB => do
    let elseB' ← elseB.mapM (fun ss => ss.mapM (mintStmt mid))
    mkS mid (.letDestructure sp en v bs (← mintExpr mid val) elseB')
  | .letStructDestructure sp sn bs val => do mkS mid (.letStructDestructure sp sn bs (← mintExpr mid val))

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
