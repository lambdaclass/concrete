import Concrete.Frontend.AST

/-!
# Typed AST — the Check → Elab boundary (Phase 6.5 #9 ≡ Phase 14 #13b)

This is the typed syntax the checker PRODUCES and the elaborator CONSUMES. It is
the one source of typing truth: after Check there is no untyped `Expr` for a
later pass to re-infer from, so "Check thought X, Elab thought Y" becomes
*unrepresentable* rather than merely gated.

Every `TExpr` carries:
- `id`   — a stable `ExprId` (module + deterministic local index), real
  provenance that later fact tables (capability, proof, report, incremental
  cache) key on — NOT the typing mechanism itself;
- `span` — source location, for diagnostics (a span says "where from"; the id
  says "which node");
- `ty`   — the type the checker committed. There is no "untyped `TExpr`";
- `mode` — the checked value-flow decision (copy / move / borrow / …), so linear
  ownership is a carried fact, not a re-derivation.

Invalid states are meant to be unconstructable: no expression without a type
after Check, no call without checked argument types, no linear value-flow
without a mode. This module defines the *shape*; Check populating it and Elab
consuming it are the two large stages that follow (the IR lands first so both
can be written against a fixed target).

This is a leaf over the AST (`Ty`, `BinOp`, `UnaryOp`, `Span`, `CapSet`,
`ImportDecl`); the constructor set mirrors the post-Resolve `Expr`/`Stmt` so the
migration is a shape-preserving lift, and surface-only forms that desugaring
removes before Check simply never get produced.
-/

namespace Concrete

/-- Stable identity of a typecheckable node: module + a per-module local index
    assigned in a deterministic traversal after `desugar` (so Check and any
    consumer agree). Distinct from `Span` — generated/desugared nodes can share a
    span but never an id. The durable key for type/capability/proof/report facts. -/
structure ExprId where
  moduleId : String
  localId  : Nat
deriving BEq, Repr, Hashable, Inhabited

/-- The checked value-flow decision attached to a `TExpr` (Concrete is linear).
    Carrying it means downstream ownership/pass-agreement reasoning reads a
    committed fact instead of re-deriving whether a use copies, moves, borrows,
    reborrows, consumes, or names a place. -/
inductive ValueMode where
  | copy      -- a `Copy` value duplicated
  | move      -- an owned value moved out
  | borrow    -- `&` shared borrow
  | reborrow  -- reborrow of an existing reference
  | consume   -- linear consume (e.g. `.drop()`, passed to a consuming callee)
  | place     -- an lvalue/place, not a value read
deriving BEq, Repr, Inhabited

mutual

/-- A typed expression: node envelope (`id`/`span`/`ty`/`mode`) + kind. -/
structure TExpr where
  id   : ExprId
  span : Span
  ty   : Ty
  mode : ValueMode
  kind : TExprKind

/-- The expression forms. Sub-expressions are `TExpr` (already typed), so a
    partially-typed tree cannot exist. Mirrors the post-Resolve `Expr` set. -/
inductive TExprKind where
  | intLit (val : Int)
  | floatLit (val : Float)
  | boolLit (val : Bool)
  | strLit (val : String)
  | charLit (val : Char)
  | ident (name : String)
  | fnRef (name : String)
  | binOp (op : BinOp) (lhs rhs : TExpr)
  | unaryOp (op : UnaryOp) (operand : TExpr)
  | call (fn : String) (typeArgs : List Ty) (args : List TExpr)
  | structLit (name : String) (typeArgs : List Ty) (fields : List (String × TExpr)) (base : Option TExpr)
  | fieldAccess (obj : TExpr) (field : String)
  | enumLit (enumName variant : String) (typeArgs : List Ty) (fields : List (String × TExpr))
  | match_ (scrutinee : TExpr) (arms : List TMatchArm)
  | borrow (inner : TExpr)
  | borrowMut (inner : TExpr)
  | deref (inner : TExpr)
  | try_ (inner : TExpr)
  | arrayLit (elems : List TExpr)
  | arrayIndex (arr : TExpr) (index : TExpr)
  | cast (inner : TExpr) (targetTy : Ty)
  | allocCall (inner : TExpr) (allocExpr : TExpr)
  | whileExpr (cond : TExpr) (body : List TStmt) (elseBody : List TStmt)
  | ifExpr (cond : TExpr) (then_ : List TStmt) (else_ : List TStmt)

/-- Typed match arm (mirrors `MatchArm`). -/
inductive TMatchArm where
  | mk (span : Span) (enumName variant : String) (bindings : List String) (guard : Option TExpr) (body : List TStmt)
  | litArm (span : Span) (value : TExpr) (guard : Option TExpr) (body : List TStmt)
  | varArm (span : Span) (binding : String) (guard : Option TExpr) (body : List TStmt)
  | rangeArm (span : Span) (lo hi : TExpr) (inclusive : Bool) (guard : Option TExpr) (body : List TStmt)

/-- A typed statement: `id`/`span` envelope + kind. -/
structure TStmt where
  id   : ExprId
  span : Span
  kind : TStmtKind

/-- Statement forms (mirror the post-Resolve `Stmt` set; the surface-only
    destructure/arrow forms are desugared before Check and never produced). -/
inductive TStmtKind where
  | letDecl (name : String) (mutable : Bool) (ty : Ty) (value : TExpr) (isGhost : Bool)
  | assign (name : String) (value : TExpr)
  | return_ (value : Option TExpr)
  | expr (e : TExpr) (isValue : Bool)
  | ifElse (cond : TExpr) (then_ : List TStmt) (else_ : Option (List TStmt))
  | while_ (cond : TExpr) (body : List TStmt) (label : Option String)
  | forLoop (init : Option TStmt) (cond : TExpr) (step : Option TStmt) (body : List TStmt) (label : Option String)
  | fieldAssign (obj : TExpr) (field : String) (value : TExpr)
  | derefAssign (target : TExpr) (value : TExpr)
  | arrayIndexAssign (arr : TExpr) (index : TExpr) (value : TExpr)
  | break_ (value : Option TExpr) (label : Option String)
  | continue_ (label : Option String)
  | defer (body : TExpr)
  | assert_ (cond : TExpr)
  | assume_ (cond : TExpr)
  | borrowIn (var ref region : String) (isMut : Bool) (body : List TStmt)

end

/-- A typed function parameter (name + committed type). -/
structure TParam where
  name : String
  ty   : Ty

/-- A typed function declaration: signature facts (types, caps) are committed
    and the body is a list of typed statements. -/
structure TFnDecl where
  name     : String
  params   : List TParam
  retTy    : Ty
  caps     : CapSet
  isTrusted : Bool := false
  isPublic  : Bool := false
  body     : List TStmt
  declSpan : Option Span := none

/-- A type-checked module. Mirrors `Frontend.AST.Module`, but the members whose
    bodies the checker types — top-level `functions` — become `TFnDecl`. The
    remaining declarations (structs/enums/traits/impls/consts/aliases/externs/
    newtypes) keep their AST types for now: their expression-level typing (impl
    method bodies, const initializers) is folded in by later increments of this
    arc, at which point those fields graduate to typed variants too. -/
structure TModule where
  name        : String
  imports     : List ImportDecl := []
  structs     : List StructDef := []
  enums       : List EnumDef := []
  functions   : List TFnDecl := []
  implBlocks  : List ImplBlock := []
  traits      : List TraitDef := []
  traitImpls  : List ImplTraitBlock := []
  constants   : List ConstDef := []
  typeAliases : List TypeAlias := []
  capAliases  : List CapAlias := []
  externFns   : List ExternFnDecl := []
  specFns     : List SpecFnDecl := []
  newtypes    : List NewtypeDef := []
  submodules  : List TModule := []

/-- The type-axis certificate: a program that has passed Check, carried as typed
    syntax. `Elab` consumes this and does zero source-level type inference; there
    is no untyped `Expr` for it to see. This will replace `ResolvedProgram` as
    Elab's input once Check produces it (the large next stage). -/
structure TypedProgram where
  modules : List TModule

end Concrete
