import Concrete.Frontend.AST
import Concrete.Frontend.LocalId

namespace Concrete

/-! ## Local binder resolution (R-0492 slice 1, verified-pipeline S5)

The ONE lexical walk over a (desugared) function body. It mints a `LocalId` for every local
value binding, records which binding every use of a local name denotes, and records each
SCOPE-1@1 collision (a binder reusing the spelling of a binding still visible at it). The checker
consumes the table: its environment is keyed by `LocalId`, its E0301 diagnostics are this walk's
collisions, and it never decides by spelling which binding a name denotes.

Visibility is lexical (MEMORY_SEMANTICS §15.1): a `let` is visible from its declaration to the
end of its block, a parameter in the whole body, an arm / `if let` / `while let` binder in its
arm (guard and body), a `for` header binder in the header and the body, a borrow-block
reference and region name in the block. Sibling scopes (two branches, two arms, consecutive
blocks and loops) never see each other, and a declaration in a loop body is one binding however
often it runs (SCOPE-2). Assignment introduces no binding; it is a use of the binding assigned.

The walk is the former `scope1*` walk of `Check.lean`, which computed visibility for E0301 only;
it now also produces the identities, so E0301 and binding identity cannot disagree.
-/

/-- One visible binding: spelling, where it was introduced, identity. -/
private structure Visible where
  name : String
  span : Span
  id : LocalId

private abbrev LocalsM := StateM LocalTable

/-- Introduce a binding of `name` (form `kind`) at `key`/`sp` into the chain `vis`. `_` is a
    wildcard, not a binding. A reuse of a visible spelling is recorded as a collision and the
    binding is introduced either way, so one collision is reported once. -/
private def bindLocal (vis : List Visible) (key : BinderKey) (name : String) (sp : Span)
    (kind : String) : LocalsM (List Visible) := do
  if name.isEmpty then return vis
  -- `_` binds nothing visible (no use can name it, and it collides with nothing), but where the
  -- checker registers it (a `_` parameter, a borrow reference, a destructured field) it is still
  -- a binding with an obligation, so it has an identity.
  if name == "_" then
    let (t', _) := (← get).mintBinder key name
    set t'
    return vis
  if let some prev := vis.find? (·.name == name) then
    modify fun t => t.mintCollision { name, kind, at_ := sp, earlier := prev.span }
  let (t', id) := (← get).mintBinder key name
  set t'
  return { name, span := sp, id } :: vis

/-- Record a use of `name` at `sp`, when `name` is a visible local there. -/
private def useLocal (vis : List Visible) (sp : Span) (name : String) : LocalsM Unit := do
  if let some v := vis.find? (·.name == name) then
    modify fun t => t.mintUse (SiteKey.ofSpan sp) name v.id

private def siteKey (sp : Span) (name : String) : BinderKey := .site (SiteKey.ofSpan sp) name

/-- The key of a struct-destructure binder: the statement's position, the FIELD and the spelling.
    One statement binds several fields, possibly several `_`s (Codex review, round 2: keyed by
    spelling alone, `S { a: _, b: _ }` gave both one identity, and the second registration
    erased the first field's obligation). The checker computes the same key (`destructureKey`). -/
def destructureKey (sp : Span) (b : PatBind) : BinderKey :=
  .site (SiteKey.ofSpan sp) s!"{b.fieldName}={b.name}"

mutual
private partial def walkExpr (vis : List Visible) : Expr → LocalsM Unit
  | .ident sp n => useLocal vis sp n
  -- a call through a fn-typed local spells the local as the callee
  | .call sp f _ args => do useLocal vis sp f; args.forM (walkExpr vis)
  | .binOp _ _ l r | .arrayIndex _ l r | .allocCall _ l r => do
    walkExpr vis l; walkExpr vis r
  | .unaryOp _ _ x | .paren _ x | .fieldAccess _ x _ | .borrow _ x | .borrowMut _ x
  | .deref _ x | .cast _ x _ => walkExpr vis x
  | .staticMethodCall _ _ _ _ args | .arrayLit _ args => args.forM (walkExpr vis)
  | .methodCall _ o _ _ args => do walkExpr vis o; args.forM (walkExpr vis)
  | .structLit _ _ _ fs base => do
    fs.forM (fun (_, e) => walkExpr vis e); base.forM (walkExpr vis)
  | .enumLit _ _ _ _ fs => fs.forM (fun (_, e) => walkExpr vis e)
  | .match_ _ scr arms => do walkExpr vis scr; arms.forM (walkArm vis)
  | .ifExpr _ c t e => do walkExpr vis c; walkBlock vis t; walkBlock vis e
  | .intLit _ _ | .floatLit _ _ | .boolLit _ _ | .strLit _ _ | .charLit _ _ | .fnRef _ _ => pure ()

/-- An arm: its pattern binders are visible in its guard and body only. A destructuring `let`
    reaches here after `desugarStmts` (its binders scope over the continuation, which is the arm
    body). -/
private partial def walkArm (vis : List Visible) : MatchArm → LocalsM Unit
  | .mk sp _ _ bs g body => do
    let mut v := vis
    for b in bs do
      v ← bindLocal v (siteKey sp b.name) b.name sp
        (if b.letBound then "destructuring `let` binder" else "pattern binder")
    g.forM (walkExpr v); walkBlock v body
  | .varArm sp b g body => do
    let v ← bindLocal vis (siteKey sp b) b sp "pattern binder"
    g.forM (walkExpr v); walkBlock v body
  | .litArm _ val g body => do
    walkExpr vis val; g.forM (walkExpr vis); walkBlock vis body
  | .rangeArm _ lo hi _ g body => do
    walkExpr vis lo; walkExpr vis hi; g.forM (walkExpr vis); walkBlock vis body

/-- A block: its declarations are visible to the statements after them, not after it. -/
private partial def walkBlock (vis : List Visible) (stmts : List Stmt) : LocalsM Unit := do
  let mut v := vis
  for s in stmts do
    v ← walkStmt v s

/-- One statement; returns the chain visible to the next statement of the same block. -/
private partial def walkStmt (vis : List Visible) : Stmt → LocalsM (List Visible)
  | .letDecl sp n _ _ val _ => do
    -- the initializer is walked before the binder exists: in `let s = f(s)` the `s` read is
    -- the earlier binding (and the new binder collides with it)
    walkExpr vis val; bindLocal vis (siteKey sp n) n sp "`let`"
  | .assign sp n val => do walkExpr vis val; useLocal vis sp n; pure vis
  | .return_ _ v | .break_ _ v _ => do v.forM (walkExpr vis); pure vis
  | .expr _ e _ | .defer _ e | .assert_ _ e | .assume_ _ e => do walkExpr vis e; pure vis
  | .ifElse _ c t e => do
    walkExpr vis c; walkBlock vis t; e.forM (walkBlock vis); pure vis
  | .while_ _ c body _ => do walkExpr vis c; walkBlock vis body; pure vis
  | .forLoop _ init c step body _ => do
    -- the header binder is visible in the header and the body, not after the loop
    let h ← match init with
      | some i => walkStmt vis i
      | none => pure vis
    walkExpr h c; walkBlock h body
    match step with
    | some st => discard <| walkStmt h st
    | none => pure ()
    pure vis
  | .fieldAssign _ o _ v | .derefAssign _ o v => do walkExpr vis o; walkExpr vis v; pure vis
  | .arrayIndexAssign _ a i v => do walkExpr vis a; walkExpr vis i; walkExpr vis v; pure vis
  | .continue_ _ _ => pure vis
  | .borrowIn sp var ref region _ body => do
    -- the borrowed variable is a use of a visible binding; the reference and the region name
    -- are bindings of the block
    useLocal vis sp var
    let v1 ← bindLocal vis (siteKey sp ref) ref sp "borrow-block reference"
    let v2 ← bindLocal v1 (siteKey sp region) region sp "borrow-block region name"
    walkBlock v2 body; pure vis
  | .letDestructure sp _ _ bs val els => do
    walkExpr vis val; els.forM (walkBlock vis)
    let mut v := vis
    for b in bs do v ← bindLocal v (siteKey sp b.name) b.name sp "destructuring `let` binder"
    pure v
  | .letStructDestructure sp _ bs val => do
    walkExpr vis val
    let mut v := vis
    for b in bs do v ← bindLocal v (destructureKey sp b) b.name sp "destructuring `let` binder"
    pure v
end

/-- The local table of one function: its parameters (in order), then its body. -/
def resolveFnLocals (f : FnDef) : LocalTable :=
  let go : LocalsM Unit := do
    let mut vis : List Visible := []
    for (p, i) in f.params.zipIdx do
      vis ← bindLocal vis (.param i) p.name f.span "parameter"
    walkBlock vis f.body
  (go.run LocalTable.mintStart).2

/-- The local table of a parameter list with no body (a spec fn, an extern fn, a trait method
    declaration): only its parameters, so two of one name collide. -/
def resolveParamLocals (ps : List Param) (sp : Span) : LocalTable :=
  let go : LocalsM Unit := do
    let mut vis : List Visible := []
    for (p, i) in ps.zipIdx do
      vis ← bindLocal vis (.param i) p.name sp "parameter"
  (go.run LocalTable.mintStart).2

/-- The local table of a module constant's initializer (no parameters, nothing visible). -/
def resolveExprLocals (e : Expr) : LocalTable :=
  ((walkExpr [] e).run LocalTable.mintStart).2

/-- Attach a local table to every function and method of a module (and its submodules). -/
partial def resolveModuleLocals (m : Module) : Module :=
  let withLocals (f : FnDef) : FnDef := { f with locals := resolveFnLocals f }
  { m with
    functions := m.functions.map withLocals
    constants := m.constants.map fun c => { c with locals := resolveExprLocals c.value }
    implBlocks := m.implBlocks.map fun ib => { ib with methods := ib.methods.map withLocals }
    traitImpls := m.traitImpls.map fun tb => { tb with methods := tb.methods.map withLocals }
    submodules := m.submodules.map resolveModuleLocals }

end Concrete
