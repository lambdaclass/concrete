import Concrete.Elab.Core

namespace Concrete

/-! ## ElabVerify (R-0492 step 3, verified-pipeline S5)

Elab gives every local binder and every reference to a source binding in Core a `LocalId` (the
identity the local walk minted, `CLocal.id`). This pass checks, over the Core Elab produced, that
those identities and the Core NAMES every backend uses tell the same story:

* B1 — every identity a binder introduces is not already visible where it is introduced (one
  binding, one identity; the parser's per-alternative copies of an or-pattern arm are disjoint
  scopes, so they may reuse it);
* U1 — a reference that carries an identity names a binding that is visible there, and spells it
  with that binding's Core name (a renaming bug in Elab, the bug 045 / 087 class, would make the
  name and the identity disagree);
* U2 — a reference without an identity (a constant, a function value, a compiler temporary) does
  not spell the Core name of a visible identified binding: that would be a source binding whose
  identity Elab dropped, which the backends would then resolve by name.

Scoping is the Core program's own: a block's binders are visible to the statements after them in
that block; an arm's binders in its guard and body; a borrow block's reference in its body.
Violations are internal compiler errors: a program the checker accepted reached Core with
inconsistent binding identities.
-/

/-- One visible binder: its Core name and, for a source binding, its identity. -/
private structure VB where
  name : String
  id : Option LocalId

private abbrev VM := StateM (List String)

private def flag (msg : String) : VM Unit := modify (· ++ [msg])

private def introduce (fn : String) (vis : List VB) (b : CLocal) : VM (List VB) := do
  if let some i := b.id then
    if vis.any (·.id == some i) then
      flag s!"B1 in '{fn}': the binding identity of '{b.name}' is introduced twice in one scope"
  return { name := b.name, id := b.id } :: vis

private def use (fn : String) (vis : List VB) (r : CLocal) : VM Unit := do
  match r.id with
  | some i =>
    match vis.find? (·.id == some i) with
    | some v =>
      if v.name != r.name then
        flag s!"U1 in '{fn}': a reference spelled '{r.name}' carries the identity of the binding named '{v.name}'"
    | none => flag s!"U1 in '{fn}': a reference to '{r.name}' carries an identity no visible binding has"
  | none =>
    if let some v := vis.find? (·.name == r.name) then
      if v.id.isSome then
        flag s!"U2 in '{fn}': a reference spelled '{r.name}' has no identity, but a visible source binding has that Core name"

mutual
private partial def vExpr (fn : String) (vis : List VB) : CExpr → VM Unit
  | .ident r _ => use fn vis r
  | .binOp _ l r _ | .arrayIndex l r _ | .allocCall l r _ => do vExpr fn vis l; vExpr fn vis r
  | .unaryOp _ x _ | .fieldAccess x _ _ | .borrow x _ | .borrowMut x _ | .deref x _ | .cast x _ =>
    vExpr fn vis x
  | .call _ _ args _ | .arrayLit args _ => args.forM (vExpr fn vis)
  | .structLit _ _ fs _ | .enumLit _ _ _ fs _ => fs.forM (fun (_, e) => vExpr fn vis e)
  | .match_ s arms _ => do vExpr fn vis s; arms.forM (vArm fn vis)
  | .ifExpr c t e _ => do vExpr fn vis c; vBlock fn vis t; vBlock fn vis e
  | .intLit _ _ | .floatLit _ _ | .boolLit _ | .strLit _ | .charLit _ | .fnRef _ _ => pure ()

private partial def vArm (fn : String) (vis : List VB) : CMatchArm → VM Unit
  | .enumArm _ _ bs g body => do
    let mut v := vis
    for (b, _) in bs do
      if b.name != "_" then v ← introduce fn v b
    g.forM (vExpr fn v); vBlock fn v body
  | .varArm b _ g body => do
    let v ← if b.name == "_" then pure vis else introduce fn vis b
    g.forM (vExpr fn v); vBlock fn v body
  | .litArm x g body => do vExpr fn vis x; g.forM (vExpr fn vis); vBlock fn vis body
  | .rangeArm lo hi _ g body => do
    vExpr fn vis lo; vExpr fn vis hi; g.forM (vExpr fn vis); vBlock fn vis body

private partial def vBlock (fn : String) (vis : List VB) (stmts : List CStmt) : VM Unit := do
  let mut v := vis
  for s in stmts do
    v ← vStmt fn v s

private partial def vStmt (fn : String) (vis : List VB) : CStmt → VM (List VB)
  | .letDecl b _ _ val => do vExpr fn vis val; introduce fn vis b
  | .assign r val => do vExpr fn vis val; use fn vis r; pure vis
  | .return_ v _ | .break_ v _ => do v.forM (vExpr fn vis); pure vis
  | .expr e _ | .defer e => do vExpr fn vis e; pure vis
  | .ifElse c t e => do vExpr fn vis c; vBlock fn vis t; e.forM (vBlock fn vis); pure vis
  | .while_ c body _ step => do vExpr fn vis c; vBlock fn vis body; vBlock fn vis step; pure vis
  | .fieldAssign o _ v | .derefAssign o v => do vExpr fn vis o; vExpr fn vis v; pure vis
  | .arrayIndexAssign a i v => do vExpr fn vis a; vExpr fn vis i; vExpr fn vis v; pure vis
  | .continue_ _ => pure vis
  | .borrowIn var ref _ _ _ body => do
    use fn vis var
    let v ← introduce fn vis ref
    vBlock fn v body; pure vis
end

/-- The ElabVerify findings for one function: parameters (with their identities), then the body. -/
def elabVerifyFn (f : CFnDef) : List String :=
  let go : VM Unit := do
    let params : List VB := (f.params.zip (f.paramIds.map some ++ List.replicate f.params.length none)).map
      fun ((n, _), i) => { name := n, id := i }
    vBlock f.name params.reverse f.body
  (go.run []).2

/-- ElabVerify over a module forest (every function, method and submodule). -/
partial def elabVerifyModules (ms : List CModule) : List String :=
  ms.flatMap fun m => m.functions.flatMap elabVerifyFn ++ elabVerifyModules m.submodules

end Concrete
