import Concrete.Elab.Core

namespace Concrete

/-! ## CoreCanonicalize — Core→Core normalization pass

Runs after Elab, before CoreCheck. Canonicalizes the Core IR,
recursing through all top-level modules and nested submodules:
- Normalize pattern match arm ordering (wildcard/var arms last)
- Canonical ordering of struct fields in literals (match definition order)
- Normalize type representations (Ty.generic "Heap" [t] → Ty.heap t)
- Types in submodule traitDefs/traitImpls are canonicalized
-/

-- ============================================================
-- Type normalization
-- ============================================================

/-- Normalize Ty.generic "Heap"/"HeapArray" to Ty.heap/Ty.heapArray. -/
private def canonTy : Ty → Ty
  | .generic "Heap" [inner] _ => .heap (canonTy inner)
  | .generic "HeapArray" [inner] _ => .heapArray (canonTy inner)
  | .ref t => .ref (canonTy t)
  | .refMut t => .refMut (canonTy t)
  | .heap t => .heap (canonTy t)
  | .heapArray t => .heapArray (canonTy t)
  | .array t n => .array (canonTy t) n
  | .generic name args caps => .generic name (args.map canonTy) caps
  | .ptrMut t => .ptrMut (canonTy t)
  | .ptrConst t => .ptrConst (canonTy t)
  | .fn_ ps cs ret => .fn_ (ps.map canonTy) cs (canonTy ret)
  | t => t

-- ============================================================
-- Match arm ordering: wildcard/var arms last
-- ============================================================

private def armIsWildcard : CMatchArm → Bool
  | .varArm _ _ none _ => true   -- a GUARDED var arm can fall through, so it is not a catch-all
  | _ => false

/-- Sort match arms: specific arms first, var/wildcard arms last. -/
private def sortMatchArms (arms : List CMatchArm) : List CMatchArm :=
  let (specific, wild) := arms.partition (fun a => !armIsWildcard a)
  specific ++ wild

-- ============================================================
-- Core expression/statement canonicalization
-- ============================================================

-- `reorderFields`/`lookupStructFields` lived here and are deleted, not kept "in case": they
-- reordered struct-literal fields into definition order, which under the 2026-08-08 source-order
-- decision would change which initializer runs first. Dead normalization code is worse than none
-- — the next person reads it as the intended behaviour.

mutual
partial def canonExpr (structs : List CStructDef) : CExpr → CExpr
  | .intLit v ty => .intLit v (canonTy ty)
  | .floatLit v ty => .floatLit v (canonTy ty)
  | .boolLit b => .boolLit b
  | .strLit s => .strLit s
  | .charLit c => .charLit c
  | .ident n ty => .ident n (canonTy ty)
  | .binOp op l r ty => .binOp op (canonExpr structs l) (canonExpr structs r) (canonTy ty)
  | .unaryOp op e ty => .unaryOp op (canonExpr structs e) (canonTy ty)
  | .call fn targs args ty =>
    .call fn (targs.map canonTy) (args.map (canonExpr structs)) (canonTy ty)
  | .structLit name targs fields ty =>
    -- Field order is NOT canonicalized (language decision, 2026-08-08: initializers evaluate in
    -- SOURCE order). This pass used to reorder the list to definition order, and the list order
    -- is what `Interp.evalFields` evaluates in — so the reorder silently changed which
    -- initializer ran first. Under source-order semantics two literals differing only in written
    -- order are different programs whenever an initializer traps or has effects, and a
    -- normalization that erases an observable distinction is not a normalization.
    --
    -- LAYOUT is unaffected: it derives from the struct DEFINITION, never from a literal's field
    -- order, and every consumer looks fields up by name.
    .structLit name (targs.map canonTy) (fields.map fun (n, e) => (n, canonExpr structs e)) (canonTy ty)
  | .fieldAccess obj f ty => .fieldAccess (canonExpr structs obj) f (canonTy ty)
  | .enumLit en v targs fields ty =>
    .enumLit en v (targs.map canonTy) (fields.map fun (n, e) => (n, canonExpr structs e)) (canonTy ty)
  | .match_ scrut arms ty =>
    let arms' := sortMatchArms (arms.map (canonArm structs))
    .match_ (canonExpr structs scrut) arms' (canonTy ty)
  | .borrow inner ty => .borrow (canonExpr structs inner) (canonTy ty)
  | .borrowMut inner ty => .borrowMut (canonExpr structs inner) (canonTy ty)
  | .deref inner ty => .deref (canonExpr structs inner) (canonTy ty)
  | .arrayLit elems ty => .arrayLit (elems.map (canonExpr structs)) (canonTy ty)
  | .arrayIndex arr idx ty => .arrayIndex (canonExpr structs arr) (canonExpr structs idx) (canonTy ty)
  | .cast inner t => .cast (canonExpr structs inner) (canonTy t)
  | .fnRef n ty => .fnRef n (canonTy ty)
  | .allocCall inner alloc ty =>
    .allocCall (canonExpr structs inner) (canonExpr structs alloc) (canonTy ty)
  | .ifExpr cond then_ else_ ty =>
    .ifExpr (canonExpr structs cond) (canonStmts structs then_) (canonStmts structs else_) (canonTy ty)

partial def canonArm (structs : List CStructDef) : CMatchArm → CMatchArm
  | .enumArm en v binds guard body =>
    .enumArm en v (binds.map fun (n, t) => (n, canonTy t)) (guard.map (canonExpr structs)) (canonStmts structs body)
  | .litArm val guard body =>
    .litArm (canonExpr structs val) (guard.map (canonExpr structs)) (canonStmts structs body)
  | .varArm b ty guard body =>
    .varArm b (canonTy ty) (guard.map (canonExpr structs)) (canonStmts structs body)
  | .rangeArm lo hi incl guard body =>
    .rangeArm (canonExpr structs lo) (canonExpr structs hi) incl (guard.map (canonExpr structs)) (canonStmts structs body)

partial def canonStmt (structs : List CStructDef) : CStmt → CStmt
  | .letDecl n m ty val => .letDecl n m (canonTy ty) (canonExpr structs val)
  | .assign n val => .assign n (canonExpr structs val)
  | .return_ (some v) ty => .return_ (some (canonExpr structs v)) (canonTy ty)
  | .return_ none ty => .return_ none (canonTy ty)
  | .expr e iv => .expr (canonExpr structs e) iv
  | .ifElse c t el =>
    .ifElse (canonExpr structs c) (canonStmts structs t) (el.map (canonStmts structs))
  | .while_ c body lbl step => .while_ (canonExpr structs c) (canonStmts structs body) lbl (canonStmts structs step)
  | .fieldAssign obj f val =>
    .fieldAssign (canonExpr structs obj) f (canonExpr structs val)
  | .derefAssign target val =>
    .derefAssign (canonExpr structs target) (canonExpr structs val)
  | .arrayIndexAssign arr idx val =>
    .arrayIndexAssign (canonExpr structs arr) (canonExpr structs idx) (canonExpr structs val)
  | .break_ (some v) lbl => .break_ (some (canonExpr structs v)) lbl
  | .break_ none lbl => .break_ none lbl
  | .continue_ lbl => .continue_ lbl
  | .defer body => .defer (canonExpr structs body)
  | .borrowIn v r reg isMut ty body =>
    .borrowIn v r reg isMut (canonTy ty) (canonStmts structs body)

partial def canonStmts (structs : List CStructDef) (stmts : List CStmt) : List CStmt :=
  stmts.map (canonStmt structs)
end

-- ============================================================
-- Module / Program entry points
-- ============================================================

def canonFn (structs : List CStructDef) (f : CFnDef) : CFnDef :=
  { f with
    params := f.params.map fun (n, t) => (n, canonTy t),
    retTy := canonTy f.retTy,
    body := canonStmts structs f.body }

partial def canonModule (m : CModule) : CModule :=
  let structs := m.structs
  { m with
    structs := m.structs.map fun s =>
      { s with fields := s.fields.map fun (n, t) => (n, canonTy t) },
    enums := m.enums.map fun e =>
      { e with variants := e.variants.map fun (vn, fields) =>
        (vn, fields.map fun (fn, ft) => (fn, canonTy ft)) },
    functions := m.functions.map (canonFn structs),
    constants := m.constants.map fun (n, t, e) => (n, canonTy t, canonExpr structs e),
    traitDefs := m.traitDefs.map fun td =>
      { td with methods := td.methods.map fun sig =>
        { sig with retTy := canonTy sig.retTy } },
    traitImpls := m.traitImpls.map fun ti =>
      { ti with methodRetTys := ti.methodRetTys.map fun (n, t) => (n, canonTy t) },
    submodules := m.submodules.map canonModule }

def canonicalizeProgram (modules : List CModule) : List CModule :=
  modules.map canonModule

end Concrete
