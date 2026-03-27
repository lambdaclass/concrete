import Concrete.Core

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
  | .generic "Heap" [inner] => .heap (canonTy inner)
  | .generic "HeapArray" [inner] => .heapArray (canonTy inner)
  | .ref t => .ref (canonTy t)
  | .refMut t => .refMut (canonTy t)
  | .heap t => .heap (canonTy t)
  | .heapArray t => .heapArray (canonTy t)
  | .array t n => .array (canonTy t) n
  | .generic name args => .generic name (args.map canonTy)
  | .ptrMut t => .ptrMut (canonTy t)
  | .ptrConst t => .ptrConst (canonTy t)
  | .fn_ ps cs ret => .fn_ (ps.map canonTy) cs (canonTy ret)
  | t => t

-- ============================================================
-- Match arm ordering: wildcard/var arms last
-- ============================================================

private def armIsWildcard : CMatchArm → Bool
  | .varArm _ _ _ => true
  | _ => false

/-- Sort match arms: specific arms first, var/wildcard arms last. -/
private def sortMatchArms (arms : List CMatchArm) : List CMatchArm :=
  let (specific, wild) := arms.partition (fun a => !armIsWildcard a)
  specific ++ wild

-- ============================================================
-- Struct field reordering
-- ============================================================

/-- Reorder struct literal fields to match definition order. -/
private def reorderFields (defFields : List (String × Ty))
    (litFields : List (String × CExpr)) : List (String × CExpr) :=
  defFields.filterMap fun (name, _) =>
    litFields.find? fun (fn, _) => fn == name

-- ============================================================
-- Core expression/statement canonicalization
-- ============================================================

private def lookupStructFields (structs : List CStructDef) (name : String) : Option (List (String × Ty)) :=
  (structs.find? fun s => s.name == name).map (·.fields)

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
    let fields' := fields.map fun (n, e) => (n, canonExpr structs e)
    let ordered := match lookupStructFields structs name with
      | some defFields => reorderFields defFields fields'
      | none => fields'
    .structLit name (targs.map canonTy) ordered (canonTy ty)
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
  | .try_ inner ty => .try_ (canonExpr structs inner) (canonTy ty)
  | .allocCall inner alloc ty =>
    .allocCall (canonExpr structs inner) (canonExpr structs alloc) (canonTy ty)
  | .whileExpr cond body elseBody ty =>
    .whileExpr (canonExpr structs cond) (canonStmts structs body) (canonStmts structs elseBody) (canonTy ty)
  | .ifExpr cond then_ else_ ty =>
    .ifExpr (canonExpr structs cond) (canonStmts structs then_) (canonStmts structs else_) (canonTy ty)

partial def canonArm (structs : List CStructDef) : CMatchArm → CMatchArm
  | .enumArm en v binds body =>
    .enumArm en v (binds.map fun (n, t) => (n, canonTy t)) (canonStmts structs body)
  | .litArm val body =>
    .litArm (canonExpr structs val) (canonStmts structs body)
  | .varArm b ty body =>
    .varArm b (canonTy ty) (canonStmts structs body)

partial def canonStmt (structs : List CStructDef) : CStmt → CStmt
  | .letDecl n m ty val => .letDecl n m (canonTy ty) (canonExpr structs val)
  | .assign n val => .assign n (canonExpr structs val)
  | .return_ (some v) ty => .return_ (some (canonExpr structs v)) (canonTy ty)
  | .return_ none ty => .return_ none (canonTy ty)
  | .expr e => .expr (canonExpr structs e)
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
