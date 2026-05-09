import Concrete.Core

namespace Concrete.Interp

open Concrete

/-! ## Source-level interpreter for the predictable/core subset

    Currently supported (oracle subset, see `docs/INTERPRETER_TRUST.md`):
    integer/bool, structs, enums, arrays, match, bounded loops, calls,
    immutable/mutable borrows of locals, fields, and array elements.

    Unsupported constructs fail with explicit "interp: ..." diagnostics
    so the differential harness in `tests/oracle/` records them as
    PENDING rather than silently mismatching.

    Limitations:
    - No I/O (print/println), no capabilities
    - No float, char, string values
    - No overflow/truncation (arbitrary-precision integers)
-/

-- ============================================================
-- Reference paths (for borrows / mutable refs)
-- ============================================================

/-- A step inside a reference path: into a struct field or array element. -/
inductive RefStep where
  | field (name : String)
  | index (i : Nat)
  deriving Repr, Inhabited

/-- A reference path roots at a variable name in the env at borrow
    creation time and walks through fields/indices. `frame` is the
    env.length at borrow time; it lets the lookup skip past callee
    frames that may shadow the borrowed name. Structured-scope rules
    ensure the binding outlives every use of the borrow. -/
structure RefPath where
  base : String
  frame : Nat
  steps : List RefStep
  isMut : Bool := false
  deriving Repr, Inhabited

-- ============================================================
-- Interpreter values
-- ============================================================

inductive IVal where
  | int (val : Int) (ty : Ty)
  | bool (val : Bool)
  | struct_ (name : String) (fields : List (String × IVal))
  | enum_ (enumName variant : String) (fields : List (String × IVal))
  | array (elems : Array IVal) (elemTy : Ty) (size : Nat)
  | unit
  | ref (path : RefPath)
  deriving Repr, Inhabited

-- ============================================================
-- Control flow signals
-- ============================================================

inductive Flow where
  | val (v : IVal)
  | ret (v : IVal)
  | brk
  | cont
  deriving Repr

-- ============================================================
-- Environment (flat list, latest binding wins)
-- ============================================================

abbrev Env := List (String × IVal)

partial def envGet (env : Env) (name : String) : Option IVal :=
  match env with
  | [] => none
  | (n, v) :: rest => if n == name then some v else envGet rest name

def envBind (env : Env) (name : String) (val : IVal) : Env :=
  (name, val) :: env

partial def envSet (env : Env) (name : String) (val : IVal) : Env :=
  match env with
  | [] => []
  | (n, v) :: rest =>
    if n == name then (n, val) :: rest
    else (n, v) :: envSet rest name val

-- ============================================================
-- Collect all function definitions from module tree
-- ============================================================

partial def collectFns : List CModule → List CFnDef
  | [] => []
  | m :: ms => m.functions ++ collectFns m.submodules ++ collectFns ms

def findFn (fns : List CFnDef) (name : String) : Option CFnDef :=
  fns.find? (fun f => f.name == name)

-- ============================================================
-- Array safe access
-- ============================================================

private def listGet (xs : List IVal) (n : Nat) : Option IVal :=
  match xs, n with
  | [], _ => none
  | x :: _, 0 => some x
  | _ :: rest, n + 1 => listGet rest n

private def arrayGet (elems : Array IVal) (n : Nat) : Option IVal :=
  listGet elems.toList n

-- ============================================================
-- Binary operations
-- ============================================================

private def intXor (a b : Int) : Int :=
  Int.ofNat (Nat.xor a.toNat b.toNat)

private def intAnd (a b : Int) : Int :=
  Int.ofNat (Nat.land a.toNat b.toNat)

private def intOr (a b : Int) : Int :=
  Int.ofNat (Nat.lor a.toNat b.toNat)

def evalBinOp (op : BinOp) (lhs rhs : IVal) : Except String IVal :=
  match op, lhs, rhs with
  | .add, .int a _, .int b ty => .ok (.int (a + b) ty)
  | .sub, .int a _, .int b ty => .ok (.int (a - b) ty)
  | .mul, .int a _, .int b ty => .ok (.int (a * b) ty)
  | .div, .int _ _, .int 0 _ => .error "interp: division by zero"
  | .div, .int a _, .int b ty => .ok (.int (a / b) ty)
  | .mod, .int _ _, .int 0 _ => .error "interp: modulo by zero"
  | .mod, .int a _, .int b ty => .ok (.int (a % b) ty)
  | .eq, .int a _, .int b _ => .ok (.bool (a == b))
  | .neq, .int a _, .int b _ => .ok (.bool (a != b))
  | .lt, .int a _, .int b _ => .ok (.bool (a < b))
  | .gt, .int a _, .int b _ => .ok (.bool (a > b))
  | .leq, .int a _, .int b _ => .ok (.bool (a <= b))
  | .geq, .int a _, .int b _ => .ok (.bool (a >= b))
  | .and_, .bool a, .bool b => .ok (.bool (a && b))
  | .or_, .bool a, .bool b => .ok (.bool (a || b))
  | .eq, .bool a, .bool b => .ok (.bool (a == b))
  | .neq, .bool a, .bool b => .ok (.bool (a != b))
  | .bitxor, .int a _, .int b ty => .ok (.int (intXor a b) ty)
  | .bitand, .int a _, .int b ty => .ok (.int (intAnd a b) ty)
  | .bitor, .int a _, .int b ty => .ok (.int (intOr a b) ty)
  | .shl, .int a _, .int b ty => .ok (.int (a * (2 ^ b.toNat)) ty)
  | .shr, .int a _, .int b ty => .ok (.int (a / (2 ^ b.toNat)) ty)
  | _, _, _ => .error "interp: unsupported binop on given value types"

-- ============================================================
-- Unary operations
-- ============================================================

def evalUnaryOp (op : UnaryOp) (v : IVal) : Except String IVal :=
  match op, v with
  | .neg, .int n ty => .ok (.int (-n) ty)
  | .not_, .bool b => .ok (.bool (!b))
  | .bitnot, .int n ty => .ok (.int (-(n + 1)) ty)
  | _, _ => .error "interp: unsupported unary op"

-- ============================================================
-- Type coercion (integer type conversions only)
-- ============================================================

def evalCast (v : IVal) (targetTy : Ty) : Except String IVal :=
  match v with
  | .int n _ => .ok (.int n targetTy)
  | .bool true => .ok (.int 1 targetTy)
  | .bool false => .ok (.int 0 targetTy)
  | _ => .error "interp: unsupported cast"

-- ============================================================
-- Reference path lookup / update
-- ============================================================

private partial def lookupAt (val : IVal) (steps : List RefStep) : Except String IVal :=
  match steps with
  | [] => .ok val
  | .field fname :: rest =>
    match val with
    | .struct_ _ fields =>
      match fields.find? (fun (n, _) => n == fname) with
      | some (_, fv) => lookupAt fv rest
      | none => .error s!"interp: ref path field '{fname}' not found"
    | _ => .error "interp: ref step .field on non-struct"
  | .index i :: rest =>
    match val with
    | .array elems _ _ =>
      match arrayGet elems i with
      | some ev => lookupAt ev rest
      | none => .error s!"interp: ref path index {i} out of bounds (size {elems.size})"
    | _ => .error "interp: ref step .index on non-array"

/-- Drop entries pushed onto env after the ref was created, so that
    same-named callee bindings don't shadow the borrowed binding. -/
private def envViewAtFrame (env : Env) (frame : Nat) : Env :=
  if env.length >= frame then env.drop (env.length - frame) else env

partial def lookupPath (env : Env) (p : RefPath) : Except String IVal := do
  let view := envViewAtFrame env p.frame
  match envGet view p.base with
  | none => .error s!"interp: ref path base '{p.base}' not in env"
  | some v => lookupAt v p.steps

private partial def updateAt (val : IVal) (steps : List RefStep) (newVal : IVal) : Except String IVal :=
  match steps with
  | [] => .ok newVal
  | .field fname :: rest =>
    match val with
    | .struct_ sname fields =>
      match fields.find? (fun (n, _) => n == fname) with
      | none => .error s!"interp: ref update field '{fname}' not found"
      | some (_, fv) => do
        let newFv ← updateAt fv rest newVal
        let newFields := fields.map fun (n, v) =>
          if n == fname then (n, newFv) else (n, v)
        return .struct_ sname newFields
    | _ => .error "interp: ref step .field on non-struct (update)"
  | .index i :: rest =>
    match val with
    | .array elems elemTy size =>
      if h : i < elems.size then do
        let oldElem := elems[i]
        let newElem ← updateAt oldElem rest newVal
        return .array (elems.set i newElem) elemTy size
      else .error s!"interp: ref update index {i} out of bounds (size {elems.size})"
    | _ => .error "interp: ref step .index on non-array (update)"

/-- Like envSet but only rewrites bindings at depth >= `skip` (counting
    from the head). Used so that a ref-update walks past intervening
    callee frames and rewrites the binding that was actually borrowed. -/
private partial def envSetSkipping (env : Env) (skip : Nat) (name : String) (val : IVal) : Env :=
  match env, skip with
  | [], _ => []
  | (n, v) :: rest, 0 =>
    if n == name then (n, val) :: rest
    else (n, v) :: envSetSkipping rest 0 name val
  | (n, v) :: rest, k + 1 =>
    (n, v) :: envSetSkipping rest k name val

partial def updatePath (env : Env) (p : RefPath) (newVal : IVal) : Except String Env := do
  let skip := if env.length >= p.frame then env.length - p.frame else 0
  let view := envViewAtFrame env p.frame
  match envGet view p.base with
  | none => .error s!"interp: ref path base '{p.base}' not in env (update)"
  | some baseVal => do
    let newBase ← updateAt baseVal p.steps newVal
    return envSetSkipping env skip p.base newBase

/-- If `v` is a ref, follow it; otherwise return `v` unchanged.
    Used for auto-deref on field-access / array-index receivers. -/
private def autoDeref (env : Env) (v : IVal) : Except String IVal :=
  match v with
  | .ref p => lookupPath env p
  | other => .ok other

-- ============================================================
-- Helpers (must precede mutual block — no forward refs in Lean 4)
-- ============================================================

private def bindParams (env : Env) (params : List (String × Ty)) (args : List IVal) : Env :=
  match params, args with
  | [], _ => env
  | _, [] => env
  | (name, _) :: ps, v :: vs => bindParams (envBind env name v) ps vs

private def bindEnumFields (env : Env) (bindings : List (String × Ty)) (fields : List (String × IVal)) : Env :=
  match bindings with
  | [] => env
  | (name, _) :: rest =>
    let val := match fields.find? (fun (n, _) => n == name) with
      | some (_, v) => v
      | none => .unit
    bindEnumFields (envBind env name val) rest fields

private def matchLit (scrutinee : IVal) (lit : IVal) : Bool :=
  match scrutinee, lit with
  | .int a _, .int b _ => a == b
  | .bool a, .bool b => a == b
  | _, _ => false

-- ============================================================
-- Core evaluator (partial, mutually recursive)
--
-- Every evalX returns the post-evaluation Env so that callee
-- mutations through borrows propagate back to the caller. Most
-- expressions thread env unchanged; only `.call` and the
-- statement-level mutators rewrite it.
-- ============================================================

mutual

/-- Resolve a CExpr to a reference path.
    Acceptable shapes: idents, field accesses, array indices, and
    derefs of a ref (for `&*r` collapse). The borrow target's index
    expressions are evaluated against the current env so the path
    snapshot reflects the value at borrow time. -/
partial def evalBorrowTarget (fns : List CFnDef) (env : Env) (e : CExpr) (isMut : Bool) :
    Except String (Env × RefPath) := do
  match e with
  | .ident name _ =>
    return (env, { base := name, frame := env.length, steps := [], isMut := isMut })
  | .fieldAccess obj fld _ => do
    let (env, p) ← evalBorrowTarget fns env obj isMut
    return (env, { p with steps := p.steps ++ [.field fld] })
  | .arrayIndex arr idx _ => do
    let (env, p) ← evalBorrowTarget fns env arr isMut
    let (env, iv) ← evalExprVal fns env idx
    match iv with
    | .int i _ =>
      if i < 0 then .error s!"interp: negative array index {i} in borrow target"
      else return (env, { p with steps := p.steps ++ [.index i.toNat] })
    | _ => .error "interp: array index in borrow target is not an integer"
  | .deref inner _ => do
    -- &(*r) and &mut (*r): forward to r's path, optionally widening mutability
    let (env, v) ← evalExprVal fns env inner
    match v with
    | .ref p => return (env, { p with isMut := p.isMut || isMut })
    | _ => .error "interp: deref of non-ref in borrow target"
  -- Borrowing a literal yields a ref to a temporary. Surface the underlying
  -- "type X not yet supported" so the harness PENDING reflects the real gap.
  | .strLit _ => .error "interp: string literals not yet supported"
  | .charLit _ => .error "interp: char literals not yet supported"
  | .floatLit _ _ => .error "interp: float literals not yet supported"
  | _ => .error "interp: unsupported borrow target shape"

partial def evalExpr (fns : List CFnDef) (env : Env) (e : CExpr) : Except String (Env × Flow) := do
  match e with
  | .intLit val ty => return (env, .val (.int val ty))
  | .boolLit val => return (env, .val (.bool val))
  | .strLit _ => .error "interp: string literals not yet supported"
  | .charLit _ => .error "interp: char literals not yet supported"
  | .floatLit _ _ => .error "interp: float literals not yet supported"

  | .ident name _ =>
    match envGet env name with
    | some v => return (env, .val v)
    | none => .error s!"interp: undefined variable '{name}'"

  | .binOp op lhs rhs _ => do
    let (env, lv) ← evalExprVal fns env lhs
    let (env, rv) ← evalExprVal fns env rhs
    let result ← evalBinOp op lv rv
    return (env, .val result)

  | .unaryOp op operand _ => do
    let (env, v) ← evalExprVal fns env operand
    let result ← evalUnaryOp op v
    return (env, .val result)

  | .call fnName _ args _ => do
    let (env, argVals) ← evalCallArgs fns env args
    match findFn fns fnName with
    | none => .error s!"interp: undefined function '{fnName}'"
    | some fdef =>
      if fdef.params.length != argVals.length then
        .error s!"interp: arity mismatch calling '{fnName}': expected {fdef.params.length}, got {argVals.length}"
      else
        -- Share env with callee so borrows resolve against caller bindings.
        -- After the call, drop the local frame; mutations to caller-visible
        -- bindings (through ref params or otherwise) survive in the lower
        -- portion of the env.
        let outerLen := env.length
        let callEnv := bindParams env fdef.params argVals
        let (postEnv, flow) ← evalStmts fns callEnv fdef.body
        let restored := postEnv.drop (postEnv.length - outerLen)
        match flow with
        | .ret v => return (restored, .val v)
        | .val v => return (restored, .val v)
        | _ => return (restored, .val .unit)

  | .structLit name _ fields _ => do
    let (env, fieldVals) ← evalFields fns env fields
    return (env, .val (.struct_ name fieldVals))

  | .fieldAccess obj field _ => do
    let (env, v) ← evalExprVal fns env obj
    let target ← autoDeref env v
    match target with
    | .struct_ _ fields =>
      match fields.find? (fun (n, _) => n == field) with
      | some (_, fv) => return (env, .val fv)
      | none => .error s!"interp: field '{field}' not found in struct"
    | _ => .error "interp: field access on non-struct value"

  | .enumLit enumName variant _ fields _ => do
    let (env, fieldVals) ← evalFields fns env fields
    return (env, .val (.enum_ enumName variant fieldVals))

  | .match_ scrutinee arms _ => do
    let (env, sv) ← evalExprVal fns env scrutinee
    evalMatch fns env sv arms

  | .arrayLit elems ty => do
    let (env, vals) ← evalCallArgs fns env elems
    let elemTy := match ty with
      | .array t _ => t
      | _ => .unit
    return (env, .val (.array vals.toArray elemTy vals.length))

  | .arrayIndex arr idx _ => do
    let (env, av) ← evalExprVal fns env arr
    let arrayVal ← autoDeref env av
    match arrayVal with
    | .array elems _ _ => do
      let (env, iv) ← evalExprVal fns env idx
      match iv with
      | .int i _ =>
        if i < 0 then .error s!"interp: negative array index {i}"
        else
          let n := i.toNat
          match arrayGet elems n with
          | some v => return (env, .val v)
          | none => .error s!"interp: array index {i} out of bounds (size {elems.size})"
      | _ => .error "interp: array index is not an integer"
    | _ => .error "interp: array index on non-array value"

  | .cast inner targetTy => do
    let (env, v) ← evalExprVal fns env inner
    let result ← evalCast v targetTy
    return (env, .val result)

  | .ifExpr cond thenStmts elseStmts _ => do
    let (env, cv) ← evalExprVal fns env cond
    match cv with
    | .bool true =>
      let outerLen := env.length
      let (branchEnv, flow) ← evalStmts fns env thenStmts
      let restored := branchEnv.drop (branchEnv.length - outerLen)
      return (restored, flow)
    | .bool false =>
      let outerLen := env.length
      let (branchEnv, flow) ← evalStmts fns env elseStmts
      let restored := branchEnv.drop (branchEnv.length - outerLen)
      return (restored, flow)
    | _ => .error "interp: if condition is not a boolean"

  | .borrow inner _ => do
    let (env, p) ← evalBorrowTarget fns env inner false
    return (env, .val (.ref p))
  | .borrowMut inner _ => do
    let (env, p) ← evalBorrowTarget fns env inner true
    return (env, .val (.ref p))
  | .deref inner _ => do
    let (env, v) ← evalExprVal fns env inner
    match v with
    | .ref p => do
      let target ← lookupPath env p
      return (env, .val target)
    | _ => .error "interp: deref of non-ref value"

  | .fnRef _ _ => .error "interp: function references not yet supported"
  | .try_ _ _ => .error "interp: try expressions not yet supported"
  | .allocCall _ _ _ => .error "interp: alloc expressions not yet supported"
  | .whileExpr _ _ _ _ => .error "interp: while expressions not yet supported"

partial def evalExprVal (fns : List CFnDef) (env : Env) (e : CExpr) : Except String (Env × IVal) := do
  let (env, f) ← evalExpr fns env e
  match f with
  | .val v => return (env, v)
  | .ret _ => .error "interp: unexpected return in value position"
  | .brk => .error "interp: unexpected break in value position"
  | .cont => .error "interp: unexpected continue in value position"

partial def evalCallArgs (fns : List CFnDef) (env : Env) (args : List CExpr) : Except String (Env × List IVal) :=
  match args with
  | [] => .ok (env, [])
  | e :: rest => do
    let (env, v) ← evalExprVal fns env e
    let (env, vs) ← evalCallArgs fns env rest
    return (env, v :: vs)

partial def evalFields (fns : List CFnDef) (env : Env) (fields : List (String × CExpr)) :
    Except String (Env × List (String × IVal)) :=
  match fields with
  | [] => .ok (env, [])
  | (name, expr) :: rest => do
    let (env, v) ← evalExprVal fns env expr
    let (env, vs) ← evalFields fns env rest
    return (env, (name, v) :: vs)

partial def evalMatch (fns : List CFnDef) (env : Env) (scrutinee : IVal) (arms : List CMatchArm) :
    Except String (Env × Flow) :=
  match arms with
  | [] => .error "interp: no matching arm in match expression"
  | arm :: rest =>
    match arm with
    | .enumArm enumName variant bindings body =>
      match scrutinee with
      | .enum_ sEnum sVariant sFields =>
        if sEnum == enumName && sVariant == variant then do
          let outerLen := env.length
          let armEnv := bindEnumFields env bindings sFields
          let (bodyEnv, flow) ← evalStmts fns armEnv body
          let restored := bodyEnv.drop (bodyEnv.length - outerLen)
          return (restored, flow)
        else
          evalMatch fns env scrutinee rest
      | _ => evalMatch fns env scrutinee rest
    | .litArm value body => do
      let (env, litVal) ← evalExprVal fns env value
      if matchLit scrutinee litVal then do
        let outerLen := env.length
        let (bodyEnv, flow) ← evalStmts fns env body
        let restored := bodyEnv.drop (bodyEnv.length - outerLen)
        return (restored, flow)
      else
        evalMatch fns env scrutinee rest
    | .varArm binding _ body => do
      let outerLen := env.length
      let armEnv := if binding == "_" then env else envBind env binding scrutinee
      let (bodyEnv, flow) ← evalStmts fns armEnv body
      let restored := bodyEnv.drop (bodyEnv.length - outerLen)
      return (restored, flow)

partial def evalStmt (fns : List CFnDef) (env : Env) (s : CStmt) : Except String (Env × Flow) := do
  match s with
  | .letDecl name _ _ value => do
    let (env, f) ← evalExpr fns env value
    match f with
    | .val v => return (envBind env name v, .val .unit)
    | .ret v => return (env, .ret v)
    | .brk => return (env, .brk)
    | .cont => return (env, .cont)

  | .assign name value => do
    let (env, f) ← evalExpr fns env value
    match f with
    | .val v => return (envSet env name v, .val .unit)
    | .ret v => return (env, .ret v)
    | .brk => return (env, .brk)
    | .cont => return (env, .cont)

  | .return_ (some expr) _ => do
    let (env, f) ← evalExpr fns env expr
    match f with
    | .val v => return (env, .ret v)
    | other => return (env, other)

  | .return_ none _ =>
    return (env, .ret .unit)

  | .expr e => do
    let (env, f) ← evalExpr fns env e
    match f with
    | .val _ => return (env, .val .unit)
    | .ret v => return (env, .ret v)
    | .brk => return (env, .brk)
    | .cont => return (env, .cont)

  | .ifElse cond thenBody elseBody => do
    let (env, cf) ← evalExpr fns env cond
    match cf with
    | .val (.bool true) =>
      let outerLen := env.length
      let (branchEnv, flow) ← evalStmts fns env thenBody
      let restoredEnv := branchEnv.drop (branchEnv.length - outerLen)
      return (restoredEnv, flow)
    | .val (.bool false) =>
      match elseBody with
      | some body =>
        let outerLen := env.length
        let (branchEnv, flow) ← evalStmts fns env body
        let restoredEnv := branchEnv.drop (branchEnv.length - outerLen)
        return (restoredEnv, flow)
      | none => return (env, .val .unit)
    | .val _ => .error "interp: if condition is not a boolean"
    | .ret v => return (env, .ret v)
    | .brk => return (env, .brk)
    | .cont => return (env, .cont)

  | .while_ cond body _label step =>
    evalWhile fns env cond body step 10000000

  | .fieldAssign obj field value => do
    let (env, newVal) ← evalExprVal fns env value
    match obj with
    | .ident name _ =>
      match envGet env name with
      | some (.struct_ sname fields) =>
        let newFields := fields.map fun (n, v) =>
          if n == field then (n, newVal) else (n, v)
        return (envSet env name (.struct_ sname newFields), .val .unit)
      | some (.ref p) =>
        let p' := { p with steps := p.steps ++ [.field field] }
        let env' ← updatePath env p' newVal
        return (env', .val .unit)
      | _ => .error s!"interp: field assign on non-struct variable '{name}'"
    | _ => .error "interp: field assign on non-ident expression"

  | .arrayIndexAssign arr idx value => do
    let (env, newVal) ← evalExprVal fns env value
    let (env, idxVal) ← evalExprVal fns env idx
    match idxVal with
    | .int i _ =>
      if i < 0 then .error s!"interp: negative array index {i} on assignment"
      else
        match arr with
        | .ident name _ =>
          match envGet env name with
          | some (.array elems elemTy size) =>
            let n := i.toNat
            if n < elems.size then
              let newElems := elems.set! n newVal
              return (envSet env name (.array newElems elemTy size), .val .unit)
            else .error s!"interp: array index {i} out of bounds (size {elems.size})"
          | some (.ref p) =>
            let p' := { p with steps := p.steps ++ [.index i.toNat] }
            let env' ← updatePath env p' newVal
            return (env', .val .unit)
          | _ => .error s!"interp: array index assign on non-array variable '{name}'"
        | _ => .error "interp: array index assign on non-ident expression"
    | _ => .error "interp: array index is not an integer"

  | .derefAssign target value => do
    let (env, newVal) ← evalExprVal fns env value
    let (env, refVal) ← evalExprVal fns env target
    match refVal with
    | .ref p =>
      if !p.isMut then .error "interp: deref-assign through immutable ref"
      else do
        let env' ← updatePath env p newVal
        return (env', .val .unit)
    | _ => .error "interp: deref-assign on non-ref value"

  | .break_ _ _ => return (env, .brk)
  | .continue_ _ => return (env, .cont)
  | .defer _ => .error "interp: defer not yet supported"
  | .borrowIn var ref _region isMut _refTy body => do
    -- `borrow var as ref in 'region { body }` — bind `ref` to a path that
    -- captures the current frame so callee shadows don't break it. Drop
    -- the binding on exit; mutations to `var` through `ref` survive.
    let path : RefPath := { base := var, frame := env.length, steps := [], isMut := isMut }
    let outerLen := env.length
    let inner := envBind env ref (.ref path)
    let (postEnv, flow) ← evalStmts fns inner body
    let restored := postEnv.drop (postEnv.length - outerLen)
    match flow with
    | .ret v => return (restored, .ret v)
    | .brk => return (restored, .brk)
    | .cont => return (restored, .cont)
    | .val _ => return (restored, .val .unit)

partial def evalStmts (fns : List CFnDef) (env : Env) (stmts : List CStmt) : Except String (Env × Flow) :=
  match stmts with
  | [] => .ok (env, .val .unit)
  | s :: rest => do
    let (env', flow) ← evalStmt fns env s
    match flow with
    | .ret v => return (env', .ret v)
    | .brk => return (env', .brk)
    | .cont => return (env', .cont)
    | .val _ => evalStmts fns env' rest

partial def evalWhile (fns : List CFnDef) (env : Env) (cond : CExpr) (body : List CStmt) (step : List CStmt) (fuel : Nat) : Except String (Env × Flow) := do
  if fuel == 0 then .error "interp: loop exceeded maximum iterations (10000000)"
  else
    let (env, cv) ← evalExprVal fns env cond
    match cv with
    | .bool false => return (env, .val .unit)
    | .bool true =>
      let (bodyEnv, flow) ← evalStmts fns env body
      match flow with
      | .ret v => return (bodyEnv, .ret v)
      | .brk => return (bodyEnv, .val .unit)
      | .val _ =>
        -- Step is already included in body (for-loop desugaring appends it).
        -- Only run step explicitly on continue (where body was cut short).
        evalWhile fns bodyEnv cond body step (fuel - 1)
      | .cont =>
        -- Continue skips the rest of body, so run step before looping.
        let (stepEnv, _) ← evalStmts fns bodyEnv step
        evalWhile fns stepEnv cond body step (fuel - 1)
    | _ => .error "interp: while condition is not a boolean"

end -- mutual

-- ============================================================
-- Entry point
-- ============================================================

/-- Interpret a program from its validated Core modules.
    Finds and runs `main`, returns exit code. -/
def interpret (modules : List CModule) : Except String Int := do
  let fns := collectFns modules
  match fns.find? (fun f => f.name == "main") with
  | none => .error "interp: no 'main' function found"
  | some mainFn =>
    let (_, flow) ← evalStmts fns [] mainFn.body
    match flow with
    | .ret (.int n _) => return n
    | .ret _ => return 0
    | .val _ => return 0
    | _ => .error "interp: main did not return normally"

-- ============================================================
-- Display
-- ============================================================

partial def IVal.toString : IVal → String
  | .int n _ => s!"{n}"
  | .bool b => s!"{b}"
  | .struct_ name fields =>
    let fs := ", ".intercalate (fields.map fun (n, v) => s!"{n}: {v.toString}")
    name ++ " { " ++ fs ++ " }"
  | .enum_ ename variant fields =>
    if fields.isEmpty then ename ++ "::" ++ variant
    else
      let fs := ", ".intercalate (fields.map fun (n, v) => s!"{n}: {v.toString}")
      ename ++ "::" ++ variant ++ " { " ++ fs ++ " }"
  | .array elems _ _ =>
    let es := ", ".intercalate (elems.toList.map IVal.toString)
    "[" ++ es ++ "]"
  | .unit => "()"
  | .ref p =>
    let stepStr (s : RefStep) : String := match s with
      | .field n => "." ++ n
      | .index i => s!"[{i}]"
    let pfx := if p.isMut then "&mut " else "&"
    pfx ++ p.base ++ String.join (p.steps.map stepStr)

instance : ToString IVal := ⟨IVal.toString⟩

end Concrete.Interp
