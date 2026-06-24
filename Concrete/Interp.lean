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
  | string (s : String)
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

partial def collectEnums : List CModule → List CEnumDef
  | [] => []
  | m :: ms => m.enums ++ collectEnums m.submodules ++ collectEnums ms

def findFn (fns : List CFnDef) (name : String) : Option CFnDef :=
  fns.find? (fun f => f.name == name)

def findEnum (enums : List CEnumDef) (name : String) : Option CEnumDef :=
  enums.find? (fun e => e.name == name)

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

/-- Bit width of an unsigned fixed-width integer type, if any. -/
private def unsignedBitWidth : Ty → Option Nat
  | .uint => some 64
  | .u8   => some 8
  | .u16  => some 16
  | .u32  => some 32
  | _     => none

/-- Wrap an integer result to its type's width for unsigned
    fixed-width types — exactly the BitVec round-trip the proof
    model uses (`Concrete.Proof.evalBinOp`), so the interpreter
    and the proof semantics agree by construction (e.g. u32 add
    is mod 2^32, u32 `<<` truncates).  Signed / `Int` types are
    left as mathematical `Int`, matching the width-agnostic
    `add`/`sub`/`mul` of the proof model. -/
private def maskWidth (ty : Ty) (n : Int) : Int :=
  match unsignedBitWidth ty with
  | some w => Int.ofNat (BitVec.ofInt w n).toNat
  | none   => n

/-- (bit width, signed?) for each fixed-width integer type, including the
    64-bit `Int`/`Uint`. -/
private def intBitWidth : Ty → Option (Nat × Bool)
  | .i8 => some (8, true)  | .i16 => some (16, true)  | .i32 => some (32, true)  | .int  => some (64, true)
  | .u8 => some (8, false) | .u16 => some (16, false) | .u32 => some (32, false) | .uint => some (64, false)
  | _   => none

/-- True two's-complement wrap of `n` into `ty`'s width, for BOTH signed and
    unsigned — what `wrapping_add`/`wrapping_sub`/`wrapping_mul` mean and exactly
    what the compiled plain LLVM add/sub/mul produce. Unlike `maskWidth`, this
    also wraps signed types (e.g. `wrapping_add(i32::MAX, 1) == i32::MIN`). -/
private def wrapToType (ty : Ty) (n : Int) : Int :=
  match intBitWidth ty with
  | some (w, signed) => if signed then (BitVec.ofInt w n).toInt else Int.ofNat (BitVec.ofInt w n).toNat
  | none             => n

/-- Checked arithmetic (ROADMAP #10 Stage 2.3): `some n` if `n` fits `ty`'s range,
    else `none` (overflow → the interpreter traps, matching the compiled abort). -/
private def checkedToType (ty : Ty) (n : Int) : Option Int :=
  match intBitWidth ty with
  | some (w, signed) =>
    let (lo, hi) : Int × Int :=
      if signed then (-((2 : Int) ^ (w - 1)), (2 : Int) ^ (w - 1) - 1)
      else (0, (2 : Int) ^ w - 1)
    if n < lo || n > hi then none else some n
  | none => some n

/-- Clamp `n` to `ty`'s representable range — what `saturating_*` mean, matching
    the `llvm.{s,u}{add,sub}.sat` intrinsics. -/
private def saturateToType (ty : Ty) (n : Int) : Int :=
  match intBitWidth ty with
  | some (w, signed) =>
    let (lo, hi) : Int × Int :=
      if signed then (-((2 : Int) ^ (w - 1)), (2 : Int) ^ (w - 1) - 1)
      else (0, (2 : Int) ^ w - 1)
    if n < lo then lo else if n > hi then hi else n
  | none => n

def evalBinOp (op : BinOp) (lhs rhs : IVal) : Except String IVal :=
  -- Result type is the LHS (value) type; for shifts this is the
  -- shifted value's width, not the shift-count's.  maskWidth then
  -- wraps unsigned results to that width.
  match op, lhs, rhs with
  -- Ordinary `+` is CHECKED (ROADMAP #10 Stage 2.3): trap on overflow, matching
  -- the compiled abort. (`-`/`*` flip in later sub-slices.)
  | .add, .int a ty, .int b _ =>
    match checkedToType ty (a + b) with
    | some v => .ok (.int v ty)
    | none   => .error "interp: arithmetic overflow (checked +)"
  | .sub, .int a ty, .int b _ =>
    match checkedToType ty (a - b) with
    | some v => .ok (.int v ty)
    | none   => .error "interp: arithmetic overflow (checked -)"
  | .mul, .int a ty, .int b _ =>
    match checkedToType ty (a * b) with
    | some v => .ok (.int v ty)
    | none   => .error "interp: arithmetic overflow (checked *)"
  -- Explicit wrapping arithmetic: true two's-complement wrap (signed + unsigned),
  -- matching the compiled plain LLVM add/sub/mul. ROADMAP #10 Stage 2.1.
  | .wrappingAdd, .int a ty, .int b _ => .ok (.int (wrapToType ty (a + b)) ty)
  | .wrappingSub, .int a ty, .int b _ => .ok (.int (wrapToType ty (a - b)) ty)
  | .wrappingMul, .int a ty, .int b _ => .ok (.int (wrapToType ty (a * b)) ty)
  -- Explicit saturating arithmetic: clamp to the type's range, matching the
  -- llvm.{s,u}{add,sub}.sat intrinsics. ROADMAP #10 Stage 2.2.
  | .saturatingAdd, .int a ty, .int b _ => .ok (.int (saturateToType ty (a + b)) ty)
  | .saturatingSub, .int a ty, .int b _ => .ok (.int (saturateToType ty (a - b)) ty)
  | .saturatingMul, .int a ty, .int b _ => .ok (.int (saturateToType ty (a * b)) ty)
  | .div, .int _ _, .int 0 _ => .error "interp: division by zero"
  | .div, .int a ty, .int b _ => .ok (.int (maskWidth ty (a / b)) ty)
  | .mod, .int _ _, .int 0 _ => .error "interp: modulo by zero"
  | .mod, .int a ty, .int b _ => .ok (.int (maskWidth ty (a % b)) ty)
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
  | .eq, .string a, .string b => .ok (.bool (a == b))
  | .neq, .string a, .string b => .ok (.bool (a != b))
  | .bitxor, .int a ty, .int b _ => .ok (.int (maskWidth ty (intXor a b)) ty)
  | .bitand, .int a ty, .int b _ => .ok (.int (maskWidth ty (intAnd a b)) ty)
  | .bitor, .int a ty, .int b _ => .ok (.int (maskWidth ty (intOr a b)) ty)
  | .shl, .int a ty, .int b _ => .ok (.int (maskWidth ty (a * (2 ^ b.toNat))) ty)
  | .shr, .int a ty, .int b _ => .ok (.int (maskWidth ty (a / (2 ^ b.toNat))) ty)
  | _, _, _ => .error "interp: unsupported binop on given value types"

-- ============================================================
-- Unary operations
-- ============================================================

def evalUnaryOp (op : UnaryOp) (v : IVal) : Except String IVal :=
  match op, v with
  | .neg, .int n ty => .ok (.int (maskWidth ty (-n)) ty)
  | .not_, .bool b => .ok (.bool (!b))
  -- ~n at an unsigned width is `2^w - 1 - n`; maskWidth turns the
  -- mathematical `-(n+1)` into exactly that (e.g. ~0 = 0xFFFFFFFF
  -- at u32, not -1).
  | .bitnot, .int n ty => .ok (.int (maskWidth ty (-(n + 1))) ty)
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
partial def evalBorrowTarget (fns : List CFnDef) (enums : List CEnumDef) (env : Env) (e : CExpr) (isMut : Bool) :
    Except String (Env × RefPath) := do
  match e with
  | .ident name _ =>
    return (env, { base := name, frame := env.length, steps := [], isMut := isMut })
  | .fieldAccess obj fld _ => do
    let (env, p) ← evalBorrowTarget fns enums env obj isMut
    return (env, { p with steps := p.steps ++ [.field fld] })
  | .arrayIndex arr idx _ => do
    let (env, p) ← evalBorrowTarget fns enums env arr isMut
    let (env, iv) ← evalExprVal fns enums env idx
    match iv with
    | .int i _ =>
      if i < 0 then .error s!"interp: negative array index {i} in borrow target"
      else return (env, { p with steps := p.steps ++ [.index i.toNat] })
    | _ => .error "interp: array index in borrow target is not an integer"
  | .deref inner _ => do
    -- &(*r) and &mut (*r): forward to r's path, optionally widening mutability
    let (env, v) ← evalExprVal fns enums env inner
    match v with
    | .ref p => return (env, { p with isMut := p.isMut || isMut })
    | _ => .error "interp: deref of non-ref in borrow target"
  -- Borrowing a literal yields a ref to a temporary. The interpreter
  -- has no separate static-data area, so we materialize the literal
  -- under a synthetic env name and ref that. The synthetic name is
  -- keyed off the current env length so successive borrows in the
  -- same statement do not collide.
  | .strLit s => do
    let name := s!"__interp_str_lit_{env.length}"
    let env := envBind env name (.string s)
    return (env, { base := name, frame := env.length, steps := [], isMut := isMut })
  | .charLit _ => .error "interp: char literals not yet supported"
  | .floatLit _ _ => .error "interp: float literals not yet supported"
  | _ => .error "interp: unsupported borrow target shape"

partial def evalExpr (fns : List CFnDef) (enums : List CEnumDef) (env : Env) (e : CExpr) : Except String (Env × Flow) := do
  match e with
  | .intLit val ty => return (env, .val (.int val ty))
  | .boolLit val => return (env, .val (.bool val))
  | .strLit s => return (env, .val (.string s))
  | .charLit _ => .error "interp: char literals not yet supported"
  | .floatLit _ _ => .error "interp: float literals not yet supported"

  | .ident name _ =>
    match envGet env name with
    | some v => return (env, .val v)
    | none => .error s!"interp: undefined variable '{name}'"

  | .binOp op lhs rhs _ => do
    let (env, lv) ← evalExprVal fns enums env lhs
    -- Short-circuit `&&`/`||` so the RHS is not evaluated when the result is
    -- already determined — matching compiled semantics (and avoiding spurious
    -- errors like `x != 0 && (10 / x) > 0` when x == 0).
    match op, lv with
    | .and_, .bool false => return (env, .val (.bool false))
    | .or_, .bool true => return (env, .val (.bool true))
    | _, _ =>
      let (env, rv) ← evalExprVal fns enums env rhs
      let result ← evalBinOp op lv rv
      return (env, .val result)

  | .unaryOp op operand _ => do
    let (env, v) ← evalExprVal fns enums env operand
    let result ← evalUnaryOp op v
    return (env, .val result)

  | .call fnName _ args _ => do
    let (env, argVals) ← evalCallArgs fns enums env args
    match findFn fns fnName with
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
        let (postEnv, flow) ← evalStmts fns enums callEnv fdef.body
        let restored := postEnv.drop (postEnv.length - outerLen)
        match flow with
        | .ret v => return (restored, .val v)
        | .val v => return (restored, .val v)
        | _ => return (restored, .val .unit)
    | none =>
      -- Builtins: a small allowlist of intrinsics whose semantics fit the
      -- value-passing model. I/O (print, println, print_string, ...) is
      -- intentionally left out — the interpreter cannot reproduce stdout
      -- side effects, so any program that calls them is PENDING.
      match fnName, argVals with
      | "string_length", [arg] => do
        let s ← (match arg with
          | .string s => .ok s
          | .ref p => do
            let v ← lookupPath env p
            match v with
            | .string s => .ok s
            | _ => .error "interp: string_length: ref does not point to a string"
          | _ => .error "interp: string_length: argument is not a string")
        return (env, .val (.int (Int.ofNat s.length) .int))
      | "drop_string", [_] =>
        -- Linear-discipline no-op: ownership transfer + heap free in the
        -- compiled binary; the interpreter has no heap, so the consume
        -- itself is enough. Return unit.
        return (env, .val .unit)
      | "print", _ | "println", _ | "print_string", _ | "print_int", _
      | "print_char", _ | "print_bool", _ =>
        -- I/O intrinsics produce stdout side effects in the compiled
        -- binary. The interpreter cannot reproduce those — its only
        -- output is the `fn main() -> Int` return value. Programs that
        -- call these are PENDING for the harness, not silent passes.
        .error s!"interp: print/IO intrinsic '{fnName}' not yet supported"
      | _, _ => .error s!"interp: undefined function '{fnName}'"

  | .structLit name _ fields _ => do
    let (env, fieldVals) ← evalFields fns enums env fields
    return (env, .val (.struct_ name fieldVals))

  | .fieldAccess obj field _ => do
    let (env, v) ← evalExprVal fns enums env obj
    let target ← autoDeref env v
    match target with
    | .struct_ _ fields =>
      match fields.find? (fun (n, _) => n == field) with
      | some (_, fv) => return (env, .val fv)
      | none => .error s!"interp: field '{field}' not found in struct"
    | _ => .error "interp: field access on non-struct value"

  | .enumLit enumName variant _ fields _ => do
    let (env, fieldVals) ← evalFields fns enums env fields
    return (env, .val (.enum_ enumName variant fieldVals))

  | .match_ scrutinee arms _ => do
    let (env, sv) ← evalExprVal fns enums env scrutinee
    -- Match on `&T`: match the pointee, not the reference. Lowering derefs a
    -- reference scrutinee once before matching (the E0715 fix); the interpreter
    -- must agree, or a var/guard arm binds the ref instead of the value (e.g.
    -- `match p { n if n > 5 => .. }` on `p : &Int` would compare a ref to an int).
    let sv ← autoDeref env sv
    evalMatch fns enums env sv arms

  | .arrayLit elems ty => do
    let (env, vals) ← evalCallArgs fns enums env elems
    let elemTy := match ty with
      | .array t _ => t
      | _ => .unit
    return (env, .val (.array vals.toArray elemTy vals.length))

  | .arrayIndex arr idx _ => do
    let (env, av) ← evalExprVal fns enums env arr
    let arrayVal ← autoDeref env av
    match arrayVal with
    | .array elems _ _ => do
      let (env, iv) ← evalExprVal fns enums env idx
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
    let (env, v) ← evalExprVal fns enums env inner
    let result ← evalCast v targetTy
    return (env, .val result)

  | .ifExpr cond thenStmts elseStmts _ => do
    let (env, cv) ← evalExprVal fns enums env cond
    match cv with
    | .bool true =>
      let outerLen := env.length
      let (branchEnv, flow) ← evalStmts fns enums env thenStmts
      let restored := branchEnv.drop (branchEnv.length - outerLen)
      return (restored, flow)
    | .bool false =>
      let outerLen := env.length
      let (branchEnv, flow) ← evalStmts fns enums env elseStmts
      let restored := branchEnv.drop (branchEnv.length - outerLen)
      return (restored, flow)
    | _ => .error "interp: if condition is not a boolean"

  | .borrow inner _ => do
    let (env, p) ← evalBorrowTarget fns enums env inner false
    return (env, .val (.ref p))
  | .borrowMut inner _ => do
    let (env, p) ← evalBorrowTarget fns enums env inner true
    return (env, .val (.ref p))
  | .deref inner _ => do
    let (env, v) ← evalExprVal fns enums env inner
    match v with
    | .ref p => do
      let target ← lookupPath env p
      return (env, .val target)
    | _ => .error "interp: deref of non-ref value"

  | .fnRef _ _ => .error "interp: function references not yet supported"
  | .try_ inner _ => do
    -- Mirrors Concrete/Lower.lean's `.try_` lowering: if `inner` is the
    -- enum's first variant (tag 0 — by convention `Ok` / `Some`), unwrap
    -- the single payload field; otherwise return the whole enum as an
    -- early function return.
    let (env, v) ← evalExprVal fns enums env inner
    match v with
    | .enum_ enumName variant fields =>
      match findEnum enums enumName with
      | none => .error s!"interp: try: enum '{enumName}' not found"
      | some ed =>
        match ed.variants with
        | [] => .error s!"interp: try: enum '{enumName}' has no variants"
        | (firstVariant, _) :: _ =>
          if variant == firstVariant then
            -- Success: unwrap the single payload field.
            match fields with
            | (_, payload) :: _ => return (env, .val payload)
            | [] => .error s!"interp: try: success variant '{variant}' has no payload"
          else
            -- Failure: return the whole enum from the current function.
            return (env, .ret (.enum_ enumName variant fields))
    | _ => .error "interp: try: inner expression did not evaluate to an enum"
  | .allocCall _ _ _ => .error "interp: alloc expressions not yet supported"
  | .whileExpr _ _ _ _ => .error "interp: while expressions not yet supported"

partial def evalExprVal (fns : List CFnDef) (enums : List CEnumDef) (env : Env) (e : CExpr) : Except String (Env × IVal) := do
  let (env, f) ← evalExpr fns enums env e
  match f with
  | .val v => return (env, v)
  | .ret _ => .error "interp: unexpected return in value position"
  | .brk => .error "interp: unexpected break in value position"
  | .cont => .error "interp: unexpected continue in value position"

partial def evalCallArgs (fns : List CFnDef) (enums : List CEnumDef) (env : Env) (args : List CExpr) : Except String (Env × List IVal) :=
  match args with
  | [] => .ok (env, [])
  | e :: rest => do
    let (env, v) ← evalExprVal fns enums env e
    let (env, vs) ← evalCallArgs fns enums env rest
    return (env, v :: vs)

partial def evalFields (fns : List CFnDef) (enums : List CEnumDef) (env : Env) (fields : List (String × CExpr)) :
    Except String (Env × List (String × IVal)) :=
  match fields with
  | [] => .ok (env, [])
  | (name, expr) :: rest => do
    let (env, v) ← evalExprVal fns enums env expr
    let (env, vs) ← evalFields fns enums env rest
    return (env, (name, v) :: vs)

partial def evalMatch (fns : List CFnDef) (enums : List CEnumDef) (env : Env) (scrutinee : IVal) (arms : List CMatchArm) :
    Except String (Env × Flow) :=
  match arms with
  | [] => .error "interp: no matching arm in match expression"
  | arm :: rest =>
    -- A guard (if present) is evaluated in the arm's bound env; a false guard
    -- falls through to the next arm.
    let guardOk := fun (armEnv : Env) (guard : Option CExpr) =>
      match guard with
      | none => Except.ok true
      | some g => do
        let (_, gv) ← evalExprVal fns enums armEnv g
        match gv with
        | .bool b => Except.ok b
        | _ => Except.error "interp: match guard is not a bool"
    match arm with
    | .enumArm enumName variant bindings guard body =>
      match scrutinee with
      | .enum_ sEnum sVariant sFields =>
        if sEnum == enumName && sVariant == variant then do
          let outerLen := env.length
          let armEnv := bindEnumFields env bindings sFields
          if ← guardOk armEnv guard then
            let (bodyEnv, flow) ← evalStmts fns enums armEnv body
            let restored := bodyEnv.drop (bodyEnv.length - outerLen)
            return (restored, flow)
          else
            evalMatch fns enums env scrutinee rest
        else
          evalMatch fns enums env scrutinee rest
      | _ => evalMatch fns enums env scrutinee rest
    | .litArm value guard body => do
      let (env, litVal) ← evalExprVal fns enums env value
      if matchLit scrutinee litVal then do
        let outerLen := env.length
        if ← guardOk env guard then
          let (bodyEnv, flow) ← evalStmts fns enums env body
          let restored := bodyEnv.drop (bodyEnv.length - outerLen)
          return (restored, flow)
        else
          evalMatch fns enums env scrutinee rest
      else
        evalMatch fns enums env scrutinee rest
    | .varArm binding _ guard body => do
      let outerLen := env.length
      let armEnv := if binding == "_" then env else envBind env binding scrutinee
      if ← guardOk armEnv guard then
        let (bodyEnv, flow) ← evalStmts fns enums armEnv body
        let restored := bodyEnv.drop (bodyEnv.length - outerLen)
        return (restored, flow)
      else
        evalMatch fns enums env scrutinee rest
    | .rangeArm lo hi inclusive guard body => do
      let (env, loVal) ← evalExprVal fns enums env lo
      let (env, hiVal) ← evalExprVal fns enums env hi
      let inRange := match scrutinee, loVal, hiVal with
        | .int s _, .int l _, .int h _ => l <= s && (if inclusive then s <= h else s < h)
        | _, _, _ => false
      if inRange then do
        let outerLen := env.length
        if ← guardOk env guard then
          let (bodyEnv, flow) ← evalStmts fns enums env body
          let restored := bodyEnv.drop (bodyEnv.length - outerLen)
          return (restored, flow)
        else
          evalMatch fns enums env scrutinee rest
      else
        evalMatch fns enums env scrutinee rest

/-- Resolve a place expression to its root variable and the `RefStep` path
    reaching it, evaluating any array-index subexpressions. Mirrors the
    compiler's `storeToPlace` so the interpreter handles nested assignment
    targets (`o.i.v`, `a[i].x`, `m[i][j]`) instead of only single-level ones. -/
partial def resolvePlaceSteps (fns : List CFnDef) (enums : List CEnumDef) (env : Env) (place : CExpr)
    : Except String (Env × String × List RefStep) := do
  match place with
  | .ident name _ => return (env, name, [])
  | .fieldAccess obj field _ =>
    let (env, root, steps) ← resolvePlaceSteps fns enums env obj
    return (env, root, steps ++ [.field field])
  | .arrayIndex arr idx _ =>
    let (env, root, steps) ← resolvePlaceSteps fns enums env arr
    let (env, iv) ← evalExprVal fns enums env idx
    match iv with
    | .int i _ =>
      if i < 0 then .error s!"interp: negative array index {i} in place"
      else return (env, root, steps ++ [.index i.toNat])
    | _ => .error "interp: array index is not an integer (place)"
  | _ => .error "interp: unsupported place expression"

partial def evalStmt (fns : List CFnDef) (enums : List CEnumDef) (env : Env) (s : CStmt) : Except String (Env × Flow) := do
  match s with
  | .letDecl name _ _ value => do
    let (env, f) ← evalExpr fns enums env value
    match f with
    | .val v => return (envBind env name v, .val .unit)
    | .ret v => return (env, .ret v)
    | .brk => return (env, .brk)
    | .cont => return (env, .cont)

  | .assign name value => do
    let (env, f) ← evalExpr fns enums env value
    match f with
    | .val v => return (envSet env name v, .val .unit)
    | .ret v => return (env, .ret v)
    | .brk => return (env, .brk)
    | .cont => return (env, .cont)

  | .return_ (some expr) _ => do
    let (env, f) ← evalExpr fns enums env expr
    match f with
    | .val v => return (env, .ret v)
    | other => return (env, other)

  | .return_ none _ =>
    return (env, .ret .unit)

  | .expr e _ => do
    let (env, f) ← evalExpr fns enums env e
    match f with
    | .val _ => return (env, .val .unit)
    | .ret v => return (env, .ret v)
    | .brk => return (env, .brk)
    | .cont => return (env, .cont)

  | .ifElse cond thenBody elseBody => do
    let (env, cf) ← evalExpr fns enums env cond
    match cf with
    | .val (.bool true) =>
      let outerLen := env.length
      let (branchEnv, flow) ← evalStmts fns enums env thenBody
      let restoredEnv := branchEnv.drop (branchEnv.length - outerLen)
      return (restoredEnv, flow)
    | .val (.bool false) =>
      match elseBody with
      | some body =>
        let outerLen := env.length
        let (branchEnv, flow) ← evalStmts fns enums env body
        let restoredEnv := branchEnv.drop (branchEnv.length - outerLen)
        return (restoredEnv, flow)
      | none => return (env, .val .unit)
    | .val _ => .error "interp: if condition is not a boolean"
    | .ret v => return (env, .ret v)
    | .brk => return (env, .brk)
    | .cont => return (env, .cont)

  | .while_ cond body _label step =>
    evalWhile fns enums env cond body step 10000000

  | .fieldAssign obj field value => do
    let (env, newVal) ← evalExprVal fns enums env value
    -- Resolve the (possibly nested) place `obj`, append the final field, and
    -- write in place — handles `o.i.v`, `a[i].x`, etc., not just `o.f`.
    let (env, root, steps) ← resolvePlaceSteps fns enums env obj
    let finalSteps := steps ++ [.field field]
    match envGet env root with
    | some (.ref p) =>
      let env' ← updatePath env { p with steps := p.steps ++ finalSteps } newVal
      return (env', .val .unit)
    | some baseVal =>
      let newBase ← updateAt baseVal finalSteps newVal
      return (envSet env root newBase, .val .unit)
    | none => .error s!"interp: field-assign place root '{root}' not in env"

  | .arrayIndexAssign arr idx value => do
    let (env, newVal) ← evalExprVal fns enums env value
    let (env, root, steps) ← resolvePlaceSteps fns enums env arr
    let (env, idxVal) ← evalExprVal fns enums env idx
    match idxVal with
    | .int i _ =>
      if i < 0 then .error s!"interp: negative array index {i} on assignment"
      else
        let finalSteps := steps ++ [.index i.toNat]
        match envGet env root with
        | some (.ref p) =>
          let env' ← updatePath env { p with steps := p.steps ++ finalSteps } newVal
          return (env', .val .unit)
        | some baseVal =>
          let newBase ← updateAt baseVal finalSteps newVal
          return (envSet env root newBase, .val .unit)
        | none => .error s!"interp: array-assign place root '{root}' not in env"
    | _ => .error "interp: array index is not an integer"

  | .derefAssign target value => do
    let (env, newVal) ← evalExprVal fns enums env value
    let (env, refVal) ← evalExprVal fns enums env target
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
    let (postEnv, flow) ← evalStmts fns enums inner body
    let restored := postEnv.drop (postEnv.length - outerLen)
    match flow with
    | .ret v => return (restored, .ret v)
    | .brk => return (restored, .brk)
    | .cont => return (restored, .cont)
    | .val _ => return (restored, .val .unit)

partial def evalStmts (fns : List CFnDef) (enums : List CEnumDef) (env : Env) (stmts : List CStmt) : Except String (Env × Flow) :=
  match stmts with
  | [] => .ok (env, .val .unit)
  | s :: rest => do
    let (env', flow) ← evalStmt fns enums env s
    match flow with
    | .ret v => return (env', .ret v)
    | .brk => return (env', .brk)
    | .cont => return (env', .cont)
    | .val _ => evalStmts fns enums env' rest

partial def evalWhile (fns : List CFnDef) (enums : List CEnumDef) (env : Env) (cond : CExpr) (body : List CStmt) (step : List CStmt) (fuel : Nat) : Except String (Env × Flow) := do
  if fuel == 0 then .error "interp: loop exceeded maximum iterations (10000000)"
  else
    let (env, cv) ← evalExprVal fns enums env cond
    match cv with
    | .bool false => return (env, .val .unit)
    | .bool true =>
      let (bodyEnv, flow) ← evalStmts fns enums env body
      match flow with
      | .ret v => return (bodyEnv, .ret v)
      | .brk => return (bodyEnv, .val .unit)
      | .val _ =>
        -- Step is already included in body (for-loop desugaring appends it).
        -- Only run step explicitly on continue (where body was cut short).
        evalWhile fns enums bodyEnv cond body step (fuel - 1)
      | .cont =>
        -- Continue skips the rest of body, so run step before looping.
        let (stepEnv, _) ← evalStmts fns enums bodyEnv step
        evalWhile fns enums stepEnv cond body step (fuel - 1)
    | _ => .error "interp: while condition is not a boolean"

end -- mutual

-- ============================================================
-- Entry point
-- ============================================================

/-- Interpret a program from its validated Core modules.
    Finds and runs `main`, returns exit code. -/
def interpret (modules : List CModule) : Except String Int := do
  let fns := collectFns modules
  let enums := collectEnums modules
  match fns.find? (fun f => f.name == "main") with
  | none => .error "interp: no 'main' function found"
  | some mainFn =>
    let (_, flow) ← evalStmts fns enums [] mainFn.body
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
  | .string s => "\"" ++ s ++ "\""
  | .ref p =>
    let stepStr (s : RefStep) : String := match s with
      | .field n => "." ++ n
      | .index i => s!"[{i}]"
    let pfx := if p.isMut then "&mut " else "&"
    pfx ++ p.base ++ String.join (p.steps.map stepStr)

instance : ToString IVal := ⟨IVal.toString⟩

end Concrete.Interp
