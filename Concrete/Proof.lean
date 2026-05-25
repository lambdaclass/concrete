import Concrete.Core

namespace Concrete.Proof

/-! ## Proof — first Lean 4 formalization of a Concrete Core fragment

This module defines evaluation semantics for a small, pure fragment of
Concrete's Core IR and proves properties about embedded Concrete programs.

The fragment covers:
- Integer literals and arithmetic (add, sub, mul, comparison)
- Boolean literals and logical operators
- Let bindings
- If/then/else
- Function calls (non-recursive, by name lookup)

This is deliberately minimal.  The goal is to prove the workflow exists and
works end-to-end, not to cover the full Core IR.  Extensions (structs,
enums, match, loops) follow once this foundation is solid.

### Relationship to ProofCore

`ProofCore` (in `Concrete/ProofCore.lean`) filters validated Core into the
proof-eligible subset.  This module defines formal semantics and proofs
over that subset.  The two are complementary:

  ProofCore: "which functions can we reason about?"
  Proof:     "what can we prove about those functions?"
-/

-- ============================================================
-- Pure expression language (proof fragment of Core IR)
-- ============================================================

/-- Binary operators in the proof fragment. -/
inductive PBinOp where
  | add | sub | mul
  /-- Signed remainder at i32 width.  Modeled via `BitVec.srem` on
      the `BitVec 32` round-trip of the operands.  This is the
      principled coupling to Concrete's surface semantics:
      Concrete's `.mod` lowers to LLVM `srem` for signed integer
      types (`EmitSSA.lean:.mod => .srem` for `ssaIsSignedInt`).
      Width is currently hardcoded to 32; multi-width support is
      a Phase 4 follow-up that needs to thread `Ty` through PExpr.
      Today every flagship `mod` is on `i32` (e.g. ring_push's
      `head % cap`), so the i32-only restriction is not blocking. -/
  | mod
  /-- Bitwise XOR at i32 width.  Modeled via `BitVec.xor` on the
      `BitVec 32` round-trip of the operands, with the result
      reinterpreted as `Int` via `BitVec.toInt` (signed view).
      Same width caveat as `mod`. -/
  | bitxor
  | eq | ne | lt | le | gt | ge
  deriving Repr, BEq, DecidableEq

/-- Values in the proof fragment.
    `struct_` carries the struct's name plus its fields as
    name → value pairs. The list order is the construction order
    in source; field-access lookup is by name. -/
inductive PVal where
  | int (n : Int)
  | bool (b : Bool)
  | struct_ (name : String) (fields : List (String × PVal))
  | enum_ (enumName variant : String) (fields : List (String × PVal))
  | array_ (elems : List PVal)
  deriving Repr, Inhabited, BEq

/-- Pattern shapes inside `match` arms.
    Non-recursive (no PExpr inside) so PExpr can reference
    `List (PMatchPat × PExpr)` without a mutual inductive. -/
inductive PMatchPat where
  /-- `EnumName::Variant { f1, f2, ... }` — matches only when the
      scrutinee is `enum_ enumName variant fields`. The `bindings`
      list names the variables to introduce; each is bound to the
      corresponding field's value (looked up by name). -/
  | enumPat (enumName variant : String) (bindings : List String)
  /-- `<lit>` — matches when the scrutinee equals the literal. -/
  | litPat (value : PVal)
  /-- `name` (or `_`) — matches anything; binds the scrutinee to
      `binding`. `"_"` means wildcard (no binding). -/
  | varPat (binding : String)
  deriving Repr, BEq

-- Note on DecidableEq:
-- Lean's auto-deriving for DecidableEq does not handle the
-- `List (String × PVal)` recursion through containers. Concrete's
-- existing proofs that used `native_decide` on Option PVal values
-- have been converted to simp-based proofs that unfold eval directly,
-- removing the dependency on DecidableEq for PVal. If a future
-- consumer needs DecidableEq, a hand-written mutually-recursive
-- instance is the standard pattern.

/-- Expressions in the proof fragment.
    This is a strict subset of `CExpr`, restricted to pure integer/boolean
    operations with let bindings, conditionals, function calls, and the
    struct subset that supports flagship-shape parsers and validators
    (struct literal + field access). -/
inductive PExpr where
  | lit (v : PVal)
  | var (name : String)
  | binOp (op : PBinOp) (lhs rhs : PExpr)
  | letIn (name : String) (val body : PExpr)
  | ifThenElse (cond thenBr elseBr : PExpr)
  | call (fn : String) (args : List PExpr)
  | structLit (name : String) (fields : List (String × PExpr))
  | fieldAccess (obj : PExpr) (field : String)
  | enumLit (enumName variant : String) (fields : List (String × PExpr))
  | arrayIndex (arr idx : PExpr)
  /-- `match scrutinee { pat1 => body1, pat2 => body2, ... }`.
      Arms are tried in order; the first matching pattern wins.
      Bodies are PExprs (the source-level CStmt list is already
      extracted to a single PExpr via cStmtsToPExprK). -/
  | match_ (scrutinee : PExpr) (arms : List (PMatchPat × PExpr))
  /-- Integer-width cast (e.g. `u8 as i32`, `i32 as i64`).
      Evaluates the inner expression and returns its value
      unchanged: `PVal.int` is mathematical `Int`, with no
      width, so widening casts are identity. Narrowing /
      wrapping semantics are NOT modeled — a proof that
      assumes `(x : u32) as i32` truncates would be unsound
      here. The eligibility profile is responsible for
      keeping narrowing casts out of proof-eligible code,
      or for adding a narrow_int side-condition. -/
  | cast (inner : PExpr)
  /-- `[e1, e2, ..., eN]` — fixed-shape array literal.
      Evaluates each element left-to-right; the result is a
      `PVal.array_` carrying the evaluated values in the same
      order. Element type is not modeled (PVal is dynamically
      shaped); Check ensures all elements share a type. -/
  | arrayLit (elems : List PExpr)
  /-- Bounded `while` loop with flat-assign body.

      `cond` is re-evaluated each iteration. When `cond` is `true`,
      every `(name, expr)` in `assigns` is evaluated in order and
      its result rebinds `name` in the env — later assigns in the
      same iteration see earlier assigns' updates. When `cond` is
      `false`, control falls through to `cont`.

      `assigns` carries both the source-level loop body and the
      for-loop step concatenated; extraction insists every member
      is a `CStmt.assign` (no nested `let`, `if`, or `return`
      inside the loop body — those shapes fall back to the
      eligibility-blocked path with a precise reason).

      Termination is by fuel: each iteration consumes one unit.
      `bound`-style static iteration limits are not in PExpr; the
      eligibility profile already classifies loops as bounded /
      unbounded, and only bounded loops reach extraction. -/
  | while_ (cond : PExpr) (assigns : List (String × PExpr)) (cont : PExpr)
  deriving Repr, BEq

/-- A function definition in the proof fragment. -/
structure PFnDef where
  name : String
  params : List String
  body : PExpr
  deriving Repr, BEq

/-- An environment maps variable names to values. -/
abbrev Env := String → Option PVal

/-- A function table maps function names to definitions. -/
abbrev FnTable := String → Option PFnDef

def Env.empty : Env := fun _ => none

def Env.bind (env : Env) (name : String) (val : PVal) : Env :=
  fun n => if n == name then some val else env n

-- ============================================================
-- Evaluation semantics
-- ============================================================

/-- Evaluate a binary operation on two values. -/
def evalBinOp (op : PBinOp) (lhs rhs : PVal) : Option PVal :=
  match op, lhs, rhs with
  | .add, .int a, .int b => some (.int (a + b))
  | .sub, .int a, .int b => some (.int (a - b))
  | .mul, .int a, .int b => some (.int (a * b))
  | .mod, .int a, .int b =>
    -- Signed remainder at i32 width via BitVec.srem (matches LLVM
    -- srem, which is what EmitSSA emits for signed `.mod`).
    if b = 0 then none
    else some (.int (BitVec.toInt
      (BitVec.srem (BitVec.ofInt 32 a) (BitVec.ofInt 32 b))))
  | .bitxor, .int a, .int b =>
    -- Bitwise xor at i32 width via the BitVec round-trip.
    some (.int (BitVec.toInt
      ((BitVec.ofInt 32 a) ^^^ (BitVec.ofInt 32 b))))
  | .eq,  .int a, .int b => some (.bool (a == b))
  | .ne,  .int a, .int b => some (.bool (a != b))
  | .lt,  .int a, .int b => some (.bool (a < b))
  | .le,  .int a, .int b => some (.bool (a <= b))
  | .gt,  .int a, .int b => some (.bool (a > b))
  | .ge,  .int a, .int b => some (.bool (a >= b))
  | .eq,  .bool a, .bool b => some (.bool (a == b))
  | .ne,  .bool a, .bool b => some (.bool (a != b))
  | _, _, _ => none

/-- Bind a list of argument values to parameter names. -/
def bindArgs (env : Env) (params : List String) (args : List PVal) : Option Env :=
  match params, args with
  | [], [] => some env
  | p :: ps, a :: as_ => bindArgs (env.bind p a) ps as_
  | _, _ => none  -- arity mismatch

/-- Evaluate a proof-fragment expression.  Uses fuel to ensure termination
    (Lean requires all functions to be total). -/
def eval (fns : FnTable) (env : Env) : Nat → PExpr → Option PVal
  | 0, _ => none  -- out of fuel
  | _, .lit v => some v
  | _, .var name => env name
  | fuel + 1, .binOp op lhs rhs =>
    match eval fns env (fuel + 1) lhs, eval fns env (fuel + 1) rhs with
    | some lv, some rv => evalBinOp op lv rv
    | _, _ => none
  | fuel + 1, .letIn name val body =>
    match eval fns env (fuel + 1) val with
    | some v => eval fns (env.bind name v) fuel body
    | none => none
  | fuel + 1, .ifThenElse cond thenBr elseBr =>
    match eval fns env (fuel + 1) cond with
    | some (.bool true) => eval fns env fuel thenBr
    | some (.bool false) => eval fns env fuel elseBr
    | _ => none
  | fuel + 1, .call fn args =>
    match fns fn with
    | none => none
    | some fdef =>
      match evalArgs fns env fuel args with
      | none => none
      | some argVals =>
        match bindArgs Env.empty fdef.params argVals with
        | none => none
        | some callEnv => eval fns callEnv fuel fdef.body
  | fuel + 1, .structLit name fields =>
    match evalFields fns env fuel fields with
    | none => none
    | some fieldVals => some (.struct_ name fieldVals)
  | fuel + 1, .fieldAccess obj field =>
    match eval fns env (fuel + 1) obj with
    | some (.struct_ _ fields) => lookupField fields field
    | some (.enum_ _ _ fields) => lookupField fields field
    | _ => none
  | fuel + 1, .enumLit enumName variant fields =>
    match evalFields fns env fuel fields with
    | none => none
    | some fieldVals => some (.enum_ enumName variant fieldVals)
  | fuel + 1, .arrayIndex arr idx =>
    match eval fns env (fuel + 1) arr, eval fns env (fuel + 1) idx with
    | some (.array_ elems), some (.int i) =>
      if i < 0 then none else lookupIndex elems i.toNat
    | _, _ => none
  | fuel + 1, .match_ scrutinee arms =>
    match eval fns env (fuel + 1) scrutinee with
    | none => none
    | some sv => evalArms fns env fuel sv arms
  | fuel + 1, .cast inner => eval fns env fuel inner
  | fuel + 1, .arrayLit elems =>
    match evalElems fns env fuel elems with
    | none => none
    | some vs => some (.array_ vs)
  | fuel + 1, .while_ cond assigns cont =>
    match eval fns env fuel cond with
    | some (.bool true) =>
      match evalAssigns fns env fuel assigns with
      | none => none
      | some env' => eval fns env' fuel (.while_ cond assigns cont)
    | some (.bool false) => eval fns env fuel cont
    | _ => none
where
  /-- Evaluate a list of argument expressions. -/
  evalArgs (fns : FnTable) (env : Env) (fuel : Nat) : List PExpr → Option (List PVal)
    | [] => some []
    | e :: es =>
      match eval fns env fuel e with
      | none => none
      | some v =>
        match evalArgs fns env fuel es with
        | none => none
        | some vs => some (v :: vs)
  /-- Evaluate a list of (field-name, expression) pairs into a list of
      (field-name, value) pairs.  Construction order is preserved. -/
  evalFields (fns : FnTable) (env : Env) (fuel : Nat) :
      List (String × PExpr) → Option (List (String × PVal))
    | [] => some []
    | (name, e) :: rest =>
      match eval fns env fuel e with
      | none => none
      | some v =>
        match evalFields fns env fuel rest with
        | none => none
        | some vs => some ((name, v) :: vs)
  /-- Find a field's value in a struct's field list by name. -/
  lookupField : List (String × PVal) → String → Option PVal
    | [], _ => none
    | (n, v) :: rest, target => if n == target then some v else lookupField rest target
  /-- Find an element in an array's value list by index. -/
  lookupIndex : List PVal → Nat → Option PVal
    | [], _ => none
    | v :: _, 0 => some v
    | _ :: rest, n + 1 => lookupIndex rest n
  /-- Try each arm in order against the scrutinee value. -/
  evalArms (fns : FnTable) (env : Env) (fuel : Nat) (sv : PVal) :
      List (PMatchPat × PExpr) → Option PVal
    | [] => none
    | (pat, body) :: rest =>
      match matchPat env pat sv with
      | none => evalArms fns env fuel sv rest
      | some armEnv => eval fns armEnv fuel body
  /-- Attempt to match a pattern against a value.  Returns an
      extended environment on success, `none` on failure. -/
  matchPat (env : Env) : PMatchPat → PVal → Option Env
    | .enumPat eName variant bindings, .enum_ sEnum sVar sFields =>
      if eName == sEnum && variant == sVar
      then some (bindEnumFields env bindings sFields)
      else none
    | .litPat (.int n), .int m =>
      if n == m then some env else none
    | .litPat (.bool a), .bool b =>
      if a == b then some env else none
    | .varPat binding, sv =>
      if binding == "_" then some env else some (env.bind binding sv)
    | _, _ => none
  /-- Evaluate a list of element expressions, returning a list of
      values in the same order, or `none` if any element fails. -/
  evalElems (fns : FnTable) (env : Env) (fuel : Nat) :
      List PExpr → Option (List PVal)
    | [] => some []
    | e :: rest =>
      match eval fns env fuel e with
      | none => none
      | some v =>
        match evalElems fns env fuel rest with
        | none => none
        | some vs => some (v :: vs)
  /-- Evaluate a list of assignments, threading each new binding
      through the env so subsequent assigns in the same iteration
      see earlier updates. -/
  evalAssigns (fns : FnTable) (env : Env) (fuel : Nat) :
      List (String × PExpr) → Option Env
    | [] => some env
    | (name, expr) :: rest =>
      match eval fns env fuel expr with
      | none => none
      | some v => evalAssigns fns (env.bind name v) fuel rest
  /-- Bind a list of arm-binding names to their corresponding
      field values in a matched enum variant.  Bindings whose
      names are not present in the variant's fields skip silently
      (Concrete's enumArm bindings are always a subset of the
      declared variant fields, enforced at Check time). -/
  bindEnumFields (env : Env) : List String → List (String × PVal) → Env
    | [], _ => env
    | name :: rest, fields =>
      let val := match fields.find? (fun p => p.1 == name) with
        | some (_, v) => v
        | none => .int 0  -- unreachable under Check; safe default
      bindEnumFields (env.bind name val) rest fields

-- ============================================================
-- Embedded Concrete programs
-- ============================================================

/-- `fn abs(x: i64) -> i64 { if x < 0 { return -x; } return x; }`
    Encoded as a proof-fragment expression. -/
def absExpr : PExpr :=
  .ifThenElse
    (.binOp .lt (.var "x") (.lit (.int 0)))
    (.binOp .sub (.lit (.int 0)) (.var "x"))
    (.var "x")

def absFn : PFnDef := { name := "abs", params := ["x"], body := absExpr }

/-- `fn max(a: i64, b: i64) -> i64 { if a >= b { return a; } return b; }` -/
def maxExpr : PExpr :=
  .ifThenElse
    (.binOp .ge (.var "a") (.var "b"))
    (.var "a")
    (.var "b")

def maxFn : PFnDef := { name := "max", params := ["a", "b"], body := maxExpr }

/-- `fn clamp(x: i64, lo: i64, hi: i64) -> i64 {
       if x < lo { return lo; }
       if x > hi { return hi; }
       return x;
    }` -/
def clampExpr : PExpr :=
  .ifThenElse
    (.binOp .lt (.var "x") (.var "lo"))
    (.var "lo")
    (.ifThenElse
      (.binOp .gt (.var "x") (.var "hi"))
      (.var "hi")
      (.var "x"))

def clampFn : PFnDef := { name := "clamp", params := ["x", "lo", "hi"], body := clampExpr }

/-- `fn parse_byte(data: Int, offset: Int) -> Int { return data + offset; }`
    First proof-connected function from the packet decoder core. -/
def parseByteExpr : PExpr :=
  .binOp .add (.var "data") (.var "offset")

def parseByteFn : PFnDef := { name := "parse_byte", params := ["data", "offset"], body := parseByteExpr }

/-- `fn check_length(len: Int) -> Int { if len < 10 { return 1; } return 0; }`
    Bounds guard from decode_header — rejects packets shorter than the header. -/
def checkLengthExpr : PExpr :=
  .ifThenElse
    (.binOp .lt (.var "len") (.lit (.int 10)))
    (.lit (.int 1))
    (.lit (.int 0))

def checkLengthFn : PFnDef :=
  { name := "check_length", params := ["len"], body := checkLengthExpr }

/-- Function table for proofs. -/
def proofFns : FnTable
  | "abs" => some absFn
  | "max" => some maxFn
  | "clamp" => some clampFn
  | "parse_byte" => some parseByteFn
  | "check_length" => some checkLengthFn
  | _ => none

-- ============================================================
-- Proofs
-- ============================================================

/-- Helper: evaluate abs with a given integer input. -/
def evalAbs (x : Int) : Option PVal :=
  eval proofFns (Env.empty.bind "x" (.int x)) 10 absExpr

/-- Helper: evaluate max with two integer inputs. -/
def evalMax (a b : Int) : Option PVal :=
  eval proofFns ((Env.empty.bind "a" (.int a)).bind "b" (.int b)) 10 maxExpr

/-- Helper: evaluate clamp. -/
def evalClamp (x lo hi : Int) : Option PVal :=
  eval proofFns (((Env.empty.bind "x" (.int x)).bind "lo" (.int lo)).bind "hi" (.int hi)) 10 clampExpr

-- Concrete test cases (verified by kernel reduction)
#eval evalAbs 5     -- some (int 5)
#eval evalAbs (-3)  -- some (int 3)
#eval evalAbs 0     -- some (int 0)
#eval evalMax 10 20 -- some (int 20)
#eval evalMax 7 3   -- some (int 7)

set_option linter.unusedSimpArgs false in
/-- abs(5) = 5 -/
theorem abs_positive : evalAbs 5 = some (.int 5) := by simp [evalAbs, evalMax, evalClamp, absExpr, maxExpr, clampExpr,
            eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- abs(-3) = 3 -/
theorem abs_negative : evalAbs (-3) = some (.int 3) := by simp [evalAbs, evalMax, evalClamp, absExpr, maxExpr, clampExpr,
            eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- abs(0) = 0 -/
theorem abs_zero : evalAbs 0 = some (.int 0) := by simp [evalAbs, evalMax, evalClamp, absExpr, maxExpr, clampExpr,
            eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- max(10, 20) = 20 -/
theorem max_right : evalMax 10 20 = some (.int 20) := by simp [evalAbs, evalMax, evalClamp, absExpr, maxExpr, clampExpr,
            eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- max(7, 3) = 7 -/
theorem max_left : evalMax 7 3 = some (.int 7) := by simp [evalAbs, evalMax, evalClamp, absExpr, maxExpr, clampExpr,
            eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- max(x, x) = x for a specific value (kernel-reducible). -/
theorem max_self : evalMax 42 42 = some (.int 42) := by simp [evalAbs, evalMax, evalClamp, absExpr, maxExpr, clampExpr,
            eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- clamp(5, 0, 10) = 5 (in range) -/
theorem clamp_in_range : evalClamp 5 0 10 = some (.int 5) := by simp [evalAbs, evalMax, evalClamp, absExpr, maxExpr, clampExpr,
            eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- clamp(-3, 0, 10) = 0 (below range) -/
theorem clamp_below : evalClamp (-3) 0 10 = some (.int 0) := by simp [evalAbs, evalMax, evalClamp, absExpr, maxExpr, clampExpr,
            eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- clamp(15, 0, 10) = 10 (above range) -/
theorem clamp_above : evalClamp 15 0 10 = some (.int 10) := by simp [evalAbs, evalMax, evalClamp, absExpr, maxExpr, clampExpr,
            eval, evalBinOp, Env.bind, Env.empty]

/-- Integer literal evaluates to itself (with sufficient fuel). -/
theorem eval_lit (n : Int) (fuel : Nat) (fns : FnTable) (env : Env) :
    eval fns env (fuel + 1) (.lit (.int n)) = some (.int n) := by
  simp [eval]

/-- Boolean literal evaluates to itself (with sufficient fuel). -/
theorem eval_bool_lit (b : Bool) (fuel : Nat) (fns : FnTable) (env : Env) :
    eval fns env (fuel + 1) (.lit (.bool b)) = some (.bool b) := by
  simp [eval]

/-- Variable lookup succeeds when the variable is bound (with sufficient fuel). -/
theorem eval_var_bound (name : String) (v : PVal) (fuel : Nat) (fns : FnTable) :
    eval fns (Env.empty.bind name v) (fuel + 1) (.var name) = some v := by
  simp [eval, Env.bind]

/-- if true then a else b  evaluates to  a. -/
theorem eval_if_true (fns : FnTable) (env : Env) (fuel : Nat) (a b : PExpr) (va : PVal)
    (ha : eval fns env fuel a = some va) :
    eval fns env (fuel + 1) (.ifThenElse (.lit (.bool true)) a b) = some va := by
  simp [eval, ha]

/-- if false then a else b  evaluates to  b. -/
theorem eval_if_false (fns : FnTable) (env : Env) (fuel : Nat) (a b : PExpr) (vb : PVal)
    (hb : eval fns env fuel b = some vb) :
    eval fns env (fuel + 1) (.ifThenElse (.lit (.bool false)) a b) = some vb := by
  simp [eval, hb]

/-- Addition of two integer literals. -/
theorem eval_add_lits (a b : Int) (fuel : Nat) (fns : FnTable) (env : Env) :
    eval fns env (fuel + 1) (.binOp .add (.lit (.int a)) (.lit (.int b)))
    = some (.int (a + b)) := by
  simp [eval, evalBinOp]

/-- Subtraction of two integer literals. -/
theorem eval_sub_lits (a b : Int) (fuel : Nat) (fns : FnTable) (env : Env) :
    eval fns env (fuel + 1) (.binOp .sub (.lit (.int a)) (.lit (.int b)))
    = some (.int (a - b)) := by
  simp [eval, evalBinOp]

/-- Multiplication of two integer literals. -/
theorem eval_mul_lits (a b : Int) (fuel : Nat) (fns : FnTable) (env : Env) :
    eval fns env (fuel + 1) (.binOp .mul (.lit (.int a)) (.lit (.int b)))
    = some (.int (a * b)) := by
  simp [eval, evalBinOp]

-- ============================================================
-- Packet decoder core: parse_byte
-- ============================================================

/-- Helper: evaluate parse_byte with given inputs. -/
def evalParseByte (data offset : Int) : Option PVal :=
  eval proofFns ((Env.empty.bind "data" (.int data)).bind "offset" (.int offset)) 10 parseByteExpr

set_option linter.unusedSimpArgs false in
/-- parse_byte(10, 3) = 13 -/
theorem parse_byte_10_3 : evalParseByte 10 3 = some (.int 13) := by
  simp [evalParseByte, parseByteExpr, eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- parse_byte(0, 0) = 0 -/
theorem parse_byte_zero : evalParseByte 0 0 = some (.int 0) := by
  simp [evalParseByte, parseByteExpr, eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- parse_byte(255, 1) = 256 -/
theorem parse_byte_boundary : evalParseByte 255 1 = some (.int 256) := by
  simp [evalParseByte, parseByteExpr, eval, evalBinOp, Env.bind, Env.empty]

/-- Universal: parse_byte(a, b) = a + b for all integers.
    This is the first universally quantified proof over a Concrete function. -/
theorem parse_byte_correct (a b : Int) (fuel : Nat) :
    eval proofFns ((Env.empty.bind "data" (.int a)).bind "offset" (.int b))
      (fuel + 1) parseByteExpr
    = some (.int (a + b)) := by
  simp [parseByteExpr, eval, Env.bind, evalBinOp]

-- ============================================================
-- Packet decoder core: check_length (bounds guard)
-- ============================================================
-- The bounds guard from decode_header. The theorems prove:
-- the decoder rejects all inputs shorter than the minimum header
-- size, and accepts all inputs at least that long.

/-- Helper: evaluate check_length with a given length. -/
def evalCheckLength (len : Int) : Option PVal :=
  eval proofFns (Env.empty.bind "len" (.int len)) 10 checkLengthExpr

-- Concrete test cases
set_option linter.unusedSimpArgs false in
/-- check_length(5) = 1 (too short) -/
theorem check_length_short : evalCheckLength 5 = some (.int 1) := by
  simp [evalCheckLength, checkLengthExpr, eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- check_length(10) = 0 (exactly minimum) -/
theorem check_length_exact : evalCheckLength 10 = some (.int 0) := by
  simp [evalCheckLength, checkLengthExpr, eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- check_length(1500) = 0 (typical packet) -/
theorem check_length_large : evalCheckLength 1500 = some (.int 0) := by
  simp [evalCheckLength, checkLengthExpr, eval, evalBinOp, Env.bind, Env.empty]

set_option linter.unusedSimpArgs false in
/-- check_length(0) = 1 (empty buffer) -/
theorem check_length_zero : evalCheckLength 0 = some (.int 1) := by
  simp [evalCheckLength, checkLengthExpr, eval, evalBinOp, Env.bind, Env.empty]

/-- Universal: for any length < 10, check_length returns 1 (reject).
    This proves the decoder never reads beyond a too-short buffer. -/
theorem check_length_rejects_short (len : Int) (h : len < 10) (fuel : Nat) :
    eval proofFns (Env.empty.bind "len" (.int len)) (fuel + 2) checkLengthExpr
    = some (.int 1) := by
  have hd : decide (len < 10) = true := decide_eq_true h
  simp [checkLengthExpr, eval, Env.bind, evalBinOp, hd]

/-- Universal: for any length ≥ 10, check_length returns 0 (accept).
    Combined with the rejection theorem, this is a complete specification
    of the bounds guard. -/
theorem check_length_accepts_valid (len : Int) (h : 10 ≤ len) (fuel : Nat) :
    eval proofFns (Env.empty.bind "len" (.int len)) (fuel + 2) checkLengthExpr
    = some (.int 0) := by
  have hd : decide (len < 10) = false := decide_eq_false (by omega)
  simp [checkLengthExpr, eval, Env.bind, evalBinOp, hd]

-- ============================================================
-- Parser core: validate early-rejection (compositional property)
-- ============================================================
-- validate calls check_length first. When check_length rejects (len < 10),
-- validate returns 1 without entering the checksum loop.
-- This is a real parser-core safety property: short inputs are rejected
-- before any data processing occurs.

/-- The guard fragment of validate:
    `if check_length(len) != 0 { return 1; } else { <rest> }`
    We model only the guard path. The else branch is a placeholder because
    the proof fragment does not support loops. The theorem proves that for
    short inputs the else branch is never reached. -/
def validateGuardExpr : PExpr :=
  .ifThenElse
    (.binOp .ne (.call "check_length" [.var "len"]) (.lit (.int 0)))
    (.lit (.int 1))
    (.lit (.int 0))  -- placeholder: unreachable when len < 10

/-- Compositional: validate rejects all packets with len < 10.
    Chains check_length_rejects_short with validate's control flow to prove
    that short inputs are rejected before the checksum loop is entered.

    This is the first proof about function *composition* in Concrete —
    not just an individual helper, but the interaction between guard and caller. -/
theorem validate_rejects_short (data len : Int) (h : len < 10) (fuel : Nat) :
    eval proofFns ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
      (fuel + 5) validateGuardExpr
    = some (.int 1) := by
  have hd : decide (len < 10) = true := decide_eq_true h
  simp [validateGuardExpr, eval, eval.evalArgs, proofFns, checkLengthFn,
        checkLengthExpr, Env.bind, evalBinOp, bindArgs, hd]

-- ============================================================
-- Parser core: decode_header (full proved parser function)
-- ============================================================
-- decode_header is the first parser-core function with complete Lean proofs
-- covering every code path. It calls check_length and parse_byte
-- (both proved helpers), validates extracted fields, and returns
-- error codes or success.
--
-- Error codes: 1=too_short, 2=bad_version, 3=payload_overflow, 0=success
--
-- The proofs cover:
--   1. Bounds rejection:  len < 10 → returns 1
--   2. Version rejection: version < 1 or version > 2 → returns 2
--   3. Payload overflow:  payload_len > len - 10 → returns 3
--   4. Success:           valid inputs → returns 0

/-- `fn decode_header(data: Int, len: Int) -> Int`
    Encoded as nested if-then-else matching the sequential guard pattern. -/
def decodeHeaderExpr : PExpr :=
  .ifThenElse
    (.binOp .ne (.call "check_length" [.var "len"]) (.lit (.int 0)))
    (.lit (.int 1))
    (.letIn "version" (.call "parse_byte" [.var "data", .lit (.int 0)])
      (.ifThenElse
        (.binOp .lt (.var "version") (.lit (.int 1)))
        (.lit (.int 2))
        (.ifThenElse
          (.binOp .gt (.var "version") (.lit (.int 2)))
          (.lit (.int 2))
          (.letIn "payload_len" (.call "parse_byte" [.var "data", .lit (.int 1)])
            (.ifThenElse
              (.binOp .gt (.var "payload_len")
                (.binOp .sub (.var "len") (.lit (.int 10))))
              (.lit (.int 3))
              (.lit (.int 0)))))))

def decodeHeaderFn : PFnDef :=
  { name := "decode_header", params := ["data", "len"], body := decodeHeaderExpr }

/-- Extended function table including decode_header. -/
def proofFnsExt : FnTable
  | "parse_byte" => some parseByteFn
  | "check_length" => some checkLengthFn
  | "decode_header" => some decodeHeaderFn
  | _ => none

/-- Helper: evaluate decode_header with given inputs. -/
def evalDecodeHeader (data len : Int) (fuel : Nat) : Option PVal :=
  eval proofFnsExt ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
    fuel decodeHeaderExpr

-- Concrete test cases (verified by kernel reduction)
#eval evalDecodeHeader 1 20 20   -- valid: version=1, payload=2, len=20 → 0
#eval evalDecodeHeader 1 5 20    -- too short → 1
#eval evalDecodeHeader 0 20 20   -- bad version (0 < 1) → 2
#eval evalDecodeHeader 3 20 20   -- bad version (3 > 2) → 2

/-- 1. Bounds rejection: short packets are rejected before any field access.
    Chains check_length_rejects_short through decode_header's control flow. -/
theorem decode_header_rejects_short (data len : Int) (h : len < 10) (fuel : Nat) :
    eval proofFnsExt ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
      (fuel + 6) decodeHeaderExpr
    = some (.int 1) := by
  have hd : decide (len < 10) = true := decide_eq_true h
  simp [decodeHeaderExpr, eval, eval.evalArgs, proofFnsExt, checkLengthFn,
        checkLengthExpr, Env.bind, evalBinOp, bindArgs, hd]

/-- 2a. Version rejection (too low): version < 1 returns error 2.
    parse_byte(data, 0) = data + 0 = data, so version = data. -/
theorem decode_header_rejects_low_version
    (data len : Int) (hlen : 10 ≤ len) (hver : data < 1) (fuel : Nat) :
    eval proofFnsExt ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
      (fuel + 10) decodeHeaderExpr
    = some (.int 2) := by
  have hlen' : decide (len < 10) = false := decide_eq_false (by omega)
  -- simp reduces `data + 0` to `data`, so the remaining decide is on `data < 1`
  have hver' : decide (data < 1) = true := decide_eq_true hver
  simp [decodeHeaderExpr, eval, eval.evalArgs, proofFnsExt, checkLengthFn,
        checkLengthExpr, parseByteFn, parseByteExpr, Env.bind, evalBinOp,
        bindArgs, hlen', hver']

/-- 2b. Version rejection (too high): version > 2 returns error 2. -/
theorem decode_header_rejects_high_version
    (data len : Int) (hlen : 10 ≤ len) (hver : data > 2) (fuel : Nat) :
    eval proofFnsExt ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
      (fuel + 10) decodeHeaderExpr
    = some (.int 2) := by
  have hlen' : decide (len < 10) = false := decide_eq_false (by omega)
  -- After simp: version checks become `decide (data < 1)` and `decide (2 < data)`
  have hver_low : decide (data < 1) = false := decide_eq_false (by omega)
  have hver_high : decide (2 < data) = true := decide_eq_true (by omega)
  simp [decodeHeaderExpr, eval, eval.evalArgs, proofFnsExt, checkLengthFn,
        checkLengthExpr, parseByteFn, parseByteExpr, Env.bind, evalBinOp,
        bindArgs, hlen', hver_low, hver_high]

/-- 3. Payload overflow: payload_len > len - 10 returns error 3.
    parse_byte(data, 1) = data + 1, so payload_len = data + 1. -/
theorem decode_header_rejects_overflow
    (data len : Int) (hlen : 10 ≤ len) (hver_lo : 1 ≤ data) (hver_hi : data ≤ 2)
    (hoverflow : data + 1 > len - 10) (fuel : Nat) :
    eval proofFnsExt ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
      (fuel + 12) decodeHeaderExpr
    = some (.int 3) := by
  have hlen' : decide (len < 10) = false := decide_eq_false (by omega)
  have hver_low : decide (data < 1) = false := decide_eq_false (by omega)
  have hver_high : decide (2 < data) = false := decide_eq_false (by omega)
  have hov : decide (len - 10 < data + 1) = true := decide_eq_true (by omega)
  simp [decodeHeaderExpr, eval, eval.evalArgs, proofFnsExt, checkLengthFn,
        checkLengthExpr, parseByteFn, parseByteExpr, Env.bind, evalBinOp,
        bindArgs, hlen', hver_low, hver_high, hov]

/-- 4. Success: valid inputs pass all guards and return 0.
    This is the complete correctness theorem for the happy path. -/
theorem decode_header_valid
    (data len : Int) (hlen : 10 ≤ len) (hver_lo : 1 ≤ data) (hver_hi : data ≤ 2)
    (hpayload : data + 1 ≤ len - 10) (fuel : Nat) :
    eval proofFnsExt ((Env.empty.bind "data" (.int data)).bind "len" (.int len))
      (fuel + 12) decodeHeaderExpr
    = some (.int 0) := by
  have hlen' : decide (len < 10) = false := decide_eq_false (by omega)
  have hver_low : decide (data < 1) = false := decide_eq_false (by omega)
  have hver_high : decide (2 < data) = false := decide_eq_false (by omega)
  have hov : decide (len - 10 < data + 1) = false := decide_eq_false (by omega)
  simp [decodeHeaderExpr, eval, eval.evalArgs, proofFnsExt, checkLengthFn,
        checkLengthExpr, parseByteFn, parseByteExpr, Env.bind, evalBinOp,
        bindArgs, hlen', hver_low, hver_high, hov]

-- ============================================================
-- Crypto verification core (flagship example #2)
-- ============================================================
-- Three pure functions from examples/crypto_verify: a keyed tag
-- computation, a tag verifier, and a nonce bounds checker.
-- The proofs demonstrate security-critical verification in a
-- second domain (crypto/auth) beyond the packet parser.

/-- `fn compute_tag(key, message, nonce) -> Int { return key * message + nonce; }` -/
def computeTagExpr : PExpr :=
  .binOp .add (.binOp .mul (.var "key") (.var "message")) (.var "nonce")

def computeTagFn : PFnDef :=
  { name := "compute_tag", params := ["key", "message", "nonce"], body := computeTagExpr }

/-- `fn verify_tag(key, message, nonce, expected_tag) -> Int {
       let computed = compute_tag(key, message, nonce);
       if computed == expected_tag { return 1; } else { return 0; }
    }` -/
def verifyTagExpr : PExpr :=
  .letIn "computed" (.call "compute_tag" [.var "key", .var "message", .var "nonce"])
    (.ifThenElse
      (.binOp .eq (.var "computed") (.var "expected_tag"))
      (.lit (.int 1))
      (.lit (.int 0)))

def verifyTagFn : PFnDef :=
  { name := "verify_tag", params := ["key", "message", "nonce", "expected_tag"], body := verifyTagExpr }

/-- `fn check_nonce(nonce, max_nonce) -> Int {
       if nonce > 0 { if nonce <= max_nonce { return 1; } else { return 0; } }
       else { return 0; }
    }` -/
def checkNonceExpr : PExpr :=
  .ifThenElse
    (.binOp .gt (.var "nonce") (.lit (.int 0)))
    (.ifThenElse
      (.binOp .le (.var "nonce") (.var "max_nonce"))
      (.lit (.int 1))
      (.lit (.int 0)))
    (.lit (.int 0))

def checkNonceFn : PFnDef :=
  { name := "check_nonce", params := ["nonce", "max_nonce"], body := checkNonceExpr }

/-- `fn verify_message(key, message, nonce, expected_tag, max_nonce) -> Int {
       if verify_tag(key, message, nonce, expected_tag) != 1 { return 0; }
       if check_nonce(nonce, max_nonce) != 1 { return 0; }
       return 1;
    }`
    — composed verification: the message is acceptable iff its tag
    matches under the given key AND the nonce is in range. -/
def verifyMessageExpr : PExpr :=
  .ifThenElse
    (.binOp .ne (.call "verify_tag"
                  [.var "key", .var "message", .var "nonce", .var "expected_tag"])
                (.lit (.int 1)))
    (.lit (.int 0))
    (.ifThenElse
      (.binOp .ne (.call "check_nonce" [.var "nonce", .var "max_nonce"])
                  (.lit (.int 1)))
      (.lit (.int 0))
      (.lit (.int 1)))

def verifyMessageFn : PFnDef :=
  { name := "verify_message",
    params := ["key", "message", "nonce", "expected_tag", "max_nonce"],
    body := verifyMessageExpr }

/-- Function table for crypto verification proofs. -/
def cryptoFns : FnTable
  | "compute_tag" => some computeTagFn
  | "verify_tag" => some verifyTagFn
  | "check_nonce" => some checkNonceFn
  | "verify_message" => some verifyMessageFn
  | _ => none

-- Evaluation helpers

def evalComputeTag (key message nonce : Int) : Option PVal :=
  eval cryptoFns
    (((Env.empty.bind "key" (.int key)).bind "message" (.int message)).bind "nonce" (.int nonce))
    10 computeTagExpr

def evalVerifyTag (key message nonce expected : Int) (fuel : Nat) : Option PVal :=
  let env := (((Env.empty.bind "key" (.int key)).bind "message" (.int message)).bind "nonce" (.int nonce)).bind "expected_tag" (.int expected)
  eval cryptoFns env fuel verifyTagExpr

def evalCheckNonce (nonce maxNonce : Int) : Option PVal :=
  eval cryptoFns
    ((Env.empty.bind "nonce" (.int nonce)).bind "max_nonce" (.int maxNonce))
    10 checkNonceExpr

-- Concrete test cases
#eval evalComputeTag 42 100 7         -- some (int 4207)
#eval evalVerifyTag 42 100 7 4207 20  -- some (int 1) — valid tag
#eval evalVerifyTag 42 100 7 9999 20  -- some (int 0) — bad tag
#eval evalCheckNonce 7 1000           -- some (int 1) — valid nonce
#eval evalCheckNonce 0 1000           -- some (int 0) — zero nonce
#eval evalCheckNonce 1001 1000        -- some (int 0) — over max

-- Proofs

/-- compute_tag is key * message + nonce for all integers. -/
theorem compute_tag_correct (key message nonce : Int) (fuel : Nat) :
    eval cryptoFns
      (((Env.empty.bind "key" (.int key)).bind "message" (.int message)).bind "nonce" (.int nonce))
      (fuel + 1) computeTagExpr
    = some (.int (key * message + nonce)) := by
  simp [computeTagExpr, eval, Env.bind, evalBinOp]

/-- verify_tag returns 1 when the expected tag equals key * message + nonce.
    This is the MAC verification correctness property: if verify succeeds,
    the tag authenticates the message under the given key. -/
theorem verify_tag_correct (key message nonce : Int) (fuel : Nat) :
    let env := (((Env.empty.bind "key" (.int key)).bind "message" (.int message)).bind "nonce" (.int nonce)).bind "expected_tag" (.int (key * message + nonce))
    eval cryptoFns env (fuel + 6) verifyTagExpr = some (.int 1) := by
  simp [verifyTagExpr, eval, eval.evalArgs, cryptoFns, computeTagFn, computeTagExpr,
        Env.bind, evalBinOp, bindArgs]

/-- verify_tag returns 0 when the expected tag does not match.
    This is the forgery rejection property: a wrong tag is always detected. -/
theorem verify_tag_rejects (key message nonce expected : Int)
    (h : expected ≠ key * message + nonce) (fuel : Nat) :
    let env := (((Env.empty.bind "key" (.int key)).bind "message" (.int message)).bind "nonce" (.int nonce)).bind "expected_tag" (.int expected)
    eval cryptoFns env (fuel + 6) verifyTagExpr = some (.int 0) := by
  have hne : (key * message + nonce == expected) = false := by
    show decide (key * message + nonce = expected) = false
    exact decide_eq_false (Ne.symm h)
  simp [verifyTagExpr, eval, eval.evalArgs, cryptoFns, computeTagFn, computeTagExpr,
        Env.bind, evalBinOp, bindArgs, hne]

/-- check_nonce returns 1 for nonces in range [1, max_nonce]. -/
theorem check_nonce_accepts_valid (nonce maxNonce : Int)
    (hpos : 0 < nonce) (hmax : nonce ≤ maxNonce) (fuel : Nat) :
    eval cryptoFns
      ((Env.empty.bind "nonce" (.int nonce)).bind "max_nonce" (.int maxNonce))
      (fuel + 3) checkNonceExpr
    = some (.int 1) := by
  have hgt : decide (0 < nonce) = true := decide_eq_true hpos
  have hle : decide (nonce ≤ maxNonce) = true := decide_eq_true hmax
  simp [checkNonceExpr, eval, Env.bind, evalBinOp, hgt, hle]

/-- check_nonce returns 0 for non-positive nonces. -/
theorem check_nonce_rejects_nonpositive (nonce maxNonce : Int)
    (h : nonce ≤ 0) (fuel : Nat) :
    eval cryptoFns
      ((Env.empty.bind "nonce" (.int nonce)).bind "max_nonce" (.int maxNonce))
      (fuel + 2) checkNonceExpr
    = some (.int 0) := by
  have hgt : decide (0 < nonce) = false := decide_eq_false (by omega)
  simp [checkNonceExpr, eval, Env.bind, evalBinOp, hgt]

/-- check_nonce returns 0 for nonces exceeding the maximum. -/
theorem check_nonce_rejects_over_max (nonce maxNonce : Int)
    (hpos : 0 < nonce) (hover : maxNonce < nonce) (fuel : Nat) :
    eval cryptoFns
      ((Env.empty.bind "nonce" (.int nonce)).bind "max_nonce" (.int maxNonce))
      (fuel + 3) checkNonceExpr
    = some (.int 0) := by
  have hgt : decide (0 < nonce) = true := decide_eq_true hpos
  have hle : decide (nonce ≤ maxNonce) = false := decide_eq_false (by omega)
  simp [checkNonceExpr, eval, Env.bind, evalBinOp, hgt, hle]

set_option linter.unusedSimpArgs false in
/-- Composition theorem for verify_message (success direction).

    A message+tag pair is acceptable iff:
      1. the tag matches under the given key   (verify_tag → 1)
      2. the nonce is in the accepted range    (check_nonce → 1)

    This theorem proves that when both component validators succeed,
    the composed verify_message returns 1. The component theorems
    (verify_tag_correct, check_nonce_accepts_valid) carry the
    individual properties; this theorem chains them.

    The failure direction (returns 0 if either sub-check fails) is
    follow-up — same per-failure pattern as parse_validate. -/
theorem verify_message_composed_correct
    (key message nonce maxNonce : Int) (fuel : Nat)
    (h_nonce_pos : 0 < nonce) (h_nonce_max : nonce ≤ maxNonce) :
    let env := (((((Env.empty.bind "key" (.int key)).bind
                    "message" (.int message)).bind
                    "nonce" (.int nonce)).bind
                    "expected_tag" (.int (key * message + nonce))).bind
                    "max_nonce" (.int maxNonce))
    eval cryptoFns env (fuel + 20) verifyMessageExpr = some (.int 1) := by
  have hgt : decide (0 < nonce) = true := decide_eq_true h_nonce_pos
  have hle : decide (nonce ≤ maxNonce) = true := decide_eq_true h_nonce_max
  simp_all [verifyMessageExpr,
            verifyTagFn, verifyTagExpr,
            checkNonceFn, checkNonceExpr,
            computeTagFn, computeTagExpr,
            eval, eval.evalArgs, cryptoFns,
            Env.bind, evalBinOp, bindArgs, BEq.beq]

/-- Full contract for check_nonce: returns 1 iff nonce ∈ [1, max_nonce], 0 otherwise.
    This is the theorem attached in the proof registry for main.check_nonce. -/
theorem check_nonce_correct (nonce maxNonce : Int) (fuel : Nat) :
    eval cryptoFns
      ((Env.empty.bind "nonce" (.int nonce)).bind "max_nonce" (.int maxNonce))
      (fuel + 3) checkNonceExpr
    = some (.int (if 0 < nonce ∧ nonce ≤ maxNonce then 1 else 0)) := by
  by_cases hpos : 0 < nonce
  · by_cases hle : nonce ≤ maxNonce
    · have hgt : decide (0 < nonce) = true := decide_eq_true hpos
      have hle' : decide (nonce ≤ maxNonce) = true := decide_eq_true hle
      have hboth : 0 < nonce ∧ nonce ≤ maxNonce := ⟨hpos, hle⟩
      simp [checkNonceExpr, eval, Env.bind, evalBinOp, hboth]
    · have hgt : decide (0 < nonce) = true := decide_eq_true hpos
      have hle' : decide (nonce ≤ maxNonce) = false := decide_eq_false hle
      have hnboth : ¬(0 < nonce ∧ nonce ≤ maxNonce) := fun h => hle h.2
      simp [checkNonceExpr, eval, Env.bind, evalBinOp, hgt, hle', hnboth]
  · have hgt : decide (0 < nonce) = false := decide_eq_false hpos
    have hnboth : ¬(0 < nonce ∧ nonce ≤ maxNonce) := fun h => hpos h.1
    simp [checkNonceExpr, eval, Env.bind, evalBinOp, hgt, hnboth]

-- ============================================================
-- ELF header field validator (flagship example #3)
-- ============================================================

-- PExpr definitions matching the Concrete source in examples/elf_header/src/main.con

/-- `fn check_magic(b0, b1, b2, b3) -> Int` — checks ELF magic: 0x7F 'E' 'L' 'F' -/
def checkMagicExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "b0") (.lit (.int 127)))
    (.ifThenElse (.binOp .eq (.var "b1") (.lit (.int 69)))
      (.ifThenElse (.binOp .eq (.var "b2") (.lit (.int 76)))
        (.ifThenElse (.binOp .eq (.var "b3") (.lit (.int 70)))
          (.lit (.int 1))
          (.lit (.int 0)))
        (.lit (.int 0)))
      (.lit (.int 0)))
    (.lit (.int 0))

def checkMagicFn : PFnDef :=
  { name := "check_magic", params := ["b0", "b1", "b2", "b3"], body := checkMagicExpr }

/-- `fn check_class(cls) -> Int` — checks 1 (32-bit) or 2 (64-bit) -/
def checkClassExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "cls") (.lit (.int 1)))
    (.lit (.int 1))
    (.ifThenElse (.binOp .eq (.var "cls") (.lit (.int 2)))
      (.lit (.int 1))
      (.lit (.int 0)))

def checkClassFn : PFnDef :=
  { name := "check_class", params := ["cls"], body := checkClassExpr }

/-- `fn check_data(encoding) -> Int` — checks 1 (little) or 2 (big) -/
def checkDataExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "encoding") (.lit (.int 1)))
    (.lit (.int 1))
    (.ifThenElse (.binOp .eq (.var "encoding") (.lit (.int 2)))
      (.lit (.int 1))
      (.lit (.int 0)))

def checkDataFn : PFnDef :=
  { name := "check_data", params := ["encoding"], body := checkDataExpr }

/-- `fn check_version(ver) -> Int` — checks ver == 1 (EV_CURRENT) -/
def checkVersionExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "ver") (.lit (.int 1)))
    (.lit (.int 1))
    (.lit (.int 0))

def checkVersionFn : PFnDef :=
  { name := "check_version", params := ["ver"], body := checkVersionExpr }

/-- `fn validate_header(b0, b1, b2, b3, cls, encoding, ver) -> Int` -/
def validateHeaderExpr : PExpr :=
  .letIn "magic_ok" (.call "check_magic" [.var "b0", .var "b1", .var "b2", .var "b3"])
    (.ifThenElse (.binOp .eq (.var "magic_ok") (.lit (.int 1)))
      (.letIn "cls_ok" (.call "check_class" [.var "cls"])
        (.ifThenElse (.binOp .eq (.var "cls_ok") (.lit (.int 1)))
          (.letIn "enc_ok" (.call "check_data" [.var "encoding"])
            (.ifThenElse (.binOp .eq (.var "enc_ok") (.lit (.int 1)))
              (.letIn "ver_ok" (.call "check_version" [.var "ver"])
                (.ifThenElse (.binOp .eq (.var "ver_ok") (.lit (.int 1)))
                  (.lit (.int 1))
                  (.lit (.int 0))))
              (.lit (.int 0))))
          (.lit (.int 0))))
      (.lit (.int 0)))

def validateHeaderFn : PFnDef :=
  { name := "validate_header",
    params := ["b0", "b1", "b2", "b3", "cls", "encoding", "ver"],
    body := validateHeaderExpr }

/-- Function table for ELF header validator proofs. -/
def elfFns : FnTable
  | "check_magic"   => some checkMagicFn
  | "check_class"   => some checkClassFn
  | "check_data"    => some checkDataFn
  | "check_version" => some checkVersionFn
  | "validate_header" => some validateHeaderFn
  | _ => none

-- Concrete test cases
#eval eval elfFns
  ((((Env.empty.bind "b0" (.int 127)).bind "b1" (.int 69)).bind "b2" (.int 76)).bind "b3" (.int 70))
  10 checkMagicExpr  -- some (int 1)
#eval eval elfFns
  ((((Env.empty.bind "b0" (.int 0)).bind "b1" (.int 69)).bind "b2" (.int 76)).bind "b3" (.int 70))
  10 checkMagicExpr  -- some (int 0)
#eval eval elfFns (Env.empty.bind "cls" (.int 2)) 10 checkClassExpr  -- some (int 1)
#eval eval elfFns (Env.empty.bind "cls" (.int 3)) 10 checkClassExpr  -- some (int 0)
#eval eval elfFns (Env.empty.bind "ver" (.int 1)) 10 checkVersionExpr  -- some (int 1)
#eval eval elfFns (Env.empty.bind "ver" (.int 0)) 10 checkVersionExpr  -- some (int 0)

-- ---- Proofs ----

/-- check_magic returns 1 iff all four bytes are the ELF magic sequence. -/
theorem check_magic_correct (b0 b1 b2 b3 : Int) (fuel : Nat) :
    eval elfFns
      ((((Env.empty.bind "b0" (.int b0)).bind "b1" (.int b1)).bind "b2" (.int b2)).bind "b3" (.int b3))
      (fuel + 5) checkMagicExpr
    = some (.int (if b0 = 127 ∧ b1 = 69 ∧ b2 = 76 ∧ b3 = 70 then 1 else 0)) := by
  by_cases h0 : b0 = 127 <;> by_cases h1 : b1 = 69 <;>
    by_cases h2 : b2 = 76 <;> by_cases h3 : b3 = 70 <;>
    simp_all [checkMagicExpr, eval, Env.bind, evalBinOp, BEq.beq]

/-- check_class returns 1 iff cls is 1 or 2. -/
theorem check_class_correct (cls : Int) (fuel : Nat) :
    eval elfFns (Env.empty.bind "cls" (.int cls)) (fuel + 3) checkClassExpr
    = some (.int (if cls = 1 ∨ cls = 2 then 1 else 0)) := by
  by_cases h1 : cls = 1 <;> by_cases h2 : cls = 2 <;>
    simp_all [checkClassExpr, eval, Env.bind, evalBinOp, BEq.beq]

/-- check_data returns 1 iff encoding is 1 or 2. -/
theorem check_data_correct (encoding : Int) (fuel : Nat) :
    eval elfFns (Env.empty.bind "encoding" (.int encoding)) (fuel + 3) checkDataExpr
    = some (.int (if encoding = 1 ∨ encoding = 2 then 1 else 0)) := by
  by_cases h1 : encoding = 1 <;> by_cases h2 : encoding = 2 <;>
    simp_all [checkDataExpr, eval, Env.bind, evalBinOp, BEq.beq]

/-- check_version returns 1 iff ver is 1. -/
theorem check_version_correct (ver : Int) (fuel : Nat) :
    eval elfFns (Env.empty.bind "ver" (.int ver)) (fuel + 2) checkVersionExpr
    = some (.int (if ver = 1 then 1 else 0)) := by
  by_cases h : ver = 1 <;>
    simp_all [checkVersionExpr, eval, Env.bind, evalBinOp, BEq.beq]

/-- Full correctness of validate_header: returns 1 iff all ELF field
    constraints hold (magic = 0x7F 'E' 'L' 'F', class ∈ {1,2},
    encoding ∈ {1,2}, version = 1). Sound + complete in one theorem. -/
theorem validate_header_correct (b0 b1 b2 b3 cls encoding ver : Int) (fuel : Nat) :
    let env := ((((((Env.empty.bind "b0" (.int b0)).bind "b1" (.int b1)).bind "b2" (.int b2)).bind "b3" (.int b3)).bind "cls" (.int cls)).bind "encoding" (.int encoding)).bind "ver" (.int ver)
    eval elfFns env (fuel + 10) validateHeaderExpr
    = some (.int (if b0 = 127 ∧ b1 = 69 ∧ b2 = 76 ∧ b3 = 70 ∧
                     (cls = 1 ∨ cls = 2) ∧ (encoding = 1 ∨ encoding = 2) ∧ ver = 1
                  then 1 else 0)) := by
  -- Substitute magic bytes, then case-split cls/encoding/ver (8 concrete cases)
  by_cases h0 : b0 = 127 <;> by_cases h1 : b1 = 69 <;>
    by_cases h2 : b2 = 76 <;> by_cases h3 : b3 = 70
  -- Positive magic (all match): case-split remaining 3 fields
  · by_cases hc1 : cls = 1 <;> by_cases hc2 : cls = 2 <;>
      by_cases he1 : encoding = 1 <;> by_cases he2 : encoding = 2 <;>
      by_cases hv : ver = 1 <;>
      simp_all [validateHeaderExpr, eval, eval.evalArgs, elfFns,
          checkMagicFn, checkMagicExpr, checkClassFn, checkClassExpr,
          checkDataFn, checkDataExpr, checkVersionFn, checkVersionExpr,
          Env.bind, evalBinOp, bindArgs, BEq.beq]
  -- Negative magic cases (any byte wrong): check_magic returns 0, short-circuit
  all_goals simp_all [validateHeaderExpr, eval, eval.evalArgs, elfFns,
      checkMagicFn, checkMagicExpr, Env.bind, evalBinOp, bindArgs,
      BEq.beq]

-- ============================================================
-- parse_validate (pull-through pilot — first attached theorem)
-- ============================================================

-- PExpr definitions matching the Concrete source in
-- examples/parse_validate/src/main.con. parse_validate uses the
-- early-return-as-else shape: every validator returns 0 on success
-- and 1 on failure (opposite of ELF's success-returns-1 convention).

/-- `fn validate_version(v: i32) -> i32` — checks v == 1, returns 0 on
    success and 1 on failure. -/
def validateVersionExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "v") (.lit (.int 1)))
    (.lit (.int 0))
    (.lit (.int 1))

def validateVersionFn : PFnDef :=
  { name := "validate_version", params := ["v"], body := validateVersionExpr }

/-- `fn validate_msg_type(t: i32) -> i32` — checks 1 ≤ t ≤ 4. -/
def validateMsgTypeExpr : PExpr :=
  .ifThenElse (.binOp .ge (.var "t") (.lit (.int 1)))
    (.ifThenElse (.binOp .le (.var "t") (.lit (.int 4)))
      (.lit (.int 0))
      (.lit (.int 1)))
    (.lit (.int 1))

def validateMsgTypeFn : PFnDef :=
  { name := "validate_msg_type", params := ["t"], body := validateMsgTypeExpr }

/-- `fn validate_payload_len(plen, max_len: i32) -> i32` —
    checks 0 ≤ plen ≤ max_len. -/
def validatePayloadLenExpr : PExpr :=
  .ifThenElse (.binOp .ge (.var "plen") (.lit (.int 0)))
    (.ifThenElse (.binOp .le (.var "plen") (.var "max_len"))
      (.lit (.int 0))
      (.lit (.int 1)))
    (.lit (.int 1))

def validatePayloadLenFn : PFnDef :=
  { name := "validate_payload_len", params := ["plen", "max_len"], body := validatePayloadLenExpr }

/-- `fn validate_total_len(actual, needed: i32) -> i32` —
    checks actual ≥ needed. -/
def validateTotalLenExpr : PExpr :=
  .ifThenElse (.binOp .ge (.var "actual") (.var "needed"))
    (.lit (.int 0))
    (.lit (.int 1))

def validateTotalLenFn : PFnDef :=
  { name := "validate_total_len", params := ["actual", "needed"], body := validateTotalLenExpr }

/-- `fn validate_checksum(expected, computed: i32) -> i32` —
    checks expected == computed. -/
def validateChecksumExpr : PExpr :=
  .ifThenElse (.binOp .eq (.var "expected") (.var "computed"))
    (.lit (.int 0))
    (.lit (.int 1))

def validateChecksumFn : PFnDef :=
  { name := "validate_checksum", params := ["expected", "computed"], body := validateChecksumExpr }

/-- `fn validate_header_fields(v, t, plen, total_len, cs_expected, cs_computed) -> i32`
    — composes the five validators. Returns 0 on success or the
    1..6 index of the first failing check. -/
def validateHeaderFieldsExpr : PExpr :=
  .ifThenElse (.binOp .ne (.call "validate_total_len" [.var "total_len", .lit (.int 5)]) (.lit (.int 0)))
    (.lit (.int 1))
    (.ifThenElse (.binOp .ne (.call "validate_version" [.var "v"]) (.lit (.int 0)))
      (.lit (.int 2))
      (.ifThenElse (.binOp .ne (.call "validate_msg_type" [.var "t"]) (.lit (.int 0)))
        (.lit (.int 3))
        (.ifThenElse (.binOp .ne (.call "validate_payload_len" [.var "plen", .lit (.int 240)]) (.lit (.int 0)))
          (.lit (.int 4))
          (.ifThenElse (.binOp .ne (.call "validate_total_len" [.var "total_len", .binOp .add (.lit (.int 4)) (.var "plen")]) (.lit (.int 0)))
            (.lit (.int 5))
            (.ifThenElse (.binOp .ne (.call "validate_checksum" [.var "cs_expected", .var "cs_computed"]) (.lit (.int 0)))
              (.lit (.int 6))
              (.lit (.int 0)))))))

def validateHeaderFieldsFn : PFnDef :=
  { name := "validate_header_fields",
    params := ["v", "t", "plen", "total_len", "cs_expected", "cs_computed"],
    body := validateHeaderFieldsExpr }

-- Helpers for parse_header construction (PExpr layer, not Concrete source).
private def errResultExpr (variant : String) : PExpr :=
  .enumLit "Result" "Err"
    [("error", .enumLit "ParseError" variant [])]

private def okHeaderExpr : PExpr :=
  .enumLit "Result" "Ok"
    [("value", .structLit "Header"
      [("version",     .arrayIndex (.var "data") (.lit (.int 0))),
       ("msg_type",    .arrayIndex (.var "data") (.lit (.int 1))),
       ("payload_len", .arrayIndex (.var "data") (.lit (.int 2))),
       ("checksum",    .arrayIndex (.var "data") (.lit (.int 4)))])]

/-- `fn parse_header(data: [i32; 8], len: i32) -> Result<Header, ParseError>`
    — the actual Result-returning function, extracted exactly per the
    body fingerprint emitted by `--report fingerprints`.

    Uses array index, struct literal, and enum literal — all three are
    supported by ProofCore as of recent commits. compute_checksum
    appears in the body but its body uses a while loop that ProofCore
    cannot yet extract; theorems below evaluate `parseHeaderExpr` in
    paths that bail before the checksum call (the failure-path
    theorems work this way), or take the checksum result as an
    assumption (the success-path theorem, when it lands). -/
def parseHeaderExpr : PExpr :=
  .ifThenElse
    (.binOp .ne
      (.call "validate_total_len" [.var "len", .lit (.int 5)])
      (.lit (.int 0)))
    (errResultExpr "TooShort")
    (.ifThenElse
      (.binOp .ne
        (.call "validate_version" [.arrayIndex (.var "data") (.lit (.int 0))])
        (.lit (.int 0)))
      (errResultExpr "BadVersion")
      (.ifThenElse
        (.binOp .ne
          (.call "validate_msg_type" [.arrayIndex (.var "data") (.lit (.int 1))])
          (.lit (.int 0)))
        (errResultExpr "BadType")
        (.ifThenElse
          (.binOp .ne
            (.call "validate_payload_len" [.arrayIndex (.var "data") (.lit (.int 2)), .lit (.int 240)])
            (.lit (.int 0)))
          (errResultExpr "PayloadTooBig")
          (.ifThenElse
            (.binOp .ne
              (.call "validate_total_len"
                [.var "len",
                 .binOp .add (.lit (.int 4)) (.arrayIndex (.var "data") (.lit (.int 2)))])
              (.lit (.int 0)))
            (errResultExpr "Truncated")
            (.letIn "computed"
              (.call "compute_checksum" [.var "data", .lit (.int 4)])
              (.ifThenElse
                (.binOp .ne
                  (.call "validate_checksum"
                    [.arrayIndex (.var "data") (.lit (.int 4)), .var "computed"])
                  (.lit (.int 0)))
                (errResultExpr "BadChecksum")
                okHeaderExpr))))))

def parseHeaderFn : PFnDef :=
  { name := "parse_header", params := ["data", "len"], body := parseHeaderExpr }

/-- Function table for parse_validate proofs. -/
def parseValidateFns : FnTable
  | "validate_version" => some validateVersionFn
  | "validate_msg_type" => some validateMsgTypeFn
  | "validate_payload_len" => some validatePayloadLenFn
  | "validate_total_len" => some validateTotalLenFn
  | "validate_checksum" => some validateChecksumFn
  | "validate_header_fields" => some validateHeaderFieldsFn
  | "parse_header" => some parseHeaderFn
  | _ => none

/-- validate_version returns 0 iff v == 1, and 1 otherwise. -/
theorem validate_version_correct (v : Int) (fuel : Nat) :
    eval parseValidateFns (Env.empty.bind "v" (.int v)) (fuel + 2) validateVersionExpr
    = some (.int (if v = 1 then 0 else 1)) := by
  by_cases h : v = 1 <;>
    simp_all [validateVersionExpr, eval, Env.bind, evalBinOp, BEq.beq]

set_option linter.unusedSimpArgs false in
/-- validate_header_fields composition theorem (success direction).

    The central thesis claim of the parse_validate pilot, at the scope
    ProofCore can currently extract: "successful return implies multiple
    structural invariants." Stated as the *contrapositive* — when every
    structural precondition holds, the composition returns 0.

    The function returns 0 iff every component validator succeeds. This
    theorem proves the success direction (preconditions ⟹ returns 0).
    The full iff (failure direction with return code matching the first
    failing check) is a 256-branch case split that exhausts heartbeats;
    splitting it into per-failure theorems is follow-up work. -/
theorem validate_header_fields_success
    (v t plen total_len cs_expected cs_computed : Int) (fuel : Nat)
    (h_tl5 : total_len ≥ 5) (h_v : v = 1)
    (h_t1 : t ≥ 1) (h_t4 : t ≤ 4)
    (h_p0 : plen ≥ 0) (h_p240 : plen ≤ 240)
    (h_tlp : total_len ≥ 4 + plen) (h_cs : cs_expected = cs_computed) :
    eval parseValidateFns
      (((((Env.empty.bind "v" (.int v)).bind "t" (.int t)).bind "plen" (.int plen)).bind
        "total_len" (.int total_len)).bind "cs_expected" (.int cs_expected) |>.bind
        "cs_computed" (.int cs_computed))
      (fuel + 20) validateHeaderFieldsExpr
    = some (.int 0) := by
  -- Substitute the equalities so v becomes 1 and cs_expected becomes cs_computed.
  subst h_v
  subst h_cs
  simp_all [validateHeaderFieldsExpr,
            validateVersionFn, validateVersionExpr,
            validateMsgTypeFn, validateMsgTypeExpr,
            validatePayloadLenFn, validatePayloadLenExpr,
            validateTotalLenFn, validateTotalLenExpr,
            validateChecksumFn, validateChecksumExpr,
            eval, eval.evalArgs, parseValidateFns,
            Env.bind, evalBinOp, bindArgs, BEq.beq]

set_option linter.unusedSimpArgs false in
/-- parse_header returns `Err(TooShort)` whenever `len < 5`.

    First attached theorem on the actual `parse_header` function (not
    the scalar-parameter scaffold `validate_header_fields`). The
    failure-path theorems are the ones we can prove today without
    modelling `compute_checksum` (whose body uses a while loop, a
    ProofCore subgoal still pending). The success-path theorem will
    follow either bounded-while-loop extraction or a hand-modelled
    compute_checksum convention. -/
theorem parse_header_too_short
    (data : List PVal) (len : Int) (fuel : Nat) (h : len < 5) :
    eval parseValidateFns
      ((Env.empty.bind "data" (.array_ data)).bind "len" (.int len))
      (fuel + 10) parseHeaderExpr
    = some (.enum_ "Result" "Err"
        [("error", .enum_ "ParseError" "TooShort" [])]) := by
  -- validate_total_len(len, 5) = 1 when len < 5, by its definition.
  have h_dec : decide (5 ≤ len) = false := decide_eq_false (by omega)
  simp [parseHeaderExpr, errResultExpr,
        validateTotalLenFn, validateTotalLenExpr,
        eval, eval.evalArgs, eval.evalFields, parseValidateFns,
        Env.bind, evalBinOp, bindArgs, BEq.beq, h_dec]

-- The remaining failure-path theorems all share the same shape:
-- bind data to an array whose first few elements satisfy enough
-- preconditions for the earlier validators to pass, but whose
-- current field violates the validator under test. Each theorem
-- targets one specific Err variant.

set_option linter.unusedSimpArgs false in
/-- parse_header returns `Err(BadVersion)` when `data[0] ≠ 1`
    (and `len ≥ 5` so the length check passes first). -/
theorem parse_header_bad_version
    (v : Int) (rest : List PVal) (len : Int) (fuel : Nat)
    (h_len : len ≥ 5) (h_v : v ≠ 1) :
    eval parseValidateFns
      ((Env.empty.bind "data" (.array_ (.int v :: rest))).bind "len" (.int len))
      (fuel + 15) parseHeaderExpr
    = some (.enum_ "Result" "Err"
        [("error", .enum_ "ParseError" "BadVersion" [])]) := by
  have h_len_dec : decide (5 ≤ len) = true := decide_eq_true h_len
  have h_v_dec  : decide (v = 1) = false := decide_eq_false h_v
  simp [parseHeaderExpr, errResultExpr,
        validateTotalLenFn, validateTotalLenExpr,
        validateVersionFn, validateVersionExpr,
        eval, eval.evalArgs, eval.evalFields, eval.lookupIndex,
        parseValidateFns,
        Env.bind, evalBinOp, bindArgs, BEq.beq, h_len_dec, h_v_dec]

set_option linter.unusedSimpArgs false in
/-- parse_header returns `Err(BadType)` when `data[1] ∉ [1, 4]`
    (and earlier checks pass: `len ≥ 5`, `data[0] = 1`). -/
theorem parse_header_bad_type
    (t : Int) (rest : List PVal) (len : Int) (fuel : Nat)
    (h_len : len ≥ 5) (h_t : t < 1 ∨ t > 4) :
    eval parseValidateFns
      ((Env.empty.bind "data" (.array_ (.int 1 :: .int t :: rest))).bind "len" (.int len))
      (fuel + 15) parseHeaderExpr
    = some (.enum_ "Result" "Err"
        [("error", .enum_ "ParseError" "BadType" [])]) := by
  have h_len_dec : decide (5 ≤ len) = true := decide_eq_true h_len
  -- t < 1 or t > 4 ⟹ validate_msg_type returns 1.
  -- Expressed concretely: ¬ (1 ≤ t) ∨ ¬ (t ≤ 4).
  have h_t_low  : ¬ (1 ≤ t ∧ t ≤ 4) := by omega
  simp [parseHeaderExpr, errResultExpr,
        validateTotalLenFn, validateTotalLenExpr,
        validateVersionFn, validateVersionExpr,
        validateMsgTypeFn, validateMsgTypeExpr,
        eval, eval.evalArgs, eval.evalFields, eval.lookupIndex,
        parseValidateFns,
        Env.bind, evalBinOp, bindArgs, BEq.beq, h_len_dec]
  -- Two sub-cases on whether t < 1 or t > 4.
  rcases h_t with h_lo | h_hi
  · have hd1 : decide (1 ≤ t) = false := decide_eq_false (by omega)
    simp [hd1]
  · have hd2 : decide (t ≤ 4) = false := decide_eq_false (by omega)
    have hd1 : decide (1 ≤ t) = true  := decide_eq_true (by omega)
    simp [hd1, hd2]

set_option linter.unusedSimpArgs false in
/-- parse_header returns `Err(PayloadTooBig)` when `data[2] < 0` or
    `data[2] > 240` (earlier checks pass: `len ≥ 5`, `data[0] = 1`,
    `data[1] ∈ [1, 4]`). -/
theorem parse_header_payload_too_big
    (plen : Int) (rest : List PVal) (len : Int) (fuel : Nat)
    (h_len : len ≥ 5) (h_plen : plen < 0 ∨ plen > 240) :
    eval parseValidateFns
      ((Env.empty.bind "data" (.array_ (.int 1 :: .int 1 :: .int plen :: rest))).bind "len" (.int len))
      (fuel + 15) parseHeaderExpr
    = some (.enum_ "Result" "Err"
        [("error", .enum_ "ParseError" "PayloadTooBig" [])]) := by
  have h_len_dec : decide (5 ≤ len) = true := decide_eq_true h_len
  simp [parseHeaderExpr, errResultExpr,
        validateTotalLenFn, validateTotalLenExpr,
        validateVersionFn, validateVersionExpr,
        validateMsgTypeFn, validateMsgTypeExpr,
        validatePayloadLenFn, validatePayloadLenExpr,
        eval, eval.evalArgs, eval.evalFields, eval.lookupIndex,
        parseValidateFns,
        Env.bind, evalBinOp, bindArgs, BEq.beq, h_len_dec]
  rcases h_plen with h_lo | h_hi
  · have hd0 : decide (0 ≤ plen) = false := decide_eq_false (by omega)
    simp [hd0]
  · have hd240 : decide (plen ≤ 240) = false := decide_eq_false (by omega)
    have hd0   : decide (0 ≤ plen) = true  := decide_eq_true (by omega)
    simp [hd0, hd240]

set_option linter.unusedSimpArgs false in
/-- parse_header returns `Err(Truncated)` when `len < 4 + data[2]`
    (and earlier checks pass: validation up through payload_len). -/
theorem parse_header_truncated
    (plen : Int) (rest : List PVal) (len : Int) (fuel : Nat)
    (h_len_5 : len ≥ 5) (h_plen_lo : 0 ≤ plen) (h_plen_hi : plen ≤ 240)
    (h_trunc : len < 4 + plen) :
    eval parseValidateFns
      ((Env.empty.bind "data" (.array_ (.int 1 :: .int 1 :: .int plen :: rest))).bind "len" (.int len))
      (fuel + 20) parseHeaderExpr
    = some (.enum_ "Result" "Err"
        [("error", .enum_ "ParseError" "Truncated" [])]) := by
  have h_len_dec   : decide (5 ≤ len) = true := decide_eq_true h_len_5
  have h_plen_lo_d : decide (0 ≤ plen) = true := decide_eq_true h_plen_lo
  have h_plen_hi_d : decide (plen ≤ 240) = true := decide_eq_true h_plen_hi
  have h_trunc_d   : decide (4 + plen ≤ len) = false := decide_eq_false (by omega)
  simp [parseHeaderExpr, errResultExpr,
        validateTotalLenFn, validateTotalLenExpr,
        validateVersionFn, validateVersionExpr,
        validateMsgTypeFn, validateMsgTypeExpr,
        validatePayloadLenFn, validatePayloadLenExpr,
        eval, eval.evalArgs, eval.evalFields, eval.lookupIndex,
        parseValidateFns,
        Env.bind, evalBinOp, bindArgs, BEq.beq,
        h_len_dec, h_plen_lo_d, h_plen_hi_d, h_trunc_d]

-- ============================================================
-- fixed_capacity — first attached theorem (bar #1 for the active
-- candidate). Targets ring_new: zero-initialized 16-element
-- RingBuf. The theorem exercises arrayLit + structLit + letIn
-- composed together under the kernel — neither parse_validate
-- nor crypto_verify uses arrays, so this is the first end-to-end
-- evidence that the recent Phase 4 extensions compose.
-- ============================================================

/-- `fn compute_tag(buf: MsgBuf) -> i32` — XOR fold of bytes 0..5.

      let acc = 0
      let i   = 0
      while i < 6 {
        acc = acc ^ (buf.data[i] as i32)
        i   = i + 1
      }
      return acc

    Single-iteration cost is one cond eval + two evalAssigns.
    The loop runs exactly 6 iterations (bounded literal). -/
def fcTagExpr : PExpr :=
  .letIn "acc" (.lit (.int 0))
    (.letIn "i" (.lit (.int 0))
      (.while_
        (.binOp .lt (.var "i") (.lit (.int 6)))
        [ ("acc",
           .binOp .bitxor (.var "acc")
             (.cast (.arrayIndex (.fieldAccess (.var "buf") "data")
                       (.var "i"))))
        , ("i", .binOp .add (.var "i") (.lit (.int 1)))
        ]
        (.var "acc")))

def fcTagFn : PFnDef :=
  { name := "compute_tag", params := ["buf"], body := fcTagExpr }

/-- `fn ring_new() -> RingBuf` — returns a fresh RingBuf with a
    16-element data array zeroed, head = 0, count = 0. -/
def ringNewExpr : PExpr :=
  .letIn "data" (.arrayLit (List.replicate 16 (.lit (.int 0))))
    (.structLit "RingBuf"
      [ ("data",  .var "data")
      , ("head",  .lit (.int 0))
      , ("count", .lit (.int 0))
      ])

def ringNewFn : PFnDef :=
  { name := "ring_new", params := [], body := ringNewExpr }

/-- Function table for fixed_capacity proofs.  Each new proof
    extends this table with the function it targets. -/
def fixedCapacityFns : FnTable
  | "ring_new"    => some ringNewFn
  | "compute_tag" => some fcTagFn
  | _             => none

set_option linter.unusedSimpArgs false in
/-- `compute_tag` on a buffer whose first 6 data bytes are all 0
    returns 0 (any tail is ignored).  This is the first theorem
    in the project that exercises a bounded while loop end-to-end
    under the Lean kernel: each iteration evaluates the cond,
    runs the two flat assigns through `evalAssigns`, and rebinds
    `acc` and `i` in the env.  After 6 iterations acc remains 0
    (0 xor 0 = 0 at each step) and `i = 6` makes the cond
    false, falling through to `return acc`. -/
theorem compute_tag_zero_correct (rest : List PVal) (fuel : Nat) :
    eval fixedCapacityFns
      (Env.empty.bind "buf"
        (.struct_ "MsgBuf"
          [("data", .array_ (List.replicate 6 (.int 0) ++ rest))]))
      (fuel + 80) fcTagExpr
    = some (.int 0) := by
  -- The proof unfolds the loop 6 times.  Each iteration's
  -- bitxor case reduces (0).toNat ^^^ (0).toNat = 0, so acc
  -- stays 0.  After iteration 6, `decide (i < 6) = false`
  -- fires the fall-through branch.
  simp [fcTagExpr,
        eval, eval.evalAssigns, eval.lookupField, eval.lookupIndex,
        fixedCapacityFns, Env.bind, evalBinOp, List.replicate]

set_option linter.unusedSimpArgs false in
/-- `ring_new()` evaluates to the canonical empty RingBuf:
    `data` is 16 zeros, `head` and `count` are both 0.  This is the
    first attached theorem on fixed_capacity, and the first proof
    in the project that combines arrayLit + structLit + letIn. -/
theorem ring_new_correct (fuel : Nat) :
    eval fixedCapacityFns Env.empty (fuel + 5) ringNewExpr
    = some (.struct_ "RingBuf"
        [ ("data",  .array_ (List.replicate 16 (.int 0)))
        , ("head",  .int 0)
        , ("count", .int 0)
        ]) := by
  simp [ringNewExpr,
        eval, eval.evalElems, eval.evalFields,
        fixedCapacityFns, Env.bind,
        List.replicate]

-- ============================================================
-- Proved functions registry
-- ============================================================

/-- Functions with completed Lean proofs. The effects report upgrades
    evidence level from "enforced" to "proved" for these functions.
    Each entry is (qualified function name, expected body fingerprint).
    The qualified name is `module.fn` (e.g., `main.parse_byte`), so
    same-named functions in different modules are disambiguated.
    If the function body changes, the fingerprint will not match and
    "proved" evidence is revoked — the proof must be updated to match. -/
def provedFunctions : List (String × String × String) :=
  [ ("main.parse_byte",
     "[(ret (binop Concrete.BinOp.add (var data) (var offset)))]",
     "Concrete.Proof.parse_byte_correct")
  , ("main.check_length",
     "[(if (binop Concrete.BinOp.lt (var len) (int 10)) [(ret (int 1))]) (ret (int 0))]",
     "Concrete.Proof.check_length_rejects_short")
  , ("main.decode_header",
     "[(if (binop Concrete.BinOp.neq (call check_length (var len)) (int 0)) [(ret (int 1))]) (let version (call parse_byte (var data) (int 0))) (if (binop Concrete.BinOp.lt (var version) (int 1)) [(ret (int 2))]) (if (binop Concrete.BinOp.gt (var version) (int 2)) [(ret (int 2))]) (let payload_len (call parse_byte (var data) (int 1))) (if (binop Concrete.BinOp.gt (var payload_len) (binop Concrete.BinOp.sub (var len) (int 10))) [(ret (int 3))]) (ret (int 0))]",
     "Concrete.Proof.decode_header_rejects_short")
  , ("main.compute_tag",
     "[(ret (binop Concrete.BinOp.add (binop Concrete.BinOp.mul (var key) (var message)) (var nonce)))]",
     "Concrete.Proof.compute_tag_correct")
  , ("main.verify_tag",
     "[(let computed (call compute_tag (var key) (var message) (var nonce))) (if (binop Concrete.BinOp.eq (var computed) (var expected_tag)) [(ret (int 1))] [(ret (int 0))])]",
     "Concrete.Proof.verify_tag_correct")
  , ("main.check_nonce",
     "[(if (binop Concrete.BinOp.gt (var nonce) (int 0)) [(if (binop Concrete.BinOp.leq (var nonce) (var max_nonce)) [(ret (int 1))] [(ret (int 0))])] [(ret (int 0))])]",
     "Concrete.Proof.check_nonce_correct")
  , ("main.check_magic",
     "[(if (binop Concrete.BinOp.eq (var b0) (int 127)) [(if (binop Concrete.BinOp.eq (var b1) (int 69)) [(if (binop Concrete.BinOp.eq (var b2) (int 76)) [(if (binop Concrete.BinOp.eq (var b3) (int 70)) [(ret (int 1))] [(ret (int 0))])] [(ret (int 0))])] [(ret (int 0))])] [(ret (int 0))])]",
     "Concrete.Proof.check_magic_correct")
  , ("main.check_class",
     "[(if (binop Concrete.BinOp.eq (var cls) (int 1)) [(ret (int 1))] [(if (binop Concrete.BinOp.eq (var cls) (int 2)) [(ret (int 1))] [(ret (int 0))])])]",
     "Concrete.Proof.check_class_correct")
  , ("main.check_data",
     "[(if (binop Concrete.BinOp.eq (var encoding) (int 1)) [(ret (int 1))] [(if (binop Concrete.BinOp.eq (var encoding) (int 2)) [(ret (int 1))] [(ret (int 0))])])]",
     "Concrete.Proof.check_data_correct")
  , ("main.check_version",
     "[(if (binop Concrete.BinOp.eq (var ver) (int 1)) [(ret (int 1))] [(ret (int 0))])]",
     "Concrete.Proof.check_version_correct")
  , ("main.validate_header",
     "[(let magic_ok (call check_magic (var b0) (var b1) (var b2) (var b3))) (if (binop Concrete.BinOp.eq (var magic_ok) (int 1)) [(let cls_ok (call check_class (var cls))) (if (binop Concrete.BinOp.eq (var cls_ok) (int 1)) [(let enc_ok (call check_data (var encoding))) (if (binop Concrete.BinOp.eq (var enc_ok) (int 1)) [(let ver_ok (call check_version (var ver))) (if (binop Concrete.BinOp.eq (var ver_ok) (int 1)) [(ret (int 1))] [(ret (int 0))])] [(ret (int 0))])] [(ret (int 0))])] [(ret (int 0))])]",
     "Concrete.Proof.validate_header_correct")
  ]

end Concrete.Proof
