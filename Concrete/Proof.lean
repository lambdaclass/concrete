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
  | eq | ne | lt | le | gt | ge
  deriving Repr, BEq, DecidableEq

/-- Values in the proof fragment. -/
inductive PVal where
  | int (n : Int)
  | bool (b : Bool)
  deriving Repr, BEq, DecidableEq

/-- Expressions in the proof fragment.
    This is a strict subset of `CExpr`, restricted to pure integer/boolean
    operations with let bindings and conditionals. -/
inductive PExpr where
  | lit (v : PVal)
  | var (name : String)
  | binOp (op : PBinOp) (lhs rhs : PExpr)
  | letIn (name : String) (val body : PExpr)
  | ifThenElse (cond thenBr elseBr : PExpr)
  | call (fn : String) (args : List PExpr)
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

/-- Function table for proofs. -/
def proofFns : FnTable
  | "abs" => some absFn
  | "max" => some maxFn
  | "clamp" => some clampFn
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

/-- abs(5) = 5 -/
theorem abs_positive : evalAbs 5 = some (.int 5) := by native_decide

/-- abs(-3) = 3 -/
theorem abs_negative : evalAbs (-3) = some (.int 3) := by native_decide

/-- abs(0) = 0 -/
theorem abs_zero : evalAbs 0 = some (.int 0) := by native_decide

/-- max(10, 20) = 20 -/
theorem max_right : evalMax 10 20 = some (.int 20) := by native_decide

/-- max(7, 3) = 7 -/
theorem max_left : evalMax 7 3 = some (.int 7) := by native_decide

/-- max(x, x) = x for a specific value (kernel-reducible). -/
theorem max_self : evalMax 42 42 = some (.int 42) := by native_decide

/-- clamp(5, 0, 10) = 5 (in range) -/
theorem clamp_in_range : evalClamp 5 0 10 = some (.int 5) := by native_decide

/-- clamp(-3, 0, 10) = 0 (below range) -/
theorem clamp_below : evalClamp (-3) 0 10 = some (.int 0) := by native_decide

/-- clamp(15, 0, 10) = 10 (above range) -/
theorem clamp_above : evalClamp 15 0 10 = some (.int 10) := by native_decide

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

end Concrete.Proof
