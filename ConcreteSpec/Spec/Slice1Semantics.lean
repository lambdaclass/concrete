import ConcreteSpec.Spec.Slice1Syntax

/-!
# Slice-1 values, evaluator and value typing

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec

/-! ### Semantics -/

/-- A value: a scalar, or a struct with its field values. -/
inductive Val where
  | scalar (v : Scalar)
  | struct (s : Name) (fields : List (Name × Scalar))
  deriving DecidableEq, Repr

/-- A binding's slot holds a value, or has been moved out of. Slots are instrumentation: the
runtime representation erases them, and a moved slot exists to make a read after a move
observable in the model. -/
inductive Slot where
  | val (v : Val)
  | moved
  deriving DecidableEq, Repr

/-- The environment, newest binding first. -/
abbrev Env := List (Var × Slot)

/-- The defined traps of slice 1 (ARITH-3). -/
inductive Trap where
  | arithmeticOverflow
  deriving DecidableEq, Repr

/-- Why execution got stuck. Safe code never gets stuck (STMT-1); `checked_never_stuck` proves it
for slice 1. `wrongMode` is a `copy` of a linear value or a `move` of a Copy one. -/
inductive Stuck where
  | movedRead
  | unbound
  | wrongMode
  | typeError
  deriving DecidableEq, Repr

/-- The result of one evaluation step. -/
inductive R (α : Type) where
  | ok (a : α)
  | trap (k : Trap)
  | stuck (w : Stuck)
  deriving Repr

/-- How a run ends (STMT-1): exit with a status, abort at a defined trap (STMT-2), stuck, or the
evaluator's fuel ran out. Running out of fuel is inconclusive; it is not divergence. -/
inductive Outcome where
  | exit (code : Nat)
  | trap (k : Trap)
  | stuck (w : Stuck)
  | outOfFuel
  deriving DecidableEq, Repr

/-- The slot of `x`. -/
def envFind : Env → Var → Option Slot
  | [], _ => none
  | (y, s) :: env, x => if y = x then some s else envFind env x

/-- Mark `x` moved. -/
def envMove : Env → Var → Env
  | [], _ => []
  | (y, s) :: env, x => if y = x then (y, .moved) :: env else (y, s) :: envMove env x

/-- Whether a value is Copy (OWN-D1, TYPE-14). -/
def Val.isCopy (Δ : Decls) : Val → Bool
  | .scalar _ => true
  | .struct s _ => match findDecl Δ s with
    | some d => d.copy
    | none => false

/-- Read a local: `copy` (`mv = false`) keeps a Copy value, `move` (`mv = true`) moves a linear
value out (Q9.1). A moved slot is stuck, and so is a read in the wrong mode. -/
def readVar (Δ : Decls) (env : Env) (x : Var) (mv : Bool) : R (Val × Env) :=
  match envFind env x with
  | none => .stuck .unbound
  | some .moved => .stuck .movedRead
  | some (.val v) =>
    if v.isCopy Δ = !mv then .ok (v, if mv then envMove env x else env) else .stuck .wrongMode

/-- Evaluate an atom that must be a scalar. -/
def evalAtom (Δ : Decls) (env : Env) : Atom → R (Scalar × Env)
  | .int t n => .ok (.int t n, env)
  | .bool b => .ok (.bool b, env)
  | .copy x => match readVar Δ env x false with
    | .ok (.scalar v, env') => .ok (v, env')
    | .ok _ => .stuck .typeError
    | .trap k => .trap k
    | .stuck w => .stuck w
  | .move x => match readVar Δ env x true with
    | .ok (.scalar v, env') => .ok (v, env')
    | .ok _ => .stuck .typeError
    | .trap k => .trap k
    | .stuck w => .stuck w

/-- Evaluate struct-literal fields in the order written (STMT-4). -/
def evalFields (Δ : Decls) : Env → List (Name × Atom) → R (List (Name × Scalar) × Env)
  | env, [] => .ok ([], env)
  | env, (f, a) :: rest => match evalAtom Δ env a with
    | .ok (v, env₁) => match evalFields Δ env₁ rest with
      | .ok (vs, env₂) => .ok ((f, v) :: vs, env₂)
      | .trap k => .trap k
      | .stuck w => .stuck w
    | .trap k => .trap k
    | .stuck w => .stuck w

/-- Checked addition (ARITH-3): the mathematical sum if it fits the operand type, else the
`arithmeticOverflow` trap. -/
def checkedAdd (t : IntTy) (a b : Int) : R Val :=
  if t.inRange (a + b) then .ok (.scalar (.int t (a + b))) else .trap .arithmeticOverflow

/-- Evaluate an expression, left operand first (STMT-4). -/
def evalExpr (Δ : Decls) : Env → Expr → R (Val × Env)
  | env, .atom (.int t n) => .ok (.scalar (.int t n), env)
  | env, .atom (.bool b) => .ok (.scalar (.bool b), env)
  | env, .atom (.copy x) => readVar Δ env x false
  | env, .atom (.move x) => readVar Δ env x true
  | env, .add l r => match evalAtom Δ env l with
    | .ok (.int t a, env₁) => match evalAtom Δ env₁ r with
      | .ok (.int t' b, env₂) =>
        if t = t' then match checkedAdd t a b with
          | .ok v => .ok (v, env₂)
          | .trap k => .trap k
          | .stuck w => .stuck w
        else .stuck .typeError
      | .ok _ => .stuck .typeError
      | .trap k => .trap k
      | .stuck w => .stuck w
    | .ok _ => .stuck .typeError
    | .trap k => .trap k
    | .stuck w => .stuck w
  | env, .mk s fs => match evalFields Δ env fs with
    | .ok (vs, env₁) => .ok (.struct s vs, env₁)
    | .trap k => .trap k
    | .stuck w => .stuck w

/-- Bind the named fields of a destructured struct value, in pattern order. -/
def destructure : List (Name × Binder) → List (Name × Scalar) → Env → R Env
  | [], _, env => .ok env
  | (_, .wild) :: rest, vs, env => destructure rest vs env
  | (f, .name x) :: rest, vs, env => match lookupF f vs with
    | some v => destructure rest vs ((x, .val (.scalar v)) :: env)
    | none => .stuck .typeError

/-- Execute one statement. A destructuring `let` consumes its scrutinee and binds its fields. -/
def execStmt (Δ : Decls) (env : Env) : Stmt → R Env
  | .let_ (.name x) _ e => match evalExpr Δ env e with
    | .ok (v, env₁) => .ok ((x, .val v) :: env₁)
    | .trap k => .trap k
    | .stuck w => .stuck w
  | .let_ .wild _ e => match evalExpr Δ env e with
    | .ok (_, env₁) => .ok env₁
    | .trap k => .trap k
    | .stuck w => .stuck w
  | .letStruct s pat e => match evalExpr Δ env e with
    | .ok (.struct s' vs, env₁) => if s' = s then destructure pat vs env₁ else .stuck .typeError
    | .ok _ => .stuck .typeError
    | .trap k => .trap k
    | .stuck w => .stuck w

/-- The outcome of `return a`: the `u8` value is the exit status (STMT-7). -/
def retOutcome (Δ : Decls) (env : Env) (r : Atom) : Outcome :=
  match evalAtom Δ env r with
  | .ok (.int .u8 k, _) => .exit k.toNat
  | .ok _ => .stuck .typeError
  | .trap k => .trap k
  | .stuck w => .stuck w

/-- The fuel-indexed evaluator: each statement and the final `return` costs one unit of fuel. -/
def runBody (Δ : Decls) : Nat → Env → List Stmt → Atom → Outcome
  | 0, _, _, _ => .outOfFuel
  | _ + 1, env, [], r => retOutcome Δ env r
  | n + 1, env, s :: ss, r => match execStmt Δ env s with
    | .ok env' => runBody Δ n env' ss r
    | .trap k => .trap k
    | .stuck w => .stuck w

/-- Run a program with the given fuel, from an empty environment. -/
def run (p : Program) (fuel : Nat) : Outcome := runBody p.decls fuel [] p.body p.ret

/-- The fuel-free semantics: the outcome a body reaches. -/
inductive Exec (Δ : Decls) : Env → List Stmt → Atom → Outcome → Prop where
  | ret {env r} : Exec Δ env [] r (retOutcome Δ env r)
  | step {env env' s ss r o} : execStmt Δ env s = .ok env' → Exec Δ env' ss r o →
      Exec Δ env (s :: ss) r o
  | trap {env s ss r k} : execStmt Δ env s = .trap k → Exec Δ env (s :: ss) r (.trap k)
  | stuck {env s ss r w} : execStmt Δ env s = .stuck w → Exec Δ env (s :: ss) r (.stuck w)

/-! ### Value typing

What it means for a run-time value to have a type. The safety theorems (`checked_never_stuck`,
`checked_exits`, in `Proofs/Slice1.lean`) are stated with it. -/

/-- A scalar value has a scalar type: an integer of its type and in its range, or a `bool`. -/
inductive SVTy : Scalar → STy → Prop where
  | int {t n} : t.inRange n = true → SVTy (.int t n) (.int t)
  | bool {b} : SVTy (.bool b) .bool

/-- A value has a type: a scalar of its scalar type, or a struct value of a declared struct with
a well-typed value for every declared field. -/
inductive VTy (Δ : Decls) : Val → Ty → Prop where
  | scalar {v st} : SVTy v st → VTy Δ (.scalar v) (.scalar st)
  | struct {s d vs} : findDecl Δ s = some d →
      (∀ f st, lookupF f d.fields = some st → ∃ v, lookupF f vs = some v ∧ SVTy v st) →
      VTy Δ (.struct s vs) (.struct s)

end ConcreteSpec
