import ConcreteSpec.Spec.Slice1Syntax
import ConcreteSpec.Diagnostics.Types
import ConcreteSpec.Diagnostics
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Slice1Semantics
import ConcreteSpec.Spec.Borrow
import ConcreteSpec.Spec.Brand

import ConcreteSpec.Proofs.Basics

/-!
# Name independence of the execution boundary

Two elaborated states may have entirely different diagnostics tables. Equal semantic state and
numerically equal operands must give the same result for each actual operation. The numeric
premise is load-bearing: a name-carrying identity cannot prove these statements. The required
local_identity gate independently mutates every dispatch to consult diagnostics and requires the
corresponding proof to fail. The semantic dependency scan closes the other route: adding a name
table to an operation inside Spec. Future codecs are tracked in ISS-XXXX, not covered here.
-/
namespace ConcreteSpec.LocalIdentity
open Core Diagnostics

/-- Numeric identity is complete: there is no hidden spelling component. -/
theorem numeric_identity (x y : Var) : x = y ↔ x.id = y.id := by
  cases x; cases y; constructor <;> intro h
  · exact congrArg Var.id h
  · cases h; rfl

/-- The first binding wins for a local identity. -/
theorem first_binding {α : Type} (x : Var) (a : α) (rest : AList Var α) :
    AList.lookup ((x, a) :: rest) x = some a := by
  rw [AList.lookup_cons, ite_eq_left rfl]

/-- Replacing the diagnostics table preserves `read`, including its semantic result. -/
theorem read_names {s t : Elaborated Env} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (Δ : Decls) (mv : Bool) :
    read Δ s x mv = read Δ t y mv := by
  have hxy := (numeric_identity x y).mpr hx
  cases s; cases t; cases hs; cases hxy; rfl

/-- Replacing the diagnostics table preserves `statement`, including its semantic result. -/
theorem statement_names {s t : Elaborated Env} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (Δ : Decls) (ty : Ty) (e : Expr) :
    statement Δ s x ty e = statement Δ t y ty e := by
  have hxy := (numeric_identity x y).mpr hx
  cases s; cases t; cases hs; cases hxy; rfl

/-- Replacing the diagnostics table preserves `drop`, including its semantic result. -/
theorem drop_names {s t : Elaborated Env} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (Δ : Decls) (ty : Ty) :
    drop Δ s x ty = drop Δ t y ty := by
  have hxy := (numeric_identity x y).mpr hx
  cases s; cases t; cases hs; cases hxy; rfl

/-- Replacing the diagnostics table preserves `borrowRead`, including its semantic result. -/
theorem borrowRead_names {s t : Elaborated Borrow.Ctx} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (out : Borrow.Ctx) :
    borrowRead s x out = borrowRead t y out := by
  have hxy := (numeric_identity x y).mpr hx
  cases s; cases t; cases hs; cases hxy; rfl

/-- Replacing the diagnostics table preserves `borrowWrite`, including its semantic result. -/
theorem borrowWrite_names {s t : Elaborated Borrow.Ctx} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (out : Borrow.Ctx) :
    borrowWrite s x out = borrowWrite t y out := by
  have hxy := (numeric_identity x y).mpr hx
  cases s; cases t; cases hs; cases hxy; rfl

/-- Replacing the diagnostics table preserves `borrowMove`, including its semantic result. -/
theorem borrowMove_names {s t : Elaborated Borrow.Ctx} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (out : Borrow.Ctx) :
    borrowMove s x out = borrowMove t y out := by
  have hxy := (numeric_identity x y).mpr hx
  cases s; cases t; cases hs; cases hxy; rfl

/-- Replacing the diagnostics table preserves `borrowRestore`, including its semantic result. -/
theorem borrowRestore_names {s t : Elaborated Borrow.Ctx} (hs : s.code = t.code)
    (body : Borrow.Ctx) :
    borrowRestore s body = borrowRestore t body := by
  cases s; cases t; cases hs; rfl

/-- Replacing the diagnostics table preserves `borrowRun`, including its semantic result. -/
theorem borrowRun_names {s t : Elaborated (Borrow.Mem × Borrow.Views)} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (fuel : Nat) (ss : List Borrow.VStmt) :
    borrowRun s x fuel ss = borrowRun t y fuel ss := by
  have hxy := (numeric_identity x y).mpr hx
  cases s; cases t; cases hs; cases hxy; rfl

/-- Replacing the diagnostics table preserves `borrowAssign`, including its semantic result. -/
theorem borrowAssign_names {s t : Elaborated (Borrow.Mem × Borrow.Views)} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (fuel i v : Nat) :
    borrowAssign s x fuel i v = borrowAssign t y fuel i v := by
  have hxy := (numeric_identity x y).mpr hx
  cases s; cases t; cases hs; cases hxy; rfl

/-- Replacing the diagnostics table preserves `borrowScope`, including its semantic result. -/
theorem borrowScope_names {s t : Elaborated (Borrow.Mem × Borrow.Views)} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (fuel : Nat) (m : Bool) (v : Var) (a b : Nat) (ss : List Borrow.VStmt) :
    borrowScope s x fuel m v a b ss = borrowScope t y fuel m v a b ss := by
  have hxy := (numeric_identity x y).mpr hx
  cases s; cases t; cases hs; cases hxy; rfl

/-- Replacing the diagnostics table preserves `brandUse`, including its semantic result. -/
theorem brandUse_names {s t : Elaborated Brand.State} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (σ : Nat → Bool) (fuel : Nat) (a : Var) :
    brandUse s x σ fuel a = brandUse t y σ fuel a := by
  have hxy := (numeric_identity x y).mpr hx
  cases s; cases t; cases hs; cases hxy; rfl

/-- Replacing the diagnostics table preserves `brandFree`, including its semantic result. -/
theorem brandFree_names {s t : Elaborated Brand.State} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (σ : Nat → Bool) (fuel : Nat) (a : Var) :
    brandFree s x σ fuel a = brandFree t y σ fuel a := by
  have hxy := (numeric_identity x y).mpr hx
  cases s; cases t; cases hs; cases hxy; rfl

/-- Replacing the diagnostics table preserves `brandRelease`, including its semantic result. -/
theorem brandRelease_names {s t : Elaborated Brand.State} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (σ : Nat → Bool) (fuel : Nat) :
    brandRelease s x σ fuel = brandRelease t y σ fuel := by
  have hxy := (numeric_identity x y).mpr hx
  cases s; cases t; cases hs; cases hxy; rfl

/-- Replacing the diagnostics table preserves `brandReset`, including its semantic result. -/
theorem brandReset_names {s t : Elaborated Brand.State} (hs : s.code = t.code)
    (x y : Var) (hx : x.id = y.id) (σ : Nat → Bool) (fuel : Nat) (a : Var) (β : Nat) :
    brandReset s x σ fuel a β = brandReset t y σ fuel a β := by
  have hxy := (numeric_identity x y).mpr hx
  cases s; cases t; cases hs; cases hxy; rfl

/-- Replacing the diagnostics table preserves `brandGhost`, including its semantic result. -/
theorem brandGhost_names {s t : Elaborated Brand.State} (hs : s.code = t.code)
    (σ : Nat → Bool) (fuel k : Nat) (ss : List Brand.Stmt) :
    brandGhost s σ fuel k ss = brandGhost t σ fuel k ss := by
  cases s; cases t; cases hs; rfl

/-- Replacing the diagnostics table preserves `brandErased`, including its semantic result. -/
theorem brandErased_names {s t : Elaborated (Core.AList Var Brand.EVal)} (hs : s.code = t.code)
    (σ : Nat → Bool) (fuel k : Nat) (ss : List Brand.Stmt) :
    brandErased s σ fuel k ss = brandErased t σ fuel k ss := by
  cases s; cases t; cases hs; rfl


/-- Diagnostic table replacement preserves the complete slice-1 evaluator. -/
theorem sliceRun_names {s t : Elaborated Env} (hs : s.code = t.code)
    (Δ : Decls) (fuel : Nat) (ss : List Stmt) (ret : Atom) :
    sliceRun s Δ fuel ss ret = sliceRun t Δ fuel ss ret := by
  cases s; cases t; cases hs; rfl

/-- Diagnostic table replacement preserves every range-view execution. -/
theorem borrowExecute_names {s t : Elaborated (Borrow.Mem × Borrow.Views)} (hs : s.code = t.code)
    (fuel : Nat) (ss : List Borrow.VStmt) :
    borrowExecute s fuel ss = borrowExecute t fuel ss := by
  cases s; cases t; cases hs; rfl

/-- Diagnostic table replacement preserves every static Brand judgement. -/
theorem brandCheck_names {s t : Elaborated Brand.Ctx} (hs : s.code = t.code)
    (brands : List Nat) (ss : List Brand.Stmt) (out : Brand.Ctx) :
    brandCheck s brands ss out = brandCheck t brands ss out := by
  cases s; cases t; cases hs; rfl

end ConcreteSpec.LocalIdentity
