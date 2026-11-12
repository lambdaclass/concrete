import ConcreteSpec.Diagnostics.Types
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Slice1Semantics
import ConcreteSpec.Spec.Borrow
import ConcreteSpec.Spec.Brand

/-!
# Execution at the diagnostics boundary

An elaborated state owns its spellings. These entry points dispatch to the actual semantic
operations, consuming only its code. They live outside Spec: no semantic rule may read diagnostics.
`Proofs/LocalIdentity` proves table replacement and numeric identity for each entry point.
-/
namespace ConcreteSpec.Diagnostics

/-- Dispatch `read` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def read (Δ : Decls) (s : Elaborated Env) (x : Var) (mv : Bool) : R (Val × Env) :=
  readVar Δ s.code x mv

/-- Dispatch `statement` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def statement (Δ : Decls) (s : Elaborated Env) (x : Var) (ty : Ty) (e : Expr) : R Env :=
  execStmt Δ s.code (.let_ (.name x) ty e)

/-- Dispatch `drop` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def drop (Δ : Decls) (s : Elaborated Env) (x : Var) (ty : Ty) : R Env :=
  execStmt Δ s.code (.let_ .wild ty (.atom (.copy x)))

/-- Dispatch `borrowRead` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def borrowRead (s : Elaborated Borrow.Ctx) (x : Var) (out : Borrow.Ctx) : Prop :=
  Borrow.Ok s.code [.read x] out

/-- Dispatch `borrowWrite` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def borrowWrite (s : Elaborated Borrow.Ctx) (x : Var) (out : Borrow.Ctx) : Prop :=
  Borrow.Ok s.code [.write x] out

/-- Dispatch `borrowMove` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def borrowMove (s : Elaborated Borrow.Ctx) (x : Var) (out : Borrow.Ctx) : Prop :=
  Borrow.Ok s.code [.move x] out

/-- Dispatch `borrowRestore` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def borrowRestore (s : Elaborated Borrow.Ctx) (body : Borrow.Ctx) : Borrow.Ctx :=
  Borrow.restore s.code body

/-- Dispatch `borrowRun` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def borrowRun (s : Elaborated (Borrow.Mem × Borrow.Views)) (x : Var) (fuel : Nat) (ss : List Borrow.VStmt) : Borrow.VR :=
  Borrow.vrun fuel s.code.1 s.code.2 [] (.getS x 0 :: ss)

/-- Dispatch `borrowAssign` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def borrowAssign (s : Elaborated (Borrow.Mem × Borrow.Views)) (x : Var) (fuel i v : Nat) : Borrow.VR :=
  Borrow.vrun fuel s.code.1 s.code.2 [] [.setS x i v]

/-- Dispatch `borrowScope` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def borrowScope (s : Elaborated (Borrow.Mem × Borrow.Views)) (x : Var) (fuel : Nat) (m : Bool) (v : Var) (a b : Nat) (ss : List Borrow.VStmt) : Borrow.VR :=
  Borrow.vrun fuel s.code.1 s.code.2 [] [.range m x v a b ss none]

/-- Dispatch `brandUse` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def brandUse (s : Elaborated Brand.State) (x : Var) (σ : Nat → Bool) (fuel : Nat) (a : Var) : Brand.GR :=
  Brand.ghost σ fuel 0 s.code [.use x a]

/-- Dispatch `brandFree` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def brandFree (s : Elaborated Brand.State) (x : Var) (σ : Nat → Bool) (fuel : Nat) (a : Var) : Brand.GR :=
  Brand.ghost σ fuel 0 s.code [.free x a]

/-- Dispatch `brandRelease` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def brandRelease (s : Elaborated Brand.State) (x : Var) (σ : Nat → Bool) (fuel : Nat) : Brand.GR :=
  Brand.ghost σ fuel 0 s.code [.release x]

/-- Dispatch `brandReset` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def brandReset (s : Elaborated Brand.State) (x : Var) (σ : Nat → Bool) (fuel : Nat) (a : Var) (β : Nat) : Brand.GR :=
  Brand.ghost σ fuel 0 s.code [.reset a x β]

/-- Dispatch `brandGhost` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def brandGhost (s : Elaborated Brand.State) (σ : Nat → Bool) (fuel k : Nat) (ss : List Brand.Stmt) : Brand.GR :=
  Brand.ghost σ fuel k s.code ss

/-- Dispatch `brandErased` on an elaborated state; diagnostic spellings are not semantic inputs. -/
def brandErased (s : Elaborated (Core.AList Var Brand.EVal)) (σ : Nat → Bool) (fuel k : Nat) (ss : List Brand.Stmt) : Brand.ER :=
  Brand.erased σ fuel k s.code ss

/-- Execute an arbitrary slice-1 body in its owning diagnostics scope. -/
def sliceRun (s : Elaborated Env) (Δ : Decls) (fuel : Nat) (ss : List Stmt) (ret : Atom) : Outcome :=
  runBody Δ fuel s.code ss ret

/-- Execute arbitrary range-view operations, including writes through views and scope exit. -/
def borrowExecute (s : Elaborated (Borrow.Mem × Borrow.Views)) (fuel : Nat) (ss : List Borrow.VStmt) : Borrow.VR :=
  Borrow.vrun fuel s.code.1 s.code.2 [] ss

/-- Check an arbitrary Brand statement list, including each consuming operation. -/
def brandCheck (s : Elaborated Brand.Ctx) (brands : List Nat) (ss : List Brand.Stmt) (out : Brand.Ctx) : Prop :=
  Brand.Ok brands s.code ss out

end ConcreteSpec.Diagnostics
