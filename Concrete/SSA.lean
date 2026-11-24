import Concrete.Memory
import Concrete.BIR

namespace Concrete.SSA
open BIR

private abbrev Env := List (ConcreteSpec.Var × Operand)

private structure State where
  nextReg : Nat := 0
  env : Env := []
  params : List (Reg × Ty) := []
  insts : List Inst := []
  blocks : Array Block := #[]
  sites : Array SiteEntry := #[]

private def operand (env : Env) : Core.Atom → Except Refusal Operand
  | .lit n =>
    if n < 256 then .ok (.imm .u8 n)
    else .error (.internal "out-of-range operand reached SSA")
  | .var x =>
    match env.lookup x with
    | some value => .ok value
    | none => .error (.internal "unbound operand reached SSA")

private def riskyAdd (path : String) (s : State) (x : ConcreteSpec.Var) (a b : Operand)
    (origin : Elab.SourceSite) : State := Id.run do
  let span := origin.operator
  let site := s.sites.size
  let trapBlock := s.blocks.size + 1
  let dest := s.nextReg
  let flag := dest + 1
  let mut next := flag + 1
  let mut args := []
  let mut params := []
  let mut env := []
  for (name, value) in (x, .reg dest) :: s.env do
    match value with
    | .imm .. => env := (name, value) :: env
    | .reg _ =>
      args := value :: args
      params := (next, .int .u8) :: params
      env := (name, .reg next) :: env
      next := next + 1
  let guard : Block :=
    ⟨s.params, (.binOvf .add .u8 dest flag a b :: s.insts).reverse,
      .condbr flag trapBlock [] (trapBlock + 1) args.reverse⟩
  let trap : Block := ⟨[], [], .trap site⟩
  let entry : SiteEntry :=
    ⟨.arithmeticOverflow, ⟨site⟩, ⟨span.start, span.stop⟩, ⟨path, span.line, span.column⟩⟩
  -- Lookup, like argument-to-parameter pairing, follows the original newest-first order.
  return ⟨next, env.reverse, params.reverse, [], (s.blocks.push guard).push trap, s.sites.push entry⟩

private def assign (path : String) (s : State) (stmt : Memory.Store) :
    Except Refusal State := do
  unless stmt.dest.layout == Memory.u8Layout do
    throw (.internal "unexpected slot layout in slice-0 SSA")
  let slot := stmt.dest.id
  match stmt.value with
  | .copy a =>
    let value ← operand s.env a
    return { s with env := (slot, value) :: s.env }
  | .add a b origin discharged =>
    let lhs ← operand s.env a
    let rhs ← operand s.env b
    if discharged.isSome then
      let dest := s.nextReg
      return { s with nextReg := dest + 1, env := (slot, .reg dest) :: s.env,
                      insts := .bin .add .u8 dest lhs rhs :: s.insts }
    else return riskyAdd path s slot lhs rhs origin

private def validate (bir : BIR.Program) : Except Refusal BIR.Program :=
  if BIR.wf bir then .ok bir
  else .error (.internal "SSA produced ill-formed BIR")

def lower (path : String) (p : Memory.Program) : Except Refusal BIR.Program := do
  let s ← p.stores.foldlM (assign path) {}
  let result ← operand s.env p.result
  let bir : BIR.Program :=
    ⟨(s.blocks.push ⟨s.params, s.insts.reverse, .ret result⟩).toList, s.sites.toList⟩
  validate bir

theorem lower_wf {path : String} {p : Memory.Program} {bir : BIR.Program}
    (h : lower path p = .ok bir) : BIR.WF bir := by
  cases hs : p.stores.foldlM (assign path) {} with
  | error e => simp [lower, hs, bind, Except.bind] at h
  | ok s =>
    cases hr : operand s.env p.result with
    | error e => simp [lower, hs, hr, bind, Except.bind] at h
    | ok result =>
      simp only [lower, hs, hr, bind, Except.bind] at h
      unfold validate at h
      split at h
      · cases h
        assumption
      · contradiction

end Concrete.SSA
