import Concrete.Elab
import Concrete.MIR
import Concrete.Refusal
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Spec.Slice0
import ConcreteSpec.Spec.Slice1Semantics
import ConcreteSpec.Spec.Slice1Syntax
import ConcreteSpec.Spec.Surface0
import Concrete.Memory
import Concrete.BIR
import Std.Data.HashMap.Lemmas

namespace Concrete.SSA
open BIR

private abbrev Names := Std.HashMap Nat Operand

/-- One ordered list of slots per checked-add success edge, in block order. -/
abbrev Witness := List (List ConcreteSpec.Var)

private def overflowSite (path : String) (site : Nat) (origin : Elab.SourceSite) : SiteEntry :=
  let span := origin.operator
  ⟨.arithmeticOverflow, ⟨site⟩, ⟨span.start, span.stop⟩, ⟨path, span.line, span.column⟩⟩

private structure State where
  nextReg : Nat := 0
  env : Names := {}
  slots : List ConcreteSpec.Var := []
  params : List (Reg × Ty) := []
  insts : List Inst := []
  blocks : Array Block := #[]
  sites : Array SiteEntry := #[]
  edges : Witness := []

private def operand (names : Names) : Core.Atom → Except Refusal Operand
  | .lit n =>
    if n < 256 then .ok (.imm .u8 n)
    else .error (.internal "out-of-range operand reached SSA")
  | .var x =>
    match names[x.id]? with
    | some value => .ok value
    | none => .error (.internal "unbound operand reached SSA")

private def State.bindSlot (s : State) (x : ConcreteSpec.Var) (op : Operand) : State :=
  let slots := if s.env.contains x.id then s.slots else x :: s.slots
  { s with env := s.env.insert x.id op, slots }

private def riskyAdd (path : String) (s : State) (x : ConcreteSpec.Var) (a b : Operand)
    (origin : Elab.SourceSite) : Except Refusal State := do
  let site := s.sites.size
  let trapBlock := s.blocks.size + 1
  let dest := s.nextReg
  let flag := dest + 1
  let mut next := flag + 1
  let mut args := []
  let mut params := []
  let s := s.bindSlot x (.reg dest)
  let mut env : Names := {}
  for name in s.slots do
    let some op := s.env[name.id]? | throw (.internal "missing SSA slot at continuation")
    match op with
    | .imm .. => env := env.insert name.id op
    | .reg _ =>
      args := op :: args
      params := (next, .int .u8) :: params
      env := env.insert name.id (.reg next)
      next := next + 1
  let guard : Block :=
    ⟨s.params, (.binOvf .add .u8 dest flag a b :: s.insts).reverse,
      .condbr flag trapBlock [] (trapBlock + 1) args.reverse⟩
  let trap : Block := ⟨[], [], .trap site⟩
  let entry := overflowSite path site origin
  return { s with
    nextReg := next, env := env, params := params.reverse, insts := [],
    blocks := (s.blocks.push guard).push trap, sites := s.sites.push entry,
    edges := s.slots :: s.edges }

private def assign (path : String) (s : State) (stmt : Memory.Store) :
    Except Refusal State := do
  unless stmt.dest.layout == Memory.u8Layout do
    throw (.internal "unexpected slot layout in slice-0 SSA")
  let slot := stmt.dest.id
  match stmt.value with
  | .copy a =>
    let value ← operand s.env a
    return s.bindSlot slot value
  | .add a b origin discharged =>
    let lhs ← operand s.env a
    let rhs ← operand s.env b
    if discharged.isSome then
      let dest := s.nextReg
      let s := s.bindSlot slot (.reg dest)
      return { s with
        nextReg := dest + 1,
        insts := .bin .add .u8 dest lhs rhs :: s.insts }
    else riskyAdd path s slot lhs rhs origin

private def RelatedOutcome (p : BIR.Program) (source : ConcreteSpec.Outcome) : BIR.Outcome → Prop
  | .exited n => source = .exit n
  | .trapped site => ∃ entry, p.sites[site]? = some entry ∧ source = .trap entry.kind
  | _ => False

/-- The SSA contract observes the return value or the kind recorded at the taken trap site. -/
def Preserves (mem : Memory.Program) (bir : BIR.Program) : Prop :=
  RelatedOutcome bir (Memory.run mem) (BIR.run bir (bir.blocks.length + 1))

/-! ## Local simulation

The source store and the lowering environment use newest-first lookup. Register values are
read from the current BIR execution environment.
-/

private def Represents (slots : List (ConcreteSpec.Var × Int)) (names : Names)
    (regs : BIR.Env) : Prop :=
  ∀ x op, names[x.id]? = some op →
    ∃ n, slots.lookup x = some n ∧ BIR.evalOp regs op = some (.int .u8 n)

private theorem operand_correct {slots : List (ConcreteSpec.Var × Int)} {names : Names}
    {regs : BIR.Env} (hrep : Represents slots names regs) {a : Core.Atom} {op : Operand}
    (h : operand names a = .ok op) :
    ∃ n, MIR.value slots a = .ok n ∧ BIR.evalOp regs op = some (.int .u8 n) := by
  cases a with
  | lit n =>
    by_cases hr : n < 256
    · simp [operand, hr] at h
      subst op
      have hRange : ConcreteSpec.IntTy.u8.inRange (n : Int) = true := by
        simp [ConcreteSpec.IntTy.inRange, ConcreteSpec.IntTy.min, ConcreteSpec.IntTy.max,
          ConcreteSpec.IntTy.signed, ConcreteSpec.IntTy.bits]
        omega
      exact ⟨n, rfl, by simp [BIR.evalOp, hRange]⟩
    · simp [operand, hr] at h
  | var x =>
    cases hv : names[x.id]? with
    | none => simp [operand, hv] at h
    | some v =>
      simp [operand, hv] at h
      subst op
      obtain ⟨n, hs, he⟩ := hrep x v hv
      exact ⟨n, by simp [MIR.value, hs], he⟩

private theorem represents_cons {slots : List (ConcreteSpec.Var × Int)} {names : Names}
    {regs : BIR.Env} (hrep : Represents slots names regs) (x : ConcreteSpec.Var)
    {op : Operand} {n : Int} (he : BIR.evalOp regs op = some (.int .u8 n)) :
    Represents ((x, n) :: slots) (names.insert x.id op) regs := by
  intro y v hv
  rw [Std.HashMap.getElem?_insert] at hv
  by_cases hxy : y = x
  · subst y
    simp at hv
    subst v
    exact ⟨n, by simp, he⟩
  · have hid : (x.id == y.id) = false := by
      apply beq_false_of_ne
      cases x
      cases y
      simpa using Ne.symm hxy
    simp only [hid, Bool.false_eq_true, ↓reduceIte] at hv
    obtain ⟨m, hs, hm⟩ := hrep y v hv
    have hne : (y == x) = false := beq_false_of_ne hxy
    exact ⟨m, by simpa [List.lookup, hne] using hs, hm⟩

private theorem discharged_add_correct {slots : List (ConcreteSpec.Var × Int)} {regs : BIR.Env}
    {lhs rhs : Core.Atom} {a b : Operand} {x y : Int}
    (ha : MIR.value slots lhs = .ok x) (hb : MIR.value slots rhs = .ok y)
    (hea : BIR.evalOp regs a = some (.int .u8 x)) (heb : BIR.evalOp regs b = some (.int .u8 y))
    (origin : Elab.SourceSite) (evidence : Int) (dest : Reg) :
    ∃ n, MIR.evalOp slots (.add lhs rhs origin (some evidence)) = .ok n ∧
      BIR.execInst regs (.bin .add .u8 dest a b) = some ((dest, .int .u8 n) :: regs) := by
  refine ⟨BIR.wrap .u8 (x + y), ?_, ?_⟩
  · simp [MIR.evalOp, ha, hb, bind, Except.bind, Pure.pure, Except.pure, BIR.ScalarBridge.wrap,
      BIR.ScalarBridge.toCore]
  · simp [BIR.execInst, BIR.intArgs, hea, heb, BIR.BinOp.exact]

private theorem checked_add_correct {slots : List (ConcreteSpec.Var × Int)} {regs : BIR.Env}
    {lhs rhs : Core.Atom} {a b : Operand} {x y : Int}
    (ha : MIR.value slots lhs = .ok x) (hb : MIR.value slots rhs = .ok y)
    (hea : BIR.evalOp regs a = some (.int .u8 x)) (heb : BIR.evalOp regs b = some (.int .u8 y))
    (origin : Elab.SourceSite) (dest flag : Reg) :
    (∃ n, MIR.evalOp slots (.add lhs rhs origin none) = .ok n ∧
      BIR.execInst regs (.binOvf .add .u8 dest flag a b) =
        some ((flag, .bool false) :: (dest, .int .u8 n) :: regs)) ∨
    (MIR.evalOp slots (.add lhs rhs origin none) = .error (.trap .arithmeticOverflow) ∧
      BIR.execInst regs (.binOvf .add .u8 dest flag a b) =
        some ((flag, .bool true) :: (dest, .int .u8 (BIR.wrap .u8 (x + y))) :: regs)) := by
  cases hr : ConcreteSpec.IntTy.u8.inRange (x + y) with
  | false =>
    right
    constructor
    · simp [MIR.evalOp, ha, hb, bind, Except.bind, ConcreteSpec.checkedAdd, hr]
      rfl
    · simp [BIR.execInst, BIR.intArgs, hea, heb, BIR.BinOp.exact, hr]
  | true =>
    left
    refine ⟨x + y, ?_, ?_⟩
    · simp [MIR.evalOp, ha, hb, bind, Except.bind, Pure.pure, Except.pure, ConcreteSpec.checkedAdd, hr]
    · simp [BIR.execInst, BIR.intArgs, hea, heb, BIR.BinOp.exact, hr, BIR.wrap_of_inRange _ _ hr]


private def Scoped (names : Names) (next : Nat) : Prop :=
  ∀ (x : Nat) r, names[x]? = some (Operand.reg r) → r < next

private def Above (names : Names) (first : Nat) : Prop :=
  ∀ (x : Nat) r, names[x]? = some (Operand.reg r) → first ≤ r

private theorem scoped_mono {names : Names} {first next : Nat}
    (h : Scoped names first) (hle : first ≤ next) : Scoped names next :=
  fun x r hr => Nat.lt_of_lt_of_le (h x r hr) hle

private theorem scoped_insert {names : Names} {next x : Nat} {op : Operand}
    (h : Scoped names next)
    (hop : ∀ r, op = .reg r → r < next) : Scoped (names.insert x op) next := by
  intro y r hr
  rw [Std.HashMap.getElem?_insert] at hr
  split at hr
  · exact hop r (Option.some.inj hr)
  · exact h y r hr

private theorem above_insert {names : Names} {first x : Nat} {op : Operand}
    (h : Above names first)
    (hop : ∀ r, op = .reg r → first ≤ r) : Above (names.insert x op) first := by
  intro y r hr
  rw [Std.HashMap.getElem?_insert] at hr
  split at hr
  · exact hop r (Option.some.inj hr)
  · exact h y r hr

private theorem represents_fresh_reg {slots : List (ConcreteSpec.Var × Int)} {names : Names}
    {regs : BIR.Env} (hrep : Represents slots names regs) (dest : Reg) (v : Val)
    (hf : ∀ (x : Nat), names[x]? ≠ some (Operand.reg dest)) : Represents slots names ((dest, v) :: regs) := by
  intro x op hx
  obtain ⟨m, hs, he⟩ := hrep x op hx
  refine ⟨m, hs, ?_⟩
  cases op with
  | imm => exact he
  | reg r =>
    have hne : dest ≠ r := by
      intro eq
      subst r
      exact hf x.id hx
    simpa [BIR.evalOp, ConcreteSpec.Core.AList.lookup, hne] using he

private theorem represents_insert_existing {slots : List (ConcreteSpec.Var × Int)}
    {names : Names} {regs : BIR.Env} (hrep : Represents slots names regs)
    {x : ConcreteSpec.Var} {op : Operand} {n : Int} (hs : slots.lookup x = some n)
    (hv : BIR.evalOp regs op = some (.int .u8 n)) :
    Represents slots (names.insert x.id op) regs := by
  intro y o ho
  rw [Std.HashMap.getElem?_insert] at ho
  split at ho
  · rename_i he
    have hxy : x = y := by
      have hId := eq_of_beq he
      cases x
      cases y
      cases hId
      rfl
    cases hxy
    cases ho
    exact ⟨n, hs, hv⟩
  · exact hrep y o ho

private theorem operand_scoped {names : Names} {next : Nat} {a : Core.Atom} {r : Reg}
    (hs : Scoped names next) (ha : operand names a = .ok (.reg r)) : r < next := by
  cases a with
  | lit n => by_cases h : n < 256 <;> simp [operand, h] at ha
  | var x =>
    cases h : names[x.id]? with
    | none => simp [operand, h] at ha
    | some op =>
      simp [operand, h] at ha
      subst op
      exact hs x.id r h

private theorem assign_copy_correct {slots : List (ConcreteSpec.Var × Int)} {names : Names}
    {regs : BIR.Env} {a : Core.Atom} {op : Operand} (x : ConcreteSpec.Var)
    (hrep : Represents slots names regs) (ha : operand names a = .ok op) :
    ∃ n, MIR.evalOp slots (.copy a) = .ok n ∧ Represents ((x, n) :: slots) (names.insert x.id op) regs := by
  obtain ⟨n, hn, he⟩ := operand_correct hrep ha
  exact ⟨n, hn, represents_cons hrep x he⟩

private theorem assign_discharged_correct {slots : List (ConcreteSpec.Var × Int)} {names : Names}
    {regs : BIR.Env} {lhs rhs : Core.Atom} {a b : Operand}
    (x : ConcreteSpec.Var) (origin : Elab.SourceSite) (evidence : Int) (dest : Reg)
    (hrep : Represents slots names regs) (hs : Scoped names dest)
    (ha : operand names lhs = .ok a) (hb : operand names rhs = .ok b) :
    ∃ n, MIR.evalOp slots (.add lhs rhs origin (some evidence)) = .ok n ∧
      BIR.execInst regs (.bin .add .u8 dest a b) = some ((dest, .int .u8 n) :: regs) ∧
      Represents ((x, n) :: slots) (names.insert x.id (.reg dest)) ((dest, .int .u8 n) :: regs) := by
  obtain ⟨m, hm, hea⟩ := operand_correct hrep ha
  obtain ⟨k, hk, heb⟩ := operand_correct hrep hb
  obtain ⟨n, hn, hi⟩ := discharged_add_correct hm hk hea heb origin evidence dest
  refine ⟨n, hn, hi, represents_cons (represents_fresh_reg hrep dest _ ?_) x ?_⟩
  · exact fun y hy => (Nat.lt_irrefl dest) (hs y dest hy)
  · simp [BIR.evalOp, ConcreteSpec.Core.AList.lookup]

private theorem assign_checked_correct {slots : List (ConcreteSpec.Var × Int)} {names : Names}
    {regs : BIR.Env} {lhs rhs : Core.Atom} {a b : Operand}
    (x : ConcreteSpec.Var) (origin : Elab.SourceSite) {dest flag : Reg} (hdf : dest < flag)
    (hrep : Represents slots names regs) (hs : Scoped names dest)
    (ha : operand names lhs = .ok a) (hb : operand names rhs = .ok b) :
    (∃ n, MIR.evalOp slots (.add lhs rhs origin none) = .ok n ∧
      BIR.execInst regs (.binOvf .add .u8 dest flag a b) =
        some ((flag, .bool false) :: (dest, .int .u8 n) :: regs) ∧
      Represents ((x, n) :: slots) (names.insert x.id (.reg dest))
        ((flag, .bool false) :: (dest, .int .u8 n) :: regs)) ∨
    (MIR.evalOp slots (.add lhs rhs origin none) = .error (.trap .arithmeticOverflow) ∧
      ∃ n, BIR.execInst regs (.binOvf .add .u8 dest flag a b) =
        some ((flag, .bool true) :: (dest, .int .u8 n) :: regs)) := by
  obtain ⟨m, hm, hea⟩ := operand_correct hrep ha
  obtain ⟨k, hk, heb⟩ := operand_correct hrep hb
  rcases checked_add_correct hm hk hea heb origin dest flag with ⟨n, hn, hi⟩ | ⟨ht, hi⟩
  · left
    have freshDest : ∀ (y : Nat), names[y]? ≠ some (Operand.reg dest) :=
      fun y hy => (Nat.lt_irrefl dest) (hs y dest hy)
    have freshFlag : ∀ (y : Nat), names[y]? ≠ some (Operand.reg flag) :=
      fun y hy => (Nat.lt_irrefl flag) (Nat.lt_trans (hs y flag hy) hdf)
    refine ⟨n, hn, hi, represents_cons
      (represents_fresh_reg (represents_fresh_reg hrep dest _ freshDest) flag _ freshFlag) x ?_⟩
    simp [BIR.evalOp, ConcreteSpec.Core.AList.lookup, Nat.ne_of_gt hdf]
  · exact Or.inr ⟨ht, _, hi⟩

/-- Walk the live-slot witness and both edge lists in order. Immediates keep their
value without using an edge argument. Each register forwarding consumes one argument
and one fresh parameter. A slot omitted by the witness cannot be read in the continuation. -/
private def transfer (before : Names) :
    List ConcreteSpec.Var → List (Reg × Ty) → List Operand → Nat → Option (Names × Nat)
  | [], [], [], next => some ({}, next)
  | x :: xs, params, args, next => do
    let op ← before[x.id]?
    match op with
    | .imm .. =>
      let (after, last) ← transfer before xs params args next
      return (after.insert x.id op, last)
    | .reg _ =>
      match params, args with
      | (p, .int .u8) :: ps, a :: as =>
        if a = op ∧ next ≤ p then
          let (after, last) ← transfer before xs ps as (p + 1)
          return (after.insert x.id (.reg p), last)
        else none
      | _, _ => none
  | _, _, _, _ => none

private theorem transfer_correct {before after : Names} {slots : List (ConcreteSpec.Var × Int)}
    {regs : BIR.Env} {xs : List ConcreteSpec.Var} {params : List (Reg × Ty)}
    {args : List Operand} {first last : Nat}
    (hrep : Represents slots before regs)
    (h : transfer before xs params args first = some (after, last)) :
    first ≤ last ∧ Above after first ∧ Scoped after last ∧ params.length = args.length ∧
      ∃ vals, args.mapM (BIR.evalOp regs) = some vals ∧
        Represents slots after ((params.map Prod.fst).zip vals ++ regs) := by
  induction xs generalizing params args first last after with
  | nil =>
    cases params <;> cases args <;> simp [transfer] at h
    obtain ⟨rfl, rfl⟩ := h
    exact ⟨Nat.le_refl _, by simp [Above], by simp [Scoped], rfl, [], rfl, by simp [Represents]⟩
  | cons x xs ih =>
    cases hop : before[x.id]? with
    | none => simp [transfer, hop] at h
    | some op =>
      obtain ⟨n, hn, hv⟩ := hrep x op hop
      cases op with
      | imm ty value =>
        cases ht : transfer before xs params args first with
        | none => simp [transfer, hop, ht] at h
        | some result =>
          obtain ⟨rest, bound⟩ := result
          simp [transfer, hop, ht] at h
          obtain ⟨rfl, rfl⟩ := h
          obtain ⟨hle, habove, hscope, harity, vals, hvals, hrelated⟩ := ih ht
          exact ⟨hle, above_insert habove (by simp), scoped_insert hscope (by simp),
            harity, vals, hvals, represents_insert_existing hrelated hn hv⟩
      | reg r =>
        cases params with
        | nil => simp [transfer, hop] at h
        | cons param ps =>
          cases args with
          | nil => simp [transfer, hop] at h
          | cons a as =>
            obtain ⟨p, ty⟩ := param
            cases ty with
            | bool => simp [transfer, hop] at h
            | int ty =>
              cases ty <;> simp [transfer, hop] at h
              · obtain ⟨⟨rfl, hle⟩, h⟩ := h
                cases ht : transfer before xs ps as (p + 1) with
                | none => simp [ht] at h
                | some result =>
                  obtain ⟨rest, bound⟩ := result
                  simp [ht] at h
                  obtain ⟨rfl, rfl⟩ := h
                  obtain ⟨hbound, habove, hscope, harity, vals, hvals, hrelated⟩ := ih ht
                  have hfresh : ∀ (y : Nat), rest[y]? ≠ some (Operand.reg p) :=
                    fun y hy => by have := habove y p hy; omega
                  refine ⟨by omega, above_insert (fun y k hk => by exact Nat.le_trans hle (Nat.le_of_lt (habove y k hk)))
                    (by intro k hk; cases hk; exact hle),
                    scoped_insert hscope (by intro k hk; cases hk; exact Nat.lt_of_succ_le hbound),
                    by simp [harity], .int .u8 n :: vals, ?_, ?_⟩
                  · simp [List.mapM_cons, hv, hvals]
                  · exact represents_insert_existing (represents_fresh_reg hrelated p _ hfresh)
                      hn (by simp [BIR.evalOp, ConcreteSpec.Core.AList.lookup])

/-- Consume the current block's instructions, then the following blocks and sites
in emission order. The slice-0 layout puts each trap immediately before its
continuation. Edge numbers are checked against that layout. -/
private def checkStores (path : String) (result : Core.Atom) :
    List Memory.Store → Names → Nat → Nat → Nat →
      List Inst → Term → List Block → SiteTable → Witness → Bool
  | [], names, _, _, _, [], .ret op, [], [], [] =>
    operand names result == .ok op
  | stmt :: rest, names, next, j, site, insts, term, blocks, sites, edges =>
    stmt.dest.layout == Memory.u8Layout &&
    match stmt.value with
    | .copy a =>
      match operand names a with
      | .ok op => checkStores path result rest (names.insert stmt.dest.id.id op) next j site
          insts term blocks sites edges
      | .error _ => false
    | .add a b origin evidence =>
      match evidence, insts with
      | some _, .bin .add .u8 dest lhs rhs :: is =>
        next ≤ dest && operand names a == .ok lhs && operand names b == .ok rhs &&
          checkStores path result rest (names.insert stmt.dest.id.id (.reg dest)) (dest + 1)
            j site is term blocks sites edges
      | none, [.binOvf .add .u8 dest flag lhs rhs] =>
        match term, blocks, sites, edges with
        | .condbr cond trap [] pass args, trapBlock :: cont :: bs, entry :: ss, xs :: es =>
          next ≤ dest && dest < flag && cond == flag && trap == j + 1 && pass == j + 2 &&
            decide (trapBlock = ⟨[], [], .trap site⟩) &&
            decide (entry = overflowSite path site origin) &&
            operand names a == .ok lhs && operand names b == .ok rhs &&
            match transfer (names.insert stmt.dest.id.id (.reg dest)) xs cont.params args (flag + 1) with
            | some (after, last) =>
              checkStores path result rest after last (j + 2) (site + 1) cont.insts cont.term bs ss es
            | none => false
        | _, _, _, _ => false
      | _, _ => false
  | _, _, _, _, _, _, _, _, _, _ => false

private def check (path : String) (mem : Memory.Program) (bir : BIR.Program) (edges : Witness) : Bool :=
  match bir.blocks with
  | ⟨[], insts, term⟩ :: blocks =>
    checkStores path mem.result mem.stores {} 0 0 0 insts term blocks bir.sites edges
  | _ => false

private def sourceFrom (stores : List Memory.Store) (result : Core.Atom)
    (slots : List (ConcreteSpec.Var × Int)) : ConcreteSpec.Outcome :=
  let answer : Except ConcreteSpec.Outcome Int := do
    let slots ← stores.foldlM (fun slots stmt => do
      let n ← MIR.evalOp slots stmt.value
      return (stmt.dest.id, n) :: slots) slots
    MIR.value slots result
  match answer with
  | .ok n => .exit n.toNat
  | .error e => e

private theorem source_cons {stmt : Memory.Store} {stores : List Memory.Store}
    {result : Core.Atom} {slots : List (ConcreteSpec.Var × Int)} {n : Int}
    (hn : MIR.evalOp slots stmt.value = .ok n) :
    sourceFrom (stmt :: stores) result slots = sourceFrom stores result ((stmt.dest.id, n) :: slots) := by
  simp [sourceFrom, hn, bind, Except.bind, Pure.pure, Except.pure]

private theorem source_trap {stmt : Memory.Store} {stores : List Memory.Store}
    {result : Core.Atom} {slots : List (ConcreteSpec.Var × Int)} {kind : ConcreteSpec.Trap}
    (hn : MIR.evalOp slots stmt.value = .error (.trap kind)) :
    sourceFrom (stmt :: stores) result slots = .trap kind := by
  simp [sourceFrom, hn, bind, Except.bind]

private theorem source_initial (mem : Memory.Program) :
    sourceFrom mem.stores mem.result [] = Memory.run mem := by
  unfold sourceFrom Memory.run MIR.run
  change _ = (match (do
    let env ← (mem.stores.map (fun (stmt : Memory.Store) => (⟨stmt.dest.id, stmt.value⟩ : MIR.Assignment))).foldlM
      (fun env (stmt : MIR.Assignment) => do
        let n ← MIR.evalOp env stmt.op
        return (stmt.dest, n) :: env) []
    MIR.value env mem.result) with
    | .ok n => ConcreteSpec.Outcome.exit n.toNat
    | .error e => e)
  rw [List.foldlM_map]

private theorem execInsts_append {initial middle : BIR.Env} {pre rest : List Inst}
    (h : BIR.execInsts initial pre = some middle) :
    BIR.execInsts initial (pre ++ rest) = BIR.execInsts middle rest := by
  induction pre generalizing initial with
  | nil => simpa [BIR.execInsts] using congrArg (fun e => e >>= (BIR.execInsts · rest)) h
  | cons i pre ih =>
    cases he : BIR.execInst initial i with
    | none => simp [BIR.execInsts, he] at h
    | some e =>
      simp only [List.cons_append, BIR.execInsts, he]
      exact ih (by simpa [BIR.execInsts, he] using h)

private theorem block_at {p : BIR.Program} {j : Nat} {b : Block} {bs : List Block}
    (h : p.blocks.drop j = b :: bs) : p.blocks[j]? = some b := by
  have hh := congrArg (fun xs : List Block => xs[0]?) h
  simpa [List.getElem?_drop] using hh

private theorem drop_next {α : Type} {xs bs : List α} {j : Nat} {b : α}
    (h : xs.drop j = b :: bs) : xs.drop (j + 1) = bs := by
  rw [← List.drop_drop, h]
  rfl

private theorem block_fuel {p : BIR.Program} {j : Nat} {b : Block} {bs : List Block}
    (h : p.blocks.drop j = b :: bs) : j < p.blocks.length := by
  have hh := congrArg List.length h
  simp only [List.length_drop, List.length_cons] at hh
  omega

private theorem run_checked_trap {p : BIR.Program} {j site flag dest fuel : Nat}
    {initial regs : BIR.Env} {n : Int} {guard : Block} {args : List Operand}
    (hguard : p.blocks[j]? = some guard)
    (htrap : p.blocks[j + 1]? = some ⟨[], [], .trap site⟩)
    (hsite : site < p.sites.length)
    (hterm : guard.term = .condbr flag (j + 1) [] (j + 2) args)
    (hexec : BIR.execInsts initial guard.insts =
      some ((flag, .bool true) :: (dest, .int .u8 n) :: regs)) :
    BIR.runBlock p (fuel + 2) j initial = .trapped site := by
  simp [BIR.runBlock, hguard, htrap, hsite, hterm, hexec, BIR.enter,
    ConcreteSpec.Core.AList.lookup, BIR.execInsts]

private theorem checkStores_correct {path : String} {p : BIR.Program} {result : Core.Atom}
    {stores : List Memory.Store} {names : Names} {next j site fuel : Nat}
    {pre insts : List Inst} {block : Block} {blocks : List Block}
    {sites : SiteTable} {edges : Witness} {slots : List (ConcreteSpec.Var × Int)}
    {initial regs : BIR.Env}
    (hc : checkStores path result stores names next j site insts block.term blocks sites edges = true)
    (hblocks : p.blocks.drop j = block :: blocks) (hsites : p.sites.drop site = sites)
    (hinsts : block.insts = pre ++ insts) (hpre : BIR.execInsts initial pre = some regs)
    (hfuel : p.blocks.length - j < fuel) (hs : Scoped names next)
    (hrep : Represents slots names regs) :
    RelatedOutcome p (sourceFrom stores result slots) (BIR.runBlock p fuel j initial) := by
  induction stores generalizing names next j site fuel pre insts block blocks sites edges slots initial regs with
  | nil =>
    obtain ⟨params, original, term⟩ := block
    cases insts <;> cases term <;> cases blocks <;> cases sites <;> cases edges <;>
      simp [checkStores] at hc
    rename_i op
    obtain ⟨n, hn, he⟩ := operand_correct hrep hc
    cases fuel with
    | zero => omega
    | succ k =>
      have hexec := execInsts_append (rest := []) hpre
      simp only [List.append_nil, BIR.execInsts] at hexec
      have horig : original = pre := by simpa using hinsts
      simp [BIR.runBlock, block_at hblocks, horig, hexec, he, RelatedOutcome, sourceFrom, hn,
        bind, Except.bind, Pure.pure, Except.pure]
  | cons stmt stores ih =>
    obtain ⟨dest, value⟩ := stmt
    cases value with
    | copy a =>
      cases ha : operand names a with
      | error e => simp [checkStores, ha] at hc
      | ok op =>
        simp [checkStores, ha] at hc
        obtain ⟨n, hn, hr⟩ := assign_copy_correct dest.id hrep ha
        rw [source_cons (stmt := ⟨dest, .copy a⟩) hn]
        exact ih hc.2 hblocks hsites hinsts hpre hfuel
          (scoped_insert hs (by intro r hr; exact operand_scoped hs (hr ▸ ha))) hr
    | add a b origin evidence =>
      cases evidence with
      | some proof =>
        cases insts with
        | nil => simp [checkStores] at hc
        | cons i is =>
          cases i with
          | trunc => simp [checkStores] at hc
          | binOvf => simp [checkStores] at hc
          | bin op ty d lhs rhs =>
            cases op <;> cases ty <;> simp [checkStores] at hc
            obtain ⟨_, ⟨⟨hd, ha⟩, hb⟩, hcheck⟩ := hc
            have hsDest := scoped_mono hs hd
            obtain ⟨n, hn, hi, hr⟩ :=
              assign_discharged_correct dest.id origin proof d hrep hsDest ha hb
            rw [source_cons (stmt := ⟨dest, .add a b origin (some proof)⟩) hn]
            apply ih hcheck hblocks hsites (pre := pre ++ [.bin .add .u8 d lhs rhs])
              (regs := (d, .int .u8 n) :: regs)
            · simpa [List.append_assoc] using hinsts
            · rw [execInsts_append hpre]
              simp [BIR.execInsts, hi]
            · exact hfuel
            · exact scoped_insert (scoped_mono hsDest (Nat.le_succ _))
                (by intro r hr; cases hr; exact Nat.lt_succ_self _)
            · exact hr
      | none =>
        cases insts with
        | nil => simp [checkStores] at hc
        | cons i is =>
          cases is with
          | cons => simp [checkStores] at hc
          | nil =>
            cases i with
            | trunc => simp [checkStores] at hc
            | bin => simp [checkStores] at hc
            | binOvf op ty d flag lhs rhs =>
              cases op <;> cases ty <;> simp [checkStores] at hc
              obtain ⟨params, original, term⟩ := block
              cases term with
              | ret => simp at hc
              | trap => simp at hc
              | br => simp at hc
              | condbr cond trap trapArgs pass args =>
                cases trapArgs with
                | cons => simp at hc
                | nil =>
                  cases blocks with
                  | nil => simp at hc
                  | cons trapBlock tail =>
                    cases tail with
                    | nil => simp at hc
                    | cons cont bs =>
                      cases sites with
                      | nil => simp at hc
                      | cons entry ss =>
                        cases edges with
                        | nil => simp at hc
                        | cons xs es =>
                          simp at hc
                          obtain ⟨_, ⟨⟨⟨⟨⟨⟨⟨⟨hd, hdf⟩, hcond⟩, htrapIndex⟩, hpass⟩,
                            htrapBlock⟩, hentry⟩, ha⟩, hb⟩, hcheck⟩ := hc
                          rw [hcond, htrapIndex, hpass, htrapBlock] at hblocks
                          rw [hentry] at hsites
                          cases ht : transfer (names.insert dest.id.id (.reg d)) xs
                              cont.params args (flag + 1) with
                          | none => simp [ht] at hcheck
                          | some renamed =>
                            obtain ⟨after, last⟩ := renamed
                            simp [ht] at hcheck
                            have hguard := block_at hblocks
                            have htrap := block_at (drop_next hblocks)
                            have hcontSuffix := drop_next (drop_next hblocks)
                            have hcont := block_at hcontSuffix
                            have hforward := block_fuel hcontSuffix
                            have hsite : p.sites[site]? = some (overflowSite path site origin) := by
                              have hh := congrArg (fun xs : SiteTable => xs[0]?) hsites
                              simpa [List.getElem?_drop] using hh
                            have hsiteBound := List.getElem?_eq_some_iff.mp hsite |>.choose
                            have hf : 2 ≤ fuel := by omega
                            let k := fuel - 2
                            have heFuel : fuel = k + 2 := by dsimp [k]; omega
                            rw [heFuel]
                            change original = pre ++ [.binOvf .add .u8 d flag lhs rhs] at hinsts
                            have hwhole : BIR.execInsts initial original =
                                BIR.execInst regs (.binOvf .add .u8 d flag lhs rhs) := by
                              rw [hinsts, execInsts_append hpre]
                              simp only [BIR.execInsts]
                              split <;> rename_i he <;> exact he.symm
                            rcases assign_checked_correct dest.id origin hdf hrep (scoped_mono hs hd)
                                ha hb with ⟨n, hn, hi, hr⟩ | ⟨hn, n, hi⟩
                            · obtain ⟨_, _, hs', harity, vals, hvals, hr'⟩ := transfer_correct hr ht
                              have henter : BIR.enter p (j + 2) args
                                  ((flag, .bool false) :: (d, .int .u8 n) :: regs) =
                                  some ((cont.params.map Prod.fst).zip vals ++
                                    (flag, .bool false) :: (d, .int .u8 n) :: regs) := by
                                simp [BIR.enter, hcont, harity, hvals]
                              have hrun : BIR.runBlock p (k + 2) j initial =
                                  BIR.runBlock p (k + 1) (j + 2)
                                    ((cont.params.map Prod.fst).zip vals ++
                                      (flag, .bool false) :: (d, .int .u8 n) :: regs) := by
                                change BIR.runBlock p ((k + 1) + 1) j initial = _
                                rw [BIR.runBlock]
                                simp only [hguard, hwhole, hi,
                                  ConcreteSpec.Core.AList.lookup, ↓reduceIte, henter]
                              rw [hrun, source_cons (stmt := ⟨dest, .add a b origin none⟩) hn]
                              apply ih hcheck (by simpa [Nat.add_assoc] using hcontSuffix)
                                (drop_next hsites) (pre := []) rfl rfl
                              · dsimp [k]; omega
                              · exact hs'
                              · exact hr'
                            · rw [run_checked_trap hguard htrap hsiteBound rfl (hwhole.trans hi),
                                source_trap (stmt := ⟨dest, .add a b origin none⟩) hn]
                              exact ⟨_, hsite, rfl⟩

private theorem check_correct {path : String} {mem : Memory.Program} {bir : BIR.Program}
    {edges : Witness} (h : check path mem bir edges = true) : Preserves mem bir := by
  unfold check at h
  cases hblocks : bir.blocks with
  | nil => simp [hblocks] at h
  | cons block blocks =>
    obtain ⟨params, insts, term⟩ := block
    cases params with
    | cons => simp [hblocks] at h
    | nil =>
      simp only [hblocks] at h
      have hrun := checkStores_correct (p := bir) (block := ⟨[], insts, term⟩)
        (fuel := bir.blocks.length + 1) (pre := []) (initial := []) (slots := [])
        h (by simpa using hblocks) rfl rfl rfl (by omega) (by simp [Scoped])
        (by simp [Represents])
      rw [source_initial mem] at hrun
      unfold Preserves BIR.run
      rw [hblocks]
      simpa [RelatedOutcome, hblocks] using hrun

/-- Validate the emitted program against the source stores and the separate edge witness. -/
def validate (path : String) (mem : Memory.Program) (bir : BIR.Program)
    (edges : Witness) : Except Refusal BIR.Program :=
  if !check path mem bir edges then .error (.internal "SSA correspondence check failed")
  else if !BIR.wf bir then .error (.internal "SSA produced ill-formed BIR")
  else .ok bir

theorem validate_correct {path : String} {mem : Memory.Program} {candidate bir : BIR.Program}
    {edges : Witness} (h : validate path mem candidate edges = .ok bir) :
    BIR.WF bir ∧ Preserves mem bir := by
  unfold validate at h
  split at h
  · contradiction
  · rename_i hc
    split at h
    · contradiction
    · rename_i hwf
      cases h
      exact ⟨by simpa [BIR.WF] using hwf, check_correct (by simpa using hc)⟩

def lower (path : String) (p : Memory.Program) : Except Refusal BIR.Program := do
  let s ← p.stores.foldlM (assign path) {}
  let result ← operand s.env p.result
  let bir : BIR.Program :=
    ⟨(s.blocks.push ⟨s.params, s.insts.reverse, .ret result⟩).toList, s.sites.toList⟩
  validate path p bir s.edges.reverse

private theorem lower_validated {path : String} {p : Memory.Program} {bir : BIR.Program}
    (h : lower path p = .ok bir) : BIR.WF bir ∧ Preserves p bir := by
  cases hs : p.stores.foldlM (assign path) {} with
  | error e => simp [lower, hs, bind, Except.bind] at h
  | ok s =>
    cases hr : operand s.env p.result with
    | error e => simp [lower, hs, hr, bind, Except.bind] at h
    | ok result =>
      simp only [lower, hs, hr, bind, Except.bind] at h
      exact validate_correct h

theorem lower_wf {path : String} {p : Memory.Program} {bir : BIR.Program}
    (h : lower path p = .ok bir) : BIR.WF bir :=
  (lower_validated h).1

/-- Every successful lowering preserves the Memory observation, including the taken trap's kind. -/
theorem lower_correct {path : String} {p : Memory.Program} {bir : BIR.Program}
    (h : lower path p = .ok bir) : Preserves p bir :=
  (lower_validated h).2

end Concrete.SSA
