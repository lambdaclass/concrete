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

/-- The SSA contract observes the return value or the kind recorded at the taken trap site. -/
def Preserves (mem : Memory.Program) (bir : BIR.Program) : Prop :=
  match BIR.run bir (bir.blocks.length + 1) with
  | .exited n => Memory.run mem = .exit n
  | .trapped site => ∃ entry, bir.sites[site]? = some entry ∧ Memory.run mem = .trap entry.kind
  | _ => False

/-- Project a completed BIR run into the source outcome space. A missing trap entry,
a stuck run, and fuel exhaustion have no corresponding outcome. -/
private def observedOutcome (bir : BIR.Program) : Option ConcreteSpec.Outcome :=
  match BIR.run bir (bir.blocks.length + 1) with
  | .exited n => some (.exit n)
  | .trapped site => bir.sites[site]?.map (fun entry => .trap entry.kind)
  | _ => none

/-- Slice 0 has closed, bounded executions, so the validator compares the observations
using the stage semantics on the original Memory program and the actual emitted BIR. -/
def validate (mem : Memory.Program) (bir : BIR.Program) : Except Refusal BIR.Program :=
  if !BIR.wf bir then .error (.internal "SSA produced ill-formed BIR")
  else if observedOutcome bir = some (Memory.run mem) then .ok bir
  else .error (.internal "SSA changed the Memory outcome")

theorem validate_correct {mem : Memory.Program} {candidate bir : BIR.Program}
    (h : validate mem candidate = .ok bir) : BIR.WF bir ∧ Preserves mem bir := by
  unfold validate at h
  split at h
  · contradiction
  · rename_i hwf
    split at h
    · rename_i hout
      cases h
      refine ⟨by simpa [BIR.WF] using hwf, ?_⟩
      unfold observedOutcome at hout
      unfold Preserves
      cases hrun : BIR.run candidate (candidate.blocks.length + 1) <;> rw [hrun] at hout
      · exact (Option.some.inj hout).symm
      · rename_i site
        cases hsite : candidate.sites[site]? with
        | none => simp [hsite] at hout
        | some entry =>
          exact ⟨entry, hsite, (Option.some.inj (by simpa [hsite] using hout)).symm⟩
      · contradiction
      · contradiction
    · contradiction

def lower (path : String) (p : Memory.Program) : Except Refusal BIR.Program := do
  let s ← p.stores.foldlM (assign path) {}
  let result ← operand s.env p.result
  let bir : BIR.Program :=
    ⟨(s.blocks.push ⟨s.params, s.insts.reverse, .ret result⟩).toList, s.sites.toList⟩
  validate p bir

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

/-! ## Local simulation

The source store and the lowering environment use newest-first lookup. Register values are
read from the current BIR execution environment.
-/

private def Represents (slots : List (ConcreteSpec.Var × Int)) (names : Env)
    (regs : BIR.Env) : Prop :=
  ∀ x op, names.lookup x = some op →
    ∃ n, slots.lookup x = some n ∧ BIR.evalOp regs op = some (.int .u8 n)

private theorem operand_correct {slots : List (ConcreteSpec.Var × Int)} {names : Env}
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
    cases hv : names.lookup x with
    | none => simp [operand, hv] at h
    | some v =>
      simp [operand, hv] at h
      subst op
      obtain ⟨n, hs, he⟩ := hrep x v hv
      exact ⟨n, by simp [MIR.value, hs], he⟩

private theorem represents_cons {slots : List (ConcreteSpec.Var × Int)} {names : Env}
    {regs : BIR.Env} (hrep : Represents slots names regs) (x : ConcreteSpec.Var)
    {op : Operand} {n : Int} (he : BIR.evalOp regs op = some (.int .u8 n)) :
    Represents ((x, n) :: slots) ((x, op) :: names) regs := by
  intro y v hv
  by_cases hxy : x = y
  · subst y
    simp at hv
    subst v
    exact ⟨n, by simp, he⟩
  · have hne : (y == x) = false := beq_false_of_ne (Ne.symm hxy)
    have hv' : names.lookup y = some v := by simpa [List.lookup, hne] using hv
    obtain ⟨m, hs, hm⟩ := hrep y v hv'
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

private theorem execInsts_snoc {initial current : BIR.Env} {insts : List Inst}
    (h : BIR.execInsts initial insts = some current) (i : Inst) :
    BIR.execInsts initial (insts ++ [i]) = BIR.execInst current i := by
  induction insts generalizing initial with
  | nil =>
    simp [BIR.execInsts] at h
    subst current
    cases hi : BIR.execInst initial i <;> simp [BIR.execInsts, hi]
  | cons j js ih =>
    cases hj : BIR.execInst initial j with
    | none => simp [BIR.execInsts, hj] at h
    | some next =>
      simp only [BIR.execInsts, hj] at h
      simpa only [List.cons_append, BIR.execInsts, hj] using ih h

private theorem represents_fresh_reg {slots : List (ConcreteSpec.Var × Int)} {names : Env}
    {regs : BIR.Env} (hrep : Represents slots names regs) (dest : Reg) (n : Int)
    (hf : ∀ x, names.lookup x ≠ some (.reg dest)) :
    Represents slots names ((dest, .int .u8 n) :: regs) := by
  intro x op hx
  obtain ⟨m, hs, he⟩ := hrep x op hx
  refine ⟨m, hs, ?_⟩
  cases op with
  | imm => exact he
  | reg r =>
    have hne : dest ≠ r := by
      intro eq
      subst r
      exact hf x hx
    simpa [BIR.evalOp, ConcreteSpec.Core.AList.lookup, hne] using he

/-- Copy assignments update the source store and the SSA name map without executing an
instruction. This holds even when the destination shadows a previous binding. -/
private theorem assign_copy_correct {path : String} {s s' : State} {dest : Memory.Slot}
    {a : Core.Atom} {slots : List (ConcreteSpec.Var × Int)} {regs : BIR.Env}
    (hrep : Represents slots s.env regs)
    (h : assign path s ⟨dest, .copy a⟩ = .ok s') :
    ∃ n op, MIR.evalOp slots (.copy a) = .ok n ∧
      s' = { s with env := (dest.id, op) :: s.env } ∧
      Represents ((dest.id, n) :: slots) s'.env regs := by
  by_cases hl : dest.layout = Memory.u8Layout
  · cases ho : operand s.env a with
    | error e => simp [assign, hl, ho, bind, Except.bind] at h
    | ok op =>
      obtain ⟨n, hn, he⟩ := operand_correct hrep ho
      have hs : s' = { s with env := (dest.id, op) :: s.env } := by
        simpa [assign, hl, ho, bind, Except.bind, Pure.pure, Except.pure, eq_comm] using h
      subst s'
      exact ⟨n, op, hn, rfl, represents_cons hrep dest.id he⟩
  · simp [assign, hl, bind, Except.bind] at h

/-- A discharged assignment appends one wrapping addition to the open block. Its result
extends the simulation when the instruction destination is fresh for the old name map. -/
private theorem assign_discharged_correct {path : String} {s s' : State} {dest : Memory.Slot}
    {lhs rhs : Core.Atom} {origin : Elab.SourceSite} {evidence : Int}
    {slots : List (ConcreteSpec.Var × Int)} {initial regs : BIR.Env}
    (hexec : BIR.execInsts initial s.insts.reverse = some regs)
    (hrep : Represents slots s.env regs)
    (hf : ∀ x, s.env.lookup x ≠ some (.reg s.nextReg))
    (h : assign path s ⟨dest, .add lhs rhs origin (some evidence)⟩ = .ok s') :
    ∃ n a b, MIR.evalOp slots (.add lhs rhs origin (some evidence)) = .ok n ∧
      s' = { s with nextReg := s.nextReg + 1, env := (dest.id, .reg s.nextReg) :: s.env,
                    insts := .bin .add .u8 s.nextReg a b :: s.insts } ∧
      BIR.execInsts initial s'.insts.reverse = some ((s.nextReg, .int .u8 n) :: regs) ∧
      Represents ((dest.id, n) :: slots) s'.env ((s.nextReg, .int .u8 n) :: regs) := by
  by_cases hl : dest.layout = Memory.u8Layout
  · cases ha : operand s.env lhs with
    | error e => simp [assign, hl, ha, bind, Except.bind] at h
    | ok a =>
      cases hb : operand s.env rhs with
      | error e => simp [assign, hl, ha, hb, bind, Except.bind] at h
      | ok b =>
        have hs : s' = { s with
              nextReg := s.nextReg + 1,
              env := (dest.id, .reg s.nextReg) :: s.env,
              insts := .bin .add .u8 s.nextReg a b :: s.insts } := by
          simpa [assign, hl, ha, hb, bind, Except.bind, Pure.pure, Except.pure, eq_comm] using h
        subst s'
        obtain ⟨x, hx, hea⟩ := operand_correct hrep ha
        obtain ⟨y, hy, heb⟩ := operand_correct hrep hb
        obtain ⟨n, hn, hi⟩ := discharged_add_correct hx hy hea heb origin evidence s.nextReg
        refine ⟨n, a, b, hn, rfl, ?_, ?_⟩
        · rw [List.reverse_cons, execInsts_snoc hexec, hi]
        · exact represents_cons (represents_fresh_reg hrep s.nextReg n hf) dest.id (by
            simp [BIR.evalOp, ConcreteSpec.Core.AList.lookup])
  · simp [assign, hl, bind, Except.bind] at h

/-- Characterize the order of the renaming loop independently of register numbering.
The accumulator holds the names already processed, in reverse order. -/
private theorem rename_loop_order (names : Env)
    (acc : Nat × List Operand × List (Reg × Ty) × Env) :
    let renamed := forIn (m := Id) names acc fun (name, value) (next, args, params, env) =>
      match value with
      | .imm .. => .yield (next, args, params, (name, value) :: env)
      | .reg _ =>
        .yield (next + 1, value :: args, (next, .int .u8) :: params, (name, .reg next) :: env)
    renamed.2.2.2.reverse.map Prod.fst =
      acc.2.2.2.reverse.map Prod.fst ++ names.map Prod.fst := by
  induction names generalizing acc with
  | nil =>
    simp only [List.forIn_nil, List.map_nil, List.append_nil]
    rfl
  | cons binding rest ih =>
    obtain ⟨name, value⟩ := binding
    obtain ⟨next, args, params, env⟩ := acc
    cases value <;> rw [List.forIn_cons] <;>
      dsimp only [bind, Id.instMonad] <;>
      rw [ih] <;> simp [List.reverse_cons, List.append_assoc]

/-- The actual checked-add lowering keeps the newest-first order, including shadowed
names, when it constructs the continuation's name map. -/
private theorem riskyAdd_name_order (path : String) (s : State) (x : ConcreteSpec.Var)
    (a b : Operand) (origin : Elab.SourceSite) :
    (riskyAdd path s x a b origin).env.map Prod.fst = x :: s.env.map Prod.fst := by
  unfold riskyAdd
  dsimp only [Id.run, bind, Pure.pure, Id.instMonad]
  simpa only [List.reverse_nil, List.map_nil, List.nil_append, List.map_cons] using
    rename_loop_order ((x, .reg s.nextReg) :: s.env) (s.nextReg + 2, [], [], [])

private theorem riskyAdd_guard (path : String) (s : State) (x : ConcreteSpec.Var)
    (a b : Operand) (origin : Elab.SourceSite) :
    ∃ args : List Operand,
      (riskyAdd path s x a b origin).blocks.toList =
        s.blocks.toList ++ [
          ⟨s.params, s.insts.reverse ++ [.binOvf .add .u8 s.nextReg (s.nextReg + 1) a b],
            .condbr (s.nextReg + 1) (s.blocks.size + 1) [] (s.blocks.size + 2) args⟩,
          ⟨[], [], .trap s.sites.size⟩] ∧
      (riskyAdd path s x a b origin).sites.toList =
        s.sites.toList ++ [⟨.arithmeticOverflow, ⟨s.sites.size⟩,
          ⟨origin.operator.start, origin.operator.stop⟩,
          ⟨path, origin.operator.line, origin.operator.column⟩⟩] := by
  unfold riskyAdd
  simp [List.reverse_cons]

/-- An unchecked assignment closes the open block with a guard and emits its overflow site.
The guard's final environment agrees with the source result on the normal edge, and has a
true overflow flag exactly on the source trap case. -/
private theorem assign_checked_correct {path : String} {s s' : State} {dest : Memory.Slot}
    {lhs rhs : Core.Atom} {origin : Elab.SourceSite}
    {slots : List (ConcreteSpec.Var × Int)} {initial regs : BIR.Env}
    (hexec : BIR.execInsts initial s.insts.reverse = some regs)
    (hrep : Represents slots s.env regs)
    (h : assign path s ⟨dest, .add lhs rhs origin none⟩ = .ok s') :
    ∃ guard entry args,
      s'.blocks.toList = s.blocks.toList ++ [guard, ⟨[], [], .trap s.sites.size⟩] ∧
      guard.term = .condbr (s.nextReg + 1) (s.blocks.size + 1) [] (s.blocks.size + 2) args ∧
      s'.sites.toList = s.sites.toList ++ [entry] ∧ entry.kind = .arithmeticOverflow ∧
      s'.env.map Prod.fst = dest.id :: s.env.map Prod.fst ∧
      ((∃ n, MIR.evalOp slots (.add lhs rhs origin none) = .ok n ∧
        BIR.execInsts initial guard.insts =
          some ((s.nextReg + 1, .bool false) :: (s.nextReg, .int .u8 n) :: regs)) ∨
       (MIR.evalOp slots (.add lhs rhs origin none) = .error (.trap .arithmeticOverflow) ∧
        ∃ n, BIR.execInsts initial guard.insts =
          some ((s.nextReg + 1, .bool true) :: (s.nextReg, .int .u8 n) :: regs))) := by
  by_cases hl : dest.layout = Memory.u8Layout
  · cases ha : operand s.env lhs with
    | error e => simp [assign, hl, ha, bind, Except.bind] at h
    | ok a =>
      cases hb : operand s.env rhs with
      | error e => simp [assign, hl, ha, hb, bind, Except.bind] at h
      | ok b =>
        have hs : s' = riskyAdd path s dest.id a b origin := by
          simpa [assign, hl, ha, hb, bind, Except.bind, Pure.pure, Except.pure, eq_comm] using h
        subst s'
        obtain ⟨args, hblocks, hsites⟩ := riskyAdd_guard path s dest.id a b origin
        refine ⟨_, _, args, hblocks, rfl, hsites, rfl,
          riskyAdd_name_order path s dest.id a b origin, ?_⟩
        rw [execInsts_snoc hexec]
        obtain ⟨x, hx, hea⟩ := operand_correct hrep ha
        obtain ⟨y, hy, heb⟩ := operand_correct hrep hb
        rcases checked_add_correct hx hy hea heb origin s.nextReg (s.nextReg + 1) with hn | ht
        · exact Or.inl hn
        · exact Or.inr ⟨ht.1, _, ht.2⟩
  · simp [assign, hl, bind, Except.bind] at h

/-- Once the checked block is reached, its true flag takes the emitted trap edge.
Any later blocks and sites may be appended without affecting this result. -/
private theorem run_checked_trap {s : State} {initial regs : BIR.Env} {n : Int}
    {guard : Block} {args : List Operand} {entry : SiteEntry}
    (hterm : guard.term = .condbr (s.nextReg + 1) (s.blocks.size + 1) [] (s.blocks.size + 2) args)
    (hexec : BIR.execInsts initial guard.insts =
      some ((s.nextReg + 1, .bool true) :: (s.nextReg, .int .u8 n) :: regs))
    (hkind : entry.kind = .arithmeticOverflow)
    (suffix : List Block) (sites : SiteTable) (fuel : Nat) :
    let p : BIR.Program :=
      ⟨s.blocks.toList ++ guard :: ⟨[], [], .trap s.sites.size⟩ :: suffix,
        s.sites.toList ++ entry :: sites⟩
    BIR.runBlock p (fuel + 2) s.blocks.size initial = .trapped s.sites.size ∧
      ∃ hit, p.sites[s.sites.size]? = some hit ∧ hit.kind = .arithmeticOverflow := by
  dsimp
  refine ⟨?_, entry, ?_, hkind⟩
  · simp [BIR.runBlock, hterm, hexec, BIR.enter, ConcreteSpec.Core.AList.lookup,
      BIR.execInsts]
  · rw [List.getElem?_append_right (by simp)]
    simp

end Concrete.SSA
