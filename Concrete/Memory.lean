import Concrete.MIR
import Concrete.CFG
import Concrete.Interfaces.Pipeline
import ConcreteSpec.Proofs.Basics

namespace Concrete.Memory

private def u8LayoutQuery : Option ConcreteSpec.StageInterface.Layout :=
  (ConcreteSpec.StageInterface.sliceLanguage ConcreteSpec.StageInterface.slice0).layout []
    ConcreteSpec.Core.linuxX8664 (.scalar (.int .u8))

private theorem u8LayoutQuery_succeeds : u8LayoutQuery.isSome := by decide

def u8Layout : ConcreteSpec.StageInterface.Layout :=
  u8LayoutQuery.get u8LayoutQuery_succeeds

example :
    (ConcreteSpec.StageInterface.sliceLanguage ConcreteSpec.StageInterface.slice0).layout []
      ConcreteSpec.Core.linuxX8664 (.scalar (.int .u8)) = some u8Layout := by
  with_unfolding_all rfl

structure Slot where
  id : ConcreteSpec.Var
  layout : ConcreteSpec.StageInterface.Layout

/-- Store to a complete slot. The local operands of the RHS read their complete slots.
The addition retains its normal and exceptional outcomes. -/
structure Store where
  dest : Slot
  value : MIR.Op

structure Program where
  stores : List Store
  result : Core.Atom

def lower (p : MIR.Program) : Program :=
  ⟨p.body.map (fun stmt => ⟨⟨stmt.dest, u8Layout⟩, stmt.op⟩), p.result⟩

/-- Execute the stores in order. The slot table holds the latest value of each binding.
Reads use the same scalar rules as MIR. -/
def runFrom (stores : List Store) (result : Core.Atom)
    (slots : List (ConcreteSpec.Var × Int)) : ConcreteSpec.Outcome :=
  match (do
    let slots ← stores.foldlM (fun slots stmt => do
      let n ← MIR.evalOp slots stmt.value
      return (stmt.dest.id, n) :: slots) slots
    MIR.value slots result : Except ConcreteSpec.Outcome Int) with
  | .ok n => .exit n.toNat
  | .error e => e

def run (p : Program) : ConcreteSpec.Outcome := runFrom p.stores p.result []

theorem lower_correct (p : MIR.Program) : run (lower p) = MIR.run p := by
  simp only [run, lower, runFrom, MIR.run, MIR.runFrom, List.foldlM_map]
  rfl

/-- The admitted lowering stages preserve the Core outcome through Memory. -/
theorem lower_admitted_correct {source admitted mir} (ha : Admission.check source = .ok admitted)
    (hl : MIR.lower admitted = .ok mir) :
    let p := Core.toCore source.program
    run (lower mir) = Core.run p (p.body.length + 1) :=
  (lower_correct mir).trans (MIR.lower_correct ha hl)

/-- The unchecked lowering stages preserve the Core outcome through Memory. -/
theorem lower_unchecked_correct {source mir} (hl : MIR.lowerUnchecked source = .ok mir) :
    let p := Core.toCore source.program
    run (lower mir) = Core.run p (p.body.length + 1) :=
  (lower_correct mir).trans (MIR.lowerUnchecked_correct hl)

end Concrete.Memory

namespace Concrete.Memory

open Concrete.Interfaces ConcreteSpec.Core ConcreteSpec.StageInterface

/-- Inspect the operand inventory of typed MIR. An address of any projection
requires materializing its entire root. -/
def takesAddress {Ty : Type} (operands : List (Ty × OperandRead)) (root : PlaceId) : Bool :=
  operands.any fun (_, read) => match read with
    | .address place _ => place == root
    | _ => false

private theorem takesAddress_iff {Ty : Type} (operands : List (Ty × OperandRead)) (root : PlaceId) :
    takesAddress operands root = true ↔
      ∃ ty path, (ty, .address root path) ∈ operands := by
  rw [takesAddress, List.any_eq_true]
  constructor
  · rintro ⟨⟨ty, read⟩, member, found⟩
    cases read <;> simp_all
    case address place path => exact ⟨ty, path, member⟩
  · rintro ⟨ty, path, member⟩
    exact ⟨(ty, .address root path), member, by simp⟩

def escapes {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (mir : MIR lang profile) (fn : FnId) (root : PlaceId) : Bool :=
  mir.operations.any fun op => op.fn == fn && takesAddress op.operands root

theorem escapes_iff {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (mir : MIR lang profile) (fn : FnId) (root : PlaceId) :
    escapes (language := language) mir fn root = true ↔ mir.Escapes fn root := by
  simp [escapes, MIR.Escapes, List.any_eq_true, Bool.and_eq_true, takesAddress_iff]

/-- A virtual local is scalar and has no address uses in its function. -/
def storageClass {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (mir : MIR lang profile) (fn : FnId) (root : PlaceId)
    (ty : language.operands.Ty) : StorageClass :=
  if (language.scalarType ty).isSome && !escapes (language := language) mir fn root then .virtual
  else .materialized

theorem storageClass_virtual_iff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (mir : MIR lang profile) (fn : FnId) (root : PlaceId) (ty : language.operands.Ty) :
    storageClass (language := language) mir fn root ty = .virtual ↔
      (language.scalarType ty).isSome ∧ ¬ mir.Escapes fn root := by
  unfold storageClass
  cases hsc : (language.scalarType ty).isSome <;> cases hesc : escapes (language := language) mir fn root
  · exact ⟨fun h => StorageClass.noConfusion h, fun h => Bool.noConfusion h.1⟩
  · exact ⟨fun h => StorageClass.noConfusion h, fun h => Bool.noConfusion h.1⟩
  · exact ⟨fun _ => ⟨rfl, fun h => Bool.noConfusion (hesc.symm.trans ((escapes_iff mir fn root).mpr h))⟩,
      fun _ => rfl⟩
  · exact ⟨fun h => StorageClass.noConfusion h, fun h => (h.2 ((escapes_iff mir fn root).mp hesc)).elim⟩

private def checkSignature {Ty : Type} [DecidableEq Ty] {lang : LoweringLanguage Ty}
    {profile : Profile} (memory : Interfaces.Memory lang profile) (fn : FnId)
    (graph : MemoryGraph lang) : Bool :=
  match AList.lookup memory.signatures fn with
  | none => false
  | some signature =>
    graph.blocks.toList.any (fun entry =>
      decide (entry.id = graph.entry ∧ entry.params.map Sigma.fst = signature.parameters)) &&
    graph.blocks.toList.all fun block =>
      (match block.ending with
        | .ret result => decide (result.map Sigma.fst = signature.result)
        | .invalid => false
        | _ => true) &&
      block.body.all fun op =>
        match op.instruction with
        | .call callee destination args _ =>
          match AList.lookup memory.signatures callee with
          | none => false
          | some target =>
            decide (args.map Sigma.fst = target.parameters ∧
              destination.map Sigma.fst = target.result)
        | _ => true

private theorem checkSignature_iff {Ty : Type} [DecidableEq Ty] {lang : LoweringLanguage Ty}
    {profile : Profile} (memory : Interfaces.Memory lang profile) (fn : FnId)
    (graph : MemoryGraph lang) :
    checkSignature memory fn graph = true ↔ memory.SignatureMatches fn graph := by
  have ending (term : MemoryTerm lang) (result : Option Ty) :
      ((match term with
        | .ret value => decide (value.map Sigma.fst = result)
        | .invalid => false
        | _ => true) : Bool) = true ↔
      (match term with
        | .ret value => value.map Sigma.fst = result
        | .invalid => False
        | _ => True) := by
    cases term <;> simp
  have call (inst : MemoryInst lang) :
      ((match inst with
        | .call callee destination args _ =>
          match AList.lookup memory.signatures callee with
          | none => false
          | some target => decide (args.map Sigma.fst = target.parameters ∧
              destination.map Sigma.fst = target.result)
        | _ => true) : Bool) = true ↔
      (match inst with
        | .call callee destination args _ =>
          ∃ target, AList.lookup memory.signatures callee = some target ∧
            args.map Sigma.fst = target.parameters ∧ destination.map Sigma.fst = target.result
        | _ => True) := by
    cases inst <;> try simp
    case call callee dst args checks => cases AList.lookup memory.signatures callee <;> simp
  unfold checkSignature Interfaces.Memory.SignatureMatches
  cases AList.lookup memory.signatures fn with
  | none => simp
  | some signature =>
    simp only [Option.some.injEq, exists_eq_left', Bool.and_eq_true, List.any_eq_true,
      decide_eq_true_eq, List.all_eq_true, ending, call]
    rfl

private def checkDeclarations (declarations : Declarations) : Bool :=
  decide ((List.map Prod.fst declarations).Nodup) &&
  decide (((List.map Prod.snd declarations).map ConcreteSpec.StructDecl.name).Nodup) &&
  declarations.all fun entry =>
    decide (entry.2.name ≠ ⟨"main"⟩ ∧ (entry.2.fields.map Prod.fst).Nodup ∧ entry.2.name = entry.1.name)

private theorem checkDeclarations_iff (declarations : Declarations) :
    checkDeclarations declarations = true ↔ DeclarationsWF declarations := by
  simp only [checkDeclarations, Bool.and_eq_true, decide_eq_true_eq, List.all_eq_true,
    DeclarationsWF, ConcreteSpec.DeclsOk, List.mem_map, Prod.forall]
  constructor
  · rintro ⟨⟨hkeys, hnames⟩, hall⟩
    refine ⟨hkeys, ⟨hnames, ?_⟩, fun id decl hmem => (hall id decl hmem).2.2⟩
    rintro d ⟨⟨id, decl⟩, hmem, rfl⟩
    exact ⟨(hall id decl hmem).1, (hall id decl hmem).2.1⟩
  · rintro ⟨hkeys, ⟨hnames, hdecls⟩, hmatch⟩
    refine ⟨⟨hkeys, hnames⟩, fun id decl hmem => ?_⟩
    obtain ⟨hne, hfields⟩ := hdecls decl ⟨(id, decl), hmem, rfl⟩
    exact ⟨hne, hfields, hmatch id decl hmem⟩

private def checkMutableLocals {Ty G : Type} (functions : AList FnId (LocalTypes Ty × G))
    (mutableLocals : AList FnId (List PlaceId)) : Bool :=
  decide ((List.map Prod.fst mutableLocals).Nodup) &&
  mutableLocals.all fun entry =>
    functions.any fun fnEntry =>
      decide (fnEntry.1 = entry.1) &&
      entry.2.all fun root => fnEntry.2.1.any fun loc => decide (loc.1 = root)

private theorem checkMutableLocals_iff {Ty G : Type} (functions : AList FnId (LocalTypes Ty × G))
    (mutableLocals : AList FnId (List PlaceId)) :
    checkMutableLocals functions mutableLocals = true ↔
      MutableLocalsWF functions mutableLocals := by
  simp only [checkMutableLocals, Bool.and_eq_true, decide_eq_true_eq, List.all_eq_true,
    List.any_eq_true, MutableLocalsWF, Prod.forall]
  constructor
  · rintro ⟨hkeys, hall⟩
    refine ⟨hkeys, fun fn roots hmem => ?_⟩
    obtain ⟨⟨fn', locals, graph⟩, hfnMem, rfl, hroots⟩ := hall fn roots hmem
    refine ⟨locals, graph, hfnMem, fun root hroot => ?_⟩
    obtain ⟨⟨p, ty⟩, hpMem, rfl⟩ := hroots root hroot
    exact ⟨ty, hpMem⟩
  · rintro ⟨hkeys, hall⟩
    refine ⟨hkeys, fun fn roots hmem => ?_⟩
    obtain ⟨locals, graph, hfnMem, hroots⟩ := hall fn roots hmem
    refine ⟨(fn, locals, graph), hfnMem, rfl, fun root hroot => ?_⟩
    obtain ⟨ty, hlocMem⟩ := hroots root hroot
    exact ⟨(root, ty), hlocMem, rfl⟩

/-- Check the shared Memory boundary, also used by the frozen SSA contract
through its Memory view. -/
def checkHandoff {Ty : Type} [DecidableEq Ty] {lang : LoweringLanguage Ty}
    {profile : Profile} (memory : Interfaces.Memory lang profile) : Bool :=
  decide ((List.map Prod.fst memory.functions).Nodup ∧ (List.map Prod.fst memory.signatures).Nodup ∧
    (List.map Prod.fst memory.storage).Nodup ∧ (List.map Prod.fst memory.layouts).Nodup) &&
  memory.storage.all (fun (_, classes) => decide (List.map Prod.fst classes).Nodup) &&
  memory.layouts.all (fun (_, layouts) => decide (List.map Prod.fst layouts).Nodup) &&
  memory.functions.all (fun (fn, locals, graph) =>
    decide (List.map Prod.fst locals).Nodup &&
    Concrete.CFG.check graph Sigma.fst Sigma.fst && checkSignature memory fn graph) &&
  decide (memory.entry ∈ List.map Prod.fst memory.functions) &&
  memory.origins.all (fun origin => Concrete.CFG.checkSpan memory.project origin.lexical) &&
  memory.layouts.all (fun table => table.2.all (fun slot => slot.2.WellFormed profile.memoryTarget)) &&
  checkDeclarations memory.declarations &&
  checkMutableLocals memory.functions memory.mutableLocals

theorem checkHandoff_iff {Ty : Type} [DecidableEq Ty] {lang : LoweringLanguage Ty}
    {profile : Profile} (memory : Interfaces.Memory lang profile) :
    checkHandoff memory = true ↔ memory.HandoffWF := by
  have valid {fn : FnId} {graph : MemoryGraph lang} (h : memory.SignatureMatches fn graph) :
      ∀ block ∈ graph.blocks.toList, block.ending ≠ .invalid := by
    obtain ⟨_, _, _, hs⟩ := h
    intro block member invalid
    have hb := (hs block member).1
    simp [invalid] at hb
  simp only [checkHandoff, Bool.and_eq_true, decide_eq_true_eq, List.all_eq_true,
    Concrete.CFG.check_iff, checkSignature_iff, Concrete.CFG.checkSpan_iff,
    checkDeclarations_iff, checkMutableLocals_iff,
    Interfaces.Memory.HandoffWF, Interfaces.Memory.LayoutsWF, Interfaces.Memory.TablesWF, Prod.forall]
  constructor
  · rintro ⟨⟨⟨⟨⟨⟨⟨⟨keys, storage⟩, layouts⟩, graphs⟩, entry⟩, origins⟩, geometry⟩, decls⟩, muts⟩
    exact ⟨⟨keys.1, keys.2.1, keys.2.2.1, keys.2.2.2, storage, layouts, fun fn locals graph h =>
      let ⟨⟨unique, cfg, _⟩, sig⟩ := graphs fn locals graph h
      ⟨unique, cfg, sig⟩⟩, entry, origins, geometry, decls, muts⟩
  · rintro ⟨⟨fns, sigs, classes, slots, storage, layouts, graphs⟩, entry, origins, geometry, decls, muts⟩
    exact ⟨⟨⟨⟨⟨⟨⟨⟨⟨fns, sigs, classes, slots⟩, storage⟩, layouts⟩, fun fn locals graph h =>
      let ⟨unique, cfg, sig⟩ := graphs fn locals graph h
      ⟨⟨unique, cfg, valid sig⟩, sig⟩⟩, entry⟩, origins⟩, geometry⟩, decls⟩, muts⟩

private theorem ownerAccess_without_conflict (owners : Own.State) (id : Hybrid.Place)
    (mode : Own.Mode) (loan : Own.Loan) (active : loan ∈ owners.loans)
    (atRoot : loan.root = id) (conflict : mode = .mutable ∨ loan.mode = .mutable) :
    Own.ownerAccess owners id mode = false := by
  have denied : (owners.loans.all fun l =>
      l.root != id || (mode == .shared && l.mode == .shared)) = false := by
    apply Bool.eq_false_iff.mpr
    intro all
    have permit := List.all_eq_true.mp all loan active
    rcases conflict with write | unique <;> simp_all
  unfold Own.ownerAccess
  cases Own.slot owners id <;> simp [denied]

private theorem reinitialize_without_loan (owners : Own.State) (id : Hybrid.Place)
    (loan : Own.Loan) (active : loan ∈ owners.loans) (atRoot : loan.root = id) :
    Own.reinitialize owners id = none := by
  have denied : (owners.loans.all fun l => l.root != id) = false := by
    apply Bool.eq_false_iff.mpr
    intro all
    have permit := List.all_eq_true.mp all loan active
    simp_all
  unfold Own.reinitialize
  cases Own.slot owners id <;> simp [denied, bind, Option.bind]

/-- The permission check for a projected owner copy still excludes a mutable
loan on the root. -/
theorem copy_refused_under_mutable_loan {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (memory : Interfaces.Memory lang profile) (state : MemoryRegisters language profile)
    {ty : language.operands.Ty} (place : MemoryPlace _ ty) (loan : Own.Loan)
    (active : loan ∈ state.ownership.loans)
    (atRoot : AList.lookup state.addressIds place.logical.root = some loan.root)
    (unique : loan.mode = .mutable) :
    memory.readValue state (.copy place) = none := by
  cases h : memory.readValue state (.copy place) with
  | none => rfl
  | some result =>
    have access := ((Interfaces.Memory.copyRead_iff memory state place result.1 result.2).mp h).2.1
    have denied := ownerAccess_without_conflict
      (memory.ownerView state place.logical.root ty .owned).1
      (memory.ownerView state place.logical.root ty .owned).2
      .shared loan (show loan ∈ state.ownership.loans from active)
      (by simp [Interfaces.Memory.ownerView, atRoot]) (.inr unique)
    rw [denied] at access
    contradiction

/-- Both reinitialization and replacement consult the loan table at the
original root, including when the destination is a projected field. -/
theorem write_refused_under_loan {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (memory : Interfaces.Memory lang profile) (state : MemoryRegisters language profile)
    {ty : language.operands.Ty} (place : MemoryPlace _ ty) (loan : Own.Loan)
    (active : loan ∈ state.ownership.loans)
    (atRoot : AList.lookup state.addressIds place.logical.root = some loan.root)
    (value : language.operands.Val) :
    memory.writeValue state place value = none := by
  cases h : memory.writeValue state place value with
  | none => rfl
  | some next =>
    have access := Interfaces.Memory.write_authorized memory state place value h
    have root : loan.root = (memory.writeOwnerView state place).2 := by
      simp [Interfaces.Memory.writeOwnerView, Interfaces.Memory.ownerView, atRoot]
    have present : loan ∈ (memory.writeOwnerView state place).1.loans := active
    have deniedInit := reinitialize_without_loan _ _ loan present root
    have deniedReplace := ownerAccess_without_conflict _ _ .mutable loan present root (.inl rfl)
    simp [deniedInit, deniedReplace] at access

/-- Moving an entire root removes initialization and its zero-byte value.
Byte memory is unchanged even when there is a nonempty backing range. -/
theorem move_root_consumes {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (memory : Interfaces.Memory lang profile) (state next : MemoryRegisters language profile)
    {ty : language.operands.Ty} (place : MemoryPlace _ ty) (value : language.operands.Val)
    (whole : place.logical.path = []) (initialized : state.initialized.Nodup)
    (step : memory.readValue state (.move place) = some (value, next)) :
    place.logical.root ∉ next.initialized ∧
      AList.lookup next.zeroValues place.logical.root = none ∧ next.bytes = state.bytes := by
  simp only [Interfaces.Memory.readValue, whole, List.isEmpty_nil, Bool.not_true,
    Bool.false_eq_true, ite_false] at step
  cases hm : Own.move (memory.ownerView state place.logical.root ty .owned).1
      (memory.ownerView state place.logical.root ty .owned).2 with
  | none => simp [hm, bind, Option.bind] at step
  | some owners =>
    cases hv : memory.loadValue state place with
    | none => simp [hm, hv, bind, Option.bind] at step
    | some loaded =>
      simp [hm, hv, bind, Option.bind] at step
      obtain ⟨_, rfl⟩ := step
      exact ⟨initialized.not_mem_erase, by simp [AList.lookup_erase], rfl⟩

end Concrete.Memory
