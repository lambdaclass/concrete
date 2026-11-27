import Concrete.Memory.Refinement
import ConcreteSpec.Proofs.Memory

namespace Concrete.Memory.AggregateLayoutControl

open Concrete.Interfaces ConcreteSpec.Core ConcreteSpec.StageInterface

-- The first two checkpoints reproduce the round-6 review. The third leaves
-- enough room for the padded aggregate to demonstrate physical replay.
inductive Scenario where
  | full | sixFree | fourteenFree

private abbrev C : Nat := 2^64
private def Scenario.backingBase : Scenario → Nat
  | .full => 1 | .sixFree => 7 | .fourteenFree => 15
private def Scenario.backingSize (s : Scenario) := C - 1 - s.backingBase
private abbrev baseLang := sliceLanguage slice0
local instance : DecidableEq baseLang.operands.Ty := baseLang.typeDecidableEq
private abbrev aggregateTy : baseLang.operands.Ty := .inr true
private abbrev backingTy : baseLang.operands.Ty := .inr false
private abbrev lowering := sliceLoweringLanguage slice0
private abbrev profile : Profile := ⟨.lcb, .x86_64Linux, 4, 4096⟩
private abbrev mir := bindReturnControl slice0
private abbrev origin := mir.functions[0].2.2.blocks[0].body[0].origin
private abbrev decl : DeclId := ⟨mir.entry.module, ⟨"T"⟩⟩
private abbrev aggregateValue : baseLang.operands.Val := .other decl
private def fld : FieldId := ⟨.struct decl, ⟨"word"⟩⟩
private abbrev u32 : baseLang.operands.Ty := .inl (.scalar (.int .u32))
private abbrev u8 : baseLang.operands.Ty := .inl (.scalar (.int .u8))
private def word : baseLang.operands.Val := .scalar (.int .u32 42)
private def Scenario.offset (s : Scenario) (fixed : Bool) : Nat :=
  match s with | .full => if fixed then 4 else 2 | _ => 0
private def description (s : Scenario) (fixed : Bool) : Layout :=
  ⟨if fixed then 8 else 6, if fixed then 4 else 1, .cons fld (s.offset fixed) (.scalar 4 4) .nil⟩
private def bytes (s : Scenario) (fixed : Bool) : List Mem.MByte :=
  (List.range (description s fixed).size).map fun i =>
    .init (if i = 0 ∨ i = s.offset fixed then 42 else 0) none

private abbrev language (s : Scenario) (fixed : Bool) : CoreLanguage := {baseLang with
  operands := {baseLang.operands with isCopy := fun ty => ty == aggregateTy || ty == backingTy || baseLang.operands.isCopy ty}
  safety := fun op => match op with
    | .expression expr => baseLang.safety (.expression expr)
    | .primitive prim args => baseLang.safety (.primitive prim args)
    | .call callee args => baseLang.safety (.call callee args)
    | .abort => baseLang.safety .abort
  layout := fun env target ty =>
    if ty == aggregateTy then some (description s fixed)
    else if ty == backingTy then some (.scalar s.backingSize 1) else baseLang.layout env target ty
  literal := fun ty bs => if (ty == aggregateTy || ty == backingTy) && bs == [42] then some aggregateValue
    else baseLang.literal ty bs
  fieldType := fun ty f => if ty == aggregateTy && f == fld then some u32 else none
  field := fun _ path => if path == [fld] then some word else none
  replaceField := fun v path _ => if path == [fld] then some v else none
  encodeOther := fun _ ty (v : DeclId) => if v != decl then none
    else if ty == backingTy then some [(0, [.init 42 none])]
    else if ty == aggregateTy then some [(0, bytes s fixed)] else none
  decodeOther := fun _ ty bs =>
    if (ty == aggregateTy || ty == backingTy) && bs.head? == some (.init 42 none) then some decl else none}

private def dst : MemoryPlace _ aggregateTy := ⟨⟨⟨0⟩, []⟩, .materialized⟩
private def backing : PlaceId := ⟨1⟩
private def fieldDst : MemoryPlace _ u32 := ⟨⟨dst.logical.root, [fld]⟩, .materialized⟩
private def params : List (Sigma (MemoryPlace baseLang.operands.Ty)) := [⟨aggregateTy, dst⟩, ⟨u32, fieldDst⟩]
private def edge : CFGEdge (Sigma (MemoryRead _ lowering.literal)) :=
  ⟨⟨1⟩, [⟨aggregateTy, .literal ⟨[42]⟩⟩, ⟨u32, .literal ⟨[42,0,0,0]⟩⟩]⟩
private def backingPlace : MemoryPlace _ backingTy := ⟨⟨backing, []⟩, .materialized⟩
private def locals : LocalTypes baseLang.operands.Ty := [(dst.logical.root, aggregateTy), (backing, backingTy)]
private def entry : MemoryBlock lowering :=
  ⟨⟨0⟩, [], [⟨origin, .beginLifetime backing⟩, ⟨origin, .bind backingPlace (.literal ⟨[42]⟩)⟩], .jump edge, []⟩
private def dest : MemoryBlock lowering := ⟨⟨1⟩, params, [], .ret (some ⟨u8, .literal ⟨[42]⟩⟩), []⟩
private def memory (s : Scenario) (fixed : Bool) : Interfaces.Memory lowering profile :=
  {project := mir.project, entry := mir.entry, signatures := mir.signatures
   functions := [(mir.entry, locals, ⟨⟨0⟩, #[entry, dest]⟩)]
   storage := [(mir.entry, [(dst.logical.root, .materialized), (backing, .materialized)])]
   mutableLocals := [(mir.entry, [dst.logical.root])]
   layouts := [(mir.entry, [(dst.logical.root, description s fixed), (backing, .scalar s.backingSize 1)])]}

private def source : MIR lowering profile := {mir with
  mutableLocals := [(mir.entry, [dst.logical.root])]
  functions := [(mir.entry, locals, ⟨⟨0⟩, #[
    ⟨⟨0⟩, [], [⟨origin, .bind backingPlace.logical (.literal ⟨[42]⟩)⟩],
      .jump ⟨⟨1⟩, [⟨aggregateTy, .literal ⟨[42]⟩⟩, ⟨u32, .literal ⟨[42,0,0,0]⟩⟩]⟩, []⟩,
    ⟨⟨1⟩, [⟨aggregateTy, dst.logical⟩, ⟨u32, fieldDst.logical⟩], [], .ret (some ⟨u8, .literal ⟨[42]⟩⟩), []⟩]⟩)]}

private def regGraph {T : Type} (jump ret : T) :
    CFG (Sigma (SSAValue baseLang.operands.Ty)) (LocatedOp (SSAInst lowering)) T :=
  ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨origin, .beginLifetime backing⟩, ⟨origin, .store backingPlace.logical (.literal ⟨[42]⟩)⟩], jump, []⟩,
    ⟨⟨1⟩, [⟨aggregateTy, ⟨0⟩⟩, ⟨u32, ⟨1⟩⟩], [⟨origin, .beginLifetime dst.logical.root⟩,
      ⟨origin, .store dst.logical (.value ⟨0⟩)⟩, ⟨origin, .store fieldDst.logical (.value ⟨1⟩)⟩], ret, []⟩]⟩

private def regEdge : CFGEdge (Sigma (SSARead _ lowering.literal)) :=
  ⟨⟨1⟩, [⟨aggregateTy, .literal ⟨[42]⟩⟩, ⟨u32, .literal ⟨[42,0,0,0]⟩⟩]⟩
private def ssa (s : Scenario) (fixed : Bool) : SSA lowering profile :=
  {project := mir.project, entry := mir.entry, signatures := mir.signatures, layouts := (memory s fixed).layouts
   functions := [(mir.entry, locals, regGraph (.jump regEdge) (.ret (some ⟨u8, .literal ⟨[42]⟩⟩)))]}
private def bir (s : Scenario) (fixed : Bool) : BIR lowering profile :=
  {project := mir.project, entry := mir.entry, signatures := mir.signatures, layouts := (memory s fixed).layouts, sites := #[]
   functions := [(mir.entry, locals, regGraph (.jump regEdge) (.ret (some ⟨u8, .literal ⟨[42]⟩⟩)))]}

private theorem geometry_required {m : Interfaces.Memory lowering profile} (h : m.LayoutsWF)
    {table slot} (t : table ∈ m.layouts) (r : slot ∈ table.2) : slot.2.WellFormed profile.memoryTarget :=
  h table t slot r

theorem malformed_refused (s : Scenario) :
    lowerGraph (language := language s false) source = .error (.layout ⟨mir.entry, dst.logical.root, aggregateTy⟩) ∧
    ¬ (memory s false).HandoffWF ∧ ¬ (ssa s false).WF (language := language s false) ∧
    ¬ (bir s false).WF (language := language s false) := by
  have bad : ¬ (description s false).WellFormed profile.memoryTarget := by cases s <;> decide
  refine ⟨by cases s <;> with_unfolding_all rfl, ?_, ?_, ?_⟩
  · exact fun h => bad (geometry_required h.2.2.2 (.head _) (.head _))
  · exact fun h => bad (geometry_required h.2.2.2 (.head _) (.head _))
  · exact fun h => bad (geometry_required h.2.2.2.2.2.2 (.head _) (.head _))

private def oneByte (align : Nat) : BIR lowering profile :=
  {project := mir.project, entry := mir.entry, signatures := mir.signatures, sites := #[]
   layouts := [(mir.entry, [(⟨0⟩, .scalar 1 align)])]
   functions := [(mir.entry, [(⟨0⟩, u8)], ⟨⟨0⟩, #[
     ⟨⟨0⟩, [], [⟨origin, .beginLifetime ⟨0⟩⟩], .ret (some ⟨u8, .literal ⟨[42]⟩⟩), []⟩]⟩)]}

theorem bir_u8_refused : ¬ (oneByte C).WF (language := baseLang) := by
  intro h
  exact (by decide : ¬ (Layout.scalar 1 C).WellFormed profile.memoryTarget)
    (geometry_required h.2.2.2.2.2.2 (.head _) (.head _))

private abbrev State := MemoryRegisters baseLang profile
private abbrev exec (s : Scenario) := Interfaces.Memory.Execution (language := language s true) (lang := lowering) (profile := profile)
private def start : State := ⟨⟨mir.entry, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, .empty, [], [], {}, 0, false⟩
private def reserve (q : State) (root : PlaceId) (base size align : Nat) : State :=
  let ptr : Mem.Ptr := ⟨⟨0⟩, base, some ⟨q.bytes.next⟩⟩
  {q with live := root :: q.live, materialized := (root, ⟨ptr, size, align⟩) :: q.materialized
          bytes := ⟨(⟨q.bytes.next⟩, Mem.candidate ⟨0⟩ base size align) :: q.bytes.blocks, q.bytes.next + 1⟩}
private def openBacking (s : Scenario) := (reserve start backing s.backingBase s.backingSize 1).advance (language := baseLang)
private def filled (s : Scenario) :=
  ((memory s true).writeValue (language := language s true) (openBacking s) backingPlace aggregateValue).getD start
private def atJump (s : Scenario) := (filled s).advance (language := baseLang)
private def arrive (s : Scenario) : State := {atJump s with block := edge.target, instruction := 0}
private abbrev vals := [aggregateValue, word]
private theorem reachable (s : Scenario) : (exec s).finite (memory s true) (atJump s) := by
  refine ⟨start, by cases s <;> with_unfolding_all rfl, .cons (b := openBacking s) ?_ (.cons ?_ (.nil _))⟩
  · cases s <;> with_unfolding_all exact ⟨rfl, ⟨_, rfl, _, ⟨nofun, .inr ⟨rfl, _, rfl, .inr
      ⟨by decide, ⟨0⟩, _, ⟨0⟩, _, rfl, rfl⟩⟩⟩, rfl⟩, [], rfl⟩
  · have write : (memory s true).Writes (language := language s true) (openBacking s) backingPlace aggregateValue (filled s) := by
      cases s <;> with_unfolding_all
        refine ⟨⟨rfl, nofun⟩, fun _ => ⟨_, rfl, nofun, nofun⟩, rfl, rfl,
          fun _ => .inr ⟨_, [(0, [.init 42 none])], rfl, rfl, fun _ => ⟨0, _, .head _, by decide⟩, ?_⟩⟩
        intro k bs mem
        cases List.mem_singleton.mp mem
        exact ⟨by decide, by decide, rfl⟩
    cases s <;> with_unfolding_all exact ⟨rfl, ⟨_, rfl, aggregateValue, _, _, rfl, write, rfl⟩, [], rfl⟩

private theorem jump (s : Scenario) :
    (memory s true).JumpAt (language := language s true) (atJump s) (atJump s) dest edge vals := by
  cases s <;> with_unfolding_all exact ⟨rfl, ⟨entry, rfl, rfl, rfl⟩, rfl, rfl, rfl⟩

private theorem handoff (s : Scenario) : (memory s true).HandoffWF :=
  (checkHandoff_iff _).mp (by cases s <;> with_unfolding_all decide)

theorem padded_lowering (s : Scenario) :
    lowerGraph (language := language s true) source = .ok (memory s true) ∧
    (memory s true).HandoffWF ∧ (ssa s true).WF (language := language s true) ∧ (bir s true).WF (language := language s true) := by
  refine ⟨by cases s <;> with_unfolding_all rfl, handoff s, (checkHandoff_iff _).mp ?_, ?_⟩
  · cases s <;> with_unfolding_all decide
  · refine ⟨?_, by cases s <;> decide, by cases s <;> decide, by cases s <;> decide, nofun, ?_⟩
    · intro fn member block present slot trap
      cases List.mem_singleton.mp member
      rcases List.mem_cons.mp present with rfl | tail
      · contradiction
      · cases List.mem_singleton.mp tail
        contradiction
    · have checked : ((bir s true).memoryView (language := language s true)).HandoffWF :=
        (checkHandoff_iff _).mp (by cases s <;> with_unfolding_all decide)
      exact ⟨checked.1, checked.2.2.2⟩

-- This witness deliberately lies beyond the computed bound (C + 12).
private abbrev farBase := C + 16
private abbrev farCeiling := farBase + 9
private def opened (s : Scenario) := reserve (arrive s) dst.logical.root farBase 8 4
private def stored (s : Scenario) :=
  ((memory s true).writeValue (language := language s true) (opened s) dst aggregateValue farCeiling).getD start
private def written (s : Scenario) :=
  ((memory s true).writeValue (language := language s true) (stored s) fieldDst word farCeiling).getD start

private theorem valid (s : Scenario) : (memory s true).ParamsValid (language := language s true) params vals (arrive s) farCeiling := by
  refine ⟨by cases s <;> with_unfolding_all decide, written s,
    .cons (language := language s true) (place := dst) (started := opened s) (stored := stored s) ?_ ?_
      (.cons (language := language s true) (memory := memory s true) (ceiling := farCeiling)
        (state := stored s) (place := fieldDst) (started := stored s) (by cases s <;> with_unfolding_all rfl) ?_ (.nil _))⟩
  · cases s <;> with_unfolding_all exact ⟨by decide, .inr ⟨rfl, _, rfl, .inr
      ⟨by decide, ⟨0⟩, farBase, ⟨1⟩, _, by rw [Mem.Memory.alloc, ite_eq_left (by decide)]; rfl, rfl⟩⟩⟩
  · cases s <;> with_unfolding_all
      refine ⟨⟨rfl, nofun⟩, fun _ => ⟨_, rfl, nofun, nofun⟩, rfl, rfl, fun _ => .inr
        ⟨_, _, rfl, rfl, fun _ => ⟨0, _, .head _, by decide⟩, ?_⟩⟩
      intro k bs member
      cases List.mem_singleton.mp member
      exact ⟨by decide, by decide, rfl⟩
  · cases s <;> with_unfolding_all
      refine ⟨⟨rfl, fun _ => rfl⟩, fun _ => ⟨_, rfl, nofun, nofun⟩, rfl, rfl, fun _ => .inr
        ⟨_, _, rfl, rfl, fun _ => ⟨0, _, .head _, by decide⟩, ?_⟩⟩
      intro k bs member
      cases List.mem_singleton.mp member
      exact ⟨by decide, by decide, rfl⟩

private def backingBlock (s : Scenario) : Mem.Block :=
  {Mem.candidate ⟨0⟩ s.backingBase s.backingSize 1 with bytes := [.init 42 none] ++ List.replicate (s.backingSize - 1) .uninit}

private theorem noFit (s : Scenario) (tight : s ≠ .fourteenFree) :
    ¬ (memory s true).ParamSpaceFits (language := language s true) params (arrive s) := by
  have request : (memory s true).freshLayouts (language := language s true) (arrive s)
      (params.map fun p => p.2.logical.root) = [description s true] := by cases s <;> with_unfolding_all rfl
  simp only [Interfaces.Memory.ParamSpaceFits, request]
  rintro ⟨space, base, id, heap, alloc, _⟩
  have fits := (Mem.alloc_result _ _ _ _ _ _ _ _ alloc).2.2
  obtain rfl : space = ⟨0⟩ := List.mem_singleton.mp fits.2.2.2.2.2.2
  have hi : base + 8 < C := fits.2.2.1
  have nz : base ≠ 0 := fits.2.2.2.2.1 (by decide : 0 < 8)
  have block : (⟨0⟩, backingBlock s) ∈ (arrive s).bytes.blocks := by cases s <;> with_unfolding_all exact .head _
  apply fits.2.2.2.2.2.1 _ block rfl
  have small : s.backingBase ≤ 7 := by cases s <;> first | decide | exact (tight rfl).elim
  have endAt : s.backingBase + s.backingSize = C - 1 := by cases s <;> decide
  exact ⟨rfl, by change 0 < s.backingSize; cases s <;> decide, (by decide : 0 < 8),
    by change s.backingBase < base + 8; omega,
    by change base < s.backingBase + s.backingSize; rw [endAt]; omega⟩

theorem padded_exhausts (s : Scenario) (tight : s ≠ .fourteenFree) :
    (exec s).finite (memory s true) (atJump s) ∧
    (exec s).step (memory s true) (atJump s) {atJump s with resourceExhausted := true} ∧
    (exec s).result (memory s true) {atJump s with resourceExhausted := true} .resourceExhausted :=
  ⟨reachable s, (Interfaces.Memory.JumpAt.exhausted_iff (language := language s true) (handoff s).2.2.2 (jump s)).mpr ⟨⟨_, valid s⟩, noFit s tight⟩⟩

theorem padded_completes :
    let s := Scenario.fourteenFree
    (exec s).finite (memory s true) (atJump s) ∧ ∃ next, (exec s).step (memory s true) (atJump s) next ∧
      next.resourceExhausted = false := by
  let s := Scenario.fourteenFree
  exact ⟨reachable s, Interfaces.Memory.JumpAt.normal_of_fit (language := language s true) (handoff s).2.2.2 (jump s) (valid s)
    (by with_unfolding_all exact ⟨⟨0⟩, 4, ⟨1⟩, _, rfl, trivial⟩)⟩

-- A valid outer edge cannot hide a malformed grandchild.
example : ¬ (⟨16, 8, .cons fld 8 (description .full false) .nil⟩ : Layout).WellFormed profile.memoryTarget := by decide

end Concrete.Memory.AggregateLayoutControl
