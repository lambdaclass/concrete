import Concrete.Interfaces.Pipeline
import Concrete.Backend.EmitLLVM
import Concrete.Backend.EmitLCB
import Concrete.Backend.LCBWordLemmas

/-!
# Production backend emitter selection, LCB semantic preservation, and validator

Connects the production two-module LLVM emitter (`Concrete.Backend.EmitLLVM`) and the
production pure LCB emitter (`Concrete.Backend.EmitLCB`) to the frozen
`Concrete.Interfaces.Pipeline` emitter boundary (`PassFunctions.emit` / `PassFunctions.emitEntry`):

1. **Unified profile-selected emitter (`emit`)**: dispatches `.llvm` profiles to
   `EmitLLVM.emit` and `.lcb` profiles to `EmitLCB.emitUnvalidated` so that
   `Concrete.Interfaces.emitGuarded emit` agrees identically with `EmitLLVM.emit` on `.llvm`
   and with `EmitLCB.emit` on `.lcb` (enforcing `ISS-0115` and `ISS-0117` width policy via
   `firstUnsupported` and preserving Case 4 `invalidLCBJob` classification).
2. **Entry-only reachability theorem (`entry_no_reachable_calls`, `bir_reachable_stays_in_entry`)**:
   proves explicitly that `bir.requiresLCBLinking = false` and `firstUnsupported = none` on a
   well-formed LCB `BIR` exclude all `.call` (and storage/lifetime/destructure) instructions
   from every entry-reachable block, so every reachable `BIR.Execution` state stays in
   `bir.entry` with `callers = []`, justifying `EmitLCB.unreachableHelperBody` for non-entry
   helper jobs in `additionalJobs`.
3. **Constructive LCB semantic preservation (`emitLCB_refines`)**: proves full safe-source
   backward `BehaviorRefinement` from `BIR.Execution` to pinned `LCBInput.Execution`
   (`Concrete.Backend.lcbSemanticsFor`) via `Concrete.Backend.LCBWordLemmas` (`wrap_steps`,
   `checkedAdd_steps`, `u8_wordBits_*`, `trapWordBits_*`), covering termination (ordinary `u8`
   exits and traps with exact stderr), constructive divergence without `Classical.choice`,
   and target safety across entry trampolines and split branch edges.
4. **Full `LCBEncoding`, `EmitsLCB`, refusal classification, and universal `(registry passes).emit.Validated`**:
   completes Case 4 of `EmitLCB.emit_error_of_wf`, proves `(registry passes).emit.Validated`
   universally across both LLVM and LCB profiles, `Stage.Sound passes .emit` unconditionally
   on LCB, and `Stage.Sound passes .emit` on LLVM under `P_print`.

## Import-closure note for C3b provisional retirement
`Concrete.lean` imports `Concrete.Backend.ValidateEmit`, which imports both
`Concrete.Backend.EmitLLVM` and `Concrete.Backend.EmitLCB` directly without any provisional
`Concrete.BIR`, `Concrete.Backend.LLVM`, or `Concrete.Backend.LCB` imports. When C3b retires
provisional `Concrete/Backend/LLVM.lean` and `Concrete/Backend/LCB.lean`, both production
emitters remain in `Concrete.lean`'s import closure through this module.
-/

namespace Concrete.Backend.ValidateEmit

open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface
open Concrete.Contracts
open Concrete.Interfaces
open Concrete.Backend.LCBWordLemmas
open Concrete.Backend.EmitLCB
open Concrete.Backend.Runtime (indexed)
open LeanCompilerBackend (Width)
open LeanCompilerBackend.Verified

/-! ## 1. Unified profile-selected production backend emitter -/

/-- Unified production backend emitter wired into `PassFunctions.emit`.
On `.llvm` profiles it delegates to `EmitLLVM.emit`. On `.lcb` profiles it delegates to
`EmitLCB.emitUnvalidated` so that `emitGuarded` applies `validateJobs` and classifies
validation rejections cleanly under Case 4 of `PassFunctions.emitEntry.refusal`. -/
def emit {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} :
    {profile : Profile} →
    BIR lang profile → Except StageFailure (BackendOutput profile)
  | ⟨.llvm, _, _, _⟩, bir => EmitLLVM.emit (language := language) bir
  | ⟨.lcb, _, _, _⟩, bir => EmitLCB.emitUnvalidated (language := language) bir

/-- On every `.llvm` profile, `emitGuarded emit` agrees identically with `EmitLLVM.emit`. -/
theorem emitGuarded_emit_llvm {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty}
    {target : Backend.LCB.Target} {maxSlots maxStack : Nat}
    (bir : BIR lang ⟨.llvm, target, maxSlots, maxStack⟩) :
    emitGuarded (language := language) (emit (language := language)) bir =
      EmitLLVM.emit (language := language) bir := by
  unfold emitGuarded
  dsimp only
  cases hSup : firstUnsupported (language := language) bir with
  | some cause =>
    exact (EmitLLVM.emit_refusal_of_firstUnsupported (language := language) rfl hSup).symm
  | none => rfl

/-- On every `.lcb` profile, `emitGuarded emit` agrees identically with `EmitLCB.emit`. -/
theorem emitGuarded_emit_lcb {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty}
    {target : Backend.LCB.Target} {maxSlots maxStack : Nat}
    (bir : BIR lang ⟨.lcb, target, maxSlots, maxStack⟩) :
    emitGuarded (language := language) (emit (language := language)) bir =
      EmitLCB.emit (language := language) bir :=
  (EmitLCB.emit_eq_emitGuarded (language := language) bir).symm

/-! ## 2. Value encoding, constructive range lemmas, and transparent semantic mirrors -/

/-- Encode an `EarlyValue` (`(sliceLanguage slice).operands.Val`) as a 32-bit LCB `Source.Value`,
matching `EmitLCB.decodeLiteralWord`. -/
def encVal : EarlyValue → Source.Value
  | .scalar (.int _ n) => word n
  | .scalar (.bool b) => boolWord b
  | .scalar (.char c) => word (Int.ofNat c.val.toNat)
  | .scalar .unit | .other _ => word 0

theorem encVal_width (v : EarlyValue) : (encVal v).width = .word := by
  cases v with
  | other _ => rfl
  | scalar s => cases s <;> rfl

/-- Value-range invariant maintained across BIR virtual registers: every stored integer
value lies within its `IntTy.InRange`. -/
def ValInRange : EarlyValue → Prop
  | .scalar (.int t n) => t.InRange n
  | _ => True

private theorem leValue_lt_local : ∀ bs : List (Fin 256), Mem.leValue bs < 256 ^ bs.length
  | [] => by simp [Mem.leValue]
  | b :: bs => by
    have ih := leValue_lt_local bs
    have hb : b.val < 256 := b.isLt
    simp only [Mem.leValue, List.length_cons, Nat.pow_succ]
    omega

private theorem pow256_byteSize_eq_pow2_bits (t : IntTy) :
    256 ^ t.byteSize = 2 ^ t.bits := by
  cases t <;> rfl

/-- Constructive range bound for `ofBits` (avoids `ConcreteSpec.Core.ofBits_inRange`, which
depends on `Classical.choice`). -/
private theorem ofBits_inRange_local (t : IntTy) (bits : Nat) (hLt : bits < 2 ^ t.bits) :
    t.InRange (ofBits t bits) := by
  unfold ofBits IntTy.InRange
  split
  · rename_i hCond
    obtain ⟨hSigned, hGe⟩ := hCond
    cases t with
    | i8 | i16 | i32 | i64 =>
      simp only [IntTy.min, IntTy.max, IntTy.signed, IntTy.bits, ↓reduceIte, Nat.reduceSub,
        Nat.reducePow, Int.reducePow] at hLt hGe ⊢
      exact ⟨by omega, by omega⟩
    | u8 | u16 | u32 | u64 => cases hSigned
  · rename_i hNot
    have hSignedLt (hs : t.signed = true) : bits < 2 ^ (t.bits - 1) := by
      cases Nat.lt_or_ge bits (2 ^ (t.bits - 1)) with
      | inl h => exact h
      | inr h => exact absurd ⟨hs, h⟩ hNot
    cases t with
    | i8 | i16 | i32 | i64 =>
      have hHalf := hSignedLt rfl
      simp only [IntTy.min, IntTy.max, IntTy.signed, IntTy.bits, ↓reduceIte, Nat.reduceSub,
        Nat.reducePow, Int.reducePow] at hLt hHalf ⊢
      exact ⟨by omega, by omega⟩
    | u8 | u16 | u32 | u64 =>
      simp only [IntTy.min, IntTy.max, IntTy.signed, IntTy.bits, Bool.false_eq_true, ↓reduceIte,
        Nat.reducePow, Int.reducePow] at hLt ⊢
      exact ⟨by omega, by omega⟩

private theorem sliceLiteral_inRange {slice : Slice} {ty : (sliceLanguage slice).operands.Ty}
    {bytes : List UInt8} {v : (sliceLanguage slice).operands.Val}
    (h : (sliceLanguage slice).literal ty bytes = some v) :
    ValInRange v := by
  unfold sliceLanguage at h
  dsimp only at h
  cases ty with
  | nominal _ => cases h
  | scalar ct =>
    cases ct with
    | bool | char | unit => cases h
    | int t =>
      dsimp only at h
      split at h
      · rename_i hLen
        cases Option.some.inj h
        have hLt : Mem.leValue (bytes.map UInt8.toFin) < 2 ^ t.bits := by
          have h1 := leValue_lt_local (bytes.map UInt8.toFin)
          rw [List.length_map, hLen, pow256_byteSize_eq_pow2_bits] at h1
          exact h1
        exact ofBits_inRange_local t _ hLt
      · cases h

private theorem decodeLiteralWord_slice {slice : Slice} {ty : (sliceLanguage slice).operands.Ty}
    (lit : Literal (sliceLanguage slice).operands.Ty ty) :
    EmitLCB.decodeLiteralWord (sliceLanguage slice) lit =
      ((sliceLanguage slice).literal ty lit.encoding).map encVal := by
  unfold EmitLCB.decodeLiteralWord
  cases (sliceLanguage slice).literal ty lit.encoding with
  | none => rfl
  | some v =>
    cases v with
    | other _ => rfl
    | scalar s => cases s <;> rfl

/-- Transparent mirror of `LCBInput.runtimeResult`. -/
private def lcbRuntimeResult {profile : Profile} (project : Project) (output : LCBInput profile) :
    Source.Value → Option Concrete.Contracts.Observation
  | .word w =>
    if w.toNat < 256 then
      some (.returned (UInt8.ofNat w.toNat) ByteArray.empty)
    else do
      let entry ← output.sites[w.toNat - 256]?
      let site ← runtimeSite project entry.origin
      return .trapped 134 entry.kind site (renderTrapLine entry.kind site).toUTF8
  | _ => none

private theorem lcbExecution_result_iff {E : Type} {profile : Profile}
    (project : Project) (output : LCBInput profile) (q : Option Source.State)
    (o : Concrete.Contracts.Observation) :
    (LCBInput.Execution E project).result output q o ↔
      (do let state ← q
          match Source.step output.body state with
          | .ok (.returned value) => lcbRuntimeResult project output value
          | _ => none) = some o := by
  cases q with
  | none => with_unfolding_all exact Iff.rfl
  | some s =>
    change (match Source.step output.body s with | .ok (.returned v) => _ | _ => none) = some o ↔
      (match Source.step output.body s with | .ok (.returned value) => lcbRuntimeResult project output value | _ => none) = some o
    cases hSt : Source.step output.body s with
    | error _ => exact Iff.rfl
    | ok tr =>
      cases tr with
      | next _ => exact Iff.rfl
      | returned v =>
        cases v <;> with_unfolding_all exact Iff.rfl

private theorem lcbExecution_trace_eq {E : Type} {profile : Profile}
    (project : Project) (output : LCBInput profile) (q : Option Source.State) :
    (LCBInput.Execution E project).trace output q =
      ((do let state ← q
           match Source.step output.body state with
           | .ok (.returned value) => lcbRuntimeResult project output value
           | _ => none).map fun (o : Concrete.Contracts.Observation) =>
        o.stderr.data.toList.map ProcessEvent.stderr).getD [] := by
  cases q with
  | none => with_unfolding_all rfl
  | some s =>
    change ((match Source.step output.body s with | .ok (.returned v) => _ | _ => none).map
        fun (o : Concrete.Contracts.Observation) => o.stderr.data.toList.map ProcessEvent.stderr).getD [] =
      ((match Source.step output.body s with | .ok (.returned value) => lcbRuntimeResult project output value | _ => none).map
        fun (o : Concrete.Contracts.Observation) => o.stderr.data.toList.map ProcessEvent.stderr).getD []
    cases hSt : Source.step output.body s with
    | error _ => rfl
    | ok tr =>
      cases tr with
      | next _ => rfl
      | returned v =>
        cases v <;> with_unfolding_all rfl

/-! ### Transparent mirror of `BIR.memoryView` (polymorphic in `language` / `lang`) -/

private def birBase {Ty : Type} {lang : LoweringLanguage Ty} (locals : LocalTypes Ty)
    (graph : BIRGraph lang) : Nat :=
  let declaredLimit := List.foldl (fun limit (root, _) => max limit (root.id + 1)) 0 locals
  graph.blocks.foldl (fun limit block =>
    block.body.foldl (fun limit op =>
      match op.instruction with
      | .beginLifetime root | .endLifetime root => max limit (root.id + 1)
      | _ => limit) limit) declaredLimit

private def birReg {Ty : Type} (base : Nat) {ty : Ty} (v : SSAValue Ty ty) : MemoryPlace Ty ty :=
  ⟨⟨⟨base + v.id⟩, []⟩, .virtual⟩

private def birMat {Ty : Type} {ty : Ty} (p : Place Ty ty) : MemoryPlace Ty ty :=
  ⟨p, .materialized⟩

private def birRead {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    (base : Nat) {ty : language.operands.Ty} :
    SSARead language.operands.Ty lang.literal ty →
    MemoryRead language.operands.Ty lang.literal ty
  | .literal lit => .literal lit
  | .value v =>
    if language.operands.isCopy ty then .copy (birReg base v) else .move (birReg base v)

private def birOpnd {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    (base : Nat) (arg : Sigma (SSARead language.operands.Ty lang.literal)) :
    Sigma (MemoryRead language.operands.Ty lang.literal) :=
  ⟨arg.1, birRead (language := language) base arg.2⟩

private def birArgs {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    (base : Nat) :
    {types : List language.operands.Ty} →
    Args (SSARead language.operands.Ty lang.literal) types →
    Args (MemoryRead language.operands.Ty lang.literal) types
  | _, .nil => .nil
  | _, .cons arg rest => .cons (birRead (language := language) base arg) (birArgs (language := language) base rest)

private def birInst {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    (base : Nat) : SSAInst lang → MemoryInst lang
  | .op dst op args addresses =>
    .op (birReg base dst) op (birArgs (language := language) base args) []
      (addresses.map fun p => ⟨p.1, birMat p.2⟩)
  | .addWithOverflow t dst flag lhs rhs =>
    .addWithOverflow t (birReg base dst) (birReg base flag)
      (birRead (language := language) base lhs) (birRead (language := language) base rhs)
  | .load (ty := ty) dst src =>
    .bind (birReg base dst)
      (if language.operands.isCopy ty then .copy (birMat src) else .move (birMat src))
  | .destructure src bindings =>
    .destructure (match src with | .register src => birReg base src | .materialized src => birMat src)
      (bindings.map fun (field, dst) => (field, ⟨dst.1, birReg base dst.2⟩))
  | .store dst src => .assign (birMat dst) (birRead (language := language) base src)
  | .beginLifetime place => .beginLifetime place
  | .endLifetime place => .endLifetime place
  | .call fn dst args => .call fn (dst.map fun p => ⟨p.1, birReg base p.2⟩)
      (args.map (birOpnd (language := language) base)) []

private def birRegDecls {Ty : Type} {lang : LoweringLanguage Ty} (base : Nat) :
    SSAInst lang → LocalTypes Ty
  | .op (ty := ty) dst .. | .load (ty := ty) dst .. => [((birReg base dst).logical.root, ty)]
  | .addWithOverflow t dst flag .. =>
    [((birReg base dst).logical.root, lang.intTy t),
     ((birReg base flag).logical.root, lang.bool)]
  | .destructure _ bindings =>
    bindings.map fun (_, dst) => ((birReg base dst.2).logical.root, dst.1)
  | .call _ (some ⟨ty, dst⟩) _ => [((birReg base dst).logical.root, ty)]
  | _ => []

private def birTerm {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) (base : Nat) :
    BIRTerm (Sigma (SSARead language.operands.Ty lang.literal))
      (SSARead language.operands.Ty lang.literal lang.bool) →
    MemoryTerm lang
  | .ret value => .ret (value.map (birOpnd (language := language) base))
  | .jump edge => .jump ⟨edge.target, edge.arguments.map (birOpnd (language := language) base)⟩
  | .branch test yes no => .branch (birRead (language := language) base test)
      ⟨yes.target, yes.arguments.map (birOpnd (language := language) base)⟩
      ⟨no.target, no.arguments.map (birOpnd (language := language) base)⟩
  | .trap slot => match bir.sites[slot.index]? with
    | some entry => .trap entry.kind entry.origin
    | none => .invalid

private def birMemoryGraph {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) (base : Nat) (graph : BIRGraph lang) :
    MemoryGraph lang :=
  ⟨graph.entry, graph.blocks.map fun block =>
    ⟨block.id, block.params.map (fun p => ⟨p.1, birReg base p.2⟩),
      block.body.map (fun op => ⟨op.origin, birInst (language := language) base op.instruction⟩),
      birTerm (language := language) bir base block.ending, block.boundaryOrigins⟩⟩

private def birMemoryView {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) :
    Memory lang profile :=
  let views := List.map (fun (fn, locals, graph) =>
    let base := birBase locals graph
    let registers := graph.blocks.toList.flatMap fun block =>
      (block.params.map fun p => ((birReg base p.2).logical.root, p.1)) ++
      (block.body.flatMap fun op => birRegDecls base op.instruction)
    let body := birMemoryGraph (language := language) bir base graph
    ((fn, locals ++ registers, body),
      (fn, List.map (fun p => (p.1, StorageClass.materialized)) locals ++
        List.map (fun p => (p.1, StorageClass.virtual)) registers),
      (fn, List.map Prod.fst registers))) bir.functions
  let mutableLocals := views.foldl (fun table (_, _, fn, registers) =>
    if registers.isEmpty then table
    else (fn, registers ++ (AList.lookup table fn).getD []) :: AList.erase table fn) bir.mutableLocals
  ⟨bir.project, bir.entry, bir.signatures, views.map Prod.fst, bir.layouts,
    views.map (·.2.1), mutableLocals, bir.declarations⟩

private theorem birMemoryView_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (bir : BIR lang profile) :
    birMemoryView (language := language) bir = bir.memoryView (language := language) := by
  conv => rhs; whnf
  unfold birMemoryView
  dsimp only
  let mkMem :
      List ((FnId × LocalTypes language.operands.Ty × MemoryGraph lang) ×
        (FnId × AList PlaceId StorageClass) × (FnId × List PlaceId)) →
      Memory lang profile := fun views =>
    let mutableLocals := views.foldl (fun table (_, _, fn, registers) =>
      if registers.isEmpty then table
      else (fn, registers ++ (AList.lookup table fn).getD []) :: AList.erase table fn) bir.mutableLocals
    ⟨bir.project, bir.entry, bir.signatures, views.map Prod.fst, bir.layouts,
      views.map (·.2.1), mutableLocals, bir.declarations⟩
  apply congrArg mkMem
  apply congrArg (fun f => List.map f bir.functions)
  funext fnEntry
  rcases fnEntry with ⟨fn, locals, graph⟩
  dsimp only [birMemoryGraph]
  have hBaseRw : ∀ (f : Nat → LocatedOp (SSAInst lang) → Nat)
      (g : Nat → PlaceId × language.operands.Ty → Nat),
      (∀ n op, f n op = match op.instruction with
        | .beginLifetime root | .endLifetime root => max n (root.id + 1)
        | _ => n) →
      (∀ n x, g n x = max n (x.1.id + 1)) →
      Array.foldl (fun limit block => List.foldl f limit block.body) (List.foldl g 0 locals) graph.blocks =
      birBase locals graph := by
    intro f g hf hg
    unfold birBase
    dsimp only
    congr 1
    · funext n blk; congr 1; funext n op; rw [hf]; cases op.instruction <;> rfl
    · congr 1; funext n x; rcases x with ⟨r, _⟩; exact hg n (r, _)
  conv => rhs; rw [hBaseRw _ _ (fun _ op => by cases op.instruction <;> rfl) (fun _ ⟨_, _⟩ => rfl)]
  have mkEntry_congr : ∀ {r₁ r₂ : LocalTypes language.operands.Ty} {g₁ g₂ : MemoryGraph lang},
      r₁ = r₂ → g₁ = g₂ →
      ((fn, locals ++ r₁, g₁),
        (fn, List.map (fun p => (p.1, StorageClass.materialized)) locals ++
          List.map (fun p => (p.1, StorageClass.virtual)) r₁),
        (fn, List.map Prod.fst r₁)) =
      ((fn, locals ++ r₂, g₂),
        (fn, List.map (fun p => (p.1, StorageClass.materialized)) locals ++
          List.map (fun p => (p.1, StorageClass.virtual)) r₂),
        (fn, List.map Prod.fst r₂)) := fun hr hg => hr ▸ hg ▸ rfl
  refine mkEntry_congr ?_ ?_
  · apply congrArg (fun f => List.flatMap f graph.blocks.toList)
    funext blk; congr 1
    apply congrArg (fun f => List.flatMap f blk.body)
    funext op
    cases op.instruction with
    | call _ dst _ => cases dst <;> rfl
    | _ => rfl
  · congr 1
    apply congrArg (fun f => Array.map f graph.blocks)
    funext blk; congr 1
    · apply congrArg (fun f => List.map f blk.body)
      funext op
      apply congrArg (LocatedOp.mk op.origin)
      cases op.instruction with
      | op dst fop args addresses =>
        apply congrArg (fun a => MemoryInst.op (birReg (birBase locals graph) dst) fop a []
          (addresses.map fun p => ⟨p.1, birMat p.2⟩))
        clear fop
        induction args with
        | nil => rfl
        | cons a r ih =>
          change Args.cons (birRead (language := language) _ a) (birArgs (language := language) _ r) =
            Args.cons _ _
          rw [ih]; cases a <;> rfl
      | addWithOverflow _ _ _ lhs rhs => cases lhs <;> cases rhs <;> rfl
      | load _ _ => rfl
      | destructure src _ => cases src <;> rfl
      | store _ src => cases src <;> rfl
      | beginLifetime _ | endLifetime _ => rfl
      | call _ _ _ => congr 1

private def birEntryState {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (memory : Memory lang profile) : MemoryRegisters language profile :=
  let graph := memory.graphAt memory.entry
  let id := (graph.map CFG.entry).getD ⟨0⟩
  let block := graph.bind (·.blocks.find? (·.id == id))
  let pc := (block.map fun block =>
    if block.params.length = 0 then 0 else block.body.length + 1).getD 0
  ⟨⟨memory.entry, id, pc, [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩

private def birHostedResult {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (memory : Memory lang profile) (state : MemoryRegisters language profile) :
    Option Concrete.Contracts.Observation := do
  match ← memory.terminalObservation state with
  | .returned status bytes => return .returned status bytes
  | .trapped status kind _ bytes =>
    let block ← memory.blockAt state
    let origin ← match block.body[state.instruction]? with
      | some op => some op.origin
      | none => match block.ending with
        | .trap _ origin => some origin
        | _ => none
    let site ← runtimeSite memory.project origin
    return .trapped status kind site bytes
  | .startupRefused status bytes => return .startupRefused status bytes
  | .resourceExhausted => return .resourceExhausted

private theorem birEntryState_eq {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) :
    BIR.entryState (language := language) bir =
      birEntryState (birMemoryView (language := language) bir) := by
  rw [BIR.entryState, ← birMemoryView_eq bir]; rfl

private theorem birHostedResult_eq {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) (state : MemoryRegisters language profile) :
    BIR.hostedResult (language := language) bir state =
      birHostedResult (birMemoryView (language := language) bir) state := by
  rw [BIR.hostedResult, ← birMemoryView_eq bir]; rfl

/-! ### Transparent properties of `birMemoryView` -/

private theorem alist_lookup_map_key {κ α β : Type} [DecidableEq κ]
    (f : α → β) (l : AList κ α) (k : κ) :
    AList.lookup (List.map (fun (a, b) => (a, f b)) l) k = (AList.lookup l k).map f := by
  induction l with
  | nil => rfl
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [List.map_cons, AList.lookup]
    split
    · rfl
    · exact ih

private theorem alist_lookup_mem {κ α : Type} [DecidableEq κ] {l : AList κ α}
    {k : κ} {v : α} (h : AList.lookup l k = some v) : (k, v) ∈ l := by
  induction l with
  | nil => cases h
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [AList.lookup] at h
    split at h
    · rename_i heq; subst heq; cases Option.some.inj h; exact List.mem_cons_self
    · exact List.mem_cons_of_mem _ (ih h)

private theorem alist_lookup_of_mem_nodup {κ α : Type} [DecidableEq κ] {l : AList κ α}
    (hnd : (List.map Prod.fst l).Nodup) {k : κ} {v : α} (hm : (k, v) ∈ l) :
    AList.lookup l k = some v := by
  induction l with
  | nil => cases hm
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    rw [List.map_cons, List.nodup_cons] at hnd
    simp only [AList.lookup]
    cases hm with
    | head => simp
    | tail _ htl =>
      have hne : k' ≠ k := fun heq => hnd.1 (heq ▸ List.mem_map.mpr ⟨(k, v), htl, rfl⟩)
      simp [hne, ih hnd.2 htl]

private theorem alist_lookup_append_none_left {κ α : Type} [DecidableEq κ]
    {l₁ l₂ : AList κ α} {k : κ} (h : AList.lookup l₁ k = none) :
    AList.lookup (l₁ ++ l₂) k = AList.lookup l₂ k := by
  induction l₁ with
  | nil => rfl
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [AList.lookup] at h
    split at h
    · cases h
    · rename_i hne
      simp only [List.cons_append, AList.lookup, hne, ↓reduceIte]
      exact ih h

private theorem birMemoryView_graphAt {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {fn : FnId}
    {locals : LocalTypes language.operands.Ty} {graph : BIRGraph lang}
    (hLk : AList.lookup bir.functions fn = some (locals, graph)) :
    (birMemoryView (language := language) bir).graphAt fn =
      some (birMemoryGraph (language := language) bir (birBase locals graph) graph) := by
  unfold Memory.graphAt birMemoryView
  dsimp only
  rw [List.map_map]
  let g : LocalTypes language.operands.Ty × BIRGraph lang →
      LocalTypes language.operands.Ty × MemoryGraph lang := fun pair =>
    let base := birBase pair.1 pair.2
    let registers := pair.2.blocks.toList.flatMap fun block =>
      (block.params.map fun q => ((birReg base q.2).logical.root, q.1)) ++
      (block.body.flatMap fun op => birRegDecls base op.instruction)
    (pair.1 ++ registers, birMemoryGraph (language := language) bir base pair.2)
  have hMap : (List.map (Prod.fst ∘ fun (p : FnId × LocalTypes language.operands.Ty × BIRGraph lang) =>
      let base := birBase p.2.1 p.2.2
      let registers := p.2.2.blocks.toList.flatMap fun block =>
        (block.params.map fun q => ((birReg base q.2).logical.root, q.1)) ++
        (block.body.flatMap fun op => birRegDecls base op.instruction)
      ((p.1, p.2.1 ++ registers, birMemoryGraph (language := language) bir base p.2.2),
        (p.1, List.map (fun q => (q.1, StorageClass.materialized)) p.2.1 ++
          List.map (fun q => (q.1, StorageClass.virtual)) registers),
        (p.1, List.map Prod.fst registers))) bir.functions) =
      List.map (fun (a, b) => (a, g b)) bir.functions := rfl
  rw [hMap, alist_lookup_map_key g bir.functions fn, hLk]
  rfl

private theorem foldl_max_ge {α : Type} (f : α → Nat) :
    ∀ (xs : List α) (init : Nat), init ≤ xs.foldl (fun acc x => max acc (f x)) init
  | [], _ => Nat.le_refl _
  | a :: rest, init =>
    Nat.le_trans (Nat.le_max_left init (f a)) (foldl_max_ge f rest (max init (f a)))

private theorem foldl_max_mem {α : Type} (f : α → Nat) :
    ∀ (xs : List α) (init : Nat) {x : α}, x ∈ xs →
      f x ≤ xs.foldl (fun acc y => max acc (f y)) init
  | [], _, _, hx => nomatch hx
  | a :: rest, init, _, hx => by
    cases hx with
    | head =>
      exact Nat.le_trans (Nat.le_max_right init (f a)) (foldl_max_ge f rest (max init (f a)))
    | tail _ htl =>
      exact foldl_max_mem f rest (max init (f a)) htl

private theorem birBase_gt_locals {Ty : Type} {lang : LoweringLanguage Ty}
    {locals : LocalTypes Ty} {graph : BIRGraph lang} {root : PlaceId} {ty : Ty}
    (hMem : (root, ty) ∈ locals) :
    root.id < birBase locals graph := by
  unfold birBase
  dsimp only
  have h1 : root.id + 1 ≤ List.foldl (fun limit (p : PlaceId × Ty) => max limit (p.1.id + 1)) 0 locals :=
    foldl_max_mem (fun p : PlaceId × Ty => p.1.id + 1) locals 0 hMem
  have hMono : ∀ (blks : List (BIRBlock lang)) (acc : Nat),
      acc ≤ blks.foldl (fun limit block =>
        block.body.foldl (fun limit op =>
          match op.instruction with
          | .beginLifetime r | .endLifetime r => max limit (r.id + 1)
          | _ => limit) limit) acc := by
    intro blks
    induction blks with
    | nil => intro acc; exact Nat.le_refl _
    | cons b rest ih =>
      intro acc
      have hStep : ∀ (ops : List (LocatedOp (SSAInst lang))) (n : Nat),
          n ≤ ops.foldl (fun limit op =>
            match op.instruction with
            | .beginLifetime r | .endLifetime r => max limit (r.id + 1)
            | _ => limit) n := by
        intro ops
        induction ops with
        | nil => intro n; exact Nat.le_refl _
        | cons o rs iho =>
          intro n
          dsimp only [List.foldl]
          cases o.instruction <;> try exact iho n
          · exact Nat.le_trans (Nat.le_max_left n _) (iho _)
          · exact Nat.le_trans (Nat.le_max_left n _) (iho _)
      exact Nat.le_trans (hStep b.body acc) (ih _)
  rw [← Array.foldl_toList]
  have h2 := hMono graph.blocks.toList (List.foldl (fun limit (p : PlaceId × Ty) => max limit (p.1.id + 1)) 0 locals)
  exact Nat.lt_of_lt_of_le (Nat.lt_of_succ_le h1) h2

/-- Every synthetic BIR register root `⟨birBase locals graph + r⟩` is never classified as
`.materialized` in `birMemoryView bir`. -/
private theorem birMemoryView_classAt_reg_ne_mat {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {fn : FnId}
    {locals : LocalTypes language.operands.Ty} {graph : BIRGraph lang}
    (hLk : AList.lookup bir.functions fn = some (locals, graph)) (r : Nat) :
    (birMemoryView (language := language) bir).classAt fn ⟨birBase locals graph + r⟩ ≠
      some .materialized := by
  unfold Memory.classAt birMemoryView
  dsimp only
  rw [List.map_map]
  let g : LocalTypes language.operands.Ty × BIRGraph lang → AList PlaceId StorageClass := fun pair =>
    let base := birBase pair.1 pair.2
    let registers := pair.2.blocks.toList.flatMap fun block =>
      (block.params.map fun q => ((birReg base q.2).logical.root, q.1)) ++
      (block.body.flatMap fun op => birRegDecls base op.instruction)
    List.map (fun q => (q.1, StorageClass.materialized)) pair.1 ++
      List.map (fun q => (q.1, StorageClass.virtual)) registers
  have hMap : (List.map ((fun x => x.2.1) ∘ fun (p : FnId × LocalTypes language.operands.Ty × BIRGraph lang) =>
      let base := birBase p.2.1 p.2.2
      let registers := p.2.2.blocks.toList.flatMap fun block =>
        (block.params.map fun q => ((birReg base q.2).logical.root, q.1)) ++
        (block.body.flatMap fun op => birRegDecls base op.instruction)
      ((p.1, p.2.1 ++ registers, birMemoryGraph (language := language) bir base p.2.2),
        (p.1, List.map (fun q => (q.1, StorageClass.materialized)) p.2.1 ++
          List.map (fun q => (q.1, StorageClass.virtual)) registers),
        (p.1, List.map Prod.fst registers))) bir.functions) =
      List.map (fun (a, b) => (a, g b)) bir.functions := rfl
  rw [hMap, alist_lookup_map_key g bir.functions fn, hLk]
  dsimp only [Option.map, bind, Option.bind, g]
  have hNoneLoc : AList.lookup (List.map (fun q => (q.1, StorageClass.materialized)) locals)
      ⟨birBase locals graph + r⟩ = none := by
    cases hL : AList.lookup (List.map (fun q => (q.1, StorageClass.materialized)) locals)
        ⟨birBase locals graph + r⟩ with
    | none => rfl
    | some sc =>
      have hMem := alist_lookup_mem hL
      obtain ⟨⟨rt, ty⟩, hIn, hEq⟩ := List.mem_map.mp hMem
      have hRt : rt = ⟨birBase locals graph + r⟩ := (Prod.mk.inj hEq).1
      have hLt := birBase_gt_locals (graph := graph) hIn
      rw [hRt] at hLt
      dsimp only at hLt
      omega
  rw [alist_lookup_append_none_left hNoneLoc]
  intro hMat
  have hMem := alist_lookup_mem hMat
  obtain ⟨_, _, hEq⟩ := List.mem_map.mp hMem
  cases (Prod.mk.inj hEq).2

private theorem bir_freshLayouts_virtual {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {fn : FnId}
    {locals : LocalTypes language.operands.Ty} {graph : BIRGraph lang}
    (hLk : AList.lookup bir.functions fn = some (locals, graph))
    (roots : List PlaceId) (hAll : ∀ root ∈ roots, ∃ r, root = ⟨birBase locals graph + r⟩) :
    ∀ live, (birMemoryView (language := language) bir).freshLayoutsFrom fn live roots = [] := by
  induction roots with
  | nil => intro _; rfl
  | cons rt rest ih =>
    intro live
    obtain ⟨r, rfl⟩ := hAll rt List.mem_cons_self
    have hRest := ih (fun x hx => hAll x (List.mem_cons_of_mem _ hx))
    unfold Memory.freshLayoutsFrom
    split
    · exact hRest live
    · rw [hRest (⟨birBase locals graph + r⟩ :: live)]
      have hNe := birMemoryView_classAt_reg_ne_mat (language := language) hLk r
      simp [hNe]

private theorem bir_paramSpaceFits_regs {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile}
    {locals : LocalTypes language.operands.Ty} {graph : BIRGraph lang}
    {state : MemoryRegisters language profile}
    (hFrame : state.frame = bir.entry)
    (hLk : AList.lookup bir.functions bir.entry = some (locals, graph))
    (params : List (Sigma (MemoryPlace language.operands.Ty)))
    (hAll : ∀ p ∈ params, ∃ r, p.2.logical.root = ⟨birBase locals graph + r⟩) :
    (birMemoryView (language := language) bir).ParamSpaceFits params state := by
  unfold Memory.ParamSpaceFits Memory.ReservationsFit Memory.freshLayouts
  rw [hFrame, bir_freshLayouts_virtual (language := language) hLk
    (params.map fun p => p.2.logical.root) (by
      intro rt hrt
      obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hrt
      exact hAll p hp)]
  trivial

/-! ## 3. Entry-only reachability theorem (justifying `unreachableHelperBody`) -/

private theorem mem_zipIdx_of_mem {α : Type} {xs : List α} {x : α} (h : x ∈ xs) :
    ∃ idx, (x, idx) ∈ xs.zipIdx := by
  have go : ∀ k (ys : List α), x ∈ ys → ∃ idx, (x, idx) ∈ ys.zipIdx k := by
    intro k ys hm
    induction ys generalizing k with
    | nil => cases hm
    | cons a rest ih =>
      cases hm with
      | head => exact ⟨k, List.mem_cons_self⟩
      | tail _ htl =>
        obtain ⟨idx, hidx⟩ := ih (k + 1) htl
        exact ⟨idx, List.mem_cons_of_mem _ hidx⟩
  exact go 0 xs h

private theorem mem_reachableFunctions_entry {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {bir : BIR lang profile}
    (hEntry : bir.entry ∈ List.map Prod.fst bir.functions) :
    bir.entry ∈ bir.reachableFunctions := by
  unfold BIR.reachableFunctions
  rw [mem_reachGraph_iff]
  exact .refl hEntry (by intro h; cases h)

private def birInstFeats {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) (fn : FnId) (entryReachable : Bool)
    (pos : FeaturePosition) (inst : SSAInst lang) : List (FeatureCause profile) :=
  let wCand : language.operands.Ty → List (FeatureCause profile) := fun ty =>
    match language.scalarType ty with
    | some (.int intTy) => [.unsupportedWidth intTy fn pos]
    | _ => []
  match inst with
  | @SSAInst.op _ _ opcode types ty _ _ _ addresses =>
    (if addresses.isEmpty then [.unsupportedStorage .registerOnly fn pos]
     else [.unsupportedStorage .addressOperand fn pos]) ++
    [.unsupportedOperation (Operator.policyOperation opcode) fn pos] ++
    (wCand ty ++ types.flatMap wCand ++ addresses.flatMap (fun p => wCand p.1))
  | .addWithOverflow t _ _ _ _ =>
    [.unsupportedOperation .addWithOverflow fn pos, .unsupportedWidth t fn pos]
  | .load (ty := ty) _ _ => .unsupportedStorage .load fn pos :: wCand ty
  | .store (ty := ty) _ _ => .unsupportedStorage .store fn pos :: wCand ty
  | .beginLifetime _ => [.unsupportedStorage .beginLifetime fn pos]
  | .endLifetime _ => [.unsupportedStorage .endLifetime fn pos]
  | .destructure (ty := ty) src bindings =>
    let form := match src with
      | .register _ => BackendPolicy.StorageForm.destructureRegister
      | .materialized _ => .destructureMaterialized
    .unsupportedStorage form fn pos ::
      (wCand ty ++ bindings.flatMap (fun (_, ⟨fty, _⟩) => wCand fty))
  | .call callee dst args =>
    let form :=
      if !entryReachable then BackendPolicy.CallForm.unreachableCall
      else if (AList.lookup bir.functions callee).isSome then .reachableInternalCall
      else .foreignCall
    .unsupportedCall form (some callee) fn pos ::
      (dst.toList.flatMap (fun d => wCand d.1) ++ args.flatMap (fun a => wCand a.1))

/-- Full classification of every instruction in a reachable block of `bir.entry` when
`firstUnsupported = none` on a well-formed LCB `BIR`:
only `.read`, `.arithmetic` (plain/wrapping add/sub/mul on `inWordProfile`), and
`.addWithOverflow` (on `inWordProfile`) can appear; `.call`, `.load`, `.store`,
`.beginLifetime`, `.endLifetime`, and `.destructure` are impossible. -/
private theorem lcb_reachable_inst_classify {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    (hBackend : profile.backend = .lcb)
    (hWF : BIR.WF (language := sliceLanguage slice) bir)
    (hSup : firstUnsupported (language := sliceLanguage slice) bir = none)
    {locals : LocalTypes (sliceLanguage slice).operands.Ty}
    {graph : BIRGraph (sliceLoweringLanguage slice)}
    (hEntry : AList.lookup bir.functions bir.entry = some (locals, graph))
    {blk : BIRBlock (sliceLoweringLanguage slice)}
    (hBlk : blk ∈ graph.blocks.toList)
    (hReach : blk.id ∈ BIR.reachableBlocks graph)
    {op : LocatedOp (SSAInst (sliceLoweringLanguage slice))}
    (hOp : op ∈ blk.body) :
    (∃ (ty : (sliceLanguage slice).operands.Ty) (dst : SSAValue (sliceLanguage slice).operands.Ty ty)
        (fop : (sliceLoweringLanguage slice).operation .read [ty] ty)
        (arg : SSARead (sliceLanguage slice).operands.Ty (sliceLoweringLanguage slice).literal ty),
      op.instruction = .op dst fop (.cons arg .nil) []) ∨
    (∃ (mOp : MachineOp) (wOp : WrapOp) (t : IntTy)
        (dst : SSAValue (sliceLanguage slice).operands.Ty ((sliceLoweringLanguage slice).intTy t))
        (fop : (sliceLoweringLanguage slice).operation (.arithmetic mOp)
          [(sliceLoweringLanguage slice).intTy t, (sliceLoweringLanguage slice).intTy t]
          ((sliceLoweringLanguage slice).intTy t))
        (lhs rhs : SSARead (sliceLanguage slice).operands.Ty (sliceLoweringLanguage slice).literal
          ((sliceLoweringLanguage slice).intTy t)),
      wrapOpOfMachineOp mOp = some wOp ∧ inWordProfile t = true ∧
      op.instruction = .op dst fop (.cons lhs (.cons rhs .nil)) []) ∨
    (∃ (t : IntTy)
        (dst : SSAValue (sliceLanguage slice).operands.Ty ((sliceLoweringLanguage slice).intTy t))
        (flag : SSAValue (sliceLanguage slice).operands.Ty (sliceLoweringLanguage slice).bool)
        (lhs rhs : SSARead (sliceLanguage slice).operands.Ty (sliceLoweringLanguage slice).literal
          ((sliceLoweringLanguage slice).intTy t)),
      inWordProfile t = true ∧ dst.id ≠ flag.id ∧
      op.instruction = .addWithOverflow t dst flag lhs rhs) := by
  let sLang := sliceLanguage slice
  let sLow := sliceLoweringLanguage slice
  letI : DecidableEq sLang.operands.Ty := sLang.typeDecidableEq
  rcases op with ⟨orig, inst⟩
  dsimp only
  have hFnMem : (bir.entry, locals, graph) ∈ bir.functions := alist_lookup_mem hEntry
  have hEntryReach : bir.entry ∈ bir.reachableFunctions :=
    mem_reachableFunctions_entry hWF.registerFormation.entry_mem
  have hAllSup := (firstUnsupported_none_iff (language := sLang) bir).mp hSup
  have hER : (bir.reachableFunctions.contains bir.entry && (BIR.reachableBlocks graph).contains blk.id) = true := by
    simp only [Bool.and_eq_true, List.contains_eq_mem, decide_eq_true_eq]
    exact ⟨hEntryReach, hReach⟩
  have hPolicyLcb : profile.policyProfile.backend = .lcb := by
    unfold Profile.policyProfile
    cases hb : profile.backend
    · rw [hb] at hBackend; cases hBackend
    · rfl
  obtain ⟨idx, hIdx⟩ := mem_zipIdx_of_mem hOp
  let normGraphCfg : NormalizedCFG sLow :=
    ⟨graph.entry, graph.blocks.toList.map fun b =>
      ⟨b.id, b.params, b.body, RegTerminator.ofBIR bir.sites.size b.ending, b.boundaryOrigins⟩⟩
  have hNormMem : (bir.entry, locals, normGraphCfg) ∈ (RegisterIR.bir bir).normalize.functions := by
    unfold RegisterIR.normalize
    exact List.mem_map.mpr ⟨(bir.entry, locals, graph), hFnMem, rfl⟩
  have hFnFormed := hWF.registerFormation.2.2.2.2.2.2.2.2.2.2 _ hNormMem
  obtain ⟨_, _, _, _, _, hBlksFormed⟩ := hFnFormed.2.2.2
  have hNormBlk : (⟨blk.id, blk.params, blk.body, RegTerminator.ofBIR bir.sites.size blk.ending,
      blk.boundaryOrigins⟩ : Block _ _ (RegTerminator sLow)) ∈ normGraphCfg.blocks :=
    List.mem_map.mpr ⟨blk, hBlk, rfl⟩
  have hLocal : instLocalOk sLang bir.declarations profile.memoryTarget bir.signatures locals inst = true :=
    ((hBlksFormed _ hNormBlk).2.2.2.2 (⟨orig, inst⟩, idx) hIdx).1
  have hBodyLocal := (Bool.and_eq_true_iff.mp hLocal).2
  have hInstIn : ∀ c : FeatureCause profile,
      c ∈ birInstFeats (language := sLang) bir bir.entry
        (bir.reachableFunctions.contains bir.entry && (BIR.reachableBlocks graph).contains blk.id)
        (.instruction blk.id idx) inst →
      BackendPolicy.FeatureSupported profile.policyProfile c.feature := by
    intro c hc
    apply hAllSup
    unfold BIR.featureCandidates
    refine List.mem_flatMap.mpr ⟨(bir.entry, locals, graph), hFnMem, ?_⟩
    apply List.mem_append_right
    refine List.mem_flatMap.mpr ⟨blk, hBlk, ?_⟩
    apply List.mem_append_left
    apply List.mem_append_right
    refine List.mem_flatMap.mpr ⟨(⟨orig, inst⟩, idx), hIdx, ?_⟩
    cases inst with
    | op _ _ _ _ => exact hc
    | addWithOverflow _ _ _ _ _ => exact hc
    | load _ _ => exact hc
    | store _ _ => exact hc
    | beginLifetime _ => exact hc
    | endLifetime _ => exact hc
    | destructure src _ => cases src <;> exact hc
    | call _ _ _ => exact hc
  have hWidthProf : ∀ {t : IntTy},
      BackendPolicy.FeatureSupported profile.policyProfile (.intWidth t) → inWordProfile t = true := by
    intro t ht
    unfold BackendPolicy.FeatureSupported BackendPolicy.IntWidthSupported
      BackendPolicy.supportsIntWidth at ht
    dsimp only at ht
    rw [hPolicyLcb] at ht
    exact ht
  cases hInst : inst with
  | op dst fop args addresses =>
    rename_i opcode types ty
    rw [hInst] at hBodyLocal hInstIn
    have hSig := (Bool.and_eq_true_iff.mp hBodyLocal).1
    have hOpSup := hInstIn (.unsupportedOperation (Operator.policyOperation opcode) bir.entry (.instruction blk.id idx))
      (by simp [birInstFeats])
    have hAddrEmpty : addresses = [] := by
      cases addresses with
      | nil => rfl
      | cons a rs =>
        have hBad := hInstIn (.unsupportedStorage .addressOperand bir.entry (.instruction blk.id idx))
          (by simp [birInstFeats])
        cases hBad
    subst hAddrEmpty
    cases opcode with
    | read =>
      simp only [opcodeSignatureOk, List.isEmpty_nil, Bool.true_and, Bool.and_eq_true] at hSig
      cases types with
      | nil => simp at hSig
      | cons t0 rest =>
        cases rest with
        | cons _ _ => simp at hSig
        | nil =>
          have hTeq : t0 = ty := of_decide_eq_true hSig.2
          subst hTeq
          cases args with
          | cons arg restArgs =>
            cases restArgs with
            | nil => exact Or.inl ⟨t0, dst, fop, arg, rfl⟩
    | arithmetic mOp =>
      have hWrap : ∃ wOp, wrapOpOfMachineOp mOp = some wOp ∧ mOp.countOperand = false := by
        cases mOp with
        | plain ao =>
          cases ao with
          | add => exact ⟨.add, rfl, rfl⟩
          | sub => exact ⟨.sub, rfl, rfl⟩
          | mul => exact ⟨.mul, rfl, rfl⟩
          | div | rem | shl | shr => cases hOpSup
        | wrapping wo =>
          cases wo with
          | add => exact ⟨.add, rfl, rfl⟩
          | sub => exact ⟨.sub, rfl, rfl⟩
          | mul => exact ⟨.mul, rfl, rfl⟩
        | saturating _ => cases hOpSup
      obtain ⟨wOp, hWOp, hCnt⟩ := hWrap
      simp only [opcodeSignatureOk, List.isEmpty_nil, Bool.true_and, hCnt, Bool.false_eq_true,
        ↓reduceIte, Bool.and_eq_true] at hSig
      cases types with
      | nil => simp at hSig
      | cons t0 rest =>
        cases rest with
        | nil => simp at hSig
        | cons t1 rest2 =>
          cases rest2 with
          | cons _ _ => simp at hSig
          | nil =>
            have hMatch := hSig.2
            dsimp only at hMatch
            obtain ⟨h12, hT1d⟩ := Bool.and_eq_true_iff.mp hMatch
            obtain ⟨hIsInt, hT0d⟩ := Bool.and_eq_true_iff.mp h12
            have hT0 : t0 = ty := of_decide_eq_true hT0d
            have hT1 : t1 = ty := of_decide_eq_true hT1d
            subst t0; subst t1
            cases hSc : sLang.scalarType ty with
            | none => simp [isIntType, hSc] at hIsInt
            | some ct =>
              cases ct with
              | bool | char | unit => simp [isIntType, hSc] at hIsInt
              | int t =>
                have hTyInt : ty = (sliceLoweringLanguage slice).intTy t := by
                  cases hTy : (ty : EarlyTy) with
                  | nominal _ => simp [sLang, sliceLanguage, hTy] at hSc
                  | scalar sc =>
                    cases sc with
                    | bool | char | unit => simp [sLang, sliceLanguage, hTy] at hSc
                    | int t' =>
                      simp [sLang, sliceLanguage, hTy] at hSc
                      subst hSc; rfl
                subst hTyInt
                have htProf := hWidthProf (hInstIn (.unsupportedWidth t bir.entry (.instruction blk.id idx)) (by
                  simp [birInstFeats, sLang, sliceLanguage, sliceLoweringLanguage]))
                cases args with
                | cons lhs r1 =>
                  cases r1 with
                  | cons rhs r2 =>
                    cases r2 with
                    | nil =>
                      exact Or.inr (Or.inl ⟨mOp, wOp, t, dst, fop, lhs, rhs, hWOp, htProf, rfl⟩)
    | named _ => cases hOpSup
  | addWithOverflow t dst flag lhs rhs =>
    rw [hInst] at hInstIn
    have htProf := hWidthProf (hInstIn (.unsupportedWidth t bir.entry (.instruction blk.id idx))
      (List.mem_cons_of_mem _ List.mem_cons_self))
    have hNe : dst.id ≠ flag.id := by
      intro hEq
      have hOpMem : (⟨orig, SSAInst.addWithOverflow t dst flag lhs rhs⟩ : LocatedOp (SSAInst sLow)) ∈ blk.body :=
        hInst ▸ hOp
      exact BIR.memoryView_addWithOverflow_aliased_not_tablesWF
        (language := sLang) hFnMem hBlk hOpMem hEq hWF.2.2.2.2.2.1
    exact Or.inr (Or.inr ⟨t, dst, flag, lhs, rhs, htProf, hNe, rfl⟩)
  | load _ _ =>
    rw [hInst] at hInstIn
    cases hInstIn (.unsupportedStorage .load bir.entry (.instruction blk.id idx)) List.mem_cons_self
  | store _ _ =>
    rw [hInst] at hInstIn
    cases hInstIn (.unsupportedStorage .store bir.entry (.instruction blk.id idx)) List.mem_cons_self
  | beginLifetime _ =>
    rw [hInst] at hInstIn
    cases hInstIn (.unsupportedStorage .beginLifetime bir.entry (.instruction blk.id idx)) List.mem_cons_self
  | endLifetime _ =>
    rw [hInst] at hInstIn
    cases hInstIn (.unsupportedStorage .endLifetime bir.entry (.instruction blk.id idx)) List.mem_cons_self
  | destructure src _ =>
    rw [hInst] at hInstIn
    cases src with
    | register _ =>
      cases hInstIn (.unsupportedStorage .destructureRegister bir.entry (.instruction blk.id idx)) List.mem_cons_self
    | materialized _ =>
      cases hInstIn (.unsupportedStorage .destructureMaterialized bir.entry (.instruction blk.id idx)) List.mem_cons_self
  | call callee _ _ =>
    rw [hInst] at hInstIn
    cases hCal : (AList.lookup bir.functions callee).isSome with
    | true =>
      have hBad := hInstIn (.unsupportedCall .reachableInternalCall (some callee) bir.entry (.instruction blk.id idx)) (by
        dsimp only [birInstFeats]; rw [hER, hCal]; exact List.mem_cons_self)
      cases hBad
    | false =>
      have hBad := hInstIn (.unsupportedCall .foreignCall (some callee) bir.entry (.instruction blk.id idx)) (by
        dsimp only [birInstFeats]; rw [hER, hCal]; exact List.mem_cons_self)
      cases hBad

/-- Explicit no-entry-reachable-call theorem for LCB: on a well-formed LCB `BIR` with
`firstUnsupported = none`, no block reachable from `bir.entry`'s entry block contains a
`.call` (or storage/lifetime/destructure) instruction. This guarantees that non-entry helper
functions in `bir.functions` are unreachable from `bir.entry`, justifying
`EmitLCB.unreachableHelperBody` in `buildAdditionalJobs`. -/
theorem entry_no_reachable_calls {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    (hBackend : profile.backend = .lcb)
    (hWF : BIR.WF (language := sliceLanguage slice) bir)
    (hSup : firstUnsupported (language := sliceLanguage slice) bir = none)
    {locals : LocalTypes (sliceLanguage slice).operands.Ty}
    {graph : BIRGraph (sliceLoweringLanguage slice)}
    (hEntry : AList.lookup bir.functions bir.entry = some (locals, graph))
    {blk : BIRBlock (sliceLoweringLanguage slice)}
    (hBlk : blk ∈ graph.blocks.toList)
    (hReach : blk.id ∈ BIR.reachableBlocks graph)
    {op : LocatedOp (SSAInst (sliceLoweringLanguage slice))}
    (hOp : op ∈ blk.body) :
    match op.instruction with
    | .call .. | .load .. | .store .. | .beginLifetime _ | .endLifetime _ | .destructure .. => False
    | _ => True := by
  rcases lcb_reachable_inst_classify hBackend hWF hSup hEntry hBlk hReach hOp with
    ⟨_, _, _, _, hEq⟩ | ⟨_, _, _, _, _, _, _, _, _, hEq⟩ | ⟨_, _, _, _, _, _, _, hEq⟩ <;>
    rw [hEq] <;> trivial

/-! ## 4. Value numbering interval disjointness and `emitBlock` alignment -/

private theorem mem_indexed_getElem? {α : Type} {l : List α} {k : Nat} {x : α}
    (h : (k, x) ∈ indexed l) : k < l.length ∧ l[k]? = some x := by
  have go : ∀ n (ys : List α), (k, x) ∈ Backend.Runtime.indexedFrom n ys →
      n ≤ k ∧ k < n + ys.length ∧ ys[k - n]? = some x := by
    intro n ys hm
    induction ys generalizing n with
    | nil => cases hm
    | cons a rest ih =>
      simp only [Backend.Runtime.indexedFrom, List.mem_cons, Prod.mk.injEq] at hm
      rcases hm with ⟨rfl, rfl⟩ | htl
      · exact ⟨Nat.le_refl _, by simp, by simp⟩
      · obtain ⟨hle, hlt, hgt⟩ := ih (n + 1) htl
        have hPos : k - n = (k - (n + 1)) + 1 := by omega
        refine ⟨by omega, by simp; omega, ?_⟩
        rw [hPos, List.getElem?_cons_succ]
        exact hgt
  obtain ⟨_, hlt, hgt⟩ := go 0 l h
  exact ⟨by simpa using hlt, by simpa using hgt⟩

private theorem indexed_length {α : Type} (l : List α) : (indexed l).length = l.length := by
  have go : ∀ n (ys : List α), (Backend.Runtime.indexedFrom n ys).length = ys.length := by
    intro n ys
    induction ys generalizing n with
    | nil => rfl
    | cons _ rest ih => simp [Backend.Runtime.indexedFrom, ih]
  exact go 0 l

private theorem getElem?_indexed_mem {α : Type} {l : List α} {k : Nat} {x : α}
    (h : l[k]? = some x) : (k, x) ∈ indexed l := by
  have hGet : (indexed l)[k]? = some (k, x) := by
    rw [Backend.Runtime.getElem?_indexed, h]
    rfl
  exact List.mem_of_getElem? hGet

private theorem list_sum_take_add {α : Type} (f : α → Nat) {l : List α} {i : Nat} {x : α}
    (hi : l[i]? = some x) :
    ((l.take (i + 1)).map f).sum = ((l.take i).map f).sum + f x := by
  rw [List.take_add_one, hi]
  simp [List.sum_append]

private theorem list_sum_take_mono {α : Type} (f : α → Nat) :
    ∀ (l : List α) {i j : Nat}, i ≤ j →
      ((l.take i).map f).sum ≤ ((l.take j).map f).sum
  | [], _, _, _ => by simp
  | a :: rest, 0, j, _ => by simp
  | a :: rest, i + 1, 0, h => by omega
  | a :: rest, i + 1, j + 1, h => by
    simp only [List.take_succ_cons, List.map_cons, List.sum_cons]
    have ih := list_sum_take_mono f rest (Nat.le_of_succ_le_succ h)
    omega

private theorem list_sum_take_le_all {α : Type} (f : α → Nat) (l : List α) (i : Nat) :
    ((l.take i).map f).sum ≤ (l.map f).sum := by
  have h := list_sum_take_mono f l (i := i) (j := l.length)
  rcases Nat.le_total i l.length with hle | hge
  · simpa [List.take_length] using h hle
  · rw [List.take_of_length_le hge]; exact Nat.le_refl _

private theorem blockBase_add_size_le_next {Ty : Type} {lang : LoweringLanguage Ty}
    (norm : NormGraph lang) {j₁ j₂ : Nat} {b₁ : NormBlock lang}
    (h1 : norm.blocks[j₁]? = some b₁) (hLt : j₁ < j₂) :
    blockBase norm j₁ + blockSize j₁ b₁ ≤ blockBase norm j₂ := by
  unfold blockBase
  have hIdx1 : (indexed norm.blocks)[j₁]? = some (j₁, b₁) := by
    rw [Backend.Runtime.getElem?_indexed, h1]; rfl
  have hStep := list_sum_take_add (fun (idx, blk) => blockSize idx blk) hIdx1
  have hMono := list_sum_take_mono (fun (idx, blk) => blockSize idx blk)
    (indexed norm.blocks) (show j₁ + 1 ≤ j₂ by omega)
  have hSum : (((indexed norm.blocks).take j₁).map fun (idx, blk) => blockSize idx blk).sum + blockSize j₁ b₁ ≤
      (((indexed norm.blocks).take j₂).map fun (idx, blk) => blockSize idx blk).sum := by
    rw [← hStep]; exact hMono
  exact Nat.add_assoc norm.paramCount _ _ ▸ Nat.add_le_add_left hSum norm.paramCount

private theorem blockBase_add_size_le_valueCount {Ty : Type} {lang : LoweringLanguage Ty}
    (norm : NormGraph lang) {j : Nat} {b : NormBlock lang}
    (hj : norm.blocks[j]? = some b) :
    blockBase norm j + blockSize j b ≤ valueCount norm := by
  unfold blockBase valueCount
  have hIdx : (indexed norm.blocks)[j]? = some (j, b) := by
    rw [Backend.Runtime.getElem?_indexed, hj]; rfl
  have hStep := list_sum_take_add (fun (idx, blk) => blockSize idx blk) hIdx
  have hAll := list_sum_take_le_all (fun (idx, blk) => blockSize idx blk)
    (indexed norm.blocks) (j + 1)
  have hSum : (((indexed norm.blocks).take j).map fun (idx, blk) => blockSize idx blk).sum + blockSize j b ≤
      ((indexed norm.blocks).map fun (idx, blk) => blockSize idx blk).sum := by
    rw [← hStep]; exact hAll
  exact Nat.add_assoc norm.paramCount _ _ ▸ Nat.add_le_add_left hSum norm.paramCount

private theorem instBase_add_width_le_next {Ty : Type} {lang : LoweringLanguage Ty}
    (j : Nat) (blk : NormBlock lang) {i₁ i₂ : Nat} {op₁ : LocatedOp (SSAInst lang)}
    (h1 : blk.body[i₁]? = some op₁) (hLt : i₁ < i₂) :
    instBase j blk i₁ + instWidth op₁.instruction ≤ instBase j blk i₂ := by
  unfold instBase
  have hStep := list_sum_take_add (fun op : LocatedOp (SSAInst lang) => instWidth op.instruction) h1
  have hMono := list_sum_take_mono (fun op : LocatedOp (SSAInst lang) => instWidth op.instruction)
    blk.body (show i₁ + 1 ≤ i₂ by omega)
  omega

private theorem instBase_add_width_le_blockSize {Ty : Type} {lang : LoweringLanguage Ty}
    (j : Nat) (blk : NormBlock lang) {i : Nat} {op : LocatedOp (SSAInst lang)}
    (hi : blk.body[i]? = some op) :
    instBase j blk i + instWidth op.instruction ≤ blockSize j blk := by
  unfold instBase blockSize
  have hStep := list_sum_take_add (fun op : LocatedOp (SSAInst lang) => instWidth op.instruction) hi
  have hAll := list_sum_take_le_all (fun op : LocatedOp (SSAInst lang) => instWidth op.instruction)
    blk.body (i + 1)
  omega

private theorem instDefs_off_lt_width {Ty : Type} {lang : LoweringLanguage Ty}
    {inst : SSAInst lang} {r off : Nat} (h : (r, off) ∈ instDefs inst) :
    off < instWidth inst := by
  cases inst with
  | @op opcode _ _ _ _ _ _ =>
    cases opcode with
    | read =>
      cases List.mem_singleton.mp h
      simp [instWidth]
    | arithmetic _ =>
      cases List.mem_singleton.mp h
      simp [instWidth]
    | named _ => cases h
  | addWithOverflow _ _ _ _ _ =>
    simp only [instDefs, List.mem_cons, List.not_mem_nil, or_false, Prod.mk.injEq] at h
    rcases h with ⟨_, rfl⟩ | ⟨_, rfl⟩ <;> simp [instWidth]
  | load _ _ | store _ _ | beginLifetime _ | endLifetime _ | destructure _ _ | call _ _ _ =>
    cases h

private theorem instDefs_unique_at_off {Ty : Type} {lang : LoweringLanguage Ty}
    {inst : SSAInst lang} {r₁ r₂ off : Nat}
    (h₁ : (r₁, off) ∈ instDefs inst) (h₂ : (r₂, off) ∈ instDefs inst) :
    r₁ = r₂ := by
  cases inst with
  | @op opcode _ _ _ _ _ _ =>
    cases opcode with
    | read =>
      have e1 := Prod.mk.inj (List.mem_singleton.mp h₁)
      have e2 := Prod.mk.inj (List.mem_singleton.mp h₂)
      exact e1.1.trans e2.1.symm
    | arithmetic _ =>
      have e1 := Prod.mk.inj (List.mem_singleton.mp h₁)
      have e2 := Prod.mk.inj (List.mem_singleton.mp h₂)
      exact e1.1.trans e2.1.symm
    | named _ => cases h₁
  | addWithOverflow _ _ _ _ _ =>
    simp only [instDefs, List.mem_cons, List.not_mem_nil, or_false, Prod.mk.injEq] at h₁ h₂
    rcases h₁ with ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ <;> rcases h₂ with ⟨rfl, h⟩ | ⟨rfl, h⟩ <;>
      first | rfl | cases h
  | load _ _ | store _ _ | beginLifetime _ | endLifetime _ | destructure _ _ | call _ _ _ =>
    cases h₁

/-- Canonical ownership of every LCB `Source.ValueId` slot in a 0-parameter `NormGraph`:
every `(r, slot) ∈ regIds norm` lies strictly inside the half-open interval of either a block
parameter (`j > 0`, `k < blk.params.length`) or an instruction (`i < blk.body.length`,
`off < instWidth op`). Disjointness of intervals makes slot ownership unique without quadratic
`Nodup` proofs. -/
private theorem regIds_slot_decompose {Ty : Type} {lang : LoweringLanguage Ty}
    {norm : NormGraph lang}
    (hZeroParams : ∀ b0, norm.blocks[0]? = some b0 → b0.params = [])
    {r slot : Nat} (hMem : (r, slot) ∈ regIds norm) :
    (∃ j nb k p, norm.blocks[j]? = some nb ∧ j ≠ 0 ∧ nb.params[k]? = some p ∧
      p.2.id = r ∧ slot = blockBase norm j + k ∧ k < blockPhiCount j nb) ∨
    (∃ j nb i op off, norm.blocks[j]? = some nb ∧ nb.body[i]? = some op ∧
      (r, off) ∈ instDefs op.instruction ∧ off < instWidth op.instruction ∧
      slot = blockBase norm j + instBase j nb i + off) := by
  unfold regIds at hMem
  obtain ⟨⟨j, nb⟩, hNbIdx, hInBlk⟩ := List.mem_flatMap.mp hMem
  obtain ⟨_, hNbGet⟩ := mem_indexed_getElem? hNbIdx
  rcases List.mem_append.mp hInBlk with hParam | hBody
  · obtain ⟨⟨k, p⟩, hpIdx, hpEq⟩ := List.mem_map.mp hParam
    obtain ⟨hkLt, hpGet⟩ := mem_indexed_getElem? hpIdx
    have hIdEq : p.2.id = r := (Prod.mk.inj hpEq).1
    have hSlotEq : (if j == 0 then k else blockBase norm j + k) = slot := (Prod.mk.inj hpEq).2
    have hjNe : j ≠ 0 := by
      intro hj0
      subst hj0
      rw [hZeroParams nb hNbGet] at hkLt
      cases hkLt
    have hjBeq : (j == 0) = false := beq_false_of_ne hjNe
    simp only [hjBeq, Bool.false_eq_true, ↓reduceIte] at hSlotEq
    have hPhiCnt : k < blockPhiCount j nb := by
      unfold blockPhiCount; simp [hjBeq, hkLt]
    exact Or.inl ⟨j, nb, k, p, hNbGet, hjNe, hpGet, hIdEq, hSlotEq.symm, hPhiCnt⟩
  · obtain ⟨⟨i, op⟩, hiIdx, hInInst⟩ := List.mem_flatMap.mp hBody
    obtain ⟨_, hiGet⟩ := mem_indexed_getElem? hiIdx
    obtain ⟨⟨r', off⟩, hDef, hEq⟩ := List.mem_map.mp hInInst
    have hrEq : r' = r := (Prod.mk.inj hEq).1
    have hSlotEq : blockBase norm j + instBase j nb i + off = slot := (Prod.mk.inj hEq).2
    subst hrEq
    exact Or.inr ⟨j, nb, i, op, off, hNbGet, hiGet, hDef, instDefs_off_lt_width hDef, hSlotEq.symm⟩

/-- Every slot in `regIds norm` is strictly less than `valueCount norm`. -/
private theorem regIds_slot_lt {Ty : Type} {lang : LoweringLanguage Ty}
    {norm : NormGraph lang}
    (hZeroParams : ∀ b0, norm.blocks[0]? = some b0 → b0.params = [])
    {r slot : Nat} (hMem : (r, slot) ∈ regIds norm) :
    slot < valueCount norm := by
  rcases regIds_slot_decompose hZeroParams hMem with
    ⟨j, nb, k, _, hNb, _, _, _, rfl, hk⟩ | ⟨j, nb, i, op, off, hNb, hi, _, hoff, rfl⟩
  · have h1 := blockBase_add_size_le_valueCount norm hNb
    unfold blockSize at h1
    omega
  · have h1 := instBase_add_width_le_blockSize j nb hi
    have h2 := blockBase_add_size_le_valueCount norm hNb
    omega

/-- Any `(r, slot) ∈ regIds norm` landing inside instruction `(j, i)`'s interval
`[blockBase norm j + instBase j nb i, blockBase norm j + instBase j nb i + instWidth op)`
must be defined by `op` at that exact offset! -/
private theorem regIds_at_inst_slot_iff {Ty : Type} {lang : LoweringLanguage Ty}
    {norm : NormGraph lang}
    (hZeroParams : ∀ b0, norm.blocks[0]? = some b0 → b0.params = [])
    {j i off r : Nat} {nb : NormBlock lang} {op : LocatedOp (SSAInst lang)}
    (hNb : norm.blocks[j]? = some nb) (hi : nb.body[i]? = some op)
    (hoff : off < instWidth op.instruction)
    (hMem : (r, blockBase norm j + instBase j nb i + off) ∈ regIds norm) :
    (r, off) ∈ instDefs op.instruction := by
  have hInBlock : blockBase norm j + instBase j nb i + off < blockBase norm j + blockSize j nb := by
    have := instBase_add_width_le_blockSize j nb hi; omega
  have hGePhi : blockBase norm j + blockPhiCount j nb ≤ blockBase norm j + instBase j nb i + off := by
    unfold instBase; omega
  rcases regIds_slot_decompose hZeroParams hMem with
    ⟨j', nb', k', _, hNb', _, _, _, hEq, hk'⟩ | ⟨j', nb', i', op', off', hNb', hi', hDef', hoff', hEq⟩
  · exfalso
    rcases Nat.lt_trichotomy j j' with hlt | rfl | hgt
    · have := blockBase_add_size_le_next norm hNb hlt; omega
    · rw [hNb] at hNb'; cases Option.some.inj hNb'; omega
    · have h1 := blockBase_add_size_le_next norm hNb' hgt
      unfold blockSize at h1; omega
  · have hjEq : j = j' := by
      rcases Nat.lt_trichotomy j j' with hlt | heq | hgt
      · have := blockBase_add_size_le_next norm hNb hlt; omega
      · exact heq
      · have h1 := blockBase_add_size_le_next norm hNb' hgt
        have h2 := instBase_add_width_le_blockSize j' nb' hi'
        omega
    subst hjEq
    rw [hNb] at hNb'; cases Option.some.inj hNb'
    have hiEq : i = i' := by
      rcases Nat.lt_trichotomy i i' with hlt | heq | hgt
      · have := instBase_add_width_le_next j nb hi hlt; omega
      · exact heq
      · have := instBase_add_width_le_next j nb hi' hgt; omega
    subst hiEq
    rw [hi] at hi'; cases Option.some.inj hi'
    have hoffEq : off = off' := by omega
    subst hoffEq
    exact hDef'

/-- Any `(r, slot) ∈ regIds norm` landing inside block `j`'s parameter interval
`[blockBase norm j, blockBase norm j + blockPhiCount j nb)` must be the exact `k`-th parameter
of block `j`! -/
private theorem regIds_at_param_slot_iff {Ty : Type} {lang : LoweringLanguage Ty}
    {norm : NormGraph lang}
    (hZeroParams : ∀ b0, norm.blocks[0]? = some b0 → b0.params = [])
    {j k r : Nat} {nb : NormBlock lang} {p : Sigma (SSAValue Ty)}
    (hNb : norm.blocks[j]? = some nb) (hjNe : j ≠ 0) (hp : nb.params[k]? = some p)
    (hMem : (r, blockBase norm j + k) ∈ regIds norm) :
    r = p.2.id := by
  have hkLt : k < blockPhiCount j nb := by
    have hlt : k < nb.params.length := by
      rcases Nat.lt_or_ge k nb.params.length with h | h
      · exact h
      · rw [List.getElem?_eq_none h] at hp; cases hp
    unfold blockPhiCount; simp [beq_false_of_ne hjNe, hlt]
  have hInBlock : blockBase norm j + k < blockBase norm j + blockSize j nb := by
    unfold blockSize; omega
  rcases regIds_slot_decompose hZeroParams hMem with
    ⟨j', nb', k', p', hNb', _, hp', hId', hEq, hk'⟩ | ⟨j', nb', i', op', off', hNb', hi', _, hoff', hEq⟩
  · have hjEq : j = j' := by
      rcases Nat.lt_trichotomy j j' with hlt | heq | hgt
      · have := blockBase_add_size_le_next norm hNb hlt; omega
      · exact heq
      · have h1 := blockBase_add_size_le_next norm hNb' hgt
        unfold blockSize at h1; omega
    subst hjEq
    rw [hNb] at hNb'; cases Option.some.inj hNb'
    have hkEq : k = k' := by omega
    subst hkEq
    rw [hp] at hp'; cases Option.some.inj hp'
    exact hId'.symm
  · exfalso
    have hjEq : j = j' := by
      rcases Nat.lt_trichotomy j j' with hlt | heq | hgt
      · have := blockBase_add_size_le_next norm hNb hlt; omega
      · exact heq
      · have h1 := blockBase_add_size_le_next norm hNb' hgt
        have h2 := instBase_add_width_le_blockSize j' nb' hi'
        omega
    subst hjEq
    rw [hNb] at hNb'; cases Option.some.inj hNb'
    unfold instBase at hEq
    omega

/-! ### `emitInst`, `emitBlock`, `adaptGraph`, and `Source.transfer` alignment -/

private theorem mapM_except_getElem? {α β ε : Type} {f : α → Except ε β}
    {xs : List α} {ys : List β} (h : xs.mapM f = .ok ys) :
    ys.length = xs.length ∧
    ∀ {i : Nat} {x : α}, xs[i]? = some x → ∃ y, ys[i]? = some y ∧ f x = .ok y := by
  induction xs generalizing ys with
  | nil =>
    cases h
    exact ⟨rfl, fun hi => by cases hi⟩
  | cons a rest ih =>
    simp only [List.mapM_cons] at h
    cases hfa : f a with
    | error e => simp [hfa, bind, Except.bind] at h
    | ok b =>
      simp only [hfa, bind, Except.bind] at h
      cases hRest : rest.mapM f with
      | error e => simp [hRest] at h
      | ok bs =>
        simp only [hRest, pure, Except.pure, Except.ok.injEq] at h
        subst h
        obtain ⟨hLen, hEl⟩ := ih hRest
        refine ⟨by simp [hLen], ?_⟩
        intro i x hi
        cases i with
        | zero =>
          cases Option.some.inj hi
          exact ⟨b, rfl, hfa⟩
        | succ i' =>
          exact hEl hi

private theorem emitInst_length {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty}
    {norm : NormGraph lang} {base : Nat} {inst : SSAInst lang} {xs : List Source.Instruction}
    (h : emitInst language norm base inst = .ok xs) :
    xs.length = instWidth inst := by
  unfold emitInst at h
  cases inst with
  | op dst fop args addresses =>
    rename_i opcode types ty
    dsimp only at h
    split at h
    · cases h
    · cases opcode with
      | read =>
        cases args with
        | nil => cases h
        | cons arg rest =>
          cases rest with
          | cons _ _ => cases h
          | nil =>
            dsimp only at h
            cases hr : emitRead language norm arg with
            | error _ => simp [hr, bind, Except.bind] at h
            | ok _ =>
              simp only [hr, bind, Except.bind, pure, Except.pure, Except.ok.injEq] at h
              subst h; rfl
      | arithmetic mOp =>
        dsimp only at h
        cases hw : wrapOpOfMachineOp mOp with
        | none => simp [hw] at h
        | some wOp =>
          simp only [hw] at h
          cases hsc : language.scalarType ty with
          | none => simp [hsc] at h
          | some ct =>
            cases ct with
            | bool | char | unit => simp [hsc] at h
            | int t =>
              simp only [hsc] at h
              split at h
              · cases h
              · cases args with
                | nil => cases h
                | cons lhs r1 =>
                  cases r1 with
                  | nil => cases h
                  | cons rhs r2 =>
                    cases r2 with
                    | cons _ _ => cases h
                    | nil =>
                      dsimp only at h
                      cases hl : emitRead language norm lhs with
                      | error _ => simp [hl, bind, Except.bind] at h
                      | ok _ =>
                        simp only [hl, bind, Except.bind] at h
                        cases hr : emitRead language norm rhs with
                        | error _ => simp [hr] at h
                        | ok _ =>
                          simp only [hr, pure, Except.pure, Except.ok.injEq] at h
                          subst h; rfl
      | named _ => cases h
  | addWithOverflow t dst flag lhs rhs =>
    dsimp only at h
    split at h
    · cases h
    · cases hl : emitRead language norm lhs with
      | error _ => simp [hl, bind, Except.bind] at h
      | ok _ =>
        simp only [hl, bind, Except.bind] at h
        cases hr : emitRead language norm rhs with
        | error _ => simp [hr] at h
        | ok _ =>
          simp only [hr, pure, Except.pure, Except.ok.injEq] at h
          subst h; rfl
  | load _ _ | store _ _ | beginLifetime _ | endLifetime _ | destructure _ _ | call _ _ _ =>
    cases h

private theorem getElem?_flatten_at {α : Type} :
    ∀ (xss : List (List α)) (i k : Nat) (xs : List α),
      xss[i]? = some xs → k < xs.length →
      xss.flatten[((xss.take i).map List.length).sum + k]? = xs[k]?
  | [], _, _, _, hi, _ => by cases hi
  | x :: rest, 0, k, xs, hi, hk => by
    cases Option.some.inj hi
    simp only [List.take_zero, List.map_nil, List.sum_nil, Nat.zero_add, List.flatten_cons]
    exact List.getElem?_append_left hk
  | x :: rest, i + 1, k, xs, hi, hk => by
    simp only [List.take_succ_cons, List.map_cons, List.sum_cons, List.flatten_cons, Nat.add_assoc]
    rw [List.getElem?_append_right (Nat.le_add_right _ _), Nat.add_sub_cancel_left]
    exact getElem?_flatten_at rest i k xs hi hk

private theorem length_flatten_eq {α : Type} :
    ∀ (xss : List (List α)), xss.flatten.length = (xss.map List.length).sum
  | [] => rfl
  | x :: rest => by simp [List.flatten_cons, List.length_append, length_flatten_eq rest]

private theorem map_length_of_emitInsts {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty}
    {norm : NormGraph lang} {j : Nat} {nb : NormBlock lang} {xss : List (List Source.Instruction)}
    (h : (indexed nb.body).mapM (fun p =>
      emitInst language norm (blockBase norm j + instBase j nb p.1) p.2.instruction) = .ok xss) :
    xss.map List.length = nb.body.map (fun op => instWidth op.instruction) := by
  obtain ⟨hLen, hEl⟩ := mapM_except_getElem? h
  have hLen' : xss.length = nb.body.length := hLen.trans (indexed_length nb.body)
  apply List.ext_getElem?
  intro i
  rw [List.getElem?_map, List.getElem?_map]
  cases hi : nb.body[i]? with
  | none =>
    have hGe : nb.body.length ≤ i := by
      rcases Nat.lt_or_ge i nb.body.length with hlt | hge
      · rw [List.getElem?_eq_getElem hlt] at hi; cases hi
      · exact hge
    rw [List.getElem?_eq_none (hLen'.symm ▸ hGe)]
    rfl
  | some op =>
    have hIdx : (indexed nb.body)[i]? = some (i, op) := by
      rw [Backend.Runtime.getElem?_indexed, hi]; rfl
    obtain ⟨xs, hXsGet, hXsEq⟩ := hEl hIdx
    rw [hXsGet]
    simp [emitInst_length hXsEq]

private theorem emitBlock_ok_inv {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty}
    {norm : NormGraph lang} {j : Nat} {nb : NormBlock lang} {lb : Source.Block}
    (h : emitBlock language norm (j, nb) = .ok lb) :
    ∃ phis xss term,
      (if j == 0 then Except.ok [] else (indexed nb.params).mapM fun p => emitPhi language norm j p.1) = .ok phis ∧
      (indexed nb.body).mapM (fun p =>
        emitInst language norm (blockBase norm j + instBase j nb p.1) p.2.instruction) = .ok xss ∧
      emitTerm language norm nb.term = .ok term ∧
      lb = { phis := phis.toArray, instructions := xss.flatten.toArray, terminator := term } := by
  unfold emitBlock at h
  dsimp only at h
  cases j with
  | zero =>
    simp only [show ((0 : Nat) == 0) = true from rfl, ↓reduceIte, bind, Except.bind, pure, Except.pure] at h ⊢
    cases hI : (indexed nb.body).mapM (fun p =>
        emitInst language norm (blockBase norm 0 + instBase 0 nb p.1) p.2.instruction) with
    | error e => simp [hI] at h
    | ok xss =>
      rw [hI] at h
      cases hT : emitTerm language norm nb.term with
      | error e => simp [hT] at h
      | ok term =>
        rw [hT] at h
        cases Except.ok.inj h
        exact ⟨[], xss, term, rfl, rfl, rfl, rfl⟩
  | succ j' =>
    have hBne : ((j' + 1 : Nat) == 0) = false := rfl
    simp only [hBne, Bool.false_eq_true, ↓reduceIte, bind, Except.bind, pure, Except.pure] at h ⊢
    cases hP : (indexed nb.params).mapM (fun p => emitPhi language norm (j' + 1) p.1) with
    | error e => simp [hP] at h
    | ok phis =>
      rw [hP] at h
      cases hI : (indexed nb.body).mapM (fun p =>
          emitInst language norm (blockBase norm (j' + 1) + instBase (j' + 1) nb p.1) p.2.instruction) with
      | error e => simp [hI] at h
      | ok xss =>
        rw [hI] at h
        cases hT : emitTerm language norm nb.term with
        | error e => simp [hT] at h
        | ok term =>
          rw [hT] at h
          cases Except.ok.inj h
          exact ⟨phis, xss, term, rfl, rfl, rfl, rfl⟩

private theorem emitBlock_insts_size {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty}
    {norm : NormGraph lang} {j : Nat} {nb : NormBlock lang} {lb : Source.Block}
    (h : emitBlock language norm (j, nb) = .ok lb) :
    lb.instructions.size = (nb.body.map fun op => instWidth op.instruction).sum := by
  obtain ⟨_, xss, _, _, hI, _, rfl⟩ := emitBlock_ok_inv h
  simp only [List.size_toArray, length_flatten_eq, map_length_of_emitInsts hI]

private theorem emitBlock_inst_slice {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty}
    {norm : NormGraph lang} {j : Nat} {nb : NormBlock lang} {lb : Source.Block}
    (h : emitBlock language norm (j, nb) = .ok lb)
    {i : Nat} {op : LocatedOp (SSAInst lang)} (hi : nb.body[i]? = some op) :
    ∃ xs, emitInst language norm (blockBase norm j + instBase j nb i) op.instruction = .ok xs ∧
      ∀ k, k < instWidth op.instruction →
        lb.instructions[((nb.body.take i).map fun o => instWidth o.instruction).sum + k]? = xs[k]? := by
  obtain ⟨_, xss, _, _, hI, _, rfl⟩ := emitBlock_ok_inv h
  obtain ⟨_, hEl⟩ := mapM_except_getElem? hI
  have hIdx : (indexed nb.body)[i]? = some (i, op) := by
    rw [Backend.Runtime.getElem?_indexed, hi]; rfl
  obtain ⟨xs, hXsGet, hXsEq⟩ := hEl hIdx
  refine ⟨xs, hXsEq, fun k hk => ?_⟩
  have hMap := map_length_of_emitInsts hI
  have hPos : ((nb.body.take i).map fun o => instWidth o.instruction).sum =
      ((xss.take i).map List.length).sum := by
    rw [List.map_take, ← hMap, ← List.map_take]
  rw [hPos]
  simpa only [List.getElem?_toArray] using
    getElem?_flatten_at xss i k xs hXsGet (by rw [emitInst_length hXsEq]; exact hk)

private theorem forIn_yields {ε σ α : Type} (F : α → σ → Except ε (ForInStep σ)) (g : α → σ → σ) :
    ∀ (l : List α) (init : σ),
      (∀ i (h : i < l.length), F l[i] ((l.take i).foldl (fun s a => g a s) init) =
        .ok (.yield (g l[i] ((l.take i).foldl (fun s a => g a s) init)))) →
      forIn l init F = .ok (l.foldl (fun s a => g a s) init)
  | [], _, _ => rfl
  | a :: l, init, h => by
    rw [List.forIn_cons]
    have h0 := h 0 (by simp)
    simp only [List.getElem_cons_zero, List.take_zero, List.foldl_nil] at h0
    simp only [h0, bind, Except.bind, List.foldl_cons]
    exact forIn_yields F g l (g a init) fun i hi => by
      have := h (i + 1) (by simp; omega)
      simpa only [List.getElem_cons_succ, List.take_succ_cons, List.foldl_cons] using this

private theorem size_foldl_set {α : Type} (g : α → Nat) (v : α → Option Source.Value) :
    ∀ (l : List α) (e : Source.Environment),
      (l.foldl (fun u x => u.set! (g x) (v x)) e).size = e.size
  | [], _ => rfl
  | x :: l, e => by simp only [List.foldl_cons, size_foldl_set g v l, size_set!]

private theorem foldl_push_toList {α β : Type} (h : α → β) :
    ∀ (l : List α) (acc : Array β),
      (l.foldl (fun pd a => pd.push (h a)) acc).toList = acc.toList ++ l.map h
  | [], acc => by simp
  | a :: l, acc => by simp

private theorem not_mem_take_of_nodup {α β : Type} (f : α → β) {l : List α} (hn : (l.map f).Nodup)
    {i : Nat} (hi : i < l.length) : f l[i] ∉ (l.take i).map f := by
  intro hm
  have hl : l = l.take i ++ l[i] :: l.drop (i + 1) := by simp
  rw [hl, List.map_append, List.map_cons] at hn
  exact (List.nodup_append.mp hn).2.2 _ hm _ (by simp) rfl

private theorem transfer_ok {f : Source.Function} {pred target : Nat} {env : Source.Environment}
    {tb : Source.Block} (htb : f.blocks[target]? = some tb) (w : Source.Phi → Source.Value)
    (hphi : ∀ i (h : i < tb.phis.toList.length), ∃ x op,
      tb.phis.toList[i].incoming.filter (·.1 == pred) = #[(x, op)] ∧
      Source.read env tb.phis.toList[i].width op = .ok (w tb.phis.toList[i]))
    (hdist : (tb.phis.toList.map (·.destination)).Nodup)
    (hW : ∀ phi ∈ tb.phis.toList, f.valueWidths[phi.destination]? = some .word ∧
      phi.destination < env.size ∧ (w phi).width = .word) :
    Source.transfer f pred target env = .ok ⟨target, 0,
      tb.phis.toList.foldl (fun u phi => u.set! phi.destination (some (w phi))) env⟩ := by
  unfold Source.transfer
  simp only [htb]
  rw [← Array.forIn_toList, forIn_yields _
    (fun phi (pd : Array (Source.ValueId × Source.Value)) => pd.push (phi.destination, w phi)) _ _
    ?loop]
  · simp only [bind, Except.bind]
    rw [← Array.forIn_toList, foldl_push_toList (fun phi => (phi.destination, w phi))]
    simp only [List.nil_append]
    rw [forIn_yields _ (fun (x : Source.ValueId × Source.Value) (u : Source.Environment) =>
      u.set! x.1 (some x.2)) _ _ ?writes]
    · simp [List.foldl_map, pure, Except.pure]
    case writes =>
      intro i hi
      have hm : (tb.phis.toList.map fun phi => (phi.destination, w phi))[i] ∈
          tb.phis.toList.map fun phi => (phi.destination, w phi) := List.getElem_mem hi
      obtain ⟨phi, hphm, he⟩ := List.mem_map.mp hm
      obtain ⟨hw, hs, hv⟩ := hW phi hphm
      rw [← he]
      have hsz := size_foldl_set (fun (x : Source.ValueId × Source.Value) => x.1)
        (fun x => some x.2) ((tb.phis.toList.map fun phi => (phi.destination, w phi)).take i) env
      simp only [write_ok hw (by rw [hsz]; exact hs) hv, pure, Except.pure]
  case loop =>
    intro i hi
    obtain ⟨x, op, hf, hr⟩ := hphi i hi
    have hany : ((tb.phis.toList.take i).foldl
        (fun (pd : Array (Source.ValueId × Source.Value)) phi => pd.push (phi.destination, w phi))
          #[]).any
        (fun y => y.fst == tb.phis.toList[i].destination) = false := by
      rw [Array.any_eq_false]
      intro k hk
      have hl := foldl_push_toList (fun phi => (phi.destination, w phi)) (tb.phis.toList.take i) #[]
      simp only [List.nil_append] at hl
      have hmem : (((tb.phis.toList.take i).foldl
          (fun (pd : Array (Source.ValueId × Source.Value)) phi => pd.push (phi.destination, w phi))
            #[])[k]).1 ∈
          (tb.phis.toList.take i).map (·.destination) := by
        have := Array.getElem_mem_toList hk
        rw [hl] at this
        obtain ⟨phi, hm, he⟩ := List.mem_map.mp this
        exact List.mem_map.mpr ⟨phi, hm, by rw [← he]⟩
      intro he
      exact not_mem_take_of_nodup (·.destination) hdist hi (by
        rw [beq_iff_eq] at he; rw [← he]; exact hmem)
    have h1 : (#[(x, op)].size != 1 || false) = false := rfl
    have h0 : (#[(x, op)] : Array (Source.BlockId × Source.Operand))[0]? = some (x, op) := rfl
    simp only [hf, hany, h1, h0, Bool.false_eq_true, ↓reduceIte, hr, bind, Except.bind, pure,
      Except.pure]

private theorem edgesTo_fst {Ty : Type} {lang : LoweringLanguage Ty} {t p : Nat}
    {term : NormTerm lang} : ∀ x ∈ edgesTo t p term, x.1 = p := by
  intro x hx
  cases term <;> simp only [edgesTo] at hx <;> (try split at hx) <;> (try split at hx) <;>
    simp at hx <;> (try rcases hx with rfl | rfl) <;> (try subst hx) <;> rfl

private theorem flatMap_indexedFrom_pick {α β : Type} (F : Nat → α → List β) (j : Nat) :
    ∀ (n : Nat) (l : List α), (Backend.Runtime.indexedFrom n l).flatMap
      (fun (kx : Nat × α) => if kx.1 = j then F kx.1 kx.2 else []) =
      if n ≤ j then (match l[j - n]? with | some x => F j x | none => []) else []
  | n, [] => by simp [Backend.Runtime.indexedFrom]
  | n, x :: l => by
    simp only [Backend.Runtime.indexedFrom, List.flatMap_cons]
    rw [flatMap_indexedFrom_pick F j (n + 1) l]
    rcases Nat.lt_trichotomy n j with h | h | h
    · have h1 : j - n = (j - (n + 1)) + 1 := by omega
      have h2 : n ≠ j := by omega
      simp only [h2, ↓reduceIte, List.nil_append, h1, List.getElem?_cons_succ,
        show n + 1 ≤ j by omega, show n ≤ j by omega]
    · subst h
      simp only [↓reduceIte, Nat.sub_self, List.getElem?_cons_zero, Nat.le_refl,
        show ¬ n + 1 ≤ n by omega, List.append_nil]
    · have h1 : ¬ n ≤ j := by omega
      have h2 : ¬ n + 1 ≤ j := by omega
      have h3 : n ≠ j := by omega
      simp only [h1, h2, h3, ↓reduceIte, List.nil_append]

private theorem incomingEdges_filter {Ty : Type} {lang : LoweringLanguage Ty}
    (norm : NormGraph lang) (target pred : Nat) {pb : NormBlock lang}
    (hpb : norm.blocks[pred]? = some pb) :
    (incomingEdges norm target).filter (·.1 == pred) = edgesTo target pred pb.term := by
  unfold incomingEdges
  rw [List.filter_flatMap]
  have hItem : ∀ p : Nat × NormBlock lang,
      (edgesTo target p.1 p.2.term).filter (·.1 == pred) =
        if p.1 = pred then edgesTo target p.1 p.2.term else [] := by
    intro ⟨k, b⟩
    by_cases h : k = pred
    · subst h
      simp only [↓reduceIte]
      exact List.filter_eq_self.mpr fun x hx => beq_iff_eq.mpr (edgesTo_fst x hx)
    · simp only [h, ↓reduceIte]
      have hFiltNil : ∀ (xs : List (Nat × PhiEdgeArg lang)),
          (∀ x ∈ xs, (x.1 == pred) = false) → xs.filter (·.1 == pred) = [] := by
        intro xs
        induction xs with
        | nil => intro _; rfl
        | cons a rs ih =>
          intro hAll
          simp only [List.filter_cons, hAll a List.mem_cons_self, Bool.false_eq_true, ↓reduceIte]
          exact ih fun x hx => hAll x (List.mem_cons_of_mem _ hx)
      refine hFiltNil _ fun x hx => ?_
      rw [edgesTo_fst x hx]
      cases hB : (k == pred) with
      | false => rfl
      | true => exact absurd (eq_of_beq hB) h
  have hCon : (indexed norm.blocks).flatMap (fun p => (edgesTo target p.1 p.2.term).filter (·.1 == pred)) =
      (indexed norm.blocks).flatMap (fun p => if p.1 = pred then edgesTo target p.1 p.2.term else []) := by
    congr 1; funext p; exact hItem p
  rw [hCon]
  have hPick := flatMap_indexedFrom_pick (fun k (b : NormBlock lang) => edgesTo target k b.term)
    pred 0 norm.blocks
  simpa [indexed, hpb] using hPick

private theorem filter_mapM_except_fst {α β γ ε : Type} {f : α → Except ε β} {g : α → γ}
    {xs : List α} {ys : List (γ × β)}
    (h : xs.mapM (fun a => do let b ← f a; return (g a, b)) = .ok ys) (p : γ → Bool) :
    (xs.filter (fun a => p (g a))).mapM (fun a => do let b ← f a; return (g a, b)) =
      .ok (ys.filter (fun y => p y.1)) := by
  induction xs generalizing ys with
  | nil => cases h; rfl
  | cons a rest ih =>
    rw [List.mapM_cons] at h
    cases hfa : f a with
    | error e =>
      simp only [hfa, bind, Except.bind] at h
      cases h
    | ok b =>
      cases hRest : rest.mapM (fun a => do let b ← f a; return (g a, b)) with
      | error e =>
        change (do let y ← (do let b ← f a; pure (g a, b)); let ys' ← rest.mapM (fun a => do let b ← f a; pure (g a, b)); pure (y :: ys')) = .ok ys at h
        rw [hfa, hRest] at h
        cases h
      | ok bs =>
        change (do let y ← (do let b ← f a; pure (g a, b)); let ys' ← rest.mapM (fun a => do let b ← f a; pure (g a, b)); pure (y :: ys')) = .ok ys at h
        rw [hfa, hRest] at h
        cases Except.ok.inj h
        simp only [List.filter_cons]
        cases hp : p (g a) with
        | false =>
          simp only [Bool.false_eq_true, ↓reduceIte]
          exact ih hRest
        | true =>
          simp only [↓reduceIte]
          rw [List.mapM_cons]
          change (do let y ← (do let b ← f a; pure (g a, b)); let ys' ← (rest.filter fun a => p (g a)).mapM (fun a => do let b ← f a; pure (g a, b)); pure (y :: ys')) = _
          rw [hfa, ih hRest]
          rfl

/-! ## 5. Constructive BIR determinism and soundness on virtual-only reachable states -/

private def execWriteParams {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (memory : Memory lang profile) :
    List (Sigma (MemoryPlace language.operands.Ty)) → List language.operands.Val →
    MemoryRegisters language profile →
    Option (MemoryRegisters language profile)
  | [], [], state => some state
  | ⟨ty, place⟩ :: ps, v :: vs, state =>
    if place.storage != .virtual || !place.logical.path.isEmpty || !state.movedFields.isEmpty then none
    else
      match
        (if state.live.contains place.logical.root then some state
         else if memory.classAt state.frame place.logical.root == some .virtual then
           some {state with live := place.logical.root :: state.live}
         else none) with
      | none => none
      | some started =>
        if !(memory.validPlace started place) then none
        else if Memory.initializedAt started place.logical.root place.logical.path &&
            !language.operands.isCopy ty then none
        else
          match memory.writeValue started place v with
          | none => none
          | some stored => execWriteParams memory ps vs stored
  | _, _, _ => none

private theorem execWriteParams_of_writeParams {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile}
    {locals : LocalTypes language.operands.Ty} {graph : BIRGraph lang}
    (hLk : AList.lookup bir.functions bir.entry = some (locals, graph))
    {params : List (Sigma (MemoryPlace language.operands.Ty))} {values : List language.operands.Val}
    {state next : MemoryRegisters language profile}
    (hFrame : state.frame = bir.entry)
    (hMoved : state.movedFields = [])
    (hAll : ∀ p ∈ params, (∃ r, p.2.logical.root = ⟨birBase locals graph + r⟩) ∧
      p.2.storage = .virtual ∧ p.2.logical.path = [])
    (hWP : (birMemoryView (language := language) bir).WriteParams params values state next false) :
    execWriteParams (birMemoryView (language := language) bir) params values state = some next := by
  generalize hEx : false = exh at hWP
  generalize hCeil : profile.memoryTarget.addressCeiling = ceil at hWP
  induction hWP with
  | nil _ => rfl
  | @cons ty place ps v vs st started stored nxt _ _ hStart hWrites hRest ih =>
    subst hCeil
    obtain ⟨⟨r, hRt⟩, hStor, hPath⟩ := hAll ⟨ty, place⟩ List.mem_cons_self
    have hStarted : started = (if st.live.contains place.logical.root then st
        else {st with live := place.logical.root :: st.live}) ∧
        (st.live.contains place.logical.root = false →
          (birMemoryView (language := language) bir).classAt st.frame place.logical.root = some .virtual) := by
      split at hStart
      · rename_i hLive
        simp only [hLive, ↓reduceIte]
        exact ⟨hStart, fun hF => by cases hF⟩
      · rename_i hNotLive
        have hF : st.live.contains place.logical.root = false := Bool.eq_false_iff.mpr hNotLive
        simp only [hF, Bool.false_eq_true, ↓reduceIte]
        rcases hStart.2 with ⟨hVirt, hEq⟩ | ⟨hMat, _⟩
        · exact ⟨hEq, fun _ => hVirt⟩
        · rw [hFrame, hRt] at hMat
          exact absurd hMat (birMemoryView_classAt_reg_ne_mat (language := language) hLk r)
    have hW := hWrites.2.2.1
    have hStFrame : started.frame = bir.entry := by
      rw [hStarted.1]; split <;> exact hFrame
    have hStMoved : started.movedFields = [] := by
      rw [hStarted.1]; split <;> exact hMoved
    have hStoredProps : stored.frame = bir.entry ∧ stored.movedFields = [] := by
      unfold Memory.writeValue at hW
      by_cases hCond : (!started.live.contains place.logical.root ||
          !(birMemoryView (language := language) bir).mayInitialize started place) = true
      · simp only [hCond, ↓reduceIte] at hW; cases hW
      · simp only [hCond, bind, Option.bind] at hW
        cases ha : (birMemoryView (language := language) bir).authorizeWrite started place with
        | none => simp [ha] at hW
        | some owners =>
          rw [ha] at hW
          dsimp only [Option.bind] at hW
          rw [hStor] at hW
          dsimp only at hW
          cases hr : Memory.replaceLocal (language := language)
              (Memory.markWritten started place owners).virtualValues place.logical v with
          | none => rw [hr] at hW; cases hW
          | some tbl =>
            rw [hr] at hW
            dsimp only [Option.bind, pure] at hW
            cases Option.some.inj hW
            exact ⟨hStFrame, by simp [Memory.markWritten, hStMoved]⟩
    unfold execWriteParams
    have hGuard1 : (place.storage != .virtual || !place.logical.path.isEmpty || !st.movedFields.isEmpty) = false := by
      rw [hStor, hPath, hMoved]; rfl
    simp only [hGuard1, Bool.false_eq_true, ↓reduceIte]
    have hStartOpt : (if st.live.contains place.logical.root then some st
        else if (birMemoryView (language := language) bir).classAt st.frame place.logical.root == some .virtual then
          some {st with live := place.logical.root :: st.live}
        else none) = some started := by
      cases hLv : st.live.contains place.logical.root with
      | true =>
        simp only [↓reduceIte]
        rw [hStarted.1, hLv]; rfl
      | false =>
        have hVirt := hStarted.2 hLv
        simp only [Bool.false_eq_true, ↓reduceIte, hVirt, beq_self_eq_true]
        rw [hStarted.1, hLv]; rfl
    rw [hStartOpt]
    dsimp only
    have hValid : (birMemoryView (language := language) bir).validPlace started place = true := hWrites.1.1
    have hCopyGuard : (Memory.initializedAt started place.logical.root place.logical.path &&
        !language.operands.isCopy ty) = false := by
      cases hi : Memory.initializedAt started place.logical.root place.logical.path with
      | false => rfl
      | true => simp [hWrites.1.2 hi]
    simp only [hValid, Bool.not_true, Bool.false_eq_true, ↓reduceIte, hCopyGuard, hW]
    exact ih hStoredProps.1 hStoredProps.2 (fun p hp => hAll p (List.mem_cons_of_mem _ hp)) hEx rfl
  | exhausted _ => cases hEx

private theorem validPlace_of_frame_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {memory : Memory lang profile} {ty : language.operands.Ty}
    {place : MemoryPlace language.operands.Ty ty}
    {s₁ s₂ : MemoryRegisters language profile} (h : s₁.frame = s₂.frame) :
    memory.validPlace s₁ place = memory.validPlace s₂ place := by
  unfold Memory.validPlace; rw [h]

private theorem loadValue_of_writeValue_virtual {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {memory : Memory lang profile} {ty : language.operands.Ty}
    {place : MemoryPlace language.operands.Ty ty} {v : language.operands.Val}
    {state stored : MemoryRegisters language profile}
    (hStor : place.storage = .virtual) (hPath : place.logical.path = [])
    (hMoved : state.movedFields = [])
    (hValid : memory.validPlace state place = true)
    (hW : memory.writeValue state place v = some stored) :
    memory.loadValue stored place = some v ∧
    stored.movedFields = [] ∧
    stored.resourceExhausted = state.resourceExhausted := by
  unfold Memory.writeValue at hW
  by_cases hCond : (!state.live.contains place.logical.root || !memory.mayInitialize state place) = true
  · simp only [hCond, ↓reduceIte] at hW; cases hW
  · simp only [hCond] at hW
    dsimp only [↓reduceIte, bind, Option.bind] at hW
    have hLive : state.live.contains place.logical.root = true := by
      cases hl : state.live.contains place.logical.root with
      | true => rfl
      | false => exfalso; apply hCond; rw [hl]; rfl
    cases ha : memory.authorizeWrite state place with
    | none => rw [ha] at hW; cases hW
    | some owners =>
      rw [ha, hStor] at hW
      dsimp only [Memory.replaceLocal] at hW
      rw [hPath] at hW
      simp only [List.isEmpty_nil, ↓reduceIte, bind, Option.bind, pure] at hW
      cases Option.some.inj hW
      have hM : (Memory.markWritten state place owners).movedFields = [] := by
        simp [Memory.markWritten, hMoved]
      have hInit : (Memory.markWritten state place owners).initialized.contains place.logical.root = true := by
        unfold Memory.markWritten
        dsimp only
        cases hi : state.initialized.contains place.logical.root with
        | true => simp only [↓reduceIte]; exact hi
        | false => simp
      refine ⟨?_, hM, rfl⟩
      generalize hS : ({ Memory.markWritten state place owners with
          virtualValues := (place.logical.root, v) ::
            AList.erase (Memory.markWritten state place owners).virtualValues place.logical.root } :
          MemoryRegisters language profile) = s'
      have hSFrame : s'.frame = state.frame := by rw [← hS]; rfl
      have hSLive : s'.live.contains place.logical.root = true := by rw [← hS]; exact hLive
      have hSInit : s'.initialized.contains place.logical.root = true := by rw [← hS]; exact hInit
      have hSMoved : s'.movedFields = [] := by rw [← hS]; exact hM
      have hSVirt : AList.lookup s'.virtualValues place.logical.root = some v := by
        rw [← hS]; simp [AList.lookup]
      unfold Memory.loadValue Memory.loadBacking Memory.initializedAt
      rw [hSInit, hSMoved]
      simp only [List.any_nil, Bool.not_false, Bool.and_true, ↓reduceIte]
      rw [validPlace_of_frame_eq hSFrame, hValid, hSLive]
      simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte, hStor, hSVirt, hPath, List.isEmpty_nil,
        bind, Option.bind]

private theorem writeParams_of_execWriteParams {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {memory : Memory lang profile}
    {params : List (Sigma (MemoryPlace language.operands.Ty))} {values : List language.operands.Val}
    {state next : MemoryRegisters language profile}
    (hExec : execWriteParams memory params values state = some next) :
    memory.WriteParams params values state next false ∧
    next.resourceExhausted = state.resourceExhausted := by
  induction params generalizing values state with
  | nil =>
    cases values with
    | nil =>
      cases Option.some.inj hExec
      exact ⟨.nil _, rfl⟩
    | cons _ _ => cases hExec
  | cons hd tl ih =>
    cases values with
    | nil => cases hExec
    | cons v vs =>
      rcases hd with ⟨ty, place⟩
      unfold execWriteParams at hExec
      cases hG1 : (place.storage != .virtual || !place.logical.path.isEmpty || !state.movedFields.isEmpty) with
      | true => rw [hG1] at hExec; cases hExec
      | false =>
        rw [hG1] at hExec
        simp only [Bool.false_eq_true, ↓reduceIte] at hExec
        have hStor : place.storage = .virtual := by
          cases hs : place.storage
          · rfl
          · simp [hs] at hG1
        have hPath : place.logical.path = [] := by
          cases hp : place.logical.path
          · rfl
          · simp [hp] at hG1
        have hMoved : state.movedFields = [] := by
          cases hm : state.movedFields
          · rfl
          · simp [hm] at hG1
        cases hStartOpt : (if state.live.contains place.logical.root then some state
            else if memory.classAt state.frame place.logical.root == some .virtual then
              some {state with live := place.logical.root :: state.live}
            else none) with
        | none => rw [hStartOpt] at hExec; cases hExec
        | some started =>
          rw [hStartOpt] at hExec
          dsimp only at hExec
          have hStartProps :
              (if state.live.contains place.logical.root then started = state
               else memory.Begins state place.logical.root started) ∧
              started.movedFields = [] ∧
              started.resourceExhausted = state.resourceExhausted := by
            cases hLv : state.live.contains place.logical.root with
            | true =>
              simp only [hLv, ↓reduceIte, Option.some.injEq] at hStartOpt
              subst hStartOpt
              simp only [↓reduceIte]
              exact ⟨trivial, hMoved, trivial⟩
            | false =>
              simp only [hLv, Bool.false_eq_true, ↓reduceIte] at hStartOpt ⊢
              cases hCls : (memory.classAt state.frame place.logical.root == some .virtual) with
              | false => rw [hCls] at hStartOpt; cases hStartOpt
              | true =>
                simp only [hCls, ↓reduceIte, Option.some.injEq] at hStartOpt
                subst hStartOpt
                have hNotMem : place.logical.root ∉ state.live := by
                  intro hm; have : state.live.contains place.logical.root = true := by
                    simpa [List.contains_eq_mem] using hm
                  rw [hLv] at this; cases this
                exact ⟨⟨hNotMem, Or.inl ⟨eq_of_beq hCls, rfl⟩⟩, hMoved, rfl⟩
          cases hValid : memory.validPlace started place with
          | false => simp [hValid] at hExec
          | true =>
            simp only [hValid, Bool.not_true, Bool.false_eq_true, ↓reduceIte] at hExec
            cases hCopyG : (Memory.initializedAt started place.logical.root place.logical.path &&
                !language.operands.isCopy ty) with
            | true => simp [hCopyG] at hExec
            | false =>
              simp only [hCopyG, Bool.false_eq_true, ↓reduceIte] at hExec
              cases hW : memory.writeValue started place v with
              | none => rw [hW] at hExec; cases hExec
              | some stored =>
                rw [hW] at hExec
                dsimp only at hExec
                obtain ⟨hLoad, _, hExhStored⟩ :=
                  loadValue_of_writeValue_virtual hStor hPath hStartProps.2.1 hValid hW
                obtain ⟨hRestWP, hExhNext⟩ := ih hExec
                have hCopyImp : Memory.initializedAt started place.logical.root place.logical.path = true →
                    language.operands.isCopy ty = true := by
                  intro hi
                  rw [hi, Bool.true_and] at hCopyG
                  cases hc : language.operands.isCopy ty with
                  | true => rfl
                  | false => rw [hc] at hCopyG; cases hCopyG
                have hWrites : memory.Writes started place v stored :=
                  ⟨⟨hValid, hCopyImp⟩, (fun hm => by rw [hStor] at hm; cases hm), hW,
                    ⟨hLoad, fun hm => by rw [hStor] at hm; cases hm⟩⟩
                exact ⟨.cons hStartProps.1 hWrites hRestWP,
                  hExhNext.trans (hExhStored.trans hStartProps.2.2)⟩

private def execEarlyOp (env : Declarations) (op : Operator) (types : List EarlyTy)
    (ty : EarlyTy) (values : List EarlyValue) : Option EarlyValue :=
  match op, types, ty, values with
  | .read, [.nominal src], .nominal dst, [.other value] =>
    if src = dst && value = dst && env.emptyLinear dst then some (.other value) else none
  | .read, [.scalar input], .scalar result, [.scalar value] =>
    if input = result ∧ value.ty = result then some (.scalar value) else none
  | .arithmetic (.plain .add), [.scalar (.int left), .scalar (.int right)],
      .scalar (.int result), [.scalar (.int lt l), .scalar (.int rt r)] =>
    if left = result ∧ right = result ∧ lt = result ∧ rt = result then
      match plain result .add l r with
      | .ok v => some (.scalar (.int result v))
      | _ => none
    else none
  | _, _, _, _ => none

private def opNotNamed : Operator → Prop
  | .named _ => False
  | _ => True

private theorem execEarlyOp_of_sliceOperation {slice : Slice} {env : Declarations}
    {op : Operator} {types : List (sliceLanguage slice).operands.Ty}
    {ty : (sliceLanguage slice).operands.Ty} {values : List (sliceLanguage slice).operands.Val}
    {target : Target} {addrs : List (OperationAddress (sliceLanguage slice).operands.Ty)}
    {before after : OperationStore (sliceLanguage slice).operands.Val}
    {result : (sliceLanguage slice).operands.Val} {effects : List (sliceLanguage slice).operands.Event}
    (h : (sliceLanguage slice).operation env op types ty values target addrs before result effects after)
    (hNotNamed : opNotNamed op := by trivial) :
    addrs = [] ∧ effects = [] ∧ before = after ∧ execEarlyOp env op types ty values = some result := by
  obtain ⟨hA, hOp, hE, hB⟩ := h
  refine ⟨hA, hE, hB, ?_⟩
  have aux : ∀ (op : Operator) (types : List EarlyTy) (ty : EarlyTy) (values : List EarlyValue)
      (result : EarlyValue),
      opNotNamed op →
      (sliceLanguage slice).operation env op types ty values target [] before result [] before →
      execEarlyOp env op types ty values = some result := by
    intro op types ty values result hNN h'
    have hOp := h'.2.1
    cases op with
    | named _ => exact False.elim hNN
    | read =>
      cases types with
      | nil => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
      | cons t0 r0 =>
        cases t0 with
        | nominal src =>
          cases r0 with
          | cons _ _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
          | nil =>
            cases ty with
            | scalar _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
            | nominal dst =>
              cases values with
              | nil => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
              | cons v0 vr =>
                cases v0 with
                | scalar _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                | other val =>
                  cases vr with
                  | cons _ _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                  | nil =>
                    unfold execEarlyOp
                    have hb : (if (decide (src = dst) && decide (val = dst) && env.emptyLinear dst) = true then
                        Res.ok (CoreValue.other val) else .stuck) = .ok result := hOp
                    by_cases hc : (decide (src = dst) && decide (val = dst) && env.emptyLinear dst) = true
                    · simp only [hc, ↓reduceIte] at hb ⊢; cases hb; rfl
                    · simp only [hc] at hb; cases hb
        | scalar input =>
          cases r0 with
          | cons _ _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
          | nil =>
            cases ty with
            | nominal _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
            | scalar resTy =>
              cases values with
              | nil => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
              | cons v0 vr =>
                cases v0 with
                | other _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                | scalar val =>
                  cases vr with
                  | cons _ _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                  | nil =>
                    unfold execEarlyOp
                    have hb : (if input = resTy ∧ val.ty = resTy then
                        Res.ok (CoreValue.scalar val) else .stuck) = .ok result := hOp
                    by_cases hc : input = resTy ∧ val.ty = resTy
                    · simp only [hc] at hb ⊢; cases hb; rfl
                    · simp only [hc] at hb; cases hb
    | arithmetic mOp =>
      cases mOp with
      | wrapping _ | saturating _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
      | plain ao =>
        cases ao with
        | sub | mul | div | rem | shl | shr => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
        | add =>
          cases types with
          | nil => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
          | cons t0 r0 =>
            cases t0 with
            | nominal _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
            | scalar s0 =>
              cases s0 with
              | bool | char | unit => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
              | int left =>
                cases r0 with
                | nil => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                | cons t1 r1 =>
                  cases t1 with
                  | nominal _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                  | scalar s1 =>
                    cases s1 with
                    | bool | char | unit => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                    | int right =>
                      cases r1 with
                      | cons _ _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                      | nil =>
                        cases ty with
                        | nominal _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                        | scalar s2 =>
                          cases s2 with
                          | bool | char | unit => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                          | int resInt =>
                            cases values with
                            | nil => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                            | cons v0 vr0 =>
                              cases v0 with
                              | other _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                              | scalar cv0 =>
                                cases cv0 with
                                | bool _ | char _ | unit => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                                | int lt l =>
                                  cases vr0 with
                                  | nil => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                                  | cons v1 vr1 =>
                                    cases v1 with
                                    | other _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                                    | scalar cv1 =>
                                      cases cv1 with
                                      | bool _ | char _ | unit => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                                      | int rt r =>
                                        cases vr1 with
                                        | cons _ _ => exact absurd (hOp : Res.stuck = .ok result) (by intro x; cases x)
                                        | nil =>
                                          unfold execEarlyOp
                                          have hb : (if left = resInt ∧ right = resInt ∧ lt = resInt ∧ rt = resInt then
                                              (plain resInt .add l r).map (CoreValue.scalar ∘ CVal.int resInt)
                                              else .stuck) = .ok result := hOp
                                          by_cases hc : left = resInt ∧ right = resInt ∧ lt = resInt ∧ rt = resInt
                                          · simp only [hc] at hb ⊢
                                            cases hp : plain resInt .add l r with
                                            | ok v => simp only [hp, Res.map] at hb; cases hb; rfl
                                            | trap _ => simp only [hp, Res.map] at hb; cases hb
                                            | stuck => simp only [hp, Res.map] at hb; cases hb
                                          · simp only [hc] at hb; cases hb
  exact aux op types ty values result hNotNamed ⟨rfl, hOp, rfl, rfl⟩

private theorem execEarlyOp_sound {slice : Slice} {env : Declarations}
    {op : Operator} {types : List (sliceLanguage slice).operands.Ty}
    {ty : (sliceLanguage slice).operands.Ty} {values : List (sliceLanguage slice).operands.Val}
    {target : Target} {before : OperationStore (sliceLanguage slice).operands.Val}
    {result : (sliceLanguage slice).operands.Val}
    (h : execEarlyOp env op types ty values = some result) :
    (sliceLanguage slice).operation env op types ty values target [] before result [] before := by
  have aux : ∀ (op : Operator) (types : List EarlyTy) (ty : EarlyTy) (values : List EarlyValue)
      (result : EarlyValue),
      execEarlyOp env op types ty values = some result →
      (sliceLanguage slice).operation env op types ty values target [] before result [] before := by
    intro op types ty values result h'
    cases op with
    | named _ => cases h'
    | read =>
      cases types with
      | nil => cases h'
      | cons t0 r0 =>
        cases t0 with
        | nominal src =>
          cases r0 with
          | cons _ _ => cases h'
          | nil =>
            cases ty with
            | scalar _ => cases h'
            | nominal dst =>
              cases values with
              | nil => cases h'
              | cons v0 vr =>
                cases v0 with
                | scalar _ => cases h'
                | other val =>
                  cases vr with
                  | cons _ _ => cases h'
                  | nil =>
                    unfold execEarlyOp at h'
                    by_cases hc : (decide (src = dst) && decide (val = dst) && env.emptyLinear dst) = true
                    · simp only [hc, ↓reduceIte, Option.some.injEq] at h'
                      subst h'
                      refine ⟨rfl, ?_, rfl, rfl⟩
                      change (if (decide (src = dst) && decide (val = dst) && env.emptyLinear dst) = true then
                        Res.ok (CoreValue.other val) else .stuck) = .ok (.other val)
                      simp [hc]
                    · simp only [hc, Bool.false_eq_true, ↓reduceIte] at h'; cases h'
        | scalar input =>
          cases r0 with
          | cons _ _ => cases h'
          | nil =>
            cases ty with
            | nominal _ => cases h'
            | scalar resTy =>
              cases values with
              | nil => cases h'
              | cons v0 vr =>
                cases v0 with
                | other _ => cases h'
                | scalar val =>
                  cases vr with
                  | cons _ _ => cases h'
                  | nil =>
                    unfold execEarlyOp at h'
                    by_cases hc : input = resTy ∧ val.ty = resTy
                    · simp only [hc.1, hc.2, and_self, ↓reduceIte, Option.some.injEq] at h'
                      subst h'
                      refine ⟨rfl, ?_, rfl, rfl⟩
                      change (if input = resTy ∧ val.ty = resTy then
                        Res.ok (CoreValue.scalar val) else .stuck) = .ok (.scalar val)
                      simp [hc]
                    · simp only [hc, ↓reduceIte] at h'; cases h'
    | arithmetic mOp =>
      cases mOp with
      | wrapping _ | saturating _ => cases h'
      | plain ao =>
        cases ao with
        | sub | mul | div | rem | shl | shr => cases h'
        | add =>
          cases types with
          | nil => cases h'
          | cons t0 r0 =>
            cases t0 with
            | nominal _ => cases h'
            | scalar s0 =>
              cases s0 with
              | bool | char | unit => cases h'
              | int left =>
                cases r0 with
                | nil => cases h'
                | cons t1 r1 =>
                  cases t1 with
                  | nominal _ => cases h'
                  | scalar s1 =>
                    cases s1 with
                    | bool | char | unit => cases h'
                    | int right =>
                      cases r1 with
                      | cons _ _ => cases h'
                      | nil =>
                        cases ty with
                        | nominal _ => cases h'
                        | scalar s2 =>
                          cases s2 with
                          | bool | char | unit => cases h'
                          | int resInt =>
                            cases values with
                            | nil => cases h'
                            | cons v0 vr0 =>
                              cases v0 with
                              | other _ => cases h'
                              | scalar cv0 =>
                                cases cv0 with
                                | bool _ | char _ | unit => cases h'
                                | int lt l =>
                                  cases vr0 with
                                  | nil => cases h'
                                  | cons v1 vr1 =>
                                    cases v1 with
                                    | other _ => cases h'
                                    | scalar cv1 =>
                                      cases cv1 with
                                      | bool _ | char _ | unit => cases h'
                                      | int rt r =>
                                        cases vr1 with
                                        | cons _ _ => cases h'
                                        | nil =>
                                          unfold execEarlyOp at h'
                                          by_cases hc : left = resInt ∧ right = resInt ∧ lt = resInt ∧ rt = resInt
                                          · simp only [hc.1, hc.2.1, hc.2.2.1, hc.2.2.2, and_self, ↓reduceIte] at h'
                                            cases hp : plain resInt .add l r with
                                            | ok v =>
                                              simp only [hp, Option.some.injEq] at h'
                                              subst h'
                                              refine ⟨rfl, ?_, rfl, rfl⟩
                                              change (if left = resInt ∧ right = resInt ∧ lt = resInt ∧ rt = resInt then
                                                (plain resInt .add l r).map (CoreValue.scalar ∘ CVal.int resInt)
                                                else .stuck) = .ok (.scalar (.int resInt v))
                                              simp only [hc.1, hc.2.1, hc.2.2.1, hc.2.2.2, and_self, ↓reduceIte, hp]; rfl
                                            | trap _ => simp [hp] at h'
                                            | stuck => simp [hp] at h'
                                          · simp only [hc, ↓reduceIte] at h'; cases h'
  exact aux op types ty values result h

/-- Computable single-step interpreter for entry-only virtual BIR states. Both sound
(`execBIRStep_sound`) and complete on reachable LCB states, enabling a 100% constructive
`BehaviorRefinement` proof with zero `Classical.choice`. -/
private def execBIRStep {slice : Slice} {profile : Profile}
    (bir : BIR (sliceLoweringLanguage slice) profile)
    (state : MemoryRegisters (sliceLanguage slice) profile) :
    Option (MemoryRegisters (sliceLanguage slice) profile) :=
  let mem := birMemoryView (language := sliceLanguage slice) bir
  if state.resourceExhausted || (birHostedResult mem state).isSome then none
  else
    match mem.blockAt state with
    | none => none
    | some block =>
      match block.body[state.instruction]? with
      | some op =>
        match op.instruction with
        | @MemoryInst.op _ _ opcode types ty dst _ args checks addresses =>
          if !(checks.isEmpty && addresses.isEmpty) then none
          else
            match mem.readArgs args state with
            | none => none
            | some (values, afterReads) =>
              match execEarlyOp mem.declarations opcode types ty values with
              | none => none
              | some result =>
                match execWriteParams mem [⟨ty, dst⟩] [result] afterReads with
                | none => none
                | some stored => some stored.advance
        | .addWithOverflow t dst flag lhs rhs =>
          if dst.logical.root == flag.logical.root then none
          else
            match mem.readValue state lhs with
            | none => none
            | some (lv, afterLhs) =>
              match mem.readValue afterLhs rhs with
              | none => none
              | some (rv, afterReads) =>
                match (lv : EarlyValue), (rv : EarlyValue) with
                | .scalar (.int t1 x), .scalar (.int t2 y) =>
                  if t1 == t && t2 == t then
                    let sum : (sliceLanguage slice).operands.Val :=
                      CoreValue.scalar (.int t (wrapping t .add x y))
                    let ovf : (sliceLanguage slice).operands.Val :=
                      CoreValue.scalar (.bool (decide (checked t .add x y = .error .overflow)))
                    match execWriteParams mem [⟨_, dst⟩, ⟨_, flag⟩] [sum, ovf] afterReads with
                    | none => none
                    | some stored => some stored.advance
                  else none
                | _, _ => none
        | _ => none
      | none =>
        if state.instruction != block.body.length then none
        else match block.ending with
        | .jump edge =>
          match mem.readOperands edge.arguments state with
          | none => none
          | some (values, afterRead) =>
            match mem.blockAt {state with block := edge.target} with
            | none => none
            | some tgtBlk =>
              letI := (sliceLanguage slice).typeDecidableEq
              if !(decide (edge.arguments.map Sigma.fst = tgtBlk.params.map Sigma.fst)) then none
              else execWriteParams mem tgtBlk.params values {afterRead with block := edge.target, instruction := 0}
        | .branch test yes no =>
          match mem.readValue state test with
          | none => none
          | some (value, afterRead) =>
            match (sliceLanguage slice).operands.test value with
            | none => none
            | some flag =>
              let edge := if flag then yes else no
              match mem.readOperands edge.arguments afterRead with
              | none => none
              | some (values, afterArgs) =>
                match mem.blockAt {afterRead with block := edge.target} with
                | none => none
                | some tgtBlk =>
                  letI := (sliceLanguage slice).typeDecidableEq
                  if !(decide (edge.arguments.map Sigma.fst = tgtBlk.params.map Sigma.fst)) then none
                  else execWriteParams mem tgtBlk.params values {afterArgs with block := edge.target, instruction := 0}
        | _ => none

/-! ## 6. Frame purity, value-update helpers, and BIR-to-LCB simulation -/

private structure FramePure {slice : Slice} {profile : Profile}
    (bir : BIR (sliceLoweringLanguage slice) profile)
    (p : MemoryRegisters (sliceLanguage slice) profile) : Prop where
  frame : p.frame = bir.entry
  callers : p.callers = []
  exhausted : p.resourceExhausted = false
  effects : p.effects = []
  materialized : p.materialized = []
  zeroValues : p.zeroValues = []
  addressIds : p.addressIds = []
  movedFields : p.movedFields = []
  bytes : p.bytes = Mem.Memory.empty

private theorem loadValue_virtual_lookup {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    {base : Nat} {ty : (sliceLanguage slice).operands.Ty}
    {reg : SSAValue (sliceLanguage slice).operands.Ty ty}
    {p : MemoryRegisters (sliceLanguage slice) profile}
    {v : (sliceLanguage slice).operands.Val}
    (hLoad : (birMemoryView (language := sliceLanguage slice) bir).loadValue p (birReg base reg) = some v) :
    AList.lookup p.virtualValues ⟨base + reg.id⟩ = some v := by
  unfold Memory.loadValue Memory.loadBacking at hLoad
  dsimp only [birReg, List.isEmpty_nil] at hLoad
  cases hInit : Memory.initializedAt p ⟨base + reg.id⟩ [] with
  | false => simp only [hInit, Bool.false_eq_true, ↓reduceIte] at hLoad; cases hLoad
  | true =>
    simp only [hInit, ↓reduceIte] at hLoad
    cases h1 : (birMemoryView (language := sliceLanguage slice) bir).validPlace p
        (ty := ty) { logical := { root := ⟨base + reg.id⟩, path := [] }, storage := .virtual } with
    | false => simp only [h1, Bool.not_false, ↓reduceIte] at hLoad; cases hLoad
    | true =>
      simp only [h1, Bool.not_true, Bool.false_eq_true, ↓reduceIte] at hLoad
      cases h2 : p.live.contains ⟨base + reg.id⟩ with
      | false => simp only [h2, Bool.not_false, ↓reduceIte] at hLoad; cases hLoad
      | true =>
        simp only [h2, Bool.not_true, Bool.false_eq_true, ↓reduceIte, bind, Option.bind] at hLoad
        cases hLk : AList.lookup p.virtualValues ⟨base + reg.id⟩ with
        | none => rw [hLk] at hLoad; cases hLoad
        | some u =>
          rw [hLk] at hLoad
          dsimp only [pure] at hLoad
          cases Option.some.inj hLoad
          rfl

private def readMatchesVirtual {slice : Slice} {profile : Profile} (base : Nat)
    {ty : (sliceLanguage slice).operands.Ty} (p : MemoryRegisters (sliceLanguage slice) profile)
    (v : (sliceLanguage slice).operands.Val) :
    SSARead (sliceLanguage slice).operands.Ty (sliceLoweringLanguage slice).literal ty → Prop
  | .literal lit => (sliceLanguage slice).literal ty lit.encoding = some v
  | .value reg => AList.lookup p.virtualValues ⟨base + reg.id⟩ = some v

private theorem readValue_virtual_inv {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    {base : Nat} {ty : (sliceLanguage slice).operands.Ty}
    {read : SSARead (sliceLanguage slice).operands.Ty (sliceLoweringLanguage slice).literal ty}
    {p afterRead : MemoryRegisters (sliceLanguage slice) profile}
    {v : (sliceLanguage slice).operands.Val}
    (hPure : FramePure bir p)
    (hRead : (birMemoryView (language := sliceLanguage slice) bir).readValue p
      (birRead (language := sliceLanguage slice) base read) = some (v, afterRead)) :
    FramePure bir afterRead ∧
    afterRead.block = p.block ∧ afterRead.instruction = p.instruction ∧
    (∀ r u, AList.lookup afterRead.virtualValues ⟨base + r⟩ = some u →
      AList.lookup p.virtualValues ⟨base + r⟩ = some u) ∧
    readMatchesVirtual base p v read := by
  cases read with
  | literal lit =>
    simp only [birRead, Memory.readValue] at hRead
    cases hl : (sliceLanguage slice).literal ty lit.encoding with
    | none => simp [hl] at hRead
    | some val =>
      simp only [hl] at hRead
      cases Option.some.inj hRead
      exact ⟨hPure, rfl, rfl, fun _ _ h => h, hl⟩
  | value reg =>
    dsimp only [birRead] at hRead
    cases hCopy : (sliceLanguage slice).operands.isCopy ty with
    | true =>
      simp only [hCopy, ↓reduceIte, Memory.copyRead_iff] at hRead
      obtain ⟨_, _, hLoad, hEq⟩ := hRead
      have hLk := loadValue_virtual_lookup hLoad
      subst hEq
      exact ⟨hPure, rfl, rfl, fun _ _ h => h, hLk⟩
    | false =>
      simp only [hCopy, Bool.false_eq_true, ↓reduceIte, Memory.readValue, bind, Option.bind] at hRead
      cases hMove : Own.move ((birMemoryView (language := sliceLanguage slice) bir).ownerView p
          (birReg base reg).logical.root ty .owned).1
          ((birMemoryView (language := sliceLanguage slice) bir).ownerView p
          (birReg base reg).logical.root ty .owned).2 with
      | none => simp [hMove] at hRead
      | some owners =>
        rw [hMove] at hRead
        dsimp only at hRead
        cases hLoad : (birMemoryView (language := sliceLanguage slice) bir).loadValue p (birReg base reg) with
        | none => rw [hLoad] at hRead; cases hRead
        | some val =>
          rw [hLoad] at hRead
          dsimp only [birReg, List.isEmpty_nil, pure] at hRead
          simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte] at hRead
          cases Option.some.inj hRead
          have hLk := loadValue_virtual_lookup hLoad
          refine ⟨⟨hPure.frame, hPure.callers, hPure.exhausted, hPure.effects, hPure.materialized,
            by simp [hPure.zeroValues, AList.erase], hPure.addressIds, hPure.movedFields,
            hPure.bytes⟩, rfl, rfl, ?_, hLk⟩
          intro r u hu
          have hSub : ∀ (tbl : AList PlaceId (sliceLanguage slice).operands.Val) (k₁ k₂ : PlaceId),
              AList.lookup (AList.erase tbl k₁) k₂ = some u → AList.lookup tbl k₂ = some u := by
            intro tbl k₁ k₂ hEr
            induction tbl with
            | nil => cases hEr
            | cons hd tl ih =>
              rcases hd with ⟨k', v'⟩
              simp only [AList.erase] at hEr
              split at hEr
              · rename_i hEq
                simp only [AList.lookup]
                split
                · rename_i hEq2
                  subst hEq; subst hEq2
                  have : AList.lookup (AList.erase tl k') k' = none := by
                    have hNoMem : ∀ (l : AList PlaceId (sliceLanguage slice).operands.Val),
                        AList.lookup (AList.erase l k') k' = none := by
                      intro l
                      induction l with
                      | nil => rfl
                      | cons a rs ihl =>
                        rcases a with ⟨ka, va⟩
                        simp only [AList.erase]
                        split
                        · exact ihl
                        · rename_i hne; simp [AList.lookup, hne, ihl]
                    exact hNoMem tl
                  rw [this] at hEr; cases hEr
                · exact ih hEr
              · simp only [AList.lookup] at hEr ⊢
                by_cases hEq2 : k' = k₂
                · simp only [hEq2, ↓reduceIte] at hEr ⊢; exact hEr
                · simp only [hEq2, ↓reduceIte] at hEr ⊢; exact ih hEr
          exact hSub p.virtualValues ⟨base + reg.id⟩ ⟨base + r⟩ hu

private theorem readOperands_virtual_inv {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {base : Nat}
    {args : List (Sigma (SSARead (sliceLanguage slice).operands.Ty (sliceLoweringLanguage slice).literal))}
    {p afterRead : MemoryRegisters (sliceLanguage slice) profile}
    {values : List (sliceLanguage slice).operands.Val}
    (hPure : FramePure bir p)
    (hRead : (birMemoryView (language := sliceLanguage slice) bir).readOperands
      (args.map (birOpnd (language := sliceLanguage slice) base)) p = some (values, afterRead)) :
    FramePure bir afterRead ∧
    afterRead.block = p.block ∧ afterRead.instruction = p.instruction ∧
    values.length = args.length ∧
    (∀ r u, AList.lookup afterRead.virtualValues ⟨base + r⟩ = some u →
      AList.lookup p.virtualValues ⟨base + r⟩ = some u) ∧
    (∀ (k : Nat) (arg : Sigma (SSARead (sliceLanguage slice).operands.Ty (sliceLoweringLanguage slice).literal))
      (val : (sliceLanguage slice).operands.Val), args[k]? = some arg → values[k]? = some val →
      match arg.2 with
      | .literal lit => (sliceLanguage slice).literal arg.1 lit.encoding = some val
      | .value reg => AList.lookup p.virtualValues ⟨base + reg.id⟩ = some val) := by
  induction args generalizing p values with
  | nil =>
    cases Option.some.inj hRead
    exact ⟨hPure, rfl, rfl, rfl, fun _ _ h => h, fun _ _ _ h _ => by simp at h⟩
  | cons hd tl ih =>
    rcases hd with ⟨ty, r0⟩
    simp only [List.map_cons, birOpnd, Memory.readOperands, bind, Option.bind] at hRead
    cases h0 : (birMemoryView (language := sliceLanguage slice) bir).readValue p
        (birRead (language := sliceLanguage slice) base r0) with
    | none => simp [h0] at hRead
    | some pair0 =>
      rcases pair0 with ⟨v0, mid⟩
      simp only [h0] at hRead
      cases hRest : (birMemoryView (language := sliceLanguage slice) bir).readOperands
          (tl.map (birOpnd (language := sliceLanguage slice) base)) mid with
      | none => simp [hRest] at hRead
      | some pair1 =>
        rcases pair1 with ⟨vs, nxt⟩
        simp only [hRest, pure, Option.some.injEq, Prod.mk.injEq] at hRead
        obtain ⟨rfl, rfl⟩ := hRead
        obtain ⟨hPureMid, hBlkMid, hInstMid, hSubMid, hVal0⟩ := readValue_virtual_inv hPure h0
        obtain ⟨hPureNxt, hBlkNxt, hInstNxt, hLenTl, hSubTl, hValsTl⟩ := ih hPureMid hRest
        refine ⟨hPureNxt, hBlkNxt.trans hBlkMid, hInstNxt.trans hInstMid, by simp [hLenTl],
          fun r u hu => hSubMid r u (hSubTl r u hu), ?_⟩
        intro k arg val hArg hVal
        cases k with
        | zero =>
          cases Option.some.inj hArg
          cases Option.some.inj hVal
          cases r0 <;> exact hVal0
        | succ k' =>
          have hStep := hValsTl k' arg val hArg hVal
          rcases arg with ⟨aty, ar⟩
          cases ar with
          | literal lit => exact hStep
          | value reg => exact hSubMid reg.id val hStep

private theorem alist_lookup_erase_ne {κ α : Type} [DecidableEq κ] {k₁ k₂ : κ} (hne : k₁ ≠ k₂) :
    ∀ (l : AList κ α), AList.lookup (AList.erase l k₁) k₂ = AList.lookup l k₂
  | [] => rfl
  | (k, v) :: rest => by
    simp only [AList.erase]
    split
    · rename_i hEq
      subst hEq
      simp only [AList.lookup, hne, ↓reduceIte]
      exact alist_lookup_erase_ne hne rest
    · simp only [AList.lookup]
      split
      · rfl
      · exact alist_lookup_erase_ne hne rest

private theorem writeValue_reg_inv {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    {base : Nat} {ty : (sliceLanguage slice).operands.Ty}
    {reg : SSAValue (sliceLanguage slice).operands.Ty ty}
    {v : (sliceLanguage slice).operands.Val}
    {p stored : MemoryRegisters (sliceLanguage slice) profile}
    (hPure : FramePure bir p)
    (hW : (birMemoryView (language := sliceLanguage slice) bir).writeValue p (birReg base reg) v = some stored) :
    FramePure bir stored ∧
    stored.block = p.block ∧ stored.instruction = p.instruction ∧
    AList.lookup stored.virtualValues ⟨base + reg.id⟩ = some v ∧
    ∀ r, r ≠ reg.id →
      AList.lookup stored.virtualValues ⟨base + r⟩ = AList.lookup p.virtualValues ⟨base + r⟩ := by
  unfold Memory.writeValue at hW
  by_cases hCond : (!p.live.contains (birReg base reg).logical.root ||
      !(birMemoryView (language := sliceLanguage slice) bir).mayInitialize p (birReg base reg)) = true
  · simp only [hCond, ↓reduceIte] at hW; cases hW
  · simp only [hCond, bind, Option.bind] at hW
    cases ha : (birMemoryView (language := sliceLanguage slice) bir).authorizeWrite p (birReg base reg) with
    | none => rw [ha] at hW; simp only [Bool.false_eq_true, ↓reduceIte] at hW; cases hW
    | some owners =>
      rw [ha] at hW
      dsimp only [birReg, Memory.replaceLocal, List.isEmpty_nil, pure] at hW
      simp only [Bool.false_eq_true, ↓reduceIte] at hW
      cases Option.some.inj hW
      have hMoved : (Memory.markWritten p (birReg base reg) owners).movedFields = [] := by
        simp [Memory.markWritten, hPure.movedFields]
      refine ⟨⟨hPure.frame, hPure.callers, hPure.exhausted, hPure.effects, hPure.materialized,
        hPure.zeroValues, hPure.addressIds, hMoved, hPure.bytes⟩, rfl, rfl, ?_, ?_⟩
      · simp [AList.lookup]
      · intro r hr
        have hNe : (⟨base + reg.id⟩ : PlaceId) ≠ ⟨base + r⟩ := by
          intro hEq; exact hr (Nat.add_left_cancel (PlaceId.mk.inj hEq)).symm
        simp only [AList.lookup, hNe, ↓reduceIte]
        exact alist_lookup_erase_ne hNe p.virtualValues

private theorem writeParams_regs_inv {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    {locals : LocalTypes (sliceLanguage slice).operands.Ty}
    {graph : BIRGraph (sliceLoweringLanguage slice)}
    (hLk : AList.lookup bir.functions bir.entry = some (locals, graph))
    {regs : List (Sigma (SSAValue (sliceLanguage slice).operands.Ty))}
    {values : List (sliceLanguage slice).operands.Val}
    {p stored : MemoryRegisters (sliceLanguage slice) profile}
    (hPure : FramePure bir p)
    (hNodup : (regs.map fun q => q.2.id).Nodup)
    (hWP : (birMemoryView (language := sliceLanguage slice) bir).WriteParams
      (regs.map fun q => ⟨q.1, birReg (birBase locals graph) q.2⟩) values p stored false) :
    FramePure bir stored ∧
    stored.block = p.block ∧ stored.instruction = p.instruction ∧
    values.length = regs.length ∧
    (∀ (k : Nat) (q : Sigma (SSAValue (sliceLanguage slice).operands.Ty))
      (v : (sliceLanguage slice).operands.Val), regs[k]? = some q → values[k]? = some v →
      AList.lookup stored.virtualValues ⟨birBase locals graph + q.2.id⟩ = some v) ∧
    (∀ r, (∀ q ∈ regs, q.2.id ≠ r) →
      AList.lookup stored.virtualValues ⟨birBase locals graph + r⟩ =
        AList.lookup p.virtualValues ⟨birBase locals graph + r⟩) := by
  induction regs generalizing values p with
  | nil =>
    cases values with
    | nil =>
      cases hWP
      exact ⟨hPure, rfl, rfl, rfl, (fun _ _ _ h _ => by simp at h), fun _ _ => rfl⟩
    | cons _ _ => cases hWP
  | cons hd tl ih =>
    cases values with
    | nil => cases hWP
    | cons v vs =>
      rcases hd with ⟨ty, reg⟩
      simp only [List.map_cons] at hWP hNodup
      rw [List.nodup_cons] at hNodup
      cases hWP with
      | @cons _ _ _ _ _ _ started mid _ _ _ hStart hWrites hRest =>
        have hPureStarted : FramePure bir started ∧
            started.block = p.block ∧ started.instruction = p.instruction ∧
            started.virtualValues = p.virtualValues := by
          split at hStart
          · subst hStart; exact ⟨hPure, rfl, rfl, rfl⟩
          · rcases hStart.2 with ⟨_, rfl⟩ | ⟨hMat, _⟩
            · exact ⟨⟨hPure.frame, hPure.callers, hPure.exhausted, hPure.effects, hPure.materialized,
                hPure.zeroValues, hPure.addressIds, hPure.movedFields, hPure.bytes⟩, rfl, rfl, rfl⟩
            · rw [hPure.frame] at hMat
              exact absurd hMat (birMemoryView_classAt_reg_ne_mat (language := sliceLanguage slice) hLk reg.id)
        obtain ⟨hPureMid, hBlkMid, hInstMid, hLkSelf, hLkOther⟩ :=
          writeValue_reg_inv hPureStarted.1 hWrites.2.2.1
        obtain ⟨hPureEnd, hBlkEnd, hInstEnd, hLenTl, hLkTl, hOtherTl⟩ :=
          ih hPureMid hNodup.2 hRest
        refine ⟨hPureEnd, hBlkEnd.trans (hBlkMid.trans hPureStarted.2.1),
          hInstEnd.trans (hInstMid.trans hPureStarted.2.2.1), by simp [hLenTl], ?_, ?_⟩
        · intro k q val hq hv
          cases k with
          | zero =>
            cases Option.some.inj hq
            cases Option.some.inj hv
            have hNotInTl : ∀ q' ∈ tl, q'.2.id ≠ reg.id := fun q' hq' hEq =>
              hNodup.1 (List.mem_map.mpr ⟨q', hq', hEq⟩)
            rw [hOtherTl reg.id hNotInTl, hLkSelf]
          | succ k' =>
            exact hLkTl k' q val hq hv
        · intro r hr
          have hNe : r ≠ reg.id := (hr ⟨ty, reg⟩ List.mem_cons_self).symm
          rw [hOtherTl r (fun q hq => hr q (List.mem_cons_of_mem _ hq)),
            hLkOther r hNe, hPureStarted.2.2.2]

/-- Convenience specialization for 1-register writes (`.read`, `.arithmetic`). -/
private theorem writeParams_one_reg_inv {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    {locals : LocalTypes (sliceLanguage slice).operands.Ty}
    {graph : BIRGraph (sliceLoweringLanguage slice)}
    (hLk : AList.lookup bir.functions bir.entry = some (locals, graph))
    {ty : (sliceLanguage slice).operands.Ty}
    {dst : SSAValue (sliceLanguage slice).operands.Ty ty}
    {v : (sliceLanguage slice).operands.Val}
    {p stored : MemoryRegisters (sliceLanguage slice) profile}
    (hPure : FramePure bir p)
    (hWP : (birMemoryView (language := sliceLanguage slice) bir).WriteParams
      [⟨ty, birReg (birBase locals graph) dst⟩] [v] p stored false) :
    FramePure bir stored ∧
    stored.block = p.block ∧ stored.instruction = p.instruction ∧
    AList.lookup stored.virtualValues ⟨birBase locals graph + dst.id⟩ = some v ∧
    ∀ r, r ≠ dst.id →
      AList.lookup stored.virtualValues ⟨birBase locals graph + r⟩ =
        AList.lookup p.virtualValues ⟨birBase locals graph + r⟩ := by
  have h := writeParams_regs_inv hLk (regs := [⟨ty, dst⟩]) (values := [v]) hPure
    (List.nodup_cons.mpr ⟨by simp, List.nodup_nil⟩) hWP
  refine ⟨h.1, h.2.1, h.2.2.1, h.2.2.2.2.1 0 ⟨ty, dst⟩ v rfl rfl, fun r hr => ?_⟩
  exact h.2.2.2.2.2 r fun q hq => by
    cases List.mem_singleton.mp hq
    exact hr.symm

/-- Convenience specialization for 2-register writes (`.addWithOverflow`). -/
private theorem writeParams_two_regs_inv {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    {locals : LocalTypes (sliceLanguage slice).operands.Ty}
    {graph : BIRGraph (sliceLoweringLanguage slice)}
    (hLk : AList.lookup bir.functions bir.entry = some (locals, graph))
    {ty₁ ty₂ : (sliceLanguage slice).operands.Ty}
    {dst : SSAValue (sliceLanguage slice).operands.Ty ty₁}
    {flag : SSAValue (sliceLanguage slice).operands.Ty ty₂}
    {v₁ v₂ : (sliceLanguage slice).operands.Val}
    {p stored : MemoryRegisters (sliceLanguage slice) profile}
    (hPure : FramePure bir p) (hNe : dst.id ≠ flag.id)
    (hWP : (birMemoryView (language := sliceLanguage slice) bir).WriteParams
      [⟨ty₁, birReg (birBase locals graph) dst⟩, ⟨ty₂, birReg (birBase locals graph) flag⟩]
      [v₁, v₂] p stored false) :
    FramePure bir stored ∧
    stored.block = p.block ∧ stored.instruction = p.instruction ∧
    AList.lookup stored.virtualValues ⟨birBase locals graph + dst.id⟩ = some v₁ ∧
    AList.lookup stored.virtualValues ⟨birBase locals graph + flag.id⟩ = some v₂ ∧
    ∀ r, r ≠ dst.id → r ≠ flag.id →
      AList.lookup stored.virtualValues ⟨birBase locals graph + r⟩ =
        AList.lookup p.virtualValues ⟨birBase locals graph + r⟩ := by
  have hNd : ([⟨ty₁, dst⟩, ⟨ty₂, flag⟩].map
      fun (q : Sigma (SSAValue (sliceLanguage slice).operands.Ty)) => q.2.id).Nodup := by
    simp [hNe]
  have h := writeParams_regs_inv hLk (regs := [⟨ty₁, dst⟩, ⟨ty₂, flag⟩]) (values := [v₁, v₂])
    hPure hNd hWP
  refine ⟨h.1, h.2.1, h.2.2.1,
    h.2.2.2.2.1 0 ⟨ty₁, dst⟩ v₁ rfl rfl,
    h.2.2.2.2.1 1 ⟨ty₂, flag⟩ v₂ rfl rfl,
    fun r hr1 hr2 => ?_⟩
  exact h.2.2.2.2.2 r fun q hq => by
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hq
    rcases hq with rfl | rfl
    · exact hr1.symm
    · exact hr2.symm

/-! ### Full `BIR.Execution` frame-purity theorem (`bir_reachable_stays_in_entry`) -/

private theorem nodup_of_mem_flatMap {α β : Type} {xs : List α} {f : α → List β}
    (h : (xs.flatMap f).Nodup) {x : α} (hx : x ∈ xs) : (f x).Nodup := by
  induction xs with
  | nil => cases hx
  | cons a rest ih =>
    rw [List.flatMap_cons, List.nodup_append] at h
    cases hx with
    | head => exact h.1
    | tail _ htl => exact ih h.2.1 htl

private theorem applyOperation_pure_callers_zero {slice : Slice} {profile : Profile}
    {p : MemoryRegisters (sliceLanguage slice) profile}
    (hCall : p.callers = []) (hZero : p.zeroValues = []) :
    Memory.applyOperation (Memory.prepareAddresses p []) false
      (Memory.operationStore (Memory.prepareAddresses p []) false) = p := by
  cases p with
  | mk frame bytes callers effects ownership nextAddr exh =>
    cases frame with
    | mk f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 =>
      subst hCall; subst hZero
      rfl

private theorem applyOperation_pure_eq {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    {p : MemoryRegisters (sliceLanguage slice) profile}
    (hPure : FramePure bir p) :
    Memory.applyOperation (Memory.prepareAddresses p []) false
      (Memory.operationStore (Memory.prepareAddresses p []) false) = p :=
  applyOperation_pure_callers_zero hPure.callers hPure.zeroValues

private theorem readValue_preserves_pure_base {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {memory : Memory lang profile} {ty : language.operands.Ty}
    {read : MemoryRead language.operands.Ty lang.literal ty}
    {state afterRead : MemoryRegisters language profile} {v : language.operands.Val}
    (hZero : state.zeroValues = [])
    (hRead : memory.readValue state read = some (v, afterRead)) :
    afterRead.callers = state.callers ∧
    afterRead.zeroValues = [] ∧
    afterRead.resourceExhausted = state.resourceExhausted := by
  cases read with
  | literal lit =>
    simp only [Memory.readValue] at hRead
    cases hl : language.literal ty lit.encoding with
    | none => simp [hl] at hRead
    | some val =>
      simp only [hl, bind, Option.bind, pure, Option.some.injEq, Prod.mk.injEq] at hRead
      obtain ⟨_, rfl⟩ := hRead
      exact ⟨rfl, hZero, rfl⟩
  | copy place =>
    obtain ⟨_, _, _, rfl⟩ := (Memory.copyRead_iff memory state place v afterRead).mp hRead
    exact ⟨rfl, hZero, rfl⟩
  | move place =>
    simp only [Memory.readValue, bind, Option.bind] at hRead
    cases hMove : Own.move (memory.ownerView state place.logical.root ty .owned).1
        (memory.ownerView state place.logical.root ty .owned).2 with
    | none => simp [hMove] at hRead
    | some owners =>
      rw [hMove] at hRead; dsimp only at hRead
      cases hLoad : memory.loadValue state place with
      | none => rw [hLoad] at hRead; cases hRead
      | some val =>
        rw [hLoad] at hRead; dsimp only at hRead
        split at hRead
        · cases Option.some.inj hRead; exact ⟨rfl, hZero, rfl⟩
        · cases Option.some.inj hRead
          exact ⟨rfl, by simp [hZero, AList.erase], rfl⟩

private theorem readArgs_preserves_pure_base {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {memory : Memory lang profile} {types : List language.operands.Ty}
    {args : Args (MemoryRead language.operands.Ty lang.literal) types}
    {state afterRead : MemoryRegisters language profile} {vs : List language.operands.Val}
    (hZero : state.zeroValues = [])
    (hRead : memory.readArgs args state = some (vs, afterRead)) :
    afterRead.callers = state.callers ∧
    afterRead.zeroValues = [] ∧
    afterRead.resourceExhausted = state.resourceExhausted := by
  induction args generalizing state vs with
  | nil =>
    cases Option.some.inj hRead
    exact ⟨rfl, hZero, rfl⟩
  | cons read rest ih =>
    simp only [Memory.readArgs, bind, Option.bind] at hRead
    cases h0 : memory.readValue state read with
    | none => simp [h0] at hRead
    | some pr0 =>
      rcases pr0 with ⟨v0, mid⟩
      simp only [h0] at hRead
      cases h1 : memory.readArgs rest mid with
      | none => simp [h1] at hRead
      | some pr1 =>
        rcases pr1 with ⟨vs1, fin⟩
        simp only [h1, pure, Option.some.injEq, Prod.mk.injEq] at hRead
        obtain ⟨_, rfl⟩ := hRead
        obtain ⟨hC0, hZ0, hE0⟩ := readValue_preserves_pure_base hZero h0
        obtain ⟨hC1, hZ1, hE1⟩ := ih hZ0 h1
        exact ⟨hC1.trans hC0, hZ1, hE1.trans hE0⟩

private theorem readOperands_preserves_pure_base {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {memory : Memory lang profile}
    {args : List (Sigma (MemoryRead language.operands.Ty lang.literal))}
    {state afterRead : MemoryRegisters language profile} {vs : List language.operands.Val}
    (hZero : state.zeroValues = [])
    (hRead : memory.readOperands args state = some (vs, afterRead)) :
    afterRead.callers = state.callers ∧
    afterRead.zeroValues = [] ∧
    afterRead.resourceExhausted = state.resourceExhausted := by
  induction args generalizing state vs with
  | nil =>
    cases Option.some.inj hRead
    exact ⟨rfl, hZero, rfl⟩
  | cons hd tl ih =>
    rcases hd with ⟨ty, read⟩
    simp only [Memory.readOperands, bind, Option.bind] at hRead
    cases h0 : memory.readValue state read with
    | none => simp [h0] at hRead
    | some pr0 =>
      rcases pr0 with ⟨v0, mid⟩
      simp only [h0] at hRead
      cases h1 : memory.readOperands tl mid with
      | none => simp [h1] at hRead
      | some pr1 =>
        rcases pr1 with ⟨vs1, fin⟩
        simp only [h1, pure, Option.some.injEq, Prod.mk.injEq] at hRead
        obtain ⟨_, rfl⟩ := hRead
        obtain ⟨hC0, hZ0, hE0⟩ := readValue_preserves_pure_base hZero h0
        obtain ⟨hC1, hZ1, hE1⟩ := ih hZ0 h1
        exact ⟨hC1.trans hC0, hZ1, hE1.trans hE0⟩

private theorem execBIRStep_sound {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    {state next : MemoryRegisters (sliceLanguage slice) profile}
    (hPure : FramePure bir state)
    (hExec : execBIRStep bir state = some next) :
    (BIR.Execution (language := sliceLanguage slice)).step bir state next := by
  let sLang := sliceLanguage slice
  let mem := birMemoryView (language := sLang) bir
  unfold execBIRStep at hExec
  dsimp only [bind, Option.bind] at hExec
  change (if state.resourceExhausted || (birHostedResult mem state).isSome then none else _) = some next at hExec
  rw [hPure.exhausted] at hExec
  simp only [Bool.false_or] at hExec
  cases hRes : birHostedResult mem state with
  | some _ => simp [hRes] at hExec
  | none =>
    simp only [hRes, Option.isSome_none, Bool.false_eq_true, ↓reduceIte] at hExec
    have hHostedNone : BIR.hostedResult (language := sLang) bir state = none := by
      rw [birHostedResult_eq]; exact hRes
    have hTracePref : Concrete.Contracts.TracePrefix
        ((BIR.Execution (language := sLang)).trace bir state)
        ((BIR.Execution (language := sLang)).trace bir next) := by
      have hTrNil : (BIR.Execution (language := sLang)).trace bir state = [] := by
        change (state.effects.map ProcessEvent.effect ++
          ((BIR.hostedResult (language := sLang) bir state).map
            fun o => o.stderr.data.toList.map ProcessEvent.stderr).getD []) = []
        rw [hPure.effects, hHostedNone]; rfl
      rw [hTrNil]
      exact ⟨_, rfl⟩
    refine ⟨hHostedNone, ?_, hTracePref⟩
    rw [← birMemoryView_eq bir]
    change mem.Step state next true
    unfold Memory.Step
    rw [hPure.exhausted]
    simp only [Bool.false_eq_true, ↓reduceIte]
    cases hBlk : mem.blockAt state with
    | none => rw [hBlk] at hExec; cases hExec
    | some block =>
      rw [hBlk] at hExec; dsimp only at hExec
      cases hBody : block.body[state.instruction]? with
      | some op =>
        rw [hBody] at hExec; dsimp only at hExec
        cases hInst : op.instruction with
        | bind _ _ | assign _ _ | beginLifetime _ | endLifetime _ | destructure _ _ | call _ _ _ _ =>
          rw [hInst] at hExec; cases hExec
        | @op opcode types ty dst _ args checks addresses =>
          rw [hInst] at hExec; dsimp only at hExec
          cases hCA : (checks.isEmpty && addresses.isEmpty) with
          | false => simp [hCA] at hExec
          | true =>
            simp only [hCA, Bool.not_true, Bool.false_eq_true, ↓reduceIte] at hExec
            have hChecksNil : checks = [] := by
              cases hc : checks with
              | nil => rfl
              | cons _ _ => simp [hc] at hCA
            have hAddrsNil : addresses = [] := by
              cases ha : addresses with
              | nil => rfl
              | cons _ _ => simp [ha] at hCA
            subst hChecksNil; subst hAddrsNil
            cases hRA : (birMemoryView (language := sLang) bir).readArgs args state with
            | none => rw [hRA] at hExec; cases hExec
            | some pr =>
              rcases pr with ⟨values, afterReads⟩
              rw [hRA] at hExec; dsimp only at hExec
              cases hEO : execEarlyOp (birMemoryView (language := sLang) bir).declarations opcode types ty values with
              | none => rw [hEO] at hExec; cases hExec
              | some result =>
                rw [hEO] at hExec; dsimp only at hExec
                cases hWP : execWriteParams (birMemoryView (language := sLang) bir) [⟨ty, dst⟩] [result] afterReads with
                | none => rw [hWP] at hExec; cases hExec
                | some stored =>
                  rw [hWP] at hExec
                  cases Option.some.inj hExec
                  obtain ⟨hC, hZ, hE⟩ := readArgs_preserves_pure_base hPure.zeroValues hRA
                  obtain ⟨hWPOk, hExhS⟩ := writeParams_of_execWriteParams hWP
                  have hNextExh : stored.advance.resourceExhausted = false := by
                    change stored.resourceExhausted = false
                    rw [hExhS, hE, hPure.exhausted]
                  simp only [hNextExh, Bool.false_eq_true, ↓reduceIte]
                  refine ⟨block, hBlk, ?_⟩
                  rw [hBody]; dsimp only; rw [hInst]
                  refine ⟨values, afterReads, [], result, [], Memory.operationStore (Memory.prepareAddresses afterReads []) false,
                    stored, hRA, rfl, rfl, execEarlyOp_sound hEO, rfl, ?_, ?_⟩
                  · simp only [Bool.false_or, ↓reduceIte]
                    rw [applyOperation_pure_callers_zero (hC.trans hPure.callers) hZ]
                    exact hWPOk
                  · simp [MemoryRegisters.advance]
        | addWithOverflow t dst flag lhs rhs =>
          rw [hInst] at hExec; dsimp only at hExec
          cases hDisj : (dst.logical.root == flag.logical.root) with
          | true => simp [hDisj] at hExec
          | false =>
            simp only [hDisj, Bool.false_eq_true, ↓reduceIte] at hExec
            have hRootNe : dst.logical.root ≠ flag.logical.root := by
              intro hEq; rw [hEq, beq_self_eq_true] at hDisj; cases hDisj
            cases hR0 : (birMemoryView (language := sLang) bir).readValue state lhs with
            | none => rw [hR0] at hExec; cases hExec
            | some pr0 =>
              rcases pr0 with ⟨lv, afterLhs⟩
              rw [hR0] at hExec; dsimp only at hExec
              cases hR1 : (birMemoryView (language := sLang) bir).readValue afterLhs rhs with
              | none => rw [hR1] at hExec; cases hExec
              | some pr1 =>
                rcases pr1 with ⟨rv, afterReads⟩
                rw [hR1] at hExec; dsimp only at hExec
                cases lv with
                | other _ => cases hExec
                | scalar sv0 =>
                  cases sv0 with
                  | bool _ | char _ | unit => cases hExec
                  | int t1 x =>
                    cases rv with
                    | other _ => cases hExec
                    | scalar sv1 =>
                      cases sv1 with
                      | bool _ | char _ | unit => cases hExec
                      | int t2 y =>
                        cases hT : (t1 == t && t2 == t) with
                        | false => simp [hT] at hExec
                        | true =>
                          simp only [hT, ↓reduceIte] at hExec
                          obtain ⟨ht1, ht2⟩ := Bool.and_eq_true_iff.mp hT
                          have ht1Eq : t1 = t := eq_of_beq ht1
                          have ht2Eq : t2 = t := eq_of_beq ht2
                          cases ht1Eq; cases ht2Eq
                          cases hWP : execWriteParams (birMemoryView (language := sLang) bir) [⟨_, dst⟩, ⟨_, flag⟩]
                              [CoreValue.scalar (.int t (wrapping t .add x y)),
                               CoreValue.scalar (.bool (decide (checked t .add x y = .error .overflow)))]
                              afterReads with
                          | none => rw [hWP] at hExec; cases hExec
                          | some stored =>
                            rw [hWP] at hExec
                            cases Option.some.inj hExec
                            obtain ⟨_, hZ0, hE0⟩ := readValue_preserves_pure_base hPure.zeroValues hR0
                            obtain ⟨_, _, hE1⟩ := readValue_preserves_pure_base hZ0 hR1
                            obtain ⟨hWPOk, hExhS⟩ := writeParams_of_execWriteParams hWP
                            have hNextExh : stored.advance.resourceExhausted = false := by
                              change stored.resourceExhausted = false
                              rw [hExhS, hE1, hE0, hPure.exhausted]
                            simp only [hNextExh, Bool.false_eq_true, ↓reduceIte]
                            refine ⟨block, hBlk, ?_⟩
                            rw [hBody]; dsimp only; rw [hInst]
                            exact ⟨rfl, rfl, Or.inl hRootNe, _, afterLhs, _, afterReads, x, y, stored,
                              hR0, hR1, rfl, rfl, hWPOk, rfl⟩
      | none =>
        rw [hBody] at hExec; dsimp only at hExec
        cases hPc : (state.instruction != block.body.length) with
        | true => simp [hPc] at hExec
        | false =>
          simp only [hPc, Bool.false_eq_true, ↓reduceIte] at hExec
          have hPcEq : state.instruction = block.body.length := by
            have : (state.instruction == block.body.length) = true := by
              cases hb : (state.instruction == block.body.length)
              · simp [bne, hb] at hPc
              · rfl
            exact eq_of_beq this
          cases hEnd : block.ending with
          | ret _ | trap _ _ | invalid => rw [hEnd] at hExec; cases hExec
          | jump edge =>
            rw [hEnd] at hExec; dsimp only at hExec
            cases hRO : (birMemoryView (language := sLang) bir).readOperands edge.arguments state with
            | none => rw [hRO] at hExec; cases hExec
            | some pr =>
              rcases pr with ⟨values, afterRead⟩
              rw [hRO] at hExec; dsimp only at hExec
              split at hExec
              · cases hExec
              · rename_i tgtBlk hTgt
                letI := sLang.typeDecidableEq
                by_cases hTyEq : edge.arguments.map Sigma.fst = tgtBlk.params.map Sigma.fst
                · simp only [hTyEq, decide_true, Bool.not_true, Bool.false_eq_true, ↓reduceIte] at hExec
                  obtain ⟨_, _, hE0⟩ := readOperands_preserves_pure_base hPure.zeroValues hRO
                  obtain ⟨hWPOk, hExhN⟩ := writeParams_of_execWriteParams hExec
                  have hNextExh : next.resourceExhausted = false := by
                    rw [hExhN]; exact hE0.trans hPure.exhausted
                  simp only [hNextExh, Bool.false_eq_true, ↓reduceIte]
                  refine ⟨block, hBlk, ?_⟩
                  rw [hBody]; dsimp only; rw [hEnd]
                  exact ⟨hPcEq, values, afterRead, tgtBlk, hRO, hTgt, hTyEq, hWPOk⟩
                · simp only [hTyEq, decide_false, Bool.not_false, ↓reduceIte] at hExec; cases hExec
          | branch test yes no =>
            rw [hEnd] at hExec; dsimp only at hExec
            cases hRT : (birMemoryView (language := sliceLanguage slice) bir).readValue state test with
            | none => rw [hRT] at hExec; cases hExec
            | some pr0 =>
              rcases pr0 with ⟨val, afterRead⟩
              rw [hRT] at hExec; dsimp only at hExec
              cases hTest : (sliceLanguage slice).operands.test val with
              | none => rw [hTest] at hExec; cases hExec
              | some flag =>
                rw [hTest] at hExec; dsimp only at hExec
                cases hRO : (birMemoryView (language := sliceLanguage slice) bir).readOperands (if flag then yes else no).arguments afterRead with
                | none => rw [hRO] at hExec; cases hExec
                | some pr1 =>
                  rcases pr1 with ⟨values, afterArgs⟩
                  rw [hRO] at hExec; dsimp only at hExec
                  cases hTgt : (birMemoryView (language := sliceLanguage slice) bir).blockAt {afterRead with block := (if flag then yes else no).target} with
                  | none => rw [hTgt] at hExec; cases hExec
                  | some tgtBlk =>
                    rw [hTgt] at hExec; dsimp only at hExec
                    letI := sLang.typeDecidableEq
                    by_cases hTyEq : (if flag then yes else no).arguments.map Sigma.fst = tgtBlk.params.map Sigma.fst
                    · simp only [hTyEq, decide_true, Bool.not_true, Bool.false_eq_true, ↓reduceIte] at hExec
                      obtain ⟨_, hZ0, hE0⟩ := readValue_preserves_pure_base hPure.zeroValues hRT
                      obtain ⟨_, _, hE1⟩ := readOperands_preserves_pure_base hZ0 hRO
                      obtain ⟨hWPOk, hExhN⟩ := writeParams_of_execWriteParams hExec
                      have hNextExh : next.resourceExhausted = false := by
                        rw [hExhN]; exact hE1.trans (hE0.trans hPure.exhausted)
                      simp only [hNextExh, Bool.false_eq_true, ↓reduceIte]
                      refine ⟨block, hBlk, ?_⟩
                      rw [hBody]; dsimp only; rw [hEnd]
                      exact ⟨hPcEq, val, afterRead, flag, hRT, hTest,
                        values, afterArgs, tgtBlk, hRO, hTgt, hTyEq, hWPOk⟩
                    · simp only [hTyEq, decide_false, Bool.not_false, ↓reduceIte] at hExec; cases hExec

private theorem bir_step_preserves_pure_and_reach {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    (hBackend : profile.backend = .lcb)
    (hWF : BIR.WF (language := sliceLanguage slice) bir)
    (hSup : firstUnsupported (language := sliceLanguage slice) bir = none)
    {locals : LocalTypes (sliceLanguage slice).operands.Ty} {graph : BIRGraph (sliceLoweringLanguage slice)}
    (hEntry : AList.lookup bir.functions bir.entry = some (locals, graph))
    {p p' : MemoryRegisters (sliceLanguage slice) profile}
    (hPure : FramePure bir p)
    (hReach : p.block ∈ BIR.reachableBlocks graph)
    (hStep : (BIR.Execution (language := sliceLanguage slice)).step bir p p') :
    FramePure bir p' ∧ p'.block ∈ BIR.reachableBlocks graph := by
  let sLang := sliceLanguage slice
  let sLow := sliceLoweringLanguage slice
  have hMemEq := birMemoryView_eq bir
  change _ ∧ (bir.memoryView (language := sLang)).Step p p' true ∧ _ at hStep
  rw [← hMemEq] at hStep
  have hGraphAt := birMemoryView_graphAt hEntry
  -- First rule out `p'.resourceExhausted = true` and extract the block at `p`.
  have hStepBody := hStep.2.1
  unfold Memory.Step at hStepBody
  rw [hPure.exhausted] at hStepBody
  simp only [Bool.false_eq_true, ↓reduceIte] at hStepBody
  have hNotExh : p'.resourceExhausted = false := by
    cases hEx : p'.resourceExhausted with
    | false => rfl
    | true =>
      exfalso
      simp only [hEx, ↓reduceIte] at hStepBody
      obtain ⟨_, partialState, block, hBlkAt, hOutcome⟩ := hStepBody
      unfold Memory.blockAt at hBlkAt
      rw [hPure.frame, hGraphAt] at hBlkAt
      dsimp only [bind, Option.bind, birMemoryGraph] at hBlkAt
      have hFind : ∃ blk ∈ graph.blocks.toList, blk.id = p.block ∧
          block = ⟨blk.id, blk.params.map (fun q => ⟨q.1, birReg (birBase locals graph) q.2⟩),
            blk.body.map (fun op => ⟨op.origin, birInst (birBase locals graph) op.instruction⟩),
            birTerm bir (birBase locals graph) blk.ending, blk.boundaryOrigins⟩ := by
        rw [← Array.find?_toList, Array.toList_map] at hBlkAt
        have hMem := List.mem_of_find?_eq_some hBlkAt
        have hId : block.id = p.block := by simpa using List.find?_some hBlkAt
        obtain ⟨blk, hBlkMem, hEq⟩ := List.mem_map.mp hMem
        exact ⟨blk, hBlkMem, (congrArg Block.id hEq).trans hId, hEq.symm⟩
      obtain ⟨blk, hBlkMem, hBlkId, rfl⟩ := hFind
      cases hBodyAt : (blk.body.map (fun op =>
          (⟨op.origin, birInst (birBase locals graph) op.instruction⟩ : LocatedOp (MemoryInst sLow))))[p.instruction]? with
      | some mOp =>
        rw [hBodyAt] at hOutcome
        rw [List.getElem?_map] at hBodyAt
        cases hOpAt : blk.body[p.instruction]? with
        | none => simp [hOpAt] at hBodyAt
        | some op =>
          rw [hOpAt] at hBodyAt
          cases Option.some.inj hBodyAt
          dsimp only at hOutcome
          have hOpMem : op ∈ blk.body := List.mem_of_getElem? hOpAt
          rcases lcb_reachable_inst_classify hBackend hWF hSup hEntry hBlkMem (hBlkId ▸ hReach) hOpMem with
            ⟨ty, dst, fop, arg, hInstEq⟩ | ⟨mOp', wOp, t, dst, fop, lhs, rhs, _, _, hInstEq⟩ |
            ⟨t, dst, flag, lhs, rhs, _, _, hInstEq⟩
          · rw [hInstEq] at hOutcome
            obtain ⟨values, afterReads, _, _, _, _, _, hRead, _, _, hOpSem, _, hPO, _⟩ := hOutcome
            obtain ⟨_, _, hStoreEq, _⟩ := execEarlyOp_of_sliceOperation hOpSem
            simp only [birArgs, Memory.readArgs, bind, Option.bind] at hRead
            cases hR0 : (birMemoryView bir).readValue p (birRead (birBase locals graph) arg) with
            | none => simp [hR0] at hRead
            | some pr =>
              simp only [hR0, pure, Option.some.injEq, Prod.mk.injEq] at hRead
              obtain ⟨_, rfl⟩ := hRead
              have hPr := (readValue_virtual_inv hPure hR0).1
              simp only [List.map_nil, Bool.false_or, ↓reduceIte] at hStoreEq hPO
              rw [← hStoreEq, applyOperation_pure_eq hPr] at hPO
              have hFits := bir_paramSpaceFits_regs hPr.frame hEntry
                [⟨ty, birReg (birBase locals graph) dst⟩]
                (fun q hq => by cases List.mem_singleton.mp hq; exact ⟨dst.id, rfl⟩)
              exact hFits.not_exhausted hPO
          · rw [hInstEq] at hOutcome
            obtain ⟨values, afterReads, _, _, _, _, _, hRead, _, _, hOpSem, _, hPO, _⟩ := hOutcome
            obtain ⟨_, _, hStoreEq, _⟩ := execEarlyOp_of_sliceOperation hOpSem
            simp only [birArgs, Memory.readArgs, bind, Option.bind] at hRead
            cases hR0 : (birMemoryView bir).readValue p (birRead (birBase locals graph) lhs) with
            | none => simp [hR0] at hRead
            | some pr0 =>
              simp only [hR0] at hRead
              cases hR1 : (birMemoryView bir).readValue pr0.2 (birRead (birBase locals graph) rhs) with
              | none => simp [hR1] at hRead
              | some pr1 =>
                simp only [hR1, pure, Option.some.injEq, Prod.mk.injEq] at hRead
                obtain ⟨_, rfl⟩ := hRead
                have hPr0 := (readValue_virtual_inv hPure hR0).1
                have hPr1 := (readValue_virtual_inv hPr0 hR1).1
                simp only [List.map_nil, Bool.false_or, ↓reduceIte] at hStoreEq hPO
                rw [← hStoreEq, applyOperation_pure_eq hPr1] at hPO
                have hFits := bir_paramSpaceFits_regs hPr1.frame hEntry
                  [⟨.scalar (.int t), birReg (birBase locals graph) dst⟩]
                  (fun q hq => by cases List.mem_singleton.mp hq; exact ⟨dst.id, rfl⟩)
                exact hFits.not_exhausted hPO
          · rw [hInstEq] at hOutcome
            obtain ⟨_, _, _, _, afterLhs, _, afterReads, _, _, _, hR0, hR1, _, _, hPO, _⟩ := hOutcome
            have hPr0 := (readValue_virtual_inv hPure hR0).1
            have hPr1 := (readValue_virtual_inv hPr0 hR1).1
            have hFits := bir_paramSpaceFits_regs hPr1.frame hEntry
              [⟨_, birReg (birBase locals graph) dst⟩, ⟨_, birReg (birBase locals graph) flag⟩]
              (fun q hq => by
                simp only [List.mem_cons, List.not_mem_nil, or_false] at hq
                rcases hq with rfl | rfl
                · exact ⟨dst.id, rfl⟩
                · exact ⟨flag.id, rfl⟩)
            exact hFits.not_exhausted hPO
      | none =>
        rw [hBodyAt] at hOutcome
        have hCtrl := hOutcome.2
        have hTrNotExh : ∀ (edge : BIREdge sLow),
            ¬ (birMemoryView bir).Transfer ⟨edge.target, edge.arguments.map (birOpnd (birBase locals graph))⟩
                p partialState true := by
          intro edge hTr
          obtain ⟨values, afterRead, tgtBlk, hRd, hTgtAt, _, hPO⟩ := hTr
          have hPr := (readOperands_virtual_inv hPure hRd).1
          unfold Memory.blockAt at hTgtAt
          rw [hPure.frame, hGraphAt] at hTgtAt
          dsimp only [bind, Option.bind, birMemoryGraph] at hTgtAt
          rw [← Array.find?_toList, Array.toList_map] at hTgtAt
          obtain ⟨rawTgt, _, hTgtEq⟩ := List.mem_map.mp (List.mem_of_find?_eq_some hTgtAt)
          rw [← hTgtEq] at hPO
          have hFits := bir_paramSpaceFits_regs
            (state := {afterRead with block := edge.target, instruction := 0}) hPr.frame hEntry
            (rawTgt.params.map fun q => ⟨q.1, birReg (birBase locals graph) q.2⟩)
            (fun q hq => by
              obtain ⟨p', _, rfl⟩ := List.mem_map.mp hq
              exact ⟨p'.2.id, rfl⟩)
          exact hFits.not_exhausted hPO
        cases hEnd : blk.ending with
        | ret ov =>
          rw [hEnd] at hCtrl
          obtain ⟨_, _, _, _, _, hCallers, _⟩ := hCtrl
          rw [hPure.callers] at hCallers; cases hCallers
        | trap slot =>
          rw [hEnd] at hCtrl
          dsimp only [birTerm, Memory.ControlStep] at hCtrl
          cases hS : bir.sites[slot.index]? <;> rw [hS] at hCtrl <;> exact hCtrl
        | jump edge =>
          rw [hEnd] at hCtrl
          exact hTrNotExh edge hCtrl
        | branch test yes no =>
          rw [hEnd] at hCtrl
          obtain ⟨_, afterRead, flag, hRd, _, hTr⟩ := hCtrl
          have hPr := (readValue_virtual_inv hPure hRd).1
          cases flag with
          | true =>
            obtain ⟨values, afterArgs, tgtBlk, hRd2, hTgtAt, _, hPO⟩ := hTr
            have hPr2 := (readOperands_virtual_inv hPr hRd2).1
            unfold Memory.blockAt at hTgtAt
            rw [hPr.frame, hGraphAt] at hTgtAt
            dsimp only [bind, Option.bind, birMemoryGraph] at hTgtAt
            rw [← Array.find?_toList, Array.toList_map] at hTgtAt
            obtain ⟨rawTgt, _, hTgtEq⟩ := List.mem_map.mp (List.mem_of_find?_eq_some hTgtAt)
            rw [← hTgtEq] at hPO
            have hFits := bir_paramSpaceFits_regs
              (state := {afterArgs with block := yes.target, instruction := 0}) hPr2.frame hEntry
              (rawTgt.params.map fun q => ⟨q.1, birReg (birBase locals graph) q.2⟩)
              (fun q hq => by
                obtain ⟨p', _, rfl⟩ := List.mem_map.mp hq
                exact ⟨p'.2.id, rfl⟩)
            exact hFits.not_exhausted hPO
          | false =>
            obtain ⟨values, afterArgs, tgtBlk, hRd2, hTgtAt, _, hPO⟩ := hTr
            have hPr2 := (readOperands_virtual_inv hPr hRd2).1
            unfold Memory.blockAt at hTgtAt
            rw [hPr.frame, hGraphAt] at hTgtAt
            dsimp only [bind, Option.bind, birMemoryGraph] at hTgtAt
            rw [← Array.find?_toList, Array.toList_map] at hTgtAt
            obtain ⟨rawTgt, _, hTgtEq⟩ := List.mem_map.mp (List.mem_of_find?_eq_some hTgtAt)
            rw [← hTgtEq] at hPO
            have hFits := bir_paramSpaceFits_regs
              (state := {afterArgs with block := no.target, instruction := 0}) hPr2.frame hEntry
              (rawTgt.params.map fun q => ⟨q.1, birReg (birBase locals graph) q.2⟩)
              (fun q hq => by
                obtain ⟨p', _, rfl⟩ := List.mem_map.mp hq
                exact ⟨p'.2.id, rfl⟩)
            exact hFits.not_exhausted hPO
  simp only [hNotExh, Bool.false_eq_true, ↓reduceIte] at hStepBody
  obtain ⟨block, hBlkAt, hOutcome⟩ := hStepBody
  unfold Memory.blockAt at hBlkAt
  rw [hPure.frame, hGraphAt] at hBlkAt
  dsimp only [bind, Option.bind, birMemoryGraph] at hBlkAt
  rw [← Array.find?_toList, Array.toList_map] at hBlkAt
  have hBlkId : block.id = p.block := by simpa using List.find?_some hBlkAt
  obtain ⟨blk, hBlkMem, hBlkEq⟩ := List.mem_map.mp (List.mem_of_find?_eq_some hBlkAt)
  subst hBlkEq
  have hRawId : blk.id = p.block := hBlkId
  -- Obtain block-param nodup from `hWF.registerFormation`
  let normGraphCfg : NormalizedCFG sLow :=
    ⟨graph.entry, graph.blocks.toList.map fun b =>
      ⟨b.id, b.params, b.body, RegTerminator.ofBIR bir.sites.size b.ending, b.boundaryOrigins⟩⟩
  have hFnMem : (bir.entry, locals, graph) ∈ bir.functions := alist_lookup_mem hEntry
  have hNormMem : (bir.entry, locals, normGraphCfg) ∈ (RegisterIR.bir bir).normalize.functions :=
    List.mem_map.mpr ⟨(bir.entry, locals, graph), hFnMem, rfl⟩
  obtain ⟨_, _, _, hDefsNd, _, _⟩ :=
    (hWF.registerFormation.2.2.2.2.2.2.2.2.2.2 _ hNormMem).2.2.2
  have hParamsNd : ∀ b ∈ graph.blocks.toList, (b.params.map fun q => q.2.id).Nodup := by
    intro b hb
    have hnb : (⟨b.id, b.params, b.body, RegTerminator.ofBIR bir.sites.size b.ending,
        b.boundaryOrigins⟩ : Block _ _ (RegTerminator sLow)) ∈ normGraphCfg.blocks :=
      List.mem_map.mpr ⟨b, hb, rfl⟩
    unfold functionDefTable at hDefsNd
    rw [List.map_flatMap] at hDefsNd
    have hOne := nodup_of_mem_flatMap hDefsNd hnb
    rw [List.map_append, List.map_map] at hOne
    exact (List.nodup_append.mp hOne).1
  cases hBodyAt : (blk.body.map (fun op =>
      (⟨op.origin, birInst (birBase locals graph) op.instruction⟩ : LocatedOp (MemoryInst sLow))))[p.instruction]? with
  | some mOp =>
    rw [hBodyAt] at hOutcome
    rw [List.getElem?_map] at hBodyAt
    cases hOpAt : blk.body[p.instruction]? with
    | none => simp [hOpAt] at hBodyAt
    | some op =>
      rw [hOpAt] at hBodyAt
      cases Option.some.inj hBodyAt
      dsimp only at hOutcome
      have hOpMem : op ∈ blk.body := List.mem_of_getElem? hOpAt
      rcases lcb_reachable_inst_classify hBackend hWF hSup hEntry hBlkMem (hRawId ▸ hReach) hOpMem with
        ⟨ty, dst, fop, arg, hInstEq⟩ | ⟨mOp', wOp, t, dst, fop, lhs, rhs, _, _, hInstEq⟩ |
        ⟨t, dst, flag, lhs, rhs, _, hNe, hInstEq⟩
      · rw [hInstEq] at hOutcome
        obtain ⟨values, afterReads, _, result, effs, changed, stored, hRead, _, _, hOpSem, _, hPO, rfl⟩ := hOutcome
        obtain ⟨_, hEffNil, hStoreEq, _⟩ := execEarlyOp_of_sliceOperation hOpSem
        simp only [birArgs, Memory.readArgs, bind, Option.bind] at hRead
        cases hR0 : (birMemoryView bir).readValue p (birRead (birBase locals graph) arg) with
        | none => simp [hR0] at hRead
        | some pr =>
          simp only [hR0, pure, Option.some.injEq, Prod.mk.injEq] at hRead
          obtain ⟨_, rfl⟩ := hRead
          have hPr := readValue_virtual_inv hPure hR0
          simp only [List.map_nil, Bool.false_or, ↓reduceIte] at hStoreEq hPO
          rw [← hStoreEq, applyOperation_pure_eq hPr.1] at hPO
          obtain ⟨hPureS, hBlkS, _, _, _⟩ := writeParams_one_reg_inv hEntry hPr.1 hPO
          subst hEffNil
          refine ⟨⟨hPureS.frame, hPureS.callers, hPureS.exhausted, by simp [hPureS.effects],
            hPureS.materialized, hPureS.zeroValues, hPureS.addressIds, hPureS.movedFields,
            hPureS.bytes⟩, ?_⟩
          simpa [MemoryRegisters.advance, hBlkS, hPr.2.1] using hReach
      · rw [hInstEq] at hOutcome
        obtain ⟨values, afterReads, _, result, effs, changed, stored, hRead, _, _, hOpSem, _, hPO, rfl⟩ := hOutcome
        obtain ⟨_, hEffNil, hStoreEq, _⟩ := execEarlyOp_of_sliceOperation hOpSem
        simp only [birArgs, Memory.readArgs, bind, Option.bind] at hRead
        cases hR0 : (birMemoryView bir).readValue p (birRead (birBase locals graph) lhs) with
        | none => simp [hR0] at hRead
        | some pr0 =>
          simp only [hR0] at hRead
          cases hR1 : (birMemoryView bir).readValue pr0.2 (birRead (birBase locals graph) rhs) with
          | none => simp [hR1] at hRead
          | some pr1 =>
            simp only [hR1, pure, Option.some.injEq, Prod.mk.injEq] at hRead
            obtain ⟨_, rfl⟩ := hRead
            have hPr0 := readValue_virtual_inv hPure hR0
            have hPr1 := readValue_virtual_inv hPr0.1 hR1
            simp only [List.map_nil, Bool.false_or, ↓reduceIte] at hStoreEq hPO
            rw [← hStoreEq, applyOperation_pure_eq hPr1.1] at hPO
            obtain ⟨hPureS, hBlkS, _, _, _⟩ := writeParams_one_reg_inv hEntry hPr1.1 hPO
            subst hEffNil
            refine ⟨⟨hPureS.frame, hPureS.callers, hPureS.exhausted, by simp [hPureS.effects],
              hPureS.materialized, hPureS.zeroValues, hPureS.addressIds, hPureS.movedFields,
              hPureS.bytes⟩, ?_⟩
            simpa [MemoryRegisters.advance, hBlkS, hPr1.2.1, hPr0.2.1] using hReach
      · rw [hInstEq] at hOutcome
        obtain ⟨_, _, _, _, afterLhs, _, afterReads, _, _, stored, hR0, hR1, _, _, hPO, rfl⟩ := hOutcome
        have hPr0 := readValue_virtual_inv hPure hR0
        have hPr1 := readValue_virtual_inv hPr0.1 hR1
        obtain ⟨hPureS, hBlkS, _, _, _, _⟩ := writeParams_two_regs_inv hEntry hPr1.1 hNe hPO
        refine ⟨⟨hPureS.frame, hPureS.callers, hPureS.exhausted, hPureS.effects,
          hPureS.materialized, hPureS.zeroValues, hPureS.addressIds, hPureS.movedFields,
          hPureS.bytes⟩, ?_⟩
        simpa [MemoryRegisters.advance, hBlkS, hPr1.2.1, hPr0.2.1] using hReach
  | none =>
    rw [hBodyAt] at hOutcome
    have hCtrl := hOutcome.2
    have hTrStep : ∀ (edge : BIREdge sLow) (st : MemoryRegisters sLang profile),
        edge ∈ blk.ending.successors → FramePure bir st →
        (birMemoryView bir).Transfer ⟨edge.target, edge.arguments.map (birOpnd (birBase locals graph))⟩
          st p' false →
        FramePure bir p' ∧ p'.block ∈ BIR.reachableBlocks graph := by
      intro edge st hEdgeMem hStPure hTr
      obtain ⟨values, afterRead, tgtBlk, hRd, hTgtAt, _, hPO⟩ := hTr
      have hPr := (readOperands_virtual_inv hStPure hRd).1
      unfold Memory.blockAt at hTgtAt
      rw [hStPure.frame, hGraphAt] at hTgtAt
      dsimp only [bind, Option.bind, birMemoryGraph] at hTgtAt
      rw [← Array.find?_toList, Array.toList_map] at hTgtAt
      have hTgtId : tgtBlk.id = edge.target := by simpa using List.find?_some hTgtAt
      obtain ⟨rawTgt, hRawTgtMem, hTgtEq⟩ := List.mem_map.mp (List.mem_of_find?_eq_some hTgtAt)
      subst hTgtEq
      have hStartPure : FramePure bir {afterRead with block := edge.target, instruction := 0} :=
        ⟨hPr.frame, hPr.callers, hPr.exhausted, hPr.effects, hPr.materialized,
          hPr.zeroValues, hPr.addressIds, hPr.movedFields, hPr.bytes⟩
      obtain ⟨hPureEnd, hBlkEnd, _, _, _, _⟩ :=
        writeParams_regs_inv hEntry hStartPure (hParamsNd rawTgt hRawTgtMem) hPO
      have hTgtReach : edge.target ∈ BIR.reachableBlocks graph := by
        unfold BIR.reachableBlocks at hReach ⊢
        rw [mem_reachGraph_iff] at hReach ⊢
        refine .snoc (hRawId ▸ hReach) ?_ (List.mem_map.mpr ⟨rawTgt, hRawTgtMem, hTgtId⟩)
          (by intro h; cases h)
        exact List.mem_flatMap.mpr ⟨blk, hBlkMem, List.mem_map.mpr ⟨edge, hEdgeMem, rfl⟩⟩
      exact ⟨hPureEnd, hBlkEnd.symm ▸ hTgtReach⟩
    cases hEnd : blk.ending with
    | ret ov =>
      rw [hEnd] at hCtrl
      obtain ⟨_, _, _, _, _, hCallers, _⟩ := hCtrl
      rw [hPure.callers] at hCallers; cases hCallers
    | trap slot =>
      rw [hEnd] at hCtrl
      dsimp only [birTerm, Memory.ControlStep] at hCtrl
      cases hS : bir.sites[slot.index]? <;> rw [hS] at hCtrl <;> exact False.elim hCtrl
    | jump edge =>
      rw [hEnd] at hCtrl
      exact hTrStep edge p (by simp [hEnd, BIRTerm.successors]) hPure hCtrl
    | branch test yes no =>
      rw [hEnd] at hCtrl
      obtain ⟨_, afterRead, flag, hRd, _, hTr⟩ := hCtrl
      have hPr := (readValue_virtual_inv hPure hRd).1
      cases flag with
      | true =>
        exact hTrStep yes afterRead (by simp [hEnd, BIRTerm.successors]) hPr hTr
      | false =>
        exact hTrStep no afterRead (by simp [hEnd, BIRTerm.successors]) hPr hTr

/-- Every reachable execution state of `BIR.Execution` on a well-formed LCB `BIR` with
`firstUnsupported = none` remains in `bir.entry` with `callers = []` and pure virtual frame
state (no materialized allocations, no side effects, no resource exhaustion). -/
theorem bir_reachable_stays_in_entry {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    (hBackend : profile.backend = .lcb)
    (hWF : BIR.WF (language := sliceLanguage slice) bir)
    (hSup : firstUnsupported (language := sliceLanguage slice) bir = none)
    {s : MemoryRegisters (sliceLanguage slice) profile}
    (hFin : (BIR.Execution (language := sliceLanguage slice)).finite bir s) :
    s.frame = bir.entry ∧ s.callers = [] ∧ s.resourceExhausted = false ∧
    s.effects = [] ∧ s.materialized = [] ∧ s.zeroValues = [] ∧
    s.addressIds = [] ∧ s.movedFields = [] ∧ s.bytes = Mem.Memory.empty := by
  let sLang := sliceLanguage slice
  let sLow := sliceLoweringLanguage slice
  have hEntryMem := hWF.registerFormation.entry_mem
  have hEntryEx : ∃ v, AList.lookup bir.functions bir.entry = some v := by
    obtain ⟨⟨fn, loc, gr⟩, hm, heq⟩ := List.mem_map.mp hEntryMem
    dsimp only at heq; subst heq
    have hnd := hWF.registerFormation.functions_nodup
    exact ⟨(loc, gr), alist_lookup_of_mem_nodup hnd hm⟩
  obtain ⟨⟨locals, graph⟩, hEntry⟩ := hEntryEx
  obtain ⟨start, hInit, hSteps⟩ := hFin
  have hInitEq : start = BIR.entryState bir := hInit
  subst hInitEq
  have hEntryBlkReach : (BIR.entryState (language := sLang) bir).block ∈ BIR.reachableBlocks graph := by
    have hMemEq := birMemoryView_eq bir
    have hGraphAt := birMemoryView_graphAt hEntry
    have hFnMem : (bir.entry, locals, graph) ∈ bir.functions := alist_lookup_mem hEntry
    let normGraphCfg : NormalizedCFG sLow :=
      ⟨graph.entry, graph.blocks.toList.map fun b =>
        ⟨b.id, b.params, b.body, RegTerminator.ofBIR bir.sites.size b.ending, b.boundaryOrigins⟩⟩
    have hNormMem : (bir.entry, locals, normGraphCfg) ∈ (RegisterIR.bir bir).normalize.functions :=
      List.mem_map.mpr ⟨(bir.entry, locals, graph), hFnMem, rfl⟩
    obtain ⟨_, _, ⟨normEntry, hNE, hNEId, _⟩, _, _, _⟩ :=
      (hWF.registerFormation.2.2.2.2.2.2.2.2.2.2 _ hNormMem).2.2.2
    obtain ⟨entryBlk, hEBMem, hEBEq⟩ := List.mem_map.mp hNE
    have hEBId : entryBlk.id = graph.entry := (congrArg Block.id hEBEq).trans hNEId
    have hBlkEq : (BIR.entryState (language := sLang) bir).block = graph.entry := by
      unfold BIR.entryState
      rw [← hMemEq]
      show (((birMemoryView bir).graphAt (birMemoryView bir).entry).map CFG.entry).getD ⟨0⟩ = graph.entry
      change (((birMemoryView bir).graphAt bir.entry).map CFG.entry).getD ⟨0⟩ = graph.entry
      rw [hGraphAt]
      rfl
    rw [hBlkEq]
    unfold BIR.reachableBlocks
    rw [mem_reachGraph_iff]
    exact .refl (List.mem_map.mpr ⟨entryBlk, hEBMem, hEBId⟩) (by intro h; cases h)
  have hStartPure : FramePure bir (BIR.entryState (language := sLang) bir) :=
    ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  have hInv : ∀ {a b}, Steps ((BIR.Execution (language := sLang)).step bir) a b →
      FramePure bir a → a.block ∈ BIR.reachableBlocks graph →
      FramePure bir b ∧ b.block ∈ BIR.reachableBlocks graph := by
    intro a b hSt
    induction hSt with
    | nil _ => intro hp hr; exact ⟨hp, hr⟩
    | cons h1 _ ih =>
      intro hp hr
      obtain ⟨hp', hr'⟩ := bir_step_preserves_pure_and_reach hBackend hWF hSup hEntry hp hr h1
      exact ih hp' hr'
  obtain ⟨hp, _⟩ := hInv hSteps hStartPure hEntryBlkReach
  exact ⟨hp.frame, hp.callers, hp.exhausted, hp.effects, hp.materialized, hp.zeroValues,
    hp.addressIds, hp.movedFields, hp.bytes⟩

/-! ## 7. Main LCB Semantic Preservation Theorem (`emitLCB_refines`) -/

private theorem block_eq_of_id_eq {Ty : Type} {lang : LoweringLanguage Ty}
    {xs : List (BIRBlock lang)} (hnd : (xs.map Block.id).Nodup)
    {b₁ b₂ : BIRBlock lang} (h₁ : b₁ ∈ xs) (h₂ : b₂ ∈ xs) (hid : b₁.id = b₂.id) :
    b₁ = b₂ := by
  induction xs with
  | nil => cases h₁
  | cons a rest ih =>
    rw [List.map_cons, List.nodup_cons] at hnd
    cases h₁ with
    | head =>
      cases h₂ with
      | head => rfl
      | tail _ h2t => exact absurd (hid ▸ List.mem_map.mpr ⟨b₂, h2t, rfl⟩) hnd.1
    | tail _ h1t =>
      cases h₂ with
      | head => exact absurd (hid.symm ▸ List.mem_map.mpr ⟨b₁, h1t, rfl⟩) hnd.1
      | tail _ h2t => exact ih hnd.2 h1t h2t

private theorem orderedReachable_nodup {Ty : Type} {lang : LoweringLanguage Ty}
    {graph : BIRGraph lang} {blocks : List (BIRBlock lang)}
    (hOrd : orderedReachableBlocks graph = some blocks)
    (hNd : (graph.blocks.toList.map Block.id).Nodup) :
    (blocks.map Block.id).Nodup ∧
    (∀ blk ∈ blocks, blk ∈ graph.blocks.toList ∧ blk.id ∈ BIR.reachableBlocks graph) ∧
    (∀ blk ∈ graph.blocks.toList, blk.id ∈ BIR.reachableBlocks graph → blk ∈ blocks) ∧
    (∃ e0 rest, blocks = e0 :: rest ∧ e0.id = graph.entry) := by
  unfold orderedReachableBlocks at hOrd
  cases hFind : graph.blocks.toList.find? (·.id == graph.entry) with
  | none => rw [hFind] at hOrd; cases hOrd
  | some entryBlk =>
    rw [hFind] at hOrd
    cases Option.some.inj hOrd
    have hEMem := List.mem_of_find?_eq_some hFind
    have hEId : entryBlk.id = graph.entry := eq_of_beq (by simpa using List.find?_some hFind)
    have hEReach : graph.entry ∈ BIR.reachableBlocks graph := by
      unfold BIR.reachableBlocks
      rw [mem_reachGraph_iff]
      exact .refl (List.mem_map.mpr ⟨entryBlk, hEMem, hEId⟩) (by intro h; cases h)
    refine ⟨?_, ?_, ?_, ⟨entryBlk, _, rfl, hEId⟩⟩
    · rw [List.map_cons, List.nodup_cons]
      refine ⟨?_, ?_⟩
      · intro hMem
        obtain ⟨b, hb, hbId⟩ := List.mem_map.mp hMem
        have hf := List.mem_filter.mp hb
        have hNe : b.id ≠ graph.entry := bne_iff_ne.mp (Bool.and_eq_true_iff.mp hf.2).1
        exact hNe (hbId.trans hEId)
      · have hFiltSub : ∀ (xs : List (BIRBlock lang)) (p : BIRBlock lang → Bool),
            (xs.map Block.id).Nodup → ((xs.filter p).map Block.id).Nodup := by
          intro xs p hnd
          induction xs with
          | nil => exact List.nodup_nil
          | cons a rs ih =>
            rw [List.map_cons, List.nodup_cons] at hnd
            simp only [List.filter_cons]
            split
            · rw [List.map_cons, List.nodup_cons]
              refine ⟨fun hm => hnd.1 ?_, ih hnd.2⟩
              obtain ⟨b, hb, heq⟩ := List.mem_map.mp hm
              exact List.mem_map.mpr ⟨b, (List.mem_filter.mp hb).1, heq⟩
            · exact ih hnd.2
        exact hFiltSub _ _ hNd
    · intro blk hBlk
      cases hBlk with
      | head => exact ⟨hEMem, hEId ▸ hEReach⟩
      | tail _ htl =>
        have hf := List.mem_filter.mp htl
        simp only [Bool.and_eq_true, bne_iff_ne, List.contains_eq_mem, decide_eq_true_eq] at hf
        exact ⟨hf.1, hf.2.2⟩
    · intro blk hBlk hReach
      by_cases hEq : blk.id = graph.entry
      · have hSame := block_eq_of_id_eq hNd hBlk hEMem (hEq.trans hEId.symm)
        subst hSame; exact List.mem_cons_self
      · apply List.mem_cons_of_mem
        exact List.mem_filter.mpr ⟨hBlk, by
          simp only [Bool.and_eq_true, bne_iff_ne, List.contains_eq_mem, decide_eq_true_eq]
          exact ⟨hEq, hReach⟩⟩

private theorem list_nodup_reverse {α : Type} : ∀ {xs : List α}, xs.Nodup → xs.reverse.Nodup
  | [], _ => List.nodup_nil
  | a :: rest, h => by
    rw [List.nodup_cons] at h
    rw [List.reverse_cons, List.nodup_append]
    refine ⟨list_nodup_reverse h.2, List.nodup_cons.mpr ⟨by simp, List.nodup_nil⟩, ?_⟩
    intro x hx y hy hEq
    subst hEq
    cases List.mem_singleton.mp hy
    exact h.1 (List.mem_reverse.mp hx)

private theorem collectSplitEdges_spec {Ty : Type} {lang : LoweringLanguage Ty}
    (offset : Nat) (blocks : List (BIRBlock lang)) (indices : AList BlockId Nat) :
    let splits := collectSplitEdges offset blocks indices
    (∀ k sp, splits[k]? = some sp →
      sp.splitIndex = offset + blocks.length + k ∧
      ∃ j blk test yes no,
        blocks[j]? = some blk ∧
        sp.predIndex = offset + j ∧
        blk.ending = .branch test yes no ∧
        yes.target = no.target ∧
        targetHasParams blocks yes.target = true ∧
        AList.lookup indices yes.target = some sp.targetIndex ∧
        sp.targetId = yes.target ∧
        sp.arguments = no.arguments) ∧
    (splits.map SplitEdge.predIndex).Nodup ∧
    (∀ j blk test yes no tgtIdx,
      blocks[j]? = some blk →
      blk.ending = .branch test yes no →
      yes.target = no.target →
      targetHasParams blocks yes.target = true →
      AList.lookup indices yes.target = some tgtIdx →
      ∃ sp ∈ splits, sp.predIndex = offset + j ∧ sp.targetIndex = tgtIdx ∧ sp.arguments = no.arguments) := by
  let P (sp : SplitEdge lang) : Prop :=
    ∃ j blk test yes no,
      blocks[j]? = some blk ∧
      sp.predIndex = offset + j ∧
      blk.ending = .branch test yes no ∧
      yes.target = no.target ∧
      targetHasParams blocks yes.target = true ∧
      AList.lookup indices yes.target = some sp.targetIndex ∧
      sp.targetId = yes.target ∧
      sp.arguments = no.arguments
  have go : ∀ (xs : List (Nat × BIRBlock lang)) (nextIdx : Nat) (revAcc : List (SplitEdge lang)),
      (∀ x ∈ xs, blocks[x.1]? = some x.2) →
      (xs.map Prod.fst).Nodup →
      (∀ sp ∈ revAcc, P sp) →
      (∀ sp ∈ revAcc, ∀ x ∈ xs, sp.predIndex ≠ offset + x.1) →
      (revAcc.map SplitEdge.predIndex).Nodup →
      nextIdx = offset + blocks.length + revAcc.length →
      (∀ k sp, revAcc.reverse[k]? = some sp → sp.splitIndex = offset + blocks.length + k) →
      let res := collectSplitEdges.go offset blocks indices xs nextIdx revAcc
      (∀ sp ∈ revAcc, sp ∈ res) ∧
      (∀ k sp, res[k]? = some sp → sp.splitIndex = offset + blocks.length + k ∧ P sp) ∧
      (res.map SplitEdge.predIndex).Nodup ∧
      (∀ j blk test yes no tgtIdx,
        (j, blk) ∈ xs →
        blk.ending = .branch test yes no →
        yes.target = no.target →
        targetHasParams blocks yes.target = true →
        AList.lookup indices yes.target = some tgtIdx →
        ∃ sp ∈ res, sp.predIndex = offset + j ∧ sp.targetIndex = tgtIdx ∧ sp.arguments = no.arguments) := by
    intro xs
    induction xs with
    | nil =>
      intro nextIdx revAcc _ _ hAcc _ hAccNd _ hIdxRev
      refine ⟨fun sp hsp => List.mem_reverse.mpr hsp, ?_, ?_, fun _ _ _ _ _ _ hm => nomatch hm⟩
      · intro k sp hGet
        exact ⟨hIdxRev k sp hGet, hAcc sp (List.mem_reverse.mp (List.mem_of_getElem? hGet))⟩
      · simpa only [collectSplitEdges.go, List.map_reverse] using list_nodup_reverse hAccNd
    | cons pair rest ih =>
      rcases pair with ⟨j, blk⟩
      intro nextIdx revAcc hXs hXsNd hAcc hDisj hAccNd hNext hIdxRev
      rw [List.map_cons, List.nodup_cons] at hXsNd
      have hBlkGet : blocks[j]? = some blk := hXs (j, blk) List.mem_cons_self
      have hRestXs : ∀ x ∈ rest, blocks[x.1]? = some x.2 :=
        fun x hx => hXs x (List.mem_cons_of_mem _ hx)
      have hDisjRest : ∀ sp ∈ revAcc, ∀ x ∈ rest, sp.predIndex ≠ offset + x.1 :=
        fun sp hsp x hx => hDisj sp hsp x (List.mem_cons_of_mem _ hx)
      cases hEnd : blk.ending with
      | ret _ | trap _ | jump _ =>
        have hEqGo : collectSplitEdges.go offset blocks indices ((j, blk) :: rest) nextIdx revAcc =
            collectSplitEdges.go offset blocks indices rest nextIdx revAcc := by
          conv => lhs; unfold collectSplitEdges.go
          rw [hEnd]
        rw [hEqGo]
        obtain ⟨hSub, hSpec, hNd, hFind⟩ := ih nextIdx revAcc hRestXs hXsNd.2 hAcc hDisjRest hAccNd hNext hIdxRev
        refine ⟨hSub, hSpec, hNd, ?_⟩
        intro j' blk' test yes no tgtIdx hm hEnd' hEqTgt hHas hLk
        cases hm with
        | head => rw [hEnd] at hEnd'; cases hEnd'
        | tail _ htl => exact hFind j' blk' test yes no tgtIdx htl hEnd' hEqTgt hHas hLk
      | branch test0 yes0 no0 =>
        have hEqGo : collectSplitEdges.go offset blocks indices ((j, blk) :: rest) nextIdx revAcc =
            if yes0.target == no0.target && targetHasParams blocks yes0.target then
              match AList.lookup indices yes0.target with
              | some tgtIdx =>
                collectSplitEdges.go offset blocks indices rest (nextIdx + 1)
                  (⟨offset + j, nextIdx, yes0.target, tgtIdx, no0.arguments⟩ :: revAcc)
              | none => collectSplitEdges.go offset blocks indices rest nextIdx revAcc
            else collectSplitEdges.go offset blocks indices rest nextIdx revAcc := by
          conv => lhs; unfold collectSplitEdges.go
          rw [hEnd]
          rfl
        rw [hEqGo]
        by_cases hCond : (yes0.target == no0.target && targetHasParams blocks yes0.target) = true
        · simp only [hCond, ↓reduceIte]
          obtain ⟨hTgtBeq, hHas0⟩ := Bool.and_eq_true_iff.mp hCond
          have hTgtEq0 : yes0.target = no0.target := eq_of_beq hTgtBeq
          cases hLk0 : AList.lookup indices yes0.target with
          | none =>
            obtain ⟨hSub, hSpec, hNd, hFind⟩ := ih nextIdx revAcc hRestXs hXsNd.2 hAcc hDisjRest hAccNd hNext hIdxRev
            refine ⟨hSub, hSpec, hNd, ?_⟩
            intro j' blk' test yes no tgtIdx hm hEnd' hEqTgt hHas hLk
            cases hm with
            | head =>
              rw [hEnd] at hEnd'; cases hEnd'
              rw [hLk0] at hLk; cases hLk
            | tail _ htl => exact hFind j' blk' test yes no tgtIdx htl hEnd' hEqTgt hHas hLk
          | some ti0 =>
            let sp0 : SplitEdge lang := ⟨offset + j, nextIdx, yes0.target, ti0, no0.arguments⟩
            have hSp0P : P sp0 :=
              ⟨j, blk, test0, yes0, no0, hBlkGet, rfl, hEnd, hTgtEq0, hHas0, hLk0, rfl, rfl⟩
            have hAcc' : ∀ sp ∈ sp0 :: revAcc, P sp := by
              intro sp hsp
              cases hsp with
              | head => exact hSp0P
              | tail _ htl => exact hAcc sp htl
            have hDisj' : ∀ sp ∈ sp0 :: revAcc, ∀ x ∈ rest, sp.predIndex ≠ offset + x.1 := by
              intro sp hsp x hx
              cases hsp with
              | head =>
                intro hEq
                have hJx : j = x.1 := Nat.add_left_cancel hEq
                exact hXsNd.1 (hJx ▸ List.mem_map.mpr ⟨x, hx, rfl⟩)
              | tail _ htl => exact hDisjRest sp htl x hx
            have hNd' : ((sp0 :: revAcc).map SplitEdge.predIndex).Nodup := by
              rw [List.map_cons, List.nodup_cons]
              refine ⟨?_, hAccNd⟩
              intro hm
              obtain ⟨sp, hsp, hEq⟩ := List.mem_map.mp hm
              exact hDisj sp hsp (j, blk) List.mem_cons_self hEq
            have hIdxRev' : ∀ k sp, (sp0 :: revAcc).reverse[k]? = some sp →
                sp.splitIndex = offset + blocks.length + k := by
              intro k sp hGet
              rw [List.reverse_cons] at hGet
              rcases Nat.lt_trichotomy k revAcc.reverse.length with hlt | heq | hgt
              · rw [List.getElem?_append_left hlt] at hGet
                exact hIdxRev k sp hGet
              · subst heq
                rw [List.getElem?_append_right (Nat.le_refl _), Nat.sub_self] at hGet
                cases Option.some.inj hGet
                simp only [sp0, List.length_reverse]
                exact hNext
              · rw [List.getElem?_append_right (Nat.le_of_lt hgt)] at hGet
                rw [List.getElem?_eq_none (by simp only [List.length_cons, List.length_nil]; exact Nat.sub_pos_of_lt hgt)] at hGet
                cases hGet
            obtain ⟨hSub, hSpec, hNd, hFind⟩ :=
              ih (nextIdx + 1) (sp0 :: revAcc) hRestXs hXsNd.2 hAcc' hDisj' hNd'
                (by rw [List.length_cons, hNext, Nat.add_assoc]) hIdxRev'
            refine ⟨fun sp hsp => hSub sp (List.mem_cons_of_mem _ hsp), hSpec, hNd, ?_⟩
            intro j' blk' test yes no tgtIdx hm hEnd' hEqTgt hHas hLk
            cases hm with
            | head =>
              rw [hEnd] at hEnd'; cases hEnd'
              rw [hLk0] at hLk; cases Option.some.inj hLk
              exact ⟨sp0, hSub sp0 List.mem_cons_self, rfl, rfl, rfl⟩
            | tail _ htl => exact hFind j' blk' test yes no tgtIdx htl hEnd' hEqTgt hHas hLk
        · simp only [hCond, Bool.false_eq_true, ↓reduceIte]
          obtain ⟨hSub, hSpec, hNd, hFind⟩ := ih nextIdx revAcc hRestXs hXsNd.2 hAcc hDisjRest hAccNd hNext hIdxRev
          refine ⟨hSub, hSpec, hNd, ?_⟩
          intro j' blk' test yes no tgtIdx hm hEnd' hEqTgt hHas hLk
          cases hm with
          | head =>
            rw [hEnd] at hEnd'; cases hEnd'
            have hT : (yes0.target == no0.target && targetHasParams blocks yes0.target) = true :=
              Bool.and_eq_true_iff.mpr ⟨beq_iff_eq.mpr hEqTgt, hHas⟩
            exact absurd hT hCond
          | tail _ htl => exact hFind j' blk' test yes no tgtIdx htl hEnd' hEqTgt hHas hLk
  have hIdxNd : ((indexed blocks).map Prod.fst).Nodup := by
    have goNd : ∀ n (ys : List (BIRBlock lang)),
        ((Backend.Runtime.indexedFrom n ys).map Prod.fst).Nodup ∧
        ∀ m ∈ (Backend.Runtime.indexedFrom n ys).map Prod.fst, n ≤ m := by
      intro n ys
      induction ys generalizing n with
      | nil => exact ⟨List.nodup_nil, fun _ hm => nomatch hm⟩
      | cons a rest ih =>
        obtain ⟨hnd, hge⟩ := ih (n + 1)
        refine ⟨?_, ?_⟩
        · simp only [Backend.Runtime.indexedFrom, List.map_cons, List.nodup_cons]
          exact ⟨fun hm => by have := hge n hm; omega, hnd⟩
        · intro m hm
          simp only [Backend.Runtime.indexedFrom, List.map_cons, List.mem_cons] at hm
          rcases hm with rfl | htl
          · exact Nat.le_refl _
          · have := hge m htl; omega
    exact (goNd 0 blocks).1
  obtain ⟨_, hSpec, hNd, hFind⟩ :=
    go (indexed blocks) (offset + blocks.length) []
      (fun x hx => (mem_indexed_getElem? hx).2) hIdxNd (fun _ hm => nomatch hm)
      (fun _ hm => nomatch hm) List.nodup_nil (by simp) (fun _ _ h => by cases h)
  refine ⟨hSpec, hNd, fun j blk test yes no tgtIdx hGet hEnd hEqTgt hHas hLk => ?_⟩
  exact hFind j blk test yes no tgtIdx (getElem?_indexed_mem hGet) hEnd hEqTgt hHas hLk

private theorem flatMap_nodup_subset {α β : Type} {f : α → List β} {xs ys : List α}
    (hXsNd : xs.Nodup) (hFlat : (xs.flatMap f).Nodup)
    (hYsNd : ys.Nodup) (hSub : ∀ y ∈ ys, y ∈ xs) :
    (ys.flatMap f).Nodup := by
  have hDisj : ∀ a b, a ∈ xs → b ∈ xs → a ≠ b → ∀ x ∈ f a, x ∉ f b := by
    clear hYsNd hSub
    induction xs with
    | nil => intro _ _ ha; cases ha
    | cons hd tl ih =>
      rw [List.nodup_cons] at hXsNd
      rw [List.flatMap_cons, List.nodup_append] at hFlat
      intro a b ha hb hne x hxa hxb
      cases ha with
      | head =>
        cases hb with
        | head => exact hne rfl
        | tail _ hbt =>
          exact hFlat.2.2 x hxa x (List.mem_flatMap.mpr ⟨b, hbt, hxb⟩) rfl
      | tail _ hat =>
        cases hb with
        | head =>
          exact hFlat.2.2 x hxb x (List.mem_flatMap.mpr ⟨a, hat, hxa⟩) rfl
        | tail _ hbt =>
          exact ih hXsNd.2 hFlat.2.1 a b hat hbt hne x hxa hxb
  have hSingle : ∀ a ∈ xs, (f a).Nodup := by
    clear hYsNd hSub hXsNd hDisj
    induction xs with
    | nil => intro _ ha; cases ha
    | cons hd tl ih =>
      rw [List.flatMap_cons, List.nodup_append] at hFlat
      intro a ha
      cases ha with
      | head => exact hFlat.1
      | tail _ hat => exact ih hFlat.2.1 a hat
  induction ys with
  | nil => exact List.nodup_nil
  | cons hd tl ih =>
    rw [List.nodup_cons] at hYsNd
    rw [List.flatMap_cons, List.nodup_append]
    refine ⟨hSingle hd (hSub hd List.mem_cons_self),
      ih hYsNd.2 (fun y hy => hSub y (List.mem_cons_of_mem _ hy)), ?_⟩
    intro x hxhd y hyTl hEq
    subst hEq
    obtain ⟨b, hbTl, hxb⟩ := List.mem_flatMap.mp hyTl
    have hNe : hd ≠ b := fun hEq => hYsNd.1 (hEq ▸ hbTl)
    exact hDisj hd b (hSub hd List.mem_cons_self) (hSub b (List.mem_cons_of_mem _ hbTl)) hNe x hxhd hxb

private theorem list_zipIdx_flatMap_eq {α β : Type} (f : α → List β) :
    ∀ (k : Nat) (xs : List α), (xs.zipIdx k).flatMap (fun p => f p.1) = xs.flatMap f
  | _, [] => rfl
  | k, a :: rest => by simp [List.zipIdx, list_zipIdx_flatMap_eq f (k + 1) rest]

private theorem emitUnvalidated_ok_inv {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (h : EmitLCB.emitUnvalidated (language := sliceLanguage slice) bir = .ok output) :
    profile.backend = .lcb ∧
    ¬ (LCBInput.trapCapacity < bir.sites.size) ∧
    bir.requiresLCBLinking = false ∧
    firstUnsupported (language := sliceLanguage slice) bir = none ∧
    output.sites = bir.sites ∧
    ∃ locals graph blocks norm sBlocks,
      AList.lookup bir.functions bir.entry = some (locals, graph) ∧
      orderedReachableBlocks graph = some blocks ∧
      normalizeGraph graph = .ok norm ∧
      (indexed norm.blocks).mapM (emitBlock (sliceLanguage slice) norm) = .ok sBlocks ∧
      output.body =
        { resultWidth := .word
          parameters := Array.replicate norm.paramCount .word
          valueWidths := Array.replicate (valueCount norm) .word
          blocks := sBlocks.toArray } := by
  unfold EmitLCB.emitUnvalidated at h
  cases hBack : (profile.backend != .lcb)
  · have hBackendEq : profile.backend = .lcb := by
      cases hb : profile.backend
      · rw [hb] at hBack; cases hBack
      · rfl
    simp only [hBack, Bool.false_eq_true, ↓reduceIte] at h
    by_cases hCap : LCBInput.trapCapacity < bir.sites.size
    · simp only [hCap, ↓reduceIte, throw, throwThe, MonadExceptOf.throw] at h; cases h
    · simp only [hCap, ↓reduceIte] at h
      cases hLink : bir.requiresLCBLinking
      · simp only [hLink, Bool.false_eq_true, ↓reduceIte] at h
        cases hSup : firstUnsupported (language := sliceLanguage slice) bir with
        | some _ => simp only [hSup, throw, throwThe, MonadExceptOf.throw] at h; cases h
        | none =>
          simp only [hSup] at h
          cases hDiag : siteDiagnostics bir with
          | none => simp only [hDiag, throw, throwThe, MonadExceptOf.throw] at h; cases h
          | some diagnostics =>
            simp only [hDiag] at h
            cases hFn : AList.lookup bir.functions bir.entry with
            | none => simp only [hFn, throw, throwThe, MonadExceptOf.throw] at h; cases h
            | some fnPair =>
              rcases fnPair with ⟨locals, graph⟩
              simp only [hFn] at h
              unfold adaptGraph at h
              cases hNorm : normalizeGraph graph with
              | error _ => simp [hNorm, bind, Except.bind] at h
              | ok norm =>
                simp only [hNorm, bind, Except.bind] at h
                cases hSB : (indexed norm.blocks).mapM (emitBlock (sliceLanguage slice) norm) with
                | error _ => simp [hSB] at h
                | ok sBlocks =>
                  simp only [hSB, pure, Except.pure, Except.ok.injEq] at h
                  subst h
                  have hOrdEx : ∃ blocks, orderedReachableBlocks graph = some blocks := by
                    unfold normalizeGraph at hNorm
                    cases ho : orderedReachableBlocks graph with
                    | none => simp [ho, throw, throwThe, MonadExceptOf.throw] at hNorm
                    | some bs => exact ⟨bs, rfl⟩
                  obtain ⟨blocks, hOrd⟩ := hOrdEx
                  exact ⟨hBackendEq, hCap, rfl, rfl, rfl, locals, graph, blocks, norm, sBlocks,
                    rfl, hOrd, hNorm, hSB, rfl⟩
      · simp only [hLink, ↓reduceIte, throw, throwThe, MonadExceptOf.throw] at h; cases h
  · simp only [hBack, ↓reduceIte, throw, throwThe, MonadExceptOf.throw] at h; cases h

private structure LCBCtx {slice : Slice} {profile : Profile}
    (bir : BIR (sliceLoweringLanguage slice) profile) (output : LCBInput profile) where
  hWF : BIR.WF (language := sliceLanguage slice) bir
  hEmit : EmitLCB.emitUnvalidated (language := sliceLanguage slice) bir = .ok output
  hBackend : profile.backend = .lcb
  hCap : ¬ (LCBInput.trapCapacity < bir.sites.size)
  hSup : firstUnsupported (language := sliceLanguage slice) bir = none
  hSitesEq : output.sites = bir.sites
  locals : LocalTypes (sliceLanguage slice).operands.Ty
  graph : BIRGraph (sliceLoweringLanguage slice)
  blocks : List (BIRBlock (sliceLoweringLanguage slice))
  norm : NormGraph (sliceLoweringLanguage slice)
  sBlocks : List Source.Block
  entryBlk : BIRBlock (sliceLoweringLanguage slice)
  restBlks : List (BIRBlock (sliceLoweringLanguage slice))
  realBlocks : List (NormBlock (sliceLoweringLanguage slice))
  hEntry : AList.lookup bir.functions bir.entry = some (locals, graph)
  hOrd : orderedReachableBlocks graph = some blocks
  hNorm : normalizeGraph graph = .ok norm
  hSBlocks : (indexed norm.blocks).mapM (emitBlock (sliceLanguage slice) norm) = .ok sBlocks
  hBodyEq : output.body =
    { resultWidth := .word
      parameters := Array.replicate norm.paramCount .word
      valueWidths := Array.replicate (valueCount norm) .word
      blocks := sBlocks.toArray }
  hNodesNd : (graph.blocks.toList.map Block.id).Nodup
  hParamsNd : ∀ b ∈ graph.blocks.toList, (b.params.map fun q => q.2.id).Nodup
  hBlocksNd : (blocks.map Block.id).Nodup
  hBlocksSub : ∀ blk ∈ blocks, blk ∈ graph.blocks.toList ∧ blk.id ∈ BIR.reachableBlocks graph
  hBlocksCov : ∀ blk ∈ graph.blocks.toList, blk.id ∈ BIR.reachableBlocks graph → blk ∈ blocks
  hBlocksCons : blocks = entryBlk :: restBlks
  hEntryBlkId : entryBlk.id = graph.entry
  hRealBlocks : (indexed blocks).mapM
    (normalizeBlock (if hasIncomingEdge blocks graph.entry then 1 else 0)
      (blockIndexTable (if hasIncomingEdge blocks graph.entry then 1 else 0) blocks)
      (collectSplitEdges (if hasIncomingEdge blocks graph.entry then 1 else 0) blocks
        (blockIndexTable (if hasIncomingEdge blocks graph.entry then 1 else 0) blocks))) = .ok realBlocks
  hParamCntEq : norm.paramCount = entryBlk.params.length
  hNormBlksEq : norm.blocks =
    (if hasIncomingEdge blocks graph.entry then
      [{ sourceId := none, params := [], body := [], term := .jumpEntryParams 1 entryBlk.params.length }]
     else []) ++ realBlocks ++
    ((collectSplitEdges (if hasIncomingEdge blocks graph.entry then 1 else 0) blocks
        (blockIndexTable (if hasIncomingEdge blocks graph.entry then 1 else 0) blocks)).map
      fun sp => { sourceId := none, params := [], body := [], term := .jump sp.targetIndex sp.arguments })
  hEntryParamsEmpty : entryBlk.params = []
  hRegIdsKeysNodup : (List.map Prod.fst (regIds norm)).Nodup

namespace LCBCtx

variable {slice : Slice} {profile : Profile}
  {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}

private abbrev base (ctx : LCBCtx bir output) : Nat := birBase ctx.locals ctx.graph
private abbrev entryHasPreds (ctx : LCBCtx bir output) : Bool := hasIncomingEdge ctx.blocks ctx.graph.entry
private abbrev offset (ctx : LCBCtx bir output) : Nat := if ctx.entryHasPreds then 1 else 0
private abbrev indices (ctx : LCBCtx bir output) : AList BlockId Nat := blockIndexTable ctx.offset ctx.blocks
private abbrev splits (ctx : LCBCtx bir output) : List (SplitEdge (sliceLoweringLanguage slice)) :=
  collectSplitEdges ctx.offset ctx.blocks ctx.indices

private abbrev tramp (ctx : LCBCtx bir output) : List (NormBlock (sliceLoweringLanguage slice)) :=
  if ctx.entryHasPreds then
    [{ sourceId := none, params := [], body := [], term := .jumpEntryParams 1 ctx.entryBlk.params.length }]
  else []

private abbrev splitBlks (ctx : LCBCtx bir output) : List (NormBlock (sliceLoweringLanguage slice)) :=
  ctx.splits.map fun sp => { sourceId := none, params := [], body := [], term := .jump sp.targetIndex sp.arguments }

private theorem tramp_length (ctx : LCBCtx bir output) : ctx.tramp.length = ctx.offset := by
  unfold tramp offset; cases ctx.entryHasPreds <;> rfl

private theorem real_length (ctx : LCBCtx bir output) : ctx.realBlocks.length = ctx.blocks.length :=
  (mapM_except_getElem? ctx.hRealBlocks).1.trans (indexed_length ctx.blocks)

private theorem paramCount_zero (ctx : LCBCtx bir output) : ctx.norm.paramCount = 0 := by
  rw [ctx.hParamCntEq, ctx.hEntryParamsEmpty]; rfl

private theorem zeroParams (ctx : LCBCtx bir output) :
    ∀ b0, ctx.norm.blocks[0]? = some b0 → b0.params = [] := by
  intro b0 h0
  rw [ctx.hNormBlksEq] at h0
  cases hEPc : hasIncomingEdge ctx.blocks ctx.graph.entry with
  | true =>
    simp only [hEPc, ↓reduceIte, List.cons_append, List.getElem?_cons_zero, Option.some.injEq] at h0
    subst h0; rfl
  | false =>
    simp only [hEPc, Bool.false_eq_true, ↓reduceIte, List.nil_append] at h0
    have h0Pos : 0 < ctx.realBlocks.length := by rw [ctx.real_length, ctx.hBlocksCons]; simp
    rw [List.getElem?_append_left h0Pos] at h0
    have hIdx0 : (indexed ctx.blocks)[0]? = some (0, ctx.entryBlk) := by
      rw [Backend.Runtime.getElem?_indexed, ctx.hBlocksCons]; rfl
    obtain ⟨nb0, hNb0, hNb0Eq⟩ := (mapM_except_getElem? ctx.hRealBlocks).2 hIdx0
    rw [hNb0] at h0; cases Option.some.inj h0
    unfold normalizeBlock at hNb0Eq
    cases hNT : normalizeTerm ctx.indices ctx.splits (ctx.offset + 0) ctx.entryBlk.ending with
    | error _ => rw [hNT] at hNb0Eq; cases hNb0Eq
    | ok nt =>
      rw [hNT] at hNb0Eq
      simp only [bind, Except.bind, pure, Except.pure] at hNb0Eq
      cases Except.ok.inj hNb0Eq
      exact ctx.hEntryParamsEmpty

private theorem realAt (ctx : LCBCtx bir output) {rk : Nat} {blk : BIRBlock (sliceLoweringLanguage slice)}
    (hBlkGet : ctx.blocks[rk]? = some blk) :
    ∃ nb lb,
      ctx.norm.blocks[ctx.offset + rk]? = some nb ∧
      output.body.blocks[ctx.offset + rk]? = some lb ∧
      emitBlock (sliceLanguage slice) ctx.norm (ctx.offset + rk, nb) = .ok lb ∧
      nb.sourceId = some blk.id ∧ nb.params = blk.params ∧ nb.body = blk.body ∧
      normalizeTerm ctx.indices ctx.splits (ctx.offset + rk) blk.ending = .ok nb.term := by
  have hIdxGet : (indexed ctx.blocks)[rk]? = some (rk, blk) := by
    rw [Backend.Runtime.getElem?_indexed, hBlkGet]; rfl
  obtain ⟨nb, hNbGet, hNbEq⟩ := (mapM_except_getElem? ctx.hRealBlocks).2 hIdxGet
  have hrkLt : rk < ctx.blocks.length := by
    rcases Nat.lt_or_ge rk ctx.blocks.length with h | h
    · exact h
    · rw [List.getElem?_eq_none h] at hBlkGet; cases hBlkGet
  have hNormGet : ctx.norm.blocks[ctx.offset + rk]? = some nb := by
    rw [ctx.hNormBlksEq]
    show (ctx.tramp ++ ctx.realBlocks ++ ctx.splitBlks)[ctx.offset + rk]? = some nb
    rw [List.append_assoc, List.getElem?_append_right (by rw [ctx.tramp_length]; omega),
      ctx.tramp_length, Nat.add_sub_cancel_left,
      List.getElem?_append_left (by rw [ctx.real_length]; exact hrkLt)]
    exact hNbGet
  have hSBIdx : (indexed ctx.norm.blocks)[ctx.offset + rk]? = some (ctx.offset + rk, nb) := by
    rw [Backend.Runtime.getElem?_indexed, hNormGet]; rfl
  obtain ⟨lb, hLbGet, hLbEq⟩ := (mapM_except_getElem? ctx.hSBlocks).2 hSBIdx
  have hBodyBlk : output.body.blocks[ctx.offset + rk]? = some lb := by
    rw [ctx.hBodyEq]; simpa only [List.getElem?_toArray] using hLbGet
  unfold normalizeBlock at hNbEq
  cases hNT : normalizeTerm ctx.indices ctx.splits (ctx.offset + rk) blk.ending with
  | error _ => simp [hNT, bind, Except.bind] at hNbEq
  | ok nt =>
    simp only [hNT, bind, Except.bind, pure, Except.pure, Except.ok.injEq] at hNbEq
    subst hNbEq
    exact ⟨_, lb, hNormGet, hBodyBlk, hLbEq, rfl, rfl, rfl, rfl⟩

private theorem splitAt (ctx : LCBCtx bir output) {k : Nat} {sp : SplitEdge (sliceLoweringLanguage slice)}
    (hSpGet : ctx.splits[k]? = some sp) :
    let nb : NormBlock (sliceLoweringLanguage slice) := ⟨none, [], [], .jump sp.targetIndex sp.arguments⟩
    ∃ lb,
      ctx.norm.blocks[sp.splitIndex]? = some nb ∧
      output.body.blocks[sp.splitIndex]? = some lb ∧
      emitBlock (sliceLanguage slice) ctx.norm (sp.splitIndex, nb) = .ok lb ∧
      lb.phis = #[] ∧ lb.instructions = #[] ∧ lb.terminator = .jump sp.targetIndex := by
  obtain ⟨hSpSpec, _, _⟩ := collectSplitEdges_spec ctx.offset ctx.blocks ctx.indices
  have hIdxEq := (hSpSpec k sp hSpGet).1
  let nb : NormBlock (sliceLoweringLanguage slice) := ⟨none, [], [], .jump sp.targetIndex sp.arguments⟩
  have hNormGet : ctx.norm.blocks[sp.splitIndex]? = some nb := by
    rw [ctx.hNormBlksEq, hIdxEq]
    show (ctx.tramp ++ ctx.realBlocks ++ ctx.splitBlks)[ctx.offset + ctx.blocks.length + k]? = some nb
    rw [List.getElem?_append_right (by rw [List.length_append, ctx.tramp_length, ctx.real_length]; omega)]
    rw [List.length_append, ctx.tramp_length, ctx.real_length, Nat.add_sub_cancel_left]
    simp only [splitBlks, List.getElem?_map, hSpGet, Option.map_some, nb]
  have hSBIdx : (indexed ctx.norm.blocks)[sp.splitIndex]? = some (sp.splitIndex, nb) := by
    rw [Backend.Runtime.getElem?_indexed, hNormGet]; rfl
  obtain ⟨lb, hLbGet, hLbEq⟩ := (mapM_except_getElem? ctx.hSBlocks).2 hSBIdx
  have hBodyBlk : output.body.blocks[sp.splitIndex]? = some lb := by
    rw [ctx.hBodyEq]; simpa only [List.getElem?_toArray] using hLbGet
  obtain ⟨phis, xss, term, hPhis, hInsts, hTerm, rfl⟩ := emitBlock_ok_inv hLbEq
  have hPhisNil : phis = [] := by
    split at hPhis
    · exact (Except.ok.inj hPhis).symm
    · cases hPhis; rfl
  have hXssNil : xss = [] := by cases hInsts; rfl
  have hTermEq : term = .jump sp.targetIndex := by cases hTerm; rfl
  subst hPhisNil; subst hXssNil; subst hTermEq
  exact ⟨_, hNormGet, hBodyBlk, hLbEq, rfl, rfl, rfl⟩

private theorem indicesNodup (ctx : LCBCtx bir output) : (List.map Prod.fst ctx.indices).Nodup := by
  have hEq : List.map Prod.fst ctx.indices = ctx.blocks.map Block.id := by
    unfold indices blockIndexTable
    rw [List.map_map]
    have go : ∀ n (ys : List (BIRBlock (sliceLoweringLanguage slice))),
        (Backend.Runtime.indexedFrom n ys).map (Prod.fst ∘ fun p => (p.2.id, ctx.offset + p.1)) =
        ys.map Block.id := by
      intro n ys
      induction ys generalizing n with
      | nil => rfl
      | cons a rest ih => simp [Backend.Runtime.indexedFrom, ih]
    exact go 0 ctx.blocks
  rw [hEq]; exact ctx.hBlocksNd

private theorem indicesAt (ctx : LCBCtx bir output) {rk : Nat} {blk : BIRBlock (sliceLoweringLanguage slice)}
    (hGet : ctx.blocks[rk]? = some blk) :
    AList.lookup ctx.indices blk.id = some (ctx.offset + rk) :=
  alist_lookup_of_mem_nodup ctx.indicesNodup (List.mem_map.mpr ⟨(rk, blk), getElem?_indexed_mem hGet, rfl⟩)

private theorem regIdParam (ctx : LCBCtx bir output) {j k : Nat}
    {nb : NormBlock (sliceLoweringLanguage slice)} {p : Sigma (SSAValue EarlyTy)}
    (hNb : ctx.norm.blocks[j]? = some nb) (hjNe : j ≠ 0) (hp : nb.params[k]? = some p) :
    regId ctx.norm p.2.id = some (blockBase ctx.norm j + k) := by
  apply alist_lookup_of_mem_nodup ctx.hRegIdsKeysNodup
  unfold regIds
  refine List.mem_flatMap.mpr ⟨(j, nb), getElem?_indexed_mem hNb, List.mem_append_left _ ?_⟩
  exact List.mem_map.mpr ⟨(k, p), getElem?_indexed_mem hp, by simp [beq_false_of_ne hjNe]⟩

private theorem regIdInst (ctx : LCBCtx bir output) {j i r off : Nat}
    {nb : NormBlock (sliceLoweringLanguage slice)}
    {op : LocatedOp (SSAInst (sliceLoweringLanguage slice))}
    (hNb : ctx.norm.blocks[j]? = some nb) (hi : nb.body[i]? = some op)
    (hDef : (r, off) ∈ instDefs op.instruction) :
    regId ctx.norm r = some (blockBase ctx.norm j + instBase j nb i + off) := by
  apply alist_lookup_of_mem_nodup ctx.hRegIdsKeysNodup
  unfold regIds
  refine List.mem_flatMap.mpr ⟨(j, nb), getElem?_indexed_mem hNb, List.mem_append_right _ ?_⟩
  refine List.mem_flatMap.mpr ⟨(i, op), getElem?_indexed_mem hi, ?_⟩
  exact List.mem_map.mpr ⟨(r, off), hDef, rfl⟩

private theorem bodyWidths (ctx : LCBCtx bir output) {m : Nat} (hm : m < valueCount ctx.norm) :
    output.body.valueWidths[m]? = some .word := by
  rw [ctx.hBodyEq]; simp [hm]

private theorem blockAt (ctx : LCBCtx bir output) {p : MemoryRegisters (sliceLanguage slice) profile}
    {rk : Nat} {blk : BIRBlock (sliceLoweringLanguage slice)}
    (hPure : FramePure bir p) (hBlkGet : ctx.blocks[rk]? = some blk) (hPBlk : p.block = blk.id) :
    (birMemoryView (language := sliceLanguage slice) bir).blockAt p = some
      ⟨blk.id, blk.params.map (fun r => ⟨r.1, birReg ctx.base r.2⟩),
        blk.body.map (fun op => ⟨op.origin, birInst ctx.base op.instruction⟩),
        birTerm bir ctx.base blk.ending, blk.boundaryOrigins⟩ := by
  have hBlkGraph := (ctx.hBlocksSub blk (List.mem_of_getElem? hBlkGet)).1
  have hGraphAt := birMemoryView_graphAt (language := sliceLanguage slice) ctx.hEntry
  unfold Memory.blockAt
  rw [hPure.frame, hGraphAt, hPBlk]
  dsimp only [bind, Option.bind, birMemoryGraph]
  rw [← Array.find?_toList, Array.toList_map]
  have hFindMap : ∀ (xs : List (BIRBlock (sliceLoweringLanguage slice)))
      (f : BIRBlock (sliceLoweringLanguage slice) → MemoryBlock (sliceLoweringLanguage slice))
      (_ : ∀ b, (f b).id = b.id),
      (xs.map f).find? (·.id == blk.id) = (xs.find? (·.id == blk.id)).map f := by
    intro xs f hfId
    induction xs with
    | nil => rfl
    | cons a rs ih =>
      simp only [List.map_cons, List.find?, hfId]
      cases (a.id == blk.id) <;> simp [ih]
  rw [hFindMap ctx.graph.blocks.toList _ (fun _ => rfl)]
  have hFindSome : ctx.graph.blocks.toList.find? (·.id == blk.id) = some blk := by
    cases hf : ctx.graph.blocks.toList.find? (·.id == blk.id) with
    | none =>
      have hNot := List.find?_eq_none.mp hf blk hBlkGraph
      simp at hNot
    | some found =>
      have hFoundMem := List.mem_of_find?_eq_some hf
      have hFoundId : found.id = blk.id := by simpa using List.find?_some hf
      rw [block_eq_of_id_eq ctx.hNodesNd hFoundMem hBlkGraph hFoundId]
  rw [hFindSome]; rfl

end LCBCtx

private theorem lcb_entry_params_empty {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    {locals : LocalTypes (sliceLanguage slice).operands.Ty}
    {graph : BIRGraph (sliceLoweringLanguage slice)}
    {blocks : List (BIRBlock (sliceLoweringLanguage slice))}
    {entryBlk : BIRBlock (sliceLoweringLanguage slice)}
    {restBlks : List (BIRBlock (sliceLoweringLanguage slice))}
    (hEntry : AList.lookup bir.functions bir.entry = some (locals, graph))
    (hOrd : orderedReachableBlocks graph = some blocks)
    (hBlocksCons : blocks = entryBlk :: restBlks)
    (hSafe : ProgramSafe (BIR.Execution (language := sliceLanguage slice)) bir) :
    entryBlk.params = [] := by
  let sLang := sliceLanguage slice
  let sLow := sliceLoweringLanguage slice
  let base := birBase locals graph
  have hMemEq := birMemoryView_eq (language := sLang) bir
  have hGraphAt := birMemoryView_graphAt (language := sLang) hEntry
  by_cases hEP : entryBlk.params.length = 0
  · exact List.eq_nil_of_length_eq_zero hEP
  · exfalso
    let p0 := BIR.entryState (language := sLang) bir
    have hP0Blk : p0.block = graph.entry := by
      unfold p0 BIR.entryState
      rw [← hMemEq]
      change (((birMemoryView bir).graphAt bir.entry).map CFG.entry).getD ⟨0⟩ = graph.entry
      rw [hGraphAt]; rfl
    have hFindMemBlk : (birMemoryView bir).blockAt p0 = some
        ⟨entryBlk.id, entryBlk.params.map (fun q => ⟨q.1, birReg base q.2⟩),
          entryBlk.body.map (fun op => ⟨op.origin, birInst base op.instruction⟩),
          birTerm bir base entryBlk.ending, entryBlk.boundaryOrigins⟩ := by
      unfold Memory.blockAt
      change (((birMemoryView bir).graphAt bir.entry).bind fun g => g.blocks.find? (·.id == p0.block)) = _
      rw [hGraphAt, hP0Blk]
      dsimp only [Option.bind, birMemoryGraph]
      rw [← Array.find?_toList, Array.toList_map]
      have hFindGraph : graph.blocks.toList.find? (·.id == graph.entry) = some entryBlk := by
        unfold orderedReachableBlocks at hOrd
        cases hf : graph.blocks.toList.find? (·.id == graph.entry) with
        | none => rw [hf] at hOrd; cases hOrd
        | some e =>
          rw [hf] at hOrd; cases Option.some.inj hOrd
          rw [hBlocksCons] at hOrd
          cases hOrd; rfl
      have hFindMap : ∀ (xs : List (BIRBlock sLow)) (f : BIRBlock sLow → MemoryBlock sLow)
          (_ : ∀ b, (f b).id = b.id),
          (xs.map f).find? (·.id == graph.entry) = (xs.find? (·.id == graph.entry)).map f := by
        intro xs f hfId
        induction xs with
        | nil => rfl
        | cons a rs ih =>
          simp only [List.map_cons, List.find?, hfId]
          cases (a.id == graph.entry) <;> simp [ih]
      rw [hFindMap graph.blocks.toList _ (fun _ => rfl), hFindGraph]; rfl
    have hP0Pc : p0.instruction = entryBlk.body.length + 1 := by
      have hP0Eq : p0 = birEntryState (birMemoryView bir) := birEntryState_eq bir
      rw [hP0Eq]
      unfold birEntryState
      have hBlkAtEntry : ((birMemoryView bir).graphAt bir.entry).bind
          (fun g => g.blocks.find? (·.id == (((birMemoryView bir).graphAt bir.entry).map CFG.entry).getD ⟨0⟩)) =
          some ⟨entryBlk.id, entryBlk.params.map (fun q => ⟨q.1, birReg base q.2⟩),
            entryBlk.body.map (fun op => ⟨op.origin, birInst base op.instruction⟩),
            birTerm bir base entryBlk.ending, entryBlk.boundaryOrigins⟩ := by
        have h2 := hFindMemBlk
        unfold Memory.blockAt at h2
        change ((birMemoryView bir).graphAt bir.entry).bind (fun g => g.blocks.find? (·.id == p0.block)) = _ at h2
        rw [hP0Blk] at h2
        rw [hGraphAt] at h2 ⊢
        exact h2
      dsimp only
      change ((((birMemoryView bir).graphAt bir.entry).bind
        (fun g => g.blocks.find? (·.id == (((birMemoryView bir).graphAt bir.entry).map CFG.entry).getD ⟨0⟩))).map
        fun b => if b.params.length = 0 then 0 else b.body.length + 1).getD 0 = entryBlk.body.length + 1
      rw [hBlkAtEntry]
      simp only [Option.map_some, List.length_map, hEP, ↓reduceIte, Option.getD_some]
    apply hSafe p0
    refine ⟨⟨p0, rfl, .nil _⟩, ?_, ?_⟩
    · intro next hStep
      change _ ∧ (bir.memoryView (language := sLang)).Step p0 next true ∧ _ at hStep
      rw [← hMemEq] at hStep
      have hSt := hStep.2.1
      unfold Memory.Step at hSt
      have hP0Exh : p0.resourceExhausted = false := rfl
      rw [hP0Exh] at hSt
      simp only [Bool.false_eq_true, ↓reduceIte] at hSt
      have hOut : ∃ ex nxt, (birMemoryView bir).StepOutcome p0 nxt ex true := by
        split at hSt
        · obtain ⟨_, ps, hO⟩ := hSt; exact ⟨true, ps, hO⟩
        · exact ⟨false, next, hSt⟩
      obtain ⟨_, _, b, hB, hO⟩ := hOut
      rw [hFindMemBlk] at hB; cases Option.some.inj hB
      rw [hP0Pc] at hO
      have hNone : (entryBlk.body.map (fun op =>
          (⟨op.origin, birInst base op.instruction⟩ : LocatedOp (MemoryInst sLow))))[entryBlk.body.length + 1]? = none :=
        List.getElem?_eq_none (by simp)
      rw [hNone] at hO
      have hEqLen := hO.1
      rw [List.length_map] at hEqLen
      exact Nat.succ_ne_self _ hEqLen
    · intro o hRes
      change BIR.hostedResult (language := sLang) bir p0 = some o at hRes
      rw [birHostedResult_eq] at hRes
      have hTermNone : (birMemoryView bir).terminalObservation p0 = none := by
        unfold Memory.terminalObservation
        have hP0Exh : p0.resourceExhausted = false := rfl
        rw [hP0Exh, hFindMemBlk, hP0Pc]
        have hNone : (entryBlk.body.map (fun op =>
            (⟨op.origin, birInst base op.instruction⟩ : LocatedOp (MemoryInst sLow))))[entryBlk.body.length + 1]? = none :=
          List.getElem?_eq_none (by simp)
        dsimp only [bind, Option.bind]
        simp only [Bool.false_eq_true, ↓reduceIte, hNone, List.length_map]
        have hBeqSucc : ∀ n : Nat, Nat.beq (n + 1) n = false := by
          intro n; induction n with | zero => rfl | succ m ih => exact ih
        have hNeLen : (entryBlk.body.length + 1 != entryBlk.body.length) = true := by
          change !(Nat.beq (entryBlk.body.length + 1) entryBlk.body.length) = true
          rw [hBeqSucc]; rfl
        simp only [hNeLen, ↓reduceIte]
      unfold birHostedResult at hRes
      rw [hTermNone] at hRes
      cases hRes

private theorem lcb_regIds_keys_nodup {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile}
    (hBackend : profile.backend = .lcb)
    (hWF : BIR.WF (language := sliceLanguage slice) bir)
    (hSup : firstUnsupported (language := sliceLanguage slice) bir = none)
    {locals : LocalTypes (sliceLanguage slice).operands.Ty}
    {graph : BIRGraph (sliceLoweringLanguage slice)}
    {blocks : List (BIRBlock (sliceLoweringLanguage slice))}
    {norm : NormGraph (sliceLoweringLanguage slice)}
    {entryBlk : BIRBlock (sliceLoweringLanguage slice)}
    {realBlocks : List (NormBlock (sliceLoweringLanguage slice))}
    (hEntry : AList.lookup bir.functions bir.entry = some (locals, graph))
    (hNodesNd : (graph.blocks.toList.map Block.id).Nodup)
    (hDefsNd : (List.map Prod.fst (functionDefTable
      (⟨graph.entry, graph.blocks.toList.map fun b =>
        ⟨b.id, b.params, b.body, RegTerminator.ofBIR bir.sites.size b.ending, b.boundaryOrigins⟩⟩ :
        NormalizedCFG (sliceLoweringLanguage slice)))).Nodup)
    (hBlocksNd : (blocks.map Block.id).Nodup)
    (hBlocksSub : ∀ blk ∈ blocks, blk ∈ graph.blocks.toList ∧ blk.id ∈ BIR.reachableBlocks graph)
    (hRealBlocks : (indexed blocks).mapM
      (normalizeBlock (if hasIncomingEdge blocks graph.entry then 1 else 0)
        (blockIndexTable (if hasIncomingEdge blocks graph.entry then 1 else 0) blocks)
        (collectSplitEdges (if hasIncomingEdge blocks graph.entry then 1 else 0) blocks
          (blockIndexTable (if hasIncomingEdge blocks graph.entry then 1 else 0) blocks))) = .ok realBlocks)
    (hNormBlksEq : norm.blocks =
      (if hasIncomingEdge blocks graph.entry then
        [{ sourceId := none, params := [], body := [], term := .jumpEntryParams 1 entryBlk.params.length }]
       else []) ++ realBlocks ++
      ((collectSplitEdges (if hasIncomingEdge blocks graph.entry then 1 else 0) blocks
          (blockIndexTable (if hasIncomingEdge blocks graph.entry then 1 else 0) blocks)).map
        fun sp => { sourceId := none, params := [], body := [], term := .jump sp.targetIndex sp.arguments })) :
    (List.map Prod.fst (regIds norm)).Nodup := by
  let sLow := sliceLoweringLanguage slice
  let entryHasPreds := hasIncomingEdge blocks graph.entry
  let offset := if entryHasPreds then 1 else 0
  let indices := blockIndexTable offset blocks
  let splits := collectSplitEdges offset blocks indices
  obtain ⟨hRealLen0, hRealEl⟩ := mapM_except_getElem? hRealBlocks
  have hRealLen : realBlocks.length = blocks.length := hRealLen0.trans (indexed_length blocks)
  have hMapFst : List.map Prod.fst (regIds norm) =
      norm.blocks.flatMap fun nb =>
        (nb.params.map fun p => p.2.id) ++
        (nb.body.flatMap fun op => (instDefs op.instruction).map Prod.fst) := by
    unfold regIds
    rw [List.map_flatMap]
    have go : ∀ n (ys : List (NormBlock sLow)),
        (Backend.Runtime.indexedFrom n ys).flatMap (fun p =>
          ((indexed p.2.params).map (fun q => (q.2.2.id, if p.1 == 0 then q.1 else blockBase norm p.1 + q.1)) ++
            (indexed p.2.body).flatMap (fun o =>
              (instDefs o.2.instruction).map (fun d => (d.1, blockBase norm p.1 + instBase p.1 p.2 o.1 + d.2)))).map Prod.fst) =
        ys.flatMap fun nb =>
          (nb.params.map fun p => p.2.id) ++
          (nb.body.flatMap fun op => (instDefs op.instruction).map Prod.fst) := by
      intro n ys
      induction ys generalizing n with
      | nil => rfl
      | cons nb rest ih =>
        rw [Backend.Runtime.indexedFrom, List.flatMap_cons, List.flatMap_cons, ih (n + 1)]
        congr 1
        rw [List.map_append, List.map_map, List.map_flatMap]
        congr 1
        · have hP : ∀ m (ps : List (Sigma (SSAValue EarlyTy))),
              (Backend.Runtime.indexedFrom m ps).map (fun q => q.2.2.id) = ps.map (fun p => p.2.id) := by
            intro m ps
            induction ps generalizing m with
            | nil => rfl
            | cons a r ihp => simp [Backend.Runtime.indexedFrom, ihp]
          exact hP 0 nb.params
        · have hB : ∀ m (ops : List (LocatedOp (SSAInst sLow))),
              (Backend.Runtime.indexedFrom m ops).flatMap
                (fun o => (instDefs o.2.instruction).map Prod.fst) =
              ops.flatMap (fun op => (instDefs op.instruction).map Prod.fst) := by
            intro m ops
            induction ops generalizing m with
            | nil => rfl
            | cons a r ihb => simp [Backend.Runtime.indexedFrom, ihb]
          have h1 : ∀ (o : Nat × LocatedOp (SSAInst sLow)),
              ((instDefs o.2.instruction).map (fun d => (d.1, blockBase norm n + instBase n nb o.1 + d.2))).map Prod.fst =
              (instDefs o.2.instruction).map Prod.fst := by
            intro o; rw [List.map_map]; rfl
          simp only [h1]
          exact hB 0 nb.body
    exact go 0 norm.blocks
  rw [hMapFst, hNormBlksEq]
  let tramp : List (NormBlock sLow) :=
    if entryHasPreds then
      [{ sourceId := none, params := [], body := [], term := .jumpEntryParams 1 entryBlk.params.length }]
    else []
  let splitBlks : List (NormBlock sLow) :=
    splits.map fun sp => { sourceId := none, params := [], body := [], term := .jump sp.targetIndex sp.arguments }
  change ((tramp ++ realBlocks ++ splitBlks).flatMap _).Nodup
  have hTrampNil : (tramp.flatMap fun nb =>
      (nb.params.map fun p => p.2.id) ++
      (nb.body.flatMap fun op => (instDefs op.instruction).map Prod.fst)) = [] := by
    unfold tramp; cases entryHasPreds <;> rfl
  have hSplitNil : (splitBlks.flatMap fun nb =>
      (nb.params.map fun p => p.2.id) ++
      (nb.body.flatMap fun op => (instDefs op.instruction).map Prod.fst)) = [] := by
    unfold splitBlks; rw [List.flatMap_map]
    induction splits with
    | nil => rfl
    | cons _ _ ih => exact ih
  rw [List.flatMap_append, List.flatMap_append, hTrampNil, hSplitNil, List.nil_append, List.append_nil]
  have hRealEqBlks : (realBlocks.flatMap fun nb =>
      (nb.params.map fun p => p.2.id) ++
      (nb.body.flatMap fun op => (instDefs op.instruction).map Prod.fst)) =
      (blocks.flatMap fun blk =>
        (blk.params.map fun p => p.2.id) ++
        (blk.body.flatMap fun op => op.instruction.defs.map Prod.fst)) := by
    have hGo : ∀ d k, d + k = blocks.length →
        ((realBlocks.drop k).flatMap fun nb =>
          (nb.params.map fun p => p.2.id) ++
          (nb.body.flatMap fun op => (instDefs op.instruction).map Prod.fst)) =
        ((blocks.drop k).flatMap fun blk =>
          (blk.params.map fun p => p.2.id) ++
          (blk.body.flatMap fun op => op.instruction.defs.map Prod.fst)) := by
      intro d
      induction d with
      | zero =>
        intro k hd
        have hkEq : k = blocks.length := by omega
        subst hkEq
        rw [List.drop_of_length_le (Nat.le_of_eq hRealLen), List.drop_length]; rfl
      | succ d' ih =>
        intro k hd
        have hkLt : k < blocks.length := by omega
        have hBlkGet : blocks[k]? = some blocks[k] := List.getElem?_eq_getElem hkLt
        have hIdxGet : (indexed blocks)[k]? = some (k, blocks[k]) := by
          rw [Backend.Runtime.getElem?_indexed, hBlkGet]; rfl
        obtain ⟨nb, hNbGet, hNbEq⟩ := hRealEl hIdxGet
        have hNbPB : nb.params = blocks[k].params ∧ nb.body = blocks[k].body := by
          change normalizeBlock offset indices splits (k, blocks[k]) = .ok nb at hNbEq
          unfold normalizeBlock at hNbEq
          cases hNT : normalizeTerm indices splits (offset + k) blocks[k].ending with
          | error _ => simp [hNT, bind, Except.bind] at hNbEq
          | ok _ =>
            simp only [hNT, bind, Except.bind, pure, Except.pure, Except.ok.injEq] at hNbEq
            subst hNbEq; exact ⟨rfl, rfl⟩
        have hDropR : realBlocks.drop k = nb :: realBlocks.drop (k + 1) :=
          List.drop_eq_getElem_cons (by rw [hRealLen]; exact hkLt) |>.trans
            (by have := List.getElem?_eq_getElem (by rw [hRealLen]; exact hkLt) (l := realBlocks)
                rw [this] at hNbGet; cases Option.some.inj hNbGet; rfl)
        have hDropB : blocks.drop k = blocks[k] :: blocks.drop (k + 1) :=
          List.drop_eq_getElem_cons hkLt
        rw [hDropR, hDropB, List.flatMap_cons, List.flatMap_cons, ih (k + 1) (by omega)]
        congr 1
        rw [hNbPB.1, hNbPB.2]
        congr 1
        obtain ⟨hBlkGraph, hBlkReach⟩ := hBlocksSub blocks[k] (List.getElem_mem hkLt)
        have hOpEq : ∀ op ∈ blocks[k].body,
            (instDefs op.instruction).map Prod.fst = op.instruction.defs.map Prod.fst := by
          intro op hOp
          rcases lcb_reachable_inst_classify hBackend hWF hSup hEntry hBlkGraph hBlkReach hOp with
            ⟨_, _, _, _, hI⟩ | ⟨_, _, _, _, _, _, _, _, _, hI⟩ | ⟨_, _, _, _, _, _, _, hI⟩ <;>
            rw [hI] <;> rfl
        have hFM : ∀ (ops : List (LocatedOp (SSAInst sLow))),
            (∀ op ∈ ops, (instDefs op.instruction).map Prod.fst = op.instruction.defs.map Prod.fst) →
            ops.flatMap (fun op => (instDefs op.instruction).map Prod.fst) =
            ops.flatMap (fun op => op.instruction.defs.map Prod.fst) := by
          intro ops ho
          induction ops with
          | nil => rfl
          | cons a r iho =>
            simp only [List.flatMap_cons, ho a List.mem_cons_self,
              iho (fun x hx => ho x (List.mem_cons_of_mem _ hx))]
        exact hFM blocks[k].body hOpEq
    simpa using hGo blocks.length 0 (by omega)
  rw [hRealEqBlks]
  have hGraphFlatNd : (graph.blocks.toList.flatMap fun blk =>
      (blk.params.map fun p => p.2.id) ++
      (blk.body.flatMap fun op => op.instruction.defs.map Prod.fst)).Nodup := by
    unfold functionDefTable at hDefsNd
    rw [List.map_flatMap, List.flatMap_map] at hDefsNd
    have hItemEq : ∀ blk : BIRBlock sLow,
        ((blk.params.map fun p => (p.2.id, (p.1, DefSite.mk blk.id .param))) ++
          blk.body.zipIdx.flatMap fun pair =>
            pair.1.instruction.defs.map fun d => (d.1, (d.2, DefSite.mk blk.id (.inst pair.2)))).map Prod.fst =
        (blk.params.map fun p => p.2.id) ++
          (blk.body.flatMap fun op => op.instruction.defs.map Prod.fst) := by
      intro blk
      rw [List.map_append, List.map_map, List.map_flatMap]
      congr 1
      have hM : ∀ p : LocatedOp (SSAInst sLow) × Nat,
          (p.1.instruction.defs.map fun d => (d.1, (d.2, DefSite.mk blk.id (.inst p.2)))).map Prod.fst =
          p.1.instruction.defs.map Prod.fst := fun p => by rw [List.map_map]; rfl
      simp only [hM]
      exact list_zipIdx_flatMap_eq (fun op => op.instruction.defs.map Prod.fst) 0 blk.body
    have hEq : (graph.blocks.toList.flatMap fun blk =>
        ((blk.params.map fun p => (p.2.id, (p.1, DefSite.mk blk.id .param))) ++
          blk.body.zipIdx.flatMap fun pair =>
            pair.1.instruction.defs.map fun d => (d.1, (d.2, DefSite.mk blk.id (.inst pair.2)))).map Prod.fst) =
        graph.blocks.toList.flatMap fun blk =>
          (blk.params.map fun p => p.2.id) ++
          (blk.body.flatMap fun op => op.instruction.defs.map Prod.fst) := by
      congr 1; funext blk; exact hItemEq blk
    rw [← hEq]; exact hDefsNd
  have hNodupOfMap : ∀ {α β : Type} {f : α → β} {l : List α}, (l.map f).Nodup → l.Nodup := by
    intro α β f l h
    induction l with
    | nil => exact List.nodup_nil
    | cons a r ih =>
      rw [List.map_cons, List.nodup_cons] at h
      rw [List.nodup_cons]
      exact ⟨fun hm => h.1 (List.mem_map.mpr ⟨a, hm, rfl⟩), ih h.2⟩
  exact flatMap_nodup_subset (hNodupOfMap hNodesNd) hGraphFlatNd (hNodupOfMap hBlocksNd)
    (fun b hb => (hBlocksSub b hb).1)

private theorem buildLCBCtx {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (hWF : BIR.WF (language := sliceLanguage slice) bir)
    (hEmit : EmitLCB.emitUnvalidated (language := sliceLanguage slice) bir = .ok output)
    (hSafe : ProgramSafe (BIR.Execution (language := sliceLanguage slice)) bir) :
    Nonempty (LCBCtx bir output) := by
  let sLow := sliceLoweringLanguage slice
  obtain ⟨hBackend, hCap, _, hSup, hSitesEq, locals, graph, blocks, norm, sBlocks,
    hEntry, hOrd, hNorm, hSBlocks, hBodyEq⟩ := emitUnvalidated_ok_inv hEmit
  have hFnMem : (bir.entry, locals, graph) ∈ bir.functions := alist_lookup_mem hEntry
  let normGraphCfg : NormalizedCFG sLow :=
    ⟨graph.entry, graph.blocks.toList.map fun b =>
      ⟨b.id, b.params, b.body, RegTerminator.ofBIR bir.sites.size b.ending, b.boundaryOrigins⟩⟩
  have hNormMem : (bir.entry, locals, normGraphCfg) ∈ (RegisterIR.bir bir).normalize.functions :=
    List.mem_map.mpr ⟨(bir.entry, locals, graph), hFnMem, rfl⟩
  have hFnFormed := hWF.registerFormation.2.2.2.2.2.2.2.2.2.2 _ hNormMem
  have hNodesNd : (graph.blocks.toList.map Block.id).Nodup := by
    have hnd := hFnFormed.2.2.1; rw [List.map_map] at hnd; exact hnd
  obtain ⟨_, _, _, hDefsNd, _, _⟩ := hFnFormed.2.2.2
  have hParamsNd : ∀ b ∈ graph.blocks.toList, (b.params.map fun q => q.2.id).Nodup := by
    intro b hb
    have hnb : (⟨b.id, b.params, b.body, RegTerminator.ofBIR bir.sites.size b.ending,
        b.boundaryOrigins⟩ : Block _ _ (RegTerminator sLow)) ∈ normGraphCfg.blocks :=
      List.mem_map.mpr ⟨b, hb, rfl⟩
    unfold functionDefTable at hDefsNd
    rw [List.map_flatMap] at hDefsNd
    have hOne := nodup_of_mem_flatMap hDefsNd hnb
    rw [List.map_append, List.map_map] at hOne
    exact (List.nodup_append.mp hOne).1
  obtain ⟨hBlocksNd, hBlocksSub, hBlocksCov, entryBlk, restBlks, hBlocksCons, hEntryBlkId⟩ :=
    orderedReachable_nodup hOrd hNodesNd
  let entryHasPreds := hasIncomingEdge blocks graph.entry
  let offset := if entryHasPreds then 1 else 0
  let indices := blockIndexTable offset blocks
  let splits := collectSplitEdges offset blocks indices
  have hNormParts : ∃ realBlocks,
      (indexed blocks).mapM (normalizeBlock offset indices splits) = .ok realBlocks ∧
      norm.paramCount = entryBlk.params.length ∧
      norm.blocks =
        (if entryHasPreds then
          [{ sourceId := none, params := [], body := [], term := .jumpEntryParams 1 entryBlk.params.length }]
         else []) ++ realBlocks ++
        (splits.map fun sp => { sourceId := none, params := [], body := [], term := .jump sp.targetIndex sp.arguments }) := by
    unfold normalizeGraph at hNorm
    rw [hOrd] at hNorm
    dsimp only at hNorm
    cases hRB : (indexed blocks).mapM (normalizeBlock offset indices splits) with
    | error _ => simp [hRB, bind, Except.bind, offset, entryHasPreds, indices, splits] at hNorm
    | ok rb =>
      simp only [bind, Except.bind, pure, Except.pure] at hNorm
      have hRB' : List.mapM
          (normalizeBlock (if hasIncomingEdge blocks graph.entry = true then 1 else 0)
            (blockIndexTable (if hasIncomingEdge blocks graph.entry = true then 1 else 0) blocks)
            (collectSplitEdges (if hasIncomingEdge blocks graph.entry = true then 1 else 0) blocks
              (blockIndexTable (if hasIncomingEdge blocks graph.entry = true then 1 else 0) blocks)))
          (indexed blocks) = Except.ok rb := hRB
      rw [hRB'] at hNorm
      simp only [hBlocksCons, Except.ok.injEq] at hNorm
      subst hNorm
      exact ⟨rb, rfl, rfl, by simp only [entryHasPreds, offset, indices, splits, hBlocksCons]⟩
  obtain ⟨realBlocks, hRealBlocks, hParamCntEq, hNormBlksEq⟩ := hNormParts
  have hEntryParamsEmpty := lcb_entry_params_empty hEntry hOrd hBlocksCons hSafe
  have hRegIdsKeysNodup := lcb_regIds_keys_nodup hBackend hWF hSup hEntry hNodesNd hDefsNd
    hBlocksNd hBlocksSub hRealBlocks hNormBlksEq
  exact ⟨⟨hWF, hEmit, hBackend, hCap, hSup, hSitesEq, locals, graph, blocks, norm, sBlocks,
    entryBlk, restBlks, realBlocks, hEntry, hOrd, hNorm, hSBlocks, hBodyEq, hNodesNd, hParamsNd,
    hBlocksNd, hBlocksSub, hBlocksCov, hBlocksCons, hEntryBlkId, hRealBlocks, hParamCntEq,
    hNormBlksEq, hEntryParamsEmpty, hRegIdsKeysNodup⟩⟩

private structure SimInv {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (ctx : LCBCtx bir output)
    (p : MemoryRegisters (sliceLanguage slice) profile) (q : Source.State) : Prop where
  pure : FramePure bir p
  atBlk : ∃ rk blk nb lb,
    ctx.blocks[rk]? = some blk ∧
    p.block = blk.id ∧
    q.block = ctx.offset + rk ∧
    ctx.norm.blocks[q.block]? = some nb ∧
    output.body.blocks[q.block]? = some lb ∧
    emitBlock (sliceLanguage slice) ctx.norm (q.block, nb) = .ok lb ∧
    nb.sourceId = some blk.id ∧ nb.params = blk.params ∧ nb.body = blk.body ∧
    normalizeTerm ctx.indices ctx.splits q.block blk.ending = .ok nb.term ∧
    p.instruction ≤ blk.body.length ∧
    q.position = ((blk.body.take p.instruction).map fun op => instWidth op.instruction).sum
  envSz : q.environment.size = valueCount ctx.norm
  env : ∀ r v, AList.lookup p.virtualValues ⟨ctx.base + r⟩ = some v →
    ValInRange v ∧
    ∀ slot, regId ctx.norm r = some slot → q.environment[slot]? = some (some (encVal v))

private theorem sim_readAgree {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (ctx : LCBCtx bir output)
    {p : MemoryRegisters (sliceLanguage slice) profile} {q : Source.State}
    {ty : EarlyTy} {read : SSARead EarlyTy (sliceLoweringLanguage slice).literal ty}
    {opnd : Source.Operand} {v : EarlyValue} {afterRead : MemoryRegisters (sliceLanguage slice) profile}
    (hSim : SimInv ctx p q)
    (hEm : emitRead (sliceLanguage slice) ctx.norm read = .ok opnd)
    (hRd : (birMemoryView (language := sliceLanguage slice) bir).readValue p
      (birRead (language := sliceLanguage slice) ctx.base read) = some (v, afterRead)) :
    ValInRange v ∧ Source.read q.environment .word opnd = .ok (encVal v) := by
  have hInv := readValue_virtual_inv hSim.pure hRd
  cases read with
  | literal lit =>
    dsimp only [emitRead] at hEm
    rw [decodeLiteralWord_slice (slice := slice) lit] at hEm
    have hLit := hInv.2.2.2.2
    rw [hLit] at hEm
    dsimp only [Option.map] at hEm
    cases Except.ok.inj hEm
    refine ⟨sliceLiteral_inRange hLit, ?_⟩
    simp only [Source.read, encVal_width v, word_bne, Bool.false_eq_true, ↓reduceIte,
      bind, Except.bind, pure, Except.pure]
  | value reg =>
    dsimp only [emitRead] at hEm
    cases hSlot : regId ctx.norm reg.id with
    | none => rw [hSlot] at hEm; cases hEm
    | some slot =>
      rw [hSlot] at hEm; dsimp only at hEm
      cases Except.ok.inj hEm
      have hLk := hInv.2.2.2.2
      obtain ⟨hInR, hAt⟩ := hSim.env reg.id v hLk
      refine ⟨hInR, ?_⟩
      simp only [Source.read, hAt slot hSlot, encVal_width v, word_bne, Bool.false_eq_true, ↓reduceIte,
        bind, Except.bind, pure, Except.pure]

private theorem sim_terminal {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (ctx : LCBCtx bir output)
    {p : MemoryRegisters (sliceLanguage slice) profile} {q : Source.State}
    {obs : Concrete.Contracts.Observation}
    (hSim : SimInv ctx p q)
    (hRes : (BIR.Execution (language := sliceLanguage slice)).result bir p obs) :
    ∃ w, Source.step output.body q = .ok (.returned w) ∧
      lcbRuntimeResult bir.project output w = some obs ∧
      (BIR.Execution (language := sliceLanguage slice)).trace bir p =
        (LCBInput.Execution (sliceLanguage slice).operands.Event bir.project).trace output (some q) := by
  let sLang := sliceLanguage slice
  let sLow := sliceLoweringLanguage slice
  obtain ⟨rk, blk, nb, lb, hBlkGet, hPBlk, hQBlk, hNormGet, hBodyGet, hEmitBlk,
    hSrcId, hNbP, hNbB, hNormTerm, hPcLe, hQPos⟩ := hSim.atBlk
  have hBlkAt := ctx.blockAt hSim.pure hBlkGet hPBlk
  have hBlkGraph := (ctx.hBlocksSub blk (List.mem_of_getElem? hBlkGet)).1
  have hBlkReach := (ctx.hBlocksSub blk (List.mem_of_getElem? hBlkGet)).2
  change BIR.hostedResult (language := sLang) bir p = some obs at hRes
  rw [birHostedResult_eq] at hRes
  have hResOrig : birHostedResult (birMemoryView (language := sLang) bir) p = some obs := hRes
  unfold birHostedResult at hRes
  have hTermSome : ∃ tObs, (birMemoryView bir).terminalObservation p = some tObs := by
    cases ht : (birMemoryView bir).terminalObservation p with
    | none => rw [ht] at hRes; cases hRes
    | some t => exact ⟨t, rfl⟩
  obtain ⟨tObs, hTObs⟩ := hTermSome
  have hTermUnf := hTObs
  unfold Memory.terminalObservation at hTermUnf
  rw [hSim.pure.exhausted, hBlkAt] at hTermUnf
  simp only [Bool.false_eq_true, ↓reduceIte, bind, Option.bind] at hTermUnf
  have hPcEq : p.instruction = blk.body.length := by
    cases hOpAt : (blk.body.map (fun op =>
        (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst (sliceLoweringLanguage slice)))))[p.instruction]? with
    | some mOp =>
      exfalso
      rw [hOpAt] at hTermUnf; dsimp only at hTermUnf
      rw [List.getElem?_map] at hOpAt
      cases hRaw : blk.body[p.instruction]? with
      | none => simp [hRaw] at hOpAt
      | some op =>
        rw [hRaw] at hOpAt; cases Option.some.inj hOpAt
        have hNoTrap : (birMemoryView bir).instructionTrap p (birInst ctx.base op.instruction) = none := by
          rcases lcb_reachable_inst_classify ctx.hBackend ctx.hWF ctx.hSup ctx.hEntry hBlkGraph hBlkReach
            (List.mem_of_getElem? hRaw) with
            ⟨_, _, _, _, hI⟩ | ⟨_, _, _, _, _, _, _, _, _, hI⟩ | ⟨_, _, _, _, _, _, _, hI⟩
          · rw [hI]; dsimp only [birInst, Memory.instructionTrap]
            cases (birMemoryView bir).readArgs (birArgs ctx.base _) p <;> rfl
          · rw [hI]; dsimp only [birInst, Memory.instructionTrap]
            cases (birMemoryView bir).readArgs (birArgs ctx.base _) p <;> rfl
          · rw [hI]; rfl
        rw [hNoTrap] at hTermUnf; cases hTermUnf
    | none =>
      rw [hOpAt] at hTermUnf; dsimp only at hTermUnf
      by_cases hEq : p.instruction = blk.body.length
      · exact hEq
      · have hBne : (p.instruction != (blk.body.map (fun op =>
            (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst (sliceLoweringLanguage slice))))).length) = true := by
          simp [hEq]
        rw [hBne] at hTermUnf; cases hTermUnf
  have hQPosEnd : q.position = lb.instructions.size := by
    rw [hQPos, hPcEq, List.take_length, emitBlock_insts_size hEmitBlk, hNbB]
  have hNoneAtEnd : (blk.body.map (fun op =>
      (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst (sliceLoweringLanguage slice)))))[p.instruction]? = none :=
    List.getElem?_eq_none (by simp [hPcEq])
  rw [hNoneAtEnd] at hTermUnf; dsimp only at hTermUnf
  simp only [hPcEq, List.length_map, bne_self_eq_false, Bool.false_eq_true, ↓reduceIte] at hTermUnf
  obtain ⟨_, _, term, _, _, hEmitTerm, hLbEq⟩ := emitBlock_ok_inv hEmitBlk
  have hLbTerm : lb.terminator = term := by rw [hLbEq]
  cases hEnd : blk.ending with
  | jump _ => rw [hEnd] at hTermUnf; cases hTermUnf
  | branch _ _ _ => rw [hEnd] at hTermUnf; cases hTermUnf
  | ret ov =>
    rw [hEnd] at hTermUnf hNormTerm
    cases ov with
    | none => cases hTermUnf
    | some pair =>
      rcases pair with ⟨rty, arg⟩
      dsimp only [birTerm, Option.map, birOpnd, bind, Option.bind] at hTermUnf
      have hEntryEq : (birMemoryView (language := sLang) bir).entry = bir.entry := rfl
      rw [hSim.pure.callers, hSim.pure.frame, hEntryEq] at hTermUnf
      simp only [List.isEmpty_nil, Bool.not_true, bne_self_eq_false, Bool.or_self,
        Bool.false_eq_true, ↓reduceIte] at hTermUnf
      cases hRd : (birMemoryView bir).readValue p (birRead ctx.base arg) with
      | none => rw [hRd] at hTermUnf; cases hTermUnf
      | some pr =>
        rcases pr with ⟨val, afterRead⟩
        rw [hRd] at hTermUnf; dsimp only at hTermUnf
        cases hCanEnd : (!afterRead.live.all ((birMemoryView bir).canEnd afterRead)) with
        | true => rw [hCanEnd] at hTermUnf; cases hTermUnf
        | false =>
          rw [hCanEnd] at hTermUnf; simp only [Bool.false_eq_true, ↓reduceIte] at hTermUnf
          cases hHost : (sliceLanguage slice).hostedResult (.returned val) with
          | none => rw [hHost] at hTermUnf; cases hTermUnf
          | some hr =>
            rw [hHost] at hTermUnf; dsimp only at hTermUnf
            cases hr with
            | trapped _ _ _ => cases hTermUnf
            | returned status =>
              cases Option.some.inj hTermUnf
              rw [hTObs] at hRes; dsimp only [bind, Option.bind] at hRes
              cases Option.some.inj hRes
              have hValU8 : ∃ n : Int, val = .scalar (.int .u8 n) ∧ IntTy.u8.InRange n ∧
                  status = UInt8.ofNat n.toNat := by
                unfold CoreLanguage.hostedResult at hHost
                cases val with
                | other _ => cases hHost
                | scalar sv =>
                  cases sv with
                  | bool _ | char _ | unit => cases hHost
                  | int t n =>
                    cases t with
                    | u8 =>
                      dsimp only at hHost
                      split at hHost
                      · rename_i hRng
                        cases Option.some.inj hHost
                        exact ⟨n, rfl, by
                          simp only [IntTy.InRange, IntTy.min, IntTy.max, IntTy.bits, IntTy.signed,
                            Bool.false_eq_true, ↓reduceIte, Int.reducePow]
                          exact ⟨hRng.1, Int.le_sub_one_of_lt hRng.2⟩, rfl⟩
                      · cases hHost
                    | i8 | u16 | i16 | u32 | i32 | u64 | i64 => cases hHost
              obtain ⟨n, hValEq, hNRng, hStatEq⟩ := hValU8
              subst hValEq; subst hStatEq
              dsimp only [normalizeTerm] at hNormTerm
              have hNbTerm : nb.term = .ret (some ⟨rty, arg⟩) := (Except.ok.inj hNormTerm).symm
              rw [hNbTerm] at hEmitTerm
              dsimp only [emitTerm, bind, Except.bind, pure, Except.pure] at hEmitTerm
              cases hArgEm : emitRead (sliceLanguage slice) ctx.norm arg with
              | error _ => rw [hArgEm] at hEmitTerm; cases hEmitTerm
              | ok arg' =>
                rw [hArgEm] at hEmitTerm
                cases Except.ok.inj hEmitTerm
                have hReadOk := (sim_readAgree ctx hSim hArgEm hRd).2
                have hStepRet : Source.step output.body q = .ok (.returned (word n)) :=
                  step_ret hBodyGet hQPosEnd hLbTerm (by rw [ctx.hBodyEq]; exact hReadOk)
                have hRt : lcbRuntimeResult bir.project output (word n) =
                    some (.returned (UInt8.ofNat n.toNat) ByteArray.empty) := by
                  unfold lcbRuntimeResult word
                  dsimp only
                  have hLt : (wordBits n).toNat < 256 := u8_wordBits_lt hNRng
                  rw [u8_wordBits_toNat hNRng] at hLt ⊢
                  simp only [hLt, ↓reduceIte]
                refine ⟨word n, hStepRet, hRt, ?_⟩
                change (p.effects.map ProcessEvent.effect ++
                  ((BIR.hostedResult (language := sLang) bir p).map
                    fun o => o.stderr.data.toList.map ProcessEvent.stderr).getD []) = _
                rw [lcbExecution_trace_eq]
                have hHosted : BIR.hostedResult (language := sLang) bir p =
                    some (.returned (UInt8.ofNat n.toNat) ByteArray.empty) := by
                  rw [birHostedResult_eq]; unfold birHostedResult; rw [hTObs]; rfl
                rw [hHosted, hSim.pure.effects]
                simp only [bind, Option.bind, hStepRet, hRt, Option.map_some, Option.getD_some,
                  List.map_nil, List.nil_append]
  | trap slot =>
    rw [hEnd] at hTermUnf hNormTerm
    dsimp only [birTerm] at hTermUnf
    cases hSite : bir.sites[slot.index]? with
    | none => rw [hSite] at hTermUnf; cases hTermUnf
    | some entry =>
      rw [hSite] at hTermUnf; dsimp only at hTermUnf
      have hHostTr : (sliceLanguage slice).hostedResult (.trapped entry.kind entry.origin.operation) =
          some (.trapped 134 entry.kind entry.origin.operation) := rfl
      rw [hHostTr] at hTermUnf; dsimp only at hTermUnf
      cases Option.some.inj hTermUnf
      rw [hTObs] at hRes; dsimp only [bind, Option.bind] at hRes
      rw [hBlkAt] at hRes; dsimp only at hRes
      rw [hNoneAtEnd] at hRes; dsimp only at hRes
      rw [hEnd] at hRes; dsimp only [birTerm] at hRes
      rw [hSite] at hRes; dsimp only [birMemoryView] at hRes
      cases hRtSite : runtimeSite bir.project entry.origin with
      | none => rw [hRtSite] at hRes; cases hRes
      | some resolved =>
        rw [hRtSite] at hRes; dsimp only [pure] at hRes
        cases Option.some.inj hRes
        have hDiagEq : trapDiagnostic bir.project entry.kind entry.origin.operation entry.origin.lexical =
            (renderTrapLine entry.kind resolved).toUTF8 := by
          rw [trapDiagnostic_origin, hRtSite]; rfl
        dsimp only [normalizeTerm] at hNormTerm
        have hNbTerm : nb.term = .trap slot := (Except.ok.inj hNormTerm).symm
        rw [hNbTerm] at hEmitTerm
        dsimp only [emitTerm] at hEmitTerm
        cases Except.ok.inj hEmitTerm
        have hStepRet : Source.step output.body q = .ok (.returned (trapWord slot.index)) :=
          step_ret hBodyGet hQPosEnd hLbTerm (by rw [ctx.hBodyEq]; exact read_literal q.environment _)
        have hSlotLt : slot.index < LCBWordLemmas.trapCapacity := by
          have h1 : slot.index < bir.sites.size := by
            rcases Nat.lt_or_ge slot.index bir.sites.size with h | h
            · exact h
            · rw [Array.getElem?_eq_none h] at hSite; cases hSite
          have h2 : bir.sites.size ≤ LCBWordLemmas.trapCapacity := by
            change bir.sites.size ≤ LCBInput.trapCapacity; have := ctx.hCap; omega
          omega
        have hRt : lcbRuntimeResult bir.project output (trapWord slot.index) =
            some (.trapped 134 entry.kind resolved (renderTrapLine entry.kind resolved).toUTF8) := by
          unfold lcbRuntimeResult trapWord
          dsimp only
          have hNotLt : ¬ (trapWordBits slot.index).toNat < 256 := trapWordBits_not_lt hSlotLt
          have hSub : (trapWordBits slot.index).toNat - 256 = slot.index := trapWordBits_sub hSlotLt
          simp only [hNotLt, ↓reduceIte, hSub, ctx.hSitesEq, hSite, hRtSite,
            bind, Option.bind, pure]
        have hRtObs : lcbRuntimeResult bir.project output (trapWord slot.index) =
            some (.trapped 134 entry.kind resolved
              (trapDiagnostic bir.project entry.kind entry.origin.operation entry.origin.lexical)) := by
          rw [hDiagEq]; exact hRt
        refine ⟨trapWord slot.index, hStepRet, hRtObs, ?_⟩
        change (p.effects.map ProcessEvent.effect ++
          ((BIR.hostedResult (language := sLang) bir p).map
            fun o => o.stderr.data.toList.map ProcessEvent.stderr).getD []) = _
        rw [lcbExecution_trace_eq]
        have hHosted : BIR.hostedResult (language := sLang) bir p =
            some (.trapped 134 entry.kind resolved
              (trapDiagnostic bir.project entry.kind entry.origin.operation entry.origin.lexical)) := by
          rw [birHostedResult_eq]; exact hResOrig
        rw [hHosted, hSim.pure.effects]
        simp only [bind, Option.bind, hStepRet, hRtObs, Option.map_some, Option.getD_some,
          List.map_nil, List.nil_append]

private theorem sim_step_read {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (ctx : LCBCtx bir output)
    {p p' : MemoryRegisters (sliceLanguage slice) profile} {q : Source.State}
    {rk : Nat} {blk : BIRBlock (sliceLoweringLanguage slice)}
    {nb : NormBlock (sliceLoweringLanguage slice)} {lb : Source.Block}
    {op : LocatedOp (SSAInst (sliceLoweringLanguage slice))}
    {ty : EarlyTy} {dst : SSAValue EarlyTy ty}
    {fop : (sliceLoweringLanguage slice).operation .read [ty] ty}
    {arg : SSARead EarlyTy (sliceLoweringLanguage slice).literal ty}
    (hSim : SimInv ctx p q) (hPure' : FramePure bir p')
    (hResNone : birHostedResult (birMemoryView (language := sliceLanguage slice) bir) p = none)
    (hBlkGet : ctx.blocks[rk]? = some blk) (hPBlk : p.block = blk.id) (hQBlk : q.block = ctx.offset + rk)
    (hNormGet : ctx.norm.blocks[q.block]? = some nb) (hBodyGet : output.body.blocks[q.block]? = some lb)
    (hEmitBlk : emitBlock (sliceLanguage slice) ctx.norm (q.block, nb) = .ok lb)
    (hSrcId : nb.sourceId = some blk.id) (hNbP : nb.params = blk.params) (hNbB : nb.body = blk.body)
    (hNormTerm : normalizeTerm ctx.indices ctx.splits q.block blk.ending = .ok nb.term)
    (hPcLt : p.instruction < blk.body.length)
    (hQPos : q.position = ((blk.body.take p.instruction).map fun op => instWidth op.instruction).sum)
    (hOpAt : blk.body[p.instruction]? = some op)
    (hInstEq : op.instruction = .op dst fop (.cons arg .nil) [])
    (hOutcome : (birMemoryView (language := sliceLanguage slice) bir).InstructionStepWith
      p p' false true (birInst ctx.base op.instruction) false) :
    execBIRStep bir p = some p' ∧
    ∃ k q', 0 < k ∧ Source.Steps output.body k q q' ∧ SimInv ctx p' q' := by
  let sLang := sliceLanguage slice
  let sLow := sliceLoweringLanguage slice
  have hBlkAt := ctx.blockAt hSim.pure hBlkGet hPBlk
  have hMOpAt : (blk.body.map (fun op =>
      (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst sLow))))[p.instruction]? =
      some ⟨op.origin, birInst ctx.base op.instruction⟩ := by
    rw [List.getElem?_map, hOpAt]; rfl
  have hNbOpAt : nb.body[p.instruction]? = some op := hNbB.symm ▸ hOpAt
  obtain ⟨xs, hEmitInst, hSlice⟩ := emitBlock_inst_slice hEmitBlk hNbOpAt
  have hSliceQ : ∀ k, k < instWidth op.instruction → lb.instructions[q.position + k]? = xs[k]? := by
    intro k hk; rw [hQPos, ← hNbB]; exact hSlice k hk
  let nBase := blockBase ctx.norm q.block + instBase q.block nb p.instruction
  have hNBaseLt : ∀ off, off < instWidth op.instruction → nBase + off < valueCount ctx.norm := by
    intro off hoff
    have h1 := instBase_add_width_le_blockSize q.block nb hNbOpAt
    have h2 := blockBase_add_size_le_valueCount ctx.norm hNormGet
    omega
  have hQPosSucc : q.position + instWidth op.instruction =
      ((blk.body.take (p.instruction + 1)).map fun o => instWidth o.instruction).sum := by
    rw [hQPos, list_sum_take_add (fun o => instWidth o.instruction) hOpAt]
  rw [hInstEq] at hOutcome hEmitInst hSliceQ hNBaseLt hQPosSucc
  obtain ⟨values, afterReads, _, result, effs, changed, stored, hRead, _, _, hOpSem, _, hPO, hNextEq⟩ := hOutcome
  obtain ⟨_, hEffNil, hStoreEq, hExecOp⟩ := execEarlyOp_of_sliceOperation hOpSem
  simp only [birArgs, Memory.readArgs, bind, Option.bind] at hRead
  cases hR0 : (birMemoryView bir).readValue p (birRead ctx.base arg) with
  | none => simp [hR0] at hRead
  | some pr =>
    rcases pr with ⟨v0, afterR0⟩
    simp only [hR0, pure, Option.some.injEq, Prod.mk.injEq] at hRead
    obtain ⟨rfl, rfl⟩ := hRead
    have hResEqV0 : result = v0 := by
      unfold execEarlyOp at hExecOp
      cases ty with
      | nominal d =>
        cases v0 with
        | other a =>
          dsimp only at hExecOp; split at hExecOp
          · exact (Option.some.inj hExecOp).symm
          · cases hExecOp
        | scalar _ => cases hExecOp
      | scalar sc =>
        cases v0 with
        | scalar sv =>
          dsimp only at hExecOp; split at hExecOp
          · exact (Option.some.inj hExecOp).symm
          · cases hExecOp
        | other _ => cases hExecOp
    cases hResEqV0
    have hPr0 := readValue_virtual_inv hSim.pure hR0
    simp only [List.map_nil, Bool.false_or, ↓reduceIte] at hStoreEq hPO
    rw [← hStoreEq, applyOperation_pure_eq hPr0.1] at hPO
    obtain ⟨hPureS, hBlkS, hInstS, hLkDst, hLkOther⟩ := writeParams_one_reg_inv ctx.hEntry hPr0.1 hPO
    have hP'Eq : p' = stored.advance := by
      subst hEffNil; rw [hNextEq, List.append_nil]; cases stored; rfl
    have hDet : execBIRStep bir p = some p' := by
      unfold execBIRStep; dsimp only
      simp only [hSim.pure.exhausted, hResNone, Option.isSome_none, Bool.or_self,
        Bool.false_eq_true, ↓reduceIte]
      rw [hBlkAt]; dsimp only
      rw [show (blk.body.map (fun op =>
          (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst (sliceLoweringLanguage slice)))))[p.instruction]? =
          some ⟨op.origin, birInst ctx.base op.instruction⟩ from hMOpAt]
      dsimp only
      rw [hInstEq]; dsimp only [birInst, birArgs, List.isEmpty_nil, Memory.readArgs, bind, Option.bind]
      rw [hR0]; dsimp only [pure]
      rw [hExecOp]; dsimp only
      rw [execWriteParams_of_writeParams ctx.hEntry hPr0.1.frame hPr0.1.movedFields
        (fun q hq => by cases List.mem_singleton.mp hq; exact ⟨⟨dst.id, rfl⟩, rfl, rfl⟩) hPO]
      exact congrArg some hP'Eq.symm
    dsimp only [emitInst] at hEmitInst
    simp only [List.isEmpty_nil, Bool.not_true, Bool.false_eq_true, ↓reduceIte, bind, Except.bind] at hEmitInst
    cases hArgEm : emitRead (sliceLanguage slice) ctx.norm arg with
    | error _ => rw [hArgEm] at hEmitInst; cases hEmitInst
    | ok arg' =>
      rw [hArgEm] at hEmitInst; dsimp only [pure, Except.pure] at hEmitInst
      cases Except.ok.inj hEmitInst
      obtain ⟨hV0Rng, hArgRd⟩ := sim_readAgree ctx hSim hArgEm hR0
      have hInst0 : lb.instructions[q.position]? = some (wordIns nBase (.copy arg')) := by
        have := hSliceQ 0 (by dsimp only [instWidth]; omega); simpa using this
      have hEv : Source.evaluate q.environment (wordIns nBase (.copy arg')) = .ok (encVal result) := by
        simp [Source.evaluate, wordIns, hArgRd]
      have hNBaseLtSz : nBase < q.environment.size :=
        hSim.envSz.symm ▸ hNBaseLt 0 (by dsimp only [instWidth]; omega)
      have hW0 : Source.write output.body q.environment nBase (encVal result) =
          .ok (q.environment.set! nBase (some (encVal result))) :=
        write_ok (ctx.bodyWidths (hNBaseLt 0 (by dsimp only [instWidth]; omega))) hNBaseLtSz (encVal_width result)
      have hStep1 : Source.Step output.body q ⟨q.block, q.position + 1,
          q.environment.set! nBase (some (encVal result))⟩ :=
        step_body hBodyGet hInst0 hEv hW0
      refine ⟨hDet, 1, _, Nat.zero_lt_one, .succ hStep1 (.zero _), ?_⟩
      refine ⟨hPure', ⟨rk, blk, nb, lb, hBlkGet, ?_, hQBlk, hNormGet, hBodyGet, hEmitBlk,
        hSrcId, hNbP, hNbB, hNormTerm, ?_, ?_⟩, ?_, ?_⟩
      · simp [hP'Eq, MemoryRegisters.advance, hBlkS, hPr0.2.1, hPBlk]
      · simp [hP'Eq, MemoryRegisters.advance, hInstS, hPr0.2.2.1]; omega
      · simpa [hP'Eq, MemoryRegisters.advance, hInstS, hPr0.2.2.1, instWidth] using hQPosSucc
      · simp only [size_set!, hSim.envSz]
      · intro r v hr
        rw [hP'Eq] at hr
        change AList.lookup stored.virtualValues ⟨ctx.base + r⟩ = some v at hr
        by_cases hEqD : r = dst.id
        · subst hEqD
          rw [hLkDst] at hr; cases Option.some.inj hr
          refine ⟨hV0Rng, fun slot hSlot => ?_⟩
          have hDstSlot : regId ctx.norm dst.id = some nBase := by
            have h := ctx.regIdInst hNormGet hNbOpAt (by rw [hInstEq]; exact List.mem_cons_self)
            simpa [nBase] using h
          rw [hDstSlot] at hSlot; cases Option.some.inj hSlot
          simp only [Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_self, hNBaseLtSz, ↓reduceIte]
        · rw [hLkOther r hEqD] at hr
          have hInP := hPr0.2.2.2.1 r v hr
          obtain ⟨hRng, hAt⟩ := hSim.env r v hInP
          refine ⟨hRng, fun slot hSlot => ?_⟩
          have hSlotNe : nBase ≠ slot := by
            intro hEq
            have hMem := alist_lookup_mem hSlot
            rw [← hEq] at hMem
            have hDef := regIds_at_inst_slot_iff ctx.zeroParams hNormGet hNbOpAt
              (off := 0) (by rw [hInstEq]; dsimp only [instWidth]; omega) (by simpa [nBase] using hMem)
            rw [hInstEq] at hDef
            exact hEqD (Prod.mk.inj (List.mem_singleton.mp hDef)).1
          rw [Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne hSlotNe]
          exact hAt slot hSlot

private theorem sim_step_arith {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (ctx : LCBCtx bir output)
    {p p' : MemoryRegisters (sliceLanguage slice) profile} {q : Source.State}
    {rk : Nat} {blk : BIRBlock (sliceLoweringLanguage slice)}
    {nb : NormBlock (sliceLoweringLanguage slice)} {lb : Source.Block}
    {op : LocatedOp (SSAInst (sliceLoweringLanguage slice))}
    {mOp : MachineOp} {wOp : WrapOp} {t : IntTy}
    {dst : SSAValue EarlyTy (.scalar (.int t))}
    {fop : (sliceLoweringLanguage slice).operation (.arithmetic mOp) [.scalar (.int t), .scalar (.int t)] (.scalar (.int t))}
    {lhs rhs : SSARead EarlyTy (sliceLoweringLanguage slice).literal (.scalar (.int t))}
    (hWOp : wrapOpOfMachineOp mOp = some wOp) (htProf : inWordProfile t = true)
    (hSim : SimInv ctx p q) (hPure' : FramePure bir p')
    (hResNone : birHostedResult (birMemoryView (language := sliceLanguage slice) bir) p = none)
    (hBlkGet : ctx.blocks[rk]? = some blk) (hPBlk : p.block = blk.id) (hQBlk : q.block = ctx.offset + rk)
    (hNormGet : ctx.norm.blocks[q.block]? = some nb) (hBodyGet : output.body.blocks[q.block]? = some lb)
    (hEmitBlk : emitBlock (sliceLanguage slice) ctx.norm (q.block, nb) = .ok lb)
    (hSrcId : nb.sourceId = some blk.id) (hNbP : nb.params = blk.params) (hNbB : nb.body = blk.body)
    (hNormTerm : normalizeTerm ctx.indices ctx.splits q.block blk.ending = .ok nb.term)
    (hPcLt : p.instruction < blk.body.length)
    (hQPos : q.position = ((blk.body.take p.instruction).map fun op => instWidth op.instruction).sum)
    (hOpAt : blk.body[p.instruction]? = some op)
    (hInstEq : op.instruction = .op dst fop (.cons lhs (.cons rhs .nil)) [])
    (hOutcome : (birMemoryView (language := sliceLanguage slice) bir).InstructionStepWith
      p p' false true (birInst ctx.base op.instruction) false) :
    execBIRStep bir p = some p' ∧
    ∃ k q', 0 < k ∧ Source.Steps output.body k q q' ∧ SimInv ctx p' q' := by
  let sLang := sliceLanguage slice
  let sLow := sliceLoweringLanguage slice
  have hBlkAt := ctx.blockAt hSim.pure hBlkGet hPBlk
  have hMOpAt : (blk.body.map (fun op =>
      (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst sLow))))[p.instruction]? =
      some ⟨op.origin, birInst ctx.base op.instruction⟩ := by
    rw [List.getElem?_map, hOpAt]; rfl
  have hNbOpAt : nb.body[p.instruction]? = some op := hNbB.symm ▸ hOpAt
  obtain ⟨xs, hEmitInst, hSlice⟩ := emitBlock_inst_slice hEmitBlk hNbOpAt
  have hSliceQ : ∀ k, k < instWidth op.instruction → lb.instructions[q.position + k]? = xs[k]? := by
    intro k hk; rw [hQPos, ← hNbB]; exact hSlice k hk
  let nBase := blockBase ctx.norm q.block + instBase q.block nb p.instruction
  have hNBaseLt : ∀ off, off < instWidth op.instruction → nBase + off < valueCount ctx.norm := by
    intro off hoff
    have h1 := instBase_add_width_le_blockSize q.block nb hNbOpAt
    have h2 := blockBase_add_size_le_valueCount ctx.norm hNormGet
    omega
  have hQPosSucc : q.position + instWidth op.instruction =
      ((blk.body.take (p.instruction + 1)).map fun o => instWidth o.instruction).sum := by
    rw [hQPos, list_sum_take_add (fun o => instWidth o.instruction) hOpAt]
  rw [hInstEq] at hOutcome hEmitInst hSliceQ hNBaseLt hQPosSucc
  obtain ⟨values, afterReads, _, result, effs, changed, stored, hRead, _, _, hOpSem, _, hPO, hNextEq⟩ := hOutcome
  obtain ⟨_, hEffNil, hStoreEq, hExecOp⟩ := execEarlyOp_of_sliceOperation hOpSem
  simp only [birArgs, Memory.readArgs, bind, Option.bind] at hRead
  cases hR0 : (birMemoryView bir).readValue p (birRead ctx.base lhs) with
  | none => simp [hR0] at hRead
  | some pr0 =>
    rcases pr0 with ⟨lv, afterR0⟩
    simp only [hR0] at hRead
    cases hR1 : (birMemoryView bir).readValue afterR0 (birRead ctx.base rhs) with
    | none => simp [hR1] at hRead
    | some pr1 =>
      rcases pr1 with ⟨rv, afterR1⟩
      simp only [hR1, pure, Option.some.injEq, Prod.mk.injEq] at hRead
      obtain ⟨rfl, rfl⟩ := hRead
      have hArith : mOp = .plain .add ∧ wOp = .add ∧
          ∃ x y : Int, lv = .scalar (.int t x) ∧ rv = .scalar (.int t y) ∧
            t.InRange (x + y) ∧ result = .scalar (.int t (x + y)) := by
        unfold execEarlyOp at hExecOp
        cases mOp with
        | wrapping _ | saturating _ => cases hExecOp
        | plain ao =>
          cases ao with
          | sub | mul | div | rem | shl | shr => cases hExecOp
          | add =>
            have hWAdd : wOp = .add := Option.some.inj hWOp.symm
            cases lv with
            | other _ => cases hExecOp
            | scalar sl =>
              cases sl with
              | bool _ | char _ | unit => cases hExecOp
              | int lt lx =>
                cases rv with
                | other _ => cases hExecOp
                | scalar sr =>
                  cases sr with
                  | bool _ | char _ | unit => cases hExecOp
                  | int rt rx =>
                    dsimp only at hExecOp
                    split at hExecOp
                    · rename_i hc
                      obtain ⟨_, _, rfl, rfl⟩ := hc
                      unfold plain checked inRangeOr at hExecOp
                      dsimp only at hExecOp
                      by_cases hInR : rt.InRange (lx + rx)
                      · simp only [hInR, ↓reduceIte] at hExecOp
                        cases Option.some.inj hExecOp
                        exact ⟨rfl, hWAdd, lx, rx, rfl, rfl, hInR, rfl⟩
                      · simp only [hInR, ↓reduceIte] at hExecOp; cases hExecOp
                    · cases hExecOp
      obtain ⟨rfl, rfl, x, y, rfl, rfl, hSumInR, rfl⟩ := hArith
      have hPr0 := readValue_virtual_inv hSim.pure hR0
      have hPr1 := readValue_virtual_inv hPr0.1 hR1
      simp only [List.map_nil, Bool.false_or, ↓reduceIte] at hStoreEq hPO
      rw [← hStoreEq, applyOperation_pure_eq hPr1.1] at hPO
      obtain ⟨hPureS, hBlkS, hInstS, hLkDst, hLkOther⟩ := writeParams_one_reg_inv ctx.hEntry hPr1.1 hPO
      have hP'Eq : p' = stored.advance := by
        subst hEffNil; rw [hNextEq, List.append_nil]; cases stored; rfl
      have hDet : execBIRStep bir p = some p' := by
        unfold execBIRStep; dsimp only
        simp only [hSim.pure.exhausted, hResNone, Option.isSome_none, Bool.or_self,
          Bool.false_eq_true, ↓reduceIte]
        rw [hBlkAt]; dsimp only
        rw [show (blk.body.map (fun op =>
            (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst (sliceLoweringLanguage slice)))))[p.instruction]? =
            some ⟨op.origin, birInst ctx.base op.instruction⟩ from hMOpAt]
        dsimp only
        rw [hInstEq]; dsimp only [birInst, birArgs, List.isEmpty_nil, Memory.readArgs, bind, Option.bind]
        rw [hR0]; dsimp only
        rw [hR1]; dsimp only [pure]
        rw [hExecOp]; dsimp only
        rw [execWriteParams_of_writeParams ctx.hEntry hPr1.1.frame hPr1.1.movedFields
          (fun q hq => by cases List.mem_singleton.mp hq; exact ⟨⟨dst.id, rfl⟩, rfl, rfl⟩) hPO]
        exact congrArg some hP'Eq.symm
      dsimp only [emitInst, List.isEmpty_nil, wrapOpOfMachineOp, bind, Except.bind] at hEmitInst
      have hSc : (sliceLanguage slice).scalarType (.scalar (.int t)) = some (.int t) := rfl
      rw [hSc] at hEmitInst; dsimp only at hEmitInst
      rw [htProf] at hEmitInst
      simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte] at hEmitInst
      cases hLhsEm : emitRead (sliceLanguage slice) ctx.norm lhs with
      | error _ => rw [hLhsEm] at hEmitInst; cases hEmitInst
      | ok lhs' =>
        rw [hLhsEm] at hEmitInst; dsimp only at hEmitInst
        cases hRhsEm : emitRead (sliceLanguage slice) ctx.norm rhs with
        | error _ => rw [hRhsEm] at hEmitInst; cases hEmitInst
        | ok rhs' =>
          rw [hRhsEm] at hEmitInst; dsimp only [pure, Except.pure] at hEmitInst
          cases Except.ok.inj hEmitInst
          have hLhsRd : Source.read q.environment .word lhs' = .ok (word x) :=
            (sim_readAgree ctx hSim hLhsEm hR0).2
          have hRhsRd : Source.read q.environment .word rhs' = .ok (word y) := by
            have hSimAfterR0 : SimInv ctx afterR0 q :=
              ⟨hPr0.1, ⟨rk, blk, nb, lb, hBlkGet, hPr0.2.1.trans hPBlk, hQBlk, hNormGet,
                hBodyGet, hEmitBlk, hSrcId, hNbP, hNbB, hNormTerm, hPr0.2.2.1.symm ▸ (Nat.le_of_lt hPcLt),
                hPr0.2.2.1.symm ▸ hQPos⟩, hSim.envSz,
                fun r v hr => hSim.env r v (hPr0.2.2.2.1 r v hr)⟩
            exact (sim_readAgree ctx hSimAfterR0 hRhsEm hR1).2
          have hBound3 : nBase + 3 < q.environment.size :=
            hSim.envSz.symm ▸ hNBaseLt 3 (by dsimp only [instWidth]; omega)
          have hSteps4 := wrap_steps (s := q) (op := .add) (t := t) (n := nBase)
            htProf hBodyGet (fun k hk => hSliceQ k (by dsimp only [instWidth]; exact hk))
            hLhsRd hRhsRd (fun m hm => ctx.bodyWidths (hSim.envSz ▸ hm)) hBound3
          have hWrapEq : wrapping t .add x y = x + y :=
            wrapping_add_eq_of_no_overflow t x y ((checked_add_no_overflow_iff t x y).mpr hSumInR)
          rw [hWrapEq] at hSteps4
          refine ⟨hDet, 4, _, by decide, hSteps4, ?_⟩
          refine ⟨hPure', ⟨rk, blk, nb, lb, hBlkGet, ?_, hQBlk, hNormGet, hBodyGet, hEmitBlk,
            hSrcId, hNbP, hNbB, hNormTerm, ?_, ?_⟩, ?_, ?_⟩
          · simp [hP'Eq, MemoryRegisters.advance, hBlkS, hPr1.2.1, hPr0.2.1, hPBlk]
          · simp [hP'Eq, MemoryRegisters.advance, hInstS, hPr1.2.2.1, hPr0.2.2.1]; omega
          · simpa [hP'Eq, MemoryRegisters.advance, hInstS, hPr1.2.2.1, hPr0.2.2.1, instWidth] using hQPosSucc
          · simp [size_set4, hSim.envSz]
          · intro r v hr
            rw [hP'Eq] at hr
            change AList.lookup stored.virtualValues ⟨ctx.base + r⟩ = some v at hr
            by_cases hEqD : r = dst.id
            · subst hEqD
              rw [hLkDst] at hr; cases Option.some.inj hr
              refine ⟨hSumInR, fun slot hSlot => ?_⟩
              have hDstSlot : regId ctx.norm dst.id = some (nBase + 3) :=
                ctx.regIdInst hNormGet hNbOpAt (by rw [hInstEq]; exact List.mem_cons_self)
              rw [hDstSlot] at hSlot; cases Option.some.inj hSlot
              rw [set4, Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_self_of_lt
                (by simp only [size_set!]; exact hBound3)]
              rfl
            · rw [hLkOther r hEqD] at hr
              have hInP := hPr0.2.2.2.1 r v (hPr1.2.2.2.1 r v hr)
              obtain ⟨hRng, hAt⟩ := hSim.env r v hInP
              refine ⟨hRng, fun slot hSlot => ?_⟩
              have hNotIn : ∀ off, off < 4 → nBase + off ≠ slot := by
                intro off hoff hEq
                have hMem := alist_lookup_mem hSlot
                rw [← hEq] at hMem
                have hDef := regIds_at_inst_slot_iff ctx.zeroParams hNormGet hNbOpAt
                  (off := off) (by rw [hInstEq]; exact hoff) hMem
                rw [hInstEq] at hDef
                exact hEqD (Prod.mk.inj (List.mem_singleton.mp hDef)).1
              dsimp only [set4]
              rw [Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (hNotIn 3 (by decide)),
                Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (hNotIn 2 (by decide)),
                Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (hNotIn 1 (by decide)),
                Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (by simpa using hNotIn 0 (by decide))]
              exact hAt slot hSlot

private theorem sim_step_addWithOverflow {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (ctx : LCBCtx bir output)
    {p p' : MemoryRegisters (sliceLanguage slice) profile} {q : Source.State}
    {rk : Nat} {blk : BIRBlock (sliceLoweringLanguage slice)}
    {nb : NormBlock (sliceLoweringLanguage slice)} {lb : Source.Block}
    {op : LocatedOp (SSAInst (sliceLoweringLanguage slice))}
    {t : IntTy} {dst : SSAValue (sliceLanguage slice).operands.Ty ((sliceLoweringLanguage slice).intTy t)}
    {flag : SSAValue (sliceLanguage slice).operands.Ty ((sliceLoweringLanguage slice).bool)}
    {lhs rhs : SSARead (sliceLanguage slice).operands.Ty (sliceLoweringLanguage slice).literal ((sliceLoweringLanguage slice).intTy t)}
    (htProf : inWordProfile t = true) (hNe : dst.id ≠ flag.id)
    (hSim : SimInv ctx p q) (hPure' : FramePure bir p')
    (hResNone : birHostedResult (birMemoryView (language := sliceLanguage slice) bir) p = none)
    (hBlkGet : ctx.blocks[rk]? = some blk) (hPBlk : p.block = blk.id) (hQBlk : q.block = ctx.offset + rk)
    (hNormGet : ctx.norm.blocks[q.block]? = some nb) (hBodyGet : output.body.blocks[q.block]? = some lb)
    (hEmitBlk : emitBlock (sliceLanguage slice) ctx.norm (q.block, nb) = .ok lb)
    (hSrcId : nb.sourceId = some blk.id) (hNbP : nb.params = blk.params) (hNbB : nb.body = blk.body)
    (hNormTerm : normalizeTerm ctx.indices ctx.splits q.block blk.ending = .ok nb.term)
    (hPcLt : p.instruction < blk.body.length)
    (hQPos : q.position = ((blk.body.take p.instruction).map fun op => instWidth op.instruction).sum)
    (hOpAt : blk.body[p.instruction]? = some op)
    (hInstEq : op.instruction = .addWithOverflow t dst flag lhs rhs)
    (hOutcome : (birMemoryView (language := sliceLanguage slice) bir).InstructionStepWith
      p p' false true (birInst ctx.base op.instruction) false) :
    execBIRStep bir p = some p' ∧
    ∃ k q', 0 < k ∧ Source.Steps output.body k q q' ∧ SimInv ctx p' q' := by
  let sLang := sliceLanguage slice
  let sLow := sliceLoweringLanguage slice
  have hBlkAt := ctx.blockAt hSim.pure hBlkGet hPBlk
  have hMOpAt : (blk.body.map (fun op =>
      (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst sLow))))[p.instruction]? =
      some ⟨op.origin, birInst ctx.base op.instruction⟩ := by
    rw [List.getElem?_map, hOpAt]; rfl
  have hNbOpAt : nb.body[p.instruction]? = some op := hNbB.symm ▸ hOpAt
  obtain ⟨xs, hEmitInst, hSlice⟩ := emitBlock_inst_slice hEmitBlk hNbOpAt
  have hSliceQ : ∀ k, k < instWidth op.instruction → lb.instructions[q.position + k]? = xs[k]? := by
    intro k hk; rw [hQPos, ← hNbB]; exact hSlice k hk
  let nBase := blockBase ctx.norm q.block + instBase q.block nb p.instruction
  have hNBaseLt : ∀ off, off < instWidth op.instruction → nBase + off < valueCount ctx.norm := by
    intro off hoff
    have h1 := instBase_add_width_le_blockSize q.block nb hNbOpAt
    have h2 := blockBase_add_size_le_valueCount ctx.norm hNormGet
    omega
  have hQPosSucc : q.position + instWidth op.instruction =
      ((blk.body.take (p.instruction + 1)).map fun o => instWidth o.instruction).sum := by
    rw [hQPos, list_sum_take_add (fun o => instWidth o.instruction) hOpAt]
  rw [hInstEq] at hOutcome hEmitInst hSliceQ hNBaseLt hQPosSucc
  obtain ⟨_, _, _, lv, afterLhs, rv, afterReads, x, y, stored, hR0, hR1, hLvEq, hRvEq, hPO, hNextEq⟩ := hOutcome
  change lv = .scalar (.int t x) at hLvEq
  change rv = .scalar (.int t y) at hRvEq
  subst hLvEq; subst hRvEq
  have hPr0 := readValue_virtual_inv hSim.pure hR0
  have hPr1 := readValue_virtual_inv hPr0.1 hR1
  obtain ⟨hPureS, hBlkS, hInstS, hLkDst, hLkFlag, hLkOther⟩ :=
    writeParams_two_regs_inv ctx.hEntry hPr1.1 hNe hPO
  have hRootNeBool : ((birReg ctx.base dst).logical.root == (birReg ctx.base flag).logical.root) = false := by
    apply beq_false_of_ne
    intro hEq
    exact hNe (Nat.add_left_cancel (PlaceId.mk.inj hEq))
  have hDet : execBIRStep bir p = some p' := by
    unfold execBIRStep; dsimp only
    simp only [hSim.pure.exhausted, hResNone, Option.isSome_none, Bool.or_self,
      Bool.false_eq_true, ↓reduceIte]
    rw [hBlkAt]; dsimp only
    rw [show (blk.body.map (fun op =>
        (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst (sliceLoweringLanguage slice)))))[p.instruction]? =
        some ⟨op.origin, birInst ctx.base op.instruction⟩ from hMOpAt]
    dsimp only
    rw [hInstEq]; dsimp only [birInst]
    rw [hRootNeBool]; simp only [Bool.false_eq_true, ↓reduceIte]
    rw [hR0]; dsimp only
    rw [hR1]; dsimp only
    simp only [beq_self_eq_true, Bool.and_self, ↓reduceIte]
    have hPO' : (birMemoryView (language := sliceLanguage slice) bir).WriteParams
        [⟨(sliceLoweringLanguage slice).intTy t, birReg ctx.base dst⟩,
         ⟨(sliceLoweringLanguage slice).bool, birReg ctx.base flag⟩]
        [CoreValue.scalar (.int t (wrapping t .add x y)),
         CoreValue.scalar (.bool (decide (checked t .add x y = .error .overflow)))]
        afterReads stored false := hPO
    rw [execWriteParams_of_writeParams ctx.hEntry hPr1.1.frame hPr1.1.movedFields
      (fun q hq => by
        simp only [List.mem_cons, List.not_mem_nil, or_false] at hq
        rcases hq with rfl | rfl
        · exact ⟨⟨dst.id, rfl⟩, rfl, rfl⟩
        · exact ⟨⟨flag.id, rfl⟩, rfl, rfl⟩) hPO']
    exact congrArg some hNextEq.symm
  dsimp only [emitInst, bind, Except.bind] at hEmitInst
  rw [htProf] at hEmitInst
  simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte] at hEmitInst
  cases hLhsEm : emitRead (sliceLanguage slice) ctx.norm lhs with
  | error _ => simp only [hLhsEm] at hEmitInst; cases hEmitInst
  | ok lhs' =>
    simp only [hLhsEm] at hEmitInst
    cases hRhsEm : emitRead (sliceLanguage slice) ctx.norm rhs with
    | error _ => simp only [hRhsEm] at hEmitInst; cases hEmitInst
    | ok rhs' =>
      simp only [hRhsEm, pure, Except.pure] at hEmitInst
      cases Except.ok.inj hEmitInst
      obtain ⟨hxInR, hLhsRd⟩ := sim_readAgree ctx hSim hLhsEm hR0
      have hSimAfterR0 : SimInv ctx afterLhs q :=
        ⟨hPr0.1, ⟨rk, blk, nb, lb, hBlkGet, hPr0.2.1.trans hPBlk, hQBlk, hNormGet,
          hBodyGet, hEmitBlk, hSrcId, hNbP, hNbB, hNormTerm, hPr0.2.2.1.symm ▸ (Nat.le_of_lt hPcLt),
          hPr0.2.2.1.symm ▸ hQPos⟩, hSim.envSz,
          fun r v hr => hSim.env r v (hPr0.2.2.2.1 r v hr)⟩
      obtain ⟨hyInR, hRhsRd⟩ := sim_readAgree ctx hSimAfterR0 hRhsEm hR1
      have hBound5 : nBase + 5 < q.environment.size :=
        hSim.envSz.symm ▸ hNBaseLt 5 (by dsimp only [instWidth]; omega)
      obtain ⟨u4, hSteps6⟩ := checkedAdd_steps (s := q) (t := t) (n := nBase)
        htProf hxInR hyInR hBodyGet
        (fun k hk => hSliceQ k (by dsimp only [instWidth]; exact hk))
        hLhsRd hRhsRd (fun m hm => ctx.bodyWidths (hSim.envSz ▸ hm)) hBound5
      refine ⟨hDet, 6, _, by decide, hSteps6, ?_⟩
      refine ⟨hPure', ⟨rk, blk, nb, lb, hBlkGet, ?_, hQBlk, hNormGet, hBodyGet, hEmitBlk,
        hSrcId, hNbP, hNbB, hNormTerm, ?_, ?_⟩, ?_, ?_⟩
      · simp [hNextEq, MemoryRegisters.advance, hBlkS, hPr1.2.1, hPr0.2.1, hPBlk]
      · simp [hNextEq, MemoryRegisters.advance, hInstS, hPr1.2.2.1, hPr0.2.2.1]; omega
      · simpa [hNextEq, MemoryRegisters.advance, hInstS, hPr1.2.2.1, hPr0.2.2.1, instWidth] using hQPosSucc
      · simp only [size_set!, size_set4, hSim.envSz]
      · intro r v hr
        rw [hNextEq] at hr
        change AList.lookup stored.virtualValues ⟨ctx.base + r⟩ = some v at hr
        by_cases hEqD : r = dst.id
        · subst hEqD
          rw [hLkDst] at hr; cases Option.some.inj hr
          refine ⟨wrapping_add_inRange t x y, fun slot hSlot => ?_⟩
          have hDstSlot : regId ctx.norm dst.id = some (nBase + 3) :=
            ctx.regIdInst hNormGet hNbOpAt (by rw [hInstEq]; exact List.mem_cons_self)
          rw [hDstSlot] at hSlot; cases Option.some.inj hSlot
          rw [Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (by omega),
            Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (by omega)]
          rw [set4, Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_self_of_lt
            (by simp only [size_set!]; omega)]
          rfl
        · by_cases hEqF : r = flag.id
          · subst hEqF
            rw [hLkFlag] at hr; cases Option.some.inj hr
            refine ⟨trivial, fun slot hSlot => ?_⟩
            have hFlagSlot : regId ctx.norm flag.id = some (nBase + 5) :=
              ctx.regIdInst hNormGet hNbOpAt (by rw [hInstEq]; exact List.mem_cons_of_mem _ List.mem_cons_self)
            rw [hFlagSlot] at hSlot; cases Option.some.inj hSlot
            rw [Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_self_of_lt
              (by simp only [size_set!, size_set4]; exact hBound5)]
            rfl
          · rw [hLkOther r hEqD hEqF] at hr
            have hInP := hPr0.2.2.2.1 r v (hPr1.2.2.2.1 r v hr)
            obtain ⟨hRng, hAt⟩ := hSim.env r v hInP
            refine ⟨hRng, fun slot hSlot => ?_⟩
            have hNotIn : ∀ off, off < 6 → nBase + off ≠ slot := by
              intro off hoff hEq
              have hMem := alist_lookup_mem hSlot
              rw [← hEq] at hMem
              have hDef := regIds_at_inst_slot_iff ctx.zeroParams hNormGet hNbOpAt
                (off := off) (by rw [hInstEq]; exact hoff) hMem
              rw [hInstEq] at hDef
              simp only [instDefs, List.mem_cons, List.not_mem_nil, or_false, Prod.mk.injEq] at hDef
              rcases hDef with ⟨hR, _⟩ | ⟨hR, _⟩
              · exact hEqD hR
              · exact hEqF hR
            dsimp only [set4]
            rw [Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (hNotIn 5 (by decide)),
              Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (hNotIn 4 (by decide)),
              Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (hNotIn 3 (by decide)),
              Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (hNotIn 2 (by decide)),
              Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (hNotIn 1 (by decide)),
              Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (by simpa using hNotIn 0 (by decide))]
            exact hAt slot hSlot

private theorem sim_doTransfer {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (ctx : LCBCtx bir output)
    {st p' : MemoryRegisters (sliceLanguage slice) profile} {q : Source.State}
    {rk : Nat} {blk : BIRBlock (sliceLoweringLanguage slice)}
    {edge : BIREdge (sliceLoweringLanguage slice)} {predIdx : Nat}
    {pNb : NormBlock (sliceLoweringLanguage slice)}
    (hBlkGet : ctx.blocks[rk]? = some blk)
    (hEdgeMem : edge ∈ blk.ending.successors)
    (hSimSt : SimInv ctx st q)
    (hTr : (birMemoryView (language := sliceLanguage slice) bir).Transfer
      ⟨edge.target, edge.arguments.map (birOpnd ctx.base)⟩ st p' false)
    (hPNb : ctx.norm.blocks[predIdx]? = some pNb)
    {tgtRk : Nat} {tgtBlk : BIRBlock (sliceLoweringLanguage slice)}
    {tgtNb : NormBlock (sliceLoweringLanguage slice)} {tgtLb : Source.Block}
    (hTgtGet : ctx.blocks[tgtRk]? = some tgtBlk) (hTgtId : tgtBlk.id = edge.target)
    (hTgtNorm : ctx.norm.blocks[ctx.offset + tgtRk]? = some tgtNb)
    (hTgtBody : output.body.blocks[ctx.offset + tgtRk]? = some tgtLb)
    (hTgtEmit : emitBlock (sliceLanguage slice) ctx.norm (ctx.offset + tgtRk, tgtNb) = .ok tgtLb)
    (hTgtSrc : tgtNb.sourceId = some tgtBlk.id) (hTgtP : tgtNb.params = tgtBlk.params)
    (hTgtB : tgtNb.body = tgtBlk.body)
    (hTgtNT : normalizeTerm ctx.indices ctx.splits (ctx.offset + tgtRk) tgtBlk.ending = .ok tgtNb.term)
    (hUniqEdge : tgtBlk.params ≠ [] →
      edgesTo (ctx.offset + tgtRk) predIdx pNb.term = [(predIdx, .reads edge.arguments)]) :
    ∃ q', Source.transfer output.body predIdx (ctx.offset + tgtRk) q.environment = .ok q' ∧
      SimInv ctx p' q' := by
  let sLang := sliceLanguage slice
  obtain ⟨values, afterRead, mTgtBlk, hRdOps, hTgtAt, _, hPO⟩ := hTr
  have hTgtMemGraph := (ctx.hBlocksSub tgtBlk (List.mem_of_getElem? hTgtGet)).1
  have hTgtBlockAt := ctx.blockAt
    (p := {st with block := edge.target})
    ⟨hSimSt.pure.frame, hSimSt.pure.callers, hSimSt.pure.exhausted, hSimSt.pure.effects,
      hSimSt.pure.materialized, hSimSt.pure.zeroValues, hSimSt.pure.addressIds,
      hSimSt.pure.movedFields, hSimSt.pure.bytes⟩ hTgtGet hTgtId.symm
  rw [hTgtBlockAt] at hTgtAt; cases Option.some.inj hTgtAt
  obtain ⟨hPureAR, _, _, hValsLen, hSubAR, hValsAt⟩ := readOperands_virtual_inv hSimSt.pure hRdOps
  have hStartPure : FramePure bir {afterRead with block := edge.target, instruction := 0} :=
    ⟨hPureAR.frame, hPureAR.callers, hPureAR.exhausted, hPureAR.effects, hPureAR.materialized,
      hPureAR.zeroValues, hPureAR.addressIds, hPureAR.movedFields, hPureAR.bytes⟩
  obtain ⟨hPureEnd, hBlkEnd, hInstEnd, hValsLenTgt, hLkParams, hLkOther⟩ :=
    writeParams_regs_inv ctx.hEntry hStartPure (ctx.hParamsNd tgtBlk hTgtMemGraph) hPO
  have hTgtIdxNe0 : ctx.offset + tgtRk ≠ 0 := by
    by_cases hIsE : tgtBlk.id = ctx.graph.entry
    · have hHasInc : ctx.entryHasPreds = true := by
        unfold LCBCtx.entryHasPreds hasIncomingEdge
        exact List.any_eq_true.mpr ⟨blk, List.mem_of_getElem? hBlkGet,
          List.any_eq_true.mpr ⟨edge, hEdgeMem, beq_iff_eq.mpr (hTgtId.symm.trans hIsE)⟩⟩
      have hOff1 : ctx.offset = 1 := by simp [LCBCtx.offset, hHasInc]
      omega
    · have hrkNe0 : tgtRk ≠ 0 := by
        intro h0; subst h0
        rw [ctx.hBlocksCons] at hTgtGet; cases Option.some.inj hTgtGet
        exact hIsE ctx.hEntryBlkId
      omega
  obtain ⟨phis, _, _, hPhisOk, _, _, hTgtLbEq⟩ := emitBlock_ok_inv hTgtEmit
  have hPhisEq : tgtLb.phis = phis.toArray := by rw [hTgtLbEq]
  simp only [beq_false_of_ne hTgtIdxNe0, Bool.false_eq_true, ↓reduceIte] at hPhisOk
  obtain ⟨hPhisLen0, hPhisEl⟩ := mapM_except_getElem? hPhisOk
  have hPhisLen : phis.length = tgtBlk.params.length := by
    rw [hPhisLen0, indexed_length, hTgtP]
  let w : Source.Phi → Source.Value := fun phi =>
    match values[phi.destination - blockBase ctx.norm (ctx.offset + tgtRk)]? with
    | some v => encVal v
    | none => word 0
  have hPhiAt : ∀ k (hk : k < phis.length),
      ∃ phi, phis[k]? = some phi ∧
        phi.destination = blockBase ctx.norm (ctx.offset + tgtRk) + k ∧
        phi.width = .word ∧
        emitPhi sLang ctx.norm (ctx.offset + tgtRk) k = .ok phi := by
    intro k hk
    have hkP : k < tgtNb.params.length := by rw [hTgtP]; omega
    have hPIdx : (indexed tgtNb.params)[k]? = some (k, tgtNb.params[k]) := by
      rw [Backend.Runtime.getElem?_indexed, List.getElem?_eq_getElem hkP]; rfl
    obtain ⟨phi, hPhiGet, hPhiEm⟩ := hPhisEl hPIdx
    have hDest : phi.destination = blockBase ctx.norm (ctx.offset + tgtRk) + k ∧ phi.width = .word := by
      unfold emitPhi at hPhiEm
      dsimp only [bind, Except.bind, pure, Except.pure] at hPhiEm
      split at hPhiEm
      · cases hPhiEm
      · cases Except.ok.inj hPhiEm
        exact ⟨rfl, rfl⟩
    exact ⟨phi, hPhiGet, hDest.1, hDest.2, hPhiEm⟩
  have hPhisToList : tgtLb.phis.toList = phis := by rw [hPhisEq]
  have hPhiCond : ∀ i (hi : i < tgtLb.phis.toList.length), ∃ x opnd,
      tgtLb.phis.toList[i].incoming.filter (·.1 == predIdx) = #[(x, opnd)] ∧
      Source.read q.environment tgtLb.phis.toList[i].width opnd = .ok (w tgtLb.phis.toList[i]) := by
    intro i hi
    revert hi
    rw [hPhisToList]
    intro hi
    obtain ⟨phi, hPhiGet, hDest, hWidth, hPhiEm⟩ := hPhiAt i hi
    have hEq : phis[i] = phi := Option.some.inj ((List.getElem?_eq_getElem hi).symm.trans hPhiGet)
    rw [hEq]
    have hParamsNe : tgtBlk.params ≠ [] := by
      intro hNil; rw [hNil] at hPhisLen; dsimp only [List.length_nil] at hPhisLen; omega
    have hEdgesFilt := incomingEdges_filter ctx.norm (ctx.offset + tgtRk) predIdx hPNb
    rw [hUniqEdge hParamsNe] at hEdgesFilt
    let emitArm : Nat × PhiEdgeArg (sliceLoweringLanguage slice) → Except StageFailure Source.Operand :=
      fun p => match p.2 with
        | .entryParam count =>
          if i < count then .ok (.local i) else .error (.invalidInput invalidInputRule)
        | .reads args =>
          match args[i]? with
          | some ⟨_, read⟩ => emitRead (sliceLanguage slice) ctx.norm read
          | none => .error (.invalidInput invalidInputRule)
    have hEmitPhiBody : emitPhi (sliceLanguage slice) ctx.norm (ctx.offset + tgtRk) i =
        (do
          let arms ← (incomingEdges ctx.norm (ctx.offset + tgtRk)).mapM (fun a => do
            let b ← emitArm a
            return (a.1, b))
          return { destination := blockBase ctx.norm (ctx.offset + tgtRk) + i,
                   width := .word, incoming := arms.toArray }) := by
      unfold emitPhi
      congr 1
      congr 1
      funext ⟨predIdx, edgeArg⟩
      cases edgeArg with
      | entryParam count => dsimp only [emitArm]; split <;> rfl
      | reads args => dsimp only [emitArm]; cases args[i]? <;> rfl
    rw [hEmitPhiBody] at hPhiEm
    cases hArms : (incomingEdges ctx.norm (ctx.offset + tgtRk)).mapM (fun a => do
        let b ← emitArm a
        return (a.1, b)) with
    | error _ => rw [hArms] at hPhiEm; cases hPhiEm
    | ok arms =>
      rw [hArms] at hPhiEm
      cases Except.ok.inj hPhiEm
      have hFiltMap := filter_mapM_except_fst hArms (· == predIdx)
      rw [hEdgesFilt] at hFiltMap
      simp only [List.mapM_cons, List.mapM_nil, emitArm] at hFiltMap
      have hiLtArgs : i < edge.arguments.length := by omega
      have hArgGet : edge.arguments[i]? = some edge.arguments[i] := List.getElem?_eq_getElem hiLtArgs
      rw [hArgGet] at hFiltMap
      cases hRdEm : emitRead (sliceLanguage slice) ctx.norm edge.arguments[i].2 with
      | error _ => simp [hRdEm, bind, Except.bind] at hFiltMap
      | ok opnd =>
        simp only [hRdEm, bind, Except.bind, pure, Except.pure, Except.ok.injEq] at hFiltMap
        have hArrFilt : arms.toArray.filter (·.1 == predIdx) = #[(predIdx, opnd)] := by
          rw [← Array.toList_inj]
          simpa only [Array.toList_filter] using hFiltMap.symm
        have hiLtVals : i < values.length := by omega
        have hValGet : values[i]? = some values[i] := List.getElem?_eq_getElem hiLtVals
        have hValSpec := hValsAt i edge.arguments[i] values[i] hArgGet hValGet
        have hSrcRd : Source.read q.environment .word opnd = .ok (encVal values[i]) := by
          rcases hArm : edge.arguments[i] with ⟨rty, read⟩
          rw [hArm] at hRdEm hValSpec
          cases read with
          | literal lit =>
            dsimp only [emitRead] at hRdEm
            rw [decodeLiteralWord_slice (slice := slice) lit, hValSpec] at hRdEm
            dsimp only [Option.map] at hRdEm
            cases Except.ok.inj hRdEm
            simp only [Source.read, encVal_width values[i], word_bne, Bool.false_eq_true, ↓reduceIte,
              bind, Except.bind, pure, Except.pure]
          | value reg =>
            dsimp only [emitRead] at hRdEm
            cases hSlot : regId ctx.norm reg.id with
            | none => rw [hSlot] at hRdEm; cases hRdEm
            | some slot =>
              rw [hSlot] at hRdEm; dsimp only at hRdEm
              cases Except.ok.inj hRdEm
              have hAt := (hSimSt.env reg.id values[i] hValSpec).2 slot hSlot
              simp only [Source.read, hAt, encVal_width values[i], word_bne, Bool.false_eq_true, ↓reduceIte,
                bind, Except.bind, pure, Except.pure]
        refine ⟨predIdx, opnd, hArrFilt, ?_⟩
        have hWVal : w ⟨blockBase ctx.norm (ctx.offset + tgtRk) + i, .word, arms.toArray⟩ = encVal values[i] := by
          dsimp only [w]; rw [Nat.add_sub_cancel_left, hValGet]
        rw [hWVal]; exact hSrcRd
  have hDestNodup : (tgtLb.phis.toList.map (·.destination)).Nodup := by
    rw [hPhisToList]
    have hDestsNdGen : ∀ (b : Nat) (xs : List Source.Phi),
        (∀ k phi, xs[k]? = some phi → phi.destination = b + k) →
        (xs.map (·.destination)).Nodup := by
      intro b xs
      induction xs generalizing b with
      | nil => intro _; exact List.nodup_nil
      | cons a rs ih =>
        intro h
        rw [List.map_cons, List.nodup_cons]
        refine ⟨fun hm => ?_, ih (b + 1) (fun k phi hk => by
          have h1 := h (k + 1) phi hk; omega)⟩
        obtain ⟨p0, hp0, hEq⟩ := List.mem_map.mp hm
        obtain ⟨k, hk⟩ := List.mem_iff_getElem?.mp hp0
        have ha := h 0 a rfl
        have hp2 := h (k + 1) p0 hk
        rw [ha, hp2] at hEq
        exact Nat.succ_ne_zero k (Nat.add_left_cancel hEq)
    refine hDestsNdGen (blockBase ctx.norm (ctx.offset + tgtRk)) phis fun k phi hk => ?_
    have hkLt : k < phis.length := by
      rcases Nat.lt_or_ge k phis.length with h | h
      · exact h
      · rw [List.getElem?_eq_none h] at hk; cases hk
    obtain ⟨phi', hPhi'Get, hDest, _, _⟩ := hPhiAt k hkLt
    rw [hPhi'Get] at hk; cases Option.some.inj hk
    exact hDest
  have hPhiW : ∀ phi ∈ tgtLb.phis.toList, output.body.valueWidths[phi.destination]? = some .word ∧
      phi.destination < q.environment.size ∧ (w phi).width = .word := by
    intro phi hPhiMem
    rw [hPhisToList] at hPhiMem
    obtain ⟨k, hPhiGet⟩ := List.mem_iff_getElem?.mp hPhiMem
    have hk : k < phis.length := by
      rcases Nat.lt_or_ge k phis.length with h | h
      · exact h
      · rw [List.getElem?_eq_none h] at hPhiGet; cases hPhiGet
    obtain ⟨phi', hPhi'Get, hDest, _, _⟩ := hPhiAt k hk
    rw [hPhi'Get] at hPhiGet; cases Option.some.inj hPhiGet
    have hLtVC : phi.destination < valueCount ctx.norm := by
      have h1 := blockBase_add_size_le_valueCount ctx.norm hTgtNorm
      have hPhiCnt : blockPhiCount (ctx.offset + tgtRk) tgtNb = tgtBlk.params.length := by
        unfold blockPhiCount; simp [beq_false_of_ne hTgtIdxNe0, hTgtP]
      have hkP : k < tgtBlk.params.length := hPhisLen ▸ hk
      unfold blockSize at h1
      rw [hPhiCnt] at h1
      rw [hDest]
      exact Nat.lt_of_lt_of_le (Nat.add_lt_add_left hkP _)
        (Nat.le_trans (Nat.add_le_add_left (Nat.le_add_right _ _) _) h1)
    refine ⟨ctx.bodyWidths hLtVC, hSimSt.envSz.symm ▸ hLtVC, ?_⟩
    dsimp only [w]
    cases values[phi.destination - blockBase ctx.norm (ctx.offset + tgtRk)]? with
    | none => rfl
    | some v => exact encVal_width v
  have hTrOk := transfer_ok hTgtBody w hPhiCond hDestNodup hPhiW
  let env' := tgtLb.phis.toList.foldl (fun u phi => u.set! phi.destination (some (w phi))) q.environment
  refine ⟨⟨ctx.offset + tgtRk, 0, env'⟩, hTrOk, ?_⟩
  have hEnv'Sz : env'.size = valueCount ctx.norm := by
    dsimp only [env']
    rw [size_foldl_set (·.destination) (fun phi => some (w phi)), hSimSt.envSz]
  have hFoldlLookup : ∀ (ps : List Source.Phi) (init : Source.Environment),
      init.size = valueCount ctx.norm →
      (ps.map (·.destination)).Nodup →
      (∀ phi ∈ ps, phi.destination < valueCount ctx.norm) →
      (∀ phi ∈ ps, (ps.foldl (fun u x => u.set! x.destination (some (w x))) init)[phi.destination]? =
        some (some (w phi))) ∧
      (∀ slot, (∀ phi ∈ ps, phi.destination ≠ slot) →
        (ps.foldl (fun u x => u.set! x.destination (some (w x))) init)[slot]? = init[slot]?) := by
    intro ps
    induction ps with
    | nil =>
      intro init _ _ _
      exact ⟨(fun _ hm => nomatch hm), fun _ _ => rfl⟩
    | cons hd tl ih =>
      intro init hSz hNd hLt
      rw [List.map_cons, List.nodup_cons] at hNd
      have hHdLt : hd.destination < init.size := hSz.symm ▸ hLt hd List.mem_cons_self
      have hStepSz : (init.set! hd.destination (some (w hd))).size = valueCount ctx.norm := by
        rw [size_set!, hSz]
      obtain ⟨hTlIn, hTlOut⟩ := ih (init.set! hd.destination (some (w hd))) hStepSz hNd.2
        (fun x hx => hLt x (List.mem_cons_of_mem _ hx))
      refine ⟨?_, ?_⟩
      · intro phi hPhiMem
        cases hPhiMem with
        | head =>
          have hNotInTl : ∀ x ∈ tl, x.destination ≠ hd.destination := fun x hx hEq =>
            hNd.1 (List.mem_map.mpr ⟨x, hx, hEq⟩)
          simp only [List.foldl_cons]
          rw [hTlOut hd.destination hNotInTl, Array.set!_eq_setIfInBounds,
            Array.getElem?_setIfInBounds_self]
          simp only [hHdLt, ↓reduceIte]
        | tail _ htl =>
          simp only [List.foldl_cons]
          exact hTlIn phi htl
      · intro slot hNot
        simp only [List.foldl_cons]
        rw [hTlOut slot (fun x hx => hNot x (List.mem_cons_of_mem _ hx)),
          Array.set!_eq_setIfInBounds,
          Array.getElem?_setIfInBounds_ne (hNot hd List.mem_cons_self)]
  obtain ⟨hEnv'Param, hEnv'Other⟩ := hFoldlLookup tgtLb.phis.toList q.environment hSimSt.envSz hDestNodup
    (fun phi hPhi => hSimSt.envSz ▸ (hPhiW phi hPhi).2.1)
  refine ⟨hPureEnd, ⟨tgtRk, tgtBlk, tgtNb, tgtLb, hTgtGet, hBlkEnd.trans hTgtId.symm, rfl,
    hTgtNorm, hTgtBody, hTgtEmit, hTgtSrc, hTgtP, hTgtB, hTgtNT,
    by rw [hInstEnd]; exact Nat.zero_le _, by simp [hInstEnd]⟩, hEnv'Sz, ?_⟩
  intro r v hr
  have hInParamsDec : (∃ (k : Nat) (pParam : Sigma (SSAValue EarlyTy)), tgtBlk.params[k]? = some pParam ∧ pParam.2.id = r) ∨
      (∀ qParam ∈ tgtBlk.params, qParam.2.id ≠ r) := by
    have go : ∀ (ps : List (Sigma (SSAValue EarlyTy))),
        (∃ (j : Nat) (pParam : Sigma (SSAValue EarlyTy)), ps[j]? = some pParam ∧ pParam.2.id = r) ∨
        (∀ qParam ∈ ps, qParam.2.id ≠ r) := by
      intro ps
      induction ps with
      | nil => exact Or.inr fun _ hm => nomatch hm
      | cons a rs ih =>
        by_cases hEq : a.2.id = r
        · exact Or.inl ⟨0, a, rfl, hEq⟩
        · rcases ih with ⟨j, pP, hj, hEqP⟩ | hAll
          · exact Or.inl ⟨j + 1, pP, hj, hEqP⟩
          · exact Or.inr fun x hx => by
              cases hx with
              | head => exact hEq
              | tail _ htl => exact hAll x htl
    exact go tgtBlk.params
  rcases hInParamsDec with ⟨k, pParam, hPGet, hPEqR⟩ | hNotParam
  · have hkLt : k < tgtBlk.params.length := by
      rcases Nat.lt_or_ge k tgtBlk.params.length with h | h
      · exact h
      · rw [List.getElem?_eq_none h] at hPGet; cases hPGet
    have hkLtV : k < values.length := by omega
    have hValGet : values[k]? = some values[k] := List.getElem?_eq_getElem hkLtV
    have hAtK := hLkParams k pParam values[k] hPGet hValGet
    rw [hPEqR] at hAtK
    rw [hAtK] at hr; cases Option.some.inj hr
    have hkLtA : k < edge.arguments.length := by omega
    have hArgGet : edge.arguments[k]? = some edge.arguments[k] := List.getElem?_eq_getElem hkLtA
    have hValSpec := hValsAt k edge.arguments[k] values[k] hArgGet hValGet
    have hVRng : ValInRange values[k] := by
      rcases hArm : edge.arguments[k] with ⟨rty, read⟩
      rw [hArm] at hValSpec
      cases read with
      | literal lit => exact sliceLiteral_inRange hValSpec
      | value reg => exact (hSimSt.env reg.id values[k] hValSpec).1
    refine ⟨hVRng, fun slot hSlot => ?_⟩
    have hParamSlot : regId ctx.norm r = some (blockBase ctx.norm (ctx.offset + tgtRk) + k) := by
      rw [← hPEqR]
      exact ctx.regIdParam hTgtNorm hTgtIdxNe0 (hTgtP.symm ▸ hPGet)
    rw [hParamSlot] at hSlot; cases Option.some.inj hSlot
    obtain ⟨phi, hPhiGet, hDest, _, _⟩ := hPhiAt k (hPhisLen.symm ▸ hkLt)
    have hPhiIn : phi ∈ tgtLb.phis.toList := by
      rw [hPhisToList]; exact List.mem_of_getElem? hPhiGet
    have hAtPhi := hEnv'Param phi hPhiIn
    rw [hDest] at hAtPhi
    have hWVal : w phi = encVal values[k] := by
      dsimp only [w]; rw [hDest, Nat.add_sub_cancel_left, hValGet]
    rwa [hWVal] at hAtPhi
  · rw [hLkOther r hNotParam] at hr
    have hInSt := hSubAR r v hr
    obtain ⟨hRng, hAt⟩ := hSimSt.env r v hInSt
    refine ⟨hRng, fun slot hSlot => ?_⟩
    have hSlotNotPhi : ∀ phi ∈ tgtLb.phis.toList, phi.destination ≠ slot := by
      intro phi hPhiIn hEq
      rw [hPhisToList] at hPhiIn
      obtain ⟨k, hPhiGet⟩ := List.mem_iff_getElem?.mp hPhiIn
      have hk : k < phis.length := by
        rcases Nat.lt_or_ge k phis.length with h | h
        · exact h
        · rw [List.getElem?_eq_none h] at hPhiGet; cases hPhiGet
      obtain ⟨phi', hPhi'Get, hDest, _, _⟩ := hPhiAt k hk
      rw [hPhi'Get] at hPhiGet; cases Option.some.inj hPhiGet
      have hkP : k < tgtBlk.params.length := hPhisLen ▸ hk
      have hPGet : tgtNb.params[k]? = some tgtBlk.params[k] := by
        rw [hTgtP, List.getElem?_eq_getElem hkP]
      have hMem := alist_lookup_mem hSlot
      rw [← hEq, hDest] at hMem
      have hEqR := regIds_at_param_slot_iff ctx.zeroParams hTgtNorm hTgtIdxNe0 hPGet hMem
      exact hNotParam tgtBlk.params[k] (List.getElem_mem hkP) hEqR.symm
    rw [hEnv'Other slot hSlotNotPhi]
    exact hAt slot hSlot

private theorem nat_beq_self (n : Nat) : (n == n) = true := Nat.beq_refl n

private theorem nat_beq_false_of_ne {a b : Nat} (h : a ≠ b) : (a == b) = false := by
  cases hB : (a == b) with
  | false => rfl
  | true => exact absurd (Nat.eq_of_beq_eq_true hB) h

private theorem sim_step_jump {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (ctx : LCBCtx bir output)
    {p p' : MemoryRegisters (sliceLanguage slice) profile} {q : Source.State}
    {rk : Nat} {blk : BIRBlock (sliceLoweringLanguage slice)}
    {nb : NormBlock (sliceLoweringLanguage slice)} {lb : Source.Block}
    {edge : BIREdge (sliceLoweringLanguage slice)}
    (hSim : SimInv ctx p q)
    (hResNone : birHostedResult (birMemoryView (language := sliceLanguage slice) bir) p = none)
    (hBlkGet : ctx.blocks[rk]? = some blk) (hPBlk : p.block = blk.id)
    (hNormGet : ctx.norm.blocks[q.block]? = some nb) (hBodyGet : output.body.blocks[q.block]? = some lb)
    (hEmitBlk : emitBlock (sliceLanguage slice) ctx.norm (q.block, nb) = .ok lb)
    (hNbB : nb.body = blk.body)
    (hNormTerm : normalizeTerm ctx.indices ctx.splits q.block blk.ending = .ok nb.term)
    (hPcEq : p.instruction = blk.body.length)
    (hQPos : q.position = ((blk.body.take p.instruction).map fun op => instWidth op.instruction).sum)
    (hEnd : blk.ending = .jump edge)
    (hCtrl : (birMemoryView (language := sliceLanguage slice) bir).Transfer
      ⟨edge.target, edge.arguments.map (birOpnd ctx.base)⟩ p p' false) :
    execBIRStep bir p = some p' ∧
    ∃ k q', 0 < k ∧ Source.Steps output.body k q q' ∧ SimInv ctx p' q' := by
  let sLow := sliceLoweringLanguage slice
  have hBlkAt := ctx.blockAt hSim.pure hBlkGet hPBlk
  have hBlkGraph := (ctx.hBlocksSub blk (List.mem_of_getElem? hBlkGet)).1
  have hBlkReach := (ctx.hBlocksSub blk (List.mem_of_getElem? hBlkGet)).2
  have hGraphAt := birMemoryView_graphAt (language := sliceLanguage slice) ctx.hEntry
  have hMNone : (blk.body.map (fun op =>
      (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst sLow))))[p.instruction]? = none :=
    List.getElem?_eq_none (by simp [hPcEq])
  have hQPosEnd : q.position = lb.instructions.size := by
    rw [hQPos, hPcEq, List.take_length, emitBlock_insts_size hEmitBlk, hNbB]
  obtain ⟨_, _, term, _, _, hEmitTerm, hLbEq⟩ := emitBlock_ok_inv hEmitBlk
  have hLbTerm : lb.terminator = term := by rw [hLbEq]
  rw [hEnd] at hNormTerm
  obtain ⟨values, afterRead, mTgtBlk, hRdOps, hTgtAt, hArgTypes, hPO⟩ := hCtrl
  have hTgtGraph : ∃ tgtBlk ∈ ctx.graph.blocks.toList, tgtBlk.id = edge.target ∧
      mTgtBlk = ⟨tgtBlk.id, tgtBlk.params.map (fun r => ⟨r.1, birReg ctx.base r.2⟩),
        tgtBlk.body.map (fun op => ⟨op.origin, birInst ctx.base op.instruction⟩),
        birTerm bir ctx.base tgtBlk.ending, tgtBlk.boundaryOrigins⟩ := by
    unfold Memory.blockAt at hTgtAt
    rw [hSim.pure.frame, hGraphAt] at hTgtAt
    dsimp only [bind, Option.bind, birMemoryGraph] at hTgtAt
    rw [← Array.find?_toList, Array.toList_map] at hTgtAt
    have hId : mTgtBlk.id = edge.target := by simpa using List.find?_some hTgtAt
    obtain ⟨raw, hRawMem, hRawEq⟩ := List.mem_map.mp (List.mem_of_find?_eq_some hTgtAt)
    exact ⟨raw, hRawMem, (congrArg Block.id hRawEq).trans hId, hRawEq.symm⟩
  obtain ⟨tgtBlk, hTgtGraphMem, hTgtId, hMTgtEq⟩ := hTgtGraph
  have hTgtReach : edge.target ∈ BIR.reachableBlocks ctx.graph := by
    unfold BIR.reachableBlocks at hBlkReach ⊢
    rw [mem_reachGraph_iff] at hBlkReach ⊢
    refine .snoc hBlkReach ?_ (List.mem_map.mpr ⟨tgtBlk, hTgtGraphMem, hTgtId⟩) (by intro h; cases h)
    exact List.mem_flatMap.mpr ⟨blk, hBlkGraph, List.mem_map.mpr ⟨edge, by simp [hEnd, BIRTerm.successors], rfl⟩⟩
  have hTgtInBlks : tgtBlk ∈ ctx.blocks := ctx.hBlocksCov tgtBlk hTgtGraphMem (hTgtId.symm ▸ hTgtReach)
  obtain ⟨tgtRk, hTgtGet⟩ := List.mem_iff_getElem?.mp hTgtInBlks
  obtain ⟨tgtNb, tgtLb, hTgtNorm, hTgtBody, hTgtEmit, hTgtSrc, hTgtP, hTgtB, hTgtNT⟩ := ctx.realAt hTgtGet
  have hLkTgt : AList.lookup ctx.indices edge.target = some (ctx.offset + tgtRk) :=
    hTgtId ▸ ctx.indicesAt hTgtGet
  dsimp only [normalizeTerm] at hNormTerm
  rw [hLkTgt] at hNormTerm
  have hNbTerm : nb.term = .jump (ctx.offset + tgtRk) edge.arguments := (Except.ok.inj hNormTerm).symm
  rw [hNbTerm] at hEmitTerm
  dsimp only [emitTerm] at hEmitTerm
  cases Except.ok.inj hEmitTerm
  have hDet : execBIRStep bir p = some p' := by
    unfold execBIRStep; dsimp only
    simp only [hSim.pure.exhausted, hResNone, Option.isSome_none, Bool.or_self,
      Bool.false_eq_true, ↓reduceIte]
    rw [hBlkAt]; dsimp only
    rw [show (blk.body.map (fun op =>
        (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst (sliceLoweringLanguage slice)))))[p.instruction]? =
        none from hMNone]
    dsimp only
    have hPcNe : (p.instruction != (blk.body.map (fun op =>
        (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst (sliceLoweringLanguage slice))))).length) = false := by
      rw [List.length_map, hPcEq]
      change (!Nat.beq blk.body.length blk.body.length) = false
      rw [Nat.beq_refl]; rfl
    rw [hPcNe]; simp only [Bool.false_eq_true, ↓reduceIte]
    rw [hEnd]; dsimp only [birTerm]
    rw [hRdOps]; dsimp only
    have hTgtAt' := hTgtAt
    dsimp only at hTgtAt'
    rw [hSim.pure.exhausted] at hTgtAt'
    rw [hTgtAt']; dsimp only
    letI := (sliceLanguage slice).typeDecidableEq
    have hTyDec : decide ((edge.arguments.map (birOpnd ctx.base)).map Sigma.fst = mTgtBlk.params.map Sigma.fst) = true :=
      decide_eq_true hArgTypes
    simp only [hTyDec, Bool.not_true, Bool.false_eq_true, ↓reduceIte]
    have hPrAR := (readOperands_virtual_inv hSim.pure hRdOps).1
    rw [hMTgtEq] at hPO ⊢
    exact execWriteParams_of_writeParams ctx.hEntry hPrAR.frame hPrAR.movedFields
      (fun q hq => by obtain ⟨p0, _, rfl⟩ := List.mem_map.mp hq; exact ⟨⟨p0.2.id, rfl⟩, rfl, rfl⟩) hPO
  obtain ⟨q', hTrOk, hSim'⟩ := sim_doTransfer ctx hBlkGet (by simp [hEnd, BIRTerm.successors]) hSim
    ⟨values, afterRead, mTgtBlk, hRdOps, hTgtAt, hArgTypes, hPO⟩ hNormGet
    hTgtGet hTgtId hTgtNorm hTgtBody hTgtEmit hTgtSrc hTgtP hTgtB hTgtNT
    (fun _ => by rw [hNbTerm]; simp only [edgesTo, nat_beq_self, ↓reduceIte])
  have hStep1 : Source.Step output.body q q' := step_jump hBodyGet hQPosEnd hLbTerm hTrOk
  exact ⟨hDet, 1, q', Nat.zero_lt_one, .succ hStep1 (.zero _), hSim'⟩

private theorem sim_step_branch {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (ctx : LCBCtx bir output)
    {p p' : MemoryRegisters (sliceLanguage slice) profile} {q : Source.State}
    {rk : Nat} {blk : BIRBlock (sliceLoweringLanguage slice)}
    {nb : NormBlock (sliceLoweringLanguage slice)} {lb : Source.Block}
    {test : SSARead (sliceLanguage slice).operands.Ty (sliceLoweringLanguage slice).literal ((sliceLoweringLanguage slice).bool)}
    {yes no : BIREdge (sliceLoweringLanguage slice)}
    (hSim : SimInv ctx p q)
    (hResNone : birHostedResult (birMemoryView (language := sliceLanguage slice) bir) p = none)
    (hBlkGet : ctx.blocks[rk]? = some blk) (hPBlk : p.block = blk.id) (hQBlk : q.block = ctx.offset + rk)
    (hNormGet : ctx.norm.blocks[q.block]? = some nb) (hBodyGet : output.body.blocks[q.block]? = some lb)
    (hEmitBlk : emitBlock (sliceLanguage slice) ctx.norm (q.block, nb) = .ok lb)
    (hSrcId : nb.sourceId = some blk.id) (hNbP : nb.params = blk.params) (hNbB : nb.body = blk.body)
    (hNormTerm : normalizeTerm ctx.indices ctx.splits q.block blk.ending = .ok nb.term)
    (hPcEq : p.instruction = blk.body.length)
    (hQPos : q.position = ((blk.body.take p.instruction).map fun op => instWidth op.instruction).sum)
    (hEnd : blk.ending = .branch test yes no)
    (hCtrl : (birMemoryView (language := sliceLanguage slice) bir).ControlStep
      p p' (birTerm bir ctx.base (.branch test yes no)) false) :
    execBIRStep bir p = some p' ∧
    ∃ k q', 0 < k ∧ Source.Steps output.body k q q' ∧ SimInv ctx p' q' := by
  let sLang := sliceLanguage slice
  let sLow := sliceLoweringLanguage slice
  have hBlkAt := ctx.blockAt hSim.pure hBlkGet hPBlk
  have hBlkGraph := (ctx.hBlocksSub blk (List.mem_of_getElem? hBlkGet)).1
  have hBlkReach := (ctx.hBlocksSub blk (List.mem_of_getElem? hBlkGet)).2
  have hGraphAt := birMemoryView_graphAt (language := sLang) ctx.hEntry
  have hMNone : (blk.body.map (fun op =>
      (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst sLow))))[p.instruction]? = none :=
    List.getElem?_eq_none (by simp [hPcEq])
  have hQPosEnd : q.position = lb.instructions.size := by
    rw [hQPos, hPcEq, List.take_length, emitBlock_insts_size hEmitBlk, hNbB]
  obtain ⟨_, _, term, _, _, hEmitTerm, hLbEq⟩ := emitBlock_ok_inv hEmitBlk
  have hLbTerm : lb.terminator = term := by rw [hLbEq]
  have hNormTermOrig := hNormTerm
  rw [hEnd] at hNormTerm
  obtain ⟨val, afterRead, flag, hRdTest, hTestVal, hTr⟩ := hCtrl
  have hValBool : val = .scalar (.bool flag) := by
    cases val with
    | other _ => cases hTestVal
    | scalar sv =>
      cases sv with
      | int _ _ | char _ | unit => cases hTestVal
      | bool b => cases Option.some.inj hTestVal; rfl
  subst hValBool
  have hPrTest := readValue_virtual_inv hSim.pure hRdTest
  have hSimAfterTest : SimInv ctx afterRead q :=
    ⟨hPrTest.1, ⟨rk, blk, nb, lb, hBlkGet, hPrTest.2.1.trans hPBlk, hQBlk, hNormGet,
      hBodyGet, hEmitBlk, hSrcId, hNbP, hNbB, hNormTermOrig, hPrTest.2.2.1.symm ▸ (Nat.le_of_eq hPcEq),
      hPrTest.2.2.1.symm ▸ hQPos⟩, hSim.envSz,
      fun r v hr => hSim.env r v (hPrTest.2.2.2.1 r v hr)⟩
  let chosenEdge := if flag then yes else no
  have hChosenMem : chosenEdge ∈ blk.ending.successors := by
    cases flag <;> simp [chosenEdge, hEnd, BIRTerm.successors]
  obtain ⟨values, afterArgs, mTgtBlk, hRdOps, hTgtAt, hArgTypes, hPO⟩ := hTr
  have hTgtGraph : ∃ tgtBlk ∈ ctx.graph.blocks.toList, tgtBlk.id = chosenEdge.target ∧
      mTgtBlk = ⟨tgtBlk.id, tgtBlk.params.map (fun r => ⟨r.1, birReg ctx.base r.2⟩),
        tgtBlk.body.map (fun op => ⟨op.origin, birInst ctx.base op.instruction⟩),
        birTerm bir ctx.base tgtBlk.ending, tgtBlk.boundaryOrigins⟩ := by
    unfold Memory.blockAt at hTgtAt
    rw [hPrTest.1.frame, hGraphAt] at hTgtAt
    dsimp only [bind, Option.bind, birMemoryGraph] at hTgtAt
    rw [← Array.find?_toList, Array.toList_map] at hTgtAt
    have hId : mTgtBlk.id = chosenEdge.target := by
      cases flag <;> simpa [chosenEdge] using List.find?_some hTgtAt
    obtain ⟨raw, hRawMem, hRawEq⟩ := List.mem_map.mp (List.mem_of_find?_eq_some hTgtAt)
    exact ⟨raw, hRawMem, (congrArg Block.id hRawEq).trans hId, hRawEq.symm⟩
  obtain ⟨tgtBlk, hTgtGraphMem, hTgtId, hMTgtEq⟩ := hTgtGraph
  have hTgtReach : chosenEdge.target ∈ BIR.reachableBlocks ctx.graph := by
    unfold BIR.reachableBlocks at hBlkReach ⊢
    rw [mem_reachGraph_iff] at hBlkReach ⊢
    refine .snoc hBlkReach ?_ (List.mem_map.mpr ⟨tgtBlk, hTgtGraphMem, hTgtId⟩) (by intro h; cases h)
    exact List.mem_flatMap.mpr ⟨blk, hBlkGraph, List.mem_map.mpr ⟨chosenEdge, hChosenMem, rfl⟩⟩
  have hTgtInBlks : tgtBlk ∈ ctx.blocks := ctx.hBlocksCov tgtBlk hTgtGraphMem (hTgtId.symm ▸ hTgtReach)
  obtain ⟨tgtRk, hTgtGet⟩ := List.mem_iff_getElem?.mp hTgtInBlks
  obtain ⟨tgtNb, tgtLb, hTgtNorm, hTgtBody, hTgtEmit, hTgtSrc, hTgtP, hTgtB, hTgtNT⟩ := ctx.realAt hTgtGet
  have hDet : execBIRStep bir p = some p' := by
    unfold execBIRStep; dsimp only
    simp only [hSim.pure.exhausted, hResNone, Option.isSome_none, Bool.or_self,
      Bool.false_eq_true, ↓reduceIte]
    rw [hBlkAt]; dsimp only
    rw [show (blk.body.map (fun op =>
        (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst (sliceLoweringLanguage slice)))))[p.instruction]? =
        none from hMNone]
    dsimp only
    have hPcNe : (p.instruction != (blk.body.map (fun op =>
        (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst (sliceLoweringLanguage slice))))).length) = false := by
      rw [List.length_map, hPcEq]
      change (!Nat.beq blk.body.length blk.body.length) = false
      rw [Nat.beq_refl]; rfl
    rw [hPcNe]; simp only [Bool.false_eq_true, ↓reduceIte]
    rw [hEnd]; dsimp only [birTerm]
    rw [hRdTest]; dsimp only
    rw [show (sliceLanguage slice).operands.test (.scalar (.bool flag)) = some flag from hTestVal]
    dsimp only
    cases flag with
    | true =>
      simp only [↓reduceIte]
      simp only [↓reduceIte] at hRdOps hTgtAt hArgTypes hPO
      rw [hRdOps]; dsimp only
      rw [hTgtAt]; dsimp only
      letI := (sliceLanguage slice).typeDecidableEq
      have hTyDec : decide ((yes.arguments.map (birOpnd ctx.base)).map Sigma.fst = mTgtBlk.params.map Sigma.fst) = true :=
        decide_eq_true hArgTypes
      simp only [hTyDec, Bool.not_true, Bool.false_eq_true, ↓reduceIte]
      have hPrAR := (readOperands_virtual_inv hPrTest.1 hRdOps).1
      rw [hMTgtEq] at hPO ⊢
      exact execWriteParams_of_writeParams ctx.hEntry hPrAR.frame hPrAR.movedFields
        (fun q hq => by obtain ⟨p0, _, rfl⟩ := List.mem_map.mp hq; exact ⟨⟨p0.2.id, rfl⟩, rfl, rfl⟩) hPO
    | false =>
      simp only [Bool.false_eq_true, ↓reduceIte]
      simp only [Bool.false_eq_true, ↓reduceIte] at hRdOps hTgtAt hArgTypes hPO
      rw [hRdOps]; dsimp only
      rw [hTgtAt]; dsimp only
      letI := (sliceLanguage slice).typeDecidableEq
      have hTyDec : decide ((no.arguments.map (birOpnd ctx.base)).map Sigma.fst = mTgtBlk.params.map Sigma.fst) = true :=
        decide_eq_true hArgTypes
      simp only [hTyDec, Bool.not_true, Bool.false_eq_true, ↓reduceIte]
      have hPrAR := (readOperands_virtual_inv hPrTest.1 hRdOps).1
      rw [hMTgtEq] at hPO ⊢
      exact execWriteParams_of_writeParams ctx.hEntry hPrAR.frame hPrAR.movedFields
        (fun q hq => by obtain ⟨p0, _, rfl⟩ := List.mem_map.mp hq; exact ⟨⟨p0.2.id, rfl⟩, rfl, rfl⟩) hPO
  unfold normalizeTerm at hNormTerm
  cases hLkY : AList.lookup ctx.indices yes.target with
  | none => simp [hLkY] at hNormTerm
  | some yesIdx =>
    cases hLkN : AList.lookup ctx.indices no.target with
    | none => simp [hLkY, hLkN] at hNormTerm
    | some rawNoIdx =>
      simp only [hLkY, hLkN] at hNormTerm
      obtain ⟨hSpSpec, hSpNd, hSpEx⟩ := collectSplitEdges_spec ctx.offset ctx.blocks ctx.indices
      have hTgtIdxLk : AList.lookup ctx.indices chosenEdge.target = some (ctx.offset + tgtRk) :=
        hTgtId ▸ ctx.indicesAt hTgtGet
      have hTgtRkLt : tgtRk < ctx.blocks.length := by
        rcases Nat.lt_or_ge tgtRk ctx.blocks.length with h | h
        · exact h
        · rw [List.getElem?_eq_none h] at hTgtGet; cases hTgtGet
      cases hFindSp : ctx.splits.find? (·.predIndex == q.block) with
      | some sp =>
        rw [hFindSp] at hNormTerm
        have hNbTerm : nb.term = .branch test yesIdx yes.arguments sp.splitIndex [] := (Except.ok.inj hNormTerm).symm
        rw [hNbTerm] at hEmitTerm
        dsimp only [emitTerm, bind, Except.bind] at hEmitTerm
        cases hTestEm : emitRead (sliceLanguage slice) ctx.norm test with
        | error _ => rw [hTestEm] at hEmitTerm; cases hEmitTerm
        | ok test' =>
          rw [hTestEm] at hEmitTerm; dsimp only [pure, Except.pure] at hEmitTerm
          cases Except.ok.inj hEmitTerm
          have hTestRd : Source.read q.environment .word test' = .ok (boolWord flag) :=
            (sim_readAgree ctx hSim hTestEm hRdTest).2
          have hSpMem : sp ∈ ctx.splits := List.mem_of_find?_eq_some hFindSp
          have hSpPredB := List.find?_some hFindSp
          have hSpPred : sp.predIndex = q.block := Nat.eq_of_beq_eq_true hSpPredB
          obtain ⟨spK, hSpKGet⟩ := List.mem_iff_getElem?.mp hSpMem
          obtain ⟨hSpIdxEq, jSp, blkSp, _, yesSp, noSp, hBlkSpGet, hSpPredJ, hBlkSpEnd,
            hYesNoSp, _, hSpLk, _, hSpArgs⟩ := hSpSpec spK sp hSpKGet
          have hJEqRk : jSp = rk := Nat.add_left_cancel (hSpPredJ.symm.trans (hSpPred.trans hQBlk))
          subst hJEqRk
          rw [hBlkGet] at hBlkSpGet; cases Option.some.inj hBlkSpGet
          rw [hEnd] at hBlkSpEnd; cases hBlkSpEnd
          have hSpTgtEq : sp.targetIndex = yesIdx := Option.some.inj (hSpLk.symm.trans hLkY)
          cases flag with
          | true =>
            have hYesEq : yesIdx = ctx.offset + tgtRk := Option.some.inj (hLkY.symm.trans hTgtIdxLk)
            subst hYesEq
            have hSpNe : sp.splitIndex ≠ ctx.offset + tgtRk := by omega
            obtain ⟨q', hTrOk, hSim'⟩ := sim_doTransfer ctx hBlkGet hChosenMem hSimAfterTest
              ⟨values, afterArgs, mTgtBlk, hRdOps, hTgtAt, hArgTypes, hPO⟩ hNormGet
              hTgtGet hTgtId hTgtNorm hTgtBody hTgtEmit hTgtSrc hTgtP hTgtB hTgtNT
              (fun _ => by
                rw [hNbTerm]
                simp only [chosenEdge, edgesTo, nat_beq_self, nat_beq_false_of_ne hSpNe,
                  Bool.false_eq_true, ↓reduceIte, List.append_nil])
            have hStep1 : Source.Step output.body q q' :=
              step_branch hBodyGet hQPosEnd hLbTerm hTestRd (by simpa [boolWord_branch] using hTrOk)
            exact ⟨hDet, 1, q', Nat.zero_lt_one, .succ hStep1 (.zero _), hSim'⟩
          | false =>
            have hNoEq : yesIdx = ctx.offset + tgtRk := by
              rw [hYesNoSp] at hLkY
              exact Option.some.inj (hLkY.symm.trans hTgtIdxLk)
            subst hNoEq
            obtain ⟨spLb, hSpNorm, hSpBody, _, hSpPhis, hSpInsts, hSpTerm⟩ := ctx.splitAt hSpKGet
            have hStep1 : Source.Step output.body q ⟨sp.splitIndex, 0, q.environment⟩ :=
              step_branch hBodyGet hQPosEnd hLbTerm hTestRd
                (by simpa [boolWord_branch] using transfer_no_phis hSpBody hSpPhis)
            have hSpArgsEq : sp.arguments = no.arguments := hSpArgs
            obtain ⟨q', hTrOk, hSim'⟩ := sim_doTransfer ctx hBlkGet hChosenMem hSimAfterTest
              ⟨values, afterArgs, mTgtBlk, hRdOps, hTgtAt, hArgTypes, hPO⟩ hSpNorm
              hTgtGet hTgtId hTgtNorm hTgtBody hTgtEmit hTgtSrc hTgtP hTgtB hTgtNT
              (fun _ => by
                simp only [chosenEdge, edgesTo, hSpTgtEq, hSpArgsEq, nat_beq_self,
                  Bool.false_eq_true, ↓reduceIte])
            have hStep2 : Source.Step output.body ⟨sp.splitIndex, 0, q.environment⟩ q' :=
              step_jump hSpBody (by simp [hSpInsts]) hSpTerm (by rw [hSpTgtEq]; exact hTrOk)
            exact ⟨hDet, 2, q', by decide, .succ hStep1 (.succ hStep2 (.zero _)), hSim'⟩
      | none =>
        rw [hFindSp] at hNormTerm
        have hNbTerm : nb.term = .branch test yesIdx yes.arguments rawNoIdx no.arguments := (Except.ok.inj hNormTerm).symm
        rw [hNbTerm] at hEmitTerm
        dsimp only [emitTerm, bind, Except.bind] at hEmitTerm
        cases hTestEm : emitRead (sliceLanguage slice) ctx.norm test with
        | error _ => rw [hTestEm] at hEmitTerm; cases hEmitTerm
        | ok test' =>
          rw [hTestEm] at hEmitTerm; dsimp only [pure, Except.pure] at hEmitTerm
          cases Except.ok.inj hEmitTerm
          have hTestRd : Source.read q.environment .word test' = .ok (boolWord flag) :=
            (sim_readAgree ctx hSim hTestEm hRdTest).2
          have hArmsDistinct : tgtBlk.params ≠ [] → yesIdx ≠ rawNoIdx := by
            intro hPNe hSameIdx
            have hTgtEqYN : yes.target = no.target := by
              have hInY := alist_lookup_mem hLkY
              have hInN := alist_lookup_mem hLkN
              obtain ⟨⟨ky, by'⟩, hky, hEqY⟩ := List.mem_map.mp hInY
              obtain ⟨⟨kn, bn'⟩, hkn, hEqN⟩ := List.mem_map.mp hInN
              have hkyEq : ctx.offset + ky = yesIdx := (Prod.mk.inj hEqY).2
              have hknEq : ctx.offset + kn = rawNoIdx := (Prod.mk.inj hEqN).2
              have hkEq : ky = kn := Nat.add_left_cancel (hkyEq.trans (hSameIdx.trans hknEq.symm))
              have hGetY := (mem_indexed_getElem? hky).2
              have hGetN := (mem_indexed_getElem? hkn).2
              rw [hkEq, hGetN] at hGetY; cases Option.some.inj hGetY
              exact (Prod.mk.inj hEqY).1.symm.trans (Prod.mk.inj hEqN).1
            have hTargetHasP : targetHasParams ctx.blocks yes.target = true := by
              unfold targetHasParams
              have hFindTgt : ctx.blocks.find? (·.id == yes.target) = some tgtBlk := by
                have hYEqChosen : yes.target = tgtBlk.id := by
                  cases flag
                  · exact hTgtEqYN.trans hTgtId.symm
                  · exact hTgtId.symm
                rw [hYEqChosen]
                cases hf : ctx.blocks.find? (·.id == tgtBlk.id) with
                | none =>
                  have := List.find?_eq_none.mp hf tgtBlk (List.mem_of_getElem? hTgtGet)
                  simp at this
                | some found =>
                  have hFMem := List.mem_of_find?_eq_some hf
                  have hFId : found.id = tgtBlk.id := by simpa using List.find?_some hf
                  rw [block_eq_of_id_eq ctx.hBlocksNd hFMem (List.mem_of_getElem? hTgtGet) hFId]
              rw [hFindTgt]
              cases hP : tgtBlk.params
              · exact absurd hP hPNe
              · simp [hP]
            obtain ⟨sp, hSpMem, hSpPred, _, _⟩ :=
              hSpEx rk blk test yes no yesIdx hBlkGet hEnd hTgtEqYN hTargetHasP hLkY
            have hNot := List.find?_eq_none.mp hFindSp sp hSpMem
            rw [hQBlk] at hNot
            exact absurd (hSpPred ▸ nat_beq_self _) hNot
          cases flag with
          | true =>
            have hYesEq : yesIdx = ctx.offset + tgtRk := Option.some.inj (hLkY.symm.trans hTgtIdxLk)
            subst hYesEq
            obtain ⟨q', hTrOk, hSim'⟩ := sim_doTransfer ctx hBlkGet hChosenMem hSimAfterTest
              ⟨values, afterArgs, mTgtBlk, hRdOps, hTgtAt, hArgTypes, hPO⟩ hNormGet
              hTgtGet hTgtId hTgtNorm hTgtBody hTgtEmit hTgtSrc hTgtP hTgtB hTgtNT
              (fun hPNe => by
                have hNe := (hArmsDistinct hPNe).symm
                rw [hNbTerm]
                simp only [chosenEdge, edgesTo, nat_beq_self, nat_beq_false_of_ne hNe,
                  Bool.false_eq_true, ↓reduceIte, List.append_nil])
            have hStep1 : Source.Step output.body q q' :=
              step_branch hBodyGet hQPosEnd hLbTerm hTestRd (by simpa [boolWord_branch] using hTrOk)
            exact ⟨hDet, 1, q', Nat.zero_lt_one, .succ hStep1 (.zero _), hSim'⟩
          | false =>
            have hNoEq : rawNoIdx = ctx.offset + tgtRk := Option.some.inj (hLkN.symm.trans hTgtIdxLk)
            subst hNoEq
            obtain ⟨q', hTrOk, hSim'⟩ := sim_doTransfer ctx hBlkGet hChosenMem hSimAfterTest
              ⟨values, afterArgs, mTgtBlk, hRdOps, hTgtAt, hArgTypes, hPO⟩ hNormGet
              hTgtGet hTgtId hTgtNorm hTgtBody hTgtEmit hTgtSrc hTgtP hTgtB hTgtNT
              (fun hPNe => by
                have hNe := hArmsDistinct hPNe
                rw [hNbTerm]
                simp only [chosenEdge, edgesTo, nat_beq_self, nat_beq_false_of_ne hNe,
                  Bool.false_eq_true, ↓reduceIte, List.nil_append])
            have hStep1 : Source.Step output.body q q' :=
              step_branch hBodyGet hQPosEnd hLbTerm hTestRd (by simpa [boolWord_branch] using hTrOk)
            exact ⟨hDet, 1, q', Nat.zero_lt_one, .succ hStep1 (.zero _), hSim'⟩

private theorem sim_step_and_det {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (ctx : LCBCtx bir output)
    {p p' : MemoryRegisters (sliceLanguage slice) profile} {q : Source.State}
    (hSim : SimInv ctx p q)
    (hStep : (BIR.Execution (language := sliceLanguage slice)).step bir p p') :
    execBIRStep bir p = some p' ∧
    ∃ k q', 0 < k ∧ Source.Steps output.body k q q' ∧ SimInv ctx p' q' := by
  let sLang := sliceLanguage slice
  let sLow := sliceLoweringLanguage slice
  obtain ⟨rk, blk, nb, lb, hBlkGet, hPBlk, hQBlk, hNormGet, hBodyGet, hEmitBlk,
    hSrcId, hNbP, hNbB, hNormTerm, hPcLe, hQPos⟩ := hSim.atBlk
  have hBlkGraph := (ctx.hBlocksSub blk (List.mem_of_getElem? hBlkGet)).1
  have hBlkReach := (ctx.hBlocksSub blk (List.mem_of_getElem? hBlkGet)).2
  have hBlkAt := ctx.blockAt hSim.pure hBlkGet hPBlk
  have hResNone : birHostedResult (birMemoryView (language := sLang) bir) p = none := by
    rw [← birHostedResult_eq bir p]; exact hStep.1
  obtain ⟨hPure', _⟩ := bir_step_preserves_pure_and_reach ctx.hBackend ctx.hWF ctx.hSup ctx.hEntry
    hSim.pure (hPBlk.symm ▸ hBlkReach) hStep
  change _ ∧ (bir.memoryView (language := sLang)).Step p p' true ∧ _ at hStep
  rw [← birMemoryView_eq bir] at hStep
  have hSt := hStep.2.1
  unfold Memory.Step at hSt
  rw [hSim.pure.exhausted, hPure'.exhausted] at hSt
  simp only [Bool.false_eq_true, ↓reduceIte] at hSt
  obtain ⟨mBlk, hMBlk, hOutcome⟩ := hSt
  rw [hBlkAt] at hMBlk; cases Option.some.inj hMBlk
  cases hOpAt : blk.body[p.instruction]? with
  | some op =>
    have hMOpAt : (blk.body.map (fun op =>
        (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst sLow))))[p.instruction]? =
        some ⟨op.origin, birInst ctx.base op.instruction⟩ := by
      rw [List.getElem?_map, hOpAt]; rfl
    rw [hMOpAt] at hOutcome
    have hPcLt : p.instruction < blk.body.length := by
      rcases Nat.lt_or_ge p.instruction blk.body.length with h | h
      · exact h
      · rw [List.getElem?_eq_none h] at hOpAt; cases hOpAt
    rcases lcb_reachable_inst_classify ctx.hBackend ctx.hWF ctx.hSup ctx.hEntry hBlkGraph hBlkReach
      (List.mem_of_getElem? hOpAt) with
      ⟨ty, dst, fop, arg, hInstEq⟩ | ⟨mOp, wOp, t, dst, fop, lhs, rhs, hWOp, htProf, hInstEq⟩ |
      ⟨t, dst, flag, lhs, rhs, htProf, hNe, hInstEq⟩
    · exact sim_step_read ctx hSim hPure' hResNone hBlkGet hPBlk hQBlk hNormGet hBodyGet
        hEmitBlk hSrcId hNbP hNbB hNormTerm hPcLt hQPos hOpAt hInstEq hOutcome
    · exact sim_step_arith ctx hWOp htProf hSim hPure' hResNone hBlkGet hPBlk hQBlk hNormGet hBodyGet
        hEmitBlk hSrcId hNbP hNbB hNormTerm hPcLt hQPos hOpAt hInstEq hOutcome
    · exact sim_step_addWithOverflow ctx htProf hNe hSim hPure' hResNone hBlkGet hPBlk hQBlk hNormGet
        hBodyGet hEmitBlk hSrcId hNbP hNbB hNormTerm hPcLt hQPos hOpAt hInstEq hOutcome
  | none =>
    have hMNone : (blk.body.map (fun op =>
        (⟨op.origin, birInst ctx.base op.instruction⟩ : LocatedOp (MemoryInst sLow))))[p.instruction]? = none := by
      rw [List.getElem?_map, hOpAt]; rfl
    rw [hMNone] at hOutcome
    have hPcEq : p.instruction = blk.body.length := by simpa using hOutcome.1
    have hCtrl := hOutcome.2
    cases hEnd : blk.ending with
    | ret ov =>
      rw [hEnd] at hCtrl
      obtain ⟨_, _, _, _, _, hCallers, _⟩ := hCtrl
      rw [hSim.pure.callers] at hCallers; cases hCallers
    | trap slot =>
      rw [hEnd] at hCtrl
      dsimp only [birTerm, Memory.ControlStep] at hCtrl
      cases hS : bir.sites[slot.index]? <;> rw [hS] at hCtrl <;> exact False.elim hCtrl
    | jump edge =>
      rw [hEnd] at hCtrl
      exact sim_step_jump ctx hSim hResNone hBlkGet hPBlk hNormGet hBodyGet hEmitBlk hNbB
        hNormTerm hPcEq hQPos hEnd hCtrl
    | branch test yes no =>
      rw [hEnd] at hCtrl
      exact sim_step_branch ctx hSim hResNone hBlkGet hPBlk hQBlk hNormGet hBodyGet hEmitBlk
        hSrcId hNbP hNbB hNormTerm hPcEq hQPos hEnd hCtrl

private theorem sim_init {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (ctx : LCBCtx bir output) :
    ∃ q0 k0 q1,
      Source.initial output.body #[] = .ok q0 ∧
      Source.Steps output.body k0 q0 q1 ∧
      SimInv ctx (BIR.entryState (language := sliceLanguage slice) bir) q1 := by
  let sLang := sliceLanguage slice
  let sLow := sliceLoweringLanguage slice
  let p0 := BIR.entryState (language := sLang) bir
  have hMemEq := birMemoryView_eq (language := sLang) bir
  have hGraphAt := birMemoryView_graphAt (language := sLang) ctx.hEntry
  have hP0Pure : FramePure bir p0 := ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
  have hP0Blk : p0.block = ctx.entryBlk.id := by
    unfold p0 BIR.entryState
    rw [← hMemEq]
    change (((birMemoryView bir).graphAt bir.entry).map CFG.entry).getD ⟨0⟩ = ctx.entryBlk.id
    rw [hGraphAt, ctx.hEntryBlkId]; rfl
  have hP0Pc : p0.instruction = 0 := by
    have hBlkAt0 := ctx.blockAt hP0Pure (ctx.hBlocksCons ▸ List.getElem?_cons_zero) hP0Blk
    have hP0Eq : p0 = birEntryState (birMemoryView bir) := birEntryState_eq bir
    rw [hP0Eq]
    unfold birEntryState
    have hAt : ((birMemoryView bir).graphAt bir.entry).bind
        (fun g => g.blocks.find? (·.id == (((birMemoryView bir).graphAt bir.entry).map CFG.entry).getD ⟨0⟩)) =
        some ⟨ctx.entryBlk.id, ctx.entryBlk.params.map (fun r => ⟨r.1, birReg ctx.base r.2⟩),
          ctx.entryBlk.body.map (fun op => ⟨op.origin, birInst ctx.base op.instruction⟩),
          birTerm bir ctx.base ctx.entryBlk.ending, ctx.entryBlk.boundaryOrigins⟩ := by
      unfold Memory.blockAt at hBlkAt0
      change ((birMemoryView bir).graphAt bir.entry).bind (fun g => g.blocks.find? (·.id == p0.block)) = _ at hBlkAt0
      rw [hP0Blk] at hBlkAt0
      rw [hGraphAt] at hBlkAt0 ⊢
      have hEntEq : ((some (birMemoryGraph (language := sLang) bir (birBase ctx.locals ctx.graph) ctx.graph)).map CFG.entry).getD ⟨0⟩ =
          ctx.entryBlk.id := ctx.hEntryBlkId.symm
      rw [hEntEq]
      exact hBlkAt0
    dsimp only
    change ((((birMemoryView bir).graphAt bir.entry).bind
      (fun g => g.blocks.find? (·.id == (((birMemoryView bir).graphAt bir.entry).map CFG.entry).getD ⟨0⟩))).map
      fun b => if b.params.length = 0 then 0 else b.body.length + 1).getD 0 = 0
    rw [hAt]; simp [ctx.hEntryParamsEmpty]
  obtain ⟨nb0, lb0, hNorm0, hBody0, hEmit0, hSrc0, hNbP0, hNbB0, hNT0⟩ :=
    ctx.realAt (ctx.hBlocksCons ▸ List.getElem?_cons_zero)
  let env0 : Source.Environment := Array.replicate (valueCount ctx.norm) none
  have hEnv0Sz : env0.size = valueCount ctx.norm := by simp [env0]
  have hSimAt : SimInv ctx p0 ⟨ctx.offset + 0, 0, env0⟩ :=
    ⟨hP0Pure, ⟨0, ctx.entryBlk, nb0, lb0, ctx.hBlocksCons ▸ List.getElem?_cons_zero, hP0Blk, rfl,
      hNorm0, hBody0, hEmit0, hSrc0, hNbP0, hNbB0, hNT0,
      by rw [hP0Pc]; exact Nat.zero_le _, by simp [hP0Pc]⟩, hEnv0Sz,
      fun _ _ h => by cases h⟩
  cases hEPc : ctx.entryHasPreds with
  | false =>
    have hOff0 : ctx.offset = 0 := by simp [LCBCtx.offset, hEPc]
    have hLb0Phis : lb0.phis = #[] := by
      obtain ⟨phis, _, _, hPhis, _, _, hEq⟩ := emitBlock_ok_inv hEmit0
      rw [hOff0] at hPhis
      cases Except.ok.inj hPhis; rw [hEq]
    have hInit : Source.initial output.body #[] = .ok ⟨0, 0, env0⟩ := by
      have hB0 : output.body.blocks[0]? = some lb0 := by simpa [hOff0] using hBody0
      have hInitEq := initial_no_params (f := output.body) (entry := lb0)
        (by rw [ctx.hBodyEq, ctx.paramCount_zero]; rfl) hB0 (by rw [hLb0Phis]; rfl)
      rw [ctx.hBodyEq] at hInitEq ⊢
      simpa [env0] using hInitEq
    exact ⟨⟨0, 0, env0⟩, 0, ⟨0, 0, env0⟩, hInit, .zero _, by simpa [hOff0] using hSimAt⟩
  | true =>
    have hOff1 : ctx.offset = 1 := by simp [LCBCtx.offset, hEPc]
    let trampNb : NormBlock sLow :=
      ⟨none, [], [], .jumpEntryParams 1 ctx.entryBlk.params.length⟩
    have hTrampNorm : ctx.norm.blocks[0]? = some trampNb := by
      rw [ctx.hNormBlksEq]
      simp [hEPc, trampNb]
    have hTrampSBIdx : (indexed ctx.norm.blocks)[0]? = some (0, trampNb) := by
      rw [Backend.Runtime.getElem?_indexed, hTrampNorm]; rfl
    obtain ⟨trampLb, hTrampLbGet, hTrampEmit⟩ := (mapM_except_getElem? ctx.hSBlocks).2 hTrampSBIdx
    have hTrampBody : output.body.blocks[0]? = some trampLb := by
      rw [ctx.hBodyEq]; simpa only [List.getElem?_toArray] using hTrampLbGet
    obtain ⟨tPhis, tXss, tTerm, hTPhis, hTInsts, hTTerm, hTLbEq⟩ := emitBlock_ok_inv hTrampEmit
    have hTPhisNil : tPhis = [] := (Except.ok.inj hTPhis).symm
    have hTXssNil : tXss = [] := (Except.ok.inj hTInsts).symm
    have hTTermEq : tTerm = .jump 1 := (Except.ok.inj hTTerm).symm
    subst hTPhisNil; subst hTXssNil; subst hTTermEq
    have hInit : Source.initial output.body #[] = .ok ⟨0, 0, env0⟩ := by
      have hInitEq := initial_no_params (f := output.body) (entry := trampLb)
        (by rw [ctx.hBodyEq, ctx.paramCount_zero]; rfl) hTrampBody (by rw [hTLbEq]; rfl)
      rw [ctx.hBodyEq] at hInitEq ⊢
      simpa [env0] using hInitEq
    have hLb0Phis : lb0.phis = #[] := by
      obtain ⟨phis, _, _, hPhis, _, _, hEq⟩ := emitBlock_ok_inv hEmit0
      rw [hNbP0, ctx.hEntryParamsEmpty] at hPhis
      split at hPhis
      · cases Except.ok.inj hPhis; rw [hEq]; rfl
      · cases Except.ok.inj hPhis; rw [hEq]; rfl
    have hStepTramp : Source.Step output.body ⟨0, 0, env0⟩ ⟨1, 0, env0⟩ :=
      step_jump hTrampBody (by rw [hTLbEq]; rfl) (by rw [hTLbEq])
        (transfer_no_phis (by simpa [hOff1] using hBody0) hLb0Phis)
    exact ⟨⟨0, 0, env0⟩, 1, ⟨1, 0, env0⟩, hInit, .succ hStepTramp (.zero _),
      by simpa [hOff1] using hSimAt⟩

private theorem bir_progress_or_term {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (ctx : LCBCtx bir output)
    (hSafe : ProgramSafe (BIR.Execution (language := sliceLanguage slice)) bir)
    {p : MemoryRegisters (sliceLanguage slice) profile} {q : Source.State}
    (hSim : SimInv ctx p q)
    (hFin : (BIR.Execution (language := sliceLanguage slice)).finite bir p) :
    (∃ obs, (BIR.Execution (language := sliceLanguage slice)).result bir p obs) ∨
    (∃ pNext, (BIR.Execution (language := sliceLanguage slice)).step bir p pNext) := by
  cases hRes : BIR.hostedResult (language := sliceLanguage slice) bir p with
  | some obs => exact Or.inl ⟨obs, hRes⟩
  | none =>
    cases hEx : execBIRStep bir p with
    | some pNext => exact Or.inr ⟨pNext, execBIRStep_sound hSim.pure hEx⟩
    | none =>
      exfalso
      apply hSafe p
      refine ⟨hFin, fun nxt hS => ?_, fun o hR => ?_⟩
      · have hDet := (sim_step_and_det ctx hSim hS).1
        rw [hEx] at hDet; cases hDet
      · change BIR.hostedResult (language := sliceLanguage slice) bir p = some o at hR
        rw [hRes] at hR; cases hR

/-- Safe-source backward `BehaviorRefinement` for `EmitLCB.emitUnvalidated` from `BIR.Execution`
to pinned `LCBInput.Execution` (`Concrete.Backend.lcbSemanticsFor`), proved constructively via
`Concrete.Backend.LCBWordLemmas` without `Classical.choice` or `sorry`. -/
theorem emitLCB_refines {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (hWF : BIR.WF (language := sliceLanguage slice) bir)
    (hEmit : EmitLCB.emitUnvalidated (language := sliceLanguage slice) bir = .ok output) :
    BehaviorRefinement
      (BIR.Execution (language := sliceLanguage slice))
      (LCBInput.Execution (sliceLanguage slice).operands.Event bir.project)
      (BIR.Execution (language := sliceLanguage slice)).result
      (LCBInput.Execution (sliceLanguage slice).operands.Event bir.project).result
      bir output := by
  let sLang := sliceLanguage slice
  let sLow := sliceLoweringLanguage slice
  let birExec := BIR.Execution (language := sLang) (lang := sLow) (profile := profile)
  let lcbExec := LCBInput.Execution (profile := profile) sLang.operands.Event bir.project
  intro hSafe
  obtain ⟨ctx⟩ := buildLCBCtx hWF hEmit hSafe
  obtain ⟨q0, k0, q1, hInit0, hStepsInit, hSim0⟩ := sim_init ctx
  let p0 := BIR.entryState (language := sLang) bir
  let birRun : Nat → MemoryRegisters sLang profile := fun n =>
    Nat.rec p0 (fun _ prev => (execBIRStep bir prev).getD prev) n
  have hBirRunSucc : ∀ n, birRun (n + 1) = (execBIRStep bir (birRun n)).getD (birRun n) := fun _ => rfl
  have hSnoc : ∀ {a b c}, Steps (birExec.step bir) a b → birExec.step bir b c →
      Steps (birExec.step bir) a c := by
    intro a b c hab hbc
    induction hab with
    | nil => exact .cons hbc (.nil _)
    | cons h _ ihs => exact .cons h (ihs hbc)
  have hStepsFromRun : ∀ m, (∀ i, i < m → birExec.step bir (birRun i) (birRun (i + 1))) →
      birExec.finite bir (birRun m) := by
    intro m hAll
    have go : ∀ k, k ≤ m → Steps (birExec.step bir) (birRun 0) (birRun k) := by
      intro k hk
      induction k with
      | zero => exact .nil _
      | succ k' ih =>
        exact hSnoc (ih (Nat.le_of_succ_le hk)) (hAll k' (Nat.lt_of_lt_of_le (Nat.lt_succ_self k') hk))
    exact ⟨birRun 0, rfl, go m (Nat.le_refl _)⟩
  have hSrcStepDet : ∀ {a b c}, Source.Step output.body a b → Source.Step output.body a c → b = c := by
    intro a b c h1 h2
    unfold Source.Step at h1 h2
    rw [h1] at h2
    exact Source.Transition.next.inj (Except.ok.inj h2)
  have lcb_steps_covered : ∀ n q, Source.Steps output.body n q0 q →
      ∃ m r qNext,
        (∀ i, i < m → birExec.step bir (birRun i) (birRun (i + 1))) ∧
        SimInv ctx (birRun m) qNext ∧
        Source.Steps output.body r q qNext := by
    intro n
    induction n with
    | zero =>
      intro q hSt
      cases hSt
      exact ⟨0, k0, q1, fun _ hi => absurd hi (Nat.not_lt_zero _), hSim0, hStepsInit⟩
    | succ n' ih =>
      intro q hSt
      have hSnocInv : ∀ {k a c}, Source.Steps output.body (k + 1) a c →
          ∃ b, Source.Steps output.body k a b ∧ Source.Step output.body b c := by
        intro k
        induction k with
        | zero =>
          intro a c h
          cases h with
          | succ h1 hRest => cases hRest; exact ⟨a, .zero _, h1⟩
        | succ k' ihk =>
          intro a c h
          cases h with
          | succ h1 hRest =>
            obtain ⟨b, hb1, hb2⟩ := ihk hRest
            exact ⟨b, .succ h1 hb1, hb2⟩
      obtain ⟨qPrev, hPrevSt, hLastStep⟩ := hSnocInv hSt
      obtain ⟨m, r, qNext, hChain, hSimM, hRem⟩ := ih qPrev hPrevSt
      cases r with
      | succ r' =>
        cases hRem with
        | succ hStep1 hRem' =>
          have hEq := hSrcStepDet hLastStep hStep1
          subst hEq
          exact ⟨m, r', qNext, hChain, hSimM, hRem'⟩
      | zero =>
        cases hRem
        have hFinM := hStepsFromRun m hChain
        have hStepM : ∃ pNext, birExec.step bir (birRun m) pNext := by
          rcases bir_progress_or_term ctx hSafe hSimM hFinM with ⟨obs, hR⟩ | hEx
          · obtain ⟨_, hRet, _, _⟩ := sim_terminal ctx hSimM hR
            exact (returned_not_step hRet hLastStep).elim
          · exact hEx
        obtain ⟨pNext, hBirStep⟩ := hStepM
        obtain ⟨hDet, k, qNext', hkPos, hStepsK, hSimNext⟩ := sim_step_and_det ctx hSimM hBirStep
        have hRunEq : birRun (m + 1) = pNext := by rw [hBirRunSucc, hDet]; rfl
        have hChain' : ∀ i, i < m + 1 → birExec.step bir (birRun i) (birRun (i + 1)) := by
          intro i hi
          rcases Nat.lt_or_eq_of_le (Nat.le_of_succ_le_succ hi) with hlt | rfl
          · exact hChain i hlt
          · rw [hRunEq]; exact hBirStep
        cases k with
        | zero => cases hkPos
        | succ k' =>
          cases hStepsK with
          | succ hStep1 hRestK =>
            have hEq := hSrcStepDet hLastStep hStep1
            subst hEq
            exact ⟨m + 1, k', qNext', hChain', hRunEq.symm ▸ hSimNext, hRestK⟩
  have lcb_finite_to_steps : ∀ {qOpt}, lcbExec.finite output qOpt →
      ∃ n q, qOpt = some q ∧ Source.Steps output.body n q0 q := by
    intro qOpt hFin
    obtain ⟨start, hInit, hSteps⟩ := hFin
    have hStartEq : start = some q0 := by
      change start = (Source.initial output.body #[]).toOption at hInit
      rw [hInit0] at hInit; exact hInit
    subst hStartEq
    have go : ∀ {a b}, Steps (lcbExec.step output) a b →
        ∀ sa, a = some sa → ∃ n sb, b = some sb ∧ Source.Steps output.body n sa sb := by
      intro a b hSt
      induction hSt with
      | nil _ => intro sa ha; exact ⟨0, sa, ha, .zero _⟩
      | cons h1 _ ih =>
        intro sa ha
        obtain ⟨s1, s2, hEq1, hEq2, hStep1⟩ := h1
        rw [ha] at hEq1; cases Option.some.inj hEq1
        obtain ⟨n, sb, hEqB, hRest⟩ := ih s2 hEq2
        exact ⟨n + 1, sb, hEqB, .succ hStep1 hRest⟩
    exact go hSteps q0 rfl
  refine ⟨?_, ?_, ?_⟩
  · -- 1. Backward termination
    intro qOpt obs hFin hRes
    obtain ⟨n, q, rfl, hStepsQ⟩ := lcb_finite_to_steps hFin
    change (LCBInput.Execution (sliceLanguage slice).operands.Event bir.project).result output (some q) obs at hRes
    rw [lcbExecution_result_iff] at hRes
    change (match Source.step output.body q with | .ok (.returned w) => lcbRuntimeResult bir.project output w | _ => none) = some obs at hRes
    cases hStQ : Source.step output.body q with
    | error _ => rw [hStQ] at hRes; cases hRes
    | ok tr =>
      cases tr with
      | next _ => rw [hStQ] at hRes; cases hRes
      | returned w =>
        rw [hStQ] at hRes; dsimp only at hRes
        obtain ⟨m, r, qNext, hChain, hSimM, hRem⟩ := lcb_steps_covered n q hStepsQ
        cases r with
        | succ r' =>
          cases hRem with
          | succ hStep1 _ => exact (returned_not_step hStQ hStep1).elim
        | zero =>
          cases hRem
          have hFinM := hStepsFromRun m hChain
          have hResM : ∃ obs', birExec.result bir (birRun m) obs' := by
            rcases bir_progress_or_term ctx hSafe hSimM hFinM with hEx | ⟨nxt, hS⟩
            · exact hEx
            · obtain ⟨_, k, _, hkPos, hStepsK, _⟩ := sim_step_and_det ctx hSimM hS
              cases k with
              | zero => cases hkPos
              | succ _ =>
                cases hStepsK with
                | succ hStep1 _ => exact (returned_not_step hStQ hStep1).elim
          obtain ⟨obs', hObs'⟩ := hResM
          obtain ⟨w', hStQ', hRt', hTrEq⟩ := sim_terminal ctx hSimM hObs'
          rw [hStQ] at hStQ'
          have hWEq : w = w' := Source.Transition.returned.inj (Except.ok.inj hStQ')
          subst hWEq
          rw [hRes] at hRt'
          cases Option.some.inj hRt'
          exact ⟨birRun m, hFinM, hObs', hTrEq⟩
  · -- 2. Constructive backward divergence (ZERO `Classical.choice`)
    intro qRun hDiv
    obtain ⟨hQRun0, hQRunStep⟩ := hDiv
    have hQ0Eq : qRun 0 = some q0 := by
      change qRun 0 = (Source.initial output.body #[]).toOption at hQRun0
      rw [hInit0] at hQRun0; exact hQRun0
    have hQRunFollow : ∀ {n s t k}, qRun n = some s → Source.Steps output.body k s t →
        qRun (n + k) = some t := by
      intro n s t k hAt hSt
      induction hSt generalizing n with
      | zero _ => simpa using hAt
      | succ h1 hRest ih =>
        obtain ⟨sa, sb, hA, hB, hStepAB⟩ := hQRunStep n
        rw [hAt] at hA; cases Option.some.inj hA
        have hEq := hSrcStepDet hStepAB h1
        subst hEq
        have hNext := ih hB
        simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using hNext
    have hBirDivChain : ∀ m, (∃ n q, qRun n = some q ∧ SimInv ctx (birRun m) q) ∧
        birExec.step bir (birRun m) (birRun (m + 1)) ∧
        birExec.finite bir (birRun m) := by
      intro m
      induction m with
      | zero =>
        have hAtK0 : qRun k0 = some q1 := by simpa using hQRunFollow hQ0Eq hStepsInit
        have hFin0 : birExec.finite bir (birRun 0) := ⟨birRun 0, rfl, .nil _⟩
        have hStep0 : ∃ p1 : MemoryRegisters sLang profile, birExec.step bir (birRun 0) p1 := by
          rcases bir_progress_or_term ctx hSafe hSim0 hFin0 with ⟨_, hR⟩ | hEx
          · obtain ⟨_, hRet, _, _⟩ := sim_terminal ctx hSim0 hR
            obtain ⟨sa, sb, hA, _, hSt⟩ := hQRunStep k0
            rw [hAtK0] at hA; cases Option.some.inj hA
            exact (returned_not_step hRet hSt).elim
          · exact hEx
        obtain ⟨p1, hBir0⟩ := hStep0
        obtain ⟨hDet, _⟩ := sim_step_and_det ctx hSim0 hBir0
        have hRun1 : birRun 1 = p1 := by
          change (execBIRStep bir (BIR.entryState (language := sliceLanguage slice) bir)).getD
            (BIR.entryState (language := sliceLanguage slice) bir) = p1
          rw [hDet]; rfl
        exact ⟨⟨k0, q1, hAtK0, hSim0⟩, hRun1.symm ▸ hBir0, hFin0⟩
      | succ m' ih =>
        obtain ⟨⟨n, q, hAtN, hSimM⟩, hStepM, hFinM⟩ := ih
        obtain ⟨_, k, q', _, hStepsK, hSimNext⟩ := sim_step_and_det ctx hSimM hStepM
        have hAtNK : qRun (n + k) = some q' := hQRunFollow hAtN hStepsK
        have hFinNext : birExec.finite bir (birRun (m' + 1)) := by
          obtain ⟨s0, hI0, hSt0⟩ := hFinM
          exact ⟨s0, hI0, hSnoc hSt0 hStepM⟩
        have hStepNext : ∃ pNext : MemoryRegisters sLang profile,
            birExec.step bir (birRun (m' + 1)) pNext := by
          rcases bir_progress_or_term ctx hSafe hSimNext hFinNext with ⟨_, hR⟩ | hEx
          · obtain ⟨_, hRet, _, _⟩ := sim_terminal ctx hSimNext hR
            obtain ⟨sa, sb, hA, _, hSt⟩ := hQRunStep (n + k)
            rw [hAtNK] at hA; cases Option.some.inj hA
            exact (returned_not_step hRet hSt).elim
          · exact hEx
        obtain ⟨pNext, hBirNext⟩ := hStepNext
        obtain ⟨hDet, _⟩ := sim_step_and_det ctx hSimNext hBirNext
        have hRunNext : birRun (m' + 2) = pNext := by
          change (execBIRStep bir (birRun (m' + 1))).getD (birRun (m' + 1)) = pNext
          rw [hDet]; rfl
        exact ⟨⟨n + k, q', hAtNK, hSimNext⟩, hRunNext.symm ▸ hBirNext, hFinNext⟩
    refine ⟨birRun, ⟨rfl, fun i => (hBirDivChain i).2.1⟩, ?_⟩
    have hBirTrEmpty : ∀ i,
        (BIR.Execution (language := sliceLanguage slice)).trace bir (birRun i) = [] := by
      intro i
      obtain ⟨⟨_, _, _, hSimI⟩, hStepI, _⟩ := hBirDivChain i
      have hNone : bir.hostedResult (birRun i) = none := hStepI.1
      change ((birRun i).effects.map ProcessEvent.effect ++
        ((bir.hostedResult (birRun i)).map fun o => o.stderr.data.toList.map ProcessEvent.stderr).getD []) = []
      rw [hSimI.pure.effects, hNone]; rfl
    have hLcbTrEmpty : ∀ j,
        (LCBInput.Execution (sliceLanguage slice).operands.Event bir.project).trace output (qRun j) = [] := by
      intro j
      obtain ⟨sa, sb, hA, _, hStepAB⟩ := hQRunStep j
      rw [hA, lcbExecution_trace_eq]
      unfold Source.Step at hStepAB
      simp [hStepAB]
    intro events
    simp only [hBirTrEmpty, hLcbTrEmpty]
  · -- 3. Target safety (`ProgramSafe lcbExec output`)
    intro qOpt hStuck
    obtain ⟨hFin, hNoStep, hNoRes⟩ := hStuck
    obtain ⟨n, q, rfl, hStepsQ⟩ := lcb_finite_to_steps hFin
    obtain ⟨m, r, qNext, hChain, hSimM, hRem⟩ := lcb_steps_covered n q hStepsQ
    cases r with
    | succ r' =>
      cases hRem with
      | succ hStep1 _ =>
        exact hNoStep (some _) ⟨q, _, rfl, rfl, hStep1⟩
    | zero =>
      cases hRem
      have hFinM := hStepsFromRun m hChain
      apply hSafe (birRun m)
      refine ⟨hFinM, fun nxt hS => ?_, fun obs hR => ?_⟩
      · obtain ⟨_, k, _, hkPos, hStepsK, _⟩ := sim_step_and_det ctx hSimM hS
        cases k with
        | zero => cases hkPos
        | succ _ =>
          cases hStepsK with
          | succ hStep1 _ => exact hNoStep (some _) ⟨q, _, rfl, rfl, hStep1⟩
      · obtain ⟨w, hRet, hRt, _⟩ := sim_terminal ctx hSimM hR
        apply hNoRes obs
        rw [lcbExecution_result_iff]
        simp [hRet, hRt]

/-! ## 8. Full `LCBEncoding`, `EmitsLCB`, refusal classification, and unified stage validator -/

/-- Every successful `EmitLCB.emitUnvalidated` on a well-formed `BIR` establishes the full
`LCBEncoding` contract (combining `EmitLCB.emitUnvalidated_structural` and `emitLCB_refines`).
Depends only on `[propext, Quot.sound]` (zero `Classical.choice`). -/
theorem emitLCB_encoding {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (hWF : BIR.WF (language := sliceLanguage slice) bir)
    (hEmit : EmitLCB.emitUnvalidated (language := sliceLanguage slice) bir = .ok output) :
    LCBEncoding (sliceLanguage slice) bir output := by
  obtain ⟨_, _, hSymNd, hSites, hRt, hFnNd, hAddNd, hBij, hLink, hTraps⟩ :=
    EmitLCB.emitUnvalidated_structural hWF hEmit
  exact ⟨hSymNd, hSites, hRt, emitLCB_refines hWF hEmit, hFnNd, hAddNd, hBij, hLink, hTraps⟩

/-- Every successful `EmitLCB.emit` on a well-formed `BIR` establishes `firstUnsupported = none`
and the full `EmitsLCB` contract. -/
theorem emitLCB_emitsLCB {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {output : LCBInput profile}
    (hWF : BIR.WF (language := sliceLanguage slice) bir)
    (hEmit : EmitLCB.emit (language := sliceLanguage slice) bir = .ok output) :
    firstUnsupported (language := sliceLanguage slice) bir = none ∧
    EmitsLCB (sliceLanguage slice) bir output := by
  have hUnval : EmitLCB.emitUnvalidated (language := sliceLanguage slice) bir = .ok output := by
    unfold EmitLCB.emit at hEmit
    cases hU : EmitLCB.emitUnvalidated (language := sliceLanguage slice) bir with
    | error _ => rw [hU] at hEmit; cases hEmit
    | ok out =>
      rw [hU] at hEmit
      cases hV : EmitLCB.validateJobs out with
      | error _ => simp [hV, Except.mapError, bind, Except.bind] at hEmit
      | ok u =>
        cases u
        simp only [hV, Except.mapError, bind, Except.bind, pure, Except.pure, Except.ok.injEq] at hEmit
        subst hEmit; rfl
  obtain ⟨hBackend, hSup, _⟩ := EmitLCB.emitUnvalidated_structural hWF hUnval
  obtain ⟨hValBody, hValAdd⟩ := EmitLCB.emit_validated_jobs hEmit
  exact ⟨hSup, hBackend, hValBody, hValAdd, emitLCB_encoding hWF hUnval⟩

/-- Complete 4-branch refusal classification for `EmitLCB.emit` on any well-formed LCB `BIR`,
including the `LCBEncoding` conjunct in Case 4 matching `PassFunctions.emitEntry.refusal`. -/
theorem emitLCB_error_of_wf {slice : Slice} {profile : Profile}
    {bir : BIR (sliceLoweringLanguage slice) profile} {failure : StageFailure}
    (hBackend : profile.backend = .lcb)
    (hWF : BIR.WF (language := sliceLanguage slice) bir)
    (hErr : EmitLCB.emit (language := sliceLanguage slice) bir = .error failure) :
    (LCBInput.trapCapacity < bir.sites.size ∧
      failure = .resource (LCBInput.trapCapacityRefusal bir.sites.size)) ∨
    (¬ (LCBInput.trapCapacity < bir.sites.size) ∧
      bir.requiresLCBLinking = true ∧
      failure = .lcbLinkingUnavailable) ∨
    (¬ (LCBInput.trapCapacity < bir.sites.size) ∧
      bir.requiresLCBLinking = false ∧
      ∃ cause, firstUnsupported (language := sliceLanguage slice) bir = some cause ∧
        failure = .unsupportedFeature profile cause) ∨
    (¬ (LCBInput.trapCapacity < bir.sites.size) ∧
      bir.requiresLCBLinking = false ∧
      firstUnsupported (language := sliceLanguage slice) bir = none ∧
      ∃ reason output, EmitLCB.emitUnvalidated (language := sliceLanguage slice) bir = .ok output ∧
        LCBEncoding (sliceLanguage slice) bir output ∧
        EmitLCB.validateJobs output = .error reason ∧
        failure = .invalidLCBJob reason) := by
  rcases EmitLCB.emit_error_of_wf hBackend hWF hErr with
    h1 | h2 | h3 | ⟨hCap, hLink, hSup, reason, output, hUnval, hVal, hFail⟩
  · exact Or.inl h1
  · exact Or.inr (Or.inl h2)
  · exact Or.inr (Or.inr (Or.inl h3))
  · exact Or.inr (Or.inr (Or.inr ⟨hCap, hLink, hSup, reason, output, hUnval,
      emitLCB_encoding hWF hUnval, hVal, hFail⟩))

/-- Universal `(registry passes).emit.Validated` theorem for the unified profile-selected
emitter across both LLVM and LCB profiles (closing `ISS-0115` and `ISS-0117` alignment at the
registered emitter stage). -/
theorem emit_validated {slice : Slice} {profile : Profile}
    (passes : Passes slice profile)
    (hEmit : passes.emit = emit (language := sliceLanguage slice)) :
    (registry passes).emit.Validated := by
  cases profile with
  | mk backend target maxSlots maxStack =>
    cases backend with
    | llvm =>
      constructor
      · intro bir output hWF hRun
        change emitGuarded (language := sliceLanguage slice) passes.emit bir = .ok output at hRun
        rw [hEmit, emitGuarded_emit_llvm] at hRun
        have hSup := EmitLLVM.emit_firstUnsupported_none hRun
        have hMeta := EmitLLVM.emit_metadataMatches hRun
        exact ⟨⟨bir, hMeta⟩, hSup, hMeta⟩
      · intro bir failure hWF hRun
        change emitGuarded (language := sliceLanguage slice) passes.emit bir = .error failure at hRun
        rw [hEmit, emitGuarded_emit_llvm] at hRun
        exact EmitLLVM.emit_error_of_wf rfl hWF hRun
    | lcb =>
      constructor
      · intro bir output hWF hRun
        change emitGuarded (language := sliceLanguage slice) passes.emit bir = .ok output at hRun
        rw [hEmit, emitGuarded_emit_lcb] at hRun
        obtain ⟨hSup, hEmits⟩ := emitLCB_emitsLCB hWF hRun
        exact ⟨⟨bir, hEmits⟩, hSup, hEmits⟩
      · intro bir failure hWF hRun
        change emitGuarded (language := sliceLanguage slice) passes.emit bir = .error failure at hRun
        rw [hEmit, emitGuarded_emit_lcb] at hRun
        rcases emitLCB_error_of_wf rfl hWF hRun with
          h1 | h2 | h3 | ⟨hCap, hLink, hSup, reason, output, hUnval, hEnc, hVal, hFail⟩
        · exact Or.inl h1
        · exact Or.inr (Or.inl h2)
        · exact Or.inr (Or.inr (Or.inl h3))
        · exact Or.inr (Or.inr (Or.inr ⟨hCap, hLink, hSup, reason, output,
            by rw [hEmit]; exact hUnval, hEnc, hVal, hFail⟩))

/-- LLVM-only specialization of `emit_validated` (introduces zero local `Classical.choice`). -/
theorem emit_llvm_validated {slice : Slice} {target : Backend.LCB.Target} {maxSlots maxStack : Nat}
    (passes : Passes slice ⟨.llvm, target, maxSlots, maxStack⟩)
    (hEmit : passes.emit = emit (language := sliceLanguage slice)) :
    (registry passes).emit.Validated := by
  constructor
  · intro bir output _ hRun
    change emitGuarded (language := sliceLanguage slice) passes.emit bir = .ok output at hRun
    rw [hEmit, emitGuarded_emit_llvm] at hRun
    have hSup := EmitLLVM.emit_firstUnsupported_none hRun
    have hMeta := EmitLLVM.emit_metadataMatches hRun
    exact ⟨⟨bir, hMeta⟩, hSup, hMeta⟩
  · intro bir failure hWF hRun
    change emitGuarded (language := sliceLanguage slice) passes.emit bir = .error failure at hRun
    rw [hEmit, emitGuarded_emit_llvm] at hRun
    exact EmitLLVM.emit_error_of_wf rfl hWF hRun

/-- Unconditional `Stage.Sound passes .emit` on every LCB profile (`trust` is `True`). -/
theorem emit_sound_lcb {slice : Slice} {profile : Profile}
    (passes : Passes slice profile)
    (hBackend : profile.backend = .lcb)
    (hEmit : passes.emit = emit (language := sliceLanguage slice)) :
    Stage.Sound passes .emit :=
  ⟨emit_validated passes hEmit, PassFunctions.emitEntry_trusted_of_lcb passes hBackend⟩

/-- Conditional `Stage.Sound passes .emit` on every LLVM profile, discharging all structural
and refusal obligations via `emit_llvm_validated` and isolating semantic soundness strictly
to the trusted `P_print` premise. -/
theorem emit_sound_llvm_of_print {slice : Slice} {target : Backend.LCB.Target}
    {maxSlots maxStack : Nat}
    (passes : Passes slice ⟨.llvm, target, maxSlots, maxStack⟩)
    (hEmit : passes.emit = emit (language := sliceLanguage slice))
    (hPrint : ∀ (bir : BIR (sliceLoweringLanguage slice) ⟨.llvm, target, maxSlots, maxStack⟩)
      (output : LLVMInput ⟨.llvm, target, maxSlots, maxStack⟩),
      BIR.WF (language := sliceLanguage slice) bir →
      EmitLLVM.emit (language := sliceLanguage slice) bir = .ok output →
      P_print (language := sliceLanguage slice) bir output) :
    Stage.Sound passes .emit := by
  refine ⟨emit_llvm_validated passes hEmit, ?_⟩
  intro bir output hWF hRun
  change emitGuarded (language := sliceLanguage slice) passes.emit bir = .ok output at hRun
  rw [hEmit, emitGuarded_emit_llvm] at hRun
  exact hPrint bir output hWF hRun

end Concrete.Backend.ValidateEmit

