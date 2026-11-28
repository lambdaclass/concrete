import ConcreteSpec.StageInterface
import ConcreteSpec.Spec.Slice0Layout
import Concrete.Interfaces.Carriers
import Concrete.Interfaces.Contracts
import Concrete.Interfaces.Pipeline

/-! Standalone freeze controls for MIR materializability (`Materializable`,
`MIRHandoffWF`), typed-root retention (`RetainsTypedRoots`, `LowersMemory.materializable`,
`LowersMemory.memoryRoot_declared`), declaration well-formedness (`DeclarationsWF`),
storage promotion (`PromotionSound`), and recursive storage geometry (`Layout.WellFormed`,
`Memory.LayoutsWF`, `Memory.HandoffWF`, including ISS-0125 strict alignment ceiling and
sibling-field disjointness).

Imports only frozen/merged `ConcreteSpec` and `Concrete` modules (no dependency
on `ConcreteChecks/StageInterfaces/Fixtures.lean`). -/

namespace ConcreteChecks.StageInterfaces.Memory.MaterializabilityControls

open Concrete
open Concrete.Interfaces
open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan
open ConcreteSpec.StageInterface

/-! ## Shared standalone fixtures (directly over `sliceLanguage` / `sliceLoweringLanguage`) -/

private def ctrlProfile : Profile := ⟨.lcb, .x86_64Linux, 8, 4096⟩
private def ctrlTarget : ConcreteSpec.Core.Target := ctrlProfile.memoryTarget

private abbrev lang0 := sliceLanguage slice0
private abbrev lower0 := sliceLoweringLanguage slice0
private abbrev lang1 := sliceLanguage sliceOne
private abbrev lower1 := sliceLoweringLanguage sliceOne

private def ctrlSourceText : SourceText :=
  SourceText.ofBytes r"fn main() -> u8 { return 0; }".toUTF8.data.toList

private def ctrlProject : Project :=
  Project.singleton mainSourcePath ctrlSourceText

private def ctrlMod : ModuleId :=
  ⟨⟨⟨"ctrl"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩

private def mainFn : FnId := ⟨ctrlMod, ⟨"main"⟩⟩
private def helperFn : FnId := ⟨ctrlMod, ⟨"helper"⟩⟩

private def ctrlSpan : FileSpan := ⟨⟨0⟩, 0, 2, 1, 1⟩

private theorem ctrlSpan_binds : ∃ span, ctrlSpan.Binds ctrlProject span := by
  refine ⟨⟨⟨0⟩, 0, 2, by decide, 1, 1, by decide, by decide⟩, ?_⟩
  refine ⟨File.load mainSourcePath ctrlSourceText, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private def ctrlOrigin : Origin := ⟨⟨0⟩, ctrlSpan, ⟨.function mainFn, ⟨[]⟩⟩⟩

private def p0 : PlaceId := ⟨0⟩
private def p1 : PlaceId := ⟨1⟩

private def emptyDeclId : DeclId := ⟨ctrlMod, ⟨"Empty"⟩⟩
private def ghostDeclId : DeclId := ⟨ctrlMod, ⟨"Ghost"⟩⟩
private def mainDeclId : DeclId := ⟨ctrlMod, ⟨"main"⟩⟩

private def emptyDecl : ConcreteSpec.StructDecl := ⟨⟨"Empty"⟩, false, []⟩
private def ghostDecl : ConcreteSpec.StructDecl := ⟨⟨"Ghost"⟩, false, []⟩
private def mainDecl : ConcreteSpec.StructDecl := ⟨⟨"main"⟩, false, []⟩

private def u8Ty : EarlyTy := .scalar (.int .u8)
private def u32Ty : EarlyTy := .scalar (.int .u32)
private def unitTy : EarlyTy := .scalar .unit
private def emptyNominalTy : EarlyTy := .nominal emptyDeclId
private def ghostNominalTy : EarlyTy := .nominal ghostDeclId

private def retZeroMirBlock {s : Slice} (body : List (LocatedOp (MIRInst (sliceLoweringLanguage s)))) :
    Block (Sigma (Place (sliceLanguage s).operands.Ty))
      (LocatedOp (MIRInst (sliceLoweringLanguage s)))
      (Terminator (Sigma (Read (sliceLanguage s).operands.Ty (sliceLoweringLanguage s).literal))
        (Read (sliceLanguage s).operands.Ty (sliceLoweringLanguage s).literal (sliceLoweringLanguage s).bool)) :=
  ⟨⟨0⟩, [], body, .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩

private def retZeroMemBlock {s : Slice} (body : List (LocatedOp (MemoryInst (sliceLoweringLanguage s)))) :
    MemoryBlock (sliceLoweringLanguage s) :=
  ⟨⟨0⟩, [], body, .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩

private def mainSig : FunctionSignature EarlyTy := ⟨[], some u8Ty⟩

private def mkMir {s : Slice} (decls : Declarations)
    (locals : LocalTypes EarlyTy)
    (body : List (LocatedOp (MIRInst (sliceLoweringLanguage s))) := []) :
    MIR (sliceLoweringLanguage s) ctrlProfile where
  project := ctrlProject
  entry := mainFn
  signatures := [(mainFn, mainSig)]
  functions := [(mainFn, locals, ⟨⟨0⟩, #[retZeroMirBlock body]⟩)]
  mutableLocals := []
  declarations := decls

private def mkMem {s : Slice} (decls : Declarations)
    (locals : LocalTypes EarlyTy)
    (storage : AList PlaceId StorageClass)
    (layouts : AList FnId (AList PlaceId Layout)) :
    Memory (sliceLoweringLanguage s) ctrlProfile where
  project := ctrlProject
  entry := mainFn
  signatures := [(mainFn, mainSig)]
  functions := [(mainFn, locals, ⟨⟨0⟩, #[retZeroMemBlock []]⟩)]
  layouts := layouts
  storage := [(mainFn, storage)]
  mutableLocals := []
  declarations := decls

private theorem nodup_one {α : Type} (x : α) : [x].Nodup :=
  List.nodup_cons.mpr ⟨(fun h => nomatch h), List.nodup_nil⟩

private theorem singleFn_logicalView_tablesWF {s : Slice} (decls : Declarations)
    (locals : LocalTypes EarlyTy) (hLocNodup : (List.map Prod.fst locals).Nodup)
    (body : List (LocatedOp (MIRInst (sliceLoweringLanguage s))))
    (hNoCalls : ∀ inst ∈ body, match inst.instruction with | .call .. => False | _ => True) :
    ((mkMir (s := s) decls locals body).logicalView (language := sliceLanguage s)).TablesWF := by
  refine ⟨nodup_one _, nodup_one _, nodup_one _, List.nodup_nil, ?_, ?_, ?_⟩
  · intro fn classes hmem
    cases List.mem_singleton.mp hmem
    change (List.map Prod.fst
      (List.map (fun e : PlaceId × EarlyTy => (e.fst, StorageClass.virtual)) locals)).Nodup
    rw [List.map_map]
    exact hLocNodup
  · intro _ _ h; cases h
  · intro fn locs graph hmem
    cases List.mem_singleton.mp hmem
    refine ⟨hLocNodup, ⟨?_, ?_, ?_⟩, ?_⟩
    · simp only [Array.toList_map]
      exact nodup_one _
    · simp only [Array.toList_map, List.map_cons, List.map_nil]
      exact ⟨_, List.mem_cons_self, rfl⟩
    · intro blk hblk edge hedge
      simp only [Array.toList_map, List.map_cons, List.map_nil, List.mem_singleton] at hblk
      subst hblk
      change edge ∈ [] at hedge
      cases hedge
    · refine ⟨mainSig, by simp [mkMir, MIR.logicalView, AList.lookup], ?_, ?_⟩
      · simp only [Array.toList_map, List.map_cons, List.map_nil]
        exact ⟨_, List.mem_cons_self, rfl, rfl⟩
      · intro blk hblk
        simp only [Array.toList_map, List.map_cons, List.map_nil, List.mem_singleton] at hblk
        subst hblk
        refine ⟨rfl, ?_⟩
        intro op hop
        simp only [List.mem_map] at hop
        obtain ⟨srcOp, hsrcMem, rfl⟩ := hop
        have hnc := hNoCalls srcOp hsrcMem
        cases hinst : srcOp.instruction with
        | call _ _ _ _ => rw [hinst] at hnc; exact False.elim hnc
        | bind _ _ | assign _ _ | op _ _ _ _ _ | endLifetime _ | destructure _ _ => trivial

private theorem singleFn_mem_tablesWF {s : Slice} (decls : Declarations)
    (locals : LocalTypes EarlyTy) (storage : AList PlaceId StorageClass)
    (layouts : AList FnId (AList PlaceId Layout))
    (hLocNodup : (List.map Prod.fst locals).Nodup)
    (hStorNodup : (List.map Prod.fst storage).Nodup)
    (hLayFnNodup : (List.map Prod.fst layouts).Nodup)
    (hLayNodup : ∀ fn slots, (fn, slots) ∈ layouts → (List.map Prod.fst slots).Nodup) :
    (mkMem (s := s) decls locals storage layouts).TablesWF := by
  refine ⟨nodup_one _, nodup_one _, nodup_one _, hLayFnNodup, ?_, hLayNodup, ?_⟩
  · intro fn classes hmem
    cases List.mem_singleton.mp hmem
    exact hStorNodup
  · intro fn locs graph hmem
    cases List.mem_singleton.mp hmem
    refine ⟨hLocNodup, ⟨nodup_one _, ⟨_, List.mem_cons_self, rfl⟩, ?_⟩, ?_⟩
    · intro blk hblk edge hedge
      cases List.mem_singleton.mp hblk
      cases hedge
    · refine ⟨mainSig, by simp [mkMem, AList.lookup], ⟨_, List.mem_cons_self, rfl, rfl⟩, ?_⟩
      intro blk hblk
      cases List.mem_singleton.mp hblk
      exact ⟨rfl, fun _ h => nomatch h⟩

private theorem singleFn_origins_bind_of_empty {s : Slice} (decls : Declarations)
    (locals : LocalTypes EarlyTy) :
    ∀ origin ∈ ((mkMir (s := s) decls locals []).logicalView (language := sliceLanguage s)).origins,
      ∃ span, origin.lexical.Binds ctrlProject span := by
  intro origin hmem
  simp only [Memory.origins, MIR.logicalView, mkMir, AList.map, CFG.origins, retZeroMirBlock,
    Terminator.origins, List.flatMap_cons, List.flatMap_nil, List.append_nil, Array.toList_map,
    List.map_cons, List.map_nil] at hmem
  change origin ∈ [] at hmem
  cases hmem

private theorem singleFn_origins_bind_of_ctrlOrigin {s : Slice} (decls : Declarations)
    (locals : LocalTypes EarlyTy) (inst : MIRInst (sliceLoweringLanguage s)) :
    ∀ origin ∈ ((mkMir (s := s) decls locals [⟨ctrlOrigin, inst⟩]).logicalView
      (language := sliceLanguage s)).origins,
      ∃ span, origin.lexical.Binds ctrlProject span := by
  intro origin hmem
  simp only [Memory.origins, MIR.logicalView, mkMir, AList.map, CFG.origins, retZeroMirBlock,
    Terminator.origins, List.flatMap_cons, List.flatMap_nil, List.append_nil, Array.toList_map,
    List.map_cons, List.map_nil] at hmem
  change origin ∈ [ctrlOrigin] at hmem
  cases List.mem_singleton.mp hmem
  exact ctrlSpan_binds

private theorem emptyDeclsWF : DeclarationsWF [] :=
  ⟨List.nodup_nil, ⟨List.nodup_nil, fun _ h => nomatch h⟩, fun _ _ h => nomatch h⟩

private theorem oneEmptyDeclWF : DeclarationsWF [(emptyDeclId, emptyDecl)] :=
  ⟨by decide, ⟨by decide, fun d hd => by cases List.mem_singleton.mp hd; exact ⟨by decide, List.nodup_nil⟩⟩,
   fun id decl h => by cases List.mem_singleton.mp h; rfl⟩

private theorem emptyMutableWF {Ty G : Type} (fns : AList FnId (LocalTypes Ty × G)) :
    MutableLocalsWF fns [] :=
  ⟨List.nodup_nil, fun _ _ h => nomatch h⟩

/-! ## 1. Positive witnesses: virtual scalar, escaping scalar, and empty nominal -/

-- 1a. Virtual scalar (`u8`, non-escaping): `Materializable`, `MIRHandoffWF`, `RetainsTypedRoots`,
-- `PromotionSound`, `LayoutsMatch`, `LayoutsWF`, and `Memory.HandoffWF` all hold.
private def virtualScalarMir : MIR lower0 ctrlProfile :=
  mkMir [] [(p0, u8Ty)] []

private def virtualScalarMem : Memory lower0 ctrlProfile :=
  mkMem [] [(p0, u8Ty)] [(p0, .virtual)] []

theorem virtual_scalar_materializable_and_retained :
    Materializable (language := lang0) virtualScalarMir ∧
    MIRHandoffWF (language := lang0) virtualScalarMir ∧
    RetainsTypedRoots virtualScalarMir virtualScalarMem ∧
    PromotionSound (language := lang0) virtualScalarMir virtualScalarMem ∧
    LayoutsMatch lang0.layout virtualScalarMem ∧
    virtualScalarMem.LayoutsWF ∧
    Memory.HandoffWF virtualScalarMem := by
  have hNoEsc : ¬ virtualScalarMir.Escapes (language := lang0) mainFn p0 := by
    rintro ⟨op, hop, _⟩
    simp [MIR.operations, virtualScalarMir, mkMir, retZeroMirBlock] at hop
  have hMat : Materializable (language := lang0) virtualScalarMir := by
    intro fn locs graph hfn place ty hloc hneed
    cases List.mem_singleton.mp hfn
    cases List.mem_singleton.mp hloc
    rcases hneed with hnone | hesc
    · cases hnone
    · exact False.elim (hNoEsc hesc)
  have hMirWF : MIRHandoffWF (language := lang0) virtualScalarMir :=
    ⟨⟨singleFn_logicalView_tablesWF [] [(p0, u8Ty)] (by decide) [] (fun _ h => nomatch h),
      by decide, singleFn_origins_bind_of_empty [] [(p0, u8Ty)],
      (fun _ h => nomatch h), emptyDeclsWF, emptyMutableWF _⟩, hMat⟩
  have hRet : RetainsTypedRoots virtualScalarMir virtualScalarMem :=
    RetainsTypedRoots.of_inventories_eq rfl (by decide)
      (fun fn locs graph hfn => by cases List.mem_singleton.mp hfn; decide)
  have hProm : PromotionSound (language := lang0) virtualScalarMir virtualScalarMem := by
    refine ⟨?_, ?_⟩
    · intro fn locs graph hfn place ty hloc
      cases List.mem_singleton.mp hfn
      cases List.mem_singleton.mp hloc
      exact ⟨[(p0, .virtual)], .virtual, by decide, by decide, ⟨fun _ => ⟨rfl, hNoEsc⟩, fun _ => rfl⟩⟩
    · intro fn classes place hfn hpl
      cases List.mem_singleton.mp hfn
      cases List.mem_singleton.mp hpl
      exact ⟨u8Ty, [(p0, u8Ty)], _, [(p0, u8Ty)], _,
        List.mem_cons_self, List.mem_cons_self, List.mem_cons_self, List.mem_cons_self, rfl, hNoEsc⟩
  have hLayMatch : LayoutsMatch lang0.layout virtualScalarMem := by
    refine ⟨?_, fun _ _ _ _ h => nomatch h⟩
    intro fn locs graph hfn place ty hloc
    cases List.mem_singleton.mp hfn
    cases List.mem_singleton.mp hloc
    exact ⟨[(p0, .virtual)], by decide, Or.inl (by decide)⟩
  have hLayWF : virtualScalarMem.LayoutsWF := fun _ h => nomatch h
  have hMemWF : Memory.HandoffWF virtualScalarMem :=
    ⟨singleFn_mem_tablesWF [] [(p0, u8Ty)] [(p0, .virtual)] [] (by decide) (by decide)
       List.nodup_nil (fun _ _ h => nomatch h),
     by decide,
     (fun _ h => by simp [Memory.origins, virtualScalarMem, mkMem, CFG.origins, retZeroMemBlock, Terminator.origins] at h),
     hLayWF, emptyDeclsWF, emptyMutableWF _⟩
  exact ⟨hMat, hMirWF, hRet, hProm, hLayMatch, hLayWF, hMemWF⟩

-- 1b. Escaping scalar (`u8`, address-taken): materialized with `Layout.scalar 1 1` (proved well-formed
-- via `sliceLanguage_layout_wf`), satisfying `Materializable`, `MIRHandoffWF`, `RetainsTypedRoots`,
-- `PromotionSound`, `LayoutsMatch`, `LayoutsWF`, and `Memory.HandoffWF`.
private def escapeAddrInst {s : Slice} (ty : EarlyTy) (place : PlaceId) :
    LocatedOp (MIRInst (sliceLoweringLanguage s)) :=
  ⟨ctrlOrigin, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty)
    ⟨p0, []⟩ () (.cons (.literal ⟨[0]⟩) .nil) [] [⟨ty, ⟨place, []⟩⟩]⟩

private def escapingU8Mir : MIR lower0 ctrlProfile :=
  mkMir [] [(p0, u8Ty)] [escapeAddrInst u8Ty p0]

private def escapingU8Mem : Memory lower0 ctrlProfile :=
  mkMem [] [(p0, u8Ty)] [(p0, .materialized)] [(mainFn, [(p0, Layout.scalar 1 1)])]

theorem escaping_u8_materializable_and_retained :
    escapingU8Mir.Escapes (language := lang0) mainFn p0 ∧
    Materializable (language := lang0) escapingU8Mir ∧
    MIRHandoffWF (language := lang0) escapingU8Mir ∧
    RetainsTypedRoots escapingU8Mir escapingU8Mem ∧
    PromotionSound (language := lang0) escapingU8Mir escapingU8Mem ∧
    LayoutsMatch lang0.layout escapingU8Mem ∧
    escapingU8Mem.LayoutsWF ∧
    Memory.HandoffWF escapingU8Mem := by
  have hEsc : escapingU8Mir.Escapes (language := lang0) mainFn p0 :=
    ⟨⟨mainFn, ⟨0⟩, 0, ctrlOrigin, .operator .read,
      [(u8Ty, .literal [0]), (u8Ty, .address p0 [])], []⟩,
     List.mem_cons_self, rfl, u8Ty, [], List.mem_cons_of_mem _ List.mem_cons_self⟩
  have hU8Lay : lang0.layout [] ctrlTarget u8Ty = some (Layout.scalar 1 1) := by decide
  have hU8WF : (Layout.scalar 1 1).WellFormed ctrlTarget = true :=
    sliceLanguage_layout_wf slice0 [] ctrlTarget u8Ty _ hU8Lay
  have hMat : Materializable (language := lang0) escapingU8Mir := by
    intro fn locs graph hfn place ty hloc _
    cases List.mem_singleton.mp hfn
    cases List.mem_singleton.mp hloc
    exact ⟨Layout.scalar 1 1, hU8Lay, hU8WF⟩
  have hMirWF : MIRHandoffWF (language := lang0) escapingU8Mir :=
    ⟨⟨singleFn_logicalView_tablesWF [] [(p0, u8Ty)] (by decide) [escapeAddrInst u8Ty p0]
        (fun inst h => by cases List.mem_singleton.mp h; trivial),
      by decide, singleFn_origins_bind_of_ctrlOrigin [] [(p0, u8Ty)] _,
      (fun _ h => nomatch h), emptyDeclsWF, emptyMutableWF _⟩, hMat⟩
  have hRet : RetainsTypedRoots escapingU8Mir escapingU8Mem :=
    RetainsTypedRoots.of_inventories_eq rfl (by decide)
      (fun fn locs graph hfn => by cases List.mem_singleton.mp hfn; decide)
  have hProm : PromotionSound (language := lang0) escapingU8Mir escapingU8Mem := by
    refine ⟨?_, ?_⟩
    · intro fn locs graph hfn place ty hloc
      cases List.mem_singleton.mp hfn
      cases List.mem_singleton.mp hloc
      exact ⟨[(p0, .materialized)], .materialized, by decide, by decide,
        ⟨(fun h => by cases h), (fun h => False.elim (h.2 hEsc))⟩⟩
    · intro fn classes place hfn hpl
      cases List.mem_singleton.mp hfn
      cases List.mem_singleton.mp hpl
  have hLayMatch : LayoutsMatch lang0.layout escapingU8Mem := by
    refine ⟨?_, ?_⟩
    · intro fn locs graph hfn place ty hloc
      cases List.mem_singleton.mp hfn
      cases List.mem_singleton.mp hloc
      exact ⟨[(p0, .materialized)], by decide,
        Or.inr ⟨by decide, [(p0, Layout.scalar 1 1)], Layout.scalar 1 1,
          by decide, by decide, hU8Lay⟩⟩
    · intro fn slots place d hfn hd
      cases List.mem_singleton.mp hfn
      cases List.mem_singleton.mp hd
      exact ⟨[(p0, u8Ty)], _, u8Ty, [(p0, .materialized)],
        List.mem_cons_self, List.mem_cons_self, by decide, by decide, hU8Lay⟩
  have hLayWF : escapingU8Mem.LayoutsWF := by
    intro table ht slot hs
    cases List.mem_singleton.mp ht
    cases List.mem_singleton.mp hs
    exact hU8WF
  have hMemWF : Memory.HandoffWF escapingU8Mem :=
    ⟨singleFn_mem_tablesWF [] [(p0, u8Ty)] [(p0, .materialized)] [(mainFn, [(p0, Layout.scalar 1 1)])]
       (by decide) (by decide) (by decide) (fun fn slots h => by cases List.mem_singleton.mp h; decide),
     by decide,
     (fun _ h => by simp [Memory.origins, escapingU8Mem, mkMem, CFG.origins, retZeroMemBlock, Terminator.origins] at h),
     hLayWF, emptyDeclsWF, emptyMutableWF _⟩
  exact ⟨hEsc, hMat, hMirWF, hRet, hProm, hLayMatch, hLayWF, hMemWF⟩

-- 1c. Declared empty nominal (`struct Empty {}`): non-scalar (must materialize even without
-- escaping), zero-sized with alignment 1 (`Layout.scalar 0 1`), well-formed via `sliceLanguage_layout_wf`.
private def emptyNominalMir : MIR lower1 ctrlProfile :=
  mkMir [(emptyDeclId, emptyDecl)] [(p0, emptyNominalTy)] []

private def emptyNominalMem : Memory lower1 ctrlProfile :=
  mkMem [(emptyDeclId, emptyDecl)] [(p0, emptyNominalTy)] [(p0, .materialized)]
    [(mainFn, [(p0, Layout.scalar 0 1)])]

theorem empty_nominal_materializable_and_retained :
    lang1.scalarType emptyNominalTy = none ∧
    lang1.layout [(emptyDeclId, emptyDecl)] ctrlTarget emptyNominalTy = some (Layout.scalar 0 1) ∧
    (Layout.scalar 0 1).WellFormed ctrlTarget = true ∧
    Materializable (language := lang1) emptyNominalMir ∧
    MIRHandoffWF (language := lang1) emptyNominalMir ∧
    RetainsTypedRoots emptyNominalMir emptyNominalMem ∧
    PromotionSound (language := lang1) emptyNominalMir emptyNominalMem ∧
    LayoutsMatch lang1.layout emptyNominalMem ∧
    emptyNominalMem.LayoutsWF ∧
    Memory.HandoffWF emptyNominalMem := by
  have hNomLay : lang1.layout [(emptyDeclId, emptyDecl)] ctrlTarget emptyNominalTy =
      some (Layout.scalar 0 1) := by decide
  have hNomWF : (Layout.scalar 0 1).WellFormed ctrlTarget = true :=
    sliceLanguage_layout_wf sliceOne [(emptyDeclId, emptyDecl)] ctrlTarget emptyNominalTy _ hNomLay
  have hMat : Materializable (language := lang1) emptyNominalMir := by
    intro fn locs graph hfn place ty hloc _
    cases List.mem_singleton.mp hfn
    cases List.mem_singleton.mp hloc
    exact ⟨Layout.scalar 0 1, hNomLay, hNomWF⟩
  have hMirWF : MIRHandoffWF (language := lang1) emptyNominalMir :=
    ⟨⟨singleFn_logicalView_tablesWF [(emptyDeclId, emptyDecl)] [(p0, emptyNominalTy)] (by decide) []
        (fun _ h => nomatch h),
      by decide, singleFn_origins_bind_of_empty [(emptyDeclId, emptyDecl)] [(p0, emptyNominalTy)],
      (fun _ h => nomatch h), oneEmptyDeclWF, emptyMutableWF _⟩, hMat⟩
  have hRet : RetainsTypedRoots emptyNominalMir emptyNominalMem :=
    RetainsTypedRoots.of_inventories_eq rfl (by decide)
      (fun fn locs graph hfn => by cases List.mem_singleton.mp hfn; decide)
  have hProm : PromotionSound (language := lang1) emptyNominalMir emptyNominalMem := by
    refine ⟨?_, ?_⟩
    · intro fn locs graph hfn place ty hloc
      cases List.mem_singleton.mp hfn
      cases List.mem_singleton.mp hloc
      exact ⟨[(p0, .materialized)], .materialized, by decide, by decide,
        ⟨(fun h => by cases h), (fun h => by cases h.1)⟩⟩
    · intro fn classes place hfn hpl
      cases List.mem_singleton.mp hfn
      cases List.mem_singleton.mp hpl
  have hLayMatch : LayoutsMatch lang1.layout emptyNominalMem := by
    refine ⟨?_, ?_⟩
    · intro fn locs graph hfn place ty hloc
      cases List.mem_singleton.mp hfn
      cases List.mem_singleton.mp hloc
      exact ⟨[(p0, .materialized)], by decide,
        Or.inr ⟨by decide, [(p0, Layout.scalar 0 1)], Layout.scalar 0 1,
          by decide, by decide, hNomLay⟩⟩
    · intro fn slots place d hfn hd
      cases List.mem_singleton.mp hfn
      cases List.mem_singleton.mp hd
      exact ⟨[(p0, emptyNominalTy)], _, emptyNominalTy, [(p0, .materialized)],
        List.mem_cons_self, List.mem_cons_self, by decide, by decide, hNomLay⟩
  have hLayWF : emptyNominalMem.LayoutsWF := by
    intro table ht slot hs
    cases List.mem_singleton.mp ht
    cases List.mem_singleton.mp hs
    exact hNomWF
  have hMemWF : Memory.HandoffWF emptyNominalMem :=
    ⟨singleFn_mem_tablesWF [(emptyDeclId, emptyDecl)] [(p0, emptyNominalTy)]
       [(p0, .materialized)] [(mainFn, [(p0, Layout.scalar 0 1)])]
       (by decide) (by decide) (by decide) (fun fn slots h => by cases List.mem_singleton.mp h; decide),
     by decide,
     (fun _ h => by simp [Memory.origins, emptyNominalMem, mkMem, CFG.origins, retZeroMemBlock, Terminator.origins] at h),
     hLayWF, oneEmptyDeclWF, emptyMutableWF _⟩
  exact ⟨rfl, hNomLay, hNomWF, hMat, hMirWF, hRet, hProm, hLayMatch, hLayWF, hMemWF⟩

/-! ## 2. Materializability refusal mutants: escaping unit & unused undeclared nominal -/

-- 2a. Escaping `unit` (`EarlyTy.scalar .unit` address-taken): has no physical layout in
-- `sliceLanguage`, so `Materializable`, `MIRHandoffWF`, and `LowersMemory` all fail.
-- Contrasts with a non-escaping `unit` local, which stays virtual and IS materializable.
private def escapingUnitMir : MIR lower0 ctrlProfile :=
  mkMir [] [(p0, unitTy)] [escapeAddrInst unitTy p0]

private def virtualUnitMir : MIR lower0 ctrlProfile :=
  mkMir [] [(p0, unitTy)] []

theorem escaping_unit_refused_vs_virtual_unit_accepted :
    escapingUnitMir.Escapes (language := lang0) mainFn p0 ∧
    ¬ Materializable (language := lang0) escapingUnitMir ∧
    ¬ MIRHandoffWF (language := lang0) escapingUnitMir ∧
    (∀ mem, ¬ LowersMemory (language := lang0) escapingUnitMir mem) ∧
    Materializable (language := lang0) virtualUnitMir := by
  have hEsc : escapingUnitMir.Escapes (language := lang0) mainFn p0 :=
    ⟨⟨mainFn, ⟨0⟩, 0, ctrlOrigin, .operator .read,
      [(u8Ty, .literal [0]), (unitTy, .address p0 [])], []⟩,
     List.mem_cons_self, rfl, unitTy, [], List.mem_cons_of_mem _ List.mem_cons_self⟩
  have hNotMat : ¬ Materializable (language := lang0) escapingUnitMir := by
    intro hmat
    obtain ⟨d, hd, _⟩ := hmat mainFn [(p0, unitTy)] _ List.mem_cons_self p0 unitTy
      List.mem_cons_self (Or.inr hEsc)
    have hnone : lang0.layout [] ctrlTarget unitTy = none := by decide
    have hcon : (none : Option Layout) = some d := hnone.symm.trans hd
    cases hcon
  have hVirtMat : Materializable (language := lang0) virtualUnitMir := by
    intro fn locs graph hfn place ty hloc hneed
    cases List.mem_singleton.mp hfn
    cases List.mem_singleton.mp hloc
    rcases hneed with hnone | hesc
    · cases hnone
    · obtain ⟨op, hop, _⟩ := hesc
      simp [MIR.operations, virtualUnitMir, mkMir, retZeroMirBlock] at hop
  exact ⟨hEsc, hNotMat, fun hwf => hNotMat hwf.materializable,
    fun _ hlow => hNotMat hlow.materializable, hVirtMat⟩

-- 2b. Unused undeclared nominal (`Ghost` absent from `declarations`, never read in CFG):
-- still fails `Materializable`, `MIRHandoffWF`, and `LowersMemory` because `Materializable`
-- governs every declared local root regardless of dynamic reachability.
private def unusedUndeclaredNominalMir : MIR lower1 ctrlProfile :=
  mkMir [] [(p0, ghostNominalTy)] []

theorem unused_undeclared_nominal_refused :
    ¬ Materializable (language := lang1) unusedUndeclaredNominalMir ∧
    ¬ MIRHandoffWF (language := lang1) unusedUndeclaredNominalMir ∧
    (∀ mem, ¬ LowersMemory (language := lang1) unusedUndeclaredNominalMir mem) := by
  have hNotMat : ¬ Materializable (language := lang1) unusedUndeclaredNominalMir := by
    intro hmat
    obtain ⟨d, hd, _⟩ := hmat mainFn [(p0, ghostNominalTy)] _ List.mem_cons_self p0 ghostNominalTy
      List.mem_cons_self (Or.inl rfl)
    have hnone : lang1.layout [] ctrlTarget ghostNominalTy = none := by decide
    have hcon : (none : Option Layout) = some d := hnone.symm.trans hd
    cases hcon
  exact ⟨hNotMat, fun hwf => hNotMat hwf.materializable, fun _ hlow => hNotMat hlow.materializable⟩

/-! ## 3. Bad declarations mutants (`DeclarationsWF`) -/

-- 3a. Key/name mismatch `(emptyDeclId, ghostDecl)` fails `DeclarationsWF`, `MIRHandoffWF`, `Memory.HandoffWF`.
-- 3b. Duplicate declaration names fail `DeclarationsWF`, `MIRHandoffWF`, `Memory.HandoffWF`.
-- 3c. Struct named `"main"` fails `DeclsOk` / `DeclarationsWF`.
theorem bad_declarations_mutants_refused :
    let mismatchDecls : Declarations := [(emptyDeclId, ghostDecl)]
    let dupDecls : Declarations := [(emptyDeclId, emptyDecl), (emptyDeclId, emptyDecl)]
    let mainNamedDecls : Declarations := [(mainDeclId, mainDecl)]
    ¬ DeclarationsWF mismatchDecls ∧
    ¬ MIRHandoffWF (language := lang1) (mkMir mismatchDecls [(p0, u8Ty)] []) ∧
    ¬ Memory.HandoffWF (mkMem (s := sliceOne) mismatchDecls [(p0, u8Ty)] [(p0, .virtual)] []) ∧
    ¬ DeclarationsWF dupDecls ∧
    ¬ MIRHandoffWF (language := lang1) (mkMir dupDecls [(p0, u8Ty)] []) ∧
    ¬ DeclarationsWF mainNamedDecls ∧
    ¬ MIRHandoffWF (language := lang1) (mkMir mainNamedDecls [(p0, u8Ty)] []) := by
  have hMis : ¬ DeclarationsWF [(emptyDeclId, ghostDecl)] := by
    intro h
    have heq := h.2.2 emptyDeclId ghostDecl List.mem_cons_self
    exact absurd heq (by decide)
  have hDup : ¬ DeclarationsWF [(emptyDeclId, emptyDecl), (emptyDeclId, emptyDecl)] := by
    intro h
    have hnd := h.1
    simp only [List.map_cons, List.nodup_cons] at hnd
    exact hnd.1 List.mem_cons_self
  have hMain : ¬ DeclarationsWF [(mainDeclId, mainDecl)] := by
    intro h
    exact (h.2.1.2 mainDecl List.mem_cons_self).1 rfl
  exact ⟨hMis, fun h => hMis h.declarationsWF, fun h => hMis h.declarationsWF,
    hDup, fun h => hDup h.declarationsWF,
    hMain, fun h => hMain h.declarationsWF⟩

/-! ## 4. Typed-root retention mutants (`RetainsTypedRoots` and `LowersMemory`) -/

-- 4a. Deleted root: MIR declares `p0 : u8`, Memory drops it -> fails `RetainsTypedRoots` & `LowersMemory`.
-- 4b. Added/fabricated root: Memory invents `p1 : u8` absent from MIR -> fails backward `RetainsTypedRoots`.
-- 4c. Retyped root: MIR declares `p0 : u8`, Memory retypes it to `p0 : u32` -> fails `RetainsTypedRoots`.
-- 4d. Dropped function and fabricated extra function -> both fail `RetainsTypedRoots`.
-- 4e. Duplicate local key `(p0, u8)` repeated -> fails `RetainsTypedRoots` even with identical membership.
theorem typed_root_retention_mutants_refused :
    let deletedRootMem : Memory lower0 ctrlProfile := mkMem [] [] [] []
    let addedRootMem : Memory lower0 ctrlProfile :=
      mkMem [] [(p0, u8Ty), (p1, u8Ty)] [(p0, .virtual), (p1, .virtual)] []
    let retypedRootMem : Memory lower0 ctrlProfile :=
      mkMem [] [(p0, u32Ty)] [(p0, .virtual)] []
    let droppedFnMem : Memory lower0 ctrlProfile := { virtualScalarMem with functions := [] }
    let addedFnMem : Memory lower0 ctrlProfile :=
      { virtualScalarMem with
        functions := [(mainFn, [(p0, u8Ty)], ⟨⟨0⟩, #[retZeroMemBlock []]⟩),
                      (helperFn, [], ⟨⟨0⟩, #[retZeroMemBlock []]⟩)] }
    let dupLocalMem : Memory lower0 ctrlProfile :=
      mkMem [] [(p0, u8Ty), (p0, u8Ty)] [(p0, .virtual)] []
    ¬ RetainsTypedRoots virtualScalarMir deletedRootMem ∧
    ¬ LowersMemory (language := lang0) virtualScalarMir deletedRootMem ∧
    ¬ RetainsTypedRoots virtualScalarMir addedRootMem ∧
    ¬ LowersMemory (language := lang0) virtualScalarMir addedRootMem ∧
    ¬ RetainsTypedRoots virtualScalarMir retypedRootMem ∧
    ¬ LowersMemory (language := lang0) virtualScalarMir retypedRootMem ∧
    ¬ RetainsTypedRoots virtualScalarMir droppedFnMem ∧
    ¬ RetainsTypedRoots virtualScalarMir addedFnMem ∧
    ¬ RetainsTypedRoots virtualScalarMir dupLocalMem := by
  have hDel : ¬ RetainsTypedRoots virtualScalarMir (mkMem [] [] [] []) := by
    intro h
    obtain ⟨dstLocs, _, hfn, heq⟩ := h.forward (fn := mainFn) List.mem_cons_self
    cases List.mem_singleton.mp hfn
    cases (heq p0 u8Ty).mp List.mem_cons_self
  have hAdd : ¬ RetainsTypedRoots virtualScalarMir
      (mkMem [] [(p0, u8Ty), (p1, u8Ty)] [(p0, .virtual), (p1, .virtual)] []) := by
    intro h
    obtain ⟨srcLocs, _, hfn, heq⟩ := h.backward (fn := mainFn) List.mem_cons_self
    cases List.mem_singleton.mp hfn
    have hmem := (heq p1 u8Ty).mp (List.mem_cons_of_mem _ List.mem_cons_self)
    cases List.mem_singleton.mp hmem
  have hRetype : ¬ RetainsTypedRoots virtualScalarMir
      (mkMem [] [(p0, u32Ty)] [(p0, .virtual)] []) := by
    intro h
    obtain ⟨dstLocs, _, hfn, heq⟩ := h.forward (fn := mainFn) List.mem_cons_self
    cases List.mem_singleton.mp hfn
    have hmem := (heq p0 u8Ty).mp List.mem_cons_self
    cases List.mem_singleton.mp hmem
  have hDropFn : ¬ RetainsTypedRoots virtualScalarMir { virtualScalarMem with functions := [] } := by
    intro h
    obtain ⟨_, _, hfn, _⟩ := h.forward (fn := mainFn) List.mem_cons_self
    cases hfn
  have hAddFn : ¬ RetainsTypedRoots virtualScalarMir
      { virtualScalarMem with
        functions := [(mainFn, [(p0, u8Ty)], ⟨⟨0⟩, #[retZeroMemBlock []]⟩),
                      (helperFn, [], ⟨⟨0⟩, #[retZeroMemBlock []]⟩)] } := by
    intro h
    obtain ⟨_, _, hfn, _⟩ := h.backward (fn := helperFn) (List.mem_cons_of_mem _ List.mem_cons_self)
    exact absurd (show helperFn = mainFn from congrArg Prod.fst (List.mem_singleton.mp hfn)) (by decide)
  have hDupLoc : ¬ RetainsTypedRoots virtualScalarMir
      (mkMem [] [(p0, u8Ty), (p0, u8Ty)] [(p0, .virtual)] []) := by
    intro h
    have hnd := h.memory_locals_nodup (fn := mainFn) List.mem_cons_self
    simp only [List.map, List.nodup_cons] at hnd
    exact hnd.1 List.mem_cons_self
  exact ⟨hDel, fun h => hDel h.retainsTypedRoots,
    hAdd, fun h => hAdd h.retainsTypedRoots,
    hRetype, fun h => hRetype h.retainsTypedRoots,
    hDropFn, hAddFn, hDupLoc⟩

/-! ## 5. Bad alignment/size & ISS-0125 geometry mutants (`Layout.WellFormed`, `Memory.LayoutsWF`) -/

private def fieldId (name : String) : FieldId := ⟨.struct emptyDeclId, ⟨name⟩⟩

-- 5a. Bad alignment (zero, non-power-of-two 3, and ISS-0125 ceiling equality `align = addressCeiling`),
-- bad size (not a multiple of align, and exceeding `target.maxObject`), and bad sibling fields
-- (overlapping ranges and duplicate field names) are all rejected by `Layout.WellFormed`.
-- Disjoint siblings and zero-sized siblings at a shared offset are accepted.
theorem layout_wellformed_geometry_and_iss0125_controls :
    let zeroAlign := Layout.scalar 0 0
    let nonPow2Align := Layout.scalar 3 3
    let ceilingAlign := Layout.scalar ctrlTarget.addressCeiling ctrlTarget.addressCeiling
    let unalignedSize := Layout.scalar 5 4
    let oversize := Layout.scalar (ctrlTarget.maxObject + 1) 1
    let overlapSiblings : Layout :=
      ⟨8, 4, .cons (fieldId "a") 0 (Layout.scalar 4 4)
        (.cons (fieldId "b") 2 (Layout.scalar 4 4) .nil)⟩
    let dupNameSiblings : Layout :=
      ⟨2, 1, .cons (fieldId "a") 0 (Layout.scalar 1 1)
        (.cons (fieldId "a") 1 (Layout.scalar 1 1) .nil)⟩
    let disjointSiblings : Layout :=
      ⟨2, 1, .cons (fieldId "a") 0 (Layout.scalar 1 1)
        (.cons (fieldId "b") 1 (Layout.scalar 1 1) .nil)⟩
    let zstSharedOffset : Layout :=
      ⟨1, 1, .cons (fieldId "z") 0 (Layout.scalar 0 1)
        (.cons (fieldId "v") 0 (Layout.scalar 1 1) .nil)⟩
    zeroAlign.WellFormed ctrlTarget = false ∧
    nonPow2Align.WellFormed ctrlTarget = false ∧
    ceilingAlign.WellFormed ctrlTarget = false ∧
    unalignedSize.WellFormed ctrlTarget = false ∧
    oversize.WellFormed ctrlTarget = false ∧
    overlapSiblings.WellFormed ctrlTarget = false ∧
    dupNameSiblings.WellFormed ctrlTarget = false ∧
    disjointSiblings.WellFormed ctrlTarget = true ∧
    zstSharedOffset.WellFormed ctrlTarget = true := by
  refine ⟨by decide, by decide, by decide, by decide, by decide,
    by decide, by decide, by decide, by decide⟩

-- 5b. Planting a bad alignment or bad size into `Memory.layouts` falsifies `Memory.LayoutsWF`,
-- `Memory.HandoffWF`, and `LowersMemory`.
theorem bad_layout_in_memory_refused :
    let badAlignMem : Memory lower0 ctrlProfile :=
      mkMem [] [(p0, u8Ty)] [(p0, .materialized)] [(mainFn, [(p0, Layout.scalar 3 3)])]
    let badSizeMem : Memory lower0 ctrlProfile :=
      mkMem [] [(p0, u8Ty)] [(p0, .materialized)] [(mainFn, [(p0, Layout.scalar 5 4)])]
    ¬ badAlignMem.LayoutsWF ∧
    ¬ Memory.HandoffWF badAlignMem ∧
    ¬ LowersMemory (language := lang0) escapingU8Mir badAlignMem ∧
    ¬ badSizeMem.LayoutsWF ∧
    ¬ Memory.HandoffWF badSizeMem ∧
    ¬ LowersMemory (language := lang0) escapingU8Mir badSizeMem := by
  have hAlign : ¬ (mkMem (s := slice0) [] [(p0, u8Ty)] [(p0, .materialized)]
      [(mainFn, [(p0, Layout.scalar 3 3)])]).LayoutsWF := by
    intro h
    have hwf := h (mainFn, [(p0, Layout.scalar 3 3)]) List.mem_cons_self
      (p0, Layout.scalar 3 3) List.mem_cons_self
    have hbad : (Layout.scalar 3 3).WellFormed ctrlTarget = false := by decide
    exact Bool.noConfusion (hbad.symm.trans hwf)
  have hSize : ¬ (mkMem (s := slice0) [] [(p0, u8Ty)] [(p0, .materialized)]
      [(mainFn, [(p0, Layout.scalar 5 4)])]).LayoutsWF := by
    intro h
    have hwf := h (mainFn, [(p0, Layout.scalar 5 4)]) List.mem_cons_self
      (p0, Layout.scalar 5 4) List.mem_cons_self
    have hbad : (Layout.scalar 5 4).WellFormed ctrlTarget = false := by decide
    exact Bool.noConfusion (hbad.symm.trans hwf)
  exact ⟨hAlign, fun h => hAlign h.layoutsWF, fun h => hAlign h.layoutsWF,
    hSize, fun h => hSize h.layoutsWF, fun h => hSize h.layoutsWF⟩

/-! ## 6. Promotion soundness mutants (escaping scalar or empty nominal marked `.virtual`) -/

theorem virtual_promotion_mutants_refused :
    let escapingAsVirtual : Memory lower0 ctrlProfile :=
      mkMem [] [(p0, u8Ty)] [(p0, .virtual)] []
    let nominalAsVirtual : Memory lower1 ctrlProfile :=
      mkMem [(emptyDeclId, emptyDecl)] [(p0, emptyNominalTy)] [(p0, .virtual)] []
    ¬ PromotionSound (language := lang0) escapingU8Mir escapingAsVirtual ∧
    ¬ LowersMemory (language := lang0) escapingU8Mir escapingAsVirtual ∧
    ¬ PromotionSound (language := lang1) emptyNominalMir nominalAsVirtual ∧
    ¬ LowersMemory (language := lang1) emptyNominalMir nominalAsVirtual := by
  have hEsc : escapingU8Mir.Escapes (language := lang0) mainFn p0 :=
    escaping_u8_materializable_and_retained.1
  have hProm1 : ¬ PromotionSound (language := lang0) escapingU8Mir
      (mkMem [] [(p0, u8Ty)] [(p0, .virtual)] []) := by
    intro h
    obtain ⟨classes, storage, hfn, hpl, hiff⟩ :=
      h.1 mainFn [(p0, u8Ty)] _ List.mem_cons_self p0 u8Ty List.mem_cons_self
    have hfn' : some [(p0, StorageClass.virtual)] = some classes := by
      simpa [mkMem, AList.lookup] using hfn
    have hcl : classes = [(p0, .virtual)] := Option.some.inj hfn'.symm
    subst hcl
    have hpl' : some StorageClass.virtual = some storage := by
      simpa [AList.lookup] using hpl
    have hst : storage = .virtual := Option.some.inj hpl'.symm
    subst hst
    exact (hiff.mp rfl).2 hEsc
  have hProm2 : ¬ PromotionSound (language := lang1) emptyNominalMir
      (mkMem [(emptyDeclId, emptyDecl)] [(p0, emptyNominalTy)] [(p0, .virtual)] []) := by
    intro h
    obtain ⟨classes, storage, hfn, hpl, hiff⟩ :=
      h.1 mainFn [(p0, emptyNominalTy)] _ List.mem_cons_self p0 emptyNominalTy List.mem_cons_self
    have hfn' : some [(p0, StorageClass.virtual)] = some classes := by
      simpa [mkMem, AList.lookup] using hfn
    have hcl : classes = [(p0, .virtual)] := Option.some.inj hfn'.symm
    subst hcl
    have hpl' : some StorageClass.virtual = some storage := by
      simpa [AList.lookup] using hpl
    have hst : storage = .virtual := Option.some.inj hpl'.symm
    subst hst
    cases (hiff.mp rfl).1
  exact ⟨hProm1, fun h => hProm1 h.2.2.2.2.2.1, hProm2, fun h => hProm2 h.2.2.2.2.2.1⟩

end ConcreteChecks.StageInterfaces.Memory.MaterializabilityControls
