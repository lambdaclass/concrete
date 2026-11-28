import Concrete.Interfaces.Pipeline

/-!
# Slice 0 freeze controls (unit F9c): two-module LLVM handoff

Standalone freeze controls written directly against the frozen interface modules
(`Concrete.Interfaces.*`, `Concrete.Backend.LLVMIR`, `Concrete.Backend.Runtime`), exercising:
1. Two trap-table slots for one operation (`bir.sites` sharing `orig0`), verifying `LauncherMatches`,
   `LLVMMetadataMatches`, both `@.site0`/`@.site1` constants, and switch dispatch cases `0` and `1`;
2. Nonzero site ID (`site 7`), verifying `runtimeSite`, `TrapEntry.diagnosticLine`, `LauncherMatches`,
   and rendered `.launcher` byte distinction from `site 0`;
3. Changed source coordinates (line, column, and file path), verifying `LauncherMatches` rejects
   mismatched launchers and `.launcher` bytes change on each coordinate change;
4. Target triple and datalayout enforcement on BOTH `.component` and `.launcher`:
   - forged target headers are unrepresentable in `LLVMModule` / `LLVMInput` (`fail_if_success`);
   - `x86_64Linux`, `aarch64Linux`, and `aarch64Macos` embed their exact headers and yield pairwise
     distinct bytes on BOTH `.component` and `.launcher`;
5. Swapped roles (`⟨out.launcher, out.component, out.sites⟩`) rejected by `LauncherMatches` and
   `LLVMMetadataMatches`;
6. Stale launcher diagnostics (updated `output.sites` paired with old `output.launcher`) rejected by
   `LauncherMatches` and `LLVMMetadataMatches`, plus `LauncherMatches.launcher_unique`;
7. Component byte sensitivity (`return 0` vs `return 42`): both satisfy `LLVMMetadataMatches`, share
   identical `.launcher` bytes, produce distinct `.component` and `packageBytes`, and are bound by
   `P_print` / `llvmBehaviors`;
8. Launcher byte sensitivity (changed diagnostic line with identical `.component`): identical
   `.component` bytes, distinct `.launcher` bytes and `packageBytes`;
9. Wrong backend kind (`.lcb`) rejected by `LLVMMetadataMatches`;
10. Two-result checked-add LLVM component rendering (`@llvm.uadd.with.overflow.i8` + `extractvalue`).
-/

namespace StageInterfaceControls.TwoModuleLLVMControls

open Concrete.Interfaces
open Concrete.Backend.LLVM
open Concrete.Backend.Runtime
open ConcreteSpec.Core
open ConcreteSpec.Core.SourcePathSpan
open ConcreteSpec.StageInterface

private abbrev s0Core : CoreLanguage := sliceLanguage slice0
private abbrev s0Lang : LoweringLanguage EarlyTy := sliceLoweringLanguage slice0
private abbrev u8Ty : EarlyTy := s0Lang.intTy .u8

private def ctrlMain : FnId :=
  ⟨⟨⟨⟨"pkg"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩, ⟨"main"⟩⟩

private def mainSrcText : SourceText :=
  SourceText.ofBytes "fn main() -> u8 {\n  return 255 + 1;\n}".toUTF8.data.toList

private def altSourcePath : SourcePath :=
  ⟨SourceText.ofBytes "alt.con".toUTF8.data.toList, by decide⟩

private def projMain : Project := Project.singleton mainSourcePath mainSrcText
private def projAlt : Project := Project.singleton altSourcePath mainSrcText

private def origAt (siteId line col : Nat) : Origin :=
  ⟨⟨siteId⟩, ⟨⟨0⟩, 0, 1, line, col⟩, ⟨.function ctrlMain, ⟨[]⟩⟩⟩

private def orig0 : Origin := origAt 0 1 30
private def origSite7 : Origin := origAt 7 1 30
private def origLine2 : Origin := origAt 0 2 30
private def origCol31 : Origin := origAt 0 1 31

private def llvmLinux : Profile := ⟨.llvm, .x86_64Linux, 64, 4096⟩
private def llvmAarch64Linux : Profile := ⟨.llvm, .aarch64Linux, 64, 4096⟩
private def llvmMac : Profile := ⟨.llvm, .aarch64Macos, 64, 4096⟩
private def lcbLinux : Profile := ⟨.lcb, .x86_64Linux, 64, 4096⟩

/-- Minimal BIR parameterized by project, profile, and trap table. -/
private def mkBIR (profile : Profile) (proj : Project) (sites : TrapTable) :
    BIR s0Lang profile :=
  let b0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
  ⟨proj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], sites, [], []⟩

/-- Structured LLVM component module defining `@concrete_body` returning `status`. -/
private def compReturn (status : Nat) : LLVMModule :=
  ⟨[.define false (.integer .u32) .body [],
    .label .entry,
    .terminator (.ret (.integer .u32, .immediate (.natural status))),
    .endFunction]⟩

/-- Structured LLVM launcher module rendered directly from canonical diagnostic lines. -/
private def launchOfLines (lines : List String) : LLVMModule :=
  ⟨llvmDiagnostics lines⟩

private def line0 : String :=
  s!"concrete: trap: overflow at {mainSourcePath.render.text}:1:30 (site 0)\n"
private def lineSite7 : String :=
  s!"concrete: trap: overflow at {mainSourcePath.render.text}:1:30 (site 7)\n"
private def lineRow2 : String :=
  s!"concrete: trap: overflow at {mainSourcePath.render.text}:2:30 (site 0)\n"
private def lineCol31 : String :=
  s!"concrete: trap: overflow at {mainSourcePath.render.text}:1:31 (site 0)\n"
private def lineAltPath : String :=
  s!"concrete: trap: overflow at {altSourcePath.render.text}:1:30 (site 0)\n"

private def headerOf (p : Profile) : String :=
  s!"target datalayout = \"{p.llvmDataLayout}\"\ntarget triple = \"{p.llvmTriple}\"\n"

private theorem packageBytes_ne_of_component {a b : LLVMPackageBytes}
    (h : a.component ≠ b.component) : a ≠ b :=
  fun heq => h (congrArg LLVMPackageBytes.component heq)

private theorem packageBytes_ne_of_launcher {a b : LLVMPackageBytes}
    (h : a.launcher ≠ b.launcher) : a ≠ b :=
  fun heq => h (congrArg LLVMPackageBytes.launcher heq)

private theorem prefixByte (head tail : String) (idx : Nat) (h : idx < head.toUTF8.data.size) :
    (head ++ tail).toUTF8.data[idx]? = head.toUTF8.data[idx]? := by
  change (head.toUTF8 ++ tail.toUTF8).data[idx]? = _
  rw [ByteArray.data_append, Array.getElem?_append_left h]

private theorem suffixByte (head tail : String) (idx : Nat) :
    (head ++ tail).toUTF8.data[head.toUTF8.data.size + idx]? = tail.toUTF8.data[idx]? := by
  change (head.toUTF8 ++ tail.toUTF8).data[head.toUTF8.data.size + idx]? = _
  rw [ByteArray.data_append, Array.getElem?_append_right (Nat.le_add_right _ _),
    Nat.add_sub_cancel_left]

private theorem foldl_append_init (init : String) (xs : List String) :
    xs.foldl (· ++ ·) init = init ++ xs.foldl (· ++ ·) "" := by
  induction xs generalizing init with
  | nil => simp
  | cons a rest ih =>
    simp only [List.foldl_cons, String.empty_append]
    rw [ih (init ++ a), ih a, String.append_assoc]

private def renderOne (x : Item) : String :=
  renderPieces x.pieces

private theorem render_nil : render [] = "" := rfl

private theorem render_cons (x : Item) (xs : List Item) :
    render (x :: xs) = renderOne x ++ render xs := by
  have hjoin : ∀ a b : List String, String.join (a ++ b) = String.join a ++ String.join b := by
    intro a b
    unfold String.join
    rw [List.foldl_append, foldl_append_init]
  unfold render renderOne renderPieces
  simp only [List.flatMap_cons, List.map_append]
  exact hjoin _ _

private theorem items_ne_of_head_ne {a b : List Item} (h : a.head? ≠ b.head?) : a ≠ b :=
  fun heq => h (congrArg List.head? heq)

private theorem items_ne_of_length_ne {a b : List Item} (h : a.length ≠ b.length) : a ≠ b :=
  fun heq => h (congrArg List.length heq)

private theorem header_diff_bytesAt {p₁ p₂ : Profile} {in₁ : LLVMInput p₁} {in₂ : LLVMInput p₂}
    {role : LLVMRole} (idx : Nat)
    (h₁ : idx < (headerOf p₁).toUTF8.data.size)
    (h₂ : idx < (headerOf p₂).toUTF8.data.size)
    (hne : (headerOf p₁).toUTF8.data[idx]? ≠ (headerOf p₂).toUTF8.data[idx]?) :
    in₁.bytesAt role ≠ in₂.bytesAt role := by
  intro heq
  have hb := congrArg (fun ba => ba.data[idx]?) heq
  change ((headerOf p₁ ++ render (in₁.moduleAt role).items).toUTF8.data[idx]? =
    (headerOf p₂ ++ render (in₂.moduleAt role).items).toUTF8.data[idx]?) at hb
  rw [prefixByte _ _ idx h₁, prefixByte _ _ idx h₂] at hb
  exact hne hb

private theorem body_diff_bytesAt {p : Profile} {in₁ in₂ : LLVMInput p} {role : LLVMRole}
    (idx : Nat)
    (hne : (render (in₁.moduleAt role).items).toUTF8.data[idx]? ≠
      (render (in₂.moduleAt role).items).toUTF8.data[idx]?) :
    in₁.bytesAt role ≠ in₂.bytesAt role := by
  intro heq
  have hb := congrArg (fun ba => ba.data[(headerOf p).toUTF8.data.size + idx]?) heq
  change ((headerOf p ++ render (in₁.moduleAt role).items).toUTF8.data[(headerOf p).toUTF8.data.size + idx]? =
    (headerOf p ++ render (in₂.moduleAt role).items).toUTF8.data[(headerOf p).toUTF8.data.size + idx]?) at hb
  rw [suffixByte, suffixByte] at hb
  exact hne hb

private theorem first_site_launcher_bytes_ne {p : Profile}
    (s₁ s₂ : String) (c₁ c₂ : LLVMModule) (sites₁ sites₂ : TrapTable) (idx : Nat)
    (h₁ : idx < (renderOne (.constant (.site 0) s₁.toUTF8)).toUTF8.data.size)
    (h₂ : idx < (renderOne (.constant (.site 0) s₂.toUTF8)).toUTF8.data.size)
    (hne : (renderOne (.constant (.site 0) s₁.toUTF8)).toUTF8.data[idx]? ≠
      (renderOne (.constant (.site 0) s₂.toUTF8)).toUTF8.data[idx]?) :
    (⟨c₁, launchOfLines [s₁], sites₁⟩ : LLVMInput p).bytesAt .launcher ≠
      (⟨c₂, launchOfLines [s₂], sites₂⟩ : LLVMInput p).bytesAt .launcher := by
  refine body_diff_bytesAt idx ?_
  change (render (llvmDiagnostics [s₁])).toUTF8.data[idx]? ≠
    (render (llvmDiagnostics [s₂])).toUTF8.data[idx]?
  obtain ⟨t₁, ht₁⟩ : ∃ t, llvmDiagnostics [s₁] = Item.constant (.site 0) s₁.toUTF8 :: t := ⟨_, rfl⟩
  obtain ⟨t₂, ht₂⟩ : ∃ t, llvmDiagnostics [s₂] = Item.constant (.site 0) s₂.toUTF8 :: t := ⟨_, rfl⟩
  rw [ht₁, ht₂, render_cons, render_cons, prefixByte _ _ idx h₁, prefixByte _ _ idx h₂]
  exact hne

/-! ## Control 1: Two trap-table slots for one operation -/

/-- One operation origin (`orig0`) occupying two distinct trap-table slots (`0` and `1`):
`LauncherMatches` and `LLVMMetadataMatches` accept the two-slot table, both `@.site0` and `@.site1`
globals and switch cases `0`/`1` are emitted, and dropping either slot fails `LauncherMatches`. -/
theorem two_slots_for_one_op :
    let sitesTwo : TrapTable := #[⟨.overflow, orig0⟩, ⟨.overflow, orig0⟩]
    let bir := mkBIR llvmLinux projMain sitesTwo
    let out : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [line0, line0], sitesTwo⟩
    let oneSlotLauncher : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [line0], sitesTwo⟩
    LauncherMatches bir out ∧
    LLVMMetadataMatches bir out ∧
    out.launcher.items.contains (.constant (.site 0) line0.toUTF8) = true ∧
    out.launcher.items.contains (.constant (.site 1) line0.toUTF8) = true ∧
    out.launcher.items.contains (.terminator (.switch .u32 .i .bad
      [(Immediate.natural 0, Label.trapSite 0), (Immediate.natural 1, Label.trapSite 1)])) = true ∧
    ¬ LauncherMatches bir oneSlotLauncher ∧
    ¬ LLVMMetadataMatches bir oneSlotLauncher := by
  intro sitesTwo bir out oneSlotLauncher
  have hMap : bir.sites.toList.mapM (fun entry => entry.diagnosticLine bir.project) =
      some [line0, line0] := by
    decide
  refine ⟨⟨[line0, line0], hMap, rfl⟩, ⟨rfl, rfl, ⟨[line0, line0], hMap, rfl⟩⟩,
    by decide, by decide, by decide, ?_, ?_⟩
  · intro hMatch
    exact items_ne_of_length_ne (by decide) (hMatch.launcher_items_eq hMap)
  · intro hMeta
    exact items_ne_of_length_ne (by decide) (hMeta.launcherMatches.launcher_items_eq hMap)

/-! ## Control 2: Nonzero site ID (`site 7`) -/

/-- A trap at nonzero site ID `7` resolves through `runtimeSite` and `TrapEntry.diagnosticLine`
to `(site 7)`, satisfies `LLVMMetadataMatches`, rejects a `(site 0)` launcher, and produces
distinct rendered `.launcher` bytes from site `0`. -/
theorem nonzero_site_diagnostic_and_bytes :
    let bir0 := mkBIR llvmLinux projMain #[⟨.overflow, orig0⟩]
    let bir7 := mkBIR llvmLinux projMain #[⟨.overflow, origSite7⟩]
    let out0 : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [line0], bir0.sites⟩
    let out7 : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [lineSite7], bir7.sites⟩
    let stale0For7 : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [line0], bir7.sites⟩
    runtimeSite projMain origSite7 = some ⟨⟨7⟩, ⟨mainSourcePath.render.text, 1, 30⟩⟩ ∧
    (TrapEntry.mk .overflow origSite7).diagnosticLine projMain = some lineSite7 ∧
    LLVMMetadataMatches bir7 out7 ∧
    ¬ LauncherMatches bir7 stale0For7 ∧
    ¬ LLVMMetadataMatches bir7 stale0For7 ∧
    out7.bytesAt .launcher ≠ out0.bytesAt .launcher := by
  intro bir0 bir7 out0 out7 stale0For7
  have hMap7 : bir7.sites.toList.mapM (fun entry => entry.diagnosticLine bir7.project) =
      some [lineSite7] := by
    decide
  have hHeadNe : (launchOfLines [line0]).items.head? ≠ (llvmDiagnostics [lineSite7]).head? := by
    intro h
    have hBytes := congrArg (fun o => o.map fun
      | Item.constant _ bs => bs.data.toList
      | _ => []) h
    exact (by decide : some line0.toUTF8.data.toList ≠ some lineSite7.toUTF8.data.toList) hBytes
  -- Byte 90 in `@.site0 = private constant [56 x i8] c"concrete: trap: overflow at main.con:1:30 (site 7)\0A"\n`
  -- is `'7'` (55) vs `'0'` (48).
  refine ⟨by decide, by decide, ⟨rfl, rfl, ⟨[lineSite7], hMap7, rfl⟩⟩,
    fun h => items_ne_of_head_ne hHeadNe (h.launcher_items_eq hMap7),
    fun h => items_ne_of_head_ne hHeadNe (h.launcherMatches.launcher_items_eq hMap7),
    first_site_launcher_bytes_ne lineSite7 line0 (compReturn 42) (compReturn 42) bir7.sites bir0.sites 87
      (by cbv) (by cbv) (by cbv; decide)⟩

/-! ## Control 3: Changed coordinates (line, column, and file path) -/

/-- Changing the trap origin's line, column, or project file path invalidates the previous launcher
under `LauncherMatches` / `LLVMMetadataMatches` and changes `bytesAt .launcher`. -/
theorem changed_coordinates_invalidate_launcher :
    let birBase := mkBIR llvmLinux projMain #[⟨.overflow, orig0⟩]
    let birLine2 := mkBIR llvmLinux projMain #[⟨.overflow, origLine2⟩]
    let birCol31 := mkBIR llvmLinux projMain #[⟨.overflow, origCol31⟩]
    let birAltPath := mkBIR llvmLinux projAlt #[⟨.overflow, orig0⟩]
    let outBase : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [line0], birBase.sites⟩
    let outLine2 : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [lineRow2], birLine2.sites⟩
    let outCol31 : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [lineCol31], birCol31.sites⟩
    let outAltPath : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [lineAltPath], birAltPath.sites⟩
    LLVMMetadataMatches birLine2 outLine2 ∧
    LLVMMetadataMatches birCol31 outCol31 ∧
    LLVMMetadataMatches birAltPath outAltPath ∧
    ¬ LauncherMatches birLine2 { outBase with sites := birLine2.sites } ∧
    ¬ LauncherMatches birCol31 { outBase with sites := birCol31.sites } ∧
    ¬ LauncherMatches birAltPath { outBase with sites := birAltPath.sites } ∧
    outLine2.bytesAt .launcher ≠ outBase.bytesAt .launcher ∧
    outCol31.bytesAt .launcher ≠ outBase.bytesAt .launcher ∧
    outAltPath.bytesAt .launcher ≠ outBase.bytesAt .launcher := by
  intro birBase birLine2 birCol31 birAltPath outBase outLine2 outCol31 outAltPath
  have hMapL2 : birLine2.sites.toList.mapM (fun entry => entry.diagnosticLine birLine2.project) =
      some [lineRow2] := by decide
  have hMapC31 : birCol31.sites.toList.mapM (fun entry => entry.diagnosticLine birCol31.project) =
      some [lineCol31] := by decide
  have hMapAlt : birAltPath.sites.toList.mapM (fun entry => entry.diagnosticLine birAltPath.project) =
      some [lineAltPath] := by decide
  have hHeadNe (s : String) (hne : line0.toUTF8.data.toList ≠ s.toUTF8.data.toList) :
      (launchOfLines [line0]).items.head? ≠ (llvmDiagnostics [s]).head? := by
    intro h
    have hb := congrArg (fun o => o.map fun | Item.constant _ bs => bs.data.toList | _ => []) h
    exact hne (Option.some.inj hb)
  refine ⟨⟨rfl, rfl, ⟨[lineRow2], hMapL2, rfl⟩⟩,
    ⟨rfl, rfl, ⟨[lineCol31], hMapC31, rfl⟩⟩,
    ⟨rfl, rfl, ⟨[lineAltPath], hMapAlt, rfl⟩⟩,
    fun h => items_ne_of_head_ne (hHeadNe lineRow2 (by decide)) (h.launcher_items_eq hMapL2),
    fun h => items_ne_of_head_ne (hHeadNe lineCol31 (by decide)) (h.launcher_items_eq hMapC31),
    fun h => items_ne_of_head_ne (hHeadNe lineAltPath (by decide)) (h.launcher_items_eq hMapAlt),
    first_site_launcher_bytes_ne lineRow2 line0 (compReturn 42) (compReturn 42) birLine2.sites birBase.sites 76
      (by cbv) (by cbv) (by cbv; decide),
    first_site_launcher_bytes_ne lineCol31 line0 (compReturn 42) (compReturn 42) birCol31.sites birBase.sites 79
      (by cbv) (by cbv) (by cbv; decide),
    first_site_launcher_bytes_ne lineAltPath line0 (compReturn 42) (compReturn 42) birAltPath.sites birBase.sites 29
      (by cbv) (by cbv) (by cbv; decide)⟩

/-! ## Control 4: Wrong triple/layout unrepresentable and platform-distinct on BOTH roles -/

/-- `LLVMModule` carries no target header fields, so a mismatched triple or datalayout cannot be
forged on either `.component` or `.launcher`. -/
example (p : Profile) : LLVMInput p := by
  fail_if_success exact (⟨"wrong-triple", p.llvmDataLayout, []⟩ : LLVMModule)
  fail_if_success exact (⟨p.llvmTriple, "wrong-layout", []⟩ : LLVMModule)
  fail_if_success exact (⟨⟨"wrong-triple", p.llvmDataLayout, []⟩, ⟨[]⟩, #[]⟩ : LLVMInput p)
  fail_if_success exact (⟨⟨[]⟩, ⟨p.llvmTriple, "wrong-layout", []⟩, #[]⟩ : LLVMInput p)
  exact ⟨compReturn 42, launchOfLines [], #[]⟩

/-- Identical structured `.component` and `.launcher` items under `x86_64Linux`, `aarch64Linux`,
and `aarch64Macos` embed each profile's exact datalayout/triple header and produce pairwise
distinct byte arrays on BOTH `.component` and `.launcher`. -/
theorem target_headers_distinct_on_both_roles :
    let comp := compReturn 42
    let launch := launchOfLines [line0]
    let inX64 : LLVMInput llvmLinux := ⟨comp, launch, #[⟨.overflow, orig0⟩]⟩
    let inArmLin : LLVMInput llvmAarch64Linux := ⟨comp, launch, #[⟨.overflow, orig0⟩]⟩
    let inMac : LLVMInput llvmMac := ⟨comp, launch, #[⟨.overflow, orig0⟩]⟩
    llvmLinux.llvmTriple ≠ llvmAarch64Linux.llvmTriple ∧
    llvmLinux.llvmTriple ≠ llvmMac.llvmTriple ∧
    llvmAarch64Linux.llvmTriple ≠ llvmMac.llvmTriple ∧
    llvmLinux.llvmDataLayout ≠ llvmAarch64Linux.llvmDataLayout ∧
    llvmLinux.llvmDataLayout ≠ llvmMac.llvmDataLayout ∧
    llvmAarch64Linux.llvmDataLayout ≠ llvmMac.llvmDataLayout ∧
    inX64.bytesAt .component ≠ inArmLin.bytesAt .component ∧
    inX64.bytesAt .component ≠ inMac.bytesAt .component ∧
    inArmLin.bytesAt .component ≠ inMac.bytesAt .component ∧
    inX64.bytesAt .launcher ≠ inArmLin.bytesAt .launcher ∧
    inX64.bytesAt .launcher ≠ inMac.bytesAt .launcher ∧
    inArmLin.bytesAt .launcher ≠ inMac.bytesAt .launcher := by
  intro comp launch inX64 inArmLin inMac
  -- Header diffs:
  -- `x86_64Linux` vs `aarch64Linux`: byte 61 (`6` in `i64:64` vs `8` in `i8:8:32`)
  -- `x86_64Linux` vs `aarch64Macos`: byte 25 (`e` in `m:e` vs `o` in `m:o`)
  -- `aarch64Linux` vs `aarch64Macos`: byte 25 (`e` in `m:e` vs `o` in `m:o`)
  refine ⟨by decide, by decide, by decide, by decide, by decide, by decide,
    header_diff_bytesAt 61 (by cbv) (by cbv) (by cbv; decide),
    header_diff_bytesAt 25 (by cbv) (by cbv) (by cbv; decide),
    header_diff_bytesAt 25 (by cbv) (by cbv) (by cbv; decide),
    header_diff_bytesAt 61 (by cbv) (by cbv) (by cbv; decide),
    header_diff_bytesAt 25 (by cbv) (by cbv) (by cbv; decide),
    header_diff_bytesAt 25 (by cbv) (by cbv) (by cbv; decide)⟩

/-! ## Control 5 & 6: Swapped roles, stale launcher diagnostics, and launcher uniqueness -/

/-- Swapping `.component` and `.launcher` modules fails both `LauncherMatches` and
`LLVMMetadataMatches`; pairing updated `sites` with a stale launcher also fails both; wrong
backend kind (`.lcb`) fails `LLVMMetadataMatches`; and `LauncherMatches.launcher_unique` pins
`.launcher` uniquely for a given `BIR`. -/
theorem swapped_roles_and_stale_launcher_rejected :
    let bir0 := mkBIR llvmLinux projMain #[⟨.overflow, orig0⟩]
    let birEmpty := mkBIR llvmLinux projMain #[]
    let out0 : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [line0], bir0.sites⟩
    let outEmpty : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [], birEmpty.sites⟩
    let swapped : LLVMInput llvmLinux := ⟨out0.launcher, out0.component, out0.sites⟩
    let staleEmptyLauncher : LLVMInput llvmLinux := ⟨compReturn 42, outEmpty.launcher, bir0.sites⟩
    let staleSitesOnEmpty : LLVMInput llvmLinux := ⟨compReturn 42, out0.launcher, birEmpty.sites⟩
    let lcbOut : LLVMInput lcbLinux := ⟨compReturn 42, launchOfLines [line0], bir0.sites⟩
    let birLCB := mkBIR lcbLinux projMain #[⟨.overflow, orig0⟩]
    LLVMMetadataMatches bir0 out0 ∧
    ¬ LauncherMatches bir0 swapped ∧
    ¬ LLVMMetadataMatches bir0 swapped ∧
    ¬ LauncherMatches bir0 staleEmptyLauncher ∧
    ¬ LLVMMetadataMatches bir0 staleEmptyLauncher ∧
    ¬ LauncherMatches birEmpty staleSitesOnEmpty ∧
    ¬ LLVMMetadataMatches birLCB lcbOut ∧
    (∀ other : LLVMInput llvmLinux, LauncherMatches bir0 other → other.launcher = out0.launcher) := by
  intro bir0 birEmpty out0 outEmpty swapped staleEmptyLauncher staleSitesOnEmpty lcbOut birLCB
  have hMap0 : bir0.sites.toList.mapM (fun entry => entry.diagnosticLine bir0.project) =
      some [line0] := by decide
  have hMapE : birEmpty.sites.toList.mapM (fun entry => entry.diagnosticLine birEmpty.project) =
      some [] := by decide
  have hMatch0 : LauncherMatches bir0 out0 := ⟨[line0], hMap0, rfl⟩
  refine ⟨⟨rfl, rfl, hMatch0⟩,
    fun h => items_ne_of_length_ne (by decide) (h.launcher_items_eq hMap0),
    fun h => items_ne_of_length_ne (by decide) (h.launcherMatches.launcher_items_eq hMap0),
    fun h => items_ne_of_length_ne (by decide) (h.launcher_items_eq hMap0),
    fun h => items_ne_of_length_ne (by decide) (h.launcherMatches.launcher_items_eq hMap0),
    fun h => items_ne_of_length_ne (by decide) (h.launcher_items_eq hMapE),
    fun h => BackendKind.noConfusion h.1,
    fun other hOther => LauncherMatches.launcher_unique hOther hMatch0⟩

/-! ## Control 7 & 8: Component byte sensitivity (`return 0` vs `return 42`), launcher byte
sensitivity (changed diagnostic line), and `P_print` binding -/

/-- Changing `@concrete_body` from `return 0` to `return 42` preserves `LLVMMetadataMatches` and
`.launcher` bytes while changing `.component` bytes and `packageBytes`; changing a diagnostic line
with identical `.component` preserves `.component` bytes while changing `.launcher` bytes and
`packageBytes`. `P_print` binds the exact `packageBytes` pair. -/
theorem component_and_launcher_byte_sensitivity :
    let bir0 := mkBIR llvmLinux projMain #[⟨.overflow, orig0⟩]
    let birLine2 := mkBIR llvmLinux projMain #[⟨.overflow, origLine2⟩]
    let in0 : LLVMInput llvmLinux := ⟨compReturn 0, launchOfLines [line0], bir0.sites⟩
    let in42 : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [line0], bir0.sites⟩
    let inLine2 : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [lineRow2], birLine2.sites⟩
    -- Component sensitivity (`return 0` vs `return 42`):
    LLVMMetadataMatches bir0 in0 ∧
    LLVMMetadataMatches bir0 in42 ∧
    in0.bytesAt .launcher = in42.bytesAt .launcher ∧
    in0.packageBytes.launcher = in42.packageBytes.launcher ∧
    in0.bytesAt .component ≠ in42.bytesAt .component ∧
    in0.packageBytes.component ≠ in42.packageBytes.component ∧
    in0.packageBytes ≠ in42.packageBytes ∧
    -- Launcher sensitivity (line 1 vs line 2, identical component):
    LLVMMetadataMatches birLine2 inLine2 ∧
    in42.bytesAt .component = inLine2.bytesAt .component ∧
    in42.packageBytes.component = inLine2.packageBytes.component ∧
    in42.bytesAt .launcher ≠ inLine2.bytesAt .launcher ∧
    in42.packageBytes.launcher ≠ inLine2.packageBytes.launcher ∧
    in42.packageBytes ≠ inLine2.packageBytes ∧
    -- `P_print` binds `packageBytes` on `.llvm`:
    (P_print (language := s0Core) bir0 in42 →
      ProgramSafe (BIR.Execution (language := s0Core)) bir0 →
      ¬ llvmBehaviors (E := s0Core.operands.Event) in42.packageBytes .wrong) := by
  intro bir0 birLine2 in0 in42 inLine2
  have hMap0 : bir0.sites.toList.mapM (fun entry => entry.diagnosticLine bir0.project) =
      some [line0] := by decide
  have hMapL2 : birLine2.sites.toList.mapM (fun entry => entry.diagnosticLine birLine2.project) =
      some [lineRow2] := by decide
  -- In `define i32 @concrete_body() {\nentry:\n  ret i32 0\n}\n` vs `...42\n}\n`, byte 47 is `'0'` vs `'4'`.
  have hCompNe : in0.bytesAt .component ≠ in42.bytesAt .component :=
    body_diff_bytesAt 47 (by cbv; decide)
  have hLaunchNe : in42.bytesAt .launcher ≠ inLine2.bytesAt .launcher :=
    first_site_launcher_bytes_ne line0 lineRow2 (compReturn 42) (compReturn 42) bir0.sites birLine2.sites 76
      (by cbv) (by cbv) (by cbv; decide)
  exact ⟨⟨rfl, rfl, ⟨[line0], hMap0, rfl⟩⟩,
    ⟨rfl, rfl, ⟨[line0], hMap0, rfl⟩⟩,
    rfl, rfl, hCompNe, hCompNe, packageBytes_ne_of_component hCompNe,
    ⟨rfl, rfl, ⟨[lineRow2], hMapL2, rfl⟩⟩,
    rfl, rfl, hLaunchNe, hLaunchNe, packageBytes_ne_of_launcher hLaunchNe,
    fun hp hsafe hwrong => hp hsafe .wrong hwrong⟩

/-! ## Control 9: Two-result checked-add LLVM component rendering (`uadd.with.overflow` + `extractvalue`) -/

private def compCheckedAddU8 : LLVMModule :=
  ⟨[.declare (.overflowPair .u8) (.overflow .add .u8) [.integer .u8, .integer .u8],
    .blank,
    .define false (.integer .u32) .body [],
    .label .entry,
    .instruction (.call (some (.overflow 0)) (.overflowPair .u8) (.overflow .add .u8)
      [(.integer .u8, .immediate (.natural 20)), (.integer .u8, .immediate (.natural 22))]),
    .instruction (.extract (.register 0) (.overflowPair .u8) (.overflow 0) 0),
    .instruction (.extract (.register 1) (.overflowPair .u8) (.overflow 0) 1),
    .terminator (.cond (.register 1) (.trapSite 0) (.block 2)),
    .label (.trapSite 0),
    .terminator (.ret (.integer .u32, .immediate (.natural 256))),
    .label (.block 2),
    .instruction (.convert (.result 2) .zext (.integer .u8, .local (.register 0)) (.integer .u32)),
    .terminator (.ret (.integer .u32, .local (.result 2))),
    .endFunction]⟩

/-- Two-result checked-add LLVM IR component renders `@llvm.uadd.with.overflow.i8` and
`extractvalue {i8, i1}` cleanly, satisfies `LLVMMetadataMatches` paired with the matching launcher,
and differs at `.component` bytes from an unguarded `return 42` component. -/
theorem checked_add_llvm_component_rendering :
    let bir0 := mkBIR llvmLinux projMain #[⟨.overflow, orig0⟩]
    let inChecked : LLVMInput llvmLinux := ⟨compCheckedAddU8, launchOfLines [line0], bir0.sites⟩
    let inPlain : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [line0], bir0.sites⟩
    let rendered := render compCheckedAddU8.items
    LLVMMetadataMatches bir0 inChecked ∧
    rendered =
      "declare {i8, i1} @llvm.uadd.with.overflow.i8(i8, i8)\n\n" ++
      "define i32 @concrete_body() {\n" ++
      "entry:\n" ++
      "  %o0 = call {i8, i1} @llvm.uadd.with.overflow.i8(i8 20, i8 22)\n" ++
      "  %r0 = extractvalue {i8, i1} %o0, 0\n" ++
      "  %r1 = extractvalue {i8, i1} %o0, 1\n" ++
      "  br i1 %r1, label %t0, label %b2\n" ++
      "t0:\n" ++
      "  ret i32 256\n" ++
      "b2:\n" ++
      "  %ret2 = zext i8 %r0 to i32\n" ++
      "  ret i32 %ret2\n" ++
      "}\n" ∧
    inChecked.bytesAt .launcher = inPlain.bytesAt .launcher ∧
    inChecked.bytesAt .component ≠ inPlain.bytesAt .component := by
  intro bir0 inChecked inPlain rendered
  have hMap0 : bir0.sites.toList.mapM (fun entry => entry.diagnosticLine bir0.project) =
      some [line0] := by decide
  -- Compare item-by-item via `render_cons`, and byte 2 of `declare ...` (`'c'`) vs `define ...` (`'f'`).
  have hRen : rendered =
      "declare {i8, i1} @llvm.uadd.with.overflow.i8(i8, i8)\n\n" ++
      "define i32 @concrete_body() {\n" ++
      "entry:\n" ++
      "  %o0 = call {i8, i1} @llvm.uadd.with.overflow.i8(i8 20, i8 22)\n" ++
      "  %r0 = extractvalue {i8, i1} %o0, 0\n" ++
      "  %r1 = extractvalue {i8, i1} %o0, 1\n" ++
      "  br i1 %r1, label %t0, label %b2\n" ++
      "t0:\n" ++
      "  ret i32 256\n" ++
      "b2:\n" ++
      "  %ret2 = zext i8 %r0 to i32\n" ++
      "  ret i32 %ret2\n" ++
      "}\n" := by
    dsimp only [rendered, compCheckedAddU8]
    simp only [render_cons, render_nil, String.append_empty]
    have h0 : renderOne (.declare (.overflowPair .u8) (.overflow .add .u8) [.integer .u8, .integer .u8]) =
        "declare {i8, i1} @llvm.uadd.with.overflow.i8(i8, i8)\n" := by cbv
    have h1 : renderOne .blank = "\n" := rfl
    have h2 : renderOne (.define false (.integer .u32) .body []) = "define i32 @concrete_body() {\n" := by cbv
    have h3 : renderOne (.label .entry) = "entry:\n" := rfl
    have h4 : renderOne (.instruction (.call (some (.overflow 0)) (.overflowPair .u8) (.overflow .add .u8)
        [(.integer .u8, .immediate (.natural 20)), (.integer .u8, .immediate (.natural 22))])) =
        "  %o0 = call {i8, i1} @llvm.uadd.with.overflow.i8(i8 20, i8 22)\n" := by cbv
    have h5 : renderOne (.instruction (.extract (.register 0) (.overflowPair .u8) (.overflow 0) 0)) =
        "  %r0 = extractvalue {i8, i1} %o0, 0\n" := by cbv
    have h6 : renderOne (.instruction (.extract (.register 1) (.overflowPair .u8) (.overflow 0) 1)) =
        "  %r1 = extractvalue {i8, i1} %o0, 1\n" := by cbv
    have h7 : renderOne (.terminator (.cond (.register 1) (.trapSite 0) (.block 2))) =
        "  br i1 %r1, label %t0, label %b2\n" := by cbv
    have h8 : renderOne (.label (.trapSite 0)) = "t0:\n" := by cbv
    have h9 : renderOne (.terminator (.ret (.integer .u32, .immediate (.natural 256)))) =
        "  ret i32 256\n" := by cbv
    have h10 : renderOne (.label (.block 2)) = "b2:\n" := by cbv
    have h11 : renderOne (.instruction (.convert (.result 2) .zext (.integer .u8, .local (.register 0)) (.integer .u32))) =
        "  %ret2 = zext i8 %r0 to i32\n" := by cbv
    have h12 : renderOne (.terminator (.ret (.integer .u32, .local (.result 2)))) =
        "  ret i32 %ret2\n" := by cbv
    have h13 : renderOne .endFunction = "}\n" := rfl
    rw [h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13]
    simp only [String.append_assoc]
    rfl
  have hDiff : inChecked.bytesAt .component ≠ inPlain.bytesAt .component := by
    refine body_diff_bytesAt 2 ?_
    change (render compCheckedAddU8.items).toUTF8.data[2]? ≠ (render (compReturn 42).items).toUTF8.data[2]?
    simp only [compCheckedAddU8, compReturn, render_cons, render_nil, String.append_empty]
    rw [prefixByte (renderOne (.declare (.overflowPair .u8) (.overflow .add .u8) [.integer .u8, .integer .u8])) _ 2 (by cbv),
      prefixByte (renderOne (.define false (.integer .u32) .body [])) _ 2 (by cbv)]
    cbv; decide
  exact ⟨⟨rfl, rfl, ⟨[line0], hMap0, rfl⟩⟩, hRen, rfl, hDiff⟩

end StageInterfaceControls.TwoModuleLLVMControls
