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
   - direct structure-eta, `Item`-exhaustiveness, and exact-header-prefix theorems showing
     `LLVMModule` is determined by `items` alone, `LLVMInput` by `(component, launcher, sites)`
     alone, `Item` has no target-directive or raw-text constructor, and `bytesAt` unconditionally
     embeds the profile header on BOTH roles, paired with `#guard_msgs` controls rejecting raw
     target-header strings as `Item` and cross-profile `LLVMInput` coercions;
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
10. Two-result checked-add LLVM component rendering (`@llvm.uadd.with.overflow.i8` + `extractvalue`);
11. Symbol-ownership split (`ComponentOwnsSymbols`) and `.body` exclusivity: negative controls
    rejecting components that define any launcher-owned symbol (`.main`, `.writeAll`, `.bad`,
    `.site 0`, `.write`, `.exit`, `.abort`) or bind `.body` via `.constant`, `.declare`,
    `internal`, wrong signature, or omit it.
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

/-- Structured LLVM launcher module rendered directly from canonical diagnostic lines via
`launcherItems` (`llvmBodyDeclaration :: llvmDiagnostics lines`). -/
private def launchOfLines (lines : List String) : LLVMModule :=
  ⟨launcherItems lines⟩

private theorem compReturn_ownsSymbols (status : Nat) (diagnostics : List String) :
    ComponentOwnsSymbols (compReturn status).items (launcherItems diagnostics) := by
  refine ComponentOwnsSymbols.of_component_globals diagnostics ?_ ?_
  · intro g hg
    exact List.mem_singleton.mp hg
  · intro g hg
    exact Or.inl (List.mem_singleton.mp hg)

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

private theorem items_ne_of_second_ne {a b : List Item} (h : a[1]? ≠ b[1]?) : a ≠ b :=
  fun heq => h (congrArg (fun l => l[1]?) heq)

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
  -- `launcherItems` prepends `llvmBodyDeclaration` (`"declare i32 @concrete_body()\n"`, 29 bytes)
  -- before `@.site0`, shifting the rendered `.launcher` byte offset by `declLen = 29`.
  let declLen := (renderOne llvmBodyDeclaration).toUTF8.data.size
  refine body_diff_bytesAt (declLen + idx) ?_
  change (render (launcherItems [s₁])).toUTF8.data[declLen + idx]? ≠
    (render (launcherItems [s₂])).toUTF8.data[declLen + idx]?
  obtain ⟨t₁, ht₁⟩ : ∃ t, llvmDiagnostics [s₁] = Item.constant (.site 0) s₁.toUTF8 :: t := ⟨_, rfl⟩
  obtain ⟨t₂, ht₂⟩ : ∃ t, llvmDiagnostics [s₂] = Item.constant (.site 0) s₂.toUTF8 :: t := ⟨_, rfl⟩
  simp only [launcherItems, ht₁, ht₂, render_cons]
  rw [suffixByte (renderOne llvmBodyDeclaration) _ idx,
    suffixByte (renderOne llvmBodyDeclaration) _ idx,
    prefixByte _ _ idx h₁, prefixByte _ _ idx h₂]
  exact hne

/-! ## Control 1: Two trap-table slots for one operation -/

/-- One operation origin (`orig0`) occupying two distinct trap-table slots (`0` and `1`):
`LauncherMatches` and `LLVMMetadataMatches` accept the two-slot table, the launcher leads with
`llvmBodyDeclaration`, the component has `llvmBodyDefinition` as its unique body header, both
`@.site0` and `@.site1` globals and switch cases `0`/`1` are emitted, and dropping either slot
fails `LauncherMatches`. -/
theorem two_slots_for_one_op :
    let sitesTwo : TrapTable := #[⟨.overflow, orig0⟩, ⟨.overflow, orig0⟩]
    let bir := mkBIR llvmLinux projMain sitesTwo
    let out : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [line0, line0], sitesTwo⟩
    let oneSlotLauncher : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [line0], sitesTwo⟩
    LauncherMatches bir out ∧
    LLVMMetadataMatches bir out ∧
    out.component.items.filter isLLVMBodyHeader = [llvmBodyDefinition] ∧
    out.launcher.items.head? = some llvmBodyDeclaration ∧
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
  have hMatch : LauncherMatches bir out :=
    ⟨[line0, line0], hMap, rfl, rfl, compReturn_ownsSymbols 42 [line0, line0]⟩
  refine ⟨hMatch, ⟨rfl, rfl, hMatch⟩, rfl, rfl,
    by decide, by decide, by decide, ?_, ?_⟩
  · intro hBad
    exact items_ne_of_length_ne (by decide) (hBad.launcher_items_eq hMap)
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
  -- Index 0 of `launcherItems` is `llvmBodyDeclaration`; `@.site0` is at index 1.
  have hSecondNe : (launchOfLines [line0]).items[1]? ≠ (launcherItems [lineSite7])[1]? := by
    intro h
    have hBytes := congrArg (fun o => o.map fun
      | Item.constant _ bs => bs.data.toList
      | _ => []) h
    exact (by decide : some line0.toUTF8.data.toList ≠ some lineSite7.toUTF8.data.toList) hBytes
  -- Byte 87 of `@.site0 = private constant [56 x i8] c"concrete: trap: overflow at main.con:1:30 (site 7)\0A"\n`
  -- (byte 29 + 87 = 116 of rendered `launcherItems`) is `'7'` (55) vs `'0'` (48).
  refine ⟨by decide, by decide,
    ⟨rfl, rfl, ⟨[lineSite7], hMap7, rfl, rfl, compReturn_ownsSymbols 42 [lineSite7]⟩⟩,
    fun h => items_ne_of_second_ne hSecondNe (h.launcher_items_eq hMap7),
    fun h => items_ne_of_second_ne hSecondNe (h.launcherMatches.launcher_items_eq hMap7),
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
  -- Index 0 of `launcherItems` is `llvmBodyDeclaration`; `@.site0` is at index 1.
  have hSecondNe (s : String) (hne : line0.toUTF8.data.toList ≠ s.toUTF8.data.toList) :
      (launchOfLines [line0]).items[1]? ≠ (launcherItems [s])[1]? := by
    intro h
    have hb := congrArg (fun o => o.map fun | Item.constant _ bs => bs.data.toList | _ => []) h
    exact hne (Option.some.inj hb)
  -- Shifted offsets inside rendered `launcherItems` after the 29-byte `llvmBodyDeclaration` line:
  -- line diff: 29 + 76 = 105; col diff: 29 + 79 = 108; path/length diff: 29 + 29 = 58.
  refine ⟨⟨rfl, rfl, ⟨[lineRow2], hMapL2, rfl, rfl, compReturn_ownsSymbols 42 [lineRow2]⟩⟩,
    ⟨rfl, rfl, ⟨[lineCol31], hMapC31, rfl, rfl, compReturn_ownsSymbols 42 [lineCol31]⟩⟩,
    ⟨rfl, rfl, ⟨[lineAltPath], hMapAlt, rfl, rfl, compReturn_ownsSymbols 42 [lineAltPath]⟩⟩,
    fun h => items_ne_of_second_ne (hSecondNe lineRow2 (by decide)) (h.launcher_items_eq hMapL2),
    fun h => items_ne_of_second_ne (hSecondNe lineCol31 (by decide)) (h.launcher_items_eq hMapC31),
    fun h => items_ne_of_second_ne (hSecondNe lineAltPath (by decide)) (h.launcher_items_eq hMapAlt),
    first_site_launcher_bytes_ne lineRow2 line0 (compReturn 42) (compReturn 42) birLine2.sites birBase.sites 76
      (by cbv) (by cbv) (by cbv; decide),
    first_site_launcher_bytes_ne lineCol31 line0 (compReturn 42) (compReturn 42) birCol31.sites birBase.sites 79
      (by cbv) (by cbv) (by cbv; decide),
    first_site_launcher_bytes_ne lineAltPath line0 (compReturn 42) (compReturn 42) birAltPath.sites birBase.sites 29
      (by cbv) (by cbv) (by cbv; decide)⟩

/-! ## Control 4: Wrong triple/layout unrepresentable and platform-distinct on BOTH roles -/

/-- `LLVMModule` has no target-header fields: it is determined exclusively by `items`. -/
theorem llvmModule_determined_by_items :
    (∀ m : LLVMModule, m = ⟨m.items⟩) ∧
    (∀ m₁ m₂ : LLVMModule, m₁.items = m₂.items → m₁ = m₂) := by
  refine ⟨fun m => by cases m; rfl, fun m₁ m₂ h => by cases m₁; cases m₂; cases h; rfl⟩

/-- `LLVMInput p` carries no separate triple/datalayout fields: it is determined exclusively by
`(component, launcher, sites)`. -/
theorem llvmInput_determined_by_fields (p : Profile) :
    (∀ x : LLVMInput p, x = ⟨x.component, x.launcher, x.sites⟩) ∧
    (∀ a b : LLVMInput p,
      a.component = b.component → a.launcher = b.launcher → a.sites = b.sites → a = b) := by
  refine ⟨fun x => by cases x; rfl, fun a b hc hl hs => by
    cases a; cases b; cases hc; cases hl; cases hs; rfl⟩

/-- `Backend.LLVM.Item` is closed to exactly 8 structured assembly constructors, with no
target-directive or raw-text constructor. -/
theorem llvmItem_exhaustive_no_target_directive (item : Item) :
    (∃ result fn args, item = .declare result fn args) ∨
    (∃ internal result fn args, item = .define internal result fn args) ∨
    (∃ name bytes, item = .constant name bytes) ∨
    (∃ name, item = .label name) ∨
    (∃ inst, item = .instruction inst) ∨
    (∃ term, item = .terminator term) ∨
    item = .endFunction ∨
    item = .blank := by
  cases item with
  | declare result fn args => exact Or.inl ⟨result, fn, args, rfl⟩
  | define internal result fn args => exact Or.inr (Or.inl ⟨internal, result, fn, args, rfl⟩)
  | constant name bytes => exact Or.inr (Or.inr (Or.inl ⟨name, bytes, rfl⟩))
  | label name => exact Or.inr (Or.inr (Or.inr (Or.inl ⟨name, rfl⟩)))
  | instruction inst => exact Or.inr (Or.inr (Or.inr (Or.inr (Or.inl ⟨inst, rfl⟩))))
  | terminator term => exact Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inl ⟨term, rfl⟩)))))
  | endFunction => exact Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inl rfl))))))
  | blank => exact Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr rfl))))))

/-- For EVERY `LLVMInput p` and BOTH roles (`.component` and `.launcher`), `bytesAt` is
unconditionally prefixed by `p`'s exact `target datalayout` and `target triple` header, so no pair
of inputs across `x86_64Linux`, `aarch64Linux`, and `aarch64Macos` can ever produce equal bytes on
either role. -/
theorem llvmInput_bytesAt_exact_profile_header :
    (∀ (p : Profile) (inp : LLVMInput p) (role : LLVMRole),
      inp.bytesAt role = (headerOf p ++ render (inp.moduleAt role).items).toUTF8 ∧
      ∀ idx, idx < (headerOf p).toUTF8.data.size →
        (inp.bytesAt role).data[idx]? = (headerOf p).toUTF8.data[idx]?) ∧
    (∀ (inX64 : LLVMInput llvmLinux) (inArmLin : LLVMInput llvmAarch64Linux)
      (inMac : LLVMInput llvmMac) (role : LLVMRole),
      inX64.bytesAt role ≠ inArmLin.bytesAt role ∧
      inX64.bytesAt role ≠ inMac.bytesAt role ∧
      inArmLin.bytesAt role ≠ inMac.bytesAt role) := by
  refine ⟨fun p inp role => ⟨rfl, fun idx h => prefixByte _ _ idx h⟩,
    fun inX64 inArmLin inMac role =>
      ⟨header_diff_bytesAt 61 (by cbv) (by cbv) (by cbv; decide),
       header_diff_bytesAt 25 (by cbv) (by cbv) (by cbv; decide),
       header_diff_bytesAt 25 (by cbv) (by cbv) (by cbv; decide)⟩⟩

-- Negative elaboration controls pinned with `#guard_msgs`: raw target-header strings cannot enter
-- `LLVMModule.items`, and an `LLVMInput` cannot cross target-profile boundaries.

/--
error: Application type mismatch: The argument
  "target triple = \"wrong-triple\"\n"
has type
  String
but is expected to have type
  Item
in the application
  List.cons "target triple = \"wrong-triple\"\n"
-/
#guard_msgs in
example : LLVMModule :=
  ⟨["target triple = \"wrong-triple\"\n"]⟩

/--
error: Application type mismatch: The argument
  "target datalayout = \"wrong-layout\"\n"
has type
  String
but is expected to have type
  Item
in the application
  List.cons "target datalayout = \"wrong-layout\"\n"
-/
#guard_msgs in
example : LLVMModule :=
  ⟨["target datalayout = \"wrong-layout\"\n"]⟩

/--
error: Type mismatch
  inp
has type
  LLVMInput llvmLinux
but is expected to have type
  LLVMInput llvmAarch64Linux
-/
#guard_msgs in
example (inp : LLVMInput llvmLinux) : LLVMInput llvmAarch64Linux :=
  inp

/--
error: Type mismatch
  inp
has type
  LLVMInput llvmLinux
but is expected to have type
  LLVMInput llvmMac
-/
#guard_msgs in
example (inp : LLVMInput llvmLinux) : LLVMInput llvmMac :=
  inp

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

/-! ## Control 5 & 6: Swapped roles, stale launcher diagnostics, bare diagnostics, and launcher uniqueness -/

/-- Swapping `.component` and `.launcher` modules fails both `LauncherMatches` and
`LLVMMetadataMatches`; pairing updated `sites` with a stale launcher also fails both; omitting the
leading `llvmBodyDeclaration` (bare `llvmDiagnostics`) fails both; wrong backend kind (`.lcb`)
fails `LLVMMetadataMatches`; and `LauncherMatches.launcher_unique` pins `.launcher` uniquely for
a given `BIR`. -/
theorem swapped_roles_and_stale_launcher_rejected :
    let bir0 := mkBIR llvmLinux projMain #[⟨.overflow, orig0⟩]
    let birEmpty := mkBIR llvmLinux projMain #[]
    let out0 : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [line0], bir0.sites⟩
    let outEmpty : LLVMInput llvmLinux := ⟨compReturn 42, launchOfLines [], birEmpty.sites⟩
    let swapped : LLVMInput llvmLinux := ⟨out0.launcher, out0.component, out0.sites⟩
    let staleEmptyLauncher : LLVMInput llvmLinux := ⟨compReturn 42, outEmpty.launcher, bir0.sites⟩
    let staleSitesOnEmpty : LLVMInput llvmLinux := ⟨compReturn 42, out0.launcher, birEmpty.sites⟩
    let bareDiagLauncher : LLVMInput llvmLinux := ⟨compReturn 42, ⟨llvmDiagnostics [line0]⟩, bir0.sites⟩
    let lcbOut : LLVMInput lcbLinux := ⟨compReturn 42, launchOfLines [line0], bir0.sites⟩
    let birLCB := mkBIR lcbLinux projMain #[⟨.overflow, orig0⟩]
    LLVMMetadataMatches bir0 out0 ∧
    ¬ LauncherMatches bir0 swapped ∧
    ¬ LLVMMetadataMatches bir0 swapped ∧
    ¬ LauncherMatches bir0 staleEmptyLauncher ∧
    ¬ LLVMMetadataMatches bir0 staleEmptyLauncher ∧
    ¬ LauncherMatches birEmpty staleSitesOnEmpty ∧
    ¬ LauncherMatches bir0 bareDiagLauncher ∧
    ¬ LLVMMetadataMatches bir0 bareDiagLauncher ∧
    ¬ LLVMMetadataMatches birLCB lcbOut ∧
    (∀ other : LLVMInput llvmLinux, LauncherMatches bir0 other → other.launcher = out0.launcher) := by
  intro bir0 birEmpty out0 outEmpty swapped staleEmptyLauncher staleSitesOnEmpty bareDiagLauncher lcbOut birLCB
  have hMap0 : bir0.sites.toList.mapM (fun entry => entry.diagnosticLine bir0.project) =
      some [line0] := by decide
  have hMapE : birEmpty.sites.toList.mapM (fun entry => entry.diagnosticLine birEmpty.project) =
      some [] := by decide
  have hMatch0 : LauncherMatches bir0 out0 :=
    ⟨[line0], hMap0, rfl, rfl, compReturn_ownsSymbols 42 [line0]⟩
  refine ⟨⟨rfl, rfl, hMatch0⟩,
    fun h => items_ne_of_length_ne (by decide) (h.launcher_items_eq hMap0),
    fun h => items_ne_of_length_ne (by decide) (h.launcherMatches.launcher_items_eq hMap0),
    fun h => items_ne_of_length_ne (by decide) (h.launcher_items_eq hMap0),
    fun h => items_ne_of_length_ne (by decide) (h.launcherMatches.launcher_items_eq hMap0),
    fun h => items_ne_of_length_ne (by decide) (h.launcher_items_eq hMapE),
    fun h => items_ne_of_length_ne (by decide) (h.launcher_items_eq hMap0),
    fun h => items_ne_of_length_ne (by decide) (h.launcherMatches.launcher_items_eq hMap0),
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
  -- In rendered `launcherItems`, byte 29 + 76 = 105 differs (`'1'` vs `'2'`).
  have hLaunchNe : in42.bytesAt .launcher ≠ inLine2.bytesAt .launcher :=
    first_site_launcher_bytes_ne line0 lineRow2 (compReturn 42) (compReturn 42) bir0.sites birLine2.sites 76
      (by cbv) (by cbv) (by cbv; decide)
  exact ⟨⟨rfl, rfl, ⟨[line0], hMap0, rfl, rfl, compReturn_ownsSymbols 0 [line0]⟩⟩,
    ⟨rfl, rfl, ⟨[line0], hMap0, rfl, rfl, compReturn_ownsSymbols 42 [line0]⟩⟩,
    rfl, rfl, hCompNe, hCompNe, packageBytes_ne_of_component hCompNe,
    ⟨rfl, rfl, ⟨[lineRow2], hMapL2, rfl, rfl, compReturn_ownsSymbols 42 [lineRow2]⟩⟩,
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

private theorem compCheckedAddU8_ownsSymbols (diagnostics : List String) :
    ComponentOwnsSymbols compCheckedAddU8.items (launcherItems diagnostics) := by
  refine ComponentOwnsSymbols.of_component_globals diagnostics ?_ ?_
  · intro g hg
    exact List.mem_singleton.mp hg
  · intro g hg
    simp only [compCheckedAddU8, llvmModuleGlobals, List.filterMap, llvmItemGlobal,
      List.mem_cons, List.not_mem_nil, or_false] at hg
    rcases hg with rfl | rfl
    · exact Or.inr ⟨.add, .u8, rfl⟩
    · exact Or.inl rfl

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
  exact ⟨⟨rfl, rfl, ⟨[line0], hMap0, rfl, rfl, compCheckedAddU8_ownsSymbols [line0]⟩⟩, hRen, rfl, hDiff⟩

/-! ## Control 10: Symbol-ownership split and `.body` exclusivity negative controls -/

/-- Negative controls for every launcher-owned symbol class (`.main`, `.writeAll`, `.bad`,
`.site 0`, `.write`, `.exit`, `.abort`) plus `.constant .body` (both standalone and alongside
`llvmBodyDefinition`), internal/wrong-signature/declared/missing/duplicate `.body`: every case is
rejected by both `LauncherMatches` and `LLVMMetadataMatches` even when paired with exact
`launcherItems`. -/
theorem symbol_ownership_and_body_exclusivity_controls :
    let bir0 := mkBIR llvmLinux projMain #[⟨.overflow, orig0⟩]
    let mkOut (items : List Item) : LLVMInput llvmLinux :=
      ⟨⟨items⟩, launchOfLines [line0], bir0.sites⟩
    let withBody (extra : List Item) : LLVMInput llvmLinux :=
      mkOut ((compReturn 42).items ++ extra)
    -- Launcher-owned symbol classes:
    ¬ LauncherMatches bir0 (withBody [.define false (.integer .u32) .main [], .endFunction]) ∧
    ¬ LLVMMetadataMatches bir0 (withBody [.define false (.integer .u32) .main [], .endFunction]) ∧
    ¬ LauncherMatches bir0 (withBody [.define true .void .writeAll [(.ptr, .p), (.integer .u64, .n)], .endFunction]) ∧
    ¬ LLVMMetadataMatches bir0 (withBody [.define true .void .writeAll [(.ptr, .p), (.integer .u64, .n)], .endFunction]) ∧
    ¬ LauncherMatches bir0 (withBody [.constant .bad ⟨#[0]⟩]) ∧
    ¬ LLVMMetadataMatches bir0 (withBody [.constant .bad ⟨#[0]⟩]) ∧
    ¬ LauncherMatches bir0 (withBody [.constant (.site 0) ⟨#[0]⟩]) ∧
    ¬ LLVMMetadataMatches bir0 (withBody [.constant (.site 0) ⟨#[0]⟩]) ∧
    ¬ LauncherMatches bir0 (withBody [.define false (.integer .u64) .write
      [(.integer .u32, .p), (.ptr, .pp), (.integer .u64, .n)], .endFunction]) ∧
    ¬ LLVMMetadataMatches bir0 (withBody [.define false (.integer .u64) .write
      [(.integer .u32, .p), (.ptr, .pp), (.integer .u64, .n)], .endFunction]) ∧
    ¬ LauncherMatches bir0 (withBody [.define false .void .exit [(.integer .u32, .p)], .endFunction]) ∧
    ¬ LLVMMetadataMatches bir0 (withBody [.define false .void .exit [(.integer .u32, .p)], .endFunction]) ∧
    ¬ LauncherMatches bir0 (withBody [.define false .void .abort [], .endFunction]) ∧
    ¬ LLVMMetadataMatches bir0 (withBody [.define false .void .abort [], .endFunction]) ∧
    -- `.constant .body` (standalone and alongside `llvmBodyDefinition`):
    ¬ LauncherMatches bir0 (mkOut [.constant .body ⟨#[0]⟩]) ∧
    ¬ LLVMMetadataMatches bir0 (mkOut [.constant .body ⟨#[0]⟩]) ∧
    ¬ LauncherMatches bir0 (withBody [.constant .body ⟨#[0]⟩]) ∧
    ¬ LLVMMetadataMatches bir0 (withBody [.constant .body ⟨#[0]⟩]) ∧
    -- Invalid `.body` headers (missing, internal, wrong return type, wrong params, declare, duplicate):
    ¬ LauncherMatches bir0 (mkOut []) ∧
    ¬ LauncherMatches bir0 (mkOut [.define true (.integer .u32) .body [], .endFunction]) ∧
    ¬ LauncherMatches bir0 (mkOut [.define false (.integer .u8) .body [], .endFunction]) ∧
    ¬ LauncherMatches bir0 (mkOut [.define false (.integer .u32) .body [(.integer .u32, .p)], .endFunction]) ∧
    ¬ LauncherMatches bir0 (mkOut [.declare (.integer .u32) .body []]) ∧
    ¬ LauncherMatches bir0 (withBody [llvmBodyDefinition, .endFunction]) := by
  intro bir0 mkOut withBody
  have rejectLauncherSym : ∀ (extra : List Item) (item : Item) (g : Global),
      item ∈ extra →
      llvmItemGlobal item = some g →
      g ∈ llvmModuleGlobals (launcherItems [line0]) →
      g ≠ llvmBodyGlobal →
      ¬ LauncherMatches bir0 (withBody extra) := by
    intro extra item g hIn hGlob hInLaunch hNe hMatch
    have hCompGlob : g ∈ llvmModuleGlobals (withBody extra).component.items :=
      List.mem_filterMap.mpr ⟨item, List.mem_append_right _ hIn, hGlob⟩
    exact hNe (hMatch.shared_global_eq_body g hCompGlob hInLaunch)
  have hMain : ¬ LauncherMatches bir0 (withBody [.define false (.integer .u32) .main [], .endFunction]) :=
    rejectLauncherSym _ (.define false (.integer .u32) .main []) .main List.mem_cons_self rfl
      (by rw [llvmModuleGlobals_launcherItems]; simp) (by intro h; cases h)
  have hWriteAll : ¬ LauncherMatches bir0
      (withBody [.define true .void .writeAll [(.ptr, .p), (.integer .u64, .n)], .endFunction]) :=
    rejectLauncherSym _ (.define true .void .writeAll [(.ptr, .p), (.integer .u64, .n)]) .writeAll
      List.mem_cons_self rfl (by rw [llvmModuleGlobals_launcherItems]; simp) (by intro h; cases h)
  have hBad : ¬ LauncherMatches bir0 (withBody [.constant .bad ⟨#[0]⟩]) :=
    rejectLauncherSym _ (.constant .bad ⟨#[0]⟩) .bad List.mem_cons_self rfl
      (by rw [llvmModuleGlobals_launcherItems]; simp) (by intro h; cases h)
  have hSite0 : ¬ LauncherMatches bir0 (withBody [.constant (.site 0) ⟨#[0]⟩]) :=
    rejectLauncherSym _ (.constant (.site 0) ⟨#[0]⟩) (.site 0) List.mem_cons_self rfl
      (by rw [llvmModuleGlobals_launcherItems]; simp [indexed, indexedFrom])
      (by intro h; cases h)
  have hWrite : ¬ LauncherMatches bir0 (withBody [.define false (.integer .u64) .write
      [(.integer .u32, .p), (.ptr, .pp), (.integer .u64, .n)], .endFunction]) :=
    rejectLauncherSym _ (.define false (.integer .u64) .write
      [(.integer .u32, .p), (.ptr, .pp), (.integer .u64, .n)]) .write
      List.mem_cons_self rfl (by rw [llvmModuleGlobals_launcherItems]; simp) (by intro h; cases h)
  have hExit : ¬ LauncherMatches bir0
      (withBody [.define false .void .exit [(.integer .u32, .p)], .endFunction]) :=
    rejectLauncherSym _ (.define false .void .exit [(.integer .u32, .p)]) .exit
      List.mem_cons_self rfl (by rw [llvmModuleGlobals_launcherItems]; simp) (by intro h; cases h)
  have hAbort : ¬ LauncherMatches bir0 (withBody [.define false .void .abort [], .endFunction]) :=
    rejectLauncherSym _ (.define false .void .abort []) .abort
      List.mem_cons_self rfl (by rw [llvmModuleGlobals_launcherItems]; simp) (by intro h; cases h)
  have hConstBodyAlone : ¬ LauncherMatches bir0 (mkOut [.constant .body ⟨#[0]⟩]) := fun h =>
    h.no_component_constant_body ⟨#[0]⟩ List.mem_cons_self
  have hConstBodyExtra : ¬ LauncherMatches bir0 (withBody [.constant .body ⟨#[0]⟩]) := fun h =>
    h.no_component_constant_body ⟨#[0]⟩ (List.mem_append_right _ List.mem_cons_self)
  refine ⟨hMain, fun h => hMain h.launcherMatches,
    hWriteAll, fun h => hWriteAll h.launcherMatches,
    hBad, fun h => hBad h.launcherMatches,
    hSite0, fun h => hSite0 h.launcherMatches,
    hWrite, fun h => hWrite h.launcherMatches,
    hExit, fun h => hExit h.launcherMatches,
    hAbort, fun h => hAbort h.launcherMatches,
    hConstBodyAlone, fun h => hConstBodyAlone h.launcherMatches,
    hConstBodyExtra, fun h => hConstBodyExtra h.launcherMatches,
    ?_, ?_, ?_, ?_, ?_, ?_⟩
  all_goals (intro h; have hc := h.component_body_header_eq; cases hc)

end StageInterfaceControls.TwoModuleLLVMControls
