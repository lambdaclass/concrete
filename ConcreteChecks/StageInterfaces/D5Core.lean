import Concrete.IntegratedCore

/-!
# D5 Core-to-MIR Witnesses, Universal Validation, and Wrong-Compiler Mutants

Standalone freeze controls for Unit P2 covering `ISS-0113` and `ISS-0118` directly
against the frozen stage interfaces and production `Concrete.Core.LowerMIR` /
`Concrete.IntegratedCore` (zero dependency on legacy fixture modules):

1. Universal validation and `Stage.Sound` instantiations on fixed slice instances
   (`slice0`, `sliceOne`) across both `.lcb` and `.llvm` compilation profiles for
   `admission`, `typing`, `lowerCore`, and `lowerUnchecked`.
2. Reachable whole-program witnesses:
   - Slice 0 checked addition (`20 + 22`, exit `42`, `.discharged` check)
   - Slice 0 partial-discharge lowering (`20 + 22` with `discharges = []`, `.residual` check)
   - Slice 0 unchecked addition (`20 + 22`, exit `42`, `.residual` check)
   - Slice 0 unchecked residual overflow (`255 + 1`, refused by `admission`, accepted by
     `typingEntry` and `lowerUnchecked`, observes exact `.trapped 134 .overflow` with
     resolved `DiagnosticSite` and `trapDiagnostic` stderr bytes)
   - Slice 1 empty-linear nominal construct + move + destructure + immediate dead
     construct-destructure (`let Token {} = Token {};`), checking `MIR.HandoffWF`,
     `DeclarationsWF`, 0-byte `Materializable`, boundary origin preservation, and exit `7`
   - Slice 1 explicit `abort`, checking `.trap .abort` observation with exact site and stderr
3. Rejected wrong-compiler mutants (`¬ (registry mutant).<edge>.Validated`) for:
   - `lowerCore` altering return value (`42` → `43`) on `slice0`
   - `lowerCore` emitting a spurious `.discharged` check when `discharges = []`
   - `lowerCore` leaving a check `.residual` when a valid discharge was supplied
   - `lowerCore` returning `.error` on a valid `Admitted` input
   - `lowerUnchecked` dropping the residual overflow check on `255 + 1` (`.discharged` mutant)
   - `lowerUnchecked` suppressing the runtime overflow trap (`255 + 1` → exit `0` mutant)
   - `lowerUnchecked` altering return value (`42` → `43`) on `slice0`
   - `lowerUnchecked` returning `.error` on valid `UncheckedWF` input
   - `admission` accepting overflowing `255 + 1` with empty discharges
   - `lowerCore` altering return value (`7` → `8`) on `sliceOne`
   - `lowerUnchecked` suppressing explicit `abort` (`abort` → exit `0`) on `sliceOne`
4. Structural negative controls (`ISS-0118`):
   - `MIR.HandoffWF` rejects a MIR module whose nominal local is undeclared
   - Cross-slice admission rejects a Slice 1 `abort` Core input on `slice0`
-/

namespace ConcreteChecks.StageInterfaces.D5Core

open ConcreteSpec
open ConcreteSpec.Core
open ConcreteSpec.Core.SourcePathSpan
open ConcreteSpec.StageInterface
open ConcreteSpec.StageGrammar
open Concrete
open Concrete.Contracts
open Concrete.Interfaces

/-! ### Compilation profiles and production pass bundles -/

private def lcbProfile : Profile := ⟨.lcb, .x86_64Linux, 8, 4096⟩
private def llvmProfile : Profile := ⟨.llvm, .x86_64Linux, 8, 4096⟩

private def stubRule : RegisteredRule :=
  ⟨ConcreteSpec.Core.registeredRules.head (by decide), List.head_mem _⟩

private def stubFailure : StageFailure := .invalidInput stubRule

/-- Production Core-stage pass bundle parameterized by slice and profile.
Downstream memory/SSA/BIR/backend slots are unused by Core-edge contracts. -/
def corePasses (slice : Slice) (profile : Profile) : Passes slice profile where
  lex := fun _ => .error (.missingFile ⟨0⟩)
  elaborate := IntegratedCore.elaborate slice
  checkCore := ConcreteSpec.Admission.checkInput
  lowerCore := IntegratedCore.lowerCore
  lowerUnchecked := IntegratedCore.lowerUnchecked
  lowerMemory := fun _ => .error stubFailure
  buildSSA := fun _ => .error stubFailure
  buildBIR := fun _ => .error stubFailure
  emit := fun _ => .error stubFailure

/-! ### 1. Universal validation and `Stage.Sound` on fixed slice instances -/

theorem slice0_lcb_admission_validated :
    (registry (corePasses slice0 lcbProfile)).admission.Validated :=
  IntegratedCore.admission_validated (corePasses slice0 lcbProfile) rfl

theorem slice0_lcb_admission_sound :
    Stage.Sound (corePasses slice0 lcbProfile) .admission :=
  IntegratedCore.admission_sound (corePasses slice0 lcbProfile) rfl

theorem slice0_llvm_admission_validated :
    (registry (corePasses slice0 llvmProfile)).admission.Validated :=
  IntegratedCore.admission_validated (corePasses slice0 llvmProfile) rfl

theorem sliceOne_lcb_admission_validated :
    (registry (corePasses sliceOne lcbProfile)).admission.Validated :=
  IntegratedCore.admission_validated (corePasses sliceOne lcbProfile) rfl

theorem slice0_typing_validated :
    (typingEntry (sliceLanguage slice0) slice0).Validated :=
  IntegratedCore.typingEntry_validated (sliceLanguage slice0) slice0

theorem sliceOne_typing_validated :
    (typingEntry (sliceLanguage sliceOne) sliceOne).Validated :=
  IntegratedCore.typingEntry_validated (sliceLanguage sliceOne) sliceOne

theorem slice0_lcb_lowerCore_validated :
    (registry (corePasses slice0 lcbProfile)).lowerCore.Validated :=
  IntegratedCore.lowerCore_validated (corePasses slice0 lcbProfile) rfl

theorem slice0_lcb_lowerCore_sound :
    Stage.Sound (corePasses slice0 lcbProfile) .lowerCore :=
  IntegratedCore.lowerCore_sound (corePasses slice0 lcbProfile) rfl

theorem slice0_llvm_lowerCore_validated :
    (registry (corePasses slice0 llvmProfile)).lowerCore.Validated :=
  IntegratedCore.lowerCore_validated (corePasses slice0 llvmProfile) rfl

theorem sliceOne_lcb_lowerCore_validated :
    (registry (corePasses sliceOne lcbProfile)).lowerCore.Validated :=
  IntegratedCore.lowerCore_validated (corePasses sliceOne lcbProfile) rfl

theorem slice0_lcb_lowerUnchecked_validated :
    (registry (corePasses slice0 lcbProfile)).lowerUnchecked.Validated :=
  IntegratedCore.lowerUnchecked_validated (corePasses slice0 lcbProfile) rfl

theorem slice0_lcb_lowerUnchecked_sound :
    Stage.Sound (corePasses slice0 lcbProfile) .lowerUnchecked :=
  IntegratedCore.lowerUnchecked_sound (corePasses slice0 lcbProfile) rfl

theorem slice0_llvm_lowerUnchecked_validated :
    (registry (corePasses slice0 llvmProfile)).lowerUnchecked.Validated :=
  IntegratedCore.lowerUnchecked_validated (corePasses slice0 llvmProfile) rfl

theorem sliceOne_lcb_lowerUnchecked_validated :
    (registry (corePasses sliceOne lcbProfile)).lowerUnchecked.Validated :=
  IntegratedCore.lowerUnchecked_validated (corePasses sliceOne lcbProfile) rfl

/-! ### Standalone fixture project, span binding, and WF checker helpers -/

private def fixtureSourceText : String :=
  "fn main() -> u8 { let a: u8 = 255 + 1; return a; }"

private def fixtureProject : Project :=
  Project.singleton mainSourcePath (SourceText.ofBytes fixtureSourceText.toUTF8.data.toList)

private def spanMain : Surface0.Span := ⟨18, 37, 1, 19, ⟨0⟩⟩
private def spanDecl : Surface0.Span := ⟨22, 23, 1, 23, ⟨0⟩⟩
private def spanLeft : Surface0.Span := ⟨30, 33, 1, 31, ⟨0⟩⟩
private def spanRight : Surface0.Span := ⟨36, 37, 1, 37, ⟨0⟩⟩
private def spanRet : Surface0.Span := ⟨39, 48, 1, 40, ⟨0⟩⟩

private theorem spanMain_binds : ∃ span, (surfaceFileSpan spanMain).Binds fixtureProject span :=
  ⟨⟨⟨0⟩, 18, 37, by decide, 1, 19, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private theorem spanDecl_binds : ∃ span, (surfaceFileSpan spanDecl).Binds fixtureProject span :=
  ⟨⟨⟨0⟩, 22, 23, by decide, 1, 23, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private theorem spanLeft_binds : ∃ span, (surfaceFileSpan spanLeft).Binds fixtureProject span :=
  ⟨⟨⟨0⟩, 30, 33, by decide, 1, 31, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private theorem spanRight_binds : ∃ span, (surfaceFileSpan spanRight).Binds fixtureProject span :=
  ⟨⟨⟨0⟩, 36, 37, by decide, 1, 37, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private theorem spanRet_binds : ∃ span, (surfaceFileSpan spanRet).Binds fixtureProject span :=
  ⟨⟨⟨0⟩, 39, 48, by decide, 1, 40, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private def knownSpan (loc : Surface0.Span) : Bool :=
  loc == spanMain || loc == spanDecl || loc == spanLeft || loc == spanRight || loc == spanRet

private theorem knownSpan_binds {loc : Surface0.Span} (h : knownSpan loc = true) :
    ∃ span, (surfaceFileSpan loc).Binds fixtureProject span := by
  simp only [knownSpan, Bool.or_eq_true, beq_iff_eq] at h
  rcases h with (((rfl | rfl) | rfl) | rfl) | rfl
  · exact spanMain_binds
  · exact spanDecl_binds
  · exact spanLeft_binds
  · exact spanRight_binds
  · exact spanRet_binds

private def knownFileSpan (loc : FileSpan) : Bool :=
  loc == surfaceFileSpan spanMain || loc == surfaceFileSpan spanDecl ||
  loc == surfaceFileSpan spanLeft || loc == surfaceFileSpan spanRight ||
  loc == surfaceFileSpan spanRet

private theorem knownFileSpan_binds {loc : FileSpan} (h : knownFileSpan loc = true) :
    ∃ span, loc.Binds fixtureProject span := by
  simp only [knownFileSpan, Bool.or_eq_true, beq_iff_eq] at h
  rcases h with (((rfl | rfl) | rfl) | rfl) | rfl
  · exact spanMain_binds
  · exact spanDecl_binds
  · exact spanLeft_binds
  · exact spanRight_binds
  · exact spanRet_binds

private theorem step_none_of_final {O : Core.Exec.Operands} {P : Core.Exec.Program O}
    {c : Core.Exec.Config O} {f : Core.Exec.Final O.Val} (hfinal : c.final? = some f) :
    Core.Exec.step P c = none := by
  cases c with | mk ctrl _ _ frames => cases ctrl with
    | exec _ | refused => cases hfinal
    | done => cases frames with | nil => rfl | cons _ _ => cases hfinal
    | exiting e => cases e with
      | brk _ | cont _ => cases hfinal
      | ret _ => cases frames with | nil => rfl | cons _ _ => cases hfinal
    | trapped _ _ => rfl

private def traceSafe {language : CoreLanguage} (m : MachineProgram language) :
    Nat → Core.Exec.Config (language.instrumented m.ownership m.declarations) → Bool
  | 0, c =>
    match c.final? with
    | some (.trapped _ site) => (AList.lookup m.origins site).isSome
    | some _ => true
    | none => false
  | n + 1, c =>
    match c.final? with
    | some (.trapped _ site) => (AList.lookup m.origins site).isSome
    | some _ => true
    | none =>
      match Core.Exec.step m.body c with
      | some (.next next) => traceSafe m n next
      | _ => false

private theorem traceSafe_sound {language : CoreLanguage} (m : MachineProgram language) :
    ∀ (n : Nat) (c state : Core.Exec.Config (language.instrumented m.ownership m.declarations))
      (ev : List (language.instrumented m.ownership m.declarations).Event)
      (kind : TrapKind) (site : SiteId),
      traceSafe m n c = true →
      Core.Exec.Steps m.body c ev state →
      state.final? = some (.trapped kind site) →
      ∃ origin, AList.lookup m.origins site = some origin := by
  intro n
  induction n with
  | zero =>
    intro c state ev kind site hsafe reach trapped
    simp only [traceSafe] at hsafe
    cases reach with
    | refl =>
      rw [trapped] at hsafe; dsimp only at hsafe
      cases hl : AList.lookup m.origins site with
      | none => rw [hl] at hsafe; cases hsafe
      | some org => exact ⟨org, rfl⟩
    | step t _ =>
      cases hf : c.final? with
      | none => rw [hf] at hsafe; cases hsafe
      | some f =>
        have hnone := step_none_of_final (P := m.body) hf
        cases t with
        | silent s => rw [hnone] at s; cases s
        | prim s _ _ => rw [hnone] at s; cases s
  | succ n ih =>
    intro c state ev kind site hsafe reach trapped
    simp only [traceSafe] at hsafe
    cases hf : c.final? with
    | some f =>
      cases reach with
      | refl =>
        rw [trapped] at hsafe; dsimp only at hsafe
        cases hl : AList.lookup m.origins site with
        | none => rw [hl] at hsafe; cases hsafe
        | some org => exact ⟨org, rfl⟩
      | step t _ =>
        have hnone := step_none_of_final (P := m.body) hf
        cases t with
        | silent s => rw [hnone] at s; cases s
        | prim s _ _ => rw [hnone] at s; cases s
    | none =>
      rw [hf] at hsafe
      cases hst : Core.Exec.step m.body c with
      | none => rw [hst] at hsafe; cases hsafe
      | some act =>
        cases act with
        | request _ _ _ _ => rw [hst] at hsafe; cases hsafe
        | next next =>
          rw [hst] at hsafe
          cases reach with
          | refl => rw [hf] at trapped; cases trapped
          | step t rest =>
            cases t with
            | silent s =>
              rw [hst] at s; cases Option.some.inj s
              exact ih next state _ kind site hsafe rest trapped
            | prim s _ _ => rw [hst] at s; cases s

private def viewReadSpans : AdmissionView → List Surface0.Span
  | .zero p => p.resultSpan :: p.bindings.flatMap fun b =>
      match b with
      | ⟨⟨_, .atom _⟩, span, _, _, _⟩ => [span]
      | ⟨⟨_, .add _ _⟩, _, _, (ls, rs), _⟩ => [ls, rs]
  | .one _ => []

private def mkCore (v : AdmissionView) : CoreInput (sliceLanguage v.slice) :=
  ⟨fixtureProject, v.slice, .straightLine v rfl⟩

private theorem mkCore_represents (v : AdmissionView) : v.Represents (mkCore v) :=
  ⟨rfl, rfl, rfl⟩

private theorem mkCore_wf (v : AdmissionView) (fuel : Nat)
    (hnodupOrig : (List.map Prod.fst v.execution.origins).Nodup)
    (hnodupOps : (List.map Prod.fst v.execution.operations).Nodup)
    (hnodupKeys : (List.map (fun e : SiteId × Origin => e.2.evidenceKey) v.execution.origins).Nodup)
    (hops : v.execution.operations.all (fun p => (AList.lookup v.execution.origins p.1).isSome) = true)
    (hsafe : traceSafe v.execution fuel v.execution.initial = true)
    (horigs : v.execution.origins.all (fun p => p.1 == p.2.operation && knownFileSpan p.2.lexical) = true)
    (hargs : v.execution.operations.all (fun p => match p.2 with | .expression _ | .abort => true | _ => false) = true)
    (hdiag : (List.map Prod.snd v.located.declarations ++ v.located.bindings.map (·.declarationSpan) ++
        viewReadSpans v).all knownSpan = true) :
    (mkCore v).WF := by
  refine ⟨⟨hnodupOrig, hnodupOps, hnodupKeys, ?_, ?_, ?_⟩, ?_, ?_⟩
  · intro site op hmem
    have hb := List.all_eq_true.mp hops (site, op) hmem
    cases hl : AList.lookup v.execution.origins site with
    | none => rw [hl] at hb; cases hb
    | some org => exact ⟨org, hl⟩
  · intro state ev kind site reach trapped
    exact traceSafe_sound v.execution fuel _ state ev kind site hsafe reach trapped
  · intro site org hmem
    have hb := List.all_eq_true.mp horigs (site, org) hmem
    simp only [Bool.and_eq_true, beq_iff_eq] at hb
    exact ⟨hb.1, knownFileSpan_binds hb.2⟩
  · intro site op hmem
    have hb := List.all_eq_true.mp hargs (site, op) hmem
    cases op with
    | expression _ | abort => trivial
    | call _ _ | primitive _ _ => cases hb
  · intro loc hmem
    have hdiag' := List.all_eq_true.mp hdiag
    apply knownSpan_binds
    cases v with
    | one _ => exact hdiag' loc hmem
    | zero p =>
      refine hdiag' loc ?_
      have hflat : p.bindings.flatMap (fun b =>
          (match b with
          | ⟨⟨_, .atom a⟩, span, _, _, _⟩ => [(a.toCore, span)]
          | ⟨⟨_, .add l r⟩, _, _, (ls, rs), _⟩ => [(l.toCore, ls), (r.toCore, rs)]).map Prod.snd) =
          p.bindings.flatMap (fun b =>
            match b with
            | ⟨⟨_, .atom _⟩, span, _, _, _⟩ => [span]
            | ⟨⟨_, .add _ _⟩, _, _, (ls, rs), _⟩ => [ls, rs]) := by
        congr 1; funext b
        rcases b with ⟨⟨_, e⟩, _, _, _, _⟩
        cases e <;> rfl
      change loc ∈ List.map Prod.snd (AdmissionView.zero p).located.declarations ++
        (AdmissionView.zero p).located.bindings.map (·.declarationSpan) ++
        (p.resultSpan :: p.bindings.flatMap (fun b =>
          (match b with
          | ⟨⟨_, .atom a⟩, span, _, _, _⟩ => [(a.toCore, span)]
          | ⟨⟨_, .add l r⟩, _, _, (ls, rs), _⟩ => [(l.toCore, ls), (r.toCore, rs)]).map Prod.snd)) at hmem
      rw [hflat] at hmem
      exact hmem

/-- Derive `CoreInput.UncheckedWF` directly from `WF` and the decidable static checker
`ConcreteSpec.checkProgramWith .runtime`. -/
private theorem mkCore_uncheckedWF (v : AdmissionView) (hwf : (mkCore v).WF)
    (hchk : (ConcreteSpec.checkProgramWith .runtime v.program).isOk = true) :
    (mkCore v).UncheckedWF := by
  cases hc : ConcreteSpec.checkProgramWith .runtime v.program with
  | error _ => rw [hc] at hchk; cases hchk
  | ok ds =>
    have hok := (ConcreteSpec.checkProgramWith_iff .runtime v.program).mp ⟨ds, hc⟩
    exact ⟨hwf, ⟨v, mkCore_represents v, hok.1, ConcreteSpec.Admission.body_staticallyTyped hok.2⟩,
      v, mkCore_represents v, hok.2⟩

/-- Combine `LowerMIR.lowerWith_observes_iff` and `Agreement.unchecked_agreement` to
extract the exact unique observation of any lowered MIR directly from `ConcreteSpec.run`. -/
private theorem mir_observes_exact {v : AdmissionView} {profile : Profile}
    {discharges : Discharges (sliceLanguage v.slice)} {mir : MIR (sliceLoweringLanguage v.slice) profile}
    {obs : Concrete.Contracts.Observation}
    (hunchecked : (mkCore v).UncheckedWF)
    (hdisch : ValidDischarges (mkCore v) discharges)
    (hok : Concrete.Core.LowerMIR.lowerWith (mkCore v) discharges = .ok mir)
    (hobsSpec : IntegratedCore.Agreement.Observable (mkCore v) v.program
      (ConcreteSpec.run v.program (v.program.body.length + 1)) obs) :
    (MIR.Execution (language := sliceLanguage v.slice)).observes mir obs := by
  obtain ⟨_, coreObs, hcoreObsable, hcoreIff⟩ :=
    IntegratedCore.Agreement.unchecked_agreement hunchecked v (mkCore_represents v)
  have hobsEq : obs = coreObs := by
    unfold IntegratedCore.Agreement.Observable at hobsSpec hcoreObsable
    generalize ConcreteSpec.run v.program (v.program.body.length + 1) = out at hobsSpec hcoreObsable
    cases out with
    | exit _ => exact hobsSpec.trans hcoreObsable.symm
    | aborted _ => exact Option.some.inj (hobsSpec.symm.trans hcoreObsable)
    | stuck _ | outOfFuel => exact False.elim hobsSpec
    | trap k =>
      cases k with
      | arithmeticOverflow => exact Option.some.inj (hobsSpec.symm.trans hcoreObsable)
  subst hobsEq
  exact (Concrete.Core.LowerMIR.lowerWith_observes_iff hunchecked hdisch hok obs).mpr
    ((hcoreIff obs).mpr rfl)

/-- Extract `CoreInput.Execution.observes` from `LowersCore` and a MIR observation. -/
private theorem core_observes_of_lowersCore {slice : Slice} {profile : Profile}
    {input : Admitted (sliceLanguage slice)} {mir : MIR (sliceLoweringLanguage slice) profile}
    {obs : Concrete.Contracts.Observation}
    (hsafe : ProgramSafe CoreInput.Execution input.core)
    (hlow : LowersCore (language := sliceLanguage slice) input mir)
    (hobs : (MIR.Execution (language := sliceLanguage slice)).observes mir obs) :
    CoreInput.Execution.observes input.core obs := by
  obtain ⟨q, hfinQ, hresQ⟩ := hobs
  obtain ⟨p, hpFin, hpRes, _⟩ := (hlow.2.2.2.2.2 hsafe).1 q obs hfinQ hresQ
  exact ⟨p, hpFin, hpRes⟩

/-! ### Concrete Slice 0 and Slice 1 fixtures -/

/-- Slice 0 fixture: `let v0 = 20 + 22; return v0;` (exits 42, admitted with discharge). -/
private def addOkView : AdmissionView :=
  .zero {
    bindings := [⟨⟨⟨0⟩, .add (.lit 20) (.lit 22)⟩, spanMain, spanDecl, (spanLeft, spanRight), .unnamed none⟩]
    ret := .var ⟨0⟩
    returnSpan := spanRet
    resultSpan := spanRet
  }

/-- Slice 0 fixture: `let v0 = 21 + 22; return v0;` (exits 43, used for wrong-return mutants). -/
private def add43View : AdmissionView :=
  .zero {
    bindings := [⟨⟨⟨0⟩, .add (.lit 21) (.lit 22)⟩, spanMain, spanDecl, (spanLeft, spanRight), .unnamed none⟩]
    ret := .var ⟨0⟩
    returnSpan := spanRet
    resultSpan := spanRet
  }

/-- Slice 0 fixture: `return 0;` (exits 0, used for dropped-trap mutants). -/
private def ret0View : AdmissionView :=
  .zero {
    bindings := []
    ret := .lit 0
    returnSpan := spanRet
    resultSpan := spanRet
  }

/-- Slice 0 overflowing fixture: `let v0 = 255 + 1; return v0;` (refused by checked admission,
accepted by unchecked typing, traps with status 134 `.overflow` at site `⟨0⟩`). -/
private def overflowView : AdmissionView :=
  .zero {
    bindings := [⟨⟨⟨0⟩, .add (.lit 255) (.lit 1)⟩, spanMain, spanDecl, (spanLeft, spanRight), .unnamed none⟩]
    ret := .var ⟨0⟩
    returnSpan := spanRet
    resultSpan := spanRet
  }

private def tokenName : ConcreteSpec.Name := ⟨"Token"⟩

/-- Slice 1 fixture exercising all empty-linear nominal operations plus immediate dead
construct-destructure (`let Token {} = Token {};`), exiting 7. -/
private def nominalView : AdmissionView :=
  .one {
    declarations := [(tokenName, spanDecl)]
    bindings := [
      ⟨.bind (.name ⟨0⟩) (.nominal tokenName) (.construct tokenName), spanMain, spanDecl, .unnamed none⟩,
      ⟨.bind (.name ⟨1⟩) (.nominal tokenName) (.move ⟨0⟩), spanMain, spanDecl, .unnamed none⟩,
      ⟨.destructure tokenName (.move ⟨1⟩), spanMain, spanDecl, .unnamed none⟩,
      ⟨.destructure tokenName (.construct tokenName), spanMain, spanDecl, .unnamed none⟩
    ]
    ending := .ret (.lit 7)
    endSpan := spanRet
  }

/-- Slice 1 fixture exiting 8 (used for Slice 1 wrong-return mutant). -/
private def nominal8View : AdmissionView :=
  .one {
    declarations := [(tokenName, spanDecl)]
    bindings := []
    ending := .ret (.lit 8)
    endSpan := spanRet
  }

/-- Slice 1 explicit `abort` fixture (traps with status 134 `.abort` at site `⟨0⟩`). -/
private def abortView : AdmissionView :=
  .one {
    declarations := []
    bindings := []
    ending := .abort
    endSpan := spanRet
  }

/-- Slice 1 exit-0 fixture (used for Slice 1 dropped-abort mutant). -/
private def ret0OneView : AdmissionView :=
  .one {
    declarations := []
    bindings := []
    ending := .ret (.lit 0)
    endSpan := spanRet
  }

private def addOkCore : CoreInput (sliceLanguage slice0) := mkCore addOkView
private def add43Core : CoreInput (sliceLanguage slice0) := mkCore add43View
private def ret0Core : CoreInput (sliceLanguage slice0) := mkCore ret0View
private def overflowCore : CoreInput (sliceLanguage slice0) := mkCore overflowView
private def nominalCore : CoreInput (sliceLanguage sliceOne) := mkCore nominalView
private def nominal8Core : CoreInput (sliceLanguage sliceOne) := mkCore nominal8View
private def abortCore : CoreInput (sliceLanguage sliceOne) := mkCore abortView
private def ret0OneCore : CoreInput (sliceLanguage sliceOne) := mkCore ret0OneView

private theorem addOkCore_wf : addOkCore.WF :=
  mkCore_wf addOkView 20 (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide)

private theorem add43Core_wf : add43Core.WF :=
  mkCore_wf add43View 20 (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide)

private theorem ret0Core_wf : ret0Core.WF :=
  mkCore_wf ret0View 20 (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide)

private theorem overflowCore_wf : overflowCore.WF :=
  mkCore_wf overflowView 20 (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide)

private theorem nominalCore_wf : nominalCore.WF :=
  mkCore_wf nominalView 40 (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide)

private theorem nominal8Core_wf : nominal8Core.WF :=
  mkCore_wf nominal8View 20 (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide)

private theorem abortCore_wf : abortCore.WF :=
  mkCore_wf abortView 20 (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide)

private theorem ret0OneCore_wf : ret0OneCore.WF :=
  mkCore_wf ret0OneView 20 (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide) (by decide)

private theorem addOkCore_uncheckedWF : addOkCore.UncheckedWF :=
  mkCore_uncheckedWF addOkView addOkCore_wf (by decide)

private theorem add43Core_uncheckedWF : add43Core.UncheckedWF :=
  mkCore_uncheckedWF add43View add43Core_wf (by decide)

private theorem ret0Core_uncheckedWF : ret0Core.UncheckedWF :=
  mkCore_uncheckedWF ret0View ret0Core_wf (by decide)

private theorem overflowCore_uncheckedWF : overflowCore.UncheckedWF :=
  mkCore_uncheckedWF overflowView overflowCore_wf (by decide)

private theorem nominalCore_uncheckedWF : nominalCore.UncheckedWF :=
  mkCore_uncheckedWF nominalView nominalCore_wf (by decide)

private theorem nominal8Core_uncheckedWF : nominal8Core.UncheckedWF :=
  mkCore_uncheckedWF nominal8View nominal8Core_wf (by decide)

private theorem abortCore_uncheckedWF : abortCore.UncheckedWF :=
  mkCore_uncheckedWF abortView abortCore_wf (by decide)

private theorem ret0OneCore_uncheckedWF : ret0OneCore.UncheckedWF :=
  mkCore_uncheckedWF ret0OneView ret0OneCore_wf (by decide)

private theorem addOk_ready : (AdmissionInput.core addOkCore).Ready slice0 :=
  ⟨rfl, addOkCore_wf, ⟨addOkView, mkCore_represents addOkView⟩⟩

private theorem overflow_ready : (AdmissionInput.core overflowCore).Ready slice0 :=
  ⟨rfl, overflowCore_wf, ⟨overflowView, mkCore_represents overflowView⟩⟩

private theorem nominal_ready : (AdmissionInput.core nominalCore).Ready sliceOne :=
  ⟨rfl, nominalCore_wf, ⟨nominalView, mkCore_represents nominalView⟩⟩

private theorem abort_ready : (AdmissionInput.core abortCore).Ready sliceOne :=
  ⟨rfl, abortCore_wf, ⟨abortView, mkCore_represents abortView⟩⟩

private def addOkDischarges : Discharges (sliceLanguage slice0) :=
  match ConcreteSpec.Admission.checkInput addOkCore with
  | .ok d => d
  | .error _ => []

private theorem checkInput_addOk :
    ConcreteSpec.Admission.checkInput addOkCore = .ok addOkDischarges := by
  unfold addOkDischarges
  cases h : ConcreteSpec.Admission.checkInput addOkCore with
  | ok d => rfl
  | error _ =>
    have hchk : (ConcreteSpec.Admission.checkInput addOkCore).isOk = true := by decide
    rw [h] at hchk; cases hchk

private theorem addOkDischarges_site0_nonempty :
    ∃ d ds, AList.lookup addOkDischarges ⟨0⟩ = some (d :: ds) := by
  have h : (match AList.lookup addOkDischarges ⟨0⟩ with | some (_ :: _) => true | _ => false) = true := by
    decide
  cases hl : AList.lookup addOkDischarges ⟨0⟩ with
  | none => rw [hl] at h; cases h
  | some xs =>
    cases xs with
    | nil => rw [hl] at h; cases h
    | cons d ds => exact ⟨d, ds, rfl⟩

private def addOkAdmitted : Admitted (sliceLanguage slice0) := ⟨addOkCore, addOkDischarges⟩

private theorem admission_run_core {slice : Slice} {profile : Profile}
    (passes : Passes slice profile) {core : CoreInput (sliceLanguage slice)}
    (hslice : core.slice = slice) :
    (registry passes).admission.run (.core core) =
      (passes.checkCore core).map fun discharges => ⟨core, discharges⟩ := by
  change (if core.slice = slice then do
    let prepared ← Except.mapError AdmissionFailure.refused (Except.ok core)
    (passes.checkCore prepared).map fun discharges => Admitted.mk prepared discharges
  else .error (.fragment (.sliceMismatch slice core.slice))) = _
  rw [ite_eq_left hslice]; rfl

private theorem addOkAdmitted_valid : addOkAdmitted.Valid := by
  have hrun : (registry (corePasses slice0 lcbProfile)).admission.run (.core addOkCore) =
      .ok addOkAdmitted := by
    rw [admission_run_core _ rfl]; dsimp only [corePasses]; rw [checkInput_addOk]; rfl
  exact (slice0_lcb_admission_validated.1 (.core addOkCore) addOkAdmitted addOk_ready hrun).1

/-- Partial-discharge admitted input: same `addOkCore`, empty discharge map (`discharges = []`). -/
private def addOkPartialAdmitted : Admitted (sliceLanguage slice0) := ⟨addOkCore, []⟩

private theorem addOkPartialAdmitted_valid : addOkPartialAdmitted.Valid :=
  ⟨addOkAdmitted_valid.1, Concrete.Core.LowerMIR.validDischarges_nil_of_uncheckedWF addOkCore_uncheckedWF⟩

private def nominalDischarges : Discharges (sliceLanguage sliceOne) :=
  match ConcreteSpec.Admission.checkInput nominalCore with
  | .ok d => d
  | .error _ => []

private theorem checkInput_nominal :
    ConcreteSpec.Admission.checkInput nominalCore = .ok nominalDischarges := by
  unfold nominalDischarges
  cases h : ConcreteSpec.Admission.checkInput nominalCore with
  | ok d => rfl
  | error _ =>
    have hchk : (ConcreteSpec.Admission.checkInput nominalCore).isOk = true := by decide
    rw [h] at hchk; cases hchk

private def nominalAdmitted : Admitted (sliceLanguage sliceOne) := ⟨nominalCore, nominalDischarges⟩

private theorem nominalAdmitted_valid : nominalAdmitted.Valid := by
  have hrun : (registry (corePasses sliceOne lcbProfile)).admission.run (.core nominalCore) =
      .ok nominalAdmitted := by
    rw [admission_run_core _ rfl]; dsimp only [corePasses]; rw [checkInput_nominal]; rfl
  exact (sliceOne_lcb_admission_validated.1 (.core nominalCore) nominalAdmitted nominal_ready hrun).1

private def abortDischarges : Discharges (sliceLanguage sliceOne) :=
  match ConcreteSpec.Admission.checkInput abortCore with
  | .ok d => d
  | .error _ => []

private theorem checkInput_abort :
    ConcreteSpec.Admission.checkInput abortCore = .ok abortDischarges := by
  unfold abortDischarges
  cases h : ConcreteSpec.Admission.checkInput abortCore with
  | ok d => rfl
  | error _ =>
    have hchk : (ConcreteSpec.Admission.checkInput abortCore).isOk = true := by decide
    rw [h] at hchk; cases hchk

private def abortAdmitted : Admitted (sliceLanguage sliceOne) := ⟨abortCore, abortDischarges⟩

private theorem abortAdmitted_valid : abortAdmitted.Valid := by
  have hrun : (registry (corePasses sliceOne lcbProfile)).admission.run (.core abortCore) =
      .ok abortAdmitted := by
    rw [admission_run_core _ rfl]; dsimp only [corePasses]; rw [checkInput_abort]; rfl
  exact (sliceOne_lcb_admission_validated.1 (.core abortCore) abortAdmitted abort_ready hrun).1

/-! ### Expected exact observations for each fixture -/

private def overflowExpectedObs : Concrete.Contracts.Observation :=
  .trapped 134 .overflow
    ⟨⟨0⟩, ⟨mainSourcePath.render.text, 1, 19⟩⟩
    (trapDiagnostic fixtureProject .overflow ⟨0⟩ (surfaceFileSpan spanMain))

private def abortExpectedObs : Concrete.Contracts.Observation :=
  .trapped 134 .abort
    ⟨⟨0⟩, ⟨mainSourcePath.render.text, 1, 40⟩⟩
    (trapDiagnostic fixtureProject .abort ⟨0⟩ (surfaceFileSpan spanRet))

private theorem addOk_observable :
    IntegratedCore.Agreement.Observable addOkCore addOkView.program
      (ConcreteSpec.run addOkView.program (addOkView.program.body.length + 1))
      (.returned 42 ByteArray.empty) := by
  rfl

private theorem add43_observable :
    IntegratedCore.Agreement.Observable add43Core add43View.program
      (ConcreteSpec.run add43View.program (add43View.program.body.length + 1))
      (.returned 43 ByteArray.empty) := by
  rfl

private theorem ret0_observable :
    IntegratedCore.Agreement.Observable ret0Core ret0View.program
      (ConcreteSpec.run ret0View.program (ret0View.program.body.length + 1))
      (.returned 0 ByteArray.empty) := by
  rfl

private theorem overflow_observable :
    IntegratedCore.Agreement.Observable overflowCore overflowView.program
      (ConcreteSpec.run overflowView.program (overflowView.program.body.length + 1))
      overflowExpectedObs := by
  rfl

private theorem nominal_observable :
    IntegratedCore.Agreement.Observable nominalCore nominalView.program
      (ConcreteSpec.run nominalView.program (nominalView.program.body.length + 1))
      (.returned 7 ByteArray.empty) := by
  rfl

private theorem nominal8_observable :
    IntegratedCore.Agreement.Observable nominal8Core nominal8View.program
      (ConcreteSpec.run nominal8View.program (nominal8View.program.body.length + 1))
      (.returned 8 ByteArray.empty) := by
  rfl

private theorem abort_observable :
    IntegratedCore.Agreement.Observable abortCore abortView.program
      (ConcreteSpec.run abortView.program (abortView.program.body.length + 1))
      abortExpectedObs := by
  rfl

private theorem ret0One_observable :
    IntegratedCore.Agreement.Observable ret0OneCore ret0OneView.program
      (ConcreteSpec.run ret0OneView.program (ret0OneView.program.body.length + 1))
      (.returned 0 ByteArray.empty) := by
  rfl

/-! ### 2. Reachable whole-program witnesses (D5, `ISS-0113`, `ISS-0118`) -/

/-- Helper: concrete MIR emitted for `addOkCore` for a given check entry. -/
private def addOkMirCheck (profile : Profile) (chk : OperationCheck) :
    MIR (sliceLoweringLanguage slice0) profile :=
  let orig0 : Origin := match addOkView.located.bindingOrigins with
    | (_, o) :: _ => o
    | [] => addOkView.located.endingOrigin.2
  let origEnd : Origin := addOkView.located.endingOrigin.2
  {
    declarations := []
    project := fixtureProject
    entry := earlyEntry
    signatures := [(earlyEntry, ⟨[], some Concrete.Core.LowerMIR.byteTy⟩)]
    functions := [(earlyEntry, ([(⟨0⟩, Concrete.Core.LowerMIR.byteTy)], {
      entry := ⟨0⟩
      blocks := #[{
        id := ⟨0⟩
        params := []
        body := [⟨orig0, .op (opcode := .arithmetic (.plain .add)) (ty := Concrete.Core.LowerMIR.byteTy)
          ⟨⟨0⟩, []⟩ () (.cons (Concrete.Core.LowerMIR.lowerAtom (.lit 20))
            (.cons (Concrete.Core.LowerMIR.lowerAtom (.lit 22)) .nil))
          [chk] []⟩]
        ending := .ret (some ⟨Concrete.Core.LowerMIR.byteTy, Concrete.Core.LowerMIR.lowerAtom (.var ⟨0⟩)⟩)
        boundaryOrigins := [origEnd] }] }))]
    mutableLocals := []
  }

private abbrev addOkMir (profile : Profile) (disp : CheckDisposition) :
    MIR (sliceLoweringLanguage slice0) profile :=
  addOkMirCheck profile ⟨0, .overflow, disp⟩

private def overflowMir (profile : Profile) (disp : CheckDisposition) :
    MIR (sliceLoweringLanguage slice0) profile :=
  let orig0 : Origin := match overflowView.located.bindingOrigins with
    | (_, o) :: _ => o
    | [] => overflowView.located.endingOrigin.2
  let origEnd : Origin := overflowView.located.endingOrigin.2
  {
    declarations := []
    project := fixtureProject
    entry := earlyEntry
    signatures := [(earlyEntry, ⟨[], some Concrete.Core.LowerMIR.byteTy⟩)]
    functions := [(earlyEntry, ([(⟨0⟩, Concrete.Core.LowerMIR.byteTy)], {
      entry := ⟨0⟩
      blocks := #[{
        id := ⟨0⟩
        params := []
        body := [⟨orig0, .op (opcode := .arithmetic (.plain .add)) (ty := Concrete.Core.LowerMIR.byteTy)
          ⟨⟨0⟩, []⟩ () (.cons (Concrete.Core.LowerMIR.lowerAtom (.lit 255))
            (.cons (Concrete.Core.LowerMIR.lowerAtom (.lit 1)) .nil))
          [⟨0, .overflow, disp⟩] []⟩]
        ending := .ret (some ⟨Concrete.Core.LowerMIR.byteTy, Concrete.Core.LowerMIR.lowerAtom (.var ⟨0⟩)⟩)
        boundaryOrigins := [origEnd] }] }))]
    mutableLocals := []
  }

private theorem lowerCore_addOkAdmitted_eq (profile : Profile) :
    Concrete.Core.LowerMIR.lowerCore (profile := profile) addOkAdmitted =
      .ok (addOkMir profile .discharged) := by
  obtain ⟨_, _, hlu⟩ := addOkDischarges_site0_nonempty
  have hchk : Concrete.Core.LowerMIR.addCheck addOkDischarges ⟨0⟩ = ⟨0, .overflow, .discharged⟩ := by
    simp [Concrete.Core.LowerMIR.addCheck, hlu]
  have heq : Concrete.Core.LowerMIR.lowerCore (profile := profile) addOkAdmitted =
      .ok (addOkMirCheck profile (Concrete.Core.LowerMIR.addCheck addOkDischarges ⟨0⟩)) := rfl
  rw [heq, hchk]

private theorem lowerUnchecked_addOk_eq (profile : Profile) :
    Concrete.Core.LowerMIR.lowerUnchecked (profile := profile) addOkCore =
      .ok (addOkMir profile .residual) := rfl

private theorem lowerUnchecked_overflow_eq (profile : Profile) :
    Concrete.Core.LowerMIR.lowerUnchecked (profile := profile) overflowCore =
      .ok (overflowMir profile .residual) := rfl

/-- D5 witness (checked Slice 0 `admission` → `lowerCore`): `20 + 22` is admitted with an
`AdditionFits` discharge, lowers to a `MIR.HandoffWF` module satisfying `LowersCore`
with `.discharged` check disposition, and observes exit status `42`. -/
theorem witness_slice0_checked_add (profile : Profile) :
    ∃ mir : MIR (sliceLoweringLanguage slice0) profile,
      (registry (corePasses slice0 profile)).admission.run (.core addOkCore) = .ok addOkAdmitted ∧
      (registry (corePasses slice0 profile)).lowerCore.run addOkAdmitted = .ok mir ∧
      MIR.HandoffWF (language := sliceLanguage slice0) mir ∧
      LowersCore (language := sliceLanguage slice0) addOkAdmitted mir ∧
      (∃ op ∈ mir.operations, op.origin.operation = ⟨0⟩ ∧ op.checks = [⟨0, .overflow, .discharged⟩]) ∧
      (MIR.Execution (language := sliceLanguage slice0)).observes mir (.returned 42 ByteArray.empty) := by
  have hadm : (registry (corePasses slice0 profile)).admission.run (.core addOkCore) = .ok addOkAdmitted := by
    rw [admission_run_core _ rfl]; dsimp only [corePasses]; rw [checkInput_addOk]; rfl
  obtain ⟨mir, hok, hwf, hlow⟩ := Concrete.Core.LowerMIR.lowerCore_spec (profile := profile) addOkAdmitted_valid
  have hmirEq : mir = addOkMir profile .discharged :=
    Except.ok.inj (hok.symm.trans (lowerCore_addOkAdmitted_eq profile))
  have hopDisch : ∃ op ∈ mir.operations, op.origin.operation = ⟨0⟩ ∧ op.checks = [⟨0, .overflow, .discharged⟩] := by
    rw [hmirEq]
    exact ⟨_, List.mem_cons_self, rfl, rfl⟩
  have hobs := mir_observes_exact addOkCore_uncheckedWF addOkAdmitted_valid.2 hok addOk_observable
  exact ⟨mir, hadm, hok, hwf, hlow, hopDisch, hobs⟩

/-- D5 witness (checked Slice 0 partial discharge): `20 + 22` with `discharges = []`
still satisfies `Admitted.Valid`, lowers to a `MIR.HandoffWF` module satisfying
`LowersCore` with `.residual` check disposition, and observes exit status `42`. -/
theorem witness_slice0_partial_discharge (profile : Profile) :
    ∃ mir : MIR (sliceLoweringLanguage slice0) profile,
      (registry (corePasses slice0 profile)).lowerCore.run addOkPartialAdmitted = .ok mir ∧
      MIR.HandoffWF (language := sliceLanguage slice0) mir ∧
      LowersCore (language := sliceLanguage slice0) addOkPartialAdmitted mir ∧
      (∃ op ∈ mir.operations, op.origin.operation = ⟨0⟩ ∧ op.checks = [⟨0, .overflow, .residual⟩]) ∧
      (MIR.Execution (language := sliceLanguage slice0)).observes mir (.returned 42 ByteArray.empty) := by
  obtain ⟨mir, hok, hwf, hlow⟩ := Concrete.Core.LowerMIR.lowerCore_spec (profile := profile) addOkPartialAdmitted_valid
  have hmirEq : mir = addOkMir profile .residual :=
    Except.ok.inj (hok.symm.trans (lowerUnchecked_addOk_eq profile))
  have hopRes : ∃ op ∈ mir.operations, op.origin.operation = ⟨0⟩ ∧ op.checks = [⟨0, .overflow, .residual⟩] := by
    rw [hmirEq]
    exact ⟨_, List.mem_cons_self, rfl, rfl⟩
  have hobs := mir_observes_exact addOkCore_uncheckedWF addOkPartialAdmitted_valid.2 hok addOk_observable
  exact ⟨mir, hok, hwf, hlow, hopRes, hobs⟩

/-- D5 witness (unchecked Slice 0 normal addition): `20 + 22` passes `typingEntry`,
lowers via `lowerUnchecked` keeping its check `.residual`, and observes exit status `42`. -/
theorem witness_slice0_unchecked_add (profile : Profile) :
    (typingEntry (sliceLanguage slice0) slice0).run addOkCore = .ok addOkCore ∧
    ∃ mir : MIR (sliceLoweringLanguage slice0) profile,
      (registry (corePasses slice0 profile)).lowerUnchecked.run addOkCore = .ok mir ∧
      MIR.HandoffWF (language := sliceLanguage slice0) mir ∧
      LowersUnchecked (language := sliceLanguage slice0) addOkCore mir ∧
      (∃ op ∈ mir.operations, op.origin.operation = ⟨0⟩ ∧ op.checks = [⟨0, .overflow, .residual⟩]) ∧
      (MIR.Execution (language := sliceLanguage slice0)).observes mir (.returned 42 ByteArray.empty) := by
  have htyp : (typingEntry (sliceLanguage slice0) slice0).run addOkCore = .ok addOkCore := rfl
  obtain ⟨mir, hok, hwf, hlow⟩ := Concrete.Core.LowerMIR.lowerUnchecked_spec (profile := profile) addOkCore_uncheckedWF
  have hmirEq : mir = addOkMir profile .residual :=
    Except.ok.inj (hok.symm.trans (lowerUnchecked_addOk_eq profile))
  have hopRes : ∃ op ∈ mir.operations, op.origin.operation = ⟨0⟩ ∧ op.checks = [⟨0, .overflow, .residual⟩] := by
    rw [hmirEq]
    exact ⟨_, List.mem_cons_self, rfl, rfl⟩
  have hobs := mir_observes_exact addOkCore_uncheckedWF
    (Concrete.Core.LowerMIR.validDischarges_nil_of_uncheckedWF addOkCore_uncheckedWF) hok addOk_observable
  exact ⟨htyp, mir, hok, hwf, hlow, hopRes, hobs⟩

/-- D5 + `ISS-0118` witness (observable residual overflow on unchecked route, refused on checked route):
`255 + 1` is refused by checked `admission` (satisfying `CheckerRefuses`), accepted by
unchecked `typingEntry`, lowers via `lowerUnchecked` to a `MIR.HandoffWF` module satisfying
`LowersUnchecked` with `.residual` check, and observes exact `.trapped 134 .overflow` with
exact `DiagnosticSite` and `trapDiagnostic` stderr bytes. -/
theorem witness_slice0_unchecked_overflow (profile : Profile) :
    (∃ diagnostic, (registry (corePasses slice0 profile)).admission.run (.core overflowCore) =
        .error (.refused diagnostic) ∧
      (AdmissionInput.core overflowCore).CheckerRefuses diagnostic) ∧
    (typingEntry (sliceLanguage slice0) slice0).run overflowCore = .ok overflowCore ∧
    ∃ mir : MIR (sliceLoweringLanguage slice0) profile,
      (registry (corePasses slice0 profile)).lowerUnchecked.run overflowCore = .ok mir ∧
      MIR.HandoffWF (language := sliceLanguage slice0) mir ∧
      LowersUnchecked (language := sliceLanguage slice0) overflowCore mir ∧
      (∃ op ∈ mir.operations, op.origin.operation = ⟨0⟩ ∧ op.checks = [⟨0, .overflow, .residual⟩]) ∧
      (MIR.Execution (language := sliceLanguage slice0)).observes mir overflowExpectedObs := by
  have href : ∃ diagnostic,
      (registry (corePasses slice0 profile)).admission.run (.core overflowCore) =
        .error (.refused diagnostic) ∧
      (AdmissionInput.core overflowCore).CheckerRefuses diagnostic := by
    cases hrun : (registry (corePasses slice0 profile)).admission.run (.core overflowCore) with
    | ok _ =>
      have herr : (ConcreteSpec.Admission.checkInput overflowCore).isOk = false := by decide
      rw [admission_run_core _ rfl] at hrun
      dsimp only [corePasses] at hrun
      cases hc : ConcreteSpec.Admission.checkInput overflowCore with
      | ok _ => rw [hc] at herr; cases herr
      | error _ => rw [hc] at hrun; cases hrun
    | error failure =>
      have hv := (IntegratedCore.admission_validated (corePasses slice0 profile) rfl).2
        (.core overflowCore) failure overflow_ready hrun
      cases failure with
      | refused diagnostic => exact ⟨diagnostic, rfl, hv.2⟩
      | fragment br =>
        cases br with
        | sliceMismatch _ _ => exact False.elim (hv.2.2 (hv.2.1.trans hv.1.symm))
        | _ => exact False.elim hv
      | _ => exact False.elim hv
  have htyp : (typingEntry (sliceLanguage slice0) slice0).run overflowCore = .ok overflowCore := rfl
  obtain ⟨mir, hok, hwf, hlow⟩ := Concrete.Core.LowerMIR.lowerUnchecked_spec (profile := profile) overflowCore_uncheckedWF
  have hmirEq : mir = overflowMir profile .residual :=
    Except.ok.inj (hok.symm.trans (lowerUnchecked_overflow_eq profile))
  have hopRes : ∃ op ∈ mir.operations, op.origin.operation = ⟨0⟩ ∧ op.checks = [⟨0, .overflow, .residual⟩] := by
    rw [hmirEq]
    exact ⟨_, List.mem_cons_self, rfl, rfl⟩
  have hobs := mir_observes_exact overflowCore_uncheckedWF
    (Concrete.Core.LowerMIR.validDischarges_nil_of_uncheckedWF overflowCore_uncheckedWF) hok overflow_observable
  exact ⟨href, htyp, mir, hok, hwf, hlow, hopRes, hobs⟩

/-- D5 + `ISS-0118` witness (Slice 1 empty-linear nominal construct, move, destructure,
and dead `let Token {} = Token {};` boundary origin preservation on both routes). -/
theorem witness_slice1_nominal (profile : Profile) :
    ∃ mir : MIR (sliceLoweringLanguage sliceOne) profile,
      (registry (corePasses sliceOne profile)).admission.run (.core nominalCore) = .ok nominalAdmitted ∧
      (registry (corePasses sliceOne profile)).lowerCore.run nominalAdmitted = .ok mir ∧
      (registry (corePasses sliceOne profile)).lowerUnchecked.run nominalCore = .ok mir ∧
      MIR.HandoffWF (language := sliceLanguage sliceOne) mir ∧
      LowersCore (language := sliceLanguage sliceOne) nominalAdmitted mir ∧
      LowersUnchecked (language := sliceLanguage sliceOne) nominalCore mir ∧
      mir.declarations.emptyLinear (Concrete.Core.LowerMIR.nominalId tokenName) = true ∧
      (MIR.Execution (language := sliceLanguage sliceOne)).observes mir (.returned 7 ByteArray.empty) := by
  have hadm : (registry (corePasses sliceOne profile)).admission.run (.core nominalCore) = .ok nominalAdmitted := by
    rw [admission_run_core _ rfl]; dsimp only [corePasses]; rw [checkInput_nominal]; rfl
  obtain ⟨mir, hokC, hwf, hlowC⟩ := Concrete.Core.LowerMIR.lowerCore_spec (profile := profile) nominalAdmitted_valid
  obtain ⟨mirU, hokU, _, hlowU⟩ := Concrete.Core.LowerMIR.lowerUnchecked_spec (profile := profile) nominalCore_uncheckedWF
  have hdischNil : nominalDischarges = [] := by decide
  have hsame : mirU = mir := by
    have h1 : Concrete.Core.LowerMIR.lowerWith (profile := profile) nominalCore [] = .ok mir := by
      simpa [Concrete.Core.LowerMIR.lowerCore, nominalAdmitted, hdischNil] using hokC
    exact Except.ok.inj (hokU.symm.trans h1)
  subst mirU
  have hemp : mir.declarations.emptyLinear (Concrete.Core.LowerMIR.nominalId tokenName) = true := by
    have hdecls : mir.declarations = nominalCore.machineProgram.declarations := hlowC.2.2.2.2.1.2
    rw [hdecls]; decide
  have hobs := mir_observes_exact nominalCore_uncheckedWF nominalAdmitted_valid.2 hokC nominal_observable
  exact ⟨mir, hadm, hokC, hokU, hwf, hlowC, hlowU, hemp, hobs⟩

/-- D5 + `ISS-0118` witness (Slice 1 explicit `abort` on both checked and unchecked routes). -/
theorem witness_slice1_abort (profile : Profile) :
    ∃ mir : MIR (sliceLoweringLanguage sliceOne) profile,
      (registry (corePasses sliceOne profile)).admission.run (.core abortCore) = .ok abortAdmitted ∧
      (registry (corePasses sliceOne profile)).lowerCore.run abortAdmitted = .ok mir ∧
      (registry (corePasses sliceOne profile)).lowerUnchecked.run abortCore = .ok mir ∧
      MIR.HandoffWF (language := sliceLanguage sliceOne) mir ∧
      LowersCore (language := sliceLanguage sliceOne) abortAdmitted mir ∧
      LowersUnchecked (language := sliceLanguage sliceOne) abortCore mir ∧
      (MIR.Execution (language := sliceLanguage sliceOne)).observes mir abortExpectedObs := by
  have hadm : (registry (corePasses sliceOne profile)).admission.run (.core abortCore) = .ok abortAdmitted := by
    rw [admission_run_core _ rfl]; dsimp only [corePasses]; rw [checkInput_abort]; rfl
  obtain ⟨mir, hokC, hwf, hlowC⟩ := Concrete.Core.LowerMIR.lowerCore_spec (profile := profile) abortAdmitted_valid
  obtain ⟨mirU, hokU, _, hlowU⟩ := Concrete.Core.LowerMIR.lowerUnchecked_spec (profile := profile) abortCore_uncheckedWF
  have hdischNil : abortDischarges = [] := by decide
  have hsame : mirU = mir := by
    have h1 : Concrete.Core.LowerMIR.lowerWith (profile := profile) abortCore [] = .ok mir := by
      simpa [Concrete.Core.LowerMIR.lowerCore, abortAdmitted, hdischNil] using hokC
    exact Except.ok.inj (hokU.symm.trans h1)
  subst mirU
  have hobs := mir_observes_exact abortCore_uncheckedWF abortAdmitted_valid.2 hokC abort_observable
  exact ⟨mir, hadm, hokC, hokU, hwf, hlowC, hlowU, hobs⟩

/-! ### 3. Wrong-compiler mutants rejected by frozen contracts (D5, `ISS-0113`, `ISS-0118`) -/

/-- Helper: rewrite all `.residual` checks in a MIR module to `.discharged` (dropped-check mutant). -/
private def stripChecksInst {slice : Slice} :
    MIRInst (sliceLoweringLanguage slice) → MIRInst (sliceLoweringLanguage slice)
  | .op dst hasOp args checks addrs =>
    .op dst hasOp args (checks.map fun c => { c with disposition := .discharged }) addrs
  | inst => inst

private def stripChecksMir {slice : Slice} {profile : Profile}
    (mir : MIR (sliceLoweringLanguage slice) profile) : MIR (sliceLoweringLanguage slice) profile :=
  { mir with
    functions := mir.functions.map fun (locals, graph) =>
      (locals, { graph with
        blocks := ⟨graph.blocks.toList.map fun blk =>
          { blk with body := blk.body.map fun op => ⟨op.origin, stripChecksInst op.instruction⟩ }⟩ }) }

/-- Mutant 1 (`lowerCore`, Slice 0): alters return value (`42` → `43`) on `addOkAdmitted`.
Rejected by `BehaviorRefinement` in `LowersCore`. -/
theorem mutant_lowerCore_wrong_return (profile : Profile) :
    let mutant : Passes slice0 profile :=
      { corePasses slice0 profile with
        lowerCore := fun _ => IntegratedCore.lowerUnchecked add43Core }
    ¬ (registry mutant).lowerCore.Validated := by
  intro mutant hval
  obtain ⟨mir43, hok43, _, _⟩ := Concrete.Core.LowerMIR.lowerUnchecked_spec (profile := profile) add43Core_uncheckedWF
  have hobs43 : (MIR.Execution (language := sliceLanguage slice0)).observes mir43
      (.returned 43 ByteArray.empty) :=
    mir_observes_exact add43Core_uncheckedWF
      (Concrete.Core.LowerMIR.validDischarges_nil_of_uncheckedWF add43Core_uncheckedWF) hok43 add43_observable
  have hlowMut : LowersCore (language := sliceLanguage slice0) addOkAdmitted mir43 :=
    (hval.1 addOkAdmitted mir43 addOkAdmitted_valid hok43).2
  obtain ⟨hsafe, coreObs, hcoreObsable, hcoreIff⟩ :=
    IntegratedCore.Agreement.unchecked_agreement addOkCore_uncheckedWF addOkView (mkCore_represents addOkView)
  have hcore43 := core_observes_of_lowersCore hsafe hlowMut hobs43
  have h42 : coreObs = .returned 42 ByteArray.empty := hcoreObsable
  have h43eq42 : (Concrete.Contracts.Observation.returned 43 ByteArray.empty) =
      .returned 42 ByteArray.empty :=
    ((hcoreIff _).mp hcore43).trans h42
  cases h43eq42

/-- Mutant 2 (`lowerCore`, Slice 0): emits a spurious `.discharged` check when `discharges = []`.
Rejected by `ConsumesEvidence` in `LowersCore`. -/
theorem mutant_lowerCore_spurious_discharge (profile : Profile) :
    let mutant : Passes slice0 profile :=
      { corePasses slice0 profile with
        lowerCore := fun input => (IntegratedCore.lowerCore input).map stripChecksMir }
    ¬ (registry mutant).lowerCore.Validated := by
  intro mutant hval
  obtain ⟨mir, hok, _, _⟩ := Concrete.Core.LowerMIR.lowerCore_spec (profile := profile) addOkPartialAdmitted_valid
  have hmirEq : mir = addOkMir profile .residual :=
    Except.ok.inj (hok.symm.trans (lowerUnchecked_addOk_eq profile))
  subst mir
  have hrunMut : (registry mutant).lowerCore.run addOkPartialAdmitted =
      .ok (stripChecksMir (addOkMir profile .residual)) := by
    change (Concrete.Core.LowerMIR.lowerCore addOkPartialAdmitted).map stripChecksMir = _
    rw [hok]; rfl
  have hlowMut : LowersCore (language := sliceLanguage slice0) addOkPartialAdmitted
      (stripChecksMir (addOkMir profile .residual)) :=
    (hval.1 addOkPartialAdmitted _ addOkPartialAdmitted_valid hrunMut).2
  have hopDisch : ∃ op ∈ (stripChecksMir (addOkMir profile .residual)).operations,
      ⟨0, .overflow, .discharged⟩ ∈ op.checks := by
    change ∃ op ∈ (addOkMir profile .discharged).operations, _
    exact ⟨_, List.mem_cons_self, List.mem_cons_self⟩
  obtain ⟨op, hopMem, hchkMem⟩ := hopDisch
  obtain ⟨_, _, _, _, _, hdischJust⟩ := hlowMut.2.2.2.1.2.2.1 op hopMem
  obtain ⟨d, hdMem, _⟩ := hdischJust ⟨0, .overflow, .discharged⟩ hchkMem rfl
  obtain ⟨_, hgrp, _⟩ := hdMem
  cases hgrp

/-- Mutant 3 (`lowerCore`, Slice 0): ignores supplied discharge and leaves check `.residual`.
Rejected by `ConsumesEvidence` in `LowersCore`. -/
theorem mutant_lowerCore_unconsumed_discharge (profile : Profile) :
    let mutant : Passes slice0 profile :=
      { corePasses slice0 profile with
        lowerCore := fun input => IntegratedCore.lowerUnchecked input.core }
    ¬ (registry mutant).lowerCore.Validated := by
  intro mutant hval
  obtain ⟨mir, hok, _, _⟩ := Concrete.Core.LowerMIR.lowerUnchecked_spec (profile := profile) addOkCore_uncheckedWF
  have hmirEq : mir = addOkMir profile .residual :=
    Except.ok.inj (hok.symm.trans (lowerUnchecked_addOk_eq profile))
  subst mir
  have hlowMut : LowersCore (language := sliceLanguage slice0) addOkAdmitted (addOkMir profile .residual) :=
    (hval.1 addOkAdmitted _ addOkAdmitted_valid hok).2
  obtain ⟨d, ds, hlu⟩ := addOkDischarges_site0_nonempty
  have hmemDisch : addOkAdmitted.discharges.Contains ⟨0⟩ d := by
    refine ⟨d :: ds, ?_, List.mem_cons_self⟩
    have hnd := addOkAdmitted_valid.2.2.1
    change (⟨0⟩, d :: ds) ∈ addOkDischarges
    have hmemOfLu : ∀ (xs : Discharges (sliceLanguage slice0)),
        (List.map Prod.fst xs).Nodup → AList.lookup xs ⟨0⟩ = some (d :: ds) →
        (⟨0⟩, d :: ds) ∈ xs := by
      intro xs hnd' hlu'
      induction xs with
      | nil => cases hlu'
      | cons hd tl ih =>
        rcases hd with ⟨k, v⟩
        simp only [AList.lookup] at hlu'
        split at hlu'
        · rename_i heq
          cases Option.some.inj hlu'; subst heq; exact List.mem_cons_self
        · exact List.mem_cons_of_mem _ (ih (List.nodup_cons.mp hnd').2 hlu')
    exact hmemOfLu addOkDischarges hnd hlu
  obtain ⟨op, hopMem, chk, hchkMem, hjust, _⟩ := hlowMut.2.2.2.1.2.2.2.1 ⟨0⟩ d hmemDisch
  rcases List.mem_singleton.mp (show op ∈ [_] from hopMem) with rfl
  rcases List.mem_singleton.mp (show chk ∈ [_] from hchkMem) with rfl
  exact absurd hjust.1 (by decide)

/-- Mutant 4 (`lowerCore`, Slice 0): spuriously returns `.error` on valid `Admitted`.
Rejected by `lowerCore.refusal := False`. -/
theorem mutant_lowerCore_spurious_error (profile : Profile) :
    let mutant : Passes slice0 profile :=
      { corePasses slice0 profile with
        lowerCore := fun _ => .error stubFailure }
    ¬ (registry mutant).lowerCore.Validated := by
  intro mutant hval
  exact hval.2 addOkAdmitted stubFailure addOkAdmitted_valid rfl

/-- Mutant 5 (`lowerUnchecked`, Slice 0): drops the residual overflow check on `255 + 1`
by flipping its disposition to `.discharged`. Rejected by `ConsumesEvidence` in `LowersUnchecked`. -/
theorem mutant_lowerUnchecked_drop_overflow_check (profile : Profile) :
    let mutant : Passes slice0 profile :=
      { corePasses slice0 profile with
        lowerUnchecked := fun input => (IntegratedCore.lowerUnchecked input).map stripChecksMir }
    ¬ (registry mutant).lowerUnchecked.Validated := by
  intro mutant hval
  obtain ⟨mir, hok, _, _⟩ := Concrete.Core.LowerMIR.lowerUnchecked_spec (profile := profile) overflowCore_uncheckedWF
  have hmirEq : mir = overflowMir profile .residual :=
    Except.ok.inj (hok.symm.trans (lowerUnchecked_overflow_eq profile))
  subst mir
  have hrunMut : (registry mutant).lowerUnchecked.run overflowCore =
      .ok (stripChecksMir (overflowMir profile .residual)) := by
    change (Concrete.Core.LowerMIR.lowerUnchecked overflowCore).map stripChecksMir = _
    rw [hok]; rfl
  have hlowMut : LowersUnchecked (language := sliceLanguage slice0) overflowCore
      (stripChecksMir (overflowMir profile .residual)) :=
    (hval.1 overflowCore _ overflowCore_uncheckedWF hrunMut).2
  have hopDisch : ∃ op ∈ (stripChecksMir (overflowMir profile .residual)).operations,
      ⟨0, .overflow, .discharged⟩ ∈ op.checks := by
    change ∃ op ∈ (overflowMir profile .discharged).operations, _
    exact ⟨_, List.mem_cons_self, List.mem_cons_self⟩
  obtain ⟨op, hopMem, hchkMem⟩ := hopDisch
  obtain ⟨_, _, _, _, _, hdischJust⟩ := hlowMut.2.2.2.2.1.2.2.1 op hopMem
  obtain ⟨d, hdMem, _⟩ := hdischJust ⟨0, .overflow, .discharged⟩ hchkMem rfl
  obtain ⟨_, hgrp, _⟩ := hdMem
  cases hgrp

/-- Mutant 6 (`lowerUnchecked`, Slice 0): suppresses the runtime overflow trap on `255 + 1`
and emits `return 0;` instead. Rejected by `BehaviorRefinement` in `LowersUnchecked`. -/
theorem mutant_lowerUnchecked_suppress_overflow_trap (profile : Profile) :
    let mutant : Passes slice0 profile :=
      { corePasses slice0 profile with
        lowerUnchecked := fun _ => IntegratedCore.lowerUnchecked ret0Core }
    ¬ (registry mutant).lowerUnchecked.Validated := by
  intro mutant hval
  obtain ⟨mir0, hok0, _, _⟩ := Concrete.Core.LowerMIR.lowerUnchecked_spec (profile := profile) ret0Core_uncheckedWF
  have hobs0 : (MIR.Execution (language := sliceLanguage slice0)).observes mir0
      (.returned 0 ByteArray.empty) :=
    mir_observes_exact ret0Core_uncheckedWF
      (Concrete.Core.LowerMIR.validDischarges_nil_of_uncheckedWF ret0Core_uncheckedWF) hok0 ret0_observable
  have hlowMut : LowersUnchecked (language := sliceLanguage slice0) overflowCore mir0 :=
    (hval.1 overflowCore mir0 overflowCore_uncheckedWF hok0).2
  obtain ⟨hsafe, coreObs, hcoreObsable, hcoreIff⟩ :=
    IntegratedCore.Agreement.unchecked_agreement overflowCore_uncheckedWF overflowView (mkCore_represents overflowView)
  have hcore0 := core_observes_of_lowersCore hsafe hlowMut.2 hobs0
  have htrap : coreObs = overflowExpectedObs :=
    Option.some.inj (hcoreObsable.symm.trans overflow_observable)
  have heq : (Concrete.Contracts.Observation.returned 0 ByteArray.empty) = overflowExpectedObs :=
    ((hcoreIff _).mp hcore0).trans htrap
  cases heq

/-- Mutant 7 (`lowerUnchecked`, Slice 0): alters return value (`42` → `43`) on `addOkCore`.
Rejected by `BehaviorRefinement` in `LowersUnchecked`. -/
theorem mutant_lowerUnchecked_wrong_return (profile : Profile) :
    let mutant : Passes slice0 profile :=
      { corePasses slice0 profile with
        lowerUnchecked := fun _ => IntegratedCore.lowerUnchecked add43Core }
    ¬ (registry mutant).lowerUnchecked.Validated := by
  intro mutant hval
  obtain ⟨mir43, hok43, _, _⟩ := Concrete.Core.LowerMIR.lowerUnchecked_spec (profile := profile) add43Core_uncheckedWF
  have hobs43 : (MIR.Execution (language := sliceLanguage slice0)).observes mir43
      (.returned 43 ByteArray.empty) :=
    mir_observes_exact add43Core_uncheckedWF
      (Concrete.Core.LowerMIR.validDischarges_nil_of_uncheckedWF add43Core_uncheckedWF) hok43 add43_observable
  have hlowMut : LowersUnchecked (language := sliceLanguage slice0) addOkCore mir43 :=
    (hval.1 addOkCore mir43 addOkCore_uncheckedWF hok43).2
  obtain ⟨hsafe, coreObs, hcoreObsable, hcoreIff⟩ :=
    IntegratedCore.Agreement.unchecked_agreement addOkCore_uncheckedWF addOkView (mkCore_represents addOkView)
  have hcore43 := core_observes_of_lowersCore hsafe hlowMut.2 hobs43
  have h42 : coreObs = .returned 42 ByteArray.empty := hcoreObsable
  have h43eq42 : (Concrete.Contracts.Observation.returned 43 ByteArray.empty) =
      .returned 42 ByteArray.empty :=
    ((hcoreIff _).mp hcore43).trans h42
  cases h43eq42

/-- Mutant 8 (`lowerUnchecked`, Slice 0): spuriously returns `.error` on valid `UncheckedWF`. -/
theorem mutant_lowerUnchecked_spurious_error (profile : Profile) :
    let mutant : Passes slice0 profile :=
      { corePasses slice0 profile with
        lowerUnchecked := fun _ => .error stubFailure }
    ¬ (registry mutant).lowerUnchecked.Validated := by
  intro mutant hval
  exact hval.2 overflowCore stubFailure overflowCore_uncheckedWF rfl

/-- Mutant 9 (`admission`, Slice 0): accepts overflowing `255 + 1` with empty discharges.
Rejected by `Admitted.Valid` (`CoreInput.Accepted`). -/
theorem mutant_admission_accepts_overflow (profile : Profile) :
    let mutant : Passes slice0 profile :=
      { corePasses slice0 profile with
        checkCore := fun _ => .ok [] }
    ¬ (registry mutant).admission.Validated := by
  intro mutant hval
  have hrun : (registry mutant).admission.run (.core overflowCore) = .ok ⟨overflowCore, []⟩ := by
    rw [admission_run_core _ rfl]; rfl
  have hacc : overflowCore.Accepted :=
    (hval.1 (.core overflowCore) ⟨overflowCore, []⟩ overflow_ready hrun).1.1
  have hsound := IntegratedCore.Agreement.admissionSound (language := sliceLanguage slice0)
  have hiff := (hsound overflowCore overflowCore_wf ⟨overflowView, mkCore_represents overflowView⟩).1
  obtain ⟨_, hok⟩ := hiff.mp hacc
  have herr : (ConcreteSpec.Admission.checkInput overflowCore).isOk = false := by decide
  rw [hok] at herr
  cases herr

/-- Mutant 10 (`lowerCore`, Slice 1): alters nominal exit value (`7` → `8`) on `nominalAdmitted`. -/
theorem mutant_lowerCore_slice1_wrong_return (profile : Profile) :
    let mutant : Passes sliceOne profile :=
      { corePasses sliceOne profile with
        lowerCore := fun _ => IntegratedCore.lowerUnchecked nominal8Core }
    ¬ (registry mutant).lowerCore.Validated := by
  intro mutant hval
  obtain ⟨mir8, hok8, _, _⟩ := Concrete.Core.LowerMIR.lowerUnchecked_spec (profile := profile) nominal8Core_uncheckedWF
  have hobs8 : (MIR.Execution (language := sliceLanguage sliceOne)).observes mir8
      (.returned 8 ByteArray.empty) :=
    mir_observes_exact nominal8Core_uncheckedWF
      (Concrete.Core.LowerMIR.validDischarges_nil_of_uncheckedWF nominal8Core_uncheckedWF) hok8 nominal8_observable
  have hlowMut : LowersCore (language := sliceLanguage sliceOne) nominalAdmitted mir8 :=
    (hval.1 nominalAdmitted mir8 nominalAdmitted_valid hok8).2
  obtain ⟨hsafe, coreObs, hcoreObsable, hcoreIff⟩ :=
    IntegratedCore.Agreement.unchecked_agreement nominalCore_uncheckedWF nominalView (mkCore_represents nominalView)
  have hcore8 := core_observes_of_lowersCore hsafe hlowMut hobs8
  have h7 : coreObs = .returned 7 ByteArray.empty := hcoreObsable
  have h8eq7 : (Concrete.Contracts.Observation.returned 8 ByteArray.empty) =
      .returned 7 ByteArray.empty :=
    ((hcoreIff _).mp hcore8).trans h7
  cases h8eq7

/-- Mutant 11 (`lowerUnchecked`, Slice 1): suppresses explicit `abort` and returns `0`. -/
theorem mutant_lowerUnchecked_slice1_drop_abort (profile : Profile) :
    let mutant : Passes sliceOne profile :=
      { corePasses sliceOne profile with
        lowerUnchecked := fun _ => IntegratedCore.lowerUnchecked ret0OneCore }
    ¬ (registry mutant).lowerUnchecked.Validated := by
  intro mutant hval
  obtain ⟨mir0, hok0, _, _⟩ := Concrete.Core.LowerMIR.lowerUnchecked_spec (profile := profile) ret0OneCore_uncheckedWF
  have hobs0 : (MIR.Execution (language := sliceLanguage sliceOne)).observes mir0
      (.returned 0 ByteArray.empty) :=
    mir_observes_exact ret0OneCore_uncheckedWF
      (Concrete.Core.LowerMIR.validDischarges_nil_of_uncheckedWF ret0OneCore_uncheckedWF) hok0 ret0One_observable
  have hlowMut : LowersUnchecked (language := sliceLanguage sliceOne) abortCore mir0 :=
    (hval.1 abortCore mir0 abortCore_uncheckedWF hok0).2
  obtain ⟨hsafe, coreObs, hcoreObsable, hcoreIff⟩ :=
    IntegratedCore.Agreement.unchecked_agreement abortCore_uncheckedWF abortView (mkCore_represents abortView)
  have hcore0 := core_observes_of_lowersCore hsafe hlowMut.2 hobs0
  have habort : coreObs = abortExpectedObs :=
    Option.some.inj (hcoreObsable.symm.trans abort_observable)
  have heq : (Concrete.Contracts.Observation.returned 0 ByteArray.empty) = abortExpectedObs :=
    ((hcoreIff _).mp hcore0).trans habort
  cases heq

/-! ### 4. Structural negative controls (`MIR.HandoffWF` & `ISS-0118` cross-slice guard) -/

/-- Negative control: erasing nominal declarations from a Slice 1 MIR module with a
nominal local breaks `MIR.HandoffWF` (`Materializable`). -/
theorem control_handoffWF_undeclared_nominal (profile : Profile) :
    ∃ mir : MIR (sliceLoweringLanguage sliceOne) profile,
      IntegratedCore.lowerUnchecked nominalCore = .ok mir ∧
      MIR.HandoffWF (language := sliceLanguage sliceOne) mir ∧
      ¬ MIR.HandoffWF (language := sliceLanguage sliceOne) (profile := profile)
          { mir with declarations := [] } := by
  obtain ⟨mir, hok, hwf, _⟩ := Concrete.Core.LowerMIR.lowerUnchecked_spec (profile := profile) nominalCore_uncheckedWF
  refine ⟨mir, hok, hwf, ?_⟩
  intro hbad
  have hmat := hbad.2
  have hlocal : ∃ locals graph,
      (earlyEntry, (locals, graph)) ∈ mir.functions ∧
      (⟨0⟩, Concrete.Core.LowerMIR.nominalTy tokenName) ∈ locals := by
    have hrun : (match Concrete.Core.LowerMIR.lowerUnchecked (profile := profile) nominalCore with
        | .ok m => m.functions.any fun (fn, locals, _) =>
            fn == earlyEntry && locals.any fun (r, ty) =>
              let ty' : EarlyTy := ty
              r == ⟨0⟩ && decide (ty' = Concrete.Core.LowerMIR.nominalTy tokenName)
        | .error _ => false) = true := by
      rfl
    rw [hok] at hrun
    obtain ⟨⟨fn, locals, graph⟩, hfnMem, hfnB⟩ := List.any_eq_true.mp hrun
    simp only [Bool.and_eq_true, beq_iff_eq] at hfnB
    obtain ⟨rfl, hlocAny⟩ := hfnB
    obtain ⟨⟨r, ty⟩, hlocMem, hlocB⟩ := List.any_eq_true.mp hlocAny
    simp only [Bool.and_eq_true, beq_iff_eq, decide_eq_true_eq] at hlocB
    obtain ⟨rfl, htyEq⟩ := hlocB
    exact ⟨locals, graph, hfnMem, htyEq ▸ hlocMem⟩
  obtain ⟨locals, graph, hfnMem, hlocMem⟩ := hlocal
  obtain ⟨d, hdLayout, _⟩ := hmat earlyEntry locals graph hfnMem ⟨0⟩
    (Concrete.Core.LowerMIR.nominalTy tokenName) hlocMem (.inl rfl)
  simp [sliceLanguage, Declarations.emptyLinear] at hdLayout

/-- Negative control (`ISS-0118` cross-slice guard): a Slice 1 `abort` Core input cannot
be ready for `slice0` admission (even though `sliceLanguage slice0 = sliceLanguage sliceOne`). -/
theorem control_slice0_rejects_slice1_abort :
    ¬ (AdmissionInput.core (language := sliceLanguage slice0) abortCore).Ready slice0 := by
  intro hready
  have hslice : sliceOne = slice0 := hready.1
  cases hslice

end ConcreteChecks.StageInterfaces.D5Core
