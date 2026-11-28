import Concrete.IntegratedCore.Agreement
import ConcreteChecks.StageInterfaces.Frontend.Elaboration

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

/-! ### Direct theorem instantiations and registered admission edge validation -/

theorem checkInput_dischargeTransfer : ConcreteSpec.Admission.DischargeTransfer :=
  IntegratedCore.Agreement.dischargeTransfer

theorem checkInput_admissionSound {language : CoreLanguage} :
    AdmissionSound (language := language) ConcreteSpec.Admission.checkInput :=
  IntegratedCore.Agreement.admissionSound

/-- Any registered pass bundle wired to `ConcreteSpec.Admission.checkInput`
satisfies the frozen `admission` stage contract on every ready input. -/
theorem admissionEntry_validated {slice : Slice} {profile : Profile}
    (passes : Passes slice profile)
    (hcheck : passes.checkCore = ConcreteSpec.Admission.checkInput) :
    (registry passes).admission.Validated := by
  have hsound := checkInput_admissionSound (language := sliceLanguage slice)
  constructor
  · intro input output ready ran
    change (if input.slice = slice then _ else _) = Except.ok output at ran
    rw [ite_eq_left ready.1] at ran
    cases hprep : input.prepare with
    | error cause => simp [hprep, Except.mapError, Bind.bind, Except.bind] at ran
    | ok prepared =>
      simp only [hprep, Except.mapError, Bind.bind, Except.bind] at ran
      rw [hcheck] at ran
      cases hchk : ConcreteSpec.Admission.checkInput prepared with
      | error _ => simp [hchk, Except.map] at ran
      | ok discharges =>
        simp only [hchk, Except.map, Except.ok.injEq] at ran
        subst output
        obtain ⟨hwf, hfrag⟩ := ready.prepared hprep
        obtain ⟨hiff, hok, _⟩ := hsound prepared hwf hfrag
        have hvalid : Admitted.Valid ⟨prepared, discharges⟩ :=
          ⟨hiff.mpr ⟨discharges, hchk⟩, (hok discharges hchk).2⟩
        exact ⟨hvalid, ready.1, prepared, hprep, rfl, hvalid⟩
  · intro input failure ready ran
    change (if input.slice = slice then _ else _) = Except.error failure at ran
    rw [ite_eq_left ready.1] at ran
    cases hprep : input.prepare with
    | error cause =>
      simp only [hprep, Except.mapError, Bind.bind, Except.bind, Except.error.injEq] at ran
      subst failure
      exact ⟨ready.1, by simp [AdmissionInput.CheckerRefuses, hprep]⟩
    | ok prepared =>
      simp only [hprep, Except.mapError, Bind.bind, Except.bind] at ran
      rw [hcheck] at ran
      cases hchk : ConcreteSpec.Admission.checkInput prepared with
      | ok _ => simp [hchk, Except.map] at ran
      | error err =>
        simp only [hchk, Except.map, Except.error.injEq] at ran
        subst failure
        obtain ⟨hwf, hfrag⟩ := ready.prepared hprep
        obtain ⟨_, _, herr⟩ := hsound prepared hwf hfrag
        obtain ⟨diagnostic, rfl, href⟩ := herr err hchk
        exact ⟨ready.1, by simp [AdmissionInput.CheckerRefuses, hprep, href]⟩

example : (registry (returnPasses fixtureElaborate)).admission.Validated :=
  admissionEntry_validated (returnPasses fixtureElaborate) rfl

/-! ### Reusable deterministic-trace and well-formedness helpers for concrete fixtures -/

private theorem step_none_of_final {O : Exec.Operands} {P : Exec.Program O} {c : Exec.Config O}
    {f : Exec.Final O.Val} (hfinal : c.final? = some f) : Exec.step P c = none := by
  cases c with | mk ctrl _ _ frames => cases ctrl with
    | exec _ | refused => cases hfinal
    | done => cases frames with | nil => rfl | cons _ _ => cases hfinal
    | exiting e => cases e with
      | brk _ | cont _ => cases hfinal
      | ret _ => cases frames with | nil => rfl | cons _ _ => cases hfinal
    | trapped _ _ => rfl

private def traceSafe {language : CoreLanguage} (m : MachineProgram language) :
    Nat → Exec.Config (language.instrumented m.ownership m.declarations) → Bool
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
      match Exec.step m.body c with
      | some (.next next) => traceSafe m n next
      | _ => false

private theorem traceSafe_sound {language : CoreLanguage} (m : MachineProgram language) :
    ∀ (n : Nat) (c state : Exec.Config (language.instrumented m.ownership m.declarations))
      (ev : List (language.instrumented m.ownership m.declarations).Event)
      (kind : TrapKind) (site : SiteId),
      traceSafe m n c = true →
      Exec.Steps m.body c ev state →
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
      cases hst : Exec.step m.body c with
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

private def traceStep {language : CoreLanguage} (m : MachineProgram language) :
    Nat → Exec.Config (language.instrumented m.ownership m.declarations) →
      Option (Exec.Config (language.instrumented m.ownership m.declarations))
  | 0, c => some c
  | n + 1, c =>
    match Exec.step m.body c with
    | some (.next next) => traceStep m n next
    | _ => none

private theorem traceStep_steps {language : CoreLanguage} (m : MachineProgram language) :
    ∀ (n : Nat) (c target : Exec.Config (language.instrumented m.ownership m.declarations)),
      traceStep m n c = some target → Exec.Steps m.body c [] target := by
  intro n
  induction n with
  | zero => intro c target h; cases h; exact .refl _
  | succ n ih =>
    intro c target h
    simp only [traceStep] at h
    cases hst : Exec.step m.body c with
    | none => rw [hst] at h; cases h
    | some act =>
      cases act with
      | request _ _ _ _ => rw [hst] at h; cases h
      | next next =>
        rw [hst] at h
        exact .step (.silent hst) (ih next target h)

private def s0Span : Surface0.Span := ⟨18, 28, 1, 19, ⟨0⟩⟩
private def s0LeftSpan : Surface0.Span := ⟨25, 26, 1, 26, ⟨0⟩⟩
private def s0RightSpan : Surface0.Span := ⟨26, 27, 1, 27, ⟨0⟩⟩
private def s0DeclSpan : Surface0.Span := ⟨3, 7, 1, 4, ⟨0⟩⟩

private theorem s0Span_binds : ∃ span, (surfaceFileSpan s0Span).Binds elaborationSource.project span :=
  ⟨⟨⟨0⟩, 18, 28, by decide, 1, 19, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private theorem s0DeclSpan_binds : ∃ span, (surfaceFileSpan s0DeclSpan).Binds elaborationSource.project span :=
  ⟨⟨⟨0⟩, 3, 7, by decide, 1, 4, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private theorem s0LeftSpan_binds : ∃ span, (surfaceFileSpan s0LeftSpan).Binds elaborationSource.project span :=
  ⟨⟨⟨0⟩, 25, 26, by decide, 1, 26, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private theorem s0RightSpan_binds : ∃ span, (surfaceFileSpan s0RightSpan).Binds elaborationSource.project span :=
  ⟨⟨⟨0⟩, 26, 27, by decide, 1, 27, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private def knownFixtureSpan (loc : Surface0.Span) : Bool :=
  loc == s0Span || loc == s0DeclSpan || loc == s0LeftSpan || loc == s0RightSpan

private theorem knownFixtureSpan_binds {loc : Surface0.Span} (h : knownFixtureSpan loc = true) :
    ∃ span, (surfaceFileSpan loc).Binds elaborationSource.project span := by
  simp only [knownFixtureSpan, Bool.or_eq_true, beq_iff_eq] at h
  rcases h with ((rfl | rfl) | rfl) | rfl
  · exact s0Span_binds
  · exact s0DeclSpan_binds
  · exact s0LeftSpan_binds
  · exact s0RightSpan_binds

private def fixtureReadSpans : AdmissionView → List Surface0.Span
  | .zero p => p.resultSpan :: p.bindings.flatMap fun b =>
      match b with
      | ⟨⟨_, .atom _⟩, span, _, _, _⟩ => [span]
      | ⟨⟨_, .add _ _⟩, _, _, (ls, rs), _⟩ => [ls, rs]
  | .one _ => []

private theorem straightLine_wf_of_checks (v : AdmissionView) (fuel : Nat)
    (hnodupOrig : (List.map Prod.fst v.execution.origins).Nodup)
    (hnodupOps : (List.map Prod.fst v.execution.operations).Nodup)
    (hnodupKeys : (List.map (fun e : SiteId × Origin => e.2.evidenceKey) v.execution.origins).Nodup)
    (hops : v.execution.operations.all (fun p => (AList.lookup v.execution.origins p.1).isSome) = true)
    (hsafe : traceSafe v.execution fuel v.execution.initial = true)
    (horigs : v.execution.origins.all (fun p => p.1 == p.2.operation && p.2.lexical == surfaceFileSpan s0Span) = true)
    (hargs : v.execution.operations.all (fun p => match p.2 with | .expression _ | .abort => true | _ => false) = true)
    (hdiag : (List.map Prod.snd v.located.declarations ++ v.located.bindings.map (·.declarationSpan) ++
        fixtureReadSpans v).all knownFixtureSpan = true) :
    CoreInput.WF (language := sliceLanguage v.slice) ⟨elaborationSource.project, v.slice, .straightLine v rfl⟩ := by
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
    exact ⟨hb.1, hb.2 ▸ s0Span_binds⟩
  · intro site op hmem
    have hb := List.all_eq_true.mp hargs (site, op) hmem
    cases op with
    | expression _ | abort => trivial
    | call _ _ | primitive _ _ => cases hb
  · intro loc hmem
    have hdiag' := List.all_eq_true.mp hdiag
    have hl : knownFixtureSpan loc = true := by
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
    exact knownFixtureSpan_binds hl

/-! ### Positive control 1: Slice 0 two-addition program (full and partial discharge subsets) -/

private def twoAddProgram : EarlyProgram :=
  ⟨[⟨⟨⟨0⟩, .atom (.lit 19)⟩, s0Span, s0DeclSpan, (), .named ⟨"a"⟩⟩,
    ⟨⟨⟨1⟩, .atom (.lit 23)⟩, s0Span, s0DeclSpan, (), .named ⟨"b"⟩⟩,
    ⟨⟨⟨2⟩, .add (.var ⟨0⟩) (.var ⟨1⟩)⟩, s0Span, s0DeclSpan, (s0LeftSpan, s0RightSpan), .named ⟨"c"⟩⟩,
    ⟨⟨⟨3⟩, .add (.var ⟨2⟩) (.lit 1)⟩, s0Span, s0DeclSpan, (s0LeftSpan, s0RightSpan), .named ⟨"d"⟩⟩],
   .var ⟨3⟩, s0Span, s0LeftSpan⟩

private def twoAddView : AdmissionView := .zero twoAddProgram

private def twoAddCore : CoreInput earlyFixtureLanguage :=
  ⟨elaborationSource.project, slice0, .straightLine twoAddView rfl⟩

private def twoAddBatch : ConcreteSpec.Admission.Batch twoAddView.program :=
  ⟨[⟨2, .u8, .copy ⟨0⟩, .copy ⟨1⟩,
      [(⟨1⟩, ⟨.scalar (.int .u8), true, false, some (.int .u8 23), []⟩),
       (⟨0⟩, ⟨.scalar (.int .u8), true, false, some (.int .u8 19), []⟩)]⟩,
    ⟨3, .u8, .copy ⟨2⟩, .int .u8 1,
      [(⟨2⟩, ⟨.scalar (.int .u8), true, false, some (.int .u8 42), []⟩),
       (⟨1⟩, ⟨.scalar (.int .u8), true, true, some (.int .u8 23), []⟩),
       (⟨0⟩, ⟨.scalar (.int .u8), true, true, some (.int .u8 19), []⟩)]⟩], rfl⟩

private def twoAddInputBatch : ConcreteSpec.Admission.InputBatch twoAddCore :=
  ⟨twoAddView, ⟨rfl, rfl, rfl⟩, twoAddBatch⟩

private def dAdd2 : ConcreteSpec.Admission.Discharge := twoAddBatch.discharges[0]'(by decide)
private def dAdd3 : ConcreteSpec.Admission.Discharge := twoAddBatch.discharges[1]'(by decide)

set_option maxRecDepth 4096 in
private theorem twoAdd_wf : twoAddCore.WF :=
  straightLine_wf_of_checks twoAddView 20 (by with_unfolding_all decide) (by with_unfolding_all decide)
    (by with_unfolding_all decide) (by with_unfolding_all rfl) (by with_unfolding_all rfl)
    (by with_unfolding_all rfl) (by with_unfolding_all rfl) (by with_unfolding_all rfl)

-- Check that `checkInput` encodes both discharges, and that full, singleton, and
-- empty subsets all transfer to `ValidDischarges` via `Agreement.dischargeTransfer`.
set_option maxRecDepth 4096 in
example :
    (∃ out, ConcreteSpec.Admission.checkInput twoAddCore = .ok out ∧
      List.map Prod.fst out = [⟨2⟩, ⟨3⟩] ∧
      twoAddCore.Accepted ∧ twoAddCore.UncheckedWF ∧ ValidDischarges twoAddCore out) ∧
    (∃ out, twoAddInputBatch.encode [dAdd2] = some out ∧ ValidDischarges twoAddCore out) ∧
    (∃ out, twoAddInputBatch.encode [dAdd3] = some out ∧ ValidDischarges twoAddCore out) ∧
    (∃ out, twoAddInputBatch.encode [] = some out ∧ ValidDischarges twoAddCore out) ∧
    ConcreteSpec.run twoAddView.program 5 = .exit 43 := by
  refine ⟨?_, ?_, ?_, ?_, rfl⟩
  · obtain ⟨out, henc⟩ := IntegratedCore.DischargeTransfer.encode_issued_isSome twoAddInputBatch
    have hok : ConcreteSpec.Admission.checkInput twoAddCore = .ok out := by
      unfold ConcreteSpec.Admission.checkInput
      have had : ConcreteSpec.Admission.admitInput twoAddCore = .ok twoAddInputBatch := by
        with_unfolding_all rfl
      simp [had, henc, Bind.bind, Except.bind, Pure.pure, Except.pure]
    have hkeys : List.map Prod.fst out = [⟨2⟩, ⟨3⟩] := by
      have hmap : (twoAddInputBatch.encode twoAddInputBatch.batch.discharges).map (List.map Prod.fst) =
          some [⟨2⟩, ⟨3⟩] := by
        with_unfolding_all rfl
      rw [henc] at hmap
      exact Option.some.inj hmap
    obtain ⟨hiff, hsound_ok, _⟩ :=
      checkInput_admissionSound twoAddCore twoAdd_wf ⟨twoAddView, rfl, rfl, rfl⟩
    obtain ⟨hunc, hval⟩ := hsound_ok out hok
    exact ⟨out, hok, hkeys, hiff.mpr ⟨out, hok⟩, hunc, hval⟩
  · obtain ⟨out, henc⟩ := IntegratedCore.DischargeTransfer.encode_validated_isSome twoAddInputBatch (selected := [dAdd2]) (by decide)
    exact ⟨out, henc, checkInput_dischargeTransfer twoAddCore twoAddInputBatch [dAdd2] out twoAdd_wf henc⟩
  · obtain ⟨out, henc⟩ := IntegratedCore.DischargeTransfer.encode_validated_isSome twoAddInputBatch (selected := [dAdd3]) (by decide)
    exact ⟨out, henc, checkInput_dischargeTransfer twoAddCore twoAddInputBatch [dAdd3] out twoAdd_wf henc⟩
  · exact ⟨[], rfl, checkInput_dischargeTransfer twoAddCore twoAddInputBatch [] [] twoAdd_wf rfl⟩

/-! ### Positive control 2: Slice 1 linear nominal + boundary `u8` addition, returning and aborting -/

private def tokName : ConcreteSpec.Name := ⟨"Token"⟩
private def s1Intro : LocatedStraightBinding :=
  ⟨.bind (.name ⟨0⟩) (.nominal tokName) (.construct tokName), s0Span, s0DeclSpan, .named ⟨"t"⟩⟩
private def s1Take : LocatedStraightBinding :=
  ⟨.destructure tokName (.move ⟨0⟩), s0Span, s0Span, .unnamed none⟩
private def s1AddBoundary : LocatedStraightBinding :=
  ⟨.bind (.name ⟨1⟩) .u8 (.scalar (.add (.lit 200) (.lit 55))), s0Span, s0DeclSpan, .named ⟨"x"⟩⟩

private def s1ReturnView : AdmissionView :=
  .one ⟨[(tokName, s0DeclSpan)], [s1Intro, s1Take, s1AddBoundary], .ret (.var ⟨1⟩), s0Span⟩
private def s1ReturnCore : CoreInput (sliceLanguage sliceOne) :=
  ⟨elaborationSource.project, sliceOne, .straightLine s1ReturnView rfl⟩

private def s1AbortAdd : LocatedStraightBinding :=
  ⟨.bind (.name ⟨0⟩) .u8 (.scalar (.add (.lit 20) (.lit 22))), s0Span, s0DeclSpan, .named ⟨"x"⟩⟩
private def s1UseX : LocatedStraightBinding :=
  ⟨.bind .wild .u8 (.scalar (.atom (.var ⟨0⟩))), s0Span, s0Span, .unnamed none⟩
private def s1AbortView : AdmissionView :=
  .one ⟨[], [s1AbortAdd, s1UseX], .abort, s0Span⟩
private def s1AbortCore : CoreInput (sliceLanguage sliceOne) :=
  ⟨elaborationSource.project, sliceOne, .straightLine s1AbortView rfl⟩

set_option maxRecDepth 4096 in
private theorem s1Return_wf : s1ReturnCore.WF :=
  straightLine_wf_of_checks s1ReturnView 20 (by with_unfolding_all decide) (by with_unfolding_all decide)
    (by with_unfolding_all decide) (by with_unfolding_all rfl) (by with_unfolding_all rfl)
    (by with_unfolding_all rfl) (by with_unfolding_all rfl) (by with_unfolding_all rfl)

set_option maxRecDepth 4096 in
private theorem s1Abort_wf : s1AbortCore.WF :=
  straightLine_wf_of_checks s1AbortView 20 (by with_unfolding_all decide) (by with_unfolding_all decide)
    (by with_unfolding_all decide) (by with_unfolding_all rfl) (by with_unfolding_all rfl)
    (by with_unfolding_all rfl) (by with_unfolding_all rfl) (by with_unfolding_all rfl)

-- Both Slice 1 programs (including explicit `abort` after a discharged addition)
-- and the literal return fixture admit cleanly with `ValidDischarges`.
set_option maxRecDepth 4096 in
example :
    (∃ out, ConcreteSpec.Admission.checkInput s1ReturnCore = .ok out ∧
      List.map Prod.fst out = [⟨2⟩] ∧ ValidDischarges s1ReturnCore out) ∧
    ConcreteSpec.run s1ReturnView.program 5 = .exit 255 ∧
    (∃ out, ConcreteSpec.Admission.checkInput s1AbortCore = .ok out ∧
      List.map Prod.fst out = [⟨0⟩] ∧ ValidDischarges s1AbortCore out) ∧
    ConcreteSpec.run s1AbortView.program 5 = .aborted ⟨2⟩ ∧
    (ConcreteSpec.Admission.checkInput (elaboratedReturn 42) = .ok [] ∧
      (elaboratedReturn 42).Accepted ∧ ValidDischarges (elaboratedReturn 42) []) := by
  refine ⟨?_, rfl, ?_, rfl, ?_⟩
  · cases hchk : ConcreteSpec.Admission.checkInput s1ReturnCore with
    | error _ =>
      have hev : (ConcreteSpec.Admission.checkInput s1ReturnCore).map (List.map Prod.fst) = .ok [⟨2⟩] := by
        with_unfolding_all rfl
      rw [hchk] at hev; cases hev
    | ok out =>
      have hkeys : List.map Prod.fst out = [⟨2⟩] := by
        have hev : (ConcreteSpec.Admission.checkInput s1ReturnCore).map (List.map Prod.fst) = .ok [⟨2⟩] := by
          with_unfolding_all rfl
        rw [hchk] at hev; exact Except.ok.inj hev
      have hval := (checkInput_admissionSound s1ReturnCore s1Return_wf ⟨s1ReturnView, rfl, rfl, rfl⟩).2.1 out hchk |>.2
      exact ⟨out, rfl, hkeys, hval⟩
  · cases hchk : ConcreteSpec.Admission.checkInput s1AbortCore with
    | error _ =>
      have hev : (ConcreteSpec.Admission.checkInput s1AbortCore).map (List.map Prod.fst) = .ok [⟨0⟩] := by
        with_unfolding_all rfl
      rw [hchk] at hev; cases hev
    | ok out =>
      have hkeys : List.map Prod.fst out = [⟨0⟩] := by
        have hev : (ConcreteSpec.Admission.checkInput s1AbortCore).map (List.map Prod.fst) = .ok [⟨0⟩] := by
          with_unfolding_all rfl
        rw [hchk] at hev; exact Except.ok.inj hev
      have hval := (checkInput_admissionSound s1AbortCore s1Abort_wf ⟨s1AbortView, rfl, rfl, rfl⟩).2.1 out hchk |>.2
      exact ⟨out, rfl, hkeys, hval⟩
  · have hok : ConcreteSpec.Admission.checkInput (elaboratedReturn 42) = .ok [] := by
      with_unfolding_all rfl
    obtain ⟨hiff, hsound_ok, _⟩ :=
      checkInput_admissionSound (elaboratedReturn 42) (returnedCoreWF 42) ⟨_, representsReturn 42⟩
    exact ⟨hok, hiff.mpr ⟨[], hok⟩, (hsound_ok [] hok).2⟩

/-! ### Negative control 1: Overflowing addition refused and forged discharge refuted by `Exec.Steps` -/

private def overflowAddProgram : EarlyProgram :=
  ⟨[⟨⟨⟨0⟩, .add (.lit 200) (.lit 56)⟩, s0Span, s0DeclSpan, (s0LeftSpan, s0RightSpan), .named ⟨"x"⟩⟩],
   .var ⟨0⟩, s0Span, s0LeftSpan⟩

private def overflowAddView : AdmissionView := .zero overflowAddProgram

private def overflowAddCore : CoreInput earlyFixtureLanguage :=
  ⟨elaborationSource.project, slice0, .straightLine overflowAddView rfl⟩

set_option maxRecDepth 4096 in
private theorem overflowAdd_wf : overflowAddCore.WF :=
  straightLine_wf_of_checks overflowAddView 10 (by with_unfolding_all decide) (by with_unfolding_all decide)
    (by with_unfolding_all decide) (by with_unfolding_all rfl) (by with_unfolding_all rfl)
    (by with_unfolding_all rfl) (by with_unfolding_all rfl) (by with_unfolding_all rfl)

private def forgedOverflowDischarge : StageInterface.Discharge earlyFixtureLanguage :=
  { operation := ⟨0⟩
    key := (overflowAddView.located.bindingOrigins[0]'(by decide)).2.evidenceKey
    obligation := ⟨⟨0⟩, .safety, 0⟩
    trap := .overflow
    operands := [(.inl (.scalar (.int .u8)), .literal [200]), (.inl (.scalar (.int .u8)), .literal [56])] }

-- `overflowAddCore` traps on `200 + 56`: `checkInput` refuses it with `CheckerRefuses`,
-- it is not `Accepted`, and any forged discharge claiming non-overflow at site `⟨0⟩`
-- is directly refuted by the trapping `Exec.Steps` transition.
set_option maxRecDepth 4096 in
example :
    (∃ diagnostic, ConcreteSpec.Admission.checkInput overflowAddCore = .error (.refused diagnostic) ∧
      overflowAddCore.CheckerRefuses diagnostic) ∧
    ¬ overflowAddCore.Accepted ∧
    ConcreteSpec.run overflowAddView.program 3 = .trap .arithmeticOverflow ∧
    ¬ ValidDischarges overflowAddCore [(⟨0⟩, [forgedOverflowDischarge])] := by
  obtain ⟨hiff, _, herr⟩ :=
    checkInput_admissionSound overflowAddCore overflowAdd_wf ⟨overflowAddView, rfl, rfl, rfl⟩
  have hchk : ∃ diagnostic, ConcreteSpec.Admission.checkInput overflowAddCore = .error (.refused diagnostic) ∧
      overflowAddCore.CheckerRefuses diagnostic := by
    cases h : ConcreteSpec.Admission.checkInput overflowAddCore with
    | ok _ =>
      have hbad : (ConcreteSpec.Admission.checkInput overflowAddCore).isOk = false := by
        with_unfolding_all rfl
      rw [h] at hbad; cases hbad
    | error err =>
      obtain ⟨d, rfl, href⟩ := herr err h
      exact ⟨d, rfl, href⟩
  obtain ⟨d, hd, href⟩ := hchk
  refine ⟨⟨d, hd, href⟩, ?_, rfl, ?_⟩
  · intro hacc
    obtain ⟨_, hok⟩ := hiff.mp hacc
    rw [hd] at hok; cases hok
  · intro hvalid
    have hcontains : Discharges.Contains ([(⟨0⟩, [forgedOverflowDischarge])] : Discharges earlyFixtureLanguage)
        ⟨0⟩ forgedOverflowDischarge := ⟨[forgedOverflowDischarge], .head _, .head _⟩
    have hnotrap := (hvalid.2.2.2 ⟨0⟩ forgedOverflowDischarge hcontains).2.2.2.2.2
    -- Reach the active `add` state in 1 silent step, then exhibit the trapping step.
    let m := overflowAddCore.machineProgram
    cases htr : traceStep m 1 m.initial with
    | none =>
      have his : (traceStep m 1 m.initial).isSome = true := by with_unfolding_all rfl
      rw [htr] at his; cases his
    | some activeState =>
      have hreach : Exec.Steps m.body m.initial [] activeState := traceStep_steps m 1 m.initial activeState htr
      have hact : m.activeOperation activeState =
          some (⟨0⟩, .expression (.inl (.add .u8 (.literal (.int .u8 200)) (.literal (.int .u8 56))))) := by
        have hev : (traceStep m 1 m.initial).bind m.activeOperation =
            some (⟨0⟩, .expression (.inl (.add .u8 (.literal (.int .u8 200)) (.literal (.int .u8 56))))) := by
          with_unfolding_all rfl
        rw [htr] at hev; exact hev
      cases hstep : Exec.step m.body activeState with
      | none =>
        have hev : ((traceStep m 1 m.initial).bind (Exec.step m.body)).isSome = true := by
          with_unfolding_all rfl
        rw [htr] at hev; simp [hstep] at hev
      | some act =>
        cases act with
        | request _ _ _ _ =>
          have hev : ((traceStep m 1 m.initial).bind (fun c =>
              match Exec.step m.body c with
              | some (.next n) => some n.ctrl
              | _ => none)) = some (.trapped .overflow ⟨0⟩) := by
            with_unfolding_all rfl
          rw [htr] at hev; simp [hstep] at hev
        | next trappedState =>
          have hctrl : trappedState.ctrl = .trapped .overflow ⟨0⟩ := by
            have hev : ((traceStep m 1 m.initial).bind (fun c =>
                match Exec.step m.body c with
                | some (.next n) => some n.ctrl
                | _ => none)) = some (.trapped .overflow ⟨0⟩) := by
              with_unfolding_all rfl
            rw [htr] at hev; simp [hstep] at hev; exact hev
          exact (hnotrap activeState [] _ hreach hact).2 .silent trappedState (.silent hstep) hctrl

/-! ### Negative control 2: Checker refusals render with located `CheckerRefuses`, never `.missingLocation` -/

private def badLhsCore : CoreInput earlyFixtureLanguage :=
  ⟨elaborationSource.project, slice0, .straightLine (.zero
    ⟨[⟨⟨⟨0⟩, .add (.lit 256) (.lit 1)⟩, s0Span, s0DeclSpan, (s0LeftSpan, s0RightSpan), .named ⟨"x"⟩⟩],
     .var ⟨0⟩, s0Span, s0LeftSpan⟩) rfl⟩

private def badRhsCore : CoreInput earlyFixtureLanguage :=
  ⟨elaborationSource.project, slice0, .straightLine (.zero
    ⟨[⟨⟨⟨0⟩, .add (.lit 1) (.lit 256)⟩, s0Span, s0DeclSpan, (s0LeftSpan, s0RightSpan), .named ⟨"x"⟩⟩],
     .var ⟨0⟩, s0Span, s0LeftSpan⟩) rfl⟩

private def dupBinderCore : CoreInput earlyFixtureLanguage :=
  ⟨elaborationSource.project, slice0, .straightLine (.zero
    ⟨[⟨⟨⟨0⟩, .atom (.lit 1)⟩, s0Span, s0DeclSpan, (), .named ⟨"a"⟩⟩,
      ⟨⟨⟨0⟩, .atom (.lit 2)⟩, s0Span, s0DeclSpan, (), .named ⟨"a"⟩⟩],
     .var ⟨0⟩, s0Span, s0LeftSpan⟩) rfl⟩

private def unusedBindingCore : CoreInput earlyFixtureLanguage :=
  ⟨elaborationSource.project, slice0, .straightLine (.zero
    ⟨[⟨⟨⟨0⟩, .atom (.lit 1)⟩, s0Span, s0DeclSpan, (), .named ⟨"a"⟩⟩],
     .lit 42, s0Span, s0LeftSpan⟩) rfl⟩

private def liveLinearCore : CoreInput (sliceLanguage sliceOne) :=
  ⟨elaborationSource.project, sliceOne, .straightLine (.one
    ⟨[(tokName, s0DeclSpan)], [s1Intro], .ret (.lit 42), s0Span⟩) rfl⟩

private def dupStructCore : CoreInput (sliceLanguage sliceOne) :=
  ⟨elaborationSource.project, sliceOne, .straightLine (.one
    ⟨[(tokName, s0DeclSpan), (tokName, s0DeclSpan)], [], .abort, s0Span⟩) rfl⟩

private def reservedStructCore : CoreInput (sliceLanguage sliceOne) :=
  ⟨elaborationSource.project, sliceOne, .straightLine (.one
    ⟨[(⟨"main"⟩, s0DeclSpan)], [], .abort, s0Span⟩) rfl⟩

private theorem refused_of_admit {language : CoreLanguage} {input : CoreInput language}
    {diagnostic : RefusalCause}
    (had : ConcreteSpec.Admission.admitInput input = .error (.refused diagnostic)) :
    ConcreteSpec.Admission.checkInput input = .error (.refused diagnostic) ∧
      input.CheckerRefuses diagnostic :=
  ⟨by simp [ConcreteSpec.Admission.checkInput, had, Bind.bind, Except.bind],
   ConcreteSpec.Admission.admitInput_refused had⟩

set_option maxRecDepth 4096 in
example :
    (let d := RefusalCause.core .slice0 (surfaceFileSpan resultSpan) []
        (.atPosition 0 (.literalOutOfRange .u8 256));
      ConcreteSpec.Admission.checkInput (elaboratedReturn 256) = .error (.refused d) ∧
      (elaboratedReturn 256).CheckerRefuses d) ∧
    (let d := RefusalCause.core .slice0 (surfaceFileSpan s0LeftSpan) []
        (.atPosition 0 (.additionOperandType (.int .u8 256) (.atomFailure (.outOfRange .u8 256))));
      ConcreteSpec.Admission.checkInput badLhsCore = .error (.refused d) ∧
      badLhsCore.CheckerRefuses d) ∧
    (let d := RefusalCause.core .slice0 (surfaceFileSpan s0RightSpan) []
        (.atPosition 0 (.additionOperandType (.int .u8 256) (.atomFailure (.outOfRange .u8 256))));
      ConcreteSpec.Admission.checkInput badRhsCore = .error (.refused d) ∧
      badRhsCore.CheckerRefuses d) ∧
    (let d := RefusalCause.core .slice0 (surfaceFileSpan s0DeclSpan) []
        (.atPosition 1 (.duplicateBinding ⟨0⟩));
      ConcreteSpec.Admission.checkInput dupBinderCore = .error (.refused d) ∧
      dupBinderCore.CheckerRefuses d) ∧
    (let d := RefusalCause.core .slice0 (surfaceFileSpan s0Span) [(⟨"binding"⟩, surfaceFileSpan s0DeclSpan)]
        (.atPosition 1 (.unusedBinding ⟨0⟩));
      ConcreteSpec.Admission.checkInput unusedBindingCore = .error (.refused d) ∧
      unusedBindingCore.CheckerRefuses d) ∧
    (let d := RefusalCause.core .returning (surfaceFileSpan s0Span) [(⟨"binding"⟩, surfaceFileSpan s0DeclSpan)]
        (.atPosition 1 (.liveLinearBinding ⟨0⟩));
      ConcreteSpec.Admission.checkInput liveLinearCore = .error (.refused d) ∧
      liveLinearCore.CheckerRefuses d) ∧
    (let d := RefusalCause.core .aborting (surfaceFileSpan s0DeclSpan) []
        (.duplicateStructName 1 tokName);
      ConcreteSpec.Admission.checkInput dupStructCore = .error (.refused d) ∧
      dupStructCore.CheckerRefuses d) ∧
    (let d := RefusalCause.core .aborting (surfaceFileSpan s0DeclSpan) []
        (.reservedStructName 0 ⟨"main"⟩);
      ConcreteSpec.Admission.checkInput reservedStructCore = .error (.refused d) ∧
      reservedStructCore.CheckerRefuses d) :=
  ⟨refused_of_admit (by with_unfolding_all rfl),
   refused_of_admit (by with_unfolding_all rfl),
   refused_of_admit (by with_unfolding_all rfl),
   refused_of_admit (by with_unfolding_all rfl),
   refused_of_admit (by with_unfolding_all rfl),
   refused_of_admit (by with_unfolding_all rfl),
   refused_of_admit (by with_unfolding_all rfl),
   refused_of_admit (by with_unfolding_all rfl)⟩

/-! ### Negative control 3: `InputBatch.encode` rejects reordered, duplicated, and forged discharges -/

example :
    twoAddInputBatch.encode [dAdd3, dAdd2] = none ∧
    twoAddInputBatch.encode [dAdd2, dAdd2] = none ∧
    twoAddInputBatch.encode [{ dAdd2 with position := 0 }] = none := by
  with_unfolding_all exact ⟨rfl, rfl, rfl⟩

/-! ### Negative control 4: Mutant checkers rejected by `AdmissionSound` -/

private def mutantAcceptOverflow :
    CoreInput earlyFixtureLanguage → Except AdmissionFailure (Discharges earlyFixtureLanguage) :=
  fun _ => .ok []

private def mutantMissingLocation :
    CoreInput earlyFixtureLanguage → Except AdmissionFailure (Discharges earlyFixtureLanguage) :=
  fun _ => .error (.missingLocation (.atPosition 0 (.literalOutOfRange .u8 256)))

private def mutantUnmappedEvidence :
    CoreInput earlyFixtureLanguage → Except AdmissionFailure (Discharges earlyFixtureLanguage) :=
  fun _ => .error .unmappedEvidence

private def forgedReturnDischarge : StageInterface.Discharge earlyFixtureLanguage :=
  { operation := ⟨0⟩
    key := twoAddView.located.endingOrigin.2.evidenceKey
    obligation := ⟨⟨0⟩, .safety, 0⟩
    trap := .overflow
    operands := [] }

private def mutantForgedDischarge :
    CoreInput earlyFixtureLanguage → Except AdmissionFailure (Discharges earlyFixtureLanguage) :=
  fun _ => .ok [(⟨0⟩, [forgedReturnDischarge])]

set_option maxRecDepth 4096 in
example :
    ¬ AdmissionSound mutantAcceptOverflow ∧
    ¬ AdmissionSound mutantMissingLocation ∧
    ¬ AdmissionSound mutantUnmappedEvidence ∧
    ¬ AdmissionSound mutantForgedDischarge := by
  refine ⟨?_, ?_, ?_, ?_⟩
  · intro hsound
    have hacc : overflowAddCore.Accepted :=
      (hsound overflowAddCore overflowAdd_wf ⟨overflowAddView, rfl, rfl, rfl⟩).1.mpr ⟨[], rfl⟩
    obtain ⟨_, hok⟩ :=
      (checkInput_admissionSound overflowAddCore overflowAdd_wf ⟨overflowAddView, rfl, rfl, rfl⟩).1.mp hacc
    have hbad : (ConcreteSpec.Admission.checkInput overflowAddCore).isOk = false := by
      with_unfolding_all rfl
    rw [hok] at hbad; cases hbad
  · intro hsound
    obtain ⟨_, _, herr⟩ :=
      hsound (elaboratedReturn 256) (returnedCoreWF 256) ⟨_, representsReturn 256⟩
    obtain ⟨_, hcontra, _⟩ := herr _ rfl
    cases hcontra
  · intro hsound
    have hacc : (elaboratedReturn 42).Accepted :=
      (checkInput_admissionSound (elaboratedReturn 42) (returnedCoreWF 42) ⟨_, representsReturn 42⟩).1.mpr
        ⟨[], by with_unfolding_all rfl⟩
    obtain ⟨_, hok⟩ :=
      (hsound (elaboratedReturn 42) (returnedCoreWF 42) ⟨_, representsReturn 42⟩).1.mp hacc
    cases hok
  · intro hsound
    have hval :=
      (hsound (elaboratedReturn 42) (returnedCoreWF 42) ⟨_, representsReturn 42⟩).2.1
        [(⟨0⟩, [forgedReturnDischarge])] rfl |>.2
    have hcontains : Discharges.Contains ([(⟨0⟩, [forgedReturnDischarge])] : Discharges earlyFixtureLanguage)
        ⟨0⟩ forgedReturnDischarge := ⟨[forgedReturnDischarge], .head _, .head _⟩
    obtain ⟨_, _, _, ⟨_, conditions, ⟨op, hmem, _, _, hconds⟩, hdin⟩, _, _⟩ :=
      hval.2.2.2 ⟨0⟩ forgedReturnDischarge hcontains
    have hempty : (elaboratedReturn 42).machineProgram.operations.all
        (fun p => (earlyFixtureLanguage.safety p.2).isEmpty) = true := by
      with_unfolding_all rfl
    have hnil := List.all_eq_true.mp hempty (⟨0⟩, op) hmem
    cases hs : earlyFixtureLanguage.safety op with
    | cons _ _ => rw [hs] at hnil; cases hnil
    | nil => rw [hs] at hconds; subst conditions; cases hdin

end StageInterfaceControls
