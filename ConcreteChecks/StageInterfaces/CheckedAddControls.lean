import Concrete.Interfaces.Pipeline

/-!
# Slice 0 freeze controls (unit F9c): checked-add, formation, feature policy, and selected emitter

Standalone freeze controls written directly against the frozen interface modules
(`Concrete.Interfaces.*`, `ConcreteSpec.*`), exercising:
1. Two-result `addWithOverflow` (`Memory`, `SSA`, `BIR`):
   - `20 + 22 → 42` across `Memory.Execution`, `SSA.Execution`, `BIR.Execution`,
     `ResidualAddsMatch`, `BuildsSSA`, and `BuildsBIR`;
   - `255 + 1 → status 134` with exact stderr across all three layers;
   - Two-site checked addition: first-site overflow traps at site 0 (never reaching site 1),
     second-site overflow traps at site 1 (never site 0), and in-range two-site sum returns 43;
   - Planted mutants: constant flag (always false / always true), inverted flag, swapped results
     (type-level rejection + wrong continuation value), aliased `dst`/`flag`, wrapped-on-overflow
     (unconditional jump using wrapped sum, plus unguarded plain add), and substituted origin;
   - Frame-slot budget: two-result `addWithOverflow` charges 2 slots (`maxBIRSlots = 2` passes,
     `maxBIRSlots = 1` fails `BIR.WF` / `BuildsBIR` and matches `buildBIR` `.resource` refusal).
2. `RegisterFormation`:
   - Positive: `u8`/`u32` checked-add, zero-slot self-loop (passes even at `maxBIRSlots = 0`),
     parameterized loop;
   - Negative: aliased `dst`/`flag`, sibling-block non-dominance, diamond non-dominance,
     unreachable-block use-before-def, unreachable `.invalid` terminator, malformed literal,
     out-of-bounds trap slot index.
3. `firstUnsupported` (`u8` vs `u32`) and selected emitter (`emitGuarded`, `PassFunctions.emitEntry`):
   - `u8` accepted on both LCB and LLVM; `u32` accepted on LLVM and refused on LCB with
     `IntWidthSupported` rev 1;
   - Always-refusing emitter fails `emitEntry.Validated` on both backends;
   - Wrong rule / wrong revision rejected by `emitEntry.refusal` and `validateRule`;
   - Unreachable vs entry-reachable calls (`requiresLCBLinking`, `.lcbLinkingUnavailable`,
     `CallSupported`);
   - Missing helper job fails `LCBEncoding` / `EmitsLCB`, present helper job succeeds;
   - LLVM-success / LCB-resource fixture (`trapCapacity < bir.sites.size`).
-/

namespace StageInterfaceControls.CheckedAddControls

open Concrete.Contracts
open Concrete.Interfaces
open Concrete.Backend
open ConcreteSpec.Core
open ConcreteSpec.Core.SourcePathSpan
open ConcreteSpec.StageInterface
open LeanCompilerBackend.Verified

private abbrev s0Core : CoreLanguage := sliceLanguage slice0
private abbrev s0Lang : LoweringLanguage EarlyTy := sliceLoweringLanguage slice0
private abbrev u8Ty : EarlyTy := s0Lang.intTy .u8
private abbrev u32Ty : EarlyTy := s0Lang.intTy .u32
private abbrev boolTy : EarlyTy := s0Lang.bool

private def sourceLine : String := "fn main() -> u8 { return 255 + 1 + 250; }"
private def ctrlProj : Project :=
  Project.singleton mainSourcePath (SourceText.ofBytes sourceLine.toUTF8.data.toList)

private def ctrlMain : FnId :=
  ⟨⟨⟨⟨"pkg"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩, ⟨"main"⟩⟩
private def ctrlHelper : FnId := { ctrlMain with name := ⟨"helper"⟩ }
private def ctrlExtern : FnId := { ctrlMain with name := ⟨"extern_fn"⟩ }

-- Site 0 points at the first `+` (byte 29, line 1, col 30); site 1 at the second `+` (byte 33, line 1, col 34).
private def span0 : FileSpan := ⟨⟨0⟩, 29, 30, 1, 30⟩
private def span1 : FileSpan := ⟨⟨0⟩, 33, 34, 1, 34⟩

private def orig0 : Origin := ⟨⟨0⟩, span0, ⟨.function ctrlMain, ⟨[]⟩⟩⟩
private def orig1 : Origin := ⟨⟨1⟩, span1, ⟨.function ctrlMain, ⟨[]⟩⟩⟩

private def lcbProfile (slots : Nat := 64) : Profile := ⟨.lcb, .x86_64Linux, slots, 4096⟩
private def llvmProfile (slots : Nat := 64) : Profile := ⟨.llvm, .x86_64Linux, slots, 4096⟩

private def trapLine0 : String :=
  s!"concrete: trap: overflow at {mainSourcePath.render.text}:1:30 (site 0)\n"
private def trapLine1 : String :=
  s!"concrete: trap: overflow at {mainSourcePath.render.text}:1:34 (site 1)\n"

private def trapObs0 : Concrete.Contracts.Observation :=
  .trapped 134 .overflow ⟨⟨0⟩, ⟨mainSourcePath.render.text, 1, 30⟩⟩ trapLine0.toUTF8

private def trapObs1 : Concrete.Contracts.Observation :=
  .trapped 134 .overflow ⟨⟨1⟩, ⟨mainSourcePath.render.text, 1, 34⟩⟩ trapLine1.toUTF8

private def retObs (n : UInt8) : Concrete.Contracts.Observation :=
  .returned n ByteArray.empty

private theorem span0_binds : ∃ sp, orig0.lexical.Binds ctrlProj sp := by
  refine ⟨⟨⟨0⟩, 29, 30, by decide, 1, 30, by decide, by decide⟩,
    File.load mainSourcePath (SourceText.ofBytes sourceLine.toUTF8.data.toList),
    rfl, rfl, ?_, rfl, rfl, rfl, rfl⟩
  decide

private theorem span1_binds : ∃ sp, orig1.lexical.Binds ctrlProj sp := by
  refine ⟨⟨⟨0⟩, 33, 34, by decide, 1, 34, by decide, by decide⟩,
    File.load mainSourcePath (SourceText.ofBytes sourceLine.toUTF8.data.toList),
    rfl, rfl, ?_, rfl, rfl, rfl, rfl⟩
  decide

private theorem writeParams_nil_eq {profile : Profile} {mem : Memory s0Lang profile}
    {s1 s2 : MemoryRegisters s0Core profile} {ceil : Nat}
    (h : mem.WriteParams [] [] s1 s2 false ceil) : s2 = s1 := by
  cases h; rfl

private theorem mem_singleton_pair {α β : Type} {a a' : α} {b b' : β}
    (h : (a, b) ∈ [(a', b')]) : a = a' ∧ b = b' :=
  Prod.mk.inj (List.mem_singleton.mp h)

private theorem mem_singleton_triple {α β γ : Type} {a a' : α} {b b' : β} {c c' : γ}
    (h : (a, b, c) ∈ [(a', b', c')]) : a = a' ∧ b = b' ∧ c = c' :=
  let ⟨h1, h23⟩ := Prod.mk.inj (List.mem_singleton.mp h)
  ⟨h1, Prod.mk.inj h23⟩

/-! ## Constructive deterministic-chain helpers (no `Classical.choice`) -/

private inductive DetChain {S : Type} (step : S → S → Prop) : S → S → Prop where
  | nil (s : S) : DetChain step s s
  | cons {a b c : S} : step a b → (∀ x, step a x → x = b) → DetChain step b c → DetChain step a c

private theorem DetChain.toSteps {S : Type} {step : S → S → Prop} {a b : S}
    (h : DetChain step a b) : Steps step a b := by
  induction h with
  | nil s => exact .nil s
  | cons hab _ _ ih => exact .cons hab ih

private theorem DetChain.progress {A E : Type} {S : Semantics A E} {a : A}
    {p qEnd : S.State} {obs : Concrete.Contracts.Observation}
    (h : DetChain (S.step a) p qEnd) (hEnd : S.result a qEnd obs)
    {q : S.State} (hSteps : Steps (S.step a) p q) :
    q = qEnd ∨ ∃ q', S.step a q q' := by
  induction h generalizing q with
  | nil s =>
    cases hSteps with
    | nil => exact .inl rfl
    | cons hst _ => exact (S.terminal a s obs hEnd _ hst).elim
  | cons hab huniq _ ih =>
    cases hSteps with
    | nil => exact .inr ⟨_, hab⟩
    | cons hst hrest =>
      rw [huniq _ hst] at hrest
      exact ih hEnd hrest

private theorem DetChain.noDiverge {A E : Type} {S : Semantics A E} {a : A}
    {q0 qEnd : S.State} {obs : Concrete.Contracts.Observation}
    (hInit : ∀ q, S.initial a q → q = q0)
    (h : DetChain (S.step a) q0 qEnd) (hEnd : S.result a qEnd obs) :
    ¬ ∃ run, S.diverges a run := by
  rintro ⟨run, h0, hstep⟩
  have hfollow : ∀ {p r : S.State}, DetChain (S.step a) p r →
      ∀ k, run k = p → ∃ n, run n = r := by
    intro p r hc
    induction hc with
    | nil _ => intro k hk; exact ⟨k, hk⟩
    | cons _ huniq _ ih =>
      intro k hk
      have hnext : run (k + 1) = _ := huniq _ (hk ▸ hstep k)
      exact ih (k + 1) hnext
  obtain ⟨n, hn⟩ := hfollow h 0 (hInit (run 0) h0)
  exact S.terminal a qEnd obs hEnd (run (n + 1)) (hn ▸ hstep n)

private theorem DetChain.safe {A E : Type} {S : Semantics A E} {a : A}
    {q0 qEnd : S.State} {obs : Concrete.Contracts.Observation}
    (hInit : ∀ q, S.initial a q → q = q0)
    (h : DetChain (S.step a) q0 qEnd) (hEnd : S.result a qEnd obs) :
    ProgramSafe S a := by
  intro q hstuck
  obtain ⟨start, hs, hsteps⟩ := hstuck.1
  rw [hInit start hs] at hsteps
  rcases h.progress hEnd hsteps with rfl | ⟨q', hst⟩
  · exact hstuck.2.2 obs hEnd
  · exact hstuck.2.1 q' hst

private theorem DetChain.observes_iff {A E : Type} {S : Semantics A E} {a : A}
    {q0 qEnd : S.State} {obs : Concrete.Contracts.Observation}
    (hInit : S.initial a q0) (hInitUniq : ∀ q, S.initial a q → q = q0)
    (h : DetChain (S.step a) q0 qEnd)
    (hEnd : ∀ o, S.result a qEnd o ↔ o = obs) :
    ∀ o, S.observes a o ↔ o = obs := by
  intro o
  constructor
  · rintro ⟨q, ⟨start, hs, hsteps⟩, hres⟩
    rw [hInitUniq start hs] at hsteps
    rcases h.progress ((hEnd obs).mpr rfl) hsteps with rfl | ⟨q', hst⟩
    · exact (hEnd o).mp hres
    · exact (S.terminal a q o hres q' hst).elim
  · rintro rfl
    exact ⟨qEnd, ⟨q0, hInit, h.toSteps⟩, (hEnd o).mpr rfl⟩

private theorem DetChain.refines {A B E : Type} {S : Semantics A E} {T : Semantics B E}
    {a : A} {b : B} {pEnd : S.State} {q0 qEnd : T.State} {obs : Concrete.Contracts.Observation}
    (hpFin : S.finite a pEnd) (hpRes : S.result a pEnd obs)
    (hqInitUniq : ∀ q, T.initial b q → q = q0)
    (hqChain : DetChain (T.step b) q0 qEnd)
    (hqEnd : ∀ o, T.result b qEnd o ↔ o = obs)
    (hTrace : S.trace a pEnd = T.trace b qEnd) :
    BehaviorRefinement S T S.result T.result a b := by
  intro _
  refine ⟨?_, ?_, hqChain.safe hqInitUniq ((hqEnd obs).mpr rfl)⟩
  · intro q o ⟨start, hs, hsteps⟩ hres
    rw [hqInitUniq start hs] at hsteps
    rcases hqChain.progress ((hqEnd obs).mpr rfl) hsteps with rfl | ⟨q', hst⟩
    · have ho := (hqEnd o).mp hres
      subst ho
      exact ⟨pEnd, hpFin, hpRes, hTrace⟩
    · exact (T.terminal b q o hres q' hst).elim
  · intro run hdiv
    exact (hqChain.noDiverge hqInitUniq ((hqEnd obs).mpr rfl) ⟨run, hdiv⟩).elim

private theorem DetChain.rejects_refinement {A B E : Type} {S : Semantics A E} {T : Semantics B E}
    {a : A} {b : B} {p0 pEnd : S.State} {obsS obsT : Concrete.Contracts.Observation}
    (hpInit : S.initial a p0) (hpInitUniq : ∀ p, S.initial a p → p = p0)
    (hpChain : DetChain (S.step a) p0 pEnd)
    (hpEnd : ∀ o, S.result a pEnd o ↔ o = obsS)
    (hqObs : T.observes b obsT) (hne : obsT ≠ obsS) :
    ¬ BehaviorRefinement S T S.result T.result a b := by
  intro href
  obtain ⟨q, hqFin, hqRes⟩ := hqObs
  obtain ⟨p, hpFin, hpRes, _⟩ :=
    (href (hpChain.safe hpInitUniq ((hpEnd obsS).mpr rfl))).1 q obsT hqFin hqRes
  have heq := (hpChain.observes_iff hpInit hpInitUniq hpEnd obsT).mp ⟨p, hpFin, hpRes⟩
  exact hne heq

private def mkRegState (profile : Profile) (blk : Nat) (pc : Nat)
    (live init : List PlaceId) (vals : AList PlaceId EarlyValue) : MemoryRegisters s0Core profile :=
  ⟨⟨ctrlMain, ⟨blk⟩, pc, vals, [], live, init, [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩

private theorem obs_ne_ret_trap (n : UInt8) (status : UInt8) (k : TrapKind) (s : DiagnosticSite) (bs : ByteArray) :
    retObs n ≠ Concrete.Contracts.Observation.trapped status k s bs := by
  intro h; cases h

private theorem obs_ne_ret_ret {a b : UInt8} (h : a ≠ b) : retObs a ≠ retObs b :=
  fun heq => h (Concrete.Contracts.Observation.returned.inj heq).1

private theorem obs_ne_trap_site : trapObs0 ≠ trapObs1 := by
  intro h
  have hSite := (Concrete.Contracts.Observation.trapped.inj h).2.2.1
  exact (by decide : (⟨0⟩ : SiteId) ≠ ⟨1⟩) (congrArg DiagnosticSite.id hSite)

/-! ## Section 1: Two-result `addWithOverflow` (`Memory`, `SSA`, `BIR`) and exact stderr -/

private def memPlace0 : MemoryPlace EarlyTy u8Ty := ⟨⟨⟨0⟩, []⟩, .virtual⟩

private def memAddBlock (a b : UInt8) : MemoryBlock s0Lang :=
  let inst : MemoryInst s0Lang :=
    .op (opcode := .arithmetic (.plain .add)) (types := [u8Ty, u8Ty]) (ty := u8Ty)
      memPlace0 () (.cons (.literal ⟨[a]⟩) (.cons (.literal ⟨[b]⟩) .nil))
      [⟨0, .overflow, .residual⟩] []
  ⟨⟨0⟩, [], [⟨orig0, .beginLifetime ⟨0⟩⟩, ⟨orig0, inst⟩],
    .ret (some ⟨u8Ty, .copy memPlace0⟩), []⟩

/-- Minimal Memory program executing a single residual plain `u8` addition `a + b` at `orig0`. -/
private def memAdd (profile : Profile) (a b : UInt8) : Memory s0Lang profile :=
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[memAddBlock a b]⟩)],
    [], [(ctrlMain, [(⟨0⟩, .virtual)])], [], []⟩

private def ssaAddB0 (a b : UInt8) : SSABlock s0Lang :=
  ⟨⟨0⟩, [],
    [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[a]⟩) (.literal ⟨[b]⟩)⟩],
    .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, [⟨u8Ty, .value ⟨0⟩⟩]⟩, []⟩

private def ssaAddB1 (trapOrig : Origin := orig0) : SSABlock s0Lang :=
  ⟨⟨1⟩, [], [], .trap .overflow trapOrig, []⟩

private def ssaAddB2 : SSABlock s0Lang :=
  ⟨⟨2⟩, [⟨u8Ty, ⟨2⟩⟩], [], .ret (some ⟨u8Ty, .value ⟨2⟩⟩), []⟩

/-- Canonical SSA lowering of `memAdd`: two-result `addWithOverflow` branching on `flag`
to empty trap block `b1` (`.trap .overflow orig0`) or continuation `b2` receiving wrapped `sum`. -/
private def ssaAdd (profile : Profile) (a b : UInt8) : SSA s0Lang profile :=
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[ssaAddB0 a b, ssaAddB1, ssaAddB2]⟩)], [], [], []⟩

/-- Canonical BIR lowering of `ssaAdd` with `sites := #[⟨.overflow, orig0⟩]`. -/
private def birAdd (profile : Profile) (a b : UInt8) (sites : TrapTable := #[⟨.overflow, orig0⟩]) :
    BIR s0Lang profile :=
  let b0 : Block _ _ _ :=
    ⟨⟨0⟩, [],
      [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[a]⟩) (.literal ⟨[b]⟩)⟩],
      .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, [⟨u8Ty, .value ⟨0⟩⟩]⟩, []⟩
  let b1 : Block _ _ _ := ⟨⟨1⟩, [], [], .trap ⟨0⟩, []⟩
  let b2 : Block _ _ _ := ⟨⟨2⟩, [⟨u8Ty, ⟨2⟩⟩], [], .ret (some ⟨u8Ty, .value ⟨2⟩⟩), []⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)], [], sites, [], []⟩

private def ssaAddB0View (a b : UInt8) : MemoryBlock s0Lang :=
  ⟨⟨0⟩, [],
    [⟨orig0, .addWithOverflow .u8 ⟨⟨⟨0⟩, []⟩, .virtual⟩ ⟨⟨⟨1⟩, []⟩, .virtual⟩
      (.literal ⟨[a]⟩) (.literal ⟨[b]⟩)⟩],
    .branch (.copy ⟨⟨⟨1⟩, []⟩, .virtual⟩) ⟨⟨1⟩, []⟩
      ⟨⟨2⟩, [⟨u8Ty, .copy ⟨⟨⟨0⟩, []⟩, .virtual⟩⟩]⟩, []⟩

private theorem ssaAdd_20_22_step0 (profile : Profile) :
    let ssa := ssaAdd profile 20 22
    let s0 := mkRegState profile 0 0 [] [] []
    let s1 := mkRegState profile 0 1 [⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
      [(⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 42))]
    (SSA.Execution (language := s0Core)).step ssa s0 s1 ∧
    ∀ next, (SSA.Execution (language := s0Core)).step ssa s0 next → next = s1 := by
  intro ssa s0 s1
  let mid0 := mkRegState profile 0 0 [⟨0⟩] [] []
  let mid1 := mkRegState profile 0 0 [⟨0⟩] [⟨0⟩] [(⟨0⟩, .scalar (.int .u8 42))]
  let mid2 := mkRegState profile 0 0 [⟨1⟩, ⟨0⟩] [⟨0⟩] [(⟨0⟩, .scalar (.int .u8 42))]
  have hStep : (ssa.memoryView (language := s0Core)).InstructionStepWith s0 s1 false true
      (.addWithOverflow .u8 ⟨⟨⟨0⟩, []⟩, .virtual⟩ ⟨⟨⟨1⟩, []⟩, .virtual⟩
        (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)) false := by
    refine ⟨rfl, rfl, Or.inl (by decide),
      .scalar (.int .u8 20), s0, .scalar (.int .u8 22), s0, 20, 22,
      { s1 with instruction := 0 }, by with_unfolding_all rfl, by with_unfolding_all rfl,
      rfl, rfl, ?_, rfl⟩
    apply Memory.WriteParams.cons (started := mid0) (stored := mid1)
    · exact ⟨List.not_mem_nil, Or.inl ⟨rfl, rfl⟩⟩
    · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun,
        by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := mid2) (stored := { s1 with instruction := 0 })
      · exact ⟨by simp [mid1, mkRegState], Or.inl ⟨rfl, rfl⟩⟩
      · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun,
          by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
      · exact .nil _
  constructor
  · refine ⟨by with_unfolding_all rfl, ?_, ⟨_, by with_unfolding_all rfl⟩⟩
    exact ⟨ssaAddB0View 20 22, by with_unfolding_all rfl, hStep⟩
  · intro next ⟨_, hst, _⟩
    change (ssa.memoryView (language := s0Core)).Step s0 next true at hst
    simp only [Memory.Step, s0, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hinst⟩ := hst
      have hblkEq : blk = ssaAddB0View 20 22 := by
        with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      exact absurd hinst (Memory.addWithOverflow_virtual_cannot_exhaust
        (by with_unfolding_all rfl) (by with_unfolding_all rfl))
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hinst⟩ := hst
      have hblkEq : blk = ssaAddB0View 20 22 := by
        with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, _, _, lv, a1, rv, a2, x, y, stored, hl, hr, hlv, hrv, hwp, rfl⟩ := hinst
      have hlEq : (lv, a1) = (.scalar (.int .u8 20), s0) := by with_unfolding_all exact Option.some.inj hl.symm
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj hlEq
      have hrEq : (rv, a2) = (.scalar (.int .u8 22), s0) := by with_unfolding_all exact Option.some.inj hr.symm
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj hrEq
      have hx : x = 20 := by cases hlv; rfl
      have hy : y = 22 := by cases hrv; rfl
      subst hx; subst hy
      cases hwp with
      | cons hbeg1 hw1 hwp2 =>
        rename_i started1 stored1
        rcases hbeg1.2 with ⟨_, hEq1⟩ | ⟨hMat, _⟩
        · rw [hEq1] at hw1
          have hstor1 : stored1 = mid1 := by with_unfolding_all exact Option.some.inj hw1.2.2.1.symm
          rw [hstor1] at hwp2
          cases hwp2 with
          | cons hbeg2 hw2 hwp3 =>
            rename_i started2 stored2
            rcases hbeg2.2 with ⟨_, hEq2⟩ | ⟨hMat, _⟩
            · rw [hEq2] at hw2
              have hstor2 : stored2 = { s1 with instruction := 0 } := by
                with_unfolding_all exact Option.some.inj hw2.2.2.1.symm
              rw [hstor2] at hwp3
              cases hwp3
              rfl
            · exact absurd hMat (by with_unfolding_all intro h; cases h)
        · exact absurd hMat (by with_unfolding_all intro h; cases h)

private theorem ssaAdd_255_1_step0 (profile : Profile) :
    let ssa := ssaAdd profile 255 1
    let s0 := mkRegState profile 0 0 [] [] []
    let s1 := mkRegState profile 0 1 [⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
      [(⟨1⟩, .scalar (.bool true)), (⟨0⟩, .scalar (.int .u8 0))]
    (SSA.Execution (language := s0Core)).step ssa s0 s1 ∧
    ∀ next, (SSA.Execution (language := s0Core)).step ssa s0 next → next = s1 := by
  intro ssa s0 s1
  let mid0 := mkRegState profile 0 0 [⟨0⟩] [] []
  let mid1 := mkRegState profile 0 0 [⟨0⟩] [⟨0⟩] [(⟨0⟩, .scalar (.int .u8 0))]
  let mid2 := mkRegState profile 0 0 [⟨1⟩, ⟨0⟩] [⟨0⟩] [(⟨0⟩, .scalar (.int .u8 0))]
  have hStep : (ssa.memoryView (language := s0Core)).InstructionStepWith s0 s1 false true
      (.addWithOverflow .u8 ⟨⟨⟨0⟩, []⟩, .virtual⟩ ⟨⟨⟨1⟩, []⟩, .virtual⟩
        (.literal ⟨[255]⟩) (.literal ⟨[1]⟩)) false := by
    refine ⟨rfl, rfl, Or.inl (by decide),
      .scalar (.int .u8 255), s0, .scalar (.int .u8 1), s0, 255, 1,
      { s1 with instruction := 0 }, by with_unfolding_all rfl, by with_unfolding_all rfl,
      rfl, rfl, ?_, rfl⟩
    apply Memory.WriteParams.cons (started := mid0) (stored := mid1)
    · exact ⟨List.not_mem_nil, Or.inl ⟨rfl, rfl⟩⟩
    · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun,
        by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := mid2) (stored := { s1 with instruction := 0 })
      · exact ⟨by simp [mid1, mkRegState], Or.inl ⟨rfl, rfl⟩⟩
      · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun,
          by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
      · exact .nil _
  constructor
  · refine ⟨by with_unfolding_all rfl, ?_, ⟨_, by with_unfolding_all rfl⟩⟩
    exact ⟨ssaAddB0View 255 1, by with_unfolding_all rfl, hStep⟩
  · intro next ⟨_, hst, _⟩
    change (ssa.memoryView (language := s0Core)).Step s0 next true at hst
    simp only [Memory.Step, s0, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hinst⟩ := hst
      have hblkEq : blk = ssaAddB0View 255 1 := by
        with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      exact absurd hinst (Memory.addWithOverflow_virtual_cannot_exhaust
        (by with_unfolding_all rfl) (by with_unfolding_all rfl))
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hinst⟩ := hst
      have hblkEq : blk = ssaAddB0View 255 1 := by
        with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, _, _, lv, a1, rv, a2, x, y, stored, hl, hr, hlv, hrv, hwp, rfl⟩ := hinst
      have hlEq : (lv, a1) = (.scalar (.int .u8 255), s0) := by with_unfolding_all exact Option.some.inj hl.symm
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj hlEq
      have hrEq : (rv, a2) = (.scalar (.int .u8 1), s0) := by with_unfolding_all exact Option.some.inj hr.symm
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj hrEq
      have hx : x = 255 := by cases hlv; rfl
      have hy : y = 1 := by cases hrv; rfl
      subst hx; subst hy
      cases hwp with
      | cons hbeg1 hw1 hwp2 =>
        rename_i started1 stored1
        rcases hbeg1.2 with ⟨_, hEq1⟩ | ⟨hMat, _⟩
        · rw [hEq1] at hw1
          have hstor1 : stored1 = mid1 := by with_unfolding_all exact Option.some.inj hw1.2.2.1.symm
          rw [hstor1] at hwp2
          cases hwp2 with
          | cons hbeg2 hw2 hwp3 =>
            rename_i started2 stored2
            rcases hbeg2.2 with ⟨_, hEq2⟩ | ⟨hMat, _⟩
            · rw [hEq2] at hw2
              have hstor2 : stored2 = { s1 with instruction := 0 } := by
                with_unfolding_all exact Option.some.inj hw2.2.2.1.symm
              rw [hstor2] at hwp3
              cases hwp3
              rfl
            · exact absurd hMat (by with_unfolding_all intro h; cases h)
        · exact absurd hMat (by with_unfolding_all intro h; cases h)

private theorem ssaAdd_20_22_chain (profile : Profile) :
    let ssa := ssaAdd profile 20 22
    let s0 := mkRegState profile 0 0 [] [] []
    let s2 := mkRegState profile 2 0 [⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨2⟩, ⟨1⟩, ⟨0⟩]
      [(⟨2⟩, .scalar (.int .u8 42)), (⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 42))]
    DetChain ((SSA.Execution (language := s0Core)).step ssa) s0 s2 ∧
    (∀ o, (SSA.Execution (language := s0Core)).result ssa s2 o ↔ o = retObs 42) := by
  intro ssa s0 s2
  let s1 := mkRegState profile 0 1 [⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
    [(⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 42))]
  let b2View : MemoryBlock s0Lang :=
    ⟨⟨2⟩, [⟨u8Ty, ⟨⟨⟨2⟩, []⟩, .virtual⟩⟩], [],
      .ret (some ⟨u8Ty, .copy ⟨⟨⟨2⟩, []⟩, .virtual⟩⟩), []⟩
  obtain ⟨h01, h01u⟩ := ssaAdd_20_22_step0 profile
  have h12 : (SSA.Execution (language := s0Core)).step ssa s1 s2 := by
    refine ⟨by with_unfolding_all rfl, ?_, ⟨_, by with_unfolding_all rfl⟩⟩
    refine ⟨ssaAddB0View 20 22, by with_unfolding_all rfl, rfl,
      .scalar (.bool false), s1, false, by with_unfolding_all rfl, rfl,
      [.scalar (.int .u8 42)], s1, b2View, by with_unfolding_all rfl, by with_unfolding_all rfl, rfl, ?_⟩
    apply Memory.WriteParams.cons
      (started := mkRegState profile 2 0 [⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
        [(⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 42))])
      (stored := s2)
    · exact ⟨by simp [s1, mkRegState], Or.inl ⟨rfl, rfl⟩⟩
    · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun,
        by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
    · exact .nil _
  have h12u : ∀ next, (SSA.Execution (language := s0Core)).step ssa s1 next → next = s2 := by
    intro next ⟨_, hst, _⟩
    change (ssa.memoryView (language := s0Core)).Step s1 next true at hst
    simp only [Memory.Step, s1, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hrest⟩ := hst
      have hblkEq : blk = ssaAddB0View 20 22 := by
        with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, val, ar, fl, hrd, htst, vs, ar2, tgt, hro, hba, _, hpo⟩ := hrest
      have hrdEq : (val, ar) = (.scalar (.bool false), s1) := by
        with_unfolding_all exact Option.some.inj hrd.symm
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj hrdEq
      have hfl : fl = false := by cases htst; rfl
      subst hfl
      have hroEq : (vs, ar2) = ([.scalar (.int .u8 42)], s1) := by
        with_unfolding_all exact Option.some.inj hro.symm
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj hroEq
      have htgtEq : tgt = b2View := by with_unfolding_all exact Option.some.inj hba.symm
      subst htgtEq
      exact (hpo.2.2 (by with_unfolding_all exact trivial)).elim
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hrest⟩ := hst
      have hblkEq : blk = ssaAddB0View 20 22 := by
        with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, val, ar, fl, hrd, htst, vs, ar2, tgt, hro, hba, _, hwp⟩ := hrest
      have hrdEq : (val, ar) = (.scalar (.bool false), s1) := by
        with_unfolding_all exact Option.some.inj hrd.symm
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj hrdEq
      have hfl : fl = false := by cases htst; rfl
      subst hfl
      have hroEq : (vs, ar2) = ([.scalar (.int .u8 42)], s1) := by
        with_unfolding_all exact Option.some.inj hro.symm
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj hroEq
      have htgtEq : tgt = b2View := by
        with_unfolding_all exact Option.some.inj hba.symm
      subst htgtEq
      cases hwp with
      | cons hbeg hw hnil =>
        rename_i started' stored'
        rcases hbeg.2 with ⟨_, hEq⟩ | ⟨hMat, _⟩
        · rw [hEq] at hw
          have hstor' : stored' = s2 := by with_unfolding_all exact Option.some.inj hw.2.2.1.symm
          exact (writeParams_nil_eq hnil).trans hstor'
        · exact absurd hMat (by with_unfolding_all intro h; cases h)
  refine ⟨.cons h01 h01u (.cons h12 h12u (.nil s2)), fun o => ?_⟩
  have hr : (SSA.Execution (language := s0Core)).result ssa s2 (retObs 42) := by
    with_unfolding_all rfl
  exact ⟨fun h => (Option.some.inj (hr.symm.trans h)).symm, fun h => h ▸ hr⟩

private theorem ssaAdd_255_1_chain (profile : Profile) :
    let ssa := ssaAdd profile 255 1
    let s0 := mkRegState profile 0 0 [] [] []
    let s2 := mkRegState profile 1 0 [⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
      [(⟨1⟩, .scalar (.bool true)), (⟨0⟩, .scalar (.int .u8 0))]
    DetChain ((SSA.Execution (language := s0Core)).step ssa) s0 s2 ∧
    (∀ o, (SSA.Execution (language := s0Core)).result ssa s2 o ↔ o = trapObs0) := by
  intro ssa s0 s2
  let s1 := mkRegState profile 0 1 [⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
    [(⟨1⟩, .scalar (.bool true)), (⟨0⟩, .scalar (.int .u8 0))]
  let b1View : MemoryBlock s0Lang := ⟨⟨1⟩, [], [], .trap .overflow orig0, []⟩
  obtain ⟨h01, h01u⟩ := ssaAdd_255_1_step0 profile
  have h12 : (SSA.Execution (language := s0Core)).step ssa s1 s2 := by
    refine ⟨by with_unfolding_all rfl, ?_, ⟨_, by with_unfolding_all rfl⟩⟩
    exact ⟨ssaAddB0View 255 1, by with_unfolding_all rfl, rfl,
      .scalar (.bool true), s1, true, by with_unfolding_all rfl, rfl,
      [], s1, b1View, by with_unfolding_all rfl, by with_unfolding_all rfl, rfl, .nil _⟩
  have h12u : ∀ next, (SSA.Execution (language := s0Core)).step ssa s1 next → next = s2 := by
    intro next ⟨_, hst, _⟩
    change (ssa.memoryView (language := s0Core)).Step s1 next true at hst
    simp only [Memory.Step, s1, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hrest⟩ := hst
      have hblkEq : blk = ssaAddB0View 255 1 := by
        with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, val, ar, fl, hrd, htst, vs, ar2, tgt, hro, hba, _, hpo⟩ := hrest
      have hrdEq : (val, ar) = (.scalar (.bool true), s1) := by
        with_unfolding_all exact Option.some.inj hrd.symm
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj hrdEq
      have hfl : fl = true := by cases htst; rfl
      subst hfl
      have hroEq : (vs, ar2) = ([], s1) := by
        with_unfolding_all exact Option.some.inj hro.symm
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj hroEq
      have htgtEq : tgt = b1View := by with_unfolding_all exact Option.some.inj hba.symm
      subst htgtEq
      exact (hpo.2.2 (by with_unfolding_all exact trivial)).elim
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hrest⟩ := hst
      have hblkEq : blk = ssaAddB0View 255 1 := by
        with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, val, ar, fl, hrd, htst, vs, ar2, tgt, hro, hba, _, hwp⟩ := hrest
      have hrdEq : (val, ar) = (.scalar (.bool true), s1) := by
        with_unfolding_all exact Option.some.inj hrd.symm
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj hrdEq
      have hfl : fl = true := by cases htst; rfl
      subst hfl
      have hroEq : (vs, ar2) = ([], s1) := by
        with_unfolding_all exact Option.some.inj hro.symm
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj hroEq
      have htgtEq : tgt = b1View := by
        with_unfolding_all exact Option.some.inj hba.symm
      subst htgtEq
      cases hwp
      rfl
  refine ⟨.cons h01 h01u (.cons h12 h12u (.nil s2)), fun o => ?_⟩
  have hr : (SSA.Execution (language := s0Core)).result ssa s2 trapObs0 := by
    with_unfolding_all rfl
  exact ⟨fun h => (Option.some.inj (hr.symm.trans h)).symm, fun h => h ▸ hr⟩

private theorem memAdd_20_22_chain (profile : Profile) :
    let mem := memAdd profile 20 22
    let m0 := mkRegState profile 0 0 [] [] []
    let m2 := mkRegState profile 0 2 [⟨0⟩] [⟨0⟩] [(⟨0⟩, .scalar (.int .u8 42))]
    DetChain ((Memory.Execution (language := s0Core)).step mem) m0 m2 ∧
    (∀ o, (Memory.Execution (language := s0Core)).result mem m2 o ↔ o = retObs 42) := by
  intro mem m0 m2
  let m1 := mkRegState profile 0 1 [⟨0⟩] [] []
  have h01 : (Memory.Execution (language := s0Core)).step mem m0 m1 := by
    refine ⟨by with_unfolding_all rfl, ?_, ⟨_, by with_unfolding_all rfl⟩⟩
    exact ⟨memAddBlock 20 22, by with_unfolding_all rfl, { m1 with instruction := 0 },
      ⟨List.not_mem_nil, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩
  have h01u : ∀ next, (Memory.Execution (language := s0Core)).step mem m0 next → next = m1 := by
    intro next ⟨_, hst, _⟩
    change mem.Step m0 next false at hst
    simp only [Memory.Step, m0, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hrest⟩ := hst
      have hblkEq : blk = memAddBlock 20 22 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      exact absurd hrest.materialized (by with_unfolding_all intro h; cases h)
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hrest⟩ := hst
      have hblkEq : blk = memAddBlock 20 22 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨started, hbeg, rfl⟩ := hrest
      rcases hbeg.2 with ⟨_, rfl⟩ | ⟨hMat, _⟩
      · rfl
      · exact absurd hMat (by with_unfolding_all intro h; cases h)
  have h12 : (Memory.Execution (language := s0Core)).step mem m1 m2 := by
    refine ⟨by with_unfolding_all rfl, ?_, ⟨_, by with_unfolding_all rfl⟩⟩
    refine ⟨memAddBlock 20 22, by with_unfolding_all rfl,
      [.scalar (.int .u8 20), .scalar (.int .u8 22)], m1, [], .scalar (.int .u8 42), [],
      ⟨Mem.Memory.empty, {}, []⟩, { m2 with instruction := 1 },
      by with_unfolding_all rfl, by with_unfolding_all rfl, rfl,
      ⟨rfl, by with_unfolding_all rfl, rfl, rfl⟩, rfl, ?_, rfl⟩
    exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun,
      by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
  have h12u : ∀ next, (Memory.Execution (language := s0Core)).step mem m1 next → next = m2 := by
    intro next ⟨_, hst, _⟩
    change mem.Step m1 next false at hst
    simp only [Memory.Step, m1, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hrest⟩ := hst
      have hblkEq : blk = memAddBlock 20 22 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, _, _, _, _, _, _, _, _, _, _, _, hF, _⟩ := hrest
      exact hF.elim
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hrest⟩ := hst
      have hblkEq : blk = memAddBlock 20 22 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨vs, ar, ops, res, effs, chg, stor, hra, _, _, hop, _, hw, rfl⟩ := hrest
      have hraEq : (vs, ar) = ([.scalar (.int .u8 20), .scalar (.int .u8 22)], m1) := by
        with_unfolding_all exact Option.some.inj hra.symm
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj hraEq
      have hresEq : res = .scalar (.int .u8 42) := by
        have h := hop.2.1; with_unfolding_all exact (Res.ok.inj h).symm
      have heffs : effs = [] := hop.2.2.1
      have hchg : chg = ⟨Mem.Memory.empty, {}, []⟩ := hop.2.2.2.symm
      subst hresEq; subst heffs; subst hchg
      have hstor : stor = { m2 with instruction := 1 } := by
        with_unfolding_all exact Option.some.inj hw.2.2.1.symm
      subst hstor; rfl
  refine ⟨.cons h01 h01u (.cons h12 h12u (.nil m2)), fun o => ?_⟩
  have hr : (Memory.Execution (language := s0Core)).result mem m2 (retObs 42) := by
    with_unfolding_all rfl
  exact ⟨fun h => (Option.some.inj (hr.symm.trans h)).symm, fun h => h ▸ hr⟩

private theorem memAdd_255_1_chain (profile : Profile) :
    let mem := memAdd profile 255 1
    let m0 := mkRegState profile 0 0 [] [] []
    let m1 := mkRegState profile 0 1 [⟨0⟩] [] []
    DetChain ((Memory.Execution (language := s0Core)).step mem) m0 m1 ∧
    (∀ o, (Memory.Execution (language := s0Core)).result mem m1 o ↔ o = trapObs0) := by
  intro mem m0 m1
  have h01 : (Memory.Execution (language := s0Core)).step mem m0 m1 := by
    refine ⟨by with_unfolding_all rfl, ?_, ⟨_, by with_unfolding_all rfl⟩⟩
    exact ⟨memAddBlock 255 1, by with_unfolding_all rfl, { m1 with instruction := 0 },
      ⟨List.not_mem_nil, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩
  have h01u : ∀ next, (Memory.Execution (language := s0Core)).step mem m0 next → next = m1 := by
    intro next ⟨_, hst, _⟩
    change mem.Step m0 next false at hst
    simp only [Memory.Step, m0, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hrest⟩ := hst
      have hblkEq : blk = memAddBlock 255 1 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      exact absurd hrest.materialized (by with_unfolding_all intro h; cases h)
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hrest⟩ := hst
      have hblkEq : blk = memAddBlock 255 1 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨started, hbeg, rfl⟩ := hrest
      rcases hbeg.2 with ⟨_, rfl⟩ | ⟨hMat, _⟩
      · rfl
      · exact absurd hMat (by with_unfolding_all intro h; cases h)
  refine ⟨.cons h01 h01u (.nil m1), fun o => ?_⟩
  have hr : (Memory.Execution (language := s0Core)).result mem m1 trapObs0 := by
    with_unfolding_all rfl
  exact ⟨fun h => (Option.some.inj (hr.symm.trans h)).symm, fun h => h ▸ hr⟩

private theorem residualAddsMatch_memAdd_ssaAdd (profile : Profile) (a b : UInt8) :
    ResidualAddsMatch (memAdd profile a b) (ssaAdd profile a b) := by
  let mGraph : MemoryGraph s0Lang := ⟨⟨0⟩, #[memAddBlock a b]⟩
  let sGraph : SSAGraph s0Lang := ⟨⟨0⟩, #[ssaAddB0 a b, ssaAddB1, ssaAddB2]⟩
  let b0 := ssaAddB0 a b
  let b1 := ssaAddB1
  let b2 := ssaAddB2
  have hLowers : sGraph.LowersFrom mGraph := by
    refine ⟨id, fun _ => SSAEnv.empty,
      fun _ => [(b0, ⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[a]⟩) (.literal ⟨[b]⟩)⟩)],
      rfl, ?_, ?_⟩
    · intro mb hmb
      rcases List.mem_singleton.mp hmb with rfl
      refine ⟨b0, List.mem_cons_self, rfl, fun _ => ?_, fun hne => (hne rfl).elim⟩
      refine ⟨rfl, rfl, [], b0.body, .nil, rfl, ?_⟩
      apply SSAGraph.InstsLower.beginLifetime (sPrefix := []) (Or.inl rfl)
      have hGuard : sGraph.BlockGuardsAddWithOverflow b0 .u8 orig0 ⟨0⟩ ⟨1⟩
          (.literal ⟨[a]⟩) (.literal ⟨[b]⟩) ⟨⟨2⟩, [⟨u8Ty, .value ⟨0⟩⟩]⟩ b2 ⟨2⟩ :=
        ⟨by decide, rfl, ⟨⟨1⟩, []⟩, b1, rfl, rfl,
          List.mem_cons_of_mem _ List.mem_cons_self, rfl, rfl, rfl, rfl,
          List.mem_cons_of_mem _ (List.mem_cons_of_mem _ List.mem_cons_self), rfl, by decide,
          rfl, 0, rfl, rfl⟩
      have hEdge : EdgeTransfersEnv (SSAEnv.empty.set ⟨0⟩ ⟨u8Ty, .value ⟨0⟩⟩)
          ⟨⟨2⟩, [⟨u8Ty, .value ⟨0⟩⟩]⟩ b2 (SSAEnv.empty.set ⟨0⟩ ⟨u8Ty, .value ⟨2⟩⟩) := by
        refine ⟨rfl, rfl, fun root ty targetRead hlook => ?_⟩
        unfold SSAEnv.set SSAEnv.empty at hlook
        split at hlook
        · rename_i heq; subst heq
          cases Option.some.inj hlook
          exact Or.inl ⟨0, ⟨2⟩, .value ⟨0⟩, rfl, rfl, rfl, by simp [SSAEnv.set]⟩
        · cases hlook
      refine SSAGraph.InstsLower.residualAdd
        (mDst := memPlace0) (mLhs := .literal ⟨[a]⟩) (mRhs := .literal ⟨[b]⟩)
        (opLoads := []) (contStores := []) (restContBody := [])
        (.mk ⟨⟨0, .overflow, .residual⟩, List.mem_cons_self, rfl, rfl⟩)
        trivial rfl rfl hGuard ⟨rfl, rfl, hEdge, by simp [SSAEnv.set, memPlace0], rfl⟩ rfl ?_
      exact .nil ⟨.value ⟨2⟩, trivial, ⟨rfl, by simp [SSAEnv.set, memPlace0]⟩, rfl⟩
    · intro sb hsb sop hsop
      rcases List.mem_cons.mp hsb with rfl | hrest
      · rcases List.mem_singleton.mp hsop with rfl
        exact ⟨memAddBlock a b, List.mem_cons_self, List.mem_cons_self⟩
      · rcases List.mem_cons.mp hrest with rfl | hrest2
        · cases hsop
        · rcases List.mem_singleton.mp hrest2 with rfl; cases hsop
  refine ⟨?_, ?_⟩
  · intro fn ml mg hfn _
    rcases Prod.mk.inj (List.mem_singleton.mp hfn) with ⟨rfl, hrest⟩
    rcases Prod.mk.inj hrest with ⟨rfl, rfl⟩
    refine ⟨[], sGraph, List.mem_cons_self, hLowers, ?_⟩
    intro _ _ _ _ _ sb hsb sop hsop _
    rcases List.mem_cons.mp hsb with rfl | hrest
    · rcases List.mem_singleton.mp hsop with rfl; trivial
    · rcases List.mem_cons.mp hrest with rfl | hrest2
      · cases hsop
      · rcases List.mem_singleton.mp hrest2 with rfl; cases hsop
  · intro fn sl sg hfn sb hsb sop hsop
    rcases Prod.mk.inj (List.mem_singleton.mp hfn) with ⟨rfl, hrest⟩
    rcases Prod.mk.inj hrest with ⟨rfl, rfl⟩
    rcases List.mem_cons.mp hsb with rfl | hrest
    · rcases List.mem_singleton.mp hsop with rfl
      refine ⟨by decide, [(⟨0⟩, u8Ty)], mGraph, List.mem_cons_self,
        Or.inl ⟨⟨memAddBlock a b, List.mem_cons_self, _,
          List.mem_cons_of_mem _ List.mem_cons_self,
          ⟨⟨0, .overflow, .residual⟩, List.mem_cons_self, rfl, rfl⟩⟩, hLowers⟩⟩
    · rcases List.mem_cons.mp hrest with rfl | hrest2
      · cases hsop
      · rcases List.mem_singleton.mp hrest2 with rfl; cases hsop

private theorem preservesOrigins_memAdd_ssaAdd (profile : Profile) (a b : UInt8) :
    PreservesOrigins (memAdd profile a b).origins (ssaAdd profile a b).origins := by
  have hM : (memAdd profile a b).origins = [orig0, orig0] := by with_unfolding_all rfl
  have hS : (ssaAdd profile a b).origins = [orig0, orig0] := by with_unfolding_all rfl
  rw [hM, hS]
  exact ⟨fun _ h => h, fun _ h => h⟩

private theorem buildsBIR_ssaAdd_birAdd (profile : Profile) (hSlots : 3 ≤ profile.maxBIRSlots) (a b : UInt8) :
    BuildsBIR (language := s0Core) (ssaAdd profile a b) (birAdd profile a b) := by
  have hView : (ssaAdd profile a b).memoryView (language := s0Core) =
      (birAdd profile a b).memoryView (language := s0Core) := by
    with_unfolding_all rfl
  refine ⟨rfl, ?_, ?_, ⟨fun _ h => h, fun _ h => h⟩, hSlots,
    ⟨rfl, rfl, congrArg (fun m => m.graphAt ctrlMain) hView⟩,
    BIR.refinesOfViewEq hView⟩
  · intro fn hfn blk hblk slot htrap
    rcases List.mem_singleton.mp hfn with rfl
    rcases List.mem_cons.mp hblk with rfl | hrest
    · cases htrap
    · rcases List.mem_cons.mp hrest with rfl | hrest2
      · cases BIRTerm.trap.inj htrap; exact ⟨⟨.overflow, orig0⟩, rfl⟩
      · rcases List.mem_singleton.mp hrest2 with rfl; cases htrap
  · intro fn hfn blk hblk kind orig htrap
    rcases List.mem_singleton.mp hfn with rfl
    rcases List.mem_cons.mp hblk with rfl | hrest
    · cases htrap
    · rcases List.mem_cons.mp hrest with rfl | hrest2
      · rcases Terminator.trap.inj htrap with ⟨rfl, rfl⟩
        exact ⟨_, List.mem_cons_self, rfl, _, List.mem_cons_of_mem _ List.mem_cons_self, rfl,
          ⟨0⟩, rfl, ⟨.overflow, orig0⟩, rfl, rfl, rfl⟩
      · rcases List.mem_singleton.mp hrest2 with rfl; cases htrap

private theorem bir_observes_of_ssa_view {profile : Profile} {ssa : SSA s0Lang profile}
    {bir : BIR s0Lang profile} {obs : Concrete.Contracts.Observation}
    (hView : ssa.memoryView (language := s0Core) = bir.memoryView (language := s0Core))
    (h : (SSA.Execution (language := s0Core)).observes ssa obs) :
    (BIR.Execution (language := s0Core)).observes bir obs := by
  have hStep : ∀ {a b},
      Steps ((SSA.Execution (language := s0Core)).step ssa) a b →
      Steps ((BIR.Execution (language := s0Core)).step bir) a b := by
    intro a b hs
    induction hs with
    | nil => exact .nil _
    | cons h1 _ ih =>
      refine .cons ?_ ih
      change (BIR.Execution (language := s0Core)).step bir _ _
      unfold BIR.Execution Semantics.onInput; dsimp only
      unfold SSA.Execution Semantics.onInput at h1; dsimp only at h1
      rw [← hView]; exact h1
  obtain ⟨q, ⟨q0, h0, hst⟩, hr⟩ := h
  refine ⟨q, ⟨q0, ?_, hStep hst⟩, ?_⟩
  · unfold BIR.Execution Semantics.onInput; dsimp only
    unfold SSA.Execution Semantics.onInput at h0; dsimp only at h0
    rw [← hView]; exact h0
  · unfold BIR.Execution Semantics.onInput; dsimp only
    unfold SSA.Execution Semantics.onInput at hr; dsimp only at hr
    rw [← hView]; exact hr

/-- Control 1a (`20 + 22 → 42`): end-to-end execution across Memory/SSA/BIR,
`ResidualAddsMatch`, `BuildsSSA`, and `BuildsBIR`. -/
theorem checkedAdd_20_22_returns_42 :
    let p := lcbProfile
    (Memory.Execution (language := s0Core)).observes (memAdd p 20 22) (retObs 42) ∧
    (SSA.Execution (language := s0Core)).observes (ssaAdd p 20 22) (retObs 42) ∧
    (BIR.Execution (language := s0Core)).observes (birAdd p 20 22) (retObs 42) ∧
    ResidualAddsMatch (memAdd p 20 22) (ssaAdd p 20 22) ∧
    BuildsSSA (language := s0Core) (memAdd p 20 22) (ssaAdd p 20 22) ∧
    BuildsBIR (language := s0Core) (ssaAdd p 20 22) (birAdd p 20 22) := by
  let p := lcbProfile
  obtain ⟨hmC, hmEnd⟩ := memAdd_20_22_chain p
  obtain ⟨hsC, hsEnd⟩ := ssaAdd_20_22_chain p
  have hView : (ssaAdd p 20 22).memoryView (language := s0Core) =
      (birAdd p 20 22).memoryView (language := s0Core) := by with_unfolding_all rfl
  have hmObs : (Memory.Execution (language := s0Core)).observes (memAdd p 20 22) (retObs 42) :=
    (hmC.observes_iff (by with_unfolding_all rfl) (fun _ h => h) hmEnd (retObs 42)).mpr rfl
  have hsObs : (SSA.Execution (language := s0Core)).observes (ssaAdd p 20 22) (retObs 42) :=
    (hsC.observes_iff (by with_unfolding_all rfl) (fun _ h => by with_unfolding_all exact h) hsEnd (retObs 42)).mpr rfl
  have hbObs : (BIR.Execution (language := s0Core)).observes (birAdd p 20 22) (retObs 42) :=
    bir_observes_of_ssa_view hView hsObs
  have hRes := residualAddsMatch_memAdd_ssaAdd p 20 22
  have hRef : BehaviorRefinement (Memory.Execution (language := s0Core)) (SSA.Execution (language := s0Core))
      (Memory.Execution (language := s0Core)).result (SSA.Execution (language := s0Core)).result
      (memAdd p 20 22) (ssaAdd p 20 22) :=
    hsC.refines ⟨_, by with_unfolding_all rfl, hmC.toSteps⟩ ((hmEnd _).mpr rfl)
      (fun _ h => by with_unfolding_all exact h) hsEnd (by with_unfolding_all rfl)
  exact ⟨hmObs, hsObs, hbObs, hRes,
    ⟨rfl, rfl, rfl, ⟨rfl, rfl⟩, preservesOrigins_memAdd_ssaAdd p 20 22, hRes, hRef⟩,
    buildsBIR_ssaAdd_birAdd p (by decide) 20 22⟩

/-- Control 1b (`255 + 1 → status 134` with exact stderr): end-to-end execution across Memory/SSA/BIR,
`ResidualAddsMatch`, `BuildsSSA`, and `BuildsBIR`. -/
theorem checkedAdd_255_1_traps_134_exact_stderr :
    let p := lcbProfile
    (Memory.Execution (language := s0Core)).observes (memAdd p 255 1) trapObs0 ∧
    (SSA.Execution (language := s0Core)).observes (ssaAdd p 255 1) trapObs0 ∧
    (BIR.Execution (language := s0Core)).observes (birAdd p 255 1) trapObs0 ∧
    trapObs0.stderr = trapLine0.toUTF8 ∧
    ResidualAddsMatch (memAdd p 255 1) (ssaAdd p 255 1) ∧
    BuildsSSA (language := s0Core) (memAdd p 255 1) (ssaAdd p 255 1) ∧
    BuildsBIR (language := s0Core) (ssaAdd p 255 1) (birAdd p 255 1) := by
  let p := lcbProfile
  obtain ⟨hmC, hmEnd⟩ := memAdd_255_1_chain p
  obtain ⟨hsC, hsEnd⟩ := ssaAdd_255_1_chain p
  have hView : (ssaAdd p 255 1).memoryView (language := s0Core) =
      (birAdd p 255 1).memoryView (language := s0Core) := by with_unfolding_all rfl
  have hmObs := (hmC.observes_iff (by with_unfolding_all rfl) (fun _ h => h) hmEnd trapObs0).mpr rfl
  have hsObs := (hsC.observes_iff (by with_unfolding_all rfl) (fun _ h => by with_unfolding_all exact h) hsEnd trapObs0).mpr rfl
  have hbObs : (BIR.Execution (language := s0Core)).observes (birAdd p 255 1) trapObs0 :=
    bir_observes_of_ssa_view hView hsObs
  have hRes := residualAddsMatch_memAdd_ssaAdd p 255 1
  have hRef : BehaviorRefinement (Memory.Execution (language := s0Core)) (SSA.Execution (language := s0Core))
      (Memory.Execution (language := s0Core)).result (SSA.Execution (language := s0Core)).result
      (memAdd p 255 1) (ssaAdd p 255 1) :=
    hsC.refines ⟨_, by with_unfolding_all rfl, hmC.toSteps⟩ ((hmEnd _).mpr rfl)
      (fun _ h => by with_unfolding_all exact h) hsEnd (by with_unfolding_all rfl)
  exact ⟨hmObs, hsObs, hbObs, rfl, hRes,
    ⟨rfl, rfl, rfl, ⟨rfl, rfl⟩, preservesOrigins_memAdd_ssaAdd p 255 1, hRes, hRef⟩,
    buildsBIR_ssaAdd_birAdd p (by decide) 255 1⟩

/-! ## Control 1c: Two-site checked addition (first-site trap vs second-site trap vs success) -/

/-- Two sequential guarded `addWithOverflow` sites (`orig0` for `a + b`, `orig1` for `(a + b) + c`). -/
private def birTwoSite (profile : Profile) (a b c : UInt8) : BIR s0Lang profile :=
  let b0 : Block _ _ _ :=
    ⟨⟨0⟩, [],
      [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[a]⟩) (.literal ⟨[b]⟩)⟩],
      .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, [⟨u8Ty, .value ⟨0⟩⟩]⟩, []⟩
  let b1 : Block _ _ _ := ⟨⟨1⟩, [], [], .trap ⟨0⟩, []⟩
  let b2 : Block _ _ _ :=
    ⟨⟨2⟩, [⟨u8Ty, ⟨2⟩⟩],
      [⟨orig1, .addWithOverflow .u8 ⟨3⟩ ⟨4⟩ (.value ⟨2⟩) (.literal ⟨[c]⟩)⟩],
      .branch (.value ⟨4⟩) ⟨⟨3⟩, []⟩ ⟨⟨4⟩, [⟨u8Ty, .value ⟨3⟩⟩]⟩, []⟩
  let b3 : Block _ _ _ := ⟨⟨3⟩, [], [], .trap ⟨1⟩, []⟩
  let b4 : Block _ _ _ := ⟨⟨4⟩, [⟨u8Ty, ⟨5⟩⟩], [], .ret (some ⟨u8Ty, .value ⟨5⟩⟩), []⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2, b3, b4]⟩)], [],
    #[⟨.overflow, orig0⟩, ⟨.overflow, orig1⟩], [], []⟩

private def twoSiteB1View : MemoryBlock s0Lang :=
  ⟨⟨1⟩, [], [], .trap .overflow orig0, []⟩

private def twoSiteB2View (c : UInt8) : MemoryBlock s0Lang :=
  ⟨⟨2⟩, [⟨u8Ty, ⟨⟨⟨2⟩, []⟩, .virtual⟩⟩],
    [⟨orig1, .addWithOverflow .u8 ⟨⟨⟨3⟩, []⟩, .virtual⟩ ⟨⟨⟨4⟩, []⟩, .virtual⟩
      (.copy ⟨⟨⟨2⟩, []⟩, .virtual⟩) (.literal ⟨[c]⟩)⟩],
    .branch (.copy ⟨⟨⟨4⟩, []⟩, .virtual⟩) ⟨⟨3⟩, []⟩
      ⟨⟨4⟩, [⟨u8Ty, .copy ⟨⟨⟨3⟩, []⟩, .virtual⟩⟩]⟩, []⟩

private def twoSiteB3View : MemoryBlock s0Lang :=
  ⟨⟨3⟩, [], [], .trap .overflow orig1, []⟩

private def twoSiteB4View : MemoryBlock s0Lang :=
  ⟨⟨4⟩, [⟨u8Ty, ⟨⟨⟨5⟩, []⟩, .virtual⟩⟩], [],
    .ret (some ⟨u8Ty, .copy ⟨⟨⟨5⟩, []⟩, .virtual⟩⟩), []⟩

private theorem birTwoSite_first_chain :
    let p := lcbProfile
    let firstTraps := birTwoSite p 255 1 250
    let s0 := mkRegState p 0 0 [] [] []
    let s2 := mkRegState p 1 0 [⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
      [(⟨1⟩, .scalar (.bool true)), (⟨0⟩, .scalar (.int .u8 0))]
    DetChain ((BIR.Execution (language := s0Core)).step firstTraps) s0 s2 ∧
    (∀ o, (BIR.Execution (language := s0Core)).result firstTraps s2 o ↔ o = trapObs0) := by
  intro p firstTraps s0 s2
  let s1 := mkRegState p 0 1 [⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
    [(⟨1⟩, .scalar (.bool true)), (⟨0⟩, .scalar (.int .u8 0))]
  let mid0 := mkRegState p 0 0 [⟨0⟩] [] []
  let mid1 := mkRegState p 0 0 [⟨0⟩] [⟨0⟩] [(⟨0⟩, .scalar (.int .u8 0))]
  let mid2 := mkRegState p 0 0 [⟨1⟩, ⟨0⟩] [⟨0⟩] [(⟨0⟩, .scalar (.int .u8 0))]
  have step01 : (BIR.Execution (language := s0Core)).step firstTraps s0 s1 := by
    refine ⟨by with_unfolding_all rfl, ⟨ssaAddB0View 255 1, by with_unfolding_all rfl, ?_⟩,
      ⟨_, by with_unfolding_all rfl⟩⟩
    refine ⟨rfl, rfl, Or.inl (by decide),
      .scalar (.int .u8 255), s0, .scalar (.int .u8 1), s0, 255, 1,
      { s1 with instruction := 0 }, by with_unfolding_all rfl, by with_unfolding_all rfl, rfl, rfl, ?_, rfl⟩
    apply Memory.WriteParams.cons (started := mid0) (stored := mid1)
    · exact ⟨List.not_mem_nil, Or.inl ⟨rfl, rfl⟩⟩
    · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun, by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := mid2) (stored := { s1 with instruction := 0 })
      · exact ⟨by simp [mid1, mkRegState], Or.inl ⟨rfl, rfl⟩⟩
      · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun, by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
      · exact .nil _
  have step01u : ∀ next, (BIR.Execution (language := s0Core)).step firstTraps s0 next → next = s1 := by
    intro next ⟨_, hst, _⟩
    change (firstTraps.memoryView (language := s0Core)).Step s0 next true at hst
    simp only [Memory.Step, s0, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hinst⟩ := hst
      have hblkEq : blk = ssaAddB0View 255 1 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      exact absurd hinst (Memory.addWithOverflow_virtual_cannot_exhaust
        (by with_unfolding_all rfl) (by with_unfolding_all rfl))
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hinst⟩ := hst
      have hblkEq : blk = ssaAddB0View 255 1 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, _, _, lv, a1, rv, a2, x, y, stored, hl, hr, hlv, hrv, hwp, rfl⟩ := hinst
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hl.symm : (lv, a1) = (.scalar (.int .u8 255), s0))
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hr.symm : (rv, a2) = (.scalar (.int .u8 1), s0))
      have hx : x = 255 := by cases hlv; rfl
      have hy : y = 1 := by cases hrv; rfl
      subst hx; subst hy
      cases hwp with
      | cons hbeg1 hw1 hwp2 =>
        rename_i started1 stored1
        rcases hbeg1.2 with ⟨_, hEq1⟩ | ⟨hMat, _⟩
        · rw [hEq1] at hw1
          have hstor1 : stored1 = mid1 := by with_unfolding_all exact Option.some.inj hw1.2.2.1.symm
          rw [hstor1] at hwp2
          cases hwp2 with
          | cons hbeg2 hw2 hwp3 =>
            rename_i started2 stored2
            rcases hbeg2.2 with ⟨_, hEq2⟩ | ⟨hMat, _⟩
            · rw [hEq2] at hw2
              have hstor2 : stored2 = { s1 with instruction := 0 } := by
                with_unfolding_all exact Option.some.inj hw2.2.2.1.symm
              rw [hstor2] at hwp3
              cases hwp3; rfl
            · exact absurd hMat (by with_unfolding_all intro h; cases h)
        · exact absurd hMat (by with_unfolding_all intro h; cases h)
  have step12 : (BIR.Execution (language := s0Core)).step firstTraps s1 s2 :=
    ⟨by with_unfolding_all rfl,
      ⟨ssaAddB0View 255 1, by with_unfolding_all rfl, rfl, .scalar (.bool true), s1, true,
        by with_unfolding_all rfl, rfl, [], s1, twoSiteB1View,
        by with_unfolding_all rfl, by with_unfolding_all rfl, rfl, .nil _⟩,
      ⟨_, by with_unfolding_all rfl⟩⟩
  have step12u : ∀ next, (BIR.Execution (language := s0Core)).step firstTraps s1 next → next = s2 := by
    intro next ⟨_, hst, _⟩
    change (firstTraps.memoryView (language := s0Core)).Step s1 next true at hst
    simp only [Memory.Step, s1, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hrest⟩ := hst
      have hblkEq : blk = ssaAddB0View 255 1 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, val, ar, fl, hrd, htst, vs, ar2, tgt, hro, hba, _, hpo⟩ := hrest
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hrd.symm : (val, ar) = (.scalar (.bool true), s1))
      have hfl : fl = true := by cases htst; rfl
      subst hfl
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hro.symm : (vs, ar2) = ([], s1))
      have htgtEq : tgt = twoSiteB1View := by with_unfolding_all exact Option.some.inj hba.symm
      subst htgtEq; exact (hpo.2.2 (by with_unfolding_all exact trivial)).elim
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hrest⟩ := hst
      have hblkEq : blk = ssaAddB0View 255 1 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, val, ar, fl, hrd, htst, vs, ar2, tgt, hro, hba, _, hwp⟩ := hrest
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hrd.symm : (val, ar) = (.scalar (.bool true), s1))
      have hfl : fl = true := by cases htst; rfl
      subst hfl
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hro.symm : (vs, ar2) = ([], s1))
      have htgtEq : tgt = twoSiteB1View := by with_unfolding_all exact Option.some.inj hba.symm
      subst htgtEq; cases hwp; rfl
  refine ⟨.cons step01 step01u (.cons step12 step12u (.nil s2)), fun o => ?_⟩
  have hr : (BIR.Execution (language := s0Core)).result firstTraps s2 trapObs0 := by
    with_unfolding_all rfl
  exact ⟨fun h => (Option.some.inj (hr.symm.trans h)).symm, fun h => h ▸ hr⟩

private theorem birTwoSite_prefix_step0 (c : UInt8) :
    let p := lcbProfile
    let prog := birTwoSite p 20 22 c
    let s0 := mkRegState p 0 0 [] [] []
    let s1 := mkRegState p 0 1 [⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
      [(⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 42))]
    (BIR.Execution (language := s0Core)).step prog s0 s1 ∧
    ∀ next, (BIR.Execution (language := s0Core)).step prog s0 next → next = s1 := by
  intro p prog s0 s1
  let mid0 := mkRegState p 0 0 [⟨0⟩] [] []
  let mid1 := mkRegState p 0 0 [⟨0⟩] [⟨0⟩] [(⟨0⟩, .scalar (.int .u8 42))]
  let mid2 := mkRegState p 0 0 [⟨1⟩, ⟨0⟩] [⟨0⟩] [(⟨0⟩, .scalar (.int .u8 42))]
  have step01 : (BIR.Execution (language := s0Core)).step prog s0 s1 := by
    refine ⟨by with_unfolding_all rfl, ⟨ssaAddB0View 20 22, by with_unfolding_all rfl, ?_⟩,
      ⟨_, by with_unfolding_all rfl⟩⟩
    refine ⟨rfl, rfl, Or.inl (by decide),
      .scalar (.int .u8 20), s0, .scalar (.int .u8 22), s0, 20, 22,
      { s1 with instruction := 0 }, by with_unfolding_all rfl, by with_unfolding_all rfl, rfl, rfl, ?_, rfl⟩
    apply Memory.WriteParams.cons (started := mid0) (stored := mid1)
    · exact ⟨List.not_mem_nil, Or.inl ⟨rfl, rfl⟩⟩
    · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun, by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := mid2) (stored := { s1 with instruction := 0 })
      · exact ⟨by simp [mid1, mkRegState], Or.inl ⟨rfl, rfl⟩⟩
      · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun, by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
      · exact .nil _
  refine ⟨step01, fun next ⟨_, hst, _⟩ => ?_⟩
  change (prog.memoryView (language := s0Core)).Step s0 next true at hst
  simp only [Memory.Step, s0, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
  cases hne : next.resourceExhausted with
  | true =>
    rw [ite_eq_left (by simp [hne])] at hst
    obtain ⟨_, _, blk, hblk, hinst⟩ := hst
    have hblkEq : blk = ssaAddB0View 20 22 := by with_unfolding_all exact Option.some.inj hblk.symm
    subst hblkEq
    exact absurd hinst (Memory.addWithOverflow_virtual_cannot_exhaust
      (by with_unfolding_all rfl) (by with_unfolding_all rfl))
  | false =>
    rw [ite_eq_right (by simp [hne])] at hst
    obtain ⟨blk, hblk, hinst⟩ := hst
    have hblkEq : blk = ssaAddB0View 20 22 := by with_unfolding_all exact Option.some.inj hblk.symm
    subst hblkEq
    obtain ⟨_, _, _, lv, a1, rv, a2, x, y, stored, hl, hr, hlv, hrv, hwp, rfl⟩ := hinst
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hl.symm : (lv, a1) = (.scalar (.int .u8 20), s0))
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hr.symm : (rv, a2) = (.scalar (.int .u8 22), s0))
    have hx : x = 20 := by cases hlv; rfl
    have hy : y = 22 := by cases hrv; rfl
    subst hx; subst hy
    cases hwp with
    | cons hbeg1 hw1 hwp2 =>
      rename_i started1 stored1
      rcases hbeg1.2 with ⟨_, hEq1⟩ | ⟨hMat, _⟩
      · rw [hEq1] at hw1
        have hstor1 : stored1 = mid1 := by with_unfolding_all exact Option.some.inj hw1.2.2.1.symm
        rw [hstor1] at hwp2
        cases hwp2 with
        | cons hbeg2 hw2 hwp3 =>
          rename_i started2 stored2
          rcases hbeg2.2 with ⟨_, hEq2⟩ | ⟨hMat, _⟩
          · rw [hEq2] at hw2
            have hstor2 : stored2 = { s1 with instruction := 0 } := by
              with_unfolding_all exact Option.some.inj hw2.2.2.1.symm
            rw [hstor2] at hwp3
            cases hwp3; rfl
          · exact absurd hMat (by with_unfolding_all intro h; cases h)
      · exact absurd hMat (by with_unfolding_all intro h; cases h)

private theorem birTwoSite_prefix_chain (c : UInt8) :
    let p := lcbProfile
    let prog := birTwoSite p 20 22 c
    let s0 := mkRegState p 0 0 [] [] []
    let s2 := mkRegState p 2 0 [⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨2⟩, ⟨1⟩, ⟨0⟩]
      [(⟨2⟩, .scalar (.int .u8 42)), (⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 42))]
    DetChain ((BIR.Execution (language := s0Core)).step prog) s0 s2 := by
  intro p prog s0 s2
  let s1 := mkRegState p 0 1 [⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
    [(⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 42))]
  obtain ⟨step01, step01u⟩ := birTwoSite_prefix_step0 c
  have step12 : (BIR.Execution (language := s0Core)).step prog s1 s2 := by
    refine ⟨by with_unfolding_all rfl, ?_, ⟨_, by with_unfolding_all rfl⟩⟩
    refine ⟨ssaAddB0View 20 22, by with_unfolding_all rfl, rfl,
      .scalar (.bool false), s1, false, by with_unfolding_all rfl, rfl,
      [.scalar (.int .u8 42)], s1, twoSiteB2View c,
      by with_unfolding_all rfl, by with_unfolding_all rfl, rfl, ?_⟩
    apply Memory.WriteParams.cons
      (started := mkRegState p 2 0 [⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
        [(⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 42))])
      (stored := s2)
    · exact ⟨by simp [s1, mkRegState], Or.inl ⟨rfl, rfl⟩⟩
    · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun, by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
    · exact .nil _
  have hView : prog.memoryView (language := s0Core) =
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain,
          [(⟨0⟩, u8Ty), (⟨1⟩, boolTy), (⟨2⟩, u8Ty), (⟨3⟩, u8Ty), (⟨4⟩, boolTy), (⟨5⟩, u8Ty)],
          ⟨⟨0⟩, #[ssaAddB0View 20 22, twoSiteB1View, twoSiteB2View c, twoSiteB3View, twoSiteB4View]⟩)],
        [],
        [(ctrlMain, [(⟨0⟩, .virtual), (⟨1⟩, .virtual), (⟨2⟩, .virtual),
          (⟨3⟩, .virtual), (⟨4⟩, .virtual), (⟨5⟩, .virtual)])],
        [(ctrlMain, [⟨0⟩, ⟨1⟩, ⟨2⟩, ⟨3⟩, ⟨4⟩, ⟨5⟩])], []⟩ := by
    with_unfolding_all rfl
  have step12u : ∀ next, (BIR.Execution (language := s0Core)).step prog s1 next → next = s2 := by
    intro next ⟨_, hst, _⟩
    change (prog.memoryView (language := s0Core)).Step s1 next true at hst
    rw [hView] at hst
    simp only [Memory.Step, s1, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hrest⟩ := hst
      have hblkEq : blk = ssaAddB0View 20 22 := Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, val, ar, fl, hrd, htst, vs, ar2, tgt, hro, hba, _, hpo⟩ := hrest
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hrd.symm : (val, ar) = (.scalar (.bool false), s1))
      have hfl : fl = false := by cases htst; rfl
      subst hfl
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hro.symm : (vs, ar2) = ([.scalar (.int .u8 42)], s1))
      have htgtEq : tgt = twoSiteB2View c := Option.some.inj hba.symm
      subst htgtEq; exact (hpo.2.2 trivial).elim
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hrest⟩ := hst
      have hblkEq : blk = ssaAddB0View 20 22 := Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, val, ar, fl, hrd, htst, vs, ar2, tgt, hro, hba, _, hwp⟩ := hrest
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hrd.symm : (val, ar) = (.scalar (.bool false), s1))
      have hfl : fl = false := by cases htst; rfl
      subst hfl
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hro.symm : (vs, ar2) = ([.scalar (.int .u8 42)], s1))
      have htgtEq : tgt = twoSiteB2View c := Option.some.inj hba.symm
      subst htgtEq
      cases hwp with
      | cons hbeg hw hnil =>
        rename_i started' stored'
        rcases hbeg.2 with ⟨_, hEq⟩ | ⟨hMat, _⟩
        · rw [hEq] at hw
          have hstor' : stored' = s2 := Option.some.inj hw.2.2.1.symm
          exact (writeParams_nil_eq hnil).trans hstor'
        · exact absurd hMat (by intro h; cases h)
  exact .cons step01 step01u (.cons step12 step12u (.nil _))

private theorem birTwoSite_second_chain :
    let p := lcbProfile
    let secondTraps := birTwoSite p 20 22 250
    let base : AList PlaceId EarlyValue :=
      [(⟨2⟩, .scalar (.int .u8 42)), (⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 42))]
    let s0 := mkRegState p 0 0 [] [] []
    let s4 := mkRegState p 3 0 [⟨4⟩, ⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨4⟩, ⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩]
      ((⟨4⟩, .scalar (.bool true)) :: (⟨3⟩, .scalar (.int .u8 36)) :: base)
    DetChain ((BIR.Execution (language := s0Core)).step secondTraps) s0 s4 ∧
    (∀ o, (BIR.Execution (language := s0Core)).result secondTraps s4 o ↔ o = trapObs1) := by
  intro p secondTraps base s0 s4
  let s2 := mkRegState p 2 0 [⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨2⟩, ⟨1⟩, ⟨0⟩] base
  let s3 := mkRegState p 2 1 [⟨4⟩, ⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨4⟩, ⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩]
    ((⟨4⟩, .scalar (.bool true)) :: (⟨3⟩, .scalar (.int .u8 36)) :: base)
  let mid0 := mkRegState p 2 0 [⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨2⟩, ⟨1⟩, ⟨0⟩] base
  let mid1 := mkRegState p 2 0 [⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩]
    ((⟨3⟩, .scalar (.int .u8 36)) :: base)
  let mid2 := mkRegState p 2 0 [⟨4⟩, ⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩]
    ((⟨3⟩, .scalar (.int .u8 36)) :: base)
  have step23 : (BIR.Execution (language := s0Core)).step secondTraps s2 s3 := by
    refine ⟨by with_unfolding_all rfl, ⟨twoSiteB2View 250, by with_unfolding_all rfl, ?_⟩,
      ⟨_, by with_unfolding_all rfl⟩⟩
    refine ⟨rfl, rfl, Or.inl (by decide),
      .scalar (.int .u8 42), s2, .scalar (.int .u8 250), s2, 42, 250,
      { s3 with instruction := 0 }, by with_unfolding_all rfl, by with_unfolding_all rfl, rfl, rfl, ?_, rfl⟩
    apply Memory.WriteParams.cons (started := mid0) (stored := mid1)
    · exact ⟨by simp [s2, mkRegState], Or.inl ⟨rfl, rfl⟩⟩
    · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun, by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := mid2) (stored := { s3 with instruction := 0 })
      · exact ⟨by simp [mid1, mkRegState], Or.inl ⟨rfl, rfl⟩⟩
      · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun, by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
      · exact .nil _
  have step23u : ∀ next, (BIR.Execution (language := s0Core)).step secondTraps s2 next → next = s3 := by
    intro next ⟨_, hst, _⟩
    change (secondTraps.memoryView (language := s0Core)).Step s2 next true at hst
    simp only [Memory.Step, s2, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hinst⟩ := hst
      have hblkEq : blk = twoSiteB2View 250 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      exact absurd hinst (Memory.addWithOverflow_virtual_cannot_exhaust
        (by with_unfolding_all rfl) (by with_unfolding_all rfl))
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hinst⟩ := hst
      have hblkEq : blk = twoSiteB2View 250 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, _, _, lv, a1, rv, a2, x, y, stored, hl, hr, hlv, hrv, hwp, rfl⟩ := hinst
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hl.symm : (lv, a1) = (.scalar (.int .u8 42), s2))
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hr.symm : (rv, a2) = (.scalar (.int .u8 250), s2))
      have hx : x = 42 := by cases hlv; rfl
      have hy : y = 250 := by cases hrv; rfl
      subst hx; subst hy
      cases hwp with
      | cons hbeg1 hw1 hwp2 =>
        rename_i started1 stored1
        rcases hbeg1.2 with ⟨_, hEq1⟩ | ⟨hMat, _⟩
        · rw [hEq1] at hw1
          have hstor1 : stored1 = mid1 := by with_unfolding_all exact Option.some.inj hw1.2.2.1.symm
          rw [hstor1] at hwp2
          cases hwp2 with
          | cons hbeg2 hw2 hwp3 =>
            rename_i started2 stored2
            rcases hbeg2.2 with ⟨_, hEq2⟩ | ⟨hMat, _⟩
            · rw [hEq2] at hw2
              have hstor2 : stored2 = { s3 with instruction := 0 } := by
                with_unfolding_all exact Option.some.inj hw2.2.2.1.symm
              rw [hstor2] at hwp3
              cases hwp3; rfl
            · exact absurd hMat (by with_unfolding_all intro h; cases h)
        · exact absurd hMat (by with_unfolding_all intro h; cases h)
  have step34 : (BIR.Execution (language := s0Core)).step secondTraps s3 s4 :=
    ⟨by with_unfolding_all rfl,
      ⟨twoSiteB2View 250, by with_unfolding_all rfl, rfl, .scalar (.bool true), s3, true,
        by with_unfolding_all rfl, rfl, [], s3, twoSiteB3View,
        by with_unfolding_all rfl, by with_unfolding_all rfl, rfl, .nil _⟩,
      ⟨_, by with_unfolding_all rfl⟩⟩
  have step34u : ∀ next, (BIR.Execution (language := s0Core)).step secondTraps s3 next → next = s4 := by
    intro next ⟨_, hst, _⟩
    change (secondTraps.memoryView (language := s0Core)).Step s3 next true at hst
    simp only [Memory.Step, s3, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hrest⟩ := hst
      have hblkEq : blk = twoSiteB2View 250 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, val, ar, fl, hrd, htst, vs, ar2, tgt, hro, hba, _, hpo⟩ := hrest
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hrd.symm : (val, ar) = (.scalar (.bool true), s3))
      have hfl : fl = true := by cases htst; rfl
      subst hfl
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hro.symm : (vs, ar2) = ([], s3))
      have htgtEq : tgt = twoSiteB3View := by with_unfolding_all exact Option.some.inj hba.symm
      subst htgtEq; exact (hpo.2.2 (by with_unfolding_all exact trivial)).elim
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hrest⟩ := hst
      have hblkEq : blk = twoSiteB2View 250 := by with_unfolding_all exact Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, val, ar, fl, hrd, htst, vs, ar2, tgt, hro, hba, _, hwp⟩ := hrest
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hrd.symm : (val, ar) = (.scalar (.bool true), s3))
      have hfl : fl = true := by cases htst; rfl
      subst hfl
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (by with_unfolding_all exact Option.some.inj hro.symm : (vs, ar2) = ([], s3))
      have htgtEq : tgt = twoSiteB3View := by with_unfolding_all exact Option.some.inj hba.symm
      subst htgtEq; cases hwp; rfl
  have appendDet : ∀ {a b c : (BIR.Execution (language := s0Core) (lang := s0Lang) (profile := p)).State},
      DetChain ((BIR.Execution (language := s0Core)).step secondTraps) a b →
      DetChain ((BIR.Execution (language := s0Core)).step secondTraps) b c →
      DetChain ((BIR.Execution (language := s0Core)).step secondTraps) a c := by
    intro a b c h1 h2; induction h1 with | nil => exact h2 | cons h hu _ ih => exact .cons h hu (ih h2)
  refine ⟨appendDet (birTwoSite_prefix_chain 250) (.cons step23 step23u (.cons step34 step34u (.nil s4))), fun o => ?_⟩
  have hr : (BIR.Execution (language := s0Core)).result secondTraps s4 trapObs1 := by
    with_unfolding_all rfl
  exact ⟨fun h => (Option.some.inj (hr.symm.trans h)).symm, fun h => h ▸ hr⟩

/-- Control 1c: When site 0 overflows (`255 + 1 + 250`), execution traps at site 0 (and NEVER
observes site 1); when site 0 fits and site 1 overflows (`20 + 22 + 250`), execution traps at
site 1 (and NEVER observes site 0); when both fit (`20 + 22 + 1`), execution returns 43.
All three pass `BIR.RegisterFormation`. -/
theorem two_site_checked_add_first_and_second_overflow :
    let p := lcbProfile
    let firstTraps := birTwoSite p 255 1 250
    let secondTraps := birTwoSite p 20 22 250
    let bothFit := birTwoSite p 20 22 1
    firstTraps.RegisterFormation (language := s0Core) ∧
    secondTraps.RegisterFormation (language := s0Core) ∧
    bothFit.RegisterFormation (language := s0Core) ∧
    (BIR.Execution (language := s0Core)).observes firstTraps trapObs0 ∧
    ¬ (BIR.Execution (language := s0Core)).observes firstTraps trapObs1 ∧
    (BIR.Execution (language := s0Core)).observes secondTraps trapObs1 ∧
    ¬ (BIR.Execution (language := s0Core)).observes secondTraps trapObs0 ∧
    (BIR.Execution (language := s0Core)).observes bothFit (retObs 43) := by
  let p := lcbProfile
  let firstTraps := birTwoSite p 255 1 250
  let secondTraps := birTwoSite p 20 22 250
  let bothFit := birTwoSite p 20 22 1
  have hFirstIff := birTwoSite_first_chain.1.observes_iff (by with_unfolding_all rfl)
    (fun _ h => by with_unfolding_all exact h) birTwoSite_first_chain.2
  have hFirst : (BIR.Execution (language := s0Core)).observes firstTraps trapObs0 :=
    (hFirstIff trapObs0).mpr rfl
  have hFirstNot1 : ¬ (BIR.Execution (language := s0Core)).observes firstTraps trapObs1 :=
    fun h => obs_ne_trap_site ((hFirstIff trapObs1).mp h).symm
  have hSecondIff := birTwoSite_second_chain.1.observes_iff (by with_unfolding_all rfl)
    (fun _ h => by with_unfolding_all exact h) birTwoSite_second_chain.2
  have hSecond : (BIR.Execution (language := s0Core)).observes secondTraps trapObs1 :=
    (hSecondIff trapObs1).mpr rfl
  have hSecondNot0 : ¬ (BIR.Execution (language := s0Core)).observes secondTraps trapObs0 :=
    fun h => obs_ne_trap_site ((hSecondIff trapObs0).mp h)
  -- 3. Both sites succeed (`20 + 22 + 1 = 43`): b2 → b4 (return 43).
  have hBoth : (BIR.Execution (language := s0Core)).observes bothFit (retObs 43) := by
    let base : AList PlaceId EarlyValue :=
      [(⟨2⟩, .scalar (.int .u8 42)), (⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 42))]
    let s2 := mkRegState p 2 0 [⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨2⟩, ⟨1⟩, ⟨0⟩] base
    let s3 := mkRegState p 2 1 [⟨4⟩, ⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨4⟩, ⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩]
      ((⟨4⟩, .scalar (.bool false)) :: (⟨3⟩, .scalar (.int .u8 43)) :: base)
    let s4 := mkRegState p 4 0 [⟨5⟩, ⟨4⟩, ⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨5⟩, ⟨4⟩, ⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩]
      ((⟨5⟩, .scalar (.int .u8 43)) :: (⟨4⟩, .scalar (.bool false)) :: (⟨3⟩, .scalar (.int .u8 43)) :: base)
    have step23 : (BIR.Execution (language := s0Core)).step bothFit s2 s3 := by
      refine ⟨by with_unfolding_all rfl, ⟨twoSiteB2View 1, by with_unfolding_all rfl, ?_⟩,
        ⟨_, by with_unfolding_all rfl⟩⟩
      refine ⟨rfl, rfl, Or.inl (by decide),
        .scalar (.int .u8 42), s2, .scalar (.int .u8 1), s2, 42, 1,
        { s3 with instruction := 0 }, by with_unfolding_all rfl, by with_unfolding_all rfl, rfl, rfl, ?_, rfl⟩
      apply Memory.WriteParams.cons
        (started := mkRegState p 2 0 [⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨2⟩, ⟨1⟩, ⟨0⟩] base)
        (stored := mkRegState p 2 0 [⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩]
          ((⟨3⟩, .scalar (.int .u8 43)) :: base))
      · exact ⟨by simp [s2, mkRegState], Or.inl ⟨rfl, rfl⟩⟩
      · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun, by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
      · apply Memory.WriteParams.cons
          (started := mkRegState p 2 0 [⟨4⟩, ⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩]
            ((⟨3⟩, .scalar (.int .u8 43)) :: base))
          (stored := { s3 with instruction := 0 })
        · exact ⟨by simp [mkRegState], Or.inl ⟨rfl, rfl⟩⟩
        · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun, by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
        · exact .nil _
    have step34 : (BIR.Execution (language := s0Core)).step bothFit s3 s4 := by
      refine ⟨by with_unfolding_all rfl, ?_, ⟨_, by with_unfolding_all rfl⟩⟩
      refine ⟨twoSiteB2View 1, by with_unfolding_all rfl, rfl, .scalar (.bool false), s3, false,
        by with_unfolding_all rfl, rfl, [.scalar (.int .u8 43)], s3, twoSiteB4View,
        by with_unfolding_all rfl, by with_unfolding_all rfl, rfl, ?_⟩
      apply Memory.WriteParams.cons
        (started := mkRegState p 4 0 [⟨5⟩, ⟨4⟩, ⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩] [⟨4⟩, ⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩]
          ((⟨4⟩, .scalar (.bool false)) :: (⟨3⟩, .scalar (.int .u8 43)) :: base))
        (stored := s4)
      · exact ⟨by simp [s3, mkRegState], Or.inl ⟨rfl, rfl⟩⟩
      · exact ⟨⟨by with_unfolding_all rfl, nofun⟩, nofun, by with_unfolding_all rfl, by with_unfolding_all rfl, nofun⟩
      · exact .nil _
    have appendSteps : ∀ {a b c : (BIR.Execution (language := s0Core) (lang := s0Lang) (profile := p)).State},
        Steps ((BIR.Execution (language := s0Core)).step bothFit) a b →
        Steps ((BIR.Execution (language := s0Core)).step bothFit) b c →
        Steps ((BIR.Execution (language := s0Core)).step bothFit) a c := by
      intro a b c h1 h2; induction h1 with | nil => exact h2 | cons h _ ih => exact .cons h (ih h2)
    exact ⟨s4, ⟨mkRegState p 0 0 [] [] [], by with_unfolding_all rfl,
      appendSteps (birTwoSite_prefix_chain 1).toSteps (.cons step23 (.cons step34 (.nil _)))⟩, by with_unfolding_all rfl⟩
  exact ⟨by decide, by decide, by decide, hFirst, hFirstNot1, hSecond, hSecondNot0, hBoth⟩

/-! ## Section 2: Planted checked-add mutants (all 6 required mutation classes) -/

-- Mutant 2c (type-level `dst`/`flag` controls): `SSAValue EarlyTy u8Ty` (`dst`) and
-- `SSAValue EarlyTy boolTy` (`flag`) are statically distinct across `ret`, `.branch`
-- continuation arguments, `.branch` condition, and both result slots of `addWithOverflow`.

-- Positive baseline: well-typed `addWithOverflow` block using `flgV` as branch condition and
-- `dstV` as `u8Ty` continuation argument elaborates cleanly.
example (dstV : SSAValue EarlyTy u8Ty) (flgV : SSAValue EarlyTy boolTy) : SSABlock s0Lang :=
  ⟨⟨0⟩, [], [⟨orig0, .addWithOverflow .u8 dstV flgV (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)⟩],
    .branch (.value flgV) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, [⟨u8Ty, .value dstV⟩]⟩, []⟩

-- 2c(i): Bool overflow flag cannot be returned where a `u8Ty` value is expected.
/--
error: Application type mismatch: The argument
  flgV
has type
  SSAValue EarlyTy boolTy
but is expected to have type
  SSAValue EarlyTy u8Ty
in the application
  SSARead.value flgV
-/
#guard_msgs in
example (flgV : SSAValue EarlyTy boolTy) : SSABlock s0Lang :=
  ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .value flgV⟩), []⟩

-- 2c(ii): Bool overflow flag cannot be passed as a `u8Ty` continuation jump argument.
/--
error: Application type mismatch: The argument
  flgV
has type
  SSAValue EarlyTy boolTy
but is expected to have type
  SSAValue EarlyTy u8Ty
in the application
  SSARead.value flgV
-/
#guard_msgs in
example (dstV : SSAValue EarlyTy u8Ty) (flgV : SSAValue EarlyTy boolTy) : SSABlock s0Lang :=
  ⟨⟨0⟩, [], [⟨orig0, .addWithOverflow .u8 dstV flgV (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)⟩],
    .branch (.value flgV) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, [⟨u8Ty, .value flgV⟩]⟩, []⟩

-- 2c(iii): Integer sum `dst` (`u8Ty`) cannot be used as the `.branch` condition (`s0Lang.bool`).
/--
error: Application type mismatch: The argument
  dstV
has type
  SSAValue EarlyTy u8Ty
but is expected to have type
  SSAValue EarlyTy s0Lang.bool
in the application
  SSARead.value dstV
-/
#guard_msgs in
example (dstV : SSAValue EarlyTy u8Ty) (flgV : SSAValue EarlyTy boolTy) : SSABlock s0Lang :=
  ⟨⟨0⟩, [], [⟨orig0, .addWithOverflow .u8 dstV flgV (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)⟩],
    .branch (.value dstV) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, [⟨u8Ty, .value dstV⟩]⟩, []⟩

-- 2c(iv): `addWithOverflow`'s `dst` slot rejects a `boolTy` SSA register.
/--
error: Application type mismatch: The argument
  badDst
has type
  SSAValue EarlyTy boolTy
but is expected to have type
  SSAValue EarlyTy (s0Lang.intTy IntTy.u8)
in the application
  SSAInst.addWithOverflow IntTy.u8 badDst
-/
#guard_msgs in
example (badDst : SSAValue EarlyTy boolTy) (flgV : SSAValue EarlyTy boolTy) : SSAInst s0Lang :=
  .addWithOverflow .u8 badDst flgV (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)

-- 2c(v): `addWithOverflow`'s `flag` slot rejects a `u8Ty` SSA register.
/--
error: Application type mismatch: The argument
  badFlag
has type
  SSAValue EarlyTy u8Ty
but is expected to have type
  SSAValue EarlyTy s0Lang.bool
in the application
  SSAInst.addWithOverflow IntTy.u8 dstV badFlag
-/
#guard_msgs in
example (dstV : SSAValue EarlyTy u8Ty) (badFlag : SSAValue EarlyTy u8Ty) : SSAInst s0Lang :=
  .addWithOverflow .u8 dstV badFlag (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)

/-- Mutant 2a (constant flag), 2b (inverted flag), 2c (wrong continuation value),
2d (aliased `dst`/`flag`), 2e (wrapped-on-overflow + unguarded plain add), and
2f (substituted origin) are ALL rejected by `ResidualAddsMatch`, `BuildsSSA`, `BuildsBIR`,
and `RegisterFormation`. -/
theorem checkedAdd_mutants_rejected :
    let p := lcbProfile
    -- 2a: Constant false flag on `255 + 1` (computed from `0 + 0`, so `RegisterFormation` passes!)
    let constFalseSSA : SSA s0Lang p :=
      let b0 : SSABlock s0Lang :=
        ⟨⟨0⟩, [],
          [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[255]⟩) (.literal ⟨[1]⟩)⟩,
           ⟨orig0, .addWithOverflow .u8 ⟨3⟩ ⟨4⟩ (.literal ⟨[0]⟩) (.literal ⟨[0]⟩)⟩],
          .branch (.value ⟨4⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, [⟨u8Ty, .value ⟨0⟩⟩]⟩, []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, ssaAddB1, ssaAddB2]⟩)], [], [], []⟩
    -- 2a(ii): Constant true flag on `20 + 22` (computed from `255 + 1`, so `RegisterFormation` passes!)
    let constTrueSSA : SSA s0Lang p :=
      let b0 : SSABlock s0Lang :=
        ⟨⟨0⟩, [],
          [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)⟩,
           ⟨orig0, .addWithOverflow .u8 ⟨3⟩ ⟨4⟩ (.literal ⟨[255]⟩) (.literal ⟨[1]⟩)⟩],
          .branch (.value ⟨4⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, [⟨u8Ty, .value ⟨0⟩⟩]⟩, []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, ssaAddB1, ssaAddB2]⟩)], [], [], []⟩
    -- 2b: Inverted flag on `20 + 22` (swaps `yes` and `no` edges of `.branch`).
    let invertedSSA : SSA s0Lang p :=
      let b0 : SSABlock s0Lang :=
        ⟨⟨0⟩, [],
          [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)⟩],
          .branch (.value ⟨1⟩) ⟨⟨2⟩, [⟨u8Ty, .value ⟨0⟩⟩]⟩ ⟨⟨1⟩, []⟩, []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, ssaAddB1, ssaAddB2]⟩)], [], [], []⟩
    -- 2c: Wrong continuation value (passes literal `0` instead of `.value ⟨0⟩` on `20 + 22`).
    let wrongContSSA : SSA s0Lang p :=
      let b0 : SSABlock s0Lang :=
        ⟨⟨0⟩, [],
          [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)⟩],
          .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, [⟨u8Ty, .literal ⟨[0]⟩⟩]⟩, []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, ssaAddB1, ssaAddB2]⟩)], [], [], []⟩
    -- 2d: Aliased `dst`/`flag` (`dst.id = flag.id = 0`).
    let aliasedSSA : SSA s0Lang p :=
      let b0 : SSABlock s0Lang :=
        ⟨⟨0⟩, [],
          [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨0⟩ (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], [], []⟩
    -- 2e(i): Wrapped-on-overflow (jumps straight to continuation using wrapped sum, ignoring flag).
    let wrappedJumpSSA : SSA s0Lang p :=
      let b0 : SSABlock s0Lang :=
        ⟨⟨0⟩, [],
          [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[255]⟩) (.literal ⟨[1]⟩)⟩],
          .jump ⟨⟨2⟩, [⟨u8Ty, .value ⟨0⟩⟩]⟩, []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, ssaAddB2]⟩)], [], [], []⟩
    -- 2e(ii): Unguarded plain add left in SSA at `orig0`.
    let unguardedPlainSSA : SSA s0Lang p :=
      let b0 : SSABlock s0Lang :=
        ⟨⟨0⟩, [],
          [⟨orig0, .op (opcode := .arithmetic (.plain .add)) (types := [u8Ty, u8Ty]) (ty := u8Ty)
            ⟨0⟩ () (.cons (.literal ⟨[20]⟩) (.cons (.literal ⟨[22]⟩) .nil))⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], [], []⟩
    -- 2f: Substituted trap origin (`orig1` on trap block `b1` / `bir.sites[0]`).
    let substOrigSSA : SSA s0Lang p :=
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[ssaAddB0 255 1, ssaAddB1 orig1, ssaAddB2]⟩)], [], [], []⟩
    let substOrigBIR := birAdd p 255 1 #[⟨.overflow, orig1⟩]
    -- Assertions:
    constFalseSSA.RegisterFormation (language := s0Core) ∧
    ¬ ResidualAddsMatch (memAdd p 255 1) constFalseSSA ∧
    ¬ BuildsSSA (language := s0Core) (memAdd p 255 1) constFalseSSA ∧
    constTrueSSA.RegisterFormation (language := s0Core) ∧
    ¬ ResidualAddsMatch (memAdd p 20 22) constTrueSSA ∧
    ¬ BuildsSSA (language := s0Core) (memAdd p 20 22) constTrueSSA ∧
    ¬ ResidualAddsMatch (memAdd p 20 22) invertedSSA ∧
    ¬ BuildsSSA (language := s0Core) (memAdd p 20 22) invertedSSA ∧
    ¬ ResidualAddsMatch (memAdd p 20 22) wrongContSSA ∧
    ¬ BuildsSSA (language := s0Core) (memAdd p 20 22) wrongContSSA ∧
    ¬ aliasedSSA.RegisterFormation (language := s0Core) ∧
    ¬ (aliasedSSA.memoryView (language := s0Core)).TablesWF ∧
    ¬ ResidualAddsMatch (memAdd p 20 22) aliasedSSA ∧
    wrappedJumpSSA.RegisterFormation (language := s0Core) ∧
    ¬ ResidualAddsMatch (memAdd p 255 1) wrappedJumpSSA ∧
    ¬ BuildsSSA (language := s0Core) (memAdd p 255 1) wrappedJumpSSA ∧
    ¬ ResidualAddsMatch (memAdd p 20 22) unguardedPlainSSA ∧
    ¬ ResidualAddsMatch (memAdd p 255 1) substOrigSSA ∧
    ¬ BuildsSSA (language := s0Core) (memAdd p 255 1) substOrigSSA ∧
    ¬ BuildsBIR (language := s0Core) (ssaAdd p 255 1) substOrigBIR := by
  intro p constFalseSSA constTrueSSA invertedSSA wrongContSSA aliasedSSA wrappedJumpSSA
    unguardedPlainSSA substOrigSSA substOrigBIR
  -- Helper: extract `BlockGuardsAddWithOverflow` from `ResidualAddsMatch` on `memAdd`.
  have extractGuard (a b : UInt8) (ssa : SSA s0Lang p) (hMatch : ResidualAddsMatch (memAdd p a b) ssa) :
      ∃ sLocals sGraph sLhs sRhs no contBlock contVal envHere opLoads,
        (ctrlMain, sLocals, sGraph) ∈ ssa.functions ∧
        ReadImage envHere opLoads (.literal ⟨[a]⟩) sLhs ∧
        ReadImage envHere opLoads (.literal ⟨[b]⟩) sRhs ∧
        sGraph.GuardsAddWithOverflow .u8 orig0 sLhs sRhs no contBlock contVal := by
    let addInst : MemoryInst s0Lang :=
      .op (opcode := .arithmetic (.plain .add)) (types := [u8Ty, u8Ty]) (ty := u8Ty)
        memPlace0 () (.cons (.literal ⟨[a]⟩) (.cons (.literal ⟨[b]⟩) .nil))
        [⟨0, .overflow, .residual⟩] []
    have hRes : addInst.HasResidualAdd :=
      ⟨⟨0, .overflow, .residual⟩, List.mem_cons_self, rfl, rfl⟩
    obtain ⟨t, _, _, _, sl, sg, slhs, srhs, no, cb, cv, eh, ol, hPlain, hfn, _, hl, hr, hG, _⟩ :=
      hMatch.guards_of_residualAdd (fn := ctrlMain) (mLocals := [(⟨0⟩, u8Ty)])
        (mGraph := ⟨⟨0⟩, #[memAddBlock a b]⟩) (mOp := ⟨orig0, addInst⟩)
        List.mem_cons_self List.mem_cons_self
        (List.mem_cons_of_mem _ List.mem_cons_self) hRes
    cases hPlain
    exact ⟨sl, sg, slhs, srhs, no, cb, cv, eh, ol, hfn, hl, hr, hG⟩
  -- 2a(i): `constFalseSSA` last instruction has operands `0, 0`, not `255, 1`.
  have hNoConstFalse : ¬ ResidualAddsMatch (memAdd p 255 1) constFalseSSA := by
    intro hM
    obtain ⟨_, sg, slhs, srhs, _, _, _, _, _, hfn, hl, _, gb, hgb, dst, flg, _, hLast, _⟩ :=
      extractGuard 255 1 constFalseSSA hM
    rcases Prod.mk.inj (List.mem_singleton.mp hfn) with ⟨_, hEq⟩
    rcases Prod.mk.inj hEq with ⟨_, rfl⟩
    have hlEq : slhs = .literal ⟨[255]⟩ := hl
    subst hlEq
    rcases List.mem_cons.mp hgb with rfl | hrest
    · cases hLast
    · rcases List.mem_cons.mp hrest with rfl | hrest2
      · cases hLast
      · rcases List.mem_singleton.mp hrest2 with rfl; cases hLast
  -- 2a(ii): `constTrueSSA` last instruction has operands `255, 1`, not `20, 22`.
  have hNoConstTrue : ¬ ResidualAddsMatch (memAdd p 20 22) constTrueSSA := by
    intro hM
    obtain ⟨_, sg, slhs, srhs, _, _, _, _, _, hfn, hl, _, gb, hgb, dst, flg, _, hLast, _⟩ :=
      extractGuard 20 22 constTrueSSA hM
    rcases Prod.mk.inj (List.mem_singleton.mp hfn) with ⟨_, hEq⟩
    rcases Prod.mk.inj hEq with ⟨_, rfl⟩
    have hlEq : slhs = .literal ⟨[20]⟩ := hl
    subst hlEq
    rcases List.mem_cons.mp hgb with rfl | hrest
    · cases hLast
    · rcases List.mem_cons.mp hrest with rfl | hrest2
      · cases hLast
      · rcases List.mem_singleton.mp hrest2 with rfl; cases hLast
  -- 2b: `invertedSSA` has `yes.arguments = [⟨u8Ty, .value ⟨0⟩⟩] ≠ []`.
  have hNoInverted : ¬ ResidualAddsMatch (memAdd p 20 22) invertedSSA := by
    intro hM
    obtain ⟨_, sg, _, _, _, _, _, _, _, hfn, _, _, gb, hgb, _, _, _, hLast, yes, _, hBr, hYesArgs, _⟩ :=
      extractGuard 20 22 invertedSSA hM
    rcases Prod.mk.inj (List.mem_singleton.mp hfn) with ⟨_, hEq⟩
    rcases Prod.mk.inj hEq with ⟨_, rfl⟩
    rcases List.mem_cons.mp hgb with rfl | hrest
    · cases hBr; cases hYesArgs
    · rcases List.mem_cons.mp hrest with rfl | hrest2
      · cases hLast
      · rcases List.mem_singleton.mp hrest2 with rfl; cases hLast
  -- 2c: `wrongContSSA` passes `.literal ⟨[0]⟩` instead of `.value dst` on `no`.
  have hNoWrongCont : ¬ ResidualAddsMatch (memAdd p 20 22) wrongContSSA := by
    intro hM
    obtain ⟨_, sg, _, _, no, cb, cv, _, _, hfn, _, _, gb, hgb, _, _, _, hLast, _, _, hBr,
      _, _, _, _, _, _, _, _, _, _, idx, hArg, _⟩ :=
      extractGuard 20 22 wrongContSSA hM
    rcases Prod.mk.inj (List.mem_singleton.mp hfn) with ⟨_, hEq⟩
    rcases Prod.mk.inj hEq with ⟨_, rfl⟩
    rcases List.mem_cons.mp hgb with rfl | hrest
    · cases hBr
      cases idx with
      | zero => cases hArg
      | succ k => cases hArg
    · rcases List.mem_cons.mp hrest with rfl | hrest2
      · cases hLast
      · rcases List.mem_singleton.mp hrest2 with rfl; cases hLast
  -- 2d: `aliasedSSA` fails `TablesWF` and `ResidualAddsMatch`.
  have hNoAliasedTables : ¬ (aliasedSSA.memoryView (language := s0Core)).TablesWF :=
    SSA.memoryView_addWithOverflow_aliased_not_tablesWF List.mem_cons_self List.mem_cons_self
      List.mem_cons_self rfl
  have hNoAliasedRes : ¬ ResidualAddsMatch (memAdd p 20 22) aliasedSSA := by
    intro hM
    have hNe := hM.addWithOverflow_distinct List.mem_cons_self List.mem_cons_self List.mem_cons_self
    exact hNe rfl
  -- 2e(i): `wrappedJumpSSA` has `.jump` instead of `.branch`.
  have hNoWrappedJump : ¬ ResidualAddsMatch (memAdd p 255 1) wrappedJumpSSA := by
    intro hM
    obtain ⟨_, sg, _, _, _, _, _, _, _, hfn, _, _, gb, hgb, _, _, _, hLast, _, _, hBr, _⟩ :=
      extractGuard 255 1 wrappedJumpSSA hM
    rcases Prod.mk.inj (List.mem_singleton.mp hfn) with ⟨_, hEq⟩
    rcases Prod.mk.inj hEq with ⟨_, rfl⟩
    rcases List.mem_cons.mp hgb with rfl | hrest
    · cases hBr
    · rcases List.mem_singleton.mp hrest with rfl; cases hLast
  -- 2e(ii): `unguardedPlainSSA` leaves plain `.add` at `orig0`.
  have hNoUnguarded : ¬ ResidualAddsMatch (memAdd p 20 22) unguardedPlainSSA := by
    intro hM
    obtain ⟨_, sg, _, _, _, _, _, _, _, hfn, _, _, gb, hgb, _, _, _, hLast, _⟩ :=
      extractGuard 20 22 unguardedPlainSSA hM
    rcases Prod.mk.inj (List.mem_singleton.mp hfn) with ⟨_, hEq⟩
    rcases Prod.mk.inj hEq with ⟨_, rfl⟩
    rcases List.mem_singleton.mp hgb with rfl
    cases hLast
  -- 2f: `substOrigSSA` has `.trap .overflow orig1` instead of `orig0`, and `substOrigBIR` has `orig1` in `sites[0]`.
  have hNoSubstOrigSSA : ¬ ResidualAddsMatch (memAdd p 255 1) substOrigSSA := by
    intro hM
    obtain ⟨_, sg, _, _, _, _, _, _, _, hfn, _, _, gb, hgb, _, _, _, hLast, yes, tb, hBr,
      _, htbMem, htbId, _, _, htbEnd, _⟩ :=
      extractGuard 255 1 substOrigSSA hM
    rcases Prod.mk.inj (List.mem_singleton.mp hfn) with ⟨_, hEq⟩
    rcases Prod.mk.inj hEq with ⟨_, rfl⟩
    rcases List.mem_cons.mp hgb with rfl | hrest
    · cases hBr
      rcases List.mem_cons.mp htbMem with rfl | ht1
      · cases htbId
      · rcases List.mem_cons.mp ht1 with rfl | ht2
        · cases htbEnd
        · rcases List.mem_singleton.mp ht2 with rfl; cases htbId
    · rcases List.mem_cons.mp hrest with rfl | hrest2
      · cases hLast
      · rcases List.mem_singleton.mp hrest2 with rfl; cases hLast
  have hNoSubstOrigBIR : ¬ BuildsBIR (language := s0Core) (ssaAdd p 255 1) substOrigBIR := by
    intro hB
    obtain ⟨_, hFnMem, hFnEq, _, hBlkMem, hBlkEq, slot, hEnd, entry, hEntry, _, hOrigEq⟩ :=
      hB.2.2.1 (ctrlMain, [], ⟨⟨0⟩, #[ssaAddB0 255 1, ssaAddB1, ssaAddB2]⟩) List.mem_cons_self
        ssaAddB1 (List.mem_cons_of_mem _ List.mem_cons_self) .overflow orig0 rfl
    rcases List.mem_singleton.mp hFnMem with rfl
    rcases List.mem_cons.mp hBlkMem with rfl | hb1
    · cases hBlkEq
    · rcases List.mem_cons.mp hb1 with rfl | hb2
      · cases BIRTerm.trap.inj hEnd
        cases Option.some.inj hEntry
        exact (by decide : orig1 ≠ orig0) hOrigEq
      · rcases List.mem_singleton.mp hb2 with rfl; cases hBlkEq
  exact ⟨by decide, hNoConstFalse, fun h => hNoConstFalse h.residualAddsMatch,
    by decide, hNoConstTrue, fun h => hNoConstTrue h.residualAddsMatch,
    hNoInverted, fun h => hNoInverted h.residualAddsMatch,
    hNoWrongCont, fun h => hNoWrongCont h.residualAddsMatch,
    by decide, hNoAliasedTables, hNoAliasedRes,
    by decide, hNoWrappedJump, fun h => hNoWrappedJump h.residualAddsMatch,
    hNoUnguarded, hNoSubstOrigSSA, fun h => hNoSubstOrigSSA h.residualAddsMatch,
    hNoSubstOrigBIR⟩

/-- Aliased `MemoryInst.addWithOverflow` (`dst` and `flag` at the same root) is stuck at `InstructionStepWith`. -/
theorem memory_addWithOverflow_aliased_stuck (profile : Profile)
    (mem : Memory s0Lang profile) (st next : MemoryRegisters s0Core profile)
    (logical startDefs exhausting : Bool) :
    ¬ mem.InstructionStepWith st next logical startDefs
      (.addWithOverflow .u8 ⟨⟨⟨0⟩, []⟩, .virtual⟩ ⟨⟨⟨0⟩, []⟩, .virtual⟩
        (.literal ⟨[1]⟩) (.literal ⟨[2]⟩)) exhausting :=
  Memory.addWithOverflow_aliased_stuck rfl rfl rfl

/-! ## Section 3: Frame-slot budget (2-slot passes, 1-slot fails) and `RegisterFormation` -/

private def ssaSingleAdd (profile : Profile) : SSA s0Lang profile :=
  let b0 : SSABlock s0Lang :=
    ⟨⟨0⟩, [],
      [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)⟩],
      .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], [], []⟩

private def birSingleAdd (profile : Profile) : BIR s0Lang profile :=
  let b0 : Block _ _ _ :=
    ⟨⟨0⟩, [],
      [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)⟩],
      .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩

/-- Minimal dummy `Passes` bundle whose `buildBIR` refuses when `maxBIRSlots < ssa.frameSlots`. -/
private def mkBudgetPasses (profile : Profile) : Passes slice0 profile where
  lex _ := .error (.missingFile ⟨0⟩)
  elaborate _ := .error (.unsupportedSlice slice0)
  checkCore _ := .error (.fragment (.sliceMismatch slice0 slice0))
  lowerCore _ := .error .lcbLinkingUnavailable
  lowerUnchecked _ := .error .lcbLinkingUnavailable
  lowerMemory _ := .error .lcbLinkingUnavailable
  buildSSA _ := .error .lcbLinkingUnavailable
  buildBIR ssa :=
    if profile.maxBIRSlots < ssa.frameSlots then
      .error (.resource ⟨⟨``BIR.frameSlots, 1⟩, ssa.frameSlots, profile.maxBIRSlots⟩)
    else .error .lcbLinkingUnavailable
  emit _ := .error .lcbLinkingUnavailable

private theorem birSingleAdd_WF (p : Profile)
    (hReg : (birSingleAdd p).RegisterFormation (language := s0Core))
    (hSlots : 2 ≤ p.maxBIRSlots) :
    (birSingleAdd p).WF (language := s0Core) := by
  have hView : (birSingleAdd p).memoryView (language := s0Core) =
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [(⟨0⟩, u8Ty), (⟨1⟩, boolTy)],
          ⟨⟨0⟩, #[⟨⟨0⟩, [],
            [⟨orig0, .addWithOverflow .u8 ⟨⟨⟨0⟩, []⟩, .virtual⟩ ⟨⟨⟨1⟩, []⟩, .virtual⟩
              (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)⟩],
            .ret (some ⟨u8Ty, .copy ⟨⟨⟨0⟩, []⟩, .virtual⟩⟩), []⟩]⟩)],
        [], [(ctrlMain, [(⟨0⟩, .virtual), (⟨1⟩, .virtual)])],
        [(ctrlMain, [⟨0⟩, ⟨1⟩])], []⟩ := by
    with_unfolding_all rfl
  have hMemWF : ((birSingleAdd p).memoryView (language := s0Core)).HandoffWF := by
    rw [hView]
    have hNodup1 : [ctrlMain].Nodup := List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩
    have hNodup01 : ([⟨0⟩, ⟨1⟩] : List PlaceId).Nodup := by decide
    have hNodupB0 : ([⟨0⟩] : List BlockId).Nodup := by decide
    refine ⟨?_, List.mem_cons_self, ?_, (fun _ h => nomatch h),
      ⟨List.nodup_nil, ⟨List.nodup_nil, (fun _ h => nomatch h)⟩, (fun _ _ h => nomatch h)⟩, ?_⟩
    · refine ⟨hNodup1, hNodup1, hNodup1, List.nodup_nil, ?_, (fun _ _ h => nomatch h), ?_⟩
      · intro fn cls hmem
        obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
        exact hNodup01
      · intro fn ls gr hmem
        obtain ⟨rfl, rfl, rfl⟩ := mem_singleton_triple hmem
        refine ⟨hNodup01, ⟨hNodupB0, ⟨_, List.mem_cons_self, rfl⟩, ?_⟩,
          ⟨⟨[], some u8Ty⟩, rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩, ?_⟩⟩
        · intro blk hblk edge hedge
          rcases List.mem_singleton.mp hblk with rfl; cases hedge
        · intro blk hblk
          rcases List.mem_singleton.mp hblk with rfl
          exact ⟨rfl, fun op hop => by rcases List.mem_singleton.mp hop with rfl; trivial⟩
    · intro orig horig
      have horig' : orig ∈ ([orig0] : List Origin) := by with_unfolding_all exact horig
      rcases List.mem_singleton.mp horig' with rfl
      exact span0_binds
    · refine ⟨hNodup1, fun fn roots hmem => ?_⟩
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      refine ⟨_, _, List.mem_cons_self, fun root hroot => ?_⟩
      rcases List.mem_cons.mp hroot with rfl | hrest
      · exact ⟨u8Ty, List.mem_cons_self⟩
      · rcases List.mem_singleton.mp hrest with rfl
        exact ⟨boolTy, List.mem_cons_of_mem _ List.mem_cons_self⟩
  refine ⟨hReg, ?_, hSlots, ?_, (fun _ h => nomatch h), hMemWF⟩
  · intro fn hfn blk hblk slot htrap
    rcases List.mem_singleton.mp hfn with rfl
    rcases List.mem_singleton.mp hblk with rfl
    cases htrap
  · intro orig horig
    have horig' : orig ∈ ([orig0] : List Origin) := by with_unfolding_all exact horig
    rcases List.mem_singleton.mp horig' with rfl
    exact span0_binds

/-- Control 3a: A single `addWithOverflow` charges exactly 2 frame slots:
`maxBIRSlots = 2` passes `BIR.WF` and `BuildsBIR`, while `maxBIRSlots = 1` fails both and
satisfies `(registry passes).buildBIR.refusal` with the exact `.resource` refusal. -/
theorem checkedAdd_slot_budget_2_passes_1_fails :
    let p2 := lcbProfile 2
    let p1 := lcbProfile 1
    let ssa2 := ssaSingleAdd p2
    let bir2 := birSingleAdd p2
    let ssa1 := ssaSingleAdd p1
    let bir1 := birSingleAdd p1
    ssa2.frameSlots = 2 ∧
    bir2.frameSlots = 2 ∧
    bir2.WF (language := s0Core) ∧
    BuildsBIR (language := s0Core) ssa2 bir2 ∧
    ¬ bir1.WF (language := s0Core) ∧
    ¬ BuildsBIR (language := s0Core) ssa1 bir1 ∧
    (registry (mkBudgetPasses p1)).buildBIR.run ssa1 =
      .error (.resource ⟨⟨``BIR.frameSlots, 1⟩, 2, 1⟩) ∧
    (registry (mkBudgetPasses p1)).buildBIR.refusal ssa1
      (.resource ⟨⟨``BIR.frameSlots, 1⟩, 2, 1⟩) := by
  intro p2 p1 ssa2 bir2 ssa1 bir1
  have hView2 : ssa2.memoryView (language := s0Core) = bir2.memoryView (language := s0Core) := by
    with_unfolding_all rfl
  have hBir2WF : bir2.WF (language := s0Core) := birSingleAdd_WF p2 (by decide) (by decide)
  have hBuilds2 : BuildsBIR (language := s0Core) ssa2 bir2 := by
    refine ⟨rfl, hBir2WF.trapSlotsWF, ?_, ⟨fun _ h => h, fun _ h => h⟩, by decide,
      ⟨rfl, rfl, congrArg (fun m => m.graphAt ctrlMain) hView2⟩,
      BIR.refinesOfViewEq hView2⟩
    intro fn hfn blk hblk k o htrap
    rcases List.mem_singleton.mp hfn with rfl
    rcases List.mem_singleton.mp hblk with rfl
    cases htrap
  exact ⟨rfl, rfl, hBir2WF, hBuilds2,
    fun h => (by decide : ¬ 2 ≤ 1) h.frameSlots_le,
    fun h => (by decide : ¬ 2 ≤ 1) h.2.2.2.2.1,
    rfl, ⟨rfl, rfl, rfl, by decide⟩⟩

/-- Control 3b: `RegisterFormation` positive controls (`u8`/`u32` checked-add, zero-slot self-loop
with `maxBIRSlots = 0`, parameterized loop) and negative controls (sibling-block non-dominance,
diamond non-dominance, unreachable use-before-def, unreachable `.invalid`, malformed `u32` literal,
out-of-bounds trap slot; aliased `dst`/`flag` is covered in `checkedAdd_mutants_rejected`). -/
theorem registerFormation_controls :
    let p := lcbProfile
    let p0 := lcbProfile 0
    let goodU8 := birAdd p 20 22
    let goodU32 : BIR s0Lang p :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig0, .addWithOverflow .u32 ⟨0⟩ ⟨1⟩ (.literal ⟨[20, 0, 0, 0]⟩) (.literal ⟨[22, 0, 0, 0]⟩)⟩],
          .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
      let b1 : Block _ _ _ := ⟨⟨1⟩, [], [], .trap ⟨0⟩, []⟩
      let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .ret (some ⟨u32Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u32Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)], [], #[⟨.overflow, orig0⟩], [], []⟩
    let zeroLoop : BIR s0Lang p0 :=
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .jump ⟨⟨0⟩, []⟩, []⟩]⟩)], [], #[], [], []⟩
    let paramLoop : BIR s0Lang p :=
      let b0 : Block _ _ _ := ⟨⟨0⟩, [], [], .jump ⟨⟨1⟩, [⟨u8Ty, .literal ⟨[1]⟩⟩]⟩, []⟩
      let b1 : Block _ _ _ :=
        ⟨⟨1⟩, [⟨u8Ty, ⟨0⟩⟩],
          [⟨orig0, .addWithOverflow .u8 ⟨1⟩ ⟨2⟩ (.value ⟨0⟩) (.literal ⟨[1]⟩)⟩],
          .branch (.value ⟨2⟩) ⟨⟨1⟩, [⟨u8Ty, .value ⟨1⟩⟩]⟩ ⟨⟨2⟩, []⟩, []⟩
      let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)], [], #[], [], []⟩
    let siblingNonDom : BIR s0Lang p :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[1]⟩)⟩],
          .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
      let b1 : Block _ _ _ :=
        ⟨⟨1⟩, [],
          [⟨orig1, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨2⟩ () (.cons (.literal ⟨[7]⟩) .nil)⟩],
          .ret (some ⟨u8Ty, .value ⟨2⟩⟩), []⟩
      let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .value ⟨2⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)], [], #[], [], []⟩
    let diamondNonDom : BIR s0Lang p :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[1]⟩)⟩],
          .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
      let b1 : Block _ _ _ :=
        ⟨⟨1⟩, [],
          [⟨orig1, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨2⟩ () (.cons (.literal ⟨[7]⟩) .nil)⟩],
          .jump ⟨⟨3⟩, []⟩, []⟩
      let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .jump ⟨⟨3⟩, []⟩, []⟩
      let b3 : Block _ _ _ := ⟨⟨3⟩, [], [], .ret (some ⟨u8Ty, .value ⟨2⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2, b3]⟩)], [], #[], [], []⟩
    let deadUseBeforeDef : SSA s0Lang p :=
      let b0 : SSABlock s0Lang := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
      let dead : SSABlock s0Lang :=
        ⟨⟨1⟩, [],
          [⟨orig0, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨1⟩ () (.cons (.value ⟨0⟩) .nil)⟩,
           ⟨orig1, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨0⟩ () (.cons (.literal ⟨[1]⟩) .nil)⟩],
          .ret (some ⟨u8Ty, .value ⟨1⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, dead]⟩)], [], [], []⟩
    let deadInvalid : SSA s0Lang p :=
      let b0 : SSABlock s0Lang := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
      let dead : SSABlock s0Lang := ⟨⟨1⟩, [], [], .invalid, []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, dead]⟩)], [], [], []⟩
    let badLitU32 : BIR s0Lang p :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig0, .addWithOverflow .u32 ⟨0⟩ ⟨1⟩ (.literal ⟨[20]⟩) (.literal ⟨[22, 0, 0, 0]⟩)⟩],
          .ret (some ⟨u32Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u32Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩
    let badTrapSlot : BIR s0Lang p := { goodU8 with sites := #[] }
    goodU8.RegisterFormation (language := s0Core) ∧
    goodU32.RegisterFormation (language := s0Core) ∧
    zeroLoop.frameSlots = 0 ∧
    zeroLoop.RegisterFormation (language := s0Core) ∧
    zeroLoop.WF (language := s0Core) ∧
    paramLoop.RegisterFormation (language := s0Core) ∧
    ¬ siblingNonDom.RegisterFormation (language := s0Core) ∧
    ¬ diamondNonDom.RegisterFormation (language := s0Core) ∧
    ¬ deadUseBeforeDef.RegisterFormation (language := s0Core) ∧
    ¬ deadInvalid.RegisterFormation (language := s0Core) ∧
    ¬ badLitU32.RegisterFormation (language := s0Core) ∧
    ¬ badTrapSlot.RegisterFormation (language := s0Core) := by
  intro p p0 goodU8 goodU32 zeroLoop paramLoop siblingNonDom diamondNonDom
    deadUseBeforeDef deadInvalid badLitU32 badTrapSlot
  have hZeroView : (zeroLoop.memoryView (language := s0Core)) =
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .jump ⟨⟨0⟩, []⟩, []⟩]⟩)],
        [], [(ctrlMain, [])], [], []⟩ := by
    with_unfolding_all rfl
  have hZeroMemWF : (zeroLoop.memoryView (language := s0Core)).HandoffWF := by
    rw [hZeroView]
    have hNodup1 : [ctrlMain].Nodup := List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩
    refine ⟨?_, List.mem_cons_self, (fun _ h => nomatch h), (fun _ h => nomatch h),
      ⟨List.nodup_nil, ⟨List.nodup_nil, (fun _ h => nomatch h)⟩, (fun _ _ h => nomatch h)⟩,
      ⟨List.nodup_nil, (fun _ _ h => nomatch h)⟩⟩
    refine ⟨hNodup1, hNodup1, hNodup1, List.nodup_nil,
      (fun _ _ h => by obtain ⟨rfl, rfl⟩ := mem_singleton_pair h; exact List.nodup_nil),
      (fun _ _ h => nomatch h), ?_⟩
    intro fn ls gr h
    obtain ⟨rfl, rfl, rfl⟩ := mem_singleton_triple h
    refine ⟨List.nodup_nil, ⟨by decide, ⟨_, List.mem_cons_self, rfl⟩, ?_⟩,
      ⟨⟨[], some u8Ty⟩, rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩, ?_⟩⟩
    · intro blk hblk edge hedge
      rcases List.mem_singleton.mp hblk with rfl
      rcases List.mem_singleton.mp hedge with rfl
      exact ⟨_, List.mem_cons_self, rfl, rfl⟩
    · intro blk hblk
      rcases List.mem_singleton.mp hblk with rfl
      exact ⟨trivial, fun _ h => nomatch h⟩
  have hZeroWF : zeroLoop.WF (language := s0Core) := by
    refine ⟨by decide, ?_, by decide, ?_, (fun _ h => nomatch h), hZeroMemWF⟩
    · intro fn hfn blk hblk slot htrap
      rcases List.mem_singleton.mp hfn with rfl
      rcases List.mem_singleton.mp hblk with rfl
      cases htrap
    · intro orig h
      have h' : orig ∈ ([] : List Origin) := by with_unfolding_all exact h
      cases h'
  exact ⟨by decide, by decide, rfl, by decide, hZeroWF, by decide,
    by decide, by decide, by decide, by decide, by decide, by decide⟩

/-! ## Section 4: `firstUnsupported` (`u8` vs `u32`), selected emitter, calls, helper jobs,
and LLVM-success / LCB-resource contrast -/

private def birU32 (profile : Profile) : BIR s0Lang profile :=
  let b0 : Block _ _ _ :=
    ⟨⟨0⟩, [],
      [⟨orig0, .addWithOverflow .u32 ⟨0⟩ ⟨1⟩ (.literal ⟨[20, 0, 0, 0]⟩) (.literal ⟨[22, 0, 0, 0]⟩)⟩],
      .ret (some ⟨u32Ty, .value ⟨0⟩⟩), []⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u32Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩

private def birBodyU32InU8 (profile : Profile) : BIR s0Lang profile :=
  let b0 : Block _ _ _ :=
    ⟨⟨0⟩, [],
      [⟨orig0, .addWithOverflow .u32 ⟨0⟩ ⟨1⟩ (.literal ⟨[1, 0, 0, 0]⟩) (.literal ⟨[2, 0, 0, 0]⟩)⟩],
      .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩

private def birUnreachCall (profile : Profile) : BIR s0Lang profile :=
  let b0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
  let dead : Block _ _ _ :=
    ⟨⟨1⟩, [], [⟨orig1, .call ctrlHelper (some ⟨u8Ty, ⟨0⟩⟩) []⟩],
      .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
  let h0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[1]⟩⟩), []⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[b0, dead]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0]⟩)], [], #[], [], []⟩

private def birReachCall (profile : Profile) : BIR s0Lang profile :=
  let b0 : Block _ _ _ :=
    ⟨⟨0⟩, [], [⟨orig1, .call ctrlHelper (some ⟨u8Ty, ⟨0⟩⟩) []⟩],
      .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
  let h0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[1]⟩⟩), []⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0]⟩)], [], #[], [], []⟩

private def lcbWordFn (n : Nat) : Source.Function :=
  ⟨.word, #[], #[], #[⟨#[], #[], .ret (.literal (.word (BitVec.ofNat 32 n)))⟩]⟩

private def dummyLLVMOut (p : Profile) : LLVMInput p :=
  ⟨⟨[llvmBodyDefinition, .label .entry,
     .terminator (.ret (.integer .u32, .immediate (.natural 42))), .endFunction]⟩,
   ⟨launcherItems []⟩, #[]⟩

private def dummyLCBOut (p : Profile) (extra : AList FnId FunctionJob := []) : LCBInput p :=
  ⟨lcbWordFn 42, extra, #[], (Runtime.qbeDiagnostics []).toUTF8⟩

/-- Control 4a & 4b: `u8` vs `u32` across LCB and LLVM (`firstUnsupported`, `firstUnsupportedRule`,
`emitGuarded`), plus unreachable vs entry-reachable calls (`requiresLCBLinking` precedes
`firstUnsupported` on LCB, `CallSupported` fires on LLVM). -/
theorem firstUnsupported_and_emitGuarded_controls :
    let lcb := lcbProfile
    let llvm := llvmProfile
    let u8LCB := birSingleAdd lcb
    let u8LLVM := birSingleAdd llvm
    let u32LCB := birU32 lcb
    let u32LLVM := birU32 llvm
    let bodyU32LCB := birBodyU32InU8 lcb
    let unreachLCB := birUnreachCall lcb
    let reachLCB := birReachCall lcb
    let reachLLVM := birReachCall llvm
    -- Width policy: `u8` accepted on both, `u32` accepted on LLVM and refused on LCB (rev 1).
    firstUnsupported (language := s0Core) u8LCB = none ∧
    firstUnsupported (language := s0Core) u8LLVM = none ∧
    firstUnsupported (language := s0Core) u32LLVM = none ∧
    firstUnsupported (language := s0Core) u32LCB =
      some (.unsupportedWidth .u32 ctrlMain .signatureResult) ∧
    firstUnsupported (language := s0Core) bodyU32LCB =
      some (.unsupportedWidth .u32 ctrlMain (.instruction ⟨0⟩ 0)) ∧
    (firstUnsupportedRule (language := s0Core) u32LCB).map Subtype.val =
      some ⟨`ConcreteSpec.Core.BackendPolicy.IntWidthSupported, 1⟩ ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.IntWidthSupported, 2⟩).isNone = true ∧
    -- `emitGuarded` enforces `firstUnsupported` on `u32` LCB while allowing `u32` LLVM:
    (emitGuarded (language := s0Core) (profile := lcb) (fun _ => .ok (dummyLCBOut lcb)) u32LCB).isOk = false ∧
    (emitGuarded (language := s0Core) (profile := llvm) (fun _ => .ok (dummyLLVMOut llvm)) u32LLVM).isOk = true ∧
    -- Unreachable call allowed (`requiresLCBLinking = false`, `firstUnsupported = none`);
    -- entry-reachable call refused with `.lcbLinkingUnavailable` on LCB and `.unsupportedFeature` on LLVM.
    unreachLCB.requiresLCBLinking = false ∧
    firstUnsupported (language := s0Core) unreachLCB = none ∧
    reachLCB.requiresLCBLinking = true ∧
    firstUnsupported (language := s0Core) reachLCB =
      some (.unsupportedCall .reachableInternalCall (some ctrlHelper) ctrlMain (.instruction ⟨0⟩ 0)) ∧
    (match emitGuarded (language := s0Core) (profile := lcb) (fun _ => .ok (dummyLCBOut lcb)) reachLCB with
      | .error .lcbLinkingUnavailable => true
      | _ => false) = true ∧
    (match emitGuarded (language := s0Core) (profile := llvm) (fun _ => .ok (dummyLLVMOut llvm)) reachLLVM with
      | .error (.unsupportedFeature _ (.unsupportedCall .reachableInternalCall (some fn) _ _)) => fn == ctrlHelper
      | _ => false) = true := by
  decide

/-- Control 4c: An always-refusing emitter fails `emitEntry.Validated` on BOTH `.llvm` and `.lcb`
on supported `u8` BIR (`birSingleAdd`), and `emitEntry.refusal` rejects wrong rule IDs and wrong
revisions on `u32` LCB. -/
theorem always_refusing_emitter_and_wrong_rule_rejected :
    let lcb := lcbProfile
    let llvm := llvmProfile
    let badPassesLCB : Passes slice0 lcb :=
      { mkBudgetPasses lcb with emit := fun _ => .error (.unsupportedProfile ⟨`bogus, 99⟩) }
    let badPassesLLVM : Passes slice0 llvm :=
      { mkBudgetPasses llvm with emit := fun _ => .error (.unsupportedProfile ⟨`bogus, 99⟩) }
    ¬ (registry badPassesLCB).emit.Validated ∧
    ¬ (registry badPassesLLVM).emit.Validated ∧
    ¬ (registry badPassesLCB).emit.refusal (birU32 lcb)
        (.unsupportedProfile ⟨BackendPolicy.intWidthRule.name, 2⟩) ∧
    ¬ (registry badPassesLCB).emit.refusal (birU32 lcb)
        (.unsupportedFeature lcb (.unsupportedOperation .addWithOverflow ctrlMain .signatureResult)) := by
  intro lcb llvm badPassesLCB badPassesLLVM
  refine ⟨?_, ?_, ?_, ?_⟩
  · intro hVal
    have hRef := hVal.2 (birSingleAdd lcb) (.unsupportedProfile ⟨`bogus, 99⟩)
      (birSingleAdd_WF lcb (by decide) (by decide)) (by with_unfolding_all rfl)
    rcases hRef with ⟨_, hEq⟩ | ⟨_, _, hEq⟩ | ⟨_, _, _, _, hEq⟩ | ⟨_, _, _, _, _, _, _, _, hEq⟩ <;> cases hEq
  · intro hVal
    have hRef := hVal.2 (birSingleAdd llvm) (.unsupportedProfile ⟨`bogus, 99⟩)
      (birSingleAdd_WF llvm (by decide) (by decide)) (by with_unfolding_all rfl)
    obtain ⟨_, _, hEq⟩ := hRef
    cases hEq
  · intro hRef
    rcases hRef with ⟨_, hEq⟩ | ⟨_, _, hEq⟩ | ⟨_, _, _, _, hEq⟩ | ⟨_, _, _, _, _, _, _, _, hEq⟩ <;> cases hEq
  · intro hRef
    rcases hRef with ⟨_, hEq⟩ | ⟨_, _, hEq⟩ | ⟨_, _, cause, hFU, hEq⟩ | ⟨_, _, _, _, _, _, _, _, hEq⟩
    · cases hEq
    · cases hEq
    · have hActual : firstUnsupported (language := s0Core) (birU32 lcb) =
          some (.unsupportedWidth .u32 ctrlMain .signatureResult) := by decide
      change firstUnsupported (language := s0Core) (birU32 lcb) = some cause at hFU
      rw [hActual] at hFU
      cases Option.some.inj hFU
      cases hEq
    · cases hEq

/-- Control 4d: Missing helper job fails `LCBEncoding` and `EmitsLCB`, while supplying the helper's
keyed `FunctionJob` with a distinct symbol passes `EmitsLCB`. -/
theorem missing_helper_job_rejected_and_present_accepted :
    let lcb := lcbProfile
    let birTwoFn := birUnreachCall lcb
    let missingHelperOut : LCBInput lcb := dummyLCBOut lcb []
    let helperJob : FunctionJob :=
      { code :=
        { symbol := { namespaceName := "concrete", name := "concrete_helper", exported := false }
          body := lcbWordFn 1
          abi := planAbi (lcbWordFn 1)
          layout := planFrame (lcbWordFn 1) } }
    let fullOut : LCBInput lcb := dummyLCBOut lcb [(ctrlHelper, helperJob)]
    ¬ LCBEncoding s0Core birTwoFn missingHelperOut ∧
    ¬ EmitsLCB s0Core birTwoFn missingHelperOut ∧
    EmitsLCB s0Core birTwoFn fullOut := by
  intro lcb birTwoFn missingHelperOut helperJob fullOut
  have hMissing : ¬ LCBEncoding s0Core birTwoFn missingHelperOut := by
    intro hEnc
    have hMem : ctrlHelper ∈ birTwoFn.entry :: List.map Prod.fst missingHelperOut.additionalJobs :=
      (hEnc.2.2.2.2.2.2.1 ctrlHelper).mp (List.mem_cons_of_mem _ List.mem_cons_self)
    rcases List.mem_singleton.mp hMem with hEq
    exact (by decide : ctrlHelper ≠ ctrlMain) hEq
  have hRef : BehaviorRefinement (BIR.Execution (language := s0Core))
      (LCBInput.Execution (profile := lcb) s0Core.operands.Event birTwoFn.project)
      (BIR.Execution (language := s0Core)).result
      (LCBInput.Execution (profile := lcb) s0Core.operands.Event birTwoFn.project).result
      birTwoFn fullOut := by
    let p0 := mkRegState lcb 0 0 [] [] []
    let q0 : Option Source.State := some ⟨0, 0, #[]⟩
    have hpRes : (BIR.Execution (language := s0Core)).result birTwoFn p0 (retObs 42) := by
      with_unfolding_all rfl
    have hqRes : (LCBInput.Execution (profile := lcb) s0Core.operands.Event birTwoFn.project).result
        fullOut q0 (retObs 42) := by
      with_unfolding_all rfl
    exact (DetChain.nil q0).refines ⟨p0, by with_unfolding_all rfl, .nil p0⟩ hpRes
      (fun _ h => by with_unfolding_all exact h)
      (fun o => ⟨fun h => (Option.some.inj (hqRes.symm.trans h)).symm, fun h => h ▸ hqRes⟩)
      (by with_unfolding_all rfl)
  have hEmits : EmitsLCB s0Core birTwoFn fullOut := by
    refine ⟨rfl, by cbv, ?_, ?_, rfl, ⟨[], rfl, rfl⟩, hRef,
      by decide, by decide, ?_, by decide, ?_⟩
    · intro pair hpair
      rcases List.mem_singleton.mp hpair with rfl
      cbv
    · exact List.nodup_cons.mpr ⟨by decide, List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩⟩
    · intro fn; simp [birTwoFn, birUnreachCall, fullOut, dummyLCBOut, List.map]
    · intro fn hfn blk hblk slot htrap
      rcases List.mem_cons.mp hfn with rfl | hfn2
      · rcases List.mem_cons.mp hblk with rfl | hblk2
        · cases htrap
        · rcases List.mem_singleton.mp hblk2 with rfl; cases htrap
      · rcases List.mem_singleton.mp hfn2 with rfl
        rcases List.mem_singleton.mp hblk with rfl; cases htrap
  exact ⟨hMissing, fun h => hMissing h.2.2.2, hEmits⟩

/-- Control 4e (LLVM-success / LCB-resource fixture): for ANY supported BIR (`firstUnsupported = none`)
whose trap table exceeds `LCBInput.trapCapacity`, `emitGuarded` on `.lcb` unconditionally refuses with
`.resource (LCBInput.trapCapacityRefusal bir.sites.size)` (satisfying `emitEntry.refusal`), while on
`.llvm` `emitGuarded` does not hit the LCB word-capacity bound and succeeds via `emitFn`. -/
theorem llvm_success_vs_lcb_trapCapacity_resource
    (birLCB : BIR s0Lang (lcbProfile)) (birLLVM : BIR s0Lang (llvmProfile))
    (hOverLCB : LCBInput.trapCapacity < birLCB.sites.size)
    (hSupLLVM : firstUnsupported (language := s0Core) birLLVM = none)
    (outLLVM : LLVMInput (llvmProfile))
    (emitLCB : BIR s0Lang (lcbProfile) → Except StageFailure (LCBInput (lcbProfile))) :
    let passesLCB : Passes slice0 (lcbProfile) := { mkBudgetPasses (lcbProfile) with emit := emitLCB }
    let passesLLVM : Passes slice0 (llvmProfile) := { mkBudgetPasses (llvmProfile) with emit := fun _ => .ok outLLVM }
    (registry passesLCB).emit.run birLCB =
      .error (.resource (LCBInput.trapCapacityRefusal birLCB.sites.size)) ∧
    (registry passesLCB).emit.refusal birLCB
      (.resource (LCBInput.trapCapacityRefusal birLCB.sites.size)) ∧
    (registry passesLLVM).emit.run birLLVM = .ok outLLVM := by
  intro passesLCB passesLLVM
  refine ⟨?_, Or.inl ⟨hOverLCB, rfl⟩, ?_⟩
  · dsimp only [registry, PassFunctions.emitEntry, lcbProfile, emitGuarded]
    exact ite_eq_left hOverLCB
  · have hEq : firstUnsupported (language := sliceLanguage slice0) (lang := sliceLoweringLanguage slice0)
        (profile := ⟨.llvm, .x86_64Linux, 64, 4096⟩) birLLVM = none := hSupLLVM
    dsimp only [registry, PassFunctions.emitEntry, llvmProfile, emitGuarded]
    rw [hEq]; rfl

end StageInterfaceControls.CheckedAddControls
