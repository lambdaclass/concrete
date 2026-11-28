import Concrete.Interfaces.Pipeline
import Concrete.Lex.StreamProofs
import Concrete.IntegratedCore
import Concrete.Parse

/-!
# Slice 0 freeze controls (unit F9c): `passesAt` folds, `RoutePipeline` indexing, and `AccountsFor`

Standalone freeze controls written directly against the frozen interface modules
(`Concrete.Interfaces.*`, `Concrete.Lex.StreamProofs`, `Concrete.IntegratedCore`,
`Concrete.Parse`), exercising:
1. `RoutePipeline` and `Registry` privacy & cross-index rejections:
   - `RoutePipeline.mk` and `Registry.mk` are private and cannot be forged;
   - cross-slice, cross-route, cross-backend, cross-BIR-slot-budget, cross-stack-budget, and
     cross-platform substitutions are rejected at elaboration time on both `.checked` and
     `.unchecked` routes.
2. All 4 `passesAt` folds (`.mir`, `.memory`, `.ssa`, `.bir`) via `Registry.pipeline` on BOTH
   `.checked` and `.unchecked` routes over real Slice 0 source bytes (`"fn main() -> u8 { return 42; }"`):
   - empty pass lists on both routes;
   - each of the 4 insertion points individually and all 4 nonidentity block-renaming passes
     simultaneously (`+1`, `+10`, `+100`, `+1000` → block `1111`) on both routes;
   - two order-sensitive passes (`[addOne, double]` → `4` vs `[double, addOne]` → `3`, `4 ≠ 3`)
     on both `.checked` and `.unchecked` routes;
   - identity fallback on declined (`none`) candidates at all 4 points on both routes;
   - genuine nonidentity block-renaming `PassPost`, `PassPost.compose`, `(pass.toStage).Validated`,
     and `foldPasses.Validated` at all 4 insertion points with non-empty remapped `Occurrence`s.
3. `AccountsFor`, `PassPost`, and `IRPass.toStage.Validated` negative & positive controls:
   - swapped trap origins (both position-wise witness failing `AccountsFor` and cross-mapped witness
     failing `BehaviorRefinement` / `PassPost` / `Validated`);
   - lost shared-origin occurrence (same `Origin` retained via another occurrence, still rejected by
     `AccountsFor`, `PassPost`, and `Validated`);
   - false elimination reasons: live trap marked `.deadValue`, used pure value marked `.deadValue`,
     reachable block marked `.unreachableBlock`, and self-justifying dead-value cycle
     (`x = read y; y = read x`) all rejected; genuine unused pure value (`.deadValue`) and
     genuinely unreachable block (`.unreachableBlock`) accepted.
4. Helper-function elimination and mutation controls:
   - uncalled helper eliminated with `.unreachableFunction` passes `AccountsFor` and `PassPost`;
   - called helper eliminated with `.unreachableFunction` fails `AccountsFor` and `PassPost`;
   - uncalled helper mutated (`1 → 99`) passes `AccountsFor` and `PassPost`;
   - called helper mutated (`1 → 99`) passes `AccountsFor` and `BIR.WF` but fails
     `BehaviorRefinement`, `PassPost`, and `(pass.toStage).Validated`.
-/

namespace StageInterfaceControls.PassListControls

open Concrete.Contracts
open Concrete.Interfaces
open Concrete.Backend
open ConcreteSpec.Core
open ConcreteSpec.Core.SourcePathSpan
open ConcreteSpec.StageInterface
open ConcreteSpec.StageGrammar

private abbrev s0Core : CoreLanguage := sliceLanguage slice0
private abbrev s0Lang : LoweringLanguage s0Core.operands.Ty := sliceLoweringLanguage slice0
private abbrev u8Ty : s0Core.operands.Ty := s0Lang.intTy .u8
private abbrev boolTy : s0Core.operands.Ty := s0Lang.bool

private def src42Bytes : ByteArray := "fn main() -> u8 { return 42; }".toUTF8
private def src42 : Source := Concrete.Parse.sourceOfBytes src42Bytes

private def ctrlProj : Project := src42.project
private def ctrlMain : FnId :=
  ⟨⟨⟨⟨"pkg"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩, ⟨"main"⟩⟩
private def ctrlHelper : FnId := { ctrlMain with name := ⟨"helper"⟩ }

-- Byte 18..24 is `return` (line 1, col 19); byte 25..27 is `42` (line 1, col 26) in `src42Bytes`.
private def span0 : FileSpan := ⟨⟨0⟩, 18, 24, 1, 19⟩
private def span1 : FileSpan := ⟨⟨0⟩, 25, 27, 1, 26⟩

private def orig0 : Origin := ⟨⟨0⟩, span0, ⟨.function ctrlMain, ⟨[]⟩⟩⟩
private def orig1 : Origin := ⟨⟨1⟩, span1, ⟨.function ctrlMain, ⟨[]⟩⟩⟩
private def origHelper : Origin := ⟨⟨2⟩, span1, ⟨.function ctrlHelper, ⟨[]⟩⟩⟩

private def llvmProfile : Profile := ⟨.llvm, .x86_64Linux, 64, 4096⟩

private def trapLine0 : String :=
  s!"concrete: trap: overflow at {mainSourcePath.render.text}:1:19 (site 0)\n"
private def trapLine1 : String :=
  s!"concrete: trap: overflow at {mainSourcePath.render.text}:1:26 (site 1)\n"

private def trapObs0 : Concrete.Contracts.Observation :=
  .trapped 134 .overflow ⟨⟨0⟩, ⟨mainSourcePath.render.text, 1, 19⟩⟩ trapLine0.toUTF8
private def trapObs1 : Concrete.Contracts.Observation :=
  .trapped 134 .overflow ⟨⟨1⟩, ⟨mainSourcePath.render.text, 1, 26⟩⟩ trapLine1.toUTF8
private def retObs (n : UInt8) : Concrete.Contracts.Observation :=
  .returned n ByteArray.empty

private theorem span0_binds : ∃ sp, orig0.lexical.Binds ctrlProj sp := by
  refine ⟨⟨⟨0⟩, 18, 24, by decide, 1, 19, by decide, by decide⟩,
    File.load mainSourcePath (SourceText.ofBytes src42Bytes.data.toList),
    rfl, rfl, ?_, rfl, rfl, rfl, rfl⟩
  decide

private theorem span1_binds : ∃ sp, orig1.lexical.Binds ctrlProj sp := by
  refine ⟨⟨⟨0⟩, 25, 27, by decide, 1, 26, by decide, by decide⟩,
    File.load mainSourcePath (SourceText.ofBytes src42Bytes.data.toList),
    rfl, rfl, ?_, rfl, rfl, rfl, rfl⟩
  decide

private theorem mem_singleton_pair {α β : Type} {a a' : α} {b b' : β}
    (h : (a, b) ∈ [(a', b')]) : a = a' ∧ b = b' :=
  Prod.mk.inj (List.mem_singleton.mp h)

private theorem mem_singleton_triple {α β γ : Type} {a a' : α} {b b' : β} {c c' : γ}
    (h : (a, b, c) ∈ [(a', b', c')]) : a = a' ∧ b = b' ∧ c = c' :=
  let ⟨h1, h23⟩ := Prod.mk.inj (List.mem_singleton.mp h)
  ⟨h1, Prod.mk.inj h23⟩

private theorem writeParams_nil_eq {profile : Profile} {mem : Memory s0Lang profile}
    {s1 s2 : MemoryRegisters s0Core profile} {ceil : Nat}
    (h : mem.WriteParams [] [] s1 s2 false ceil) : s2 = s1 := by
  cases h; rfl

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
      exact ih (k + 1) (huniq _ (hk ▸ hstep k))
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
  exact hne ((hpChain.observes_iff hpInit hpInitUniq hpEnd obsT).mp ⟨p, hpFin, hpRes⟩)

private def mkRegState (profile : Profile) (fn : FnId) (blk : Nat) (pc : Nat)
    (live init : List PlaceId) (vals : AList PlaceId EarlyValue)
    (callers : List (MemoryContinuation EarlyTy EarlyValue) := []) : MemoryRegisters s0Core profile :=
  ⟨⟨fn, ⟨blk⟩, pc, vals, [], live, init, [], [], []⟩, Mem.Memory.empty, callers, [], {}, 0, false⟩

/-! ## Section 1: `RoutePipeline` and `Registry` privacy & cross-index rejections -/

-- Private constructors `RoutePipeline.mk` and `Registry.mk` cannot be forged outside `Pipeline.lean`,
-- either by explicit constructor name or via anonymous constructor notation `⟨...⟩`.

/--
error: Unknown constant `Concrete.Interfaces.RoutePipeline.mk`
-/
#guard_msgs in
example (_pLCB : Pipeline Source (BackendOutput ⟨.lcb, .x86_64Linux, 8, 4096⟩)) :
    RoutePipeline slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩ .checked :=
  RoutePipeline.mk _pLCB

/--
error: Unknown constant `Concrete.Interfaces.RoutePipeline.mk`
-/
#guard_msgs in
example (_pLCB : Pipeline Source (BackendOutput ⟨.lcb, .x86_64Linux, 8, 4096⟩)) :
    RoutePipeline slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩ .unchecked :=
  RoutePipeline.mk _pLCB

/--
error: Unknown constant `Concrete.Interfaces.RoutePipeline.mk`
-/
#guard_msgs in
example (_pLLVM : Pipeline Source (BackendOutput ⟨.llvm, .x86_64Linux, 8, 4096⟩)) :
    RoutePipeline slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩ .checked :=
  RoutePipeline.mk _pLLVM

/--
error: Unknown constant `Concrete.Interfaces.RoutePipeline.mk`
-/
#guard_msgs in
example (_pLLVM : Pipeline Source (BackendOutput ⟨.llvm, .x86_64Linux, 8, 4096⟩)) :
    RoutePipeline slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩ .unchecked :=
  RoutePipeline.mk _pLLVM

/--
error: Invalid `⟨...⟩` notation: Constructor for `Concrete.Interfaces.RoutePipeline` is marked as private
-/
#guard_msgs in
example (_pLCB : Pipeline Source (BackendOutput ⟨.lcb, .x86_64Linux, 8, 4096⟩)) :
    RoutePipeline slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩ .checked :=
  ⟨_pLCB⟩

/--
error: Invalid `⟨...⟩` notation: Constructor for `Concrete.Interfaces.RoutePipeline` is marked as private
-/
#guard_msgs in
example (_pLLVM : Pipeline Source (BackendOutput ⟨.llvm, .x86_64Linux, 8, 4096⟩)) :
    RoutePipeline slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩ .unchecked :=
  ⟨_pLLVM⟩

/--
error: Unknown constant `Concrete.Interfaces.Registry.mk`
-/
#guard_msgs in
#check Registry.mk

/--
error: Invalid `⟨...⟩` notation: Constructor for `Concrete.Interfaces.Registry` is marked as private
-/
#guard_msgs in
example : Registry slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩ :=
  ⟨⟩

-- Positive baseline controls: matching indices elaborate via `Registry.pipeline`.
example (passes : Passes slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩ .checked :=
  Registry.pipeline passes .checked

example (passes : Passes slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩ .unchecked :=
  Registry.pipeline passes .unchecked

-- Cross-index rejections on `.checked` (`slice`, `route`, `backend`, `maxBIRSlots`,
-- `maxImageStackBytes`, and `backendTarget` / platform).

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.lcb, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.checked
but is expected to have type
  RoutePipeline sliceOne
    { backend := BackendKind.lcb, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.checked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline sliceOne ⟨.lcb, .x86_64Linux, 8, 4096⟩ .checked :=
  let rp := Registry.pipeline passes .checked
  rp

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.lcb, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.checked
but is expected to have type
  RoutePipeline slice0
    { backend := BackendKind.lcb, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.unchecked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩ .unchecked :=
  let rp := Registry.pipeline passes .checked
  rp

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.lcb, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.checked
but is expected to have type
  RoutePipeline slice0
    { backend := BackendKind.llvm, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.checked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩ .checked :=
  let rp := Registry.pipeline passes .checked
  rp

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.lcb, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.checked
but is expected to have type
  RoutePipeline slice0
    { backend := BackendKind.lcb, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 4,
      maxImageStackBytes := 4096 }
    Route.checked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.lcb, .x86_64Linux, 4, 4096⟩ .checked :=
  let rp := Registry.pipeline passes .checked
  rp

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.lcb, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.checked
but is expected to have type
  RoutePipeline slice0
    { backend := BackendKind.lcb, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 8192 }
    Route.checked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.lcb, .x86_64Linux, 8, 8192⟩ .checked :=
  let rp := Registry.pipeline passes .checked
  rp

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.lcb, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.checked
but is expected to have type
  RoutePipeline slice0
    { backend := BackendKind.lcb, backendTarget := LCB.Target.aarch64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.checked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.lcb, .aarch64Linux, 8, 4096⟩ .checked :=
  let rp := Registry.pipeline passes .checked
  rp

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.lcb, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.checked
but is expected to have type
  RoutePipeline slice0
    { backend := BackendKind.lcb, backendTarget := LCB.Target.aarch64Macos, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.checked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.lcb, .aarch64Macos, 8, 4096⟩ .checked :=
  let rp := Registry.pipeline passes .checked
  rp

-- Cross-index rejections on `.unchecked` and `.llvm`.

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.llvm, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.unchecked
but is expected to have type
  RoutePipeline sliceOne
    { backend := BackendKind.llvm, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.unchecked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline sliceOne ⟨.llvm, .x86_64Linux, 8, 4096⟩ .unchecked :=
  let rp := Registry.pipeline passes .unchecked
  rp

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.llvm, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.unchecked
but is expected to have type
  RoutePipeline slice0
    { backend := BackendKind.llvm, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.checked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩ .checked :=
  let rp := Registry.pipeline passes .unchecked
  rp

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.llvm, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.unchecked
but is expected to have type
  RoutePipeline slice0
    { backend := BackendKind.lcb, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.unchecked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩ .unchecked :=
  let rp := Registry.pipeline passes .unchecked
  rp

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.llvm, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.unchecked
but is expected to have type
  RoutePipeline slice0
    { backend := BackendKind.llvm, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 4,
      maxImageStackBytes := 4096 }
    Route.unchecked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.llvm, .x86_64Linux, 4, 4096⟩ .unchecked :=
  let rp := Registry.pipeline passes .unchecked
  rp

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.llvm, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.unchecked
but is expected to have type
  RoutePipeline slice0
    { backend := BackendKind.llvm, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 8192 }
    Route.unchecked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.llvm, .x86_64Linux, 8, 8192⟩ .unchecked :=
  let rp := Registry.pipeline passes .unchecked
  rp

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.llvm, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.unchecked
but is expected to have type
  RoutePipeline slice0
    { backend := BackendKind.llvm, backendTarget := LCB.Target.aarch64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.unchecked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.llvm, .aarch64Linux, 8, 4096⟩ .unchecked :=
  let rp := Registry.pipeline passes .unchecked
  rp

/--
error: Type mismatch
  rp
has type
  RoutePipeline slice0
    { backend := BackendKind.llvm, backendTarget := LCB.Target.x86_64Linux, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.unchecked
but is expected to have type
  RoutePipeline slice0
    { backend := BackendKind.llvm, backendTarget := LCB.Target.aarch64Macos, maxBIRSlots := 8,
      maxImageStackBytes := 4096 }
    Route.unchecked
-/
#guard_msgs in
example (passes : Passes slice0 ⟨.llvm, .x86_64Linux, 8, 4096⟩) :
    RoutePipeline slice0 ⟨.llvm, .aarch64Macos, 8, 4096⟩ .unchecked :=
  let rp := Registry.pipeline passes .unchecked
  rp

/-! ## Section 2: All 4 `passesAt` folds via `Registry.pipeline` on both routes -/

private def renameTerm {A C : Type} (f : Nat → Nat) : Terminator A C → Terminator A C
  | .ret v => .ret v
  | .jump e => .jump { e with target := ⟨f e.target.id⟩ }
  | .branch t y n => .branch t { y with target := ⟨f y.target.id⟩ } { n with target := ⟨f n.target.id⟩ }
  | .trap k o => .trap k o
  | .invalid => .invalid

private def renameBIRTerm {A C : Type} (f : Nat → Nat) : BIRTerm A C → BIRTerm A C
  | .ret v => .ret v
  | .jump e => .jump { e with target := ⟨f e.target.id⟩ }
  | .branch t y n => .branch t { y with target := ⟨f y.target.id⟩ } { n with target := ⟨f n.target.id⟩ }
  | .trap s => .trap s

private def renameCFG {P I T : Type} (f : Nat → Nat) (mapT : T → T) (g : CFG P I T) : CFG P I T :=
  ⟨⟨f g.entry.id⟩, g.blocks.map fun blk => { blk with id := ⟨f blk.id.id⟩, ending := mapT blk.ending }⟩

private def renameAccount (f : Nat → Nat) (occs : List (Occurrence × Origin)) : Option Account :=
  Account.mkChecked (occs.map fun (occ, _) => ({ occ with block := ⟨f occ.block.id⟩ }, [occ])) []

private def renameMIRPass (f : Nat → Nat) : IRPass s0Lang llvmProfile .mir := fun mir =>
  let out : MIR s0Lang llvmProfile :=
    { mir with functions := List.map (fun e => (e.1, e.2.1, renameCFG f (renameTerm f) e.2.2)) mir.functions }
  (renameAccount f (AtLang.occurrences (point := .mir) mir)).map fun acc => (out, acc)

private def renameMemoryPass (f : Nat → Nat) : IRPass s0Lang llvmProfile .memory := fun mem =>
  let out : Memory s0Lang llvmProfile :=
    { mem with functions := List.map (fun e => (e.1, e.2.1, renameCFG f (renameTerm f) e.2.2)) mem.functions }
  (renameAccount f (AtLang.occurrences (point := .memory) mem)).map fun acc => (out, acc)

private def renameSSAPass (f : Nat → Nat) : IRPass s0Lang llvmProfile .ssa := fun ssa =>
  let out : SSA s0Lang llvmProfile :=
    { ssa with functions := List.map (fun e => (e.1, e.2.1, renameCFG f (renameTerm f) e.2.2)) ssa.functions }
  (renameAccount f (AtLang.occurrences (point := .ssa) ssa)).map fun acc => (out, acc)

private def renameBIRPass (f : Nat → Nat) : IRPass s0Lang llvmProfile .bir := fun bir =>
  let out : BIR s0Lang llvmProfile :=
    { bir with functions := List.map (fun e => (e.1, e.2.1, renameCFG f (renameBIRTerm f) e.2.2)) bir.functions }
  (renameAccount f (AtLang.occurrences (point := .bir) bir)).map fun acc => (out, acc)

private def mirBlock (b : Nat) : MIR s0Lang llvmProfile :=
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨b⟩, #[⟨⟨b⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩]⟩)], [], []⟩

private def memBlock (b : Nat) : Memory s0Lang llvmProfile :=
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨b⟩, #[⟨⟨b⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩]⟩)],
    [], [(ctrlMain, [])], [], []⟩

private def ssaBlock (b : Nat) : SSA s0Lang llvmProfile :=
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨b⟩, #[⟨⟨b⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩]⟩)], [], [], []⟩

private def birBlock (b : Nat) : BIR s0Lang llvmProfile :=
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨b⟩, #[⟨⟨b⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩]⟩)],
    [], #[], [], []⟩

private def renameBoundaryAcc (b1 b2 : Nat) : Account where
  mapped := [(⟨ctrlMain, ⟨b2⟩, .boundary 0⟩, [⟨ctrlMain, ⟨b1⟩, .boundary 0⟩])]
  eliminated := []
  mappedKeysNodup := List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩
  mappedSourcesNodup := by
    intro e he; rcases List.mem_singleton.mp he with rfl
    exact List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩
  eliminatedKeysNodup := List.nodup_nil
  disjoint := fun _ _ _ _ => rfl

private theorem renameAccount_singleton (f : Nat → Nat) (b : Nat) :
    renameAccount f [(⟨ctrlMain, ⟨b⟩, .boundary 0⟩, orig0)] = some (renameBoundaryAcc b (f b)) := by
  unfold renameAccount
  exact (Account.mkChecked_eq_some_iff _ _ (renameBoundaryAcc b (f b))).mpr ⟨rfl, rfl⟩

private theorem renameMIRPass_mirBlock (f : Nat → Nat) (b : Nat) :
    renameMIRPass f (mirBlock b) = some (mirBlock (f b), renameBoundaryAcc b (f b)) := by
  simp [renameMIRPass, mirBlock, renameCFG, renameTerm, AtLang.occurrences, MIR.occurrences,
    CFG.occurrences, Block.occurrences, Terminator.trapOrigin, renameAccount_singleton]; rfl

private theorem renameMemoryPass_memBlock (f : Nat → Nat) (b : Nat) :
    renameMemoryPass f (memBlock b) = some (memBlock (f b), renameBoundaryAcc b (f b)) := by
  simp [renameMemoryPass, memBlock, renameCFG, renameTerm, AtLang.occurrences, Memory.occurrences,
    CFG.occurrences, Block.occurrences, Terminator.trapOrigin, renameAccount_singleton]; rfl

private theorem renameSSAPass_ssaBlock (f : Nat → Nat) (b : Nat) :
    renameSSAPass f (ssaBlock b) = some (ssaBlock (f b), renameBoundaryAcc b (f b)) := by
  simp [renameSSAPass, ssaBlock, renameCFG, renameTerm, AtLang.occurrences, SSA.occurrences,
    CFG.occurrences, Block.occurrences, Terminator.trapOrigin, renameAccount_singleton]; rfl

private theorem renameBIRPass_birBlock (f : Nat → Nat) (b : Nat) :
    renameBIRPass f (birBlock b) = some (birBlock (f b), renameBoundaryAcc b (f b)) := by
  simp [renameBIRPass, birBlock, renameCFG, renameBIRTerm, AtLang.occurrences, BIR.occurrences,
    CFG.occurrences, Block.occurrences, renameAccount_singleton]; rfl

private def entryBlockId {L P I T : Type} (entry : FnId) (fns : AList FnId (L × CFG P I T)) : Nat :=
  match AList.lookup fns entry with
  | some (_, g) => g.entry.id
  | none => 99999

private def llvmTagOut (tag : Nat) : LLVMInput llvmProfile :=
  ⟨⟨[llvmBodyDefinition, .label (.block tag),
     .terminator (.ret (.integer .u32, .immediate (.natural 0))), .endFunction]⟩,
   ⟨launcherItems []⟩, #[]⟩

private theorem llvmTagOut_ne {a b : Nat} (h : a ≠ b) : llvmTagOut a ≠ llvmTagOut b := by
  intro heq
  have hi := congrArg (fun (out : LLVMInput llvmProfile) => out.component.items) heq
  cases (List.cons.inj (List.cons.inj hi).2).1
  exact h rfl

private def stream42 : TokenStream :=
  let tok := fun start stop value => (⟨⟨⟨0⟩, start, stop, 1, start + 1⟩, value⟩ : SurfaceToken)
  let raw : Array SurfaceToken := #[
    tok 0 2 (.fixed .fn rfl), tok 3 7 (.name "main"), tok 7 8 (.fixed .lparen rfl),
    tok 8 9 (.fixed .rparen rfl), tok 10 12 (.fixed .arrow rfl), tok 13 15 (.fixed .u8 rfl),
    tok 16 17 (.fixed .lbrace rfl), tok 18 24 (.fixed .return_ rfl), tok 25 27 (.integer 42 .decimal none),
    tok 27 28 (.fixed .semi rfl), tok 29 30 (.fixed .rbrace rfl)]
  let tokens := raw.map fun decoded =>
    (⟨decoded, src42Bytes.extract decoded.span.startOffset decoded.span.endOffset⟩ : RetainedToken)
  let trivia := raw.toList.zipIdx.map fun (token, i) =>
    let stop := token.span.startOffset
    let start := if i = 0 then 0 else
      ((raw[i - 1]?).map (fun previous => previous.span.endOffset)).getD 0
    if start = stop then #[] else
      #[⟨⟨⟨0⟩, start, stop, 1, start + 1⟩, src42Bytes.extract start stop⟩]
  ⟨src42, tokens, trivia.toArray.push #[]⟩

private def mkTestPasses
    (passesAt : (k : IRPoint) → List (IRPass s0Lang llvmProfile k)) :
    Passes slice0 llvmProfile where
  lex _ := .ok stream42
  elaborate := Concrete.IntegratedCore.elaborate slice0
  checkCore _ := .ok []
  lowerCore _ := .ok (mirBlock 0)
  lowerUnchecked _ := .ok (mirBlock 0)
  lowerMemory mir := .ok (memBlock (entryBlockId mir.entry mir.functions))
  buildSSA mem := .ok (ssaBlock (entryBlockId mem.entry mem.functions))
  buildBIR ssa := .ok (birBlock (entryBlockId ssa.entry ssa.functions))
  emit bir := .ok (llvmTagOut (entryBlockId bir.entry bir.functions))
  passesAt := passesAt

private theorem pipeline_next_ok {A R B C : Type} {s : Stage A R B} {rest : Pipeline B C}
    {x : A} {y : B} (h : s.run x = .ok y) :
    (Pipeline.next s rest).run x = rest.run y := by
  simp only [Pipeline.run, h]

private theorem pipeline_append_run {A B C : Type} (p1 : Pipeline A B) (p2 : Pipeline B C) (x : A) :
    (pipelineAppend p1 p2).run x = match p1.run x with
      | .ok y => p2.run y
      | .error e => .error e := by
  induction p1 with
  | done => rfl
  | next s rest ih =>
    simp only [pipelineAppend, Pipeline.run]
    cases s.run x with
    | error e => rfl
    | ok y => exact ih p2 y

private theorem emit_birBlock
    (passesAt : (k : IRPoint) → List (IRPass s0Lang llvmProfile k)) (b : Nat)
    (hSup : firstUnsupported (language := s0Core) (birBlock b) = none) :
    (registry (mkTestPasses passesAt)).emit.run (birBlock b) = .ok (llvmTagOut b) := by
  have hEq : firstUnsupported (language := sliceLanguage slice0) (lang := sliceLoweringLanguage slice0)
      (profile := ⟨.llvm, .x86_64Linux, 64, 4096⟩) (birBlock b) = none := hSup
  dsimp only [registry, PassFunctions.emitEntry, llvmProfile, emitGuarded]
  rw [hEq]
  simp only [mkTestPasses, entryBlockId, birBlock, AList.lookup]
  rfl

private theorem entryBlockId_mir (b : Nat) :
    entryBlockId (mirBlock b).entry (mirBlock b).functions = b := by
  simp [entryBlockId, mirBlock, AList.lookup]

private theorem entryBlockId_mem (b : Nat) :
    entryBlockId (memBlock b).entry (memBlock b).functions = b := by
  simp [entryBlockId, memBlock, AList.lookup]

private theorem entryBlockId_ssa (b : Nat) :
    entryBlockId (ssaBlock b).entry (ssaBlock b).functions = b := by
  simp [entryBlockId, ssaBlock, AList.lookup]

set_option maxRecDepth 4096 in
/-- Evaluate the frozen front end (lexer → parser → elaborator → route core gate → lowerCore/lowerUnchecked)
on `src42` once via `Concrete.Parse.KernelEval.sliceParser_run_eq_sparse`, yielding `mirBlock 0` before
`loweringChain` on BOTH `.checked` and `.unchecked` routes. -/
private theorem testPasses_run_eq_loweringChain
    (passesAt : (k : IRPoint) → List (IRPass s0Lang llvmProfile k)) (route : Route) :
    (Registry.pipeline (mkTestPasses passesAt) route).run src42 =
      ((registry (mkTestPasses passesAt)).mirPasses.run (mirBlock 0)).bind fun mir1 =>
      (((registry (mkTestPasses passesAt)).memoryPasses.run
          (memBlock (entryBlockId mir1.entry mir1.functions))).bind fun mem1 =>
      (((registry (mkTestPasses passesAt)).ssaPasses.run
          (ssaBlock (entryBlockId mem1.entry mem1.functions))).bind fun ssa1 =>
      (((registry (mkTestPasses passesAt)).birPasses.run
          (birBlock (entryBlockId ssa1.entry ssa1.functions))).bind fun bir1 =>
      ((registry (mkTestPasses passesAt)).emit.run bir1).mapError fun e => ⟨_, e⟩))) := by
  have hAll :
      (match (Concrete.Parse.KernelEval.runEarlySparse stream42.parserView).2 with
       | some (.ok a) =>
         match Concrete.IntegratedCore.elaborate slice0 a with
         | .ok inp =>
           match inp.prepare with
           | .ok c => (c.slice == slice0) && (inp.slice == slice0) && c.checkStatic.isOk
           | .error _ => false
         | .error _ => false
       | _ => false) = true := by decide +kernel
  cases hSparse : (Concrete.Parse.KernelEval.runEarlySparse stream42.parserView).2 with
  | none => rw [hSparse] at hAll; cases hAll
  | some pres =>
    rw [hSparse] at hAll
    cases pres with
    | error e => cases hAll
    | ok ast =>
      dsimp only at hAll
      have hParserRun : (registry (mkTestPasses passesAt)).parser.run stream42 = .ok ast := by
        apply Option.some.inj
        exact (parserEntry_run_eq_some (sliceParser slice0) (sliceParser_nonExhausted slice0) stream42).trans
          ((congrArg Prod.snd (Concrete.Parse.KernelEval.sliceParser_run_eq_sparse stream42.parserView)).trans hSparse)
      cases hElab : Concrete.IntegratedCore.elaborate slice0 ast with
      | error e => rw [hElab] at hAll; cases hAll
      | ok admIn =>
        rw [hElab] at hAll; dsimp only at hAll
        cases hPrep : admIn.prepare with
        | error e => rw [hPrep] at hAll; cases hAll
        | ok core =>
          rw [hPrep] at hAll; dsimp only at hAll
          simp only [Bool.and_eq_true, beq_iff_eq] at hAll
          cases hStat : core.checkStatic with
          | error e => rw [hStat] at hAll; cases hAll.2
          | ok u =>
            cases u
            obtain ⟨⟨hCoreSlice, hAdmSlice⟩, _⟩ := hAll
            have hLowerChain : ∀ (mir0 : MIR s0Lang llvmProfile),
                ((registry (mkTestPasses passesAt)).mirPasses.run mir0).bind (fun mir1 =>
                (((registry (mkTestPasses passesAt)).memoryPasses.run
                    (memBlock (entryBlockId mir1.entry mir1.functions))).bind fun mem1 =>
                (((registry (mkTestPasses passesAt)).ssaPasses.run
                    (ssaBlock (entryBlockId mem1.entry mem1.functions))).bind fun ssa1 =>
                (((registry (mkTestPasses passesAt)).birPasses.run
                    (birBlock (entryBlockId ssa1.entry ssa1.functions))).bind fun bir1 =>
                ((registry (mkTestPasses passesAt)).emit.run bir1).mapError fun e => ⟨_, e⟩)))) =
                (pipelineAppend (registry (mkTestPasses passesAt)).mirPasses
                  (.next (registry (mkTestPasses passesAt)).lowerMemory
                    (pipelineAppend (registry (mkTestPasses passesAt)).memoryPasses
                      (.next (registry (mkTestPasses passesAt)).buildSSA
                        (pipelineAppend (registry (mkTestPasses passesAt)).ssaPasses
                          (.next (registry (mkTestPasses passesAt)).buildBIR
                            (pipelineAppend (registry (mkTestPasses passesAt)).birPasses
                              (.next (registry (mkTestPasses passesAt)).emit .done)))))))).run mir0 := by
              intro mir0
              have hLM (m : MIR s0Lang llvmProfile) :
                  (registry (mkTestPasses passesAt)).lowerMemory.run m =
                    .ok (memBlock (entryBlockId m.entry m.functions)) := rfl
              have hBS (m : Memory s0Lang llvmProfile) :
                  (registry (mkTestPasses passesAt)).buildSSA.run m =
                    .ok (ssaBlock (entryBlockId m.entry m.functions)) := rfl
              have hBB (s : SSA s0Lang llvmProfile) :
                  (registry (mkTestPasses passesAt)).buildBIR.run s =
                    .ok (birBlock (entryBlockId s.entry s.functions)) := rfl
              rw [pipeline_append_run]
              cases (registry (mkTestPasses passesAt)).mirPasses.run mir0 with
              | error e => rfl
              | ok mir1 =>
                dsimp only [Except.bind, Pipeline.run]
                rw [hLM mir1]; dsimp only
                rw [pipeline_append_run]
                cases (registry (mkTestPasses passesAt)).memoryPasses.run (memBlock (entryBlockId mir1.entry mir1.functions)) with
                | error e => rfl
                | ok mem1 =>
                  dsimp only [Except.bind, Pipeline.run]
                  rw [hBS mem1]; dsimp only
                  rw [pipeline_append_run]
                  cases (registry (mkTestPasses passesAt)).ssaPasses.run (ssaBlock (entryBlockId mem1.entry mem1.functions)) with
                  | error e => rfl
                  | ok ssa1 =>
                    dsimp only [Except.bind, Pipeline.run]
                    rw [hBB ssa1]; dsimp only
                    rw [pipeline_append_run]
                    cases (registry (mkTestPasses passesAt)).birPasses.run (birBlock (entryBlockId ssa1.entry ssa1.functions)) with
                    | error e => rfl
                    | ok bir1 =>
                      dsimp only [Except.bind, Pipeline.run]
                      cases (registry (mkTestPasses passesAt)).emit.run bir1 <;> rfl
            rw [hLowerChain (mirBlock 0)]
            cases route with
            | checked =>
              have hLexRun : (registry (mkTestPasses passesAt)).lexer.run src42 = .ok stream42 := rfl
              have hElabRun : (registry (mkTestPasses passesAt)).elaborator.run ast = .ok admIn := hElab
              have hAdmRun : (registry (mkTestPasses passesAt)).admission.toStage.run admIn =
                  .ok ({ core := core, discharges := [] } : Admitted s0Core) := by
                change (if admIn.slice = slice0 then do
                  let prepared ← admIn.prepare.mapError AdmissionFailure.refused
                  (Except.ok [] : Except AdmissionFailure (Discharges s0Core)).map fun d =>
                    ({ core := prepared, discharges := d } : Admitted s0Core)
                  else _) = _
                rw [ite_eq_left hAdmSlice, hPrep]; rfl
              have hLowRun : (registry (mkTestPasses passesAt)).lowerCore.run
                  ({ core := core, discharges := [] } : Admitted s0Core) = .ok (mirBlock 0) := rfl
              change (Pipeline.next (registry (mkTestPasses passesAt)).lexer
                (.next (registry (mkTestPasses passesAt)).parser
                  (.next (registry (mkTestPasses passesAt)).elaborator
                    (.next (registry (mkTestPasses passesAt)).admission.toStage
                      (.next (registry (mkTestPasses passesAt)).lowerCore _))))).run src42 = _
              rw [pipeline_next_ok hLexRun, pipeline_next_ok hParserRun,
                pipeline_next_ok hElabRun, pipeline_next_ok hAdmRun, pipeline_next_ok hLowRun]
              rfl
            | unchecked =>
              have hLexRun : (registry (mkTestPasses passesAt)).lexer.run src42 = .ok stream42 := rfl
              have hElabRun : (registry (mkTestPasses passesAt)).elaborator.run ast = .ok admIn := hElab
              have hEligRun : (registry (mkTestPasses passesAt)).eligibility.run admIn = .ok core := by
                change (if admIn.slice = slice0 then
                  (admIn.prepare .eligibility).mapError (fun c => StageFailure.admission (.refused c))
                  else _) = _
                rw [ite_eq_left hAdmSlice]
                have hPrepElig : admIn.prepare .eligibility = .ok core := by
                  have : admIn.prepare .eligibility = admIn.prepare .admission := by
                    cases admIn with
                    | pending proj surface shape =>
                      cases surface <;> rfl
                    | core c => rfl
                  rw [this]; exact hPrep
                rw [hPrepElig]; rfl
              have hTypRun : (registry (mkTestPasses passesAt)).typing.run core = .ok core := by
                change (if core.slice = slice0 then
                  (core.checkStatic.map (fun _ => core)).mapError StageFailure.admission else _) = _
                rw [ite_eq_left hCoreSlice, hStat]; rfl
              have hLowRun : (registry (mkTestPasses passesAt)).lowerUnchecked.run core = .ok (mirBlock 0) := rfl
              change (Pipeline.next (registry (mkTestPasses passesAt)).lexer
                (.next (registry (mkTestPasses passesAt)).parser
                  (.next (registry (mkTestPasses passesAt)).elaborator
                    (.next (registry (mkTestPasses passesAt)).eligibility
                      (.next (registry (mkTestPasses passesAt)).typing
                        (.next (registry (mkTestPasses passesAt)).lowerUnchecked _)))))).run src42 = _
              rw [pipeline_next_ok hLexRun, pipeline_next_ok hParserRun,
                pipeline_next_ok hElabRun, pipeline_next_ok hEligRun,
                pipeline_next_ok hTypRun, pipeline_next_ok hLowRun]
              rfl

/-- Control 2a: All 4 `passesAt` lists (`.mir`, `.memory`, `.ssa`, `.bir`) are folded through
`Registry.pipeline` on BOTH `.checked` and `.unchecked` routes:
- empty lists produce block `0`;
- each individual insertion point transforms the block ID (`1`, `10`, `100`, `1000`);
- all 4 active simultaneously compose to `1111` (`≠ 0`);
- two order-sensitive passes (`[+1, *2]` vs `[*2, +1]` after `+1` at `.mir`) produce distinct
  outputs `4` vs `3` (`4 ≠ 3`) on BOTH routes;
- declined (`none`) candidates fall back to identity (`0`) on BOTH routes and pass `foldPasses.Validated`. -/
theorem passesAt_all_four_folds_both_routes :
    let empty : (k : IRPoint) → List (IRPass s0Lang llvmProfile k) := fun _ => []
    let onlyMIR : (k : IRPoint) → List (IRPass s0Lang llvmProfile k)
      | .mir => [renameMIRPass (· + 1)] | _ => []
    let onlyMem : (k : IRPoint) → List (IRPass s0Lang llvmProfile k)
      | .memory => [renameMemoryPass (· + 10)] | _ => []
    let onlySSA : (k : IRPoint) → List (IRPass s0Lang llvmProfile k)
      | .ssa => [renameSSAPass (· + 100)] | _ => []
    let onlyBIR : (k : IRPoint) → List (IRPass s0Lang llvmProfile k)
      | .bir => [renameBIRPass (· + 1000)] | _ => []
    let allFour : (k : IRPoint) → List (IRPass s0Lang llvmProfile k)
      | .mir => [renameMIRPass (· + 1)]
      | .memory => [renameMemoryPass (· + 10)]
      | .ssa => [renameSSAPass (· + 100)]
      | .bir => [renameBIRPass (· + 1000)]
    let orderAddThenDouble : (k : IRPoint) → List (IRPass s0Lang llvmProfile k)
      | .mir => [renameMIRPass (· + 1)]
      | .bir => [renameBIRPass (· + 1), renameBIRPass (· * 2)]
      | _ => []
    let orderDoubleThenAdd : (k : IRPoint) → List (IRPass s0Lang llvmProfile k)
      | .mir => [renameMIRPass (· + 1)]
      | .bir => [renameBIRPass (· * 2), renameBIRPass (· + 1)]
      | _ => []
    let allDecline : (k : IRPoint) → List (IRPass s0Lang llvmProfile k) := fun _ => [fun _ => none, fun _ => none]
    -- Empty on both routes
    (Registry.pipeline (mkTestPasses empty) .checked).run src42 = .ok (llvmTagOut 0) ∧
    (Registry.pipeline (mkTestPasses empty) .unchecked).run src42 = .ok (llvmTagOut 0) ∧
    -- Each of the 4 points individually on both routes
    (Registry.pipeline (mkTestPasses onlyMIR) .checked).run src42 = .ok (llvmTagOut 1) ∧
    (Registry.pipeline (mkTestPasses onlyMIR) .unchecked).run src42 = .ok (llvmTagOut 1) ∧
    (Registry.pipeline (mkTestPasses onlyMem) .checked).run src42 = .ok (llvmTagOut 10) ∧
    (Registry.pipeline (mkTestPasses onlyMem) .unchecked).run src42 = .ok (llvmTagOut 10) ∧
    (Registry.pipeline (mkTestPasses onlySSA) .checked).run src42 = .ok (llvmTagOut 100) ∧
    (Registry.pipeline (mkTestPasses onlySSA) .unchecked).run src42 = .ok (llvmTagOut 100) ∧
    (Registry.pipeline (mkTestPasses onlyBIR) .checked).run src42 = .ok (llvmTagOut 1000) ∧
    (Registry.pipeline (mkTestPasses onlyBIR) .unchecked).run src42 = .ok (llvmTagOut 1000) ∧
    -- All 4 active simultaneously on both routes
    (Registry.pipeline (mkTestPasses allFour) .checked).run src42 = .ok (llvmTagOut 1111) ∧
    (Registry.pipeline (mkTestPasses allFour) .unchecked).run src42 = .ok (llvmTagOut 1111) ∧
    llvmTagOut 1111 ≠ llvmTagOut 0 ∧
    -- Order sensitivity on both routes: (1 + 1) * 2 = 4 vs (1 * 2) + 1 = 3
    (Registry.pipeline (mkTestPasses orderAddThenDouble) .checked).run src42 = .ok (llvmTagOut 4) ∧
    (Registry.pipeline (mkTestPasses orderAddThenDouble) .unchecked).run src42 = .ok (llvmTagOut 4) ∧
    (Registry.pipeline (mkTestPasses orderDoubleThenAdd) .checked).run src42 = .ok (llvmTagOut 3) ∧
    (Registry.pipeline (mkTestPasses orderDoubleThenAdd) .unchecked).run src42 = .ok (llvmTagOut 3) ∧
    llvmTagOut 4 ≠ llvmTagOut 3 ∧
    -- Identity fallback on declined candidates at all 4 points on both routes
    (Registry.pipeline (mkTestPasses allDecline) .checked).run src42 = .ok (llvmTagOut 0) ∧
    (Registry.pipeline (mkTestPasses allDecline) .unchecked).run src42 = .ok (llvmTagOut 0) ∧
    (∀ k : IRPoint, (foldPasses (language := s0Core) (allDecline k)).Validated) := by
  intro empty onlyMIR onlyMem onlySSA onlyBIR allFour orderAddThenDouble orderDoubleThenAdd allDecline
  have evalStep : ∀ (pa : (k : IRPoint) → List (IRPass s0Lang llvmProfile k)) (r : Route) (n : Nat),
      ((foldPasses (language := s0Core) (pa .mir)).run (mirBlock 0)).bind (fun mir1 =>
        (((foldPasses (language := s0Core) (pa .memory)).run
            (memBlock (entryBlockId mir1.entry mir1.functions))).bind fun mem1 =>
        (((foldPasses (language := s0Core) (pa .ssa)).run
            (ssaBlock (entryBlockId mem1.entry mem1.functions))).bind fun ssa1 =>
        (((foldPasses (language := s0Core) (pa .bir)).run
            (birBlock (entryBlockId ssa1.entry ssa1.functions))).bind fun bir1 =>
        ((registry (mkTestPasses pa)).emit.run bir1).mapError fun e => ⟨_, e⟩)))) = .ok (llvmTagOut n) →
      (Registry.pipeline (mkTestPasses pa) r).run src42 = .ok (llvmTagOut n) :=
    fun pa r _ h => (testPasses_run_eq_loweringChain pa r).trans h
  have hEmit (pa) (n : Nat) (h : firstUnsupported (language := s0Core) (birBlock n) = none) :=
    emit_birBlock pa n h
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_,
    llvmTagOut_ne (by decide), ?_, ?_, ?_, ?_, llvmTagOut_ne (by decide), ?_, ?_, ?_⟩
  · apply evalStep; simp [empty, foldPasses, Pipeline.run, Except.bind, Except.mapError,
      AtLang.entry, entryBlockId_mir, entryBlockId_mem, entryBlockId_ssa, hEmit _ 0 (by decide)]
  · apply evalStep; simp [empty, foldPasses, Pipeline.run, Except.bind, Except.mapError,
      AtLang.entry, entryBlockId_mir, entryBlockId_mem, entryBlockId_ssa, hEmit _ 0 (by decide)]
  · apply evalStep; simp [onlyMIR, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameMIRPass_mirBlock, AtLang.entry, entryBlockId_mir, entryBlockId_mem,
      entryBlockId_ssa, hEmit _ 1 (by decide)]
  · apply evalStep; simp [onlyMIR, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameMIRPass_mirBlock, AtLang.entry, entryBlockId_mir, entryBlockId_mem,
      entryBlockId_ssa, hEmit _ 1 (by decide)]
  · apply evalStep; simp [onlyMem, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameMemoryPass_memBlock, AtLang.entry, entryBlockId_mir, entryBlockId_mem,
      entryBlockId_ssa, hEmit _ 10 (by decide)]
  · apply evalStep; simp [onlyMem, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameMemoryPass_memBlock, AtLang.entry, entryBlockId_mir, entryBlockId_mem,
      entryBlockId_ssa, hEmit _ 10 (by decide)]
  · apply evalStep; simp [onlySSA, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameSSAPass_ssaBlock, AtLang.entry, entryBlockId_mir, entryBlockId_mem,
      entryBlockId_ssa, hEmit _ 100 (by decide)]
  · apply evalStep; simp [onlySSA, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameSSAPass_ssaBlock, AtLang.entry, entryBlockId_mir, entryBlockId_mem,
      entryBlockId_ssa, hEmit _ 100 (by decide)]
  · apply evalStep; simp [onlyBIR, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameBIRPass_birBlock, AtLang.entry, entryBlockId_mir, entryBlockId_mem,
      entryBlockId_ssa, hEmit _ 1000 (by decide)]
  · apply evalStep; simp [onlyBIR, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameBIRPass_birBlock, AtLang.entry, entryBlockId_mir, entryBlockId_mem,
      entryBlockId_ssa, hEmit _ 1000 (by decide)]
  · apply evalStep; simp [allFour, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameMIRPass_mirBlock, renameMemoryPass_memBlock,
      renameSSAPass_ssaBlock, renameBIRPass_birBlock, AtLang.entry, entryBlockId_mir, entryBlockId_mem,
      entryBlockId_ssa, hEmit _ 1111 (by decide)]
  · apply evalStep; simp [allFour, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameMIRPass_mirBlock, renameMemoryPass_memBlock,
      renameSSAPass_ssaBlock, renameBIRPass_birBlock, AtLang.entry, entryBlockId_mir, entryBlockId_mem,
      entryBlockId_ssa, hEmit _ 1111 (by decide)]
  · apply evalStep; simp [orderAddThenDouble, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameMIRPass_mirBlock, renameBIRPass_birBlock,
      AtLang.entry, entryBlockId_mir, entryBlockId_mem, entryBlockId_ssa, hEmit _ 4 (by decide)]
  · apply evalStep; simp [orderAddThenDouble, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameMIRPass_mirBlock, renameBIRPass_birBlock,
      AtLang.entry, entryBlockId_mir, entryBlockId_mem, entryBlockId_ssa, hEmit _ 4 (by decide)]
  · apply evalStep; simp [orderDoubleThenAdd, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameMIRPass_mirBlock, renameBIRPass_birBlock,
      AtLang.entry, entryBlockId_mir, entryBlockId_mem, entryBlockId_ssa, hEmit _ 3 (by decide)]
  · apply evalStep; simp [orderDoubleThenAdd, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, renameMIRPass_mirBlock, renameBIRPass_birBlock,
      AtLang.entry, entryBlockId_mir, entryBlockId_mem, entryBlockId_ssa, hEmit _ 3 (by decide)]
  · apply evalStep; simp [allDecline, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, AtLang.entry, entryBlockId_mir, entryBlockId_mem, entryBlockId_ssa, hEmit _ 0 (by decide)]
  · apply evalStep; simp [allDecline, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      Except.bind, Except.mapError, AtLang.entry, entryBlockId_mir, entryBlockId_mem, entryBlockId_ssa, hEmit _ 0 (by decide)]
  · intro k
    exact foldPasses_cons_validated IRPass.declined_validated
      (foldPasses_cons_validated IRPass.declined_validated foldPasses_nil_validated)

/-! ### Nonidentity block-renaming `PassPost`, `PassPost.compose`, and `IRPass.toStage.Validated` -/

private theorem memBlock_handoffWF (b : Nat) : (memBlock b).HandoffWF := by
  have hNodup1 : [ctrlMain].Nodup := List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩
  have hNodupB : ([⟨b⟩] : List BlockId).Nodup := List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩
  refine ⟨?_, List.mem_cons_self, ?_, (fun _ h => nomatch h),
    ⟨List.nodup_nil, ⟨List.nodup_nil, (fun _ h => nomatch h)⟩, (fun _ _ h => nomatch h)⟩,
    ⟨List.nodup_nil, (fun _ _ h => nomatch h)⟩⟩
  · refine ⟨hNodup1, hNodup1, hNodup1, List.nodup_nil,
      (fun _ _ h => by obtain ⟨rfl, rfl⟩ := mem_singleton_pair h; exact List.nodup_nil),
      (fun _ _ h => nomatch h), ?_⟩
    intro fn ls gr h
    obtain ⟨rfl, rfl, rfl⟩ := mem_singleton_triple h
    refine ⟨List.nodup_nil, ⟨hNodupB, ⟨_, List.mem_cons_self, rfl⟩,
      fun blk hblk _ hedge => by rcases List.mem_singleton.mp hblk with rfl; cases hedge⟩,
      ⟨⟨[], some u8Ty⟩, rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩,
        fun blk hblk => by rcases List.mem_singleton.mp hblk with rfl; exact ⟨rfl, fun _ h => nomatch h⟩⟩⟩
  · intro orig horig
    have horig' : orig ∈ ([orig0] : List Origin) := by with_unfolding_all exact horig
    rcases List.mem_singleton.mp horig' with rfl
    exact span0_binds

private theorem mirBlock_wf (b : Nat) : (mirBlock b).HandoffWF (language := s0Core) := by
  have hView : (mirBlock b).logicalView (language := s0Core) = memBlock b := by
    with_unfolding_all rfl
  refine ⟨hView ▸ memBlock_handoffWF b, ?_⟩
  intro fn ls gr hfn place ty hloc
  obtain ⟨rfl, rfl, rfl⟩ := mem_singleton_triple hfn
  cases hloc

private theorem ssaBlock_wf (b : Nat)
    (hReg : (ssaBlock b).RegisterFormation (language := s0Core)) :
    (ssaBlock b).WF (language := s0Core) := by
  have hView : (ssaBlock b).memoryView (language := s0Core) = memBlock b := by
    with_unfolding_all rfl
  exact ⟨hReg, hView ▸ memBlock_handoffWF b⟩

private theorem birBlock_wf (b : Nat)
    (hReg : (birBlock b).RegisterFormation (language := s0Core)) :
    (birBlock b).WF (language := s0Core) := by
  have hView : (birBlock b).memoryView (language := s0Core) = memBlock b := by
    with_unfolding_all rfl
  have hSlots : (birBlock b).frameSlots ≤ llvmProfile.maxBIRSlots := by
    have h0 : (birBlock b).frameSlots = 0 := by with_unfolding_all rfl
    rw [h0]; decide
  refine ⟨hReg, ?_, hSlots, ?_, (fun _ h => nomatch h), hView ▸ memBlock_handoffWF b⟩
  · intro fn hfn blk hblk slot htrap
    rcases List.mem_singleton.mp hfn with rfl
    rcases List.mem_singleton.mp hblk with rfl
    cases htrap
  · intro orig horig
    have horig' : orig ∈ ([orig0] : List Origin) := by with_unfolding_all exact horig
    rcases List.mem_singleton.mp horig' with rfl
    exact span0_binds

private theorem memBlock_terminalObservation (b : Nat) :
    (memBlock b).terminalObservation (mkRegState llvmProfile ctrlMain b 0 [] [] []) =
      some (.returned 42 ByteArray.empty) := by
  have hBlk : (memBlock b).blockAt (mkRegState llvmProfile ctrlMain b 0 [] [] []) =
      some ⟨⟨b⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩ := by
    simp [Memory.blockAt, Memory.graphAt, memBlock, mkRegState, AList.lookup, Array.find?]
  simp only [Memory.terminalObservation]
  rw [hBlk]
  with_unfolding_all rfl

private theorem memBlock_hostedResult (b : Nat) :
    (Memory.Execution (language := s0Core)).result (memBlock b)
      (mkRegState llvmProfile ctrlMain b 0 [] [] []) (retObs 42) := by
  have hTerm := memBlock_terminalObservation b
  with_unfolding_all
    change Option.bind ((memBlock b).terminalObservation (mkRegState llvmProfile ctrlMain b 0 [] [] [])) _ =
      some (retObs 42)
  rw [hTerm]; rfl

private theorem memBlock_entryState (b : Nat) :
    ∀ s, (Memory.Execution (language := s0Core)).initial (memBlock b) s ↔
      s = mkRegState llvmProfile ctrlMain b 0 [] [] [] := by
  intro s
  have hFind : (Array.find? (fun blk : MemoryBlock s0Lang => blk.id == ⟨b⟩)
      #[⟨⟨b⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩]) =
      some ⟨⟨b⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩ := by
    simp [Array.find?]
  with_unfolding_all
    change (s = ⟨⟨ctrlMain, ⟨b⟩,
      ((Array.find? (fun blk : MemoryBlock s0Lang => blk.id == ⟨b⟩)
          #[⟨⟨b⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩]).map
        fun blk => if blk.params.length = 0 then 0 else blk.body.length + 1).getD 0,
      [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩) ↔ _
  rw [hFind]; rfl

private theorem memBlock_initial (b : Nat) :
    (Memory.Execution (language := s0Core)).initial (memBlock b)
      (mkRegState llvmProfile ctrlMain b 0 [] [] []) :=
  (memBlock_entryState b _).mpr rfl

private theorem memBlock_trace (b : Nat) :
    (Memory.Execution (language := s0Core)).trace (memBlock b)
      (mkRegState llvmProfile ctrlMain b 0 [] [] []) = [] := by
  have hTerm := memBlock_terminalObservation b
  with_unfolding_all
    change [] ++ ((Option.bind ((memBlock b).terminalObservation (mkRegState llvmProfile ctrlMain b 0 [] [] [])) _).map _).getD [] = []
  rw [hTerm]; rfl

private theorem memBlock_refines (b1 b2 : Nat) :
    BehaviorRefinement (Memory.Execution (language := s0Core)) (Memory.Execution (language := s0Core))
      (Memory.Execution (language := s0Core)).result (Memory.Execution (language := s0Core)).result
      (memBlock b1) (memBlock b2) := by
  let p0 := mkRegState llvmProfile ctrlMain b1 0 [] [] []
  let q0 := mkRegState llvmProfile ctrlMain b2 0 [] [] []
  have hpRes := memBlock_hostedResult b1
  have hqRes := memBlock_hostedResult b2
  exact (DetChain.nil q0).refines ⟨p0, memBlock_initial b1, .nil p0⟩ hpRes
    (fun _ h => (memBlock_entryState b2 _).mp h)
    (fun o => ⟨fun h => (Option.some.inj (hqRes.symm.trans h)).symm, fun h => h ▸ hqRes⟩)
    ((memBlock_trace b1).trans (memBlock_trace b2).symm)

private theorem passPost_renameBlock_all_four (b1 b2 : Nat)
    (hRegSSA : (ssaBlock b2).RegisterFormation (language := s0Core))
    (hRegBIR : (birBlock b2).RegisterFormation (language := s0Core)) :
    PassPost (slice := slice0) (profile := llvmProfile) (k := .mir)
      (mirBlock b1) (mirBlock b2) (renameBoundaryAcc b1 b2) ∧
    PassPost (slice := slice0) (profile := llvmProfile) (k := .memory)
      (memBlock b1) (memBlock b2) (renameBoundaryAcc b1 b2) ∧
    PassPost (slice := slice0) (profile := llvmProfile) (k := .ssa)
      (ssaBlock b1) (ssaBlock b2) (renameBoundaryAcc b1 b2) ∧
    PassPost (slice := slice0) (profile := llvmProfile) (k := .bir)
      (birBlock b1) (birBlock b2) (renameBoundaryAcc b1 b2) := by
  have hAccMIR : AccountsFor (slice := slice0) (profile := llvmProfile) (point := .mir)
      (mirBlock b1) (mirBlock b2) (renameBoundaryAcc b1 b2) := by
    refine ⟨⟨?_, ?_, ?_, (fun _ h => nomatch h), ?_⟩, ?_, ?_, (fun _ _ h => nomatch h)⟩
    · intro out hout; rcases List.mem_singleton.mp hout with rfl; exact List.mem_cons_self
    · intro out hout; rcases List.mem_singleton.mp hout with rfl
      exact ⟨⟨ctrlMain, ⟨b1⟩, .boundary 0⟩, by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup]⟩
    · intro out src hmap
      obtain ⟨srcs, hmem, hsrc⟩ := ((renameBoundaryAcc b1 b2).mapsTo_iff_mem_mapped out src).mp hmap
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      rcases List.mem_singleton.mp hsrc with rfl; exact List.mem_cons_self
    · intro src hsrc; rcases List.mem_singleton.mp hsrc with rfl
      exact Or.inl ⟨⟨ctrlMain, ⟨b2⟩, .boundary 0⟩, by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup]⟩
    · intro out hout; rcases List.mem_singleton.mp hout with rfl
      exact ⟨⟨ctrlMain, ⟨b1⟩, .boundary 0⟩,
        by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup],
        by simp [AtLang.originOf, AtLang.occurrences, MIR.occurrences, CFG.occurrences, Block.occurrences,
          mirBlock, AList.lookup]⟩
    · intro out src hmap
      obtain ⟨srcs, hmem, hsrc⟩ := ((renameBoundaryAcc b1 b2).mapsTo_iff_mem_mapped out src).mp hmap
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      rcases List.mem_singleton.mp hsrc with rfl
      exact ⟨by simp, fun h => nomatch h⟩
  have hAccMem : AccountsFor (slice := slice0) (profile := llvmProfile) (point := .memory)
      (memBlock b1) (memBlock b2) (renameBoundaryAcc b1 b2) := by
    refine ⟨⟨?_, ?_, ?_, (fun _ h => nomatch h), ?_⟩, ?_, ?_, (fun _ _ h => nomatch h)⟩
    · intro out hout; rcases List.mem_singleton.mp hout with rfl; exact List.mem_cons_self
    · intro out hout; rcases List.mem_singleton.mp hout with rfl
      exact ⟨⟨ctrlMain, ⟨b1⟩, .boundary 0⟩, by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup]⟩
    · intro out src hmap
      obtain ⟨srcs, hmem, hsrc⟩ := ((renameBoundaryAcc b1 b2).mapsTo_iff_mem_mapped out src).mp hmap
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      rcases List.mem_singleton.mp hsrc with rfl; exact List.mem_cons_self
    · intro src hsrc; rcases List.mem_singleton.mp hsrc with rfl
      exact Or.inl ⟨⟨ctrlMain, ⟨b2⟩, .boundary 0⟩, by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup]⟩
    · intro out hout; rcases List.mem_singleton.mp hout with rfl
      exact ⟨⟨ctrlMain, ⟨b1⟩, .boundary 0⟩,
        by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup],
        by simp [AtLang.originOf, AtLang.occurrences, Memory.occurrences, CFG.occurrences, Block.occurrences,
          memBlock, AList.lookup]⟩
    · intro out src hmap
      obtain ⟨srcs, hmem, hsrc⟩ := ((renameBoundaryAcc b1 b2).mapsTo_iff_mem_mapped out src).mp hmap
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      rcases List.mem_singleton.mp hsrc with rfl
      exact ⟨by simp, fun h => nomatch h⟩
  have hAccSSA : AccountsFor (slice := slice0) (profile := llvmProfile) (point := .ssa)
      (ssaBlock b1) (ssaBlock b2) (renameBoundaryAcc b1 b2) := by
    refine ⟨⟨?_, ?_, ?_, (fun _ h => nomatch h), ?_⟩, ?_, ?_, (fun _ _ h => nomatch h)⟩
    · intro out hout; rcases List.mem_singleton.mp hout with rfl; exact List.mem_cons_self
    · intro out hout; rcases List.mem_singleton.mp hout with rfl
      exact ⟨⟨ctrlMain, ⟨b1⟩, .boundary 0⟩, by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup]⟩
    · intro out src hmap
      obtain ⟨srcs, hmem, hsrc⟩ := ((renameBoundaryAcc b1 b2).mapsTo_iff_mem_mapped out src).mp hmap
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      rcases List.mem_singleton.mp hsrc with rfl; exact List.mem_cons_self
    · intro src hsrc; rcases List.mem_singleton.mp hsrc with rfl
      exact Or.inl ⟨⟨ctrlMain, ⟨b2⟩, .boundary 0⟩, by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup]⟩
    · intro out hout; rcases List.mem_singleton.mp hout with rfl
      exact ⟨⟨ctrlMain, ⟨b1⟩, .boundary 0⟩,
        by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup],
        by simp [AtLang.originOf, AtLang.occurrences, SSA.occurrences, CFG.occurrences, Block.occurrences,
          ssaBlock, AList.lookup]⟩
    · intro out src hmap
      obtain ⟨srcs, hmem, hsrc⟩ := ((renameBoundaryAcc b1 b2).mapsTo_iff_mem_mapped out src).mp hmap
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      rcases List.mem_singleton.mp hsrc with rfl
      exact ⟨by simp, fun h => nomatch h⟩
  have hAccBIR : AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      (birBlock b1) (birBlock b2) (renameBoundaryAcc b1 b2) := by
    refine ⟨⟨?_, ?_, ?_, (fun _ h => nomatch h), ?_⟩, ?_, ?_, (fun _ _ h => nomatch h)⟩
    · intro out hout; rcases List.mem_singleton.mp hout with rfl; exact List.mem_cons_self
    · intro out hout; rcases List.mem_singleton.mp hout with rfl
      exact ⟨⟨ctrlMain, ⟨b1⟩, .boundary 0⟩, by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup]⟩
    · intro out src hmap
      obtain ⟨srcs, hmem, hsrc⟩ := ((renameBoundaryAcc b1 b2).mapsTo_iff_mem_mapped out src).mp hmap
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      rcases List.mem_singleton.mp hsrc with rfl; exact List.mem_cons_self
    · intro src hsrc; rcases List.mem_singleton.mp hsrc with rfl
      exact Or.inl ⟨⟨ctrlMain, ⟨b2⟩, .boundary 0⟩, by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup]⟩
    · intro out hout; rcases List.mem_singleton.mp hout with rfl
      exact ⟨⟨ctrlMain, ⟨b1⟩, .boundary 0⟩,
        by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup],
        by simp [AtLang.originOf, AtLang.occurrences, BIR.occurrences, CFG.occurrences, Block.occurrences,
          birBlock, AList.lookup]⟩
    · intro out src hmap
      obtain ⟨srcs, hmem, hsrc⟩ := ((renameBoundaryAcc b1 b2).mapsTo_iff_mem_mapped out src).mp hmap
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      rcases List.mem_singleton.mp hsrc with rfl
      exact ⟨by simp, fun h => nomatch h⟩
  have hMirView (b : Nat) : (mirBlock b).logicalView (language := s0Core) = memBlock b := by with_unfolding_all rfl
  have hSsaView (b : Nat) : (ssaBlock b).memoryView (language := s0Core) = memBlock b := by with_unfolding_all rfl
  have hBirView (b : Nat) : (birBlock b).memoryView (language := s0Core) = memBlock b := by with_unfolding_all rfl
  have hRefMIR : BehaviorRefinement (MIR.Execution (language := s0Core)) (MIR.Execution (language := s0Core))
      (MIR.Execution (language := s0Core)).result (MIR.Execution (language := s0Core)).result
      (mirBlock b1) (mirBlock b2) := by
    let p0 := mkRegState llvmProfile ctrlMain b1 0 [] [] []
    let q0 := mkRegState llvmProfile ctrlMain b2 0 [] [] []
    have hpRes : (MIR.Execution (language := s0Core)).result (mirBlock b1) p0 (retObs 42) := by
      show (Memory.Execution (language := s0Core)).result ((mirBlock b1).logicalView (language := s0Core)) p0 (retObs 42)
      rw [hMirView]; exact memBlock_hostedResult b1
    have hqRes : (MIR.Execution (language := s0Core)).result (mirBlock b2) q0 (retObs 42) := by
      show (Memory.Execution (language := s0Core)).result ((mirBlock b2).logicalView (language := s0Core)) q0 (retObs 42)
      rw [hMirView]; exact memBlock_hostedResult b2
    have htr : (MIR.Execution (language := s0Core)).trace (mirBlock b1) p0 =
        (MIR.Execution (language := s0Core)).trace (mirBlock b2) q0 := by
      show (Memory.Execution (language := s0Core)).trace ((mirBlock b1).logicalView (language := s0Core)) p0 =
        (Memory.Execution (language := s0Core)).trace ((mirBlock b2).logicalView (language := s0Core)) q0
      rw [hMirView, hMirView]
      exact (memBlock_trace b1).trans (memBlock_trace b2).symm
    have hpInit : (MIR.Execution (language := s0Core)).initial (mirBlock b1) p0 := by
      show (Memory.Execution (language := s0Core)).initial ((mirBlock b1).logicalView (language := s0Core)) p0
      rw [hMirView]; exact memBlock_initial b1
    have hqInit : ∀ s, (MIR.Execution (language := s0Core)).initial (mirBlock b2) s → s = q0 := by
      intro s hs
      have hs' : (Memory.Execution (language := s0Core)).initial ((mirBlock b2).logicalView (language := s0Core)) s := hs
      rw [hMirView] at hs'; exact (memBlock_entryState b2 s).mp hs'
    exact (DetChain.nil q0).refines ⟨p0, hpInit, .nil p0⟩ hpRes hqInit
      (fun o => ⟨fun h => (Option.some.inj (hqRes.symm.trans h)).symm, fun h => h ▸ hqRes⟩)
      htr
  have hRefSSA : BehaviorRefinement (SSA.Execution (language := s0Core)) (SSA.Execution (language := s0Core))
      (SSA.Execution (language := s0Core)).result (SSA.Execution (language := s0Core)).result
      (ssaBlock b1) (ssaBlock b2) := by
    let p0 := mkRegState llvmProfile ctrlMain b1 0 [] [] []
    let q0 := mkRegState llvmProfile ctrlMain b2 0 [] [] []
    have hpRes : (SSA.Execution (language := s0Core)).result (ssaBlock b1) p0 (retObs 42) := by
      unfold SSA.Execution Semantics.onInput; dsimp only; rw [hSsaView]; exact memBlock_hostedResult b1
    have hqRes : (SSA.Execution (language := s0Core)).result (ssaBlock b2) q0 (retObs 42) := by
      unfold SSA.Execution Semantics.onInput; dsimp only; rw [hSsaView]; exact memBlock_hostedResult b2
    have htr : (SSA.Execution (language := s0Core)).trace (ssaBlock b1) p0 =
        (SSA.Execution (language := s0Core)).trace (ssaBlock b2) q0 := by
      unfold SSA.Execution Semantics.onInput; dsimp only; rw [hSsaView, hSsaView]
      exact (memBlock_trace b1).trans (memBlock_trace b2).symm
    have hpInit : (SSA.Execution (language := s0Core)).initial (ssaBlock b1) p0 := by
      unfold SSA.Execution Semantics.onInput; dsimp only; rw [hSsaView]; exact memBlock_initial b1
    have hqInit : ∀ s, (SSA.Execution (language := s0Core)).initial (ssaBlock b2) s → s = q0 := by
      intro s hs; unfold SSA.Execution Semantics.onInput at hs; dsimp only at hs
      rw [hSsaView] at hs; exact (memBlock_entryState b2 s).mp hs
    exact (DetChain.nil q0).refines ⟨p0, hpInit, .nil p0⟩ hpRes hqInit
      (fun o => ⟨fun h => (Option.some.inj (hqRes.symm.trans h)).symm, fun h => h ▸ hqRes⟩)
      htr
  have hRefBIR : BehaviorRefinement (BIR.Execution (language := s0Core)) (BIR.Execution (language := s0Core))
      (BIR.Execution (language := s0Core)).result (BIR.Execution (language := s0Core)).result
      (birBlock b1) (birBlock b2) := by
    let p0 := mkRegState llvmProfile ctrlMain b1 0 [] [] []
    let q0 := mkRegState llvmProfile ctrlMain b2 0 [] [] []
    have hpRes : (BIR.Execution (language := s0Core)).result (birBlock b1) p0 (retObs 42) := by
      unfold BIR.Execution Semantics.onInput; dsimp only; rw [hBirView]; exact memBlock_hostedResult b1
    have hqRes : (BIR.Execution (language := s0Core)).result (birBlock b2) q0 (retObs 42) := by
      unfold BIR.Execution Semantics.onInput; dsimp only; rw [hBirView]; exact memBlock_hostedResult b2
    have htr : (BIR.Execution (language := s0Core)).trace (birBlock b1) p0 =
        (BIR.Execution (language := s0Core)).trace (birBlock b2) q0 := by
      unfold BIR.Execution Semantics.onInput; dsimp only; rw [hBirView, hBirView]
      exact (memBlock_trace b1).trans (memBlock_trace b2).symm
    have hpInit : (BIR.Execution (language := s0Core)).initial (birBlock b1) p0 := by
      unfold BIR.Execution Semantics.onInput; dsimp only; rw [hBirView]; exact memBlock_initial b1
    have hqInit : ∀ s, (BIR.Execution (language := s0Core)).initial (birBlock b2) s → s = q0 := by
      intro s hs; unfold BIR.Execution Semantics.onInput at hs; dsimp only at hs
      rw [hBirView] at hs; exact (memBlock_entryState b2 s).mp hs
    exact (DetChain.nil q0).refines ⟨p0, hpInit, .nil p0⟩ hpRes hqInit
      (fun o => ⟨fun h => (Option.some.inj (hqRes.symm.trans h)).symm, fun h => h ▸ hqRes⟩)
      htr
  exact
    ⟨⟨mirBlock_wf b2, rfl, rfl, rfl, (fun _ _ h => nomatch h), hAccMIR, hRefMIR⟩,
     ⟨memBlock_handoffWF b2, rfl, rfl, rfl, (fun _ _ h => nomatch h), hAccMem, memBlock_refines b1 b2⟩,
     ⟨ssaBlock_wf b2 hRegSSA, rfl, rfl, rfl, (fun _ _ h => nomatch h), hAccSSA, hRefSSA⟩,
     ⟨birBlock_wf b2 hRegBIR, rfl, rfl, rfl, (fun _ _ h => nomatch h), hAccBIR, hRefBIR⟩⟩

private def eqEarlyTy (a b : s0Core.operands.Ty) : Bool :=
  let a' : EarlyTy := a
  let b' : EarlyTy := b
  decide (a' = b')

private theorem eqEarlyTy_eq {a b : s0Core.operands.Ty} (h : eqEarlyTy a b = true) : a = b := by
  let a' : EarlyTy := a
  let b' : EarlyTy := b
  have h' : a' = b' := of_decide_eq_true h
  exact h'

private def isMainU8Sig : List (FnId × FunctionSignature s0Core.operands.Ty) → Bool
  | p :: [] =>
    match p.2.parameters, p.2.result with
    | [], some ty => decide (p.1 = ctrlMain) && eqEarlyTy ty u8Ty
    | _, _ => false
  | _ => false

private theorem isMainU8Sig_eq {sigs : List (FnId × FunctionSignature s0Core.operands.Ty)}
    (h : isMainU8Sig sigs = true) : sigs = [(ctrlMain, ⟨[], some u8Ty⟩)] := by
  cases sigs with
  | nil => cases h
  | cons p rest =>
    cases rest with
    | cons _ _ => cases h
    | nil =>
      rcases p with ⟨fn, ⟨ps, r⟩⟩
      cases ps with
      | cons _ _ => cases h
      | nil =>
        cases r with
        | none => cases h
        | some ty =>
          simp only [isMainU8Sig, Bool.and_eq_true, decide_eq_true_eq] at h
          obtain ⟨rfl, hty⟩ := h
          rw [eqEarlyTy_eq hty]

private def isRet42Term :
    BIRTerm (Sigma (SSARead s0Core.operands.Ty s0Lang.literal)) (SSARead s0Core.operands.Ty s0Lang.literal boolTy) → Bool
  | .ret (some ⟨ty, .literal ⟨[b42]⟩⟩) => eqEarlyTy ty u8Ty && decide (b42 = 42)
  | _ => false

private theorem isRet42Term_eq
    {t : BIRTerm (Sigma (SSARead s0Core.operands.Ty s0Lang.literal)) (SSARead s0Core.operands.Ty s0Lang.literal boolTy)}
    (h : isRet42Term t = true) : t = .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩) := by
  cases t with
  | ret rv =>
    cases rv with
    | none => cases h
    | some op =>
      rcases op with ⟨ty, rd⟩
      cases rd with
      | value _ => cases h
      | literal enc =>
        cases enc with
        | mk bs =>
          cases bs with
          | nil => cases h
          | cons b rest =>
            cases rest with
            | cons _ _ => cases h
            | nil =>
              simp only [isRet42Term, Bool.and_eq_true, decide_eq_true_eq] at h
              obtain ⟨hty, rfl⟩ := h
              rw [eqEarlyTy_eq hty]
  | jump _ => cases h
  | branch _ _ _ => cases h
  | trap _ => cases h

private def isRet42Block (srcB : Nat) (blk : Block (Sigma (SSAValue s0Core.operands.Ty))
    (LocatedOp (SSAInst s0Lang))
    (BIRTerm (Sigma (SSARead s0Core.operands.Ty s0Lang.literal)) (SSARead s0Core.operands.Ty s0Lang.literal boolTy))) : Bool :=
  match blk.params, blk.body, blk.boundaryOrigins with
  | [], [], o :: [] => decide (blk.id.id = srcB ∧ o = orig0) && isRet42Term blk.ending
  | _, _, _ => false

private theorem isRet42Block_eq {srcB : Nat} {blk : Block (Sigma (SSAValue s0Core.operands.Ty))
    (LocatedOp (SSAInst s0Lang))
    (BIRTerm (Sigma (SSARead s0Core.operands.Ty s0Lang.literal)) (SSARead s0Core.operands.Ty s0Lang.literal boolTy))}
    (h : isRet42Block srcB blk = true) :
    blk = ⟨⟨srcB⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩ := by
  cases blk with
  | mk bid bps bbd bending borigs =>
    cases bid with
    | mk b' =>
      cases bps with
      | cons _ _ => cases h
      | nil =>
        cases bbd with
        | cons _ _ => cases h
        | nil =>
          cases borigs with
          | nil => cases h
          | cons o0 restO =>
            cases restO with
            | cons _ _ => cases h
            | nil =>
              simp only [isRet42Block, Bool.and_eq_true, decide_eq_true_eq] at h
              obtain ⟨⟨rfl, rfl⟩, hEnd⟩ := h
              rw [isRet42Term_eq hEnd]

private def isMainRetFns (srcB : Nat) : AList FnId (LocalTypes s0Core.operands.Ty × BIRGraph s0Lang) → Bool
  | f :: [] =>
    match f.2.1, f.2.2.blocks.toList with
    | [], blk :: [] => decide (f.1 = ctrlMain ∧ f.2.2.entry.id = srcB) && isRet42Block srcB blk
    | _, _ => false
  | _ => false

private theorem isMainRetFns_eq {srcB : Nat}
    {fns : AList FnId (LocalTypes s0Core.operands.Ty × BIRGraph s0Lang)}
    (h : isMainRetFns srcB fns = true) :
    fns = [(ctrlMain, [], ⟨⟨srcB⟩, #[⟨⟨srcB⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩]⟩)] := by
  cases fns with
  | nil => cases h
  | cons f rest =>
    cases rest with
    | cons _ _ => cases h
    | nil =>
      rcases f with ⟨fn, ls, g⟩
      cases ls with
      | cons _ _ => cases h
      | nil =>
        cases g with
        | mk ent blks =>
          cases ent with
          | mk b =>
            cases blks with
            | mk blkList =>
              cases blkList with
              | nil => cases h
              | cons blk restBlks =>
                cases restBlks with
                | cons _ _ => cases h
                | nil =>
                  simp only [isMainRetFns, Bool.and_eq_true, decide_eq_true_eq] at h
                  obtain ⟨⟨rfl, rfl⟩, hBlk⟩ := h
                  rw [isRet42Block_eq hBlk]

private def isBirBlock (srcB : Nat) (bir : BIR s0Lang llvmProfile) : Bool :=
  decide (bir.entry = ctrlMain) &&
  bir.layouts.isEmpty && bir.sites.isEmpty && bir.mutableLocals.isEmpty && bir.declarations.isEmpty &&
  isMainU8Sig bir.signatures &&
  isMainRetFns srcB bir.functions

private theorem isBirBlock_eq {srcB : Nat} {bir : BIR s0Lang llvmProfile}
    (h : isBirBlock srcB bir = true) : bir = { birBlock srcB with project := bir.project } := by
  cases bir with
  | mk proj entry sigs fns layouts sites muts decls =>
    unfold isBirBlock at h
    simp only [Bool.and_eq_true, decide_eq_true_eq] at h
    obtain ⟨⟨⟨⟨⟨⟨rfl, hLay⟩, hSit⟩, hMut⟩, hDec⟩, hSig⟩, hFns⟩ := h
    cases layouts with | cons _ _ => cases hLay | nil =>
    cases sites with | mk s => cases s with | cons _ _ => cases hSit | nil =>
    cases muts with | cons _ _ => cases hMut | nil =>
    cases decls with | cons _ _ => cases hDec | nil =>
    obtain rfl := isMainU8Sig_eq hSig
    obtain rfl := isMainRetFns_eq hFns
    rfl

private theorem birBlockP_passPost (proj : Project) (b1 b2 : Nat)
    (hSpan : ∃ sp, orig0.lexical.Binds proj sp)
    (hRegBIR : (birBlock b2).RegisterFormation (language := s0Core)) :
    PassPost (slice := slice0) (profile := llvmProfile) (k := .bir)
      { birBlock b1 with project := proj } { birBlock b2 with project := proj }
      (renameBoundaryAcc b1 b2) := by
  let inB : BIR s0Lang llvmProfile := { birBlock b1 with project := proj }
  let outB : BIR s0Lang llvmProfile := { birBlock b2 with project := proj }
  let inM : Memory s0Lang llvmProfile := { memBlock b1 with project := proj }
  let outM : Memory s0Lang llvmProfile := { memBlock b2 with project := proj }
  have hInView : inB.memoryView (language := s0Core) = inM := by with_unfolding_all rfl
  have hOutView : outB.memoryView (language := s0Core) = outM := by with_unfolding_all rfl
  have hOutMemWF : outM.HandoffWF := by
    have hNodup1 : [ctrlMain].Nodup := List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩
    have hNodupB : ([⟨b2⟩] : List BlockId).Nodup := List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩
    refine ⟨?_, List.mem_cons_self, ?_, (fun _ h => nomatch h),
      ⟨List.nodup_nil, ⟨List.nodup_nil, (fun _ h => nomatch h)⟩, (fun _ _ h => nomatch h)⟩,
      ⟨List.nodup_nil, (fun _ _ h => nomatch h)⟩⟩
    · refine ⟨hNodup1, hNodup1, hNodup1, List.nodup_nil,
        (fun _ _ h => by obtain ⟨rfl, rfl⟩ := mem_singleton_pair h; exact List.nodup_nil),
        (fun _ _ h => nomatch h), ?_⟩
      intro fn ls gr h
      obtain ⟨rfl, rfl, rfl⟩ := mem_singleton_triple h
      refine ⟨List.nodup_nil, ⟨hNodupB, ⟨_, List.mem_cons_self, rfl⟩,
        fun blk hblk _ hedge => by rcases List.mem_singleton.mp hblk with rfl; cases hedge⟩,
        ⟨⟨[], some u8Ty⟩, rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩,
          fun blk hblk => by rcases List.mem_singleton.mp hblk with rfl; exact ⟨rfl, fun _ h => nomatch h⟩⟩⟩
    · intro orig horig
      have horig' : orig ∈ ([orig0] : List Origin) := by with_unfolding_all exact horig
      rcases List.mem_singleton.mp horig' with rfl
      exact hSpan
  have hOutWF : outB.WF (language := s0Core) := by
    have hSlots : outB.frameSlots ≤ llvmProfile.maxBIRSlots := by
      have h0 : outB.frameSlots = 0 := by with_unfolding_all rfl
      rw [h0]; decide
    refine ⟨hRegBIR, ?_, hSlots, ?_, (fun _ h => nomatch h), hOutView ▸ hOutMemWF⟩
    · intro fn hfn blk hblk slot htrap
      rcases List.mem_singleton.mp hfn with rfl
      rcases List.mem_singleton.mp hblk with rfl
      cases htrap
    · intro orig horig
      have horig' : orig ∈ ([orig0] : List Origin) := by with_unfolding_all exact horig
      rcases List.mem_singleton.mp horig' with rfl
      exact hSpan
  have hAcc : AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      inB outB (renameBoundaryAcc b1 b2) := by
    refine ⟨⟨?_, ?_, ?_, (fun _ h => nomatch h), ?_⟩, ?_, ?_, (fun _ _ h => nomatch h)⟩
    · intro out hout; rcases List.mem_singleton.mp hout with rfl; exact List.mem_cons_self
    · intro out hout; rcases List.mem_singleton.mp hout with rfl
      exact ⟨⟨ctrlMain, ⟨b1⟩, .boundary 0⟩, by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup]⟩
    · intro out src hmap
      obtain ⟨srcs, hmem, hsrc⟩ := ((renameBoundaryAcc b1 b2).mapsTo_iff_mem_mapped out src).mp hmap
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      rcases List.mem_singleton.mp hsrc with rfl; exact List.mem_cons_self
    · intro src hsrc; rcases List.mem_singleton.mp hsrc with rfl
      exact Or.inl ⟨⟨ctrlMain, ⟨b2⟩, .boundary 0⟩, by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup]⟩
    · intro out hout; rcases List.mem_singleton.mp hout with rfl
      exact ⟨⟨ctrlMain, ⟨b1⟩, .boundary 0⟩,
        by simp [Account.MapsTo, Account.sourcesOf, renameBoundaryAcc, AList.lookup],
        by simp [inB, outB, AtLang.originOf, AtLang.occurrences, BIR.occurrences, CFG.occurrences, Block.occurrences,
          birBlock, AList.lookup]⟩
    · intro out src hmap
      obtain ⟨srcs, hmem, hsrc⟩ := ((renameBoundaryAcc b1 b2).mapsTo_iff_mem_mapped out src).mp hmap
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      rcases List.mem_singleton.mp hsrc with rfl
      exact ⟨by simp, fun h => nomatch h⟩
  have hTerm (b : Nat) :
      ({ memBlock b with project := proj } : Memory s0Lang llvmProfile).terminalObservation
        (mkRegState llvmProfile ctrlMain b 0 [] [] []) = some (.returned 42 ByteArray.empty) := by
    have hBlk : ({ memBlock b with project := proj } : Memory s0Lang llvmProfile).blockAt
        (mkRegState llvmProfile ctrlMain b 0 [] [] []) =
        some ⟨⟨b⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩ := by
      simp [Memory.blockAt, Memory.graphAt, memBlock, mkRegState, AList.lookup, Array.find?]
    simp only [Memory.terminalObservation]
    rw [hBlk]; with_unfolding_all rfl
  have hEntry (b : Nat) : ∀ s, (Memory.Execution (language := s0Core)).initial
      ({ memBlock b with project := proj } : Memory s0Lang llvmProfile) s ↔
      s = mkRegState llvmProfile ctrlMain b 0 [] [] [] := by
    intro s
    have hFind : (Array.find? (fun blk : MemoryBlock s0Lang => blk.id == ⟨b⟩)
        #[⟨⟨b⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩]) =
        some ⟨⟨b⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩ := by
      simp [Array.find?]
    with_unfolding_all
      change (s = ⟨⟨ctrlMain, ⟨b⟩,
        ((Array.find? (fun blk : MemoryBlock s0Lang => blk.id == ⟨b⟩)
            #[⟨⟨b⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩]).map
          fun blk => if blk.params.length = 0 then 0 else blk.body.length + 1).getD 0,
        [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩) ↔ _
    rw [hFind]; rfl
  let p0 := mkRegState llvmProfile ctrlMain b1 0 [] [] []
  let q0 := mkRegState llvmProfile ctrlMain b2 0 [] [] []
  have hpRes : (BIR.Execution (language := s0Core)).result inB p0 (retObs 42) := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hInView]
    with_unfolding_all change Option.bind (inM.terminalObservation p0) _ = some (retObs 42)
    rw [hTerm b1]; rfl
  have hqRes : (BIR.Execution (language := s0Core)).result outB q0 (retObs 42) := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hOutView]
    with_unfolding_all change Option.bind (outM.terminalObservation q0) _ = some (retObs 42)
    rw [hTerm b2]; rfl
  have hpTr : (BIR.Execution (language := s0Core)).trace inB p0 = [] := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hInView]
    with_unfolding_all change [] ++ ((Option.bind (inM.terminalObservation p0) _).map _).getD [] = []
    rw [hTerm b1]; rfl
  have hqTr : (BIR.Execution (language := s0Core)).trace outB q0 = [] := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hOutView]
    with_unfolding_all change [] ++ ((Option.bind (outM.terminalObservation q0) _).map _).getD [] = []
    rw [hTerm b2]; rfl
  have hpInit : (BIR.Execution (language := s0Core)).initial inB p0 := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hInView]; exact (hEntry b1 p0).mpr rfl
  have hqInit : ∀ s, (BIR.Execution (language := s0Core)).initial outB s → s = q0 := by
    intro s hs; unfold BIR.Execution Semantics.onInput at hs; dsimp only at hs; rw [hOutView] at hs
    exact (hEntry b2 s).mp hs
  have hRef : BehaviorRefinement (BIR.Execution (language := s0Core)) (BIR.Execution (language := s0Core))
      (BIR.Execution (language := s0Core)).result (BIR.Execution (language := s0Core)).result inB outB :=
    (DetChain.nil q0).refines ⟨p0, hpInit, .nil p0⟩ hpRes hqInit
      (fun o => ⟨fun h => (Option.some.inj (hqRes.symm.trans h)).symm, fun h => h ▸ hqRes⟩)
      (hpTr.trans hqTr.symm)
  exact ⟨hOutWF, rfl, rfl, rfl, (fun _ _ h => nomatch h), hAcc, hRef⟩

/-- Targeted nonidentity BIR block-renaming pass that rewrites `birBlock srcB` (preserving `project`)
to `birBlock dstB` (and declines every other candidate), enabling universal `(pass.toStage).Validated`. -/
private def validatedBIRRenamePass (srcB dstB : Nat) : IRPass s0Lang llvmProfile .bir := fun bir =>
  if isBirBlock srcB bir then some ({ birBlock dstB with project := bir.project }, renameBoundaryAcc srcB dstB) else none

private theorem validatedBIRRenamePass_apply (srcB dstB : Nat) :
    validatedBIRRenamePass srcB dstB (birBlock srcB) = some (birBlock dstB, renameBoundaryAcc srcB dstB) := by
  have hTrue : isBirBlock srcB (birBlock srcB) = true := by
    simp [isBirBlock, birBlock, isMainU8Sig, isMainRetFns, isRet42Block, isRet42Term, eqEarlyTy]
    rfl
  simp only [validatedBIRRenamePass, hTrue, ↓reduceIte]
  rfl

private theorem validatedBIRRenamePass_validated (srcB dstB : Nat)
    (hRegBIR : (birBlock dstB).RegisterFormation (language := s0Core)) :
    ((validatedBIRRenamePass srcB dstB).toStage (language := s0Core)).Validated := by
  refine (IRPass.toStage_validated_iff _).mpr ?_
  intro input candidate account hWF hpass
  unfold validatedBIRRenamePass at hpass
  split at hpass
  · rename_i hEq
    have hIn := isBirBlock_eq hEq
    cases Option.some.inj hpass
    have hSpan : ∃ sp, orig0.lexical.Binds input.project sp := by
      have hOrigMem : orig0 ∈ input.origins := by
        rw [hIn]; with_unfolding_all exact List.mem_cons_self
      exact hWF.2.2.2.1 orig0 hOrigMem
    rw [hIn]
    exact birBlockP_passPost input.project srcB dstB hSpan hRegBIR
  · cases hpass

/-- Control 2b: Nonidentity block renaming satisfies `PassPost` (with non-empty remapped
`Occurrence`s) at all 4 insertion points, composes via `PassPost.compose` (`1 → 2 → 4` vs
`1 → 2 → 3` order-sensitive composition), and validates end-to-end through `foldPasses_cons_validated`. -/
theorem nonidentity_block_renaming_passPost_and_validated :
    let pAdd := validatedBIRRenamePass 1 2
    let pDbl := validatedBIRRenamePass 2 4
    let pDblFirst := validatedBIRRenamePass 1 2
    let pAddSecond := validatedBIRRenamePass 2 3
    -- Nonidentity `PassPost` at all 4 points (block `0 → 7`, genuinely remapping `.boundary 0`):
    PassPost (slice := slice0) (profile := llvmProfile) (k := .mir)
      (mirBlock 0) (mirBlock 7) (renameBoundaryAcc 0 7) ∧
    PassPost (slice := slice0) (profile := llvmProfile) (k := .memory)
      (memBlock 0) (memBlock 7) (renameBoundaryAcc 0 7) ∧
    PassPost (slice := slice0) (profile := llvmProfile) (k := .ssa)
      (ssaBlock 0) (ssaBlock 7) (renameBoundaryAcc 0 7) ∧
    PassPost (slice := slice0) (profile := llvmProfile) (k := .bir)
      (birBlock 0) (birBlock 7) (renameBoundaryAcc 0 7) ∧
    -- `Account.identityFor` fails for nonidentity block renaming (occurrence address truly moved):
    ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      (birBlock 0) (birBlock 7) (Account.identityFor (point := .bir) (birBlock 0)) ∧
    -- Validated nonidentity `foldPasses` pipeline with `PassPost.compose`:
    (foldPasses (language := s0Core) [pAdd, pDbl]).Validated ∧
    (foldPasses (language := s0Core) [pDblFirst, pAddSecond]).Validated ∧
    (foldPasses (language := s0Core) [pAdd, pDbl]).run (birBlock 1) = .ok (birBlock 4) ∧
    (foldPasses (language := s0Core) [pDblFirst, pAddSecond]).run (birBlock 1) = .ok (birBlock 3) ∧
    PassPost (slice := slice0) (profile := llvmProfile) (k := .bir)
      (birBlock 1) (birBlock 4) (Account.compose (renameBoundaryAcc 1 2) (renameBoundaryAcc 2 4)) := by
  intro pAdd pDbl pDblFirst pAddSecond
  have h4 := passPost_renameBlock_all_four 0 7 (by decide) (by decide)
  have hComp : PassPost (slice := slice0) (profile := llvmProfile) (k := .bir)
      (birBlock 1) (birBlock 4) (Account.compose (renameBoundaryAcc 1 2) (renameBoundaryAcc 2 4)) :=
    PassPost.compose (passPost_renameBlock_all_four 1 2 (by decide) (by decide)).2.2.2
      (passPost_renameBlock_all_four 2 4 (by decide) (by decide)).2.2.2
      (fun _ _ h => nomatch h)
  refine ⟨h4.1, h4.2.1, h4.2.2.1, h4.2.2.2, by decide,
    foldPasses_cons_validated (validatedBIRRenamePass_validated 1 2 (by decide))
      (foldPasses_cons_validated (validatedBIRRenamePass_validated 2 4 (by decide)) foldPasses_nil_validated),
    foldPasses_cons_validated (validatedBIRRenamePass_validated 1 2 (by decide))
      (foldPasses_cons_validated (validatedBIRRenamePass_validated 2 3 (by decide)) foldPasses_nil_validated),
    ?_, ?_, hComp⟩
  · simp only [pAdd, pDbl, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      validatedBIRRenamePass_apply]
  · simp only [pDblFirst, pAddSecond, foldPasses, Pipeline.run, IRPass.toStage, IRPass.runOrIdentity,
      validatedBIRRenamePass_apply]

/-! ## Section 3: `AccountsFor`, `PassPost`, and `IRPass.toStage.Validated` negative controls -/

private def birTwoTrapSites (swap : Bool) : BIR s0Lang llvmProfile :=
  let b0 : Block _ _ _ :=
    ⟨⟨0⟩, [],
      [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[255]⟩) (.literal ⟨[1]⟩)⟩],
      .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
  let b1 : Block _ _ _ := ⟨⟨1⟩, [], [], .trap (if swap then ⟨1⟩ else ⟨0⟩), []⟩
  let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .trap (if swap then ⟨0⟩ else ⟨1⟩), []⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)], [],
    #[⟨.overflow, orig0⟩, ⟨.overflow, orig1⟩], [], []⟩

private def birTwoTrapB0View : MemoryBlock s0Lang :=
  ⟨⟨0⟩, [],
    [⟨orig0, .addWithOverflow .u8 ⟨⟨⟨0⟩, []⟩, .virtual⟩ ⟨⟨⟨1⟩, []⟩, .virtual⟩
      (.literal ⟨[255]⟩) (.literal ⟨[1]⟩)⟩],
    .branch (.copy ⟨⟨⟨1⟩, []⟩, .virtual⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩

private def birTwoTrapView (swap : Bool) : Memory s0Lang llvmProfile :=
  let b1V : MemoryBlock s0Lang := ⟨⟨1⟩, [], [], .trap .overflow (if swap then orig1 else orig0), []⟩
  let b2V : MemoryBlock s0Lang := ⟨⟨2⟩, [], [], .trap .overflow (if swap then orig0 else orig1), []⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [(⟨0⟩, u8Ty), (⟨1⟩, boolTy)], ⟨⟨0⟩, #[birTwoTrapB0View, b1V, b2V]⟩)],
    [], [(ctrlMain, [(⟨0⟩, .virtual), (⟨1⟩, .virtual)])],
    [(ctrlMain, [⟨0⟩, ⟨1⟩])], []⟩

private theorem birTwoTrap_view_eq (swap : Bool) :
    (birTwoTrapSites swap).memoryView (language := s0Core) = birTwoTrapView swap := by
  cases swap <;> with_unfolding_all rfl

private theorem birTwoTrap_step0 (swap : Bool) :
    let bir := birTwoTrapSites swap
    let s0 := mkRegState llvmProfile ctrlMain 0 0 [] [] []
    let s1 := mkRegState llvmProfile ctrlMain 0 1 [⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
      [(⟨1⟩, .scalar (.bool true)), (⟨0⟩, .scalar (.int .u8 0))]
    (BIR.Execution (language := s0Core)).step bir s0 s1 ∧
    ∀ next, (BIR.Execution (language := s0Core)).step bir s0 next → next = s1 := by
  intro bir s0 s1
  let mid0 := mkRegState llvmProfile ctrlMain 0 0 [⟨0⟩] [] []
  let mid1 := mkRegState llvmProfile ctrlMain 0 0 [⟨0⟩] [⟨0⟩] [(⟨0⟩, .scalar (.int .u8 0))]
  let mid2 := mkRegState llvmProfile ctrlMain 0 0 [⟨1⟩, ⟨0⟩] [⟨0⟩] [(⟨0⟩, .scalar (.int .u8 0))]
  have hView := birTwoTrap_view_eq swap
  constructor
  · unfold BIR.Execution Semantics.onInput; dsimp only; rw [hView]
    refine ⟨by with_unfolding_all rfl, ?_, ⟨_, by with_unfolding_all rfl⟩⟩
    refine ⟨birTwoTrapB0View, rfl, rfl, rfl, Or.inl (by decide),
      .scalar (.int .u8 255), s0, .scalar (.int .u8 1), s0, 255, 1,
      { s1 with instruction := 0 }, rfl, rfl, rfl, rfl, ?_, rfl⟩
    apply Memory.WriteParams.cons (started := mid0) (stored := mid1)
    · exact ⟨List.not_mem_nil, Or.inl ⟨rfl, rfl⟩⟩
    · exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := mid2) (stored := { s1 with instruction := 0 })
      · exact ⟨by simp [mid1, mkRegState], Or.inl ⟨rfl, rfl⟩⟩
      · exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
  · intro next ⟨_, hst, _⟩
    change (bir.memoryView (language := s0Core)).Step s0 next true at hst
    rw [hView] at hst
    simp only [Memory.Step, s0, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hinst⟩ := hst
      have hblkEq : blk = birTwoTrapB0View := Option.some.inj hblk.symm
      subst hblkEq
      exact absurd hinst (Memory.addWithOverflow_virtual_cannot_exhaust rfl rfl)
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hinst⟩ := hst
      have hblkEq : blk = birTwoTrapB0View := Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, _, _, lv, a1, rv, a2, x, y, stored, hl, hr, hlv, hrv, hwp, rfl⟩ := hinst
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hl.symm : (lv, a1) = (.scalar (.int .u8 255), s0))
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hr.symm : (rv, a2) = (.scalar (.int .u8 1), s0))
      have hx : x = 255 := by cases hlv; rfl
      have hy : y = 1 := by cases hrv; rfl
      subst hx; subst hy
      cases hwp with
      | cons hbeg1 hw1 hwp2 =>
        rename_i started1 stored1
        rcases hbeg1.2 with ⟨_, hEq1⟩ | ⟨hMat, _⟩
        · rw [hEq1] at hw1
          have hstor1 : stored1 = mid1 := Option.some.inj hw1.2.2.1.symm
          rw [hstor1] at hwp2
          cases hwp2 with
          | cons hbeg2 hw2 hwp3 =>
            rename_i started2 stored2
            rcases hbeg2.2 with ⟨_, hEq2⟩ | ⟨hMat, _⟩
            · rw [hEq2] at hw2
              have hstor2 : stored2 = { s1 with instruction := 0 } := Option.some.inj hw2.2.2.1.symm
              exact congrArg MemoryRegisters.advance ((writeParams_nil_eq hwp3).trans hstor2)
            · exact absurd hMat (by intro h; cases h)
        · exact absurd hMat (by intro h; cases h)

private theorem birTwoTrap_chain (swap : Bool) :
    let bir := birTwoTrapSites swap
    let s0 := mkRegState llvmProfile ctrlMain 0 0 [] [] []
    let s2 := mkRegState llvmProfile ctrlMain 1 0 [⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
      [(⟨1⟩, .scalar (.bool true)), (⟨0⟩, .scalar (.int .u8 0))]
    DetChain ((BIR.Execution (language := s0Core)).step bir) s0 s2 ∧
    (∀ o, (BIR.Execution (language := s0Core)).result bir s2 o ↔
      o = if swap then trapObs1 else trapObs0) := by
  intro bir s0 s2
  let s1 := mkRegState llvmProfile ctrlMain 0 1 [⟨1⟩, ⟨0⟩] [⟨1⟩, ⟨0⟩]
    [(⟨1⟩, .scalar (.bool true)), (⟨0⟩, .scalar (.int .u8 0))]
  let b1V : MemoryBlock s0Lang := ⟨⟨1⟩, [], [], .trap .overflow (if swap then orig1 else orig0), []⟩
  have hView := birTwoTrap_view_eq swap
  obtain ⟨step01, step01u⟩ := birTwoTrap_step0 swap
  have step12 : (BIR.Execution (language := s0Core)).step bir s1 s2 := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hView]
    cases swap <;>
      exact ⟨by with_unfolding_all rfl,
        ⟨birTwoTrapB0View, rfl, rfl, .scalar (.bool true), s1, true, rfl, rfl,
          [], s1, b1V, rfl, rfl, rfl, .nil _⟩,
        ⟨_, by with_unfolding_all rfl⟩⟩
  have step12u : ∀ next, (BIR.Execution (language := s0Core)).step bir s1 next → next = s2 := by
    intro next ⟨_, hst, _⟩
    change (bir.memoryView (language := s0Core)).Step s1 next true at hst
    rw [hView] at hst
    simp only [Memory.Step, s1, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
    cases hne : next.resourceExhausted with
    | true =>
      rw [ite_eq_left (by simp [hne])] at hst
      obtain ⟨_, _, blk, hblk, hrest⟩ := hst
      have hblkEq : blk = birTwoTrapB0View := Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, val, ar, fl, hrd, htst, vs, ar2, tgt, hro, hba, _, hpo⟩ := hrest
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hrd.symm : (val, ar) = (.scalar (.bool true), s1))
      have hfl : fl = true := by cases htst; rfl
      subst hfl
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hro.symm : (vs, ar2) = ([], s1))
      have htgtEq : tgt = b1V := Option.some.inj hba.symm
      subst htgtEq; exact (hpo.2.2 trivial).elim
    | false =>
      rw [ite_eq_right (by simp [hne])] at hst
      obtain ⟨blk, hblk, hrest⟩ := hst
      have hblkEq : blk = birTwoTrapB0View := Option.some.inj hblk.symm
      subst hblkEq
      obtain ⟨_, val, ar, fl, hrd, htst, vs, ar2, tgt, hro, hba, _, hwp⟩ := hrest
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hrd.symm : (val, ar) = (.scalar (.bool true), s1))
      have hfl : fl = true := by cases htst; rfl
      subst hfl
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hro.symm : (vs, ar2) = ([], s1))
      have htgtEq : tgt = b1V := Option.some.inj hba.symm
      subst htgtEq
      exact writeParams_nil_eq hwp
  refine ⟨.cons step01 step01u (.cons step12 step12u (.nil s2)), fun o => ?_⟩
  have hr : (BIR.Execution (language := s0Core)).result bir s2 (if swap then trapObs1 else trapObs0) := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hView]
    cases swap <;> with_unfolding_all rfl
  exact ⟨fun h => (Option.some.inj (hr.symm.trans h)).symm, fun h => h ▸ hr⟩

private theorem birTwoTrapSites_wf : (birTwoTrapSites false).WF (language := s0Core) := by
  have hView := birTwoTrap_view_eq false
  have hMemWF : ((birTwoTrapSites false).memoryView (language := s0Core)).HandoffWF := by
    rw [hView]
    have hNodup1 : [ctrlMain].Nodup := List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩
    have hNodup01 : ([⟨0⟩, ⟨1⟩] : List PlaceId).Nodup := by decide
    have hNodupB : ([⟨0⟩, ⟨1⟩, ⟨2⟩] : List BlockId).Nodup := by decide
    refine ⟨?_, List.mem_cons_self, ?_, (fun _ h => nomatch h),
      ⟨List.nodup_nil, ⟨List.nodup_nil, (fun _ h => nomatch h)⟩, (fun _ _ h => nomatch h)⟩, ?_⟩
    · refine ⟨hNodup1, hNodup1, hNodup1, List.nodup_nil, ?_, (fun _ _ h => nomatch h), ?_⟩
      · intro fn cls hmem; obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem; exact hNodup01
      · intro fn ls gr hmem
        obtain ⟨rfl, rfl, rfl⟩ := mem_singleton_triple hmem
        refine ⟨hNodup01, ⟨hNodupB, ⟨_, List.mem_cons_self, rfl⟩, ?_⟩,
          ⟨⟨[], some u8Ty⟩, rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩, ?_⟩⟩
        · intro blk hblk edge hedge
          rcases List.mem_cons.mp hblk with rfl | hb1
          · rcases List.mem_cons.mp hedge with rfl | he2
            · exact ⟨_, List.mem_cons_of_mem _ List.mem_cons_self, rfl, rfl⟩
            · rcases List.mem_singleton.mp he2 with rfl
              exact ⟨_, List.mem_cons_of_mem _ (List.mem_cons_of_mem _ List.mem_cons_self), rfl, rfl⟩
          · rcases List.mem_cons.mp hb1 with rfl | hb2
            · cases hedge
            · rcases List.mem_singleton.mp hb2 with rfl; cases hedge
        · intro blk hblk
          rcases List.mem_cons.mp hblk with rfl | hb1
          · exact ⟨trivial, fun op hop => by rcases List.mem_singleton.mp hop with rfl; trivial⟩
          · rcases List.mem_cons.mp hb1 with rfl | hb2
            · exact ⟨trivial, fun _ h => nomatch h⟩
            · rcases List.mem_singleton.mp hb2 with rfl; exact ⟨trivial, fun _ h => nomatch h⟩
    · intro orig horig
      have horig' : orig ∈ ([orig0, orig0, orig1] : List Origin) := by with_unfolding_all exact horig
      rcases List.mem_cons.mp horig' with rfl | h1
      · exact span0_binds
      · rcases List.mem_cons.mp h1 with rfl | h2
        · exact span0_binds
        · rcases List.mem_singleton.mp h2 with rfl; exact span1_binds
    · refine ⟨hNodup1, fun fn roots hmem => ?_⟩
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      refine ⟨_, _, List.mem_cons_self, fun root hroot => ?_⟩
      rcases List.mem_cons.mp hroot with rfl | hrest
      · exact ⟨u8Ty, List.mem_cons_self⟩
      · rcases List.mem_singleton.mp hrest with rfl; exact ⟨boolTy, List.mem_cons_of_mem _ List.mem_cons_self⟩
  refine ⟨by decide, ?_, by decide, ?_, ?_, hMemWF⟩
  · intro fn hfn blk hblk slot htrap
    rcases List.mem_singleton.mp hfn with rfl
    rcases List.mem_cons.mp hblk with rfl | hb1
    · cases htrap
    · rcases List.mem_cons.mp hb1 with rfl | hb2
      · cases BIRTerm.trap.inj htrap; exact ⟨⟨.overflow, orig0⟩, rfl⟩
      · rcases List.mem_singleton.mp hb2 with rfl
        cases BIRTerm.trap.inj htrap; exact ⟨⟨.overflow, orig1⟩, rfl⟩
  · intro orig horig
    have horig' : orig ∈ ([orig0, orig0, orig1] : List Origin) := by with_unfolding_all exact horig
    rcases List.mem_cons.mp horig' with rfl | h1
    · exact span0_binds
    · rcases List.mem_cons.mp h1 with rfl | h2
      · exact span0_binds
      · rcases List.mem_singleton.mp h2 with rfl; exact span1_binds
  · intro site hsite
    rcases List.mem_cons.mp hsite with rfl | h2
    · exact span0_binds
    · rcases List.mem_singleton.mp h2 with rfl; exact span1_binds

private def birSharedOriginTwoOps : BIR s0Lang llvmProfile :=
  let b0 : Block _ _ _ :=
    ⟨⟨0⟩, [],
      [⟨orig0, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨0⟩ () (.cons (.literal ⟨[42]⟩) .nil)⟩,
       ⟨orig0, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨1⟩ () (.cons (.literal ⟨[7]⟩) .nil)⟩],
      .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩

private def birSharedOriginDropped : BIR s0Lang llvmProfile :=
  let b0 : Block _ _ _ :=
    ⟨⟨0⟩, [],
      [⟨orig0, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨0⟩ () (.cons (.literal ⟨[42]⟩) .nil)⟩],
      .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩

private theorem birSharedOriginTwoOps_wf : birSharedOriginTwoOps.WF (language := s0Core) := by
  have hView : birSharedOriginTwoOps.memoryView (language := s0Core) =
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [(⟨0⟩, u8Ty), (⟨1⟩, u8Ty)],
          ⟨⟨0⟩, #[⟨⟨0⟩, [],
            [⟨orig0, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty)
              ⟨⟨⟨0⟩, []⟩, .virtual⟩ () (.cons (.literal ⟨[42]⟩) .nil) [] []⟩,
             ⟨orig0, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty)
              ⟨⟨⟨1⟩, []⟩, .virtual⟩ () (.cons (.literal ⟨[7]⟩) .nil) [] []⟩],
            .ret (some ⟨u8Ty, .copy ⟨⟨⟨0⟩, []⟩, .virtual⟩⟩), []⟩]⟩)],
        [], [(ctrlMain, [(⟨0⟩, .virtual), (⟨1⟩, .virtual)])],
        [(ctrlMain, [⟨0⟩, ⟨1⟩])], []⟩ := by
    with_unfolding_all rfl
  have hMemWF : (birSharedOriginTwoOps.memoryView (language := s0Core)).HandoffWF := by
    rw [hView]
    have hNodup1 : [ctrlMain].Nodup := List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩
    have hNodup01 : ([⟨0⟩, ⟨1⟩] : List PlaceId).Nodup := by decide
    have hNodupB : ([⟨0⟩] : List BlockId).Nodup := by decide
    refine ⟨?_, List.mem_cons_self, ?_, (fun _ h => nomatch h),
      ⟨List.nodup_nil, ⟨List.nodup_nil, (fun _ h => nomatch h)⟩, (fun _ _ h => nomatch h)⟩, ?_⟩
    · refine ⟨hNodup1, hNodup1, hNodup1, List.nodup_nil, ?_, (fun _ _ h => nomatch h), ?_⟩
      · intro fn cls hmem; obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem; exact hNodup01
      · intro fn ls gr hmem
        obtain ⟨rfl, rfl, rfl⟩ := mem_singleton_triple hmem
        refine ⟨hNodup01, ⟨hNodupB, ⟨_, List.mem_cons_self, rfl⟩,
          fun blk hblk _ hedge => by rcases List.mem_singleton.mp hblk with rfl; cases hedge⟩,
          ⟨⟨[], some u8Ty⟩, rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩,
            fun blk hblk => by
              rcases List.mem_singleton.mp hblk with rfl
              exact ⟨rfl, fun op hop => by
                rcases List.mem_cons.mp hop with rfl | h2
                · trivial
                · rcases List.mem_singleton.mp h2 with rfl; trivial⟩⟩⟩
    · intro orig horig
      have horig' : orig ∈ ([orig0, orig0] : List Origin) := by with_unfolding_all exact horig
      rcases List.mem_cons.mp horig' with rfl | h2
      · exact span0_binds
      · rcases List.mem_singleton.mp h2 with rfl; exact span0_binds
    · refine ⟨hNodup1, fun fn roots hmem => ?_⟩
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      refine ⟨_, _, List.mem_cons_self, fun root hroot => ?_⟩
      rcases List.mem_cons.mp hroot with rfl | h2
      · exact ⟨u8Ty, List.mem_cons_self⟩
      · rcases List.mem_singleton.mp h2 with rfl; exact ⟨u8Ty, List.mem_cons_of_mem _ List.mem_cons_self⟩
  refine ⟨by decide, ?_, by decide, ?_, (fun _ h => nomatch h), hMemWF⟩
  · intro fn hfn blk hblk slot htrap
    rcases List.mem_singleton.mp hfn with rfl
    rcases List.mem_singleton.mp hblk with rfl
    cases htrap
  · intro orig horig
    have horig' : orig ∈ ([orig0, orig0] : List Origin) := by with_unfolding_all exact horig
    rcases List.mem_cons.mp horig' with rfl | h2
    · exact span0_binds
    · rcases List.mem_singleton.mp h2 with rfl; exact span0_binds

/-- Control 3a: Swapped trap origins (both under position-wise accounting and under cross-mapped
accounting) AND lost shared-origin occurrence (even though the `Origin` remains in `origins`) are
ALL rejected by `AccountsFor`, `PassPost`, and `IRPass.toStage.Validated`. -/
theorem accountsFor_swapped_trap_origins_and_lost_shared_origin_rejected :
    let origBIR := birTwoTrapSites false
    let swappedBIR := birTwoTrapSites true
    let posAcc := Account.identityFor (point := .bir) origBIR
    let crossAcc : Account :=
      { mapped := [(⟨ctrlMain, ⟨0⟩, .instruction 0⟩, [⟨ctrlMain, ⟨0⟩, .instruction 0⟩]),
          (⟨ctrlMain, ⟨1⟩, .trap⟩, [⟨ctrlMain, ⟨2⟩, .trap⟩]),
          (⟨ctrlMain, ⟨2⟩, .trap⟩, [⟨ctrlMain, ⟨1⟩, .trap⟩])],
        eliminated := [] }
    let swapPosPass : IRPass s0Lang llvmProfile .bir := fun _ => some (swappedBIR, posAcc)
    let swapCrossPass : IRPass s0Lang llvmProfile .bir := fun _ => some (swappedBIR, crossAcc)
    -- Lost shared-origin occurrence (`orig0` appears twice in input, once in output, with empty `eliminated`):
    let lostAcc : Account :=
      { mapped := [(⟨ctrlMain, ⟨0⟩, .instruction 0⟩, [⟨ctrlMain, ⟨0⟩, .instruction 0⟩])],
        eliminated := [] }
    let lostPass : IRPass s0Lang llvmProfile .bir := fun _ => some (birSharedOriginDropped, lostAcc)
    -- 1. Position-wise swapped trap origins fail `AccountsFor`, `PassPost`, and `Validated`:
    ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir) origBIR swappedBIR posAcc ∧
    ¬ PassPost (slice := slice0) (profile := llvmProfile) (k := .bir) origBIR swappedBIR posAcc ∧
    ¬ (swapPosPass.toStage (language := s0Core)).Validated ∧
    -- 2. Cross-mapped swapped trap origins fail `BehaviorRefinement`, `PassPost`, and `Validated`
    -- (so NO accounting witness can validate swapping trap sites):
    (∀ acc, ¬ PassPost (slice := slice0) (profile := llvmProfile) (k := .bir) origBIR swappedBIR acc) ∧
    ¬ (swapCrossPass.toStage (language := s0Core)).Validated ∧
    -- 3. Lost shared-origin occurrence: `PreservesOrigins` holds, yet `AccountsFor`, `PassPost`, and `Validated` reject it!
    PreservesOrigins birSharedOriginTwoOps.origins birSharedOriginDropped.origins ∧
    ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      birSharedOriginTwoOps birSharedOriginDropped lostAcc ∧
    ¬ PassPost (slice := slice0) (profile := llvmProfile) (k := .bir)
      birSharedOriginTwoOps birSharedOriginDropped lostAcc ∧
    ¬ (lostPass.toStage (language := s0Core)).Validated := by
  intro origBIR swappedBIR posAcc crossAcc swapPosPass swapCrossPass lostAcc lostPass
  have hNoPos : ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      origBIR swappedBIR posAcc := by decide
  have hNoRef : ¬ BehaviorRefinement (BIR.Execution (language := s0Core)) (BIR.Execution (language := s0Core))
      (BIR.Execution (language := s0Core)).result (BIR.Execution (language := s0Core)).result
      origBIR swappedBIR := by
    have hOrig := birTwoTrap_chain false
    have hSwap := birTwoTrap_chain true
    have hSwapObs : (BIR.Execution (language := s0Core)).observes swappedBIR trapObs1 :=
      (hSwap.1.observes_iff (by with_unfolding_all rfl) (fun _ h => by with_unfolding_all exact h)
        hSwap.2 trapObs1).mpr rfl
    have hNe : trapObs1 ≠ trapObs0 := by
      intro h
      have hSite := (Concrete.Contracts.Observation.trapped.inj h).2.2.1
      exact (by decide : (⟨1⟩ : SiteId) ≠ ⟨0⟩) (congrArg DiagnosticSite.id hSite)
    exact hOrig.1.rejects_refinement (by with_unfolding_all rfl)
      (fun _ h => by with_unfolding_all exact h) hOrig.2 hSwapObs hNe
  have hNoLost : ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      birSharedOriginTwoOps birSharedOriginDropped lostAcc := by decide
  have hPresOrig : PreservesOrigins birSharedOriginTwoOps.origins birSharedOriginDropped.origins := by
    have h1 : birSharedOriginTwoOps.origins = [orig0, orig0] := by with_unfolding_all rfl
    have h2 : birSharedOriginDropped.origins = [orig0] := by with_unfolding_all rfl
    rw [h1, h2]
    refine ⟨fun o ho => ?_, fun o ho => ?_⟩
    · rcases List.mem_singleton.mp ho with rfl; exact List.mem_cons_self
    · rcases List.mem_cons.mp ho with rfl | hrest
      · exact List.mem_cons_self
      · rcases List.mem_singleton.mp hrest with rfl; exact List.mem_cons_self
  refine ⟨hNoPos, fun h => hNoPos h.2.2.2.2.2.1, ?_, fun _ h => hNoRef h.2.2.2.2.2.2, ?_,
    hPresOrig, hNoLost, fun h => hNoLost h.2.2.2.2.2.1, ?_⟩
  · intro hVal
    have hPost := (IRPass.toStage_validated_iff swapPosPass).mp hVal origBIR swappedBIR posAcc
      birTwoTrapSites_wf rfl
    exact hNoPos hPost.2.2.2.2.2.1
  · intro hVal
    have hPost := (IRPass.toStage_validated_iff swapCrossPass).mp hVal origBIR swappedBIR crossAcc
      birTwoTrapSites_wf rfl
    exact hNoRef hPost.2.2.2.2.2.2
  · intro hVal
    have hPost := (IRPass.toStage_validated_iff lostPass).mp hVal birSharedOriginTwoOps
      birSharedOriginDropped lostAcc birSharedOriginTwoOps_wf rfl
    exact hNoLost hPost.2.2.2.2.2.1

/-- Control 3b: False elimination reasons (live trap as `.deadValue`, used pure value as `.deadValue`,
reachable block as `.unreachableBlock`, and self-justifying cycle `v0 = read v1; v1 = read v0` as
`.deadValue`) are ALL rejected by `AccountsFor` and `PassPost`, whereas a genuinely unused pure
value (`.deadValue`) and genuinely unreachable block (`.unreachableBlock`) are accepted. -/
theorem accountsFor_false_elimination_reasons_rejected :
    -- 1. Live trap eliminated with `.deadValue`
    let liveTrapBIR : BIR s0Lang llvmProfile :=
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .trap ⟨0⟩, []⟩]⟩)], [], #[⟨.overflow, orig0⟩], [], []⟩
    let emptyRetBIR : BIR s0Lang llvmProfile :=
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩]⟩)], [], #[], [], []⟩
    let trapAsDead : Account := { mapped := [], eliminated := [(⟨ctrlMain, ⟨0⟩, .trap⟩, .deadValue)] }
    -- 2. Used pure value eliminated with `.deadValue`
    let usedPureBIR := birSharedOriginDropped
    let usedAsDead : Account := { mapped := [], eliminated := [(⟨ctrlMain, ⟨0⟩, .instruction 0⟩, .deadValue)] }
    -- 3. Reachable block occurrence eliminated with `.unreachableBlock`
    let reachAsUnreach : Account := { mapped := [], eliminated := [(⟨ctrlMain, ⟨0⟩, .instruction 0⟩, .unreachableBlock)] }
    -- 4. Self-justifying dead-value cycle (`v0 = read v1; v1 = read v0`, both marked `.deadValue`)
    let cycleBIR : BIR s0Lang llvmProfile :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig0, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨0⟩ () (.cons (.value ⟨1⟩) .nil)⟩,
           ⟨orig1, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨1⟩ () (.cons (.value ⟨0⟩) .nil)⟩],
          .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩
    let cycleAsDead : Account :=
      { mapped := [],
        eliminated := [(⟨ctrlMain, ⟨0⟩, .instruction 0⟩, .deadValue), (⟨ctrlMain, ⟨0⟩, .instruction 1⟩, .deadValue)] }
    -- 5. Positive controls: genuinely unused pure value (`.deadValue`) & genuinely unreachable block (`.unreachableBlock`)
    let goodDeadAcc : Account :=
      { mapped := [(⟨ctrlMain, ⟨0⟩, .instruction 0⟩, [⟨ctrlMain, ⟨0⟩, .instruction 0⟩])],
        eliminated := [(⟨ctrlMain, ⟨0⟩, .instruction 1⟩, .deadValue)] }
    let withDeadBlockBIR : BIR s0Lang llvmProfile :=
      let b0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
      let b1 : Block _ _ _ := ⟨⟨1⟩, [], [], .trap ⟨0⟩, []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1]⟩)], [], #[⟨.overflow, orig0⟩], [], []⟩
    let goodUnreachBlockAcc : Account :=
      { mapped := [], eliminated := [(⟨ctrlMain, ⟨1⟩, .trap⟩, .unreachableBlock)] }
    ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir) liveTrapBIR emptyRetBIR trapAsDead ∧
    ¬ PassPost (slice := slice0) (profile := llvmProfile) (k := .bir) liveTrapBIR emptyRetBIR trapAsDead ∧
    ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir) usedPureBIR emptyRetBIR usedAsDead ∧
    ¬ PassPost (slice := slice0) (profile := llvmProfile) (k := .bir) usedPureBIR emptyRetBIR usedAsDead ∧
    ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir) usedPureBIR emptyRetBIR reachAsUnreach ∧
    ¬ PassPost (slice := slice0) (profile := llvmProfile) (k := .bir) usedPureBIR emptyRetBIR reachAsUnreach ∧
    ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir) cycleBIR emptyRetBIR cycleAsDead ∧
    ¬ PassPost (slice := slice0) (profile := llvmProfile) (k := .bir) cycleBIR emptyRetBIR cycleAsDead ∧
    AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      birSharedOriginTwoOps birSharedOriginDropped goodDeadAcc ∧
    AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      withDeadBlockBIR emptyRetBIR goodUnreachBlockAcc := by
  intro liveTrapBIR emptyRetBIR trapAsDead usedPureBIR usedAsDead reachAsUnreach
    cycleBIR cycleAsDead goodDeadAcc withDeadBlockBIR goodUnreachBlockAcc
  have h1 : ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      liveTrapBIR emptyRetBIR trapAsDead := by decide
  have h2 : ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      usedPureBIR emptyRetBIR usedAsDead := by decide
  have h3 : ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      usedPureBIR emptyRetBIR reachAsUnreach := by decide
  have h4 : ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      cycleBIR emptyRetBIR cycleAsDead := by decide
  exact ⟨h1, fun h => h1 h.2.2.2.2.2.1, h2, fun h => h2 h.2.2.2.2.2.1,
    h3, fun h => h3 h.2.2.2.2.2.1, h4, fun h => h4 h.2.2.2.2.2.1,
    by decide, by decide⟩

/-! ## Section 4: Uncalled vs called helper function elimination and mutation controls -/

private def birUncalledHelper (helperVal : UInt8) : BIR s0Lang llvmProfile :=
  let m0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩
  let h0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[helperVal]⟩⟩), [origHelper]⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[m0]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0]⟩)], [], #[], [], []⟩

private def birCalledHelper (helperVal : UInt8) : BIR s0Lang llvmProfile :=
  let m0 : Block _ _ _ :=
    ⟨⟨0⟩, [], [⟨orig0, .call ctrlHelper (some ⟨u8Ty, ⟨0⟩⟩) []⟩],
      .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
  let h0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[helperVal]⟩⟩), [origHelper]⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[m0]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0]⟩)], [], #[], [], []⟩

private theorem birUncalledHelper_wf (v : UInt8)
    (hReg : (birUncalledHelper v).RegisterFormation (language := s0Core)) :
    (birUncalledHelper v).WF (language := s0Core) := by
  have hView : (birUncalledHelper v).memoryView (language := s0Core) =
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩]⟩),
         (ctrlHelper, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[v]⟩⟩), [origHelper]⟩]⟩)],
        [], [(ctrlMain, []), (ctrlHelper, [])], [], []⟩ := by
    with_unfolding_all rfl
  have hMemWF : ((birUncalledHelper v).memoryView (language := s0Core)).HandoffWF := by
    rw [hView]
    have hNodup2 : [ctrlMain, ctrlHelper].Nodup := by decide
    have hNodupB : ([⟨0⟩] : List BlockId).Nodup := by decide
    refine ⟨?_, List.mem_cons_self, ?_, (fun _ h => nomatch h),
      ⟨List.nodup_nil, ⟨List.nodup_nil, (fun _ h => nomatch h)⟩, (fun _ _ h => nomatch h)⟩,
      ⟨List.nodup_nil, (fun _ _ h => nomatch h)⟩⟩
    · refine ⟨hNodup2, hNodup2, hNodup2, List.nodup_nil, ?_, (fun _ _ h => nomatch h), ?_⟩
      · intro fn cls hmem
        rcases List.mem_cons.mp hmem with h1 | h2
        · obtain ⟨rfl, rfl⟩ := Prod.mk.inj h1; exact List.nodup_nil
        · obtain ⟨rfl, rfl⟩ := mem_singleton_pair h2; exact List.nodup_nil
      · intro fn ls gr hmem
        rcases List.mem_cons.mp hmem with h1 | h2
        · obtain ⟨rfl, h1'⟩ := Prod.mk.inj h1; obtain ⟨rfl, rfl⟩ := Prod.mk.inj h1'
          refine ⟨List.nodup_nil, ⟨hNodupB, ⟨_, List.mem_cons_self, rfl⟩,
            fun blk hblk _ hedge => by rcases List.mem_singleton.mp hblk with rfl; cases hedge⟩,
            ⟨⟨[], some u8Ty⟩, rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩,
              fun blk hblk => by rcases List.mem_singleton.mp hblk with rfl; exact ⟨rfl, fun _ h => nomatch h⟩⟩⟩
        · obtain ⟨rfl, rfl, rfl⟩ := mem_singleton_triple h2
          refine ⟨List.nodup_nil, ⟨hNodupB, ⟨_, List.mem_cons_self, rfl⟩,
            fun blk hblk _ hedge => by rcases List.mem_singleton.mp hblk with rfl; cases hedge⟩,
            ⟨⟨[], some u8Ty⟩, by simp [AList.lookup, show ctrlMain ≠ ctrlHelper from by decide], ⟨_, List.mem_cons_self, rfl, rfl⟩,
              fun blk hblk => by rcases List.mem_singleton.mp hblk with rfl; exact ⟨rfl, fun _ h => nomatch h⟩⟩⟩
    · intro orig horig
      have horig' : orig ∈ ([orig0, origHelper] : List Origin) := by with_unfolding_all exact horig
      rcases List.mem_cons.mp horig' with rfl | h2
      · exact span0_binds
      · rcases List.mem_singleton.mp h2 with rfl; exact span1_binds
  have hSlots : (birUncalledHelper v).frameSlots ≤ llvmProfile.maxBIRSlots := by
    have h0 : (birUncalledHelper v).frameSlots = 0 := by with_unfolding_all rfl
    rw [h0]; decide
  refine ⟨hReg, ?_, hSlots, ?_, (fun _ h => nomatch h), hMemWF⟩
  · intro fn hfn blk hblk slot htrap
    rcases List.mem_cons.mp hfn with rfl | h2
    · rcases List.mem_singleton.mp hblk with rfl; cases htrap
    · rcases List.mem_singleton.mp h2 with rfl
      rcases List.mem_singleton.mp hblk with rfl; cases htrap
  · intro orig horig
    have horig' : orig ∈ ([orig0, origHelper] : List Origin) := by with_unfolding_all exact horig
    rcases List.mem_cons.mp horig' with rfl | h2
    · exact span0_binds
    · rcases List.mem_singleton.mp h2 with rfl; exact span1_binds

private theorem birCalledHelper_wf (v : UInt8)
    (hReg : (birCalledHelper v).RegisterFormation (language := s0Core)) :
    (birCalledHelper v).WF (language := s0Core) := by
  have hView : (birCalledHelper v).memoryView (language := s0Core) =
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [(⟨0⟩, u8Ty)],
          ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨orig0, .call ctrlHelper (some ⟨u8Ty, ⟨⟨⟨0⟩, []⟩, .virtual⟩⟩) [] []⟩],
            .ret (some ⟨u8Ty, .copy ⟨⟨⟨0⟩, []⟩, .virtual⟩⟩), []⟩]⟩),
         (ctrlHelper, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[v]⟩⟩), [origHelper]⟩]⟩)],
        [], [(ctrlMain, [(⟨0⟩, .virtual)]), (ctrlHelper, [])],
        [(ctrlMain, [⟨0⟩])], []⟩ := by
    with_unfolding_all rfl
  have hMemWF : ((birCalledHelper v).memoryView (language := s0Core)).HandoffWF := by
    rw [hView]
    have hNodup2 : [ctrlMain, ctrlHelper].Nodup := by decide
    have hNodup1 : [ctrlMain].Nodup := List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩
    have hNodup0 : ([⟨0⟩] : List PlaceId).Nodup := by decide
    have hNodupB : ([⟨0⟩] : List BlockId).Nodup := by decide
    refine ⟨?_, List.mem_cons_self, ?_, (fun _ h => nomatch h),
      ⟨List.nodup_nil, ⟨List.nodup_nil, (fun _ h => nomatch h)⟩, (fun _ _ h => nomatch h)⟩, ?_⟩
    · refine ⟨hNodup2, hNodup2, hNodup2, List.nodup_nil, ?_, (fun _ _ h => nomatch h), ?_⟩
      · intro fn cls hmem
        rcases List.mem_cons.mp hmem with h1 | h2
        · obtain ⟨rfl, rfl⟩ := Prod.mk.inj h1; exact hNodup0
        · obtain ⟨rfl, rfl⟩ := mem_singleton_pair h2; exact List.nodup_nil
      · intro fn ls gr hmem
        rcases List.mem_cons.mp hmem with h1 | h2
        · obtain ⟨rfl, h1'⟩ := Prod.mk.inj h1; obtain ⟨rfl, rfl⟩ := Prod.mk.inj h1'
          refine ⟨hNodup0, ⟨hNodupB, ⟨_, List.mem_cons_self, rfl⟩,
            fun blk hblk _ hedge => by rcases List.mem_singleton.mp hblk with rfl; cases hedge⟩,
            ⟨⟨[], some u8Ty⟩, rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩,
              fun blk hblk => by
                rcases List.mem_singleton.mp hblk with rfl
                refine ⟨rfl, fun op hop => ?_⟩
                rcases List.mem_singleton.mp hop with rfl
                exact ⟨⟨[], some u8Ty⟩, by simp [AList.lookup, show ctrlMain ≠ ctrlHelper from by decide], rfl, rfl⟩⟩⟩
        · obtain ⟨rfl, rfl, rfl⟩ := mem_singleton_triple h2
          refine ⟨List.nodup_nil, ⟨hNodupB, ⟨_, List.mem_cons_self, rfl⟩,
            fun blk hblk _ hedge => by rcases List.mem_singleton.mp hblk with rfl; cases hedge⟩,
            ⟨⟨[], some u8Ty⟩, by simp [AList.lookup, show ctrlMain ≠ ctrlHelper from by decide], ⟨_, List.mem_cons_self, rfl, rfl⟩,
              fun blk hblk => by rcases List.mem_singleton.mp hblk with rfl; exact ⟨rfl, fun _ h => nomatch h⟩⟩⟩
    · intro orig horig
      have horig' : orig ∈ ([orig0, origHelper] : List Origin) := by with_unfolding_all exact horig
      rcases List.mem_cons.mp horig' with rfl | h2
      · exact span0_binds
      · rcases List.mem_singleton.mp h2 with rfl; exact span1_binds
    · refine ⟨hNodup1, fun fn roots hmem => ?_⟩
      obtain ⟨rfl, rfl⟩ := mem_singleton_pair hmem
      refine ⟨_, _, List.mem_cons_self, fun root hroot => ?_⟩
      rcases List.mem_singleton.mp hroot with rfl
      exact ⟨u8Ty, List.mem_cons_self⟩
  have hSlots : (birCalledHelper v).frameSlots ≤ llvmProfile.maxBIRSlots := by
    have h1 : (birCalledHelper v).frameSlots = 1 := by with_unfolding_all rfl
    rw [h1]; decide
  refine ⟨hReg, ?_, hSlots, ?_, (fun _ h => nomatch h), hMemWF⟩
  · intro fn hfn blk hblk slot htrap
    rcases List.mem_cons.mp hfn with rfl | h2
    · rcases List.mem_singleton.mp hblk with rfl; cases htrap
    · rcases List.mem_singleton.mp h2 with rfl
      rcases List.mem_singleton.mp hblk with rfl; cases htrap
  · intro orig horig
    have horig' : orig ∈ ([orig0, origHelper] : List Origin) := by with_unfolding_all exact horig
    rcases List.mem_cons.mp horig' with rfl | h2
    · exact span0_binds
    · rcases List.mem_singleton.mp h2 with rfl; exact span1_binds

private theorem birCalledHelper_chain (v : UInt8) (hv : v = 1 ∨ v = 99) :
    let bir := birCalledHelper v
    let s0 := mkRegState llvmProfile ctrlMain 0 0 [] [] []
    let s2 := mkRegState llvmProfile ctrlMain 0 1 [⟨0⟩] [⟨0⟩] [(⟨0⟩, .scalar (.int .u8 (v.toNat : Int)))]
    (BIR.Execution (language := s0Core)).initial bir s0 ∧
    (∀ s, (BIR.Execution (language := s0Core)).initial bir s → s = s0) ∧
    DetChain ((BIR.Execution (language := s0Core)).step bir) s0 s2 ∧
    (∀ o, (BIR.Execution (language := s0Core)).result bir s2 o ↔ o = retObs v) := by
  intro bir s0 s2
  let m0V : MemoryBlock s0Lang :=
    ⟨⟨0⟩, [], [⟨orig0, .call ctrlHelper (some ⟨u8Ty, ⟨⟨⟨0⟩, []⟩, .virtual⟩⟩) [] []⟩],
      .ret (some ⟨u8Ty, .copy ⟨⟨⟨0⟩, []⟩, .virtual⟩⟩), []⟩
  let h0V : MemoryBlock s0Lang :=
    ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[v]⟩⟩), [origHelper]⟩
  let callerFrame : MemoryContinuation EarlyTy EarlyValue :=
    ⟨⟨ctrlMain, ⟨0⟩, 1, [], [], [], [], [], [], []⟩, some ⟨u8Ty, ⟨⟨⟨0⟩, []⟩, .virtual⟩⟩⟩
  let s1 := mkRegState llvmProfile ctrlHelper 0 0 [] [] [] [callerFrame]
  let retVal : s0Core.operands.Val := CoreValue.scalar (CVal.int IntTy.u8 (v.toNat : Int))
  let retO := retObs v
  have hMainNeHelper : ctrlMain ≠ ctrlHelper := by decide
  rcases hv with rfl | rfl <;> (
    have hView : bir.memoryView (language := s0Core) =
        ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
          [(ctrlMain, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[m0V]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0V]⟩)],
          [], [(ctrlMain, [(⟨0⟩, .virtual)]), (ctrlHelper, [])],
          [(ctrlMain, [⟨0⟩])], []⟩ := by
      with_unfolding_all rfl
    have hInit : (BIR.Execution (language := s0Core)).initial bir s0 := by
      unfold BIR.Execution Semantics.onInput; dsimp only; rw [hView]; with_unfolding_all rfl
    have hInitU : ∀ s, (BIR.Execution (language := s0Core)).initial bir s → s = s0 := by
      intro s hs; unfold BIR.Execution Semantics.onInput at hs; dsimp only at hs; rw [hView] at hs
      with_unfolding_all exact hs
    have step01 : (BIR.Execution (language := s0Core)).step bir s0 s1 := by
      unfold BIR.Execution Semantics.onInput; dsimp only; rw [hView]
      refine ⟨by with_unfolding_all rfl, ?_, ⟨_, by with_unfolding_all rfl⟩⟩
      exact ⟨m0V, rfl, rfl, [], s0, ⟨⟨0⟩, #[h0V]⟩, h0V, rfl,
        by simp [Memory.graphAt, AList.lookup, hMainNeHelper], rfl, rfl, .nil _⟩
    have step01u : ∀ next, (BIR.Execution (language := s0Core)).step bir s0 next → next = s1 := by
      intro next ⟨_, hst, _⟩
      change (bir.memoryView (language := s0Core)).Step s0 next true at hst
      rw [hView] at hst
      simp only [Memory.Step, s0, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
      cases hne : next.resourceExhausted with
      | true =>
        rw [ite_eq_left (by simp [hne])] at hst
        obtain ⟨_, _, blk, hblk, hrest⟩ := hst
        have hblkEq : blk = m0V := Option.some.inj hblk.symm
        subst hblkEq
        obtain ⟨_, vs, ar, gr, ent, hro, hga, hbe, _, hpo⟩ := hrest
        obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hro.symm : (vs, ar) = ([], s0))
        have hgrEq : gr = ⟨⟨0⟩, #[h0V]⟩ :=
          Option.some.inj (by simpa [Memory.graphAt, AList.lookup, hMainNeHelper] using hga.symm)
        subst hgrEq
        have hentEq : ent = h0V := Option.some.inj hbe.symm
        subst hentEq
        exact (hpo.2.2 trivial).elim
      | false =>
        rw [ite_eq_right (by simp [hne])] at hst
        obtain ⟨blk, hblk, hrest⟩ := hst
        have hblkEq : blk = m0V := Option.some.inj hblk.symm
        subst hblkEq
        obtain ⟨_, vs, ar, gr, ent, hro, hga, hbe, _, hwp⟩ := hrest
        obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hro.symm : (vs, ar) = ([], s0))
        have hgrEq : gr = ⟨⟨0⟩, #[h0V]⟩ :=
          Option.some.inj (by simpa [Memory.graphAt, AList.lookup, hMainNeHelper] using hga.symm)
        subst hgrEq
        have hentEq : ent = h0V := Option.some.inj hbe.symm
        subst hentEq
        exact writeParams_nil_eq hwp
    have step12 : (BIR.Execution (language := s0Core)).step bir s1 s2 := by
      unfold BIR.Execution Semantics.onInput; dsimp only; rw [hView]
      refine ⟨by with_unfolding_all rfl, ?_, ⟨_, by with_unfolding_all rfl⟩⟩
      have hH0 : (⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
          [(ctrlMain, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[m0V]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0V]⟩)],
          [], [(ctrlMain, [(⟨0⟩, .virtual)]), (ctrlHelper, [])],
          [(ctrlMain, [⟨0⟩])], []⟩ : Memory s0Lang llvmProfile).blockAt s1 = some h0V := by
        simp only [Memory.blockAt, Memory.graphAt, s1, mkRegState, AList.lookup, hMainNeHelper, ↓reduceIte]
        rfl
      refine ⟨h0V, hH0, rfl, callerFrame, [], [retVal], s1, s1, rfl, rfl, .nil _, rfl, ?_⟩
      apply Memory.WriteParams.cons
        (started := mkRegState llvmProfile ctrlMain 0 1 [⟨0⟩] [] [])
        (stored := s2)
      · exact ⟨List.not_mem_nil, Or.inl ⟨rfl, rfl⟩⟩
      · exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
    have step12u : ∀ next, (BIR.Execution (language := s0Core)).step bir s1 next → next = s2 := by
      intro next ⟨_, hst, _⟩
      change (bir.memoryView (language := s0Core)).Step s1 next true at hst
      rw [hView] at hst
      simp only [Memory.Step, s1, mkRegState, Bool.false_eq_true, ↓reduceIte] at hst
      cases hne : next.resourceExhausted with
      | true =>
        rw [ite_eq_left (by simp [hne])] at hst
        obtain ⟨_, _, blk, hblk, hrest⟩ := hst
        have hblkEq : blk = h0V := by
          simp only [Memory.blockAt, Memory.graphAt, AList.lookup, hMainNeHelper, ↓reduceIte] at hblk
          exact Option.some.inj hblk.symm
        subst hblkEq
        obtain ⟨_, c, rest, vs, ar, ended, hcal, hro, hend, _, hpo⟩ := hrest
        obtain ⟨rfl, rfl⟩ := List.cons.inj hcal
        obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hro.symm : (vs, ar) = ([retVal], s1))
        cases hend
        exact (hpo.2.2 trivial).elim
      | false =>
        rw [ite_eq_right (by simp [hne])] at hst
        obtain ⟨blk, hblk, hrest⟩ := hst
        have hblkEq : blk = h0V := by
          simp only [Memory.blockAt, Memory.graphAt, AList.lookup, hMainNeHelper, ↓reduceIte] at hblk
          exact Option.some.inj hblk.symm
        subst hblkEq
        obtain ⟨_, c, rest, vs, ar, ended, hcal, hro, hend, _, hwp⟩ := hrest
        obtain ⟨rfl, rfl⟩ := List.cons.inj hcal
        obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hro.symm : (vs, ar) = ([retVal], s1))
        cases hend
        cases hwp with
        | cons hbeg hw hnil =>
          rename_i started' stored'
          rcases hbeg.2 with ⟨_, hEq⟩ | ⟨hMat, _⟩
          · rw [hEq] at hw
            have hstor' : stored' = s2 := Option.some.inj hw.2.2.1.symm
            exact (writeParams_nil_eq hnil).trans hstor'
          · exact absurd hMat (by intro h; cases h)
    refine ⟨hInit, hInitU, .cons step01 step01u (.cons step12 step12u (.nil s2)), fun o => ?_⟩
    have hr : (BIR.Execution (language := s0Core)).result bir s2 retO := by
      unfold BIR.Execution Semantics.onInput; dsimp only; rw [hView]; with_unfolding_all rfl
    exact ⟨fun h => (Option.some.inj (hr.symm.trans h)).symm, fun h => h ▸ hr⟩)

/-- Control 4: Uncalled vs called helper function elimination and mutation:
1. Eliminating an uncalled helper with `.unreachableFunction` passes `AccountsFor` and `PassPost`;
2. Eliminating a called helper with `.unreachableFunction` fails `AccountsFor` and `PassPost`;
3. Mutating an uncalled helper (`1 → 99`) passes `AccountsFor` and `PassPost`;
4. Mutating a called helper (`1 → 99`) passes `AccountsFor` and `BIR.WF` but fails
   `BehaviorRefinement`, `PassPost`, and `(pass.toStage).Validated`. -/
theorem helper_function_elimination_and_mutation_controls :
    let uncalled1 := birUncalledHelper 1
    let uncalled99 := birUncalledHelper 99
    let mainOnly := birBlock 0
    let elimUncalledAcc : Account :=
      { mapped := [(⟨ctrlMain, ⟨0⟩, .boundary 0⟩, [⟨ctrlMain, ⟨0⟩, .boundary 0⟩])],
        eliminated := [(⟨ctrlHelper, ⟨0⟩, .boundary 0⟩, .unreachableFunction)] }
    let called1 := birCalledHelper 1
    let called99 := birCalledHelper 99
    let elimCalledAcc : Account :=
      { mapped := [(⟨ctrlMain, ⟨0⟩, .instruction 0⟩, [⟨ctrlMain, ⟨0⟩, .instruction 0⟩])],
        eliminated := [(⟨ctrlHelper, ⟨0⟩, .boundary 0⟩, .unreachableFunction)] }
    let mutCalledPass : IRPass s0Lang llvmProfile .bir := fun _ =>
      some (called99, Account.identityFor (point := .bir) called1)
    -- 1. Uncalled helper eliminated: PASSES `AccountsFor` and `PassPost`
    AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      uncalled1 mainOnly elimUncalledAcc ∧
    PassPost (slice := slice0) (profile := llvmProfile) (k := .bir)
      uncalled1 mainOnly elimUncalledAcc ∧
    -- 2. Called helper eliminated: FAILS `AccountsFor` and `PassPost`
    ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      called1 mainOnly elimCalledAcc ∧
    ¬ PassPost (slice := slice0) (profile := llvmProfile) (k := .bir)
      called1 mainOnly elimCalledAcc ∧
    -- 3. Uncalled helper mutated (`1 → 99`): PASSES `AccountsFor` and `PassPost`
    AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      uncalled1 uncalled99 (Account.identityFor (point := .bir) uncalled1) ∧
    PassPost (slice := slice0) (profile := llvmProfile) (k := .bir)
      uncalled1 uncalled99 (Account.identityFor (point := .bir) uncalled1) ∧
    -- 4. Called helper mutated (`1 → 99`): `AccountsFor` and `BIR.WF` pass, yet
    -- `BehaviorRefinement`, `PassPost`, and `Validated` ALL reject it!
    AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      called1 called99 (Account.identityFor (point := .bir) called1) ∧
    called99.WF (language := s0Core) ∧
    ¬ BehaviorRefinement (BIR.Execution (language := s0Core)) (BIR.Execution (language := s0Core))
        (BIR.Execution (language := s0Core)).result (BIR.Execution (language := s0Core)).result
        called1 called99 ∧
    ¬ PassPost (slice := slice0) (profile := llvmProfile) (k := .bir)
      called1 called99 (Account.identityFor (point := .bir) called1) ∧
    ¬ (mutCalledPass.toStage (language := s0Core)).Validated := by
  intro uncalled1 uncalled99 mainOnly elimUncalledAcc called1 called99 elimCalledAcc mutCalledPass
  let p0 := mkRegState llvmProfile ctrlMain 0 0 [] [] []
  have hUncView (v : UInt8) : (birUncalledHelper v).memoryView (language := s0Core) =
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩]⟩),
         (ctrlHelper, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[v]⟩⟩), [origHelper]⟩]⟩)],
        [], [(ctrlMain, []), (ctrlHelper, [])], [], []⟩ := by with_unfolding_all rfl
  have hMainView : mainOnly.memoryView (language := s0Core) = memBlock 0 := by with_unfolding_all rfl
  have hpRes1 : (BIR.Execution (language := s0Core)).result uncalled1 p0 (retObs 42) := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hUncView 1]; with_unfolding_all rfl
  have hpInit1 : (BIR.Execution (language := s0Core)).initial uncalled1 p0 := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hUncView 1]; with_unfolding_all rfl
  have hpTr1 : (BIR.Execution (language := s0Core)).trace uncalled1 p0 = [] := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hUncView 1]; with_unfolding_all rfl
  have hqResMain : (BIR.Execution (language := s0Core)).result mainOnly p0 (retObs 42) := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hMainView]; exact memBlock_hostedResult 0
  have hqInitMain : ∀ s, (BIR.Execution (language := s0Core)).initial mainOnly s → s = p0 := by
    intro s hs; unfold BIR.Execution Semantics.onInput at hs; dsimp only at hs; rw [hMainView] at hs
    exact (memBlock_entryState 0 s).mp hs
  have hqTrMain : (BIR.Execution (language := s0Core)).trace mainOnly p0 = [] := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hMainView]; exact memBlock_trace 0
  have hRefUncalledToMain : BehaviorRefinement (BIR.Execution (language := s0Core)) (BIR.Execution (language := s0Core))
      (BIR.Execution (language := s0Core)).result (BIR.Execution (language := s0Core)).result
      uncalled1 mainOnly :=
    (DetChain.nil p0).refines ⟨p0, hpInit1, .nil p0⟩ hpRes1 hqInitMain
      (fun o => ⟨fun h => (Option.some.inj (hqResMain.symm.trans h)).symm, fun h => h ▸ hqResMain⟩)
      (hpTr1.trans hqTrMain.symm)
  have hqRes99 : (BIR.Execution (language := s0Core)).result uncalled99 p0 (retObs 42) := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hUncView 99]; with_unfolding_all rfl
  have hqInit99 : ∀ s, (BIR.Execution (language := s0Core)).initial uncalled99 s → s = p0 := by
    intro s hs; unfold BIR.Execution Semantics.onInput at hs; dsimp only at hs; rw [hUncView 99] at hs
    with_unfolding_all exact hs
  have hqTr99 : (BIR.Execution (language := s0Core)).trace uncalled99 p0 = [] := by
    unfold BIR.Execution Semantics.onInput; dsimp only; rw [hUncView 99]; with_unfolding_all rfl
  have hRefUncalledMut : BehaviorRefinement (BIR.Execution (language := s0Core)) (BIR.Execution (language := s0Core))
      (BIR.Execution (language := s0Core)).result (BIR.Execution (language := s0Core)).result
      uncalled1 uncalled99 :=
    (DetChain.nil p0).refines ⟨p0, hpInit1, .nil p0⟩ hpRes1 hqInit99
      (fun o => ⟨fun h => (Option.some.inj (hqRes99.symm.trans h)).symm, fun h => h ▸ hqRes99⟩)
      (hpTr1.trans hqTr99.symm)
  have hNoElimCalled : ¬ AccountsFor (slice := slice0) (profile := llvmProfile) (point := .bir)
      called1 mainOnly elimCalledAcc := by decide
  have hNoRefCalledMut : ¬ BehaviorRefinement (BIR.Execution (language := s0Core)) (BIR.Execution (language := s0Core))
      (BIR.Execution (language := s0Core)).result (BIR.Execution (language := s0Core)).result
      called1 called99 := by
    obtain ⟨hInit1, hInitU1, hCh1, hRes1⟩ := birCalledHelper_chain 1 (Or.inl rfl)
    obtain ⟨hInit99, hInitU99, hCh99, hRes99⟩ := birCalledHelper_chain 99 (Or.inr rfl)
    have hObs99 : (BIR.Execution (language := s0Core)).observes called99 (retObs 99) :=
      (hCh99.observes_iff hInit99 hInitU99 hRes99 (retObs 99)).mpr rfl
    have hNe : retObs 99 ≠ retObs 1 := fun h =>
      (by decide : (99 : UInt8) ≠ 1) (Concrete.Contracts.Observation.returned.inj h).1
    exact hCh1.rejects_refinement hInit1 hInitU1 hRes1 hObs99 hNe
  refine ⟨by decide,
    ⟨birBlock_wf 0 (by decide), rfl, rfl, rfl, (fun _ _ h => nomatch h), by decide, hRefUncalledToMain⟩,
    hNoElimCalled, fun h => hNoElimCalled h.2.2.2.2.2.1,
    by decide,
    ⟨birUncalledHelper_wf 99 (by decide), rfl, rfl, rfl, (fun _ _ h => nomatch h), by decide, hRefUncalledMut⟩,
    by decide, birCalledHelper_wf 99 (by decide), hNoRefCalledMut, fun h => hNoRefCalledMut h.2.2.2.2.2.2, ?_⟩
  intro hVal
  have hPost := (IRPass.toStage_validated_iff mutCalledPass).mp hVal called1 called99
    (Account.identityFor (point := .bir) called1) (birCalledHelper_wf 1 (by decide)) rfl
  exact hNoRefCalledMut hPost.2.2.2.2.2.2

end StageInterfaceControls.PassListControls
