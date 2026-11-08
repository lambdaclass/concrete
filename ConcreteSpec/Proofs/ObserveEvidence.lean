import ConcreteSpec.Proofs.Observe2

/-! # Mutation evidence for stuttering and byte-column arithmetic -/
namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core
namespace Evidence

abbrev signature := Demo.signature
def event : Event signature := .output "kept"
def before : List (Step signature) := [.emit event]
def after : List (Step signature) := [.silent, .emit event]

/-- Helper (computation). Closed witness of a genuine inserted step, used by the finite law
instances; not an independent contract law. (approved: PENDING) -/
theorem insertion : SilentInsertion before after := .silent (.emit event .nil)

/-- Fault: observes only the first internal step, losing later visible output. -/
def firstStepVisible {S : EventSignature} (steps : List (Step S)) : List (Event S) :=
  (steps.head?.bind Step.event?).toList

/-- Instance (law `ConcreteSpec.Core.Observe.silent_internal_steps`, case `leading_silence`). -/
example : SameObservation (.finite before (.exited 0) 7) (.finite after (.exited 0) 7) :=
  silent_internal_steps insertion (.exited 0) 7

/-- Planted (law `ConcreteSpec.Core.Observe.silent_internal_steps`, case `leading_silence`,
subject `ConcreteSpec.Core.Observe.visible`, mutant `ConcreteSpec.Core.Observe.Evidence.firstStepVisible`). -/
example : SilentInsertion before after ∧
    ¬ (firstStepVisible before = firstStepVisible after ∧ Terminal.exited 0 = .exited 0 ∧
      (7 : Int) = 7) := by
  refine ⟨insertion, ?_⟩
  intro invalid
  have equal := invalid.1
  change [event] = [] at equal
  cases equal

/-- Fault: inspects only the first step of a finite execution when projecting its observation,
so inserting a leading silent step loses the terminal observation even though `SameObservation`
holds. -/
def firstStepHasFullObservation {S : EventSignature} : Execution S → FullObservation S → Prop
  | .finite steps terminal state, .terminal observation =>
      observation = ⟨firstStepVisible steps, terminal, state⟩
  | execution, observation => HasFullObservation execution observation

/-- Instance (law `ConcreteSpec.Core.Observe.full_observation_preservation`, case `stuttered_terminal_state`). -/
example : HasFullObservation (.finite after (.exited 0) 7) (.terminal ⟨[event], .exited 0, 7⟩) :=
  full_observation_preservation (x := .finite before (.exited 0) 7)
    (y := .finite after (.exited 0) 7) (observation := .terminal ⟨[event], .exited 0, 7⟩)
    (silent_internal_steps insertion (.exited 0) 7) rfl

/-- Planted (law `ConcreteSpec.Core.Observe.full_observation_preservation`, case `stuttered_terminal_state`,
subject `ConcreteSpec.Core.Observe.HasFullObservation`, mutant `ConcreteSpec.Core.Observe.Evidence.firstStepHasFullObservation`). -/
example : SameObservation (.finite before (.exited 0) 7) (.finite after (.exited 0) 7) ∧
    firstStepHasFullObservation (.finite before (.exited 0) 7) (.terminal ⟨[event], .exited 0, 7⟩) ∧
    ¬ firstStepHasFullObservation (.finite after (.exited 0) 7) (.terminal ⟨[event], .exited 0, 7⟩) := by
  refine ⟨silent_internal_steps insertion (.exited 0) 7, rfl, ?_⟩
  intro invalid
  have eventsEqual := congrArg Observation.events invalid
  change [event] = [] at eventsEqual
  cases eventsEqual

/-- Instance (law `ConcreteSpec.Core.Observe.blocked_silent_internal_steps`, case `pending_silence`). -/
example : SameObservation (.blocked before ⟨Demo.Op.double, 3⟩)
    (.blocked after ⟨Demo.Op.double, 3⟩) :=
  blocked_silent_internal_steps insertion ⟨Demo.Op.double, 3⟩

/-- Planted (law `ConcreteSpec.Core.Observe.blocked_silent_internal_steps`, case `pending_silence`,
subject `ConcreteSpec.Core.Observe.visible`, mutant `ConcreteSpec.Core.Observe.Evidence.firstStepVisible`). -/
example : SilentInsertion before after ∧
    ¬ (firstStepVisible before = firstStepVisible after ∧
      (⟨Demo.Op.double, 3⟩ : Request signature) = ⟨Demo.Op.double, 3⟩) := by
  refine ⟨insertion, ?_⟩
  intro invalid
  have equal := invalid.1
  change [event] = [] at equal
  cases equal

def emitting : Nat → Step signature := fun _ => .emit event
def delayed : Nat → Step signature
  | 0 => .silent
  | _ + 1 => .emit event

/-- Helper (computation). Closed step-index witness for the infinite law instance, including
both retained emissions and an unretained silent step. (approved: PENDING) -/
theorem shifted : InfiniteStuttering emitting (fun n => n + 1) delayed := by
  refine ⟨?_, ?_, ?_⟩
  · intro n
    change n + 1 < n + 1 + 1
    omega
  · intro n
    rfl
  · intro m absent
    cases m with
    | zero => rfl
    | succ n => exact False.elim (absent n rfl)

/-- Fault: repeats the first step's event and never inspects the requested prefix. -/
def firstPrefix {S : EventSignature} (steps : Nat → Step S) (_n : Nat) : List (Event S) :=
  (steps 0).event?.toList

/-- Instance (law `ConcreteSpec.Core.Observe.infinite_stuttering`, case `delayed_emissions`). -/
example : SameInfiniteObservation emitting delayed :=
  infinite_stuttering shifted

/-- Planted (law `ConcreteSpec.Core.Observe.infinite_stuttering`, case `delayed_emissions`,
subject `ConcreteSpec.Core.Observe.visiblePrefix`, mutant `ConcreteSpec.Core.Observe.Evidence.firstPrefix`). -/
example : InfiniteStuttering emitting (fun n => n + 1) delayed ∧
    ¬ ((∀ n, ∃ m rest, firstPrefix delayed m = firstPrefix emitting n ++ rest) ∧
      (∀ n, ∃ m rest, firstPrefix emitting m = firstPrefix delayed n ++ rest)) := by
  refine ⟨shifted, ?_⟩
  intro invalid
  obtain ⟨m, rest, equal⟩ := invalid.1 1
  change [] = event :: rest at equal
  cases equal

/-- Fault: counts a tab twice while retaining the UTF-8 sizes of other characters. -/
def doubleTabColumn (text : String) : Nat := byteColumn text + text.toList.count '\t'

/-- Instance (law `ConcreteSpec.Core.Observe.byteColumn_tab`, case `utf8_then_tab`). -/
example : byteColumn ("é" ++ "\t") = byteColumn "é" + 1 := byteColumn_tab "é"

/-- Planted (law `ConcreteSpec.Core.Observe.byteColumn_tab`, case `utf8_then_tab`,
subject `ConcreteSpec.Core.Observe.byteColumn`, mutant `ConcreteSpec.Core.Observe.Evidence.doubleTabColumn`). -/
example : ¬ (doubleTabColumn ("é" ++ "\t") = doubleTabColumn "é" + 1) := by decide

def punctuatedSpan : Span := ⟨"src/a:b (site 9)/main.con", 3, 9, by decide, by decide⟩
def shiftedColSpan : Span := ⟨"src/a:b (site 9)/main.con", 3, 10, by decide, by decide⟩

/-- Fault: omits the typed site identity when rendering a trap line, so distinct trap sites at
the same source position collide. -/
def trapLineWithoutSite (kind : TrapKind) (_site : SiteId) (sp : Span) : String :=
  String.ofList ("concrete: trap: ".toList ++ kind.name.toList ++ " at ".toList ++
    sp.path.toList ++ ":".toList ++ (toString sp.line).toList ++ ":".toList ++
    (toString sp.col).toList)

/-- Fault: omits the byte column when rendering a trap line, so distinct byte columns on the
same line collide. -/
def trapLineWithoutCol (kind : TrapKind) (site : SiteId) (sp : Span) : String :=
  String.ofList ("concrete: trap: ".toList ++ kind.name.toList ++ " at ".toList ++
    sp.path.toList ++ ":".toList ++ (toString sp.line).toList ++
    " (site ".toList ++ (toString site.id).toList ++ ")".toList)

/-- Instance (law `ConcreteSpec.Core.Observe.trapLine_injective`, case `distinct_sites_punctuated_path`). -/
example : trapLine .overflow ⟨7⟩ punctuatedSpan = trapLine .overflow ⟨8⟩ punctuatedSpan ↔
    TrapKind.overflow = .overflow ∧ (⟨7⟩ : SiteId) = ⟨8⟩ ∧
      punctuatedSpan = punctuatedSpan :=
  trapLine_injective .overflow .overflow ⟨7⟩ ⟨8⟩ punctuatedSpan punctuatedSpan

/-- Planted (law `ConcreteSpec.Core.Observe.trapLine_injective`, case `distinct_sites_punctuated_path`,
subject `ConcreteSpec.Core.Observe.trapLine`, mutant `ConcreteSpec.Core.Observe.Evidence.trapLineWithoutSite`). -/
example : ¬ (trapLineWithoutSite .overflow ⟨7⟩ punctuatedSpan =
      trapLineWithoutSite .overflow ⟨8⟩ punctuatedSpan ↔
    TrapKind.overflow = .overflow ∧ (⟨7⟩ : SiteId) = ⟨8⟩ ∧
      punctuatedSpan = punctuatedSpan) := by decide

/-- Instance (law `ConcreteSpec.Core.Observe.trapLine_injective`, case `distinct_columns_punctuated_path`). -/
example : trapLine .overflow ⟨7⟩ punctuatedSpan = trapLine .overflow ⟨7⟩ shiftedColSpan ↔
    TrapKind.overflow = .overflow ∧ (⟨7⟩ : SiteId) = ⟨7⟩ ∧
      punctuatedSpan = shiftedColSpan :=
  trapLine_injective .overflow .overflow ⟨7⟩ ⟨7⟩ punctuatedSpan shiftedColSpan

/-- Planted (law `ConcreteSpec.Core.Observe.trapLine_injective`, case `distinct_columns_punctuated_path`,
subject `ConcreteSpec.Core.Observe.trapLine`, mutant `ConcreteSpec.Core.Observe.Evidence.trapLineWithoutCol`). -/
example : ¬ (trapLineWithoutCol .overflow ⟨7⟩ punctuatedSpan =
      trapLineWithoutCol .overflow ⟨7⟩ shiftedColSpan ↔
    TrapKind.overflow = .overflow ∧ (⟨7⟩ : SiteId) = ⟨7⟩ ∧
      punctuatedSpan = shiftedColSpan) := by decide

end Evidence
end ConcreteSpec.Core.Observe
