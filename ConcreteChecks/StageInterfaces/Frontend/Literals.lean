import Concrete.IntegratedCore
import Concrete.Parse
import ConcreteChecks.StageInterfaces.Fixtures

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

private def literalSourceText (n : Nat) := "fn main() -> u8 { return " ++ toString n ++ "; }"

def literalSource (n : Nat) : Source :=
  ⟨Project.singleton mainSourcePath (SourceText.ofBytes (literalSourceText n).toUTF8.data.toList),
    ⟨0⟩, early⟩

def elaborationSourceText := literalSourceText 42
def elaborationSource := literalSource 42

private def entryNameSpan : Surface0.Span := ⟨3, 7, 1, 4, ⟨0⟩⟩
def resultSpan : Surface0.Span := ⟨25, 27, 1, 26, ⟨0⟩⟩

private def literalProgram (n : Nat) : Surface0.Program :=
  ⟨"main", entryNameSpan, [.ret (.integer n none resultSpan) ⟨18, 28, 1, 19, ⟨0⟩⟩]⟩

abbrev returnProgram := literalProgram 42

def returnSyntax (n : Nat) : SyntaxTree :=
  .node .program [.leaf ⟨⟨⟨0⟩, 3, 7, 1, 4⟩, .name "main"⟩, .node .body [
    .node .return_ [.leaf ⟨⟨⟨0⟩, 18, 24, 1, 19⟩, .fixed .return_ rfl⟩,
      .leaf ⟨⟨⟨0⟩, 25, 27, 1, 26⟩, .integer n .decimal none⟩,
      .leaf ⟨⟨⟨0⟩, 27, 28, 1, 28⟩, .fixed .semi rfl⟩]]]

def parsedReturn : Parsed (SyntaxTree ⊕ Bool) :=
  ⟨elaborationSource, .inl (returnSyntax 42)⟩

abbrev earlyFixtureLanguage := sliceLanguage slice0

private def returnViewCore (n : Nat) := AdmissionView.zero ⟨[], .lit n, ⟨18, 28, 1, 19, ⟨0⟩⟩, resultSpan⟩

def elaboratedReturn (n : Nat) : CoreInput earlyFixtureLanguage :=
  ⟨elaborationSource.project, early, .straightLine (returnViewCore n) rfl⟩

def pendingLiteral (n : Nat) : AdmissionInput earlyFixtureLanguage :=
  .pending elaborationSource.project (.zero (literalProgram n)) rfl

abbrev pendingReturn := pendingLiteral 42

abbrev fixtureElaborate := IntegratedCore.elaborate slice0

theorem representsReturn (n : Nat) : (returnViewCore n).Represents (elaboratedReturn n) :=
  ⟨rfl, rfl, rfl⟩

private theorem typedReturn : (elaboratedReturn 42).StaticallyTyped :=
  ⟨_, representsReturn 42, ⟨by decide, .ret (.int rfl)⟩⟩

set_option maxRecDepth 2048 in
theorem returnedCoreWF (n : Nat) : (elaboratedReturn n).WF := by
  let core := elaboratedReturn n
  let origin : SiteId × Origin := ⟨⟨0⟩, ⟨⟨0⟩, ⟨⟨0⟩, 18, 28, 1, 19⟩,
    ⟨.function earlyEntry, ⟨[⟨.return, .ordinal 0⟩]⟩⟩⟩⟩
  refine ⟨⟨by change [SiteId.mk 0].Nodup; simp, by change [SiteId.mk 0].Nodup; simp,
    by change [origin.2.evidenceKey].Nodup; simp, ?_, ?_, ?_⟩, ?_, ?_⟩
  · intro site op member
    change (site, op) ∈ [(⟨0⟩, .expression (.inl (.read (.literal (.int .u8 n)))))] at member
    cases List.mem_singleton.mp member
    exact ⟨origin.2, rfl⟩
  · intro state events kind site reached trapped
    let start := core.machineProgram.initial
    let next (q : Exec.Config core.machineProgram.machine) :=
      match Exec.step core.machineProgram.body q with
      | some (.next result) => result
      | _ => q
    let returned := next start
    let unscoped := next returned
    let finished := next unscoped
    have peel (current successor : Exec.Config core.machineProgram.machine)
        (dispatch : Exec.step core.machineProgram.body current = some (.next successor))
        (nontrap : current.final? ≠ some (.trapped kind site)) :
        ∀ trace, Exec.Steps core.machineProgram.body current trace state →
          ∃ rest, Exec.Steps core.machineProgram.body successor rest state := by
      intro trace path
      cases path with
      | refl => exact (nontrap trapped).elim
      | step tick rest =>
        cases tick with
        | silent tick =>
          rw [dispatch] at tick
          cases Option.some.inj tick
          exact ⟨_, rest⟩
        | prim tick _ _ => rw [dispatch] at tick; contradiction
    obtain ⟨_, reached⟩ := peel start returned (by with_unfolding_all rfl)
      (by with_unfolding_all exact nofun) _ reached
    obtain ⟨_, reached⟩ := peel returned unscoped (by with_unfolding_all rfl)
      (by with_unfolding_all exact nofun) _ reached
    obtain ⟨_, reached⟩ := peel unscoped finished (by with_unfolding_all rfl)
      (by with_unfolding_all exact nofun) _ reached
    have blocked (current final : Exec.Config core.machineProgram.machine)
        (stopped : Exec.step core.machineProgram.body current = none)
        (nontrap : current.final? ≠ some (.trapped kind site))
        (trace : List core.machineProgram.machine.Event)
        (path : Exec.Steps core.machineProgram.body current trace final) :
        final.final? ≠ some (.trapped kind site) := by
      cases path with
      | refl => exact nontrap
      | step tick _ =>
        cases tick with
        | silent tick => rw [stopped] at tick; contradiction
        | prim tick _ _ => rw [stopped] at tick; contradiction
    exact (blocked finished state (by with_unfolding_all rfl)
      (by with_unfolding_all exact nofun) _ reached trapped).elim
  · intro site found member
    change (site, found) ∈ [origin] at member
    have same := List.mem_singleton.mp member
    obtain rfl := congrArg Prod.fst same
    obtain rfl := congrArg Prod.snd same
    let specSpan : ConcreteSpec.Core.Span := ⟨⟨0⟩, 18, 28, by decide, 1, 19, by decide, by decide⟩
    exact ⟨rfl, specSpan, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩
  · intro site op member
    change (site, op) ∈ [(⟨0⟩, .expression (.inl (.read (.literal (.int .u8 n)))))] at member
    cases List.mem_singleton.mp member
    trivial
  · intro location member
    change location ∈ [resultSpan] at member
    cases List.mem_singleton.mp member
    exact ⟨⟨⟨0⟩, 25, 27, by decide, 1, 26, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

example :
    (Concrete.Lex.lex elaborationSourceText.toUTF8).bind
      (Concrete.Parse.parseForDriver (Concrete.Parse.eofSpan elaborationSourceText.toUTF8)) = .ok returnProgram :=
  rfl

def returnView (n : Nat := 42) : ParserView :=
  let stop := 25 + (toString n).length
  let tok := fun start stop value => (⟨⟨⟨0⟩, start, stop, 1, start + 1⟩, value⟩ : SurfaceToken)
  ⟨literalSource n, #[
    tok 0 2 (.fixed .fn rfl), tok 3 7 (.name "main"), tok 7 8 (.fixed .lparen rfl),
    tok 8 9 (.fixed .rparen rfl), tok 10 12 (.fixed .arrow rfl), tok 13 15 (.fixed .u8 rfl),
    tok 16 17 (.fixed .lbrace rfl), tok 18 24 (.fixed .return_ rfl), tok 25 stop (.integer n .decimal none),
    tok stop (stop + 1) (.fixed .semi rfl), tok (stop + 2) (stop + 3) (.fixed .rbrace rfl)]⟩

def returnStream (n : Nat := 42) : TokenStream :=
  let view := returnView n
  let bytes := (literalSourceText n).toUTF8
  let tokens := view.tokens.map fun decoded =>
    (⟨decoded, bytes.extract decoded.span.startOffset decoded.span.endOffset⟩ : RetainedToken)
  let trivia := view.tokens.toList.zipIdx.map fun (token, i) =>
    let stop := token.span.startOffset
    let start := if i = 0 then 0 else
      ((view.tokens[i - 1]?).map (fun previous => previous.span.endOffset)).getD 0
    if start = stop then #[] else
      #[⟨⟨⟨0⟩, start, stop, 1, start + 1⟩, bytes.extract start stop⟩]
  ⟨view.source, tokens, trivia.toArray.push #[]⟩

end StageInterfaceControls
