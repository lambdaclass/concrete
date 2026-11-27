import Concrete.Driver.Launcher
import Concrete.Driver.Project
import Concrete.Driver.Reference
import Concrete.Stages
import ConcreteSpec.Spec.Examples
import ConcreteSpec.Spec.Rules
import ConcreteSpecEvidence.ExecExamples
import ConcreteSpecEvidence.Rules

open ConcreteSpec.Core (PremiseId)
open ConcreteSpec.Project
open Concrete.Driver
open ConcreteSpec.Core.ProjectRegistry (facts supported package function)

private def expect (label : String) (condition : Bool) : IO Unit :=
  unless condition do throw (IO.userError s!"slice 1 driver control failed: {label}")

private def premise : PremiseId := ⟨.stack, .package package ⟨⟨"stack"⟩⟩⟩

private def requireRefusal (clause : Clause) (bad : FunctionFacts) (source : EvidenceOrigin)
    (reason : Violation) : IO Unit :=
  match checkClause ⟨clause, []⟩ bad with
  | .ok () => expect "planted policy violation" false
  | .error detail => do
    expect "policy clause" (decide (detail.clause = clause))
    expect "policy function" (decide (detail.function = bad.name))
    expect "policy source" (decide (detail.source = source))
    expect "policy reason" (decide (detail.violation = reason))

#eval expect "pending admission has no specification rule"
  (!ConcreteSpec.Core.registeredRules.any fun rule =>
    rule.name.toString == "ConcreteSpec.Project.ManifestAdmitted")

#eval show IO Unit from do
  let requests := predictable [] ++ [⟨.terminates, []⟩, ⟨.stackBound, []⟩, ⟨.maxStackBytes 32, []⟩]
  expect "all clauses accept established facts" ((Project.checkScope [] (.resolved requests [facts] [])).isOk)
  requireRefusal .noAlloc {facts with allocates := .established true [] (.site ⟨0⟩)} (.site ⟨0⟩) .contradicted
  requireRefusal .noAlloc {facts with allocates := .unknown (.site ⟨0⟩)} (.site ⟨0⟩) .unknown
  requireRefusal .noBlocking {facts with blocks := .established true [] (.site ⟨1⟩)} (.site ⟨1⟩) .contradicted
  requireRefusal .noForeignCalls {facts with callsForeign := .established true [] (.site ⟨2⟩)} (.site ⟨2⟩) .contradicted
  requireRefusal .terminates {facts with terminates := .established false [] (.site ⟨3⟩)} (.site ⟨3⟩) .contradicted
  requireRefusal .stackBound {facts with stackBytes := .unknown (.site ⟨4⟩)} (.site ⟨4⟩) .unknown
  requireRefusal (.maxStackBytes 31) facts .function .contradicted
  let conditional := {facts with stackBytes := .established 32 [premise] (.site ⟨4⟩)}
  requireRefusal .stackBound conditional (.site ⟨4⟩) (.unacceptedAssumption premise)
  expect "named premise accepted" ((checkClause ⟨.stackBound, [premise]⟩ conditional).isOk)
  let foreignPremise : PremiseId := ⟨.stack, .package
    { package with name := ⟨"dependency"⟩ } ⟨⟨"stack"⟩⟩⟩
  expect "same key in another package cannot discharge a premise"
    (!(checkClause ⟨.stackBound, [foreignPremise]⟩ conditional).isOk)
  let dep := Project.Scope.resolved [⟨.stackBound, [premise]⟩] [conditional] []
  expect "dependency cannot relax inherited requirement"
    (!(Project.checkScope [] (.resolved [⟨.stackBound, []⟩] [] [.resolved [] [] [dep]])).isOk)
  expect "unresolved dependency refused"
    (!(Project.checkScope requests (.resolved [] [] [.unresolved package])).isOk)
  expect "duplicate additions are recorded once"
    (decide (assumptionDiff [] [premise, premise] = ⟨[premise], []⟩))
  expect "reordering assumptions leaves an empty diff"
    (decide (assumptionDiff [premise, foreignPremise] [foreignPremise, premise, premise] = ⟨[], []⟩))
  expect "assumption removal is visible"
    (decide (assumptionDiff [premise] [] = ⟨[], [premise]⟩))
  let tables : Tables := ⟨some ⟨1, requests⟩, some ⟨2, [premise]⟩⟩
  expect "table revisions supported" ((tables.checkVersions 1 2).isOk)
  expect "unsupported policy revision"
    (decide (tables.checkVersions 2 2 = .error ⟨.policy, 1, 2⟩))
  expect "unsupported assumption revision"
    (decide (tables.checkVersions 1 1 = .error ⟨.assumptions, 2, 1⟩))
  expect "standalone pin selection" (decide (selectLanguage supported .standalone = .ok supported))
  expect "missing project pin" (decide (selectLanguage supported (.project none) = .error .missing))
  let other : LanguageRulesVersion := ⟨2, 0⟩
  expect "unsupported project pin"
    (decide (selectLanguage supported (.project (some other)) = .error (.unsupported other supported)))

#eval show IO Unit from do
  let source := "fn main() -> u8 { return 42; }"
  let .ok input := Concrete.Stages.Input.load "answer.con" source.toUTF8
    | throw (IO.userError "fixture source did not load")
  let .ok out := Concrete.Stages.compile input
    | throw (IO.userError "real source did not compile")
  let actual := Project.mirFacts function out.mir
  let .ok [coverage] := Project.checkScope [] (.resolved (predictable []) [actual] [])
    | throw (IO.userError "compiled function did not satisfy predictable")
  expect "coverage names the function and all checked requirements"
    (decide (coverage.function = function ∧ coverage.requirements = predictable []))
  requireRefusal .stackBound actual .function .unknown

-- A missing token binds EOF. A wrong token binds its own byte range,
-- including after multibyte text or before a later unterminated quote.
example : Project.parseManifest "\"é\" = [" = .error (.invalid (.malformed ⟨8, 8⟩)) ∧
    Project.parseManifest "x = [0 1]" = .error (.invalid (.malformed ⟨7, 8⟩)) ∧
    Project.parseManifest "]\nx = \"open" = .error (.invalid (.malformed ⟨0, 1⟩)) ∧
    Project.manifestText ⟨#[120, 61, 0xc2]⟩ = .error (.encoding ⟨3, 3⟩) := by
  cbv <;> decide

private def manifestRule (text : String) :=
  match Project.parseManifest text with
  | .error e => e.ruleId
  | .ok () => none

#eval show IO Unit from do
  let valid := Project.parseManifest "[policy]\n[assumptions]\n"
  expect "valid TOML table syntax" valid.isOk
  for text in ["[policy\n", "[policy]\nx = true\nx = false\n"] do
    match Project.parseManifest text with
    | .error (.invalid (.malformed _)) => pure ()
    | _ => expect "malformed or duplicate assignment" false
  let quotes := String.ofList (List.replicate 40 '[')
  let quoted := Project.parseManifest s!"x = \"{quotes}\" # {quotes}\n"
  expect "brackets in quoted values and comments" quoted.isOk
  let arrayAtDepth := fun depth =>
    "x = " ++ String.ofList (List.replicate depth '[') ++
      "true" ++ String.ofList (List.replicate depth ']')
  let limit := Project.parseManifest (arrayAtDepth (Project.manifestDepthLimit - 1))
  expect "array at depth limit" limit.isOk
  match Project.parseManifest (arrayAtDepth Project.manifestDepthLimit) with
  | .error (.invalid (.depth _)) => pure ()
  | _ => expect "excess nesting" false
  let segments := fun n => String.intercalate "." (List.replicate n "a")
  let nested := fun n =>
    "[" ++ segments 8 ++ "]\n" ++ segments n ++ " = true\n"
  expect "combined header and dotted path at limit" (Project.parseManifest (nested 8)).isOk
  expect "combined header and dotted path exceeds limit"
    (decide (manifestRule (nested 9) = some Manifest.depthRule))
  let inlineAt := fun n => "[" ++ segments 8 ++ "]\nx = {" ++ segments n ++ " = true}\n"
  expect "combined inline table path at limit" (Project.parseManifest (inlineAt 6)).isOk
  expect "depth includes inline table keys"
    (decide (manifestRule (inlineAt 7) = some Manifest.depthRule))
  for (table, cost) in [("{}", 1), ("{ y = true }", 2)] do
    for extra in [0, 1] do
      let n := Project.manifestDepthLimit - cost + extra
      let arrays := n - 1
      for text in [segments n ++ " = " ++ table,
          "[" ++ segments (n - 1) ++ "]\nx = " ++ table,
          "x = " ++ String.ofList (List.replicate arrays '[') ++ table ++
            String.ofList (List.replicate arrays ']')] do
        expect ("inline table at depth boundary: " ++ text)
          (if extra == 0 then (Project.parseManifest text).isOk else manifestRule text == some Manifest.depthRule)
  for text in ["a.b = 1\n[a]\nc = 2\n", "[a]\nb.c = 1\n[a.b]\nd = 2\n",
      "[a.b]\nx = 1\n[a]\nb.y = 2\n", "x = 00", "x = -01", "# " ++ String.singleton (Char.ofNat 0),
      "# " ++ String.singleton (Char.ofNat 1), "x = \"\"\"a\\\n \r b\"\"\""] do
    expect ("malformed manifest: " ++ reprStr text)
      (decide (manifestRule text = some Manifest.syntaxRule))
  for text in ["[a.b]\nx = 1\n[a]\nc = 2\n", "a.b = 1\na.c = 2\n[a.d]\ne = 3\n",
      "x = 0\ny = -10\n#\t comment", "x = \"\"\"a\\\r\n \t\r\n b\"\"\""] do
    expect ("valid manifest: " ++ reprStr text) (Project.parseManifest text).isOk
  expect "quoted dots are one segment"
    (Project.parseManifest ("\"" ++ segments 20 ++ "\".a = true\n")).isOk
  expect "duplicate decoded key"
    (decide (manifestRule "a = true\n\"\\u0061\" = false\n" = some Manifest.syntaxRule))
  expect "single-line string does not consume following lines"
    (decide (manifestRule ("a = \"unterminated\n" ++ arrayAtDepth 40) = some Manifest.syntaxRule))
  expect "multiline string hides delimiters and consumes a closing run"
    (Project.parseManifest ("a = \"\"\"\n" ++ quotes ++ "\"\"\"\"\"\nb = true\n")).isOk
  match Project.parseManifest (String.ofList (List.replicate (Project.manifestByteLimit + 1) ' ')) with
  | .error .size => pure ()
  | _ => expect "oversize manifest" false
  expect "invalid UTF-8" (decide (Project.manifestText ⟨#[120, 61, 255]⟩ = .error (.encoding ⟨2, 3⟩)))

#eval show IO Unit from do
  let some component := Reference.Component.ofChecked ConcreteSpec.Examples.moveThenConsume
    | throw (IO.userError "reference component refused")
  let launcher : Reference.Launcher := ⟨component⟩
  let observed := (launcher.run 10).observation.map ConcreteSpec.Observe.Observation.status
  expect "reference observation" (observed == some 42)
  expect "planted wrong expected exit status" (observed != some 41)
  expect "fuel exhaustion remains explicit" (decide ((launcher.run 0).outcome = .outOfFuel))
  expect "use after move component refused"
    ((Reference.Component.ofChecked ConcreteSpec.Examples.useAfterMove).isNone)

open ConcreteSpec.Core.Exec in
#eval show IO Unit from do
  let config := Demo.start ⟨[], .int, ⟨0⟩, Demo.callLog 42⟩ [] .init
  let .executed report := Reference.replay Demo.prog (some config) Demo.env 30
    | throw (IO.userError "configured reference launcher refused")
  expect "reference replay retains events" (report.events == [42])
  let .executed silent := Reference.replay Demo.prog (some config) (fun _ _ _ _ _ => none) 30
    | throw (IO.userError "configured reference launcher refused")
  expect "silent oracle is distinguished from a successful run"
    (match silent.ending with | .noAnswer => true | _ => false)
  expect "absent startup configuration refuses before evaluation"
    (match Reference.replay Demo.prog none Demo.env 30 with
     | .startupRefused => true
     | _ => false)

#eval show IO Unit from do
  let program := ConcreteSpec.Authority.prog
  let permitted : ConcreteSpec.Authority.ProgOk ConcreteSpec.Authority.prog := by decide
  let .ok trace := Reference.replayAuthority program permitted [(.stdout, 99)] 10
    | throw (IO.userError "configured authority launch refused")
  expect "authority trace retains both writes on the configured grant"
    (decide (trace.events = [⟨.writeStdout, 99⟩, ⟨.writeStdout, 99⟩] ∧ trace.complete = true))
  for grants in [[], [(.stdin, 99)]] do
    let .error observation := Reference.replayAuthority program permitted grants 10
      | throw (IO.userError "missing output grant was accepted")
    expect "missing or mismatched launch grant reports status 125" (observation.status == 125)

#eval IO.FS.withTempDir fun dir => do
  let path := dir / "Concrete.toml"
  expect "missing directory entry permits standalone" ((← Project.checkInvocation path).isOk)
  IO.FS.writeFile path "[policy]\n[assumptions]\n"
  match ← Project.checkInvocation path with
  | .error .schemaPending => pure ()
  | _ => expect "present table awaits approved schema" false
  IO.FS.writeFile path "[policy\n"
  match ← Project.checkInvocation path with
  | .error (.invalid (.malformed _)) => pure ()
  | _ => expect "malformed project never falls back" false
  IO.FS.removeFile path
  IO.FS.createDir path
  match ← Project.checkInvocation path with
  | .error (.read _) => pure ()
  | _ => expect "non-file project entry refused" false

def main (args : List String) : IO Unit := do
  let [dir] := args | throw (IO.userError "expected a scratch directory")
  let dir := System.FilePath.mk dir
  let some good := Launcher.prepareLLVM Concrete.BIR.Examples.answerDischarged
    | throw (IO.userError "answer BIR refused")
  let some trapping := Launcher.prepareLLVM Concrete.BIR.Examples.overflow
    | throw (IO.userError "trap control BIR refused")
  let invalid := { Concrete.BIR.Examples.answerDischarged with blocks := [] }
  expect "malformed BIR cannot prepare a launcher" (Launcher.prepareLLVM invalid).isNone
  expect "prepared launcher and component match" (good.matches good.component good.launcher)
  expect "planted launcher substitution" (!good.matches good.component trapping.launcher)
  expect "planted component substitution" (!good.matches trapping.component good.launcher)
  for (name, inputs) in [("answer", good), ("trap", trapping)] do
    let dest := dir / name
    IO.FS.createDirAll dest
    IO.FS.writeFile (dest / "component.ll") inputs.component
    IO.FS.writeFile (dest / "launcher.ll") inputs.launcher
