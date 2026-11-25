import Lake.Toml.Load
import Concrete.Driver.Launcher
import Concrete.Driver.Project
import Concrete.Driver.Reference
import ConcreteSpec.Spec.Examples
import ConcreteSpec.Spec.Rules

open ConcreteSpec.Project
open Concrete.Driver
open ConcreteSpec.Core.ProjectRegistry (facts supported)

private def expect (label : String) (condition : Bool) : IO Unit :=
  unless condition do throw (IO.userError s!"slice 1 driver control failed: {label}")

private def parseToml (text : String) : IO (Option Lake.Toml.Table) := do
  return (← (Lake.Toml.loadToml (Lean.Parser.mkInputContext text "Concrete.toml")).toBaseIO).toOption

private def premise : AssumptionId := ⟨.stack, "test stack premise"⟩

private def requireRefusal (clause : Clause) (bad : FunctionFacts) (source : String)
    (reason : Violation) : IO Unit :=
  match checkClause ⟨clause, []⟩ bad with
  | .ok () => expect "planted policy violation" false
  | .error detail => do
    expect "policy clause" (decide (detail.clause = clause))
    expect "policy function" (detail.function == bad.name)
    expect "policy source" (detail.source == source)
    expect "policy reason" (decide (detail.violation = reason))

#eval show IO Unit from do
  let requests := predictable [] ++ [⟨.terminates, []⟩, ⟨.stackBound, []⟩, ⟨.maxStackBytes 32, []⟩]
  expect "all clauses accept established facts" ((Project.checkScope [] (.resolved requests [facts] [])).isOk)
  requireRefusal .noAlloc {facts with allocates := .established true [] "alloc"} "alloc" .contradicted
  requireRefusal .noAlloc {facts with allocates := .unknown "alloc"} "alloc" .unknown
  requireRefusal .noBlocking {facts with blocks := .established true [] "block"} "block" .contradicted
  requireRefusal .noForeignCalls {facts with callsForeign := .established true [] "ffi"} "ffi" .contradicted
  requireRefusal .terminates {facts with terminates := .established false [] "term"} "term" .contradicted
  requireRefusal .stackBound {facts with stackBytes := .unknown "stack"} "stack" .unknown
  requireRefusal (.maxStackBytes 31) facts "frame analysis" .contradicted
  let conditional := {facts with stackBytes := .established 32 [premise] "stack"}
  requireRefusal .stackBound conditional "stack" (.unacceptedAssumption premise)
  expect "named premise accepted" ((checkClause ⟨.stackBound, [premise]⟩ conditional).isOk)
  let dep := Project.Scope.resolved [⟨.stackBound, [premise]⟩] [conditional] []
  expect "dependency cannot relax inherited requirement"
    (!(Project.checkScope [] (.resolved [⟨.stackBound, []⟩] [] [.resolved [] [] [dep]])).isOk)
  expect "unresolved dependency refused"
    (!(Project.checkScope requests (.resolved [] [] [.unresolved "dependency"])).isOk)
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
  let valid ← Project.parseManifest parseToml "[policy]\n[assumptions]\n"
  expect "valid TOML table syntax" valid.isOk
  for text in ["[policy\n", "[policy]\nx = true\nx = false\n"] do
    match ← Project.parseManifest parseToml text with
    | .error .malformed => pure ()
    | _ => expect "malformed or duplicate assignment" false
  let quotes := String.ofList (List.replicate 40 '[')
  let quoted ← Project.parseManifest parseToml s!"x = \"{quotes}\" # {quotes}\n"
  expect "brackets in quoted values and comments" quoted.isOk
  let arrayAtDepth := fun depth =>
    "x = " ++ String.ofList (List.replicate depth '[') ++
      "true" ++ String.ofList (List.replicate depth ']')
  let limit ← Project.parseManifest parseToml (arrayAtDepth Project.manifestDepthLimit)
  expect "array at depth limit" limit.isOk
  match ← Project.parseManifest parseToml (arrayAtDepth (Project.manifestDepthLimit + 1)) with
  | .error .depth => pure ()
  | _ => expect "excess nesting" false
  match ← Project.parseManifest parseToml (String.ofList (List.replicate (Project.manifestByteLimit + 1) ' ')) with
  | .error .size => pure ()
  | _ => expect "oversize manifest" false
  expect "invalid UTF-8" (decide (Project.manifestText ⟨#[255]⟩ = .error .encoding))

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

#eval IO.FS.withTempDir fun dir => do
  let path := dir / "Concrete.toml"
  expect "missing directory entry permits standalone" ((← Project.checkInvocation parseToml path).isOk)
  IO.FS.writeFile path "[policy]\n[assumptions]\n"
  match ← Project.checkInvocation parseToml path with
  | .error .schemaPending => pure ()
  | _ => expect "present table awaits approved schema" false
  IO.FS.writeFile path "[policy\n"
  match ← Project.checkInvocation parseToml path with
  | .error .malformed => pure ()
  | _ => expect "malformed project never falls back" false
  IO.FS.removeFile path
  IO.FS.createDir path
  match ← Project.checkInvocation parseToml path with
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
