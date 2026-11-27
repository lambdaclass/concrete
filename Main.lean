import Concrete.Stages
import Concrete.Driver.Project
import Concrete.Driver.Launcher

open Concrete Concrete.Backend

private def usage : String :=
  "usage: concrete build SOURCE.con -o OUTPUT-DIRECTORY [--backend llvm|lcb] | concrete help"

private def toolIO {α : Type} (operation : String) (action : IO α) : ExceptT Refusal IO α :=
  ExceptT.mk do
    try
      return .ok (← action)
    catch e =>
      return .error (.toolFailure s!"{operation}: {e}")

private def writeOutput (name : String) (getStream : BaseIO IO.FS.Stream) (text : String) :
    ExceptT Refusal IO Unit :=
  toolIO s!"write {name}" do
    let stream ← getStream
    stream.putStr text
    stream.flush

private def writeBuildInput (lib path : System.FilePath) (text : String) : ExceptT Refusal IO Unit :=
  toolIO s!"write {path}" do
    let result ← IO.Process.output { cmd := "python3", args := #[(lib / "slice0_write.py").toString, path.toString] } (some text)
    if result.exitCode != 0 then
      throw (IO.userError s!"checked writer exited with status {result.exitCode}: {result.stderr.trimAscii}")

private def outputTransaction (lib : System.FilePath) (operation : String) (args : Array String) :
    ExceptT Refusal IO String :=
  toolIO operation do
    let result ← IO.Process.output {
      cmd := "python3", args := #["-B", (lib / "slice0_publish.py").toString] ++ args }
    if result.exitCode != 0 then
      throw (IO.userError result.stderr.trimAscii.toString)
    return result.stdout

private def buildNative (lib dir : System.FilePath) (backend : Option String)
    (lcb : String) (inputs : Array (String × String)) (artifacts : Array String)
    (version : ConcreteSpec.Project.LanguageRulesVersion) : ExceptT Refusal IO Unit := do
  let names := inputs.map Prod.fst ++ artifacts ++ #["runtime-premises.txt"]
  let path ← outputTransaction lib "prepare build outputs" (#["prepare", dir.toString] ++ names)
  let staging := System.FilePath.mk (path.dropEnd 1 |>.toString)
  try
    for (name, text) in inputs do
      writeBuildInput lib (staging / name) text
    let helper := (lib / "slice0_native.sh").toString
    let args := #[staging.toString, lcb] ++ backend.toArray
    let child ← toolIO "run native build helper"
      (IO.Process.output { cmd := "bash", args := #[helper, "bound"] ++ args })
    if child.exitCode != 0 then
      let detail := (child.stdout ++ child.stderr).trimAscii.toString
      throw (.toolFailure s!"native build helper exited with status {child.exitCode}{if detail.isEmpty then "" else s!": {detail}"}")
    if !child.stdout.isEmpty then writeOutput "stdout" IO.getStdout child.stdout
    if !child.stderr.isEmpty then writeOutput "stderr" IO.getStderr child.stderr
    for (name, text) in inputs do
      let retained ← toolIO s!"read staged input {name}" (IO.FS.readBinFile (staging / name))
      unless text.toUTF8 == retained do
        throw (.toolFailure s!"staged link input changed during native build: {name}")
    let verified ← toolIO "verify native build"
      (IO.Process.output { cmd := "bash", args := #[helper, "verify"] ++ args })
    if verified.exitCode != 0 then
      let detail := (verified.stdout ++ verified.stderr).trimAscii.toString
      throw (.toolFailure s!"native replay exited with status {verified.exitCode}: {detail}")
    -- Finish fallible reporting before committing the verified generation.
    writeOutput "stdout" IO.getStdout s!"language-rules version {version.major}.{version.minor}\n"
    discard <| outputTransaction lib "publish build outputs"
      (#["publish", dir.toString, staging.toString] ++ names)
  finally
    -- The publisher moves a private candidate out of staging on first publication.
    -- On exchange the retired output remains in staging for cleanup.
    let cleanup : IO Unit := do
      try IO.FS.removeDirAll staging catch _ => pure ()
    cleanup

private def report (refusal : Refusal) : IO UInt32 := do
  let message := if refusal.code == "SYNTAX" then s!"{refusal.code.toLower} error: {refusal.message}" else refusal.message
  let message := match refusal with
    | .source cause => s!"{message} [rule {cause.rule.name}@{cause.rule.rev}, file {cause.primary.file.index}, bytes {cause.primary.startOffset}..{cause.primary.endOffset}]"
    | _ => message
  match ← (writeOutput "stderr" IO.getStderr s!"{refusal.code}: {message}\n").run with
  | .ok () => return refusal.exitCode
  | .error outputFailure => return outputFailure.exitCode

private def build (source dir : System.FilePath) (backend : Option String) :
    ExceptT Refusal IO Unit := do
  let loc : BIR.Loc := ⟨source.toString, 1, 1⟩
  unless loc.wf && source.extension == some "con" do
    throw (.usageError "expected a project-relative .con path")
  let path ← toolIO "resolve source path" (IO.FS.realPath source)
  let root ← toolIO "resolve project root" (IO.FS.realPath ".")
  unless path == root / source do
    throw (.usageError "source path must be canonical and project-relative")
  let version ← liftExcept <| (← Driver.Project.checkInvocation (root / "Concrete.toml")).mapError
    Refusal.manifest
  let bytes ← toolIO "read source" (IO.FS.readBinFile path)
  let input ← liftExcept (Stages.Input.load source.toString bytes)
  let out ← liftExcept (Stages.compile input)
  let executable ← toolIO "resolve compiler installation" (IO.appPath >>= IO.FS.realPath)
  let some installation := executable.parent >>= (·.parent) >>= (·.parent) >>= (·.parent)
    | throw (.internal "cannot locate the compiler installation")
  let lib := installation / "scripts/checks/lib"
  let helper := (lib / "slice0_native.sh").toString
  let (lcb, asm) ← if backend == some "llvm" then pure ("-", none) else do
    let f ← liftExcept (Stages.prepareLCB out.bir)
    let lcb ← toolIO "locate LCB"
      (IO.FS.realPath (installation / ".lake/packages/lean-compiler-backend/.lake/build/bin/lcb"))
    let host ← toolIO "detect native target" (IO.Process.run { cmd := "bash", args := #[helper, "target"] })
    let host := host.trimAscii.toString
    let some target := LCB.Target.ofName host
      | throw (.internal s!"native helper returned an unknown target: {host}")
    let text ← liftExcept (LCB.assemble target f)
    pure (lcb.toString, some text)
  let mut inputs := #[]
  let mut artifacts := #[]
  if backend != some "lcb" then
    let some pair := Driver.Launcher.prepareLLVM out.bir
      | throw (.internal "invalid LLVM link inputs")
    inputs := #[("component.ll", pair.component), ("launcher.ll", pair.launcher)]
    artifacts := #["component.o", "launcher.o", "llvm.exe", "llvm.link.sha256"]
  if let some text := asm then
    inputs := inputs ++ #[("lcb_body.s", text), ("lcb_rt.ssa", Runtime.qbe out.bir.sites)]
    artifacts := artifacts ++ #["lcb_body.o", "lcb_rt.s", "lcb_rt.o", "lcb.exe", "lcb.link.sha256"]
  buildNative lib dir backend lcb inputs artifacts version

def main (args : List String) : IO UInt32 := do
  let result ← try
    match args with
      | ["help"] | ["--help"] => (writeOutput "stdout" IO.getStdout (usage.push '\n')).run
      | ["build", source, "-o", dir] => (build source dir none).run
      | ["build", source, "-o", dir, "--backend", backend] =>
        if backend == "llvm" || backend == "lcb" then (build source dir (some backend)).run
        else pure (.error (.usageError usage))
      | _ => pure (.error (.usageError usage))
  catch e => pure (.error (.internal (toString e)))
  match result with
  | .ok () => return 0
  | .error e => report e
