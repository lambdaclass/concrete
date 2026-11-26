import Concrete.Stages
import Concrete.Driver.Project
import Concrete.Driver.Launcher

open Concrete Concrete.Backend

private def usage : String :=
  "usage: concrete build SOURCE.con -o OUTPUT-DIRECTORY [--backend llvm|lcb] | concrete help"

private def report (refusal : Refusal) : IO UInt32 := do
  IO.eprintln s!"{refusal.code}: {refusal.message}"
  return 1

private def build (source dir : System.FilePath) (backend : Option String) : IO UInt32 := do
  let loc : BIR.Loc := ⟨source.toString, 1, 1⟩
  unless loc.wf && source.extension == some "con" do
    return ← report (.syntaxError "expected a project-relative .con path")
  let path ← IO.FS.realPath source
  let root ← IO.FS.realPath "."
  unless path == root / source do
    return ← report (.syntaxError "source path must be canonical and project-relative")
  let version ← match ← Driver.Project.checkInvocation (root / "Concrete.toml") with
    | .ok version => pure version
    | .error refusal =>
      let rule := refusal.ruleId
      IO.eprintln s!"{rule.name}@{rule.rev}: {refusal.message}"
      return 1
  let bytes ← IO.FS.readBinFile path
  let compiled : Except Refusal (Stages.Output × Option LeanCompilerBackend.Verified.Source.Function) := do
    let out ← Stages.compile ⟨source.toString, bytes⟩
    let lcb ← if backend == some "llvm" then pure none else some <$> Stages.prepareLCB out.bir
    return (out, lcb)
  match compiled with
  | .error e => report e
  | .ok (out, lcbInput) =>
    let helper := "scripts/checks/lib/slice0_native.sh"
    let (lcb, asm) ← match lcbInput with
      | none => pure ("-", none)
      | some f => do
        let lcb ← IO.FS.realPath ".lake/packages/lean-compiler-backend/.lake/build/bin/lcb"
        let host ← IO.Process.run { cmd := "bash", args := #[helper, "target"] }
        let some target := LCB.Target.ofName host.trimAscii.toString
          | throw (IO.userError s!"unsupported target: {host}")
        match LCB.assemble target f with
        | .error e => throw (IO.userError e)
        | .ok text => pure (lcb.toString, some text)
    let parent := dir.parent.getD "."
    IO.FS.createDirAll parent
    let parent ← IO.FS.realPath parent
    let stage ← IO.Process.run { cmd := "mktemp", args := #["-d", (parent / ".concrete-build.XXXXXXXX").toString] }
    let stage := System.FilePath.mk stage.trimAscii.toString
    try
      let inputs ← if backend == some "lcb" then pure none else
        match Driver.Launcher.prepareLLVM out.bir with
        | some inputs => pure (some inputs)
        | none => throw (IO.userError "invalid LLVM link inputs")
      if let some pair := inputs then
        IO.FS.writeFile (stage / "component.ll") pair.component
        IO.FS.writeFile (stage / "launcher.ll") pair.launcher
      if let some text := asm then
        IO.FS.writeFile (stage / "lcb_body.s") text
        IO.FS.writeFile (stage / "lcb_rt.ssa") (Runtime.qbe out.bir.sites)
      let args := #[helper, "bound", stage.toString, lcb] ++ backend.toArray
      let child ← IO.Process.output { cmd := "bash", args }
      if !child.stdout.isEmpty then IO.print child.stdout
      if !child.stderr.isEmpty then IO.eprint child.stderr
      if child.exitCode != 0 then return child.exitCode
      if let some pair := inputs then
        unless pair.matches (← IO.FS.readFile (stage / "component.ll"))
            (← IO.FS.readFile (stage / "launcher.ll")) do
          return ← report (.internal "LLVM link inputs changed during native build")
      if let some text := asm then
        unless text == (← IO.FS.readFile (stage / "lcb_body.s")) &&
            Runtime.qbe out.bir.sites == (← IO.FS.readFile (stage / "lcb_rt.ssa")) do
          return ← report (.internal "LCB link inputs changed during native build")
      IO.FS.rename stage dir
      IO.println s!"language-rules version {version.major}.{version.minor}"
      return 0
    finally
      if ← stage.pathExists then IO.FS.removeDirAll stage

def main (args : List String) : IO UInt32 := do
  try
    match args with
    | ["help"] | ["--help"] => IO.println usage; return 0
    | ["build", source, "-o", dir] => build source dir none
    | ["build", source, "-o", dir, "--backend", backend] =>
      if backend == "llvm" || backend == "lcb" then build source dir (some backend)
      else IO.eprintln usage; return 2
    | _ => IO.eprintln usage; return 2
  catch e => report (.internal (toString e))
