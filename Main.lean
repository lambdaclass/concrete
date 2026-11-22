import Concrete.Stages

open Concrete Concrete.Backend

private def usage : String :=
  "usage: concrete build SOURCE.con -o OUTPUT-DIRECTORY [--backend llvm|lcb] | concrete help"

private def report (refusal : Refusal) : IO UInt32 := do
  let message := match refusal with
    | .syntaxError detail => "syntax error: " ++ detail
    | _ => refusal.message
  IO.eprintln s!"{refusal.code}: {message}"
  return 1

private def build (source dir : System.FilePath) (backend : Option String) : IO UInt32 := do
  let loc : BIR.Loc := ⟨source.toString, 1, 1⟩
  unless loc.wf && source.extension == some "con" do
    return ← report (.syntaxError "expected a project-relative .con path")
  let path ← IO.FS.realPath source
  let root ← IO.FS.realPath "."
  unless path == root / source do
    return ← report (.syntaxError "source path must be canonical and project-relative")
  match Stages.compile ⟨source.toString, ← IO.FS.readBinFile path⟩ with
  | .error e => report e
  | .ok out =>
    let helper := "scripts/checks/lib/slice0_native.sh"
    let lcb ← IO.FS.realPath ".lake/packages/lean-compiler-backend/.lake/build/bin/lcb"
    let asm ← if backend == some "llvm" then pure none else do
      let host ← IO.Process.run { cmd := "bash", args := #[helper, "target"] }
      let some target := LCB.Target.ofName host.trimAscii.toString
        | throw (IO.userError s!"unsupported target: {host}")
      match LCB.assemble target out.lcb with
      | .error e => throw (IO.userError e)
      | .ok text => pure (some text)
    IO.FS.createDirAll dir
    if backend != some "lcb" then
      IO.FS.writeFile (dir / "llvm.ll") out.llvm
    if let some text := asm then
      IO.FS.writeFile (dir / "lcb_body.s") text
      IO.FS.writeFile (dir / "lcb_rt.ssa") (Runtime.qbe out.bir.sites)
    let args := #[helper, dir.toString, lcb.toString] ++ backend.toArray
    let child ← IO.Process.output { cmd := "bash", args }
    if !child.stdout.isEmpty then IO.print child.stdout
    if !child.stderr.isEmpty then IO.eprint child.stderr
    return child.exitCode

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
