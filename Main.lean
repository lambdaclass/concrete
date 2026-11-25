import Concrete.Stages

open Concrete Concrete.Backend

private def usage : String :=
  "usage: concrete build SOURCE.con -o OUTPUT-DIRECTORY [--backend llvm|lcb] | concrete help"

private def report (refusal : Refusal) : IO UInt32 := do
  let message := match refusal with
    | .syntaxError detail => "syntax error: " ++ detail
    | _ => refusal.message
  IO.eprintln s!"{refusal.code}: {message}"
  return refusal.exitCode

private def toolIO {α : Type} (operation : String) (action : IO α) : ExceptT Refusal IO α :=
  ExceptT.mk do
    try
      return .ok (← action)
    catch e =>
      return .error (.toolFailure s!"{operation}: {e}")

private def build (source dir : System.FilePath) (backend : Option String) :
    ExceptT Refusal IO Unit := do
  let loc : BIR.Loc := ⟨source.toString, 1, 1⟩
  unless loc.wf && source.extension == some "con" do
    throw (.usageError "expected a project-relative .con path")
  let path ← toolIO "resolve source path" (IO.FS.realPath source)
  let root ← toolIO "resolve project root" (IO.FS.realPath ".")
  unless path == root / source do
    throw (.usageError "source path must be canonical and project-relative")
  let bytes ← toolIO "read source" (IO.FS.readBinFile path)
  let out ← liftExcept (Stages.compile ⟨source.toString, bytes⟩)
  let helper := "scripts/checks/lib/slice0_native.sh"
  let (lcb, asm) ← if backend == some "llvm" then pure ("-", none) else do
    let f ← liftExcept (Stages.prepareLCB out.bir)
    let lcb ← toolIO "locate LCB"
      (IO.FS.realPath ".lake/packages/lean-compiler-backend/.lake/build/bin/lcb")
    let host ← toolIO "detect native target" (IO.Process.run { cmd := "bash", args := #[helper, "target"] })
    let some target := LCB.Target.ofName host.trimAscii.toString
      | throw (.internal s!"native helper returned an unknown target: {host}")
    let text ← liftExcept (LCB.assemble target f)
    pure (lcb.toString, some text)
  toolIO "write build inputs" do
    IO.FS.createDirAll dir
    if backend != some "lcb" then
      IO.FS.writeFile (dir / "llvm.ll") (LLVM.module out.bir)
    if let some text := asm then
      IO.FS.writeFile (dir / "lcb_body.s") text
      IO.FS.writeFile (dir / "lcb_rt.ssa") (Runtime.qbe out.bir.sites)
  let args := #[helper, dir.toString, lcb] ++ backend.toArray
  let child ← toolIO "run native build helper" (IO.Process.output { cmd := "bash", args })
  if child.exitCode != 0 then
    let detail := child.stderr.trimAscii.toString
    throw (.toolFailure s!"native build helper exited with status {child.exitCode}{if detail.isEmpty then "" else s!": {detail}"}")
  if !child.stdout.isEmpty then liftM (IO.print child.stdout)
  if !child.stderr.isEmpty then liftM (IO.eprint child.stderr)

def main (args : List String) : IO UInt32 := do
  try
    let result ← match args with
      | ["help"] | ["--help"] => IO.println usage; pure (.ok ())
      | ["build", source, "-o", dir] => (build source dir none).run
      | ["build", source, "-o", dir, "--backend", backend] =>
        if backend == "llvm" || backend == "lcb" then (build source dir (some backend)).run
        else pure (.error (.usageError usage))
      | _ => pure (.error (.usageError usage))
    match result with
    | .ok () => return 0
    | .error e => report e
  catch e => report (.internal (toString e))
