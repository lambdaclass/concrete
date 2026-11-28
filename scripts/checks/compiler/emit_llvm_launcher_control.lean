import Concrete.Backend.EmitLLVM

/-!
# Two-module LLVM launcher & component clang control

Renders the component (`@concrete_body`) and standalone launcher (`launcherItems`, i.e.,
`declare i32 @concrete_body()` followed by `Backend.Runtime.llvmDiagnostics`) emitted by
`Concrete.Backend.EmitLLVM.emit` for two sample programs:

1. `fn main() -> u8 { return 42; }` (must exit 42 with empty stdout and stderr);
2. `fn main() -> u8 { return 255 + 1; }` (must exit 134 with empty stdout and the exact trap
   line `concrete: trap: overflow at src/main.con:1:30 (site 0)\n` on stderr).

Also checks a negative control: rendering the old launcher without the leading body declaration
(bare `Backend.Runtime.llvmDiagnostics`) violates `LauncherMatches` in Lean and fails
`clang -x ir -c` with `use of undefined value '@concrete_body'`.

Usage:
  lake env lean --run scripts/checks/compiler/emit_llvm_launcher_control.lean [run [target]]
  lake env lean --run scripts/checks/compiler/emit_llvm_launcher_control.lean emit <dir> [target]
-/

namespace Concrete.Backend.EmitLLVM.LauncherControl

open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface
open Concrete.Interfaces
open Concrete.Backend.LLVM

abbrev sLang : CoreLanguage := sliceLanguage slice0
abbrev sLow : LoweringLanguage sLang.operands.Ty := sliceLoweringLanguage slice0
abbrev sU8 : sLang.operands.Ty := sLow.intTy .u8

def sampleMain : FnId := ⟨⟨⟨⟨"p6a"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩, ⟨"main"⟩⟩

def samplePath : SourcePath :=
  match SourcePath.parse (SourceText.ofBytes "src/main.con".toUTF8.data.toList) with
  | .ok p => p
  | .error _ => mainSourcePath

def return42Project : Project :=
  Project.singleton samplePath
    (SourceText.ofBytes "fn main() -> u8 { return 42; }".toUTF8.data.toList)

def overflowProject : Project :=
  Project.singleton samplePath
    (SourceText.ofBytes "fn main() -> u8 { return 255 + 1; }".toUTF8.data.toList)

def overflowOrigin : Origin :=
  ⟨⟨0⟩, ⟨⟨0⟩, 29, 30, 1, 30⟩, ⟨.function sampleMain, ⟨[]⟩⟩⟩

def expectedTrapLine : String :=
  "concrete: trap: overflow at src/main.con:1:30 (site 0)\n"

def llvmProfile (target : Backend.LCB.Target) : Profile :=
  ⟨.llvm, target, 8, 4096⟩

/-- Sample BIR for `fn main() -> u8 { return 42; }`. -/
def return42BIR (target : Backend.LCB.Target) : BIR sLow (llvmProfile target) :=
  let b0 : EmitLLVM.BIRBlock sLow :=
    { id := ⟨0⟩
      params := []
      body := []
      ending := .ret (some ⟨sU8, .literal ⟨[42]⟩⟩)
      boundaryOrigins := [] }
  { project := return42Project
    entry := sampleMain
    signatures := [(sampleMain, ⟨[], some sU8⟩)]
    functions := [(sampleMain, [], ⟨⟨0⟩, #[b0]⟩)]
    layouts := []
    sites := #[]
    mutableLocals := []
    declarations := [] }

/-- Sample BIR for `fn main() -> u8 { return 255 + 1; }` with runtime overflow trap at site 0. -/
def overflowBIR (target : Backend.LCB.Target) : BIR sLow (llvmProfile target) :=
  let b0 : EmitLLVM.BIRBlock sLow :=
    { id := ⟨0⟩
      params := []
      body := [⟨overflowOrigin, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩
        (.literal ⟨[255]⟩) (.literal ⟨[1]⟩)⟩]
      ending := .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, [⟨sU8, .value ⟨0⟩⟩]⟩
      boundaryOrigins := [] }
  let b1 : EmitLLVM.BIRBlock sLow :=
    { id := ⟨1⟩, params := [], body := [], ending := .trap ⟨0⟩, boundaryOrigins := [] }
  let b2 : EmitLLVM.BIRBlock sLow :=
    { id := ⟨2⟩, params := [⟨sU8, ⟨2⟩⟩], body := [],
      ending := .ret (some ⟨sU8, .value ⟨2⟩⟩), boundaryOrigins := [] }
  { project := overflowProject
    entry := sampleMain
    signatures := [(sampleMain, ⟨[], some sU8⟩)]
    functions := [(sampleMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)]
    layouts := []
    sites := #[⟨.overflow, overflowOrigin⟩]
    mutableLocals := []
    declarations := [] }

-- Kernel-checked controls: both sample programs emit valid two-module outputs satisfying
-- `LLVMMetadataMatches` / `LauncherMatches`, tying `launcherItems` to `llvmBodyDefinition`.
example :
    ∃ out,
      EmitLLVM.emit (language := sLang) (return42BIR .x86_64Linux) = .ok out ∧
      LLVMMetadataMatches (return42BIR .x86_64Linux) out ∧
      out.component.items.filter isLLVMBodyHeader = [llvmBodyDefinition] ∧
      out.launcher.items = launcherItems [] := by
  refine ⟨_, rfl, ?_, rfl, rfl⟩
  exact EmitLLVM.emit_metadataMatches (language := sLang) (bir := return42BIR .x86_64Linux) rfl

example :
    ∃ out,
      EmitLLVM.emit (language := sLang) (overflowBIR .x86_64Linux) = .ok out ∧
      LLVMMetadataMatches (overflowBIR .x86_64Linux) out ∧
      out.component.items.filter isLLVMBodyHeader = [llvmBodyDefinition] ∧
      out.launcher.items = launcherItems [expectedTrapLine] := by
  refine ⟨_, rfl, ?_, rfl, rfl⟩
  exact EmitLLVM.emit_metadataMatches (language := sLang) (bir := overflowBIR .x86_64Linux) rfl

-- Kernel-checked negative control: bare `Backend.Runtime.llvmDiagnostics` (missing the
-- body declaration) violates `LauncherMatches`.
example :
    ¬ LauncherMatches (overflowBIR .x86_64Linux)
      ⟨⟨[llvmBodyDefinition, .endFunction]⟩,
       ⟨Backend.Runtime.llvmDiagnostics [expectedTrapLine]⟩,
       (overflowBIR .x86_64Linux).sites⟩ := by
  intro hMatch
  have hEq := hMatch.launcher_items_eq (diagnostics := [expectedTrapLine]) rfl
  cases hEq

private def hasSubstr (haystack needle : String) : Bool :=
  !needle.isEmpty && (haystack.splitOn needle).length > 1

private def detectHostTarget : IO Backend.LCB.Target := do
  let sysOut ← IO.Process.output { cmd := "uname", args := #["-s"] }
  let archOut ← IO.Process.output { cmd := "uname", args := #["-m"] }
  if sysOut.exitCode != 0 || archOut.exitCode != 0 then
    throw (IO.userError "failed to detect host target via uname")
  match s!"{sysOut.stdout.trimAscii.toString}-{archOut.stdout.trimAscii.toString}" with
  | "Linux-x86_64" => return .x86_64Linux
  | "Linux-aarch64" => return .aarch64Linux
  | "Darwin-arm64" => return .aarch64Macos
  | other => throw (IO.userError s!"unsupported host target: {other}")

private def resolveTarget (arg? : Option String) : IO Backend.LCB.Target :=
  match arg? with
  | none => detectHostTarget
  | some name =>
    match Backend.LCB.Target.ofName name with
    | some t => return t
    | none => throw (IO.userError s!"unknown target: {name}")

private def emitCase {target : Backend.LCB.Target} (label : String)
    (bir : BIR sLow (llvmProfile target)) (dest : System.FilePath) :
    IO (LLVMInput (llvmProfile target)) := do
  match EmitLLVM.emit (language := sLang) bir with
  | .error _ => throw (IO.userError s!"EmitLLVM.emit failed on {label}")
  | .ok out =>
    IO.FS.createDirAll dest
    IO.FS.writeBinFile (dest / "component.ll") (out.bytesAt .component)
    IO.FS.writeBinFile (dest / "launcher.ll") (out.bytesAt .launcher)
    return out

private def writeBrokenLauncher {target : Backend.LCB.Target}
    (out : LLVMInput (llvmProfile target)) (diagnostics : List String) (dest : System.FilePath) :
    IO System.FilePath := do
  let broken : LLVMInput (llvmProfile target) :=
    { out with launcher := ⟨Backend.Runtime.llvmDiagnostics diagnostics⟩ }
  let path := dest / "broken_launcher.ll"
  IO.FS.writeBinFile path (broken.bytesAt .launcher)
  return path

private def runCmd (cmd : String) (args : Array String) : IO IO.Process.Output :=
  IO.Process.output { cmd, args }

private def requireOk (label cmd : String) (args : Array String) : IO Unit := do
  let out ← runCmd cmd args
  if out.exitCode != 0 then
    throw (IO.userError
      s!"{label} failed (exit {out.exitCode}):\nstdout:\n{out.stdout}\nstderr:\n{out.stderr}")

private def runClangControls (target : Backend.LCB.Target) : IO Unit := do
  let clang := (← IO.getEnv "CONCRETE_SLICE0_CLANG").getD "clang"
  IO.FS.withTempDir fun dir => do
    let r42Dir := dir / "return42"
    let ovfDir := dir / "overflow"
    let _ ← emitCase "return42" (return42BIR target) r42Dir
    let ovfOut ← emitCase "overflow" (overflowBIR target) ovfDir
    let brokenPath ← writeBrokenLauncher ovfOut [expectedTrapLine] ovfDir

    -- Assemble and link `return 42;`
    requireOk "clang return42 component" clang
      #["-x", "ir", "-c", (r42Dir / "component.ll").toString, "-o", (r42Dir / "component.o").toString]
    requireOk "clang return42 launcher" clang
      #["-x", "ir", "-c", (r42Dir / "launcher.ll").toString, "-o", (r42Dir / "launcher.o").toString]
    requireOk "clang link return42" clang
      #[(r42Dir / "component.o").toString, (r42Dir / "launcher.o").toString,
        "-o", (r42Dir / "return42.exe").toString]
    let r42Run ← runCmd (r42Dir / "return42.exe").toString #[]
    if r42Run.exitCode != 42 || !r42Run.stdout.isEmpty || !r42Run.stderr.isEmpty then
      throw (IO.userError
        s!"return42 mismatch: exit={r42Run.exitCode}, stdout={reprStr r42Run.stdout}, stderr={reprStr r42Run.stderr}")
    IO.println "ok   return 42;: exit=42, stdout empty, stderr empty"

    -- Assemble and link `255 + 1`
    requireOk "clang overflow component" clang
      #["-x", "ir", "-c", (ovfDir / "component.ll").toString, "-o", (ovfDir / "component.o").toString]
    requireOk "clang overflow launcher" clang
      #["-x", "ir", "-c", (ovfDir / "launcher.ll").toString, "-o", (ovfDir / "launcher.o").toString]
    requireOk "clang link overflow" clang
      #[(ovfDir / "component.o").toString, (ovfDir / "launcher.o").toString,
        "-o", (ovfDir / "overflow.exe").toString]
    let ovfRun ← runCmd (ovfDir / "overflow.exe").toString #[]
    if ovfRun.exitCode != 134 || !ovfRun.stdout.isEmpty || ovfRun.stderr != expectedTrapLine then
      throw (IO.userError
        s!"overflow mismatch: exit={ovfRun.exitCode}, stdout={reprStr ovfRun.stdout}, stderr={reprStr ovfRun.stderr}")
    IO.println s!"ok   255 + 1: exit=134, stdout empty, stderr={reprStr ovfRun.stderr}"

    -- Negative control: bare `llvmDiagnostics` without `@concrete_body` declaration fails clang
    let brokenRes ← runCmd clang
      #["-x", "ir", "-c", brokenPath.toString, "-o", (ovfDir / "broken_launcher.o").toString]
    if brokenRes.exitCode == 0 || !hasSubstr brokenRes.stderr "use of undefined value '@concrete_body'" then
      throw (IO.userError
        s!"broken launcher negative control did not fail as expected: exit={brokenRes.exitCode}, stderr={reprStr brokenRes.stderr}")
    IO.println "ok   negative control (bare llvmDiagnostics): clang rejected with undefined '@concrete_body'"

end Concrete.Backend.EmitLLVM.LauncherControl

open Concrete.Backend.EmitLLVM.LauncherControl in
def main (args : List String) : IO Unit := do
  match args with
  | [] =>
    runClangControls (← resolveTarget none)
  | ["run"] =>
    runClangControls (← resolveTarget none)
  | ["run", targetName] =>
    runClangControls (← resolveTarget (some targetName))
  | ["emit", dir] =>
    let target ← resolveTarget none
    let root := System.FilePath.mk dir
    let _ ← emitCase "return42" (return42BIR target) (root / "return42")
    let ovfOut ← emitCase "overflow" (overflowBIR target) (root / "overflow")
    let _ ← writeBrokenLauncher ovfOut [expectedTrapLine] (root / "overflow")
  | ["emit", dir, targetName] =>
    let target ← resolveTarget (some targetName)
    let root := System.FilePath.mk dir
    let _ ← emitCase "return42" (return42BIR target) (root / "return42")
    let ovfOut ← emitCase "overflow" (overflowBIR target) (root / "overflow")
    let _ ← writeBrokenLauncher ovfOut [expectedTrapLine] (root / "overflow")
  | _ =>
    throw (IO.userError "usage: emit_llvm_launcher_control.lean [run [target] | emit <dir> [target]]")
