import Concrete
import Concrete.Proof.ObligationCore
import Concrete.Report.CompilerLedger
import Concrete.Backend.Backend
import Concrete.Resolve.Project

open Concrete

-- Shared CLI exit-code taxonomy (ROADMAP Phase 4 #14). One source of truth for
-- every command's exit code AND the documented `EXIT CODES` help block, so the
-- codes and their documentation cannot drift.
namespace ExitCode
def ok : UInt32 := 0
def usage : UInt32 := 1
def obligationsMissing : UInt32 := 2
def staleEvidence : UInt32 := 3
def proofCheckFailure : UInt32 := 4
def solverFailure : UInt32 := 5
def internalError : UInt32 := 6

/-- (code, meaning) — the single source for both exits and the help block. -/
def taxonomy : List (UInt32 × String) :=
  [ (ok, "success (proved / clean / info printed)"),
    (usage, "invalid invocation (bad args, unknown function)"),
    (obligationsMissing, "obligations missing (eligible but unproved)"),
    (staleEvidence, "stale evidence (body changed since the proof was linked)"),
    (proofCheckFailure, "proof-check failure (Lean kernel rejected a referenced theorem)"),
    (solverFailure, "solver/checker failure (omega/bv_decide/lake could not run)"),
    (internalError, "internal compiler error") ]

/-- The `EXIT CODES` help block, generated from `taxonomy` (no hand-kept copy). -/
def helpBlock : List String :=
  "EXIT CODES:" :: taxonomy.map (fun (c, m) => s!"  {c}  {m}")
end ExitCode

-- Shared command-line flag parsing (ROADMAP Phase 4 #14b). One definition of what
-- a boolean flag and a valued flag mean, so every command parses flags the same
-- way instead of re-deriving `args.contains` / `dropWhile` inline.
namespace Cli
/-- True iff the boolean flag `name` is present. -/
def hasFlag (args : List String) (name : String) : Bool := args.contains name

/-- The value following `name` (e.g. `--out PATH`), or `none` if `name` is absent
    or the next token is itself a flag — so `--out --json` does NOT capture
    `--json` as the path. This guard is applied uniformly (several call sites
    previously omitted it). -/
def flagValue (args : List String) (name : String) : Option String :=
  match args.dropWhile (· != name) with
  | _ :: v :: _ => if v.startsWith "-" then none else some v
  | _ => none
end Cli

/-- Phase 6E #1: the grouped, context-free help page. Groups commands by use
    per the roadmap taxonomy; the full flag matrix stays in `usage` below. -/
def helpText : String := String.intercalate "\n" [
  "concrete — the Concrete compiler",
  "",
  "USAGE: concrete <command> [args]   |   concrete <file.con> [flags]",
  "",
  "DAILY:",
  "  build [file|project]      compile (project mode reads Concrete.toml)",
  "  run   [file|project]      compile and run",
  "  test  [--module <name>]   run #[test] functions",
  "  fmt   <file.con>          format (--check | --write | --stdin)",
  "",
  "REPORTS & EVIDENCE:",
  "  <file> --report <kind>    caps, unsafe, layout, proof-status, obligations, …",
  "  prove <file> <fn>         prove/check an obligation (prove --help for more)",
  "  <file> --query <kind>     semantic queries (why-capability, evidence, …)",
  "",
  "DEBUGGING:",
  "  <file> --trace-pipeline   per-stage trace naming the first failing phase",
  "  <file> --emit-trace-json  per-stage telemetry (counts, timing)",
  "  reduce <file> --predicate <p>   minimize a failing program",
  "  debug-bundle <file>       capture a reproducible failure bundle",
  "",
  "INTERNALS / COMPAT:",
  "  <file> --emit-core | --emit-ssa | --emit-llvm | --interp",
  "  diff, snapshot, audit, check, validate-bundle, --version",
  "",
  "Run `concrete` with no arguments for the full flag matrix.",
  ""]

def usage : String :=
  "Usage: concrete <file.con> [-o output] [--emit-llvm] [--emit-core] [--emit-ssa] [--emit-trace-json] [--trace-pipeline] [--test] [--test --module <name>] [--interp] [--report caps|unsafe|layout|interface|alloc|mono|authority|proof|eligibility|proof-status|obligations|extraction|subject-facts|lean-stubs|check-proofs|proof-diagnostics|proof-deps|proof-bundle|traceability|diagnostics-json|effects|recursion|stack-depth|fingerprints|consistency|contracts|vcs|obligation-ledger|compiler-ledger|verify|audit|arithmetic|multi-kernel|bridge-check|solver-cert|lowering-agreement|artifact-fuzz|term-ir|core-semantics-diff] [--report multi-kernel [--rocq] [--isabelle] [--all-provers] [--require-two-kernels]] [--query KIND|KIND:FUNCTION|fn:FUNCTION] [--fmt (legacy; use `concrete fmt`)]\n       concrete build [-o output] [--emit-llvm]\n       concrete check\n       concrete fmt <file.con> [--check | --write | --stdin]\n       concrete audit <file.con>\n       concrete prove <file.con> <module.function> [--json] [--out <path>] [--force] [--emit-link] [--emit-lean] [--emit-artifacts] [--out-dir <dir>] [--show-obligation <id>] [--replay] [--nearest-lemmas] [--check] [--workspace <dir>]\n       concrete prove --help=agent | --capabilities | --schema\n       concrete run [-- args...]\n       concrete test [--module <name>]\n       concrete diff <old.json> <new.json> [--json]\n       concrete snapshot <file.con> [-o output.json]\n       concrete debug-bundle <file.con> [-o dir]\n       concrete reduce <file.con> --predicate <pred> [-o output] [--verbose]\n       concrete --version"

/-- Capture compiler identity: version, git commit, lean toolchain. -/
def compilerIdentity : IO String := do
  let version := "0.1.0"
  let commit ← try
    let r ← IO.Process.output { cmd := "git", args := #["rev-parse", "--short", "HEAD"] }
    if r.exitCode == 0 then
      let hash := r.stdout.trimAscii.toString
      -- Check for tracked modifications and untracked files
      let d ← IO.Process.output { cmd := "git", args := #["status", "--porcelain"] }
      pure (if d.stdout.trimAscii.toString.isEmpty then hash else hash ++ "-dirty")
    else pure "unknown"
  catch _ => pure "unknown"
  let toolchain ← try
    let tc ← IO.FS.readFile ⟨"lean-toolchain"⟩
    pure tc.trimAscii.toString
  catch _ => pure "unknown"
  return s!"concrete {version} ({commit}) [{toolchain}]"

/-- The compiler identity a RECEIPT binds: the build identity embedded in this binary.

    TOTAL, PURE, AND PORTABLE — and each of those is a correction.

    Three earlier versions named something other than the compiler. `git status --porcelain` made any
    untracked file invalidate every receipt. `git rev-parse HEAD` made a binary built from one commit
    claim another the moment the checkout moved. `sha256sum /proc/self/exe` named the right object and
    could not run on macOS, an active CI platform, where receipt issuance simply refused — so the
    refusal machinery this replaced was not a safety net but an outage.

    `Concrete.buildIdentity` is computed at BUILD time over the compiler's own sources and travels
    with the artifact, so it is a property of the binary rather than of the tree the binary is later
    pointed at. Uncommitted sources are covered for free: the generator digests the working tree.

    128 bits, the project standard, not the 64 an earlier version kept — rehashing a truncated digest
    inside `toolchainIdOf` cannot recover discarded entropy, and this value exists to resist
    substitution.

    KNOWN LIMIT: a binary PATCHED AFTER BUILD keeps the constant its sources produced. This names the
    build, not the bytes on disk. -/
def compilerEvidenceIdentity : String :=
  s!"concrete 0.1.0 (build:{Concrete.buildIdentity})"

def writeFile (path : String) (content : String) : IO Unit := do
  IO.FS.writeFile ⟨path⟩ content

/-- Detect macOS SDK sysroot for clang linking. Returns `--sysroot=<path>` flag if found. -/
def getMacOSSysrootFlags : IO (Array String) := do
  -- Only relevant on macOS
  let os ← IO.Process.output { cmd := "uname", args := #["-s"] }
  if os.stdout.trimAscii.toString != "Darwin" then return #[]
  -- Use xcrun to find the SDK path
  let result ← IO.Process.output { cmd := "xcrun", args := #["--show-sdk-path"] }
  if result.exitCode == 0 then
    let sdkPath := result.stdout.trimAscii.toString
    if sdkPath.length > 0 then return #[s!"--sysroot={sdkPath}"]
  return #[]

/-- Build clang arguments for linking LLVM IR to a native binary. -/
def clangArgs (llPath : String) (outputPath : String) (extraFlags : Array String := #[]) : IO (Array String) := do
  let sysrootFlags ← getMacOSSysrootFlags
  return #[llPath, "-o", outputPath, "-Wno-override-module", "-w", "-O2"] ++ sysrootFlags ++ extraFlags

def runCmd (cmd : String) (args : Array String) : IO UInt32 := do
  let child ← IO.Process.spawn {
    cmd := cmd
    args := args
    stdout := .piped
    stderr := .piped
  }
  let exitCode ← child.wait
  if exitCode != 0 then
    let stderr ← child.stderr.readToEnd
    IO.eprintln stderr
  return exitCode

/-- Hard-error gate for runtime-safety obligations already classified as
    `VIOLATION`. This is intentionally narrower than the contracts report:
    `unproven` obligations remain report/policy facts, while `violation` means
    the compiler proved the safe program is wrong. -/
def enforceProvenRuntimeViolations (modules : List Module) (sourceMap : SourceMap) : IO Bool := do
  let ds := Report.provenViolationDiagnostics modules
  if hasErrors ds then
    IO.eprintln (renderDiagnostics ds (sourceMap := sourceMap))
    return false
  return true

/-- Validate LLVM IR via `llvm-as` (parse-only, no output).
    Returns true if valid or if llvm-as is not found on PATH.
    Returns false (and prints errors) if the IR is malformed. -/
def validateLLVMIR (llPath : String) : IO Bool := do
  -- Check if llvm-as is available
  let which ← IO.Process.output { cmd := "which", args := #["llvm-as"] }
  if which.exitCode != 0 then
    return true  -- llvm-as not available; skip validation
  let result ← IO.Process.output {
    cmd := "llvm-as"
    args := #[llPath, "-o", "/dev/null"]
  }
  if result.exitCode != 0 then
    IO.eprintln s!"LLVM IR validation failed for {llPath}:"
    IO.eprintln result.stderr
    return false
  return true


/-- Best-effort resident-set size in KB. Reads `/proc/self/statm` (Linux); returns
    `none` on any platform without it (e.g. macOS) — the telemetry field is
    platform-graceful, never a hard dependency. -/
def readRssKb : IO (Option Nat) := do
  try
    let contents ← IO.FS.readFile "/proc/self/statm"
    match contents.splitOn " " with
    | _ :: resident :: _ => match resident.toNat? with
      | some pages => pure (some (pages * 4096 / 1024))
      | none => pure none
    | _ => pure none
  catch _ => pure none

def traceTelemetry (inputPath : String) : IO UInt32 := do
  let source ← readFile inputPath
  let git ← compilerIdentity
  let rss ← readRssKb
  let srcMap := [(inputPath, source)]
  -- Run the pipeline stage by stage so we can time each pass. Timing is a debug
  -- aid, not a perf claim; the gate pins schema + monotonic sanity, not magnitudes.
  let baseDir := let parts := inputPath.splitOn "/"
    match parts.reverse with | _ :: rest => "/".intercalate rest.reverse | [] => "."
  -- Run the pipeline stage by stage so each pass can be timed. Timing is a debug
  -- aid, not a perf claim; the gate pins schema + monotonic sanity, not magnitudes.
  let mut timing : List (String × Nat) := []
  let mut t ← IO.monoMsNow
  match Pipeline.parse source with
  | .error ds => IO.eprintln (renderDiagnostics ds (sourceMap := srcMap)); return 1
  | .ok parsed =>
  let astNodes := Pipeline.astModuleNodes parsed.modules
  timing := timing ++ [("parse", (← IO.monoMsNow) - t)]; t ← IO.monoMsNow
  match ← Pipeline.resolveFiles baseDir parsed inputPath resolveAllModules with
  | .error ds => IO.eprintln (renderDiagnostics ds (sourceMap := srcMap)); return 1
  | .ok (resolved, _) =>
  let summary := Pipeline.buildSummary resolved
  timing := timing ++ [("resolve-files", (← IO.monoMsNow) - t)]; t ← IO.monoMsNow
  match Pipeline.resolve resolved summary with
  | .error ds => IO.eprintln (renderDiagnostics ds (sourceMap := srcMap)); return 1
  | .ok rp =>
  let resolvedProg := Pipeline.desugar rp
  timing := timing ++ [("resolve", (← IO.monoMsNow) - t)]; t ← IO.monoMsNow
  match Pipeline.check resolvedProg summary with
  | .error ds => IO.eprintln (renderDiagnostics ds (sourceMap := srcMap)); return 1
  | .ok () =>
  timing := timing ++ [("check", (← IO.monoMsNow) - t)]; t ← IO.monoMsNow
  match Pipeline.elaborate resolvedProg summary with
  | .error ds => IO.eprintln (renderDiagnostics ds (sourceMap := srcMap)); return 1
  | .ok elabProg =>
  timing := timing ++ [("elaborate", (← IO.monoMsNow) - t)]; t ← IO.monoMsNow
  match Pipeline.coreCheck elabProg with
  | .error ds => IO.eprintln (renderDiagnostics ds (sourceMap := srcMap)); return 1
  | .ok validCore =>
  timing := timing ++ [("coreCheck", (← IO.monoMsNow) - t)]; t ← IO.monoMsNow
  match Pipeline.monomorphize validCore with
  | .error ds => IO.eprintln (renderDiagnostics ds (sourceMap := srcMap)); return 1
  | .ok mono =>
  timing := timing ++ [("monomorphize", (← IO.monoMsNow) - t)]; t ← IO.monoMsNow
  match Pipeline.lower mono with
  | .error ds => IO.eprintln (renderDiagnostics ds (sourceMap := srcMap)); return 1
  | .ok ssa =>
  timing := timing ++ [("lower", (← IO.monoMsNow) - t)]
  IO.println (Pipeline.telemetryJson git astNodes validCore mono ssa 0 timing rss)
  return 0

/-- Phase 6C #3: per-stage pipeline trace as JSON. Runs the pipeline stage by
    stage, recording which stages passed and — for a rejected program — the FIRST
    stage that failed plus its diagnostic code. An accepted program reports every
    stage `ok` plus the telemetry counts. Backs `concrete <file> --trace-pipeline`. -/
def tracePipeline (inputPath : String) : IO UInt32 := do
  let source ← readFile inputPath
  let git ← compilerIdentity
  let firstCode : Diagnostics → String := fun ds => match ds.head? with | some d => d.code | none => ""
  let emit := fun (okRev : List String) (fail : Option (String × String)) (counts : String) =>
    IO.println (Pipeline.traceJson git inputPath okRev.reverse fail counts)
  match Pipeline.parse source with
  | .error ds => emit [] (some ("parse", firstCode ds)) "null"; return 0
  | .ok parsed =>
  let ok := ["parse"]
  let baseDir := let parts := inputPath.splitOn "/"
    match parts.reverse with | _ :: rest => "/".intercalate rest.reverse | [] => "."
  match ← Pipeline.resolveFiles baseDir parsed inputPath resolveAllModules with
  | .error ds => emit ok (some ("resolve-files", firstCode ds)) "null"; return 0
  | .ok (resolved, _) =>
  let ok := "resolve-files" :: ok
  let summary := Pipeline.buildSummary resolved
  match Pipeline.resolve resolved summary with
  | .error ds => emit ok (some ("resolve", firstCode ds)) "null"; return 0
  | .ok resolvedProg =>
  let ok := "resolve" :: ok
  let resolvedProg := Pipeline.desugar resolvedProg
  match Pipeline.check resolvedProg summary with
  | .error ds => emit ok (some ("check", firstCode ds)) "null"; return 0
  | .ok () =>
  let ok := "check" :: ok
  match Pipeline.elaborate resolvedProg summary with
  | .error ds => emit ok (some ("elaborate", firstCode ds)) "null"; return 0
  | .ok elabProg =>
  let ok := "elaborate" :: ok
  match Pipeline.coreCheck elabProg with
  | .error ds => emit ok (some ("coreCheck", firstCode ds)) "null"; return 0
  | .ok validCore =>
  let ok := "coreCheck" :: ok
  match Pipeline.monomorphize validCore with
  | .error ds => emit ok (some ("monomorphize", firstCode ds)) "null"; return 0
  | .ok mono =>
  let ok := "monomorphize" :: ok
  match Pipeline.lower mono with
  | .error ds => emit ok (some ("lower", firstCode ds)) "null"; return 0
  | .ok ssa =>
  let ok := "lower" :: ok
  emit ok none (Pipeline.telemetryJson git (Pipeline.astModuleNodes parsed.modules) validCore mono ssa 0)
  return 0

/-- Compile via SSA pipeline: Parse → Resolve → Check → Elab → CoreCanonicalize → CoreCheck → Mono → Lower → SSAVerify → SSACleanup → EmitSSA → clang -/
def compileSSA (inputPath : String) (outputPath : String) (emitLLVM : Bool) : IO UInt32 := do
  let source ← readFile inputPath
  match ← Pipeline.runFrontend inputPath source resolveAllModules with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := [(inputPath, source)]))
    return 1
  | .ok (parsed, _, validCore, srcMap) =>
  if !(← enforceProvenRuntimeViolations parsed.modules srcMap) then
    return 1
  match Pipeline.monomorphize validCore with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := srcMap))
    return 1
  | .ok mono =>
  match Pipeline.lower mono with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := srcMap))
    return 1
  | .ok ssa =>
    -- An executable needs an entry point. Without one, EmitSSA emits no `@main`
    -- wrapper and the failure would otherwise leak from clang/ld as an opaque
    -- "Undefined symbols: _main" (bug 025). Reject it here as a diagnostic.
    -- `--emit-llvm` is exempt: dumping IR for inspection is legitimate with no
    -- entry point (and there is no separate library/embedded build profile).
    if !emitLLVM && !(ssa.ssaModules.any fun m => m.functions.any (·.isEntryPoint)) then
      IO.eprintln "error[link]: no `main` function found; an executable needs an entry point — define `fn main() -> Int` in the root module"
      return 1
    let llvmIR := Pipeline.emit ssa
    let llPath := inputPath ++ ".ll"
    writeFile llPath llvmIR
    if emitLLVM then
      IO.println llvmIR
      return 0
    -- Validate LLVM IR (if llvm-as available)
    let llValid ← validateLLVMIR llPath
    if !llValid then
      IO.FS.removeFile ⟨llPath⟩
      return 1
    -- Compile with clang
    let args ← clangArgs llPath outputPath
    let exitCode ← runCmd "clang" args
    if exitCode != 0 then
      IO.eprintln "clang compilation failed"
      return exitCode
    -- Clean up .ll file
    IO.FS.removeFile ⟨llPath⟩
    IO.println s!"Compiled {inputPath} -> {outputPath}"
    return 0

/-- Interpret a program via the source-level interpreter (no codegen). -/
def interpProgram (inputPath : String) : IO UInt32 := do
  let source ← readFile inputPath
  match ← Pipeline.runFrontend inputPath source resolveAllModules with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := [(inputPath, source)]))
    return 1
  | .ok (_, _, validCore, _) =>
    match Interp.interpret validCore.coreModules with
    | .error msg =>
      IO.eprintln msg
      return 1
    | .ok (retVal, out) =>
      -- Match compiled binary contract byte-for-byte: program output first (the
      -- print_* buffer), then the return value on its own line — but ONLY when
      -- `main` returns a value. A Unit `main` prints no value line, exactly like
      -- the compiled binary (which used to diverge: interp printed a stray `0`).
      IO.print out
      match retVal with
      | some n => IO.println s!"{n}"
      | none => pure ()
      return 0

/-- Compile and run tests: Parse → ... → EmitSSA (test mode) → clang → run -/
def compileTest (inputPath : String) (moduleFilter : Option String := none) : IO UInt32 := do
  let source ← readFile inputPath
  match ← Pipeline.runFrontend inputPath source resolveAllModules with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := [(inputPath, source)]))
    return 1
  | .ok (parsed, _, validCore, srcMap) =>
  if !(← enforceProvenRuntimeViolations parsed.modules srcMap) then
    return 1
  match Pipeline.monomorphize validCore with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := srcMap))
    return 1
  | .ok mono =>
  match Pipeline.lower mono with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := srcMap))
    return 1
  | .ok ssa =>
    let llvmIR := Pipeline.emit ssa (testMode := true) (moduleFilter := moduleFilter)
    let llPath := inputPath ++ ".test.ll"
    let outPath := inputPath ++ ".test"
    writeFile llPath llvmIR
    -- Validate LLVM IR (if llvm-as available)
    let llValid ← validateLLVMIR llPath
    if !llValid then return 1
    let args ← clangArgs llPath outPath
    let exitCode ← runCmd "clang" args
    if exitCode != 0 then
      IO.eprintln "clang compilation failed"
      IO.eprintln s!"LLVM IR left at: {llPath}"
      return exitCode
    -- Run the test binary (keep .ll and binary for debugging)
    let child ← IO.Process.spawn {
      cmd := outPath
      stdout := .piped
      stderr := .piped
    }
    let stdout ← child.stdout.readToEnd
    let stderr ← child.stderr.readToEnd
    let exitCode ← child.wait
    IO.print stdout
    if !stderr.isEmpty then IO.eprint stderr
    if exitCode != 0 then
      IO.eprintln s!"Test binary exited with code {exitCode}"
      IO.eprintln s!"LLVM IR at: {llPath}"
      IO.eprintln s!"Binary at: {outPath}"
    else
      IO.FS.removeFile ⟨llPath⟩
      IO.FS.removeFile ⟨outPath⟩
    return exitCode

/-- Emit Core or SSA IR for inspection. Runs full pipeline including new passes. -/
def compileAndEmit (inputPath : String) (mode : String) : IO UInt32 := do
  let source ← readFile inputPath
  match ← Pipeline.runFrontend inputPath source resolveAllModules with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := [(inputPath, source)]))
    return 1
  | .ok (parsed, _, validCore, srcMap) =>
    if mode == "core" then
      for cm in validCore.coreModules do
        IO.println (ppCModule cm)
      return 0
    if !(← enforceProvenRuntimeViolations parsed.modules srcMap) then
      return 1
    match Pipeline.monomorphize validCore with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := srcMap))
      return 1
    | .ok mono =>
    let lowerResult := if mode == "ssa-unverified"
                        then Pipeline.lowerUnverified mono
                        else Pipeline.lower mono
    match lowerResult with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := srcMap))
      return 1
    | .ok ssa =>
      for sm in ssa.ssaModules do
        IO.println (ppSModule sm)
      return 0


/-- Stable schema version for `concrete prove` machine-readable output. -/
def proveSchemaVersion : String := "1"

/-- THE ONE PACKAGE IDENTITY PRODUCER for every CLI path.

    Four call sites built this independently — report, query, check and the snapshot/diff path — and
    three of them synthesized an identity from the INVOKED FILE'S TEXT while the project loader
    derived one from the manifest. The same program therefore had two identities depending on which
    command you ran: `elf_header` reported 5 proved through one path and 4 through another, because
    the generated attestations matched one identity and not the other.

    A manifest in scope wins, because it is the declared identity of the package the definitions
    belong to. Without one the identity is synthetic over the COMPLETE resolved source map — not the
    invoked file alone, which made two programs sharing one file the same package.

    Returns the `Except` rather than aborting, so each caller keeps its own refusal reporting. -/
def resolvePackageIdentity (inputPath : String) (moduleNames : List String) (srcMap : SourceMap)
    : IO (Except Proof.PackageIdentityRefusal Proof.PackageIdentity) := do
  match ← findProjectRoot (dirOf inputPath) with
  | some root =>
    -- THE PROJECT'S OWN COMPUTATION, not a re-derivation from whatever this command happened to
    -- load. Package identity binds the MODULE INVENTORY, so computing it from the subset of modules
    -- one command resolved gives a different answer than computing it from the package — which is
    -- how `concrete snapshot` and `--report proof-status` disagreed about the same program after the
    -- first fix. `loadProject` is the producer; everything else asks it.
    match ← loadProject root with
    | .ok ctx => return ctx.packageIdentity
    -- A project that cannot load has no identity to report. Falling back to a synthetic one would
    -- reintroduce the second producer this function exists to remove, and would silently answer a
    -- question the project itself could not.
    | .error _ => return .error (.emptyComponent "project failed to load")
  | none => return Proof.PackageIdentity.syntheticForModules moduleNames (srcMap.map (·.2))

/-- Run pipeline to needed depth and produce a report. -/
def compileAndQuery (inputPath : String) (query : String) : IO UInt32 := do
  let source ← readFile inputPath
  let mainSrcMap : SourceMap := [(inputPath, source)]
  match ← Pipeline.runFrontend inputPath source resolveAllModules with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := mainSrcMap))
    return 1
  | .ok (parsed, _, validCore, srcMap) =>
    let locMap := Report.buildFnLocMap parsed.modules inputPath
    let simpleLocMap := locMap.map fun e => (e.qualName, (e.file, e.fnSpan.line))
    let registry ← loadRegistryWithLinks inputPath parsed.modules validCore.coreModules
    -- ONE PRODUCER: manifest identity when a project is in scope, synthetic over the complete
    -- resolved source map otherwise. A refusal ABORTS rather than substituting a
    -- placeholder: every definition identity minted from this ProofCore is package-scoped, and an
    -- "unidentified" fallback would be shared by every such compilation, reintroducing the
    -- collision this migration removes.
    let packageIdentity ← resolvePackageIdentity inputPath (validCore.coreModules.map (·.name)) srcMap
    let pc ← match extractProofCore? validCore packageIdentity simpleLocMap registry with
      | .ok pc => pure pc
      | .error w => IO.eprintln s!"error: {w.explain}"; return 1
    -- Traceability queries need the backend pipeline
    let parts := query.splitOn ":"
    if parts[0]! == "traceability" then
      match Pipeline.monomorphize validCore with
      | .error ds =>
        IO.eprintln (renderDiagnostics ds (sourceMap := srcMap))
        return 1
      | .ok mono =>
        match Pipeline.lower mono with
        | .error ds =>
          IO.eprintln (renderDiagnostics ds (sourceMap := srcMap))
          return 1
        | .ok ssa =>
          let fnFilter := if parts.length == 2 then some parts[1]! else none
          IO.println (Report.queryTraceability validCore.coreModules mono.coreModules ssa.ssaModules locMap fnFilter (registry := registry) (pc := pc))
          return 0
    else
      match Report.queryFacts validCore.coreModules locMap query (registry := registry) (pc := pc) with
      | .ok result =>
        IO.println result
        return 0
      | .error msg =>
        IO.eprintln s!"error: {msg}"
        return 1

-- ============================================================
-- Concrete.toml project support
-- ============================================================


/-- Empty-content stand-in for a CModule, used to preserve a parent
    wrapper (and thus the qualified-name prefix) without dragging in
    sibling functions when scoping. -/
private def emptyWrapper (m : CModule) (subs : List CModule) : CModule :=
  { m with functions := [], structs := [], enums := []
         , externFns := [], constants := []
         , traitDefs := [], traitImpls := [], newtypes := []
         , submodules := subs }

/-- Match a single subtree by bare name, preserving parent wrappers so
    downstream iteration produces fully-qualified names like
    `pkg.<sub>.<leaf>` instead of just `<leaf>`. -/
partial def scopeSubtreeByName
    (m : CModule) (targetNames : List String) : Option CModule :=
  if targetNames.contains m.name then some m
  else
    let scopedSubs := m.submodules.filterMap fun sm => scopeSubtreeByName sm targetNames
    if scopedSubs.isEmpty then none
    else some (emptyWrapper m scopedSubs)

/-- Find any nested CModule subtree whose `name` matches one of `targetNames`.
    Bare-name fallback used when a path-derived module path isn't available
    (e.g. when inputPath lives outside `<projectRoot>/src/`). Note: this
    fallback merges duplicate basenames across subtrees; prefer
    `findMatchingSubtreesByPath` whenever a project root is known. -/
def findMatchingSubtrees (mods : List CModule) (targetNames : List String) : List CModule :=
  mods.filterMap fun m => scopeSubtreeByName m targetNames

/-- Match a single subtree by qualified path suffix, preserving parent
    wrappers so qualified names retain their full prefix. -/
partial def scopeSubtreeByPath
    (m : CModule) (targetPath : List String) (currentPath : List String)
    : Option CModule :=
  let p := currentPath ++ [m.name]
  let isMatch :=
    if targetPath.length > p.length then false
    else (p.drop (p.length - targetPath.length)) == targetPath
  if isMatch then some m
  else
    let scopedSubs := m.submodules.filterMap fun sm => scopeSubtreeByPath sm targetPath p
    if scopedSubs.isEmpty then none
    else some (emptyWrapper m scopedSubs)

/-- Find subtrees whose qualified path (root → leaf) ends with `targetPath`.
    Disambiguates duplicate basenames in different subtrees: with
    `targetPath := ["a", "foo"]`, `pkg.a.foo` matches but `pkg.b.foo`
    does not. Preserves the qualified-name prefix via empty parent
    wrappers (so iteration yields `pkg.a.foo.<fn>`, not `foo.<fn>`). -/
def findMatchingSubtreesByPath
    (mods : List CModule) (targetPath : List String) : List CModule :=
  mods.filterMap fun m => scopeSubtreeByPath m targetPath []

/-- Convert a source file path to its package-relative module path
    segments, given the project root. Returns `none` when the file is
    not under `<projectRoot>/src/`. The package entries `src/main.con`
    and `src/lib.con` map to `[]` (use the parsed top-level name to
    locate the package's root module). `src/foo/mod.con` maps to
    `["foo"]` (the file declares the parent directory's module). -/
def filePathToModulePath (projectRoot inputPath : String) : Option (List String) :=
  let srcPrefix := projectRoot ++ "/src/"
  if !inputPath.startsWith srcPrefix then none
  else
    let chars := inputPath.toList
    let rel := String.ofList (chars.drop srcPrefix.length)
    if !rel.endsWith ".con" then none
    else
      let stripped := String.ofList (rel.toList.take (rel.length - 4))
      let parts := stripped.splitOn "/"
      let parts := if parts.length > 1 && parts.getLast? == some "mod"
                   then parts.dropLast else parts
      some parts

/-- Scope `userModules` (a project's full user-package set) to the modules
    declared in `inputPath`. Uses path-derived qualified module paths to
    disambiguate duplicate basenames across subtrees; falls back to bare
    parsed-name matching when the file is outside `<projectRoot>/src/`. -/
def scopeUserModulesToFile (userModules : List CModule) (inputPath projectRoot : String)
    : IO (List CModule) := do
  let source ← try readFile inputPath catch _ => pure ""
  if source.isEmpty then return userModules
  match Pipeline.parse source with
  | .error _ => return userModules
  | .ok parsedFile =>
    -- BOTH SIDES CANONICAL. `filePathToModulePath` compares `inputPath` against
    -- `projectRoot ++ "/src/"` as a string prefix, so the two must be in the same form.
    -- `findProjectRoot` now returns a real (absolute) path — it had to, because walking up from a
    -- one-level-deep relative path skipped the current directory entirely — and that instantly broke
    -- the comparison here: an absolute root against a relative input never matches, the path-derived
    -- scoping fell back to bare-name matching, and `src/a/foo.con` and `src/b/foo.con` merged back
    -- into one module. Canonicalising the input restores the prefix relation instead of reverting
    -- the walk.
    let inputPath ← try
        pure (← IO.FS.realPath ⟨inputPath⟩).toString
      catch _ => pure inputPath
    let parsedNames := parsedFile.modules.map (·.name)
    -- Path-derived match (preferred — handles duplicate basenames).
    let pathMatched : List CModule :=
      match filePathToModulePath projectRoot inputPath with
      | none => []
      | some [] => []  -- File isn't under src/; nothing to do.
      -- src/main.con and src/lib.con are package entries: the file's
      -- parsed top-level module names ARE the package's top-level
      -- modules, so bare-name lookup is correct here.
      | some ["main"] | some ["lib"] => findMatchingSubtrees userModules parsedNames
      | some path => findMatchingSubtreesByPath userModules path
    if !pathMatched.isEmpty then return pathMatched
    -- Fallback: bare-name match. Used when inputPath isn't under
    -- `<projectRoot>/src/` (e.g. exotic layouts or symlinked files).
    let nameMatched := findMatchingSubtrees userModules parsedNames
    if nameMatched.isEmpty then return userModules else return nameMatched

/-- Discharge call-site contract obligations with `bv_decide`. Each candidate is
    `(index, leanBoolGoal)`; returns the indices that kernel-check. Runs one
    batched `lake env lean` (all goals), falling back to per-goal runs only if
    the batch fails. No external SMT — `bv_decide` is a kernel decision procedure. -/
def bvDischargeCallSites (candidates : List (Nat × String)) : IO (List Nat) := do
  if candidates.isEmpty then return []
  let mkSrc (cs : List (Nat × String)) : String :=
    "import Std.Tactic.BVDecide\n\n"
      ++ String.join (cs.map (fun (i, g) => s!"theorem cobl_{i} : {g} = true := by bv_decide\n"))
  let runLean (src : String) : IO UInt32 := do
    let tmpDir ← IO.Process.output { cmd := "mktemp", args := #["-d"] }
    let dir := tmpDir.stdout.trimAscii.toString
    IO.FS.writeFile ⟨dir ++ "/cobl.lean"⟩ src
    let r ← IO.Process.output { cmd := "lake", args := #["env", "lean", dir ++ "/cobl.lean"],
                                env := #[("LAKE_TERM_ANSI", "0")] }
    let _ ← IO.Process.output { cmd := "rm", args := #["-rf", dir] }
    return r.exitCode
  if (← runLean (mkSrc candidates)) == 0 then
    return candidates.map (·.1)
  else
    let mut proved : List Nat := []
    for c in candidates do
      if (← runLean (mkSrc [c])) == 0 then proved := proved ++ [c.1]
    return proved

/-- Discharge nonlinear integer-overflow goals with `bv_decide`. Each candidate
    is `(key, propGoal)` where the goal is a quantified BitVec proposition
    (`∀ (v : BitVec w), … → BitVec.ule e hi`); returns the keys that kernel-check.
    `intros; bv_decide` introduces the operands and bound hypotheses, then
    bitblasts — an LRAT-checked kernel decision, no external SMT. Batched,
    falling back per-goal. -/
def bvDischargeOverflow (candidates : List (String × String)) : IO (List String) := do
  if candidates.isEmpty then return []
  let indexed := (List.range candidates.length).zip candidates
  let mkSrc (cs : List (Nat × (String × String))) : String :=
    "import Std.Tactic.BVDecide\n\n"
      ++ String.join (cs.map (fun (i, (_, g)) => s!"theorem ovf_{i} : {g} := by intros; bv_decide\n"))
  let runLean (src : String) : IO UInt32 := do
    let tmpDir ← IO.Process.output { cmd := "mktemp", args := #["-d"] }
    let dir := tmpDir.stdout.trimAscii.toString
    IO.FS.writeFile ⟨dir ++ "/ovf.lean"⟩ src
    let r ← IO.Process.output { cmd := "lake", args := #["env", "lean", dir ++ "/ovf.lean"],
                                env := #[("LAKE_TERM_ANSI", "0")] }
    let _ ← IO.Process.output { cmd := "rm", args := #["-rf", dir] }
    return r.exitCode
  if (← runLean (mkSrc indexed)) == 0 then
    return candidates.map (·.1)
  else
    let mut proved : List String := []
    for c in indexed do
      if (← runLean (mkSrc [c])) == 0 then proved := proved ++ [c.2.1]
    return proved

/-- Discharge loop init/variant VCs with `omega`. Each candidate is
    `(key, leanGoal)`; returns the keys that kernel-check. These VCs are linear
    integer facts over `Int` (the same domain as the preservation proof), so the
    decision procedure is `omega`, not `bv_decide` (bitvector-only). Batched,
    falling back per-goal. No external SMT — `omega` is a kernel decision
    procedure with a checked certificate. -/
def kernelDischargeLoopVCs (candidates : List (String × String)) : IO (List String) := do
  if candidates.isEmpty then return []
  let indexed := (List.range candidates.length).zip candidates
  let mkSrc (cs : List (Nat × (String × String))) : String :=
    String.join (cs.map (fun (i, (_, g)) => s!"theorem lvc_{i} : {g} := by intros; omega\n"))
  let runLean (src : String) : IO UInt32 := do
    let tmpDir ← IO.Process.output { cmd := "mktemp", args := #["-d"] }
    let dir := tmpDir.stdout.trimAscii.toString
    IO.FS.writeFile ⟨dir ++ "/lvc.lean"⟩ src
    let r ← IO.Process.output { cmd := "lake", args := #["env", "lean", dir ++ "/lvc.lean"],
                                env := #[("LAKE_TERM_ANSI", "0")] }
    let _ ← IO.Process.output { cmd := "rm", args := #["-rf", dir] }
    return r.exitCode
  if (← runLean (mkSrc indexed)) == 0 then
    return candidates.map (·.1)
  else
    let mut proved : List String := []
    for c in indexed do
      if (← runLean (mkSrc [c])) == 0 then proved := proved ++ [c.2.1]
    return proved

-- (Phase 3 #14) The former `computeVacuousQuals` / `computeAssumeQuals` /
-- `computeSolverTrustedQuals` side channels are gone: policy now reads these
-- facts from the one obligation ledger via `computePolicyQuals` (below), which
-- projects `ObligationCore.{vacuousFunctions,assumeFunctions,solverTrustedIds}`.

/-- Build the VC schedule and fold in the kernel-checked discharge results
    (omega over the linear goals; `bv_decide` over the BitVec call-site and
    overflow goals). The proved/`counterexample`/constant verdicts already
    decided structurally by `collectVCs` are preserved; only `planned` VCs are
    upgraded — and only to `proved_by_kernel_decision` (or `arithmetic_proved`
    for loop preservation), never to an external-solver class. -/
def computeVCsDischarged (modules : List Concrete.Module) (locMap : Report.FnLocMap)
    (registry : Concrete.ProofRegistry) : IO (List Report.VC) := do
  let vcs := Report.collectVCs modules locMap registry
  let omegaGoals := Report.callPrecondGoals modules ++ Report.assertGoals modules
    ++ Report.vacuityGoals modules ++ Report.loopVCGoals modules
    ++ Report.boundsGoals modules ++ Report.divGoals modules ++ Report.divQuotGoals modules ++ Report.shiftGoals modules ++ Report.overflowGoals modules
  let omegaProved ← kernelDischargeLoopVCs omegaGoals
  let obs := Report.callSiteObligations modules
  let cands := ((List.range obs.length).zip obs).filterMap fun (i, o) => o.leanGoal.map (fun g => (i, g))
  let bvIdx ← bvDischargeCallSites cands
  let bvCallKeys := bvIdx.filterMap fun i => obs[i]?.map (·.key)
  let ovfBV := (Report.overflowBVGoals modules).filter (fun (k, _) => !omegaProved.contains k)
  let bvOvfKeys ← bvDischargeOverflow ovfBV
  -- R-0461: cap each obligation by the weakest thing its hypotheses rest on. LAST, because
  -- whether a loop's O1/O2 is proved is only known once every VC has a final status — the
  -- ordering is the reason this is a pass rather than a check inside the collectors.
  return Report.capOnHypothesisDebt (Report.dischargeVCs vcs omegaProved (bvCallKeys ++ bvOvfKeys))

/-- Read the value Z3 assigned to variable `v` from `(get-model)` output. Handles
    both `... Int 100000)` and the negative `... Int (- 5))` shapes, across lines.
    The declared name IS the source variable name, so no remapping is needed. -/
def smtModelValue (out v : String) : Option String :=
  match ((out.splitOn s!"define-fun {v} () Int").drop 1).head? with
  | none => none
  | some after =>
    let cs := after.toList.dropWhile (fun c => !c.isDigit && c != '-')
    match cs with
    | [] => none
    | c :: rest =>
      if c == '-' then
        let num := (rest.dropWhile (· == ' ')).takeWhile Char.isDigit
        if num.isEmpty then none else some (String.ofList ('-' :: num))
      else
        let num := cs.takeWhile Char.isDigit
        if num.isEmpty then none else some (String.ofList num)

/-- Solver identity + version for provenance, e.g. "z3 4.16.0", or "z3 (unavailable)"
    when Z3 is not on PATH. `z3 --version` prints "Z3 version X.Y.Z - 64 bit". -/
def z3VersionId : IO String := do
  try
    let o ← IO.Process.output { cmd := "z3", args := #["--version"] }
    if o.exitCode != 0 then return "z3 (unavailable)"
    -- extract the version token after "version"
    let toks := o.stdout.trimAscii.toString.splitOn " "
    match toks.dropWhile (· != "version") with
    | _ :: v :: _ => return s!"z3 {v}"
    | _ => return "z3"
  catch _ => return "z3 (unavailable)"

/-- External-SMT adapter (Phase 2 #8/#10). One solver (Z3), pinned timeout. For each
    `(vcKey, smtlibScript)` it writes the script, runs `z3 -T:<timeout>`, and reads
    the first line: `unsat` → `solver_trusted` (solver-proved, solver in the TCB —
    NOT a kernel-checked class), `sat` → `counterexample` (with the model parsed
    back to source variables via `(get-model)`), `unknown`/`timeout` accordingly. If
    Z3 is not on PATH the honest verdict is `solver_error` for every query — an
    absent solver never yields a proof. Only ever called behind an explicit flag.
    Returns `(vcKey, resultClass, counterexampleModel)`. -/
def smtDischarge (queries : List (String × String)) (timeoutSec : Nat)
    (timeoutMs : Option Nat := none)
    : IO (List (String × String × List (String × String))) := do
  if queries.isEmpty then return []
  -- a configured tiny millisecond soft-timeout (`-t:<ms>`) forces `unknown`; the
  -- default is the per-run wall timeout (`-T:<sec>`).
  let timeoutArg := match timeoutMs with
    | some ms => "-t:" ++ toString ms
    | none => "-T:" ++ toString timeoutSec
  let haveZ3 ← (try
      let o ← IO.Process.output { cmd := "bash", args := #["-c", "command -v z3"] }
      pure (o.exitCode == 0)
    catch _ => pure false)
  if !haveZ3 then
    return queries.map (fun (k, _) => (k, "solver_error", []))
  let mut res : List (String × String × List (String × String)) := []
  for (k, script) in queries do
    let r ← (try
        let mk ← IO.Process.output { cmd := "mktemp", args := #["-t", "concrete-vc-XXXXXX"] }
        let path := mk.stdout.trimAscii.toString
        IO.FS.writeFile ⟨path⟩ script
        let out ← IO.Process.output { cmd := "z3", args := #[timeoutArg, path] }
        let _ ← IO.Process.output { cmd := "rm", args := #["-f", path] }
        let stdout := out.stdout
        let line := ((stdout.trimAscii.toString.splitOn "\n").head?.getD "").trimAscii.toString
        if line == "unsat" then pure ("solver_trusted", ([] : List (String × String)))
        else if line == "sat" then
          -- the declared vars are the source names; read each from the model.
          let vars := (script.splitOn "\n").filterMap fun l =>
            let t := l.trimAscii.toString
            if t.startsWith "(declare-const " then ((t.drop "(declare-const ".length).toString.splitOn " ").head?
            else none
          let model := vars.filterMap fun vn => (smtModelValue stdout vn).map (fun x => (vn, x))
          pure ("counterexample", model)
        else if line == "unknown" then pure ("unknown", [])
        else if line == "timeout" then pure ("timeout", [])
        else pure ("solver_error", [])
      catch _ => pure ("solver_error", ([] : List (String × String))))
    res := res ++ [(k, r.1, r.2)]
  return res

/-- Lean replay (Phase 2 #12). Given `(vcKey, leanTheoremSource)` pairs, write each
    theorem to a file and run `lake env lean` on it. Returns the keys Lean
    INDEPENDENTLY closed (exit 0) — those graduate from `solver_trusted` to
    `proved_by_lean_replay`. Only the in-toolchain tactic in the source (`omega`)
    is used; no Mathlib. A theorem omega cannot close simply fails to compile and
    its key is not returned, so the VC honestly stays `solver_trusted`. -/
def leanReplayCheck (goals : List (String × String)) : IO (List String) := do
  if goals.isEmpty then return []
  let mut closed : List String := []
  for (k, src) in goals do
    let ok ← (try
        let tmpDir ← IO.Process.output { cmd := "mktemp", args := #["-d"] }
        let dir := tmpDir.stdout.trimAscii.toString
        IO.FS.writeFile ⟨dir ++ "/vc_replay.lean"⟩ src
        let r ← IO.Process.output { cmd := "lake", args := #["env", "lean", dir ++ "/vc_replay.lean"],
                                    env := #[("LAKE_TERM_ANSI", "0")] }
        let _ ← IO.Process.output { cmd := "rm", args := #["-rf", dir] }
        pure (r.exitCode == 0)
      catch _ => pure false)
    if ok then closed := closed ++ [k]
  return closed

/-- Identity of the IN-TOOLCHAIN kernel (Lean), read from `lean-toolchain`, e.g.
    "lean leanprover/lean4:v4.28.0". Recorded alongside the external kernels so a
    stored multi-kernel claim names every attester, not only the external ones. -/
def leanToolchainId : IO String := do
  try
    let tc ← IO.FS.readFile ⟨"lean-toolchain"⟩
    let t := tc.trimAscii.toString
    pure (if t.isEmpty then "lean (unknown)" else s!"lean {t}")
  catch _ => pure "lean (unknown)"

/-- Rocq (second-kernel) identity + version for provenance, e.g. "coqc 8.20.1", or
    "coqc (unavailable)" when Coq is not on PATH. `coqc --version` prints
    "The Coq Proof Assistant, version X.Y.Z". -/
def coqVersionId : IO String := do
  try
    let o ← IO.Process.output { cmd := "coqc", args := #["--version"] }
    if o.exitCode != 0 then return "coqc (unavailable)"
    -- `coqc --version` prints two lines ("The Rocq Prover, version X.Y.Z" then
    -- "compiled with OCaml ..."); parse the FIRST line only so the version token
    -- does not swallow the newline + "compiled".
    let firstLine := ((o.stdout.splitOn "\n").head?.getD "").trimAscii.toString
    match (firstLine.splitOn " ").dropWhile (· != "version") with
    | _ :: v :: _ => return s!"coqc {v.trimAscii.toString}"
    | _ => return "coqc"
  catch _ => return "coqc (unavailable)"

/-- Is a command on PATH? Used to distinguish "kernel absent" from "kernel refused"
    in the discharge functions — the difference between "not asked" and "said no". -/
def commandAvailable (cmd : String) : IO Bool := do
  try
    let o ← IO.Process.output { cmd := "bash", args := #["-c", s!"command -v {cmd}"] }
    pure (o.exitCode == 0)
  catch _ => pure false

/-- Where prover verdicts are cached (gitignored). -/
def proverCacheDir : String := ".concrete-cache/provers"

/-- Does `pat` occur in `s`? (`splitOn` on a non-empty needle yields >1 piece iff
    the needle occurs.) Used to classify prover output by message, not exit code. -/
def strContains (s pat : String) : Bool := (s.splitOn pat).length > 1

/-- What an external kernel actually said about one goal. THREE-valued, because
    exit-code-only classification lies: `coqc` exits 1 both when `lia` genuinely
    cannot prove the goal and when our emitted script is malformed. Reporting the
    latter as `refused` claims an independent kernel DISAGREES with Lean when in
    truth our own lowering is broken — the most misleading thing an evidence report
    can do, because it looks like exactly the signal the report exists to find.

    Empirically determined (coqc 9.0.1 / Isabelle2025-2), see `classifyRocqFailure`
    and `classifyIsabelleFailure` for the message markers. -/
inductive KernelVerdict where
  /-- The kernel checked a proof term for the goal. -/
  | closed
  /-- The kernel was asked and its decision procedure could not close the goal.
      This is the only verdict that means "the kernel says no". -/
  | refused
  /-- No verdict was obtained: malformed script, missing library, timeout, or an
      infrastructure failure. OUR problem, not a statement about the goal. -/
  | error (detail : String)
deriving Inhabited, BEq

/-- Report-cell spelling of a verdict. -/
def KernelVerdict.cell : KernelVerdict → String
  | .closed => "closed"
  | .refused => "refused"
  | .error _ => "error"

/-- Is this a statement ABOUT THE GOAL (so safe to cache and to count as evidence)?
    An `error` is not: it says nothing about the goal and must never persist. -/
def KernelVerdict.isDefinitive : KernelVerdict → Bool
  | .error _ => false
  | _ => true

/-- Memoize a prover verdict on disk, keyed on `(tag, script)`. `tag` MUST encode
    the prover name AND version, so a tool upgrade invalidates the key.

    Two properties this cache must have, both of which a naive version gets wrong:

    * Only DEFINITIVE verdicts are stored. Caching an `error` would turn a transient
      failure (mktemp failure, OOM-killed prover, full disk) into a PERMANENT
      "this kernel refuses this goal" — and since neither the tool version nor the
      script changes on a re-run, the key never changes, so it would never
      self-heal. A release gate would then fail forever for a reason that has
      nothing to do with the proof.
    * The full key WITNESS is stored alongside the verdict and compared on read. A
      64-bit `hash` as the filename is not collision-free, and a collision on a
      stored `closed` would FABRICATE evidence that a kernel proved a goal it never
      saw. On witness mismatch we treat the entry as a miss and recompute.

    Any cache I/O error falls back to recomputation: the cache can only make things
    faster, never change a verdict. -/
def cachedKernelVerdict (tag src : String) (run : IO KernelVerdict) : IO KernelVerdict := do
  -- `\n` separates the two fields; the witness is stored verbatim so an exact
  -- comparison on read is possible.
  let witness := tag ++ "\n" ++ src
  let path : System.FilePath := ⟨proverCacheDir ++ "/" ++ toString (hash witness)⟩
  let hit ← (try
      let contents ← IO.FS.readFile path
      match contents.splitOn "\n" with
      | verdict :: rest =>
        -- witness mismatch ⇒ hash collision ⇒ not our entry ⇒ recompute
        if "\n".intercalate rest == witness then
          pure (match verdict with
                | "closed"  => some KernelVerdict.closed
                | "refused" => some KernelVerdict.refused
                | _         => none)
        else pure none
      | [] => pure none
    catch _ => pure none)
  match hit with
  | some v => pure v
  | none =>
    let v ← run
    if v.isDefinitive then
      try
        IO.FS.createDirAll proverCacheDir
        IO.FS.writeFile path (v.cell ++ "\n" ++ witness)
      catch _ => pure ()
    pure v

/-- Classify a nonzero-exit `coqc` run. Verified against coqc 9.0.1:

    * `lia` cannot prove it      → `Error: Tactic failure:  Cannot find witness.`
    * our lowering is malformed  → `Error: Syntax error: ...`
    * bad/missing import         → `Error: Cannot find a physical path bound to ...`

    All three exit 1, so ONLY the tactic-failure marker counts as the kernel saying
    no; anything else is our bug and must surface as `error`. -/
def classifyRocqFailure (output : String) : KernelVerdict :=
  if strContains output "Tactic failure" then .refused
  else .error (firstDiagnosticLine output)
where
  /-- First `Error:`-bearing line, trimmed, for a short report detail. -/
  firstDiagnosticLine (s : String) : String :=
    let errLines := (s.splitOn "\n").filter (fun l => strContains l "Error")
    let line := (errLines.head?.getD "unclassified prover failure").trimAscii.toString
    if line.length > 160 then String.ofList (line.toList.take 160) ++ "..." else line

/-- Classify a nonzero-exit `isabelle build` run. Verified against Isabelle2025-2
    (markers appear on STDOUT, and stderr additionally carries unrelated fontconfig
    noise, so match markers rather than "did it print anything"):

    * `presburger` cannot prove it → `*** Failed to apply initial proof method`
    * our lowering is malformed    → `*** Inner syntax error` / `Failed to parse prop`

    Both exit 1. `Failed to finish proof` covers the multi-step-proof form. -/
def classifyIsabelleFailure (output : String) : KernelVerdict :=
  if strContains output "Failed to apply initial proof method"
     || strContains output "Failed to finish proof" then .refused
  else .error (firstDiagnosticLine output)
where
  firstDiagnosticLine (s : String) : String :=
    let errLines := (s.splitOn "\n").filter (fun l => strContains l "***")
    let line := (errLines.head?.getD "unclassified prover failure").trimAscii.toString
    if line.length > 160 then String.ofList (line.toList.take 160) ++ "..." else line

/-- Second-kernel discharge (Rocq/`coqc`). Given `(vcKey, coqSource)` pairs, write
    each `.v` and run `coqc` on it. Returns `none` if `coqc` is NOT on PATH (the
    kernel was never asked — no honest verdict), else a per-goal `KernelVerdict`:

    * `closed`  — exit 0: `lia` closed the goal and the kernel checked the term.
    * `refused` — coqc reported a TACTIC FAILURE: the decision procedure said no.
    * `error`   — nonzero exit for any other reason (malformed script, missing
                  library, tool failure). Says nothing about the goal.

    A key absent from the result was never emitted (outside the fragment). No VC is
    ever fabricated. Only called behind `--rocq`. -/
def rocqDischarge (goals : List (String × String)) : IO (Option (List (String × KernelVerdict))) := do
  if !(← commandAvailable "coqc") then return none
  let version ← coqVersionId
  let mut out : List (String × KernelVerdict) := []
  for (k, src) in goals do
    let v ← cachedKernelVerdict s!"rocq|{version}" src (do
      try
        let tmpDir ← IO.Process.output { cmd := "mktemp", args := #["-d"] }
        let dir := tmpDir.stdout.trimAscii.toString
        if dir.isEmpty then
          pure (.error "could not create a temp dir for the Rocq script")
        else do
          IO.FS.writeFile ⟨dir ++ "/vc_rocq.v"⟩ src
          -- `-native-compiler no` skips a native `.vo` (faster); `cwd := dir` keeps
          -- coqc's `.lia.cache` scratch inside the temp dir, removed below.
          let r ← IO.Process.output { cmd := "coqc", args := #["-native-compiler", "no", dir ++ "/vc_rocq.v"], cwd := some dir }
          let _ ← IO.Process.output { cmd := "rm", args := #["-rf", dir] }
          -- coqc reports diagnostics on stderr; check both streams.
          let combined := r.stdout ++ "\n" ++ r.stderr
          -- ATTEST before believing exit 0. `coqc` exits 0 on `Admitted.` as well as
          -- `Qed.` — verified — so the exit code alone cannot distinguish a closed proof
          -- from a merely stated one. The emitted script ends in `Print Assumptions`,
          -- which prints "Closed under the global context" for a Qed-closed proof and
          -- lists the constant under "Axioms:" otherwise. An unattested exit 0 is OUR
          -- problem (a malformed or non-closing script), never a statement about the
          -- goal, so it classifies as `error` rather than `closed` or `refused`.
          pure (if r.exitCode == 0 then
                  if strContains combined "Axioms:" then
                    .error "proof not closed: `Print Assumptions` reports axioms (admitted, not proved)"
                  else if !strContains combined "Closed under the global context" then
                    .error "proof unattested: no `Print Assumptions` confirmation in coqc output"
                  else .closed
                else classifyRocqFailure combined)
      catch e => pure (.error s!"coqc could not be run: {e}"))
    out := out ++ [(k, v)]
  return some out

/-- Isabelle (HOL kernel) identity + version, e.g. "isabelle Isabelle2025", or
    "isabelle (unavailable)" when not on PATH. `isabelle version` prints "Isabelle2025". -/
def isabelleVersionId : IO String := do
  try
    let o ← IO.Process.output { cmd := "isabelle", args := #["version"] }
    if o.exitCode != 0 then return "isabelle (unavailable)"
    return s!"isabelle {o.stdout.trimAscii.toString}"
  catch _ => return "isabelle (unavailable)"

/-- Independent-kernel discharge (Isabelle/HOL). Same contract as `rocqDischarge`:
    `none` if `isabelle` is absent, else a per-goal `KernelVerdict`. Writes each
    `(vcKey, theorySource)` as `VC.thy` + a minimal `ROOT` and runs `isabelle build`
    — exit 0 iff `presburger` closed the lemma and the HOL kernel checked it. HOL
    (not CIC), so agreement with Lean/Rocq spans logics. Only called behind
    `--isabelle`; slower than coqc (session build), so opt-in. -/
def isabelleDischarge (goals : List (String × String)) : IO (Option (List (String × KernelVerdict))) := do
  if !(← commandAvailable "isabelle") then return none
  let version ← isabelleVersionId
  let mut out : List (String × KernelVerdict) := []
  for (k, src) in goals do
    let v ← cachedKernelVerdict s!"isabelle|{version}" src (do
      try
        let tmpDir ← IO.Process.output { cmd := "mktemp", args := #["-d"] }
        let dir := tmpDir.stdout.trimAscii.toString
        if dir.isEmpty then
          pure (.error "could not create a temp dir for the Isabelle theory")
        else do
          IO.FS.writeFile ⟨dir ++ "/VC.thy"⟩ src
          IO.FS.writeFile ⟨dir ++ "/ROOT"⟩ "session VCsess = HOL +\n  theories VC\n"
          -- build the one-theory session; the prebuilt HOL heap is reused.
          let r ← IO.Process.output { cmd := "isabelle", args := #["build", "-D", dir], cwd := some dir }
          let _ ← IO.Process.output { cmd := "rm", args := #["-rf", dir] }
          -- Isabelle prints proof/syntax diagnostics on STDOUT; stderr additionally
          -- carries unrelated fontconfig noise, so classify on markers, not streams.
          pure (if r.exitCode == 0 then .closed
                else classifyIsabelleFailure (r.stdout ++ "\n" ++ r.stderr))
      catch e => pure (.error s!"isabelle could not be run: {e}"))
    out := out ++ [(k, v)]
  return some out

/-- Only `closed` counts as evidence: `refused` and `error` both contribute nothing
    (and `error` is not even a statement about the goal). -/
def closedKeysOf (r : Option (List (String × KernelVerdict))) : List String :=
  (r.getD []).filterMap (fun (k, v) => if v == .closed then some k else none)

/-- Obligation ids the kernel actively REFUSED — it rendered the goal, decided it, and
    said no. Distinct from `error` (our script broke) and from absence (never asked),
    and the distinction is the whole point: only a refusal can dissent from Lean.
    Kept separate from `closedKeysOf` because "did not attest" and "said no" are
    different facts and collapsing them is what hid verdict disagreement. -/
def refusedKeysOf (r : Option (List (String × KernelVerdict))) : List String :=
  (r.getD []).filterMap (fun (k, v) => if v == .refused then some k else none)

-- REMOVED 2026-08-03: `disagreeingKeysOf`, which returned the ids whose agreement lemma
-- was REFUSED. Every consumer now uses `validatedKeysOf` instead, and leaving the old
-- function in place would be worse than dead code: it is the fail-open shape, sitting one
-- identifier away from the fail-closed one, in the file where confusing them awards a
-- badge to an unvalidated lowering. If a future caller genuinely needs "which renderings
-- were refused" — for a diagnostic, not for a badge — reintroduce it with that use named.

/-- Run the SMT rendering-validation queries and classify each instance.

    Expected answer is ALWAYS `unsat` — see `smtAgreementGoals`: the query asserts the
    negation when the reference says true and the formula itself when it says false, so
    agreement means unsat either way. `sat` is therefore a real disagreement (the rendering
    denotes something else), and `unknown`/timeout/absence is OUR problem, classified as
    `error` so it can never pass as validation.

    Deliberately NOT reusing `smtDischarge`: that maps answers onto solver-evidence classes
    (`solver_trusted` / `counterexample`), which is a statement about the OBLIGATION. Here
    the same answers mean something different — a statement about the RENDERING — and
    collapsing the two is how a validation result would end up read as a proof result. -/
def smtAgreementCheck (queries : List (String × String)) : IO (Option (List (String × KernelVerdict))) := do
  if queries.isEmpty then return some []
  let haveZ3 ← (try
      let o ← IO.Process.output { cmd := "bash", args := #["-c", "command -v z3"] }
      pure (o.exitCode == 0)
    catch _ => pure false)
  if !haveZ3 then return none          -- absent tool: no verdicts, never a false pass
  let dir ← IO.Process.run { cmd := "mktemp", args := #["-d"] }
  let dir := dir.trimAscii.toString
  let mut out : List (String × KernelVerdict) := []
  for (k, q) in queries do
    let f := s!"{dir}/q.smt2"
    IO.FS.writeFile f q
    let r ← (try
        IO.Process.output { cmd := "z3", args := #["-T:5", f] }
      catch _ => pure { exitCode := 1, stdout := "", stderr := "" })
    let o := r.stdout.trimAscii.toString
    let v : KernelVerdict :=
      if o.startsWith "unsat" then .closed
      else if o.startsWith "sat" then .refused
      else .error s!"z3 said '{o}' (expected unsat)"
    out := out ++ [(k, v)]
  _ ← (try IO.Process.run { cmd := "rm", args := #["-rf", dir] } catch _ => pure "")
  return some out

/-- Obligation ids whose SMT RENDERING is validated: EVERY ground instance agreed, and
    there was at least one.

    Keys arrive as `{obligationId}#smtagree{N}` — one per instance — so they are grouped
    back to the obligation. `all` rather than `any` is the whole point: one disagreeing
    instance means the rendering denotes something else, and a single agreeing instance
    proves nothing about the rest. The non-empty check blocks the vacuous case, where an
    obligation contributed no instances and would otherwise pass `all` trivially.

    `none` (no agreement run — no z3) yields `[]`: nothing validated, so nothing folds.
    Fail closed, same direction as `validatedKeysOf`. -/
def smtValidatedObligations (r : Option (List (String × KernelVerdict))) : List String :=
  match r with
  | none => []
  | some vs =>
    let base := fun (k : String) => match k.splitOn "#smtagree" with | h :: _ => h | [] => k
    let ids := (vs.map (fun (k, _) => base k)).eraseDups
    ids.filter (fun id =>
      let mine := vs.filter (fun (k, _) => base k == id)
      !mine.isEmpty && mine.all (fun (_, v) => v == .closed))

/-- Keep only the SMT results whose rendering was validated, and say how many were dropped.

    Without this an obligation could be marked `solver_trusted` — a verdict that enters the
    TCB with no kernel re-deriving it — on a rendering nobody checked. It is the same
    composition the witness change closed for the kernel path: closing a goal is evidence
    about the OBLIGATION only if the goal denotes the obligation.

    Dropped verdicts are reported rather than silently discarded. A VC that quietly stays
    `unproven` because its rendering failed validation is indistinguishable from one the
    solver could not close, and those are different facts. -/
def gateSmtOnValidation (results : List (String × String × List (String × String)))
    (validated : List String) : IO (List (String × String × List (String × String))) := do
  let kept := results.filter (fun (k, _, _) => validated.contains k)
  let dropped := results.length - kept.length
  if dropped > 0 then
    IO.eprintln s!"warning: {dropped} SMT verdict(s) DROPPED — rendering not validated \
(run `--report lowering-agreement` for the disagreeing instances). A solver verdict on an \
unvalidated rendering is not evidence about the obligation."
  pure kept

/-- Obligation ids whose lowering was POSITIVELY VALIDATED: the agreement lemma was
    closed, so the emitted goal provably has the same truth table as the reference
    evaluator on the sampled grid.

    Validation must be asserted, never inferred from the absence of a refusal. An
    agreement run can end in `error` (our emitted script was malformed, a library was
    missing, the tool died) or omit a key entirely, and in both cases we know NOTHING
    about whether that rendering denotes the obligation. Treating "no refusal" as
    "validated" hands the badge to a lowering whose check never ran — fail-open in the
    one place the composition exists to fail closed. Found by an external review,
    2026-08-01; the previous code keyed only on `.refused` while the comment beside it
    claimed unvalidated lowerings were excluded. -/
def validatedKeysOf (r : Option (List (String × KernelVerdict))) : List String :=
  (r.getD []).filterMap (fun (k, v) => if v == .closed then some k else none)

/-- What the external kernels told us about one module set. A named record rather than
    nested tuples: this was `(((closed),(validated)),(refused))`, which is unreadable at
    the call site and made it easy to wire the wrong pair — and the whole point of the
    witness change is that a caller cannot accidentally supply the wrong fact.

    `*Validated` are the ids whose AGREEMENT lemma closed. They are what mints a
    `LoweringValidated` witness; the closed/refused sets alone can never do that, which is
    what stops a consumer asserting a validation it did not perform. -/
structure ExternalKernelEvidence where
  rocqClosed    : List String := []
  isaClosed     : List String := []
  rocqRefused   : List String := []
  isaRefused    : List String := []
  rocqValidated : List String := []
  isaValidated  : List String := []

/-- R-0465: collapse one kernel's two id lists into per-obligation cells, which is what
    `foldMultiKernelResults` now takes. The lists are disjoint by construction upstream —
    both come from a single `List (String × KernelVerdict)`, and a verdict is one or the
    other — but that fact lived in the producer, not the type.

    **Refusals are listed FIRST, so `find?` resolves any overlap to `.refused`.** The order
    only matters if the upstream invariant is ever broken, which is exactly when a default
    must be chosen deliberately: a kernel that said no is the safety-relevant fact, and
    resolving to `.closed` would turn a broken invariant into a badge. Fail-closed. -/
def kernelCells (closed refused : List String) : List (String × Report.KernelCell) :=
  refused.map (fun k => (k, Report.KernelCell.refused))
    ++ closed.map (fun k => (k, Report.KernelCell.closed))

/-- Run the enabled external kernels over the multi-kernel obligation goals and
    return the VC ids each independently closed, PLUS the ids whose lowering failed
    the agreement check. Shared by the multi-kernel report, the ledger fold and the
    release gate so every surface composes the same two facts.

    Why both: closing a goal is only evidence about the OBLIGATION if the goal
    actually denotes the obligation. Those are separate checks, and running the first
    without the second is how a corrupted operator column earns
    `proved_by_two_kernels` — the kernel really did close a proposition, just not this
    one. `--report lowering-agreement` existed for this; nothing consulted it, so the
    badge and the release gate certified lowerings they had never validated. -/
def externalKernelFacts (modules : List Concrete.Module) (rocqRun isaRun : Bool)
    : IO ExternalKernelEvidence := do
  -- Bind each discharge ONCE and derive both fact sets from it. Refusals are needed so
  -- the LEDGER can express a verdict disagreement rather than only the report; running
  -- the discharge a second time to collect them would double the prover cost for facts
  -- already in hand.
  let rocqRes ← if rocqRun then rocqDischarge (Report.rocqReplayGoals modules) else pure none
  let isaRes ← if isaRun then isabelleDischarge (Report.isabelleReplayGoals modules) else pure none
  let rocqClosed := closedKeysOf rocqRes
  let isaClosed := closedKeysOf isaRes
  let rocqRefused := refusedKeysOf rocqRes
  let isaRefused := refusedKeysOf isaRes
  -- Agreement is checked with the SAME discharge path, so a malformed agreement script
  -- reads as `error` (our bug). We therefore collect what was POSITIVELY VALIDATED
  -- rather than what was refused: an `error`, or a key the agreement run never reached,
  -- leaves us knowing nothing about that rendering, and "we did not learn it is wrong"
  -- is not "we checked it". Returning the validated set makes the caller's filter
  -- fail closed by construction.
  let rocqOk ← if rocqRun then do
      let r ← rocqDischarge (Report.rocqAgreementGoals modules); pure (validatedKeysOf r)
    else pure []
  let isaOk ← if isaRun then do
      let r ← isabelleDischarge (Report.isabelleAgreementGoals modules); pure (validatedKeysOf r)
    else pure []
  pure { rocqClosed, isaClosed, rocqRefused, isaRefused
       , rocqValidated := rocqOk, isaValidated := isaOk }

/-- Attestation sets restricted to lowerings that were POSITIVELY VALIDATED — the
    composed fact every badge and gate should rest on.

    Note the direction: keep what was validated, rather than drop what was refused. The
    two differ exactly on the cases where validation did not produce an answer (`error`,
    a key the agreement run never reached), and those are the cases where we know
    nothing. Keeping-what-was-validated is fail-closed; dropping-what-was-refused was
    fail-open and is what this function used to do.

    Refusals are filtered the same way: a kernel whose lowering was never validated did
    not dissent about THIS obligation either, so its refusal is no more admissible than
    its closure. Such a kernel neither attests nor dissents, which is the honest
    reading. -/
def externalClosedSets (modules : List Concrete.Module) (rocqRun isaRun : Bool)
    : IO ExternalKernelEvidence := do
  let e ← externalKernelFacts modules rocqRun isaRun
  -- The validated sets are carried through UNFILTERED because they are what mints the
  -- witnesses downstream. Filtering closed/refused here as well is belt-and-braces: a
  -- consumer that forgot to pass the validated sets would then get an empty attestation
  -- set rather than an unchecked one.
  pure { e with
         rocqClosed  := e.rocqClosed.filter  (fun k => e.rocqValidated.contains k)
       , isaClosed   := e.isaClosed.filter   (fun k => e.isaValidated.contains k)
       , rocqRefused := e.rocqRefused.filter (fun k => e.rocqValidated.contains k)
       , isaRefused  := e.isaRefused.filter  (fun k => e.isaValidated.contains k) }

/-- **R-0465 (5th part): the two-kernel release gate, read OFF the ledger.**

    `computeBelowTwoKernels` used to run its own prover discharge and rebuild the verdicts
    itself. Two consequences, and the second is the one that matters:

    - The provers ran twice per release build, once for the artifact and once for the gate.
    - The gate and the stored artifact could be built from SEPARATE prover runs. Nothing
      forced their transient results to agree — a timeout, a flaky `isabelle build`, or a
      version skew between the two runs and the release ships an artifact whose badges the
      gate never actually checked. Unifying the input is the only fix that scales; keeping
      two derivations in step by inspection is what R-0461 measured the cost of.

    So this folds multi-kernel results into the SAME ledger the policy quals come from, and
    reads the answer out of the resulting statuses. An obligation is below two kernels unless
    its VC carries a two-or-more badge. That follows from the firewall rather than from a
    reimplementation of the counting: `multiKernelAdapter.allowed` contains exactly the
    badges, so a VC holding one is a VC that passed admission with ≥2 attesters.

    Three cases fall out for free, each of which the old recount had to state:
    - `kernel_disagreement` is not a badge, so a dissent blocks. An unexplained disagreement
      means one of the two kernels is wrong and we do not know which.
    - A VC Lean never proved was outside `actsOn`, so it holds no badge and blocks.
    - A VC R-0461 CAPPED reads `assumed`, is outside `actsOn`, and blocks — so an obligation
      resting on an unproved invariant now fails `require-two-kernels` as well, which is the
      composition H23 was missing, arriving here at no extra cost.

    An obligation with no VC at all counts as below: fail-closed, because "we could not find
    what this claim is" is not evidence that two kernels agreed on it. -/
def belowTwoKernelsOf (modules : List Concrete.Module) (folded : List Report.VC) : List String :=
  (Report.multiKernelObligations modules).filterMap fun o =>
    match folded.find? (·.id == o.key) with
    | some v =>
      if v.status == "proved_by_two_kernels" || v.status == "proved_by_multi_kernel"
      then none else some o.key
    | none => some o.key

/-- Phase 3 #14: policy inputs derived from the ONE obligation ledger. Builds the
    discharged ledger (folding the external-SMT path when a solver-evidence stance
    is set, so `solver_trusted` is present), then projects the vacuous / assume /
    solver-trusted facts the release policy acts on — replacing the three
    `compute*Quals` side channels. Returns `(vacuousQuals, assumeQuals,
    solverTrustedQuals)`, dep-filtered exactly as before. -/
def computePolicyQuals (policy : Concrete.ProjectPolicy) (modules : List Concrete.Module)
    (depNames : List String) (locMap : Report.FnLocMap) (registry : Concrete.ProofRegistry) :
    IO (List String × List String × List String × List (String × List String)
        × List String × Bool) := do
  let dvcs ← computeVCsDischarged modules locMap registry
  -- fold the external-SMT path into the ledger only when a stance is set (matches
  -- the old computeSolverTrustedQuals gating; otherwise no solver runs).
  let dvcs ← if policy.solverEvidence.isEmpty then pure dvcs else do
    let smtGoals := Report.overflowSmtGoals modules
    if smtGoals.isEmpty then pure dvcs else do
      let replayGoals := Report.leanReplayGoals modules
      let dvcs := Report.markSmtEligible dvcs smtGoals replayGoals
      let solverId ← z3VersionId
      let results ← smtDischarge smtGoals 5
      -- A solver verdict counts only if the rendering it answered about denotes the
      -- obligation. Same composition as the kernel path; this is the release-policy side,
      -- so an unvalidated rendering must not reach `solver_trusted`.
      let agree ← smtAgreementCheck (Report.smtAgreementGoals modules)
      let results ← gateSmtOnValidation results (smtValidatedObligations agree)
      let dvcs := Report.foldSmtResults dvcs results solverId
      let trusted := dvcs.filterMap fun v => if v.status == "solver_trusted" then some v.id else none
      let rg := replayGoals.filter (fun (k, _) => trusted.contains k)
      let replayed ← leanReplayCheck rg
      pure (Report.foldReplayResults dvcs replayed)
  -- R-0465 (5th part): fold multi-kernel evidence into THIS ledger when the policy needs
  -- it, so `require-two-kernels` is answered from the same VCs the rest of the gate reads
  -- instead of from a second prover run. Only under the policy: coqc/isabelle are slow and
  -- a normal build must not silently start invoking them.
  let mut externalRan := false
  let dvcs ← if !policy.requireTwoKernels then pure dvcs else do
    let rocqAvail ← commandAvailable "coqc"
    let isaAvail ← commandAvailable "isabelle"
    externalRan := rocqAvail || isaAvail
    if !externalRan then pure dvcs else do
      -- externalClosedSets already drops kernels whose lowering disagreed with the
      -- reference evaluator, so the gate cannot certify a kernel that closed a different
      -- proposition than the obligation.
      let ev ← externalClosedSets modules rocqAvail isaAvail
      let lv ← leanToolchainId
      let rv ← if rocqAvail then coqVersionId else pure "rocq (not run)"
      let iv ← if isaAvail then isabelleVersionId else pure "isabelle (not run)"
      pure (Report.foldMultiKernelResults dvcs
              (kernelCells ev.rocqClosed ev.rocqRefused)
              (kernelCells ev.isaClosed ev.isaRefused)
              lv rv iv ev.rocqValidated ev.isaValidated)
  let ledger := Concrete.ObligationCore.ledgerOfVCs dvcs
  let keep := fun (q : String) => !depNames.any (fun d => q.startsWith (d ++ "."))
  let vac := (Concrete.ObligationCore.vacuousFunctions ledger).filter keep
  let asm := (Concrete.ObligationCore.assumeFunctions ledger).filter keep
  let st := Concrete.ObligationCore.solverTrustedIds ledger
  -- R-0461: obligations the cap demoted, with what they still owe. Projected from the SAME
  -- discharged ledger as the other three, so the gate and the reports cannot disagree.
  -- BUG 067: keyed on the DEBT, not on the status.
  --
  -- This read `v.status == "assumed" && !v.hypDebt.isEmpty`. When enough kernels attest,
  -- multiKernelVerdict promotes the status to proved_by_two_kernels/proved_by_multi_kernel,
  -- the VC falls out of this list, and forbid-assume never sees it — so an obligation
  -- resting on an UNPROVED HYPOTHESIS passed the release gate exactly when MORE provers were
  -- available. Measured: green locally with no external provers, red in CI with Rocq and
  -- Isabelle installed.
  --
  -- Kernel count cannot discharge a hypothesis. Each kernel closes ITS LOWERING of the same
  -- unproved premise, so N attestations of an unsound premise is not evidence the premise
  -- holds. hypDebt is derived from the obligation's hypotheses and the function's loop
  -- contracts alone — it does not move with kernel count — which makes it the right key and
  -- makes the cap dominate attestation, as multiKernelVerdict's own comment already says it
  -- must ("Kernel count never overrides that").
  let cap := dvcs.filterMap (fun v =>
    if !v.hypDebt.isEmpty then some (v.id, v.hypDebt) else none)
  let btk := if policy.requireTwoKernels then belowTwoKernelsOf modules dvcs else []
  return (vac, asm, st, cap, btk, externalRan)


-- REMOVED 2026-08-03 (R-0465, 5th part): `computeBelowTwoKernels`, which ran its own
-- prover discharge and rebuilt the multi-kernel verdicts to answer
-- `[policy] require-two-kernels`. `computePolicyQuals` now folds multi-kernel evidence into
-- the one ledger and `belowTwoKernelsOf` reads the answer off the resulting statuses.
--
-- Deleted rather than left unused, for the reason `disagreeingKeysOf` was: a second
-- derivation of the same fact, sitting one identifier away from the one in use, is worse
-- than dead code. The whole defect this closes is that a release gate and a stored artifact
-- could be computed from separate prover runs; keeping a function that does exactly that
-- available to the next caller re-arms it.
--
-- It also halves the prover cost of a `require-two-kernels` build, which used to invoke
-- coqc and isabelle twice.


/-- Render the contracts report plus the call-site obligation section AS A VIEW
    over the one discharged ObligationCore ledger (Phase 3 #15 / #18e).

    Instead of re-running the per-family discharge (the old duplicate path that
    `check_contracts_ledger_parity.sh` guards), this reads `computeVCsDischarged`
    — the exact same ledger `--report obligation-ledger` and policy consume — and
    slices the proved-key sets out of it: a key is omega-proved iff its discharged
    VC carries engine `omega`, bv-proved iff engine `bv_decide` (`dischargeVCs`
    sets the engine per adapter). Each render helper matches only its own family's
    keys, so passing the shared sets is byte-identical to the former separate
    discharge — now single-source, so the two surfaces cannot diverge. -/
def renderContracts (parsedModules : List Concrete.Module) (registry : Concrete.ProofRegistry)
    (locMap : Report.FnLocMap) : IO String := do
  let dvcs ← computeVCsDischarged parsedModules locMap registry
  let omegaProved := dvcs.filterMap fun v => if v.engine == "omega" then some v.id else none
  let bvProved    := dvcs.filterMap fun v => if v.engine == "bv_decide" then some v.id else none
  let obs := Report.callSiteObligations parsedModules
  -- renderCallSites wants bv-proved call sites as obligation INDICES, not keys.
  let bvCallIdx := (List.range obs.length).filter fun i =>
    match obs[i]? with | some o => bvProved.contains o.key | none => false
  let boundsObls := Report.boundsObligations parsedModules
  let divObls := Report.divObligations parsedModules
  let ovfObls := Report.overflowObligations parsedModules
  -- Every helper below is handed the SAME omega/bv proved-key sets and filters its
  -- own family's keys (loop / vacuity / assert / #pre / #bounds / #div / #ovf are
  -- disjoint key spaces). overflow alone needs both (omega vs bv rendering).
  return Report.contractsReport parsedModules registry omegaProved omegaProved
    ++ Report.renderCallSites obs bvCallIdx omegaProved
    ++ Report.renderAssertAssume parsedModules omegaProved
    ++ Report.renderBounds boundsObls omegaProved
    ++ Report.renderDiv divObls omegaProved
    ++ Report.renderOverflow ovfObls omegaProved bvProved

-- `findLakeWorkspace` moved to `Concrete.Proof.Replay` on 2026-08-16. It is part of answering
-- "which checker ran", so it belongs with the replay producer rather than beside the CLI: a second
-- caller needing a workspace would otherwise have copied it, and two answers to "where does this
-- replay" is exactly the drift slice 4 was about.

/-- Run pipeline and check a profile constraint.
    If the input file lives inside a `Concrete.toml` project, route
    through project mode so std and other dependencies resolve. -/
def compileAndReport (inputPath : String) (reportType : String)
    (proveTarget : Option String := none) (proveOut : Option String := none)
    (proveForce : Bool := false) (proveEmitLink : Bool := false)
    (proveShowObl : Option String := none) (proveReplay : Bool := false)
    (proveJson : Bool := false) (proveNearestLemmas : Bool := false)
    (proveEmitLean : Bool := false) (proveStdout : Bool := false)
    (proveEmitArtifacts : Bool := false) (proveOutDir : Option String := none)
    (proveCheck : Bool := false) (proveWorkspace : Option String := none)
    (proveNearestId : Option String := none) (reportJson : Bool := false)
    (smtRun : Bool := false) (smtEmit : Bool := false)
    (smtReplay : Bool := false) (emitLeanReplay : Bool := false)
    (smtTimeoutMs : Option Nat := none) (rocqRun : Bool := false)
    (isaRun : Bool := false) (reqTwoKernels : Bool := false)
    -- Receipt storage. `receiptsOut` is where `--report receipts` writes; `receiptsIn` is the store
    -- `--report proof-status` reads back and CHECKS. They are separate because writing is an
    -- authority-bearing act and reading is consuming untrusted bytes.
    (receiptsOut : Option String := none) (receiptsIn : Option String := none) : IO UInt32 := do
  -- The second-kernel flags only affect the multi-kernel report; warn rather than
  -- silently ignore them elsewhere (finding: silent no-op reads as "it ran").
  -- Three cases, not two. Saying "ignored" for a report that then visibly runs coqc
  -- misinforms about the very thing this warning exists to prevent.
  let flagSelected := ["multi-kernel", "obligation-ledger", "lowering-agreement"]
  -- These invoke an external prover UNCONDITIONALLY, so the flag is redundant rather
  -- than ignored: `core-semantics-diff` discharges its equations with coqc, and
  -- `solver-cert` corroborates with Rocq `nia`, whether or not --rocq was passed.
  let proverAlways := ["core-semantics-diff", "solver-cert"]
  if (rocqRun || isaRun) && !flagSelected.contains reportType then
    if proverAlways.contains reportType then
      IO.eprintln s!"note: `--report {reportType}` already uses an external prover unconditionally; --rocq/--isabelle are redundant here, not ignored"
    else
      IO.eprintln s!"warning: --rocq/--isabelle select kernels for `--report {"|".intercalate flagSelected}`; ignored for `--report {reportType}`"
  let source ← readFile inputPath
  let mainSrcMap : SourceMap := [(inputPath, source)]
  -- Interface report only needs parse + resolveFiles + summary
  if reportType == "interface" then
    match Pipeline.parse source with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := mainSrcMap))
      return 1
    | .ok parsed =>
    match ← Pipeline.resolveFiles (dirOf inputPath) parsed inputPath resolveAllModules with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := mainSrcMap))
      return 1
    | .ok (resolved, _) =>
      let summary := Pipeline.buildSummary resolved
      IO.println (Report.interfaceReport summary.entries)
      return 0
  -- All other reports need the full frontend. If the file is inside a
  -- Concrete.toml project, route through project mode so dependency imports
  -- (e.g. std) resolve; mirrors compileAndCheck.
  let inputDir := dirOf inputPath
  -- Returns (parsed, fullValidCore, scopedValidCore, srcMap). In project
  -- mode the two validCores differ: full covers the entire user package
  -- (used for pc/registry validation, so sibling-file entries don't
  -- appear "unknown"), scoped covers just the file the user invoked
  -- (used to drive report output). In standalone mode they're identical.
  -- THE PACKAGE IDENTITY TRAVELS WITH THE FRONTEND RESULT, and it must: this path used to destructure
  -- the project context, DROP `ctx.packageIdentity`, and then synthesize an identity from the invoked
  -- file's text alone. The same program therefore had two identities — one when checked as a project,
  -- another when reported as a file — so generated attestations matched one and not the other, and
  -- `elf_header` reported 5 proved through the file path and 4 through the project path. One producer
  -- means one answer; carrying it here is what makes that true rather than intended.
  let frontendResult : Except UInt32
      (ParsedProgram × ValidatedCore × ValidatedCore × SourceMap ×
       Except Proof.PackageIdentityRefusal Proof.PackageIdentity × List CModule ×
       List (String × String × String)) ←
    match ← findProjectRoot inputDir with
    | some root =>
      match ← loadProject root with
      | .error ec => pure (Except.error ec)
      | .ok ctx =>
        let { validCore, parsed, allSrcMap, depNames, packageIdentity, filePackages, .. } := ctx
        let userModules := validCore.coreModules.filter fun m => !depNames.contains m.name
        let fullValidCore : ValidatedCore := { validCore with coreModules := userModules }
        let scopedModules ← scopeUserModulesToFile userModules inputPath root
        let scopedValidCore : ValidatedCore := { validCore with coreModules := scopedModules }
        -- The dependency modules travel too (R-0484 R10): the unsafe report lists the
        -- foreign-binding assumptions a program inherits from them.
        let depModules := validCore.coreModules.filter fun m => depNames.contains m.name
        pure (Except.ok (parsed, fullValidCore, scopedValidCore, allSrcMap, packageIdentity, depModules, filePackages))
    | none =>
      match ← Pipeline.runFrontend inputPath source resolveAllModules with
      | .error ds =>
        IO.eprintln (renderDiagnostics ds (sourceMap := mainSrcMap))
        pure (Except.error 1)
      | .ok (parsed, _, validCore, srcMap) =>
        -- STANDALONE: synthetic over the COMPLETE resolved source map, not the invoked file alone.
        -- Hashing one file made two programs sharing that file — and differing everywhere else —
        -- the same package, which is the collision content-binding exists to prevent.
        pure (Except.ok (parsed, validCore, validCore, srcMap,
          Proof.PackageIdentity.syntheticForModules (validCore.coreModules.map (·.name))
            (srcMap.map (·.2)), ([] : List CModule), ([] : List (String × String × String))))
  match frontendResult with
  | .error ec => return ec
  | .ok (parsed, fullValidCore, scopedValidCore, projSrcMap, packageIdentity, depModules, filePackages) =>
    -- R-0484 R10: assumption identities are package-scoped. Each source file maps to its
    -- package's canonical identity; standalone mode has one (synthetic) package.
    let assumptionPkgDefault : String × String := match packageIdentity with
      | .ok pid => (pid.digest, "program")
      | .error _ => ("unscoped:program", "program")
    let assumptionPackageOf : String → Option (String × String) := fun f =>
      (filePackages.find? (·.1 == f)).map fun (_, k, n) => (k, n)
    -- THE REPORT'S SOURCE MAP MUST CONTAIN THE KEY ITS LOCATION MAP USES. `buildFnLocMap` records
    -- every function under `inputPath` — the path the user typed — while a project's `allSrcMap` is
    -- keyed by the resolved, absolute entry path. Those are the same file under two names, and a
    -- `List` lookup does not know that: every snippet lookup missed, so source snippets and their
    -- caret lines disappeared from every report in project mode while the locations above them kept
    -- printing correctly. Invisible, because a missing snippet renders as "".
    --
    -- Surfaced by the `findProjectRoot` repair rather than caused by it: before that fix, running
    -- from inside a project silently fell back to standalone mode, where the one key is `inputPath`
    -- and the two agreed by accident. Correcting project detection is what made the mismatch reachable.
    --
    -- Prepended, so the invoked file resolves under the name the user gave it; the project map still
    -- backs every other file. Not a fuzzy or suffix-tolerant lookup — that would paper over a real
    -- key mismatch instead of supplying the key.
    let srcMap := mainSrcMap ++ projSrcMap
    let locMap := Report.buildFnLocMap parsed.modules inputPath
    let simpleLocMap := locMap.map fun e => (e.qualName, (e.file, e.fnSpan.line))
    -- The registry is the in-source proof links (#[proof_by]/#[spec]/...)
    -- synthesized from the FULL user package.
    let registry := Report.synthesizeSourceLinks parsed.modules fullValidCore.coreModules
    -- pc and registry validation run on the FULL user package: a
    -- registry entry naming a function defined in a sibling file must
    -- still validate when the user is querying just one file.
    -- STANDALONE CLI PATH: no manifest in scope, so the package identity is synthetic over the
    -- module inventory, via the single producer. A refusal ABORTS rather than substituting a
    -- placeholder: every definition identity minted from this ProofCore is package-scoped, and an
    -- "unidentified" fallback would be shared by every such compilation, reintroducing the
    -- collision this migration removes.
    let pc ← match extractProofCore? fullValidCore packageIdentity simpleLocMap registry with
      | .ok pc => pure pc
      | .error w => IO.eprintln s!"error: {w.explain}"; return 1
    -- Report output still iterates only the scoped modules.
    let validCore := scopedValidCore
    -- Validate registry against ProofCore and surface warnings/errors
    let regIssues := Concrete.validateRegistry pc registry
    for issue in regIssues do
      IO.eprintln (Concrete.renderRegistryIssue issue)
    let hasRegistryErrors := regIssues.any (·.isError)
    -- `concrete prove <function>`: read-only per-function proof scaffold.
    -- Writes nothing unless --out is given (and then refuses to clobber).
    if let some target := proveTarget then
      match Report.proveResolve pc target with
      | .error msg => IO.eprintln msg; return 1
      | .ok qual =>
        -- --emit-link: print the in-source proof-link block (text or JSON).
        if proveEmitLink then
          IO.println (if proveJson then Report.emitProofLinkJson registry qual inputPath
                      else Report.emitProofLink registry qual)
          return 0
        let provedVCs ← kernelDischargeLoopVCs (Report.loopVCGoals parsed.modules)
        -- --nearest-lemmas: proof-recipe hints per obligation kind + features.
        if proveNearestLemmas then
          IO.println (Report.nearestLemmas pc parsed.modules qual provedVCs proveJson proveNearestId)
          return 0
        -- --emit-lean: compilable single-function Lean proof stub (ends in `sorry`).
        if proveEmitLean then
          let stub := Report.emitLeanStub pc registry parsed.modules qual provedVCs
          match proveOut with
          | some path =>
            if proveStdout then IO.println stub; return 0
            if (← System.FilePath.pathExists path) && !proveForce then
              IO.eprintln s!"refusing to overwrite existing '{path}' (pass --force to clobber)."
              return 1
            if let some parent := (System.FilePath.mk path).parent then
              IO.FS.createDirAll parent
            IO.FS.writeFile path stub
            IO.println s!"wrote Lean proof stub for {qual} to {path}"
            return 0
          | none => IO.println stub; return 0
        -- --emit-artifacts: write a reproducible bundle per failed obligation.
        if proveEmitArtifacts then
          -- R-0479: NOT `.getD "missing"`. `missing` is a real canonical status, so a failed lookup
          -- would render identically to a found obligation that is genuinely missing.
          let proveStatus := (pc.obligations.find? (·.functionId.qualName == qual)).map (·.status.canonical)
                               |>.getD Concrete.noObligationRecord
          let obs := Report.callSiteObligations parsed.modules
          let myCands := ((List.range obs.length).zip obs).filterMap fun (i, o) =>
            if o.caller == qual then o.leanGoal.map (fun g => (i, g)) else none
          let bvProved ← bvDischargeCallSites myCands
          let failingCalls := myCands.filter (fun (i, _) => !bvProved.contains i)
          let arts := Report.proveArtifacts pc registry parsed.modules qual inputPath provedVCs failingCalls proveStatus
          if arts.isEmpty then
            IO.println s!"no failed obligations for {qual} (nothing to emit)."
            return 0
          let baseDir := (proveOutDir.getD ".build/prove") ++ "/" ++ Report.artifactDirName qual
          for a in arts do
            let dir := baseDir ++ "/" ++ a.dirName
            IO.FS.createDirAll dir
            IO.FS.writeFile (dir ++ "/context.json") a.context
            IO.FS.writeFile (dir ++ "/failed.lean") a.failedLean
            IO.FS.writeFile (dir ++ "/command.txt") a.command
            IO.FS.writeFile (dir ++ "/README.txt") a.readme
            IO.println s!"wrote {dir}/  (context.json, failed.lean, command.txt, README.txt)"
          IO.println s!"{arts.length} failed-obligation artifact(s) under {baseDir}/"
          return 0
        -- --check: run the Lean kernel on this function's linked theorem(s) and
        -- map the result back to obligation id / theorem / status / span / error.
        -- The closed repair loop: agents read structured status, not raw stderr.
        if proveCheck then
          let q := Report.jsonStr
          -- (obligationId, theorem, originalObligationStatus) for this function
          let mut checks : List (String × String × String) := []
          for o in pc.obligations do
            if o.functionId.qualName == qual then
              match o.spec with
              | some s =>
                if !checks.any (fun (_, t, _) => t == s.proofName) then
                  checks := checks ++ [(s!"{qual}#refines_spec", s.proofName, o.status.canonical)]
              | none => pure ()
          for e in registry do
            if e.function == qual then
              match e.ensuresProof with
              | some t => if !checks.any (fun (_, tt, _) => tt == t) then
                            checks := checks ++ [(s!"{qual}#ensures", t, "ensures")]
              | none => pure ()
          let srcLine := ((parsed.modules.flatMap Report.allFunctions).find?
            (fun (pfx, fn) => pfx ++ fn.name == qual)).map (fun (_, fn) => fn.span.line) |>.getD 0
          let stubCmd := s!"concrete prove {inputPath} {qual} --emit-lean --out failed.lean --force"
          -- No linked theorem: report missing_theorem without invoking Lean.
          if checks.isEmpty then
            if proveJson then
              IO.println (String.join ["{\n  \"function\": ", q qual, ",\n  \"schema_version\": ", q proveSchemaVersion,
                ",\n  \"all_checked\": false,\n  \"checks\": [\n",
                "    {\"obligation_id\": ", q s!"{qual}#refines_spec", ", \"theorem\": null, \"status\": \"missing_theorem\"",
                ", \"source_line\": ", toString srcLine, ", \"stub_command\": ", q stubCmd, "}\n  ],\n",
                "  \"lean_error\": ", q "no #[proof_by]/#[ensures_proof] theorem linked on this function",
                ",\n  \"summary\": ", q "0 checked, 1 missing theorem", "\n}"])
            else
              IO.println s!"=== concrete prove --check: {qual} ===\n\n  ✗ {qual}#refines_spec — no linked theorem (missing_theorem)\n\nSummary: 0 checked, 1 missing theorem."
            return 2
          -- Invoke the Lean kernel once on all linked theorems.
          let tmp ← IO.Process.output { cmd := "mktemp", args := #["-d"] }
          let tmpPath := tmp.stdout.trimAscii.toString ++ "/prove_check.lean"
          -- `import Examples` too: flagship example proofs live in the separate
          -- `Examples` Lake library, not the compiler umbrella (see check-proofs).
          let mut src := "import Concrete\nimport Examples\n\n-- Auto-generated by `concrete prove --check`.\n"
          for (_, thm, _) in checks do src := src ++ s!"#check @{thm}\n"
          IO.FS.writeFile ⟨tmpPath⟩ src
          let r ← IO.Process.output { cmd := "lake", args := #["env", "lean", tmpPath], env := #[("LAKE_TERM_ANSI", "0")] }
          let combined := r.stdout.trimAscii.toString ++ "\n" ++ r.stderr.trimAscii.toString
          let _ ← IO.Process.output { cmd := "rm", args := #["-rf", tmp.stdout.trimAscii.toString] }
          let has := fun (s sub : String) => (s.splitOn sub).length != 1
          let anyNeedle := checks.any (fun (_, thm, _) => has combined s!"`{thm}`")
          let envFailure := r.exitCode != 0 && !anyNeedle
          let results := checks.map fun (id, thm, st) =>
            let failed := has combined s!"`{thm}`"
            let status :=
              if envFailure then "env_failure"
              else if failed then (if has combined "unknown" then "missing_theorem" else "failed")
              else if st == "stale" then "stale"
              else "checked"
            (id, thm, status)
          let allChecked := results.all (fun (_, _, st) => st == "checked")
          let exitCode : UInt32 :=
            if envFailure then 5
            else if results.any (fun (_, _, st) => st == "failed") then 4
            else if results.any (fun (_, _, st) => st == "missing_theorem") then 2
            else if results.any (fun (_, _, st) => st == "stale") then 3
            else 0
          let leanErr := if allChecked then "" else String.ofList (combined.toList.take 800)
          if proveJson then
            let body := results.map fun (id, thm, st) =>
              String.join ["    {\"obligation_id\": ", q id, ", \"theorem\": ", q thm, ", \"status\": ", q st,
                ", \"source_line\": ", toString srcLine, ", \"stub_command\": ", q stubCmd, "}"]
            IO.println (String.join ["{\n  \"function\": ", q qual, ",\n  \"schema_version\": ", q proveSchemaVersion,
              ",\n  \"toolchain\": ", q ((← (try IO.FS.readFile ⟨"lean-toolchain"⟩ catch _ => pure "unknown")).trimAscii.toString),
              ",\n  \"all_checked\": ", (if allChecked then "true" else "false"),
              ",\n  \"checks\": [\n", ",\n".intercalate body, "\n  ],\n",
              "  \"lean_error\": ", q leanErr,
              ",\n  \"summary\": ", q s!"{results.countP (fun (_,_,s) => s == "checked")} checked, {results.countP (fun (_,_,s) => s != "checked")} not checked",
              "\n}"])
          else
            let mut out := s!"=== concrete prove --check: {qual} ===\n\n"
            for (id, thm, st) in results do
              let mark := if st == "checked" then "✓" else "✗"
              out := out ++ s!"  {mark} {id} — {thm} ({st})\n"
            if !leanErr.isEmpty then out := out ++ s!"\nLean output:\n{leanErr}\n"
            out := out ++ s!"\nSummary: {results.countP (fun (_,_,s) => s == "checked")} checked, {results.countP (fun (_,_,s) => s != "checked")} not checked"
            IO.println out
          return exitCode
        -- --workspace DIR: compose the read-only surfaces into one directory.
        if let some wsArg := proveWorkspace then
          let dir := if wsArg.isEmpty then s!".build/prove/{Report.artifactDirName qual}/workspace" else wsArg
          let files := Report.workspaceFiles pc registry parsed.modules qual inputPath proveSchemaVersion provedVCs
          IO.FS.createDirAll dir
          for (rel, contents) in files do
            let path := dir ++ "/" ++ rel
            if let some parent := (System.FilePath.mk path).parent then
              IO.FS.createDirAll parent
            IO.FS.writeFile path contents
            if rel.endsWith ".sh" then
              let _ ← IO.Process.output { cmd := "chmod", args := #["+x", path] }
          IO.println s!"wrote proof workspace for {qual} to {dir}/ ({files.length} files)"
          return 0
        -- --show-obligation <id>: print one obligation in full (text or JSON).
        if let some oblId := proveShowObl then
          IO.println (if proveJson then Report.showObligationJson parsed.modules qual oblId provedVCs inputPath
                      else Report.showObligation parsed.modules qual oblId provedVCs)
          return 0
        -- --replay: re-run omega / bv_decide discharge and report per obligation.
        if proveReplay then
          let myLoop := (Report.loopVCGoals parsed.modules).filter (fun (k, _) => k.startsWith (qual ++ "@"))
          let obs := Report.callSiteObligations parsed.modules
          let myCands := ((List.range obs.length).zip obs).filterMap fun (i, o) =>
            if o.caller == qual then o.leanGoal.map (fun g => (i, g)) else none
          let bvProved ← bvDischargeCallSites myCands
          if proveJson then
            let q := Report.jsonStr
            let loopObs := myLoop.map fun (k, _) =>
              s!"\{\"id\": {q k}, \"engine\": \"omega\", \"closes\": {if provedVCs.contains k then "true" else "false"}}"
            let callObs := myCands.map fun (i, _) =>
              s!"\{\"id\": {q s!"{qual}#call{i}"}, \"engine\": \"bv_decide\", \"closes\": {if bvProved.contains i then "true" else "false"}}"
            let allPass := myLoop.all (fun (k, _) => provedVCs.contains k) && myCands.all (fun (i, _) => bvProved.contains i)
            IO.println (String.join [
              "{\n",
              s!"  \"function\": {q qual},\n",
              s!"  \"all_pass\": {if allPass then "true" else "false"},\n",
              s!"  \"obligations\": [{", ".intercalate (loopObs ++ callObs)}],\n",
              s!"  \"next_actions\": [\{\"kind\": \"run_audit\", \"command\": {q s!"concrete audit {inputPath}"}, \"output_format\": \"text\", \"resolves\": {if allPass then "\"proved\"" else "\"stale\""}}]\n",
              "}" ])
            return (if myLoop.all (fun (k, _) => provedVCs.contains k) && myCands.all (fun (i, _) => bvProved.contains i) then 0 else 4)
          let mut out := s!"=== concrete prove --replay: {qual} ===\n"
          out := out ++ "\nloop obligations (omega):\n"
          if myLoop.isEmpty then out := out ++ "  (none)\n"
          for (k, _) in myLoop do
            out := out ++ (if provedVCs.contains k then s!"  ok   {k} — still closes\n" else s!"  FAIL {k} — no longer closes\n")
          out := out ++ "\ncall-site obligations (bv_decide):\n"
          if myCands.isEmpty then out := out ++ "  (none)\n"
          for (i, _) in myCands do
            out := out ++ (if bvProved.contains i then s!"  ok   call obligation #{i} — still closes\n" else s!"  FAIL call obligation #{i} — no longer closes\n")
          IO.println out
          return 0
        -- Process exit code follows the documented taxonomy (see --help=agent):
        --   0 proved/clean · 2 obligations missing · 3 stale evidence.
        -- R-0479: see the note at the other `--emit-artifacts` site. A failed lookup must not
        -- render as `missing`, which is a real status.
        let proveStatus := (pc.obligations.find? (·.functionId.qualName == qual)).map (·.status.canonical)
                             |>.getD Concrete.noObligationRecord
        let proveExit : UInt32 := match proveStatus with
          | "stale" => ExitCode.staleEvidence
          | "missing" => ExitCode.obligationsMissing
          | "blocked" => ExitCode.obligationsMissing
          | _ => ExitCode.ok
        -- --json: structured proof context + next_actions.
        if proveJson then
          IO.println (Report.proveReportJson pc registry parsed.modules qual provedVCs inputPath proveSchemaVersion)
          return proveExit
        let report := Report.proveReport pc registry parsed.modules qual provedVCs
        match proveOut with
        | none => IO.println report; return proveExit
        | some path =>
          if (← System.FilePath.pathExists path) && !proveForce then
            IO.eprintln s!"refusing to overwrite '{path}' (pass --force to replace it)"
            return 1
          writeFile path report
          IO.println s!"wrote proof scaffold to {path}"
          return 0
    if reportType == "backend-contracts" then
      -- The guarantees the code generator commits to (Phase 4 #17a), rendered from
      -- the same Concrete.Backend facts the emitter uses, plus this program's
      -- trusted boundaries. Human + JSON from one source.
      if reportJson then IO.println (Concrete.Backend.toJson validCore.coreModules 1)
      else IO.println (Concrete.Backend.report validCore.coreModules)
      return 0
    if reportType == "contracts" then
      IO.println (← renderContracts parsed.modules registry locMap)
      return 0
    if reportType == "obligation-ledger" then
      -- Phase 3: the unified ObligationCore ledger — the discharged VC families
      -- (runtime-safety, contracts incl. clause diagnostics) PLUS the proof-link
      -- freshness family (#11), projected into the one schema.
      let dvcs ← computeVCsDischarged parsed.modules locMap registry
      -- Opt-in: with --rocq/--isabelle, fold independent-kernel agreement into the
      -- ONE ledger so `proved_by_two_kernels`/`proved_by_multi_kernel` are produced
      -- by a real ledger path (not only the multi-kernel report). Default unchanged.
      let dvcs ← if rocqRun || isaRun then do
          let ev ← externalClosedSets parsed.modules rocqRun isaRun
          -- Record WHICH prover builds attested, not just that some did.
          let lv ← leanToolchainId
          let rv ← if rocqRun then coqVersionId else pure "rocq (not run)"
          let iv ← if isaRun then isabelleVersionId else pure "isabelle (not run)"
          -- Validated sets are passed EXPLICITLY: the fold mints witnesses from them and
          -- attests nothing without one. Omitting them yields no attestations rather than
          -- unchecked ones.
          -- R-0450: validate Lean's OWN rendering before trusting the badges it underwrites.
          -- Costs an omega pass, no external toolchain, and it is the rendering every other
          -- kernel's goal is derived from — so it is the one whose fault no amount of
          -- kernel agreement could reveal.
          let leanAgree := Report.leanAgreementGoals parsed.modules
          let leanOk ← if leanAgree.isEmpty then pure none else do
            let closed ← kernelDischargeLoopVCs leanAgree
            pure (some (Report.leanValidatedObligations leanAgree closed))
          pure (Report.foldMultiKernelResults dvcs
                  (kernelCells ev.rocqClosed ev.rocqRefused)
                  (kernelCells ev.isaClosed ev.isaRefused)
                  lv rv iv ev.rocqValidated ev.isaValidated leanOk)
        else pure dvcs
      let vcLedger := Concrete.ObligationCore.ledgerOfVCs dvcs
      let proofLinks := Concrete.ObligationCore.proofLinkLedger
        (Report.proofStatusEntries validCore.coreModules locMap registry pc)
      let ledger := vcLedger ++ proofLinks
      if reportJson then IO.println (Concrete.ObligationCore.ledgerJson ledger 1)
      else IO.println (Concrete.ObligationCore.ledgerReport ledger)
      return 0
    if reportType == "vcs" then
      let dvcs ← computeVCsDischarged parsed.modules locMap registry
      -- External-SMT path is OPT-IN. Without --smt/--emit-smt nothing here touches
      -- a solver and no VC ever advertises `smt` — the kernel boundary is the default.
      let smtGoals := Report.overflowSmtGoals parsed.modules
      let replayGoals := Report.leanReplayGoals parsed.modules
      if smtEmit then
        -- stable SMT-LIB output path: emit the scripts (no solver needed).
        if smtGoals.isEmpty then IO.println "; no SMT-eligible VCs (nonlinear arithmetic) in this file"
        for (k, script) in smtGoals do
          IO.println s!";; ==== {k} ====\n{script}\n"
        return 0
      if emitLeanReplay then
        -- Lean replay artifact: the same obligation as a kernel-checkable theorem.
        if replayGoals.isEmpty then IO.println "-- no SMT-eligible VCs to replay in this file"
        for (k, src) in replayGoals do
          IO.println s!"-- ==== {k} ====\n-- check: lake env lean vc_replay.lean\n{src}\n"
        return 0
      let dvcs := if smtRun || smtEmit then Report.markSmtEligible dvcs smtGoals replayGoals else dvcs
      let dvcs ← if smtRun then do
          let solverId ← z3VersionId
          let results ← smtDischarge smtGoals 5 smtTimeoutMs
          -- Same gate as the policy path: no verdict without a validated rendering.
          let agree ← smtAgreementCheck (Report.smtAgreementGoals parsed.modules)
          let results ← gateSmtOnValidation results (smtValidatedObligations agree)
          let dvcs := Report.foldSmtResults dvcs results solverId
          -- --replay: try to kernel-check each solver_trusted VC in Lean; a success
          -- graduates it to proved_by_lean_replay (solver dropped from the claim).
          if smtReplay then
            let trusted := dvcs.filterMap fun v => if v.status == "solver_trusted" then some v.id else none
            let rg := replayGoals.filter (fun (k, _) => trusted.contains k)
            let replayed ← leanReplayCheck rg
            pure (Report.foldReplayResults dvcs replayed)
          else pure dvcs
        else pure dvcs
      -- Phase 3 #18d: the VC schedule IS a list of obligations (one record type),
      -- so the VC reports render it directly — no conversion glue.
      if reportJson then
        IO.println (Report.vcsJson dvcs 1)
      else
        IO.println (Report.vcsReport dvcs (Report.unresolvedBoundsAccesses parsed.modules))
      return 0
    if reportType == "vcgen-diff" then
      -- SHADOW differential: the WP-style calculus (`Concrete/Report/VCGen.lean`) against the
      -- hand-written walkers, which are the oracle now that their nine defects are fixed and
      -- gated. Nothing consumes the calculus yet; this exists so that switching over is evidence-
      -- driven rather than a leap.
      let d := Report.VCGen.diff parsed.modules
      IO.println "=== VC-generator differential (calculus vs hand-written walkers) ==="
      let (nHere, nThere) := Report.VCGen.diffTotals parsed.modules
      IO.println s!"  compared {nHere} calculus requirement(s) against {nThere} walker obligation(s)"
      if nHere == 0 && nThere == 0 then
        IO.println "  VACUOUS: neither side found anything here — agreement means nothing"
      let only := Report.VCGen.calculusOnly parsed.modules
      if !only.isEmpty then
        IO.println s!"  CALCULUS-ONLY ({only.length}) — obligation kinds no walker can produce:"
        for (fq, what) in only.take 8 do
          IO.println s!"    {fq}: {what}"
        IO.println "    (these trap at runtime today with nothing proved; reported separately so"
        IO.println "     they cannot masquerade as a differential disagreement)"
      if d.isEmpty then
        IO.println "  AGREE on every function and family in this file"
      else
        for (fq, fam, onlyHere, onlyThere) in d do
          IO.println s!"  {fq} [{fam}]"
          if !onlyHere.isEmpty then
            IO.println s!"    only the CALCULUS found: {", ".intercalate onlyHere}"
          if !onlyThere.isEmpty then
            IO.println s!"    only the WALKER found:   {", ".intercalate onlyThere}"
      return 0
    if reportType == "bool-kernel" then
      -- Branch spike/non-arithmetic-multi-kernel: the first NON-arithmetic obligation carried to
      -- all three kernels. Boolean postconditions, closed by case analysis (`destruct` / `simp` /
      -- `decide`) rather than an arithmetic decision procedure -- so agreement here is not three
      -- invocations of the same idea. Emits each kernel's source; run them under
      -- `nix develop .#provers`.
      let obls := Report.boolPostObligations parsed.modules
      let skipped := Report.boolKernelSkipped parsed.modules
      let (nClauses, nRefine, nReached) := Report.coverageSummary parsed.modules
      IO.println s!"  COVERAGE: {nClauses} contract clause(s) in the parsed program (IMPORTS INCLUDED — not just this file), {nReached} reached a tier, {nRefine} are refinement obligations (`result == spec(…)`) that need the spec's DEFINITION and cannot be discharged with an opaque symbol."
      IO.println "=== Non-arithmetic multi-kernel evidence (boolean postconditions) ==="
      IO.println "  reference-evaluator agreement is EXHAUSTIVE here (all 2^n assignments),"
      IO.println "  not sampled as in the arithmetic tier."
      if obls.isEmpty then
        IO.println ""
        IO.println "(no boolean postcondition obligations in this file)"
      for o in obls do
        let ref := match o.holdsEverywhere with
          | some true => "holds on every assignment"
          | some false => "FALSE somewhere (a kernel closing it would be proving something else)"
          | none => "not evaluable (no attestation possible)"
        IO.println ""
        IO.println s!"  [{o.key}]  reference evaluator: {ref}"
        match o.leanGoal with
        | some g => IO.println s!"    lean:decide goal: {g}"
        | none => IO.println "    lean:decide outside the boolean fragment"
        match o.rocqScript with
        | some sc => IO.println s!"    --- rocq:destruct ---\n{sc}"
        | none => IO.println "    rocq:destruct outside the boolean fragment"
        match o.isabelleTheory "BoolPost" with
        | some th => IO.println s!"    --- isabelle:auto ---\n{th}"
        | none => IO.println "    isabelle:auto outside the boolean fragment"
      if !skipped.isEmpty then
        IO.println ""
        IO.println "ALL-BOOLEAN FUNCTIONS SKIPPED (coverage stated, not implied):"
        for (fn, why) in skipped do
          IO.println s!"  {fn}: {why}"
      -- Rung 5: uninterpreted functions. The symbol is a QUANTIFIED function variable in every
      -- kernel, never a Parameter or axiom, so `Print Assumptions` stays clean.
      let eufs := Report.eufObligations parsed.modules
      if !eufs.isEmpty then
        IO.println ""
        IO.println "=== EUF: uninterpreted spec functions (rung 5) ==="
        IO.println "  the reference check here is propositional abstraction: SOUND but"
        IO.println "  INCOMPLETE — a congruence-valid goal is not a propositional tautology."
        for o in eufs do
          let verdict := match o.abstractionVerdict with
            | some () => "tautology under abstraction — valid for EVERY interpretation"
            | none => "inconclusive (abstraction cannot refute: it forgets f is a function)"
          IO.println ""
          IO.println s!"  [{o.key}]  symbols: {", ".intercalate (o.symbols.map (·.1))}"
          IO.println s!"    abstraction: {verdict}"
          match o.leanScript with
          | some sc => IO.println s!"    --- lean:by_cases ---\n{sc}"
          | none => IO.println "    lean: outside the EUF fragment"
          match o.rocqScript with
          | some sc => IO.println s!"    --- rocq:congruence ---\n{sc}"
          | none => IO.println "    rocq: outside the EUF fragment"
          match o.isabelleTheory "EufGoal" with
          | some th => IO.println s!"    --- isabelle:auto (euf) ---\n{th}"
          | none => IO.println "    isabelle: outside the EUF fragment"
      -- Rungs 6+7: struct declarations are EMITTED with the goal (nothing told a prover a
      -- Concrete struct existed before), and arrays are total index -> value functions, which is
      -- sound because the bounds family proves every index in range.
      let sts := Report.structObligations parsed.modules
      if !sts.isEmpty then
        IO.println ""
        IO.println "=== DATATYPES + ARRAYS: struct fields and array reads (rungs 6+7) ==="
        for o in sts do
          let verdict := match o.abstractionVerdict with
            | some () => "tautology under abstraction — valid for EVERY interpretation"
            | none => "inconclusive (abstraction can confirm, never refute)"
          IO.println ""
          IO.println s!"  [{o.key}]  structs: {", ".intercalate (o.structs.map (·.1))}  arrays: {", ".intercalate o.arrays}"
          IO.println s!"    abstraction: {verdict}"
          match o.leanScript with
          | some sc => IO.println s!"    --- lean:by_cases (struct) ---\n{sc}"
          | none => IO.println "    lean: outside the fragment"
          match o.rocqScript with
          | some sc => IO.println s!"    --- rocq:record ---\n{sc}"
          | none => IO.println "    rocq: outside the fragment"
          match o.isabelleTheory "StructGoal" with
          | some th => IO.println s!"    --- isabelle:record ---\n{th}"
          | none => IO.println "    isabelle: outside the fragment"
      -- REFINEMENT against a DEFINED spec. This is the coverage fix: a body-less `spec fn` is
      -- uninterpreted, so `result == spec(args)` is unprovable outside Lean, where the spec's
      -- meaning lives. With a definitional body the obligation is an equation between two
      -- expressions of THIS language and every kernel can see it.
      let refs := Report.refineObligations parsed.modules
      let refSkip := Report.refineSkipped parsed.modules
      if !refs.isEmpty || !refSkip.isEmpty then
        IO.println ""
        IO.println "=== REFINEMENT against a defined spec ==="
        for o in refs do
          IO.println ""
          let triv := if o.trivial then
                "  *** TRIVIAL: the spec RESTATES the implementation, so this obligation is `body = body` and proves nothing ***"
              else ""
          IO.println s!"  [{o.key}]  spec: {o.specName} (defined){triv}"
          match o.leanScript with
          | some sc => IO.println s!"    --- lean:omega (refine) ---\n{sc}"
          | none => IO.println "    lean: outside the fragment"
          match o.rocqScript with
          | some sc => IO.println s!"    --- rocq:lia (refine) ---\n{sc}"
          | none => IO.println "    rocq: outside the fragment"
          match o.isabelleTheory "Refines" with
          | some th => IO.println s!"    --- isabelle:simp (refine) ---\n{th}"
          | none => IO.println "    isabelle: outside the fragment"
        if !refSkip.isEmpty then
          IO.println ""
          IO.println "REFINEMENT OBLIGATIONS NOT REACHABLE (coverage stated, not implied):"
          for (fn, why) in refSkip do
            IO.println s!"  {fn}: {why}"
      return 0
    if reportType == "multi-kernel" then
      -- Spike (branch spike/multi-prover-evidence): the prover-neutral obligation
      -- layer. The SAME linear no-overflow obligations are discharged by Lean's
      -- in-toolchain `omega` AND by each enabled EXTERNAL kernel — Rocq's `lia`
      -- (CIC) and Isabelle's `presburger` (HOL). Each kernel is a driver
      -- (`Report.ProverLowering`); adding a prover is data, not new plumbing. A VC
      -- Lean + ≥1 external kernel independently close on the same subject graduates
      -- to `proved_by_two_kernels` (n=2) / `proved_by_multi_kernel` (n≥3). Externals
      -- are OPT-IN (`--rocq`, `--isabelle`); an absent/off kernel never attests.
      -- ALL linear runtime-safety families (overflow / bounds / div), not just
      -- overflow — the prover-neutral layer covers every family via multiKernelObligations.
      let linear := Report.multiKernelObligations parsed.modules
      -- R-0461 / H23: the badge must not outrank the LEDGER. This report used to recompute
      -- its class from `omegaProved` alone, so an obligation the ledger had capped to
      -- `assumed` — because it rests on a loop invariant whose O1/O2 is unproven — still
      -- displayed `proved_by_multi_kernel` here. Two surfaces, two answers, and the wrong
      -- one was the louder. Consult the discharged ledger instead of re-deriving.
      let cappedVCs ← computeVCsDischarged parsed.modules locMap registry
      let cappedKeys := cappedVCs.filterMap (fun v =>
        if v.status == "assumed" && !v.hypDebt.isEmpty then some (v.id, v.hypDebt) else none)
      let omegaProved ← kernelDischargeLoopVCs
        (Report.overflowGoals parsed.modules ++ Report.boundsGoals parsed.modules ++ Report.divGoals parsed.modules ++ Report.divQuotGoals parsed.modules ++ Report.shiftGoals parsed.modules)
      -- External-kernel drivers, each: (name, displayLabel, enabled, versionId,
      -- emittedKeys, result). `emittedKeys` are the obligations the driver COULD
      -- lower (others are dropped as outside the fragment — "not asked"); `result`
      -- is `none` when the tool is absent (also "not asked") vs. `some closedKeys`
      -- when it ran. This three-valued model is what keeps "refused" distinct from
      -- "never asked" — the one thing a graded-evidence report must not conflate.
      let rocqGoals := Report.rocqReplayGoals parsed.modules
      let isaGoals := Report.isabelleReplayGoals parsed.modules
      let rocqId ← if rocqRun then coqVersionId else pure "not run (pass --rocq)"
      let rocqRes ← if rocqRun then rocqDischarge rocqGoals else pure none
      let isaId ← if isaRun then isabelleVersionId else pure "not run (pass --isabelle)"
      let isaRes ← if isaRun then isabelleDischarge isaGoals else pure none
      -- COMPOSITION: closing a goal is evidence about the obligation only if the goal
      -- denotes the obligation. Both facts are computed here and the badge rests on
      -- their conjunction. Previously `--report lowering-agreement` checked the second
      -- fact and nothing consulted it, so a corrupted operator column still earned
      -- `proved_by_two_kernels` — the kernel did close a proposition, just not this one.
      -- POSITIVELY VALIDATED, not "not refused". An agreement run that ends in `error`
      -- tells us nothing about that rendering, and treating "no refusal" as "checked" is
      -- what let a badge rest on an unvalidated lowering. `validatedKeysOf` keys on
      -- `.closed`, so absence of an answer excludes the kernel.
      let rocqOk ← if rocqRun then do
          let r ← rocqDischarge (Report.rocqAgreementGoals parsed.modules)
          pure (validatedKeysOf r)
        else pure []
      let isaOk ← if isaRun then do
          let r ← isabelleDischarge (Report.isabelleAgreementGoals parsed.modules)
          pure (validatedKeysOf r)
        else pure []
      let validatedOf := fun (name : String) => if name == "rocq" then rocqOk else isaOk
      let kernels : List (String × String × Bool × String × List String × Option (List (String × KernelVerdict))) :=
        [ ("rocq", "rocq:lia", rocqRun, rocqId, rocqGoals.map (·.1), rocqRes),
          ("isabelle", "isabelle:presburger", isaRun, isaId, isaGoals.map (·.1), isaRes) ]
      -- cell verdict for obligation key `k` under one external kernel. `error` is
      -- kept DISTINCT from `refused`: both come from a nonzero prover exit, but only
      -- `refused` is the kernel saying no about the goal — `error` means the script
      -- we emitted was malformed / the tool broke, i.e. OUR bug. Collapsing them
      -- would report a fake kernel disagreement, which reads exactly like the
      -- genuine signal this report exists to surface.
      let cellOf := fun (en : Bool) (emitted : List String)
                        (res : Option (List (String × KernelVerdict))) (k : String) =>
        if !en then "off"
        else match res with
          | none => "unavailable"
          | some verdicts =>
            if !emitted.contains k then "not-asked"
            else match verdicts.lookup k with
              | none => "not-asked"
              | some v => v.cell
      -- error details, surfaced separately so a broken lowering is loud, not a cell.
      let kernelErrors : List (String × String × String) :=
        kernels.flatMap (fun (_, label, en, _, _, res) =>
          if !en then [] else
          match res with
          | none => []
          | some verdicts => verdicts.filterMap (fun (k, v) =>
              match v with | .error d => some (label, k, d) | _ => none))
      let mut out := "=== Multi-kernel evidence (spike: prover-neutral obligation layer) ==="
      let mut belowTwo := 0
      out := out ++ "\n  lean:omega (in-toolchain kernel)"
      for (_, label, _, id, _, _) in kernels do out := out ++ s!"\n  {label}:  {id}"
      -- Honesty boundary: CHECKER diversity, not BRIDGE diversity. All kernels check
      -- their own lowering of the SAME obligation Expr from Concrete's single
      -- Core->VC bridge; that bridge is not diversified and stays trusted. There is
      -- no digest cross-check that the lowerings spell the same proposition.
      out := out ++ "\n  attests: N independent kernels each closed a lowering of the OBLIGATION whose"
      out := out ++ "\n           truth table matches the reference evaluator (composed here, per kernel)."
      out := out ++ "\n  does NOT attest: that the Core->obligation lowering itself is faithful —"
      out := out ++ "\n                   see `--report core-semantics-diff` for that axis.\n"
      out := out ++ "\n  cell legend: closed | refused (kernel says no) | error (OUR script/tool broke —"
      out := out ++ "\n               NOT a kernel disagreement) | not-asked (outside fragment) |"
      out := out ++ "\n               unavailable (no tool) | off (flag not set)"
      out := out ++ "\n  a kernel whose lowering DISAGREES with the reference evaluator does not attest,"
      out := out ++ "\n  even when it closed its goal: it proved a different proposition.\n"
      if linear.isEmpty then
        out := out ++ "\n(no linear runtime-safety obligations in this file)\n"
      let mut lowerBad := 0
      let mut disagreed := 0
      for o in linear do
        let leanOk := omegaProved.contains o.key
        -- Attesting kernels = lean (if omega closed) + externals that BOTH closed
        -- their goal AND whose lowering agreed with the reference evaluator. A kernel
        -- that closed a goal denoting something else contributes nothing.
        -- ONE derivation, shared with the ledger fold (Report.multiKernelVerdict), so
        -- this report and the stored artifact cannot disagree about the same
        -- obligation. They used to compute the class independently.
        -- R-0465: one shared constructor with the other two consumers. This surface learns
        -- what a kernel said from a driver result string rather than a verdict list, which is
        -- its own business; what happens NEXT — absent contributes nothing, the witness is
        -- minted — is not, and is no longer restated here.
        let inputs : List Report.KernelInput := kernels.flatMap (fun (name, label, en, _, em, res) =>
          Report.kernelInputOf o.key name label
            (match cellOf en em res o.key with
              | "closed"  => .closed
              | "refused" => .refused
              | _         => .absent)     -- off / unavailable / not-asked / error
            (validatedOf name))
        -- A capped obligation earns no badge, whatever the kernels said. The kernels did
        -- close their goals; the goals rest on something unestablished. Recorded as
        -- `assumed` for the same reason the ledger does — it is gate-forbiddable.
        let verdict := Report.multiKernelVerdict o.key leanOk inputs
        let attest := verdict.attest
        let n := attest.length
        if n < 2 then belowTwo := belowTwo + 1
        -- VERDICT disagreement, as distinct from LOWERING disagreement below. Two
        -- kernels that render the SAME proposition (lowering agrees) and return
        -- OPPOSITE verdicts have told us something no amount of agreement can: either
        -- this driver renders that goal wrongly for that kernel, or a decision
        -- procedure is wrong. For the linear fragment both `omega` and `lia`/
        -- `presburger` are COMPLETE, so a disagreement here is nearly always our bug —
        -- which is exactly why it must be loud rather than absorbed.
        --
        -- Only `closed`/`refused` count: those are verdicts. `off`, `unavailable`,
        -- `not-asked` and `error` are absences, and treating an absence as dissent is
        -- the conflation this report exists to prevent.
        --
        -- Kernels whose LOWERING disagreed are excluded — their verdict is about a
        -- different proposition, so it is not evidence about this one, and it is
        -- already reported on its own line.
        if !verdict.dissent.isEmpty then disagreed := disagreed + 1
        -- `present`, not `cls`: Register C's C3 guarantees a claim with outstanding
        -- assumptions renders as `assumed` rather than any `proved_*` string. Empty
        -- R-0461 made this non-empty: the ledger's outstanding debt is loaded into the
        -- verdict's evidence, so C3 does the capping and this surface cannot disagree
        -- with the ledger about whether the obligation is proved.
        let cls := match cappedKeys.find? (fun p => p.1 == o.key) with
          | some (_, debt) => (debt.foldl (fun (e : Report.Evidence) r => e.assuming r)
                                 verdict.evidence).present
          | none => verdict.displayStatus
        out := out ++ s!"\n  [{o.key}]  {o.desc}"
        out := out ++ s!"\n      lean:omega = {if leanOk then "closed" else "refused"}"
        for (_, label, en, _, em, res) in kernels do out := out ++ s!"   {label} = {cellOf en em res o.key}"
        -- Name the disqualified kernels explicitly. A silently-dropped attester would
        -- look identical to a kernel that simply could not close the goal.
        -- A kernel that ran, gave a verdict, and has NO validation witness. Previously
        -- this listed only kernels whose agreement lemma was refused, so a kernel whose
        -- agreement check errored was silently treated as fine and never named here.
        let disq := kernels.filterMap (fun (name, label, en, _, em, res) =>
          let c := cellOf en em res o.key
          if en && (c == "closed" || c == "refused") && !(validatedOf name).contains o.key
          then some label else none)
        if !disq.isEmpty then
          lowerBad := lowerBad + 1
          out := out ++ s!"\n      LOWERING DISAGREES ({", ".intercalate disq}) — closed a different"
          out := out ++ " proposition, so it does NOT attest"
        out := out ++ s!"\n      => {cls}"
      -- A kernel `error` is a defect in THIS tool (a malformed emitted script, a
      -- missing library, a broken install), never evidence about the program. Print
      -- the diagnostics so it gets fixed instead of silently degrading the report to
      -- "that kernel didn't attest".
      if !kernelErrors.isEmpty then
        out := out ++ s!"\n\n  ERRORS ({kernelErrors.length}) — these are OUR bugs, not kernel disagreements:"
        for (label, k, d) in kernelErrors do
          out := out ++ s!"\n    {label} [{k}]: {d}"
      if lowerBad > 0 then
        out := out ++ s!"\n\n  LOWERING DISAGREEMENTS: {lowerBad} obligation(s) where a kernel's rendering"
        out := out ++ "\n  does not denote the obligation. Those kernels were excluded from the badge."
        out := out ++ "\n  Run `--report lowering-agreement` for the per-instance detail."
      -- A verdict disagreement is the single most informative thing this report can
      -- produce, so it gets its own block and its own exit code rather than being a
      -- missing badge. `omega` and `lia`/`presburger` are all complete for linear
      -- integer arithmetic: if they differ, something here is WRONG, and shipping while
      -- it is unexplained means shipping a claim we cannot account for.
      if disagreed > 0 then
        out := out ++ s!"\n\n  KERNEL DISAGREEMENTS: {disagreed} obligation(s) where kernels rendering the"
        out := out ++ "\n  SAME proposition returned OPPOSITE verdicts. All these kernels are complete for"
        out := out ++ "\n  linear integer arithmetic, so this is a defect — most likely in our driver for"
        out := out ++ "\n  the dissenting kernel, possibly in a decision procedure. Investigate before"
        out := out ++ "\n  trusting any badge in this file: the same driver produced them all."
      -- Release gate: with --require-two-kernels, fail if any in-scope obligation
      -- did not reach ≥2 independent kernels. A genuine CI gate, not just a report.
      if reqTwoKernels && (belowTwo > 0 || disagreed > 0) then
        if disagreed > 0 then
          out := out ++ s!"\n\nGATE: --require-two-kernels FAILED — {disagreed} kernel disagreement(s)."
        if belowTwo > 0 then
          out := out ++ s!"\n\nGATE: --require-two-kernels FAILED — {belowTwo} obligation(s) below 2 independent kernels."
        IO.println (out ++ "\n")
        return 1
      if reqTwoKernels then out := out ++ "\n\nGATE: --require-two-kernels PASSED — every obligation has ≥2 independent kernels."
      IO.println (out ++ "\n")
      return 0
    if reportType == "bridge-check" then
      -- Feature #1: differential-test the Core→VC BRIDGE against an INDEPENDENT
      -- concrete evaluator. For each linear obligation, fuzz hypothesis-satisfying
      -- assignments and look for one that refutes the safety conclusion. A
      -- counterexample under a PROVED obligation is a bridge/discharge unsoundness
      -- (exit 1). A counterexample under an UNPROVED one is expected and shows the
      -- fuzzer has teeth. This checks a different axis than multi-kernel: not "do N
      -- kernels agree the VC is valid" but "does the VC actually match execution".
      let omegaProved ← kernelDischargeLoopVCs
        (Report.overflowGoals parsed.modules ++ Report.boundsGoals parsed.modules ++ Report.divGoals parsed.modules ++ Report.divQuotGoals parsed.modules ++ Report.shiftGoals parsed.modules)
      let results := Report.bridgeFuzz parsed.modules
      let mut out := "=== Bridge differential-check (concrete evaluation vs the VC) ==="
      out := out ++ "\n  tests whether a PROVED obligation is ever refuted by a concrete, hypothesis-"
      out := out ++ "\n  satisfying input (would mean the Core→VC bridge or discharge is unsound).\n"
      let mut unsound := 0
      for r in results do
        let proved := omegaProved.contains r.key
        let verdict ←
          match r.counterexample with
          | some env =>
            let e := ", ".intercalate (env.map (fun (n, v) => s!"{n}={v}"))
            if proved then do
              unsound := unsound + 1
              pure s!"UNSOUND — proved, but refuted by [{e}]"
            else pure s!"violation found by fuzzer (expected: unproved) [{e}]"
          | none =>
            if r.hypSat == 0 then pure "inconclusive (no hypothesis-satisfying inputs in grid)"
            else if proved then pure s!"ok — proved; {r.hypSat} inputs checked, no counterexample"
            else pure s!"no counterexample in grid ({r.hypSat} inputs); unproved"
        out := out ++ s!"\n  [{r.key}]  {r.desc}\n      {verdict}"
      if unsound > 0 then
        out := out ++ s!"\n\nBRIDGE-CHECK FAILED — {unsound} proved obligation(s) refuted by concrete inputs."
        IO.println (out ++ "\n"); return 1
      out := out ++ "\n\nBRIDGE-CHECK: no proved obligation refuted by concrete evaluation."
      IO.println (out ++ "\n")
      return 0
    if reportType == "core-semantics-diff" then
      -- BRIDGE diversity, the gap `--report multi-kernel` names as a
      -- non-attestation: all kernels check their own lowering of an obligation
      -- produced by ONE Core→VC bridge, so a bridge bug is invisible to every one of
      -- them. Here Core takes a SECOND, independent path — extracted to Rocq
      -- (Gallina) as a computation — and the two are differential-tested:
      --   * Concrete's Lean interpreter evaluates f(args)   (the existing oracle)
      --   * Rocq's kernel evaluates the extracted definition, by checking
      --     `Goal f args = <interpreter result>. Proof. reflexivity. Qed.`
      -- Agreement means two independently-implemented evaluators computed the same
      -- value. A mismatch is a real bridge-level defect.
      let fns := Concrete.Interp.collectFns validCore.coreModules
      let enums := Concrete.Interp.collectEnums validCore.coreModules
      let extractable := Concrete.CoreExtract.extractableFns fns
      let missed := Concrete.CoreExtract.unextractableFns fns
      let allDefs := "\n".intercalate (extractable.map (·.2))
      -- Probe values: small magnitudes plus negatives, because the truncating-vs-
      -- flooring division difference (Z.quot vs Z.div) only shows on negatives.
      let probes : List Int := [-7, -3, -1, 0, 1, 2, 3, 7]
      let perFnCap := 24
      let mut out := "=== Core-semantics differential test (Rocq extraction vs the interpreter) ==="
      out := out ++ "\n  Two INDEPENDENT paths from Core to a value:"
      out := out ++ "\n    (1) Concrete's Lean interpreter — the existing differential-testing oracle"
      out := out ++ "\n    (2) a Rocq/Gallina extraction of the same function, evaluated by Rocq's kernel"
      out := out ++ "\n  agree    = both evaluators computed the same value on every sampled input"
      out := out ++ "\n  DISAGREE = the two paths differ — a Core-level bridge defect"
      out := out ++ "\n  Differential test, not a proof of equivalence: it covers sampled inputs only.\n"
      if extractable.isEmpty then
        out := out ++ "\n  (no function in this file is inside the extractable fragment)"
      let mut disagree := 0
      let mut checked := 0
      let mut errored := 0
      for (f, _) in extractable do
        -- Only integer parameters can be sampled with literals.
        let intParams := f.params.all (fun (_, t) => Concrete.CoreExtract.isIntTy t)
        if !intParams then
          out := out ++ s!"\n  [{f.name}]  extracted, but not sampled (non-integer parameter)"
        else
          -- cartesian product of probes over the parameters, capped
          let mut product : List (List Int) := [[]]
          for _ in f.params do
            product := product.flatMap (fun a => probes.map (fun v => a ++ [v]))
          let argSets := product.take perFnCap
          let mut goals : List String := []
          let mut skipped := 0
          for args in argSets do
            -- Build `f(a, b, ...)` in Core and evaluate it with the interpreter.
            let argExprs := (args.zip f.params).map
              (fun (v, (_, t)) => Concrete.CExpr.intLit v t)
            let callExpr := Concrete.CExpr.call (.direct f.name) [] argExprs f.retTy
            match Concrete.Interp.evalExprVal fns enums [] callExpr with
            | .error _ => skipped := skipped + 1   -- trap or unsupported: never counted
            | .ok (_, v) =>
              let expected : Option String := match v with
                | .int n _ => some (if n < 0 then s!"({n})" else s!"{n}")
                | .bool b => some (if b then "true" else "false")
                | _ => none
              match expected with
              | none => skipped := skipped + 1
              | some e =>
                let argStr := " ".intercalate
                  (args.map (fun v => if v < 0 then s!"({v})" else s!"{v}"))
                let app := if argStr.isEmpty then f.name else s!"{f.name} {argStr}"
                -- Named lemma + `Print Assumptions` so this script attests its own
                -- integrity like every other Rocq script we emit. rocqDischarge
                -- requires that attestation before believing exit 0, because `coqc`
                -- exits 0 on `Admitted.` as well as `Qed.`; an unattested script is
                -- classified `error`, so every emitter must opt in or its results stop
                -- counting. Uniform by construction rather than per-call-site.
                let nm := s!"diff{goals.length}"
                goals := goals ++
                  [s!"Lemma {nm} : {app} = {e}. Proof. reflexivity. Qed.\nPrint Assumptions {nm}."]
          if goals.isEmpty then
            out := out ++ s!"\n  [{f.name}]  no comparable input (all {skipped} probes trapped or were unsupported)"
          else
            let src := Concrete.CoreExtract.extractPreamble ++ allDefs ++ "\n"
                       ++ "\n".intercalate goals ++ "\n"
            let res ← rocqDischarge [(f.name, src)]
            let cell := match res with
              | none => "unavailable (coqc not on PATH)"
              | some verdicts => match verdicts.lookup f.name with
                | none => "not-asked"
                -- coqc accepted every equation: the two evaluators agree.
                | some .closed => s!"agree ({goals.length} inputs, {skipped} skipped)"
                -- `reflexivity` failing means the two computed DIFFERENT values.
                -- classifyRocqFailure calls that an `error` because it is not a
                -- tactic-failure marker, so re-read it here where a non-unifying
                -- equation is precisely the signal we are looking for.
                | some (.error d) =>
                  if strContains d "Unable to unify" || strContains d "Not convertible"
                  then s!"DISAGREE — {d}"
                  else s!"error — {d}"
                | some .refused => "DISAGREE — Rocq could not verify the equation"
            -- Count only genuine agreement as agreement. Counting an `error` here
            -- would let a broken emitted script read as a checked function, which is
            -- the same class of lie as calling it `refused`.
            if strContains cell "DISAGREE" then disagree := disagree + 1
            else if strContains cell "error" then errored := errored + 1
            else if strContains cell "agree" then checked := checked + 1
            out := out ++ s!"\n  [{f.name}]  {cell}"
      if !missed.isEmpty then
        out := out ++ s!"\n\n  OUTSIDE the extractable fragment ({missed.length}), so NOT bridge-checked:"
        out := out ++ s!"\n    {", ".intercalate missed}"
        out := out ++ "\n  (loops, structs, enums, arrays, matches, borrows, casts, generics, floats,"
        out := out ++ "\n   strings — named rather than silently skipped, so coverage is not overstated)"
      if disagree > 0 then
        out := out ++ s!"\n\nCORE-SEMANTICS-DIFF FAILED — {disagree} function(s) where the interpreter and the"
        out := out ++ "\n  Rocq extraction compute different values. One of the two mis-models Core."
        IO.println (out ++ "\n")
        return 1
      if errored > 0 then
        out := out ++ s!"\n\nCORE-SEMANTICS-DIFF FAILED — {errored} function(s) produced an unusable extraction"
        out := out ++ "\n  (a malformed or non-self-contained Gallina script). That is a defect in the"
        out := out ++ "\n  extractor, not evidence about the program, so it is not reported as agreement."
        IO.println (out ++ "\n")
        return 1
      out := out ++ s!"\n\nCORE-SEMANTICS-DIFF: {checked} function(s) agree across two independent evaluators."
      IO.println (out ++ "\n")
      return 0
    if reportType == "term-ir" then
      -- R-0455: how much the STRING lowering loses, as a number. `exprToProver` drops any
      -- term containing `div`/`mod` or a spec call; `ofExpr` carries both. The two therefore
      -- disagree on exactly the obligations the prover path silently loses today, which is
      -- the defect R-0455 describes in prose.
      let dropped := Report.droppedByStringLayer parsed.modules
      let carried := Report.carriedByBoth parsed.modules
      let both := Report.droppedByBoth parsed.modules
      IO.println "=== Term IR coverage vs the string lowering (R-0455) ==="
      IO.println "  `exprToProver` renders an obligation to a prover's syntax and returns"
      IO.println "  `none` for anything outside an infix-only operator table — div/mod inside a"
      IO.println "  larger term, and spec-function calls. Those obligations are DROPPED from the"
      IO.println "  prover path. The IR carries them."
      IO.println ""
      IO.println s!"  obligations examined:              {carried.length + dropped.length + both.length}"
      IO.println s!"    carried by both layers:          {carried.length}"
      IO.println s!"    DROPPED by the string layer only: {dropped.length}   <- the IR's win"
      IO.println s!"    dropped by BOTH:                 {both.length}   <- outside the IR's fragment too"
      IO.println ""
      IO.println "  Casts ARE modelled, as a wrap at the target width matching the reference"
      IO.println "  (`Interp.evalCast`/`IntArith.wrapToType`) — not as identity, which would make"
      IO.println "  the IR denote a different value than the program computes. Before that,"
      IO.println "  `arr[(a / b) as Int]` was dropped by BOTH layers and the IR recovered nothing."
      IO.println ""
      IO.println "  Measured across examples/: the IR recovers 0, because the corpus contains no"
      IO.println "  obligation with a division inside a cast. The capability is exercised by a"
      IO.println "  constructed fixture in check_transform_register.sh, so 'recovers 0 here' is a"
      IO.println "  fact about this corpus rather than about the IR."
      if carried.isEmpty && dropped.isEmpty && both.isEmpty then
        IO.println ""
        IO.println "  (no linear obligations in this file — the comparison examined nothing,"
        IO.println "   which is not the same as nothing being dropped)"
      for (k, e) in dropped do
        IO.println s!"    [{k}]  {e}"
      return 0
    if reportType == "artifact-fuzz" then
      -- R-0462: emit a driver that exercises every fuzzable function with contract-satisfying
      -- inputs. Compiling and running it is `check_artifact_fuzz.sh`'s job — the compiler
      -- cannot re-invoke itself, and keeping the plan separate makes it deterministic and
      -- diffable rather than hidden inside a process the gate spawns.
      -- Classify each function by what the obligation layer CLAIMS about its runtime safety.
      -- Derived from the SAME discharged ledger the reports and the release gate read, so the
      -- fuzzer cannot disagree with them about whether something was proved.
      let dvcs ← computeVCsDischarged parsed.modules locMap registry
      let safetyKinds := ["no_overflow", "array_bounds", "div_nonzero",
                          "div_quotient_in_range", "shift_amount_in_range"]
      let claimOf : String → String := fun fq =>
        let mine := dvcs.filter (fun v => v.fn == fq && safetyKinds.contains v.kind)
        if mine.isEmpty then "unclaimed"
        else if mine.all (fun v => v.status.startsWith "proved" || v.status == "arithmetic_proved")
        then "claimed" else "unproved"
      -- The invoked file's own functions, from the SCOPED Core rather than from `locMap`
      -- (whose `file` field is stamped uniformly and cannot distinguish dependencies).
      let ownFns := (scopedValidCore.coreModules.flatMap Report.coreFnFingerprints).map (·.1)
      let cases := Report.artifactFuzzCases parsed.modules ownFns claimOf
      let total := cases.foldl (fun n c => n + c.argRows.length) 0
      let allFns := ownFns.length
      IO.println "=== Artifact fuzz plan (R-0462: run the BINARY against the safety claims) ==="
      IO.println "  Register A says: if the obligation holds, the runtime property holds."
      IO.println "  This tests that empirically against the artifact that ships, so it also"
      IO.println "  crosses surface -> Core -> SSA -> LLVM, which no register row covers."
      IO.println ""
      IO.println s!"  fuzzable functions: {cases.length} of {allFns}   argument tuples: {total}"
      if cases.isEmpty then
        IO.println ""
        IO.println "  (nothing fuzzable here — see the exclusion rules on `artifactFuzzCases`:"
        IO.println "   public, integer params and result, no capabilities, no type params)"
      let byClaim := fun (k : String) => cases.filter (fun c => c.claim == k)
      IO.println s!"  claimed   (all safety obligations proved): {(byClaim "claimed").length}"
      IO.println s!"  unclaimed (no safety obligation at all):   {(byClaim "unclaimed").length}"
      IO.println s!"  unproved  (NOT fuzzed — a trap is expected): {(byClaim "unproved").length}"
      IO.println ""
      for c in cases do
        IO.println s!"    [{c.claim}] {c.fnQual}  ({c.argRows.length} rows)"
      IO.println ""
      IO.println "--- BEGIN DRIVER ---"
      IO.print (Report.artifactFuzzDriver cases)
      IO.println "--- END DRIVER ---"
      IO.println "--- BEGIN MECHANISM DRIVER ---"
      IO.print (Report.artifactFuzzMechanismDriver cases)
      IO.println "--- END MECHANISM DRIVER ---"
      return 0
    if reportType == "lowering-agreement" then
      -- Closes the hole `--report multi-kernel` states as a non-attestation: that the
      -- per-kernel lowerings spell the SAME proposition. For each obligation we emit
      -- GROUND instances of that prover's own rendering and make the prover decide
      -- them, comparing against the independent concrete evaluator. `refused` here
      -- means the rendering has a different truth table than the reference — a real
      -- disagreement, and grounds to distrust that kernel's cell in the badge.
      let drivers : List (String × String × Bool × List (String × String)) :=
        [ ("rocq:lia", "rocq", rocqRun, Report.rocqAgreementGoals parsed.modules),
          ("isabelle:presburger", "isabelle", isaRun, Report.isabelleAgreementGoals parsed.modules) ]
      let mut out := "=== Lowering-agreement check (does each kernel's rendering mean the SAME thing?) ==="
      out := out ++ "\n  method: substitute literals -> the driver renders a CLOSED proposition ->"
      out := out ++ "\n          the prover decides it -> compare against the independent evaluator."
      out := out ++ s!"\n  up to {Report.agreementInstanceCap} ground instances per obligation,"
      out := out ++ " hypothesis-satisfying ones first.\n"
      out := out ++ "\n  agrees   = same truth table as the reference evaluator on the grid"
      out := out ++ "\n  DISAGREES = the rendering means something else (do not trust that cell)"
      out := out ++ "\n  error    = the rendering is malformed (our bug)\n"
      let mut disagreements := 0
      let mut ran := false
      for (label, _, enabled, goals) in drivers do
        if !enabled then
          out := out ++ s!"\n  {label}: off (flag not set)"
        else
          ran := true
          let res ← if label == "isabelle:presburger"
                    then isabelleDischarge goals else rocqDischarge goals
          match res with
          | none => out := out ++ s!"\n  {label}: unavailable (tool not on PATH)"
          | some verdicts =>
            out := out ++ s!"\n  {label}:"
            if verdicts.isEmpty then
              out := out ++ "\n      (no obligation lowerable to this prover in this file)"
            for (k, v) in verdicts do
              let cell := match v with
                | .closed => "agrees"
                | .refused => "DISAGREES — rendering differs from the reference evaluator"
                | .error d => s!"error — {d}"
              if v == .refused then disagreements := disagreements + 1
              out := out ++ s!"\n      [{k}]  {cell}"
      if !ran then
        out := out ++ "\n\n  (pass --rocq and/or --isabelle to actually check a kernel's lowering)"
      -- The SMT rendering is checked ALWAYS, with no flag, because unlike the external
      -- kernels it needs no extra toolchain (z3 is in the base dev shell) and unlike them
      -- its verdict enters the TCB as `solver_trusted` with no kernel re-deriving it. It
      -- was the only lowering with no agreement check at all — the cheapest diversity in
      -- the system (one printer, every SMT-LIB consumer) and the least validated.
      -- R-0450: Lean's OWN rendering, checked on the same scale as everyone else's and
      -- with no flag, because omega needs no extra toolchain. Lean was the last unchecked
      -- lowering, and the reason recorded for that — "its rendering IS the reference, so
      -- nothing exists to validate it with" — was wrong: the reference is the EVALUATOR,
      -- and Lean's rendering is measured against it exactly as Rocq's and Isabelle's are.
      let leanGoals := Report.leanAgreementGoals parsed.modules
      if leanGoals.isEmpty then
        out := out ++ "\n\n  lean:omega (exprToLeanProp): no obligation lowerable"
      else
        let closed ← kernelDischargeLoopVCs leanGoals
        out := out ++ s!"\n\n  lean:omega (exprToLeanProp): {closed.length}/{leanGoals.length} agree"
        for (k, _) in leanGoals do
          if !closed.contains k then
            disagreements := disagreements + 1
            out := out ++ s!"\n      [{k}]  DISAGREES — rendering differs from the reference evaluator"
      let smtGoals := Report.smtAgreementGoals parsed.modules
      if smtGoals.isEmpty then
        out := out ++ "\n\n  smt (exprToSmt): no obligation renders into the SMT column"
      else
        match ← smtAgreementCheck smtGoals with
        | none =>
          out := out ++ "\n\n  smt (exprToSmt): unavailable (no z3) — NOT validated"
        | some verdicts =>
          let bad := verdicts.filter (fun (_, v) => v == .refused)
          let errs := verdicts.filter (fun (_, v) => match v with | .error _ => true | _ => false)
          out := out ++ s!"\n\n  smt (exprToSmt): {verdicts.length - bad.length - errs.length}/{verdicts.length} ground instances agree"
          for (k, _) in bad do
            out := out ++ s!"\n      [{k}]  DISAGREES — exprToSmt renders a different proposition"
            disagreements := disagreements + 1
          for (k, v) in errs do
            match v with
            | .error d => out := out ++ s!"\n      [{k}]  error — {d}"
            | _ => pure ()
      if disagreements > 0 then
        out := out ++ s!"\n\nLOWERING-AGREEMENT FAILED — {disagreements} lowering(s) do not mean what the"
        out := out ++ "\n  obligation means. Any multi-kernel badge resting on them is unearned."
        IO.println (out ++ "\n")
        return 1
      out := out ++ "\n\nLOWERING-AGREEMENT: every checked rendering matches the reference evaluator."
      IO.println (out ++ "\n")
      return 0
    if reportType == "solver-cert" then
      -- Feature #2: certificate-check the SMT solver. Nonlinear overflow VCs the
      -- kernel tiers can't close go to an external solver (`solver_trusted` — solver
      -- in the TCB). Here we ALSO hand each to Rocq's `nia`: a VC the solver reports
      -- unsat AND `nia` independently closes graduates to `solver_checked` — a kernel
      -- corroborated the solver, so the solver drops out of the trusted base. Honest
      -- boundary: this is kernel corroboration, not literal LRAT/Alethe proof-term
      -- replay (that needs a certified SAT/SMT checker — future work).
      let smtGoals := Report.overflowSmtGoals parsed.modules
      let solverId ← z3VersionId
      let z3res ← smtDischarge smtGoals 5
      let niaGoals := Report.rocqNiaGoals parsed.modules
      let niaEmitted := niaGoals.map (·.1)
      let niaClosed ← rocqDischarge niaGoals
      -- Certificate REPLAY (strictly stronger than corroboration): Isabelle's `smt`
      -- with smt_oracle=false reconstructs the solver's proof in the HOL kernel, and
      -- the emitted theory asserts the result carries no oracle.
      let replayGoals := Report.isabelleSmtReplayGoals parsed.modules
      let replayEmitted := replayGoals.map (·.1)
      let replayRes ← isabelleDischarge replayGoals
      let mut out := "=== Solver certificate-check (SMT solver + independent kernel corroboration + REPLAY) ==="
      out := out ++ s!"\n  solver: {solverId}   corroborating kernel: Rocq nia   replay: Isabelle smt(verit), smt_oracle=false"
      out := out ++ "\n  solver_trusted  = solver in the TCB."
      out := out ++ "\n  solver_checked  = an independent decision procedure reached the same verdict"
      out := out ++ "\n                    (solver drops out of the TCB, but its reasoning was not checked)."
      out := out ++ "\n  solver_replayed = the solver's PROOF was reconstructed inference-by-inference in a"
      out := out ++ "\n                    kernel, and asserted oracle-free. Strictly stronger."
      out := out ++ "\n  NOTE: reconstruction supports LINEAR integer arithmetic only — z3, cvc5 and"
      out := out ++ "\n        veriT all fail on nonlinear goals. These VCs are nonlinear by selection,"
      out := out ++ "\n        so `refused` in the replay column is the expected status today, not a bug.\n"
      if smtGoals.isEmpty then
        out := out ++ "\n(no SMT-eligible nonlinear obligations in this file)\n"
      for (k, cls, _) in z3res do
        let niaCell := match niaClosed with
          | none => "unavailable"
          | some verdicts =>
            if !niaEmitted.contains k then "not-asked"
            else match verdicts.lookup k with
              | none => "not-asked"
              | some v => v.cell
        let replayCell := match replayRes with
          | none => "unavailable"
          | some verdicts =>
            if !replayEmitted.contains k then "not-asked"
            else match verdicts.lookup k with
              | none => "not-asked"
              | some v => v.cell
        -- Only a genuine `closed` graduates the class. An `error` cell must never
        -- graduate (and never silently read as `refused`). Replay outranks
        -- corroboration: a checked proof is stronger than a matching verdict.
        let final :=
          if cls == "solver_trusted" && replayCell == "closed" then "solver_replayed"
          else if cls == "solver_trusted" && niaCell == "closed" then "solver_checked"
          else cls
        out := out ++ s!"\n  [{k}]\n      z3 = {cls}   rocq:nia = {niaCell}   isabelle:smt(verit) = {replayCell}   => {final}"
      -- Loud on our own bugs, as in the multi-kernel report.
      match niaClosed with
      | some verdicts =>
        let errs := verdicts.filterMap (fun (k, v) =>
          match v with | .error d => some (k, d) | _ => none)
        if !errs.isEmpty then
          out := out ++ s!"\n\n  ERRORS ({errs.length}) — OUR bugs, not kernel disagreements:"
          for (k, d) in errs do out := out ++ s!"\n    rocq:nia [{k}]: {d}"
      | none => pure ()
      IO.println (out ++ "\n")
      return 0
    if reportType == "caps" then
      IO.println (Report.capabilityReport validCore.coreModules)
      return 0
    if reportType == "unsafe" then
      -- R-0484 R10: one assumption summary over the program AND its loaded dependencies.
      let assumptions := Assumptions.build (fullValidCore.coreModules ++ depModules) assumptionPackageOf assumptionPkgDefault
      IO.println (Report.unsafeReport validCore.coreModules pc assumptions (depsLoaded := !depModules.isEmpty))
      return 0
    if reportType == "assumptions" then
      -- R-0484 R10: the machine-readable view of the assumption summary the text reports render.
      let assumptions := Assumptions.build (fullValidCore.coreModules ++ depModules) assumptionPackageOf assumptionPkgDefault
      IO.println (assumptions.toJson (validCore.coreModules.map (·.name)) (!depModules.isEmpty))
      return 0
    if reportType == "trust-edges" then
      IO.println (Report.trustEdgeReport validCore.coreModules)
      return 0
    if reportType == "layout" then
      IO.println (Report.layoutReport validCore.coreModules)
      return 0
    if reportType == "alloc" then
      IO.println (Report.allocReport validCore.coreModules)
      return 0
    if reportType == "authority" then
      IO.println (Report.authorityReport validCore.coreModules)
      return 0
    if reportType == "proof" then
      IO.println (Report.proofReport validCore.coreModules pc)
      return 0
    if reportType == "eligibility" then
      IO.println (Report.eligibilityReport pc)
      return 0
    if reportType == "proof-status" then
      IO.println (Report.proofStatusReport validCore.coreModules locMap srcMap (registry := registry) (pc := pc))
      -- RECEIPT CONSUMPTION. The status above is derived from source facts alone and explicitly does
      -- NOT mean the kernel re-ran. A stored receipt is the only thing that can say it did — and it
      -- is untrusted input, so it is re-checked against material computed fresh right now. A receipt
      -- that disagrees makes nothing current; it is reported as disagreeing, which is a fact worth
      -- more than its absence.
      match receiptsIn with
      | none => pure ()
      | some path =>
        let raw ← try pure (some (← IO.FS.readFile ⟨path⟩)) catch _ => pure none
        match raw with
        | none => IO.println s!"\n=== Replay Receipts ===\n  error: cannot read receipt store '{path}'"
        | some contents =>
        let records := Proof.decodeStore contents
        let compilerVersion := compilerEvidenceIdentity
        let workspaceId := match packageIdentity with
          | .ok p => p.digest
          | .error _ => ""
        -- The CURRENT checker, asked without paying for a kernel run — the same producer `replay`
        -- resolves through, so "which toolchain" and "which proof library" each have one answer.
        let checkerNow ← Proof.resolveChecker
          { inputPath := inputPath, fallbackDir := "."
          , imports := ["Concrete", "Examples"], targets := [] }
        let toolchain := match checkerNow with
          | some (_, e) => e.toolchain
          | none => "unknown"
        -- THE PROOF LIBRARY, BY CONTENT. A receipt that did not identify whose theorems were
        -- accepted could be replayed against a different library and still read current.
        let importsId := match checkerNow with
          | some (_, e) => Proof.importsIdOf e.importDigests
          | none => ""
        let statusEntries := Report.proofStatusEntries validCore.coreModules locMap registry pc
        let trustedDepsOf := fun (qn : String) =>
          (statusEntries.find? (fun s => s.qualName == qn)).map (·.trustedDeps) |>.getD []
        let env : Proof.IssueEnvironment := { compilerVersion, workspaceId, importsId }
        let mut lines : List String := []
        let mut current := 0
        let mut notCurrent := 0
        let mut unreadable := 0
        for (subject, decoded) in records do
          match decoded with
          | .error e =>
            -- A RECORD THAT WILL NOT DECODE IS NOT AN ABSENT RECORD. Reporting nothing here would
            -- make a corrupted store look like a store with fewer receipts in it.
            unreadable := unreadable + 1
            lines := lines ++ [s!"    ? {subject} — unreadable [{e.canonical}] {e.explain}"]
          | .ok st =>
            match pc.entries.find? (fun e => e.qualName == subject) with
            | none =>
              -- A receipt for a claim this program does not have. Named rather than dropped: it is
              -- the shape a receipt from another package takes when it lands in this store.
              notCurrent := notCurrent + 1
              lines := lines ++ [s!"    ✗ {subject} — no such claim in this program; the receipt describes something else"]
            | some entry =>
              match Proof.storedDispositionFor pc env toolchain trustedDepsOf entry st with
              | .error w =>
                notCurrent := notCurrent + 1
                lines := lines ++ [s!"    ✗ {subject} — cannot be checked [{w.canonical}] {w.explain}"]
              | .ok .current =>
                current := current + 1
                let trust := if st.carriesTrust then s!" ASSUMING {", ".intercalate st.trustedBoundaries}" else ""
                lines := lines ++ [s!"    ✓ {subject} — kernel-replayed, receipt current ({st.theoremArtifact}){trust}"]
              | .ok .notCurrent =>
                notCurrent := notCurrent + 1
                lines := lines ++ [s!"    ✗ {subject} — receipt NOT current: it was established against different material"]
              | .ok .needsRecheck =>
                unreadable := unreadable + 1
                lines := lines ++ [s!"    ? {subject} — receipt written under a different envelope; re-verify and re-record"]
        -- COVERAGE IS PART OF THE VERDICT. An independent review deleted one record from a store
        -- and the section came back all-green: the claim simply vanished, and a dependent of it
        -- still read `current`. An all-tick section with a silently truncated denominator is
        -- exactly the output a reviewer acts on, so every claim that COULD carry a receipt and has
        -- none is named here.
        let covered := records.map (·.1)
        let missing := pc.entries.filterMap fun e =>
          if e.spec.isSome && !covered.contains e.qualName then some e.qualName else none
        for m in missing do
          lines := lines ++ [s!"    · {m} — NO RECEIPT in this store (proof-linked, so one is expected)"]
        IO.println (String.intercalate "\n"
          ([s!"\n=== Replay Receipts ({path}) ==="] ++ lines
           ++ [s!"\n  {current} current, {notCurrent} not current, {unreadable} unreadable, {missing.length} no receipt"
              , "  A receipt makes a claim kernel-replayed ONLY while it agrees with fresh facts."
              , "  Status above is derived from source facts and does not itself mean the kernel ran."]))
        -- THE EXIT CODE CARRIES THE VERDICT when receipts were consumed. It was unconditionally 0 —
        -- five current, zero current, an empty store and an unreadable one all exited 0 — so any CI
        -- gate built on this surface passed no matter what it found. Only the `--receipts` mode is
        -- affected: without a store, this report answers a different question and keeps its old
        -- exit semantics.
        if notCurrent + unreadable + missing.length > 0 then return 1
      return (if hasRegistryErrors then 1 else 0)
    if reportType == "obligations" then
      IO.println (Report.obligationsReport validCore.coreModules locMap registry pc)
      return (if hasRegistryErrors then 1 else 0)
    if reportType == "proof-deps" then
      IO.println (Report.proofDepsReport pc)
      return 0
    if reportType == "proof-diagnostics" then
      IO.println (Report.proofDiagnosticsReport (pc := pc))
      return 0
    if reportType == "proof-bundle" then
      let ms ← IO.monoMsNow
      let timestamp := toString ms
      let ident ← compilerIdentity
      IO.println (Report.proofBundleJson inputPath timestamp ident validCore.coreModules locMap (registry := registry) (pc := pc))
      return (if hasRegistryErrors then 1 else 0)
    if reportType == "extraction" then
      IO.println (Report.extractionReport (registry := registry) (pc := pc))
      return 0
    -- THESE THREE PRINTED THEIR REPORT AND THEN FELL THROUGH to the "Unknown report type" branch,
    -- exiting 1 with a diagnostic naming a report the caller had just successfully received. Every
    -- neighbouring handler returns 0; these three did not, so any caller checking exit status saw a
    -- failure on correct output. Found by a test harness that started rejecting nonzero exits and
    -- immediately caught three of its own inputs.
    if reportType == "attestation-join" then
      IO.println (Report.attestationJoinReport (pc := pc))
      return 0
    if reportType == "generated-implementations" then
      IO.println (Report.generatedImplementationsReport (pc := pc))
      return 0
    if reportType == "impl-manifest" then
      IO.println (Report.implementationManifestReport (pc := pc))
      return 0
    if reportType == "body-bytes" then
      IO.println (Report.bodyBytesReport (pc := pc))
      return 0
    if reportType == "migration-manifest" then
      IO.println (Report.migrationManifestReport (pc := pc) (registry := registry))
      return 0
    if reportType == "subject-facts" then
      IO.println (Report.subjectFactsReport (pc := pc))
      return 0
    if reportType == "lean-stubs" then
      IO.println (Report.leanStubsReport pc (registry := registry))
      return 0
    if reportType == "migration" then
      -- THE V1 -> V2 DRY RUN. Every stored `#[proof_fingerprint]` is a V1 body-only value; activating
      -- V2 makes all of them not comparable at once, so activation without migration would turn the
      -- corpus `needs_recheck` in a single commit. This says exactly what would be written and what
      -- would not, and reconciles: total = migrate + already_v2 + refused.
      -- NO REPLAY. The plan asks which body a stored fingerprint records; the kernel's verdict on
      -- the linked theorem is a different question with its own report, and making it an input here
      -- stranded every fixture whose theorem is fictional by design.
      let rows := Proof.migrationPlan pc
      let mut out := "=== V1 -> V2 Migration Plan ===\n"
      for r in rows do
        let d := r.disposition
        let detail := match d with
          | .migrate v2 => s!"{r.stored} -> {v2}"
          | .alreadyV2 => s!"{r.stored} (already v2)"
          | .staleNoHonestValue =>
            "body has moved since the proof was pinned; NO honest v2 value exists, so this keeps its v1 value and becomes needs_recheck"
          | .noSubjectDigest => "no v2 subject digest is computable"
        out := out ++ s!"  [{d.canonical}] {r.subject} ({r.status}): {detail}\n"
      let mig := (Proof.MigrationRow.migrating rows).length
      let ref := (Proof.MigrationRow.refused rows).length
      let already := rows.length - mig - ref
      out := out ++ s!"\nTotal {rows.length} stored fingerprint(s) = {mig} migrate + {already} already v2 + {ref} refused"
      out := out ++ "\n  DRY RUN. Nothing is written; V2 activation is a separate, atomic step."
      IO.println out
      return 0
    if reportType == "receipts" then
      -- PRODUCTION RECEIPT ISSUANCE. Until this existed, `ProofEvidenceReceipt` was a well-tested
      -- helper type: the envelope was closed and the minting authority was closed, and nothing in
      -- the pipeline ever minted one. This is the path from "the kernel accepted this" to a
      -- receipt, and it derives none of its four inputs itself — each comes from the single
      -- producer that already answers that question elsewhere, and each refuses on its own terms.
      match ← Proof.replay
          { inputPath := inputPath
          , fallbackDir := "."
          , imports := ["Concrete", "Examples"]
          , targets := Proof.replayTargetsOf pc } with
      | .error .noTargets =>
        IO.println "=== Replay Receipts ===\n\nNo claims with proof links to replay, so nothing to issue."
        return 0
      | .error refusal =>
        -- NO KERNEL RAN. Issuing nothing is correct; reporting it as "no receipts" would be the
        -- same sentence a fully-refused corpus produces, so the obstacle is named instead.
        IO.println s!"=== Replay Receipts ===\n\nerror: {refusal.explain}"
        return 1
      | .ok res =>
      -- The environment identities issuance cannot derive for itself. The TOOLCHAIN half is
      -- deliberately absent: it comes from the replay that actually ran, so a caller cannot name a
      -- checker other than the one that checked.
      let compilerVersion := compilerEvidenceIdentity
      let workspaceId := match packageIdentity with
        | .ok p => p.digest
        | .error _ => ""
      -- THE PROOF LIBRARY THE KERNEL ACTUALLY READ, by content, from the replay that just ran.
      -- Until 2026-08-16 this bound the compiler's checked-in classification table, which is
      -- compiled into the binary — so replaying the same theorem NAMES against a different proof
      -- library produced a byte-identical receipt, and nothing in the evidence said whose theorems
      -- had been accepted. That was an authority gap, not a documentation one.
      let importsId := Proof.importsIdOf res.environment.importDigests
      let statusEntries := Report.proofStatusEntries validCore.coreModules locMap registry pc
      -- Keyed on the qualified name, which is what `ProofStatusEntry` carries. Trust comes from
      -- the same producer the status report reads; recomputing it here would be a second answer to
      -- "which boundaries does this rest on", and the receipt would be the copy nobody re-checks.
      let trustedDepsOf := fun (qn : String) =>
        (statusEntries.find? (fun s => s.qualName == qn)).map (·.trustedDeps) |>.getD []
      let outcomes := Proof.issueAll pc res
        { compilerVersion, workspaceId, importsId } trustedDepsOf
      let issued := Proof.IssueOutcome.issued outcomes
      let withheld := Proof.IssueOutcome.withheld outcomes
      let mut out := "=== Replay Receipts ===\n"
      out := out ++ s!"\nWorkspace: {res.environment.workspace}"
      out := out ++ (if res.environment.workspaceFromInput then " (from input)\n" else " (from working directory)\n")
      out := out ++ s!"Toolchain: {res.environment.toolchain}\n"
      -- NAMED, not just bound. The receipt binds a digest of (compiler, toolchain), so without this
      -- line nothing observable says WHICH compiler produced the evidence — and an identity that
      -- cannot be read cannot be checked against the executable it claims to describe.
      out := out ++ s!"Compiler: {compilerVersion}\n"
      if !issued.isEmpty then
        out := out ++ s!"\n  Issued ({issued.length}):\n"
        for o in issued do
          match o.result with
          | .ok r =>
            let trust := if r.carriesTrust then s!" ASSUMING {", ".intercalate r.trustedBoundaries}" else ""
            out := out ++ s!"    + {o.subject} — {r.theoremArtifact}{trust}\n"
            out := out ++ s!"        subject={r.subjectDigest} root={Concrete.shortHash r.dependencyRoot} edge={r.edge.canonical}\n"
          | .error _ => pure ()
      -- EVERY WITHHOLDING NAMES ITS CAUSE. "No receipt" and "receipt withheld because X" are
      -- different facts, and only the second is actionable.
      if !withheld.isEmpty then
        out := out ++ s!"\n  Withheld ({withheld.length}):\n"
        for o in withheld do
          match o.result with
          | .error e => out := out ++ s!"    - {o.subject} [{e.canonical}] {e.explain}\n"
          | .ok _ => pure ()
      out := out ++ s!"\nSummary: {issued.length} issued, {withheld.length} withheld"
      match receiptsOut with
      | some path =>
        IO.FS.writeFile ⟨path⟩ (Proof.encodeStore outcomes)
        out := out ++ s!"\n  Stored {issued.length} receipt(s) to {path}."
        -- WHAT A STORED RECEIPT IS WORTH, said where it is written. It is bytes; anything can write
        -- them. `--report proof-status --receipts <path>` re-checks each against material computed
        -- fresh, and a receipt that disagrees makes nothing current.
        out := out ++ "\n  A stored receipt is not evidence on its own: it is re-checked against"
        out := out ++ " fresh facts, and only agreement counts."
      | none =>
        out := out ++ "\n  NOTE: not stored. Pass --out <path> to write them."
      IO.println out
      return 0
    if reportType == "check-proofs" then
      -- WHICH claims to replay is a policy question, and it stays here. WHETHER the kernel accepted
      -- them is answered by the single producer `Concrete.Proof.replay`; this report renders that
      -- answer and re-derives none of it. Before the extraction the check was inline, entangled with
      -- `reportJson`, and its failure paths printed — so no other caller (receipt minting, the link
      -- migration, `concrete check`) could ask the question without writing a second answer to it.
      -- ONE producer for "which claims to replay", shared with receipt issuance: replaying a
      -- different set than issuance asks about would withhold receipts silently.
      let refinementTargets := Proof.replayTargetsOf pc
      -- Source-contract discharge theorems (the `ensures_proof` naming the theorem that proves an
      -- `#[ensures(...)]` obligation) are replayed too, so an obligation reported `proved_by_lean`
      -- is backed by a real kernel check.
      let ensuresThms := registry.filterMap fun e => e.ensuresProof.map (e.function, ·)
      let ensuresTargets : List Proof.ReplayTarget := ensuresThms.map fun (fn, thm) =>
        { subject := fn, theoremName := thm, kind := .ensures
        , origin := .sourceLinked, binding := .bound }
      -- Obligation status is a RENDERING input only: a stale claim whose theorem type-checks is
      -- reported `stale` rather than `checked`. It is not part of the replay question, so it is not
      -- part of the request.
      let statusByTheorem : List (String × String) :=
        pc.obligations.filterMap fun o => o.spec.map fun s => (s.proofName, o.status.canonical)
      match ← Proof.replay
          { inputPath := inputPath
          , fallbackDir := "."
          -- The compiler umbrella for infrastructure/refinement theorems, AND the `Examples`
          -- library for the flagship example-correctness proofs. The example proofs live in their
          -- own Lake library, so `import Concrete` alone does not make them reachable for `#check`.
          , imports := ["Concrete", "Examples"]
          , targets := refinementTargets ++ ensuresTargets } with
      | .error .noTargets =>
        if reportJson then
          IO.println "{\n  \"schema_version\": \"1\",\n  \"all_checked\": true,\n  \"checks\": [],\n  \"lean_error\": \"\",\n  \"summary\": {\"verified\": 0, \"failed\": 0, \"total\": 0}\n}"
        else
          IO.println "=== Lean Proof Kernel Check ===\n\nNo proved, stale, or unbound obligations with proof names to check."
        return 0
      | .error refusal =>
        -- Every remaining refusal means NO KERNEL RAN. Say what is actually wrong: reporting N
        -- missing theorems for one missing workspace sends the reader to look for the wrong thing.
        let msg := refusal.explain
        if reportJson then
          IO.println s!"\{\n  \"schema_version\": \"1\",\n  \"all_checked\": false,\n  \"checks\": [],\n  \"refusal\": \"{refusal.canonical}\",\n  \"lean_error\": \"{msg}\",\n  \"summary\": \{\"verified\": 0, \"failed\": 0, \"total\": 0}\n}"
        else
          IO.println s!"=== Lean Proof Kernel Check ===\n\nerror: {msg}"
        return 1
      | .ok res =>
      -- `ensures` checks are deliberately kept OUT of the verified/failed counts, which have always
      -- been refinement-only; they are rendered in their own section below.
      let refs := fun (cs : List Proof.ReplayCheck) => cs.filter (·.target.kind == .refinement)
      let verified := (refs res.accepted).map (·.target.subject)
      let unboundChecked := (refs res.acceptedUnbound).map (·.target.subject)
      let failed := (refs res.rejected).map fun c => (c.target.subject, c.target.theoremName)
      let notAttempted := (refs res.notAttempted).map (·.target.subject)
      let generalFailure := res.generalFailure
      let combined := res.transcript
      let tc := res.environment.toolchain
      let ws := res.environment.workspace
      let wsFromInput := res.environment.workspaceFromInput
      let theoremOf := fun (fn : String) =>
        (refinementTargets.find? (·.subject == fn)).map (·.theoremName) |>.getD "?"
      -- --json: structured whole-file proof-check summary (human output unchanged).
      if reportJson then
        let q := Report.jsonStr
        let lineOf := fun (fn : String) =>
          ((parsed.modules.flatMap Report.allFunctions).find?
            (fun (pfx, f) => pfx ++ f.name == fn)).map (fun (_, f) => f.span.line) |>.getD 0
        let has := fun (sub : String) => (combined.splitOn sub).length != 1
        let statusOf := fun (thm : String) (oblStatus : String) =>
          if generalFailure then "env_failure"
          else if has s!"`{thm}`" then (if has "unknown" then "missing_theorem" else "failed")
          else if oblStatus == "stale" then "stale"
          else "checked"
        let checkObj := fun (fn thm kind origin oblStatus : String) =>
          String.join ["    {\"function\": ", q fn, ", \"theorem\": ", q thm, ", \"obligation_kind\": ", q kind,
            ", \"origin\": ", q origin, ", \"status\": ", q (statusOf thm oblStatus),
            ", \"source_line\": ", toString (lineOf fn), "}"]
        let refChecks := refinementTargets.map fun t =>
          -- NOT `.getD "proved"`. Every refinement target comes from an obligation that has a
          -- status, so the fallback is unreachable — which is exactly why defaulting it to a real
          -- status is wrong: on the day it IS reached it would report a claim as proved because
          -- nothing was found. `unknown` is outside the status vocabulary and cannot be mistaken
          -- for one. Caught by check_one_producer.sh, which forbids defaulting a fact to a valid
          -- value of itself.
          checkObj t.subject t.theoremName "refinement" t.origin.canonical
            ((statusByTheorem.find? (·.1 == t.theoremName)).map (·.2) |>.getD "unknown")
        let ensChecks := ensuresThms.map fun (fn, thm) => checkObj fn thm "ensures" "source_linked" "proved"
        let allObjs := refChecks ++ ensChecks
        let leanErr := if generalFailure || !failed.isEmpty then String.ofList (combined.toList.take 800) else ""
        IO.println (String.join [
          "{\n  \"schema_version\": ", q "1", ",\n  \"toolchain\": ", q tc,
          ",\n  \"all_checked\": ", (if failed.isEmpty && !generalFailure then "true" else "false"),
          ",\n  \"checks\": [\n", ",\n".intercalate allObjs, "\n  ],\n",
          "  \"lean_error\": ", q leanErr,
          ",\n  \"summary\": {\"verified\": ", toString verified.length,
          ", \"unbound\": ", toString unboundChecked.length, ", \"failed\": ", toString failed.length,
          ", \"total\": ", toString (refinementTargets.length + ensuresThms.length), "}\n}"])
        return (if failed.isEmpty && res.rejected.isEmpty && !generalFailure then 0 else 1)
      -- Render report
      let mut out := "=== Lean Proof Kernel Check ===\n"
      -- Name the workspace. Slice 4 needs a deterministic workspace identity in
      -- the receipt, and a verdict that does not say where it was produced
      -- cannot be audited — especially in the fallback case above.
      out := out ++ s!"\nWorkspace: {ws}"
      out := out ++ (if wsFromInput then " (from input)\n" else " (from working directory; the input is outside any workspace)\n")
      out := out ++ s!"\nToolchain: "
      out := out ++ tc ++ "\n"
      if !verified.isEmpty then
        out := out ++ s!"\n  Kernel-verified ({verified.length}):\n"
        for fn in verified do
          let sourceTag :=
            if (refinementTargets.find? (·.subject == fn)).map (·.origin) == some .sourceLinked
            then "registry" else "hardcoded"
          out := out ++ s!"    ✓ {fn} — {theoremOf fn} ({sourceTag})\n"
      if !unboundChecked.isEmpty then
        out := out ++ s!"\n  Type-checked but UNBOUND — not proved ({unboundChecked.length}):\n"
        for fn in unboundChecked do
          out := out ++ s!"    ? {fn} — {theoremOf fn} (no stored proof-subject digest; add #[proof_fingerprint(\"...\")])\n"
      if !failed.isEmpty then
        out := out ++ s!"\n  Failed ({failed.length}):\n"
        for (fn, proofName) in failed do
          out := out ++ s!"    ✗ {fn} — {proofName} (theorem not found)\n"
      if !ensuresThms.isEmpty then
        out := out ++ s!"\n  Contract obligations — #[ensures] discharge ({ensuresThms.length}):\n"
        for (fn, thm) in ensuresThms do
          let mark := if (combined.splitOn s!"`{thm}`").length != 1 then "✗" else "✓"
          out := out ++ s!"    {mark} {fn} — ensures discharged by {thm}\n"
      if generalFailure then
        -- THE FILE DID NOT COMPILE, so no theorem has an individual verdict. Naming them as
        -- individually failed — which is what this report did while `generalFailure` was
        -- unreachable — blames artifacts that were very likely fine and sends the reader to look
        -- for the wrong thing. They are listed as NOT ATTEMPTED, and the transcript is shown
        -- because it is the only diagnosis available.
        out := out ++ s!"\n  Lean check failed as a whole (exit code {res.exitCode}) — no theorem was individually judged:\n"
        out := out ++ s!"    {combined.take 500}\n"
        if !notAttempted.isEmpty then
          out := out ++ s!"\n  NOT ATTEMPTED ({notAttempted.length}) — the file did not compile, so these were never judged:\n"
          for fn in notAttempted do
            out := out ++ s!"    · {fn} — {theoremOf fn}\n"
      -- Generate taxonomy diagnostics for failures
      let checkDiags := Concrete.checkProofResultsToDiagnostics
        (failed.map fun (fn, pn) => (fn, pn, true))
      if !checkDiags.isEmpty then
        out := out ++ s!"\n  Diagnostics ({checkDiags.length}):\n"
        for d in checkDiags do
          out := out ++ s!"    [{d.kind.code}] {d.function}: failure={d.failureClass}, repair={d.repairClass}\n"
      -- Keep `N verified, M failed` CONTIGUOUS: check_proof_freshness.sh extracts
      -- exactly that substring to compare verdicts across working directories, and
      -- because it compares two extracted strings, breaking the pattern would make
      -- both empty and the assertion pass VACUOUSLY rather than fail. The unbound
      -- count is appended after, so the honest number is present without silently
      -- disarming an existing gate.
      out := out ++ s!"\nSummary: {verified.length} verified, {failed.length} failed"
      if !unboundChecked.isEmpty then
        out := out ++ s!"; {unboundChecked.length} unbound (type-checked, NOT proved)"
      if generalFailure then
        -- "0 verified, 0 failed" alone reads like nothing was wrong. Say how many claims went
        -- unjudged, so the zero cannot be mistaken for a clean run.
        out := out ++ s!" (general compilation error; {notAttempted.length} NOT ATTEMPTED)"
      if !unboundChecked.isEmpty then
        -- Enforcement lives in the policy gate, which fails closed on `.unbound`
        -- (E0612). Say so, so a reader does not mistake exit 0 here for "shippable".
        out := out ++ "\n  NOTE: an unbound claim is not evidence. `[policy] require-proofs`"
        out := out ++ " rejects these (E0612)."
      IO.println out
      -- ENSURES REJECTIONS COUNT TOWARD THE VERDICT. A contract-discharge theorem the kernel
      -- refused was rendered with an X and then exited 0, so `--report check-proofs` reported
      -- success on a program whose `#[ensures]` obligation had no accepted proof. The verified/
      -- failed COUNTS stay refinement-only because gates extract them by exact string; the
      -- VERDICT is over every target the kernel judged, which is what an exit code is for.
      return (if failed.isEmpty && res.rejected.isEmpty && !generalFailure then 0 else 1)
    if reportType == "diagnostics-json" then
      IO.println (Report.diagnosticsJson validCore.coreModules locMap (registry := registry) (pc := pc))
      return 0
    if reportType == "schema" then
      IO.println Report.schemaReport
      return 0
    if reportType == "diagnostic-codes" then
      IO.println Report.diagnosticCodesReport
      return 0
    if reportType == "effects" then
      IO.println (Report.effectsReport validCore.coreModules locMap pc)
      return 0
    if reportType == "arithmetic" then
      IO.println (Report.arithmeticReport validCore.coreModules locMap)
      return 0
    if reportType == "recursion" then
      IO.println (Report.recursionReport pc)
      return 0
    if reportType == "stack-depth" then
      IO.println (Report.stackDepthReport validCore.coreModules locMap pc)
      return 0
    if reportType == "fingerprints" then
      IO.println (Report.fingerprintReport pc)
      return 0
    if reportType == "consistency" then
      let violations := pc.selfCheck
      IO.println (ConsistencyViolation.render violations)
      if violations.isEmpty then return 0 else return 1
    if reportType == "audit" then
      -- fold the kernel-checked VC discharge into the reviewer artifact (no
      -- external solver by default — SMT evidence stays opt-in via --report vcs --smt).
      -- Phase 3 #18d: one record type — the audit VC summary reads the obligation
      -- schedule directly.
      let auditVCs ← computeVCsDischarged parsed.modules locMap registry
      let vcSum := Report.vcAuditSummary auditVCs
      let assumptions := Assumptions.build (fullValidCore.coreModules ++ depModules) assumptionPackageOf assumptionPkgDefault
      IO.println (Report.auditReport validCore.coreModules locMap srcMap (registry := registry) (pc := pc) (vcSummary := vcSum)
        (assumptions := assumptions) (depsLoaded := !depModules.isEmpty))
      if Report.hasContracts parsed.modules then
        IO.println (← renderContracts parsed.modules registry locMap)
      return (if hasRegistryErrors then 1 else 0)
    if reportType == "verify" then
      -- Pass-by-pass verify gates: post-elab, post-mono, post-lower,
      -- post-cleanup. Each gate's diagnostics are reported separately
      -- per docs/verification/VERIFY_GATES.md. Earlier failures (mono refusing to
      -- run, lower aborting entirely) surface via the standard
      -- Pipeline.* error path; runVerifyGates short-circuits at the
      -- first such failure.
      let r := Pipeline.runVerifyGates validCore
      IO.println (renderVerifyGates r.postElab r.postMono r.postLower r.postCleanup)
      if r.errorCount > 0 then return 1 else return 0
    if reportType == "traceability" then
      match Pipeline.monomorphize validCore with
      | .error ds =>
        IO.eprintln (renderDiagnostics ds (sourceMap := srcMap))
        return 1
      | .ok mono =>
        match Pipeline.lower mono with
        | .error ds =>
          IO.eprintln (renderDiagnostics ds (sourceMap := srcMap))
          return 1
        | .ok ssa =>
          IO.println (Report.traceabilityReport validCore.coreModules mono.coreModules ssa.ssaModules locMap (registry := registry) (pc := pc))
          return 0
    if reportType == "mono" then
      match Pipeline.monomorphize validCore with
      | .error ds =>
        IO.eprintln (renderDiagnostics ds (sourceMap := srcMap))
        return 1
      | .ok mono =>
        IO.println (Report.monoReport validCore.coreModules mono.coreModules)
        return 0
    IO.eprintln s!"Unknown report type: {reportType}. Use: caps, unsafe, layout, interface, alloc, mono, authority, proof, eligibility, proof-status, obligations, extraction, proof-diagnostics, proof-deps, proof-bundle, lean-stubs, check-proofs, traceability, diagnostics-json, schema, diagnostic-codes, effects, recursion, fingerprints, consistency, vcs, obligation-ledger, backend-contracts, verify, audit, bool-kernel, vcgen-diff"
    return 1

def compileAndCheck (inputPath : String) (checkType : String) : IO UInt32 := do
  if checkType != "predictable" then
    IO.eprintln s!"Unknown check type: {checkType}. Use: predictable"
    return 1
  let inputDir := dirOf inputPath
  match ← findProjectRoot inputDir with
  | some root =>
    match ← loadProject root with
    | .error exitCode => return exitCode
    | .ok ctx =>
      let { validCore, parsed, allSrcMap, depNames, packageIdentity, .. } := ctx
      -- Drop dependency modules (std etc.) so the report focuses on
      -- user-package code.
      let userModules := validCore.coreModules.filter fun m => !depNames.contains m.name
      -- Build the ProofCore from the FULL user package: recursion
      -- classification, call graph, and extern info must reflect every
      -- caller — even ones in sibling files — for a per-file query to
      -- be accurate.
      let fullUserValidCore : ValidatedCore := { validCore with coreModules := userModules }
      let locMap := Report.buildFnLocMap parsed.modules inputPath
      let simpleLocMap := locMap.map fun (e : Report.FnLocEntry) => (e.qualName, (e.file, e.fnSpan.line))
      -- ONE PRODUCER: manifest identity when a project is in scope, synthetic otherwise. A refusal
      -- ABORTS rather than substituting a placeholder: every definition identity minted from this ProofCore is package-scoped, and an
      -- "unidentified" fallback would be shared by every such compilation, reintroducing the
      -- collision this migration removes.
      let pc ← match extractProofCore? fullUserValidCore
          -- PROJECT path, not standalone: use the identity `loadProject` already computed from the
          -- manifest rather than synthesizing one. Synthesizing here would be a second producer of
          -- package identity for a package that has a declared one.
          packageIdentity
          simpleLocMap with
        | .ok pc => pure pc
        | .error w => IO.eprintln s!"error: {w.explain}"; return 1
      -- Scope to the specific file the user invoked for the report
      -- output itself.
      let scopedModules ← scopeUserModulesToFile userModules inputPath root
      let (pass, report) := Report.checkPredictable scopedModules locMap allSrcMap pc
      IO.println report
      return if pass then 0 else 1
  | none =>
    -- Standalone mode (no Concrete.toml in the ancestor chain).
    let source ← readFile inputPath
    match ← Pipeline.runFrontend inputPath source resolveAllModules with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := [(inputPath, source)]))
      return 1
    | .ok (parsed, _, validCore, _) =>
      let locMap := Report.buildFnLocMap parsed.modules inputPath
      let simpleLocMap := locMap.map fun (e : Report.FnLocEntry) => (e.qualName, (e.file, e.fnSpan.line))
      -- ONE PRODUCER: manifest identity when a project is in scope, synthetic otherwise. A refusal
      -- ABORTS rather than substituting a placeholder: every definition identity minted from this ProofCore is package-scoped, and an
      -- "unidentified" fallback would be shared by every such compilation, reintroducing the
      -- collision this migration removes.
      let packageIdentity ← resolvePackageIdentity inputPath (validCore.coreModules.map (·.name))
        [(inputPath, source)]
      let pc ← match extractProofCore? validCore packageIdentity simpleLocMap with
        | .ok pc => pure pc
        | .error w => IO.eprintln s!"error: {w.explain}"; return 1
      let srcMap : SourceMap := [(inputPath, source)]
      let (pass, report) := Report.checkPredictable validCore.coreModules locMap srcMap pc
      IO.println report
      return if pass then 0 else 1

/-- Compile a project from Concrete.toml. -/
def compileBuild (projectRoot : String) (outputPath : Option String) (emitLLVM : Bool) (quiet : Bool := false) : IO UInt32 := do
  match ← loadProject projectRoot (stripTestFns := true) with
  | .error exitCode => return exitCode
  | .ok ctx =>
    let { validCore, parsed, allSrcMap, tomlContent, mainPath, depNames, policy, policyWarnings,
          policyLocMap, registry, pc, .. } := ctx
    -- [policy], location map, registry, and ProofCore all come from loadProject.
    for w in policyWarnings do IO.eprintln w
    if !(← enforceProvenRuntimeViolations parsed.modules allSrcMap) then
      return 1
    if !policy.isEmpty then
      let (vac, asm, st, cap, btk, ekr) ← computePolicyQuals policy parsed.modules depNames policyLocMap registry
      let policyDs := enforcePolicy policy validCore.coreModules
        (locMap := policyLocMap) (pc := pc) (depNames := depNames) (vacuousQuals := vac)
        (assumeQuals := asm) (solverTrustedQuals := st) (cappedObligations := cap)
        (belowTwoKernelQuals := btk) (externalKernelRan := ekr)
      if hasErrors policyDs then
        IO.eprintln (renderDiagnostics policyDs (sourceMap := allSrcMap))
        return 1
    match Pipeline.monomorphize validCore with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return 1
    | .ok mono =>
    match Pipeline.lower mono with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return 1
    | .ok ssa =>
      let llvmIR := Pipeline.emit ssa
      let llPath := mainPath ++ ".ll"
      writeFile llPath llvmIR
      if emitLLVM then
        IO.println llvmIR
        return 0
      -- Validate LLVM IR (if llvm-as available)
      let llValid ← validateLLVMIR llPath
      if !llValid then
        IO.FS.removeFile ⟨llPath⟩
        return 1
      -- Determine output path
      let outPath := match outputPath with
        | some p => p
        | none =>
          -- Extract package name from Concrete.toml
          let nameLines := tomlContent.splitOn "\n" |>.filter (·.trimAscii.toString.startsWith "name")
          match nameLines with
          | l :: _ =>
            match l.splitOn "\"" with
            | _ :: n :: _ => n
            | _ => "a.out"
          | _ => "a.out"
      let args ← clangArgs llPath outPath
      let exitCode ← runCmd "clang" args
      if exitCode != 0 then
        IO.eprintln "clang compilation failed"
        return exitCode
      IO.FS.removeFile ⟨llPath⟩
      if !quiet then
        IO.println s!"Built {outPath}"
        -- Print proof summary line (user package only)
        IO.println (Report.proofSummaryLine (pc.scopeToUser depNames))
      return 0

/-- Run tests for a project from Concrete.toml. Like compileBuild but in test mode. -/
partial def compileTestBuild (projectRoot : String) (moduleFilter : Option String := none) : IO UInt32 := do
  match ← loadProject projectRoot with
  | .error exitCode => return exitCode
  | .ok ctx =>
    let { validCore, parsed, allSrcMap, mainPath, depNames, policy, policyWarnings,
          policyLocMap, registry, pc, .. } := ctx
    -- [policy], location map, registry, and ProofCore all come from loadProject.
    for w in policyWarnings do IO.eprintln w
    if !(← enforceProvenRuntimeViolations parsed.modules allSrcMap) then
      return 1
    if !policy.isEmpty then
      let (vac, asm, st, cap, btk, ekr) ← computePolicyQuals policy parsed.modules depNames policyLocMap registry
      let policyDs := enforcePolicy policy validCore.coreModules
        (locMap := policyLocMap) (pc := pc) (depNames := depNames) (vacuousQuals := vac)
        (assumeQuals := asm) (solverTrustedQuals := st) (cappedObligations := cap)
        (belowTwoKernelQuals := btk) (externalKernelRan := ekr)
      if hasErrors policyDs then
        IO.eprintln (renderDiagnostics policyDs (sourceMap := allSrcMap))
        return 1
    match Pipeline.monomorphize validCore with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return 1
    | .ok mono =>
    match Pipeline.lower mono with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return 1
    | .ok ssa =>
      let llvmIR := Pipeline.emit ssa (testMode := true) (moduleFilter := moduleFilter)
      let llPath := mainPath ++ ".test.ll"
      let outPath := "/tmp/concrete_test_" ++ toString (← IO.monoMsNow)
      writeFile llPath llvmIR
      -- Validate LLVM IR (if llvm-as available)
      let llValid ← validateLLVMIR llPath
      if !llValid then return 1
      let args ← clangArgs llPath outPath
      let exitCode ← runCmd "clang" args
      if exitCode != 0 then
        IO.eprintln "clang compilation failed"
        IO.eprintln s!"LLVM IR left at: {llPath}"
        return exitCode
      let child ← IO.Process.spawn {
        cmd := outPath
        stdout := .piped
        stderr := .piped
      }
      let stdout ← child.stdout.readToEnd
      let stderr ← child.stderr.readToEnd
      let testExit ← child.wait
      IO.print stdout
      if !stderr.isEmpty then IO.eprint stderr
      if testExit != 0 then
        IO.eprintln s!"Test binary exited with code {testExit}"
        IO.eprintln s!"LLVM IR at: {llPath}"
        IO.eprintln s!"Binary at: {outPath}"
      else
        IO.FS.removeFile ⟨llPath⟩
        try IO.FS.removeFile ⟨outPath⟩ catch _ => pure ()
      return testExit

/-- `concrete prove --help=agent`: the agent entrypoint. Prints the
    proof-authoring sequence, stable output formats, the exit-code taxonomy, and
    the next command to run for each common status. -/
def proveAgentHelp : String := String.intercalate "\n" ([
  "concrete prove — agent guide (schema v" ++ proveSchemaVersion ++ ")",
  "",
  "PURPOSE: author and verify a Lean proof for one Concrete function.",
  "",
  "WORKFLOW (each step prints what to do next):",
  "  1. concrete prove <file> <module.fn> --json     # proof context + next_actions",
  "  2. concrete prove <file> <module.fn> --show-obligation <id>   # one obligation",
  "     concrete prove <file> <module.fn> --nearest-lemmas [<id>]  # tactic/lemma hints (all, or one obligation)",
  "  3. concrete prove <file> <module.fn> --emit-lean             # compilable Lean stub",
  "     concrete prove <file> <module.fn> --emit-artifacts        # one bundle per UNPROVED obligation",
  "  4. write the Lean proof in Concrete/Proof.lean (see PROOFKIT_GUIDE)",
  "  5. concrete prove <file> <module.fn> --emit-link             # in-source link block",
  "     (paste #[spec]/#[proof_by]/#[proof_coverage]/#[proof_fingerprint] above the fn)",
  "  6. concrete prove <file> <module.fn> --check [--json]        # kernel-verify THIS fn's theorem",
  "     concrete <file> --report check-proofs                     # kernel-verify all linked proofs",
  "  7. concrete prove <file> <module.fn> --replay                # re-run omega/bv_decide",
  "",
  "ALL-IN-ONE:",
  "  concrete prove <file> <module.fn> --workspace [dir]          # generate a self-contained",
  "    proof workspace (manifest.json, context.json, obligations/, <Fn>Proofs.lean, link.con.txt,",
  "    check.sh, replay.sh, README.md). Disposable build output; default dir .build/prove/<fn>/workspace.",
  "",
  "DISCOVERY (no file needed):",
  "  concrete prove --capabilities      # JSON: supported features + schema version",
  "  concrete prove --schema            # JSON schema for --json output",
  "  concrete prove --help=agent        # this guide",
  "",
  "OUTPUT FORMATS: text (default) or JSON (--json on prove/--show-obligation/--replay).",
  "  JSON is stable and carries the same obligation ids/statuses as",
  "  `--report contracts` and `concrete audit`. Every JSON response has next_actions.",
  "",
  ] ++ ExitCode.helpBlock ++ [
  "",
  "PROOF MODEL: in-source links only (no proof-registry.json). A function carries",
  "  #[spec]/#[proof_by]/#[ensures_proof]/#[proof_coverage]/#[proof_fingerprint]." ])

/-- `concrete prove --capabilities`: machine-readable feature discovery. -/
def proveCapabilitiesJson : String := String.intercalate "\n" [
  "{",
  "  \"schema_version\": \"" ++ proveSchemaVersion ++ "\",",
  "  \"features\": {",
  "    \"prove_json\": true,",
  "    \"show_obligation_json\": true,",
  "    \"emit_lean\": true,",
  "    \"emit_link\": true,",
  "    \"nearest_lemmas\": true,",
  "    \"replay_json\": true,",
  "    \"failed_artifacts\": true,",
  "    \"check\": true,",
  "    \"workspace\": true",
  "  },",
  "  \"obligation_kinds\": [\"invariant_init\", \"invariant_preservation\", \"loop_exit_post_link\", \"variant_nonnegative\", \"variant_decreases\", \"array_bounds\", \"division_nonzero\", \"integer_overflow\", \"ensures\"],",
  "  \"evidence_classes\": [\"proved_by_lean\", \"proved_by_kernel_decision\", \"partial\", \"stale\", \"missing\", \"blocked\", \"ineligible\", \"trusted\", \"tested_by_oracle\", \"runtime_checked\"],",
  "  \"proof_model\": \"in_source_links\",",
  "  \"link_attributes\": [\"spec\", \"proof_by\", \"ensures_proof\", \"proof_coverage\", \"proof_fingerprint\"],",
  "  \"mcp_available\": false",
  "}" ]

/-- `concrete prove --schema`: JSON schema (+ version) for `--json` output. -/
def proveSchemaJson : String := String.intercalate "\n" [
  "{",
  "  \"schema_version\": \"" ++ proveSchemaVersion ++ "\",",
  "  \"title\": \"concrete prove --json\",",
  "  \"type\": \"object\",",
  "  \"properties\": {",
  "    \"schema_version\": {\"type\": \"string\"},",
  "    \"function\": {\"type\": \"string\", \"description\": \"qualified name\"},",
  "    \"eligible\": {\"type\": \"boolean\"},",
  "    \"exclusion_reason\": {\"type\": [\"string\", \"null\"]},",
  "    \"body_fingerprint\": {\"type\": \"string\"},",
  "    \"proof_link\": {\"type\": [\"object\", \"null\"], \"description\": \"spec, proof, ensures_proof, coverage, fingerprint, origin (source_linked|hardcoded)\"},",
  "    \"status\": {\"type\": \"string\", \"enum\": [\"proved\", \"stale\", \"missing\", \"blocked\", \"ineligible\", \"trusted\"]},",
  "    \"evidence_class\": {\"type\": \"string\"},",
  "    \"obligations\": {\"type\": \"array\", \"items\": {\"type\": \"object\", \"description\": \"id, kind, status, hypotheses, conclusion, engine, theorem_shape\"}},",
  "    \"replay_command\": {\"type\": \"string\"},",
  "    \"proofkit_imports\": {\"type\": \"array\", \"items\": {\"type\": \"string\"}},",
  "    \"suggested_theorems\": {\"type\": \"array\", \"items\": {\"type\": \"string\"}},",
  "    \"next_actions\": {\"type\": \"array\", \"items\": {\"type\": \"object\", \"description\": \"kind, command, output_format, resolves\"}}",
  "  },",
  "  \"required\": [\"schema_version\", \"function\", \"status\", \"next_actions\"]",
  "}" ]

/-- Shared project-root prologue (ROADMAP Phase 4 #14): discover the enclosing
    project root, or emit the one canonical "no Concrete.toml" diagnostic and exit
    with `ExitCode.usage`. Every project-scoped command goes through here, so the
    discovery, the message, and the exit code are defined once. -/
def withProjectRoot (k : String → IO UInt32)
    (hint : String := "create one with [package] section, or run from a project directory") : IO UInt32 := do
  let cwd ← IO.currentDir
  match ← findProjectRoot cwd.toString with
  | none =>
    IO.eprintln s!"error: no Concrete.toml found in current directory or parent\nhint: {hint}"
    return ExitCode.usage
  | some root => k root

/-- `concrete <file> --report profile [--profile <name>]` — ROADMAP #10 Stage 1
    (profile mechanism only; no compilation, no codegen). Resolve the active build
    profile with precedence CLI flag > Concrete.toml `[profile]` > default, then
    print its policy bundle. An unknown profile name is rejected. -/
def reportProfile (inputPath : String) (cliProfile : Option String) : IO UInt32 := do
  match cliProfile with
  | some nm =>
    -- 1. CLI flag wins.
    match Concrete.BuildProfile.ofString? nm with
    | some p => IO.println (Concrete.renderProfileReport p .cli); return 0
    | none =>
      IO.eprintln s!"error: unknown profile '{nm}' (known: {Concrete.BuildProfile.known})"
      return ExitCode.usage
  | none =>
    -- 2. Concrete.toml [profile] name, if the file is inside a project.
    let manifestName ← (do
      match ← findProjectRoot (dirOf inputPath) with
      | some root =>
        let tomlPath := root ++ "/Concrete.toml"
        if ← System.FilePath.pathExists tomlPath then
          return Concrete.parseProfileName (← IO.FS.readFile tomlPath)
        else return none
      | none => return none)
    match manifestName with
    | some nm =>
      match Concrete.BuildProfile.ofString? nm with
      | some p => IO.println (Concrete.renderProfileReport p .manifest); return 0
      | none =>
        IO.eprintln s!"error: Concrete.toml [profile] name '{nm}' is unknown (known: {Concrete.BuildProfile.known})"
        return ExitCode.usage
    | none =>
      -- 3. Default profile.
      IO.println (Concrete.renderProfileReport Concrete.defaultProfile .default)
      return 0

def main (args : List String) : IO UInt32 := do
  -- Phase 6E #1: help is the most reliable command — it must work from ANY
  -- directory, before any file/project parsing, and never throw. `concrete
  -- --help`, `concrete help`, and `concrete <cmd> --help` all reach here first.
  if args.head? == some "--help" || args.head? == some "help" || args.head? == some "-h"
     || args.contains "--help" then
    IO.println helpText
    return 0
  -- Phase 6E #3: daily command aliases. Normalized to the legacy flag spelling
  -- UP FRONT, so an alias is byte-identical to the flag form by construction
  -- (`concrete report caps f.con` == `concrete f.con --report caps`;
  -- `concrete trace f.con [--json]` == `concrete f.con --trace-pipeline`).
  -- Legacy spellings keep working untouched (6E #4 compatibility).
  let args := match args with
    | "report" :: kind :: file :: rest => file :: "--report" :: kind :: rest
    | "trace" :: file :: rest => file :: "--trace-pipeline" :: rest.filter (· != "--json")
    | a => a
  -- `concrete --report compiler-ledger [--json]` — the project-scoped non-proof
  -- fact store (Phase 4 #2). Project mode: loads the one ProjectContext and renders
  -- its CompilerLedger (toolchain id filled here, lazily).
  if args.head? == some "--report" && args[1]? == some "compiler-ledger" then
    let json := Cli.hasFlag args "--json"
    return ← withProjectRoot (hint := "`--report compiler-ledger` reads a project; run from a project directory") fun root => do
      match ← loadProject root with
      | .error exitCode => return exitCode
      | .ok ctx =>
        let toolId ← compilerIdentity
        let l := { ctx.ledger with toolchainId := toolId }
        IO.println (if json then l.toJson 1 else l.render)
        return 0
  -- Check for "build" command first (before generic single-arg pattern)
  if args.head? == some "build" then
    return ← withProjectRoot fun root => do
      let emitLLVM := Cli.hasFlag args "--emit-llvm"
      let outPath := Cli.flagValue args "-o"
      compileBuild root outPath emitLLVM
  -- concrete run [-- args...]
  if args.head? == some "run" then
    return ← withProjectRoot fun root => do
      -- Build to a temporary path
      let tmpBin := "/tmp/concrete_run_" ++ toString (← IO.monoMsNow)
      let buildResult ← compileBuild root (some tmpBin) (emitLLVM := false) (quiet := true)
      if buildResult != 0 then return buildResult
      -- Extract user args after "--"
      let userArgs := match args.dropWhile (· != "--") with
        | _ :: rest => rest
        | [] => []
      -- Run the built binary
      let child ← IO.Process.spawn {
        cmd := tmpBin
        args := userArgs.toArray
        stdin := .inherit
        stdout := .inherit
        stderr := .inherit
      }
      let exitCode ← child.wait
      -- Clean up temp binary
      try IO.FS.removeFile ⟨tmpBin⟩ catch _ => pure ()
      return exitCode
  -- concrete test [--module <name>]
  if args.head? == some "test" then
    return ← withProjectRoot fun root => do
      let modFilter := Cli.flagValue args "--module"
      compileTestBuild root modFilter
  -- concrete check — run frontend + proof status without codegen
  if args.head? == some "check" then
    return ← withProjectRoot fun root => do
      match ← loadProject root (stripTestFns := true) with
      | .error exitCode => return exitCode
      | .ok ctx =>
        let { validCore, parsed, allSrcMap, depNames, policy, policyWarnings,
              policyLocMap, registry, pc, ledger, .. } := ctx
        -- [policy], location map, registry, ProofCore, and diagnostics all come from
        -- loadProject. Registry diagnostics are rendered FROM the ledger (Phase 4 #4).
        for w in policyWarnings do IO.eprintln w
        -- PROJECT-MODE trust edges. Single-file mode elaborates one module, so the
        -- dependency's functions are absent and the report looked empty for a
        -- std-using program — the same one-module blind spot behind R-0484's
        -- cross-package opacity. In project mode the dependency IS checked (its
        -- diagnostics arrive prefixed `[std] [string]`), so its edges exist here.
        if args.contains "--report" && args.contains "trust-edges" then
          IO.println (Report.trustEdgeReport validCore.coreModules)
          return 0
        if !(← enforceProvenRuntimeViolations parsed.modules allSrcMap) then
          return 1
        for d in ledger.diagnostics do
          if d.code == "registry" then IO.eprintln d.message
        if !policy.isEmpty then
          let (vac, asm, st, cap, btk, ekr) ← computePolicyQuals policy parsed.modules depNames policyLocMap registry
          let policyDs := enforcePolicy policy validCore.coreModules
            (locMap := policyLocMap) (pc := pc) (depNames := depNames) (vacuousQuals := vac)
            (assumeQuals := asm) (solverTrustedQuals := st) (cappedObligations := cap)
            (belowTwoKernelQuals := btk) (externalKernelRan := ekr)
          if hasErrors policyDs then
            IO.eprintln (renderDiagnostics policyDs (sourceMap := allSrcMap))
            return 1
        -- Scope to user package for reporting and exit code
        let userModules := validCore.coreModules.filter fun m => !depNames.contains m.name
        let userPc := pc.scopeToUser depNames
        -- Print proof status report (user package only)
        let srcMap := ctx.allSrcMap
        IO.println (Report.proofStatusReport userModules policyLocMap srcMap (registry := registry) (pc := userPc))
        -- Print next steps if any
        let nextSteps := Report.proofNextSteps userPc
        if !nextSteps.isEmpty then IO.println nextSteps
        -- Show dependency context
        let depOblCount := pc.obligations.length - userPc.obligations.length
        if depOblCount > 0 then
          IO.println s!"\n({depOblCount} dependency obligations hidden — use --report proof-status for full view)"
        -- Exit code: 0 if all user-package eligible proved, 1 if any stale/missing/blocked
        -- EVERY NON-CURRENT STATUS COUNTS AS AN ISSUE, enumerated through `isCurrentForDependents`
        -- rather than by listing three constructors. The list was `stale || missing || blocked`, so
        -- `concrete check` exited 0 on a project containing an unjustified closure — a false green in
        -- the command a release gate runs — and would have done the same for `unbound`,
        -- `needsRecheck` and `depsNotCurrent`. A hand-written subset of a status vocabulary is the
        -- restated-fact hazard this file keeps finding; derive it from the predicate that already
        -- decides what may be relied upon.
        let hasIssues := userPc.obligations.any fun o =>
          !o.status.isCurrentForDependents && o.status != .ineligible
        let hasRegistryErrors := ledger.diagnostics.any (fun d => d.code == "registry" && d.severity == "error")
        return (if hasIssues || hasRegistryErrors then 1 else 0)
  -- concrete diff <old.json> <new.json> [--json]
  if args.head? == some "diff" then
    let rest := args.drop 1
    match rest with
    | [oldPath, newPath] =>
      let oldJson ← try pure (some (← readFile oldPath)) catch _ => pure none
      let newJson ← try pure (some (← readFile newPath)) catch _ => pure none
      match oldJson, newJson with
      | none, _ =>
        IO.eprintln s!"error: file not found: {oldPath}"
        return 1
      | _, none =>
        IO.eprintln s!"error: file not found: {newPath}"
        return 1
      | some oldJ, some newJ =>
      let (oldParsed, oldWarns) := Report.parseFactsWarn oldJ
      let (newParsed, newWarns) := Report.parseFactsWarn newJ
      for w in oldWarns do IO.eprintln s!"{oldPath}: {w}"
      for w in newWarns do IO.eprintln s!"{newPath}: {w}"
      match oldParsed, newParsed with
      | some oldFacts, some newFacts =>
        match Report.diffFacts oldFacts newFacts with
        | .error e =>
          IO.eprintln s!"error: {e}"
          return 2
        | .ok entries =>
          IO.println (Report.renderDiffReport entries)
          return (if entries.any (·.drift == "weakened") then 1 else 0)
      | none, _ =>
        IO.eprintln s!"error: could not parse JSON from {oldPath}"
        return 1
      | _, none =>
        IO.eprintln s!"error: could not parse JSON from {newPath}"
        return 1
    | [oldPath, newPath, "--json"] =>
      let oldJson2 ← try pure (some (← readFile oldPath)) catch _ => pure none
      let newJson2 ← try pure (some (← readFile newPath)) catch _ => pure none
      match oldJson2, newJson2 with
      | none, _ =>
        IO.eprintln s!"error: file not found: {oldPath}"
        return 1
      | _, none =>
        IO.eprintln s!"error: file not found: {newPath}"
        return 1
      | some oldJ2, some newJ2 =>
      let (oldParsed2, oldWarns2) := Report.parseFactsWarn oldJ2
      let (newParsed2, newWarns2) := Report.parseFactsWarn newJ2
      for w in oldWarns2 do IO.eprintln s!"{oldPath}: {w}"
      for w in newWarns2 do IO.eprintln s!"{newPath}: {w}"
      match oldParsed2, newParsed2 with
      | some oldFacts, some newFacts =>
        match Report.diffFacts oldFacts newFacts with
        | .error e =>
          IO.eprintln s!"error: {e}"
          return 2
        | .ok entries =>
          IO.println (Report.renderDiffJson entries)
          return (if entries.any (·.drift == "weakened") then 1 else 0)
      | none, _ =>
        IO.eprintln s!"error: could not parse JSON from {oldPath}"
        return 1
      | _, none =>
        IO.eprintln s!"error: could not parse JSON from {newPath}"
        return 1
    | _ =>
      IO.eprintln "Usage: concrete diff <old.json> <new.json> [--json]"
      return 1
  -- concrete snapshot <file.con> [-o output.json]
  if args.head? == some "snapshot" then
    let rest := args.drop 1
    let inputPath := match rest with
      | p :: _ => some p
      | [] => none
    match inputPath with
    | none =>
      IO.eprintln "Usage: concrete snapshot <file.con> [-o output.json]"
      return 1
    | some inp =>
      let outputPath := match rest with
        | _ :: "-o" :: p :: _ => p
        | _ =>
          if inp.endsWith ".con" then
            String.ofList (inp.toList.take (inp.length - 4)) ++ ".facts.json"
          else inp ++ ".facts.json"
      let source ← readFile inp
      let mainSrcMap : SourceMap := [(inp, source)]
      match ← Pipeline.runFrontend inp source resolveAllModules with
      | .error ds =>
        IO.eprintln (renderDiagnostics ds (sourceMap := mainSrcMap))
        return 1
      | .ok (parsed, _, validCore, _srcMap) =>
        let locMap := Report.buildFnLocMap parsed.modules inp
        let simpleLocMap := locMap.map fun e => (e.qualName, (e.file, e.fnSpan.line))
        let registry ← loadRegistryWithLinks inp parsed.modules validCore.coreModules
        -- ONE PRODUCER: manifest identity when a project is in scope, synthetic otherwise. A refusal
        -- ABORTS rather than substituting a placeholder: every definition identity minted from this ProofCore is package-scoped, and an
        -- "unidentified" fallback would be shared by every such compilation, reintroducing the
        -- collision this migration removes.
        let packageIdentity ← resolvePackageIdentity inp (validCore.coreModules.map (·.name))
          [(inp, source)]
        let pc ← match extractProofCore? validCore packageIdentity simpleLocMap registry with
          | .ok pc => pure pc
          | .error w => IO.eprintln s!"error: {w.explain}"; return 1
        -- Collect core facts (same as diagnostics-json) plus source-contract facts
        -- (AST metadata, absent from Core) so `concrete diff` can detect contract
        -- API drift (precondition strengthening, postcondition weakening).
        let coreFacts := Report.collectCoreFacts validCore.coreModules locMap registry pc
          ++ Report.collectContractFacts parsed.modules
        -- Run backend pipeline for traceability facts
        let (traceFacts, traceWarns) ← match Pipeline.monomorphize validCore with
          | .error ds =>
            let msg := ds.map (·.message) |> ", ".intercalate
            pure ([], [s!"warning: snapshot incomplete — monomorphization failed: {msg}"])
          | .ok mono =>
            match Pipeline.lower mono with
            | .error ds =>
              let msg := ds.map (·.message) |> ", ".intercalate
              pure ([], [s!"warning: snapshot incomplete — lowering failed: {msg}"])
            | .ok ssa =>
              pure (Report.collectTraceabilityFacts
                validCore.coreModules mono.coreModules ssa.ssaModules locMap (registry := registry) (pc := pc), [])
        for w in traceWarns do IO.eprintln w
        -- Generate timestamp
        let ms ← IO.monoMsNow
        let timestamp := toString ms
        let json := Report.snapshotJson inp timestamp coreFacts traceFacts
        writeFile outputPath json
        let factCount := coreFacts.length + traceFacts.length
        IO.println s!"Snapshot written: {outputPath} ({factCount} facts)"
        return 0
  -- concrete debug-bundle <file.con> [-o dir]
  if args.head? == some "debug-bundle" then
    let rest := args.drop 1
    let inputPath := match rest with
      | p :: _ => some p
      | [] => none
    match inputPath with
    | none =>
      IO.eprintln "Usage: concrete debug-bundle <file.con> [-o dir]"
      return 1
    | some inp =>
      let bundleDir := match rest with
        | _ :: "-o" :: p :: _ => p
        | _ =>
          if inp.endsWith ".con" then
            String.ofList (inp.toList.take (inp.length - 4)) ++ ".debug-bundle"
          else inp ++ ".debug-bundle"
      let source ← readFile inp
      let compilerVersion ← compilerIdentity
      let st ← DebugBundle.capturePipeline inp source resolveAllModules
      DebugBundle.writeBundle bundleDir st compilerVersion
      let status := match st.failStage with
        | some stage => s!"failed at {stage}"
        | none => "complete (no failure)"
      IO.println s!"Debug bundle written: {bundleDir}/ — {status}"
      return (if st.failStage.isSome then 1 else 0)
  -- concrete validate-bundle <dir>
  if args.head? == some "validate-bundle" then
    let rest := args.drop 1
    match rest with
    | [bundleDir] =>
      let issues ← DebugBundle.validateBundle bundleDir
      if issues.isEmpty then
        IO.println s!"Bundle {bundleDir} is valid."
        return 0
      else
        for issue in issues do IO.eprintln issue
        let errors := issues.filter (·.startsWith "error")
        if errors.isEmpty then
          IO.println s!"Bundle {bundleDir} has {issues.length} warning(s)."
          return 0
        else
          IO.eprintln s!"Bundle {bundleDir} has {errors.length} error(s)."
          return 1
    | _ =>
      IO.eprintln "Usage: concrete validate-bundle <dir>"
      return 1
  -- concrete reduce <file.con> --predicate <pred> [-o output] [--verbose]
  if args.head? == some "reduce" then
    let rest := args.drop 1
    let inputPath := match rest with
      | p :: _ => some p
      | [] => none
    match inputPath with
    | none =>
      IO.eprintln "Usage: concrete reduce <file.con> --predicate <pred> [-o output] [--verbose]"
      IO.eprintln "Predicates: parse-error, resolve-error, check-error, elab-error, core-check-error,"
      IO.eprintln "            mono-error, lower-error, consistency-violation, verify-warning, crash"
      IO.eprintln "Substring match: check-error:expected Int"
      IO.eprintln "External:        external:scripts/reduce/expect-error-code.sh E0708"
      IO.eprintln "                 (the candidate file is appended as the final argument)"
      return 1
    | some inp =>
      let predStr := match rest with
        | _ :: "--predicate" :: p :: _ => some p
        | _ => none
      match predStr with
      | none =>
        IO.eprintln "error: --predicate is required"
        return 1
      | some ps =>
        match Reduce.parsePredicate ps with
        | none =>
          IO.eprintln s!"error: unknown predicate '{ps}'"
          return 1
        | some pred =>
          let outputPath := match rest with
            | _ :: _ =>
              let rec findO : List String → Option String
                | "-o" :: p :: _ => some p
                | _ :: rest => findO rest
                | [] => none
              findO rest
            | [] => none
          let outputPath := outputPath.getD (inp ++ ".reduced")
          let verbose := rest.any (· == "--verbose")
          let source ← readFile inp
          if verbose then IO.eprintln s!"Reducing {inp} with predicate '{ps}'..."
          let result ← Reduce.reduce source pred resolveAllModules verbose
          writeFile outputPath result
          let origLines := (source.splitOn "\n").length
          let redLines := (result.splitOn "\n").length
          IO.println s!"Reduced: {origLines} → {redLines} lines — {outputPath}"
          return 0
  -- concrete fmt — format source files (Phase 6 #1). CLI promotion of the legacy
  -- `--fmt` flag to a real subcommand. Default: format to stdout. `--check`: exit
  -- nonzero if formatting would change the file. `--write`: rewrite in place.
  -- `--stdin`: read stdin, write stdout. A file is "formatted" iff
  -- formatProgram(parse(it)) == it (formatProgram emits exactly one trailing \n).
  if args.head? == some "fmt" then
    let rest := args.drop 1
    let check := Cli.hasFlag rest "--check"
    let write := Cli.hasFlag rest "--write"
    let stdin := Cli.hasFlag rest "--stdin"
    if stdin then
      let source ← (← IO.getStdin).readToEnd
      match parse source with
      | .error e => IO.eprintln s!"parse error: {renderDiagnostics e}"; return 1
      | .ok modules => IO.print (formatProgram modules); return 0
    -- positional path = first non-flag argument
    match rest.filter (fun a => !a.startsWith "-") |>.head? with
    | none =>
      IO.eprintln "Usage: concrete fmt <file.con> [--check | --write | --stdin]"
      return 1
    | some path =>
      let source ← readFile path
      match parse source with
      | .error e => IO.eprintln s!"parse error in {path}: {renderDiagnostics e}"; return 1
      | .ok modules =>
        let formatted := formatProgram modules
        if check then
          if formatted == source then return 0
          else
            IO.eprintln s!"{path}: not formatted (run `concrete fmt --write {path}`)"
            return 1
        else if write then
          if formatted == source then return 0
          else
            writeFile path formatted
            IO.println s!"formatted {path}"
            return 0
        else
          IO.print formatted
          return 0
  -- concrete audit <file.con> — alias for `concrete <file> --report audit`
  if args.head? == some "audit" then
    match args.drop 1 with
    | [inputPath] => return (← compileAndReport inputPath "audit")
    | _ =>
      IO.eprintln "Usage: concrete audit <file.con>"
      return 1
  -- concrete prove <file.con> <module.function> [--out <path>] [--force]
  --   Read-only proof-scaffold generator. Prints to stdout; --out writes a
  --   stub file (refusing to overwrite without --force). Never auto-proves.
  if args.head? == some "prove" then
    let pargs := args.drop 1
    -- Discovery commands (no file/function needed) — the agent entrypoint.
    if Cli.hasFlag pargs "--help=agent" then IO.println proveAgentHelp; return 0
    if Cli.hasFlag pargs "--capabilities" then IO.println proveCapabilitiesJson; return 0
    if Cli.hasFlag pargs "--schema" then IO.println proveSchemaJson; return 0
    match pargs with
    | inputPath :: target :: rest =>
      let force := Cli.hasFlag rest "--force"
      let emitLink := Cli.hasFlag rest "--emit-link"
      let replay := Cli.hasFlag rest "--replay"
      let proveJson := Cli.hasFlag rest "--json"
      let nearestLemmas := Cli.hasFlag rest "--nearest-lemmas"
      let nearestId := Cli.flagValue rest "--nearest-lemmas"
      let emitLean := Cli.hasFlag rest "--emit-lean"
      let emitArtifacts := Cli.hasFlag rest "--emit-artifacts"
      let check := Cli.hasFlag rest "--check"
      -- `--workspace` with no value means "default workspace" (empty string sentinel).
      let workspace := if Cli.hasFlag rest "--workspace"
        then some (Cli.flagValue rest "--workspace" |>.getD "") else none
      let stdout := Cli.hasFlag rest "--stdout"
      let outDir := Cli.flagValue rest "--out-dir"
      let outPath := Cli.flagValue rest "--out"
      let showObl := Cli.flagValue rest "--show-obligation"
      return (← compileAndReport inputPath "prove"
        (proveTarget := some target) (proveOut := outPath) (proveForce := force)
        (proveEmitLink := emitLink) (proveShowObl := showObl) (proveReplay := replay)
        (proveJson := proveJson) (proveNearestLemmas := nearestLemmas)
        (proveEmitLean := emitLean) (proveStdout := stdout)
        (proveEmitArtifacts := emitArtifacts) (proveOutDir := outDir)
        (proveCheck := check) (proveWorkspace := workspace)
        (proveNearestId := nearestId))
    | _ =>
      IO.eprintln "Usage: concrete prove <file.con> <module.function> [--json] [--out <path>] [--force] [--emit-link] [--emit-lean] [--emit-artifacts] [--out-dir <dir>] [--show-obligation <id>] [--replay] [--nearest-lemmas] [--check] [--workspace <dir>]\n       concrete prove --help=agent | --capabilities | --schema   (discovery; no file needed)"
      return 1
  -- concrete --version
  if args == ["--version"] then
    let id ← compilerIdentity
    IO.println id
    return 0
  -- `concrete <file> --diagnostics-json` — frontend diagnostics as JSON, rendered
  -- from the SAME structured Diagnostic records the human output uses (Phase 4 #4).
  if args.length == 2 && args[1]? == some "--diagnostics-json" then
    let inputPath := args.headD ""
    let source ← readFile inputPath
    -- Error-tolerant path (Phase 4 #12a): a failing pass does not erase diagnostics
    -- from passes that can still run. Returns only diagnostics + a `partial` flag —
    -- structurally never a codegen/proof artifact.
    let (ds, isPartial) ← Pipeline.runFrontendDiagnostics inputPath source resolveAllModules
    IO.println (Concrete.diagnosticsToJson ds 1 isPartial)
    return (if Concrete.hasErrors ds then 1 else 0)
  -- Unknown command / missing input file (Phase 4 #15): the single-file dispatch
  -- below treats the first argument as a source path. If it is neither a recognized
  -- subcommand (all handled above) nor a readable file, fail cleanly here instead of
  -- letting the later `readFile` throw an uncaught exception.
  if let some first := args.head? then
    if !first.startsWith "-" && !(← System.FilePath.pathExists first) then
      IO.eprintln s!"error: '{first}' is not a readable file or a known command\nhint: run `concrete` with no arguments to see usage"
      return ExitCode.usage
  match args with
  | [] =>
    IO.eprintln usage
    return 1
  | [inputPath] =>
    let outputPath := if inputPath.endsWith ".con" then String.ofList (inputPath.toList.take (inputPath.length - 4)) else inputPath ++ ".out"
    compileSSA inputPath outputPath false
  | [inputPath, "--test"] =>
    compileTest inputPath
  | [inputPath, "--test", "--module", moduleName] =>
    compileTest inputPath (moduleFilter := some moduleName)
  | [inputPath, "--emit-llvm"] =>
    compileSSA inputPath "" true
  | [inputPath, "--interp"] =>
    interpProgram inputPath
  | [inputPath, "--emit-core"] =>
    compileAndEmit inputPath "core"
  | [inputPath, "--emit-ssa"] =>
    compileAndEmit inputPath "ssa"
  | [inputPath, "--emit-ssa-unverified"] =>
    compileAndEmit inputPath "ssa-unverified"
  | [inputPath, "--emit-trace-json"] =>
    traceTelemetry inputPath
  | [inputPath, "--trace-pipeline"] =>
    tracePipeline inputPath
  | [inputPath, "-o", outputPath] =>
    compileSSA inputPath outputPath false
  | [inputPath, "--check", checkType] =>
    compileAndCheck inputPath checkType
  -- ROADMAP #10 Stage 1: build-profile report (must precede the generic
  -- `--report <type>` arm; "profile" is not a compileAndReport report type).
  | [inputPath, "--report", "profile"] =>
    reportProfile inputPath none
  | [inputPath, "--report", "profile", "--profile", name] =>
    reportProfile inputPath (some name)
  | [inputPath, "--report", reportType] =>
    compileAndReport inputPath reportType
  | [inputPath, "--report", reportType, "--json"] =>
    compileAndReport inputPath reportType (reportJson := true)
  | [inputPath, "--report", reportType, "--emit-smt"] =>
    compileAndReport inputPath reportType (smtEmit := true)
  | [inputPath, "--report", reportType, "--emit-lean-replay"] =>
    compileAndReport inputPath reportType (emitLeanReplay := true)
  | [inputPath, "--report", reportType, "--rocq"] =>
    compileAndReport inputPath reportType (rocqRun := true)
  | [inputPath, "--report", reportType, "--isabelle"] =>
    compileAndReport inputPath reportType (isaRun := true)
  -- both orders accepted so flag order is not load-bearing
  -- Receipt storage. Writing is authority-bearing; reading consumes untrusted bytes. Separate flags
  -- so neither can be mistaken for the other.
  | [inputPath, "--report", reportType, "--out", path] =>
    compileAndReport inputPath reportType (receiptsOut := some path)
  | [inputPath, "--report", reportType, "--receipts", path] =>
    compileAndReport inputPath reportType (receiptsIn := some path)
  | [inputPath, "--report", reportType, "--rocq", "--isabelle"] =>
    compileAndReport inputPath reportType (rocqRun := true) (isaRun := true)
  | [inputPath, "--report", reportType, "--isabelle", "--rocq"] =>
    compileAndReport inputPath reportType (rocqRun := true) (isaRun := true)
  | [inputPath, "--report", reportType, "--all-provers"] =>
    compileAndReport inputPath reportType (rocqRun := true) (isaRun := true)
  | [inputPath, "--report", reportType, "--all-provers", "--require-two-kernels"] =>
    compileAndReport inputPath reportType (rocqRun := true) (isaRun := true) (reqTwoKernels := true)
  | [inputPath, "--report", reportType, "--rocq", "--require-two-kernels"] =>
    compileAndReport inputPath reportType (rocqRun := true) (reqTwoKernels := true)
  | [inputPath, "--report", reportType, "--smt"] =>
    compileAndReport inputPath reportType (smtRun := true)
  | [inputPath, "--report", reportType, "--smt", "--json"] =>
    compileAndReport inputPath reportType (reportJson := true) (smtRun := true)
  | [inputPath, "--report", reportType, "--smt", "--replay"] =>
    compileAndReport inputPath reportType (smtRun := true) (smtReplay := true)
  | [inputPath, "--report", reportType, "--smt", "--replay", "--json"] =>
    compileAndReport inputPath reportType (reportJson := true) (smtRun := true) (smtReplay := true)
  | [inputPath, "--report", reportType, "--smt", "--smt-timeout-ms", ms, "--json"] =>
    compileAndReport inputPath reportType (reportJson := true) (smtRun := true)
      (smtTimeoutMs := ms.toNat?)
  | [inputPath, "--report", reportType, "--smt", "--smt-timeout-ms", ms] =>
    compileAndReport inputPath reportType (smtRun := true) (smtTimeoutMs := ms.toNat?)
  | [inputPath, "--query", query] =>
    compileAndQuery inputPath query
  -- LEGACY: `--fmt` flag, superseded by the `concrete fmt` subcommand (Phase 6
  -- #1). Kept byte-identical for the golden-format baselines; documented as
  -- deprecated. New usage should prefer `concrete fmt`.
  | [inputPath, "--fmt"] =>
    let source ← readFile inputPath
    match parse source with
    | .error e =>
      IO.eprintln s!"parse error: {renderDiagnostics e}"
      return 1
    | .ok modules =>
      IO.print (formatProgram modules)
      return 0
  -- General prover-flag parsing for the multi-kernel family: ANY subset, ANY order.
  -- The exact-match patterns above enumerate combinations one by one, so a perfectly
  -- valid request like `--rocq --isabelle --require-two-kernels` fell through to the
  -- usage dump — and a caller checking only the exit code would read that usage error
  -- as a failed evidence gate. Placed after the specific patterns so they still win.
  | inputPath :: "--report" :: reportType :: rest =>
    -- `--json` is accepted alongside them: the multi-kernel provenance is recorded in
    -- the JSON artifact, so asking for both at once is the natural request.
    let proverFlags := ["--rocq", "--isabelle", "--all-provers", "--require-two-kernels", "--json"]
    if !rest.isEmpty && rest.all (fun a => proverFlags.contains a) then
      -- Flags go through Cli.hasFlag, not an inline `contains`: one definition of what
      -- a boolean flag means (ROADMAP Phase 4 #14b, enforced by check_cli_plumbing).
      let allProvers := Cli.hasFlag rest "--all-provers"
      compileAndReport inputPath reportType
        (rocqRun := allProvers || Cli.hasFlag rest "--rocq")
        (isaRun := allProvers || Cli.hasFlag rest "--isabelle")
        (reqTwoKernels := Cli.hasFlag rest "--require-two-kernels")
        (reportJson := Cli.hasFlag rest "--json")
    else do
      IO.eprintln usage
      return 1
  | _ =>
    IO.eprintln usage
    return 1
