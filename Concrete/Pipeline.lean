import Concrete.AST
import Concrete.Diagnostic
import Concrete.FileSummary
import Concrete.Resolve
import Concrete.Parser
import Concrete.Check
import Concrete.Core
import Concrete.Elab
import Concrete.CoreCanonicalize
import Concrete.CoreCheck
import Concrete.Mono
import Concrete.SSA
import Concrete.Lower
import Concrete.SSAVerify
import Concrete.SSACleanup
import Concrete.EmitSSA
import Concrete.Verify

namespace Concrete

/-! ## Pipeline — cacheable compiler artifacts

Each pipeline stage produces a named artifact type.
Runner functions wrap the underlying pass with diagnostic handling.
No serialization yet — these types are the prerequisite for future `ToJson`/`FromJson` instances.
-/

-- ============================================================
-- Artifact types
-- ============================================================

structure ParsedProgram where
  modules : List Module

structure SummaryTable where
  entries : List (String × FileSummary)

structure ResolvedProgram where
  modules : List ResolvedModule

structure ElaboratedProgram where
  coreModules : List CModule

/-- Core IR that has passed `coreCheckProgram`.
    This is the proof-oriented artifact boundary: downstream passes (Mono, Lower, Emit)
    may assume all Core-level invariants hold.  Only `Pipeline.coreCheck` constructs this. -/
structure ValidatedCore where
  coreModules : List CModule

structure MonomorphizedProgram where
  coreModules : List CModule

structure SSAProgram where
  ssaModules : List SModule

-- ============================================================
-- Pipeline runner functions
-- ============================================================

namespace Pipeline

/-- Parse source code into a `ParsedProgram`. Expands capability aliases. -/
def parse (source : String) : Except Diagnostics ParsedProgram :=
  match Concrete.parse source with
  | .ok modules => .ok { modules := modules.map Module.expandCapAliases }
  | .error ds => .error ds

/-- Resolve `mod X;` declarations by reading sub-module files from disk.
    Wraps `resolveAllModules` (IO because it reads files).
    Returns the resolved program and a source map for diagnostics. -/
def resolveFiles (baseDir : String) (prog : ParsedProgram) (inputPath : String)
    (resolveAllModules : String → List Module → String → IO (Except String (List Module × SourceMap)))
    : IO (Except Diagnostics (ParsedProgram × SourceMap)) := do
  match ← resolveAllModules baseDir prog.modules inputPath with
  | .error e =>
    return .error [{ severity := .error, message := e, pass := "resolve", span := none, hint := none }]
  | .ok (modules, srcMap) =>
    return .ok ({ modules }, srcMap)

/-- Build the cross-file summary table from parsed modules. -/
def buildSummary (prog : ParsedProgram) : SummaryTable :=
  { entries := buildSummaryTable prog.modules }

/-- Name-resolution pass: validates all identifiers and imports. -/
def resolve (prog : ParsedProgram) (summary : SummaryTable) : Except Diagnostics ResolvedProgram :=
  match resolveProgram prog.modules summary.entries with
  | .ok resolved => .ok { modules := resolved }
  | .error ds => .error ds

/-- Desugar destructuring let statements before Check and Elab see them. -/
def desugar (resolved : ResolvedProgram) : ResolvedProgram :=
  { modules := resolved.modules.map fun rm =>
      { rm with module := desugarModule rm.module } }

/-- Type-checking pass. Consumes resolved program (proving name resolution happened). -/
def check (resolved : ResolvedProgram) (summary : SummaryTable) : Except Diagnostics Unit :=
  checkProgram resolved.modules summary.entries

/-- Elaborate and canonicalize (no validation yet). Consumes resolved program. -/
def elaborate (resolved : ResolvedProgram) (summary : SummaryTable) : Except Diagnostics ElaboratedProgram :=
  match elabProgram resolved.modules summary.entries with
  | .error ds => .error ds
  | .ok coreModules => .ok { coreModules := canonicalizeProgram coreModules }

/-- Validate elaborated Core IR.  This is the only way to construct a `ValidatedCore`.
    Note: the post-Elab placeholder check (`verifyPostElab`) is **not** run here — it is
    only available via `--report verify`. Placeholder legitimately survives elaboration
    in try/defer expressions and is resolved during lowering. -/
def coreCheck (elabProg : ElaboratedProgram) : Except Diagnostics ValidatedCore :=
  match coreCheckProgram elabProg.coreModules with
  | .error ds => .error ds
  | .ok () => .ok { coreModules := elabProg.coreModules }

/-- Run the post-Elab verifier (placeholder check) as a non-blocking diagnostic pass.
    Returns warnings for any Ty.placeholder found; does not block compilation. -/
def verifyPostElab (modules : List CModule) : Diagnostics :=
  (verifyNoPlaceholders modules).map fun d => { d with severity := .warning }

/-- Monomorphize generic functions.  Runs the post-Mono verifier (no Ty.typeVar). -/
def monomorphize (vc : ValidatedCore) : Except Diagnostics MonomorphizedProgram :=
  match monoProgram vc.coreModules with
  | .ok modules =>
    let monoDs := verifyPostMono modules
    if !monoDs.isEmpty then .error monoDs
    else .ok { coreModules := modules }
  | .error ds => .error ds

/-- Lower to SSA, verify, and clean up. -/
def lower (mono : MonomorphizedProgram) : Except Diagnostics SSAProgram := do
  let ssaModules ← mono.coreModules.mapM (fun m => lowerModule m)
  match ssaVerifyProgram ssaModules with
  | .error ds => .error ds
  | .ok () =>
    let ssaModules := ssaCleanupProgram ssaModules
    -- Verify cleanup preserved SSA invariants
    match ssaVerifyProgram ssaModules with
    | .error ds => .error ds
    | .ok () => .ok { ssaModules }

/-- Same as `lower`, but skip both verifier passes AND the cleanup
    pass. Diagnostic-only — used by debugging entry points to print
    raw SSA that the verifier rejects. Not on the production compile
    path. -/
def lowerUnverified (mono : MonomorphizedProgram) : Except Diagnostics SSAProgram := do
  let ssaModules ← mono.coreModules.mapM (fun m => lowerModule m)
  .ok { ssaModules }

/-- Per-gate verify report.  Each field collects diagnostics produced
    at a specific named pipeline boundary; an empty list means the gate
    passed cleanly. Currently:

      `postElab`        Ty.placeholder leak after elab (warnings —
                        try/defer have documented exceptions per
                        `docs/VERIFY_GATES.md`).
      `postMono`        Ty.typeVar leak + Copy-field violations after
                        monomorphization (errors).
      `postLower`       SSA structural / dominator invariants on the
                        raw `lowerModule` output (errors).
      `postCleanup`     Same invariants re-checked after
                        `ssaCleanupProgram` (errors — non-empty means
                        cleanup broke something Lower got right).

    Earlier-pass failures (parse / resolve / check / elab / coreCheck /
    mono) are not represented here; they are surfaced through the
    Except return of the corresponding Pipeline.* function, since they
    block construction of the input artifact this gate runs on. -/
structure VerifyReport where
  postElab    : Diagnostics
  postMono    : Diagnostics
  postLower   : Diagnostics
  postCleanup : Diagnostics
  deriving Inhabited

namespace VerifyReport

def empty : VerifyReport :=
  { postElab := [], postMono := [], postLower := [], postCleanup := [] }

def isClean (r : VerifyReport) : Bool :=
  r.postElab.isEmpty && r.postMono.isEmpty
    && r.postLower.isEmpty && r.postCleanup.isEmpty

def errorCount (r : VerifyReport) : Nat :=
  let count (ds : Diagnostics) : Nat := (ds.filter (·.severity == .error)).length
  count r.postElab + count r.postMono + count r.postLower + count r.postCleanup

def warningCount (r : VerifyReport) : Nat :=
  let count (ds : Diagnostics) : Nat := (ds.filter (·.severity == .warning)).length
  count r.postElab + count r.postMono + count r.postLower + count r.postCleanup

def allDiagnostics (r : VerifyReport) : Diagnostics :=
  r.postElab ++ r.postMono ++ r.postLower ++ r.postCleanup

end VerifyReport

/-- Run every per-gate verifier on a validated Core program, returning
    the per-boundary diagnostics. Earlier-pass failures must already
    have been handled by the caller; this function consumes
    ValidatedCore and produces the report or short-circuits if a
    downstream pass (mono / lower) fails outright. -/
def runVerifyGates (vc : ValidatedCore) : VerifyReport :=
  let postElab := verifyPostElab vc.coreModules
  match monoProgram vc.coreModules with
  | .error ds =>
    -- Mono refused to even run; surface its diagnostics as post-mono
    -- errors. We keep the per-gate label on what the diagnostics
    -- actually represent (a failure of the post-mono boundary's
    -- input artifact to be well-formed), rather than dropping them.
    { VerifyReport.empty with postElab := postElab, postMono := ds }
  | .ok monoMods =>
    let postMono := verifyPostMono monoMods
    if !postMono.isEmpty then
      { VerifyReport.empty with postElab := postElab, postMono := postMono }
    else
      let ssa := monoMods.map lowerModule
      -- Each lowerModule produces Except — short-circuit on first error.
      let lowerErrors : Diagnostics := ssa.foldl (fun acc r =>
        match r with | .error ds => acc ++ ds | .ok _ => acc) []
      if !lowerErrors.isEmpty then
        { VerifyReport.empty with postElab := postElab, postMono := postMono,
                                  postLower := lowerErrors }
      else
        let ssaMods : List SModule := ssa.foldl (fun acc r =>
          match r with | .ok m => acc ++ [m] | .error _ => acc) []
        let postLower := match ssaVerifyProgram ssaMods with
          | .error ds => ds
          | .ok () => []
        -- post-cleanup checks that ssaCleanupProgram preserved SSA
        -- invariants. That claim is only meaningful when post-lower
        -- was clean; running cleanup on already-invalid SSA and
        -- reporting it as "post-cleanup ok" would imply cleanup was
        -- meaningfully checked on bad input. Skip it instead.
        let postCleanup :=
          if !postLower.isEmpty then ([] : Diagnostics)
          else
            let cleaned := ssaCleanupProgram ssaMods
            match ssaVerifyProgram cleaned with
            | .error ds => ds
            | .ok () => []
        { postElab := postElab, postMono := postMono,
          postLower := postLower, postCleanup := postCleanup }

/-- Emit LLVM IR from SSA modules. -/
def emit (ssa : SSAProgram) (testMode : Bool := false) (moduleFilter : Option String := none) : String :=
  emitSSAProgram ssa.ssaModules testMode moduleFilter

-- ============================================================
-- Shared frontend helper
-- ============================================================

/-- Run the shared frontend: parse → resolveFiles → buildSummary → resolve → check → elaborate → coreCheck.
    This is the common prefix of all CLI entry points (except interface report).
    Returns validated Core alongside earlier artifacts for diagnostic rendering. -/
def runFrontend (inputPath source : String)
    (resolveAllModules : String → List Module → String → IO (Except String (List Module × SourceMap)))
    : IO (Except Diagnostics (ParsedProgram × SummaryTable × ValidatedCore × SourceMap)) := do
  let mainSrcMap : SourceMap := [(inputPath, source)]
  match Pipeline.parse source with
  | .error ds => return .error ds
  | .ok parsed =>
  let baseDir := let parts := inputPath.splitOn "/"
    match parts.reverse with
    | _ :: rest => "/".intercalate rest.reverse
    | [] => "."
  match ← Pipeline.resolveFiles baseDir parsed inputPath resolveAllModules with
  | .error ds => return .error ds
  | .ok (resolved, subSrcMap) =>
    let srcMap := mainSrcMap ++ subSrcMap
    let summary := Pipeline.buildSummary resolved
    match Pipeline.resolve resolved summary with
    | .error ds => return .error ds
    | .ok resolvedProg =>
    let resolvedProg := Pipeline.desugar resolvedProg
    match Pipeline.check resolvedProg summary with
    | .error ds => return .error ds
    | .ok () =>
    match Pipeline.elaborate resolvedProg summary with
    | .error ds => return .error ds
    | .ok elabProg =>
    match Pipeline.coreCheck elabProg with
    | .error ds => return .error ds
    | .ok validCore => return .ok (resolved, summary, validCore, srcMap)

end Pipeline
end Concrete
