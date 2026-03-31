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

namespace Concrete

/-! ## Pipeline — cacheable compiler artifacts

Each pipeline stage produces a named artifact type.
Runner functions wrap the underlying pass with `liftStringError` / diagnostic handling.
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
  match liftStringError "parse" (Concrete.parse source) with
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

/-- Type-checking pass. Consumes resolved program (proving name resolution happened). -/
def check (resolved : ResolvedProgram) (summary : SummaryTable) : Except Diagnostics Unit :=
  checkProgram resolved.modules summary.entries

/-- Elaborate and canonicalize (no validation yet). Consumes resolved program. -/
def elaborate (resolved : ResolvedProgram) (summary : SummaryTable) : Except Diagnostics ElaboratedProgram :=
  match elabProgram resolved.modules summary.entries with
  | .error ds => .error ds
  | .ok coreModules => .ok { coreModules := canonicalizeProgram coreModules }

/-- Validate elaborated Core IR.  This is the only way to construct a `ValidatedCore`. -/
def coreCheck (elabProg : ElaboratedProgram) : Except Diagnostics ValidatedCore :=
  match coreCheckProgram elabProg.coreModules with
  | .error ds => .error ds
  | .ok () => .ok { coreModules := elabProg.coreModules }

/-- Monomorphize generic functions. -/
def monomorphize (vc : ValidatedCore) : Except Diagnostics MonomorphizedProgram :=
  match liftStringError "mono" (monoProgram vc.coreModules) with
  | .ok modules => .ok { coreModules := modules }
  | .error ds => .error ds

/-- Lower to SSA, verify, and clean up. -/
def lower (mono : MonomorphizedProgram) : Except Diagnostics SSAProgram := do
  let ssaModules ← mono.coreModules.mapM (fun m =>
    match lowerModule m with
    | .ok sm => .ok sm
    | .error e => .error [{ severity := .error, message := e, pass := "lower", span := none, hint := none }])
  match ssaVerifyProgram ssaModules with
  | .error ds => .error ds
  | .ok () =>
    let ssaModules := ssaCleanupProgram ssaModules
    -- Verify cleanup preserved SSA invariants
    match ssaVerifyProgram ssaModules with
    | .error ds => .error ds
    | .ok () => .ok { ssaModules }

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
