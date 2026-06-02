import Concrete.Pipeline
import Concrete.Report
import Concrete.ProofCore
import Concrete.Verify

namespace Concrete.DebugBundle

/-! ## DebugBundle — stable bundle format for reproducing compiler failures

A debug bundle captures everything needed to reproduce a compilation failure:
source files, compiler state at each pipeline stage, diagnostics, and metadata.

Bundle layout:
```
bundle/
  manifest.json        — compiler version, source path, flags, failure stage
  source/              — original source files (main + submodules)
  diagnostics.txt      — rendered diagnostics at failure point
  core.txt             — Core IR dump (if elaboration succeeded)
  ssa.txt              — SSA IR dump (if lowering succeeded)
  llvm.ll              — LLVM IR (if emission succeeded)
  consistency.txt      — ProofCore self-check results (if available)
  verify.txt           — Verifier pass results (if available)
```
-/

/-- Which pipeline stage the bundle was captured at. -/
inductive CaptureStage where
  | parse
  | resolve
  | check
  | elaborate
  | coreCheck
  | mono
  | lower
  | emit
  | complete
  deriving BEq

def CaptureStage.toString : CaptureStage → String
  | .parse     => "parse"
  | .resolve   => "resolve"
  | .check     => "check"
  | .elaborate => "elaborate"
  | .coreCheck => "coreCheck"
  | .mono      => "mono"
  | .lower     => "lower"
  | .emit      => "emit"
  | .complete  => "complete"

instance : ToString CaptureStage := ⟨CaptureStage.toString⟩

/-- Accumulated pipeline state for the debug bundle. -/
structure BundleState where
  inputPath    : String
  source       : String
  sourceMap    : SourceMap := []
  failStage    : Option CaptureStage := none
  diagnostics  : Diagnostics := []
  coreModules  : Option (List CModule) := none
  monoModules  : Option (List CModule) := none
  ssaModules   : Option (List SModule) := none
  llvmIR       : Option String := none
  proofCore    : Option ProofCore := none
  verifyDs     : Diagnostics := []

/-- Escape a string for JSON output. -/
private def jsonEscape (s : String) : String :=
  s.foldl (fun acc c =>
    acc ++ match c with
    | '"'  => "\\\""
    | '\\' => "\\\\"
    | '\n' => "\\n"
    | '\t' => "\\t"
    | c    => c.toString
  ) ""

/-- Generate the manifest.json content. -/
def renderManifest (st : BundleState) (compilerVersion : String) : String :=
  let stage := match st.failStage with
    | some s => s!"\"failed_at\": \"{s}\""
    | none   => "\"failed_at\": null"
  let diagCount := st.diagnostics.length
  let hasCore := st.coreModules.isSome
  let hasMono := st.monoModules.isSome
  let hasSSA  := st.ssaModules.isSome
  let hasLLVM := st.llvmIR.isSome
  let hasPC   := st.proofCore.isSome
  s!"\{
  \"version\": 1,
  \"compiler\": \"{jsonEscape compilerVersion}\",
  \"source_path\": \"{jsonEscape st.inputPath}\",
  {stage},
  \"diagnostic_count\": {diagCount},
  \"artifacts\": \{
    \"core_ir\": {hasCore},
    \"mono_ir\": {hasMono},
    \"ssa_ir\": {hasSSA},
    \"llvm_ir\": {hasLLVM},
    \"proof_core\": {hasPC}
  }
}"

/-- Validate a debug bundle directory for structural integrity.
    Uses string matching on manifest.json since we don't depend on a full JSON parser here.
    Returns a list of issues found. Empty list means valid. -/
def validateBundle (bundleDir : String) : IO (List String) := do
  let mut issues : List String := []
  let manifestPath := bundleDir ++ "/manifest.json"
  let manifestContent ← try
    pure (some (← IO.FS.readFile ⟨manifestPath⟩))
  catch _ => pure none
  match manifestContent with
  | none => return ["error: manifest.json missing from bundle"]
  | some content =>
  -- Basic structural validation via string inspection
  let has (haystack needle : String) : Bool := (haystack.splitOn needle).length > 1
  -- Extract the value after a "key": ... by splitting on the key and taking what follows the colon
  let valAfter (haystack key : String) : Option String :=
    match (haystack.splitOn key) with
    | _ :: rest :: _ =>
      let after := rest.trimAscii.toString
      -- skip the colon
      if after.startsWith ":" then some (after.drop 1 |>.trimAscii.toString)
      else none
    | _ => none
  let trimmed := content.trimAscii.toString
  if !trimmed.startsWith "{" || !trimmed.endsWith "}" then
    issues := issues ++ ["error: manifest.json is not a valid JSON object"]
  else
    -- Check required fields exist
    for field in ["\"version\"", "\"source_path\"", "\"failed_at\"", "\"artifacts\""] do
      if !has content field then
        issues := issues ++ [s!"warning: manifest.json missing {field} field"]
    -- Validate field types
    match valAfter content "\"version\"" with
    | some v =>
      -- version must start with a digit (numeric)
      if v.isEmpty || !(v.front |>.isDigit) then
        issues := issues ++ ["error: manifest.json \"version\" must be a number"]
    | none => pure ()
    match valAfter content "\"source_path\"" with
    | some v =>
      if !v.startsWith "\"" then
        issues := issues ++ ["error: manifest.json \"source_path\" must be a string"]
    | none => pure ()
    -- failed_at may be a string or null (null means compilation succeeded)
    match valAfter content "\"failed_at\"" with
    | some v =>
      if !(v.startsWith "\"" || v.startsWith "null") then
        issues := issues ++ ["error: manifest.json \"failed_at\" must be a string or null"]
    | none => pure ()
    -- Check artifacts sub-fields and their types
    if has content "\"artifacts\"" then
      for sub in ["\"core_ir\"", "\"mono_ir\"", "\"ssa_ir\"", "\"llvm_ir\"", "\"proof_core\""] do
        if !has content sub then
          issues := issues ++ [s!"warning: manifest.json artifacts missing {sub} field"]
        else
          match valAfter content sub with
          | some v =>
            if !(v.startsWith "true" || v.startsWith "false") then
              issues := issues ++ [s!"error: manifest.json artifacts {sub} must be a boolean"]
          | none => pure ()
  -- Check source file exists
  let srcExists ← try
    let _ ← IO.FS.readFile ⟨bundleDir ++ "/source/main.con"⟩
    pure true
  catch _ =>
    -- Try with the source_path basename
    try
      let files ← System.FilePath.readDir ⟨bundleDir ++ "/source"⟩
      pure (files.size > 0)
    catch _ => pure false
  if !srcExists then
    issues := issues ++ ["warning: source/ directory missing or empty"]
  return issues

/-- Write the debug bundle to a directory. -/
def writeBundle (bundleDir : String) (st : BundleState) (compilerVersion : String) : IO Unit := do
  -- Create directories
  IO.FS.createDirAll ⟨bundleDir⟩
  IO.FS.createDirAll ⟨bundleDir ++ "/source"⟩

  -- manifest.json
  IO.FS.writeFile ⟨bundleDir ++ "/manifest.json"⟩ (renderManifest st compilerVersion)

  -- Source files
  IO.FS.writeFile ⟨bundleDir ++ "/source/" ++ baseName st.inputPath⟩ st.source
  for (path, content) in st.sourceMap do
    let name := baseName path
    if !name.isEmpty then
      IO.FS.writeFile ⟨bundleDir ++ "/source/" ++ name⟩ content

  -- Diagnostics
  if !st.diagnostics.isEmpty then
    let rendered := renderDiagnostics st.diagnostics (sourceMap := st.sourceMap)
    IO.FS.writeFile ⟨bundleDir ++ "/diagnostics.txt"⟩ rendered

  -- Core IR
  if let some modules := st.coreModules then
    let coreStr := modules.foldl (fun acc m => acc ++ ppCModule m ++ "\n") ""
    IO.FS.writeFile ⟨bundleDir ++ "/core.txt"⟩ coreStr

  -- SSA IR
  if let some modules := st.ssaModules then
    let ssaStr := modules.foldl (fun acc m => acc ++ ppSModule m ++ "\n") ""
    IO.FS.writeFile ⟨bundleDir ++ "/ssa.txt"⟩ ssaStr

  -- LLVM IR
  if let some ir := st.llvmIR then
    IO.FS.writeFile ⟨bundleDir ++ "/llvm.ll"⟩ ir

  -- ProofCore consistency
  if let some pc := st.proofCore then
    let violations := pc.selfCheck
    IO.FS.writeFile ⟨bundleDir ++ "/consistency.txt"⟩ (ConsistencyViolation.render violations)

  -- Verifier results
  if !st.verifyDs.isEmpty then
    IO.FS.writeFile ⟨bundleDir ++ "/verify.txt"⟩ (renderVerifyDiagnostics st.verifyDs)
where
  baseName (path : String) : String :=
    match path.splitOn "/" |>.reverse with
    | name :: _ => name
    | [] => path

/-- Load proof registry, returning empty list on failure. -/
private def loadProofRegistry (inputPath : String) : IO ProofRegistry := do
  let dir := match inputPath.splitOn "/" |>.reverse with
    | _ :: rest => "/".intercalate rest.reverse
    | [] => "."
  let regPath := dir ++ "/proof-registry.json"
  try
    let content ← IO.FS.readFile ⟨regPath⟩
    let (registry, _warnings) := parseRegistryJson content
    return registry
  catch _ => return []

/-- Run the full pipeline, capturing state at each stage.
    Returns the bundle state (which may represent a failure at any point). -/
partial def capturePipeline (inputPath source : String)
    (resolveAllModules : String → List Module → String → IO (Except String (List Module × SourceMap)))
    : IO BundleState := do
  let srcMap0 : SourceMap := [(inputPath, source)]
  let mk (stage : CaptureStage) (ds : Diagnostics) (extras : BundleState → BundleState := id) : BundleState :=
    extras { inputPath, source, sourceMap := srcMap0, failStage := some stage, diagnostics := ds }

  -- Parse
  match Pipeline.parse source with
  | .error ds => return mk .parse ds
  | .ok parsed =>

  -- Resolve files
  let baseDir := match inputPath.splitOn "/" |>.reverse with
    | _ :: rest => "/".intercalate rest.reverse
    | [] => "."
  match ← Pipeline.resolveFiles baseDir parsed inputPath resolveAllModules with
  | .error ds => return mk .resolve ds
  | .ok (resolved, subSrcMap) =>

  let srcMap := srcMap0 ++ subSrcMap
  let summary := Pipeline.buildSummary resolved

  -- Resolve names
  match Pipeline.resolve resolved summary with
  | .error ds => return mk .resolve ds (fun s => { s with sourceMap := srcMap })
  | .ok resolvedProg =>

  -- Check
  match Pipeline.check resolvedProg summary with
  | .error ds => return mk .check ds (fun s => { s with sourceMap := srcMap })
  | .ok () =>

  -- Elaborate
  match Pipeline.elaborate resolvedProg summary with
  | .error ds => return mk .elaborate ds (fun s => { s with sourceMap := srcMap })
  | .ok elabProg =>

  -- CoreCheck
  match Pipeline.coreCheck elabProg with
  | .error ds => return mk .coreCheck ds (fun s => { s with sourceMap := srcMap })
  | .ok validCore =>

  -- ProofCore (non-blocking)
  let locMap := Report.buildFnLocMap resolved.modules inputPath
  let simpleLocMap := locMap.map fun e => (e.qualName, (e.file, e.fnSpan.line))
  let registry ← loadProofRegistry inputPath
  let pc := extractProofCore validCore simpleLocMap registry

  -- Verifier (non-blocking)
  let elabDs := Pipeline.verifyPostElab validCore.coreModules

  let base : BundleState :=
    { inputPath := inputPath
      source := source
      sourceMap := srcMap
      coreModules := some validCore.coreModules
      proofCore := some pc
      verifyDs := elabDs }

  -- Monomorphize
  match Pipeline.monomorphize validCore with
  | .error ds => return { base with failStage := some .mono, diagnostics := ds }
  | .ok mono =>

  -- Lower
  match Pipeline.lower mono with
  | .error ds =>
      return { base with
        failStage := some .lower
        monoModules := some mono.coreModules
        diagnostics := ds }
  | .ok ssa =>

  -- Emit
  let llvmIR := Pipeline.emit ssa

  -- Complete (no failure)
  return { base with
    monoModules := some mono.coreModules
    ssaModules := some ssa.ssaModules
    llvmIR := some llvmIR }

end Concrete.DebugBundle
