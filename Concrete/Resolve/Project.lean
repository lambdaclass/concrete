import Concrete
import Concrete.Report.CompilerLedger
import Concrete.Proof.ObligationCore

/-! ## Project — the project-loading boundary (ROADMAP Phase 4 #16b)

`ProjectContext` and `loadProject`, plus the dependency / TOML / module-resolution
helpers they need, live here as a **boundary** library module so external tooling
(editor/LSP, MCP, package managers) can discover and load a Concrete project
without shelling out to the CLI or importing compiler internals.

Built up in small verified cuts (#16b): path/IO leaves first, then module
resolution, then TOML/deps/registry, then `ProjectContext` / `loadProject`. -/

namespace Concrete

-- Path / IO leaf helpers (#16b stage 1).

def readFile (path : String) : IO String := do
  IO.FS.readFile ⟨path⟩

/-- Check if a module is an empty stub from `mod X;` declaration. -/
def isModuleStub (m : Module) : Bool :=
  m.functions.isEmpty && m.structs.isEmpty && m.enums.isEmpty &&
  m.imports.isEmpty && m.implBlocks.isEmpty && m.traits.isEmpty &&
  m.traitImpls.isEmpty && m.constants.isEmpty && m.typeAliases.isEmpty &&
  m.externFns.isEmpty && m.newtypes.isEmpty && m.submodules.isEmpty

/-- Get directory of a file path. -/
def dirOf (path : String) : String :=
  let parts := path.splitOn "/"
  match parts.reverse with
  | _ :: rest => "/".intercalate rest.reverse
  | [] => "."

-- Module resolution: read `mod X;` files from disk, recursively (#16b stage 2).

/-- Resolve `mod X;` declarations by reading X.con files from the same directory.
    Detects circular imports via parsedPaths. Collects source map entries. -/
partial def resolveModules (baseDir : String) (m : Module) (parsedPaths : List String)
    (srcMap : SourceMap := [])
    : IO (Except String (Module × List String × SourceMap)) := do
  let mut resolvedSubs : List Module := []
  let mut paths := parsedPaths
  let mut sources := srcMap
  for sub in m.submodules do
    if isModuleStub sub then
      let filePath := if baseDir == "" then sub.name ++ ".con"
                      else baseDir ++ "/" ++ sub.name ++ ".con"
      -- Try name.con first, then name/mod.con for directory modules
      let dirPath := if baseDir == "" then sub.name ++ "/mod.con"
                     else baseDir ++ "/" ++ sub.name ++ "/mod.con"
      let (actualPath, source) ← do
        match ← (try pure (some (← readFile filePath)) catch _ => pure none) with
        | some s => pure (filePath, s)
        | none =>
          match ← (try pure (some (← readFile dirPath)) catch _ => pure none) with
          | some s => pure (dirPath, s)
          | none => return .error s!"module file not found: {filePath} or {dirPath}"
      let filePath := actualPath
      -- For directory modules, resolve sub-stubs from the subdirectory
      let subBaseDir := if actualPath == dirPath then
        if baseDir == "" then sub.name else baseDir ++ "/" ++ sub.name
      else baseDir
      if paths.contains filePath then
        return .error s!"circular module import: {filePath}"
      sources := sources ++ [(filePath, source)]
      match parse source with
      | .error e => return .error s!"error in module '{sub.name}': {renderDiagnostics e}"
      | .ok subModules =>
        match subModules with
        | [subMod] =>
          paths := paths ++ [filePath]
          match ← resolveModules subBaseDir { subMod with name := sub.name, sourceFile := filePath } paths sources with
          | .ok (resolved, newPaths, newSources) =>
            paths := newPaths
            sources := newSources
            resolvedSubs := resolvedSubs ++ [resolved]
          | .error e => return .error e
        | _ => return .error s!"module file '{filePath}' must contain exactly one module"
    else
      -- Inline module (mod X { ... }), recurse to resolve nested stubs
      match ← resolveModules baseDir sub paths sources with
      | .ok (resolved, newPaths, newSources) =>
        paths := newPaths
        sources := newSources
        resolvedSubs := resolvedSubs ++ [resolved]
      | .error e => return .error e
  return .ok ({ m with submodules := resolvedSubs }, paths, sources)

/-- Resolve all modules in a program. Returns resolved modules and source map. -/
def resolveAllModules (baseDir : String) (modules : List Module) (inputPath : String)
    : IO (Except String (List Module × SourceMap)) := do
  let mut resolved : List Module := []
  let mut paths : List String := [inputPath]
  let mut sources : SourceMap := []
  for m in modules do
    match ← resolveModules baseDir m paths sources with
    | .ok (rm, newPaths, newSources) =>
      paths := newPaths
      sources := newSources
      resolved := resolved ++ [rm]
    | .error e => return .error e
  return .ok (resolved, sources)

-- TOML / dependency / registry / discovery helpers (#16b stage 3).

/-- The proof registry a report/query sees: the in-source proof links
    synthesized from `#[proof_by]`/`#[spec]`/`#[proof_fingerprint]`. Legacy JSON
    `proof-registry.json` support was removed — source links are the only model,
    so every registry-consuming path (report, query, policy, snapshot,
    traceability) goes through this one synthesizer. -/
def loadRegistryWithLinks (_inputPath : String)
    (astModules : List Concrete.Module) (coreModules : List Concrete.CModule) :
    IO Concrete.ProofRegistry := do
  return Report.synthesizeSourceLinks astModules coreModules

/-- Parse `[dependencies]` from Concrete.toml.
    Only handles the format: `name = { path = "...", ... }` under `[dependencies]`.
    Returns (entries, warnings) — warnings are non-empty for unparseable lines. -/
def parseDependencies (content : String) : List (String × String) × List String :=
  let lines := content.splitOn "\n"
  let rec go (ls : List String) (inDeps : Bool) (acc : List (String × String))
      (warns : List String) :=
    match ls with
    | [] => (acc, warns)
    | l :: rest =>
      let trimmed := l.trimAscii.toString
      if trimmed.startsWith "[dependencies]" then
        go rest true acc warns
      else if trimmed.startsWith "[" then
        go rest false acc warns
      else if inDeps && trimmed.length > 0 && !trimmed.startsWith "#" then
        match trimmed.splitOn "=" with
        | name :: valParts =>
          let depName := name.trimAscii.toString
          let valStr := "=".intercalate valParts
          match valStr.splitOn "path" with
          | _ :: pathRest :: _ =>
            let afterPath := ("path".intercalate [pathRest])
            match afterPath.splitOn "\"" with
            | _ :: pathVal :: _ =>
              go rest true (acc ++ [(depName, pathVal)]) warns
            | _ => go rest true acc
              (warns ++ [s!"warning: Concrete.toml [dependencies]: could not parse path in '{trimmed}'"])
          | _ => go rest true acc
            (warns ++ [s!"warning: Concrete.toml [dependencies]: missing 'path' in '{trimmed}'"])
        | _ => go rest true acc
          (warns ++ [s!"warning: Concrete.toml [dependencies]: could not parse line '{trimmed}'"])
      else
        go rest inDeps acc warns
  go lines false [] []

/-- Validate Concrete.toml content and return warnings for structural issues. -/
def validateToml (content : String) : List String :=
  let lines := content.splitOn "\n"
  let trimmedLines := lines.map (·.trimAscii.toString)
  let hasPackage := trimmedLines.any (·.startsWith "[package]")
  -- Check [package] section
  let pkgWarns := if !hasPackage then
    ["warning: Concrete.toml missing [package] section"]
  else
    let inPkg := lines.foldl (fun (acc : Bool × Bool) l =>
      let t := l.trimAscii.toString
      if t.startsWith "[package]" then (true, acc.2)
      else if t.startsWith "[" then (false, acc.2)
      else if acc.1 && t.startsWith "name" then (acc.1, true)
      else acc
    ) (false, false)
    if !inPkg.2 then ["warning: Concrete.toml [package] missing 'name' field"]
    else []
  -- Warn about unknown top-level sections
  let knownSections := ["[package]", "[dependencies]", "[policy]", "[profile]"]
  let sectionWarns := trimmedLines.filterMap fun l =>
    if l.startsWith "[" && !l.startsWith "#" then
      if knownSections.any (l.startsWith ·) then none
      else some s!"warning: Concrete.toml has unrecognized section '{l}'"
    else none
  pkgWarns ++ sectionWarns

/-- Find Concrete.toml by walking up from a directory. -/
partial def findProjectRoot (startDir : String) : IO (Option String) := do
  let tomlPath := startDir ++ "/Concrete.toml"
  let tomlExists ← try
    let _ ← IO.FS.readFile ⟨tomlPath⟩
    pure true
  catch _ => pure false
  if tomlExists then
    return some startDir
  -- Walk up the ancestor chain until we hit the filesystem root.
  let parent := dirOf startDir
  if parent == startDir then return none
  findProjectRoot parent

/-- Resolve a dependency path relative to the project root. -/
def resolveDependencyPath (projectRoot : String) (depPath : String) : String :=
  if depPath.startsWith "/" then depPath
  else projectRoot ++ "/" ++ depPath

/-- Check if a path contains a valid std library (has src/lib.con). -/
def hasStdLib (path : String) : IO Bool := do
  let libPath := path ++ "/src/lib.con"
  try let _ ← IO.FS.readFile ⟨libPath⟩; pure true catch _ => pure false

/-- Find the builtin std library relative to the compiler binary.
    Searches common relative paths from the executable location.
    Falls back to CONCRETE_STD environment variable. -/
def findBuiltinStd : IO (Option String) := do
  -- Try CONCRETE_STD environment variable first
  let envStd ← IO.getEnv "CONCRETE_STD"
  match envStd with
  | some stdPath =>
    let ok ← hasStdLib stdPath
    if ok then return some stdPath
  | none => pure ()
  -- Find relative to compiler binary
  let exePath ← IO.appPath
  let exeDir := dirOf exePath.toString
  let candidates := [
    exeDir ++ "/../../../std",   -- .lake/build/bin/
    exeDir ++ "/../../std",      -- build/bin/
    exeDir ++ "/../std",         -- bin/
    exeDir ++ "/std"             -- same dir
  ]
  for candidate in candidates do
    let ok ← hasStdLib candidate
    if ok then return some candidate
  return none

/-- Load and parse a dependency's lib.con, resolving its submodules. -/
partial def loadDependency (depName : String) (depPath : String)
    : IO (Except String (List Module × SourceMap)) := do
  let libPath := depPath ++ "/src/lib.con"
  let source ← try
    readFile libPath
  catch _ =>
    return .error s!"error: dependency '{depName}': cannot read {libPath}\nhint: check the path in [dependencies] or ensure the dependency has src/lib.con"
  match parse source with
  | .error e => return .error s!"dependency '{depName}': parse error: {renderDiagnostics e}"
  | .ok modules =>
    let baseDir := depPath ++ "/src"
    match ← resolveAllModules baseDir modules libPath with
    | .error e => return .error s!"dependency '{depName}': {e}"
    | .ok (resolved, srcMap) =>
      let srcMap := [(libPath, source)] ++ srcMap
      -- The dependency's root modules were parsed from ITS lib.con, not the
      -- project's main file — stamp them so their diagnostics say so (#24a).
      let resolved := resolved.map fun m =>
        if m.sourceFile.isEmpty then { m with sourceFile := libPath } else m
      return .ok (resolved, srcMap)


-- The one typed project surface + its loader (#16b stage 4).

/-- The one typed project surface (ROADMAP Phase 4 #1). Every project-mode command
    loads this ONCE via `loadProject` instead of re-discovering source, re-parsing
    `Concrete.toml`, or re-deriving the policy. New project facts (target/build
    profile, oracle manifests, …) are added here, not recomputed per command. -/
structure ProjectContext where
  projectRoot   : String                 -- the project root (dir holding Concrete.toml)
  validCore     : ValidatedCore
  parsed        : ParsedProgram           -- merged modules (deps + project)
  allSrcMap     : SourceMap
  tomlContent   : String
  mainPath      : String                  -- entry point (src/main.con)
  depNames      : List String
  policy        : Concrete.ProjectPolicy  -- the [policy] release profile, parsed once
  policyWarnings : List String            -- structural warnings from parsing [policy]
  -- proof/diagnostic facts derived once (Phase 4 #1): the source-location map, the
  -- synthesized proof registry, and the ProofCore — so build / test / report /
  -- policy stop re-deriving them per command.
  policyLocMap  : Report.FnLocMap
  registry      : Concrete.ProofRegistry
  pc            : Concrete.ProofCore
  -- the non-proof compiler fact store (Phase 4 #2), built once from this load and
  -- linked to the ObligationCore (proof) ledger.
  ledger        : Concrete.CompilerLedger.CompilerLedger

/-- Load a project to ValidatedCore. Shared by build, test, and check. -/
partial def loadProject (projectRoot : String) (stripTestFns : Bool := false) : IO (Except UInt32 ProjectContext) := do
  let tLoadStart ← IO.monoMsNow
  let tomlPath := projectRoot ++ "/Concrete.toml"
  let tomlContent ← readFile tomlPath
  let tomlWarnings := validateToml tomlContent
  for w in tomlWarnings do IO.eprintln w
  let (userDeps, depWarnings) := parseDependencies tomlContent
  for w in depWarnings do IO.eprintln w

  -- Inject builtin std if user didn't declare it explicitly
  let hasStdDep := userDeps.any fun (name, _) => name == "std"
  let deps ← if hasStdDep then
    pure userDeps
  else
    match ← findBuiltinStd with
    | some stdPath => pure (("std", stdPath) :: userDeps)
    | none =>
      IO.eprintln "warning: builtin std not found\nhint: set CONCRETE_STD=/path/to/std or add std = { path = \"...\" } to [dependencies]"
      pure userDeps

  -- Load all dependencies
  let mut depModules : List Module := []
  let mut depSrcMap : SourceMap := []
  for (depName, depPath) in deps do
    let resolvedPath := if depPath.startsWith "/" then depPath
      else resolveDependencyPath projectRoot depPath
    match ← loadDependency depName resolvedPath with
    | .error e =>
      IO.eprintln e
      return Except.error 1  -- early exit
    | .ok (modules, srcMap) =>
      depModules := depModules ++ modules
      depSrcMap := depSrcMap ++ srcMap
  let tDepsLoaded ← IO.monoMsNow

  -- Load project's main source
  let mainPath := projectRoot ++ "/src/main.con"
  let sourceResult ← try
    let s ← readFile mainPath
    pure (some s)
  catch _ => pure none
  match sourceResult with
  | none =>
    IO.eprintln s!"error: cannot read {mainPath}\nhint: projects need a src/main.con entry point"
    return Except.error 1
  | some source =>

  -- Parse the project source
  match Pipeline.parse source with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := [(mainPath, source)]))
    return Except.error 1
  | .ok parsed =>
  let baseDir := projectRoot ++ "/src"
  match ← Pipeline.resolveFiles baseDir parsed mainPath resolveAllModules with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := [(mainPath, source)]))
    return Except.error 1
  | .ok (resolvedParsed, subSrcMap) =>
    -- Optionally strip #[test] functions from dependency modules
    let depModulesUsed := if stripTestFns then
      let stripTests : Module → Module := fun m =>
        { m with
          functions := m.functions.filter fun f => !f.isTest
          submodules := m.submodules.map fun sub =>
            { sub with functions := sub.functions.filter fun f => !f.isTest }
        }
      depModules.map stripTests
    else depModules
    let allModules : List Module := depModulesUsed ++ resolvedParsed.modules
    let allSrcMap : SourceMap := [(mainPath, source)] ++ subSrcMap ++ depSrcMap
    let merged : ParsedProgram := { modules := allModules }
    let summary := Pipeline.buildSummary merged
    match Pipeline.resolve merged summary with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return Except.error 1
    | .ok resolvedProg =>
    let depNames := depModules.map (·.name)
    let projectResolved : List ResolvedModule :=
      resolvedProg.modules.filter fun rm => !depNames.contains rm.module.name
    let projectResolvedProg : ResolvedProgram := { modules := projectResolved }
    match Pipeline.check projectResolvedProg summary with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return Except.error 1
    | .ok () =>
    match Pipeline.elaborate resolvedProg summary with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return Except.error 1
    | .ok elabProg =>
    match Pipeline.coreCheck elabProg with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return Except.error 1
    | .ok validCore =>
    -- Parse the release [policy] once, here, so no command re-derives it.
    let (policy, policyWarnings) := parsePolicy tomlContent
    -- Derive the location map / proof registry / ProofCore once (Phase 4 #1).
    let policyLocMap := Report.buildFnLocMap merged.modules mainPath
    let simpleLocMap := policyLocMap.map fun e => (e.qualName, (e.file, e.fnSpan.line))
    let registry ← loadRegistryWithLinks mainPath merged.modules validCore.coreModules
    let pc := extractProofCore validCore simpleLocMap registry
    -- Build the non-proof compiler fact store once (Phase 4 #2) from the facts this
    -- load already holds. Cheap facts only here (modules / deps / source files /
    -- obligation link); the git-backed toolchain id is filled lazily at render time.
    let mut ledger := (({} : Concrete.CompilerLedger.CompilerLedger).linkObligations
      "ObligationCore ledger — `concrete --report obligation-ledger`")
    for (n, p) in deps do ledger := ledger.recordDependency n p
    for m in merged.modules do ledger := ledger.recordFact "module" m.name (if depNames.contains m.name then "dependency" else "project")
    for (file, _) in allSrcMap do ledger := ledger.recordSourceMap file []
    -- The frontend pass chain as named artifacts (Phase 4 #3): every pass that ran
    -- to produce this context, with input → output provenance, so the pipeline is a
    -- replayable fact chain. They all succeeded (we are on the ok path).
    let chain : List (String × String × String) :=
      [ ("ast", "parse", "source"), ("resolved", "resolve", "ast"),
        ("checked", "typecheck", "resolved"), ("elaborated", "elaborate", "checked"),
        ("core", "core-check", "elaborated") ]
    let projModules := merged.modules.filter (fun m => !depNames.contains m.name)
    let summaryFor : String → String := fun oid =>
      if oid == "core" then s!"{validCore.coreModules.length} core modules"
      else if oid == "resolved" then s!"{merged.modules.length} modules ({projModules.length} project, {depNames.length} dep)"
      else ""
    for (oid, passName, inp) in chain do
      let art : Concrete.CompilerLedger.Artifact :=
        { id := oid, pass := passName, inputIds := [inp], outputIds := [oid],
          summary := summaryFor oid, replay := s!"concrete check  (pass: {passName})" }
      ledger := ledger.recordArtifact art
    -- per-phase timings (runtime-variable facts; consumers normalize them out for
    -- determinism checks).
    let tFrontend ← IO.monoMsNow
    ledger := ledger.recordTiming "load-deps" (tDepsLoaded - tLoadStart)
    ledger := ledger.recordTiming "frontend" (tFrontend - tDepsLoaded)
    -- Record proof-registry validation diagnostics into the one store (Phase 4 #4):
    -- commands render these from the ledger instead of recomputing them.
    for issue in Concrete.validateRegistry pc registry do
      let d : Concrete.CompilerLedger.Diag :=
        { code := "registry", severity := if issue.isError then "error" else "warning",
          message := Concrete.renderRegistryIssue issue }
      ledger := ledger.recordDiagnostic d
    return Except.ok { projectRoot, validCore, parsed := merged, allSrcMap, tomlContent,
                       mainPath, depNames, policy, policyWarnings, policyLocMap, registry, pc, ledger }

end Concrete
