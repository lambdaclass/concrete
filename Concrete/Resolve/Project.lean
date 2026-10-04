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
      -- CANONICAL COMPARISON. `paths` is the already-visited list and it was compared as RAW TEXT,
      -- so the same file reached under two spellings — `./src/x.con` and `src/x.con` — was not
      -- recognised as visited. That let one module enter `sources` twice, and a package identity
      -- computed over that list moved with the input path's FORM rather than with the program.
      --
      -- The duplicate is prevented here, at the producer, rather than erased downstream by
      -- deduplicating content digests: a content list that silently collapses `[X, X]` into `[X]`
      -- also collapses a module that went MISSING into one that was merely repeated.
      let canon ← try pure (← IO.FS.realPath ⟨filePath⟩).toString catch _ => pure filePath
      let canonPaths ← paths.mapM fun q =>
        try pure (← IO.FS.realPath ⟨q⟩).toString catch _ => pure q
      if canonPaths.contains canon then
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
  -- CANONICALISED FIRST, and this is a correctness fix rather than tidiness.
  --
  -- `dirOf "src"` is `""`, so walking up from a one-level-deep RELATIVE path went straight to
  -- `"/Concrete.toml"` — the filesystem root — and never looked at the current directory. Running
  -- `concrete src/main.con` from inside a project therefore found no project at all and silently
  -- fell back to STANDALONE analysis: a synthetic package identity, no dependency resolution, and
  -- evidence that disagreed with the same file named from the repository root. `crypto_verify`
  -- reported correspondence 4/4 one way and 0/4 the other, and the only difference was where the
  -- caller stood — the location-dependence slice 4 exists to eliminate.
  --
  -- Failing to canonicalise leaves the previous behaviour rather than inventing a root: a directory
  -- that does not exist has no project, and guessing one would be worse than not finding it.
  let start ← try
      pure (← IO.FS.realPath ⟨if startDir.isEmpty then "." else startDir⟩).toString
    catch _ => pure startDir
  -- NO FUEL. A depth cap returning `none` is indistinguishable from "there is no project here", so a
  -- validly-nested project past the cap would silently fall back to STANDALONE analysis — exactly the
  -- location-dependent authority bug this walk was rewritten to fix, reintroduced at a different
  -- depth. `start` is canonicalised above, so the ancestor chain is finite and terminates at the
  -- filesystem root where `dirOf` reaches its fixed point; an arbitrary 64 bought nothing but a
  -- silent wrong answer for deep trees.
  let rec up (dir : String) : IO (Option String) := do
    let tomlExists ← try
      let _ ← IO.FS.readFile ⟨dir ++ "/Concrete.toml"⟩
      pure true
    catch _ => pure false
    if tomlExists then return some dir
    let parent := dirOf dir
    if parent == dir || parent.isEmpty then return none
    up parent
  up start

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
  /-- The scoped package identity, computed ONCE here and threaded, not re-derived per consumer.
      `Except` rather than a defaulted value: a project with no declared name gets a typed refusal
      and therefore no scoped evidence, because a default would make every unnamed project the same
      package. -/
  packageIdentity : Except Proof.PackageIdentityRefusal Proof.PackageIdentity
  policy        : Concrete.ProjectPolicy  -- the [policy] release profile, parsed once
  policyWarnings : List String            -- structural warnings from parsing [policy]
  -- proof/diagnostic facts derived once (Phase 4 #1): the source-location map, the
  -- synthesized proof registry, and the ProofCore — so build / test / report /
  -- policy stop re-deriving them per command.
  policyLocMap  : Report.FnLocMap
  registry      : Concrete.ProofRegistry
  pc            : Concrete.ProofCore
  /-- Which package each source file belongs to: (file, package key, package name). The key is
      the package's `PackageIdentity.digest`, formed by the same `packageIdentityOf` the root uses
      from the package's OWN manifest, or `unscoped:<name>` when it has none — never `""`. Module
      names cannot scope a declaration: two packages may each define a module `util` (R-0484 R10
      assumption identity). -/
  filePackages  : List (String × String × String) := []
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

  -- Inject builtin std if user didn't declare it explicitly.
  --
  -- NOT INTO ITSELF. `std` is a package, and when it is the project being loaded this injected
  -- `std` became a dependency of `std` — so every one of its modules landed in `depNames`, the
  -- `userModules` filter (`!depNames.contains m.name`) removed all of them, and the whole proof
  -- surface answered "0 functions" in silence. That is why a library package appeared to carry no
  -- scoped identity and no proof evidence: not a scoping gap for libraries in general, but a
  -- package shadowing itself. Measured on `std` 2026-08-16, which resolved 79 modules and reported
  -- zero of them.
  let selfName := (Concrete.Proof.packageField tomlContent "name").getD ""
  let hasStdDep := selfName == "std" || userDeps.any fun (name, _) => name == "std"
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
  let mut filePackages : List (String × String × String) := []
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
      let depToml ← try pure (some (← readFile (resolvedPath ++ "/Concrete.toml"))) catch _ => pure none
      let key := match depToml with
        | none => s!"unscoped:{depName}"
        | some t =>
          match Proof.packageIdentityOf t (modules.map (·.name)) [] (srcMap.map (·.2)) with
          | .ok pid => pid.digest
          | .error _ => s!"unscoped:{depName}"
      filePackages := filePackages ++ srcMap.map fun (f, _) => (f, key, depName)
  let tDepsLoaded ← IO.monoMsNow

  -- The project's entry source: `src/main.con`, or `src/lib.con` for a library package.
  let mainCandidate := projectRoot ++ "/src/main.con"
  let libCandidate := projectRoot ++ "/src/lib.con"
  let tryRead := fun (p : String) => do
    try pure (some (← readFile p)) catch _ => pure none
  let entry ← do
    match ← tryRead mainCandidate with
    | some s => pure (some (mainCandidate, s))
    | none   => pure ((← tryRead libCandidate).map fun s => (libCandidate, s))
  match entry with
  | none =>
    IO.eprintln s!"error: cannot read {mainCandidate}\nhint: projects need a src/main.con entry point (or src/lib.con for a library)"
    return Except.error 1
  | some (mainPath, source) =>

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
    -- Dependency structs (std's `Writer<cap C>`) are only visible once merged, so capability
    -- arguments are normalized again over the whole program.
    let merged : ParsedProgram := { modules := normalizeProgramCapArgs allModules }
    let summary := Pipeline.buildSummary merged
    match Pipeline.resolve merged summary with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return Except.error 1
    | .ok resolvedProg =>
    let depNames := depModules.map (·.name)
    -- Check EVERYTHING, dependencies included. The dep filter here was the
    -- H12-era exemption (std couldn't pass full front-end check yet); H12
    -- closed (std at 0 violations), and the filter had become a blind spot:
    -- a wrong-typed std edit (owned mode passed where &String expected,
    -- bug-043 fallout) compiled fine in project mode while
    -- `concrete std/src/lib.con --test` rejected it — defect-queue item 3.
    match Pipeline.check resolvedProg summary with
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
    -- A PROJECT THAT PARSES BUT VALIDATES TO NOTHING IS A REFUSAL, not a report of zero.
    --
    -- Found owning the library-package gap (R-0004, 2026-08-16): `std` resolves — 79 modules, 340
    -- exports, a populated obligation walker — and its VALIDATED core is empty, so every
    -- proof-surface report answered "0 functions" with no error. Silence there is the worst
    -- available answer: it reads identically to a package with nothing to prove, which is how a
    -- whole package can carry no evidence and no one notices.
    --
    -- Guarded on `parsed.modules` being non-empty so an genuinely empty source still behaves as
    -- before rather than acquiring a new failure mode.
    if validCore.coreModules.isEmpty && !merged.modules.isEmpty then
      IO.eprintln s!"error: '{mainPath}' parsed {merged.modules.length} module(s) but validated to an empty core, so every proof-surface report would answer 0 without explaining why\nhint: this is a compiler gap, not an empty package — see ROADMAP.md, library-package scoped identity"
      return Except.error 1

    -- Parse the release [policy] once, here, so no command re-derives it.
    let (policy, policyWarnings) := parsePolicy tomlContent
    -- Derive the location map / proof registry / ProofCore once (Phase 4 #1).
    let policyLocMap := Report.buildFnLocMap merged.modules mainPath
    let simpleLocMap := policyLocMap.map fun e => (e.qualName, (e.file, e.fnSpan.line))
    let registry ← loadRegistryWithLinks mainPath merged.modules validCore.coreModules
    -- THE PROJECT PATH has manifest material, so the identity is derived from it rather than
    -- synthesized. Computed HERE, above `pc`, because `extractProofCore` requires it — it was
    -- previously computed at the return statement, which is below this binding.
    let packageIdentity := Proof.packageIdentityOf tomlContent (merged.modules.map (·.name)) depNames
                             (allSrcMap.map (·.2))
    let pcE := extractProofCore? validCore packageIdentity simpleLocMap registry
    -- A project whose manifest declares no name yields no scoped identity and therefore no
    -- ProofCore. Refusing the LOAD is correct: every downstream consumer of this context treats
    -- `pc` as authoritative, so a ProofCore built without scope would let unscoped definitions
    -- participate in evidence — which is what this migration exists to prevent.
    let pc ← match pcE with
      | .ok pc => pure pc
      | .error w =>
        IO.eprintln s!"error: cannot extract proof core — {w.explain}"
        IO.eprintln "hint: Concrete.toml [package] must declare a name; definition identity is package-scoped"
        return Except.error 1
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
    let rootKey := match packageIdentity with
      | .ok pid => pid.digest
      | .error _ => s!"unscoped:{selfName}"
    let rootFiles := [mainPath] ++ subSrcMap.map (·.1)
    let allFilePackages := filePackages ++ rootFiles.map fun f => (f, rootKey, selfName)
    return Except.ok { projectRoot, validCore, parsed := merged, allSrcMap, tomlContent,
                       mainPath, depNames, packageIdentity, policy, policyWarnings, policyLocMap,
                       registry, pc, ledger, filePackages := allFilePackages }

end Concrete
