import Concrete

open Concrete

def usage : String :=
  "Usage: concrete <file.con> [-o output] [--emit-llvm] [--emit-core] [--emit-ssa] [--test] [--test --module <name>] [--interp] [--report caps|unsafe|layout|interface|alloc|mono|authority|proof|eligibility|proof-status|obligations|extraction|lean-stubs|check-proofs|proof-diagnostics|proof-deps|proof-bundle|traceability|diagnostics-json|effects|recursion|stack-depth|fingerprints|consistency|contracts|verify|audit] [--query KIND|KIND:FUNCTION|fn:FUNCTION] [--fmt]\n       concrete build [-o output] [--emit-llvm]\n       concrete check\n       concrete audit <file.con>\n       concrete prove <file.con> <module.function> [--json] [--out <path>] [--force] [--emit-link] [--emit-lean] [--show-obligation <id>] [--replay] [--nearest-lemmas]\n       concrete prove --help=agent | --capabilities | --schema\n       concrete run [-- args...]\n       concrete test [--module <name>]\n       concrete diff <old.json> <new.json> [--json]\n       concrete snapshot <file.con> [-o output.json]\n       concrete debug-bundle <file.con> [-o dir]\n       concrete reduce <file.con> --predicate <pred> [-o output] [--verbose]\n       concrete --version"

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

def writeFile (path : String) (content : String) : IO Unit := do
  IO.FS.writeFile ⟨path⟩ content

def readFile (path : String) : IO String := do
  IO.FS.readFile ⟨path⟩

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
          match ← resolveModules subBaseDir { subMod with name := sub.name } paths sources with
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

/-- Compile via SSA pipeline: Parse → Resolve → Check → Elab → CoreCanonicalize → CoreCheck → Mono → Lower → SSAVerify → SSACleanup → EmitSSA → clang -/
def compileSSA (inputPath : String) (outputPath : String) (emitLLVM : Bool) : IO UInt32 := do
  let source ← readFile inputPath
  match ← Pipeline.runFrontend inputPath source resolveAllModules with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := [(inputPath, source)]))
    return 1
  | .ok (_, _, validCore, srcMap) =>
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
    | .ok exitCode =>
      -- Match compiled binary contract: print return value, exit 0
      IO.println s!"{exitCode}"
      return 0

/-- Compile and run tests: Parse → ... → EmitSSA (test mode) → clang → run -/
def compileTest (inputPath : String) (moduleFilter : Option String := none) : IO UInt32 := do
  let source ← readFile inputPath
  match ← Pipeline.runFrontend inputPath source resolveAllModules with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := [(inputPath, source)]))
    return 1
  | .ok (_, _, validCore, srcMap) =>
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
  | .ok (_, _, validCore, srcMap) =>
    if mode == "core" then
      for cm in validCore.coreModules do
        IO.println (ppCModule cm)
      return 0
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

/-- The proof registry a report/query sees: the in-source proof links
    synthesized from `#[proof_by]`/`#[spec]`/`#[proof_fingerprint]`. Legacy JSON
    `proof-registry.json` support was removed — source links are the only model,
    so every registry-consuming path (report, query, policy, snapshot,
    traceability) goes through this one synthesizer. -/
def loadRegistryWithLinks (_inputPath : String)
    (astModules : List Concrete.Module) (coreModules : List Concrete.CModule) :
    IO Concrete.ProofRegistry := do
  return Report.synthesizeSourceLinks astModules coreModules

/-- Stable schema version for `concrete prove` machine-readable output. -/
def proveSchemaVersion : String := "1"

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
    let pc := extractProofCore validCore simpleLocMap registry
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

/-- Minimal TOML parser for Concrete.toml: extracts dependency paths.
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
  let knownSections := ["[package]", "[dependencies]", "[policy]"]
  let sectionWarns := trimmedLines.filterMap fun l =>
    if l.startsWith "[" && !l.startsWith "#" then
      if knownSections.any (l.startsWith ·) then none
      else some s!"warning: Concrete.toml has unrecognized section '{l}'"
    else none
  pkgWarns ++ sectionWarns

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
  -- Try common relative paths:
  -- .lake/build/bin/concrete → ../../.. → repo root → std/
  -- build/bin/concrete → ../.. → repo root → std/
  -- bin/concrete → .. → repo root → std/
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
      return .ok (resolved, srcMap)

/-- Shared context produced by loading a project from Concrete.toml. -/
structure ProjectContext where
  validCore   : ValidatedCore
  parsed      : ParsedProgram      -- merged modules (deps + project)
  allSrcMap   : SourceMap
  tomlContent : String
  mainPath    : String
  depNames    : List String

/-- Load a project to ValidatedCore. Shared by build, test, and check. -/
partial def loadProject (projectRoot : String) (stripTestFns : Bool := false) : IO (Except UInt32 ProjectContext) := do
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
    return Except.ok { validCore, parsed := merged, allSrcMap, tomlContent, mainPath, depNames }

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

/-- Render the contracts report plus the call-site obligation section, running
    the `bv_decide` backend on the closed-but-non-literal call-site obligations
    and `omega` on the loop init/variant VCs. -/
def renderContracts (parsedModules : List Concrete.Module) (registry : Concrete.ProofRegistry) : IO String := do
  let obs := Report.callSiteObligations parsedModules
  let cands := ((List.range obs.length).zip obs).filterMap fun (i, o) => o.leanGoal.map (fun g => (i, g))
  let proved ← bvDischargeCallSites cands
  let provedVCs ← kernelDischargeLoopVCs (Report.loopVCGoals parsedModules)
  let boundsObls := Report.boundsObligations parsedModules
  let provedBounds ← kernelDischargeLoopVCs (Report.boundsGoals parsedModules)
  let divObls := Report.divObligations parsedModules
  let provedDiv ← kernelDischargeLoopVCs (Report.divGoals parsedModules)
  let ovfObls := Report.overflowObligations parsedModules
  let provedOvf ← kernelDischargeLoopVCs (Report.overflowGoals parsedModules)
  -- nonlinear fallback: only the bv goals omega did NOT already discharge.
  let bvOvfCands := (Report.overflowBVGoals parsedModules).filter (fun (k, _) => !provedOvf.contains k)
  let provedOvfBV ← bvDischargeOverflow bvOvfCands
  return Report.contractsReport parsedModules registry provedVCs
    ++ Report.renderCallSites obs proved
    ++ Report.renderBounds boundsObls provedBounds
    ++ Report.renderDiv divObls provedDiv
    ++ Report.renderOverflow ovfObls provedOvf provedOvfBV

/-- Run pipeline and check a profile constraint.
    If the input file lives inside a `Concrete.toml` project, route
    through project mode so std and other dependencies resolve. -/
def compileAndReport (inputPath : String) (reportType : String)
    (proveTarget : Option String := none) (proveOut : Option String := none)
    (proveForce : Bool := false) (proveEmitLink : Bool := false)
    (proveShowObl : Option String := none) (proveReplay : Bool := false)
    (proveJson : Bool := false) (proveNearestLemmas : Bool := false)
    (proveEmitLean : Bool := false) (proveStdout : Bool := false) : IO UInt32 := do
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
  let frontendResult : Except UInt32 (ParsedProgram × ValidatedCore × ValidatedCore × SourceMap) ←
    match ← findProjectRoot inputDir with
    | some root =>
      match ← loadProject root with
      | .error ec => pure (Except.error ec)
      | .ok ctx =>
        let { validCore, parsed, allSrcMap, depNames, .. } := ctx
        let userModules := validCore.coreModules.filter fun m => !depNames.contains m.name
        let fullValidCore : ValidatedCore := { validCore with coreModules := userModules }
        let scopedModules ← scopeUserModulesToFile userModules inputPath root
        let scopedValidCore : ValidatedCore := { validCore with coreModules := scopedModules }
        pure (Except.ok (parsed, fullValidCore, scopedValidCore, allSrcMap))
    | none =>
      match ← Pipeline.runFrontend inputPath source resolveAllModules with
      | .error ds =>
        IO.eprintln (renderDiagnostics ds (sourceMap := mainSrcMap))
        pure (Except.error 1)
      | .ok (parsed, _, validCore, srcMap) =>
        pure (Except.ok (parsed, validCore, validCore, srcMap))
  match frontendResult with
  | .error ec => return ec
  | .ok (parsed, fullValidCore, scopedValidCore, srcMap) =>
    let locMap := Report.buildFnLocMap parsed.modules inputPath
    let simpleLocMap := locMap.map fun e => (e.qualName, (e.file, e.fnSpan.line))
    -- The registry is the in-source proof links (#[proof_by]/#[spec]/...)
    -- synthesized from the FULL user package.
    let registry := Report.synthesizeSourceLinks parsed.modules fullValidCore.coreModules
    -- pc and registry validation run on the FULL user package: a
    -- registry entry naming a function defined in a sibling file must
    -- still validate when the user is querying just one file.
    let pc := extractProofCore fullValidCore simpleLocMap registry
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
          IO.println (Report.nearestLemmas pc parsed.modules qual provedVCs proveJson)
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
        let proveStatus := (pc.obligations.find? (·.functionId.qualName == qual)).map (·.status.canonical) |>.getD "missing"
        let proveExit : UInt32 := match proveStatus with
          | "stale" => 3 | "missing" => 2 | "blocked" => 2 | _ => 0
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
    if reportType == "contracts" then
      IO.println (← renderContracts parsed.modules registry)
      return 0
    if reportType == "caps" then
      IO.println (Report.capabilityReport validCore.coreModules)
      return 0
    if reportType == "unsafe" then
      IO.println (Report.unsafeReport validCore.coreModules pc)
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
    if reportType == "lean-stubs" then
      IO.println (Report.leanStubsReport pc (registry := registry))
      return 0
    if reportType == "check-proofs" then
      -- Collect all proof names from obligations with specs attached
      let proofNames := pc.obligations.filterMap fun o =>
        match o.spec with
        | some s =>
          if o.status == .proved || o.status == .stale then
            some (o.functionId.qualName, s.proofName, s.source, o.status)
          else none
        | none => none
      if proofNames.isEmpty then
        IO.println "=== Lean Proof Kernel Check ===\n\nNo proved or stale obligations with proof names to check."
        return 0
      -- Generate temp Lean file with #check for each theorem
      let tmpDir ← IO.Process.output { cmd := "mktemp", args := #["-d"] }
      let tmpPath := tmpDir.stdout.trimAscii.toString ++ "/check_proofs.lean"
      -- Import the umbrella module (not just Concrete.Proof) so that
      -- refinement theorems defined in Concrete.Sha256Refine — which lives
      -- in the Concrete.Proof *namespace* but a different *module* — are
      -- reachable for `#check`. The umbrella imports Proof, so all existing
      -- flagship proofs remain checkable.
      let mut leanSrc := "import Concrete\n\n"
      leanSrc := leanSrc ++ "-- Auto-generated by `concrete --report check-proofs`\n"
      leanSrc := leanSrc ++ "-- Verifies that referenced Lean theorems exist and type-check.\n\n"
      for (fn, proofName, _, _) in proofNames do
        leanSrc := leanSrc ++ s!"-- {fn}\n#check @{proofName}\n\n"
      -- Also kernel-check source-contract discharge theorems (the `ensures_proof`
      -- naming the theorem that proves a `#[ensures(...)]` obligation), so an
      -- obligation reported `proved_by_lean` is backed by a real kernel check.
      let ensuresThms := registry.filterMap fun e => e.ensuresProof.map (e.function, ·)
      for (fn, thm) in ensuresThms do
        leanSrc := leanSrc ++ s!"-- {fn} (ensures)\n#check @{thm}\n\n"
      IO.FS.writeFile ⟨tmpPath⟩ leanSrc
      -- Invoke lake env lean to check the file
      let result ← IO.Process.output {
        cmd := "lake"
        args := #["env", "lean", tmpPath]
        env := #[("LAKE_TERM_ANSI", "0")]
      }
      -- Parse results: Lean may put errors on stdout or stderr
      let combined := result.stdout.trimAscii.toString ++ "\n" ++ result.stderr.trimAscii.toString
      let mut verified : List String := []
      let mut failed : List (String × String) := []
      for (fn, proofName, _, _) in proofNames do
        -- Check if this proof name produced an error (Lean uses backticks in error messages)
        let errNeedle := s!"`{proofName}`"
        let stderrParts := combined.splitOn errNeedle
        let hasError := stderrParts.length != 1
        if hasError then
          failed := failed ++ [(fn, proofName)]
        else
          verified := verified ++ [fn]
      -- If exit code non-zero but no specific failures found, mark all as failed
      if result.exitCode != 0 && failed.isEmpty then
        for (fn, proofName, _, _) in proofNames do
          if !failed.any (fun (f, _) => f == fn) then
            failed := failed ++ [(fn, proofName)]
        verified := []
      let generalFailure := result.exitCode != 0 && failed.isEmpty
      -- Render report
      let mut out := "=== Lean Proof Kernel Check ===\n"
      out := out ++ s!"\nToolchain: "
      let tc ← try
        let tcContent ← IO.FS.readFile ⟨"lean-toolchain"⟩
        pure tcContent.trimAscii.toString
      catch _ => pure "unknown"
      out := out ++ tc ++ "\n"
      if !verified.isEmpty then
        out := out ++ s!"\n  Kernel-verified ({verified.length}):\n"
        for fn in verified do
          let proofName := (proofNames.find? fun (f, _, _, _) => f == fn).map (·.2.1) |>.getD "?"
          let source := (proofNames.find? fun (f, _, _, _) => f == fn).map (·.2.2.1) |>.getD .hardcoded
          let sourceTag := if source == .registry then "registry" else "hardcoded"
          out := out ++ s!"    ✓ {fn} — {proofName} ({sourceTag})\n"
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
        out := out ++ s!"\n  Lean check failed (exit code {result.exitCode}):\n"
        out := out ++ s!"    {combined.take 500}\n"
      -- Generate taxonomy diagnostics for failures
      let checkDiags := Concrete.checkProofResultsToDiagnostics
        (failed.map fun (fn, pn) => (fn, pn, true))
      if !checkDiags.isEmpty then
        out := out ++ s!"\n  Diagnostics ({checkDiags.length}):\n"
        for d in checkDiags do
          out := out ++ s!"    [{d.kind.code}] {d.function}: failure={d.failureClass}, repair={d.repairClass}\n"
      out := out ++ s!"\nSummary: {verified.length} verified, {failed.length} failed"
      if generalFailure then
        out := out ++ " (general compilation error)"
      -- Clean up temp dir
      let _ ← IO.Process.output { cmd := "rm", args := #["-rf", tmpDir.stdout.trimAscii.toString] }
      IO.println out
      return (if failed.isEmpty && !generalFailure then 0 else 1)
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
      IO.println (Report.auditReport validCore.coreModules locMap srcMap (registry := registry) (pc := pc))
      if Report.hasContracts parsed.modules then
        IO.println (← renderContracts parsed.modules registry)
      return (if hasRegistryErrors then 1 else 0)
    if reportType == "verify" then
      -- Pass-by-pass verify gates: post-elab, post-mono, post-lower,
      -- post-cleanup. Each gate's diagnostics are reported separately
      -- per docs/VERIFY_GATES.md. Earlier failures (mono refusing to
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
    IO.eprintln s!"Unknown report type: {reportType}. Use: caps, unsafe, layout, interface, alloc, mono, authority, proof, eligibility, proof-status, obligations, extraction, proof-diagnostics, proof-deps, proof-bundle, lean-stubs, check-proofs, traceability, diagnostics-json, schema, diagnostic-codes, effects, recursion, fingerprints, consistency, verify, audit"
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
      let { validCore, parsed, allSrcMap, depNames, .. } := ctx
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
      let pc := extractProofCore fullUserValidCore simpleLocMap
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
      let pc := extractProofCore validCore simpleLocMap
      let srcMap : SourceMap := [(inputPath, source)]
      let (pass, report) := Report.checkPredictable validCore.coreModules locMap srcMap pc
      IO.println report
      return if pass then 0 else 1

/-- Compile a project from Concrete.toml. -/
def compileBuild (projectRoot : String) (outputPath : Option String) (emitLLVM : Bool) (quiet : Bool := false) : IO UInt32 := do
  match ← loadProject projectRoot (stripTestFns := true) with
  | .error exitCode => return exitCode
  | .ok ctx =>
    let { validCore, parsed, allSrcMap, tomlContent, mainPath, depNames, .. } := ctx
    -- Enforce [policy] from Concrete.toml
    let (policy, polWarnings) := parsePolicy tomlContent
    for w in polWarnings do IO.eprintln w
    -- Always compute ProofCore for summary and policy
    let policyLocMap := Report.buildFnLocMap parsed.modules mainPath
    let simpleLocMap := policyLocMap.map fun e => (e.qualName, (e.file, e.fnSpan.line))
    let registry ← loadRegistryWithLinks mainPath parsed.modules validCore.coreModules
    let pc := extractProofCore validCore simpleLocMap registry
    if !policy.isEmpty then
      let policyDs := enforcePolicy policy validCore.coreModules
        (locMap := policyLocMap) (pc := pc) (depNames := depNames)
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
    let { validCore, parsed, allSrcMap, tomlContent, mainPath, depNames, .. } := ctx
    -- Enforce [policy] from Concrete.toml
    let (policy, polWarnings) := parsePolicy tomlContent
    for w in polWarnings do IO.eprintln w
    if !policy.isEmpty then
      let policyLocMap := Report.buildFnLocMap parsed.modules mainPath
      let simpleLocMap := policyLocMap.map fun e => (e.qualName, (e.file, e.fnSpan.line))
      let registry ← loadRegistryWithLinks mainPath parsed.modules validCore.coreModules
      let pc := extractProofCore validCore simpleLocMap registry
      let policyDs := enforcePolicy policy validCore.coreModules
        (locMap := policyLocMap) (pc := pc) (depNames := depNames)
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
def proveAgentHelp : String := String.intercalate "\n" [
  "concrete prove — agent guide (schema v" ++ proveSchemaVersion ++ ")",
  "",
  "PURPOSE: author and verify a Lean proof for one Concrete function.",
  "",
  "WORKFLOW (each step prints what to do next):",
  "  1. concrete prove <file> <module.fn> --json     # proof context + next_actions",
  "  2. concrete prove <file> <module.fn> --show-obligation <id>   # one obligation",
  "  3. concrete prove <file> <module.fn> --emit-lean             # compilable Lean stub",
  "  4. write the Lean proof in Concrete/Proof.lean (see PROOFKIT_GUIDE)",
  "  5. concrete prove <file> <module.fn> --emit-link             # in-source link block",
  "     (paste #[spec]/#[proof_by]/#[proof_coverage]/#[proof_fingerprint] above the fn)",
  "  6. concrete <file> --report check-proofs                     # kernel-verify",
  "  7. concrete prove <file> <module.fn> --replay                # re-run omega/bv_decide",
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
  "EXIT CODES:",
  "  0  success (proved / clean / info printed)",
  "  1  invalid invocation (bad args, unknown function)",
  "  2  obligations missing (eligible but unproved)",
  "  3  stale evidence (body changed since the proof was linked)",
  "  4  proof-check failure (Lean kernel rejected a referenced theorem)",
  "  5  solver/checker failure (omega/bv_decide/lake could not run)",
  "  6  internal compiler error",
  "",
  "PROOF MODEL: in-source links only (no proof-registry.json). A function carries",
  "  #[spec]/#[proof_by]/#[ensures_proof]/#[proof_coverage]/#[proof_fingerprint]." ]

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
  "    \"replay_json\": true",
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

def main (args : List String) : IO UInt32 := do
  -- Check for "build" command first (before generic single-arg pattern)
  if args.head? == some "build" then
    let cwd ← IO.currentDir
    match ← findProjectRoot cwd.toString with
    | none =>
      IO.eprintln "error: no Concrete.toml found in current directory or parent\nhint: create one with [package] section, or run from a project directory"
      return 1
    | some root =>
      let emitLLVM := args.contains "--emit-llvm"
      let outPath := match args with
        | _ :: "-o" :: p :: _ => some p
        | _ => none
      return ← compileBuild root outPath emitLLVM
  -- concrete run [-- args...]
  if args.head? == some "run" then
    let cwd ← IO.currentDir
    match ← findProjectRoot cwd.toString with
    | none =>
      IO.eprintln "error: no Concrete.toml found in current directory or parent\nhint: create one with [package] section, or run from a project directory"
      return 1
    | some root =>
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
    let cwd ← IO.currentDir
    match ← findProjectRoot cwd.toString with
    | none =>
      IO.eprintln "error: no Concrete.toml found in current directory or parent\nhint: create one with [package] section, or run from a project directory"
      return 1
    | some root =>
      let modFilter := match args with
        | _ :: "--module" :: m :: _ => some m
        | _ => none
      return ← compileTestBuild root modFilter
  -- concrete check — run frontend + proof status without codegen
  if args.head? == some "check" then
    let cwd ← IO.currentDir
    match ← findProjectRoot cwd.toString with
    | none =>
      IO.eprintln "error: no Concrete.toml found in current directory or parent\nhint: create one with [package] section, or run from a project directory"
      return 1
    | some root =>
      match ← loadProject root (stripTestFns := true) with
      | .error exitCode => return exitCode
      | .ok ctx =>
        let { validCore, parsed, allSrcMap, tomlContent, mainPath, depNames, .. } := ctx
        -- Enforce [policy] from Concrete.toml
        let (policy, polWarnings) := parsePolicy tomlContent
        for w in polWarnings do IO.eprintln w
        let policyLocMap := Report.buildFnLocMap parsed.modules mainPath
        let simpleLocMap := policyLocMap.map fun e => (e.qualName, (e.file, e.fnSpan.line))
        let registry ← loadRegistryWithLinks mainPath parsed.modules validCore.coreModules
        let pc := extractProofCore validCore simpleLocMap registry
        -- Validate registry
        let regIssues := Concrete.validateRegistry pc registry
        for issue in regIssues do
          IO.eprintln (Concrete.renderRegistryIssue issue)
        if !policy.isEmpty then
          let policyDs := enforcePolicy policy validCore.coreModules
            (locMap := policyLocMap) (pc := pc) (depNames := depNames)
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
        let hasIssues := userPc.obligations.any fun o =>
          o.status == .stale || o.status == .missing || o.status == .blocked
        let hasRegistryErrors := regIssues.any (·.isError)
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
        let pc := extractProofCore validCore simpleLocMap registry
        -- Collect core facts (same as diagnostics-json)
        let coreFacts := Report.collectCoreFacts validCore.coreModules locMap registry pc
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
    if pargs.contains "--help=agent" then IO.println proveAgentHelp; return 0
    if pargs.contains "--capabilities" then IO.println proveCapabilitiesJson; return 0
    if pargs.contains "--schema" then IO.println proveSchemaJson; return 0
    match pargs with
    | inputPath :: target :: rest =>
      let force := rest.contains "--force"
      let emitLink := rest.contains "--emit-link"
      let replay := rest.contains "--replay"
      let proveJson := rest.contains "--json"
      let nearestLemmas := rest.contains "--nearest-lemmas"
      let emitLean := rest.contains "--emit-lean"
      let stdout := rest.contains "--stdout"
      let outPath := match rest.dropWhile (· != "--out") with
        | _ :: p :: _ => some p
        | _ => none
      let showObl := match rest.dropWhile (· != "--show-obligation") with
        | _ :: id :: _ => some id
        | _ => none
      return (← compileAndReport inputPath "prove"
        (proveTarget := some target) (proveOut := outPath) (proveForce := force)
        (proveEmitLink := emitLink) (proveShowObl := showObl) (proveReplay := replay)
        (proveJson := proveJson) (proveNearestLemmas := nearestLemmas)
        (proveEmitLean := emitLean) (proveStdout := stdout))
    | _ =>
      IO.eprintln "Usage: concrete prove <file.con> <module.function> [--json] [--out <path>] [--force] [--emit-link] [--emit-lean] [--show-obligation <id>] [--replay] [--nearest-lemmas]\n       concrete prove --help=agent | --capabilities | --schema   (discovery; no file needed)"
      return 1
  -- concrete --version
  if args == ["--version"] then
    let id ← compilerIdentity
    IO.println id
    return 0
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
  | [inputPath, "-o", outputPath] =>
    compileSSA inputPath outputPath false
  | [inputPath, "--check", checkType] =>
    compileAndCheck inputPath checkType
  | [inputPath, "--report", reportType] =>
    compileAndReport inputPath reportType
  | [inputPath, "--query", query] =>
    compileAndQuery inputPath query
  | [inputPath, "--fmt"] =>
    let source ← readFile inputPath
    match parse source with
    | .error e =>
      IO.eprintln s!"parse error: {renderDiagnostics e}"
      return 1
    | .ok modules =>
      IO.print (formatProgram modules)
      return 0
  | _ =>
    IO.eprintln usage
    return 1
