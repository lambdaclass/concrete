import Concrete

open Concrete

def usage : String :=
  "Usage: concrete <file.con> [-o output] [--emit-llvm] [--emit-core] [--emit-ssa] [--test] [--test --module <name>] [--report caps|unsafe|layout|interface|alloc|mono|authority|proof] [--fmt]\n       concrete build [-o output] [--emit-llvm]\n       concrete run [-- args...]\n       concrete test [--module <name>]"

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
  return #[llPath, "-o", outputPath, "-Wno-override-module", "-O2"] ++ sysrootFlags ++ extraFlags

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
      | .error e => return .error s!"error in module '{sub.name}': {e}"
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
    match Pipeline.lower mono with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := srcMap))
      return 1
    | .ok ssa =>
      for sm in ssa.ssaModules do
        IO.println (ppSModule sm)
      return 0

/-- Run pipeline to needed depth and produce a report. -/
def compileAndReport (inputPath : String) (reportType : String) : IO UInt32 := do
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
  -- All other reports need the full frontend
  match ← Pipeline.runFrontend inputPath source resolveAllModules with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := mainSrcMap))
    return 1
  | .ok (_, _, validCore, srcMap) =>
    if reportType == "caps" then
      IO.println (Report.capabilityReport validCore.coreModules)
      return 0
    if reportType == "unsafe" then
      IO.println (Report.unsafeReport validCore.coreModules)
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
      IO.println (Report.proofReport validCore.coreModules)
      return 0
    if reportType == "mono" then
      match Pipeline.monomorphize validCore with
      | .error ds =>
        IO.eprintln (renderDiagnostics ds (sourceMap := srcMap))
        return 1
      | .ok mono =>
        IO.println (Report.monoReport validCore.coreModules mono.coreModules)
        return 0
    IO.eprintln s!"Unknown report type: {reportType}. Use: caps, unsafe, layout, interface, alloc, mono, authority, proof"
    return 1

-- ============================================================
-- Concrete.toml project support
-- ============================================================

/-- Minimal TOML parser for Concrete.toml: extracts dependency paths.
    Only handles the format: `name = { path = "...", ... }` under `[dependencies]`. -/
def parseDependencies (content : String) : List (String × String) :=
  let lines := content.splitOn "\n"
  let rec go (ls : List String) (inDeps : Bool) (acc : List (String × String)) :=
    match ls with
    | [] => acc
    | l :: rest =>
      let trimmed := l.trimAscii.toString
      if trimmed.startsWith "[dependencies]" then
        go rest true acc
      else if trimmed.startsWith "[" then
        -- New section, stop parsing dependencies
        go rest false acc
      else if inDeps && trimmed.length > 0 then
        -- Try to parse: name = { path = "...", ... }
        match trimmed.splitOn "=" with
        | name :: valParts =>
          let depName := name.trimAscii.toString
          let valStr := "=".intercalate valParts  -- rejoin after first =
          -- Extract path = "..." from the value
          match valStr.splitOn "path" with
          | _ :: pathRest :: _ =>
            let afterPath := ("path".intercalate [pathRest])  -- text after "path"
            -- Find first quote after =
            match afterPath.splitOn "\"" with
            | _ :: pathVal :: _ =>
              go rest true (acc ++ [(depName, pathVal)])
            | _ => go rest true acc
          | _ => go rest true acc
        | _ => go rest true acc
      else
        go rest inDeps acc
  go lines false []

/-- Find Concrete.toml by walking up from a directory. -/
def findProjectRoot (startDir : String) : IO (Option String) := do
  let tomlPath := startDir ++ "/Concrete.toml"
  let tomlExists ← try
    let _ ← IO.FS.readFile ⟨tomlPath⟩
    pure true
  catch _ => pure false
  if tomlExists then
    return some startDir
  -- Try parent directory (one level up)
  let parent := dirOf startDir
  if parent == startDir then return none
  let parentToml := parent ++ "/Concrete.toml"
  let parentExists ← try
    let _ ← IO.FS.readFile ⟨parentToml⟩
    pure true
  catch _ => pure false
  if parentExists then
    return some parent
  return none

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
  | .error e => return .error s!"dependency '{depName}': parse error: {e}"
  | .ok modules =>
    let baseDir := depPath ++ "/src"
    match ← resolveAllModules baseDir modules libPath with
    | .error e => return .error s!"dependency '{depName}': {e}"
    | .ok (resolved, srcMap) =>
      let srcMap := [(libPath, source)] ++ srcMap
      return .ok (resolved, srcMap)

/-- Compile a project from Concrete.toml. -/
def compileBuild (projectRoot : String) (outputPath : Option String) (emitLLVM : Bool) (quiet : Bool := false) : IO UInt32 := do
  -- Read Concrete.toml
  let tomlPath := projectRoot ++ "/Concrete.toml"
  let tomlContent ← readFile tomlPath
  let userDeps := parseDependencies tomlContent

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
    -- For builtin std (injected above), depPath is already absolute
    let resolvedPath := if depPath.startsWith "/" then depPath
      else resolveDependencyPath projectRoot depPath
    match ← loadDependency depName resolvedPath with
    | .error e =>
      IO.eprintln e
      return 1
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
    return 1
  | some source =>

  -- Parse the project source
  match Pipeline.parse source with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := [(mainPath, source)]))
    return 1
  | .ok parsed =>
  -- Resolve project's own mod X; stubs
  let baseDir := projectRoot ++ "/src"
  match ← Pipeline.resolveFiles baseDir parsed mainPath resolveAllModules with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := [(mainPath, source)]))
    return 1
  | .ok (resolvedParsed, subSrcMap) =>
    -- Strip #[test] functions from dependency modules (they aren't needed for regular builds
    -- and can trigger capability/monomorphization errors for test-only code paths)
    let stripTests : Module → Module := fun m =>
      { m with
        functions := m.functions.filter fun f => !f.isTest
        submodules := m.submodules.map fun sub =>
          { sub with functions := sub.functions.filter fun f => !f.isTest }
      }
    let depModulesClean := depModules.map stripTests
    -- Merge: dependency modules come first, then project modules
    let allModules : List Module := depModulesClean ++ resolvedParsed.modules
    let allSrcMap : SourceMap := [(mainPath, source)] ++ subSrcMap ++ depSrcMap
    let merged : ParsedProgram := { modules := allModules }

    -- Now run the standard pipeline from buildSummary onward
    let summary := Pipeline.buildSummary merged
    match Pipeline.resolve merged summary with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return 1
    | .ok resolvedProg =>
    -- Only check project modules (skip dependency modules which are already validated)
    let depNames := depModules.map (·.name)
    let projectResolved : List ResolvedModule :=
      resolvedProg.modules.filter fun rm => !depNames.contains rm.module.name
    let projectResolvedProg : ResolvedProgram := { modules := projectResolved }
    match Pipeline.check projectResolvedProg summary with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return 1
    | .ok () =>
    match Pipeline.elaborate resolvedProg summary with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return 1
    | .ok elabProg =>
    match Pipeline.coreCheck elabProg with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return 1
    | .ok validCore =>
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
      if !quiet then IO.println s!"Built {outPath}"
      return 0

/-- Run tests for a project from Concrete.toml. Like compileBuild but in test mode. -/
partial def compileTestBuild (projectRoot : String) (moduleFilter : Option String := none) : IO UInt32 := do
  let tomlPath := projectRoot ++ "/Concrete.toml"
  let tomlContent ← readFile tomlPath
  let userDeps := parseDependencies tomlContent

  -- Inject builtin std
  let hasStdDep := userDeps.any fun (name, _) => name == "std"
  let deps ← if hasStdDep then pure userDeps
    else match ← findBuiltinStd with
      | some stdPath => pure (("std", stdPath) :: userDeps)
      | none => pure userDeps

  -- Load dependencies
  let mut depModules : List Module := []
  let mut depSrcMap : SourceMap := []
  for (depName, depPath) in deps do
    let resolvedPath := if depPath.startsWith "/" then depPath
      else resolveDependencyPath projectRoot depPath
    match ← loadDependency depName resolvedPath with
    | .error e => IO.eprintln e; return 1
    | .ok (modules, srcMap) =>
      depModules := depModules ++ modules
      depSrcMap := depSrcMap ++ srcMap

  -- Load project source
  let mainPath := projectRoot ++ "/src/main.con"
  let sourceResult ← try
    let s ← readFile mainPath
    pure (some s)
  catch _ => pure none
  match sourceResult with
  | none =>
    IO.eprintln s!"error: cannot read {mainPath}\nhint: projects need a src/main.con entry point"
    return 1
  | some source =>

  match Pipeline.parse source with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := [(mainPath, source)]))
    return 1
  | .ok parsed =>
  let baseDir := projectRoot ++ "/src"
  match ← Pipeline.resolveFiles baseDir parsed mainPath resolveAllModules with
  | .error ds =>
    IO.eprintln (renderDiagnostics ds (sourceMap := [(mainPath, source)]))
    return 1
  | .ok (resolvedParsed, subSrcMap) =>
    let allModules := depModules ++ resolvedParsed.modules
    let allSrcMap := [(mainPath, source)] ++ subSrcMap ++ depSrcMap
    let merged : ParsedProgram := { modules := allModules }
    let summary := Pipeline.buildSummary merged
    match Pipeline.resolve merged summary with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return 1
    | .ok resolvedProg =>
    let depNames := depModules.map (·.name)
    let projectResolved :=
      resolvedProg.modules.filter fun rm => !depNames.contains rm.module.name
    let projectResolvedProg : ResolvedProgram := { modules := projectResolved }
    match Pipeline.check projectResolvedProg summary with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return 1
    | .ok () =>
    match Pipeline.elaborate resolvedProg summary with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return 1
    | .ok elabProg =>
    match Pipeline.coreCheck elabProg with
    | .error ds =>
      IO.eprintln (renderDiagnostics ds (sourceMap := allSrcMap))
      return 1
    | .ok validCore =>
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
  | [inputPath, "--emit-core"] =>
    compileAndEmit inputPath "core"
  | [inputPath, "--emit-ssa"] =>
    compileAndEmit inputPath "ssa"
  | [inputPath, "-o", outputPath] =>
    compileSSA inputPath outputPath false
  | [inputPath, "--report", reportType] =>
    compileAndReport inputPath reportType
  | [inputPath, "--fmt"] =>
    let source ← readFile inputPath
    match parse source with
    | .error e =>
      IO.eprintln s!"parse error: {e}"
      return 1
    | .ok modules =>
      IO.print (formatProgram modules)
      return 0
  | _ =>
    IO.eprintln usage
    return 1
