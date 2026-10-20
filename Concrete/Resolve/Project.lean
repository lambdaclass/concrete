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
  let knownSections := ["[package]", "[dependencies]", "[policy]", "[profile]", "[native]"]
  let sectionWarns := trimmedLines.filterMap fun l =>
    if l.startsWith "[" && !l.startsWith "#" then
      if knownSections.any (l.startsWith ·) then none
      else some s!"warning: Concrete.toml has unrecognized section '{l}'"
    else none
  pkgWarns ++ sectionWarns

/-! ## Native libraries — the `[native]` section (gap G1; docs/platform/NATIVE_LIBRARIES.md)

A package declares the native libraries its `extern fn`s resolve against:

```toml
[native]
pkg-config = ["rocksdb"]     # resolved with `pkg-config --libs` at link time
libs       = ["m"]           # passed as -l<name>
lib-dirs   = ["native/lib"]  # relative to the package root; -L<dir> and an rpath
```

The declarations of the root package and of every dependency package are collected by
`loadProject` and turned into link flags by `resolveNativeLinkFlags`, for the final link only.

FAIL CLOSED, everywhere. An unknown key, a malformed array, a name that is not a plain library
name, a `[native.*]` table, a missing pkg-config, an unknown pkg-config package, a pkg-config flag
outside the accepted set and a library directory that does not exist are each refused with a
diagnostic naming the package. Nothing is silently dropped, because a dropped library reappears
as an unresolved symbol far from its cause — the failure G1 recorded.

NO FLAG INJECTION. Every value reaches clang as one argv element, never through a shell. Names are
restricted to `[A-Za-z0-9_][A-Za-z0-9_.+-]*`, so a name cannot begin with `-` and become a flag.
Directories are canonicalised, must exist, and must not contain `,` (which `-Wl,-rpath,<dir>` would
split into further linker arguments). pkg-config output is parsed against an allow-list (`-L`, `-l`,
`-pthread`, `-framework <name>`); anything else in a `.pc` file is refused, not forwarded. -/

inductive NativeKind where
  | pkgConfig | lib | libDir
  deriving BEq, Repr, Inhabited

def NativeKind.key : NativeKind → String
  | .pkgConfig => "pkg-config"
  | .lib => "libs"
  | .libDir => "lib-dirs"

/-- One declaration from one package's `[native]` section. -/
structure NativeLibDecl where
  /-- The declaring package: its dependency binding, or the root package's name. -/
  package : String
  /-- The declaring package's root directory; `lib-dirs` resolve against it. -/
  packageRoot : String
  kind : NativeKind
  /-- As written in the manifest. -/
  value : String
  deriving BEq, Repr, Inhabited

def NativeLibDecl.describe (d : NativeLibDecl) : String :=
  match d.kind with
  | .pkgConfig => s!"pkg-config package '{d.value}'"
  | .lib => s!"library '{d.value}' (-l{d.value})"
  | .libDir => s!"library directory '{d.value}'"

/-- A library or pkg-config package name: `[A-Za-z0-9_][A-Za-z0-9_.+-]*`, at most 128 characters.
    Never begins with `-`, so it can never be read as a flag. -/
def isValidNativeName (s : String) : Bool :=
  let cs := s.toList
  let ok (c : Char) := c.isAlphanum || c == '_' || c == '.' || c == '+' || c == '-'
  match cs with
  | [] => false
  | c :: _ => cs.length ≤ 128 && (c.isAlphanum || c == '_') && cs.all ok

/-- A name `-l<name>` may carry: a valid native name whose `-l` spelling is not ALSO a driver or
    linker option. Prefixing `-l` does not make an argument a library: ld64 reads `-lto_library`,
    `-ld_classic`, `-ld_new`, `-lazy-l…`, `-lazy_library`, `-lazy_framework` and `-load_hidden` as
    options, clang reads `-lazy_*` itself, and GNU ld matches single-dash long options such as
    `-library…` before `-l`. Names beginning with those spellings are refused, in the manifest and
    in pkg-config output alike. -/
def isLinkableLibName (s : String) : Bool :=
  isValidNativeName s &&
    !(["to_", "to-", "d_", "d-", "azy", "oad_", "ibrary"].any (s.startsWith ·))

/-- A library directory as written: non-empty, not flag-shaped, no `,` (it is spliced into
    `-Wl,-rpath,<dir>`), no `:` (an ELF rpath list separator) or `$` (an rpath token such as
    `$ORIGIN`), no control characters. Checked on the written and the canonical path. -/
def isValidNativeDir (s : String) : Bool :=
  !s.isEmpty && s.length ≤ 4096 && !s.startsWith "-" &&
    s.toList.all fun c => c != ',' && c != ':' && c != '$' && c.toNat ≥ 0x20 && c.toNat != 0x7f

/-- Parse a one-line TOML array of plain strings: `["a", "b"]`, optionally followed by a comment.
    Escapes are refused rather than interpreted. -/
partial def parseNativeStrArray (s : String) : Except String (List String) :=
  let ws (c : Char) := c == ' ' || c == '\t' || c == '\r'
  let rec finish (cs : List Char) (acc : List String) : Except String (List String) :=
    match cs.dropWhile ws with
    | [] => .ok acc
    | '#' :: _ => .ok acc
    | _ => .error "unexpected text after ']'"
  let rec elems (cs : List Char) (acc : List String) : Except String (List String) :=
    match cs.dropWhile ws with
    | ']' :: tail => finish tail acc
    | '"' :: tail =>
      let (str, rest) := tail.span (· != '"')
      match rest with
      | '"' :: rest' =>
        if str.contains '\\' then .error "escape sequences are not accepted in [native] strings"
        else
          match rest'.dropWhile ws with
          | ',' :: t => elems t (acc ++ [String.ofList str])
          | ']' :: t => finish t (acc ++ [String.ofList str])
          | _ => .error "expected ',' or ']' after a string"
      | _ => .error "unterminated string (arrays must be on one line)"
    | [] => .error "unterminated array (arrays must be on one line)"
    | _ => .error "expected a quoted string"
  match (s.trimAscii.toString).toList with
  | '[' :: rest => elems rest []
  | _ => .error "expected an array of strings, like [\"name\"]"

/-- A TOML key or table name with quoting and whitespace removed, so `"native"`, `'native'` and
    ` native ` are recognised as the same name. Used only to RECOGNISE spellings of `native`, never
    to accept them: any spelling but the plain one is refused. -/
private def normTomlName (s : String) : String :=
  String.ofList (s.toList.filter fun c => c != '"' && c != '\'' && c != ' ' && c != '\t')

/-- The `[native]` declarations of one manifest, in order, or the reason they are refused.

    Only the plain spelling is read: one `[native]` header, then `key = ["...", ...]` lines. Every
    other TOML spelling that would name the same table — a quoted header (`["native"]`), a subtable
    or array table (`[native.x]`, `[[native]]`), a top-level `native = {...}` inline table or
    dotted key (`native.libs = [...]`) — is REFUSED rather than ignored, as are a second `[native]`
    section and a repeated key: this line reader is not a full TOML parser, so anything it cannot
    read exactly must stop the build instead of silently declaring nothing. For the same reason a
    table header or top-level key written with a backslash escape (`["\u006eative"]`) is refused:
    escapes are not decoded, so such a name cannot be shown not to be `native`.

    Lines inside a multi-line string (`"""` or `'''`) are text, not structure, and are skipped, so
    a description that mentions `[native]` declares nothing and refuses nothing. -/
private partial def tomlScanBasic (cs : List Char) : Except String (List Char) :=
  match cs with
  | '\\' :: _ :: r => tomlScanBasic r
  | '"' :: r => .ok r
  | _ :: r => tomlScanBasic r
  | [] => .error "unterminated string"

private partial def tomlScanLiteral (cs : List Char) : Except String (List Char) :=
  match cs with
  | '\'' :: r => .ok r
  | _ :: r => tomlScanLiteral r
  | [] => .error "unterminated string"

/-- The scan state at the end of one line, given the state at its start: `(str, depth)` where `str`
    is 0 outside any string, 1 inside a multi-line basic string, 2 inside a multi-line literal
    string, and `depth` is the bracket/brace nesting of the VALUE being read (a multi-line array
    or inline table). Quote-, escape- and comment-aware, so a delimiter or bracket inside a plain
    string or a comment changes nothing. A multi-line string may end with up to two extra quotes
    (`""""`, `"""""`), which belong to its content. Brackets of a table header balance on its line. -/
private partial def tomlScanLine (st : Nat × Nat) (cs : List Char) : Except String (Nat × Nat) :=
  let (str, d) := st
  match str, cs with
  | 1, '\\' :: _ :: r => tomlScanLine (1, d) r
  | 1, '"' :: '"' :: '"' :: '"' :: '"' :: r => tomlScanLine (0, d) r
  | 1, '"' :: '"' :: '"' :: '"' :: r => tomlScanLine (0, d) r
  | 1, '"' :: '"' :: '"' :: r => tomlScanLine (0, d) r
  | 1, _ :: r => tomlScanLine (1, d) r
  | 2, '\'' :: '\'' :: '\'' :: '\'' :: '\'' :: r => tomlScanLine (0, d) r
  | 2, '\'' :: '\'' :: '\'' :: '\'' :: r => tomlScanLine (0, d) r
  | 2, '\'' :: '\'' :: '\'' :: r => tomlScanLine (0, d) r
  | 2, _ :: r => tomlScanLine (2, d) r
  | 0, '#' :: _ => .ok (0, d)
  | 0, '"' :: '"' :: '"' :: r => tomlScanLine (1, d) r
  | 0, '\'' :: '\'' :: '\'' :: r => tomlScanLine (2, d) r
  | 0, '"' :: r => do tomlScanLine (0, d) (← tomlScanBasic r)
  | 0, '\'' :: r => do tomlScanLine (0, d) (← tomlScanLiteral r)
  | 0, '[' :: r => tomlScanLine (0, d + 1) r
  | 0, '{' :: r => tomlScanLine (0, d + 1) r
  | 0, ']' :: r => if d == 0 then .error "unbalanced ']'" else tomlScanLine (0, d - 1) r
  | 0, '}' :: r => if d == 0 then .error "unbalanced '}'" else tomlScanLine (0, d - 1) r
  | 0, _ :: r => tomlScanLine (0, d) r
  | _, _ => .ok (str, d)

def parseNativeSection (content : String) : Except String (List (NativeKind × String)) := do
  let mut inNative := false
  let mut seenNative := false
  let mut atTop := true
  let mut seenKeys : List String := []
  let mut out : List (NativeKind × String) := []
  let mut strState : Nat × Nat := (0, 0)  -- tomlScanLine's state at the start of the current line
  for l in content.splitOn "\n" do
    let startState := strState
    strState ← match tomlScanLine startState l.toList with
      | .ok s => pure s
      | .error e => throw s!"{e} in line '{l.trimAscii}'"
    -- A line that BEGINS inside a multi-line string, array or inline table is part of a value,
    -- not structure: it is neither a table header nor a key.
    if startState != (0, 0) then continue
    let t := l.trimAscii.toString
    if t.startsWith "[" then
      atTop := false
      let header := String.ofList ((t.toList.dropWhile (· == '[')).takeWhile (· != ']'))
      if header.contains '\\' then
        throw s!"table header '{t}' uses a backslash escape, which Concrete.toml does not decode"
      let name := normTomlName header
      if name == "native" || name.startsWith "native." then
        let afterHeader := (t.drop "[native]".length).trimAscii.toString
        if t.startsWith "[native]" && (afterHeader.isEmpty || afterHeader.startsWith "#") then
          if seenNative then throw "a second [native] section (declare everything in one)"
          inNative := true
          seenNative := true
        else
          throw s!"unsupported table '{t}': native libraries are declared in one plain [native] section"
      else
        inNative := false
    else if !t.isEmpty && !t.startsWith "#" then
      let rawKey := ((t.splitOn "=").headD "").trimAscii.toString
      if atTop then
        if rawKey.contains '\\' then
          throw s!"top-level key '{rawKey}' uses a backslash escape, which Concrete.toml does not decode"
        let k := normTomlName rawKey
        if k == "native" || k.startsWith "native." then
          throw s!"unsupported top-level key '{rawKey}': native libraries are declared in a plain [native] section"
      else if inNative then
        let valParts := (t.splitOn "=").drop 1
        let key := normTomlName rawKey
        let kind ← match key with
          | "pkg-config" => pure NativeKind.pkgConfig
          | "libs" => pure NativeKind.lib
          | "lib-dirs" => pure NativeKind.libDir
          | _ => throw s!"unknown key '{rawKey}' (accepted: pkg-config, libs, lib-dirs)"
        if seenKeys.contains key then throw s!"key '{key}' is given twice"
        seenKeys := key :: seenKeys
        let vals ← match parseNativeStrArray ("=".intercalate valParts) with
          | .ok v => pure v
          | .error e => throw s!"'{key}': {e}"
        for v in vals do
          let valid := match kind with
            | .libDir => isValidNativeDir v
            | .lib => isLinkableLibName v
            | .pkgConfig => isValidNativeName v
          if !valid then
            throw (if kind == .libDir then
              s!"'{key}': '{v}' is not an accepted library directory (non-empty, not starting with '-', no ',', ':', '$' or control characters)"
            else if kind == .lib && isValidNativeName v then
              s!"'{key}': '{v}' is not a library name: -l{v} would be read as a linker option"
            else
              s!"'{key}': '{v}' is not a library name (accepted: letters, digits, '_', '.', '+', '-'; not starting with '-')")
          out := out ++ [(kind, v)]
  if strState.1 != 0 then throw "unterminated multi-line string"
  if strState.2 != 0 then throw "unterminated array or inline table"
  return out

/-- The section for the trust reports: every declared native library, by package. Empty when none
    is declared. Native code is linked into the binary and is not checked by Concrete, so each one
    is a trusted dependency of the program. -/
def nativeTrustSection (decls : List NativeLibDecl) : String :=
  if decls.isEmpty then ""
  else
    "\nNative libraries (trusted foreign dependencies: linked into the binary, not checked by Concrete):\n" ++
      "\n".intercalate (decls.map fun d => s!"  package '{d.package}': {d.describe}") ++ "\n"

/-- Canonical existing directory or a refusal. -/
private def canonicalNativeDir (d : NativeLibDecl) (path : String) (origin : String) : IO (Except String String) := do
  let canon ← try pure (some (← IO.FS.realPath ⟨path⟩).toString) catch _ => pure none
  match canon with
  | none => return .error s!"error: package '{d.package}' [native] {origin}: library directory '{path}' does not exist"
  | some c =>
    if !(← System.FilePath.isDir ⟨c⟩) then
      return .error s!"error: package '{d.package}' [native] {origin}: '{c}' is not a directory"
    if !isValidNativeDir c then
      return .error s!"error: package '{d.package}' [native] {origin}: library directory '{c}' contains a character the link line cannot carry (',', ':', '$' or a control character)"
    return .ok c

/-- The link flags for the declared native libraries, in declaration order (the root package's
    first, then each dependency's — a library before the libraries it uses), or a refusal. -/
def resolveNativeLinkFlags (decls : List NativeLibDecl) : IO (Except String (Array String)) := do
  let mut flags : Array String := #[]
  let mut pkgConfigChecked := false
  let addDir (fs : Array String) (dir : String) : Array String :=
    if fs.contains s!"-L{dir}" then fs else fs ++ #[s!"-L{dir}", s!"-Wl,-rpath,{dir}"]
  for d in decls do
    match d.kind with
    | .libDir =>
      let path := if d.value.startsWith "/" then d.value else d.packageRoot ++ "/" ++ d.value
      match ← canonicalNativeDir d path "lib-dirs" with
      | .error e => return .error e
      | .ok c => flags := addDir flags c
    | .lib => flags := flags.push s!"-l{d.value}"
    | .pkgConfig =>
      if !isValidNativeName d.value then
        return .error s!"error: package '{d.package}' [native] pkg-config: '{d.value}' is not a package name"
      if !pkgConfigChecked then
        let found ← try
            let r ← IO.Process.output { cmd := "pkg-config", args := #["--version"] }
            pure (r.exitCode == 0)
          catch _ => pure false
        if !found then
          return .error s!"error: package '{d.package}' declares [native] pkg-config = [\"{d.value}\"], but pkg-config was not found\nhint: install pkg-config (the repo devshell provides it: nix develop)"
        pkgConfigChecked := true
      let ex ← IO.Process.output { cmd := "pkg-config", args := #["--print-errors", "--exists", d.value] }
      if ex.exitCode != 0 then
        let why := ex.stderr.trimAscii.toString
        return .error s!"error: package '{d.package}' declares [native] pkg-config = [\"{d.value}\"], but pkg-config cannot find '{d.value}'{if why.isEmpty then "" else "\n" ++ why}\nhint: install the library with its {d.value}.pc, or add its directory to PKG_CONFIG_PATH"
      let r ← IO.Process.output { cmd := "pkg-config", args := #["--libs", d.value] }
      if r.exitCode != 0 then
        return .error s!"error: package '{d.package}': pkg-config --libs {d.value} failed: {r.stderr.trimAscii}"
      let toks := (r.stdout.split (fun c => c == ' ' || c == '\t' || c == '\n' || c == '\r')).toList.map (·.toString)
        |>.filter (!·.isEmpty)
      let refuse (tok : String) : Except String (Array String) :=
        .error s!"error: package '{d.package}': pkg-config --libs {d.value} emitted '{tok}', which is not an accepted link flag (accepted: -L<dir>, -l<name>, -pthread, -framework <name>); refusing rather than forwarding it"
      let mut rest := toks
      while !rest.isEmpty do
        match rest with
        | [] => pure ()
        | tok :: more =>
          rest := more
          if tok.startsWith "-L" then
            match ← canonicalNativeDir d (tok.drop 2).toString s!"pkg-config '{d.value}'" with
            | .error e => return .error e
            | .ok c => flags := addDir flags c
          else if tok.startsWith "-l" && isLinkableLibName (tok.drop 2).toString then
            flags := flags.push tok
          else if tok == "-pthread" then
            flags := flags.push tok
          else if tok == "-framework" then
            match more with
            | fw :: more' =>
              if isValidNativeName fw then
                flags := flags ++ #["-framework", fw]; rest := more'
              else return refuse s!"-framework {fw}"
            | [] => return refuse tok
          else return refuse tok
  return .ok flags

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

/-- The native declarations a SINGLE-FILE compile (`concrete <file> -o out`, `--test`) links: those
    of the package enclosing the file, if any. File mode loads no dependency package, so no
    dependency's code is linked and no dependency's library is needed; but the enclosing package's
    own `[native]` is, and it is refused here exactly as `loadProject` refuses it, so the same
    manifest cannot link in one mode and be ignored in the other. -/
def fileModeNativeDecls (inputPath : String) : IO (Except String (List NativeLibDecl)) := do
  match ← findProjectRoot (dirOf inputPath) with
  | none => return .ok []
  | some root =>
    let tomlPath := root ++ "/Concrete.toml"
    let toml ← try pure (Except.ok (← readFile tomlPath)) catch e => pure (Except.error (toString e))
    match toml with
    | .error e => return .error s!"error: cannot read {tomlPath}: {e}"
    | .ok toml =>
    match parseNativeSection toml with
    | .error e => return .error s!"error: {tomlPath} [native]: {e}\nhint: see docs/platform/NATIVE_LIBRARIES.md"
    | .ok v =>
      let name := (Proof.packageField toml "name").getD "the project"
      return .ok (v.map fun (kind, value) => { package := name, packageRoot := root, kind, value })

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
  /-- The resolved identity (R-0482), computed ONCE here and threaded, not re-derived per consumer:
      the project's scope, its own artifact, its resolution context, and every top-level module's
      owning scope. `Except` rather than a defaulted value: a project with no declared name gets a
      typed refusal and therefore no scoped evidence, because a default would make every unnamed
      project the same package. -/
  identity : Except Proof.IdentityRefusal Proof.ResolvedIdentity
  policy        : Concrete.ProjectPolicy  -- the [policy] release profile, parsed once
  policyWarnings : List String            -- structural warnings from parsing [policy]
  -- proof/diagnostic facts derived once (Phase 4 #1): the source-location map, the
  -- synthesized proof registry, and the ProofCore — so build / test / report /
  -- policy stop re-deriving them per command.
  policyLocMap  : Report.FnLocMap
  registry      : Concrete.ProofRegistry
  pc            : Concrete.ProofCore
  /-- Which package each source file belongs to: (file, package key, package name). The key is
      the package's `PackageScopeIdentity.digest` (R-0482) — from its OWN manifest, or the
      `standalone` scope over its own paired inventory when it has none. Never `""`, and
      never a bare name label, which two manifestless packages could share. Module
      names cannot scope a declaration: two packages may each define a module `util` (R-0484 R10
      assumption identity). -/
  filePackages  : List (String × String × String) := []
  /-- Every `[native]` declaration in the resolved graph, root package first, then each dependency
      in declaration order. Linked into the final binary by `resolveNativeLinkFlags`, and listed by
      the trust reports: native code is a trusted dependency. -/
  nativeLibs    : List NativeLibDecl := []
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
  -- The root package's [native] declarations (G1). Refused, never skipped, when malformed.
  let rootNative ← match parseNativeSection tomlContent with
    | .ok v => pure v
    | .error e =>
      IO.eprintln s!"error: Concrete.toml [native]: {e}\nhint: see docs/platform/NATIVE_LIBRARIES.md"
      return Except.error 1

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

  -- One binding per name. A duplicate used to be appended silently, so which path a name meant
  -- depended on list order (R-0482).
  let depBindings := deps.map (·.1)
  if let some dupName := depBindings.find? (fun n => (depBindings.filter (· == n)).length > 1) then
    IO.eprintln s!"error: {(Proof.IdentityRefusal.duplicateBinding dupName).explain}\nhint: each [dependencies] name must be bound once"
    return Except.error 1
  -- The bundled std, canonically: only the std the resolver found beside the compiler receives the
  -- reserved toolchain scope (D4). A user package or dependency NAMED `std` does not.
  let bundledStd? ← do
    match ← findBuiltinStd with
    | some p => try pure (some (← IO.FS.realPath ⟨p⟩).toString) catch _ => pure (some p)
    | none => pure none
  let isBundledStd : String → IO Bool := fun path => do
    let canon ← try pure (← IO.FS.realPath ⟨path⟩).toString catch _ => pure path
    pure (bundledStd? == some canon)
  -- Load all dependencies
  let mut depModules : List Module := []
  let mut depIdentities : List (String × Proof.PackageScopeIdentity × Proof.PackageArtifactIdentity) := []
  let mut depSrcMap : SourceMap := []
  let mut filePackages : List (String × String × String) := []
  let mut moduleOwners : List (String × String × String) := []  -- (module, package, file)
  let mut depNative : List NativeLibDecl := []
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
      -- ABSENT is not UNREADABLE. A dependency with no manifest is a legitimate standalone package;
      -- a manifest that exists and cannot be read used to be treated the same way, silently
      -- dropping its [native] declarations and its identity. It is refused instead.
      let depTomlPath := resolvedPath ++ "/Concrete.toml"
      let depTomlRead : Except String (Option String) ←
        if !(← System.FilePath.pathExists ⟨depTomlPath⟩) then pure (.ok none)
        else try pure (.ok (some (← readFile depTomlPath))) catch e => pure (.error (toString e))
      let depToml ← match depTomlRead with
        | .ok t => pure t
        | .error e =>
          IO.eprintln s!"error: dependency '{depName}': cannot read {depTomlPath}: {e}"
          return Except.error 1
      -- The dependency's [native] declarations propagate to this build's link (G1).
      if let some t := depToml then
        match parseNativeSection t with
        | .ok v => depNative := depNative ++ v.map fun (kind, value) =>
            { package := depName, packageRoot := resolvedPath, kind, value }
        | .error e =>
          IO.eprintln s!"error: dependency '{depName}': Concrete.toml [native]: {e}\nhint: see docs/platform/NATIVE_LIBRARIES.md"
          return Except.error 1
      -- R-0482: the dependency's OWN artifact (its paired inventory, never its dependencies) and its
      -- scope — the bundled std's reserved toolchain scope, its manifest's name and origin, or the
      -- `standalone` scope over its own artifact when it has no manifest. An unidentifiable
      -- dependency REFUSES the load: the old `unidentified:<name>` key let two such dependencies
      -- that share a name merge into one package.
      let bundled ← isBundledStd resolvedPath
      let ident : Except Proof.IdentityRefusal (Proof.PackageScopeIdentity × Proof.PackageArtifactIdentity) := do
        let paired ← Proof.pairSources (resolvedPath ++ "/src/lib.con") srcMap
        let art ← Proof.PackageArtifactIdentity.of? paired
        let scope ← if bundled && depName == "std" then Proof.PackageScopeIdentity.toolchainStd "resolver:bundled-std"
          else match depToml with
            | none => pure (Proof.PackageScopeIdentity.standalone art)
            | some t =>
              match Proof.packageField t "name" with
              | none => throw (.emptyComponent s!"[package] name of dependency '{depName}'")
              | some n => Proof.PackageScopeIdentity.ofManifest n (Proof.packageFieldRaw t "origin")
        pure (scope, art)
      match ident with
      | .error w =>
        IO.eprintln s!"error: dependency '{depName}' has no identity: {w.explain}"
        return Except.error 1
      | .ok (scope, art) =>
      depIdentities := depIdentities ++ [(depName, scope, art)]
      filePackages := filePackages ++ srcMap.map fun (f, _) => (f, scope.digest, depName)
      moduleOwners := moduleOwners ++ modules.map fun m => (m.name, depName, m.sourceFile)
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
    -- BUG 074: a top-level module name defined by two packages is REFUSED here, before anything
    -- downstream sees the merged program. Both definitions used to pass every frontend check and
    -- reach code generation as one module, where LLVM validation failed on a duplicate symbol
    -- ("invalid redefinition of function") with no compiler diagnostic — and report modes, which
    -- generate no code, silently merged the two packages' declarations. Packages cannot yet
    -- share a module path, so the collision is an error naming both owners.
    let owners := moduleOwners ++ resolvedParsed.modules.map fun m =>
      (m.name, (if selfName.isEmpty then "the project" else selfName), m.sourceFile)
    let dup := owners.findSome? fun (n, pkg, file) =>
      (owners.find? fun (n2, pkg2, file2) => n2 == n && (pkg2 != pkg || file2 != file)).map
        fun (_, pkg2, file2) => (n, pkg, file, pkg2, file2)
    if let some (n, pkg, file, pkg2, file2) := dup then
      IO.eprintln s!"error: module '{n}' is defined by two packages: '{pkg}' ({file}) and '{pkg2}' ({file2})\nhint: packages cannot yet share a top-level module name; rename one of the modules (bug 074)"
      return Except.error 1
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
    -- THE PROJECT'S RESOLVED IDENTITY (R-0482), computed HERE, above `pc`, because
    -- `extractProofCore` requires it. Three identities, not one over-bound digest:
    --   scope     — the manifest's name and origin (never version, dependencies or content);
    --   artifact  — the project's OWN paired modules (never dependencies or std);
    --   context   — each resolved dependency's (binding, scope, artifact), std included.
    -- The old single digest hashed every source in the build, std included, so an unused std edit
    -- renamed every manifestful package's generated symbols.
    let rootBundled ← isBundledStd projectRoot
    let identity : Except Proof.IdentityRefusal Proof.ResolvedIdentity := do
      let paired ← Proof.pairSources mainPath ([(mainPath, source)] ++ subSrcMap)
      let art ← Proof.PackageArtifactIdentity.of? paired
      let scope ← if rootBundled && selfName == "std" then Proof.PackageScopeIdentity.toolchainStd "resolver:bundled-std"
        else Proof.PackageScopeIdentity.ofManifest selfName (Proof.packageFieldRaw tomlContent "origin")
      -- D1 at the resolver: two distinct `undeclared` scopes of one name in one resolved graph.
      Proof.refuseAmbiguousUndeclared ((scope, art) :: depIdentities.map (·.2))
      let ctx ← Proof.ResolutionContextIdentity.of? depIdentities
      let owners := moduleOwners.filterMap fun (m, depName, _) =>
        (depIdentities.find? (·.1 == depName)).map fun (_, sc, _) => (m, sc)
      pure { scope, artifact := art, context := ctx, owners }
    -- A refusal other than the root's missing name (D1, inventory, binding) refuses the load here;
    -- a missing name keeps its established path below (no ProofCore, with the manifest hint).
    if let .error w := identity then
      if w != .emptyComponent "name" then
        IO.eprintln s!"error: {w.explain}"
        return Except.error 1
    let rootKey := match identity with
      | .ok r => r.scope.digest
      | .error _ => s!"unidentified:{selfName}"
    let rootFiles := [mainPath] ++ subSrcMap.map (·.1)
    let allFilePackages := filePackages ++ rootFiles.map fun f => (f, rootKey, selfName)
    -- Proof ADMISSION reads the same summary the authority reports do (R-0484 R10).
    let assumptions := Assumptions.forProgram
      (validCore.coreModules.filter fun m => !depNames.contains m.name)
      (validCore.coreModules.filter fun m => depNames.contains m.name)
      (Assumptions.packageOfFiles allFilePackages) (rootKey, selfName)
    let pcE := extractProofCore? validCore identity assumptions simpleLocMap registry
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
    let rootLabel := if selfName.isEmpty then "the project" else selfName
    let nativeLibs : List NativeLibDecl :=
      rootNative.map (fun (kind, value) => { package := rootLabel, packageRoot := projectRoot, kind, value })
        ++ depNative
    for d in nativeLibs do
      ledger := ledger.recordFact "native-library" d.package s!"{d.kind.key}:{d.value} (trusted foreign dependency)"
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
                       mainPath, depNames, identity, policy, policyWarnings, policyLocMap,
                       registry, pc, ledger, filePackages := allFilePackages, nativeLibs }

end Concrete
