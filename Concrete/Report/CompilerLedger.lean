/-!
# CompilerLedger — the non-proof compiler fact store (Phase 4 #2)

The ordinary compiler pipeline's analogue of `ObligationCore`: one typed store for
every NON-proof fact a command derives about a project — modules, imports,
diagnostics, source maps, pass artifacts and their provenance, timings,
target/backend assumptions, emitted files, and cache/dependency facts — plus a
LINK to the `ObligationCore` ledger so the two halves compose into one project
picture. Commands record into it through the `recordX` API and render it instead
of recomputing; `loadProject` builds it once so every project-mode command reads
the same facts. See ROADMAP Phase 4 #2 and `docs/COMPILER_PIPELINE.md`.

This module is deliberately dependency-light (plain data + a manual JSON view):
the RECORDING happens in the pipeline/commands that already hold the facts, so the
store itself needs no compiler internals.
-/

namespace Concrete.CompilerLedger

/-- A typed non-proof compiler fact. `category` mirrors the pipeline surface:
    `module` | `import` | `name` | `type` | `ownership` | `capability` |
    `target` | `backend` | `cache` | `link`. -/
structure Fact where
  category : String
  key      : String
  value    : String
  deriving Inhabited

/-- A named pass artifact: a pipeline stage's output with its provenance, so the
    pipeline is a replayable fact chain (input ids → output ids). -/
structure Artifact where
  id        : String
  pass      : String
  inputIds  : List String := []
  outputIds : List String := []
  summary   : String := ""        -- a deterministic one-line summary of the output
  replay    : String := ""
  deriving Inhabited

/-- A diagnostic, in the one shared shape (code/severity/message/loc). -/
structure Diag where
  code     : String
  severity : String
  message  : String
  file     : String := ""
  line     : Nat := 0
  deriving Inhabited

/-- A per-pass timing (milliseconds). 0 until a clock measures it — `Date.now` is
    unavailable in this toolchain, so timings are recorded by the caller. -/
structure Timing where
  pass   : String
  millis : Nat := 0
  deriving Inhabited

/-- A source-map fact: which modules a source file contributes. -/
structure SourceFile where
  file    : String
  modules : List String := []
  deriving Inhabited

/-- The non-proof compiler fact store. The proof/obligation half lives in
    `ObligationCore`; `obligationLink` references it so the two ledgers compose. -/
structure CompilerLedger where
  artifacts      : List Artifact := []
  diagnostics    : List Diag := []
  facts          : List Fact := []
  dependencies   : List (String × String) := []   -- (name, resolved path)
  timings        : List Timing := []
  sourceFiles    : List SourceFile := []
  emittedFiles   : List String := []
  replayCommands : List (String × String) := []    -- (artifact id, command)
  toolchainId    : String := ""
  obligationLink : String := ""                     -- how to obtain the ObligationCore ledger
  deriving Inhabited

namespace CompilerLedger

/-- The empty ledger. -/
def empty : CompilerLedger := {}

-- ── the recordX API (pure; each returns the updated ledger) ──────────────────
def recordArtifact   (l : CompilerLedger) (a : Artifact) : CompilerLedger := { l with artifacts := l.artifacts ++ [a] }
def recordDiagnostic (l : CompilerLedger) (d : Diag) : CompilerLedger := { l with diagnostics := l.diagnostics ++ [d] }
def recordFact       (l : CompilerLedger) (category key value : String) : CompilerLedger :=
  { l with facts := l.facts ++ [{ category, key, value }] }
def recordDependency (l : CompilerLedger) (name path : String) : CompilerLedger := { l with dependencies := l.dependencies ++ [(name, path)] }
def recordTiming     (l : CompilerLedger) (pass : String) (millis : Nat) : CompilerLedger := { l with timings := l.timings ++ [{ pass, millis }] }
def recordSourceMap  (l : CompilerLedger) (file : String) (modules : List String) : CompilerLedger :=
  { l with sourceFiles := l.sourceFiles ++ [{ file, modules }] }
def recordEmitted    (l : CompilerLedger) (file : String) : CompilerLedger := { l with emittedFiles := l.emittedFiles ++ [file] }
def recordReplayCommand (l : CompilerLedger) (id cmd : String) : CompilerLedger := { l with replayCommands := l.replayCommands ++ [(id, cmd)] }

/-- Link this fact store to the ObligationCore (proof) ledger. -/
def linkObligations (l : CompilerLedger) (reference : String) : CompilerLedger := { l with obligationLink := reference }

-- ── views ────────────────────────────────────────────────────────────────────
private def esc (s : String) : String :=
  s.foldl (fun a c => a ++ (match c with
    | '"' => "\\\"" | '\\' => "\\\\" | '\n' => "\\n" | '\t' => "\\t" | c => c.toString)) ""
private def q (s : String) : String := "\"" ++ esc s ++ "\""
private def arr (xs : List String) : String := "[" ++ ", ".intercalate (xs.map q) ++ "]"
private def pairArr (xs : List (String × String)) : String :=
  "[" ++ ", ".intercalate (xs.map (fun (a, b) => "{" ++ q "name" ++ ": " ++ q a ++ ", " ++ q "value" ++ ": " ++ q b ++ "}")) ++ "]"

/-- Versioned JSON envelope of the compiler fact store. -/
def toJson (l : CompilerLedger) (schemaVer : Nat) : String :=
  let factObjs := l.facts.map fun f => "{" ++ q "category" ++ ": " ++ q f.category ++ ", "
    ++ q "key" ++ ": " ++ q f.key ++ ", " ++ q "value" ++ ": " ++ q f.value ++ "}"
  let diagObjs := l.diagnostics.map fun d => "{" ++ q "code" ++ ": " ++ q d.code ++ ", "
    ++ q "severity" ++ ": " ++ q d.severity ++ ", " ++ q "message" ++ ": " ++ q d.message ++ "}"
  let srcObjs := l.sourceFiles.map fun s => "{" ++ q "file" ++ ": " ++ q s.file ++ ", "
    ++ q "modules" ++ ": " ++ arr s.modules ++ "}"
  let artObjs := l.artifacts.map fun a => "{" ++ q "id" ++ ": " ++ q a.id ++ ", "
    ++ q "pass" ++ ": " ++ q a.pass ++ ", " ++ q "input_ids" ++ ": " ++ arr a.inputIds ++ ", "
    ++ q "output_ids" ++ ": " ++ arr a.outputIds ++ ", " ++ q "summary" ++ ": " ++ q a.summary
    ++ ", " ++ q "replay" ++ ": " ++ q a.replay ++ "}"
  -- timings carry runtime-variable values; consumers that compare ledgers for
  -- determinism normalize `millis` out (see check_compiler_ledger.sh).
  let timeObjs := l.timings.map fun t => "{" ++ q "pass" ++ ": " ++ q t.pass ++ ", "
    ++ q "millis" ++ ": " ++ toString t.millis ++ "}"
  String.join [
    "{", s!"{q "schema_version"}: {schemaVer}, ",
    s!"{q "schema_kind"}: {q "compiler_ledger"}, ",
    s!"{q "toolchain"}: {q l.toolchainId}, ",
    s!"{q "obligation_link"}: {q l.obligationLink}, ",
    s!"{q "artifacts"}: [", ", ".intercalate artObjs, "], ",
    s!"{q "timings"}: [", ", ".intercalate timeObjs, "], ",
    s!"{q "facts"}: [", ", ".intercalate factObjs, "], ",
    s!"{q "diagnostics"}: [", ", ".intercalate diagObjs, "], ",
    s!"{q "dependencies"}: {pairArr l.dependencies}, ",
    s!"{q "source_files"}: [", ", ".intercalate srcObjs, "], ",
    s!"{q "emitted_files"}: {arr l.emittedFiles}, ",
    s!"{q "counts"}: \{{q "artifacts"}: {l.artifacts.length}, {q "facts"}: {l.facts.length}, {q "diagnostics"}: {l.diagnostics.length}, "
      ++ s!"{q "dependencies"}: {l.dependencies.length}, {q "source_files"}: {l.sourceFiles.length}}",
    "}" ]

/-- Human-readable fact store. -/
def render (l : CompilerLedger) : String := Id.run do
  let mut out := "=== Compiler Ledger (project facts) ==="
  out := out ++ s!"\n\ntoolchain:  {l.toolchainId}"
  out := out ++ s!"\nobligations: {l.obligationLink}"
  if !l.artifacts.isEmpty then
    out := out ++ "\n\npass artifacts (input → output):"
    for a in l.artifacts do
      let sm := if a.summary.isEmpty then "" else s!"  [{a.summary}]"
      out := out ++ s!"\n  {a.pass}:  {", ".intercalate a.inputIds} → {", ".intercalate a.outputIds}{sm}"
  if !l.timings.isEmpty then
    out := out ++ "\n\ntimings:"
    for t in l.timings do out := out ++ s!"\n  {t.pass}:  {t.millis} ms"
  if !l.dependencies.isEmpty then
    out := out ++ "\n\ndependencies:"
    for (n, p) in l.dependencies do out := out ++ s!"\n  {n}  ({p})"
  if !l.sourceFiles.isEmpty then
    out := out ++ "\n\nsource files:"
    for s in l.sourceFiles do out := out ++ s!"\n  {s.file}  →  {", ".intercalate s.modules}"
  if !l.facts.isEmpty then
    out := out ++ "\n\nfacts:"
    for f in l.facts do out := out ++ s!"\n  [{f.category}] {f.key} = {f.value}"
  out := out ++ s!"\n\nTotal: {l.facts.length} facts, {l.diagnostics.length} diagnostics, "
    ++ s!"{l.dependencies.length} dependencies, {l.sourceFiles.length} source files"
  return out

end CompilerLedger
end Concrete.CompilerLedger
