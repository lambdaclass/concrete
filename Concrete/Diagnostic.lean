import Concrete.Token

namespace Concrete

/-! ## Diagnostic — structured compiler diagnostics

Provides a uniform error/warning reporting infrastructure across all compiler passes.
-/

-- ============================================================
-- Diagnostic types
-- ============================================================

inductive Severity where
  | error
  | warning
  | note
  deriving BEq

structure Diagnostic where
  severity : Severity
  message  : String
  pass     : String         -- "check", "elab", "ssa-verify", etc.
  span     : Option Span    -- primary span (line/col from Token.lean)
  hint     : Option String  -- suggested fix (the "help" section)
  code     : String := ""   -- stable error code (e.g. "E0201"), empty if unclassified
  file     : String := ""   -- source file path (empty = unknown)
  context  : List String := []  -- context chain, outermost first
  -- rich diagnostic surface (Phase 4 #11). All default empty, so a diagnostic that
  -- does not set them renders exactly as before; producers opt in. Human and JSON
  -- both render from these same fields, so the two outputs cannot drift.
  related   : List (Span × String) := []  -- secondary/related spans, each with a note
  reason    : Option String := none        -- WHY this is an error
  nextAction : Option String := none        -- the concrete next step
  expected  : Option String := none        -- expected fact (for expected-vs-actual)
  actual    : Option String := none         -- actual fact
  evidence  : List (String × String) := []  -- policy / evidence context (key → value)

abbrev Diagnostics := List Diagnostic

/-- Source map: list of (file path, source text) pairs. -/
abbrev SourceMap := List (String × String)

-- ============================================================
-- Rendering
-- ============================================================

private def severityStr : Severity → String
  | .error   => "error"
  | .warning => "warning"
  | .note    => "note"

/-- Extract a 1-indexed line from source text. Returns "" if out of bounds. -/
private def getSourceLine (source : String) (lineNum : Nat) : String :=
  let lines := source.splitOn "\n"
  if lineNum == 0 then ""
  else match lines[lineNum - 1]? with
    | some l => l
    | none => ""

/-- Build the source-line snippet with caret underline. -/
private def renderSnippet (source : String) (sp : Span) : String :=
  let line := getSourceLine source sp.line
  if line.isEmpty then ""
  else
    let lineNumStr := toString sp.line
    let pad := lineNumStr.length
    let gutter := String.ofList (List.replicate pad ' ')
    -- Caret line: spaces up to col, then ^ characters
    let caretStart := if sp.col > 0 then sp.col - 1 else 0
    let caretLen := if sp.endCol > sp.col then sp.endCol - sp.col else 1
    let spaces := String.ofList (List.replicate caretStart ' ')
    let carets := String.ofList (List.replicate caretLen '^')
    s!"\n {lineNumStr} | {line}\n {gutter} | {spaces}{carets}"

def Diagnostic.render (d : Diagnostic) (sourceMap : SourceMap := []) : String :=
  -- If file is unset but source map has entries, fall back to the first entry
  let effectiveFile := if d.file.isEmpty then
      match sourceMap with
      | (path, _) :: _ => path
      | [] => ""
    else d.file
  let filePrefix := if effectiveFile.isEmpty then "" else s!"{effectiveFile}:"
  let locStr := match d.span with
    | some sp =>
      if sp.endLine > 0 then s!"{filePrefix}{sp.line}:{sp.col}-{sp.endLine}:{sp.endCol}: "
      else s!"{filePrefix}{sp.line}:{sp.col}: "
    | none =>
      if effectiveFile.isEmpty then "" else s!"{filePrefix} "
  let hintStr := match d.hint with
    | some h => s!"\n  hint: {h}"
    | none => ""
  let source := if d.file.isEmpty then
      match sourceMap with
      | (_, src) :: _ => some src
      | [] => none
    else sourceMap.lookup d.file
  let snippetStr := match d.span, source with
    | some sp, some src => renderSnippet src sp
    | _, _ => ""
  let ctxStr := if d.context.isEmpty then ""
    else "\n" ++ ("\n".intercalate (d.context.map fun c => s!"  = {c}"))
  let codeStr := if d.code.isEmpty then "" else s!"({d.code}) "
  -- rich sections (Phase 4 #11): each rendered only when present.
  let srcFor : Option String := if d.file.isEmpty then (match sourceMap with | (_, s) :: _ => some s | [] => none) else sourceMap.lookup d.file
  let relatedStr := String.join (d.related.map fun (sp, note) =>
    let snip := match srcFor with | some src => renderSnippet src sp | none => ""
    s!"\n  related ({sp.line}:{sp.col}): {note}{snip}")
  let reasonStr := match d.reason with | some r => s!"\n  reason: {r}" | none => ""
  let exp := match d.expected, d.actual with
    | some e, some a => s!"\n  expected: {e}\n  found:    {a}"
    | some e, none   => s!"\n  expected: {e}"
    | none,   some a => s!"\n  found:    {a}"
    | none,   none   => ""
  let nextStr := match d.nextAction with | some n => s!"\n  next: {n}" | none => ""
  let evStr := if d.evidence.isEmpty then ""
    else "\n" ++ ("\n".intercalate (d.evidence.map fun (k, v) => s!"  {k}: {v}"))
  s!"{locStr}{severityStr d.severity}[{d.pass}]: {codeStr}{d.message}{snippetStr}{relatedStr}{reasonStr}{exp}{hintStr}{nextStr}{evStr}{ctxStr}"

def renderDiagnostics (ds : Diagnostics) (sourceMap : SourceMap := []) : String :=
  "\n".intercalate (ds.map fun d => d.render sourceMap)

-- ============================================================
-- JSON rendering (Phase 4 #4 / #11): human and machine output from the SAME
-- structured record, so they cannot drift. `Diagnostic.render` (above) and
-- `Diagnostic.toJson` (below) read the same fields.
-- ============================================================

private def jesc (s : String) : String :=
  s.foldl (fun a c => a ++ (match c with
    | '"' => "\\\"" | '\\' => "\\\\" | '\n' => "\\n" | '\t' => "\\t" | c => c.toString)) ""
private def jq (s : String) : String := "\"" ++ jesc s ++ "\""

/-- A diagnostic as a JSON object: the SAME code / severity / message / pass /
    file / span / hint / context the human renderer uses (Phase 4 #4). -/
def Diagnostic.toJson (d : Diagnostic) : String :=
  let spanJson := match d.span with
    | some sp => s!"\{{jq "line"}: {sp.line}, {jq "col"}: {sp.col}, {jq "end_line"}: {sp.endLine}, {jq "end_col"}: {sp.endCol}}"
    | none => "null"
  let hintJson := match d.hint with | some h => jq h | none => "null"
  let ctxJson := "[" ++ ", ".intercalate (d.context.map jq) ++ "]"
  let optJson := fun (o : Option String) => match o with | some x => jq x | none => "null"
  let spanObj := fun (sp : Span) => s!"\{{jq "line"}: {sp.line}, {jq "col"}: {sp.col}, {jq "end_line"}: {sp.endLine}, {jq "end_col"}: {sp.endCol}}"
  let relJson := "[" ++ ", ".intercalate (d.related.map fun (sp, note) =>
    "{" ++ jq "span" ++ ": " ++ spanObj sp ++ ", " ++ jq "note" ++ ": " ++ jq note ++ "}") ++ "]"
  let evJson := "[" ++ ", ".intercalate (d.evidence.map fun (k, v) =>
    "{" ++ jq "key" ++ ": " ++ jq k ++ ", " ++ jq "value" ++ ": " ++ jq v ++ "}") ++ "]"
  String.join [
    "{", s!"{jq "severity"}: {jq (severityStr d.severity)}, ",
    s!"{jq "code"}: {jq d.code}, ", s!"{jq "pass"}: {jq d.pass}, ",
    s!"{jq "message"}: {jq d.message}, ", s!"{jq "file"}: {jq d.file}, ",
    s!"{jq "span"}: {spanJson}, ", s!"{jq "hint"}: {hintJson}, ",
    s!"{jq "related"}: {relJson}, ", s!"{jq "reason"}: {optJson d.reason}, ",
    s!"{jq "next_action"}: {optJson d.nextAction}, ",
    s!"{jq "expected"}: {optJson d.expected}, ", s!"{jq "actual"}: {optJson d.actual}, ",
    s!"{jq "evidence"}: {evJson}, ",
    s!"{jq "context"}: {ctxJson}", "}" ]

/-- A versioned JSON envelope of a diagnostics list. `partial` is true when the
    diagnostics may be incomplete because a pass was skipped (Phase 4 #12a) — the
    consumer must treat the set as best-effort, never as a complete verdict. -/
def diagnosticsToJson (ds : Diagnostics) (schemaVer : Nat := 1) (isPartial : Bool := false) : String :=
  String.join [
    "{", s!"{jq "schema_version"}: {schemaVer}, ",
    s!"{jq "schema_kind"}: {jq "diagnostics"}, ",
    s!"{jq "partial"}: {if isPartial then "true" else "false"}, ",
    s!"{jq "count"}: {ds.length}, ",
    s!"{jq "diagnostics"}: [", ", ".intercalate (ds.map Diagnostic.toJson), "]}" ]

-- field-parity: the JSON carries the same code / message / severity the human
-- render shows (kernel-checked at build time).
private def fwDiag : Diagnostic :=
  { severity := .error, message := "boom", pass := "parse",
    span := some ⟨3, 5, 3, 9⟩, hint := some "fix it", code := "E0001", file := "a.con" }
example : (fwDiag.toJson.splitOn "\"E0001\"").length = 2
    ∧ (fwDiag.toJson.splitOn "\"boom\"").length = 2
    ∧ (fwDiag.render.splitOn "E0001").length ≥ 2 := by native_decide

-- ============================================================
-- Queries
-- ============================================================

def hasErrors (ds : Diagnostics) : Bool :=
  ds.any fun d => d.severity == .error

-- ============================================================
-- Context helpers
-- ============================================================

/-- Prepend a context frame to a diagnostic. -/
def Diagnostic.addContext (d : Diagnostic) (ctx : String) : Diagnostic :=
  { d with context := ctx :: d.context }

/-- Prepend a context frame to all diagnostics in a list. -/
def Diagnostics.addContext (ds : Diagnostics) (ctx : String) : Diagnostics :=
  ds.map (·.addContext ctx)

/-- Run a fallible computation; on error, prepend context to all diagnostics. -/
def withContext [Monad m] (ctx : String) (action : ExceptT Diagnostics m α) : ExceptT Diagnostics m α :=
  ExceptT.mk do
    match ← action.run with
    | .ok a => return .ok a
    | .error ds => return .error (ds.map (·.addContext ctx))

/-- Add context to an `Except Diagnostics` value. -/
def Except.addContext (ctx : String) : Except Diagnostics α → Except Diagnostics α
  | .ok a => .ok a
  | .error ds => .error (ds.map (·.addContext ctx))

end Concrete
