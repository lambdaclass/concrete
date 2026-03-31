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
  span     : Option Span    -- line/col from Token.lean
  hint     : Option String  -- suggested fix
  file     : String := ""   -- source file path (empty = unknown)

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
  s!"{locStr}{severityStr d.severity}[{d.pass}]: {d.message}{snippetStr}{hintStr}"

def renderDiagnostics (ds : Diagnostics) (sourceMap : SourceMap := []) : String :=
  "\n".intercalate (ds.map fun d => d.render sourceMap)

-- ============================================================
-- Queries
-- ============================================================

def hasErrors (ds : Diagnostics) : Bool :=
  ds.any fun d => d.severity == .error

-- ============================================================
-- Lift helpers
-- ============================================================

/-- Convert an `Except String α` into `Except Diagnostics α`. -/
def liftStringError (pass : String) : Except String α → Except Diagnostics α
  | .ok a => .ok a
  | .error msg => .error [{ severity := .error, message := msg, pass := pass, span := none, hint := none }]

end Concrete
