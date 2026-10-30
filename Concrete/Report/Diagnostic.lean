import Concrete.Frontend.Token

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
  -- Bug 206: no default. Every diagnostic names its stable code (e.g. "E0201"), so a code-less
  -- one cannot be written; `check_diagnostic_codes_present.sh` refuses an empty or malformed one.
  code     : String
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

/-- The `spec` evidence of a rule-tagged diagnostic: the rule's anchor in the language
    specification (`docs/spec/<chapter>.md#<ID>`), computed from its versioned tag (`REF-3@1`).
    The chapter of each ID prefix is the chapter map of docs/spec/overview.md, and
    `scripts/tests/check_rules_2i.sh` compares every tagged diagnostic's evidence with
    `scripts/spec/spec.py anchors`. -/
def ruleSpecRef (rule : String) : String :=
  let id := (rule.splitOn "@").headD rule
  let chapter := match (id.splitOn "-").headD "" with
    | "REF" => "references"
    | "SCOPE" => "scope"
    | "PAT" => "patterns"
    | "FFI" => "ffi"
    | "PASS" => "pipeline"
    | "OWN" => "ownership"
    | "TYPE" => "types"
    | "MEM" => "memory"
    | "TRUST" => "trusted"
    | "STMT" => "statements"
    | "ARITH" => "arithmetic"
    | "CON" => "contracts"
    | "MOD" => "modules"
    | "SYN" => "syntax"
    | "TRAIT" => "traits"
    | _ => "README"
  s!"docs/spec/{chapter}.md#{id}"

/-- The versioned rule a diagnostic code enforces, for the codes whose rule is a whole-code
    property (one rule per code). The language specification owns this mapping: every entry's
    `Codes:` field in docs/spec, checked equal to this table and to the `--report
    diagnostic-codes` ledger by `scripts/tests/check_spec.sh` (S6). A code that several rules
    share (E0236, E0238, E0239) is tagged at its sites instead (`CheckError.rule`). -/
def codeRuleTag : String → Option String
  -- docs/spec/ownership.md
  | "E0205" | "E0230" => some "OWN-1@2"
  | "E0208" | "E0210" | "E0211" => some "OWN-2@2"
  | "E0209" | "E0212" | "E0213" => some "OWN-3@2"
  | "E0207" => some "OWN-4@2"
  | "E0287" | "E0288" | "E0289" | "E0295" => some "OWN-5@2"
  | "E0286" => some "OWN-6@1"
  | "E0294" => some "OWN-7@3"
  | "E0290" => some "OWN-8@2"
  | "E0219" | "E0291" => some "OWN-9@2"
  | "E0217" | "E0233" => some "OWN-10@2"
  | "E0243" | "E0244" => some "OWN-11@1"
  | "E0206" | "E0279" | "E0282" => some "OWN-12@1"
  | "E0570" | "E0584" | "E0272" | "E0273" => some "OWN-13@2"
  | "E0571" => some "OWN-14@2"
  -- docs/spec/types.md
  | "E0220" | "E0222" | "E0224" | "E0225" | "E0226" | "E0500" | "E0560" => some "TYPE-4@2"
  | "E0223" | "E0504" | "E0505" | "E0508" | "E0540" | "E0541" => some "TYPE-5@2"
  | "E0228" | "E0229" | "E0501" | "E0502" | "E0503" | "E0506" | "E0507" | "E0509" => some "TYPE-6@2"
  | "E0250" | "E0251" | "E0253" | "E0254" | "E0260" | "E0261" | "E0306" | "E0416" | "E0550"
  | "E0551" | "E0552" => some "TYPE-7@1"
  | "E0221" | "E0554" | "E0555" => some "TYPE-8@1"
  | "E0262" | "E0264" | "E0265" | "E0266" | "E0522" | "E0322" => some "TYPE-9@2"
  | "E0268" | "E0269" | "E0270" | "E0271" | "E0274" | "E0421" => some "TYPE-10@1"
  | "E0296" | "E0297" | "E0298" => some "TYPE-11@1"
  | "E0113" | "E0114" | "E0115" => some "TYPE-15@1"
  -- docs/spec/references.md
  | "E0237" => some "REF-12@1"
  -- docs/spec/syntax.md
  | "E0001" => some "SYN-1@1"
  | "E0283" => some "SYN-3@1"
  -- docs/spec/modules.md
  | "E0100" | "E0101" | "E0102" | "E0103" | "E0104" | "E0105" | "E0106" | "E0107" | "E0108"
  | "E0109" | "E0112" | "E0125" | "E0200" | "E0201" | "E0202" | "E0263" | "E0267" => some "MOD-1@1"
  | "E0110" | "E0111" | "E0284" | "E0285" | "E0418" | "E0419" => some "MOD-2@1"
  | "E0126" | "E0323" | "E0423" => some "MOD-2@1"
  | "E0326" => some "MOD-6@1"
  | "E0299" | "E0120" | "E0578" => some "MOD-3@2"
  -- docs/spec/trusted.md and docs/spec/ffi.md
  | "E0121" | "E0240" | "E0242" | "E0520" | "E0521" => some "TRUST-4@1"
  | "E0116" | "E0117" | "E0574" | "E0575" => some "FFI-3@1"
  | "E0572" | "E0573" | "E0576" | "E0577" | "E0585" => some "FFI-4@1"
  | "E0118" | "E0119" => some "FFI-5@1"
  | "E0588" => some "FFI-1@1"
  | "E0311" => some "TRUST-7@1"
  -- docs/spec/statements.md
  | "E0278" => some "STMT-5@1"
  | "E0216" | "E0245" | "E0280" | "E0281" | "E0542" | "E0543" => some "STMT-6@3"
  | "E0310" => some "STMT-7@3"
  | "E0307" => some "STMT-9@1"
  -- docs/spec/arithmetic.md
  | "E0227" => some "ARITH-2@2"
  | "E0553" => some "ARITH-5@1"
  -- docs/spec/patterns.md
  | "E0530" | "E0534" => some "PAT-2@2"
  | "E0304" | "E0309" | "E0531" | "E0532" | "E0533" => some "PAT-3@3"
  | "E0308" => some "PAT-5@3"
  | "E0300" => some "PAT-4@2"
  -- docs/spec/references.md (codes REF-8 gained when ownership migrated)
  | "E0218" | "E0235" => some "REF-8@1"
  -- docs/spec/traits.md (trait coherence, bug 186)
  | "E0590" => some "TRAIT-1@1"
  | "E0591" => some "TRAIT-2@1"
  | "E0592" => some "TRAIT-3@1"
  | "E0593" => some "TRAIT-4@1"
  | "E0594" => some "TRAIT-5@1"
  | "E0595" => some "TRAIT-6@1"
  | "E0579" | "E0580" => some "TRAIT-8@1"
  | "E0581" | "E0582" | "E0586" | "E0587" => some "TRAIT-9@1"
  | "E0241" => some "TRAIT-10@2"
  | "E0583" => some "TYPE-16@1"
  -- docs/spec/contracts.md (one rule since bug 203 moved the append() refusal to E0421)
  | "E0420" => some "CON-4@1"
  -- bug 206: codes for the refusals that had none
  | "E0122" | "E0123" | "E0124" => some "MOD-4@1"
  | "E0313" => some "TYPE-9@2"
  | "E0314" => some "TYPE-10@1"
  | "E0315" => some "TYPE-10@1"
  | "E0316" => some "TYPE-7@1"
  | "E0317" => some "TYPE-9@2"
  | "E0318" => some "PAT-8@1"
  | "E0319" => some "STMT-10@1"
  | "E0320" => some "CON-3@1"
  | "E0321" => some "CON-3@1"
  | _ => none

/-- The rules of a code that several rules share without a whole-code tag (`codeRuleTag` is `none`
    for it): its sites tag each diagnostic (`CheckError.rule`), and the ledger lists them all. -/
def codeSharedRules : String → List String
  | "E0252" => ["PAT-5@3", "TYPE-7@1"]
  | _ => []

/-- The `rule` and `spec` evidence of a diagnostic whose code has a whole-code rule. -/
def codeRuleEvidence (code : String) : List (String × String) :=
  match codeRuleTag code with
  | some r => [("rule", r), ("spec", ruleSpecRef r)]
  | none => []

/-- The evidence a diagnostic is rendered with: its own, plus the `rule` and `spec` of its code's
    whole-code rule when no site set a rule (so every path that builds a diagnostic carries the
    specification's tag, not only `throwCheck`). -/
def Diagnostic.fullEvidence (d : Diagnostic) : List (String × String) :=
  if d.evidence.any (·.1 == "rule") then d.evidence else d.evidence ++ codeRuleEvidence d.code

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
  let evStr := if d.fullEvidence.isEmpty then ""
    else "\n" ++ ("\n".intercalate (d.fullEvidence.map fun (k, v) => s!"  {k}: {v}"))
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
  let evJson := "[" ++ ", ".intercalate (d.fullEvidence.map fun (k, v) =>
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

/-- Stamp a source-file attribution onto diagnostics that don't have one.
    Used for module-scoped diagnostics from `mod x;` sub-files, so the
    rendered location (and snippet, via the SourceMap) names the file the
    span actually points into instead of the main input file (ROADMAP #24a). -/
def Diagnostics.stampFile (ds : Diagnostics) (f : String) : Diagnostics :=
  if f.isEmpty then ds
  else ds.map fun d => if d.file.isEmpty then { d with file := f } else d

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
