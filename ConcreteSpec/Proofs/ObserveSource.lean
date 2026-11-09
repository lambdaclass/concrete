import ConcreteSpec.Proofs.Observe2

/-!
# Source-derived trap positions

The operation constructs its column from the source prefix; callers do not promise that a
pre-existing column is correct. The discriminator below tests the rendered diagnostic, using
the independently proved injectivity of its complete parser/renderer pair.
-/

namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core

/-- Law (subject `ConcreteSpec.Core.Observe.sourceSpan`).
A candidate column renders the source-derived diagnostic exactly when it is the UTF-8 byte
column. This includes both the matching-column case and rejection of every wrong column,
without assuming a mismatch or admission property of the produced position. -/
theorem source_trap_column (path : SourcePath) (line : Nat)
    (linePositive : 0 < line) (before : String) (kind : TrapKind) (site : SiteId) (column : Nat)
    (columnPositive : 0 < column) :
    (trapLine kind site (sourceSpan path line linePositive before) =
      trapLine kind site ⟨path, 0, 0, by decide, line, column, linePositive, columnPositive⟩ ↔
      column = before.utf8ByteSize + 1) := by
  rw [trapLine_injective]
  simp [sourceSpan, eq_comm]

/-- A planted source-position producer counts Unicode scalars instead of UTF-8 bytes. -/
def scalarColumnSpan (path : SourcePath) (line : Nat) (linePositive : 0 < line)
    (before : String) : Span :=
  ⟨path, before.utf8ByteSize, before.utf8ByteSize, Nat.le_refl _, line, before.toList.length + 1, linePositive, Nat.zero_lt_succ _⟩

/-- Instance (law `ConcreteSpec.Core.Observe.source_trap_column`, case `utf8_tab`). -/
example : (trapLine .overflow ⟨7⟩ (sourceSpan mainSourcePath 1 (by decide) "a\té") =
    trapLine .overflow ⟨7⟩ ⟨mainSourcePath, 0, 0, by decide, 1, 4, by decide, by decide⟩ ↔
    4 = "a\té".utf8ByteSize + 1) :=
  source_trap_column mainSourcePath 1 (by decide) "a\té" .overflow ⟨7⟩ 4 (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.source_trap_column`, case `utf8_tab`,
subject `ConcreteSpec.Core.Observe.sourceSpan`, mutant `ConcreteSpec.Core.Observe.scalarColumnSpan`). -/
example : (0 < 1) ∧ (0 < 4) ∧
    ¬ (trapLine .overflow ⟨7⟩ (scalarColumnSpan mainSourcePath 1 (by decide) "a\té") =
      trapLine .overflow ⟨7⟩ ⟨mainSourcePath, 0, 0, by decide, 1, 4, by decide, by decide⟩ ↔
      4 = "a\té".utf8ByteSize + 1) := by decide

/-- Instance (law `ConcreteSpec.Core.Observe.source_trap_column`, case `matching_utf8_tab`). -/
example : (trapLine .overflow ⟨7⟩ (sourceSpan mainSourcePath 1 (by decide) "a\té") =
    trapLine .overflow ⟨7⟩ ⟨mainSourcePath, 0, 0, by decide, 1, 5, by decide, by decide⟩ ↔
    5 = "a\té".utf8ByteSize + 1) :=
  source_trap_column mainSourcePath 1 (by decide) "a\té" .overflow ⟨7⟩ 5 (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.source_trap_column`, case `matching_utf8_tab`,
subject `ConcreteSpec.Core.Observe.sourceSpan`, mutant `ConcreteSpec.Core.Observe.scalarColumnSpan`). -/
example : (0 < 1) ∧ (0 < 5) ∧
    ¬ (trapLine .overflow ⟨7⟩ (scalarColumnSpan mainSourcePath 1 (by decide) "a\té") =
      trapLine .overflow ⟨7⟩ ⟨mainSourcePath, 0, 0, by decide, 1, 5, by decide, by decide⟩ ↔
      5 = "a\té".utf8ByteSize + 1) := by decide

/-- Helper (computation). Scanning a character list containing no newline preserves the newline
count and appends the characters to the current line accumulator; feeds
`ConcreteSpec.Core.Observe.sourcePrefixSpan_second_line`. (approved: APPR-C6-agy-6477c65a7) -/
theorem scanSourcePrefix_no_newline (cs rest : List Char) (newlines : Nat)
    (current : List Char) (noNewline : '\n' ∉ cs) :
    scanSourcePrefix (cs ++ rest) newlines current =
      scanSourcePrefix rest newlines (current ++ cs) := by
  induction cs generalizing newlines current with
  | nil => simp
  | cons c cs ih =>
    have hc : c ≠ '\n' := fun h => noNewline (by simp [h])
    have hcs : '\n' ∉ cs := fun h => noNewline (List.mem_cons_of_mem _ h)
    simpa [scanSourcePrefix, hc, List.append_assoc] using ih newlines (current ++ [c]) hcs

/-- Law (subject `ConcreteSpec.Core.Observe.sourcePrefixSpan`).
Across a newline boundary, multi-line source prefix scanning increments the 1-based line
number to `2`, resets the line accumulator, and computes the second line's 1-based UTF-8 byte
column from the characters after the newline. -/
theorem sourcePrefixSpan_second_line (path : SourcePath)
    (firstLine before : String)
    (noNewlineFirst : '\n' ∉ firstLine.toList)
    (noNewlineBefore : '\n' ∉ before.toList) :
    sourcePrefixSpan path (firstLine ++ "\n" ++ before) =
      { sourceSpan path 2 (by decide) before with
        startOffset := (firstLine ++ "\n" ++ before).utf8ByteSize
        endOffset := (firstLine ++ "\n" ++ before).utf8ByteSize
        range_ok := Nat.le_refl _ } := by
  have splitChars : (firstLine ++ "\n" ++ before).toList =
      firstLine.toList ++ ('\n' :: before.toList) := by
    simp [String.toList_append, List.append_assoc]
  have step1 := scanSourcePrefix_no_newline firstLine.toList ('\n' :: before.toList) 0 []
    noNewlineFirst
  have step2 : scanSourcePrefix ('\n' :: before.toList) 0 firstLine.toList =
      scanSourcePrefix before.toList 1 [] := rfl
  have step3 := scanSourcePrefix_no_newline before.toList [] 1 [] noNewlineBefore
  simp only [List.nil_append, List.append_nil] at step1 step3
  have hscan : scanSourcePrefix (firstLine ++ "\n" ++ before).toList 0 [] = (1, before.toList) := by
    rw [splitChars, step1, step2, step3]
    rfl
  simp only [sourcePrefixSpan, sourceSpan, hscan, String.ofList_toList]

/-- Fault: counts Unicode scalars on the current line instead of UTF-8 bytes after scanning
newlines. -/
def scalarPrefixSpan (path : SourcePath) (prefixText : String) : Span :=
  let scanned := scanSourcePrefix prefixText.toList 0 []
  { scalarColumnSpan path (scanned.1 + 1) (Nat.zero_lt_succ _) (String.ofList scanned.2) with
    startOffset := prefixText.utf8ByteSize
    endOffset := prefixText.utf8ByteSize
    range_ok := Nat.le_refl _ }

/-- Fault: ignores newline breaks and treats the entire multi-line prefix as a single line. -/
def singleLinePrefixSpan (path : SourcePath) (prefixText : String) : Span :=
  sourceSpan path 1 (by decide) prefixText

/-- Instance (law `ConcreteSpec.Core.Observe.sourcePrefixSpan_second_line`, case `multiline_utf8_tab`). -/
example : sourcePrefixSpan mainSourcePath ("let x = 1;" ++ "\n" ++ "a\té") =
    { sourceSpan mainSourcePath 2 (by decide) "a\té" with
        startOffset := ("let x = 1;" ++ "\n" ++ "a\té").utf8ByteSize
        endOffset := ("let x = 1;" ++ "\n" ++ "a\té").utf8ByteSize
        range_ok := Nat.le_refl _ } :=
  sourcePrefixSpan_second_line mainSourcePath "let x = 1;" "a\té"
    (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.sourcePrefixSpan_second_line`, case `multiline_utf8_tab`,
subject `ConcreteSpec.Core.Observe.sourcePrefixSpan`, mutant `ConcreteSpec.Core.Observe.scalarPrefixSpan`). -/
example : ('\n' ∉ "let x = 1;".toList) ∧ ('\n' ∉ "a\té".toList) ∧
    ¬ (scalarPrefixSpan mainSourcePath ("let x = 1;" ++ "\n" ++ "a\té") =
      { sourceSpan mainSourcePath 2 (by decide) "a\té" with
        startOffset := ("let x = 1;" ++ "\n" ++ "a\té").utf8ByteSize
        endOffset := ("let x = 1;" ++ "\n" ++ "a\té").utf8ByteSize
        range_ok := Nat.le_refl _ }) := by
  decide

/-- Planted (law `ConcreteSpec.Core.Observe.sourcePrefixSpan_second_line`, case `multiline_utf8_tab`,
subject `ConcreteSpec.Core.Observe.sourcePrefixSpan`, mutant `ConcreteSpec.Core.Observe.singleLinePrefixSpan`). -/
example : ('\n' ∉ "let x = 1;".toList) ∧ ('\n' ∉ "a\té".toList) ∧
    ¬ (singleLinePrefixSpan mainSourcePath ("let x = 1;" ++ "\n" ++ "a\té") =
      { sourceSpan mainSourcePath 2 (by decide) "a\té" with
        startOffset := ("let x = 1;" ++ "\n" ++ "a\té").utf8ByteSize
        endOffset := ("let x = 1;" ++ "\n" ++ "a\té").utf8ByteSize
        range_ok := Nat.le_refl _ }) := by
  decide

/-- Helper (arithmetic). Each Unicode scalar occupies at least one UTF-8 byte. Used to
compare source-produced byte columns with scalar columns. (approved: APPR-C6-agy-6477c65a7) -/
theorem source_bytes_ge_scalars (cs : List Char) :
    cs.length ≤ (String.ofList cs).utf8ByteSize := by
  induction cs with
  | nil => simp
  | cons c cs ih =>
    have positive := c.utf8Size_pos
    simp only [String.ofList_cons, String.utf8ByteSize_append,
      String.utf8ByteSize_singleton, List.length_cons]
    omega

/-- Helper (computation). ASCII encoding contributes exactly one byte per character, independently
of source-position production. (approved: APPR-C6-agy-6477c65a7) -/
theorem source_ascii_bytes (cs : List Char) (ascii : ∀ c ∈ cs, c.val ≤ 127) :
    (String.ofList cs).utf8ByteSize = cs.length := by
  induction cs with
  | nil => simp
  | cons c cs ih =>
    have one := Char.utf8Size_eq_one_iff.mpr (ascii c (by simp))
    have rest := ih (fun c hc => ascii c (by simp [hc]))
    simp [String.utf8ByteSize_append, String.utf8ByteSize_singleton, one, rest, Nat.add_comm]

/-- Helper (arithmetic). A non-ASCII scalar contributes more than one UTF-8 byte; the other
scalars cannot cancel this excess. (approved: APPR-C6-agy-6477c65a7) -/
theorem source_multibyte_bytes (cs : List Char) (multi : ∃ c ∈ cs, 127 < c.val) :
    cs.length < (String.ofList cs).utf8ByteSize := by
  induction cs with
  | nil => simp at multi
  | cons c cs ih =>
    have positive := c.utf8Size_pos
    have lower := source_bytes_ge_scalars cs
    simp only [String.ofList_cons, String.utf8ByteSize_append,
      String.utf8ByteSize_singleton, List.length_cons]
    obtain ⟨d, member, large⟩ := multi
    cases member with
    | head =>
      have notOne : c.utf8Size ≠ 1 := by
        intro one
        have small := Char.utf8Size_eq_one_iff.mp one
        exact (UInt32.not_lt.mpr small) large
      omega
    | tail _ member =>
      have strict := ih ⟨d, member, large⟩
      omega

/-- Law (subject `ConcreteSpec.Core.Observe.sourceSpan`).
For ASCII source text, the produced byte column coincides with the 1-based scalar column.
The premise describes the source alphabet, never the computed position. -/
theorem sourceSpan_ascii_eq_char_col (path : SourcePath)
    (line : Nat) (positive : 0 < line) (before : String)
    (ascii : ∀ c ∈ before.toList, c.val ≤ 127) :
    (sourceSpan path line positive before).col = before.toList.length + 1 := by
  have bytes := source_ascii_bytes before.toList ascii
  simpa [sourceSpan] using bytes

/-- Law (subject `ConcreteSpec.Core.Observe.sourceSpan`).
Any non-ASCII scalar makes the produced byte column strictly exceed the scalar column. -/
theorem sourceSpan_multibyte_gt_char_col (path : SourcePath)
    (line : Nat) (positive : 0 < line) (before : String)
    (multi : ∃ c ∈ before.toList, 127 < c.val) :
    before.toList.length + 1 < (sourceSpan path line positive before).col := by
  have bytes := source_multibyte_bytes before.toList multi
  simp only [String.ofList_toList] at bytes
  change before.toList.length + 1 < before.utf8ByteSize + 1
  omega

/-- Fault: expands tabs to eight columns, even in ASCII source. -/
def expandedTabSpan (path : SourcePath) (line : Nat) (positive : 0 < line)
    (before : String) : Span :=
  let sp := sourceSpan path line positive before
  { sp with col := sp.col + 7 * (before.toList.filter (· == '\t')).length,
            col_pos := by have h := sp.col_pos; omega }

/-- Instance (law `ConcreteSpec.Core.Observe.sourceSpan_ascii_eq_char_col`, case `ascii_tab`). -/
example : (sourceSpan mainSourcePath 1 (by decide) "a\tb").col =
    "a\tb".toList.length + 1 :=
  sourceSpan_ascii_eq_char_col mainSourcePath 1 (by decide) "a\tb" (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.sourceSpan_ascii_eq_char_col`, case `ascii_tab`,
subject `ConcreteSpec.Core.Observe.sourceSpan`, mutant `ConcreteSpec.Core.Observe.expandedTabSpan`). -/
example : (0 < 1) ∧
    (∀ c ∈ "a\tb".toList, c.val ≤ 127) ∧
    ¬ ((expandedTabSpan mainSourcePath 1 (by decide) "a\tb").col =
      "a\tb".toList.length + 1) := by decide

/-- Instance (law `ConcreteSpec.Core.Observe.sourceSpan_multibyte_gt_char_col`, case `multibyte_tab`). -/
example : "a\té".toList.length + 1 <
    (sourceSpan mainSourcePath 1 (by decide) "a\té").col :=
  sourceSpan_multibyte_gt_char_col mainSourcePath 1 (by decide) "a\té" (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.sourceSpan_multibyte_gt_char_col`, case `multibyte_tab`,
subject `ConcreteSpec.Core.Observe.sourceSpan`, mutant `ConcreteSpec.Core.Observe.scalarColumnSpan`). -/
example : (0 < 1) ∧
    (∃ c ∈ "a\té".toList, 127 < c.val) ∧
    ¬ ("a\té".toList.length + 1 <
      (scalarColumnSpan mainSourcePath 1 (by decide) "a\té").col) := by decide

/-- Helper (computation). On a single source line the scanner retains the complete prefix,
so its span has the byte column computed from that text. This equation is assembly support,
not independent column-law evidence. (approved: PENDING) -/
theorem sourcePrefixSpan_hasByteColumn (path : SourcePath) (before : String)
    (singleLine : '\n' ∉ before.toList) :
    Span.HasByteColumn (sourcePrefixSpan path before) before := by
  have scan := scanSourcePrefix_no_newline before.toList [] 0 [] singleLine
  simp only [List.append_nil, List.nil_append] at scan
  simp [sourcePrefixSpan, scan, scanSourcePrefix, Span.HasByteColumn, sourceSpan, byteColumn]

end ConcreteSpec.Core.Observe
