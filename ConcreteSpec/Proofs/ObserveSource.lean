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
The diagnostic for a source-derived position differs from every diagnostic that supplies a
wrong byte column. In particular a scalar-counting or tab-expanding source-position producer
cannot satisfy this law by rejecting an admission premise about its own output. -/
theorem source_trap_column (path : String) (pathOk : PathOk path) (line : Nat)
    (linePositive : 0 < line) (before : String) (kind : TrapKind) (site : SiteId) (column : Nat)
    (columnPositive : 0 < column) (wrong : column ≠ before.utf8ByteSize + 1) :
    trapLine kind site (sourceSpan path pathOk line linePositive before) ≠
      trapLine kind site ⟨path, line, column, linePositive, columnPositive⟩ := by
  intro equal
  obtain ⟨_, _, spanEqual⟩ := (trapLine_injective _ _ _ _ _ _).mp equal
  cases spanEqual
  exact wrong rfl

/-- A planted source-position producer counts Unicode scalars instead of UTF-8 bytes. -/
def scalarColumnSpan (path : String) (_pathOk : PathOk path) (line : Nat) (linePositive : 0 < line)
    (before : String) : Span :=
  ⟨path, line, before.toList.length + 1, linePositive, Nat.zero_lt_succ _⟩

/-- Instance (law `ConcreteSpec.Core.Observe.source_trap_column`, case `utf8_tab`). -/
example : trapLine .overflow ⟨7⟩ (sourceSpan "src/main.con" (by decide) 1 (by decide) "a\té") ≠
    trapLine .overflow ⟨7⟩ ⟨"src/main.con", 1, 4, by decide, by decide⟩ :=
  source_trap_column "src/main.con" (by decide) 1 (by decide) "a\té" .overflow ⟨7⟩ 4
    (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.source_trap_column`, case `utf8_tab`,
subject `ConcreteSpec.Core.Observe.sourceSpan`, mutant `ConcreteSpec.Core.Observe.scalarColumnSpan`). -/
example : PathOk "src/main.con" ∧ (0 < 1) ∧ (0 < 4) ∧ (4 ≠ "a\té".utf8ByteSize + 1) ∧
    ¬ (trapLine .overflow ⟨7⟩ (scalarColumnSpan "src/main.con" (by decide) 1 (by decide) "a\té") ≠
      trapLine .overflow ⟨7⟩ ⟨"src/main.con", 1, 4, by decide, by decide⟩) := by
  decide

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
theorem sourcePrefixSpan_second_line (path : String) (pathOk : PathOk path)
    (firstLine before : String)
    (noNewlineFirst : '\n' ∉ firstLine.toList)
    (noNewlineBefore : '\n' ∉ before.toList) :
    sourcePrefixSpan path pathOk (firstLine ++ "\n" ++ before) =
      sourceSpan path pathOk 2 (by decide) before := by
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
def scalarPrefixSpan (path : String) (pathOk : PathOk path) (prefixText : String) : Span :=
  let scanned := scanSourcePrefix prefixText.toList 0 []
  scalarColumnSpan path pathOk (scanned.1 + 1) (Nat.zero_lt_succ _) (String.ofList scanned.2)

/-- Fault: ignores newline breaks and treats the entire multi-line prefix as a single line. -/
def singleLinePrefixSpan (path : String) (pathOk : PathOk path) (prefixText : String) : Span :=
  sourceSpan path pathOk 1 (by decide) prefixText

/-- Instance (law `ConcreteSpec.Core.Observe.sourcePrefixSpan_second_line`, case `multiline_utf8_tab`). -/
example : sourcePrefixSpan "src/main.con" (by decide) ("let x = 1;" ++ "\n" ++ "a\té") =
    sourceSpan "src/main.con" (by decide) 2 (by decide) "a\té" :=
  sourcePrefixSpan_second_line "src/main.con" (by decide) "let x = 1;" "a\té"
    (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.sourcePrefixSpan_second_line`, case `multiline_utf8_tab`,
subject `ConcreteSpec.Core.Observe.sourcePrefixSpan`, mutant `ConcreteSpec.Core.Observe.scalarPrefixSpan`). -/
example : PathOk "src/main.con" ∧ ('\n' ∉ "let x = 1;".toList) ∧ ('\n' ∉ "a\té".toList) ∧
    ¬ (scalarPrefixSpan "src/main.con" (by decide) ("let x = 1;" ++ "\n" ++ "a\té") =
      sourceSpan "src/main.con" (by decide) 2 (by decide) "a\té") := by
  decide

/-- Planted (law `ConcreteSpec.Core.Observe.sourcePrefixSpan_second_line`, case `multiline_utf8_tab`,
subject `ConcreteSpec.Core.Observe.sourcePrefixSpan`, mutant `ConcreteSpec.Core.Observe.singleLinePrefixSpan`). -/
example : PathOk "src/main.con" ∧ ('\n' ∉ "let x = 1;".toList) ∧ ('\n' ∉ "a\té".toList) ∧
    ¬ (singleLinePrefixSpan "src/main.con" (by decide) ("let x = 1;" ++ "\n" ++ "a\té") =
      sourceSpan "src/main.con" (by decide) 2 (by decide) "a\té") := by
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
theorem sourceSpan_ascii_eq_char_col (path : String) (ok : PathOk path)
    (line : Nat) (positive : 0 < line) (before : String)
    (ascii : ∀ c ∈ before.toList, c.val ≤ 127) :
    (sourceSpan path ok line positive before).col = before.toList.length + 1 := by
  have bytes := source_ascii_bytes before.toList ascii
  simpa [sourceSpan] using bytes

/-- Law (subject `ConcreteSpec.Core.Observe.sourceSpan`).
Any non-ASCII scalar makes the produced byte column strictly exceed the scalar column. -/
theorem sourceSpan_multibyte_gt_char_col (path : String) (ok : PathOk path)
    (line : Nat) (positive : 0 < line) (before : String)
    (multi : ∃ c ∈ before.toList, 127 < c.val) :
    before.toList.length + 1 < (sourceSpan path ok line positive before).col := by
  have bytes := source_multibyte_bytes before.toList multi
  simp only [String.ofList_toList] at bytes
  change before.toList.length + 1 < before.utf8ByteSize + 1
  omega

/-- Fault: expands tabs to eight columns, even in ASCII source. -/
def expandedTabSpan (path : String) (ok : PathOk path) (line : Nat) (positive : 0 < line)
    (before : String) : Span :=
  let sp := sourceSpan path ok line positive before
  { sp with col := sp.col + 7 * (before.toList.filter (· == '\t')).length,
            col_pos := by have h := sp.col_pos; omega }

/-- Instance (law `ConcreteSpec.Core.Observe.sourceSpan_ascii_eq_char_col`, case `ascii_tab`). -/
example : (sourceSpan "src/main.con" (by decide) 1 (by decide) "a\tb").col =
    "a\tb".toList.length + 1 :=
  sourceSpan_ascii_eq_char_col "src/main.con" (by decide) 1 (by decide) "a\tb" (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.sourceSpan_ascii_eq_char_col`, case `ascii_tab`,
subject `ConcreteSpec.Core.Observe.sourceSpan`, mutant `ConcreteSpec.Core.Observe.expandedTabSpan`). -/
example : PathOk "src/main.con" ∧ (0 < 1) ∧
    (∀ c ∈ "a\tb".toList, c.val ≤ 127) ∧
    ¬ ((expandedTabSpan "src/main.con" (by decide) 1 (by decide) "a\tb").col =
      "a\tb".toList.length + 1) := by decide

/-- Instance (law `ConcreteSpec.Core.Observe.sourceSpan_multibyte_gt_char_col`, case `multibyte_tab`). -/
example : "a\té".toList.length + 1 <
    (sourceSpan "src/main.con" (by decide) 1 (by decide) "a\té").col :=
  sourceSpan_multibyte_gt_char_col "src/main.con" (by decide) 1 (by decide) "a\té" (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.sourceSpan_multibyte_gt_char_col`, case `multibyte_tab`,
subject `ConcreteSpec.Core.Observe.sourceSpan`, mutant `ConcreteSpec.Core.Observe.scalarColumnSpan`). -/
example : PathOk "src/main.con" ∧ (0 < 1) ∧
    (∃ c ∈ "a\té".toList, 127 < c.val) ∧
    ¬ ("a\té".toList.length + 1 <
      (scalarColumnSpan "src/main.con" (by decide) 1 (by decide) "a\té").col) := by decide

/-- Helper (computation). On a single source line the scanner retains the complete prefix,
so its span has the byte column computed from that text. This equation is assembly support,
not independent column-law evidence. (approved: APPR-C6-agy-6477c65a7) -/
theorem sourcePrefixSpan_hasByteColumn (path : String) (ok : PathOk path) (before : String)
    (singleLine : '\n' ∉ before.toList) :
    Span.HasByteColumn (sourcePrefixSpan path ok before) before := by
  have scan := scanSourcePrefix_no_newline before.toList [] 0 [] singleLine
  simp only [List.append_nil, List.nil_append] at scan
  simp [sourcePrefixSpan, scan, scanSourcePrefix, Span.HasByteColumn, sourceSpan, byteColumn]

end ConcreteSpec.Core.Observe
