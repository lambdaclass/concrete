# ISS-0045: source coordinates

Design only; verified against tracked files at `3b4d01d6fee3910436eb7b63e49847f1e734a3d3`. No implementation is authorized by this note. The architectural criterion is one authoritative fact with checked views (docs/PHILOSOPHY.md:68-73).

Non-vacuity convention: each proposed law is a schema quantified over the named executable/predicate subject, with that subject occurring in its conclusion. Instantiate it with the intended subject and with a mutant of exactly the same type, at identical inputs and identical premises. A negative control must falsify the conclusion, never manufacture an inconsistent premise or assume the desired output. Closed cases below are proposed proof obligations, not claims of executed Lean checks. Preservation of an existing theorem means its full quantified proposition, including refusal and hostile-input cases, survives; proof scripts may change.

## Pair and decision: VIEW

ConcreteSpec/Spec/Ident.lean:695-702 owns byte `lineAt`/`columnAt`, and :812-814 owns `Span.LocationOk` (range plus coordinate agreement). ConcreteSpec/Spec/Observe.lean:278-307 independently owns UTF-8 length arithmetic, character scanning, constructors and a column-only predicate. Ident handles arbitrary bytes and offsets; Observe accepts strings, preserves a caller-supplied candidate line, and renders diagnostics. Neither constructor currently establishes whole-file correspondence.

Ident remains the single owner of coordinate arithmetic. Add `SourceText.pointSpan path bytes offset` with an explicit offset-bound proof; its line/column come directly from `lineAt`/`columnAt`. Observe.sourcePrefixSpan becomes this constructor at the end of `prefixText.toUTF8.toList`. It remains a candidate API: even a correct prefix does not establish that it is a prefix of the loaded file. Loader admission remains mandatory.

Preserve the total legacy sourceSpan signature and every current result, including newline-containing inputs and arbitrary positive supplied lines. Ident owns a distinctly named `candidateLineSpan path line positive bytes`: offsets are bytes.length, line is supplied, column is bytes.length+1. This is a real abstraction for a caller-described line fragment, explicitly not a whole-file coordinate computation. Observe.sourceSpan only encodes UTF-8 and delegates. It must not pretend to call columnAt on a fragment containing a newline. Ident also owns `lineFragmentColumn bytes := bytes.length+1`; Observe.byteColumn delegates to that candidate operation. Its correspondence with columnAt requires absence of byte 10. This distinction is necessary to preserve the total API without silently changing diagnostics.

Retire scanSourcePrefix from production construction. Its generalized character-accumulator API and helper theorem have no independent consumer outside the source proof module: delete that helper and rewrite its dependent proof using byte-prefix lemmas. Retain HasByteColumn only as the useful column-only candidate postcondition, delegating to Ident.lineFragmentColumn. It is explicitly weaker than LocationOk.

## Laws and discriminators

Notation `B s := s.toUTF8.toList`, `F := Observe.sourcePrefixSpan`, `C := Observe.sourceSpan`, `D := Observe.byteColumn`, `H := Span.HasByteColumn`.

* `∀ path s suffix, (F path s).LocationOk (B s ++ suffix)` and `(F path s).startOffset = (B s).length ∧ (F path s).endOffset = (B s).length`. Subject F has exact type `SourcePath → String → Span`. Closed path mainSourcePath, s `"a\n\té"`, suffix `[]`: offset 5, line 2, col 4. Same-type mutants replace line by 1, column by scalar count, expand tab by eight, or reset offsets to final-line length. All preserve the prefix premise and fail this case (use scalar column 3 for the multibyte mutant).
* `∀ path line hp s, C path line hp s = Ident.candidateLineSpan path line hp (B s)`. Closed line 2, s `"a\té"`: offsets 4, line 2, col 5. C's exact dependent type is its existing signature at Observe:283-285. Mutants of that type retain all fields except column scalar-count+1 or tab-expanded; use the same witnesses. A line-reset mutant is refuted at supplied line 2. Include s `"a\n"` to prevent incorrectly substituting columnAt for fragment length.
* `∀ s, D s = Ident.lineFragmentColumn (B s)` and `10 ∉ B s → D s = (B s).columnAt (B s).length`. Subject D : String → Nat. Closed s `"a\té"`, newline-free, expected 5; scalar-count and double-tab mutants fail with unchanged premise.
* `∀ sp s, H sp s ↔ sp.col = Ident.lineFragmentColumn (B s)`. Subject H : Span → String → Prop. Closed candidate above; mutant always True fails when sp.col is 6; mutant always False fails on the candidate. Correspondence is about the actual span, not a premise that already assumes its column.
* Point constructor: `∀ p b o h, (pointSpan p b o h).LocationOk b ∧ (pointSpan p b o h).startOffset = o ∧ (pointSpan p b o h).endOffset = o`. Exact type `∀ p : SourcePath, ∀ b : SourceText, ∀ o : Nat, o ≤ b.length → Span`. Closed b `[10,9,195,169]`, o=4; column+1 and offset-reset mutants preserve the bound proof and fail. Candidate helper law is its full record equation at arbitrary p,line,h,b; bytes `[10]`, supplied line 2 distinguishes it from the point constructor. Fragment helper law is `∀ b, lineFragmentColumn b = b.length+1`; `[9,195,169]` refutes scalar/tab mutants. These helper equations fix the candidate abstraction, not a second definition of whole-file validity.

## Existing strength and order

Keep every Ident newline/tab/location theorem unchanged. Keep source_trap_column, sourceSpan_ascii_eq_char_col, sourceSpan_multibyte_gt_char_col, byteColumn_tab, sourcePrefixSpan_second_line and sourcePrefixSpan_hasByteColumn with their present statements. Keep all rendered-diagnostic discriminators and their planted controls. Only scanSourcePrefix_no_newline disappears with its retired subject; it was a scan implementation helper, not an admission or coordinate guarantee. The inventory below identifies every dependent declaration to reprove, including examples and evidence modules.

First add Ident constructors and byte-prefix/UTF-8 lemmas in Spec/Ident and Proofs/Ident; then rebuild Observe views; then rewrite Proofs/ObserveSource, Observe2, ObserveEvidence and ObserveOutput, and README's API description. Remove the scan only after rewriting every use. Run the Lean target and source/report gates during implementation.

## QUESTIONS

None for this behavior-preserving design. Any future replacement of the candidate API by guaranteed whole-file admission, restriction of its accepted strings/lines, or changed diagnostics requires a separate owner answer. This proposal does none of those.

## Verified user and affected declaration inventory

Repository-wide tracked-text search at the SHA above, excluding historical legacy/, task material, issue rows and proposal notes. Includes declarations, qualified and unqualified references, proof unfolding, examples, comments and documentation; broad matches intentionally retain dependent users rather than losing namespace-local calls. No production user is excluded by a hand-picked file list. External Lean library internals are not repository users.

Search expression: `\b(lineAt|columnAt|LocationOk|byteColumn|sourceSpan|sourcePrefixSpan|scanSourcePrefix|HasByteColumn)\b`. Reproduce with `git grep -n -E` over the tracked nonhistorical sources (the expression uses standard word boundaries).

Every matching use (path:line):

```text
ConcreteSpec/Proofs/Ident.lean:1255: /-- Law (subject `ConcreteSpec.Core.SourceText.lineAt`).
ConcreteSpec/Proofs/Ident.lean:1258: (source ++ [byte]).lineAt (source.length + 1) = source.lineAt source.length + if byte = 10 then 1 else 0 := by
ConcreteSpec/Proofs/Ident.lean:1261: simp only [SourceText.lineAt]
ConcreteSpec/Proofs/Ident.lean:1265: /-- Law (subject `ConcreteSpec.Core.SourceText.lineAt`).
ConcreteSpec/Proofs/Ident.lean:1268: (source ++ [10]).lineAt (source.length + 1) = source.lineAt source.length + 1 := by
ConcreteSpec/Proofs/Ident.lean:1271: /-- Law (subject `ConcreteSpec.Core.SourceText.lineAt`).
ConcreteSpec/Proofs/Ident.lean:1274: (source ++ [byte]).lineAt (source.length + 1) = source.lineAt source.length := by
ConcreteSpec/Proofs/Ident.lean:1277: /-- Law (subject `ConcreteSpec.Core.SourceText.columnAt`).
ConcreteSpec/Proofs/Ident.lean:1280: (source ++ [byte]).columnAt (source.length + 1) =
ConcreteSpec/Proofs/Ident.lean:1281: if byte = 10 then 1 else source.columnAt source.length + 1 := by
ConcreteSpec/Proofs/Ident.lean:1284: simp only [SourceText.columnAt]
ConcreteSpec/Proofs/Ident.lean:1288: /-- Law (subject `ConcreteSpec.Core.SourceText.columnAt`).
ConcreteSpec/Proofs/Ident.lean:1291: (source ++ [9]).columnAt (source.length + 1) = source.columnAt source.length + 1 := by
ConcreteSpec/Proofs/Ident.lean:1294: /-- Law (subject `ConcreteSpec.Core.SourceText.columnAt`).
ConcreteSpec/Proofs/Ident.lean:1297: (source ++ [10]).columnAt (source.length + 1) = 1 := by
ConcreteSpec/Proofs/Ident.lean:1300: /-- Helper (projection). `LocationOk` projects to bounds and the two computed coordinates.
ConcreteSpec/Proofs/Ident.lean:1303: sp.LocationOk source ↔ sp.endOffset ≤ source.length ∧
ConcreteSpec/Proofs/Ident.lean:1304: sp.line = source.lineAt sp.startOffset ∧ sp.col = source.columnAt sp.startOffset := Iff.rfl
ConcreteSpec/Proofs/Ident.lean:2278: example : (aSource ++ [10]).lineAt (aSource.length + 1) = aSource.lineAt aSource.length + 1 :=
ConcreteSpec/Proofs/Ident.lean:2281: /-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.constantLine`). -/
ConcreteSpec/Proofs/Ident.lean:2286: example : (aSource ++ [9]).lineAt (aSource.length + 1) = aSource.lineAt aSource.length :=
ConcreteSpec/Proofs/Ident.lean:2289: /-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append_other`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.offsetLine`). -/
ConcreteSpec/Proofs/Ident.lean:2300: example : (aSource ++ [9]).columnAt (aSource.length + 1) = aSource.columnAt aSource.length + 1 :=
ConcreteSpec/Proofs/Ident.lean:2303: /-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append_tab`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.tabStopFour`). -/
ConcreteSpec/Proofs/Ident.lean:2312: example : (aTab ++ [10]).columnAt (aTab.length + 1) = 1 :=
ConcreteSpec/Proofs/Ident.lean:2315: /-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aTab`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.noResetColumn`). -/
ConcreteSpec/Proofs/Ident.lean:2319: example : (aSource ++ [10]).lineAt (aSource.length + 1) =
ConcreteSpec/Proofs/Ident.lean:2320: aSource.lineAt aSource.length + if (10 : UInt8) = 10 then 1 else 0 :=
ConcreteSpec/Proofs/Ident.lean:2323: /-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.constantLine`). -/
ConcreteSpec/Proofs/Ident.lean:2333: example : (utf8Prefix ++ [169]).columnAt (utf8Prefix.length + 1) =
ConcreteSpec/Proofs/Ident.lean:2334: if (169 : UInt8) = 10 then 1 else utf8Prefix.columnAt utf8Prefix.length + 1 :=
ConcreteSpec/Proofs/Ident.lean:2337: /-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append`, case `ConcreteSpec.Core.Vacuity.utf8Prefix`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.scalarColumn`). -/
ConcreteSpec/Proofs/Ident.lean:2345: /-- Accepted: the tab span satisfies the `LocationOk` projection at a real in-bounds range. -/
ConcreteSpec/Proofs/Ident.lean:2346: example : tabSpan.LocationOk tabSource ↔ tabSpan.endOffset ≤ tabSource.length ∧
ConcreteSpec/Proofs/Ident.lean:2347: tabSpan.line = tabSource.lineAt tabSpan.startOffset ∧
ConcreteSpec/Proofs/Ident.lean:2348: tabSpan.col = tabSource.columnAt tabSpan.startOffset := Span.locationOk_iff tabSource tabSpan
ConcreteSpec/Proofs/Ident.lean:2350: /-- Refused: the same stored coordinates do not satisfy `LocationOk` for a source with a different
ConcreteSpec/Proofs/Ident.lean:2352: example : ¬ tabSpan.LocationOk otherTabSource ∧
ConcreteSpec/Proofs/Ident.lean:2354: tabSpan.line = otherTabSource.lineAt tabSpan.startOffset ∧
ConcreteSpec/Proofs/Ident.lean:2355: tabSpan.col = otherTabSource.columnAt tabSpan.startOffset) := by decide
ConcreteSpec/Proofs/Observe2.lean:72: AList.lookup sources entry.2.path = some bytes ∧ entry.2.LocationOk bytes := by
ConcreteSpec/Proofs/Observe2.lean:76: | some bytes => by_cases h : entry.2.LocationOk bytes <;> simp [h]
ConcreteSpec/Proofs/Observe2.lean:83: AList.lookup sources entry.2.path = some bytes ∧ entry.2.LocationOk bytes := by
ConcreteSpec/Proofs/Observe2.lean:424: sp.PathOk ∧ AList.lookup project.sources sp.path = some bytes ∧ sp.LocationOk bytes ∧
ConcreteSpec/Proofs/Observe2.lean:555: exact (by decide : ¬ badSpan.LocationOk tabSource) loc
ConcreteSpec/Proofs/Observe2.lean:642: exact (by decide : ¬ badSpan.LocationOk tabSource) loc
ConcreteSpec/Proofs/Observe2.lean:706: sp.PathOk ∧ AList.lookup project.sources sp.path = some bytes ∧ sp.LocationOk bytes ∧
ConcreteSpec/Proofs/Observe2.lean:716: sp.PathOk ∧ AList.lookup project.sources sp.path = some bytes ∧ sp.LocationOk bytes ∧
ConcreteSpec/Proofs/Observe2.lean:1517: sp.line = source.lineAt sp.startOffset ∧
ConcreteSpec/Proofs/Observe2.lean:1518: sp.col = source.columnAt sp.startOffset ∧
ConcreteSpec/Proofs/Observe2.lean:1557: source.lineAt sp.startOffset, source.columnAt sp.startOffset)
ConcreteSpec/Proofs/Observe2.lean:1616: /-- Law (subject `ConcreteSpec.Core.Observe.byteColumn`).
ConcreteSpec/Proofs/Observe2.lean:1618: theorem byteColumn_tab (before : String) : byteColumn (before ++ "\t") = byteColumn before + 1 := by
ConcreteSpec/Proofs/Observe2.lean:1620: simp [byteColumn, String.utf8ByteSize_append, htab, Nat.add_assoc]
ConcreteSpec/Proofs/ObserveEvidence.lean:116: def doubleTabColumn (text : String) : Nat := byteColumn text + text.toList.count '\t'
ConcreteSpec/Proofs/ObserveEvidence.lean:119: example : byteColumn ("é" ++ "\t") = byteColumn "é" + 1 := byteColumn_tab "é"
ConcreteSpec/Proofs/ObserveEvidence.lean:122: subject `ConcreteSpec.Core.Observe.byteColumn`, mutant `ConcreteSpec.Core.Observe.Evidence.doubleTabColumn`). -/
ConcreteSpec/Proofs/ObserveOutput.lean:196: def trapSpan : Span := sourceSpan mainSourcePath 1 (by decide) "a\té"
ConcreteSpec/Proofs/ObserveReport.lean:306: sp.endOffset ≤ bytes.length ∧ sp.line = bytes.lineAt sp.startOffset ∧
ConcreteSpec/Proofs/ObserveReport.lean:307: sp.col = bytes.columnAt sp.startOffset ∧
ConcreteSpec/Proofs/ObserveReport.lean:750: sp.endOffset ≤ bytes.length ∧ sp.line = bytes.lineAt sp.startOffset ∧
ConcreteSpec/Proofs/ObserveReport.lean:751: sp.col = bytes.columnAt sp.startOffset ∧
ConcreteSpec/Proofs/ObserveReport.lean:771: sp.endOffset ≤ bytes.length ∧ sp.line = bytes.lineAt sp.startOffset ∧
ConcreteSpec/Proofs/ObserveReport.lean:772: sp.col = bytes.columnAt sp.startOffset ∧
ConcreteSpec/Proofs/ObserveReport.lean:801: sp.endOffset ≤ bytes.length ∧ sp.line = bytes.lineAt sp.startOffset ∧
ConcreteSpec/Proofs/ObserveReport.lean:802: sp.col = bytes.columnAt sp.startOffset ∧
ConcreteSpec/Proofs/ObserveSource.lean:14: /-- Law (subject `ConcreteSpec.Core.Observe.sourceSpan`).
ConcreteSpec/Proofs/ObserveSource.lean:21: (trapLine kind site (sourceSpan path line linePositive before) =
ConcreteSpec/Proofs/ObserveSource.lean:25: simp [sourceSpan, eq_comm]
ConcreteSpec/Proofs/ObserveSource.lean:33: example : (trapLine .overflow ⟨7⟩ (sourceSpan mainSourcePath 1 (by decide) "a\té") =
ConcreteSpec/Proofs/ObserveSource.lean:39: subject `ConcreteSpec.Core.Observe.sourceSpan`, mutant `ConcreteSpec.Core.Observe.scalarColumnSpan`). -/
ConcreteSpec/Proofs/ObserveSource.lean:46: example : (trapLine .overflow ⟨7⟩ (sourceSpan mainSourcePath 1 (by decide) "a\té") =
ConcreteSpec/Proofs/ObserveSource.lean:52: subject `ConcreteSpec.Core.Observe.sourceSpan`, mutant `ConcreteSpec.Core.Observe.scalarColumnSpan`). -/
ConcreteSpec/Proofs/ObserveSource.lean:63: scanSourcePrefix (cs ++ rest) newlines current =
ConcreteSpec/Proofs/ObserveSource.lean:64: scanSourcePrefix rest newlines (current ++ cs) := by
ConcreteSpec/Proofs/ObserveSource.lean:70: simpa [scanSourcePrefix, hc, List.append_assoc] using ih newlines (current ++ [c]) hcs
ConcreteSpec/Proofs/ObserveSource.lean:72: /-- Law (subject `ConcreteSpec.Core.Observe.sourcePrefixSpan`).
ConcreteSpec/Proofs/ObserveSource.lean:80: sourcePrefixSpan path (firstLine ++ "\n" ++ before) =
ConcreteSpec/Proofs/ObserveSource.lean:81: { sourceSpan path 2 (by decide) before with
ConcreteSpec/Proofs/ObserveSource.lean:90: have step2 : scanSourcePrefix ('\n' :: before.toList) 0 firstLine.toList =
ConcreteSpec/Proofs/ObserveSource.lean:91: scanSourcePrefix before.toList 1 [] := rfl
ConcreteSpec/Proofs/ObserveSource.lean:94: have hscan : scanSourcePrefix (firstLine ++ "\n" ++ before).toList 0 [] = (1, before.toList) := by
ConcreteSpec/Proofs/ObserveSource.lean:97: simp only [sourcePrefixSpan, sourceSpan, hscan, String.ofList_toList]
ConcreteSpec/Proofs/ObserveSource.lean:102: let scanned := scanSourcePrefix prefixText.toList 0 []
ConcreteSpec/Proofs/ObserveSource.lean:110: sourceSpan path 1 (by decide) prefixText
ConcreteSpec/Proofs/ObserveSource.lean:113: example : sourcePrefixSpan mainSourcePath ("let x = 1;" ++ "\n" ++ "a\té") =
ConcreteSpec/Proofs/ObserveSource.lean:114: { sourceSpan mainSourcePath 2 (by decide) "a\té" with
ConcreteSpec/Proofs/ObserveSource.lean:122: subject `ConcreteSpec.Core.Observe.sourcePrefixSpan`, mutant `ConcreteSpec.Core.Observe.scalarPrefixSpan`). -/
ConcreteSpec/Proofs/ObserveSource.lean:125: { sourceSpan mainSourcePath 2 (by decide) "a\té" with
ConcreteSpec/Proofs/ObserveSource.lean:132: subject `ConcreteSpec.Core.Observe.sourcePrefixSpan`, mutant `ConcreteSpec.Core.Observe.singleLinePrefixSpan`). -/
ConcreteSpec/Proofs/ObserveSource.lean:135: { sourceSpan mainSourcePath 2 (by decide) "a\té" with
ConcreteSpec/Proofs/ObserveSource.lean:187: /-- Law (subject `ConcreteSpec.Core.Observe.sourceSpan`).
ConcreteSpec/Proofs/ObserveSource.lean:193: (sourceSpan path line positive before).col = before.toList.length + 1 := by
ConcreteSpec/Proofs/ObserveSource.lean:195: simpa [sourceSpan] using bytes
ConcreteSpec/Proofs/ObserveSource.lean:197: /-- Law (subject `ConcreteSpec.Core.Observe.sourceSpan`).
ConcreteSpec/Proofs/ObserveSource.lean:202: before.toList.length + 1 < (sourceSpan path line positive before).col := by
ConcreteSpec/Proofs/ObserveSource.lean:211: let sp := sourceSpan path line positive before
ConcreteSpec/Proofs/ObserveSource.lean:216: example : (sourceSpan mainSourcePath 1 (by decide) "a\tb").col =
ConcreteSpec/Proofs/ObserveSource.lean:221: subject `ConcreteSpec.Core.Observe.sourceSpan`, mutant `ConcreteSpec.Core.Observe.expandedTabSpan`). -/
ConcreteSpec/Proofs/ObserveSource.lean:229: (sourceSpan mainSourcePath 1 (by decide) "a\té").col :=
ConcreteSpec/Proofs/ObserveSource.lean:233: subject `ConcreteSpec.Core.Observe.sourceSpan`, mutant `ConcreteSpec.Core.Observe.scalarColumnSpan`). -/
ConcreteSpec/Proofs/ObserveSource.lean:244: Span.HasByteColumn (sourcePrefixSpan path before) before := by
ConcreteSpec/Proofs/ObserveSource.lean:247: simp [sourcePrefixSpan, scan, scanSourcePrefix, Span.HasByteColumn, sourceSpan, byteColumn]
ConcreteSpec/README.md:79: unrendered byte ranges. `Observe.sourceSpan` constructs byte columns from source prefixes; the operational
ConcreteSpec/Spec/Ident.lean:695: def SourceText.lineAt (source : SourceText) (offset : Nat) : Nat :=
ConcreteSpec/Spec/Ident.lean:701: def SourceText.columnAt (source : SourceText) (offset : Nat) : Nat :=
ConcreteSpec/Spec/Ident.lean:797: for diagnostics and rendering, while `Span.LocationOk` checks them against the actual source bytes.
ConcreteSpec/Spec/Ident.lean:812: def Span.LocationOk (sp : Span) (source : SourceText) : Prop :=
ConcreteSpec/Spec/Ident.lean:813: sp.endOffset ≤ source.length ∧ sp.line = source.lineAt sp.startOffset ∧
ConcreteSpec/Spec/Ident.lean:814: sp.col = source.columnAt sp.startOffset
ConcreteSpec/Spec/Ident.lean:816: instance (sp : Span) (source : SourceText) : Decidable (sp.LocationOk source) := by
ConcreteSpec/Spec/Ident.lean:817: unfold Span.LocationOk
ConcreteSpec/Spec/Ident.lean:920: example : tabSpan.LocationOk tabSource := by decide
ConcreteSpec/Spec/Ident.lean:923: example : ¬ ({ tabSpan with col := 2, col_pos := by decide }.LocationOk tabSource) := by decide
ConcreteSpec/Spec/Observe.lean:169: ∃ source, AList.lookup sources sp.path = some source ∧ sp.LocationOk source
ConcreteSpec/Spec/Observe.lean:223: | some bytes => if entry.2.LocationOk bytes then none
ConcreteSpec/Spec/Observe.lean:256: AList.lookup project.sources entry.2.path = some bytes ∧ entry.2.LocationOk bytes
ConcreteSpec/Spec/Observe.lean:278: def byteColumn (before : String) : Nat := before.utf8ByteSize + 1
ConcreteSpec/Spec/Observe.lean:282: `sourcePrefixSpan` for a whole-file offset. Only the project loader admits a candidate for reporting. -/
ConcreteSpec/Spec/Observe.lean:283: def sourceSpan (path : SourcePath) (line : Nat) (linePositive : 0 < line)
ConcreteSpec/Spec/Observe.lean:289: def scanSourcePrefix : List Char → Nat → List Char → Nat × List Char
ConcreteSpec/Spec/Observe.lean:292: if c = '\n' then scanSourcePrefix cs (newlines + 1) []
ConcreteSpec/Spec/Observe.lean:293: else scanSourcePrefix cs newlines (current ++ [c])
ConcreteSpec/Spec/Observe.lean:298: 1-based UTF-8 byte column (`byteColumn`), with a tab counting one byte. -/
ConcreteSpec/Spec/Observe.lean:299: def sourcePrefixSpan (path : SourcePath) (prefixText : String) : Span :=
ConcreteSpec/Spec/Observe.lean:300: let scanned := scanSourcePrefix prefixText.toList 0 []
ConcreteSpec/Spec/Observe.lean:301: { sourceSpan path (scanned.1 + 1) (Nat.zero_lt_succ _) (String.ofList scanned.2) with
ConcreteSpec/Spec/Observe.lean:307: def Span.HasByteColumn (sp : Span) (before : String) : Prop := sp.col = byteColumn before
ConcreteSpec/Spec/Observe.lean:791: exact (by decide : ¬ badLocationSpan.LocationOk tabSource) location
ConcreteSpec/Spec/Observe.lean:809: example : byteColumn "a\té" = 5 := by decide
ConcreteSpec/Spec/Observe.lean:811: example : Span.HasByteColumn
scripts/proofs/axioms.golden:1935: ConcreteSpec.Core.Observe.byteColumn.eq_1
```

Enclosing declarations, including every directly affected existing theorem and example (names are original source headers; continuation lines remain in the source):

```text
ConcreteSpec/Proofs/Ident.lean:1236: theorem LoadedModules.load_admitted (modules admitted : List LoadedModule) :
ConcreteSpec/Proofs/Ident.lean:1257: theorem SourceText.lineAt_append (source : SourceText) (byte : UInt8) :
ConcreteSpec/Proofs/Ident.lean:1267: theorem SourceText.lineAt_append_newline (source : SourceText) :
ConcreteSpec/Proofs/Ident.lean:1273: theorem SourceText.lineAt_append_other (source : SourceText) (byte : UInt8) (other : byte ≠ 10) :
ConcreteSpec/Proofs/Ident.lean:1279: theorem SourceText.columnAt_append (source : SourceText) (byte : UInt8) :
ConcreteSpec/Proofs/Ident.lean:1290: theorem SourceText.columnAt_append_tab (source : SourceText) :
ConcreteSpec/Proofs/Ident.lean:1296: theorem SourceText.columnAt_append_newline (source : SourceText) :
ConcreteSpec/Proofs/Ident.lean:1302: theorem Span.locationOk_iff (source : SourceText) (sp : Span) :
ConcreteSpec/Proofs/Ident.lean:2278: example : (aSource ++ [10]).lineAt (aSource.length + 1) = aSource.lineAt aSource.length + 1 :=
ConcreteSpec/Proofs/Ident.lean:2286: example : (aSource ++ [9]).lineAt (aSource.length + 1) = aSource.lineAt aSource.length :=
ConcreteSpec/Proofs/Ident.lean:2300: example : (aSource ++ [9]).columnAt (aSource.length + 1) = aSource.columnAt aSource.length + 1 :=
ConcreteSpec/Proofs/Ident.lean:2312: example : (aTab ++ [10]).columnAt (aTab.length + 1) = 1 :=
ConcreteSpec/Proofs/Ident.lean:2319: example : (aSource ++ [10]).lineAt (aSource.length + 1) =
ConcreteSpec/Proofs/Ident.lean:2333: example : (utf8Prefix ++ [169]).columnAt (utf8Prefix.length + 1) =
ConcreteSpec/Proofs/Ident.lean:2343: def wrongTabSpan : Span := { tabSpan with col := 2, col_pos := by decide }
ConcreteSpec/Proofs/Ident.lean:2346: example : tabSpan.LocationOk tabSource ↔ tabSpan.endOffset ≤ tabSource.length ∧
ConcreteSpec/Proofs/Ident.lean:2352: example : ¬ tabSpan.LocationOk otherTabSource ∧
ConcreteSpec/Proofs/Observe2.lean:70: theorem LoadedProject.siteIssue_none (sources : SourceTable) (entry : SiteId × Span) :
ConcreteSpec/Proofs/Observe2.lean:80: theorem LoadedProject.findSiteIssue_none (sources : SourceTable) (sites : SiteTable) :
ConcreteSpec/Proofs/Observe2.lean:418: theorem LoadedProject.load_trap_resolves (input : ProjectSources) (kind : TrapKind) :
ConcreteSpec/Proofs/Observe2.lean:545: theorem badLocation_invalid : ¬ ∃ project, badLocation.Realizes project := by
ConcreteSpec/Proofs/Observe2.lean:633: example : ¬ (match ignoreLocation badLocation with
ConcreteSpec/Proofs/Observe2.lean:701: example : match LoadedProject.load good with
ConcreteSpec/Proofs/Observe2.lean:711: example : ¬ (match loseSites good with
ConcreteSpec/Proofs/Observe2.lean:1511: theorem trapTerminal_valid_location_inv (sources : SourceTable) (sites : SiteTable)
ConcreteSpec/Proofs/Observe2.lean:1550: def SourceTrapObservation (sources : SourceTable) (sites : SiteTable) (kind : TrapKind)
ConcreteSpec/Proofs/Observe2.lean:1609: example : SiteTable.Valid validSources validSites ∧
ConcreteSpec/Proofs/Observe2.lean:1618: theorem byteColumn_tab (before : String) : byteColumn (before ++ "\t") = byteColumn before + 1 := by
ConcreteSpec/Proofs/ObserveEvidence.lean:116: def doubleTabColumn (text : String) : Nat := byteColumn text + text.toList.count '\t'
ConcreteSpec/Proofs/ObserveEvidence.lean:119: example : byteColumn ("é" ++ "\t") = byteColumn "é" + 1 := byteColumn_tab "é"
ConcreteSpec/Proofs/ObserveOutput.lean:196: def trapSpan : Span := sourceSpan mainSourcePath 1 (by decide) "a\té"
ConcreteSpec/Proofs/ObserveReport.lean:293: theorem runFrom_source_trap {O : Exec.Operands} {S : EventSignature}
ConcreteSpec/Proofs/ObserveReport.lean:743: example : ∃ sp bytes,
ConcreteSpec/Proofs/ObserveReport.lean:758: example : ([Demo.site] = List.map Prod.fst sourceAdapter.sourceInput.sites) ∧
ConcreteSpec/Proofs/ObserveReport.lean:788: example : ([Demo.site] = List.map Prod.fst sourceAdapter.sourceInput.sites) ∧
ConcreteSpec/Proofs/ObserveSource.lean:1: (module documentation or namespace context)
ConcreteSpec/Proofs/ObserveSource.lean:18: theorem source_trap_column (path : SourcePath) (line : Nat)
ConcreteSpec/Proofs/ObserveSource.lean:33: example : (trapLine .overflow ⟨7⟩ (sourceSpan mainSourcePath 1 (by decide) "a\té") =
ConcreteSpec/Proofs/ObserveSource.lean:46: example : (trapLine .overflow ⟨7⟩ (sourceSpan mainSourcePath 1 (by decide) "a\té") =
ConcreteSpec/Proofs/ObserveSource.lean:61: theorem scanSourcePrefix_no_newline (cs rest : List Char) (newlines : Nat)
ConcreteSpec/Proofs/ObserveSource.lean:76: theorem sourcePrefixSpan_second_line (path : SourcePath)
ConcreteSpec/Proofs/ObserveSource.lean:101: def scalarPrefixSpan (path : SourcePath) (prefixText : String) : Span :=
ConcreteSpec/Proofs/ObserveSource.lean:109: def singleLinePrefixSpan (path : SourcePath) (prefixText : String) : Span :=
ConcreteSpec/Proofs/ObserveSource.lean:113: example : sourcePrefixSpan mainSourcePath ("let x = 1;" ++ "\n" ++ "a\té") =
ConcreteSpec/Proofs/ObserveSource.lean:123: example : ('\n' ∉ "let x = 1;".toList) ∧ ('\n' ∉ "a\té".toList) ∧
ConcreteSpec/Proofs/ObserveSource.lean:133: example : ('\n' ∉ "let x = 1;".toList) ∧ ('\n' ∉ "a\té".toList) ∧
ConcreteSpec/Proofs/ObserveSource.lean:166: theorem source_multibyte_bytes (cs : List Char) (multi : ∃ c ∈ cs, 127 < c.val) :
ConcreteSpec/Proofs/ObserveSource.lean:190: theorem sourceSpan_ascii_eq_char_col (path : SourcePath)
ConcreteSpec/Proofs/ObserveSource.lean:199: theorem sourceSpan_multibyte_gt_char_col (path : SourcePath)
ConcreteSpec/Proofs/ObserveSource.lean:209: def expandedTabSpan (path : SourcePath) (line : Nat) (positive : 0 < line)
ConcreteSpec/Proofs/ObserveSource.lean:216: example : (sourceSpan mainSourcePath 1 (by decide) "a\tb").col =
ConcreteSpec/Proofs/ObserveSource.lean:228: example : "a\té".toList.length + 1 <
ConcreteSpec/Proofs/ObserveSource.lean:242: theorem sourcePrefixSpan_hasByteColumn (path : SourcePath) (before : String)
ConcreteSpec/README.md:1: (module documentation or namespace context)
ConcreteSpec/Spec/Ident.lean:695: def SourceText.lineAt (source : SourceText) (offset : Nat) : Nat :=
ConcreteSpec/Spec/Ident.lean:701: def SourceText.columnAt (source : SourceText) (offset : Nat) : Nat :=
ConcreteSpec/Spec/Ident.lean:791: def mainSourcePath : SourcePath :=
ConcreteSpec/Spec/Ident.lean:812: def Span.LocationOk (sp : Span) (source : SourceText) : Prop :=
ConcreteSpec/Spec/Ident.lean:816: instance (sp : Span) (source : SourceText) : Decidable (sp.LocationOk source) := by
ConcreteSpec/Spec/Ident.lean:920: example : tabSpan.LocationOk tabSource := by decide
ConcreteSpec/Spec/Ident.lean:923: example : ¬ ({ tabSpan with col := 2, col_pos := by decide }.LocationOk tabSource) := by decide
ConcreteSpec/Spec/Observe.lean:167: def SiteTable.Valid (sources : SourceTable) (sites : SiteTable) : Prop :=
ConcreteSpec/Spec/Observe.lean:219: def LoadedProject.siteIssue (sources : SourceTable) (entry : SiteId × Span) :
ConcreteSpec/Spec/Observe.lean:251: def ProjectSources.Realizes (input : ProjectSources) (project : LoadedProject) : Prop :=
ConcreteSpec/Spec/Observe.lean:278: def byteColumn (before : String) : Nat := before.utf8ByteSize + 1
ConcreteSpec/Spec/Observe.lean:283: def sourceSpan (path : SourcePath) (line : Nat) (linePositive : 0 < line)
ConcreteSpec/Spec/Observe.lean:289: def scanSourcePrefix : List Char → Nat → List Char → Nat × List Char
ConcreteSpec/Spec/Observe.lean:299: def sourcePrefixSpan (path : SourcePath) (prefixText : String) : Span :=
ConcreteSpec/Spec/Observe.lean:307: def Span.HasByteColumn (sp : Span) (before : String) : Prop := sp.col = byteColumn before
ConcreteSpec/Spec/Observe.lean:786: example : ¬ SiteTable.Valid locationSources [(site, badLocationSpan)] := by
ConcreteSpec/Spec/Observe.lean:809: example : byteColumn "a\té" = 5 := by decide
ConcreteSpec/Spec/Observe.lean:811: example : Span.HasByteColumn
scripts/proofs/axioms.golden:1: (module documentation or namespace context)
```
