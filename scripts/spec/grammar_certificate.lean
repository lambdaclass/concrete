import ConcreteSpec.Spec.TableVerifier
import ConcreteSpec.Spec.GrammarData
import ConcreteSpec.Spec.EarlyProposal
import ConcreteSpec.Proofs.EarlyProposal

open ConcreteSpec.GrammarVerification ConcreteSpec.GrammarData
open Concrete.LL1

private def catalog : RawGrammar := earlyRawGrammar

-- The real catalog must fit by kernel reduction of the input dimensions alone.
-- The quantified control also rules out hidden tests on the generated program.
example : representable catalog := earlyRawGrammar_representable
example (g : RawGrammar) (shape : g.valid) (words : g.layout.scratch+1 < UInt64.size)
    (indices : g.cfg.prods.length < UInt64.size) : representable g := by
  simp only [representable, shape, words, indices, decide_true, Bool.and_self]

-- The production certificate is proved structurally in ConcreteSpec.Proofs.EarlyProposal,
-- never by kernel reduction of the full verifyRun execution.
example : ValidCertificate earlyRawGrammar (fun a => earlyProposal.getD a 0) := earlyProposal_valid
example : (compileTableVerifier earlyRawGrammar).accepts earlyProposal earlyRawGrammar.budget = true :=
  earlyProposal_accepted

/-- Small independently calculated proposals exercise the kernel's reduction of
the same word machine used by the executable controls. Require the right length
so an accidental change to the layout cannot turn a negative control green. -/
private def kernelVerdict (g : RawGrammar) (words : Array UInt64) : Option Bool := do
  let v := compileTableVerifier g
  guard (words.size == v.words)
  (ConcreteSpec.StageGrammar.verifyRun v.program ⟨0,words⟩ g.budget).2

-- S → ε. The first word selects production 0 on EOF.
example : kernelVerdict ⟨1,0,0,[(0,[])]⟩ #[1,1,2,2,0,1,1,1,1,0,0] = some true := by decide +kernel
-- S → S. The nullable-prefix edge would require rank(S) < rank(S).
example : kernelVerdict ⟨1,0,0,[(0,[.n 0])]⟩
    #[0,0,0,0,0,1,1,0,0,1,0,1,0,0,0] = some false := by decide +kernel
-- S → t S has a FIRST set and no finite terminal yield.
example : kernelVerdict ⟨1,1,0,[(0,[.t 0,.n 0])]⟩
    #[0,1,1,0,0,0,2,1,0,1,0,0,0,0,1,0,0,1,0,1,0,3,0,0,0] = some false := by decide +kernel
-- S → A t, A → ε | t. Selecting the empty alternative on t omits the other prediction.
example : kernelVerdict ⟨2,1,0,[(0,[.n 1,.t 0]),(1,[]),(1,[.t 0])]⟩
    #[0,1,0,2,2,1,0,2,4,2,0,4,0,2,1,0,0,2,1,3,0,1,1,0,0,0,1,1,0,1,3,2,1,1,2,1,0,3,0,1,0,0,0,0,0,1,0,0,0]
      = some false := by decide +kernel

private def exportRows (g : RawGrammar) : String :=
  let lines := g.cfg.prods.map fun (lhs, rhs) =>
    String.intercalate " " (toString lhs :: rhs.map fun | .t t => s!"t{t}" | .n n => s!"n{n}")
  String.intercalate "\n" (s!"{g.nonterminals} {g.terminals} {g.cfg.start}" :: lines) ++ "\n"

private def rawProposal (g : RawGrammar) : IO String := do
  let result ← IO.Process.output
    {cmd := "python3", args := #["-I", "scripts/spec/grammar_proposal.py"]} (some (exportRows g))
  unless result.exitCode == 0 do throw (IO.userError result.stderr)
  return result.stdout

private def encodeProposal (words : Array UInt64) : String :=
  String.intercalate " " (words.toList.map fun w => toString w.toNat) ++ "\n"

private def proposal (g : RawGrammar) : IO (Array UInt64) := do
  let stdout ← rawProposal g
  let words := stdout.trimAscii.toString.splitOn " "
  let some numbers := words.mapM String.toNat? | throw (IO.userError "invalid proposal encoding")
  unless numbers.all (· < UInt64.size) do throw (IO.userError "word exceeds UInt64")
  return numbers.toArray.map Nat.toUInt64

private def exercise (label : String) (g : RawGrammar) : IO (Array UInt64) := do
  let mem ← proposal g
  let v := compileTableVerifier g
  unless v.accepts mem g.budget do throw (IO.userError s!"valid {label} certificate rejected")
  IO.println s!"{label}: {g.cfg.prods.length} rows, {mem.size} words, {v.program.size} instructions"
  return mem

/-- Independently extract the expected cell table and nonterminal ranks directly
from the independent producer's accepted word vector, without using `decodeCells`
or `decodeRank`. -/
private def expectedTableFromProducer (l : Layout) (producerWords : Array UInt64) :
    Option (Array (Option Nat) × Array Nat) := do
  guard (producerWords.size == l.scratch + 1)
  let mut cells : Array (Option Nat) := Array.mkEmpty (l.n * l.c)
  for n in [:l.n] do
    for col in [:l.c] do
      let w ← producerWords[l.cell n col]?
      cells := cells.push (if w == 0 then none else some (w.toNat - 1))
  let mut ranks : Array Nat := Array.mkEmpty l.n
  for n in [:l.n] do
    let w ← producerWords[l.rank n]?
    ranks := ranks.push w.toNat
  return (cells, ranks)

/-- Decode every nonterminal rank through `decodeRank`. -/
private def decodeAllRanks (l : Layout) (memory : Array UInt64) : Option (Array Nat) :=
  ((List.finRange l.n).mapM (decodeRank l memory)).map List.toArray

/-- Compare EVERY decoded cell `(n, col)` and EVERY decoded nonterminal rank `n`
against independently stated expected values. -/
private def matchesExpectedTable (l : Layout) (expectedCells : Array (Option Nat))
    (expectedRanks : Array Nat) (candidateCells : Array (Option Nat))
    (candidateRanks : Array Nat) : Bool :=
  candidateCells.size == l.n * l.c &&
  expectedCells.size == l.n * l.c &&
  candidateRanks.size == l.n &&
  expectedRanks.size == l.n &&
  (List.range l.n).all fun n =>
    candidateRanks[n]? == expectedRanks[n]? &&
    (List.range l.c).all fun col =>
      let addr := l.cell n col
      candidateCells[addr]? == expectedCells[addr]?

def main (args : List String) : IO Unit := do
  if args == ["--export"] then IO.print (exportRows catalog); return
  -- Byte-exact reproducibility: regenerate twice from scripts/spec/grammar_proposal.py
  -- and confirm both match ConcreteSpec.Spec.EarlyProposal.earlyProposal identically.
  let run1 ← rawProposal catalog
  let run2 ← rawProposal catalog
  unless run1 == run2 do
    throw (IO.userError "producer is not reproducible across two invocations")
  let encoded := encodeProposal earlyProposal
  unless run1.toUTF8 == encoded.toUTF8 do
    throw (IO.userError "stored earlyProposal differs byte-for-byte from scripts/spec/grammar_proposal.py")
  let full ← exercise "early grammar" catalog
  unless full == earlyProposal do
    throw (IO.userError "decoded proposal differs from stored earlyProposal")
  let fullVerifier := compileTableVerifier catalog
  let (fullSteps, fullVerdict) :=
    ConcreteSpec.StageGrammar.verifyRun fullVerifier.program ⟨0, earlyProposal⟩ catalog.budget
  unless earlyProposal.size == fullVerifier.words && fullVerdict == some true &&
      catalog.positions ≤ fullSteps && fullSteps ≤ catalog.budget &&
      fullVerifier.accepts earlyProposal catalog.budget do
    throw (IO.userError "compiled verifyRun rejected stored earlyProposal or violated step bounds")
  let fl := catalog.layout
  unless fullVerifier.accepts (earlyProposal.set! fl.scratch 0xffffffffffffffff) catalog.budget do
    throw (IO.userError "initial scratch word changed full earlyProposal acceptance")
  -- Single-word cell, rank, nullability, productivity and unfounded-fact mutants on earlyProposal.
  let fnCell := fl.cell Nonterminal.program.ctorIdx (ConcreteSpec.Surface0.Terminal.fn.ctorIdx + 1)
  let eofProgramCell := fl.cell Nonterminal.program.ctorIdx 0
  let retProgramCell := fl.cell Nonterminal.program.ctorIdx (ConcreteSpec.Surface0.Terminal.return_.ctorIdx + 1)
  let letBodyCell := fl.cell Nonterminal.body.ctorIdx (ConcreteSpec.Surface0.Terminal.let_.ctorIdx + 1)
  let rankProg := fl.rank Nonterminal.program.ctorIdx
  let rankStructs := fl.rank Nonterminal.structs.ctorIdx
  unless earlyProposal[fnCell]? == some 1 do
    throw (IO.userError "full grammar lost the fn prediction")
  for (label, addr, word) in [
      ("full missing cell", fnCell, (0 : UInt64)),
      ("full spurious EOF cell", eofProgramCell, 1),
      ("full spurious non-EOF cell", retProgramCell, 1),
      ("full wrong-index cell (different lhs)", fnCell, 2),
      ("full wrong-index cell (same lhs)", letBodyCell, (ProductionId.retStmt.ctorIdx + 1).toUInt64),
      ("full out-of-range cell index", fnCell, 0xffffffffffffffff),
      ("full rank zero", rankProg, 0),
      ("full rank non-decreasing", rankProg, earlyProposal[rankStructs]!),
      ("full missing nullability", fl.nullNt Nonterminal.structs.ctorIdx, 0),
      ("full unfounded nullability", fl.nullNt Nonterminal.program.ctorIdx, 1),
      ("full missing productivity (program)", fl.productive Nonterminal.program.ctorIdx, 0),
      ("full missing productivity (atom)", fl.productive Nonterminal.atom.ctorIdx, 0),
      ("full unfounded FIRST", fl.firstNt Nonterminal.program.ctorIdx (ConcreteSpec.Surface0.Terminal.return_.ctorIdx + 1), 1),
      ("full missing FOLLOW seed", fl.follow Nonterminal.program.ctorIdx 0, 0),
      ("full unfounded FOLLOW", fl.follow Nonterminal.program.ctorIdx (ConcreteSpec.Surface0.Terminal.fn.ctorIdx + 1), 1)] do
    unless addr < earlyProposal.size && earlyProposal[addr]! != word &&
        !fullVerifier.accepts (earlyProposal.set! addr word) catalog.budget do
      throw (IO.userError s!"{label} mutation escaped")
  -- Wrong-length controls and full decoded-table/rank identity checks against the
  -- independent producer's accepted word vector (`full`).
  let progIdx : Fin fl.n := ⟨Nonterminal.program.ctorIdx, by decide⟩
  for wrongSize in [earlyProposal.pop, earlyProposal.push 0] do
    unless !fullVerifier.accepts wrongSize catalog.budget &&
        (decodeCells fl wrongSize).isNone && (decodeRank fl wrongSize progIdx).isNone do
      throw (IO.userError "wrong full certificate dimensions accepted")
  let some (expectedFullCells, expectedFullRanks) := expectedTableFromProducer fl full
    | throw (IO.userError "failed to derive expected cells/ranks from independent producer")
  let some fullDecoded := decodeCells fl earlyProposal
    | throw (IO.userError "failed to decode accepted full earlyProposal")
  let some fullRanks := decodeAllRanks fl earlyProposal
    | throw (IO.userError "failed to decode all accepted nonterminal ranks")
  unless matchesExpectedTable fl expectedFullCells expectedFullRanks fullDecoded fullRanks &&
      fullDecoded[fnCell]? == some (some ProductionId.program.ctorIdx) &&
      fullDecoded[eofProgramCell]? == some none &&
      fullDecoded[letBodyCell]? == some (some ProductionId.binding.ctorIdx) do
    throw (IO.userError "full decoded cell table or ranks differ from independent producer output")
  -- Negative table-identity controls: every mutation is tested against the independently
  -- expected producer output, never against a modified copy of the candidate array.
  let swappedBodyLet := fullDecoded.set! letBodyCell (some ProductionId.retStmt.ctorIdx)
  if matchesExpectedTable fl expectedFullCells expectedFullRanks swappedBodyLet fullRanks then
    throw (IO.userError "table identity control accepted swapping body/let cell to retStmt")
  let clearedFn := fullDecoded.set! fnCell none
  if matchesExpectedTable fl expectedFullCells expectedFullRanks clearedFn fullRanks then
    throw (IO.userError "table identity control accepted clearing program/fn cell")
  let spuriousEof := fullDecoded.set! eofProgramCell (some ProductionId.program.ctorIdx)
  if matchesExpectedTable fl expectedFullCells expectedFullRanks spuriousEof fullRanks then
    throw (IO.userError "table identity control accepted spurious program/EOF cell")
  let swappedRank := fullRanks.set! Nonterminal.program.ctorIdx
    fullRanks[Nonterminal.structs.ctorIdx]!
  if matchesExpectedTable fl expectedFullCells expectedFullRanks fullDecoded swappedRank then
    throw (IO.userError "table identity control accepted swapping program rank to structs rank")
  let bumpedRank := fullRanks.set! Nonterminal.program.ctorIdx
    (fullRanks[Nonterminal.program.ctorIdx]! + 1)
  if matchesExpectedTable fl expectedFullCells expectedFullRanks fullDecoded bumpedRank then
    throw (IO.userError "table identity control accepted incrementing program rank")
  -- S -> A t0 ; A -> epsilon | t1. The start is also nullable in the second toy.
  let g : RawGrammar := ⟨2, 2, 0, [(0,[.n 1,.t 0]), (1,[]), (1,[.t 1])]⟩
  let mem ← exercise "nullable prefix" g
  let v := compileTableVerifier g
  let l := g.layout
  for (label, addr, word) in [
      ("missing", l.cell 1 1, 0), ("spurious EOF", l.cell 1 0, 2),
      ("wrong index", l.cell 1 2, 1), ("out-of-range index", l.cell 1 2, 0xffffffffffffffff),
      ("wrong production with same lhs", l.cell 1 2, 2),
      ("unproductive", l.productive 1, 0), ("unfounded FIRST", l.firstNt 1 1, 1), ("prefix rank", l.rank 0, 0),
      ("FOLLOW seed", l.follow 0 0, 0)] do
    unless addr < mem.size && mem[addr]! != word && !v.accepts (mem.set! addr word) g.budget do
      throw (IO.userError s!"{label} mutation escaped")
  let rankIndex : Fin l.n := ⟨1, by decide⟩
  for wrongSize in [mem.pop, mem.push 0] do
    unless !v.accepts wrongSize g.budget && (decodeCells l wrongSize).isNone &&
        (decodeRank l wrongSize rankIndex).isNone do
      throw (IO.userError "wrong certificate dimensions accepted")
  unless v.accepts (mem.set! l.scratch 0xffffffffffffffff) g.budget do
    throw (IO.userError "initial scratch word changed acceptance")
  -- This FOLLOW bit is closed under the recursive occurrence and yields a
  -- conflict-free extra epsilon cell. Its only possible justification is itself.
  let recursive : RawGrammar := ⟨1, 2, 0, [(0,[.t 0,.n 0]), (0,[])]⟩
  let original ← exercise "right recursion" recursive
  let forged := (original.set! (recursive.layout.follow 0 2) 2).set! (recursive.layout.cell 0 2) 2
  let cert := compileTableVerifier recursive
  if cert.accepts forged recursive.budget then throw (IO.userError "self-supported FOLLOW escaped")
  let expectedToyCells : Array (Option Nat) := #[none, some 0, some 0, none, some 1, some 2]
  let expectedToyRanks : Array Nat := #[2, 1]
  let some (producerToyCells, producerToyRanks) := expectedTableFromProducer l mem
    | throw (IO.userError "failed to derive expected cells/ranks for toy grammar")
  let some decoded := decodeCells l mem | throw (IO.userError "failed to decode accepted table")
  let some decodedRanks := decodeAllRanks l mem | throw (IO.userError "failed to decode accepted ranks")
  unless producerToyCells == expectedToyCells && producerToyRanks == expectedToyRanks &&
      matchesExpectedTable l expectedToyCells expectedToyRanks decoded decodedRanks do
    throw (IO.userError "toy decoded cells or ranks differ from expected table")
  if matchesExpectedTable l expectedToyCells expectedToyRanks
      (decoded.set! (l.cell 1 1) (some 0)) decodedRanks then
    throw (IO.userError "toy table identity control accepted cell substitution")
  if matchesExpectedTable l expectedToyCells expectedToyRanks
      decoded (decodedRanks.set! 0 1) then
    throw (IO.userError "toy table identity control accepted rank substitution")
  let eps : RawGrammar := ⟨1, 1, 0, [(0,[])]⟩
  let empty ← exercise "physical EOF" eps
  unless !(compileTableVerifier eps).accepts (empty.set! 0 0) eps.budget do
    throw (IO.userError "missing EOF prediction escaped")
  let dup := {eps with cfg.prods := [(0,[]), (0,[])]}
  let duplicate := compileTableVerifier dup
  -- Both occurrences genuinely predict EOF. Fill every true certificate fact
  -- for the repeated empty production, then try each occurrence as the winner.
  let dl := dup.layout
  let mut m := Array.replicate duplicate.words (0 : UInt64)
  for p in [:2] do
    for addr in [dl.prefixNull p, dl.suffixNull p, dl.suffixProductive p] do
      m := m.set! addr 1
  for (addr,w) in [(dl.rank 0,1),(dl.nullNt 0,2),(dl.productive 0,2),(dl.follow 0 0,1)] do
    m := m.set! addr w
  for i in [1,2] do
    if duplicate.accepts (m.set! 0 i) dup.budget then throw (IO.userError "duplicate row escaped")
  let invalid := {g with cfg.start := g.nonterminals}
  unless !(compileTableVerifier invalid).accepts mem invalid.budget do
    throw (IO.userError "invalid nonterminal address escaped")
