import ConcreteSpec.Spec.TableVerifier
import ConcreteSpec.Spec.GrammarData

open ConcreteSpec.GrammarVerification ConcreteSpec.GrammarData
open Concrete.LL1

private def catalog : RawGrammar :=
  ⟨Nonterminal.count, ConcreteSpec.Surface0.Terminal.count, numberedGrammar productionIds⟩

-- The real catalog must fit by kernel reduction of the input dimensions alone.
-- The quantified control also rules out hidden tests on the generated program.
example : representable catalog := by decide +kernel
example (g : RawGrammar) (shape : g.valid) (words : g.layout.scratch+1 < UInt64.size)
    (indices : g.cfg.prods.length < UInt64.size) : representable g := by
  simp only [representable, shape, words, indices, decide_true, Bool.and_self]

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

private def proposal (g : RawGrammar) : IO (Array UInt64) := do
  let result ← IO.Process.output
    {cmd := "python3", args := #["-I", "scripts/spec/grammar_proposal.py"]} (some (exportRows g))
  unless result.exitCode == 0 do throw (IO.userError result.stderr)
  let words := result.stdout.trimAscii.toString.splitOn " "
  let some numbers := words.mapM String.toNat? | throw (IO.userError "invalid proposal encoding")
  unless numbers.all (· < UInt64.size) do throw (IO.userError "word exceeds UInt64")
  return numbers.toArray.map Nat.toUInt64

private def exercise (label : String) (g : RawGrammar) : IO (Array UInt64) := do
  let mem ← proposal g
  let v := compileTableVerifier g
  unless v.accepts mem g.budget do throw (IO.userError s!"valid {label} certificate rejected")
  IO.println s!"{label}: {g.cfg.prods.length} rows, {mem.size} words, {v.program.size} instructions"
  return mem

def main (args : List String) : IO Unit := do
  if args == ["--export"] then IO.print (exportRows catalog); return
  let full ← exercise "early grammar" catalog
  let fullVerifier := compileTableVerifier catalog
  let fnCell := catalog.layout.cell Nonterminal.program.ctorIdx (ConcreteSpec.Surface0.Terminal.fn.ctorIdx+1)
  unless full[fnCell]? == some 1 && !fullVerifier.accepts (full.set! fnCell 0) catalog.budget do
    throw (IO.userError "full grammar lost the fn prediction or accepted its deletion")
  unless full == (← proposal catalog) do throw (IO.userError "producer is not reproducible")
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
  unless decodeRank l mem rankIndex == some mem[l.rank 1]!.toNat do
    throw (IO.userError "decoder lost the accepted nonterminal rank")
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
  let some decoded := decodeCells l mem | throw (IO.userError "failed to decode accepted table")
  unless decoded.size == l.n*l.c && decoded[l.cell 1 1]? == some (some 1) &&
      decoded[l.cell 1 0]? == some none && decoded != decoded.set! (l.cell 1 1) (some 0) do
    throw (IO.userError "decoder lost the accepted row-major prefix")
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
