import Concrete.Stages

open Concrete Concrete.Backend

private def fixturePath := "scaling.con"

-- Fixed-width names keep bytes per statement constant. Reusing v100000 from
-- every later statement exercises lookups of an older, still-live value.
private def letChain (n : Nat) : ByteArray := Id.run do
  let mut src := "fn main() -> u8 {\nlet v100000: u8 = 0 + 0;\n"
  for i in [1:n] do
    src := src ++ s!"let v{100000 + i}: u8 = v{99999 + i} + v100000;\n"
  return (src ++ s!"return v{99999 + n};\n" ++ "}\n").toUTF8

private def plusChain (n : Nat) : ByteArray :=
  ("fn main() -> u8 { return 0" ++ String.join (List.replicate n " + 0") ++ "; }").toUTF8

private def nested (n : Nat) : ByteArray :=
  ("fn main() -> u8 { return " ++ String.ofList (List.replicate n '(') ++ "0" ++
    String.ofList (List.replicate n ')') ++ "; }").toUTF8

private structure Case where
  stage : String
  shape : String
  n : Nat
  repetitions : Nat := 1

private inductive Verdict where
  | pass | fail | incomplete
  deriving BEq

private def Verdict.label : Verdict → String
  | .pass => "PASS"
  | .fail => "FAIL"
  | .incomplete => "INCOMPLETE"

private abbrev Pair α := α × α

-- Retain the result across the second clock read, so dropping the result and
-- checking the fixture are outside the measured stage.
@[noinline] private def timed {α β : Type} (stage : α → Except String β) (input : α) :
    IO (Nat × Except String β) := do
  let start ← IO.monoNanosNow
  let output := stage input
  let stop ← IO.monoNanosNow
  return (stop - start, output)

private def checked {α : Type} (name : String) (result : Except String α) : IO α :=
  match result with
  | .ok output => pure output
  | .error reason => throw (IO.userError s!"{name}: {reason}")

private def sample {α β : Type} (c : Case) (stage : α → Except String β) (input : α) :
    IO Nat := do
  let mut total := 0
  for _ in [0:c.repetitions] do
    let (ns, result) ← timed stage input
    let _ ← checked c.stage result
    total := total + ns
  return total

private def median (values : Array Float) : Float :=
  let sorted := values.qsort (· < ·)
  (sorted[(sorted.size - 1) / 2]! + sorted[sorted.size / 2]!) / 2

private def limit : Float := 10.5

private def classify (ratio : Float) : Verdict := if ratio ≤ limit then .pass else .fail

private def stableVerdict (ratios : Array Float) : Verdict :=
  if ratios.size != 5 then .incomplete else
    let full := classify (median ratios)
    if (List.range ratios.size).all (fun i =>
        classify (median (ratios.extract 0 i ++ ratios.extract (i + 1) ratios.size)) == full) then
      full
    else .incomplete

#guard stableVerdict #[8, 8, 8, 8, 64] == .pass
#guard stableVerdict #[8, 64, 64, 64, 64] == .fail
#guard stableVerdict #[8, 8, 8, 14, 14] == .incomplete
#guard stableVerdict #[8, 8, 12, 12, 12] == .incomplete

private def measureCase {α β : Type} (c : Case) (stage : α → Except String β)
    (input : Pair α) : IO Verdict := do
  for attempt in [1:3] do
    let mut ratios := #[]
    for i in [0:5] do
      let (small, large) ← if i % 2 == 0 then do
        let small ← sample c stage input.1
        let large ← sample c stage input.2
        pure (small, large)
      else do
        let large ← sample c stage input.2
        let small ← sample c stage input.1
        pure (small, large)
      let valid := small > 0 && large > 0
      let ratio := large.toFloat / small.toFloat
      let displayed := if valid then toString ratio else "-"
      IO.println s!"pair\t{c.stage}\t{c.shape}\t{c.n}\t{8*c.n}\t{c.repetitions}\t{attempt}\t{i+1}\t{small}\t{large}\t{displayed}\t-"
      if valid then
        ratios := ratios.push ratio
    let ratio := if ratios.size == 5 then toString (median ratios) else "-"
    let verdict := stableVerdict ratios
    IO.println s!"summary\t{c.stage}\t{c.shape}\t{c.n}\t{8*c.n}\t{c.repetitions}\t{attempt}\t-\t-\t-\t{ratio}\t{verdict.label}"
    if verdict != .incomplete then return verdict
  return .incomplete

private def compiler {α β : Type} (stage : α → Except Refusal β) (input : α) :
    Except String β :=
  (stage input).mapError fun refusal => s!"{refusal.code}: {refusal.message}"

-- Prepare each next input and warm up both sizes before measuring the edge.
private def prepare {α β : Type} (name : String) (stage : α → Except String β)
    (input : Pair α) : IO (Pair β) := do
  let small ← checked name (stage input.1)
  let large ← checked name (stage input.2)
  return (small, large)

private def bench {α β : Type} (c : Case) (stage : α → Except String β)
    (input : Pair α) : IO (Verdict × Pair β) := do
  let outputs ← prepare c.stage stage input
  let verdict ← measureCase c stage input
  return (verdict, outputs)

private def sizes {α : Type} (make : Nat → α) (n : Nat) : Pair α :=
  (make n, make (8*n))

private def memory (p : MIR.Program) : Except String Memory.Program :=
  .ok (Memory.lower p)

-- A planted scan from every statement through the remaining statements, with
-- the checksum validated outside the timed region. The underlying pass is the
-- same production Memory.lower used by the positive control.
private def suffixWork (body : List MIR.Assignment) : Nat := Id.run do
  let mut remaining := body
  let mut total := 0
  for _ in body do
    total := total + remaining.length
    remaining := remaining.drop 1
  return total

private def planted (p : MIR.Program) : Except String (Memory.Program × Nat) :=
  .ok (Memory.lower p, suffixWork p.body)

private def checkControl (verdict expected : Verdict) : Verdict :=
  if verdict == expected then .pass else if verdict == .incomplete then .incomplete else .fail

def main : IO UInt32 := do
  IO.println s!"# ratio_limit={limit}"
  IO.println "record\tstage\tshape\tn\t8n\trepetitions\tattempt\tpair\tn_total_ns\t8n_total_ns\tratio\tverdict"
  let n := 256
  let shape := "let-add-chain"
  let (lex, tokens) ← bench ⟨"lex", shape, n, 1⟩ (compiler Lex.lex) (sizes letChain n)
  let (parse, ast) ← bench ⟨"parse", shape, n, 1⟩ (compiler Parse.parse) tokens
  let (elaborate, core) ← bench ⟨"elaborate", shape, n, 1⟩ (compiler Elab.elaborate) ast
  let (admit, admitted) ← bench ⟨"admission", shape, n, 1⟩ (compiler Admission.check) core
  let (mir, lowered) ← bench ⟨"MIR", shape, n, 1⟩ (compiler MIR.lower) admitted
  let (mem, slots) ← bench ⟨"Memory", shape, n, 32⟩ memory lowered
  let (ssa, bir) ← bench ⟨"SSA/BIR", shape, n, 1⟩ (compiler (SSA.lower fixturePath)) slots
  let (llvm, _) ← bench ⟨"LLVM print", shape, n, 1⟩ (fun p => .ok (LLVM.module p)) bir
  let lcbN := 32
  let lcbInputs ← prepare "LCB input" (compiler fun src =>
    (·.bir) <$> Stages.compile ⟨fixturePath, src⟩) (sizes letChain lcbN)
  let (lcb, _) ← bench ⟨"LCB emit", shape, lcbN, 1⟩ LCB.emit lcbInputs
  let parenTokens ← prepare "nested tokens" (compiler Lex.lex) (sizes nested n)
  let (paren, _) ← bench ⟨"parse", "nested-parentheses", n, 4⟩ (compiler Parse.parse) parenTokens
  let plusTokens ← prepare "plus tokens" (compiler Lex.lex) (sizes plusChain n)
  let (plusParse, plusAST) ← bench ⟨"parse", "plus-chain", n, 4⟩ (compiler Parse.parse) plusTokens
  let (plusElab, _) ← bench ⟨"elaborate", "plus-chain", n, 4⟩ (compiler Elab.elaborate) plusAST
  let (quadratic, plantedOutput) ← bench ⟨"Memory quadratic control", shape, n, 1⟩
    planted lowered
  for (size, actual) in [(n, plantedOutput.1.2), (8*n, plantedOutput.2.2)] do
    unless actual == size * (size + 1) / 2 do
      throw (IO.userError "quadratic control checksum disagrees with fixture size")
  let linearControl := checkControl mem .pass
  let quadraticControl := checkControl quadratic .fail
  IO.println s!"control\tlinear\t{linearControl.label}"
  IO.println s!"control\tquadratic\t{quadraticControl.label}"
  let verdicts := [lex, parse, elaborate, admit, mir, mem, ssa, llvm, lcb, paren, plusParse,
    plusElab, linearControl, quadraticControl]
  if verdicts.contains .incomplete then return 3
  if verdicts.contains .fail then return 1
  return 0
