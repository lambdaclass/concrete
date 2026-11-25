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
  factor : Nat := 8

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

private def ratioLimit (factor : Nat) : Float :=
  if factor == 16 then 22.5 else 10.5

private def classify (ratio : Float) (factor : Nat) : Verdict :=
  if ratio ≤ ratioLimit factor then .pass else .fail

private def stableVerdict (ratios : Array Float) (factor : Nat := 8) : Verdict :=
  if ratios.size != 5 then .incomplete else
    let full := classify (median ratios) factor
    if (List.range ratios.size).all (fun i =>
        classify (median (ratios.extract 0 i ++ ratios.extract (i + 1) ratios.size)) factor == full) then
      full
    else .incomplete

#guard stableVerdict #[8, 8, 8, 8, 64] == .pass
#guard stableVerdict #[8, 64, 64, 64, 64] == .fail
#guard stableVerdict #[8, 8, 8, 14, 14] == .incomplete
#guard stableVerdict #[8, 8, 12, 12, 12] == .incomplete
-- A quadratic term contributing 1/32 at the small endpoint gives this 16x ratio.
#guard classify (16 * (31 + 16) / 32) 16 == .fail

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
      IO.println s!"pair\t{c.stage}\t{c.shape}\t{c.n}\t{c.factor*c.n}\t{c.repetitions}\t{attempt}\t{i+1}\t{small}\t{large}\t{displayed}\t-"
      if valid then
        ratios := ratios.push ratio
    let ratio := if ratios.size == 5 then toString (median ratios) else "-"
    let verdict := stableVerdict ratios c.factor
    IO.println s!"summary\t{c.stage}\t{c.shape}\t{c.n}\t{c.factor*c.n}\t{c.repetitions}\t{attempt}\t-\t-\t-\t{ratio}\t{verdict.label}"
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

private def sizes {α : Type} (make : Nat → α) (n factor : Nat) : Pair α :=
  (make n, make (factor*n))

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

private def validateBIR (p : BIR.Program) : Except String Unit :=
  if BIR.wf p then .ok () else .error "fixture is not well formed"

private def checkedInput (n : Nat) : Except Refusal Memory.Program := do
  let tokens ← Lex.lex (letChain n)
  let ast ← Parse.parse tokens
  let core ← Elab.elaborate ast
  return Memory.lower (← MIR.lowerUnchecked core)

private def widePhi (n : Nat) : BIR.Program :=
  let params := (List.range n).map fun i => (i, BIR.Ty.int .u8)
  let args := (List.range n).map fun i => BIR.Operand.imm .u8 (Int.ofNat (i % 256))
  ⟨[⟨[], [], .br 1 args⟩, ⟨params, [], .ret (.reg (n - 1))⟩], []⟩

private def backends (row : String → Nat → Case) (bir : Pair BIR.Program) : IO (List Verdict) := do
  let (llvm, _) ← bench (row "LLVM print" 4) (fun p => .ok (LLVM.module p)) bir
  let (lcb, _) ← bench (row "LCB emit" 8) LCB.emit bir
  return [llvm, lcb]

private def lcbProduction (shape : String) (n factor : Nat)
    (inputs : Pair BIR.Program) : IO (List Verdict) := do
  let (prepared, functions) ← bench ⟨"LCB prepare", shape, n, 32, factor⟩
    (compiler Stages.prepareLCB) inputs
  let jobs := (LCB.job functions.1, LCB.job functions.2)
  let (validated, _) ← bench ⟨"LCB job validation", shape, n, 32, factor⟩
    (fun job => (LeanCompilerBackend.Verified.validateJob job).mapError reprStr) jobs
  let (linux, _) ← bench ⟨"LCB assembly x86_64-linux", shape, n, 16, factor⟩
    (LCB.assemble .x86_64Linux) functions
  let (macos, _) ← bench ⟨"LCB assembly aarch64-macos", shape, n, 16, factor⟩
    (LCB.assemble .aarch64Macos) functions
  return [prepared, validated, linux, macos]

private def runWindow (n factor : Nat) : IO (List Verdict) := do
  let shape := "let-add-chain"
  let row := fun stage reps => Case.mk stage shape n reps factor
  let (lex, tokens) ← bench (row "lex" 4) (compiler Lex.lex) (sizes letChain n factor)
  let (parse, ast) ← bench (row "parse" 4) (compiler Parse.parse) tokens
  let (elaborate, core) ← bench (row "elaborate" 4) (compiler Elab.elaborate) ast
  let (admit, admitted) ← bench (row "admission" 4) (compiler Admission.check) core
  let (mir, lowered) ← bench (row "MIR" 8) (compiler MIR.lower) admitted
  let (mem, slots) ← bench (row "Memory" 32) memory lowered
  let (ssa, bir) ← bench (row "SSA/BIR" 4) (compiler (SSA.lower fixturePath)) slots
  let straight ← backends row bir
  let checkedRow := fun stage reps => Case.mk stage "checked-add-chain" n reps factor
  let (uncheckedMIR, residual) ← bench (checkedRow "MIR unchecked" 8)
    (compiler MIR.lowerUnchecked) core
  let (uncheckedSSA, checkedBIR) ← bench (checkedRow "SSA/BIR" 4)
    (compiler (SSA.lower fixturePath)) (Memory.lower residual.1, Memory.lower residual.2)
  for (size, p) in [(n, checkedBIR.1), (factor*n, checkedBIR.2)] do
    unless p.blocks.length == 2 * size + 1 && p.sites.length == size &&
        p.blocks.any (fun block => !block.params.isEmpty) do
      throw (IO.userError "checked-add fixture must exercise branches, phi parameters and trap sites")
  let (wf, _) ← bench (checkedRow "BIR validation" 4) validateBIR checkedBIR
  let checkedBackends ← backends checkedRow checkedBIR
  let (runtime, _) ← bench (checkedRow "LCB runtime QBE" 4)
    (fun (p : BIR.Program) => .ok (Runtime.qbe p.sites)) checkedBIR
  let wide := sizes widePhi n factor
  let _ ← prepare "wide-phi BIR" validateBIR wide
  let wideBackends ← backends (fun stage reps => ⟨stage, "wide-phi", n, reps, factor⟩) wide
  let parenTokens ← prepare "nested tokens" (compiler Lex.lex) (sizes nested n factor)
  let (paren, _) ← bench ⟨"parse", "nested-parentheses", n, 4, factor⟩ (compiler Parse.parse) parenTokens
  let plusTokens ← prepare "plus tokens" (compiler Lex.lex) (sizes plusChain n factor)
  let (plusParse, plusAST) ← bench ⟨"parse", "plus-chain", n, 4, factor⟩ (compiler Parse.parse) plusTokens
  let (plusElab, _) ← bench ⟨"elaborate", "plus-chain", n, 4, factor⟩ (compiler Elab.elaborate) plusAST
  let (quadratic, plantedOutput) ← bench (row "Memory quadratic control" 1) planted lowered
  for (size, actual) in [(n, plantedOutput.1.2), (factor*n, plantedOutput.2.2)] do
    unless actual == size * (size + 1) / 2 do
      throw (IO.userError "quadratic control checksum disagrees with fixture size")
  let linearControl := checkControl mem .pass
  let quadraticControl := checkControl quadratic .fail
  IO.println s!"control\tlinear\t{n}\t{factor*n}\t{linearControl.label}"
  IO.println s!"control\tquadratic\t{n}\t{factor*n}\t{quadraticControl.label}"
  return [lex, parse, elaborate, admit, mir, mem, ssa, uncheckedMIR, uncheckedSSA, wf, runtime,
    paren, plusParse, plusElab, linearControl, quadraticControl] ++ straight ++ checkedBackends ++ wideBackends

def main : IO UInt32 := do
  IO.println s!"# ratio_limit_at_8x={ratioLimit 8} ratio_limit_at_16x={ratioLimit 16}"
  IO.println "record\tstage\tshape\tn\tlarge_n\trepetitions\tattempt\tpair\tn_total_ns\tlarge_total_ns\tratio\tverdict"
  let first ← runWindow 256 8
  let second ← runWindow 256 16
  -- The verified backend profile accepts up to 480 definitions and 64 blocks.
  let straightInputs ← prepare "LCB straight input" (compiler fun src =>
    (·.bir) <$> Stages.compile ⟨fixturePath, src⟩) (sizes letChain 7 16)
  let straight ← lcbProduction "let-add-chain" 7 16 straightInputs
  let mem ← prepare "LCB checked input" (compiler checkedInput) (3, 24)
  let checkedBIR ← prepare "LCB checked SSA" (compiler (SSA.lower fixturePath)) mem
  let residual ← lcbProduction "checked-add-chain" 3 8 checkedBIR
  let verdicts := first ++ second ++ straight ++ residual
  if verdicts.contains .incomplete then return 3
  if verdicts.contains .fail then return 1
  return 0
