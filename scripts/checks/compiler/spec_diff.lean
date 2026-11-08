import Concrete

/-!
Seeded spec/compiler differential driver (`scripts/checks/compiler/spec_diff.sh`). Generators live
in the existing Concrete library as flat `Concrete/SpecDiff*.lean` modules; no lake library, new
folder, or root entry is added.
Normal commands accept no mutation option. Planted defects require the explicit `control-emit`
command, `--mutate`, and `--control SPEC_DIFF_PLANTED_CONTROL`.
-/

open Concrete.BIR Concrete.Backend
open ConcreteSpec (Trap)
open LeanCompilerBackend (Width)
open LeanCompilerBackend.Verified

namespace ConcreteSpecDiff

/-- Same expectation bytes and backend artifacts as slice0_bir.lean; the script module itself is
not on Lake's import path, so its small IO adapter is mirrored here over the same backend calls. -/
def writeExpected (dir : System.FilePath) (status : String) (stderr : String) : IO Unit := do
  IO.FS.writeFile (dir / "status") (status ++ "\n")
  IO.FS.writeFile (dir / "stderr") stderr

def writeLcb (dir : System.FilePath) (target : LCB.Target)
    (emitted : Source.Function × SiteTable) : IO Bool := do
  let (f, sites) := emitted
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "lcb_source.txt") (reprStr f ++ "\n")
  let job : FunctionJob :=
    { code := { symbol := LCB.bodySymbol, body := f, abi := planAbi f, layout := planFrame f } }
  IO.FS.writeFile (dir / "lcb_job.txt") (reprStr job ++ "\n")
  if let .error msg := LCB.assemble .aarch64Macos f then
    IO.eprintln s!"{dir}: LCB first path: {msg}"; return false
  match LCB.assemble target f with
  | .error msg => IO.eprintln s!"{dir}: LCB: {msg}"; return false
  | .ok asm =>
    IO.FS.writeFile (dir / "lcb_body.s") asm
    IO.FS.writeFile (dir / "lcb_rt.ssa") (Runtime.qbe sites)
    IO.FS.writeFile (dir / "lcb_body.ssa") (LCB.render f)
    return true

def writeBackends (dir : System.FilePath) (target : LCB.Target) (p : Program)
    (emitted : Source.Function × SiteTable) : IO Bool := do
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "bir.txt") (reprStr p ++ "\n")
  IO.FS.writeFile (dir / "llvm.ll") (LLVM.module p)
  writeLcb dir target emitted

def unwiredMessage := "no Core→BIR lowering yet (slice 1)"

/-- THE integration point for the lowering branch. Its implementation must lower the spec's Core
program to BIR; until that branch lands, the spec leg runs its generator/oracle invariants but has
no compiled programs and never claims a spec/native comparison. -/
def compileCore (_ : ConcreteSpec.Program) : Except String Program :=
  .error unwiredMessage

inductive Mutation where
  | none | dropOverflowLLVM | lcbOffByOne
  deriving DecidableEq, Repr

def mutationOfName : String → Option Mutation
  | "drop-overflow-llvm" => some .dropOverflowLLVM
  | "lcb-off-by-one" => some .lcbOffByOne
  | _ => none

/-- Replace every overflow-flag extraction by constant false. The wrapped value and control-flow
shape remain, so a trapping generated program exits through its non-trap arm instead. -/
def dropOverflowLLVM (text : String) : String :=
  let lines := text.splitOn "\n"
  String.intercalate "\n" <| lines.map fun line =>
    match line.splitOn " = extractvalue " with
    | [lhs, _] => if line.endsWith ", 1" then lhs ++ " = xor i1 0, 0" else line
    | _ => line

/-- Add one to every adapted body return. This is used only by the explicit off-by-one control,
whose generated program exits; both assembly and the in-process evaluator consume this same value. -/
def lcbOffByOne (f : Source.Function) : Source.Function :=
  let (blocks, _, widths) := f.blocks.foldl
    (fun (acc : Array Source.Block × Nat × Array Width) b =>
      let (blocks, next, widths) := acc
      match b.terminator with
      | .ret a =>
        let ins := LCB.ins next (.binary .add a (.literal (LCB.word 1)))
        (blocks.push { b with instructions := b.instructions.push ins,
                              terminator := .ret (.local next) },
         next + 1, widths.push .word)
      | _ => (blocks.push b, next, widths))
    (#[], f.valueWidths.size, f.valueWidths)
  { f with blocks := blocks, valueWidths := widths }

def safeSeed (seed : String) : String :=
  let cleaned := seed.toList.map fun c => if c.isAlphanum || c = '_' then c else '_'
  let result := String.ofList cleaned
  if result.isEmpty then "seed" else result

def regressionName (seed : String) (index : Nat) : String :=
  s!"regression_{safeSeed seed}_{index}"

def trapKindName : Trap → String
  | .arithmeticOverflow => "arithmeticOverflow"

def outcomeName : Outcome → String
  | .exited _ => "exit"
  | .trapped _ => "trap"
  | .stuck => "stuck"
  | .outOfFuel => "out_of_fuel"

def writeObservation (dir : System.FilePath) (status : String) (stderr : String) : IO Unit := do
  IO.FS.createDirAll dir
  writeExpected dir status stderr
  IO.FS.writeFile (dir / "stdout") ""

def writeOutcome (dir : System.FilePath) (outcome : Outcome) (sites : SiteTable) : IO Bool := do
  match outcome with
  | .exited c => writeObservation dir (toString c) ""; return true
  | .trapped s => match sites[s]? with
    | some e => writeObservation dir "134" (Runtime.trapLine e ++ "\n"); return true
    | none => IO.eprintln s!"{dir}: no trap site {s}"; return false
  | _ => IO.eprintln s!"{dir}: outcome {repr outcome} has no final observation"; return false

def evaluatorObservation (f : Source.Function) (sites : SiteTable) : String × String :=
  match LCB.evalBody f 100000 with
  | none => ("incomplete", "")
  | some w =>
    if w < 256 then (toString w, "") else
      match sites[w - 256]? with
      | some e => ("134", Runtime.trapLine e ++ "\n")
      | none => ("134", Runtime.invalidLine ++ "\n")

def writeEvaluator (dir : System.FilePath) (f : Source.Function) (sites : SiteTable) : IO Unit := do
  let (status, stderr) := evaluatorObservation f sites
  IO.FS.writeFile (dir / "lcb_eval.status") (status ++ "\n")
  IO.FS.writeFile (dir / "lcb_eval.stderr") stderr
  IO.FS.writeFile (dir / "lcb_eval.stdout") ""

def writeRegressionMetadata (dir : System.FilePath) (name : String) (outcome : Outcome)
    (sites : SiteTable) : IO Unit := do
  let expectation ← match outcome with
    | .exited c => pure s!"exit {c}"
    | .trapped s => match sites[s]? with
      | some e => pure s!"trap {trapKindName e.kind} site {s}"
      | none => pure "INVALID"
    | _ => pure "INVALID"
  IO.FS.writeFile (dir / "manifest-row.txt")
    s!"{name}\tbir:{name}\t{expectation}\tslice0\n"
  IO.FS.writeFile (dir / "regression-name.txt") (name ++ "\n")

/-- Emit exactly the files consumed by slice0_native.sh, plus the evaluator's observation and the
same judge inputs as the manifest runner. The expectation is supplied separately for the spec leg. -/
def emitArtifacts (dir : System.FilePath) (target : LCB.Target) (p : Program) (mutation : Mutation) :
    IO Bool := do
  unless wf p do IO.eprintln s!"{dir}: generated BIR is not well formed"; return false
  match LCB.emit p with
  | .error msg => IO.eprintln s!"{dir}: adapter: {msg}"; return false
  | .ok (f, sites) =>
    let f := if mutation = .lcbOffByOne then lcbOffByOne f else f
    unless ← writeBackends dir target p (f, sites) do return false
    if mutation = .dropOverflowLLVM then
      let original ← IO.FS.readFile (dir / "llvm.ll")
      let changed := dropOverflowLLVM original
      if changed = original then IO.eprintln s!"{dir}: drop-overflow-llvm changed nothing"; return false
      IO.FS.writeFile (dir / "llvm.ll") changed
    writeEvaluator dir f sites
    return true

def emitBirCandidate (seed : String) (index : Nat) (dir : System.FilePath) (target : LCB.Target)
    (mutation : Mutation) : IO UInt32 := do
  let g := BirGen.generate seed index
  unless wf g.program do
    IO.eprintln s!"seed {seed} index {index}: rejected: generated BIR is not well formed"
    return 4
  match LCB.emit g.program with
  | .error msg => IO.eprintln s!"seed {seed} index {index}: rejected: {msg}"; return 4
  | .ok _ =>
    let outcome := run g.program 1000
    unless outcome.final && outcome != .stuck do
      IO.eprintln s!"seed {seed} index {index}: non-final/stuck outcome {repr outcome}"
      return 1
    unless ← emitArtifacts dir target g.program mutation do return 1
    unless ← writeOutcome dir outcome g.program.sites do return 1
    IO.FS.writeFile (dir / "program.lean.txt") (reprStr g.program ++ "\n")
    IO.FS.writeFile (dir / "constructs.txt") (String.intercalate "," g.constructs ++ "\n")
    writeRegressionMetadata dir (regressionName seed index) outcome g.program.sites
    IO.println s!"seed {seed} index {index}: {outcomeName outcome} {BirGen.Production.name g.production}"
    return 0

abbrev Counts := List (String × Nat)

def bump (key : String) : Counts → Counts
  | [] => [(key, 1)]
  | (k, n) :: rest => if k = key then (k, n + 1) :: rest else (k, n) :: bump key rest

def addConstructs (counts : Counts) (xs : List String) : Counts :=
  xs.foldl (fun acc x => bump x acc) counts

def countsLines (kind : String) (counts : Counts) : String :=
  String.join <| counts.map fun (name, n) => s!"{kind}\t{name}\t{n}\n"

/-- Generate and filter N independent BIR candidates. Rejections are data and are counted; the
shell gate imposes its stated yield floor and coverage/vacuity requirements. -/
def genBir (seed : String) (n : Nat) (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  IO.FS.createDirAll dir
  let mut programs := ""
  let mut rejects := ""
  let mut constructs : Counts := []
  let mut accepted := 0
  let mut rejected := 0
  let mut exits := 0
  let mut traps := 0
  for index in List.range n do
    let g := BirGen.generate seed index
    if !wf g.program then
      rejected := rejected + 1
      rejects := rejects ++ s!"reject\t{index}\tnot well formed\n"
    else match LCB.emit g.program with
      | .error msg =>
        rejected := rejected + 1
        rejects := rejects ++ s!"reject\t{index}\t{msg}\n"
      | .ok _ =>
        let outcome := run g.program 1000
        if !outcome.final || outcome == .stuck then
          IO.eprintln s!"seed {seed} index {index}: generated outcome {repr outcome}"
          return 1
        let child := dir / s!"p{index}"
        unless ← emitArtifacts child target g.program .none do return 1
        unless ← writeOutcome child outcome g.program.sites do return 1
        IO.FS.writeFile (child / "program.lean.txt") (reprStr g.program ++ "\n")
        IO.FS.writeFile (child / "constructs.txt") (String.intercalate "," g.constructs ++ "\n")
        writeRegressionMetadata child (regressionName seed index) outcome g.program.sites
        accepted := accepted + 1
        constructs := addConstructs constructs g.constructs
        if outcomeName outcome = "exit" then exits := exits + 1 else traps := traps + 1
        programs := programs ++
          s!"program\t{index}\t{outcomeName outcome}\t{BirGen.Production.name g.production}\n"
    IO.FS.writeFile (dir / "summary.tsv")
      (s!"seed\t{seed}\naccepted\t{accepted}\nrejected\t{rejected}\nattempted\t{n}\n" ++
       s!"outcome\texit\t{exits}\noutcome\ttrap\t{traps}\n" ++
       countsLines "construct" constructs ++ programs ++ rejects)
  IO.println s!"bir leg: {accepted} accepted, {rejected} rejected, exits {exits}, traps {traps}"
  IO.print <| countsLines "construct" constructs
  return 0

def specOutcome (q : ConcreteSpec.Slice0.Program) : ConcreteSpec.Outcome :=
  ConcreteSpec.run (ConcreteSpec.Slice0.toCore q) (q.body.length + 1)

/-- The spec outcome alone supplies the status and trap kind. Slice 0 is exit-only. For a future
trapping fragment, the lowering's site table supplies the line at its first site of that kind. -/
def writeSpecOutcome (dir : System.FilePath) (p : Program) (o : ConcreteSpec.Outcome) : IO Bool := do
  match o with
  | .exit c => writeObservation dir (toString c) ""; return true
  | .trap k => match p.sites.find? (fun e => e.kind = k) with
    | some e => writeObservation dir "134" (Runtime.trapLine e ++ "\n"); return true
    | none => IO.eprintln s!"{dir}: spec trap {repr k} has no lowered site"; return false
  | _ => IO.eprintln s!"{dir}: spec outcome {repr o} has no observation"; return false

def specOutcomeName : ConcreteSpec.Outcome → String
  | .exit _ => "exit" | .trap _ => "trap" | .stuck _ => "stuck" | .outOfFuel => "out_of_fuel"

/-- Generate accepted slice-0 Core programs, run the spec oracle, and offer each to compileCore.
While the fixed hook is unwired, only its exact refusal is non-fatal and no native-ready directory
is produced. -/
def genSpec (seed : String) (n : Nat) (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  IO.FS.createDirAll dir
  let mut programs := ""
  let mut rejects := ""
  let mut constructs : Counts := []
  let mut accepted := 0
  let mut rejected := 0
  let mut compiled := 0
  let mut exits := 0
  let mut traps := 0
  for index in List.range n do
    let g := SpecGen.generate seed index
    let core := ConcreteSpec.Slice0.toCore g.program
    if !ConcreteSpec.check core then
      rejected := rejected + 1
      rejects := rejects ++ s!"reject\t{index}\tspec check\n"
    else
      let outcome := specOutcome g.program
      match outcome with
      | .exit c =>
        unless c < 256 do IO.eprintln s!"seed {seed} index {index}: invalid spec exit {c}"; return 1
        accepted := accepted + 1
        exits := exits + 1
        constructs := addConstructs constructs g.constructs
        let child := dir / s!"p{index}"
        IO.FS.createDirAll child
        IO.FS.writeFile (child / "program.lean.txt") (reprStr g.program ++ "\n")
        IO.FS.writeFile (child / "constructs.txt") (String.intercalate "," g.constructs ++ "\n")
        IO.FS.writeFile (child / "oracle.txt") s!"exit {c}\n"
        let mut didCompile := false
        match compileCore core with
        | .error msg =>
          if msg != unwiredMessage then
            IO.eprintln s!"seed {seed} index {index}: compileCore: {msg}"
            return 1
        | .ok bir =>
          unless ← emitArtifacts child target bir .none do return 1
          unless ← writeSpecOutcome child bir outcome do return 1
          IO.FS.writeFile (child / "native-ready") ""
          writeRegressionMetadata child (regressionName seed index) (.exited c) bir.sites
          compiled := compiled + 1
          didCompile := true
        programs := programs ++
          s!"program\t{index}\texit\t{if didCompile then "true" else "false"}\n"
      | .trap _ =>
        accepted := accepted + 1
        traps := traps + 1
        IO.eprintln s!"seed {seed} index {index}: accepted slice-0 program trapped"
        return 1
      | _ =>
        IO.eprintln s!"seed {seed} index {index}: spec oracle gives {repr outcome}"
        return 1
    IO.FS.writeFile (dir / "summary.tsv")
      (s!"seed\t{seed}\naccepted\t{accepted}\nrejected\t{rejected}\nattempted\t{n}\ncompiled\t{compiled}\n" ++
       s!"outcome\texit\t{exits}\noutcome\ttrap\t{traps}\n" ++
       countsLines "construct" constructs ++ programs ++ rejects)
  IO.FS.writeFile (dir / "summary.tsv")
    (s!"seed\t{seed}\naccepted\t{accepted}\nrejected\t{rejected}\nattempted\t{n}\ncompiled\t{compiled}\n" ++
     s!"outcome\texit\t{exits}\noutcome\ttrap\t{traps}\n" ++
     countsLines "construct" constructs ++ programs ++ rejects)
  IO.println s!"spec leg: {accepted} accepted, {rejected} rejected, exits {exits}, traps {traps}"
  IO.print <| countsLines "construct" constructs
  if compiled = 0 then
    IO.println "spec leg: 0 programs compiled (no Core→BIR lowering; becomes required in the lowering branch)"
  else
    IO.println s!"spec leg: {compiled} programs compiled for native comparison"
  return 0

/-- Stand-in compiled output for the spec-oracle control: Slice0.answer and BIR answer both exit 42,
but the judge receives a planted expected 43. -/
def controlSpecOracle (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  let q := ConcreteSpec.Slice0.answer
  let p := Concrete.BIR.Examples.answer
  unless ← emitArtifacts dir target p .none do return 1
  unless ← writeSpecOutcome (dir / "spec-expected") p (specOutcome q) do return 1
  writeObservation (dir / "wrong-expected") "43" ""
  IO.println "control spec-oracle: spec exits 42; planted expectation exits 43"
  return 0

def withTarget (name : String) (k : LCB.Target → IO UInt32) : IO UInt32 :=
  match LCB.Target.ofName name with
  | some t => k t
  | none => do IO.eprintln s!"unknown target {name}"; return 2

def parseNat (value : String) : Option Nat := value.toNat?

end ConcreteSpecDiff

open ConcreteSpecDiff

def main (args : List String) : IO UInt32 := do
  match args with
  | ["gen-spec", seed, n, dir] => match parseNat n with
    | some n => do
      let target := (← IO.getEnv "SPEC_DIFF_TARGET").getD "x86_64-linux"
      withTarget target (genSpec seed n dir)
    | none => return 2
  | ["gen-bir", seed, n, dir, target] => match parseNat n with
    | some n => withTarget target (genBir seed n dir)
    | none => return 2
  | ["replay", seed, index, dir, target] => match parseNat index with
    | some index => withTarget target (fun t => emitBirCandidate seed index dir t .none)
    | none => return 2
  | ["control-emit", "--mutate", mutation, seed, index, dir, target,
      "--control", "SPEC_DIFF_PLANTED_CONTROL"] => match mutationOfName mutation, parseNat index with
    | some mutation, some index =>
      withTarget target (fun t => emitBirCandidate seed index dir t mutation)
    | _, _ => return 2
  | ["control-spec-oracle", dir, target, "--control", "SPEC_DIFF_PLANTED_CONTROL"] =>
    withTarget target (controlSpecOracle dir)
  | _ =>
    IO.eprintln "usage: spec_diff.lean gen-spec SEED N DIR | gen-bir SEED N DIR TARGET | replay SEED INDEX DIR TARGET | control-emit --mutate NAME SEED INDEX DIR TARGET --control SPEC_DIFF_PLANTED_CONTROL | control-spec-oracle DIR TARGET --control SPEC_DIFF_PLANTED_CONTROL"
    return 2
