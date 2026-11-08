import Concrete

/-!
Emitter for the slice-0 backend gate (`scripts/checks/compiler/slice0_backends.sh`). PROVISIONAL.

`lake env lean --run scripts/checks/compiler/slice0_bir.lean list` prints the hand-written BIR examples;
`... emit NAME DIR TARGET` checks the example in Lean (well formed; its BIR run gives the expected
outcome; LCB's own evaluator on the adapted body returns the same result word) and writes
`DIR/llvm.ll` (body + runtime entry), `DIR/lcb_body.s` (the adapted body, assembled by LCB in
process for TARGET: `x86_64-linux`, `aarch64-linux` or `aarch64-macos`), `DIR/lcb_rt.ssa` (the
runtime entry as QBE text, for LCB's command line), `DIR/lcb_body.ssa` (a debug dump only), and the
observation the executables must show: `DIR/status` (the exit status) and `DIR/stderr` (the exact
bytes on descriptor 2; descriptor 1 must stay empty).

Two controls exercise the gate itself: `... loop DIR TARGET` emits a well-formed BIR body that
never returns (`DIR/status` is `incomplete`), and `... invalid DIR TARGET` emits a body returning a
word that encodes nothing, which the runtime must refuse (`Runtime.invalidLine`, then abort).
`... numbering` runs the deterministic prepared-numbering, predecessor-grouping and
reference-equality regressions (ISS-0030), including exact refusal-diagnostic controls and a
non-timed stack-depth check on `sparse` and `deepBlocks`.
-/

open Concrete.BIR Concrete.Backend

def writeExpected (dir : System.FilePath) (status : String) (stderr : String) : IO Unit := do
  IO.FS.writeFile (dir / "status") (status ++ "\n")
  IO.FS.writeFile (dir / "stderr") stderr

/-- Write the LCB leg: the body assembled in process, the runtime entry as QBE text. -/
def writeLcb (dir : System.FilePath) (target : LCB.Target)
    (emitted : LeanCompilerBackend.Verified.Source.Function × SiteTable) : IO Bool := do
  let (f, sites) := emitted
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "lcb_source.txt") (reprStr f ++ "\n")
  let job : LeanCompilerBackend.Verified.FunctionJob :=
    { code := { symbol := LCB.bodySymbol, body := f,
                abi := LeanCompilerBackend.Verified.planAbi f,
                layout := LeanCompilerBackend.Verified.planFrame f } }
  IO.FS.writeFile (dir / "lcb_job.txt") (reprStr job ++ "\n")
  -- On every host, LCB's verified first path (the `aarch64-macos` target) must admit the body.
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
    (emitted : LeanCompilerBackend.Verified.Source.Function × SiteTable) : IO Bool := do
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "bir.txt") (reprStr p ++ "\n")
  IO.FS.writeFile (dir / "llvm.ll") (LLVM.module p)
  writeLcb dir target emitted

def emit (name : String) (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  let some (_, p, expected) := Examples.all.find? (·.1 == name)
    | IO.eprintln s!"unknown example {name}"; return 2
  unless wf p do IO.eprintln s!"{name}: not well formed"; return 1
  let got := run p 1000
  unless got == expected do
    IO.eprintln s!"{name}: BIR run gives {repr got}, expected {repr expected}"; return 1
  let some word := encode expected | IO.eprintln s!"{name}: no result word"; return 1
  match LCB.emit p with
  | .error msg => IO.eprintln s!"{name}: adapter: {msg}"; return 1
  | .ok (f, tbl) =>
    match LCB.evalBody f 100000 with
    | some w =>
      unless w == word do
        IO.eprintln s!"{name}: LCB's evaluator returns {w}, BIR's result word is {word}"; return 1
    | none => IO.eprintln s!"{name}: LCB's evaluator does not return"; return 1
    unless ← writeBackends dir target p (f, tbl) do return 1
    match expected with
    | .exited c => writeExpected dir (toString c) ""
    | .trapped s => match tbl[s]? with
      | some e => writeExpected dir "134" (Runtime.trapLine e ++ "\n")
      | none => IO.eprintln s!"{name}: no site {s}"; return 1
    | _ => IO.eprintln s!"{name}: no observation"; return 1
    return 0

/-- A body that branches back to itself forever. -/
def loopBody : Program :=
  { blocks := [{ params := [], insts := [], term := .br 1 [] },
               { params := [], insts := [], term := .br 1 [] }], sites := [] }

def emitLoop (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  unless wf loopBody && run loopBody 1000 == .outOfFuel do
    IO.eprintln "loop: not a well-formed diverging body"; return 1
  match LCB.emit loopBody with
  | .error msg => IO.eprintln s!"loop: adapter: {msg}"; return 1
  | .ok (f, tbl) =>
    if (LCB.evalBody f 10000).isSome then IO.eprintln "loop: LCB's evaluator returns"; return 1
    unless ← writeBackends dir target loopBody (f, tbl) do return 1
    writeExpected dir "incomplete" ""
    return 0

/-- One site, and a body returning `256 + 1`: past the table, so the word encodes nothing. -/
def emitInvalid (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  let tbl : SiteTable := [Examples.plusSite]
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "llvm.ll")
    ("define i32 @concrete_body() {\nentry:\n  ret i32 257\n}\n\n" ++ Runtime.llvm tbl)
  let f : LeanCompilerBackend.Verified.Source.Function :=
    { resultWidth := .word, parameters := #[], valueWidths := #[],
      blocks := #[{ phis := #[], instructions := #[], terminator := .ret (.literal (LCB.word 257)) }] }
  unless ← writeLcb dir target (f, tbl) do return 1
  writeExpected dir "134" (Runtime.invalidLine ++ "\n")
  return 0

def withTarget (name : String) (k : LCB.Target → IO UInt32) : IO UInt32 :=
  match LCB.Target.ofName name with
  | some t => k t
  | none => do IO.eprintln s!"unknown target {name}"; return 2

private def sparse (n : Nat) (stride : Nat := 1000000000000) : Program :=
  { blocks := [{ params := [],
                 insts := (List.range n).map (fun i =>
                   .bin .add .u8 (stride * (i + 1)) (.imm .u8 1) (.imm .u8 2)),
                 term := .ret (.reg (stride * n)) }],
    sites := [] }

private def wide (n : Nat) : Program :=
  { blocks := [
      { params := [], insts := [.binOvf .add .u8 (n + 1) (n + 2) (.imm .u8 0) (.imm .u8 0)],
        term := .condbr (n + 2) 1
          ((List.range n).map fun i => .imm .u8 (Int.ofNat (i % 256))) 1
          ((List.range n).map fun i => .imm .u8 (Int.ofNat ((i + 1) % 256))) },
      { params := (List.range n).map (fun i => (i, .int .u8)), insts := [], term := .ret (.imm .u8 0) }],
    sites := [] }

private def deepBlocks (n : Nat) : Program :=
  { blocks := (List.range n).map (fun _ => { params := [], insts := [], term := .ret (.imm .u8 0) }),
    sites := [] }

private def requireOk (label : String) (ok : Bool) : IO Unit :=
  unless ok do throw (IO.userError s!"numbering: {label}")

private def expectAdaptError (label expected : String) (p : Program) : IO Unit := do
  match LCB.adapt p with
  | .error actual =>
    requireOk s!"{label}: wrong diagnostic: {actual} (expected {expected})" (actual == expected)
  | .ok _ => throw (IO.userError s!"numbering: {label}: accepted planted defect")

/-- Deterministic ISS-0030 regressions: sparse/large register names, first-definition-wins lookup,
exact refusal diagnostics, conditional-arm order and multiplicity, huge branch targets,
prepared/reference equality (`adapt = adaptSpec`, `emit = emitSpec`, `LLVM.print = LLVM.printSpec`),
and a non-timed stack-depth check on `sparse` and `deepBlocks`. -/
def checkNumbering : IO UInt32 := do
  let large := sparse 1
  requireOk "large register lookup" (LCB.regIdFast (LCB.prepare large) 1000000000000 == some 3)
  let duplicate : Program :=
    { blocks := [{ params := [],
                   insts := [.bin .add .u8 1000000000000 (.imm .u8 1) (.imm .u8 2),
                             .bin .add .u8 1000000000000 (.imm .u8 3) (.imm .u8 4)],
                   term := .ret (.imm .u8 0) }],
      sites := [] }
  requireOk "duplicate first definition wins"
    (LCB.regIdFast (LCB.prepare duplicate) 1000000000000 == some 3)
  expectAdaptError "duplicate register"
    "the numbering is not injective (a register defined twice?)" duplicate
  let missing : Program :=
    { blocks := [{ params := [], insts := [], term := .ret (.reg 1000000000000) }], sites := [] }
  expectAdaptError "missing register" "register %r1000000000000 has no definition" missing
  let short : Program :=
    { blocks := [{ params := [], insts := [], term := .br 1 [] },
                 { params := [(0, .int .u8)], insts := [], term := .ret (.reg 0) }],
      sites := [] }
  expectAdaptError "short phi payload" "block 0 passes too few arguments to block 1" short
  let repeated := wide 8
  match LCB.adapt repeated with
  | .error e => throw (IO.userError s!"numbering: conditional arms unexpectedly refused: {e}")
  | .ok f =>
    let arms := ((f.blocks[1]?.map (·.phis)).getD #[])[0]?
    let expected : Array (Nat × LeanCompilerBackend.Verified.Source.Operand) :=
      #[(0, .literal (LCB.word 0)), (0, .literal (LCB.word 1))]
    requireOk "conditional-arm order and multiplicity"
      (arms.map (fun phi => reprStr phi.incoming) == some (reprStr expected))
  let shortSecond := { repeated with blocks := match repeated.blocks with
    | b :: rest => { b with term := .condbr 10 1 (List.replicate 8 (.imm .u8 0)) 1 [] } :: rest
    | [] => [] }
  expectAdaptError "short second conditional arm"
    "block 0 passes too few arguments to block 1" shortSecond
  let outOfProfileBin : Program :=
    { blocks := [{ params := [], insts := [.bin .add .u32 0 (.imm .u32 1) (.imm .u32 2)],
                   term := .ret (.imm .u8 0) }], sites := [] }
  expectAdaptError "out-of-profile bin"
    s!"{repr ConcreteSpec.IntTy.u32} is outside LCB's slice-0 profile" outOfProfileBin
  let outOfProfileTrunc : Program :=
    { blocks := [{ params := [], insts := [.trunc .u32 .u8 0 (.imm .u32 1)],
                   term := .ret (.reg 0) }], sites := [] }
  expectAdaptError "out-of-profile trunc"
    s!"{repr ConcreteSpec.IntTy.u32} → {repr ConcreteSpec.IntTy.u8} is outside LCB's slice-0 profile"
    outOfProfileTrunc
  -- Negative controls for the diagnostic matcher itself: wrong diagnostic or unexpected .ok must fail.
  let rejectedWrong ← try expectAdaptError "ctl-wrong" "wrong diagnostic" duplicate; pure false
    catch _ => pure true
  requireOk "expectAdaptError must reject a wrong diagnostic" rejectedWrong
  let rejectedOk ← try expectAdaptError "ctl-ok" "any" large; pure false
    catch _ => pure true
  requireOk "expectAdaptError must reject an accepted program" rejectedOk
  let hugeTarget : Program :=
    { blocks := [{ params := [], insts := [], term := .br 1000000000000 [] }], sites := [] }
  for p in [large, duplicate, missing, short, shortSecond, outOfProfileBin, outOfProfileTrunc,
      hugeTarget, wide 8, sparse 8] ++ Examples.all.map (·.2.1) do
    requireOk "prepared adapter differs from reference"
      (reprStr (LCB.adapt p) == reprStr (LCB.adaptSpec p))
    requireOk "prepared emit differs from reference"
      (reprStr (LCB.emit p) == reprStr (LCB.emitSpec p))
    requireOk "prepared LLVM differs from reference" (LLVM.print p == LLVM.printSpec p)
  let depth := 20000
  let deepInsts := sparse depth
  match LCB.emit deepInsts with
  | .error e => throw (IO.userError s!"numbering: sparse stack-depth check refused: {e}")
  | .ok (f, _) =>
    requireOk "sparse stack-depth shape"
      (f.blocks.size == 1 && f.valueWidths.size == 4 * depth &&
        (f.blocks[0]?.map (·.instructions.size)) == some (4 * depth))
  requireOk "sparse stack-depth LLVM" ((LLVM.module deepInsts).length > 0)
  let deepCfg := deepBlocks depth
  match LCB.emit deepCfg with
  | .error e => throw (IO.userError s!"numbering: deepBlocks stack-depth check refused: {e}")
  | .ok (f, _) =>
    requireOk "deepBlocks stack-depth shape" (f.blocks.size == depth && f.valueWidths.size == 0)
  requireOk "deepBlocks stack-depth LLVM" ((LLVM.module deepCfg).length > 0)
  return 0

def main (args : List String) : IO UInt32 := do
  match args with
  | ["list"] => do
    for (n, _, _) in Examples.all do IO.println n
    return 0
  | ["emit", name, dir, t] => withTarget t (emit name dir)
  | ["loop", dir, t] => withTarget t (emitLoop dir)
  | ["invalid", dir, t] => withTarget t (emitInvalid dir)
  | ["numbering"] => checkNumbering
  | _ =>
    IO.eprintln "usage: slice0_bir.lean list | numbering | (emit NAME | loop | invalid) DIR TARGET"
    return 2
