import Concrete

/-!
Deterministic BIR scaling family and in-process stage timer. This is test machinery, not a
compiler stage. `scaled k rep` has one checked operation and trap branch, one wrapping operation
and one truncation per repetition; block arguments carry every result into the next repetition.
The repetition number varies both immediate operands inside `blocks` (via an add/sub pair that
preserves the value `1` without overflowing) and source-site metadata, preventing hoisting/CSE
across timings even for stages that inspect only `blocks`.

All forcing functions and planted stages use fixed-width 64-bit wrapping arithmetic (`UInt64`), so
digest accumulation and sample formatting are O(1) per visited element and never allocate unbounded
`Nat` values or large strings.

`lean --run` measures the interpreted compiler. That is the compiler available today (the `concrete`
executable does not exist yet), and exponents over the same interpreted stages are the useful signal
until an executable exists; absolute times are not claimed to predict a future native executable.
-/

open Concrete.BIR Concrete.Backend

@[noinline] private def opaqueIdentity (x : α) : α := x

private def mix64 (acc x : UInt64) : UInt64 :=
  (acc ^^^ x) * 1099511628211

private def site (rep i : Nat) : SiteEntry :=
  ⟨.arithmeticOverflow, ⟨i⟩, ⟨rep + 4 * i, rep + 4 * i + 3⟩,
    ⟨"tests/slice0/scaled.con", rep + i + 1, 1⟩⟩

private def bodyBlock (k rep i : Nat) : Block :=
  let base := 6 * i
  let salt : Int := Int.ofNat ((rep + i) % 256)
  let previous : Operand := if i = 0 then .imm .u8 1 else .reg base
  let next := if i + 1 = k then 2 * k else 2 * (i + 1)
  { params := if i = 0 then [] else [(base, .int .u8)]
    insts := [
      .trunc .u8 .u16 (base + 1) previous,
      .binOvf .add .u16 (base + 2) (base + 3) (.reg (base + 1)) (.imm .u16 salt),
      .bin .sub .u16 (base + 4) (.reg (base + 2)) (.imm .u16 salt),
      .trunc .u16 .u8 (base + 5) (.reg (base + 4))]
    term := .condbr (base + 3) (2 * i + 1) [] next [.reg (base + 5)] }

/-- A linear-size, non-dead family whose expected result is always `exited 1`. -/
def scaled (k rep : Nat) : Program :=
  { blocks := (List.range k).flatMap (fun i =>
        [bodyBlock k rep i, { params := [], insts := [], term := .trap i }]) ++
      [{ params := [(6 * k, .int .u8)], insts := [], term := .ret (.reg (6 * k)) }]
    sites := (List.range k).map (site rep) }

/-- Register magnitude varies independently of program size; instructions are chained through
registers so every instruction is live and performs a register lookup (`regIdFast`). -/
private def registers (n rep stride : Nat) : Program :=
  let salt : Int := Int.ofNat (rep % 128)
  { blocks := [{ params := []
                 insts := (List.range n).map (fun i =>
                   let prev : Operand := if i == 0 then .imm .u8 salt else .reg (stride * i)
                   let rhs : Operand := .imm .u8 (if i == 0 then 1 else 0)
                   .bin .add .u8 (stride * (i + 1)) prev rhs)
                 term := .ret (.reg (stride * n)) }]
    sites := [] }

private def widePhi (n rep : Nat) : Program :=
  let salt : Int := Int.ofNat (rep % 128)
  { blocks := [
    { params := [], insts := [.binOvf .add .u8 (n + 1) (n + 2) (.imm .u8 0) (.imm .u8 0)],
      term := .condbr (n + 2) 1 [] 2 [] },
    { params := [], insts := [], term := .br 3
        ((List.range n).map fun i => .imm .u8 (Int.ofNat ((i + rep) % 256))) },
    { params := [], insts := [], term := .br 3
        ((List.range n).map fun i =>
          if i + 1 == n then .imm .u8 (salt + 1)
          else .imm .u8 (Int.ofNat ((i + rep + 1) % 256))) },
    { params := (List.range n).map (fun i => (i, .int .u8)), insts := [],
      term := .ret (if n == 0 then .imm .u8 1 else .reg (n - 1)) }], sites := [] }

/-- A well-formed chain of `n` blocks connected by branches with block parameters and instructions,
exercising `blockOffset`, `instOffset`, `incomingBuckets`, and `LLVM.edgeBuckets`. -/
private def manyBlocks (n rep : Nat) : Program :=
  let salt : Int := Int.ofNat (rep % 128)
  { blocks := (List.range n).map (fun j =>
      let prev : Operand := if j == 0 then .imm .u8 salt else .reg (2 * j)
      let rhs : Operand := .imm .u8 (if j == 0 then 1 else 0)
      { params := if j == 0 then [] else [(2 * j, .int .u8)]
        insts := [.bin .add .u8 (2 * j + 1) prev rhs]
        term := if j + 1 < n then .br (j + 1) [.reg (2 * j + 1)] else .ret (.reg (2 * j + 1)) })
    sites := [] }

private def backendFamilies : List (String × (Nat → Nat → Program)) :=
  [("wide_phi", widePhi), ("sparse_registers", fun n rep => registers n rep 1000000000000),
   ("dense_registers", fun n rep => registers n rep 1), ("many_blocks", manyBlocks)]

private def allFamilies : List (String × (Nat → Nat → Program)) :=
  ("scaled", scaled) :: backendFamilies

private def instructions (p : Program) : List Inst := p.blocks.flatMap (·.insts)

private def scan (is : List Inst) (salt : UInt64) : UInt64 :=
  is.foldl (fun acc i => mix64 acc (i.defs.length.toUInt64 + salt)) salt

private def plantLinear (p : Program) : UInt64 :=
  let is := instructions p
  (List.range 256).foldl
    (fun acc j => mix64 acc (scan (opaqueIdentity is) (opaqueIdentity j.toUInt64 + acc))) 0

private def plantQuadratic (p : Program) : UInt64 :=
  let is := instructions p
  (List.range is.length).foldl
    (fun acc j => mix64 acc (scan (opaqueIdentity is) (opaqueIdentity j.toUInt64 + acc))) 0

private def terminatorDigest : LeanCompilerBackend.Verified.Source.Terminator → UInt64
  | .ret _ => 1
  | .jump target => target.toUInt64 + 2
  | .branch _ yes no => yes.toUInt64 + no.toUInt64 + 3

private def blockDigest (b : LeanCompilerBackend.Verified.Source.Block) : UInt64 :=
  let h := mix64 b.instructions.size.toUInt64 (terminatorDigest b.terminator)
  b.phis.foldl (fun acc phi => mix64 acc (phi.incoming.size.toUInt64 + phi.destination.toUInt64)) h

/-- Linear structural forcing: every block, phi arm, instruction count and terminator is visited
using fixed-width 64-bit wrapping arithmetic (`UInt64`), with no `repr`, no string construction and
no unbounded `Nat` growth. -/
private def forceEmit : Except String (LeanCompilerBackend.Verified.Source.Function × SiteTable) → UInt64
  | .ok (f, sites) =>
    let init := mix64 14695981039346656037 (f.valueWidths.size.toUInt64 + sites.length.toUInt64)
    f.blocks.foldl (fun n b => mix64 n (blockDigest b)) init
  | .error msg => msg.utf8ByteSize.toUInt64

private def forceString (s : String) : UInt64 :=
  s.length.toUInt64

private def forceAssemble : Except String String → UInt64
  | .ok asm => asm.length.toUInt64
  | .error msg => msg.length.toUInt64

/-- Evaluate `force result` in a branch BEFORE reading the second clock, so laziness cannot move the
stage's work past the measurement. Returning the result lets the next measured stage consume it. -/
private def timedPure (value : Unit → α) (force : α → UInt64) : IO (α × Nat × UInt64) := do
  -- A sample covers at least 20 ms, reducing timer/startup granularity at small sizes.
  -- lazyPure executes the stage on every iteration; forcing stays before the clock read.
  let start ← IO.monoNanosNow
  let mut count := 0
  repeat
    let result ← IO.lazyPure value
    let digest := force result
    let stop ← match digest &&& 1 with
      | 0 => IO.monoNanosNow
      | _ => IO.monoNanosNow
    count := count + 1
    if stop - start ≥ 20000000 then
      return (result, (stop - start) / count, digest)

private def sample (rep k : Nat) (stage : String) (measurement : α × Nat × UInt64) : IO Unit :=
  IO.println s!"sample\t{rep}\t{k}\t{stage}\t{measurement.2.1}\t{measurement.2.2}"

private def measureOne (rep k : Nat) (t : LCB.Target) (p : Program) (plant : Option String) :
    IO Bool := do
  match plant with
  | some "linear" =>
    let planted ← timedPure (fun _ => plantLinear (opaqueIdentity p)) id
    sample rep k "plant_linear" planted
    return true
  | some "quadratic" =>
    let planted ← timedPure (fun _ => plantQuadratic (opaqueIdentity p)) id
    sample rep k "plant_quadratic" planted
    return true
  | some _ => IO.eprintln "unknown plant"; return false
  | none =>
    let llvm ← timedPure (fun _ => LLVM.module (opaqueIdentity p)) forceString
    sample rep k "llvm_module" llvm
    let emitted ← timedPure (fun _ => LCB.emit (opaqueIdentity p)) forceEmit
    sample rep k "lcb_emit" emitted
    match emitted.1 with
    | .error msg => IO.eprintln s!"scaled {k}: LCB emit: {msg}"; return false
    | .ok (f, _) =>
      let assembly ← timedPure (fun _ => LCB.assemble t (opaqueIdentity f)) forceAssemble
      sample rep k "lcb_assemble" assembly
      match assembly.1 with
      | .error msg => IO.eprintln s!"scaled {k}: LCB assemble: {msg}"; return false
      | .ok _ => return true

private def measureBackendOne (rep k : Nat) (p : Program) : IO Bool := do
  let llvm ← timedPure (fun _ => LLVM.module (opaqueIdentity p)) forceString
  sample rep k "llvm_module" llvm
  let emitted ← timedPure (fun _ => LCB.emit (opaqueIdentity p)) forceEmit
  sample rep k "lcb_emit" emitted
  match emitted.1 with
  | .error msg => IO.eprintln s!"backend family {k}: LCB emit: {msg}"; return false
  | .ok _ => return true

private def validate (k : Nat) (p : Program) : IO Bool := do
  unless wf p do IO.eprintln s!"scaled {k}: not well formed"; return false
  unless run p (k + 2) == .exited 1 do
    IO.eprintln s!"scaled {k}: expected exit 1, got {repr (run p (k + 2))}"; return false
  return true

private def runMeasurement (k0 repetitions : Nat) (targetName : String) (plant : Option String) : IO UInt32 := do
  if k0 = 0 || repetitions = 0 then IO.eprintln "k0 and repetitions must be positive"; return 2
  let some target := LCB.Target.ofName targetName
    | IO.eprintln s!"unknown target {targetName}"; return 2
  let sizes := [k0, 10 * k0, 100 * k0]
  for rep in List.range repetitions do
    for k in sizes do
      unless ← validate k (scaled k rep) do return 1
  IO.println s!"family\tk0={k0}\trepetitions={repetitions}\ttarget={targetName}\texpected=exit-1"
  for rep in List.range repetitions do
    for k in (if rep % 2 == 0 then sizes else sizes.reverse) do
      unless ← measureOne rep k target (scaled k rep) plant do return 1
  return 0

/-- PR coverage is deterministic correctness/shape coverage, not a timing qualification or a
claim about algorithmic work. Every family is checked for BIR well-formedness, evaluator outcome,
LCB body evaluation, repeated emission and LLVM rendering determinism, sparse/dense canonical
numbering equivalence, and in-process assembly at sizes 1, 10 and 100, followed by deep
instruction, block and wide-phi probes. -/
private def checkFamilies (depth : Nat) : IO UInt32 := do
  let some target := LCB.Target.ofName "x86_64-linux" | return 2
  for (family, make) in allFamilies do
    for n in [1, 10, 100] do
      let p := make n 0
      unless wf p do
        IO.eprintln s!"FAIL scaling PR: {family} {n}: not well formed"; return 1
      unless run p (2 * n + 4) == .exited 1 do
        IO.eprintln s!"FAIL scaling PR: {family} {n}: wrong evaluator result {repr (run p (2 * n + 4))}"; return 1
      let first := LCB.emit p
      match first with
      | .error msg => IO.eprintln s!"FAIL scaling PR: {family} {n}: {msg}"; return 1
      | .ok (f, _) =>
        unless f.blocks.size == p.blocks.length do
          IO.eprintln s!"FAIL scaling PR: {family} {n}: block count"; return 1
        unless LCB.evalBody f (20 * n + 100) == encode (.exited 1) do
          IO.eprintln s!"FAIL scaling PR: {family} {n}: LCB evaluator mismatch"; return 1
        unless reprStr first == reprStr (LCB.emit (opaqueIdentity p)) &&
            LLVM.module p == LLVM.module (opaqueIdentity p) do
          IO.eprintln s!"FAIL scaling PR: {family} {n}: repeated output differs"; return 1
        match LCB.assemble target f with
        | .error msg => IO.eprintln s!"FAIL scaling PR: {family} {n}: assembly: {msg}"; return 1
        | .ok asm =>
          unless !asm.isEmpty do IO.eprintln s!"FAIL scaling PR: {family} {n}: empty assembly"; return 1
        IO.println s!"coverage\t{family}\t{n}\tblocks={f.blocks.size}\tdigest={forceEmit first}"
  for n in [1, 10, 100, 1000] do
    unless reprStr (LCB.emit (registers n 0 1000000000000)) == reprStr (LCB.emit (registers n 0 1)) do
      IO.eprintln s!"FAIL scaling PR: sparse/dense register canonical numbering differs at {n}"; return 1
  let phiDepth := if depth ≥ 10000 then 10000 else 1000
  for (family, probeDepth, p) in [("wide_phi_depth", phiDepth, widePhi phiDepth 0),
                                  ("deep_instructions", depth, registers depth 0 1000000000000),
                                  ("deep_dense_registers", depth, registers depth 0 1),
                                  ("deep_blocks", depth, manyBlocks depth 0)] do
    match LCB.emit p with
    | .error msg => IO.eprintln s!"FAIL scaling depth: {family}: {msg}"; return 1
    | .ok result =>
      let llvmLen := forceString (LLVM.module p)
      unless llvmLen > 0 do IO.eprintln s!"FAIL scaling depth: {family}: empty LLVM module"; return 1
      IO.println s!"depth\t{family}\t{probeDepth}\tdigest={forceEmit (.ok result)}\tllvm={llvmLen}"
  IO.println "PASS scaling deterministic family coverage (no wall-clock qualification)"
  return 0

private def measureFamily (family : String) (k0 repetitions : Nat) (targetName : String) : IO UInt32 := do
  let some (_, make) := backendFamilies.find? (·.1 == family)
    | IO.eprintln s!"unknown scaling family {family}"; return 2
  let some target := LCB.Target.ofName targetName
    | IO.eprintln s!"unknown target {targetName}"; return 2
  if k0 == 0 || repetitions == 0 then return 2
  let base := make k0 0
  unless wf base && run base (2 * k0 + 4) == .exited 1 do
    IO.eprintln s!"{family} {k0}: not well formed or unexpected BIR run outcome"; return 1
  match LCB.emit base with
  | .error msg => IO.eprintln s!"{family} {k0}: LCB emit: {msg}"; return 1
  | .ok (f, _) =>
    if let .error msg := LCB.assemble target f then
      IO.eprintln s!"{family} {k0}: LCB assemble: {msg}"; return 1
  for rep in List.range repetitions do
    -- Rotate and reverse the size order to avoid a fixed correlation with warmup/drift.
    let sizes := if rep % 2 == 0 then [k0, 10*k0, 100*k0] else [100*k0, 10*k0, k0]
    for k in sizes do
      unless ← measureBackendOne rep k (make k rep) do return 1
  return 0

private def emitInputs (k : Nat) (dir : System.FilePath) (targetName : String) : IO UInt32 := do
  let p := scaled k 0
  unless ← validate k p do return 1
  let some target := LCB.Target.ofName targetName
    | IO.eprintln s!"unknown target {targetName}"; return 2
  match LCB.emit p with
  | .error msg => IO.eprintln s!"scaled {k}: LCB emit: {msg}"; return 1
  | .ok (f, sites) =>
    match LCB.assemble target f with
    | .error msg => IO.eprintln s!"scaled {k}: LCB assemble: {msg}"; return 1
    | .ok asm =>
      IO.FS.createDirAll dir
      IO.FS.writeFile (dir / "llvm.ll") (LLVM.module p)
      IO.FS.writeFile (dir / "lcb_body.s") asm
      IO.FS.writeFile (dir / "lcb_rt.ssa") (Runtime.qbe sites)
      return 0

private def parsePlant : List String → Option (Option String)
  | [] => some none
  | ["--allow-planted-failure", "--plant", "linear"] => some (some "linear")
  | ["--allow-planted-failure", "--plant", "quadratic"] => some (some "quadratic")
  | _ => none

def main (args : List String) : IO UInt32 := do
  match args with
  | ["check-small"] => checkFamilies 1000
  | ["check-depth"] => checkFamilies 100000
  | ["measure-family", family, k, reps, target] =>
    match k.toNat?, reps.toNat? with
    | some n, some r => measureFamily family n r target
    | _, _ => IO.eprintln "measure-family: sizes must be naturals"; return 2
  | ["emit", k, dir, target] => match k.toNat? with
    | some n => emitInputs n dir target
    | none => IO.eprintln "emit: K must be a natural"; return 2
  | "measure" :: k :: reps :: target :: plantArgs =>
    match k.toNat?, reps.toNat?, parsePlant plantArgs with
    | some n, some r, some plant => runMeasurement n r target plant
    | _, _, none =>
      IO.eprintln "plants require --allow-planted-failure --plant linear|quadratic"; return 2
    | _, _, _ => IO.eprintln "measure: K0 and REPETITIONS must be naturals"; return 2
  | _ =>
    IO.eprintln "usage: scaling.lean check-small | check-depth | measure-family FAMILY K0 REPETITIONS TARGET | emit K DIR TARGET | measure K0 REPETITIONS TARGET [--allow-planted-failure --plant linear|quadratic]"
    return 2
