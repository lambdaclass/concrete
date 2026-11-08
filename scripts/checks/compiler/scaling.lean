import Concrete

/-!
Deterministic BIR scaling family and in-process stage timer. This is test machinery, not a
compiler stage. `scaled k rep` has one checked operation and trap branch, one wrapping operation
and one truncation per repetition; block arguments carry every result into the next repetition.
The repetition number changes only source-site metadata, preventing hoisting/CSE across timings.

`lean --run` measures the interpreted compiler. That is the compiler available today (the `concrete`
executable does not exist yet), and exponents over the same interpreted stages are the useful signal
until an executable exists; absolute times are not claimed to predict a future native executable.
-/

open Concrete.BIR Concrete.Backend

private def site (rep i : Nat) : SiteEntry :=
  ⟨.arithmeticOverflow, ⟨i⟩, ⟨rep + 4 * i, rep + 4 * i + 3⟩,
    ⟨"tests/slice0/scaled.con", rep + i + 1, 1⟩⟩

private def bodyBlock (k i : Nat) : Block :=
  let base := 6 * i
  let previous : Operand := if i = 0 then .imm .u8 1 else .reg base
  let next := if i + 1 = k then 2 * k else 2 * (i + 1)
  { params := if i = 0 then [] else [(base, .int .u8)]
    insts := [
      .trunc .u8 .u16 (base + 1) previous,
      .binOvf .add .u16 (base + 2) (base + 3) (.reg (base + 1)) (.imm .u16 0),
      .bin .add .u16 (base + 4) (.reg (base + 2)) (.imm .u16 0),
      .trunc .u16 .u8 (base + 5) (.reg (base + 4))]
    term := .condbr (base + 3) (2 * i + 1) [] next [.reg (base + 5)] }

/-- A linear-size, non-dead family whose expected result is always `exited 1`. -/
def scaled (k rep : Nat) : Program :=
  { blocks := (List.range k).flatMap (fun i =>
        [bodyBlock k i, { params := [], insts := [], term := .trap i }]) ++
      [{ params := [(6 * k, .int .u8)], insts := [], term := .ret (.reg (6 * k)) }]
    sites := (List.range k).map (site rep) }

private def instructions (p : Program) : List Inst := p.blocks.flatMap (·.insts)

private def scan (is : List Inst) : Nat := is.foldl (fun n i => n + i.defs.length) 0

private def plantLinear (p : Program) : Nat :=
  let is := instructions p
  (List.range 16384).foldl (fun n _ => n + scan is) 0

private def plantQuadratic (p : Program) : Nat :=
  let is := instructions p
  (List.range (256 * is.length)).foldl (fun n _ => n + scan is) 0

private def terminatorDigest : LeanCompilerBackend.Verified.Source.Terminator → Nat
  | .ret _ => 1
  | .jump target => target + 2
  | .branch _ yes no => yes + no + 3

private def blockDigest (b : LeanCompilerBackend.Verified.Source.Block) : Nat :=
  b.instructions.size + b.phis.foldl (fun n phi => n + phi.incoming.size) 0 +
    terminatorDigest b.terminator

/-- Linear structural forcing: every block, phi arm, instruction count and terminator is visited,
with no `repr` or string construction. -/
private def forceEmit : Except String (LeanCompilerBackend.Verified.Source.Function × SiteTable) → Nat
  | .ok (f, sites) =>
    f.blocks.foldl (fun n b => 33 * n + blockDigest b) (f.valueWidths.size + sites.length)
  | .error msg => msg.length

@[noinline] private def opaqueIdentity (x : α) : α := x

/-- Evaluate `force result` in a branch BEFORE reading the second clock, so laziness cannot move the
stage's work past the measurement. Returning the result lets the next measured stage consume it. -/
private def timedPure (value : Unit → α) (force : α → Nat) : IO (α × Nat × Nat) := do
  let start ← IO.monoNanosNow
  let result := value ()
  let digest := force result
  match digest % 2 with
  | 0 =>
    let stop ← IO.monoNanosNow
    return (result, stop - start, digest)
  | _ =>
    let stop ← IO.monoNanosNow
    return (result, stop - start, digest)

private def sample (rep k : Nat) (stage : String) (measurement : α × Nat × Nat) : IO Unit :=
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
    let llvm ← timedPure (fun _ => LLVM.module (opaqueIdentity p)) String.length
    sample rep k "llvm_module" llvm
    let emitted ← timedPure (fun _ => LCB.emit (opaqueIdentity p)) forceEmit
    sample rep k "lcb_emit" emitted
    match emitted.1 with
    | .error msg => IO.eprintln s!"scaled {k}: LCB emit: {msg}"; return false
    | .ok (f, _) =>
      let assembly ← timedPure (fun _ => LCB.assemble t (opaqueIdentity f)) (fun
        | .ok asm => asm.length
        | .error msg => msg.length)
      sample rep k "lcb_assemble" assembly
      match assembly.1 with
      | .error msg => IO.eprintln s!"scaled {k}: LCB assemble: {msg}"; return false
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
  for k in sizes do unless ← validate k (scaled k 0) do return 1
  IO.println s!"family\tk0={k0}\trepetitions={repetitions}\ttarget={targetName}\texpected=exit-1"
  for rep in List.range repetitions do
    for k in sizes do
      unless ← measureOne rep k target (scaled k rep) plant do return 1
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
    IO.eprintln "usage: scaling.lean emit K DIR TARGET | measure K0 REPETITIONS TARGET [--allow-planted-failure --plant linear|quadratic]"
    return 2
