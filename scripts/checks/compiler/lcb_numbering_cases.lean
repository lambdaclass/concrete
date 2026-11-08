import Concrete.Backend.LCB
import Concrete.Backend.LLVM

/-! Focused ISS-0030 regressions and scaling families. `--full` is for the server/scheduled tier.
The default is a cheap PR-sized run. Refusals must match their exact planted diagnostic. -/
open Concrete Concrete.BIR Concrete.Backend

private def sparse (n : Nat) (stride : Nat := 1000000000000) : Program :=
  { blocks := [{ params := [], insts := (List.range n).map (fun i => .bin .add .u8 (stride * (i + 1)) (.imm .u8 1) (.imm .u8 2)), term := .ret (.reg (stride * n)) }], sites := [] }

private def wide (n : Nat) : Program :=
  { blocks := [
      { params := [], insts := [.binOvf .add .u8 (n + 1) (n + 2) (.imm .u8 0) (.imm .u8 0)], term := .condbr (n + 2) 1
          ((List.range n).map fun i => .imm .u8 (Int.ofNat (i % 256))) 1
          ((List.range n).map fun i => .imm .u8 (Int.ofNat ((i + 1) % 256))) },
      { params := (List.range n).map (fun i => (i, .int .u8)), insts := [], term := .ret (.imm .u8 0) }],
    sites := [] }

private def deepBlocks (n : Nat) : Program :=
  { blocks := (List.range n).map (fun _ => { params := [], insts := [], term := .ret (.imm .u8 0) }),
    sites := [] }

private def require (label : String) (ok : Bool) : IO Unit :=
  unless ok do throw (IO.userError s!"ISS-0030: {label}")

private def expectError (label expected : String) (p : Program) : IO Unit := do
  match LCB.adapt p with
  | .error actual => require s!"{label}: wrong diagnostic: {actual}" (actual == expected)
  | .ok _ => throw (IO.userError s!"ISS-0030: {label}: accepted planted defect")

private def regressions : IO Unit := do
  let large := sparse 1
  require "large register lookup" (LCB.regIdFast (LCB.prepare large) 1000000000000 == some 3)
  let duplicate : Program := { blocks := [{ params := [], insts := [.bin .add .u8 1000000000000 (.imm .u8 1) (.imm .u8 2),
        .bin .add .u8 1000000000000 (.imm .u8 3) (.imm .u8 4)], term := .ret (.imm .u8 0) }], sites := [] }
  require "duplicate first definition wins"
    (LCB.regIdFast (LCB.prepare duplicate) 1000000000000 == some 3)
  expectError "duplicate register" "the numbering is not injective (a register defined twice?)" duplicate
  let missing : Program := { blocks := [{ params := [], insts := [], term := .ret (.reg 1000000000000) }], sites := [] }
  expectError "missing register" "register %r1000000000000 has no definition" missing
  let short : Program := { blocks := [
      { params := [], insts := [], term := .br 1 [] },
      { params := [(0, .int .u8)], insts := [], term := .ret (.reg 0) }], sites := [] }
  expectError "short phi payload" "block 0 passes too few arguments to block 1" short
  let repeated := wide 8
  match LCB.adapt repeated with
  | .error e => throw (IO.userError s!"ISS-0030: conditional arms unexpectedly refused: {e}")
  | .ok f =>
    let arms := ((f.blocks[1]?.map (·.phis)).getD #[])[0]?
    let expected : Array (Nat × LeanCompilerBackend.Verified.Source.Operand) :=
      #[(0, .literal (LCB.word 0)), (0, .literal (LCB.word 1))]
    require "conditional-arm order and multiplicity" (arms.map (fun phi => reprStr phi.incoming) == some (reprStr expected))
  let shortSecond := { repeated with blocks := match repeated.blocks with
    | b :: rest => { b with term := .condbr 10 1 (List.replicate 8 (.imm .u8 0)) 1 [] } :: rest
    | [] => [] }
  expectError "short second conditional arm" "block 0 passes too few arguments to block 1" shortSecond
  let hugeTarget : Program := { blocks := [{ params := [], insts := [], term := .br 1000000000000 [] }], sites := [] }
  for p in [large, duplicate, missing, short, hugeTarget, wide 8, sparse 8] do
    require "prepared adapter differs from reference" (reprStr (LCB.adapt p) == reprStr (LCB.adaptSpec p))
    require "prepared LLVM differs from reference" (LLVM.print p == LLVM.printSpec p)
  IO.println "ISS-0030 regressions: PASS (exact refusal diagnostics, first definition, both conditional arms, sparse names/targets)"

/-- Force a traversal of the produced function without quadratic prefix sums or list indexing. -/
private def digest (p : Program) : UInt64 :=
  match LCB.emit p with
  | .error e => hash e
  | .ok (f, _) => f.blocks.foldl (fun h b =>
      let h := b.phis.foldl (fun h phi => phi.incoming.foldl
        (fun h arm => h + hash (reprStr arm)) (h + UInt64.ofNat phi.destination)) h
      b.instructions.foldl (fun h i => h + hash (reprStr i)) (h + hash (reprStr b.terminator)))
      (UInt64.ofNat f.valueWidths.size)

private def timeStage (family stage : String) (n : Nat) (run : Unit → UInt64) : IO Nat := do
  let mut best := 1000000000000000000
  let mut previous : Option UInt64 := none
  for _ in List.range 3 do
    let t ← IO.monoNanosNow
    let d ← IO.lazyPure run
    let elapsed := (← IO.monoNanosNow) - t
    best := min best elapsed
    if let some old := previous then require "unstable structural digest" (old == d)
    previous := some d
  IO.println s!"{family}\t{n}\t{stage}\t{best}\t{previous.getD 0}"
  pure best

private def measureFamily (family : String) (n : Nat) (p : Program) : IO (Nat × Nat) := do
  match LCB.emit p with
  | .error e => throw (IO.userError s!"ISS-0030: scaling family {family} refused: {e}")
  | .ok _ => pure ()
  let lcb ← timeStage family "lcb_emit" n (fun _ => digest p)
  let llvm ← timeStage family "llvm_module" n (fun _ => hash (LLVM.module p))
  pure (lcb, llvm)

def main (args : List String) : IO Unit := do
  regressions
  IO.println "family\tn\tstage\tnanoseconds\tdigest"
  let full := args.contains "--full"
  let sizes := if full then [100, 1000, 10000] else [10, 100, 1000]
  for (family, make) in [("wide_phi", wide), ("sparse_registers", fun n => sparse n),
      ("dense_registers", fun n => sparse n 1), ("many_blocks", deepBlocks)] do
    let mut previous : Option (Nat × Nat) := none
    for n in sizes do
      let times ← measureFamily family n (make n)
      if full then
        if let some old := previous then
          -- 15.84893 < 10^1.2. Exact integer arithmetic; failures name stage and family.
          require s!"{family}: lcb_emit exceeds 1.2 scaling exponent" (times.1 * 100000 ≤ old.1 * 1584893)
          require s!"{family}: llvm_module exceeds 1.2 scaling exponent" (times.2 * 100000 ≤ old.2 * 1584893)
      previous := some times
  if full then
    -- The extended tier also exercises stack depth independently of the timing ratios.
    discard <| measureFamily "deep_instructions" 100000 (sparse 100000)
    discard <| measureFamily "deep_blocks" 100000 (deepBlocks 100000)
