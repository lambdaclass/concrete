import Concrete

/-!
Emitter for the slice-0 backend gate (`scripts/checks/slice0_backends.sh`). PROVISIONAL.

`lake env lean --run scripts/checks/slice0_bir.lean list` prints the hand-written BIR examples;
`... emit NAME DIR` checks the example in Lean (well formed; its BIR run gives the expected
outcome; LCB's own evaluator on the adapted body returns the same result word) and writes
`DIR/llvm.ll` (body + runtime entry), `DIR/lcb.ssa` (adapted body + runtime entry) and
`DIR/expected` (the observation the executables must show).
-/

open Concrete.BIR Concrete.Backend

def expectedObservation (p : Program) : Outcome → String
  | .exited c => s!"exit {c}"
  | .trapped s => match p.sites[s]? with
    | some e => s!"trap {Runtime.trapLine e}"
    | none => "invalid"
  | _ => "invalid"

def emit (name : String) (dir : System.FilePath) : IO UInt32 := do
  let some (_, p, expected) := Examples.all.find? (·.1 == name)
    | IO.eprintln s!"unknown example {name}"; return 2
  unless wf p do IO.eprintln s!"{name}: not well formed"; return 1
  let got := run p 1000
  unless got == expected do
    IO.eprintln s!"{name}: BIR run gives {repr got}, expected {repr expected}"; return 1
  let some word := encode expected | IO.eprintln s!"{name}: no result word"; return 1
  match LCB.adapt p with
  | .error msg => IO.eprintln s!"{name}: adapter: {msg}"; return 1
  | .ok f =>
    match LCB.evalBody f 100000 with
    | some w =>
      unless w == word do
        IO.eprintln s!"{name}: LCB's evaluator returns {w}, BIR's result word is {word}"; return 1
    | none => IO.eprintln s!"{name}: LCB's evaluator does not return"; return 1
    IO.FS.createDirAll dir
    IO.FS.writeFile (dir / "llvm.ll") (LLVM.module p)
    IO.FS.writeFile (dir / "lcb.ssa") (LCB.render f ++ "\n" ++ Runtime.qbe p.sites)
    IO.FS.writeFile (dir / "expected") (expectedObservation p expected ++ "\n")
    return 0

def main (args : List String) : IO UInt32 := do
  match args with
  | ["list"] => do
    for (n, _, _) in Examples.all do IO.println n
    return 0
  | ["emit", name, dir] => emit name dir
  | _ => IO.eprintln "usage: slice0_bir.lean list | emit NAME DIR"; return 2
