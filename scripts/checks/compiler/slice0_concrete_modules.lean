import Lean.Elab.Command
import Concrete

open Lean Elab Command in
#eval show CommandElabM Unit from do
  for m in (← getEnv).header.moduleNames do
    if m.getRoot == `Concrete then IO.println m
