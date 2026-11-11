-- Appended to a temporary proof source by imports.py, including named anonymous examples.
-- Inspect types AND values, and all generated local constants. Do not follow dependencies
-- transitively: the module defining each directly referenced constant owns that dependency.
open Lean in
#eval show CoreM Unit from do
  let env ← getEnv
  let mut modules : Std.HashSet Name := {}
  for (n, ci) in env.constants.map₂.toList do
    if (env.getModuleIdxFor? n).isSome then continue
    for c in ci.getUsedConstantsAsSet.toList do
      if let some i := env.getModuleIdxFor? c then
        let m := env.header.moduleNames[i.toNat]!
        if m.getRoot == `ConcreteSpec then modules := modules.insert m
  for m in modules.toList.mergeSort (fun a b => toString a < toString b) do
    IO.println s!"IMPORT {m}"
