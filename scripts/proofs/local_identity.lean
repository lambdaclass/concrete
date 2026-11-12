import Lean

open Lean

def main (args : List String) : IO Unit := do
  let [target] := args | throw (IO.userError "expected one import module")
  unsafe enableInitializersExecution
  initSearchPath (← findSysroot)
  let env ← importModules #[{module := target.toName}] {} (loadExts := true)
  let some (.ctorInfo ctor) := env.find? `ConcreteSpec.Var.mk
    | throw (IO.userError "missing Var constructor")
  unless ctor.numFields == 1 && ctor.numParams == 0 &&
      ctor.type.bindingDomain! == mkConst `Nat do
    throw (IO.userError "Var must have exactly one Nat field")
  -- Reverse reachability visits each dependency edge once, including recursive components.
  let mut users : Std.HashMap Name (Array Name) := {}
  for (n, ci) in env.constants.toList do
    for c in ci.getUsedConstantsAsSet do
      users := users.insert c ((users[c]?.getD #[]).push n)
  let mut tainted : Std.HashSet Name := {}
  let mut queue := #[`ConcreteSpec.LocalNames, `ConcreteSpec.Elaborated]
  let mut pos := 0
  while pos < queue.size do
    let n := queue[pos]!
    pos := pos + 1
    if tainted.contains n then continue
    tainted := tainted.insert n
    queue := queue ++ (users[n]?.getD #[])
  let mut bad := false
  for (n, ci) in env.constants.toList do
    let some i := env.getModuleIdxFor? n | continue
    let m := env.header.moduleNames[i.toNat]!.toString
    if !(m == "ConcreteSpec.Spec" || m.startsWith "ConcreteSpec.Spec.") then continue
    -- The sole declaration site and per-program presentation examples are explicit exceptions.
    if n == `ConcreteSpec.LocalNames then continue
    if m == "ConcreteSpec.Spec.Examples" && ci.type.getAppFn.isConstOf `ConcreteSpec.Elaborated then continue
    if tainted.contains n then
      IO.eprintln s!"semantic diagnostics dependency: {n}"
      bad := true
  if bad then throw (IO.userError "semantic definitions refer to LocalNames/Elaborated")
  IO.println "numeric Var and semantic diagnostics separation: pass"
