-- Appended to a temporary proof source by imports.py, including named anonymous examples.
-- Inspect types AND values, and all generated local constants. Do not follow dependencies
-- transitively: the module defining each directly referenced constant owns that dependency.
#eval show _root_.Lean.Core.CoreM _root_.Unit from do
  let env ← _root_.Lean.MonadEnv.getEnv
  let mut modules : _root_.Std.HashSet _root_.Lean.Name := {}
  for (n, ci) in _root_.Lean.PersistentHashMap.toList
      (_root_.Lean.SMap.map₂ (_root_.Lean.Environment.constants env)) do
    if _root_.Option.isSome (_root_.Lean.Environment.getModuleIdxFor? env n) then continue
    for c in _root_.Std.TreeSet.toList (_root_.Lean.ConstantInfo.getUsedConstantsAsSet ci) do
      if let _root_.Option.some i := _root_.Lean.Environment.getModuleIdxFor? env c then
        let m := (_root_.Lean.EnvironmentHeader.moduleNames (_root_.Lean.Environment.header env))[_root_.Lean.ModuleIdx.toNat i]!
        if _root_.Lean.Name.getRoot m == (_root_.Lean.Name.str _root_.Lean.Name.anonymous "ConcreteSpec") then
          modules := _root_.Std.HashSet.insert modules m
  for m in _root_.List.mergeSort (_root_.Std.HashSet.toList modules) (fun a b => _root_.ToString.toString a < _root_.ToString.toString b) do
    _root_.IO.println s!"IMPORT {m}"
