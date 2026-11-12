import Lean

open Lean

-- Realized reserved names can be stored in an unrelated importing module.
-- Compiler helpers without their own source range belong to the nearest enclosing
-- declaration. Strip a realization's private prefix when looking for that owner.
partial def owner (env : Environment) (n : Name) : Name := Id.run do
  if !isReservedName env (privateToUserName n) &&
      ((declRangeExt.find? (level := .private) env n).isSome ||
       (declRangeExt.find? (level := .server) env n).isSome) then
    return n
  let mut p := n.getPrefix
  while !p.isAnonymous do
    if env.contains p then return owner env p
    p := p.getPrefix
  let u := privateToUserName n
  if u != n then
    p := u.getPrefix
    while !p.isAnonymous do
      if env.contains p then return owner env p
      p := p.getPrefix
  return n

def main (args : List String) : IO Unit := do
  let [target] := args | throw (IO.userError "expected one module")
  unsafe enableInitializersExecution
  initSearchPath (← findSysroot)
  let env ← importModules #[{module := target.toName}] {} (loadExts := true)
  let mut all : Std.HashSet Name := {}
  let mut named : Std.HashSet Name := {}
  for (n, ci) in env.constants.toList do
    let some i := env.getModuleIdxFor? n | continue
    if env.header.moduleNames[i.toNat]! != target.toName then continue
    let some oi := env.getModuleIdxFor? (owner env n) | continue
    if env.header.moduleNames[oi.toNat]! != target.toName then continue
    let exampleDecl := (privateToUserName n).toString.contains "⟪import-example⟫"
    for c in ci.getUsedConstantsAsSet do
      let some j := env.getModuleIdxFor? (owner env c) | continue
      let m := env.header.moduleNames[j.toNat]!
      if m.getRoot == `ConcreteSpec && m != target.toName then
        all := all.insert m
        if !exampleDecl then named := named.insert m
  for m in all.toList.mergeSort (fun a b => a.toString < b.toString) do
    IO.println s!"IMPORT {m}"
  for m in named.toList.mergeSort (fun a b => a.toString < b.toString) do
    IO.println s!"NAMED {m}"
