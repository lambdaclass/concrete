import Lean

open Lean Lean.Elab

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

def auditedModule (m : Name) : Bool :=
  [`ConcreteSpec, `Concrete, `LeanCompilerBackend].contains m.getRoot ||
    m == `Std.Data.HashMap.Lemmas

def aliasOwners (env : Environment) : Std.HashMap (Name × Name) (List Name) := Id.run do
  let mut aliases : Std.HashMap (Name × Name) (List Name) := {}
  for (mod, index) in env.header.moduleNames.toList.zipIdx do
    if !auditedModule mod then continue
    for entry in aliasExtension.getModuleEntries env index do
      aliases := aliases.alter entry fun owners => some (mod :: owners.getD [])
  return aliases

def aliasCandidates (ctx : ContextInfo) (id : Name) : List Name := Id.run do
  if (`_root_).isPrefixOf id then
    return [id.replacePrefix `_root_ .anonymous]
  let mut candidates := [id]
  let mut ns := ctx.currNamespace
  while !ns.isAnonymous do
    candidates := (ns ++ id) :: candidates
    ns := ns.getPrefix
  for decl in ctx.openDecls do
    match decl with
    | .simple openedNamespace excluded =>
      if !excluded.contains id then candidates := (openedNamespace ++ id) :: candidates
    | .explicit opened resolved =>
      if opened.isPrefixOf id then candidates := id.replacePrefix opened resolved :: candidates
  return candidates

-- The term info pairs the source identifier with the constant it resolved to.
-- Alias provenance comes from the module that registered that exact pair.
def sourceAliases (env : Environment) (moduleName : Name) (path : String) :
    IO (Std.HashSet Name) := do
  let opts := ({} : Options).setBool `autoImplicit false |>.setBool `warningAsError true
  let input := Parser.mkInputContext (← IO.FS.readFile path) path
  let (header, parserState, messages) ← Parser.parseHeader input
  unsafe enableInitializersExecution
  let (context, messages) ← Elab.processHeader header opts messages input (mainModule := moduleName)
  let state ← IO.processCommands input parserState (Command.mkState context messages opts)
  if state.commandState.messages.hasErrors then
    for msg in state.commandState.messages.toList do IO.eprintln (← msg.toString)
    throw (IO.userError s!"failed to elaborate source for alias inspection: {path}")
  let owners := aliasOwners env
  let mut modules : Std.HashSet Name := {}
  for tree in state.commandState.infoState.trees do
    modules := tree.foldInfo (init := modules) fun ctx info found => Id.run do
      let .ofTermInfo term := info | return found
      if !term.stx.isIdent then return found
      let .const resolved _ := term.expr.getAppFn | return found
      let mut result := found
      for candidate in aliasCandidates ctx term.stx.getId.eraseMacroScopes do
        if candidate == resolved then continue
        for mod in owners[(candidate, resolved)]?.getD [] do
          result := result.insert mod
      return result
  return modules

def main (args : List String) : IO Unit := do
  let target :: extra := args | throw (IO.userError "expected module and optional source")
  unsafe enableInitializersExecution
  initSearchPath (← findSysroot)
  let env ← importModules #[{module := target.toName}] {} (loadExts := true)
  let mut all : Std.HashSet Name := {}
  let mut named : Std.HashSet Name := {}
  for (n, ci) in env.constants.toList do
    let some i := env.getModuleIdxFor? n | continue
    if env.header.moduleNames[i.toNat]! != target.toName then continue
    let some oi := env.getModuleIdxFor? (owner env n) | continue
    if env.header.moduleNames[oi.toNat]! != target.toName then
      IO.println s!"FOREIGN {n}"
      continue
    let exampleDecl := (privateToUserName n).toString.contains "⟪import-example⟫"
    for c in ci.getUsedConstantsAsSet do
      let some j := env.getModuleIdxFor? (owner env c) | continue
      let m := env.header.moduleNames[j.toNat]!
      if auditedModule m then
        if m == target.toName then continue
        all := all.insert m
        if !exampleDecl then named := named.insert m
  if let [source] := extra then
    let some index := env.header.moduleNames.idxOf? target.toName
      | throw (IO.userError s!"missing inspected module {target}")
    for use in getExtraModUses env index do
      if auditedModule use.module && use.module != target.toName then
        all := all.insert use.module
    all := all.union (← sourceAliases env target.toName source)
  IO.println s!"MODULE {target}"
  for m in all.toList.mergeSort (fun a b => a.toString < b.toString) do
    IO.println s!"IMPORT {m}"
  for m in named.toList.mergeSort (fun a b => a.toString < b.toString) do
    IO.println s!"NAMED {m}"
