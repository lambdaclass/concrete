-- Emits one JSON object per line describing the ConcreteSpec library, for scripts/spec/lean_site.py.
-- Run from the repository root: `lake env lean scripts/spec/lean_site.lean > decls.jsonl`.
-- Every fact comes from the elaborated environment: module docs, declarations with their source
-- ranges, doc comments, types, and the ConcreteSpec constants each one uses.
import ConcreteSpec
import Lean
open Lean Elab Command

def inSpec (env : Environment) (n : Name) : Option Name := do
  let i ← env.getModuleIdxFor? n
  let m := env.header.moduleNames[i.toNat]!
  if m.getRoot == `ConcreteSpec then some m else none

#eval show CommandElabM Unit from do
  let env ← getEnv
  -- module docs
  for m in env.header.moduleNames do
    if m.getRoot == `ConcreteSpec then
      let docs := (getModuleDoc? env m).getD #[]
      for d in docs do
        IO.println (Json.compress (Json.mkObj [("kind", "moduleDoc"), ("module", toString m),
          ("line", d.declarationRange.pos.line), ("endLine", d.declarationRange.endPos.line),
          ("doc", d.doc)]))
  -- declarations
  let mut names : Std.HashSet Name := {}
  for (n, _) in env.constants.map₁.toList do
    if (inSpec env n).isSome && !n.isInternal then
      if (← liftCoreM (findDeclarationRanges? n)).isSome then names := names.insert n
  for n in names.toList do
    let some ci := env.find? n | continue
    let some m := inSpec env n | continue
    let some r ← liftCoreM (findDeclarationRanges? n) | continue
    let kind := match ci with
      | .thmInfo _ => "theorem" | .defnInfo _ => "def" | .inductInfo _ => "inductive"
      | .ctorInfo _ => "constructor" | .axiomInfo _ => "axiom" | .opaqueInfo _ => "opaque"
      | .recInfo _ => "recursor" | .quotInfo _ => "quot"
    let doc := (← findDocString? env n).getD ""
    let ty := (← liftTermElabM (Meta.ppExpr ci.type)).pretty 100
    let used := (ci.getUsedConstantsAsSet.toList.filter (fun c => names.contains c && c != n)).map toString
    IO.println (Json.compress (Json.mkObj [("kind", kind), ("name", toString n),
      ("module", toString m), ("line", r.selectionRange.pos.line), ("endLine", r.range.endPos.line),
      ("doc", doc), ("type", ty), ("uses", toJson used)]))
