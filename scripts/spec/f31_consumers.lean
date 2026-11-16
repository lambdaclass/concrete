import Lean
import ConcreteSpec
import Concrete

/-!
F31's measured elaborated dependency surface. Run after `lake build ConcreteSpec Concrete`:
`lake env lean scripts/spec/f31_consumers.lean > docs/proposals/f31-declarations.json`.

This is an engineering inventory, not a semantic driver or proof classifier. It includes generated
and private declarations, because their types and bodies must elaborate after the switch too.
`seed` names the old integer/scalar representation and BIR modulo policy to retire; `direct` names
an immediate user; `dependent` names a transitive requalification obligation. Dependencies include
proof bodies, not just theorem statements. The least fixed point is finite and uncapped.
Anonymous examples and non-Lean consumers are inventoried separately in f31-consumers.md.
-/
open Lean

private def seed (n : Name) : Bool :=
  [`ConcreteSpec.IntTy, `ConcreteSpec.STy, `ConcreteSpec.Scalar, `Concrete.BIR.wrap].any
    (·.isPrefixOf n)

private def declKind : ConstantInfo → String
  | .thmInfo _ => "theorem"
  | .defnInfo _ => "definition"
  | .opaqueInfo _ => "opaque"
  | .axiomInfo _ => "axiom"
  | .inductInfo _ => "inductive"
  | .ctorInfo _ => "constructor"
  | .recInfo _ => "recursor"
  | .quotInfo _ => "quotient"

#eval show CoreM Unit from do
  let env ← getEnv
  let mut decls : Array (Name × ConstantInfo × Name) := #[]
  for (n, ci) in env.constants.map₁.toList ++ env.constants.map₂.toList do
    if let some i := env.getModuleIdxFor? n then
      if let some m := env.header.moduleNames[i.toNat]? then
        if m.getRoot == `ConcreteSpec || m.getRoot == `Concrete then
          decls := decls.push (n, ci, m)
  let mut reached : NameSet := {}
  for (n, _, _) in decls do
    if seed n then reached := reached.insert n
  if reached.isEmpty then throwError "F31: empty seed population"
  let mut changed := true
  while changed do
    changed := false
    for (n, ci, _) in decls do
      if !reached.contains n && ci.getUsedConstantsAsSet.toList.any reached.contains then
        reached := reached.insert n
        changed := true
  let mut rows : Array (String × Json) := #[]
  for (n, ci, m) in decls do
    let authoritative := m == `ConcreteSpec.Spec.Scalar || m == `ConcreteSpec.Proofs.Scalar
    let protocol := m == `Concrete.Backend.Runtime || m == `Concrete.Premises
    if reached.contains n || authoritative || protocol then
      let deps := ci.getUsedConstantsAsSet.toList.filter reached.contains
      let role := if !reached.contains n then (if authoritative then "authoritative" else "protocol")
        else if seed n then "seed" else if deps.any seed then "direct" else "dependent"
      let edges := (deps.map toString).toArray.qsort (· < ·)
      let nameParts := fun (s : String) => toJson (s.splitOn ".")
      let row := Json.mkObj [
        ("module", toJson (toString m)), ("declaration", nameParts (toString n)),
        ("kind", toJson (declKind ci)), ("role", toJson role),
        ("dependencies", toJson (edges.map nameParts))]
      rows := rows.push (toString n, row)
  IO.println (toJson ((rows.qsort (fun a b => a.1 < b.1)).map (·.2))).pretty
