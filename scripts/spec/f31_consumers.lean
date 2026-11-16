import Lean
import ConcreteSpec
import Concrete

/-!
F31 migration tool: retire with the manifest and companion tool when F31 closes.
F31's measured elaborated dependency surface. Run after `lake build ConcreteSpec Concrete`:
`python3 -I scripts/spec/f31_inventory.py --output /tmp/f31-inventory`.

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

private def nameParts : Name → List Json
  | .anonymous => []
  | .str p s => nameParts p ++ [Json.mkObj [("string", toJson s)]]
  | .num p n => nameParts p ++ [Json.mkObj [("number", toJson n)]]

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
    let authoritative := m == `ConcreteSpec.Spec.Scalar || m == `ConcreteSpec.Proofs.Scalar ||
      m == `ConcreteSpec.Diagnostics.ScalarFixtures
    let protocol := m == `Concrete.Backend.Runtime || m == `Concrete.Premises
    if reached.contains n || authoritative || protocol then
      let deps := ci.getUsedConstantsAsSet.toList.filter reached.contains
      let role := if !reached.contains n then (if authoritative then "authoritative" else "protocol")
        else if seed n then "seed" else if deps.any seed then "direct" else "dependent"
      let edges := deps.toArray.qsort (fun a b =>
        (toJson (nameParts a)).compress < (toJson (nameParts b)).compress)
      let row := Json.mkObj [
        ("module", toJson (toString m)), ("declaration", toJson (nameParts n)),
        ("kind", toJson (declKind ci)), ("role", toJson role),
        ("dependencies", toJson (edges.map (fun d => toJson (nameParts d))))]
      rows := rows.push ((toJson (nameParts n)).compress, row)
  IO.println (toJson ((rows.qsort (fun a b => a.1 < b.1)).map (·.2))).pretty

-- Read the live registry through its owning Lean definitions, never scrape its source text.
#eval do
  for p in Concrete.Premises.all do
    IO.eprintln s!"F31-REGISTRY\tpremise\t{p.id}"
  for n in Concrete.Premises.theorems do
    IO.eprintln s!"F31-REGISTRY\theadline\t{n}"
