-- The gate appends this same exporter body to fixtures after their declarations.
import ConcreteSpec.Spec.Rules
import Lean
open Lean Elab Command

-- EXPORTER BODY
/-- Check closed type-level syntax without ignoring constructor parameters,
indices or proof-field propositions. Opaque definitions and axioms are not concrete
syntax, even when their values would be irrelevant to the evidence theorem. -/
def concreteType (e : Expr) : MetaM Bool := do
  if e.hasFVar || e.hasMVar || e.hasLooseBVars then return false
  -- Follow definition bodies before reduction, so an alias or an erasing
  -- computation cannot hide an opaque dependency. A visited set bounds shared graphs.
  let mut pending := e.getUsedConstants
  let mut seen : NameSet := {}
  while !pending.isEmpty do
    let n := pending.back!
    pending := pending.pop
    if seen.contains n then continue
    seen := seen.insert n
    match (← getEnv).find? n with
    | some (.opaqueInfo _) | some (.axiomInfo _) => return false
    | some (.defnInfo d) =>
      if (← getReducibilityStatus n) == .irreducible then return false
      pending := pending ++ d.value.getUsedConstants
    | none => return false
    | _ => pure ()
  return true

/-- Concrete data normalizes completely to constructors and literals. Proofs are
permitted only as fields of concrete data, and only after checking their proposition.
The evidence theorem's separate transitive axiom check covers the proof terms. -/
partial def concreteData (e : Expr) : MetaM Bool := do
  if e.hasFVar || e.hasMVar || e.hasLooseBVars then return false
  if ← Meta.isProof e then return false
  if !(← concreteType e) || !(← concreteType (← Meta.inferType e)) then return false
  let e ← Meta.reduce e
  match e with
  | .lit _ => return true
  | _ =>
    let .const n _ := e.getAppFn | return false
    let some (.ctorInfo c) := (← getEnv).find? n | return false
    let args := e.getAppArgs
    if args.size != c.numParams + c.numFields then return false
    for arg in args[:c.numParams] do
      if !(← concreteType arg) then return false
    for arg in args[c.numParams:] do
      if ← Meta.isProof arg then
        if !(← concreteType (← Meta.inferType arg)) then return false
      else if !(← concreteData arg) then return false
    return true

#eval show CommandElabM Unit from do
  let env ← getEnv
  for r in ConcreteSpec.Core.registeredRules do
    let doc := (← findDocString? env r.name).getD ""
    IO.println (Json.compress (Json.mkObj [("kind", "rule"), ("name", toString r.name),
      ("rev", r.rev), ("exists", toJson (env.contains r.name)), ("doc", doc)]))
  for e in ConcreteSpec.Core.registeredRuleExamples do
    let ci := env.find? e.declaration
    let axioms ← match ci with
      | some _ => liftCoreM (collectAxioms e.declaration)
      | none => pure #[]
    let proved := match ci with
      | some (.thmInfo _) => true
      | _ => false
    let (verdict, usesRule) ← liftTermElabM do
      let some c := ci | return ("Invalid", false)
      let ty := c.type
      let args := ty.getAppArgs
      let head := ty.getAppFn.constName?
      let verdict := if head == some ``ConcreteSpec.Core.RuleAccepted then "Accepted"
        else if head == some ``ConcreteSpec.Core.RuleRefused then "Refused" else "Invalid"
      if verdict == "Invalid" || args.size != 3 then return ("Invalid", false)
      let linked := args[1]!.isConstOf e.rule
      let concrete ← concreteData args[2]!
      return (if concrete then verdict else "Invalid", linked)
    let trusted := axioms.all fun a => [``propext, ``Classical.choice, ``Quot.sound].contains a
    IO.println (Json.compress (Json.mkObj [("kind", "example"), ("rule", toString e.rule),
      ("name", toString e.declaration),
      ("verdict", verdict),
      ("elaborated", toJson (proved && trusted)), ("usesRule", toJson usesRule)]))
  IO.println (Json.compress (Json.mkObj [("end", toJson true)]))
