-- The gate appends this same exporter body to fixtures after their declarations.
import ConcreteSpec.Spec.Rules
import Lean
open Lean Elab Command

-- EXPORTER BODY
/-- Inspect the complete declaration graph before reduction can erase dependencies.
Every kernel declaration kind has an explicit policy. Signatures, theorem proofs,
mutual inductives and recursor equations share one visited set, including cycles. -/
def concreteType (e : Expr) : MetaM Bool := do
  if e.hasFVar || e.hasMVar || e.hasLooseBVars then return false
  let mut pending := e.getUsedConstants
  let mut seen : NameSet := {}
  while !pending.isEmpty do
    let n := pending.back!
    pending := pending.pop
    if seen.contains n then continue
    seen := seen.insert n
    let some ci := (← getEnv).find? n | return false
    if ci.isUnsafe then return false
    pending := pending ++ ci.type.getUsedConstants
    match ci with
    | .opaqueInfo _ => return false
    | .axiomInfo _ =>
      if ![``propext, ``Classical.choice, ``Quot.sound].contains n then return false
    | .defnInfo d =>
      if (← getReducibilityStatus n) == .irreducible then return false
      pending := pending ++ d.value.getUsedConstants
    | .thmInfo t => pending := pending ++ t.value.getUsedConstants
    | .inductInfo i => pending := pending ++ i.all.toArray ++ i.ctors.toArray
    | .ctorInfo c => pending := pending.push c.induct
    | .recInfo r =>
      pending := pending ++ r.all.toArray
      for rule in r.rules do
        pending := (pending.push rule.ctor) ++ rule.rhs.getUsedConstants
    | .quotInfo q =>
      -- Quotient primitives have kernel computation rules, not Lean bodies.
      -- Their signatures above remain part of the graph.
      match q.kind with
      | .type | .ctor | .lift | .ind => pure ()
  return true

/-- Concrete data normalizes completely to constructors and literals. Proofs are
permitted only as fields of concrete data, and only after checking their proposition.
The declaration traversal also checks proof terms before reduction. -/
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
    -- Instantiate each declared domain before inspecting the next one. Inferred
    -- argument types may be definitionally equal but omit an erased annotation.
    let mut signature ← Meta.inferType e.getAppFn
    for arg in args do
      let .forallE _ domain body _ := signature | return false
      if !(← concreteType domain) then return false
      signature := body.instantiate1 arg
    for arg in args[:c.numParams] do
      if !(← concreteType arg) then return false
    for arg in args[c.numParams:] do
      if ← Meta.isProof arg then
        if !(← concreteType arg) || !(← concreteType (← Meta.inferType arg)) then return false
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
    let proved := match ci with
      | some (.thmInfo _) => true
      | _ => false
    -- Trust and concreteness use the same closed graph policy, including opaque
    -- dependencies that Lean.collectAxioms intentionally does not enumerate.
    let trusted ← liftTermElabM do
      if ci.isNone then return false
      concreteType (mkConst e.declaration)
    let (verdict, usesRule) ← liftTermElabM do
      let some c := ci | return ("Invalid", false)
      if !c.levelParams.isEmpty then return ("Invalid", false)
      let ty := c.type
      let args := ty.getAppArgs
      let head := ty.getAppFn.constName?
      let verdict := if head == some ``ConcreteSpec.Core.RuleAccepted then "Accepted"
        else if head == some ``ConcreteSpec.Core.RuleRefused then "Refused" else "Invalid"
      if verdict == "Invalid" || args.size != 3 then return ("Invalid", false)
      let linked := args[1]!.isConstOf e.rule
      let concrete ← if trusted && (← concreteType args[0]!) then concreteData args[2]! else pure false
      return (if concrete then verdict else "Invalid", linked)
    IO.println (Json.compress (Json.mkObj [("kind", "example"), ("rule", toString e.rule),
      ("name", toString e.declaration),
      ("verdict", verdict),
      ("elaborated", toJson (proved && trusted)), ("usesRule", toJson usesRule)]))
  IO.println (Json.compress (Json.mkObj [("end", toJson true)]))
