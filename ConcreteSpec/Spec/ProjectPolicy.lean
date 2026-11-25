import ConcreteSpec.Spec.Ident

/-!
# Project admission and policy over facts

PROPOSED: part of the specification before Core freeze. The typed boundary has no dependency
on the TOML field spelling or on the compiler backend. See R-0576, R-0589 and R-0603.
-/

namespace ConcreteSpec.Project
open ConcreteSpec.Core (RuleId)

-- A language rules identity is independent of the product, facts schema and spec header versions.
structure LanguageRulesVersion where
  major : Nat
  minor : Nat
  deriving DecidableEq, Repr

inductive LanguageSelection where
  | standalone
  | project (pin : Option LanguageRulesVersion)
  deriving DecidableEq, Repr

/-- A single-file invocation selects the sole supported rules version. A manifest must supply
that same version explicitly. The slice's compiler supplies its supported identity.
See R-0603 and docs/decisions.md#decided-project-concrete-toml. -/
def LanguagePinAccepted (request : LanguageRulesVersion × LanguageSelection) : Prop :=
  match request.2 with
  | .standalone => True
  | .project pin => pin = some request.1

instance (request : LanguageRulesVersion × LanguageSelection) :
    Decidable (LanguagePinAccepted request) :=
  match request with
  | (_, .standalone) => inferInstanceAs (Decidable True)
  | (supported, .project pin) => inferInstanceAs (Decidable (pin = some supported))

def languagePinRule : RuleId := ⟨``LanguagePinAccepted, 1⟩

inductive PinRefusal where
  | missing
  | unsupported (requested supported : LanguageRulesVersion)
  deriving DecidableEq, Repr

def selectLanguage (supported : LanguageRulesVersion) (selection : LanguageSelection) :
    Except PinRefusal LanguageRulesVersion :=
  if LanguagePinAccepted (supported, selection) then .ok supported
  else
    match selection with
    | .project (some requested) => .error (.unsupported requested supported)
    | _ => .error .missing

inductive AssumptionKind where
  | target | heap | stack | foreignFunction | trustedSurface
  deriving DecidableEq, Repr

structure AssumptionId where
  kind : AssumptionKind
  name : String
  deriving DecidableEq, Repr

/-- Established evidence retains its dependencies. Missing evidence and known violations are
different cases and both carry the reported source. -/
inductive Evidence (α : Type) where
  | established (value : α) (premises : List AssumptionId) (source : String)
  | unknown (source : String)
  deriving Repr

inductive Clause where
  | noAlloc | noBlocking | noForeignCalls | terminates | stackBound | maxStackBytes (bytes : Nat)
  deriving DecidableEq, Repr

structure FunctionFacts where
  name : String
  allocates : Evidence Bool
  blocks : Evidence Bool
  callsForeign : Evidence Bool
  terminates : Evidence Bool
  stackBytes : Evidence Nat
  deriving Repr

/-- Each scope carries its own explicit assumption allowance. Conjunction preserves the
allowance of every inherited clause, including clauses repeated in a module or dependency. -/
structure Requirement where
  clause : Clause
  acceptableAssumptions : List AssumptionId
  deriving DecidableEq, Repr

inductive Violation where
  | unknown | contradicted | unacceptedAssumption (id : AssumptionId)
  deriving DecidableEq, Repr

structure PolicyRefusal where
  function : String
  clause : Clause
  source : String
  violation : Violation
  deriving DecidableEq, Repr

private def checkEvidence {α : Type} (requirement : Requirement) (function : String)
    (test : α → Bool) : Evidence α → Except PolicyRefusal Unit
  | .unknown source => .error ⟨function, requirement.clause, source, .unknown⟩
  | .established value premises source =>
    if !test value then .error ⟨function, requirement.clause, source, .contradicted⟩
    else
      match premises.find? (fun id => !requirement.acceptableAssumptions.contains id) with
      | some id => .error ⟨function, requirement.clause, source, .unacceptedAssumption id⟩
      | none => .ok ()

def checkClause (requirement : Requirement) (facts : FunctionFacts) :
    Except PolicyRefusal Unit :=
  let check := fun {α : Type} => @checkEvidence α requirement facts.name
  match requirement.clause with
  | .noAlloc => check not facts.allocates
  | .noBlocking => check not facts.blocks
  | .noForeignCalls => check not facts.callsForeign
  | .terminates => check id facts.terminates
  | .stackBound => check (fun _ => true) facts.stackBytes
  | .maxStackBytes bytes => check (fun used => used ≤ bytes) facts.stackBytes

/-- One explicit clause is checked against precisely the corresponding fact, with affirmative
evidence and only explicitly accepted premises. See R-0576 and
docs/decisions.md#decided-project-predictable-policy. -/
def ClauseAccepted (input : Requirement × FunctionFacts) : Prop :=
  (checkClause input.1 input.2).isOk = true

instance (input : Requirement × FunctionFacts) : Decidable (ClauseAccepted input) :=
  inferInstanceAs (Decidable (_ = true))

def clauseRule : RuleId := ⟨``ClauseAccepted, 1⟩

def PolicyRefusal.ruleId (_ : PolicyRefusal) : RuleId := clauseRule

/-- The mandatory clauses of R-0576, with an explicit set of permitted premises. Additional
requirements can be conjoined by callers. -/
def predictable (permitted : List AssumptionId) : List Requirement :=
  [.noAlloc, .noBlocking, .noForeignCalls].map (fun clause => ⟨clause, permitted⟩)

-- Table schema identities are assigned by the eventual wire schema. Each table is checked
-- independently, even when there is only one supported schema for both.
structure VersionedTable (α : Type) where
  revision : Nat
  value : α
  deriving Repr

structure Tables where
  policy : Option (VersionedTable (List Requirement))
  assumptions : Option (VersionedTable (List AssumptionId))
  deriving Repr

inductive TableKind where
  | policy | assumptions
  deriving DecidableEq, Repr

structure VersionRefusal where
  table : TableKind
  received : Nat
  supported : Nat
  deriving DecidableEq, Repr

private def checkVersion {α : Type} (kind : TableKind) (supported : Nat)
    (table : Option (VersionedTable α)) : Except VersionRefusal Unit := do
  if let some entry := table then
    unless entry.revision == supported do
      throw ⟨kind, entry.revision, supported⟩

def Tables.checkVersions (tables : Tables) (policy assumptions : Nat) :
    Except VersionRefusal Unit := do
  checkVersion .policy policy tables.policy
  checkVersion .assumptions assumptions tables.assumptions

/-- Each supplied table has to use its supported schema revision. The revisions are supplied by
the decoder. See R-0589 and docs/decisions.md#decided-project-concrete-toml. -/
def TableVersionsAccepted (input : Tables × Nat × Nat) : Prop :=
  (input.1.checkVersions input.2.1 input.2.2).isOk = true

instance (input : Tables × Nat × Nat) : Decidable (TableVersionsAccepted input) :=
  inferInstanceAs (Decidable (_ = true))

def tableVersionRule : RuleId := ⟨``TableVersionsAccepted, 1⟩

def VersionRefusal.ruleId (_ : VersionRefusal) : RuleId := tableVersionRule

structure AssumptionDiff where
  added : List AssumptionId
  removed : List AssumptionId
  deriving DecidableEq, Repr

/-- Compare declarations as sets. Moving entries cannot create a change in requirements. -/
def assumptionDiff (previous current : List AssumptionId) : AssumptionDiff :=
  ⟨(current.filter (fun id => !previous.contains id)).eraseDups,
   (previous.filter (fun id => !current.contains id)).eraseDups⟩

end ConcreteSpec.Project
