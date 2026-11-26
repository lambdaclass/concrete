import ConcreteSpec.Spec.Ident

/-!
# Project admission and policy over facts

PROPOSED: part of the specification before Core freeze. The typed boundary has no dependency
on the TOML field spelling or on the compiler backend. See R-0576, R-0589 and R-0603.
-/

namespace ConcreteSpec.Project
open ConcreteSpec.Core (RuleId FnId PremiseId SiteId)

-- A language rules identity is independent of the product, facts schema and spec header versions.
structure LanguageRulesVersion where
  major : Nat
  minor : Nat
  deriving DecidableEq, Repr

/-- The sole language rules implemented by this pre-freeze slice. -/
def supportedLanguage : LanguageRulesVersion := ⟨0, 1⟩

inductive LanguageSelection where
  | standalone
  | project (pin : Option LanguageRulesVersion)
  deriving DecidableEq, Repr

/-- A single-file invocation selects the sole supported rules version. A manifest must supply
that same version explicitly. The slice's compiler supplies its supported identity.
The selection behavior was approved in the delegated slice 1 decision (item 4,
2026-11-25). R-0603 and docs/decisions.md#decided-project-concrete-toml track the
project configuration and its remaining schema work. -/
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

/-- A project invocation may proceed only with an available, valid and interpreted
configuration (R-0589, docs/decisions.md#decided-project-concrete-toml).
An absent entry is governed separately by the language selection rule. -/
def ManifestAdmitted (ready : Bool) : Prop := ready = true

instance (ready : Bool) : Decidable (ManifestAdmitted ready) :=
  inferInstanceAs (Decidable (ready = true))

def manifestRule : RuleId := ⟨``ManifestAdmitted, 1⟩

inductive PinRefusal where
  | missing
  | unsupported (requested supported : LanguageRulesVersion)
  deriving DecidableEq, Repr

def PinRefusal.ruleId (_ : PinRefusal) : RuleId := languagePinRule

def selectLanguage (supported : LanguageRulesVersion) (selection : LanguageSelection) :
    Except PinRefusal LanguageRulesVersion :=
  if LanguagePinAccepted (supported, selection) then .ok supported
  else
    match selection with
    | .project (some requested) => .error (.unsupported requested supported)
    | _ => .error .missing

theorem selectLanguage_correct (supported : LanguageRulesVersion) (selection : LanguageSelection) :
    selectLanguage supported selection = .ok supported ↔ LanguagePinAccepted (supported, selection) := by
  unfold selectLanguage
  split
  · rename_i h
    simp [h]
  · rename_i h
    cases selection with
    | standalone => simp [h]
    | project pin => cases pin <;> simp [h]

/-- An analysis can report function-wide evidence or identify a particular site. -/
inductive EvidenceOrigin where
  | function
  | site (id : SiteId)
  deriving DecidableEq, Repr

/-- Established evidence retains its dependencies. Missing evidence and known violations are
different cases and both carry the reported source. -/
inductive Evidence (α : Type) where
  | established (value : α) (premises : List PremiseId) (source : EvidenceOrigin)
  | unknown (source : EvidenceOrigin)
  deriving Repr

inductive Clause where
  | noAlloc | noBlocking | noForeignCalls | terminates | stackBound | maxStackBytes (bytes : Nat)
  deriving DecidableEq, Repr

structure FunctionFacts where
  name : FnId
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
  acceptableAssumptions : List PremiseId
  deriving DecidableEq, Repr

inductive Violation where
  | unknown | contradicted | unacceptedAssumption (id : PremiseId)
  deriving DecidableEq, Repr

structure PolicyRefusal where
  function : FnId
  clause : Clause
  source : EvidenceOrigin
  violation : Violation
  deriving DecidableEq, Repr

private def checkEvidence {α : Type} (requirement : Requirement) (function : FnId)
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

private def Evidence.accepted {α : Type} (allowed : List PremiseId) (test : α → Bool) :
    Evidence α → Bool
  | .unknown _ => false
  | .established v premises _ => test v && premises.all allowed.contains

/-- One explicit clause is checked against precisely the corresponding fact, with affirmative
evidence and only explicitly accepted premises. See R-0576 and
docs/decisions.md#decided-project-predictable-policy. -/
def ClauseAccepted (input : Requirement × FunctionFacts) : Prop :=
  let (r, f) := input
  let accepted := fun {α : Type} => @Evidence.accepted α r.acceptableAssumptions
  (match r.clause with
   | .noAlloc => accepted not f.allocates
   | .noBlocking => accepted not f.blocks
   | .noForeignCalls => accepted not f.callsForeign
   | .terminates => accepted id f.terminates
   | .stackBound => accepted (fun _ => true) f.stackBytes
   | .maxStackBytes bytes => accepted (fun used => used ≤ bytes) f.stackBytes) = true

instance (input : Requirement × FunctionFacts) : Decidable (ClauseAccepted input) :=
  inferInstanceAs (Decidable (_ = true))

private theorem checkEvidence_correct {α : Type} (r : Requirement) (f : FnId)
    (test : α → Bool) (e : Evidence α) :
    (checkEvidence r f test e).isOk = e.accepted r.acceptableAssumptions test := by
  cases e with
  | unknown => rfl
  | established v premises source =>
    by_cases h : test v = true
    · simp only [checkEvidence, Evidence.accepted, h, Bool.not_true, Bool.false_eq_true,
        ↓reduceIte, Bool.true_and]
      induction premises with
      | nil => rfl
      | cons p ps ih =>
        by_cases hp : p ∈ r.acceptableAssumptions
        · simpa [List.find?, List.all, hp] using ih
        · simp [List.find?, List.all, hp]
          rfl
    · cases hv : test v <;> simp_all [checkEvidence, Evidence.accepted] <;> rfl

theorem checkClause_correct (r : Requirement) (f : FunctionFacts) :
    (checkClause r f).isOk = true ↔ ClauseAccepted (r, f) := by
  rcases r with ⟨clause, allowed⟩
  cases clause <;> exact iff_of_eq (congrArg (· = true) (checkEvidence_correct _ _ _ _))

def clauseRule : RuleId := ⟨``ClauseAccepted, 1⟩

def PolicyRefusal.ruleId (_ : PolicyRefusal) : RuleId := clauseRule

/-- The mandatory clauses of R-0576, with an explicit set of permitted premises. Additional
requirements can be conjoined by callers. -/
def predictable (permitted : List PremiseId) : List Requirement :=
  [.noAlloc, .noBlocking, .noForeignCalls].map (fun clause => ⟨clause, permitted⟩)

-- Table schema identities are assigned by the eventual wire schema. Each table is checked
-- independently, even when there is only one supported schema for both.
structure VersionedTable (α : Type) where
  revision : Nat
  value : α
  deriving Repr

structure Tables where
  policy : Option (VersionedTable (List Requirement))
  assumptions : Option (VersionedTable (List PremiseId))
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

private def tableMatches {α : Type} (revision : Nat) (table : Option (VersionedTable α)) : Bool :=
  match table with
  | none => true
  | some entry => entry.revision == revision

/-- Each supplied table has to use its supported schema revision. The revisions are supplied by
the decoder. See R-0589 and docs/decisions.md#decided-project-concrete-toml. -/
def TableVersionsAccepted (input : Tables × Nat × Nat) : Prop :=
  tableMatches input.2.1 input.1.policy && tableMatches input.2.2 input.1.assumptions

instance (input : Tables × Nat × Nat) : Decidable (TableVersionsAccepted input) :=
  inferInstanceAs (Decidable (_ = true))

theorem Tables.checkVersions_correct (tables : Tables) (policy assumptions : Nat) :
    (tables.checkVersions policy assumptions).isOk = true ↔
      TableVersionsAccepted (tables, policy, assumptions) := by
  rcases tables with ⟨p, a⟩
  cases p <;> cases a <;>
    simp only [Tables.checkVersions, checkVersion, TableVersionsAccepted, tableMatches]
  · rfl
  · split <;> simp_all <;> rfl
  · split <;> simp_all <;> rfl
  · split <;> split <;> simp_all <;> rfl

def tableVersionRule : RuleId := ⟨``TableVersionsAccepted, 1⟩

def VersionRefusal.ruleId (_ : VersionRefusal) : RuleId := tableVersionRule

structure AssumptionDiff where
  added : List PremiseId
  removed : List PremiseId
  deriving DecidableEq, Repr

/-- Compare declarations as sets. Moving entries cannot create a change in requirements. -/
def assumptionDiff (previous current : List PremiseId) : AssumptionDiff :=
  ⟨(current.filter (fun id => !previous.contains id)).eraseDups,
   (previous.filter (fun id => !current.contains id)).eraseDups⟩

/-- Assumption diffs compare membership, independent of order and multiplicity. -/
theorem assumptionDiff_mem (previous current : List PremiseId) (id : PremiseId) :
    (id ∈ (assumptionDiff previous current).added ↔ id ∈ current ∧ id ∉ previous) ∧
    (id ∈ (assumptionDiff previous current).removed ↔ id ∈ previous ∧ id ∉ current) := by
  simp [assumptionDiff, and_comm]

end ConcreteSpec.Project
