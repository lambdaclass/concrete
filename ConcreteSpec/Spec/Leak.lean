import ConcreteSpec.Spec.Exec

/-!
# Secret leakage and release

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`. Foundation contract C7 under docs/decisions.md#decided-2026-11-08-foundation,
docs/decisions.md#decided-project-infoflow-2026-11-06.
Secret, no-leak and constant-time remain in scope under
docs/decisions.md#decided-2026-11-16-secret-wiping, which defers wiping out of the spec.

Leakage is what the baseline timing observer sees, not ordinary output. It is separate from public
observations. `InstrumentedSteps` instruments C1's existing `Exec.Step` relation without changing
the machine: each step contributes public observations, leakage, ordered release records and the
external choices on which it depended. The operand machine installs these projections and proves that choices come from accepted
primitive answers. C6 can supply an external-interaction contract at this boundary.

`DelimitedReleaseNI` and `ConstantTime` are different claims about executions. Both assume low-equivalent
initial states, matched external choices and the same ordered release sites and values. NI requires
the same public observations; CT requires the same leakage. A release policy authorizes sites; it
does not discharge either claim. No scalar secret typing rule is chosen here (slice 7).

Byte accounting is owned separately by `Spec/Accounting.lean`. Secret typing, hardware timing,
binary constant time and speculation remain
in their named later slices or trust premises.
-/

namespace ConcreteSpec.Core.Leak
open ConcreteSpec.Core
open ConcreteSpec.Core.Exec

/-- One entry of the baseline timing-observer trace. Branch decisions, exact addresses, allocation
sizes, loop counts and operands of variable-time operations are distinct entries. Container lengths
are public; contracted external effects are recorded as such rather than confused with output. -/
inductive Leakage (Val Address Effect : Type) where
  | branch (site : SiteId) (decision : Bool)
  | address (site : SiteId) (address : Address)
  | allocation (lineage : LineageId) (site : SiteId) (bytes : Nat)
  | loop (loop : LoopId) (iterations : Nat)
  | variableTime (site : SiteId) (operands : List Val)
  | publicLength (site : SiteId) (length : Nat)
  | external (effect : Effect)

/-- Every leakage constructor has a nonempty representative. -/
def leakageCatalog : List (Leakage Nat Nat Nat) :=
  [.branch ⟨0⟩ true, .address ⟨1⟩ 4096, .allocation ⟨2⟩ ⟨3⟩ 8,
    .loop ⟨4⟩ 3, .variableTime ⟨5⟩ [1, 2], .publicLength ⟨6⟩ 2, .external 7]

/-- Representation lemma. Concrete length check showing `leakageCatalog` covers all seven `Leakage` constructors. -/
theorem leakage_catalog_size : leakageCatalog.length = 7 := by decide

/-- Representation lemma. Concrete inequality check distinguishing `Leakage.branch` true and false decisions at the same site.

Planted changed-event controls for every constructor. -/
theorem branch_decision_visible : (show Leakage Nat Nat Nat from .branch ⟨0⟩ true) ≠ .branch ⟨0⟩ false := by simp

/-- Representation lemma. Concrete inequality check distinguishing `Leakage.address` entries with different addresses. -/
theorem address_visible : (show Leakage Nat Nat Nat from .address ⟨1⟩ 4096) ≠ .address ⟨1⟩ 8192 := by simp

/-- Representation lemma. Concrete inequality check distinguishing `Leakage.allocation` entries with different byte counts. -/
theorem allocation_bytes_visible : (show Leakage Nat Nat Nat from .allocation ⟨2⟩ ⟨3⟩ 8) ≠ .allocation ⟨2⟩ ⟨3⟩ 9 := by simp

/-- Representation lemma. Concrete inequality check distinguishing `Leakage.loop` entries with different iteration counts. -/
theorem loop_count_visible : (show Leakage Nat Nat Nat from .loop ⟨4⟩ 3) ≠ .loop ⟨4⟩ 4 := by simp

/-- Representation lemma. Concrete inequality check distinguishing `Leakage.variableTime` entries with different operand values. -/
theorem variable_time_operand_visible : (show Leakage Nat Nat Nat from .variableTime ⟨5⟩ [1]) ≠ .variableTime ⟨5⟩ [2] := by simp

/-- Representation lemma. Concrete inequality check distinguishing `Leakage.publicLength` entries with different container lengths. -/
theorem public_length_visible : (show Leakage Nat Nat Nat from .publicLength ⟨6⟩ 2) ≠ .publicLength ⟨6⟩ 3 := by simp

/-- Representation lemma. Concrete inequality check distinguishing `Leakage.external` entries with different effect payloads. -/
theorem external_effect_visible : (show Leakage Nat Nat Nat from .external 7) ≠ .external 8 := by simp

/-- A typed release site. The site identifies the operation in the facts; `ty` is the type of the
value explicitly declared for release there. -/
structure ReleaseSite (Ty : Type) where
  site : SiteId
  ty : Ty
  deriving DecidableEq, Repr

/-- One semantic release: exactly a typed site and its explicitly released value. Its order in
an execution matters. Source reasons and call paths belong to `ReleaseFact`, outside semantic run
types; fact text cannot become an input to execution or strengthen the security premise.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
structure Release (Ty Val : Type) where
  site : ReleaseSite Ty
  value : Val
  deriving DecidableEq, Repr

/-- A release fact retains the semantic release plus its source reason and call path. This is a
facts record, not a field of the execution instrumentation. The source/site facts producer supplies
metadata; the execution traces and policies consume only its `release` projection.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
structure ReleaseFact (Ty Val : Type) where
  release : Release Ty Val
  reason : String
  path : List FnId
  deriving DecidableEq, Repr

/-- The security comparison keeps exactly the ordered typed sites and released values.
Reason and call-path metadata remain available to facts but do not strengthen the premise.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
def releaseTrace {Ty Val : Type} (rs : List (Release Ty Val)) : List (ReleaseSite Ty × Val) :=
  rs.map fun r => (r.site, r.value)

/-- Erase fact-only metadata before forming the semantic release premise. -/
def ReleaseFact.semanticTrace {Ty Val : Type} (facts : List (ReleaseFact Ty Val)) :
    List (ReleaseSite Ty × Val) := releaseTrace (facts.map ReleaseFact.release)

/-- The release-policy interface: an exhaustive list of permitted typed sites. An empty list
forbids every release. Authorization says nothing about observations or leakage elsewhere. -/
structure ReleasePolicy (Ty : Type) where
  allowed : List (ReleaseSite Ty)

/-- The policy permits this exact typed site; the released value does not affect site authorization.
Reason/path metadata is outside this semantic record. Authority: docs/decisions.md#decided-2026-11-08-foundation (C7);
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def ReleasePolicy.Permits {Ty Val : Type} (policy : ReleasePolicy Ty) (r : Release Ty Val) : Prop :=
  r.site ∈ policy.allowed

/-- A policy with no permitted sites forbids release. Authority: docs/decisions.md#decided-2026-11-08-foundation (C7);
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def ReleasePolicy.ForbidsRelease {Ty : Type} (policy : ReleasePolicy Ty) : Prop :=
  policy.allowed = []

/-- Every release in an ordered execution trace is authorized by the policy. Authority: docs/decisions.md#decided-2026-11-08-foundation (C7);
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def ReleasePolicy.Authorized {Ty Val : Type} (policy : ReleasePolicy Ty)
    (rs : List (Release Ty Val)) : Prop :=
  ∀ r ∈ rs, policy.Permits r

/-- Instance. Concrete acceptance check that a single release at the listed site satisfies `ReleasePolicy.Authorized`.

Accepted: a release at the one listed site is authorized. Refused: another typed site is not. -/
theorem listed_release_authorized : (ReleasePolicy.mk [⟨⟨0⟩, ()⟩]).Authorized
    [⟨⟨⟨0⟩, ()⟩, 7⟩] := by
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits]

/-- Instance. Concrete refusal check that a release at an unlisted site fails `ReleasePolicy.Authorized`. -/
theorem unlisted_release_unauthorized : ¬ (ReleasePolicy.mk [⟨⟨0⟩, ()⟩]).Authorized
    [⟨⟨⟨1⟩, ()⟩, 7⟩] := by
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits]

/-- Representation lemma. Concrete unfolding check that an empty allowed-site list satisfies `ReleasePolicy.ForbidsRelease`. -/
theorem empty_policy_forbids_release : (ReleasePolicy.mk ([] : List (ReleaseSite Unit))).ForbidsRelease := rfl

/-- Instance. Concrete refusal check that a nonempty release trace fails `ReleasePolicy.Authorized` under the empty policy.

A concrete nonempty release trace is unauthorized under the empty policy; the general law
`forbidsRelease_not_authorized` is proved in `ConcreteSpec/Proofs/Leak.lean`. -/
theorem empty_policy_refuses_populated_trace : ¬ (ReleasePolicy.mk ([] : List (ReleaseSite Unit))).Authorized
    [⟨⟨⟨0⟩, ()⟩, 7⟩] := by
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits]

/-- Typed-site, value and ordering controls for release comparison and authorization. -/
def typedRelease (ty : String) (value : Nat) : Release String Nat :=
  ⟨⟨⟨0⟩, ty⟩, value⟩
def typedReleasePolicy : ReleasePolicy String := ⟨[⟨⟨0⟩, "public"⟩]⟩

/-- Representation lemma. Concrete check that `typedReleasePolicy` permits a release at site 0 with matching type `"public"`. -/
theorem exact_type_permitted : typedReleasePolicy.Permits (typedRelease "public" 7) := by
  simp [typedReleasePolicy, ReleasePolicy.Permits, typedRelease]

/-- Representation lemma. Concrete check that `typedReleasePolicy` rejects a release at site 0 with mismatched type `"secret"`. -/
theorem different_type_refused : ¬ typedReleasePolicy.Permits (typedRelease "secret" 7) := by
  simp [typedReleasePolicy, ReleasePolicy.Permits, typedRelease]

/-- Representation lemma. Concrete inequality check showing `Release` records with distinct released values are not equal. -/
theorem release_values_distinguished : typedRelease "public" 7 ≠ typedRelease "public" 8 := by decide

/-- Representation lemma. Concrete inequality check showing ordered release traces with swapped elements are not equal. -/
theorem release_order_distinguished : [typedRelease "public" 7, typedRelease "public" 8] ≠
    [typedRelease "public" 8, typedRelease "public" 7] := by decide

/-- What is public in a C1 state, parametric in a labelling. `project` exposes the public view of a
value, or none for a secret; `lowStore` relates the public parts of shared state. -/
structure Labelling (O : Operands) where
  PublicValue : Type
  project : O.Val → Option PublicValue
  lowStore : O.Store → O.Store → Prop

/-- The public local bindings of a configuration. Secret bindings and their values are absent. -/
def publicLocals {O : Operands} (L : Labelling O) (c : Config O) :
    AList Var L.PublicValue :=
  c.locals.filterMap fun e => (L.project e.2).map fun v => (e.1, v)

/-- Low-equivalence: equal public local bindings and related public shared state. Secret values may
differ. Frames and control are execution data, not silently made public by this definition. Authority: docs/decisions.md#decided-2026-11-08-foundation (C7);
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def LowEquivalent {O : Operands} (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  publicLocals L c₁ = publicLocals L c₂ ∧ L.lowStore c₁.store c₂.store

/-- C1 instrumentation for C7. Each step projection observes an existing machine transition.
The machine observer qualifies the projections used in NI and CT. The final observation
includes termination when the machine exposes it. The timing and public traces are separate. -/
structure Instrumentation (O : Operands) where
  Address : Type
  Effect : Type
  Observation : Type
  Choice : Type
  observeStep : Config O → Label O → Config O → List Observation
  observeFinal : Config O → Option Observation
  leakage : Config O → Label O → Config O → List (Leakage O.Val Address Effect)
  releases : Config O → Label O → Config O → List (Release O.Ty O.Val)
  choices : Config O → Label O → Config O → List Choice

/-- The observing semantics installed by the operand machine. The external-choice contract
projects only accepted primitive answers. It can omit deterministic primitive operations.
Internal transitions consume no external choices. The machine supplies its public observations,
leakage and releases together with this contract, independently of a security claim. -/
class MachineObserver (O : Operands) where
  instrumentation : Instrumentation O
  externalChoice : O.Prim → List O.Val → SiteId → Answer O.Val O.Event O.Store →
    Option instrumentation.Choice
  internalChoices {P : Program O} {c d : Config O} :
    step P c = some (.next d) → instrumentation.choices c .silent d = []
  answerChoices {P : Program O} {c d : Config O} {p vs site st k a} :
    step P c = some (.request p vs site st k) → O.allows p vs st a = true →
    k a = some d → instrumentation.choices c a.label d = (externalChoice p vs site a).toList

/-- Qualification fixes all projections to the operand machine's installed semantics. -/
def ObserverQualified {O : Operands} [M : MachineObserver O] (I : Instrumentation O) : Prop :=
  I = M.instrumentation

/-- C1 steps with their C7 traces in execution order. A primitive step is still answered within its
C1 contract; instrumentation only observes the transition and cannot authorize a new one. -/
inductive InstrumentedSteps {O : Operands} (I : Instrumentation O) (P : Program O) :
    Config O → List I.Observation → List (Leakage O.Val I.Address I.Effect) →
      List (Release O.Ty O.Val) → List I.Choice → Config O → Prop where
  | refl (c : Config O) : InstrumentedSteps I P c [] [] [] [] c
  | step {c l c' obs leaks rs choices c''} (h : Step P c l c')
      (rest : InstrumentedSteps I P c' obs leaks rs choices c'') :
      InstrumentedSteps I P c (I.observeStep c l c' ++ obs) (I.leakage c l c' ++ leaks)
        (I.releases c l c' ++ rs) (I.choices c l c' ++ choices) c''

/-- One complete finite instrumented execution. Its proof is a C1 execution, not a separate
semantics, and `ending` is its C1 final outcome. -/
structure Run {O : Operands} (I : Instrumentation O) (P : Program O) (initial : Config O) where
  final : Config O
  ending : Final O.Val
  finalEnding : final.final? = some ending
  observations : List I.Observation
  leakage : List (Leakage O.Val I.Address I.Effect)
  releases : List (Release O.Ty O.Val)
  choices : List I.Choice
  steps : InstrumentedSteps I P initial observations leakage releases choices final

/-- Facts attached to one identified semantic release list. Index by that list, rather than
by a machine configuration: diagnostic strings never share a carrier with numeric execution
state. A projection theorem connects these facts to a run's releases in the proof layer.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
structure RunFacts {Ty Val : Type} (releases : List (Release Ty Val)) where
  entries : List (ReleaseFact Ty Val)
  erases : entries.map ReleaseFact.release = releases

/-- Attach fact-only metadata to every semantic release, preserving its order. This constructor
establishes semantic correspondence; it does not certify the source reason or path. -/
def ReleaseFact.attach {Ty Val : Type} (releases : List (Release Ty Val))
    (reason : Release Ty Val → String) (path : Release Ty Val → List FnId) : RunFacts releases where
  entries := releases.map fun release => ⟨release, reason release, path release⟩
  erases := by simp [List.map_map, Function.comp_def]

/-- All public observations of a run, including its final observation when it has one. -/
def Run.publicObservations {O : Operands} {I : Instrumentation O} {P : Program O} {c : Config O}
    (r : Run I P c) : List I.Observation :=
  r.observations ++ (I.observeFinal r.final).toList

/-- Every reachable finite execution prefix from `c` emits only permitted releases, including
prefixes of stuck or diverging executions. Authority: docs/decisions.md#decided-2026-11-08-foundation (C7);
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def ReleasePolicy.RunsAuthorized {O : Operands} (policy : ReleasePolicy O.Ty)
    (I : Instrumentation O) (P : Program O) (c : Config O) : Prop :=
  ∀ ⦃obs leaks rs choices d⦄, InstrumentedSteps I P c obs leaks rs choices d → policy.Authorized rs

/-- Public container-length metadata is observable independently of the timing-only entries.
Authority: docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def publicLengths {Val Address Effect : Type} (trace : List (Leakage Val Address Effect)) :
    List (SiteId × Nat) := trace.filterMap fun e =>
  match e with
  | .publicLength site length => some (site, length)
  | _ => none

/-- The run comparison underlying delimited-release noninterference. For
low-equivalent initial states, matched external choices and equal ordered release traces, every
pair of complete finite runs has equal public observations. No equality of leakage is asserted. Authority: docs/decisions.md#decided-2026-11-08-foundation (C7);
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def NIComparison {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ∀ (r₁ : Run I P c₁) (r₂ : Run I P c₂),
    LowEquivalent L c₁ c₂ → r₁.choices = r₂.choices → releaseTrace r₁.releases = releaseTrace r₂.releases →
      r₁.publicObservations = r₂.publicObservations ∧
        publicLengths r₁.leakage = publicLengths r₂.leakage

/-- The run comparison underlying constant time between the same entries and under the same delimited-release premises: the
complete baseline leakage traces are equal. This is separate from `DelimitedReleaseNI`, not a
consequence of authorization. Authority: docs/decisions.md#decided-2026-11-08-foundation (C7);
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def TimingComparison {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ∀ (r₁ : Run I P c₁) (r₂ : Run I P c₂),
    LowEquivalent L c₁ c₂ → r₁.choices = r₂.choices → releaseTrace r₁.releases = releaseTrace r₂.releases →
      r₁.leakage = r₂.leakage


/-- Delimited-release noninterference uses the machine's qualified observing semantics. -/
def DelimitedReleaseNI {O : Operands} [MachineObserver O] (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ObserverQualified I ∧ NIComparison I P L c₁ c₂

/-- Constant time uses the same qualified observing semantics and compares baseline leakage. -/
def ConstantTime {O : Operands} [MachineObserver O] (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ObserverQualified I ∧ TimingComparison I P L c₁ c₂

end ConcreteSpec.Core.Leak
