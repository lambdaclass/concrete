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
external choices on which it depended. The observing semantics derives these projections from the machine and its primitive contract. C6 can supply an external-interaction contract at this boundary.

`DelimitedReleaseNI` and `ConstantTime` are different claims about executions. Both assume low-equivalent
initial states, matched external choices and the same ordered release sites and values. NI requires
prefix compatibility on public observations and public lengths; CT requires prefix compatibility
on leakage. Completed runs still compare their traces in full. A release policy authorizes sites; it
does not discharge either claim. No scalar secret typing rule is chosen here (slice 7).

Byte accounting is owned separately by `Spec/Accounting.lean`. Secret typing, hardware timing,
binary constant time and speculation remain
in their named later slices or trust premises.
-/

namespace ConcreteSpec.Core.Leak
open ConcreteSpec.Core
open ConcreteSpec.Core.Exec

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

/-- Public observations retain the exact C1 output and final-outcome payloads. -/
inductive Observation (Event Val : Type) where
  | output (event : Event)
  | final (outcome : Final Val)
  deriving DecidableEq, Repr

/-- C1 instrumentation for C7. Each step projection observes an existing machine transition.
The machine observer qualifies the projections used in authorization, NI and CT.
Final observations are used in the comparison of completed runs. Timing and public traces are separate. -/
structure Instrumentation (O : Operands) where
  observeStep : Config O → Label O → Config O → List (Observation O.Event O.Val)
  observeFinal : Config O → Option (Observation O.Event O.Val)
  leakage : Config O → Label O → Config O → Option (Answer O.Val O.Event O.Store) →
    List (Leakage O.Val O.Address O.Effect)
  releases : Config O → Label O → Config O → List (Release O.Ty O.Val)
  choices : Config O → Label O → Config O → Option (Answer O.Val O.Event O.Store) →
    List (Answer O.Val O.Event O.Store)

/-- Read the same evaluated request and post-evaluation store as the C1 dispatcher. -/
def primitiveRequest {O : Operands} (c : Config O) :
    Option (O.Prim × List O.Val × SiteId × O.Store) :=
  match c.ctrl with
  | .exec (.prim _ p args site) =>
    match evalArgs O c.locals c.store args with
    | .ok (vs, st) => some (p, vs, site, st)
    | _ => none
  | _ => none

/-- Record every evaluated branch and loop-test decision on the C1 transition.
A true loop test contributes one iteration and a false loop test contributes zero. -/
def controlLeakage {O : Operands} (c : Config O) : List (Leakage O.Val O.Address O.Effect) :=
  let decision (e : O.Expr) (site : SiteId) (loop : Option LoopId) :=
    match O.eval c.locals c.store e with
    | .ok (v, _) =>
      match O.test v with
      | some b => [.branch site b] ++ (loop.toList.map fun l => .loop l (if b then 1 else 0))
      | none => []
    | _ => []
  match c.ctrl, c.frames with
  | .exec (.cond e _ _ site), _ => decision e site none
  | .done, .test l _ e _ site _ :: _ => decision e site (some l)
  | _, _ => []

/-- The C1 oracle answer is an external choice only when the contract admits an alternative. -/
noncomputable def externalChoices {O : Operands} (c : Config O)
    (answer : Option (Answer O.Val O.Event O.Store)) : List (Answer O.Val O.Event O.Store) :=
  match answer with
  | none => []
  | some a =>
    match primitiveRequest c with
    | none => []
    | some (p, vs, _, st) =>
      open Classical in
      if O.allows p vs st a = true ∧ (∃ b, b ≠ a ∧ O.allows p vs st b = true) then [a] else []

/-- Shared projection construction, with an executable choice implementation when available. -/
abbrev instrumentationWithChoices (O : Operands)
    (choices : Config O → Option (Answer O.Val O.Event O.Store) →
      List (Answer O.Val O.Event O.Store)) : Instrumentation O where
  observeStep := fun _ label _ => label.events.map .output
  observeFinal := fun c => c.final?.map .final
  leakage := fun c _ _ answer => controlLeakage c ++
    (match primitiveRequest c with
    | some (p, vs, site, st) => O.primitiveLeakage p vs site st answer
    | none => [])
  releases := fun c _ _ =>
    match primitiveRequest c with
    | some (p, [v], site, _) => ((O.releaseType p).map fun ty => (⟨⟨site, ty⟩, v⟩ : Release O.Ty O.Val)).toList
    | _ => []
  choices := fun c _ _ answer => choices c answer

/-- All qualified projections are determined by C1 and the operand machine's primitive contract. -/
noncomputable def machineInstrumentation (O : Operands) : Instrumentation O :=
  instrumentationWithChoices O externalChoices

/-- Qualification has no replaceable instance. -/
def ObserverQualified {O : Operands} (I : Instrumentation O) : Prop :=
  I = machineInstrumentation O

/-- A C1 step indexed by its oracle answer, retained even if the continuation discards the result.
The constructors mirror `Exec.Step`; erasure forgets only the answer index. -/
inductive AnsweredStep {O : Operands} (P : Program O) :
    Config O → Label O → Config O → Option (Answer O.Val O.Event O.Store) → Prop where
  | silent {c c'} : step P c = some (.next c') → AnsweredStep P c .silent c' none
  | prim {c c' p vs site st k a} :
      step P c = some (.request p vs site st k) → O.allows p vs st a = true →
      k a = some c' → AnsweredStep P c a.label c' (some a)

/-- The four ordered traces accumulated by an instrumented C1 execution. -/
structure ExecutionTrace (O : Operands) where
  observations : List (Observation O.Event O.Val)
  leakage : List (Leakage O.Val O.Address O.Effect)
  releases : List (Release O.Ty O.Val)
  choices : List (Answer O.Val O.Event O.Store)

/-- The empty trace of a zero-step execution. -/
def ExecutionTrace.empty {O : Operands} : ExecutionTrace O := ⟨[], [], [], []⟩

/-- Join two consecutive trace contributions in execution order. -/
def ExecutionTrace.append {O : Operands} (a b : ExecutionTrace O) : ExecutionTrace O :=
  ⟨a.observations ++ b.observations, a.leakage ++ b.leakage,
    a.releases ++ b.releases, a.choices ++ b.choices⟩

/-- Record all projections of one step and its actual answer, when present. -/
def Instrumentation.trace {O : Operands} (I : Instrumentation O)
    (c : Config O) (l : Label O) (d : Config O)
    (a : Option (Answer O.Val O.Event O.Store)) : ExecutionTrace O :=
  ⟨I.observeStep c l d, I.leakage c l d a, I.releases c l d, I.choices c l d a⟩

/-- Every trace contribution is justified by an underlying C1 transition. -/
inductive InstrumentedExecution {O : Operands} (I : Instrumentation O) (P : Program O) :
    Config O → ExecutionTrace O → Config O → Prop where
  | refl (c : Config O) : InstrumentedExecution I P c .empty c
  | step {c l d answer tail f} (h : AnsweredStep P c l d answer)
      (rest : InstrumentedExecution I P d tail f) :
      InstrumentedExecution I P c ((I.trace c l d answer).append tail) f

/-- View an instrumented execution through its four individual traces. -/
abbrev InstrumentedSteps {O : Operands} (I : Instrumentation O) (P : Program O)
    (c : Config O) (obs : List (Observation O.Event O.Val)) (leaks : List (Leakage O.Val O.Address O.Effect))
    (rs : List (Release O.Ty O.Val)) (choices : List (Answer O.Val O.Event O.Store)) (d : Config O) : Prop :=
  InstrumentedExecution I P c ⟨obs, leaks, rs, choices⟩ d

/-- The four trace projections of a zero-step execution are empty. -/
theorem InstrumentedSteps.refl {O : Operands} {I : Instrumentation O} {P : Program O} (c : Config O) :
    InstrumentedSteps I P c [] [] [] [] c :=
  InstrumentedExecution.refl c

/-- Add one C1 transition and its observations to an instrumented execution. -/
theorem InstrumentedSteps.step {O : Operands} {I : Instrumentation O} {P : Program O}
    {c l c' answer obs leaks rs choices c''} (h : AnsweredStep P c l c' answer)
    (rest : InstrumentedSteps I P c' obs leaks rs choices c'') :
    InstrumentedSteps I P c (I.observeStep c l c' ++ obs) (I.leakage c l c' answer ++ leaks)
      (I.releases c l c' ++ rs) (I.choices c l c' answer ++ choices) c'' :=
  InstrumentedExecution.step h rest

/-- One complete finite instrumented execution. Its proof is a C1 execution, not a separate
semantics, and `ending` is its C1 final outcome. -/
structure Run {O : Operands} (I : Instrumentation O) (P : Program O) (initial : Config O) where
  final : Config O
  ending : Final O.Val
  finalEnding : final.final? = some ending
  observations : List (Observation O.Event O.Val)
  leakage : List (Leakage O.Val O.Address O.Effect)
  releases : List (Release O.Ty O.Val)
  choices : List (Answer O.Val O.Event O.Store)
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
    (r : Run I P c) : List (Observation O.Event O.Val) :=
  r.observations ++ (I.observeFinal r.final).toList

/-- The raw trace check used to state authorization and exercise observer mutants. -/
def ReleasePolicy.PrefixesAuthorized {O : Operands} (policy : ReleasePolicy O.Ty)
    (I : Instrumentation O) (P : Program O) (c : Config O) : Prop :=
  ∀ ⦃obs leaks rs choices d⦄, InstrumentedSteps I P c obs leaks rs choices d → policy.Authorized rs

/-- Every reachable finite prefix, including one of a stuck or diverging execution,
emits only permitted releases under the qualified observer.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def ReleasePolicy.RunsAuthorized {O : Operands} (policy : ReleasePolicy O.Ty)
    (I : Instrumentation O) (P : Program O) (c : Config O) : Prop :=
  ObserverQualified I ∧ policy.PrefixesAuthorized I P c

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
def CompletedNIComparison {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ∀ (r₁ : Run I P c₁) (r₂ : Run I P c₂),
    LowEquivalent L c₁ c₂ → r₁.choices = r₂.choices → releaseTrace r₁.releases = releaseTrace r₂.releases →
      r₁.publicObservations = r₂.publicObservations ∧
        publicLengths r₁.leakage = publicLengths r₂.leakage

/-- The run comparison underlying constant time between the same entries and under the same delimited-release premises: the
complete baseline leakage traces are equal. This is separate from `DelimitedReleaseNI`, not a
consequence of authorization. Authority: docs/decisions.md#decided-2026-11-08-foundation (C7);
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def CompletedTimingComparison {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ∀ (r₁ : Run I P c₁) (r₂ : Run I P c₂),
    LowEquivalent L c₁ c₂ → r₁.choices = r₂.choices → releaseTrace r₁.releases = releaseTrace r₂.releases →
      r₁.leakage = r₂.leakage


/-- Two finite traces agree up to the shorter trace. -/
def PrefixCompatible {α : Type} (xs ys : List α) : Prop := xs <+: ys ∨ ys <+: xs

/-- Compare public observations and public lengths on reachable prefixes. -/
def NIPrefixComparison {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ∀ ⦃o₁ l₁ r₁ x₁ d₁ o₂ l₂ r₂ x₂ d₂⦄,
    InstrumentedSteps I P c₁ o₁ l₁ r₁ x₁ d₁ →
    InstrumentedSteps I P c₂ o₂ l₂ r₂ x₂ d₂ →
    LowEquivalent L c₁ c₂ → x₁ = x₂ → releaseTrace r₁ = releaseTrace r₂ →
    PrefixCompatible o₁ o₂ ∧ PrefixCompatible (publicLengths l₁) (publicLengths l₂)

/-- Compare timing leakage on reachable prefixes. -/
def TimingPrefixComparison {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ∀ ⦃o₁ l₁ r₁ x₁ d₁ o₂ l₂ r₂ x₂ d₂⦄,
    InstrumentedSteps I P c₁ o₁ l₁ r₁ x₁ d₁ →
    InstrumentedSteps I P c₂ o₂ l₂ r₂ x₂ d₂ →
    LowEquivalent L c₁ c₂ → x₁ = x₂ → releaseTrace r₁ = releaseTrace r₂ →
    PrefixCompatible l₁ l₂

/-- NI compares finite prefixes and retains full equality for two complete runs. -/
def NIComparison {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  CompletedNIComparison I P L c₁ c₂ ∧ NIPrefixComparison I P L c₁ c₂

/-- CT compares finite prefixes and retains full equality for two complete runs. -/
def TimingComparison {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  CompletedTimingComparison I P L c₁ c₂ ∧ TimingPrefixComparison I P L c₁ c₂

/-- Delimited-release noninterference uses the machine's qualified observing semantics. -/
def DelimitedReleaseNI {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ObserverQualified I ∧ NIComparison I P L c₁ c₂

/-- Constant time uses the same qualified observing semantics and compares baseline leakage. -/
def ConstantTime {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ObserverQualified I ∧ TimingComparison I P L c₁ c₂

end ConcreteSpec.Core.Leak
