import ConcreteSpec.Spec.Exec

/-!
# Secret leakage, release and byte accounting

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`. Foundation contract C7 (the foundation proposal approved by the
owner on 2026-11-08), under the information-flow and memory decisions of 2026-11-06 and Q9.10.

Leakage is what the baseline timing observer sees, not ordinary output. It is separate from public
observations. `InstrumentedSteps` instruments C1's existing `Exec.Step` relation without changing
the machine: each step contributes public observations, leakage, ordered release records and the
external choices on which it depended. C6's observation and environment contracts can supply the
observation and choice instances; the instrumentation keeps those types explicit at this boundary.

`DelimitedReleaseNI` and `ConstantTime` are different claims about executions. Both assume low-equivalent
initial states, matched external choices and the same ordered release sites and values. NI requires
the same public observations; CT requires the same leakage. A release policy authorizes sites; it
does not discharge either claim. No scalar secret typing rule is chosen here (slice 7).

The accounting half is an abstract per-lineage alloc/free/reset model. C4's successful sized
allocation and free operations instantiate `Action.alloc` and `Action.free`; a reset instantiates
`Action.reset`, whose outflow is read from the actual pre-transition account. Addresses, provenance
and failed operations are C4's concern; a failed operation has no accounting action. Requested bytes, net
change, peak logical bytes and an allocator-defined footprint have independent evidence statuses.
Stack accounting is separate. Wipes are effects, and `WipePreservation` states the obligation that
erasure and optimization preserve them. Arrays, wiping implementations, heaps, hardware timing,
binary constant time and speculation remain in their named later slices or trust premises.
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

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : leakageCatalog.length = 7 := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Planted changed-event controls for every constructor. -/
example : (show Leakage Nat Nat Nat from .branch ⟨0⟩ true) ≠ .branch ⟨0⟩ false := by simp

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : (show Leakage Nat Nat Nat from .address ⟨1⟩ 4096) ≠ .address ⟨1⟩ 8192 := by simp

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : (show Leakage Nat Nat Nat from .allocation ⟨2⟩ ⟨3⟩ 8) ≠ .allocation ⟨2⟩ ⟨3⟩ 9 := by simp

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : (show Leakage Nat Nat Nat from .loop ⟨4⟩ 3) ≠ .loop ⟨4⟩ 4 := by simp

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : (show Leakage Nat Nat Nat from .variableTime ⟨5⟩ [1]) ≠ .variableTime ⟨5⟩ [2] := by simp

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : (show Leakage Nat Nat Nat from .publicLength ⟨6⟩ 2) ≠ .publicLength ⟨6⟩ 3 := by simp

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : (show Leakage Nat Nat Nat from .external 7) ≠ .external 8 := by simp

/-- A typed release site. The site identifies the operation in the facts; `ty` is the type of the
value explicitly declared for release there. -/
structure ReleaseSite (Ty : Type) where
  site : SiteId
  ty : Ty
  deriving DecidableEq, Repr

/-- One explicit release. Releases are compared as an ordered list, including site and value.
`reason` is the source reason and `path` is the call path, retained as fact metadata. -/
structure Release (Ty Val : Type) where
  site : ReleaseSite Ty
  value : Val
  reason : String
  path : List FnId
  deriving DecidableEq, Repr

/-- The release-policy interface: an exhaustive list of permitted typed sites. An empty list
forbids every release. Authorization says nothing about observations or leakage elsewhere. -/
structure ReleasePolicy (Ty : Type) where
  allowed : List (ReleaseSite Ty)

/-- The policy permits this exact typed site; the record's value, reason and path do not affect
site authorization. -/
def ReleasePolicy.Permits {Ty Val : Type} (policy : ReleasePolicy Ty) (r : Release Ty Val) : Prop :=
  r.site ∈ policy.allowed

/-- A policy with no permitted sites forbids release. -/
def ReleasePolicy.ForbidsRelease {Ty : Type} (policy : ReleasePolicy Ty) : Prop :=
  policy.allowed = []

/-- Every release in an ordered execution trace is authorized by the policy. -/
def ReleasePolicy.Authorized {Ty Val : Type} (policy : ReleasePolicy Ty)
    (rs : List (Release Ty Val)) : Prop :=
  ∀ r ∈ rs, policy.Permits r

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Accepted: a release at the one listed site is authorized. Refused: another typed site is not. -/
example : (ReleasePolicy.mk [⟨⟨0⟩, ()⟩]).Authorized
    [⟨⟨⟨0⟩, ()⟩, 7, "answer", []⟩] := by
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ (ReleasePolicy.mk [⟨⟨0⟩, ()⟩]).Authorized
    [⟨⟨⟨1⟩, ()⟩, 7, "not declared", []⟩] := by
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : (ReleasePolicy.mk ([] : List (ReleaseSite Unit))).ForbidsRelease := rfl

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Every nonempty release trace is unauthorized under the empty policy. -/
example (r : Release Unit Nat) (rs : List (Release Unit Nat)) :
    ¬ (ReleasePolicy.mk []).Authorized (r :: rs) := by
  intro h
  have hr := h r (by simp)
  simp [ReleasePolicy.Permits] at hr

/-- Typed-site, value and ordering controls for release comparison and authorization. -/
def typedRelease (ty : String) (value : Nat) : Release String Nat :=
  ⟨⟨⟨0⟩, ty⟩, value, "example", []⟩
def typedReleasePolicy : ReleasePolicy String := ⟨[⟨⟨0⟩, "public"⟩]⟩

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : typedReleasePolicy.Permits (typedRelease "public" 7) := by
  simp [typedReleasePolicy, ReleasePolicy.Permits, typedRelease]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ typedReleasePolicy.Permits (typedRelease "secret" 7) := by
  simp [typedReleasePolicy, ReleasePolicy.Permits, typedRelease]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : typedRelease "public" 7 ≠ typedRelease "public" 8 := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : [typedRelease "public" 7, typedRelease "public" 8] ≠
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
differ. Frames and control are execution data, not silently made public by this definition. -/
def LowEquivalent {O : Operands} (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  publicLocals L c₁ = publicLocals L c₂ ∧ L.lowStore c₁.store c₂.store

/-- C1 instrumentation for C7. Each function projects one existing machine step; C6 can supply the
public observation and external-choice instances later. The final observation includes termination
when the instrumentation exposes it. Leakage is never read from the public observation trace. -/
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

/-- All public observations of a run, including its final observation when it has one. -/
def Run.publicObservations {O : Operands} {I : Instrumentation O} {P : Program O} {c : Config O}
    (r : Run I P c) : List I.Observation :=
  r.observations ++ (I.observeFinal r.final).toList

/-- Delimited-release noninterference between two entry configurations of one program. For
low-equivalent initial states, matched external choices and equal ordered release traces, every
pair of complete finite runs has equal public observations. No equality of leakage is asserted. -/
def DelimitedReleaseNI {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ∀ (r₁ : Run I P c₁) (r₂ : Run I P c₂),
    LowEquivalent L c₁ c₂ → r₁.choices = r₂.choices → r₁.releases = r₂.releases →
      r₁.publicObservations = r₂.publicObservations

/-- Constant time between the same entries and under the same delimited-release premises: the
complete baseline leakage traces are equal. This is separate from `DelimitedReleaseNI`, not a
consequence of authorization. -/
def ConstantTime {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ∀ (r₁ : Run I P c₁) (r₂ : Run I P c₂),
    LowEquivalent L c₁ c₂ → r₁.choices = r₂.choices → r₁.releases = r₂.releases →
      r₁.leakage = r₂.leakage

/-! ## Logical byte accounting -/

/-- The live logical-byte accounting of one allocator lineage. `requested` is cumulative, `live` is
the current logical bytes, `peak` is the greatest live value reached by the whole execution, and
`functionPeak` is the greatest live value reached since the most recent `Account.enter`. -/
structure Account where
  requested : Nat
  live : Nat
  peak : Nat
  functionPeak : Nat
  deriving DecidableEq, Repr

/-- No allocation and no history. -/
def Account.empty : Account := ⟨0, 0, 0, 0⟩

/-- A well-formed account has already reached at least its current live bytes, both historically
and since function entry. -/
def Account.WF (a : Account) : Prop := a.live ≤ a.peak ∧ a.live ≤ a.functionPeak

/-- Mark a function entry. This starts its local peak at the bytes already live on entry without
changing ownership, cumulative requests or the whole-execution historical peak. -/
def Account.enter (a : Account) : Account :=
  { a with functionPeak := a.live }

/-- Account for a successful request of `bytes`: cumulative requested and live bytes increase, and
both peaks move only if the new live value exceeds them. -/
def Account.alloc (a : Account) (bytes : Nat) : Account where
  requested := a.requested + bytes
  live := a.live + bytes
  peak := max a.peak (a.live + bytes)
  functionPeak := max a.functionPeak (a.live + bytes)

/-- Account for a successful free. Cumulative history and both peaks are conserved. The abstract
machine admits the action only when `bytes ≤ a.live`. -/
def Account.free (a : Account) (bytes : Nat) : Account :=
  { a with live := a.live - bytes }

/-- Account for a reset: all live bytes are released, while cumulative history and both peaks
remain. -/
def Account.reset (a : Account) : Account :=
  { a with live := 0 }

/-- An abstract C4 accounting action. A reset names only its lineage: the amount released is a fact
of the transition, not a caller-supplied field that the transition could be asked to confirm. -/
inductive Action where
  | alloc (lineage : LineageId) (bytes : Nat)
  | free (lineage : LineageId) (bytes : Nat)
  | reset (lineage : LineageId)
  deriving DecidableEq, Repr

/-- Abstract memory is only the accounting projection of C4 state. Every lineage starts empty. -/
abbrev Memory := LineageId → Account

/-- Replace one lineage's account, leaving every other lineage unchanged. -/
def Memory.set (m : Memory) (lineage : LineageId) (a : Account) : Memory :=
  fun l => if l = lineage then a else m l

/-- The empty accounting projection. -/
def Memory.empty : Memory := fun _ => Account.empty

/-- Apply an action. Freeing more than the lineage's live bytes has no transition. A reset releases
exactly the lineage's current live bytes by setting that account's live field to zero. -/
def apply (m : Memory) : Action → Option Memory
  | .alloc lineage bytes => some (Memory.set m lineage ((m lineage).alloc bytes))
  | .free lineage bytes =>
      if bytes ≤ (m lineage).live then some (Memory.set m lineage ((m lineage).free bytes)) else none
  | .reset lineage => some (Memory.set m lineage (m lineage).reset)

/-- A successful sequence of abstract C4 accounting actions. -/
def run (m : Memory) : List Action → Option Memory
  | [] => some m
  | a :: actions => (apply m a).bind fun m' => run m' actions

/-- Bytes allocated for `lineage` by a sequence. -/
def allocated (lineage : LineageId) : List Action → Nat
  | [] => 0
  | .alloc l bytes :: rest => (if l = lineage then bytes else 0) + allocated lineage rest
  | _ :: rest => allocated lineage rest

/-- Bytes explicitly freed for `lineage` by a sequence. -/
def freed (lineage : LineageId) : List Action → Nat
  | [] => 0
  | .free l bytes :: rest => (if l = lineage then bytes else 0) + freed lineage rest
  | _ :: rest => freed lineage rest

/-- The reset outflow of one action, read from the account immediately before that action. -/
def resetOutflow (lineage : LineageId) (m : Memory) : Action → Nat
  | .reset l => if l = lineage then (m l).live else 0
  | _ => 0

/-- Bytes released by resets of `lineage` while executing a sequence from `m`. Each outflow comes
from the actual intermediate state. An unsuccessful suffix contributes nothing. -/
def resetBytes (lineage : LineageId) (m : Memory) : List Action → Nat
  | [] => 0
  | action :: rest =>
      match apply m action with
      | none => 0
      | some m' => resetOutflow lineage m action + resetBytes lineage m' rest

/-- Independent conservation obligation for a reset. The recorded outflow is not an input to the
account transition, so a wrong record makes this predicate false rather than making the reset
inadmissible. Cumulative history and both peaks must be conserved. -/
def ResetConservation (before after : Account) (recordedOutflow : Nat) : Prop :=
  after.requested = before.requested ∧
    after.live = 0 ∧ after.live + recordedOutflow = before.live ∧
    after.peak = before.peak ∧ after.functionPeak = before.functionPeak

/-- Independent maximum of live bytes at entry and every successful intermediate state.
This observer never reads either recorded peak field. A failed suffix has no successor state. -/
def observedPeak (lineage : LineageId) (m : Memory) : List Action → Nat
  | [] => (m lineage).live
  | action :: rest =>
      match apply m action with
      | none => (m lineage).live
      | some m' => max (m lineage).live (observedPeak lineage m' rest)

/-- The named allocator boundary defines footprint from the lineage, entry memory and actions.
No logical-byte theorem invents a footprint for an allocator that does not provide one. -/
structure AllocatorContract where
  footprint : LineageId → Memory → List Action → Option Nat

/-- Evidence for a reported bound. A bound is either established, with its stated limit, or not
established. The latter never turns an unknown into a reassuring numeric claim. -/
inductive BoundStatus where
  | boundEstablished
  | notEstablished
  deriving DecidableEq, Repr

/-- A reported bound and its independent evidence status. -/
structure Bound (α : Type) where
  limit : α
  status : BoundStatus
  deriving DecidableEq, Repr

/-- An established bound is sound for `actual`; `notEstablished` asserts no bound. -/
def Bound.Sound {α : Type} [LE α] (actual : α) (b : Bound α) : Prop :=
  b.status = .boundEstablished → actual ≤ b.limit

/-- An established footprint bound needs an allocator-defined footprint; no contract-defined value
means that footprint evidence cannot be established. -/
def Bound.SoundOptional {α : Type} [LE α] (actual : Option α) (b : Bound α) : Prop :=
  b.status = .boundEstablished → ∃ a, actual = some a ∧ a ≤ b.limit

/-- Per-lineage facts for one function: bytes requested by that function, its net live-byte change,
its peak logical bytes since entry, and footprint when the allocator contract defines one. Each has
its own status. -/
structure LineageReport where
  lineage : LineageId
  requested : Bound Nat
  netChange : Bound Int
  peak : Bound Nat
  footprint : Bound Nat

/-- Soundness of a lineage report against explicitly identified entry and exit accounts.
The identity must agree with the report; numeric bounds cannot be relabelled. Requested bytes exclude
requests made before entry. The peak is the maximum live bytes reached since `Account.enter`, not
the historical maximum of the whole program. The caller applies `Account.enter` at function entry
and does not reset the local peak again before the function exits. -/
def LineageReport.Sound (lineage : LineageId) (entry exit : Account) (footprint : Option Nat)
    (r : LineageReport) : Prop :=
  r.lineage = lineage ∧
    Bound.Sound (exit.requested - entry.requested) r.requested ∧
    Bound.Sound ((exit.live : Int) - (entry.live : Int)) r.netChange ∧
    Bound.Sound exit.functionPeak r.peak ∧ Bound.SoundOptional footprint r.footprint

/-- Whole-run WF obligation, including every possible successful result. -/
def PreservesWF (execute : Memory → List Action → Option Memory) (m : Memory)
    (actions : List Action) : Prop :=
  (∀ l, (m l).WF) → ∀ m', execute m actions = some m' → ∀ l, (m' l).WF

/-- Whole-run exact conservation, independently for each lineage. -/
def Conserves (execute : Memory → List Action → Option Memory) (m : Memory)
    (actions : List Action) (lineage : LineageId) : Prop :=
  ∀ m', execute m actions = some m' →
    (m' lineage).requested = (m lineage).requested + allocated lineage actions ∧
    ((m' lineage).live : Int) = (m lineage).live + allocated lineage actions -
      freed lineage actions - resetBytes lineage m actions

/-- Whole-run retention of both recorded maxima. -/
def RetainsPeaks (execute : Memory → List Action → Option Memory) (m : Memory)
    (actions : List Action) (lineage : LineageId) : Prop :=
  ∀ m', execute m actions = some m' →
    (m lineage).peak ≤ (m' lineage).peak ∧
    (m lineage).functionPeak ≤ (m' lineage).functionPeak

/-- Whole-run agreement with the independently observed live-byte trajectory. -/
def TracksPeak (execute : Memory → List Action → Option Memory) (m : Memory)
    (actions : List Action) (lineage : LineageId) : Prop :=
  (∀ l, (m l).WF) → ∀ m', execute m actions = some m' →
    (m' lineage).functionPeak = max (m lineage).functionPeak (observedPeak lineage m actions)

/-- Report contract from independent analysis and a named allocator footprint. The executor's exit
is quantified inside the obligation, rather than fixed by a test's arguments. -/
def CertifiesReport (execute : Memory → List Action → Option Memory) (m : Memory)
    (actions : List Action) (allocator : AllocatorContract) (lineage : LineageId)
    (requestedLimit : Nat) (netLimit : Int) (peakLimit footprintLimit : Nat) : Prop :=
  (∀ l, (m l).WF) → (m lineage).functionPeak = (m lineage).live →
  allocated lineage actions ≤ requestedLimit →
  (allocated lineage actions : Int) - freed lineage actions -
    resetBytes lineage m actions ≤ netLimit →
  observedPeak lineage m actions ≤ peakLimit →
  (∃ bytes, allocator.footprint lineage m actions = some bytes ∧ bytes ≤ footprintLimit) →
  ∀ m', execute m actions = some m' →
    LineageReport.Sound lineage (m lineage) (m' lineage) (allocator.footprint lineage m actions)
      ⟨lineage, ⟨requestedLimit, .boundEstablished⟩, ⟨netLimit, .boundEstablished⟩,
        ⟨peakLimit, .boundEstablished⟩, ⟨footprintLimit, .boundEstablished⟩⟩

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Accepted: a valid allocate/free/reset sequence. Refused: freeing a byte not owned by the
lineage has no accounting transition. -/
example : (run Memory.empty
    [.alloc ⟨0⟩ 8, .free ⟨0⟩ 3, .reset ⟨0⟩]).map (· ⟨0⟩) = some ⟨8, 0, 8, 8⟩ := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : apply Memory.empty (.free ⟨0⟩ 1) = none := by decide

/-! ## Wipes as preserved effects -/

/-- A logical wipe effect. Actual clearing of bytes is a backend premise until the array/wipe and
heap slices prove it; the event itself is already part of the semantic effect trace. -/
structure WipeEvent where
  site : SiteId
  lineage : LineageId
  bytes : Nat
  deriving DecidableEq, Repr

/-- The erasure/optimization obligation for wipes: whenever `related` certifies a source/target pair,
their ordered wipe effects are identical. A law that keeps only ordinary observations does not
satisfy this obligation. -/
def WipePreservation {Source Target : Type} (related : Source → Target → Prop)
    (sourceWipes : Source → List WipeEvent) (targetWipes : Target → List WipeEvent) : Prop :=
  ∀ source target, related source target → sourceWipes source = targetWipes target

end ConcreteSpec.Core.Leak
