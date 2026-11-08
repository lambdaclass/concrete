import ConcreteSpec.Proofs.Observe2

/-! # Valid observations of evaluator runs

Unlike the individual report branch equations, this property is checked on actual runs. The
trace obligation is independent of the report's representation: blocked histories must contain
the pending request, and terminal histories must discharge every request.
-/

namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core

/-- The independently stated trace, environment, and report-correspondence obligations of a
reported observation. `none` is permitted only for evaluator/premise reports (`stuck`,
`badAnswer`, `outOfFuel`) or unmapped `final` states (`A.terminal? final = none`), never for a
valid terminal run or a well-formed blocked request; a mutant that always returns `none`
violates `ReportTraceValid`. -/
def ReportTraceValid {O : Exec.Operands} {S : EventSignature} (P : Exec.Program O) (A : ExecAdapter O S)
    (C : OperationContract S) (input : C.Input) (r : Exec.Report O) :
    Option (FullObservation S) → Prop
  | none =>
    match r.ending with
    | .final final _ => A.terminal? final = none
    | .noAnswer _ => False
    | .stuck _ | .badAnswer _ | .outOfFuel _ => True
  | some (.terminal obs) =>
    CompleteTrace C input obs.events ∧
    ∃ final store, r.ending = .final final store ∧
      A.terminal? final = some obs.terminal ∧
      obs.events = A.project r.events ∧
      obs.state = A.state store
  | some (.blocked events q) =>
    ValidTrace C input (some q) events ∧ C.mayBlock input events q ∧
    events = A.project r.events ++ [.request q] ∧
    ∃ c p vs site store k, r.ending = .noAnswer c ∧
      Exec.step P c = some (.request p vs site store k) ∧
      A.request p vs site store = some q
  | some (.finiteDivergence _) | some (.infiniteDivergence _) => False

/-- Law (subject `ConcreteSpec.Core.Observe.reportObservation`).
Reporting an actual well-formed evaluator run preserves sequential trace validity, ties terminal
and blocked observations to the report's ending and projected events, and never returns `none` on
a well-formed `noAnswer` or mapped `final` ending. -/
theorem runFrom_report_trace {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (ρ : Exec.Oracle O) (wellFormed : ExecAdapter.WellFormed A C input ρ)
    (fuel : Nat) (hist : List O.Event) (c : Exec.Config O)
    (valid : CompleteTrace C input (A.project hist)) :
    ReportTraceValid P A C input (Exec.runFrom P ρ fuel hist c)
      (reportObservation P A C input (Exec.runFrom P ρ fuel hist c)
        (runFrom_blockingPermitted P A C input ρ wellFormed valid)) := by
  obtain ⟨history, hnoAnswer⟩ := runFrom_validTrace P A C input ρ wellFormed fuel hist c valid
  generalize ending : (Exec.runFrom P ρ fuel hist c).ending = e
  cases e with
  | final f store =>
    simp only [reportObservation, ending]
    cases hterm : A.terminal? f with
    | none => simp [ReportTraceValid, ending, hterm]
    | some term =>
      simp only [Option.map_some, ReportTraceValid, ending]
      exact ⟨history, f, store, rfl, hterm, trivial, rfl⟩
  | noAnswer pending =>
    obtain ⟨p, vs, site, store, k, q, step, typed, _⟩ := hnoAnswer ending
    simp only [reportObservation, ending, step, typed, Option.map_some, ReportTraceValid]
    have hvalid := runFrom_noAnswer_pending_valid P A C input ρ wellFormed valid ending step typed
    exact ⟨hvalid.1, hvalid.2, trivial, pending, p, vs, site, store, k, rfl, step, typed⟩
  | stuck _ => simp [reportObservation, ending, ReportTraceValid]
  | badAnswer _ => simp [reportObservation, ending, ReportTraceValid]
  | outOfFuel _ => simp [reportObservation, ending, ReportTraceValid]

/-- Law (subject `ConcreteSpec.Core.Observe.reportObservation`).
An actual run ending at a typed unanswered request produces exactly the pending observation,
including every prior projected event. The evaluator induction also establishes the pending
trace and its independently specified blocking policy. Returning no observation, truncating
prior history, or reordering that history violates the result. -/
theorem runFrom_blocked_report {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (ρ : Exec.Oracle O) (wellFormed : ExecAdapter.WellFormed A C input ρ)
    (fuel : Nat) (hist : List O.Event) (c : Exec.Config O)
    (valid : CompleteTrace C input (A.project hist))
    (pendingConfig : Exec.Config O) (p : O.Prim) (vs : List O.Val) (site : SiteId)
    (store : O.Store) (k : Exec.Answer O.Val O.Event O.Store → Option (Exec.Config O))
    (q : Request S)
    (ending : (Exec.runFrom P ρ fuel hist c).ending = .noAnswer pendingConfig)
    (pending : Exec.step P pendingConfig = some (.request p vs site store k))
    (typed : A.request p vs site store = some q) :
    reportObservation P A C input (Exec.runFrom P ρ fuel hist c)
      (runFrom_blockingPermitted P A C input ρ wellFormed valid) =
        some (.blocked (A.project (Exec.runFrom P ρ fuel hist c).events ++ [.request q]) q) ∧
    ValidTrace C input (some q)
      (A.project (Exec.runFrom P ρ fuel hist c).events ++ [.request q]) ∧
    C.mayBlock input (A.project (Exec.runFrom P ρ fuel hist c).events ++ [.request q]) q := by
  constructor
  · simp [reportObservation, ending, pending, typed]
  · exact runFrom_noAnswer_pending_valid P A C input ρ wellFormed valid ending pending typed

/-- A report adapter fault drops only the prior history, retaining the correctly typed pending
request. Its result can remain a valid permitted pending trace, while violating preservation. -/
def onlyPendingHistory {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
    Option (FullObservation S) :=
  match reportObservation P A C input r permitted with
  | some (.blocked _ q) => some (.blocked [.request q] q)
  | other => other

/-- A report adapter fault: a blocked result drops its history, including the pending request.
The environment, evaluator and all admission premises are unchanged. -/
def dropBlockedHistory {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
    Option (FullObservation S) :=
  match reportObservation P A C input r permitted with
  | some (.blocked _ q) => some (.blocked [] q)
  | other => other

/-- A report adapter fault that always returns `none`, suppressing valid terminal and blocked
observations. -/
def alwaysNoneReport {O : Exec.Operands} {S : EventSignature}
    (_P : Exec.Program O) (_A : ExecAdapter O S) (C : OperationContract S) (_input : C.Input)
    (_r : Exec.Report O) (_permitted : ExecAdapter.BlockingPermitted _P _A C _input _r) :
    Option (FullObservation S) :=
  none

/-- Helper (computation). By induction on evaluator fuel, an `.outOfFuel c'` ending occurs only at a
non-final configuration `c'.final? = none`, feeding `runFrom_outOfFuel_report_none`.
(approved: PENDING) -/
theorem runFrom_outOfFuel_not_final {O : Exec.Operands}
    (P : Exec.Program O) (ρ : Exec.Oracle O) (fuel : Nat) (hist : List O.Event)
    (c c' : Exec.Config O)
    (ending : (Exec.runFrom P ρ fuel hist c).ending = .outOfFuel c') :
    c'.final? = none := by
  induction fuel generalizing hist c with
  | zero =>
    unfold Exec.runFrom at ending
    cases hfin : c.final? with
    | some f => simp [hfin] at ending
    | none =>
      simp only [hfin, Exec.Ending.outOfFuel.injEq] at ending
      subst ending
      exact hfin
  | succ n ih =>
    unfold Exec.runFrom at ending
    cases hfin : c.final? with
    | some f => simp [hfin] at ending
    | none =>
      simp only [hfin] at ending
      cases hstep : Exec.step P c with
      | none => simp [hstep] at ending
      | some out =>
        cases out with
        | next nextConfig =>
          simp only [hstep] at ending
          exact ih hist nextConfig ending
        | request p vs site st k =>
          simp only [hstep] at ending
          cases hans : ρ hist p vs site st with
          | none => simp [hans] at ending
          | some a =>
            simp only [hans] at ending
            by_cases hallows : O.allows p vs st a = true
            · simp only [hallows, ↓reduceIte] at ending
              cases hk : k a with
              | none => simp [hk] at ending
              | some nextConfig =>
                simp only [hk] at ending
                exact ih (hist ++ a.event.toList) nextConfig ending
            · simp [hallows] at ending

/-- Law (subject `ConcreteSpec.Core.Observe.reportObservation`).
When an evaluator run exhausts its step budget (`.outOfFuel c'`), the configuration `c'` is not
final (`c'.final? = none`), the events emitted before fuel ran out form a complete valid trace, and
`reportObservation` returns `none` rather than fabricating divergence or termination. -/
theorem runFrom_outOfFuel_report_none {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (ρ : Exec.Oracle O) (wellFormed : ExecAdapter.WellFormed A C input ρ)
    (fuel : Nat) (hist : List O.Event) (c c' : Exec.Config O)
    (valid : CompleteTrace C input (A.project hist))
    (ending : (Exec.runFrom P ρ fuel hist c).ending = .outOfFuel c') :
    c'.final? = none ∧
    CompleteTrace C input (A.project (Exec.runFrom P ρ fuel hist c).events) ∧
    reportObservation P A C input (Exec.runFrom P ρ fuel hist c)
      (runFrom_blockingPermitted P A C input ρ wellFormed valid) = none := by
  refine ⟨runFrom_outOfFuel_not_final P ρ fuel hist c c' ending,
    (runFrom_validTrace P A C input ρ wellFormed fuel hist c valid).1, ?_⟩
  simp [reportObservation, ending]

/-- Fault: fabricates `.finiteDivergence` when `runFrom` runs out of fuel, confusing budget
exhaustion with program divergence. -/
def divergeOnOutOfFuelReport {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
    Option (FullObservation S) :=
  match r.ending with
  | .outOfFuel _ => some (.finiteDivergence (A.project r.events))
  | _ => reportObservation P A C input r permitted

/-- Fault: fabricates a normal `.exited 0` terminal observation when `runFrom` runs out of fuel. -/
def exitZeroOnOutOfFuelReport {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
    Option (FullObservation S) :=
  match r.ending with
  | .outOfFuel c' => some (.terminal ⟨A.project r.events, .exited 0, A.state c'.store⟩)
  | _ => reportObservation P A C input r permitted

/-- Law (subject `ConcreteSpec.Core.Observe.launchObservation`, `ConcreteSpec.Core.Observe.Terminal.status`).
When a required capability grant `g ∈ required` is missing from the launch configuration
(`g ∉ configured`), `launchObservation` refuses startup before running `main`: it produces
`some (.terminal ⟨[], .startupRefused, A.state c.store⟩)`, whose exit status is `125` with no
trap line (`trapLine? = none`). -/
theorem launch_missing_grant_status_125 {α : Type} [DecidableEq α]
    {O : Exec.Operands} {S : EventSignature}
    (required configured : List α) (g : α)
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (ρ : Exec.Oracle O) (fuel : Nat) (c : Exec.Config O)
    (permitted : ExecAdapter.BlockingPermitted P A C input (Exec.run P ρ fuel c))
    (hreq : g ∈ required) (hmiss : g ∉ configured) :
    launchObservation required configured P A C input ρ fuel c permitted =
      some (.terminal ⟨[], .startupRefused, A.state c.store⟩) ∧
    Terminal.status .startupRefused = 125 ∧
    Terminal.trapLine? .startupRefused = none := by
  have hgrants : grantsConfigured required configured = false := by
    unfold grantsConfigured
    rw [List.all_eq_false]
    exact ⟨g, hreq, by simp [hmiss]⟩
  simp [launchObservation, hgrants, Terminal.status, Terminal.trapLine?]

/-- Fault: ignores missing launch grants and runs `main` regardless of `grantsConfigured`. -/
def ignoreGrantsLaunchObservation {α : Type} [DecidableEq α]
    {O : Exec.Operands} {S : EventSignature}
    (_required _configured : List α)
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (ρ : Exec.Oracle O) (fuel : Nat) (c : Exec.Config O)
    (permitted : ExecAdapter.BlockingPermitted P A C input (Exec.run P ρ fuel c)) :
    Option (FullObservation S) :=
  reportObservation P A C input (Exec.run P ρ fuel c) permitted

/-- Fault: reports exit status `1` instead of `125` on `.startupRefused`. -/
def wrongStartupStatus : Terminal → Nat
  | .exited code => code
  | .trapped _ _ _ _ => 134
  | .startupRefused => 1

/-- Law (subject `ConcreteSpec.Core.Observe.reportObservation`, `ConcreteSpec.Core.Observe.Terminal.status`).
When an evaluator run ends in `.final (.trapped kind site) store` and the well-formed adapter
resolves `site` to `sp`, `reportObservation` produces the trapped terminal observation retaining
all prior projected events and the final external state `A.state store`, with valid relative
path `sp.PathOk`, exit status `134`, diagnostic line `trapLine kind site sp`, and terminal
classification distinct from normal `.exited 134`. -/
theorem runFrom_trapped_status_134 {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (ρ : Exec.Oracle O) (wellFormed : ExecAdapter.WellFormed A C input ρ)
    (fuel : Nat) (hist : List O.Event) (c : Exec.Config O)
    (valid : CompleteTrace C input (A.project hist))
    (kind : TrapKind) (site : SiteId) (store : O.Store) (sp : Span)
    (ending : (Exec.runFrom P ρ fuel hist c).ending = .final (.trapped kind site) store)
    (resolved : AList.lookup A.sites site = some sp) :
    reportObservation P A C input (Exec.runFrom P ρ fuel hist c)
      (runFrom_blockingPermitted P A C input ρ wellFormed valid) =
        some (.terminal ⟨A.project (Exec.runFrom P ρ fuel hist c).events,
          .trapped kind site sp (trapLine kind site sp), A.state store⟩) ∧
    sp.PathOk ∧
    Terminal.status (.trapped kind site sp (trapLine kind site sp)) = 134 ∧
    Terminal.trapLine? (.trapped kind site sp (trapLine kind site sp)) =
      some (trapLine kind site sp) ∧
    (.trapped kind site sp (trapLine kind site sp) : Terminal) ≠ .exited 134 := by
  have hpath : sp.PathOk := wellFormed.sites_valid site sp resolved
  refine ⟨?_, hpath, rfl, rfl, by intro h; cases h⟩
  simp [reportObservation, ending, ExecAdapter.terminal?, trapTerminal, resolved, hpath]

/-- Fault: collapses trapped runs to `.exited 134`, losing the trap classification and diagnostic
line while keeping the numeric status. -/
def exit134OnTrapReport {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
    Option (FullObservation S) :=
  match reportObservation P A C input r permitted with
  | some (.terminal ⟨events, .trapped _ _ _ _, state⟩) =>
    some (.terminal ⟨events, .exited 134, state⟩)
  | other => other

/-- Fault: reports exit status `1` instead of `134` on `.trapped`. -/
def wrongTrapStatus : Terminal → Nat
  | .exited code => code
  | .trapped _ _ _ _ => 1
  | .startupRefused => 125

/-- Law (subject `ConcreteSpec.Core.Observe.trapTerminal`).
A site-table trap lookup produces a trapped terminal observation if and only if the site resolves
to `sp`, `sp.PathOk` holds (nonempty, project-relative, forward slashes only), and the diagnostic
line is `trapLine kind site sp`. -/
theorem trapTerminal_eq_some_iff (sites : SiteTable) (kind : TrapKind) (site : SiteId)
    (sp : Span) (line : String) :
    trapTerminal sites kind site = some (.trapped kind site sp line) ↔
      AList.lookup sites site = some sp ∧ sp.PathOk ∧ line = trapLine kind site sp := by
  unfold trapTerminal
  cases resolved : AList.lookup sites site with
  | none =>
    simp
  | some actual =>
    constructor
    · intro h
      by_cases hpath : actual.PathOk
      · simp only [hpath, decide_true, ↓reduceIte, Option.some.injEq,
          Terminal.trapped.injEq] at h
        obtain ⟨_, _, rfl, rfl⟩ := h
        exact ⟨rfl, hpath, rfl⟩
      · simp [hpath] at h
    · rintro ⟨hsp, hpath, rfl⟩
      cases hsp
      simp [hpath]

/-- Fault: resolves trap sites without checking `Span.PathOk`, allowing absolute or backslash
paths into terminal observations. -/
def uncheckedTrapTerminal (sites : SiteTable) (kind : TrapKind) (site : SiteId) : Option Terminal :=
  (AList.lookup sites site).map fun sp => .trapped kind site sp (trapLine kind site sp)

/-- Fault: mistakes a request waiting at the fuel boundary for an unanswered oracle call. -/
def fabricatedBlockingReport {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
    Option (FullObservation S) :=
  match r.ending with
  | .outOfFuel c =>
    match Exec.step P c with
    | some (.request p vs site store _) =>
      (A.request p vs site store).map fun q => .blocked (A.project r.events ++ [.request q]) q
    | _ => none
  | _ => reportObservation P A C input r permitted

namespace ReportDemo
open AdapterDemo

/-- Helper (computation). A closed witness for the nonempty completed history used in the
operational instance, not an independent contract law. (approved: PENDING) -/
theorem prior_complete : CompleteTrace permittingContract () (adapter.project [1]) :=
  .response (.request .idle) (RequestResponse.matched (S := signature) Op.double 1 2) rfl

/-- Instance (law `ConcreteSpec.Core.Observe.runFrom_report_trace`, case `prior_exchange_block`). -/
example : ReportTraceValid program adapter permittingContract ()
    (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
    (reportObservation program adapter permittingContract ()
      (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
      (runFrom_blockingPermitted program adapter permittingContract () blockingOracle
        blockingWellFormed prior_complete)) :=
  runFrom_report_trace program adapter permittingContract () blockingOracle blockingWellFormed
    1 [1] (requesting 3) prior_complete

/-- Planted (law `ConcreteSpec.Core.Observe.runFrom_report_trace`, case `prior_exchange_block`,
subject `ConcreteSpec.Core.Observe.reportObservation`, mutant `ConcreteSpec.Core.Observe.dropBlockedHistory`). -/
example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
    CompleteTrace permittingContract () (adapter.project [1]) ∧
    ¬ ReportTraceValid program adapter permittingContract ()
      (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
      (dropBlockedHistory program adapter permittingContract ()
        (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
        (runFrom_blockingPermitted program adapter permittingContract () blockingOracle
          blockingWellFormed prior_complete)) := by
  refine ⟨blockingWellFormed, prior_complete, ?_⟩
  change ¬ (ValidTrace permittingContract () (some (query 3)) [] ∧ _)
  intro invalid
  have pending := invalid.1
  generalize empty : ([] : List (Event signature)) = events at pending
  cases pending with
  | request previous => simp at empty

/-- Instance (law `ConcreteSpec.Core.Observe.runFrom_report_trace`, case `refuse_always_none`). -/
example : ReportTraceValid program adapter permittingContract ()
    (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
    (reportObservation program adapter permittingContract ()
      (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
      (runFrom_blockingPermitted program adapter permittingContract () blockingOracle
        blockingWellFormed prior_complete)) :=
  runFrom_report_trace program adapter permittingContract () blockingOracle blockingWellFormed
    1 [1] (requesting 3) prior_complete

/-- Planted (law `ConcreteSpec.Core.Observe.runFrom_report_trace`, case `refuse_always_none`,
subject `ConcreteSpec.Core.Observe.reportObservation`, mutant `ConcreteSpec.Core.Observe.alwaysNoneReport`). -/
example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
    CompleteTrace permittingContract () (adapter.project [1]) ∧
    ¬ ReportTraceValid program adapter permittingContract ()
      (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
      (alwaysNoneReport program adapter permittingContract ()
        (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
        (runFrom_blockingPermitted program adapter permittingContract () blockingOracle
          blockingWellFormed prior_complete)) := by
  refine ⟨blockingWellFormed, prior_complete, ?_⟩
  intro invalid
  exact invalid

/-- The independently reached request's continuation, exposed so every instance argument is
closed, including the continuation in the step premise. -/
def continuation (a : Exec.Answer Int Int Unit) : Option (Exec.Config ops) :=
  match a.result with
  | .ok _ => some ⟨.done, [], a.store, []⟩
  | .error kind => some ⟨.trapped kind ⟨0⟩, [], a.store, []⟩

/-- Helper (computation). Exposes the actual evaluator continuation for the closed report
instance; no observation property is assumed. (approved: PENDING) -/
theorem pending_step : Exec.step program (requesting 3) =
    some (.request 3 [] ⟨0⟩ () continuation) := by
  change some (Exec.Out.request (O := ops) 3 [] ⟨0⟩ () _) = _
  congr 2
  funext a
  cases a with
  | mk event result store =>
    cases result <;> rfl

/-- Instance (law `ConcreteSpec.Core.Observe.runFrom_blocked_report`, case `preserve_prior_exchange`). -/
example : reportObservation program adapter permittingContract ()
      (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
      (runFrom_blockingPermitted program adapter permittingContract () blockingOracle
        blockingWellFormed prior_complete) =
      some (.blocked (adapter.project
        (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events ++ [request 3]) (query 3)) ∧
    ValidTrace permittingContract () (some (query 3))
      (adapter.project (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events ++ [request 3]) ∧
    permittingContract.mayBlock ()
      (adapter.project (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events ++ [request 3])
      (query 3) :=
  runFrom_blocked_report program adapter permittingContract () blockingOracle blockingWellFormed
    1 [1] (requesting 3) prior_complete (requesting 3) 3 [] ⟨0⟩ () continuation (query 3) rfl pending_step rfl

/-- Planted (law `ConcreteSpec.Core.Observe.runFrom_blocked_report`, case `preserve_prior_exchange`,
subject `ConcreteSpec.Core.Observe.reportObservation`, mutant `ConcreteSpec.Core.Observe.onlyPendingHistory`). -/
example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
    CompleteTrace permittingContract () (adapter.project [1]) ∧
    (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).ending = .noAnswer (requesting 3) ∧
    Exec.step program (requesting 3) = some (.request 3 [] ⟨0⟩ () continuation) ∧
    adapter.request 3 [] ⟨0⟩ () = some (query 3) ∧
    ¬ (onlyPendingHistory program adapter permittingContract ()
      (Exec.runFrom program blockingOracle 1 [1] (requesting 3))
      (runFrom_blockingPermitted program adapter permittingContract () blockingOracle
        blockingWellFormed prior_complete) =
        some (.blocked (adapter.project
          (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events ++ [request 3]) (query 3)) ∧
      ValidTrace permittingContract () (some (query 3))
        (adapter.project (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events ++ [request 3]) ∧
      permittingContract.mayBlock ()
        (adapter.project (Exec.runFrom program blockingOracle 1 [1] (requesting 3)).events ++ [request 3])
        (query 3)) := by
  refine ⟨blockingWellFormed, prior_complete, rfl, pending_step, rfl, ?_⟩
  intro invalid
  have equal := invalid.1
  change (some (.blocked [request 3] (query 3)) : Option (FullObservation signature)) =
    some (.blocked [request 1, response 2, request 3] (query 3)) at equal
  have lengths := congrArg (fun r : Option (FullObservation signature) =>
    match r with | some (.blocked events _) => events.length | _ => 0) equal
  contradiction

/-- A concrete adapter resolving the decided project-relative path. -/
def relativePathAdapter : ExecAdapter ops signature :=
  { adapter with sites := Demo.sites }

/-- Helper (computation). A concrete well-formedness witness, including a nonempty valid site
table. This is not independent evidence about elaboration. (approved: PENDING) -/
theorem relativePathWellFormed :
    ExecAdapter.WellFormed relativePathAdapter permittingContract () blockingOracle := by
  refine ⟨blockingWellFormed.internal, blockingWellFormed.answered,
    blockingWellFormed.blocked, ?_⟩
  intro site sp resolved
  simp only [relativePathAdapter, Demo.sites, AList.lookup] at resolved
  split at resolved
  · cases resolved
    decide
  · cases resolved

/-- Accepted: a reachable hosted trap projection uses the valid path. -/
example : (relativePathAdapter.terminal? (.trapped .overflow Demo.site)) =
    some (.trapped .overflow Demo.site Demo.sp (trapLine .overflow Demo.site Demo.sp)) := rfl

/-- Refused: the missing-answer oracle violates a nonblocking environment contract, even though
its request remains a valid pending request. -/
example : ¬ ExecAdapter.WellFormed adapter forbiddingContract () blockingOracle := by
  intro formed
  obtain ⟨_, _, hblock⟩ :=
    formed.blocked (hist := [1]) (p := 3) (vs := []) (site := ⟨0⟩) (store := ()) rfl
  exact hblock

/-- Instance (law `ConcreteSpec.Core.Observe.runFrom_report_trace`, case `fuel_is_not_blocking`). -/
example : ReportTraceValid program adapter permittingContract ()
    (Exec.runFrom program blockingOracle 0 [] (requesting 3))
    (reportObservation program adapter permittingContract ()
      (Exec.runFrom program blockingOracle 0 [] (requesting 3))
      (runFrom_blockingPermitted program adapter permittingContract () blockingOracle
        blockingWellFormed (show CompleteTrace permittingContract () (adapter.project []) from .idle))) :=
  runFrom_report_trace program adapter permittingContract () blockingOracle blockingWellFormed
    0 [] (requesting 3) ValidTrace.idle

/-- Planted (law `ConcreteSpec.Core.Observe.runFrom_report_trace`, case `fuel_is_not_blocking`,
subject `ConcreteSpec.Core.Observe.reportObservation`, mutant `ConcreteSpec.Core.Observe.fabricatedBlockingReport`). -/
example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
    CompleteTrace permittingContract () (adapter.project []) ∧
    ¬ ReportTraceValid program adapter permittingContract ()
      (Exec.runFrom program blockingOracle 0 [] (requesting 3))
      (fabricatedBlockingReport program adapter permittingContract ()
        (Exec.runFrom program blockingOracle 0 [] (requesting 3))
        (runFrom_blockingPermitted program adapter permittingContract () blockingOracle
          blockingWellFormed (show CompleteTrace permittingContract () (adapter.project []) from .idle))) := by
  refine ⟨blockingWellFormed, .idle, ?_⟩
  intro invalid
  obtain ⟨c, p, vs, site, store, k, ending, _, _⟩ := invalid.2.2.2
  cases ending

/-- Instance (law `ConcreteSpec.Core.Observe.runFrom_outOfFuel_report_none`, case `zero_fuel_at_request`). -/
example : (requesting 3).final? = none ∧
    CompleteTrace permittingContract ()
      (adapter.project (Exec.runFrom program answeringOracle 0 [1] (requesting 3)).events) ∧
    reportObservation program adapter permittingContract ()
      (Exec.runFrom program answeringOracle 0 [1] (requesting 3))
      (runFrom_blockingPermitted program adapter permittingContract () answeringOracle
        (answeringWellFormed (fun _ _ _ => True)) prior_complete) = none :=
  runFrom_outOfFuel_report_none program adapter permittingContract () answeringOracle
    (answeringWellFormed (fun _ _ _ => True)) 0 [1] (requesting 3) (requesting 3)
    prior_complete rfl

/-- Planted (law `ConcreteSpec.Core.Observe.runFrom_outOfFuel_report_none`, case `zero_fuel_at_request`,
subject `ConcreteSpec.Core.Observe.reportObservation`, mutant `ConcreteSpec.Core.Observe.divergeOnOutOfFuelReport`). -/
example : ExecAdapter.WellFormed adapter permittingContract () answeringOracle ∧
    CompleteTrace permittingContract () (adapter.project [1]) ∧
    (Exec.runFrom program answeringOracle 0 [1] (requesting 3)).ending =
      .outOfFuel (requesting 3) ∧
    ¬ ((requesting 3).final? = none ∧
      CompleteTrace permittingContract ()
        (adapter.project (Exec.runFrom program answeringOracle 0 [1] (requesting 3)).events) ∧
      divergeOnOutOfFuelReport program adapter permittingContract ()
        (Exec.runFrom program answeringOracle 0 [1] (requesting 3))
        (runFrom_blockingPermitted program adapter permittingContract () answeringOracle
          (answeringWellFormed (fun _ _ _ => True)) prior_complete) = none) := by
  refine ⟨answeringWellFormed (fun _ _ _ => True), prior_complete, rfl, ?_⟩
  intro invalid
  have hnone := invalid.2.2
  change (some (.finiteDivergence [request 1, response 2]) : Option (FullObservation signature)) =
    none at hnone
  cases hnone

/-- Instance (law `ConcreteSpec.Core.Observe.runFrom_outOfFuel_report_none`, case `reject_fabricated_exit_on_out_of_fuel`). -/
example : (requesting 3).final? = none ∧
    CompleteTrace permittingContract ()
      (adapter.project (Exec.runFrom program answeringOracle 0 [1] (requesting 3)).events) ∧
    reportObservation program adapter permittingContract ()
      (Exec.runFrom program answeringOracle 0 [1] (requesting 3))
      (runFrom_blockingPermitted program adapter permittingContract () answeringOracle
        (answeringWellFormed (fun _ _ _ => True)) prior_complete) = none :=
  runFrom_outOfFuel_report_none program adapter permittingContract () answeringOracle
    (answeringWellFormed (fun _ _ _ => True)) 0 [1] (requesting 3) (requesting 3)
    prior_complete rfl

/-- Planted (law `ConcreteSpec.Core.Observe.runFrom_outOfFuel_report_none`, case `reject_fabricated_exit_on_out_of_fuel`,
subject `ConcreteSpec.Core.Observe.reportObservation`, mutant `ConcreteSpec.Core.Observe.exitZeroOnOutOfFuelReport`). -/
example : ExecAdapter.WellFormed adapter permittingContract () answeringOracle ∧
    CompleteTrace permittingContract () (adapter.project [1]) ∧
    (Exec.runFrom program answeringOracle 0 [1] (requesting 3)).ending =
      .outOfFuel (requesting 3) ∧
    ¬ ((requesting 3).final? = none ∧
      CompleteTrace permittingContract ()
        (adapter.project (Exec.runFrom program answeringOracle 0 [1] (requesting 3)).events) ∧
      exitZeroOnOutOfFuelReport program adapter permittingContract ()
        (Exec.runFrom program answeringOracle 0 [1] (requesting 3))
        (runFrom_blockingPermitted program adapter permittingContract () answeringOracle
          (answeringWellFormed (fun _ _ _ => True)) prior_complete) = none) := by
  refine ⟨answeringWellFormed (fun _ _ _ => True), prior_complete, rfl, ?_⟩
  intro invalid
  have hnone := invalid.2.2
  change (some (.terminal ⟨[request 1, response 2], .exited 0, ()⟩) :
    Option (FullObservation signature)) = none at hnone
  cases hnone

/-- Instance (law `ConcreteSpec.Core.Observe.launch_missing_grant_status_125`, case `missing_grant_refuses_startup`). -/
example : launchObservation ["Fs", "Net"] ["Fs"]
      program adapter permittingContract () blockingOracle 1 (requesting 3) blockedPermitted =
      some (.terminal ⟨[], .startupRefused, adapter.state (requesting 3).store⟩) ∧
    Terminal.status .startupRefused = 125 ∧
    Terminal.trapLine? .startupRefused = none :=
  launch_missing_grant_status_125 ["Fs", "Net"] ["Fs"] "Net"
    program adapter permittingContract () blockingOracle 1 (requesting 3) blockedPermitted
    (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.launch_missing_grant_status_125`, case `missing_grant_refuses_startup`,
subject `ConcreteSpec.Core.Observe.launchObservation`, mutant `ConcreteSpec.Core.Observe.ignoreGrantsLaunchObservation`). -/
example : "Net" ∈ ["Fs", "Net"] ∧
    "Net" ∉ ["Fs"] ∧
    ¬ (ignoreGrantsLaunchObservation ["Fs", "Net"] ["Fs"]
        program adapter permittingContract () blockingOracle 1 (requesting 3) blockedPermitted =
        some (.terminal ⟨[], .startupRefused, adapter.state (requesting 3).store⟩) ∧
      Terminal.status .startupRefused = 125 ∧
      Terminal.trapLine? .startupRefused = none) := by
  refine ⟨by decide, by decide, ?_⟩
  intro invalid
  have heq := invalid.1
  change (some (.blocked [request 3] (query 3)) : Option (FullObservation signature)) =
    some (.terminal ⟨[], .startupRefused, ()⟩) at heq
  cases heq

/-- Instance (law `ConcreteSpec.Core.Observe.launch_missing_grant_status_125`, case `missing_grant_status_code`). -/
example : launchObservation ["Fs", "Net"] ["Fs"]
      program adapter permittingContract () blockingOracle 1 (requesting 3) blockedPermitted =
      some (.terminal ⟨[], .startupRefused, adapter.state (requesting 3).store⟩) ∧
    Terminal.status .startupRefused = 125 ∧
    Terminal.trapLine? .startupRefused = none :=
  launch_missing_grant_status_125 ["Fs", "Net"] ["Fs"] "Net"
    program adapter permittingContract () blockingOracle 1 (requesting 3) blockedPermitted
    (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.launch_missing_grant_status_125`, case `missing_grant_status_code`,
subject `ConcreteSpec.Core.Observe.Terminal.status`, mutant `ConcreteSpec.Core.Observe.wrongStartupStatus`). -/
example : "Net" ∈ ["Fs", "Net"] ∧
    "Net" ∉ ["Fs"] ∧
    ¬ (launchObservation ["Fs", "Net"] ["Fs"]
        program adapter permittingContract () blockingOracle 1 (requesting 3) blockedPermitted =
        some (.terminal ⟨[], .startupRefused, adapter.state (requesting 3).store⟩) ∧
      wrongStartupStatus .startupRefused = 125 ∧
      Terminal.trapLine? .startupRefused = none) := by
  refine ⟨by decide, by decide, ?_⟩
  intro invalid
  have hstatus := invalid.2.1
  change 1 = 125 at hstatus
  omega

/-- A configuration that aborts at `Demo.site`. -/
def abortingConfig : Exec.Config ops := ⟨.exec (.abort Demo.site), [], (), []⟩

/-- Instance (law `ConcreteSpec.Core.Observe.runFrom_trapped_status_134`, case `trapped_report_not_exited_134`). -/
example : reportObservation program relativePathAdapter permittingContract ()
      (Exec.runFrom program blockingOracle 1 [1] abortingConfig)
      (runFrom_blockingPermitted program relativePathAdapter permittingContract () blockingOracle
        relativePathWellFormed prior_complete) =
      some (.terminal ⟨relativePathAdapter.project
          (Exec.runFrom program blockingOracle 1 [1] abortingConfig).events,
        .trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp),
        relativePathAdapter.state ()⟩) ∧
    Demo.sp.PathOk ∧
    Terminal.status (.trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp)) = 134 ∧
    Terminal.trapLine? (.trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp)) =
      some (trapLine .abort Demo.site Demo.sp) ∧
    (.trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp) : Terminal) ≠
      .exited 134 :=
  runFrom_trapped_status_134 program relativePathAdapter permittingContract () blockingOracle
    relativePathWellFormed 1 [1] abortingConfig prior_complete .abort Demo.site () Demo.sp rfl rfl

/-- Planted (law `ConcreteSpec.Core.Observe.runFrom_trapped_status_134`, case `trapped_report_not_exited_134`,
subject `ConcreteSpec.Core.Observe.reportObservation`, mutant `ConcreteSpec.Core.Observe.exit134OnTrapReport`). -/
example : ExecAdapter.WellFormed relativePathAdapter permittingContract () blockingOracle ∧
    CompleteTrace permittingContract () (relativePathAdapter.project [1]) ∧
    (Exec.runFrom program blockingOracle 1 [1] abortingConfig).ending =
      .final (.trapped .abort Demo.site) () ∧
    AList.lookup relativePathAdapter.sites Demo.site = some Demo.sp ∧
    ¬ (exit134OnTrapReport program relativePathAdapter permittingContract ()
        (Exec.runFrom program blockingOracle 1 [1] abortingConfig)
        (runFrom_blockingPermitted program relativePathAdapter permittingContract () blockingOracle
          relativePathWellFormed prior_complete) =
        some (.terminal ⟨relativePathAdapter.project
            (Exec.runFrom program blockingOracle 1 [1] abortingConfig).events,
          .trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp),
          relativePathAdapter.state ()⟩) ∧
      Demo.sp.PathOk ∧
      Terminal.status (.trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp)) = 134 ∧
      Terminal.trapLine? (.trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp)) =
        some (trapLine .abort Demo.site Demo.sp) ∧
      (.trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp) : Terminal) ≠
        .exited 134) := by
  refine ⟨relativePathWellFormed, prior_complete, rfl, rfl, ?_⟩
  intro invalid
  have heq := invalid.1
  change (some (.terminal ⟨[request 1, response 2], .exited 134, ()⟩) :
      Option (FullObservation signature)) =
    some (.terminal ⟨[request 1, response 2],
      .trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp), ()⟩) at heq
  cases heq

/-- Instance (law `ConcreteSpec.Core.Observe.runFrom_trapped_status_134`, case `trapped_exit_status_134`). -/
example : reportObservation program relativePathAdapter permittingContract ()
      (Exec.runFrom program blockingOracle 1 [1] abortingConfig)
      (runFrom_blockingPermitted program relativePathAdapter permittingContract () blockingOracle
        relativePathWellFormed prior_complete) =
      some (.terminal ⟨relativePathAdapter.project
          (Exec.runFrom program blockingOracle 1 [1] abortingConfig).events,
        .trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp),
        relativePathAdapter.state ()⟩) ∧
    Demo.sp.PathOk ∧
    Terminal.status (.trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp)) = 134 ∧
    Terminal.trapLine? (.trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp)) =
      some (trapLine .abort Demo.site Demo.sp) ∧
    (.trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp) : Terminal) ≠
      .exited 134 :=
  runFrom_trapped_status_134 program relativePathAdapter permittingContract () blockingOracle
    relativePathWellFormed 1 [1] abortingConfig prior_complete .abort Demo.site () Demo.sp rfl rfl

/-- Planted (law `ConcreteSpec.Core.Observe.runFrom_trapped_status_134`, case `trapped_exit_status_134`,
subject `ConcreteSpec.Core.Observe.Terminal.status`, mutant `ConcreteSpec.Core.Observe.wrongTrapStatus`). -/
example : ExecAdapter.WellFormed relativePathAdapter permittingContract () blockingOracle ∧
    CompleteTrace permittingContract () (relativePathAdapter.project [1]) ∧
    (Exec.runFrom program blockingOracle 1 [1] abortingConfig).ending =
      .final (.trapped .abort Demo.site) () ∧
    AList.lookup relativePathAdapter.sites Demo.site = some Demo.sp ∧
    ¬ (reportObservation program relativePathAdapter permittingContract ()
        (Exec.runFrom program blockingOracle 1 [1] abortingConfig)
        (runFrom_blockingPermitted program relativePathAdapter permittingContract () blockingOracle
          relativePathWellFormed prior_complete) =
        some (.terminal ⟨relativePathAdapter.project
            (Exec.runFrom program blockingOracle 1 [1] abortingConfig).events,
          .trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp),
          relativePathAdapter.state ()⟩) ∧
      Demo.sp.PathOk ∧
      wrongTrapStatus (.trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp)) = 134 ∧
      Terminal.trapLine? (.trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp)) =
        some (trapLine .abort Demo.site Demo.sp) ∧
      (.trapped .abort Demo.site Demo.sp (trapLine .abort Demo.site Demo.sp) : Terminal) ≠
        .exited 134) := by
  refine ⟨relativePathWellFormed, prior_complete, rfl, rfl, ?_⟩
  intro invalid
  have hstatus := invalid.2.2.1
  change 1 = 134 at hstatus
  omega

def absSpan : Span := ⟨"/tmp/main.con", 1, 1, by decide, by decide⟩
def absSites : SiteTable := [(⟨7⟩, absSpan)]

def backslashSpan : Span := ⟨"src\\main.con", 1, 1, by decide, by decide⟩
def backslashSites : SiteTable := [(⟨7⟩, backslashSpan)]

/-- Instance (law `ConcreteSpec.Core.Observe.trapTerminal_eq_some_iff`, case `reject_absolute_path`). -/
example : trapTerminal absSites .overflow ⟨7⟩ =
      some (.trapped .overflow ⟨7⟩ absSpan (trapLine .overflow ⟨7⟩ absSpan)) ↔
    AList.lookup absSites ⟨7⟩ = some absSpan ∧ absSpan.PathOk ∧
      trapLine .overflow ⟨7⟩ absSpan = trapLine .overflow ⟨7⟩ absSpan :=
  trapTerminal_eq_some_iff absSites .overflow ⟨7⟩ absSpan (trapLine .overflow ⟨7⟩ absSpan)

/-- Planted (law `ConcreteSpec.Core.Observe.trapTerminal_eq_some_iff`, case `reject_absolute_path`,
subject `ConcreteSpec.Core.Observe.trapTerminal`, mutant `ConcreteSpec.Core.Observe.uncheckedTrapTerminal`). -/
example : ¬ (uncheckedTrapTerminal absSites .overflow ⟨7⟩ =
      some (.trapped .overflow ⟨7⟩ absSpan (trapLine .overflow ⟨7⟩ absSpan)) ↔
    AList.lookup absSites ⟨7⟩ = some absSpan ∧ absSpan.PathOk ∧
      trapLine .overflow ⟨7⟩ absSpan = trapLine .overflow ⟨7⟩ absSpan) := by decide

/-- Instance (law `ConcreteSpec.Core.Observe.trapTerminal_eq_some_iff`, case `reject_backslash_path`). -/
example : trapTerminal backslashSites .overflow ⟨7⟩ =
      some (.trapped .overflow ⟨7⟩ backslashSpan (trapLine .overflow ⟨7⟩ backslashSpan)) ↔
    AList.lookup backslashSites ⟨7⟩ = some backslashSpan ∧ backslashSpan.PathOk ∧
      trapLine .overflow ⟨7⟩ backslashSpan = trapLine .overflow ⟨7⟩ backslashSpan :=
  trapTerminal_eq_some_iff backslashSites .overflow ⟨7⟩ backslashSpan
    (trapLine .overflow ⟨7⟩ backslashSpan)

/-- Planted (law `ConcreteSpec.Core.Observe.trapTerminal_eq_some_iff`, case `reject_backslash_path`,
subject `ConcreteSpec.Core.Observe.trapTerminal`, mutant `ConcreteSpec.Core.Observe.uncheckedTrapTerminal`). -/
example : ¬ (uncheckedTrapTerminal backslashSites .overflow ⟨7⟩ =
      some (.trapped .overflow ⟨7⟩ backslashSpan (trapLine .overflow ⟨7⟩ backslashSpan)) ↔
    AList.lookup backslashSites ⟨7⟩ = some backslashSpan ∧ backslashSpan.PathOk ∧
      trapLine .overflow ⟨7⟩ backslashSpan = trapLine .overflow ⟨7⟩ backslashSpan) := by decide

/-- An adapter fault resolves a trap to an absolute host path. -/
def absolutePathAdapter : ExecAdapter ops signature :=
  { adapter with sites := absSites }

/-- Refused: a blocking policy cannot authorize an invalid trap-site path. This example tests
site-table validity directly, without relying on an unrelated answer or continuation failure. -/
example : ¬ ExecAdapter.WellFormed absolutePathAdapter permittingContract () blockingOracle := by
  intro formed
  have path := formed.sites_valid ⟨7⟩ absSpan rfl
  exact (by decide : ¬ absSpan.PathOk) path

/-- An adapter fault uses host-specific backslashes in a relative path. -/
def backslashPathAdapter : ExecAdapter ops signature :=
  { adapter with sites := backslashSites }

/-- Refused: relative spelling alone does not satisfy the forward-slash path requirement. -/
example : ¬ ExecAdapter.WellFormed backslashPathAdapter permittingContract () blockingOracle := by
  intro formed
  have path := formed.sites_valid ⟨7⟩ backslashSpan rfl
  exact (by decide : ¬ backslashSpan.PathOk) path

end ReportDemo
end ConcreteSpec.Core.Observe
