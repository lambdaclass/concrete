import ConcreteSpec.Proofs.Observe2

/-! # Valid observations of evaluator runs

Unlike the individual report branch equations, this property is checked on actual runs. The
trace obligation is independent of the report's representation: blocked histories must contain
the pending request, and terminal histories must discharge every request.
-/

namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core

/-- The independently stated trace and environment obligations of a reported observation.
`none` is an evaluator/premise report and makes no claim about program behavior. -/
def ReportTraceValid {S : EventSignature} (C : OperationContract S) (input : C.Input) :
    Option (FullObservation S) → Prop
  | none => True
  | some (.terminal obs) => CompleteTrace C input obs.events
  | some (.blocked events q) => ValidTrace C input (some q) events ∧ C.mayBlock input events q
  | some (.finiteDivergence _) | some (.infiniteDivergence _) => False

/-- Law (subject `ConcreteSpec.Core.Observe.reportObservation`).
Reporting an actual well-formed evaluator run preserves sequential trace validity, and an
observed pending request has the environment's blocking permission in the observed history.
The evaluator induction in `ConcreteSpec.Core.Observe.runFrom_validTrace` establishes the
history obligation; permission is inherited from the independently supplied environment policy,
not evidence that every environment is permitted to block. -/
theorem runFrom_report_trace {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (ρ : Exec.Oracle O) (wellFormed : ExecAdapter.WellFormed A C input ρ)
    (fuel : Nat) (hist : List O.Event) (c : Exec.Config O)
    (valid : CompleteTrace C input (A.project hist)) :
    ReportTraceValid C input (reportObservation P A C input (Exec.runFrom P ρ fuel hist c)
      (runFrom_blockingPermitted P A C input ρ wellFormed valid)) := by
  obtain ⟨history, _⟩ := runFrom_validTrace P A C input ρ wellFormed fuel hist c valid
  generalize ending : (Exec.runFrom P ρ fuel hist c).ending = e
  cases e with
  | final f =>
    simp only [reportObservation, ending]
    cases A.terminal? f <;> simp_all [ReportTraceValid]
  | noAnswer pending =>
    simp only [reportObservation, ending]
    cases step : Exec.step P pending with
    | none => trivial
    | some out =>
      cases out with
      | next next => trivial
      | request p vs site store k =>
        cases typed : A.request p vs site store with
        | none => simp [typed, ReportTraceValid]
        | some q =>
          simp only [typed, Option.map_some, ReportTraceValid]
          exact runFrom_noAnswer_pending_valid P A C input ρ wellFormed valid ending step typed
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

/-- Helper (inversion). A trap returned by a well-formed adapter resolves to a span whose
path meets the site-table obligation. This inverts the terminal projection and uses the adapter's
explicit semantic premise; it is not independent elaboration correctness. (approved: PENDING) -/
theorem terminal_trap_path_inv {O : Exec.Operands} {S : EventSignature}
    (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input) (ρ : Exec.Oracle O)
    (wellFormed : ExecAdapter.WellFormed A C input ρ)
    {final : Exec.Final O.Val} {kind : TrapKind} {site : SiteId} {sp : Span} {line : String}
    (observed : A.terminal? final = some (.trapped kind site sp line)) : sp.PathOk := by
  cases final with
  | normal => cases observed
  | returned value =>
    cases status : A.status value <;> simp [ExecAdapter.terminal?, status] at observed
  | trapped k s =>
    cases resolved : AList.lookup A.sites s with
    | none => simp [ExecAdapter.terminal?, trapTerminal, resolved] at observed
    | some actual =>
      simp only [ExecAdapter.terminal?, trapTerminal, resolved, Option.map_some,
        Option.some.injEq, Terminal.trapped.injEq] at observed
      obtain ⟨rfl, rfl, rfl, rfl⟩ := observed
      exact wellFormed.sites_valid s actual resolved

namespace ReportDemo
open AdapterDemo

/-- Helper (computation). A closed witness for the nonempty completed history used in the
operational instance, not an independent contract law. (approved: PENDING) -/
theorem prior_complete : CompleteTrace permittingContract () (adapter.project [1]) :=
  .response (.request .idle) (RequestResponse.matched (S := signature) Op.double 1 2) rfl

/-- Instance (law `ConcreteSpec.Core.Observe.runFrom_report_trace`, case `prior_exchange_block`). -/
example : ReportTraceValid permittingContract ()
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
    ¬ ReportTraceValid permittingContract ()
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
  exact formed.blocked (hist := [1]) (p := 3) (vs := []) (site := ⟨0⟩) (store := ())
    (q := query 3) rfl rfl

/-- Fuel-zero computation at an actual requesting configuration. The driver reports fuel
exhaustion and the reporter fabricates no observation; this is a computation example, not an
independent contract law. -/
example : (Exec.runFrom program answeringOracle 0 [] (requesting 3)).ending =
      .outOfFuel (requesting 3) ∧
    reportObservation program adapter forbiddingContract ()
      (Exec.runFrom program answeringOracle 0 [] (requesting 3))
      (runFrom_blockingPermitted program adapter forbiddingContract () answeringOracle
        (answeringWellFormed (fun _ _ _ => False)) (fuel := 0) (hist := [])
        (c := requesting 3) .idle) = none := ⟨rfl, rfl⟩

/-- An adapter fault resolves a trap to an absolute host path. -/
def absolutePathAdapter : ExecAdapter ops signature :=
  { adapter with sites := [(⟨7⟩, ⟨"/tmp/main.con", 1, 1, by decide, by decide⟩)] }

/-- Refused: a blocking policy cannot authorize an invalid trap-site path. This example tests
site-table validity directly, without relying on an unrelated answer or continuation failure. -/
example : ¬ ExecAdapter.WellFormed absolutePathAdapter permittingContract () blockingOracle := by
  intro formed
  have path := formed.sites_valid ⟨7⟩ ⟨"/tmp/main.con", 1, 1, by decide, by decide⟩ rfl
  exact (by decide : ¬ (⟨"/tmp/main.con", 1, 1, by decide, by decide⟩ : Span).PathOk) path

/-- An adapter fault uses host-specific backslashes in a relative path. -/
def backslashPathAdapter : ExecAdapter ops signature :=
  { adapter with sites := [(⟨7⟩, ⟨"src\\main.con", 1, 1, by decide, by decide⟩)] }

/-- Refused: relative spelling alone does not satisfy the forward-slash path requirement. -/
example : ¬ ExecAdapter.WellFormed backslashPathAdapter permittingContract () blockingOracle := by
  intro formed
  have path := formed.sites_valid ⟨7⟩ ⟨"src\\main.con", 1, 1, by decide, by decide⟩ rfl
  exact (by decide : ¬ (⟨"src\\main.con", 1, 1, by decide, by decide⟩ : Span).PathOk) path

end ReportDemo
end ConcreteSpec.Core.Observe
