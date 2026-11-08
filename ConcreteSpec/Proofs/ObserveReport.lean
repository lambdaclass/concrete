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
  have history := (runFrom_validTrace P A C input ρ wellFormed fuel hist c valid).1
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

/-- A report adapter fault: a blocked result drops its history, including the pending request.
The environment, evaluator and all admission premises are unchanged. -/
def dropBlockedHistory {O : Exec.Operands} {S : EventSignature}
    (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (r : Exec.Report O) (permitted : ExecAdapter.BlockingPermitted P A C input r) :
    Option (FullObservation S) :=
  match reportObservation P A C input r permitted with
  | some (.blocked _ q) => some (.blocked [] q)
  | other => other

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

end ReportDemo
end ConcreteSpec.Core.Observe
