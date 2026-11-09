import ConcreteSpec.Proofs.Observe2

/-! # Independent environment production and trace stepping

`Responds` specifies relational environment admission, while `Environment` specifies executable
environment dispatch by operation kind. The admission bridge is a helper. The operational laws execute typed handlers through C1,
binding and returning the computed reply in `Environment.call`, preserving the C1 store, connecting
both answered and blocked runs to `reportObservation`, and advancing completed traces to valid
completed or pending traces; `Demo`, `HistoryDemo` and `RoutingDemo` supply the closed `Instance`
and `Planted` witnesses.
-/

namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core

/-- Helper (computation). Admission bridge for typed handlers: repackages handler compliance
and dependent routing. Operational C1 evidence is `environment_run_reply`. (approved: PENDING) -/
theorem environment_respond_valid {S : EventSignature} (C : OperationContract S)
    (E : Environment S C.Input) (valid : Environment.Valid C E)
    (input : C.Input) (history : List (Event S)) (q : Request S) :
    Responds C input (history ++ [.request q]) q
      (E.respond input (history ++ [.request q]) q) ∧
    ∀ resp, E.reply input (history ++ [.request q]) q.1 q.2 = some resp →
      E.respond input (history ++ [.request q]) q = .response ⟨q.1, resp⟩ ∧
      RequestResponse q ⟨q.1, resp⟩ ∧
      C.allows input (history ++ [.request q]) q ⟨q.1, resp⟩ := by
  obtain ⟨k, req⟩ := q
  constructor
  · unfold Environment.respond
    cases h : E.reply input (history ++ [.request ⟨k, req⟩]) k req with
    | none =>
      exact Responds.blocked (valid.mayBlock h)
    | some resp =>
      exact Responds.response (RequestResponse.matched k req resp) (valid.allows h)
  · intro resp h
    refine ⟨?_, RequestResponse.matched k req resp, valid.allows h⟩
    simp [Environment.respond, h]

/-- Helper (computation). Establishes every C1 adapter obligation for the oracle actually built
from typed handlers. Contract admission is inherited from the handler's independent proof;
answer encoding, histories and stores are computed by the bridge. (approved: PENDING) -/
theorem environment_oracle_wellFormed {S : EventSignature} (C : OperationContract S)
    (E : Environment S C.Input) (valid : Environment.Valid C E) (input : C.Input) :
    ExecAdapter.WellFormed (Environment.adapter S) C input (E.oracle input) := by
  constructor
  · intro hist p vs site store a answer typed
    cases typed
  · intro hist q vs site store a typedQ answer typed
    have equal : q = typedQ := Option.some.inj typed
    subst typedQ
    unfold Environment.oracle at answer
    cases h : E.reply input (Environment.history hist ++ [.request q]) q.1 q.2 with
    | none => simp [h] at answer
    | some reply =>
      simp only [h, Option.map_some, Option.some.injEq] at answer
      subst a
      refine ⟨[], ⟨q.1, reply⟩, [], ?_, ⟨[], rfl⟩, ⟨[], rfl⟩,
        RequestResponse.matched q.1 q.2 reply, ?_⟩
      · rfl
      · cases q
        simpa only [ExecAdapter.project, Environment.adapter, Environment.history,
          List.append_nil] using valid.allows h
  · intro hist q vs site store answer
    refine ⟨q, rfl, ?_⟩
    unfold Environment.oracle at answer
    have blocked : E.reply input (Environment.history hist ++ [.request q]) q.1 q.2 = none := by
      cases h : E.reply input (Environment.history hist ++ [.request q]) q.1 q.2 with
      | none => rfl
      | some reply => simp [h] at answer
    exact valid.mayBlock blocked
  · intro site sp found
    cases found

/-- Law (subject `ConcreteSpec.Core.Observe.Environment.oracle`).
Executing a typed handler call in C1 binds the oracle's returned value to local `⟨0⟩` and returns
it while preserving the C1 store `store : S.State`. When `E.reply` produces `some reply`, the C1
run ends in `.final (.returned (some ⟨q.1, reply⟩)) store`, `reportObservation` reports the
terminal `.exited 0` observation with the extended event history and final external state `store`,
and the projected history is a `CompleteTrace`. When `E.reply` returns `none`, the C1 run blocks
at the primitive request, `reportObservation` reports `.blocked`, and the pending trace satisfies
`ValidTrace` and `C.mayBlock`. -/
theorem environment_run_reply {S : EventSignature} (C : OperationContract S)
    (E : Environment S C.Input) (validEnv : Environment.Valid C E) (input : C.Input)
    (hist : List (Request S × Response S)) (q : Request S) (site : SiteId) (store : S.State)
    (validHist : CompleteTrace C input (Environment.history hist)) :
    (∀ reply, E.reply input (Environment.history hist ++ [.request q]) q.1 q.2 = some reply →
      Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
          (Environment.call S q site store) =
        ⟨hist ++ [(q, ⟨q.1, reply⟩)], .final (.returned (some ⟨q.1, reply⟩)) store⟩ ∧
      (∃ permitted,
        reportObservation ⟨[]⟩ (Environment.adapter S) C input
          (Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
            (Environment.call S q site store)) permitted =
          some (.terminal ⟨Environment.history hist ++ [.request q, .response ⟨q.1, reply⟩],
            .exited 0, store⟩)) ∧
      CompleteTrace C input
        ((Environment.adapter S).project
          (Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
            (Environment.call S q site store)).events)) ∧
    (E.reply input (Environment.history hist ++ [.request q]) q.1 q.2 = none →
      (∃ permitted,
        reportObservation ⟨[]⟩ (Environment.adapter S) C input
          (Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
            (Environment.call S q site store)) permitted =
          some (.blocked (Environment.history hist ++ [.request q]) q)) ∧
      ValidTrace C input (some q) (Environment.history hist ++ [.request q]) ∧
      C.mayBlock input (Environment.history hist ++ [.request q]) q) := by
  have wf := environment_oracle_wellFormed C E validEnv input
  have hperm : ExecAdapter.BlockingPermitted ⟨[]⟩ (Environment.adapter S) C input
      (Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
        (Environment.call S q site store)) :=
    runFrom_blockingPermitted ⟨[]⟩ (Environment.adapter S) C input
      (E.oracle input) wf (fuel := 4) (hist := hist) (c := Environment.call S q site store) validHist
  constructor
  · intro reply answered
    have answer : E.oracle input hist q [] site store =
        some ⟨some (q, ⟨q.1, reply⟩), .ok (some ⟨q.1, reply⟩), store⟩ := by
      simp only [Environment.oracle, answered, Option.map_some]
    have hrun : Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
        (Environment.call S q site store) =
        ⟨hist ++ [(q, ⟨q.1, reply⟩)], .final (.returned (some ⟨q.1, reply⟩)) store⟩ := by
      unfold Environment.call
      conv => lhs; unfold Exec.runFrom
      dsimp [Exec.Config.final?, Exec.step]
      conv => lhs; unfold Exec.runFrom
      dsimp [Exec.Config.final?, Exec.step, Exec.evalArgs, Exec.onRes, Environment.operands]
      rw [answer]
      dsimp [Exec.bindLife, Exec.bindDst]
      conv => lhs; unfold Exec.runFrom
      dsimp [Exec.Config.final?, Exec.step]
      conv => lhs; unfold Exec.runFrom
      dsimp [Exec.Config.final?, Exec.step, Exec.onRes, Environment.operands, AList.lookup]
      unfold Exec.runFrom
      rfl
    refine ⟨hrun, ⟨hperm, ?_⟩, ?_⟩
    · simp [reportObservation, hrun, ExecAdapter.terminal?, Environment.adapter,
        ExecAdapter.project, Environment.history]
    · exact (runFrom_validTrace ⟨[]⟩ (Environment.adapter S) C input (E.oracle input) wf
        4 hist (Environment.call S q site store) validHist).1
  · intro blocked
    have answer : E.oracle input hist q [] site store = none := by
      simp [Environment.oracle, blocked]
    have hrun : Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 4 hist
        (Environment.call S q site store) =
        ⟨hist, .noAnswer ⟨.exec (.prim (some ⟨"reply", 0⟩) q [] site), [], store, [.seq (.ret ⟨"reply", 0⟩ site)]⟩⟩ := by
      unfold Environment.call
      conv => lhs; unfold Exec.runFrom
      dsimp [Exec.Config.final?, Exec.step]
      conv => lhs; unfold Exec.runFrom
      dsimp [Exec.Config.final?, Exec.step, Exec.evalArgs, Exec.onRes, Environment.operands]
      rw [answer]
    refine ⟨⟨hperm, ?_⟩, ValidTrace.request validHist, ?_⟩
    · simp only [reportObservation, hrun, Exec.step, Exec.evalArgs, Exec.onRes,
        Environment.operands, Environment.adapter, Option.map_some]
      rfl
    · obtain ⟨k, req⟩ := q
      exact validEnv.mayBlock blocked

/-- Fault: records the response event and preserves the store, but returns `none` as the C1
primitive result value instead of `some ⟨q.1, reply⟩`. -/
def dropReturnValueOracle {S : EventSignature} {Input : Type}
    (E : Environment S Input) (input : Input) : Exec.Oracle (Environment.operands S) :=
  fun hist q _ _ store =>
    (E.reply input (Environment.history hist ++ [.request q]) q.1 q.2).map fun reply =>
      ⟨some (q, ⟨q.1, reply⟩), .ok none, store⟩

/-- Fault: discards every typed handler answer before C1 can consume it. -/
def noReplyOracle {S : EventSignature} {Input : Type}
    (_E : Environment S Input) (_input : Input) : Exec.Oracle (Environment.operands S) :=
  fun _ _ _ _ _ => none

/-- Fault: invokes the right handler with an empty preceding C1 history. -/
def forgetHistoryOracle {S : EventSignature} {Input : Type}
    (E : Environment S Input) (input : Input) : Exec.Oracle (Environment.operands S) :=
  fun _ q vs site store => E.oracle input [] q vs site store

/-- Fault: environment dispatch always reports `.blocked` instead of delivering the handler's
computed response. -/
def alwaysBlockedRespond {S : EventSignature} {Input : Type}
    (_E : Environment S Input) (_input : Input) (_history : List (Event S))
    (_q : Request S) : Answer S :=
  .blocked

/-- Law (subject `ConcreteSpec.Core.Observe.Environment.stepHistory`).
Stepping a valid completed trace `history` with a valid environment handler `E` on request `q`
produces a `ValidTrace` whose pending state matches `E.respond`, and when `E.reply` produces
`some resp`, `E.stepHistory` appends `[.request q, .response ⟨q.1, resp⟩]` and yields a
`CompleteTrace`. -/
theorem environment_stepHistory_valid {S : EventSignature} (C : OperationContract S)
    (E : Environment S C.Input) (validEnv : Environment.Valid C E)
    (input : C.Input) (history : List (Event S)) (q : Request S)
    (validTrace : CompleteTrace C input history) :
    ValidTrace C input (E.stepHistory input history q).2
      (E.stepHistory input history q).1 ∧
    ∀ resp, E.reply input (history ++ [.request q]) q.1 q.2 = some resp →
      E.stepHistory input history q =
        (history ++ [.request q, .response ⟨q.1, resp⟩], none) ∧
      CompleteTrace C input (history ++ [.request q, .response ⟨q.1, resp⟩]) := by
  obtain ⟨k, req⟩ := q
  have hreq : ValidTrace C input (some ⟨k, req⟩) (history ++ [.request ⟨k, req⟩]) :=
    ValidTrace.request validTrace
  constructor
  · unfold Environment.stepHistory Environment.respond
    cases h : E.reply input (history ++ [.request ⟨k, req⟩]) k req with
    | none =>
      exact hreq
    | some resp =>
      simpa only [List.append_assoc, List.singleton_append] using
        ValidTrace.response hreq (RequestResponse.matched k req resp) (validEnv.allows h)
  · intro resp h
    have hcomp : CompleteTrace C input
        (history ++ [.request ⟨k, req⟩, .response ⟨k, resp⟩]) := by
      simpa only [CompleteTrace, List.append_assoc, List.singleton_append] using
        ValidTrace.response hreq (RequestResponse.matched k req resp) (validEnv.allows h)
    refine ⟨?_, hcomp⟩
    simp [Environment.stepHistory, Environment.respond, h]

/-- Fault: records the environment's response in the trace without first recording the request. -/
def dropRequestStepHistory {S : EventSignature} {Input : Type}
    (E : Environment S Input) (input : Input) (history : List (Event S))
    (q : Request S) : List (Event S) × Option (Request S) :=
  match E.respond input (history ++ [.request q]) q with
  | .response r => (history ++ [.response r], none)
  | .blocked => (history ++ [.request q], some q)

/-- Fault: leaves the request marked as pending (`some q`) even after recording its response. -/
def leavePendingOnAnswerStepHistory {S : EventSignature} {Input : Type}
    (E : Environment S Input) (input : Input) (history : List (Event S))
    (q : Request S) : List (Event S) × Option (Request S) :=
  match E.respond input (history ++ [.request q]) q with
  | .response r => (history ++ [.request q, .response r], some q)
  | .blocked => (history ++ [.request q], some q)

/-- Executable environment handler for `Demo`, implementing doubling by addition. -/
def Demo.envHandler : Environment Demo.signature Unit where
  reply := fun _ _ _ n => some (n + n)

/-- Helper (computation). Proves that `Demo.envHandler` satisfies `Environment.Valid` for
`Demo.contract` by arithmetic (`n * 2 = n + n`), feeding the closed `Instance` and `Planted`
witnesses of `environment_run_reply` and `environment_stepHistory_valid`.
(approved: PENDING) -/
theorem Demo.envHandler_valid : Environment.Valid Demo.contract Demo.envHandler := by
  constructor
  · intro input history k req resp h
    cases k
    change Nat at req resp
    simp only [Demo.envHandler, Option.some.injEq] at h
    subst h
    change req * 2 = req + req
    omega
  · intro input history k req h
    simp [Demo.envHandler] at h

/-- Instance (law `ConcreteSpec.Core.Observe.environment_run_reply`, case `doubling_c1_call`). -/
example : (∀ reply, Demo.envHandler.reply () (Environment.history [] ++ [.request ⟨.double, 3⟩]) .double 3 = some reply →
      Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (Demo.envHandler.oracle ()) 4 []
          (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7) =
        ⟨[] ++ [(⟨.double, 3⟩, ⟨.double, reply⟩)], .final (.returned (some ⟨.double, reply⟩)) 7⟩ ∧
      (∃ permitted,
        reportObservation ⟨[]⟩ (Environment.adapter Demo.signature) Demo.contract ()
          (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (Demo.envHandler.oracle ()) 4 []
            (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)) permitted =
          some (.terminal ⟨Environment.history [] ++ [.request ⟨.double, 3⟩, .response ⟨.double, reply⟩],
            .exited 0, 7⟩)) ∧
      CompleteTrace Demo.contract ()
        ((Environment.adapter Demo.signature).project
          (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (Demo.envHandler.oracle ()) 4 []
            (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)).events)) ∧
    (Demo.envHandler.reply () (Environment.history [] ++ [.request ⟨.double, 3⟩]) .double 3 = none →
      (∃ permitted,
        reportObservation ⟨[]⟩ (Environment.adapter Demo.signature) Demo.contract ()
          (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (Demo.envHandler.oracle ()) 4 []
            (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)) permitted =
          some (.blocked (Environment.history [] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩)) ∧
      ValidTrace Demo.contract () (some ⟨.double, 3⟩) (Environment.history [] ++ [.request ⟨.double, 3⟩]) ∧
      Demo.contract.mayBlock () (Environment.history [] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩) :=
  environment_run_reply Demo.contract Demo.envHandler Demo.envHandler_valid () []
    ⟨.double, 3⟩ ⟨7⟩ 7 .idle

/-- Planted (law `ConcreteSpec.Core.Observe.environment_run_reply`, case `doubling_c1_call`,
subject `ConcreteSpec.Core.Observe.Environment.oracle`, mutant `ConcreteSpec.Core.Observe.dropReturnValueOracle`). -/
example : Environment.Valid Demo.contract Demo.envHandler ∧
    CompleteTrace Demo.contract () (Environment.history []) ∧
    ¬ ((∀ reply, Demo.envHandler.reply () (Environment.history [] ++ [.request ⟨.double, 3⟩]) .double 3 = some reply →
        Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (dropReturnValueOracle Demo.envHandler ()) 4 []
            (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7) =
          ⟨[] ++ [(⟨.double, 3⟩, ⟨.double, reply⟩)], .final (.returned (some ⟨.double, reply⟩)) 7⟩ ∧
        (∃ permitted,
          reportObservation ⟨[]⟩ (Environment.adapter Demo.signature) Demo.contract ()
            (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (dropReturnValueOracle Demo.envHandler ()) 4 []
              (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)) permitted =
            some (.terminal ⟨Environment.history [] ++ [.request ⟨.double, 3⟩, .response ⟨.double, reply⟩],
              .exited 0, 7⟩)) ∧
        CompleteTrace Demo.contract ()
          ((Environment.adapter Demo.signature).project
            (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (dropReturnValueOracle Demo.envHandler ()) 4 []
              (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)).events)) ∧
      (Demo.envHandler.reply () (Environment.history [] ++ [.request ⟨.double, 3⟩]) .double 3 = none →
        (∃ permitted,
          reportObservation ⟨[]⟩ (Environment.adapter Demo.signature) Demo.contract ()
            (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (dropReturnValueOracle Demo.envHandler ()) 4 []
              (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)) permitted =
            some (.blocked (Environment.history [] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩)) ∧
        ValidTrace Demo.contract () (some ⟨.double, 3⟩) (Environment.history [] ++ [.request ⟨.double, 3⟩]) ∧
        Demo.contract.mayBlock () (Environment.history [] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩)) := by
  refine ⟨Demo.envHandler_valid, .idle, ?_⟩
  intro invalid
  have hrun := (invalid.1 6 rfl).1
  have hend := congrArg Exec.Report.ending hrun
  change (Exec.Ending.final (.returned none) 7 : Exec.Ending (Environment.operands Demo.signature)) =
    .final (.returned (some ⟨.double, 6⟩)) 7 at hend
  cases hend

/-- Computation example for the handler admission bridge, not independent law evidence. -/
example : Responds Demo.contract () ([] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩
      (Demo.envHandler.respond () ([] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩) ∧
    ∀ resp, Demo.envHandler.reply () ([] ++ [.request ⟨.double, 3⟩]) .double 3 = some resp →
      Demo.envHandler.respond () ([] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩ =
        .response ⟨.double, resp⟩ ∧
      RequestResponse (S := Demo.signature) ⟨.double, 3⟩ ⟨.double, resp⟩ ∧
      Demo.contract.allows () ([] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩ ⟨.double, resp⟩ :=
  environment_respond_valid Demo.contract Demo.envHandler Demo.envHandler_valid () [] ⟨.double, 3⟩

/-- Computation example for the handler admission bridge, not independent law evidence. -/
example : Environment.Valid Demo.contract Demo.envHandler ∧
    ¬ (Responds Demo.contract () ([] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩
        (alwaysBlockedRespond Demo.envHandler () ([] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩) ∧
      ∀ resp, Demo.envHandler.reply () ([] ++ [.request ⟨.double, 3⟩]) .double 3 = some resp →
        alwaysBlockedRespond Demo.envHandler () ([] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩ =
          .response ⟨.double, resp⟩ ∧
        RequestResponse (S := Demo.signature) ⟨.double, 3⟩ ⟨.double, resp⟩ ∧
        Demo.contract.allows () ([] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩ ⟨.double, resp⟩) := by
  refine ⟨Demo.envHandler_valid, ?_⟩
  intro invalid
  cases invalid.1 with
  | blocked hblock => exact hblock

/-- Instance (law `ConcreteSpec.Core.Observe.environment_stepHistory_valid`, case `doubling_exchange_step`). -/
example : ValidTrace Demo.contract () (Demo.envHandler.stepHistory () [] ⟨.double, 3⟩).2
      (Demo.envHandler.stepHistory () [] ⟨.double, 3⟩).1 ∧
    ∀ resp, Demo.envHandler.reply () ([] ++ [.request ⟨.double, 3⟩]) .double 3 = some resp →
      Demo.envHandler.stepHistory () [] ⟨.double, 3⟩ =
        ([] ++ [.request ⟨.double, 3⟩, .response ⟨.double, resp⟩], none) ∧
      CompleteTrace Demo.contract ()
        ([] ++ [.request ⟨.double, 3⟩, .response ⟨.double, resp⟩]) :=
  environment_stepHistory_valid Demo.contract Demo.envHandler Demo.envHandler_valid () []
    ⟨.double, 3⟩ .idle

/-- Planted (law `ConcreteSpec.Core.Observe.environment_stepHistory_valid`, case `doubling_exchange_step`,
subject `ConcreteSpec.Core.Observe.Environment.stepHistory`, mutant `ConcreteSpec.Core.Observe.dropRequestStepHistory`). -/
example : Environment.Valid Demo.contract Demo.envHandler ∧
    CompleteTrace Demo.contract () [] ∧
    ¬ (ValidTrace Demo.contract () (dropRequestStepHistory Demo.envHandler () [] ⟨.double, 3⟩).2
        (dropRequestStepHistory Demo.envHandler () [] ⟨.double, 3⟩).1 ∧
      ∀ resp, Demo.envHandler.reply () ([] ++ [.request ⟨.double, 3⟩]) .double 3 = some resp →
        dropRequestStepHistory Demo.envHandler () [] ⟨.double, 3⟩ =
          ([] ++ [.request ⟨.double, 3⟩, .response ⟨.double, resp⟩], none) ∧
        CompleteTrace Demo.contract ()
          ([] ++ [.request ⟨.double, 3⟩, .response ⟨.double, resp⟩])) := by
  refine ⟨Demo.envHandler_valid, .idle, ?_⟩
  intro invalid
  have hstep := (invalid.2 6 rfl).1
  change ([Demo.response 6], (none : Option (Request Demo.signature))) =
    ([Demo.request 3, Demo.response 6], none) at hstep
  have hlen := congrArg (fun p : List (Event Demo.signature) × Option (Request Demo.signature) =>
    p.1.length) hstep
  contradiction

namespace HistoryDemo

/-- A history-sensitive fixture: the current request is part of the handler's input history. -/
def contract : OperationContract Demo.signature where
  Input := Unit
  allows := fun _ history q r => q.2 * 2 + history.length = r.2
  mayBlock := fun _ _ _ => False

def handler : Environment Demo.signature Unit where
  reply := fun _ history _ n => some (n + n + history.length)

/-- Helper (arithmetic). Proves the independent history-sensitive operation policy for its
addition-based handler; no admission or response guard is executed. (approved: PENDING) -/
theorem handler_valid : Environment.Valid contract handler := by
  constructor
  · intro input history k req resp answer
    cases k
    simp only [handler, Option.some.injEq] at answer
    subst resp
    exact congrArg (fun n : Nat =>
      n + (history ++ [Event.request (S := Demo.signature) ⟨.double, req⟩]).length)
      (Nat.mul_two req)
  · intro input history k req answer
    simp [handler] at answer

def prior : List (Request Demo.signature × Response Demo.signature) :=
  [(⟨.double, 1⟩, ⟨.double, 3⟩)]

/-- Helper (computation). The first exchange satisfies the policy at its own request history.
It is used as a closed premise of the actual second C1 call. (approved: PENDING) -/
theorem prior_valid : CompleteTrace contract () (Environment.history prior) :=
  .response (.request .idle) (RequestResponse.matched _ _ _) rfl

/-- Instance (law `ConcreteSpec.Core.Observe.environment_run_reply`, case `history_sensitive_c1_call`). -/
example : (∀ reply, handler.reply () (Environment.history prior ++ [.request ⟨.double, 3⟩]) .double 3 = some reply →
      Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (handler.oracle ()) 4 prior
          (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7) =
        ⟨prior ++ [(⟨.double, 3⟩, ⟨.double, reply⟩)], .final (.returned (some ⟨.double, reply⟩)) 7⟩ ∧
      (∃ permitted,
        reportObservation ⟨[]⟩ (Environment.adapter Demo.signature) contract ()
          (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (handler.oracle ()) 4 prior
            (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)) permitted =
          some (.terminal ⟨Environment.history prior ++ [.request ⟨.double, 3⟩, .response ⟨.double, reply⟩],
            .exited 0, 7⟩)) ∧
      CompleteTrace contract ()
        ((Environment.adapter Demo.signature).project
          (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (handler.oracle ()) 4 prior
            (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)).events)) ∧
    (handler.reply () (Environment.history prior ++ [.request ⟨.double, 3⟩]) .double 3 = none →
      (∃ permitted,
        reportObservation ⟨[]⟩ (Environment.adapter Demo.signature) contract ()
          (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (handler.oracle ()) 4 prior
            (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)) permitted =
          some (.blocked (Environment.history prior ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩)) ∧
      ValidTrace contract () (some ⟨.double, 3⟩) (Environment.history prior ++ [.request ⟨.double, 3⟩]) ∧
      contract.mayBlock () (Environment.history prior ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩) :=
  environment_run_reply contract handler handler_valid () prior ⟨.double, 3⟩ ⟨7⟩ 7 prior_valid

/-- Planted (law `ConcreteSpec.Core.Observe.environment_run_reply`, case `history_sensitive_c1_call`,
subject `ConcreteSpec.Core.Observe.Environment.oracle`, mutant `ConcreteSpec.Core.Observe.forgetHistoryOracle`). -/
example : Environment.Valid contract handler ∧
    CompleteTrace contract () (Environment.history prior) ∧
    ¬ ((∀ reply, handler.reply () (Environment.history prior ++ [.request ⟨.double, 3⟩]) .double 3 = some reply →
        Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (forgetHistoryOracle handler ()) 4 prior
            (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7) =
          ⟨prior ++ [(⟨.double, 3⟩, ⟨.double, reply⟩)], .final (.returned (some ⟨.double, reply⟩)) 7⟩ ∧
        (∃ permitted,
          reportObservation ⟨[]⟩ (Environment.adapter Demo.signature) contract ()
            (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (forgetHistoryOracle handler ()) 4 prior
              (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)) permitted =
            some (.terminal ⟨Environment.history prior ++ [.request ⟨.double, 3⟩, .response ⟨.double, reply⟩],
              .exited 0, 7⟩)) ∧
        CompleteTrace contract ()
          ((Environment.adapter Demo.signature).project
            (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (forgetHistoryOracle handler ()) 4 prior
              (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)).events)) ∧
      (handler.reply () (Environment.history prior ++ [.request ⟨.double, 3⟩]) .double 3 = none →
        (∃ permitted,
          reportObservation ⟨[]⟩ (Environment.adapter Demo.signature) contract ()
            (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (forgetHistoryOracle handler ()) 4 prior
              (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)) permitted =
            some (.blocked (Environment.history prior ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩)) ∧
        ValidTrace contract () (some ⟨.double, 3⟩) (Environment.history prior ++ [.request ⟨.double, 3⟩]) ∧
        contract.mayBlock () (Environment.history prior ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩)) := by
  refine ⟨handler_valid, prior_valid, ?_⟩
  intro invalid
  have hrun := (invalid.1 9 rfl).1
  have responses := congrArg (fun r : Exec.Report (Environment.operands Demo.signature) =>
    r.events.map fun pair => pair.2.2) hrun
  change [3, 7] = [3, 9] at responses
  exact (by decide : ([3, 7] : List Nat) ≠ [3, 9]) responses

end HistoryDemo

namespace RoutingDemo

inductive Kind where
  | twice
  | fourTimes
  deriving DecidableEq

abbrev signature : EventSignature where
  Kind := Kind
  Request := fun _ => Nat
  Response := fun _ => Nat
  Output := String
  State := Unit
  initState := ()

/-- Separate operation policy: response routing and multiplication determine correctness. -/
def contract : OperationContract signature where
  Input := Unit
  allows := fun _ _ q r => q.1 = r.1 ∧
    q.2 * (match q.1 with | .twice => 2 | .fourTimes => 4) = r.2
  mayBlock := fun _ _ _ => False

/-- Executable environment handler dispatching on `Kind` by addition. -/
def envHandler : Environment signature Unit where
  reply := fun _ _ k n =>
    match k with
    | .twice => some (n + n)
    | .fourTimes => some ((n + n) + (n + n))

/-- Helper (computation). Proves that `RoutingDemo.envHandler` satisfies `Environment.Valid` for
both `.twice` and `.fourTimes` operation kinds, feeding the closed `Instance` and `Planted`
witnesses of `environment_run_reply` and `environment_stepHistory_valid`.
(approved: PENDING) -/
theorem envHandler_valid : Environment.Valid contract envHandler := by
  constructor
  · intro input history k req resp h
    cases k with
    | twice =>
      change Nat at req resp
      simp only [envHandler, Option.some.injEq] at h
      subst h
      refine ⟨rfl, ?_⟩
      exact Nat.mul_two req
    | fourTimes =>
      change Nat at req resp
      simp only [envHandler, Option.some.injEq] at h
      subst h
      refine ⟨rfl, ?_⟩
      change req * 4 = (req + req) + (req + req)
      omega
  · intro input history k req h
    cases k <;> simp [envHandler] at h

/-- Instance (law `ConcreteSpec.Core.Observe.environment_run_reply`, case `routed_c1_call`). -/
example : (∀ reply, envHandler.reply () (Environment.history [] ++ [.request ⟨.fourTimes, 3⟩]) .fourTimes 3 = some reply →
      Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (envHandler.oracle ()) 4 []
          (Environment.call signature ⟨.fourTimes, 3⟩ ⟨7⟩ ()) =
        ⟨[] ++ [(⟨.fourTimes, 3⟩, ⟨.fourTimes, reply⟩)], .final (.returned (some ⟨.fourTimes, reply⟩)) ()⟩ ∧
      (∃ permitted,
        reportObservation ⟨[]⟩ (Environment.adapter signature) contract ()
          (Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (envHandler.oracle ()) 4 []
            (Environment.call signature ⟨.fourTimes, 3⟩ ⟨7⟩ ())) permitted =
          some (.terminal ⟨Environment.history [] ++ [.request ⟨.fourTimes, 3⟩, .response ⟨.fourTimes, reply⟩],
            .exited 0, ()⟩)) ∧
      CompleteTrace contract ()
        ((Environment.adapter signature).project
          (Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (envHandler.oracle ()) 4 []
            (Environment.call signature ⟨.fourTimes, 3⟩ ⟨7⟩ ())).events)) ∧
    (envHandler.reply () (Environment.history [] ++ [.request ⟨.fourTimes, 3⟩]) .fourTimes 3 = none →
      (∃ permitted,
        reportObservation ⟨[]⟩ (Environment.adapter signature) contract ()
          (Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (envHandler.oracle ()) 4 []
            (Environment.call signature ⟨.fourTimes, 3⟩ ⟨7⟩ ())) permitted =
          some (.blocked (Environment.history [] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩)) ∧
      ValidTrace contract () (some ⟨.fourTimes, 3⟩) (Environment.history [] ++ [.request ⟨.fourTimes, 3⟩]) ∧
      contract.mayBlock () (Environment.history [] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩) :=
  environment_run_reply contract envHandler envHandler_valid () []
    ⟨.fourTimes, 3⟩ ⟨7⟩ () .idle

/-- Planted (law `ConcreteSpec.Core.Observe.environment_run_reply`, case `routed_c1_call`,
subject `ConcreteSpec.Core.Observe.Environment.oracle`, mutant `ConcreteSpec.Core.Observe.noReplyOracle`). -/
example : Environment.Valid contract envHandler ∧
    CompleteTrace contract () (Environment.history []) ∧
    ¬ ((∀ reply, envHandler.reply () (Environment.history [] ++ [.request ⟨.fourTimes, 3⟩]) .fourTimes 3 = some reply →
        Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (noReplyOracle envHandler ()) 4 []
            (Environment.call signature ⟨.fourTimes, 3⟩ ⟨7⟩ ()) =
          ⟨[] ++ [(⟨.fourTimes, 3⟩, ⟨.fourTimes, reply⟩)], .final (.returned (some ⟨.fourTimes, reply⟩)) ()⟩ ∧
        (∃ permitted,
          reportObservation ⟨[]⟩ (Environment.adapter signature) contract ()
            (Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (noReplyOracle envHandler ()) 4 []
              (Environment.call signature ⟨.fourTimes, 3⟩ ⟨7⟩ ())) permitted =
            some (.terminal ⟨Environment.history [] ++ [.request ⟨.fourTimes, 3⟩, .response ⟨.fourTimes, reply⟩],
              .exited 0, ()⟩)) ∧
        CompleteTrace contract ()
          ((Environment.adapter signature).project
            (Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (noReplyOracle envHandler ()) 4 []
              (Environment.call signature ⟨.fourTimes, 3⟩ ⟨7⟩ ())).events)) ∧
      (envHandler.reply () (Environment.history [] ++ [.request ⟨.fourTimes, 3⟩]) .fourTimes 3 = none →
        (∃ permitted,
          reportObservation ⟨[]⟩ (Environment.adapter signature) contract ()
            (Exec.runFrom (O := Environment.operands signature) ⟨[]⟩ (noReplyOracle envHandler ()) 4 []
              (Environment.call signature ⟨.fourTimes, 3⟩ ⟨7⟩ ())) permitted =
            some (.blocked (Environment.history [] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩)) ∧
        ValidTrace contract () (some ⟨.fourTimes, 3⟩) (Environment.history [] ++ [.request ⟨.fourTimes, 3⟩]) ∧
        contract.mayBlock () (Environment.history [] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩)) := by
  refine ⟨envHandler_valid, .idle, ?_⟩
  intro invalid
  have hrun := (invalid.1 12 rfl).1
  have ending := congrArg Exec.Report.ending hrun
  cases ending

/-- Computation example for the handler admission bridge, not independent law evidence. -/
example : Responds contract () ([] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩
      (envHandler.respond () ([] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩) ∧
    ∀ resp, envHandler.reply () ([] ++ [.request ⟨.fourTimes, 3⟩]) .fourTimes 3 = some resp →
      envHandler.respond () ([] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩ =
        .response ⟨.fourTimes, resp⟩ ∧
      RequestResponse (S := signature) ⟨.fourTimes, 3⟩ ⟨.fourTimes, resp⟩ ∧
      contract.allows () ([] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩ ⟨.fourTimes, resp⟩ :=
  environment_respond_valid contract envHandler envHandler_valid () [] ⟨.fourTimes, 3⟩

/-- Computation example for the handler admission bridge, not independent law evidence. -/
example : Environment.Valid contract envHandler ∧
    ¬ (Responds contract () ([] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩
        (alwaysBlockedRespond envHandler () ([] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩) ∧
      ∀ resp, envHandler.reply () ([] ++ [.request ⟨.fourTimes, 3⟩]) .fourTimes 3 = some resp →
        alwaysBlockedRespond envHandler () ([] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩ =
          .response ⟨.fourTimes, resp⟩ ∧
        RequestResponse (S := signature) ⟨.fourTimes, 3⟩ ⟨.fourTimes, resp⟩ ∧
        contract.allows () ([] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩ ⟨.fourTimes, resp⟩) := by
  refine ⟨envHandler_valid, ?_⟩
  intro invalid
  cases invalid.1 with
  | blocked hblock => exact hblock

/-- Instance (law `ConcreteSpec.Core.Observe.environment_stepHistory_valid`, case `routed_exchange_step`). -/
example : ValidTrace contract () (envHandler.stepHistory () [] ⟨.twice, 3⟩).2
      (envHandler.stepHistory () [] ⟨.twice, 3⟩).1 ∧
    ∀ resp, envHandler.reply () ([] ++ [.request ⟨.twice, 3⟩]) .twice 3 = some resp →
      envHandler.stepHistory () [] ⟨.twice, 3⟩ =
        ([] ++ [.request ⟨.twice, 3⟩, .response ⟨.twice, resp⟩], none) ∧
      CompleteTrace contract ()
        ([] ++ [.request ⟨.twice, 3⟩, .response ⟨.twice, resp⟩]) :=
  environment_stepHistory_valid contract envHandler envHandler_valid () [] ⟨.twice, 3⟩ .idle

/-- Planted (law `ConcreteSpec.Core.Observe.environment_stepHistory_valid`, case `routed_exchange_step`,
subject `ConcreteSpec.Core.Observe.Environment.stepHistory`, mutant `ConcreteSpec.Core.Observe.leavePendingOnAnswerStepHistory`). -/
example : Environment.Valid contract envHandler ∧
    CompleteTrace contract () [] ∧
    ¬ (ValidTrace contract () (leavePendingOnAnswerStepHistory envHandler () [] ⟨.twice, 3⟩).2
        (leavePendingOnAnswerStepHistory envHandler () [] ⟨.twice, 3⟩).1 ∧
      ∀ resp, envHandler.reply () ([] ++ [.request ⟨.twice, 3⟩]) .twice 3 = some resp →
        leavePendingOnAnswerStepHistory envHandler () [] ⟨.twice, 3⟩ =
          ([] ++ [.request ⟨.twice, 3⟩, .response ⟨.twice, resp⟩], none) ∧
        CompleteTrace contract ()
          ([] ++ [.request ⟨.twice, 3⟩, .response ⟨.twice, resp⟩])) := by
  refine ⟨envHandler_valid, .idle, ?_⟩
  intro invalid
  have hstep := (invalid.2 6 rfl).1
  have hpend := congrArg Prod.snd hstep
  change (some ⟨Kind.twice, 3⟩ : Option (Request signature)) = none at hpend
  cases hpend

end RoutingDemo

namespace BlockingEnvDemo

/-- Contract permitting blocking while still requiring doubled responses when answered. -/
def contract : OperationContract Demo.signature where
  Input := Unit
  allows := fun _ _ q r => q.2 * 2 = r.2
  mayBlock := fun _ _ _ => True

/-- History-sensitive fixture: an empty-history probe answers, while the real history ending
in the current request blocks. -/
def blockingHandler : Environment Demo.signature Unit where
  reply := fun _ hist _ n => if hist.isEmpty then some (n + n) else none

/-- Helper (computation). `blockingHandler` satisfies `Environment.Valid` for `BlockingEnvDemo.contract`
because answered probes double their arguments and unanswered requests may block.
(approved: PENDING) -/
theorem blockingHandler_valid : Environment.Valid contract blockingHandler := by
  constructor
  · intro input history k req resp h
    cases k
    simp only [blockingHandler] at h
    split at h
    · simp only [Option.some.injEq] at h
      subst resp
      exact Nat.mul_two req
    · cases h
  · intro input history k req _
    trivial

/-- Fault: consults the handler at an empty history, fabricating an answer when the actual
history-sensitive request blocks. It has exactly the polymorphic type of `Environment.oracle`. -/
def fabricateReplyOnBlockOracle {S : EventSignature} {Input : Type}
    (E : Environment S Input) (input : Input) : Exec.Oracle (Environment.operands S) :=
  fun _ q _ _ store =>
    (E.reply input [] q.1 q.2).map fun reply =>
      ⟨some (q, ⟨q.1, reply⟩), .ok (some ⟨q.1, reply⟩), store⟩

/-- Instance (law `ConcreteSpec.Core.Observe.environment_run_reply`, case `blocking_c1_call`). -/
example : (∀ reply, blockingHandler.reply () (Environment.history [] ++ [.request ⟨.double, 3⟩]) .double 3 = some reply →
      Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (blockingHandler.oracle ()) 4 []
          (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7) =
        ⟨[] ++ [(⟨.double, 3⟩, ⟨.double, reply⟩)], .final (.returned (some ⟨.double, reply⟩)) 7⟩ ∧
      (∃ permitted,
        reportObservation ⟨[]⟩ (Environment.adapter Demo.signature) contract ()
          (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (blockingHandler.oracle ()) 4 []
            (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)) permitted =
          some (.terminal ⟨Environment.history [] ++ [.request ⟨.double, 3⟩, .response ⟨.double, reply⟩],
            .exited 0, 7⟩)) ∧
      CompleteTrace contract ()
        ((Environment.adapter Demo.signature).project
          (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (blockingHandler.oracle ()) 4 []
            (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)).events)) ∧
    (blockingHandler.reply () (Environment.history [] ++ [.request ⟨.double, 3⟩]) .double 3 = none →
      (∃ permitted,
        reportObservation ⟨[]⟩ (Environment.adapter Demo.signature) contract ()
          (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (blockingHandler.oracle ()) 4 []
            (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)) permitted =
          some (.blocked (Environment.history [] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩)) ∧
      ValidTrace contract () (some ⟨.double, 3⟩) (Environment.history [] ++ [.request ⟨.double, 3⟩]) ∧
      contract.mayBlock () (Environment.history [] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩) :=
  environment_run_reply contract blockingHandler blockingHandler_valid () []
    ⟨.double, 3⟩ ⟨7⟩ 7 .idle

/-- Planted (law `ConcreteSpec.Core.Observe.environment_run_reply`, case `blocking_c1_call`,
subject `ConcreteSpec.Core.Observe.Environment.oracle`, mutant `ConcreteSpec.Core.Observe.BlockingEnvDemo.fabricateReplyOnBlockOracle`). -/
example : Environment.Valid contract blockingHandler ∧
    CompleteTrace contract () (Environment.history []) ∧
    ¬ ((∀ reply, blockingHandler.reply () (Environment.history [] ++ [.request ⟨.double, 3⟩]) .double 3 = some reply →
        Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (fabricateReplyOnBlockOracle blockingHandler ()) 4 []
            (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7) =
          ⟨[] ++ [(⟨.double, 3⟩, ⟨.double, reply⟩)], .final (.returned (some ⟨.double, reply⟩)) 7⟩ ∧
        (∃ permitted,
          reportObservation ⟨[]⟩ (Environment.adapter Demo.signature) contract ()
            (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (fabricateReplyOnBlockOracle blockingHandler ()) 4 []
              (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)) permitted =
            some (.terminal ⟨Environment.history [] ++ [.request ⟨.double, 3⟩, .response ⟨.double, reply⟩],
              .exited 0, 7⟩)) ∧
        CompleteTrace contract ()
          ((Environment.adapter Demo.signature).project
            (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (fabricateReplyOnBlockOracle blockingHandler ()) 4 []
              (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)).events)) ∧
      (blockingHandler.reply () (Environment.history [] ++ [.request ⟨.double, 3⟩]) .double 3 = none →
        (∃ permitted,
          reportObservation ⟨[]⟩ (Environment.adapter Demo.signature) contract ()
            (Exec.runFrom (O := Environment.operands Demo.signature) ⟨[]⟩ (fabricateReplyOnBlockOracle blockingHandler ()) 4 []
              (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩ 7)) permitted =
            some (.blocked (Environment.history [] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩)) ∧
        ValidTrace contract () (some ⟨.double, 3⟩) (Environment.history [] ++ [.request ⟨.double, 3⟩]) ∧
        contract.mayBlock () (Environment.history [] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩)) := by
  refine ⟨blockingHandler_valid, .idle, ?_⟩
  intro invalid
  obtain ⟨permitted, hrep⟩ := (invalid.2 rfl).1
  change (some (.terminal ⟨[Demo.request 3, Demo.response 6], .exited 0, 7⟩) :
      Option (FullObservation Demo.signature)) =
    some (.blocked [Demo.request 3] ⟨.double, 3⟩) at hrep
  cases hrep

end BlockingEnvDemo

end ConcreteSpec.Core.Observe
