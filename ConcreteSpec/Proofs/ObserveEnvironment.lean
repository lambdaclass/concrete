import ConcreteSpec.Proofs.Observe2

/-! # Independent environment production and trace stepping

`Responds` specifies relational environment admission, while `Environment` specifies executable
environment dispatch by operation kind. The admission bridge is a helper. The operational laws execute typed handlers through C1,
preserving computed replies and ordered histories, and advance completed traces to valid
completed or pending traces; `Demo` and
`RoutingDemo` supply the closed `Instance` and `Planted` witnesses.
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
Executing a typed handler call in C1 records the ordered request and computed reply, passes the
same reply as the result, preserves the C1 store, and extends a valid history to a complete
contract-compliant trace. No response-admission guard is evaluated by this bridge. -/
theorem environment_run_reply {S : EventSignature} (C : OperationContract S)
    (E : Environment S C.Input) (validEnv : Environment.Valid C E) (input : C.Input)
    (hist : List (Request S × Response S)) (q : Request S) (site : SiteId)
    (reply : S.Response q.1)
    (validHist : CompleteTrace C input (Environment.history hist))
    (answered : E.reply input (Environment.history hist ++ [.request q]) q.1 q.2 = some reply) :
    Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 1 hist (Environment.call S q site) =
      ⟨hist ++ [(q, ⟨q.1, reply⟩)], .final .normal ()⟩ ∧
    CompleteTrace C input
      ((Environment.adapter S).project
        (Exec.runFrom (O := Environment.operands S) ⟨[]⟩ (E.oracle input) 1 hist (Environment.call S q site)).events) := by
  constructor
  · have answer : E.oracle input hist q [] site () =
        some ⟨some (q, ⟨q.1, reply⟩), .ok (some ⟨q.1, reply⟩), ()⟩ := by
      simp only [Environment.oracle, answered, Option.map_some]
    unfold Environment.call
    rw [Exec.runFrom.eq_def]
    dsimp [Exec.Config.final?, Exec.step, Exec.evalArgs, Exec.onRes, Environment.operands]
    rw [answer]
    rfl
  · exact (runFrom_validTrace ⟨[]⟩ (Environment.adapter S) C input (E.oracle input)
      (environment_oracle_wellFormed C E validEnv input) 1 hist
      (Environment.call S q site) validHist).1

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
witnesses of `environment_respond_valid` and `environment_stepHistory_valid`.
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
example : Exec.runFrom ⟨[]⟩ (Demo.envHandler.oracle ()) 1 []
      (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩) =
      ⟨[] ++ [(⟨.double, 3⟩, ⟨.double, 6⟩)], .final .normal ()⟩ ∧
    CompleteTrace Demo.contract () ((Environment.adapter Demo.signature).project
      (Exec.runFrom ⟨[]⟩ (Demo.envHandler.oracle ()) 1 []
        (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩)).events) :=
  environment_run_reply Demo.contract Demo.envHandler Demo.envHandler_valid () []
    ⟨.double, 3⟩ ⟨7⟩ 6 .idle rfl

/-- Planted (law `ConcreteSpec.Core.Observe.environment_run_reply`, case `doubling_c1_call`,
subject `ConcreteSpec.Core.Observe.Environment.oracle`, mutant `ConcreteSpec.Core.Observe.noReplyOracle`). -/
example : Environment.Valid Demo.contract Demo.envHandler ∧
    CompleteTrace Demo.contract () (Environment.history []) ∧
    Demo.envHandler.reply () (Environment.history [] ++ [.request ⟨.double, 3⟩]) .double 3 = some 6 ∧
    ¬ (Exec.runFrom ⟨[]⟩ (noReplyOracle Demo.envHandler ()) 1 []
        (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩) =
        ⟨[] ++ [(⟨.double, 3⟩, ⟨.double, 6⟩)], .final .normal ()⟩ ∧
      CompleteTrace Demo.contract () ((Environment.adapter Demo.signature).project
        (Exec.runFrom ⟨[]⟩ (noReplyOracle Demo.envHandler ()) 1 []
          (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩)).events)) := by
  refine ⟨Demo.envHandler_valid, .idle, rfl, ?_⟩
  intro invalid
  have ending := congrArg Exec.Report.ending invalid.1
  cases ending

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
example : Exec.runFrom ⟨[]⟩ (handler.oracle ()) 1 prior
      (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩) =
      ⟨prior ++ [(⟨.double, 3⟩, ⟨.double, 9⟩)], .final .normal ()⟩ ∧
    CompleteTrace contract () ((Environment.adapter Demo.signature).project
      (Exec.runFrom ⟨[]⟩ (handler.oracle ()) 1 prior
        (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩)).events) :=
  environment_run_reply contract handler handler_valid () prior ⟨.double, 3⟩ ⟨7⟩ 9 prior_valid rfl

/-- Planted (law `ConcreteSpec.Core.Observe.environment_run_reply`, case `history_sensitive_c1_call`,
subject `ConcreteSpec.Core.Observe.Environment.oracle`, mutant `ConcreteSpec.Core.Observe.forgetHistoryOracle`). -/
example : Environment.Valid contract handler ∧
    CompleteTrace contract () (Environment.history prior) ∧
    handler.reply () (Environment.history prior ++ [.request ⟨.double, 3⟩]) .double 3 = some 9 ∧
    ¬ (Exec.runFrom ⟨[]⟩ (forgetHistoryOracle handler ()) 1 prior
        (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩) =
        ⟨prior ++ [(⟨.double, 3⟩, ⟨.double, 9⟩)], .final .normal ()⟩ ∧
      CompleteTrace contract () ((Environment.adapter Demo.signature).project
        (Exec.runFrom ⟨[]⟩ (forgetHistoryOracle handler ()) 1 prior
          (Environment.call Demo.signature ⟨.double, 3⟩ ⟨7⟩)).events)) := by
  refine ⟨handler_valid, prior_valid, rfl, ?_⟩
  intro invalid
  have responses := congrArg (fun r : Exec.Report (Environment.operands Demo.signature) =>
    r.events.map fun pair => pair.2.2) invalid.1
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
witnesses of `environment_respond_valid` and `environment_stepHistory_valid`.
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
      change req * 2 = req + req
      omega
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
example : Exec.runFrom ⟨[]⟩ (envHandler.oracle ()) 1 []
      (Environment.call signature ⟨.fourTimes, 3⟩ ⟨7⟩) =
      ⟨[] ++ [(⟨.fourTimes, 3⟩, ⟨.fourTimes, 12⟩)], .final .normal ()⟩ ∧
    CompleteTrace contract () ((Environment.adapter signature).project
      (Exec.runFrom ⟨[]⟩ (envHandler.oracle ()) 1 []
        (Environment.call signature ⟨.fourTimes, 3⟩ ⟨7⟩)).events) :=
  environment_run_reply contract envHandler envHandler_valid () []
    ⟨.fourTimes, 3⟩ ⟨7⟩ 12 .idle rfl

/-- Planted (law `ConcreteSpec.Core.Observe.environment_run_reply`, case `routed_c1_call`,
subject `ConcreteSpec.Core.Observe.Environment.oracle`, mutant `ConcreteSpec.Core.Observe.noReplyOracle`). -/
example : Environment.Valid contract envHandler ∧
    CompleteTrace contract () (Environment.history []) ∧
    envHandler.reply () (Environment.history [] ++ [.request ⟨.fourTimes, 3⟩]) .fourTimes 3 = some 12 ∧
    ¬ (Exec.runFrom ⟨[]⟩ (noReplyOracle envHandler ()) 1 []
        (Environment.call signature ⟨.fourTimes, 3⟩ ⟨7⟩) =
        ⟨[] ++ [(⟨.fourTimes, 3⟩, ⟨.fourTimes, 12⟩)], .final .normal ()⟩ ∧
      CompleteTrace contract () ((Environment.adapter signature).project
        (Exec.runFrom ⟨[]⟩ (noReplyOracle envHandler ()) 1 []
          (Environment.call signature ⟨.fourTimes, 3⟩ ⟨7⟩)).events)) := by
  refine ⟨envHandler_valid, .idle, rfl, ?_⟩
  intro invalid
  have ending := congrArg Exec.Report.ending invalid.1
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

end ConcreteSpec.Core.Observe
