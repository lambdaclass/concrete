import ConcreteSpec.Spec.Observe

/-! # Independent environment production and trace stepping

`Responds` specifies relational environment admission, while `Environment` specifies executable
environment dispatch by operation kind. The general laws here prove that any valid `Environment`
handler produces answers satisfying `Responds`, preserves `RequestResponse` routing by construction,
and advances `CompleteTrace` histories to valid completed or pending traces; `Demo` and
`RoutingDemo` supply the closed `Instance` and `Planted` witnesses.
-/

namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core

/-- Law (subject `ConcreteSpec.Core.Observe.Environment.respond`).
For any valid executable environment handler `E`, dispatching `E.respond` on a pending request `q`
after `history ++ [.request q]` satisfies the relational environment contract `Responds C input
(history ++ [.request q]) q`, and whenever `E.reply` returns `some resp`, the resulting answer is
`.response ⟨q.1, resp⟩` with `RequestResponse q ⟨q.1, resp⟩` and `C.allows`. -/
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

/-- Instance (law `ConcreteSpec.Core.Observe.environment_respond_valid`, case `doubling_reply`). -/
example : Responds Demo.contract () ([] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩
      (Demo.envHandler.respond () ([] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩) ∧
    ∀ resp, Demo.envHandler.reply () ([] ++ [.request ⟨.double, 3⟩]) .double 3 = some resp →
      Demo.envHandler.respond () ([] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩ =
        .response ⟨.double, resp⟩ ∧
      RequestResponse (S := Demo.signature) ⟨.double, 3⟩ ⟨.double, resp⟩ ∧
      Demo.contract.allows () ([] ++ [.request ⟨.double, 3⟩]) ⟨.double, 3⟩ ⟨.double, resp⟩ :=
  environment_respond_valid Demo.contract Demo.envHandler Demo.envHandler_valid () [] ⟨.double, 3⟩

/-- Planted (law `ConcreteSpec.Core.Observe.environment_respond_valid`, case `doubling_reply`,
subject `ConcreteSpec.Core.Observe.Environment.respond`, mutant `ConcreteSpec.Core.Observe.alwaysBlockedRespond`). -/
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

/-- Instance (law `ConcreteSpec.Core.Observe.environment_respond_valid`, case `routed_reply`). -/
example : Responds contract () ([] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩
      (envHandler.respond () ([] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩) ∧
    ∀ resp, envHandler.reply () ([] ++ [.request ⟨.fourTimes, 3⟩]) .fourTimes 3 = some resp →
      envHandler.respond () ([] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩ =
        .response ⟨.fourTimes, resp⟩ ∧
      RequestResponse (S := signature) ⟨.fourTimes, 3⟩ ⟨.fourTimes, resp⟩ ∧
      contract.allows () ([] ++ [.request ⟨.fourTimes, 3⟩]) ⟨.fourTimes, 3⟩ ⟨.fourTimes, resp⟩ :=
  environment_respond_valid contract envHandler envHandler_valid () [] ⟨.fourTimes, 3⟩

/-- Planted (law `ConcreteSpec.Core.Observe.environment_respond_valid`, case `routed_reply`,
subject `ConcreteSpec.Core.Observe.Environment.respond`, mutant `ConcreteSpec.Core.Observe.alwaysBlockedRespond`). -/
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
