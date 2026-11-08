import ConcreteSpec.Spec.Exec

/-!
# Evaluator adequacy and termination

Foundation contract C8 (Core Q6.6–Q6.7 and Q9.9). This module adds the query record that C1's
oracle interface leaves abstract. It does not change the machine or its driver: `runFromQ` is an
instrumented copy of `runFrom`, and `Proofs/Adequacy.lean` proves that erasing the queries gives
exactly the original report.

A query contains the event history, request kind and arguments, logical site, request state, and
its zero-based occurrence number at that site. Occurrences count requests, including requests that
emit no event and requests that receive no answer. A fixed `Oracle` answers a query by the pure
function `answer`; in particular, the fuel budget is not an input.
-/

namespace ConcreteSpec.Core.Exec
open ConcreteSpec.Core

variable {O : Operands}

/-- One oracle query. `occurrence` is the number of earlier queries at the same logical site. -/
structure Query (O : Operands) where
  events : List O.Event
  prim : O.Prim
  args : List O.Val
  site : SiteId
  store : O.Store
  occurrence : Nat

/-- The number of queries already issued at `site`. -/
def occurrenceAt (issued : List (Query O)) (site : SiteId) : Nat :=
  (issued.filter fun q => decide (q.site = site)).length

/-- Construct the next query at a site from the execution history and the requests already issued. -/
def Query.issue (events : List O.Event) (p : O.Prim) (args : List O.Val) (site : SiteId)
    (store : O.Store) (issued : List (Query O)) : Query O :=
  ⟨events, p, args, site, store, occurrenceAt issued site⟩

/-- A fixed oracle's deterministic answer to a query. Fuel and occurrence are not extra inputs to
C1's oracle; the occurrence is nevertheless recorded so a later typed query interface can use it. -/
def answer (ρ : Oracle O) (q : Query O) : Option (Answer O.Val O.Event O.Store) :=
  ρ q.events q.prim q.args q.site q.store

/-- A report together with every query issued, including the last query of a `noAnswer`,
`badAnswer`, or continuation-refused `stuck` report. Events and queries include the histories passed
to `runFromQ`. -/
structure QueryReport (O : Operands) where
  events : List O.Event
  queries : List (Query O)
  ending : Ending O

/-- Forget the query instrumentation. -/
def QueryReport.toReport (r : QueryReport O) : Report O := ⟨r.events, r.ending⟩

/-- `runFrom` instrumented with its issued queries. The query issued at a request is recorded before
matching its answer, so even an unanswered request has an occurrence number. -/
def runFromQ (P : Program O) (ρ : Oracle O) :
    Nat → List O.Event → List (Query O) → Config O → QueryReport O
  | fuel, events, issued, c =>
    match c.final?, fuel with
    | some f, _ => ⟨events, issued, .final f⟩
    | none, 0 => ⟨events, issued, .outOfFuel c⟩
    | none, n + 1 =>
      match step P c with
      | none => ⟨events, issued, .stuck c⟩
      | some (.next c') => runFromQ P ρ n events issued c'
      | some (.request p vs site st k) =>
        let q := Query.issue events p vs site st issued
        match answer ρ q with
        | none => ⟨events, issued ++ [q], .noAnswer c⟩
        | some a =>
          if O.allows p vs st a then
            match k a with
            | some c' => runFromQ P ρ n (events ++ a.event.toList) (issued ++ [q]) c'
            | none => ⟨events, issued ++ [q], .stuck c⟩
          else ⟨events, issued ++ [q], .badAnswer c⟩

/-- Run from `c` with empty event and query histories. -/
def runQ (P : Program O) (ρ : Oracle O) (fuel : Nat) (c : Config O) : QueryReport O :=
  runFromQ P ρ fuel [] [] c

/-- A position in an execution: its configuration and the complete event and query histories before
it. Carrying all three makes finite prefixes compose into one infinite execution, rather than merely
supplying unrelated finite witnesses. -/
structure Position (O : Operands) where
  config : Config O
  events : List O.Event
  queries : List (Query O)

/-- One transition under a fixed oracle. A request records its query before the answer is used. -/
inductive Move (P : Program O) (ρ : Oracle O) : Position O → Position O → Prop where
  | silent {c c' events issued} : step P c = some (.next c') →
      Move P ρ ⟨c, events, issued⟩ ⟨c', events, issued⟩
  | prim {c c' events issued p vs site st k a} :
      step P c = some (.request p vs site st k) →
      answer ρ (Query.issue events p vs site st issued) = some a →
      O.allows p vs st a = true → k a = some c' →
      Move P ρ ⟨c, events, issued⟩
        ⟨c', events ++ a.event.toList, issued ++ [Query.issue events p vs site st issued]⟩

/-- Exactly `n` consecutive oracle-checked transitions. -/
inductive Moves (P : Program O) (ρ : Oracle O) : Nat → Position O → Position O → Prop where
  | refl (pos : Position O) : Moves P ρ 0 pos pos
  | step {n pos pos' pos''} : Move P ρ pos pos' → Moves P ρ n pos' pos'' →
      Moves P ρ (n + 1) pos pos''

/-- Why an evaluator report is justified at the final position of its finite path. The `stuck`
constructors distinguish a refused machine rule from a refused request continuation. -/
inductive Conclusion (P : Program O) (ρ : Oracle O) : Ending O → Position O → Prop where
  | final {f pos} : pos.config.final? = some f → Conclusion P ρ (.final f) pos
  | stuck {c events issued} : step P c = none →
      Conclusion P ρ (.stuck c) ⟨c, events, issued⟩
  | stuckContinuation {c events issued p vs site st k a} :
      step P c = some (.request p vs site st k) →
      answer ρ (Query.issue events p vs site st issued) = some a →
      O.allows p vs st a = true → k a = none →
      Conclusion P ρ (.stuck c) ⟨c, events, issued⟩
  | noAnswer {c events issued p vs site st k} :
      step P c = some (.request p vs site st k) →
      answer ρ (Query.issue events p vs site st issued) = none →
      Conclusion P ρ (.noAnswer c) ⟨c, events, issued⟩
  | badAnswer {c events issued p vs site st k a} :
      step P c = some (.request p vs site st k) →
      answer ρ (Query.issue events p vs site st issued) = some a →
      O.allows p vs st a = false →
      Conclusion P ρ (.badAnswer c) ⟨c, events, issued⟩
  | outOfFuel {c events issued} : c.final? = none →
      Conclusion P ρ (.outOfFuel c) ⟨c, events, issued⟩

/-- An infinite maximal execution under one fixed oracle. Every adjacent pair is a successful
transition, so this excludes final, stuck, blocked, and bad-answer endpoints. -/
structure InfiniteExecution (P : Program O) (ρ : Oracle O) (c : Config O) where
  states : Nat → Position O
  initial : states 0 = ⟨c, [], []⟩
  step : ∀ n, Move P ρ (states n) (states (n + 1))

/-- A compatible limit of fuel-indexed prefixes. The `n`-step run is exactly the `n`-th position;
these are not independently chosen finite witnesses. -/
structure FuelLimit (P : Program O) (ρ : Oracle O) (c : Config O) where
  states : Nat → Position O
  prefixes : ∀ n, runQ P ρ n c =
    ⟨(states n).events, (states n).queries, .outOfFuel (states n).config⟩

/-- An infinite execution emits only finitely many events exactly when its event-history length has
one finite bound. -/
def FiniteEvents {P : Program O} {ρ : Oracle O} {c : Config O}
    (X : InfiniteExecution P ρ c) : Prop :=
  ∃ bound, ∀ n, (X.states n).events.length ≤ bound

/-- An infinite execution interacts forever when its event history outgrows every finite bound. -/
def InfiniteEvents {P : Program O} {ρ : Oracle O} {c : Config O}
    (X : InfiniteExecution P ρ c) : Prop :=
  ∀ bound, ∃ n, bound < (X.states n).events.length

/-- Silent divergence: infinitely many steps but only finitely many externally visible events. -/
def SilentDivergence (P : Program O) (ρ : Oracle O) (c : Config O) : Prop :=
  ∃ X : InfiniteExecution P ρ c, FiniteEvents X

/-- Infinite interaction: an infinite execution with infinitely many events. -/
def InfiniteInteraction (P : Program O) (ρ : Oracle O) (c : Config O) : Prop :=
  ∃ X : InfiniteExecution P ρ c, InfiniteEvents X

/-- Once `fuel` reaches a blocked request, every larger budget retains C1's `noAnswer` ending.
This is C8's blocking classification over C1; it is not divergence and has no infinite `Move`. -/
def EndlesslyBlocked (P : Program O) (ρ : Oracle O) (fuel : Nat) (c blocked : Config O) : Prop :=
  ∀ n, fuel ≤ n → (run P ρ n c).ending = .noAnswer blocked

/-- Per-function termination evidence. `Premises` names the trusted premises; `admits` selects the
allowed oracles, arguments, and stores. `dependencies` names the other function identities whose
termination evidence this certificate uses. The final field is a theorem over every allowed run:
entry succeeds and the fixed fuel budget reports a final outcome. Concrete variants and cost models
are deliberately not part of this interface. -/
structure TerminationEvidence (O : Operands) (P : Program O) where
  function : FnId
  Premises : Type
  dependencies : List FnId
  admits : Premises → Oracle O → List O.Val → O.Store → Prop
  fuel : Premises → Oracle O → List O.Val → O.Store → Nat
  terminates : ∀ premises ρ args st fn c,
    AList.lookup P.fns function = some fn → admits premises ρ args st →
    enter fn args st = some c → ∃ f, (run P ρ (fuel premises ρ args st) c).ending = .final f

end ConcreteSpec.Core.Exec
