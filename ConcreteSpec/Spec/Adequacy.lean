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

end ConcreteSpec.Core.Exec
