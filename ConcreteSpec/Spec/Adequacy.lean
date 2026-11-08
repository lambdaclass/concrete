import ConcreteSpec.Spec.Observe

/-!
# Evaluator adequacy and termination

Foundation contract C8 (Core Q6.6–Q6.7 and Q9.9). This module adds the query record that C1's
oracle interface leaves abstract. It does not change the machine or its driver: `runFromQ` is an
instrumented copy of `runFrom`, and `Proofs/Adequacy.lean` proves that erasing the queries gives
exactly the original report.

A query contains the event history, request kind and arguments, logical site, request state, and
its zero-based occurrence number at that site. Occurrences count requests, including requests that
emit no event and requests that receive no answer. A fixed `QueryOracle` answers by a pure function
of that complete record; in particular, the fuel budget is not an input. C1's `Oracle` is the
special case `Oracle.toQueryOracle` that ignores the occurrence number.
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

/-- Every query in a history carries the number of earlier queries at the same logical site. -/
def QueryHistoryValid (issued : List (Query O)) : Prop :=
  ∀ i q, issued[i]? = some q → q.occurrence = occurrenceAt (issued.take i) q.site

/-- An oracle at the query level. Its answer is a pure function of the complete query, including the
per-site occurrence number; fuel is not an input. -/
def QueryOracle (O : Operands) : Type :=
  Query O → Option (Answer O.Val O.Event O.Store)

/-- C1's oracle is the query-level oracle that ignores the occurrence number. -/
def Oracle.toQueryOracle (ρ : Oracle O) : QueryOracle O := fun q =>
  ρ q.events q.prim q.args q.site q.store

instance : Coe (Oracle O) (QueryOracle O) where
  coe := Oracle.toQueryOracle

/-- A fixed query-level oracle's deterministic answer. -/
def answer (ρ : QueryOracle O) (q : Query O) : Option (Answer O.Val O.Event O.Store) := ρ q

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
def runFromQ (P : Program O) (ρ : QueryOracle O) :
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
def runQ (P : Program O) (ρ : QueryOracle O) (fuel : Nat) (c : Config O) : QueryReport O :=
  runFromQ P ρ fuel [] [] c

/-- A position in an execution: its configuration and the complete event and query histories before
it. Carrying all three makes finite prefixes compose into one infinite execution, rather than merely
supplying unrelated finite witnesses. -/
structure Position (O : Operands) where
  config : Config O
  events : List O.Event
  queries : List (Query O)

/-- The independent oracle/contract specification of a single issued request. Successful answers
produce an oracle-checked transition and its residual report; terminal answers preserve the old
event history and record exactly this query. No answer or continuation is assumed by callers. -/
inductive RequestResolution (P : Program O) (ρ : QueryOracle O) (fuel : Nat)
    (pos : Position O) (p : O.Prim) (vs : List O.Val) (site : SiteId) (st : O.Store)
    (k : Answer O.Val O.Event O.Store → Option (Config O)) : QueryReport O → Prop where
  | unanswered (ha : answer ρ (Query.issue pos.events p vs site st pos.queries) = none) :
      RequestResolution P ρ fuel pos p vs site st k
        ⟨pos.events, pos.queries ++ [Query.issue pos.events p vs site st pos.queries],
          .noAnswer pos.config⟩
  | invalid {a} (ha : answer ρ (Query.issue pos.events p vs site st pos.queries) = some a)
      (hl : O.allows p vs st a = false) :
      RequestResolution P ρ fuel pos p vs site st k
        ⟨pos.events, pos.queries ++ [Query.issue pos.events p vs site st pos.queries],
          .badAnswer pos.config⟩
  | refused {a} (ha : answer ρ (Query.issue pos.events p vs site st pos.queries) = some a)
      (hl : O.allows p vs st a = true) (hk : k a = none) :
      RequestResolution P ρ fuel pos p vs site st k
        ⟨pos.events, pos.queries ++ [Query.issue pos.events p vs site st pos.queries],
          .stuck pos.config⟩
  | continued {a c'}
      (ha : answer ρ (Query.issue pos.events p vs site st pos.queries) = some a)
      (hl : O.allows p vs st a = true) (hk : k a = some c') :
      RequestResolution P ρ fuel pos p vs site st k
        (runFromQ P ρ fuel (pos.events ++ a.event.toList)
          (pos.queries ++ [Query.issue pos.events p vs site st pos.queries]) c')

/-- One transition under a fixed query-level oracle. A request records its query before the answer is
used. -/
inductive Move (P : Program O) (ρ : QueryOracle O) : Position O → Position O → Prop where
  | silent {c c' events issued} : step P c = some (.next c') →
      Move P ρ ⟨c, events, issued⟩ ⟨c', events, issued⟩
  | prim {c c' events issued p vs site st k a} :
      step P c = some (.request p vs site st k) →
      answer ρ (Query.issue events p vs site st issued) = some a →
      O.allows p vs st a = true → k a = some c' →
      Move P ρ ⟨c, events, issued⟩
        ⟨c', events ++ a.event.toList, issued ++ [Query.issue events p vs site st issued]⟩

/-- Exactly `n` consecutive oracle-checked transitions. -/
inductive Moves (P : Program O) (ρ : QueryOracle O) : Nat → Position O → Position O → Prop where
  | refl (pos : Position O) : Moves P ρ 0 pos pos
  | step {n pos pos' pos''} : Move P ρ pos pos' → Moves P ρ n pos' pos'' →
      Moves P ρ (n + 1) pos pos''

/-- Why an evaluator report is justified at the final position of its finite path. The `stuck`
constructors distinguish a refused machine rule from a refused request continuation. -/
inductive Conclusion (P : Program O) (ρ : QueryOracle O) : Ending O → Position O → Prop where
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

/-- The exact query history reported for a conclusion, indexed by that conclusion's justification.
Terminal requests append the query constructed from the request in `step`; other endings append
nothing. Indexing by `Conclusion` also keeps a machine-stuck report distinct from a request whose
continuation was refused. -/
inductive ReportedQueries (P : Program O) (ρ : QueryOracle O) :
    {ending : Ending O} → {pos : Position O} → Conclusion P ρ ending pos →
      List (Query O) → Prop where
  | final {f} {pos : Position O} (hf : pos.config.final? = some f) :
      ReportedQueries P ρ (ending := .final f) (pos := pos) (Conclusion.final hf) pos.queries
  | stuck {c : Config O} {events : List O.Event} {issued : List (Query O)}
      (hs : step P c = none) :
      ReportedQueries P ρ (ending := .stuck c) (pos := ⟨c, events, issued⟩)
        (Conclusion.stuck hs) issued
  | stuckContinuation {c : Config O} {events : List O.Event} {issued : List (Query O)}
      {p vs site st k a} (hs : step P c = some (.request p vs site st k))
      (ha : answer ρ (Query.issue events p vs site st issued) = some a)
      (hallow : O.allows p vs st a = true) (hk : k a = none) :
      ReportedQueries P ρ (ending := .stuck c) (pos := ⟨c, events, issued⟩)
        (Conclusion.stuckContinuation hs ha hallow hk)
        (issued ++ [Query.issue events p vs site st issued])
  | noAnswer {c : Config O} {events : List O.Event} {issued : List (Query O)}
      {p vs site st k} (hs : step P c = some (.request p vs site st k))
      (ha : answer ρ (Query.issue events p vs site st issued) = none) :
      ReportedQueries P ρ (ending := .noAnswer c) (pos := ⟨c, events, issued⟩)
        (Conclusion.noAnswer hs ha) (issued ++ [Query.issue events p vs site st issued])
  | badAnswer {c : Config O} {events : List O.Event} {issued : List (Query O)}
      {p vs site st k a} (hs : step P c = some (.request p vs site st k))
      (ha : answer ρ (Query.issue events p vs site st issued) = some a)
      (hallow : O.allows p vs st a = false) :
      ReportedQueries P ρ (ending := .badAnswer c) (pos := ⟨c, events, issued⟩)
        (Conclusion.badAnswer hs ha hallow) (issued ++ [Query.issue events p vs site st issued])
  | outOfFuel {c : Config O} {events : List O.Event} {issued : List (Query O)}
      (hf : c.final? = none) :
      ReportedQueries P ρ (ending := .outOfFuel c) (pos := ⟨c, events, issued⟩)
        (Conclusion.outOfFuel hf) issued

/-- An infinite maximal execution under one fixed query-level oracle. Every adjacent pair is a
successful transition, so this excludes final, stuck, blocked, and bad-answer endpoints. -/
structure InfiniteExecution (P : Program O) (ρ : QueryOracle O) (c : Config O) where
  states : Nat → Position O
  initial : states 0 = ⟨c, [], []⟩
  step : ∀ n, Move P ρ (states n) (states (n + 1))

/-- A compatible limit of fuel-indexed prefixes. The `n`-step run is exactly the `n`-th position;
these are not independently chosen finite witnesses. -/
structure FuelLimit (P : Program O) (ρ : QueryOracle O) (c : Config O) where
  states : Nat → Position O
  prefixes : ∀ n, runQ P ρ n c =
    ⟨(states n).events, (states n).queries, .outOfFuel (states n).config⟩

/-- The raw C1 event history has a finite bound. This is an internal classification: an adapter
may erase some or all of these events. -/
def InternalFiniteEvents {P : Program O} {ρ : QueryOracle O} {c : Config O}
    (X : InfiniteExecution P ρ c) : Prop :=
  ∃ bound, ∀ n, (X.states n).events.length ≤ bound

/-- The raw C1 event history outgrows every finite bound. This need not be external interaction. -/
def InternalInfiniteEvents {P : Program O} {ρ : QueryOracle O} {c : Config O}
    (X : InfiniteExecution P ρ c) : Prop :=
  ∀ bound, ∃ n, bound < (X.states n).events.length

/-- The projected C6 history has a finite bound. Events erased by the adapter do not count. -/
def FiniteEvents {S : Observe.EventSignature} {P : Program O} {ρ : QueryOracle O} {c : Config O}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) : Prop :=
  ∃ bound, ∀ n, (A.project (X.states n).events).length ≤ bound

/-- The projected C6 history outgrows every finite bound. -/
def InfiniteEvents {S : Observe.EventSignature} {P : Program O} {ρ : QueryOracle O} {c : Config O}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) : Prop :=
  ∀ bound, ∃ n, bound < (A.project (X.states n).events).length

/-- Internal silent divergence, classified by the raw C1 history rather than C6 observations. -/
def InternalSilentDivergence (P : Program O) (ρ : QueryOracle O) (c : Config O) : Prop :=
  ∃ X : InfiniteExecution P ρ c, InternalFiniteEvents X

/-- Internal infinite interaction, explicitly retaining the raw-event classification. -/
def InternalInfiniteInteraction (P : Program O) (ρ : QueryOracle O) (c : Config O) : Prop :=
  ∃ X : InfiniteExecution P ρ c, InternalInfiniteEvents X

/-- External silent divergence: infinitely many steps but only finitely many projected C6 events. -/
def SilentDivergence {S : Observe.EventSignature} (P : Program O) (A : Observe.ExecAdapter O S)
    (ρ : QueryOracle O) (c : Config O) : Prop :=
  ∃ X : InfiniteExecution P ρ c, FiniteEvents A X

/-- External infinite interaction: an infinite execution with infinitely many projected C6 events. -/
def InfiniteInteraction {S : Observe.EventSignature} (P : Program O)
    (A : Observe.ExecAdapter O S) (ρ : QueryOracle O) (c : Config O) : Prop :=
  ∃ X : InfiniteExecution P ρ c, InfiniteEvents A X

/-- A C6 step sequence has exactly the same event history as the adapter-projected C1 positions.
Both directions are required: C6 may stutter arbitrarily, but may neither invent nor lose an event. -/
def MatchesProjectedHistory {S : Observe.EventSignature} {P : Program O} {ρ : QueryOracle O}
    {c : Config O} (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c)
    (steps : Nat → Observe.Step S) : Prop :=
  (∀ n, ∃ m rest, Observe.visiblePrefix steps m = A.project (X.states n).events ++ rest) ∧
    (∀ m, ∃ n rest, A.project (X.states n).events = Observe.visiblePrefix steps m ++ rest)

/-- Once `fuel` reaches a blocked request, every larger budget retains C1's `noAnswer` ending.
This is C8's blocking classification over C1; it is not divergence and has no infinite `Move`. -/
def EndlesslyBlocked (P : Program O) (ρ : Oracle O) (fuel : Nat) (c blocked : Config O) : Prop :=
  ∀ n, fuel ≤ n → (run P ρ n c).ending = .noAnswer blocked

/-- A declared call domain, fixed independently of any proof or certificate. Empty domains are
allowed for conditional theorems, but cannot support usable termination evidence. -/
structure CallDomain (O : Operands) where
  function : FnId
  calls : Oracle O → List O.Val → O.Store → Prop

/-- A conditional termination theorem covers the entire declared domain. Trusted premises may be
uninhabited; such a theorem alone is deliberately not usable termination evidence. -/
structure ConditionalTermination (O : Operands) (P : Program O) (domain : CallDomain O) where
  Premises : Type
  /-- Syntactic callee metadata; semantic assumptions belong to `Premises`. -/
  dependencies : List FnId
  fuel : Premises → Oracle O → List O.Val → O.Store → Nat
  terminates : ∀ premises ρ args st, domain.calls ρ args st →
    ∃ fn c f, AList.lookup P.fns domain.function = some fn ∧ enter fn args st = some c ∧
      (run P ρ (fuel premises ρ args st) c).ending = .final f

/-- Usable evidence discharges the conditional theorem's trusted premises and supplies an actual
call in its independently declared domain. There is no certificate-local admission filter: every
declared call is covered, so neither an empty premise type nor a false filter can disable coverage.
Lookup and entry success remain consequences, rather than restrictions on the call domain. -/
structure TerminationEvidence (O : Operands) (P : Program O) (domain : CallDomain O) where
  conditional : ConditionalTermination O P domain
  premises : conditional.Premises
  admitted : ∃ ρ args st, domain.calls ρ args st

end ConcreteSpec.Core.Exec
