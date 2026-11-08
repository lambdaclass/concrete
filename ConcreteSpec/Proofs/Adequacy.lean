import ConcreteSpec.Spec.Adequacy
import ConcreteSpec.Proofs.Exec
import ConcreteSpec.Proofs.Observe2

/-!
# Evaluator adequacy and termination (`Spec/Adequacy.lean`, foundation contract C8)

- Query instrumentation is observationally inert (`runFromQ_toReport`), occurrences are numbered per
  site, and adding fuel only appends queries (`runFromQ_queries_prefix_le`).
- Reports have matching finite executions with exactly their issued queries (`runFromQ_sound`),
  final finite executions are obtained with every sufficient fuel budget (`runFrom_complete_le`),
  completed reports are fuel-monotone (`runFrom_mono`), and event traces are prefix-monotone
  (`runFrom_trace_prefix_le`).
- Infinite maximal executions and compatible fuel limits are definitionally round-trip equivalent
  and unique for each initial run (`InfiniteExecution.unique`). External silent divergence and
  infinite interaction use adapter-projected histories, are mutually exclusive, and agree with C6
  (`finite_projected_observation`, `infinite_projected_observation`). `Demo.event_infinite` shows
  that raw interaction can erase to silent divergence. A blocked request keeps its `noAnswer`
  classification and excludes infinite execution
  (`endlesslyBlocked_of_noAnswer`, `not_infinite_of_noAnswer`).
- Per-function termination evidence quantifies over every entry run admitted by its named premises
  and excludes infinite execution (`TerminationEvidence.excludes_infinite`).
-/

namespace ConcreteSpec.Core.Exec
open ConcreteSpec.Core

variable {O : Operands} {P : Program O} {ρ : QueryOracle O} {c : Config O}

/-- Helper for constructing a query; this definitional equality is not execution-level contract
evidence. -/
theorem issue_occurrence (events : List O.Event) (p : O.Prim) (args : List O.Val) (site : SiteId)
    (st : O.Store) (issued : List (Query O)) :
    (Query.issue events p args site st issued).occurrence = occurrenceAt issued site := rfl

/-- Helper for reasoning about a history append; this is not execution-level contract evidence. -/
theorem issue_next_occurrence (issued : List (Query O)) (site : SiteId) (q : Query O) :
    occurrenceAt (issued ++ [q]) site =
      occurrenceAt issued site + if q.site = site then 1 else 0 := by
  simp [occurrenceAt, List.filter_append]
  split <;> simp_all

/-- Helper for rewriting an oracle answer under query equality; this congruence is not
execution-level contract evidence. -/
theorem answer_deterministic (ρ : QueryOracle O) {q₁ q₂ : Query O} (h : q₁ = q₂) :
    answer ρ q₁ = answer ρ q₂ := congrArg (answer ρ) h

/-- Helper for unfolding the adapter; this definitional equality is not execution-level contract
evidence. -/
theorem answer_toQueryOracle (ρ₀ : Oracle O) (q : Query O) :
    answer ρ₀.toQueryOracle q = ρ₀ q.events q.prim q.args q.site q.store := rfl

/-- Appending the authenticated next query preserves the per-site occurrence invariant. -/
theorem QueryHistoryValid.append {issued : List (Query O)} {q : Query O}
    (valid : QueryHistoryValid issued) (occurrence : q.occurrence = occurrenceAt issued q.site) :
    QueryHistoryValid (issued ++ [q]) := by
  intro i q' hq'
  by_cases hi : i < issued.length
  · rw [List.getElem?_append_left hi] at hq'
    rw [valid i q' hq', List.take_append_of_le_length (Nat.le_of_lt hi)]
  · have hge : issued.length ≤ i := Nat.le_of_not_gt hi
    have hlt : i < (issued ++ [q]).length := by
      rw [List.getElem?_eq_some_iff] at hq'
      exact hq'.1
    have heq : i = issued.length := by simp at hlt; omega
    subst i
    rw [List.getElem?_append_right (Nat.le_refl _)] at hq'
    simp at hq'
    subst q'
    simpa [List.take_append, List.take_of_length_le] using occurrence

/-- **Execution-level occurrence invariant.** Every query reported by an actual instrumented run
counts exactly the earlier queries at its own site, including terminal unanswered queries. -/
theorem runFromQ_queryHistoryValid : ∀ (fuel : Nat) (events : List O.Event)
    (issued : List (Query O)) (c : Config O), QueryHistoryValid issued →
      QueryHistoryValid (runFromQ P ρ fuel events issued c).queries := by
  intro fuel
  induction fuel with
  | zero =>
    intro events issued c valid
    cases hf : c.final? <;> simpa [runFromQ, hf] using valid
  | succ n ih =>
    intro events issued c valid
    cases hf : c.final? with
    | some f => simpa [runFromQ, hf] using valid
    | none =>
      cases hs : step P c with
      | none => simpa [runFromQ, hf, hs] using valid
      | some out =>
        cases out with
        | next c' => simpa [runFromQ, hf, hs] using ih events issued c' valid
        | request p vs site st k =>
          let q := Query.issue events p vs site st issued
          have nextValid : QueryHistoryValid (issued ++ [q]) := valid.append rfl
          cases ha : answer ρ q with
          | none => simpa [runFromQ, hf, hs, q, ha] using nextValid
          | some a =>
            cases hallow : O.allows p vs st a with
            | false => simpa [runFromQ, hf, hs, q, ha, hallow] using nextValid
            | true =>
              cases hk : k a with
              | none => simpa [runFromQ, hf, hs, q, ha, hallow, hk] using nextValid
              | some c' =>
                simpa [runFromQ, hf, hs, q, ha, hallow, hk] using
                  ih (events ++ a.event.toList) (issued ++ [q]) c' nextValid

/-- Helper (computation). The empty query history has no entries to check (approved: PENDING). -/
theorem QueryHistoryValid.nil : QueryHistoryValid ([] : List (Query O)) := by
  intro i q h
  simp at h

/-- Helper (computation). Specializes the evaluator invariant to its public entry point
(approved: PENDING). -/
theorem runQ_queryHistoryValid (fuel : Nat) (c : Config O) :
    QueryHistoryValid (runQ P ρ fuel c).queries :=
  runFromQ_queryHistoryValid fuel [] [] c QueryHistoryValid.nil

/-- Helper (arithmetic). The count between two prefix boundaries is the sum of the
per-site contributions, including silent requests (approved: PENDING). -/
theorem occurrenceAt_take_interval (qs : List (Query O)) (site : SiteId)
    {i j : Nat} (hij : i ≤ j) :
    occurrenceAt (qs.take j) site = occurrenceAt (qs.take i) site +
      occurrenceAt ((qs.drop i).take (j - i)) site := by
  have split : qs.take j = qs.take i ++ (qs.drop i).take (j - i) := by
    rw [← List.take_add, Nat.add_sub_cancel' hij]
  rw [split]
  simp [occurrenceAt, List.filter_append]

/-- Helper (computation). Execution occurrence accounting over an arbitrary interval; unlike
adjacent-site peeling this relates both occurrences at the same site (approved: PENDING). -/
theorem runFromQ_interleaved_site (fuel : Nat) (events : List O.Event)
    (issued : List (Query O)) (c : Config O) (valid : QueryHistoryValid issued)
    {i j : Nat} {q q' : Query O} (hij : i ≤ j)
    (hq : (runFromQ P ρ fuel events issued c).queries[i]? = some q)
    (hq' : (runFromQ P ρ fuel events issued c).queries[j]? = some q')
    (site : q.site = q'.site) :
    q'.occurrence = q.occurrence + occurrenceAt
      (((runFromQ P ρ fuel events issued c).queries.drop i).take (j - i)) q.site := by
  have validRun := runFromQ_queryHistoryValid (P := P) (ρ := ρ) fuel events issued c valid
  rw [validRun j q' hq', ← site, occurrenceAt_take_interval _ _ hij,
    ← validRun i q hq]

/-- **Repeated requests at one site advance their occurrences.** These are consecutive queries in
the actual run history; arbitrarily many silent machine transitions may separate the requests. -/
theorem runFromQ_consecutive_same_site (fuel : Nat) (events : List O.Event)
    (issued : List (Query O)) (c : Config O) (valid : QueryHistoryValid issued)
    {i : Nat} {q q' : Query O}
    (hq : (runFromQ P ρ fuel events issued c).queries[i]? = some q)
    (hq' : (runFromQ P ρ fuel events issued c).queries[i + 1]? = some q')
    (site : q.site = q'.site) : q'.occurrence = q.occurrence + 1 := by
  have history := runFromQ_queryHistoryValid (P := P) (ρ := ρ) fuel events issued c valid
  have before := history i q hq
  have after := history (i + 1) q' hq'
  rw [after, List.take_add_one, hq]
  simp only [Option.toList_some]
  rw [issue_next_occurrence, before, site]
  simp

/-- **Interleaved sites keep separate counters.** A query at another site does not advance the
current query's occurrence count, even when the requests are consecutive in an actual run. -/
theorem runFromQ_consecutive_other_site (fuel : Nat) (events : List O.Event)
    (issued : List (Query O)) (c : Config O) (valid : QueryHistoryValid issued)
    {i : Nat} {q q' : Query O}
    (hq : (runFromQ P ρ fuel events issued c).queries[i]? = some q)
    (hq' : (runFromQ P ρ fuel events issued c).queries[i + 1]? = some q')
    (site : q.site ≠ q'.site) : q'.occurrence = occurrenceAt
      ((runFromQ P ρ fuel events issued c).queries.take i) q'.site := by
  have history := runFromQ_queryHistoryValid (P := P) (ρ := ρ) fuel events issued c valid
  rw [history (i + 1) q' hq', List.take_add_one, hq]
  simp only [Option.toList_some]
  rw [issue_next_occurrence]
  simp [site]

/-- Erasing query instrumentation gives C1's original driver when the query-level oracle is C1's
oracle lifted to ignore occurrence numbers. -/
theorem runFromQ_toReport {ρ₀ : Oracle O} :
    ∀ (fuel : Nat) (events : List O.Event) (issued : List (Query O)) (c : Config O),
    (runFromQ P ρ₀.toQueryOracle fuel events issued c).toReport = runFrom P ρ₀ fuel events c := by
  intro fuel
  induction fuel with
  | zero =>
    intro events issued c
    cases h : c.final? <;> simp [runFromQ, runFrom, h, QueryReport.toReport]
  | succ n ih =>
    intro events issued c
    cases hf : c.final? with
    | some f => simp [runFromQ, runFrom, hf, QueryReport.toReport]
    | none =>
      cases hs : step P c with
      | none => simp [runFromQ, runFrom, hf, hs, QueryReport.toReport]
      | some out =>
        cases out with
        | next c' => simpa [runFromQ, runFrom, hf, hs] using ih events issued c'
        | request p vs site st k =>
          cases ha : answer ρ₀.toQueryOracle (Query.issue events p vs site st issued) with
          | none =>
            simp [answer, Oracle.toQueryOracle, Query.issue] at ha
            simp [runFromQ, runFrom, hf, hs, answer, Oracle.toQueryOracle, Query.issue, ha,
              QueryReport.toReport]
          | some a =>
            simp [answer, Oracle.toQueryOracle, Query.issue] at ha
            cases hallow : O.allows p vs st a with
            | false => simp [runFromQ, runFrom, hf, hs, answer, Oracle.toQueryOracle, Query.issue,
                ha, hallow, QueryReport.toReport]
            | true =>
              cases hk : k a with
              | none => simp [runFromQ, runFrom, hf, hs, answer, Oracle.toQueryOracle, Query.issue,
                  ha, hallow, hk, QueryReport.toReport]
              | some c' =>
                simpa [runFromQ, runFrom, hf, hs, answer, Oracle.toQueryOracle, Query.issue, ha,
                  hallow, hk] using
                  ih (events ++ a.event.toList) (issued ++ [Query.issue events p vs site st issued]) c'

/-- Helper (inversion). Exact primitive transition characterization, including the complete
query history. This describes `Move`, not independent evaluator evidence (approved: PENDING). -/
theorem Move.toQueryOracle_answer {ρ₀ : Oracle O} {pos pos' : Position O}
    {p : O.Prim} {vs : List O.Val} {site : SiteId}
    {st : O.Store} {k : Answer O.Val O.Event O.Store → Option (Config O)}
    (request : step P pos.config = some (.request p vs site st k)) :
    Move P ρ₀.toQueryOracle pos pos' ↔
    ∃ a, ρ₀ pos.events p vs site st = some a ∧ O.allows p vs st a = true ∧
      k a = some pos'.config ∧ pos'.events = pos.events ++ a.event.toList ∧
      pos'.queries = pos.queries ++ [Query.issue pos.events p vs site st pos.queries] := by
  constructor
  · intro move
    cases move with
    | silent silentStep => rw [request] at silentStep; cases silentStep
    | prim primStep ha allows continued =>
      rw [request] at primStep
      cases primStep
      exact ⟨_, ha, allows, continued, rfl, rfl⟩
  · rintro ⟨a, ha, allows, continued, he, hq⟩
    cases pos with
    | mk c events issued =>
      cases pos' with
      | mk c' events' issued' =>
        simp only at he hq
        subst events'; subst issued'
        exact .prim request ha allows continued

/-- Helper (computation). Evaluator-level request/answer correspondence. Only issuance and
nonfinality are premises: the selected answer and all four continuations are computed by the
evaluator, including unanswered, invalid and refused answers (approved: PENDING). -/
theorem runFromQ_request {fuel : Nat} {events : List O.Event} {issued : List (Query O)}
    {c : Config O} {p : O.Prim} {vs : List O.Val} {site : SiteId} {st : O.Store}
    {k : Answer O.Val O.Event O.Store → Option (Config O)}
    (hf : c.final? = none) (hs : step P c = some (.request p vs site st k)) :
    runFromQ P ρ (fuel + 1) events issued c =
      let q := Query.issue events p vs site st issued
      match answer ρ q with
      | none => ⟨events, issued ++ [q], .noAnswer c⟩
      | some a => if O.allows p vs st a then
          match k a with
          | some c' => runFromQ P ρ fuel (events ++ a.event.toList) (issued ++ [q]) c'
          | none => ⟨events, issued ++ [q], .stuck c⟩
        else ⟨events, issued ++ [q], .badAnswer c⟩ := by
  simp only [runFromQ, hf, hs]
  rfl

/-- Law (subject `runFromQ`). An actually issued request is answered at its exact event and query
history. The report obeys that answer, the contract, and the partial continuation, in every branch. -/
theorem runFromQ_request_correspondence {fuel : Nat} {events : List O.Event}
    {issued : List (Query O)} {c : Config O} {p : O.Prim} {vs : List O.Val} {site : SiteId}
    {st : O.Store} {k : Answer O.Val O.Event O.Store → Option (Config O)}
    (hf : c.final? = none) (hs : step P c = some (.request p vs site st k)) :
    RequestResolution P ρ fuel ⟨c, events, issued⟩ p vs site st k
      (runFromQ P ρ (fuel + 1) events issued c) := by
  rw [runFromQ_request hf hs]
  dsimp only
  cases ha : answer ρ (Query.issue events p vs site st issued) with
  | none => exact RequestResolution.unanswered (pos := ⟨c, events, issued⟩) ha
  | some a =>
    cases hl : O.allows p vs st a with
    | false => simpa only [hl, Bool.false_eq_true, ↓reduceIte] using
        (RequestResolution.invalid (pos := ⟨c, events, issued⟩) (k := k) ha hl)
    | true =>
      simp only [hl, ↓reduceIte]
      cases hk : k a with
      | none => exact RequestResolution.refused (pos := ⟨c, events, issued⟩) ha hl hk
      | some c' => exact RequestResolution.continued (pos := ⟨c, events, issued⟩) ha hl hk

/-- Every instrumented run extends the query history with which it started. -/
theorem runFromQ_queries_from : ∀ (fuel : Nat) (events : List O.Event) (issued : List (Query O))
    (c : Config O), ∃ later, (runFromQ P ρ fuel events issued c).queries = issued ++ later := by
  intro fuel
  induction fuel with
  | zero =>
    intro events issued c
    cases h : c.final? <;> exact ⟨[], by simp [runFromQ, h]⟩
  | succ n ih =>
    intro events issued c
    cases hf : c.final? with
    | some f => exact ⟨[], by simp [runFromQ, hf]⟩
    | none =>
      cases hs : step P c with
      | none => exact ⟨[], by simp [runFromQ, hf, hs]⟩
      | some out =>
        cases out with
        | next c' => simpa [runFromQ, hf, hs] using ih events issued c'
        | request p vs site st k =>
          let q := Query.issue events p vs site st issued
          cases ha : answer ρ q with
          | none => exact ⟨[q], by simp [runFromQ, hf, hs, q, ha]⟩
          | some a =>
            cases hallow : O.allows p vs st a with
            | false => exact ⟨[q], by simp [runFromQ, hf, hs, q, ha, hallow]⟩
            | true =>
              cases hk : k a with
              | none => exact ⟨[q], by simp [runFromQ, hf, hs, q, ha, hallow, hk]⟩
              | some c' =>
                obtain ⟨later, h⟩ := ih (events ++ a.event.toList) (issued ++ [q]) c'
                exact ⟨q :: later, by simp [runFromQ, hf, hs, q, ha, hallow, hk, h,
                  List.append_assoc]⟩

/-- One more unit of fuel preserves every query already issued. -/
theorem runFromQ_queries_prefix : ∀ (fuel : Nat) (events : List O.Event) (issued : List (Query O))
    (c : Config O),
    (runFromQ P ρ fuel events issued c).queries <+:
      (runFromQ P ρ (fuel + 1) events issued c).queries := by
  intro fuel
  induction fuel with
  | zero =>
    intro events issued c
    obtain ⟨later, h⟩ := runFromQ_queries_from (P := P) (ρ := ρ) 1 events issued c
    refine ⟨later, ?_⟩
    have h0 : (runFromQ P ρ 0 events issued c).queries = issued := by
      cases hf : c.final? <;> simp [runFromQ, hf]
    rw [h0]
    exact h.symm
  | succ n ih =>
    intro events issued c
    cases hf : c.final? with
    | some f => simp [runFromQ, hf]
    | none =>
      cases hs : step P c with
      | none => simp [runFromQ, hf, hs]
      | some out =>
        cases out with
        | next c' => simpa [runFromQ, hf, hs, Nat.succ_eq_add_one] using ih events issued c'
        | request p vs site st k =>
          let q := Query.issue events p vs site st issued
          cases ha : answer ρ q with
          | none => simp [runFromQ, hf, hs, q, ha]
          | some a =>
            cases hallow : O.allows p vs st a with
            | false => simp [runFromQ, hf, hs, q, ha, hallow]
            | true =>
              cases hk : k a with
              | none => simp [runFromQ, hf, hs, q, ha, hallow, hk]
              | some c' =>
                simpa [runFromQ, hf, hs, q, ha, hallow, hk, Nat.succ_eq_add_one] using
                  ih (events ++ a.event.toList) (issued ++ [q]) c'

/-- Query histories are prefix-monotone for every increase in fuel. -/
theorem runFromQ_queries_prefix_le {n m : Nat} (hle : n ≤ m) (events : List O.Event)
    (issued : List (Query O)) (c : Config O) :
    (runFromQ P ρ n events issued c).queries <+: (runFromQ P ρ m events issued c).queries := by
  obtain ⟨k, rfl⟩ := Nat.exists_eq_add_of_le hle
  clear hle
  induction k with
  | zero => exact ⟨[], by simp⟩
  | succ k ih =>
    obtain ⟨rest₁, h₁⟩ := ih
    obtain ⟨rest₂, h₂⟩ := runFromQ_queries_prefix (P := P) (ρ := ρ) (n + k) events issued c
    refine ⟨rest₁ ++ rest₂, ?_⟩
    rw [← List.append_assoc, h₁]
    exact h₂

/-- The first queries of a run are independent of its fuel: if two budgets both issue query `i`,
they issue the same query there. -/
theorem runFromQ_query_fuel_independent {n m : Nat} {events : List O.Event}
    {issued : List (Query O)} {c : Config O} {i : Nat} {qn qm : Query O}
    (hn : (runFromQ P ρ n events issued c).queries[i]? = some qn)
    (hm : (runFromQ P ρ m events issued c).queries[i]? = some qm)
    (hle : n ≤ m) : qn = qm := by
  obtain ⟨rest, hrest⟩ := runFromQ_queries_prefix_le (P := P) (ρ := ρ) hle events issued c
  have hlt : i < (runFromQ P ρ n events issued c).queries.length := by
    rw [List.getElem?_eq_some_iff] at hn
    exact hn.1
  have hget : (runFromQ P ρ m events issued c).queries[i]? =
      (runFromQ P ρ n events issued c).queries[i]? := by
    rw [← hrest, List.getElem?_append_left hlt]
  rw [hn, hm] at hget
  exact Option.some.inj hget.symm

/-! ### Finite executions and evaluator adequacy -/

/-- One oracle-checked move is one ordinary labeled step, with the same event contribution. -/
theorem Move.toStep {pos pos' : Position O} (h : Move P ρ pos pos') :
    ∃ l, Step P pos.config l pos'.config ∧ pos'.events = pos.events ++ l.events ∧
      ∃ later, pos'.queries = pos.queries ++ later := by
  cases h with
  | silent hs => exact ⟨.silent, .silent hs, by simp [Label.events], [], by simp⟩
  | @prim c c' events issued p vs site st k a hs ha hallow hk =>
    refine ⟨a.label, .prim hs hallow hk, ?_, [Query.issue events p vs site st issued], rfl⟩
    cases he : a.event <;> simp [Answer.label, Label.events, he]

/-- An oracle-checked finite path erases to `Steps`; its final histories extend its initial ones. -/
theorem Moves.toSteps {n : Nat} {pos pos' : Position O} (h : Moves P ρ n pos pos') :
    ∃ tr later, pos'.events = pos.events ++ tr ∧ pos'.queries = pos.queries ++ later ∧
      Steps P pos.config tr pos'.config := by
  induction h with
  | refl pos => exact ⟨[], [], by simp, by simp, .refl _⟩
  | step hm hms ih =>
    obtain ⟨l, hs, he, qs, hq⟩ := hm.toStep
    obtain ⟨tr, later, he', hq', hsteps⟩ := ih
    refine ⟨l.events ++ tr, qs ++ later, ?_, ?_, Steps.trans (Steps.single hs) hsteps⟩
    · rw [he', he, List.append_assoc]
    · rw [hq', hq, List.append_assoc]

/-- A configuration with an available machine transition is not final. -/
theorem final?_none_of_step {c : Config O} {out : Out O} (h : step P c = some out) :
    c.final? = none := by
  cases c with
  | mk ctrl locals store frames =>
    cases ctrl with
    | exec s => rfl
    | done =>
      cases frames with
      | nil => simp [step] at h
      | cons f k => rfl
    | exiting e =>
      cases e with
      | brk l => rfl
      | cont l => rfl
      | ret v =>
        cases frames with
        | nil => simp [step, stepExit] at h
        | cons f k => rfl
    | trapped kd site => simp [step] at h

/-- **Soundness.** Every instrumented report has a finite oracle-checked path to a position that
justifies its ending. `ReportedQueries` is indexed by that justification and identifies the
reported history exactly: the final position's history, plus the authenticated terminal query for
a request-stopping ending. -/
theorem runFromQ_sound : ∀ (fuel : Nat) (events : List O.Event) (issued : List (Query O))
    (c : Config O), ∃ n pos, Moves P ρ n ⟨c, events, issued⟩ pos ∧
      (runFromQ P ρ fuel events issued c).events = pos.events ∧
      ∃ hc : Conclusion P ρ (runFromQ P ρ fuel events issued c).ending pos,
        ReportedQueries P ρ hc (runFromQ P ρ fuel events issued c).queries := by
  intro fuel
  induction fuel with
  | zero =>
    intro events issued c
    cases hf : c.final? with
    | some f =>
      refine ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf], ?_⟩
      simpa [runFromQ, hf] using
        (show ∃ hc : Conclusion P ρ (.final f) (⟨c, events, issued⟩ : Position O),
            ReportedQueries P ρ hc issued from
          ⟨Conclusion.final hf, ReportedQueries.final hf⟩)
    | none =>
      refine ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf], ?_⟩
      simpa [runFromQ, hf] using
        (show ∃ hc : Conclusion P ρ (.outOfFuel c) (⟨c, events, issued⟩ : Position O),
            ReportedQueries P ρ hc issued from
          ⟨Conclusion.outOfFuel hf, ReportedQueries.outOfFuel (events := events) hf⟩)
  | succ n ih =>
    intro events issued c
    cases hf : c.final? with
    | some f =>
      refine ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf], ?_⟩
      simpa [runFromQ, hf] using
        (show ∃ hc : Conclusion P ρ (.final f) (⟨c, events, issued⟩ : Position O),
            ReportedQueries P ρ hc issued from
          ⟨Conclusion.final hf, ReportedQueries.final hf⟩)
    | none =>
      cases hs : step P c with
      | none =>
        refine ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf, hs], ?_⟩
        simpa [runFromQ, hf, hs] using
          (show ∃ hc : Conclusion P ρ (.stuck c) (⟨c, events, issued⟩ : Position O),
              ReportedQueries P ρ hc issued from
            ⟨Conclusion.stuck hs, ReportedQueries.stuck (events := events) hs⟩)
      | some out =>
        cases out with
        | next c' =>
          obtain ⟨m, pos, hpath, he, hc, hr⟩ := ih events issued c'
          exact ⟨m + 1, pos, .step (.silent hs) hpath, by simpa [runFromQ, hf, hs] using he,
            by simpa [runFromQ, hf, hs] using hc, by simpa [runFromQ, hf, hs] using hr⟩
        | request p vs site st k =>
          let q := Query.issue events p vs site st issued
          cases ha : answer ρ q with
          | none =>
            refine ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf, hs, q, ha], ?_⟩
            simpa [runFromQ, hf, hs, q, ha] using
              (show ∃ hc : Conclusion P ρ (.noAnswer c) (⟨c, events, issued⟩ : Position O),
                  ReportedQueries P ρ hc (issued ++ [q]) from
                ⟨Conclusion.noAnswer hs ha, ReportedQueries.noAnswer hs ha⟩)
          | some a =>
            cases hallow : O.allows p vs st a with
            | false =>
              refine ⟨0, ⟨c, events, issued⟩, .refl _,
                by simp [runFromQ, hf, hs, q, ha, hallow], ?_⟩
              simpa [runFromQ, hf, hs, q, ha, hallow] using
                (show ∃ hc : Conclusion P ρ (.badAnswer c) (⟨c, events, issued⟩ : Position O),
                    ReportedQueries P ρ hc (issued ++ [q]) from
                  ⟨Conclusion.badAnswer hs ha hallow, ReportedQueries.badAnswer hs ha hallow⟩)
            | true =>
              cases hk : k a with
              | none =>
                refine ⟨0, ⟨c, events, issued⟩, .refl _,
                  by simp [runFromQ, hf, hs, q, ha, hallow, hk], ?_⟩
                simpa [runFromQ, hf, hs, q, ha, hallow, hk] using
                  (show ∃ hc : Conclusion P ρ (.stuck c) (⟨c, events, issued⟩ : Position O),
                      ReportedQueries P ρ hc (issued ++ [q]) from
                    ⟨Conclusion.stuckContinuation hs ha hallow hk,
                      ReportedQueries.stuckContinuation hs ha hallow hk⟩)
              | some c' =>
                obtain ⟨m, pos, hpath, he, hc, hr⟩ :=
                  ih (events ++ a.event.toList) (issued ++ [q]) c'
                exact ⟨m + 1, pos, .step (.prim hs ha hallow hk) hpath,
                  by simpa [runFromQ, hf, hs, q, ha, hallow, hk] using he,
                  by simpa [runFromQ, hf, hs, q, ha, hallow, hk] using hc,
                  by simpa [runFromQ, hf, hs, q, ha, hallow, hk] using hr⟩

/-- **Soundness for C1's `runFrom`.** Query erasure preserves the finite execution and its ending
justification. `Moves.toSteps` turns the same witness into the ordinary `Steps` relation. -/
theorem runFrom_sound {ρ₀ : Oracle O} (fuel : Nat) (events : List O.Event) (c : Config O) :
    ∃ n pos, Moves P ρ₀.toQueryOracle n ⟨c, events, []⟩ pos ∧
      (runFrom P ρ₀ fuel events c).events = pos.events ∧
      Conclusion P ρ₀.toQueryOracle (runFrom P ρ₀ fuel events c).ending pos := by
  obtain ⟨n, pos, hpath, he, hc, hr⟩ :=
    runFromQ_sound (P := P) (ρ := ρ₀.toQueryOracle) fuel events [] c
  have hreport := runFromQ_toReport (P := P) (ρ₀ := ρ₀) fuel events [] c
  have he' : (runFrom P ρ₀ fuel events c).events =
      (runFromQ P ρ₀.toQueryOracle fuel events [] c).events := by
    simpa [QueryReport.toReport] using congrArg Report.events hreport.symm
  have hend : (runFrom P ρ₀ fuel events c).ending =
      (runFromQ P ρ₀.toQueryOracle fuel events [] c).ending := by
    simpa [QueryReport.toReport] using congrArg Report.ending hreport.symm
  refine ⟨n, pos, hpath, he'.trans he, ?_⟩
  rw [hend]
  exact hc

/-- Every report extends the event history supplied to `runFrom`. -/
theorem runFrom_events_from {ρ₀ : Oracle O} (fuel : Nat) (events : List O.Event) (c : Config O) :
    ∃ tr, (runFrom P ρ₀ fuel events c).events = events ++ tr := by
  obtain ⟨n, pos, hpath, he, hc⟩ := runFrom_sound (P := P) (ρ₀ := ρ₀) fuel events c
  obtain ⟨tr, later, hepos, hq, hsteps⟩ := Moves.toSteps hpath
  refine ⟨tr, ?_⟩
  rw [he, hepos]

/-- **Completeness.** Exactly `n` units of fuel suffice for an `n`-step oracle-checked execution to
a final configuration; the report carries the path's complete event and query histories. -/
theorem runFromQ_complete {n : Nat} {pos pos' : Position O} (h : Moves P ρ n pos pos')
    {f : Final O.Val} (hf : pos'.config.final? = some f) :
    runFromQ P ρ n pos.events pos.queries pos.config =
      ⟨pos'.events, pos'.queries, .final f⟩ := by
  induction h with
  | refl pos => simp [runFromQ, hf]
  | step hm hms ih =>
    cases hm with
    | silent hs =>
      have hn := final?_none_of_step hs
      simpa [runFromQ, hn, hs] using ih hf
    | @prim c c' events issued p vs site st k a hs ha hallow hk =>
      have hn := final?_none_of_step hs
      simpa [runFromQ, hn, hs, ha, hallow, hk] using ih hf

/-- **Completeness for C1's `runFrom`.** Every finite execution from an empty query history to a
final configuration is reported with exactly its number of steps as fuel. -/
theorem runFrom_complete {ρ₀ : Oracle O} {n : Nat} {events : List O.Event} {c : Config O}
    {pos' : Position O} (h : Moves P ρ₀.toQueryOracle n ⟨c, events, []⟩ pos')
    {f : Final O.Val} (hf : pos'.config.final? = some f) :
    runFrom P ρ₀ n events c = ⟨pos'.events, .final f⟩ := by
  have hq := runFromQ_complete (P := P) (ρ := ρ₀.toQueryOracle) h hf
  have hr := runFromQ_toReport (P := P) (ρ₀ := ρ₀) n events [] c
  rw [hq] at hr
  simpa [QueryReport.toReport] using hr.symm

/-- **Monotonicity.** Extra fuel preserves every report other than `outOfFuel`. -/
theorem runFrom_mono {ρ₀ : Oracle O} : ∀ (n m : Nat), n ≤ m →
    ∀ (events : List O.Event) (c : Config O),
    (∀ c', (runFrom P ρ₀ n events c).ending ≠ .outOfFuel c') →
    runFrom P ρ₀ m events c = runFrom P ρ₀ n events c := by
  intro n
  induction n with
  | zero =>
    intro m hle events c hnot
    cases hf : c.final? with
    | some f => cases m <;> simp [runFrom, hf]
    | none => exact (hnot c (by simp [runFrom, hf])).elim
  | succ n ih =>
    intro m hle events c hnot
    cases m with
    | zero => omega
    | succ m =>
      have hle' : n ≤ m := Nat.succ_le_succ_iff.mp hle
      cases hf : c.final? with
      | some f => simp [runFrom, hf]
      | none =>
        cases hs : step P c with
        | none => simp [runFrom, hf, hs]
        | some out =>
          cases out with
          | next c' =>
            have hnot' : ∀ c₀, (runFrom P ρ₀ n events c').ending ≠ .outOfFuel c₀ := by
              simpa [runFrom, hf, hs] using hnot
            simpa [runFrom, hf, hs] using ih m hle' events c' hnot'
          | request p vs site st k =>
            cases ha : ρ₀ events p vs site st with
            | none => simp [runFrom, hf, hs, ha]
            | some a =>
              cases hallow : O.allows p vs st a with
              | false => simp [runFrom, hf, hs, ha, hallow]
              | true =>
                cases hk : k a with
                | none => simp [runFrom, hf, hs, ha, hallow, hk]
                | some c' =>
                  have hnot' : ∀ c₀,
                      (runFrom P ρ₀ n (events ++ a.event.toList) c').ending ≠ .outOfFuel c₀ := by
                    simpa [runFrom, hf, hs, ha, hallow, hk] using hnot
                  simpa [runFrom, hf, hs, ha, hallow, hk] using
                    ih m hle' (events ++ a.event.toList) c' hnot'

/-- **Completeness with enough fuel.** Every budget at least the finite path length reports that
path's final outcome and events. -/
theorem runFrom_complete_le {ρ₀ : Oracle O} {n m : Nat} (hle : n ≤ m)
    {events : List O.Event} {c : Config O} {pos' : Position O}
    (h : Moves P ρ₀.toQueryOracle n ⟨c, events, []⟩ pos')
    {f : Final O.Val} (hf : pos'.config.final? = some f) :
    runFrom P ρ₀ m events c = ⟨pos'.events, .final f⟩ := by
  have hexact := runFrom_complete (P := P) (ρ₀ := ρ₀) h hf
  have hnot : ∀ c', (runFrom P ρ₀ n events c).ending ≠ .outOfFuel c' := by
    rw [hexact]
    simp
  rw [runFrom_mono (P := P) (ρ₀ := ρ₀) n m hle events c hnot]
  exact hexact

/-- **Trace-prefix consistency.** Increasing fuel never retracts or changes an event already
reported. -/
theorem runFrom_trace_prefix_le {ρ₀ : Oracle O} : ∀ (n m : Nat), n ≤ m →
    ∀ (events : List O.Event) (c : Config O),
    (runFrom P ρ₀ n events c).events <+: (runFrom P ρ₀ m events c).events := by
  intro n
  induction n with
  | zero =>
    intro m hle events c
    obtain ⟨tr, h⟩ := runFrom_events_from (P := P) (ρ₀ := ρ₀) m events c
    refine ⟨tr, ?_⟩
    have h0 : (runFrom P ρ₀ 0 events c).events = events := by
      cases hf : c.final? <;> simp [runFrom, hf]
    rw [h0]
    exact h.symm
  | succ n ih =>
    intro m hle events c
    cases m with
    | zero => omega
    | succ m =>
      have hle' : n ≤ m := Nat.succ_le_succ_iff.mp hle
      cases hf : c.final? with
      | some f => simp [runFrom, hf]
      | none =>
        cases hs : step P c with
        | none => simp [runFrom, hf, hs]
        | some out =>
          cases out with
          | next c' => simpa [runFrom, hf, hs] using ih m hle' events c'
          | request p vs site st k =>
            cases ha : ρ₀ events p vs site st with
            | none => simp [runFrom, hf, hs, ha]
            | some a =>
              cases hallow : O.allows p vs st a with
              | false => simp [runFrom, hf, hs, ha, hallow]
              | true =>
                cases hk : k a with
                | none => simp [runFrom, hf, hs, ha, hallow, hk]
                | some c' => simpa [runFrom, hf, hs, ha, hallow, hk] using
                  (ih m hle' (events ++ a.event.toList) c')

/-- The events of fuel `n` are a prefix of those of fuel `n + 1`. -/
theorem runFrom_trace_prefix {ρ₀ : Oracle O} (n : Nat) (events : List O.Event) (c : Config O) :
    (runFrom P ρ₀ n events c).events <+: (runFrom P ρ₀ (n + 1) events c).events :=
  runFrom_trace_prefix_le n (n + 1) (Nat.le_add_right n 1) events c

/-- A final report has an ordinary finite `Steps` witness ending in exactly that final
configuration, with the reported events. -/
theorem runFrom_final_sound {ρ₀ : Oracle O} {fuel : Nat} {events : List O.Event} {c : Config O}
    {f : Final O.Val} (h : (runFrom P ρ₀ fuel events c).ending = .final f) :
    ∃ tr c', Steps P c tr c' ∧ c'.final? = some f ∧
      (runFrom P ρ₀ fuel events c).events = events ++ tr := by
  obtain ⟨n, pos, hpath, he, hc⟩ := runFrom_sound (P := P) (ρ₀ := ρ₀) fuel events c
  rw [h] at hc
  cases hc with
  | final hf =>
    obtain ⟨tr, later, hepos, hq, hsteps⟩ := Moves.toSteps hpath
    refine ⟨tr, pos.config, hsteps, hf, ?_⟩
    rw [he, hepos]

/-! ### Infinite executions are compatible fuel limits -/

/-- If `n` steps exhaust the budget at `pos`, then `n + m` steps are exactly `m` steps from `pos`.
This is the compatibility operation used by the infinite-limit theorem. -/
theorem runFromQ_add_of_outOfFuel {n : Nat} {events : List O.Event} {issued : List (Query O)}
    {c : Config O} {pos : Position O}
    (h : runFromQ P ρ n events issued c =
      ⟨pos.events, pos.queries, .outOfFuel pos.config⟩) : ∀ m,
    runFromQ P ρ (n + m) events issued c = runFromQ P ρ m pos.events pos.queries pos.config := by
  induction n generalizing events issued c pos with
  | zero =>
    intro m
    cases hf : c.final? with
    | some f => simp [runFromQ, hf] at h
    | none =>
      cases pos with
      | mk pc pe pq =>
        simp [runFromQ, hf] at h
        obtain ⟨rfl, rfl, rfl⟩ := h
        simp only [Nat.zero_add]
  | succ n ih =>
    intro m
    cases hf : c.final? with
    | some f => simp [runFromQ, hf] at h
    | none =>
      cases hs : step P c with
      | none => simp [runFromQ, hf, hs] at h
      | some out =>
        cases out with
        | next c' =>
          have h' : runFromQ P ρ n events issued c' =
              ⟨pos.events, pos.queries, .outOfFuel pos.config⟩ := by
            simpa [runFromQ, hf, hs] using h
          simpa [runFromQ, hf, hs, Nat.succ_add] using ih h' m
        | request p vs site st k =>
          cases ha : answer ρ (Query.issue events p vs site st issued) with
          | none => simp [runFromQ, hf, hs, ha] at h
          | some a =>
            cases hallow : O.allows p vs st a with
            | false => simp [runFromQ, hf, hs, ha, hallow] at h
            | true =>
              cases hk : k a with
              | none => simp [runFromQ, hf, hs, ha, hallow, hk] at h
              | some c' =>
                have h' : runFromQ P ρ n (events ++ a.event.toList)
                    (issued ++ [Query.issue events p vs site st issued]) c' =
                    ⟨pos.events, pos.queries, .outOfFuel pos.config⟩ := by
                  simpa [runFromQ, hf, hs, ha, hallow, hk] using h
                simpa [runFromQ, hf, hs, ha, hallow, hk, Nat.succ_add] using ih h' m

/-- A zero-step `outOfFuel` report identifies its position exactly. -/
theorem eq_of_runFromQ_zero_out {events : List O.Event} {issued : List (Query O)} {c : Config O}
    {pos : Position O}
    (h : runFromQ P ρ 0 events issued c = ⟨pos.events, pos.queries, .outOfFuel pos.config⟩) :
    pos = ⟨c, events, issued⟩ := by
  cases pos with
  | mk pc pe pq =>
    cases hf : c.final? with
    | some f => simp [runFromQ, hf] at h
    | none =>
      simp [runFromQ, hf] at h
      obtain ⟨rfl, rfl, rfl⟩ := h
      rfl

/-- A move between two nonfinal positions determines the one-unit report from its source. -/
theorem Move.runFromQ_one {pos pos' : Position O} (hm : Move P ρ pos pos')
    (hf : pos.config.final? = none) (hf' : pos'.config.final? = none) :
    runFromQ P ρ 1 pos.events pos.queries pos.config =
      ⟨pos'.events, pos'.queries, .outOfFuel pos'.config⟩ := by
  cases hm with
  | silent hs => simp [runFromQ, hf, hs, hf']
  | prim hs ha hallow hk => simp [runFromQ, hf, hs, ha, hallow, hk, hf']

/-- A move's source is not final. -/
theorem Move.not_final {pos pos' : Position O} (hm : Move P ρ pos pos') :
    pos.config.final? = none := by
  cases hm with
  | silent hs => exact final?_none_of_step hs
  | prim hs _ _ _ => exact final?_none_of_step hs

/-- Two consecutive fuel-prefix reports determine exactly one transition between their positions. -/
theorem move_of_fuel_succ {n : Nat} {events : List O.Event} {issued : List (Query O)}
    {c : Config O} {pos pos' : Position O}
    (h0 : runFromQ P ρ n events issued c = ⟨pos.events, pos.queries, .outOfFuel pos.config⟩)
    (h1 : runFromQ P ρ (n + 1) events issued c =
      ⟨pos'.events, pos'.queries, .outOfFuel pos'.config⟩) : Move P ρ pos pos' := by
  have hadd := runFromQ_add_of_outOfFuel (P := P) (ρ := ρ) h0 1
  have h : runFromQ P ρ 1 pos.events pos.queries pos.config =
      ⟨pos'.events, pos'.queries, .outOfFuel pos'.config⟩ := hadd.symm.trans h1
  cases pos with
  | mk c events issued =>
    cases hf : c.final? with
    | some f => simp [runFromQ, hf] at h
    | none =>
      cases hs : step P c with
      | none => simp [runFromQ, hf, hs] at h
      | some out =>
        cases out with
        | next c' =>
          simp only [runFromQ, hf, hs] at h
          have hp := eq_of_runFromQ_zero_out (P := P) (ρ := ρ) h
          subst pos'
          exact .silent hs
        | request p vs site st k =>
          cases ha : answer ρ (Query.issue events p vs site st issued) with
          | none => simp [runFromQ, hf, hs, ha] at h
          | some a =>
            cases hallow : O.allows p vs st a with
            | false => simp [runFromQ, hf, hs, ha, hallow] at h
            | true =>
              cases hk : k a with
              | none => simp [runFromQ, hf, hs, ha, hallow, hk] at h
              | some c' =>
                simp only [runFromQ, hf, hs, ha, hallow, hk] at h
                have hp := eq_of_runFromQ_zero_out (P := P) (ρ := ρ) h
                subst pos'
                exact .prim hs ha hallow hk

/-- No position of an infinite execution is final. -/
theorem InfiniteExecution.not_final (X : InfiniteExecution P ρ c) (n : Nat) :
    (X.states n).config.final? = none :=
  (X.step n).not_final

/-- Every infinite execution has exactly the fuel-indexed prefixes obtained by truncating it. -/
def InfiniteExecution.toFuelLimit (X : InfiniteExecution P ρ c) : FuelLimit P ρ c := by
  refine ⟨X.states, ?_⟩
  intro n
  induction n with
  | zero =>
    have hn := X.not_final 0
    rw [X.initial] at hn
    simp [runQ, runFromQ, X.initial, hn]
  | succ n ih =>
    have hadd := runFromQ_add_of_outOfFuel (P := P) (ρ := ρ) ih 1
    change runFromQ P ρ (n + 1) [] [] c = _
    rw [hadd]
    exact (X.step n).runFromQ_one (X.not_final n) (X.not_final (n + 1))

/-- Compatible fuel prefixes correspond to an infinite maximal execution. -/
def FuelLimit.toInfiniteExecution (L : FuelLimit P ρ c) : InfiniteExecution P ρ c := by
  refine ⟨L.states, ?_, ?_⟩
  · apply eq_of_runFromQ_zero_out (P := P) (ρ := ρ)
    simpa [runQ] using L.prefixes 0
  · intro n
    exact move_of_fuel_succ (L.prefixes n) (L.prefixes (n + 1))

/-- Infinite executions and compatible fuel limits are two presentations of the same behavior. -/
theorem infinite_iff_fuelLimit : Nonempty (InfiniteExecution P ρ c) ↔ Nonempty (FuelLimit P ρ c) :=
  ⟨fun ⟨X⟩ => ⟨X.toFuelLimit⟩, fun ⟨L⟩ => ⟨L.toInfiniteExecution⟩⟩

/-- Truncating an infinite execution and reconstructing it returns the original execution. -/
theorem InfiniteExecution.toFuelLimit_toInfiniteExecution (X : InfiniteExecution P ρ c) :
    X.toFuelLimit.toInfiniteExecution = X := rfl

/-- Reconstructing a fuel limit and truncating it returns the original limit. -/
theorem FuelLimit.toInfiniteExecution_toFuelLimit (L : FuelLimit P ρ c) :
    L.toInfiniteExecution.toFuelLimit = L := rfl

/-- Compatible fuel limits from one initial configuration have the same positions. -/
theorem fuelLimit_states_unique (L K : FuelLimit P ρ c) : L.states = K.states := by
  funext n
  have h := (L.prefixes n).symm.trans (K.prefixes n)
  have he := congrArg QueryReport.events h
  have hq := congrArg QueryReport.queries h
  have hc : (L.states n).config = (K.states n).config := by
    have hend := congrArg QueryReport.ending h
    injection hend
  cases hl : L.states n with
  | mk lc le lq =>
    cases hk : K.states n with
    | mk kc ke kq =>
      simp only [hl, hk] at hc he hq
      cases hc
      cases he
      cases hq
      rfl

/-- There is at most one compatible fuel limit for a fixed query-level oracle and initial
configuration. -/
theorem FuelLimit.unique (L K : FuelLimit P ρ c) : L = K := by
  cases L with
  | mk ls lp =>
    cases K with
    | mk ks kp =>
      have hs : ls = ks := fuelLimit_states_unique ⟨ls, lp⟩ ⟨ks, kp⟩
      cases hs
      rfl

/-- A fixed query-level oracle and initial configuration determine a unique infinite execution. -/
theorem InfiniteExecution.unique (X Y : InfiniteExecution P ρ c) : X = Y := by
  have h : X.toFuelLimit = Y.toFuelLimit := FuelLimit.unique _ _
  calc
    X = X.toFuelLimit.toInfiniteExecution := (toFuelLimit_toInfiniteExecution X).symm
    _ = Y.toFuelLimit.toInfiniteExecution :=
      congrArg (fun L : FuelLimit P ρ c => L.toInfiniteExecution) h
    _ = Y := toFuelLimit_toInfiniteExecution Y

/-- One transition extends, and never changes, the raw event history. -/
theorem Move.events_prefix {pos pos' : Position O} (h : Move P ρ pos pos') :
    pos.events <+: pos'.events := by
  cases h with
  | silent => exact ⟨[], by simp⟩
  | prim => exact ⟨_, rfl⟩

/-- Every later position of an infinite execution extends every earlier raw history. -/
theorem InfiniteExecution.events_prefix (X : InfiniteExecution P ρ c) {m n : Nat} (h : m ≤ n) :
    (X.states m).events <+: (X.states n).events := by
  obtain ⟨d, rfl⟩ := Nat.exists_eq_add_of_le h
  induction d with
  | zero => exact ⟨[], by simp⟩
  | succ d ih =>
    obtain ⟨rest, hrest⟩ := ih (Nat.le_add_right m d)
    obtain ⟨next, hnext⟩ := (X.step (m + d)).events_prefix
    refine ⟨rest ++ next, ?_⟩
    rw [← List.append_assoc, hrest, show m + (d + 1) = m + d + 1 by omega, hnext]

/-- Adapter projection preserves the execution's history-prefix order. -/
theorem InfiniteExecution.projected_prefix {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) {m n : Nat} (h : m ≤ n) :
    A.project (X.states m).events <+: A.project (X.states n).events := by
  obtain ⟨rest, hrest⟩ := X.events_prefix h
  exact ⟨A.project rest, by rw [← Observe.ExecAdapter.project_append, hrest]⟩

/-- A prefix determines every list entry that lies in the shorter list. -/
theorem getElem?_eq_of_prefix {α : Type} {xs ys : List α} (h : xs <+: ys) {i : Nat}
    (hi : i < xs.length) : ys[i]? = xs[i]? := by
  obtain ⟨rest, rfl⟩ := h
  exact List.getElem?_append_left hi

/-- The canonical eventually silent C6 execution has exactly `events.take n` events after `n`
steps. -/
theorem visiblePrefix_finiteDivergenceSteps {S : Observe.EventSignature}
    (events : List (Observe.Event S)) (n : Nat) :
    Observe.visiblePrefix (Observe.finiteDivergenceSteps events) n = events.take n := by
  induction n with
  | zero => rfl
  | succ n ih =>
    rw [Observe.visiblePrefix_succ, ih, List.take_add_one]
    by_cases h : n < events.length
    · simp [Observe.finiteDivergenceSteps, Observe.Step.event?, h]
    · simp [Observe.finiteDivergenceSteps, Observe.Step.event?, h]

/-- An emitting C6 execution has exactly the first `n` trace events after `n` steps. -/
theorem visiblePrefix_emitTrace {S : Observe.EventSignature} (trace : Observe.InfiniteTrace S)
    (n : Nat) : Observe.visiblePrefix (fun i => Observe.Step.emit (trace i)) n =
      (List.range n).map trace := by
  induction n with
  | zero => rfl
  | succ n ih => simp [Observe.visiblePrefix_succ, ih, Observe.Step.event?, List.range_succ]

/-- A bounded monotone sequence of natural numbers is eventually constant. -/
theorem eventually_constant_of_bounded {f : Nat → Nat} (mono : ∀ {m n}, m ≤ n → f m ≤ f n) :
    ∀ bound, (∀ n, f n ≤ bound) → ∃ start, ∀ n, start ≤ n → f n = f start := by
  intro bound
  induction bound with
  | zero =>
    intro bounded
    exact ⟨0, fun n _ => by rw [Nat.eq_zero_of_le_zero (bounded n), Nat.eq_zero_of_le_zero (bounded 0)]⟩
  | succ bound ih =>
    intro bounded
    by_cases reached : ∃ n, f n = bound + 1
    · obtain ⟨start, hs⟩ := reached
      refine ⟨start, fun n hn => ?_⟩
      apply Nat.le_antisymm
      · rw [hs]
        exact bounded n
      · exact mono hn
    · apply ih
      intro n
      have hn := bounded n
      have hne : f n ≠ bound + 1 := fun h => reached ⟨n, h⟩
      omega

/-- Helper (computation). Cofinality through the actual projected histories relates arbitrary
matching streams, including noncanonical stuttering schedules (approved: PENDING). -/
theorem MatchesProjectedHistory.sameObservation {S : Observe.EventSignature}
    {A : Observe.ExecAdapter O S} {X : InfiniteExecution P ρ c}
    {xs ys : Nat → Observe.Step S}
    (hx : MatchesProjectedHistory A X xs) (hy : MatchesProjectedHistory A X ys) :
    Observe.SameInfiniteObservation xs ys := by
  constructor
  · intro n
    obtain ⟨i, r, hi⟩ := hx.2 n
    obtain ⟨m, t, hm⟩ := hy.1 i
    exact ⟨m, r ++ t, by rw [hm, hi, List.append_assoc]⟩
  · intro n
    obtain ⟨i, r, hi⟩ := hy.2 n
    obtain ⟨m, t, hm⟩ := hx.1 i
    exact ⟨m, r ++ t, by rw [hm, hi, List.append_assoc]⟩

/-- **Finite projected behavior agrees with C6.** A finite external history determines the
canonical eventually silent C6 execution with exactly the same prefixes and a `finiteDivergence`
observation. -/
theorem finite_projected_canonical {S : Observe.EventSignature} {A : Observe.ExecAdapter O S}
    {X : InfiniteExecution P ρ c} (hf : FiniteEvents A X) :
    ∃ steps events, MatchesProjectedHistory A X steps ∧
      Observe.HasFullObservation (.infinite steps) (.finiteDivergence events) := by
  obtain ⟨bound, bounded⟩ := hf
  let lengths : Nat → Nat := fun n => (A.project (X.states n).events).length
  have mono : ∀ {m n}, m ≤ n → lengths m ≤ lengths n := by
    intro m n h
    obtain ⟨rest, hr⟩ := X.projected_prefix A h
    change (A.project (X.states m).events).length ≤ (A.project (X.states n).events).length
    rw [← hr, List.length_append]
    exact Nat.le_add_right _ _
  obtain ⟨start, stable⟩ := eventually_constant_of_bounded mono bound bounded
  let events := A.project (X.states start).events
  let steps := Observe.finiteDivergenceSteps events
  refine ⟨steps, events, ⟨?_, ?_⟩, Observe.SameInfiniteObservation.refl _⟩
  · intro n
    have pref : A.project (X.states n).events <+: events := by
      by_cases h : n ≤ start
      · exact X.projected_prefix A h
      · have hs : start ≤ n := Nat.le_of_not_ge h
        have heq : A.project (X.states n).events = events := by
          have sameLength := stable n hs
          obtain ⟨rest, hr⟩ := X.projected_prefix A hs
          have hnil : rest = [] := by
            have hl := congrArg List.length hr
            simp only [List.length_append] at hl
            change (A.project (X.states n).events).length =
              (A.project (X.states start).events).length at sameLength
            exact List.eq_nil_of_length_eq_zero (by omega)
          simpa [events, hnil] using hr.symm
        exact heq ▸ List.prefix_rfl
    obtain ⟨rest, hr⟩ := pref
    refine ⟨events.length, rest, ?_⟩
    rw [visiblePrefix_finiteDivergenceSteps, List.take_of_length_le (Nat.le_refl _), hr]
  · intro n
    refine ⟨start, events.drop n, ?_⟩
    rw [visiblePrefix_finiteDivergenceSteps, List.take_append_drop]

/-- **Infinite projected behavior agrees with C6.** An unbounded projected history determines an
infinite C6 trace with exactly the same finite prefixes and an `infiniteDivergence` observation. -/
theorem infinite_projected_canonical {S : Observe.EventSignature} {A : Observe.ExecAdapter O S}
    {X : InfiniteExecution P ρ c} (hi : InfiniteEvents A X) :
    ∃ steps trace, MatchesProjectedHistory A X steps ∧
      Observe.HasFullObservation (.infinite steps) (.infiniteDivergence trace) := by
  classical
  let first : Nat → Nat := fun i => Classical.choose (hi i)
  have first_spec (i : Nat) : i < (A.project (X.states (first i)).events).length :=
    Classical.choose_spec (hi i)
  let trace : Observe.InfiniteTrace S := fun i =>
    (A.project (X.states (first i)).events)[i]'(first_spec i)
  have trace_get (i : Nat) {n : Nat} (hn : i < (A.project (X.states n).events).length) :
      (A.project (X.states n).events)[i]? = some (trace i) := by
    rw [List.getElem?_eq_getElem hn]
    by_cases h : n ≤ first i
    · have he := getElem?_eq_of_prefix (X.projected_prefix A h) hn
      rw [List.getElem?_eq_getElem (first_spec i), List.getElem?_eq_getElem hn] at he
      exact he.symm
    · have hf : first i ≤ n := Nat.le_of_not_ge h
      have he := getElem?_eq_of_prefix (X.projected_prefix A hf) (first_spec i)
      rw [List.getElem?_eq_getElem (first_spec i), List.getElem?_eq_getElem hn] at he
      exact he
  have history_eq_range (n : Nat) : A.project (X.states n).events =
      (List.range ((A.project (X.states n).events).length)).map trace := by
    apply List.ext_getElem?
    intro i
    by_cases h : i < (A.project (X.states n).events).length
    · rw [trace_get i h]
      simp [List.getElem?_map, List.getElem?_range h]
    · have hn : (A.project (X.states n).events).length ≤ i := by omega
      simp [hn]
  have take_eq_range (n m : Nat) (hm : m ≤ (A.project (X.states n).events).length) :
      (List.range m).map trace = (A.project (X.states n).events).take m := by
    induction m with
    | zero => rfl
    | succ m ih =>
      rw [List.range_succ, List.map_append, List.take_add_one, ih (by omega),
        trace_get m (by omega)]
      rfl
  let steps : Nat → Observe.Step S := fun i => .emit (trace i)
  refine ⟨steps, trace, ⟨?_, ?_⟩, Observe.SameInfiniteObservation.refl _⟩
  · intro n
    refine ⟨(A.project (X.states n).events).length, [], ?_⟩
    rw [visiblePrefix_emitTrace, ← history_eq_range]
    simp
  · intro n
    refine ⟨first n, (A.project (X.states (first n)).events).drop n, ?_⟩
    rw [visiblePrefix_emitTrace, take_eq_range _ _ (Nat.le_of_lt (first_spec n)),
      List.take_append_drop]

/-- Helper (computation). Finite projected adequacy supplies a stream and proves the
observation for every matching stream, independent of its silent scheduling (approved: PENDING). -/
theorem finite_projected_observation {S : Observe.EventSignature} {A : Observe.ExecAdapter O S}
    {X : InfiniteExecution P ρ c} (hf : FiniteEvents A X) :
    ∃ events, (∃ steps, MatchesProjectedHistory A X steps) ∧
      ∀ steps, MatchesProjectedHistory A X steps →
        Observe.HasFullObservation (.infinite steps) (.finiteDivergence events) := by
  obtain ⟨canonical, events, hm, ho⟩ := finite_projected_canonical hf
  refine ⟨events, ⟨canonical, hm⟩, ?_⟩
  intro steps hs
  exact Observe.SameInfiniteObservation.trans (hs.sameObservation hm) ho

/-- Helper (computation). Infinite projected adequacy constrains every matching stream,
including streams with arbitrary finite silent gaps (approved: PENDING). -/
theorem infinite_projected_observation {S : Observe.EventSignature} {A : Observe.ExecAdapter O S}
    {X : InfiniteExecution P ρ c} (hi : InfiniteEvents A X) :
    ∃ trace, (∃ steps, MatchesProjectedHistory A X steps) ∧
      ∀ steps, MatchesProjectedHistory A X steps →
        Observe.HasFullObservation (.infinite steps) (.infiniteDivergence trace) := by
  obtain ⟨canonical, trace, hm, ho⟩ := infinite_projected_canonical hi
  refine ⟨trace, ⟨canonical, hm⟩, ?_⟩
  intro steps hs
  exact Observe.SameInfiniteObservation.trans (hs.sameObservation hm) ho

/-- Every infinite execution has either finitely many or infinitely many projected events; these
are the two external classes `SilentDivergence` and `InfiniteInteraction`. -/
theorem finite_or_infinite_events {S : Observe.EventSignature} (A : Observe.ExecAdapter O S)
    (X : InfiniteExecution P ρ c) : FiniteEvents A X ∨ InfiniteEvents A X := by
  classical
  by_cases h : ∃ bound, ∀ n, (A.project (X.states n).events).length ≤ bound
  · exact .inl h
  · refine .inr (fun bound => ?_)
    apply Classical.byContradiction
    intro hn
    apply h
    refine ⟨bound, fun n => ?_⟩
    exact Nat.le_of_not_gt (fun hlt => hn ⟨n, hlt⟩)

/-- One infinite execution cannot have both finitely many and infinitely many projected events. -/
theorem finite_not_infinite_events {S : Observe.EventSignature} {A : Observe.ExecAdapter O S}
    {X : InfiniteExecution P ρ c} (hf : FiniteEvents A X) (hi : InfiniteEvents A X) : False := by
  obtain ⟨bound, hb⟩ := hf
  obtain ⟨n, hn⟩ := hi bound
  exact Nat.not_lt_of_ge (hb n) hn

/-- External silent divergence and infinite interaction are mutually exclusive on the same run. -/
theorem silentDivergence_not_infiniteInteraction {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) :
    SilentDivergence P A ρ c → InfiniteInteraction P A ρ c → False := by
  rintro ⟨X, hf⟩ ⟨Y, hi⟩
  have hxy : X = Y := InfiniteExecution.unique X Y
  subst Y
  exact finite_not_infinite_events hf hi

/-- The raw classification has the same finite/infinite dichotomy, explicitly internally. -/
theorem internal_finite_or_infinite_events (X : InfiniteExecution P ρ c) :
    InternalFiniteEvents X ∨ InternalInfiniteEvents X := by
  classical
  by_cases h : ∃ bound, ∀ n, (X.states n).events.length ≤ bound
  · exact .inl h
  · refine .inr (fun bound => ?_)
    apply Classical.byContradiction
    intro hn
    apply h
    exact ⟨bound, fun n => Nat.le_of_not_gt (fun hlt => hn ⟨n, hlt⟩)⟩

/-- A final report excludes an infinite execution from the same initial configuration. -/
theorem not_infinite_of_final {ρ₀ : Oracle O} {fuel : Nat} {c : Config O} {f : Final O.Val}
    (h : (run P ρ₀ fuel c).ending = .final f)
    (X : InfiniteExecution P ρ₀.toQueryOracle c) : False := by
  have hp := X.toFuelLimit.prefixes fuel
  change runFromQ P ρ₀.toQueryOracle fuel [] [] c = _ at hp
  have hr := runFromQ_toReport (P := P) (ρ₀ := ρ₀) fuel [] [] c
  rw [hp] at hr
  change (runFrom P ρ₀ fuel [] c).ending = .final f at h
  simp [QueryReport.toReport] at hr
  rw [← hr] at h
  cases h

/-- An endlessly blocked `noAnswer` report excludes an infinite execution from the same initial
configuration. -/
theorem not_infinite_of_noAnswer {ρ₀ : Oracle O} {fuel : Nat} {c blocked : Config O}
    (h : (run P ρ₀ fuel c).ending = .noAnswer blocked)
    (X : InfiniteExecution P ρ₀.toQueryOracle c) : False := by
  have hp := X.toFuelLimit.prefixes fuel
  change runFromQ P ρ₀.toQueryOracle fuel [] [] c = _ at hp
  have hr := runFromQ_toReport (P := P) (ρ₀ := ρ₀) fuel [] [] c
  rw [hp] at hr
  change (runFrom P ρ₀ fuel [] c).ending = .noAnswer blocked at h
  simp [QueryReport.toReport] at hr
  rw [← hr] at h
  cases h

/-- A blocked request remains blocked at every larger budget; it never turns into divergence or a
program outcome merely because more fuel is supplied. -/
theorem endlesslyBlocked_of_noAnswer {ρ₀ : Oracle O} {fuel : Nat} {c blocked : Config O}
    (h : (run P ρ₀ fuel c).ending = .noAnswer blocked) : EndlesslyBlocked P ρ₀ fuel c blocked := by
  change (runFrom P ρ₀ fuel [] c).ending = .noAnswer blocked at h
  intro n hle
  have hm : run P ρ₀ n c = run P ρ₀ fuel c := by
    apply runFrom_mono fuel n hle [] c
    intro c'
    rw [h]
    simp
  exact (congrArg Report.ending hm).trans h

/-- Every admitted call has a successful lookup and entry and reaches a final outcome. The same
outcome is reported at every fuel budget above the certificate's witness budget. -/
theorem TerminationEvidence.all_allowed_runs {domain : CallDomain O} (E : TerminationEvidence O P domain)
     (ρ : Oracle O) (args : List O.Val) (st : O.Store)
    (ha : domain.calls ρ args st) (m : Nat) (hm : E.conditional.fuel E.premises ρ args st ≤ m) :
    ∃ fn c f, AList.lookup P.fns domain.function = some fn ∧ enter fn args st = some c ∧
      (run P ρ m c).ending = .final f := by
  obtain ⟨fn, c, f, hf, he, hfinal⟩ := E.conditional.terminates E.premises ρ args st ha
  refine ⟨fn, c, f, hf, he, ?_⟩
  have hnot : ∀ c', (run P ρ (E.conditional.fuel E.premises ρ args st) c).ending ≠ .outOfFuel c' := by
    rw [hfinal]
    simp
  have hmono : run P ρ m c = run P ρ (E.conditional.fuel E.premises ρ args st) c := by
    apply runFrom_mono (P := P) (ρ₀ := ρ) (E.conditional.fuel E.premises ρ args st) m hm [] c hnot
  exact (congrArg Report.ending hmono).trans hfinal

/-- Per-function termination evidence excludes divergence from every successful admitted entry
configuration. -/
theorem TerminationEvidence.excludes_infinite {domain : CallDomain O} (E : TerminationEvidence O P domain)
     (ρ : Oracle O) (args : List O.Val) (st : O.Store)
    (fn : Fn O) (c : Config O) (hf : AList.lookup P.fns domain.function = some fn)
    (ha : domain.calls ρ args st) (he : enter fn args st = some c)
    (X : InfiniteExecution P ρ.toQueryOracle c) : False := by
  obtain ⟨fn', c', f, hf', he', hfinal⟩ := E.conditional.terminates E.premises ρ args st ha
  rw [hf] at hf'
  cases hf'
  rw [he] at he'
  cases he'
  exact not_infinite_of_final hfinal X

/-! ### Fuel exhaustion is only a budget report -/

namespace Demo

/-- A whole-program configuration that needs one silent step before it is final. -/
def finishingConfig : Config ops := ⟨.exec .skip, [], .init, []⟩

/-- The demo's silent, nonterminating loop. -/
def spinConfig : Config ops :=
  start ⟨[], .int, ⟨0⟩, .loop ⟨0⟩ .skip (.lit 1) .skip s⟩ [] .init

/-- `outOfFuel` does not imply divergence: this run only needed one more unit of fuel. -/
example :
    isOutOfFuel (run prog env 0 finishingConfig) = true ∧
      brief (run prog env 1 finishingConfig) = ([], some .normal) := by decide

/-- A configuration whose first step aborts. -/
def failingConfig : Config ops := ⟨.exec (.abort s), [], .init, []⟩

/-- `outOfFuel` also does not exclude failure: one more unit of fuel reveals this trap. -/
example : isOutOfFuel (run prog env 0 failingConfig) = true ∧
    brief (run prog env 1 failingConfig) = ([], some (.trapped .abort s)) := by decide

/-- `outOfFuel` is not a program failure either: the silent loop exhausts its simulation budget
without trapping or becoming stuck. The infinite-execution witness is `spin_infinite` below. -/
example : isOutOfFuel (run prog env 50 spinConfig) = true := by decide

/-- The store after entering the demo's parameterless main function. -/
def spinStore : DState := DState.life (.enter []) .init

/-- The main function's scope frame. -/
def spinScope : Frame ops := .scope ⟨0⟩ [] []

/-- The loop's condition-test and body continuation frames. -/
def spinTest : Frame ops := .test ⟨0⟩ .skip (.lit 1) .skip s []
def spinBody : Frame ops := .body ⟨0⟩ .skip (.lit 1) .skip s

/-- The four successors of `spinConfig` in one five-step silent loop iteration. -/
def spinConfig1 : Config ops := ⟨.exec .skip, [], spinStore, [spinTest, spinScope]⟩
def spinConfig2 : Config ops := ⟨.done, [], spinStore, [spinTest, spinScope]⟩
def spinConfig3 : Config ops := ⟨.exec .skip, [], spinStore, [spinBody, spinScope]⟩
def spinConfig4 : Config ops := ⟨.done, [], spinStore, [spinBody, spinScope]⟩

/-- The configuration at a phase of the periodic silent execution. -/
def spinConfigAt : Nat → Config ops
  | 0 => spinConfig
  | 1 => spinConfig1
  | 2 => spinConfig2
  | 3 => spinConfig3
  | 4 => spinConfig4
  | _ => spinConfig

/-- No events or queries occur in the periodic execution. -/
def spinPosition (n : Nat) : Position ops := ⟨spinConfigAt (n % 5), [], []⟩

theorem spin_move_01 : Move prog env ⟨spinConfig, [], []⟩ ⟨spinConfig1, [], []⟩ := .silent rfl
theorem spin_move_12 : Move prog env ⟨spinConfig1, [], []⟩ ⟨spinConfig2, [], []⟩ := .silent rfl
theorem spin_move_23 : Move prog env ⟨spinConfig2, [], []⟩ ⟨spinConfig3, [], []⟩ := .silent rfl
theorem spin_move_34 : Move prog env ⟨spinConfig3, [], []⟩ ⟨spinConfig4, [], []⟩ := .silent rfl
theorem spin_move_40 : Move prog env ⟨spinConfig4, [], []⟩ ⟨spinConfig, [], []⟩ := .silent rfl

/-- Every adjacent pair of periodic positions is one machine transition. -/
theorem spin_move (n : Nat) : Move prog env (spinPosition n) (spinPosition (n + 1)) := by
  have hlt : n % 5 < 5 := Nat.mod_lt _ (by decide)
  have hc : n % 5 = 0 ∨ n % 5 = 1 ∨ n % 5 = 2 ∨ n % 5 = 3 ∨ n % 5 = 4 := by omega
  rcases hc with h | h | h | h | h
  · have hn : (n + 1) % 5 = 1 := by omega
    simpa [spinPosition, spinConfigAt, h, hn] using spin_move_01
  · have hn : (n + 1) % 5 = 2 := by omega
    simpa [spinPosition, spinConfigAt, h, hn] using spin_move_12
  · have hn : (n + 1) % 5 = 3 := by omega
    simpa [spinPosition, spinConfigAt, h, hn] using spin_move_23
  · have hn : (n + 1) % 5 = 4 := by omega
    simpa [spinPosition, spinConfigAt, h, hn] using spin_move_34
  · have hn : (n + 1) % 5 = 0 := by omega
    simpa [spinPosition, spinConfigAt, h, hn] using spin_move_40

/-- The demo loop is an infinite maximal execution under its fixed environment. -/
def spin_infinite : InfiniteExecution prog env spinConfig where
  states := spinPosition
  initial := rfl
  step := spin_move

/-- In particular it is internal silent divergence: infinitely many steps and no raw events. -/
theorem spin_internal_silent_divergence : InternalSilentDivergence prog env spinConfig :=
  ⟨spin_infinite, ⟨0, fun n => by simp [spin_infinite, spinPosition]⟩⟩

/-! ### Raw events need not be externally visible -/

/-- The same loop as `spinConfig`, except that its body logs one raw event per iteration. -/
def eventBody : Stmt ops := .prim none .log [.lit 1] s
def eventConfig : Config ops :=
  start ⟨[], .int, ⟨0⟩, .loop ⟨0⟩ .skip (.lit 1) eventBody s⟩ [] .init

def eventTest : Frame ops := .test ⟨0⟩ .skip (.lit 1) eventBody s []
def eventBodyFrame : Frame ops := .body ⟨0⟩ .skip (.lit 1) eventBody s
def eventConfig1 : Config ops := ⟨.exec .skip, [], spinStore, [eventTest, spinScope]⟩
def eventConfig2 : Config ops := ⟨.done, [], spinStore, [eventTest, spinScope]⟩
def eventConfig3 : Config ops := ⟨.exec eventBody, [], spinStore, [eventBodyFrame, spinScope]⟩
def eventConfig4 : Config ops := ⟨.done, [], spinStore, [eventBodyFrame, spinScope]⟩

def eventConfigAt : Nat → Config ops
  | 0 => eventConfig
  | 1 => eventConfig1
  | 2 => eventConfig2
  | 3 => eventConfig3
  | 4 => eventConfig4
  | _ => eventConfig

/-- The raw history after `n` completed logging requests. -/
def rawEvents (n : Nat) : List Int := (List.range n).map fun _ => 1

/-- The query issued by iteration `n`, including its actual occurrence and prior raw history. -/
def eventQuery (n : Nat) : Query ops := ⟨rawEvents n, .log, [1], s, spinStore, n⟩

/-- One event is emitted on the transition from phase three to phase four. -/
def eventCount (n : Nat) : Nat := n / 5 + if 3 < n % 5 then 1 else 0

def eventPosition (n : Nat) : Position ops :=
  ⟨eventConfigAt (n % 5), rawEvents (eventCount n),
    (List.range (eventCount n)).map eventQuery⟩

theorem event_move_01 (n : Nat) : Move prog env
    ⟨eventConfig, rawEvents n, (List.range n).map eventQuery⟩
    ⟨eventConfig1, rawEvents n, (List.range n).map eventQuery⟩ := .silent rfl
theorem event_move_12 (n : Nat) : Move prog env
    ⟨eventConfig1, rawEvents n, (List.range n).map eventQuery⟩
    ⟨eventConfig2, rawEvents n, (List.range n).map eventQuery⟩ := .silent rfl
theorem event_move_23 (n : Nat) : Move prog env
    ⟨eventConfig2, rawEvents n, (List.range n).map eventQuery⟩
    ⟨eventConfig3, rawEvents n, (List.range n).map eventQuery⟩ := .silent rfl
theorem event_move_40 (n : Nat) : Move prog env
    ⟨eventConfig4, rawEvents n, (List.range n).map eventQuery⟩
    ⟨eventConfig, rawEvents n, (List.range n).map eventQuery⟩ := .silent rfl

/-- The logging transition appends exactly its raw event and its next per-site query. -/
theorem event_move_34 (n : Nat) : Move prog env
    ⟨eventConfig3, rawEvents n, (List.range n).map eventQuery⟩
    ⟨eventConfig4, rawEvents (n + 1), (List.range (n + 1)).map eventQuery⟩ := by
  have hocc : occurrenceAt ((List.range n).map eventQuery) s = n := by
    induction n with
    | zero => rfl
    | succ n ih =>
      rw [List.range_succ, List.map_append]
      simp only [List.map_cons, List.map_nil]
      rw [issue_next_occurrence, ih]
      simp [eventQuery]
  have hq : Query.issue (O := ops) (rawEvents n) .log [1] s spinStore
      ((List.range n).map eventQuery) = eventQuery n := by
    simp [Query.issue, eventQuery, hocc]
  have hm : Move prog env ⟨eventConfig3, rawEvents n, (List.range n).map eventQuery⟩
      ⟨eventConfig4, rawEvents n ++ [1], (List.range n).map eventQuery ++ [eventQuery n]⟩ := by
    rw [← hq]
    exact Move.prim (P := prog) (ρ := env) (c := eventConfig3) (c' := eventConfig4)
      (events := rawEvents n) (issued := (List.range n).map eventQuery) (p := .log) (vs := [1])
      (site := s) (st := spinStore) (a := ⟨some 1, .ok 0, spinStore⟩) rfl rfl rfl rfl
  simpa [rawEvents, List.range_succ] using hm

/-- Every adjacent pair of positions in the logging loop is one real machine transition. -/
theorem event_move (n : Nat) : Move prog env (eventPosition n) (eventPosition (n + 1)) := by
  have hlt : n % 5 < 5 := Nat.mod_lt _ (by decide)
  have hc : n % 5 = 0 ∨ n % 5 = 1 ∨ n % 5 = 2 ∨ n % 5 = 3 ∨ n % 5 = 4 := by omega
  rcases hc with h | h | h | h | h
  · have hn : (n + 1) % 5 = 1 := by omega
    have hd : (n + 1) / 5 = n / 5 := by omega
    simpa [eventPosition, eventCount, eventConfigAt, h, hn, hd] using event_move_01 (eventCount n)
  · have hn : (n + 1) % 5 = 2 := by omega
    have hd : (n + 1) / 5 = n / 5 := by omega
    simpa [eventPosition, eventCount, eventConfigAt, h, hn, hd] using event_move_12 (eventCount n)
  · have hn : (n + 1) % 5 = 3 := by omega
    have hd : (n + 1) / 5 = n / 5 := by omega
    simpa [eventPosition, eventCount, eventConfigAt, h, hn, hd] using event_move_23 (eventCount n)
  · have hn : (n + 1) % 5 = 4 := by omega
    have hd : (n + 1) / 5 = n / 5 := by omega
    simpa [eventPosition, eventCount, eventConfigAt, h, hn, hd] using event_move_34 (eventCount n)
  · have hn : (n + 1) % 5 = 0 := by omega
    have hd : (n + 1) / 5 = n / 5 + 1 := by omega
    simpa [eventPosition, eventCount, eventConfigAt, h, hn, hd] using event_move_40 (eventCount n)

/-- The logging loop is a genuine infinite execution with one raw event per iteration. -/
def event_infinite : InfiniteExecution prog env eventConfig where
  states := eventPosition
  initial := rfl
  step := event_move

/-- An adapter that erases every raw event and gives every primitive an internal contract. -/
def erasingAdapter : Observe.ExecAdapter ops Observe.Demo.signature where
  events := fun _ => []
  request := fun _ _ _ _ => none
  status := fun _ => none
  sites := []

/-- An adapter that retains every raw event as public output. -/
def keepingAdapter : Observe.ExecAdapter ops Observe.Demo.signature where
  events := fun n => [.output (toString n)]
  request := fun _ _ _ _ => none
  status := fun _ => none
  sites := []

theorem erasing_project (events : List Int) : erasingAdapter.project events = [] := by
  induction events with
  | nil => rfl
  | cons event events ih => simp [Observe.ExecAdapter.project, erasingAdapter]

theorem keeping_project_rawEvents (n : Nat) : keepingAdapter.project (rawEvents n) =
    (List.range n).map fun _ => Observe.Event.output "1" := by
  induction n with
  | zero => rfl
  | succ n ih =>
    rw [show rawEvents (n + 1) = rawEvents n ++ [1] by simp [rawEvents, List.range_succ],
      Observe.ExecAdapter.project_append, ih]
    simp [Observe.ExecAdapter.project, keepingAdapter, List.range_succ] <;> decide

/-- Empty output satisfies C6's internal-answer rule even though C1 emits a raw event. -/
theorem erasingWellFormed : Observe.ExecAdapter.WellFormed erasingAdapter Observe.Demo.contract () env := by
  constructor
  · intro hist p vs site store a answer typed
    exact ⟨[], erasing_project _⟩
  · intro hist p vs site store a q answer typed
    simp [erasingAdapter] at typed
  · intro hist p vs site store q answer typed
    simp [erasingAdapter] at typed

/-- Externally the erasing run is silent divergence. -/
theorem erasing_silent_divergence : SilentDivergence prog erasingAdapter env eventConfig :=
  ⟨event_infinite, ⟨0, fun n => by simp [event_infinite, eventPosition, erasing_project]⟩⟩

/-- Internally that same run has infinitely many raw events. -/
theorem erasing_internal_infinite : InternalInfiniteInteraction prog env eventConfig := by
  refine ⟨event_infinite, fun bound => ⟨5 * (bound + 1), ?_⟩⟩
  simp [event_infinite, eventPosition, eventCount, rawEvents]

/-- Retaining the raw events turns the same execution into external infinite interaction. -/
theorem keeping_infinite_interaction : InfiniteInteraction prog keepingAdapter env eventConfig := by
  refine ⟨event_infinite, fun bound => ⟨5 * (bound + 1), ?_⟩⟩
  simp [event_infinite, eventPosition, eventCount, keeping_project_rawEvents]

/-- Non-vacuity for `finite_projected_observation`: the erasing adapter has a real C6 finite
observation even though the raw execution interacts forever. -/
example : ∃ steps events, MatchesProjectedHistory erasingAdapter event_infinite steps ∧
    Observe.HasFullObservation (.infinite steps) (.finiteDivergence events) :=
  finite_projected_canonical (show FiniteEvents erasingAdapter event_infinite from
    ⟨0, fun n => by simp [erasing_project]⟩)

/-- Non-vacuity for `infinite_projected_observation`: the keeping adapter has a real infinite C6
trace. -/
example : ∃ steps trace, MatchesProjectedHistory keepingAdapter event_infinite steps ∧
    Observe.HasFullObservation (.infinite steps) (.infiniteDivergence trace) :=
  infinite_projected_canonical (show InfiniteEvents keepingAdapter event_infinite from by
    intro bound
    refine ⟨5 * (bound + 1), ?_⟩
    simp [event_infinite, eventPosition, eventCount, keeping_project_rawEvents])

/-- Planted raw-event classification: it disagrees with C6 on the erasing adapter. The run has
infinite internal interaction but no external infinite interaction. -/
example : InternalInfiniteInteraction prog env eventConfig ∧
    ¬ InfiniteInteraction prog erasingAdapter env eventConfig :=
  ⟨erasing_internal_infinite,
    silentDivergence_not_infiniteInteraction erasingAdapter erasing_silent_divergence⟩

/-- Planted invented C6 history: a constantly emitting step sequence cannot match the erasing
adapter's empty projected history. -/
example : ¬ MatchesProjectedHistory erasingAdapter event_infinite
    (fun _ => Observe.Step.emit (Observe.Event.output "invented")) := by
  intro matched
  obtain ⟨n, rest, history⟩ := matched.2 1
  simp [erasing_project, Observe.visiblePrefix_succ, Observe.Step.event?] at history

/-! ### Execution-level occurrence and oracle witnesses -/

/-- Two consecutive silent requests at one site. -/
def silentRequestConfig : Config ops := ⟨.exec (.seq
  (.prim none .bump [] s) (.prim none .bump [] s)), [], .init, []⟩

/-- The same requests, but at two distinct logical sites. -/
def otherSite : SiteId := ⟨1⟩
def interleavedRequestConfig : Config ops := ⟨.exec (.seq
  (.prim none .bump [] s) (.prim none .bump [] otherSite)), [], .init, []⟩

def silentRequests : QueryReport ops := runQ prog env 4 silentRequestConfig
def interleavedRequests : QueryReport ops := runQ prog env 4 interleavedRequestConfig

/-- The authenticated queries of those actual runs. -/
def silentQuery0 : Query ops := Query.issue [] .bump [] s .init []
def silentQuery1 : Query ops := Query.issue [] .bump [] s ⟨1, []⟩ [silentQuery0]
def interleavedQuery0 : Query ops := Query.issue [] .bump [] s .init []
def interleavedQuery1 : Query ops := Query.issue [] .bump [] otherSite ⟨1, []⟩ [interleavedQuery0]

/-- Non-vacuity: both runs really issue two requests and emit no raw event. -/
example : silentRequests.events = [] ∧ silentRequests.queries = [silentQuery0, silentQuery1] :=
  ⟨rfl, rfl⟩
example : interleavedRequests.events = [] ∧
    interleavedRequests.queries = [interleavedQuery0, interleavedQuery1] := ⟨rfl, rfl⟩

/-- Non-vacuity for `runFromQ_consecutive_same_site`: the second actual query has occurrence one. -/
example : silentQuery1.occurrence = silentQuery0.occurrence + 1 := by
  apply runFromQ_consecutive_same_site (P := prog) (ρ := env) 4 [] [] silentRequestConfig
    (i := 0) (q := silentQuery0) (q' := silentQuery1)
  · intro i q h
    simp at h
  · rfl
  · rfl
  · rfl

/-- Non-vacuity for `runFromQ_consecutive_other_site`: the second site's first occurrence is zero. -/
example : interleavedQuery1.occurrence = 0 := by
  have h := runFromQ_consecutive_other_site (P := prog) (ρ := env) 4 [] []
    interleavedRequestConfig (i := 0) (q := interleavedQuery0) (q' := interleavedQuery1)
    (by intro i q h; simp at h) (by rfl) (by rfl) (by decide)
  simpa [occurrenceAt] using h

/-- A planted counter that never advances. -/
def frozenQueries : List (Query ops) := [silentQuery0, { silentQuery1 with occurrence := 0 }]

/-- Planted counter-example for the same-site law: the frozen second occurrence is not the number
of earlier queries at that site. -/
example : ¬ QueryHistoryValid frozenQueries := by
  intro valid
  have second := valid 1 frozenQueries[1] (by rfl)
  change 0 = 1 at second
  contradiction

/-- A planted counter shared across sites, which wrongly advances the second site's occurrence. -/
def sharedQueries : List (Query ops) :=
  [interleavedQuery0, { interleavedQuery1 with occurrence := 1 }]

/-- Planted counter-example for the interleaved-site law. -/
example : ¬ QueryHistoryValid sharedQueries := by
  intro valid
  have second := valid 1 sharedQueries[1] (by rfl)
  change 1 = 0 at second
  contradiction

/-- One direct silent request and the successful configuration produced by its ordinary answer. -/
def bumpConfig : Config ops := ⟨.exec (.prim none .bump [] s), [], .init, []⟩
def bumpContinued : Config ops := ⟨.done, [], ⟨1, []⟩, []⟩
def bumpQuery : Query ops := Query.issue [] .bump [] s .init []
def bumpK : Answer ops.Val ops.Event ops.Store → Option (Config ops) := fun a =>
  match a.result with
  | .ok v => (bindLife ops none a.store).map fun st' => ⟨.done, bindDst none v [], st', []⟩
  | .error kd => some ⟨.trapped kd s, [], a.store, []⟩

/-- A real transition under the lifted ordinary demo oracle. -/
theorem bumpedMove : Move prog env ⟨bumpConfig, [], []⟩ ⟨bumpContinued, [], [bumpQuery]⟩ :=
  Move.prim (P := prog) (ρ := env) (c := bumpConfig) (c' := bumpContinued) (events := [])
    (issued := []) (p := .bump) (vs := []) (site := s) (st := .init)
    (a := ⟨none, .ok 0, ⟨1, []⟩⟩) rfl rfl rfl rfl

/-- Non-vacuity for `Move.toQueryOracle_answer`: the actual transition uses exactly `env`'s answer. -/
example : ∃ a, env [] .bump [] s .init = some a ∧ ops.allows .bump [] .init a = true ∧
    bumpK a = some bumpContinued ∧ [] = [] ++ a.event.toList ∧
      [bumpQuery] = [] ++ [bumpQuery] := by
  exact (Move.toQueryOracle_answer (ρ₀ := env) (pos := ⟨bumpConfig, [], []⟩)
    (pos' := ⟨bumpContinued, [], [bumpQuery]⟩) (p := .bump) (vs := []) (site := s)
    (st := .init) (k := bumpK) (by
      simp [bumpConfig, step, evalArgs, onRes]
      funext a
      cases hr : a.result <;> simp [bumpK, hr])).mp bumpedMove

/-- An occurrence-sensitive mutation answers only the first request at each site. -/
def occurrenceSensitiveOracle : QueryOracle ops := fun q =>
  if q.occurrence = 0 then answer env.toQueryOracle q else none

/-- Planted counter-example for lifted-oracle preservation: unlike `env.toQueryOracle`, this oracle
refuses the actual second request and therefore produces a different run. -/
example : (runQ prog occurrenceSensitiveOracle 4 silentRequestConfig).toReport ≠
    run prog env 4 silentRequestConfig := by
  intro h
  cases h

/-! ### Exact terminal-query witnesses -/

/-- A configuration whose first machine step requests `log 1`. -/
def terminalConfig : Config ops := ⟨.exec (.prim none .log [.lit 1] s), [], .init, []⟩

/-- The unique first query issued by `terminalConfig`. -/
def terminalQuery : Query ops := Query.issue [] .log [1] s .init []

/-- A planted fabrication: the same request with occurrence `1` instead of its actual occurrence
`0`. -/
def fabricatedTerminalQuery : Query ops := { terminalQuery with occurrence := 1 }

/-- A query oracle that never answers. -/
def noAnswerOracle : QueryOracle ops := fun _ => none

/-- Non-vacuous soundness instance: the concrete run reports exactly the authenticated query. -/
example : runFromQ prog noAnswerOracle 1 [] [] terminalConfig =
    ⟨[], [terminalQuery], .noAnswer terminalConfig⟩ := rfl

/-- The exact terminal query is accepted together with its conclusion. -/
example : ∃ hc : Conclusion prog noAnswerOracle (.noAnswer terminalConfig)
    ⟨terminalConfig, [], []⟩, ReportedQueries prog noAnswerOracle hc [terminalQuery] := by
  refine ⟨.noAnswer rfl rfl, ?_⟩
  exact ReportedQueries.noAnswer (P := prog) (ρ := noAnswerOracle) (c := terminalConfig)
    (events := []) (issued := []) (p := .log) (vs := [1]) (site := s) (st := .init) rfl rfl

/-- Planted counter-example: no justification accepts a fabricated occurrence number. -/
example : ¬ ∃ hc : Conclusion prog noAnswerOracle (.noAnswer terminalConfig)
    ⟨terminalConfig, [], []⟩,
    ReportedQueries prog noAnswerOracle hc [fabricatedTerminalQuery] := by
  rintro ⟨hc, hr⟩
  cases hc
  cases hr

/-- An oracle whose answer has the wrong event for `log 1`. -/
def badAnswerOracle : QueryOracle ops := fun _ => some ⟨some 5, .ok 0, .init⟩

/-- A refused answer still records exactly the request that was issued. -/
example : ∃ hc : Conclusion prog badAnswerOracle (.badAnswer terminalConfig)
    ⟨terminalConfig, [], []⟩, ReportedQueries prog badAnswerOracle hc [terminalQuery] := by
  refine ⟨.badAnswer rfl rfl rfl, ?_⟩
  exact ReportedQueries.badAnswer (P := prog) (ρ := badAnswerOracle) (c := terminalConfig)
    (events := []) (issued := []) (p := .log) (vs := [1]) (site := s) (st := .init) rfl rfl rfl

/-- Planted counter-example: `badAnswer` cannot record the fabricated query either. -/
example : ¬ ∃ hc : Conclusion prog badAnswerOracle (.badAnswer terminalConfig)
    ⟨terminalConfig, [], []⟩,
    ReportedQueries prog badAnswerOracle hc [fabricatedTerminalQuery] := by
  rintro ⟨hc, hr⟩
  cases hc
  cases hr

/-- A machine-stuck configuration with no request in flight. -/
def terminalStuckConfig : Config ops := ⟨.trapped .abort s, [], .init, []⟩

/-- Machine stuckness appends no query. -/
example : ∃ hc : Conclusion prog noAnswerOracle (.stuck terminalStuckConfig)
    ⟨terminalStuckConfig, [], []⟩, ReportedQueries prog noAnswerOracle hc [] := by
  refine ⟨.stuck rfl, ?_⟩
  exact ReportedQueries.stuck (P := prog) (ρ := noAnswerOracle) (c := terminalStuckConfig)
    (events := []) (issued := []) rfl

/-- Planted counter-example: no `stuck` justification lets machine stuckness acquire a query. -/
example : ¬ ∃ hc : Conclusion prog noAnswerOracle (.stuck terminalStuckConfig)
    ⟨terminalStuckConfig, [], []⟩, ReportedQueries prog noAnswerOracle hc [terminalQuery] := by
  rintro ⟨hc, hr⟩
  cases hc <;> cases hr <;> simp_all [terminalStuckConfig, step]

/-- Demo operands whose lifecycle discipline refuses every primitive result binding. -/
abbrev refusingBindOps : Operands := { ops with
  life := fun e st => match e with
    | .bind _ => none
    | _ => some (DState.life e st) }

/-- The empty program is sufficient for a direct primitive statement. -/
def refusingBindProgram : Program refusingBindOps := ⟨[]⟩

/-- A request whose otherwise valid answer cannot be bound to `x`. -/
def refusingBindConfig : Config refusingBindOps :=
  ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩

/-- The allowed answer to `refusingBindConfig`'s request. -/
def refusingBindOracle : QueryOracle refusingBindOps := fun _ => some ⟨some 1, .ok 0, .init⟩

/-- The query issued before the continuation refuses the answer. -/
def refusingBindQuery : Query refusingBindOps := Query.issue [] .log [1] s .init []

/-- Continuation refusal records the authenticated terminal query. -/
example : ∃ hc : Conclusion refusingBindProgram refusingBindOracle (.stuck refusingBindConfig)
    ⟨refusingBindConfig, [], []⟩, ReportedQueries refusingBindProgram refusingBindOracle hc
      [refusingBindQuery] := by
  refine ⟨.stuckContinuation rfl rfl rfl rfl, ?_⟩
  exact ReportedQueries.stuckContinuation (P := refusingBindProgram) (ρ := refusingBindOracle)
    (c := refusingBindConfig) (events := []) (issued := []) (p := .log) (vs := [1]) (site := s)
    (st := .init) rfl rfl rfl rfl

/-- Planted counter-example: no `stuck` justification lets continuation refusal omit its query and
masquerade as machine stuckness. -/
example : ¬ ∃ hc : Conclusion refusingBindProgram refusingBindOracle (.stuck refusingBindConfig)
    ⟨refusingBindConfig, [], []⟩, ReportedQueries refusingBindProgram refusingBindOracle hc [] := by
  rintro ⟨hc, hr⟩
  cases hc <;> cases hr <;>
    simp_all [refusingBindConfig, refusingBindOps, ops, step, evalArgs, evalE, onRes,
      Res.bind, Res.map]

/-! ### Usable termination certificates and rejected certificates -/

/-- A declared singleton call domain, independent of its termination proof. -/
def terminatingDomain : CallDomain ops :=
  ⟨boomF, fun ρ args st => ρ = env ∧ args = [] ∧ st = .init⟩

/-- This aborting function terminates with a trapped outcome; termination does not promise success. -/
def terminatingConditional : ConditionalTermination ops prog terminatingDomain where
  Premises := Unit
  dependencies := []
  fuel := fun _ _ _ _ => 10
  terminates := by
    intro _ ρ args st h
    obtain ⟨rfl, rfl, rfl⟩ := h
    exact ⟨boomFn, start boomFn [] .init, .trapped .abort ⟨9⟩,
      rfl, rfl, rfl⟩

/-- An inhabited certificate for a declared function and an actual successful entry. -/
def terminatingEvidence : TerminationEvidence ops prog terminatingDomain :=
  ⟨terminatingConditional, (), env, [], .init, rfl, rfl, rfl⟩

/-- The empty declared domain has no usable certificate, even though its conditional coverage
can be proved with no executions. -/
example : ¬ Nonempty (TerminationEvidence ops prog ⟨boomF, fun _ _ _ => False⟩) := by
  rintro ⟨E⟩
  obtain ⟨ρ, args, st, h⟩ := E.admitted
  exact h

/-- A missing function cannot be certified by restricting the certificate's admission filter. -/
example : ¬ Nonempty (TerminationEvidence ops (⟨[]⟩ : Program ops) terminatingDomain) := by
  rintro ⟨E⟩
  obtain ⟨fn, c, f, hf, _, _⟩ := E.conditional.terminates E.premises env [] .init ⟨rfl, rfl, rfl⟩
  cases hf

/-- The domain of a call whose entry lifecycle is refused. -/
def refusedEntryOps : Operands := { ops with life := fun _ _ => none }
def refusedEntryFn : Fn refusedEntryOps := ⟨[], .unit, ⟨101⟩, .skip⟩
def refusedEntryProgram : Program refusedEntryOps := ⟨[(boomF, refusedEntryFn)]⟩
def refusedEntryDomain : CallDomain refusedEntryOps := ⟨boomF, fun _ _ _ => True⟩

/-- Entry failure is a failed certificate obligation, not a premise silently excluding the call. -/
example : ¬ Nonempty (TerminationEvidence refusedEntryOps refusedEntryProgram refusedEntryDomain) := by
  rintro ⟨E⟩
  obtain ⟨ρ, args, st, h⟩ := E.admitted
  obtain ⟨fn, c, f, hf, he, _⟩ := E.conditional.terminates E.premises ρ args st h
  have eqfn : fn = refusedEntryFn := by simpa [refusedEntryProgram, refusedEntryDomain, AList.lookup] using hf.symm
  subst fn
  simp [enter, refusedEntryOps] at he

/-- A declared looping function, with the same scope as `spinConfig`. -/
def spinningFn : Fn ops := ⟨[], .int, ⟨0⟩, .loop ⟨0⟩ .skip (.lit 1) .skip s⟩
def spinningProgram : Program ops := ⟨[(mainF, spinningFn)]⟩
def spinningDomain : CallDomain ops :=
  ⟨mainF, fun ρ args st => ρ = env ∧ args = [] ∧ st = .init⟩

/-- Program lookup is irrelevant to this call-free loop. -/
def admittedSpin : InfiniteExecution spinningProgram env (start spinningFn [] .init) where
  states := spinPosition
  initial := rfl
  step := by
    intro n
    have spin_move_01 : Move spinningProgram env ⟨spinConfig, [], []⟩ ⟨spinConfig1, [], []⟩ := .silent rfl
    have spin_move_12 : Move spinningProgram env ⟨spinConfig1, [], []⟩ ⟨spinConfig2, [], []⟩ := .silent rfl
    have spin_move_23 : Move spinningProgram env ⟨spinConfig2, [], []⟩ ⟨spinConfig3, [], []⟩ := .silent rfl
    have spin_move_34 : Move spinningProgram env ⟨spinConfig3, [], []⟩ ⟨spinConfig4, [], []⟩ := .silent rfl
    have spin_move_40 : Move spinningProgram env ⟨spinConfig4, [], []⟩ ⟨spinConfig, [], []⟩ := .silent rfl
    have hlt : n % 5 < 5 := Nat.mod_lt _ (by decide)
    have hc : n % 5 = 0 ∨ n % 5 = 1 ∨ n % 5 = 2 ∨ n % 5 = 3 ∨ n % 5 = 4 := by omega
    rcases hc with h | h | h | h | h
    · have hn : (n + 1) % 5 = 1 := by omega
      simpa [spinPosition, spinConfigAt, h, hn] using spin_move_01
    · have hn : (n + 1) % 5 = 2 := by omega
      simpa [spinPosition, spinConfigAt, h, hn] using spin_move_12
    · have hn : (n + 1) % 5 = 3 := by omega
      simpa [spinPosition, spinConfigAt, h, hn] using spin_move_23
    · have hn : (n + 1) % 5 = 4 := by omega
      simpa [spinPosition, spinConfigAt, h, hn] using spin_move_34
    · have hn : (n + 1) % 5 = 0 := by omega
      simpa [spinPosition, spinConfigAt, h, hn] using spin_move_40

/-- A domain containing the spinning call cannot acquire usable evidence. -/
example : ¬ Nonempty (TerminationEvidence ops spinningProgram spinningDomain) := by
  rintro ⟨E⟩
  exact E.excludes_infinite env [] .init spinningFn (start spinningFn [] .init)
    rfl ⟨rfl, rfl, rfl⟩ rfl admittedSpin

end Demo

end ConcreteSpec.Core.Exec
