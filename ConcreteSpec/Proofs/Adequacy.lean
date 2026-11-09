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
- Usable termination evidence discharges its trusted premises, covers its independently declared
  inhabited call domain, and excludes infinite execution (`TerminationEvidence.excludes_infinite`).
-/

namespace ConcreteSpec.Core.Exec
open ConcreteSpec.Core

variable {O : Operands} {P : Program O} {ρ : QueryOracle O} {c : Config O}

/-- Helper (inversion). A final conclusion certifies a final configuration independently of
its recorded store. (approved: PENDING) -/
private theorem Conclusion.final_config {f : Final O.Val} {store : O.Store} {pos : Position O}
    (h : Conclusion P ρ (.final f store) pos) : pos.config.final? = some f := by
  cases h with
  | final hf => exact hf

/-- Helper (projection). Derived by erasing instrumentation or inverting an existing semantic witness
(approved: APPR-integ-agy-0dbeba29e).
Helper for constructing a query; this definitional equality is not execution-level contract
evidence. -/
theorem issue_occurrence (events : List O.Event) (p : O.Prim) (args : List O.Val) (site : SiteId)
    (st : O.Store) (issued : List (Query O)) :
    (Query.issue events p args site st issued).occurrence = occurrenceAt issued site := rfl

/-- Helper (arithmetic). Derived list-prefix and per-site counting calculation, rather than
independent execution evidence (approved: APPR-integ-agy-0dbeba29e).
Helper for reasoning about a history append; this is not execution-level contract evidence. -/
theorem issue_next_occurrence (issued : List (Query O)) (site : SiteId) (q : Query O) :
    occurrenceAt (issued ++ [q]) site =
      occurrenceAt issued site + if q.site = site then 1 else 0 := by
  simp [occurrenceAt, List.filter_append]
  split <;> simp_all

/-- Helper (congruence). Equal queries have equal pure-function answers (approved: APPR-integ-agy-0dbeba29e).
Helper for rewriting an oracle answer under query equality; this congruence is not
execution-level contract evidence. -/
theorem answer_deterministic (ρ : QueryOracle O) {q₁ q₂ : Query O} (h : q₁ = q₂) :
    answer ρ q₁ = answer ρ q₂ := congrArg (answer ρ) h

/-- Helper (projection). Derived by erasing instrumentation or inverting an existing semantic witness
(approved: APPR-integ-agy-0dbeba29e).
Helper for unfolding the adapter; this definitional equality is not execution-level contract
evidence. -/
theorem answer_toQueryOracle (ρ₀ : Oracle O) (q : Query O) :
    answer ρ₀.toQueryOracle q = ρ₀ q.events q.prim q.args q.site q.store := rfl

/-- Helper (arithmetic). Derived list-prefix and per-site counting calculation, rather than
independent execution evidence (approved: APPR-integ-agy-0dbeba29e).
Appending the authenticated next query preserves the per-site occurrence invariant. -/
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

/-- Law (subject `runFromQ`).
**Execution-level occurrence invariant.** Every query reported by an actual instrumented run
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

/-- Helper (computation). The empty history has no entries to check (approved: APPR-integ-agy-0dbeba29e). -/
theorem QueryHistoryValid.nil : QueryHistoryValid ([] : List (Query O)) := by
  intro i q h
  simp at h

/-- Helper (projection). Specializes the general evaluator invariant to empty histories (approved:
APPR-integ-agy-0dbeba29e). -/
theorem runQ_queryHistoryValid (fuel : Nat) (c : Config O) :
    QueryHistoryValid (runQ P ρ fuel c).queries :=
  runFromQ_queryHistoryValid fuel [] [] c QueryHistoryValid.nil

/-- Helper (arithmetic). Derived list-prefix and per-site counting calculation, rather than
independent execution evidence (approved: APPR-integ-agy-0dbeba29e).
Splits a per-site prefix count into its old prefix and the queries in the interval. -/
theorem occurrenceAt_take_interval (qs : List (Query O)) (site : SiteId)
    {i j : Nat} (hij : i ≤ j) :
    occurrenceAt (qs.take j) site = occurrenceAt (qs.take i) site +
      occurrenceAt ((qs.drop i).take (j - i)) site := by
  have split : qs.take j = qs.take i ++ (qs.drop i).take (j - i) := by
    rw [← List.take_add, Nat.add_sub_cancel' hij]
  rw [split]
  simp [occurrenceAt, List.filter_append]

/-- Law (subject `runFromQ`).
Between two issued queries at the same site, the occurrence increases by exactly the number
of requests at that site in the intervening half-open interval; other sites contribute zero. -/
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

/-- Helper (arithmetic). An interval containing its first query and no further query at that
site contributes exactly one to the prefix count (approved: APPR-integ-agy-0dbeba29e). -/
theorem occurrenceAt_take_no_other (qs : List (Query O)) {i j : Nat} {q : Query O}
    (hq : qs[i]? = some q)
    (separate : ∀ k r, i < k → k < j → qs[k]? = some r → r.site ≠ q.site) :
    ∀ n, i < n → n ≤ j →
      occurrenceAt (qs.take n) q.site = occurrenceAt (qs.take i) q.site + 1 := by
  intro n
  induction n with
  | zero => intro h; omega
  | succ n ih =>
    intro hin hnj
    rw [List.take_add_one]
    by_cases hi : i = n
    · subst n
      rw [hq]
      simp only [Option.toList_some]
      rw [issue_next_occurrence]
      simp
    · have hin' : i < n := by omega
      cases hn : qs[n]? with
      | none => simpa only [Option.toList_none, List.append_nil] using ih hin' (by omega)
      | some r =>
        simp only [Option.toList_some]
        rw [issue_next_occurrence]
        have ne := separate n r hin' (by omega) hn
        simp [ne, ih hin' (by omega)]

/-- Helper (arithmetic). Two requests to the same site separated only by other sites differ
by exactly one occurrence, regardless of silent machine work or unrelated queries
(approved: APPR-integ-agy-0dbeba29e). -/
theorem runFromQ_interleaved_other_sites (fuel : Nat) (events : List O.Event)
    (issued : List (Query O)) (c : Config O) (valid : QueryHistoryValid issued)
    {i j : Nat} {q q' : Query O} (hij : i < j)
    (hq : (runFromQ P ρ fuel events issued c).queries[i]? = some q)
    (hq' : (runFromQ P ρ fuel events issued c).queries[j]? = some q')
    (site : q.site = q'.site)
    (separate : ∀ k r, i < k → k < j →
      (runFromQ P ρ fuel events issued c).queries[k]? = some r → r.site ≠ q.site) :
    q'.occurrence = q.occurrence + 1 := by
  have history := runFromQ_queryHistoryValid (P := P) (ρ := ρ) fuel events issued c valid
  rw [history j q' hq', ← site, occurrenceAt_take_no_other _ hq separate j hij (Nat.le_refl j),
    ← history i q hq]

/-- Helper (arithmetic). Derived list-prefix and per-site counting calculation, rather than
independent execution evidence (approved: APPR-integ-agy-0dbeba29e).
**Repeated requests at one site advance their occurrences.** These are consecutive queries in
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

/-- Helper (arithmetic). Derived list-prefix and per-site counting calculation, rather than
independent execution evidence (approved: APPR-integ-agy-0dbeba29e).
**Interleaved sites keep separate counters.** A query at another site does not advance the
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

/-- Law (subject `runFromQ`).
Erasing query instrumentation gives C1's original driver when the query-level oracle is C1's
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

/-- Helper (projection). Derived by erasing instrumentation or inverting an existing semantic witness
(approved: APPR-integ-agy-0dbeba29e).
Exact primitive-move characterization under a lifted ordinary oracle, in both directions,
including the successor configuration, events, and queries. This is constructor inversion,
not the independent evaluator request/answer law. -/
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

/-- Helper (computation). Unfolds one issued request into the four evaluator branches. This
computation helper
is used to prove the independent oracle/contract relation in the request correspondence law.
(approved: APPR-integ-agy-0dbeba29e). -/
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

/-- Helper (computation). Every instrumented run extends the query history with which it started.
(approved: APPR-integ-agy-0dbeba29e). -/
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

/-- Helper (computation). One more unit of fuel preserves every query already issued. (approved:
APPR-integ-agy-0dbeba29e). -/
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

/-- Helper (computation). Query histories are prefix-monotone for every increase in fuel. (approved:
APPR-integ-agy-0dbeba29e). -/
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

/-- Helper (computation). The first queries of a run are independent of its fuel: if two budgets both
issue query `i`,
they issue the same query there. (approved: APPR-integ-agy-0dbeba29e). -/
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

/-- Helper (projection). Derived by erasing instrumentation or inverting an existing semantic witness
(approved: APPR-integ-agy-0dbeba29e).
One oracle-checked move is one ordinary labeled step, with the same event contribution. -/
theorem Move.toStep {pos pos' : Position O} (h : Move P ρ pos pos') :
    ∃ l, Step P pos.config l pos'.config ∧ pos'.events = pos.events ++ l.events ∧
      ∃ later, pos'.queries = pos.queries ++ later := by
  cases h with
  | silent hs => exact ⟨.silent, .silent hs, by simp [Label.events], [], by simp⟩
  | @prim c c' events issued p vs site st k a hs ha hallow hk =>
    refine ⟨a.label, .prim hs hallow hk, ?_, [Query.issue events p vs site st issued], rfl⟩
    cases he : a.event <;> simp [Answer.label, Label.events, he]

/-- Helper (projection). Derived by erasing instrumentation or inverting an existing semantic witness
(approved: APPR-integ-agy-0dbeba29e).
An oracle-checked finite path erases to `Steps`; its final histories extend its initial ones. -/
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

/-- Helper (computation). A configuration with an available machine transition is not final.
(approved: APPR-integ-agy-0dbeba29e). -/
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

/-- Law (subject `runFromQ`).
**Soundness.** Every instrumented report has a finite oracle-checked path to a position that
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
        (show ∃ hc : Conclusion P ρ (.final f c.store) (⟨c, events, issued⟩ : Position O),
            ReportedQueries P ρ hc issued from
          ⟨Conclusion.final (pos := ⟨c, events, issued⟩) hf, ReportedQueries.final (pos := ⟨c, events, issued⟩) hf⟩)
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
        (show ∃ hc : Conclusion P ρ (.final f c.store) (⟨c, events, issued⟩ : Position O),
            ReportedQueries P ρ hc issued from
          ⟨Conclusion.final (pos := ⟨c, events, issued⟩) hf, ReportedQueries.final (pos := ⟨c, events, issued⟩) hf⟩)
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

/-- Law (subject `runFromQ`).
An actually issued request is answered at its exact event and query history. Terminal request
outcomes stop at the requesting position with the authenticated terminal query; a continued request
steps by `Move` to an intermediate position and from there by `Moves` to the report's justified
endpoint. -/
theorem runFromQ_request_correspondence {fuel : Nat} {events : List O.Event}
    {issued : List (Query O)} {c : Config O} {p : O.Prim} {vs : List O.Val} {site : SiteId}
    {st : O.Store} {k : Answer O.Val O.Event O.Store → Option (Config O)}
    (hf : c.final? = none) (hs : step P c = some (.request p vs site st k)) :
    RequestResolution P ρ ⟨c, events, issued⟩ p vs site st k
      (runFromQ P ρ (fuel + 1) events issued c) := by
  rw [runFromQ_request hf hs]
  dsimp only
  let q := Query.issue events p vs site st issued
  cases ha : answer ρ q with
  | none =>
    exact RequestResolution.unanswered (pos := ⟨c, events, issued⟩) ha
      (Conclusion.noAnswer hs ha) (ReportedQueries.noAnswer hs ha)
  | some a =>
    cases hl : O.allows p vs st a with
    | false =>
      simpa only [hl, Bool.false_eq_true, ↓reduceIte] using
        RequestResolution.invalid (pos := ⟨c, events, issued⟩) (k := k) ha hl
          (Conclusion.badAnswer hs ha hl) (ReportedQueries.badAnswer hs ha hl)
    | true =>
      simp only [hl, ↓reduceIte]
      cases hk : k a with
      | none =>
        exact RequestResolution.refused (pos := ⟨c, events, issued⟩) ha hl hk
          (Conclusion.stuckContinuation hs ha hl hk)
          (ReportedQueries.stuckContinuation hs ha hl hk)
      | some c' =>
        obtain ⟨n, pos'', hpath, he, hc, hr⟩ :=
          runFromQ_sound (P := P) (ρ := ρ) fuel (events ++ a.event.toList) (issued ++ [q]) c'
        exact RequestResolution.continued (pos := ⟨c, events, issued⟩)
          (pos' := ⟨c', events ++ a.event.toList, issued ++ [q]⟩)
          ha hl hk (.prim hs ha hl hk) hpath he hc hr

/-- Helper (projection). Derived by erasing instrumentation or inverting an existing semantic witness
(approved: APPR-integ-agy-0dbeba29e).
**Soundness for C1's `runFrom`.** Query erasure preserves the finite execution and its ending
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

/-- Helper (computation). Every report extends the event history supplied to `runFrom`. (approved:
APPR-integ-agy-0dbeba29e). -/
theorem runFrom_events_from {ρ₀ : Oracle O} (fuel : Nat) (events : List O.Event) (c : Config O) :
    ∃ tr, (runFrom P ρ₀ fuel events c).events = events ++ tr := by
  obtain ⟨n, pos, hpath, he, hc⟩ := runFrom_sound (P := P) (ρ₀ := ρ₀) fuel events c
  obtain ⟨tr, later, hepos, hq, hsteps⟩ := Moves.toSteps hpath
  refine ⟨tr, ?_⟩
  rw [he, hepos]

/-- Law (subject `runFromQ`).
**Completeness.** Exactly `n` units of fuel suffice for an `n`-step oracle-checked execution to
a final configuration; the report carries the path's complete event and query histories. -/
theorem runFromQ_complete {n : Nat} {pos pos' : Position O} (h : Moves P ρ n pos pos')
    {f : Final O.Val} (hf : pos'.config.final? = some f) :
    runFromQ P ρ n pos.events pos.queries pos.config =
      ⟨pos'.events, pos'.queries, .final f pos'.config.store⟩ := by
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

/-- Helper (projection). Derived by erasing instrumentation or inverting an existing semantic witness
(approved: PENDING).
**Completeness for C1's `runFrom`.** Every finite execution from an empty query history to a
final configuration is reported with exactly its number of steps as fuel. -/
theorem runFrom_complete {ρ₀ : Oracle O} {n : Nat} {events : List O.Event} {c : Config O}
    {pos' : Position O} (h : Moves P ρ₀.toQueryOracle n ⟨c, events, []⟩ pos')
    {f : Final O.Val} (hf : pos'.config.final? = some f) :
    runFrom P ρ₀ n events c = ⟨pos'.events, .final f pos'.config.store⟩ := by
  have hq := runFromQ_complete (P := P) (ρ := ρ₀.toQueryOracle) h hf
  have hr := runFromQ_toReport (P := P) (ρ₀ := ρ₀) n events [] c
  rw [hq] at hr
  simpa [QueryReport.toReport] using hr.symm

/-- Law (subject `runFrom`).
Extra fuel preserves every report other than `outOfFuel`. -/
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

/-- Helper (computation). **Completeness with enough fuel.** Every budget at least the finite path
length reports that
path's final outcome and events. (approved: PENDING). -/
theorem runFrom_complete_le {ρ₀ : Oracle O} {n m : Nat} (hle : n ≤ m)
    {events : List O.Event} {c : Config O} {pos' : Position O}
    (h : Moves P ρ₀.toQueryOracle n ⟨c, events, []⟩ pos')
    {f : Final O.Val} (hf : pos'.config.final? = some f) :
    runFrom P ρ₀ m events c = ⟨pos'.events, .final f pos'.config.store⟩ := by
  have hexact := runFrom_complete (P := P) (ρ₀ := ρ₀) h hf
  have hnot : ∀ c', (runFrom P ρ₀ n events c).ending ≠ .outOfFuel c' := by
    rw [hexact]
    simp
  rw [runFrom_mono (P := P) (ρ₀ := ρ₀) n m hle events c hnot]
  exact hexact

/-- Law (subject `runFrom`).
Increasing fuel never retracts or changes an event already reported. -/
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

/-- Helper (computation). The events of fuel `n` are a prefix of those of fuel `n + 1`. (approved:
APPR-integ-agy-0dbeba29e). -/
theorem runFrom_trace_prefix {ρ₀ : Oracle O} (n : Nat) (events : List O.Event) (c : Config O) :
    (runFrom P ρ₀ n events c).events <+: (runFrom P ρ₀ (n + 1) events c).events :=
  runFrom_trace_prefix_le n (n + 1) (Nat.le_add_right n 1) events c

/-- Helper (computation). A final report has an ordinary finite `Steps` witness ending in exactly that
final
configuration, with the reported events. (approved: PENDING). -/
theorem runFrom_final_sound {ρ₀ : Oracle O} {fuel : Nat} {events : List O.Event} {c : Config O}
    {f : Final O.Val} {finalStore : O.Store} (h : (runFrom P ρ₀ fuel events c).ending = .final f finalStore) :
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

/-- Helper (computation). If `n` steps exhaust the budget at `pos`, then `n + m` steps are exactly `m`
steps from `pos`.
This is the compatibility operation used by the infinite-limit theorem. (approved: APPR-integ-agy-0dbeba29e). -/
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

/-- Helper (computation). A zero-step `outOfFuel` report identifies its position exactly. (approved:
APPR-integ-agy-0dbeba29e). -/
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

/-- Helper (computation). A move between two nonfinal positions determines the one-unit report from
its source. (approved: APPR-integ-agy-0dbeba29e). -/
theorem Move.runFromQ_one {pos pos' : Position O} (hm : Move P ρ pos pos')
    (hf : pos.config.final? = none) (hf' : pos'.config.final? = none) :
    runFromQ P ρ 1 pos.events pos.queries pos.config =
      ⟨pos'.events, pos'.queries, .outOfFuel pos'.config⟩ := by
  cases hm with
  | silent hs => simp [runFromQ, hf, hs, hf']
  | prim hs ha hallow hk => simp [runFromQ, hf, hs, ha, hallow, hk, hf']

/-- Helper (computation). A move's source is not final. (approved: APPR-integ-agy-0dbeba29e). -/
theorem Move.not_final {pos pos' : Position O} (hm : Move P ρ pos pos') :
    pos.config.final? = none := by
  cases hm with
  | silent hs => exact final?_none_of_step hs
  | prim hs _ _ _ => exact final?_none_of_step hs

/-- Law (subject `runFromQ`).
Two consecutive fuel-exhausted evaluator reports determine one machine transition between
their residual positions. -/
theorem move_of_fuel_succ {n : Nat} {events : List O.Event} {issued : List (Query O)}
    {c : Config O}
    (h0 : (runFromQ P ρ n events issued c).isOutOfFuel = true)
    (h1 : (runFromQ P ρ (n + 1) events issued c).isOutOfFuel = true) :
    Move P ρ
      ((runFromQ P ρ n events issued c).position c)
      ((runFromQ P ρ (n + 1) events issued c).position c) := by
  generalize hr0 : runFromQ P ρ n events issued c = r0 at h0 ⊢
  generalize hr1 : runFromQ P ρ (n + 1) events issued c = r1 at h1 ⊢
  cases r0 with
  | mk ev0 q0 end0 =>
    cases end0 with
    | final _ | stuck _ | noAnswer _ | badAnswer _ =>
      simp [QueryReport.isOutOfFuel] at h0
    | outOfFuel c0 =>
      cases r1 with
      | mk ev1 q1 end1 =>
        cases end1 with
        | final _ | stuck _ | noAnswer _ | badAnswer _ =>
          simp [QueryReport.isOutOfFuel] at h1
        | outOfFuel c1 =>
          have hadd := runFromQ_add_of_outOfFuel (P := P) (ρ := ρ) (pos := ⟨c0, ev0, q0⟩) hr0 1
          have h : runFromQ P ρ 1 ev0 q0 c0 = ⟨ev1, q1, .outOfFuel c1⟩ :=
            hadd.symm.trans hr1
          simp only [QueryReport.position]
          cases hf : c0.final? with
          | some f => simp [runFromQ, hf] at h
          | none =>
            cases hs : step P c0 with
            | none => simp [runFromQ, hf, hs] at h
            | some out =>
              cases out with
              | next c' =>
                simp only [runFromQ, hf, hs] at h
                have hp := eq_of_runFromQ_zero_out (P := P) (ρ := ρ) (pos := ⟨c1, ev1, q1⟩) h
                cases hp
                exact .silent hs
              | request p vs site st k =>
                cases ha : answer ρ (Query.issue ev0 p vs site st q0) with
                | none => simp [runFromQ, hf, hs, ha] at h
                | some a =>
                  cases hallow : O.allows p vs st a with
                  | false => simp [runFromQ, hf, hs, ha, hallow] at h
                  | true =>
                    cases hk : k a with
                    | none => simp [runFromQ, hf, hs, ha, hallow, hk] at h
                    | some c' =>
                      simp only [runFromQ, hf, hs, ha, hallow, hk] at h
                      have hp := eq_of_runFromQ_zero_out (P := P) (ρ := ρ) (pos := ⟨c1, ev1, q1⟩) h
                      cases hp
                      exact .prim hs ha hallow hk

/-- Helper (projection). Derived by erasing instrumentation or inverting an existing semantic witness
(approved: APPR-integ-agy-0dbeba29e).
No position of an infinite execution is final. -/
theorem InfiniteExecution.not_final (X : InfiniteExecution P ρ c) (n : Nat) :
    (X.states n).config.final? = none :=
  (X.step n).not_final

/-- Law (subject `runQ`).
Every position of a genuine infinite execution is exactly the
corresponding evaluator prefix, including its events, queries, and residual configuration. -/
theorem InfiniteExecution.evaluator_prefix (X : InfiniteExecution P ρ c) (n : Nat) :
    runQ P ρ n c =
      ⟨(X.states n).events, (X.states n).queries, .outOfFuel (X.states n).config⟩ := by
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

/-- Package pointwise evaluator adequacy as a compatible fuel limit. The proof of
`InfiniteExecution.evaluator_prefix`, rather than the representation round trip, is the evidence. -/
def InfiniteExecution.toFuelLimit (X : InfiniteExecution P ρ c) : FuelLimit P ρ c :=
  ⟨X.states, X.evaluator_prefix⟩

/-- Compatible fuel prefixes correspond to an infinite maximal execution. -/
def FuelLimit.toInfiniteExecution (L : FuelLimit P ρ c) : InfiniteExecution P ρ c := by
  refine ⟨L.states, ?_, ?_⟩
  · apply eq_of_runFromQ_zero_out (P := P) (ρ := ρ)
    simpa [runQ] using L.prefixes 0
  · intro n
    have h0 : (runFromQ P ρ n [] [] c).isOutOfFuel = true := by
      have hp := L.prefixes n
      simpa [runQ, QueryReport.isOutOfFuel] using congrArg QueryReport.isOutOfFuel hp
    have h1 : (runFromQ P ρ (n + 1) [] [] c).isOutOfFuel = true := by
      have hp := L.prefixes (n + 1)
      simpa [runQ, QueryReport.isOutOfFuel] using congrArg QueryReport.isOutOfFuel hp
    have hm := move_of_fuel_succ (P := P) (ρ := ρ) (c := c) h0 h1
    have hp0 : (runFromQ P ρ n [] [] c).position c = L.states n := by
      have hp := L.prefixes n
      change runFromQ P ρ n [] [] c = _ at hp
      rw [hp]
      rfl
    have hp1 : (runFromQ P ρ (n + 1) [] [] c).position c = L.states (n + 1) := by
      have hp := L.prefixes (n + 1)
      change runFromQ P ρ (n + 1) [] [] c = _ at hp
      rw [hp]
      rfl
    rwa [hp0, hp1] at hm

/-- Representation lemma. This repackages the same states and proof fields; it supplies no independent
behavioral evidence (approved: APPR-integ-agy-0dbeba29e).
Infinite executions and compatible fuel limits are two presentations of the same behavior. -/
theorem infinite_iff_fuelLimit : Nonempty (InfiniteExecution P ρ c) ↔ Nonempty (FuelLimit P ρ c) :=
  ⟨fun ⟨X⟩ => ⟨X.toFuelLimit⟩, fun ⟨L⟩ => ⟨L.toInfiniteExecution⟩⟩

/-- Representation lemma. This repackages the same states and proof fields; it supplies no independent
behavioral evidence (approved: APPR-integ-agy-0dbeba29e).
Truncating an infinite execution and reconstructing it returns the original execution. -/
theorem InfiniteExecution.toFuelLimit_toInfiniteExecution (X : InfiniteExecution P ρ c) :
    X.toFuelLimit.toInfiniteExecution = X := rfl

/-- Representation lemma. This repackages the same states and proof fields; it supplies no independent
behavioral evidence (approved: APPR-integ-agy-0dbeba29e).
Reconstructing a fuel limit and truncating it returns the original limit. -/
theorem FuelLimit.toInfiniteExecution_toFuelLimit (L : FuelLimit P ρ c) :
    L.toInfiniteExecution.toFuelLimit = L := rfl

/-- Helper (computation). Compatible fuel limits from one initial configuration have the same
positions. (approved: APPR-integ-agy-0dbeba29e). -/
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

/-- Helper (computation). There is at most one compatible fuel limit for a fixed query-level oracle
and initial
configuration. (approved: APPR-integ-agy-0dbeba29e). -/
theorem FuelLimit.unique (L K : FuelLimit P ρ c) : L = K := by
  cases L with
  | mk ls lp =>
    cases K with
    | mk ks kp =>
      have hs : ls = ks := fuelLimit_states_unique ⟨ls, lp⟩ ⟨ks, kp⟩
      cases hs
      rfl

/-- Helper (congruence). Equality of fuel limits transports back to equality of executions;
this representation calculation is separate from semantic determinism. (approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.unique_execution (X Y : InfiniteExecution P ρ c) : X = Y := by
  have h : X.toFuelLimit = Y.toFuelLimit := FuelLimit.unique _ _
  have hxy : X = Y := calc
    X = X.toFuelLimit.toInfiniteExecution := (toFuelLimit_toInfiniteExecution X).symm
    _ = Y.toFuelLimit.toInfiniteExecution :=
      congrArg (fun L : FuelLimit P ρ c => L.toInfiniteExecution) h
    _ = Y := toFuelLimit_toInfiniteExecution Y
  exact hxy

/-- Law (subject `Move`).
Under the fixed oracle, any two state sequences starting at the initial position and following
the execution relation coincide. The relation, rather than a representation getter, is the subject. -/
theorem InfiniteExecution.unique : ExecutionUnique (Move P ρ) ⟨c, [], []⟩ := by
  rintro ⟨xs, ys, hx, hy, hxs, hys, different⟩
  have h := fuelLimit_states_unique
    (InfiniteExecution.toFuelLimit ⟨xs, hx, hxs⟩)
    (InfiniteExecution.toFuelLimit ⟨ys, hy, hys⟩)
  exact different h

/-- Helper (projection). Derived by erasing instrumentation or inverting an existing semantic witness
(approved: APPR-integ-agy-0dbeba29e).
One transition extends, and never changes, the raw event history. -/
theorem Move.events_prefix {pos pos' : Position O} (h : Move P ρ pos pos') :
    pos.events <+: pos'.events := by
  cases h with
  | silent => exact ⟨[], by simp⟩
  | prim => exact ⟨_, rfl⟩

/-- Helper (arithmetic). Derived list-prefix and per-site counting calculation, rather than
independent execution evidence (approved: APPR-integ-agy-0dbeba29e).
Every later position of an infinite execution extends every earlier raw history. -/
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

/-- Helper (arithmetic). Derived list-prefix and per-site counting calculation, rather than
independent execution evidence (approved: APPR-integ-agy-0dbeba29e).
Adapter projection preserves the execution's history-prefix order. -/
theorem InfiniteExecution.projected_prefix {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) {m n : Nat} (h : m ≤ n) :
    A.project (X.states m).events <+: A.project (X.states n).events := by
  obtain ⟨rest, hrest⟩ := X.events_prefix h
  exact ⟨A.project rest, by rw [← Observe.ExecAdapter.project_append, hrest]⟩

/-- Helper (arithmetic). Derived list-prefix and per-site counting calculation, rather than
independent execution evidence (approved: APPR-integ-agy-0dbeba29e).
A prefix determines every list entry that lies in the shorter list. -/
theorem getElem?_eq_of_prefix {α : Type} {xs ys : List α} (h : xs <+: ys) {i : Nat}
    (hi : i < xs.length) : ys[i]? = xs[i]? := by
  obtain ⟨rest, rfl⟩ := h
  exact List.getElem?_append_left hi

/-- Helper (computation). The canonical eventually silent C6 execution has exactly `events.take n`
events after `n`
steps. (approved: APPR-integ-agy-0dbeba29e). -/
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

/-- Helper (computation). An emitting C6 execution has exactly the first `n` trace events after `n`
steps. (approved: APPR-integ-agy-0dbeba29e). -/
theorem visiblePrefix_emitTrace {S : Observe.EventSignature} (trace : Observe.InfiniteTrace S)
    (n : Nat) : Observe.visiblePrefix (fun i => Observe.Step.emit (trace i)) n =
      (List.range n).map trace := by
  induction n with
  | zero => rfl
  | succ n ih => simp [Observe.visiblePrefix_succ, ih, Observe.Step.event?, List.range_succ]

/-- Helper (arithmetic). Derived list-prefix and per-site counting calculation, rather than
independent execution evidence (approved: APPR-integ-agy-0dbeba29e).
A bounded monotone sequence of natural numbers is eventually constant. -/
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

/-- Helper (computation). Cofinality through the actual projected histories relates any two matching
streams,
even with different silent-step schedules. This is a relational composition helper. (approved:
APPR-integ-agy-0dbeba29e). -/
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

/-- Helper (computation). Prefix concatenation is monotone in the block count.
(approved: APPR-integ-agy-0dbeba29e). -/
theorem blockPrefix_prefix {S : Observe.EventSignature} (blocks : Nat → List (Observe.Step S))
    {m n : Nat} (h : m ≤ n) : blockPrefix blocks m <+: blockPrefix blocks n := by
  obtain ⟨d, rfl⟩ := Nat.exists_eq_add_of_le h
  induction d with
  | zero => exact ⟨[], by simp⟩
  | succ d ih =>
    obtain ⟨rest, hr⟩ := ih (by omega)
    exact ⟨rest ++ blocks (m + d), by simp [blockPrefix, ← hr, List.append_assoc]⟩

/-- Helper (arithmetic). Nonempty blocks embed machine positions strictly into step indices.
(approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.boundary_growth {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n : Nat) :
    X.observeBoundary A n < X.observeBoundary A (n + 1) := by
  have hb : 0 < (X.observeBlock A n).length := by
    unfold InfiniteExecution.observeBlock
    split <;> simp
  simp only [InfiniteExecution.observeBoundary, blockPrefix, List.length_append]
  omega

/-- Helper (arithmetic). Every machine transition consumes at least one C6 step.
(approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.boundary_ge {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n : Nat) :
    n ≤ X.observeBoundary A n := by
  induction n with
  | zero => exact Nat.zero_le _
  | succ n ih => have := X.boundary_growth A n; omega

/-- Helper (arithmetic). The boundary map strictly preserves the machine-position order, hence
is an embedding rather than a cofinal rescheduling. (approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.boundary_strict {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) {m n : Nat} (h : m < n) :
    X.observeBoundary A m < X.observeBoundary A n := by
  induction n with
  | zero => omega
  | succ n ih =>
    have growth := X.boundary_growth A n
    by_cases he : m = n
    · subst m; exact growth
    · have earlier := ih (by omega); omega

/-- Helper (arithmetic). Distinct machine positions have distinct embedded C6 boundaries.
(approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.boundary_injective {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) {m n : Nat}
    (h : X.observeBoundary A m = X.observeBoundary A n) : m = n := by
  by_cases hmn : m < n
  · have := X.boundary_strict A hmn; omega
  · by_cases hnm : n < m
    · have := X.boundary_strict A hnm; omega
    · omega

/-- Helper (computation). Flattening agrees with each finite concatenation at every index in it.
(approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.flatten_get {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n i : Nat)
    (hi : i < X.observeBoundary A n) :
    X.toObserveSteps A i = (blockPrefix (X.observeBlock A) n)[i]?.getD .silent := by
  unfold InfiniteExecution.toObserveSteps
  by_cases hn : n ≤ i + 1
  · rw [getElem?_eq_of_prefix (blockPrefix_prefix _ hn) hi]
  · have hge := X.boundary_ge A (i + 1)
    have hlt : i < (blockPrefix (X.observeBlock A) (i + 1)).length := by
      change i < X.observeBoundary A (i + 1); omega
    rw [getElem?_eq_of_prefix (blockPrefix_prefix _ (show i + 1 ≤ n by omega)) hlt]

/-- Helper (computation). At an embedded boundary, the flattened steps are precisely the finite
concatenation of all preceding blocks. (approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.flatten_boundary {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n : Nat) :
    (List.range (X.observeBoundary A n)).map (X.toObserveSteps A) =
      blockPrefix (X.observeBlock A) n := by
  apply List.ext_getElem
  · simp [InfiniteExecution.observeBoundary]
  · intro i hi hj
    simp only [List.getElem_map, List.getElem_range]
    rw [X.flatten_get A n i (by simpa using hi), List.getElem?_eq_getElem hj]
    rfl

/-- Helper (computation). A block's visible events are its transition's projected difference.
(approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.visible_block {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n : Nat) :
    Observe.visible (X.observeBlock A n) = X.transitionEvents A n := by
  unfold InfiniteExecution.observeBlock
  split <;> simp_all [Observe.visible, Observe.Step.event?, List.filterMap_map, Function.comp_def]

/-- Helper (arithmetic). A projected prefix decomposes into its old history and the current
transition's events. (approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.transition_append {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n : Nat) :
    A.project (X.states (n + 1)).events =
      A.project (X.states n).events ++ X.transitionEvents A n := by
  obtain ⟨rest, hr⟩ := X.projected_prefix A (Nat.le_succ n)
  simp [InfiniteExecution.transitionEvents, ← hr]

/-- Helper (inversion). A silent machine transition retains the event history.
(approved: APPR-integ-agy-0dbeba29e). -/
theorem Move.silent_events {pos pos' : Position O} {cNext : Config O}
    (move : Move P ρ pos pos') (silent : step P pos.config = some (.next cNext)) :
    pos'.events = pos.events := by
  cases move with
  | silent => rfl
  | prim hs _ _ _ => rw [silent] at hs; cases hs

/-- Helper (computation). Projected differences recover exactly the raw events appended by this
transition, including multi-event projections and erasure. (approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.transitionEvents_eq {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n : Nat)
    (events : List O.Event)
    (h : (X.states (n + 1)).events = (X.states n).events ++ events) :
    X.transitionEvents A n = A.project events := by
  simp [InfiniteExecution.transitionEvents, h, Observe.ExecAdapter.project_append]

/-- Helper (computation). Silent and erased transitions occupy a singleton silent block.
(approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.observeBlock_erased {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n : Nat)
    (empty : X.transitionEvents A n = []) : X.observeBlock A n = [.silent] := by
  simp [InfiniteExecution.observeBlock, empty]

/-- Helper (inversion). A silent machine transition maps to a silent block, regardless of the
adapter's expansion of earlier transitions. (approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.observeBlock_silent {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n : Nat)
    {cNext : Config O} (silent : Exec.step P (X.states n).config = some (.next cNext)) :
    X.observeBlock A n = [.silent] := by
  have he := (X.step n).silent_events silent
  apply X.observeBlock_erased A n
  simp [InfiniteExecution.transitionEvents, he]

/-- Helper (computation). Concatenated blocks recover exactly the projected machine history.
(approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.visible_blocks {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n : Nat) :
    Observe.visible (blockPrefix (X.observeBlock A) n) = A.project (X.states n).events := by
  induction n with
  | zero => simp [blockPrefix, Observe.visible, X.initial, Observe.ExecAdapter.project]
  | succ n ih =>
    rw [blockPrefix]
    simp only [Observe.visible, List.filterMap_append] at *
    rw [ih]
    change _ ++ Observe.visible (X.observeBlock A n) = _
    rw [X.visible_block A n, ← X.transition_append A n]

/-- Helper (computation). The explicit boundary embedding aligns flattened visible prefixes with
machine histories. (approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.visiblePrefix_toObserveSteps {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n : Nat) :
    Observe.visiblePrefix (X.toObserveSteps A) (X.observeBoundary A n) =
      A.project (X.states n).events := by
  unfold Observe.visiblePrefix
  rw [X.flatten_boundary A n, X.visible_blocks A n]

/-- Helper (computation). Cutting the flattened stream at successive embedded boundaries gives
exactly that transition's block, including its silent step when erased. (approved: APPR-integ-agy-0dbeba29e). -/
theorem InfiniteExecution.flatten_interval {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n : Nat) :
    (((List.range (X.observeBoundary A (n + 1))).map (X.toObserveSteps A)).drop
      (X.observeBoundary A n)) = X.observeBlock A n := by
  rw [X.flatten_boundary A (n + 1)]
  simp [blockPrefix, InfiniteExecution.observeBoundary]

/-- Law (subject `InfiniteExecution.toObserveSteps`).
The flattened C6 stream contains exactly each transition's events inside its own boundary
interval. This is stronger than cofinality: events may not cross a machine boundary. -/
theorem InfiniteExecution.toObserveSteps_blocks {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n : Nat) :
    Observe.visible (((List.range (X.observeBoundary A (n + 1))).map (X.toObserveSteps A)).drop
      (X.observeBoundary A n)) = X.transitionEvents A n := by
  rw [X.flatten_interval A n]
  exact X.visible_block A n

/-- Law (subject `InfiniteExecution.toObserveSteps`).
Every infinite execution matches its block-flattened C6 stream in both prefix directions. -/
theorem InfiniteExecution.toObserveSteps_matches {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) :
    MatchesProjectedHistory A X (X.toObserveSteps A) := by
  constructor
  · intro n
    exact ⟨X.observeBoundary A n, [], by rw [X.visiblePrefix_toObserveSteps]; simp⟩
  · intro m
    obtain ⟨rest, hr⟩ := Observe.visiblePrefix_prefix (X.toObserveSteps A) (X.boundary_ge A m)
    exact ⟨m, rest, by rw [X.visiblePrefix_toObserveSteps] at hr; exact hr⟩

/-- Law (subject `InfiniteExecution.projectedTrace`).
Projecting the actual execution yields a complete C6 trace at each machine position. The proof
follows the execution relation and the adapter's answer contract, independently of the evaluator. -/
theorem InfiniteExecution.projectedTrace_valid {S : Observe.EventSignature}
    {C : Observe.OperationContract S} {input : C.Input} {ρ₀ : Oracle O}
    {A : Observe.ExecAdapter O S} (X : InfiniteExecution P ρ₀.toQueryOracle c)
    (wellFormed : Observe.ExecAdapter.WellFormed A C input ρ₀) (n : Nat) :
    Observe.CompleteTrace C input (X.projectedTrace A n) := by
  have extend : ∀ {pos pos' : Position O}, Move P ρ₀.toQueryOracle pos pos' →
      Observe.CompleteTrace C input (A.project pos.events) →
      Observe.CompleteTrace C input (A.project pos'.events) := by
    intro pos pos' move valid
    cases move with
    | silent => exact valid
    | @prim c c' events issued p vs site st k a hs ha hl hk =>
      have answered : ρ₀ events p vs site st = some a := ha
      rw [Observe.ExecAdapter.project_append]
      cases typed : A.request p vs site st with
      | none =>
        obtain ⟨outputs, projection⟩ := wellFormed.internal answered typed
        rw [projection]
        exact valid.outputs outputs
      | some q =>
        obtain ⟨before, r, after, projection, beforeOutputs, afterOutputs, matching,
          allows⟩ := wellFormed.answered answered typed
        rw [projection]
        exact Observe.ValidTrace.answer_projection valid beforeOutputs afterOutputs matching allows
  change Observe.CompleteTrace C input (A.project (X.states n).events)
  induction n with
  | zero =>
    rw [X.initial]
    exact .idle
  | succ n ih => exact extend (X.step n) ih

/-- Helper (projection). This notation specialization of `InfiniteExecution.projectedTrace_valid`
uses the adapter directly on the raw history; it supplies no independent semantic evidence.
(approved: APPR-integ-agy-0dbeba29e).
Every finite prefix of an infinite execution under a well-formed adapter and C1 oracle has a
complete valid C6 event trace. -/
theorem InfiniteExecution.projected_completeTrace {S : Observe.EventSignature}
    {C : Observe.OperationContract S} {input : C.Input} {ρ₀ : Oracle O}
    {A : Observe.ExecAdapter O S} (X : InfiniteExecution P ρ₀.toQueryOracle c)
    (wellFormed : Observe.ExecAdapter.WellFormed A C input ρ₀) (n : Nat) :
    Observe.CompleteTrace C input (A.project (X.states n).events) :=
  X.projectedTrace_valid wellFormed n

/-- Law (subject `InfiniteExecution.toObserveSteps`).
For a well-formed execution adapter, an infinite execution with finitely many projected events
stabilizes to a complete valid C6 trace `events`, and its induced C6 step stream has
`.finiteDivergence events` as its full C6 observation (and any other history-matching C6 stream has
the same observation). -/
theorem finite_projected_observation {S : Observe.EventSignature}
    {C : Observe.OperationContract S} {input : C.Input} {ρ₀ : Oracle O}
    {A : Observe.ExecAdapter O S} {X : InfiniteExecution P ρ₀.toQueryOracle c}
    (wellFormed : Observe.ExecAdapter.WellFormed A C input ρ₀)
    (hf : FiniteEvents A X) :
    ∃ events,
      (∃ start, ∀ n, start ≤ n → A.project (X.states n).events = events) ∧
      Observe.CompleteTrace C input events ∧
      Observe.HasFullObservation (.infinite (X.toObserveSteps A)) (.finiteDivergence events) ∧
      ∀ steps, MatchesProjectedHistory A X steps →
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
  have hstable : ∀ n, start ≤ n → A.project (X.states n).events = events := by
    intro n hs
    have sameLength := stable n hs
    obtain ⟨rest, hr⟩ := X.projected_prefix A hs
    have hnil : rest = [] := by
      have hl := congrArg List.length hr
      simp only [List.length_append] at hl
      change (A.project (X.states n).events).length =
        (A.project (X.states start).events).length at sameLength
      exact List.eq_nil_of_length_eq_zero (by omega)
    simpa [events, hnil] using hr.symm
  have hpref_events : ∀ n, A.project (X.states n).events <+: events := by
    intro n
    by_cases h : n ≤ start
    · exact X.projected_prefix A h
    · rw [hstable n (Nat.le_of_not_ge h)]
      exact List.prefix_rfl
  have hm := X.toObserveSteps_matches A
  have hobs : Observe.HasFullObservation (.infinite (X.toObserveSteps A))
      (.finiteDivergence events) := by
    constructor
    · intro m
      obtain ⟨n, rest₁, hn⟩ := hm.2 m
      obtain ⟨rest₂, hev⟩ := hpref_events n
      refine ⟨events.length, rest₁ ++ rest₂, ?_⟩
      rw [visiblePrefix_finiteDivergenceSteps, List.take_of_length_le (Nat.le_refl _),
        ← hev, hn, List.append_assoc]
    · intro m
      obtain ⟨k, rest, hk⟩ := hm.1 start
      refine ⟨k, events.drop m ++ rest, ?_⟩
      rw [visiblePrefix_finiteDivergenceSteps, ← List.append_assoc, List.take_append_drop, hk]
  refine ⟨events, ⟨start, hstable⟩, X.projected_completeTrace wellFormed start, hobs, ?_⟩
  intro steps hs
  exact Observe.SameInfiniteObservation.trans (hs.sameObservation hm) hobs

/-- Law (subject `InfiniteExecution.toObserveSteps`).
For a well-formed execution adapter, an infinite execution with infinitely many projected events
determines an infinite C6 trace `trace` whose prefixes are complete valid C6 traces and match every
projected position history, and its induced C6 step stream has `.infiniteDivergence trace` as its
full C6 observation (and any other history-matching C6 stream has the same observation). -/
theorem infinite_projected_observation {S : Observe.EventSignature}
    {C : Observe.OperationContract S} {input : C.Input} {ρ₀ : Oracle O}
    {A : Observe.ExecAdapter O S} {X : InfiniteExecution P ρ₀.toQueryOracle c}
    (wellFormed : Observe.ExecAdapter.WellFormed A C input ρ₀)
    (hi : InfiniteEvents A X) :
    ∃ trace : Observe.InfiniteTrace S,
      (∀ n, A.project (X.states n).events =
        (List.range (A.project (X.states n).events).length).map trace ∧
        Observe.CompleteTrace C input (A.project (X.states n).events)) ∧
      Observe.HasFullObservation (.infinite (X.toObserveSteps A)) (.infiniteDivergence trace) ∧
      ∀ steps, MatchesProjectedHistory A X steps →
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
  have hm := X.toObserveSteps_matches A
  have hobs : Observe.HasFullObservation (.infinite (X.toObserveSteps A))
      (.infiniteDivergence trace) := by
    constructor
    · intro m
      obtain ⟨n, rest, hn⟩ := hm.2 m
      refine ⟨(A.project (X.states n).events).length, rest, ?_⟩
      rw [visiblePrefix_emitTrace, ← history_eq_range n, hn]
    · intro m
      obtain ⟨k, rest, hk⟩ := hm.1 (first m)
      refine ⟨k, (A.project (X.states (first m)).events).drop m ++ rest, ?_⟩
      rw [visiblePrefix_emitTrace, take_eq_range _ _ (Nat.le_of_lt (first_spec m)),
        ← List.append_assoc, List.take_append_drop, hk]
  refine ⟨trace, fun n => ⟨history_eq_range n, X.projected_completeTrace wellFormed n⟩, hobs, ?_⟩
  intro steps hs
  exact Observe.SameInfiniteObservation.trans (hs.sameObservation hm) hobs

/-- Helper (arithmetic). Derived list-prefix and per-site counting calculation, rather than
independent execution evidence (approved: APPR-integ-agy-0dbeba29e).
Every infinite execution has either finitely many or infinitely many projected events; these
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

/-- Helper (arithmetic). Derived list-prefix and per-site counting calculation, rather than
independent execution evidence (approved: APPR-integ-agy-0dbeba29e).
One infinite execution cannot have both finitely many and infinitely many projected events. -/
theorem finite_not_infinite_events {S : Observe.EventSignature} {A : Observe.ExecAdapter O S}
    {X : InfiniteExecution P ρ c} (hf : FiniteEvents A X) (hi : InfiniteEvents A X) : False := by
  obtain ⟨bound, hb⟩ := hf
  obtain ⟨n, hn⟩ := hi bound
  exact Nat.not_lt_of_ge (hb n) hn

/-- Law (subject `InfiniteInteraction`).
External silent divergence excludes external infinite interaction on the same run. -/
theorem silentDivergence_not_infiniteInteraction {S : Observe.EventSignature}
    (A : Observe.ExecAdapter O S) (hs : SilentDivergence P A ρ c) :
    ¬ InfiniteInteraction P A ρ c := by
  rintro ⟨Y, hi⟩
  obtain ⟨X, hf⟩ := hs
  have hxy : X = Y := InfiniteExecution.unique_execution X Y
  subst Y
  exact finite_not_infinite_events hf hi

/-- Helper (arithmetic). Derived list-prefix and per-site counting calculation, rather than
independent execution evidence (approved: APPR-integ-agy-0dbeba29e).
The raw classification has the same finite/infinite dichotomy, explicitly internally. -/
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

/-- Law (subject `InfiniteExecution`).
A final report excludes an infinite execution from the same initial configuration. -/
theorem not_infinite_of_final {ρ₀ : Oracle O} {fuel : Nat} {c : Config O} {f : Final O.Val} {finalStore : O.Store}
    (h : (run P ρ₀ fuel c).ending = .final f finalStore) :
    ¬ Nonempty (InfiniteExecution P ρ₀.toQueryOracle c) := by
  rintro ⟨X⟩
  have hp := X.toFuelLimit.prefixes fuel
  change runFromQ P ρ₀.toQueryOracle fuel [] [] c = _ at hp
  have hr := runFromQ_toReport (P := P) (ρ₀ := ρ₀) fuel [] [] c
  rw [hp] at hr
  change (runFrom P ρ₀ fuel [] c).ending = .final f finalStore at h
  simp [QueryReport.toReport] at hr
  rw [← hr] at h
  cases h

/-- Law (subject `InfiniteExecution`).
An endlessly blocked `noAnswer` report excludes an infinite execution from the same initial
configuration. -/
theorem not_infinite_of_noAnswer {ρ₀ : Oracle O} {fuel : Nat} {c blocked : Config O}
    (h : (run P ρ₀ fuel c).ending = .noAnswer blocked) :
    ¬ Nonempty (InfiniteExecution P ρ₀.toQueryOracle c) := by
  rintro ⟨X⟩
  have hp := X.toFuelLimit.prefixes fuel
  change runFromQ P ρ₀.toQueryOracle fuel [] [] c = _ at hp
  have hr := runFromQ_toReport (P := P) (ρ₀ := ρ₀) fuel [] [] c
  rw [hp] at hr
  change (runFrom P ρ₀ fuel [] c).ending = .noAnswer blocked at h
  simp [QueryReport.toReport] at hr
  rw [← hr] at h
  cases h

/-- Law (subject `run`).
A blocked request remains blocked at every larger budget; it never turns into divergence or a
program outcome merely because more fuel is supplied. -/
theorem endlesslyBlocked_of_noAnswer {ρ₀ : Oracle O} {fuel : Nat} {c blocked : Config O}
    (h : (run P ρ₀ fuel c).ending = .noAnswer blocked) :
    ∀ n, fuel ≤ n → (run P ρ₀ n c).ending = .noAnswer blocked := by
  change (runFrom P ρ₀ fuel [] c).ending = .noAnswer blocked at h
  intro n hle
  have hm : run P ρ₀ n c = run P ρ₀ fuel c := by
    apply runFrom_mono fuel n hle [] c
    intro c'
    rw [h]
    simp
  exact (congrArg Report.ending hm).trans h

/-- Law (subject `run`).
Every admitted call has a successful lookup and entry and reaches a final outcome. The same
outcome is reported at every fuel budget above the certificate's witness budget. -/
theorem TerminationEvidence.all_allowed_runs {domain : CallDomain O} (E : TerminationEvidence O P domain)
     (ρ : Oracle O) (args : List O.Val) (st : O.Store)
    (ha : domain.calls ρ args st) (m : Nat) (hm : E.conditional.fuel E.premises ρ args st ≤ m) :
    ∃ fn c f finalStore, AList.lookup P.fns domain.function = some fn ∧ enter fn args st = some c ∧
      (run P ρ m c).ending = .final f finalStore := by
  obtain ⟨fn, c, f, finalStore, hf, he, hfinal⟩ := E.conditional.terminates E.premises ρ args st ha
  refine ⟨fn, c, f, finalStore, hf, he, ?_⟩
  have hnot : ∀ c', (run P ρ (E.conditional.fuel E.premises ρ args st) c).ending ≠ .outOfFuel c' := by
    rw [hfinal]
    simp
  have hmono : run P ρ m c = run P ρ (E.conditional.fuel E.premises ρ args st) c := by
    apply runFrom_mono (P := P) (ρ₀ := ρ) (E.conditional.fuel E.premises ρ args st) m hm [] c hnot
  exact (congrArg Report.ending hmono).trans hfinal

/-- Law (subject `InfiniteExecution`).
Per-function termination evidence excludes divergence from every successful admitted entry
configuration. -/
theorem TerminationEvidence.excludes_infinite {domain : CallDomain O} (E : TerminationEvidence O P domain)
     (ρ : Oracle O) (args : List O.Val) (st : O.Store)
    (fn : Fn O) (c : Config O) (hf : AList.lookup P.fns domain.function = some fn)
    (ha : domain.calls ρ args st) (he : enter fn args st = some c) :
    ¬ Nonempty (InfiniteExecution P ρ.toQueryOracle c) := by
  obtain ⟨fn', c', f, finalStore, hf', he', hfinal⟩ := E.conditional.terminates E.premises ρ args st ha
  rw [hf] at hf'
  cases hf'
  rw [he] at he'
  cases he'
  exact not_infinite_of_final hfinal

/-! ### Fuel exhaustion is only a budget report -/

namespace Demo

/-- A total fixture environment for the observation examples. Well-shaped demo operations use
C1's original environment; other requests receive an explicit error answer, so they cannot be
mistaken for an unmodelled blocked external request. (approved: PENDING) -/
def observationEnv : Oracle ops := fun hist p vs site st =>
  some ((env hist p vs site st).getD ⟨none, .error .abort, st⟩)

/-- Helper (computation). Completing unanswered malformed fixture requests preserves every
answer supplied by the original C1 demo environment. (approved: PENDING) -/
theorem observationEnv_of_answer {hist p vs site st a}
    (h : env hist p vs site st = some a) : observationEnv hist p vs site st = some a := by
  simp [observationEnv, h]

/-- A whole-program configuration that needs one silent step before it is final. -/
def finishingConfig : Config ops := ⟨.exec .skip, [], .init, []⟩

/-- The demo's silent, nonterminating loop. -/
def spinConfig : Config ops :=
  start ⟨[], .int, ⟨0⟩, .loop ⟨0⟩ .skip (.lit 1) .skip s⟩ [] .init

/-- `outOfFuel` does not imply divergence: this run only needed one more unit of fuel. -/
example :
    isOutOfFuel (run prog observationEnv 0 finishingConfig) = true ∧
      brief (run prog observationEnv 1 finishingConfig) = ([], some .normal) := by decide

/-- A configuration whose first step aborts. -/
def failingConfig : Config ops := ⟨.exec (.abort s), [], .init, []⟩

/-- `outOfFuel` also does not exclude failure: one more unit of fuel reveals this trap. -/
example : isOutOfFuel (run prog observationEnv 0 failingConfig) = true ∧
    brief (run prog observationEnv 1 failingConfig) = ([], some (.trapped .abort s)) := by decide

/-- `outOfFuel` is not a program failure either: the silent loop exhausts its simulation budget
without trapping or becoming stuck. The infinite-execution witness is `spin_infinite` below. -/
example : isOutOfFuel (run prog observationEnv 50 spinConfig) = true := by decide

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

/-- Helper (computation). Concrete computation for `spin_move_01`. (approved: PENDING). -/
theorem spin_move_01 : Move prog observationEnv ⟨spinConfig, [], []⟩ ⟨spinConfig1, [], []⟩ := .silent rfl
/-- Helper (computation). Concrete computation for `spin_move_12`. (approved: PENDING). -/
theorem spin_move_12 : Move prog observationEnv ⟨spinConfig1, [], []⟩ ⟨spinConfig2, [], []⟩ := .silent rfl
/-- Helper (computation). Concrete computation for `spin_move_23`. (approved: PENDING). -/
theorem spin_move_23 : Move prog observationEnv ⟨spinConfig2, [], []⟩ ⟨spinConfig3, [], []⟩ := .silent rfl
/-- Helper (computation). Concrete computation for `spin_move_34`. (approved: PENDING). -/
theorem spin_move_34 : Move prog observationEnv ⟨spinConfig3, [], []⟩ ⟨spinConfig4, [], []⟩ := .silent rfl
/-- Helper (computation). Concrete computation for `spin_move_40`. (approved: PENDING). -/
theorem spin_move_40 : Move prog observationEnv ⟨spinConfig4, [], []⟩ ⟨spinConfig, [], []⟩ := .silent rfl

/-- Helper (computation). Every adjacent pair of periodic positions is one machine transition.
(approved: PENDING). -/
theorem spin_move (n : Nat) : Move prog observationEnv (spinPosition n) (spinPosition (n + 1)) := by
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
def spin_infinite : InfiniteExecution prog observationEnv spinConfig where
  states := spinPosition
  initial := rfl
  step := spin_move

/-- Helper (computation). In particular it is internal silent divergence: infinitely many steps and no
raw events. (approved: PENDING). -/
theorem spin_internal_silent_divergence : InternalSilentDivergence prog observationEnv spinConfig :=
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

/-- Helper (computation). Concrete computation for `event_move_01`. (approved: PENDING). -/
theorem event_move_01 (n : Nat) : Move prog observationEnv
    ⟨eventConfig, rawEvents n, (List.range n).map eventQuery⟩
    ⟨eventConfig1, rawEvents n, (List.range n).map eventQuery⟩ := .silent rfl
/-- Helper (computation). Concrete computation for `event_move_12`. (approved: PENDING). -/
theorem event_move_12 (n : Nat) : Move prog observationEnv
    ⟨eventConfig1, rawEvents n, (List.range n).map eventQuery⟩
    ⟨eventConfig2, rawEvents n, (List.range n).map eventQuery⟩ := .silent rfl
/-- Helper (computation). Concrete computation for `event_move_23`. (approved: PENDING). -/
theorem event_move_23 (n : Nat) : Move prog observationEnv
    ⟨eventConfig2, rawEvents n, (List.range n).map eventQuery⟩
    ⟨eventConfig3, rawEvents n, (List.range n).map eventQuery⟩ := .silent rfl
/-- Helper (computation). Concrete computation for `event_move_40`. (approved: PENDING). -/
theorem event_move_40 (n : Nat) : Move prog observationEnv
    ⟨eventConfig4, rawEvents n, (List.range n).map eventQuery⟩
    ⟨eventConfig, rawEvents n, (List.range n).map eventQuery⟩ := .silent rfl

/-- Helper (computation). The logging transition appends exactly its raw event and its next per-site
query. (approved: PENDING). -/
theorem event_move_34 (n : Nat) : Move prog observationEnv
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
  have hm : Move prog observationEnv ⟨eventConfig3, rawEvents n, (List.range n).map eventQuery⟩
      ⟨eventConfig4, rawEvents n ++ [1], (List.range n).map eventQuery ++ [eventQuery n]⟩ := by
    rw [← hq]
    exact Move.prim (P := prog) (ρ := observationEnv) (c := eventConfig3) (c' := eventConfig4)
      (events := rawEvents n) (issued := (List.range n).map eventQuery) (p := .log) (vs := [1])
      (site := s) (st := spinStore) (a := ⟨some 1, .ok 0, spinStore⟩) rfl rfl rfl rfl
  simpa [rawEvents, List.range_succ] using hm

/-- Helper (computation). Every adjacent pair of positions in the logging loop is one real machine
transition. (approved: PENDING). -/
theorem event_move (n : Nat) : Move prog observationEnv (eventPosition n) (eventPosition (n + 1)) := by
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
def event_infinite : InfiniteExecution prog observationEnv eventConfig where
  states := eventPosition
  initial := rfl
  step := event_move

/-- An adapter that erases every raw event and gives every primitive an internal contract. -/
def erasingAdapter : Observe.ExecAdapter ops Observe.Demo.signature where
  events := fun _ => []
  request := fun _ _ _ _ => none
  status := fun _ => none
  state := fun st => st.counter
  sourceProject := ⟨[], []⟩
  sourceInput := ⟨[], []⟩
  loaded := rfl

/-- An adapter that retains every raw event as public output. -/
def keepingAdapter : Observe.ExecAdapter ops Observe.Demo.signature where
  events := fun n => [.output (toString n)]
  request := fun _ _ _ _ => none
  status := fun _ => none
  state := fun st => st.counter
  sourceProject := ⟨[], []⟩
  sourceInput := ⟨[], []⟩
  loaded := rfl

/-- Helper (computation). Concrete computation for `erasing_project`. (approved: APPR-integ-agy-0dbeba29e). -/
theorem erasing_project (events : List Int) : erasingAdapter.project events = [] := by
  induction events with
  | nil => rfl
  | cons event events ih => simp [Observe.ExecAdapter.project, erasingAdapter]

/-- Helper (computation). Concrete computation for `keeping_project_rawEvents`. (approved: APPR-integ-agy-0dbeba29e). -/
theorem keeping_project_rawEvents (n : Nat) : keepingAdapter.project (rawEvents n) =
    (List.range n).map fun _ => Observe.Event.output "1" := by
  induction n with
  | zero => rfl
  | succ n ih =>
    rw [show rawEvents (n + 1) = rawEvents n ++ [1] by simp [rawEvents, List.range_succ],
      Observe.ExecAdapter.project_append, ih]
    simp [Observe.ExecAdapter.project, keepingAdapter, List.range_succ] <;> decide

/-- Helper (computation). Empty output satisfies C6's internal-answer rule even though C1 emits a raw
event. (approved: PENDING). -/
theorem erasingWellFormed : Observe.ExecAdapter.WellFormed erasingAdapter Observe.Demo.contract () observationEnv := by
  constructor
  · intro hist p vs site store a answer typed
    exact ⟨[], erasing_project _⟩
  · intro hist p vs site store a q answer typed
    simp [erasingAdapter] at typed
  · intro hist p vs site store answer
    simp [observationEnv, env] at answer

/-- Helper (computation). Retaining raw events as public output satisfies C6's internal-answer rule.
(approved: PENDING). -/
theorem keepingWellFormed : Observe.ExecAdapter.WellFormed keepingAdapter Observe.Demo.contract () observationEnv := by
  constructor
  · intro hist p vs site store a answer typed
    cases a with
    | mk ev res st =>
      cases ev with
      | none => exact ⟨[], rfl⟩
      | some n => exact ⟨[toString n], rfl⟩
  · intro hist p vs site store a q answer typed
    simp [keepingAdapter] at typed
  · intro hist p vs site store answer
    simp [observationEnv, env] at answer

/-- Helper (computation). Externally the erasing run is silent divergence. (approved: PENDING). -/
theorem erasing_silent_divergence : SilentDivergence prog erasingAdapter observationEnv eventConfig :=
  ⟨event_infinite, ⟨0, fun n => by simp [event_infinite, eventPosition, erasing_project]⟩⟩

/-- Helper (computation). Internally that same run has infinitely many raw events. (approved:
APPR-integ-agy-0dbeba29e). -/
theorem erasing_internal_infinite : InternalInfiniteInteraction prog observationEnv eventConfig := by
  refine ⟨event_infinite, fun bound => ⟨5 * (bound + 1), ?_⟩⟩
  simp [event_infinite, eventPosition, eventCount, rawEvents]

/-- Helper (computation). Retaining the raw events turns the same execution into external infinite
interaction. (approved: PENDING). -/
theorem keeping_infinite_interaction : InfiniteInteraction prog keepingAdapter observationEnv eventConfig := by
  refine ⟨event_infinite, fun bound => ⟨5 * (bound + 1), ?_⟩⟩
  simp [event_infinite, eventPosition, eventCount, keeping_project_rawEvents]

/-- Non-vacuity for `finite_projected_observation`: the erasing adapter has a real C6 finite
observation on its induced step stream even though the raw execution interacts forever. -/
example : ∃ events,
    (∃ start, ∀ n, start ≤ n → erasingAdapter.project (event_infinite.states n).events = events) ∧
    Observe.CompleteTrace Observe.Demo.contract () events ∧
    Observe.HasFullObservation (.infinite (event_infinite.toObserveSteps erasingAdapter))
      (.finiteDivergence events) ∧
    ∀ steps, MatchesProjectedHistory erasingAdapter event_infinite steps →
      Observe.HasFullObservation (.infinite steps) (.finiteDivergence events) :=
  finite_projected_observation erasingWellFormed (show FiniteEvents erasingAdapter event_infinite from
    ⟨0, fun _ => by simp [erasing_project]⟩)

/-- Non-vacuity for `infinite_projected_observation`: the keeping adapter has a real infinite C6
trace on its induced step stream. -/
example : ∃ trace : Observe.InfiniteTrace Observe.Demo.signature,
    (∀ n, keepingAdapter.project (event_infinite.states n).events =
      (List.range (keepingAdapter.project (event_infinite.states n).events).length).map trace ∧
      Observe.CompleteTrace Observe.Demo.contract ()
        (keepingAdapter.project (event_infinite.states n).events)) ∧
    Observe.HasFullObservation (.infinite (event_infinite.toObserveSteps keepingAdapter))
      (.infiniteDivergence trace) ∧
    ∀ steps, MatchesProjectedHistory keepingAdapter event_infinite steps →
      Observe.HasFullObservation (.infinite steps) (.infiniteDivergence trace) :=
  infinite_projected_observation keepingWellFormed (show InfiniteEvents keepingAdapter event_infinite from by
    intro bound
    refine ⟨5 * (bound + 1), ?_⟩
    simp [event_infinite, eventPosition, eventCount, keeping_project_rawEvents])

/-- Planted raw-event classification: it disagrees with C6 on the erasing adapter. The run has
infinite internal interaction but no external infinite interaction. -/
example : InternalInfiniteInteraction prog observationEnv eventConfig ∧
    ¬ InfiniteInteraction prog erasingAdapter observationEnv eventConfig :=
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

def silentRequests : QueryReport ops := runQ prog observationEnv 4 silentRequestConfig
def interleavedRequests : QueryReport ops := runQ prog observationEnv 4 interleavedRequestConfig

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
  apply runFromQ_consecutive_same_site (P := prog) (ρ := observationEnv) 4 [] [] silentRequestConfig
    (i := 0) (q := silentQuery0) (q' := silentQuery1)
  · intro i q h
    simp at h
  · rfl
  · rfl
  · rfl

/-- Non-vacuity for `runFromQ_consecutive_other_site`: the second site's first occurrence is zero. -/
example : interleavedQuery1.occurrence = 0 := by
  have h := runFromQ_consecutive_other_site (P := prog) (ρ := observationEnv) 4 [] []
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

/-- Helper (computation). A real transition under the lifted ordinary demo oracle. (approved:
APPR-integ-agy-0dbeba29e). -/
theorem bumpedMove : Move prog observationEnv ⟨bumpConfig, [], []⟩ ⟨bumpContinued, [], [bumpQuery]⟩ :=
  Move.prim (P := prog) (ρ := observationEnv) (c := bumpConfig) (c' := bumpContinued) (events := [])
    (issued := []) (p := .bump) (vs := []) (site := s) (st := .init)
    (a := ⟨none, .ok 0, ⟨1, []⟩⟩) rfl rfl rfl rfl

/-- Non-vacuity for `Move.toQueryOracle_answer`: the actual transition uses exactly `observationEnv`'s answer. -/
example : ∃ a, observationEnv [] .bump [] s .init = some a ∧ ops.allows .bump [] .init a = true ∧
    bumpK a = some bumpContinued ∧ [] = [] ++ a.event.toList ∧
      [bumpQuery] = [] ++ [bumpQuery] := by
  exact (Move.toQueryOracle_answer (ρ₀ := observationEnv) (pos := ⟨bumpConfig, [], []⟩)
    (pos' := ⟨bumpContinued, [], [bumpQuery]⟩) (p := .bump) (vs := []) (site := s)
    (st := .init) (k := bumpK) (by
      simp [bumpConfig, step, evalArgs, onRes]
      funext a
      cases hr : a.result <;> simp [bumpK, hr])).mp bumpedMove

/-- An occurrence-sensitive mutation answers only the first request at each site. -/
def occurrenceSensitiveOracle : QueryOracle ops := fun q =>
  if q.occurrence = 0 then answer observationEnv.toQueryOracle q else none

/-- Planted counter-example for lifted-oracle preservation: unlike `observationEnv.toQueryOracle`, this oracle
refuses the actual second request and therefore produces a different run. -/
example : (runQ prog occurrenceSensitiveOracle 4 silentRequestConfig).toReport ≠
    run prog observationEnv 4 silentRequestConfig := by
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
  ⟨boomF, fun ρ args st => ρ = observationEnv ∧ args = [] ∧ st = .init⟩

/-- This aborting function terminates with a trapped outcome; termination does not promise success. -/
def terminatingConditional : ConditionalTermination ops prog terminatingDomain where
  dependencies := []
  Premise := fun _ => Unit
  fuel := fun _ _ _ _ => 10
  terminates := by
    intro _ ρ args st h
    obtain ⟨rfl, rfl, rfl⟩ := h
    exact ⟨boomFn, start boomFn [] .init, .trapped .abort ⟨9⟩, _,
      rfl, rfl, rfl⟩

/-- An inhabited certificate for a declared function and an actual successful entry. -/
def terminatingEvidence : TerminationEvidence ops prog terminatingDomain :=
  ⟨terminatingConditional, (fun _ h => nomatch h), observationEnv, [], .init, rfl, rfl, rfl⟩

/-- The empty declared domain has no usable certificate, even though its conditional coverage
can be proved with no executions. -/
example : ¬ Nonempty (TerminationEvidence ops prog ⟨boomF, fun _ _ _ => False⟩) := by
  rintro ⟨E⟩
  obtain ⟨ρ, args, st, h⟩ := E.admitted
  exact h

/-- A missing function cannot be certified by restricting the certificate's admission filter. -/
example : ¬ Nonempty (TerminationEvidence ops (⟨[]⟩ : Program ops) terminatingDomain) := by
  rintro ⟨E⟩
  obtain ⟨fn, c, f, finalStore, hf, _, _⟩ := E.conditional.terminates E.premises observationEnv [] .init ⟨rfl, rfl, rfl⟩
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
  obtain ⟨fn, c, f, finalStore, hf, he, _⟩ := E.conditional.terminates E.premises ρ args st h
  have eqfn : fn = refusedEntryFn := by simpa [refusedEntryProgram, refusedEntryDomain, AList.lookup] using hf.symm
  subst fn
  simp [enter, refusedEntryOps] at he

/-- A declared looping function, with the same scope as `spinConfig`. -/
def spinningFn : Fn ops := ⟨[], .int, ⟨0⟩, .loop ⟨0⟩ .skip (.lit 1) .skip s⟩
def spinningProgram : Program ops := ⟨[(mainF, spinningFn)]⟩
def spinningDomain : CallDomain ops :=
  ⟨mainF, fun ρ args st => ρ = observationEnv ∧ args = [] ∧ st = .init⟩

/-- Program lookup is irrelevant to this call-free loop. -/
def admittedSpin : InfiniteExecution spinningProgram observationEnv (start spinningFn [] .init) where
  states := spinPosition
  initial := rfl
  step := by
    intro n
    have spin_move_01 : Move spinningProgram observationEnv ⟨spinConfig, [], []⟩ ⟨spinConfig1, [], []⟩ := .silent rfl
    have spin_move_12 : Move spinningProgram observationEnv ⟨spinConfig1, [], []⟩ ⟨spinConfig2, [], []⟩ := .silent rfl
    have spin_move_23 : Move spinningProgram observationEnv ⟨spinConfig2, [], []⟩ ⟨spinConfig3, [], []⟩ := .silent rfl
    have spin_move_34 : Move spinningProgram observationEnv ⟨spinConfig3, [], []⟩ ⟨spinConfig4, [], []⟩ := .silent rfl
    have spin_move_40 : Move spinningProgram observationEnv ⟨spinConfig4, [], []⟩ ⟨spinConfig, [], []⟩ := .silent rfl
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
  exact E.excludes_infinite observationEnv [] .init spinningFn (start spinningFn [] .init)
    rfl ⟨rfl, rfl, rfl⟩ rfl ⟨admittedSpin⟩

/-- The per-callee termination premise for `spinningDomain`: every admitted call terminates within
its witness budget. -/
def spinningCalleePremise : Type :=
  PLift (∀ ρ args st, spinningDomain.calls ρ args st →
    ∃ fn c f finalStore, AList.lookup spinningProgram.fns spinningDomain.function = some fn ∧
      enter fn args st = some c ∧ (run spinningProgram ρ 10 c).ending = .final f finalStore)

/-- A conditional theorem that lists an uncertified dependency (`mainF`) whose premise requires
termination of `spinningDomain` cannot have its dependency-indexed `Premises` discharged. -/
def uncertifiedDepConditional : ConditionalTermination ops prog terminatingDomain where
  dependencies := [mainF]
  Premise := fun _ => spinningCalleePremise
  fuel := fun _ _ _ _ => 10
  terminates := by
    intro _ ρ args st h
    obtain ⟨rfl, rfl, rfl⟩ := h
    exact ⟨boomFn, start boomFn [] .init, .trapped .abort ⟨9⟩, _,
      rfl, rfl, rfl⟩

/-- Because `dependencies` indexes `Premises`, a conditional theorem depending on a diverging
callee has an uninhabited premise family. -/
example : ¬ Nonempty uncertifiedDepConditional.Premises := by
  rintro ⟨premises⟩
  obtain ⟨h⟩ := premises mainF (.head [])
  obtain ⟨fn, c, f, finalStore, hf, he, hfinal⟩ := h observationEnv [] .init ⟨rfl, rfl, rfl⟩
  cases hf
  cases he
  exact not_infinite_of_final hfinal ⟨admittedSpin⟩

/-! ### Closed round-four controls -/

/-- The actual request continuation, extracted without postulating its answer. -/
def terminalK : Answer ops.Val ops.Event ops.Store → Option (Config ops) := fun a =>
  match a.result with
  | .ok v => (bindLife ops none a.store).map fun st' => ⟨.done, bindDst none v [], st', []⟩
  | .error kd => some ⟨.trapped kd s, [], a.store, []⟩

/-- Helper (computation). Computes the request and continuation issued by the concrete logging
statement. (approved: APPR-integ-agy-0dbeba29e). -/
theorem terminalRequest : step prog terminalConfig = some (.request .log [1] s .init terminalK) := by
  simp [terminalConfig, step, evalArgs, evalE, onRes, Res.bind, Res.map]
  funext a
  cases hr : a.result <;> simp [terminalK, hr]

/-- Instance (law `runFromQ_request_correspondence`, case `terminalConfig`). -/
example : RequestResolution prog noAnswerOracle ⟨terminalConfig, [], []⟩ .log [1] s .init terminalK
    (runFromQ prog noAnswerOracle 1 [] [] terminalConfig) :=
  runFromQ_request_correspondence (O := ops) (P := prog) (ρ := noAnswerOracle)
  (fuel := 0) (events := []) (issued := []) (c := terminalConfig) (p := .log) (vs := [1])
  (site := s) (st := .init) (k := terminalK) rfl terminalRequest

/-- Instance (law `runFromQ_request_correspondence`, case `badAnswerOracle`). -/
example : RequestResolution prog badAnswerOracle ⟨terminalConfig, [], []⟩ .log [1] s .init terminalK
    (runFromQ prog badAnswerOracle 1 [] [] terminalConfig) :=
  runFromQ_request_correspondence (O := ops) (P := prog) (ρ := badAnswerOracle)
  (fuel := 0) (events := []) (issued := []) (c := terminalConfig) (p := .log) (vs := [1])
  (site := s) (st := .init) (k := terminalK) rfl terminalRequest

/-- Instance (law `runFromQ_request_correspondence`, case `observationEnv`). -/
example : RequestResolution prog observationEnv.toQueryOracle ⟨terminalConfig, [], []⟩ .log [1] s .init terminalK
    (runFromQ prog observationEnv.toQueryOracle 1 [] [] terminalConfig) :=
  runFromQ_request_correspondence (O := ops) (P := prog) (ρ := observationEnv.toQueryOracle)
  (fuel := 0) (events := []) (issued := []) (c := terminalConfig) (p := .log) (vs := [1])
  (site := s) (st := .init) (k := terminalK) rfl terminalRequest

/-- A mutation fabricates successful termination after an unanswered or rejected request.
This tests report soundness separately from the wrong-history oracle mutation. -/
def fabricatedFinalRun {O : Operands} (P : Program O) (ρ : QueryOracle O) (fuel : Nat)
    (events : List O.Event) (issued : List (Query O)) (c : Config O) : QueryReport O :=
  { runFromQ P ρ fuel events issued c with ending := .final .normal c.store }

/-- An evaluator mutation discards all supplied answers. -/
def droppedAnswerRun {O : Operands} (P : Program O) (_ : QueryOracle O) (fuel : Nat)
    (events : List O.Event) (issued : List (Query O)) (c : Config O) : QueryReport O :=
  runFromQ P (fun _ => none) fuel events issued c

/-- Planted (law `runFromQ_request_correspondence`, case `terminalConfig`,
subject `runFromQ`, mutant `fabricatedFinalRun`). The issuance premises remain true. -/
example : terminalConfig.final? = none ∧
    step prog terminalConfig = some (.request .log [1] s .init terminalK) ∧
    ¬ RequestResolution prog noAnswerOracle ⟨terminalConfig, [], []⟩ .log [1] s .init terminalK
      (fabricatedFinalRun prog noAnswerOracle 1 [] [] terminalConfig) := by
  refine ⟨rfl, terminalRequest, ?_⟩
  generalize he : fabricatedFinalRun prog noAnswerOracle 1 [] [] terminalConfig = report
  intro h
  cases h <;> simp_all [answer, noAnswerOracle, fabricatedFinalRun, terminalConfig]

/-- Planted (law `runFromQ_request_correspondence`, case `badAnswerOracle`,
subject `runFromQ`, mutant `fabricatedFinalRun`). The issuance premises remain true, while the
invalid oracle answer cannot resolve to `.final .normal`. -/
example : terminalConfig.final? = none ∧
    step prog terminalConfig = some (.request .log [1] s .init terminalK) ∧
    ¬ RequestResolution prog badAnswerOracle ⟨terminalConfig, [], []⟩ .log [1] s .init terminalK
      (fabricatedFinalRun prog badAnswerOracle 1 [] [] terminalConfig) := by
  refine ⟨rfl, terminalRequest, ?_⟩
  generalize he : fabricatedFinalRun prog badAnswerOracle 1 [] [] terminalConfig = report
  intro h
  cases h with
  | unanswered ha _ _ =>
    simp [answer, badAnswerOracle] at ha
  | invalid _ _ _ _ =>
    have hend := congrArg QueryReport.ending he
    simp [fabricatedFinalRun] at hend
  | refused ha hl _ _ _ =>
    simp [answer, badAnswerOracle] at ha
    subst ha
    simp [ops] at hl
  | continued ha hl _ _ _ _ _ _ =>
    simp [answer, badAnswerOracle] at ha
    subst ha
    simp [ops] at hl

/-- Planted (law `runFromQ_request_correspondence`, case `observationEnv`,
subject `runFromQ`, mutant `droppedAnswerRun`). Dropping the valid oracle answer omits the
continued transition's emitted event and reports `.noAnswer` instead. -/
example : terminalConfig.final? = none ∧
    step prog terminalConfig = some (.request .log [1] s .init terminalK) ∧
    ¬ RequestResolution prog observationEnv.toQueryOracle ⟨terminalConfig, [], []⟩ .log [1] s .init terminalK
      (droppedAnswerRun prog observationEnv.toQueryOracle 1 [] [] terminalConfig) := by
  refine ⟨rfl, terminalRequest, ?_⟩
  have hev : (droppedAnswerRun prog observationEnv.toQueryOracle 1 [] [] terminalConfig).events = [] := rfl
  generalize he : droppedAnswerRun prog observationEnv.toQueryOracle 1 [] [] terminalConfig = report at hev ⊢
  intro h
  cases h with
  | unanswered ha _ _ =>
    simp [answer, Oracle.toQueryOracle, observationEnv, env, Query.issue] at ha
  | invalid ha hl _ _ =>
    simp [answer, Oracle.toQueryOracle, observationEnv, env, Query.issue] at ha
    subst ha
    simp [ops] at hl
  | refused ha _ hk _ _ =>
    simp [answer, Oracle.toQueryOracle, observationEnv, env, Query.issue] at ha
    subst ha
    simp [terminalK, bindLife, ops, DState.life] at hk
  | continued _ _ _ hm hpath hevents _ _ =>
    obtain ⟨a, ha, _, _, hpos', _⟩ := (Move.toQueryOracle_answer terminalRequest).mp hm
    simp [observationEnv, env] at ha
    subst ha
    obtain ⟨tr, _, hpath_ev, _, _⟩ := Moves.toSteps hpath
    rw [hev, hpath_ev, hpos'] at hevents
    simp at hevents

/-- Helper (computation). Finite projected histories of the erasing execution. (approved: APPR-integ-agy-0dbeba29e). -/
theorem erasedFinite : FiniteEvents erasingAdapter event_infinite :=
  ⟨0, fun _ => by simp [erasing_project]⟩
/-- Helper (computation). Unbounded projected histories of the keeping execution. (approved: APPR-integ-agy-0dbeba29e). -/
theorem keptInfinite : InfiniteEvents keepingAdapter event_infinite := by
  intro bound
  refine ⟨5 * (bound + 1), ?_⟩
  simp [event_infinite, eventPosition, eventCount, keeping_project_rawEvents]

/-- A configuration that logs once and then enters the silent loop. -/
def logThenSpinConfig : Config ops :=
  ⟨.exec eventBody, [], spinStore,
    [.seq (.loop ⟨0⟩ .skip (.lit 1) .skip s), spinScope]⟩

def logThenSpinConfig1 : Config ops :=
  ⟨.done, [], spinStore,
    [.seq (.loop ⟨0⟩ .skip (.lit 1) .skip s), spinScope]⟩

def logThenSpinPosition : Nat → Position ops
  | 0 => ⟨logThenSpinConfig, [], []⟩
  | 1 => ⟨logThenSpinConfig1, [1], [eventQuery 0]⟩
  | n + 2 => ⟨spinConfigAt (n % 5), [1], [eventQuery 0]⟩

/-- Helper (computation). Every adjacent pair of positions in `logThenSpinPosition` is one machine
transition. (approved: PENDING). -/
theorem logThenSpin_move : ∀ n, Move prog observationEnv (logThenSpinPosition n) (logThenSpinPosition (n + 1))
  | 0 =>
    Move.prim (P := prog) (ρ := observationEnv) (c := logThenSpinConfig) (c' := logThenSpinConfig1)
      (events := []) (issued := []) (p := .log) (vs := [1]) (site := s) (st := spinStore)
      (a := ⟨some 1, .ok 0, spinStore⟩) rfl rfl rfl rfl
  | 1 => .silent rfl
  | n + 2 => by
    have hlt : n % 5 < 5 := Nat.mod_lt _ (by decide)
    have hc : n % 5 = 0 ∨ n % 5 = 1 ∨ n % 5 = 2 ∨ n % 5 = 3 ∨ n % 5 = 4 := by omega
    rcases hc with h | h | h | h | h
    · have hn : (n + 1) % 5 = 1 := by omega
      simpa [logThenSpinPosition, spinConfigAt, h, hn] using
        (Move.silent rfl : Move prog observationEnv ⟨spinConfig, [1], [eventQuery 0]⟩
          ⟨spinConfig1, [1], [eventQuery 0]⟩)
    · have hn : (n + 1) % 5 = 2 := by omega
      simpa [logThenSpinPosition, spinConfigAt, h, hn] using
        (Move.silent rfl : Move prog observationEnv ⟨spinConfig1, [1], [eventQuery 0]⟩
          ⟨spinConfig2, [1], [eventQuery 0]⟩)
    · have hn : (n + 1) % 5 = 3 := by omega
      simpa [logThenSpinPosition, spinConfigAt, h, hn] using
        (Move.silent rfl : Move prog observationEnv ⟨spinConfig2, [1], [eventQuery 0]⟩
          ⟨spinConfig3, [1], [eventQuery 0]⟩)
    · have hn : (n + 1) % 5 = 4 := by omega
      simpa [logThenSpinPosition, spinConfigAt, h, hn] using
        (Move.silent rfl : Move prog observationEnv ⟨spinConfig3, [1], [eventQuery 0]⟩
          ⟨spinConfig4, [1], [eventQuery 0]⟩)
    · have hn : (n + 1) % 5 = 0 := by omega
      simpa [logThenSpinPosition, spinConfigAt, h, hn] using
        (Move.silent rfl : Move prog observationEnv ⟨spinConfig4, [1], [eventQuery 0]⟩
          ⟨spinConfig, [1], [eventQuery 0]⟩)

/-- An infinite execution that emits one visible event and then spins silently forever. -/
def logThenSpin_infinite : InfiniteExecution prog observationEnv logThenSpinConfig where
  states := logThenSpinPosition
  initial := rfl
  step := logThenSpin_move

/-- Helper (computation). The one-event-then-spin execution has bounded projected history under the
keeping adapter. (approved: APPR-integ-agy-0dbeba29e). -/
theorem logThenSpinFinite : FiniteEvents keepingAdapter logThenSpin_infinite := by
  refine ⟨1, fun n => ?_⟩
  cases n with
  | zero => decide
  | succ m =>
    cases m with
    | zero => decide
    | succ k => exact Nat.le_refl 1

/-- A genuinely multi-event adapter: each raw log is two public outputs. -/
def doubleAdapter : Observe.ExecAdapter ops Observe.Demo.signature :=
  { keepingAdapter with events := fun n => [.output (toString n), .output "second"] }

/-- A scheduler mutant exchanges adjacent steps, moving an output from the first transition's
block into the following silent transition without losing the event globally. -/
def crossBoundarySteps {O : Operands} {S : Observe.EventSignature} {P : Program O}
    {ρ : QueryOracle O} {c : Config O} (A : Observe.ExecAdapter O S)
    (X : InfiniteExecution P ρ c) (i : Nat) : Observe.Step S :=
  X.toObserveSteps A (if i = 1 then 2 else if i = 2 then 1 else i)

/-- Instance (law `InfiniteExecution.toObserveSteps_blocks`, case `doubleAdapter`). -/
example : Observe.visible
    (((List.range (logThenSpin_infinite.observeBoundary doubleAdapter (0 + 1))).map
      (logThenSpin_infinite.toObserveSteps doubleAdapter)).drop
      (logThenSpin_infinite.observeBoundary doubleAdapter 0)) =
    logThenSpin_infinite.transitionEvents doubleAdapter 0 :=
  InfiniteExecution.toObserveSteps_blocks doubleAdapter logThenSpin_infinite 0

/-- Planted (law `InfiniteExecution.toObserveSteps_blocks`, case `doubleAdapter`,
subject `InfiniteExecution.toObserveSteps`, mutant `crossBoundarySteps`). -/
example : ¬ (Observe.visible
    (((List.range (logThenSpin_infinite.observeBoundary doubleAdapter (0 + 1))).map
      (crossBoundarySteps doubleAdapter logThenSpin_infinite)).drop
      (logThenSpin_infinite.observeBoundary doubleAdapter 0)) =
    logThenSpin_infinite.transitionEvents doubleAdapter 0) := by
  change ¬ ([Observe.Event.output "1"] : List (Observe.Event Observe.Demo.signature)) =
    [.output "1", .output "second"]
  intro h
  cases h

/-- The mutant preserves the global event order after the next boundary: cofinal event agreement
alone cannot detect this error. -/
example : Observe.visiblePrefix (crossBoundarySteps doubleAdapter logThenSpin_infinite) 3 =
    Observe.visiblePrefix (logThenSpin_infinite.toObserveSteps doubleAdapter) 3 := rfl

/-- Accepted: the silent transition following the two-event transition remains entirely silent. -/
example : logThenSpin_infinite.observeBoundary doubleAdapter 1 = 2 ∧
    logThenSpin_infinite.observeBoundary doubleAdapter 2 = 3 ∧
    logThenSpin_infinite.toObserveSteps doubleAdapter 2 = .silent := ⟨rfl, rfl, rfl⟩

/-- A step-stream mutation that turns every machine transition into a silent C6 step, dropping all
emitted projected events. -/
def silentObserveSteps {O : Operands} {S : Observe.EventSignature} {P : Program O}
    {ρ : QueryOracle O} {c : Config O} (_ : Observe.ExecAdapter O S)
    (_ : InfiniteExecution P ρ c) (_ : Nat) : Observe.Step S := .silent

/-- Helper (computation). Every visible prefix of `silentObserveSteps` is empty. (approved:
APPR-integ-agy-0dbeba29e). -/
theorem visiblePrefix_silentObserveSteps {O : Operands} {S : Observe.EventSignature}
    {P : Program O} {ρ : QueryOracle O} {c : Config O}
    (A : Observe.ExecAdapter O S) (X : InfiniteExecution P ρ c) (n : Nat) :
    Observe.visiblePrefix (silentObserveSteps A X) n = [] := by
  induction n with
  | zero => rfl
  | succ n ih => simp [Observe.visiblePrefix_succ, silentObserveSteps, Observe.Step.event?, ih]

/-- A malformed projection leaves a fabricated request outstanding whenever the event signature
has a request. It changes the operation, while retaining C6's fixed completeness semantics. -/
noncomputable def pendingTrace {O : Operands} {S : Observe.EventSignature} {P : Program O}
    {ρ : QueryOracle O} {c : Config O} (A : Observe.ExecAdapter O S)
    (X : InfiniteExecution P ρ c) (n : Nat) : List (Observe.Event S) := by
  classical
  exact if h : Nonempty (Observe.Request S) then [.request (Classical.choice h)]
  else X.projectedTrace A n

/-- Instance (law `InfiniteExecution.projectedTrace_valid`, case `keepingAdapter`). -/
example : Observe.CompleteTrace Observe.Demo.contract ()
    (event_infinite.projectedTrace keepingAdapter 5) :=
  InfiniteExecution.projectedTrace_valid (O := ops) (P := prog) (ρ₀ := observationEnv)
    (c := eventConfig) (S := Observe.Demo.signature) (C := Observe.Demo.contract)
    (input := ()) (A := keepingAdapter) event_infinite keepingWellFormed 5

/-- Planted (law `InfiniteExecution.projectedTrace_valid`, case `keepingAdapter`,
subject `InfiniteExecution.projectedTrace`, mutant `pendingTrace`). -/
example : Observe.ExecAdapter.WellFormed keepingAdapter Observe.Demo.contract () observationEnv ∧
    ¬ Observe.CompleteTrace Observe.Demo.contract ()
      (pendingTrace keepingAdapter event_infinite 5) := by
  refine ⟨keepingWellFormed, ?_⟩
  have h : Nonempty (Observe.Request Observe.Demo.signature) := ⟨⟨.double, 3⟩⟩
  simp only [pendingTrace, h, ↓reduceDIte, Observe.CompleteTrace]
  intro valid
  generalize he : ([Observe.Event.request (Classical.choice h)] :
    List (Observe.Event Observe.Demo.signature)) = events at valid
  cases valid <;> have last := congrArg List.getLast? he <;> simp at last

/-- Instance (law `InfiniteExecution.toObserveSteps_matches`, case `keepingAdapter`). -/
example : MatchesProjectedHistory keepingAdapter event_infinite
    (event_infinite.toObserveSteps keepingAdapter) :=
  InfiniteExecution.toObserveSteps_matches keepingAdapter event_infinite

/-- Planted (law `InfiniteExecution.toObserveSteps_matches`, case `keepingAdapter`,
subject `InfiniteExecution.toObserveSteps`, mutant `silentObserveSteps`). -/
example : ¬ MatchesProjectedHistory keepingAdapter event_infinite
    (silentObserveSteps keepingAdapter event_infinite) := by
  intro hm
  obtain ⟨m, rest, h⟩ := hm.1 5
  rw [visiblePrefix_silentObserveSteps] at h
  have h5 : keepingAdapter.project (event_infinite.states 5).events = [.output "1"] := rfl
  rw [h5] at h
  cases h

/-- Instance (law `finite_projected_observation`, case `logThenSpin`). -/
example : ∃ events,
    (∃ start, ∀ n, start ≤ n →
      keepingAdapter.project (logThenSpin_infinite.states n).events = events) ∧
    Observe.CompleteTrace Observe.Demo.contract () events ∧
    Observe.HasFullObservation (.infinite (logThenSpin_infinite.toObserveSteps keepingAdapter))
      (.finiteDivergence events) ∧
    ∀ steps, MatchesProjectedHistory keepingAdapter logThenSpin_infinite steps →
      Observe.HasFullObservation (.infinite steps) (.finiteDivergence events) :=
  finite_projected_observation (O := ops) (P := prog) (ρ₀ := observationEnv) (c := logThenSpinConfig)
    (S := Observe.Demo.signature) (C := Observe.Demo.contract) (input := ())
    (A := keepingAdapter) (X := logThenSpin_infinite) keepingWellFormed logThenSpinFinite

/-- Planted (law `finite_projected_observation`, case `logThenSpin`,
subject `InfiniteExecution.toObserveSteps`, mutant `silentObserveSteps`). -/
example : Observe.ExecAdapter.WellFormed keepingAdapter Observe.Demo.contract () observationEnv ∧
    FiniteEvents keepingAdapter logThenSpin_infinite ∧
    ¬ (∃ events,
      (∃ start, ∀ n, start ≤ n →
        keepingAdapter.project (logThenSpin_infinite.states n).events = events) ∧
      Observe.CompleteTrace Observe.Demo.contract () events ∧
      Observe.HasFullObservation (.infinite (silentObserveSteps keepingAdapter logThenSpin_infinite))
        (.finiteDivergence events) ∧
      ∀ steps, MatchesProjectedHistory keepingAdapter logThenSpin_infinite steps →
        Observe.HasFullObservation (.infinite steps) (.finiteDivergence events)) := by
  refine ⟨keepingWellFormed, logThenSpinFinite, ?_⟩
  rintro ⟨events, ⟨start, hstable⟩, _, hobs, _⟩
  have hev : events = [.output "1"] := by
    have h1 := hstable (start + 1) (Nat.le_succ start)
    have hproj :
        keepingAdapter.project (logThenSpin_infinite.states (start + 1)).events = [.output "1"] := by
      cases start <;> rfl
    exact h1.symm.trans hproj
  subst events
  obtain ⟨m, rest, hr⟩ := hobs.2 1
  rw [visiblePrefix_silentObserveSteps, visiblePrefix_finiteDivergenceSteps] at hr
  cases hr

/-- Instance (law `infinite_projected_observation`, case `keepingAdapter`). -/
example : ∃ trace : Observe.InfiniteTrace Observe.Demo.signature,
    (∀ n, keepingAdapter.project (event_infinite.states n).events =
      (List.range (keepingAdapter.project (event_infinite.states n).events).length).map trace ∧
      Observe.CompleteTrace Observe.Demo.contract ()
        (keepingAdapter.project (event_infinite.states n).events)) ∧
    Observe.HasFullObservation (.infinite (event_infinite.toObserveSteps keepingAdapter))
      (.infiniteDivergence trace) ∧
    ∀ steps, MatchesProjectedHistory keepingAdapter event_infinite steps →
      Observe.HasFullObservation (.infinite steps) (.infiniteDivergence trace) :=
  infinite_projected_observation (O := ops) (P := prog) (ρ₀ := observationEnv) (c := eventConfig)
    (S := Observe.Demo.signature) (C := Observe.Demo.contract) (input := ())
    (A := keepingAdapter) (X := event_infinite) keepingWellFormed keptInfinite

/-- Planted (law `infinite_projected_observation`, case `keepingAdapter`,
subject `InfiniteExecution.toObserveSteps`, mutant `silentObserveSteps`). -/
example : Observe.ExecAdapter.WellFormed keepingAdapter Observe.Demo.contract () observationEnv ∧
    InfiniteEvents keepingAdapter event_infinite ∧
    ¬ (∃ trace : Observe.InfiniteTrace Observe.Demo.signature,
      (∀ n, keepingAdapter.project (event_infinite.states n).events =
        (List.range (keepingAdapter.project (event_infinite.states n).events).length).map trace ∧
        Observe.CompleteTrace Observe.Demo.contract ()
          (keepingAdapter.project (event_infinite.states n).events)) ∧
      Observe.HasFullObservation (.infinite (silentObserveSteps keepingAdapter event_infinite))
        (.infiniteDivergence trace) ∧
      ∀ steps, MatchesProjectedHistory keepingAdapter event_infinite steps →
        Observe.HasFullObservation (.infinite steps) (.infiniteDivergence trace)) := by
  refine ⟨keepingWellFormed, keptInfinite, ?_⟩
  rintro ⟨trace, _, hobs, _⟩
  obtain ⟨m, rest, hr⟩ := hobs.2 1
  rw [visiblePrefix_silentObserveSteps, visiblePrefix_emitTrace] at hr
  cases hr

/-- A wrong-history evaluator resets every occurrence before querying the supplied oracle. -/
def wrongHistoryRun {O : Operands} (P : Program O) (ρ : QueryOracle O) (fuel : Nat)
    (events : List O.Event) (issued : List (Query O)) (c : Config O) : QueryReport O :=
  runFromQ P (fun q => ρ { q with occurrence := 0 }) fuel events issued c

/-- Instance (law `runFromQ_request_correspondence`, case `occurrenceSensitiveOracle`). -/
example : RequestResolution prog occurrenceSensitiveOracle ⟨terminalConfig, [], [terminalQuery]⟩
    .log [1] s .init terminalK
    (runFromQ prog occurrenceSensitiveOracle 1 [] [terminalQuery] terminalConfig) :=
  runFromQ_request_correspondence (O := ops) (P := prog) (ρ := occurrenceSensitiveOracle)
    (fuel := 0) (events := []) (issued := [terminalQuery]) (c := terminalConfig) (p := .log)
    (vs := [1]) (site := s) (st := .init) (k := terminalK) rfl terminalRequest

/-- Planted (law `runFromQ_request_correspondence`, case `occurrenceSensitiveOracle`,
subject `runFromQ`, mutant `wrongHistoryRun`). Resetting the occurrence substitutes the first
answer for an unanswered repeated request. Both issuance premises are unchanged. -/
example : terminalConfig.final? = none ∧
    step prog terminalConfig = some (.request .log [1] s .init terminalK) ∧
    ¬ RequestResolution prog occurrenceSensitiveOracle ⟨terminalConfig, [], [terminalQuery]⟩
      .log [1] s .init terminalK
      (wrongHistoryRun prog occurrenceSensitiveOracle 1 [] [terminalQuery] terminalConfig) := by
  refine ⟨rfl, terminalRequest, ?_⟩
  have hfinal : (wrongHistoryRun prog occurrenceSensitiveOracle 1 [] [terminalQuery]
    terminalConfig).ending = .final .normal .init := rfl
  generalize he : wrongHistoryRun prog occurrenceSensitiveOracle 1 [] [terminalQuery]
    terminalConfig = report at hfinal ⊢
  intro h
  cases h with
  | unanswered _ _ _ => cases hfinal
  | invalid ha _ _ _ => simp [answer, occurrenceSensitiveOracle, Query.issue, occurrenceAt,
      terminalQuery] at ha
  | refused ha _ _ _ _ => simp [answer, occurrenceSensitiveOracle, Query.issue, occurrenceAt,
      terminalQuery] at ha
  | continued ha _ _ _ _ _ _ _ => simp [answer, occurrenceSensitiveOracle, Query.issue, occurrenceAt,
      terminalQuery] at ha

/-- The actual continuation of a lifecycle-refused request. -/
def refusingK : Answer refusingBindOps.Val refusingBindOps.Event refusingBindOps.Store →
    Option (Config refusingBindOps) := fun a =>
  match a.result with
  | .ok v => (bindLife refusingBindOps (some x) a.store).map fun st' =>
      ⟨.done, bindDst (some x) v [], st', []⟩
  | .error kd => some ⟨.trapped kd s, [], a.store, []⟩

/-- Helper (computation). Computes the request and continuation issued by the concrete
lifecycle-refused statement. (approved: APPR-integ-agy-0dbeba29e). -/
theorem refusingBindRequest :
    step refusingBindProgram refusingBindConfig = some (.request .log [1] s .init refusingK) := by
  simp [refusingBindConfig, step, evalArgs, evalE, onRes, Res.bind, Res.map]
  funext a
  cases hr : a.result <;> simp [refusingK, hr]

/-- Instance (law `runFromQ_request_correspondence`, case `refusingBindConfig`). -/
example : RequestResolution refusingBindProgram refusingBindOracle ⟨refusingBindConfig, [], []⟩
    .log [1] s .init refusingK
    (runFromQ refusingBindProgram refusingBindOracle 1 [] [] refusingBindConfig) :=
  runFromQ_request_correspondence (O := refusingBindOps) (P := refusingBindProgram)
    (ρ := refusingBindOracle) (fuel := 0) (events := []) (issued := []) (c := refusingBindConfig)
    (p := .log) (vs := [1]) (site := s) (st := .init) (k := refusingK) rfl refusingBindRequest

/-- Planted (law `runFromQ_request_correspondence`, case `refusingBindConfig`,
subject `runFromQ`, mutant `fabricatedFinalRun`). Refusing the continuation produces `.stuck`,
not `.final .normal`. -/
example : refusingBindConfig.final? = none ∧
    step refusingBindProgram refusingBindConfig = some (.request .log [1] s .init refusingK) ∧
    ¬ RequestResolution refusingBindProgram refusingBindOracle ⟨refusingBindConfig, [], []⟩
      .log [1] s .init refusingK
      (fabricatedFinalRun refusingBindProgram refusingBindOracle 1 [] [] refusingBindConfig) := by
  refine ⟨rfl, refusingBindRequest, ?_⟩
  generalize he :
    fabricatedFinalRun refusingBindProgram refusingBindOracle 1 [] [] refusingBindConfig = report
  intro h
  cases h with
  | unanswered ha _ _ =>
    simp [answer, refusingBindOracle] at ha
  | invalid ha hl _ _ =>
    simp [answer, refusingBindOracle] at ha
    subst ha
    simp [refusingBindOps, ops] at hl
  | refused _ _ _ _ _ =>
    have hend := congrArg QueryReport.ending he
    simp [fabricatedFinalRun] at hend
  | continued ha _ hk _ _ _ _ _ =>
    simp [answer, refusingBindOracle] at ha
    subst ha
    simp [refusingK, bindLife, refusingBindOps] at hk

/-- A genuine three-query execution revisits the first site after an unrelated request. -/
def threeRequestConfig : Config ops := ⟨.exec (.seq (.prim none .bump [] s)
  (.seq (.prim none .bump [] otherSite) (.prim none .bump [] s))), [], .init, []⟩
def thirdQuery : Query ops := Query.issue [] .bump [] s ⟨2, []⟩
  [interleavedQuery0, interleavedQuery1]

/-- A frozen revisited-site occurrence disagrees with the actual interval contribution. -/
example : ¬ ({ thirdQuery with occurrence := 0 } : Query ops).occurrence =
    interleavedQuery0.occurrence + occurrenceAt
      (((runQ prog observationEnv 7 threeRequestConfig).queries.drop 0).take (2 - 0)) s := by decide

/-- Instance (law `runFromQ_interleaved_site`, case `threeRequestConfig`). -/
example : thirdQuery.occurrence = interleavedQuery0.occurrence +
    occurrenceAt (((runFromQ prog observationEnv 7 [] [] threeRequestConfig).queries.drop 0).take (2 - 0))
      interleavedQuery0.site :=
  runFromQ_interleaved_site (O := ops) (P := prog) (ρ := observationEnv.toQueryOracle)
    7 [] [] threeRequestConfig QueryHistoryValid.nil (i := 0) (j := 2)
    (q := interleavedQuery0) (q' := thirdQuery) (by decide) rfl rfl rfl

/-- A mutation shares the first two sites' counters by rewriting the second site's identity.
The two endpoint queries retain exactly the identities used by the law's instance. -/
def sharedSiteRun {O : Operands} (P : Program O) (ρ : QueryOracle O) (fuel : Nat)
    (events : List O.Event) (issued : List (Query O)) (c : Config O) : QueryReport O :=
  let report := runFromQ P ρ fuel events issued c
  { report with queries := report.queries.map fun q =>
      if q.site = ⟨1⟩ then { q with site := ⟨0⟩ } else q }

/-- Planted (law `runFromQ_interleaved_site`, case `threeRequestConfig`,
subject `runFromQ`, mutant `sharedSiteRun`). -/
example : QueryHistoryValid ([] : List (Query ops)) ∧ 0 ≤ 2 ∧
    (sharedSiteRun prog observationEnv 7 [] [] threeRequestConfig).queries[0]? = some interleavedQuery0 ∧
    (sharedSiteRun prog observationEnv 7 [] [] threeRequestConfig).queries[2]? = some thirdQuery ∧
    interleavedQuery0.site = thirdQuery.site ∧
    ¬ thirdQuery.occurrence = interleavedQuery0.occurrence +
      occurrenceAt (((sharedSiteRun prog observationEnv 7 [] [] threeRequestConfig).queries.drop 0).take
        (2 - 0)) interleavedQuery0.site := by
  exact ⟨QueryHistoryValid.nil, by decide, rfl, rfl, rfl, by decide⟩

/-- Instance (law `runFromQ_toReport`, case `terminalConfig`). -/
example : (runFromQ prog observationEnv.toQueryOracle 1 [] [] terminalConfig).toReport =
    runFrom prog observationEnv 1 [] terminalConfig :=
  runFromQ_toReport (O := ops) (P := prog) (ρ₀ := observationEnv) 1 [] [] terminalConfig

/-- Planted (law `runFromQ_toReport`, case `terminalConfig`,
subject `runFromQ`, mutant `droppedAnswerRun`). -/
example : (droppedAnswerRun prog observationEnv.toQueryOracle 1 [] [] terminalConfig).toReport ≠
    runFrom prog observationEnv 1 [] terminalConfig := by
  intro h
  have he := congrArg Report.events h
  cases he

/-- The successful primitive request's one-step endpoint. -/
def terminalFinished : Position ops := ⟨⟨.done, [], .init, []⟩, [1], [terminalQuery]⟩

/-- Helper (computation). A real path includes the oracle's event and its recorded query
(approved: PENDING).
Constructs a concrete one-step oracle-checked path, including its event and query. -/
theorem terminalPath : Moves prog observationEnv 1 ⟨terminalConfig, [], []⟩ terminalFinished :=
  .step (.prim (k := terminalK) (a := ⟨some 1, .ok 0, .init⟩)
    terminalRequest rfl rfl rfl) (.refl _)

/-- Instance (law `runFromQ_complete`, case `terminalConfig`). -/
example : runFromQ prog observationEnv.toQueryOracle 1 [] [] terminalConfig =
    ⟨terminalFinished.events, terminalFinished.queries, .final .normal .init⟩ :=
  runFromQ_complete (O := ops) (P := prog) (ρ := observationEnv.toQueryOracle) (n := 1)
    (pos := ⟨terminalConfig, [], []⟩) (pos' := terminalFinished) terminalPath
    (f := .normal) rfl

/-- Planted (law `runFromQ_complete`, case `terminalConfig`,
subject `runFromQ`, mutant `droppedAnswerRun`). -/
example : Moves prog observationEnv 1 ⟨terminalConfig, [], []⟩ terminalFinished ∧
    terminalFinished.config.final? = some (.normal : Final Int) ∧
    droppedAnswerRun prog observationEnv.toQueryOracle 1 [] [] terminalConfig ≠
      ⟨terminalFinished.events, terminalFinished.queries, .final .normal .init⟩ := by
  exact ⟨terminalPath, rfl, by intro h; cases h⟩

/-- Instance (law `runFromQ_sound`, case `terminalConfig`). -/
example : ∃ n pos, Moves prog noAnswerOracle n ⟨terminalConfig, [], []⟩ pos ∧
    (runFromQ prog noAnswerOracle 1 [] [] terminalConfig).events = pos.events ∧
    ∃ hc : Conclusion prog noAnswerOracle (runFromQ prog noAnswerOracle 1 [] [] terminalConfig).ending pos,
      ReportedQueries prog noAnswerOracle hc (runFromQ prog noAnswerOracle 1 [] [] terminalConfig).queries :=
  runFromQ_sound (O := ops) (P := prog) (ρ := noAnswerOracle) 1 [] [] terminalConfig

/-- Helper (inversion). No successful move answers this unanswered request (approved: APPR-integ-agy-0dbeba29e).
Inverts a purported move at the concrete unanswered request; neither a silent step
nor an answered primitive transition is possible. -/
theorem unanswered_no_move {pos : Position ops} :
    ¬ Move prog noAnswerOracle ⟨terminalConfig, [], []⟩ pos := by
  intro h
  cases h with
  | silent hs => rw [terminalRequest] at hs; cases hs
  | prim _ ha _ _ => simp [answer, noAnswerOracle] at ha

/-- Planted (law `runFromQ_sound`, case `terminalConfig`,
subject `runFromQ`, mutant `fabricatedFinalRun`). -/
example : ¬ (∃ n pos, Moves prog noAnswerOracle n ⟨terminalConfig, [], []⟩ pos ∧
    (fabricatedFinalRun prog noAnswerOracle 1 [] [] terminalConfig).events = pos.events ∧
    ∃ hc : Conclusion prog noAnswerOracle
        (fabricatedFinalRun prog noAnswerOracle 1 [] [] terminalConfig).ending pos,
      ReportedQueries prog noAnswerOracle hc
        (fabricatedFinalRun prog noAnswerOracle 1 [] [] terminalConfig).queries) := by
  rintro ⟨n, pos, path, _, hc, _⟩
  cases path with
  | refl =>
    have hf := Conclusion.final_config hc
    cases hf
  | step hm _ => exact unanswered_no_move hm

/-- An infinite-prefix mutation loses accumulated histories and restarts the configuration. -/
def forgetfulRunQ {O : Operands} (_ : Program O) (_ : QueryOracle O) (_ : Nat)
    (c : Config O) : QueryReport O := ⟨[], [], .outOfFuel c⟩

/-- Instance (law `InfiniteExecution.evaluator_prefix`, case `event_infinite`). -/
example : runQ prog observationEnv 4 eventConfig =
    ⟨(event_infinite.states 4).events, (event_infinite.states 4).queries,
      .outOfFuel (event_infinite.states 4).config⟩ :=
  InfiniteExecution.evaluator_prefix (O := ops) (P := prog) (ρ := observationEnv.toQueryOracle)
    (c := eventConfig) event_infinite 4

/-- Planted (law `InfiniteExecution.evaluator_prefix`, case `event_infinite`,
subject `runQ`, mutant `forgetfulRunQ`). -/
example : forgetfulRunQ prog observationEnv 4 eventConfig ≠
    ⟨(event_infinite.states 4).events, (event_infinite.states 4).queries,
      .outOfFuel (event_infinite.states 4).config⟩ := by
  intro h
  have he := congrArg QueryReport.events h
  cases he

/-- Instance (law `runFromQ_queryHistoryValid`, case `threeRequestConfig`). -/
example : QueryHistoryValid (runFromQ prog observationEnv 7 [] [] threeRequestConfig).queries :=
  runFromQ_queryHistoryValid (O := ops) (P := prog) (ρ := observationEnv.toQueryOracle)
    7 [] [] threeRequestConfig QueryHistoryValid.nil

/-- Planted (law `runFromQ_queryHistoryValid`, case `threeRequestConfig`,
subject `runFromQ`, mutant `sharedSiteRun`). -/
example : QueryHistoryValid ([] : List (Query ops)) ∧
    ¬ QueryHistoryValid (sharedSiteRun prog observationEnv 7 [] [] threeRequestConfig).queries := by
  refine ⟨QueryHistoryValid.nil, ?_⟩
  intro h
  have wrong := h 1 { interleavedQuery1 with site := s } rfl
  have impossible : (0 : Nat) = 1 := wrong
  cases impossible

/-- A budget-specific evaluator defect preserves the certificate's witness budget (10), but
incorrectly reports exhaustion at the larger budget used by the exported law (11). -/
def earlyStopRun {O : Operands} (P : Program O) (ρ : Oracle O) (fuel : Nat)
    (c : Config O) : Report O :=
  if fuel = 11 then ⟨[], .outOfFuel c⟩ else run P ρ fuel c

/-- Instance (law `TerminationEvidence.all_allowed_runs`, case `terminatingEvidence`). -/
example : ∃ fn c f finalStore, AList.lookup prog.fns terminatingDomain.function = some fn ∧
    enter fn [] .init = some c ∧ (run prog observationEnv 11 c).ending = .final f finalStore :=
  TerminationEvidence.all_allowed_runs (O := ops) (P := prog) (domain := terminatingDomain)
    terminatingEvidence observationEnv [] .init ⟨rfl, rfl, rfl⟩ 11 (by decide)

/-- Planted (law `TerminationEvidence.all_allowed_runs`, case `terminatingEvidence`,
subject `run`, mutant `earlyStopRun`). -/
example : terminatingDomain.calls observationEnv [] .init ∧
    terminatingEvidence.conditional.fuel terminatingEvidence.premises observationEnv [] .init ≤ 11 ∧
    ¬ (∃ fn c f finalStore, AList.lookup prog.fns terminatingDomain.function = some fn ∧
      enter fn [] .init = some c ∧ (earlyStopRun prog observationEnv 11 c).ending = .final f finalStore) := by
  refine ⟨⟨rfl, rfl, rfl⟩, by decide, ?_⟩
  rintro ⟨fn, c, f, finalStore, _, _, h⟩
  simp [earlyStopRun] at h

/-- The no-intervening-same-site specialization is exercised by the same three-query run. -/
example : thirdQuery.occurrence = interleavedQuery0.occurrence + 1 :=
  runFromQ_interleaved_other_sites (O := ops) (P := prog) (ρ := observationEnv.toQueryOracle)
    7 [] [] threeRequestConfig QueryHistoryValid.nil (i := 0) (j := 2)
    (q := interleavedQuery0) (q' := thirdQuery) (by decide) rfl rfl rfl (by
      intro k r hik hkj hkr
      have hk : k = 1 := by omega
      subst k
      have hr : some interleavedQuery1 = some r := hkr
      cases hr
      decide)

/-- A fuel-sensitive evaluator defect that agrees with `runFrom` up to fuel `1`, but drops
reported events at larger fuel budgets. -/
def extraFuelDropRunFrom {O : Operands} (P : Program O) (ρ : Oracle O) (fuel : Nat)
    (events : List O.Event) (c : Config O) : Report O :=
  if fuel ≤ 1 then runFrom P ρ fuel events c
  else ⟨[], (runFrom P ρ fuel events c).ending⟩

/-- Instance (law `runFrom_mono`, case `terminalConfig`). -/
example : runFrom prog observationEnv 2 [] terminalConfig = runFrom prog observationEnv 1 [] terminalConfig :=
  runFrom_mono (O := ops) (P := prog) (ρ₀ := observationEnv) 1 2 (by decide) [] terminalConfig
    (by intro c' h; cases h)

/-- Planted (law `runFrom_mono`, case `terminalConfig`,
subject `runFrom`, mutant `extraFuelDropRunFrom`). -/
example : 1 ≤ 2 ∧
    (∀ c', (extraFuelDropRunFrom prog observationEnv 1 [] terminalConfig).ending ≠ .outOfFuel c') ∧
    extraFuelDropRunFrom prog observationEnv 2 [] terminalConfig ≠
      extraFuelDropRunFrom prog observationEnv 1 [] terminalConfig := by
  refine ⟨by decide, ?_, ?_⟩
  · intro c' h
    cases h
  · intro h
    have he := congrArg Report.events h
    cases he

/-- Instance (law `runFrom_trace_prefix_le`, case `terminalConfig`). -/
example : (runFrom prog observationEnv 1 [] terminalConfig).events <+:
    (runFrom prog observationEnv 2 [] terminalConfig).events :=
  runFrom_trace_prefix_le (O := ops) (P := prog) (ρ₀ := observationEnv) 1 2 (by decide) [] terminalConfig

/-- Planted (law `runFrom_trace_prefix_le`, case `terminalConfig`,
subject `runFrom`, mutant `extraFuelDropRunFrom`). -/
example : 1 ≤ 2 ∧
    ¬ (extraFuelDropRunFrom prog observationEnv 1 [] terminalConfig).events <+:
      (extraFuelDropRunFrom prog observationEnv 2 [] terminalConfig).events := by
  refine ⟨by decide, ?_⟩
  rintro ⟨rest, h⟩
  cases h

/-- A reconstruction mutation skips every second transition by doubling the fuel budget. -/
def skippedStepRunFromQ {O : Operands} (P : Program O) (ρ : QueryOracle O) (fuel : Nat)
    (events : List O.Event) (issued : List (Query O)) (c : Config O) : QueryReport O :=
  runFromQ P ρ (2 * fuel) events issued c

/-- Instance (law `move_of_fuel_succ`, case `eventConfig`). -/
example : Move prog observationEnv
    ((runFromQ prog observationEnv 0 [] [] eventConfig).position eventConfig)
    ((runFromQ prog observationEnv (0 + 1) [] [] eventConfig).position eventConfig) :=
  move_of_fuel_succ (O := ops) (P := prog) (ρ := observationEnv.toQueryOracle) (n := 0)
    (events := []) (issued := []) (c := eventConfig) rfl rfl

/-- Planted (law `move_of_fuel_succ`, case `eventConfig`,
subject `runFromQ`, mutant `skippedStepRunFromQ`). Doubling the fuel budget skips every second
transition; both fuel prefixes still report `outOfFuel`, but no single machine transition links
the initial position to the two-step position. -/
example : (skippedStepRunFromQ prog observationEnv 0 [] [] eventConfig).isOutOfFuel = true ∧
    (skippedStepRunFromQ prog observationEnv (0 + 1) [] [] eventConfig).isOutOfFuel = true ∧
    ¬ Move prog observationEnv
      ((skippedStepRunFromQ prog observationEnv 0 [] [] eventConfig).position eventConfig)
      ((skippedStepRunFromQ prog observationEnv (0 + 1) [] [] eventConfig).position eventConfig) := by
  refine ⟨rfl, rfl, ?_⟩
  change ¬ Move prog observationEnv ⟨eventConfig, [], []⟩ ⟨eventConfig2, [], []⟩
  intro hm
  generalize hp : (⟨eventConfig2, [], []⟩ : Position ops) = pos' at hm
  cases hm with
  | silent hs =>
    cases hp
    cases hs
  | prim hs _ _ _ =>
    cases hs

/-- A nondeterministic execution mutant permits an extra stationary transition. -/
def stationaryOrMove {O : Operands} (P : Program O) (ρ : QueryOracle O)
    (x y : Position O) : Prop := Move P ρ x y ∨ x = y

/-- Instance (law `InfiniteExecution.unique`, case `spin_infinite`). -/
example : ExecutionUnique (Move prog observationEnv) ⟨spinConfig, [], []⟩ :=
  InfiniteExecution.unique (P := prog) (ρ := observationEnv) (c := spinConfig)

/-- Planted (law `InfiniteExecution.unique`, case `spin_infinite`,
subject `Move`, mutant `stationaryOrMove`). -/
example : ¬ ExecutionUnique (stationaryOrMove prog observationEnv) ⟨spinConfig, [], []⟩ := by
  intro unique
  apply unique
  refine ⟨spin_infinite.states, (fun _ => ⟨spinConfig, [], []⟩),
    spin_infinite.initial, rfl, (fun n => Or.inl (spin_infinite.step n)),
    (fun _ => Or.inr rfl), ?_⟩
  intro he
  have h1 := congrArg (fun f => (f 1).config) he
  cases h1

/-- A classification mutation that confuses external infinite interaction with raw internal
infinite interaction, ignoring the execution adapter's projection. -/
def rawInfiniteInteraction {O : Operands} {S : Observe.EventSignature} (P : Program O)
    (_ : Observe.ExecAdapter O S) (ρ : QueryOracle O) (c : Config O) : Prop :=
  InternalInfiniteInteraction P ρ c

/-- Instance (law `silentDivergence_not_infiniteInteraction`, case `erasingAdapter`). -/
example : ¬ InfiniteInteraction prog erasingAdapter observationEnv eventConfig :=
  silentDivergence_not_infiniteInteraction erasingAdapter erasing_silent_divergence

/-- Planted (law `silentDivergence_not_infiniteInteraction`, case `erasingAdapter`,
subject `InfiniteInteraction`, mutant `rawInfiniteInteraction`). -/
example : SilentDivergence prog erasingAdapter observationEnv eventConfig ∧
    ¬ ¬ rawInfiniteInteraction prog erasingAdapter observationEnv eventConfig :=
  ⟨erasing_silent_divergence, fun h => h erasing_internal_infinite⟩

/-- An execution-record mutation that drops the transition-step obligation (`step`), allowing a
stationary configuration sequence to pose as an infinite execution. -/
def stationaryExecution {O : Operands} (_ : Program O) (_ : QueryOracle O)
    (c : Config O) : Type :=
  { states : Nat → Position O // states 0 = ⟨c, [], []⟩ }

/-- Instance (law `not_infinite_of_final`, case `finishingConfig`). -/
example : ¬ Nonempty (InfiniteExecution prog observationEnv.toQueryOracle finishingConfig) :=
  not_infinite_of_final (O := ops) (P := prog) (ρ₀ := observationEnv) (fuel := 1)
    (c := finishingConfig) (f := .normal) rfl

/-- Planted (law `not_infinite_of_final`, case `finishingConfig`,
subject `InfiniteExecution`, mutant `stationaryExecution`). -/
example : (run prog observationEnv 1 finishingConfig).ending = .final .normal .init ∧
    ¬ ¬ Nonempty (stationaryExecution prog observationEnv.toQueryOracle finishingConfig) :=
  ⟨rfl, fun h => h ⟨⟨fun _ => ⟨finishingConfig, [], []⟩, rfl⟩⟩⟩

/-- A C1 oracle that leaves every primitive request unanswered. -/
def blockingEnv : Oracle ops := fun _ _ _ _ _ => none

/-- Instance (law `not_infinite_of_noAnswer`, case `terminalConfig`). -/
example : ¬ Nonempty (InfiniteExecution prog blockingEnv.toQueryOracle terminalConfig) :=
  not_infinite_of_noAnswer (O := ops) (P := prog) (ρ₀ := blockingEnv) (fuel := 1)
    (c := terminalConfig) (blocked := terminalConfig) rfl

/-- Planted (law `not_infinite_of_noAnswer`, case `terminalConfig`,
subject `InfiniteExecution`, mutant `stationaryExecution`). -/
example : (run prog blockingEnv 1 terminalConfig).ending = .noAnswer terminalConfig ∧
    ¬ ¬ Nonempty (stationaryExecution prog blockingEnv.toQueryOracle terminalConfig) :=
  ⟨rfl, fun h => h ⟨⟨fun _ => ⟨terminalConfig, [], []⟩, rfl⟩⟩⟩

/-- A blocking-persistence mutation that preserves the one-step blocked report, but fabricates
a final report at larger fuel budgets. -/
def resumeBlockedRun {O : Operands} (P : Program O) (ρ : Oracle O) (fuel : Nat)
    (c : Config O) : Report O :=
  if fuel ≤ 1 then run P ρ fuel c else ⟨[], .final .normal c.store⟩

/-- Instance (law `endlesslyBlocked_of_noAnswer`, case `terminalConfig`). -/
example : ∀ n, 1 ≤ n →
    (run prog blockingEnv n terminalConfig).ending = .noAnswer terminalConfig :=
  endlesslyBlocked_of_noAnswer (O := ops) (P := prog) (ρ₀ := blockingEnv) (fuel := 1)
    (c := terminalConfig) (blocked := terminalConfig) rfl

/-- Planted (law `endlesslyBlocked_of_noAnswer`, case `terminalConfig`,
subject `run`, mutant `resumeBlockedRun`). -/
example : (resumeBlockedRun prog blockingEnv 1 terminalConfig).ending = .noAnswer terminalConfig ∧
    ¬ (∀ n, 1 ≤ n →
      (resumeBlockedRun prog blockingEnv n terminalConfig).ending = .noAnswer terminalConfig) := by
  refine ⟨rfl, ?_⟩
  intro h
  have h2 := h 2 (by decide)
  cases h2

/-- Instance (law `TerminationEvidence.excludes_infinite`, case `terminatingEvidence`). -/
example : ¬ Nonempty (InfiniteExecution prog observationEnv.toQueryOracle (start boomFn [] .init)) :=
  TerminationEvidence.excludes_infinite (O := ops) (P := prog) (domain := terminatingDomain)
    terminatingEvidence observationEnv [] .init boomFn (start boomFn [] .init) rfl ⟨rfl, rfl, rfl⟩ rfl

/-- Planted (law `TerminationEvidence.excludes_infinite`, case `terminatingEvidence`,
subject `InfiniteExecution`, mutant `stationaryExecution`). -/
example : AList.lookup prog.fns terminatingDomain.function = some boomFn ∧
    terminatingDomain.calls observationEnv [] .init ∧
    enter boomFn [] .init = some (start boomFn [] .init) ∧
    ¬ ¬ Nonempty (stationaryExecution prog observationEnv.toQueryOracle (start boomFn [] .init)) :=
  ⟨rfl, ⟨rfl, rfl, rfl⟩, rfl, fun h => h ⟨⟨fun _ => ⟨start boomFn [] .init, [], []⟩, rfl⟩⟩⟩

end Demo

end ConcreteSpec.Core.Exec
