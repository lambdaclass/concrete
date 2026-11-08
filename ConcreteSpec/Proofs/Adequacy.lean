import ConcreteSpec.Spec.Adequacy
import ConcreteSpec.Proofs.Exec

/-!
# Evaluator adequacy and termination (`Spec/Adequacy.lean`, foundation contract C8)

- Query instrumentation is observationally inert (`runFromQ_toReport`), occurrences are numbered per
  site, and adding fuel only appends queries (`runFromQ_queries_prefix_le`).
- Reports have matching finite executions with exactly their issued queries (`runFromQ_sound`),
  final finite executions are obtained with every sufficient fuel budget (`runFrom_complete_le`),
  completed reports are fuel-monotone (`runFrom_mono`), and event traces are prefix-monotone
  (`runFrom_trace_prefix_le`).
- Infinite maximal executions and compatible fuel limits are interconvertible
  (`infinite_iff_fuelLimit`). Silent divergence and infinite interaction are distinct event
  classifications; `Demo.spin_infinite` witnesses the former. A blocked request keeps its `noAnswer`
  classification (`endlesslyBlocked_of_noAnswer`).
- Per-function termination evidence quantifies over every entry run admitted by its named premises
  and excludes infinite execution (`TerminationEvidence.excludes_infinite`).
-/

namespace ConcreteSpec.Core.Exec
open ConcreteSpec.Core

variable {O : Operands} {P : Program O} {ρ : QueryOracle O} {c : Config O}

/-- A query's occurrence is exactly the number of earlier requests at its site. -/
theorem issue_occurrence (events : List O.Event) (p : O.Prim) (args : List O.Val) (site : SiteId)
    (st : O.Store) (issued : List (Query O)) :
    (Query.issue events p args site st issued).occurrence = occurrenceAt issued site := rfl

/-- The next request at `site` has the next occurrence number; requests at other sites do not change
that number. -/
theorem issue_next_occurrence (issued : List (Query O)) (site : SiteId) (q : Query O) :
    occurrenceAt (issued ++ [q]) site =
      occurrenceAt issued site + if q.site = site then 1 else 0 := by
  simp [occurrenceAt, List.filter_append]
  split <;> simp_all

/-- A fixed query-level oracle is a function of the complete query alone. -/
theorem answer_deterministic (ρ : QueryOracle O) {q₁ q₂ : Query O} (h : q₁ = q₂) :
    answer ρ q₁ = answer ρ q₂ := congrArg (answer ρ) h

/-- C1's oracle answers exactly the corresponding query-level oracle that ignores `occurrence`. -/
theorem answer_toQueryOracle (ρ₀ : Oracle O) (q : Query O) :
    answer ρ₀.toQueryOracle q = ρ₀ q.events q.prim q.args q.site q.store := rfl

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
justifies its ending. `ReportedQueries` identifies the reported history exactly: it is the final
position's history, plus the terminal query for a request-stopping ending. -/
theorem runFromQ_sound : ∀ (fuel : Nat) (events : List O.Event) (issued : List (Query O))
    (c : Config O), ∃ n pos, Moves P ρ n ⟨c, events, issued⟩ pos ∧
      (runFromQ P ρ fuel events issued c).events = pos.events ∧
      Conclusion P ρ (runFromQ P ρ fuel events issued c).ending pos ∧
      ReportedQueries (runFromQ P ρ fuel events issued c).ending pos
        (runFromQ P ρ fuel events issued c).queries := by
  intro fuel
  induction fuel with
  | zero =>
    intro events issued c
    cases hf : c.final? with
    | some f => exact ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf],
        by simpa [runFromQ, hf] using (Conclusion.final (P := P) (ρ := ρ) hf),
        by simpa [runFromQ, hf] using
          (ReportedQueries.final (f := f) (pos := (⟨c, events, issued⟩ : Position O)))⟩
    | none => exact ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf],
        by simpa [runFromQ, hf] using (Conclusion.outOfFuel (P := P) (ρ := ρ) hf),
        by simpa [runFromQ, hf] using
          (ReportedQueries.outOfFuel (c := c) (events := events) (issued := issued))⟩
  | succ n ih =>
    intro events issued c
    cases hf : c.final? with
    | some f => exact ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf],
        by simpa [runFromQ, hf] using (Conclusion.final (P := P) (ρ := ρ) hf),
        by simpa [runFromQ, hf] using
          (ReportedQueries.final (f := f) (pos := (⟨c, events, issued⟩ : Position O)))⟩
    | none =>
      cases hs : step P c with
      | none => exact ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf, hs],
          by simpa [runFromQ, hf, hs] using (Conclusion.stuck (P := P) (ρ := ρ) hs),
          by simpa [runFromQ, hf, hs] using
            (ReportedQueries.stuck (c := c) (events := events) (issued := issued))⟩
      | some out =>
        cases out with
        | next c' =>
          obtain ⟨m, pos, hpath, he, hc, hr⟩ := ih events issued c'
          exact ⟨m + 1, pos, .step (.silent hs) hpath, by simpa [runFromQ, hf, hs] using he,
            by simpa [runFromQ, hf, hs] using hc, by simpa [runFromQ, hf, hs] using hr⟩
        | request p vs site st k =>
          let q := Query.issue events p vs site st issued
          cases ha : answer ρ q with
          | none => exact ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf, hs, q, ha],
              by simpa [runFromQ, hf, hs, q, ha] using
                (Conclusion.noAnswer (P := P) (ρ := ρ) hs ha),
              by simpa [runFromQ, hf, hs, q, ha] using
                (ReportedQueries.noAnswer (c := c) (events := events) (issued := issued) (q := q))⟩
          | some a =>
            cases hallow : O.allows p vs st a with
            | false => exact ⟨0, ⟨c, events, issued⟩, .refl _,
                by simp [runFromQ, hf, hs, q, ha, hallow],
                by simpa [runFromQ, hf, hs, q, ha, hallow] using
                  (Conclusion.badAnswer (P := P) (ρ := ρ) hs ha hallow),
                by simpa [runFromQ, hf, hs, q, ha, hallow] using
                  (ReportedQueries.badAnswer (c := c) (events := events) (issued := issued)
                    (q := q))⟩
            | true =>
              cases hk : k a with
              | none => exact ⟨0, ⟨c, events, issued⟩, .refl _,
                  by simp [runFromQ, hf, hs, q, ha, hallow, hk],
                  by simpa [runFromQ, hf, hs, q, ha, hallow, hk] using
                    (Conclusion.stuckContinuation (P := P) (ρ := ρ) hs ha hallow hk),
                  by simpa [runFromQ, hf, hs, q, ha, hallow, hk] using
                    (ReportedQueries.stuckContinuation (c := c) (events := events)
                      (issued := issued) (q := q))⟩
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

/-- Every infinite execution has either finitely many events or infinitely many; these are the two
distinct classes `SilentDivergence` and `InfiniteInteraction`. -/
theorem finite_or_infinite_events (X : InfiniteExecution P ρ c) :
    FiniteEvents X ∨ InfiniteEvents X := by
  classical
  by_cases h : ∃ bound, ∀ n, (X.states n).events.length ≤ bound
  · exact .inl h
  · refine .inr (fun bound => ?_)
    apply Classical.byContradiction
    intro hn
    apply h
    refine ⟨bound, fun n => ?_⟩
    exact Nat.le_of_not_gt (fun hlt => hn ⟨n, hlt⟩)

/-- Silent divergence cannot also be infinite interaction. -/
theorem finite_not_infinite_events {X : InfiniteExecution P ρ c}
    (hf : FiniteEvents X) (hi : InfiniteEvents X) : False := by
  obtain ⟨bound, hb⟩ := hf
  obtain ⟨n, hn⟩ := hi bound
  exact Nat.not_lt_of_ge (hb n) hn

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

/-- The evidence field applies to every oracle, argument list, store, and entry configuration that
satisfies the named premises. -/
theorem TerminationEvidence.all_allowed_runs (E : TerminationEvidence O P)
    (premises : E.Premises) (ρ : Oracle O) (args : List O.Val) (st : O.Store)
    (fn : Fn O) (c : Config O) (hf : AList.lookup P.fns E.function = some fn)
    (ha : E.admits premises ρ args st) (he : enter fn args st = some c) :
    ∃ f, (run P ρ (E.fuel premises ρ args st) c).ending = .final f :=
  E.terminates premises ρ args st fn c hf ha he

/-- Per-function termination evidence excludes divergence from every allowed entry run. -/
theorem TerminationEvidence.excludes_infinite (E : TerminationEvidence O P)
    (premises : E.Premises) (ρ : Oracle O) (args : List O.Val) (st : O.Store)
    (fn : Fn O) (c : Config O) (hf : AList.lookup P.fns E.function = some fn)
    (ha : E.admits premises ρ args st) (he : enter fn args st = some c)
    (X : InfiniteExecution P ρ c) : False := by
  obtain ⟨f, hfinal⟩ := E.all_allowed_runs premises ρ args st fn c hf ha he
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

/-- In particular it is silent divergence: infinitely many steps and no events at all. -/
theorem spin_silent_divergence : SilentDivergence prog env spinConfig :=
  ⟨spin_infinite, ⟨0, fun n => by simp [spin_infinite, spinPosition]⟩⟩

end Demo

end ConcreteSpec.Core.Exec
