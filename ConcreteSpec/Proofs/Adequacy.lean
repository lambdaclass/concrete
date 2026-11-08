import ConcreteSpec.Spec.Adequacy
import ConcreteSpec.Proofs.Exec

/-!
# Oracle discipline for the reference evaluator

The query instrumentation is observationally inert (`runFromQ_toReport`). Occurrences are numbered
per site (`issue_occurrence`, `issue_next_occurrence`), answers are deterministic for a fixed oracle,
and adding one unit of fuel can only append queries (`runFromQ_queries_prefix`). Thus a query that a
run issues does not depend on the remaining fuel.
-/

namespace ConcreteSpec.Core.Exec
open ConcreteSpec.Core

variable {O : Operands} {P : Program O} {ρ : Oracle O}

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

/-- A fixed oracle is a function of the query alone. -/
theorem answer_deterministic (ρ : Oracle O) {q₁ q₂ : Query O} (h : q₁ = q₂) :
    answer ρ q₁ = answer ρ q₂ := congrArg (answer ρ) h

/-- Erasing query instrumentation gives C1's original driver, with the same events and ending. -/
theorem runFromQ_toReport : ∀ (fuel : Nat) (events : List O.Event) (issued : List (Query O))
    (c : Config O),
    (runFromQ P ρ fuel events issued c).toReport = runFrom P ρ fuel events c := by
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
          cases ha : answer ρ (Query.issue events p vs site st issued) with
          | none =>
            simp [answer, Query.issue] at ha
            simp [runFromQ, runFrom, hf, hs, answer, Query.issue, ha, QueryReport.toReport]
          | some a =>
            simp [answer, Query.issue] at ha
            cases hallow : O.allows p vs st a with
            | false => simp [runFromQ, runFrom, hf, hs, answer, Query.issue, ha, hallow,
                QueryReport.toReport]
            | true =>
              cases hk : k a with
              | none => simp [runFromQ, runFrom, hf, hs, answer, Query.issue, ha, hallow, hk,
                  QueryReport.toReport]
              | some c' =>
                simpa [runFromQ, runFrom, hf, hs, answer, Query.issue, ha, hallow, hk] using
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
justifies its ending. In particular, `final`, `stuck`, `noAnswer`, and `badAnswer` are witnessed by
exactly the steps and oracle answers that produced the report. -/
theorem runFromQ_sound : ∀ (fuel : Nat) (events : List O.Event) (issued : List (Query O))
    (c : Config O), ∃ n pos, Moves P ρ n ⟨c, events, issued⟩ pos ∧
      (runFromQ P ρ fuel events issued c).events = pos.events ∧
      (∃ later, (runFromQ P ρ fuel events issued c).queries = pos.queries ++ later) ∧
      Conclusion P ρ (runFromQ P ρ fuel events issued c).ending pos := by
  intro fuel
  induction fuel with
  | zero =>
    intro events issued c
    cases hf : c.final? with
    | some f => exact ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf],
        ⟨[], by simp [runFromQ, hf]⟩,
        by simpa [runFromQ, hf] using (Conclusion.final (P := P) (ρ := ρ) hf)⟩
    | none => exact ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf],
        ⟨[], by simp [runFromQ, hf]⟩,
        by simpa [runFromQ, hf] using (Conclusion.outOfFuel (P := P) (ρ := ρ) hf)⟩
  | succ n ih =>
    intro events issued c
    cases hf : c.final? with
    | some f => exact ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf],
        ⟨[], by simp [runFromQ, hf]⟩,
        by simpa [runFromQ, hf] using (Conclusion.final (P := P) (ρ := ρ) hf)⟩
    | none =>
      cases hs : step P c with
      | none => exact ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf, hs],
          ⟨[], by simp [runFromQ, hf, hs]⟩,
          by simpa [runFromQ, hf, hs] using (Conclusion.stuck (P := P) (ρ := ρ) hs)⟩
      | some out =>
        cases out with
        | next c' =>
          obtain ⟨m, pos, hpath, he, hq, hc⟩ := ih events issued c'
          exact ⟨m + 1, pos, .step (.silent hs) hpath, by simpa [runFromQ, hf, hs] using he,
            by simpa [runFromQ, hf, hs] using hq, by simpa [runFromQ, hf, hs] using hc⟩
        | request p vs site st k =>
          let q := Query.issue events p vs site st issued
          cases ha : answer ρ q with
          | none => exact ⟨0, ⟨c, events, issued⟩, .refl _, by simp [runFromQ, hf, hs, q, ha],
              ⟨[q], by simp [runFromQ, hf, hs, q, ha]⟩,
              by simpa [runFromQ, hf, hs, q, ha] using
                (Conclusion.noAnswer (P := P) (ρ := ρ) hs ha)⟩
          | some a =>
            cases hallow : O.allows p vs st a with
            | false => exact ⟨0, ⟨c, events, issued⟩, .refl _,
                by simp [runFromQ, hf, hs, q, ha, hallow],
                ⟨[q], by simp [runFromQ, hf, hs, q, ha, hallow]⟩,
                by simpa [runFromQ, hf, hs, q, ha, hallow] using
                  (Conclusion.badAnswer (P := P) (ρ := ρ) hs ha hallow)⟩
            | true =>
              cases hk : k a with
              | none => exact ⟨0, ⟨c, events, issued⟩, .refl _,
                  by simp [runFromQ, hf, hs, q, ha, hallow, hk],
                  ⟨[q], by simp [runFromQ, hf, hs, q, ha, hallow, hk]⟩,
                  by simpa [runFromQ, hf, hs, q, ha, hallow, hk] using
                    (Conclusion.stuckContinuation (P := P) (ρ := ρ) hs ha hallow hk)⟩
              | some c' =>
                obtain ⟨m, pos, hpath, he, hq, hc⟩ :=
                  ih (events ++ a.event.toList) (issued ++ [q]) c'
                exact ⟨m + 1, pos, .step (.prim hs ha hallow hk) hpath,
                  by simpa [runFromQ, hf, hs, q, ha, hallow, hk] using he,
                  by simpa [runFromQ, hf, hs, q, ha, hallow, hk] using hq,
                  by simpa [runFromQ, hf, hs, q, ha, hallow, hk] using hc⟩

/-- **Soundness for C1's `runFrom`.** Query erasure preserves the finite execution and its ending
justification. `Moves.toSteps` turns the same witness into the ordinary `Steps` relation. -/
theorem runFrom_sound (fuel : Nat) (events : List O.Event) (c : Config O) :
    ∃ n pos, Moves P ρ n ⟨c, events, []⟩ pos ∧
      (runFrom P ρ fuel events c).events = pos.events ∧
      Conclusion P ρ (runFrom P ρ fuel events c).ending pos := by
  obtain ⟨n, pos, hpath, he, hq, hc⟩ := runFromQ_sound (P := P) (ρ := ρ) fuel events [] c
  have hreport := runFromQ_toReport (P := P) (ρ := ρ) fuel events [] c
  have he' : (runFrom P ρ fuel events c).events = (runFromQ P ρ fuel events [] c).events := by
    simpa [QueryReport.toReport] using congrArg Report.events hreport.symm
  have hend : (runFrom P ρ fuel events c).ending = (runFromQ P ρ fuel events [] c).ending := by
    simpa [QueryReport.toReport] using congrArg Report.ending hreport.symm
  refine ⟨n, pos, hpath, he'.trans he, ?_⟩
  rw [hend]
  exact hc

/-- Every report extends the event history supplied to `runFrom`. -/
theorem runFrom_events_from (fuel : Nat) (events : List O.Event) (c : Config O) :
    ∃ tr, (runFrom P ρ fuel events c).events = events ++ tr := by
  obtain ⟨n, pos, hpath, he, hc⟩ := runFrom_sound (P := P) (ρ := ρ) fuel events c
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

/-- **Completeness for C1's `runFrom`.** Every finite execution to a final configuration is reported
with enough fuel, using the same oracle answers as that execution. -/
theorem runFrom_complete {n : Nat} {pos pos' : Position O} (h : Moves P ρ n pos pos')
    {f : Final O.Val} (hf : pos'.config.final? = some f) :
    runFrom P ρ n pos.events pos.config = ⟨pos'.events, .final f⟩ := by
  have hq := runFromQ_complete (P := P) (ρ := ρ) h hf
  have hr := runFromQ_toReport (P := P) (ρ := ρ) n pos.events pos.queries pos.config
  rw [hq] at hr
  simpa [QueryReport.toReport] using hr.symm

/-- **Monotonicity.** Extra fuel preserves every report other than `outOfFuel`. -/
theorem runFrom_mono : ∀ (n m : Nat), n ≤ m → ∀ (events : List O.Event) (c : Config O),
    (∀ c', (runFrom P ρ n events c).ending ≠ .outOfFuel c') →
    runFrom P ρ m events c = runFrom P ρ n events c := by
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
            have hnot' : ∀ c₀, (runFrom P ρ n events c').ending ≠ .outOfFuel c₀ := by
              simpa [runFrom, hf, hs] using hnot
            simpa [runFrom, hf, hs] using ih m hle' events c' hnot'
          | request p vs site st k =>
            cases ha : ρ events p vs site st with
            | none => simp [runFrom, hf, hs, ha]
            | some a =>
              cases hallow : O.allows p vs st a with
              | false => simp [runFrom, hf, hs, ha, hallow]
              | true =>
                cases hk : k a with
                | none => simp [runFrom, hf, hs, ha, hallow, hk]
                | some c' =>
                  have hnot' : ∀ c₀,
                      (runFrom P ρ n (events ++ a.event.toList) c').ending ≠ .outOfFuel c₀ := by
                    simpa [runFrom, hf, hs, ha, hallow, hk] using hnot
                  simpa [runFrom, hf, hs, ha, hallow, hk] using
                    ih m hle' (events ++ a.event.toList) c' hnot'

/-- **Trace-prefix consistency.** Increasing fuel never retracts or changes an event already
reported. -/
theorem runFrom_trace_prefix_le : ∀ (n m : Nat), n ≤ m → ∀ (events : List O.Event) (c : Config O),
    (runFrom P ρ n events c).events <+: (runFrom P ρ m events c).events := by
  intro n
  induction n with
  | zero =>
    intro m hle events c
    obtain ⟨tr, h⟩ := runFrom_events_from (P := P) (ρ := ρ) m events c
    refine ⟨tr, ?_⟩
    have h0 : (runFrom P ρ 0 events c).events = events := by
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
            cases ha : ρ events p vs site st with
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
theorem runFrom_trace_prefix (n : Nat) (events : List O.Event) (c : Config O) :
    (runFrom P ρ n events c).events <+: (runFrom P ρ (n + 1) events c).events :=
  runFrom_trace_prefix_le n (n + 1) (Nat.le_add_right n 1) events c

/-- A final report has an ordinary finite `Steps` witness ending in exactly that final
configuration, with the reported events. -/
theorem runFrom_final_sound {fuel : Nat} {events : List O.Event} {c : Config O} {f : Final O.Val}
    (h : (runFrom P ρ fuel events c).ending = .final f) :
    ∃ tr c', Steps P c tr c' ∧ c'.final? = some f ∧
      (runFrom P ρ fuel events c).events = events ++ tr := by
  obtain ⟨n, pos, hpath, he, hc⟩ := runFrom_sound (P := P) (ρ := ρ) fuel events c
  rw [h] at hc
  cases hc with
  | final hf =>
    obtain ⟨tr, later, hepos, hq, hsteps⟩ := Moves.toSteps hpath
    refine ⟨tr, pos.config, hsteps, hf, ?_⟩
    rw [he, hepos]

end ConcreteSpec.Core.Exec
