import ConcreteSpec.Spec.Adequacy

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

end ConcreteSpec.Core.Exec
