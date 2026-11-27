import ConcreteSpec.Spec.GrammarVocab
import ConcreteSpec.Spec.LL1
import ConcreteSpec.Spec.GrammarData

namespace ConcreteSpec.GrammarData

theorem nonterminal_index_inverse : ∀ i : Fin Nonterminal.count,
    (Nonterminal.ofNat i.val).ctorIdx = i.val := by decide

theorem nonterminal_roundTrip (n : Nonterminal) : Nonterminal.ofNat n.ctorIdx = n := by
  cases n <;> rfl

theorem productionIds_complete (p : ProductionId) : p ∈ productionIds := by
  cases p <;> decide

theorem productionIds_nodup : productionIds.Nodup := by decide

theorem projection_iff (slice : Nat) (p : ProductionId) : p ∈ projection slice ↔ AdmittedAt slice p := by
  simp [projection, AdmittedAt, productionIds_complete]

theorem projection_monotone {a b : Nat} (h : a ≤ b) {p : ProductionId}
    (hp : p ∈ projection a) : p ∈ projection b :=
  (projection_iff ..).2 (Nat.le_trans ((projection_iff ..).1 hp) h)

theorem terminalSlices_exact (t : ConcreteSpec.Surface0.Terminal) :
    terminalSlices[t.ctorIdx]? = some (ConcreteSpec.Surface0.terminalFirstSlice t) := by
  cases t <;> decide

theorem integerSlices_exact (r : ConcreteSpec.Surface0.Radix) :
    ∀ suffix : Fin 9, integerSlices[r.ctorIdx * 9 + suffix.val]? = some r.firstSlice := by
  cases r <;> decide

theorem witness_at_lookahead (p : ProductionId) :
    ∀ t, p.row.witness = some t → p.row.rhs.head? = some (.t t) := by
  cases p <;> simp [ProductionId.row, primitiveReturn]

theorem recognition_ll1 : Concrete.LL1.ll1 (erase productionIds) = true := by decide

theorem projection_saturates {s : Nat} (h : 11 ≤ s) : projection s = productionIds := by
  apply List.filter_eq_self.mpr
  intro p _
  have admitted : p.row.firstSlice ≤ 11 := by cases p <;> decide
  exact decide_eq_true (Nat.le_trans admitted h)

private theorem initial_projections_ll1 : ∀ s : Fin 3,
    Concrete.LL1.ll1 (erase (projection s.val)) = true := by decide

private theorem conditional_projection_ll1 : Concrete.LL1.ll1 (erase (projection 3)) = true := by decide
private theorem named_return_projection_ll1 : Concrete.LL1.ll1 (erase (projection 5)) = true := by decide

theorem projection_ll1 (slice : Nat) : Concrete.LL1.ll1 (erase (projection slice)) = true := by
  by_cases h : slice < 11
  · have distinct : ∀ s : Fin 11, s.val < 3 ∨
        projection s.val = projection 3 ∨ projection s.val = projection 5 := by decide
    rcases distinct ⟨slice,h⟩ with early | conditional | namedReturn
    · exact initial_projections_ll1 ⟨slice,early⟩
    · rw [show projection slice = projection 3 from conditional]
      exact conditional_projection_ll1
    · rw [show projection slice = projection 5 from namedReturn]
      exact named_return_projection_ll1
  · rw [projection_saturates (by omega)]
    exact recognition_ll1

theorem numberedGrammar_size (ids : List ProductionId) :
    (numberedGrammar ids).prods.length = ids.length := by simp [numberedGrammar,erase]

theorem terminal_admitted_by_row (p : ProductionId) :
    p.row.rhs.all (fun | .t t => ConcreteSpec.Surface0.terminalFirstSlice t ≤ p.row.firstSlice
                       | .n _ => true) = true := by cases p <;> decide

theorem word_spellings_unique : (ConcreteSpec.Surface0.wordTokens.map Prod.fst).Nodup := by decide
theorem word_terminals_unique : (ConcreteSpec.Surface0.wordTokens.map Prod.snd).Nodup := by decide

theorem reserved_word_coverage : ConcreteSpec.Surface0.wordTokens.all (fun (spelling,t) =>
    ConcreteSpec.Surface0.word spelling == t && t != .ident) = true := by decide

end ConcreteSpec.GrammarData
