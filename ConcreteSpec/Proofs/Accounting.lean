import ConcreteSpec.Spec.Accounting
import ConcreteSpec.Spec.Ident

/-!
# C7 accounting laws and definition controls

The twelve Laws establish conservation, exact trajectory peaks and report certification.
Each has populated Instance evidence and premise-preserving, single-rule Planted counterexamples.
Authority: docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests; classification:
docs/decisions.md#decided-2026-11-13-proof-labels.
-/

namespace ConcreteSpec.Core.Leak

/-! ## Accounting conservation -/

/-- Law (subject `Account.alloc`). Zero-sized requests invent no bytes.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
theorem Account.alloc_zero (a : Account) (h : a.WF) : a.alloc 0 = a := by
  cases a
  simp_all [Account.alloc, Account.WF]

/-- Representation lemma. Functional update identity.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-memory-2026-11-06.

Writing back the existing account at a lineage is the identity on `Memory`. -/
theorem Memory.set_same (m : Memory) (lineage : LineageId) :
    Memory.set m lineage (m lineage) = m := by
  funext l
  by_cases h : l = lineage <;> simp [Memory.set, h]

/-- Helper (arithmetic). Initializes the local peak at live bytes and retains the historical
peak bound; used to initialize `function_entry_peak`. -/
theorem Account.enter_wf (a : Account) (h : a.WF) : a.enter.WF := by
  cases a
  simp_all [Account.enter, Account.WF]

/-- Helper (arithmetic). Upper-bounds live + bytes by both `max` expressions in `Account.alloc` for single-step WF preservation. -/
theorem Account.alloc_wf (a : Account) (bytes : Nat) : (a.alloc bytes).WF := by
  exact ⟨Nat.le_max_right _ _, Nat.le_max_right _ _⟩

/-- Helper (arithmetic). Uses `Nat.sub_le` transitivity to preserve peak bounds across `Account.free` for single-step WF preservation. -/
theorem Account.free_wf (a : Account) (bytes : Nat) (h : a.WF) : (a.free bytes).WF := by
  exact ⟨Nat.le_trans (Nat.sub_le _ _) h.1, Nat.le_trans (Nat.sub_le _ _) h.2⟩

/-- Helper (arithmetic). Shows zero live bytes after `Account.reset` satisfy the non-negative peak bounds for single-step WF preservation. -/
theorem Account.reset_wf (a : Account) : a.reset.WF := by
  simp [Account.WF, Account.reset]

/-- Helper (inversion). Lifts `Account.alloc_wf`, `Account.free_wf`, and `Account.reset_wf` across one successful `apply` transition for `run_wf` and `run_peak_exact`.

Every successful accounting action preserves well-formedness at every lineage. -/
theorem apply_wf {m : Memory} {action : Action} {m' : Memory}
    (h : apply m action = some m') (hwf : ∀ l, (m l).WF) : ∀ l, (m' l).WF := by
  cases action with
  | alloc lineage bytes =>
    cases h
    intro l
    by_cases hl : l = lineage
    · subst l
      simpa [Memory.set] using Account.alloc_wf (m lineage) bytes
    · simpa [Memory.set, hl] using hwf l
  | free lineage bytes =>
    simp only [apply] at h
    split at h
    next hle =>
      cases h
      intro l
      by_cases hl : l = lineage
      · subst l
        simpa [Memory.set] using Account.free_wf (m lineage) bytes (hwf lineage)
      · simpa [Memory.set, hl] using hwf l
    next => cases h
  | reset lineage =>
    cases h
    intro l
    by_cases hl : l = lineage
    · subst l
      simpa [Memory.set] using Account.reset_wf (m lineage)
    · simpa [Memory.set, hl] using hwf l

/-- Law (subject `run`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-memory-2026-11-06.

Every successful sequence preserves well-formedness at every lineage. -/
theorem run_wf (m : Memory) (actions : List Action)
    (hwf : ∀ l, (m l).WF) (hrun : (run m actions).isSome = true) :
    ∃ m', run m actions = some m' ∧ ∀ l, (m' l).WF := by
  obtain ⟨m', h⟩ := Option.isSome_iff_exists.mp hrun
  clear hrun
  refine ⟨m', h, ?_⟩
  induction actions generalizing m with
  | nil => cases h; exact hwf
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      exact ih m₁ (apply_wf ha hwf) hr

/-- Law (subject `apply`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-memory-2026-11-06.

A zero-byte allocation changes no account at any lineage. -/
theorem apply_alloc_zero {m : Memory} (hwf : ∀ l, (m l).WF) (lineage : LineageId) :
    apply m (.alloc lineage 0) = some m := by
  simp only [apply, Account.alloc_zero (m lineage) (hwf lineage), Memory.set_same]

/-- Helper (computation). Exposes the fields of a successful free and its frame for the
independent byte-balance theorem `free_conservation`. -/
theorem free_update (m : Memory) (lineage : LineageId) (bytes : Nat)
    (hle : bytes ≤ (m lineage).live) :
    ∃ m', apply m (.free lineage bytes) = some m' ∧
      (m' lineage).requested = (m lineage).requested ∧
      (m' lineage).live = (m lineage).live - bytes ∧
      (m' lineage).peak = (m lineage).peak ∧
      (m' lineage).functionPeak = (m lineage).functionPeak ∧
      ∀ l, l ≠ lineage → m' l = m l := by
  refine ⟨Memory.set m lineage ((m lineage).free bytes), ?_, ?_⟩
  · simp [apply, hle]
  · simp only [Memory.set, ↓reduceIte]
    refine ⟨rfl, rfl, rfl, rfl, ?_⟩
    intro l hl
    simp [hl]

/-- Law (subject `ActionPermitted`). Free succeeds exactly when its outflow is owned.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-memory-2026-11-06. Both implications are required. -/
theorem free_permitted_iff (m : Memory) (l : LineageId) (bytes : Nat) :
    ActionPermitted m (.free l bytes) ↔ bytes ≤ (m l).live := by
  simp [ActionPermitted, apply]

/-- Law (subject `apply`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-memory-2026-11-06.

A successful free balances the remaining bytes plus its outflow against the bytes owned before
it, while retaining request history, both peaks, and all other lineages. -/
theorem free_conservation (m : Memory) (lineage : LineageId) (bytes : Nat)
    (hle : bytes ≤ (m lineage).live) :
    ∃ m', apply m (.free lineage bytes) = some m' ∧
      (m' lineage).requested = (m lineage).requested ∧
      (m' lineage).live + bytes = (m lineage).live ∧
      (m' lineage).peak = (m lineage).peak ∧
      (m' lineage).functionPeak = (m lineage).functionPeak ∧
      ∀ l, l ≠ lineage → m' l = m l := by
  obtain ⟨m', h, hreq, hlive, hp, hfp, hframe⟩ := free_update m lineage bytes hle
  refine ⟨m', h, hreq, ?_, hp, hfp, hframe⟩
  omega

/-- Law (subject `apply`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-memory-2026-11-06.

A reset satisfies the independent `ResetConservation` predicate with the actual live bytes before
the transition as its outflow, and leaves every other lineage unchanged. -/
theorem reset_conservation (m : Memory) (lineage : LineageId) :
    ∃ m', apply m (.reset lineage) = some m' ∧
      ResetConservation (m lineage) (m' lineage) (resetOutflow lineage m (.reset lineage)) ∧
      ∀ l, l ≠ lineage → m' l = m l := by
  refine ⟨Memory.set m lineage (m lineage).reset, rfl, ?_, ?_⟩
  · simp [ResetConservation, resetOutflow, Memory.set, Account.reset]
  · intro l hl
    simp [Memory.set, hl]

/-- Helper (computation). Unfolds one cons step of `resetBytes` on a successful `apply` transition for `run_conservation`. -/
@[simp] theorem resetBytes_cons {m m' : Memory} {action : Action} {rest : List Action}
    (lineage : LineageId) (h : apply m action = some m') :
    resetBytes lineage m (action :: rest) =
      resetOutflow lineage m action + resetBytes lineage m' rest := by
  simp [resetBytes, h]

/-- Law (subject `run`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-memory-2026-11-06.

**Whole-sequence conservation.** Requested bytes increase by exactly the sequence's allocation
requests. Over the integers, net live bytes equal allocations minus explicit frees and reset
outflows read from the actual intermediate states; no truncated natural subtraction hides an
outflow. The statement holds independently for every allocator lineage, including untouched ones. -/
theorem run_conservation (m : Memory) (actions : List Action) (lineage : LineageId)
    (hrun : (run m actions).isSome = true) :
    ∃ m', run m actions = some m' ∧
      (m' lineage).requested = (m lineage).requested + allocated lineage actions ∧
      ((m' lineage).live : Int) = (m lineage).live + allocated lineage actions -
        freed lineage actions - resetBytes lineage m actions := by
  obtain ⟨m', h⟩ := Option.isSome_iff_exists.mp hrun
  clear hrun
  refine ⟨m', h, ?_⟩
  induction actions generalizing m with
  | nil => cases h; simp [allocated, freed, resetBytes]
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      have hc := ih m₁ hr
      rw [resetBytes_cons lineage ha]
      cases action with
      | alloc l bytes =>
        simp only [apply, Option.some.injEq] at ha
        subst m₁
        by_cases hl : l = lineage
        · subst l
          simp [Memory.set, Account.alloc, allocated, freed, resetOutflow] at hc ⊢
          omega
        · simp [Memory.set, hl, Ne.symm hl, allocated, freed, resetOutflow] at hc ⊢
          omega
      | free l bytes =>
        by_cases hle : bytes ≤ (m l).live
        · simp only [apply, hle, ↓reduceIte, Option.some.injEq] at ha
          subst m₁
          by_cases hl : l = lineage
          · subst l
            simp [Memory.set, Account.free, allocated, freed, resetOutflow] at hc ⊢
            omega
          · simp [Memory.set, hl, Ne.symm hl, allocated, freed, resetOutflow] at hc ⊢
            omega
        · simp [apply, hle] at ha
      | reset l =>
        simp only [apply, Option.some.injEq] at ha
        subst m₁
        by_cases hl : l = lineage
        · subst l
          simp [Memory.set, Account.reset, allocated, freed, resetOutflow] at hc ⊢
          omega
        · simp [Memory.set, hl, Ne.symm hl, allocated, freed, resetOutflow] at hc ⊢
          omega

/-- Helper (arithmetic). Shows one successful `apply` transition never decreases peak or functionPeak for `run_peak_monotone`.

No successful action lowers either the historical or function-local peak of any lineage. -/
theorem apply_peak_monotone {m m' : Memory} {action : Action}
    (h : apply m action = some m') (lineage : LineageId) :
    (m lineage).peak ≤ (m' lineage).peak ∧
      (m lineage).functionPeak ≤ (m' lineage).functionPeak := by
  cases action with
  | alloc l bytes =>
    cases h
    by_cases hl : l = lineage
    · subst l
      simp [Memory.set, Account.alloc, Nat.le_max_left]
    · simp [Memory.set, Ne.symm hl]
  | free l bytes =>
    simp only [apply] at h
    split at h
    next =>
      cases h
      by_cases hl : l = lineage
      · subst l; simp [Memory.set, Account.free]
      · simp [Memory.set, Ne.symm hl]
    next => cases h
  | reset l =>
    cases h
    by_cases hl : l = lineage
    · subst l; simp [Memory.set, Account.reset]
    · simp [Memory.set, Ne.symm hl]

/-- Law (subject `run`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-memory-2026-11-06.

No successful sequence lowers either the historical or function-local peak of any lineage. -/
theorem run_peak_monotone (m : Memory) (actions : List Action) (lineage : LineageId)
    (hrun : (run m actions).isSome = true) :
    ∃ m', run m actions = some m' ∧
      (m lineage).peak ≤ (m' lineage).peak ∧
      (m lineage).functionPeak ≤ (m' lineage).functionPeak := by
  obtain ⟨m', h⟩ := Option.isSome_iff_exists.mp hrun
  clear hrun
  refine ⟨m', h, ?_⟩
  induction actions generalizing m with
  | nil => cases h; exact ⟨Nat.le_refl _, Nat.le_refl _⟩
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      have ha' := apply_peak_monotone ha lineage
      have hr' := ih m₁ hr
      exact ⟨Nat.le_trans ha'.1 hr'.1, Nat.le_trans ha'.2 hr'.2⟩

/-- Helper (computation). Expresses the post-step functionPeak as `max` of the pre-step functionPeak and post-step live bytes for `run_peak_exact`.

Each successful transition records the maximum of its old peak and new live bytes. -/
theorem apply_peak_exact {m m' : Memory} {action : Action}
    (hwf : ∀ l, (m l).WF) (h : apply m action = some m') (lineage : LineageId) :
    (m' lineage).functionPeak = max (m lineage).functionPeak (m' lineage).live := by
  cases action with
  | alloc l bytes =>
    cases h
    by_cases hl : lineage = l
    · subst lineage; simp [Memory.set, Account.alloc]
    · simp [Memory.set, hl, Nat.max_eq_left (hwf lineage).2]
  | free l bytes =>
    simp only [apply] at h
    split at h
    next =>
      cases h
      by_cases hl : lineage = l
      · subst lineage
        simp only [Memory.set, ↓reduceIte, Account.free]
        exact (Nat.max_eq_left (Nat.le_trans (Nat.sub_le _ _) (hwf l).2)).symm
      · simp [Memory.set, hl, Nat.max_eq_left (hwf lineage).2]
    next => cases h
  | reset l =>
    cases h
    by_cases hl : lineage = l
    · subst lineage; simp [Memory.set, Account.reset]
    · simp [Memory.set, hl, Nat.max_eq_left (hwf lineage).2]

/-- Law (subject `run`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-memory-2026-11-06.

The recorded local peak equals the maximum of the entry functionPeak and the independently
observed maximum `observedPeak` over the live-byte trajectory. -/
theorem run_peak_exact (m : Memory) (actions : List Action) (lineage : LineageId)
    (hwf : ∀ l, (m l).WF) (hrun : (run m actions).isSome = true) :
    ∃ m', run m actions = some m' ∧
      (m' lineage).functionPeak = max (m lineage).functionPeak (observedPeak lineage m actions) := by
  obtain ⟨m', h⟩ := Option.isSome_iff_exists.mp hrun
  clear hrun
  refine ⟨m', h, ?_⟩
  induction actions generalizing m with
  | nil =>
    cases h
    exact (Nat.max_eq_left (hwf lineage).2).symm
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      rw [ih m₁ (apply_wf ha hwf) hr, apply_peak_exact hwf ha lineage]
      simp only [observedPeak, ha]
      rw [Nat.max_assoc]
      have hrest : (m₁ lineage).live ≤ observedPeak lineage m₁ rest := by
        cases rest with
        | nil => exact Nat.le_refl _
        | cons a xs =>
          simp only [observedPeak]
          split
          · exact Nat.le_refl _
          · exact Nat.le_max_left _ _
      rw [Nat.max_eq_right hrest, ← Nat.max_assoc, Nat.max_eq_left (hwf lineage).2]

/-- Helper (arithmetic). A successful step records the exact historical high-water mark. -/
theorem apply_historical_peak_exact {m m' : Memory} {action : Action}
    (hwf : ∀ l, (m l).WF) (h : apply m action = some m') (lineage : LineageId) :
    (m' lineage).peak = max (m lineage).peak (m' lineage).live := by
  cases action with
  | alloc l bytes =>
    cases h
    by_cases hl : lineage = l
    · subst lineage; simp [Memory.set, Account.alloc]
    · simp [Memory.set, hl, Nat.max_eq_left (hwf lineage).1]
  | free l bytes =>
    simp only [apply] at h
    split at h
    next =>
      cases h
      by_cases hl : lineage = l
      · subst lineage
        simp only [Memory.set, ↓reduceIte, Account.free]
        exact (Nat.max_eq_left (Nat.le_trans (Nat.sub_le _ _) (hwf l).1)).symm
      · simp [Memory.set, hl, Nat.max_eq_left (hwf lineage).1]
    next => cases h
  | reset l =>
    cases h
    by_cases hl : lineage = l
    · subst lineage; simp [Memory.set, Account.reset]
    · simp [Memory.set, hl, Nat.max_eq_left (hwf lineage).1]

/-- Law (subject `run`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-memory-2026-11-06.

The recorded historical peak equals the maximum of the entry historical peak and the independently
observed maximum `observedPeak` over the live-byte trajectory. -/
theorem run_historical_peak_exact (m : Memory) (actions : List Action) (lineage : LineageId)
    (hwf : ∀ l, (m l).WF) (hrun : (run m actions).isSome = true) :
    ∃ m', run m actions = some m' ∧
      (m' lineage).peak = max (m lineage).peak (observedPeak lineage m actions) := by
  obtain ⟨m', h⟩ := Option.isSome_iff_exists.mp hrun
  clear hrun
  refine ⟨m', h, ?_⟩
  induction actions generalizing m with
  | nil =>
    cases h
    exact (Nat.max_eq_left (hwf lineage).1).symm
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      rw [ih m₁ (apply_wf ha hwf) hr, apply_historical_peak_exact hwf ha lineage]
      simp only [observedPeak, ha]
      rw [Nat.max_assoc]
      have hrest : (m₁ lineage).live ≤ observedPeak lineage m₁ rest := by
        cases rest with
        | nil => exact Nat.le_refl _
        | cons a xs =>
          simp only [observedPeak]
          split
          · exact Nat.le_refl _
          · exact Nat.le_max_left _ _
      rw [Nat.max_eq_right hrest, ← Nat.max_assoc, Nat.max_eq_left (hwf lineage).1]

/-- Law (subject `Account.enter`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-memory-2026-11-06.

Initializing function accounting and then executing a successful action sequence records exactly
its independently observed live-byte maximum, with no contribution from a caller's local peak. -/
theorem function_entry_peak (m : Memory) (actions : List Action) (lineage : LineageId)
    (hwf : ∀ l, (m l).WF)
    (hrun : (run (fun l => (m l).enter) actions).isSome = true) :
    ∃ m', run (fun l => (m l).enter) actions = some m' ∧
      (m' lineage).functionPeak = observedPeak lineage (fun l => (m l).enter) actions := by
  obtain ⟨m', h, hp⟩ := run_peak_exact (fun l => (m l).enter) actions lineage
    (fun l => Account.enter_wf (m l) (hwf l)) hrun
  refine ⟨m', h, ?_⟩
  have hentry : (m lineage).live ≤ observedPeak lineage (fun l => (m l).enter) actions := by
    cases actions with
    | nil => exact Nat.le_refl _
    | cons a xs =>
      simp only [observedPeak]
      split
      · exact Nat.le_refl _
      · exact Nat.le_max_left _ _
  change (m' lineage).functionPeak =
    max (m lineage).live (observedPeak lineage (fun l => (m l).enter) actions) at hp
  rw [Nat.max_eq_right hentry] at hp
  exact hp

/-- Law (subject `run`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-memory-2026-11-06.

Independent analysis bounds certify any `LineageReport` (whether all bounds are established, some
fields are .notEstablished, or the allocator defines no footprint) for an actual function run.
Peak evidence is derived from the independent live-state trajectory `observedPeak` and function
entry initialization (m lineage).functionPeak = (m lineage).live, never from the exit peak. -/
theorem report_certified (m : Memory) (actions : List Action) (allocator : AllocatorContract)
    (lineage : LineageId) (r : LineageReport)
    (hlineage : r.lineage = lineage)
    (hwf : ∀ l, (m l).WF)
    (hentry : (m lineage).functionPeak = (m lineage).live)
    (hrun : (run m actions).isSome = true)
    (hrequested : Bound.Sound (allocated lineage actions) r.requested)
    (hnet : SignedInterval.Sound ((allocated lineage actions : Int) - freed lineage actions -
      resetBytes lineage m actions) r.netChange)
    (hpeak : Bound.Sound (observedPeak lineage m actions - (m lineage).live) r.peak)
    (hfootprint : Bound.SoundOptional (allocator.footprint lineage m actions) r.footprint) :
    ∃ m', run m actions = some m' ∧
      LineageReport.Sound lineage (m lineage) (m' lineage)
        (allocator.footprint lineage m actions) r := by
  obtain ⟨m', h, hreqEq, hliveEq⟩ := run_conservation m actions lineage hrun
  obtain ⟨m'', h', hpeakEq⟩ := run_peak_exact m actions lineage hwf hrun
  rw [h] at h'
  cases h'
  refine ⟨m', h, hlineage, ?_, ?_, ?_, hfootprint⟩
  · have hd : (m' lineage).requested - (m lineage).requested = allocated lineage actions := by omega
    rw [hd]
    exact hrequested
  · have hn : ((m' lineage).live : Int) - ((m lineage).live : Int) =
        (allocated lineage actions : Int) - freed lineage actions -
          resetBytes lineage m actions := by omega
    rw [hn]
    exact hnet
  · intro hstatus
    change (m' lineage).functionPeak - (m lineage).live ≤ r.peak.limit
    rw [hpeakEq, hentry]
    have hobserved : (m lineage).live ≤ observedPeak lineage m actions := by
      cases actions with
      | nil => exact Nat.le_refl _
      | cons a xs =>
        simp only [observedPeak]
        split
        · exact Nat.le_refl _
        · exact Nat.le_max_left _ _
    rw [Nat.max_eq_right hobserved]
    exact hpeak hstatus

end ConcreteSpec.Core.Leak
