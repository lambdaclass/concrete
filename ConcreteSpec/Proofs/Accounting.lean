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

namespace Evidence

/-- Helper definition. Transition-parametric fold for uniform planted substitution. -/
def run (m : Memory) (actions : List Action)
    (transition : Memory → Action → Option Memory := apply) : Option Memory :=
  match actions with
  | [] => some m
  | a :: rest => (transition m a).bind fun m' => run m' rest transition

/-- Helper definition. Transition-parametric fold for uniform planted substitution. -/
def resetBytes (lineage : LineageId) (m : Memory) (actions : List Action)
    (transition : Memory → Action → Option Memory := apply) : Nat :=
  match actions with
  | [] => 0
  | action :: rest =>
      match transition m action with
      | none => 0
      | some m' => resetOutflow lineage m action + resetBytes lineage m' rest transition

/-- Helper definition. Transition-parametric fold for uniform planted substitution. -/
def observedPeak (lineage : LineageId) (m : Memory) (actions : List Action)
    (transition : Memory → Action → Option Memory := apply) : Nat :=
  match actions with
  | [] => (m lineage).live
  | action :: rest =>
      match transition m action with
      | none => (m lineage).live
      | some m' => max (m lineage).live (observedPeak lineage m' rest transition)

/-- Helper (agreement). The evidence fold at `apply` is the normative execution. -/
theorem run_apply (m : Memory) (actions : List Action) :
    run m actions apply = ConcreteSpec.Core.Leak.run m actions := by
  induction actions generalizing m with
  | nil => rfl
  | cons a rest ih =>
    simp only [run, ConcreteSpec.Core.Leak.run]
    cases h : apply m a <;> simp [ih]

/-- Helper (agreement). Reset observation at `apply` is the normative reset observation. -/
theorem resetBytes_apply (l : LineageId) (m : Memory) (actions : List Action) :
    resetBytes l m actions apply = ConcreteSpec.Core.Leak.resetBytes l m actions := by
  induction actions generalizing m with
  | nil => rfl
  | cons a rest ih =>
    simp only [resetBytes, ConcreteSpec.Core.Leak.resetBytes]
    cases h : apply m a <;> simp [ih]

/-- Helper (agreement). Peak observation at `apply` is the normative independent observer. -/
theorem observedPeak_apply (l : LineageId) (m : Memory) (actions : List Action) :
    observedPeak l m actions apply = ConcreteSpec.Core.Leak.observedPeak l m actions := by
  induction actions generalizing m with
  | nil => rfl
  | cons a rest ih =>
    simp only [observedPeak, ConcreteSpec.Core.Leak.observedPeak]
    cases h : apply m a <;> simp [ih]

end Evidence

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

/-! ## Concrete accounting fixtures and definition controls -/

/-- A nonempty well-formed account and a second lineage with independent accounting. -/
def accountA : Account := ⟨100, 5, 12, 9⟩
def accountB : Account := ⟨7, 2, 2, 2⟩
def memoryA : Memory := Memory.set (Memory.set Memory.empty ⟨0⟩ accountA) ⟨1⟩ accountB

def conservationActions : List Action := [.alloc ⟨0⟩ 8, .free ⟨0⟩ 3, .reset ⟨0⟩]
def twoLineageActions : List Action :=
  [.alloc ⟨0⟩ 8, .alloc ⟨1⟩ 4, .free ⟨0⟩ 3, .reset ⟨0⟩,
    .alloc ⟨0⟩ 6, .reset ⟨1⟩, .reset ⟨0⟩]

/-- Peaks lowered below live bytes. -/
def badPeakAccount : Account := { accountA.alloc 10 with peak := 14, functionPeak := 14 }

/-- Helper (computation). Evaluates `Account.WF` across all lineages of the concrete `memoryA` fixture for Audit instances and mutants. -/
theorem memoryA_wf : ∀ l, (memoryA l).WF := by
  intro l
  by_cases h₀ : l = ⟨0⟩
  · subst l
    simp [memoryA, Memory.set, Account.WF, accountA]
  · by_cases h₁ : l = ⟨1⟩
    · subst l
      simp [memoryA, Memory.set, Account.WF, accountB]
    · simp [memoryA, Memory.set, h₀, h₁, Account.WF, Memory.empty, Account.empty]

/-- Representation lemma. Concrete initialization check showing `Account.empty` is well-formed and `Memory.empty` maps lineage 0 to `Account.empty`. -/
theorem empty_memory_has_well_formed_account : Account.empty.WF ∧ Memory.empty ⟨0⟩ = Account.empty := by
  simp [Account.WF, Memory.empty, Account.empty]

/-- Representation lemma. Concrete refusal check showing an account whose live exceeds peak and functionPeak fails `Account.WF`. -/
theorem peak_below_live_is_invalid : ¬ (⟨0, 5, 0, 0⟩ : Account).WF := by simp [Account.WF]

/-- Representation lemma. Concrete frame check showing `Memory.set` on lineage 0 leaves lineage 1 unchanged at `accountB`. -/
theorem setting_account_preserves_other_lineage : Memory.set memoryA ⟨0⟩ (accountA.alloc 1) ⟨1⟩ = accountB := rfl

/-- Representation lemma. Concrete refusal check showing `badPeakAccount` (where peaks are 14 but live is 15) fails `Account.WF`. -/
theorem bad_peak_account_is_invalid : ¬ badPeakAccount.WF := by
  simp [badPeakAccount, Account.WF, accountA, Account.alloc]

/-- Representation lemma. Concrete refusal check showing `run` returns `none` when an intermediate .free exceeds current live bytes. -/
theorem run_refuses_excessive_free : run Memory.empty [.alloc ⟨0⟩ 8, .free ⟨0⟩ 9, .alloc ⟨0⟩ 1] = none := by decide

/-- Representation lemma. Concrete witness showing live bytes (unlike peak and functionPeak) strictly decrease on a non-zero .free transition. -/
theorem free_can_release_entry_bytes : ∃ m m' : Memory, ∃ l : LineageId,
    apply m (.free l 3) = some m' ∧ ¬ (m l).live ≤ (m' l).live := by
  refine ⟨memoryA, Memory.set memoryA ⟨0⟩ (accountA.free 3), ⟨0⟩, rfl, ?_⟩
  decide

/-- Representation lemma. Concrete per-lineage sum check for `allocated` on interleaved `twoLineageActions`. -/
theorem allocation_totals_by_lineage : allocated ⟨0⟩ twoLineageActions = 14 ∧ allocated ⟨1⟩ twoLineageActions = 4 := by decide

/-- Representation lemma. Concrete per-lineage sum check for `freed` on interleaved `twoLineageActions`. -/
theorem free_totals_by_lineage : freed ⟨0⟩ twoLineageActions = 3 ∧ freed ⟨1⟩ twoLineageActions = 0 := by decide

/-- Representation lemma. Concrete per-lineage outflow check for `resetBytes` across multiple resets in `twoLineageActions`. -/
theorem reset_totals_by_lineage : resetBytes ⟨0⟩ memoryA twoLineageActions = 16 ∧
    resetBytes ⟨1⟩ memoryA twoLineageActions = 6 := by decide

/-- Representation lemma. Concrete check that `Bound.Sound` holds when an .boundEstablished limit is at least the actual value. -/
theorem bound_accepts_overestimate : Bound.Sound 10 ⟨12, .boundEstablished⟩ := by simp [Bound.Sound]

/-- Representation lemma. Concrete check that `Bound.Sound` fails when an .boundEstablished limit is strictly less than the actual value. -/
theorem bound_refuses_underestimate : ¬ Bound.Sound 10 ⟨9, .boundEstablished⟩ := by simp [Bound.Sound]

/-- Representation lemma. Concrete check that `Bound.SoundOptional` holds when an optional footprint is some 10 and the established limit is 12. -/
theorem optional_bound_accepts_established_fact : Bound.SoundOptional (some 10) ⟨12, .boundEstablished⟩ := by
  simp [Bound.SoundOptional]

/-- Representation lemma. Concrete check that `Bound.SoundOptional` fails when an optional footprint is some 10 and the established limit is 9. -/
theorem optional_bound_refuses_underestimate : ¬ Bound.SoundOptional (some 10) ⟨9, .boundEstablished⟩ := by
  simp [Bound.SoundOptional]

/-- Representation lemma. Concrete check that `Bound.SoundOptional` fails when the allocator footprint is `none` but the bound claims .boundEstablished. -/
theorem optional_bound_refuses_missing_fact : ¬ Bound.SoundOptional none ⟨12, .boundEstablished⟩ := by
  simp [Bound.SoundOptional]

/-- Example allocator contract: its footprint is the independent logical trajectory maximum.
A real allocator may include metadata and slack; no such equality is assumed generically. -/
def demoAllocator (transition : Memory → Action → Option Memory := apply) : AllocatorContract :=
  ⟨fun lineage m actions => some (Evidence.observedPeak lineage m actions transition)⟩

/-- Allocator contract that defines no footprint (`none`), exercising .notEstablished footprint
reports. -/
def noFootprintAllocator : AllocatorContract := ⟨fun _ _ _ => none⟩

/-- Entry for the concrete report example: 100 historical requested bytes and two live bytes. -/
def reportEntry : Account := ⟨100, 2, 100, 2⟩

def reportEntryMemory : Memory := Memory.set Memory.empty ⟨0⟩ reportEntry

def reportActions : List Action := [.alloc ⟨0⟩ 8, .free ⟨0⟩ 3]

def reportExit : Account := ⟨108, 7, 100, 10⟩

/-- Entry memory for an empty-run function after `Account.enter` on 100 historical requested bytes
and zero live bytes. -/
def emptyFunctionEntryMemory : Memory :=
  Memory.set Memory.empty ⟨0⟩ (Account.enter ⟨100, 0, 100, 0⟩)

/-- An independently written report with all fields established. -/
def establishedReport : LineageReport :=
  ⟨⟨0⟩, ⟨8, .boundEstablished⟩, ⟨5, 5, .boundEstablished⟩,
    ⟨8, .boundEstablished⟩, ⟨12, .boundEstablished⟩⟩

/-- A mixed-evidence report: net change is .notEstablished, while the other three fields are
.boundEstablished. -/
def mixedStatusReport : LineageReport :=
  ⟨⟨0⟩, ⟨8, .boundEstablished⟩, ⟨-100, 100, .notEstablished⟩,
    ⟨8, .boundEstablished⟩, ⟨12, .boundEstablished⟩⟩

/-- Report for an empty function run under `noFootprintAllocator`: zero local request, net, and
peak bounds are .boundEstablished, while footprint is .notEstablished. -/
def emptyFunctionReport : LineageReport :=
  ⟨⟨0⟩, ⟨0, .boundEstablished⟩, ⟨0, 0, .boundEstablished⟩,
    ⟨0, .boundEstablished⟩, ⟨0, .notEstablished⟩⟩

/-- Representation lemma. Concrete refusal check showing `LineageReport.Sound` rejects a report whose lineage field (⟨1⟩) does not match the queried lineage (⟨0⟩). -/
theorem report_refuses_wrong_lineage : ¬ LineageReport.Sound ⟨0⟩ reportEntry reportExit (some 10)
    { establishedReport with lineage := ⟨1⟩ } := by
  simp [LineageReport.Sound, establishedReport]


namespace Audit

/-- A mutant allocator invents one live byte even for a zero request. -/
def allocInventsByte (a : Account) (bytes : Nat) : Account :=
  { a.alloc bytes with live := a.live + bytes + 1 }

/-- Instance (law `Account.alloc_zero`, case `accountA`). -/
theorem alloc_zero_example_accountA : accountA.alloc 0 = accountA :=
  Account.alloc_zero accountA (by simp [Account.WF, accountA])

/-- Planted (law `Account.alloc_zero`, case `accountA`, subject `Account.alloc`, mutant `allocInventsByte`). -/
theorem alloc_zero_planted_accountA_allocInventsByte : accountA.WF ∧ ¬ (allocInventsByte accountA 0 = accountA) := by
  constructor
  · simp [Account.WF, accountA]
  · decide

/-- A setter with the same signature corrupts live even when writing back the existing account. -/
def corruptSetter (m : Memory) (lineage : LineageId) (a : Account) : Memory :=
  Memory.set m lineage { a with live := a.live + 1 }

/-- Representation lemma: a concrete control for a function-backed identity, not Law evidence. -/
theorem same_account_write_preserves_memory : Memory.set memoryA ⟨0⟩ (memoryA ⟨0⟩) = memoryA :=
  Memory.set_same memoryA ⟨0⟩

/-- Representation lemma: a concrete control for a function-backed identity, not Law evidence. -/
theorem corrupting_setter_changes_memory : ¬ (corruptSetter memoryA ⟨0⟩ (memoryA ⟨0⟩) = memoryA) := by
  intro h
  have hc := congrArg (fun m => (m ⟨0⟩).live) h
  change (6 : Nat) = 5 at hc
  omega

/-- Mutant function-entry operation that clears functionPeak to zero instead of `a.live`. -/
def enterLowersPeak (a : Account) : Account :=
  { a.enter with functionPeak := 0 }

/-- Instance (law `function_entry_peak`, case `freeAfterEntry`). -/
theorem function_entry_peak_example_freeAfterEntry : ∃ m', run (fun l => (memoryA l).enter) [.free ⟨0⟩ 3] = some m' ∧
    (m' ⟨0⟩).functionPeak = observedPeak ⟨0⟩ (fun l => (memoryA l).enter) [.free ⟨0⟩ 3] :=
  function_entry_peak memoryA [.free ⟨0⟩ 3] ⟨0⟩ memoryA_wf rfl

/-- Planted (law `function_entry_peak`, case `freeAfterEntry`, subject `Account.enter`, mutant `enterLowersPeak`). -/
theorem function_entry_peak_planted_freeAfterEntry_enterLowersPeak : (∀ l, (memoryA l).WF) ∧
    (run (fun l => enterLowersPeak (memoryA l)) [.free ⟨0⟩ 3]).isSome = true ∧
    ¬ (∃ m', run (fun l => enterLowersPeak (memoryA l)) [.free ⟨0⟩ 3] = some m' ∧
      (m' ⟨0⟩).functionPeak =
        observedPeak ⟨0⟩ (fun l => enterLowersPeak (memoryA l)) [.free ⟨0⟩ 3]) := by
  refine ⟨memoryA_wf, rfl, ?_⟩
  rintro ⟨m', hm', hp⟩
  cases hm'
  change (0 : Nat) = 5 at hp
  omega

/-- A mutant allocation additionally charges an unrelated lineage; free and reset retain their rules. -/
def applyCorruptsOther (m : Memory) : Action → Option Memory
  | .alloc lineage bytes =>
      let allocated := Memory.set m lineage ((m lineage).alloc bytes)
      some (Memory.set allocated ⟨1⟩ ((allocated ⟨1⟩).alloc 1))
  | .free lineage bytes =>
      if bytes ≤ (m lineage).live then some (Memory.set m lineage ((m lineage).free bytes)) else none
  | .reset lineage => some (Memory.set m lineage (m lineage).reset)

/-- Instance (law `apply_alloc_zero`, case `memoryA`). -/
theorem apply_alloc_zero_example_memoryA : apply memoryA (.alloc ⟨0⟩ 0) = some memoryA :=
  @apply_alloc_zero memoryA memoryA_wf ⟨0⟩

/-- Planted (law `apply_alloc_zero`, case `memoryA`, subject `apply`, mutant `applyCorruptsOther`). -/
theorem apply_alloc_zero_planted_memoryA_applyCorruptsOther : (∀ l, (memoryA l).WF) ∧ ¬ (applyCorruptsOther memoryA (.alloc ⟨0⟩ 0) = some memoryA) := by
  refine ⟨memoryA_wf, ?_⟩
  intro h
  have h₁ := congrArg (fun result => result.map (fun m => m ⟨1⟩)) h
  change some (accountB.alloc 1) = some accountB at h₁
  exact (by decide : some (accountB.alloc 1) ≠ some accountB) h₁

/-- Instance (law `free_conservation`, case `memoryA`). -/
theorem free_conservation_example_memoryA : ∃ m', apply memoryA (.free ⟨0⟩ 3) = some m' ∧
    (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested ∧
    (m' ⟨0⟩).live + 3 = (memoryA ⟨0⟩).live ∧
    (m' ⟨0⟩).peak = (memoryA ⟨0⟩).peak ∧
    (m' ⟨0⟩).functionPeak = (memoryA ⟨0⟩).functionPeak ∧
    ∀ l, l ≠ ⟨0⟩ → m' l = memoryA l :=
  free_conservation memoryA ⟨0⟩ 3 (by decide)

/-- Mutant transition that clears peak and functionPeak on a reset instead of retaining them. -/
def applyBadReset (m : Memory) : Action → Option Memory
  | .reset lineage => some (Memory.set m lineage { (m lineage).reset with peak := 0 })
  | action => apply m action

/-- Instance (law `reset_conservation`, case `memoryA`). -/
theorem reset_conservation_example_memoryA : ∃ m', apply memoryA (.reset ⟨0⟩) = some m' ∧
    ResetConservation (memoryA ⟨0⟩) (m' ⟨0⟩) (resetOutflow ⟨0⟩ memoryA (.reset ⟨0⟩)) ∧
    ∀ l, l ≠ ⟨0⟩ → m' l = memoryA l :=
  reset_conservation memoryA ⟨0⟩

/-- Planted (law `reset_conservation`, case `memoryA`, subject `apply`, mutant `applyBadReset`). -/
theorem reset_conservation_planted_memoryA_applyBadReset : ¬ (∃ m', applyBadReset memoryA (.reset ⟨0⟩) = some m' ∧
    ResetConservation (memoryA ⟨0⟩) (m' ⟨0⟩) (resetOutflow ⟨0⟩ memoryA (.reset ⟨0⟩)) ∧
    ∀ l, l ≠ ⟨0⟩ → m' l = memoryA l) := by
  rintro ⟨m', hm', hrc, _⟩
  cases hm'
  have hp : (0 : Nat) = 12 := hrc.2.2.2.1
  omega


/-- Instance (law `run_wf`, case `conservationActions`). -/
theorem run_wf_example_conservationActions : ∃ m', run memoryA conservationActions = some m' ∧ ∀ l, (m' l).WF :=
  run_wf memoryA conservationActions memoryA_wf rfl

/-- Instance (law `run_conservation`, case `conservationActions`). -/
theorem run_conservation_example_conservationActions : ∃ m', run memoryA conservationActions = some m' ∧
    (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested + allocated ⟨0⟩ conservationActions ∧
    ((m' ⟨0⟩).live : Int) = (memoryA ⟨0⟩).live + allocated ⟨0⟩ conservationActions -
      freed ⟨0⟩ conservationActions - resetBytes ⟨0⟩ memoryA conservationActions :=
  run_conservation memoryA conservationActions ⟨0⟩ rfl

/-- Instance (law `run_peak_monotone`, case `conservationActions`). -/
theorem run_peak_monotone_example_conservationActions : ∃ m', run memoryA conservationActions = some m' ∧
    (memoryA ⟨0⟩).peak ≤ (m' ⟨0⟩).peak ∧
    (memoryA ⟨0⟩).functionPeak ≤ (m' ⟨0⟩).functionPeak :=
  run_peak_monotone memoryA conservationActions ⟨0⟩ rfl

/-- Instance (law `run_peak_exact`, case `conservationActions`). -/
theorem run_peak_exact_example_conservationActions : ∃ m', run memoryA conservationActions = some m' ∧
    (m' ⟨0⟩).functionPeak = max (memoryA ⟨0⟩).functionPeak
      (observedPeak ⟨0⟩ memoryA conservationActions) :=
  run_peak_exact memoryA conservationActions ⟨0⟩ memoryA_wf rfl

/-- Helper (computation). Evaluates `Account.WF` across all lineages of `reportEntryMemory` for `report_certified` instances and mutants. -/
theorem reportEntry_wf : ∀ l, (reportEntryMemory l).WF := by
  intro l
  by_cases hl : l = ⟨0⟩
  · subst l; simp [reportEntryMemory, Memory.set, reportEntry, Account.WF]
  · simp [reportEntryMemory, Memory.set, hl, Memory.empty, Account.empty, Account.WF]

/-- Helper (computation). Evaluates `Account.WF` across all lineages of `emptyFunctionEntryMemory` for the emptyFunction instance and mutant of `report_certified`. -/
theorem emptyFunctionEntry_wf : ∀ l, (emptyFunctionEntryMemory l).WF := by
  intro l
  by_cases hl : l = ⟨0⟩
  · subst l; simp [emptyFunctionEntryMemory, Memory.set, Account.enter, Account.WF]
  · simp [emptyFunctionEntryMemory, Memory.set, hl, Memory.empty, Account.empty, Account.WF]

/-- Instance (law `report_certified`, case `established`). -/
theorem report_certified_example_established : ∃ m', run reportEntryMemory reportActions = some m' ∧
    LineageReport.Sound ⟨0⟩ (reportEntryMemory ⟨0⟩) (m' ⟨0⟩)
      (demoAllocator.footprint ⟨0⟩ reportEntryMemory reportActions) establishedReport :=
  report_certified reportEntryMemory reportActions demoAllocator ⟨0⟩ establishedReport
    rfl reportEntry_wf rfl rfl
    (by dsimp [Bound.Sound, SignedInterval.Sound, establishedReport]; decide)
    (by dsimp [Bound.Sound, SignedInterval.Sound, establishedReport]; decide)
    (by dsimp [Bound.Sound, SignedInterval.Sound, establishedReport]; decide)
    (by intro _; exact ⟨10, rfl, by decide⟩)

/-- Instance (law `report_certified`, case `mixedStatus`). -/
theorem report_certified_example_mixedStatus : ∃ m', run reportEntryMemory reportActions = some m' ∧
    LineageReport.Sound ⟨0⟩ (reportEntryMemory ⟨0⟩) (m' ⟨0⟩)
      (demoAllocator.footprint ⟨0⟩ reportEntryMemory reportActions) mixedStatusReport :=
  report_certified reportEntryMemory reportActions demoAllocator ⟨0⟩ mixedStatusReport
    rfl reportEntry_wf rfl rfl
    (by dsimp [Bound.Sound, SignedInterval.Sound, mixedStatusReport, establishedReport]; decide)
    (by simp [SignedInterval.Sound, mixedStatusReport])
    (by dsimp [Bound.Sound, SignedInterval.Sound, mixedStatusReport, establishedReport]; decide)
    (by intro _; exact ⟨10, rfl, by decide⟩)

/-- Instance (law `report_certified`, case `emptyFunction`). -/
theorem report_certified_example_emptyFunction : ∃ m', run emptyFunctionEntryMemory [] = some m' ∧
    LineageReport.Sound ⟨0⟩ (emptyFunctionEntryMemory ⟨0⟩) (m' ⟨0⟩)
      (noFootprintAllocator.footprint ⟨0⟩ emptyFunctionEntryMemory []) emptyFunctionReport :=
  report_certified emptyFunctionEntryMemory [] noFootprintAllocator ⟨0⟩ emptyFunctionReport
    rfl emptyFunctionEntry_wf rfl rfl
    (by dsimp [Bound.Sound, SignedInterval.Sound, emptyFunctionReport, establishedReport]; decide)
    (by dsimp [Bound.Sound, SignedInterval.Sound, emptyFunctionReport, establishedReport]; decide)
    (by dsimp [Bound.Sound, SignedInterval.Sound, emptyFunctionReport, establishedReport]; decide)
    (by simp [Bound.SoundOptional, emptyFunctionReport])

/-- Representation lemma. Concrete trajectory calculation showing `observedPeak` records 10 across `reportActions` (including 2 bytes live at entry) and 2 on an empty action list. -/
theorem observed_peak_for_populated_and_empty_runs : observedPeak ⟨0⟩ reportEntryMemory reportActions = 10 ∧
    observedPeak ⟨0⟩ reportEntryMemory [] = 2 := by decide

/-- Planted definition: a free forgets to subtract its bytes; all other fields and rules agree. -/
def freeKeepsLive (m : Memory) : Action → Option Memory
  | .free l n => if n ≤ (m l).live then some m else none
  | a => apply m a

/-- Planted (law `free_conservation`, case `populatedFree`, mutant `freeKeepsLive`).
the populated free has all original premises, but its live-byte equation fails. -/
theorem free_keeps_live : 3 ≤ (memoryA ⟨0⟩).live ∧
    ¬ (∃ m', freeKeepsLive memoryA (.free ⟨0⟩ 3) = some m' ∧
      (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested ∧
      (m' ⟨0⟩).live + 3 = (memoryA ⟨0⟩).live ∧
      (m' ⟨0⟩).peak = (memoryA ⟨0⟩).peak ∧
      (m' ⟨0⟩).functionPeak = (memoryA ⟨0⟩).functionPeak ∧
      ∀ l, l ≠ ⟨0⟩ → m' l = memoryA l) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨m', h, _, bad, _, _, _⟩
  cases h
  change 5 + 3 = (5 : Nat) at bad
  omega

/-- Planted definition: allocation forgets to record requested bytes, changing only that rule. -/
def losesRequest (m : Memory) : Action → Option Memory
  | .alloc l n => some (Memory.set m l { (m l).alloc n with requested := (m l).requested })
  | a => apply m a

/-- Planted definition: allocation overestimates its local peak by one; flow and historical peak agree. -/
def extraPeak (m : Memory) : Action → Option Memory
  | .alloc l n => some (Memory.set m l
      { (m l).alloc n with functionPeak := max (m l).functionPeak ((m l).live + n + 1) })
  | a => apply m a

/-- Planted definition: allocation fails to update the local peak, retaining every other rule. -/
def stalePeak (m : Memory) : Action → Option Memory
  | .alloc l n => some (Memory.set m l
      { (m l).alloc n with functionPeak := (m l).functionPeak })
  | a => apply m a

/-- Planted (law `run_wf`, case `populatedAllocation`, mutant `stalePeak`).
run_wf fails with a stale local peak on a successful allocation from populated memory. -/
theorem run_wf_planted : (∀ l, (memoryA l).WF) ∧
    (Evidence.run memoryA [.alloc ⟨0⟩ 8] stalePeak).isSome = true ∧
    ¬ (∃ m', Evidence.run memoryA [.alloc ⟨0⟩ 8] stalePeak = some m' ∧ ∀ l, (m' l).WF) := by
  refine ⟨memoryA_wf, rfl, ?_⟩
  rintro ⟨m', h, wf⟩
  cases h
  have bad := (wf ⟨0⟩).2
  change 13 ≤ 9 at bad
  omega

/-- Planted (law `run_conservation`, case `populatedSequence`, mutant `losesRequest`).
run_conservation rejects lost requested bytes. Reset outflows use the same mutant
transition at every intermediate state, including the derived observer. -/
theorem conservation_planted :
    (Evidence.run memoryA conservationActions losesRequest).isSome = true ∧
    ¬ (∃ m', Evidence.run memoryA conservationActions losesRequest = some m' ∧
      (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested + allocated ⟨0⟩ conservationActions ∧
      ((m' ⟨0⟩).live : Int) = (memoryA ⟨0⟩).live + allocated ⟨0⟩ conservationActions -
        freed ⟨0⟩ conservationActions - Evidence.resetBytes ⟨0⟩ memoryA conservationActions losesRequest) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨m', h, bad, _⟩
  cases h
  change (100 : Nat) = 108 at bad
  omega

/-- Planted (law `run_peak_monotone`, case `populatedReset`, mutant `applyBadReset`).
run_peak_monotone rejects resetting the historical peak on a populated account. -/
theorem monotone_planted :
    (Evidence.run memoryA conservationActions applyBadReset).isSome = true ∧
    ¬ (∃ m', Evidence.run memoryA conservationActions applyBadReset = some m' ∧
      (memoryA ⟨0⟩).peak ≤ (m' ⟨0⟩).peak ∧
      (memoryA ⟨0⟩).functionPeak ≤ (m' ⟨0⟩).functionPeak) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨m', h, bad, _⟩
  cases h
  change 12 ≤ 0 at bad
  omega

/-- Planted (law `run_peak_exact`, case `oneExtraByte`, mutant `extraPeak`).
run_peak_exact rejects a one-byte overestimate. The trajectory uses the mutant too;
its live fields remain correct, so this is independent of both flow and well-formedness. -/
theorem exact_peak_planted : (∀ l, (memoryA l).WF) ∧
    (Evidence.run memoryA conservationActions extraPeak).isSome = true ∧
    ¬ (∃ m', Evidence.run memoryA conservationActions extraPeak = some m' ∧
      (m' ⟨0⟩).functionPeak = max (memoryA ⟨0⟩).functionPeak
        (Evidence.observedPeak ⟨0⟩ memoryA conservationActions extraPeak)) := by
  refine ⟨memoryA_wf, rfl, ?_⟩
  rintro ⟨m', h, bad⟩
  cases h
  change (14 : Nat) = 13 at bad
  omega

/-- Planted definition: allocation records one extra requested byte, keeping live and peaks correct. -/
def extraRequest (m : Memory) : Action → Option Memory
  | .alloc l n => some (Memory.set m l { (m l).alloc n with requested := (m l).requested + n + 1 })
  | a => apply m a

/-- Planted (law `report_certified`, case `populatedReport`, mutant `extraRequest`).
report_certified rejects an extra request even with correct independent analysis bounds.
Both reset and peak observers use the substituted transition. -/
theorem report_planted : establishedReport.lineage = ⟨0⟩ ∧
    (∀ l, (reportEntryMemory l).WF) ∧
    (reportEntryMemory ⟨0⟩).functionPeak = (reportEntryMemory ⟨0⟩).live ∧
    (Evidence.run reportEntryMemory reportActions extraRequest).isSome = true ∧
    Bound.Sound (allocated ⟨0⟩ reportActions) establishedReport.requested ∧
    SignedInterval.Sound ((allocated ⟨0⟩ reportActions : Int) - freed ⟨0⟩ reportActions -
      Evidence.resetBytes ⟨0⟩ reportEntryMemory reportActions extraRequest) establishedReport.netChange ∧
    Bound.Sound (Evidence.observedPeak ⟨0⟩ reportEntryMemory reportActions extraRequest - (reportEntryMemory ⟨0⟩).live) establishedReport.peak ∧
    Bound.SoundOptional ((demoAllocator extraRequest).footprint ⟨0⟩ reportEntryMemory reportActions) establishedReport.footprint ∧
    ¬ (∃ m', Evidence.run reportEntryMemory reportActions extraRequest = some m' ∧
      LineageReport.Sound ⟨0⟩ (reportEntryMemory ⟨0⟩) (m' ⟨0⟩)
        ((demoAllocator extraRequest).footprint ⟨0⟩ reportEntryMemory reportActions) establishedReport) := by
  refine ⟨rfl, reportEntry_wf, rfl, rfl, by dsimp [Bound.Sound, SignedInterval.Sound, establishedReport]; decide,
    by dsimp [Bound.Sound, SignedInterval.Sound, establishedReport]; decide,
    by dsimp [Bound.Sound, SignedInterval.Sound, establishedReport]; decide,
    (by intro _; exact ⟨10, rfl, by decide⟩), ?_⟩
  rintro ⟨m', h, sound⟩
  cases h
  have bad := sound.2.1 rfl
  change (9 : Nat) ≤ 8 at bad
  omega

/-- Helper: all premises of report_certified, packaged once for isolated report mutants.
No conclusion or report soundness is hidden among these premises. -/
def ReportConditions (m : Memory) (actions : List Action) (allocator : AllocatorContract)
    (lineage : LineageId) (r : LineageReport) : Prop :=
  r.lineage = lineage ∧ (∀ l, (m l).WF) ∧
    (m lineage).functionPeak = (m lineage).live ∧ (run m actions).isSome = true ∧
    Bound.Sound (allocated lineage actions) r.requested ∧
    SignedInterval.Sound ((allocated lineage actions : Int) - freed lineage actions -
      resetBytes lineage m actions) r.netChange ∧
    Bound.Sound (observedPeak lineage m actions - (m lineage).live) r.peak ∧
    Bound.SoundOptional (allocator.footprint lineage m actions) r.footprint

/-- Wrong report judgment: exactly one actual quantity is mismeasured per selector.
0 counts caller-live bytes in peak; 1 reverses signed net; 2 invents footprint bytes.
Every remaining bound and the lineage check are unchanged. -/
def wrongReportSound (fault : Nat) (lineage : LineageId) (entry exit : Account)
    (footprint : Option Nat) (r : LineageReport) : Prop :=
  r.lineage = lineage ∧ Bound.Sound (exit.requested - entry.requested) r.requested ∧
    SignedInterval.Sound (if fault = 1 then (entry.live : Int) - exit.live
      else (exit.live : Int) - entry.live) r.netChange ∧
    Bound.Sound (if fault = 0 then exit.functionPeak else exit.functionPeak - entry.live) r.peak ∧
    Bound.SoundOptional (if fault = 2 then footprint.map (· + 1) else footprint) r.footprint

/-- Helper: entry well-formedness for the common report controls. -/
theorem report_conditions (actions : List Action) (r : LineageReport)
    (h : r.lineage = ⟨0⟩ ∧ (run reportEntryMemory actions).isSome = true ∧
      Bound.Sound (allocated ⟨0⟩ actions) r.requested ∧
      SignedInterval.Sound ((allocated ⟨0⟩ actions : Int) - freed ⟨0⟩ actions -
        resetBytes ⟨0⟩ reportEntryMemory actions) r.netChange ∧
      Bound.Sound (observedPeak ⟨0⟩ reportEntryMemory actions - reportEntry.live) r.peak ∧
      Bound.SoundOptional (some 2) r.footprint) :
    ReportConditions reportEntryMemory actions ⟨fun _ _ _ => some 2⟩ ⟨0⟩ r :=
  ⟨h.1, reportEntry_wf, rfl, h.2⟩

/-- Planted (law `report_certified`, fault `absolutePeak`). Two caller-live bytes and no
allocations satisfy every analysis premise, but absolute peak rejects their zero local peak. -/
theorem report_certified_planted_absolutePeak : let r : LineageReport := { emptyFunctionReport with footprint := ⟨2, .boundEstablished⟩ }
    ReportConditions reportEntryMemory [] ⟨fun _ _ _ => some 2⟩ ⟨0⟩ r ∧
    ¬ (∃ m', run reportEntryMemory [] = some m' ∧
      wrongReportSound 0 ⟨0⟩ reportEntry (m' ⟨0⟩) (some 2) r) := by
  dsimp only
  constructor
  · apply report_conditions
    refine ⟨rfl, rfl, ?_, ?_, ?_, ?_⟩
    · dsimp [Bound.Sound, emptyFunctionReport]; decide
    · dsimp [SignedInterval.Sound, emptyFunctionReport]; decide
    · dsimp [Bound.Sound, emptyFunctionReport]; decide
    · intro _; exact ⟨2, rfl, by decide⟩
  · rintro ⟨m', h, bad⟩
    cases h
    have hp := bad.2.2.2.1 rfl
    change (2 : Nat) ≤ 0 at hp
    omega

/-- Planted (law `report_certified`, fault `reversedNet`). Freeing one entry byte has
net -1; reversing only its sign contradicts the exact signed interval despite all premises. -/
theorem report_certified_planted_reversedNet : let r : LineageReport := { emptyFunctionReport with
      netChange := ⟨-1, -1, .boundEstablished⟩, footprint := ⟨2, .boundEstablished⟩ }
    ReportConditions reportEntryMemory [.free ⟨0⟩ 1] ⟨fun _ _ _ => some 2⟩ ⟨0⟩ r ∧
    ¬ (∃ m', run reportEntryMemory [.free ⟨0⟩ 1] = some m' ∧
      wrongReportSound 1 ⟨0⟩ reportEntry (m' ⟨0⟩) (some 2) r) := by
  dsimp only
  constructor
  · apply report_conditions
    refine ⟨rfl, rfl, ?_, ?_, ?_, ?_⟩
    · dsimp [Bound.Sound, emptyFunctionReport]; decide
    · dsimp [SignedInterval.Sound, emptyFunctionReport]; decide
    · dsimp [Bound.Sound, emptyFunctionReport]; decide
    · intro _; exact ⟨2, rfl, by decide⟩
  · rintro ⟨m', h, bad⟩
    cases h
    have hn := (bad.2.2.1 rfl).2
    change (1 : Int) ≤ -1 at hn
    omega

/-- Planted (law `report_certified`, fault `inventedFootprint`). The allocator contract
reports exactly two bytes; adding only one footprint byte breaks certification. -/
theorem report_certified_planted_inventedFootprint : let r : LineageReport := { emptyFunctionReport with footprint := ⟨2, .boundEstablished⟩ }
    ReportConditions reportEntryMemory [] ⟨fun _ _ _ => some 2⟩ ⟨0⟩ r ∧
    ¬ (∃ m', run reportEntryMemory [] = some m' ∧
      wrongReportSound 2 ⟨0⟩ reportEntry (m' ⟨0⟩) (some 2) r) := by
  dsimp only
  constructor
  · apply report_conditions
    refine ⟨rfl, rfl, ?_, ?_, ?_, ?_⟩
    · dsimp [Bound.Sound, emptyFunctionReport]; decide
    · dsimp [SignedInterval.Sound, emptyFunctionReport]; decide
    · dsimp [Bound.Sound, emptyFunctionReport]; decide
    · intro _; exact ⟨2, rfl, by decide⟩
  · rintro ⟨m', h, bad⟩
    cases h
    obtain ⟨n, hn, hb⟩ := bad.2.2.2.2 rfl
    cases hn
    change (3 : Nat) ≤ 2 at hb
    omega

/-- Instance (law `run_historical_peak_exact`, case `populatedHistory`). -/
theorem run_historical_peak_exact_example_populatedHistory : ∃ m', run memoryA conservationActions = some m' ∧
    (m' ⟨0⟩).peak = max (memoryA ⟨0⟩).peak
      (observedPeak ⟨0⟩ memoryA conservationActions) :=
  run_historical_peak_exact memoryA conservationActions ⟨0⟩ memoryA_wf rfl

/-- Planted definition. Add one byte only to the historical peak. -/
def extraHistoricalPeak (m : Memory) : Action → Option Memory
  | .alloc l n => some (Memory.set m l
      { (m l).alloc n with peak := max (m l).peak ((m l).live + n + 1) })
  | a => apply m a

/-- Planted (law `run_historical_peak_exact`, fault `oneExtraHistoricalByte`).
The successful run and its independent observer both use the substituted transition. -/
theorem historical_exact_planted : (∀ l, (memoryA l).WF) ∧
    (Evidence.run memoryA conservationActions extraHistoricalPeak).isSome = true ∧
    ¬ (∃ m', Evidence.run memoryA conservationActions extraHistoricalPeak = some m' ∧
      (m' ⟨0⟩).peak = max (memoryA ⟨0⟩).peak
        (Evidence.observedPeak ⟨0⟩ memoryA conservationActions extraHistoricalPeak)) := by
  refine ⟨memoryA_wf, rfl, ?_⟩
  rintro ⟨m', h, bad⟩
  cases h
  change (14 : Nat) = 13 at bad
  omega

/-- Instance (law `free_permitted_iff`, case `ownedAndExcessive`). -/
theorem free_permitted_iff_example_ownedAndExcessive : ActionPermitted memoryA (.free ⟨0⟩ 5) ∧
    ¬ ActionPermitted memoryA (.free ⟨0⟩ 6) := by
  constructor <;> rw [free_permitted_iff] <;> decide

/-- Planted definition. Excessive frees are admitted, retaining every account update. -/
def admitsOverfree (m : Memory) : Action → Option Memory
  | .free l n => some (Memory.set m l ((m l).free n))
  | a => apply m a

/-- Planted (law `free_permitted_iff`, fault `admitsExcessiveOutflow`). -/
theorem free_permitted_iff_planted_admitsExcessiveOutflow : ¬ ((admitsOverfree memoryA (.free ⟨0⟩ 6)).isSome = true ↔
    6 ≤ (memoryA ⟨0⟩).live) := by decide

/-- Planted definition. A valid positive free is refused as though no bytes were owned. -/
def refusesOwnedFree (m : Memory) : Action → Option Memory
  | .free _ _ => none
  | a => apply m a

/-- Planted (law `free_permitted_iff`, fault `refusesOwnedOutflow`). -/
theorem free_permitted_iff_planted_refusesOwnedOutflow : ¬ ((refusesOwnedFree memoryA (.free ⟨0⟩ 5)).isSome = true ↔
    5 ≤ (memoryA ⟨0⟩).live) := by decide

/-- Instance (law `free_permitted_iff`, case `doubleFree`). The second free is refused. -/
theorem free_permitted_iff_example_doubleFree : run Memory.empty [.alloc ⟨0⟩ 5, .free ⟨0⟩ 5, .free ⟨0⟩ 5] = none := by decide

/-- Planted (law `run_conservation`, fault `doubleFreeAdmitted`).
All execution and observer occurrences use the same guard-dropping mutant. -/
theorem run_conservation_planted_doubleFreeAdmitted : (Evidence.run Memory.empty [.alloc ⟨0⟩ 5, .free ⟨0⟩ 5, .free ⟨0⟩ 5]
    admitsOverfree).isSome = true ∧
    ¬ (∃ m', Evidence.run Memory.empty [.alloc ⟨0⟩ 5, .free ⟨0⟩ 5, .free ⟨0⟩ 5]
      admitsOverfree = some m' ∧
      ((m' ⟨0⟩).live : Int) = (Memory.empty ⟨0⟩).live +
        allocated ⟨0⟩ [.alloc ⟨0⟩ 5, .free ⟨0⟩ 5, .free ⟨0⟩ 5] -
        freed ⟨0⟩ [.alloc ⟨0⟩ 5, .free ⟨0⟩ 5, .free ⟨0⟩ 5] -
        Evidence.resetBytes ⟨0⟩ Memory.empty [.alloc ⟨0⟩ 5, .free ⟨0⟩ 5, .free ⟨0⟩ 5]
          admitsOverfree) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨m', h, bad⟩
  cases h
  change (0 : Int) = -5 at bad
  omega
/-- Planted definition. Explicit frees are over-counted by one byte. -/
def extraFreed (l : LineageId) (actions : List Action) : Nat := freed l actions + 1

/-- Planted (law `run_conservation`, fault `extraFreedByte`). -/
theorem run_conservation_planted_extraFreedByte : (run memoryA conservationActions).isSome = true ∧
    ¬ (∃ m', run memoryA conservationActions = some m' ∧
      (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested + allocated ⟨0⟩ conservationActions ∧
      ((m' ⟨0⟩).live : Int) = (memoryA ⟨0⟩).live + allocated ⟨0⟩ conservationActions -
        extraFreed ⟨0⟩ conservationActions - resetBytes ⟨0⟩ memoryA conservationActions) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨m', h, _, bad⟩
  cases h
  change (0 : Int) = -1 at bad
  omega

/-- Planted definition. Reset outflow is over-counted by one byte. -/
def extraResetBytes (l : LineageId) (m : Memory) (actions : List Action) : Nat :=
  resetBytes l m actions + 1

/-- Planted (law `run_conservation`, fault `extraResetByte`). -/
theorem run_conservation_planted_extraResetByte : (run memoryA conservationActions).isSome = true ∧
    ¬ (∃ m', run memoryA conservationActions = some m' ∧
      (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested + allocated ⟨0⟩ conservationActions ∧
      ((m' ⟨0⟩).live : Int) = (memoryA ⟨0⟩).live + allocated ⟨0⟩ conservationActions -
        freed ⟨0⟩ conservationActions - extraResetBytes ⟨0⟩ memoryA conservationActions) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨m', h, _, bad⟩
  cases h
  change (0 : Int) = -1 at bad
  omega

/-- Instance. A no-allocation call reports zero peak with two caller-live bytes. -/
theorem no_allocation_reports_zero_peak : LineageReport.Sound ⟨0⟩ reportEntry reportEntry none
    ⟨⟨0⟩, ⟨0, .boundEstablished⟩, ⟨0, 0, .boundEstablished⟩,
      ⟨0, .boundEstablished⟩, ⟨0, .notEstablished⟩⟩ := by
  simp [LineageReport.Sound, reportEntry, Bound.Sound, SignedInterval.Sound, Bound.SoundOptional]

/-- Instance. A signed net interval permits released entry storage to give a negative change. -/
theorem net_interval_permits_entry_release : SignedInterval.Sound (-3) ⟨-4, -2, .boundEstablished⟩ := by
  simp [SignedInterval.Sound]

/-- Planted definition. Dropping the lower endpoint falsely accepts unbounded net release. -/
def upperOnlyNet (actual : Int) (b : SignedInterval) : Prop :=
  b.status = .boundEstablished → actual ≤ b.hi

/-- Planted. Upper-only checking cannot establish the signed interval contract. -/
theorem upper_only_check_accepts_excessive_release : upperOnlyNet (-5) ⟨-4, -2, .boundEstablished⟩ ∧
    ¬ SignedInterval.Sound (-5) ⟨-4, -2, .boundEstablished⟩ := by
  simp [upperOnlyNet, SignedInterval.Sound]


end Audit

end ConcreteSpec.Core.Leak
