import ConcreteSpec.Spec.Exec

/-!
# The laws of execution, calls and cleanup (`Spec/Exec.lean`, foundation contract C1)

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.

What these prove, for every operand language:

- **Sequencing**: `seq s t` runs `s` with `t` as its continuation; `t` runs when `s` completes, and
  an exit from `s` skips it (`step_seq`, `step_done_seq`, `step_exit_seq`).
- **Scope restoration**: leaving a scope keeps exactly the locals visible when it was entered,
  with their current values (`lookup_restrict`).
- **Cleanup order**: an exit leaving a scope runs its deferred calls newest first, then restores
  the scope (`exit_scope_runs_defers`); the normal end of a block does the same
  (`done_scope_runs_defers`); an exit passing several frames runs the calls of `plan`, innermost
  scope first (`unwind_runs_plan`); `register` adds a call as the newest of the innermost scope.
- **Exit values**: a `return` evaluates its value before any cleanup (`step_ret`), and every
  unwinding step carries the exit unchanged (`unwind_runs_plan` ends in the same exit).
- **Traps are terminal**: a trapped configuration has no step and no later event
  (`steps_from_trapped`); `abort()` runs no cleanup (`abort_runs_no_cleanup`), and so neither does a
  cleanup call that traps.
- **Evaluation order**: the first argument that traps decides the trap (`evalArgs_first_trap`).
- Silent steps are deterministic (`step_silent_deterministic`).
-/

namespace ConcreteSpec.Core.Exec
open ConcreteSpec.Core

variable {O : Operands} {V : Type}

/-! ### Many steps -/

theorem Steps.single {P : Program O} {c c' : Config O} {l : Label O} (h : Step P c l c') :
    Steps P c l.events c' := by
  have := Steps.step h (Steps.refl c')
  simpa using this

theorem Steps.trans {P : Program O} {c c' c'' : Config O} {tr tr' : List O.Event}
    (h : Steps P c tr c') (h' : Steps P c' tr' c'') : Steps P c (tr ++ tr') c'' := by
  induction h with
  | refl => simpa using h'
  | step hs _ ih => simpa [List.append_assoc] using Steps.step hs (ih h')

/-- A silent step followed by more steps. -/
theorem steps_next {P : Program O} {c c' c'' : Config O} {tr : List O.Event}
    (h : step P c = some (.next c')) (h' : Steps P c' tr c'') : Steps P c tr c'' := by
  simpa [Label.events] using Steps.step (Step.silent h) h'

/-- A primitive operation answered within its contract, followed by more steps. -/
theorem steps_request {P : Program O} {c c'' : Config O} {p : O.Prim} {vs : List O.Val}
    {site : SiteId} {k : Except TrapKind O.Val → Config O} {ev : O.Event}
    {r : Except TrapKind O.Val} {tr : List O.Event}
    (h : step P c = some (.request p vs site k)) (ha : O.allows p vs ev r = true)
    (h' : Steps P (k r) tr c'') : Steps P c (ev :: tr) c'' := by
  simpa [Label.events] using Steps.step (Step.emit h ha) h'

/-! ### Sequencing -/

theorem step_seq (P : Program O) (s t : Stmt O) (L : AList Var O.Val) (k : List (Frame O)) :
    step P ⟨.exec (.seq s t), L, k⟩ = some (.next ⟨.exec s, L, .seq t :: k⟩) := rfl

theorem step_done_seq (P : Program O) (t : Stmt O) (L : AList Var O.Val) (k : List (Frame O)) :
    step P ⟨.done, L, .seq t :: k⟩ = some (.next ⟨.exec t, L, k⟩) := rfl

theorem step_exit_seq (P : Program O) (e : Exit O.Val) (t : Stmt O) (L : AList Var O.Val)
    (k : List (Frame O)) : step P ⟨.exiting e, L, .seq t :: k⟩ = some (.next ⟨.exiting e, L, k⟩) :=
  rfl

/-- Entering a block records the locals visible at its start. -/
theorem step_block (P : Program O) (b : BlockId) (s : Stmt O) (L : AList Var O.Val)
    (k : List (Frame O)) :
    step P ⟨.exec (.block b s), L, k⟩ = some (.next ⟨.exec s, L, .scope b (dom L) [] :: k⟩) := rfl

/-! ### Scope restoration -/

theorem lookup_restrict (dm : List Var) : ∀ (L : AList Var V) (x : Var),
    AList.lookup (restrict dm L) x = if x ∈ dm then AList.lookup L x else none
  | [], x => by by_cases h : x ∈ dm <;> simp [restrict, AList.lookup, h]
  | (y, v) :: L, x => by
    have ih := lookup_restrict dm L x
    by_cases hy : y ∈ dm
    · have hr : restrict dm ((y, v) :: L) = (y, v) :: restrict dm L := by
        simp [restrict, hy]
      rw [hr]
      by_cases hyx : y = x
      · subst hyx; simp [AList.lookup, hy]
      · simp only [AList.lookup, ite_eq_right hyx]; exact ih
    · have hr : restrict dm ((y, v) :: L) = restrict dm L := by
        simp [restrict, hy]
      rw [hr, ih]
      by_cases hyx : y = x
      · subst hyx; simp [hy]
      · simp [AList.lookup, hyx]

/-- Leaving a scope entered with locals `L₀` keeps no local bound inside it. -/
theorem restrict_dom_drops (L₀ L : AList Var V) (x : Var) (hx : x ∉ dom L₀) :
    AList.lookup (restrict (dom L₀) L) x = none := by
  rw [lookup_restrict]; simp [hx]

/-! ### Exits and cleanup -/

/-- `return` evaluates its value first: the next configuration carries the value and the same
frames, so no cleanup has run. -/
theorem step_ret (P : Program O) (e : O.Expr) (site : SiteId) (L : AList Var O.Val)
    (k : List (Frame O)) (v : O.Val) (hv : O.eval L e = .ok v) :
    step P ⟨.exec (.ret e site), L, k⟩ = some (.next ⟨.exiting (.ret v), L, k⟩) := by
  simp [step, onRes, hv]

/-- A `break` reaching its loop's body frame continues after the loop. -/
theorem step_break_loop (P : Program O) (l : LoopId) (cr b : Stmt O) (t : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (k : List (Frame O)) :
    step P ⟨.exiting (.brk l), L, .body l cr t b site :: k⟩ = some (.next ⟨.done, L, k⟩) := by
  simp [step, stepExit]

/-- A `continue` reaching its loop's body frame starts the loop again from its condition region. -/
theorem step_continue_loop (P : Program O) (l : LoopId) (cr b : Stmt O) (t : O.Expr)
    (site : SiteId) (L : AList Var O.Val) (k : List (Frame O)) :
    step P ⟨.exiting (.cont l), L, .body l cr t b site :: k⟩ =
      some (.next ⟨.exec (.loop l cr t b site), L, k⟩) := by
  simp [step, stepExit]

/-- A `return` reaching the call frame restores the caller's locals and binds the value. -/
theorem step_return_caller (P : Program O) (v : O.Val) (dst : Option Var)
    (caller L : AList Var O.Val) (k : List (Frame O)) :
    step P ⟨.exiting (.ret v), L, .ret dst caller :: k⟩ =
      some (.next ⟨.done, bindDst dst v caller, k⟩) := rfl

/-- A deferred call is registered as the newest call of the innermost scope. -/
theorem register_scope (d : Deferred O) (b : BlockId) (dm : List Var) (ds : List (Deferred O))
    (k : List (Frame O)) : register d (.scope b dm ds :: k) = some (.scope b dm (d :: ds) :: k) :=
  rfl

theorem register_seq (d : Deferred O) (t : Stmt O) (k : List (Frame O)) :
    register d (.seq t :: k) = (register d k).map (.seq t :: ·) := rfl

/-- The plan of nested scopes: the innermost scope's calls come first. -/
theorem plan_scopes (b₁ b₂ : BlockId) (d₁ d₂ : List Var) (ds₁ ds₂ : List (Deferred O))
    (k : List (Frame O)) :
    plan (.scope b₁ d₁ ds₁ :: .scope b₂ d₂ ds₂ :: k) = ds₁ ++ ds₂ ++ plan k := by
  simp [plan]

/-- An exit leaving a scope runs the scope's deferred calls, newest first (each with the events it
emits), and leaves the scope with its locals restored; the exit, and a returned value, are
unchanged. -/
theorem exit_scope_runs_defers (P : Program O) (tr : Deferred O → List O.Event) (e : Exit O.Val)
    (b : BlockId) (dm : List Var) (L : AList Var O.Val) (k : List (Frame O)) :
    ∀ ds : List (Deferred O), (∀ d ∈ ds, Completes P L d (tr d)) →
      Steps P ⟨.exiting e, L, .scope b dm ds :: k⟩ (ds.flatMap tr) ⟨.exiting e, restrict dm L, k⟩
  | [], _ => by simpa using steps_next rfl (Steps.refl (P := P) ⟨.exiting e, restrict dm L, k⟩)
  | d :: ds, hc => by
    have hd := hc d (List.mem_cons_self ..) (.resume (.exit e) :: .scope b dm ds :: k)
    have ih := exit_scope_runs_defers P tr e b dm L k ds
      (fun d' hd' => hc d' (List.mem_cons_of_mem _ hd'))
    have := steps_next (c := ⟨.exiting e, L, .scope b dm (d :: ds) :: k⟩) rfl
      (Steps.trans hd (steps_next rfl ih))
    simpa [List.flatMap_cons] using this

/-- The normal end of a block runs its deferred calls the same way. -/
theorem done_scope_runs_defers (P : Program O) (tr : Deferred O → List O.Event) (b : BlockId)
    (dm : List Var) (L : AList Var O.Val) (k : List (Frame O)) :
    ∀ ds : List (Deferred O), (∀ d ∈ ds, Completes P L d (tr d)) →
      Steps P ⟨.done, L, .scope b dm ds :: k⟩ (ds.flatMap tr) ⟨.done, restrict dm L, k⟩
  | [], _ => by simpa using steps_next rfl (Steps.refl (P := P) ⟨.done, restrict dm L, k⟩)
  | d :: ds, hc => by
    have hd := hc d (List.mem_cons_self ..) (.resume .fall :: .scope b dm ds :: k)
    have ih := done_scope_runs_defers P tr b dm L k ds
      (fun d' hd' => hc d' (List.mem_cons_of_mem _ hd'))
    have := steps_next (c := ⟨.done, L, .scope b dm (d :: ds) :: k⟩) rfl
      (Steps.trans hd (steps_next rfl ih))
    simpa [List.flatMap_cons] using this

/-- **Cleanup order.** An exit passing frames `pre` runs exactly the deferred calls of
`plan pre`, in that order (the innermost scope first, the newest registration of each scope
first), each emitting its events, and arrives at the frames below with the same exit and the
locals each scope restored. -/
theorem unwind_runs_plan (P : Program O) (tr : Deferred O → List O.Event) (e : Exit O.Val)
    (k : List (Frame O)) :
    ∀ (pre : List (Frame O)) (L : AList Var O.Val), (∀ f ∈ pre, Passes e f) → Cleans P tr pre L →
      Steps P ⟨.exiting e, L, pre ++ k⟩ ((plan pre).flatMap tr) ⟨.exiting e, unwindLocals pre L, k⟩
  | [], L, _, _ => by simpa [plan, unwindLocals] using Steps.refl (P := P) ⟨.exiting e, L, k⟩
  | f :: pre, L, hp, hc => by
    have hrest : ∀ f' ∈ pre, Passes e f' := fun f' h => hp f' (List.mem_cons_of_mem _ h)
    have hf : Passes e f := hp f (List.mem_cons_self ..)
    cases f with
    | seq t =>
      exact steps_next rfl (unwind_runs_plan P tr e k pre L hrest hc)
    | scope b dm ds =>
      have h1 := exit_scope_runs_defers P tr e b dm L (pre ++ k) ds hc.1
      have h2 := unwind_runs_plan P tr e k pre (restrict dm L) hrest hc.2
      simpa [plan, unwindLocals, List.flatMap_append] using Steps.trans h1 h2
    | test l cr t b site =>
      exact steps_next rfl (unwind_runs_plan P tr e k pre L hrest hc)
    | body l cr t b site =>
      have ih := unwind_runs_plan P tr e k pre L hrest hc
      cases e with
      | brk l' =>
        have hne : l' ≠ l := hf
        exact steps_next (by simp [step, stepExit, hne]) ih
      | cont l' =>
        have hne : l' ≠ l := hf
        exact steps_next (by simp [step, stepExit, hne]) ih
      | ret v => exact steps_next rfl ih
    | resume _ => exact absurd hf id
    | ret _ _ => exact absurd hf id

/-! ### Traps are terminal -/

theorem step_trapped (P : Program O) (kd : TrapKind) (site : SiteId) (L : AList Var O.Val)
    (k : List (Frame O)) : step P ⟨.trapped kd site, L, k⟩ = none := rfl

/-- After a trap nothing runs: no step, no event, and the frames are not unwound. -/
theorem steps_from_trapped {P : Program O} {kd : TrapKind} {site : SiteId} {L : AList Var O.Val}
    {k : List (Frame O)} {tr : List O.Event} {c : Config O}
    (h : Steps P ⟨.trapped kd site, L, k⟩ tr c) : tr = [] ∧ c = ⟨.trapped kd site, L, k⟩ := by
  cases h with
  | refl => exact ⟨rfl, rfl⟩
  | step hs _ =>
    cases hs with
    | silent h => simp [step_trapped] at h
    | emit h _ => simp [step_trapped] at h

/-- **`abort()` runs no cleanup.** From `abort()`, the only step is to the trap, with the same
frames, and nothing follows: no deferred call runs and no event is emitted. -/
theorem abort_runs_no_cleanup {P : Program O} {site : SiteId} {L : AList Var O.Val}
    {k : List (Frame O)} {tr : List O.Event} {c : Config O}
    (h : Steps P ⟨.exec (.abort site), L, k⟩ tr c) :
    tr = [] ∧ (c = ⟨.exec (.abort site), L, k⟩ ∨ c = ⟨.trapped .abort site, L, k⟩) := by
  cases h with
  | refl => exact ⟨rfl, .inl rfl⟩
  | step hs hrest =>
    cases hs with
    | silent h =>
      simp only [step, Option.some.injEq, Out.next.injEq] at h
      subst h
      have := steps_from_trapped hrest
      exact ⟨by simpa [Label.events] using this.1, .inr this.2⟩
    | emit h _ => simp [step] at h

/-! ### Evaluation order and determinism -/

/-- The first argument that traps decides the trap, whatever follows it. -/
theorem evalArgs_first_trap (L : AList Var O.Val) (e : O.Expr) (kd : TrapKind)
    (he : O.eval L e = .trap kd) (es₂ : List O.Expr) :
    ∀ (es₁ : List O.Expr) (vs : List O.Val), evalArgs O L es₁ = .ok vs →
      evalArgs O L (es₁ ++ e :: es₂) = .trap kd
  | [], _, _ => by simp [evalArgs, he, Res.bind]
  | e₁ :: es₁, vs, h => by
    show evalArgs O L (e₁ :: (es₁ ++ e :: es₂)) = _
    cases h₁ : O.eval L e₁ with
    | ok v₁ =>
      cases h₂ : evalArgs O L es₁ with
      | ok vs₁ =>
        have := evalArgs_first_trap L e kd he es₂ es₁ vs₁ h₂
        simp [evalArgs, h₁, this, Res.bind, Res.map]
      | trap _ => simp [evalArgs, h₁, h₂, Res.bind, Res.map] at h
      | stuck => simp [evalArgs, h₁, h₂, Res.bind, Res.map] at h
    | trap _ => simp [evalArgs, h₁, Res.bind] at h
    | stuck => simp [evalArgs, h₁, Res.bind] at h

/-- Silent steps are deterministic: only the environment's answers can make two runs differ. -/
theorem step_silent_deterministic {P : Program O} {c c₁ c₂ : Config O}
    (h₁ : Step P c .silent c₁) (h₂ : Step P c .silent c₂) : c₁ = c₂ := by
  cases h₁ with
  | silent h₁ =>
    cases h₂ with
    | silent h₂ => rw [h₁] at h₂; cases h₂; rfl

/-! ### Destruction -/

/-- The cleanup call that destroys a value calls the type's one `Destroy` function, which takes the
value as its only, ordinary parameter and returns `Unit`. -/
theorem destroyCall_sig {P : Program O} (D : DestroyProtocol P) (T : O.Ty) (arg : O.Expr)
    (site : SiteId) (d : Deferred O) (h : D.destroyCall T arg site = some d) :
    d.args = [arg] ∧ ∃ fn x, AList.lookup P.fns d.f = some fn ∧ fn.params = [(x, T)] ∧
      fn.result = O.unitTy := by
  unfold DestroyProtocol.destroyCall at h
  cases hi : D.impl T with
  | none => rw [hi] at h; cases h
  | some f =>
    rw [hi] at h; cases h
    exact ⟨rfl, D.sig T f hi⟩

/-- A type with a `Destroy` function is not `Copy`. -/
theorem destroy_not_copy {P : Program O} (D : DestroyProtocol P) (T : O.Ty) (f : FnId)
    (h : D.impl T = some f) : O.isCopy T = false := D.notCopy T f h

/-! ### Non-vacuity

The cleanup laws assume that each deferred call completes (`Completes`). Real calls meet it: in the
demo language of `Spec/Exec.lean`, a deferred `log(1)` completes from any locals and on any frames,
emitting `1`. -/

theorem demo_log_completes (L : AList Var Demo.ops.Val) :
    Completes Demo.prog L ⟨Demo.logF, [.lit 1], Demo.s⟩ [1] := by
  intro k
  refine steps_next rfl (steps_request (O := Demo.ops) (P := Demo.prog) (ev := (1 : Int))
    (r := .ok (0 : Int)) (tr := []) rfl rfl ?_)
  exact steps_next rfl (steps_next rfl (Steps.refl _))

end ConcreteSpec.Core.Exec
