import ConcreteSpec.Spec.Exec

/-!
# The laws of execution, calls and cleanup (`Spec/Exec.lean`, foundation contract C1)

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.

What these prove, for every operand language:

- **Sequencing**: `seq s t` runs `s` with `t` as its continuation; `t` runs when `s` completes, and
  an exit from `s` skips it (`step_seq`, `step_done_seq`, `step_exit_seq`).
- **Scope restoration**: leaving a scope keeps exactly the locals visible when it was entered,
  with their current values (`lookup_restrict`); the shared state is not restored: a call's
  return keeps the state the callee left (`step_return_caller`).
- **Registration**: `defer` adds a call as the newest of the innermost scope (`register_scope`),
  so `defer d₁; defer d₂` leaves `d₂` before `d₁` (`two_defers_register`).
- **Cleanup order**, in two forms. *Every* run: the only step an exit or the end of a block can
  take at a scope with registered calls starts its newest one (`exit_scope_next`,
  `done_scope_next`). *A run whose calls complete*: an exit through a scope runs its calls newest
  first, threading the state, and then restores the scope (`exit_scope_runs_defers`,
  `done_scope_runs_defers`); an exit through several frames runs the calls of `plan`, innermost
  scope first, and arrives unchanged (`unwind_runs_plan`); the cleanup runs exactly the calls of
  `plan`, in order. Actual first-boundary executions dispatch exactly `planWith` and perform its
  state updates (`raw_cleanup_runs_plan`, `cleanup_runs_exact_plan`); the former derives
  segmentation from arbitrary raw first-arrival executions. `cleans_iff_runs_plan_inv` only relates abstract
  certificates. A `return` then reaches its caller with
  the value bound (`return_reaches_caller`), and a `break` or `continue` its loop
  (`break_reaches_loop`, `continue_reaches_loop`).
- **Exit values**: a `return` evaluates its value before any cleanup (`step_ret`).
- **Traps are terminal**: a trapped configuration has no step and no later event
  (`steps_from_trapped`); `abort()` runs no cleanup (`abort_runs_no_cleanup`), and so neither does a
  cleanup call that traps.
- **Evaluation order**: the first argument that traps decides the trap (`evalArgs_first_trap`).
- Function-backed determinism is classified as representation inversion
  (`step_silent_deterministic_inv`, `step_deterministic_inv`), not an independent rule law.
- **Destruction order** (Q2.3): a sequence's element `i` is visited `i`-th and the backing last; a
  map's `i`-th entry has its key at `2i` and its value at `2i+1`, and the nodes come last.
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
    {site : SiteId} {st : O.Store} {k : Answer O.Val O.Event O.Store → Option (Config O)}
    {a : Answer O.Val O.Event O.Store} {tr : List O.Event}
    (h : step P c = some (.request p vs site st k)) (ha : O.allows p vs st a = true)
    {c' : Config O} (hk : k a = some c') (h' : Steps P c' tr c'') : Steps P c (a.label.events ++ tr) c'' :=
  Steps.step (Step.prim h ha hk) h'

/-! ### Sequencing -/

theorem step_seq (P : Program O) (s t : Stmt O) (L : AList Var O.Val) (st : O.Store)
    (k : List (Frame O)) :
    step P ⟨.exec (.seq s t), L, st, k⟩ = some (.next ⟨.exec s, L, st, .seq t :: k⟩) := rfl

theorem step_done_seq (P : Program O) (t : Stmt O) (L : AList Var O.Val) (st : O.Store)
    (k : List (Frame O)) :
    step P ⟨.done, L, st, .seq t :: k⟩ = some (.next ⟨.exec t, L, st, k⟩) := rfl

theorem step_exit_seq (P : Program O) (e : Exit O.Val) (t : Stmt O) (L : AList Var O.Val)
    (st : O.Store) (k : List (Frame O)) :
    step P ⟨.exiting e, L, st, .seq t :: k⟩ = some (.next ⟨.exiting e, L, st, k⟩) := rfl

/-- Entering a block records the locals visible at its start. -/
theorem step_block (P : Program O) (b : BlockId) (s : Stmt O) (L : AList Var O.Val)
    (st : O.Store) (k : List (Frame O)) :
    step P ⟨.exec (.block b s), L, st, k⟩ =
      some (.next ⟨.exec s, L, st, .scope b (dom L) [] :: k⟩) := rfl

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

/-! ### Registration -/

/-- A deferred call is registered as the newest call of the innermost scope. -/
theorem register_scope (d : Deferred O) (b : BlockId) (dm : List Var) (ds : List (Deferred O))
    (k : List (Frame O)) : register d (.scope b dm ds :: k) = some (.scope b dm (d :: ds) :: k) :=
  rfl

theorem register_seq (d : Deferred O) (t : Stmt O) (k : List (Frame O)) :
    register d (.seq t :: k) = (register d k).map (.seq t :: ·) := rfl

/-- **Reverse registration order.** A block running `defer d₁; defer d₂; s` reaches `s` with its
scope holding `d₂` before `d₁`, so `d₂` runs first; nothing is emitted. -/
theorem two_defers_register (P : Program O) (b : BlockId) (d₁ d₂ : Deferred O) (s : Stmt O)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    Steps P ⟨.exec (.block b (.seq (.defer d₁.f d₁.args d₁.site)
        (.seq (.defer d₂.f d₂.args d₂.site) s))), L, st, k⟩ []
      ⟨.exec s, L, st, .scope b (dom L) [d₂, d₁] :: k⟩ :=
  steps_next rfl (steps_next rfl (steps_next rfl (steps_next rfl (steps_next rfl
    (steps_next rfl (steps_next rfl (Steps.refl _)))))))

/-! ### Exits -/

/-- `return` evaluates its value first: the next configuration carries the value and the same
frames, so no cleanup has run. -/
theorem step_ret (P : Program O) (e : O.Expr) (site : SiteId) (L : AList Var O.Val)
    (st st' : O.Store) (k : List (Frame O)) (v : O.Val) (hv : O.eval L st e = .ok (v, st')) :
    step P ⟨.exec (.ret e site), L, st, k⟩ = some (.next ⟨.exiting (.ret v), L, st', k⟩) := by
  simp [step, onRes, hv]

/-- A `break` reaching its loop's body frame continues after the loop. -/
theorem step_break_loop (P : Program O) (l : LoopId) (cr b : Stmt O) (t : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    step P ⟨.exiting (.brk l), L, st, .body l cr t b site :: k⟩ =
      some (.next ⟨.done, L, st, k⟩) := by
  simp [step, stepExit]

/-- A `continue` reaching its loop's body frame starts the loop again from its condition region. -/
theorem step_continue_loop (P : Program O) (l : LoopId) (cr b : Stmt O) (t : O.Expr)
    (site : SiteId) (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    step P ⟨.exiting (.cont l), L, st, .body l cr t b site :: k⟩ =
      some (.next ⟨.exec (.loop l cr t b site), L, st, k⟩) := by
  simp [step, stepExit]

/-- A `return` reaching the call frame restores the caller's locals and binds the value; the state
is the one the callee left, with the instrumentation told the activation ended and the result
bound. -/
theorem step_return_caller (P : Program O) (v : O.Val) (dst : Option Var)
    (caller L : AList Var O.Val) (st : O.Store) (k : List (Frame O))
    (stLeave stBind : O.Store) (hl : O.life .leave st = some stLeave)
    (hb : bindLife O dst stLeave = some stBind) :
    step P ⟨.exiting (.ret v), L, st, .ret dst caller :: k⟩ =
      some (.next ⟨.done, bindDst dst v caller, stBind, k⟩) := by
  simp [step, stepExit, hl, hb]

/-! ### Cleanup -/

/-- **Every run.** At a scope with registered calls, the only step an exit can take starts the
newest call, remembering the exit. -/
theorem exit_scope_next {P : Program O} {e : Exit O.Val} {b : BlockId} {dm : List Var}
    {d : Deferred O} {ds : List (Deferred O)} {L : AList Var O.Val} {st : O.Store}
    {k : List (Frame O)} {l : Label O} {c : Config O}
    (h : Step P ⟨.exiting e, L, st, .scope b dm (d :: ds) :: k⟩ l c) :
    l = .silent ∧ c = ⟨.exec d.stmt, L, st, .resume (.exit e) :: .scope b dm ds :: k⟩ := by
  cases h with
  | silent h =>
    simp only [step, stepExit, startCleanup, Option.some.injEq, Out.next.injEq] at h
    exact ⟨rfl, h.symm⟩
  | prim h _ _ => simp [step, stepExit, startCleanup] at h

/-- The same at the normal end of a block. -/
theorem done_scope_next {P : Program O} {b : BlockId} {dm : List Var} {d : Deferred O}
    {ds : List (Deferred O)} {L : AList Var O.Val} {st : O.Store} {k : List (Frame O)}
    {l : Label O} {c : Config O} (h : Step P ⟨.done, L, st, .scope b dm (d :: ds) :: k⟩ l c) :
    l = .silent ∧ c = ⟨.exec d.stmt, L, st, .resume .fall :: .scope b dm ds :: k⟩ := by
  cases h with
  | silent h =>
    simp only [step, startCleanup, Option.some.injEq, Out.next.injEq] at h
    exact ⟨rfl, h.symm⟩
  | prim h _ _ => simp [step, startCleanup] at h

/-- An exit leaving a scope whose calls complete runs them newest first, each with its events and
its effect on the state, and leaves the scope with its locals restored; the exit, and a returned
value, are unchanged. -/
theorem exit_scope_runs_defers (P : Program O) (e : Exit O.Val) (b : BlockId) (dm : List Var)
    (L : AList Var O.Val) (k : List (Frame O)) {ds : List (Deferred O)} {st st' : O.Store}
    {tr : List O.Event} (h : RunsAll P L ds st tr st') {stDrop : O.Store}
    (hd : dropOut O dm L st' = some stDrop) :
    Steps P ⟨.exiting e, L, st, .scope b dm ds :: k⟩ tr
      ⟨.exiting e, restrict dm L, stDrop, k⟩ := by
  induction h with
  | nil st => exact steps_next (by simp [step, stepExit, hd]) (Steps.refl _)
  | @cons d ds st tr₁ st₁ tr₂ st₂ hcall _ ih =>
    exact steps_next (c := ⟨.exiting e, L, st, .scope b dm (d :: ds) :: k⟩) rfl
      (Steps.trans (hcall _) (steps_next rfl (ih hd)))

/-- The normal end of a block runs its calls the same way. -/
theorem done_scope_runs_defers (P : Program O) (b : BlockId) (dm : List Var)
    (L : AList Var O.Val) (k : List (Frame O)) {ds : List (Deferred O)} {st st' : O.Store}
    {tr : List O.Event} (h : RunsAll P L ds st tr st') {stDrop : O.Store}
    (hd : dropOut O dm L st' = some stDrop) :
    Steps P ⟨.done, L, st, .scope b dm ds :: k⟩ tr ⟨.done, restrict dm L, stDrop, k⟩ := by
  induction h with
  | nil st => exact steps_next (by simp [step, hd]) (Steps.refl _)
  | @cons d ds st tr₁ st₁ tr₂ st₂ hcall _ ih =>
    exact steps_next (c := ⟨.done, L, st, .scope b dm (d :: ds) :: k⟩) rfl
      (Steps.trans (hcall _) (steps_next rfl (ih hd)))

/-- **Cleanup order.** When the cleanup calls complete (`Cleans`, which runs the calls of
`plan pre` in that order: the innermost scope first, each scope's newest registration first), an
exit passing frames `pre` arrives at the frames below with the same exit, the locals the scopes
restored and the state the calls left. -/
theorem unwind_runs_plan (P : Program O) (e : Exit O.Val) (k : List (Frame O))
    {pre : List (Frame O)} {L : AList Var O.Val} {st st' : O.Store} {tr : List O.Event}
    (h : Cleans P pre L st tr st') (hp : ∀ f ∈ pre, Passes e f) :
    Steps P ⟨.exiting e, L, st, pre ++ k⟩ tr ⟨.exiting e, unwindLocals pre L, st', k⟩ := by
  induction h with
  | nil L st => exact Steps.refl _
  | @scope b dm ds pre L st tr₁ st₁ stDrop tr₂ st₂ hr hd _ ih =>
    have h1 := exit_scope_runs_defers P e b dm L (pre ++ k) hr hd
    have h2 := ih (fun f hf => hp f (List.mem_cons_of_mem _ hf))
    simpa [unwindLocals] using Steps.trans h1 h2
  | seq _ ih => exact steps_next rfl (ih (fun f hf => hp f (List.mem_cons_of_mem _ hf)))
  | test hd _ ih =>
    exact steps_next (by simp [step, stepExit, hd]) (ih (fun f hf => hp f (List.mem_cons_of_mem _ hf)))
  | @body l cr t b site pre L st tr st' _ ih =>
    have ih' := ih (fun f hf => hp f (List.mem_cons_of_mem _ hf))
    have hf : Passes e (.body l cr t b site) := hp _ List.mem_cons_self
    cases e with
    | brk l' =>
      have hne : l' ≠ l := hf
      exact steps_next (by simp [step, stepExit, hne]) ih'
    | cont l' =>
      have hne : l' ≠ l := hf
      exact steps_next (by simp [step, stepExit, hne]) ih'
    | ret v => exact steps_next rfl ih'

/-- The plan of nested scopes lists the inner scope's calls first. -/
theorem plan_scopes (b₁ b₂ : BlockId) (d₁ d₂ : List Var) (ds₁ ds₂ : List (Deferred O))
    (k : List (Frame O)) :
    plan (.scope b₁ d₁ ds₁ :: .scope b₂ d₂ ds₂ :: k) = ds₁ ++ (ds₂ ++ plan k) := rfl

/-- **A `return` reaches its caller** after the cleanup of every scope it leaves: the caller's
locals come back with the value bound, in the state the cleanup left. -/
theorem return_reaches_caller (P : Program O) (v : O.Val) (dst : Option Var)
    (caller : AList Var O.Val) (k : List (Frame O)) {pre : List (Frame O)}
    {L : AList Var O.Val} {st st' : O.Store} {tr : List O.Event}
    (h : Cleans P pre L st tr st') (hp : ∀ f ∈ pre, Passes (.ret v) f)
    (stLeave stBind : O.Store) (hl : O.life .leave st' = some stLeave)
    (hb : bindLife O dst stLeave = some stBind) :
    Steps P ⟨.exiting (.ret v), L, st, pre ++ .ret dst caller :: k⟩ tr
      ⟨.done, bindDst dst v caller, stBind, k⟩ := by
  have := Steps.trans (unwind_runs_plan P (.ret v) (.ret dst caller :: k) h hp)
    (steps_next (step_return_caller P v dst caller _ st' k stLeave stBind hl hb) (Steps.refl _))
  simpa using this

/-- **A `break` reaches its loop** after the cleanup of the scopes inside the loop body, and the
loop is done. -/
theorem break_reaches_loop (P : Program O) (l : LoopId) (cr b : Stmt O) (t : O.Expr)
    (site : SiteId) (k : List (Frame O)) {pre : List (Frame O)} {L : AList Var O.Val}
    {st st' : O.Store} {tr : List O.Event}
    (h : Cleans P pre L st tr st') (hp : ∀ f ∈ pre, Passes (.brk l) f) :
    Steps P ⟨.exiting (.brk l), L, st, pre ++ .body l cr t b site :: k⟩ tr
      ⟨.done, unwindLocals pre L, st', k⟩ := by
  have := Steps.trans (unwind_runs_plan P (.brk l) (.body l cr t b site :: k) h hp)
    (steps_next (step_break_loop P l cr b t site _ st' k) (Steps.refl _))
  simpa using this

/-- **A `continue` reaches its loop** after the same cleanup, and the loop starts again from its
condition region. -/
theorem continue_reaches_loop (P : Program O) (l : LoopId) (cr b : Stmt O) (t : O.Expr)
    (site : SiteId) (k : List (Frame O)) {pre : List (Frame O)} {L : AList Var O.Val}
    {st st' : O.Store} {tr : List O.Event}
    (h : Cleans P pre L st tr st') (hp : ∀ f ∈ pre, Passes (.cont l) f) :
    Steps P ⟨.exiting (.cont l), L, st, pre ++ .body l cr t b site :: k⟩ tr
      ⟨.exec (.loop l cr t b site), unwindLocals pre L, st', k⟩ := by
  have := Steps.trans (unwind_runs_plan P (.cont l) (.body l cr t b site :: k) h hp)
    (steps_next (step_continue_loop P l cr b t site _ st' k) (Steps.refl _))
  simpa using this

/-! ### What the cleanup runs

`Cleans` runs the calls of `plan`, each with the locals of its scope's point in the unwinding, and
tells the instrumentation which locals each scope it leaves drops: `planWith` lists those actions in
order, its calls are `plan`'s (`planWith_calls`), and a cleanup is a run of exactly those actions
(`cleans_iff_runs_plan_inv`). -/

/-- One action of a cleanup: a call, with the locals it runs with, or leaving a scope entered with
`dm`, from locals `L`. -/
inductive CleanAct (O : Operands) where
  | call (L : AList Var O.Val) (d : Deferred O)
  | drop (dm : List Var) (L : AList Var O.Val)

/-- The call of an action, if it is one. -/
def CleanAct.call? : CleanAct O → Option (Deferred O)
  | .call _ d => some d
  | .drop .. => none

/-- The actions an exit through frames `pre` performs, from locals `L`, in order. -/
def planWith : List (Frame O) → AList Var O.Val → List (CleanAct O)
  | [], _ => []
  | .scope _ dm ds :: k, L => ds.map (.call L) ++ .drop dm L :: planWith k (restrict dm L)
  | .test _ _ _ _ _ dm :: k, L => .drop dm L :: planWith k (restrict dm L)
  | _ :: k, L => planWith k L

theorem planWith_calls : ∀ (pre : List (Frame O)) (L : AList Var O.Val),
    (planWith pre L).filterMap CleanAct.call? = plan pre
  | [], _ => rfl
  | .scope _ dm ds :: k, L => by
    have hds : (ds.map (CleanAct.call L)).filterMap CleanAct.call? = ds := by
      induction ds with
      | nil => rfl
      | cons d ds ih => simp [CleanAct.call?] at ih ⊢; exact ih
    simp only [planWith, plan, List.filterMap_append, hds, List.filterMap_cons, CleanAct.call?,
      planWith_calls k (restrict dm L)]
  | .test _ _ _ _ _ dm :: k, L => by
    simp only [planWith, plan, List.filterMap_cons, CleanAct.call?]
    exact planWith_calls k (restrict dm L)
  | .seq _ :: k, L => planWith_calls k L
  | .body .. :: k, L => planWith_calls k L
  | .resume _ :: k, L => planWith_calls k L
  | .ret .. :: k, L => planWith_calls k L

/-- A list of cleanup actions run one after another, the state threaded: each call completes, and
each scope left tells the instrumentation what it drops. -/
inductive RunsEach (P : Program O) : List (CleanAct O) → O.Store → List O.Event → O.Store → Prop where
  | nil (st : O.Store) : RunsEach P [] st [] st
  | call {L d rest st tr₁ st₁ tr₂ st₂} : Completes P L d st tr₁ st₁ →
      RunsEach P rest st₁ tr₂ st₂ → RunsEach P (.call L d :: rest) st (tr₁ ++ tr₂) st₂
  | drop {dm L rest st stDrop tr st'} : dropOut O dm L st = some stDrop →
      RunsEach P rest stDrop tr st' →
      RunsEach P (.drop dm L :: rest) st tr st'

theorem RunsAll.each {P : Program O} {L : AList Var O.Val} {ds : List (Deferred O)}
    {st st₁ st₂ : O.Store} {tr₁ tr₂ : List O.Event} {rest : List (CleanAct O)}
    (h : RunsAll P L ds st tr₁ st₁) (h' : RunsEach P rest st₁ tr₂ st₂) :
    RunsEach P (ds.map (.call L) ++ rest) st (tr₁ ++ tr₂) st₂ := by
  induction h with
  | nil => simpa using h'
  | cons hd _ ih => simpa [List.append_assoc] using RunsEach.call hd (ih h')

theorem RunsEach.split_calls {P : Program O} {L : AList Var O.Val} :
    ∀ {ds : List (Deferred O)} {rest : List (CleanAct O)} {st st₂ : O.Store} {tr : List O.Event},
    RunsEach P (ds.map (.call L) ++ rest) st tr st₂ →
    ∃ tr₁ st₁ tr₂, tr = tr₁ ++ tr₂ ∧ RunsAll P L ds st tr₁ st₁ ∧ RunsEach P rest st₁ tr₂ st₂
  | [], _, _, _, _, h => ⟨[], _, _, rfl, .nil _, by simpa using h⟩
  | d :: ds, rest, st, st₂, tr, h => by
    cases h with
    | call hd hr =>
      obtain ⟨tr₁, st₁, tr₂, rfl, ha, hb⟩ := RunsEach.split_calls hr
      exact ⟨_, st₁, tr₂, by simp [List.append_assoc], .cons hd ha, hb⟩

/-- The frames an exit can pass: every frame but a running cleanup call's and a call's (`Passes`
for some exit). -/
def Frame.passable : Frame O → Bool
  | .resume _ | .ret .. => false
  | _ => true

/-- Helper (inversion). Relates two cleanup certificates; not a law about actual runs. -/
theorem cleans_iff_runs_plan_inv {P : Program O} :
    ∀ {pre : List (Frame O)} {L : AList Var O.Val} {st st' : O.Store} {tr : List O.Event},
    (∀ f ∈ pre, f.passable = true) →
    (Cleans P pre L st tr st' ↔ RunsEach P (planWith pre L) st tr st')
  | [], L, st, st', tr, _ => by
    constructor
    · intro h; cases h; exact .nil _
    · intro h; cases h; exact .nil _ _
  | .scope b dm ds :: pre, L, st, st', tr, hp => by
    have hp' := fun f hf => hp f (List.mem_cons_of_mem _ hf)
    constructor
    · intro h
      cases h with
      | scope hr hd hc => exact RunsAll.each hr (.drop hd ((cleans_iff_runs_plan_inv hp').1 hc))
    · intro h
      obtain ⟨tr₁, st₁, tr₂, rfl, ha, hb⟩ := RunsEach.split_calls h
      cases hb with
      | drop hd hb => exact .scope ha hd ((cleans_iff_runs_plan_inv hp').2 hb)
  | .test l cr t b site dm :: pre, L, st, st', tr, hp => by
    have hp' := fun f hf => hp f (List.mem_cons_of_mem _ hf)
    constructor
    · intro h; cases h with
      | test hd hc => exact .drop hd ((cleans_iff_runs_plan_inv hp').1 hc)
    · intro h; cases h with
      | drop hd hb => exact .test hd ((cleans_iff_runs_plan_inv hp').2 hb)
  | .seq _ :: pre, L, st, st', tr, hp => by
    have hp' := fun f hf => hp f (List.mem_cons_of_mem _ hf)
    constructor
    · intro h; cases h with
      | seq hc => exact (cleans_iff_runs_plan_inv hp').1 hc
    · intro h; exact .seq ((cleans_iff_runs_plan_inv hp').2 h)
  | .body .. :: pre, L, st, st', tr, hp => by
    have hp' := fun f hf => hp f (List.mem_cons_of_mem _ hf)
    constructor
    · intro h; cases h with
      | body hc => exact (cleans_iff_runs_plan_inv hp').1 hc
    · intro h; exact .body ((cleans_iff_runs_plan_inv hp').2 h)
  | .resume _ :: _, _, _, _, _, hp => absurd (hp _ List.mem_cons_self) (by simp [Frame.passable])
  | .ret .. :: _, _, _, _, _, hp => absurd (hp _ List.mem_cons_self) (by simp [Frame.passable])

/-- Every frame an exit passes is passable. -/
theorem Passes.passable {e : Exit O.Val} : ∀ {f : Frame O}, Passes e f → f.passable = true
  | .seq _, _ | .scope .., _ | .test .., _ | .body .., _ => rfl
  | .resume _, h => h.elim
  | .ret .., h => h.elim

/-! ### Actual cleanup executions

A boundary is a fixed suffix of the continuation stack. `BoundaryRun` observes actual transitions
until an exit first reaches that suffix. Ordinary call bodies are first-return segments, so their
own nested cleanup is not confused with the registrations of the scopes being observed. The
observer neither consults `planWith` nor prescribes which deferred function a step must select.
It records the call selected by the transition and drops at the scope/test frame being left.
Consequently changing the dispatcher to skip or insert a call invalidates `cleanup_runs_exact_plan`.
-/

/-- An actual execution stopped at its first configuration satisfying `stop`. -/
inductive FirstSteps (R : Config O → Label O → Config O → Prop) (stop : Config O → Prop) :
    Config O → List O.Event → Config O → Prop where
  | stop {c} : stop c → FirstSteps R stop c [] c
  | step {c l c' tr c''} : ¬ stop c → R c l c' → FirstSteps R stop c' tr c'' →
      FirstSteps R stop c (l.events ++ tr) c''

/-- Helper (inversion). Forgetting first-arrival information preserves the actual execution. -/
theorem FirstSteps.steps_inv {P : Program O} {stop : Config O → Prop} {c c' : Config O}
    {tr : List O.Event} (h : FirstSteps (Step P) stop c tr c') : Steps P c tr c' := by
  induction h with
  | stop => exact .refl _
  | step _ hs _ ih => exact .step hs ih

/-- The local-lifetime action observed when an exit directly leaves its current frame.
Calls, including any nested cleanup inside them, are recorded separately. -/
def leaving : List (Frame O) → AList Var O.Val → List (CleanAct O)
  | .scope _ dm [] :: _, L => [.drop dm L]
  | .test _ _ _ _ _ dm :: _, L => [.drop dm L]
  | _, _ => []

/-- An observer of a successful exit to a fixed boundary, using a supplied outer transition
relation `R`. Call bodies use actual first-return executions. The parameter permits planted
mutations of the cleanup dispatcher without mutating any tracked source. The production law
uses `R = Step P`. A nonempty prefix is required before every observed transition: execution
stops at the first arrival at the boundary, before the caller/loop below it can run. -/
inductive BoundaryRun (P : Program O) (R : Config O → Label O → Config O → Prop)
    (e : Exit O.Val) (k : List (Frame O)) :
    List (Frame O) → AList Var O.Val → O.Store → List (CleanAct O) → List O.Event →
      AList Var O.Val → O.Store → Prop where
  | stop {L st} : BoundaryRun P R e k [] L st [] [] L st
  | pass {pre pre' L L' st st' acts tr Lend stEnd} : pre ≠ [] →
      R ⟨.exiting e, L, st, pre ++ k⟩ .silent ⟨.exiting e, L', st', pre' ++ k⟩ →
      BoundaryRun P R e k pre' L' st' acts tr Lend stEnd →
      BoundaryRun P R e k pre L st (leaving pre L ++ acts) tr Lend stEnd
  | call {pre rest L st st' d acts tr₁ tr₂ Lend stEnd} : pre ≠ [] →
      R ⟨.exiting e, L, st, pre ++ k⟩ .silent
        ⟨.exec d.stmt, L, st, .resume (.exit e) :: rest ++ k⟩ →
      FirstSteps (Step P) (fun c => c.ctrl = .done ∧ c.frames = .resume (.exit e) :: rest ++ k)
        ⟨.exec d.stmt, L, st, .resume (.exit e) :: rest ++ k⟩ tr₁
        ⟨.done, L, st', .resume (.exit e) :: rest ++ k⟩ →
      R ⟨.done, L, st', .resume (.exit e) :: rest ++ k⟩ .silent
        ⟨.exiting e, L, st', rest ++ k⟩ →
      BoundaryRun P R e k rest L st' acts tr₂ Lend stEnd →
      BoundaryRun P R e k pre L st (.call L d :: acts) (tr₁ ++ tr₂) Lend stEnd

/-- Representation lemma. A boundary observation is an actual execution to the stated suffix;
its segments do not add any transitions to the machine. -/
theorem BoundaryRun.steps {P : Program O} {e : Exit O.Val} {k pre : List (Frame O)}
    {L L' : AList Var O.Val} {st st' : O.Store} {acts : List (CleanAct O)} {tr : List O.Event}
    (h : BoundaryRun P (Step P) e k pre L st acts tr L' st') :
    Steps P ⟨.exiting e, L, st, pre ++ k⟩ tr ⟨.exiting e, L', st', k⟩ := by
  induction h with
  | stop => exact .refl _
  | pass _ hs _ ih => simpa [Label.events] using Steps.step hs ih
  | call _ hs hc hr _ ih =>
    have tail := Steps.step hr ih
    have middle := Steps.trans hc.steps_inv tail
    simpa [Label.events] using Steps.step hs middle

/-- Execute an action list independently of the cleanup dispatcher. Calls must actually return
on some continuation; drops must actually perform the lifecycle update. This weaker, per-run
notion replaces `RunsEach`'s universal call-completion premise for observed executions. -/
inductive Performed (P : Program O) :
    List (CleanAct O) → O.Store → List O.Event → O.Store → Prop where
  | nil {st} : Performed P [] st [] st
  | call {L d rest st tr₁ st₁ tr₂ st₂ k} :
      Steps P ⟨.exec d.stmt, L, st, k⟩ tr₁ ⟨.done, L, st₁, k⟩ →
      Performed P rest st₁ tr₂ st₂ →
      Performed P (.call L d :: rest) st (tr₁ ++ tr₂) st₂
  | drop {dm L rest st stDrop tr st'} : dropOut O dm L st = some stDrop →
      Performed P rest stDrop tr st' → Performed P (.drop dm L :: rest) st tr st'

/-- Helper (inversion). Compose independent action executions. -/
private theorem performed_append_inv {P : Program O} {as bs : List (CleanAct O)}
    {st st₁ st₂ : O.Store} {tr₁ tr₂ : List O.Event}
    (ha : Performed P as st tr₁ st₁) (hb : Performed P bs st₁ tr₂ st₂) :
    Performed P (as ++ bs) st (tr₁ ++ tr₂) st₂ := by
  induction ha with
  | nil => simpa using hb
  | call hc _ ih => simpa [List.append_assoc] using Performed.call hc (ih hb)
  | drop hd _ ih => exact .drop hd (ih hb)

/-- Helper (inversion). Unwinding a frame never itself makes a primitive request. -/
private theorem stepExit_no_request_inv (L : AList Var O.Val) (st : O.Store) (e : Exit O.Val)
    (frames : List (Frame O)) (p : O.Prim) (vs : List O.Val) (site : SiteId) (st' : O.Store)
    (cont : Answer O.Val O.Event O.Store → Option (Config O)) :
    stepExit L st e frames ≠ some (.request p vs site st' cont) := by
  cases frames with
  | nil => simp [stepExit]
  | cons f frames =>
    cases f with
    | seq t => simp [stepExit]
    | scope b dm ds =>
      cases ds with
      | nil => cases hd : dropOut O dm L st <;> simp [stepExit, hd]
      | cons d ds => simp [stepExit, startCleanup]
    | test l cr t body site dm =>
      cases hd : dropOut O dm L st <;> simp [stepExit, hd]
    | body l cr t body site =>
      cases e with
      | ret v => simp [stepExit]
      | brk l' => by_cases he : l' = l <;> simp [stepExit, he]
      | cont l' => by_cases he : l' = l <;> simp [stepExit, he]
    | resume p => simp [stepExit]
    | ret dst caller =>
      cases e with
      | brk l => simp [stepExit]
      | cont l => simp [stepExit]
      | ret v =>
        cases hl : O.life .leave st with
        | none => simp [stepExit, hl]
        | some stLeave =>
          cases hb : bindLife O dst stLeave <;> simp [stepExit, hl, hb]

/-- Helper (inversion). Every actual exit transition is a machine step. -/
private theorem exit_step_next_inv {P : Program O} {e : Exit O.Val} {L : AList Var O.Val}
    {st : O.Store} {frames : List (Frame O)} {label : Label O} {c : Config O}
    (h : Step P ⟨.exiting e, L, st, frames⟩ label c) :
    step P ⟨.exiting e, L, st, frames⟩ = some (.next c) := by
  cases h with
  | silent h => exact h
  | prim h _ _ => exact False.elim (stepExit_no_request_inv L st e frames _ _ _ _ _ h)

/-- Helper (inversion). A directly passing exit step removes precisely the top frame and
performs its local drop, if any. This is a rule-sensitive inversion, not a contract law. -/
private theorem exit_pass_plan_inv {P : Program O} {e : Exit O.Val} {k pre pre' : List (Frame O)}
    {L L' : AList Var O.Val} {st st' : O.Store} {label : Label O}
    (h : Step P ⟨.exiting e, L, st, pre ++ k⟩ label
      ⟨.exiting e, L', st', pre' ++ k⟩) (hne : pre ≠ []) :
    planWith pre L = leaving pre L ++ planWith pre' L' ∧
      Performed P (leaving pre L) st [] st' := by
  cases pre with
  | nil => exact False.elim (hne rfl)
  | cons f pre =>
    have next : step P ⟨.exiting e, L, st, f :: (pre ++ k)⟩ =
        some (.next ⟨.exiting e, L', st', pre' ++ k⟩) := exit_step_next_inv h
    have finish (hL : L = L') (hk : pre ++ k = pre' ++ k) :
        L = L' ∧ pre = pre' := ⟨hL, List.append_cancel_right hk⟩
    cases f with
    | seq t =>
      simp only [step, stepExit, Option.some.injEq, Out.next.injEq, Config.mk.injEq] at next
      obtain ⟨_, hL, hst, hk⟩ := next
      subst st'
      obtain ⟨rfl, rfl⟩ := finish hL hk
      exact ⟨rfl, .nil⟩
    | scope b dm ds =>
      cases ds with
      | cons d ds => simp [step, stepExit, startCleanup] at next
      | nil =>
        cases hd : dropOut O dm L st with
        | none => simp [step, stepExit, hd] at next
        | some stDrop =>
          simp only [step, stepExit, hd, Option.map_some, Option.some.injEq, Out.next.injEq,
            Config.mk.injEq] at next
          obtain ⟨_, hL, hst, hk⟩ := next
          have hk' := List.append_cancel_right hk
          subst pre'
          subst L'
          subst st'
          exact ⟨rfl, .drop hd .nil⟩
    | test l cr t body site dm =>
      cases hd : dropOut O dm L st with
      | none => simp [step, stepExit, hd] at next
      | some stDrop =>
        simp only [step, stepExit, hd, Option.map_some, Option.some.injEq, Out.next.injEq,
          Config.mk.injEq] at next
        obtain ⟨_, hL, hst, hk⟩ := next
        have hk' := List.append_cancel_right hk
        subst pre'
        subst L'
        subst st'
        exact ⟨rfl, .drop hd .nil⟩
    | body l cr t body site =>
      cases e with
      | ret v =>
        simp only [step, stepExit, Option.some.injEq, Out.next.injEq, Config.mk.injEq] at next
        obtain ⟨_, hL, hst, hk⟩ := next
        obtain ⟨rfl, rfl⟩ := finish hL hk
        subst st'
        exact ⟨rfl, .nil⟩
      | brk l' =>
        by_cases he : l' = l
        · simp [step, stepExit, he] at next
        · simp only [step, stepExit, he, ↓reduceIte, Option.some.injEq, Out.next.injEq,
            Config.mk.injEq] at next
          obtain ⟨_, hL, hst, hk⟩ := next
          obtain ⟨rfl, rfl⟩ := finish hL hk
          subst st'
          exact ⟨rfl, .nil⟩
      | cont l' =>
        by_cases he : l' = l
        · simp [step, stepExit, he] at next
        · simp only [step, stepExit, he, ↓reduceIte, Option.some.injEq, Out.next.injEq,
            Config.mk.injEq] at next
          obtain ⟨_, hL, hst, hk⟩ := next
          obtain ⟨rfl, rfl⟩ := finish hL hk
          subst st'
          exact ⟨rfl, .nil⟩
    | resume p => simp [step, stepExit] at next
    | ret dst caller =>
      cases e <;> simp [step, stepExit] at next
      case ret v =>
        cases hl : O.life .leave st <;> simp [hl] at next


/-- Helper (inversion). A cleanup dispatch step selects exactly the head registration, preserving
its scope with the remaining registrations. -/
private theorem exit_call_plan_inv {P : Program O} {e : Exit O.Val} {k pre rest : List (Frame O)}
    {L : AList Var O.Val} {st : O.Store} {d : Deferred O} {label : Label O}
    (h : Step P ⟨.exiting e, L, st, pre ++ k⟩ label
      ⟨.exec d.stmt, L, st, .resume (.exit e) :: rest ++ k⟩) (hne : pre ≠ []) :
    planWith pre L = .call L d :: planWith rest L := by
  cases pre with
  | nil => exact False.elim (hne rfl)
  | cons f pre =>
    have hn : step P ⟨.exiting e, L, st, f :: (pre ++ k)⟩ =
        some (.next ⟨.exec d.stmt, L, st, .resume (.exit e) :: rest ++ k⟩) :=
      exit_step_next_inv h
    clear h
    have h := hn
    cases f with
    | scope b dm ds =>
      cases ds with
      | nil =>
        cases hd : dropOut O dm L st <;> simp [step, stepExit, hd] at h
      | cons d' ds =>
        simp only [step, stepExit, startCleanup, Option.some.injEq, Out.next.injEq,
          Config.mk.injEq, Ctrl.exec.injEq] at h
        obtain ⟨hd, _, _, hk⟩ := h
        have hds : d' = d := by
          have hd' : d'.f = d.f ∧ d'.args = d.args ∧ d'.site = d.site := by
            simpa [Deferred.stmt, Stmt.call.injEq] using hd
          cases d' with | mk f args site =>
            cases d with | mk f' args' site' =>
              obtain ⟨hf, ha, hs⟩ := hd'
              simp only at hf ha hs
              subst f'; subst args'; subst site'
              rfl
        subst d'
        have hr : .scope b dm ds :: pre = rest := by
          apply List.append_cancel_right (bs := k)
          exact List.cons.inj hk |>.2
        subst rest
        simp [planWith]
    | seq t => simp [step, stepExit] at h
    | test l cr t body site dm =>
      cases hd : dropOut O dm L st <;> simp [step, stepExit, hd] at h
    | resume p => simp [step, stepExit] at h
    | ret dst caller =>
      cases e <;> simp [step, stepExit] at h
      case ret v => cases hl : O.life .leave st <;> simp [hl] at h
    | body l cr t body site =>
      cases e with
      | ret v => simp [step, stepExit] at h
      | brk l' => by_cases he : l' = l <;> simp [step, stepExit, he] at h
      | cont l' => by_cases he : l' = l <;> simp [step, stepExit, he, Deferred.stmt] at h

/-- A successful actual unwind, stopped on first arrival at the selected stack boundary, dispatches
exactly the planned calls and drops in order. The observed actions are read from actual transitions,
not required by the execution certificate. No extra or missing cleanup action is possible. -/
theorem cleanup_runs_exact_plan {P : Program O} {e : Exit O.Val} {k pre : List (Frame O)}
    {L L' : AList Var O.Val} {st st' : O.Store} {acts : List (CleanAct O)} {tr : List O.Event}
    (h : BoundaryRun P (Step P) e k pre L st acts tr L' st') :
    acts = planWith pre L ∧ Performed P acts st tr st' := by
  induction h with
  | stop => exact ⟨rfl, .nil⟩
  | pass hne hs _ ih =>
    obtain ⟨he, hp⟩ := exit_pass_plan_inv hs hne
    exact ⟨by rw [ih.1, ← he], by simpa using performed_append_inv hp ih.2⟩
  | call hne hs hc _ _ ih =>
    exact ⟨by rw [ih.1, ← exit_call_plan_inv hs hne], .call hc.steps_inv ih.2⟩

/-! ### Traps are terminal -/

theorem step_trapped (P : Program O) (kd : TrapKind) (site : SiteId) (L : AList Var O.Val)
    (st : O.Store) (k : List (Frame O)) : step P ⟨.trapped kd site, L, st, k⟩ = none := rfl

/-- After a trap nothing runs: no step, no event, and the frames are not unwound. -/
theorem steps_from_trapped {P : Program O} {kd : TrapKind} {site : SiteId} {L : AList Var O.Val}
    {st : O.Store} {k : List (Frame O)} {tr : List O.Event} {c : Config O}
    (h : Steps P ⟨.trapped kd site, L, st, k⟩ tr c) :
    tr = [] ∧ c = ⟨.trapped kd site, L, st, k⟩ := by
  cases h with
  | refl => exact ⟨rfl, rfl⟩
  | step hs _ =>
    cases hs with
    | silent h => simp [step_trapped] at h
    | prim h _ _ => simp [step_trapped] at h

/-- A call retains its saved caller frame until normal return. -/
private def Protected (L : AList Var O.Val) (q : List (Frame O)) (c : Config O) : Prop :=
  ∃ pre, c.frames = pre ++ .ret none L :: q

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem register_protected_inv (d : Deferred O) (caller : AList Var O.Val)
    (q : List (Frame O)) : ∀ (pre : List (Frame O)) (k' : List (Frame O)),
    register d (pre ++ .ret none caller :: q) = some k' →
    ∃ pre', k' = pre' ++ .ret none caller :: q
  | [], _, h => by simp [register] at h
  | f :: pre, k', h => by
    cases f with
    | seq t =>
      simp only [List.cons_append, register] at h
      cases hr : register d (pre ++ .ret none caller :: q) with
      | none => simp [hr] at h
      | some rest =>
        simp only [hr, Option.map_some, Option.some.injEq] at h
        obtain ⟨pre', rfl⟩ := register_protected_inv d caller q pre rest hr
        exact ⟨.seq t :: pre', h.symm⟩
    | scope b dm ds =>
      simp only [List.cons_append, register, Option.some.injEq] at h
      exact ⟨.scope b dm (d :: ds) :: pre, h.symm⟩
    | test l cr t body site dm => simp [register] at h
    | body l cr t body site => simp [register] at h
    | resume p => simp [register] at h
    | ret dst L => simp [register] at h

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem protected_step_inv {P : Program O} {caller : AList Var O.Val}
    {q : List (Frame O)} {c c' : Config O} {label : Label O}
    (hp : Protected caller q c) (h : Step P c label c') :
    Protected caller q c' ∨ (c'.ctrl = .done ∧ c'.locals = caller ∧ c'.frames = q) := by
  cases c with | mk ctrl locals st frames =>
    obtain ⟨pre, rfl⟩ := hp
    cases h with
    | silent h =>
      cases pre with
      | nil =>
        change step P ⟨ctrl, locals, st, .ret none caller :: q⟩ = _ at h
        cases ctrl with
        | exec stmt =>
          cases stmt <;> simp only [step, onRes, Option.bind, Option.map, bindLife] at h <;>
            repeat' first | simp_all [register] | split at h
          all_goals try simp only [Option.some.injEq, Out.next.injEq] at h
          all_goals try subst c'
          all_goals try contradiction
          all_goals repeat' first | simp_all [Protected] | split at h
          all_goals try grind [Protected, register_protected_inv]
          all_goals try first
            | rfl
            | exact ⟨[], rfl⟩
            | exact ⟨[_], rfl⟩
            | exact ⟨[_, _], rfl⟩
            | exact .inl ⟨[], rfl⟩
            | exact .inl ⟨[_], rfl⟩
            | exact .inl ⟨[_, _], rfl⟩
            | (rcases h with ⟨_, rfl⟩; exact .inl ⟨[_, _], rfl⟩)
        | done =>
          simp only [step, Option.bind, Option.map, bindLife] at h
          repeat' first | simp_all | split at h
          all_goals try simp only [Option.some.injEq, Out.next.injEq] at h
          all_goals try subst c'
          all_goals try contradiction
          all_goals repeat' first | simp_all [Protected] | split at h
          all_goals exact .inr rfl
        | exiting e =>
          cases e <;> simp only [step, stepExit, Option.bind, Option.map, bindLife] at h
          all_goals repeat' first | simp_all | split at h
          all_goals try simp only [Option.some.injEq, Out.next.injEq] at h
          all_goals try subst c'
          all_goals try contradiction
          all_goals repeat' first | simp_all [Protected, bindDst] | split at h
          all_goals exact .inr rfl
        | trapped kd site => simp [step] at h
      | cons f pre =>
        change step P ⟨ctrl, locals, st, f :: (pre ++ .ret none caller :: q)⟩ = _ at h
        cases ctrl with
        | exec stmt =>
          cases stmt <;> simp only [step, onRes, Option.bind, Option.map, bindLife] at h <;>
            repeat' first | simp_all | split at h
          all_goals try simp only [Option.some.injEq, Out.next.injEq] at h
          all_goals try subst c'
          all_goals try contradiction
          all_goals repeat' first | simp_all [Protected] | split at h
          all_goals try grind [Protected, register_protected_inv]
          all_goals try first
            | rfl
            | exact ⟨f :: pre, rfl⟩
            | exact ⟨_ :: f :: pre, rfl⟩
            | exact ⟨_ :: _ :: f :: pre, rfl⟩
            | exact .inl ⟨f :: pre, rfl⟩
            | exact .inl (register_protected_inv _ caller q (f :: pre) _ (by assumption))
            | exact .inl ⟨_ :: f :: pre, rfl⟩
            | exact .inl ⟨_ :: _ :: f :: pre, rfl⟩
        | done =>
          cases f <;> simp only [step, onRes, startCleanup, Option.bind, Option.map, bindLife] at h <;>
            repeat' first | simp_all | split at h
          all_goals try simp only [Option.some.injEq, Out.next.injEq] at h
          all_goals try subst c'
          all_goals try contradiction
          all_goals repeat' first | simp_all [Protected, bindDst] | split at h
          all_goals try grind [Protected, register_protected_inv]
          all_goals try first
            | rfl
            | exact ⟨pre, rfl⟩
            | exact ⟨_ :: pre, rfl⟩
            | exact ⟨_ :: _ :: pre, rfl⟩
            | exact .inl ⟨pre, rfl⟩
            | exact .inl ⟨_ :: pre, rfl⟩
            | exact .inl ⟨_ :: _ :: pre, rfl⟩
        | exiting e =>
          cases f <;> cases e <;>
            simp only [step, stepExit, startCleanup, Option.bind, Option.map, bindLife] at h <;>
            repeat' first | simp_all | split at h
          all_goals try simp only [Option.some.injEq, Out.next.injEq] at h
          all_goals try subst c'
          all_goals try contradiction
          all_goals repeat' first | simp_all [Protected, bindDst] | split at h
          all_goals try grind [Protected, register_protected_inv]
          all_goals try first
            | rfl
            | exact ⟨pre, rfl⟩
            | exact ⟨_ :: pre, rfl⟩
            | exact ⟨_ :: _ :: pre, rfl⟩
            | exact .inl ⟨pre, rfl⟩
            | exact .inl ⟨_ :: pre, rfl⟩
            | exact .inl ⟨_ :: _ :: pre, rfl⟩
        | trapped kd site => simp [step] at h
    | prim h ha hk =>
      cases ctrl with
      | exec stmt =>
        cases stmt <;> simp only [step, onRes, Option.bind, Option.map] at h <;>
          repeat' first | simp_all [Protected, bindLife] | split at h | split at hk
        all_goals try grind [Protected]
        all_goals exact ⟨pre, rfl⟩
      | done =>
        cases pre with
        | nil =>
          change step P ⟨.done, locals, st, .ret none caller :: q⟩ = _ at h
          simp only [step, Option.bind, Option.map, bindLife] at h
          repeat' first | simp_all | split at h
        | cons f pre =>
          change step P ⟨.done, locals, st, f :: (pre ++ .ret none caller :: q)⟩ = _ at h
          cases f <;> simp only [step, onRes, startCleanup, Option.bind, Option.map] at h <;>
            repeat' first | simp_all | split at h
      | exiting e => exact False.elim (stepExit_no_request_inv locals st e _ _ _ _ _ _ h)
      | trapped kd site => simp [step] at h
/-- A step count for a first-arrival execution, used only for induction on strict subpaths. -/
private inductive FirstStepsN (R : Config O → Label O → Config O → Prop) (stop : Config O → Prop) :
    Config O → List O.Event → Config O → Nat → Prop where
  | stop {c} : stop c → FirstStepsN R stop c [] c 0
  | step {c l c' tr c'' n} : ¬ stop c → R c l c' → FirstStepsN R stop c' tr c'' n →
      FirstStepsN R stop c (l.events ++ tr) c'' (n + 1)

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem FirstSteps.sized_inv {P : Program O} {stop : Config O → Prop}
    {c c' : Config O} {tr : List O.Event} (h : FirstSteps (Step P) stop c tr c') :
    ∃ n, FirstStepsN (Step P) stop c tr c' n := by
  induction h with
  | stop hs => exact ⟨0, .stop hs⟩
  | step hn hs _ ih => obtain ⟨n, ih⟩ := ih; exact ⟨n + 1, .step hn hs ih⟩

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem FirstStepsN.unsized_inv {P : Program O} {stop : Config O → Prop}
    {c c' : Config O} {tr : List O.Event} {n : Nat} (h : FirstStepsN (Step P) stop c tr c' n) :
    FirstSteps (Step P) stop c tr c' := by
  induction h with
  | stop hs => exact .stop hs
  | step hn hs _ ih => exact .step hn hs ih

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem protected_height_inv {caller : AList Var O.Val} {q : List (Frame O)}
    {c : Config O} (h : Protected caller q c) : q.length < c.frames.length := by
  obtain ⟨pre, he⟩ := h
  simp only [he, List.length_append, List.length_cons]
  omega

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem protected_not_return_inv {caller : AList Var O.Val} {q : List (Frame O)}
    {c : Config O} (h : Protected caller q c) : ¬ (c.ctrl = .done ∧ c.frames = q) := by
  intro hr
  have hh := protected_height_inv h
  rw [hr.2] at hh
  exact Nat.lt_irrefl _ hh

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem FirstStepsN.split_return_inv {P : Program O} {bound : Nat}
    {c endC : Config O} {tr : List O.Event} {n : Nat}
    (h : FirstStepsN (Step P) (fun c => c.frames.length ≤ bound) c tr endC n) :
    ∀ {caller : AList Var O.Val} {q : List (Frame O)}, bound < q.length → Protected caller q c →
      ∃ n₁ n₂ tr₁ st₁ tr₂, n = n₁ + n₂ ∧ tr = tr₁ ++ tr₂ ∧
        FirstStepsN (Step P) (fun c => c.ctrl = .done ∧ c.frames = q) c tr₁ ⟨.done, caller, st₁, q⟩ n₁ ∧
        FirstStepsN (Step P) (fun c => c.frames.length ≤ bound) ⟨.done, caller, st₁, q⟩ tr₂ endC n₂ := by
  induction h with
  | stop hs =>
    intro caller q hq hp
    have hh := protected_height_inv hp
    omega
  | @step c l c' tr endC n hn hs ht ih =>
    intro caller q hq hp
    cases protected_step_inv hp hs with
    | inl hp' =>
      obtain ⟨n₁, n₂, tr₁, st₁, tr₂, hc, he, ha, hb⟩ := ih hq hp'
      exact ⟨n₁ + 1, n₂, l.events ++ tr₁, st₁, tr₂, by omega,
        by simp [he, List.append_assoc], .step (protected_not_return_inv hp) hs ha, hb⟩
    | inr hr =>
      have he : c' = ⟨.done, caller, c'.store, q⟩ := by cases c'; simp_all
      rw [he] at hs ht
      have hseg : FirstStepsN (Step P) (fun c => c.ctrl = .done ∧ c.frames = q)
          c l.events ⟨.done, caller, c'.store, q⟩ 1 := by
        simpa using (FirstStepsN.step (protected_not_return_inv hp) hs
          (FirstStepsN.stop (R := Step P) (stop := fun c : Config O => c.ctrl = .done ∧ c.frames = q) ⟨rfl, rfl⟩))
      exact ⟨1, n, l.events, c'.store, tr, by omega, rfl, hseg, ht⟩

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem call_protects_inv {P : Program O} {L : AList Var O.Val} {st : O.Store}
    {q : List (Frame O)} {d : Deferred O} {label : Label O} {c' : Config O}
    (h : Step P ⟨.exec d.stmt, L, st, q⟩ label c') :
    (∃ kd site, c'.ctrl = .trapped kd site ∧ c'.frames = q) ∨ Protected L q c' := by
  cases h with
  | silent h =>
    simp only [step, Deferred.stmt, onRes, Option.bind, Option.map] at h
    repeat' first | simp_all [Protected] | split at h
    all_goals try subst c'
    all_goals try simp_all
    all_goals try first
      | exact .inl ⟨_, _, rfl, rfl⟩
      | exact ⟨[_], rfl⟩
      | (rcases h with ⟨_, rfl⟩; exact .inr ⟨[_], rfl⟩)
  | prim h _ _ =>
    simp only [step, Deferred.stmt, onRes, Option.bind, Option.map] at h
    repeat' first | simp_all | split at h

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem FirstStepsN.end_stop_inv {P : Program O} {stop : Config O → Prop}
    {c c' : Config O} {tr : List O.Event} {n : Nat} (h : FirstStepsN (Step P) stop c tr c' n) : stop c' := by
  induction h with
  | stop hs => exact hs
  | step _ _ _ ih => exact ih

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem FirstStepsN.trap_impossible_inv {P : Program O} {bound : Nat}
    {kd : TrapKind} {site : SiteId} {L : AList Var O.Val} {st : O.Store}
    {q : List (Frame O)} {tr : List O.Event} {endC : Config O} {n : Nat}
    (hq : bound < q.length)
    (h : FirstStepsN (Step P) (fun c => c.frames.length ≤ bound) ⟨.trapped kd site, L, st, q⟩ tr endC n) :
    False := by
  have he := (steps_from_trapped h.unsized_inv.steps_inv).2
  have hs := h.end_stop_inv
  rw [he] at hs
  exact Nat.not_le_of_gt hq hs

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem FirstStepsN.call_return_inv {P : Program O} {bound : Nat}
    {L : AList Var O.Val} {st : O.Store} {q : List (Frame O)} {d : Deferred O}
    {tr : List O.Event} {endC : Config O} {n : Nat} (hq : bound < q.length)
    (h : FirstStepsN (Step P) (fun c => c.frames.length ≤ bound) ⟨.exec d.stmt, L, st, q⟩ tr endC n) :
    ∃ n₁ n₂ tr₁ st₁ tr₂, n = n₁ + n₂ ∧ tr = tr₁ ++ tr₂ ∧
      FirstStepsN (Step P) (fun c => c.ctrl = .done ∧ c.frames = q)
        ⟨.exec d.stmt, L, st, q⟩ tr₁ ⟨.done, L, st₁, q⟩ n₁ ∧
      FirstStepsN (Step P) (fun c => c.frames.length ≤ bound) ⟨.done, L, st₁, q⟩ tr₂ endC n₂ := by
  cases h with
  | stop hs => exact False.elim (Nat.not_le_of_gt hq hs)
  | @step _ l c' tr endC n hn hs ht =>
    cases call_protects_inv hs with
    | inl hp =>
      obtain ⟨kd, site, hc, hf⟩ := hp
      have he : c' = ⟨.trapped kd site, c'.locals, c'.store, q⟩ := by cases c'; simp_all
      rw [he] at ht
      exact False.elim (FirstStepsN.trap_impossible_inv hq ht)
    | inr hp =>
      obtain ⟨n₁, n₂, tr₁, st₁, tr₂, hc, he, ha, hb⟩ := ht.split_return_inv hq hp
      exact ⟨n₁ + 1, n₂, l.events ++ tr₁, st₁, tr₂, by omega,
        by simp [he, List.append_assoc], .step (by intro h; cases h.1) hs ha, hb⟩

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem exit_label_inv {P : Program O} {e : Exit O.Val} {L : AList Var O.Val}
    {st : O.Store} {frames : List (Frame O)} {label : Label O} {c : Config O}
    (h : Step P ⟨.exiting e, L, st, frames⟩ label c) : label = .silent := by
  cases h with
  | silent _ => rfl
  | prim h _ _ => exact False.elim (stepExit_no_request_inv L st e frames _ _ _ _ _ h)

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem resume_step_inv {P : Program O} {e : Exit O.Val} {L : AList Var O.Val}
    {st : O.Store} {frames : List (Frame O)} {label : Label O} {c : Config O}
    (h : Step P ⟨.done, L, st, .resume (.exit e) :: frames⟩ label c) :
    label = .silent ∧ c = ⟨.exiting e, L, st, frames⟩ := by
  cases h with
  | silent h =>
    simp only [step, Option.some.injEq, Out.next.injEq] at h
    exact ⟨rfl, h.symm⟩
  | prim h _ _ => simp [step] at h

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem boundary_complete_inv {P : Program O} {e : Exit O.Val} {k pre : List (Frame O)}
    {L Lend : AList Var O.Val} {st stEnd : O.Store} {tr : List O.Event} {n : Nat} {endC : Config O}
    (h : FirstStepsN (Step P) (fun c => c.frames.length ≤ k.length)
      ⟨.exiting e, L, st, pre ++ k⟩ tr endC n)
    (he : endC = ⟨.exiting e, Lend, stEnd, k⟩)
    (hp : ∀ f ∈ pre, Passes e f) :
    ∃ acts, BoundaryRun P (Step P) e k pre L st acts tr Lend stEnd := by
  induction n using Nat.strongRecOn generalizing pre L st tr endC with
  | ind n ih =>
    cases pre with
    | nil =>
      cases h with
      | stop _ =>
        have hm : L = Lend ∧ st = stEnd := by
          simpa only [List.nil_append, Config.mk.injEq, true_and, and_true] using he
        obtain ⟨rfl, rfl⟩ := hm
        exact ⟨[], .stop⟩
      | step hn _ _ => exact False.elim (hn (by simp))
    | cons f pre =>
      have hp' : ∀ f ∈ pre, Passes e f := fun f hf => hp f (List.mem_cons_of_mem _ hf)
      have hpf : Passes e f := hp f List.mem_cons_self
      cases h with
      | stop hs => simp at hs; omega
      | @step _ label c' tr₀ _ m hn hs ht =>
        have hl := exit_label_inv hs
        subst label
        have hnext := exit_step_next_inv hs
        change step P ⟨.exiting e, L, st, f :: (pre ++ k)⟩ = _ at hnext
        have pass (pre' : List (Frame O)) (L' : AList Var O.Val) (st' : O.Store)
            (hc : c' = ⟨.exiting e, L', st', pre' ++ k⟩)
            (hpp : ∀ f ∈ pre', Passes e f) :
            ∃ acts, BoundaryRun P (Step P) e k (f :: pre) L st acts
              (Label.silent.events ++ tr₀) Lend stEnd := by
          rw [hc] at ht hs
          obtain ⟨acts, hr⟩ := ih m (by omega) ht he hpp
          exact ⟨leaving (f :: pre) L ++ acts,
            by simpa [Label.events] using BoundaryRun.pass (by simp) hs hr⟩
        cases f with
        | seq t =>
          simp only [step, stepExit, Option.some.injEq, Out.next.injEq] at hnext
          exact pass pre L st hnext.symm hp'
        | scope b dm ds =>
          cases ds with
          | nil =>
            cases hd : dropOut O dm L st with
            | none => simp [step, stepExit, hd] at hnext
            | some stDrop =>
              simp only [step, stepExit, hd, Option.map_some, Option.some.injEq, Out.next.injEq] at hnext
              exact pass pre (restrict dm L) stDrop hnext.symm hp'
          | cons d ds =>
            simp only [step, stepExit, startCleanup, Option.some.injEq, Out.next.injEq] at hnext
            subst c'
            have hq : k.length < (.resume (.exit e) :: .scope b dm ds :: (pre ++ k)).length := by
              simp only [List.length_cons, List.length_append]; omega
            obtain ⟨nc, nr, trCall, stCall, trRest, hnCall, heCall, hCall, hRest⟩ :=
              ht.call_return_inv hq
            cases hRest with
            | stop hstop => simp only [List.length_cons, List.length_append] at hstop; omega
            | @step _ lab cNext trTail _ nt hnRest hsRest hTail =>
              obtain ⟨hl, hc⟩ := resume_step_inv hsRest
              subst lab
              subst cNext
              have hpp : ∀ f ∈ (.scope b dm ds :: pre), Passes e f := by
                intro f hf
                cases hf with
                | head => trivial
                | tail _ hf => exact hp' f hf
              obtain ⟨acts, hr⟩ := ih nt (by omega) hTail he hpp
              refine ⟨.call L d :: acts, ?_⟩
              have hb := BoundaryRun.call (pre := .scope b dm (d :: ds) :: pre)
                (rest := .scope b dm ds :: pre) (by simp) hs hCall.unsized_inv hsRest hr
              simpa [heCall, Label.events] using hb
        | test l cr t body site dm =>
          cases hd : dropOut O dm L st with
          | none => simp [step, stepExit, hd] at hnext
          | some stDrop =>
            simp only [step, stepExit, hd, Option.map_some, Option.some.injEq, Out.next.injEq] at hnext
            exact pass pre (restrict dm L) stDrop hnext.symm hp'
        | body l cr t body site =>
          cases e with
          | ret v =>
            simp only [step, stepExit, Option.some.injEq, Out.next.injEq] at hnext
            exact pass pre L st hnext.symm hp'
          | brk l' =>
            have hne : l' ≠ l := hpf
            simp only [step, stepExit, hne, ↓reduceIte, Option.some.injEq, Out.next.injEq] at hnext
            exact pass pre L st hnext.symm hp'
          | cont l' =>
            have hne : l' ≠ l := hpf
            simp only [step, stepExit, hne, ↓reduceIte, Option.some.injEq, Out.next.injEq] at hnext
            exact pass pre L st hnext.symm hp'
        | resume p => exact False.elim hpf
        | ret dst caller => exact False.elim hpf

/-- Every raw first-boundary unwind through passable frames has an observation; the observer
cannot silently exclude an execution with an unexpected dispatcher shape. The proof derives
call-return segmentation from stack protection, rather than requiring it of the raw run. -/
theorem raw_cleanup_runs_plan {P : Program O} {e : Exit O.Val} {k pre : List (Frame O)}
    {L Lend : AList Var O.Val} {st stEnd : O.Store} {tr : List O.Event}
    (h : FirstSteps (Step P) (fun c => c.frames.length ≤ k.length)
      ⟨.exiting e, L, st, pre ++ k⟩ tr ⟨.exiting e, Lend, stEnd, k⟩)
    (hp : ∀ f ∈ pre, Passes e f) :
    ∃ acts, BoundaryRun P (Step P) e k pre L st acts tr Lend stEnd ∧
      acts = planWith pre L ∧ Performed P acts st tr stEnd := by
  obtain ⟨n, hn⟩ := h.sized_inv
  obtain ⟨acts, ha⟩ := boundary_complete_inv hn rfl hp
  exact ⟨acts, ha, cleanup_runs_exact_plan ha⟩


/-- **`abort()` runs no cleanup.** From `abort()` the only step is to the trap, with the same
frames and state, and nothing follows: no deferred call runs and no event is emitted. -/
theorem abort_runs_no_cleanup {P : Program O} {site : SiteId} {L : AList Var O.Val}
    {st : O.Store} {k : List (Frame O)} {tr : List O.Event} {c : Config O}
    (h : Steps P ⟨.exec (.abort site), L, st, k⟩ tr c) :
    tr = [] ∧ (c = ⟨.exec (.abort site), L, st, k⟩ ∨ c = ⟨.trapped .abort site, L, st, k⟩) := by
  cases h with
  | refl => exact ⟨rfl, .inl rfl⟩
  | step hs hrest =>
    cases hs with
    | silent h =>
      simp only [step, Option.some.injEq, Out.next.injEq] at h
      subst h
      have := steps_from_trapped hrest
      exact ⟨by simpa [Label.events] using this.1, .inr this.2⟩
    | prim h _ _ => simp [step] at h

/-! ### Evaluation order and determinism -/

/-- The first argument that traps decides the trap, whatever follows it: once the arguments
before it evaluate, in whatever state they leave. -/
theorem evalArgs_first_trap (L : AList Var O.Val) (e : O.Expr) (kd : TrapKind)
    (es₂ : List O.Expr) :
    ∀ (es₁ : List O.Expr) (st st₁ : O.Store) (vs : List O.Val),
      evalArgs O L st es₁ = .ok (vs, st₁) → O.eval L st₁ e = .trap kd →
      evalArgs O L st (es₁ ++ e :: es₂) = .trap kd
  | [], st, st₁, _, h, he => by
    simp only [evalArgs, Res.ok.injEq, Prod.mk.injEq] at h
    obtain ⟨_, rfl⟩ := h
    simp [evalArgs, he, Res.bind]
  | e₁ :: es₁, st, st₁, vs, h, he => by
    show evalArgs O L st (e₁ :: (es₁ ++ e :: es₂)) = _
    cases h₁ : O.eval L st e₁ with
    | ok p =>
      obtain ⟨v₁, st₀⟩ := p
      cases h₂ : evalArgs O L st₀ es₁ with
      | ok q =>
        obtain ⟨vs₁, st₂⟩ := q
        have hst : st₂ = st₁ := by
          simp [evalArgs, h₁, h₂, Res.bind, Res.map] at h; exact h.2
        subst hst
        have := evalArgs_first_trap L e kd es₂ es₁ st₀ st₂ vs₁ h₂ he
        simp [evalArgs, h₁, this, Res.bind, Res.map]
      | trap _ => simp [evalArgs, h₁, h₂, Res.bind, Res.map] at h
      | stuck => simp [evalArgs, h₁, h₂, Res.bind, Res.map] at h
    | trap _ => simp [evalArgs, h₁, Res.bind] at h
    | stuck => simp [evalArgs, h₁, Res.bind] at h

/-- Helper (congruence). Function injectivity, not a contract law about machine rules. -/
theorem step_silent_deterministic_inv {P : Program O} {c c₁ c₂ : Config O}
    (h₁ : step P c = some (.next c₁)) (h₂ : step P c = some (.next c₂)) : c₁ = c₂ := by
  rw [h₁] at h₂; cases h₂; rfl

/-- Helper (inversion). The function-backed representation of `Step` separates machine steps
from answers. This is not an independent determinism contract law. -/
theorem step_deterministic_inv {P : Program O} {c c₁ c₂ : Config O} {l₁ l₂ : Label O}
    (h₁ : Step P c l₁ c₁) (h₂ : Step P c l₂ c₂) :
    (l₁ = l₂ ∧ c₁ = c₂) ∨ ∃ p vs site st k a₁ a₂, step P c = some (.request p vs site st k) ∧
      O.allows p vs st a₁ = true ∧ O.allows p vs st a₂ = true ∧
      l₁ = a₁.label ∧ l₂ = a₂.label ∧ k a₁ = some c₁ ∧ k a₂ = some c₂ := by
  cases h₁ with
  | silent h =>
    cases h₂ with
    | silent h' => exact .inl ⟨rfl, step_silent_deterministic_inv h h'⟩
    | prim h' _ _ => rw [h] at h'; cases h'
  | prim h ha hk =>
    cases h₂ with
    | silent h' => rw [h] at h'; cases h'
    | prim h' ha' hk' =>
      rw [h] at h'; cases h'
      exact .inr ⟨_, _, _, _, _, _, _, h, ha, ha', rfl, rfl, hk, hk'⟩

/-! ### Lifecycle refusal

No law assumes that `life` is total. `step_return_caller` and `return_reaches_caller`
now require successful leave and result binding; `exit_scope_runs_defers` and
`done_scope_runs_defers` require a successful drop. `Cleans` and `RunsEach` carry
successful drops, so their existing plan and unwinding laws keep the same meaning.
`steps_request` requires successful continuation, and the inversion helper `step_deterministic_inv` records
those successful continuations explicitly. All other laws retain their premises.
-/

/-- No labeled transition is available. This is about the operational relation, including
primitive answers, rather than the optional result of the implementation. -/
def Blocked (P : Program O) (c : Config O) : Prop := ∀ l c', ¬ Step P c l c'

/-- Helper (inversion). A missing implementation result has no relational transition. -/
private theorem no_step_of_none_inv {P : Program O} {c : Config O} (h : step P c = none) :
    Blocked P c := by
  intro l c' hs
  cases hs with
  | silent hs => rw [h] at hs; cases hs
  | prim hs _ _ => rw [h] at hs; cases hs

/-- Helper (inversion). For a silent lifecycle operation, absence of all transitions is exactly
refusal. The operation's configuration transformer is unrestricted. -/
private theorem blocked_map_inv {P : Program O} {c : Config O} (r : Option O.Store)
    (f : O.Store → Config O) (h : step P c = r.map (fun st => .next (f st))) :
    Blocked P c ↔ r = none := by
  cases hr : r with
  | none => exact ⟨fun _ => rfl, fun _ => no_step_of_none_inv (by simpa [hr] using h)⟩
  | some st =>
    constructor
    · intro hb
      exact False.elim (hb .silent (f st) (.silent (by simpa [hr] using h)))
    · intro hn; cases hn

/-- Initialization is the sole obstruction after successful operand evaluation: refusal is
necessary and sufficient for the absence of every labeled transition. -/
theorem bind_refused (P : Program O) (x : Var) (e : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st st' : O.Store) (v : O.Val) (k : List (Frame O))
    (he : O.eval L st e = .ok (v, st')) :
    Blocked P ⟨.exec (.bind x e site), L, st, k⟩ ↔ O.life (.bind x) st' = none :=
  blocked_map_inv _ (fun st => ⟨.done, (x, v) :: L, st, k⟩) (by simp [step, onRes, he])

/-- For an existing destination and successful RHS, only lifecycle refusal blocks assignment. -/
theorem assign_refused (P : Program O) (x : Var) (e : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st st' : O.Store) (v old : O.Val) (k : List (Frame O))
    (he : O.eval L st e = .ok (v, st')) (hx : AList.lookup L x = some old) :
    Blocked P ⟨.exec (.assign x e site), L, st, k⟩ ↔ O.life (.bind x) st' = none :=
  blocked_map_inv _ (fun st => ⟨.done, AList.update L x (fun _ => v), st, k⟩)
    (by simp [step, onRes, he, hx])

/-- Helper (inversion). Initial entry projects the partial activation handler. Internal entry's
operational contract is `call_refused`; this helper is not a contract law. -/
theorem enter_refused_inv (fn : Fn O) (vs : List O.Val) (st : O.Store) :
    enter fn vs st = none ↔ O.life (.enter (fn.params.map Prod.fst)) st = none := by
  simp [enter]

/-- After successful arguments, lookup and arity, activation refusal is exactly what blocks a call. -/
theorem call_refused (P : Program O) (dst : Option Var) (f : FnId) (args : List O.Expr)
    (site : SiteId) (L : AList Var O.Val) (st st' : O.Store) (vs : List O.Val)
    (fn : Fn O) (k : List (Frame O)) (he : evalArgs O L st args = .ok (vs, st'))
    (hf : AList.lookup P.fns f = some fn) (hlen : fn.params.length = vs.length) :
    Blocked P ⟨.exec (.call dst f args site), L, st, k⟩ ↔
      O.life (.enter (fn.params.map Prod.fst)) st' = none :=
  blocked_map_inv _ (fun st => ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, st,
    .scope fn.block (fn.params.map Prod.fst) [] :: .ret dst L :: k⟩)
    (by simp [step, onRes, he, hf, hlen])

/-- A normal scope end with no remaining calls is blocked exactly when dropping its locals refuses. -/
theorem scope_drop_refused (P : Program O) (b : BlockId) (dm : List Var)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    Blocked P ⟨.done, L, st, .scope b dm [] :: k⟩ ↔ dropOut O dm L st = none :=
  blocked_map_inv _ _ rfl

/-- A structured exit through an empty scope is blocked exactly when its drop refuses. -/
theorem exit_drop_refused (P : Program O) (e : Exit O.Val) (b : BlockId) (dm : List Var)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    Blocked P ⟨.exiting e, L, st, .scope b dm [] :: k⟩ ↔ dropOut O dm L st = none :=
  blocked_map_inv _ _ rfl

/-- The test-frame drop during an exit is also necessary and sufficient for progress. -/
theorem exit_test_drop_refused (P : Program O) (e : Exit O.Val) (dm : List Var)
    (l : LoopId) (cr body : Stmt O) (t : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    Blocked P ⟨.exiting e, L, st, .test l cr t body site dm :: k⟩ ↔ dropOut O dm L st = none :=
  blocked_map_inv _ _ rfl

/-- After a successful boolean test, either branch progresses exactly when its temporary drop
succeeds. Non-boolean operands cannot masquerade as lifecycle refusals. -/
theorem test_drop_refused (P : Program O) (l : LoopId) (cr body : Stmt O) (t : O.Expr)
    (site : SiteId) (dm : List Var) (L : AList Var O.Val) (st st' : O.Store)
    (v : O.Val) (go : Bool) (k : List (Frame O)) (he : O.eval L st t = .ok (v, st'))
    (ht : O.test v = some go) :
    Blocked P ⟨.done, L, st, .test l cr t body site dm :: k⟩ ↔ dropOut O dm L st' = none := by
  apply blocked_map_inv _ (fun st => if go then
    ⟨.exec body, restrict dm L, st, .body l cr t body site :: k⟩ else
    ⟨.done, restrict dm L, st, k⟩)
  cases go <;> simp [step, onRes, he, ht]

/-- Explicit returns are blocked exactly by refusal of activation end or result initialization.
The analogous implicit Unit return has the same obstruction. -/
theorem return_refused (P : Program O) (dst : Option Var) (v : O.Val)
    (caller L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    (Blocked P ⟨.exiting (.ret v), L, st, .ret dst caller :: k⟩ ↔
      (O.life .leave st).bind (bindLife O dst) = none) ∧
    (Blocked P ⟨.done, L, st, .ret dst caller :: k⟩ ↔
      (O.life .leave st).bind (bindLife O dst) = none) := by
  constructor
  · apply blocked_map_inv _ (fun st => ⟨.done, bindDst dst v caller, st, k⟩)
    cases hl : O.life .leave st <;> simp [step, stepExit, hl]
  · apply blocked_map_inv _ (fun st => ⟨.done, bindDst dst O.unitVal caller, st, k⟩)
    cases hl : O.life .leave st <;> simp [step, hl]

/-- The selected allowed answer cannot be committed when its continuation refuses: the driver
ends stuck at the request, preserving the entire history (including no response event). This is
per answer: another allowed answer may still have a transition. -/
theorem continuation_refused (P : Program O) (ρ : Oracle O) (n : Nat)
    (hist : List O.Event) (c : Config O) (p : O.Prim) (vs : List O.Val)
    (site : SiteId) (st : O.Store) (cont : Answer O.Val O.Event O.Store → Option (Config O))
    (a : Answer O.Val O.Event O.Store) (hf : c.final? = none)
    (hr : step P c = some (.request p vs site st cont))
    (ho : ρ hist p vs site st = some a) (ha : O.allows p vs st a = true)
    (hk : cont a = none) : runFrom P ρ (n + 1) hist c = ⟨hist, .stuck c⟩ := by
  simp [runFrom, hf, hr, ho, ha, hk]

/-- A successful primitive answer whose binding refuses stops the actual run at its requesting
configuration, without accepting its event. All argument evaluation and answer validation succeed. -/
theorem prim_bind_refused (P : Program O) (ρ : Oracle O) (n : Nat) (hist : List O.Event)
    (x : Var) (p : O.Prim) (args : List O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st st' : O.Store) (vs : List O.Val) (k : List (Frame O))
    (a : Answer O.Val O.Event O.Store) (v : O.Val)
    (he : evalArgs O L st args = .ok (vs, st'))
    (ho : ρ hist p vs site st' = some a) (ha : O.allows p vs st' a = true)
    (hv : a.result = .ok v) (hl : O.life (.bind x) a.store = none) :
    runFrom P ρ (n + 1) hist ⟨.exec (.prim (some x) p args site), L, st, k⟩ =
      ⟨hist, .stuck ⟨.exec (.prim (some x) p args site), L, st, k⟩⟩ := by
  exact continuation_refused P ρ n hist _ p vs site st' _ a rfl
    (by simp [step, onRes, he]; rfl) ho ha (by simp [hv, bindLife, hl])

/-- Error answers bypass result initialization even when that initialization would refuse:
the actual labeled transition traps, retaining the answer's store and event. -/
theorem prim_error_bypasses_bind (P : Program O) (x : Var) (p : O.Prim) (args : List O.Expr)
    (site : SiteId) (L : AList Var O.Val) (st st' : O.Store) (vs : List O.Val)
    (k : List (Frame O)) (a : Answer O.Val O.Event O.Store) (kd : TrapKind)
    (he : evalArgs O L st args = .ok (vs, st')) (ha : O.allows p vs st' a = true)
    (hv : a.result = .error kd) :
    Step P ⟨.exec (.prim (some x) p args site), L, st, k⟩ a.label
      ⟨.trapped kd site, L, a.store, k⟩ := by
  apply Step.prim (a := a) (p := p) (vs := vs) (site := site) (st := st')
    (k := fun a => match a.result with
      | .ok v => (bindLife O (some x) a.store).map fun st => ⟨.done, bindDst (some x) v L, st, k⟩
      | .error kd => some ⟨.trapped kd site, L, a.store, k⟩)
    (by simp [step, onRes, he]; rfl) ha
  simp [hv]

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

/-- Helper (projection). Extracts the protocol admission obligation, not a contract law.
The concrete demo protocol below discharges that obligation by type cases. -/
theorem destroy_not_copy_proj {P : Program O} (D : DestroyProtocol P) (T : O.Ty) (f : FnId)
    (h : D.impl T = some f) : O.isCopy T = false := D.notCopy T f h

/-- A sequence's element `i` is visited at position `i`: in increasing index order. -/
theorem seqDestroyOrder_elem (n i : Nat) (h : i < n) : (seqDestroyOrder n)[i]? = some (.elem i) := by
  rw [seqDestroyOrder, List.getElem?_append_left (by simpa using h)]
  simp [h]

/-- A sequence's backing is released last, after all `n` elements. -/
theorem seqDestroyOrder_storage (n : Nat) :
    (seqDestroyOrder n).length = n + 1 ∧ (seqDestroyOrder n)[n]? = some .storage := by
  simp [seqDestroyOrder]

/-- The entries of a map, in visiting order. -/
private def entryParts (n : Nat) : List Part := (List.range n).flatMap fun i => [.key i, .value i]

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem entryParts_succ (n : Nat) :
    entryParts (n + 1) = entryParts n ++ [.key n, .value n] := by
  simp [entryParts, List.range_succ, List.flatMap_append]

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem entryParts_length : ∀ n, (entryParts n).length = 2 * n
  | 0 => rfl
  | n + 1 => by rw [entryParts_succ]; simp [entryParts_length n]; omega

/-- Helper (inversion). Decomposes a raw execution using the machine stack rules. -/
private theorem entryParts_get : ∀ (n i : Nat), i < n →
    (entryParts n)[2 * i]? = some (.key i) ∧ (entryParts n)[2 * i + 1]? = some (.value i)
  | 0, _, h => absurd h (Nat.not_lt_zero _)
  | n + 1, i, h => by
    rw [entryParts_succ]
    by_cases hi : i < n
    · have hl := entryParts_length n
      rw [List.getElem?_append_left (by omega), List.getElem?_append_left (by omega)]
      exact entryParts_get n i hi
    · have hin : i = n := by omega
      subst hin
      have hl := entryParts_length i
      rw [List.getElem?_append_right (by omega), List.getElem?_append_right (by omega), hl]
      simp

/-- A map's `i`-th entry (in ascending key order) is visited key first, at position `2i`, then
value, at `2i+1`. -/
theorem mapDestroyOrder_entry (n i : Nat) (h : i < n) :
    (mapDestroyOrder n)[2 * i]? = some (.key i) ∧
      (mapDestroyOrder n)[2 * i + 1]? = some (.value i) := by
  have hl := entryParts_length n
  have := entryParts_get n i h
  show (entryParts n ++ [Part.storage])[2 * i]? = _ ∧
    (entryParts n ++ [Part.storage])[2 * i + 1]? = _
  rw [List.getElem?_append_left (by omega), List.getElem?_append_left (by omega)]
  exact this

/-- A map's nodes are released last, after all `n` entries. -/
theorem mapDestroyOrder_storage (n : Nat) :
    (mapDestroyOrder n).length = 2 * n + 1 ∧ (mapDestroyOrder n)[2 * n]? = some .storage := by
  have hl := entryParts_length n
  show (entryParts n ++ [Part.storage]).length = _ ∧ (entryParts n ++ [Part.storage])[2 * n]? = _
  rw [List.getElem?_append_right (by omega), hl]
  simp [hl]

/-! ### Non-vacuity

The cleanup laws assume that each deferred call completes (`Completes`). Real calls meet it: in the
demo language of `Spec/Exec.lean`, a deferred `log(1)` completes from any locals, in any state and
on any frames, emitting `1` (its parameter is a fresh local of a new activation); a deferred
`bump()` completes silently and adds one to the counter. -/

theorem demo_log_completes (L : AList Var Demo.ops.Val) (st : Demo.ops.Store) :
    Completes Demo.prog L ⟨Demo.logF, [.lit 1], Demo.s⟩ st [1] st := by
  intro k
  refine steps_next rfl (steps_request (O := Demo.ops) (P := Demo.prog)
    (a := ⟨some (1 : Int), .ok (0 : Int), Demo.DState.life (.enter [Demo.x]) st⟩) (tr := []) rfl
    (by simp [Demo.ops, Demo.logFn]) rfl ?_)
  exact steps_next rfl (steps_next rfl (Steps.refl _))

theorem demo_bump_completes (L : AList Var Demo.ops.Val) (st : Demo.ops.Store) :
    Completes Demo.prog L ⟨Demo.bumpF, [], Demo.s⟩ st [] { st with counter := st.counter + 1 } := by
  intro k
  refine steps_next rfl (steps_request (O := Demo.ops) (P := Demo.prog)
    (a := ⟨none, .ok (0 : Int),
      { Demo.DState.life (.enter []) st with counter := st.counter + 1 }⟩) (tr := []) rfl
    (by simp [Demo.ops, Demo.DState.life]) rfl ?_)
  exact steps_next rfl (steps_next rfl (Steps.refl _))

/-- So the cleanup law applies to a real scope: the end of a block that registered `log(1)` and
then `bump()` runs `bump` (silently, adding one) and then `log`, from any state. -/
example (L : AList Var Demo.ops.Val) (st : Demo.DState) (k : List (Frame Demo.ops))
    (dm : List Var) :
    Steps Demo.prog ⟨.done, L, st, .scope ⟨1⟩ dm [⟨Demo.bumpF, [], Demo.s⟩,
      ⟨Demo.logF, [.lit 1], Demo.s⟩] :: k⟩ [1]
      ⟨.done, restrict dm L, Demo.DState.life (.drop (dead dm L)) { st with counter := st.counter + 1 }, k⟩ :=
  done_scope_runs_defers Demo.prog ⟨1⟩ dm L k
    (.cons (demo_bump_completes L st) (.cons (demo_log_completes L _) (.nil _))) rfl

/-! ### Cleanup evidence and planted dispatcher mutations -/
namespace CleanupEvidence
open Demo

/-- Representation lemma. A concrete first-return segment for an ordinary deferred call. This certificate is also used
inside the planted dispatchers: the mutation changes only selection, not the call's behavior. -/
private theorem logSegment (n : Int) (e : Exit Int) (L : AList Var Int)
    (rest : List (Frame ops)) :
    FirstSteps (Step prog) (fun c => c.ctrl = .done ∧ c.frames = .resume (.exit e) :: rest)
      ⟨.exec (.call none logF [.lit n] s), L, .init, .resume (.exit e) :: rest⟩ [n]
      ⟨.done, L, .init, .resume (.exit e) :: rest⟩ := by
  let q : List (Frame ops) := .resume (.exit e) :: rest
  let entered : DState := DState.life (.enter [x]) .init
  refine FirstSteps.step (R := Step prog) (c' := ⟨.exec logFn.body, [(x, n)], entered,
      .scope logFn.block [x] [] :: .ret none L :: q⟩)
    (by intro h; cases h.1) (.silent rfl) ?_
  refine FirstSteps.step (R := Step prog) (c' := ⟨.done, [(x, n)], entered,
      .scope logFn.block [x] [] :: .ret none L :: q⟩)
    (by intro h; cases h.1)
    (Step.prim (P := prog) (p := .log) (vs := [n]) (site := s) (st := entered) (a := ⟨some n, .ok 0, entered⟩) rfl (by simp [ops]) rfl) ?_
  refine FirstSteps.step (R := Step prog) (c' := ⟨.done, [(x, n)], entered, .ret none L :: q⟩)
    (by intro h; cases h.2) (.silent rfl) ?_
  refine FirstSteps.step (R := Step prog) (c' := ⟨.done, L, .init, q⟩)
    (by intro h; cases h.2) (.silent rfl) ?_
  exact .stop ⟨rfl, rfl⟩

private def d (n : Int) : Deferred ops := ⟨logF, [.lit n], s⟩
private def inner : Frame ops := .scope ⟨1⟩ [] [d 1]
private def outer : Frame ops := .scope ⟨2⟩ [] [d 2]
private def actions : List (CleanAct ops) :=
  [.call [] (d 1), .drop [] [], .call [] (d 2), .drop [] []]

/-- Instance (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`). Two real calls on nested
scopes emit `[1,2]`, with each scope drop between calls. The exit's value is carried to the
boundary without executing anything below it. -/
example : BoundaryRun prog (Step prog) (.ret 7) [] [inner, outer] [] .init actions [1, 2] [] .init ∧
    actions = planWith [inner, outer] [] ∧ Performed prog actions .init [1, 2] .init := by
  have h : BoundaryRun prog (Step prog) (.ret 7) [] [inner, outer] [] .init actions [1, 2] [] .init := by
    refine BoundaryRun.call (P := prog) (R := Step prog) (pre := [inner, outer])
      (d := d 1) (rest := [.scope ⟨1⟩ [] [], outer])
      (by intro h; cases h) (.silent rfl)
      (logSegment 1 (.ret 7) [] [.scope ⟨1⟩ [] [], outer]) (.silent rfl) ?_
    refine BoundaryRun.pass (pre' := [outer]) (by intro h; cases h) (.silent rfl) ?_
    refine BoundaryRun.call (P := prog) (R := Step prog) (pre := [outer])
      (d := d 2) (rest := [.scope ⟨2⟩ [] []]) (by intro h; cases h) (.silent rfl)
      (logSegment 2 (.ret 7) [] [.scope ⟨2⟩ [] []]) (.silent rfl) ?_
    exact BoundaryRun.pass (pre' := []) (by intro h; cases h) (.silent rfl) .stop
  exact ⟨h, cleanup_runs_exact_plan h⟩

/-- A planted dispatcher deleting a scope's pending call (and performing its drop). -/
private inductive Missing : Config ops → Label ops → Config ops → Prop where
  | skip : Missing ⟨.exiting (.ret 7), [], .init, [inner]⟩ .silent
      ⟨.exiting (.ret 7), [], .init, []⟩

/-- Planted (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`). A mutated rule deletes a
registered call. The observer admits the actual mutated execution but its actions disagree with
the independent plan; the law is therefore false for this rule. -/
example : ¬ (∀ acts tr L st, BoundaryRun prog Missing (.ret 7) [] [inner] [] .init acts tr L st →
    acts = planWith [inner] [] ∧ Performed prog acts .init tr st) := by
  intro h
  have hr : BoundaryRun prog Missing (.ret 7) [] [inner] [] .init [] [] [] .init :=
    BoundaryRun.pass (pre' := []) (by intro hn; cases hn) .skip .stop
  have he := (h _ _ _ _ hr).1
  change ([] : List (CleanAct ops)) = [.call [] (d 1), .drop [] []] at he
  cases he

/-- A planted dispatcher inserting an unregistered call at a scope with no registrations. -/
private inductive Extra : Config ops → Label ops → Config ops → Prop where
  | inject : Extra ⟨.exiting (.ret 7), [], .init, [.scope ⟨1⟩ [] []]⟩ .silent
      ⟨.exec (d 9).stmt, [], .init, [.resume (.exit (.ret 7)), .scope ⟨1⟩ [] []]⟩
  | resume : Extra ⟨.done, [], .init, [.resume (.exit (.ret 7)), .scope ⟨1⟩ [] []]⟩ .silent
      ⟨.exiting (.ret 7), [], .init, [.scope ⟨1⟩ [] []]⟩
  | drop : Extra ⟨.exiting (.ret 7), [], .init, [.scope ⟨1⟩ [] []]⟩ .silent
      ⟨.exiting (.ret 7), [], .init, []⟩

/-- Planted (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`). A mutated rule adds a real
`log(9)` not registered in the scope. Its first-return execution emits `9`, and the law rejects
the extra observed call. -/
example : ¬ (∀ acts tr L st, BoundaryRun prog Extra (.ret 7) [] [.scope ⟨1⟩ [] []]
    [] .init acts tr L st → acts = planWith [.scope ⟨1⟩ [] []] [] ∧ Performed prog acts .init tr st) := by
  intro h
  have hr : BoundaryRun prog Extra (.ret 7) [] [.scope ⟨1⟩ [] []] [] .init
      [.call [] (d 9), .drop [] []] [9] [] .init :=
    BoundaryRun.call (P := prog) (R := Extra) (pre := [.scope ⟨1⟩ [] []])
      (d := d 9) (rest := [.scope ⟨1⟩ [] []]) (by intro hn; cases hn) Extra.inject (logSegment 9 (.ret 7) [] [.scope ⟨1⟩ [] []])
      Extra.resume (BoundaryRun.pass (pre' := []) (by intro hn; cases hn) Extra.drop .stop)
  have he := (h _ _ _ _ hr).1
  change ([.call [] (d 9), .drop [] []] : List (CleanAct ops)) = [.drop [] []] at he
  cases he

/-- A planted dispatcher that forgets the lifecycle effect while popping a scope. -/
private inductive MissingDrop : Config ops → Label ops → Config ops → Prop where
  | skip : MissingDrop ⟨.exiting (.ret 7), [(x, 1)], ⟨0, [[x]]⟩, [.scope ⟨1⟩ [] []]⟩ .silent
      ⟨.exiting (.ret 7), [], ⟨0, [[x]]⟩, []⟩

/-- Planted (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`). Popping the scope but
forgetting its lifecycle update leaves `x` marked moved. Although the action list and event
trace look right, independently executing the drop cannot produce this final store. -/
example : ¬ (∀ acts tr L st, BoundaryRun prog MissingDrop (.ret 7) [] [.scope ⟨1⟩ [] []]
    [(x, 1)] ⟨0, [[x]]⟩ acts tr L st →
      acts = planWith (O := ops) [.scope ⟨1⟩ [] []] [(x, 1)] ∧ Performed prog acts ⟨0, [[x]]⟩ tr st) := by
  intro h
  have hr : BoundaryRun prog MissingDrop (.ret 7) [] [.scope ⟨1⟩ [] []] [(x, 1)] ⟨0, [[x]]⟩
      [.drop [] [(x, 1)]] [] [] ⟨0, [[x]]⟩ :=
    BoundaryRun.pass (pre' := []) (by intro hn; cases hn) .skip .stop
  have hp := (h _ _ _ _ hr).2
  cases hp with
  | drop hd hr =>
    cases hr
    have he : (⟨0, [[]]⟩ : DState) = ⟨0, [[x]]⟩ := Option.some.inj hd
    have hm := congrArg DState.moved he
    cases hm

/-- Representation lemma. Extend a raw run with a concrete deferred log's complete execution.
The outer boundary remains untouched throughout the call; only the supplied tail reaches it. -/
private theorem logContinue_inv (n : Int) (e : Exit Int) (L : AList Var Int)
    (rest : List (Frame ops)) {tr : List Int} {endC : Config ops}
    (tail : FirstSteps (Step prog) (fun c => c.frames.length ≤ 0)
      ⟨.done, L, .init, .resume (.exit e) :: rest⟩ tr endC) :
    FirstSteps (Step prog) (fun c => c.frames.length ≤ 0)
      ⟨.exec (.call none logF [.lit n] s), L, .init, .resume (.exit e) :: rest⟩ (n :: tr) endC := by
  let q : List (Frame ops) := .resume (.exit e) :: rest
  let entered : DState := DState.life (.enter [x]) .init
  refine FirstSteps.step (R := Step prog) (c' := ⟨.exec logFn.body, [(x, n)], entered,
      .scope logFn.block [x] [] :: .ret none L :: q⟩)
    (by simp) (.silent rfl) ?_
  refine FirstSteps.step (R := Step prog) (c' := ⟨.done, [(x, n)], entered,
      .scope logFn.block [x] [] :: .ret none L :: q⟩)
    (by simp [q])
    (Step.prim (P := prog) (p := .log) (vs := [n]) (site := s) (st := entered)
      (a := ⟨some n, .ok 0, entered⟩) rfl (by simp [ops]) rfl) ?_
  refine FirstSteps.step (R := Step prog) (c' := ⟨.done, [(x, n)], entered, .ret none L :: q⟩)
    (by simp [q]) (.silent rfl) ?_
  simpa [Label.events] using FirstSteps.step (R := Step prog) (by simp [q])
    (Step.silent (P := prog) (c' := ⟨.done, L, .init, q⟩) rfl) tail

/-- Instance (law `ConcreteSpec.Core.Exec.raw_cleanup_runs_plan`). The premise is a raw
first-arrival execution, with no action list or call segmentation supplied: a registered `log(1)`
runs and its scope drops before the exit first reaches the empty boundary. -/
example : ∃ acts, BoundaryRun prog (Step prog) (.ret 7) [] [inner] [] .init acts [1] [] .init ∧
    acts = planWith [inner] [] ∧ Performed prog acts .init [1] .init := by
  have h : FirstSteps (Step prog) (fun c => c.frames.length ≤ 0)
      ⟨.exiting (.ret 7), [], .init, [inner]⟩ [1] ⟨.exiting (.ret 7), [], .init, []⟩ := by
    refine FirstSteps.step (R := Step prog)
      (c' := ⟨.exec (d 1).stmt, [], .init, [.resume (.exit (.ret 7)), .scope ⟨1⟩ [] []]⟩)
      (by simp [inner]) (.silent rfl) ?_
    apply logContinue_inv 1 (.ret 7) [] [.scope ⟨1⟩ [] []]
    refine FirstSteps.step (R := Step prog)
      (c' := ⟨.exiting (.ret 7), [], .init, [.scope ⟨1⟩ [] []]⟩) (by simp) (.silent rfl) ?_
    refine FirstSteps.step (R := Step prog)
      (c' := ⟨.exiting (.ret 7), [], .init, []⟩) (by simp) (.silent rfl) ?_
    exact .stop (by simp)
  exact raw_cleanup_runs_plan h (by intro f hf; cases hf with | head => trivial | tail _ hf => cases hf)

/-- A different missing-call mutation: it jumps through a normal resume instead of dispatching
a call. This path has the same boundary as a real unwind, but an unexpected intermediate control. -/
private inductive Escape : Config ops → Label ops → Config ops → Prop where
  | skip : Escape ⟨.exiting (.ret 7), [], .init, [inner]⟩ .silent
      ⟨.done, [], .init, [.resume (.exit (.ret 7))]⟩
  | resume : Escape ⟨.done, [], .init, [.resume (.exit (.ret 7))]⟩ .silent
      ⟨.exiting (.ret 7), [], .init, []⟩

/-- Planted (law `ConcreteSpec.Core.Exec.raw_cleanup_runs_plan`). A raw first-boundary path
which skips the call via `.done`/`.resume` cannot escape the contract: it falsifies the extraction
obligation itself, even though the earlier observer-only law would have excluded this path. -/
example : ¬ (∀ tr L st, FirstSteps Escape (fun c => c.frames.length ≤ 0)
    ⟨.exiting (.ret 7), [], .init, [inner]⟩ tr ⟨.exiting (.ret 7), L, st, []⟩ →
      ∃ acts, BoundaryRun prog Escape (.ret 7) [] [inner] [] .init acts tr L st ∧
        acts = planWith [inner] [] ∧ Performed prog acts .init tr st) := by
  intro h
  have raw : FirstSteps Escape (fun c => c.frames.length ≤ 0)
      ⟨.exiting (.ret 7), [], .init, [inner]⟩ [] ⟨.exiting (.ret 7), [], .init, []⟩ :=
    FirstSteps.step (by simp [inner]) Escape.skip
      (FirstSteps.step (by simp) Escape.resume (.stop (by simp)))
  obtain ⟨_, observed, _⟩ := h [] [] .init raw
  generalize htr : ([] : List Int) = trace at observed
  cases observed with
  | pass _ hs _ => cases hs
  | call _ hs _ _ _ => cases hs

end CleanupEvidence

/-! ### Lifecycle evidence and planted refusal bypasses

`Refusal.ops` refuses every lifecycle event. Each configuration below reaches exactly one
lifecycle callback: operands, destination lookup, callee lookup, arity, and boolean testing all
succeed first. `Bypass` plants transitions which ignore the refused callback on the same operand language.
The production and mutant therefore share all predecessor-success and refusal hypotheses.
-/
namespace Refusal
open Demo

abbrev ops : Operands := { Demo.ops with life := fun _ _ => none }
def fn : Fn ops := ⟨[], .unit, ⟨0⟩, .skip⟩
def prog : Program ops := ⟨[(bumpF, fn)]⟩

/-- Planted outer rules, on the same refusing operand language. Each rule performs the
otherwise valid local/control update while ignoring the refused lifecycle callback. -/
private inductive Bypass : Config ops → Label ops → Config ops → Prop where
  | bind : Bypass ⟨.exec (.bind x (.lit 1) s), [], .init, []⟩ .silent
      ⟨.done, [(x, 1)], .init, []⟩
  | assign : Bypass ⟨.exec (.assign x (.lit 1) s), [(x, 0)], .init, []⟩ .silent
      ⟨.done, [(x, 1)], .init, []⟩
  | enter : Bypass ⟨.exec (.call none bumpF [] s), [], .init, []⟩ .silent
      ⟨.exec .skip, [], .init, [.scope ⟨0⟩ [] [], .ret none []]⟩
  | scope : Bypass ⟨.done, [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ .silent
      ⟨.done, [], .init, []⟩
  | exitScope : Bypass ⟨.exiting (.ret 7), [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ .silent
      ⟨.exiting (.ret 7), [], .init, []⟩
  | exitTest : Bypass ⟨.exiting (.ret 7), [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ .silent
      ⟨.exiting (.ret 7), [], .init, []⟩
  | test : Bypass ⟨.done, [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ .silent
      ⟨.exec .skip, [], .init, [.body ⟨0⟩ .skip (.lit 1) .skip s]⟩
  | ret : Bypass ⟨.exiting (.ret 7), [], .init, [.ret (some x) [(y, 2)]]⟩ .silent
      ⟨.done, [(x, 7), (y, 2)], .init, []⟩

private inductive BypassImplicit : Config ops → Label ops → Config ops → Prop where
  | ret : BypassImplicit ⟨.done, [], .init, [.ret none []]⟩ .silent ⟨.done, [], .init, []⟩

/-- Instance (law `ConcreteSpec.Core.Exec.bind_refused`). Only the lifecycle callback refuses. -/
example : Blocked prog ⟨.exec (.bind x (.lit 1) s), [], .init, []⟩ :=
  (bind_refused prog x (.lit 1) s [] .init .init 1 [] rfl).2 rfl

/-- Planted (law `ConcreteSpec.Core.Exec.bind_refused`). The mutated transition ignores the
refusal while preserving the preceding successful checks, in the very same operand language. -/
example : ¬ ((∀ l c', ¬ Bypass ⟨.exec (.bind x (.lit 1) s), [], .init, []⟩ l c') ↔
    ops.life (.bind x) DState.init = none) := by
  intro h
  exact h.mpr rfl .silent _ .bind

/-- Instance (law `ConcreteSpec.Core.Exec.assign_refused`). Only the lifecycle callback refuses. -/
example : Blocked prog ⟨.exec (.assign x (.lit 1) s), [(x, 0)], .init, []⟩ :=
  (assign_refused prog x (.lit 1) s [(x, 0)] .init .init 1 0 [] rfl rfl).2 rfl

/-- Planted (law `ConcreteSpec.Core.Exec.assign_refused`). The mutated transition ignores the
refusal while preserving the preceding successful checks, in the very same operand language. -/
example : ¬ ((∀ l c', ¬ Bypass ⟨.exec (.assign x (.lit 1) s), [(x, 0)], .init, []⟩ l c') ↔
    ops.life (.bind x) DState.init = none) := by
  intro h
  exact h.mpr rfl .silent _ .assign

/-- Instance (law `ConcreteSpec.Core.Exec.call_refused`). Only the lifecycle callback refuses. -/
example : Blocked prog ⟨.exec (.call none bumpF [] s), [], .init, []⟩ :=
  (call_refused prog none bumpF [] s [] .init .init [] fn [] rfl rfl rfl).2 rfl

/-- Planted (law `ConcreteSpec.Core.Exec.call_refused`). The mutated transition ignores the
refusal while preserving the preceding successful checks, in the very same operand language. -/
example : ¬ ((∀ l c', ¬ Bypass ⟨.exec (.call none bumpF [] s), [], .init, []⟩ l c') ↔
    ops.life (.enter []) DState.init = none) := by
  intro h
  exact h.mpr rfl .silent _ .enter

/-- Instance (law `ConcreteSpec.Core.Exec.scope_drop_refused`). Only the lifecycle callback refuses. -/
example : Blocked prog ⟨.done, [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ :=
  (scope_drop_refused prog ⟨0⟩ [] [(x, 1)] .init []).2 rfl

/-- Planted (law `ConcreteSpec.Core.Exec.scope_drop_refused`). The mutated transition ignores the
refusal while preserving the preceding successful checks, in the very same operand language. -/
example : ¬ ((∀ l c', ¬ Bypass ⟨.done, [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ l c') ↔
    dropOut ops [] [(x, 1)] DState.init = none) := by
  intro h
  exact h.mpr rfl .silent _ .scope

/-- Instance (law `ConcreteSpec.Core.Exec.exit_drop_refused`). Only the lifecycle callback refuses. -/
example : Blocked prog ⟨.exiting (.ret 7), [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ :=
  (exit_drop_refused prog (.ret 7) ⟨0⟩ [] [(x, 1)] .init []).2 rfl

/-- Planted (law `ConcreteSpec.Core.Exec.exit_drop_refused`). The mutated transition ignores the
refusal while preserving the preceding successful checks, in the very same operand language. -/
example : ¬ ((∀ l c', ¬ Bypass ⟨.exiting (.ret 7), [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ l c') ↔
    dropOut ops [] [(x, 1)] DState.init = none) := by
  intro h
  exact h.mpr rfl .silent _ .exitScope

/-- Instance (law `ConcreteSpec.Core.Exec.exit_test_drop_refused`). Only the lifecycle callback refuses. -/
example : Blocked prog ⟨.exiting (.ret 7), [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ :=
  (exit_test_drop_refused prog (.ret 7) [] ⟨0⟩ .skip .skip (.lit 1) s [(x, 1)] .init []).2 rfl

/-- Planted (law `ConcreteSpec.Core.Exec.exit_test_drop_refused`). The mutated transition ignores the
refusal while preserving the preceding successful checks, in the very same operand language. -/
example : ¬ ((∀ l c', ¬ Bypass ⟨.exiting (.ret 7), [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ l c') ↔
    dropOut ops [] [(x, 1)] DState.init = none) := by
  intro h
  exact h.mpr rfl .silent _ .exitTest

/-- Instance (law `ConcreteSpec.Core.Exec.test_drop_refused`). Only the lifecycle callback refuses. -/
example : Blocked prog ⟨.done, [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ :=
  (test_drop_refused prog ⟨0⟩ .skip .skip (.lit 1) s [] [(x, 1)] .init .init 1 true [] rfl rfl).2 rfl

/-- Planted (law `ConcreteSpec.Core.Exec.test_drop_refused`). The mutated transition ignores the
refusal while preserving the preceding successful checks, in the very same operand language. -/
example : ¬ ((∀ l c', ¬ Bypass ⟨.done, [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ l c') ↔
    dropOut ops [] [(x, 1)] DState.init = none) := by
  intro h
  exact h.mpr rfl .silent _ .test

/-- Instance (law `ConcreteSpec.Core.Exec.return_refused`). Only the lifecycle callback refuses. -/
example : Blocked prog ⟨.exiting (.ret 7), [], .init, [.ret (some x) [(y, 2)]]⟩ :=
  (return_refused prog (some x) 7 [(y, 2)] [] .init []).1.2 rfl

/-- Planted (law `ConcreteSpec.Core.Exec.return_refused`). The mutated transition ignores the
refusal while preserving the preceding successful checks, in the very same operand language. -/
example : ¬ ((∀ l c', ¬ Bypass ⟨.exiting (.ret 7), [], .init, [.ret (some x) [(y, 2)]]⟩ l c') ↔
    (ops.life .leave DState.init).bind (bindLife ops (some x)) = none) := by
  intro h
  exact h.mpr rfl .silent _ .ret

/-- Instance (law `ConcreteSpec.Core.Exec.return_refused`). Implicit Unit return also refuses leave. -/
example : Blocked prog ⟨.done, [], .init, [.ret none []]⟩ :=
  (return_refused prog none 7 [] [] .init []).2.2 rfl

/-- Planted (law `ConcreteSpec.Core.Exec.return_refused`). The implicit-return mutation
ignores refused leave in the same operand language and falsifies the second refusal iff. -/
example : ¬ ((∀ l c', ¬ BypassImplicit ⟨.done, [], .init, [.ret none []]⟩ l c') ↔
    (ops.life .leave DState.init).bind (bindLife ops none) = none) := by
  intro h
  exact h.mpr rfl .silent _ .ret

/-- Only result initialization refuses; activation leave succeeds independently. -/
private abbrev bindOnlyOps : Operands := { Demo.ops with life := fun e st => match e with
    | .bind _ => none
    | _ => some (DState.life e st) }

/-- Instance (law `ConcreteSpec.Core.Exec.return_refused`). The law itself proves blockage
when leave succeeds and result initialization alone refuses. -/
example : bindOnlyOps.life .leave DState.init = some DState.init ∧
    Blocked (O := bindOnlyOps) ⟨[]⟩ ⟨.exiting (.ret 7), [], .init, [.ret (some x) []]⟩ :=
  ⟨rfl, (return_refused (O := bindOnlyOps) ⟨[]⟩ (some x) 7 [] [] .init []).1.2 rfl⟩

private inductive BypassResultBind : Config bindOnlyOps → Label bindOnlyOps → Config bindOnlyOps → Prop where
  | ret : BypassResultBind ⟨.exiting (.ret 7), [], .init, [.ret (some x) []]⟩ .silent
      ⟨.done, [(x, 7)], .init, []⟩

/-- Planted (law `ConcreteSpec.Core.Exec.return_refused`). Successful leave does not permit
bypassing the separately refused result bind; the mutated return violates the explicit-return iff. -/
example : ¬ ((∀ l c', ¬ BypassResultBind
    ⟨.exiting (.ret 7), [], .init, [.ret (some x) []]⟩ l c') ↔
      (bindOnlyOps.life .leave DState.init).bind (bindLife bindOnlyOps (some x)) = none) := by
  intro h
  exact h.mpr rfl .silent _ .ret

/-- Instance (law `ConcreteSpec.Core.Exec.prim_bind_refused`). Instance (law `ConcreteSpec.Core.Exec.continuation_refused`). The allowed log answer would emit `1`, but only
its result initialization refuses. A pre-existing history must survive unchanged. -/
example : runFrom prog Demo.env 10 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
    ⟨[9], .stuck ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩⟩ :=
  prim_bind_refused prog Demo.env 9 [9] x .log [.lit 1] s [] .init .init [1] []
    ⟨some 1, .ok 0, .init⟩ 0 rfl rfl (by decide) rfl rfl

/-- Planted response rule: continue an allowed successful answer without consulting `bindLife`.
This uses the same refusing instrumentation and oracle as the production instance above. -/
private def uncheckedResponse (hist : List Int) (a : Answer Int Int DState) : Report ops :=
  match a.result with
  | .ok v => runFrom prog Demo.env 9 (hist ++ a.event.toList)
      ⟨.done, [(x, v)], a.store, []⟩
  | .error kd => runFrom prog Demo.env 9 (hist ++ a.event.toList)
      ⟨.trapped kd s, [], a.store, []⟩

/-- Planted (law `ConcreteSpec.Core.Exec.prim_bind_refused`). Planted (law `ConcreteSpec.Core.Exec.continuation_refused`).
Ignoring the refused bind on the same allowed answer accepts its event and finishes, violating
both the stuck outcome and unchanged history. -/
example : ¬ uncheckedResponse [9] ⟨some 1, .ok 0, .init⟩ =
    ⟨[9], .stuck ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩⟩ := by
  intro h
  have he := congrArg Report.events h
  change [9, 1] = [9] at he
  cases he

/-- Error-capable operand contract: the environment may trap instead of producing a value. -/
abbrev errorOps : Operands := { ops with allows := fun _ _ _ _ => true }

/-- Instance (law `ConcreteSpec.Core.Exec.prim_error_bypasses_bind`). The bind callback refuses,
but an allowed error answer still produces a real labeled trap transition. -/
example : Step (O := errorOps) ⟨[]⟩
    ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ (.emit 1)
    ⟨.trapped .abort s, [], .init, []⟩ :=
  prim_error_bypasses_bind (O := errorOps) ⟨[]⟩ x .log [.lit 1] s [] .init .init [1] []
    ⟨some 1, .error .abort, .init⟩ .abort rfl rfl rfl

/-- Planted continuation: it incorrectly requires initialization even for error answers. -/
private def bindErrorContinuation (a : Answer Int Int DState) : Option (Config errorOps) :=
  (errorOps.life (.bind x) a.store).map fun st => match a.result with
    | .ok v => ⟨.done, [(x, v)], st, []⟩
    | .error kd => ⟨.trapped kd s, [], st, []⟩

/-- The bad response rule retains argument evaluation, the environment contract and the answer
label, replacing only the continuation. -/
private inductive BindErrors : Config errorOps → Label errorOps → Config errorOps → Prop where
  | prim {a c'} : errorOps.allows .log [1] DState.init a = true →
      bindErrorContinuation a = some c' →
      BindErrors ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ a.label c'

/-- Planted (law `ConcreteSpec.Core.Exec.prim_error_bypasses_bind`). The same request and
allowed error answer cannot take the law's required labeled trap transition under the bad rule. -/
example : ¬ BindErrors ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ (.emit 1)
    ⟨.trapped .abort s, [], .init, []⟩ := by
  intro h
  generalize hl : (Label.emit (O := errorOps) 1) = label at h
  cases h with
  | prim _ hk => simp [bindErrorContinuation] at hk

end Refusal

/-- A concrete destruction protocol, checked independently by cases on demo types and the
function table. In particular the non-Copy obligation is satisfiable alongside the signature. -/
def Demo.destroyProtocol : DestroyProtocol Demo.prog where
  impl := fun T => match T with | .tok => some Demo.eatF | _ => none
  sig := by
    intro T f h
    cases T with
    | unit => cases h
    | int => cases h
    | tok => cases h; exact ⟨Demo.eatFn, Demo.p, rfl, rfl, rfl⟩
  notCopy := by
    intro T f h
    cases T <;> simp_all

end ConcreteSpec.Core.Exec
