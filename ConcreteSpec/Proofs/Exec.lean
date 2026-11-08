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
  `plan`, in order (`cleans_iff_runs_plan`, `planWith_calls`); a `return` then reaches its caller with
  the value bound (`return_reaches_caller`), and a `break` or `continue` its loop
  (`break_reaches_loop`, `continue_reaches_loop`).
- **Exit values**: a `return` evaluates its value before any cleanup (`step_ret`).
- **Traps are terminal**: a trapped configuration has no step and no later event
  (`steps_from_trapped`); `abort()` runs no cleanup (`abort_runs_no_cleanup`), and so neither does a
  cleanup call that traps.
- **Evaluation order**: the first argument that traps decides the trap (`evalArgs_first_trap`).
- Steps of the machine itself are deterministic (`step_silent_deterministic`): only the answers of
  primitive operations, each allowed by its contract, can make two runs differ
  (`step_deterministic`).
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
(`cleans_iff_runs_plan`). -/

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

/-- **A cleanup runs exactly the plan**: through frames an exit can pass, the calls of `plan pre`,
in order, each with its scope's locals, with each scope's drop between them, and nothing else. -/
theorem cleans_iff_runs_plan {P : Program O} :
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
      | scope hr hd hc => exact RunsAll.each hr (.drop hd ((cleans_iff_runs_plan hp').1 hc))
    · intro h
      obtain ⟨tr₁, st₁, tr₂, rfl, ha, hb⟩ := RunsEach.split_calls h
      cases hb with
      | drop hd hb => exact .scope ha hd ((cleans_iff_runs_plan hp').2 hb)
  | .test l cr t b site dm :: pre, L, st, st', tr, hp => by
    have hp' := fun f hf => hp f (List.mem_cons_of_mem _ hf)
    constructor
    · intro h; cases h with
      | test hd hc => exact .drop hd ((cleans_iff_runs_plan hp').1 hc)
    · intro h; cases h with
      | drop hd hb => exact .test hd ((cleans_iff_runs_plan hp').2 hb)
  | .seq _ :: pre, L, st, st', tr, hp => by
    have hp' := fun f hf => hp f (List.mem_cons_of_mem _ hf)
    constructor
    · intro h; cases h with
      | seq hc => exact (cleans_iff_runs_plan hp').1 hc
    · intro h; exact .seq ((cleans_iff_runs_plan hp').2 h)
  | .body .. :: pre, L, st, st', tr, hp => by
    have hp' := fun f hf => hp f (List.mem_cons_of_mem _ hf)
    constructor
    · intro h; cases h with
      | body hc => exact (cleans_iff_runs_plan hp').1 hc
    · intro h; exact .body ((cleans_iff_runs_plan hp').2 h)
  | .resume _ :: _, _, _, _, _, hp => absurd (hp _ List.mem_cons_self) (by simp [Frame.passable])
  | .ret .. :: _, _, _, _, _, hp => absurd (hp _ List.mem_cons_self) (by simp [Frame.passable])

/-- Every frame an exit passes is passable. -/
theorem Passes.passable {e : Exit O.Val} : ∀ {f : Frame O}, Passes e f → f.passable = true
  | .seq _, _ | .scope .., _ | .test .., _ | .body .., _ => rfl
  | .resume _, h => h.elim
  | .ret .., h => h.elim

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

/-- Steps of the machine itself are deterministic. -/
theorem step_silent_deterministic {P : Program O} {c c₁ c₂ : Config O}
    (h₁ : step P c = some (.next c₁)) (h₂ : step P c = some (.next c₂)) : c₁ = c₂ := by
  rw [h₁] at h₂; cases h₂; rfl

/-- **Determinism up to the environment.** Two steps from one configuration reach the same
configuration with the same label, unless the configuration requests a primitive operation: then
each step takes an answer the contract allows, is labeled by it and continues with it, so only the
environment's answers can differ, and equal answers give equal steps. -/
theorem step_deterministic {P : Program O} {c c₁ c₂ : Config O} {l₁ l₂ : Label O}
    (h₁ : Step P c l₁ c₁) (h₂ : Step P c l₂ c₂) :
    (l₁ = l₂ ∧ c₁ = c₂) ∨ ∃ p vs site st k a₁ a₂, step P c = some (.request p vs site st k) ∧
      O.allows p vs st a₁ = true ∧ O.allows p vs st a₂ = true ∧
      l₁ = a₁.label ∧ l₂ = a₂.label ∧ k a₁ = some c₁ ∧ k a₂ = some c₂ := by
  cases h₁ with
  | silent h =>
    cases h₂ with
    | silent h' => exact .inl ⟨rfl, step_silent_deterministic h h'⟩
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
`steps_request` requires successful continuation, and `step_deterministic` records
those successful continuations explicitly. All other laws retain their premises.
-/

/-- A refused machine step has no transition, silent or primitive. -/
theorem no_step_of_none {P : Program O} {c : Config O} (h : step P c = none)
    (l : Label O) (c' : Config O) : ¬ Step P c l c' := by
  intro hs
  cases hs with
  | silent hs => rw [h] at hs; cases hs
  | prim hs _ _ => rw [h] at hs; cases hs

/-- Refusing initialization makes a binding stuck after its operand succeeds. -/
theorem bind_refused (P : Program O) (x : Var) (e : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st st' : O.Store) (v : O.Val) (k : List (Frame O))
    (he : O.eval L st e = .ok (v, st')) (hl : O.life (.bind x) st' = none) :
    step P ⟨.exec (.bind x e site), L, st, k⟩ = none := by
  simp [step, onRes, he, hl]

/-- Assignment also propagates refused initialization, even for an existing local. -/
theorem assign_refused (P : Program O) (x : Var) (e : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st st' : O.Store) (v : O.Val) (k : List (Frame O))
    (he : O.eval L st e = .ok (v, st')) (hl : O.life (.bind x) st' = none) :
    step P ⟨.exec (.assign x e site), L, st, k⟩ = none := by
  simp only [step, onRes, he]
  cases AList.lookup L x <;> simp [hl]

/-- Initial entry, including main, cannot produce a configuration if activation is refused. -/
theorem enter_refused (fn : Fn O) (vs : List O.Val) (st : O.Store)
    (hl : O.life (.enter (fn.params.map Prod.fst)) st = none) :
    enter fn vs st = none := by
  simp [enter, hl]

/-- Internal call entry refuses the step after evaluating all arguments. -/
theorem call_refused (P : Program O) (dst : Option Var) (f : FnId) (args : List O.Expr)
    (site : SiteId) (L : AList Var O.Val) (st st' : O.Store) (vs : List O.Val)
    (fn : Fn O) (k : List (Frame O)) (he : evalArgs O L st args = .ok (vs, st'))
    (hf : AList.lookup P.fns f = some fn)
    (hl : O.life (.enter (fn.params.map Prod.fst)) st' = none) :
    step P ⟨.exec (.call dst f args site), L, st, k⟩ = none := by
  simp [step, onRes, he, hf, hl]

/-- A normal scope exit cannot drop locals when instrumentation refuses. -/
theorem scope_drop_refused (P : Program O) (b : BlockId) (dm : List Var)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O))
    (hl : O.life (.drop (dead dm L)) st = none) :
    step P ⟨.done, L, st, .scope b dm [] :: k⟩ = none := by
  simp [step, dropOut, hl]

/-- The same refusal stops a structured exit through a scope or a test frame. -/
theorem exit_drop_refused (P : Program O) (e : Exit O.Val) (b : BlockId) (dm : List Var)
    (l : LoopId) (cr body : Stmt O) (t : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O))
    (hl : O.life (.drop (dead dm L)) st = none) :
    step P ⟨.exiting e, L, st, .scope b dm [] :: k⟩ = none ∧
    step P ⟨.exiting e, L, st, .test l cr t body site dm :: k⟩ = none := by
  simp [step, stepExit, dropOut, hl]

/-- After a successful loop test evaluation, refusal to drop its temporaries stops either branch. -/
theorem test_drop_refused (P : Program O) (l : LoopId) (cr body : Stmt O) (t : O.Expr)
    (site : SiteId) (dm : List Var) (L : AList Var O.Val) (st st' : O.Store)
    (v : O.Val) (k : List (Frame O)) (he : O.eval L st t = .ok (v, st'))
    (hl : O.life (.drop (dead dm L)) st' = none) :
    step P ⟨.done, L, st, .test l cr t body site dm :: k⟩ = none := by
  simp only [step, onRes, he]
  cases O.test v <;> simp [dropOut, hl]

/-- Refusing either activation end or result binding stops both explicit and Unit returns. -/
theorem return_refused (P : Program O) (dst : Option Var) (v : O.Val)
    (caller L : AList Var O.Val) (st : O.Store) (k : List (Frame O))
    (hl : (O.life .leave st).bind (bindLife O dst) = none) :
    step P ⟨.exiting (.ret v), L, st, .ret dst caller :: k⟩ = none ∧
    step P ⟨.done, L, st, .ret dst caller :: k⟩ = none := by
  cases he : O.life .leave st with
  | none => simp [step, stepExit, he]
  | some st' =>
    have hb : bindLife O dst st' = none := by simpa [he] using hl
    simp [step, stepExit, he, hb]

/-- Refusing a primitive result binding refuses its request continuation. Error answers bypass
binding and remain traps. The oracle's answer kind is unchanged. -/
theorem prim_bind_refused (P : Program O) (x : Var) (p : O.Prim) (args : List O.Expr)
    (site : SiteId) (L : AList Var O.Val) (st st' : O.Store) (vs : List O.Val)
    (k : List (Frame O)) (a : Answer O.Val O.Event O.Store) (v : O.Val)
    (he : evalArgs O L st args = .ok (vs, st')) (ha : a.result = .ok v)
    (hl : O.life (.bind x) a.store = none) :
    ∃ cont, step P ⟨.exec (.prim (some x) p args site), L, st, k⟩ =
      some (.request p vs site st' cont) ∧ cont a = none := by
  refine ⟨(fun ans => match ans.result with
    | .ok w => (bindLife O (some x) ans.store).map fun st'' =>
        ⟨.done, bindDst (some x) w L, st'', k⟩
    | .error kd => some ⟨.trapped kd site, L, ans.store, k⟩), ?_, ?_⟩
  · simp [step, onRes, he]
    rfl
  · simp [ha, bindLife, hl]

/-- A refused continuation admits no successful response transition for that answer. -/
theorem continuation_refused (cont : Answer O.Val O.Event O.Store → Option (Config O))
    (a : Answer O.Val O.Event O.Store) (h : cont a = none) :
    ¬ ∃ c', cont a = some c' := by
  simp [h]

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

private theorem entryParts_succ (n : Nat) :
    entryParts (n + 1) = entryParts n ++ [.key n, .value n] := by
  simp [entryParts, List.range_succ, List.flatMap_append]

private theorem entryParts_length : ∀ n, (entryParts n).length = 2 * n
  | 0 => rfl
  | n + 1 => by rw [entryParts_succ]; simp [entryParts_length n]; omega

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

end ConcreteSpec.Core.Exec
