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
- **Traps are terminal**: a trapped configuration is final (`Config.final?`) and blocked (`step_trapped`),
  and has no later step or event (`steps_from_trapped`); `abort()` runs no cleanup (`abort_runs_no_cleanup`),
  and so neither does a cleanup call that traps.
- **Evaluation order and determinism**: the first argument that traps decides the trap (`evalArgs_first_trap`);
  internal steps are unique (`rules_silent_deterministic`), and fixing an external answer fixes
  both label and next configuration by independent rule inversion (`rules_deterministic`).
  `rules_sound` and `rules_complete` relate these rules to the executable dispatcher.
- **Destruction** (Q2.3): an independently admitted destructor enters the ordinary consuming
  call semantics (`destroyCall_enters`). Validator characterizations `destroy_not_copy` and
  `destroyCall_sig` are representation lemmas. A sequence's
  element `i` is visited `i`-th and the backing last (`seqDestroyOrder_elem`, `seqDestroyOrder_storage`);
  a map's `i`-th entry has its key at `2i` and its value at `2i+1`, and the nodes come last
  (`mapDestroyOrder_entry`, `mapDestroyOrder_storage`).
-/

namespace ConcreteSpec.Core.Exec
open ConcreteSpec.Core

variable {O : Operands} {V : Type}

/-! ### Many steps -/

/-- Representation lemma. Lifts a single labeled step into a one-step execution trace
(approved: APPR-C1-codex-5531d9e34). -/
theorem Steps.single {P : Program O} {c c' : Config O} {l : Label O} (h : Step P c l c') :
    Steps P c l.events c' := by
  have := Steps.step h (Steps.refl c')
  simpa using this

/-- Representation lemma. Concatenates two multi-step execution traces (approved: APPR-C1-codex-5531d9e34). -/
theorem Steps.trans {P : Program O} {c c' c'' : Config O} {tr tr' : List O.Event}
    (h : Steps P c tr c') (h' : Steps P c' tr' c'') : Steps P c (tr ++ tr') c'' := by
  induction h with
  | refl => simpa using h'
  | step hs _ ih => simpa [List.append_assoc] using Steps.step hs (ih h')

/-- Representation lemma. Prepends a silent machine step to a multi-step execution trace
(approved: APPR-C1-codex-5531d9e34). -/
theorem steps_next {P : Program O} {c c' c'' : Config O} {tr : List O.Event}
    (h : step P c = some (.next c')) (h' : Steps P c' tr c'') : Steps P c tr c'' := by
  simpa [Label.events] using Steps.step (Step.silent h) h'

/-- Representation lemma. Prepends a primitive request/response step to a multi-step execution
trace (approved: APPR-C1-codex-5531d9e34). -/
private theorem steps_request_inv {P : Program O} {c c'' : Config O} {p : O.Prim} {vs : List O.Val}
    {site : SiteId} {st : O.Store} {k : Answer O.Val O.Event O.Store → Option (Config O)}
    {a : Answer O.Val O.Event O.Store} {tr : List O.Event}
    (h : step P c = some (.request p vs site st k)) (ha : O.allows p vs st a = true)
    {c' : Config O} (hk : k a = some c') (h' : Steps P c' tr c'') : Steps P c (a.label.events ++ tr) c'' :=
  Steps.step (Step.prim h ha hk) h'

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Executing `Stmt.prim` on dst, p, args, and site after
successful argument evaluation, an allowed oracle answer a with `Except.ok` v, and a successful
destination bind `bindLife` takes the labeled transition `Step` with `Answer.label` of a to `Ctrl.done` with
`bindDst` applied to dst, v, and L and the bound store. -/
theorem steps_request (P : Program O) (dst : Option Var) (p : O.Prim) (args : List O.Expr)
    (site : SiteId) (L : AList Var O.Val) (st st' st'' : O.Store) (vs : List O.Val)
    (k : List (Frame O)) (a : Answer O.Val O.Event O.Store) (v : O.Val)
    (he : evalArgs O L st args = .ok (vs, st')) (ha : O.allows p vs st' a = true)
    (hv : a.result = .ok v) (hb : bindLife O dst a.store = some st'') :
    Step P ⟨.exec (.prim dst p args site), L, st, k⟩ a.label
      ⟨.done, bindDst dst v L, st'', k⟩ := by
  apply Step.prim (by simp only [step, onRes, he]; rfl) ha
  simp [hv, hb]

/-! ### Sequencing -/

/-- Helper (computation). Definitional unfolding of `step` on `Stmt.seq` s t (approved: APPR-C1-codex-5531d9e34). -/
theorem step_seq (P : Program O) (s t : Stmt O) (L : AList Var O.Val) (st : O.Store)
    (k : List (Frame O)) :
    step P ⟨.exec (.seq s t), L, st, k⟩ = some (.next ⟨.exec s, L, st, .seq t :: k⟩) := rfl

/-- Helper (computation). Definitional unfolding of `step` at `Ctrl.done` on `Frame.seq` t
(approved: APPR-C1-codex-5531d9e34). -/
theorem step_done_seq (P : Program O) (t : Stmt O) (L : AList Var O.Val) (st : O.Store)
    (k : List (Frame O)) :
    step P ⟨.done, L, st, .seq t :: k⟩ = some (.next ⟨.exec t, L, st, k⟩) := rfl

/-- Helper (computation). Definitional unfolding of `step` at `Ctrl.exiting` e on `Frame.seq` t
(approved: APPR-C1-codex-5531d9e34). -/
theorem step_exit_seq (P : Program O) (e : Exit O.Val) (t : Stmt O) (L : AList Var O.Val)
    (st : O.Store) (k : List (Frame O)) :
    step P ⟨.exiting e, L, st, .seq t :: k⟩ = some (.next ⟨.exiting e, L, st, k⟩) := rfl

/-- Helper (computation). Definitional unfolding of `step` on `Stmt.block` b s (approved: APPR-C1-codex-5531d9e34). -/
theorem step_block (P : Program O) (b : BlockId) (s : Stmt O) (L : AList Var O.Val)
    (st : O.Store) (k : List (Frame O)) :
    step P ⟨.exec (.block b s), L, st, k⟩ =
      some (.next ⟨.exec s, L, st, .scope b (dom L) [] :: k⟩) := rfl

/-! ### Scope restoration -/

/-- Law (subject `ConcreteSpec.Core.Exec.restrict`). Looking up x in `restrict` dm L returns
`AList.lookup` L x when x ∈ dm and `none` otherwise. -/
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

/-- Law (subject `ConcreteSpec.Core.Exec.restrict`). Leaving a scope entered with locals L₀
keeps no local bound inside it. -/
theorem restrict_dom_drops (L₀ L : AList Var V) (x : Var) (hx : x ∉ dom L₀) :
    AList.lookup (restrict (dom L₀) L) x = none := by
  rw [lookup_restrict]; simp [hx]

/-! ### Registration -/

/-- Helper (computation). Definitional unfolding of `register` at `Frame.scope` (approved: APPR-C1-codex-5531d9e34). -/
theorem register_scope (d : Deferred O) (b : BlockId) (dm : List Var) (ds : List (Deferred O))
    (k : List (Frame O)) : register d (.scope b dm ds :: k) = some (.scope b dm (d :: ds) :: k) :=
  rfl

/-- Helper (computation). Definitional unfolding of `register` at `Frame.seq` (approved: APPR-C1-codex-5531d9e34). -/
theorem register_seq (d : Deferred O) (t : Stmt O) (k : List (Frame O)) :
    register d (.seq t :: k) = (register d k).map (.seq t :: ·) := rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Steps`). A block running defer d₁; defer d₂; s reaches
s with its scope holding d₂ before d₁, so d₂ runs first; nothing is emitted. -/
theorem two_defers_register (P : Program O) (b : BlockId) (d₁ d₂ : Deferred O) (s : Stmt O)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    Steps P ⟨.exec (.block b (.seq (.defer d₁.f d₁.args d₁.site)
        (.seq (.defer d₂.f d₂.args d₂.site) s))), L, st, k⟩ []
      ⟨.exec s, L, st, .scope b (dom L) [d₂, d₁] :: k⟩ :=
  steps_next rfl (steps_next rfl (steps_next rfl (steps_next rfl (steps_next rfl
    (steps_next rfl (steps_next rfl (Steps.refl _)))))))

/-! ### Exits -/

/-- Law (subject `ConcreteSpec.Core.Exec.step`). A `Stmt.ret` statement evaluates its value first: the next
configuration carries the value and the same frames, so no cleanup has run. -/
theorem step_ret (P : Program O) (e : O.Expr) (site : SiteId) (L : AList Var O.Val)
    (st st' : O.Store) (k : List (Frame O)) (v : O.Val) (hv : O.eval L st e = .ok (v, st')) :
    step P ⟨.exec (.ret e site), L, st, k⟩ = some (.next ⟨.exiting (.ret v), L, st', k⟩) := by
  simp [step, onRes, hv]

/-- Helper (computation). Definitional unfolding of `step` at `Ctrl.exiting` (`Exit.brk` l) on matching
`Frame.body` l (approved: APPR-C1-codex-5531d9e34). -/
theorem step_break_loop (P : Program O) (l : LoopId) (cr b : Stmt O) (t : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    step P ⟨.exiting (.brk l), L, st, .body l cr t b site :: k⟩ =
      some (.next ⟨.done, L, st, k⟩) := by
  simp [step, stepExit]

/-- Helper (computation). Definitional unfolding of `step` at `Ctrl.exiting` (`Exit.cont` l) on matching
`Frame.body` l (approved: APPR-C1-codex-5531d9e34). -/
theorem step_continue_loop (P : Program O) (l : LoopId) (cr b : Stmt O) (t : O.Expr)
    (site : SiteId) (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    step P ⟨.exiting (.cont l), L, st, .body l cr t b site :: k⟩ =
      some (.next ⟨.exec (.loop l cr t b site), L, st, k⟩) := by
  simp [step, stepExit]

/-- Law (subject `ConcreteSpec.Core.Exec.step`). A `Exit.ret` exit reaching the call frame restores the
caller's locals and binds the value; the state is the one the callee left, with the
instrumentation told the activation ended and the result bound. -/
theorem step_return_caller (P : Program O) (v : O.Val) (dst : Option Var)
    (caller L : AList Var O.Val) (st : O.Store) (k : List (Frame O))
    (stLeave stBind : O.Store) (hl : O.life .leave st = some stLeave)
    (hb : bindLife O dst stLeave = some stBind) :
    step P ⟨.exiting (.ret v), L, st, .ret dst caller :: k⟩ =
      some (.next ⟨.done, bindDst dst v caller, stBind, k⟩) := by
  simp [step, stepExit, hl, hb]

/-! ### Cleanup -/

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). At a scope with registered calls, an exit steps
if and only if the step is silent and starts the newest deferred call while remembering the exit. -/
theorem exit_scope_next (P : Program O) (e : Exit O.Val) (b : BlockId) (dm : List Var)
    (d : Deferred O) (ds : List (Deferred O)) (L : AList Var O.Val) (st : O.Store)
    (k : List (Frame O)) (l : Label O) (c : Config O) :
    Step P ⟨.exiting e, L, st, .scope b dm (d :: ds) :: k⟩ l c ↔
      l = .silent ∧ c = ⟨.exec d.stmt, L, st, .resume (.exit e) :: .scope b dm ds :: k⟩ := by
  constructor
  · intro h
    cases h with
    | silent h =>
      simp only [step, stepExit, startCleanup, Option.some.injEq, Out.next.injEq] at h
      exact ⟨rfl, h.symm⟩
    | prim h _ _ => simp [step, stepExit, startCleanup] at h
  · rintro ⟨rfl, rfl⟩
    exact .silent rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). At the normal end of a block with registered
calls, the machine steps if and only if the step is silent and starts the newest deferred call
while remembering `Pending.fall`. -/
theorem done_scope_next (P : Program O) (b : BlockId) (dm : List Var) (d : Deferred O)
    (ds : List (Deferred O)) (L : AList Var O.Val) (st : O.Store) (k : List (Frame O))
    (l : Label O) (c : Config O) :
    Step P ⟨.done, L, st, .scope b dm (d :: ds) :: k⟩ l c ↔
      l = .silent ∧ c = ⟨.exec d.stmt, L, st, .resume .fall :: .scope b dm ds :: k⟩ := by
  constructor
  · intro h
    cases h with
    | silent h =>
      simp only [step, startCleanup, Option.some.injEq, Out.next.injEq] at h
      exact ⟨rfl, h.symm⟩
    | prim h _ _ => simp [step, startCleanup] at h
  · rintro ⟨rfl, rfl⟩
    exact .silent rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Steps`). An exit leaving a scope whose calls complete
runs them newest first, each with its events and its effect on the state, and leaves the scope
with its locals restored; the exit, and a returned value, are unchanged. -/
theorem exit_scope_runs_defers (P : Program O) (e : Exit O.Val) (b : BlockId) (dm : List Var)
    (L : AList Var O.Val) (k : List (Frame O)) (ds : List (Deferred O)) (st st' : O.Store)
    (tr : List O.Event) (stDrop : O.Store) (h : RunsAll P L ds st tr st')
    (hd : dropOut O dm L st' = some stDrop) :
    Steps P ⟨.exiting e, L, st, .scope b dm ds :: k⟩ tr
      ⟨.exiting e, restrict dm L, stDrop, k⟩ := by
  induction h with
  | nil st => exact steps_next (by simp [step, stepExit, hd]) (Steps.refl _)
  | @cons d ds st tr₁ st₁ tr₂ st₂ hcall _ ih =>
    exact steps_next (c := ⟨.exiting e, L, st, .scope b dm (d :: ds) :: k⟩) rfl
      (Steps.trans (hcall _) (steps_next rfl (ih hd)))

/-- Law (subject `ConcreteSpec.Core.Exec.Steps`). The normal end of a block runs its registered
calls newest first and restores the scope's locals. -/
theorem done_scope_runs_defers (P : Program O) (b : BlockId) (dm : List Var)
    (L : AList Var O.Val) (k : List (Frame O)) (ds : List (Deferred O)) (st st' : O.Store)
    (tr : List O.Event) (stDrop : O.Store) (h : RunsAll P L ds st tr st')
    (hd : dropOut O dm L st' = some stDrop) :
    Steps P ⟨.done, L, st, .scope b dm ds :: k⟩ tr ⟨.done, restrict dm L, stDrop, k⟩ := by
  induction h with
  | nil st => exact steps_next (by simp [step, hd]) (Steps.refl _)
  | @cons d ds st tr₁ st₁ tr₂ st₂ hcall _ ih =>
    exact steps_next (c := ⟨.done, L, st, .scope b dm (d :: ds) :: k⟩) rfl
      (Steps.trans (hcall _) (steps_next rfl (ih hd)))

/-- Law (subject `ConcreteSpec.Core.Exec.Steps`). When the cleanup calls complete (`Cleans`), an
exit passing frames pre arrives at the frames below with the same exit, the locals the scopes
restored, and the state the calls left. -/
theorem unwind_runs_plan (P : Program O) (e : Exit O.Val) (k pre : List (Frame O))
    (L : AList Var O.Val) (st st' : O.Store) (tr : List O.Event)
    (h : Cleans P pre L st tr st') (hp : ∀ (f : Frame O), f ∈ pre → Passes e f) :
    Steps P ⟨.exiting e, L, st, pre ++ k⟩ tr ⟨.exiting e, unwindLocals pre L, st', k⟩ := by
  induction h with
  | nil L st => exact Steps.refl _
  | @scope b dm ds pre L st tr₁ st₁ stDrop tr₂ st₂ hr hd _ ih =>
    have h1 := exit_scope_runs_defers P e b dm L (pre ++ k) ds st st₁ tr₁ stDrop hr hd
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

/-- Helper (computation). Definitional unfolding of `plan` on nested `Frame.scope` frames
(approved: APPR-C1-codex-5531d9e34). -/
theorem plan_scopes (b₁ b₂ : BlockId) (d₁ d₂ : List Var) (ds₁ ds₂ : List (Deferred O))
    (k : List (Frame O)) :
    plan (.scope b₁ d₁ ds₁ :: .scope b₂ d₂ ds₂ :: k) = ds₁ ++ (ds₂ ++ plan k) := rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Steps`). A `Exit.ret` exit reaches its caller after the cleanup
of every scope it leaves: the caller's locals come back with the value bound, in the state the
cleanup left. -/
theorem return_reaches_caller (P : Program O) (v : O.Val) (dst : Option Var)
    (caller : AList Var O.Val) (k pre : List (Frame O))
    (L : AList Var O.Val) (st st' : O.Store) (tr : List O.Event)
    (stLeave stBind : O.Store) (h : Cleans P pre L st tr st')
    (hp : ∀ (f : Frame O), f ∈ pre → Passes (.ret v) f)
    (hl : O.life .leave st' = some stLeave)
    (hb : bindLife O dst stLeave = some stBind) :
    Steps P ⟨.exiting (.ret v), L, st, pre ++ .ret dst caller :: k⟩ tr
      ⟨.done, bindDst dst v caller, stBind, k⟩ := by
  have := Steps.trans (unwind_runs_plan P (.ret v) (.ret dst caller :: k) pre L st st' tr h hp)
    (steps_next (step_return_caller P v dst caller _ st' k stLeave stBind hl hb) (Steps.refl _))
  simpa using this

/-- Law (subject `ConcreteSpec.Core.Exec.Steps`). A `Exit.brk` exit reaches its loop after the cleanup of
the scopes inside the loop body, and the loop is done. -/
theorem break_reaches_loop (P : Program O) (l : LoopId) (cr b : Stmt O) (t : O.Expr)
    (site : SiteId) (k pre : List (Frame O)) (L : AList Var O.Val)
    (st st' : O.Store) (tr : List O.Event)
    (h : Cleans P pre L st tr st') (hp : ∀ (f : Frame O), f ∈ pre → Passes (.brk l) f) :
    Steps P ⟨.exiting (.brk l), L, st, pre ++ .body l cr t b site :: k⟩ tr
      ⟨.done, unwindLocals pre L, st', k⟩ := by
  have := Steps.trans (unwind_runs_plan P (.brk l) (.body l cr t b site :: k) pre L st st' tr h hp)
    (steps_next (step_break_loop P l cr b t site _ st' k) (Steps.refl _))
  simpa using this

/-- Law (subject `ConcreteSpec.Core.Exec.Steps`). A `Exit.cont` exit reaches its loop after the same
cleanup, and the loop starts again from its condition region. -/
theorem continue_reaches_loop (P : Program O) (l : LoopId) (cr b : Stmt O) (t : O.Expr)
    (site : SiteId) (k pre : List (Frame O)) (L : AList Var O.Val)
    (st st' : O.Store) (tr : List O.Event)
    (h : Cleans P pre L st tr st') (hp : ∀ (f : Frame O), f ∈ pre → Passes (.cont l) f) :
    Steps P ⟨.exiting (.cont l), L, st, pre ++ .body l cr t b site :: k⟩ tr
      ⟨.exec (.loop l cr t b site), unwindLocals pre L, st', k⟩ := by
  have := Steps.trans (unwind_runs_plan P (.cont l) (.body l cr t b site :: k) pre L st st' tr h hp)
    (steps_next (step_continue_loop P l cr b t site _ st' k) (Steps.refl _))
  simpa using this

/-! ### What the cleanup runs

`Cleans` runs the calls of `plan`, each with the locals of its scope's point in the unwinding, and
tells the instrumentation which locals each scope it leaves drops: `planWith` lists those actions in
order, its calls are `plan`'s (`planWith_calls`), and a cleanup is a run of exactly those actions
(`cleans_iff_runs_plan_inv`). -/

/-- One action of a cleanup: a call, with the locals it runs with, or leaving a scope entered with
`dm`, from locals `L` to restored locals `L'`. -/
inductive CleanAct (O : Operands) where
  | call (L : AList Var O.Val) (d : Deferred O)
  | drop (dm : List Var) (L L' : AList Var O.Val)

/-- The call of an action, if it is one. -/
def CleanAct.call? : CleanAct O → Option (Deferred O)
  | .call _ d => some d
  | .drop .. => none

/-- The actions an exit through frames `pre` performs, from locals `L`, in order. -/
def planWith : List (Frame O) → AList Var O.Val → List (CleanAct O)
  | [], _ => []
  | .scope _ dm ds :: k, L =>
    ds.map (.call L) ++ .drop dm L (restrict dm L) :: planWith k (restrict dm L)
  | .test _ _ _ _ _ dm :: k, L => .drop dm L (restrict dm L) :: planWith k (restrict dm L)
  | _ :: k, L => planWith k L

/-- Law (subject `ConcreteSpec.Core.Exec.planWith`, `ConcreteSpec.Core.Exec.plan`). Filtering
`planWith` pre L to its `CleanAct.call` actions yields `plan` pre. -/
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
      RunsEach P (.drop dm L (restrict dm L) :: rest) st tr st'

/-- Helper (inversion). Composes a `RunsAll` call certificate sequence with a `RunsEach` suffix
(approved: APPR-C1-codex-5531d9e34). -/
theorem RunsAll.each {P : Program O} {L : AList Var O.Val} {ds : List (Deferred O)}
    {st st₁ st₂ : O.Store} {tr₁ tr₂ : List O.Event} {rest : List (CleanAct O)}
    (h : RunsAll P L ds st tr₁ st₁) (h' : RunsEach P rest st₁ tr₂ st₂) :
    RunsEach P (ds.map (.call L) ++ rest) st (tr₁ ++ tr₂) st₂ := by
  induction h with
  | nil => simpa using h'
  | cons hd _ ih => simpa [List.append_assoc] using RunsEach.call hd (ih h')

/-- Helper (inversion). Splits a `RunsEach` over mapped calls and a suffix
(approved: APPR-C1-codex-5531d9e34). -/
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

/-- Helper (inversion). Relates two cleanup certificates; not a law about actual runs
(approved: APPR-C1-codex-5531d9e34). -/
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

/-- Helper (inversion). Every frame an exit passes is passable (approved: APPR-C1-codex-5531d9e34). -/
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

/-- Helper (inversion). Forgetting first-arrival information preserves the actual execution
(approved: APPR-C1-codex-5531d9e34). -/
theorem FirstSteps.steps_inv {P : Program O} {stop : Config O → Prop} {c c' : Config O}
    {tr : List O.Event} (h : FirstSteps (Step P) stop c tr c') : Steps P c tr c' := by
  induction h with
  | stop => exact .refl _
  | step _ hs _ ih => exact .step hs ih

/-- The local-lifetime action observed when an exit directly leaves its current frame:
it records both the pre-step locals `L` and the actual post-step locals `L'` produced by the
transition. Calls and their nested cleanup are recorded separately. -/
def leaving : List (Frame O) → AList Var O.Val → AList Var O.Val → List (CleanAct O)
  | .scope _ dm [] :: _, L, L' => [.drop dm L L']
  | .test _ _ _ _ _ dm :: _, L, L' => [.drop dm L L']
  | _, _, _ => []

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
      BoundaryRun P R e k pre L st (leaving pre L L' ++ acts) tr Lend stEnd
  | call {pre rest L st st' d acts tr₁ tr₂ Lend stEnd q} : pre ≠ [] →
      R ⟨.exiting e, L, st, pre ++ k⟩ .silent
        ⟨.exec d.stmt, L, st, .resume (.exit e) :: rest ++ k⟩ →
      FirstSteps (Step P) (fun c => c.ctrl = .done ∧ c.frames = q)
        ⟨.exec d.stmt, L, st, q⟩ tr₁
        ⟨.done, L, st', q⟩ →
      R ⟨.done, L, st', .resume (.exit e) :: rest ++ k⟩ .silent
        ⟨.exiting e, L, st', rest ++ k⟩ →
      BoundaryRun P R e k rest L st' acts tr₂ Lend stEnd →
      BoundaryRun P R e k pre L st (.call L d :: acts) (tr₁ ++ tr₂) Lend stEnd

/-- Execute an action list independently of the cleanup dispatcher, threading both the local
environment and the store: calls must complete their first-return execution preserving the
caller's locals, and drops must perform both `dropOut` on the store and `restrict` on the locals. -/
inductive Performed (P : Program O) :
    List (CleanAct O) → AList Var O.Val → O.Store → List O.Event → AList Var O.Val → O.Store → Prop where
  | nil {L st} : Performed P [] L st [] L st
  | call {L d rest st tr₁ st₁ tr₂ L₂ st₂ q} :
      FirstSteps (Step P) (fun c => c.ctrl = .done ∧ c.frames = q)
        ⟨.exec d.stmt, L, st, q⟩ tr₁ ⟨.done, L, st₁, q⟩ →
      Performed P rest L st₁ tr₂ L₂ st₂ →
      Performed P (.call L d :: rest) L st (tr₁ ++ tr₂) L₂ st₂
  | drop {dm L rest st stDrop tr L' st'} : dropOut O dm L st = some stDrop →
      Performed P rest (restrict dm L) stDrop tr L' st' →
      Performed P (.drop dm L (restrict dm L) :: rest) L st tr L' st'

/-- Helper (inversion). Compose independent action executions (approved: APPR-C1-codex-5531d9e34). -/
private theorem performed_append_inv {P : Program O} {as bs : List (CleanAct O)}
    {L L₁ L₂ : AList Var O.Val} {st st₁ st₂ : O.Store} {tr₁ tr₂ : List O.Event}
    (ha : Performed P as L st tr₁ L₁ st₁) (hb : Performed P bs L₁ st₁ tr₂ L₂ st₂) :
    Performed P (as ++ bs) L st (tr₁ ++ tr₂) L₂ st₂ := by
  induction ha with
  | nil => simpa using hb
  | call hc _ ih => simpa [List.append_assoc] using Performed.call hc (ih hb)
  | drop hd _ ih => exact .drop hd (ih hb)

/-- Helper (inversion). Internal dispatcher decomposition supporting the observable silence
law `exit_label`; not an independent soundness claim (approved: APPR-C1-codex-5531d9e34). -/
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

/-- Helper (inversion). Converts the transition representation using the separately proved exit-silence restriction (approved: APPR-C1-codex-5531d9e34). -/
private theorem exit_step_next_inv {P : Program O} {e : Exit O.Val} {L : AList Var O.Val}
    {st : O.Store} {frames : List (Frame O)} {label : Label O} {c : Config O}
    (h : Step P ⟨.exiting e, L, st, frames⟩ label c) :
    step P ⟨.exiting e, L, st, frames⟩ = some (.next c) := by
  cases h with
  | silent h => exact h
  | prim h _ _ => exact False.elim (stepExit_no_request_inv L st e frames _ _ _ _ _ h)

/-- Helper (inversion). A directly passing exit step removes precisely the top frame,
restores its saved locals, and performs its local drop (approved: APPR-C1-codex-5531d9e34). -/
private theorem exit_pass_plan_inv {P : Program O} {e : Exit O.Val} {k pre pre' : List (Frame O)}
    {L L' : AList Var O.Val} {st st' : O.Store} {label : Label O}
    (h : Step P ⟨.exiting e, L, st, pre ++ k⟩ label
      ⟨.exiting e, L', st', pre' ++ k⟩) (hne : pre ≠ []) :
    ((∀ f ∈ pre', Passes e f) → ∀ f ∈ pre, Passes e f) ∧
      planWith pre L = leaving pre L L' ++ planWith pre' L' ∧
      unwindLocals pre L = unwindLocals pre' L' ∧
      Performed P (leaving pre L L') L st [] L' st' := by
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
      refine ⟨?_, rfl, rfl, .nil⟩
      intro hp f₀ hf₀
      cases hf₀ with
      | head => trivial
      | tail _ hf₀ => exact hp f₀ hf₀
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
          refine ⟨?_, rfl, rfl, .drop hd .nil⟩
          intro hp f₀ hf₀
          cases hf₀ with
          | head => trivial
          | tail _ hf₀ => exact hp f₀ hf₀
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
        refine ⟨?_, rfl, rfl, .drop hd .nil⟩
        intro hp f₀ hf₀
        cases hf₀ with
        | head => trivial
        | tail _ hf₀ => exact hp f₀ hf₀
    | body l cr t body site =>
      cases e with
      | ret v =>
        simp only [step, stepExit, Option.some.injEq, Out.next.injEq, Config.mk.injEq] at next
        obtain ⟨_, hL, hst, hk⟩ := next
        obtain ⟨rfl, rfl⟩ := finish hL hk
        subst st'
        refine ⟨?_, rfl, rfl, .nil⟩
        intro hp f₀ hf₀
        cases hf₀ with
        | head => trivial
        | tail _ hf₀ => exact hp f₀ hf₀
      | brk l' =>
        by_cases he : l' = l
        · simp [step, stepExit, he] at next
        · simp only [step, stepExit, he, ↓reduceIte, Option.some.injEq, Out.next.injEq,
            Config.mk.injEq] at next
          obtain ⟨_, hL, hst, hk⟩ := next
          obtain ⟨rfl, rfl⟩ := finish hL hk
          subst st'
          refine ⟨?_, rfl, rfl, .nil⟩
          intro hp f₀ hf₀
          cases hf₀ with
          | head => exact he
          | tail _ hf₀ => exact hp f₀ hf₀
      | cont l' =>
        by_cases he : l' = l
        · simp [step, stepExit, he] at next
        · simp only [step, stepExit, he, ↓reduceIte, Option.some.injEq, Out.next.injEq,
            Config.mk.injEq] at next
          obtain ⟨_, hL, hst, hk⟩ := next
          obtain ⟨rfl, rfl⟩ := finish hL hk
          subst st'
          refine ⟨?_, rfl, rfl, .nil⟩
          intro hp f₀ hf₀
          cases hf₀ with
          | head => exact he
          | tail _ hf₀ => exact hp f₀ hf₀
    | resume p => simp [step, stepExit] at next
    | ret dst caller =>
      cases e <;> simp [step, stepExit] at next
      case ret v =>
        cases hl : O.life .leave st <;> simp [hl] at next


/-- Helper (inversion). A cleanup dispatch step selects the head registration and preserves
its scope with the remaining registrations (approved: APPR-C1-codex-5531d9e34). -/
private theorem exit_call_plan_inv {P : Program O} {e : Exit O.Val} {k pre rest : List (Frame O)}
    {L : AList Var O.Val} {st : O.Store} {d : Deferred O} {label : Label O}
    (h : Step P ⟨.exiting e, L, st, pre ++ k⟩ label
      ⟨.exec d.stmt, L, st, .resume (.exit e) :: rest ++ k⟩) (hne : pre ≠ []) :
    ((∀ f ∈ rest, Passes e f) → ∀ f ∈ pre, Passes e f) ∧
      planWith pre L = .call L d :: planWith rest L ∧
      unwindLocals pre L = unwindLocals rest L := by
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
        refine ⟨?_, by simp [planWith], rfl⟩
        intro hp f₀ hf₀
        cases hf₀ with
        | head => trivial
        | tail _ hf₀ => exact hp f₀ (List.mem_cons_of_mem _ hf₀)
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

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). A completed sequence of deferred calls dispatches each call before the final scope drop. -/
theorem boundary_scope_calls {P : Program O} {e : Exit O.Val} {k : List (Frame O)}
    (b : BlockId) (dm : List Var) (pre : List (Frame O)) (ds : List (Deferred O))
    {L L' : AList Var O.Val} {st st' : O.Store}
    {acts : List (CleanAct O)} {tr : List O.Event}
    (hperf : Performed P (ds.map (.call L) ++ acts) L st tr L' st')
    (hbase : ∀ st₁ tr₂, Performed P acts L st₁ tr₂ L' st' →
      BoundaryRun P (Step P) e k (.scope b dm [] :: pre) L st₁ acts tr₂ L' st') :
    BoundaryRun P (Step P) e k (.scope b dm ds :: pre) L st (ds.map (.call L) ++ acts) tr L' st' := by
  induction ds generalizing st tr with
  | nil => exact hbase st tr hperf
  | cons d ds ih =>
    cases hperf with
    | call hc hrest =>
      have hr := ih hrest
      simpa [List.append_assoc] using
        BoundaryRun.call (pre := .scope b dm (d :: ds) :: pre)
          (rest := .scope b dm ds :: pre) (by simp) (.silent rfl) hc (.silent rfl) hr

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). An independently performed action plan reconstructs the actual first-boundary execution. -/
theorem boundary_of_performed {P : Program O} {e : Exit O.Val} {k : List (Frame O)}
    (pre : List (Frame O)) {L L' : AList Var O.Val} {st st' : O.Store} {tr : List O.Event}
    (hp : ∀ f ∈ pre, Passes e f)
    (hperf : Performed P (planWith pre L) L st tr L' st') :
    BoundaryRun P (Step P) e k pre L st (planWith pre L) tr L' st' := by
  induction pre generalizing L st tr with
  | nil =>
    cases hperf
    exact .stop
  | cons f pre ih =>
    have hp' : ∀ f₀ ∈ pre, Passes e f₀ := fun f₀ hf₀ => hp f₀ (List.mem_cons_of_mem _ hf₀)
    have hpf : Passes e f := hp f List.mem_cons_self
    cases f with
    | seq t =>
      simpa [leaving, planWith] using BoundaryRun.pass (pre := .seq t :: pre) (pre' := pre)
        (by simp) (.silent rfl) (ih hp' hperf)
    | scope b dm ds =>
      simp only [planWith] at hperf ⊢
      refine boundary_scope_calls b dm pre ds hperf ?_
      intro st₁ tr₂ hdrop
      cases hdrop with
      | drop hd hrest =>
        have hs : Step P ⟨.exiting e, L, st₁, .scope b dm [] :: (pre ++ k)⟩ .silent
            ⟨.exiting e, restrict dm L, _, pre ++ k⟩ :=
          .silent (by simp only [step, stepExit, hd]; rfl)
        simpa [leaving] using BoundaryRun.pass (pre := .scope b dm [] :: pre) (pre' := pre)
          (by simp) hs (ih hp' hrest)
    | test l cr t body site dm =>
      simp only [planWith] at hperf ⊢
      cases hperf with
      | drop hd hrest =>
        have hs : Step P ⟨.exiting e, L, st, .test l cr t body site dm :: (pre ++ k)⟩ .silent
            ⟨.exiting e, restrict dm L, _, pre ++ k⟩ :=
          .silent (by simp only [step, stepExit, hd]; rfl)
        simpa [leaving] using BoundaryRun.pass (pre := .test l cr t body site dm :: pre)
          (pre' := pre) (by simp) hs (ih hp' hrest)
    | body l cr t body site =>
      cases e with
      | ret v =>
        simpa [leaving, planWith] using BoundaryRun.pass
          (pre := .body l cr t body site :: pre) (pre' := pre) (by simp) (.silent rfl) (ih hp' hperf)
      | brk l' =>
        have hne : l' ≠ l := hpf
        have hs : Step P ⟨.exiting (.brk l'), L, st, .body l cr t body site :: (pre ++ k)⟩ .silent
            ⟨.exiting (.brk l'), L, st, pre ++ k⟩ :=
          .silent (by simp [step, stepExit, hne])
        simpa [leaving, planWith] using BoundaryRun.pass
          (pre := .body l cr t body site :: pre) (pre' := pre) (by simp) hs (ih hp' hperf)
      | cont l' =>
        have hne : l' ≠ l := hpf
        have hs : Step P ⟨.exiting (.cont l'), L, st, .body l cr t body site :: (pre ++ k)⟩ .silent
            ⟨.exiting (.cont l'), L, st, pre ++ k⟩ :=
          .silent (by simp [step, stepExit, hne])
        simpa [leaving, planWith] using BoundaryRun.pass
          (pre := .body l cr t body site :: pre) (pre' := pre) (by simp) hs (ih hp' hperf)
    | resume p => exact False.elim hpf
    | ret dst caller => exact False.elim hpf

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). A successful actual unwind, stopped on first
arrival at the selected stack boundary, occurs if and only if every frame in pre passes the
exit, the observed actions are `planWith` pre L, the final locals are `unwindLocals` pre L, and
all planned calls and drops are performed in order. -/
theorem cleanup_runs_exact_plan (P : Program O) (e : Exit O.Val) (k pre : List (Frame O))
    (L L' : AList Var O.Val) (st st' : O.Store) (acts : List (CleanAct O)) (tr : List O.Event) :
    BoundaryRun P (Step P) e k pre L st acts tr L' st' ↔
      (∀ (f : Frame O), f ∈ pre → Passes e f) ∧ acts = planWith pre L ∧
        L' = unwindLocals pre L ∧ Performed P acts L st tr L' st' := by
  constructor
  · intro h
    induction h with
    | stop => exact ⟨(by intro _ hf; cases hf), rfl, rfl, Performed.nil⟩
    | pass hne hs _ ih =>
      obtain ⟨hpp, he, hu, hp⟩ := exit_pass_plan_inv hs hne
      exact ⟨hpp ih.1, by rw [ih.2.1, ← he], by rw [ih.2.2.1, ← hu],
        by simpa using performed_append_inv hp ih.2.2.2⟩
    | call hne hs hc _ _ ih =>
      obtain ⟨hpp, he, hu⟩ := exit_call_plan_inv hs hne
      exact ⟨hpp ih.1, by rw [ih.2.1, ← he], by rw [ih.2.2.1, ← hu], Performed.call hc ih.2.2.2⟩
  · rintro ⟨hp, rfl, _, hperf⟩
    exact boundary_of_performed pre hp hperf

/-! ### Traps are terminal -/

/-- No labeled transition is available under `R`. Parameterized by the transition relation so
contract laws and planted mutants share the same predicate. -/
def Blocked (R : Config O → Label O → Config O → Prop) (c : Config O) : Prop :=
  ∀ l c', ¬ R c l c'

/-- Helper (inversion). A missing implementation result has no relational transition
(approved: APPR-C1-codex-5531d9e34). -/
private theorem no_step_of_none_inv {P : Program O} {c : Config O} (h : step P c = none) :
    Blocked (Step P) c := by
  intro l c' hs
  cases hs with
  | silent hs => rw [h] at hs; cases hs
  | prim hs _ _ => rw [h] at hs; cases hs

/-- Law (subject `ConcreteSpec.Core.Exec.Config.final?`, `ConcreteSpec.Core.Exec.Step`). A
configuration's `Config.final?` reports `Final.trapped` kd site if and only if its control is
`Ctrl.trapped` kd site, and every trapped configuration is blocked in `Step`. -/
theorem step_trapped (P : Program O) (ctrl : Ctrl O) (kd : TrapKind) (site : SiteId)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    (Config.final? ⟨ctrl, L, st, k⟩ = some (.trapped kd site) ↔ ctrl = .trapped kd site) ∧
      Blocked (Step P) ⟨.trapped kd site, L, st, k⟩ := by
  refine ⟨⟨fun h => ?_, fun h => by subst h; rfl⟩, no_step_of_none_inv rfl⟩
  cases ctrl with
  | exec _ => cases h
  | done => cases k <;> cases h
  | exiting e => cases e <;> cases k <;> cases h
  | trapped kd' site' => cases h; rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Steps`). After a trap nothing runs: a multi-step execution
from a trapped configuration is equivalent to emitting no events and remaining in that exact
trapped configuration. -/
theorem steps_from_trapped (P : Program O) (kd : TrapKind) (site : SiteId) (L : AList Var O.Val)
    (st : O.Store) (k : List (Frame O)) (tr : List O.Event) (c : Config O) :
    Steps P ⟨.trapped kd site, L, st, k⟩ tr c ↔
      tr = [] ∧ c = ⟨.trapped kd site, L, st, k⟩ := by
  constructor
  · intro h
    cases h with
    | refl => exact ⟨rfl, rfl⟩
    | step hs _ =>
      exact False.elim ((step_trapped P (.trapped kd site) kd site L st k).2 _ _ hs)
  · rintro ⟨rfl, rfl⟩
    exact .refl _

/-- A call retains its saved caller frame until normal return. -/
def Protected (L : AList Var O.Val) (q : List (Frame O)) (c : Config O) : Prop :=
  ∃ pre, c.frames = pre ++ .ret none L :: q

/-- Registration preserves a saved caller boundary for every successful result. -/
def RegistrationPreservesCaller (reg : Deferred O → List (Frame O) → Option (List (Frame O)))
    (d : Deferred O) (caller : AList Var O.Val) (q : List (Frame O)) : Prop :=
  ∀ pre k', reg d (pre ++ .ret none caller :: q) = some k' →
    ∃ pre', k' = pre' ++ .ret none caller :: q

/-- A transition either preserves the saved boundary or restores it with the caller locals. -/
def PreservesCaller (R : Config O → Label O → Config O → Prop)
    (caller : AList Var O.Val) (q : List (Frame O)) (c : Config O) : Prop :=
  ∀ label c', R c label c' →
    Protected caller q c' ∨ (c'.ctrl = .done ∧ c'.locals = caller ∧ c'.frames = q)

/-- Law (subject `ConcreteSpec.Core.Exec.register`). Defer registration preserves the saved caller boundary. -/
theorem register_protected (d : Deferred O) (caller : AList Var O.Val)
    (q : List (Frame O)) : RegistrationPreservesCaller register d caller q := by
  intro pre
  induction pre with
  | nil =>
    intro k' h
    simp [register] at h
  | cons f pre ih =>
    intro k' h
    cases f with
    | seq t =>
      simp only [List.cons_append, register] at h
      cases hr : register d (pre ++ .ret none caller :: q) with
      | none => simp [hr] at h
      | some rest =>
        simp only [hr, Option.map_some, Option.some.injEq] at h
        obtain ⟨pre', rfl⟩ := ih rest hr
        exact ⟨.seq t :: pre', h.symm⟩
    | scope b dm ds =>
      simp only [List.cons_append, register, Option.some.injEq] at h
      exact ⟨.scope b dm (d :: ds) :: pre, h.symm⟩
    | test l cr t body site dm => simp [register] at h
    | body l cr t body site => simp [register] at h
    | resume p => simp [register] at h
    | ret dst L => simp [register] at h

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Each transition preserves the saved caller boundary or returns to it with caller locals restored. -/
theorem protected_step {P : Program O} {caller : AList Var O.Val}
    {q : List (Frame O)} {c : Config O} (hp : Protected caller q c) :
    PreservesCaller (Step P) caller q c := by
  intro label c' h
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
          all_goals try grind [Protected, register_protected]
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
          all_goals try grind [Protected, register_protected]
          all_goals try first
            | rfl
            | exact ⟨f :: pre, rfl⟩
            | exact ⟨_ :: f :: pre, rfl⟩
            | exact ⟨_ :: _ :: f :: pre, rfl⟩
            | exact .inl ⟨f :: pre, rfl⟩
            | exact .inl (register_protected _ caller q (f :: pre) _ (by assumption))
            | exact .inl ⟨_ :: f :: pre, rfl⟩
            | exact .inl ⟨_ :: _ :: f :: pre, rfl⟩
        | done =>
          cases f <;> simp only [step, onRes, startCleanup, Option.bind, Option.map, bindLife] at h <;>
            repeat' first | simp_all | split at h
          all_goals try simp only [Option.some.injEq, Out.next.injEq] at h
          all_goals try subst c'
          all_goals try contradiction
          all_goals repeat' first | simp_all [Protected, bindDst] | split at h
          all_goals try grind [Protected, register_protected]
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
          all_goals try grind [Protected, register_protected]
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
inductive FirstStepsN (R : Config O → Label O → Config O → Prop) (stop : Config O → Prop) :
    Config O → List O.Event → Config O → Nat → Prop where
  | stop {c} : stop c → FirstStepsN R stop c [] c 0
  | step {c l c' tr c'' n} : ¬ stop c → R c l c' → FirstStepsN R stop c' tr c'' n →
      FirstStepsN R stop c (l.events ++ tr) c'' (n + 1)

/-- Helper (inversion). Adds an induction index to the first-arrival certificate without changing its execution (approved: APPR-C1-codex-5531d9e34). -/
private theorem FirstSteps.sized_inv {P : Program O} {stop : Config O → Prop}
    {c c' : Config O} {tr : List O.Event} (h : FirstSteps (Step P) stop c tr c') :
    ∃ n, FirstStepsN (Step P) stop c tr c' n := by
  induction h with
  | stop hs => exact ⟨0, .stop hs⟩
  | step hn hs _ ih => obtain ⟨n, ih⟩ := ih; exact ⟨n + 1, .step hn hs ih⟩

/-- Helper (inversion). Erases only the induction index of a first-arrival certificate (approved: APPR-C1-codex-5531d9e34). -/
private theorem FirstStepsN.unsized_inv {P : Program O} {stop : Config O → Prop}
    {c c' : Config O} {tr : List O.Event} {n : Nat} (h : FirstStepsN (Step P) stop c tr c' n) :
    FirstSteps (Step P) stop c tr c' := by
  induction h with
  | stop hs => exact .stop hs
  | step hn hs _ ih => exact .step hn hs ih

/-- Helper (inversion). List-length consequence of the saved-boundary predicate used in return segmentation (approved: APPR-C1-codex-5531d9e34). -/
private theorem protected_height_inv {caller : AList Var O.Val} {q : List (Frame O)}
    {c : Config O} (h : Protected caller q c) : q.length < c.frames.length := by
  obtain ⟨pre, he⟩ := h
  simp only [he, List.length_append, List.length_cons]
  omega

/-- Helper (inversion). Length contradiction excluding an already returned configuration while its saved frame remains (approved: APPR-C1-codex-5531d9e34). -/
private theorem protected_not_return_inv {caller : AList Var O.Val} {q : List (Frame O)}
    {c : Config O} (h : Protected caller q c) : ¬ (c.ctrl = .done ∧ c.frames = q) := by
  intro hr
  have hh := protected_height_inv h
  rw [hr.2] at hh
  exact Nat.lt_irrefl _ hh

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). A successful path crossing a saved caller boundary first returns with the saved caller locals. -/
theorem FirstStepsN.split_return {P : Program O} {bound : Nat}
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
    cases protected_step hp _ _ hs with
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

/-- A call traps on its existing continuation or protects the caller boundary. -/
def SavesCaller (R : Config O → Label O → Config O → Prop) (L : AList Var O.Val)
    (st : O.Store) (q : List (Frame O)) (d : Deferred O) : Prop :=
  ∀ label c', R ⟨.exec d.stmt, L, st, q⟩ label c' →
    (∃ kd site, c'.ctrl = .trapped kd site ∧ c'.frames = q) ∨ Protected L q c'

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). A call either traps without changing its continuation, or saves the caller boundary. -/
theorem call_protects {P : Program O} {L : AList Var O.Val} {st : O.Store}
    {q : List (Frame O)} {d : Deferred O} :
    SavesCaller (Step P) L st q d := by
  intro label c' h
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

/-- Helper (inversion). Extracts the endpoint predicate from the first-arrival certificate (approved: APPR-C1-codex-5531d9e34). -/
private theorem FirstStepsN.end_stop_inv {P : Program O} {stop : Config O → Prop}
    {c c' : Config O} {tr : List O.Event} {n : Nat} (h : FirstStepsN (Step P) stop c tr c' n) : stop c' := by
  induction h with
  | stop hs => exact hs
  | step _ _ _ ih => exact ih

/-- Helper (inversion). Combines the retained trapped-run law with the endpoint bound; no additional trap semantics (approved: APPR-C1-codex-5531d9e34). -/
private theorem FirstStepsN.trap_impossible_inv {P : Program O} {bound : Nat}
    {kd : TrapKind} {site : SiteId} {L : AList Var O.Val} {st : O.Store}
    {q : List (Frame O)} {tr : List O.Event} {endC : Config O} {n : Nat}
    (hq : bound < q.length)
    (h : FirstStepsN (Step P) (fun c => c.frames.length ≤ bound) ⟨.trapped kd site, L, st, q⟩ tr endC n) :
    False := by
  have he := ((steps_from_trapped P kd site L st q tr endC).mp h.unsized_inv.steps_inv).2
  have hs := h.end_stop_inv
  rw [he] at hs
  exact Nat.not_le_of_gt hq hs

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Every successful deferred call path crossing its continuation first returns with its caller locals restored. -/
theorem FirstStepsN.call_return {P : Program O} {bound : Nat}
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
    cases call_protects _ _ hs with
    | inl hp =>
      obtain ⟨kd, site, hc, hf⟩ := hp
      have he : c' = ⟨.trapped kd site, c'.locals, c'.store, q⟩ := by cases c'; simp_all
      rw [he] at ht
      exact False.elim (FirstStepsN.trap_impossible_inv hq ht)
    | inr hp =>
      obtain ⟨n₁, n₂, tr₁, st₁, tr₂, hc, he, ha, hb⟩ := ht.split_return hq hp
      exact ⟨n₁ + 1, n₂, l.events ++ tr₁, st₁, tr₂, by omega,
        by simp [he, List.append_assoc], .step (by intro h; cases h.1) hs ha, hb⟩

/-- An exit dispatcher itself contributes no event. -/
def ExitSilent (R : Config O → Label O → Config O → Prop) (e : Exit O.Val)
    (L : AList Var O.Val) (st : O.Store) (frames : List (Frame O)) : Prop :=
  ∀ label c, R ⟨.exiting e, L, st, frames⟩ label c → label = .silent

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Unwinding itself emits no event; effects belong to dispatched calls. -/
theorem exit_label {P : Program O} {e : Exit O.Val} {L : AList Var O.Val}
    {st : O.Store} {frames : List (Frame O)} :
    ExitSilent (Step P) e L st frames := by
  intro label c h
  cases h with
  | silent _ => rfl
  | prim h _ _ => exact False.elim (stepExit_no_request_inv L st e frames _ _ _ _ _ h)

/-- Helper (computation). Computes the resume dispatcher result to convert its representation inside raw-run completeness (approved: APPR-C1-codex-5531d9e34). -/
private theorem resume_step_inv {P : Program O} {e : Exit O.Val} {L : AList Var O.Val}
    {st : O.Store} {frames : List (Frame O)} {label : Label O} {c : Config O}
    (h : Step P ⟨.done, L, st, .resume (.exit e) :: frames⟩ label c) :
    label = .silent ∧ c = ⟨.exiting e, L, st, frames⟩ := by
  cases h with
  | silent h =>
    simp only [step, Option.some.injEq, Out.next.injEq] at h
    exact ⟨rfl, h.symm⟩
  | prim h _ _ => simp [step] at h

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Raw first-arrival cleanup executions admit a complete dispatch segmentation. -/
theorem boundary_complete {P : Program O} {e : Exit O.Val} {k pre : List (Frame O)}
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
        have hl := exit_label _ _ hs
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
          exact ⟨leaving (f :: pre) L L' ++ acts,
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
              ht.call_return hq
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

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Every raw first-boundary unwind through
passable frames has a segmented `BoundaryRun` observation that runs `planWith` pre L, restores
Lend = `unwindLocals` pre L, and performs every planned call and drop in order. -/
theorem raw_cleanup_runs_plan (P : Program O) (e : Exit O.Val) (k pre : List (Frame O))
    (L Lend : AList Var O.Val) (st stEnd : O.Store) (tr : List O.Event)
    (hp : ∀ (f : Frame O), f ∈ pre → Passes e f)
    (h : FirstSteps (Step P) (fun c => c.frames.length ≤ k.length)
      ⟨.exiting e, L, st, pre ++ k⟩ tr ⟨.exiting e, Lend, stEnd, k⟩) :
    ∃ acts, BoundaryRun P (Step P) e k pre L st acts tr Lend stEnd ∧
      acts = planWith pre L ∧ Lend = unwindLocals pre L ∧ Performed P acts L st tr Lend stEnd := by
  obtain ⟨n, hn⟩ := h.sized_inv
  obtain ⟨acts, ha⟩ := boundary_complete hn rfl hp
  obtain ⟨_, hacts, hL, hperf⟩ :=
    (cleanup_runs_exact_plan P e k pre L Lend st stEnd acts tr).mp ha
  exact ⟨acts, ha, hacts, hL, hperf⟩


/-- Law (subject `ConcreteSpec.Core.Exec.Steps`). From `Stmt.abort` the only reachable configurations
are the initial `Stmt.abort` configuration and the `Ctrl.trapped` `TrapKind.abort` site configuration, both with empty
event trace and unchanged locals, store, and frame stack. -/
theorem abort_runs_no_cleanup (P : Program O) (site : SiteId) (L : AList Var O.Val)
    (st : O.Store) (k : List (Frame O)) (tr : List O.Event) (c : Config O) :
    Steps P ⟨.exec (.abort site), L, st, k⟩ tr c ↔
      tr = [] ∧ (c = ⟨.exec (.abort site), L, st, k⟩ ∨ c = ⟨.trapped .abort site, L, st, k⟩) := by
  constructor
  · intro h
    cases h with
    | refl => exact ⟨rfl, .inl rfl⟩
    | step hs hrest =>
      cases hs with
      | silent h =>
        simp only [step, Option.some.injEq, Out.next.injEq] at h
        subst h
        have := (steps_from_trapped P .abort site L st k _ c).mp hrest
        exact ⟨by simpa [Label.events] using this.1, .inr this.2⟩
      | prim h _ _ => simp [step] at h
  · rintro ⟨rfl, rfl | rfl⟩
    · exact .refl _
    · exact steps_next rfl (.refl _)

/-! ### Evaluation order and determinism -/

/-- Law (subject `ConcreteSpec.Core.Exec.evalArgs`). The first argument that traps decides the trap,
whatever follows it: once the arguments before it evaluate, in whatever state they leave. -/
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

/-- Helper (congruence). Function injectivity on `step` (approved: APPR-C1-codex-5531d9e34). -/
private theorem step_silent_deterministic_inv {P : Program O} {c c₁ c₂ : Config O}
    (h₁ : step P c = some (.next c₁)) (h₂ : step P c = some (.next c₂)) : c₁ = c₂ := by
  rw [h₁] at h₂; cases h₂; rfl

/-- Representation lemma. Function/relation bridge, not determinism evidence (approved: APPR-C1-codex-5531d9e34). When the step function returns `Out.next` c₁, a
relational `Step` from c occurs if and only if the label is `Label.silent` and the target is c₁. -/
theorem step_silent_representation (P : Program O) (c c₁ c₂ : Config O) (l : Label O)
    (h : step P c = some (.next c₁)) :
    Step P c l c₂ ↔ l = .silent ∧ c₂ = c₁ := by
  constructor
  · intro hs
    cases hs with
    | silent h' => exact ⟨rfl, step_silent_deterministic_inv h' h⟩
    | prim h' _ _ => rw [h] at h'; cases h'
  · rintro ⟨rfl, rfl⟩
    exact .silent h

/-- Helper (inversion). The function-backed representation of `Step` separates machine steps
from answers (approved: APPR-C1-codex-5531d9e34). -/
private theorem step_deterministic_inv {P : Program O} {c c₁ c₂ : Config O} {l₁ l₂ : Label O}
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

/-- Representation lemma. Constructor decomposition, not determinism evidence (approved: APPR-C1-codex-5531d9e34). Two relational transitions from c occur if and
only if either they come from the same silent `Out.next` step or both arise from the same `Out.request`
by allowed oracle answers whose continuations succeed. -/
theorem step_pair_representation (P : Program O) (c c₁ c₂ : Config O) (l₁ l₂ : Label O) :
    (Step P c l₁ c₁ ∧ Step P c l₂ c₂) ↔
      (step P c = some (.next c₁) ∧ l₁ = .silent ∧ l₂ = .silent ∧ c₁ = c₂) ∨
      ∃ p vs site st k a₁ a₂, step P c = some (.request p vs site st k) ∧
        O.allows p vs st a₁ = true ∧ O.allows p vs st a₂ = true ∧
        l₁ = a₁.label ∧ l₂ = a₂.label ∧ k a₁ = some c₁ ∧ k a₂ = some c₂ := by
  constructor
  · rintro ⟨h₁, h₂⟩
    cases h₁ with
    | silent h =>
      cases h₂ with
      | silent h' => exact .inl ⟨h, rfl, rfl, step_silent_deterministic_inv h h'⟩
      | prim h' _ _ => rw [h] at h'; cases h'
    | prim h ha hk =>
      cases h₂ with
      | silent h' => rw [h] at h'; cases h'
      | prim h' ha' hk' =>
        rw [h] at h'; cases h'
        exact .inr ⟨_, _, _, _, _, _, _, h, ha, ha', rfl, rfl, hk, hk'⟩
  · rintro (⟨h, rfl, rfl, rfl⟩ | ⟨p, vs, site, st, k, a₁, a₂, h, ha₁, ha₂, rfl, rfl, hk₁, hk₂⟩)
    · exact ⟨.silent h, .silent h⟩
    · exact ⟨.prim h ha₁ hk₁, .prim h ha₂ hk₂⟩

/-- A transition with its external input retained. `none` denotes an internal machine
step; `some a` denotes precisely answer `a`, even when that answer emits no event. This
relation does not identify different answers merely because their observable labels coincide. -/
inductive AnswerStep (P : Program O) : Config O → Option (Answer O.Val O.Event O.Store) →
    Label O → Config O → Prop where
  | silent {c c'} : step P c = some (.next c') → AnswerStep P c none .silent c'
  | prim {c c' p vs site st k a} : step P c = some (.request p vs site st k) →
      O.allows p vs st a = true → k a = some c' → AnswerStep P c (some a) a.label c'

/-- Representation lemma. Erases the input index to recover the observable transition
(approved: APPR-C1-codex-5531d9e34). -/
theorem AnswerStep.step_inv {P : Program O} {c c' : Config O}
    {a : Option (Answer O.Val O.Event O.Store)} {l : Label O}
    (h : AnswerStep P c a l c') : Step P c l c' := by
  cases h with
  | silent h => exact .silent h
  | prim h ha hk => exact .prim h ha hk

/-- Uniqueness of the observation and destination for a fixed machine input. The alternate
transition is quantified inside this property, so it remains part of the conclusion, not an
assumption selecting the expected answer or result. -/
def InputDeterministic
    (R : Program O → Config O → Option (Answer O.Val O.Event O.Store) → Label O → Config O → Prop)
    (P : Program O) (c c₁ : Config O) (a : Option (Answer O.Val O.Event O.Store)) (l₁ : Label O) : Prop :=
  ∀ l₂ c₂, R P c a l₂ c₂ → l₁ = l₂ ∧ c₁ = c₂

/-- Representation lemma. Function-backed internal transitions compare equal dispatcher
results; this is not independent determinism evidence (approved: PENDING). -/
theorem step_silent_deterministic (P : Program O) (c c₁ : Config O) (l₁ : Label O)
    (h₁ : AnswerStep P c none l₁ c₁) :
    InputDeterministic AnswerStep P c c₁ none l₁ := by
  intro l₂ c₂ h₂
  cases h₁ with
  | silent h₁ =>
    cases h₂ with
    | silent h₂ => exact ⟨rfl, step_silent_deterministic_inv h₁ h₂⟩

/-- Representation lemma. Function-backed transitions at a fixed answer compare equal
dispatcher and continuation results; this is not soundness evidence (approved: PENDING). -/
theorem step_deterministic (P : Program O) (c c₁ : Config O)
    (a : Option (Answer O.Val O.Event O.Store)) (l₁ : Label O)
    (h₁ : AnswerStep P c a l₁ c₁) :
    InputDeterministic AnswerStep P c c₁ a l₁ := by
  intro l₂ c₂ h₂
  cases h₁ with
  | silent h₁ =>
    cases h₂ with
    | silent h₂ => exact ⟨rfl, step_silent_deterministic_inv h₁ h₂⟩
  | prim h₁ _ hk₁ =>
    cases h₂ with
    | prim h₂ _ hk₂ =>
      rw [h₁] at h₂
      cases h₂
      rw [hk₁] at hk₂
      cases hk₂
      exact ⟨rfl, rfl⟩

/-! ### Independent operational semantics and correspondence -/

/-- Representation lemma. Argument judgment and left-to-right executable argument traversal
have the same results; proved by list/result cases (approved: PENDING). -/
theorem arguments_representation (L : AList Var O.Val) (st : O.Store) (es : List O.Expr)
    (r : Res (List O.Val × O.Store)) : Arguments O L st es r ↔ evalArgs O L st es = r := by
  constructor
  · intro h
    induction h with
    | nil => rfl
    | cons h _ ih => simp [evalArgs, h, Res.bind, Res.map, ih]
    | headStuck h => simp [evalArgs, h, Res.bind]
    | tailStuck h _ ih => simp [evalArgs, h, Res.bind, Res.map, ih]
    | headTrap h => simp [evalArgs, h, Res.bind]
    | tailTrap h _ ih => simp [evalArgs, h, Res.bind, Res.map, ih]
  · intro h
    induction es generalizing st r with
    | nil => cases h; exact .nil
    | cons e es ih =>
      cases he : O.eval L st e with
      | stuck => simp [evalArgs, he, Res.bind] at h; subst r; exact .headStuck he
      | trap kd => simp [evalArgs, he, Res.bind] at h; subst r; exact .headTrap he
      | ok val =>
        obtain ⟨v, st₁⟩ := val
        cases hr : evalArgs O L st₁ es with
        | stuck => simp [evalArgs, he, hr, Res.bind, Res.map] at h; subst r; exact .tailStuck he (ih st₁ _ hr)
        | trap kd =>
          simp [evalArgs, he, hr, Res.bind, Res.map] at h; subst r
          exact .tailTrap he (ih st₁ _ hr)
        | ok val =>
          obtain ⟨vs, st₂⟩ := val
          simp [evalArgs, he, hr, Res.bind, Res.map] at h; subst r
          exact .cons he (ih st₁ _ hr)

/-- Representation lemma. The independently specified registration judgment agrees with the
executable stack traversal (approved: PENDING). -/
theorem registered_representation (d : Deferred O) (k k' : List (Frame O)) :
    Registered d k k' ↔ register d k = some k' := by
  constructor
  · intro h; induction h with
    | scope => rfl
    | seq _ ih => simp [register, ih]
  · intro h
    induction k generalizing k' with
    | nil => simp [register] at h
    | cons f k ih =>
      cases f <;> simp only [register] at h
      all_goals first
      | cases h; exact .scope
      | obtain ⟨j, hj, rfl⟩ := Option.map_eq_some_iff.mp h; exact .seq (ih _ hj)
      | contradiction

/-- Representation lemma. Destination admission agrees with the executable destination
and lifecycle adapters (approved: PENDING). -/
theorem destination_representation (dst : Option Var) (v : O.Val) (L L' : AList Var O.Val)
    (st st' : O.Store) : Destination O dst v L st L' st' ↔
      bindLife O dst st = some st' ∧ L' = bindDst dst v L := by
  cases dst with
  | none => constructor
            · intro h; cases h; exact ⟨rfl, rfl⟩
            · rintro ⟨h, rfl⟩; cases h; exact .discard
  | some x => constructor
              · intro h; cases h with | bind h => exact ⟨h, rfl⟩
              · rintro ⟨h, rfl⟩; exact .bind h

/-- Helper (congruence). Independent argument rules have a unique result, by induction on
argument derivations and inversion of the other derivation (approved: PENDING). -/
theorem arguments_unique_inv {L : AList Var O.Val} {st : O.Store} {es : List O.Expr}
    {r r' : Res (List O.Val × O.Store)} (h : Arguments O L st es r)
    (h' : Arguments O L st es r') : r = r' := by
  induction h generalizing r' <;> cases h' <;> simp_all <;> grind

/-- Helper (congruence). Registration is unique by stack-rule inversion (approved: PENDING). -/
theorem registered_unique_inv {d : Deferred O} {k k₁ k₂ : List (Frame O)}
    (h : Registered d k k₁) (h' : Registered d k k₂) : k₁ = k₂ := by
  induction h generalizing k₂ <;> cases h' <;> simp_all <;> grind

/-- Helper (congruence). Destination binding is unique by admission-rule inversion
(approved: PENDING). -/
theorem destination_unique_inv {dst : Option Var} {v : O.Val} {L L₁ L₂ : AList Var O.Val}
    {st st₁ st₂ : O.Store} (h : Destination O dst v L st L₁ st₁)
    (h' : Destination O dst v L st L₂ st₂) : L₁ = L₂ ∧ st₁ = st₂ := by
  cases h <;> cases h' <;> simp_all

/-- Helper (inversion). Translates each independent operational constructor into the
function-backed representation; no uniqueness argument uses this lemma (approved: PENDING). -/
theorem rules_answer_sound_inv {P : Program O} {c c' : Config O}
    {a : Option (Answer O.Val O.Event O.Store)} {l : Label O}
    (h : Rules P c a l c') : AnswerStep P c a l c' := by
  cases h
  case primOk L st k dst p args site vs st₁ st₂ a v L' he ha hv hd =>
    obtain ⟨hb, rfl⟩ := (destination_representation _ _ _ _ _ _).mp hd
    apply AnswerStep.prim (by simp only [step, onRes, (arguments_representation _ _ _ _).mp he]; rfl) ha
    simp [hv, hb]
  case primError L st k dst p args site vs st₁ a kd he ha hv =>
    apply AnswerStep.prim (by simp only [step, onRes, (arguments_representation _ _ _ _).mp he]; rfl) ha
    simp [hv]
  all_goals
    apply AnswerStep.silent
    try simp_all only [arguments_representation, registered_representation, destination_representation]
    simp_all only [step, stepExit, onRes, startCleanup, Deferred.stmt, dropOut, dead, dom,
      restrict, bindDst, bindLife]
    all_goals simp_all

/-- Law (subject `ConcreteSpec.Core.Exec.Rules`). A fixed external input determines both
label and destination by operational-rule inversion, independently of the dispatcher. -/
theorem rules_deterministic (P : Program O) (c c₁ : Config O)
    (a : Option (Answer O.Val O.Event O.Store)) (l₁ : Label O)
    (h₁ : Rules P c a l₁ c₁) : InputDeterministic Rules P c c₁ a l₁ := by
  intro l₂ c₂ h₂
  cases h₁ <;> cases h₂ <;>
    first | exact ⟨rfl, rfl⟩ | (expose_names; simp_all)
  all_goals grind only [arguments_unique_inv, registered_unique_inv, destination_unique_inv]

/-- Helper (inversion). Statement completeness, with explicit operand and admission cases
rather than a product of all dispatcher branches (approved: PENDING). -/
private theorem rules_exec_complete_inv {P : Program O} (s : Stmt O)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) {c' : Config O}
    (hs : step P ⟨.exec s, L, st, k⟩ = some (.next c')) :
    Rules P ⟨.exec s, L, st, k⟩ none .silent c' := by
  cases s with
  | skip => cases hs; exact .skip
  | seq s t => cases hs; exact .seq
  | block b s => cases hs; exact .block
  | loop l cr test body site => cases hs; exact .loop
  | brk l => cases hs; exact .brk
  | cont l => cases hs; exact .cont
  | abort site => cases hs; exact .abort
  | defer f args site =>
    simp only [step, Option.map_eq_some_iff] at hs
    obtain ⟨k', hr, h⟩ := hs
    cases h
    exact .defer ((registered_representation _ _ _).mpr hr)
  | bind x e site =>
    cases he : O.eval L st e with
    | stuck => simp [step, he, onRes] at hs
    | trap kd => simp only [step, he, onRes, Option.some.injEq, Out.next.injEq] at hs
                 cases hs; exact .bindTrap he
    | ok val =>
      obtain ⟨v, st₁⟩ := val
      simp only [step, he, onRes, Option.map_eq_some_iff] at hs
      obtain ⟨st₂, hb, h⟩ := hs
      cases h
      exact .bind he hb
  | assign x e site =>
    cases he : O.eval L st e with
    | stuck => simp [step, he, onRes] at hs
    | trap kd => simp only [step, he, onRes, Option.some.injEq, Out.next.injEq] at hs
                 cases hs; exact .assignTrap he
    | ok val =>
      obtain ⟨v, st₁⟩ := val
      simp only [step, he, onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at hs
      obtain ⟨old, hl, st₂, hb, h⟩ := hs
      cases h
      exact .assign he hl hb
  | cond e yes no site =>
    cases he : O.eval L st e with
    | stuck => simp [step, he, onRes] at hs
    | trap kd => simp only [step, he, onRes, Option.some.injEq, Out.next.injEq] at hs
                 cases hs; exact .condTrap he
    | ok val =>
      obtain ⟨v, st₁⟩ := val
      simp only [step, he, onRes, Option.map_eq_some_iff] at hs
      obtain ⟨b, ht, h⟩ := hs
      cases b <;> cases h
      · exact .condfalse he ht
      · exact .condtrue he ht
  | ret e site =>
    cases he : O.eval L st e with
    | stuck => simp [step, he, onRes] at hs
    | trap kd => simp only [step, he, onRes, Option.some.injEq, Out.next.injEq] at hs
                 cases hs; exact .retTrap he
    | ok val =>
      obtain ⟨v, st₁⟩ := val
      simp only [step, he, onRes, Option.some.injEq, Out.next.injEq] at hs
      cases hs; exact .ret he
  | call dst f args site =>
    cases he : evalArgs O L st args with
    | stuck => simp [step, he, onRes] at hs
    | trap kd =>
      simp only [step, he, onRes, Option.some.injEq, Out.next.injEq] at hs
      cases hs; exact .callTrap ((arguments_representation _ _ _ _).mpr he)
    | ok val =>
      obtain ⟨vs, st₁⟩ := val
      simp only [step, he, onRes, Option.bind_eq_some_iff] at hs
      obtain ⟨fn, hl, hs⟩ := hs
      split at hs
      next har =>
        obtain ⟨st₂, hb, h⟩ := Option.map_eq_some_iff.mp hs
        cases h
        exact .call ((arguments_representation _ _ _ _).mpr he) hl har hb
      next => contradiction
  | prim dst p args site =>
    cases he : evalArgs O L st args with
    | stuck => simp [step, he, onRes] at hs
    | trap kd =>
      simp only [step, he, onRes, Option.some.injEq, Out.next.injEq] at hs
      cases hs; exact .primTrap ((arguments_representation _ _ _ _).mpr he)
    | ok val => simp [step, he, onRes] at hs

/-- Helper (inversion). Normal-completion completeness by frame and operand-result cases
(approved: PENDING). -/
private theorem rules_done_complete_inv {P : Program O}
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) {c' : Config O}
    (hs : step P ⟨.done, L, st, k⟩ = some (.next c')) :
    Rules P ⟨.done, L, st, k⟩ none .silent c' := by
  cases k with
  | nil => simp [step] at hs
  | cons f k =>
    cases f with
    | seq t => cases hs; exact .doneSeq
    | scope b dm ds =>
      cases ds with
      | cons d ds => cases hs; exact .doneCleanup
      | nil =>
        obtain ⟨st₁, hd, h⟩ := Option.map_eq_some_iff.mp hs
        cases h; exact .doneScope hd
    | test l cr test body site dm =>
      cases he : O.eval L st test with
      | stuck => simp [step, he, onRes] at hs
      | trap kd =>
        simp only [step, he, onRes, Option.some.injEq, Out.next.injEq] at hs
        cases hs; exact .testTrap he
      | ok val =>
        obtain ⟨v, st₁⟩ := val
        simp only [step, he, onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at hs
        obtain ⟨b, ht, st₂, hd, h⟩ := hs
        cases b <;> cases h
        · exact .testfalse he ht hd
        · exact .testtrue he ht hd
    | body l cr test body site => cases hs; exact .doneBody
    | resume pending =>
      cases pending with
      | fall => cases hs; exact .resumeFall
      | exit e => cases hs; exact .resumeExit
    | ret dst caller =>
      simp only [step, Option.bind_eq_some_iff, Option.map_eq_some_iff] at hs
      obtain ⟨st₁, hl, st₂, hb, h⟩ := hs
      cases h
      exact .implicitReturn hl ((destination_representation _ _ _ _ _ _).mpr ⟨hb, rfl⟩)

/-- Helper (inversion). Exit completeness by frame and, where inspected, exit kind
(approved: PENDING). -/
private theorem rules_exit_complete_inv {P : Program O}
    (e : Exit O.Val) (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) {c' : Config O}
    (hs : step P ⟨.exiting e, L, st, k⟩ = some (.next c')) :
    Rules P ⟨.exiting e, L, st, k⟩ none .silent c' := by
  cases k with
  | nil => simp [step, stepExit] at hs
  | cons f k =>
    cases f with
    | seq t => cases hs; exact .exitSeq
    | scope b dm ds =>
      cases ds with
      | cons d ds => cases hs; exact .exitCleanup
      | nil =>
        obtain ⟨st₁, hd, h⟩ := Option.map_eq_some_iff.mp hs
        cases h; exact .exitScope hd
    | test l cr test body site dm =>
      obtain ⟨st₁, hd, h⟩ := Option.map_eq_some_iff.mp hs
      cases h; exact .exitTest hd
    | body l cr test body site =>
      cases e with
      | ret v => cases hs; exact .returnBody
      | brk l' =>
        by_cases hl : l' = l
        · subst l'; simp only [step, stepExit, ↓reduceIte, Option.some.injEq, Out.next.injEq] at hs
          cases hs; exact .brkHere
        · simp only [step, stepExit, hl, ↓reduceIte, Option.some.injEq, Out.next.injEq] at hs
          cases hs; exact .brkOuter hl
      | cont l' =>
        by_cases hl : l' = l
        · subst l'; simp only [step, stepExit, ↓reduceIte, Option.some.injEq, Out.next.injEq] at hs
          cases hs; exact .contHere
        · simp only [step, stepExit, hl, ↓reduceIte, Option.some.injEq, Out.next.injEq] at hs
          cases hs; exact .contOuter hl
    | resume pending => simp [step, stepExit] at hs
    | ret dst caller =>
      cases e with
      | brk l => simp [step, stepExit] at hs
      | cont l => simp [step, stepExit] at hs
      | ret v =>
        simp only [step, stepExit, Option.bind_eq_some_iff, Option.map_eq_some_iff] at hs
        obtain ⟨st₁, hl, st₂, hb, h⟩ := hs
        cases h
        exact .explicitReturn hl ((destination_representation _ _ _ _ _ _).mpr ⟨hb, rfl⟩)

/-- Helper (inversion). Silent completeness reduces to statement and frame inversion
(approved: PENDING). -/
theorem rules_silent_complete_inv {P : Program O} {c c' : Config O}
    (hs : step P c = some (.next c')) : Rules P c none .silent c' := by
  obtain ⟨ctrl, L, st, k⟩ := c
  cases ctrl with
  | exec s => exact rules_exec_complete_inv s L st k hs
  | done => exact rules_done_complete_inv L st k hs
  | exiting e => exact rules_exit_complete_inv e L st k hs
  | trapped kd site => simp [step] at hs

/-- Helper (inversion). Every successful request continuation is a primitive operational
transition; other statement and frame forms cannot request (approved: PENDING). -/
private theorem rules_request_complete_inv {P : Program O} {c c' : Config O}
    {p : O.Prim} {vs : List O.Val} {site : SiteId} {st : O.Store}
    {cont : Answer O.Val O.Event O.Store → Option (Config O)}
    {a : Answer O.Val O.Event O.Store}
    (hs : step P c = some (.request p vs site st cont))
    (ha : O.allows p vs st a = true) (hk : cont a = some c') :
    Rules P c (some a) a.label c' := by
  obtain ⟨ctrl, L, st₀, k⟩ := c
  cases ctrl with
  | trapped kd site => simp [step] at hs
  | exiting e => exact False.elim (stepExit_no_request_inv L st₀ e k _ _ _ _ _ hs)
  | done =>
    cases k with
    | nil => simp [step] at hs
    | cons f k =>
      cases f <;> simp only [step, onRes, startCleanup, Option.bind, Option.map] at hs <;>
        repeat' first | simp_all [bindLife] | split at hs
  | exec stmt =>
    cases stmt with
    | prim dst p₀ args site₀ =>
      cases he : evalArgs O L st₀ args with
      | stuck => simp [step, onRes, he] at hs
      | trap kd => simp [step, onRes, he] at hs
      | ok val =>
        obtain ⟨vs₀, st₁⟩ := val
        simp only [step, onRes, he, Option.some.injEq, Out.request.injEq] at hs
        obtain ⟨rfl, rfl, rfl, rfl, rfl⟩ := hs
        cases hr : a.result with
        | error kd =>
          simp only [hr, Option.some.injEq] at hk
          cases hk
          exact .primError ((arguments_representation _ _ _ _).mpr he) ha hr
        | ok v =>
          simp only [hr, Option.map_eq_some_iff] at hk
          obtain ⟨st₂, hb, h⟩ := hk
          cases h
          exact .primOk ((arguments_representation _ _ _ _).mpr he) ha hr
            ((destination_representation _ _ _ _ _ _).mpr ⟨hb, rfl⟩)
    | _ =>
      simp only [step, onRes, Option.bind, Option.map] at hs
      repeat' first | simp_all | split at hs

/-- A dispatcher's answer-indexed observable transition. This adapter exposes the operation
as a parameter so correspondence laws can independently vary the executable implementation. -/
def Dispatches (dispatch : Program O → Config O → Option (Out O)) (P : Program O)
    (c : Config O) (a : Option (Answer O.Val O.Event O.Store)) (l : Label O)
    (c' : Config O) : Prop :=
  match a with
  | none => l = .silent ∧ dispatch P c = some (.next c')
  | some a => ∃ p vs site st cont, dispatch P c = some (.request p vs site st cont) ∧
      O.allows p vs st a = true ∧ cont a = some c' ∧ l = a.label

/-- Law (subject `ConcreteSpec.Core.Exec.step`). Every independent operational transition
is implemented by the dispatcher, including the exact answer, label and destination. -/
theorem rules_sound (P : Program O) (c : Config O)
    (a : Option (Answer O.Val O.Event O.Store)) (l : Label O) (c' : Config O)
    (h : Rules P c a l c') : Dispatches step P c a l c' := by
  have hs := rules_answer_sound_inv h
  cases hs with
  | silent hs => exact ⟨rfl, hs⟩
  | prim hs ha hk => exact ⟨_, _, _, _, _, hs, ha, hk, rfl⟩

/-- Completeness at a fixed source and input: every implementation outcome has an
independent operational derivation. Quantifying outcomes inside the property permits a
premise-preserving mutant to exhibit an extra implementation transition. -/
def DispatchComplete (dispatch : Program O → Config O → Option (Out O))
    (P : Program O) (c : Config O) (a : Option (Answer O.Val O.Event O.Store)) : Prop :=
  ∀ l c', Dispatches dispatch P c a l c' → Rules P c a l c'

/-- Law (subject `ConcreteSpec.Core.Exec.step`). Every admitted dispatcher transition has
an independent operational derivation; no executable transition is omitted. -/
theorem rules_complete (P : Program O) (c : Config O)
    (a : Option (Answer O.Val O.Event O.Store)) : DispatchComplete step P c a := by
  intro l c' h
  cases a with
  | none => obtain ⟨rfl, hs⟩ := h; exact rules_silent_complete_inv hs
  | some a =>
    obtain ⟨p, vs, site, st, cont, hs, ha, hk, rfl⟩ := h
    exact rules_request_complete_inv hs ha hk

/-- Representation lemma. Exact correspondence with the answer-retaining relation follows
from dispatcher soundness and completeness (approved: PENDING). -/
theorem rules_iff_answerStep (P : Program O) (c : Config O)
    (a : Option (Answer O.Val O.Event O.Store)) (l : Label O) (c' : Config O) :
    Rules P c a l c' ↔ AnswerStep P c a l c' := by
  constructor
  · exact rules_answer_sound_inv
  · intro h
    apply rules_complete P c a l c'
    cases h with
    | silent hs => exact ⟨rfl, hs⟩
    | prim hs ha hk => exact ⟨_, _, _, _, _, hs, ha, hk, rfl⟩

/-- Representation lemma. Erasing exactly the answer index gives the original observable
single-step relation (approved: PENDING). -/
theorem step_iff_rules (P : Program O) (c : Config O) (l : Label O) (c' : Config O) :
    Step P c l c' ↔ ∃ a, Rules P c a l c' := by
  constructor
  · intro h
    cases h with
    | silent hs => exact ⟨none, rules_silent_complete_inv hs⟩
    | prim hs ha hk => exact ⟨_, rules_request_complete_inv hs ha hk⟩
  · rintro ⟨a, h⟩
    exact (rules_answer_sound_inv h).step_inv

/-- Law (subject `ConcreteSpec.Core.Exec.Rules`). Internal steps have a unique label and
configuration, by independent rule inversion. -/
theorem rules_silent_deterministic (P : Program O) (c c₁ : Config O) (l₁ : Label O)
    (h₁ : Rules P c none l₁ c₁) : InputDeterministic Rules P c c₁ none l₁ := by
  exact rules_deterministic P c c₁ none l₁ h₁

/-! ### Lifecycle refusal

No law assumes that `life` is total. `step_return_caller` and `return_reaches_caller`
now require successful leave and result binding; `exit_scope_runs_defers` and
`done_scope_runs_defers` require a successful drop. `Cleans` and `RunsEach` carry
successful drops, so their existing plan and unwinding laws keep the same meaning.
`steps_request` requires successful continuation, and `step_deterministic` records
those successful continuations explicitly. All other laws retain their premises.
-/

/-- Helper (inversion). For a silent lifecycle operation, absence of all transitions is exactly
refusal (approved: APPR-C1-codex-5531d9e34). -/
private theorem blocked_map_inv {P : Program O} {c : Config O} (r : Option O.Store)
    (f : O.Store → Config O) (h : step P c = r.map (fun st => .next (f st))) :
    Blocked (Step P) c ↔ r = none := by
  cases hr : r with
  | none => exact ⟨fun _ => rfl, fun _ => no_step_of_none_inv (by simpa [hr] using h)⟩
  | some st =>
    constructor
    · intro hb
      exact False.elim (hb .silent (f st) (.silent (by simpa [hr] using h)))
    · intro hn; cases hn

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Initialization is the sole obstruction after
successful operand evaluation: refusal is necessary and sufficient for the absence of every
labeled transition. -/
theorem bind_refused (P : Program O) (x : Var) (e : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st st' : O.Store) (v : O.Val) (k : List (Frame O))
    (he : O.eval L st e = .ok (v, st')) :
    Blocked (Step P) ⟨.exec (.bind x e site), L, st, k⟩ ↔ O.life (.bind x) st' = none :=
  blocked_map_inv _ (fun st => ⟨.done, (x, v) :: L, st, k⟩) (by simp [step, onRes, he])

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). For an existing destination and successful RHS,
only lifecycle refusal blocks assignment. -/
theorem assign_refused (P : Program O) (x : Var) (e : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st st' : O.Store) (v old : O.Val) (k : List (Frame O))
    (he : O.eval L st e = .ok (v, st')) (hx : AList.lookup L x = some old) :
    Blocked (Step P) ⟨.exec (.assign x e site), L, st, k⟩ ↔ O.life (.bind x) st' = none :=
  blocked_map_inv _ (fun st => ⟨.done, AList.update L x (fun _ => v), st, k⟩)
    (by simp [step, onRes, he, hx])

/-- Law (subject `ConcreteSpec.Core.Exec.runFn`). Top-level function entry from matching
arguments is refused if and only if the `Life.enter` lifecycle hook refuses the parameter list. -/
theorem enter_refused (P : Program O) (ρ : Oracle O) (fuel : Nat) (fn : Fn O)
    (vs : List O.Val) (st : O.Store) (hlen : fn.params.length = vs.length) :
    runFn P ρ fuel fn vs st = none ↔ O.life (.enter (fn.params.map Prod.fst)) st = none := by
  simp [runFn, enter, hlen]

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). After successful argument evaluation, function
lookup, and arity check (fn.params.length = vs.length, which `step` verifies before calling
`enter`), activation refusal is necessary and sufficient to block a call. -/
theorem call_refused (P : Program O) (dst : Option Var) (f : FnId) (args : List O.Expr)
    (site : SiteId) (L : AList Var O.Val) (st st' : O.Store) (vs : List O.Val)
    (fn : Fn O) (k : List (Frame O)) (he : evalArgs O L st args = .ok (vs, st'))
    (hf : AList.lookup P.fns f = some fn) (hlen : fn.params.length = vs.length) :
    Blocked (Step P) ⟨.exec (.call dst f args site), L, st, k⟩ ↔
      O.life (.enter (fn.params.map Prod.fst)) st' = none :=
  blocked_map_inv _ (fun st => ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, st,
    .scope fn.block (fn.params.map Prod.fst) [] :: .ret dst L :: k⟩)
    (by simp [step, onRes, he, hf, hlen])

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). A normal scope end with no remaining calls is
blocked if and only if dropping its locals refuses. -/
theorem scope_drop_refused (P : Program O) (b : BlockId) (dm : List Var)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    Blocked (Step P) ⟨.done, L, st, .scope b dm [] :: k⟩ ↔ dropOut O dm L st = none :=
  blocked_map_inv _ _ rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). A structured exit through an empty scope is
blocked if and only if its drop refuses. -/
theorem exit_drop_refused (P : Program O) (e : Exit O.Val) (b : BlockId) (dm : List Var)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    Blocked (Step P) ⟨.exiting e, L, st, .scope b dm [] :: k⟩ ↔ dropOut O dm L st = none :=
  blocked_map_inv _ _ rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). The test-frame drop during an exit is necessary
and sufficient for progress. -/
theorem exit_test_drop_refused (P : Program O) (e : Exit O.Val) (dm : List Var)
    (l : LoopId) (cr body : Stmt O) (t : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    Blocked (Step P) ⟨.exiting e, L, st, .test l cr t body site dm :: k⟩ ↔ dropOut O dm L st = none :=
  blocked_map_inv _ _ rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). After a successful boolean test, either branch
progresses if and only if its temporary drop succeeds. -/
theorem test_drop_refused (P : Program O) (l : LoopId) (cr body : Stmt O) (t : O.Expr)
    (site : SiteId) (dm : List Var) (L : AList Var O.Val) (st st' : O.Store)
    (v : O.Val) (go : Bool) (k : List (Frame O)) (he : O.eval L st t = .ok (v, st'))
    (ht : O.test v = some go) :
    Blocked (Step P) ⟨.done, L, st, .test l cr t body site dm :: k⟩ ↔ dropOut O dm L st' = none := by
  apply blocked_map_inv _ (fun st => if go then
    ⟨.exec body, restrict dm L, st, .body l cr t body site :: k⟩ else
    ⟨.done, restrict dm L, st, k⟩)
  cases go <;> simp [step, onRes, he, ht]

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Explicit returns and implicit Unit returns are
blocked if and only if activation end or result initialization refuses. -/
theorem return_refused (P : Program O) (dst : Option Var) (v : O.Val)
    (caller L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    (Blocked (Step P) ⟨.exiting (.ret v), L, st, .ret dst caller :: k⟩ ↔
      (O.life .leave st).bind (bindLife O dst) = none) ∧
    (Blocked (Step P) ⟨.done, L, st, .ret dst caller :: k⟩ ↔
      (O.life .leave st).bind (bindLife O dst) = none) := by
  constructor
  · apply blocked_map_inv _ (fun st => ⟨.done, bindDst dst v caller, st, k⟩)
    cases hl : O.life .leave st <;> simp [step, stepExit, hl]
  · apply blocked_map_inv _ (fun st => ⟨.done, bindDst dst O.unitVal caller, st, k⟩)
    cases hl : O.life .leave st <;> simp [step, hl]

/-- Helper (inversion). A zero-fuel run never reports `Ending.stuck` (approved: APPR-C1-codex-5531d9e34). -/
private theorem runFrom_zero_ne_stuck_inv (P : Program O) (ρ : Oracle O)
    (hist' hist : List O.Event) (c' c : Config O) :
    runFrom P ρ 0 hist' c' ≠ ⟨hist, .stuck c⟩ := by
  intro h
  cases hf : c'.final? <;> simp [runFrom, hf] at h

/-- Law (subject `ConcreteSpec.Core.Exec.runFrom`). At a primitive request with an allowed
oracle answer, a one-step run is stuck at the requesting configuration with unchanged history
if and only if the answer continuation refuses. -/
theorem continuation_refused (P : Program O) (ρ : Oracle O)
    (hist : List O.Event) (c : Config O) (p : O.Prim) (vs : List O.Val)
    (site : SiteId) (st : O.Store) (cont : Answer O.Val O.Event O.Store → Option (Config O))
    (a : Answer O.Val O.Event O.Store) (hf : c.final? = none)
    (hr : step P c = some (.request p vs site st cont))
    (ho : ρ hist p vs site st = some a) (ha : O.allows p vs st a = true) :
    runFrom P ρ 1 hist c = ⟨hist, .stuck c⟩ ↔ cont a = none := by
  cases hk : cont a with
  | none =>
    simp [runFrom, hf, hr, ho, ha, hk]
  | some c' =>
    constructor
    · intro h
      simp only [runFrom, hf, hr, ho, ha, ↓reduceIte, hk] at h
      exact False.elim (runFrom_zero_ne_stuck_inv P ρ (hist ++ a.event.toList) hist c' c h)
    · intro h; cases h

/-- Law (subject `ConcreteSpec.Core.Exec.runFrom`). A one-step run from a primitive call with a
destination variable and an allowed successful oracle answer is stuck at the requesting
configuration if and only if binding the destination variable is refused by `Operands.life`. -/
theorem prim_bind_refused (P : Program O) (ρ : Oracle O) (hist : List O.Event)
    (x : Var) (p : O.Prim) (args : List O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st st' : O.Store) (vs : List O.Val) (k : List (Frame O))
    (a : Answer O.Val O.Event O.Store) (v : O.Val)
    (he : evalArgs O L st args = .ok (vs, st'))
    (ho : ρ hist p vs site st' = some a) (ha : O.allows p vs st' a = true)
    (hv : a.result = .ok v) :
    runFrom P ρ 1 hist ⟨.exec (.prim (some x) p args site), L, st, k⟩ =
      ⟨hist, .stuck ⟨.exec (.prim (some x) p args site), L, st, k⟩⟩ ↔
      O.life (.bind x) a.store = none := by
  rw [continuation_refused P ρ hist _ p vs site st' _ a rfl (by simp [step, onRes, he]; rfl) ho ha]
  simp [hv, bindLife]

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Relational primitive-bind refusal: after
successful argument evaluation, a primitive call with destination `some` x is blocked in `Step`
if and only if every allowed answer succeeds with a value whose `Life.bind` x lifecycle hook refuses. -/
theorem prim_bind_step (P : Program O) (x : Var) (p : O.Prim) (args : List O.Expr)
    (site : SiteId) (L : AList Var O.Val) (st st' : O.Store) (vs : List O.Val)
    (k : List (Frame O)) (he : evalArgs O L st args = .ok (vs, st')) :
    Blocked (Step P) ⟨.exec (.prim (some x) p args site), L, st, k⟩ ↔
      ∀ a, O.allows p vs st' a = true → ∃ v, a.result = .ok v ∧ O.life (.bind x) a.store = none := by
  constructor
  · intro hb a ha
    cases hr : a.result with
    | error kd =>
      have hs : Step P ⟨.exec (.prim (some x) p args site), L, st, k⟩ a.label
          ⟨.trapped kd site, L, a.store, k⟩ :=
        Step.prim (by simp only [step, onRes, he]; rfl) ha (by simp [hr])
      exact False.elim (hb a.label _ hs)
    | ok v =>
      refine ⟨v, rfl, ?_⟩
      cases hl : O.life (.bind x) a.store with
      | none => rfl
      | some stBind =>
        have hs : Step P ⟨.exec (.prim (some x) p args site), L, st, k⟩ a.label
            ⟨.done, (x, v) :: L, stBind, k⟩ :=
          Step.prim (by simp only [step, onRes, he]; rfl) ha (by simp [hr, bindLife, hl, bindDst])
        exact False.elim (hb a.label _ hs)
  · intro Hall l c' hs
    cases hs with
    | silent h => simp [step, onRes, he] at h
    | prim h ha hk =>
      simp only [step, onRes, he, Option.some.injEq, Out.request.injEq] at h
      obtain ⟨rfl, hp, rfl, hst, hcont⟩ := h
      cases hp; cases hst; cases hcont
      obtain ⟨v, hv, hl⟩ := Hall _ ha
      simp [hv, bindLife, hl] at hk

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Error answers bypass result initialization even
when that initialization would refuse: the actual labeled transition traps, retaining the
answer's store and event. -/
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

/-- Helper (projection). Extracts the protocol admission obligation from `DestroyProtocol`
(approved: APPR-C1-codex-5531d9e34). -/
private theorem destroy_not_copy_proj {P : Program O} (D : DestroyProtocol P) (T : O.Ty) (f : FnId)
    (h : D.impl T = some f) : O.isCopy T = false := D.notCopy T f h

/-- Representation lemma. Characterizes the validator checks, not admission soundness
(approved: APPR-C1-codex-5531d9e34). The executable destroy-function
validator accepts (T, f) if and only if T is non-Copy (`Operands.isCopy` T = false) and f is in the
program table taking a single parameter of type T and returning `Operands.unitTy`. -/
theorem destroy_not_copy [DecidableEq O.Ty] (P : Program O) (T : O.Ty) (f : FnId) :
    validDestroyFn P T f = true ↔
      O.isCopy T = false ∧ ∃ fn x, AList.lookup P.fns f = some fn ∧
        fn.params = [(x, T)] ∧ fn.result = O.unitTy := by
  constructor
  · intro h
    unfold validDestroyFn at h
    cases hc : O.isCopy T with
    | true => simp [hc] at h
    | false =>
      cases hl : AList.lookup P.fns f with
      | none => simp [hc, hl] at h
      | some fn =>
        obtain ⟨params, result, block, body⟩ := fn
        cases params with
        | nil => simp [hc, hl] at h
        | cons p ps =>
          cases ps with
          | cons _ _ => simp [hc, hl] at h
          | nil =>
            obtain ⟨x, T'⟩ := p
            simp only [hc, Bool.not_false, hl, Bool.true_and, Bool.and_eq_true,
              decide_eq_true_eq] at h
            exact ⟨rfl, ⟨[(x, T')], result, block, body⟩, x, rfl, by rw [h.1], h.2⟩
  · rintro ⟨hc, fn, x, hl, hp, hr⟩
    simp [validDestroyFn, hc, hl, hp, hr]

/-- Representation lemma. Characterizes the checked builder, not admission soundness
(approved: APPR-C1-codex-5531d9e34). The executable checked destroy-call
builder produces `some` d if and only if impl T = `some` (`Deferred.f` d), `Deferred.args` d = [arg], `Deferred.site` d = site,
T is non-Copy, and `Deferred.f` d has signature (x : T) -> Unit in `Program.fns` P. -/
theorem destroyCall_sig [DecidableEq O.Ty] (P : Program O) (impl : O.Ty → Option FnId) (T : O.Ty)
    (arg : O.Expr) (site : SiteId) (d : Deferred O) :
    checkedDestroyCall P impl T arg site = some d ↔
      impl T = some d.f ∧ d.args = [arg] ∧ d.site = site ∧
      O.isCopy T = false ∧ ∃ fn x, AList.lookup P.fns d.f = some fn ∧
        fn.params = [(x, T)] ∧ fn.result = O.unitTy := by
  constructor
  · intro h
    unfold checkedDestroyCall at h
    cases hi : impl T with
    | none => rw [hi] at h; cases h
    | some f =>
      rw [hi, Option.bind_some] at h
      cases hv : validDestroyFn P T f with
      | false => simp [hv] at h
      | true =>
        simp only [hv, ↓reduceIte, Option.some.injEq] at h
        subst h
        obtain ⟨hnc, fn, x, hfn, hp, hr⟩ := (destroy_not_copy P T f).mp hv
        exact ⟨rfl, rfl, rfl, hnc, fn, x, hfn, hp, hr⟩
  · rintro ⟨hi, hargs, hsite, hnc, fn, x, hfn, hp, hr⟩
    have hv : validDestroyFn P T d.f = true :=
      (destroy_not_copy P T d.f).mpr ⟨hnc, fn, x, hfn, hp, hr⟩
    cases d with | mk df dargs dsite =>
    simp only at hi hargs hsite hv
    subst hargs; subst hsite
    simp [checkedDestroyCall, hi, hv]

/-- Law (subject `ConcreteSpec.Core.Exec.Step`, `ConcreteSpec.Core.Exec.DestroyProtocol.destroyCall`).
An independently admitted consuming destructor enters its ordinary singleton-parameter body
through the actual call semantics. Argument evaluation may move the value; its resulting store
is threaded through activation entry. Admission, rather than the executable validator, supplies
the non-Copy and Unit-result facts. A refused activation is covered separately by `call_refused`. -/
theorem destroyCall_enters (P : Program O) (D : DestroyProtocol P) (T : O.Ty)
    (f : FnId) (fn : Fn O) (x : Var) (arg : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st st' st'' : O.Store) (v : O.Val) (k : List (Frame O))
    (hi : D.impl T = some f) (hf : AList.lookup P.fns f = some fn)
    (hp : fn.params = [(x, T)]) (he : O.eval L st arg = .ok (v, st'))
    (hl : O.life (.enter [x]) st' = some st'') :
    ∃ d, D.destroyCall T arg site = some d ∧ fn.result = O.unitTy ∧ O.isCopy T = false ∧
      Step P ⟨.exec d.stmt, L, st, k⟩ .silent
        ⟨.exec fn.body, [(x, v)], st'', .scope fn.block [x] [] :: .ret none L :: k⟩ := by
  obtain ⟨admitted, _, hfn, _, hr⟩ := D.sig T f hi
  rw [hf] at hfn
  cases hfn
  refine ⟨⟨f, [arg], site⟩, ?_, hr, D.notCopy T f hi, ?_⟩
  · simp [DestroyProtocol.destroyCall, hi]
  · apply Step.silent
    simp [Deferred.stmt, step, evalArgs, he, Res.bind, Res.map, onRes, hf, hp, hl]

/-- Law (subject `ConcreteSpec.Core.Exec.seqDestroyOrder`). A sequence's element i is visited at
position i: in increasing index order. -/
theorem seqDestroyOrder_elem (n i : Nat) (h : i < n) : (seqDestroyOrder n)[i]? = some (.elem i) := by
  rw [seqDestroyOrder, List.getElem?_append_left (by simpa using h)]
  simp [h]

/-- Law (subject `ConcreteSpec.Core.Exec.seqDestroyOrder`). A sequence's backing is released last,
after all n elements. -/
theorem seqDestroyOrder_storage (n : Nat) :
    (seqDestroyOrder n).length = n + 1 ∧ (seqDestroyOrder n)[n]? = some .storage := by
  simp [seqDestroyOrder]

/-- The entries of a map, in visiting order. -/
private def entryParts (n : Nat) : List Part := (List.range n).flatMap fun i => [.key i, .value i]

/-- Helper (arithmetic). Appending an entry adds its key and value visits (approved: APPR-C1-codex-5531d9e34). -/
private theorem entryParts_succ (n : Nat) :
    entryParts (n + 1) = entryParts n ++ [.key n, .value n] := by
  simp [entryParts, List.range_succ, List.flatMap_append]

/-- Helper (arithmetic). Each map entry contributes two visits (approved: APPR-C1-codex-5531d9e34). -/
private theorem entryParts_length : ∀ n, (entryParts n).length = 2 * n
  | 0 => rfl
  | n + 1 => by rw [entryParts_succ]; simp [entryParts_length n]; omega

/-- Helper (arithmetic). Locate the key and value in the alternating entry schedule
(approved: APPR-C1-codex-5531d9e34). -/
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

/-- Law (subject `ConcreteSpec.Core.Exec.mapDestroyOrder`). A map's i-th entry (in ascending key
order) is visited key first, at position 2i, then value, at 2i+1. -/
theorem mapDestroyOrder_entry (n i : Nat) (h : i < n) :
    (mapDestroyOrder n)[2 * i]? = some (.key i) ∧
      (mapDestroyOrder n)[2 * i + 1]? = some (.value i) := by
  have hl := entryParts_length n
  have := entryParts_get n i h
  show (entryParts n ++ [Part.storage])[2 * i]? = _ ∧
    (entryParts n ++ [Part.storage])[2 * i + 1]? = _
  rw [List.getElem?_append_left (by omega), List.getElem?_append_left (by omega)]
  exact this

/-- Law (subject `ConcreteSpec.Core.Exec.mapDestroyOrder`). A map's nodes are released last, after
all n entries. -/
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

/-- Representation lemma. In the demo language, a deferred `Demo.logF` call on 1 completes from any locals,
store, and frame stack, emitting [1] (approved: APPR-C1-codex-5531d9e34). -/
theorem demo_log_completes (L : AList Var Demo.ops.Val) (st : Demo.ops.Store) :
    Completes Demo.prog L ⟨Demo.logF, [.lit 1], Demo.s⟩ st [1] st := by
  intro k
  refine steps_next rfl (steps_request_inv (O := Demo.ops) (P := Demo.prog)
    (a := ⟨some (1 : Int), .ok (0 : Int), Demo.DState.life (.enter [Demo.x]) st⟩) (tr := []) rfl
    (by simp [Demo.ops, Demo.logFn]) rfl ?_)
  exact steps_next rfl (steps_next rfl (Steps.refl _))

/-- Representation lemma. In the demo language, a deferred `Demo.bumpF` call completes from any locals,
store, and frame stack, incrementing the counter (approved: APPR-C1-codex-5531d9e34). -/
theorem demo_bump_completes (L : AList Var Demo.ops.Val) (st : Demo.ops.Store) :
    Completes Demo.prog L ⟨Demo.bumpF, [], Demo.s⟩ st [] { st with counter := st.counter + 1 } := by
  intro k
  refine steps_next rfl (steps_request_inv (O := Demo.ops) (P := Demo.prog)
    (a := ⟨none, .ok (0 : Int),
      { Demo.DState.life (.enter []) st with counter := st.counter + 1 }⟩) (tr := []) rfl
    (by simp [Demo.ops, Demo.DState.life]) rfl ?_)
  exact steps_next rfl (steps_next rfl (Steps.refl _))

/-! ### Cleanup evidence and planted dispatcher mutations -/
namespace CleanupEvidence
open Demo

/-- Representation lemma. A concrete first-return segment for an ordinary deferred call
(approved: APPR-C1-codex-5531d9e34). -/
private theorem logSegment (n : Int) (e : Exit Int) (L : AList Var Int)
    (rest : List (Frame ops)) :
    FirstSteps (Step prog) (fun c => c.ctrl = .done ∧ c.frames = .resume (.exit e) :: rest)
      ⟨.exec (⟨logF, [.lit n], s⟩ : Deferred ops).stmt, L, .init, .resume (.exit e) :: rest⟩ [n]
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

def d (n : Int) : Deferred ops := ⟨logF, [.lit n], s⟩
def inner : Frame ops := .scope ⟨1⟩ [] [d 1]
def outer : Frame ops := .scope ⟨2⟩ [] [d 2]
def emptyScope : Frame ops := .scope ⟨1⟩ [] []
def actions : List (CleanAct ops) :=
  [.call [] (d 1), .drop [] [] [], .call [] (d 2), .drop [] [] []]

/-- Planted mutant of `Step`: deletes a scope's pending deferred calls and drops the scope directly. -/
inductive MissingStep (P : Program O) : Config O → Label O → Config O → Prop where
  | skip {e L st b dm ds k stDrop} :
      dropOut O dm L st = some stDrop →
      MissingStep P ⟨.exiting e, L, st, .scope b dm ds :: k⟩ .silent
        ⟨.exiting e, restrict dm L, stDrop, k⟩

/-- Planted mutant of `Step`: injects an unregistered call at an empty scope before dropping it. -/
inductive ExtraStep (P : Program O) : Config O → Label O → Config O → Prop where
  | inject {e L st b dm k} {callD : Deferred O} :
      ExtraStep P ⟨.exiting e, L, st, .scope b dm [] :: k⟩ .silent
        ⟨.exec callD.stmt, L, st, .resume (.exit e) :: .scope b dm [] :: k⟩
  | resume {e L st k} :
      ExtraStep P ⟨.done, L, st, .resume (.exit e) :: k⟩ .silent
        ⟨.exiting e, L, st, k⟩
  | drop {e L st b dm k stDrop} :
      dropOut O dm L st = some stDrop →
      ExtraStep P ⟨.exiting e, L, st, .scope b dm [] :: k⟩ .silent
        ⟨.exiting e, restrict dm L, stDrop, k⟩

/-- Planted mutant of `Step`: pops an empty scope and restricts locals, but forgets `dropOut`. -/
inductive MissingDropStep (P : Program O) : Config O → Label O → Config O → Prop where
  | skip {e L st b dm k} :
      MissingDropStep P ⟨.exiting e, L, st, .scope b dm [] :: k⟩ .silent
        ⟨.exiting e, restrict dm L, st, k⟩

/-- Planted mutant of `Step`: pops an empty scope and updates the store via `dropOut`, but
forgets to restore locals via `restrict`. -/
inductive MissingRestrictStep (P : Program O) : Config O → Label O → Config O → Prop where
  | skip {e L st b dm k stDrop} :
      dropOut O dm L st = some stDrop →
      MissingRestrictStep P ⟨.exiting e, L, st, .scope b dm [] :: k⟩ .silent
        ⟨.exiting e, L, stDrop, k⟩

/-- Instance (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`, case `nested`). Two real
calls on nested scopes emit [1, 2], with each scope drop between calls and locals restored. -/
example : BoundaryRun prog (Step prog) (.ret 7) [] [inner, outer] [] .init actions [1, 2] [] .init ↔
    (∀ (f : Frame ops), f ∈ ([inner, outer] : List (Frame ops)) → Passes (O := ops) (.ret 7) f) ∧
      actions = planWith [inner, outer] [] ∧
      ([] : AList Var Int) = unwindLocals [inner, outer] [] ∧
      Performed prog actions [] .init [1, 2] [] .init :=
  cleanup_runs_exact_plan prog (.ret 7) [] [inner, outer] [] [] .init .init actions [1, 2]

/-- Planted (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`, case `nested`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CleanupEvidence.ExtraStep`).
At the nested-scope plan `actions`, `ExtraStep` only acts on empty scopes and cannot dispatch the
pending deferred calls on `inner` and `outer`, falsifying the equivalence at the positive instance's
exact arguments. -/
example : ¬ (BoundaryRun prog (ExtraStep prog) (.ret 7) [] [inner, outer] [] .init actions [1, 2] [] .init ↔
    (∀ (f : Frame ops), f ∈ ([inner, outer] : List (Frame ops)) → Passes (O := ops) (.ret 7) f) ∧
      actions = planWith [inner, outer] [] ∧
      ([] : AList Var Int) = unwindLocals [inner, outer] [] ∧
      Performed prog actions [] .init [1, 2] [] .init) := by
  intro h
  have hp : ∀ (f : Frame ops), f ∈ ([inner, outer] : List (Frame ops)) → Passes (O := ops) (.ret 7) f := by
    intro f hf
    cases hf with
    | head => trivial
    | tail _ hf =>
      cases hf with
      | head => trivial
      | tail _ hf => cases hf
  have hperf : Performed prog actions [] .init [1, 2] [] .init :=
    Performed.call (logSegment 1 (.ret 7) [] [.scope ⟨1⟩ [] [], outer])
      (Performed.drop (P := prog) (dm := []) (L := []) rfl
        (Performed.call (logSegment 2 (.ret 7) [] [.scope ⟨2⟩ [] []])
          (Performed.drop (P := prog) (dm := []) (L := []) rfl Performed.nil)))
  have hrun := h.mpr ⟨hp, rfl, rfl, hperf⟩
  generalize hacts : actions = acts₀ at hrun
  generalize htr : ([1, 2] : List Int) = tr₀ at hrun
  cases hrun with
  | pass _ hs _ =>
    generalize hc : (⟨.exiting (.ret 7), _, _, _⟩ : Config ops) = c' at hs
    cases hs <;> simp [inner] at hc
  | call _ hs _ _ _ =>
    cases hs

/-- Instance (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`, case `missing`). Skipping a
registered call produces empty actions, which `cleanup_runs_exact_plan` characterizes as
impossible under `Step`. -/
example : BoundaryRun prog (Step prog) (.ret 7) [] [inner] [] .init [] [] [] .init ↔
    (∀ (f : Frame ops), f ∈ ([inner] : List (Frame ops)) → Passes (O := ops) (.ret 7) f) ∧
      ([] : List (CleanAct ops)) = planWith [inner] [] ∧
      ([] : AList Var Int) = unwindLocals [inner] [] ∧
      Performed prog [] [] .init [] [] .init :=
  cleanup_runs_exact_plan prog (.ret 7) [] [inner] [] [] .init .init [] []

/-- Planted (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`, case `missing`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CleanupEvidence.MissingStep`).
A mutated rule deletes a registered call: the observer admits the mutated execution with empty
actions, which disagrees with `planWith` on `inner`. -/
example : ¬ (BoundaryRun prog (MissingStep prog) (.ret 7) [] [inner] [] .init [] [] [] .init ↔
    (∀ (f : Frame ops), f ∈ ([inner] : List (Frame ops)) → Passes (O := ops) (.ret 7) f) ∧
      ([] : List (CleanAct ops)) = planWith [inner] [] ∧
      ([] : AList Var Int) = unwindLocals [inner] [] ∧
      Performed prog [] [] .init [] [] .init) := by
  intro h
  have hr : BoundaryRun prog (MissingStep prog) (.ret 7) [] [inner] [] .init [] [] [] .init := by
    have hp := BoundaryRun.pass (P := prog) (R := MissingStep prog) (e := .ret 7) (k := [])
      (pre := [inner]) (pre' := []) (L := []) (L' := []) (st := .init) (st' := .init)
      (by intro hn; cases hn) (.skip rfl) .stop
    simpa [inner, leaving, restrict] using hp
  have he := (h.mp hr).2.1
  change ([] : List (CleanAct ops)) = [.call [] (d 1), .drop [] [] []] at he
  cases he

/-- Instance (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`, case `extra`). An empty
scope cannot dispatch an unregistered `logF` 9 call under `Step`. -/
example : BoundaryRun prog (Step prog) (.ret 7) [] [emptyScope] [] .init
      [.call [] (d 9), .drop [] [] []] [9] [] .init ↔
    (∀ (f : Frame ops), f ∈ ([emptyScope] : List (Frame ops)) → Passes (O := ops) (.ret 7) f) ∧
      ([.call [] (d 9), .drop [] [] []] : List (CleanAct ops)) = planWith [emptyScope] [] ∧
      ([] : AList Var Int) = unwindLocals [emptyScope] [] ∧
      Performed prog [.call [] (d 9), .drop [] [] []] [] .init [9] [] .init :=
  cleanup_runs_exact_plan prog (.ret 7) [] [emptyScope] [] [] .init .init
    [.call [] (d 9), .drop [] [] []] [9]

/-- Planted (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`, case `extra`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CleanupEvidence.ExtraStep`).
A mutated rule adds a real `logF` 9 call not registered in the scope; its execution emits 9 and
disagrees with `planWith`. -/
example : ¬ (BoundaryRun prog (ExtraStep prog) (.ret 7) [] [emptyScope] [] .init
      [.call [] (d 9), .drop [] [] []] [9] [] .init ↔
    (∀ (f : Frame ops), f ∈ ([emptyScope] : List (Frame ops)) → Passes (O := ops) (.ret 7) f) ∧
      ([.call [] (d 9), .drop [] [] []] : List (CleanAct ops)) = planWith [emptyScope] [] ∧
      ([] : AList Var Int) = unwindLocals [emptyScope] [] ∧
      Performed prog [.call [] (d 9), .drop [] [] []] [] .init [9] [] .init) := by
  intro h
  have hdrop : BoundaryRun prog (ExtraStep prog) (.ret 7) [] [emptyScope] [] .init
      [.drop [] [] []] [] [] .init := by
    have hp := BoundaryRun.pass (P := prog) (R := ExtraStep prog) (e := .ret 7) (k := [])
      (pre := [emptyScope]) (pre' := []) (L := []) (L' := []) (st := .init) (st' := .init)
      (by intro hn; cases hn) (.drop rfl) .stop
    simpa [emptyScope, leaving, restrict] using hp
  have hr : BoundaryRun prog (ExtraStep prog) (.ret 7) [] [emptyScope] [] .init
      [.call [] (d 9), .drop [] [] []] [9] [] .init :=
    BoundaryRun.call (P := prog) (R := ExtraStep prog) (pre := [emptyScope])
      (d := d 9) (rest := [emptyScope]) (by intro hn; cases hn) ExtraStep.inject
      (logSegment 9 (.ret 7) [] [emptyScope]) ExtraStep.resume hdrop
  have he := (h.mp hr).2.1
  change ([.call [] (d 9), .drop [] [] []] : List (CleanAct ops)) = [.drop [] [] []] at he
  cases he

/-- Instance (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`, case `missing_drop`).
Leaving a scope without updating the store via `dropOut` is rejected by `cleanup_runs_exact_plan`. -/
example : BoundaryRun prog (Step prog) (.ret 7) [] [emptyScope] [(x, 1)] ⟨0, [[x]]⟩
      [.drop [] [(x, 1)] []] [] [] ⟨0, [[x]]⟩ ↔
    (∀ (f : Frame ops), f ∈ ([emptyScope] : List (Frame ops)) → Passes (O := ops) (.ret 7) f) ∧
      ([.drop [] [(x, 1)] []] : List (CleanAct ops)) = planWith [emptyScope] [(x, 1)] ∧
      ([] : AList Var Int) = unwindLocals [emptyScope] [(x, 1)] ∧
      Performed prog [.drop [] [(x, 1)] []] [(x, 1)] ⟨0, [[x]]⟩ [] [] ⟨0, [[x]]⟩ :=
  cleanup_runs_exact_plan prog (.ret 7) [] [emptyScope] [(x, 1)] [] ⟨0, [[x]]⟩ ⟨0, [[x]]⟩
    [.drop [] [(x, 1)] []] []

/-- Planted (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`, case `missing_drop`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CleanupEvidence.MissingDropStep`).
Popping the scope and restricting locals while forgetting the store lifecycle update leaves `x`
marked moved, violating `Performed`. -/
example : ¬ (BoundaryRun prog (MissingDropStep prog) (.ret 7) [] [emptyScope] [(x, 1)] ⟨0, [[x]]⟩
      [.drop [] [(x, 1)] []] [] [] ⟨0, [[x]]⟩ ↔
    (∀ (f : Frame ops), f ∈ ([emptyScope] : List (Frame ops)) → Passes (O := ops) (.ret 7) f) ∧
      ([.drop [] [(x, 1)] []] : List (CleanAct ops)) = planWith [emptyScope] [(x, 1)] ∧
      ([] : AList Var Int) = unwindLocals [emptyScope] [(x, 1)] ∧
      Performed prog [.drop [] [(x, 1)] []] [(x, 1)] ⟨0, [[x]]⟩ [] [] ⟨0, [[x]]⟩) := by
  intro h
  have hr : BoundaryRun prog (MissingDropStep prog) (.ret 7) [] [emptyScope] [(x, 1)] ⟨0, [[x]]⟩
      [.drop [] [(x, 1)] []] [] [] ⟨0, [[x]]⟩ := by
    have hp := BoundaryRun.pass (P := prog) (R := MissingDropStep prog) (e := .ret 7) (k := [])
      (pre := [emptyScope]) (pre' := []) (L := [(x, 1)]) (L' := [])
      (st := ⟨0, [[x]]⟩) (st' := ⟨0, [[x]]⟩) (by intro hn; cases hn) .skip .stop
    simpa [emptyScope, leaving, restrict] using hp
  have hp := (h.mp hr).2.2.2
  cases hp with
  | drop hd hr =>
    cases hr
    have he : (⟨0, [[]]⟩ : DState) = ⟨0, [[x]]⟩ := Option.some.inj hd
    have hm := congrArg DState.moved he
    cases hm

/-- Instance (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`, case `missing_restrict`).
Leaving a scope that updates the store via `dropOut` but fails to restrict L' is rejected by
`cleanup_runs_exact_plan`. -/
example : BoundaryRun prog (Step prog) (.ret 7) [] [emptyScope] [(x, 1)] ⟨0, [[x]]⟩
      [.drop [] [(x, 1)] [(x, 1)]] [] [(x, 1)] ⟨0, [[]]⟩ ↔
    (∀ (f : Frame ops), f ∈ ([emptyScope] : List (Frame ops)) → Passes (O := ops) (.ret 7) f) ∧
      ([.drop [] [(x, 1)] [(x, 1)]] : List (CleanAct ops)) = planWith [emptyScope] [(x, 1)] ∧
      ([(x, 1)] : AList Var Int) = unwindLocals [emptyScope] [(x, 1)] ∧
      Performed prog [.drop [] [(x, 1)] [(x, 1)]] [(x, 1)] ⟨0, [[x]]⟩ [] [(x, 1)] ⟨0, [[]]⟩ :=
  cleanup_runs_exact_plan prog (.ret 7) [] [emptyScope] [(x, 1)] [(x, 1)] ⟨0, [[x]]⟩ ⟨0, [[]]⟩
    [.drop [] [(x, 1)] [(x, 1)]] []

/-- Planted (law `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`, case `missing_restrict`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CleanupEvidence.MissingRestrictStep`).
Popping the scope and performing `dropOut` on the store while failing to restrict L' to
`restrict` [] [(`x`, 1)] = [] violates acts = `planWith`, L' = `unwindLocals`, and `Performed`. -/
example : ¬ (BoundaryRun prog (MissingRestrictStep prog) (.ret 7) [] [emptyScope] [(x, 1)] ⟨0, [[x]]⟩
      [.drop [] [(x, 1)] [(x, 1)]] [] [(x, 1)] ⟨0, [[]]⟩ ↔
    (∀ (f : Frame ops), f ∈ ([emptyScope] : List (Frame ops)) → Passes (O := ops) (.ret 7) f) ∧
      ([.drop [] [(x, 1)] [(x, 1)]] : List (CleanAct ops)) = planWith [emptyScope] [(x, 1)] ∧
      ([(x, 1)] : AList Var Int) = unwindLocals [emptyScope] [(x, 1)] ∧
      Performed prog [.drop [] [(x, 1)] [(x, 1)]] [(x, 1)] ⟨0, [[x]]⟩ [] [(x, 1)] ⟨0, [[]]⟩) := by
  intro h
  have hr : BoundaryRun prog (MissingRestrictStep prog) (.ret 7) [] [emptyScope] [(x, 1)] ⟨0, [[x]]⟩
      [.drop [] [(x, 1)] [(x, 1)]] [] [(x, 1)] ⟨0, [[]]⟩ := by
    have hp := BoundaryRun.pass (P := prog) (R := MissingRestrictStep prog) (e := .ret 7) (k := [])
      (pre := [emptyScope]) (pre' := []) (L := [(x, 1)]) (L' := [(x, 1)])
      (st := ⟨0, [[x]]⟩) (st' := ⟨0, [[]]⟩) (by intro hn; cases hn) (.skip rfl) .stop
    simpa [emptyScope, leaving, restrict] using hp
  have hL := (h.mp hr).2.2.1
  change ([(x, 1)] : AList Var Int) = [] at hL
  cases hL

/-- Representation lemma. Extend a raw run with a concrete deferred log's complete execution
(approved: APPR-C1-codex-5531d9e34). -/
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

/-- Representation lemma. Concrete raw first-arrival execution of `inner` (approved: APPR-C1-codex-5531d9e34). -/
private theorem inner_raw_run : FirstSteps (Step prog) (fun c => c.frames.length ≤ 0)
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

/-- Planted mutant of `Step` for `raw_cleanup_runs_plan`: jumps directly from `.exiting` to
`.done` while emitting an event from `O.eval`, bypassing call dispatch. -/
inductive EscapeEmitStep (P : Program O) : Config O → Label O → Config O → Prop where
  | skip {e L st b dm d ds k ev ev' st' l} :
      d.args = [ev] →
      O.eval L st ev = .ok (ev', st') →
      EscapeEmitStep P ⟨.exiting e, L, st, .scope b dm (d :: ds) :: k⟩ l
        ⟨.done, L, st', .resume (.exit e) :: k⟩
  | resume {e L st k} :
      EscapeEmitStep P ⟨.done, L, st, .resume (.exit e) :: k⟩ .silent
        ⟨.exiting e, L, st, k⟩

/-- Planted mutant of `Step` for `raw_cleanup_runs_plan`: jumps silently through `.done` /
`.resume` instead of dispatching a call. -/
inductive EscapeStep (P : Program O) : Config O → Label O → Config O → Prop where
  | skip {e L st b dm ds k} :
      EscapeStep P ⟨.exiting e, L, st, .scope b dm ds :: k⟩ .silent
        ⟨.done, L, st, .resume (.exit e) :: k⟩
  | resume {e L st k} :
      EscapeStep P ⟨.done, L, st, .resume (.exit e) :: k⟩ .silent
        ⟨.exiting e, L, st, k⟩

/-- Helper (computation). The concrete singleton cleanup frame passes a return
(approved: APPR-C1-codex-5531d9e34). -/
private theorem inner_passes : ∀ (f : Frame ops), f ∈ ([inner] : List (Frame ops)) →
    Passes (O := ops) (.ret 7) f := by
  intro f hf
  cases hf with
  | head => trivial
  | tail _ hf => cases hf

/-- Instance (law `ConcreteSpec.Core.Exec.raw_cleanup_runs_plan`, case `single`). A completed
raw execution yields the exact dispatch plan, performed actions and restored locals. -/
example : ∃ acts, BoundaryRun prog (Step prog) (.ret 7) [] [inner] [] .init acts [1] [] .init ∧
    acts = planWith [inner] [] ∧ ([] : AList Var Int) = unwindLocals [inner] [] ∧
    Performed prog acts [] .init [1] [] .init :=
  raw_cleanup_runs_plan prog (.ret 7) [] [inner] [] [] .init .init [1] inner_passes inner_raw_run

/-- Planted (law `ConcreteSpec.Core.Exec.raw_cleanup_runs_plan`, case `single`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CleanupEvidence.EscapeEmitStep`).
A bypass preserves passability and raw execution but admits no plan segmentation. -/
example : (∀ (f : Frame ops), f ∈ ([inner] : List (Frame ops)) → Passes (O := ops) (.ret 7) f) ∧
    FirstSteps (EscapeEmitStep prog) (fun c => c.frames.length ≤ ([] : List (Frame ops)).length)
      ⟨.exiting (.ret 7), [], .init, [inner] ++ []⟩ [1] ⟨.exiting (.ret 7), [], .init, []⟩ ∧
    ¬ (∃ acts, BoundaryRun prog (EscapeEmitStep prog) (.ret 7) [] [inner] [] .init acts [1] [] .init ∧
      acts = planWith [inner] [] ∧ ([] : AList Var Int) = unwindLocals [inner] [] ∧
      Performed prog acts [] .init [1] [] .init) := by
  refine ⟨inner_passes, ?_, ?_⟩
  · exact FirstSteps.step (R := EscapeEmitStep prog) (l := .emit 1) (by simp)
      (EscapeEmitStep.skip (O := ops) (ev := .lit 1) (ev' := 1) rfl rfl)
      (FirstSteps.step (R := EscapeEmitStep prog) (by simp) EscapeEmitStep.resume (.stop (by simp)))
  · rintro ⟨_, observed, _⟩
    generalize htr : ([1] : List Int) = trace at observed
    cases observed with
    | pass _ hs _ => cases hs
    | call _ hs _ _ _ => cases hs

/-- Mutant with no executable plan transitions. -/
inductive NoPlanStep (P : Program O) : Config O → Label O → Config O → Prop

/-- Mutant that preserves ordinary transitions but refuses every nonempty cleanup dispatch. -/
inductive NoDispatchStep (P : Program O) : Config O → Label O → Config O → Prop where
  | retain {c l c'} : Step P c l c' → (match c.ctrl, c.frames with
      | .exiting _, .scope _ _ (_ :: _) :: _ => False
      | _, _ => True) → NoDispatchStep P c l c'

/-- Helper (inversion). Concrete singleton passability certificate (approved: APPR-C1-codex-5531d9e34). -/
private theorem empty_passes : ∀ f ∈ [emptyScope], Passes (O := ops) (.ret 7) f := by
  intro f hf
  cases hf with
  | head => trivial
  | tail _ hf => cases hf

/-- Representation lemma. Concrete independent drop execution (approved: APPR-C1-codex-5531d9e34). -/
private theorem drop_performed : Performed prog [.drop [] [] []] [] .init [] [] .init :=
  .drop rfl .nil

/-- Instance (law `ConcreteSpec.Core.Exec.boundary_of_performed`, case `scope_drop`). -/
example : BoundaryRun prog (Step prog) (.ret 7) [] [emptyScope] [] .init
    (planWith [emptyScope] []) [] [] .init :=
  boundary_of_performed (P := prog) (e := .ret 7) (k := []) [emptyScope]
    (L := []) (L' := []) (st := .init) (st' := .init) (tr := []) empty_passes drop_performed

/-- Planted (law `ConcreteSpec.Core.Exec.boundary_of_performed`, case `scope_drop`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CleanupEvidence.NoPlanStep`).
The independent drop succeeds but removing the actual drop transition prevents the execution. -/
example : (∀ f ∈ [emptyScope], Passes (O := ops) (.ret 7) f) ∧
    Performed prog (planWith [emptyScope] []) [] .init [] [] .init ∧
    ¬ BoundaryRun prog (NoPlanStep prog) (.ret 7) [] [emptyScope] [] .init
      (planWith [emptyScope] []) [] [] .init := by
  refine ⟨empty_passes, drop_performed, fun h => ?_⟩
  generalize ha : planWith [emptyScope] [] = acts at h
  generalize ht : ([] : List Int) = trace at h
  cases h with
  | pass _ hs _ => cases hs
  | call _ hs _ _ _ => cases hs

/-- Representation lemma. Concrete action sequence with its first-return call certificate
(approved: APPR-C1-codex-5531d9e34). -/
private theorem single_performed : Performed prog
    ([d 1].map (.call []) ++ [.drop [] [] []]) [] .init [1] [] .init :=
  .call (logSegment 1 (.ret 7) [] [emptyScope]) (.drop rfl .nil)

/-- Representation lemma. Production execution of an empty scope from an independently
performed drop, used as a closed higher-order premise (approved: APPR-C1-codex-5531d9e34). -/
private theorem base_execution : ∀ st₁ tr₂,
    Performed prog [.drop [] [] []] [] st₁ tr₂ [] .init →
    BoundaryRun prog (Step prog) (.ret 7) [] [emptyScope] [] st₁ [.drop [] [] []] tr₂ [] .init := by
  intro st₁ tr₂ hp
  exact boundary_of_performed (P := prog) (e := .ret 7) (k := []) [emptyScope] empty_passes hp

/-- Representation lemma. The no-dispatch mutant retains successful empty-scope drops
(approved: PENDING). -/
private theorem mutant_base_execution : ∀ st₁ tr₂,
    Performed prog [.drop [] [] []] [] st₁ tr₂ [] .init →
    BoundaryRun prog (NoDispatchStep prog) (.ret 7) [] [emptyScope] [] st₁ [.drop [] [] []] tr₂ [] .init := by
  intro st₁ tr₂ hp
  cases hp with
  | drop hd hrest =>
    cases hrest
    have hs : NoDispatchStep prog ⟨.exiting (.ret 7), [], st₁, [emptyScope]⟩ .silent
        ⟨.exiting (.ret 7), [], .init, []⟩ :=
      .retain (.silent (by simp [step, stepExit, emptyScope, hd, restrict])) True.intro
    simpa [leaving, emptyScope] using
      (BoundaryRun.pass (pre := [emptyScope]) (pre' := []) (by simp) hs .stop)

/-- Instance (law `ConcreteSpec.Core.Exec.boundary_scope_calls`, case `call_then_drop`). -/
example : BoundaryRun prog (Step prog) (.ret 7) [] [.scope ⟨1⟩ [] [d 1]] [] .init
    ([d 1].map (.call []) ++ [.drop [] [] []]) [1] [] .init :=
  boundary_scope_calls (P := prog) (e := .ret 7) (k := []) ⟨1⟩ [] [] [d 1]
    (L := []) (L' := []) (st := .init) (st' := .init) (acts := [.drop [] [] []]) (tr := [1])
    single_performed base_execution

/-- Planted (law `ConcreteSpec.Core.Exec.boundary_scope_calls`, case `call_then_drop`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CleanupEvidence.NoDispatchStep`).
The independent call and the final drop execute, but dispatch of the nonempty scope is missing. -/
example : Performed prog ([d 1].map (.call []) ++ [.drop [] [] []]) [] .init [1] [] .init ∧
    (∀ st₁ tr₂, Performed prog [.drop [] [] []] [] st₁ tr₂ [] .init →
      BoundaryRun prog (NoDispatchStep prog) (.ret 7) [] [emptyScope] [] st₁ [.drop [] [] []] tr₂ [] .init) ∧
    ¬ BoundaryRun prog (NoDispatchStep prog) (.ret 7) [] [.scope ⟨1⟩ [] [d 1]] [] .init
      ([d 1].map (.call []) ++ [.drop [] [] []]) [1] [] .init := by
  refine ⟨single_performed, mutant_base_execution, fun h => ?_⟩
  generalize ha : ([d 1].map (.call []) ++ [.drop [] [] []] : List (CleanAct ops)) = acts at h
  generalize ht : ([1] : List Int) = trace at h
  cases h with
  | pass _ hs _ => cases hs with | retain _ impossible => exact impossible
  | call _ hs _ _ _ => cases hs with | retain _ impossible => exact impossible

/-- Mutant that skips normal return: it may advance only to configurations whose control is
not `.done`. It can cross a stack boundary without restoring the caller first. -/
inductive SkipReturnStep (P : Program O) : Config O → Label O → Config O → Prop where
  | skip {c c'} : c'.ctrl ≠ .done → SkipReturnStep P c .silent c'

/-- Helper (inversion). A shape invariant of the planted relation: no positive-length path
can introduce `Ctrl.done` (approved: PENDING). -/
private theorem skip_no_done {P : Program O} {stop : Config O → Prop}
    {c c' : Config O} {tr : List O.Event} {n : Nat}
    (h : FirstStepsN (SkipReturnStep P) stop c tr c' n) : c.ctrl ≠ .done → c'.ctrl ≠ .done := by
  induction h with
  | stop _ => exact id
  | step _ hs _ ih => intro _; cases hs with | skip hnd => exact ih hnd

/-- A concrete saved continuation for return-segmentation witnesses. -/
def returnQ : List (Frame ops) := [.resume (.exit (.ret 7))]
def savedStart : Config ops := ⟨.exec .skip, [], .init, .ret none [(x, 7)] :: returnQ⟩
def savedEnd : Config ops := ⟨.exiting (.ret 7), [(x, 7)], .init, []⟩

/-- Representation lemma. A concrete three-step return/restoration execution
(approved: APPR-C1-codex-5531d9e34). -/
private theorem saved_three : FirstStepsN (Step prog) (fun c => c.frames.length ≤ 0)
    savedStart [] savedEnd 3 := by
  refine FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (by decide) (Step.silent (P := prog) (c' :=
    ⟨.done, [], .init, .ret none [(x, 7)] :: returnQ⟩) rfl) ?_
  refine FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (by decide) (Step.silent (P := prog) (c' :=
    ⟨.done, [(x, 7)], .init, returnQ⟩) rfl) ?_
  exact FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (by decide) (Step.silent (P := prog) rfl) (.stop (by decide))

/-- Representation lemma. A three-step mutant path crosses the same boundary while never
reaching the required restored return configuration (approved: PENDING). -/
private theorem skipped_three : FirstStepsN (SkipReturnStep prog) (fun c => c.frames.length ≤ 0)
    savedStart [] savedEnd 3 := by
  let mid : Config ops := ⟨.exiting (.ret 7), [], .init, returnQ⟩
  exact FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (c' := mid) (by decide) (.skip (by intro h; cases h))
    (FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (c' := mid) (by decide) (.skip (by intro h; cases h))
      (FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (by decide) (.skip (by intro h; cases h)) (.stop (by decide))))

/-- Instance (law `ConcreteSpec.Core.Exec.FirstStepsN.split_return`, case `restore`). -/
example : ∃ n₁ n₂ tr₁ st₁ tr₂, 3 = n₁ + n₂ ∧ ([] : List Int) = tr₁ ++ tr₂ ∧
    FirstStepsN (Step prog) (fun c => c.ctrl = .done ∧ c.frames = returnQ)
      savedStart tr₁ ⟨.done, [(x, 7)], st₁, returnQ⟩ n₁ ∧
    FirstStepsN (Step prog) (fun c => c.frames.length ≤ 0)
      ⟨.done, [(x, 7)], st₁, returnQ⟩ tr₂ savedEnd n₂ :=
  FirstStepsN.split_return (P := prog) (bound := 0) (c := savedStart) (endC := savedEnd)
    (tr := []) (n := 3) saved_three (caller := [(x, 7)]) (q := returnQ) (by decide) ⟨[], rfl⟩

/-- Planted (law `ConcreteSpec.Core.Exec.FirstStepsN.split_return`, case `restore`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CleanupEvidence.SkipReturnStep`). -/
example : FirstStepsN (SkipReturnStep prog) (fun c => c.frames.length ≤ 0) savedStart [] savedEnd 3 ∧
    0 < returnQ.length ∧ Protected (O := ops) [(x, 7)] returnQ savedStart ∧
    ¬ (∃ n₁ n₂ tr₁ st₁ tr₂, 3 = n₁ + n₂ ∧ ([] : List Int) = tr₁ ++ tr₂ ∧
      FirstStepsN (SkipReturnStep prog) (fun c => c.ctrl = .done ∧ c.frames = returnQ)
        savedStart tr₁ ⟨.done, [(x, 7)], st₁, returnQ⟩ n₁ ∧
      FirstStepsN (SkipReturnStep prog) (fun c => c.frames.length ≤ 0)
        ⟨.done, [(x, 7)], st₁, returnQ⟩ tr₂ savedEnd n₂) := by
  refine ⟨skipped_three, by decide, ⟨[], rfl⟩, ?_⟩
  rintro ⟨_, _, _, _, _, _, _, first, _⟩
  exact skip_no_done first (by intro h; cases h) rfl

/-- A deferred bump and its post-boundary endpoint. -/
def bumpD : Deferred ops := ⟨bumpF, [], s⟩
def bumpEnd : Config ops := ⟨.exiting (.ret 7), [(x, 7)], ⟨1, []⟩, []⟩

/-- Representation lemma. A complete five-step deferred bump and boundary crossing
(approved: APPR-C1-codex-5531d9e34). -/
private theorem bump_five : FirstStepsN (Step prog) (fun c => c.frames.length ≤ 0)
    ⟨.exec bumpD.stmt, [(x, 7)], .init, returnQ⟩ [] bumpEnd 5 := by
  refine FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (by decide) (Step.silent (P := prog) (c' :=
    ⟨.exec bumpFn.body, [], ⟨0, [[]]⟩, .scope bumpFn.block [] [] :: .ret none [(x, 7)] :: returnQ⟩) rfl) ?_
  refine FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (by decide) (Step.prim (P := prog) (a := (⟨none, .ok 0, ⟨1, [[]]⟩⟩ : Answer Int Int DState))
    (c' := ⟨.done, [], ⟨1, [[]]⟩, .scope bumpFn.block [] [] :: .ret none [(x, 7)] :: returnQ⟩)
    rfl (by simp [ops]) rfl) ?_
  refine FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (by decide) (Step.silent (P := prog) (c' :=
    ⟨.done, [], ⟨1, [[]]⟩, .ret none [(x, 7)] :: returnQ⟩) rfl) ?_
  refine FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (by decide) (Step.silent (P := prog) (c' :=
    ⟨.done, [(x, 7)], ⟨1, []⟩, returnQ⟩) rfl) ?_
  exact FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (by decide) (Step.silent (P := prog) rfl) (.stop (by decide))

/-- Representation lemma. The mutant completes five steps without a restored return
(approved: PENDING). -/
private theorem skipped_five : FirstStepsN (SkipReturnStep prog) (fun c => c.frames.length ≤ 0)
    ⟨.exec bumpD.stmt, [(x, 7)], .init, returnQ⟩ [] bumpEnd 5 := by
  let mid : Config ops := ⟨.exiting (.ret 7), [], .init, returnQ⟩
  apply FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (c' := mid) (by decide) (.skip (by intro h; cases h))
  apply FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (c' := mid) (by decide) (.skip (by intro h; cases h))
  apply FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (c' := mid) (by decide) (.skip (by intro h; cases h))
  apply FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (c' := mid) (by decide) (.skip (by intro h; cases h))
  exact FirstStepsN.step (O := ops) (stop := fun c => c.frames.length ≤ 0) (l := .silent) (by decide) (.skip (by intro h; cases h)) (.stop (by decide))

/-- Instance (law `ConcreteSpec.Core.Exec.FirstStepsN.call_return`, case `deferred_bump`). -/
example : ∃ n₁ n₂ tr₁ st₁ tr₂, 5 = n₁ + n₂ ∧ ([] : List Int) = tr₁ ++ tr₂ ∧
    FirstStepsN (Step prog) (fun c => c.ctrl = .done ∧ c.frames = returnQ)
      ⟨.exec bumpD.stmt, [(x, 7)], .init, returnQ⟩ tr₁ ⟨.done, [(x, 7)], st₁, returnQ⟩ n₁ ∧
    FirstStepsN (Step prog) (fun c => c.frames.length ≤ 0)
      ⟨.done, [(x, 7)], st₁, returnQ⟩ tr₂ bumpEnd n₂ :=
  FirstStepsN.call_return (P := prog) (bound := 0) (L := [(x, 7)]) (st := .init)
    (q := returnQ) (d := bumpD) (tr := []) (endC := bumpEnd) (n := 5) (by decide) bump_five

/-- Planted (law `ConcreteSpec.Core.Exec.FirstStepsN.call_return`, case `deferred_bump`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CleanupEvidence.SkipReturnStep`). -/
example : 0 < returnQ.length ∧
    FirstStepsN (SkipReturnStep prog) (fun c => c.frames.length ≤ 0)
      ⟨.exec bumpD.stmt, [(x, 7)], .init, returnQ⟩ [] bumpEnd 5 ∧
    ¬ (∃ n₁ n₂ tr₁ st₁ tr₂, 5 = n₁ + n₂ ∧ ([] : List Int) = tr₁ ++ tr₂ ∧
      FirstStepsN (SkipReturnStep prog) (fun c => c.ctrl = .done ∧ c.frames = returnQ)
        ⟨.exec bumpD.stmt, [(x, 7)], .init, returnQ⟩ tr₁ ⟨.done, [(x, 7)], st₁, returnQ⟩ n₁ ∧
      FirstStepsN (SkipReturnStep prog) (fun c => c.frames.length ≤ 0)
        ⟨.done, [(x, 7)], st₁, returnQ⟩ tr₂ bumpEnd n₂) := by
  refine ⟨by decide, skipped_five, ?_⟩
  rintro ⟨_, _, _, _, _, _, _, first, _⟩
  exact skip_no_done first (by intro h; cases h) rfl

/-- Mutant raw execution escapes through `.done` instead of dispatching. -/
inductive PaddedEscape (P : Program O) : Config O → Label O → Config O → Prop where
  | skip {e L st frames k} : PaddedEscape P
      ⟨.exiting e, L, st, frames⟩ .silent ⟨.done, L, st, .resume (.exit e) :: k⟩
  | resume {e L st k} : PaddedEscape P
      ⟨.done, L, st, .resume (.exit e) :: k⟩ .silent ⟨.exiting e, L, st, k⟩

/-- Helper (inversion). The concrete two-frame boundary passes a return (approved: APPR-C1-codex-5531d9e34). -/
private theorem two_passes : ∀ f ∈ [.seq .skip, emptyScope], Passes (O := ops) (.ret 7) f := by
  intro f hf
  cases hf with
  | head => trivial
  | tail _ hf => exact empty_passes f hf

/-- Representation lemma. A concrete two-step raw exit (approved: APPR-C1-codex-5531d9e34). -/
private theorem drop_two : FirstStepsN (Step prog) (fun c => c.frames.length ≤ 0)
    ⟨.exiting (.ret 7), [], .init, [.seq .skip, emptyScope]⟩ [] ⟨.exiting (.ret 7), [], .init, []⟩ 2 := by
  exact FirstStepsN.step (R := Step prog) (l := .silent) (by decide)
    (Step.silent (P := prog) (c' := ⟨.exiting (.ret 7), [], .init, [emptyScope]⟩) rfl)
    (FirstStepsN.step (R := Step prog) (l := .silent) (by decide) (Step.silent rfl) (.stop (by decide)))

/-- Instance (law `ConcreteSpec.Core.Exec.boundary_complete`, case `raw_drop`). -/
example : ∃ acts, BoundaryRun prog (Step prog) (.ret 7) [] [.seq .skip, emptyScope] [] .init acts [] [] .init :=
  boundary_complete (P := prog) (e := .ret 7) (k := []) (pre := [.seq .skip, emptyScope])
    (L := []) (Lend := []) (st := .init) (stEnd := .init) (tr := []) (n := 2)
    (endC := ⟨.exiting (.ret 7), [], .init, []⟩) drop_two rfl two_passes

/-- Planted (law `ConcreteSpec.Core.Exec.boundary_complete`, case `raw_drop`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CleanupEvidence.PaddedEscape`).
The same two-step raw first arrival takes an unexpected `Ctrl.done`/resume path and cannot be
segmented as direct frame passing or deferred dispatch. -/
example : FirstStepsN (PaddedEscape prog) (fun c => c.frames.length ≤ 0)
      ⟨.exiting (.ret 7), [], .init, [.seq .skip, emptyScope] ++ []⟩ []
      ⟨.exiting (.ret 7), [], .init, []⟩ 2 ∧
    (⟨.exiting (.ret 7), [], .init, []⟩ : Config ops) = ⟨.exiting (.ret 7), [], .init, []⟩ ∧
    (∀ f ∈ [.seq .skip, emptyScope], Passes (O := ops) (.ret 7) f) ∧
    ¬ (∃ acts, BoundaryRun prog (PaddedEscape prog) (.ret 7) [] [.seq .skip, emptyScope]
      [] .init acts [] [] .init) := by
  refine ⟨?_, rfl, two_passes, ?_⟩
  · exact FirstStepsN.step (R := PaddedEscape prog) (l := .silent) (by decide)
      (PaddedEscape.skip (frames := [.seq .skip, emptyScope]) (k := []))
      (FirstStepsN.step (R := PaddedEscape prog) (l := .silent) (by decide)
        PaddedEscape.resume (.stop (by decide)))
  · rintro ⟨_, h⟩
    generalize ht : ([] : List Int) = trace at h
    cases h with
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

/-- Planted mutant of `Step`: performs the local/control transition while ignoring the refused
lifecycle callback. -/
inductive BypassStep (P : Program O) : Config O → Label O → Config O → Prop where
  | bind {x e site L st st' v k} :
      O.eval L st e = .ok (v, st') →
      BypassStep P ⟨.exec (.bind x e site), L, st, k⟩ .silent
        ⟨.done, (x, v) :: L, st', k⟩
  | assign {x e site L st st' v old k} :
      O.eval L st e = .ok (v, st') →
      AList.lookup L x = some old →
      BypassStep P ⟨.exec (.assign x e site), L, st, k⟩ .silent
        ⟨.done, AList.update L x (fun _ => v), st', k⟩
  | enter {dst f args site L st st' vs fn k} :
      evalArgs O L st args = .ok (vs, st') →
      AList.lookup P.fns f = some fn →
      fn.params.length = vs.length →
      BypassStep P ⟨.exec (.call dst f args site), L, st, k⟩ .silent
        ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, st',
          .scope fn.block (fn.params.map Prod.fst) [] :: .ret dst L :: k⟩
  | scope {b dm L st k} :
      BypassStep P ⟨.done, L, st, .scope b dm [] :: k⟩ .silent
        ⟨.done, restrict dm L, st, k⟩
  | exitScope {e b dm L st k} :
      BypassStep P ⟨.exiting e, L, st, .scope b dm [] :: k⟩ .silent
        ⟨.exiting e, restrict dm L, st, k⟩
  | exitTest {e l cr t body site dm L st k} :
      BypassStep P ⟨.exiting e, L, st, .test l cr t body site dm :: k⟩ .silent
        ⟨.exiting e, restrict dm L, st, k⟩
  | test {l cr t body site dm L st st' v go k} :
      O.eval L st t = .ok (v, st') →
      O.test v = some go →
      BypassStep P ⟨.done, L, st, .test l cr t body site dm :: k⟩ .silent
        (if go then ⟨.exec body, restrict dm L, st', .body l cr t body site :: k⟩
         else ⟨.done, restrict dm L, st', k⟩)
  | ret {dst v caller L st k} :
      BypassStep P ⟨.exiting (.ret v), L, st, .ret dst caller :: k⟩ .silent
        ⟨.done, bindDst dst v caller, st, k⟩
  | retImplicit {dst caller L st k} :
      BypassStep P ⟨.done, L, st, .ret dst caller :: k⟩ .silent
        ⟨.done, bindDst dst O.unitVal caller, st, k⟩
  | prim {x p args site L st st' vs k a v} :
      evalArgs O L st args = .ok (vs, st') →
      O.allows p vs st' a = true →
      a.result = .ok v →
      BypassStep P ⟨.exec (.prim (some x) p args site), L, st, k⟩ a.label
        ⟨.done, (x, v) :: L, a.store, k⟩

/-- Planted mutant of `Step`: refuses every transition even when lifecycle hooks succeed. -/
inductive StuckStep (P : Program O) : Config O → Label O → Config O → Prop

/-- Planted mutant of `runFn`: enters the function body without consulting `O.life (.enter ...)`. -/
def bypassRunFn (P : Program O) (ρ : Oracle O) (fuel : Nat) (fn : Fn O) (vs : List O.Val)
    (st : O.Store) : Option (Report O) :=
  some (run P ρ fuel ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, st,
    [.scope fn.block (fn.params.map Prod.fst) []]⟩)

/-- Planted mutant of `runFn`: always refuses entry even when `O.life (.enter ...)` succeeds. -/
def stuckRunFn (_ : Program O) (_ : Oracle O) (_ : Nat) (_ : Fn O) (_ : List O.Val)
    (_ : O.Store) : Option (Report O) :=
  none

/-- Instance (law `ConcreteSpec.Core.Exec.bind_refused`, case `refused`). Under `Refusal.ops`,
`Stmt.bind` `x` is blocked because `Operands.life` on `ops` for `Life.bind` `x` refuses. -/
example : Blocked (Step prog) ⟨.exec (.bind x (.lit 1) s), [], .init, []⟩ ↔
    ops.life (.bind x) DState.init = none :=
  bind_refused prog x (.lit 1) s [] .init .init 1 [] rfl

/-- Planted (law `ConcreteSpec.Core.Exec.bind_refused`, case `refused`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.BypassStep`).
`BypassStep` updates locals despite the refused `Life.bind` `x` lifecycle hook. -/
example : ops.eval [] DState.init (.lit 1) = .ok (1, DState.init) ∧
    ¬ (Blocked (BypassStep prog) ⟨.exec (.bind x (.lit 1) s), [], .init, []⟩ ↔
      ops.life (.bind x) DState.init = none) := by
  refine ⟨rfl, fun h => ?_⟩
  exact h.mpr rfl .silent _ (.bind rfl)

/-- Instance (law `ConcreteSpec.Core.Exec.bind_refused`, case `accepted`). Under `Demo.ops`,
`Stmt.bind` `x` succeeds so the configuration is not blocked. -/
example : Blocked (Step Demo.prog) ⟨.exec (.bind x (.lit 1) s), [], .init, []⟩ ↔
    Demo.ops.life (.bind x) DState.init = none :=
  bind_refused Demo.prog x (.lit 1) s [] .init .init 1 [] rfl

/-- Planted (law `ConcreteSpec.Core.Exec.bind_refused`, case `accepted`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.StuckStep`).
`StuckStep` blocks even when `Operands.life` on `Demo.ops` for `Life.bind` `x` succeeds. -/
example : Demo.ops.eval [] DState.init (.lit 1) = .ok (1, DState.init) ∧
    ¬ (Blocked (StuckStep Demo.prog) ⟨.exec (.bind x (.lit 1) s), [], .init, []⟩ ↔
      Demo.ops.life (.bind x) DState.init = none) := by
  refine ⟨rfl, fun h => ?_⟩
  have hnone := h.mp (fun _ _ hs => nomatch hs)
  cases hnone

/-- Instance (law `ConcreteSpec.Core.Exec.assign_refused`, case `refused`). Under `Refusal.ops`,
`Stmt.assign` `x` is blocked because `Operands.life` on `ops` for `Life.bind` `x` refuses. -/
example : Blocked (Step prog) ⟨.exec (.assign x (.lit 1) s), [(x, 0)], .init, []⟩ ↔
    ops.life (.bind x) DState.init = none :=
  assign_refused prog x (.lit 1) s [(x, 0)] .init .init 1 0 [] rfl rfl

/-- Planted (law `ConcreteSpec.Core.Exec.assign_refused`, case `refused`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.BypassStep`).
`BypassStep` updates locals despite the refused `Life.bind` `x` lifecycle hook. -/
example : ops.eval [(x, 0)] DState.init (.lit 1) = .ok (1, DState.init) ∧
    AList.lookup [(x, 0)] x = some 0 ∧
    ¬ (Blocked (BypassStep prog) ⟨.exec (.assign x (.lit 1) s), [(x, 0)], .init, []⟩ ↔
      ops.life (.bind x) DState.init = none) := by
  refine ⟨rfl, rfl, fun h => ?_⟩
  exact h.mpr rfl .silent _ (.assign rfl rfl)

/-- Instance (law `ConcreteSpec.Core.Exec.assign_refused`, case `accepted`). Under `Demo.ops`,
`Stmt.assign` `x` succeeds so the configuration is not blocked. -/
example : Blocked (Step Demo.prog) ⟨.exec (.assign x (.lit 1) s), [(x, 0)], .init, []⟩ ↔
    Demo.ops.life (.bind x) DState.init = none :=
  assign_refused Demo.prog x (.lit 1) s [(x, 0)] .init .init 1 0 [] rfl rfl

/-- Planted (law `ConcreteSpec.Core.Exec.assign_refused`, case `accepted`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.StuckStep`).
`StuckStep` blocks even when `Operands.life` on `Demo.ops` for `Life.bind` `x` succeeds. -/
example : Demo.ops.eval [(x, 0)] DState.init (.lit 1) = .ok (1, DState.init) ∧
    AList.lookup [(x, 0)] x = some 0 ∧
    ¬ (Blocked (StuckStep Demo.prog) ⟨.exec (.assign x (.lit 1) s), [(x, 0)], .init, []⟩ ↔
      Demo.ops.life (.bind x) DState.init = none) := by
  refine ⟨rfl, rfl, fun h => ?_⟩
  have hnone := h.mp (fun _ _ hs => nomatch hs)
  cases hnone

/-- Instance (law `ConcreteSpec.Core.Exec.enter_refused`, case `refused`). Under `Refusal.ops`,
top-level `runFn` is refused because `Operands.life` on `ops` for `Life.enter` returns `none`. -/
example : runFn prog Demo.env 1 fn [] .init = none ↔
    ops.life (.enter (fn.params.map Prod.fst)) DState.init = none :=
  enter_refused prog Demo.env 1 fn [] .init rfl

/-- Planted (law `ConcreteSpec.Core.Exec.enter_refused`, case `refused`, subject `ConcreteSpec.Core.Exec.runFn`, mutant `ConcreteSpec.Core.Exec.Refusal.bypassRunFn`).
`bypassRunFn` enters the function despite the refused `Life.enter` lifecycle hook. -/
example : fn.params.length = ([] : List Int).length ∧
    ¬ (bypassRunFn prog Demo.env 1 fn [] .init = none ↔
      ops.life (.enter (fn.params.map Prod.fst)) DState.init = none) := by
  refine ⟨rfl, fun h => ?_⟩
  have hnone := h.mpr rfl
  cases hnone

/-- Instance (law `ConcreteSpec.Core.Exec.enter_refused`, case `accepted`). Under `Demo.ops`,
top-level `runFn` succeeds when `Life.enter` succeeds. -/
example : runFn Demo.prog Demo.env 1 Demo.bumpFn [] .init = none ↔
    Demo.ops.life (.enter (Demo.bumpFn.params.map Prod.fst)) DState.init = none :=
  enter_refused Demo.prog Demo.env 1 Demo.bumpFn [] .init rfl

/-- Planted (law `ConcreteSpec.Core.Exec.enter_refused`, case `accepted`, subject `ConcreteSpec.Core.Exec.runFn`, mutant `ConcreteSpec.Core.Exec.Refusal.stuckRunFn`).
`stuckRunFn` returns `none` even when `Operands.life` on `Demo.ops` for `Life.enter` succeeds. -/
example : Demo.bumpFn.params.length = ([] : List Int).length ∧
    ¬ (stuckRunFn Demo.prog Demo.env 1 Demo.bumpFn [] .init = none ↔
      Demo.ops.life (.enter (Demo.bumpFn.params.map Prod.fst)) DState.init = none) := by
  refine ⟨rfl, fun h => ?_⟩
  have hnone := h.mp rfl
  cases hnone

/-- Instance (law `ConcreteSpec.Core.Exec.call_refused`, case `refused`). Under `Refusal.ops`,
calling `bumpF` is blocked because `Operands.life` on `ops` for `Life.enter` refuses. -/
example : Blocked (Step prog) ⟨.exec (.call none bumpF [] s), [], .init, []⟩ ↔
    ops.life (.enter (fn.params.map Prod.fst)) DState.init = none :=
  call_refused prog none bumpF [] s [] .init .init [] fn [] rfl rfl rfl

/-- Planted (law `ConcreteSpec.Core.Exec.call_refused`, case `refused`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.BypassStep`).
`BypassStep` enters the callee despite the refused `Life.enter` lifecycle hook. -/
example : evalArgs ops [] DState.init [] = .ok ([], DState.init) ∧
    AList.lookup prog.fns bumpF = some fn ∧
    fn.params.length = ([] : List Int).length ∧
    ¬ (Blocked (BypassStep prog) ⟨.exec (.call none bumpF [] s), [], .init, []⟩ ↔
      ops.life (.enter (fn.params.map Prod.fst)) DState.init = none) := by
  refine ⟨rfl, rfl, rfl, fun h => ?_⟩
  exact h.mpr rfl .silent _ (.enter rfl rfl rfl)

/-- Instance (law `ConcreteSpec.Core.Exec.call_refused`, case `accepted`). Under `Demo.ops`,
calling `bumpF` is not blocked when `Operands.life` on `Demo.ops` for `Life.enter` succeeds. -/
example : Blocked (Step Demo.prog) ⟨.exec (.call none bumpF [] s), [], .init, []⟩ ↔
    Demo.ops.life (.enter (Demo.bumpFn.params.map Prod.fst)) DState.init = none :=
  call_refused Demo.prog none bumpF [] s [] .init .init [] Demo.bumpFn [] rfl rfl rfl

/-- Planted (law `ConcreteSpec.Core.Exec.call_refused`, case `accepted`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.StuckStep`).
`StuckStep` blocks the call even when `Operands.life` on `Demo.ops` for `Life.enter` succeeds. -/
example : evalArgs Demo.ops [] DState.init [] = .ok ([], DState.init) ∧
    AList.lookup Demo.prog.fns bumpF = some Demo.bumpFn ∧
    Demo.bumpFn.params.length = ([] : List Int).length ∧
    ¬ (Blocked (StuckStep Demo.prog) ⟨.exec (.call none bumpF [] s), [], .init, []⟩ ↔
      Demo.ops.life (.enter (Demo.bumpFn.params.map Prod.fst)) DState.init = none) := by
  refine ⟨rfl, rfl, rfl, fun h => ?_⟩
  have hnone := h.mp (fun _ _ hs => nomatch hs)
  cases hnone

/-- Instance (law `ConcreteSpec.Core.Exec.scope_drop_refused`, case `refused`). Under `Refusal.ops`,
normal scope end is blocked because `dropOut` refuses. -/
example : Blocked (Step prog) ⟨.done, [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ ↔
    dropOut ops [] [(x, 1)] DState.init = none :=
  scope_drop_refused prog ⟨0⟩ [] [(x, 1)] .init []

/-- Planted (law `ConcreteSpec.Core.Exec.scope_drop_refused`, case `refused`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.BypassStep`).
`BypassStep` pops the scope despite the refused `dropOut`. -/
example : ¬ (Blocked (BypassStep prog) ⟨.done, [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ ↔
    dropOut ops [] [(x, 1)] DState.init = none) := by
  intro h
  exact h.mpr rfl .silent _ .scope

/-- Instance (law `ConcreteSpec.Core.Exec.scope_drop_refused`, case `accepted`). Under `Demo.ops`,
normal scope end is not blocked when `dropOut` succeeds. -/
example : Blocked (Step Demo.prog) ⟨.done, [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ ↔
    dropOut Demo.ops [] [(x, 1)] DState.init = none :=
  scope_drop_refused Demo.prog ⟨0⟩ [] [(x, 1)] .init []

/-- Planted (law `ConcreteSpec.Core.Exec.scope_drop_refused`, case `accepted`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.StuckStep`).
`StuckStep` blocks scope exit even when `dropOut` succeeds. -/
example : ¬ (Blocked (StuckStep Demo.prog) ⟨.done, [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ ↔
    dropOut Demo.ops [] [(x, 1)] DState.init = none) := by
  intro h
  have hnone := h.mp (fun _ _ hs => nomatch hs)
  cases hnone

/-- Instance (law `ConcreteSpec.Core.Exec.exit_drop_refused`, case `refused`). Under `Refusal.ops`,
structured exit through an empty scope is blocked because `dropOut` refuses. -/
example : Blocked (Step prog) ⟨.exiting (.ret 7), [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ ↔
    dropOut ops [] [(x, 1)] DState.init = none :=
  exit_drop_refused prog (.ret 7) ⟨0⟩ [] [(x, 1)] .init []

/-- Planted (law `ConcreteSpec.Core.Exec.exit_drop_refused`, case `refused`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.BypassStep`).
`BypassStep` pops the scope on exit despite the refused `dropOut`. -/
example : ¬ (Blocked (BypassStep prog) ⟨.exiting (.ret 7), [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ ↔
    dropOut ops [] [(x, 1)] DState.init = none) := by
  intro h
  exact h.mpr rfl .silent _ .exitScope

/-- Instance (law `ConcreteSpec.Core.Exec.exit_drop_refused`, case `accepted`). Under `Demo.ops`,
structured exit through an empty scope is not blocked when `dropOut` succeeds. -/
example : Blocked (Step Demo.prog) ⟨.exiting (.ret 7), [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ ↔
    dropOut Demo.ops [] [(x, 1)] DState.init = none :=
  exit_drop_refused Demo.prog (.ret 7) ⟨0⟩ [] [(x, 1)] .init []

/-- Planted (law `ConcreteSpec.Core.Exec.exit_drop_refused`, case `accepted`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.StuckStep`).
`StuckStep` blocks scope exit even when `dropOut` succeeds. -/
example : ¬ (Blocked (StuckStep Demo.prog) ⟨.exiting (.ret 7), [(x, 1)], .init, [.scope ⟨0⟩ [] []]⟩ ↔
    dropOut Demo.ops [] [(x, 1)] DState.init = none) := by
  intro h
  have hnone := h.mp (fun _ _ hs => nomatch hs)
  cases hnone

/-- Instance (law `ConcreteSpec.Core.Exec.exit_test_drop_refused`, case `refused`). Under
`Refusal.ops`, structured exit through a test frame is blocked because `dropOut` refuses. -/
example : Blocked (Step prog) ⟨.exiting (.ret 7), [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ ↔
    dropOut ops [] [(x, 1)] DState.init = none :=
  exit_test_drop_refused prog (.ret 7) [] ⟨0⟩ .skip .skip (.lit 1) s [(x, 1)] .init []

/-- Planted (law `ConcreteSpec.Core.Exec.exit_test_drop_refused`, case `refused`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.BypassStep`).
`BypassStep` pops the test frame on exit despite the refused `dropOut`. -/
example : ¬ (Blocked (BypassStep prog) ⟨.exiting (.ret 7), [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ ↔
    dropOut ops [] [(x, 1)] DState.init = none) := by
  intro h
  exact h.mpr rfl .silent _ .exitTest

/-- Instance (law `ConcreteSpec.Core.Exec.exit_test_drop_refused`, case `accepted`). Under
`Demo.ops`, structured exit through a test frame is not blocked when `dropOut` succeeds. -/
example : Blocked (Step Demo.prog) ⟨.exiting (.ret 7), [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ ↔
    dropOut Demo.ops [] [(x, 1)] DState.init = none :=
  exit_test_drop_refused Demo.prog (.ret 7) [] ⟨0⟩ .skip .skip (.lit 1) s [(x, 1)] .init []

/-- Planted (law `ConcreteSpec.Core.Exec.exit_test_drop_refused`, case `accepted`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.StuckStep`).
`StuckStep` blocks test-frame exit even when `dropOut` succeeds. -/
example : ¬ (Blocked (StuckStep Demo.prog) ⟨.exiting (.ret 7), [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ ↔
    dropOut Demo.ops [] [(x, 1)] DState.init = none) := by
  intro h
  have hnone := h.mp (fun _ _ hs => nomatch hs)
  cases hnone

/-- Instance (law `ConcreteSpec.Core.Exec.test_drop_refused`, case `refused`). Under `Refusal.ops`,
loop test evaluation is blocked after boolean test when `dropOut` refuses. -/
example : Blocked (Step prog) ⟨.done, [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ ↔
    dropOut ops [] [(x, 1)] DState.init = none :=
  test_drop_refused prog ⟨0⟩ .skip .skip (.lit 1) s [] [(x, 1)] .init .init 1 true [] rfl rfl

/-- Planted (law `ConcreteSpec.Core.Exec.test_drop_refused`, case `refused`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.BypassStep`).
`BypassStep` enters the loop body despite the refused `dropOut`. -/
example : ops.eval [(x, 1)] DState.init (.lit 1) = .ok (1, DState.init) ∧
    ops.test (1 : Int) = some true ∧
    ¬ (Blocked (BypassStep prog) ⟨.done, [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ ↔
      dropOut ops [] [(x, 1)] DState.init = none) := by
  refine ⟨rfl, rfl, fun h => ?_⟩
  exact h.mpr rfl .silent _ (.test rfl rfl)

/-- Instance (law `ConcreteSpec.Core.Exec.test_drop_refused`, case `accepted`). Under `Demo.ops`,
loop test evaluation is not blocked when `dropOut` succeeds. -/
example : Blocked (Step Demo.prog) ⟨.done, [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ ↔
    dropOut Demo.ops [] [(x, 1)] DState.init = none :=
  test_drop_refused Demo.prog ⟨0⟩ .skip .skip (.lit 1) s [] [(x, 1)] .init .init 1 true [] rfl rfl

/-- Planted (law `ConcreteSpec.Core.Exec.test_drop_refused`, case `accepted`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.StuckStep`).
`StuckStep` blocks loop test progress even when `dropOut` succeeds. -/
example : Demo.ops.eval [(x, 1)] DState.init (.lit 1) = .ok (1, DState.init) ∧
    Demo.ops.test (1 : Int) = some true ∧
    ¬ (Blocked (StuckStep Demo.prog) ⟨.done, [(x, 1)], .init, [.test ⟨0⟩ .skip (.lit 1) .skip s []]⟩ ↔
      dropOut Demo.ops [] [(x, 1)] DState.init = none) := by
  refine ⟨rfl, rfl, fun h => ?_⟩
  have hnone := h.mp (fun _ _ hs => nomatch hs)
  cases hnone

/-- Instance (law `ConcreteSpec.Core.Exec.return_refused`, case `explicit_refused`). Under
`Refusal.ops`, explicit return is blocked because `Life.leave` refuses. -/
example : (Blocked (Step prog) ⟨.exiting (.ret 7), [], .init, [.ret (some x) [(y, 2)]]⟩ ↔
      (ops.life .leave DState.init).bind (bindLife ops (some x)) = none) ∧
    (Blocked (Step prog) ⟨.done, [], .init, [.ret (some x) [(y, 2)]]⟩ ↔
      (ops.life .leave DState.init).bind (bindLife ops (some x)) = none) :=
  return_refused prog (some x) 7 [(y, 2)] [] .init []

/-- Planted (law `ConcreteSpec.Core.Exec.return_refused`, case `explicit_refused`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.BypassStep`).
`BypassStep` returns to the caller despite the refused `Life.leave` lifecycle hook. -/
example : ¬ ((Blocked (BypassStep prog) ⟨.exiting (.ret 7), [], .init, [.ret (some x) [(y, 2)]]⟩ ↔
      (ops.life .leave DState.init).bind (bindLife ops (some x)) = none) ∧
    (Blocked (BypassStep prog) ⟨.done, [], .init, [.ret (some x) [(y, 2)]]⟩ ↔
      (ops.life .leave DState.init).bind (bindLife ops (some x)) = none)) := by
  intro h
  exact h.1.mpr rfl .silent _ .ret

/-- Instance (law `ConcreteSpec.Core.Exec.return_refused`, case `implicit_refused`). Under
`Refusal.ops`, implicit Unit return is also blocked when `Life.leave` refuses. -/
example : (Blocked (Step prog) ⟨.exiting (.ret 7), [], .init, [.ret none []]⟩ ↔
      (ops.life .leave DState.init).bind (bindLife ops none) = none) ∧
    (Blocked (Step prog) ⟨.done, [], .init, [.ret none []]⟩ ↔
      (ops.life .leave DState.init).bind (bindLife ops none) = none) :=
  return_refused prog none 7 [] [] .init []

/-- Planted (law `ConcreteSpec.Core.Exec.return_refused`, case `implicit_refused`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.BypassStep`).
`BypassStep` performs the implicit return despite the refused `Life.leave` lifecycle hook. -/
example : ¬ ((Blocked (BypassStep prog) ⟨.exiting (.ret 7), [], .init, [.ret none []]⟩ ↔
      (ops.life .leave DState.init).bind (bindLife ops none) = none) ∧
    (Blocked (BypassStep prog) ⟨.done, [], .init, [.ret none []]⟩ ↔
      (ops.life .leave DState.init).bind (bindLife ops none) = none)) := by
  intro h
  exact h.2.mpr rfl .silent _ .retImplicit

/-- Only result initialization refuses; activation leave succeeds independently. -/
abbrev bindOnlyOps : Operands := { Demo.ops with life := fun e st => match e with
    | .bind _ => none
    | _ => some (DState.life e st) }
def bindOnlyProg : Program bindOnlyOps := ⟨[]⟩

/-- Instance (law `ConcreteSpec.Core.Exec.return_refused`, case `bind_only_refused`). When `Life.leave`
succeeds and `Life.bind` on `x` alone refuses, return is still blocked. -/
example : (Blocked (Step bindOnlyProg) ⟨.exiting (.ret 7), [], .init, [.ret (some x) []]⟩ ↔
      (bindOnlyOps.life .leave DState.init).bind (bindLife bindOnlyOps (some x)) = none) ∧
    (Blocked (Step bindOnlyProg) ⟨.done, [], .init, [.ret (some x) []]⟩ ↔
      (bindOnlyOps.life .leave DState.init).bind (bindLife bindOnlyOps (some x)) = none) :=
  return_refused bindOnlyProg (some x) 7 [] [] .init []

/-- Planted (law `ConcreteSpec.Core.Exec.return_refused`, case `bind_only_refused`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.BypassStep`).
Successful `Life.leave` does not permit bypassing the refused result `Life.bind` on `x`. -/
example : ¬ ((Blocked (BypassStep bindOnlyProg) ⟨.exiting (.ret 7), [], .init, [.ret (some x) []]⟩ ↔
      (bindOnlyOps.life .leave DState.init).bind (bindLife bindOnlyOps (some x)) = none) ∧
    (Blocked (BypassStep bindOnlyProg) ⟨.done, [], .init, [.ret (some x) []]⟩ ↔
      (bindOnlyOps.life .leave DState.init).bind (bindLife bindOnlyOps (some x)) = none)) := by
  intro h
  exact h.1.mpr rfl .silent _ .ret

/-- Instance (law `ConcreteSpec.Core.Exec.return_refused`, case `accepted`). Under `Demo.ops`,
both `Life.leave` and `Life.bind` on `x` succeed so return is not blocked. -/
example : (Blocked (Step Demo.prog) ⟨.exiting (.ret 7), [], .init, [.ret (some x) [(y, 2)]]⟩ ↔
      (Demo.ops.life .leave DState.init).bind (bindLife Demo.ops (some x)) = none) ∧
    (Blocked (Step Demo.prog) ⟨.done, [], .init, [.ret (some x) [(y, 2)]]⟩ ↔
      (Demo.ops.life .leave DState.init).bind (bindLife Demo.ops (some x)) = none) :=
  return_refused Demo.prog (some x) 7 [(y, 2)] [] .init []

/-- Planted (law `ConcreteSpec.Core.Exec.return_refused`, case `accepted`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.StuckStep`).
`StuckStep` blocks return even when both `Life.leave` and `Life.bind` on `x` succeed. -/
example : ¬ ((Blocked (StuckStep Demo.prog) ⟨.exiting (.ret 7), [], .init, [.ret (some x) [(y, 2)]]⟩ ↔
      (Demo.ops.life .leave DState.init).bind (bindLife Demo.ops (some x)) = none) ∧
    (Blocked (StuckStep Demo.prog) ⟨.done, [], .init, [.ret (some x) [(y, 2)]]⟩ ↔
      (Demo.ops.life .leave DState.init).bind (bindLife Demo.ops (some x)) = none)) := by
  intro h
  have hnone := h.1.mp (fun _ _ hs => nomatch hs)
  cases hnone

/-- Concrete request continuation for `.prim (some x) .log [.lit 1] s` with empty locals/frames. -/
def logCont (O : Operands) (a : Answer O.Val O.Event O.Store) : Option (Config O) :=
  match a.result with
  | .ok v => (bindLife O (some x) a.store).map fun st => ⟨.done, bindDst (some x) v [], st, []⟩
  | .error kd => some ⟨.trapped kd s, [], a.store, []⟩

/-- Planted mutant of `runFrom`: commits an allowed answer's event and finishes even when
its continuation `cont a` returns `none`. -/
def bypassRunFrom (P : Program O) (ρ : Oracle O) (fuel : Nat) (hist : List O.Event)
    (c : Config O) : Report O :=
  match c.final?, fuel with
  | some f, _ => ⟨hist, .final f⟩
  | none, 0 => ⟨hist, .outOfFuel c⟩
  | none, n + 1 =>
    match step P c with
    | none => ⟨hist, .stuck c⟩
    | some (.next c') => runFrom P ρ n hist c'
    | some (.request p vs site st cont) =>
      match ρ hist p vs site st with
      | none => ⟨hist, .noAnswer c⟩
      | some a =>
        if O.allows p vs st a then
          match cont a with
          | some c' => runFrom P ρ n (hist ++ a.event.toList) c'
          | none => ⟨hist ++ a.event.toList, .final (.returned O.unitVal)⟩
        else
          ⟨hist, .badAnswer c⟩

/-- Planted mutant of `runFrom`: always reports `.stuck c` without executing the continuation. -/
def stuckRunFrom (_ : Program O) (_ : Oracle O) (_ : Nat) (hist : List O.Event)
    (c : Config O) : Report O :=
  ⟨hist, .stuck c⟩

/-- Instance (law `ConcreteSpec.Core.Exec.continuation_refused`, case `refused`). Under
`Refusal.ops`, `logCont` returns `none` so a one-step run is stuck with unchanged history. -/
example : runFrom prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
      ⟨[9], .stuck ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩⟩ ↔
    logCont ops ⟨some 1, .ok 0, .init⟩ = none :=
  continuation_refused prog Demo.env [9]
    ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ .log [1] s .init
    (logCont ops) ⟨some 1, .ok 0, .init⟩ rfl rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Exec.continuation_refused`, case `refused`, subject `ConcreteSpec.Core.Exec.runFrom`, mutant `ConcreteSpec.Core.Exec.Refusal.bypassRunFrom`).
`bypassRunFrom` accepts the answer event 1 despite `logCont` on `ops` returning `none`. -/
example : (⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ : Config ops).final? = none ∧
    step prog ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
      some (.request .log [1] s .init (logCont ops)) ∧
    Demo.env [9] .log [1] s .init = some ⟨some 1, .ok 0, .init⟩ ∧
    ops.allows .log [1] .init ⟨some 1, .ok 0, .init⟩ = true ∧
    ¬ (bypassRunFrom prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
        ⟨[9], .stuck ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩⟩ ↔
      logCont ops ⟨some 1, .ok 0, .init⟩ = none) := by
  refine ⟨rfl, rfl, rfl, by decide, fun h => ?_⟩
  have hrep := h.mpr rfl
  have hev := congrArg Report.events hrep
  change [9, 1] = [9] at hev
  cases hev

/-- Instance (law `ConcreteSpec.Core.Exec.continuation_refused`, case `accepted`). Under
`Demo.ops`, `logCont` returns `some` so a one-step run is not stuck. -/
example : runFrom Demo.prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
      ⟨[9], .stuck ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩⟩ ↔
    logCont Demo.ops ⟨some 1, .ok 0, .init⟩ = none :=
  continuation_refused Demo.prog Demo.env [9]
    ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ .log [1] s .init
    (logCont Demo.ops) ⟨some 1, .ok 0, .init⟩ rfl rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Exec.continuation_refused`, case `accepted`, subject `ConcreteSpec.Core.Exec.runFrom`, mutant `ConcreteSpec.Core.Exec.Refusal.stuckRunFrom`).
`stuckRunFrom` reports `Ending.stuck` even when `logCont` on `Demo.ops` succeeds. -/
example : (⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ : Config Demo.ops).final? = none ∧
    step Demo.prog ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
      some (.request .log [1] s .init (logCont Demo.ops)) ∧
    Demo.env [9] .log [1] s .init = some ⟨some 1, .ok 0, .init⟩ ∧
    Demo.ops.allows .log [1] .init ⟨some 1, .ok 0, .init⟩ = true ∧
    ¬ (stuckRunFrom Demo.prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
        ⟨[9], .stuck ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩⟩ ↔
      logCont Demo.ops ⟨some 1, .ok 0, .init⟩ = none) := by
  refine ⟨rfl, rfl, rfl, by decide, fun h => ?_⟩
  have hnone := h.mp rfl
  cases hnone

/-- Instance (law `ConcreteSpec.Core.Exec.prim_bind_refused`, case `refused`). Under
`Refusal.ops`, `Stmt.prim` with `Pr.log` and destination `x` is stuck on one step because `Operands.life` on `Life.bind` refuses. -/
example : runFrom prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
      ⟨[9], .stuck ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩⟩ ↔
    ops.life (.bind x) (⟨some 1, .ok 0, .init⟩ : Answer Int Int DState).store = none :=
  prim_bind_refused prog Demo.env [9] x .log [.lit 1] s [] .init .init [1] []
    ⟨some 1, .ok 0, .init⟩ 0 rfl rfl (by decide) rfl

/-- Planted (law `ConcreteSpec.Core.Exec.prim_bind_refused`, case `refused`, subject `ConcreteSpec.Core.Exec.runFrom`, mutant `ConcreteSpec.Core.Exec.Refusal.bypassRunFrom`).
`bypassRunFrom` accepts the primitive answer's event 1 despite the refused `Life.bind` hook on `x`. -/
example : evalArgs ops [] DState.init [.lit 1] = .ok ([1], DState.init) ∧
    Demo.env [9] .log [1] s DState.init = some ⟨some 1, .ok 0, .init⟩ ∧
    ops.allows .log [1] DState.init ⟨some 1, .ok 0, .init⟩ = true ∧
    (⟨some 1, .ok 0, .init⟩ : Answer Int Int DState).result = .ok 0 ∧
    ¬ (bypassRunFrom prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
        ⟨[9], .stuck ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩⟩ ↔
      ops.life (.bind x) (⟨some 1, .ok 0, .init⟩ : Answer Int Int DState).store = none) := by
  refine ⟨rfl, rfl, by decide, rfl, fun h => ?_⟩
  have hrep := h.mpr rfl
  have hev := congrArg Report.events hrep
  change [9, 1] = [9] at hev
  cases hev

/-- Instance (law `ConcreteSpec.Core.Exec.prim_bind_refused`, case `accepted`). Under
`Demo.ops`, `Stmt.prim` with `Pr.log` and destination `x` is not stuck because `Operands.life` on `Life.bind` succeeds. -/
example : runFrom Demo.prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
      ⟨[9], .stuck ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩⟩ ↔
    Demo.ops.life (.bind x) (⟨some 1, .ok 0, .init⟩ : Answer Int Int DState).store = none :=
  prim_bind_refused Demo.prog Demo.env [9] x .log [.lit 1] s [] .init .init [1] []
    ⟨some 1, .ok 0, .init⟩ 0 rfl rfl (by decide) rfl

/-- Planted (law `ConcreteSpec.Core.Exec.prim_bind_refused`, case `accepted`, subject `ConcreteSpec.Core.Exec.runFrom`, mutant `ConcreteSpec.Core.Exec.Refusal.stuckRunFrom`).
`stuckRunFrom` reports `Ending.stuck` even when `Operands.life` on `Life.bind` succeeds under `Demo.ops`. -/
example : evalArgs Demo.ops [] DState.init [.lit 1] = .ok ([1], DState.init) ∧
    Demo.env [9] .log [1] s DState.init = some ⟨some 1, .ok 0, .init⟩ ∧
    Demo.ops.allows .log [1] DState.init ⟨some 1, .ok 0, .init⟩ = true ∧
    (⟨some 1, .ok 0, .init⟩ : Answer Int Int DState).result = .ok 0 ∧
    ¬ (stuckRunFrom Demo.prog Demo.env 1 [9] ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ =
        ⟨[9], .stuck ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩⟩ ↔
      Demo.ops.life (.bind x) (⟨some 1, .ok 0, .init⟩ : Answer Int Int DState).store = none) := by
  refine ⟨rfl, rfl, by decide, rfl, fun h => ?_⟩
  have hnone := h.mp rfl
  cases hnone

/-- Instance (law `ConcreteSpec.Core.Exec.prim_bind_step`, case `refused`). Under `Refusal.ops`,
the relational primitive call is blocked because every allowed `Pr.log` answer succeeds with `Except.ok`
and `Operands.life` on `Life.bind` refuses. -/
example : Blocked (Step prog) ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ ↔
    ∀ a, ops.allows .log [1] DState.init a = true →
      ∃ v, a.result = .ok v ∧ ops.life (.bind x) a.store = none :=
  prim_bind_step prog x .log [.lit 1] s [] .init .init [1] [] rfl

/-- Planted (law `ConcreteSpec.Core.Exec.prim_bind_step`, case `refused`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.BypassStep`).
`BypassStep.prim` commits the primitive answer despite the refused `Life.bind` lifecycle hook on `x`. -/
example : evalArgs ops [] DState.init [.lit 1] = .ok ([1], DState.init) ∧
    ¬ (Blocked (BypassStep prog) ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ ↔
      ∀ a, ops.allows .log [1] DState.init a = true →
        ∃ v, a.result = .ok v ∧ ops.life (.bind x) a.store = none) := by
  refine ⟨rfl, fun h => ?_⟩
  have hall : ∀ a, ops.allows .log [1] DState.init a = true →
      ∃ v, a.result = .ok v ∧ ops.life (.bind x) a.store = none := by
    intro a ha
    cases hev : a.event with
    | none => simp [ops, Demo.ops, hev] at ha
    | some ev =>
      cases hres : a.result with
      | error _ => simp [ops, Demo.ops, hev, hres] at ha
      | ok v => exact ⟨v, rfl, rfl⟩
  exact h.mpr hall (.emit 1) _
    (BypassStep.prim (P := prog) (a := ⟨some 1, .ok 0, .init⟩) rfl (by decide) rfl)

/-- Instance (law `ConcreteSpec.Core.Exec.prim_bind_step`, case `accepted`). Under `Demo.ops`,
the relational primitive call is not blocked because `Operands.life` on `Life.bind` succeeds. -/
example : Blocked (Step Demo.prog) ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ ↔
    ∀ a, Demo.ops.allows .log [1] DState.init a = true →
      ∃ v, a.result = .ok v ∧ Demo.ops.life (.bind x) a.store = none :=
  prim_bind_step Demo.prog x .log [.lit 1] s [] .init .init [1] [] rfl

/-- Planted (law `ConcreteSpec.Core.Exec.prim_bind_step`, case `accepted`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.StuckStep`).
`StuckStep` blocks the relational primitive call even when `Operands.life` on `Life.bind` succeeds under `Demo.ops`. -/
example : evalArgs Demo.ops [] DState.init [.lit 1] = .ok ([1], DState.init) ∧
    ¬ (Blocked (StuckStep Demo.prog) ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩ ↔
      ∀ a, Demo.ops.allows .log [1] DState.init a = true →
        ∃ v, a.result = .ok v ∧ Demo.ops.life (.bind x) a.store = none) := by
  refine ⟨rfl, fun h => ?_⟩
  obtain ⟨_, _, hnone⟩ := h.mp (fun _ _ hs => nomatch hs) ⟨some 1, .ok 0, .init⟩ (by decide)
  cases hnone

/-- Error-capable operand contract: the environment may trap instead of producing a value. -/
abbrev errorOps : Operands := { ops with allows := fun _ _ _ _ => true }
def errorProg : Program errorOps := ⟨[]⟩
def errAns : Answer errorOps.Val errorOps.Event errorOps.Store := ⟨some 1, .error .abort, .init⟩

/-- Planted mutant of `Step`: incorrectly requires `O.life (.bind x)` even on error answers. -/
inductive BindErrorsStep (P : Program O) : Config O → Label O → Config O → Prop where
  | prim {x p args site L st st' vs k a stBind c'} :
      evalArgs O L st args = .ok (vs, st') →
      O.allows p vs st' a = true →
      O.life (.bind x) a.store = some stBind →
      c' = (match a.result with
        | .ok v => ⟨.done, (x, v) :: L, stBind, k⟩
        | .error kd => ⟨.trapped kd site, L, stBind, k⟩) →
      BindErrorsStep P ⟨.exec (.prim (some x) p args site), L, st, k⟩ a.label c'

/-- Instance (law `ConcreteSpec.Core.Exec.prim_error_bypasses_bind`, case `error_trap`). The bind
callback refuses, but an allowed error answer still produces a real labeled trap transition. -/
example : Step errorProg
    ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩
    errAns.label
    ⟨.trapped .abort s, [], errAns.store, []⟩ :=
  prim_error_bypasses_bind errorProg x .log [.lit 1] s [] .init .init [1] []
    errAns .abort rfl rfl rfl

/-- Planted (law `ConcreteSpec.Core.Exec.prim_error_bypasses_bind`, case `error_trap`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.BindErrorsStep`).
`BindErrorsStep` consults `Operands.life` on `Life.bind` under `errorOps` for error answers and therefore cannot take
the trap transition when `Life.bind` on `x` refuses. -/
example : evalArgs errorOps [] DState.init [.lit 1] = .ok ([1], DState.init) ∧
    errorOps.allows .log [1] DState.init errAns = true ∧
    errAns.result = .error .abort ∧
    ¬ BindErrorsStep errorProg
      ⟨.exec (.prim (some x) .log [.lit 1] s), [], .init, []⟩
      errAns.label
      ⟨.trapped .abort s, [], errAns.store, []⟩ := by
  refine ⟨rfl, rfl, rfl, fun h => ?_⟩
  generalize hl : errAns.label = label at h
  cases h with
  | prim _ _ hb _ => cases hb

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

/-! ### Core contract law instances and planted mutations -/
namespace CoreEvidence
open Demo

/-- Planted mutant of `Steps`: only zero-step reflexive traces exist; no transition can advance. -/
inductive ReflOnlySteps (P : Program O) : Config O → List O.Event → Config O → Prop where
  | refl (c : Config O) : ReflOnlySteps P c [] c

/-- Planted mutant of `Steps`: a trapped configuration spuriously recovers to `.done`. -/
inductive RecoverTrapSteps (P : Program O) : Config O → List O.Event → Config O → Prop where
  | recover {kd site L st k} :
      RecoverTrapSteps P ⟨.trapped kd site, L, st, k⟩ [] ⟨.done, L, st, k⟩

/-- Planted mutant of `restrict`: drops every binding regardless of `dom`. -/
def dropAllRestrict (_ : List Var) (_ : AList Var V) : AList Var V := []

/-- Planted mutant of `restrict`: retains every binding regardless of `dom`. -/
def keepAllRestrict (_ : List Var) (L : AList Var V) : AList Var V := L

/-- Planted mutant of `step`: always returns `none`. -/
def stuckStep (_ : Program O) (_ : Config O) : Option (Out O) := none

/-- Planted mutant of `planWith`: reverses the planned cleanup action order. -/
def reversePlanWith (pre : List (Frame O)) (L : AList Var O.Val) : List (CleanAct O) :=
  (planWith pre L).reverse

/-- Planted mutant of `plan`: forgets all planned deferred calls. -/
def emptyPlan (_ : List (Frame O)) : List (Deferred O) := []

/-- Planted mutant of `Config.final?`: never recognizes a final configuration. -/
def noTrapFinal? (_ : Config O) : Option (Final O.Val) := none

/-- Planted mutant of `Step`: a trapped configuration spuriously transitions to `.done`. -/
inductive RecoverTrapStep (P : Program O) : Config O → Label O → Config O → Prop where
  | recover {kd site L st k} :
      RecoverTrapStep P ⟨.trapped kd site, L, st, k⟩ .silent ⟨.done, L, st, k⟩

/-- Planted mutant of `evalArgs`: swallows an argument trap and returns `.ok ([], st)`. -/
def swallowTrapEvalArgs (O : Operands) (L : AList Var O.Val) :
    O.Store → List O.Expr → Res (List O.Val × O.Store)
  | st, [] => .ok ([], st)
  | st, e :: es => match O.eval L st e with
    | .ok (v, st₁) => (swallowTrapEvalArgs O L st₁ es).map fun (vs, st₂) => (v :: vs, st₂)
    | .trap _ => .ok ([], st)
    | .stuck => .stuck

/-- Planted mutant of `validDestroyFn`: rejects every destroy candidate. -/
def rejectAllDestroyFn [DecidableEq O.Ty] (_ : Program O) (_ : O.Ty) (_ : FnId) : Bool :=
  false

/-- Planted mutant of `validDestroyFn`: checks signature in `P.fns` but forgets `!O.isCopy T`. -/
def allowCopyDestroyFn [DecidableEq O.Ty] (P : Program O) (T : O.Ty) (f : FnId) : Bool :=
  match AList.lookup P.fns f with
  | some fn =>
    match fn.params with
    | [(_, paramTy)] => decide (paramTy = T) && decide (fn.result = O.unitTy)
    | _ => false
  | none => false

/-- Planted mutant of `checkedDestroyCall`: drops the target argument expression. -/
def dropArgDestroyCall [DecidableEq O.Ty] (P : Program O) (impl : O.Ty → Option FnId)
    (T : O.Ty) (_ : O.Expr) (site : SiteId) : Option (Deferred O) :=
  (impl T).bind fun f =>
    if validDestroyFn P T f then some ⟨f, [], site⟩ else none

/-- Planted mutant of `checkedDestroyCall`: skips `validDestroyFn` signature/non-Copy validation. -/
def uncheckedDestroyCall [DecidableEq O.Ty] (_ : Program O) (impl : O.Ty → Option FnId)
    (T : O.Ty) (arg : O.Expr) (site : SiteId) : Option (Deferred O) :=
  (impl T).map fun f => ⟨f, [arg], site⟩

/-- Planted mutant of `seqDestroyOrder`: visits sequence elements in reverse index order. -/
def revSeqDestroyOrder (n : Nat) : List Part :=
  (List.range n).reverse.map .elem ++ [.storage]

/-- Planted mutant of `seqDestroyOrder`: releases backing storage before elements. -/
def storageFirstSeqDestroyOrder (n : Nat) : List Part :=
  .storage :: (List.range n).map .elem

/-- Planted mutant of `mapDestroyOrder`: visits value before key in each entry. -/
def valFirstMapDestroyOrder (n : Nat) : List Part :=
  ((List.range n).flatMap fun i => [.value i, .key i]) ++ [.storage]

/-- Planted mutant of `mapDestroyOrder`: releases map storage before entries. -/
def storageFirstMapDestroyOrder (n : Nat) : List Part :=
  .storage :: (List.range n).flatMap fun i => [.key i, .value i]

def logReqCfg : Config ops := ⟨.exec (.prim none .log [.lit 1] s), [], .init, []⟩
def logNextCfg : Config ops := ⟨.done, [], .init, []⟩
def logCont (a : Answer Int Int DState) : Option (Config ops) :=
  match a.result with
  | .ok v => (bindLife ops none a.store).map fun st => ⟨.done, bindDst none v [], st, []⟩
  | .error kd => some ⟨.trapped kd s, [], a.store, []⟩
def logAns : Answer Int Int DState := ⟨some 1, .ok 0, .init⟩

/-- Instance (law `ConcreteSpec.Core.Exec.steps_request`, case `log_ok`). Executing a `Pr.log`
primitive call with an allowed answer `logAns` takes the labeled transition with `Answer.label` to
`Ctrl.done`. -/
example : Step prog ⟨.exec (.prim none .log [.lit 1] s), [], .init, []⟩ logAns.label
    ⟨.done, bindDst none 0 [], .init, []⟩ :=
  steps_request prog none .log [.lit 1] s [] .init .init .init [1] [] logAns 0
    rfl rfl rfl rfl

/-- Planted (law `ConcreteSpec.Core.Exec.steps_request`, case `log_ok`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.StuckStep`).
`Refusal.StuckStep` refuses the labeled primitive request transition even when all hypotheses hold. -/
example : evalArgs ops [] .init [.lit 1] = .ok ([1], .init) ∧
    ops.allows .log [1] .init logAns = true ∧
    logAns.result = .ok 0 ∧
    bindLife ops none logAns.store = some .init ∧
    ¬ Refusal.StuckStep prog ⟨.exec (.prim none .log [.lit 1] s), [], .init, []⟩ logAns.label
      ⟨.done, bindDst none 0 [], .init, []⟩ := by
  refine ⟨rfl, rfl, rfl, rfl, fun h => ?_⟩
  exact nomatch h

/-- Instance (law `ConcreteSpec.Core.Exec.lookup_restrict`, case `kept`). Looking up a variable `x`
that belongs to the restricted domain returns its original binding in the local environment. -/
example : AList.lookup (restrict [x] [(x, 10), (p, 20)]) x =
    if x ∈ [x] then AList.lookup [(x, 10), (p, 20)] x else none :=
  lookup_restrict [x] [(x, 10), (p, 20)] x

/-- Planted (law `ConcreteSpec.Core.Exec.lookup_restrict`, case `kept`, subject `ConcreteSpec.Core.Exec.restrict`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.dropAllRestrict`).
`dropAllRestrict` discards `x` even though `x` belongs to the restricted domain. -/
example : ¬ (AList.lookup (dropAllRestrict [x] [(x, 10), (p, 20)]) x =
    if x ∈ [x] then AList.lookup [(x, 10), (p, 20)] x else none) := by
  decide

/-- Instance (law `ConcreteSpec.Core.Exec.lookup_restrict`, case `dropped`). Looking up a variable
`p` outside the restricted domain returns `none`. -/
example : AList.lookup (restrict [x] [(x, 10), (p, 20)]) p =
    if p ∈ [x] then AList.lookup [(x, 10), (p, 20)] p else none :=
  lookup_restrict [x] [(x, 10), (p, 20)] p

/-- Planted (law `ConcreteSpec.Core.Exec.lookup_restrict`, case `dropped`, subject `ConcreteSpec.Core.Exec.restrict`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.keepAllRestrict`).
`keepAllRestrict` leaks the inner variable `p` that is not in the restricted domain. -/
example : ¬ (AList.lookup (keepAllRestrict [x] [(x, 10), (p, 20)]) p =
    if p ∈ [x] then AList.lookup [(x, 10), (p, 20)] p else none) := by
  decide

/-- Instance (law `ConcreteSpec.Core.Exec.restrict_dom_drops`, case `inner_local`). Restricting to
the outer `dom` removes the newly bound inner variable `p`. -/
example : AList.lookup (restrict (dom [(x, 10)]) ((p, 20) :: [(x, 10)])) p = none :=
  restrict_dom_drops [(x, 10)] ((p, 20) :: [(x, 10)]) p (by decide)

/-- Planted (law `ConcreteSpec.Core.Exec.restrict_dom_drops`, case `inner_local`, subject `ConcreteSpec.Core.Exec.restrict`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.keepAllRestrict`).
`keepAllRestrict` fails to drop the inner local `p`. -/
example : p ∉ dom [(x, 10)] ∧
    ¬ (AList.lookup (keepAllRestrict (dom [(x, 10)]) ((p, 20) :: [(x, 10)])) p = none) := by
  decide

def d1 : Deferred ops := ⟨bumpF, [], s⟩
def d2 : Deferred ops := ⟨logF, [.lit 1], s⟩

/-- Instance (law `ConcreteSpec.Core.Exec.two_defers_register`, case `lifo`). A block registering
`d1` then `d2` reaches `Stmt.skip` with its scope holding `d2` before `d1`. -/
example : Steps prog ⟨.exec (.block ⟨1⟩ (.seq (.defer d1.f d1.args d1.site)
      (.seq (.defer d2.f d2.args d2.site) .skip))), [], .init, []⟩ []
    ⟨.exec .skip, [], .init, .scope ⟨1⟩ (dom ([] : AList Var Int)) [d2, d1] :: []⟩ :=
  two_defers_register prog ⟨1⟩ d1 d2 .skip [] .init []

/-- Planted (law `ConcreteSpec.Core.Exec.two_defers_register`, case `lifo`, subject `ConcreteSpec.Core.Exec.Steps`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.ReflOnlySteps`).
`ReflOnlySteps` fails to advance the two-defer registration block. -/
example : ¬ ReflOnlySteps prog ⟨.exec (.block ⟨1⟩ (.seq (.defer d1.f d1.args d1.site)
      (.seq (.defer d2.f d2.args d2.site) .skip))), [], .init, []⟩ []
    ⟨.exec .skip, [], .init, .scope ⟨1⟩ (dom ([] : AList Var Int)) [d2, d1] :: []⟩ := by
  intro h
  cases h

/-- Instance (law `ConcreteSpec.Core.Exec.step_ret`, case `eval_first`). Returning `E.var` on `x` with
`x` bound to 7 transitions to `Ctrl.exiting` with `Exit.ret` without touching the frame stack. -/
example : step prog ⟨.exec (.ret (.var x) s), [(x, 7)], .init, []⟩ =
    some (.next ⟨.exiting (.ret 7), [(x, 7)], .init, []⟩) :=
  step_ret prog (.var x) s [(x, 7)] .init .init [] 7 rfl

/-- Planted (law `ConcreteSpec.Core.Exec.step_ret`, case `eval_first`, subject `ConcreteSpec.Core.Exec.step`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.stuckStep`).
`stuckStep` refuses the return step even when the return expression evaluates to 7. -/
example : ops.eval [(x, 7)] .init (.var x) = .ok (7, .init) ∧
    ¬ (stuckStep prog ⟨.exec (.ret (.var x) s), [(x, 7)], .init, []⟩ =
      some (.next ⟨.exiting (.ret 7), [(x, 7)], .init, []⟩)) := by
  refine ⟨rfl, fun h => ?_⟩
  cases h

/-- Instance (law `ConcreteSpec.Core.Exec.step_return_caller`, case `restore_and_bind`). Reaching a
caller frame `Frame.ret` on `x` with return value 7 restores caller locals and binds `x` to 7. -/
example : step prog ⟨.exiting (.ret 7), [(p, 99)], .init, .ret (some x) [] :: []⟩ =
    some (.next ⟨.done, bindDst (some x) 7 [], .init, []⟩) :=
  step_return_caller prog 7 (some x) [] [(p, 99)] .init [] .init .init rfl rfl

/-- Planted (law `ConcreteSpec.Core.Exec.step_return_caller`, case `restore_and_bind`, subject `ConcreteSpec.Core.Exec.step`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.stuckStep`).
`stuckStep` fails to return to the caller frame even when `Life.leave` and `bindLife` succeed. -/
example : ops.life .leave .init = some .init ∧
    bindLife ops (some x) .init = some .init ∧
    ¬ (stuckStep prog ⟨.exiting (.ret 7), [(p, 99)], .init, .ret (some x) [] :: []⟩ =
      some (.next ⟨.done, bindDst (some x) 7 [], .init, []⟩)) := by
  refine ⟨rfl, rfl, fun h => ?_⟩
  cases h

/-- Instance (law `ConcreteSpec.Core.Exec.exit_scope_next`, case `dispatch_first`). Unwinding an
exit into a scope with `d1` before `d2` dispatches `d1` first and saves `Frame.resume` with `Pending.exit`. -/
example : Step prog ⟨.exiting (.ret 0), [], .init, .scope ⟨1⟩ [] (d1 :: [d2]) :: []⟩ .silent
      ⟨.exec d1.stmt, [], .init, .resume (.exit (.ret 0)) :: .scope ⟨1⟩ [] [d2] :: []⟩ ↔
    (Label.silent : Label ops) = .silent ∧
      (⟨.exec d1.stmt, [], .init, .resume (.exit (.ret 0)) :: .scope ⟨1⟩ [] [d2] :: []⟩ : Config ops) =
        ⟨.exec d1.stmt, [], .init, .resume (.exit (.ret 0)) :: .scope ⟨1⟩ [] [d2] :: []⟩ :=
  exit_scope_next prog (.ret 0) ⟨1⟩ [] d1 [d2] [] .init [] .silent
    ⟨.exec d1.stmt, [], .init, .resume (.exit (.ret 0)) :: .scope ⟨1⟩ [] [d2] :: []⟩

/-- Planted (law `ConcreteSpec.Core.Exec.exit_scope_next`, case `dispatch_first`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.StuckStep`).
`Refusal.StuckStep` blocks the scope-exit cleanup dispatch. -/
example : ¬ (Refusal.StuckStep prog
      ⟨.exiting (.ret 0), [], .init, .scope ⟨1⟩ [] (d1 :: [d2]) :: []⟩ .silent
      ⟨.exec d1.stmt, [], .init, .resume (.exit (.ret 0)) :: .scope ⟨1⟩ [] [d2] :: []⟩ ↔
    (Label.silent : Label ops) = .silent ∧
      (⟨.exec d1.stmt, [], .init, .resume (.exit (.ret 0)) :: .scope ⟨1⟩ [] [d2] :: []⟩ : Config ops) =
        ⟨.exec d1.stmt, [], .init, .resume (.exit (.ret 0)) :: .scope ⟨1⟩ [] [d2] :: []⟩) := by
  intro h
  exact nomatch h.mpr ⟨rfl, rfl⟩

/-- Instance (law `ConcreteSpec.Core.Exec.done_scope_next`, case `dispatch_first`). Normal
completion of a scope with `d1` before `d2` dispatches `d1` first and saves `Frame.resume` with `Pending.fall`. -/
example : Step prog ⟨.done, [], .init, .scope ⟨1⟩ [] (d1 :: [d2]) :: []⟩ .silent
      ⟨.exec d1.stmt, [], .init, .resume .fall :: .scope ⟨1⟩ [] [d2] :: []⟩ ↔
    (Label.silent : Label ops) = .silent ∧
      (⟨.exec d1.stmt, [], .init, .resume .fall :: .scope ⟨1⟩ [] [d2] :: []⟩ : Config ops) =
        ⟨.exec d1.stmt, [], .init, .resume .fall :: .scope ⟨1⟩ [] [d2] :: []⟩ :=
  done_scope_next prog ⟨1⟩ [] d1 [d2] [] .init [] .silent
    ⟨.exec d1.stmt, [], .init, .resume .fall :: .scope ⟨1⟩ [] [d2] :: []⟩

/-- Planted (law `ConcreteSpec.Core.Exec.done_scope_next`, case `dispatch_first`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.StuckStep`).
`Refusal.StuckStep` blocks the normal scope-completion cleanup dispatch. -/
example : ¬ (Refusal.StuckStep prog
      ⟨.done, [], .init, .scope ⟨1⟩ [] (d1 :: [d2]) :: []⟩ .silent
      ⟨.exec d1.stmt, [], .init, .resume .fall :: .scope ⟨1⟩ [] [d2] :: []⟩ ↔
    (Label.silent : Label ops) = .silent ∧
      (⟨.exec d1.stmt, [], .init, .resume .fall :: .scope ⟨1⟩ [] [d2] :: []⟩ : Config ops) =
        ⟨.exec d1.stmt, [], .init, .resume .fall :: .scope ⟨1⟩ [] [d2] :: []⟩) := by
  intro h
  exact nomatch h.mpr ⟨rfl, rfl⟩

def afterBumpLog : DState := { DState.init with counter := 1 }

/-- Instance (law `ConcreteSpec.Core.Exec.exit_scope_runs_defers`, case `bump_then_log`). Exiting a
scope that registered `logFn` then `bumpFn` runs `bumpFn` then `logFn` and drops the scope. -/
example : Steps prog ⟨.exiting (.ret 0), [], .init, .scope ⟨1⟩ [] [d1, d2] :: []⟩ ([] ++ [1])
    ⟨.exiting (.ret 0), restrict [] [], afterBumpLog, []⟩ :=
  exit_scope_runs_defers prog (.ret 0) ⟨1⟩ [] [] [] [d1, d2] .init afterBumpLog ([] ++ [1])
    afterBumpLog (.cons (demo_bump_completes [] .init) (.cons (demo_log_completes [] afterBumpLog) (.nil _))) rfl

/-- Planted (law `ConcreteSpec.Core.Exec.exit_scope_runs_defers`, case `bump_then_log`, subject `ConcreteSpec.Core.Exec.Steps`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.ReflOnlySteps`).
`ReflOnlySteps` cannot execute the scope's deferred cleanup sequence on exit. -/
example : RunsAll prog [] [d1, d2] .init ([] ++ [1]) afterBumpLog ∧
    dropOut ops [] [] afterBumpLog = some afterBumpLog ∧
    ¬ ReflOnlySteps prog ⟨.exiting (.ret 0), [], .init, .scope ⟨1⟩ [] [d1, d2] :: []⟩ ([] ++ [1])
      ⟨.exiting (.ret 0), restrict [] [], afterBumpLog, []⟩ := by
  refine ⟨.cons (demo_bump_completes [] .init) (.cons (demo_log_completes [] afterBumpLog) (.nil _)),
    rfl, fun h => ?_⟩
  cases h

/-- Instance (law `ConcreteSpec.Core.Exec.done_scope_runs_defers`, case `bump_then_log`). Completing
a scope that registered `logFn` then `bumpFn` runs `bumpFn` then `logFn` and drops the scope. -/
example : Steps prog ⟨.done, [], .init, .scope ⟨1⟩ [] [d1, d2] :: []⟩ ([] ++ [1])
    ⟨.done, restrict [] [], afterBumpLog, []⟩ :=
  done_scope_runs_defers prog ⟨1⟩ [] [] [] [d1, d2] .init afterBumpLog ([] ++ [1]) afterBumpLog
    (.cons (demo_bump_completes [] .init) (.cons (demo_log_completes [] afterBumpLog) (.nil _))) rfl

/-- Planted (law `ConcreteSpec.Core.Exec.done_scope_runs_defers`, case `bump_then_log`, subject `ConcreteSpec.Core.Exec.Steps`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.ReflOnlySteps`).
`ReflOnlySteps` cannot execute the scope's deferred cleanup sequence on normal completion. -/
example : RunsAll prog [] [d1, d2] .init ([] ++ [1]) afterBumpLog ∧
    dropOut ops [] [] afterBumpLog = some afterBumpLog ∧
    ¬ ReflOnlySteps prog ⟨.done, [], .init, .scope ⟨1⟩ [] [d1, d2] :: []⟩ ([] ++ [1])
      ⟨.done, restrict [] [], afterBumpLog, []⟩ := by
  refine ⟨.cons (demo_bump_completes [] .init) (.cons (demo_log_completes [] afterBumpLog) (.nil _)),
    rfl, fun h => ?_⟩
  cases h

/-- Helper (inversion). Every element of the singleton scope prefix is passable
(approved: APPR-C1-codex-5531d9e34). -/
private theorem passes_single_scope (e : Exit Int) :
    ∀ (f : Frame ops), f ∈ ([.scope ⟨1⟩ [] [d2]] : List (Frame ops)) → Passes e f := by
  intro f hf
  cases hf with
  | head => trivial
  | tail _ hf => cases hf

/-- Instance (law `ConcreteSpec.Core.Exec.unwind_runs_plan`, case `single_scope`). Unwinding a
single scope frame runs its deferred calls and reaches the empty boundary. -/
example : Steps prog ⟨.exiting (.ret 0), [], .init, [.scope ⟨1⟩ [] [d2]] ++ []⟩ ([1] ++ [])
    ⟨.exiting (.ret 0), unwindLocals [.scope ⟨1⟩ [] [d2]] [], .init, []⟩ :=
  unwind_runs_plan prog (.ret 0) [] [.scope ⟨1⟩ [] [d2]] [] .init .init ([1] ++ [])
    (.scope (.cons (demo_log_completes [] .init) (.nil _)) rfl (.nil _ _))
    (passes_single_scope (.ret 0))

/-- Planted (law `ConcreteSpec.Core.Exec.unwind_runs_plan`, case `single_scope`, subject `ConcreteSpec.Core.Exec.Steps`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.ReflOnlySteps`).
`ReflOnlySteps` cannot unwind the scope prefix. -/
example : Cleans prog [.scope ⟨1⟩ [] [d2]] [] .init ([1] ++ []) .init ∧
    (∀ (f : Frame ops), f ∈ ([.scope ⟨1⟩ [] [d2]] : List (Frame ops)) → Passes (O := ops) (.ret 0) f) ∧
    ¬ ReflOnlySteps prog ⟨.exiting (.ret 0), [], .init, [.scope ⟨1⟩ [] [d2]] ++ []⟩ ([1] ++ [])
      ⟨.exiting (.ret 0), unwindLocals [.scope ⟨1⟩ [] [d2]] [], .init, []⟩ := by
  refine ⟨.scope (.cons (demo_log_completes [] .init) (.nil _)) rfl (.nil _ _),
    passes_single_scope (.ret 0), fun h => ?_⟩
  cases h

/-- Instance (law `ConcreteSpec.Core.Exec.return_reaches_caller`, case `unwind_and_return`). A
return of 7 unwinds the enclosing scope, runs its deferred `logFn`, leaves the activation, and
binds `x` to 7 in the caller. -/
example : Steps prog
    ⟨.exiting (.ret 7), [], .init, [.scope ⟨1⟩ [] [d2]] ++ .ret (some x) [] :: []⟩ ([1] ++ [])
    ⟨.done, bindDst (some x) 7 [], .init, []⟩ :=
  return_reaches_caller prog 7 (some x) [] [] [.scope ⟨1⟩ [] [d2]] [] .init .init ([1] ++ [])
    .init .init
    (.scope (.cons (demo_log_completes [] .init) (.nil _)) rfl (.nil _ _))
    (passes_single_scope (.ret 7)) rfl rfl

/-- Planted (law `ConcreteSpec.Core.Exec.return_reaches_caller`, case `unwind_and_return`, subject `ConcreteSpec.Core.Exec.Steps`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.ReflOnlySteps`).
`ReflOnlySteps` fails to unwind the scope and return to the caller. -/
example : Cleans prog [.scope ⟨1⟩ [] [d2]] [] .init ([1] ++ []) .init ∧
    (∀ (f : Frame ops), f ∈ ([.scope ⟨1⟩ [] [d2]] : List (Frame ops)) → Passes (O := ops) (.ret 7) f) ∧
    ops.life .leave .init = some .init ∧
    bindLife ops (some x) .init = some .init ∧
    ¬ ReflOnlySteps prog
      ⟨.exiting (.ret 7), [], .init, [.scope ⟨1⟩ [] [d2]] ++ .ret (some x) [] :: []⟩ ([1] ++ [])
      ⟨.done, bindDst (some x) 7 [], .init, []⟩ := by
  refine ⟨.scope (.cons (demo_log_completes [] .init) (.nil _)) rfl (.nil _ _),
    passes_single_scope (.ret 7), rfl, rfl, fun h => ?_⟩
  cases h

/-- Instance (law `ConcreteSpec.Core.Exec.break_reaches_loop`, case `unwind_and_break`). A `Stmt.brk`
unwinds the loop body's scope, runs its deferred `logFn`, and exits the target loop `Frame.body`. -/
example : Steps prog
    ⟨.exiting (.brk ⟨1⟩), [], .init,
      [.scope ⟨1⟩ [] [d2]] ++ .body ⟨1⟩ .skip (.lit 1) .skip s :: []⟩ ([1] ++ [])
    ⟨.done, unwindLocals [.scope ⟨1⟩ [] [d2]] [], .init, []⟩ :=
  break_reaches_loop prog ⟨1⟩ .skip .skip (.lit 1) s [] [.scope ⟨1⟩ [] [d2]] [] .init .init
    ([1] ++ [])
    (.scope (.cons (demo_log_completes [] .init) (.nil _)) rfl (.nil _ _))
    (passes_single_scope (.brk ⟨1⟩))

/-- Planted (law `ConcreteSpec.Core.Exec.break_reaches_loop`, case `unwind_and_break`, subject `ConcreteSpec.Core.Exec.Steps`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.ReflOnlySteps`).
`ReflOnlySteps` fails to unwind to the target loop boundary on `Stmt.brk`. -/
example : Cleans prog [.scope ⟨1⟩ [] [d2]] [] .init ([1] ++ []) .init ∧
    (∀ (f : Frame ops), f ∈ ([.scope ⟨1⟩ [] [d2]] : List (Frame ops)) → Passes (.brk ⟨1⟩) f) ∧
    ¬ ReflOnlySteps prog
      ⟨.exiting (.brk ⟨1⟩), [], .init,
        [.scope ⟨1⟩ [] [d2]] ++ .body ⟨1⟩ .skip (.lit 1) .skip s :: []⟩ ([1] ++ [])
      ⟨.done, unwindLocals [.scope ⟨1⟩ [] [d2]] [], .init, []⟩ := by
  refine ⟨.scope (.cons (demo_log_completes [] .init) (.nil _)) rfl (.nil _ _),
    passes_single_scope (.brk ⟨1⟩), fun h => ?_⟩
  cases h

/-- Instance (law `ConcreteSpec.Core.Exec.continue_reaches_loop`, case `unwind_and_continue`). A
`Stmt.cont` unwinds the loop body's scope, runs its deferred `logFn`, and restarts the target loop. -/
example : Steps prog
    ⟨.exiting (.cont ⟨1⟩), [], .init,
      [.scope ⟨1⟩ [] [d2]] ++ .body ⟨1⟩ .skip (.lit 1) .skip s :: []⟩ ([1] ++ [])
    ⟨.exec (.loop ⟨1⟩ .skip (.lit 1) .skip s), unwindLocals [.scope ⟨1⟩ [] [d2]] [], .init, []⟩ :=
  continue_reaches_loop prog ⟨1⟩ .skip .skip (.lit 1) s [] [.scope ⟨1⟩ [] [d2]] [] .init .init
    ([1] ++ [])
    (.scope (.cons (demo_log_completes [] .init) (.nil _)) rfl (.nil _ _))
    (passes_single_scope (.cont ⟨1⟩))

/-- Planted (law `ConcreteSpec.Core.Exec.continue_reaches_loop`, case `unwind_and_continue`, subject `ConcreteSpec.Core.Exec.Steps`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.ReflOnlySteps`).
`ReflOnlySteps` fails to unwind and re-enter the target loop on `Stmt.cont`. -/
example : Cleans prog [.scope ⟨1⟩ [] [d2]] [] .init ([1] ++ []) .init ∧
    (∀ (f : Frame ops), f ∈ ([.scope ⟨1⟩ [] [d2]] : List (Frame ops)) → Passes (.cont ⟨1⟩) f) ∧
    ¬ ReflOnlySteps prog
      ⟨.exiting (.cont ⟨1⟩), [], .init,
        [.scope ⟨1⟩ [] [d2]] ++ .body ⟨1⟩ .skip (.lit 1) .skip s :: []⟩ ([1] ++ [])
      ⟨.exec (.loop ⟨1⟩ .skip (.lit 1) .skip s), unwindLocals [.scope ⟨1⟩ [] [d2]] [], .init, []⟩ := by
  refine ⟨.scope (.cons (demo_log_completes [] .init) (.nil _)) rfl (.nil _ _),
    passes_single_scope (.cont ⟨1⟩), fun h => ?_⟩
  cases h

/-- Instance (law `ConcreteSpec.Core.Exec.planWith_calls`, case `nested`). Projecting the `CleanAct.call`
actions from `planWith` on `CleanupEvidence.inner` and `CleanupEvidence.outer` yields `plan`. -/
example : (planWith [CleanupEvidence.inner, CleanupEvidence.outer] []).filterMap CleanAct.call? =
    plan [CleanupEvidence.inner, CleanupEvidence.outer] :=
  planWith_calls [CleanupEvidence.inner, CleanupEvidence.outer] []

/-- Planted (law `ConcreteSpec.Core.Exec.planWith_calls`, case `nested`, subject `ConcreteSpec.Core.Exec.planWith`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.reversePlanWith`).
Reversing `planWith` reverses the projected call list and disagrees with `plan`. -/
example : ¬ ((reversePlanWith [CleanupEvidence.inner, CleanupEvidence.outer] []).filterMap
      CleanAct.call? =
    plan [CleanupEvidence.inner, CleanupEvidence.outer]) := by
  intro h
  cases h

/-- Planted (law `ConcreteSpec.Core.Exec.planWith_calls`, case `nested`, subject `ConcreteSpec.Core.Exec.plan`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.emptyPlan`).
Replacing `plan` by `emptyPlan` drops the two planned deferred calls. -/
example : ¬ ((planWith [CleanupEvidence.inner, CleanupEvidence.outer] []).filterMap
      CleanAct.call? =
    emptyPlan [CleanupEvidence.inner, CleanupEvidence.outer]) := by
  intro h
  cases h

/-- Instance (law `ConcreteSpec.Core.Exec.step_trapped`, case `trapped_terminal`). A trapped
configuration is recognized by `Config.final?` and is blocked in `Step`. -/
example : (Config.final? (⟨.trapped .abort s, [], .init, []⟩ : Config ops) =
      some (.trapped .abort s) ↔
      (Ctrl.trapped .abort s : Ctrl ops) = .trapped .abort s) ∧
    Blocked (Step prog) ⟨.trapped .abort s, [], .init, []⟩ :=
  step_trapped prog (.trapped .abort s) .abort s [] .init []

/-- Planted (law `ConcreteSpec.Core.Exec.step_trapped`, case `trapped_terminal`, subject `ConcreteSpec.Core.Exec.Config.final?`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.noTrapFinal?`).
`noTrapFinal?` fails to recognize the trapped configuration as final. -/
example : ¬ ((noTrapFinal? (⟨.trapped .abort s, [], .init, []⟩ : Config ops) =
      some (.trapped .abort s) ↔
      (Ctrl.trapped .abort s : Ctrl ops) = .trapped .abort s) ∧
    Blocked (Step prog) ⟨.trapped .abort s, [], .init, []⟩) := by
  intro h
  have hf := h.1.mpr rfl
  cases hf

/-- Planted (law `ConcreteSpec.Core.Exec.step_trapped`, case `trapped_terminal`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.RecoverTrapStep`).
`RecoverTrapStep` allows a transition out of a trapped configuration. -/
example : ¬ ((Config.final? (⟨.trapped .abort s, [], .init, []⟩ : Config ops) =
      some (.trapped .abort s) ↔
      (Ctrl.trapped .abort s : Ctrl ops) = .trapped .abort s) ∧
    Blocked (RecoverTrapStep prog) ⟨.trapped .abort s, [], .init, []⟩) := by
  intro h
  exact h.2 .silent ⟨.done, [], .init, []⟩ .recover

/-- Instance (law `ConcreteSpec.Core.Exec.steps_from_trapped`, case `no_unwind`). A trapped
configuration cannot transition to `Ctrl.done`. -/
example : Steps prog ⟨.trapped .abort s, [], .init, []⟩ [] ⟨.done, [], .init, []⟩ ↔
    ([] : List Int) = [] ∧
      (⟨.done, [], .init, []⟩ : Config ops) = ⟨.trapped .abort s, [], .init, []⟩ :=
  steps_from_trapped prog .abort s [] .init [] [] ⟨.done, [], .init, []⟩

/-- Planted (law `ConcreteSpec.Core.Exec.steps_from_trapped`, case `no_unwind`, subject `ConcreteSpec.Core.Exec.Steps`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.RecoverTrapSteps`).
`RecoverTrapSteps` allows a trapped configuration to recover to `Ctrl.done`. -/
example : ¬ (RecoverTrapSteps prog ⟨.trapped .abort s, [], .init, []⟩ [] ⟨.done, [], .init, []⟩ ↔
    ([] : List Int) = [] ∧
      (⟨.done, [], .init, []⟩ : Config ops) = ⟨.trapped .abort s, [], .init, []⟩) := by
  intro h
  have he := (h.mp .recover).2
  cases he

/-- Instance (law `ConcreteSpec.Core.Exec.abort_runs_no_cleanup`, case `traps_without_cleanup`).
From `Ctrl.exec` of `Stmt.abort`, the trapped configuration `Ctrl.trapped` with `TrapKind.abort` is reachable with empty
trace and untouched frame stack. -/
example : Steps prog ⟨.exec (.abort s), [], .init, []⟩ [] ⟨.trapped .abort s, [], .init, []⟩ ↔
    ([] : List Int) = [] ∧
      ((⟨.trapped .abort s, [], .init, []⟩ : Config ops) = ⟨.exec (.abort s), [], .init, []⟩ ∨
        (⟨.trapped .abort s, [], .init, []⟩ : Config ops) = ⟨.trapped .abort s, [], .init, []⟩) :=
  abort_runs_no_cleanup prog s [] .init [] [] ⟨.trapped .abort s, [], .init, []⟩

/-- Planted (law `ConcreteSpec.Core.Exec.abort_runs_no_cleanup`, case `traps_without_cleanup`, subject `ConcreteSpec.Core.Exec.Steps`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.ReflOnlySteps`).
`ReflOnlySteps` fails to transition from `Ctrl.exec` of `Stmt.abort` to `Ctrl.trapped` with `TrapKind.abort`. -/
example : ¬ (ReflOnlySteps prog ⟨.exec (.abort s), [], .init, []⟩ []
      ⟨.trapped .abort s, [], .init, []⟩ ↔
    ([] : List Int) = [] ∧
      ((⟨.trapped .abort s, [], .init, []⟩ : Config ops) = ⟨.exec (.abort s), [], .init, []⟩ ∨
        (⟨.trapped .abort s, [], .init, []⟩ : Config ops) = ⟨.trapped .abort s, [], .init, []⟩)) := by
  intro h
  have hs := h.mpr ⟨rfl, .inr rfl⟩
  cases hs

/-- Instance (law `ConcreteSpec.Core.Exec.evalArgs_first_trap`, case `first_trap_wins`). After
`E.lit` succeeds, `E.fail` with `TrapKind.overflow` traps before `E.fail` with `TrapKind.divisionByZero` is
evaluated. -/
example : evalArgs ops [] .init ([.lit 1] ++ .fail .overflow :: [.fail .divisionByZero]) =
    .trap .overflow :=
  evalArgs_first_trap (O := ops) [] (.fail .overflow) .overflow [.fail .divisionByZero]
    [.lit 1] .init .init [1] rfl rfl

/-- Planted (law `ConcreteSpec.Core.Exec.evalArgs_first_trap`, case `first_trap_wins`, subject `ConcreteSpec.Core.Exec.evalArgs`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.swallowTrapEvalArgs`).
`swallowTrapEvalArgs` evaluates `E.lit` to `Res.ok` but swallows the `TrapKind.overflow` trap
instead of propagating `Res.trap`. -/
example : swallowTrapEvalArgs ops [] .init [.lit 1] = .ok ([1], .init) ∧
    ops.eval [] .init (.fail .overflow) = .trap .overflow ∧
    ¬ (swallowTrapEvalArgs ops [] .init
      ([.lit 1] ++ .fail .overflow :: [.fail .divisionByZero]) = .trap .overflow) := by
  decide

/-- Independent inductive mutant retaining every rule and adding overlapping skip and
primitive rules with incorrect observable labels. -/
inductive CompetingRules (P : Program O) : Config O → Option (Answer O.Val O.Event O.Store) →
    Label O → Config O → Prop where
  | base {c a l c'} : Rules P c a l c' → CompetingRules P c a l c'
  | skipExtra {L st k ev} :
      CompetingRules P ⟨.exec .skip, L, st, k⟩ none (.emit ev) ⟨.done, L, st, k⟩
  | primExtra {dst p args site L st k a l c'} :
      Rules P ⟨.exec (.prim dst p args site), L, st, k⟩ (some a) l c' →
      CompetingRules P ⟨.exec (.prim dst p args site), L, st, k⟩ (some a) .silent c'

/-- Instance (law `ConcreteSpec.Core.Exec.rules_silent_deterministic`, case `skip`). -/
example : InputDeterministic Rules prog ⟨.exec .skip, [], .init, []⟩
    ⟨.done, [], .init, []⟩ none .silent :=
  rules_silent_deterministic prog ⟨.exec .skip, [], .init, []⟩ ⟨.done, [], .init, []⟩
    .silent .skip

/-- Planted (law `ConcreteSpec.Core.Exec.rules_silent_deterministic`, case `skip`, subject `ConcreteSpec.Core.Exec.Rules`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.CompetingRules`).
The original skip remains admitted while an overlapping rule emits an event. -/
example : CompetingRules prog ⟨.exec .skip, [], .init, []⟩ none .silent ⟨.done, [], .init, []⟩ ∧
    ¬ InputDeterministic CompetingRules prog ⟨.exec .skip, [], .init, []⟩
      ⟨.done, [], .init, []⟩ none .silent := by
  refine ⟨.base .skip, fun h => ?_⟩
  have hh := (h (.emit 1) ⟨.done, [], .init, []⟩ .skipExtra).1
  cases hh

/-- Instance (law `ConcreteSpec.Core.Exec.rules_deterministic`, case `log_answer`). -/
example : InputDeterministic Rules prog logReqCfg logNextCfg (some logAns) (.emit 1) :=
  rules_deterministic prog logReqCfg logNextCfg (some logAns) (.emit 1)
    (.primOk (.cons rfl .nil) rfl rfl .discard)

/-- Planted (law `ConcreteSpec.Core.Exec.rules_deterministic`, case `log_answer`, subject `ConcreteSpec.Core.Exec.Rules`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.CompetingRules`).
The allowed answer and original transition remain; the overlap loses its event. -/
example : CompetingRules prog logReqCfg (some logAns) (.emit 1) logNextCfg ∧
    ¬ InputDeterministic CompetingRules prog logReqCfg logNextCfg (some logAns) (.emit 1) := by
  refine ⟨.base (.primOk (.cons rfl .nil) rfl rfl .discard), fun h => ?_⟩
  have hh := (h .silent logNextCfg (.primExtra (.primOk (.cons rfl .nil) rfl rfl .discard))).1
  cases hh

/-- Instance (law `ConcreteSpec.Core.Exec.rules_sound`, case `log_answer`). -/
example : Dispatches step prog logReqCfg (some logAns) (.emit 1) logNextCfg :=
  rules_sound prog logReqCfg (some logAns) (.emit 1) logNextCfg
    (.primOk (.cons rfl .nil) rfl rfl .discard)

/-- Planted (law `ConcreteSpec.Core.Exec.rules_sound`, case `log_answer`, subject `ConcreteSpec.Core.Exec.step`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.stuckStep`).
Removing requests fails implementation soundness while the independent derivation survives. -/
example : Rules prog logReqCfg (some logAns) (.emit 1) logNextCfg ∧
    ¬ Dispatches stuckStep prog logReqCfg (some logAns) (.emit 1) logNextCfg := by
  refine ⟨.primOk (.cons rfl .nil) rfl rfl .discard, ?_⟩
  rintro ⟨_, _, _, _, _, impossible, _⟩
  cases impossible

/-- Dispatcher mutant replacing skip completion by an abort trap, retaining other cases. -/
def trapSkipStep (P : Program O) (c : Config O) : Option (Out O) :=
  match c.ctrl with
  | .exec .skip => some (.next ⟨.trapped .abort s, c.locals, c.store, c.frames⟩)
  | _ => step P c

/-- Instance (law `ConcreteSpec.Core.Exec.rules_complete`, case `skip`). -/
example : DispatchComplete step prog ⟨.exec .skip, [], .init, []⟩ none :=
  rules_complete prog ⟨.exec .skip, [], .init, []⟩ none

/-- Planted (law `ConcreteSpec.Core.Exec.rules_complete`, case `skip`, subject `ConcreteSpec.Core.Exec.step`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.trapSkipStep`).
The mutant's extra trap is not an operational skip result. -/
example : ¬ DispatchComplete trapSkipStep prog ⟨.exec .skip, [], .init, []⟩ none := by
  intro h
  have bad := h .silent ⟨.trapped .abort s, [], .init, []⟩ ⟨rfl, rfl⟩
  cases bad

/-- Planted mutant of `DestroyProtocol.destroyCall`: builds a zero-argument call instead of passing
the consumed value, so the singleton destructor call fails its arity check in `Step`. -/
def dropArgProtocolDestroyCall {P : Program O} (D : DestroyProtocol P) (T : O.Ty)
    (_arg : O.Expr) (site : SiteId) : Option (Deferred O) :=
  (D.impl T).map fun f => ⟨f, [], site⟩

/-- Instance (law `ConcreteSpec.Core.Exec.destroyCall_enters`, case `consume_tok`). Moving `p`
marks it moved in the caller before the destructor's activation is entered. -/
example : ∃ d, Demo.destroyProtocol.destroyCall .tok (.move p) s = some d ∧
    eatFn.result = ops.unitTy ∧ ops.isCopy .tok = false ∧
    Step prog ⟨.exec d.stmt, [(p, 7)], ⟨0, [[]]⟩, []⟩ .silent
      ⟨.exec eatFn.body, [(p, 7)], ⟨0, [[], [p]]⟩,
        [.scope eatFn.block [p] [], .ret none [(p, 7)]]⟩ :=
  destroyCall_enters prog Demo.destroyProtocol .tok eatF eatFn p (.move p) s [(p, 7)]
    ⟨0, [[]]⟩ ⟨0, [[p]]⟩ ⟨0, [[], [p]]⟩ 7 [] rfl rfl rfl rfl rfl

/-- Planted (law `ConcreteSpec.Core.Exec.destroyCall_enters`, case `consume_tok`, subject `ConcreteSpec.Core.Exec.DestroyProtocol.destroyCall`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.dropArgProtocolDestroyCall`).
Admission, lookup, singleton parameters, move evaluation and activation all succeed, but dropping
the consumed argument from the synthesized destroy call prevents `Step` from entering `eatFn`. -/
example : Demo.destroyProtocol.impl .tok = some eatF ∧
    AList.lookup prog.fns eatF = some eatFn ∧ eatFn.params = [(p, T.tok)] ∧
    ops.eval [(p, 7)] ⟨0, [[]]⟩ (.move p) = .ok (7, ⟨0, [[p]]⟩) ∧
    ops.life (.enter [p]) ⟨0, [[p]]⟩ = some ⟨0, [[], [p]]⟩ ∧
    ¬ (∃ d, dropArgProtocolDestroyCall Demo.destroyProtocol .tok (.move p) s = some d ∧
      eatFn.result = ops.unitTy ∧ ops.isCopy .tok = false ∧
      Step prog ⟨.exec d.stmt, [(p, 7)], ⟨0, [[]]⟩, []⟩ .silent
        ⟨.exec eatFn.body, [(p, 7)], ⟨0, [[], [p]]⟩,
          [.scope eatFn.block [p] [], .ret none [(p, 7)]]⟩) := by
  refine ⟨rfl, rfl, rfl, rfl, rfl, ?_⟩
  rintro ⟨d, hd, _, _, hs⟩
  cases hd
  generalize hl : (Label.silent : Label ops) = l at hs
  cases hs with
  | silent hstep => cases hstep
  | prim hreq _ _ => cases hreq

/-- Planted (law `ConcreteSpec.Core.Exec.destroyCall_enters`, case `consume_tok`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.Refusal.StuckStep`).
Admission, lookup, singleton parameters, move evaluation and activation all succeed, but
removing the actual call step prevents consuming entry. -/
example : Demo.destroyProtocol.impl .tok = some eatF ∧
    AList.lookup prog.fns eatF = some eatFn ∧ eatFn.params = [(p, T.tok)] ∧
    ops.eval [(p, 7)] ⟨0, [[]]⟩ (.move p) = .ok (7, ⟨0, [[p]]⟩) ∧
    ops.life (.enter [p]) ⟨0, [[p]]⟩ = some ⟨0, [[], [p]]⟩ ∧
    ¬ (∃ d, Demo.destroyProtocol.destroyCall .tok (.move p) s = some d ∧
      eatFn.result = ops.unitTy ∧ ops.isCopy .tok = false ∧
      Refusal.StuckStep prog ⟨.exec d.stmt, [(p, 7)], ⟨0, [[]]⟩, []⟩ .silent
        ⟨.exec eatFn.body, [(p, 7)], ⟨0, [[], [p]]⟩,
          [.scope eatFn.block [p] [], .ret none [(p, 7)]]⟩) := by
  refine ⟨rfl, rfl, rfl, rfl, rfl, ?_⟩
  rintro ⟨_, _, _, _, hs⟩
  cases hs

/-- Mutant registration loses the continuation instead of saving the deferred call. -/
def forgetRegister (_d : Deferred O) (_k : List (Frame O)) : Option (List (Frame O)) := some []
/-- Mutant transitions admit arbitrary outcomes. -/
inductive arbitraryStep (P : Program O) : Config O → Label O → Config O → Prop where
  | any {c l c'} : arbitraryStep P c l c'
/-- Instance (law `ConcreteSpec.Core.Exec.register_protected`, case `saved_caller`). -/
example : ∀ (pre k' : List (Frame ops)),
    register d2 (pre ++ .ret none [(x, 7)] :: []) = some k' →
    ∃ pre', k' = pre' ++ .ret none [(x, 7)] :: [] :=
  register_protected d2 [(x, 7)] []
/-- Planted (law `ConcreteSpec.Core.Exec.register_protected`, case `saved_caller`, subject `ConcreteSpec.Core.Exec.register`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.forgetRegister`). -/
example : ¬ (∀ (pre k' : List (Frame ops)),
    forgetRegister d2 (pre ++ .ret none [(x, 7)] :: []) = some k' →
    ∃ pre', k' = pre' ++ .ret none [(x, 7)] :: []) := by
  intro h
  obtain ⟨pre, hp⟩ := h [.scope ⟨1⟩ [] []] [] rfl
  have hh := congrArg List.length hp
  simp at hh

/-- Instance (law `ConcreteSpec.Core.Exec.protected_step`, case `saved_caller`). -/
example : ∀ label c', Step prog ⟨.exec .skip, [], .init, [.ret none [(x, 7)]]⟩ label c' →
    Protected (O := ops) [(x, 7)] [] c' ∨ (c'.ctrl = .done ∧ c'.locals = [(x, 7)] ∧ c'.frames = []) :=
  protected_step (P := prog) (c := ⟨.exec .skip, [], .init, [.ret none [(x, 7)]]⟩) ⟨[], rfl⟩
/-- Planted (law `ConcreteSpec.Core.Exec.protected_step`, case `saved_caller`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.arbitraryStep`). -/
example : Protected (O := ops) [(x, 7)] [] (⟨.exec .skip, [], .init, [.ret none [(x, 7)]]⟩ : Config ops) ∧
    ¬ (∀ label c', arbitraryStep prog ⟨.exec .skip, [], .init, [.ret none [(x, 7)]]⟩ label c' →
      Protected (O := ops) [(x, 7)] [] c' ∨ (c'.ctrl = .done ∧ c'.locals = [(x, 7)] ∧ c'.frames = [])) := by
  refine ⟨⟨[], rfl⟩, fun h => ?_⟩
  cases h .silent ⟨.trapped .abort s, [], .init, []⟩ .any with
  | inl hp => have hh := protected_height_inv hp; simp at hh
  | inr hr => cases hr.1

/-- Instance (law `ConcreteSpec.Core.Exec.call_protects`, case `callee_entry`). -/
example : ∀ label c', Step prog ⟨.exec d2.stmt, [(x, 7)], .init, []⟩ label c' →
    (∃ kd site, c'.ctrl = .trapped kd site ∧ c'.frames = []) ∨ Protected (O := ops) [(x, 7)] [] c' :=
  call_protects (P := prog) (L := [(x, 7)]) (st := .init) (q := []) (d := d2)
/-- Planted (law `ConcreteSpec.Core.Exec.call_protects`, case `callee_entry`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.arbitraryStep`). -/
example : ¬ (∀ label c', arbitraryStep prog ⟨.exec d2.stmt, [(x, 7)], .init, []⟩ label c' →
    (∃ kd site, c'.ctrl = .trapped kd site ∧ c'.frames = []) ∨ Protected (O := ops) [(x, 7)] [] c') := by
  intro h
  cases h .silent ⟨.done, [], .init, []⟩ .any with
  | inl hp => obtain ⟨_, _, hh, _⟩ := hp; cases hh
  | inr hp => have hh := protected_height_inv hp; simp at hh

/-- Instance (law `ConcreteSpec.Core.Exec.exit_label`, case `dispatch`). -/
example : ∀ label c, Step prog ⟨.exiting (.ret 7), [], .init, [CleanupEvidence.inner]⟩ label c →
    label = .silent :=
  exit_label (P := prog) (e := .ret 7) (L := []) (st := .init) (frames := [CleanupEvidence.inner])
/-- Planted (law `ConcreteSpec.Core.Exec.exit_label`, case `dispatch`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.arbitraryStep`). -/
example : ¬ (∀ label c, arbitraryStep prog ⟨.exiting (.ret 7), [], .init, [CleanupEvidence.inner]⟩ label c →
    label = .silent) := by
  intro h
  have hh := h (.emit 1) ⟨.done, [], .init, []⟩ .any
  cases hh

def copyEatFn : Fn ops := ⟨[(p, .int)], .unit, ⟨1⟩, .skip⟩
def copyProg : Program ops := ⟨[(eatF, copyEatFn)]⟩
def tokImpl : T → Option FnId := fun | .tok => some eatF | _ => none
def badImpl : T → Option FnId := fun | .tok => some bumpF | _ => none
def eatCall : Deferred ops := ⟨eatF, [.var p], s⟩
def bumpCall : Deferred ops := ⟨bumpF, [.var p], s⟩

/-- Regression for validator representation. `eatF` is a valid
destroy function for the non-Copy type `.tok`. -/
example : validDestroyFn prog .tok eatF = true ↔
    ops.isCopy .tok = false ∧ ∃ fn x, AList.lookup prog.fns eatF = some fn ∧
      fn.params = [(x, .tok)] ∧ fn.result = ops.unitTy :=
  destroy_not_copy prog .tok eatF

/-- Regression for validator representation.
`rejectAllDestroyFn` rejects the valid linear destroy function `eatF` on `.tok`. -/
example : ¬ (rejectAllDestroyFn prog .tok eatF = true ↔
    ops.isCopy .tok = false ∧ ∃ fn x, AList.lookup prog.fns eatF = some fn ∧
      fn.params = [(x, .tok)] ∧ fn.result = ops.unitTy) := by
  intro h
  have hv := h.mpr ⟨rfl, eatFn, p, rfl, rfl, rfl⟩
  cases hv

/-- Regression for validator representation. Even when
`eatF` has signature `(p : .int) -> .unit`, `validDestroyFn` rejects it because `.int` is `Copy`. -/
example : validDestroyFn copyProg .int eatF = true ↔
    ops.isCopy .int = false ∧ ∃ fn x, AList.lookup copyProg.fns eatF = some fn ∧
      fn.params = [(x, .int)] ∧ fn.result = ops.unitTy :=
  destroy_not_copy copyProg .int eatF

/-- Regression for validator representation.
`allowCopyDestroyFn` forgets `!ops.isCopy T` and accepts a destroy function on the `Copy` type `.int`. -/
example : ¬ (allowCopyDestroyFn copyProg .int eatF = true ↔
    ops.isCopy .int = false ∧ ∃ fn x, AList.lookup copyProg.fns eatF = some fn ∧
      fn.params = [(x, .int)] ∧ fn.result = ops.unitTy) := by
  intro h
  have hc := (h.mp rfl).1
  cases hc

/-- Regression for validator representation. Building a checked
destroy call for `.tok` produces `eatCall` with single argument `.var p` and verified signature. -/
example : checkedDestroyCall prog tokImpl .tok (.var p) s = some eatCall ↔
    tokImpl .tok = some eatCall.f ∧ eatCall.args = [.var p] ∧ eatCall.site = s ∧
    ops.isCopy .tok = false ∧ ∃ fn x, AList.lookup prog.fns eatCall.f = some fn ∧
      fn.params = [(x, .tok)] ∧ fn.result = ops.unitTy :=
  destroyCall_sig prog tokImpl .tok (.var p) s eatCall

/-- Regression for validator representation.
`dropArgDestroyCall` drops the target argument `.var p` and fails to produce `eatCall`. -/
example : ¬ (dropArgDestroyCall prog tokImpl .tok (.var p) s = some eatCall ↔
    tokImpl .tok = some eatCall.f ∧ eatCall.args = [.var p] ∧ eatCall.site = s ∧
    ops.isCopy .tok = false ∧ ∃ fn x, AList.lookup prog.fns eatCall.f = some fn ∧
      fn.params = [(x, .tok)] ∧ fn.result = ops.unitTy) := by
  intro h
  have hc := h.mpr ⟨rfl, rfl, rfl, rfl, eatFn, p, rfl, rfl, rfl⟩
  cases hc

/-- Regression for validator representation. When `badImpl`
maps `.tok` to `bumpF` (whose parameter list is `[]`), `checkedDestroyCall` rejects it. -/
example : checkedDestroyCall prog badImpl .tok (.var p) s = some bumpCall ↔
    badImpl .tok = some bumpCall.f ∧ bumpCall.args = [.var p] ∧ bumpCall.site = s ∧
    ops.isCopy .tok = false ∧ ∃ fn x, AList.lookup prog.fns bumpCall.f = some fn ∧
      fn.params = [(x, .tok)] ∧ fn.result = ops.unitTy :=
  destroyCall_sig prog badImpl .tok (.var p) s bumpCall

/-- Regression for validator representation.
`uncheckedDestroyCall` skips `validDestroyFn` and returns `some bumpCall` despite `bumpF` taking no
parameters. -/
example : ¬ (uncheckedDestroyCall prog badImpl .tok (.var p) s = some bumpCall ↔
    badImpl .tok = some bumpCall.f ∧ bumpCall.args = [.var p] ∧ bumpCall.site = s ∧
    ops.isCopy .tok = false ∧ ∃ fn x, AList.lookup prog.fns bumpCall.f = some fn ∧
      fn.params = [(x, .tok)] ∧ fn.result = ops.unitTy) := by
  intro h
  obtain ⟨_, _, _, _, fn, _, hfn, hp, _⟩ := h.mp rfl
  cases hfn
  cases hp

/-- Instance (law `ConcreteSpec.Core.Exec.seqDestroyOrder_elem`, case `first_elem`). In a 2-element
sequence, element 0 is visited at position 0. -/
example : (seqDestroyOrder 2)[0]? = some (.elem 0) :=
  seqDestroyOrder_elem 2 0 (by decide)

/-- Planted (law `ConcreteSpec.Core.Exec.seqDestroyOrder_elem`, case `first_elem`, subject `ConcreteSpec.Core.Exec.seqDestroyOrder`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.revSeqDestroyOrder`).
`revSeqDestroyOrder` visits `Part.elem` 1 instead of `Part.elem` 0 at position 0. -/
example : 0 < 2 ∧ ¬ ((revSeqDestroyOrder 2)[0]? = some (.elem 0)) := by
  decide

/-- Instance (law `ConcreteSpec.Core.Exec.seqDestroyOrder_storage`, case `storage_last`). A
2-element sequence has 3 destruction steps with `Part.storage` at index 2. -/
example : (seqDestroyOrder 2).length = 2 + 1 ∧ (seqDestroyOrder 2)[2]? = some .storage :=
  seqDestroyOrder_storage 2

/-- Planted (law `ConcreteSpec.Core.Exec.seqDestroyOrder_storage`, case `storage_last`, subject `ConcreteSpec.Core.Exec.seqDestroyOrder`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.storageFirstSeqDestroyOrder`).
`storageFirstSeqDestroyOrder` places `Part.storage` at index 0 instead of index 2. -/
example : ¬ ((storageFirstSeqDestroyOrder 2).length = 2 + 1 ∧
    (storageFirstSeqDestroyOrder 2)[2]? = some .storage) := by
  decide

/-- Instance (law `ConcreteSpec.Core.Exec.mapDestroyOrder_entry`, case `first_entry`). In a 2-entry
map, entry 0 visits `Part.key` 0 at index 0 and `Part.value` 0 at index 1. -/
example : (mapDestroyOrder 2)[2 * 0]? = some (.key 0) ∧
    (mapDestroyOrder 2)[2 * 0 + 1]? = some (.value 0) :=
  mapDestroyOrder_entry 2 0 (by decide)

/-- Planted (law `ConcreteSpec.Core.Exec.mapDestroyOrder_entry`, case `first_entry`, subject `ConcreteSpec.Core.Exec.mapDestroyOrder`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.valFirstMapDestroyOrder`).
`valFirstMapDestroyOrder` visits `Part.value` 0 before `Part.key` 0. -/
example : 0 < 2 ∧ ¬ ((valFirstMapDestroyOrder 2)[2 * 0]? = some (.key 0) ∧
    (valFirstMapDestroyOrder 2)[2 * 0 + 1]? = some (.value 0)) := by
  decide

/-- Instance (law `ConcreteSpec.Core.Exec.mapDestroyOrder_storage`, case `storage_last`). A 2-entry
map has 5 destruction steps with `Part.storage` at index 4. -/
example : (mapDestroyOrder 2).length = 2 * 2 + 1 ∧ (mapDestroyOrder 2)[2 * 2]? = some .storage :=
  mapDestroyOrder_storage 2

/-- Planted (law `ConcreteSpec.Core.Exec.mapDestroyOrder_storage`, case `storage_last`, subject `ConcreteSpec.Core.Exec.mapDestroyOrder`, mutant `ConcreteSpec.Core.Exec.CoreEvidence.storageFirstMapDestroyOrder`).
`storageFirstMapDestroyOrder` places `Part.storage` at index 0 instead of index 4. -/
example : ¬ ((storageFirstMapDestroyOrder 2).length = 2 * 2 + 1 ∧
    (storageFirstMapDestroyOrder 2)[2 * 2]? = some .storage) := by
  decide

end CoreEvidence

end ConcreteSpec.Core.Exec
