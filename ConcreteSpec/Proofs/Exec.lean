import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Scalar

/-!
# Elementary execution and structured exits (F09a, E1)

PROPOSED until Core freeze. No historical approval is inherited.
Each Law cites the admitted Spec/Exec definitions by stable declaration name and
its decision basis beside the declaration. These are consequences of the existing
machine, not new choices of execution policy. The foundation C1 interface/law
record (`docs/decisions.md#decided-2026-11-08-foundation`, `#foundation-q1`)
approves execution, calls and cleanup laws; the rules live in Lean. Core remains
unfrozen. The sole `runFrom` driver is unchanged.
Classification: `docs/decisions.md#decided-2026-11-13-proof-labels`.
Demo conditions are a toy interface, never Concrete Boolean encoding (ISS-0069).
-/

namespace ConcreteSpec.Core.Exec
open ConcreteSpec.Core
variable {O : Operands} {V : Type}

/-- Representation lemma. Lifts a single labeled step into a one-step execution trace. -/
theorem Steps.single {P : Program O} {c c' : Config O} {l : Label O} (h : Step P c l c') :
    Steps P c l.events c' := by
  have := Steps.step h (Steps.refl c')
  simpa using this

/-- Representation lemma. Concatenates two multi-step execution traces. -/
theorem Steps.trans {P : Program O} {c c' c'' : Config O} {tr tr' : List O.Event}
    (h : Steps P c tr c') (h' : Steps P c' tr' c'') : Steps P c (tr ++ tr') c'' := by
  induction h with
  | refl => simpa using h'
  | step hs _ ih => simpa [List.append_assoc] using Steps.step hs (ih h')

/-- Representation lemma. Prepends a silent machine step to a multi-step execution trace. -/
theorem steps_next {P : Program O} {c c' c'' : Config O} {tr : List O.Event}
    (h : step P c = some (.next c')) (h' : Steps P c' tr c'') : Steps P c tr c'' := by
  simpa [Label.events] using Steps.step (Step.silent h) h'

/-- Representation lemma. Prepends a primitive request/response step to a multi-step execution
trace. -/
private theorem steps_request_inv {P : Program O} {c c'' : Config O} {p : O.Prim} {vs : List O.Val}
    {site : SiteId} {st : O.Store} {k : Answer O.Val O.Event O.Store → Option (Config O)}
    {a : Answer O.Val O.Event O.Store} {tr : List O.Event}
    (h : step P c = some (.request p vs site st k)) (ha : O.allows p vs st a = true)
    {c' : Config O} (hk : k a = some c') (h' : Steps P c' tr c'') : Steps P c (a.label.events ++ tr) c'' :=
  Steps.step (Step.prim h ha hk) h'

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Executing `Stmt.prim` on dst, p, args, and site after
successful argument evaluation, an allowed oracle answer a with `Except.ok` v, and a successful
destination bind `bindLife` takes the labeled transition `Step` with `Answer.label` of a to `Ctrl.done` with
`bindDst` applied to dst, v, and L and the bound store.
Basis: `ConcreteSpec/Spec/Exec.lean`, `step` (primitive continuation), `Step.prim`.
`docs/decisions.md#decided-2026-11-05-core`, Q9.7: typed primitive contracts
(with its recorded registry revision); `#decided-2026-11-13-lifecycle-refusal`:
accepted result binding or refusal. The registry decision alone does not specify this transition. -/
theorem steps_request (P : Program O) (dst : Option Var) (p : O.Prim) (args : List O.Expr)
    (site : SiteId) (L : AList Var O.Val) (st st' st'' : O.Store) (vs : List O.Val)
    (k : List (Frame O)) (a : Answer O.Val O.Event O.Store) (v : O.Val)
    (he : evalArgs O L st args = .ok (vs, st')) (ha : O.allows p vs st' a = true)
    (hv : a.result = .ok v) (hb : bindLife O dst a.store = some st'') :
    Step P ⟨.exec (.prim dst p args site), L, st, k⟩ a.label
      ⟨.done, bindDst dst v L, st'', k⟩ := by
  apply Step.prim (by simp only [step, onRes, he]; rfl) ha
  simp [hv, hb]

/-- Helper. In the demo language, a deferred `Demo.logF` call on v completes from any locals,
store, and frame stack, emitting [v]. Crosses the function-end boundary on the
ordinary call; caller locals are arbitrary. This is demo completion, not a
general primitive-response law. -/
theorem demo_log_value_completes (v : Int) (L : AList Var Demo.ops.Val) (st : Demo.ops.Store) :
    Completes Demo.prog L ⟨Demo.logF, [.lit v], Demo.s⟩ st [v] st := by
  intro k
  refine steps_next rfl (steps_request_inv (O := Demo.ops) (P := Demo.prog)
    (a := ⟨some v, .ok (0 : Int), Demo.DState.life (.enter [Demo.x]) st⟩) (tr := []) rfl
    (by simp [Demo.ops, Demo.logFn]) rfl ?_)
  exact steps_next rfl (steps_next rfl (steps_next rfl (Steps.refl _)))

/-- Helper. Specialization of demo completion to event 1. -/
theorem demo_log_completes (L : AList Var Demo.ops.Val) (st : Demo.ops.Store) :
    Completes Demo.prog L ⟨Demo.logF, [.lit 1], Demo.s⟩ st [1] st :=
  demo_log_value_completes 1 L st

/-- Helper. In the demo language, a deferred `Demo.bumpF` call completes from any locals,
store, and frame stack, incrementing the counter. Crosses the same function-end
boundary before restoring arbitrary caller locals; no historical driver is used. -/
theorem demo_bump_completes (L : AList Var Demo.ops.Val) (st : Demo.ops.Store) :
    Completes Demo.prog L ⟨Demo.bumpF, [], Demo.s⟩ st [] { st with counter := st.counter + 1 } := by
  intro k
  refine steps_next rfl (steps_request_inv (O := Demo.ops) (P := Demo.prog)
    (a := ⟨none, .ok (0 : Int),
      { Demo.DState.life (.enter []) st with counter := st.counter + 1 }⟩) (tr := []) rfl
    (by simp [Demo.ops, Demo.DState.life]) rfl ?_)
  exact steps_next rfl (steps_next rfl (steps_next rfl (Steps.refl _)))

/-- Helper (computation). Definitional unfolding of `step` on `Stmt.seq` s t. -/
theorem step_seq (P : Program O) (s t : Stmt O) (L : AList Var O.Val) (st : O.Store)
    (k : List (Frame O)) :
    step P ⟨.exec (.seq s t), L, st, k⟩ = some (.next ⟨.exec s, L, st, .seq t :: k⟩) := rfl

/-- Helper (computation). Definitional unfolding of `step` at `Ctrl.done` on `Frame.seq` t. -/
theorem step_done_seq (P : Program O) (t : Stmt O) (L : AList Var O.Val) (st : O.Store)
    (k : List (Frame O)) :
    step P ⟨.done, L, st, .seq t :: k⟩ = some (.next ⟨.exec t, L, st, k⟩) := rfl

/-- Helper (computation). Definitional unfolding of `step` at `Ctrl.exiting` e on `Frame.seq` t. -/
theorem step_exit_seq (P : Program O) (e : Exit O.Val) (t : Stmt O) (L : AList Var O.Val)
    (st : O.Store) (k : List (Frame O)) :
    step P ⟨.exiting e, L, st, .seq t :: k⟩ = some (.next ⟨.exiting e, L, st, k⟩) := rfl

/-- Helper (computation). Definitional unfolding of `step` on `Stmt.block` b s. -/
theorem step_block (P : Program O) (b : BlockId) (s : Stmt O) (L : AList Var O.Val)
    (st : O.Store) (k : List (Frame O)) :
    step P ⟨.exec (.block b s), L, st, k⟩ =
      some (.next ⟨.exec s, L, st, .scope b (dom L) [] :: k⟩) := rfl

/-- Law (subject `ConcreteSpec.Core.Exec.restrict`). Looking up x in `restrict` dm L returns
`AList.lookup` L x when x ∈ dm and `none` otherwise.
Basis: `ConcreteSpec/Spec/Exec.lean`, `restrict` and `dom`, used by `stepExit`.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 and `#foundation-q1`:
approved interface/law boundary, with rules in Lean; its C1 contract
(`docs/proposals/FOUNDATION.md#c1-execution-calls-and-cleanup`) requires scope-restoration
proofs. This is a mathematical consequence of the admitted filter, not an OD2 claim
about association lists. -/
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
keeps no local bound inside it.
Basis: `ConcreteSpec/Spec/Exec.lean`, `restrict`, `dom`, and their scope-exit use in
`stepExit`; `docs/decisions.md#decided-2026-11-08-foundation`, C1, and `#foundation-q1`
approve the interface/law boundary and scope-restoration proofs (C1 contract at
`docs/proposals/FOUNDATION.md#c1-execution-calls-and-cleanup`). No filtering policy is added. -/
theorem restrict_dom_drops (L₀ L : AList Var V) (x : Var) (hx : x ∉ dom L₀) :
    AList.lookup (restrict (dom L₀) L) x = none := by
  rw [lookup_restrict]; simp [hx]

/-- Helper (computation). Definitional unfolding of `register` at `Frame.scope`. -/
theorem register_scope (d : Deferred O) (b : BlockId) (dm : List Var) (ds : List (Deferred O))
    (k : List (Frame O)) : register d (.scope b dm ds :: k) = some (.scope b dm (d :: ds) :: k) :=
  rfl

/-- Helper (computation). Definitional unfolding of `register` at `Frame.seq`. -/
theorem register_seq (d : Deferred O) (t : Stmt O) (k : List (Frame O)) :
    register d (.seq t :: k) = (register d k).map (.seq t :: ·) := rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Steps`). A block running defer d₁; defer d₂; s reaches
s with its scope holding d₂ before d₁, so d₂ runs first; nothing is emitted. Authority:
`docs/decisions.md#core-q9-8`: "Nested deferred scopes; late reads; reverse cleanup";
`docs/decisions.md#od2`, clause (5): deferred calls run last registered first,
innermost scope first, on every non-abort exit. -/
theorem two_defers_register (P : Program O) (b : BlockId) (d₁ d₂ : Deferred O) (s : Stmt O)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    Steps P ⟨.exec (.block b (.seq (.defer d₁.f d₁.args d₁.site)
        (.seq (.defer d₂.f d₂.args d₂.site) s))), L, st, k⟩ []
      ⟨.exec s, L, st, .scope b (dom L) [d₂, d₁] :: k⟩ :=
  steps_next rfl (steps_next rfl (steps_next rfl (steps_next rfl (steps_next rfl
    (steps_next rfl (steps_next rfl (Steps.refl _)))))))

/-- Law (subject `ConcreteSpec.Core.Exec.step`). A `Stmt.ret` statement evaluates its value first: the next
configuration carries the value and the same frames, so no cleanup has run. Authority: `docs/decisions.md#od2`, clause (5): "the exit value, then the scope's deferred calls". -/
theorem step_ret (P : Program O) (e : O.Expr) (site : SiteId) (L : AList Var O.Val)
    (st st' : O.Store) (k : List (Frame O)) (v : O.Val) (hv : O.eval L st e = .ok (v, st')) :
    step P ⟨.exec (.ret e site), L, st, k⟩ = some (.next ⟨.exiting (.ret v), L, st', k⟩) := by
  simp [step, onRes, hv]

/-- Helper (computation). Definitional unfolding of `step` at `Ctrl.exiting` (`Exit.brk` l) on matching
`Frame.body` l. -/
theorem step_break_loop (P : Program O) (l : LoopId) (cr b : Stmt O) (t : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    step P ⟨.exiting (.brk l), L, st, .body l cr t b site :: k⟩ =
      some (.next ⟨.done, L, st, k⟩) := by
  simp [step, stepExit]

/-- Helper (computation). Definitional unfolding of `step` at `Ctrl.exiting` (`Exit.cont` l) on matching
`Frame.body` l. -/
theorem step_continue_loop (P : Program O) (l : LoopId) (cr b : Stmt O) (t : O.Expr)
    (site : SiteId) (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    step P ⟨.exiting (.cont l), L, st, .body l cr t b site :: k⟩ =
      some (.next ⟨.exec (.loop l cr t b site), L, st, k⟩) := by
  simp [step, stepExit]

/-- Law (subject `ConcreteSpec.Core.Exec.step`). A `Exit.ret` exit reaching the call frame restores the
caller's locals and binds the value; the state is the one the callee left, with result binding
admitted; activation leave was already admitted at functionEnd.
Basis: `ConcreteSpec/Spec/Exec.lean`, `stepExit` (`Frame.ret` and separate `Frame.functionEnd` cases),
`bindDst`, `bindLife`. `docs/decisions.md#decided-2026-11-05-core`, Q9.4:
structured exits; `#decided-2026-11-08-foundation`, C1 / `#foundation-q1`:
execution/call interface and laws live in Lean; `#decided-2026-11-13-lifecycle-refusal`:
result binding and final scope exit require admission. -/
theorem step_return_caller (P : Program O) (v : O.Val) (dst : Option Var)
    (caller L : AList Var O.Val) (st : O.Store) (k : List (Frame O))
    (stBind : O.Store) (hb : bindLife O dst st = some stBind) :
    step P ⟨.exiting (.ret v), L, st, .ret dst caller :: k⟩ =
      some (.next ⟨.done, bindDst dst v caller, stBind, k⟩) := by
  simp [step, stepExit, hb]

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). At a scope with registered calls, an exit steps
if and only if the step is silent and starts the newest deferred call while remembering the exit.
Authority: `docs/decisions.md#core-q9-8`: "Nested deferred scopes; late reads; reverse cleanup";
`docs/decisions.md#od2`, clause (5): deferred calls run last registered first,
innermost scope first, on every non-abort exit. -/
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
while remembering `Pending.fall`. Authority: `docs/decisions.md#core-q9-8`: "Nested deferred scopes; late reads; reverse cleanup";
`docs/decisions.md#od2`, clause (5): deferred calls run last registered first,
innermost scope first, on every non-abort exit. -/
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
with its locals restored; the exit, and a returned value, are unchanged.
Basis: `ConcreteSpec/Spec/Exec.lean`, `stepExit`, `startCleanup`, `Completes`, `RunsAll`.
`docs/decisions.md#core-q9-8`: nested defers, late reads, reverse cleanup;
`#od2`, clause (5): last registered first, innermost scope first on every non-abort exit.
`#decided-2026-11-08-foundation`, C1 / `#foundation-q1` admits cleanup-preservation
and scope-restoration laws; `#decided-2026-11-13-lifecycle-refusal` admits scope teardown. -/
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
calls newest first and restores the scope's locals.
Basis: `ConcreteSpec/Spec/Exec.lean`, `step` (done scope), `startCleanup`, `Completes`, `RunsAll`.
`docs/decisions.md#core-q9-8`: nested defers, late reads, reverse cleanup;
`#od2`, clause (5): last registered first, innermost scope first on every non-abort exit.
`#decided-2026-11-08-foundation`, C1 / `#foundation-q1` admits cleanup-preservation
and scope-restoration laws; `#decided-2026-11-13-lifecycle-refusal` admits scope teardown. -/
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
restored, and the state the calls left.
Basis: `ConcreteSpec/Spec/Exec.lean`, `Passes`, `plan`, `unwindLocals`, `Cleans`, `stepExit`.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1` approves
execution/cleanup interfaces and laws in Lean, including scope restoration and cleanup
preservation; `#core-q9-8` and `#od2`, clause (5), establish nested reverse cleanup.
`#decided-2026-11-13-lifecycle-refusal` supplies the scope-teardown admissions. -/
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

/-- Helper (computation). Definitional unfolding of `plan` on nested `Frame.scope` frames. -/
theorem plan_scopes (b₁ b₂ : BlockId) (d₁ d₂ : List Var) (ds₁ ds₂ : List (Deferred O))
    (k : List (Frame O)) :
    plan (.scope b₁ d₁ ds₁ :: .scope b₂ d₂ ds₂ :: k) = ds₁ ++ (ds₂ ++ plan k) := rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Steps`). A `Exit.ret` exit reaches its caller after the cleanup
of every scope it leaves: the caller's locals come back with the value bound, in the state the
cleanup left.
Basis: `ConcreteSpec/Spec/Exec.lean`, `Cleans`, `Passes`, `unwindLocals`, `stepExit`
(function-end leave, then caller restoration/binding).
`docs/decisions.md#decided-2026-11-05-core`, Q9.4: structured exits;
`#od2`, clause (5), and `#core-q9-8`: ordered cleanup;
`#decided-2026-11-08-foundation`, C1 / `#foundation-q1`: execution/call/cleanup laws in Lean;
`#decided-2026-11-13-lifecycle-refusal`: successful final leave before result binding. -/
theorem return_reaches_caller (P : Program O) (v : O.Val) (dst : Option Var)
    (caller : AList Var O.Val) (k pre : List (Frame O))
    (L : AList Var O.Val) (st st' : O.Store) (tr : List O.Event)
    (stLeave stBind : O.Store) (h : Cleans P pre L st tr st')
    (hp : ∀ (f : Frame O), f ∈ pre → Passes (.ret v) f)
    (hl : O.life .leave st' = some stLeave)
    (hb : bindLife O dst stLeave = some stBind) :
    Steps P ⟨.exiting (.ret v), L, st, pre ++ .functionEnd :: .ret dst caller :: k⟩ tr
      ⟨.done, bindDst dst v caller, stBind, k⟩ := by
  have := Steps.trans (unwind_runs_plan P (.ret v) (.functionEnd :: .ret dst caller :: k) pre L st st' tr h hp)
    (steps_next (by simp [step, stepExit, hl])
      (steps_next (step_return_caller P v dst caller (unwindLocals pre L) stLeave k stBind hb) (Steps.refl _)))
  simpa using this

/-- Law (subject `ConcreteSpec.Core.Exec.Steps`). A `Exit.brk` exit reaches its loop after the cleanup of
the scopes inside the loop body, and the loop is done.
Basis: `ConcreteSpec/Spec/Exec.lean`, `Cleans`, `Passes`, `unwindLocals`, `stepExit`
(matching break reception). `docs/decisions.md#decided-2026-11-05-core`, Q9.4:
structured exits; `#core-q9-8` and `#od2`, clause (5): ordered cleanup;
`#decided-2026-11-08-foundation`, C1 / `#foundation-q1`: scope-restoration and cleanup laws
in Lean; `#decided-2026-11-13-lifecycle-refusal`: scope-teardown admission. -/
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
cleanup, and the loop starts again from its condition region.
Basis: `ConcreteSpec/Spec/Exec.lean`, `Cleans`, `Passes`, `unwindLocals`, `stepExit`
(matching continue reception). `docs/decisions.md#decided-2026-11-05-core`, Q9.3:
condition region re-run every iteration; Q9.4: structured exits;
`#core-q9-8` and `#od2`, clause (5): ordered cleanup;
`#decided-2026-11-08-foundation`, C1 / `#foundation-q1`: scope-restoration and cleanup laws
in Lean; `#decided-2026-11-13-lifecycle-refusal`: scope-teardown admission. -/
theorem continue_reaches_loop (P : Program O) (l : LoopId) (cr b : Stmt O) (t : O.Expr)
    (site : SiteId) (k pre : List (Frame O)) (L : AList Var O.Val)
    (st st' : O.Store) (tr : List O.Event)
    (h : Cleans P pre L st tr st') (hp : ∀ (f : Frame O), f ∈ pre → Passes (.cont l) f) :
    Steps P ⟨.exiting (.cont l), L, st, pre ++ .body l cr t b site :: k⟩ tr
      ⟨.exec (.loop l cr t b site), unwindLocals pre L, st', k⟩ := by
  have := Steps.trans (unwind_runs_plan P (.cont l) (.body l cr t b site :: k) pre L st st' tr h hp)
    (steps_next (step_continue_loop P l cr b t site _ st' k) (Steps.refl _))
  simpa using this

/-! ## Concrete applications and isolated planted rules

Each dispatcher mutation below removes or reorders one named transition rule and
otherwise delegates to `step`. `MutSteps` is a proof-only trace relation over
that dispatcher, not a second executable fuel/oracle driver. Premises use the
original operand contracts and lifecycle admissions unchanged.
-/
namespace CoreEvidence
open Demo

/-- Planted. Remove or reorder a single rule; all other cases use the real dispatcher. -/
inductive MissingRule where
  | request | registerSecond | retValue | returnBind | exitCleanup | doneCleanup
  | exitSeq | functionLeave | breakHere | continueHere
  | skipRestriction | skipDrop
  | reverseExit | reverseDone | outerFirst | fifoRegister | cleanupBeforeValue | bindBeforeLeave
  deriving DecidableEq

/-- Planted support. Find existing registrations through sequencing frames. -/
def hasRegistered : List (Frame O) → Bool
  | .seq _ :: k => hasRegistered k
  | .scope _ _ (_ :: _) :: _ => true
  | _ => false

/-- Planted support. FIFO registration changes only insertion order. -/
def fifoRegister (d : Deferred O) : List (Frame O) → Option (List (Frame O))
  | .seq t :: k => (fifoRegister d k).map (.seq t :: ·)
  | .scope b dm ds :: k => some (.scope b dm (ds ++ [d]) :: k)
  | _ => none

/-- Planted. Each case changes only its named rule, with no change to operands or admission. -/
def missingStep (r : MissingRule) (P : Program O) (c : Config O) : Option (Out O) :=
  match r, c.ctrl, c.frames with
  | .skipRestriction, .exiting e, .scope _ dm [] :: k =>
      (dropOut O dm c.locals c.store).map fun st => .next ⟨.exiting e, c.locals, st, k⟩
  | .skipRestriction, .done, .scope _ dm [] :: k =>
      (dropOut O dm c.locals c.store).map fun st => .next ⟨.done, c.locals, st, k⟩
  | .skipDrop, .exiting e, .scope _ dm [] :: k =>
      some (.next ⟨.exiting e, restrict dm c.locals, c.store, k⟩)
  | .skipDrop, .done, .scope _ dm [] :: k =>
      some (.next ⟨.done, restrict dm c.locals, c.store, k⟩)
  | .bindBeforeLeave, .exiting (.ret v), .functionEnd :: .ret dst caller :: k => do
      let st ← bindLife O dst c.store
      let st' ← O.life .leave st
      pure (.next ⟨.done, bindDst dst v caller, st', k⟩)
  | .cleanupBeforeValue, .exec (.ret e site), .scope b dm (d :: ds) :: k =>
      some (.next ⟨.exec (.seq d.stmt (.ret e site)), c.locals, c.store,
        .scope b dm ds :: k⟩)
  | .fifoRegister, .exec (.defer f args site), _ =>
      (fifoRegister ⟨f, args, site⟩ c.frames).map fun k =>
        .next ⟨.done, c.locals, c.store, k⟩
  | .reverseExit, .exiting e, .scope b dm ds :: k =>
      step P ⟨.exiting e, c.locals, c.store, .scope b dm (ds.reverse.take 1 ++ ds.dropLast) :: k⟩
  | .reverseDone, .done, .scope b dm ds :: k =>
      step P ⟨.done, c.locals, c.store, .scope b dm (ds.reverse.take 1 ++ ds.dropLast) :: k⟩
  | .outerFirst, .exiting e,
      .scope b dm (d :: ds) :: .scope b' dm' (d' :: ds') :: k =>
      step P ⟨.exiting e, c.locals, c.store,
        .scope b' dm' (d' :: ds') :: .scope b dm (d :: ds) :: k⟩
  | .request, .exec (.prim ..), _ => none
  | .registerSecond, .exec (.defer ..), _ =>
      if hasRegistered c.frames then none else step P c
  | .retValue, .exec (.ret ..), _ => none
  | .returnBind, .exiting (.ret _), .ret .. :: _ => none
  | .exitCleanup, .exiting _, .scope _ _ (_ :: _) :: _ => none
  | .doneCleanup, .done, .scope _ _ (_ :: _) :: _ => none
  | .exitSeq, .exiting _, .seq _ :: _ => none
  | .functionLeave, .exiting (.ret _), .functionEnd :: _ => none
  | .breakHere, .exiting (.brk l), .body l' .. :: _ =>
      if l = l' then none else step P c
  | .continueHere, .exiting (.cont l), .body l' .. :: _ =>
      if l = l' then none else step P c
  | _, _, _ => step P c

/-- Planted. The ordinary labeled transition relation over one mutated dispatcher. -/
inductive MutStep (r : MissingRule) (P : Program O) : Config O → Label O → Config O → Prop where
  | silent {c c'} : missingStep r P c = some (.next c') → MutStep r P c .silent c'
  | prim {c p vs site st k a c'} :
      missingStep r P c = some (.request p vs site st k) →
      O.allows p vs st a = true → k a = some c' → MutStep r P c a.label c'

/-- Planted. Reflexivity and composition are unchanged; only the dispatcher differs. -/
inductive MutSteps (r : MissingRule) (P : Program O) : Config O → List O.Event → Config O → Prop where
  | refl (c) : MutSteps r P c [] c
  | step {c l c' tr c''} : MutStep r P c l c' → MutSteps r P c' tr c'' →
      MutSteps r P c (l.events ++ tr) c''

/-- Helper. A refused mutated dispatcher cannot take even its first transition. -/
private theorem missing_blocked {r : MissingRule} {P : Program O} {c c' : Config O}
    {l : Label O} (h : missingStep r P c = none) : ¬ MutStep r P c l c' := by
  intro hs
  cases hs <;> simp_all

/-- Helper. A blocked mutated trace retains its initial configuration and empty trace. -/
private theorem missing_trace {r : MissingRule} {P : Program O} {c c' : Config O}
    {tr : List O.Event} (h : missingStep r P c = none) (hs : MutSteps r P c tr c') :
    c = c' ∧ tr = [] := by
  cases hs with
  | refl => exact ⟨rfl, rfl⟩
  | step hst _ => exact False.elim (missing_blocked h hst)

/-- Helper. A silent mutated dispatch either has not run yet or exposes its remaining trace. -/
private theorem missing_next {r : MissingRule} {P : Program O} {c c' target : Config O}
    {tr : List O.Event} (hd : missingStep r P c = some (.next c'))
    (hs : MutSteps r P c tr target) :
    (c = target ∧ tr = []) ∨ MutSteps r P c' tr target := by
  cases hs with
  | refl => exact Or.inl ⟨rfl, rfl⟩
  | step hst tail =>
    cases hst with
    | silent h =>
      rw [hd] at h
      cases h
      exact Or.inr (by simpa [Label.events] using tail)
    | prim h _ _ => rw [hd] at h; cases h

/-- Planted. Scope restoration wrongly retains inner bindings, changing only domain filtering. -/
def keepAllRestrict (_ : List Var) (L : AList Var V) := L

/-- Planted. Scope restoration wrongly loses an outer binding, changing only domain filtering. -/
def dropAllRestrict (_ : List Var) (_ : AList Var V) : AList Var V := []

/-- Instance (law `lookup_restrict`). Both retained and removed locals have concrete values. -/
theorem lookup_restrict_instance :
    AList.lookup (restrict [x] [(x, (10 : Int)), (p, 20)]) x = some 10 ∧
    AList.lookup (restrict [x] [(x, (10 : Int)), (p, 20)]) p = none := by
  constructor <;> rw [lookup_restrict] <;> decide

/-- Planted (law `lookup_restrict`). Both lookup branches reject their respective wrong domain filters. -/
theorem lookup_restrict_planted :
    AList.lookup (dropAllRestrict [x] [(x, (10 : Int)), (p, 20)]) x ≠
      (if x ∈ [x] then AList.lookup [(x, (10 : Int)), (p, 20)] x else none) ∧
    AList.lookup (keepAllRestrict [x] [(x, (10 : Int)), (p, 20)]) p ≠
      (if p ∈ [x] then AList.lookup [(x, (10 : Int)), (p, 20)] p else none) := by decide

/-- Instance (law `restrict_dom_drops`). A fresh inner local disappears. -/
theorem restrict_dom_drops_instance :
    AList.lookup (restrict (dom [(x, (10 : Int))]) [(p, 20), (x, 10)]) p = none :=
  restrict_dom_drops [(x, 10)] [(p, 20), (x, 10)] p (by decide)

/-- Planted (law `restrict_dom_drops`). The same fresh-local premise rejects keeping everything. -/
theorem restrict_dom_drops_planted : p ∉ dom [(x, (10 : Int))] ∧
    AList.lookup (keepAllRestrict (dom [(x, (10 : Int))]) [(p, 20), (x, 10)]) p ≠ none := by decide

/-- A real accepted answer, unchanged in the primitive mutation. -/
def logAns : Answer Int Int DState := ⟨some 1, .ok 0, .init⟩

/-- Instance (law `steps_request`). The answer binds x to 0, preserves y and the
continuation, and emits 1; dropping the binding or frames changes the result. -/
theorem steps_request_instance :
    Step prog ⟨.exec (.prim (some x) .log [.lit 1] s), [(y, 12)], .init, [.seq .skip]⟩ logAns.label
      ⟨.done, [(x, 0), (y, 12)], .init, [.seq .skip]⟩ :=
  steps_request prog (some x) .log [.lit 1] s [(y, 12)] .init .init .init [1] [.seq .skip] logAns 0 rfl rfl rfl rfl

/-- Planted (law `steps_request`). Only primitive response admission is removed; all premises hold. -/
theorem steps_request_planted : evalArgs ops [(y, 12)] .init [.lit 1] = .ok ([1], .init) ∧
    ops.allows .log [1] .init logAns = true ∧ logAns.result = .ok 0 ∧
    bindLife ops (some x) logAns.store = some .init ∧
    ¬ MutStep .request prog ⟨.exec (.prim (some x) .log [.lit 1] s), [(y, 12)], .init, [.seq .skip]⟩ logAns.label
      ⟨.done, [(x, 0), (y, 12)], .init, [.seq .skip]⟩ :=
  ⟨rfl, rfl, rfl, rfl, missing_blocked rfl⟩

/-- Instance (law `step_ret`). Moving p and reading the counter returns 7 and
records the move before cleanup. Running the deferred bump first would return 8. -/
theorem step_ret_instance :
    step prog ⟨.exec (.ret (.arith .add (.move p) .counter) s), [(p, 7)], ⟨0, [[]]⟩, [.scope ⟨1⟩ [p] [⟨bumpF, [], s⟩]]⟩ =
      some (.next ⟨.exiting (.ret 7), [(p, 7)], DState.onTop (p :: ·) ⟨0, [[]]⟩, [.scope ⟨1⟩ [p] [⟨bumpF, [], s⟩]]⟩) :=
  step_ret prog (.arith .add (.move p) .counter) s [(p, 7)] ⟨0, [[]]⟩ _ [.scope ⟨1⟩ [p] [⟨bumpF, [], s⟩]] 7 rfl

/-- Planted (law `step_ret`). The same evaluable move cannot return when only return is removed. -/
theorem step_ret_planted : ops.eval [(p, 7)] ⟨0, [[]]⟩ (.arith .add (.move p) .counter) =
      .ok (7, DState.onTop (p :: ·) ⟨0, [[]]⟩) ∧
    missingStep .retValue prog ⟨.exec (.ret (.arith .add (.move p) .counter) s), [(p, 7)], ⟨0, [[]]⟩, [.scope ⟨1⟩ [p] [⟨bumpF, [], s⟩]]⟩ ≠
      some (.next ⟨.exiting (.ret 7), [(p, 7)], DState.onTop (p :: ·) ⟨0, [[]]⟩, [.scope ⟨1⟩ [p] [⟨bumpF, [], s⟩]]⟩) :=
  ⟨rfl, by intro h; cases h⟩

/-- Instance (law `step_return_caller`). Callee p is discarded, caller y restored,
x bound to 7, and the moved marker for x cleared in the caller activation. -/
theorem step_return_caller_instance :
    step prog ⟨.exiting (.ret 7), [(p, 99)], ⟨0, [[x, p], [y]]⟩, [.ret (some x) [(y, 12)]]⟩ =
      some (.next ⟨.done, bindDst (some x) 7 [(y, 12)], ⟨0, [[p], [y]]⟩, []⟩) :=
  step_return_caller prog 7 (some x) [(y, 12)] [(p, 99)] ⟨0, [[x, p], [y]]⟩ [] ⟨0, [[p], [y]]⟩ rfl

/-- Planted (law `step_return_caller`). Destination admission still succeeds; only caller return is removed. -/
theorem step_return_caller_planted : bindLife ops (some x) ⟨0, [[x, p], [y]]⟩ = some ⟨0, [[p], [y]]⟩ ∧
    missingStep .returnBind prog ⟨.exiting (.ret 7), [(p, 99)], ⟨0, [[x, p], [y]]⟩, [.ret (some x) [(y, 12)]]⟩ ≠
      some (.next ⟨.done, bindDst (some x) 7 [(y, 12)], ⟨0, [[p], [y]]⟩, []⟩) :=
  ⟨rfl, by intro h; cases h⟩

/-- Distinct events 1 and 2 distinguish both cleanup orders. -/
def d1 : Deferred ops := ⟨logF, [.lit 1], s⟩
/-- The second distinct deferred call. -/
def d2 : Deferred ops := ⟨logF, [.lit 2], s⟩
/-- Logging preserves the store. -/
def afterLogs : DState := DState.init

/-- Helper. Real calls satisfy both completion premises, with a nonempty event trace. -/
private theorem realCalls : RunsAll prog [] [d1, d2] .init ([1] ++ [2]) afterLogs :=
  .cons (demo_log_completes [] .init) (.cons (demo_log_value_completes 2 [] afterLogs) (.nil _))

/-- Helper. A real enclosing scope supplies the cleanup prefix premise. -/
private theorem realCleanup (st : DState) :
    Cleans prog [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]] [] st [1, 2] st := by
  have hd : dropOut ops [] [] st = some st := by
    rcases st with ⟨counter, moved⟩
    cases moved <;> simp [dropOut, dead, dom, ops, DState.life, DState.onTop]
  exact .scope (.cons (demo_log_completes [] st) (.nil _)) hd
    (.scope (.cons (demo_log_value_completes 2 [] st) (.nil _)) hd (.nil _ _))

/-- Helper. A scope is passable for all three structured exits. -/
private theorem passes_nested_scopes (e : Exit Int) :
    ∀ (f : Frame ops), f ∈ ([.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]] : List (Frame ops)) → Passes e f := by
  intro f hf
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hf
  rcases hf with rfl | rfl <;> trivial

/-- Instance (law `ConcreteSpec.Core.Exec.two_defers_register`, case `lifo`). A block registering
`d1` then `d2` reaches `Stmt.skip` with its scope holding `d2` before `d1`. -/
theorem two_defers_register_instance : Steps prog ⟨.exec (.block ⟨1⟩ (.seq (.defer d1.f d1.args d1.site)
      (.seq (.defer d2.f d2.args d2.site) .skip))), [], .init, []⟩ []
    ⟨.exec .skip, [], .init, .scope ⟨1⟩ (dom ([] : AList Var Int)) [d2, d1] :: []⟩ :=
  two_defers_register prog ⟨1⟩ d1 d2 .skip [] .init []

/-- Instance (law `ConcreteSpec.Core.Exec.exit_scope_next`, case `dispatch_first`). Unwinding an
exit into a scope with `d1` before `d2` dispatches `d1` first and saves `Frame.resume` with `Pending.exit`. -/
theorem exit_scope_next_instance : Step prog ⟨.exiting (.ret 0), [], .init, .scope ⟨1⟩ [] (d1 :: [d2]) :: []⟩ .silent
      ⟨.exec d1.stmt, [], .init, .resume (.exit (.ret 0)) :: .scope ⟨1⟩ [] [d2] :: []⟩ ↔
    (Label.silent : Label ops) = .silent ∧
      (⟨.exec d1.stmt, [], .init, .resume (.exit (.ret 0)) :: .scope ⟨1⟩ [] [d2] :: []⟩ : Config ops) =
        ⟨.exec d1.stmt, [], .init, .resume (.exit (.ret 0)) :: .scope ⟨1⟩ [] [d2] :: []⟩ :=
  exit_scope_next prog (.ret 0) ⟨1⟩ [] d1 [d2] [] .init [] .silent
    ⟨.exec d1.stmt, [], .init, .resume (.exit (.ret 0)) :: .scope ⟨1⟩ [] [d2] :: []⟩

/-- Instance (law `ConcreteSpec.Core.Exec.done_scope_next`, case `dispatch_first`). Normal
completion of a scope with `d1` before `d2` dispatches `d1` first and saves `Frame.resume` with `Pending.fall`. -/
theorem done_scope_next_instance : Step prog ⟨.done, [], .init, .scope ⟨1⟩ [] (d1 :: [d2]) :: []⟩ .silent
      ⟨.exec d1.stmt, [], .init, .resume .fall :: .scope ⟨1⟩ [] [d2] :: []⟩ ↔
    (Label.silent : Label ops) = .silent ∧
      (⟨.exec d1.stmt, [], .init, .resume .fall :: .scope ⟨1⟩ [] [d2] :: []⟩ : Config ops) =
        ⟨.exec d1.stmt, [], .init, .resume .fall :: .scope ⟨1⟩ [] [d2] :: []⟩ :=
  done_scope_next prog ⟨1⟩ [] d1 [d2] [] .init [] .silent
    ⟨.exec d1.stmt, [], .init, .resume .fall :: .scope ⟨1⟩ [] [d2] :: []⟩

/-- Instance (law `ConcreteSpec.Core.Exec.exit_scope_runs_defers`, case `log_one_then_two`). Exiting a
scope that registered log(2) then log(1) emits [1, 2] and drops the scope. -/
theorem exit_scope_runs_defers_instance : Steps prog ⟨.exiting (.ret 0), [], .init, .scope ⟨1⟩ [] [d1, d2] :: []⟩ ([1] ++ [2])
    ⟨.exiting (.ret 0), restrict [] [], afterLogs, []⟩ :=
  exit_scope_runs_defers prog (.ret 0) ⟨1⟩ [] [] [] [d1, d2] .init afterLogs ([1] ++ [2])
    afterLogs (.cons (demo_log_completes [] .init) (.cons (demo_log_value_completes 2 [] afterLogs) (.nil _))) rfl

/-- Instance (law `ConcreteSpec.Core.Exec.done_scope_runs_defers`, case `log_one_then_two`). Completing
a scope that registered log(2) then log(1) emits [1, 2] and drops the scope. -/
theorem done_scope_runs_defers_instance : Steps prog ⟨.done, [], .init, .scope ⟨1⟩ [] [d1, d2] :: []⟩ ([1] ++ [2])
    ⟨.done, restrict [] [], afterLogs, []⟩ :=
  done_scope_runs_defers prog ⟨1⟩ [] [] [] [d1, d2] .init afterLogs ([1] ++ [2]) afterLogs
    (.cons (demo_log_completes [] .init) (.cons (demo_log_value_completes 2 [] afterLogs) (.nil _))) rfl

/-- Instance (law `ConcreteSpec.Core.Exec.unwind_runs_plan`, case `nested_scopes`). Unwinding
two nested scope frames emits inner 1 before outer 2 and reaches the empty boundary. -/
theorem unwind_runs_plan_instance : Steps prog ⟨.exiting (.ret 0), [], .init, [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]] ++ []⟩ ([1] ++ ([2] ++ []))
    ⟨.exiting (.ret 0), unwindLocals [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]] [], .init, []⟩ :=
  unwind_runs_plan prog (.ret 0) [] [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]] [] .init .init ([1] ++ ([2] ++ []))
    (.scope (.cons (demo_log_completes [] .init) (.nil _)) rfl
      (.scope (.cons (demo_log_value_completes 2 [] .init) (.nil _)) rfl (.nil _ _)))
    (passes_nested_scopes (.ret 0))

/-- Instance (law `ConcreteSpec.Core.Exec.return_reaches_caller`, case `unwind_and_return`). A
return of 7 unwinds two scopes, emits inner 1 then outer 2, leaves the activation, and
binds `x` to 7 in the caller. -/
theorem return_reaches_caller_instance : Steps prog
    ⟨.exiting (.ret 7), [], ⟨0, [[p], [x]]⟩,
      [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2], .functionEnd, .ret (some x) [(y, 12)]]⟩ [1, 2]
    ⟨.done, bindDst (some x) 7 [(y, 12)], ⟨0, [[]]⟩, []⟩ :=
  return_reaches_caller prog 7 (some x) [(y, 12)] []
    [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]] [] ⟨0, [[p], [x]]⟩ ⟨0, [[p], [x]]⟩ [1, 2]
    ⟨0, [[x]]⟩ ⟨0, [[]]⟩ (realCleanup _) (passes_nested_scopes (.ret 7)) rfl rfl

/-- Instance (law `ConcreteSpec.Core.Exec.break_reaches_loop`, case `unwind_and_break`). A `Stmt.brk`
unwinds nested scopes, emits inner 1 then outer 2, and exits the target loop `Frame.body`. -/
theorem break_reaches_loop_instance : Steps prog
    ⟨.exiting (.brk ⟨1⟩), [], .init,
      [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]] ++ .body ⟨1⟩ .skip (.lit 1) .skip s :: []⟩ ([1] ++ ([2] ++ []))
    ⟨.done, unwindLocals [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]] [], .init, []⟩ :=
  break_reaches_loop prog ⟨1⟩ .skip .skip (.lit 1) s [] [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]] [] .init .init
    ([1] ++ ([2] ++ []))
    (.scope (.cons (demo_log_completes [] .init) (.nil _)) rfl
      (.scope (.cons (demo_log_value_completes 2 [] .init) (.nil _)) rfl (.nil _ _)))
    (passes_nested_scopes (.brk ⟨1⟩))

/-- Instance (law `ConcreteSpec.Core.Exec.continue_reaches_loop`, case `unwind_and_continue`). A
`Stmt.cont` unwinds nested scopes, emits inner 1 then outer 2, and restarts the target loop. -/
theorem continue_reaches_loop_instance : Steps prog
    ⟨.exiting (.cont ⟨1⟩), [], .init,
      [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]] ++ .body ⟨1⟩ .skip (.lit 1) .skip s :: []⟩ ([1] ++ ([2] ++ []))
    ⟨.exec (.loop ⟨1⟩ .skip (.lit 1) .skip s), unwindLocals [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]] [], .init, []⟩ :=
  continue_reaches_loop prog ⟨1⟩ .skip .skip (.lit 1) s [] [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]] [] .init .init
    ([1] ++ ([2] ++ []))
    (.scope (.cons (demo_log_completes [] .init) (.nil _)) rfl
      (.scope (.cons (demo_log_value_completes 2 [] .init) (.nil _)) rfl (.nil _ _)))
    (passes_nested_scopes (.cont ⟨1⟩))

/-- Helper. Every allowed demo log response emits its argument. -/
private theorem allowed_log_event (v : Int) (st : DState) (a : Answer Int Int DState)
    (ha : ops.allows .log [v] st a = true) : a.event = some v := by
  rcases a with ⟨event, result, store⟩
  cases event with
  | none => simp at ha
  | some ev =>
    cases result with
    | error err => simp at ha
    | ok n =>
      by_cases hn : n = 0
      · subst n; simp [ops] at ha; simp [ha.1]
      · simp [ops, hn] at ha

/-- Helper. An accepted log(2) request cannot begin a trace with 1, for any
continuation or final configuration. This checks all allowed answers, not one oracle. -/
private theorem log_two_not_one {r : MissingRule} {c target : Config ops}
    {st : DState} {k : Answer Int Int DState → Option (Config ops)}
    (hd : missingStep r prog c = some (.request .log [2] s st k))
    : ¬ MutSteps r prog c [1, 2] target := by
  intro h
  generalize ht : ([1, 2] : List Int) = tr at h
  cases h with
  | refl => cases ht
  | step hs tail =>
    cases hs with
    | silent he => rw [hd] at he; cases he
    | prim he ha hk =>
      rw [hd] at he
      simp only [Option.some.injEq, Out.request.injEq] at he
      rcases he with ⟨rfl, rfl, rfl, rfl, rfl⟩
      have hev := allowed_log_event _ _ _ ha
      simp [Answer.label, hev, Label.events] at ht


/-- Helper. A log(1) request cannot reach a distinct target without emitting. -/
private theorem log_one_not_empty {r : MissingRule} {c target : Config ops}
    {st : DState} {k : Answer Int Int DState → Option (Config ops)}
    (hd : missingStep r prog c = some (.request .log [1] s st k))
    (hne : c ≠ target) : ¬ MutSteps r prog c [] target := by
  intro h
  generalize ht : ([] : List Int) = tr at h
  cases h with
  | refl => exact hne rfl
  | step hs tail =>
    cases hs with
    | silent he => rw [hd] at he; cases he
    | prim he ha hk =>
      rw [hd] at he
      simp only [Option.some.injEq, Out.request.injEq] at he
      rcases he with ⟨rfl, rfl, rfl, rfl, rfl⟩
      have hev := allowed_log_event _ _ _ ha
      simp [Answer.label, hev, Label.events] at ht


/-- Planted (law `two_defers_register`). Only registration into an already populated scope
is removed. The first registration and all sequencing still run. -/
theorem two_defers_register_planted :
    ¬ MutSteps .registerSecond prog
      ⟨.exec (.block ⟨1⟩ (.seq (.defer d1.f d1.args d1.site)
        (.seq (.defer d2.f d2.args d2.site) .skip))), [], .init, []⟩ []
      ⟨.exec .skip, [], .init, [.scope ⟨1⟩ [] [d2, d1]]⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  have he := (missing_trace rfl h).1
  cases he

/-- Planted (law `exit_scope_next`). Deleting only this cleanup dispatch refutes the forward obligation. -/
theorem exit_scope_next_planted :
    ¬ (MutStep .exitCleanup prog ⟨.exiting (.ret 0), [], .init, [.scope ⟨1⟩ [] [d1, d2]]⟩ .silent
      ⟨.exec d1.stmt, [], .init, [.resume (.exit (.ret 0)), .scope ⟨1⟩ [] [d2]]⟩ ↔
      (Label.silent : Label ops) = .silent ∧
      (⟨.exec d1.stmt, [], .init, [.resume (.exit (.ret 0)), .scope ⟨1⟩ [] [d2]]⟩ : Config ops) =
      ⟨.exec d1.stmt, [], .init, [.resume (.exit (.ret 0)), .scope ⟨1⟩ [] [d2]]⟩) := by
  intro h
  exact missing_blocked rfl (h.mpr ⟨rfl, rfl⟩)

/-- Planted (law `done_scope_next`). Deleting only this cleanup dispatch refutes the forward obligation. -/
theorem done_scope_next_planted :
    ¬ (MutStep .doneCleanup prog ⟨.done, [], .init, [.scope ⟨1⟩ [] [d1, d2]]⟩ .silent
      ⟨.exec d1.stmt, [], .init, [.resume (.fall), .scope ⟨1⟩ [] [d2]]⟩ ↔
      (Label.silent : Label ops) = .silent ∧
      (⟨.exec d1.stmt, [], .init, [.resume (.fall), .scope ⟨1⟩ [] [d2]]⟩ : Config ops) =
      ⟨.exec d1.stmt, [], .init, [.resume (.fall), .scope ⟨1⟩ [] [d2]]⟩) := by
  intro h
  exact missing_blocked rfl (h.mpr ⟨rfl, rfl⟩)

/-- Planted (law `exit_scope_runs_defers`). Both real completion and drop premises hold;
only the named cleanup dispatch is missing. Its trace cannot emit the required log. -/
theorem exit_scope_runs_defers_planted : RunsAll prog [] [d1, d2] .init ([1] ++ [2]) afterLogs ∧
    dropOut ops [] [] afterLogs = some afterLogs ∧
    ¬ MutSteps .exitCleanup prog ⟨.exiting (.ret 0), [], .init, [.scope ⟨1⟩ [] [d1, d2]]⟩ ([1] ++ [2])
      ⟨.exiting (.ret 0), restrict [] [], afterLogs, []⟩ := by
  refine ⟨realCalls, rfl, fun h => ?_⟩
  have ht := (missing_trace rfl h).2
  cases ht

/-- Planted (law `done_scope_runs_defers`). Both real completion and drop premises hold;
only the named cleanup dispatch is missing. Its trace cannot emit the required log. -/
theorem done_scope_runs_defers_planted : RunsAll prog [] [d1, d2] .init ([1] ++ [2]) afterLogs ∧
    dropOut ops [] [] afterLogs = some afterLogs ∧
    ¬ MutSteps .doneCleanup prog ⟨.done, [], .init, [.scope ⟨1⟩ [] [d1, d2]]⟩ ([1] ++ [2])
      ⟨.done, restrict [] [], afterLogs, []⟩ := by
  refine ⟨realCalls, rfl, fun h => ?_⟩
  have ht := (missing_trace rfl h).2
  cases ht

/-- Planted (law `unwind_runs_plan`). Only skipping a sequence on exit is removed;
the empty cleanup certificate and passability remain satisfied. -/
theorem unwind_runs_plan_planted :
    Cleans prog [.seq .skip] [] .init [] .init ∧
    (∀ f ∈ ([.seq .skip] : List (Frame ops)), Passes (O := ops) (.ret 7) f) ∧
    ¬ MutSteps .exitSeq prog ⟨.exiting (.ret 7), [], .init, [.seq .skip]⟩ []
      ⟨.exiting (.ret 7), [], .init, []⟩ := by
  refine ⟨.seq (.nil _ _), ?_, fun h => ?_⟩
  · intro f hf; simp only [List.mem_singleton] at hf; subst f; trivial
  · have he := (missing_trace rfl h).1; cases he

/-- Planted (law `return_reaches_caller`). Only activation-end return admission is removed;
cleanup, passability, leave admission and binding premises are all satisfied. -/
theorem return_reaches_caller_planted :
    Cleans prog [] [] .init [] .init ∧
    (∀ f ∈ ([] : List (Frame ops)), Passes (O := ops) (.ret 7) f) ∧
    ops.life .leave .init = some .init ∧ bindLife ops (some x) .init = some .init ∧
    ¬ MutSteps .functionLeave prog
      ⟨.exiting (.ret 7), [], .init, [.functionEnd, .ret (some x) []]⟩ []
      ⟨.done, bindDst (some x) 7 [], .init, []⟩ := by
  refine ⟨.nil _ _, by simp, rfl, rfl, fun h => ?_⟩
  have he := (missing_trace rfl h).1
  cases he

/-- Planted (law `break_reaches_loop`). Only reception by the matching loop is removed.
The empty cleanup prefix is real and passable; other loop identities are unaffected. -/
theorem break_reaches_loop_planted : Cleans prog [] [] .init [] .init ∧
    (∀ f ∈ ([] : List (Frame ops)), Passes (.brk ⟨1⟩) f) ∧
    ¬ MutSteps .breakHere prog
      ⟨.exiting (.brk ⟨1⟩), [], .init, [.body ⟨1⟩ .skip (.lit 1) .skip s]⟩ []
      ⟨.done, [], .init, []⟩ := by
  refine ⟨.nil _ _, by simp, fun h => ?_⟩
  have he := (missing_trace (by simp [missingStep]) h).1
  cases he

/-- Planted (law `continue_reaches_loop`). Only reception by the matching loop is removed.
The empty cleanup prefix is real and passable; other loop identities are unaffected. -/
theorem continue_reaches_loop_planted : Cleans prog [] [] .init [] .init ∧
    (∀ f ∈ ([] : List (Frame ops)), Passes (.cont ⟨1⟩) f) ∧
    ¬ MutSteps .continueHere prog
      ⟨.exiting (.cont ⟨1⟩), [], .init, [.body ⟨1⟩ .skip (.lit 1) .skip s]⟩ []
      ⟨.exec (.loop ⟨1⟩ .skip (.lit 1) .skip s), [], .init, []⟩ := by
  refine ⟨.nil _ _, by simp, fun h => ?_⟩
  have he := (missing_trace (by simp [missingStep]) h).1
  cases he

/-- Planted (law `exit_scope_runs_defers`). Reversing the calls within the scope
runs log(2) first, so the exact positive instance's trace [1, 2] is impossible. -/
theorem exit_scope_runs_defers_reversed :
    ¬ MutSteps .reverseExit prog ⟨.exiting (.ret 0), [], .init, [.scope ⟨1⟩ [] [d1, d2]]⟩ [1, 2]
      ⟨.exiting (.ret 0), [], .init, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, ht⟩ | h
  · cases ht
  rcases missing_next rfl h with ⟨he, ht⟩ | h
  · cases ht
  exact log_two_not_one rfl h

/-- Planted (law `done_scope_runs_defers`). Reversing the calls within the scope
runs log(2) first, so the exact positive instance's trace [1, 2] is impossible. -/
theorem done_scope_runs_defers_reversed :
    ¬ MutSteps .reverseDone prog ⟨.done, [], .init, [.scope ⟨1⟩ [] [d1, d2]]⟩ [1, 2]
      ⟨.done, [], .init, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, ht⟩ | h
  · cases ht
  rcases missing_next rfl h with ⟨he, ht⟩ | h
  · cases ht
  exact log_two_not_one rfl h

/-- Planted (law `unwind_runs_plan`). Reversing inner and outer scopes
runs log(2) first, so the exact positive instance's trace [1, 2] is impossible. -/
theorem unwind_runs_plan_reversed :
    ¬ MutSteps .outerFirst prog ⟨.exiting (.ret 0), [], .init, [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]]⟩ [1, 2]
      ⟨.exiting (.ret 0), [], .init, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, ht⟩ | h
  · cases ht
  rcases missing_next rfl h with ⟨he, ht⟩ | h
  · cases ht
  exact log_two_not_one rfl h

/-- Planted (law `return_reaches_caller`). Reversing inner and outer scopes
runs log(2) first, so the exact positive instance's trace [1, 2] is impossible. -/
theorem return_reaches_caller_reversed :
    ¬ MutSteps .outerFirst prog ⟨.exiting (.ret 7), [], ⟨0, [[p], [x]]⟩, [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2], .functionEnd, .ret (some x) [(y, 12)]]⟩ [1, 2]
      ⟨.done, bindDst (some x) 7 [(y, 12)], ⟨0, [[]]⟩, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, ht⟩ | h
  · cases ht
  rcases missing_next rfl h with ⟨he, ht⟩ | h
  · cases ht
  exact log_two_not_one rfl h

/-- Planted (law `break_reaches_loop`). Reversing inner and outer scopes
runs log(2) first, so the exact positive instance's trace [1, 2] is impossible. -/
theorem break_reaches_loop_reversed :
    ¬ MutSteps .outerFirst prog ⟨.exiting (.brk ⟨1⟩), [], .init, [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2], .body ⟨1⟩ .skip (.lit 1) .skip s]⟩ [1, 2]
      ⟨.done, [], .init, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, ht⟩ | h
  · cases ht
  rcases missing_next rfl h with ⟨he, ht⟩ | h
  · cases ht
  exact log_two_not_one rfl h

/-- Planted (law `continue_reaches_loop`). Reversing inner and outer scopes
runs log(2) first, so the exact positive instance's trace [1, 2] is impossible. -/
theorem continue_reaches_loop_reversed :
    ¬ MutSteps .outerFirst prog ⟨.exiting (.cont ⟨1⟩), [], .init, [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2], .body ⟨1⟩ .skip (.lit 1) .skip s]⟩ [1, 2]
      ⟨.exec (.loop ⟨1⟩ .skip (.lit 1) .skip s), [], .init, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, ht⟩ | h
  · cases ht
  rcases missing_next rfl h with ⟨he, ht⟩ | h
  · cases ht
  exact log_two_not_one rfl h

/-- Planted (law `two_defers_register`). FIFO insertion preserves both calls but
cannot reach the LIFO instance's frame list, even by continuing execution. -/
theorem two_defers_register_fifo :
    ¬ MutSteps .fifoRegister prog
      ⟨.exec (.block ⟨1⟩ (.seq (.defer d1.f d1.args d1.site)
        (.seq (.defer d2.f d2.args d2.site) .skip))), [], .init, []⟩ []
      ⟨.exec .skip, [], .init, [.scope ⟨1⟩ [] [d2, d1]]⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  exact log_one_not_empty rfl (by intro he; cases he) h

/-- Planted (law `exit_scope_next`). Reversed dispatch selects log(2), not log(1). -/
theorem exit_scope_next_reversed :
    ¬ MutStep .reverseExit prog ⟨.exiting (.ret 0), [], .init, [.scope ⟨1⟩ [] [d1, d2]]⟩ .silent
      ⟨.exec d1.stmt, [], .init, [.resume (.exit (.ret 0)), .scope ⟨1⟩ [] [d2]]⟩ := by
  intro h
  generalize hl : (Label.silent : Label ops) = l at h
  cases h with
  | silent he => cases he
  | prim he _ _ => cases he

/-- Planted (law `done_scope_next`). Reversed dispatch selects log(2), not log(1). -/
theorem done_scope_next_reversed :
    ¬ MutStep .reverseDone prog ⟨.done, [], .init, [.scope ⟨1⟩ [] [d1, d2]]⟩ .silent
      ⟨.exec d1.stmt, [], .init, [.resume (.fall), .scope ⟨1⟩ [] [d2]]⟩ := by
  intro h
  generalize hl : (Label.silent : Label ops) = l at h
  cases h with
  | silent he => cases he
  | prim he _ _ => cases he

/-- Planted (law `step_ret`). Beginning deferred cleanup before evaluating the
return operand runs bump first; evaluating the same expression afterwards returns
8 instead of 7. The instance requires p moved and cleanup still pending. -/
theorem step_ret_cleanup_first :
    missingStep .cleanupBeforeValue prog
      ⟨.exec (.ret (.arith .add (.move p) .counter) s), [(p, 7)], ⟨0, [[]]⟩,
        [.scope ⟨1⟩ [p] [⟨bumpF, [], s⟩]]⟩ ≠
      some (.next ⟨.exiting (.ret 7), [(p, 7)],
        DState.onTop (p :: ·) ⟨0, [[]]⟩, [.scope ⟨1⟩ [p] [⟨bumpF, [], s⟩]]⟩) := by
  intro h
  cases h

/-- Planted (law `step_ret`). The early-cleanup dispatcher preserves the return
expression. Running its next configuration returns 8, while the real order
returns 7; both runs complete, so failure is specifically the order of effects. -/
theorem step_ret_cleanup_first_value :
    (run prog env 100
      ⟨.exec (.ret (.arith .add (.move p) .counter) s), [(p, 7)], ⟨0, [[]]⟩,
        [.scope ⟨1⟩ [p] [⟨bumpF, [], s⟩]]⟩).ending = .final (.returned 7) ∧
    (match missingStep .cleanupBeforeValue prog
      ⟨.exec (.ret (.arith .add (.move p) .counter) s), [(p, 7)], ⟨0, [[]]⟩,
        [.scope ⟨1⟩ [p] [⟨bumpF, [], s⟩]]⟩ with
      | some (.next c) => (run prog env 100 c).ending
      | _ => .stuck) = .final (.returned 8) := by constructor <;> rfl

/-- Instance (law `return_reaches_caller`, case `leave_before_bind`). With a
moved x in the caller, leaving first then binding clears it. Binding in the
callee first would leave the caller's x moved. -/
theorem return_reaches_caller_lifecycle_instance : Steps prog
    ⟨.exiting (.ret 7), [], ⟨0, [[p], [x]]⟩, [.functionEnd, .ret (some x) [(y, 12)]]⟩ []
    ⟨.done, bindDst (some x) 7 [(y, 12)], ⟨0, [[]]⟩, []⟩ :=
  return_reaches_caller prog 7 (some x) [(y, 12)] [] [] []
    ⟨0, [[p], [x]]⟩ ⟨0, [[p], [x]]⟩ [] ⟨0, [[x]]⟩ ⟨0, [[]]⟩
    (.nil _ _) (by simp) rfl rfl

/-- Planted (law `return_reaches_caller`). Reordering the two admitted lifecycle
effects binds in the wrong activation. Both operations succeed, but the moved
marker for the caller's x remains, refuting the lifecycle instance. -/
theorem return_reaches_caller_bind_before_leave :
    ¬ MutSteps .bindBeforeLeave prog
      ⟨.exiting (.ret 7), [], ⟨0, [[p], [x]]⟩, [.functionEnd, .ret (some x) [(y, 12)]]⟩ []
      ⟨.done, bindDst (some x) 7 [(y, 12)], ⟨0, [[]]⟩, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  have he := (missing_trace rfl h).1
  cases he

/-- Planted support. The reverseExit mutation completes with [2, 1]; it neither
blocks nor loops, and both original calls and their lifecycle effects run. -/
theorem reverse_exit_completes : MutSteps .reverseExit prog
    ⟨.exiting (.ret 0), [], .init, [.scope ⟨1⟩ [] [d1, d2]]⟩ [2, 1] ⟨.exiting (.ret 0), [], .init, []⟩ := by
  repeat first
    | exact MutSteps.refl _
    | apply MutSteps.step (MutStep.silent rfl)
    | apply MutSteps.step (O := ops) (tr := [1]) (MutStep.prim (O := ops)
        (a := ⟨some 2, .ok 0, DState.life (.enter [x]) .init⟩) rfl rfl rfl)
    | apply MutSteps.step (O := ops) (tr := []) (MutStep.prim (O := ops)
        (a := ⟨some 1, .ok 0, DState.life (.enter [x]) .init⟩) rfl rfl rfl)

/-- Planted support. The reverseDone mutation completes with [2, 1]; it neither
blocks nor loops, and both original calls and their lifecycle effects run. -/
theorem reverse_done_completes : MutSteps .reverseDone prog
    ⟨.done, [], .init, [.scope ⟨1⟩ [] [d1, d2]]⟩ [2, 1] ⟨.done, [], .init, []⟩ := by
  repeat first
    | exact MutSteps.refl _
    | apply MutSteps.step (MutStep.silent rfl)
    | apply MutSteps.step (O := ops) (tr := [1]) (MutStep.prim (O := ops)
        (a := ⟨some 2, .ok 0, DState.life (.enter [x]) .init⟩) rfl rfl rfl)
    | apply MutSteps.step (O := ops) (tr := []) (MutStep.prim (O := ops)
        (a := ⟨some 1, .ok 0, DState.life (.enter [x]) .init⟩) rfl rfl rfl)

/-- Planted support. The outerFirst mutation completes with [2, 1]; it neither
blocks nor loops, and both original calls and their lifecycle effects run. -/
theorem outer_first_completes : MutSteps .outerFirst prog
    ⟨.exiting (.ret 0), [], .init, [.scope ⟨1⟩ [] [d1], .scope ⟨2⟩ [] [d2]]⟩ [2, 1] ⟨.exiting (.ret 0), [], .init, []⟩ := by
  repeat first
    | exact MutSteps.refl _
    | apply MutSteps.step (MutStep.silent rfl)
    | apply MutSteps.step (O := ops) (tr := [1]) (MutStep.prim (O := ops)
        (a := ⟨some 2, .ok 0, DState.life (.enter [x]) .init⟩) rfl rfl rfl)
    | apply MutSteps.step (O := ops) (tr := []) (MutStep.prim (O := ops)
        (a := ⟨some 1, .ok 0, DState.life (.enter [x]) .init⟩) rfl rfl rfl)


/-! Populated preservation/teardown witnesses complement the event-order witnesses above.
The outer x survives, inner p disappears, p's moved marker disappears, and x's
marker, the counter and the lower activation survive. No operation is a no-op. -/
/-- Helper. Locals before leaving the inner scope. -/
def scopeLocals : AList Var Int := [(p, 20), (x, 10)]
/-- Helper. Independently stated surviving locals. -/
def outerLocals : AList Var Int := [(x, 10)]
/-- Helper. Both live and dead locals have lifecycle markers; lower activation is populated. -/
def scopeState : DState := ⟨9, [[p, x], [y]]⟩
/-- Helper. Only the dead local's marker is removed. -/
def outerState : DState := ⟨9, [[x], [y]]⟩
/-- Helper. A populated scope with no remaining deferred calls. -/
def teardown : Frame ops := .scope ⟨3⟩ [x] []
/-- Helper. Cleanup certificate includes a genuinely changing scope drop. -/
private theorem teardownCleans : Cleans prog [teardown] scopeLocals scopeState [] outerState :=
  .scope (.nil _) rfl (.nil _ _)
/-- Helper. The scope passes every structured exit. -/
private theorem teardownPasses (e : Exit Int) :
    ∀ f ∈ ([teardown] : List (Frame ops)), Passes e f := by
  intro f hf
  simp only [List.mem_singleton] at hf
  subst f
  trivial

/-- Instance (law `exit_scope_runs_defers`). Scope exit restricts locals and drops
only the inner lifecycle marker, preserving the counter and lower activation. -/
theorem exit_scope_teardown_instance : Steps prog
    ⟨.exiting (.ret 7), scopeLocals, scopeState, [teardown]⟩ []
    ⟨.exiting (.ret 7), outerLocals, outerState, []⟩ :=
  exit_scope_runs_defers prog (.ret 7) ⟨3⟩ [x] scopeLocals [] []
    scopeState scopeState [] outerState (.nil _) rfl

/-- Instance (law `done_scope_runs_defers`). Normal completion has the same
observable local and lifecycle teardown obligations as structured exit. -/
theorem done_scope_teardown_instance : Steps prog
    ⟨.done, scopeLocals, scopeState, [teardown]⟩ []
    ⟨.done, outerLocals, outerState, []⟩ :=
  done_scope_runs_defers prog ⟨3⟩ [x] scopeLocals [] []
    scopeState scopeState [] outerState (.nil _) rfl

/-- Instance (law `unwind_runs_plan`). The cleanup prefix must retain outer x,
remove inner p, and remove only p's lifecycle marker before reaching its boundary. -/
theorem unwind_runs_plan_teardown_instance : Steps prog
    ⟨.exiting (.ret 7), scopeLocals, scopeState, teardown :: []⟩ []
    ⟨.exiting (.ret 7), outerLocals, outerState, []⟩ :=
  unwind_runs_plan prog (.ret 7) [] [teardown] scopeLocals scopeState outerState [] teardownCleans (teardownPasses _)

/-- Instance (law `break_reaches_loop`). The cleanup prefix must retain outer x,
remove inner p, and remove only p's lifecycle marker before reaching its boundary. -/
theorem break_reaches_loop_teardown_instance : Steps prog
    ⟨.exiting (.brk ⟨1⟩), scopeLocals, scopeState, teardown :: [.body ⟨1⟩ .skip (.lit 0) .skip s]⟩ []
    ⟨.done, outerLocals, outerState, []⟩ :=
  break_reaches_loop prog ⟨1⟩ .skip .skip (.lit 0) s [] [teardown] scopeLocals scopeState outerState [] teardownCleans (teardownPasses _)

/-- Instance (law `continue_reaches_loop`). The cleanup prefix must retain outer x,
remove inner p, and remove only p's lifecycle marker before reaching its boundary. -/
theorem continue_reaches_loop_teardown_instance : Steps prog
    ⟨.exiting (.cont ⟨1⟩), scopeLocals, scopeState, teardown :: [.body ⟨1⟩ .skip (.lit 0) .skip s]⟩ []
    ⟨.exec (.loop ⟨1⟩ .skip (.lit 0) .skip s), outerLocals, outerState, []⟩ :=
  continue_reaches_loop prog ⟨1⟩ .skip .skip (.lit 0) s [] [teardown] scopeLocals scopeState outerState [] teardownCleans (teardownPasses _)

/-- Instance (law `return_reaches_caller`). Scope teardown clears p before leave;
the populated lower activation then undergoes result binding, preserving counter 9.
The intermediate local restriction is witnessed by exit_scope_teardown_instance. -/
theorem return_reaches_caller_teardown_instance : Steps prog
    ⟨.exiting (.ret 7), scopeLocals, scopeState,
      [teardown, .functionEnd, .ret (some y) [(x, 10)]]⟩ []
    ⟨.done, [(y, 7), (x, 10)], ⟨9, [[]]⟩, []⟩ :=
  return_reaches_caller prog 7 (some y) [(x, 10)] [] [teardown] scopeLocals
    scopeState outerState [] ⟨9, [[y]]⟩ ⟨9, [[]]⟩ teardownCleans (teardownPasses _) rfl rfl

/-- Planted (law `exit_scope_runs_defers`). Only skipRestriction changes: it retains inner p while performing the lifecycle drop.
The exact populated teardown instance is unreachable. -/
theorem exit_scope_runs_defers_skipRestriction :
    ¬ MutSteps .skipRestriction prog ⟨.exiting (.ret 7), scopeLocals, scopeState, teardown :: []⟩ []
      ⟨.exiting (.ret 7), outerLocals, outerState, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  have he := (missing_trace rfl h).1
  cases he

/-- Planted (law `exit_scope_runs_defers`). Only skipDrop changes: it restricts locals correctly but retains the dead lifecycle marker.
The exact populated teardown instance is unreachable. -/
theorem exit_scope_runs_defers_skipDrop :
    ¬ MutSteps .skipDrop prog ⟨.exiting (.ret 7), scopeLocals, scopeState, teardown :: []⟩ []
      ⟨.exiting (.ret 7), outerLocals, outerState, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  have he := (missing_trace rfl h).1
  cases he

/-- Planted (law `done_scope_runs_defers`). Only skipRestriction changes: it retains inner p while performing the lifecycle drop.
The exact populated teardown instance is unreachable. -/
theorem done_scope_runs_defers_skipRestriction :
    ¬ MutSteps .skipRestriction prog ⟨.done, scopeLocals, scopeState, teardown :: []⟩ []
      ⟨.done, outerLocals, outerState, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  have he := (missing_trace rfl h).1
  cases he

/-- Planted (law `done_scope_runs_defers`). Only skipDrop changes: it restricts locals correctly but retains the dead lifecycle marker.
The exact populated teardown instance is unreachable. -/
theorem done_scope_runs_defers_skipDrop :
    ¬ MutSteps .skipDrop prog ⟨.done, scopeLocals, scopeState, teardown :: []⟩ []
      ⟨.done, outerLocals, outerState, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  have he := (missing_trace rfl h).1
  cases he

/-- Planted (law `unwind_runs_plan`). Only skipRestriction changes: it retains inner p while performing the lifecycle drop.
The exact populated teardown instance is unreachable. -/
theorem unwind_runs_plan_skipRestriction :
    ¬ MutSteps .skipRestriction prog ⟨.exiting (.ret 7), scopeLocals, scopeState, teardown :: []⟩ []
      ⟨.exiting (.ret 7), outerLocals, outerState, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  have he := (missing_trace rfl h).1
  cases he

/-- Planted (law `unwind_runs_plan`). Only skipDrop changes: it restricts locals correctly but retains the dead lifecycle marker.
The exact populated teardown instance is unreachable. -/
theorem unwind_runs_plan_skipDrop :
    ¬ MutSteps .skipDrop prog ⟨.exiting (.ret 7), scopeLocals, scopeState, teardown :: []⟩ []
      ⟨.exiting (.ret 7), outerLocals, outerState, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  have he := (missing_trace rfl h).1
  cases he

/-- Planted (law `break_reaches_loop`). Only skipRestriction changes: it retains inner p while performing the lifecycle drop.
The exact populated teardown instance is unreachable. -/
theorem break_reaches_loop_skipRestriction :
    ¬ MutSteps .skipRestriction prog ⟨.exiting (.brk ⟨1⟩), scopeLocals, scopeState, teardown :: [.body ⟨1⟩ .skip (.lit 0) .skip s]⟩ []
      ⟨.done, outerLocals, outerState, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  have he := (missing_trace rfl h).1
  cases he

/-- Planted (law `break_reaches_loop`). Only skipDrop changes: it restricts locals correctly but retains the dead lifecycle marker.
The exact populated teardown instance is unreachable. -/
theorem break_reaches_loop_skipDrop :
    ¬ MutSteps .skipDrop prog ⟨.exiting (.brk ⟨1⟩), scopeLocals, scopeState, teardown :: [.body ⟨1⟩ .skip (.lit 0) .skip s]⟩ []
      ⟨.done, outerLocals, outerState, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  have he := (missing_trace rfl h).1
  cases he

/-- Planted (law `continue_reaches_loop`). Only skipRestriction changes: it retains inner p while performing the lifecycle drop.
The exact populated teardown instance is unreachable. -/
theorem continue_reaches_loop_skipRestriction :
    ¬ MutSteps .skipRestriction prog ⟨.exiting (.cont ⟨1⟩), scopeLocals, scopeState, teardown :: [.body ⟨1⟩ .skip (.lit 0) .skip s]⟩ []
      ⟨.exec (.loop ⟨1⟩ .skip (.lit 0) .skip s), outerLocals, outerState, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  have he := (missing_trace rfl h).1
  cases he

/-- Planted (law `continue_reaches_loop`). Only skipDrop changes: it restricts locals correctly but retains the dead lifecycle marker.
The exact populated teardown instance is unreachable. -/
theorem continue_reaches_loop_skipDrop :
    ¬ MutSteps .skipDrop prog ⟨.exiting (.cont ⟨1⟩), scopeLocals, scopeState, teardown :: [.body ⟨1⟩ .skip (.lit 0) .skip s]⟩ []
      ⟨.exec (.loop ⟨1⟩ .skip (.lit 0) .skip s), outerLocals, outerState, []⟩ := by
  intro h
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  rcases missing_next rfl h with ⟨he, _⟩ | h
  · cases he
  have he := (missing_trace rfl h).1
  cases he


/-- Instance (law `steps_request`). Moving the argument changes lifecycle state;
result binding clears x but preserves p, the counter, the lower activation and frames. -/
theorem steps_request_lifecycle_instance :
    Step prog ⟨.exec (.prim (some x) .log [.move p] s), [(p, 20), (y, 12)],
      ⟨9, [[x], [y]]⟩, [.seq .skip]⟩ (.emit 20)
      ⟨.done, [(x, 0), (p, 20), (y, 12)], ⟨9, [[p], [y]]⟩, [.seq .skip]⟩ :=
  steps_request prog (some x) .log [.move p] s [(p, 20), (y, 12)]
    ⟨9, [[x], [y]]⟩ ⟨9, [[p, x], [y]]⟩ ⟨9, [[p], [y]]⟩ [20] [.seq .skip]
    ⟨some 20, .ok 0, ⟨9, [[p, x], [y]]⟩⟩ 0 rfl rfl rfl rfl

/-- Instance (law `steps_request`). The accepted answer's counter differs from the
request store: retaining the old store loses the primitive's effect. -/
theorem steps_request_store_instance :
    Step prog ⟨.exec (.prim (some x) .bump [] s), [(y, 12)], scopeState, [.seq .skip]⟩ .silent
      ⟨.done, [(x, 0), (y, 12)], ⟨10, [[p], [y]]⟩, [.seq .skip]⟩ :=
  steps_request prog (some x) .bump [] s [(y, 12)] scopeState scopeState
    ⟨10, [[p], [y]]⟩ [] [.seq .skip] ⟨none, .ok 0, ⟨10, [[p, x], [y]]⟩⟩ 0 rfl rfl rfl rfl

/-- Instance (law `two_defers_register`). Populated locals determine the saved
domain; registration preserves state and the enclosing continuation. -/
theorem two_defers_register_preservation_instance : Steps prog
    ⟨.exec (.block ⟨1⟩ (.seq (.defer d1.f d1.args d1.site)
      (.seq (.defer d2.f d2.args d2.site) .skip))), scopeLocals, scopeState, [.seq .skip]⟩ []
    ⟨.exec .skip, scopeLocals, scopeState,
      [.scope ⟨1⟩ [p, x] [d2, d1], .seq .skip]⟩ :=
  two_defers_register prog ⟨1⟩ d1 d2 .skip scopeLocals scopeState [.seq .skip]

/-- Instance (law `exit_scope_next`). Dispatch preserves populated locals, domain,
state, remaining calls and lower continuation while remembering the exit value. -/
theorem exit_scope_next_preservation_instance : Step prog
    ⟨.exiting (.ret 7), scopeLocals, scopeState, [.scope ⟨1⟩ [x] [d1, d2], .seq .skip]⟩ .silent
    ⟨.exec d1.stmt, scopeLocals, scopeState,
      [.resume (.exit (.ret 7)), .scope ⟨1⟩ [x] [d2], .seq .skip]⟩ :=
  (exit_scope_next prog (.ret 7) ⟨1⟩ [x] d1 [d2] scopeLocals scopeState [.seq .skip] .silent _).mpr ⟨rfl, rfl⟩

/-- Instance (law `done_scope_next`). Dispatch preserves populated locals, saved domain,
state, remaining calls and the lower continuation, while remembering the pending exit. -/
theorem done_scope_next_preservation_instance : Step prog
    ⟨.done, scopeLocals, scopeState, [.scope ⟨1⟩ [x] [d1, d2], .seq .skip]⟩ .silent
    ⟨.exec d1.stmt, scopeLocals, scopeState,
      [.resume (.fall), .scope ⟨1⟩ [x] [d2], .seq .skip]⟩ :=
  (done_scope_next prog ⟨1⟩ [x] d1 [d2] scopeLocals scopeState [.seq .skip] .silent _).mpr ⟨rfl, rfl⟩

/-- Instance (law `exit_scope_runs_defers`). Bump and two logs make store threading and
trace order observable together with local restriction and lifecycle teardown. -/
theorem exit_scope_runs_defers_effects_instance : Steps prog
    ⟨.exiting (.ret 7), scopeLocals, scopeState,
      [.scope ⟨1⟩ [x] [⟨bumpF, [], s⟩, d1, d2], .seq .skip]⟩ [1, 2]
    ⟨.exiting (.ret 7), outerLocals, ⟨10, [[x], [y]]⟩, [.seq .skip]⟩ :=
  exit_scope_runs_defers prog (.ret 7) ⟨1⟩ [x] scopeLocals [.seq .skip]
    [⟨bumpF, [], s⟩, d1, d2] scopeState ⟨10, [[p, x], [y]]⟩ [1, 2]
    ⟨10, [[x], [y]]⟩
    (.cons (demo_bump_completes _ _) (.cons (demo_log_completes _ _)
      (.cons (demo_log_value_completes 2 _ _) (.nil _)))) rfl

/-- Instance (law `done_scope_runs_defers`). Bump and two logs make store threading and
trace order observable together with local restriction and lifecycle teardown. -/
theorem done_scope_runs_defers_effects_instance : Steps prog
    ⟨.done, scopeLocals, scopeState,
      [.scope ⟨1⟩ [x] [⟨bumpF, [], s⟩, d1, d2], .seq .skip]⟩ [1, 2]
    ⟨.done, outerLocals, ⟨10, [[x], [y]]⟩, [.seq .skip]⟩ :=
  done_scope_runs_defers prog ⟨1⟩ [x] scopeLocals [.seq .skip]
    [⟨bumpF, [], s⟩, d1, d2] scopeState ⟨10, [[p, x], [y]]⟩ [1, 2]
    ⟨10, [[x], [y]]⟩
    (.cons (demo_bump_completes _ _) (.cons (demo_log_completes _ _)
      (.cons (demo_log_value_completes 2 _ _) (.nil _)))) rfl


/-- Helper. Both nested scopes have a different dead local and a distinct log. -/
def populatedScopes : List (Frame ops) :=
  [.scope ⟨1⟩ [y, x] [d1], .scope ⟨2⟩ [x] [d2]]
/-- Helper. Nested cleanup removes p then y, preserves x, threads the store,
and logs inner 1 before outer 2. The two drops are both observable. -/
private theorem populatedCleanup : Cleans prog populatedScopes
    [(p, 20), (y, 30), (x, 10)] ⟨9, [[p, y, x], [a]]⟩ [1, 2] ⟨9, [[x], [a]]⟩ :=
  .scope (.cons (demo_log_completes _ _) (.nil _)) rfl
    (.scope (.cons (demo_log_value_completes 2 _ _) (.nil _)) rfl (.nil _ _))
/-- Helper. Both nested scopes pass each structured exit. -/
private theorem populatedPasses (e : Exit Int) :
    ∀ f ∈ populatedScopes, Passes e f := by
  intro f hf
  simp only [populatedScopes, List.mem_cons, List.not_mem_nil, or_false] at hf
  rcases hf with rfl | rfl <;> trivial

/-- Instance (law `unwind_runs_plan`). Inner p and outer y disappear in separate
scope exits; x and its moved marker survive, as do counter and lower activation.
The trace simultaneously witnesses inner-before-outer cleanup. -/
theorem unwind_runs_plan_populated_instance : Steps prog
    ⟨.exiting (.ret 7), [(p, 20), (y, 30), (x, 10)], ⟨9, [[p, y, x], [a]]⟩,
      populatedScopes ++ []⟩ [1, 2]
    ⟨.exiting (.ret 7), [(x, 10)], ⟨9, [[x], [a]]⟩, []⟩ :=
  unwind_runs_plan prog (.ret 7) [] populatedScopes [(p, 20), (y, 30), (x, 10)]
    ⟨9, [[p, y, x], [a]]⟩ ⟨9, [[x], [a]]⟩ [1, 2] populatedCleanup (populatedPasses _)

/-- Instance (law `break_reaches_loop`). Inner p and outer y disappear in separate
scope exits; x and its moved marker survive, as do counter and lower activation.
The trace simultaneously witnesses inner-before-outer cleanup. -/
theorem break_reaches_loop_populated_instance : Steps prog
    ⟨.exiting (.brk ⟨1⟩), [(p, 20), (y, 30), (x, 10)], ⟨9, [[p, y, x], [a]]⟩,
      populatedScopes ++ [.body ⟨1⟩ .skip (.lit 0) .skip s]⟩ [1, 2]
    ⟨.done, [(x, 10)], ⟨9, [[x], [a]]⟩, []⟩ :=
  break_reaches_loop prog ⟨1⟩ .skip .skip (.lit 0) s [] populatedScopes [(p, 20), (y, 30), (x, 10)]
    ⟨9, [[p, y, x], [a]]⟩ ⟨9, [[x], [a]]⟩ [1, 2] populatedCleanup (populatedPasses _)

/-- Instance (law `continue_reaches_loop`). Inner p and outer y disappear in separate
scope exits; x and its moved marker survive, as do counter and lower activation.
The trace simultaneously witnesses inner-before-outer cleanup. -/
theorem continue_reaches_loop_populated_instance : Steps prog
    ⟨.exiting (.cont ⟨1⟩), [(p, 20), (y, 30), (x, 10)], ⟨9, [[p, y, x], [a]]⟩,
      populatedScopes ++ [.body ⟨1⟩ .skip (.lit 0) .skip s]⟩ [1, 2]
    ⟨.exec (.loop ⟨1⟩ .skip (.lit 0) .skip s), [(x, 10)], ⟨9, [[x], [a]]⟩, []⟩ :=
  continue_reaches_loop prog ⟨1⟩ .skip .skip (.lit 0) s [] populatedScopes [(p, 20), (y, 30), (x, 10)]
    ⟨9, [[p, y, x], [a]]⟩ ⟨9, [[x], [a]]⟩ [1, 2] populatedCleanup (populatedPasses _)

end CoreEvidence
end ConcreteSpec.Core.Exec
