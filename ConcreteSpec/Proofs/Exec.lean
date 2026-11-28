import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Tactics

/-!
# Execution, structured exits and cleanup boundaries (F09a/E1–F10/E4)

PROPOSED until Core freeze. No historical approval is inherited.
Each Law cites the admitted Spec/Exec definitions by stable declaration name and
its decision basis beside the declaration. These are consequences of the existing
machine, not new choices of execution policy. The foundation C1 interface/law
record (`docs/decisions.md#decided-2026-11-08-foundation`, `#foundation-q1`)
approves execution, calls and cleanup laws; the rules live in Lean. Core remains
unfrozen. The sole `runFrom` driver is unchanged.
E3 authority: `docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests`
admits inductive law subjects, classifies function-tautological determinism as
Representation lemmas, and requires independent-rule determinism and two-way
agreement with the executable step under the same external answer. Its law tests
use same-shape mutants breaking one rule, substituted at every occurrence, with
Lean-checked concrete counterexamples preserving the law's premises.
Tag vocabulary only: `docs/decisions.md#decided-2026-11-13-proof-labels`.
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

/-! ## Cleanup and saved-caller boundaries (F09b, E2)

Only scope/test cleanup is observed before a function-end boundary. No second
driver is introduced. Historical approval tokens are discarded.

The complete source command groups are repaired here: cleanup action planning and
certificates; first-boundary observation; trapped runs; saved-caller preservation
and first-return segmentation; raw cleanup completeness; abort and first argument
trap; the complete CleanupEvidence replacement and call-projection controls.
E1 owns the shared completion and execution-composition helpers.

Projection and reconstruction of observation certificates are Representation
lemmas or Helpers, not independent language guarantees. In particular, an observer
can certify a call body at a chosen saved continuation; it does not assert a raw
execution of the entire unwind. `boundary_complete` starts from actual machine
steps, and `raw_cleanup_runs_plan` is the language guarantee of exact cleanup.
Their controls preserve actual raw first-arrival and endpoint premises.

Held, without introducing their definitions or dependent evidence: U+FEFF, bare CR,
constant restrictions and queued partial approvals (ISS-0064/ISS-0065); path policy
(ISS-0068); Boolean encoding (ISS-0069); pointer bytes, struct layout and whole-pointer
representation (ISS-0070); SPARK char preconditions (ISS-0071);
startup and ABI (ISS-0073). The demo interfaces do not settle those questions.
The 2026-11-21 decision
(docs/decisions.md#decided-2026-11-21-secrets-after-1-0) defers wiping until after 1.0
and closes ISS-0072.
-/
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

/-- Representation lemma (subject `ConcreteSpec.Core.Exec.planWith`, `ConcreteSpec.Core.Exec.plan`). Filtering
`planWith` pre L to its `CleanAct.call` actions yields `plan` pre.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. `#od2` clause (5) / `#core-q9-8` require nested reverse cleanup;
`#od69` limits teardown to storage; `#decided-2026-11-13-lifecycle-refusal`
requires lifecycle admission. Function-end is a boundary, as in admitted `Passes`. -/
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
  | .functionEnd :: k, L => planWith_calls k L

/-- A list of cleanup actions run one after another, the state threaded: each call completes, and
each scope left tells the instrumentation what it drops. -/
inductive RunsEach (P : Program O) : List (CleanAct O) → O.Store → List O.Event → O.Store → Prop where
  | nil (st : O.Store) : RunsEach P [] st [] st
  | call {L d rest st tr₁ st₁ tr₂ st₂} : Completes P L d st tr₁ st₁ →
      RunsEach P rest st₁ tr₂ st₂ → RunsEach P (.call L d :: rest) st (tr₁ ++ tr₂) st₂
  | drop {dm L rest st stDrop tr st'} : dropOut O dm L st = some stDrop →
      RunsEach P rest stDrop tr st' →
      RunsEach P (.drop dm L (restrict dm L) :: rest) st tr st'

/-- Helper (inversion). Composes a `RunsAll` call certificate sequence with a `RunsEach` suffix. -/
theorem RunsAll.each {P : Program O} {L : AList Var O.Val} {ds : List (Deferred O)}
    {st st₁ st₂ : O.Store} {tr₁ tr₂ : List O.Event} {rest : List (CleanAct O)}
    (h : RunsAll P L ds st tr₁ st₁) (h' : RunsEach P rest st₁ tr₂ st₂) :
    RunsEach P (ds.map (.call L) ++ rest) st (tr₁ ++ tr₂) st₂ := by
  induction h with
  | nil => simpa using h'
  | cons hd _ ih => simpa [List.append_assoc] using RunsEach.call hd (ih h')

/-- Helper (inversion). Splits a `RunsEach` over mapped calls and a suffix. -/
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

/-- Frames eligible for scope-cleanup traversal. Function-end, saved caller and
running-cleanup boundaries are excluded, consistently with admitted `Passes`. -/
def Frame.passable : Frame O → Bool
  | .resume _ | .ret .. | .functionEnd => false
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
  | .functionEnd :: _, _, _, _, _, hp => absurd (hp _ List.mem_cons_self) (by simp [Frame.passable])
  | .ret .. :: _, _, _, _, _, hp => absurd (hp _ List.mem_cons_self) (by simp [Frame.passable])

/-- Helper (inversion). Every frame an exit passes is passable. -/
theorem Passes.passable {e : Exit O.Val} : ∀ {f : Frame O}, Passes e f → f.passable = true
  | .seq _, _ | .scope .., _ | .test .., _ | .body .., _ => rfl
  | .resume _, h => h.elim
  | .ret .., h => h.elim
  | .functionEnd, h => h.elim

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
uses `R = Step P`. A nonempty prefix is required before every observed transition. A direct pass
never crosses function-end, the boundary excluded by admitted `Passes`: execution
stops at the first arrival at the boundary, before the caller/loop below it can run. -/
inductive BoundaryRun (P : Program O) (R : Config O → Label O → Config O → Prop)
    (e : Exit O.Val) (k : List (Frame O)) :
    List (Frame O) → AList Var O.Val → O.Store → List (CleanAct O) → List O.Event →
      AList Var O.Val → O.Store → Prop where
  | stop {L st} : BoundaryRun P R e k [] L st [] [] L st
  | pass {pre pre' L L' st st' acts tr Lend stEnd} : pre ≠ [] →
      pre.head? ≠ some .functionEnd →
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

/-- Helper (inversion). Compose independent action executions. -/
private theorem performed_append_inv {P : Program O} {as bs : List (CleanAct O)}
    {L L₁ L₂ : AList Var O.Val} {st st₁ st₂ : O.Store} {tr₁ tr₂ : List O.Event}
    (ha : Performed P as L st tr₁ L₁ st₁) (hb : Performed P bs L₁ st₁ tr₂ L₂ st₂) :
    Performed P (as ++ bs) L st (tr₁ ++ tr₂) L₂ st₂ := by
  induction ha with
  | nil => simpa using hb
  | call hc _ ih => simpa [List.append_assoc] using Performed.call hc (ih hb)
  | drop hd _ ih => exact .drop hd (ih hb)

/-- Helper (inversion). Internal dispatcher decomposition supporting the observable silence
law `exit_label`; not an independent soundness claim. -/
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
        cases hb : bindLife O dst st <;> simp [stepExit, hb]
    | functionEnd =>
      cases e <;> cases hl : O.life .leave st <;> simp [stepExit, hl]

/-- Helper (inversion). Converts the transition representation using the separately proved exit-silence restriction. -/
private theorem exit_step_next_inv {P : Program O} {e : Exit O.Val} {L : AList Var O.Val}
    {st : O.Store} {frames : List (Frame O)} {label : Label O} {c : Config O}
    (h : Step P ⟨.exiting e, L, st, frames⟩ label c) :
    step P ⟨.exiting e, L, st, frames⟩ = some (.next c) := by
  cases h with
  | silent h => exact h
  | prim h _ _ => exact False.elim (stepExit_no_request_inv L st e frames _ _ _ _ _ h)

/-- Helper (inversion). A directly passing exit step removes precisely the top frame,
restores its saved locals, and performs its local drop. -/
private theorem exit_pass_plan_inv {P : Program O} {e : Exit O.Val} {k pre pre' : List (Frame O)}
    {L L' : AList Var O.Val} {st st' : O.Store} {label : Label O}
    (h : Step P ⟨.exiting e, L, st, pre ++ k⟩ label
      ⟨.exiting e, L', st', pre' ++ k⟩) (hne : pre ≠ [])
    (hbound : pre.head? ≠ some .functionEnd) :
    ((∀ f ∈ pre', Passes e f) → ∀ f ∈ pre, Passes e f) ∧
      planWith pre L = leaving pre L L' ++ planWith pre' L' ∧
      unwindLocals pre L = unwindLocals pre' L' ∧
      Performed P (leaving pre L L') L st [] L' st' := by
  cases pre with
  | nil => exact False.elim (hne rfl)
  | cons f pre =>
    have next : step P ⟨.exiting e, L, st, f :: (pre ++ k)⟩ =
        some (.next ⟨.exiting e, L', st', pre' ++ k⟩) := exit_step_next_inv h
    cases f with
    | seq t =>
      unfold_step step, stepExit at next
      invert_step next
      have hk' := List.append_cancel_right ‹_›
      subst pre'
      exact ⟨by forall_cons, rfl, rfl, .nil⟩
    | scope b dm ds =>
      cases ds with
      | cons d ds => unfold_step step, stepExit, startCleanup at next
      | nil =>
        crush_option [step, stepExit] at next
        invert_step next
        have hk' := List.append_cancel_right ‹_›
        subst pre'
        exact ⟨by forall_cons, rfl, rfl, .drop ‹_› .nil⟩
    | test l cr t body site dm =>
      crush_option [step, stepExit] at next
      invert_step next
      have hk' := List.append_cancel_right ‹_›
      subst pre'
      exact ⟨by forall_cons, rfl, rfl, .drop ‹_› .nil⟩
    | body l cr t body site =>
      cases e with
      | ret v =>
        unfold_step step, stepExit at next
        invert_step next
        have hk' := List.append_cancel_right ‹_›
        subst pre'
        exact ⟨by forall_cons, rfl, rfl, .nil⟩
      | brk l' | cont l' =>
        crush_option [step, stepExit] at next
        invert_step next
        have hk' := List.append_cancel_right ‹_›
        subst pre'
        exact ⟨by forall_cons, rfl, rfl, .nil⟩
    | functionEnd => exact False.elim (hbound rfl)
    | resume p => unfold_step step, stepExit at next
    | ret dst caller => cases e <;> (crush_option [step, stepExit] at next; try invert_step next)


/-- Helper (inversion). A cleanup dispatch step selects the head registration and preserves
its scope with the remaining registrations. -/
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

    | functionEnd => cases e <;> cases hl : O.life .leave st <;> simp [step, stepExit, hl] at h
    | body l cr t body site =>
      cases e with
      | ret v => simp [step, stepExit] at h
      | brk l' => by_cases he : l' = l <;> simp [step, stepExit, he] at h
      | cont l' => by_cases he : l' = l <;> simp [step, stepExit, he, Deferred.stmt] at h

/-- Helper (subject `ConcreteSpec.Core.Exec.Step`). A completed sequence of deferred calls dispatches each call before the final scope drop.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. `#od2` clause (5) / `#core-q9-8` require nested reverse cleanup;
`#od69` limits teardown to storage; `#decided-2026-11-13-lifecycle-refusal`
requires lifecycle admission. Function-end is a boundary, as in admitted `Passes`. -/
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

/-- Helper (subject `ConcreteSpec.Core.Exec.Step`). An independently performed action plan reconstructs the actual first-boundary execution.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. `#od2` clause (5) / `#core-q9-8` require nested reverse cleanup;
`#od69` limits teardown to storage; `#decided-2026-11-13-lifecycle-refusal`
requires lifecycle admission. Function-end is a boundary, as in admitted `Passes`. -/
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
        (by simp) (by simp) (.silent rfl) (ih hp' hperf)
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
          (by simp) (by simp) hs (ih hp' hrest)
    | test l cr t body site dm =>
      simp only [planWith] at hperf ⊢
      cases hperf with
      | drop hd hrest =>
        have hs : Step P ⟨.exiting e, L, st, .test l cr t body site dm :: (pre ++ k)⟩ .silent
            ⟨.exiting e, restrict dm L, _, pre ++ k⟩ :=
          .silent (by simp only [step, stepExit, hd]; rfl)
        simpa [leaving] using BoundaryRun.pass (pre := .test l cr t body site dm :: pre)
          (pre' := pre) (by simp) (by simp) hs (ih hp' hrest)
    | body l cr t body site =>
      cases e with
      | ret v =>
        simpa [leaving, planWith] using BoundaryRun.pass
          (pre := .body l cr t body site :: pre) (pre' := pre) (by simp) (by simp) (.silent rfl) (ih hp' hperf)
      | brk l' =>
        have hne : l' ≠ l := hpf
        have hs : Step P ⟨.exiting (.brk l'), L, st, .body l cr t body site :: (pre ++ k)⟩ .silent
            ⟨.exiting (.brk l'), L, st, pre ++ k⟩ :=
          .silent (by simp [step, stepExit, hne])
        simpa [leaving, planWith] using BoundaryRun.pass
          (pre := .body l cr t body site :: pre) (pre' := pre) (by simp) (by simp) hs (ih hp' hperf)
      | cont l' =>
        have hne : l' ≠ l := hpf
        have hs : Step P ⟨.exiting (.cont l'), L, st, .body l cr t body site :: (pre ++ k)⟩ .silent
            ⟨.exiting (.cont l'), L, st, pre ++ k⟩ :=
          .silent (by simp [step, stepExit, hne])
        simpa [leaving, planWith] using BoundaryRun.pass
          (pre := .body l cr t body site :: pre) (pre' := pre) (by simp) (by simp) hs (ih hp' hperf)
    | functionEnd => exact False.elim hpf
    | resume p => exact False.elim hpf
    | ret dst caller => exact False.elim hpf

/-- Helper (subject `ConcreteSpec.Core.Exec.Step`). A successful actual unwind, stopped on first
arrival at the selected stack boundary, occurs if and only if every frame in pre passes the
exit, the observed actions are `planWith` pre L, the final locals are `unwindLocals` pre L, and
all planned calls and drops are performed in order.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. `#od2` clause (5) / `#core-q9-8` require nested reverse cleanup;
`#od69` limits teardown to storage; `#decided-2026-11-13-lifecycle-refusal`
requires lifecycle admission. Function-end is a boundary, as in admitted `Passes`. -/
theorem cleanup_runs_exact_plan (P : Program O) (e : Exit O.Val) (k pre : List (Frame O))
    (L L' : AList Var O.Val) (st st' : O.Store) (acts : List (CleanAct O)) (tr : List O.Event) :
    BoundaryRun P (Step P) e k pre L st acts tr L' st' ↔
      (∀ (f : Frame O), f ∈ pre → Passes e f) ∧ acts = planWith pre L ∧
        L' = unwindLocals pre L ∧ Performed P acts L st tr L' st' := by
  constructor
  · intro h
    induction h with
    | stop => exact ⟨(by intro _ hf; cases hf), rfl, rfl, Performed.nil⟩
    | pass hne hbound hs _ ih =>
      obtain ⟨hpp, he, hu, hp⟩ := exit_pass_plan_inv hs hne hbound
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

/-- Helper (inversion). A missing implementation result has no relational transition. -/
theorem no_step_of_none_inv {P : Program O} {c : Config O} (h : step P c = none) :
    Blocked (Step P) c := by
  intro l c' hs
  cases hs with
  | silent hs => rw [h] at hs; cases hs
  | prim hs _ _ => rw [h] at hs; cases hs

/-- Law (subject `ConcreteSpec.Core.Exec.Config.final?`, `ConcreteSpec.Core.Exec.Step`). A
configuration's `Config.final?` reports `Final.trapped` kd site if and only if its control is
`Ctrl.trapped` kd site, and every trapped configuration is blocked in `Step`.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. `#od68` ends the program on a trapping deferred call;
`#od10` and `#od2` clause (5) forbid abort cleanup. -/
theorem step_trapped (P : Program O) (ctrl : Ctrl O) (kd : TrapKind) (site : SiteId)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    (Config.final? ⟨ctrl, L, st, k⟩ = some (.trapped kd site) ↔ ctrl = .trapped kd site) ∧
      Blocked (Step P) ⟨.trapped kd site, L, st, k⟩ := by
  refine ⟨⟨fun h => ?_, fun h => by subst h; rfl⟩, no_step_of_none_inv rfl⟩
  cases ctrl with
  | exec _ => cases h
  | done => cases k <;> cases h
  | exiting e => cases e <;> cases k <;> cases h
  | refused => cases h
  | trapped kd' site' => cases h; rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Steps`). After a trap nothing runs: a multi-step execution
from a trapped configuration is equivalent to emitting no events and remaining in that exact
trapped configuration.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. `#od68` ends the program on a trapping deferred call;
`#od10` and `#od2` clause (5) forbid abort cleanup. -/
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

/-- Law (subject `ConcreteSpec.Core.Exec.register`). Defer registration preserves the saved caller boundary.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. Caller restoration follows admitted `step`/`stepExit` and `register`;
`#decided-2026-11-13-lifecycle-refusal` requires successful admission at each lifecycle site. -/
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
    | functionEnd => simp [register] at h

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Each transition preserves the saved caller boundary or returns to it with caller locals restored.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. Caller restoration follows admitted `step`/`stepExit` and `register`;
`#decided-2026-11-13-lifecycle-refusal` requires successful admission at each lifecycle site. -/
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
            | exact ⟨[_, _, _], rfl⟩
            | exact ⟨[_, _], rfl⟩
            | exact ⟨[_], rfl⟩
            | exact ⟨[_, _, _], rfl⟩
            | exact ⟨[_, _], rfl⟩
            | exact .inl ⟨[], rfl⟩
            | exact .inl ⟨[_], rfl⟩
            | exact .inl ⟨[_, _, _], rfl⟩
            | exact .inl ⟨[_, _], rfl⟩
            | (rcases h with ⟨_, rfl⟩; exact .inl ⟨[_, _, _], rfl⟩)
        | done =>
          simp only [step, Option.map, bindLife] at h
          repeat' first | simp_all | split at h
          all_goals try simp only [Option.some.injEq, Out.next.injEq] at h
          all_goals try subst c'
          all_goals try contradiction
          all_goals repeat' first | simp_all [Protected] | split at h
          all_goals exact .inr rfl
        | exiting e =>
          cases e <;> simp only [step, stepExit, Option.map, bindLife] at h
          all_goals repeat' first | simp_all | split at h
          all_goals try simp only [Option.some.injEq, Out.next.injEq] at h
          all_goals try subst c'
          all_goals try contradiction
          all_goals repeat' first | simp_all [Protected, bindDst] | split at h
          all_goals exact .inr rfl
        | refused => simp [step] at h
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
            | exact ⟨_ :: _ :: _ :: f :: pre, rfl⟩
            | exact ⟨_ :: _ :: f :: pre, rfl⟩
            | exact .inl ⟨f :: pre, rfl⟩
            | exact .inl (register_protected _ caller q (f :: pre) _ (by assumption))
            | exact .inl ⟨_ :: f :: pre, rfl⟩
            | exact .inl ⟨_ :: _ :: _ :: f :: pre, rfl⟩
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
            simp only [step, stepExit, startCleanup, Option.map, bindLife] at h <;>
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
        | refused => simp [step] at h
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
          simp only [step, Option.map, bindLife] at h
          repeat' first | simp_all | split at h
        | cons f pre =>
          change step P ⟨.done, locals, st, f :: (pre ++ .ret none caller :: q)⟩ = _ at h
          cases f <;> simp only [step, onRes, startCleanup, Option.bind, Option.map] at h <;>
            repeat' first | simp_all | split at h
      | exiting e => exact False.elim (stepExit_no_request_inv locals st e _ _ _ _ _ _ h)
      | refused => simp [step] at h
      | trapped kd site => simp [step] at h
/-- A step count for a first-arrival execution, used only for induction on strict subpaths. -/
inductive FirstStepsN (R : Config O → Label O → Config O → Prop) (stop : Config O → Prop) :
    Config O → List O.Event → Config O → Nat → Prop where
  | stop {c} : stop c → FirstStepsN R stop c [] c 0
  | step {c l c' tr c'' n} : ¬ stop c → R c l c' → FirstStepsN R stop c' tr c'' n →
      FirstStepsN R stop c (l.events ++ tr) c'' (n + 1)

/-- Helper (inversion). Adds an induction index to the first-arrival certificate without changing its execution. -/
private theorem FirstSteps.sized_inv {R : Config O → Label O → Config O → Prop} {stop : Config O → Prop}
    {c c' : Config O} {tr : List O.Event} (h : FirstSteps R stop c tr c') :
    ∃ n, FirstStepsN R stop c tr c' n := by
  induction h with
  | stop hs => exact ⟨0, .stop hs⟩
  | step hn hs _ ih => obtain ⟨n, ih⟩ := ih; exact ⟨n + 1, .step hn hs ih⟩

/-- Helper (inversion). Erases only the induction index of a first-arrival certificate. -/
theorem FirstStepsN.unsized_inv {R : Config O → Label O → Config O → Prop} {stop : Config O → Prop}
    {c c' : Config O} {tr : List O.Event} {n : Nat} (h : FirstStepsN R stop c tr c' n) :
    FirstSteps R stop c tr c' := by
  induction h with
  | stop hs => exact .stop hs
  | step hn hs _ ih => exact .step hn hs ih

/-- Helper (inversion). List-length consequence of the saved-boundary predicate used in return segmentation. -/
private theorem protected_height_inv {caller : AList Var O.Val} {q : List (Frame O)}
    {c : Config O} (h : Protected caller q c) : q.length < c.frames.length := by
  obtain ⟨pre, he⟩ := h
  simp only [he, List.length_append, List.length_cons]
  omega

/-- Helper (inversion). Length contradiction excluding an already returned configuration while its saved frame remains. -/
private theorem protected_not_return_inv {caller : AList Var O.Val} {q : List (Frame O)}
    {c : Config O} (h : Protected caller q c) : ¬ (c.ctrl = .done ∧ c.frames = q) := by
  intro hr
  have hh := protected_height_inv h
  rw [hr.2] at hh
  exact Nat.lt_irrefl _ hh

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). A successful path crossing a saved caller boundary first returns with the saved caller locals.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. Caller restoration follows admitted `step`/`stepExit` and `register`;
`#decided-2026-11-13-lifecycle-refusal` requires successful admission at each lifecycle site. -/
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

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). A call either traps without changing its continuation, or saves the caller boundary.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. Caller restoration follows admitted `step`/`stepExit` and `register`;
`#decided-2026-11-13-lifecycle-refusal` requires successful admission at each lifecycle site. -/
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
      | exact ⟨[_, _], rfl⟩
      | exact ⟨[_], rfl⟩
      | (rcases h with ⟨_, rfl⟩; exact .inr ⟨[_, _], rfl⟩)
  | prim h _ _ =>
    simp only [step, Deferred.stmt, onRes, Option.bind, Option.map] at h
    repeat' first | simp_all | split at h

/-- Helper (inversion). Extracts the endpoint predicate from the first-arrival certificate. -/
private theorem FirstStepsN.end_stop_inv {P : Program O} {stop : Config O → Prop}
    {c c' : Config O} {tr : List O.Event} {n : Nat} (h : FirstStepsN (Step P) stop c tr c' n) : stop c' := by
  induction h with
  | stop hs => exact hs
  | step _ _ _ ih => exact ih

/-- Helper (inversion). Combines the retained trapped-run law with the endpoint bound; no additional trap semantics. -/
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

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Every successful deferred call path crossing its continuation first returns with its caller locals restored.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. Caller restoration follows admitted `step`/`stepExit` and `register`;
`#decided-2026-11-13-lifecycle-refusal` requires successful admission at each lifecycle site. -/
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

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Unwinding itself emits no event; effects belong to dispatched calls.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. Caller restoration follows admitted `step`/`stepExit` and `register`;
`#decided-2026-11-13-lifecycle-refusal` requires successful admission at each lifecycle site. -/
theorem exit_label {P : Program O} {e : Exit O.Val} {L : AList Var O.Val}
    {st : O.Store} {frames : List (Frame O)} :
    ExitSilent (Step P) e L st frames := by
  intro label c h
  cases h with
  | silent _ => rfl
  | prim h _ _ => exact False.elim (stepExit_no_request_inv L st e frames _ _ _ _ _ h)

/-- Helper (computation). Computes the resume dispatcher result to convert its representation inside raw-run completeness. -/
private theorem resume_step_inv {P : Program O} {e : Exit O.Val} {L : AList Var O.Val}
    {st : O.Store} {frames : List (Frame O)} {label : Label O} {c : Config O}
    (h : Step P ⟨.done, L, st, .resume (.exit e) :: frames⟩ label c) :
    label = .silent ∧ c = ⟨.exiting e, L, st, frames⟩ := by
  cases h with
  | silent h =>
    simp only [step, Option.some.injEq, Out.next.injEq] at h
    exact ⟨rfl, h.symm⟩
  | prim h _ _ => simp [step] at h

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Raw first-arrival cleanup executions admit a complete dispatch segmentation.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. `#od2` clause (5) / `#core-q9-8` require nested reverse cleanup;
`#od69` limits teardown to storage; `#decided-2026-11-13-lifecycle-refusal`
requires lifecycle admission. Function-end is a boundary, as in admitted `Passes`. -/
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
            by simpa [Label.events] using BoundaryRun.pass (by simp) (by cases f <;> simp_all [Passes]) hs hr⟩
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
        | functionEnd => exact False.elim hpf
        | resume p => exact False.elim hpf
        | ret dst caller => exact False.elim hpf

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Every raw first-boundary unwind through
passable frames has a segmented `BoundaryRun` observation that runs `planWith` pre L, restores
Lend = `unwindLocals` pre L, and performs every planned call and drop in order.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. `#od2` clause (5) / `#core-q9-8` require nested reverse cleanup;
`#od69` limits teardown to storage; `#decided-2026-11-13-lifecycle-refusal`
requires lifecycle admission. Function-end is a boundary, as in admitted `Passes`. -/
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
event trace and unchanged locals, store, and frame stack.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. `#od68` ends the program on a trapping deferred call;
`#od10` and `#od2` clause (5) forbid abort cleanup. -/
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
whatever follows it: once the arguments before it evaluate, in whatever state they leave.
Basis: admitted `Spec/Exec` definitions named in the subject and proof.
`docs/decisions.md#decided-2026-11-08-foundation`, C1 / `#foundation-q1`,
admits execution, call and cleanup laws in Lean. The first-trap result is a consequence of admitted `evalArgs`, not a new
evaluation-order decision. -/
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


/-- Representation lemma. Function injectivity on `step`. -/
private theorem step_silent_deterministic_inv {P : Program O} {c c₁ c₂ : Config O}
    (h₁ : step P c = some (.next c₁)) (h₂ : step P c = some (.next c₂)) : c₁ = c₂ := by
  rw [h₁] at h₂; cases h₂; rfl

/-- Representation lemma. Function/relation bridge, not determinism evidence. When the step function returns `Out.next` c₁, a
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

/-- Representation lemma. The function-backed representation of `Step` separates machine steps
from answers. -/
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

/-- Representation lemma. Constructor decomposition, not determinism evidence. Two relational transitions from c occur if and
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

/-- A dispatcher's answer-indexed observable transition. This sole adapter exposes the
operation as a parameter so correspondence laws and their planted dispatchers use exactly
the same transition conditions. `none` denotes an internal step; `some a` retains the
entire answer, even when its label is silent. -/
inductive Dispatches (dispatch : Program O → Config O → Option (Out O)) (P : Program O) :
    Config O → Option (Answer O.Val O.Event O.Store) → Label O → Config O → Prop where
  | silent {c c'} : dispatch P c = some (.next c') → Dispatches dispatch P c none .silent c'
  | prim {c c' p vs site st k a} : dispatch P c = some (.request p vs site st k) →
      O.allows p vs st a = true → k a = some c' → Dispatches dispatch P c (some a) a.label c'

/-- A transition with its external input retained. `none` denotes an internal machine
step; `some a` denotes precisely answer `a`, even when that answer emits no event. This
relation does not identify different answers merely because their observable labels coincide.
It specializes the shared dispatcher adapter without adding transition conditions. -/
abbrev AnswerStep := @Dispatches O step

/-- Representation lemma. Erases the input index to recover the observable transition. -/
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
results; this is not independent determinism evidence. -/
theorem step_silent_deterministic (P : Program O) (c c₁ : Config O) (l₁ : Label O)
    (h₁ : AnswerStep P c none l₁ c₁) :
    InputDeterministic AnswerStep P c c₁ none l₁ := by
  intro l₂ c₂ h₂
  cases h₁ with
  | silent h₁ =>
    cases h₂ with
    | silent h₂ => exact ⟨rfl, step_silent_deterministic_inv h₁ h₂⟩

/-- Representation lemma. Function-backed transitions at a fixed answer compare equal
dispatcher and continuation results; this is not soundness evidence. -/
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
have the same results; proved by list/result cases. -/
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
executable stack traversal. -/
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
and lifecycle adapters. -/
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
argument derivations and inversion of the other derivation. -/
theorem arguments_unique_inv {L : AList Var O.Val} {st : O.Store} {es : List O.Expr}
    {r r' : Res (List O.Val × O.Store)} (h : Arguments O L st es r)
    (h' : Arguments O L st es r') : r = r' := by
  induction h generalizing r' <;> cases h' <;> simp_all <;> grind

/-- Helper (congruence). Registration is unique by stack-rule inversion. -/
theorem registered_unique_inv {d : Deferred O} {k k₁ k₂ : List (Frame O)}
    (h : Registered d k k₁) (h' : Registered d k k₂) : k₁ = k₂ := by
  induction h generalizing k₂ <;> cases h' <;> simp_all <;> grind

/-- Helper (congruence). Destination binding is unique by admission-rule inversion. -/
theorem destination_unique_inv {dst : Option Var} {v : O.Val} {L L₁ L₂ : AList Var O.Val}
    {st st₁ st₂ : O.Store} (h : Destination O dst v L st L₁ st₁)
    (h' : Destination O dst v L st L₂ st₂) : L₁ = L₂ ∧ st₁ = st₂ := by
  cases h <;> cases h' <;> simp_all

/-- Helper (inversion). Translates each independent operational constructor into the
function-backed representation; no uniqueness argument uses this lemma. -/
theorem rules_answer_sound_inv {P : Program O} {c c' : Config O}
    {a : Option (Answer O.Val O.Event O.Store)} {l : Label O}
    (h : Rules P c a l c') : AnswerStep P c a l c' := by
  cases h
  case primOk L st k dst p args site vs st₁ st₂ a v L' he ha hv hd =>
    obtain ⟨hb, rfl⟩ := (destination_representation _ _ _ _ _ _).mp hd
    apply Dispatches.prim (by simp only [step, onRes, (arguments_representation _ _ _ _).mp he]; rfl) ha
    simp [hv, hb]
  case primError L st k dst p args site vs st₁ a kd he ha hv =>
    apply Dispatches.prim (by simp only [step, onRes, (arguments_representation _ _ _ _).mp he]; rfl) ha
    simp [hv]
  all_goals
    apply Dispatches.silent
    try simp_all only [arguments_representation, registered_representation, destination_representation]
    simp_all only [step, stepExit, onRes, startCleanup, Deferred.stmt, dropOut, dead, dom,
      restrict, bindDst, bindLife]
    all_goals simp_all

/-- Law (subject `ConcreteSpec.Core.Exec.Rules`). A fixed external input determines both
label and destination by operational-rule inversion, independently of the dispatcher.
Authority: `docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests`:
determinism over independently written rules, admitted as inductive law subjects.
Foundation `#foundation-q1` supplies the interface/law boundary; the November 13
`#decided-2026-11-13-proof-labels` record supplies only the tag vocabulary. -/
theorem rules_deterministic (P : Program O) (c c₁ : Config O)
    (a : Option (Answer O.Val O.Event O.Store)) (l₁ : Label O)
    (h₁ : Rules P c a l₁ c₁) : InputDeterministic Rules P c c₁ a l₁ := by
  intro l₂ c₂ h₂
  cases h₁ <;> cases h₂ <;>
    first | exact ⟨rfl, rfl⟩ | (expose_names; simp_all)
  all_goals grind only [arguments_unique_inv, registered_unique_inv, destination_unique_inv]

/-- Helper (inversion). Statement completeness, with explicit operand and admission cases
rather than a product of all dispatcher branches. -/
private theorem rules_exec_complete_inv {P : Program O} (s : Stmt O)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) {c' : Config O}
    (hs : step P ⟨.exec s, L, st, k⟩ = some (.next c')) :
    Rules P ⟨.exec s, L, st, k⟩ none .silent c' := by
  cases s with
  | skip => invert_step hs; exact .skip
  | seq s t => invert_step hs; exact .seq
  | block b s => invert_step hs; exact .block
  | loop l cr test body site => invert_step hs; exact .loop
  | brk l => invert_step hs; exact .brk
  | cont l => invert_step hs; exact .cont
  | abort site => invert_step hs; exact .abort
  | defer f args site =>
    crush_option [step] at hs
    invert_step hs
    exact .defer ((registered_representation _ _ _).mpr ‹_›)
  | bind x e site =>
    crush_option [step, onRes] at hs
    · invert_step hs; exact .bind ‹_› ‹_›
    · invert_step hs; exact .bindTrap ‹_›
  | assign x e site =>
    crush_option [step, onRes] at hs
    · invert_step hs; exact .assign ‹_› ‹_› ‹_›
    · invert_step hs; exact .assignTrap ‹_›
  | cond e yes no site =>
    crush_option [step, onRes] at hs
    · invert_step hs; exact .condtrue ‹_› ‹_›
    · invert_step hs
      exact .condfalse ‹_› (by simp only [Bool.eq_false_iff.mpr ‹_›] at *; assumption)
    · invert_step hs; exact .condTrap ‹_›
  | ret e site =>
    crush_option [step, onRes] at hs
    · invert_step hs; exact .ret ‹_›
    · invert_step hs; exact .retTrap ‹_›
  | call dst f args site =>
    crush_option [step, onRes] at hs
    · invert_step hs; exact .call ((arguments_representation _ _ _ _).mpr ‹_›) ‹_› ‹_› ‹_›
    · invert_step hs; exact .callTrap ((arguments_representation _ _ _ _).mpr ‹_›)
  | prim dst p args site =>
    crush_option [step, onRes] at hs
    invert_step hs
    exact .primTrap ((arguments_representation _ _ _ _).mpr ‹_›)

/-- Helper (inversion). Normal-completion completeness by frame and operand-result cases. -/
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
    | functionEnd =>
      obtain ⟨st₁, hl, h⟩ := Option.map_eq_some_iff.mp hs
      cases h; exact .endFall hl
    | ret dst caller =>
      simp only [step, Option.map_eq_some_iff] at hs
      obtain ⟨st₂, hb, h⟩ := hs
      cases h
      exact .implicitReturn ((destination_representation _ _ _ _ _ _).mpr ⟨hb, rfl⟩)

/-- Helper (inversion). Exit completeness by frame and, where inspected, exit kind. -/
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
    | functionEnd =>
      cases e with
      | ret v =>
        obtain ⟨st₁, hl, h⟩ := Option.map_eq_some_iff.mp hs
        cases h; exact .endReturn hl
      | brk l => simp [step, stepExit] at hs
      | cont l => simp [step, stepExit] at hs
    | ret dst caller =>
      cases e with
      | brk l => simp [step, stepExit] at hs
      | cont l => simp [step, stepExit] at hs
      | ret v =>
        simp only [step, stepExit, Option.map_eq_some_iff] at hs
        obtain ⟨st₂, hb, h⟩ := hs
        cases h
        exact .explicitReturn ((destination_representation _ _ _ _ _ _).mpr ⟨hb, rfl⟩)

/-- Helper (inversion). Silent completeness reduces to statement and frame inversion. -/
theorem rules_silent_complete_inv {P : Program O} {c c' : Config O}
    (hs : step P c = some (.next c')) : Rules P c none .silent c' := by
  obtain ⟨ctrl, L, st, k⟩ := c
  cases ctrl with
  | exec s => exact rules_exec_complete_inv s L st k hs
  | done => exact rules_done_complete_inv L st k hs
  | exiting e => exact rules_exit_complete_inv e L st k hs
  | trapped kd site => simp [step] at hs
  | refused => simp [step] at hs

/-- Helper (inversion). Every successful request continuation is a primitive operational
transition; other statement and frame forms cannot request. -/
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
  | refused => simp [step] at hs
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

/-- Law (subject `ConcreteSpec.Core.Exec.step`). Every independent operational transition
is implemented by the dispatcher, including the exact answer, label and destination.
Authority: `docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests`:
the independent rules and executable step must agree in both directions under the
same external answer. Foundation `#foundation-q1` supplies the interface/law boundary;
`#decided-2026-11-13-proof-labels` supplies only the tag vocabulary. -/
theorem rules_sound (P : Program O) (c : Config O)
    (a : Option (Answer O.Val O.Event O.Store)) (l : Label O) (c' : Config O)
    (h : Rules P c a l c') : Dispatches step P c a l c' := by
  exact rules_answer_sound_inv h

/-- Completeness at a fixed source and input: every implementation outcome has an
independent operational derivation. Quantifying outcomes inside the property permits a
premise-preserving mutant to exhibit an extra implementation transition. -/
def DispatchComplete (dispatch : Program O → Config O → Option (Out O))
    (P : Program O) (c : Config O) (a : Option (Answer O.Val O.Event O.Store)) : Prop :=
  ∀ l c', Dispatches dispatch P c a l c' → Rules P c a l c'

/-- Law (subject `ConcreteSpec.Core.Exec.step`). Every admitted dispatcher transition has
an independent operational derivation; no executable transition is omitted.
Authority: `docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests`:
the independent rules and executable step must agree in both directions under the
same external answer. Foundation `#foundation-q1` supplies the interface/law boundary;
`#decided-2026-11-13-proof-labels` supplies only the tag vocabulary. -/
theorem rules_complete (P : Program O) (c : Config O)
    (a : Option (Answer O.Val O.Event O.Store)) : DispatchComplete step P c a := by
  intro l c' h
  cases h with
  | silent hs => exact rules_silent_complete_inv hs
  | prim hs ha hk => exact rules_request_complete_inv hs ha hk

/-- Representation lemma. Exact correspondence with the answer-retaining relation follows
from dispatcher soundness and completeness. -/
theorem rules_iff_answerStep (P : Program O) (c : Config O)
    (a : Option (Answer O.Val O.Event O.Store)) (l : Label O) (c' : Config O) :
    Rules P c a l c' ↔ AnswerStep P c a l c' := by
  constructor
  · exact rules_answer_sound_inv
  · exact rules_complete P c a l c'

/-- Representation lemma. Erasing exactly the answer index gives the original observable
single-step relation. -/
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
configuration, by independent rule inversion.
Authority: `docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests`:
determinism over independently written rules, admitted as inductive law subjects.
Foundation `#foundation-q1` supplies the interface/law boundary; the November 13
`#decided-2026-11-13-proof-labels` record supplies only the tag vocabulary. -/
theorem rules_silent_deterministic (P : Program O) (c c₁ : Config O) (l₁ : Label O)
    (h₁ : Rules P c none l₁ c₁) : InputDeterministic Rules P c c₁ none l₁ := by
  exact rules_deterministic P c c₁ none l₁ h₁


/-! ## E3 controls

The fixed-input index is retained even for silent primitive answers. These controls use
populated locals, a nonzero counter, two activations and a continuation. The overlapping
mutant adds only a second skip conclusion; it is substituted in both occurrences of the
relation in the determinism obligation. Agreement controls separately delete and add an
executable transition. There is no derived closure in these one-step obligations.
Authority: `docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests` admits
inductive law subjects and requires same-shape mutants breaking one specific rule,
substituted everywhere that rule occurs in the law. Lean checks concrete examples
where the premises hold and the conclusion fails; errors and timeouts are not evidence.
The November 13 `#decided-2026-11-13-proof-labels` record supplies only tag vocabulary.
No held path, constant, Boolean representation, pointer or startup policy is introduced here.
The 2026-11-21 decision
(docs/decisions.md#decided-2026-11-21-secrets-after-1-0) defers wiping until after 1.0
and closes ISS-0072.
-/


/-! ## F10 / E4: lifecycle refusal and explicit destruction

Authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal,
#decided-2026-11-14-trap-sites-and-law-tests and #decided-2026-11-13-proof-labels.
The legacy E4 activation/entry interfaces are replaced by main's functionEnd and
unsafe-stuck entry reports. No alternate executor or lookup is introduced.
-/

/-- Helper (inversion). For a silent lifecycle operation, absence of all transitions is exactly
refusal. -/
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
successful operand evaluation: refusal is necessary and sufficient for the absence of every labeled
transition. Authority: `docs/decisions.md#foundation-q1`, `#decided-2026-11-13-lifecycle-refusal`.
-/
theorem bind_refused (P : Program O) (x : Var) (e : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st st' : O.Store) (v : O.Val) (k : List (Frame O))
    (he : O.eval L st e = .ok (v, st')) :
    Blocked (Step P) ⟨.exec (.bind x e site), L, st, k⟩ ↔ O.life (.bind x) st' = none :=
  blocked_map_inv _ (fun st => ⟨.done, (x, v) :: L, st, k⟩) (by simp [step, onRes, he])

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). For an existing destination and successful RHS,
only lifecycle refusal blocks assignment. Authority: `docs/decisions.md#foundation-q1`,
`#decided-2026-11-13-lifecycle-refusal`. -/
theorem assign_refused (P : Program O) (x : Var) (e : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st st' : O.Store) (v old : O.Val) (k : List (Frame O))
    (he : O.eval L st e = .ok (v, st')) (hx : AList.lookup L x = some old) :
    Blocked (Step P) ⟨.exec (.assign x e site), L, st, k⟩ ↔ O.life (.bind x) st' = none :=
  blocked_map_inv _ (fun st => ⟨.done, AList.update L x (fun _ => v), st, k⟩)
    (by simp [step, onRes, he, hx])

/-- Law (subject `ConcreteSpec.Core.Exec.enter`). Entry refuses exactly when
activation admission refuses. The reporting wrapper retains this as unsafe stuckness. Authority:
`docs/decisions.md#decided-2026-11-13-lifecycle-refusal`. -/
theorem enter_refused (fn : Fn O) (vs : List O.Val) (st : O.Store) :
    enter fn vs st = none ↔ O.life (.enter (fn.params.map Prod.fst)) st = none := by
  cases h : O.life (.enter (fn.params.map Prod.fst)) st <;> simp [enter, h]

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). After successful argument evaluation, function
lookup, and arity check (fn.params.length = vs.length, which `step` verifies before calling
`enter`), activation refusal is necessary and sufficient to block a call. Authority:
`docs/decisions.md#foundation-q1`, `#decided-2026-11-13-lifecycle-refusal`. -/
theorem call_refused (P : Program O) (dst : Option Var) (f : FnId) (args : List O.Expr)
    (site : SiteId) (L : AList Var O.Val) (st st' : O.Store) (vs : List O.Val)
    (fn : Fn O) (k : List (Frame O)) (he : evalArgs O L st args = .ok (vs, st'))
    (hf : AList.lookup P.fns f = some fn) (hlen : fn.params.length = vs.length) :
    Blocked (Step P) ⟨.exec (.call dst f args site), L, st, k⟩ ↔
      O.life (.enter (fn.params.map Prod.fst)) st' = none :=
  blocked_map_inv _ (fun st => ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, st,
    .scope fn.block (fn.params.map Prod.fst) [] :: .functionEnd :: .ret dst L :: k⟩)
    (by simp [step, onRes, he, hf, hlen])

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). A normal scope end with no remaining calls is
blocked if and only if dropping its locals refuses. Authority: `docs/decisions.md#foundation-q1`,
`#decided-2026-11-13-lifecycle-refusal`. -/
theorem scope_drop_refused (P : Program O) (b : BlockId) (dm : List Var)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    Blocked (Step P) ⟨.done, L, st, .scope b dm [] :: k⟩ ↔ dropOut O dm L st = none :=
  blocked_map_inv _ _ rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). A structured exit through an empty scope is blocked
if and only if its drop refuses. Authority: `docs/decisions.md#foundation-q1`,
`#decided-2026-11-13-lifecycle-refusal`. -/
theorem exit_drop_refused (P : Program O) (e : Exit O.Val) (b : BlockId) (dm : List Var)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    Blocked (Step P) ⟨.exiting e, L, st, .scope b dm [] :: k⟩ ↔ dropOut O dm L st = none :=
  blocked_map_inv _ _ rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). The test-frame drop during an exit is necessary and
sufficient for progress. Authority: `docs/decisions.md#foundation-q1`,
`#decided-2026-11-13-lifecycle-refusal`. -/
theorem exit_test_drop_refused (P : Program O) (e : Exit O.Val) (dm : List Var)
    (l : LoopId) (cr body : Stmt O) (t : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    Blocked (Step P) ⟨.exiting e, L, st, .test l cr t body site dm :: k⟩ ↔ dropOut O dm L st = none :=
  blocked_map_inv _ _ rfl

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). After a successful boolean test, either branch
progresses if and only if its temporary drop succeeds. Authority: `docs/decisions.md#foundation-q1`,
`#decided-2026-11-13-lifecycle-refusal`. -/
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
blocked if and only if result initialization refuses; activation end was admitted at the preceding
functionEnd frame. Authority: `docs/decisions.md#foundation-q1`,
`#decided-2026-11-13-lifecycle-refusal`. -/
theorem return_refused (P : Program O) (dst : Option Var) (v : O.Val)
    (caller L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    (Blocked (Step P) ⟨.exiting (.ret v), L, st, .ret dst caller :: k⟩ ↔
      bindLife O dst st = none) ∧
    (Blocked (Step P) ⟨.done, L, st, .ret dst caller :: k⟩ ↔
      bindLife O dst st = none) := by
  constructor
  · apply blocked_map_inv _ (fun st => ⟨.done, bindDst dst v caller, st, k⟩)
    simp [step, stepExit]
  · apply blocked_map_inv _ (fun st => ⟨.done, bindDst dst O.unitVal caller, st, k⟩)
    simp [step]

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Both normal and explicit-return activation
boundaries stop exactly on refused leave; neither reaches finality first. Authority:
`docs/decisions.md#decided-2026-11-13-lifecycle-refusal`. -/
theorem leave_refused (P : Program O) (v : O.Val) (L : AList Var O.Val)
    (st : O.Store) (k : List (Frame O)) :
    (Blocked (Step P) ⟨.done, L, st, .functionEnd :: k⟩ ↔ O.life .leave st = none) ∧
    (Blocked (Step P) ⟨.exiting (.ret v), L, st, .functionEnd :: k⟩ ↔
      O.life .leave st = none) := by
  constructor
  · exact blocked_map_inv _ _ rfl
  · exact blocked_map_inv _ _ rfl

/-- Law (subject `ConcreteSpec.Core.Exec.runFn`). Entry refusal preserves the proposed locals,
original store and entire budget in an unsafe, unresumable report. This is a semantic boundary
report, never a rollback claim about outside effects. Authority:
`docs/decisions.md#decided-2026-11-13-lifecycle-refusal` (unsafe stopping) and
`#decided-2026-11-05-core` Q9.9 (total clock-indexed instrumented evaluation).
The exact residual fields and refusal-before-fuel precedence are the proposed reporting
convention in `Spec/Exec.lean` (`runFn`/`runFrom`), tracked by ISS-0049; the lifecycle
record explicitly leaves entry versus mid-run reporting undecided. -/
theorem entry_refusal_report (P : Program O) (ρ : Oracle O) (fuel : Nat) (fn : Fn O)
    (vs : List O.Val) (st : O.Store) (hlen : fn.params.length = vs.length)
    (hl : O.life (.enter (fn.params.map Prod.fst)) st = none) :
    runFn P ρ fuel fn vs st = some ⟨[], .stuck,
      ⟨.refused, (fn.params.map Prod.fst).zip vs, st,
        [.scope fn.block (fn.params.map Prod.fst) [], .functionEnd]⟩, fuel⟩ := by
  simp [runFn, hlen, enter, hl, run, runFrom]

/-- Law (subject `ConcreteSpec.Core.Exec.runFrom`). With an allowed answer, the entire request
boundary report (including unused fuel) is retained exactly when its continuation refuses. Events
already performed outside the semantics are not rolled back. Authority:
`docs/decisions.md#decided-2026-11-13-lifecycle-refusal`, `#decided-2026-11-05-core` Q9.7 and Q9.9.
-/
theorem continuation_refused (P : Program O) (ρ : Oracle O)
    (hist : List O.Event) (c : Config O) (p : O.Prim) (vs : List O.Val)
    (site : SiteId) (st : O.Store) (cont : Answer O.Val O.Event O.Store → Option (Config O))
    (a : Answer O.Val O.Event O.Store) (hf : c.final? = none)
    (hc : c.ctrl ≠ .refused) (hr : step P c = some (.request p vs site st cont))
    (ho : ρ hist p vs site st = some a) (ha : O.allows p vs st a = true) :
    runFrom P ρ 1 hist c = ⟨hist, .stuck, c, 1⟩ ↔ cont a = none := by
  cases hk : cont a with
  | none => cases hctrl : c.ctrl <;> simp_all [runFrom]
  | some c' =>
    constructor
    · intro h
      have hz : (runFrom P ρ 0 (hist ++ a.event.toList) c').fuel = 0 := by
        cases hctrl : c'.ctrl <;> cases hfinal : c'.final? <;> simp [runFrom, hctrl, hfinal]
      have he : runFrom P ρ 1 hist c = runFrom P ρ 0 (hist ++ a.event.toList) c' := by
        cases hctrl : c.ctrl <;> simp_all [runFrom]
      rw [he] at h
      have := congrArg Report.fuel h
      rw [hz] at this
      contradiction
    · intro h; cases h

/-- Law (subject `ConcreteSpec.Core.Exec.Step`). Relational primitive-bind refusal: after successful
argument evaluation, a primitive call with destination `some` x is blocked in `Step` if and only if
every allowed answer succeeds with a value whose `Life.bind` x lifecycle hook refuses. Authority:
`docs/decisions.md#foundation-q1`, `#decided-2026-11-13-lifecycle-refusal`;
`#decided-2026-11-05-core` Q9.7. This is the admitted primitive contract, not destructor policy. -/
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
when that initialization would refuse: the actual labeled transition traps, retaining the answer's
store and event. Authority: `docs/decisions.md#foundation-q1`,
`#decided-2026-11-13-lifecycle-refusal`; `#decided-2026-11-05-core` Q9.7. This is the admitted
primitive contract, not destructor policy. -/
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

/-- Helper (projection). Extracts the protocol admission obligation from `DestroyProtocol`. -/
private theorem destroy_not_copy_proj {P : Program O} (D : DestroyProtocol P) (T : O.Ty) (f : FnId)
    (h : D.impl T = some f) : O.isCopy T = false := D.notCopy T f h

/-- Representation lemma. Characterizes the validator checks, not admission soundness. The executable
destroy-function validator accepts (T, f) if and only if T is non-Copy (`Operands.isCopy` T = false)
and f is in the program table taking a single parameter of type T and returning `Operands.unitTy`.
-/
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

/-- Representation lemma. Characterizes the checked builder, not admission soundness. The executable
checked destroy-call builder produces `some` d if and only if impl T = `some` (`Deferred.f` d),
`Deferred.args` d = [arg], `Deferred.site` d = site, T is non-Copy, and `Deferred.f` d has signature
(x : T) -> Unit in `Program.fns` P. -/
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

/-- Law (subject `ConcreteSpec.Core.Exec.Step`,
`ConcreteSpec.Core.Exec.DestroyProtocol.destroyCall`). An independently admitted consuming
destructor enters its ordinary singleton-parameter body through the actual call semantics. Argument
evaluation may move the value; its resulting store is threaded through activation entry. Admission,
rather than the executable validator, supplies the non-Copy and Unit-result facts. A refused
activation is covered separately by `call_refused`. Lifecycle authority:
`docs/decisions.md#decided-2026-11-13-lifecycle-refusal`. Authority:
`docs/decisions.md#foundation-q1`, `#decided-2026-11-05-core` Q2.3, Q2.7–Q2.8; `#od60`;
`#decided-2026-11-08-language-revisions`. -/
theorem destroyCall_enters (P : Program O) (D : DestroyProtocol P) (T : O.Ty)
    (f : FnId) (fn : Fn O) (x : Var) (arg : O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st st' st'' : O.Store) (v : O.Val) (k : List (Frame O))
    (hi : D.impl T = some f) (hf : AList.lookup P.fns f = some fn)
    (hp : fn.params = [(x, T)]) (he : O.eval L st arg = .ok (v, st'))
    (hl : O.life (.enter [x]) st' = some st'') :
    ∃ d, D.destroyCall T arg site = some d ∧ fn.result = O.unitTy ∧ O.isCopy T = false ∧
      Step P ⟨.exec d.stmt, L, st, k⟩ .silent
        ⟨.exec fn.body, [(x, v)], st'', .scope fn.block [x] [] :: .functionEnd :: .ret none L :: k⟩ := by
  obtain ⟨admitted, _, hfn, _, hr⟩ := D.sig T f hi
  rw [hf] at hfn
  cases hfn
  refine ⟨⟨f, [arg], site⟩, ?_, hr, destroy_not_copy_proj D T f hi, ?_⟩
  · simp [DestroyProtocol.destroyCall, hi]
  · apply Step.silent
    simp [Deferred.stmt, step, evalArgs, he, Res.bind, Res.map, onRes, hf, hp, hl]

/-- Law (subject `ConcreteSpec.Core.Exec.seqDestroyOrder`). A sequence's element i is visited at
position i: in increasing index order. Authority:
`docs/decisions.md#decided-2026-11-05-core` Q2.3 and Q3.8. -/
theorem seqDestroyOrder_elem (n i : Nat) (h : i < n) : (seqDestroyOrder n)[i]? = some (.elem i) := by
  rw [seqDestroyOrder, List.getElem?_append_left (by simpa using h)]
  simp [h]

/-- Law (subject `ConcreteSpec.Core.Exec.seqDestroyOrder`). A sequence's backing is released last,
after all n elements. Authority: `docs/decisions.md#decided-2026-11-05-core` Q2.3 and Q3.8. -/
theorem seqDestroyOrder_storage (n : Nat) :
    (seqDestroyOrder n).length = n + 1 ∧ (seqDestroyOrder n)[n]? = some .storage := by
  simp [seqDestroyOrder]

/-- The entries of a map, in visiting order. -/
private def entryParts (n : Nat) : List Part := (List.range n).flatMap fun i => [.key i, .value i]

/-- Helper (arithmetic). Appending an entry adds its key and value visits. -/
private theorem entryParts_succ (n : Nat) :
    entryParts (n + 1) = entryParts n ++ [.key n, .value n] := by
  simp [entryParts, List.range_succ, List.flatMap_append]

/-- Helper (arithmetic). Each map entry contributes two visits. -/
private theorem entryParts_length : ∀ n, (entryParts n).length = 2 * n
  | 0 => rfl
  | n + 1 => by rw [entryParts_succ]; simp [entryParts_length n]; omega

/-- Helper (arithmetic). Locate the key and value in the alternating entry schedule. -/
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
order) is visited key first, at position 2i, then value, at 2i+1. Authority:
`docs/decisions.md#decided-2026-11-05-core` Q2.3 and Q3.8. -/
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
all n entries. Authority: `docs/decisions.md#decided-2026-11-05-core` Q2.3 and Q3.8. -/
theorem mapDestroyOrder_storage (n : Nat) :
    (mapDestroyOrder n).length = 2 * n + 1 ∧ (mapDestroyOrder n)[2 * n]? = some .storage := by
  have hl := entryParts_length n
  show (entryParts n ++ [Part.storage]).length = _ ∧ (entryParts n ++ [Part.storage])[2 * n]? = _
  rw [List.getElem?_append_right (by omega), hl]
  simp [hl]


/-- Law (subject `ConcreteSpec.Core.Exec.runFrom`). Successful primitive answers bind their result
through lifecycle admission; refusal retains the requesting configuration and budget. Authority:
`docs/decisions.md#decided-2026-11-13-lifecycle-refusal`, `#decided-2026-11-05-core` Q9.7. -/
theorem prim_bind_refused (P : Program O) (ρ : Oracle O) (hist : List O.Event)
    (x : Var) (p : O.Prim) (args : List O.Expr) (site : SiteId)
    (L : AList Var O.Val) (st st' : O.Store) (vs : List O.Val) (k : List (Frame O))
    (a : Answer O.Val O.Event O.Store) (v : O.Val)
    (he : evalArgs O L st args = .ok (vs, st'))
    (ho : ρ hist p vs site st' = some a) (ha : O.allows p vs st' a = true)
    (hv : a.result = .ok v) :
    runFrom P ρ 1 hist ⟨.exec (.prim (some x) p args site), L, st, k⟩ =
      ⟨hist, .stuck, ⟨.exec (.prim (some x) p args site), L, st, k⟩, 1⟩ ↔
      O.life (.bind x) a.store = none := by
  rw [continuation_refused P ρ hist _ p vs site st' _ a rfl (by intro h; cases h)
    (by simp [step, onRes, he]; rfl) ho ha]
  simp [hv, bindLife]

/-- Law (subject `ConcreteSpec.Core.Exec.runFrom`). Explicit refusal is an unsafe stop, even with
zero fuel and pending cleanup. Neither a final result nor a trap is synthesized. Authority:
`docs/decisions.md#decided-2026-11-13-lifecycle-refusal` (unsafe stopping) and
`#decided-2026-11-05-core` Q9.9 (total clock-indexed instrumented evaluation).
The exact residual fields and refusal-before-fuel precedence are the proposed reporting
convention in `Spec/Exec.lean` (`runFn`/`runFrom`), tracked by ISS-0049; the lifecycle
record explicitly leaves entry versus mid-run reporting undecided. -/
theorem refused_stops (P : Program O) (ρ : Oracle O) (fuel : Nat) (hist : List O.Event)
    (L : AList Var O.Val) (st : O.Store) (k : List (Frame O)) :
    runFrom P ρ fuel hist ⟨.refused, L, st, k⟩ =
      ⟨hist, .stuck, ⟨.refused, L, st, k⟩, fuel⟩ := by
  cases fuel <;> rfl





end ConcreteSpec.Core.Exec
