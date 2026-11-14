import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Scalar

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
representation (ISS-0070); SPARK char preconditions (ISS-0071); wiping (ISS-0072);
startup and ABI (ISS-0073). The demo interfaces do not settle those questions.
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
    | functionEnd => exact False.elim (hbound rfl)
    | resume p => simp [step, stepExit] at next
    | ret dst caller =>
      cases e <;> simp [step, stepExit] at next



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
private theorem no_step_of_none_inv {P : Program O} {c : Config O} (h : step P c = none) :
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
private theorem FirstStepsN.unsized_inv {R : Config O → Label O → Config O → Prop} {stop : Config O → Prop}
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


namespace CleanupEvidence
open Demo

/-- Representation lemma. A concrete first-return segment for an ordinary deferred call; unlike E1’s
universal completion helper, this additionally certifies first arrival. -/
private theorem logSegment (n : Int) (e : Exit Int) (L : AList Var Int)
    (rest : List (Frame ops)) (st : DState) :
    FirstSteps (Step prog) (fun c => c.ctrl = .done ∧ c.frames = .resume (.exit e) :: rest)
      ⟨.exec (⟨logF, [.lit n], s⟩ : Deferred ops).stmt, L, st, .resume (.exit e) :: rest⟩ [n]
      ⟨.done, L, st, .resume (.exit e) :: rest⟩ := by
  let q : List (Frame ops) := .resume (.exit e) :: rest
  let entered : DState := DState.life (.enter [x]) st
  refine FirstSteps.step (R := Step prog) (c' := ⟨.exec logFn.body, [(x, n)], entered,
      .scope logFn.block [x] [] :: .functionEnd :: .ret none L :: q⟩)
    (by intro h; cases h.1) (.silent rfl) ?_
  refine FirstSteps.step (R := Step prog) (c' := ⟨.done, [(x, n)], entered,
      .scope logFn.block [x] [] :: .functionEnd :: .ret none L :: q⟩)
    (by intro h; cases h.1)
    (Step.prim (P := prog) (p := .log) (vs := [n]) (site := s) (st := entered) (a := ⟨some n, .ok 0, entered⟩) rfl (by simp [ops]) rfl) ?_
  refine FirstSteps.step (R := Step prog) (c' := ⟨.done, [(x, n)], entered, .functionEnd :: .ret none L :: q⟩)
    (by intro h; cases h.2) (.silent rfl) ?_
  refine FirstSteps.step (R := Step prog) (c' := ⟨.done, [(x, n)], st, .ret none L :: q⟩)
    (by intro h; cases h.2) (.silent rfl) ?_
  refine FirstSteps.step (R := Step prog) (c' := ⟨.done, L, st, q⟩)
    (by intro h; cases h.2) (.silent rfl) ?_
  exact .stop ⟨rfl, rfl⟩

/-- Populated caller state. `x` is local to the inner scope; `y` survives until the outer
scope. Unrelated moved entries and a second activation expose accidental state replacement. -/
def live : AList Var Int := [(x, 7), (y, 8)]
def initial : DState := ⟨4, [[x, p], [t]]⟩
def middle : DState := ⟨4, [[p], [t]]⟩
def suffix : List (Frame ops) := [.seq (.abort s)]
def d (n : Int) : Deferred ops := ⟨logF, [.lit n], s⟩
def inner : Frame ops := .scope ⟨1⟩ [y] [d 1]
def outer : Frame ops := .scope ⟨2⟩ [] [d 2]
def emptyScope : Frame ops := .scope ⟨1⟩ [y] []
def actions : List (CleanAct ops) :=
  [.call live (d 1), .drop [y] live [(y, 8)],
   .call [(y, 8)] (d 2), .drop [] [(y, 8)] []]

/-- Helper. Both nested scope frames pass a return. -/
private theorem nested_passes : ∀ f ∈ [inner, outer], Passes (O := ops) (.ret 7) f := by
  intro f hf
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hf
  rcases hf with rfl | rfl <;> trivial

/-- Helper. The populated empty scope passes a return. -/
private theorem empty_passes : ∀ f ∈ [emptyScope], Passes (O := ops) (.ret 7) f := by
  intro f hf
  simp only [List.mem_singleton] at hf
  subst f
  trivial

/-- Helper. Independent execution of the nested calls and their distinct storage drops.
The trace changes on reversal, and the second call sees the restricted local environment. -/
private theorem performed : Performed prog actions live initial [1, 2] [] middle := by
  exact .call (logSegment 1 (.ret 7) live [] initial)
    (.drop rfl (.call (logSegment 2 (.ret 7) [(y, 8)] [] middle) (.drop rfl .nil)))

/-- Helper (application of `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`). Two registered calls,
both local restorations and the lifecycle update are performed before the saved suffix. -/
example : BoundaryRun prog (Step prog) (.ret 7) suffix [inner, outer]
    live initial actions [1, 2] [] middle :=
  (cleanup_runs_exact_plan prog (.ret 7) suffix [inner, outer] live [] initial middle
    actions [1, 2]).mpr ⟨nested_passes, rfl, rfl, performed⟩

/-- Helper (application of `ConcreteSpec.Core.Exec.planWith_calls`). Projection retains call order,
while removing only storage actions. -/
example : (planWith [inner, outer] live).filterMap CleanAct.call? = plan [inner, outer] :=
  planWith_calls [inner, outer] live

/-- Planted. Reverse the action plan, preserving every action. -/
def reversePlanWith (pre : List (Frame O)) (L : AList Var O.Val) := (planWith pre L).reverse
/-- Planted. Omit calls from the call-only projection. -/
def emptyPlan (_ : List (Frame O)) : List (Deferred O) := []
/-- Planted (certificate `ConcreteSpec.Core.Exec.planWith_calls`). Reversal changes the two calls. -/
example : (reversePlanWith [inner, outer] live).filterMap CleanAct.call? ≠ plan [inner, outer] := by
  intro h; cases h
/-- Planted (certificate `ConcreteSpec.Core.Exec.planWith_calls`). Omitting calls changes projection. -/
example : (planWith [inner, outer] live).filterMap CleanAct.call? ≠ emptyPlan [inner, outer] := by
  intro h; cases h

/-- Planted. Omit only dispatch of a pending call; retain every other production transition. -/
inductive NoDispatchStep (P : Program O) : Config O → Label O → Config O → Prop where
  | retain {c l c'} : Step P c l c' → (match c.ctrl, c.frames with
      | .exiting _, .scope b _ (_ :: _) :: _ => if b = ⟨1⟩ then False else True
      | _, _ => True) → NoDispatchStep P c l c'

/-- Helper. The omitted-dispatch relation cannot start the populated nested unwind. -/
private theorem no_dispatch : ¬ BoundaryRun prog (NoDispatchStep prog) (.ret 7) suffix
    [inner, outer] live initial actions [1, 2] [] middle := by
  intro h
  generalize ha : actions = acts at h
  generalize ht : ([1, 2] : List Int) = tr at h
  cases h with
  | pass _ _ hs _ => cases hs with | retain _ impossible => exact impossible
  | call _ hs _ _ _ => cases hs with | retain _ impossible => exact impossible

/-- Planted (certificate `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`). All independent actions
succeed, but omission of the first dispatch falsifies the equivalence at those same arguments. -/
example : ¬ (BoundaryRun prog (NoDispatchStep prog) (.ret 7) suffix [inner, outer]
      live initial actions [1, 2] [] middle ↔
    (∀ f ∈ [inner, outer], Passes (O := ops) (.ret 7) f) ∧
      actions = planWith [inner, outer] live ∧ [] = unwindLocals [inner, outer] live ∧
      Performed prog actions live initial [1, 2] [] middle) := by
  intro h
  exact no_dispatch (h.mpr ⟨nested_passes, rfl, rfl, performed⟩)

/-- Planted. Remove just the storage callback, retaining the local restriction. -/
inductive MissingDropStep (P : Program O) : Config O → Label O → Config O → Prop where
  | skip {e L st b dm k} : MissingDropStep P
      ⟨.exiting e, L, st, .scope b dm [] :: k⟩ .silent
      ⟨.exiting e, restrict dm L, st, k⟩
/-- Planted. Remove just the local restriction, retaining the storage callback. -/
inductive MissingRestrictStep (P : Program O) : Config O → Label O → Config O → Prop where
  | skip {e L st b dm k stDrop} : dropOut O dm L st = some stDrop → MissingRestrictStep P
      ⟨.exiting e, L, st, .scope b dm [] :: k⟩ .silent ⟨.exiting e, L, stDrop, k⟩

/-- Helper (application of `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`). The storage drop removes
`x` from the current activation while preserving `p`, the caller activation and counter. -/
example : BoundaryRun prog (Step prog) (.ret 7) suffix [emptyScope] live initial
    [.drop [y] live [(y, 8)]] [] [(y, 8)] middle :=
  (cleanup_runs_exact_plan prog (.ret 7) suffix [emptyScope] live [(y, 8)] initial middle
    [.drop [y] live [(y, 8)]] []).mpr ⟨empty_passes, rfl, rfl, .drop rfl .nil⟩

/-- Planted (certificate `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`). Missing storage teardown
has a real observed run with correct locals but cannot satisfy independent action execution. -/
example : BoundaryRun prog (MissingDropStep prog) (.ret 7) suffix [emptyScope] live initial
      [.drop [y] live [(y, 8)]] [] [(y, 8)] initial ∧
    ¬ Performed prog [.drop [y] live [(y, 8)]] live initial [] [(y, 8)] initial := by
  refine ⟨?_, ?_⟩
  · simpa [leaving, emptyScope, live, restrict, x, y, suffix] using
      (BoundaryRun.pass (pre := [emptyScope]) (pre' := []) (by simp)
        (by simp [emptyScope]) (MissingDropStep.skip (L := live)) BoundaryRun.stop)
  · intro h
    cases h with
    | drop hd rest =>
      cases rest
      have bad : middle = initial := Option.some.inj hd
      cases bad

/-- Planted (certificate `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`). Missing local restoration
has correct lifecycle state but its observed post-locals disagree with the planned action. -/
example : BoundaryRun prog (MissingRestrictStep prog) (.ret 7) suffix [emptyScope] live initial
      [.drop [y] live live] [] live middle ∧
    [.drop [y] live live] ≠ planWith [emptyScope] live := by
  refine ⟨?_, ?_⟩
  · simpa [leaving, emptyScope, live, restrict, x, y, suffix] using
      (BoundaryRun.pass (pre := [emptyScope]) (pre' := []) (by simp)
        (by simp [emptyScope]) (MissingRestrictStep.skip (stDrop := middle) rfl) BoundaryRun.stop)
  · intro h; cases h

/-- Planted. Add exactly one unregistered call to an empty scope; drops and resume stay real. -/
inductive ExtraStep (P : Program O) : Config O → Label O → Config O → Prop where
  | inject {e L st b dm k} {d : Deferred O} : ExtraStep P ⟨.exiting e, L, st, .scope b dm [] :: k⟩ .silent
      ⟨.exec d.stmt, L, st, .resume (.exit e) :: .scope b dm [] :: k⟩
  | retain {c l c'} : Step P c l c' → ExtraStep P c l c'

/-- Planted (certificate `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`). The injected call actually
emits 9; the sole added call is absent from the independent plan. -/
example : BoundaryRun prog (ExtraStep prog) (.ret 7) suffix [emptyScope] live initial
      [.call live (d 9), .drop [y] live [(y, 8)]] [9] [(y, 8)] middle ∧
    [.call live (d 9), .drop [y] live [(y, 8)]] ≠ planWith [emptyScope] live := by
  refine ⟨?_, ?_⟩
  · refine BoundaryRun.call (pre := [emptyScope]) (rest := [emptyScope]) (by simp)
      ExtraStep.inject (logSegment 9 (.ret 7) live (.resume (.exit (.ret 7)) :: emptyScope :: suffix) initial)
      (.retain (.silent rfl)) ?_
    simpa [leaving, emptyScope, live, restrict, x, y, suffix] using
      (BoundaryRun.pass (pre := [emptyScope]) (pre' := []) (by simp) (by simp [emptyScope])
        (ExtraStep.retain (Step.silent (P := prog) (c' := ⟨.exiting (.ret 7), [(y, 8)], middle, suffix⟩) rfl)) BoundaryRun.stop)
  · intro h; cases h

/-- Helper. The populated independent empty-scope drop. -/
private theorem drop_performed : Performed prog (planWith [emptyScope] live)
    live initial [] [(y, 8)] middle := .drop rfl .nil

/-- Helper (application of `ConcreteSpec.Core.Exec.boundary_of_performed`). Independent storage and
local restoration reconstruct the actual first-boundary execution. -/
example : BoundaryRun prog (Step prog) (.ret 7) suffix [emptyScope] live initial
    (planWith [emptyScope] live) [] [(y, 8)] middle :=
  boundary_of_performed [emptyScope] empty_passes drop_performed

/-- Planted. Omit only empty-scope teardown transitions. -/
inductive NoDropStep (P : Program O) : Config O → Label O → Config O → Prop where
  | retain {c l c'} : Step P c l c' → (match c.ctrl, c.frames with
      | .exiting _, .scope _ _ [] :: _ => False
      | _, _ => True) → NoDropStep P c l c'

/-- Planted (certificate `ConcreteSpec.Core.Exec.boundary_of_performed`). The independent populated
drop still succeeds, but the sole missing transition prevents reconstruction. -/
example : Performed prog (planWith [emptyScope] live) live initial [] [(y, 8)] middle ∧
    ¬ BoundaryRun prog (NoDropStep prog) (.ret 7) suffix [emptyScope] live initial
      (planWith [emptyScope] live) [] [(y, 8)] middle := by
  refine ⟨drop_performed, fun h => ?_⟩
  generalize ha : planWith [emptyScope] live = acts at h
  generalize ht : ([] : List Int) = tr at h
  cases h with
  | pass _ _ hs _ => cases hs with | retain _ impossible => exact impossible
  | call _ hs _ _ _ => cases hs with | retain _ impossible => exact impossible

/-- Helper. The nested continuation after the first scope's calls have completed. -/
private theorem base_execution : ∀ st tr,
    Performed prog [.drop [y] live [(y, 8)], .call [(y, 8)] (d 2), .drop [] [(y, 8)] []]
      live st tr [] middle →
    BoundaryRun prog (Step prog) (.ret 7) suffix [.scope ⟨1⟩ [y] [], outer] live st
      [.drop [y] live [(y, 8)], .call [(y, 8)] (d 2), .drop [] [(y, 8)] []] tr [] middle := by
  intro st tr h
  exact boundary_of_performed _ (by
    intro f hf
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hf
    rcases hf with rfl | rfl <;> trivial) h

/-- Helper (application of `ConcreteSpec.Core.Exec.boundary_scope_calls`). The final populated scope
drop is strictly after its call; the outer scope's call remains in the continuation. -/
example : BoundaryRun prog (Step prog) (.ret 7) suffix [inner, outer] live initial
    actions [1, 2] [] middle :=
  boundary_scope_calls ⟨1⟩ [y] [outer] [d 1] performed base_execution

/-- Planted (certificate `ConcreteSpec.Core.Exec.boundary_scope_calls`). Dispatch omission alone
prevents the witnessed call-then-drop execution. Independent actions remain satisfiable. -/
example : Performed prog actions live initial [1, 2] [] middle ∧
    ¬ BoundaryRun prog (NoDispatchStep prog) (.ret 7) suffix [inner, outer]
      live initial actions [1, 2] [] middle := ⟨performed, no_dispatch⟩

/-- A saved cleanup continuation, with an observable caller suffix below it. -/
def returnQ : List (Frame ops) := .resume (.exit (.ret 7)) :: emptyScope :: suffix
def entered : DState := DState.life (.enter [x]) initial
/-- Finite path vertices retain the actual function-end boundary. -/
def c0 : Config ops := ⟨.exec (d 1).stmt, live, initial, returnQ⟩
def c1 : Config ops := ⟨.exec logFn.body, [(x, 1)], entered,
  .scope logFn.block [x] [] :: .functionEnd :: .ret none live :: returnQ⟩
def c2 : Config ops := { c1 with ctrl := .done }
def c3 : Config ops := ⟨.done, [(x, 1)], entered, .functionEnd :: .ret none live :: returnQ⟩
def c4 : Config ops := ⟨.done, [(x, 1)], initial, .ret none live :: returnQ⟩
def c5 : Config ops := ⟨.done, live, initial, returnQ⟩
def c6 : Config ops := ⟨.exiting (.ret 7), live, initial, emptyScope :: suffix⟩
def c7 : Config ops := ⟨.exiting (.ret 7), [(y, 8)], middle, suffix⟩

/-- Helper. The callee's body, scope teardown, activation leave, caller restoration and
pending exit resume all execute before the outer cleanup boundary. -/
private theorem callee_raw : FirstStepsN (Step prog) (fun c => c.frames.length ≤ suffix.length)
    c1 [1] c7 6 := by
  refine FirstStepsN.step (O := ops) (R := Step prog) (stop := fun c => c.frames.length ≤ suffix.length) (l := .emit 1) (c' := c2) (by decide)
    (Step.prim (P := prog) (c := c1) (c' := c2) (a := ⟨some 1, .ok 0, entered⟩) rfl (by decide) rfl) ?_
  refine FirstStepsN.step (O := ops) (R := Step prog) (stop := fun c => c.frames.length ≤ suffix.length) (l := .silent) (c' := c3) (by decide) (.silent rfl) ?_
  refine FirstStepsN.step (O := ops) (R := Step prog) (stop := fun c => c.frames.length ≤ suffix.length) (l := .silent) (c' := c4) (by decide) (.silent rfl) ?_
  refine FirstStepsN.step (O := ops) (R := Step prog) (stop := fun c => c.frames.length ≤ suffix.length) (l := .silent) (c' := c5) (by decide) (.silent rfl) ?_
  refine FirstStepsN.step (O := ops) (R := Step prog) (stop := fun c => c.frames.length ≤ suffix.length) (l := .silent) (c' := c6) (by decide) (.silent rfl) ?_
  exact FirstStepsN.step (O := ops) (R := Step prog) (stop := fun c => c.frames.length ≤ suffix.length) (l := .silent) (by decide) (.silent rfl) (.stop (by decide))

/-- Helper. A full deferred call path with nonempty event trace and populated caller state. -/
private theorem call_raw : FirstStepsN (Step prog) (fun c => c.frames.length ≤ suffix.length)
    c0 [1] c7 7 := .step (by decide) (.silent rfl) callee_raw

/-- Instance (law `ConcreteSpec.Core.Exec.FirstStepsN.split_return`). The saved caller is
restored after a real event and activation leave, before the pending exit resumes. -/
example : ∃ n₁ n₂ tr₁ st₁ tr₂, 6 = n₁ + n₂ ∧ [1] = tr₁ ++ tr₂ ∧
    FirstStepsN (Step prog) (fun c => c.ctrl = .done ∧ c.frames = returnQ)
      c1 tr₁ ⟨.done, live, st₁, returnQ⟩ n₁ ∧
    FirstStepsN (Step prog) (fun c => c.frames.length ≤ suffix.length)
      ⟨.done, live, st₁, returnQ⟩ tr₂ c7 n₂ :=
  FirstStepsN.split_return callee_raw (by decide) ⟨[_, _], rfl⟩

/-- Instance (law `ConcreteSpec.Core.Exec.FirstStepsN.call_return`). A deferred call crosses
its saved continuation only after restoring populated caller locals and the prior activation. -/
example : ∃ n₁ n₂ tr₁ st₁ tr₂, 7 = n₁ + n₂ ∧ [1] = tr₁ ++ tr₂ ∧
    FirstStepsN (Step prog) (fun c => c.ctrl = .done ∧ c.frames = returnQ)
      c0 tr₁ ⟨.done, live, st₁, returnQ⟩ n₁ ∧
    FirstStepsN (Step prog) (fun c => c.frames.length ≤ suffix.length)
      ⟨.done, live, st₁, returnQ⟩ tr₂ c7 n₂ :=
  FirstStepsN.call_return (d := d 1) (by decide) call_raw

/-- Planted. On this complete concrete path, replace only caller-local restoration by keeping
callee locals. Every other edge is a production transition; the event and activation leave
are unchanged. A finite path relation makes the omitted operation explicit. -/
def bad5 : Config ops := { c5 with locals := [(x, 1)] }
def bad6 : Config ops := { c6 with locals := [(x, 1)] }
def bad7 : Config ops := { c7 with locals := [] }
inductive WrongReturn : Config ops → Label ops → Config ops → Prop where
  | enter : WrongReturn c0 .silent c1
  | event : WrongReturn c1 (.emit 1) c2
  | drop : WrongReturn c2 .silent c3
  | leave : WrongReturn c3 .silent c4
  | forget : WrongReturn c4 .silent bad5
  | resume : WrongReturn bad5 .silent bad6
  | finish : WrongReturn bad6 .silent bad7

/-- Helper. Every unchanged edge of the planted path is checked against production. -/
private theorem unchanged_edges : Step prog c0 .silent c1 ∧ Step prog c1 (.emit 1) c2 ∧
    Step prog c2 .silent c3 ∧ Step prog c3 .silent c4 ∧
    Step prog bad5 .silent bad6 ∧ Step prog bad6 .silent bad7 :=
  ⟨.silent rfl, .prim (P := prog) (c := c1) (c' := c2) (a := ⟨some 1, .ok 0, entered⟩) rfl (by decide) rfl, .silent rfl, .silent rfl, .silent rfl, .silent rfl⟩

/-- Helper. The mutant never produces the restored caller boundary. -/
private theorem wrong_not_restored {c c' : Config ops} {label : Label ops}
    (h : WrongReturn c label c') : ¬ (c'.ctrl = .done ∧ c'.locals = live ∧ c'.frames = returnQ) := by
  cases h <;> simp [c1, c2, c3, c4, c5, c6, c7, bad5, bad6, bad7, returnQ, emptyScope, suffix, live]

/-- Helper. A path of this mutant starting away from the restored configuration cannot visit it. -/
private theorem wrong_no_visit {stop : Config ops → Prop} {c c' : Config ops} {tr n}
    (h : FirstStepsN WrongReturn stop c tr c' n)
    (hn : ¬ (c.ctrl = .done ∧ c.locals = live ∧ c.frames = returnQ)) :
    ¬ (c'.ctrl = .done ∧ c'.locals = live ∧ c'.frames = returnQ) := by
  induction h with
  | stop => exact hn
  | step _ hs _ ih => exact ih (wrong_not_restored hs)

/-- Helper. The planted call still reaches the same outer boundary with its real event. -/
private theorem wrong_tail : FirstStepsN WrongReturn (fun c => c.frames.length ≤ suffix.length)
    c1 [1] bad7 6 := by
  refine FirstStepsN.step (R := WrongReturn) (stop := fun c => c.frames.length ≤ suffix.length)
    (c' := c2) (by decide) WrongReturn.event ?_
  refine FirstStepsN.step (R := WrongReturn) (stop := fun c => c.frames.length ≤ suffix.length)
    (c' := c3) (by decide) WrongReturn.drop ?_
  refine FirstStepsN.step (R := WrongReturn) (stop := fun c => c.frames.length ≤ suffix.length)
    (c' := c4) (by decide) WrongReturn.leave ?_
  refine FirstStepsN.step (R := WrongReturn) (stop := fun c => c.frames.length ≤ suffix.length)
    (c' := bad5) (by decide) WrongReturn.forget ?_
  refine FirstStepsN.step (R := WrongReturn) (stop := fun c => c.frames.length ≤ suffix.length)
    (c' := bad6) (by decide) WrongReturn.resume ?_
  exact FirstStepsN.step (R := WrongReturn) (stop := fun c => c.frames.length ≤ suffix.length)
    (by decide) WrongReturn.finish (.stop (by decide))

/-- Helper. Extend the mutant path by its unchanged entry transition. -/
private theorem wrong_raw : FirstStepsN WrongReturn (fun c => c.frames.length ≤ suffix.length)
    c0 [1] bad7 7 := .step (by decide) WrongReturn.enter wrong_tail

/-- Planted (law `ConcreteSpec.Core.Exec.FirstStepsN.call_return`). Raw execution, boundary
height and the emitted event survive; restoring the caller before crossing does not. -/
example : FirstStepsN WrongReturn (fun c => c.frames.length ≤ suffix.length) c0 [1] bad7 7 ∧
    ¬ (∃ n₁ n₂ tr₁ st₁ tr₂, 7 = n₁ + n₂ ∧ [1] = tr₁ ++ tr₂ ∧
      FirstStepsN WrongReturn (fun c => c.ctrl = .done ∧ c.frames = returnQ)
        c0 tr₁ ⟨.done, live, st₁, returnQ⟩ n₁ ∧
      FirstStepsN WrongReturn (fun c => c.frames.length ≤ suffix.length)
        ⟨.done, live, st₁, returnQ⟩ tr₂ bad7 n₂) := by
  refine ⟨wrong_raw, ?_⟩
  rintro ⟨_, _, _, _, _, _, _, first, _⟩
  exact wrong_no_visit first (by intro h; cases h.1) ⟨rfl, rfl, rfl⟩

/-- Planted (law `ConcreteSpec.Core.Exec.FirstStepsN.split_return`). Starting inside the
same callee retains its protected saved frame and raw endpoint but loses caller restoration. -/
example : Protected (O := ops) live returnQ c1 ∧
    FirstStepsN WrongReturn (fun c => c.frames.length ≤ suffix.length) c1 [1] bad7 6 ∧
    ¬ (∃ n₁ n₂ tr₁ st₁ tr₂, 6 = n₁ + n₂ ∧ [1] = tr₁ ++ tr₂ ∧
      FirstStepsN WrongReturn (fun c => c.ctrl = .done ∧ c.frames = returnQ)
        c1 tr₁ ⟨.done, live, st₁, returnQ⟩ n₁ ∧
      FirstStepsN WrongReturn (fun c => c.frames.length ≤ suffix.length)
        ⟨.done, live, st₁, returnQ⟩ tr₂ bad7 n₂) := by
  refine ⟨⟨[_, _], rfl⟩, ?_, ?_⟩
  · exact wrong_tail
  · rintro ⟨_, _, _, _, _, _, _, first, _⟩
    exact wrong_no_visit first (by intro h; cases h.1) ⟨rfl, rfl, rfl⟩

/-- Instance (law `ConcreteSpec.Core.Exec.protected_step`). Return restores the populated
caller while preserving the current lifecycle store and saved suffix. -/
example : PreservesCaller (Step prog) live returnQ c4 := protected_step ⟨[], rfl⟩
/-- Planted (law `ConcreteSpec.Core.Exec.protected_step`). Keeping callee locals violates
only the return-restoration operation at the actual saved boundary. -/
example : Protected (O := ops) live returnQ c4 ∧ ¬ PreservesCaller WrongReturn live returnQ c4 := by
  refine ⟨⟨[], rfl⟩, fun h => ?_⟩
  cases h .silent bad5 .forget with
  | inl hp =>
    obtain ⟨pre, he⟩ := hp
    have hh := congrArg List.length he
    simp only [bad5, c5, List.length_append, List.length_cons] at hh
    omega
  | inr hr => have bad := hr.2.1; cases bad

/-- Instance (law `ConcreteSpec.Core.Exec.call_protects`). Entering the actual deferred log
installs a protected saved caller beneath scope and function-end frames. -/
example : SavesCaller (Step prog) live initial returnQ (d 1) := call_protects

/-- Planted. Entry is identical except that it omits the saved caller frame. -/
inductive MissingCaller : Config ops → Label ops → Config ops → Prop where
  | enter : MissingCaller c0 .silent
      ⟨.exec logFn.body, [(x, 1)], entered, .scope logFn.block [x] [] :: .functionEnd :: returnQ⟩

/-- Planted (law `ConcreteSpec.Core.Exec.call_protects`). Omitting the saved caller frame
cannot protect this populated caller; the callee, event opportunity and lifecycle entry stay. -/
example : ¬ SavesCaller MissingCaller live initial returnQ (d 1) := by
  intro h
  cases h .silent _ .enter with
  | inl ht => obtain ⟨_, _, ht, _⟩ := ht; cases ht
  | inr hp =>
    obtain ⟨pre, he⟩ := hp
    change .scope logFn.block [x] [] :: .functionEnd :: returnQ = _ at he
    have hm : Frame.ret none live ∈
        (.scope logFn.block [x] [] :: .functionEnd :: returnQ : List (Frame ops)) := by
      rw [he]
      simp
    simp [returnQ, emptyScope, suffix] at hm

/-- Helper. A populated raw scope unwind includes dispatch, the complete call, restoration,
resume and storage teardown, stopping before the abort in the caller's suffix. -/
private theorem raw_scope : FirstStepsN (Step prog) (fun c => c.frames.length ≤ suffix.length)
    ⟨.exiting (.ret 7), live, initial, inner :: suffix⟩ [1] c7 8 :=
  .step (by decide) (.silent rfl) call_raw

/-- Helper. The singleton registered scope passes a return. -/
private theorem inner_passes : ∀ f ∈ [inner], Passes (O := ops) (.ret 7) f := by
  intro f hf
  simp only [List.mem_singleton] at hf
  subst f
  trivial

/-- Instance (law `ConcreteSpec.Core.Exec.boundary_complete`). A real first-arrival execution
with event, populated locals and activation state admits dispatch segmentation. -/
example : ∃ acts, BoundaryRun prog (Step prog) (.ret 7) suffix [inner]
    live initial acts [1] [(y, 8)] middle := boundary_complete raw_scope rfl inner_passes

/-- Instance (law `ConcreteSpec.Core.Exec.raw_cleanup_runs_plan`). The actual first-arrival
path gives exactly the independent plan, restored locals and performed actions. -/
example : ∃ acts, BoundaryRun prog (Step prog) (.ret 7) suffix [inner]
      live initial acts [1] [(y, 8)] middle ∧
    acts = planWith [inner] live ∧ [(y, 8)] = unwindLocals [inner] live ∧
    Performed prog acts live initial [1] [(y, 8)] middle :=
  raw_cleanup_runs_plan prog (.ret 7) suffix [inner] live [(y, 8)] initial middle [1]
    inner_passes raw_scope.unsized_inv

/-- Planted. Replace just the dispatched call statement by `skip`; resume and scope teardown
are the production transitions. The other registrations and lifecycle state are retained. -/
def skipped : Config ops := ⟨.exec .skip, live, initial, returnQ⟩
inductive SkipCall : Config ops → Label ops → Config ops → Prop where
  | skip : SkipCall ⟨.exiting (.ret 7), live, initial, inner :: suffix⟩ .silent skipped
  | done : SkipCall skipped .silent c5
  | resume : SkipCall c5 .silent c6
  | drop : SkipCall c6 .silent c7

/-- Helper. Only the selected call was omitted; all subsequent edges remain real steps. -/
private theorem skip_edges : Step prog skipped .silent c5 ∧ Step prog c5 .silent c6 ∧
    Step prog c6 .silent c7 := ⟨.silent rfl, .silent rfl, .silent rfl⟩

/-- Helper. The planted skip still reaches the boundary, with correct final locals and store. -/
private theorem skip_raw : FirstStepsN SkipCall (fun c => c.frames.length ≤ suffix.length)
    ⟨.exiting (.ret 7), live, initial, inner :: suffix⟩ [] c7 4 := by
  refine FirstStepsN.step (R := SkipCall) (stop := fun c => c.frames.length ≤ suffix.length)
    (c' := skipped) (by decide) SkipCall.skip ?_
  refine FirstStepsN.step (R := SkipCall) (stop := fun c => c.frames.length ≤ suffix.length)
    (c' := c5) (by decide) SkipCall.done ?_
  refine FirstStepsN.step (R := SkipCall) (stop := fun c => c.frames.length ≤ suffix.length)
    (c' := c6) (by decide) SkipCall.resume ?_
  exact FirstStepsN.step (R := SkipCall) (stop := fun c => c.frames.length ≤ suffix.length)
    (by decide) SkipCall.drop (.stop (by decide))

/-- Helper. An omitted call cannot be segmented as a real deferred-call dispatch. -/
private theorem skip_no_boundary : ¬ ∃ acts, BoundaryRun prog SkipCall (.ret 7) suffix [inner]
    live initial acts [] [(y, 8)] middle := by
  rintro ⟨_, h⟩
  generalize ht : ([] : List Int) = tr at h
  cases h with
  | pass _ _ hs _ => cases hs
  | call _ hs _ _ _ => cases hs

/-- Planted (law `ConcreteSpec.Core.Exec.boundary_complete`). All raw-path and endpoint
premises survive omission of dispatch; no first-boundary segmentation exists. -/
example : FirstStepsN SkipCall (fun c => c.frames.length ≤ suffix.length)
      ⟨.exiting (.ret 7), live, initial, inner :: suffix⟩ [] c7 4 ∧
    (∀ f ∈ [inner], Passes (O := ops) (.ret 7) f) ∧
    ¬ ∃ acts, BoundaryRun prog SkipCall (.ret 7) suffix [inner]
      live initial acts [] [(y, 8)] middle := ⟨skip_raw, inner_passes, skip_no_boundary⟩

/-- Planted (law `ConcreteSpec.Core.Exec.raw_cleanup_runs_plan`). This raw first arrival
preserves passability and storage teardown but omits the planned call, so the law fails. -/
example : FirstSteps SkipCall (fun c => c.frames.length ≤ suffix.length)
      ⟨.exiting (.ret 7), live, initial, inner :: suffix⟩ [] c7 ∧
    ¬ (∃ acts, BoundaryRun prog SkipCall (.ret 7) suffix [inner]
        live initial acts [] [(y, 8)] middle ∧
      acts = planWith [inner] live ∧ [(y, 8)] = unwindLocals [inner] live ∧
      Performed prog acts live initial [] [(y, 8)] middle) := by
  refine ⟨?_, ?_⟩
  · exact skip_raw.unsized_inv
  · rintro ⟨acts, hb, _⟩
    exact skip_no_boundary ⟨acts, hb⟩

/-- Instance (law `ConcreteSpec.Core.Exec.register_protected`). Registration traverses a
sequence frame and preserves populated scope, saved caller and nonempty continuation. -/
example : register (d 2) ([.seq .skip, inner] ++ .ret none live :: returnQ) =
      some ([.seq .skip, .scope ⟨1⟩ [y] [d 2, d 1]] ++ .ret none live :: returnQ) ∧
    RegistrationPreservesCaller register (d 2) live returnQ := ⟨rfl, register_protected _ _ _⟩

/-- Planted. Register normally in the nearest scope, but erase only the saved caller frame. -/
def forgetRegister (d : Deferred O) (frames : List (Frame O)) : Option (List (Frame O)) :=
  (register d frames).map (List.filter fun f => match f with | .ret .. => false | _ => true)

/-- Planted (law `ConcreteSpec.Core.Exec.register_protected`). A successful populated
registration loses its saved boundary when only that frame is erased. -/
example : ¬ RegistrationPreservesCaller forgetRegister (d 2) live returnQ := by
  intro h
  obtain ⟨pre, he⟩ := h [.seq .skip, inner] _ rfl
  change [.seq .skip, .scope ⟨1⟩ [y] [d 2, d 1]] ++ returnQ = _ at he
  have hm : Frame.ret none live ∈
      ([.seq .skip, .scope ⟨1⟩ [y] [d 2, d 1]] ++ returnQ : List (Frame ops)) := by
    rw [he]; simp
  simp [returnQ, emptyScope, suffix] at hm

/-- Instance (law `ConcreteSpec.Core.Exec.exit_label`). Dispatch is silent; the event belongs
to the call body, as witnessed by `raw_scope`. -/
example : ExitSilent (Step prog) (.ret 7) live initial (inner :: suffix) := exit_label

/-- Planted. Change only the label on the real scope dispatch. -/
inductive NoisyExit : Config ops → Label ops → Config ops → Prop where
  | dispatch : NoisyExit ⟨.exiting (.ret 7), live, initial, inner :: suffix⟩ (.emit 9) c0

/-- Planted (law `ConcreteSpec.Core.Exec.exit_label`). A spurious dispatcher event is visible
with the same source and target as the production silent transition. -/
example : Step prog ⟨.exiting (.ret 7), live, initial, inner :: suffix⟩ .silent c0 ∧
    ¬ ExitSilent NoisyExit (.ret 7) live initial (inner :: suffix) := by
  refine ⟨.silent rfl, fun h => ?_⟩
  cases h (.emit 9) c0 NoisyExit.dispatch

/-- A trap retains queued calls, lifecycle state and saved caller frames without executing them. -/
def trapped : Config ops := ⟨.trapped .abort s, live, initial,
  [inner, outer, .functionEnd, .ret none [(t, 12)]]⟩

/-- Instance (law `ConcreteSpec.Core.Exec.step_trapped`). The pending cleanup and caller do
not prevent final recognition or allow another transition. -/
example : (trapped.final? = some (.trapped .abort s) ↔ trapped.ctrl = .trapped .abort s) ∧
    Blocked (Step prog) trapped := step_trapped prog _ _ _ _ _ _

/-- Planted. Omit only trapped-state final recognition. -/
def noTrapFinal? (c : Config O) : Option (Final O.Val) :=
  match c.ctrl with | .trapped .. => none | _ => c.final?
/-- Planted (law `ConcreteSpec.Core.Exec.step_trapped`). The real trap is no longer final. -/
example : ¬ (noTrapFinal? trapped = some (.trapped .abort s) ↔
    trapped.ctrl = .trapped .abort s) := by
  intro h; cases h.mpr rfl

/-- Planted. Add only recovery from this trapped state to `done`. -/
inductive RecoverTrap : Config ops → Label ops → Config ops → Prop where
  | recover : RecoverTrap trapped .silent { trapped with ctrl := .done }
/-- Planted (law `ConcreteSpec.Core.Exec.step_trapped`). Recovery violates terminality while
preserving final recognition, populated locals, lifecycle state and pending calls. -/
example : trapped.final? = some (.trapped .abort s) ∧ ¬ Blocked RecoverTrap trapped := by
  refine ⟨rfl, fun h => ?_⟩
  exact h .silent _ RecoverTrap.recover

/-- Instance (law `ConcreteSpec.Core.Exec.steps_from_trapped`). Recovery cannot change even
just the control field of the populated trapped endpoint. -/
example : ¬ Steps prog trapped [] { trapped with ctrl := .done } := by
  intro h
  have bad := ((steps_from_trapped prog .abort s live initial _ [] _).mp h).2
  cases bad

/-- Instance (law `ConcreteSpec.Core.Exec.steps_from_trapped`). No event can be emitted even
when the endpoint remains exactly the trapped configuration. -/
example : ¬ Steps prog trapped [9] trapped := by
  intro h
  have bad := ((steps_from_trapped prog .abort s live initial _ [9] trapped).mp h).1
  cases bad

/-- Planted. Add only silent recovery to a changed endpoint. -/
inductive RecoverTrapSteps : Config ops → List Int → Config ops → Prop where
  | recover : RecoverTrapSteps trapped [] { trapped with ctrl := .done }
/-- Planted (law `ConcreteSpec.Core.Exec.steps_from_trapped`). The changed-endpoint exclusion
fails with just the forbidden silent recovery. -/
example : ¬ (¬ RecoverTrapSteps trapped [] { trapped with ctrl := .done }) :=
  fun h => h .recover

/-- Planted. Add only an event at the identical trapped endpoint. -/
inductive NoisyTrapSteps : Config ops → List Int → Config ops → Prop where
  | emit : NoisyTrapSteps trapped [9] trapped
/-- Planted (law `ConcreteSpec.Core.Exec.steps_from_trapped`). The nonempty-trace exclusion
fails with just the forbidden event, without changing the endpoint. -/
example : ¬ (¬ NoisyTrapSteps trapped [9] trapped) := fun h => h .emit

/-- Instance (law `ConcreteSpec.Core.Exec.abort_runs_no_cleanup`). Abort reaches its trap
without touching registered calls, locals, activation markers or the caller frame. -/
example : Steps prog { trapped with ctrl := .exec (.abort s) } [] trapped :=
  (abort_runs_no_cleanup prog s live initial _ [] trapped).mpr ⟨rfl, .inr rfl⟩

/-- Planted. Replace abort's terminal transition by unwinding; all later transitions are real. -/
inductive AbortCleanupSteps : Config ops → List Int → Config ops → Prop where
  | cleanup {tr c} : Steps prog ⟨.exiting (.ret 7), live, initial, inner :: suffix⟩ tr c →
      AbortCleanupSteps ⟨.exec (.abort s), live, initial, inner :: suffix⟩ tr c
/-- Planted (law `ConcreteSpec.Core.Exec.abort_runs_no_cleanup`). Wrongly starting cleanup
emits the registered call's real event, violating abort's empty-trace conclusion. -/
example : AbortCleanupSteps ⟨.exec (.abort s), live, initial, inner :: suffix⟩ [1] c7 ∧
    ¬ ([1] = ([] : List Int) ∧
      (c7 = ⟨.exec (.abort s), live, initial, inner :: suffix⟩ ∨
       c7 = ⟨.trapped .abort s, live, initial, inner :: suffix⟩)) :=
  ⟨.cleanup raw_scope.unsized_inv.steps_inv, by simp⟩

/-- Instance (law `ConcreteSpec.Core.Exec.steps_from_trapped`). A deferred trap stops the
program before either later registered log or activation leave, as OD68 requires. -/
example : runFrom prog env 30 [13]
    ⟨.exiting (.ret 7), live, initial,
      [.scope ⟨1⟩ [] [⟨boomF, [], s⟩, d 2], outer, .functionEnd]⟩ =
    ⟨[13], .final (.trapped .abort ⟨9⟩),
      ⟨.trapped .abort ⟨9⟩, [], DState.life (.enter []) initial,
        [.scope boomFn.block [] [], .functionEnd, .ret none live,
         .resume (.exit (.ret 7)), .scope ⟨1⟩ [] [d 2], outer, .functionEnd]⟩, 27⟩ := rfl

/-- Helper. A toy operand interface observing the earlier move: reading `y` traps with
overflow exactly when `y` is marked moved. All other operands use the existing demo evaluator.
This is evidence for abstract operand threading, not a Concrete operand policy. -/
abbrev trapOps : Operands := { ops with eval := fun L st e => match e with
  | .var v => if v = y ∧ y ∈ st.top then .trap .overflow else ops.eval L st e
  | _ => ops.eval L st e }

/-- Instance (law `ConcreteSpec.Core.Exec.evalArgs_first_trap`). Only the prefix's move makes
this operand trap; evaluating the suffix would instead report division by zero. -/
example : evalArgs trapOps [(y, 8)] initial
    ([.move y] ++ .var y :: [.fail .divisionByZero]) = .trap .overflow :=
  evalArgs_first_trap (O := trapOps) [(y, 8)] (.var y) .overflow
    [.fail .divisionByZero] [.move y] initial ⟨4, [[y, x, p], [t]]⟩ [8] rfl rfl

/-- Planted. Discard only each successful operand's updated store. -/
def lostStateArgs (O : Operands) (L : AList Var O.Val) :
    O.Store → List O.Expr → Res (List O.Val × O.Store)
  | st, [] => .ok ([], st)
  | st, e :: es => (O.eval L st e).bind fun (v, _) =>
      (lostStateArgs O L st es).map fun (vs, st₂) => (v :: vs, st₂)
/-- Planted (law `ConcreteSpec.Core.Exec.evalArgs_first_trap`). Losing the prefix state
prevents the state-dependent overflow, exposing the later division-by-zero trap. -/
example : lostStateArgs trapOps [(y, 8)] initial
    [.move y, .var y, .fail .divisionByZero] = .trap .divisionByZero ∧
    lostStateArgs trapOps [(y, 8)] initial
      [.move y, .var y, .fail .divisionByZero] ≠ .trap .overflow := by decide

/-- Planted. Continue past a trap only when a later operand exists. Successful effects
remain threaded; a final trap is still propagated. -/
def pastTrapArgs (O : Operands) (L : AList Var O.Val) :
    O.Store → List O.Expr → Res (List O.Val × O.Store)
  | st, [] => .ok ([], st)
  | st, e :: es => match O.eval L st e with
    | .ok (v, st₁) => (pastTrapArgs O L st₁ es).map fun (vs, st₂) => (v :: vs, st₂)
    | .trap kd => match es with | [] => .trap kd | _ :: _ => pastTrapArgs O L st es
    | .stuck => .stuck
/-- Planted (law `ConcreteSpec.Core.Exec.evalArgs_first_trap`). Continuing replaces the
state-dependent first trap by the suffix's distinct trap. -/
example : pastTrapArgs trapOps [(y, 8)] initial
    [.move y, .var y, .fail .divisionByZero] = .trap .divisionByZero ∧
    pastTrapArgs trapOps [(y, 8)] initial
      [.move y, .var y, .fail .divisionByZero] ≠ .trap .overflow := by decide

/-- Planted. Swallow only an argument trap; successful prefix effects remain threaded. -/
def swallowTrapEvalArgs (O : Operands) (L : AList Var O.Val) :
    O.Store → List O.Expr → Res (List O.Val × O.Store)
  | st, [] => .ok ([], st)
  | st, e :: es => match O.eval L st e with
    | .ok (v, st₁) => (swallowTrapEvalArgs O L st₁ es).map fun (vs, st₂) => (v :: vs, st₂)
    | .trap _ => .ok ([], st)
    | .stuck => .stuck
/-- Planted (law `ConcreteSpec.Core.Exec.evalArgs_first_trap`). Swallowing the same first
trap returns the moved prefix and its updated state rather than propagating overflow. -/
example : swallowTrapEvalArgs trapOps [(y, 8)] initial
    [.move y, .var y, .fail .divisionByZero] = .ok ([8], ⟨4, [[y, x, p], [t]]⟩) ∧
    swallowTrapEvalArgs trapOps [(y, 8)] initial
      [.move y, .var y, .fail .divisionByZero] ≠ .trap .overflow := by decide


/-- Helper. With only inner dispatch omitted, the higher-order final-drop premise remains
executable: the outer call still dispatches and both scope drops retain their callbacks. -/
private theorem mutant_base_execution : ∀ st tr,
    Performed prog [.drop [y] live [(y, 8)], .call [(y, 8)] (d 2), .drop [] [(y, 8)] []]
      live st tr [] middle →
    BoundaryRun prog (NoDispatchStep prog) (.ret 7) suffix [.scope ⟨1⟩ [y] [], outer] live st
      [.drop [y] live [(y, 8)], .call [(y, 8)] (d 2), .drop [] [(y, 8)] []] tr [] middle := by
  intro st tr h
  cases h with
  | drop hd h =>
    cases h with
    | call hc h =>
      cases h with
      | drop hd' h =>
        cases h
        rename_i stDrop trCall stCall q
        refine BoundaryRun.pass (pre := [.scope ⟨1⟩ [y] [], outer])
          (st' := stDrop) (pre' := [outer]) (by simp) (by simp) ?_ ?_
        · exact .retain (.silent (by simp only [List.cons_append, List.nil_append, step, stepExit, hd, Option.map_some]; rfl)) True.intro
        · refine BoundaryRun.call (pre := [outer]) (rest := [.scope ⟨2⟩ [] []]) (by simp)
            (.retain (.silent rfl) (by change (if (⟨2⟩ : BlockId) = ⟨1⟩ then False else True); decide)) hc (.retain (.silent rfl) True.intro) ?_
          simpa only [leaving, List.append_nil] using
            (BoundaryRun.pass (pre := [.scope ⟨2⟩ [] []]) (pre' := []) (by simp) (by simp)
              (NoDispatchStep.retain (Step.silent (by simp only [List.cons_append, List.nil_append, step, stepExit, hd', Option.map_some]; rfl)) True.intro)
              BoundaryRun.stop)

/-- Planted. The call-then-drop reconstruction's higher-order premise is checked against the
mutant itself. Omitting only the inner dispatch cannot be hidden by a broken base premise. -/
example : (∀ st tr,
    Performed prog [.drop [y] live [(y, 8)], .call [(y, 8)] (d 2), .drop [] [(y, 8)] []]
      live st tr [] middle →
    BoundaryRun prog (NoDispatchStep prog) (.ret 7) suffix [.scope ⟨1⟩ [y] [], outer] live st
      [.drop [y] live [(y, 8)], .call [(y, 8)] (d 2), .drop [] [(y, 8)] []] tr [] middle) ∧
    Performed prog actions live initial [1, 2] [] middle ∧
    ¬ BoundaryRun prog (NoDispatchStep prog) (.ret 7) suffix [inner, outer]
      live initial actions [1, 2] [] middle := ⟨mutant_base_execution, performed, no_dispatch⟩

/-- Helper. A concrete order-sensitive effect witness for cleanup: bump before the late
counter read emits 5; reversing only those two registered calls emits 4. Both restore the
same populated locals and produce the same final counter and activation state. This uses
the admitted sole driver, not another execution policy. -/
def orderFrames (reverse : Bool) : List (Frame ops) :=
  [.scope ⟨1⟩ [y] (if reverse then [⟨logF, [.counter], s⟩, ⟨bumpF, [], s⟩]
      else [⟨bumpF, [], s⟩, ⟨logF, [.counter], s⟩])] ++ suffix
/-- Helper. Positive state-dependent call-order witness accompanying the raw cleanup law. -/
example : (runFrom prog env 40 [13] ⟨.exiting (.ret 7), live, initial, orderFrames false⟩).events = [13, 5] ∧
    (runFrom prog env 40 [13] ⟨.exiting (.ret 7), live, initial, orderFrames false⟩).residual.store =
      ⟨5, [[p], [t]]⟩ := ⟨rfl, rfl⟩
/-- Planted. Reverse just the registered calls. The counter read now observes the earlier
state, despite identical final counter, locals and lifecycle state. -/
example : (runFrom prog env 40 [13] ⟨.exiting (.ret 7), live, initial, orderFrames true⟩).events = [13, 4] ∧
    (runFrom prog env 40 [13] ⟨.exiting (.ret 7), live, initial, orderFrames true⟩).residual.store =
      ⟨5, [[p], [t]]⟩ ∧
    (runFrom prog env 40 [13] ⟨.exiting (.ret 7), live, initial, orderFrames true⟩).residual.locals = [(y, 8)] :=
  ⟨rfl, rfl, rfl⟩

end CleanupEvidence
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
No held path,
constant, Boolean representation, pointer, wiping or startup policy is introduced here.
-/
namespace DeterminismEvidence
open Demo CleanupEvidence

/-- Instance support. A skip in a populated running activation. -/
def source : Config ops := ⟨.exec .skip, live, initial, suffix⟩
/-- Instance support. Skip retains every component except control. -/
def target : Config ops := ⟨.done, live, initial, suffix⟩
/-- Planted support. The overlapping skip rule erroneously drops the continuation. -/
def wrong : Config ops := ⟨.done, live, initial, []⟩

/-- Instance (law `rules_deterministic`). A real skip premise and its uniqueness conclusion. -/
theorem rules_deterministic_instance : Rules prog source none .silent target ∧
    InputDeterministic Rules prog source target none .silent :=
  ⟨.skip, rules_deterministic prog source target none .silent .skip⟩

/-- Instance (law `rules_silent_deterministic`). Nonempty locals, store and frames are preserved. -/
theorem rules_silent_deterministic_instance : Rules prog source none .silent target ∧
    InputDeterministic Rules prog source target none .silent :=
  ⟨.skip, rules_silent_deterministic prog source target .silent .skip⟩

/-- Planted. Keep all original rules and add just an overlapping skip transition. -/
inductive Overlap (P : Program ops) : Config ops → Option (Answer Int Int DState) →
    Label ops → Config ops → Prop where
  | original {c a l c'} : Rules P c a l c' → Overlap P c a l c'
  | skip {L st k} : Overlap P ⟨.exec .skip, L, st, k⟩ none .silent ⟨.done, L, st, []⟩

/-- Planted (law `rules_deterministic`). The original transition still satisfies the premise,
but the uniformly substituted relation has a second destination at exactly the same input. -/
theorem rules_deterministic_planted : Overlap prog source none .silent target ∧
    ¬ InputDeterministic Overlap prog source target none .silent := by
  refine ⟨.original .skip, ?_⟩
  intro h
  have he := (h .silent wrong .skip).2
  have hf := congrArg Config.frames he
  cases hf

/-- Planted (law `rules_silent_deterministic`). The added skip also refutes the silent specialization. -/
theorem rules_silent_deterministic_planted :
    ¬ (∀ (c c₁ : Config ops) (l₁ : Label ops), Overlap prog c none l₁ c₁ →
      InputDeterministic Overlap prog c c₁ none l₁) := by
  intro h
  exact rules_deterministic_planted.2 (h source target .silent rules_deterministic_planted.1)

/-- Instance (law `rules_sound`). The independent skip rule is implemented exactly. -/
theorem rules_sound_instance : Rules prog source none .silent target ∧
    Dispatches step prog source none .silent target :=
  ⟨.skip, rules_sound prog source none .silent target .skip⟩

/-- Instance (law `rules_complete`). A populated executable skip has a rule derivation. -/
theorem rules_complete_instance : Dispatches step prog source none .silent target ∧
    Rules prog source none .silent target :=
  ⟨.silent rfl, rules_complete prog source none .silent target (.silent rfl)⟩

/-- Planted. Delete only the executable skip transition; every other dispatch is unchanged. -/
def removed (P : Program O) (c : Config O) : Option (Out O) :=
  match c.ctrl with
  | .exec .skip => none
  | _ => step P c

/-- Planted. Add only a transition at an already trapped configuration. Traps must stop;
the mutant resumes with the same locals, store and continuation. -/
def added (P : Program O) (c : Config O) : Option (Out O) :=
  match c.ctrl with
  | .trapped _ _ => some (.next { c with ctrl := .done })
  | _ => step P c

/-- Planted (law `rules_sound`). Removing skip preserves the independent premise while
refuting the implementation conclusion. The mutated dispatcher occurs uniformly. -/
theorem rules_sound_planted : Rules prog source none .silent target ∧
    ¬ Dispatches removed prog source none .silent target := by
  refine ⟨.skip, ?_⟩
  intro h
  cases h with
  | silent hs => cases hs

/-- Instance support. A populated terminal trap with unexecuted cleanup frames. -/
def stopped : Config ops := ⟨.trapped .abort s, live, initial, suffix⟩

/-- Planted (law `rules_complete`). Adding a transition at a trap gives a real mutant
premise and no independent conclusion; no failed proof or timeout is used as evidence. -/
theorem rules_complete_planted :
    Dispatches added prog stopped none .silent target ∧
    ¬ DispatchComplete added prog stopped none := by
  refine ⟨.silent rfl, ?_⟩
  intro h
  have hr := h .silent target (.silent rfl)
  cases hr

/-- Instance support. A contract-admitted silent primitive answer changes the counter. -/
def bumpAnswer : Answer Int Int DState := ⟨none, .ok 0, ⟨5, initial.moved⟩⟩
/-- Instance support. Primitive invocation with populated machine state. -/
def bumpSource : Config ops := ⟨.exec (.prim none .bump [] s), live, initial, suffix⟩
/-- Instance support. The admitted answer's state is retained. -/
def bumpTarget : Config ops := ⟨.done, live, bumpAnswer.store, suffix⟩

/-- Instance (law `rules_deterministic`). Primitive determinism fixes the entire external
answer, including its store, even when its observation is silent. -/
theorem primitive_determinism_instance :
    Rules prog bumpSource (some bumpAnswer) .silent bumpTarget ∧
    InputDeterministic Rules prog bumpSource bumpTarget (some bumpAnswer) .silent := by
  have hr : Rules prog bumpSource (some bumpAnswer) .silent bumpTarget :=
    .primOk .nil rfl rfl .discard
  exact ⟨hr, rules_deterministic prog bumpSource bumpTarget _ .silent hr⟩

/-- Instance (laws `rules_sound`, `rules_complete`). Both agreement directions use the same
admitted primitive answer; no oracle is selected by these laws. -/
theorem primitive_agreement_instance :
    Dispatches step prog bumpSource (some bumpAnswer) .silent bumpTarget ∧
    Rules prog bumpSource (some bumpAnswer) .silent bumpTarget := by
  have hr : Rules prog bumpSource (some bumpAnswer) .silent bumpTarget :=
    .primOk .nil rfl rfl .discard
  have hd := rules_sound prog bumpSource _ .silent bumpTarget hr
  exact ⟨hd, rules_complete prog bumpSource _ .silent bumpTarget hd⟩

/-- Representation lemma. Absence of a transition agrees too, for any fixed input. This
includes stuck operands, lifecycle refusals, rejected answers and refused continuations.
No totality or rollback claim is made. -/
theorem no_transition_agreement (P : Program O) (c : Config O)
    (a : Option (Answer O.Val O.Event O.Store)) :
    (¬ ∃ l c', Rules P c a l c') ↔ (¬ ∃ l c', AnswerStep P c a l c') := by
  simp only [rules_iff_answerStep]

/-- Instance. A moved local is stuck with populated state. Both semantics have no transition. -/
theorem stuck_agreement_instance :
    let c : Config ops := ⟨.exec (.ret (.var x) s), live, initial, suffix⟩
    (¬ ∃ a l c', Rules prog c a l c') ∧ (¬ ∃ a l c', AnswerStep prog c a l c') := by
  dsimp
  have hn : ∀ a l c', ¬ AnswerStep prog
      ⟨.exec (.ret (.var x) s), live, initial, suffix⟩ a l c' := by
    intro a l c' h
    cases h with
    | silent hs => simp [step, onRes, ops, evalE, live, initial,
        DState.top, x, y, p, t, AList.lookup] at hs
    | prim hs _ _ => simp [step, onRes, ops, evalE, live, initial,
        DState.top, x, y, p, t, AList.lookup] at hs
  constructor
  · rintro ⟨a, l, c', h⟩
    exact hn a l c' ((rules_iff_answerStep _ _ _ _ _).mp h)
  · rintro ⟨a, l, c', h⟩; exact hn a l c' h

/-- Instance support. A toy lifecycle adapter refuses binding alone; this is unsafe
stopping, as decided in `docs/decisions.md#decided-2026-11-13-lifecycle-refusal`. -/
abbrev refusing : Operands := { ops with life := fun e st => match e with
  | .bind _ => none
  | _ => some (DState.life e st) }

/-- Instance. Lifecycle binding refusal has neither a silent nor primitive transition in
either semantics. It does not become a trap. -/
theorem refusal_agreement_instance :
    let P : Program refusing := ⟨[]⟩
    let c : Config refusing := ⟨.exec (.bind x (.lit 9) s), live, initial, [.seq .skip]⟩
    (¬ ∃ a l c', Rules P c a l c') ∧ (¬ ∃ a l c', AnswerStep P c a l c') := by
  dsimp
  have hn : ∀ a l c', ¬ AnswerStep (⟨[]⟩ : Program refusing)
      ⟨.exec (.bind x (.lit 9) s), live, initial, [.seq .skip]⟩ a l c' := by
    intro a l c' h
    cases h with
    | silent hs => simp [step, onRes, refusing, ops, evalE] at hs
    | prim hs _ _ => simp [step, onRes, refusing, ops, evalE] at hs
  constructor
  · rintro ⟨a, l, c', h⟩
    exact hn a l c' ((rules_iff_answerStep _ _ _ _ _).mp h)
  · rintro ⟨a, l, c', h⟩; exact hn a l c' h

/-- Representation lemma. A refused dispatcher has no answer-indexed transition, and hence
no independent rule. This includes final configurations as well as unsafe stopping. -/
theorem no_dispatch_no_rule {P : Program O} {c : Config O} (hs : step P c = none) :
    (¬ ∃ a l c', Rules P c a l c') ∧ (¬ ∃ a l c', AnswerStep P c a l c') := by
  have hn : ∀ a l c', ¬ AnswerStep P c a l c' := by
    intro a l c' h
    cases h with
    | silent h => rw [hs] at h; cases h
    | prim h _ _ => rw [hs] at h; cases h
  constructor
  · rintro ⟨a, l, c', h⟩
    exact hn a l c' ((rules_iff_answerStep _ _ _ _ _).mp h)
  · rintro ⟨a, l, c', h⟩; exact hn a l c' h

/-- Instance. Explicit lifecycle refusal control remains stopped with populated state. -/
theorem refused_control_instance :
    let P : Program refusing := ⟨[]⟩
    let c : Config refusing := ⟨.refused, live, initial, [.seq .skip]⟩
    (¬ ∃ a l c', Rules P c a l c') ∧ (¬ ∃ a l c', AnswerStep P c a l c') :=
  no_dispatch_no_rule rfl

/-- Instance. A contract-admitted log answer is still refused if its destination binding
is refused. Its event is not accepted as a semantic transition by either semantics. -/
theorem continuation_refusal_instance :
    let P : Program refusing := ⟨[]⟩
    let c : Config refusing := ⟨.exec (.prim (some x) .log [.lit 7] s), live, initial, [.seq .skip]⟩
    let a : Answer Int Int DState := ⟨some 7, .ok 0, initial⟩
    refusing.allows .log [7] initial a = true ∧
    (¬ ∃ l c', Rules P c (some a) l c') ∧
    (¬ ∃ l c', AnswerStep P c (some a) l c') := by
  dsimp
  have hn : ∀ l c', ¬ AnswerStep (⟨[]⟩ : Program refusing)
      ⟨.exec (.prim (some x) .log [.lit 7] s), live, initial, [.seq .skip]⟩
      (some ⟨some 7, .ok 0, initial⟩) l c' := by
    intro l c' h
    cases h with
    | prim hs _ hk =>
      simp only [step, onRes, evalArgs, refusing, ops, evalE, Res.bind, Res.map,
        Option.some.injEq, Out.request.injEq] at hs
      obtain ⟨rfl, rfl, rfl, rfl, rfl⟩ := hs
      cases hk
  refine ⟨rfl, ?_, ?_⟩
  · rintro ⟨l, c', h⟩
    exact hn l c' ((rules_iff_answerStep _ _ _ _ _).mp h)
  · rintro ⟨l, c', h⟩; exact hn l c' h

end DeterminismEvidence


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

namespace LifecycleEvidence
open Demo

/-- Instance support. Nonempty locals, moved-local instrumentation and a caller activation. -/
def live : AList Var Int := [(x, 17), (y, 29)]
/-- Instance support. The counter and both activation records must survive refusal. -/
def state : DState := ⟨41, [[y], [p]]⟩
/-- Instance support. Pending work makes an illicit continuation observable. -/
def tail : List (Frame O) := [.seq (.abort s)]
/-- Instance support. Refusing all hooks is a toy ledger, not a checker guarantee. -/
abbrev refusing : Operands := { ops with life := fun _ _ => none }
/-- Instance support. A populated callee with a real parameter and deferred body work. -/
def fn : Fn refusing := ⟨[(p, .tok)], .unit, ⟨73⟩, .prim none .log [.var p] s⟩
/-- Instance support. Unique lookup uses the shared numeric/name-independent table. -/
def program : Program refusing := ⟨[(eatF, fn)]⟩

/-- Planted support. The selected constructor is the sole rule changed by each mutant. -/
inductive Site where
  | bind | assign | call | scope | exitScope | exitTest | test
  | returnExplicit | returnImplicit | leaveNormal | leaveReturn | primitive
  deriving DecidableEq

/-- Planted support. Classifies the admission boundary without executing it. -/
def boundary (c : Config O) : Option Site := match c.ctrl, c.frames with
  | .exec (.bind ..), _ => some .bind
  | .exec (.assign ..), _ => some .assign
  | .exec (.call ..), _ => some .call
  | .exec (.prim ..), _ => some .primitive
  | .done, .scope _ _ [] :: _ => some .scope
  | .exiting _, .scope _ _ [] :: _ => some .exitScope
  | .exiting _, .test .. :: _ => some .exitTest
  | .done, .test .. :: _ => some .test
  | .exiting (.ret _), .ret .. :: _ => some .returnExplicit
  | .done, .ret .. :: _ => some .returnImplicit
  | .done, .functionEnd :: _ => some .leaveNormal
  | .exiting (.ret _), .functionEnd :: _ => some .leaveReturn
  | _, _ => none

/-- Planted. Add a silent transition only at a refused selected boundary. Other rules and all
successful transitions are unchanged. This violates unsafe stopping, without changing operand
evaluation or the lifecycle hook itself. -/
def added (which : Site) (P : Program O) (c : Config O) : Option (Out O) :=
  if boundary c = some which then
    match step P c with
    | none => some (.next c)
    | out => out
  else step P c

/-- Planted. Delete only the selected boundary's dispatch; all other rules are unchanged. -/
def deleted (which : Site) (P : Program O) (c : Config O) : Option (Out O) :=
  if boundary c = some which then none else step P c

/-- Helper. A concrete added transition falsifies the refusal equivalence. All uses of the mutated
dispatch go through E3's shared Dispatches adapter. -/
private theorem added_counterexample (which : Site) (P : Program O) (c : Config O)
    (hs : boundary c = some which) (hn : step P c = none) (refusal : Prop) (hr : refusal) :
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (added which) P c a l c') c ↔ refusal) := by
  intro h
  exact h.mpr hr .silent c ⟨none, .silent (by simp [added, hs, hn])⟩

/-- Helper. A deleted dispatch has no transition under any answer. -/
private theorem deleted_blocked (which : Site) (P : Program O) (c : Config O)
    (hs : boundary c = some which) :
    Blocked (fun c l c' => ∃ a, Dispatches (deleted which) P c a l c') c := by
  rintro l c' ⟨a, h⟩
  cases h <;> simp [deleted, hs] at *

/-- Instance support. Successful move has observable instrumentation before refusal. -/
def moved : DState := ⟨41, [[x, y], [p]]⟩

/-- Instance (law `bind_refused`). Populated state actually reaches the refused hook; the source
remains blocked, with operand/lookup premises satisfied. -/
theorem bind_instance :
    (refusing.eval live state (.move x) = .ok (17, moved)) ∧ Blocked (Step program) (⟨.exec (.bind p (.move x) s), live, state, tail⟩ : Config refusing) := by
  exact ⟨by decide, (bind_refused program p (.move x) s live state moved 17 tail rfl).mpr rfl⟩

/-- Planted (law `bind_refused`). Add only the `bind` boundary transition. The original law's
premises still hold and its equivalence fails in the kernel. -/
theorem bind_planted : (refusing.eval live state (.move x) = .ok (17, moved)) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (added .bind) program c a l c')
      (⟨.exec (.bind p (.move x) s), live, state, tail⟩ : Config refusing) ↔ refusing.life (.bind p) moved = none) := by
  exact ⟨by decide, added_counterexample .bind program _ rfl rfl _ rfl⟩

/-- Instance (law `assign_refused`). Populated state actually reaches the refused hook; the source
remains blocked, with operand/lookup premises satisfied. -/
theorem assign_instance :
    (refusing.eval live state (.move x) = .ok (17, moved) ∧ AList.lookup live x = some 17) ∧ Blocked (Step program) (⟨.exec (.assign x (.move x) s), live, state, tail⟩ : Config refusing) := by
  exact ⟨by decide, (assign_refused program x (.move x) s live state moved 17 17 tail rfl rfl).mpr rfl⟩

/-- Planted (law `assign_refused`). Add only the `assign` boundary transition. The original law's
premises still hold and its equivalence fails in the kernel. -/
theorem assign_planted : (refusing.eval live state (.move x) = .ok (17, moved) ∧ AList.lookup live x = some 17) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (added .assign) program c a l c')
      (⟨.exec (.assign x (.move x) s), live, state, tail⟩ : Config refusing) ↔ refusing.life (.bind x) moved = none) := by
  exact ⟨by decide, added_counterexample .assign program _ rfl rfl _ rfl⟩

/-- Instance (law `call_refused`). Populated state actually reaches the refused hook; the source
remains blocked, with operand/lookup premises satisfied. -/
theorem call_instance :
    (evalArgs refusing live state [.move x] = .ok ([17], moved) ∧ AList.lookup program.fns eatF = some fn ∧ fn.params.length = ([17] : List Int).length) ∧ Blocked (Step program) (⟨.exec (.call (some y) eatF [.move x] s), live, state, tail⟩ : Config refusing) := by
  exact ⟨⟨rfl, rfl, rfl⟩, (call_refused program (some y) eatF [.move x] s live state moved [17] fn tail rfl rfl rfl).mpr rfl⟩

/-- Planted (law `call_refused`). Add only the `call` boundary transition. The original law's
premises still hold and its equivalence fails in the kernel. -/
theorem call_planted : (evalArgs refusing live state [.move x] = .ok ([17], moved) ∧ AList.lookup program.fns eatF = some fn ∧ fn.params.length = ([17] : List Int).length) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (added .call) program c a l c')
      (⟨.exec (.call (some y) eatF [.move x] s), live, state, tail⟩ : Config refusing) ↔ refusing.life (.enter [p]) moved = none) := by
  exact ⟨⟨rfl, rfl, rfl⟩, added_counterexample .call program _ rfl rfl _ rfl⟩

/-- Instance (law `scope_drop_refused`). Populated state actually reaches the refused hook; the
source remains blocked, with operand/lookup premises satisfied. -/
theorem scope_instance :
    (True) ∧ Blocked (Step program) (⟨.done, live, state, .scope ⟨73⟩ [y] [] :: tail⟩ : Config refusing) := by
  exact ⟨by decide, (scope_drop_refused program ⟨73⟩ [y] live state tail).mpr rfl⟩

/-- Planted (law `scope_drop_refused`). Add only the `scope` boundary transition. The original law's
premises still hold and its equivalence fails in the kernel. -/
theorem scope_planted : (True) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (added .scope) program c a l c')
      (⟨.done, live, state, .scope ⟨73⟩ [y] [] :: tail⟩ : Config refusing) ↔ dropOut refusing [y] live state = none) := by
  exact ⟨by decide, added_counterexample .scope program _ rfl rfl _ rfl⟩

/-- Instance (law `exit_drop_refused`). Populated state actually reaches the refused hook; the
source remains blocked, with operand/lookup premises satisfied. -/
theorem exitScope_instance :
    (True) ∧ Blocked (Step program) (⟨.exiting (.ret 53), live, state, .scope ⟨73⟩ [y] [] :: tail⟩ : Config refusing) := by
  exact ⟨by decide, (exit_drop_refused program (.ret 53) ⟨73⟩ [y] live state tail).mpr rfl⟩

/-- Planted (law `exit_drop_refused`). Add only the `exitScope` boundary transition. The original
law's premises still hold and its equivalence fails in the kernel. -/
theorem exitScope_planted : (True) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (added .exitScope) program c a l c')
      (⟨.exiting (.ret 53), live, state, .scope ⟨73⟩ [y] [] :: tail⟩ : Config refusing) ↔ dropOut refusing [y] live state = none) := by
  exact ⟨by decide, added_counterexample .exitScope program _ rfl rfl _ rfl⟩

/-- Instance (law `exit_test_drop_refused`). Populated state actually reaches the refused hook; the
source remains blocked, with operand/lookup premises satisfied. -/
theorem exitTest_instance :
    (True) ∧ Blocked (Step program) (⟨.exiting (.ret 53), live, state, .test ⟨8⟩ .skip (.lit 1) (.abort s) s [y] :: tail⟩ : Config refusing) := by
  exact ⟨by decide, (exit_test_drop_refused program (.ret 53) [y] ⟨8⟩ .skip (.abort s) (.lit 1) s live state tail).mpr rfl⟩

/-- Planted (law `exit_test_drop_refused`). Add only the `exitTest` boundary transition. The
original law's premises still hold and its equivalence fails in the kernel. -/
theorem exitTest_planted : (True) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (added .exitTest) program c a l c')
      (⟨.exiting (.ret 53), live, state, .test ⟨8⟩ .skip (.lit 1) (.abort s) s [y] :: tail⟩ : Config refusing) ↔ dropOut refusing [y] live state = none) := by
  exact ⟨by decide, added_counterexample .exitTest program _ rfl rfl _ rfl⟩

/-- Instance (law `test_drop_refused`). Populated state actually reaches the refused hook; the
source remains blocked, with operand/lookup premises satisfied. -/
theorem test_instance :
    (refusing.eval live state (.move x) = .ok (17, moved) ∧ refusing.test (17 : Int) = some true) ∧ Blocked (Step program) (⟨.done, live, state, .test ⟨8⟩ .skip (.move x) (.abort s) s [y] :: tail⟩ : Config refusing) := by
  exact ⟨by decide, (test_drop_refused program ⟨8⟩ .skip (.abort s) (.move x) s [y] live state moved 17 true tail rfl rfl).mpr rfl⟩

/-- Planted (law `test_drop_refused`). Add only the `test` boundary transition. The original law's
premises still hold and its equivalence fails in the kernel. -/
theorem test_planted : (refusing.eval live state (.move x) = .ok (17, moved) ∧ refusing.test (17 : Int) = some true) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (added .test) program c a l c')
      (⟨.done, live, state, .test ⟨8⟩ .skip (.move x) (.abort s) s [y] :: tail⟩ : Config refusing) ↔ dropOut refusing [y] live moved = none) := by
  exact ⟨by decide, added_counterexample .test program _ rfl rfl _ rfl⟩

/-- Instance (law `return_refused`). Both return modes stop before restoring the populated caller.
-/
theorem return_instance :
    Blocked (Step program) (⟨.exiting (.ret 53), live, state, .ret (some x) [(p, 67)] :: tail⟩ : Config refusing) ∧
    Blocked (Step program) (⟨.done, live, state, .ret (some x) [(p, 67)] :: tail⟩ : Config refusing) :=
  ⟨(return_refused program (some x) 53 [(p, 67)] live state tail).1.mpr rfl, (return_refused program (some x) 53 [(p, 67)] live state tail).2.mpr rfl⟩

/-- Planted (law `return_refused`). Only `returnExplicit` is added; the same mutated relation
replaces both occurrences in the conjunction. -/
theorem returnExplicit_planted :
    ¬ ((Blocked (fun c l c' => ∃ a, Dispatches (added .returnExplicit) program c a l c') (⟨.exiting (.ret 53), live, state, .ret (some x) [(p, 67)] :: tail⟩ : Config refusing) ↔ bindLife refusing (some x) state = none) ∧
       (Blocked (fun c l c' => ∃ a, Dispatches (added .returnExplicit) program c a l c') (⟨.done, live, state, .ret (some x) [(p, 67)] :: tail⟩ : Config refusing) ↔ bindLife refusing (some x) state = none)) := by
  intro h
  exact added_counterexample .returnExplicit program (⟨.exiting (.ret 53), live, state, .ret (some x) [(p, 67)] :: tail⟩ : Config refusing) rfl rfl _ rfl h.1

/-- Planted (law `return_refused`). Only `returnImplicit` is added; the same mutated relation
replaces both occurrences in the conjunction. -/
theorem returnImplicit_planted :
    ¬ ((Blocked (fun c l c' => ∃ a, Dispatches (added .returnImplicit) program c a l c') (⟨.exiting (.ret 53), live, state, .ret (some x) [(p, 67)] :: tail⟩ : Config refusing) ↔ bindLife refusing (some x) state = none) ∧
       (Blocked (fun c l c' => ∃ a, Dispatches (added .returnImplicit) program c a l c') (⟨.done, live, state, .ret (some x) [(p, 67)] :: tail⟩ : Config refusing) ↔ bindLife refusing (some x) state = none)) := by
  intro h
  exact added_counterexample .returnImplicit program (⟨.done, live, state, .ret (some x) [(p, 67)] :: tail⟩ : Config refusing) rfl rfl _ rfl h.2

/-- Instance (law `leave_refused`). Both return modes stop before restoring the populated caller. -/
theorem leave_instance :
    Blocked (Step program) (⟨.exiting (.ret 53), live, state, .functionEnd :: tail⟩ : Config refusing) ∧
    Blocked (Step program) (⟨.done, live, state, .functionEnd :: tail⟩ : Config refusing) :=
  ⟨(leave_refused program 53 live state tail).2.mpr rfl, (leave_refused program 53 live state tail).1.mpr rfl⟩

/-- Planted (law `leave_refused`). Only `leaveReturn` is added; the same mutated relation replaces
both occurrences in the conjunction. -/
theorem leaveReturn_planted :
    ¬ ((Blocked (fun c l c' => ∃ a, Dispatches (added .leaveReturn) program c a l c') (⟨.exiting (.ret 53), live, state, .functionEnd :: tail⟩ : Config refusing) ↔ refusing.life .leave state = none) ∧
       (Blocked (fun c l c' => ∃ a, Dispatches (added .leaveReturn) program c a l c') (⟨.done, live, state, .functionEnd :: tail⟩ : Config refusing) ↔ refusing.life .leave state = none)) := by
  intro h
  exact added_counterexample .leaveReturn program (⟨.exiting (.ret 53), live, state, .functionEnd :: tail⟩ : Config refusing) rfl rfl _ rfl h.1

/-- Planted (law `leave_refused`). Only `leaveNormal` is added; the same mutated relation replaces
both occurrences in the conjunction. -/
theorem leaveNormal_planted :
    ¬ ((Blocked (fun c l c' => ∃ a, Dispatches (added .leaveNormal) program c a l c') (⟨.exiting (.ret 53), live, state, .functionEnd :: tail⟩ : Config refusing) ↔ refusing.life .leave state = none) ∧
       (Blocked (fun c l c' => ∃ a, Dispatches (added .leaveNormal) program c a l c') (⟨.done, live, state, .functionEnd :: tail⟩ : Config refusing) ↔ refusing.life .leave state = none)) := by
  intro h
  exact added_counterexample .leaveNormal program (⟨.done, live, state, .functionEnd :: tail⟩ : Config refusing) rfl rfl _ rfl h.2

end LifecycleEvidence



namespace LifecycleEvidence
open Demo

/-- Instance (law `enter_refused`). A real parameter list is refused at entry. -/
theorem enter_instance : enter fn [17] state = none ∧
    (enter fn [17] state = none ↔ refusing.life (.enter [p]) state = none) :=
  ⟨rfl, enter_refused fn [17] state⟩

/-- Planted. Remove only entry's lifecycle admission; preserve the standard frame layout. -/
def uncheckedEnter (f : Fn O) (vs : List O.Val) (st : O.Store) : Option (Config O) :=
  some ⟨.exec f.body, (f.params.map Prod.fst).zip vs, st,
    [.scope f.block (f.params.map Prod.fst) [], .functionEnd]⟩

/-- Planted (law `enter_refused`). A populated entry satisfies the premises;
unadmitted entry contradicts the refusal equivalence. -/
theorem enter_planted : ¬ (uncheckedEnter fn [17] state = none ↔ refusing.life (.enter [p]) state = none) := by
  intro h
  have := h.mpr rfl
  cases this

/-- Instance (law `entry_refusal_report`). Original state, parameter values and all seven units of
fuel remain, even though the body would issue an external request. -/
theorem entry_report_instance :
    runFn program env 7 fn [17] state = some ⟨[], .stuck,
      ⟨.refused, [(p, 17)], state, [.scope fn.block [p] [], .functionEnd]⟩, 7⟩ :=
  entry_refusal_report program env 7 fn [17] state rfl rfl

/-- Planted. Replace only refused entry's control with the body control. The ordinary driver and
successful-entry behavior are reused unchanged. -/
def bypassEntry (P : Program O) (ρ : Oracle O) (fuel : Nat) (f : Fn O)
    (vs : List O.Val) (st : O.Store) : Option (Report O) :=
  if f.params.length = vs.length then
    match enter f vs st with
    | none => (uncheckedEnter f vs st).map (run P ρ fuel)
    | some _ => runFn P ρ fuel f vs st
  else none

/-- Planted (law `entry_refusal_report`). Both premises hold; the unadmitted entry spends fuel and
changes the retained boundary. -/
theorem entry_report_planted : fn.params.length = ([17] : List Int).length ∧
    refusing.life (.enter [p]) state = none ∧
    bypassEntry program env 7 fn [17] state ≠ some ⟨[], .stuck,
      ⟨.refused, [(p, 17)], state, [.scope fn.block [p] [], .functionEnd]⟩, 7⟩ := by
  refine ⟨rfl, rfl, fun h => ?_⟩
  have := congrArg (fun r => r.map Report.fuel) h
  change some 6 = some 7 at this
  cases this

/-- Instance support. A successful answer carries a real event and unchanged external store. -/
def answer : Answer Int Int DState := ⟨some 17, .ok 0, moved⟩
/-- Instance support. The primitive evaluates a move before requesting its answer. -/
def request : Config refusing :=
  ⟨.exec (.prim (some p) .log [.move x] s), live, state, tail⟩
/-- Instance support. The real request continuation, with result initialization. -/
def continuation (a : Answer Int Int DState) : Option (Config refusing) := match a.result with
  | .ok v => (bindLife refusing (some p) a.store).map fun st =>
      ⟨.done, bindDst (some p) v live, st, tail⟩
  | .error kd => some ⟨.trapped kd s, live, a.store, tail⟩

/-- Instance (law `continuation_refused`). All request premises hold, and refusal retains the
earlier history, original operand state and unused budget. -/
theorem continuation_instance : request.final? = none ∧ request.ctrl ≠ .refused ∧
    step program request = some (.request .log [17] s moved continuation) ∧
    env [91] .log [17] s moved = some answer ∧
    refusing.allows .log [17] moved answer = true ∧
    (runFrom program env 1 [91] request = ⟨[91], .stuck, request, 1⟩ ∧
      continuation answer = none) := by
  refine ⟨rfl, (by intro h; cases h), (by simp only [step, request, (show evalArgs refusing live state [.move x] = .ok ([17], moved) from rfl), onRes]; apply congrArg some; apply congrArg (Out.request (O := refusing) .log [17] s moved); funext a; cases hr : a.result <;> simp [continuation, hr, bindLife, refusing]), rfl, by decide, ?_⟩
  exact ⟨(continuation_refused program env [91] request .log [17] s moved continuation
    answer rfl (by intro h; cases h) (by simp only [step, request, (show evalArgs refusing live state [.move x] = .ok ([17], moved) from rfl), onRes]; apply congrArg some; apply congrArg (Out.request (O := refusing) .log [17] s moved); funext a; cases hr : a.result <;> simp [continuation, hr, bindLife, refusing]) rfl (by decide)).mpr rfl, rfl⟩

/-- Planted. Only refused response continuation handling is changed: incorrectly append its event
while retaining the requesting machine. All recursive branches use the same mutant. -/
def acceptRefused (P : Program O) (ρ : Oracle O) : Nat → List O.Event → Config O → Report O
  | fuel, hist, c =>
    match c.ctrl, c.final?, fuel with
    | .refused, _, _ => ⟨hist, .stuck, c, fuel⟩
    | _, some f, _ => ⟨hist, .final f, c, fuel⟩
    | _, none, 0 => ⟨hist, .outOfFuel, c, 0⟩
    | _, none, n + 1 =>
      match step P c with
      | none => ⟨hist, .stuck, c, n + 1⟩
      | some (.next c') => acceptRefused P ρ n hist c'
      | some (.request p vs site st k) =>
        match ρ hist p vs site st with
        | none => ⟨hist, .noAnswer, c, n + 1⟩
        | some a =>
          if O.allows p vs st a then
            match k a with
            | some c' => acceptRefused P ρ n (hist ++ a.event.toList) c'
            | none => ⟨hist ++ a.event.toList, .stuck, c, n + 1⟩
          else ⟨hist, .badAnswer, c, n + 1⟩


/-- Planted (law `continuation_refused`). The admitted answer and all other premises are the
Instance's concrete witnesses; the mutated driver's conclusion fails. -/
theorem continuation_planted : request.final? = none ∧ request.ctrl ≠ .refused ∧
    step program request = some (.request .log [17] s moved continuation) ∧
    env [91] .log [17] s moved = some answer ∧
    refusing.allows .log [17] moved answer = true ∧
    ¬ (acceptRefused program env 1 [91] request = ⟨[91], .stuck, request, 1⟩ ↔
      continuation answer = none) := by
  refine ⟨rfl, (by intro h; cases h), (by simp only [step, request, (show evalArgs refusing live state [.move x] = .ok ([17], moved) from rfl), onRes]; apply congrArg some; apply congrArg (Out.request (O := refusing) .log [17] s moved); funext a; cases hr : a.result <;> simp [continuation, hr, bindLife, refusing]), rfl, by decide, fun h => ?_⟩
  have := congrArg Report.events (h.mpr rfl)
  change [91, 17] = [91] at this
  cases this

/-- Instance (law `prim_bind_refused`). Refusal is at the answer store after operand move. -/
theorem primitive_bind_instance :
    evalArgs refusing live state [.move x] = .ok ([17], moved) ∧
    env [91] .log [17] s moved = some answer ∧
    refusing.allows .log [17] moved answer = true ∧ answer.result = .ok 0 ∧
    (runFrom program env 1 [91] request = ⟨[91], .stuck, request, 1⟩ ∧
      refusing.life (.bind p) answer.store = none) :=
  ⟨rfl, rfl, by decide, rfl,
    ⟨(prim_bind_refused program env [91] p .log [.move x] s live state moved [17] tail
      answer 0 rfl rfl (by decide) rfl).mpr rfl, rfl⟩⟩

/-- Planted (law `prim_bind_refused`). Same premises as above; accepting the refused binding's event
breaks the exact boundary report. -/
theorem primitive_bind_planted :
    evalArgs refusing live state [.move x] = .ok ([17], moved) ∧
    env [91] .log [17] s moved = some answer ∧
    refusing.allows .log [17] moved answer = true ∧ answer.result = .ok 0 ∧
    ¬ (acceptRefused program env 1 [91] request = ⟨[91], .stuck, request, 1⟩ ↔
      refusing.life (.bind p) answer.store = none) := by
  refine ⟨rfl, rfl, by decide, rfl, fun h => ?_⟩
  have := congrArg Report.events (h.mpr rfl)
  change [91, 17] = [91] at this
  cases this

/-- Helper. Every allowed demo log answer succeeds; binding then refuses. -/
private theorem all_answers_refuse : ∀ a, refusing.allows .log [17] moved a = true →
    ∃ v, a.result = .ok v ∧ refusing.life (.bind p) a.store = none := by
  intro a ha
  cases hev : a.event with
  | none => simp [refusing, ops, hev] at ha
  | some ev =>
    cases hr : a.result with
    | error _ => simp [refusing, ops, hev, hr] at ha
    | ok v => exact ⟨v, rfl, rfl⟩

/-- Instance (law `prim_bind_step`). The universal contract premise is populated by answer, and the
actual request has no transition. -/
theorem primitive_step_instance :
    refusing.allows .log [17] moved answer = true ∧
    (Blocked (Step program) request ∧ ∀ a, refusing.allows .log [17] moved a = true →
      ∃ v, a.result = .ok v ∧ refusing.life (.bind p) a.store = none) :=
  ⟨by decide, (prim_bind_step program p .log [.move x] s live state moved [17] tail rfl).mpr
    all_answers_refuse, all_answers_refuse⟩

/-- Planted. Remove only successful primitive-result admission, leaving error responses and every
other statement unchanged. -/
def bypassPrimitiveBind (P : Program O) (c : Config O) : Option (Out O) :=
  match c.ctrl with
  | .exec (.prim dst p args site) => onRes (evalArgs O c.locals c.store args)
      site c.locals c.store c.frames fun vs st =>
        some (.request p vs site st fun a => match a.result with
          | .ok v => some ⟨.done, bindDst dst v c.locals, a.store, c.frames⟩
          | .error kd => some ⟨.trapped kd site, c.locals, a.store, c.frames⟩)
  | _ => step P c

/-- Planted (law `prim_bind_step`). An allowed answer supplies a real added labeled transition
despite the universal refusal premise. -/
theorem primitive_step_planted :
    evalArgs refusing live state [.move x] = .ok ([17], moved) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches bypassPrimitiveBind program c a l c') request ↔
      ∀ a, refusing.allows .log [17] moved a = true →
        ∃ v, a.result = .ok v ∧ refusing.life (.bind p) a.store = none) := by
  refine ⟨rfl, fun h => ?_⟩
  exact h.mpr all_answers_refuse answer.label _
    ⟨some answer, .prim rfl (by decide) rfl⟩

/-- Instance support. An error-capable contract and refusing result ledger. -/
abbrev errorOps : Operands := { refusing with allows := fun _ _ _ _ => true }
/-- Instance support. A visible error answer changes the outside store. -/
def errorAnswer : Answer Int Int DState := ⟨some 93, .error .abort, ⟨84, [[x], [p]]⟩⟩
/-- Instance support. Pending frames and moved argument distinguish the retained stores. -/
def errorRequest : Config errorOps :=
  ⟨.exec (.prim (some p) .log [.move x] s), live, state, tail⟩

/-- Instance (law `prim_error_bypasses_bind`). Error delivery keeps the answer's event and store,
even though successful-result initialization would refuse. -/
theorem primitive_error_instance :
    Step (⟨[]⟩ : Program errorOps) errorRequest errorAnswer.label
      ⟨.trapped .abort s, live, errorAnswer.store, tail⟩ :=
  prim_error_bypasses_bind (O := errorOps) ⟨[]⟩ p .log [.move x] s live state moved [17] tail
    errorAnswer .abort rfl rfl rfl

/-- Planted. Consult result admission only on error responses, an extra obligation. Successful
answers and other rules retain their ordinary behavior. -/
def bindErrors (P : Program O) (c : Config O) : Option (Out O) :=
  match c.ctrl with
  | .exec (.prim dst p args site) => onRes (evalArgs O c.locals c.store args)
      site c.locals c.store c.frames fun vs st =>
        some (.request p vs site st fun a => match a.result with
          | .ok v => (bindLife O dst a.store).map fun st =>
              ⟨.done, bindDst dst v c.locals, st, c.frames⟩
          | .error kd => (bindLife O dst a.store).map fun st =>
              ⟨.trapped kd site, c.locals, st, c.frames⟩)
  | _ => step P c

/-- Planted (law `prim_error_bypasses_bind`). All premises hold, but the error-binding mutant loses
the actual labeled trap transition. -/
theorem primitive_error_planted :
    evalArgs errorOps live state [.move x] = .ok ([17], moved) ∧
    errorOps.allows .log [17] moved errorAnswer = true ∧ errorAnswer.result = .error .abort ∧
    ¬ (∃ a, Dispatches bindErrors (⟨[]⟩ : Program errorOps) errorRequest a errorAnswer.label
      ⟨.trapped .abort s, live, errorAnswer.store, tail⟩) := by
  refine ⟨rfl, rfl, rfl, ?_⟩
  generalize hl : (@Answer.label errorOps errorAnswer) = label
  rintro ⟨a, h⟩
  cases h with
  | silent hs => cases hs
  | prim hs ha hk =>
    simp only [bindErrors, errorRequest, (show evalArgs errorOps live state [.move x] = .ok ([17], moved) from rfl), onRes] at hs
    simp only [Option.some.injEq, Out.request.injEq] at hs
    obtain ⟨rfl, rfl, rfl, rfl, hcont⟩ := hs
    cases hcont
    rename_i response
    cases hr : response.result <;> simp [hr, bindLife, errorOps, refusing] at hk

end LifecycleEvidence

/-- Instance support. Admission is independently checked against the real demo table; validator
acceptance is not used to manufacture the protocol's obligations. -/
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

namespace DestructionEvidence
open Demo LifecycleEvidence

/-- Instance (Representation lemma `destroy_not_copy`). The validator accepts the non-Copy singleton
consuming signature, and rejects a Copy type. -/
theorem validator_instance : validDestroyFn prog .tok eatF = true ∧
    validDestroyFn prog .int eatF = false := by decide

/-- Instance (Representation lemma `destroyCall_sig`). The checked builder keeps the moving operand
and its source site, using the same function table as execution. -/
theorem builder_instance : checkedDestroyCall prog destroyProtocol.impl .tok (.move x) s =
    some ⟨eatF, [.move x], s⟩ := rfl

/-- Instance (law `destroyCall_enters`). A populated caller moves its token into the ordinary
singleton-parameter destructor; the updated instrumentation enters a new activation. -/
theorem enters_instance :
    ∃ d, destroyProtocol.destroyCall .tok (.move x) s = some d ∧
      eatFn.result = ops.unitTy ∧ ops.isCopy .tok = false ∧
      Step prog ⟨.exec d.stmt, live, state, tail⟩ .silent
        ⟨.exec eatFn.body, [(p, 17)], ⟨41, [[], [x, y], [p]]⟩,
          .scope eatFn.block [p] [] :: .functionEnd :: .ret none live :: tail⟩ :=
  destroyCall_enters prog destroyProtocol .tok eatF eatFn p (.move x) s live state
    moved ⟨41, [[], [x, y], [p]]⟩ 17 tail rfl rfl rfl rfl rfl

/-- Planted. Omit activation entry only for ordinary calls. All other machine rules remain
unchanged; the shared dispatcher adapter substitutes it in the entire conclusion. -/
def noCall (P : Program O) (c : Config O) : Option (Out O) := match c.ctrl with
  | .exec (.call ..) => none
  | _ => step P c

/-- Planted (law `destroyCall_enters`). Independent admission, lookup, consuming signature, moved
operand and successful lifecycle premises all still hold. Removing the ordinary call rule destroys
the existential conclusion, for every proposed deferred call. -/
theorem enters_planted :
    destroyProtocol.impl .tok = some eatF ∧ AList.lookup prog.fns eatF = some eatFn ∧
    eatFn.params = [(p, .tok)] ∧ ops.eval live state (.move x) = .ok (17, moved) ∧
    ops.life (.enter [p]) moved = some ⟨41, [[], [x, y], [p]]⟩ ∧
    ¬ (∃ d, destroyProtocol.destroyCall .tok (.move x) s = some d ∧
      eatFn.result = ops.unitTy ∧ ops.isCopy .tok = false ∧
      (∃ a, Dispatches noCall prog ⟨.exec d.stmt, live, state, tail⟩ a .silent
        ⟨.exec eatFn.body, [(p, 17)], ⟨41, [[], [x, y], [p]]⟩,
          .scope eatFn.block [p] [] :: .functionEnd :: .ret none live :: tail⟩)) := by
  refine ⟨rfl, rfl, rfl, rfl, rfl, ?_⟩
  rintro ⟨d, hd, _, _, a, hs⟩
  have he : d = ⟨eatF, [.move x], s⟩ := Option.some.inj hd.symm
  subst d
  generalize hl : (Label.silent : Label ops) = label at hs
  cases hs <;> simp [noCall, Deferred.stmt] at *

/-- Planted. Delete the admitted call builder only; execution and protocol admission are fixed. -/
def missingDestroyCall {O : Operands} {P : Program O} (D : DestroyProtocol P)
    (T : O.Ty) (_ : O.Expr) (_ : SiteId) : Option (Deferred O) :=
  (D.impl T).bind fun _ => none

/-- Planted (law `destroyCall_enters`). All original admission and evaluation premises hold;
the substituted builder cannot supply any call to the unchanged Step relation. -/
theorem builder_deleted :
    destroyProtocol.impl .tok = some eatF ∧ AList.lookup prog.fns eatF = some eatFn ∧
    eatFn.params = [(p, .tok)] ∧ ops.eval live state (.move x) = .ok (17, moved) ∧
    ops.life (.enter [p]) moved = some ⟨41, [[], [x, y], [p]]⟩ ∧
    ¬ (∃ d, missingDestroyCall destroyProtocol .tok (.move x) s = some d ∧
      eatFn.result = ops.unitTy ∧ ops.isCopy .tok = false ∧
      Step prog ⟨.exec d.stmt, live, state, tail⟩ .silent
        ⟨.exec eatFn.body, [(p, 17)], ⟨41, [[], [x, y], [p]]⟩,
          .scope eatFn.block [p] [] :: .functionEnd :: .ret none live :: tail⟩) := by
  refine ⟨rfl, rfl, rfl, rfl, rfl, ?_⟩
  rintro ⟨d, hd, _⟩; cases hd

/-- Instance (law `seqDestroyOrder_elem`). The first of three distinct elements is visited at its
index, while both neighbors and backing remain present. -/
theorem sequence_element_instance :
    (seqDestroyOrder 3)[0]? = some (.elem 0) := seqDestroyOrder_elem 3 0 (by decide)

/-- Planted. Reverse element order, preserving every element and the final backing. -/
def reversedSequence (n : Nat) : List Part :=
  ((List.range n).map Part.elem).reverse ++ [.storage]

/-- Planted (law `seqDestroyOrder_elem`). Index zero distinguishes ascending from reversed visits
with three populated elements. -/
theorem sequence_element_planted : 0 < 3 ∧
    (reversedSequence 3)[0]? ≠ some (.elem 0) := by decide

/-- Instance (law `seqDestroyOrder_storage`). Three elements precede the unique backing. -/
theorem sequence_storage_instance :
    (seqDestroyOrder 3).length = 4 ∧ (seqDestroyOrder 3)[3]? = some .storage :=
  seqDestroyOrder_storage 3

/-- Planted. Move backing release first, retaining element order and total population. -/
def earlySequenceStorage (n : Nat) : List Part := .storage :: (List.range n).map .elem

/-- Planted (law `seqDestroyOrder_storage`). Correct length cannot mask early release. -/
theorem sequence_storage_planted :
    ¬ ((earlySequenceStorage 3).length = 4 ∧
      (earlySequenceStorage 3)[3]? = some .storage) := by decide

/-- Instance (law `mapDestroyOrder_entry`). The first of three entries visits its key then value,
independently of both neighboring entries. -/
theorem map_entry_instance : (mapDestroyOrder 3)[0]? = some (.key 0) ∧
    (mapDestroyOrder 3)[1]? = some (.value 0) := mapDestroyOrder_entry 3 0 (by decide)

/-- Planted. Swap key and value within each entry; entry order and nodes remain unchanged. -/
def valueFirstMap (n : Nat) : List Part :=
  (List.range n).flatMap (fun i => [.value i, .key i]) ++ [.storage]

/-- Planted (law `mapDestroyOrder_entry`). Both occurrences use the same mutant. -/
theorem map_entry_planted : 1 < 3 ∧
    ¬ ((valueFirstMap 3)[2]? = some (.key 1) ∧
       (valueFirstMap 3)[3]? = some (.value 1)) := by decide

/-- Planted. Reverse only entry order, preserving key-before-value within each entry. -/
def descendingMap (n : Nat) : List Part :=
  (List.range n).reverse.flatMap (fun i => [.key i, .value i]) ++ [.storage]

/-- Planted (law `mapDestroyOrder_entry`). Three entries distinguish ascending key order from
descending order; within-entry ordering remains correct. -/
theorem map_order_planted : 0 < 3 ∧
    ¬ ((descendingMap 3)[0]? = some (.key 0) ∧
       (descendingMap 3)[1]? = some (.value 0)) := by decide

/-- Instance (law `mapDestroyOrder_storage`). Nodes follow all six key/value visits. -/
theorem map_storage_instance : (mapDestroyOrder 3).length = 7 ∧
    (mapDestroyOrder 3)[6]? = some .storage := mapDestroyOrder_storage 3

/-- Planted. Release nodes before entries; all key/value visits remain in their order. -/
def earlyMapStorage (n : Nat) : List Part :=
  .storage :: (List.range n).flatMap (fun i => [.key i, .value i])

/-- Planted (law `mapDestroyOrder_storage`). Both schedule occurrences are substituted; a correct
population does not excuse releasing nodes early. -/
theorem map_storage_planted : ¬ ((earlyMapStorage 3).length = 7 ∧
    (earlyMapStorage 3)[6]? = some .storage) := by decide

end DestructionEvidence

namespace LifecycleEvidence
open Demo

/-- Instance (law `bind_refused`). Successful admission is necessary as well: this populated example
progresses through the selected `bind` boundary. -/
theorem bind_accepted : (ops.eval live state (.move x) = .ok (17, moved)) ∧ ¬ Blocked (Step Demo.prog) (⟨.exec (.bind p (.move x) s), live, state, tail⟩ : Config ops) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun hb => ?_⟩
  have hn := (bind_refused Demo.prog p (.move x) s live state moved 17 tail rfl).mp hb
  cases hn

/-- Planted (law `bind_refused`). Delete only the accepted `bind` boundary. The original operand and
lookup premises hold; the converse refusal implication fails. -/
theorem bind_deleted : (ops.eval live state (.move x) = .ok (17, moved)) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (deleted .bind) Demo.prog c a l c')
      (⟨.exec (.bind p (.move x) s), live, state, tail⟩ : Config ops) ↔ ops.life (.bind p) moved = none) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun h => ?_⟩
  have hn := h.mp (deleted_blocked .bind Demo.prog (⟨.exec (.bind p (.move x) s), live, state, tail⟩ : Config ops) rfl)
  cases hn

/-- Instance (law `assign_refused`). Successful admission is necessary as well: this populated
example progresses through the selected `assign` boundary. -/
theorem assign_accepted : (ops.eval live state (.move x) = .ok (17, moved) ∧ AList.lookup live x = some 17) ∧ ¬ Blocked (Step Demo.prog) (⟨.exec (.assign x (.move x) s), live, state, tail⟩ : Config ops) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun hb => ?_⟩
  have hn := (assign_refused Demo.prog x (.move x) s live state moved 17 17 tail rfl rfl).mp hb
  cases hn

/-- Planted (law `assign_refused`). Delete only the accepted `assign` boundary. The original operand
and lookup premises hold; the converse refusal implication fails. -/
theorem assign_deleted : (ops.eval live state (.move x) = .ok (17, moved) ∧ AList.lookup live x = some 17) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (deleted .assign) Demo.prog c a l c')
      (⟨.exec (.assign x (.move x) s), live, state, tail⟩ : Config ops) ↔ ops.life (.bind x) moved = none) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun h => ?_⟩
  have hn := h.mp (deleted_blocked .assign Demo.prog (⟨.exec (.assign x (.move x) s), live, state, tail⟩ : Config ops) rfl)
  cases hn

/-- Instance (law `call_refused`). Successful admission is necessary as well: this populated example
progresses through the selected `call` boundary. -/
theorem call_accepted : (evalArgs ops live state [.move x] = .ok ([17], moved) ∧ AList.lookup Demo.prog.fns eatF = some eatFn ∧ eatFn.params.length = ([17] : List Int).length) ∧ ¬ Blocked (Step Demo.prog) (⟨.exec (.call (some y) eatF [.move x] s), live, state, tail⟩ : Config ops) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun hb => ?_⟩
  have hn := (call_refused Demo.prog (some y) eatF [.move x] s live state moved [17] eatFn tail rfl rfl rfl).mp hb
  cases hn

/-- Planted (law `call_refused`). Delete only the accepted `call` boundary. The original operand and
lookup premises hold; the converse refusal implication fails. -/
theorem call_deleted : (evalArgs ops live state [.move x] = .ok ([17], moved) ∧ AList.lookup Demo.prog.fns eatF = some eatFn ∧ eatFn.params.length = ([17] : List Int).length) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (deleted .call) Demo.prog c a l c')
      (⟨.exec (.call (some y) eatF [.move x] s), live, state, tail⟩ : Config ops) ↔ ops.life (.enter [p]) moved = none) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun h => ?_⟩
  have hn := h.mp (deleted_blocked .call Demo.prog (⟨.exec (.call (some y) eatF [.move x] s), live, state, tail⟩ : Config ops) rfl)
  cases hn

/-- Instance (law `scope_drop_refused`). Successful admission is necessary as well: this populated
example progresses through the selected `scope` boundary. -/
theorem scope_accepted : (True) ∧ ¬ Blocked (Step Demo.prog) (⟨.done, live, state, .scope ⟨73⟩ [y] [] :: tail⟩ : Config ops) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun hb => ?_⟩
  have hn := (scope_drop_refused Demo.prog ⟨73⟩ [y] live state tail).mp hb
  cases hn

/-- Planted (law `scope_drop_refused`). Delete only the accepted `scope` boundary. The original
operand and lookup premises hold; the converse refusal implication fails. -/
theorem scope_deleted : (True) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (deleted .scope) Demo.prog c a l c')
      (⟨.done, live, state, .scope ⟨73⟩ [y] [] :: tail⟩ : Config ops) ↔ dropOut ops [y] live state = none) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun h => ?_⟩
  have hn := h.mp (deleted_blocked .scope Demo.prog (⟨.done, live, state, .scope ⟨73⟩ [y] [] :: tail⟩ : Config ops) rfl)
  cases hn

/-- Instance (law `exit_drop_refused`). Successful admission is necessary as well: this populated
example progresses through the selected `exitScope` boundary. -/
theorem exitScope_accepted : (True) ∧ ¬ Blocked (Step Demo.prog) (⟨.exiting (.ret 53), live, state, .scope ⟨73⟩ [y] [] :: tail⟩ : Config ops) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun hb => ?_⟩
  have hn := (exit_drop_refused Demo.prog (.ret 53) ⟨73⟩ [y] live state tail).mp hb
  cases hn

/-- Planted (law `exit_drop_refused`). Delete only the accepted `exitScope` boundary. The original
operand and lookup premises hold; the converse refusal implication fails. -/
theorem exitScope_deleted : (True) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (deleted .exitScope) Demo.prog c a l c')
      (⟨.exiting (.ret 53), live, state, .scope ⟨73⟩ [y] [] :: tail⟩ : Config ops) ↔ dropOut ops [y] live state = none) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun h => ?_⟩
  have hn := h.mp (deleted_blocked .exitScope Demo.prog (⟨.exiting (.ret 53), live, state, .scope ⟨73⟩ [y] [] :: tail⟩ : Config ops) rfl)
  cases hn

/-- Instance (law `exit_test_drop_refused`). Successful admission is necessary as well: this
populated example progresses through the selected `exitTest` boundary. -/
theorem exitTest_accepted : (True) ∧ ¬ Blocked (Step Demo.prog) (⟨.exiting (.ret 53), live, state, .test ⟨8⟩ .skip (.lit 1) (.abort s) s [y] :: tail⟩ : Config ops) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun hb => ?_⟩
  have hn := (exit_test_drop_refused Demo.prog (.ret 53) [y] ⟨8⟩ .skip (.abort s) (.lit 1) s live state tail).mp hb
  cases hn

/-- Planted (law `exit_test_drop_refused`). Delete only the accepted `exitTest` boundary. The
original operand and lookup premises hold; the converse refusal implication fails. -/
theorem exitTest_deleted : (True) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (deleted .exitTest) Demo.prog c a l c')
      (⟨.exiting (.ret 53), live, state, .test ⟨8⟩ .skip (.lit 1) (.abort s) s [y] :: tail⟩ : Config ops) ↔ dropOut ops [y] live state = none) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun h => ?_⟩
  have hn := h.mp (deleted_blocked .exitTest Demo.prog (⟨.exiting (.ret 53), live, state, .test ⟨8⟩ .skip (.lit 1) (.abort s) s [y] :: tail⟩ : Config ops) rfl)
  cases hn

/-- Instance (law `test_drop_refused`). Successful admission is necessary as well: this populated
example progresses through the selected `test` boundary. -/
theorem test_accepted : (ops.eval live state (.move x) = .ok (17, moved) ∧ ops.test (17 : Int) = some true) ∧ ¬ Blocked (Step Demo.prog) (⟨.done, live, state, .test ⟨8⟩ .skip (.move x) (.abort s) s [y] :: tail⟩ : Config ops) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun hb => ?_⟩
  have hn := (test_drop_refused Demo.prog ⟨8⟩ .skip (.abort s) (.move x) s [y] live state moved 17 true tail rfl rfl).mp hb
  cases hn

/-- Planted (law `test_drop_refused`). Delete only the accepted `test` boundary. The original
operand and lookup premises hold; the converse refusal implication fails. -/
theorem test_deleted : (ops.eval live state (.move x) = .ok (17, moved) ∧ ops.test (17 : Int) = some true) ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (deleted .test) Demo.prog c a l c')
      (⟨.done, live, state, .test ⟨8⟩ .skip (.move x) (.abort s) s [y] :: tail⟩ : Config ops) ↔ dropOut ops [y] live moved = none) := by
  refine ⟨by repeat first | apply And.intro | exact True.intro | rfl, fun h => ?_⟩
  have hn := h.mp (deleted_blocked .test Demo.prog (⟨.done, live, state, .test ⟨8⟩ .skip (.move x) (.abort s) s [y] :: tail⟩ : Config ops) rfl)
  cases hn

end LifecycleEvidence



namespace LifecycleEvidence
open Demo

/-- Instance (law `refused_stops`). Refusal outranks zero-fuel exhaustion, retaining nonempty
history, locals, ownership state and pending work. -/
theorem refused_stop_instance : runFrom program env 0 [91] ⟨.refused, live, state, tail⟩ =
    ⟨[91], .stuck, ⟨.refused, live, state, tail⟩, 0⟩ :=
  refused_stops program env 0 [91] live state tail

/-- Planted. Change only explicit refusal's ending to normal completion. -/
def normalizeRefusal (P : Program O) (ρ : Oracle O) : Nat → List O.Event → Config O → Report O
  | fuel, hist, c =>
    match c.ctrl, c.final?, fuel with
    | .refused, _, _ => ⟨hist, .final .normal, c, fuel⟩
    | _, some f, _ => ⟨hist, .final f, c, fuel⟩
    | _, none, 0 => ⟨hist, .outOfFuel, c, 0⟩
    | _, none, n + 1 =>
      match step P c with
      | none => ⟨hist, .stuck, c, n + 1⟩
      | some (.next c') => normalizeRefusal P ρ n hist c'
      | some (.request p vs site st k) =>
        match ρ hist p vs site st with
        | none => ⟨hist, .noAnswer, c, n + 1⟩
        | some a =>
          if O.allows p vs st a then
            match k a with
            | some c' => normalizeRefusal P ρ n (hist ++ a.event.toList) c'
            | none => ⟨hist, .stuck, c, n + 1⟩
          else ⟨hist, .badAnswer, c, n + 1⟩


/-- Planted (law `refused_stops`). Populated refusal cannot be normalized, even when all other
fields of the boundary report are retained. -/
theorem refused_stop_planted :
    normalizeRefusal program env 0 [91] ⟨.refused, live, state, tail⟩ ≠
      ⟨[91], .stuck, ⟨.refused, live, state, tail⟩, 0⟩ := by
  intro h
  have := congrArg Report.ending h
  cases this

/-- Instance (law `test_drop_refused`). False conditions also admit temporary drop; refusal cannot
silently take the loop-exit branch. -/
theorem false_test_instance :
    refusing.eval live state (.lit 0) = .ok (0, state) ∧ refusing.test (0 : Int) = some false ∧
    Blocked (Step program)
      ⟨.done, live, state, .test ⟨8⟩ .skip (.lit 0) (.abort s) s [y] :: tail⟩ :=
  ⟨rfl, rfl, (test_drop_refused program ⟨8⟩ .skip (.abort s) (.lit 0) s [y] live state state
    0 false tail rfl rfl).mpr rfl⟩

/-- Planted (law `test_drop_refused`). Add only a false-test boundary transition; the original
evaluated-value and Boolean-test premises remain true. -/
theorem false_test_planted : refusing.eval live state (.lit 0) = .ok (0, state) ∧
    refusing.test (0 : Int) = some false ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (added .test) program c a l c')
      (⟨.done, live, state, .test ⟨8⟩ .skip (.lit 0) (.abort s) s [y] :: tail⟩ : Config refusing) ↔
      dropOut refusing [y] live state = none) :=
  ⟨rfl, rfl, added_counterexample .test program _ rfl rfl _ rfl⟩


/-- Instance (law `return_refused`). Both populated modes progress after accepted admission.
Uncaptured returns also progress without a result-binding hook. -/
theorem return_accepted (dst : Option Var) :
    ¬ Blocked (Step Demo.prog) (⟨.exiting (.ret 53), live, state, .ret dst [(p, 67)] :: tail⟩ : Config ops) ∧
    ¬ Blocked (Step Demo.prog) (⟨.done, live, state, .ret dst [(p, 67)] :: tail⟩ : Config ops) := by
  have h := return_refused Demo.prog dst 53 [(p, 67)] live state tail
  constructor
  · intro hb; have hn := h.1.mp hb; cases dst <;> cases hn
  · intro hb; have hn := h.2.mp hb; cases dst <;> cases hn

/-- Planted (law `return_refused`). Delete only `returnExplicit`; use that dispatch in both clauses. -/
theorem returnExplicit_deleted (dst : Option Var) :
    ¬ ((Blocked (fun c l c' => ∃ a, Dispatches (deleted .returnExplicit) Demo.prog c a l c') (⟨.exiting (.ret 53), live, state, .ret dst [(p, 67)] :: tail⟩ : Config ops) ↔ bindLife ops dst state = none) ∧
       (Blocked (fun c l c' => ∃ a, Dispatches (deleted .returnExplicit) Demo.prog c a l c') (⟨.done, live, state, .ret dst [(p, 67)] :: tail⟩ : Config ops) ↔ bindLife ops dst state = none)) := by
  intro h
  have hn := h.1.mp (deleted_blocked .returnExplicit Demo.prog (⟨.exiting (.ret 53), live, state, .ret dst [(p, 67)] :: tail⟩ : Config ops) rfl)
  cases dst <;> cases hn

/-- Planted (law `return_refused`). Delete only `returnImplicit`; use that dispatch in both clauses. -/
theorem returnImplicit_deleted (dst : Option Var) :
    ¬ ((Blocked (fun c l c' => ∃ a, Dispatches (deleted .returnImplicit) Demo.prog c a l c') (⟨.exiting (.ret 53), live, state, .ret dst [(p, 67)] :: tail⟩ : Config ops) ↔ bindLife ops dst state = none) ∧
       (Blocked (fun c l c' => ∃ a, Dispatches (deleted .returnImplicit) Demo.prog c a l c') (⟨.done, live, state, .ret dst [(p, 67)] :: tail⟩ : Config ops) ↔ bindLife ops dst state = none)) := by
  intro h
  have hn := h.2.mp (deleted_blocked .returnImplicit Demo.prog (⟨.done, live, state, .ret dst [(p, 67)] :: tail⟩ : Config ops) rfl)
  cases dst <;> cases hn

/-- Instance (law `leave_refused`). Both populated modes progress after accepted admission. -/
theorem leave_accepted :
    ¬ Blocked (Step Demo.prog) (⟨.done, live, state, .functionEnd :: tail⟩ : Config ops) ∧
    ¬ Blocked (Step Demo.prog) (⟨.exiting (.ret 53), live, state, .functionEnd :: tail⟩ : Config ops) := by
  have h := leave_refused Demo.prog 53 live state tail
  constructor
  · intro hb; have hn := h.1.mp hb; cases hn
  · intro hb; have hn := h.2.mp hb; cases hn

/-- Planted (law `leave_refused`). Delete only `leaveNormal`; use that dispatch in both clauses. -/
theorem leaveNormal_deleted :
    ¬ ((Blocked (fun c l c' => ∃ a, Dispatches (deleted .leaveNormal) Demo.prog c a l c') (⟨.done, live, state, .functionEnd :: tail⟩ : Config ops) ↔ ops.life .leave state = none) ∧
       (Blocked (fun c l c' => ∃ a, Dispatches (deleted .leaveNormal) Demo.prog c a l c') (⟨.exiting (.ret 53), live, state, .functionEnd :: tail⟩ : Config ops) ↔ ops.life .leave state = none)) := by
  intro h
  have hn := h.1.mp (deleted_blocked .leaveNormal Demo.prog (⟨.done, live, state, .functionEnd :: tail⟩ : Config ops) rfl)
  cases hn

/-- Planted (law `leave_refused`). Delete only `leaveReturn`; use that dispatch in both clauses. -/
theorem leaveReturn_deleted :
    ¬ ((Blocked (fun c l c' => ∃ a, Dispatches (deleted .leaveReturn) Demo.prog c a l c') (⟨.done, live, state, .functionEnd :: tail⟩ : Config ops) ↔ ops.life .leave state = none) ∧
       (Blocked (fun c l c' => ∃ a, Dispatches (deleted .leaveReturn) Demo.prog c a l c') (⟨.exiting (.ret 53), live, state, .functionEnd :: tail⟩ : Config ops) ↔ ops.life .leave state = none)) := by
  intro h
  have hn := h.2.mp (deleted_blocked .leaveReturn Demo.prog (⟨.exiting (.ret 53), live, state, .functionEnd :: tail⟩ : Config ops) rfl)
  cases hn

/-- Instance (law `enter_refused`). Accepted entry creates a populated parameter activation. -/
theorem enter_accepted : enter eatFn [17] state = some
    ⟨.exec eatFn.body, [(p, 17)], ⟨41, [[], [y], [p]]⟩,
      [.scope eatFn.block [p] [], .functionEnd]⟩ := rfl

/-- Planted. Delete only successful entry; refused entry is unchanged. -/
def deletedEnter (f : Fn O) (vs : List O.Val) (st : O.Store) : Option (Config O) :=
  match enter f vs st with | none => none | some _ => none

/-- Planted (law `enter_refused`). Accepted lifecycle admission cannot silently lose entry. -/
theorem enter_deleted : ¬ (deletedEnter eatFn [17] state = none ↔
    ops.life (.enter [p]) state = none) := by
  intro h; have hn := h.mp rfl; cases hn

/-- Instance support. The accepted version of the same populated primitive request. -/
def acceptedRequest : Config ops :=
  ⟨.exec (.prim (some p) .log [.move x] s), live, state, tail⟩
/-- Instance support. Actual result continuation after accepted result binding. -/
def acceptedContinuation (a : Answer Int Int DState) : Option (Config ops) := match a.result with
  | .ok v => (bindLife ops (some p) a.store).map fun st =>
      ⟨.done, bindDst (some p) v live, st, tail⟩
  | .error kd => some ⟨.trapped kd s, live, a.store, tail⟩

/-- Instance (law `prim_bind_step`). An allowed populated answer witnesses real progress. -/
theorem primitive_step_accepted :
    evalArgs ops live state [.move x] = .ok ([17], moved) ∧
    ops.allows .log [17] moved answer = true ∧ ¬ Blocked (Step Demo.prog) acceptedRequest := by
  refine ⟨rfl, by decide, fun hb => ?_⟩
  have h := (prim_bind_step Demo.prog p .log [.move x] s live state moved [17] tail rfl).mp hb
  obtain ⟨v, _, hn⟩ := h answer (by decide)
  cases hn

/-- Planted (law `prim_bind_step`). Deleting the primitive dispatch loses accepted progress. -/
theorem primitive_deleted : evalArgs ops live state [.move x] = .ok ([17], moved) ∧
    ops.allows .log [17] moved answer = true ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (deleted .primitive) Demo.prog c a l c')
      acceptedRequest ↔ ∀ a, ops.allows .log [17] moved a = true →
        ∃ v, a.result = .ok v ∧ ops.life (.bind p) a.store = none) := by
  refine ⟨rfl, by decide, fun h => ?_⟩
  obtain ⟨v, _, hn⟩ := h.mp (deleted_blocked .primitive Demo.prog acceptedRequest rfl)
    answer (by decide)
  cases hn


/-- Planted. Delete only accepted request continuation; recursive silent steps use this mutant. -/
def stuckContinuation (P : Program O) (ρ : Oracle O) : Nat → List O.Event → Config O → Report O
  | fuel, hist, c =>
    match c.ctrl, c.final?, fuel with
    | .refused, _, _ => ⟨hist, .stuck, c, fuel⟩
    | _, some f, _ => ⟨hist, .final f, c, fuel⟩
    | _, none, 0 => ⟨hist, .outOfFuel, c, 0⟩
    | _, none, n + 1 =>
      match step P c with
      | none => ⟨hist, .stuck, c, n + 1⟩
      | some (.next c') => stuckContinuation P ρ n hist c'
      | some (.request p vs site st k) =>
        match ρ hist p vs site st with
        | none => ⟨hist, .noAnswer, c, n + 1⟩
        | some a =>
          if O.allows p vs st a then
            match k a with
            | some _ => ⟨hist, .stuck, c, n + 1⟩
            | none => ⟨hist, .stuck, c, n + 1⟩
          else ⟨hist, .badAnswer, c, n + 1⟩



/-- Instance (law `continuation_refused`). All allowed-request premises hold;
accepted continuation spends fuel and progresses. -/
theorem continuation_accepted : acceptedRequest.final? = none ∧ acceptedRequest.ctrl ≠ .refused ∧
    step Demo.prog acceptedRequest = some (.request .log [17] s moved acceptedContinuation) ∧
    env [91] .log [17] s moved = some answer ∧
    ops.allows .log [17] moved answer = true ∧
    (¬ (runFrom Demo.prog env 1 [91] acceptedRequest = ⟨[91], .stuck, acceptedRequest, 1⟩) ∧
      acceptedContinuation answer ≠ none) := by
  refine ⟨rfl, (by intro h; cases h), (by simp only [step, acceptedRequest, (show evalArgs ops live state [.move x] = .ok ([17], moved) from rfl), onRes]; apply congrArg some; apply congrArg (Out.request (O := ops) .log [17] s moved); funext a; cases hr : a.result <;> simp [acceptedContinuation, hr, bindLife, ops]), rfl, by decide, ?_⟩
  constructor
  · intro h; have hf := congrArg Report.fuel h; change 0 = 1 at hf; cases hf
  · intro h; cases h

/-- Planted (law `continuation_refused`). All allowed-request premises hold;
accepted continuation cannot be silently discarded. -/
theorem continuation_deleted : acceptedRequest.final? = none ∧ acceptedRequest.ctrl ≠ .refused ∧
    step Demo.prog acceptedRequest = some (.request .log [17] s moved acceptedContinuation) ∧
    env [91] .log [17] s moved = some answer ∧
    ops.allows .log [17] moved answer = true ∧
    (¬ (stuckContinuation Demo.prog env 1 [91] acceptedRequest = ⟨[91], .stuck, acceptedRequest, 1⟩ ↔
      acceptedContinuation answer = none)) := by
  refine ⟨rfl, (by intro h; cases h), (by simp only [step, acceptedRequest, (show evalArgs ops live state [.move x] = .ok ([17], moved) from rfl), onRes]; apply congrArg some; apply congrArg (Out.request (O := ops) .log [17] s moved); funext a; cases hr : a.result <;> simp [acceptedContinuation, hr, bindLife, ops]), rfl, by decide, ?_⟩
  intro h; have hn := h.mp rfl; cases hn

/-- Instance (law `prim_bind_refused`). Successful result admission
advances the populated request. -/
theorem primitive_bind_accepted :
    evalArgs ops live state [.move x] = .ok ([17], moved) ∧
    env [91] .log [17] s moved = some answer ∧
    ops.allows .log [17] moved answer = true ∧ answer.result = .ok 0 ∧
    (¬ (runFrom Demo.prog env 1 [91] acceptedRequest = ⟨[91], .stuck, acceptedRequest, 1⟩) ∧ ¬ (ops.life (.bind p) answer.store = none)) := by
  refine ⟨rfl, rfl, by decide, rfl, ?_⟩
  constructor
  · intro h; have hf := congrArg Report.fuel h; change 0 = 1 at hf; cases hf
  · intro h; cases h

/-- Planted (law `prim_bind_refused`). Successful result admission
rejects a deleted continuation. -/
theorem primitive_bind_deleted :
    evalArgs ops live state [.move x] = .ok ([17], moved) ∧
    env [91] .log [17] s moved = some answer ∧
    ops.allows .log [17] moved answer = true ∧ answer.result = .ok 0 ∧
    (¬ (stuckContinuation Demo.prog env 1 [91] acceptedRequest = ⟨[91], .stuck, acceptedRequest, 1⟩ ↔ ops.life (.bind p) answer.store = none)) := by
  refine ⟨rfl, rfl, by decide, rfl, ?_⟩
  intro h; have hn := h.mp rfl; cases hn


/-- Instance (law `return_refused`). Concrete captured and uncaptured destinations both progress. -/
theorem return_destinations_accepted :
    (¬ Blocked (Step Demo.prog) ⟨.exiting (.ret 53), live, state, .ret (some x) [(p, 67)] :: tail⟩) ∧
    (¬ Blocked (Step Demo.prog) ⟨.done, live, state, .ret none [(p, 67)] :: tail⟩) :=
  ⟨(return_accepted (some x)).1, (return_accepted none).2⟩

/-- Instance (law `test_drop_refused`). Accepted drop also progresses along the false branch. -/
theorem false_test_accepted : ops.eval live state (.lit 0) = .ok (0, state) ∧
    ops.test (0 : Int) = some false ∧ ¬ Blocked (Step Demo.prog)
      ⟨.done, live, state, .test ⟨8⟩ .skip (.lit 0) (.abort s) s [y] :: tail⟩ := by
  refine ⟨rfl, rfl, fun hb => ?_⟩
  have hn := (test_drop_refused Demo.prog ⟨8⟩ .skip (.abort s) (.lit 0) s [y] live state state
    0 false tail rfl rfl).mp hb
  cases hn

/-- Planted (law `test_drop_refused`). The false branch also rejects deletion of accepted drop. -/
theorem false_test_deleted : ops.eval live state (.lit 0) = .ok (0, state) ∧
    ops.test (0 : Int) = some false ∧
    ¬ (Blocked (fun c l c' => ∃ a, Dispatches (deleted .test) Demo.prog c a l c')
      (⟨.done, live, state, .test ⟨8⟩ .skip (.lit 0) (.abort s) s [y] :: tail⟩ : Config ops) ↔
      dropOut ops [y] live state = none) := by
  refine ⟨rfl, rfl, fun h => ?_⟩
  have hn := h.mp (deleted_blocked .test Demo.prog _ rfl)
  cases hn

end LifecycleEvidence

end ConcreteSpec.Core.Exec
