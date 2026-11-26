import ConcreteSpecEvidence.ExecExamples
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Proofs.Exec

namespace ConcreteSpec.Core.Exec
open ConcreteSpec.Core
variable {O : Operands} {V : Type}

/-- Representation lemma. Prepends a primitive request/response step to a multi-step execution
trace. -/
private theorem steps_request_inv {P : Program O} {c c'' : Config O} {p : O.Prim} {vs : List O.Val}
    {site : SiteId} {st : O.Store} {k : Answer O.Val O.Event O.Store → Option (Config O)}
    {a : Answer O.Val O.Event O.Store} {tr : List O.Event}
    (h : step P c = some (.request p vs site st k)) (ha : O.allows p vs st a = true)
    {c' : Config O} (hk : k a = some c') (h' : Steps P c' tr c'') : Steps P c (a.label.events ++ tr) c'' :=
  Steps.step (Step.prim h ha hk) h'

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
theorem example_Helper_application_ConcreteSpec_Core_Exec_cleanup_runs_exact_plan : BoundaryRun prog (Step prog) (.ret 7) suffix [inner, outer]
    live initial actions [1, 2] [] middle :=
  (cleanup_runs_exact_plan prog (.ret 7) suffix [inner, outer] live [] initial middle
    actions [1, 2]).mpr ⟨nested_passes, rfl, rfl, performed⟩

/-- Helper (application of `ConcreteSpec.Core.Exec.planWith_calls`). Projection retains call order,
while removing only storage actions. -/
theorem example_Helper_application_ConcreteSpec_Core_Exec_planWith_calls : (planWith [inner, outer] live).filterMap CleanAct.call? = plan [inner, outer] :=
  planWith_calls [inner, outer] live

/-- Planted. Reverse the action plan, preserving every action. -/
def reversePlanWith (pre : List (Frame O)) (L : AList Var O.Val) := (planWith pre L).reverse
/-- Planted. Omit calls from the call-only projection. -/
def emptyPlan (_ : List (Frame O)) : List (Deferred O) := []
/-- Planted (certificate `ConcreteSpec.Core.Exec.planWith_calls`). Reversal changes the two calls. -/
theorem planted_certificate_ConcreteSpec_Core_Exec_planWith_calls_Reversal : (reversePlanWith [inner, outer] live).filterMap CleanAct.call? ≠ plan [inner, outer] := by
  intro h; cases h
/-- Planted (certificate `ConcreteSpec.Core.Exec.planWith_calls`). Omitting calls changes projection. -/
theorem planted_certificate_ConcreteSpec_Core_Exec_planWith_calls_Omitting : (planWith [inner, outer] live).filterMap CleanAct.call? ≠ emptyPlan [inner, outer] := by
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
theorem planted_certificate_ConcreteSpec_Core_Exec_cleanup_runs_exact_plan_All : ¬ (BoundaryRun prog (NoDispatchStep prog) (.ret 7) suffix [inner, outer]
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
theorem example_Helper_application_ConcreteSpec_Core_Exec_cleanup_runs_exact_plan_2 : BoundaryRun prog (Step prog) (.ret 7) suffix [emptyScope] live initial
    [.drop [y] live [(y, 8)]] [] [(y, 8)] middle :=
  (cleanup_runs_exact_plan prog (.ret 7) suffix [emptyScope] live [(y, 8)] initial middle
    [.drop [y] live [(y, 8)]] []).mpr ⟨empty_passes, rfl, rfl, .drop rfl .nil⟩

/-- Planted (certificate `ConcreteSpec.Core.Exec.cleanup_runs_exact_plan`). Missing storage teardown
has a real observed run with correct locals but cannot satisfy independent action execution. -/
theorem planted_certificate_ConcreteSpec_Core_Exec_cleanup_runs_exact_plan_Missing : BoundaryRun prog (MissingDropStep prog) (.ret 7) suffix [emptyScope] live initial
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
theorem planted_certificate_ConcreteSpec_Core_Exec_cleanup_runs_exact_plan_Missing_2 : BoundaryRun prog (MissingRestrictStep prog) (.ret 7) suffix [emptyScope] live initial
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
theorem planted_certificate_ConcreteSpec_Core_Exec_cleanup_runs_exact_plan_injected : BoundaryRun prog (ExtraStep prog) (.ret 7) suffix [emptyScope] live initial
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
theorem example_Helper_application_ConcreteSpec_Core_Exec_boundary_of_performed : BoundaryRun prog (Step prog) (.ret 7) suffix [emptyScope] live initial
    (planWith [emptyScope] live) [] [(y, 8)] middle :=
  boundary_of_performed [emptyScope] empty_passes drop_performed

/-- Planted. Omit only empty-scope teardown transitions. -/
inductive NoDropStep (P : Program O) : Config O → Label O → Config O → Prop where
  | retain {c l c'} : Step P c l c' → (match c.ctrl, c.frames with
      | .exiting _, .scope _ _ [] :: _ => False
      | _, _ => True) → NoDropStep P c l c'

/-- Planted (certificate `ConcreteSpec.Core.Exec.boundary_of_performed`). The independent populated
drop still succeeds, but the sole missing transition prevents reconstruction. -/
theorem planted_certificate_ConcreteSpec_Core_Exec_boundary_of_performed_independent : Performed prog (planWith [emptyScope] live) live initial [] [(y, 8)] middle ∧
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
theorem example_Helper_application_ConcreteSpec_Core_Exec_boundary_scope_calls : BoundaryRun prog (Step prog) (.ret 7) suffix [inner, outer] live initial
    actions [1, 2] [] middle :=
  boundary_scope_calls ⟨1⟩ [y] [outer] [d 1] performed base_execution

/-- Planted (certificate `ConcreteSpec.Core.Exec.boundary_scope_calls`). Dispatch omission alone
prevents the witnessed call-then-drop execution. Independent actions remain satisfiable. -/
theorem planted_certificate_ConcreteSpec_Core_Exec_boundary_scope_calls_Dispatch : Performed prog actions live initial [1, 2] [] middle ∧
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
theorem split_return_example : ∃ n₁ n₂ tr₁ st₁ tr₂, 6 = n₁ + n₂ ∧ [1] = tr₁ ++ tr₂ ∧
    FirstStepsN (Step prog) (fun c => c.ctrl = .done ∧ c.frames = returnQ)
      c1 tr₁ ⟨.done, live, st₁, returnQ⟩ n₁ ∧
    FirstStepsN (Step prog) (fun c => c.frames.length ≤ suffix.length)
      ⟨.done, live, st₁, returnQ⟩ tr₂ c7 n₂ :=
  FirstStepsN.split_return callee_raw (by decide) ⟨[_, _], rfl⟩

/-- Instance (law `ConcreteSpec.Core.Exec.FirstStepsN.call_return`). A deferred call crosses
its saved continuation only after restoring populated caller locals and the prior activation. -/
theorem call_return_example : ∃ n₁ n₂ tr₁ st₁ tr₂, 7 = n₁ + n₂ ∧ [1] = tr₁ ++ tr₂ ∧
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
theorem call_return_planted : FirstStepsN WrongReturn (fun c => c.frames.length ≤ suffix.length) c0 [1] bad7 7 ∧
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
theorem split_return_planted : Protected (O := ops) live returnQ c1 ∧
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
theorem protected_step_example : PreservesCaller (Step prog) live returnQ c4 := protected_step ⟨[], rfl⟩
/-- Planted (law `ConcreteSpec.Core.Exec.protected_step`). Keeping callee locals violates
only the return-restoration operation at the actual saved boundary. -/
theorem protected_step_planted : Protected (O := ops) live returnQ c4 ∧ ¬ PreservesCaller WrongReturn live returnQ c4 := by
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
theorem call_protects_example : SavesCaller (Step prog) live initial returnQ (d 1) := call_protects

/-- Planted. Entry is identical except that it omits the saved caller frame. -/
inductive MissingCaller : Config ops → Label ops → Config ops → Prop where
  | enter : MissingCaller c0 .silent
      ⟨.exec logFn.body, [(x, 1)], entered, .scope logFn.block [x] [] :: .functionEnd :: returnQ⟩

/-- Planted (law `ConcreteSpec.Core.Exec.call_protects`). Omitting the saved caller frame
cannot protect this populated caller; the callee, event opportunity and lifecycle entry stay. -/
theorem call_protects_planted : ¬ SavesCaller MissingCaller live initial returnQ (d 1) := by
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
theorem boundary_complete_example : ∃ acts, BoundaryRun prog (Step prog) (.ret 7) suffix [inner]
    live initial acts [1] [(y, 8)] middle := boundary_complete raw_scope rfl inner_passes

/-- Instance (law `ConcreteSpec.Core.Exec.raw_cleanup_runs_plan`). The actual first-arrival
path gives exactly the independent plan, restored locals and performed actions. -/
theorem raw_cleanup_runs_plan_example : ∃ acts, BoundaryRun prog (Step prog) (.ret 7) suffix [inner]
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
theorem boundary_complete_planted : FirstStepsN SkipCall (fun c => c.frames.length ≤ suffix.length)
      ⟨.exiting (.ret 7), live, initial, inner :: suffix⟩ [] c7 4 ∧
    (∀ f ∈ [inner], Passes (O := ops) (.ret 7) f) ∧
    ¬ ∃ acts, BoundaryRun prog SkipCall (.ret 7) suffix [inner]
      live initial acts [] [(y, 8)] middle := ⟨skip_raw, inner_passes, skip_no_boundary⟩

/-- Planted (law `ConcreteSpec.Core.Exec.raw_cleanup_runs_plan`). This raw first arrival
preserves passability and storage teardown but omits the planned call, so the law fails. -/
theorem raw_cleanup_runs_plan_planted : FirstSteps SkipCall (fun c => c.frames.length ≤ suffix.length)
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
theorem register_protected_example : register (d 2) ([.seq .skip, inner] ++ .ret none live :: returnQ) =
      some ([.seq .skip, .scope ⟨1⟩ [y] [d 2, d 1]] ++ .ret none live :: returnQ) ∧
    RegistrationPreservesCaller register (d 2) live returnQ := ⟨rfl, register_protected _ _ _⟩

/-- Planted. Register normally in the nearest scope, but erase only the saved caller frame. -/
def forgetRegister (d : Deferred O) (frames : List (Frame O)) : Option (List (Frame O)) :=
  (register d frames).map (List.filter fun f => match f with | .ret .. => false | _ => true)

/-- Planted (law `ConcreteSpec.Core.Exec.register_protected`). A successful populated
registration loses its saved boundary when only that frame is erased. -/
theorem register_protected_planted : ¬ RegistrationPreservesCaller forgetRegister (d 2) live returnQ := by
  intro h
  obtain ⟨pre, he⟩ := h [.seq .skip, inner] _ rfl
  change [.seq .skip, .scope ⟨1⟩ [y] [d 2, d 1]] ++ returnQ = _ at he
  have hm : Frame.ret none live ∈
      ([.seq .skip, .scope ⟨1⟩ [y] [d 2, d 1]] ++ returnQ : List (Frame ops)) := by
    rw [he]; simp
  simp [returnQ, emptyScope, suffix] at hm

/-- Instance (law `ConcreteSpec.Core.Exec.exit_label`). Dispatch is silent; the event belongs
to the call body, as witnessed by `raw_scope`. -/
theorem exit_label_example : ExitSilent (Step prog) (.ret 7) live initial (inner :: suffix) := exit_label

/-- Planted. Change only the label on the real scope dispatch. -/
inductive NoisyExit : Config ops → Label ops → Config ops → Prop where
  | dispatch : NoisyExit ⟨.exiting (.ret 7), live, initial, inner :: suffix⟩ (.emit 9) c0

/-- Planted (law `ConcreteSpec.Core.Exec.exit_label`). A spurious dispatcher event is visible
with the same source and target as the production silent transition. -/
theorem exit_label_planted : Step prog ⟨.exiting (.ret 7), live, initial, inner :: suffix⟩ .silent c0 ∧
    ¬ ExitSilent NoisyExit (.ret 7) live initial (inner :: suffix) := by
  refine ⟨.silent rfl, fun h => ?_⟩
  cases h (.emit 9) c0 NoisyExit.dispatch

/-- A trap retains queued calls, lifecycle state and saved caller frames without executing them. -/
def trapped : Config ops := ⟨.trapped .abort s, live, initial,
  [inner, outer, .functionEnd, .ret none [(t, 12)]]⟩

/-- Instance (law `ConcreteSpec.Core.Exec.step_trapped`). The pending cleanup and caller do
not prevent final recognition or allow another transition. -/
theorem step_trapped_example : (trapped.final? = some (.trapped .abort s) ↔ trapped.ctrl = .trapped .abort s) ∧
    Blocked (Step prog) trapped := step_trapped prog _ _ _ _ _ _

/-- Planted. Omit only trapped-state final recognition. -/
def noTrapFinal? (c : Config O) : Option (Final O.Val) :=
  match c.ctrl with | .trapped .. => none | _ => c.final?
/-- Planted (law `ConcreteSpec.Core.Exec.step_trapped`). The real trap is no longer final. -/
theorem step_trapped_planted : ¬ (noTrapFinal? trapped = some (.trapped .abort s) ↔
    trapped.ctrl = .trapped .abort s) := by
  intro h; cases h.mpr rfl

/-- Planted. Add only recovery from this trapped state to `done`. -/
inductive RecoverTrap : Config ops → Label ops → Config ops → Prop where
  | recover : RecoverTrap trapped .silent { trapped with ctrl := .done }
/-- Planted (law `ConcreteSpec.Core.Exec.step_trapped`). Recovery violates terminality while
preserving final recognition, populated locals, lifecycle state and pending calls. -/
theorem step_trapped_planted_2 : trapped.final? = some (.trapped .abort s) ∧ ¬ Blocked RecoverTrap trapped := by
  refine ⟨rfl, fun h => ?_⟩
  exact h .silent _ RecoverTrap.recover

/-- Instance (law `ConcreteSpec.Core.Exec.steps_from_trapped`). Recovery cannot change even
just the control field of the populated trapped endpoint. -/
theorem steps_from_trapped_example : ¬ Steps prog trapped [] { trapped with ctrl := .done } := by
  intro h
  have bad := ((steps_from_trapped prog .abort s live initial _ [] _).mp h).2
  cases bad

/-- Instance (law `ConcreteSpec.Core.Exec.steps_from_trapped`). No event can be emitted even
when the endpoint remains exactly the trapped configuration. -/
theorem steps_from_trapped_example_2 : ¬ Steps prog trapped [9] trapped := by
  intro h
  have bad := ((steps_from_trapped prog .abort s live initial _ [9] trapped).mp h).1
  cases bad

/-- Planted. Add only silent recovery to a changed endpoint. -/
inductive RecoverTrapSteps : Config ops → List Int → Config ops → Prop where
  | recover : RecoverTrapSteps trapped [] { trapped with ctrl := .done }
/-- Planted (law `ConcreteSpec.Core.Exec.steps_from_trapped`). The changed-endpoint exclusion
fails with just the forbidden silent recovery. -/
theorem steps_from_trapped_planted : ¬ (¬ RecoverTrapSteps trapped [] { trapped with ctrl := .done }) :=
  fun h => h .recover

/-- Planted. Add only an event at the identical trapped endpoint. -/
inductive NoisyTrapSteps : Config ops → List Int → Config ops → Prop where
  | emit : NoisyTrapSteps trapped [9] trapped
/-- Planted (law `ConcreteSpec.Core.Exec.steps_from_trapped`). The nonempty-trace exclusion
fails with just the forbidden event, without changing the endpoint. -/
theorem steps_from_trapped_planted_2 : ¬ (¬ NoisyTrapSteps trapped [9] trapped) := fun h => h .emit

/-- Instance (law `ConcreteSpec.Core.Exec.abort_runs_no_cleanup`). Abort reaches its trap
without touching registered calls, locals, activation markers or the caller frame. -/
theorem abort_runs_no_cleanup_example : Steps prog { trapped with ctrl := .exec (.abort s) } [] trapped :=
  (abort_runs_no_cleanup prog s live initial _ [] trapped).mpr ⟨rfl, .inr rfl⟩

/-- Planted. Replace abort's terminal transition by unwinding; all later transitions are real. -/
inductive AbortCleanupSteps : Config ops → List Int → Config ops → Prop where
  | cleanup {tr c} : Steps prog ⟨.exiting (.ret 7), live, initial, inner :: suffix⟩ tr c →
      AbortCleanupSteps ⟨.exec (.abort s), live, initial, inner :: suffix⟩ tr c
/-- Planted (law `ConcreteSpec.Core.Exec.abort_runs_no_cleanup`). Wrongly starting cleanup
emits the registered call's real event, violating abort's empty-trace conclusion. -/
theorem abort_runs_no_cleanup_planted : AbortCleanupSteps ⟨.exec (.abort s), live, initial, inner :: suffix⟩ [1] c7 ∧
    ¬ ([1] = ([] : List Int) ∧
      (c7 = ⟨.exec (.abort s), live, initial, inner :: suffix⟩ ∨
       c7 = ⟨.trapped .abort s, live, initial, inner :: suffix⟩)) :=
  ⟨.cleanup raw_scope.unsized_inv.steps_inv, by simp⟩

/-- Instance (law `ConcreteSpec.Core.Exec.steps_from_trapped`). A deferred trap stops the
program before either later registered log or activation leave, as OD68 requires. -/
theorem steps_from_trapped_example_3 : runFrom prog env 30 [13]
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
theorem evalArgs_first_trap_example : evalArgs trapOps [(y, 8)] initial
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
theorem evalArgs_first_trap_planted : lostStateArgs trapOps [(y, 8)] initial
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
theorem evalArgs_first_trap_planted_2 : pastTrapArgs trapOps [(y, 8)] initial
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
theorem evalArgs_first_trap_planted_3 : swallowTrapEvalArgs trapOps [(y, 8)] initial
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
theorem planted_call_then_drop_reconstruction_s_higher : (∀ st tr,
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
theorem example_Helper_Positive_state_dependent_call_order : (runFrom prog env 40 [13] ⟨.exiting (.ret 7), live, initial, orderFrames false⟩).events = [13, 5] ∧
    (runFrom prog env 40 [13] ⟨.exiting (.ret 7), live, initial, orderFrames false⟩).residual.store =
      ⟨5, [[p], [t]]⟩ := ⟨rfl, rfl⟩
/-- Planted. Reverse just the registered calls. The counter read now observes the earlier
state, despite identical final counter, locals and lifecycle state. -/
theorem planted_Reverse_just_registered_calls_counter_read : (runFrom prog env 40 [13] ⟨.exiting (.ret 7), live, initial, orderFrames true⟩).events = [13, 4] ∧
    (runFrom prog env 40 [13] ⟨.exiting (.ret 7), live, initial, orderFrames true⟩).residual.store =
      ⟨5, [[p], [t]]⟩ ∧
    (runFrom prog env 40 [13] ⟨.exiting (.ret 7), live, initial, orderFrames true⟩).residual.locals = [(y, 8)] :=
  ⟨rfl, rfl, rfl⟩

end CleanupEvidence

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
