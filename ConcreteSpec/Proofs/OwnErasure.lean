import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Own
import ConcreteSpec.Spec.Scalar

/-! # Ownership erasure and argument evaluation (O2)

Authority: docs/decisions.md#decided-2026-11-05-core (Q9.10),
#decided-2026-11-08-recovered-d3 (C5 forward simulation and accepted converse),
#foundation-q1 (C5), and #decided-2026-11-13-lifecycle-refusal.
The populated fixtures reuse O1's Q3.1–Q3.4 and OD15/OD23 whole-root rules.
No language policy is inferred from the fixture's integer condition interface.
The checker acceptance premise is an
obligation, not a claim that a checker implementation has been proved correct.
O1 owns ledger preservation and lifetime facts; this module owns execution erasure.
Source commands in the extraction's trailing preservation/lifetime/finality groups
already belong to O1 and are not copied here. O3 remains out of scope.
Held policies remain ISS-0064/ISS-0065, ISS-0068 through ISS-0073 and ISS-0076:
source characters/paths, constant restrictions, codecs, wiping, startup/ABI and
aggregate/whole-pointer layout. None is needed or encoded by these erasure laws.
Only Law counts as evidence; Helper, Representation lemma, Instance and Planted
follow #decided-2026-11-13-proof-labels. Mutants follow
#decided-2026-11-14-trap-sites-and-law-tests.
-/
namespace ConcreteSpec.Core.Own
section ExecErasure
variable {O : Exec.Operands} {types : Var → Option Descriptor}
  {permit : AList Var O.Val → O.Expr → State → Option State}

/-- Helper (inversion). Structural inversion for `eval_ok_erasure` on a given ok result. -/
theorem eval_ok_erasure_result (L : AList Var O.Val) (s t : Store O.Store) (e : O.Expr) (v : O.Val)
    (h : (instrumentOperands O types permit).eval L s e = .ok (v, t)) :
    O.eval L (erase s) e = .ok (v, erase t) := by
  dsimp [instrumentOperands, instrumentEval] at h
  cases hp : permit L e s.own with
  | none => rw [hp] at h; cases h
  | some own =>
    rw [hp] at h
    cases he : O.eval L s.concrete e with
    | ok r =>
      rcases r with ⟨w, concrete⟩
      rw [he] at h
      cases h
      exact he
    | trap _ => rw [he] at h; cases h
    | stuck => rw [he] at h; cases h

/-- Helper (computation). Calculates one lifted operand result; the independently stated finite execution contract
is Steps_erasure. -/
theorem eval_ok_erasure (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) :
    match (instrumentOperands O types permit).eval L s e with
    | .ok (v, t) => O.eval L (erase s) e = .ok (v, erase t)
    | _ => True := by
  cases h : (instrumentOperands O types permit).eval L s e with
  | ok r => exact eval_ok_erasure_result L s r.2 e r.1 h
  | trap _ => trivial
  | stuck => trivial

/-- Helper (inversion). Structural inversion for `eval_trap_erasure` on a given trap result. -/
theorem eval_trap_erasure_result (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) (kd : TrapKind)
    (h : (instrumentOperands O types permit).eval L s e = .trap kd) :
    O.eval L (erase s) e = .trap kd := by
  dsimp [instrumentOperands, instrumentEval] at h
  cases hp : permit L e s.own with
  | none => rw [hp] at h; cases h
  | some own =>
    rw [hp] at h
    cases he : O.eval L s.concrete e with
    | ok r => rcases r with ⟨_, _⟩; rw [he] at h; cases h
    | trap _ => rw [he] at h; cases h; exact he
    | stuck => rw [he] at h; cases h

/-- Helper (computation). Calculates one lifted trap result; runFrom_erasure establishes terminal execution
agreement. -/
theorem eval_trap_erasure (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) :
    match (instrumentOperands O types permit).eval L s e with
    | .trap kd => O.eval L (erase s) e = .trap kd
    | _ => True := by
  cases h : (instrumentOperands O types permit).eval L s e with
  | ok _ => trivial
  | trap kd => exact eval_trap_erasure_result L s e kd h
  | stuck => trivial

/-- Law (authority: docs/decisions.md#decided-2026-11-05-core (Q9.10);
#decided-2026-11-08-recovered-d3 (C5 erasure);
#decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.instrumentOperands`). Successful argument
evaluation
preserves values and concrete storage. The result remains inside the conclusion so a mutant
cannot hide a wrong value behind an output fixed as a theorem argument. -/
theorem evalArgs_ok_erasure (L : AList Var O.Val) (args : List O.Expr) (s : Store O.Store) :
    match Exec.evalArgs (instrumentOperands O types permit) L s args with
    | .ok (vs, t) => Exec.evalArgs O L (erase s) args = .ok (vs, erase t)
    | _ => True := by
  induction args generalizing s with
  | nil => rfl
  | cons e es ih =>
    cases h : Exec.evalArgs (instrumentOperands O types permit) L s (e :: es) with
    | trap _ => trivial
    | stuck => trivial
    | ok pair =>
      rcases pair with ⟨vs, t⟩
      simp only [Exec.evalArgs, instrumentOperands] at h ⊢
      cases he : instrumentEval O permit L s e with
      | ok r =>
        rcases r with ⟨v, s₁⟩
        rw [he] at h
        rw [eval_ok_erasure_result (O := O) (types := types) L s s₁ e v he]
        dsimp [Res.bind] at h ⊢
        have ih₁ := ih s₁
        cases hes : Exec.evalArgs (instrumentOperands O types permit) L s₁ es with
        | ok r' =>
          rcases r' with ⟨vs', s₂⟩
          rw [hes] at ih₁
          simp only [instrumentOperands] at hes
          rw [hes] at h
          rw [ih₁]
          cases h
          rfl
        | trap _ =>
          simp only [instrumentOperands] at hes
          rw [hes] at h; cases h
        | stuck =>
          simp only [instrumentOperands] at hes
          rw [hes] at h; cases h
      | trap _ => rw [he] at h; cases h
      | stuck => rw [he] at h; cases h

/-- Law (authority: docs/decisions.md#decided-2026-11-05-core (Q9.10);
#decided-2026-11-08-recovered-d3 (C5 erasure);
#decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.instrumentOperands`). A trapping argument
evaluation
preserves the actual trap kind, without fixing that kind in a success premise. -/
theorem evalArgs_trap_erasure (L : AList Var O.Val) (args : List O.Expr) (s : Store O.Store) :
    match Exec.evalArgs (instrumentOperands O types permit) L s args with
    | .trap kd => Exec.evalArgs O L (erase s) args = .trap kd
    | _ => True := by
  induction args generalizing s with
  | nil => trivial
  | cons e es ih =>
    cases h : Exec.evalArgs (instrumentOperands O types permit) L s (e :: es) with
    | ok _ => trivial
    | stuck => trivial
    | trap kd =>
      simp only [Exec.evalArgs, instrumentOperands] at h ⊢
      cases he : instrumentEval O permit L s e with
      | ok r =>
        rcases r with ⟨v, s₁⟩
        rw [he] at h
        rw [eval_ok_erasure_result (O := O) (types := types) L s s₁ e v he]
        dsimp [Res.bind] at h ⊢
        have ih₁ := ih s₁
        cases hes : Exec.evalArgs (instrumentOperands O types permit) L s₁ es with
        | ok r' =>
          rcases r' with ⟨_, _⟩
          simp only [instrumentOperands] at hes
          rw [hes] at h; cases h
        | trap kd' =>
          rw [hes] at ih₁
          simp only [instrumentOperands] at hes
          rw [hes] at h
          cases h
          rw [ih₁]
          rfl
        | stuck =>
          simp only [instrumentOperands] at hes
          rw [hes] at h; cases h
      | trap _ =>
        rw [he] at h
        cases h
        rw [eval_trap_erasure_result (O := O) (types := types) L s e kd he]
        rfl
      | stuck => rw [he] at h; cases h

/-- Helper (projection). Condition tests are identical in the instrumented and erased operand instances. -/
theorem test_erasure_result (v : O.Val) :
    (instrumentOperands O types permit).test v = O.test v := rfl

/-- Helper (computation). Projects the unchanged concrete test; Steps_erasure establishes agreement of actual branch
execution. -/
theorem test_erasure (v : O.Val) :
    match (instrumentOperands O types permit).test v with
    | some b => O.test v = some b
    | none => O.test v = none := by
  dsimp [instrumentOperands]
  cases O.test v <;> rfl

/-- Helper (inversion). Structural inversion for `instrumentLife_erasure` on a given output store. -/
theorem instrumentLife_erasure_result (ev : Exec.Life) (s t : Store O.Store)
    (h : (instrumentOperands O types permit).life ev s = some t) :
    O.life ev (erase s) = some (erase t) := by
  dsimp [instrumentOperands, instrumentLife] at h
  cases hown : life types ev s.own with
  | none => rw [hown] at h; cases h
  | some own =>
    rw [hown] at h
    cases hc : O.life ev s.concrete with
    | none => rw [hc] at h; cases h
    | some concrete =>
      rw [hc] at h
      cases h
      exact hc

/-- Helper (computation). Calculates the lifted lifecycle result; Steps_erasure establishes execution agreement
through cleanup. -/
theorem instrumentLife_erasure (ev : Exec.Life) (s : Store O.Store) :
    match (instrumentOperands O types permit).life ev s with
    | some t => O.life ev (erase s) = some (erase t) ∧ life types ev s.own = some t.own
    | none => True := by
  cases h : (instrumentOperands O types permit).life ev s with
  | none => trivial
  | some t =>
    refine ⟨instrumentLife_erasure_result ev s t h, ?_⟩
    dsimp [instrumentOperands, instrumentLife] at h
    cases hown : life types ev s.own with
    | none => rw [hown] at h; cases h
    | some own =>
      rw [hown] at h
      cases hc : O.life ev s.concrete with
      | none => rw [hc] at h; cases h
      | some concrete => rw [hc] at h; cases h; rfl

/-- Helper (projection). Extracts the two defining conjuncts of the instrumented answer guard. -/
theorem allows_erasure_result (p : O.Prim) (vs : List O.Val) (s : Store O.Store)
    (a : Exec.Answer O.Val O.Event (Store O.Store))
    (h : (instrumentOperands O types permit).allows p vs s a = true) :
    O.allows p vs (erase s) (eraseAnswer a) = true ∧ a.store.own = s.own := by
  dsimp [instrumentOperands, instrumentAllows] at h
  obtain ⟨ha, hown⟩ := Bool.and_eq_true_iff.mp h
  exact ⟨ha, of_decide_eq_true hown⟩

/-- Helper (computation). Unpacks the primitive answer guard; Steps_erasure and runFrom_erasure establish actual
primitive execution agreement. -/
theorem allows_erasure (p : O.Prim) (vs : List O.Val) (s : Store O.Store)
    (a : Exec.Answer O.Val O.Event (Store O.Store)) :
    ((instrumentOperands O types permit).allows p vs s a = true ↔
      O.allows p vs (erase s) (eraseAnswer a) = true ∧ a.store.own = s.own) := by
  dsimp [instrumentOperands, instrumentAllows]
  rw [Bool.and_eq_true_iff, decide_eq_true_eq]

/-- Helper (projection). Result-binding lifecycle erases to the concrete `Exec.bindLife`. -/
private theorem bindLife_erasure_proj (dst : Option Var) (s t : Store O.Store)
    (h : Exec.bindLife (instrumentOperands O types permit) dst s = some t) :
    Exec.bindLife O dst (erase s) = some (erase t) := by
  cases dst with
  | none => cases h; rfl
  | some x => exact instrumentLife_erasure_result (.bind x) s t h

/-- Helper (projection). Scope-exit drop erases to the concrete `Exec.dropOut`. -/
private theorem dropOut_erasure_proj (dm : List Var) (L : AList Var O.Val) (s t : Store O.Store)
    (h : Exec.dropOut (instrumentOperands O types permit) dm L s = some t) :
    Exec.dropOut O dm L (erase s) = some (erase t) :=
  instrumentLife_erasure_result (.drop (Exec.dead dm L)) s t h

/-- Helper (projection). Deferred cleanup registration commutes with frame erasure. -/
private theorem register_erasure_proj (d : Exec.Deferred (instrumentOperands O types permit)) :
    ∀ (k k' : List (Exec.Frame (instrumentOperands O types permit))),
      Exec.register d k = some k' →
      Exec.register (eraseDeferred d) (k.map eraseFrame) = some (k'.map eraseFrame)
  | [], _, h => by cases h
  | .seq t :: k, k', h => by
    simp only [Exec.register, Option.map_eq_some_iff] at h
    obtain ⟨k₁, hk₁, rfl⟩ := h
    simp [Exec.register, eraseFrame, register_erasure_proj d k k₁ hk₁]
  | .scope b dom ds :: k, k', h => by
    cases h
    rfl
  | .test .. :: _, _, h => by cases h
  | .body .. :: _, _, h => by cases h
  | .resume _ :: _, _, h => by cases h
  | .ret .. :: _, _, h => by cases h

/-- Helper (projection). Function lookup in an erased program erases the looked-up function. -/
private theorem lookup_eraseProgram_proj :
    ∀ (fns : AList FnId (Exec.Fn (instrumentOperands O types permit))) (f : FnId),
      AList.lookup (List.map (fun e => (e.1, eraseFn e.2)) fns) f =
        (AList.lookup fns f).map eraseFn
  | [], _ => rfl
  | (g, fn) :: fns, f => by
    simp only [List.map_cons, AList.lookup]
    by_cases h : g = f
    · simp [h]
    · simp [h, lookup_eraseProgram_proj fns f]

/-- Law (authority: docs/decisions.md#decided-2026-11-05-core (Q9.10);
#decided-2026-11-08-recovered-d3 (C5 erasure);
#decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.stepInstrumented`). Silent machine-step
forward simulation
(owner decision 2026-11-08 Q3): whenever `stepInstrumented` on the instrumented C1 machine produces a next
configuration, `Exec.step` on the erased machine produces the erased configuration. -/
theorem step_next_erasure (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    match stepInstrumented O types permit P c with
    | some (.next c') => Exec.step (eraseProgram P) (eraseConfig c) = some (.next (eraseConfig c'))
    | _ => True := by
  cases h : stepInstrumented O types permit P c with
  | none => trivial
  | some out =>
    cases out with
    | request _ _ _ _ _ => trivial
    | next c' =>
      change Exec.step P c = some (.next c') at h
      rcases c with ⟨ctrl, L, st, k⟩
      cases ctrl with
      | exec s =>
        cases s with
        | skip =>
          cases h
          rfl
        | bind x e site =>
          simp only [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at h ⊢
          cases he : instrumentEval O permit L st e with
          | ok r =>
            rcases r with ⟨v, st'⟩
            rw [he] at h
            simp only [Exec.onRes, Option.map_eq_some_iff] at h
            obtain ⟨st'', hst'', hnext⟩ := h
            cases hnext
            rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
            simp [Exec.onRes,
              instrumentLife_erasure_result (O := O) (permit := permit) (.bind x) st' st'' hst'']
          | trap kd =>
            rw [he] at h
            cases h
            rw [eval_trap_erasure_result (O := O) (types := types) L st e kd he]
            simp [Exec.onRes]
          | stuck => rw [he] at h; cases h
        | assign x e site =>
          simp only [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at h ⊢
          cases he : instrumentEval O permit L st e with
          | ok r =>
            rcases r with ⟨v, st'⟩
            rw [he] at h
            simp only [Exec.onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at h
            obtain ⟨w, hw, st'', hst'', hnext⟩ := h
            cases hnext
            rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
            simp [Exec.onRes, hw,
              instrumentLife_erasure_result (O := O) (permit := permit) (.bind x) st' st'' hst'']
          | trap kd =>
            rw [he] at h
            cases h
            rw [eval_trap_erasure_result (O := O) (types := types) L st e kd he]
            simp [Exec.onRes]
          | stuck => rw [he] at h; cases h
        | seq s t =>
          cases h
          rfl
        | block b s =>
          cases h
          rfl
        | defer f args site =>
          simp only [Exec.step, Option.map_eq_some_iff] at h
          obtain ⟨k', hk', hnext⟩ := h
          cases hnext
          have hreg := register_erasure_proj (O := O) (types := types) (permit := permit) ⟨f, args, site⟩ k k' hk'
          simp only [eraseDeferred] at hreg
          simp [Exec.step, eraseConfig, eraseCtrl, eraseStmt, hreg]
        | cond e s t site =>
          simp only [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at h ⊢
          cases he : instrumentEval O permit L st e with
          | ok r =>
            rcases r with ⟨v, st'⟩
            rw [he] at h
            simp only [Exec.onRes, Option.map_eq_some_iff] at h
            obtain ⟨b, hb, hnext⟩ := h
            cases hnext
            rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
            cases b <;> simp [Exec.onRes, hb]
          | trap kd =>
            rw [he] at h
            cases h
            rw [eval_trap_erasure_result (O := O) (types := types) L st e kd he]
            simp [Exec.onRes]
          | stuck => rw [he] at h; cases h
        | loop l cr t b site =>
          cases h
          rfl
        | brk l =>
          cases h
          rfl
        | cont l =>
          cases h
          rfl
        | ret e site =>
          simp only [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at h ⊢
          cases he : instrumentEval O permit L st e with
          | ok r =>
            rcases r with ⟨v, st'⟩
            rw [he] at h
            cases h
            rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
            simp [Exec.onRes]
          | trap kd =>
            rw [he] at h
            cases h
            rw [eval_trap_erasure_result (O := O) (types := types) L st e kd he]
            simp [Exec.onRes]
          | stuck => rw [he] at h; cases h
        | call dst f args site =>
          dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at h ⊢
          cases he : Exec.evalArgs (instrumentOperands O types permit) L st args with
          | ok r =>
            rcases r with ⟨vs, st'⟩
            have he_erased : Exec.evalArgs O L (erase st) args = .ok (vs, erase st') := by
              have h0 := evalArgs_ok_erasure (O := O) (types := types) (permit := permit) L args st
              rw [he] at h0
              exact h0
            rw [he] at h
            simp only [Exec.onRes, Option.bind_eq_some_iff] at h
            obtain ⟨fn, hfn, h⟩ := h
            by_cases hlen : fn.params.length = vs.length
            · simp only [hlen, ↓reduceIte, Option.map_eq_some_iff] at h
              obtain ⟨st'', hst'', hnext⟩ := h
              cases hnext
              rw [he_erased]
              dsimp [Exec.onRes, eraseProgram]
              rw [lookup_eraseProgram_proj P.fns f, hfn]
              dsimp [Option.bind, eraseFn]
              simp only [hlen, ↓reduceIte]
              rw [instrumentLife_erasure_result (O := O) (permit := permit)
                (.enter (fn.params.map Prod.fst)) st' st'' hst'']
              rfl
            · simp only [hlen, ↓reduceIte] at h
              cases h
          | trap kd =>
            have he_erased : Exec.evalArgs O L (erase st) args = .trap kd := by
              have h0 := evalArgs_trap_erasure (O := O) (types := types) (permit := permit) L args st
              rw [he] at h0
              exact h0
            rw [he] at h
            cases h
            rw [he_erased]
            rfl
          | stuck => rw [he] at h; cases h
        | prim x p args site =>
          dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at h ⊢
          cases he : Exec.evalArgs (instrumentOperands O types permit) L st args with
          | ok r =>
            rcases r with ⟨_, _⟩
            rw [he] at h
            cases h
          | trap kd =>
            have he_erased : Exec.evalArgs O L (erase st) args = .trap kd := by
              have h0 := evalArgs_trap_erasure (O := O) (types := types) (permit := permit) L args st
              rw [he] at h0
              exact h0
            rw [he] at h
            cases h
            rw [he_erased]
            rfl
          | stuck => rw [he] at h; cases h
        | abort site =>
          cases h
          rfl
      | done =>
        cases k with
        | nil => cases h
        | cons f k =>
          cases f with
          | seq t =>
            cases h
            rfl
          | scope b dm ds =>
            cases ds with
            | nil =>
              simp only [Exec.step, Option.map_eq_some_iff] at h
              obtain ⟨st', hst', hnext⟩ := h
              cases hnext
              dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map]
              rw [dropOut_erasure_proj (O := O) (permit := permit) dm L st st' hst']
              rfl
            | cons d ds =>
              cases h
              rfl
          | test l cr t b site dm =>
            dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map, instrumentOperands] at h ⊢
            cases he : instrumentEval O permit L st t with
            | ok r =>
              rcases r with ⟨v, st'⟩
              rw [he] at h
              simp only [Exec.onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at h
              obtain ⟨go, hgo, st'', hst'', hnext⟩ := h
              rw [eval_ok_erasure_result (O := O) (types := types) L st st' t v he]
              dsimp [Exec.onRes]
              rw [hgo]
              dsimp [Option.bind]
              rw [dropOut_erasure_proj (O := O) (permit := permit) dm L st' st'' hst'']
              cases go <;> cases hnext <;> rfl
            | trap kd =>
              rw [he] at h
              cases h
              rw [eval_trap_erasure_result (O := O) (types := types) L st t kd he]
              rfl
            | stuck =>
              rw [he] at h
              cases h
          | body l cr t b site =>
            cases h
            rfl
          | resume p =>
            cases p <;> cases h <;> rfl
          | functionEnd =>
            simp only [Exec.step, Option.map_eq_some_iff] at h
            obtain ⟨st', hst', hnext⟩ := h
            cases hnext
            dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map]
            rw [instrumentLife_erasure_result (O := O) (permit := permit) .leave st st' hst']
            rfl
          | ret dst caller =>
            simp only [Exec.step, Option.map_eq_some_iff] at h
            obtain ⟨st', hst', hnext⟩ := h
            cases hnext
            dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map]
            rw [bindLife_erasure_proj (O := O) (permit := permit) dst st st' hst']
            rfl
      | exiting e =>
        dsimp [Exec.step, eraseConfig, eraseCtrl] at h ⊢
        cases k with
        | nil => cases h
        | cons f k =>
          cases f with
          | seq t =>
            cases h
            rfl
          | scope b dm ds =>
            cases ds with
            | nil =>
              simp only [Exec.stepExit, Option.map_eq_some_iff] at h
              obtain ⟨st', hst', hnext⟩ := h
              cases hnext
              simp [Exec.stepExit, eraseFrame, dropOut_erasure_proj (O := O) (permit := permit) dm L st st' hst']
            | cons d ds =>
              cases h
              rfl
          | test l cr t body site dm =>
            simp only [Exec.stepExit, Option.map_eq_some_iff] at h
            obtain ⟨st', hst', hnext⟩ := h
            cases hnext
            simp [Exec.stepExit, eraseFrame, dropOut_erasure_proj (O := O) (permit := permit) dm L st st' hst']
          | body l cr t b site =>
            cases e with
            | brk l' =>
              by_cases hl : l' = l
              · simp only [Exec.stepExit, hl, ↓reduceIte, Option.some.injEq] at h
                cases h
                simp [Exec.stepExit, eraseFrame, hl]
              · simp only [Exec.stepExit, hl, ↓reduceIte, Option.some.injEq] at h
                cases h
                simp [Exec.stepExit, eraseFrame, hl]
            | cont l' =>
              by_cases hl : l' = l
              · simp only [Exec.stepExit, hl, ↓reduceIte, Option.some.injEq] at h
                cases h
                simp [Exec.stepExit, eraseFrame, hl, eraseStmt]
              · simp only [Exec.stepExit, hl, ↓reduceIte, Option.some.injEq] at h
                cases h
                simp [Exec.stepExit, eraseFrame, hl]
            | ret v =>
              cases h
              rfl
          | resume p => cases h
          | functionEnd =>
            cases e with
            | brk _ => cases h
            | cont _ => cases h
            | ret v =>
              simp only [Exec.stepExit, Option.map_eq_some_iff] at h
              obtain ⟨st', hst', hnext⟩ := h
              cases hnext
              dsimp [Exec.stepExit, eraseFrame, eraseConfig, eraseCtrl, List.map]
              rw [instrumentLife_erasure_result (O := O) (permit := permit) .leave st st' hst']
              rfl
          | ret dst caller =>
            cases e with
            | brk _ => cases h
            | cont _ => cases h
            | ret v =>
              simp only [Exec.stepExit, Option.map_eq_some_iff] at h
              obtain ⟨st', hst', hnext⟩ := h
              cases hnext
              dsimp [Exec.stepExit, eraseFrame, eraseConfig, eraseCtrl, List.map]
              rw [bindLife_erasure_proj (O := O) (permit := permit) dst st st' hst']
              rfl
      | trapped _ _ => cases h
      | refused => cases h

/-- Law (authority: docs/decisions.md#decided-2026-11-05-core (Q9.10);
#decided-2026-11-08-recovered-d3 (C5 erasure);
#decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.stepInstrumented`). Primitive-request
forward simulation
(owner decision 2026-11-08 Q3): whenever `stepInstrumented` on the instrumented C1 machine issues a primitive
request, `Exec.step` on the erased machine issues the matching request with the erased store and agreeing
continuation. -/
theorem step_request_erasure (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    match stepInstrumented O types permit P c with
    | some (.request p vs site st k) =>
      ∃ k_erased,
        Exec.step (eraseProgram P) (eraseConfig c) =
          some (.request p vs site (erase st) k_erased) ∧
        ∀ a c', k a = some c' → k_erased (eraseAnswer a) = some (eraseConfig c')
    | _ => True := by
  cases h : stepInstrumented O types permit P c with
  | none => trivial
  | some out =>
    cases out with
    | next _ => trivial
    | request p vs site st k =>
      change Exec.step P c = some (.request p vs site st k) at h
      rcases c with ⟨ctrl, L, s, frames⟩
      cases ctrl with
      | exec stmt =>
        cases stmt with
        | skip => cases h
        | bind x e site' =>
          simp only [Exec.step, instrumentOperands] at h
          cases he : instrumentEval O permit L s e with
          | ok r =>
            rcases r with ⟨_, _⟩
            rw [he] at h
            simp only [Exec.onRes, Option.map_eq_some_iff] at h
            obtain ⟨_, _, h⟩ := h; cases h
          | trap _ => rw [he] at h; cases h
          | stuck => rw [he] at h; cases h
        | assign x e site' =>
          simp only [Exec.step, instrumentOperands] at h
          cases he : instrumentEval O permit L s e with
          | ok r =>
            rcases r with ⟨_, _⟩
            rw [he] at h
            simp only [Exec.onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at h
            obtain ⟨_, _, _, _, h⟩ := h; cases h
          | trap _ => rw [he] at h; cases h
          | stuck => rw [he] at h; cases h
        | seq _ _ => cases h
        | block _ _ => cases h
        | defer _ _ _ =>
          simp only [Exec.step, Option.map_eq_some_iff] at h
          obtain ⟨_, _, h⟩ := h; cases h
        | cond e _ _ _ =>
          simp only [Exec.step, instrumentOperands] at h
          cases he : instrumentEval O permit L s e with
          | ok r =>
            rcases r with ⟨_, _⟩
            rw [he] at h
            simp only [Exec.onRes, Option.map_eq_some_iff] at h
            obtain ⟨_, _, h⟩ := h; cases h
          | trap _ => rw [he] at h; cases h
          | stuck => rw [he] at h; cases h
        | loop _ _ _ _ _ => cases h
        | brk _ => cases h
        | cont _ => cases h
        | ret e _ =>
          simp only [Exec.step, instrumentOperands] at h
          cases he : instrumentEval O permit L s e with
          | ok r => rcases r with ⟨_, _⟩; rw [he] at h; cases h
          | trap _ => rw [he] at h; cases h
          | stuck => rw [he] at h; cases h
        | call dst f args site' =>
          simp only [Exec.step] at h
          cases he : Exec.evalArgs (instrumentOperands O types permit) L s args with
          | ok r =>
            rcases r with ⟨vs', st'⟩
            rw [he] at h
            simp only [Exec.onRes, Option.bind_eq_some_iff] at h
            obtain ⟨fn, _, h⟩ := h
            by_cases hlen : fn.params.length = vs'.length
            · simp only [hlen, ↓reduceIte, Option.map_eq_some_iff] at h
              obtain ⟨_, _, h⟩ := h; cases h
            · simp only [hlen, ↓reduceIte] at h; cases h
          | trap _ => rw [he] at h; cases h
          | stuck => rw [he] at h; cases h
        | prim x p' args site' =>
          dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at h ⊢
          cases he : Exec.evalArgs (instrumentOperands O types permit) L s args with
          | ok r =>
            rcases r with ⟨vs', st'⟩
            have he_erased : Exec.evalArgs O L (erase s) args = .ok (vs', erase st') := by
              have h0 := evalArgs_ok_erasure (O := O) (types := types) (permit := permit) L args s
              rw [he] at h0
              exact h0
            rw [he] at h
            dsimp [Exec.onRes] at h
            injection h with h
            injection h with hp hvs hsite hst hk_eq
            subst hp; subst hvs; subst hsite; subst hst; subst hk_eq
            refine ⟨_, by rw [he_erased]; rfl, ?_⟩
            intro a c' hk
            dsimp at hk
            cases ha : a.result with
            | ok v =>
              rw [ha] at hk
              simp only [Option.map_eq_some_iff] at hk
              obtain ⟨st'', hst'', rfl⟩ := hk
              dsimp [eraseAnswer]
              rw [ha]
              dsimp
              rw [bindLife_erasure_proj (O := O) (permit := permit) x a.store st'' hst'']
              rfl
            | error kd =>
              rw [ha] at hk
              cases hk
              dsimp [eraseAnswer]
              rw [ha]
          | trap _ => rw [he] at h; cases h
          | stuck => rw [he] at h; cases h
        | abort _ => cases h
      | done =>
        cases frames with
        | nil => cases h
        | cons f k' =>
          cases f with
          | seq _ => cases h
          | scope b dm ds =>
            cases ds with
            | nil =>
              simp only [Exec.step, Option.map_eq_some_iff] at h
              obtain ⟨_, _, h⟩ := h; cases h
            | cons _ _ => cases h
          | test l cr t b site' dm =>
            simp only [Exec.step, instrumentOperands] at h
            cases he : instrumentEval O permit L s t with
            | ok r =>
              rcases r with ⟨_, _⟩
              rw [he] at h
              simp only [Exec.onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at h
              obtain ⟨go, _, _, _, h⟩ := h
              cases go <;> cases h
            | trap _ => rw [he] at h; cases h
            | stuck => rw [he] at h; cases h
          | body _ _ _ _ _ => cases h
          | resume p' => cases p' <;> cases h
          | functionEnd =>
            simp only [Exec.step, Option.map_eq_some_iff] at h
            obtain ⟨_, _, h⟩ := h; cases h
          | ret _ _ =>
            simp only [Exec.step, Option.map_eq_some_iff] at h
            obtain ⟨_, _, h⟩ := h; cases h
      | exiting e =>
        dsimp [Exec.step] at h
        cases frames with
        | nil => cases h
        | cons f k' =>
          cases f with
          | seq _ => cases h
          | scope b dm ds =>
            cases ds with
            | nil =>
              simp only [Exec.stepExit, Option.map_eq_some_iff] at h
              obtain ⟨_, _, h⟩ := h; cases h
            | cons _ _ => cases h
          | test _ _ _ _ _ _ =>
            simp only [Exec.stepExit, Option.map_eq_some_iff] at h
            obtain ⟨_, _, h⟩ := h; cases h
          | body l _ _ _ _ =>
            cases e with
            | brk l' => by_cases hl : l' = l <;> simp [Exec.stepExit, hl] at h
            | cont l' => by_cases hl : l' = l <;> simp [Exec.stepExit, hl] at h
            | ret _ => cases h
          | resume _ => cases h
          | functionEnd =>
            cases e with
            | brk _ => cases h
            | cont _ => cases h
            | ret _ =>
              simp only [Exec.stepExit, Option.map_eq_some_iff] at h
              obtain ⟨_, _, h⟩ := h; cases h
          | ret _ _ =>
            cases e with
            | brk _ => cases h
            | cont _ => cases h
            | ret _ =>
              simp only [Exec.stepExit, Option.map_eq_some_iff] at h
              obtain ⟨_, _, h⟩ := h; cases h
      | trapped _ _ => cases h
      | refused => cases h

/-- Helper (projection). Erasing a step label preserves its emitted event list. -/
private theorem eraseLabel_events_proj (l : Exec.Label (instrumentOperands O types permit)) :
    (eraseLabel l).events = l.events := by
  cases l <;> rfl

/-- Helper (projection). Erasing an answer's step label equals the erased answer's label. -/
private theorem eraseLabel_answer_proj (a : Exec.Answer O.Val O.Event (Store O.Store)) :
    eraseLabel (O := O) (types := types) (permit := permit) a.label = (eraseAnswer a).label := by
  rcases a with ⟨ev, _, _⟩
  cases ev <;> rfl

/-- Law (authority: docs/decisions.md#decided-2026-11-05-core (Q9.10);
#decided-2026-11-08-recovered-d3 (C5 erasure);
#decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.stepInstrumented`). Single-step forward
simulation
(owner decision 2026-11-08 Q3): every labeled step produced by `stepInstrumented` on the instrumented C1 machine is
matched by a step of the erased machine with the same event label and concrete store. -/
theorem Step_erasure (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    match stepInstrumented O types permit P c with
    | some (.next c') =>
      Exec.Step (eraseProgram P) (eraseConfig c) .silent (eraseConfig c')
    | some (.request p vs _site st k) =>
      ∀ a c', (instrumentOperands O types permit).allows p vs st a = true →
        k a = some c' →
        Exec.Step (eraseProgram P) (eraseConfig c) (eraseLabel a.label) (eraseConfig c')
    | none => True := by
  cases hs : stepInstrumented O types permit P c with
  | none => trivial
  | some out =>
    cases out with
    | next c' =>
      have hnext := step_next_erasure P c
      rw [hs] at hnext
      exact .silent hnext
    | request p vs site st k =>
      intro a c' hallows hk
      have hreq := step_request_erasure P c
      rw [hs] at hreq
      obtain ⟨k_erased, hs_erased, hk_erased⟩ := hreq
      obtain ⟨hallows_erased, _⟩ := allows_erasure_result _ _ _ _ hallows
      rw [eraseLabel_answer_proj]
      exact .prim hs_erased hallows_erased (hk_erased _ _ hk)

/-- Law (authority: docs/decisions.md#decided-2026-11-05-core (Q9.10);
#decided-2026-11-08-recovered-d3 (C5 erasure);
#decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.stepInstrumented`). Every finite execution
of the
instrumented machine is simulated by C1 with the instrumentation erased. This is the
unconditional Q3 obligation, separate from oracle evaluation and its `Ending.NotStuck` premise. -/
theorem Steps_erasure (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    FiniteSimulation O types permit (stepInstrumented O types permit P) P c := by
  intro tr c' h
  induction h with
  | refl c => exact .refl (eraseConfig c)
  | @silent c₁ c₂ _ _ hs _ ih =>
    have hstep := Step_erasure P c₁
    rw [hs] at hstep
    exact .step hstep ih
  | @prim c₁ c₂ _ _ p vs site st k a hs ha hk _ ih =>
    have hstep := Step_erasure P c₁
    rw [hs] at hstep
    have hstep' := hstep a c₂ ha hk
    simpa [eraseLabel_events_proj] using Exec.Steps.step hstep' ih

/-- Helper (projection). Configuration finality is invariant under erasure. -/
private theorem final?_eraseConfig_proj (c : Exec.Config (instrumentOperands O types permit)) :
    (eraseConfig c).final? = c.final? := by
  rcases c with ⟨ctrl, L, st, frames⟩
  cases ctrl with
  | exec _ => rfl
  | done => cases frames <;> rfl
  | exiting e =>
    cases e with
    | brk _ => rfl
    | cont _ => rfl
    | ret _ => cases frames <;> rfl
  | trapped _ _ => rfl
  | refused => rfl

/-- Helper (projection). Lifting a concrete answer preserves the primitive contract check. -/
private theorem allows_liftAnswer_proj (p : O.Prim) (vs : List O.Val) (s : Store O.Store)
    (a : Exec.Answer O.Val O.Event O.Store) :
    (instrumentOperands O types permit).allows p vs s (liftAnswer s.own a) =
      O.allows p vs (erase s) a := by
  simp [instrumentOperands, instrumentAllows, eraseAnswer, liftAnswer, erase]

/-- Helper (projection). Refused control is preserved and reflected by erasure. -/
private theorem refused_erase (c : Exec.Config (instrumentOperands O types permit)) :
    (eraseConfig c).ctrl = .refused ↔ c.ctrl = .refused := by
  rcases c with ⟨ctrl, L, s, k⟩
  cases ctrl <;> simp [eraseConfig, eraseCtrl]

/-- Helper (computation). Exposes the sole driver's non-refused branch for induction.
It is an equation for Exec.runFrom, not a second execution policy. -/
private theorem runFrom_active {A : Exec.Operands} (P : Exec.Program A) (ρ : Exec.Oracle A)
    (fuel : Nat) (hist : List A.Event) (c : Exec.Config A) (h : c.ctrl ≠ .refused) :
    Exec.runFrom P ρ fuel hist c =
      match c.final? with
      | some f => ⟨hist, .final f, c, fuel⟩
      | none => match fuel with
        | 0 => ⟨hist, .outOfFuel, c, 0⟩
        | n + 1 => match Exec.step P c with
          | none => ⟨hist, .stuck, c, n + 1⟩
          | some (.next c') => Exec.runFrom P ρ n hist c'
          | some (.request p vs site st k) => match ρ hist p vs site st with
            | none => ⟨hist, .noAnswer, c, n + 1⟩
            | some a => if A.allows p vs st a then match k a with
              | some c' => Exec.runFrom P ρ n (hist ++ a.event.toList) c'
              | none => ⟨hist, .stuck, c, n + 1⟩
              else ⟨hist, .badAnswer, c, n + 1⟩ := by
  cases hc : c.ctrl <;> try exact False.elim (h hc)
  all_goals
    rw [Exec.runFrom.eq_def]
    simp only [hc]
    cases c.final? <;> cases fuel <;> rfl


/-- Law (authority: docs/decisions.md#decided-2026-11-05-core (Q9.10);
#decided-2026-11-08-recovered-d3 (C5 erasure);
#decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.liftOracle`). Fuel-indexed evaluator forward
simulation (owner decision 2026-11-08 Q3): whenever
the instrumented run reaches a non-stuck outcome (final—normal, returned or trapped—
noAnswer, badAnswer or outOfFuel), the erased run reaches the matching outcome with
the same events in order and the same concrete store. -/
theorem runFrom_erasure (P : Exec.Program (instrumentOperands O types permit))
    (ρ : Exec.Oracle O) :
    ∀ (fuel : Nat) (hist : List O.Event)
      (c : Exec.Config (instrumentOperands O types permit)),
      Ending.NotStuck (Exec.runFrom P (liftOracle O types permit ρ) fuel hist c).ending →
      Exec.runFrom (eraseProgram P) ρ fuel hist (eraseConfig c) =
        eraseReport (Exec.runFrom P (liftOracle O types permit ρ) fuel hist c)
  | 0, hist, c, hzero => by
    by_cases hr : c.ctrl = .refused
    · have he := (refused_erase (O := O) (types := types) (permit := permit) c).mpr hr
      simp [Exec.runFrom, hr, he, eraseReport, eraseEnding]
    · have her : (eraseConfig c).ctrl ≠ .refused := fun h => hr ((refused_erase c).mp h)
      rw [runFrom_active P _ _ _ c hr, runFrom_active (eraseProgram P) _ _ _ _ her]
      rw [final?_eraseConfig_proj]
      cases c.final? <;> rfl
  | n + 1, hist, c, hns => by
    by_cases hr : c.ctrl = .refused
    · have he := (refused_erase (O := O) (types := types) (permit := permit) c).mpr hr
      simp [Exec.runFrom, hr, he, eraseReport, eraseEnding]
    · have her : (eraseConfig c).ctrl ≠ .refused := fun h => hr ((refused_erase c).mp h)
      rw [runFrom_active P _ _ _ c hr] at hns ⊢
      rw [runFrom_active (eraseProgram P) _ _ _ _ her]
      rw [final?_eraseConfig_proj]
      cases hf : c.final? with
      | some f => rfl
      | none =>
        rw [hf] at hns
        dsimp at hns ⊢
        cases hs : Exec.step P c with
        | none =>
          rw [hs] at hns
          cases hns
        | some out =>
          rw [hs] at hns
          cases out with
          | next c' =>
            have hnext := step_next_erasure P c
            rw [show stepInstrumented O types permit P c = some (.next c') from hs] at hnext
            rw [hnext]
            exact runFrom_erasure P ρ n hist c' hns
          | request p vs site st k =>
            have hreq := step_request_erasure P c
            rw [show stepInstrumented O types permit P c = some (.request p vs site st k) from hs] at hreq
            obtain ⟨k_erased, hs_erased, hk_erased⟩ := hreq
            rw [hs_erased]
            dsimp [liftOracle] at hns ⊢
            cases hρ : ρ hist p vs site (erase st) with
            | none => rfl
            | some a₀ =>
              rw [hρ] at hns
              have ha_eq : instrumentAllows O p vs st (liftAnswer st.own a₀) =
                  O.allows p vs (erase st) a₀ :=
                allows_liftAnswer_proj (O := O) (types := types) (permit := permit) p vs st a₀
              by_cases ha : O.allows p vs (erase st) a₀ = true
              · have ha' : instrumentAllows O p vs st (liftAnswer st.own a₀) = true :=
                  ha_eq.trans ha
                dsimp [instrumentOperands] at hns ⊢
                simp only [ha', ha, ↓reduceIte] at hns ⊢
                cases hk : k (liftAnswer st.own a₀) with
                | none =>
                  rw [hk] at hns
                  cases hns
                | some c' =>
                  rw [hk] at hns
                  have hk' := hk_erased (liftAnswer st.own a₀) c' hk
                  have ha_ans : eraseAnswer (liftAnswer st.own a₀) = a₀ := by
                    cases a₀; rfl
                  rw [ha_ans] at hk'
                  rw [hk']
                  exact runFrom_erasure P ρ n (hist ++ a₀.event.toList) c' hns
              · have ha' : ¬ instrumentAllows O p vs st (liftAnswer st.own a₀) = true := by
                  rw [ha_eq]; exact ha
                dsimp [instrumentOperands] at hns ⊢
                simp only [ha', ha] at hns ⊢
                rfl


/-- Law (authority: docs/decisions.md#decided-2026-11-05-core (Q9.10);
#decided-2026-11-08-recovered-d3 (C5 erasure);
#decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.liftOracle`). Whole-run forward simulation
from an empty event history (owner decision 2026-11-08 Q3). -/
theorem run_erasure (P : Exec.Program (instrumentOperands O types permit))
    (ρ : Exec.Oracle O) (fuel : Nat)
    (c : Exec.Config (instrumentOperands O types permit))
    (h : Ending.NotStuck (Exec.run P (liftOracle O types permit ρ) fuel c).ending) :
    Exec.run (eraseProgram P) ρ fuel (eraseConfig c) =
      eraseReport (Exec.run P (liftOracle O types permit ρ) fuel c) :=
  runFrom_erasure P ρ fuel [] c h


/-- Helper (inversion). Under `EvalPermitted`, an instrumented operand evaluation that is
stuck is also stuck in the erased operand instance. -/
private theorem eval_stuck_of_permitted_inv (L : AList Var O.Val) (s : Store O.Store)
    (e : O.Expr) (hp : EvalPermitted O permit L s e)
    (hs : (instrumentOperands O types permit).eval L s e = .stuck) :
    O.eval L (erase s) e = .stuck := by
  dsimp [instrumentOperands, instrumentEval] at hs
  cases hc : O.eval L s.concrete e with
  | ok r =>
    rcases r with ⟨_, _⟩
    have hns : O.eval L s.concrete e ≠ .stuck := by rw [hc]; intro h; cases h
    obtain ⟨own', hown⟩ := Option.isSome_iff_exists.mp (hp hns)
    rw [hown, hc] at hs
    cases hs
  | trap _ =>
    have hns : O.eval L s.concrete e ≠ .stuck := by rw [hc]; intro h; cases h
    obtain ⟨own', hown⟩ := Option.isSome_iff_exists.mp (hp hns)
    rw [hown, hc] at hs
    cases hs
  | stuck => exact hc

/-- Helper (inversion). Under `LifePermitted`, an instrumented lifecycle transition that
returns `none` also returns `none` on the erased store. -/
private theorem life_none_of_permitted_inv (ev : Exec.Life) (s : Store O.Store)
    (hp : LifePermitted O types ev s)
    (hs : (instrumentOperands O types permit).life ev s = none) :
    O.life ev (erase s) = none := by
  dsimp [instrumentOperands, instrumentLife] at hs
  cases hc : O.life ev s.concrete with
  | none => exact hc
  | some _ =>
    have hsome : (O.life ev s.concrete).isSome = true := by rw [hc]; rfl
    obtain ⟨own', hown⟩ := Option.isSome_iff_exists.mp (hp hsome)
    rw [hown, hc] at hs
    cases hs

/-- Helper (inversion). Under `BindDstPermitted`, an instrumented destination-binding
lifecycle that returns `none` also returns `none` on the erased store. -/
private theorem bindLife_none_of_permitted_inv (dst : Option Var) (s : Store O.Store)
    (hp : BindDstPermitted O types dst s)
    (hs : Exec.bindLife (instrumentOperands O types permit) dst s = none) :
    Exec.bindLife O dst (erase s) = none := by
  cases dst with
  | none => cases hs
  | some x => exact life_none_of_permitted_inv (O := O) (permit := permit) (.bind x) s hp hs

/-- Helper (inversion). Under `LifePermitted`, an instrumented scope-exit drop that
returns `none` also returns `none` on the erased store. -/
private theorem dropOut_none_of_permitted_inv (dm : List Var) (L : AList Var O.Val)
    (s : Store O.Store) (hp : LifePermitted O types (.drop (Exec.dead dm L)) s)
    (hs : Exec.dropOut (instrumentOperands O types permit) dm L s = none) :
    Exec.dropOut O dm L (erase s) = none :=
  life_none_of_permitted_inv (O := O) (permit := permit) (.drop (Exec.dead dm L)) s hp hs

/-- Helper (projection). Deferred registration failure is invariant under frame erasure. -/
private theorem register_none_proj (d : Exec.Deferred (instrumentOperands O types permit)) :
    ∀ (k : List (Exec.Frame (instrumentOperands O types permit))),
      Exec.register d k = none →
      Exec.register (eraseDeferred d) (k.map eraseFrame) = none
  | [], _ => rfl
  | .seq t :: k, h => by
    simp only [Exec.register, Option.map_eq_none_iff] at h
    simp [Exec.register, eraseFrame, register_none_proj d k h]
  | .scope _ _ _ :: _, h => by cases h
  | .test .. :: _, _ => rfl
  | .body .. :: _, _ => rfl
  | .resume _ :: _, _ => rfl
  | .ret .. :: _, _ => rfl
  | .functionEnd :: _, _ => rfl

/-- Helper (inversion). `CheckerAccepted` is preserved across any single machine step. -/
private theorem checkerAccepted_step_inv
    (P : Exec.Program (instrumentOperands O types permit))
    (c c' : Exec.Config (instrumentOperands O types permit))
    (l : Exec.Label (instrumentOperands O types permit))
    (hacc : CheckerAccepted O types permit P c)
    (hs : Exec.Step P c l c') :
    CheckerAccepted O types permit P c' :=
  fun tr c'' hsteps => hacc (l.events ++ tr) c'' (.step hs hsteps)

/-- Law (authority: docs/decisions.md#decided-2026-11-05-core (Q9.10);
#decided-2026-11-08-recovered-d3 (C5 erasure);
#decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.stepInstrumented`). Single-step stuck
equivalence for checker-accepted programs (owner decision 2026-11-08 Q3):
at any configuration accepted by the ownership checker, the instrumented C1 machine has no
step iff the erased machine has no step. -/
theorem step_stuck_iff_of_accepted
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit))
    (hacc : CheckerAccepted O types permit P c) :
    stepInstrumented O types permit P c = none ↔
      Exec.step (eraseProgram P) (eraseConfig c) = none := by
  constructor
  · intro hs
    have hp : StepPermitted O types permit P c := (hacc [] c (.refl c)).2
    clear hacc
    change Exec.step P c = none at hs
    rcases c with ⟨ctrl, L, st, k⟩
    have hargs_stuck : ∀ (args : List O.Expr) (s : Store O.Store),
        ArgsPermitted O types permit L s args →
        Exec.evalArgs (instrumentOperands O types permit) L s args = .stuck →
        Exec.evalArgs O L (erase s) args = .stuck := by
      intro args
      induction args with
      | nil => intro _ _ h0; cases h0
      | cons e es ih =>
        intro s ⟨hpe, hpes⟩ h0
        simp only [Exec.evalArgs, instrumentOperands] at h0 ⊢
        cases he : instrumentEval O permit L s e with
        | ok r =>
          rcases r with ⟨v, s₁⟩
          rw [he] at h0
          rw [eval_ok_erasure_result (O := O) (types := types) L s s₁ e v he]
          dsimp [Res.bind] at h0 ⊢
          cases hes : Exec.evalArgs (instrumentOperands O types permit) L s₁ es with
          | ok r' =>
            rcases r' with ⟨_, _⟩
            simp only [instrumentOperands] at hes
            rw [hes] at h0; cases h0
          | trap _ =>
            simp only [instrumentOperands] at hes
            rw [hes] at h0; cases h0
          | stuck =>
            rw [ih s₁ (hpes v s₁ he) hes]
            rfl
        | trap _ => rw [he] at h0; cases h0
        | stuck =>
          rw [eval_stuck_of_permitted_inv (O := O) (types := types) L s e hpe he]
          rfl
    cases ctrl with
    | exec s =>
      cases s with
      | skip => cases hs
      | bind x e site =>
        obtain ⟨hpe, hpl⟩ := hp
        dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at hs ⊢
        cases he : instrumentEval O permit L st e with
        | ok r =>
          rcases r with ⟨v, st'⟩
          rw [he] at hs
          simp only [Exec.onRes, Option.map_eq_none_iff] at hs
          rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
          dsimp [Exec.onRes]
          rw [life_none_of_permitted_inv (O := O) (permit := permit) (.bind x) st' (hpl v st' he) hs]
          rfl
        | trap _ => rw [he] at hs; cases hs
        | stuck =>
          rw [eval_stuck_of_permitted_inv (O := O) (types := types) L st e hpe he]
          rfl
      | assign x e site =>
        obtain ⟨hpe, hpl⟩ := hp
        dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at hs ⊢
        cases he : instrumentEval O permit L st e with
        | ok r =>
          rcases r with ⟨v, st'⟩
          rw [he] at hs
          rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
          dsimp [Exec.onRes] at hs ⊢
          cases hlook : AList.lookup L x with
          | none => rfl
          | some w =>
            rw [hlook] at hs
            dsimp [Option.bind] at hs ⊢
            simp only [Option.map_eq_none_iff] at hs
            have hsome : (AList.lookup L x).isSome = true := by rw [hlook]; rfl
            rw [life_none_of_permitted_inv (O := O) (permit := permit) (.bind x) st'
              (hpl v st' he hsome) hs]
            rfl
        | trap _ => rw [he] at hs; cases hs
        | stuck =>
          rw [eval_stuck_of_permitted_inv (O := O) (types := types) L st e hpe he]
          rfl
      | seq _ _ => cases hs
      | block _ _ => cases hs
      | defer f args site =>
        simp only [Exec.step, Option.map_eq_none_iff] at hs
        have hreg := register_none_proj (O := O) (types := types) (permit := permit) ⟨f, args, site⟩ k hs
        simp only [eraseDeferred] at hreg
        simp [Exec.step, eraseConfig, eraseCtrl, eraseStmt, hreg]
      | cond e s t site =>
        dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at hs ⊢
        cases he : instrumentEval O permit L st e with
        | ok r =>
          rcases r with ⟨v, st'⟩
          rw [he] at hs
          simp only [Exec.onRes, Option.map_eq_none_iff] at hs
          rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
          simp [Exec.onRes, hs]
        | trap _ => rw [he] at hs; cases hs
        | stuck =>
          rw [eval_stuck_of_permitted_inv (O := O) (types := types) L st e hp he]
          rfl
      | loop _ _ _ _ _ => cases hs
      | brk _ => cases hs
      | cont _ => cases hs
      | ret e site =>
        dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at hs ⊢
        cases he : instrumentEval O permit L st e with
        | ok r => rcases r with ⟨_, _⟩; rw [he] at hs; cases hs
        | trap _ => rw [he] at hs; cases hs
        | stuck =>
          rw [eval_stuck_of_permitted_inv (O := O) (types := types) L st e hp he]
          rfl
      | call dst f args site =>
        obtain ⟨hpa, hpl⟩ := hp
        dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at hs ⊢
        cases he : Exec.evalArgs (instrumentOperands O types permit) L st args with
        | ok r =>
          rcases r with ⟨vs, st'⟩
          have he_erased : Exec.evalArgs O L (erase st) args = .ok (vs, erase st') := by
            have h0 := evalArgs_ok_erasure (O := O) (types := types) (permit := permit) L args st
            rw [he] at h0
            exact h0
          rw [he] at hs
          rw [he_erased]
          dsimp [Exec.onRes, eraseProgram] at hs ⊢
          rw [lookup_eraseProgram_proj P.fns f]
          cases hfn : AList.lookup P.fns f with
          | none => rfl
          | some fn =>
            rw [hfn] at hs
            dsimp [Option.bind, eraseFn] at hs ⊢
            by_cases hlen : fn.params.length = vs.length
            · simp only [hlen, ↓reduceIte, Option.map_eq_none_iff] at hs ⊢
              exact life_none_of_permitted_inv (O := O) (permit := permit)
                (.enter (fn.params.map Prod.fst)) st' (hpl vs st' fn he hfn hlen) hs
            · simp [hlen]
        | trap _ => rw [he] at hs; cases hs
        | stuck =>
          rw [hargs_stuck args st hpa he]
          rfl
      | prim x p args site =>
        obtain ⟨hpa, _⟩ := hp
        dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at hs ⊢
        cases he : Exec.evalArgs (instrumentOperands O types permit) L st args with
        | ok r => rcases r with ⟨_, _⟩; rw [he] at hs; cases hs
        | trap _ => rw [he] at hs; cases hs
        | stuck =>
          rw [hargs_stuck args st hpa he]
          rfl
      | abort _ => cases hs
    | done =>
      cases k with
      | nil => rfl
      | cons f k =>
        cases f with
        | seq _ => cases hs
        | scope b dm ds =>
          cases ds with
          | nil =>
            simp only [Exec.step, Option.map_eq_none_iff] at hs
            dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map]
            rw [dropOut_none_of_permitted_inv (O := O) (permit := permit) dm L st hp hs]
            rfl
          | cons _ _ => cases hs
        | test l cr t b site dm =>
          obtain ⟨hpt, hpd⟩ := hp
          dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map, instrumentOperands] at hs ⊢
          cases he : instrumentEval O permit L st t with
          | ok r =>
            rcases r with ⟨v, st'⟩
            rw [he] at hs
            rw [eval_ok_erasure_result (O := O) (types := types) L st st' t v he]
            dsimp [Exec.onRes] at hs ⊢
            cases hgo : O.test v with
            | none => rfl
            | some go =>
              rw [hgo] at hs
              dsimp [Option.bind] at hs ⊢
              simp only [Option.map_eq_none_iff] at hs
              have hsome : (O.test v).isSome = true := by rw [hgo]; rfl
              rw [dropOut_none_of_permitted_inv (O := O) (permit := permit) dm L st'
                (hpd v st' he hsome) hs]
              rfl
          | trap _ => rw [he] at hs; cases hs
          | stuck =>
            rw [eval_stuck_of_permitted_inv (O := O) (types := types) L st t hpt he]
            rfl
        | body _ _ _ _ _ => cases hs
        | resume p => cases p <;> cases hs
        | functionEnd =>
          simp only [Exec.step, Option.map_eq_none_iff] at hs
          dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map]
          rw [life_none_of_permitted_inv (O := O) (permit := permit) .leave st hp hs]
          rfl
        | ret dst caller =>
          simp only [Exec.step, Option.map_eq_none_iff] at hs
          dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map]
          rw [bindLife_none_of_permitted_inv (O := O) (permit := permit) dst st hp hs]
          rfl
    | exiting e =>
      dsimp [Exec.step, eraseConfig, eraseCtrl] at hs ⊢
      cases k with
      | nil => rfl
      | cons f k =>
        cases f with
        | seq _ => cases hs
        | scope b dm ds =>
          cases ds with
          | nil =>
            simp only [Exec.stepExit, Option.map_eq_none_iff] at hs
            dsimp [Exec.stepExit, eraseFrame, List.map]
            rw [dropOut_none_of_permitted_inv (O := O) (permit := permit) dm L st hp hs]
            rfl
          | cons _ _ => cases hs
        | test l cr t body site dm =>
          simp only [Exec.stepExit, Option.map_eq_none_iff] at hs
          dsimp [Exec.stepExit, eraseFrame, List.map]
          rw [dropOut_none_of_permitted_inv (O := O) (permit := permit) dm L st hp hs]
          rfl
        | body l cr t b site =>
          cases e with
          | brk l' =>
            by_cases hl : l' = l <;> simp [Exec.stepExit, hl] at hs
          | cont l' =>
            by_cases hl : l' = l <;> simp [Exec.stepExit, hl] at hs
          | ret _ => cases hs
        | resume _ => rfl
        | functionEnd =>
          cases e with
          | brk _ => rfl
          | cont _ => rfl
          | ret v =>
            simp only [Exec.stepExit, Option.map_eq_none_iff] at hs
            dsimp [Exec.stepExit, eraseFrame, List.map]
            rw [life_none_of_permitted_inv (O := O) (permit := permit) .leave st hp hs]
            rfl
        | ret dst caller =>
          cases e with
          | brk _ => rfl
          | cont _ => rfl
          | ret v =>
            simp only [Exec.stepExit, Option.map_eq_none_iff] at hs
            dsimp [Exec.stepExit, eraseFrame, List.map]
            rw [bindLife_none_of_permitted_inv (O := O) (permit := permit) dst st hp hs]
            rfl
    | trapped _ _ => rfl
    | refused => rfl
  · intro herased
    cases hs : stepInstrumented O types permit P c with
    | none => rfl
    | some out =>
      cases out with
      | next c' =>
        have h' := step_next_erasure P c
        rw [hs, herased] at h'
        cases h'
      | request p vs site st k =>
        have h' := step_request_erasure P c
        rw [hs] at h'
        obtain ⟨_, h', _⟩ := h'
        rw [herased] at h'
        cases h'

/-- Law (authority: docs/decisions.md#decided-2026-11-05-core (Q9.10);
#decided-2026-11-08-recovered-d3 (C5 erasure);
#decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.liftOracle`). Exact fuel-indexed simulation
for checker-accepted programs (owner decision 2026-11-08 Q3):
when the program and initial configuration satisfy `CheckerAccepted`, the erased run equals the erased report of the
instrumented run for every outcome, including stuck. -/
theorem runFrom_erasure_of_accepted
    (P : Exec.Program (instrumentOperands O types permit))
    (ρ : Exec.Oracle O) :
    ∀ (fuel : Nat) (hist : List O.Event)
      (c : Exec.Config (instrumentOperands O types permit)),
      CheckerAccepted O types permit P c →
      Exec.runFrom (eraseProgram P) ρ fuel hist (eraseConfig c) =
        eraseReport (Exec.runFrom P (liftOracle O types permit ρ) fuel hist c)
  | 0, hist, c, hzero => by
    by_cases hr : c.ctrl = .refused
    · have he := (refused_erase (O := O) (types := types) (permit := permit) c).mpr hr
      simp [Exec.runFrom, hr, he, eraseReport, eraseEnding]
    · have her : (eraseConfig c).ctrl ≠ .refused := fun h => hr ((refused_erase c).mp h)
      rw [runFrom_active P _ _ _ c hr, runFrom_active (eraseProgram P) _ _ _ _ her]
      rw [final?_eraseConfig_proj]
      cases c.final? <;> rfl
  | n + 1, hist, c, hacc => by
    by_cases hr : c.ctrl = .refused
    · have he := (refused_erase (O := O) (types := types) (permit := permit) c).mpr hr
      simp [Exec.runFrom, hr, he, eraseReport, eraseEnding]
    · have her : (eraseConfig c).ctrl ≠ .refused := fun h => hr ((refused_erase c).mp h)
      have hperm : StepPermitted O types permit P c := (hacc [] c (.refl c)).2
      rw [runFrom_active P _ _ _ c hr, runFrom_active (eraseProgram P) _ _ _ _ her]
      rw [final?_eraseConfig_proj]
      cases hf : c.final? with
      | some _ => rfl
      | none =>
        dsimp
        cases hs : Exec.step P c with
        | none =>
          have hnone : Exec.step (eraseProgram P) (eraseConfig c) = none :=
            (step_stuck_iff_of_accepted P c hacc).mp hs
          rw [hnone]
          rfl
        | some out =>
          cases out with
          | next c' =>
            have hnext := step_next_erasure P c
            rw [show stepInstrumented O types permit P c = some (.next c') from hs] at hnext
            rw [hnext]
            exact runFrom_erasure_of_accepted P ρ n hist c'
              (checkerAccepted_step_inv P c c' .silent hacc (.silent hs))
          | request p vs site st k =>
            have hreq := step_request_erasure P c
            rw [show stepInstrumented O types permit P c = some (.request p vs site st k) from hs] at hreq
            obtain ⟨k_erased, hs_erased, hk_erased⟩ := hreq
            rw [hs_erased]
            dsimp [liftOracle]
            cases hρ : ρ hist p vs site (erase st) with
            | none => rfl
            | some a₀ =>
              have ha_eq : instrumentAllows O p vs st (liftAnswer st.own a₀) =
                  O.allows p vs (erase st) a₀ :=
                allows_liftAnswer_proj (O := O) (types := types) (permit := permit) p vs st a₀
              by_cases ha : O.allows p vs (erase st) a₀ = true
              · have ha' : (instrumentOperands O types permit).allows p vs st
                    (liftAnswer st.own a₀) = true := ha_eq.trans ha
                have ha_inst : instrumentAllows O p vs st (liftAnswer st.own a₀) = true := ha'
                dsimp [instrumentOperands]
                simp only [ha_inst, ha, ↓reduceIte]
                cases hk : k (liftAnswer st.own a₀) with
                | none =>
                  have hreq_none : ∃ k_e2,
                      Exec.step (eraseProgram P) (eraseConfig c) =
                        some (.request p vs site (erase st) k_e2) ∧
                      k_e2 (eraseAnswer (liftAnswer st.own a₀)) = none := by
                    clear hacc hf hs_erased hk_erased ha_eq hρ ha ha_inst
                    generalize liftAnswer st.own a₀ = a at ha' hk ⊢
                    rcases c with ⟨ctrl, L, s, frames⟩
                    cases ctrl with
                    | exec stmt =>
                      cases stmt with
                      | skip => cases hs
                      | bind x e _ =>
                        simp only [Exec.step, instrumentOperands] at hs
                        cases he : instrumentEval O permit L s e with
                        | ok r =>
                          rcases r with ⟨_, _⟩; rw [he] at hs
                          simp only [Exec.onRes, Option.map_eq_some_iff] at hs
                          obtain ⟨_, _, hs⟩ := hs; cases hs
                        | trap _ => rw [he] at hs; cases hs
                        | stuck => rw [he] at hs; cases hs
                      | assign x e _ =>
                        simp only [Exec.step, instrumentOperands] at hs
                        cases he : instrumentEval O permit L s e with
                        | ok r =>
                          rcases r with ⟨_, _⟩; rw [he] at hs
                          simp only [Exec.onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at hs
                          obtain ⟨_, _, _, _, hs⟩ := hs; cases hs
                        | trap _ => rw [he] at hs; cases hs
                        | stuck => rw [he] at hs; cases hs
                      | seq _ _ => cases hs
                      | block _ _ => cases hs
                      | defer _ _ _ =>
                        simp only [Exec.step, Option.map_eq_some_iff] at hs
                        obtain ⟨_, _, hs⟩ := hs; cases hs
                      | cond e _ _ _ =>
                        simp only [Exec.step, instrumentOperands] at hs
                        cases he : instrumentEval O permit L s e with
                        | ok r =>
                          rcases r with ⟨_, _⟩; rw [he] at hs
                          simp only [Exec.onRes, Option.map_eq_some_iff] at hs
                          obtain ⟨_, _, hs⟩ := hs; cases hs
                        | trap _ => rw [he] at hs; cases hs
                        | stuck => rw [he] at hs; cases hs
                      | loop _ _ _ _ _ => cases hs
                      | brk _ => cases hs
                      | cont _ => cases hs
                      | ret e _ =>
                        simp only [Exec.step, instrumentOperands] at hs
                        cases he : instrumentEval O permit L s e with
                        | ok r => rcases r with ⟨_, _⟩; rw [he] at hs; cases hs
                        | trap _ => rw [he] at hs; cases hs
                        | stuck => rw [he] at hs; cases hs
                      | call dst f args _ =>
                        simp only [Exec.step] at hs
                        cases he : Exec.evalArgs (instrumentOperands O types permit) L s args with
                        | ok r =>
                          rcases r with ⟨vs', st'⟩; rw [he] at hs
                          simp only [Exec.onRes, Option.bind_eq_some_iff] at hs
                          obtain ⟨fn, _, hs⟩ := hs
                          by_cases hlen : fn.params.length = vs'.length
                          · simp only [hlen, ↓reduceIte, Option.map_eq_some_iff] at hs
                            obtain ⟨_, _, hs⟩ := hs; cases hs
                          · simp only [hlen, ↓reduceIte] at hs; cases hs
                        | trap _ => rw [he] at hs; cases hs
                        | stuck => rw [he] at hs; cases hs
                      | prim x p' args site' =>
                        obtain ⟨_, hpb⟩ := hperm
                        dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at hs ⊢
                        cases he : Exec.evalArgs (instrumentOperands O types permit) L s args with
                        | ok r =>
                          rcases r with ⟨vs', st'⟩
                          have he_erased : Exec.evalArgs O L (erase s) args = .ok (vs', erase st') := by
                            have h0 := evalArgs_ok_erasure (O := O) (types := types) (permit := permit) L args s
                            rw [he] at h0
                            exact h0
                          rw [he] at hs
                          dsimp [Exec.onRes] at hs
                          injection hs with hs
                          injection hs with hp_eq hvs hsite hst hk_eq
                          subst hp_eq; subst hvs; subst hsite; subst hst; subst hk_eq
                          refine ⟨_, by rw [he_erased]; rfl, ?_⟩
                          dsimp at hk ⊢
                          cases ha : a.result with
                          | ok v =>
                            rw [ha] at hk
                            simp only [Option.map_eq_none_iff] at hk
                            have hok : a.result.isOk = true := by rw [ha]; rfl
                            have hbind := hpb vs' st' a he ha' hok
                            dsimp [eraseAnswer]
                            rw [ha]
                            dsimp
                            rw [bindLife_none_of_permitted_inv (O := O) (permit := permit) x a.store hbind hk]
                            rfl
                          | error kd =>
                            rw [ha] at hk
                            cases hk
                        | trap _ => rw [he] at hs; cases hs
                        | stuck => rw [he] at hs; cases hs
                      | abort _ => cases hs
                    | done =>
                      cases frames with
                      | nil => cases hs
                      | cons f k' =>
                        cases f with
                        | seq _ => cases hs
                        | scope b dm ds =>
                          cases ds with
                          | nil =>
                            simp only [Exec.step, Option.map_eq_some_iff] at hs
                            obtain ⟨_, _, hs⟩ := hs; cases hs
                          | cons _ _ => cases hs
                        | test l cr t b site' dm =>
                          simp only [Exec.step, instrumentOperands] at hs
                          cases he : instrumentEval O permit L s t with
                          | ok r =>
                            rcases r with ⟨_, _⟩; rw [he] at hs
                            simp only [Exec.onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at hs
                            obtain ⟨go, _, _, _, hs⟩ := hs
                            cases go <;> cases hs
                          | trap _ => rw [he] at hs; cases hs
                          | stuck => rw [he] at hs; cases hs
                        | body _ _ _ _ _ => cases hs
                        | resume p' => cases p' <;> cases hs
                        | functionEnd =>
                          simp only [Exec.step, Option.map_eq_some_iff] at hs
                          obtain ⟨_, _, hs⟩ := hs; cases hs
                        | ret _ _ =>
                          simp only [Exec.step, Option.map_eq_some_iff] at hs
                          obtain ⟨_, _, hs⟩ := hs; cases hs
                    | exiting e =>
                      dsimp [Exec.step] at hs
                      cases frames with
                      | nil => cases hs
                      | cons f k' =>
                        cases f with
                        | seq _ => cases hs
                        | scope b dm ds =>
                          cases ds with
                          | nil =>
                            simp only [Exec.stepExit, Option.map_eq_some_iff] at hs
                            obtain ⟨_, _, hs⟩ := hs; cases hs
                          | cons _ _ => cases hs
                        | test _ _ _ _ _ _ =>
                          simp only [Exec.stepExit, Option.map_eq_some_iff] at hs
                          obtain ⟨_, _, hs⟩ := hs; cases hs
                        | body l _ _ _ _ =>
                          cases e with
                          | brk l' => by_cases hl : l' = l <;> simp [Exec.stepExit, hl] at hs
                          | cont l' => by_cases hl : l' = l <;> simp [Exec.stepExit, hl] at hs
                          | ret _ => cases hs
                        | resume _ => cases hs
                        | functionEnd =>
                          cases e with
                          | brk _ => cases hs
                          | cont _ => cases hs
                          | ret _ =>
                            simp only [Exec.stepExit, Option.map_eq_some_iff] at hs
                            obtain ⟨_, _, hs⟩ := hs; cases hs
                        | ret _ _ =>
                          cases e with
                          | brk _ => cases hs
                          | cont _ => cases hs
                          | ret _ =>
                            simp only [Exec.stepExit, Option.map_eq_some_iff] at hs
                            obtain ⟨_, _, hs⟩ := hs; cases hs
                    | trapped _ _ => cases hs
                    | refused => cases hs
                  obtain ⟨k_e2, hs_e2, hk_none⟩ := hreq_none
                  rw [hs_erased] at hs_e2
                  injection hs_e2 with hs_e2
                  injection hs_e2 with _ _ _ _ hk_eq
                  subst hk_eq
                  have ha_ans : eraseAnswer (liftAnswer st.own a₀) = a₀ := by
                    cases a₀; rfl
                  rw [ha_ans] at hk_none
                  rw [hk_none]
                  rfl
                | some c' =>
                  have hk' := hk_erased (liftAnswer st.own a₀) c' hk
                  have ha_ans : eraseAnswer (liftAnswer st.own a₀) = a₀ := by
                    cases a₀; rfl
                  rw [ha_ans] at hk'
                  rw [hk']
                  exact runFrom_erasure_of_accepted P ρ n (hist ++ a₀.event.toList) c'
                    (checkerAccepted_step_inv P c c' (liftAnswer st.own a₀).label hacc
                      (.prim hs ha' hk))
              · have ha_inst : ¬ instrumentAllows O p vs st (liftAnswer st.own a₀) = true := by
                  rw [ha_eq]; exact ha
                dsimp [instrumentOperands]
                simp only [ha_inst, ha]
                rfl

  /-- Helper (projection). `Ending.IsStuck` is invariant under `eraseEnding`. -/
  private theorem isStuck_eraseEnding_proj (e : Exec.Ending (instrumentOperands O types permit)) :
      Ending.IsStuck (eraseEnding e) = Ending.IsStuck e := by
    cases e <;> rfl


/-- Law (authority: docs/decisions.md#decided-2026-11-05-core (Q9.10);
#decided-2026-11-08-recovered-d3 (C5 erasure);
#decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.liftOracle`). Converse stuck equivalence for
checker-accepted programs (owner decision 2026-11-08 Q3):
for a program and initial configuration accepted by the ownership checker, the instrumented
C1 machine is stuck iff the erased machine is stuck. -/
theorem run_stuck_iff_of_accepted
    (P : Exec.Program (instrumentOperands O types permit))
    (ρ : Exec.Oracle O) (fuel : Nat)
    (c : Exec.Config (instrumentOperands O types permit))
    (hacc : CheckerAccepted O types permit P c) :
    Ending.IsStuck (Exec.run P (liftOracle O types permit ρ) fuel c).ending =
      Ending.IsStuck (Exec.run (eraseProgram P) ρ fuel (eraseConfig c)).ending := by
  unfold Exec.run
  rw [runFrom_erasure_of_accepted P ρ fuel [] c hacc]
  exact (isStuck_eraseEnding_proj _).symm

end ExecErasure

namespace ExecErasureWitnesses
open _root_.ConcreteSpec.Core.Exec
open ConcreteSpec.Core.Own.Examples

def emptyProg : Exec.Program instrumentedOps := ⟨[]⟩

def bindMoveConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.bind y (.move x) ⟨0⟩), [(x, 7)], ⟨0, initial⟩, []⟩

def primOneConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.prim (some y) () [.read x] ⟨0⟩), [(x, 7)], ⟨0, initial⟩, []⟩

/-- Helper (inversion). A step from a configuration whose `Exec.step` returns a next configuration must
land in that target configuration. -/
private theorem step_det_next_inv {O : Exec.Operands} {P : Exec.Program O}
    {c c₁ c' : Exec.Config O} {l : Exec.Label O}
    (hs : Exec.step P c = some (.next c₁)) (hstep : Exec.Step P c l c') :
    c' = c₁ := by
  cases hstep with
  | silent h =>
    rw [hs] at h
    injection h with h
    injection h with h
    exact h.symm
  | prim h _ _ =>
    rw [hs] at h
    cases h

/-- Helper (inversion). A configuration whose `Exec.step` is `none` cannot take an `Exec.Step`. -/
private theorem step_det_none_inv {O : Exec.Operands} {P : Exec.Program O}
    {c c' : Exec.Config O} {l : Exec.Label O}
    (hs : Exec.step P c = none) (hstep : Exec.Step P c l c') : False := by
  cases hstep with
  | silent h => rw [hs] at h; cases h
  | prim h _ _ => rw [hs] at h; cases h

/-- Helper (inversion). The moved-owner program `moveConfig` is rejected by `CheckerAccepted`
because after moving `x` to `y` it reaches a read of `x` where concrete evaluation succeeds
while `permitE` refuses. -/
private theorem move_not_checkerAccepted_inv :
    ¬ CheckerAccepted erasedOps types permitE emptyProg moveConfig := by
  intro hacc
  have hsteps : Exec.Steps emptyProg moveConfig [] _ :=
    .step (.silent rfl) (.step (.silent rfl) (.step (.silent rfl) (.refl _)))
  have hperm := (hacc _ _ hsteps).2
  have hok : erasedEval [(y, 7), (x, 7)] 0 (.read x) ≠ .stuck := by decide
  have hsome := hperm hok
  cases hsome

/-- Helper (inversion). The valid move-and-return program `acceptedMoveConfig`
satisfies `CheckerAccepted`. -/
private theorem acceptedMove_checkerAccepted_inv :
    CheckerAccepted erasedOps types permitE emptyProg acceptedMoveConfig := by
  intro tr c hsteps
  cases hsteps with
  | refl => exact ⟨by decide, trivial⟩
  | step hs₁ hsteps =>
    obtain rfl := step_det_next_inv rfl hs₁
    cases hsteps with
    | refl =>
      refine ⟨by decide, ⟨fun _ => by decide, fun v s' he => ?_⟩⟩
      intro _
      have he' : instrumentEval erasedOps permitE [(x, 7)] ⟨0, initial⟩ (.move x) = .ok (v, s') := he
      injection he' with hpair
      rcases Prod.mk.inj hpair with ⟨_, rfl⟩
      decide
    | step hs₂ hsteps =>
      obtain rfl := step_det_next_inv rfl hs₂
      cases hsteps with
      | refl => exact ⟨by decide, trivial⟩
      | step hs₃ hsteps =>
        obtain rfl := step_det_next_inv rfl hs₃
        cases hsteps with
        | refl => exact ⟨by decide, fun _ => by decide⟩
        | step hs₄ hsteps =>
          obtain rfl := step_det_next_inv rfl hs₄
          cases hsteps with
          | refl => exact ⟨by decide, fun _ => by decide⟩
          | step hs₅ hsteps =>
            obtain rfl := step_det_next_inv rfl hs₅
            cases hsteps with
            | refl => exact ⟨by decide, trivial⟩
            | step hs₆ _ => exact False.elim (step_det_none_inv rfl hs₆)

/-- Helper (inversion). The unbound-assignment configuration `acceptedStuckConfig`
satisfies `CheckerAccepted` and gets stuck in both the instrumented and erased machines. -/
private theorem acceptedStuck_checkerAccepted_inv :
    CheckerAccepted erasedOps types permitE emptyProg acceptedStuckConfig := by
  intro tr c hsteps
  cases hsteps with
  | refl =>
    exact ⟨by decide, ⟨fun _ => by decide, fun _ _ _ hlook => by cases hlook⟩⟩
  | step hs _ => exact False.elim (step_det_none_inv rfl hs)

/-- Instance. Moving an initialized owner in the instrumented operand instance erases to the concrete value and
store. -/
example : ∃ s' : Store Int,
    instrumentedOps.eval [(x, 7)] ⟨0, initial⟩ (.move x) = .ok (7, s') ∧
    erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.move x) = .ok (7, erase s') := by
  refine ⟨⟨0, (move initial ⟨0⟩).getD initial⟩, by decide, ?_⟩
  exact eval_ok_erasure_result (O := erasedOps) (types := types) (permit := permitE)
    [(x, 7)] ⟨0, initial⟩ _ (.move x) 7 (by decide)

/-- Instance (refused comparison). A corrupted concrete value disagrees with the erased evaluation of `.move x`. -/
example : ¬ (erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.move x) = .ok (8, 0)) := by
  decide

/-- Instance. A trapping operand in the instrumented instance traps with the same kind in the erased instance. -/
example : instrumentedOps.eval [(x, 7)] ⟨0, initial⟩ (.fail .abort) = .trap .abort ∧
    erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.fail .abort) = .trap .abort :=
  ⟨by decide, eval_trap_erasure_result (O := erasedOps) (types := types) (permit := permitE)
    [(x, 7)] ⟨0, initial⟩ (.fail .abort) .abort (by decide)⟩

/-- Instance (refused comparison). A trapping operand does not erase to a normal value. -/
example : ¬ (erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.fail .abort) = .ok (0, 0)) := by
  decide

/-- Instance. Left-to-right instrumented argument evaluation erases to the concrete argument values and store. -/
example : ∃ s' : Store Int,
    Exec.evalArgs instrumentedOps [(x, 7)] ⟨0, initial⟩ [.read x, .move x] = .ok ([7, 7], s') ∧
    Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.read x, .move x] =
      .ok ([7, 7], erase s') :=
  ⟨⟨0, (move initial ⟨0⟩).getD initial⟩, by decide, by decide⟩

/-- Instance (refused comparison). Reversing or corrupting the argument list violates erased argument evaluation. -/
example : ¬ (Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.read x, .move x] =
    .ok ([7, 0], 0)) := by
  decide

/-- Instance. An argument list that traps after a move traps with the same kind in the erased instance. -/
example : Exec.evalArgs instrumentedOps [(x, 7)] ⟨0, initial⟩ [.move x, .fail .overflow] = .trap .overflow ∧
    Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.move x, .fail .overflow] =
      .trap .overflow :=
  ⟨by decide, by decide⟩

/-- Instance (refused comparison). A trapping argument list cannot erase to a different trap kind. -/
example : ¬ (Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.move x, .fail .overflow] =
    .trap .abort) := by
  decide

/-- Instance. Condition testing agrees between the instrumented and erased instances on both truth values. -/
example : instrumentedOps.test 7 = erasedOps.test 7 ∧
    instrumentedOps.test 0 = erasedOps.test 0 ∧
    erasedOps.test 7 = some true ∧ erasedOps.test 0 = some false :=
  ⟨test_erasure_result (O := erasedOps) (types := types) (permit := permitE) 7,
   test_erasure_result (O := erasedOps) (types := types) (permit := permitE) 0,
   by decide, by decide⟩

/-- Instance (refused comparison). A nonzero condition value does not test as `false`. -/
example : ¬ (erasedOps.test 7 = some false) := by decide

/-- Instance. Binding a fresh variable updates the ownership frame and erases to the concrete lifecycle step. -/
example : ∃ s' : Store Int,
    instrumentedOps.life (.bind y) ⟨7, initial⟩ = some s' ∧
    erasedOps.life (.bind y) (erase (⟨7, initial⟩ : Store Int)) = some (erase s') := by
  refine ⟨⟨7, (bind types y initial).getD initial⟩, by decide, ?_⟩
  exact instrumentLife_erasure_result (O := erasedOps) (types := types) (permit := permitE)
    (.bind y) ⟨7, initial⟩ _ (by decide)

/-- Instance (refused comparison). A concrete lifecycle step cannot alter the concrete store to a different integer.
-/
example : ¬ (erasedOps.life (.bind y) (erase (⟨7, initial⟩ : Store Int)) = some 99) := by
  decide

/-- Instance. An allowed primitive answer satisfies the erased primitive contract and preserves the ownership state.
-/
example : let a : Exec.Answer Int Int (Store Int) := ⟨some 7, .ok 7, ⟨7, initial⟩⟩
    instrumentedOps.allows () [7] ⟨0, initial⟩ a = true ∧
    erasedOps.allows () [7] (erase (⟨0, initial⟩ : Store Int)) (eraseAnswer a) = true ∧
    a.store.own = initial := by
  dsimp
  exact ⟨by decide, allows_erasure_result (O := erasedOps) (types := types) (permit := permitE)
    () [7] ⟨0, initial⟩ ⟨some 7, .ok 7, ⟨7, initial⟩⟩ (by decide)⟩

/-- Instance (refused comparison). A primitive answer that mutates the ownership state or emits the wrong event is
rejected by `instrumentAllows`. -/
example : ¬ (instrumentedOps.allows () [7] ⟨0, initial⟩
    ⟨some 7, .ok 7, ⟨7, (move initial ⟨0⟩).getD initial⟩⟩ = true) := by
  decide

/-- Instance. A silent `.bind` step on the instrumented machine erases to the matching `.bind` step on the erased
machine. -/
example : ∃ c' : Exec.Config instrumentedOps,
    Exec.step emptyProg bindMoveConfig = some (.next c') ∧
    Exec.step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) =
      some (.next (eraseConfig c')) :=
  ⟨_, rfl, @step_next_erasure erasedOps types permitE emptyProg bindMoveConfig⟩

/-- Instance (refused comparison). A silent step cannot land in a configuration with a different local binding
value. -/
example : ¬ (Exec.step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) =
    some (.next ⟨.done, [(y, 99), (x, 7)], 0, []⟩)) := by
  intro h
  cases h

/-- Instance. A primitive request step erases to the concrete request and its continuation commutes with
`eraseConfig`. -/
example : ∃ p vs site st k,
    Exec.step emptyProg primOneConfig = some (.request p vs site st k) ∧
    ∃ k_erased,
      Exec.step (eraseProgram emptyProg) (eraseConfig primOneConfig) =
        some (.request p vs site (erase st) k_erased) ∧
      ∀ a c', k a = some c' → k_erased (eraseAnswer a) = some (eraseConfig c') :=
  ⟨(), [7], ⟨0⟩, ⟨0, initial⟩, _, rfl,
    @step_request_erasure erasedOps types permitE emptyProg primOneConfig⟩

/-- Instance (refused comparison). A primitive request step never erases to `none`. -/
example : ¬ (Exec.step (eraseProgram emptyProg) (eraseConfig primOneConfig) = none) := by
  intro h
  cases h

/-- Instance. Both silent and primitive-event single steps of the instrumented machine erase to matching steps of
the erased machine. -/
example : ∃ c' : Exec.Config instrumentedOps,
    Exec.Step emptyProg primOneConfig
      (liftAnswer initial ⟨some 7, .ok 7, 7⟩).label c' ∧
    Exec.Step (eraseProgram emptyProg) (eraseConfig primOneConfig)
      (eraseLabel (O := erasedOps) (types := types) (permit := permitE)
        (liftAnswer initial ⟨some 7, .ok 7, 7⟩).label)
      (eraseConfig c') := by
  have hs : Exec.Step emptyProg primOneConfig
      (liftAnswer initial ⟨some 7, .ok 7, 7⟩).label _ :=
    .prim rfl (by decide) rfl
  exact ⟨_, hs, @Step_erasure erasedOps types permitE emptyProg primOneConfig
    (liftAnswer initial ⟨some 7, .ok 7, 7⟩) _ (by decide) rfl⟩

/-- Instance (refused comparison). Erasing an event label never drops or alters its emitted event. -/
example : ¬ ((eraseLabel (O := erasedOps) (types := types) (permit := permitE)
    (liftAnswer initial ⟨some 7, .ok 7, 7⟩).label).events = [8]) := by
  decide

/-- Instance. A multi-step instrumented run emitting two primitive events `[7, 3]` in order erases to a multi-step
erased run with the exact same event trace `[7, 3]` and concrete store `10`. -/
example : ∃ c' : Exec.Config instrumentedOps,
    Exec.Steps emptyProg primTraceConfig [7, 3] c' ∧
    Exec.Steps (eraseProgram emptyProg) (eraseConfig primTraceConfig) [7, 3] (eraseConfig c') ∧
    erase c'.store = 10 ∧ c'.final? = some (.returned 7) := by
  have hsteps : Exec.Steps emptyProg primTraceConfig _ _ :=
    .step (.silent rfl)
      (.step (.prim (a := liftAnswer ((move initial ⟨0⟩).getD initial) ⟨some 7, .ok 7, 7⟩) rfl rfl rfl)
        (.step (.silent rfl)
          (.step (.silent rfl)
            (.step (.prim (a := liftAnswer ((bind types y ((move initial ⟨0⟩).getD initial)).getD initial) ⟨some 3,
                .ok 10, 10⟩)
                rfl rfl rfl)
              (.step (.silent rfl)
                (.step (.silent rfl) (.step (.silent rfl) (.refl _))))))))
  have hfin : FiniteExecution (stepInstrumented erasedOps types permitE emptyProg) primTraceConfig _ _ :=
    .silent rfl
      (.prim (a := liftAnswer ((move initial ⟨0⟩).getD initial) ⟨some 7, .ok 7, 7⟩) rfl rfl rfl
        (.silent rfl
          (.silent rfl
            (.prim (a := liftAnswer ((bind types y ((move initial ⟨0⟩).getD initial)).getD initial) ⟨some 3, .ok 10,
                10⟩)
                rfl rfl rfl
              (.silent rfl
                (.silent rfl (.silent rfl (.refl _))))))))
  exact ⟨_, hsteps, @Steps_erasure erasedOps types permitE emptyProg primTraceConfig [7, 3] _ hfin, rfl, rfl⟩

/-- Instance (refused comparison). Reversing the event order `[3, 7]` or corrupting the final store falsifies the
erased trace report. -/
example : ¬ ((Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primTraceConfig)).events = [3, 7]) := by
  decide

/-- Instance. Non-`stuck` fuel-indexed runs (`returned`, `normal`, `trapped`, `noAnswer`, `badAnswer` and
`outOfFuel`) erase with identical events and concrete stores. -/
example :
    let ρI := liftOracle erasedOps types permitE erasedOracle
    let cTrap : Exec.Config instrumentedOps :=
      ⟨.exec (.ret (.fail .abort) ⟨0⟩), [(x, 7)], ⟨0, initial⟩, []⟩
    let cNoAns : Exec.Config instrumentedOps :=
      ⟨.exec (.prim none () [.lit 1, .lit 2] ⟨0⟩), [(x, 7)], ⟨0, initial⟩, []⟩
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [1] (eraseConfig primTraceConfig) =
      eraseReport (Exec.runFrom emptyProg ρI 100 [1] primTraceConfig) ∧
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig cTrap) =
      eraseReport (Exec.runFrom emptyProg ρI 100 [] cTrap) ∧
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig cNoAns) =
      eraseReport (Exec.runFrom emptyProg ρI 100 [] cNoAns) ∧
    Exec.runFrom (eraseProgram emptyProg) badOracle 100 [] (eraseConfig primTraceConfig) =
      eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE badOracle) 100 [] primTraceConfig) ∧
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 2 [] (eraseConfig primTraceConfig) =
      eraseReport (Exec.runFrom emptyProg ρI 2 [] primTraceConfig) :=
  ⟨runFrom_erasure emptyProg erasedOracle 100 [1] primTraceConfig rfl,
   runFrom_erasure emptyProg erasedOracle 100 [] _ rfl,
   runFrom_erasure emptyProg erasedOracle 100 [] _ rfl,
   runFrom_erasure emptyProg badOracle 100 [] primTraceConfig rfl,
   runFrom_erasure emptyProg erasedOracle 2 [] primTraceConfig rfl⟩

/-- Instance (refused comparison). Dropping the `NotStuck` premise fails on the moved-owner counterexample: the
instrumented run is `stuck` while the erased run returns `7`. -/
example : ¬ (Exec.runFrom (eraseProgram emptyProg)
      erasedOracle 100 [] (eraseConfig moveConfig) =
    eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] moveConfig)) := by
  intro h
  have h1 := congrArg (fun r => Ending.IsStuck r.ending) h
  revert h1
  decide

/-- Instance. A full instrumented run with primitive events `[7, 3]`, store updates `0 → 7 → 10`, and return value
`7` erases to the identical concrete run. -/
example :
    Exec.run (eraseProgram emptyProg)
      erasedOracle 100 (eraseConfig primTraceConfig) =
    eraseReport (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 primTraceConfig) ∧
    (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primTraceConfig)).events = [7, 3] ∧
    (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primTraceConfig)).ending =
      .final (.returned 7) :=
  ⟨run_erasure emptyProg erasedOracle 100 primTraceConfig rfl, rfl, rfl⟩

/-- Instance (refused comparison). The moved-owner counterexample refutes unconditional converse erasure on
`Exec.run`: instrumented is `stuck`, erased returns `7`. -/
example : ¬ (erasedMoveRun = eraseReport instrumentedMoveRun) := by
  intro h
  have h1 := congrArg (fun r => Ending.IsStuck r.ending) h
  revert h1
  decide

/-- Instance. On checker-accepted configurations (`acceptedMoveConfig` and `acceptedStuckConfig`), the instrumented
step is `none` iff the erased step is `none`, while `CheckerAccepted` rejects `moveConfig`. -/
example :
    CheckerAccepted erasedOps types permitE emptyProg acceptedMoveConfig ∧
    CheckerAccepted erasedOps types permitE emptyProg acceptedStuckConfig ∧
    ¬ CheckerAccepted erasedOps types permitE emptyProg moveConfig ∧
    (Exec.step emptyProg acceptedMoveConfig = none ↔
      Exec.step (eraseProgram emptyProg) (eraseConfig acceptedMoveConfig) = none) ∧
    (Exec.step emptyProg acceptedStuckConfig = none ↔
      Exec.step (eraseProgram emptyProg) (eraseConfig acceptedStuckConfig) = none) :=
  ⟨acceptedMove_checkerAccepted_inv,
   acceptedStuck_checkerAccepted_inv,
   move_not_checkerAccepted_inv,
   step_stuck_iff_of_accepted emptyProg acceptedMoveConfig acceptedMove_checkerAccepted_inv,
   step_stuck_iff_of_accepted emptyProg acceptedStuckConfig acceptedStuck_checkerAccepted_inv⟩

/-- Instance (refused comparison). At the unaccepted moved-read step `.ret (.read x)`, the instrumented step is
`none` while the erased step succeeds. -/
example :
    let cMovedRead : Exec.Config instrumentedOps :=
      ⟨.exec (.ret (.read x) ⟨1⟩), [(y, 7), (x, 7)], ⟨7, (move initial ⟨0⟩).getD initial⟩, []⟩
    ¬ (Exec.step emptyProg cMovedRead = none ↔
      Exec.step (eraseProgram emptyProg) (eraseConfig cMovedRead) = none) := by
  dsimp
  intro h
  have h1 : Exec.step emptyProg
      ⟨.exec (.ret (.read x) ⟨1⟩), [(y, 7), (x, 7)], ⟨7, (move initial ⟨0⟩).getD initial⟩, []⟩ = none := rfl
  have h2 := h.mp h1
  cases h2

/-- Instance. For both non-stuck (`acceptedMoveConfig`) and stuck (`acceptedStuckConfig`) checker-accepted programs,
`runFrom` on the erased machine equals `eraseReport` of the instrumented `runFrom`. -/
example :
    let ρI := liftOracle erasedOps types permitE erasedOracle
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig acceptedMoveConfig) =
      eraseReport (Exec.runFrom emptyProg ρI 100 [] acceptedMoveConfig) ∧
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig acceptedStuckConfig) =
      eraseReport (Exec.runFrom emptyProg ρI 100 [] acceptedStuckConfig) :=
  ⟨runFrom_erasure_of_accepted emptyProg erasedOracle 100 [] acceptedMoveConfig
      acceptedMove_checkerAccepted_inv,
   runFrom_erasure_of_accepted emptyProg erasedOracle 100 [] acceptedStuckConfig
      acceptedStuck_checkerAccepted_inv⟩

/-- Instance (refused comparison). Without `CheckerAccepted`, `moveConfig` violates `runFrom` erasure because
instrumentation refuses `.read x` after `.move x`. -/
example : ¬ (Exec.runFrom (eraseProgram emptyProg)
      erasedOracle 100 [] (eraseConfig moveConfig) =
    eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] moveConfig)) := by
  intro h
  have h1 := congrArg (fun r => Ending.IsStuck r.ending) h
  revert h1
  decide

/-- Instance. For checker-accepted programs, the instrumented run is `stuck` iff the erased run is `stuck` (`false`
on `acceptedMoveConfig`, `true` on `acceptedStuckConfig`). -/
example :
    let ρI := liftOracle erasedOps types permitE erasedOracle
    Ending.IsStuck (Exec.run emptyProg ρI 100 acceptedMoveConfig).ending =
      Ending.IsStuck (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig acceptedMoveConfig)).ending ∧
    Ending.IsStuck (Exec.run emptyProg ρI 100 acceptedMoveConfig).ending = false ∧
    Ending.IsStuck (Exec.run emptyProg ρI 100 acceptedStuckConfig).ending =
      Ending.IsStuck (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig acceptedStuckConfig)).ending ∧
    Ending.IsStuck (Exec.run emptyProg ρI 100 acceptedStuckConfig).ending = true :=
  ⟨run_stuck_iff_of_accepted emptyProg erasedOracle 100 acceptedMoveConfig acceptedMove_checkerAccepted_inv,
   by decide,
   run_stuck_iff_of_accepted emptyProg erasedOracle 100 acceptedStuckConfig acceptedStuck_checkerAccepted_inv,
   by decide⟩

/-- Instance (refused comparison). Without `CheckerAccepted`, `moveConfig` is stuck in the instrumented run (`true`)
and not stuck in the erased run (`false`). -/
example : ¬ (Ending.IsStuck instrumentedMoveRun.ending = Ending.IsStuck erasedMoveRun.ending) := by
  decide

/-- Candidate unconditional converse, deliberately without checker acceptance. The moved-owner
execution below refutes this stronger statement; it is not a control for a conditional law. -/
def UnconditionalRunConverse : Prop :=
  ∀ (fuel : Nat) (c : Exec.Config instrumentedOps),
    Exec.run (eraseProgram emptyProg) erasedOracle fuel (eraseConfig c) =
      eraseReport (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) fuel c)

/-- Helper (computation). The owner-required moved-owner execution refutes the explicitly
unconditional candidate. This makes no claim against the checker-accepted converse. -/
theorem unconditionalRunConverse_refuted : ¬ UnconditionalRunConverse := by
  intro h
  have he := congrArg (fun r => Ending.IsStuck r.ending) (h 100 moveConfig)
  revert he
  decide

/-- Planted: instrumentation replaces every operand value with `O.unitVal`. -/
abbrev zeroOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with eval := fun (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) =>
      match (permit L e s.own) with
      | none => .stuck
      | some own => (O.eval L s.concrete e).map fun (_, concrete) =>
          (O.unitVal, ⟨concrete, own⟩) }

/-- Planted: instrumentation changes only the concrete-trap propagation rule to `.trap .abort`. -/
abbrev abortOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with eval := fun (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) =>
      match (permit L e s.own) with
      | none => .stuck
      | some own => match O.eval L s.concrete e with
        | .ok (v, concrete) => .ok (v, ⟨concrete, own⟩)
        | .trap _ => .trap .abort
        | .stuck => .stuck }

/-- Planted: instrumentation changes only the successful-operand rule to `.stuck`. -/
abbrev stuckOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with eval := fun (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) =>
      match (permit L e s.own) with
      | none => .stuck
      | some _ => match O.eval L s.concrete e with
        | .ok _ => .stuck
        | .trap k => .trap k
        | .stuck => .stuck }

/-- Planted: instrumentation inverts condition truth values. -/
abbrev invertTestOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with test := fun v => (O.test v).map Bool.not }

/-- Planted: instrumentation skips the ownership lifecycle transition while running the concrete one. -/
abbrev skipLifeOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with
    life := fun ev s => do
      let own := s.own
      let concrete ← O.life ev s.concrete
      pure ⟨concrete, own⟩ }

/-- Planted: instrumentation replaces only the ghost-state equality guard with false. -/
abbrev refuseAllowsOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with allows := fun p vs s a => O.allows p vs (erase s) (eraseAnswer a) && false }

/-- Planted: oracle lifting corrupts the ownership frame of the lifted answer (`nextLoan + 1`),
causing the instrumented machine to reject the answer with `.badAnswer` (which satisfies
`Ending.NotStuck`!) while the erased machine accepts it and emits `[7, 3]`. -/
def liftOracleMutateOwn (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (ρ : Exec.Oracle O) : Exec.Oracle (instrumentOperands O types permit) :=
  fun hist p vs site s =>
    (ρ hist p vs site s.concrete).map fun a =>
      liftAnswer { s.own with nextLoan := s.own.nextLoan + 1 } a

/-- Planted substitution closure: the body of Exec.step with only its operand
interpreter substituted, including evalArgs. All syntax, lifecycle sites, requests,
continuations and exits are unchanged. FiniteExecution uses this same substituted
step at every transition; this is test support, not a second normative driver. -/
private def stepWithEval (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (replacement : AList Var O.Val → Store O.Store → O.Expr → Res (O.Val × Store O.Store))
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    Option (Exec.Out (instrumentOperands O types permit)) :=
  let L := c.locals
  let st := c.store
  match c.ctrl, c.frames with
  -- running a statement
  | .exec .skip, k => some (.next ⟨.done, L, st, k⟩)
  | .exec (.bind x e site), k => onRes (replacement L st e) site L st k fun v st' =>
      ((instrumentOperands O types permit).life (.bind x) st').map fun st'' => .next ⟨.done, (x, v) :: L, st'', k⟩
  | .exec (.assign x e site), k => onRes (replacement L st e) site L st k fun v st' =>
      (AList.lookup L x).bind fun _ =>
        ((instrumentOperands O types permit).life (.bind x) st').map fun st'' =>
          .next ⟨.done, AList.update L x (fun _ => v), st'', k⟩
  | .exec (.seq s t), k => some (.next ⟨.exec s, L, st, .seq t :: k⟩)
  | .exec (.block b s), k => some (.next ⟨.exec s, L, st, .scope b (dom L) [] :: k⟩)
  | .exec (.defer f args site), k =>
      (register ⟨f, args, site⟩ k).map fun k' => .next ⟨.done, L, st, k'⟩
  | .exec (.cond e s t site), k => onRes (replacement L st e) site L st k fun v st' =>
      ((instrumentOperands O types permit).test v).map fun b => .next ⟨.exec (if b then s else t), L, st', k⟩
  | .exec (.loop l cr t b site), k =>
      some (.next ⟨.exec cr, L, st, .test l cr t b site (dom L) :: k⟩)
  | .exec (.brk l), k => some (.next ⟨.exiting (.brk l), L, st, k⟩)
  | .exec (.cont l), k => some (.next ⟨.exiting (.cont l), L, st, k⟩)
  | .exec (.ret e site), k => onRes (replacement L st e) site L st k fun v st' =>
      some (.next ⟨.exiting (.ret v), L, st', k⟩)
  | .exec (.call dst f args site), k => onRes (evalArgs { instrumentOperands O types permit with eval := replacement
      } L st args) site L st k fun vs st' =>
      (AList.lookup P.fns f).bind fun fn =>
        if fn.params.length = vs.length then
          ((instrumentOperands O types permit).life (.enter (fn.params.map Prod.fst)) st').map fun st'' =>
            .next ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, st'',
              .scope fn.block (fn.params.map Prod.fst) [] :: .functionEnd :: .ret dst L :: k⟩
        else none
  | .exec (.prim x p args site), k => onRes (evalArgs { instrumentOperands O types permit with eval := replacement }
      L st args) site L st k fun vs st' =>
      some (.request p vs site st' fun a => match a.result with
        | .ok v => (Exec.bindLife (instrumentOperands O types permit) x a.store).map fun st' => ⟨.done, bindDst x v
            L, st', k⟩
        | .error kd => some ⟨.trapped kd site, L, a.store, k⟩)
  | .exec (.abort site), k => some (.next ⟨.trapped .abort site, L, st, k⟩)
  -- a statement completed normally
  | .done, .seq t :: k => some (.next ⟨.exec t, L, st, k⟩)
  | .done, .scope b dm (d :: ds) :: k => some (startCleanup L st b dm d ds .fall k)
  | .done, .scope _ dm [] :: k => (Exec.dropOut (instrumentOperands O types permit) dm L st).map fun st' => .next
      ⟨.done, restrict dm L, st', k⟩
  | .done, .test l cr t b site dm :: k => onRes (replacement L st t) site L st k fun v st' =>
      ((instrumentOperands O types permit).test v).bind fun go =>
        (Exec.dropOut (instrumentOperands O types permit) dm L st').map fun st'' =>
          if go then .next ⟨.exec b, restrict dm L, st'', .body l cr t b site :: k⟩
          else .next ⟨.done, restrict dm L, st'', k⟩
  | .done, .body l cr t b site :: k => some (.next ⟨.exec (.loop l cr t b site), L, st, k⟩)
  | .done, .resume .fall :: k => some (.next ⟨.done, L, st, k⟩)
  | .done, .resume (.exit e) :: k => some (.next ⟨.exiting e, L, st, k⟩)
  -- a function body that ends without `return`: only a `Unit` function may (the checker refuses
  -- any other, C9), and its result is `()`
  | .done, .functionEnd :: k =>
      ((instrumentOperands O types permit).life .leave st).map fun st' => .next ⟨.done, L, st', k⟩
  | .done, .ret dst caller :: k =>
      (Exec.bindLife (instrumentOperands O types permit) dst st).map fun st'' => .next ⟨.done, bindDst dst
          (instrumentOperands O types permit).unitVal caller, st'', k⟩
  | .done, [] => none
  -- an exit unwinding
  | .exiting e, k => stepExit L st e k
  -- a trap is terminal
  | .trapped .., _ => none
  | .refused, _ => none

/-- Planted: uniform step closure of the success-value mutation. -/
def stepInstrumentedZeroVal (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    Option (Exec.Out (instrumentOperands O types permit)) :=
  stepWithEval O types permit (zeroOperands O types permit).eval P c

/-- Planted: uniform step closure of the successful-operand refusal mutation. -/
def stepInstrumentedRefuse (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    Option (Exec.Out (instrumentOperands O types permit)) :=
  stepWithEval O types permit (stuckOperands O types permit).eval P c

/-- Planted: oracle lifting drops the concrete oracle's answer and returns `none`. -/
def liftOracleDropAnswer (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (ρ : Exec.Oracle O) : Exec.Oracle (instrumentOperands O types permit) :=
  fun hist p vs site s => match ρ hist p vs site s.concrete with
    | none => none
    | some _ => none

def primFrameStuckConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.prim none () [.lit 0] ⟨0⟩), [(x, 7)], ⟨0, initial⟩,
    [.seq (.assign y (.lit 1) ⟨1⟩)]⟩

/-- Helper (inversion). `primFrameStuckConfig` satisfies `CheckerAccepted`. -/
private theorem primFrameStuck_checkerAccepted_inv :
    CheckerAccepted erasedOps types permitE emptyProg primFrameStuckConfig := by
  intro tr c hsteps
  cases hsteps with
  | refl =>
    exact ⟨by decide, ⟨⟨fun _ => rfl, fun _ _ _ => trivial⟩, fun _ _ _ _ _ _ => trivial⟩⟩
  | step hs₁ hsteps =>
    cases hs₁ with
    | silent h => cases h
    | @prim c₁ p vs site st k a hreq hallows hk =>
      have hreq' : Exec.step emptyProg primFrameStuckConfig =
          some (.request p vs site st k) := hreq
      dsimp [primFrameStuckConfig, Exec.step, Exec.evalArgs, instrumentOperands,
        instrumentEval, permitE, erasedEval, Res.bind, Exec.onRes] at hreq'
      injection hreq' with hreq'
      injection hreq' with hp hvs hsite hst hk_eq
      subst hp; subst hvs; subst hsite; subst hst; subst hk_eq
      have hown : a.store.own = initial :=
        (allows_erasure_result (O := erasedOps) (types := types) (permit := permitE)
          () [0] ⟨0, initial⟩ a hallows).2
      dsimp at hk
      cases ha : a.result with
      | error kd =>
        rw [ha] at hk
        injection hk with hc₁
        subst hc₁
        cases hsteps with
        | refl =>
          dsimp [StepPermitted]
          rw [hown]
          exact ⟨by decide, trivial⟩
        | step hs₂ _ => exact False.elim (step_det_none_inv rfl hs₂)
      | ok v =>
        rw [ha] at hk
        dsimp [Exec.bindLife] at hk
        injection hk with hc₁
        subst hc₁
        cases hsteps with
        | refl =>
          dsimp [StepPermitted, DoneFramePermitted]
          rw [hown]
          exact ⟨by decide, trivial⟩
        | step hs₂ hsteps =>
          obtain rfl := step_det_next_inv rfl hs₂
          cases hsteps with
          | refl =>
            dsimp [StepPermitted, StmtPermitted]
            rw [hown]
            exact ⟨by decide, ⟨fun _ => rfl, fun _ _ _ hlook => by cases hlook⟩⟩
          | step hs₃ _ => exact False.elim (step_det_none_inv rfl hs₃)

/-- Instance (helper computation). -/
example : (match (instrumentOperands erasedOps types permitE).eval [(x, 7)] ⟨0, initial⟩ (.move x) with
    | .ok (v, t) => erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.move x) = .ok (v, erase t)
    | _ => True) :=
  @eval_ok_erasure erasedOps types permitE [(x, 7)] ⟨0, initial⟩ (.move x)

/-- Instance (helper computation). -/
example : ¬ (match (zeroOperands erasedOps types permitE).eval [(x, 7)] ⟨0, initial⟩ (.move x) with
    | .ok (v, t) => erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.move x) = .ok (v, erase t)
    | _ => True) := by
  change ¬ ((Res.ok (7, 0) : Res (Int × Int)) = .ok (0, 0))
  decide

/-- Instance (helper computation). -/
example : (match (instrumentOperands erasedOps types permitE).eval [(x, 7)] ⟨0, initial⟩ (.fail .overflow) with
    | .trap kd => erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.fail .overflow) = .trap kd
    | _ => True) :=
  @eval_trap_erasure erasedOps types permitE [(x, 7)] ⟨0, initial⟩ (.fail .overflow)

/-- Instance (helper computation). -/
example : ¬ (match (abortOperands erasedOps types permitE).eval [(x, 7)] ⟨0, initial⟩ (.fail .overflow) with
    | .trap kd => erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.fail .overflow) = .trap kd
    | _ => True) := by
  change ¬ ((Res.trap .overflow : Res (Int × Int)) = .trap .abort)
  decide

/-- Instance (helper computation). -/
example : (match (instrumentOperands erasedOps types permitE).test 7 with
    | some b => erasedOps.test 7 = some b
    | none => erasedOps.test 7 = none) :=
  @test_erasure erasedOps types permitE 7

/-- Instance (helper computation). -/
example : ¬ (match (invertTestOperands erasedOps types permitE).test 7 with
    | some b => erasedOps.test 7 = some b
    | none => erasedOps.test 7 = none) := by
  change ¬ (some true = some false)
  decide

/-- Instance (helper computation). -/
example : (match (instrumentOperands erasedOps types permitE).life (.bind y) ⟨7, initial⟩ with
    | some t => erasedOps.life (.bind y) (erase (⟨7, initial⟩ : Store Int)) = some (erase t) ∧
        life types (.bind y) initial = some t.own
    | none => True) :=
  @instrumentLife_erasure erasedOps types permitE (.bind y) ⟨7, initial⟩

/-- Instance (helper computation). -/
example : ¬ (match (skipLifeOperands erasedOps types permitE).life (.bind y) ⟨7, initial⟩ with
    | some t => erasedOps.life (.bind y) (erase (⟨7, initial⟩ : Store Int)) = some (erase t) ∧
        life types (.bind y) initial = some t.own
    | none => True) := by
  change ¬ (some 7 = some 7 ∧ life types (.bind y) initial = some initial)
  decide

/-- Instance (helper computation). -/
example : ((instrumentOperands erasedOps types permitE).allows () [7] ⟨0, initial⟩
      ⟨some 7, .ok 7, ⟨7, initial⟩⟩ = true ↔
    erasedOps.allows () [7] (erase (⟨0, initial⟩ : Store Int))
      (eraseAnswer ⟨some 7, .ok 7, ⟨7, initial⟩⟩) = true ∧ initial = initial) :=
  @allows_erasure erasedOps types permitE () [7] ⟨0, initial⟩ ⟨some 7, .ok 7, ⟨7, initial⟩⟩

/-- Instance (helper computation). -/
example : ¬ ((refuseAllowsOperands erasedOps types permitE).allows () [7] ⟨0, initial⟩
      ⟨some 7, .ok 7, ⟨7, initial⟩⟩ = true ↔
    erasedOps.allows () [7] (erase (⟨0, initial⟩ : Store Int))
      (eraseAnswer ⟨some 7, .ok 7, ⟨7, initial⟩⟩) = true ∧ initial = initial) := by
  decide

/-- Instance (law `ConcreteSpec.Core.Own.step_next_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`). -/
example : (match stepInstrumented erasedOps types permitE emptyProg bindMoveConfig with
    | some (.next c') => Exec.step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) = some (.next (eraseConfig c'))
    | _ => True) :=
  @step_next_erasure erasedOps types permitE emptyProg bindMoveConfig

/-- Planted (law `ConcreteSpec.Core.Own.step_next_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedZeroVal`). -/
example : ¬ (match stepInstrumentedZeroVal erasedOps types permitE emptyProg bindMoveConfig with
    | some (.next c') => Exec.step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) = some (.next (eraseConfig c'))
    | _ => True) := by
  intro h
  cases h

/-- Instance (law `ConcreteSpec.Core.Own.step_request_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.primOneConfig`). -/
example : (match stepInstrumented erasedOps types permitE emptyProg primOneConfig with
    | some (.request p vs site st k) =>
      ∃ k_erased,
        Exec.step (eraseProgram emptyProg) (eraseConfig primOneConfig) =
          some (.request p vs site (erase st) k_erased) ∧
        ∀ a c', k a = some c' → k_erased (eraseAnswer a) = some (eraseConfig c')
    | _ => True) :=
  @step_request_erasure erasedOps types permitE emptyProg primOneConfig

/-- Planted (law `ConcreteSpec.Core.Own.step_request_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.primOneConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`, mutant
`ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedZeroVal`). -/
example : ¬ (match stepInstrumentedZeroVal erasedOps types permitE emptyProg primOneConfig with
    | some (.request p vs site st k) =>
      ∃ k_erased,
        Exec.step (eraseProgram emptyProg) (eraseConfig primOneConfig) =
          some (.request p vs site (erase st) k_erased) ∧
        ∀ a c', k a = some c' → k_erased (eraseAnswer a) = some (eraseConfig c')
    | _ => True) := by
  rintro ⟨_, h, _⟩
  cases h

/-- Instance (law `ConcreteSpec.Core.Own.Step_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`). -/
example : (match stepInstrumented erasedOps types permitE emptyProg bindMoveConfig with
    | some (.next c') =>
      Exec.Step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) .silent (eraseConfig c')
    | some (.request p vs _site st k) =>
      ∀ a c', (instrumentOperands erasedOps types permitE).allows p vs st a = true →
        k a = some c' →
        Exec.Step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) (eraseLabel a.label) (eraseConfig c')
    | none => True) :=
  @Step_erasure erasedOps types permitE emptyProg bindMoveConfig

/-- Planted (law `ConcreteSpec.Core.Own.Step_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedZeroVal`). -/
example : ¬ (match stepInstrumentedZeroVal erasedOps types permitE emptyProg bindMoveConfig with
    | some (.next c') =>
      Exec.Step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) .silent (eraseConfig c')
    | some (.request p vs _site st k) =>
      ∀ a c', (instrumentOperands erasedOps types permitE).allows p vs st a = true →
        k a = some c' →
        Exec.Step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) (eraseLabel a.label) (eraseConfig c')
    | none => True) := by
  intro h
  have heq := step_det_next_inv rfl h
  cases heq

/-- Endpoint of the complete two-primitive execution, including both concrete store updates. -/
def traceSuccessor : Exec.Config instrumentedOps :=
  ⟨.exiting (.ret 7), [(y, 7), (x, 7)], ⟨10, ((life types .leave ((move ((bind types y ((move initial ⟨0⟩).getD
      initial)).getD initial) ⟨1⟩).getD initial)).getD initial)⟩, []⟩

/-- Helper (computation). The seven-step execution emits both primitive events in order. -/
private theorem traceSteps : Exec.Steps emptyProg primTraceConfig [7, 3] traceSuccessor := by
  have hsteps : Exec.Steps emptyProg primTraceConfig _ _ :=
    .step (.silent rfl)
      (.step (.prim (a := liftAnswer ((move initial ⟨0⟩).getD initial) ⟨some 7, .ok 7, 7⟩) rfl rfl rfl)
        (.step (.silent rfl)
          (.step (.silent rfl)
            (.step (.prim (a := liftAnswer ((bind types y ((move initial ⟨0⟩).getD initial)).getD initial)
                  ⟨some 3, .ok 10, 10⟩) rfl rfl rfl)
              (.step (.silent rfl)
                (.step (.silent rfl) (.step (.silent rfl) (.refl _))))))))
  exact hsteps

/-- Instance (law `ConcreteSpec.Core.Own.Steps_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`). -/
example : FiniteSimulation erasedOps types permitE
    (stepInstrumented erasedOps types permitE emptyProg) emptyProg bindMoveConfig :=
  @Steps_erasure erasedOps types permitE emptyProg bindMoveConfig

/-- Endpoint of the mutant's one-step execution: it has corrupted the newly bound value. -/
def zeroSuccessor : Exec.Config instrumentedOps :=
  ⟨.done, [(y, 0), (x, 7)], ⟨0, (bind types y ((move initial ⟨0⟩).getD initial)).getD initial⟩, []⟩

/-- Planted (law `ConcreteSpec.Core.Own.Steps_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedZeroVal`). -/
example : ¬ FiniteSimulation erasedOps types permitE
    (stepInstrumentedZeroVal erasedOps types permitE emptyProg) emptyProg bindMoveConfig := by
  intro h
  have bad : FiniteExecution (stepInstrumentedZeroVal erasedOps types permitE emptyProg)
      bindMoveConfig [] zeroSuccessor := .silent rfl (.refl _)
  have impossible : ∀ tr, ¬ Exec.Steps (eraseProgram emptyProg)
      (eraseConfig bindMoveConfig) tr (eraseConfig zeroSuccessor) := by
    intro tr he
    cases he with
    | step hs rest =>
      have hd := step_det_next_inv (O := erasedOps) (P := eraseProgram emptyProg) (c₁ := ⟨.done, [(y, 7), (x, 7)],
          0, []⟩) rfl hs
      subst hd
      cases rest with
      | step hs _ => exact step_det_none_inv rfl hs
  exact impossible [] (h [] zeroSuccessor bad)

/-- Instance (law `ConcreteSpec.Core.Own.runFrom_erasure`, case `ConcreteSpec.Core.Own.Examples.primTraceConfig`).
-/
example : Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primTraceConfig) =
    eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] primTraceConfig) :=
  @runFrom_erasure erasedOps types permitE emptyProg erasedOracle 100 [] primTraceConfig rfl

/-- Planted (law `ConcreteSpec.Core.Own.runFrom_erasure`, case `ConcreteSpec.Core.Own.Examples.primTraceConfig`,
subject `ConcreteSpec.Core.Own.liftOracle`, mutant
`ConcreteSpec.Core.Own.ExecErasureWitnesses.liftOracleMutateOwn`). -/
example : Ending.NotStuck (Exec.runFrom emptyProg
      (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 [] primTraceConfig).ending ∧
    ¬ (Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primTraceConfig) =
      eraseReport (Exec.runFrom emptyProg (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 []
          primTraceConfig)) := by
  exact ⟨rfl, by intro h; have he := congrArg (fun r => r.events) h; cases he⟩

/-- Instance (law `ConcreteSpec.Core.Own.run_erasure`, case `ConcreteSpec.Core.Own.Examples.primTraceConfig`). -/
example : Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primTraceConfig) =
    eraseReport (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 primTraceConfig) :=
  @run_erasure erasedOps types permitE emptyProg erasedOracle 100 primTraceConfig rfl

/-- Planted (law `ConcreteSpec.Core.Own.run_erasure`, case `ConcreteSpec.Core.Own.Examples.primTraceConfig`, subject
`ConcreteSpec.Core.Own.liftOracle`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.liftOracleMutateOwn`). -/
example : Ending.NotStuck (Exec.run emptyProg
      (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 primTraceConfig).ending ∧
    ¬ (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primTraceConfig) =
      eraseReport (Exec.run emptyProg (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100
          primTraceConfig)) := by
  exact ⟨rfl, by intro h; have he := congrArg (fun r => r.events) h; cases he⟩

/-- Helper (inversion). The single-step move-binding configuration `bindMoveConfig`
satisfies `CheckerAccepted`. -/
private theorem bindMove_checkerAccepted_inv :
    CheckerAccepted erasedOps types permitE emptyProg bindMoveConfig := by
  intro tr c hsteps
  cases hsteps with
  | refl =>
    refine ⟨by decide, ⟨fun _ => by decide, fun v s' he => ?_⟩⟩
    intro _
    have he' : instrumentEval erasedOps permitE [(x, 7)] ⟨0, initial⟩ (.move x) = .ok (v, s') := he
    injection he' with hpair
    rcases Prod.mk.inj hpair with ⟨_, rfl⟩
    decide
  | step hs₁ hsteps =>
    obtain rfl := step_det_next_inv rfl hs₁
    cases hsteps with
    | refl => exact ⟨by decide, trivial⟩
    | step hs₂ _ => exact False.elim (step_det_none_inv rfl hs₂)

/-- Instance (law `ConcreteSpec.Core.Own.step_stuck_iff_of_accepted`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`). -/
example : stepInstrumented erasedOps types permitE emptyProg bindMoveConfig = none ↔
    Exec.step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) = none :=
  @step_stuck_iff_of_accepted erasedOps types permitE emptyProg bindMoveConfig
    bindMove_checkerAccepted_inv

/-- Planted (law `ConcreteSpec.Core.Own.step_stuck_iff_of_accepted`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedRefuse`). -/
example : CheckerAccepted erasedOps types permitE emptyProg bindMoveConfig ∧
    ¬ (stepInstrumentedRefuse erasedOps types permitE emptyProg bindMoveConfig = none ↔
      Exec.step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) = none) :=
  ⟨bindMove_checkerAccepted_inv, fun h => by cases h.mp rfl⟩

/-- Instance (law `ConcreteSpec.Core.Own.runFrom_erasure_of_accepted`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.primFrameStuckConfig`). -/
example : Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primFrameStuckConfig) =
    eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 []
        primFrameStuckConfig) :=
  @runFrom_erasure_of_accepted erasedOps types permitE emptyProg erasedOracle 100 []
    primFrameStuckConfig primFrameStuck_checkerAccepted_inv

/-- Planted (law `ConcreteSpec.Core.Own.runFrom_erasure_of_accepted`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.primFrameStuckConfig`, subject `ConcreteSpec.Core.Own.liftOracle`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.liftOracleMutateOwn`). -/
example : CheckerAccepted erasedOps types permitE emptyProg primFrameStuckConfig ∧
    ¬ (Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primFrameStuckConfig) =
      eraseReport (Exec.runFrom emptyProg (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 []
          primFrameStuckConfig)) :=
  ⟨primFrameStuck_checkerAccepted_inv,
    by intro h; have he := congrArg (fun r => r.events) h; cases he⟩

/-- Instance (law `ConcreteSpec.Core.Own.run_stuck_iff_of_accepted`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.primFrameStuckConfig`). -/
example : Ending.IsStuck (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) 100
    primFrameStuckConfig).ending =
    Ending.IsStuck (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primFrameStuckConfig)).ending :=
  @run_stuck_iff_of_accepted erasedOps types permitE emptyProg erasedOracle 100
    primFrameStuckConfig primFrameStuck_checkerAccepted_inv

/-- Planted (law `ConcreteSpec.Core.Own.run_stuck_iff_of_accepted`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.primFrameStuckConfig`, subject `ConcreteSpec.Core.Own.liftOracle`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.liftOracleDropAnswer`). -/
example : CheckerAccepted erasedOps types permitE emptyProg primFrameStuckConfig ∧
    ¬ (Ending.IsStuck (Exec.run emptyProg (liftOracleDropAnswer erasedOps types permitE erasedOracle) 100
        primFrameStuckConfig).ending =
      Ending.IsStuck (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primFrameStuckConfig)).ending) :=
  ⟨primFrameStuck_checkerAccepted_inv, by decide⟩

end ExecErasureWitnesses

namespace ArgumentEvidence
open Examples

open ExecErasureWitnesses

/-- Instance (law `ConcreteSpec.Core.Own.evalArgs_ok_erasure`, case `ConcreteSpec.Core.Own.Examples.initial`). -/
example : (match Exec.evalArgs (instrumentOperands erasedOps types permitE) [(x, 7)] ⟨0, initial⟩ [.read x, .move x]
    with
    | .ok (vs, t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.read x, .move x] = .ok
        (vs, erase t)
    | _ => True) :=
  @evalArgs_ok_erasure erasedOps types permitE [(x, 7)] [.read x, .move x] ⟨0, initial⟩

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_ok_erasure`, case `ConcreteSpec.Core.Own.Examples.initial`, subject
`ConcreteSpec.Core.Own.instrumentOperands`, mutant `ConcreteSpec.Core.Own.ArgumentEvidence.zeroOperands`). -/
example : ¬ (match Exec.evalArgs (zeroOperands erasedOps types permitE) [(x, 7)] ⟨0, initial⟩ [.read x, .move x] with
    | .ok (vs, t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.read x, .move x] = .ok
        (vs, erase t)
    | _ => True) := by
  change ¬ ((Res.ok ([7, 7], 0) : Res (List Int × Int)) = .ok ([0, 0], 0))
  decide

/-- Instance (law `ConcreteSpec.Core.Own.evalArgs_trap_erasure`, case `ConcreteSpec.Core.Own.Examples.initial`). -/
example : (match Exec.evalArgs (instrumentOperands erasedOps types permitE) [(x, 7)] ⟨0, initial⟩ [.move x, .fail
    .overflow] with
    | .trap kd => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.move x, .fail .overflow] =
        .trap kd
    | _ => True) :=
  @evalArgs_trap_erasure erasedOps types permitE [(x, 7)] [.move x, .fail .overflow] ⟨0, initial⟩

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_trap_erasure`, case `ConcreteSpec.Core.Own.Examples.initial`,
subject `ConcreteSpec.Core.Own.instrumentOperands`, mutant `ConcreteSpec.Core.Own.ArgumentEvidence.abortOperands`).
-/
example : ¬ (match Exec.evalArgs (abortOperands erasedOps types permitE) [(x, 7)] ⟨0, initial⟩ [.move x, .fail
    .overflow] with
    | .trap kd => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.move x, .fail .overflow] =
        .trap kd
    | _ => True) := by
  change ¬ ((Res.trap .overflow : Res (List Int × Int)) = .trap .abort)
  decide

end ArgumentEvidence

namespace PopulatedErasureEvidence
open Examples ExecErasureWitnesses

/-- A real shared loan remains live while an owner read binds a fresh concrete local. -/
def readConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.bind y (.read x) ⟨0⟩), [(x, 7)], ⟨4, shared⟩, []⟩

/-- Expected successful destination, retaining the populated loan ledger. -/
def readDestination : Exec.Config instrumentedOps :=
  ⟨.done, [(y, 7), (x, 7)], ⟨4, (bind types y shared).getD shared⟩, []⟩

/-- A primitive request evaluates an owner read while its shared loan stays live. -/
def requestConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.prim (some y) () [.read x] ⟨0⟩), [(x, 7)], ⟨4, shared⟩, []⟩

/-- Helper: acceptance is inhabited at both reachable configurations, with no refused lifecycle site. -/
private theorem readAccepted : CheckerAccepted erasedOps types permitE emptyProg readConfig := by
  intro tr c hs
  cases hs with
  | refl =>
    refine ⟨by decide, ⟨fun _ => by decide, fun v s he => ?_⟩⟩
    have he' : instrumentEval erasedOps permitE [(x, 7)] ⟨4, shared⟩ (.read x) = .ok (v, s) := he
    injection he' with hp
    rcases Prod.mk.inj hp with ⟨_, rfl⟩
    intro _
    decide
  | step h rest =>
    obtain rfl := step_det_next_inv rfl h
    cases rest with
    | refl => exact ⟨by decide, trivial⟩
    | step h _ => exact False.elim (step_det_none_inv rfl h)

/-- Instance: successful operand/argument erasure exercises a live loan, then explicitly
ends it before moving its root. The values and the emptied loan ledger are observed. -/
example :
    Exec.evalArgs instrumentedOps [(x, 7)] ⟨4, shared⟩
      [.read x, .endLoan ⟨0⟩, .move x] =
      .ok ([7, 0, 7], ⟨4, { ((move initial ⟨0⟩).getD initial) with nextLoan := 1 }⟩) ∧
    (match Exec.evalArgs instrumentedOps [(x, 7)] ⟨4, shared⟩
        [.read x, .endLoan ⟨0⟩, .move x] with
      | .ok (vs, s) =>
        Exec.evalArgs erasedOps [(x, 7)] 4 [.read x, .endLoan ⟨0⟩, .move x] = .ok (vs, erase s)
      | _ => True) := by
  constructor
  · decide
  · exact @evalArgs_ok_erasure erasedOps types permitE [(x, 7)]
      [.read x, .endLoan ⟨0⟩, .move x] ⟨4, shared⟩

/-- Instance: the loan-preserving first argument succeeds before the real overflow trap. -/
example :
    Exec.evalArgs instrumentedOps [(x, 7)] ⟨4, shared⟩ [.read x, .fail .overflow] = .trap .overflow ∧
    (match Exec.evalArgs instrumentedOps [(x, 7)] ⟨4, shared⟩ [.read x, .fail .overflow] with
      | .trap k => Exec.evalArgs erasedOps [(x, 7)] 4 [.read x, .fail .overflow] = .trap k
      | _ => True) :=
  ⟨rfl, @evalArgs_trap_erasure erasedOps types permitE [(x, 7)]
    [.read x, .fail .overflow] ⟨4, shared⟩⟩

/-- Instance: silent erasure succeeds on populated locals and loans, and retains the loan. -/
example :
    stepInstrumented erasedOps types permitE emptyProg readConfig = some (.next readDestination) ∧
    Exec.step (eraseProgram emptyProg) (eraseConfig readConfig) =
      some (.next (eraseConfig readDestination)) ∧
    readDestination.store.own.loans = shared.loans :=
  ⟨rfl, step_next_erasure emptyProg readConfig, rfl⟩

/-- Instance: primitive erasure exposes the same argument, concrete state and accepting
continuation while preserving a populated loan ledger. -/
example :
    match stepInstrumented erasedOps types permitE emptyProg requestConfig with
    | some (.request p vs site st k) =>
      vs = [7] ∧ st.concrete = 4 ∧ st.own.loans = shared.loans ∧
      ∃ ke,
        Exec.step (eraseProgram emptyProg) (eraseConfig requestConfig) =
          some (.request p vs site (erase st) ke) ∧
        ∀ a c, k a = some c → ke (eraseAnswer a) = some (eraseConfig c)
    | _ => False :=
  ⟨rfl, rfl, rfl, step_request_erasure emptyProg requestConfig⟩

/-- Instance: relational single-step and finite erasure both exercise the accepting read. -/
example :
    Exec.Step (eraseProgram emptyProg) (eraseConfig readConfig) .silent
      (eraseConfig readDestination) ∧
    FiniteExecution (stepInstrumented erasedOps types permitE emptyProg)
      readConfig [] readDestination ∧
    Exec.Steps (eraseProgram emptyProg) (eraseConfig readConfig) []
      (eraseConfig readDestination) :=
  ⟨Step_erasure emptyProg readConfig,
    .silent rfl (.refl _),
    Steps_erasure emptyProg readConfig [] readDestination (.silent rfl (.refl _))⟩

/-- Instance: forward fuel and whole-run erasure are non-vacuous; the actual run
finishes normally and retains both its new concrete local and its original loan. -/
example :
    Ending.NotStuck (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle)
      3 readConfig).ending ∧
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 3 [9] (eraseConfig readConfig) =
      eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle)
        3 [9] readConfig) ∧
    Exec.run (eraseProgram emptyProg) erasedOracle 3 (eraseConfig readConfig) =
      eraseReport (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) 3 readConfig) :=
  ⟨rfl, runFrom_erasure emptyProg erasedOracle 3 [9] readConfig rfl,
    run_erasure emptyProg erasedOracle 3 readConfig rfl⟩

/-- Instance: accepted converse runs in both directions on a successful, loan-populated
execution. Single-step stuck equivalence is exercised in both directions too. -/
example :
    CheckerAccepted erasedOps types permitE emptyProg readConfig ∧
    (stepInstrumented erasedOps types permitE emptyProg readConfig = none ↔
      Exec.step (eraseProgram emptyProg) (eraseConfig readConfig) = none) ∧
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 3 [] (eraseConfig readConfig) =
      eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle)
        3 [] readConfig) ∧
    Ending.IsStuck (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle)
      3 readConfig).ending =
      Ending.IsStuck (Exec.run (eraseProgram emptyProg) erasedOracle 3 (eraseConfig readConfig)).ending :=
  ⟨readAccepted, step_stuck_iff_of_accepted emptyProg readConfig readAccepted,
    runFrom_erasure_of_accepted emptyProg erasedOracle 3 [] readConfig readAccepted,
    run_stuck_iff_of_accepted emptyProg erasedOracle 3 readConfig readAccepted⟩

/-- Planted: change only the concrete-stuck branch into an invented successful result.
The permission guard, real successes and real traps remain unchanged. -/
def recoverStuck (O : Exec.Operands)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) : Res (O.Val × Store O.Store) :=
  match permit L e s.own with
  | none => .stuck
  | some own => match O.eval L s.concrete e with
    | .ok (v, concrete) => .ok (v, ⟨concrete, own⟩)
    | .trap k => .trap k
    | .stuck => .ok (O.unitVal, ⟨s.concrete, own⟩)

/-- A concrete missing-local failure with real ownership storage and a live shared loan.
CheckerAccepted is the abstract permission obligation, not a proof of concrete-local availability. -/
def missingConcrete : Exec.Config instrumentedOps :=
  ⟨.exec (.ret (.read x) ⟨0⟩), [], ⟨4, shared⟩, []⟩

/-- Helper: no real transition leaves this configuration; its permission premise is inhabited. -/
private theorem missingAccepted : CheckerAccepted erasedOps types permitE emptyProg missingConcrete := by
  intro tr c hs
  cases hs with
  | refl => exact ⟨by decide, fun h => False.elim (h rfl)⟩
  | step h _ => exact False.elim (step_det_none_inv rfl h)

/-- Planted: an added transition refutes the opposite direction of accepted stuck
 equivalence. The real checker premise holds; the mutant changes only concrete-stuck propagation. -/
example :
    CheckerAccepted erasedOps types permitE emptyProg missingConcrete ∧
    ¬ (stepWithEval erasedOps types permitE (recoverStuck erasedOps permitE)
        emptyProg missingConcrete = none ↔
      Exec.step (eraseProgram emptyProg) (eraseConfig missingConcrete) = none) :=
  ⟨missingAccepted, fun h => by cases h.mpr rfl⟩

/-- Planted: change only successful concrete evaluation into abort, keeping refusal,
concrete traps and concrete stuckness intact. This isolates control erasure from value erasure. -/
def abortSuccess (O : Exec.Operands)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) : Res (O.Val × Store O.Store) :=
  match permit L e s.own with
  | none => .stuck
  | some _ => match O.eval L s.concrete e with
    | .ok _ => .trap .abort
    | .trap k => .trap k
    | .stuck => .stuck

/-- Planted: successful concrete argument evaluation must never erase to abort.
The actual loan-protected read succeeds, so no permission or concrete failure masks the mutation. -/
example :
    instrumentEval erasedOps permitE [(x, 7)] ⟨4, shared⟩ (.read x) = .ok (7, ⟨4, shared⟩) ∧
    ¬ (match Exec.evalArgs
        { instrumentedOps with eval := abortSuccess erasedOps permitE }
        [(x, 7)] ⟨4, shared⟩ [.read x] with
      | .trap k => Exec.evalArgs erasedOps [(x, 7)] 4 [.read x] = .trap k
      | _ => True) := by
  constructor
  · rfl
  · change ¬ ((Res.ok ([7], 4) : Res (List Int × Int)) = .trap .abort)
    decide

/-- Planted: the same control mutation is substituted into the full step body.
Its next configuration is trapped while the real erased step binds a local normally. -/
example :
    stepInstrumented erasedOps types permitE emptyProg readConfig = some (.next readDestination) ∧
    ¬ (match stepWithEval erasedOps types permitE (abortSuccess erasedOps permitE)
        emptyProg readConfig with
      | some (.next c) => Exec.step (eraseProgram emptyProg) (eraseConfig readConfig) =
          some (.next (eraseConfig c))
      | _ => True) := by
  constructor
  · rfl
  · intro h; cases h

end PopulatedErasureEvidence

namespace AddedAnswerEvidence
open Examples ExecErasureWitnesses

/-- Planted: change only the no-answer branch of oracle lifting to an invented answer.
Existing answers still lift unchanged. The invented allowed answer adds a primitive
transition, so an absence/equivalence law must reject this direction too. -/
def inventAnswer (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (ρ : Exec.Oracle O) (fallback : Exec.Answer O.Val O.Event O.Store) :
    Exec.Oracle (instrumentOperands O types permit) :=
  fun hist p vs site s => match ρ hist p vs site s.concrete with
    | none => some (liftAnswer s.own fallback)
    | some a => some (liftAnswer s.own a)

/-- Instance: the checker-accepted request waits when the actual oracle gives no answer. -/
example :
    CheckerAccepted erasedOps types permitE emptyProg primFrameStuckConfig ∧
    Ending.IsStuck (Exec.run emptyProg
      (liftOracle erasedOps types permitE (fun _ _ _ _ _ => none)) 10 primFrameStuckConfig).ending =
      Ending.IsStuck (Exec.run (eraseProgram emptyProg)
        (fun _ _ _ _ _ => none) 10 (eraseConfig primFrameStuckConfig)).ending :=
  ⟨primFrameStuck_checkerAccepted_inv,
    run_stuck_iff_of_accepted emptyProg (fun _ _ _ _ _ => none) 10
      primFrameStuckConfig primFrameStuck_checkerAccepted_inv⟩

/-- Planted: the fabricated allowed answer adds a real transition and event before the
unbound assignment gets stuck. The real run waits, so accepted stuck equivalence fails.
The exact same abstract checker-acceptance premise is inhabited. -/
example :
    CheckerAccepted erasedOps types permitE emptyProg primFrameStuckConfig ∧
    ¬ (Exec.runFrom (eraseProgram emptyProg) (fun _ _ _ _ _ => none)
        10 [] (eraseConfig primFrameStuckConfig) =
      eraseReport (Exec.runFrom emptyProg
        (inventAnswer erasedOps types permitE (fun _ _ _ _ _ => none)
          ⟨some 0, .ok 0, 0⟩) 10 [] primFrameStuckConfig)) ∧
    ¬ (Ending.IsStuck (Exec.run emptyProg
        (inventAnswer erasedOps types permitE (fun _ _ _ _ _ => none)
          ⟨some 0, .ok 0, 0⟩) 10 primFrameStuckConfig).ending =
      Ending.IsStuck (Exec.run (eraseProgram emptyProg)
        (fun _ _ _ _ _ => none) 10 (eraseConfig primFrameStuckConfig)).ending) :=
  by
    refine ⟨primFrameStuck_checkerAccepted_inv, ?_, ?_⟩
    · intro h
      have he := congrArg (fun r => r.events) h
      cases he
    · decide

end AddedAnswerEvidence

end ConcreteSpec.Core.Own
