import ConcreteSpec.Proofs.Exec
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Own

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
Held policies remain ISS-0064/ISS-0065, ISS-0068 through ISS-0073, ISS-0076 and ISS-0080:
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

/-- Law (subject `ConcreteSpec.Core.Own.instrumentOperands`; authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal;
#decided-2026-11-05-core, Q9.10). Permission refusal is unsafe stopping,
including when the concrete expression would succeed or trap. -/
theorem eval_refusal (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr)
    (h : permit L e s.own = none) :
    (instrumentOperands O types permit).eval L s e = .stuck := by
  simp [instrumentOperands, instrumentEval, h]

/-- Law (subject `ConcreteSpec.Core.Own.instrumentOperands`; authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal).
A failed ownership lifecycle cannot be converted to a concrete success. -/
theorem life_refusal (ev : Exec.Life) (s : Store O.Store)
    (h : life types ev s.own = none) :
    (instrumentOperands O types permit).life ev s = none := by
  simp [instrumentOperands, instrumentLife, h]

/-- Law (subject `ConcreteSpec.Core.Own.instrumentOperands`; authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal).
A refused head argument stops argument evaluation, rather than returning or trapping. -/
theorem evalArgs_refusal (L : AList Var O.Val) (s : Store O.Store)
    (e : O.Expr) (es : List O.Expr) (h : permit L e s.own = none) :
    Exec.evalArgs (instrumentOperands O types permit) L s (e :: es) = .stuck := by
  simp [Exec.evalArgs, instrumentOperands, instrumentEval, h, Res.bind]

/-- Law (subject `ConcreteSpec.Core.Own.instrumentOperands`; authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal).
Refusal during a binding expression leaves the dispatcher without a transition. -/
theorem step_eval_refusal (P : Exec.Program (instrumentOperands O types permit))
    (L : AList Var O.Val) (s : Store O.Store)
    (k : List (Exec.Frame (instrumentOperands O types permit)))
    (x : Var) (e : O.Expr) (site : SiteId) (h : permit L e s.own = none) :
    Exec.step P ⟨.exec (.bind x e site), L, s, k⟩ = none := by
  simp [Exec.step, instrumentOperands, instrumentEval, h, Exec.onRes]

/-- Law (subject `ConcreteSpec.Core.Own.instrumentOperands`; authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal).
A failed result-binding lifecycle is unsafe stopping after a successful operand. -/
theorem step_life_refusal (P : Exec.Program (instrumentOperands O types permit))
    (L : AList Var O.Val) (s t : Store O.Store)
    (k : List (Exec.Frame (instrumentOperands O types permit)))
    (x : Var) (e : O.Expr) (site : SiteId) (v : O.Val)
    (he : (instrumentOperands O types permit).eval L s e = .ok (v, t))
    (hl : life types (.bind x) t.own = none) :
    Exec.step P ⟨.exec (.bind x e site), L, s, k⟩ = none := by
  change instrumentEval O permit L s e = .ok (v, t) at he
  simp [Exec.step, instrumentOperands, he, Exec.onRes, instrumentLife, hl]

/-- Law (subject `ConcreteSpec.Core.Own.instrumentOperands`; authority: docs/decisions.md#decided-2026-11-08-recovered-d3, C5;
#decided-2026-11-05-core, Q9.10). Permitted arguments have exactly the concrete
result after erasure, including traps and stuckness. This converse does not assume
that the instrumented result succeeded. -/
theorem evalArgs_erasure_of_permitted (L : AList Var O.Val)
    (args : List O.Expr) (s : Store O.Store) (hp : ArgsPermitted O types permit L s args) :
    Exec.evalArgs O L (erase s) args =
      (Exec.evalArgs (instrumentOperands O types permit) L s args).map
        (fun (vs, t) => (vs, erase t)) := by
  induction args generalizing s with
  | nil => rfl
  | cons e es ih =>
    obtain ⟨hpe, hpes⟩ := hp
    cases hc : O.eval L s.concrete e with
    | stuck =>
      cases hg : permit L e s.own <;>
        simp [Exec.evalArgs, instrumentOperands, instrumentEval, hg, hc, erase, Res.bind, Res.map]
    | trap kd =>
      obtain ⟨own, hg⟩ := Option.isSome_iff_exists.mp (hpe (by rw [hc]; intro h; cases h))
      simp [Exec.evalArgs, instrumentOperands, instrumentEval, hg, hc, erase, Res.bind, Res.map]
    | ok r =>
      obtain ⟨v, concrete⟩ := r
      obtain ⟨own, hg⟩ := Option.isSome_iff_exists.mp (hpe (by rw [hc]; intro h; cases h))
      have he : (instrumentOperands O types permit).eval L s e = .ok (v, ⟨concrete, own⟩) := by
        simp [instrumentOperands, instrumentEval, hg, hc, Res.map, Res.bind]
      have hi := ih ⟨concrete, own⟩ (hpes v ⟨concrete, own⟩ he)
      change instrumentEval O permit L s e = .ok (v, ⟨concrete, own⟩) at he
      simp only [Exec.evalArgs, instrumentOperands, he, Res.bind, erase, hc]
      change Exec.evalArgs O L concrete es = _ at hi
      rw [hi]
      cases Exec.evalArgs (instrumentOperands O types permit) L ⟨concrete, own⟩ es <;> rfl

/-- Law (subject `ConcreteSpec.Core.Own.instrumentOperands`; authority: docs/decisions.md#decided-2026-11-08-recovered-d3, C5).
Permitted arguments are stuck on exactly the same inputs in either execution. -/
theorem evalArgs_stuck_iff_of_permitted (L : AList Var O.Val)
    (args : List O.Expr) (s : Store O.Store) (hp : ArgsPermitted O types permit L s args) :
    Exec.evalArgs (instrumentOperands O types permit) L s args = .stuck ↔
      Exec.evalArgs O L (erase s) args = .stuck := by
  rw [evalArgs_erasure_of_permitted L args s hp]
  cases Exec.evalArgs (instrumentOperands O types permit) L s args <;> simp [Res.map, Res.bind]

/-- Law (subject `ConcreteSpec.Core.Own.instrumentOperands`; authority: docs/decisions.md#decided-2026-11-08-recovered-d3, C5).
A concrete trap is also a tracked trap when argument permission holds. -/
theorem evalArgs_trap_iff_of_permitted (L : AList Var O.Val)
    (args : List O.Expr) (s : Store O.Store) (hp : ArgsPermitted O types permit L s args)
    (kd : TrapKind) :
    Exec.evalArgs (instrumentOperands O types permit) L s args = .trap kd ↔
      Exec.evalArgs O L (erase s) args = .trap kd := by
  rw [evalArgs_erasure_of_permitted L args s hp]
  cases Exec.evalArgs (instrumentOperands O types permit) L s args <;> simp [Res.map, Res.bind]

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
        Exec.evalArgs O L (erase s) args = .stuck :=
      fun args s hp hs => (evalArgs_stuck_iff_of_permitted (O := O) (types := types) (permit := permit) L args s hp).mp hs
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

/-- Helper: accepted request continuations preserve failure as well as success.
This is shared by relational converse and oracle report erasure. -/
private theorem request_none_of_permitted
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit))
    (hperm : StepPermitted O types permit P c)
    (p : O.Prim) (vs : List O.Val) (site : SiteId) (st : Store O.Store)
    (k : Exec.Answer O.Val O.Event (Store O.Store) → Option (Exec.Config (instrumentOperands O types permit)))
    (a : Exec.Answer O.Val O.Event (Store O.Store))
    (hs : Exec.step P c = some (.request p vs site st k))
    (ha' : (instrumentOperands O types permit).allows p vs st a = true)
    (hk : k a = none) :
    ∃ ke, Exec.step (eraseProgram P) (eraseConfig c) =
      some (.request p vs site (erase st) ke) ∧ ke (eraseAnswer a) = none := by
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

/-- Law (subject `ConcreteSpec.Core.Own.stepInstrumented`; authority: docs/decisions.md#decided-2026-11-08-recovered-d3, C5;
#decided-2026-11-05-core, Q9.10). Every concrete relational transition lifts
from an accepted configuration. The answer is chosen independently at each step;
no deterministic oracle restricts the environment. -/
theorem Step_converse_of_accepted
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit))
    (hacc : CheckerAccepted O types permit P c)
    (l : Exec.Label O) (d : Exec.Config O)
    (hd : Exec.Step (eraseProgram P) (eraseConfig c) l d) :
    ∃ li ci, Exec.Step P c li ci ∧ eraseLabel li = l ∧ eraseConfig ci = d := by
  cases hs : Exec.step P c with
  | none =>
    exact False.elim (Exec.no_step_of_none_inv
      ((step_stuck_iff_of_accepted P c hacc).mp hs) l d hd)
  | some out =>
    cases out with
    | next ci =>
      have he := step_next_erasure P c
      rw [show stepInstrumented O types permit P c = some (.next ci) from hs] at he
      obtain ⟨rfl, rfl⟩ := (Exec.step_silent_representation _ _ _ _ _ he).mp hd
      exact ⟨.silent, ci, .silent hs, rfl, rfl⟩
    | request p vs site st k =>
      have he := step_request_erasure P c
      rw [show stepInstrumented O types permit P c = some (.request p vs site st k) from hs] at he
      obtain ⟨ke, he, hke⟩ := he
      cases hd with
      | silent h => rw [he] at h; cases h
      | prim h ha hk =>
        rw [he] at h
        cases h
        rename_i a
        have ha' : (instrumentOperands O types permit).allows p vs st
            (liftAnswer st.own a) = true := (allows_liftAnswer_proj (O := O) (types := types) (permit := permit) p vs st a).trans ha
        have hea : eraseAnswer (liftAnswer st.own a) = a := by cases a; rfl
        cases hki : k (liftAnswer st.own a) with
        | none =>
          obtain ⟨ke', he', hn⟩ := request_none_of_permitted P c
            (hacc [] c (.refl c)).2 p vs site st k (liftAnswer st.own a) hs ha' hki
          rw [he] at he'
          cases he'
          rw [hea, hk] at hn
          cases hn
        | some ci =>
          have hc := hke _ _ hki
          rw [hea, hk] at hc
          injection hc with hc
          exact ⟨(liftAnswer st.own a).label, ci, .prim hs ha' hki,
            by rw [eraseLabel_answer_proj, hea], hc.symm⟩

/-- Law (subject `ConcreteSpec.Core.Own.stepInstrumented`; authority: docs/decisions.md#decided-2026-11-08-recovered-d3, C5).
Every finite concrete trace lifts under checker acceptance, preserving its full
ordered events and concrete endpoint, even with history-dependent answer choices. -/
theorem Steps_converse_of_accepted
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit))
    (hacc : CheckerAccepted O types permit P c)
    (tr : List O.Event) (d : Exec.Config O)
    (hd : Exec.Steps (eraseProgram P) (eraseConfig c) tr d) :
    ∃ ci, Exec.Steps P c tr ci ∧ eraseConfig ci = d := by
  generalize he : eraseConfig c = ec at hd
  induction hd generalizing c with
  | refl ec => exact ⟨c, .refl c, he⟩
  | @step ec l ec' tr ec'' hs rest ih =>
    subst ec
    obtain ⟨li, ci, hsi, hl, hi⟩ := Step_converse_of_accepted P c hacc l ec' hs
    obtain ⟨dest, hdest, hend⟩ := ih ci (checkerAccepted_step_inv P c ci li hacc hsi) hi
    refine ⟨dest, ?_, hend⟩
    have hev : li.events = l.events := by rw [← hl]; exact (eraseLabel_events_proj li).symm
    simpa only [hev] using Exec.Steps.step hsi hdest

/-- Law (authority: docs/decisions.md#decided-2026-11-05-core (Q9.10);
#decided-2026-11-08-recovered-d3 (C5 erasure);
#decided-2026-11-13-lifecycle-refusal). (subjects `ConcreteSpec.Core.Own.liftOracle`,
`ConcreteSpec.Core.Own.instrumentOperands`). Exact fuel-indexed simulation
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
                    exact request_none_of_permitted P c hperm p vs site st k
                      (liftAnswer st.own a₀) hs ha' hk
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
end ConcreteSpec.Core.Own
