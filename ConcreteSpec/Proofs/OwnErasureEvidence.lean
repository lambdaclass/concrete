import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Own
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Proofs.OwnErasure
import ConcreteSpec.Proofs.Exec

/-! # Ownership erasure instances and planted counterexamples

Authorities and held policies are recorded in `ConcreteSpec.Proofs.OwnErasure`.
Only its Laws count as evidence; these fixtures exercise them and reject one-rule mutants.
The evaluator parameter in the sole Exec dispatcher and driver supplies substitutions.
-/
namespace ConcreteSpec.Core.Own


namespace ExecErasureWitnesses
open _root_.ConcreteSpec.Core.Exec
open ConcreteSpec.Core.Own.Examples

def emptyProg : Exec.Program instrumentedOps := ⟨[]⟩

def bindReadConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.bind y (.read x) ⟨0⟩), [(x, 7)], ⟨0, shared⟩, []⟩

def primOneConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.prim (some y) () [.read x] ⟨0⟩), [(x, 7)], ⟨0, shared⟩, []⟩

/-- Helper (inversion). A step from a configuration whose `Exec.step` returns a next configuration must
land in that target configuration. -/
private theorem step_det_next_inv {O : Exec.Operands} {P : Exec.Program O}
    {c c₁ c' : Exec.Config O} {l : Exec.Label O}
    (hs : Exec.step P c = some (.next c₁)) (hstep : Exec.Step P c l c') :
    c' = c₁ :=
  ((Exec.step_silent_representation P c c₁ c' l hs).mp hstep).2

/-- Helper: reuse the execution module's missing-dispatch inversion. -/
private theorem step_det_none_inv {O : Exec.Operands} {P : Exec.Program O}
    {c c' : Exec.Config O} {l : Exec.Label O}
    (hs : Exec.step P c = none) (hstep : Exec.Step P c l c') : False :=
  Exec.no_step_of_none_inv hs l c' hstep

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

/-- Instance. A trapping operand in the instrumented instance traps with the same kind in the erased instance. -/
example : instrumentedOps.eval [(x, 7)] ⟨0, initial⟩ (.fail .abort) = .trap .abort ∧
    erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.fail .abort) = .trap .abort :=
  ⟨by decide, eval_trap_erasure_result (O := erasedOps) (types := types) (permit := permitE)
    [(x, 7)] ⟨0, initial⟩ (.fail .abort) .abort (by decide)⟩

/-- Instance. Left-to-right instrumented argument evaluation erases to the concrete argument values and store. -/
example : ∃ s' : Store Int,
    Exec.evalArgs instrumentedOps [(x, 7)] ⟨0, initial⟩ [.read x, .move x] = .ok ([7, 7], s') ∧
    Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.read x, .move x] =
      .ok ([7, 7], erase s') :=
  ⟨⟨0, (move initial ⟨0⟩).getD initial⟩, by decide, by decide⟩

/-- Instance. An argument list that traps after a move traps with the same kind in the erased instance. -/
example : Exec.evalArgs instrumentedOps [(x, 7)] ⟨0, initial⟩ [.move x, .fail .overflow] = .trap .overflow ∧
    Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.move x, .fail .overflow] =
      .trap .overflow :=
  ⟨by decide, by decide⟩

/-- Instance. Condition testing agrees between the instrumented and erased instances on both truth values. -/
example : instrumentedOps.test 7 = erasedOps.test 7 ∧
    instrumentedOps.test 0 = erasedOps.test 0 ∧
    erasedOps.test 7 = some true ∧ erasedOps.test 0 = some false :=
  ⟨test_erasure_result (O := erasedOps) (types := types) (permit := permitE) 7,
   test_erasure_result (O := erasedOps) (types := types) (permit := permitE) 0,
   by decide, by decide⟩

/-- Instance. Binding a fresh variable updates the ownership frame and erases to the concrete lifecycle step. -/
example : ∃ s' : Store Int,
    instrumentedOps.life (.bind y) ⟨7, initial⟩ = some s' ∧
    erasedOps.life (.bind y) (erase (⟨7, initial⟩ : Store Int)) = some (erase s') := by
  refine ⟨⟨7, (bind types y initial).getD initial⟩, by decide, ?_⟩
  exact instrumentLife_erasure_result (O := erasedOps) (types := types) (permit := permitE)
    (.bind y) ⟨7, initial⟩ _ (by decide)

/-- Instance. An allowed primitive answer satisfies the erased primitive contract and preserves the ownership state.
-/
example : let a : Exec.Answer Int Int (Store Int) := ⟨some 7, .ok 7, ⟨7, initial⟩⟩
    instrumentedOps.allows () [7] ⟨0, initial⟩ a = true ∧
    erasedOps.allows () [7] (erase (⟨0, initial⟩ : Store Int)) (eraseAnswer a) = true ∧
    a.store.own = initial := by
  dsimp
  exact ⟨by decide, allows_erasure_result (O := erasedOps) (types := types) (permit := permitE)
    () [7] ⟨0, initial⟩ ⟨some 7, .ok 7, ⟨7, initial⟩⟩ (by decide)⟩

/-- Instance. A silent `.bind` step on the instrumented machine erases to the matching `.bind` step on the erased
machine. -/
example : ∃ c' : Exec.Config instrumentedOps,
    Exec.step emptyProg bindReadConfig = some (.next c') ∧
    Exec.step (eraseProgram emptyProg) (eraseConfig bindReadConfig) =
      some (.next (eraseConfig c')) :=
  ⟨_, rfl, @step_next_erasure erasedOps types permitE emptyProg bindReadConfig⟩

/-- Instance. A primitive request step erases to the concrete request and its continuation commutes with
`eraseConfig`. -/
example : ∃ p vs site st k,
    Exec.step emptyProg primOneConfig = some (.request p vs site st k) ∧
    ∃ k_erased,
      Exec.step (eraseProgram emptyProg) (eraseConfig primOneConfig) =
        some (.request p vs site (erase st) k_erased) ∧
      ∀ a c', k a = some c' → k_erased (eraseAnswer a) = some (eraseConfig c') :=
  ⟨(), [7], ⟨0⟩, ⟨0, shared⟩, _, rfl,
    @step_request_erasure erasedOps types permitE emptyProg primOneConfig⟩

/-- Instance. Both silent and primitive-event single steps of the instrumented machine erase to matching steps of
the erased machine. -/
example : ∃ c' : Exec.Config instrumentedOps,
    Exec.Step emptyProg primOneConfig
      (liftAnswer shared ⟨some 7, .ok 7, 7⟩).label c' ∧
    Exec.Step (eraseProgram emptyProg) (eraseConfig primOneConfig)
      (eraseLabel (O := erasedOps) (types := types) (permit := permitE)
        (liftAnswer shared ⟨some 7, .ok 7, 7⟩).label)
      (eraseConfig c') := by
  have hs : Exec.Step emptyProg primOneConfig
      (liftAnswer shared ⟨some 7, .ok 7, 7⟩).label _ :=
    .prim rfl (by decide) rfl
  exact ⟨_, hs, @Step_erasure erasedOps types permitE emptyProg primOneConfig
    (liftAnswer shared ⟨some 7, .ok 7, 7⟩) _ (by decide) rfl⟩

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

/-- Planted: instrumentation skips the ownership lifecycle transition while running the concrete one. -/
abbrev skipLifeOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with
    life := fun ev s => do
      let own := s.own
      let concrete ← O.life ev s.concrete
      pure ⟨concrete, own⟩ }

/-- Planted: oracle lifting corrupts the ownership frame of the lifted answer (`nextLoan + 1`),
causing the instrumented machine to reject the answer with `.badAnswer` (which satisfies
`Ending.NotStuck`!) while the erased machine accepts it and emits `[7, 3]`. -/
def liftOracleMutateOwn (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (ρ : Exec.Oracle O) : Exec.Oracle (instrumentOperands O types permit) :=
  fun hist p vs site s =>
    (ρ hist p vs site s.concrete).map fun a =>
      liftAnswer { s.own with nextLoan := s.own.nextLoan + 1 } a

/-- Planted: uniform step closure of the success-value mutation. -/
def stepInstrumentedZeroVal (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    Option (Exec.Out (instrumentOperands O types permit)) :=
  Exec.step P c (zeroOperands O types permit).eval

/-- Planted: uniform step closure of the successful-operand refusal mutation. -/
def stepInstrumentedRefuse (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    Option (Exec.Out (instrumentOperands O types permit)) :=
  Exec.step P c (stuckOperands O types permit).eval

/-- Planted: oracle lifting drops the concrete oracle's answer and returns `none`. -/
def liftOracleDropAnswer (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (ρ : Exec.Oracle O) : Exec.Oracle (instrumentOperands O types permit) :=
  fun hist p vs site s => match ρ hist p vs site s.concrete with
    | none => none
    | some _ => none

def primFrameStuckConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.prim none () [.lit 0] ⟨0⟩), [(x, 7)], ⟨0, shared⟩,
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
      have hown : a.store.own = shared :=
        (allows_erasure_result (O := erasedOps) (types := types) (permit := permitE)
          () [0] ⟨0, shared⟩ a hallows).2
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

/-- Instance (law `ConcreteSpec.Core.Own.step_next_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindReadConfig`). -/
example : (match stepInstrumented erasedOps types permitE emptyProg bindReadConfig with
    | some (.next c') => Exec.step (eraseProgram emptyProg) (eraseConfig bindReadConfig) = some (.next (eraseConfig c'))
    | _ => True) :=
  @step_next_erasure erasedOps types permitE emptyProg bindReadConfig

/-- Planted (law `ConcreteSpec.Core.Own.step_next_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindReadConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedZeroVal`). -/
example : (∃ ci, stepInstrumentedZeroVal erasedOps types permitE emptyProg bindReadConfig = some (.next ci)) ∧
    ¬ (match stepInstrumentedZeroVal erasedOps types permitE emptyProg bindReadConfig with
    | some (.next c') => Exec.step (eraseProgram emptyProg) (eraseConfig bindReadConfig) = some (.next (eraseConfig c'))
    | _ => True) := by
  constructor
  · exact ⟨_, rfl⟩
  · intro h
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
example : (∃ p vs site st k a ci,
      stepInstrumentedZeroVal erasedOps types permitE emptyProg primOneConfig =
        some (.request p vs site st k) ∧
      instrumentedOps.allows p vs st a = true ∧ k a = some ci) ∧
    ¬ (match stepInstrumentedZeroVal erasedOps types permitE emptyProg primOneConfig with
    | some (.request p vs site st k) =>
      ∃ k_erased,
        Exec.step (eraseProgram emptyProg) (eraseConfig primOneConfig) =
          some (.request p vs site (erase st) k_erased) ∧
        ∀ a c', k a = some c' → k_erased (eraseAnswer a) = some (eraseConfig c')
    | _ => True) := by
  constructor
  · exact ⟨(), [0], ⟨0⟩, ⟨0, shared⟩, _, liftAnswer shared ⟨some 0, .ok 0, 0⟩, _, rfl, rfl, rfl⟩
  · rintro ⟨_, h, _⟩
    cases h

/-- Instance (law `ConcreteSpec.Core.Own.Step_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindReadConfig`). -/
example : (match stepInstrumented erasedOps types permitE emptyProg bindReadConfig with
    | some (.next c') =>
      Exec.Step (eraseProgram emptyProg) (eraseConfig bindReadConfig) .silent (eraseConfig c')
    | some (.request p vs _site st k) =>
      ∀ a c', (instrumentOperands erasedOps types permitE).allows p vs st a = true →
        k a = some c' →
        Exec.Step (eraseProgram emptyProg) (eraseConfig bindReadConfig) (eraseLabel a.label) (eraseConfig c')
    | none => True) :=
  @Step_erasure erasedOps types permitE emptyProg bindReadConfig

/-- Planted (law `ConcreteSpec.Core.Own.Step_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindReadConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedZeroVal`). -/
example : (∃ ci, stepInstrumentedZeroVal erasedOps types permitE emptyProg bindReadConfig = some (.next ci)) ∧
    ¬ (match stepInstrumentedZeroVal erasedOps types permitE emptyProg bindReadConfig with
    | some (.next c') =>
      Exec.Step (eraseProgram emptyProg) (eraseConfig bindReadConfig) .silent (eraseConfig c')
    | some (.request p vs _site st k) =>
      ∀ a c', (instrumentOperands erasedOps types permitE).allows p vs st a = true →
        k a = some c' →
        Exec.Step (eraseProgram emptyProg) (eraseConfig bindReadConfig) (eraseLabel a.label) (eraseConfig c')
    | none => True) := by
  constructor
  · exact ⟨_, rfl⟩
  · intro h
    have heq := step_det_next_inv rfl h
    cases heq

/-- Instance (law `ConcreteSpec.Core.Own.Steps_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindReadConfig`). -/
example : FiniteSimulation erasedOps types permitE
    (stepInstrumented erasedOps types permitE emptyProg) emptyProg bindReadConfig :=
  @Steps_erasure erasedOps types permitE emptyProg bindReadConfig

/-- Endpoint of the mutant's one-step execution: it has corrupted the newly bound value. -/
def zeroSuccessor : Exec.Config instrumentedOps :=
  ⟨.done, [(y, 0), (x, 7)], ⟨0, (bind types y shared).getD shared⟩, []⟩

/-- Planted (law `ConcreteSpec.Core.Own.Steps_erasure`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindReadConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedZeroVal`). -/
example : FiniteExecution (stepInstrumentedZeroVal erasedOps types permitE emptyProg)
      bindReadConfig [] zeroSuccessor ∧
    ¬ FiniteSimulation erasedOps types permitE
    (stepInstrumentedZeroVal erasedOps types permitE emptyProg) emptyProg bindReadConfig := by
  constructor
  · exact .silent rfl (.refl _)
  · intro h
    have bad : FiniteExecution (stepInstrumentedZeroVal erasedOps types permitE emptyProg)
        bindReadConfig [] zeroSuccessor := .silent rfl (.refl _)
    have impossible : ∀ tr, ¬ Exec.Steps (eraseProgram emptyProg)
        (eraseConfig bindReadConfig) tr (eraseConfig zeroSuccessor) := by
      intro tr he
      cases he with
      | step hs rest =>
        have hd := step_det_next_inv (O := erasedOps) (P := eraseProgram emptyProg) (c₁ := ⟨.done, [(y, 7), (x, 7)],
            0, []⟩) rfl hs
        subst hd
        cases rest with
        | step hs _ => exact step_det_none_inv rfl hs
    exact impossible [] (h [] zeroSuccessor bad)

/-- Instance (law `ConcreteSpec.Core.Own.runFrom_erasure`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.primOneConfig`).
-/
example : Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primOneConfig) =
    eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] primOneConfig) :=
  @runFrom_erasure erasedOps types permitE emptyProg erasedOracle 100 [] primOneConfig rfl

/-- Planted (law `ConcreteSpec.Core.Own.runFrom_erasure`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.primOneConfig`,
subject `ConcreteSpec.Core.Own.liftOracle`, mutant
`ConcreteSpec.Core.Own.ExecErasureWitnesses.liftOracleMutateOwn`). -/
example : Ending.NotStuck (Exec.runFrom emptyProg
      (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 [] primOneConfig).ending ∧
    ¬ (Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primOneConfig) =
      eraseReport (Exec.runFrom emptyProg (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 []
          primOneConfig)) := by
  exact ⟨rfl, by intro h; have he := congrArg (fun r => r.events) h; cases he⟩

/-- Instance (law `ConcreteSpec.Core.Own.run_erasure`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.primOneConfig`). -/
example : Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primOneConfig) =
    eraseReport (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 primOneConfig) :=
  @run_erasure erasedOps types permitE emptyProg erasedOracle 100 primOneConfig rfl

/-- Planted (law `ConcreteSpec.Core.Own.run_erasure`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.primOneConfig`, subject
`ConcreteSpec.Core.Own.liftOracle`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.liftOracleMutateOwn`). -/
example : Ending.NotStuck (Exec.run emptyProg
      (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 primOneConfig).ending ∧
    ¬ (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primOneConfig) =
      eraseReport (Exec.run emptyProg (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100
          primOneConfig)) := by
  exact ⟨rfl, by intro h; have he := congrArg (fun r => r.events) h; cases he⟩

/-- Helper (inversion). The single-step move-binding configuration `bindReadConfig`
satisfies `CheckerAccepted`. -/
private theorem bindRead_checkerAccepted_inv :
    CheckerAccepted erasedOps types permitE emptyProg bindReadConfig := by
  intro tr c hsteps
  cases hsteps with
  | refl =>
    refine ⟨by decide, ⟨fun _ => by decide, fun v s' he => ?_⟩⟩
    intro _
    have he' : instrumentEval erasedOps permitE [(x, 7)] ⟨0, shared⟩ (.read x) = .ok (v, s') := he
    injection he' with hpair
    rcases Prod.mk.inj hpair with ⟨_, rfl⟩
    decide
  | step hs₁ hsteps =>
    obtain rfl := step_det_next_inv rfl hs₁
    cases hsteps with
    | refl => exact ⟨by decide, trivial⟩
    | step hs₂ _ => exact False.elim (step_det_none_inv rfl hs₂)

/-- Instance (law `ConcreteSpec.Core.Own.step_stuck_iff_of_accepted`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindReadConfig`). -/
example : stepInstrumented erasedOps types permitE emptyProg bindReadConfig = none ↔
    Exec.step (eraseProgram emptyProg) (eraseConfig bindReadConfig) = none :=
  @step_stuck_iff_of_accepted erasedOps types permitE emptyProg bindReadConfig
    bindRead_checkerAccepted_inv

/-- Planted (law `ConcreteSpec.Core.Own.step_stuck_iff_of_accepted`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindReadConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedRefuse`). -/
example : CheckerAccepted erasedOps types permitE emptyProg bindReadConfig ∧
    ¬ (stepInstrumentedRefuse erasedOps types permitE emptyProg bindReadConfig = none ↔
      Exec.step (eraseProgram emptyProg) (eraseConfig bindReadConfig) = none) :=
  ⟨bindRead_checkerAccepted_inv, fun h => by cases h.mp rfl⟩

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

/-- Instance (law `ConcreteSpec.Core.Own.evalArgs_ok_erasure`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
example : (match Exec.evalArgs (instrumentOperands erasedOps types permitE) [(x, 7)] ⟨0, shared⟩ [.read x, .read x]
    with
    | .ok (vs, t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, shared⟩ : Store Int)) [.read x, .read x] = .ok
        (vs, erase t)
    | _ => True) :=
  @evalArgs_ok_erasure erasedOps types permitE [(x, 7)] [.read x, .read x] ⟨0, shared⟩

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_ok_erasure`, case `ConcreteSpec.Core.Own.Examples.shared`, subject
`ConcreteSpec.Core.Own.instrumentOperands`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.zeroOperands`). -/
example : Exec.evalArgs (zeroOperands erasedOps types permitE) [(x, 7)] ⟨0, shared⟩
      [.read x, .read x] = .ok ([0, 0], ⟨0, shared⟩) ∧
    ¬ (match Exec.evalArgs (zeroOperands erasedOps types permitE) [(x, 7)] ⟨0, shared⟩ [.read x, .read x] with
    | .ok (vs, t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, shared⟩ : Store Int)) [.read x, .read x] = .ok
        (vs, erase t)
    | _ => True) := by
  constructor
  · rfl
  · change ¬ ((Res.ok ([7, 7], 0) : Res (List Int × Int)) = .ok ([0, 0], 0))
    decide

/-- Instance (law `ConcreteSpec.Core.Own.evalArgs_trap_erasure`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
example : (match Exec.evalArgs (instrumentOperands erasedOps types permitE) [(x, 7)] ⟨0, shared⟩ [.read x, .fail
    .overflow] with
    | .trap kd => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, shared⟩ : Store Int)) [.read x, .fail .overflow] =
        .trap kd
    | _ => True) :=
  @evalArgs_trap_erasure erasedOps types permitE [(x, 7)] [.read x, .fail .overflow] ⟨0, shared⟩

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_trap_erasure`, case `ConcreteSpec.Core.Own.Examples.shared`,
subject `ConcreteSpec.Core.Own.instrumentOperands`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.abortOperands`).
-/
example : Exec.evalArgs (abortOperands erasedOps types permitE) [(x, 7)] ⟨0, shared⟩
      [.read x, .fail .overflow] = .trap .abort ∧
    ¬ (match Exec.evalArgs (abortOperands erasedOps types permitE) [(x, 7)] ⟨0, shared⟩ [.read x, .fail
    .overflow] with
    | .trap kd => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, shared⟩ : Store Int)) [.read x, .fail .overflow] =
        .trap kd
    | _ => True) := by
  constructor
  · rfl
  · change ¬ ((Res.trap .overflow : Res (List Int × Int)) = .trap .abort)
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

/-- Instance (law `ConcreteSpec.Core.Own.evalArgs_trap_erasure`, case
`ConcreteSpec.Core.Own.Examples.shared`). The first read succeeds before a real overflow trap. -/
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
  match (permit L e s.own) with
  | none => .stuck
  | some own => match O.eval L s.concrete e with
    | .ok (v, concrete) => .ok (v, ⟨concrete, own⟩)
    | .trap k => .trap k
    | .stuck => .ok (O.unitVal, ⟨s.concrete, own⟩)

/-- A concrete missing-local failure with real ownership storage and a live shared loan.
CheckerAccepted is the abstract permission obligation, not a proof of concrete-local availability. -/
def missingConcrete : Exec.Config instrumentedOps :=
  ⟨.exec (.ret (.read x) ⟨0⟩), [(y, 9)], ⟨4, shared⟩, []⟩

/-- Helper: no real transition leaves this configuration; its permission premise is inhabited. -/
private theorem missingAccepted : CheckerAccepted erasedOps types permitE emptyProg missingConcrete := by
  intro tr c hs
  cases hs with
  | refl => exact ⟨by decide, fun h => False.elim (h rfl)⟩
  | step h _ => exact False.elim (step_det_none_inv rfl h)

/-- Planted (law `ConcreteSpec.Core.Own.step_stuck_iff_of_accepted`, case
`ConcreteSpec.Core.Own.PopulatedErasureEvidence.missingConcrete`,
subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.PopulatedErasureEvidence.recoverStuck`).
The missing `x` leaves concrete evaluation stuck despite populated locals and loans. -/
example :
    CheckerAccepted erasedOps types permitE emptyProg missingConcrete ∧
    ¬ (Exec.step emptyProg missingConcrete (recoverStuck erasedOps permitE) = none ↔
      Exec.step (eraseProgram emptyProg) (eraseConfig missingConcrete) = none) :=
  ⟨missingAccepted, fun h => by cases h.mpr rfl⟩

/-- Planted: change only successful concrete evaluation into abort, keeping refusal,
concrete traps and concrete stuckness intact. This isolates control erasure from value erasure. -/
def abortSuccess (O : Exec.Operands)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) : Res (O.Val × Store O.Store) :=
  match (permit L e s.own) with
  | none => .stuck
  | some _ => match O.eval L s.concrete e with
    | .ok _ => .trap .abort
    | .trap k => .trap k
    | .stuck => .stuck

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_trap_erasure`, case
`ConcreteSpec.Core.Own.PopulatedErasureEvidence.readConfig`,
subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.PopulatedErasureEvidence.abortSuccess`).
The actual loan-protected read succeeds before its outcome is corrupted to abort. -/
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

/-- Planted (law `ConcreteSpec.Core.Own.step_next_erasure`, case
`ConcreteSpec.Core.Own.PopulatedErasureEvidence.readConfig`,
subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.PopulatedErasureEvidence.abortSuccess`).
The sole dispatcher returns a trapped configuration instead of binding the local. -/
example :
    stepInstrumented erasedOps types permitE emptyProg readConfig = some (.next readDestination) ∧
    ¬ (match Exec.step emptyProg readConfig (abortSuccess erasedOps permitE) with
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

/-- Instance (law `ConcreteSpec.Core.Own.run_stuck_iff_of_accepted`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.primFrameStuckConfig`).
The checker-accepted request waits when the actual oracle gives no answer. -/
example :
    CheckerAccepted erasedOps types permitE emptyProg primFrameStuckConfig ∧
    Ending.IsStuck (Exec.run emptyProg
      (liftOracle erasedOps types permitE (fun _ _ _ _ _ => none)) 10 primFrameStuckConfig).ending =
      Ending.IsStuck (Exec.run (eraseProgram emptyProg)
        (fun _ _ _ _ _ => none) 10 (eraseConfig primFrameStuckConfig)).ending :=
  ⟨primFrameStuck_checkerAccepted_inv,
    run_stuck_iff_of_accepted emptyProg (fun _ _ _ _ _ => none) 10
      primFrameStuckConfig primFrameStuck_checkerAccepted_inv⟩

/-- Planted (law `ConcreteSpec.Core.Own.runFrom_erasure_of_accepted`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.primFrameStuckConfig`,
subject `ConcreteSpec.Core.Own.liftOracle`,
mutant `ConcreteSpec.Core.Own.AddedAnswerEvidence.inventAnswer`).
The fabricated allowed answer adds a transition and event before the unbound
assignment gets stuck. It also refutes accepted run stuck equivalence. -/
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

namespace RefusalEvidence
open Examples ExecErasureWitnesses PopulatedErasureEvidence

/-- Planted: replace only permission refusal by concrete evaluation with the current
ledger. Permitted evaluations, including their ownership updates, remain unchanged. -/
abbrev skipPermitOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with
    eval := fun (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) =>
      match (permit L e s.own) with
      | some _ => instrumentEval O permit L s e
      | none => match (O.eval L s.concrete e) with
        | .ok (v, concrete) => .ok (v, ⟨concrete, s.own⟩)
        | .trap kd => .trap kd
        | .stuck => .stuck }

/-- Planted: swallow only a concrete trap after permission succeeds. -/
abbrev swallowTrapOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with eval := fun (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) =>
      match (permit L e s.own) with
      | none => .stuck
      | some own => match O.eval L s.concrete e with
        | .ok (v, concrete) => .ok (v, ⟨concrete, own⟩)
        | .trap _ => .ok (O.unitVal, ⟨s.concrete, own⟩)
        | .stuck => .stuck }

/-- Instance (law `ConcreteSpec.Core.Own.eval_refusal`, case
`ConcreteSpec.Core.Own.Examples.shared`). A live loan forbids moving its root. -/
example : permitE [(x, 7)] (.move x) shared = none ∧
    instrumentedOps.eval [(x, 7)] ⟨4, shared⟩ (.move x) = .stuck :=
  ⟨by decide, eval_refusal _ _ _ (by decide)⟩

/-- Planted (law `ConcreteSpec.Core.Own.eval_refusal`, case
`ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.RefusalEvidence.skipPermitOperands`). -/
example : permitE [(x, 7)] (.move x) shared = none ∧
    ¬ ((skipPermitOperands erasedOps types permitE).eval [(x, 7)] ⟨4, shared⟩ (.move x) = .stuck) :=
  ⟨by decide, by decide⟩

/-- Instance (law `ConcreteSpec.Core.Own.evalArgs_refusal`, case
`ConcreteSpec.Core.Own.Examples.shared`). Refusal precedes a later trap. -/
example : permitE [(x, 7)] (.move x) shared = none ∧
    Exec.evalArgs instrumentedOps [(x, 7)] ⟨4, shared⟩ [.move x, .fail .overflow] = .stuck :=
  ⟨by decide, evalArgs_refusal _ _ _ _ (by decide)⟩

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_refusal`, case
`ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.RefusalEvidence.skipPermitOperands`). -/
example : permitE [(x, 7)] (.move x) shared = none ∧
    ¬ (Exec.evalArgs (skipPermitOperands erasedOps types permitE) [(x, 7)] ⟨4, shared⟩
      [.move x, .fail .overflow] = .stuck) := ⟨by decide, by decide⟩

/-- Instance (law `ConcreteSpec.Core.Own.life_refusal`, case
`ConcreteSpec.Core.Own.Examples.shared`). Rebinding a loaned live owner refuses. -/
example : life types (.bind x) shared = none ∧
    instrumentedOps.life (.bind x) ⟨4, shared⟩ = none :=
  ⟨by decide, life_refusal _ _ (by decide)⟩

/-- Planted (law `ConcreteSpec.Core.Own.life_refusal`, case
`ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.skipLifeOperands`). -/
example : life types (.bind x) shared = none ∧
    ¬ ((skipLifeOperands erasedOps types permitE).life (.bind x) ⟨4, shared⟩ = none) :=
  ⟨by decide, by decide⟩

/-- Instance (law `ConcreteSpec.Core.Own.step_eval_refusal`, case
`ConcreteSpec.Core.Own.Examples.shared`). No transition, including a trap transition, is available. -/
example : permitE [(x, 7)] (.move x) shared = none ∧
    Exec.step emptyProg ⟨.exec (.bind y (.move x) ⟨0⟩), [(x, 7)], ⟨4, shared⟩, []⟩ = none :=
  ⟨by decide, step_eval_refusal _ _ _ _ _ _ _ (by decide)⟩

/-- Planted (law `ConcreteSpec.Core.Own.step_eval_refusal`, case
`ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.RefusalEvidence.skipPermitOperands`). -/
example : permitE [(x, 7)] (.move x) shared = none ∧
    ¬ (Exec.step emptyProg ⟨.exec (.bind y (.move x) ⟨0⟩), [(x, 7)], ⟨4, shared⟩, []⟩
      (skipPermitOperands erasedOps types permitE).eval = none) := ⟨by decide, by decide⟩

/-- Instance (law `ConcreteSpec.Core.Own.step_life_refusal`, case
`ConcreteSpec.Core.Own.Examples.shared`). The operand succeeds but result binding refuses. -/
example : instrumentedOps.eval [(x, 7)] ⟨4, shared⟩ (.lit 7) = .ok (7, ⟨4, shared⟩) ∧
    life types (.bind x) shared = none ∧
    Exec.step emptyProg ⟨.exec (.bind x (.lit 7) ⟨0⟩), [(x, 7)], ⟨4, shared⟩, []⟩ = none :=
  ⟨rfl, by decide, step_life_refusal _ _ _ _ _ _ _ _ _ rfl (by decide)⟩

/-- Planted (law `ConcreteSpec.Core.Own.step_life_refusal`, case
`ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.skipLifeOperands`). The real dispatcher
runs the mutated lifecycle; only its ownership admission is skipped. -/
example : (skipLifeOperands erasedOps types permitE).eval [(x, 7)] ⟨4, shared⟩ (.lit 7) =
      .ok (7, ⟨4, shared⟩) ∧
    life types (.bind x) shared = none ∧
    ¬ (Exec.step (⟨[]⟩ : Exec.Program (skipLifeOperands erasedOps types permitE))
      ⟨.exec (.bind x (.lit 7) ⟨0⟩), [(x, 7)], ⟨4, shared⟩, []⟩ = none) :=
  ⟨rfl, by decide, by decide⟩

/-- Helper: one loan-protected read has an inhabited argument-permission premise. -/
theorem readPermitted : ArgsPermitted erasedOps types permitE [(x, 7)] ⟨4, shared⟩ [.read x] :=
  ⟨fun _ => by decide, fun _ _ _ => trivial⟩

/-- Helper: a real overflow trap is permitted; it is not a refused ownership operation. -/
theorem trapPermitted : ArgsPermitted erasedOps types permitE [(x, 7)] ⟨4, shared⟩ [.fail .overflow] :=
  ⟨fun _ => rfl, fun _ _ _ => trivial⟩

/-- Instance (law `ConcreteSpec.Core.Own.evalArgs_erasure_of_permitted`, case
`ConcreteSpec.Core.Own.Examples.shared`). Values and concrete state survive erasure. -/
example : ArgsPermitted erasedOps types permitE [(x, 7)] ⟨4, shared⟩ [.read x] ∧
    Exec.evalArgs erasedOps [(x, 7)] 4 [.read x] =
      (Exec.evalArgs instrumentedOps [(x, 7)] ⟨4, shared⟩ [.read x]).map
        (fun (vs, t) => (vs, erase t)) :=
  ⟨readPermitted, evalArgs_erasure_of_permitted _ _ _ readPermitted⟩

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_erasure_of_permitted`, case
`ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.zeroOperands`). -/
example : ArgsPermitted erasedOps types permitE [(x, 7)] ⟨4, shared⟩ [.read x] ∧
    ¬ (Exec.evalArgs erasedOps [(x, 7)] 4 [.read x] =
      (Exec.evalArgs (zeroOperands erasedOps types permitE) [(x, 7)] ⟨4, shared⟩ [.read x]).map
        (fun (vs, t) => (vs, erase t))) := ⟨readPermitted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.Own.evalArgs_stuck_iff_of_permitted`, case
`ConcreteSpec.Core.Own.Examples.shared`). Permission does not hide concrete stuckness. -/
example : ArgsPermitted erasedOps types permitE [(x, 7)] ⟨4, shared⟩ [.read y] ∧
    (Exec.evalArgs instrumentedOps [(x, 7)] ⟨4, shared⟩ [.read y] = .stuck ↔
      Exec.evalArgs erasedOps [(x, 7)] 4 [.read y] = .stuck) :=
  ⟨⟨fun h => False.elim (h rfl), fun _ _ _ => trivial⟩,
    evalArgs_stuck_iff_of_permitted _ _ _ ⟨fun h => False.elim (h rfl), fun _ _ _ => trivial⟩⟩

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_stuck_iff_of_permitted`, case
`ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stuckOperands`). -/
example : ArgsPermitted erasedOps types permitE [(x, 7)] ⟨4, shared⟩ [.read x] ∧
    ¬ (Exec.evalArgs (stuckOperands erasedOps types permitE) [(x, 7)] ⟨4, shared⟩ [.read x] = .stuck ↔
      Exec.evalArgs erasedOps [(x, 7)] 4 [.read x] = .stuck) := ⟨readPermitted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.Own.evalArgs_trap_iff_of_permitted`, case
`ConcreteSpec.Core.Own.Examples.shared`). The concrete and tracked traps actually occur. -/
example : ArgsPermitted erasedOps types permitE [(x, 7)] ⟨4, shared⟩ [.fail .overflow] ∧
    Exec.evalArgs instrumentedOps [(x, 7)] ⟨4, shared⟩ [.fail .overflow] = .trap .overflow ∧
    (Exec.evalArgs instrumentedOps [(x, 7)] ⟨4, shared⟩ [.fail .overflow] = .trap .overflow ↔
      Exec.evalArgs erasedOps [(x, 7)] 4 [.fail .overflow] = .trap .overflow) :=
  ⟨trapPermitted, rfl, evalArgs_trap_iff_of_permitted _ _ _ trapPermitted _⟩

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_trap_iff_of_permitted`, case
`ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.RefusalEvidence.swallowTrapOperands`). -/
example : ArgsPermitted erasedOps types permitE [(x, 7)] ⟨4, shared⟩ [.fail .overflow] ∧
    ¬ (Exec.evalArgs (swallowTrapOperands erasedOps types permitE) [(x, 7)] ⟨4, shared⟩
        [.fail .overflow] = .trap .overflow ↔
      Exec.evalArgs erasedOps [(x, 7)] 4 [.fail .overflow] = .trap .overflow) :=
  ⟨trapPermitted, by decide⟩

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_ok_erasure`, case
`ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.RefusalEvidence.swallowTrapOperands`). A swallowed trap
cannot masquerade as a concrete argument value. -/
example : Exec.evalArgs (swallowTrapOperands erasedOps types permitE) [(x, 7)] ⟨4, shared⟩
      [.fail .overflow] = .ok ([0], ⟨4, shared⟩) ∧
    ¬ (match Exec.evalArgs (swallowTrapOperands erasedOps types permitE) [(x, 7)] ⟨4, shared⟩
        [.fail .overflow] with
      | .ok (vs, t) => Exec.evalArgs erasedOps [(x, 7)] 4 [.fail .overflow] = .ok (vs, erase t)
      | _ => True) := ⟨rfl, by intro h; cases h⟩

/-- Planted (law `ConcreteSpec.Core.Own.step_next_erasure`, case
`ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.RefusalEvidence.swallowTrapOperands`). -/
example : ¬ (match Exec.step emptyProg
      ⟨.exec (.ret (.fail .overflow) ⟨0⟩), [(x, 7)], ⟨4, shared⟩, []⟩
      (swallowTrapOperands erasedOps types permitE).eval with
    | some (.next ci) => Exec.step (eraseProgram emptyProg)
        ⟨.exec (.ret (.fail .overflow) ⟨0⟩), [(x, 7)], 4, []⟩ = some (.next (eraseConfig ci))
    | _ => True) := by intro h; cases h

/-- Planted (law `ConcreteSpec.Core.Own.runFrom_erasure`, case
`ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.RefusalEvidence.swallowTrapOperands`). Substitution is
threaded through every driver transition, including the terminal result. -/
example : let c : Exec.Config instrumentedOps :=
      ⟨.exec (.ret (.fail .overflow) ⟨0⟩), [(x, 7)], ⟨4, shared⟩, []⟩
    Ending.NotStuck (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle)
      3 [] c (swallowTrapOperands erasedOps types permitE).eval).ending ∧
    ¬ (Exec.runFrom (eraseProgram emptyProg) erasedOracle 3 [] (eraseConfig c) =
      eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle)
        3 [] c (swallowTrapOperands erasedOps types permitE).eval)) := by
  exact ⟨rfl, by intro h; have he := congrArg (fun r => r.ending) h; cases he⟩

/-- Planted (law `ConcreteSpec.Core.Own.run_erasure`, case
`ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.instrumentOperands`,
mutant `ConcreteSpec.Core.Own.RefusalEvidence.swallowTrapOperands`). -/
example : let c : Exec.Config instrumentedOps :=
      ⟨.exec (.ret (.fail .overflow) ⟨0⟩), [(x, 7)], ⟨4, shared⟩, []⟩
    Ending.NotStuck (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle)
      3 c (swallowTrapOperands erasedOps types permitE).eval).ending ∧
    ¬ (Exec.run (eraseProgram emptyProg) erasedOracle 3 (eraseConfig c) =
      eraseReport (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle)
        3 c (swallowTrapOperands erasedOps types permitE).eval)) := by
  exact ⟨rfl, by intro h; have he := congrArg (fun r => r.ending) h; cases he⟩

end RefusalEvidence

namespace RelationalConverseEvidence
open Examples ExecErasureWitnesses PopulatedErasureEvidence

/-- Planted: the existing successful-operand refusal mutant, lifted through the
shared dispatcher adapter. No second transition dispatcher is introduced. -/
def refuseStep (P : Exec.Program instrumentedOps) (c : Exec.Config instrumentedOps)
    (l : Exec.Label instrumentedOps) (d : Exec.Config instrumentedOps) : Prop :=
  ∃ a, Exec.Dispatches (stepInstrumentedRefuse erasedOps types permitE) P c a l d

/-- Planted: finite closure of exactly `refuseStep`, so the same operand mutation
applies at every transition of a trace. -/
inductive RefuseSteps (P : Exec.Program instrumentedOps) :
    Exec.Config instrumentedOps → List Int → Exec.Config instrumentedOps → Prop where
  | refl (c) : RefuseSteps P c [] c
  | step {c l c' tr d} : refuseStep P c l c' → RefuseSteps P c' tr d →
      RefuseSteps P c (l.events ++ tr) d

/-- Helper: the planted evaluator refuses the populated read before any labeled step. -/
theorem refuseRead (l : Exec.Label instrumentedOps) (d : Exec.Config instrumentedOps) :
    ¬ refuseStep emptyProg readConfig l d := by
  rintro ⟨_, h⟩
  cases h with
  | silent h => cases h
  | prim h _ _ => cases h

/-- Instance (law `ConcreteSpec.Core.Own.Step_converse_of_accepted`, case
`ConcreteSpec.Core.Own.PopulatedErasureEvidence.readConfig`). Both acceptance and a
concrete transition are populated before invoking the converse. -/
example : CheckerAccepted erasedOps types permitE emptyProg readConfig ∧
    Exec.Step (eraseProgram emptyProg) (eraseConfig readConfig) .silent (eraseConfig readDestination) ∧
    ∃ li ci, Exec.Step emptyProg readConfig li ci ∧
      eraseLabel li = .silent ∧ eraseConfig ci = eraseConfig readDestination :=
  ⟨readAccepted, .silent rfl,
    @Step_converse_of_accepted erasedOps types permitE _ _ readAccepted _ _ (.silent rfl)⟩

/-- Planted (law `ConcreteSpec.Core.Own.Step_converse_of_accepted`, case
`ConcreteSpec.Core.Own.PopulatedErasureEvidence.readConfig`, subject `ConcreteSpec.Core.Exec.Step`,
mutant `ConcreteSpec.Core.Own.RelationalConverseEvidence.refuseStep`). -/
example : CheckerAccepted erasedOps types permitE emptyProg readConfig ∧
    Exec.Step (eraseProgram emptyProg) (eraseConfig readConfig) .silent (eraseConfig readDestination) ∧
    ¬ (∃ li ci, refuseStep emptyProg readConfig li ci ∧
      eraseLabel li = .silent ∧ eraseConfig ci = eraseConfig readDestination) :=
  ⟨readAccepted, .silent rfl, by rintro ⟨li, ci, h, _, _⟩; exact refuseRead li ci h⟩

/-- Instance (law `ConcreteSpec.Core.Own.Steps_converse_of_accepted`, case
`ConcreteSpec.Core.Own.PopulatedErasureEvidence.readConfig`). A nonempty execution lifts. -/
example : CheckerAccepted erasedOps types permitE emptyProg readConfig ∧
    Exec.Steps (eraseProgram emptyProg) (eraseConfig readConfig) [] (eraseConfig readDestination) ∧
    ∃ ci, Exec.Steps emptyProg readConfig [] ci ∧ eraseConfig ci = eraseConfig readDestination :=
  ⟨readAccepted, .step (.silent rfl) (.refl _),
    @Steps_converse_of_accepted erasedOps types permitE _ _ readAccepted _ _ (.step (.silent rfl) (.refl _))⟩

/-- Planted (law `ConcreteSpec.Core.Own.Steps_converse_of_accepted`, case
`ConcreteSpec.Core.Own.PopulatedErasureEvidence.readConfig`, subject `ConcreteSpec.Core.Exec.Steps`,
mutant `ConcreteSpec.Core.Own.RelationalConverseEvidence.RefuseSteps`). -/
example : CheckerAccepted erasedOps types permitE emptyProg readConfig ∧
    Exec.Steps (eraseProgram emptyProg) (eraseConfig readConfig) [] (eraseConfig readDestination) ∧
    ¬ (∃ ci, RefuseSteps emptyProg readConfig [] ci ∧ eraseConfig ci = eraseConfig readDestination) := by
  refine ⟨readAccepted, .step (.silent rfl) (.refl _), ?_⟩
  rintro ⟨ci, ht, he⟩
  generalize ([] : List Int) = tr at ht
  cases ht with
  | refl => cases he
  | step hs _ => exact refuseRead _ _ hs

/-- Instance (law `ConcreteSpec.Core.Own.Step_converse_of_accepted`, case
`ConcreteSpec.Core.Own.ExecErasureWitnesses.primFrameStuckConfig`). This case lifts an
actual allowed primitive answer and its event, independently of any oracle. -/
example : CheckerAccepted erasedOps types permitE emptyProg primFrameStuckConfig ∧
    ∃ li ci, Exec.Step emptyProg primFrameStuckConfig li ci ∧
      eraseLabel (O := erasedOps) (types := types) (permit := permitE) li = .emit 0 ∧
      eraseConfig (O := erasedOps) (types := types) (permit := permitE) ci = ⟨.done, [(x, 7)], 0, [.seq (.assign y (.lit 1) ⟨1⟩)]⟩ :=
  ⟨primFrameStuck_checkerAccepted_inv,
    @Step_converse_of_accepted erasedOps types permitE _ _ primFrameStuck_checkerAccepted_inv _ _
      (Exec.Step.prim (O := erasedOps) (a := (⟨some 0, .ok 0, 0⟩ : Exec.Answer Int Int Int)) rfl rfl rfl)⟩

end RelationalConverseEvidence

end ConcreteSpec.Core.Own
