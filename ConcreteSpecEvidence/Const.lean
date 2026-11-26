import ConcreteSpecEvidence.ConstExamples
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Const
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Proofs.Const

namespace ConcreteSpec.Core

/-- Helper (computation). Empty environments trivially agree. -/
private theorem agree_nil : Agree [] [] := by
  intro _ _ h
  cases h

/-- Helper (computation). A `DependencyOrder` witnesses `Acyclic`. -/
private theorem acyclic_of_dependencyOrder (U : ConstTypes) (source order : List (ConstDecl U))
    (hord : DependencyOrder U source order) : Acyclic U source :=
  fun chain hcycle => cycle_excludes_dependencyOrder U source chain order hcycle hord

/-- Mutant: only division-by-zero is misreported as budget exhaustion. -/
def divisionAsFuel (env : AList ConstId CVal) (fuel : Nat) (e : CExpr) : CEvalResult :=
  match e.eval env fuel with
  | .failure ⟨.plain .div, _, .arithmetic .divisionByZero⟩ => .outOfFuel
  | r => r

/-- Mutant: only wrapping addition is incorrectly refused as invalid. -/
def refuseWrappingAdd (env : AList ConstId CVal) (fuel : Nat) (e : CExpr) : CEvalResult :=
  match e with
  | .wrapping .add _ _ => .invalid e
  | _ => e.eval env fuel

/-- Instance (law `ConcreteSpec.Core.eval_wellTyped_progress`, case `div_zero`). -/
theorem eval_wellTyped_progress_example_div_zero : (∃ v, (CExpr.plain .div (.int .i8 1) (.int .i8 0)).eval [] 2 = .value v ∧
      v.ty = .int .i8 ∧ v.Valid) ∨
    (∃ f, (CExpr.plain .div (.int .i8 1) (.int .i8 0)).eval [] 2 = .failure f ∧
      f.InExpr (.plain .div (.int .i8 1) (.int .i8 0))) :=
  eval_wellTyped_progress [] [] agree_nil
    (.plain .div (.int .i8 1) (.int .i8 0)) (.int .i8) (by decide) 2 (by decide)

/-- Planted (law `ConcreteSpec.Core.eval_wellTyped_progress`, case `div_zero`,
subject `ConcreteSpec.Core.CExpr.eval`, mutant `ConcreteSpec.Core.divisionAsFuel`). -/
theorem eval_wellTyped_progress_planted_div_zero_divisionAsFuel : Agree [] [] ∧
    (CExpr.plain .div (.int .i8 1) (.int .i8 0)).ty? [] = some (.int .i8) ∧
    (CExpr.plain .div (.int .i8 1) (.int .i8 0)).height ≤ 2 ∧
    ¬ ((∃ v, divisionAsFuel [] 2 (.plain .div (.int .i8 1) (.int .i8 0)) = .value v ∧
        v.ty = .int .i8 ∧ v.Valid) ∨
      (∃ f, divisionAsFuel [] 2 (.plain .div (.int .i8 1) (.int .i8 0)) = .failure f ∧
        f.InExpr (.plain .div (.int .i8 1) (.int .i8 0)))) := by
  refine ⟨agree_nil, by decide, by decide, ?_⟩
  change ¬ ((∃ v, CEvalResult.outOfFuel = .value v ∧ v.ty = .int .i8 ∧ v.Valid) ∨
    (∃ f, CEvalResult.outOfFuel = .failure f ∧ f.InExpr (.plain .div (.int .i8 1) (.int .i8 0))))
  simp

/-- Instance (law `ConcreteSpec.Core.eval_total`, case `wrap`). -/
theorem eval_total_example_wrap : ∃ v, (CExpr.wrapping .add (.int .u8 255) (.int .u8 1)).eval [] 2 = .value v ∧
    v.ty = .int .u8 ∧ v.Valid :=
  eval_total [] [] agree_nil
    (.wrapping .add (.int .u8 255) (.int .u8 1)) (.int .u8) (by decide) (by decide) 2 (by decide)

/-- Planted (law `ConcreteSpec.Core.eval_total`, case `wrap`,
subject `ConcreteSpec.Core.CExpr.eval`, mutant `ConcreteSpec.Core.refuseWrappingAdd`). -/
theorem eval_total_planted_wrap_refuseWrappingAdd : Agree [] [] ∧
    (CExpr.wrapping .add (.int .u8 255) (.int .u8 1)).ty? [] = some (.int .u8) ∧
    (CExpr.wrapping .add (.int .u8 255) (.int .u8 1)).fallibleFree = true ∧
    (CExpr.wrapping .add (.int .u8 255) (.int .u8 1)).height ≤ 2 ∧
    ¬ ∃ v, refuseWrappingAdd [] 2 (.wrapping .add (.int .u8 255) (.int .u8 1)) = .value v ∧
      v.ty = .int .u8 ∧ v.Valid := by
  refine ⟨agree_nil, by decide, by decide, by decide, ?_⟩
  simp [refuseWrappingAdd]

/-- A broken declaration evaluator that returns `bool` for every declaration. -/
def boolConstValues (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U))
    (env : AList ConstId CVal) : Except ConstEvalRefusal (AList ConstId CVal) :=
  match evalConsts U fuel ds env with
  | .ok _ => .ok (ds.map (fun d => (d.id, CVal.bool true)) ++ env)
  | .error r => .error r

/-- A concrete successful division declaration. -/
def divisionDecls : List (ConstDecl ConstDemo.U) :=
  [⟨ConstDemo.cid "Q", .c (.int .i8), .plain .div (.int .i8 100) (.int .i8 7)⟩]

/-- Instance (law `ConcreteSpec.Core.evalConsts_sound`, case `division`). -/
theorem evalConsts_sound_example_division : ∃ env, evalConsts ConstDemo.U 3 divisionDecls [] = .ok env ∧
    ∀ d ∈ divisionDecls, ∃ v, AList.lookup env d.id = some v ∧
      ConstDemo.U.ofC v.ty = d.ty ∧ v.Valid :=
  evalConsts_sound ConstDemo.U 3 divisionDecls (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.evalConsts_sound`, case `division`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.boolConstValues`). -/
theorem evalConsts_sound_planted_division_boolConstValues : ConstsTyped ConstDemo.U divisionDecls ∧
    (boolConstValues ConstDemo.U 3 divisionDecls []).isOk = true ∧
    ¬ (∃ env, boolConstValues ConstDemo.U 3 divisionDecls [] = .ok env ∧
      ∀ d ∈ divisionDecls, ∃ v, AList.lookup env d.id = some v ∧
        ConstDemo.U.ofC v.ty = d.ty ∧ v.Valid) := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨env, he, h⟩
  cases he
  obtain ⟨v, hv, hty, _⟩ := h ⟨ConstDemo.cid "Q", .c (.int .i8), .plain .div (.int .i8 100) (.int .i8 7)⟩
    (by simp [divisionDecls])
  simp only [divisionDecls, List.map, List.append_nil, AList.lookup,
    ↓reduceIte, Option.some.injEq] at hv
  change ConstDemo.U.ofC v.ty = .c (.int .i8) at hty
  subst v
  cases hty

/-- A broken declaration evaluator that misclassifies an arithmetic failure as .invalid. -/
def invalidRefusalEvalConsts (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U))
    (env : AList ConstId CVal) : Except ConstEvalRefusal (AList ConstId CVal) :=
  match evalConsts U fuel ds env with
  | .error (.operation c f) => .error (.invalid c f.expression)
  | r => r

/-- Instance (law `ConcreteSpec.Core.typed_evalConsts_refusal`, case `unused_div_zero`). -/
theorem typed_evalConsts_refusal_example_unused_div_zero : (∃ env, evalConsts ConstDemo.U 2
      [⟨ConstDemo.cid "USED", .c (.int .u8), .int .u8 1⟩,
       ⟨ConstDemo.cid "UNUSED", .c (.int .i8), .plain .div (.int .i8 1) (.int .i8 0)⟩] [] = .ok env) ∨
    (∃ d ∈ [ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "USED") (.c (.int .u8)) (.int .u8 1),
            ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "UNUSED") (.c (.int .i8))
              (.plain .div (.int .i8 1) (.int .i8 0))],
      ∃ f, evalConsts ConstDemo.U 2
        [⟨ConstDemo.cid "USED", .c (.int .u8), .int .u8 1⟩,
         ⟨ConstDemo.cid "UNUSED", .c (.int .i8), .plain .div (.int .i8 1) (.int .i8 0)⟩] [] =
          .error (.operation d.id f) ∧ f.InExpr d.init) :=
  typed_evalConsts_refusal ConstDemo.U 2
    [⟨ConstDemo.cid "USED", .c (.int .u8), .int .u8 1⟩,
     ⟨ConstDemo.cid "UNUSED", .c (.int .i8), .plain .div (.int .i8 1) (.int .i8 0)⟩]
    (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.typed_evalConsts_refusal`, case `unused_div_zero`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.invalidRefusalEvalConsts`). -/
theorem typed_evalConsts_refusal_planted_unused_div_zero_invalidRefusalEvalConsts : ConstsTyped ConstDemo.U
      [⟨ConstDemo.cid "USED", .c (.int .u8), .int .u8 1⟩,
       ⟨ConstDemo.cid "UNUSED", .c (.int .i8), .plain .div (.int .i8 1) (.int .i8 0)⟩] ∧
    (∀ d ∈ [ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "USED") (.c (.int .u8)) (.int .u8 1),
            ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "UNUSED") (.c (.int .i8))
              (.plain .div (.int .i8 1) (.int .i8 0))], d.init.height ≤ 2) ∧
    ¬ ((∃ env, invalidRefusalEvalConsts ConstDemo.U 2
          [⟨ConstDemo.cid "USED", .c (.int .u8), .int .u8 1⟩,
           ⟨ConstDemo.cid "UNUSED", .c (.int .i8), .plain .div (.int .i8 1) (.int .i8 0)⟩] [] = .ok env) ∨
      (∃ d ∈ [ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "USED") (.c (.int .u8)) (.int .u8 1),
              ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "UNUSED") (.c (.int .i8))
                (.plain .div (.int .i8 1) (.int .i8 0))],
        ∃ f, invalidRefusalEvalConsts ConstDemo.U 2
          [⟨ConstDemo.cid "USED", .c (.int .u8), .int .u8 1⟩,
           ⟨ConstDemo.cid "UNUSED", .c (.int .i8), .plain .div (.int .i8 1) (.int .i8 0)⟩] [] =
            .error (.operation d.id f) ∧ f.InExpr d.init)) := by
  refine ⟨by decide, by decide, ?_⟩
  rintro (⟨env, h⟩ | ⟨d, _, f, h, _⟩) <;> cases h

/-- Mutant: replace each newly established static binding with unit, retaining the closed
admission, declared-type agreement and freshness guards. This corrupts only the binding rule. -/
def unitTypeConsts (U : ConstTypes) : List (ConstDecl U) → AList ConstId CTy →
    Option (AList ConstId CTy)
  | [], Γ => some Γ
  | d :: ds, Γ => match d.init.ty? Γ with
    | some T =>
      if ConstantType T ∧ U.ofC T = d.ty ∧ AList.lookup Γ d.id = none then
        unitTypeConsts U ds ((d.id, .unit) :: Γ)
      else none
    | none => none

/-- A broken declaration typechecker that omits the U.ofC T = d.ty check. -/
def uncheckedTypeConsts (U : ConstTypes) : List (ConstDecl U) → AList ConstId CTy →
    Option (AList ConstId CTy)
  | [], Γ => some Γ
  | d :: ds, Γ => match d.init.ty? Γ with
    | some T =>
      if ConstantType T ∧ AList.lookup Γ d.id = none then
        uncheckedTypeConsts U ds ((d.id, T) :: Γ)
      else none
    | none => none

/-- Instance (law `ConcreteSpec.Core.admitted_copy`, case `division_type`). -/
theorem admitted_copy_example_division_type : ∃ Γ env, typeConsts ConstDemo.U divisionDecls [] = some Γ ∧
    evalConsts ConstDemo.U 3 divisionDecls [] = .ok env ∧
    (∀ d ∈ divisionDecls, ∃ T, AList.lookup Γ d.id = some T ∧
      ConstDemo.U.ofC T = d.ty ∧ ConstDemo.U.isCopy d.ty = true) ∧
    (∀ d ∈ divisionDecls, ∃ v, AList.lookup env d.id = some v ∧
      ConstDemo.U.ofC v.ty = d.ty ∧ ConstDemo.U.isCopy (ConstDemo.U.ofC v.ty) = true) :=
  admitted_copy ConstDemo.U 3 divisionDecls (by decide) (by decide)

/-- Scalar success followed by a mismatched non-Copy declaration. -/
def copyAuditDecls : List (ConstDecl ConstDemo.U) :=
  [⟨ConstDemo.cid "Q", .c (.int .i8), .int .i8 1⟩,
   ⟨ConstDemo.cid "S", .text, .bool true⟩]

/-- Instance (law `ConcreteSpec.Core.typeConsts_refuses_nonCopy`, case `mixed_declarations`). -/
theorem typeConsts_refuses_nonCopy_example_mixed_declarations : typeConsts ConstDemo.U copyAuditDecls [] = none :=
  typeConsts_refuses_nonCopy ConstDemo.U copyAuditDecls
    ⟨ConstDemo.cid "S", .text, .bool true⟩ (by decide) rfl

/-- Planted (law `ConcreteSpec.Core.typeConsts_refuses_nonCopy`, case `mixed_declarations`,
subject `ConcreteSpec.Core.typeConsts`, mutant `ConcreteSpec.Core.uncheckedTypeConsts`). -/
theorem typeConsts_refuses_nonCopy_planted_mixed_declarations_uncheckedTypeConsts : ⟨ConstDemo.cid "S", .text, .bool true⟩ ∈ copyAuditDecls ∧
    ConstDemo.U.isCopy (⟨ConstDemo.cid "S", .text, .bool true⟩ : ConstDecl ConstDemo.U).ty = false ∧
    ¬ (uncheckedTypeConsts ConstDemo.U copyAuditDecls [] = none) := by
  refine ⟨by decide, rfl, ?_⟩
  decide

/-- A demand-driven declaration evaluator that skips intermediate declarations whose identity is
not referenced by any later declaration in the list. -/
def skipUnusedEvalConsts (U : ConstTypes) (fuel : Nat) :
    List (ConstDecl U) → AList ConstId CVal → Except ConstEvalRefusal (AList ConstId CVal)
  | [], env => .ok env
  | [d], env => match d.init.eval env fuel with
    | .value v => .ok ((d.id, v) :: env)
    | .failure f => .error (.operation d.id f)
    | .outOfFuel => .error (.outOfFuel d.id)
    | .invalid e => .error (.invalid d.id e)
  | d :: ds, env =>
    if ds.any (fun d' => d'.init.refs.contains d.id) then
      match d.init.eval env fuel with
      | .value v => skipUnusedEvalConsts U fuel ds ((d.id, v) :: env)
      | .failure f => .error (.operation d.id f)
      | .outOfFuel => .error (.outOfFuel d.id)
      | .invalid e => .error (.invalid d.id e)
    else
      skipUnusedEvalConsts U fuel ds env

/-- A demand-driven admission predicate that ignores evaluation failures in unreferenced
intermediate declarations. -/
def skipUnusedConstsAdmitted (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U)) : Prop :=
  ConstsTyped U ds ∧ (skipUnusedEvalConsts U fuel ds []).isOk = true

/-- Failure-swallowing expression evaluator. -/
def swallowFailure (env : AList ConstId CVal) (fuel : Nat) (e : CExpr) : CEvalResult :=
  match e.eval env fuel with
  | .failure _ => .value (.int .i8 0)
  | r => r

/-- Fabricated-failure expression evaluator. -/
def fabricateFailure (env : AList ConstId CVal) (fuel : Nat) (e : CExpr) : CEvalResult :=
  match e, e.eval env fuel with
  | .plain .div _ _, .value _ => .failure ⟨.plain .div, e, .arithmetic .divisionByZero⟩
  | _, r => r

/-- A failing closed division and its primitive diagnostic. -/
def zeroDivision : CExpr := .plain .div (.int .i8 1) (.int .i8 0)

/-- Instance (law `ConcreteSpec.Core.eval_agrees`, case `division_failure`). -/
theorem eval_agrees_example_division_failure : ConstEval [] 3 zeroDivision (zeroDivision.eval [] 3) ∧
    ∀ r, ConstEval [] 3 zeroDivision r → zeroDivision.eval [] 3 = r :=
  eval_agrees [] 3 zeroDivision

/-- Planted (law `ConcreteSpec.Core.eval_agrees`, case `division_failure`,
subject `ConcreteSpec.Core.CExpr.eval`, mutant `ConcreteSpec.Core.swallowFailure`). -/
theorem eval_agrees_planted_division_failure_swallowFailure : ¬ (ConstEval [] 3 zeroDivision (swallowFailure [] 3 zeroDivision) ∧
    ∀ r, ConstEval [] 3 zeroDivision r → swallowFailure [] 3 zeroDivision = r) := by
  intro h
  have hderiv : ConstEval [] 3 zeroDivision
      (.failure ⟨.plain .div, zeroDivision, .arithmetic .divisionByZero⟩) :=
    .plain_failure (.int_ok (by decide)) (.int_ok (by decide)) rfl
  have bad := h.2 _ hderiv
  revert bad
  decide

/-- Instance (law `ConcreteSpec.Core.eval_agrees`, case `division_success`). -/
theorem eval_agrees_example_division_success : ConstEval [] 3 (.plain .div (.int .i8 100) (.int .i8 7))
      ((.plain .div (.int .i8 100) (.int .i8 7) : CExpr).eval [] 3) ∧
    ∀ r, ConstEval [] 3 (.plain .div (.int .i8 100) (.int .i8 7)) r →
      ((.plain .div (.int .i8 100) (.int .i8 7) : CExpr).eval [] 3) = r :=
  eval_agrees [] 3 (.plain .div (.int .i8 100) (.int .i8 7))

/-- Planted (law `ConcreteSpec.Core.eval_agrees`, case `division_success`,
subject `ConcreteSpec.Core.CExpr.eval`, mutant `ConcreteSpec.Core.fabricateFailure`). -/
theorem eval_agrees_planted_division_success_fabricateFailure : ¬ (ConstEval [] 3 (.plain .div (.int .i8 100) (.int .i8 7))
      (fabricateFailure [] 3 (.plain .div (.int .i8 100) (.int .i8 7))) ∧
    ∀ r, ConstEval [] 3 (.plain .div (.int .i8 100) (.int .i8 7)) r →
      fabricateFailure [] 3 (.plain .div (.int .i8 100) (.int .i8 7)) = r) := by
  intro h
  have hderiv : ConstEval [] 3 (.plain .div (.int .i8 100) (.int .i8 7))
      (.value (.int .i8 14)) :=
    .plain_ok (.int_ok (by decide)) (.int_ok (by decide)) rfl
  have bad := h.2 _ hderiv
  revert bad
  decide

/-- A preceding successful declaration. -/
def failurePrefix : List (ConstDecl ConstDemo.U) :=
  [⟨ConstDemo.cid "P", .c .bool, .bool true⟩]

/-- The failed constant is unused by both neighbours. -/
def unusedFailureDecl : ConstDecl ConstDemo.U :=
  ⟨ConstDemo.cid "unused", .c (.int .i8), zeroDivision⟩

/-- Located primitive failure for the unused declaration. -/
def zeroFailure : ConstFailure :=
  ⟨.plain .div, zeroDivision, .arithmetic .divisionByZero⟩

/-- Instance (law `ConcreteSpec.Core.semantic_failure_refuses`, case `unused_after_prefix`). -/
theorem semantic_failure_refuses_example_unused_after_prefix : evalConsts ConstDemo.U 3 (failurePrefix ++ unusedFailureDecl :: divisionDecls) [] =
      .error (.operation unusedFailureDecl.id zeroFailure) ∧
    ¬ ConstsAdmitted ConstDemo.U 3 (failurePrefix ++ unusedFailureDecl :: divisionDecls) :=
  semantic_failure_refuses ConstDemo.U 3 failurePrefix divisionDecls unusedFailureDecl
    [(ConstDemo.cid "P", .bool true)] zeroFailure
    (.value _ [] [] (.bool true) _ (.bool 2 true) (.nil _))
    (.plain_failure (.int_ok (by decide)) (.int_ok (by decide)) rfl)

/-- Planted (law `ConcreteSpec.Core.semantic_failure_refuses`, case `unused_after_prefix`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.skipUnusedEvalConsts`).
The independent successful-prefix derivation is retained; derived admission uses the
substituted evaluator everywhere. -/
theorem semantic_failure_refuses_planted_unused_after_prefix_skipUnusedEvalConsts : ConstsRun ConstDemo.U 3 failurePrefix [] (.ok [(ConstDemo.cid "P", .bool true)]) ∧
    ConstEval [(ConstDemo.cid "P", .bool true)] 3 unusedFailureDecl.init (.failure zeroFailure) ∧
    ¬ (skipUnusedEvalConsts ConstDemo.U 3 (failurePrefix ++ unusedFailureDecl :: divisionDecls) [] =
      .error (.operation unusedFailureDecl.id zeroFailure) ∧
      ¬ skipUnusedConstsAdmitted ConstDemo.U 3 (failurePrefix ++ unusedFailureDecl :: divisionDecls)) := by
  refine ⟨.value _ [] [] (.bool true) _ (.bool 2 true) (.nil _),
    .plain_failure (.int_ok (by decide)) (.int_ok (by decide)) rfl, ?_⟩
  intro h
  exact h.2 ⟨by decide, by decide⟩

/-- A broken ordering relation that checks permutation and duplicate-free names without checking
that references appear earlier. -/
def unsortedPermOrder (U : ConstTypes) (source order : List (ConstDecl U)) : Prop :=
  order.Perm source ∧ (source.map ConstDecl.id).Nodup

/-- Instance (law `ConcreteSpec.Core.cycle_excludes_dependencyOrder`, case `two_constants`). -/
theorem cycle_excludes_dependencyOrder_example_two_constants : ¬ DependencyOrder ConstDemo.U
    [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
     ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]
    [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
     ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩] :=
  cycle_excludes_dependencyOrder ConstDemo.U
    [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
     ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]
    [ConstDemo.cid "A", ConstDemo.cid "B", ConstDemo.cid "A"]
    [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
     ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]
    ⟨ConstDemo.cid "A", [ConstDemo.cid "B"], rfl, by decide⟩

/-- Planted (law `ConcreteSpec.Core.cycle_excludes_dependencyOrder`, case `two_constants`,
subject `ConcreteSpec.Core.DependencyOrder`, mutant `ConcreteSpec.Core.unsortedPermOrder`). -/
theorem cycle_excludes_dependencyOrder_planted_two_constants_unsortedPermOrder : CycleChain ConstDemo.U
      [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]
      [ConstDemo.cid "A", ConstDemo.cid "B", ConstDemo.cid "A"] ∧
    ¬ (¬ unsortedPermOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]
      [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]) := by
  refine ⟨⟨ConstDemo.cid "A", [ConstDemo.cid "B"], rfl, by decide⟩, ?_⟩
  intro hnot
  exact hnot ⟨List.Perm.refl _, by decide⟩

/-- A broken elaborator order criterion that forbids reordering and requires source declaration
order to already satisfy `ReferencesEarlier`. -/
def sourceOrderOnly (U : ConstTypes) (source order : List (ConstDecl U)) : Prop :=
  order = source ∧ DependencyOrder U source order

/-- Instance (law `ConcreteSpec.Core.DependencySortingComplete`, case `forward_ref`). -/
theorem DependencySortingComplete_example_forward_ref : ∃ order, DependencyOrder ConstDemo.U
    [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
     ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩] order :=
  DependencySortingComplete ConstDemo.U
    [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
     ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩]
    (by decide)
    (acyclic_of_dependencyOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩]
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩]
      (by decide))

/-- Planted (law `ConcreteSpec.Core.DependencySortingComplete`, case `forward_ref`,
subject `ConcreteSpec.Core.DependencyOrder`, mutant `ConcreteSpec.Core.sourceOrderOnly`). -/
theorem DependencySortingComplete_planted_forward_ref_sourceOrderOnly : SourceNamesResolved ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩] ∧
    Acyclic ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩] ∧
    ¬ (∃ order, sourceOrderOnly ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩] order) := by
  refine ⟨by decide,
    acyclic_of_dependencyOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩]
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩]
      (by decide), ?_⟩
  rintro ⟨order, rfl, _, _, hearlier⟩
  simp [ReferencesEarlier, CExpr.refs] at hearlier

/-- Instance (law `ConcreteSpec.Core.constant_types`, case `closed_list`).
Both directions are populated for integer and Boolean carriers. -/
theorem constant_types_example_closed_list : ConstantType (.int .u8) ∧ ConstantType .bool ∧
    ¬ ConstantType .unit ∧ ¬ ConstantType .char := by decide

/-- Mutant: extend only the closed type list with unit, keeping char refused. -/
def admitUnit (T : CTy) : Prop := ConstantType T ∨ T = .unit

/-- Planted (law `ConcreteSpec.Core.constant_types`, case `unit`,
subject `ConcreteSpec.Core.ConstantType`, mutant `ConcreteSpec.Core.admitUnit`). -/
theorem constant_types_planted_unit_admitUnit : admitUnit .unit ∧ ¬ (admitUnit .unit ↔ (∃ i, CTy.unit = .int i) ∨ CTy.unit = .bool) := by
  simp [admitUnit, ConstantType]

/-- Instance (law `ConcreteSpec.Core.typed_constant_closed`, case `division`). -/
theorem typed_constant_closed_example_division : ∀ d ∈ divisionDecls, ∃ T,
    AList.lookup [(ConstDemo.cid "Q", CTy.int .i8)] d.id = some T ∧
    ConstDemo.U.ofC T = d.ty ∧ ConstantType T :=
  typed_constant_closed ConstDemo.U divisionDecls _ (by decide)

/-- Planted (law `ConcreteSpec.Core.typed_constant_closed`, case `unit_binding`,
subject `ConcreteSpec.Core.typeConsts`, mutant `ConcreteSpec.Core.unitTypeConsts`).
The static-success premise uses the mutant too; its fabricated unit binding is not admitted. -/
theorem typed_constant_closed_planted_unit_binding_unitTypeConsts : unitTypeConsts ConstDemo.U divisionDecls [] =
      some [(ConstDemo.cid "Q", CTy.unit)] ∧
    ¬ (∀ d ∈ divisionDecls, ∃ T,
      AList.lookup [(ConstDemo.cid "Q", CTy.unit)] d.id = some T ∧
      ConstDemo.U.ofC T = d.ty ∧ ConstantType T) := by
  refine ⟨by decide, ?_⟩
  intro h
  obtain ⟨T, hT, _, hc⟩ := h _ (List.Mem.head _)
  have heq : CTy.unit = T := by simpa [AList.lookup] using hT
  subst T
  exact hc

/-- Mutant: erase only the distinction between budget exhaustion and static invalidity. -/
def fuelAsInvalid (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U))
    (env : AList ConstId CVal) : Except ConstEvalRefusal (AList ConstId CVal) :=
  match evalConsts U fuel ds env with
  | .error (.outOfFuel c) => .error (.invalid c (.bool false))
  | r => r

/-- Instance (law `ConcreteSpec.Core.exhausted_refuses`, case `zero_budget`). -/
theorem exhausted_refuses_example_zero_budget : evalConsts ConstDemo.U 0 divisionDecls [] =
      .error (.outOfFuel (ConstDemo.cid "Q")) ∧
    ¬ ConstsAdmitted ConstDemo.U 0 divisionDecls :=
  exhausted_refuses ConstDemo.U 0 [] [] (divisionDecls.head (by decide)) []
    (.nil []) (.exhausted _)

/-- Planted (law `ConcreteSpec.Core.exhausted_refuses`, case `zero_budget`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.fuelAsInvalid`).
The independent empty-prefix derivation stays unchanged; the derived admission closure
uses the mutant. -/
theorem exhausted_refuses_planted_zero_budget_fuelAsInvalid : ConstsRun ConstDemo.U 0 [] [] (.ok []) ∧
    ConstEval [] 0 (divisionDecls.head (by decide)).init .outOfFuel ∧
    ¬ (fuelAsInvalid ConstDemo.U 0 divisionDecls [] =
        .error (.outOfFuel (ConstDemo.cid "Q")) ∧
      ¬ (ConstsTyped ConstDemo.U divisionDecls ∧
        (fuelAsInvalid ConstDemo.U 0 divisionDecls []).isOk = true)) := by
  refine ⟨.nil [], .exhausted _, ?_⟩
  intro h
  cases h.1

/-- A forward reference followed by two independent ready declarations. -/
def tiedSource : List (ConstDecl ConstDemo.U) :=
  [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "C")⟩,
   ⟨ConstDemo.cid "B", .c .bool, .bool true⟩,
   ⟨ConstDemo.cid "C", .c .bool, .bool false⟩]

/-- Instance: the implementation tie-break selects B before C, then C before A. -/
theorem example_implementation_tie_break_selects_B_before : (sortConsts ConstDemo.U tiedSource).map ConstDecl.id =
    [ConstDemo.cid "B", ConstDemo.cid "C", ConstDemo.cid "A"] := by decide

/-- Mutant: reverse source tie-breaking while retaining a valid dependency order. -/
def reverseTies (U : ConstTypes) (source : List (ConstDecl U)) : List (ConstDecl U) :=
  sortConsts U source.reverse

/-- Planted: both outputs satisfy the dependency contract, so a naive validity-only test
misses the reversed tie-break. The implementation's exact ordering check rejects it. -/
theorem planted_both_outputs_satisfy_dependency_contract_so : DependencyOrder ConstDemo.U tiedSource (sortConsts ConstDemo.U tiedSource) ∧
    DependencyOrder ConstDemo.U tiedSource (reverseTies ConstDemo.U tiedSource) ∧
    (reverseTies ConstDemo.U tiedSource).map ConstDecl.id ≠
      [ConstDemo.cid "B", ConstDemo.cid "C", ConstDemo.cid "A"] := by decide

/-- Instance (law `ConcreteSpec.Core.ConstOrderIndependent`, case `forward_and_ties`).
Different valid orders produce different association-list orderings but equal lookups;
syntactic environment equality would incorrectly reject this permutation. -/
theorem ConstOrderIndependent_example_forward_and_ties : (ConstsAdmitted ConstDemo.U 3 (sortConsts ConstDemo.U tiedSource) ↔
      ConstsAdmitted ConstDemo.U 3 (reverseTies ConstDemo.U tiedSource)) ∧
    ∀ env₁ env₂,
      evalConsts ConstDemo.U 3 (sortConsts ConstDemo.U tiedSource) [] = .ok env₁ →
      evalConsts ConstDemo.U 3 (reverseTies ConstDemo.U tiedSource) [] = .ok env₂ →
      ∀ c, AList.lookup env₁ c = AList.lookup env₂ c := by
  have h := ConstOrderIndependent ConstDemo.U 3 tiedSource
    (sortConsts ConstDemo.U tiedSource) (reverseTies ConstDemo.U tiedSource)
    (by decide) (by decide)
  exact ⟨h.2.2.1, h.2.2.2⟩

/-- Instance: both directions of admission are populated, and list equality is too strong. -/
theorem example_both_directions_admission_are_populated_and : ConstsAdmitted ConstDemo.U 3 (sortConsts ConstDemo.U tiedSource) ∧
    ConstsAdmitted ConstDemo.U 3 (reverseTies ConstDemo.U tiedSource) ∧
    evalConsts ConstDemo.U 3 (sortConsts ConstDemo.U tiedSource) [] ≠
      evalConsts ConstDemo.U 3 (reverseTies ConstDemo.U tiedSource) [] := by decide

/-- Planted (law `ConcreteSpec.Core.eval_agrees`, case `forward_failure`,
subject `ConcreteSpec.Core.CExpr.eval`, mutant `ConcreteSpec.Core.swallowFailure`).
Test the forward implication alone, so the reverse implication cannot mask a defect. -/
theorem eval_agrees_planted_forward_failure_swallowFailure : ¬ ConstEval [] 3 zeroDivision (swallowFailure [] 3 zeroDivision) := by
  intro h
  have bad := (eval_agrees _ _ _).2 _ h
  revert bad
  decide

/-- Planted (law `ConcreteSpec.Core.eval_agrees`, case `forward_success`,
subject `ConcreteSpec.Core.CExpr.eval`, mutant `ConcreteSpec.Core.fabricateFailure`). -/
theorem eval_agrees_planted_forward_success_fabricateFailure : ¬ ConstEval [] 3 (.plain .div (.int .i8 100) (.int .i8 7))
    (fabricateFailure [] 3 (.plain .div (.int .i8 100) (.int .i8 7))) := by
  intro h
  have bad := (eval_agrees _ _ _).2 _ h
  revert bad
  decide

/-- An unused overflowing initializer, independent of the successful neighbours. -/
def unusedOverflow : ConstDecl ConstDemo.U :=
  ⟨ConstDemo.cid "OVER", .c (.int .i8), .plain .add (.int .i8 127) (.int .i8 1)⟩

/-- Instance (law `ConcreteSpec.Core.semantic_failure_refuses`, case `unused_overflow`).
Overflow in an unused constant is a compile-time refusal, never a deferred runtime trap.
Authority: docs/decisions.md#decided-2026-11-14-constant-types. -/
theorem semantic_failure_refuses_example_unused_overflow : evalConsts ConstDemo.U 3 (failurePrefix ++ unusedOverflow :: divisionDecls) [] =
      .error (.operation unusedOverflow.id
        ⟨.plain .add, unusedOverflow.init, .arithmetic .overflow⟩) ∧
    ¬ ConstsAdmitted ConstDemo.U 3 (failurePrefix ++ unusedOverflow :: divisionDecls) :=
  semantic_failure_refuses ConstDemo.U 3 failurePrefix divisionDecls unusedOverflow
    [(ConstDemo.cid "P", .bool true)] _
    (.value _ [] [] (.bool true) _ (.bool 2 true) (.nil _))
    (.plain_failure (.int_ok (by decide)) (.int_ok (by decide)) (by decide))

/-- Planted (law `ConcreteSpec.Core.semantic_failure_refuses`, case `unused_overflow`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.skipUnusedEvalConsts`).
The independent prefix premise remains populated; derived admission uses the
substituted evaluator uniformly. -/
theorem semantic_failure_refuses_planted_unused_overflow_skipUnusedEvalConsts : ConstsRun ConstDemo.U 3 failurePrefix [] (.ok [(ConstDemo.cid "P", .bool true)]) ∧
    ConstEval [(ConstDemo.cid "P", .bool true)] 3 unusedOverflow.init
      (.failure ⟨.plain .add, unusedOverflow.init, .arithmetic .overflow⟩) ∧
    ¬ (skipUnusedEvalConsts ConstDemo.U 3 (failurePrefix ++ unusedOverflow :: divisionDecls) [] =
        .error (.operation unusedOverflow.id
          ⟨.plain .add, unusedOverflow.init, .arithmetic .overflow⟩) ∧
      ¬ skipUnusedConstsAdmitted ConstDemo.U 3 (failurePrefix ++ unusedOverflow :: divisionDecls)) := by
  refine ⟨.value _ [] [] (.bool true) _ (.bool 2 true) (.nil _),
    .plain_failure (.int_ok (by decide)) (.int_ok (by decide)) (by decide), ?_⟩
  intro h
  exact h.2 ⟨by decide, by decide⟩

/-- Mutant: remove only Boolean admission from the closed list. -/
def intOnlyConstantType (T : CTy) : Prop := ∃ i, T = .int i

/-- Planted (law `ConcreteSpec.Core.constant_types`, case `bool_reverse`,
subject `ConcreteSpec.Core.ConstantType`, mutant `ConcreteSpec.Core.intOnlyConstantType`). -/
theorem constant_types_planted_bool_reverse_intOnlyConstantType : ((∃ i, CTy.bool = .int i) ∨ CTy.bool = .bool) ∧
    ¬ intOnlyConstantType .bool := by simp [intOnlyConstantType]

/-- An asymmetric three-cycle makes reversing edges observable. -/
def diagnosticCycle : List (ConstDecl ConstDemo.U) :=
  [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
   ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "C")⟩,
   ⟨ConstDemo.cid "C", .c .bool, .ref (ConstDemo.cid "A")⟩]

/-- The populated diagnostic traverses all three source edges. -/
def diagnosticChain : List ConstId :=
  [ConstDemo.cid "A", ConstDemo.cid "B", ConstDemo.cid "C", ConstDemo.cid "A"]

/-- Instance (law `ConcreteSpec.Core.cycle_chain_real`, case `three_cycle`). -/
theorem cycle_chain_real_example_three_cycle : refChain ConstDemo.U diagnosticCycle diagnosticChain = true ∧
    ActualRefChain ConstDemo.U diagnosticCycle diagnosticChain :=
  ⟨by decide, (cycle_chain_real _ _ _).mp (by decide)⟩

/-- Mutant: accept just the empty chain in addition to the real checker. -/
def acceptEmptyChain (U : ConstTypes) (ds : List (ConstDecl U)) (cs : List ConstId) : Bool :=
  cs.isEmpty || refChain U ds cs

/-- Planted (law `ConcreteSpec.Core.cycle_chain_real`, case `empty_forward`,
subject `ConcreteSpec.Core.refChain`, mutant `ConcreteSpec.Core.acceptEmptyChain`). -/
theorem cycle_chain_real_planted_empty_forward_acceptEmptyChain : acceptEmptyChain ConstDemo.U diagnosticCycle [] = true ∧
    ¬ ActualRefChain ConstDemo.U diagnosticCycle [] := by
  exact ⟨rfl, id⟩

/-- Mutant: inspect source references backwards, preserving the nonempty-chain checks. -/
def backwardsChain (U : ConstTypes) (ds : List (ConstDecl U)) : List ConstId → Bool
  | [] => false
  | [c] => (ds.map ConstDecl.id).contains c
  | a :: b :: rest => dependsOn U ds b a && backwardsChain U ds (b :: rest)

/-- Planted (law `ConcreteSpec.Core.cycle_chain_real`, case `real_reverse`,
subject `ConcreteSpec.Core.refChain`, mutant `ConcreteSpec.Core.backwardsChain`). -/
theorem cycle_chain_real_planted_real_reverse_backwardsChain : ActualRefChain ConstDemo.U diagnosticCycle diagnosticChain ∧
    backwardsChain ConstDemo.U diagnosticCycle diagnosticChain ≠ true :=
  ⟨(cycle_chain_real _ _ _).mp (by decide), by decide⟩

/-- Planted (law `ConcreteSpec.Core.cycle_chain_real`, case `backwards_forward`,
subject `ConcreteSpec.Core.refChain`, mutant `ConcreteSpec.Core.backwardsChain`). -/
theorem cycle_chain_real_planted_backwards_forward_backwardsChain : backwardsChain ConstDemo.U diagnosticCycle diagnosticChain.reverse = true ∧
    ¬ ActualRefChain ConstDemo.U diagnosticCycle diagnosticChain.reverse := by
  refine ⟨by decide, ?_⟩
  intro h
  have := (cycle_chain_real _ _ _).mpr h
  revert this
  decide

/-- Planted (law `ConcreteSpec.Core.admitted_copy`, case `noncopy_declaration`,
subject `ConcreteSpec.Core.typeConsts`, mutant `ConcreteSpec.Core.uncheckedTypeConsts`).
Dropping only declared-type agreement admits text. The failed conclusion is Copy itself. -/
theorem admitted_copy_planted_noncopy_declaration_uncheckedTypeConsts : (uncheckedTypeConsts ConstDemo.U copyAuditDecls []).isSome = true ∧
    ConstsEvaluate ConstDemo.U 2 copyAuditDecls ∧
    ¬ (∀ d ∈ copyAuditDecls, ConstDemo.U.isCopy d.ty = true) := by decide

/-- Mutant: corrupt only a successful i8 value's range, retaining its scalar type. -/
def outOfRangeEval (env : AList ConstId CVal) (fuel : Nat) (e : CExpr) : CEvalResult :=
  match e.eval env fuel with
  | .value (.int .i8 _) => .value (.int .i8 128)
  | r => r

/-- Mutant: use the same range-corrupting scalar rule in declaration evaluation. -/
def outOfRangeConstValues (U : ConstTypes) (fuel : Nat) :
    List (ConstDecl U) → AList ConstId CVal → Except ConstEvalRefusal (AList ConstId CVal)
  | [], env => .ok env
  | d :: ds, env => match outOfRangeEval env fuel d.init with
    | .value v => outOfRangeConstValues U fuel ds ((d.id, v) :: env)
    | .failure f => .error (.operation d.id f)
    | .outOfFuel => .error (.outOfFuel d.id)
    | .invalid e => .error (.invalid d.id e)

/-- Planted (law `ConcreteSpec.Core.eval_total`, case `same_type_range`,
subject `ConcreteSpec.Core.CExpr.eval`, mutant `ConcreteSpec.Core.outOfRangeEval`). -/
theorem eval_total_planted_same_type_range_outOfRangeEval : (CExpr.int .i8 1).ty? [] = some (.int .i8) ∧
    (CExpr.int .i8 1).fallibleFree = true ∧ (CExpr.int .i8 1).height ≤ 1 ∧
    outOfRangeEval [] 1 (.int .i8 1) = .value (.int .i8 128) ∧
    (CVal.int .i8 128).ty = .int .i8 ∧ ¬ (CVal.int .i8 128).Valid := by
  unfold CVal.Valid
  decide

/-- Planted (law `ConcreteSpec.Core.eval_wellTyped_progress`, case `same_type_range`,
subject `ConcreteSpec.Core.CExpr.eval`, mutant `ConcreteSpec.Core.outOfRangeEval`). -/
theorem eval_wellTyped_progress_planted_same_type_range_outOfRangeEval : ¬ ((∃ v, outOfRangeEval [] 1 (.int .i8 1) = .value v ∧
      v.ty = .int .i8 ∧ v.Valid) ∨
    ∃ f, outOfRangeEval [] 1 (.int .i8 1) = .failure f ∧ f.InExpr (.int .i8 1)) := by
  have he : outOfRangeEval [] 1 (.int .i8 1) = .value (.int .i8 128) := by decide
  rw [he]
  simp only [CEvalResult.value.injEq, exists_eq_left', CVal.ty, true_and,
    reduceCtorEq, false_and, exists_false, or_false]
  unfold CVal.Valid
  decide

/-- Planted (law `ConcreteSpec.Core.evalConsts_sound`, case `same_type_range`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.outOfRangeConstValues`). -/
theorem evalConsts_sound_planted_same_type_range_outOfRangeConstValues : ConstsTyped ConstDemo.U divisionDecls ∧
    outOfRangeConstValues ConstDemo.U 3 divisionDecls [] =
      .ok [(ConstDemo.cid "Q", .int .i8 128)] ∧
    ¬ (∀ d ∈ divisionDecls, ∃ v,
      AList.lookup [(ConstDemo.cid "Q", CVal.int .i8 128)] d.id = some v ∧
      ConstDemo.U.ofC v.ty = d.ty ∧ v.Valid) := by
  refine ⟨by decide, by decide, ?_⟩
  intro h
  obtain ⟨v, hv, _, hr⟩ := h _ (List.Mem.head _)
  have heq : CVal.int .i8 128 = v := by simpa [divisionDecls, AList.lookup] using hv
  subst v
  exact (by unfold CVal.Valid; decide : ¬ (CVal.int .i8 128).Valid) hr

/-- Mutant: type references using only the immediately preceding declaration. All other
checks, including declared type, closed admission and duplicate names, are unchanged. -/
def adjacentTypeConsts (U : ConstTypes) : List (ConstDecl U) → AList ConstId CTy →
    Option (AList ConstId CTy)
  | [], Γ => some Γ
  | d :: ds, Γ => match d.init.ty? (Γ.take 1) with
    | some T =>
      if ConstantType T ∧ U.ofC T = d.ty ∧ AList.lookup Γ d.id = none then
        adjacentTypeConsts U ds ((d.id, T) :: Γ)
      else none
    | none => none

/-- Mutant: evaluate references using only the immediately preceding declaration.
Every initializer still runs, with unchanged fuel, operations and refusal propagation. -/
def adjacentEvalConsts (U : ConstTypes) (fuel : Nat) :
    List (ConstDecl U) → AList ConstId CVal → Except ConstEvalRefusal (AList ConstId CVal)
  | [], env => .ok env
  | d :: ds, env => match d.init.eval (env.take 1) fuel with
    | .value v => adjacentEvalConsts U fuel ds ((d.id, v) :: env)
    | .failure f => .error (.operation d.id f)
    | .outOfFuel => .error (.outOfFuel d.id)
    | .invalid e => .error (.invalid d.id e)

/-- Mutant: add only the spurious rule that true literals cannot follow A's binding. -/
def lateBoolTypeConsts (U : ConstTypes) : List (ConstDecl U) → AList ConstId CTy →
    Option (AList ConstId CTy)
  | [], Γ => some Γ
  | d :: ds, Γ =>
    if d.init = .bool true ∧ (AList.lookup Γ (ConstDemo.cid "A")).isSome then none
    else match typeConsts U [d] Γ with
      | none => none
      | some Γ' => lateBoolTypeConsts U ds Γ'

/-- Mutant: add the same spurious true-literal precondition to evaluation only. -/
def lateBoolEvalConsts (U : ConstTypes) (fuel : Nat) :
    List (ConstDecl U) → AList ConstId CVal → Except ConstEvalRefusal (AList ConstId CVal)
  | [], env => .ok env
  | d :: ds, env =>
    if d.init = .bool true ∧ (AList.lookup env (ConstDemo.cid "A")).isSome then
      .error (.invalid d.id d.init)
    else match evalConsts U fuel [d] env with
      | .error e => .error e
      | .ok env' => lateBoolEvalConsts U fuel ds env'

/-- Test substitution at the underlying rule, exactly as ConstsTyped is defined. -/
def typedWith (typeRule : List (ConstDecl ConstDemo.U) → AList ConstId CTy →
    Option (AList ConstId CTy)) (ds : List (ConstDecl ConstDemo.U)) : Prop :=
  (typeRule ds []).isSome = true

/-- Test substitution at the underlying rule, exactly as ConstsEvaluate is defined. -/
def evaluatesWith (evalRule : List (ConstDecl ConstDemo.U) → AList ConstId CVal →
    Except ConstEvalRefusal (AList ConstId CVal)) (ds : List (ConstDecl ConstDemo.U)) : Prop :=
  (evalRule ds []).isOk = true

/-- Admission always closes over the same substituted rules as typing and evaluation. -/
def admittedWith (typeRule : List (ConstDecl ConstDemo.U) → AList ConstId CTy →
    Option (AList ConstId CTy)) (evalRule : List (ConstDecl ConstDemo.U) →
    AList ConstId CVal → Except ConstEvalRefusal (AList ConstId CVal))
    (ds : List (ConstDecl ConstDemo.U)) : Prop :=
  typedWith typeRule ds ∧ evaluatesWith evalRule ds

/-- Helper (computation). Real-rule substitution is exactly the three shared judgments;
these equalities prevent the test closures from drifting from the definitions they exercise. -/
private theorem realRuleSubstitution (ds : List (ConstDecl ConstDemo.U)) (fuel : Nat) :
    (typedWith (typeConsts ConstDemo.U) ds ↔ ConstsTyped ConstDemo.U ds) ∧
    (evaluatesWith (evalConsts ConstDemo.U fuel) ds ↔ ConstsEvaluate ConstDemo.U fuel ds) ∧
    (admittedWith (typeConsts ConstDemo.U) (evalConsts ConstDemo.U fuel) ds ↔
      ConstsAdmitted ConstDemo.U fuel ds) := ⟨Iff.rfl, Iff.rfl, Iff.rfl⟩

/-- A two-edge dependency chain plus an unrelated declaration. -/
def chainSource : List (ConstDecl ConstDemo.U) :=
  tiedSource ++ [⟨ConstDemo.cid "D", .c .bool, .ref (ConstDemo.cid "A")⟩]

/-- Keep both references adjacent to their dependencies. -/
def adjacentOrder : List (ConstDecl ConstDemo.U) :=
  [chainSource[2], chainSource[0], chainSource[3], chainSource[1]]

/-- Insert B between C and A. A naive permutation/dependency-order check still passes. -/
def separatedOrder : List (ConstDecl ConstDemo.U) :=
  [chainSource[2], chainSource[1], chainSource[0], chainSource[3]]

/-- Instance (law `ConcreteSpec.Core.ConstOrderIndependent`, case `two_edge_chain`).
All three equivalences have populated premises in both directions. -/
theorem ConstOrderIndependent_example_two_edge_chain : DependencyOrder ConstDemo.U chainSource adjacentOrder ∧
    DependencyOrder ConstDemo.U chainSource separatedOrder ∧
    ConstsTyped ConstDemo.U adjacentOrder ∧ ConstsTyped ConstDemo.U separatedOrder ∧
    ConstsEvaluate ConstDemo.U 3 adjacentOrder ∧ ConstsEvaluate ConstDemo.U 3 separatedOrder ∧
    ConstsAdmitted ConstDemo.U 3 adjacentOrder ∧ ConstsAdmitted ConstDemo.U 3 separatedOrder := by
  decide

/-- Planted (law `ConcreteSpec.Core.ConstOrderIndependent`, case `typing_forward`,
subject `ConcreteSpec.Core.typeConsts`, mutant `ConcreteSpec.Core.adjacentTypeConsts`).
Both orders satisfy the full dependency contract. The unchanged rule succeeds in both;
the changed rule and admission reject only the destination of this implication. -/
theorem ConstOrderIndependent_planted_typing_forward_adjacentTypeConsts : DependencyOrder ConstDemo.U chainSource adjacentOrder ∧
    DependencyOrder ConstDemo.U chainSource separatedOrder ∧
    evaluatesWith (evalConsts ConstDemo.U 3) adjacentOrder ∧
    evaluatesWith (evalConsts ConstDemo.U 3) separatedOrder ∧
    ¬ (typedWith (adjacentTypeConsts ConstDemo.U) adjacentOrder →
       typedWith (adjacentTypeConsts ConstDemo.U) separatedOrder) ∧
    ¬ (admittedWith (adjacentTypeConsts ConstDemo.U) (evalConsts ConstDemo.U 3) adjacentOrder →
       admittedWith (adjacentTypeConsts ConstDemo.U) (evalConsts ConstDemo.U 3) separatedOrder) := by
  unfold admittedWith typedWith evaluatesWith
  decide

/-- Planted (law `ConcreteSpec.Core.ConstOrderIndependent`, case `typing_reverse`,
subject `ConcreteSpec.Core.typeConsts`, mutant `ConcreteSpec.Core.lateBoolTypeConsts`).
Both orders satisfy the full dependency contract. The unchanged rule succeeds in both;
the changed rule and admission reject only the destination of this implication. -/
theorem ConstOrderIndependent_planted_typing_reverse_lateBoolTypeConsts : DependencyOrder ConstDemo.U chainSource adjacentOrder ∧
    DependencyOrder ConstDemo.U chainSource separatedOrder ∧
    evaluatesWith (evalConsts ConstDemo.U 3) adjacentOrder ∧
    evaluatesWith (evalConsts ConstDemo.U 3) separatedOrder ∧
    ¬ (typedWith (lateBoolTypeConsts ConstDemo.U) separatedOrder →
       typedWith (lateBoolTypeConsts ConstDemo.U) adjacentOrder) ∧
    ¬ (admittedWith (lateBoolTypeConsts ConstDemo.U) (evalConsts ConstDemo.U 3) separatedOrder →
       admittedWith (lateBoolTypeConsts ConstDemo.U) (evalConsts ConstDemo.U 3) adjacentOrder) := by
  unfold admittedWith typedWith evaluatesWith
  decide

/-- Planted (law `ConcreteSpec.Core.ConstOrderIndependent`, case `evaluation_forward`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.adjacentEvalConsts`).
Both orders satisfy the full dependency contract. The unchanged rule succeeds in both;
the changed rule and admission reject only the destination of this implication. -/
theorem ConstOrderIndependent_planted_evaluation_forward_adjacentEvalConsts : DependencyOrder ConstDemo.U chainSource adjacentOrder ∧
    DependencyOrder ConstDemo.U chainSource separatedOrder ∧
    typedWith (typeConsts ConstDemo.U) adjacentOrder ∧
    typedWith (typeConsts ConstDemo.U) separatedOrder ∧
    ¬ (evaluatesWith (adjacentEvalConsts ConstDemo.U 3) adjacentOrder →
       evaluatesWith (adjacentEvalConsts ConstDemo.U 3) separatedOrder) ∧
    ¬ (admittedWith (typeConsts ConstDemo.U) (adjacentEvalConsts ConstDemo.U 3) adjacentOrder →
       admittedWith (typeConsts ConstDemo.U) (adjacentEvalConsts ConstDemo.U 3) separatedOrder) := by
  unfold admittedWith typedWith evaluatesWith
  decide

/-- Planted (law `ConcreteSpec.Core.ConstOrderIndependent`, case `evaluation_reverse`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.lateBoolEvalConsts`).
Both orders satisfy the full dependency contract. The unchanged rule succeeds in both;
the changed rule and admission reject only the destination of this implication. -/
theorem ConstOrderIndependent_planted_evaluation_reverse_lateBoolEvalConsts : DependencyOrder ConstDemo.U chainSource adjacentOrder ∧
    DependencyOrder ConstDemo.U chainSource separatedOrder ∧
    typedWith (typeConsts ConstDemo.U) adjacentOrder ∧
    typedWith (typeConsts ConstDemo.U) separatedOrder ∧
    ¬ (evaluatesWith (lateBoolEvalConsts ConstDemo.U 3) separatedOrder →
       evaluatesWith (lateBoolEvalConsts ConstDemo.U 3) adjacentOrder) ∧
    ¬ (admittedWith (typeConsts ConstDemo.U) (lateBoolEvalConsts ConstDemo.U 3) separatedOrder →
       admittedWith (typeConsts ConstDemo.U) (lateBoolEvalConsts ConstDemo.U 3) adjacentOrder) := by
  unfold admittedWith typedWith evaluatesWith
  decide

/-- A legal embedding in which the unadmitted unit carrier is not Copy. -/
def copyBoundaryU : ConstTypes where
  Ty := CTy
  deq := inferInstance
  isCopy := fun T => decide (ConstantType T)
  ofC := id
  ofC_inj := fun _ _ h => h
  ofC_copy := fun _ h => by simpa using h

/-- A scalar declaration in the Copy-auditing universe. -/
def copyBoundaryDecls : List (ConstDecl copyBoundaryU) :=
  [⟨ConstDemo.cid "Q", .int .i8, .int .i8 1⟩]

/-- Mutant: replace successful values by unit, retaining failures. -/
def unitConstValues (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U))
    (env : AList ConstId CVal) : Except ConstEvalRefusal (AList ConstId CVal) :=
  match evalConsts U fuel ds env with
  | .ok vs => .ok (List.map (fun cv => (cv.1, CVal.unit)) vs)
  | .error e => .error e

/-- Planted (law `ConcreteSpec.Core.admitted_copy`, case `noncopy_value`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.unitConstValues`).
The declared type remains Copy; the evaluated value's Copy conclusion itself fails. -/
theorem admitted_copy_planted_noncopy_value_unitConstValues : ConstsTyped copyBoundaryU copyBoundaryDecls ∧
    unitConstValues copyBoundaryU 1 copyBoundaryDecls [] =
      .ok [(ConstDemo.cid "Q", .unit)] ∧
    copyBoundaryU.isCopy (.int .i8) = true ∧
    copyBoundaryU.isCopy (copyBoundaryU.ofC CVal.unit.ty) ≠ true := by decide

/-- Mutant: invert only the first successful Boolean binding, then evaluate the remaining
initializers normally. This breaks binding-value preservation, not initializer demand. -/
def invertFirstBinding (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U))
    (env : AList ConstId CVal) : Except ConstEvalRefusal (AList ConstId CVal) :=
  match ds with
  | [] => .ok env
  | d :: rest => match evalConsts U fuel [d] env with
    | .ok ((c, .bool b) :: vs) => evalConsts U fuel rest ((c, .bool (!b)) :: vs)
    | .ok vs => evalConsts U fuel rest vs
    | .error e => .error e

/-- Planted (law `ConcreteSpec.Core.ConstOrderIndependent`, case `binding_value`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.invertFirstBinding`).
Both valid orders type, evaluate and admit under uniform substitution. Only the value
agreement conclusion fails: C is inverted exactly when it is the first binding. -/
theorem ConstOrderIndependent_planted_binding_value_invertFirstBinding : DependencyOrder ConstDemo.U tiedSource (sortConsts ConstDemo.U tiedSource) ∧
    DependencyOrder ConstDemo.U tiedSource (reverseTies ConstDemo.U tiedSource) ∧
    admittedWith (typeConsts ConstDemo.U) (invertFirstBinding ConstDemo.U 3)
      (sortConsts ConstDemo.U tiedSource) ∧
    admittedWith (typeConsts ConstDemo.U) (invertFirstBinding ConstDemo.U 3)
      (reverseTies ConstDemo.U tiedSource) ∧
    ∃ env₁ env₂,
      invertFirstBinding ConstDemo.U 3 (sortConsts ConstDemo.U tiedSource) [] = .ok env₁ ∧
      invertFirstBinding ConstDemo.U 3 (reverseTies ConstDemo.U tiedSource) [] = .ok env₂ ∧
      AList.lookup env₁ (ConstDemo.cid "C") ≠ AList.lookup env₂ (ConstDemo.cid "C") := by
  refine ⟨by decide, by decide, ?_, ?_, ?_⟩
  · unfold admittedWith typedWith evaluatesWith; decide
  · unfold admittedWith typedWith evaluatesWith; decide
  · exact ⟨_, _, rfl, rfl, by decide⟩

end ConcreteSpec.Core
