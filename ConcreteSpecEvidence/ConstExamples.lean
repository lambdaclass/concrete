import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Const

namespace ConcreteSpec.Core

namespace ConstDemo

inductive T where
  | c (t : CTy)
  | text
  deriving DecidableEq, Repr

def U : ConstTypes where
  Ty := T
  deq := inferInstance
  isCopy := fun
    | .c _ => true
    | .text => false
  ofC := .c
  ofC_inj := fun _ _ h => by cases h; rfl
  ofC_copy := fun _ _ => rfl

def cid (n : String) : ConstId := ConstId.mk (ModuleId.mk ⟨⟨"const"⟩, ⟨⟨"0"⟩⟩⟩ [⟨"m"⟩] (by decide)) ⟨n⟩

/-- Instance: accepted (rule `ConcreteSpec.Core.ConstsAdmitted`): wrapping, bitwise and lossless operations
from the old fragment still compute their values. -/
theorem example_accepted_rule_ConcreteSpec_Core_ConstsAdmitted_wrapping : ConstsAdmitted U 20 [⟨cid "MASK", .c (.int .u8), .int .u8 0x0f⟩,
    ⟨cid "HIGH", .c (.int .u8), .wrapping .mul (.ref (cid "MASK")) (.int .u8 16)⟩,
    ⟨cid "BOTH", .c (.int .u8), .bit .or (.ref (cid "MASK")) (.ref (cid "HIGH"))⟩,
    ⟨cid "WIDE", .c (.int .u16), .convert .u16 (.ref (cid "BOTH"))⟩] := by decide

/-- Instance: accepted (rule `ConcreteSpec.Core.ConstsAdmitted`): plain division succeeds as `100 / 7 = 14`,
and an unused successful constant is evaluated and admitted too. -/
theorem example_accepted_rule_ConcreteSpec_Core_ConstsAdmitted_plain : ConstsAdmitted U 20 [⟨cid "QUOT", .c (.int .i16), .plain .div (.int .i16 100) (.int .i16 7)⟩,
    ⟨cid "UNUSED", .c (.int .u8), .plain .add (.int .u8 2) (.int .u8 3)⟩] := by decide

/-- Instance: The accepted division and unused constant have these values. -/
theorem example_accepted_division_and_unused_constant_have : (evalConsts U 20 [⟨cid "QUOT", .c (.int .i16),
      .plain .div (.int .i16 100) (.int .i16 7)⟩,
    ⟨cid "UNUSED", .c (.int .u8), .plain .add (.int .u8 2) (.int .u8 3)⟩] []).toOption.bind
      (fun env => AList.lookup env (cid "QUOT")) = some (.int .i16 14) := by decide

/-- Instance: accepted (rule `ConcreteSpec.Core.ConstsAdmitted`): fallible negation succeeds. -/
theorem example_accepted_rule_ConcreteSpec_Core_ConstsAdmitted_fallible : ConstsAdmitted U 20 [⟨cid "M", .c (.int .i8), .plainNeg (.int .i8 5)⟩] := by decide

/-- Instance: refused (rule `ConcreteSpec.Core.ConstsAdmitted`): `1 / 0` reports division by zero at the
plain-division sub-expression; it is not a trap. -/
theorem example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_reports : ¬ ConstsAdmitted U 20 [⟨cid "BAD", .c (.int .i8),
    .plain .div (.int .i8 1) (.int .i8 0)⟩] := by decide

/-- Instance: The division refusal names both the constant and the operation. -/
theorem example_division_refusal_names_both_constant_and : evalConsts U 20 [⟨cid "BAD", .c (.int .i8),
    .plain .div (.int .i8 1) (.int .i8 0)⟩] [] =
    .error (.operation (cid "BAD") ⟨.plain .div,
      .plain .div (.int .i8 1) (.int .i8 0), .arithmetic .divisionByZero⟩) := by decide

/-- Instance: refused (rule `ConcreteSpec.Core.ConstsAdmitted`): signed `i8` addition `127 + 1` overflows. -/
theorem example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_signed : ¬ ConstsAdmitted U 20 [⟨cid "BAD", .c (.int .i8),
    .plain .add (.int .i8 127) (.int .i8 1)⟩] := by decide

/-- Instance: refused (rule `ConcreteSpec.Core.ConstsAdmitted`): a shift by the width is out of range. -/
theorem example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_shift : ¬ ConstsAdmitted U 20 [⟨cid "BAD", .c (.int .u8),
    .plain .shl (.int .u8 1) (.int .u8 8)⟩] := by decide

/-- Instance: refused (rule `ConcreteSpec.Core.ConstsAdmitted`): an UNUSED constant is evaluated and its
failure refuses the module although no other constant refers to it. -/
theorem example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_UNUSED : ¬ ConstsAdmitted U 20 [⟨cid "USED", .c (.int .u8), .int .u8 1⟩,
    ⟨cid "UNUSED", .c (.int .i8), .plain .add (.int .i8 127) (.int .i8 1)⟩] := by decide

/-- Instance: refused (rule `ConcreteSpec.Core.ConstsAdmitted`): a finite initializer exhausts an explicitly
too-small fuel bound. -/
theorem example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_finite : ¬ ConstsAdmitted U 1 [⟨cid "DEEP", .c (.int .u8),
    .plain .add (.plain .add (.int .u8 1) (.int .u8 2)) (.int .u8 3)⟩] := by decide

/-- Instance: The same finite initializer succeeds with adequate fuel. -/
theorem example_same_finite_initializer_succeeds_adequate_fuel : ConstsAdmitted U 5 [⟨cid "DEEP", .c (.int .u8),
    .plain .add (.plain .add (.int .u8 1) (.int .u8 2)) (.int .u8 3)⟩] := by decide

/-- Instance: refused (rule `ConcreteSpec.Core.ConstsAdmitted`): a non-`Copy` declared type (`text`) lies
outside the `Copy` scalar constant universe (`U.ofC_copy`), so no constant initializer can inhabit
it. -/
theorem example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_non : U.isCopy .text = false ∧
    ¬ ConstsAdmitted U 20 [⟨cid "S", .text, .bool true⟩] := by
  exact ⟨rfl, by decide⟩

/-- Instance: refused: a reference later in an already supplied Core dependency order is invalid. -/
theorem example_refused_reference_later_in_already_supplied : ¬ ConstsAdmitted U 20 [⟨cid "A", .c (.int .u8), .ref (cid "B")⟩,
    ⟨cid "B", .c (.int .u8), .int .u8 1⟩] := by decide

/-- Instance: refused: a literal out of range, and operands of different types. -/
theorem example_refused_literal_out_range_and_operands : ¬ ConstsAdmitted U 20 [⟨cid "A", .c (.int .u8), .int .u8 256⟩] := by decide

/-- Instance: mixed integer widths are refused. -/
theorem example_mixed_integer_widths_are_refused : ¬ ConstsAdmitted U 20 [⟨cid "A", .c (.int .u16),
    .wrapping .add (.int .u8 1) (.int .u16 1)⟩] := by decide

/-- Instance: refused: one name declared twice. -/
theorem example_refused_one_name_declared_twice : ¬ ConstsAdmitted U 20 [⟨cid "A", .c .bool, .bool true⟩,
    ⟨cid "A", .c .bool, .bool false⟩] := by decide

/-- Instance: accepted (rule `ConcreteSpec.Core.ConstsAdmitted`): source `A = B + 1` before `B = 2` is sorted
to `[B, A]` by `sortConsts`; this is item 19's forward reference. -/
theorem example_accepted_rule_ConcreteSpec_Core_ConstsAdmitted_source : DependencyOrder U [⟨cid "A", .c (.int .u8),
      .plain .add (.ref (cid "B")) (.int .u8 1)⟩,
    ⟨cid "B", .c (.int .u8), .int .u8 2⟩]
    (sortConsts U [⟨cid "A", .c (.int .u8),
      .plain .add (.ref (cid "B")) (.int .u8 1)⟩,
    ⟨cid "B", .c (.int .u8), .int .u8 2⟩]) ∧
    ConstsAdmitted U 20 (sortConsts U [⟨cid "A", .c (.int .u8),
      .plain .add (.ref (cid "B")) (.int .u8 1)⟩,
    ⟨cid "B", .c (.int .u8), .int .u8 2⟩]) := by
  refine ⟨⟨List.Perm.swap _ _ [], by decide, ?_⟩, by decide⟩
  simp [sortConsts, sortConstsAux, takeReady, readyDecl, ReferencesEarlier, CExpr.refs, cid]

/-- Instance: refused (rule `ConcreteSpec.Core.ConstsAdmitted`): elaboration reports the real two-constant
cycle `A → B → A`, and neither declaration order is admissible. -/
theorem example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_elaboration : CycleChain U [⟨cid "A", .c (.int .u8), .ref (cid "B")⟩,
    ⟨cid "B", .c (.int .u8), .ref (cid "A")⟩] [cid "A", cid "B", cid "A"] := by
  exact ⟨cid "A", [cid "B"], rfl, by decide⟩

/-- Instance: refused (rule `ConcreteSpec.Core.ConstsAdmitted`): self-reference reports `A → A`. -/
theorem example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_self : CycleChain U [⟨cid "A", .c (.int .u8), .ref (cid "A")⟩]
    [cid "A", cid "A"] := by
  exact ⟨cid "A", [], rfl, by decide⟩

/-- Instance: A malformed diagnostic: the claimed chain `A → B → A` is
not real when `A` has no reference to `B`, so it cannot justify cycle refusal. -/
theorem example_malformed_diagnostic_claimed_chain_B_is : ¬ CycleChain U [⟨cid "A", .c (.int .u8), .int .u8 1⟩,
    ⟨cid "B", .c (.int .u8), .ref (cid "A")⟩] [cid "A", cid "B", cid "A"] := by
  intro h
  obtain ⟨c, rest, _, href⟩ := h
  have : refChain U [⟨cid "A", .c (.int .u8), .int .u8 1⟩,
      ⟨cid "B", .c (.int .u8), .ref (cid "A")⟩] [cid "A", cid "B", cid "A"] = false := by decide
  rw [this] at href
  cases href

/-- Instance: accepted (rule `ConcreteSpec.Core.ConstsAdmitted`): the sorted forward reference computes
`A = 3`, rather than merely accepting a sorting certificate. -/
theorem example_accepted_rule_ConcreteSpec_Core_ConstsAdmitted_sorted : (evalConsts U 3
    [⟨cid "B", .c (.int .u8), .int .u8 2⟩,
     ⟨cid "A", .c (.int .u8), .plain .add (.ref (cid "B")) (.int .u8 1)⟩] []).toOption.bind
    (fun env => AList.lookup env (cid "A")) = some (.int .u8 3) := by decide

/-- Instance: refused (rule `ConcreteSpec.Core.ConstsAdmitted`): neither order of a two-constant cycle
passes the independent static and evaluation checks. -/
theorem example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_neither : ¬ ConstsAdmitted U 3
    [⟨cid "A", .c .bool, .ref (cid "B")⟩, ⟨cid "B", .c .bool, .ref (cid "A")⟩] ∧
    ¬ ConstsAdmitted U 3
    [⟨cid "B", .c .bool, .ref (cid "A")⟩, ⟨cid "A", .c .bool, .ref (cid "B")⟩] := by decide

/-- Instance: refused (rule `ConcreteSpec.Core.ConstsAdmitted`): self-reference has no Core value. -/
theorem example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_self_2 : ¬ ConstsAdmitted U 3 [⟨cid "A", .c .bool, .ref (cid "A")⟩] := by decide

/-- Instance: Self-reference also fails the dependency-order contract, even for a singleton list. -/
theorem example_Self_reference_also_fails_dependency_order : ¬ DependencyOrder U [⟨cid "A", .c .bool, .ref (cid "A")⟩]
    [⟨cid "A", .c .bool, .ref (cid "A")⟩] := by
  intro h
  have hs := h.2.2
  simp [ReferencesEarlier, CExpr.refs] at hs

/-- Instance: Fuel refusal carries the identity of the constant, including an unused one. -/
theorem example_Fuel_refusal_carries_identity_constant_including : evalConsts U 1 [⟨cid "UNUSED", .c (.int .u8),
    .plain .add (.int .u8 1) (.int .u8 2)⟩] [] = .error (.outOfFuel (cid "UNUSED")) := by decide

/-- Instance: An unresolved reference is not a dependency-order certificate, even for an acyclic singleton. -/
theorem example_unresolved_reference_is_not_dependency_order : ¬ DependencyOrder U [⟨cid "A", .c .bool, .ref (cid "MISSING")⟩]
    [⟨cid "A", .c .bool, .ref (cid "MISSING")⟩] := by
  intro h
  have hs := h.2.2
  simp [ReferencesEarlier, CExpr.refs] at hs

/-- Instance: The evaluation check is independent: this list types but division by zero refuses it. -/
theorem example_evaluation_check_is_independent_this_list : ConstsTyped U [⟨cid "BAD", .c (.int .i8),
    .plain .div (.int .i8 1) (.int .i8 0)⟩] ∧
    ¬ ConstsEvaluate U 3 [⟨cid "BAD", .c (.int .i8),
    .plain .div (.int .i8 1) (.int .i8 0)⟩] := by
  exact ⟨by decide, by decide⟩

/-- Instance: the closed admission list accepts integer and bool, independently of
expression typing; adding unit or char to Scalar cannot silently extend constants. -/
theorem example_closed_admission_list_accepts_integer_and : ConstantType (.int .u32) ∧ ConstantType .bool ∧
    ¬ ConstantType .unit ∧ ¬ ConstantType .char := by decide

/-- Instance: a unit-typed declaration is refused even in a populated environment with a
matching unit-typed reference. Thus this tests closed admission, not a missing reference
or a declared/initializer type mismatch. -/
theorem example_unit_typed_declaration_is_refused_even : typeConsts U [⟨cid "UNIT", .c .unit, .ref (cid "SEED")⟩]
    [(cid "SEED", .unit)] = none := by decide

/-- Instance: a char-typed declaration is refused with the same matched-type setup.
No char literal or construction policy is chosen. -/
theorem example_char_typed_declaration_is_refused_same : typeConsts U [⟨cid "CHAR", .c .char, .ref (cid "SEED")⟩]
    [(cid "SEED", .char)] = none := by decide

/-- Instance: the analogous populated bool environment admits the reference. -/
theorem example_analogous_populated_bool_environment_admits_reference : typeConsts U [⟨cid "BOOL", .c .bool, .ref (cid "SEED")⟩]
    [(cid "SEED", .bool)] = some [(cid "BOOL", .bool), (cid "SEED", .bool)] := by decide

/-- Instance: a whole unit declaration has a well-typed, successfully evaluating
initializer in a matched seed environment, yet declaration admission refuses it.
The closed grammar has no unit literal, so the seed is supplied rather than invented. -/
theorem example_whole_unit_declaration_has_well_typed : (CExpr.ref (cid "SEED")).ty? [(cid "SEED", .unit)] = some .unit ∧
    (evalConsts U 3 [⟨cid "UNIT", .c .unit, .ref (cid "SEED")⟩]
      [(cid "SEED", .unit)]).isOk = true ∧
    typeConsts U [⟨cid "UNIT", .c .unit, .ref (cid "SEED")⟩]
      [(cid "SEED", .unit)] = none := by decide

/-- Instance: the corresponding char declaration also isolates ConstantType admission;
the environment supplies a scalar value without adding character literal syntax. -/
theorem example_corresponding_char_declaration_also_isolates_ConstantType : (CExpr.ref (cid "SEED")).ty? [(cid "SEED", .char)] = some .char ∧
    (evalConsts U 3 [⟨cid "CHAR", .c .char, .ref (cid "SEED")⟩]
      [(cid "SEED", .char 'A')]).isOk = true ∧
    typeConsts U [⟨cid "CHAR", .c .char, .ref (cid "SEED")⟩]
      [(cid "SEED", .char)] = none := by decide

/-- Instance: bool-to-integer conversion is held (ISS-0069); this otherwise well-typed
bool expression is not an integer conversion operand. -/
theorem example_bool_integer_conversion_is_held_ISS : (CExpr.convert .u8 (.not (.bool false))).ty? [] = none ∧
    (CExpr.convert .u8 (.not (.bool false))).eval [] 3 =
      .invalid (CExpr.convert .u8 (.not (.bool false))) := by decide

/-- A source prefix leads into a cycle nested inside arithmetic. A self-edge-only check,
or a check limited to cycles through the first declaration, would miss it. -/
def nestedCycle : List (ConstDecl U) :=
  [⟨cid "ENTRY", .c (.int .u8), .ref (cid "B")⟩,
   ⟨cid "B", .c (.int .u8), .plain .add (.int .u8 1) (.ref (cid "C"))⟩,
   ⟨cid "C", .c (.int .u8), .ref (cid "D")⟩,
   ⟨cid "D", .c (.int .u8), .ref (cid "B")⟩]

/-- Instance: the diagnostic names the actual nested three-edge cycle. -/
theorem example_diagnostic_names_actual_nested_three_edge : SourceNamesResolved U nestedCycle ∧
    CycleChain U nestedCycle [cid "B", cid "C", cid "D", cid "B"] := by
  refine ⟨by decide, cid "B", [cid "C", cid "D"], rfl, ?_⟩
  decide

/-- Instance: exhausted fuel and overflow are observably different outcomes for the same
well-typed arithmetic; neither can be mistaken for a successful value. -/
theorem example_exhausted_fuel_and_overflow_are_observably : evalConsts U 1 [⟨cid "BAD", .c (.int .i8),
      .plain .add (.int .i8 127) (.int .i8 1)⟩] [] =
    .error (.outOfFuel (cid "BAD")) ∧
    evalConsts U 2 [⟨cid "BAD", .c (.int .i8),
      .plain .add (.int .i8 127) (.int .i8 1)⟩] [] =
    .error (.operation (cid "BAD") ⟨.plain .add,
      .plain .add (.int .i8 127) (.int .i8 1), .arithmetic .overflow⟩) := by decide

/-- Instance: accepted endpoints adjacent to the signed and unsigned overflow refusals,
the shift-width refusal and signed negation overflow. Exact values pin inclusive bounds. -/
theorem example_accepted_endpoints_adjacent_signed_and_unsigned :
    (CExpr.plain .add (.int .i8 126) (.int .i8 1)).eval [] 3 = .value (.int .i8 127) ∧
    (CExpr.plain .add (.int .u8 254) (.int .u8 1)).eval [] 3 = .value (.int .u8 255) ∧
    (CExpr.plain .shl (.int .u8 1) (.int .u8 7)).eval [] 3 = .value (.int .u8 128) ∧
    (CExpr.plainNeg (.int .i8 (-127))).eval [] 3 = .value (.int .i8 127) ∧
    (CExpr.plainNeg (.int .u8 0)).eval [] 3 = .value (.int .u8 0) ∧
    (CExpr.plain .div (.int .i8 (-127)) (.int .i8 (-1))).eval [] 3 =
      .value (.int .i8 127) := by decide

/-- Instance: all accepted arithmetic endpoints also pass whole-declaration admission. -/
theorem example_all_accepted_arithmetic_endpoints_also_pass : ConstsAdmitted U 3
    [⟨cid "IADD", .c (.int .i8), .plain .add (.int .i8 126) (.int .i8 1)⟩,
     ⟨cid "UADD", .c (.int .u8), .plain .add (.int .u8 254) (.int .u8 1)⟩,
     ⟨cid "SHIFT", .c (.int .u8), .plain .shl (.int .u8 1) (.int .u8 7)⟩,
     ⟨cid "INEG", .c (.int .i8), .plainNeg (.int .i8 (-127))⟩,
     ⟨cid "UNEG", .c (.int .u8), .plainNeg (.int .u8 0)⟩,
     ⟨cid "IDIV", .c (.int .i8), .plain .div (.int .i8 (-127)) (.int .i8 (-1))⟩] := by decide

/-- Instance: signed minimum negation is a well-typed operation overflow, not invalid syntax. -/
theorem example_signed_minimum_negation_is_well_typed : evalConsts U 3 [⟨cid "BAD", .c (.int .i8), .plainNeg (.int .i8 (-128))⟩] [] =
    .error (.operation (cid "BAD") ⟨.plainNeg, .plainNeg (.int .i8 (-128)),
      .arithmetic .overflow⟩) ∧
    ¬ ConstsAdmitted U 3 [⟨cid "BAD", .c (.int .i8), .plainNeg (.int .i8 (-128))⟩] := by decide

/-- Instance: even the smallest nonzero unsigned negation overflows. -/
theorem example_even_smallest_nonzero_unsigned_negation_overflows : evalConsts U 3 [⟨cid "BAD", .c (.int .u8), .plainNeg (.int .u8 1)⟩] [] =
    .error (.operation (cid "BAD") ⟨.plainNeg, .plainNeg (.int .u8 1),
      .arithmetic .overflow⟩) ∧
    ¬ ConstsAdmitted U 3 [⟨cid "BAD", .c (.int .u8), .plainNeg (.int .u8 1)⟩] := by decide

/-- Instance: signed minimum divided by minus one overflows despite a nonzero divisor. -/
theorem example_signed_minimum_divided_minus_one_overflows : evalConsts U 3 [⟨cid "BAD", .c (.int .i8),
      .plain .div (.int .i8 (-128)) (.int .i8 (-1))⟩] [] =
    .error (.operation (cid "BAD") ⟨.plain .div,
      .plain .div (.int .i8 (-128)) (.int .i8 (-1)), .arithmetic .overflow⟩) ∧
    ¬ ConstsAdmitted U 3 [⟨cid "BAD", .c (.int .i8),
      .plain .div (.int .i8 (-128)) (.int .i8 (-1))⟩] := by decide

/-- Instance: sorting preserves the blocked cyclic suffix, which both independent
admission checks must refuse. A real cycle diagnostic alone is insufficient evidence. -/
theorem example_sorting_preserves_blocked_cyclic_suffix_which : ¬ DependencyOrder U nestedCycle (sortConsts U nestedCycle) ∧
    ¬ ConstsAdmitted U 10 (sortConsts U nestedCycle) := by
  constructor
  · intro h
    have hs := h.2.2
    simp [nestedCycle, sortConsts, sortConstsAux, takeReady, readyDecl,
      ReferencesEarlier, CExpr.refs, cid] at hs
  · decide

/-- Three forward-linked declarations and two simultaneously ready declarations.
The tie is broken by source order, including when A becomes ready ahead of TIE. -/
def forwardChain : List (ConstDecl U) :=
  [⟨cid "A", .c (.int .u8), .ref (cid "B")⟩,
   ⟨cid "B", .c (.int .u8), .ref (cid "C")⟩,
   ⟨cid "C", .c (.int .u8), .int .u8 7⟩,
   ⟨cid "TIE", .c (.int .u8), .int .u8 9⟩]

/-- Instance: exact output distinguishes stable ready selection from list reversal. -/
theorem example_exact_output_distinguishes_stable_ready_selection : (sortConsts U forwardChain).map ConstDecl.id =
    [cid "C", cid "B", cid "A", cid "TIE"] ∧
    sortConsts U forwardChain ≠ forwardChain.reverse ∧
    ConstsAdmitted U 10 (sortConsts U forwardChain) := by decide

/-- Instance: the longer sorted chain satisfies the full dependency-order contract. -/
theorem example_longer_sorted_chain_satisfies_full_dependency : DependencyOrder U forwardChain (sortConsts U forwardChain) := by
  refine ⟨?_, by decide, ?_⟩
  · decide
  · decide

end ConstDemo

end ConcreteSpec.Core
