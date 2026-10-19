import Concrete.Proof.Proof

/-!
# Test-program fixtures — real specifications and theorems (bug 079)

The programs under `tests/programs/` that exercise proof-link plumbing (spec identity, the registry,
obligations, traceability, diff and snapshot) linked theorems and specifications that did not exist.
They read `proved` only because the fingerprint was the whole check. A claim is now proved only when
its body and parameters ARE a specification its theorem evaluates, so the plumbing needs real ones.

The specification names are the ones those programs already declare (`add_commutativity`,
`PureAdd.spec_add`, …), so their `#[spec]` attributes are unchanged. The theorems they name live here
under `Examples.TestFixtures.Proofs` rather than the compiler's namespace (check_proof_namespace.sh),
except `PureAdd.add_comm`, which already names its own namespace.
-/

open Concrete.Proof

/-- `a + b`. -/
def add_commutativity : PExpr := .binOp .add (.var "a") (.var "b")
/-- `a * b`. -/
def mul_associativity : PExpr := .binOp .mul (.var "a") (.var "b")
/-- `a - b`. -/
def sub_identity : PExpr := .binOp .sub (.var "a") (.var "b")
/-- `a + b`, in a module named `left`. -/
def left_add_spec : PExpr := .binOp .add (.var "a") (.var "b")
/-- `a + b`, in a module named `right`. -/
def right_add_spec : PExpr := .binOp .add (.var "a") (.var "b")


namespace PureAdd

/-- `a + b`. -/
def spec_add : PExpr := .binOp .add (.var "a") (.var "b")

theorem add_comm (a b : Int) (fuel : Nat) :
    eval FnTable.empty ((Env.empty.bind "a" (.int a)).bind "b" (.int b)) (fuel + 1) spec_add
      = some (.int (a + b)) := by
  simp [spec_add, eval, Env.bind, evalBinOp]

end PureAdd

namespace Examples.TestFixtures.Proofs

/-! Parameter declarations, here rather than beside the specifications because the generator reads
declarations only under the `Concrete` and `Examples` roots. One per distinct body: the `a + b`
specifications share one, and agree. -/
def addCommutativityDecl : SpecDecl := { params := ["a", "b"], body := add_commutativity }
def mulAssociativityDecl : SpecDecl := { params := ["a", "b"], body := mul_associativity }
def subIdentityDecl : SpecDecl := { params := ["a", "b"], body := sub_identity }

theorem pure_add_correct (a b : Int) (fuel : Nat) :
    eval FnTable.empty ((Env.empty.bind "a" (.int a)).bind "b" (.int b)) (fuel + 1) add_commutativity
      = some (.int (a + b)) := by
  simp [add_commutativity, eval, Env.bind, evalBinOp]

theorem pure_mul_correct (a b : Int) (fuel : Nat) :
    eval FnTable.empty ((Env.empty.bind "a" (.int a)).bind "b" (.int b)) (fuel + 1) mul_associativity
      = some (.int (a * b)) := by
  simp [mul_associativity, eval, Env.bind, evalBinOp]

theorem pure_sub_correct (a b : Int) (fuel : Nat) :
    eval FnTable.empty ((Env.empty.bind "a" (.int a)).bind "b" (.int b)) (fuel + 1) sub_identity
      = some (.int (a - b)) := by
  simp [sub_identity, eval, Env.bind, evalBinOp]

theorem left_add_correct (a b : Int) (fuel : Nat) :
    eval FnTable.empty ((Env.empty.bind "a" (.int a)).bind "b" (.int b)) (fuel + 1) left_add_spec
      = some (.int (a + b)) := by
  simp [left_add_spec, eval, Env.bind, evalBinOp]

theorem right_add_correct (a b : Int) (fuel : Nat) :
    eval FnTable.empty ((Env.empty.bind "a" (.int a)).bind "b" (.int b)) (fuel + 1) right_add_spec
      = some (.int (a + b)) := by
  simp [right_add_spec, eval, Env.bind, evalBinOp]

/-- A theorem that MENTIONS a specification only in a hypothesis and concludes nothing about it.
    It must not bind any claim: the binding reads the conclusion (check_subject_binding.sh). -/
theorem spec_in_hypothesis_only
    (_h : eval FnTable.empty Env.empty 1 add_commutativity = none) : True := trivial

/-- The same assumption one connective down: a conjunct that is an IMPLICATION whose domain mentions
    the specification. Its conclusion is `True`; it must bind nothing (check_subject_binding.sh, H). -/
theorem spec_in_nested_implication :
    True ∧ ((eval FnTable.empty Env.empty 1 add_commutativity = none) → True) :=
  ⟨trivial, fun _ => trivial⟩

/-- A disjunction with an irrelevant `True` disjunct: the evaluation is never established, so it must
    bind nothing (check_subject_binding.sh, H). -/
theorem spec_in_irrelevant_disjunction :
    (eval FnTable.empty Env.empty 1 add_commutativity = some (.int 7)) ∨ True :=
  Or.inr trivial

/-- A function that ignores a proof argument. -/
def ignoreProof (P : Prop) (_ : P) : Nat := 0

/-- The specification inside an arbitrary function's PROPOSITION ARGUMENT, in a conclusion that is an
    equation of values: it says only that `ignoreProof` is 0, so it must bind nothing — only a direct
    evaluator application (or a whole spec term) is a recognized operand (Codex land1 confirmation
    review; check_subject_binding.sh, H). -/
theorem spec_in_proof_argument
    (h : eval FnTable.empty Env.empty 1 add_commutativity = none) :
    ignoreProof (eval FnTable.empty Env.empty 1 add_commutativity = none) h = 0 := rfl

end Examples.TestFixtures.Proofs
