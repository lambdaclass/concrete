"""Light byte-compatibility checks for the shared population consumers.
No whole-spec build is needed; the caller must hold the shared Lean build slot.
"""
import subprocess
from vacuity_driver import run, ROOT

fixture = """
namespace ConcreteSpecControl
theorem publicProof : True := trivial
private theorem hidden : 2 = 2 := rfl
def recursive : Nat → Nat
  | 0 => 0
  | n + 1 => recursive n + 1
example : 1 = 1 := rfl
end ConcreteSpecControl
"""
old = subprocess.check_output(["git", "show", "9cddd5891:scripts/proofs/theorems.lean"], cwd=ROOT, text=True)
for mode in ["theorems", "constants", "examples", "heads", "modules", "imported"]:
    before = run(ROOT, "", fixture, inventory=True, driver_text=old, proof_mode=mode)
    after = run(ROOT, "", fixture, inventory=True, proof_mode=mode)
    assert before[0] == after[0] == 0, (mode, before, after)
    assert before[1] == after[1], (mode, before, after)
    print("PASS shared population compatibility " + mode, flush=True)
# Rule checker source and JSONL consumer are unchanged by this round.
for path in ["scripts/spec/rules.py"]:
    baseline = subprocess.check_output(["git", "show", "6c76ac3c:" + path], cwd=ROOT)
    assert (ROOT / path).read_bytes() == baseline, path
    print("PASS unchanged rule consumer " + path, flush=True)
for path in ["scripts/proofs/axioms.sh", "scripts/proofs/trust_ban.sh"]:
    baseline = subprocess.check_output(["git", "show", "9cddd5891:" + path], cwd=ROOT)
    assert (ROOT / path).read_bytes() == baseline, path
    print("PASS unchanged audit " + path, flush=True)

registry_fixture = """
namespace ConcreteSpec.Core
structure RuleId where
  name : Lean.Name
  rev : Nat
/-- The fixture rule (Q9.1). -/
def rule : Prop := True
def registeredRules : List RuleId := [⟨``rule, 1⟩]
end ConcreteSpec.Core
"""
original_registry = (ROOT.parent / "gates-vacuity-c10/scripts/spec/rules.lean").read_text()
before = run(ROOT, "", registry_fixture, inventory=True, driver_text=original_registry)
after = run(ROOT, "", registry_fixture, inventory=True, proof_mode="rules")
assert before[0] == after[0] == 0, (before, after)
assert before[1] == after[1], (before, after)
print("PASS registered-rule JSONL compatibility", flush=True)
