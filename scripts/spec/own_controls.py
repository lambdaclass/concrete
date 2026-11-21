#!/usr/bin/env python3
"""Reject isolated ownership mutants at the specific conformance observations.

Only private temporary source files are compiled. Six additional per-operation
permission omissions are checked against populated refused operands in Own.lean.
Run in the Spec shell after lake build ConcreteSpec.
"""
import json
from pathlib import Path
import re
import subprocess
import tempfile

source = Path("ConcreteSpec/Spec/Own.lean").read_text()
ranges = json.loads(subprocess.check_output(
    ["lake", "env", "lean", "scripts/spec/own_control_ranges.lean"], text=True))
namespace = "ConcreteSpec.Core.Own.Examples"
owner_observations = ["example_immediate_direct_owner_lending_is_live",
                      "example_deferred_registration_leaves_owner_reads_available"]
mutants = {
    "argument-access": (
        "if loanAccess s i .mutable then .ok (0, s) else .stuck", ".ok (0, s)",
        ["example_OD23_refuses_second_operand_itself_before"]),
    "scope-drop": (
        "| .drop xs, s => drop xs s", "| .drop _, s => some s",
        ["example_outer_obligation_is_consumed_Refusal_retains"]),
    "owner-access": (
        "ownerAccess s.own root mode && decide (f.id < s.own.nextActivation)",
        "decide (f.id < s.own.nextActivation)",
        ["example_direct_owner_lending_cannot_bypass_populated"]),
    "owner-refusal": (
        "if ownerAccess s.own root mode && decide (f.id < s.own.nextActivation) then",
        "if false then", owner_observations),
    "owner-preservation": (
        "f.id, true, true⟩ :: s.own.loans", "f.id, true, true⟩ :: []",
        owner_observations),
    "owner-duration": (
        "(s.loans.filter (·.activation == activation))",
        "(s.loans.filter (fun l => l.activation == activation && !l.callRoot))",
        owner_observations),
}

with tempfile.TemporaryDirectory(prefix="own-mutants-") as temporary:
    for name, (old, new, observations) in mutants.items():
        if source.count(old) != 1:
            raise RuntimeError(f"{name}: mutation anchor must occur exactly once")
        # Lean's baseline ranges still identify the mutated observations.
        if old.count("\n") != new.count("\n"):
            raise RuntimeError(f"{name}: mutation must preserve line numbers")
        changed = source.replace(old, new)
        path = Path(temporary) / "Own.lean"
        path.write_text(changed)
        result = subprocess.run(["lake", "env", "lean", str(path)],
                                capture_output=True, text=True)
        diagnostics = result.stdout + result.stderr
        errors = {int(line) for line in re.findall(r":(\d+):\d+: error:", diagnostics)}
        if result.returncode == 0:
            raise RuntimeError(f"{name}: mutant survived")
        for observation in observations:
            declaration = f"{namespace}.{observation}"
            if declaration not in ranges:
                raise RuntimeError(f"{name}: missing observation {declaration}")
            first, last = ranges[declaration]
            if not errors.intersection(range(first, last + 1)):
                raise RuntimeError(f"{name}: {declaration} did not fail\n{diagnostics}")
        print(f"{name}: rejected at every required conformance observation", flush=True)

# Disable just the declarative ledger arm. The pinned positive witness must fail on
# its own, even if the corresponding preservation proof can be closed vacuously.
ledger = Path("ConcreteSpec/Proofs/OwnLegalStep.lean").read_text()
loan_access_arm = (
    "  | .accessLoan i m => LiveDestinations s.own ∧ AccessLoan s.own i m t.own"
    " ∧ t.pending = s.pending"
)
if ledger.count(loan_access_arm) != 1:
    raise RuntimeError("ledger loan access: mutation anchor must occur exactly once")
witness = "theorem ledgerAccessLoan_live "
witness_start = next(
    number for number, line in enumerate(ledger.splitlines(), 1) if line.startswith(witness)
)
with tempfile.TemporaryDirectory(prefix="ledger-mutant-") as temporary:
    path = Path(temporary) / "OwnLegalStep.lean"
    path.write_text(ledger.replace(loan_access_arm, "  | .accessLoan _ _ => False"))
    result = subprocess.run(["lake", "env", "lean", str(path)], capture_output=True, text=True)
    diagnostics = result.stdout + result.stderr
    errors = {int(line) for line in re.findall(r":(\d+):\d+: error:", diagnostics)}
    if result.returncode == 0 or not errors.intersection(range(witness_start, witness_start + 3)):
        raise RuntimeError(f"disabled ledger loan access did not kill its pinned witness\n{diagnostics}")
    print("disabled ledger loan access: rejected at its pinned positive witness", flush=True)
