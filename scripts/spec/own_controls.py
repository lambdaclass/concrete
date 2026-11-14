#!/usr/bin/env python3
"""Reject isolated ownership mutants at the specific conformance observations.

Only private temporary source files are compiled. Six additional per-operation
permission omissions are checked against populated refused operands in Own.lean.
Run in the Spec shell after lake build ConcreteSpec.
"""
from pathlib import Path
import re
import subprocess
import tempfile

source = Path("ConcreteSpec/Spec/Own.lean").read_text()
owner_observations = ["example : (match (ownerRun false)",
                      "example : (match (ownerRun true)"]
mutants = {
    "argument-access": (
        "if loanAccess s i .mutable then .ok (0, s) else .stuck", ".ok (0, s)",
        ["example : Exec.evalArgs callOps"]),
    "scope-drop": (
        "| .drop xs, s => drop xs s", "| .drop _, s => some s",
        ["example : (runBody finalBlock).residual"]),
    "owner-access": (
        "ownerAccess s.own root mode && decide (f.id < s.own.nextActivation)",
        "decide (f.id < s.own.nextActivation)",
        ["example : passOwner ⟨exclusive"]),
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
            if changed.count(observation) != 1:
                raise RuntimeError(f"{name}: observation anchor must occur exactly once")
            start = changed.index(observation)
            end = changed.index(":= by decide", start) + len(":= by decide")
            first = changed.count("\n", 0, start) + 1
            last = changed.count("\n", 0, end) + 1
            if not errors.intersection(range(first, last + 1)):
                raise RuntimeError(f"{name}: required observation did not fail\n{diagnostics}")
        print(f"{name}: rejected at every required conformance observation", flush=True)
