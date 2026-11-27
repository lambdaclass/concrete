"""Qualify the atomic rollback proof against an impossible failure after a prefix write.

Run under the compiler gate's bounds. Lake rebuilds the mutated interface and
its consumers in a disposable package using the ordinary dependency graph.
"""

import json
from pathlib import Path
import shutil
import subprocess
import tempfile


def check_rejection(package, module, constructor):
    command = ["lake", "lean", str(module), "--", "-DwarningAsError=true", "-DautoImplicit=false", "--json"]
    result = subprocess.run(command, cwd=package, capture_output=True, text=True)
    diagnostics = [json.loads(line) for line in result.stdout.splitlines() if line.startswith("{")]
    errors = [entry["data"] for entry in diagnostics if entry["severity"] == "error"]
    if result.returncode != 1 or not errors or not all(constructor in error and "False" in error for error in errors):
        raise SystemExit(f"expected rejection at {constructor}\n{result.stdout}{result.stderr}")


def main():
    root = Path.cwd()
    control = Path("Concrete/Memory/TransferControl.lean")
    contracts = Path("Concrete/Interfaces/Contracts.lean")
    premise = "memory.Writes started place value stored ceiling →"
    mutation = "(if exhausting then False else memory.Writes started place value stored ceiling) →"
    unavailable = "(¬ memory.ParamSpaceFits (⟨ty, place⟩ :: params) state) →"
    source = contracts.read_text()
    if source.count(unavailable) != 1:
        raise SystemExit("expected one failed reservation in WriteParams.exhausted")
    if source.count(premise) != 1:
        raise SystemExit("expected one prefix step in WriteParams.cons")

    subprocess.run(["lake", "build", "Concrete.Memory.TransferControl"], check=True)
    with tempfile.TemporaryDirectory() as temporary:
        package = Path(temporary)
        shutil.copytree("Concrete", package / "Concrete")
        for name in ("lakefile.toml", "lake-manifest.json", "lean-toolchain",
                     "Concrete.lean", "ConcreteSpec", "ConcreteSpec.lean",
                     "ConcreteSpecEvidence", "ConcreteSpecEvidence.lean"):
            (package / name).symlink_to(root / name)
        (package / ".lake").mkdir()
        (package / ".lake/packages").symlink_to(root / ".lake/packages")
        # Copy the local build cache. Re-elaboration can replace any of its files.
        subprocess.run(["cp", "-a", "--reflink=auto", ".lake/build", package / ".lake/build"], check=True)
        # Pin the reachable claim as well as the failing constructor. Removing
        # or weakening the public theorem must fail the pristine compilation.
        probe = package / control
        with probe.open("a") as stream:
            stream.write("""
namespace Concrete.Memory.TransferControl
example : (program true).HandoffWF ∧ exec.finite (program true) forced ∧
    (writtenByte 2).bytes ≠ forced.bytes ∧
    exec.step (program true) forced {forced with resourceExhausted := true} ∧
    exec.result (program true) {forced with resourceExhausted := true} .resourceExhausted ∧
    ∀ next, exec.step (program true) forced next → next = {forced with resourceExhausted := true} :=
  rollback_after_write
end Concrete.Memory.TransferControl
""")

        # A broken interface, import or compiler is a gate failure.
        command = ["lake", "lean", str(control), "--", "-DwarningAsError=true", "-DautoImplicit=false", "--json"]
        subprocess.run(command, cwd=package, check=True)
        # Disabling failure altogether rejects the general progress theorem.
        (package / contracts).write_text(source.replace(unavailable, "False →"))
        check_rejection(package, contracts, "WriteParams.exhausted")
        print("ok   an impossible WriteParams.exhausted prevents genuine exhaustion")

        # Still allow an immediate abort, but disallow one following a write.
        # This isolates the rollback fixture's nonempty physical prefix.
        (package / contracts).write_text(source.replace(premise, mutation))
        subprocess.run(["lake", "build", "Concrete.Memory.Refinement"], cwd=package, check=True)
        check_rejection(package, control, "WriteParams.cons")
        print("ok   disabling exhaustion after a prefix write prevents the reachable rollback proof")


if __name__ == "__main__":
    main()
