"""Qualify the atomic rollback proof against an impossible reservation failure.

Run under the compiler gate's bounds. Lake rebuilds the mutated interface and
its consumers in a disposable package using the ordinary dependency graph.
"""

import json
from pathlib import Path
import shutil
import subprocess
import tempfile


def main():
    root = Path.cwd()
    control = Path("Concrete/Memory/TransferControl.lean")
    contracts = Path("Concrete/Interfaces/Contracts.lean")
    premise = "      memory.BeginExhausted state place.logical.root →"
    source = contracts.read_text()
    if source.count(premise) != 1:
        raise SystemExit("expected one exhaustion premise in WriteParams.exhausted")

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
        (package / contracts).write_text(source.replace(premise, "      False →"))

        # A broken interface, import or compiler is a gate failure.
        command = ["lake", "lean", str(control), "--", "--json"]
        subprocess.run(["lake", "build", "Concrete.Memory.Refinement"], cwd=package, check=True)
        result = subprocess.run(command, cwd=package, capture_output=True, text=True)
        diagnostics = [json.loads(line) for line in result.stdout.splitlines() if line.startswith("{")]
        errors = [entry["data"] for entry in diagnostics if entry["severity"] == "error"]
        if result.returncode != 1 or not errors or not all(
            "WriteParams.exhausted" in error and "False" in error for error in errors
        ):
            raise SystemExit(f"expected rejection of the rollback witness\n{result.stdout}{result.stderr}")
        print("ok   an impossible WriteParams.exhausted prevents the reachable rollback proof")


if __name__ == "__main__":
    main()
