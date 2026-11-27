"""Build the stage controls and check the target covers every Lean source in ConcreteChecks/."""
import json
from pathlib import Path
import subprocess
import sys


TARGET = "ConcreteChecks.StageInterfaces"


def query(*targets):
    output = subprocess.check_output(["lake", "query", "--json", *targets], text=True)
    return [json.loads(line) for line in output.splitlines()]


def main():
    subprocess.run(["lake", "build", TARGET], check=True)
    # Lake's leanArts build consumes exactly its :modules facet. Ask Lake for
    # those modules' source paths so a stale .olean cannot satisfy the inventory.
    [modules] = query(f"{TARGET}:modules")
    if not modules:
        sys.exit(f"{TARGET} contains no control modules")
    built = {Path(p) for p in query(*(f"+{m}:lean" for m in modules))}
    root = Path.cwd()
    sources = {p for p in (root / "ConcreteChecks").rglob("*.lean", recurse_symlinks=True) if p.is_file()}
    # Also catch a new package-level root if one is introduced.
    if (root / "ConcreteChecks.lean").is_file():
        sources.add(root / "ConcreteChecks.lean")
    for path in sorted(sources - built):
        print(f"missing from {TARGET}: {path.relative_to(root)}", file=sys.stderr)
    for path in sorted(built - sources):
        print(f"unexpected source in {TARGET}: {path}", file=sys.stderr)
    return int(sources != built)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except subprocess.CalledProcessError as exc:
        sys.exit(exc.returncode)
