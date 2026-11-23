#!/usr/bin/env python3
"""Inventory the F31 representation closure in a fresh Lean frontend run."""
import argparse
from collections import defaultdict, deque
from hashlib import sha256
import json
import os
from pathlib import Path, PurePosixPath
import subprocess
import tempfile

LEGACY_NAMES = (
    "ConcreteSpec.IntTy", "ConcreteSpec.STy", "ConcreteSpec.Scalar",
    "ConcreteSpec.Rep.ofBits",
    "Concrete.BIR.Ty", "Concrete.BIR.Val", "Concrete.BIR.BinOp",
    "Concrete.BIR.wrap", "Concrete.BIR.ScalarBridge",
    "Concrete.Backend.LLVM.pattern",
)


def surface_role(path):
    if path in ("Concrete.lean", "ConcreteSpec.lean") or path.startswith(("Concrete/", "ConcreteSpec/")):
        if path.endswith(".lean"):
            return "module"
    if path.startswith(("scripts/", ".githooks/")):
        return "lean-probe" if path.endswith(".lean") else "tool"
    if path.startswith("tests/"):
        return "golden" if path.startswith("tests/golden/") else "test"
    if path in ("Concrete/README.md", "ConcreteSpec/README.md", "lakefile.toml",
                "lake-manifest.json", "lean-toolchain") or path.startswith(".github/workflows/"):
        return "configuration"
    return None


def subprocess_env(command):
    env = os.environ.copy()
    # Git can list these variable names without opening a repository.
    local = subprocess.check_output(["git", "rev-parse", "--local-env-vars"],
                                    env={"PATH": env.get("PATH", os.defpath)},
                                    text=True)
    for name in local.splitlines():
        env.pop(name, None)
    if command == "lake":
        for name in ("LEAN_PATH", "LEAN_SRC_PATH"):
            env.pop(name, None)
    return env


def snapshot(root, source):
    names = subprocess.check_output(["git", "ls-files", "--cached", "-z"], cwd=root,
                                    env=subprocess_env("git"))
    rows = []
    for raw in sorted(set(names.split(b"\0")) - {b""}):
        path = os.fsdecode(raw)
        role = surface_role(path)
        if role is None:
            continue
        original = root / path
        parents = PurePosixPath(path).parents
        if any((root / p).is_symlink() for p in parents) or original.is_symlink() or not original.is_file():
            raise ValueError(f"expected a tracked regular input: {path}")
        contents = original.read_bytes()
        copied = source / path
        copied.parent.mkdir(parents=True, exist_ok=True)
        copied.write_bytes(contents)
        rows.append({"file": path, "role": role, "sha256": sha256(contents).hexdigest()})
    return rows


def write_jsonl(path, rows):
    with path.open("w", encoding="utf-8") as out:
        for row in rows:
            out.write(json.dumps(row, sort_keys=True) + "\n")


def is_legacy(name):
    return any(name == seed or name.startswith(seed + ".") for seed in LEGACY_NAMES)


def classify(records):
    headers = {row["file"]: row["imports"] for row in records if row["record"] == "header"}
    modules = {file.removesuffix(".lean").replace("/", "."): file for file in headers}
    reachable = {}
    for file in headers:
        imports = set()
        pending = list(headers[file])
        while pending:
            imported = modules.get(pending.pop())
            if imported is not None and imported not in imports:
                imports.add(imported)
                pending.extend(headers[imported])
        reachable[file] = imports

    declarations = defaultdict(set)
    for row in records:
        if row["record"] == "declaration":
            declarations[row["name"]].add(row["file"])

    def owners(file, name):
        # Standalone probes can have identically named local declarations.
        # An imported reserved helper can also be realized in several files.
        candidates = declarations.get(name, set())
        return {file} if file in candidates else candidates & reachable[file]

    reverse = defaultdict(set)
    affected = set()
    for name, files in declarations.items():
        if is_legacy(name):
            affected.update((file, name) for file in files)
    for row in records:
        if row["record"] != "declaration":
            continue
        file, name = row["file"], row["name"]
        for reference in row["uses"]:
            if is_legacy(reference):
                affected.add((file, name))
            for owner in owners(file, reference):
                reverse[owner, reference].add((file, name))
    pending = deque(affected)
    while pending:
        for caller in reverse[pending.popleft()] - affected:
            affected.add(caller)
            pending.append(caller)

    for row in records:
        file = row["file"]
        refs = row["uses"]
        row["direct"] = sorted(name for name in refs if is_legacy(name))
        row["via"] = sorted(name for name in refs if not is_legacy(name) and any(
            (owner, name) in affected for owner in owners(file, name)))
        row["legacyDeclaration"] = "name" in row and is_legacy(row["name"])


def run(args, source):
    # Source commands can print arbitrary diagnostics. The exporter writes its
    # machine-readable records to a separate private file.
    result = subprocess.run(args, cwd=source, env=subprocess_env(args[0]),
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if result.returncode:
        raise RuntimeError(f"{' '.join(map(str, args))} failed:\n{os.fsdecode(result.stdout)}")


def generate(root, source, result):
    surface = snapshot(root, source)
    run(["lake", "build", "Concrete", "ConcreteSpec"], source)
    records = []
    for row in surface:
        if row["role"] not in ("module", "lean-probe"):
            continue
        path = row["file"]
        module = path.removesuffix(".lean").replace("/", ".")
        export = source / "frontend.jsonl"
        run(["lake", "env", "lean", "--run", str(source / "scripts/spec/f31_census.lean"),
             path, module, str(export)], source)
        records.extend(json.loads(line) for line in export.read_text(encoding="utf-8").splitlines())
    classify(records)
    write_jsonl(result / "lean.jsonl", records)
    write_jsonl(result / "surface.jsonl", surface)


def publish(destination, produce):
    destination = destination.absolute()
    # Staging and destination share a filesystem. Only publishers to this
    # destination contend; readers see absence or a complete directory.
    with tempfile.TemporaryDirectory(prefix=".f31-", dir=destination.parent) as scratch:
        scratch = Path(scratch)
        source = scratch / "source"
        result = scratch / "result"
        source.mkdir()
        result.mkdir()
        produce(source, result)
        payloads = sorted(result.iterdir())
        digests = {p.name: sha256(p.read_bytes()).hexdigest() for p in payloads}
        (result / "sha256.json").write_text(json.dumps(digests, sort_keys=True) + "\n", encoding="utf-8")
        claim = destination.with_name(destination.name + ".f31-claim")
        claim.mkdir()
        try:
            if os.path.lexists(destination):
                raise FileExistsError(f"inventory already exists: {destination}")
            result.rename(destination)
        finally:
            claim.rmdir()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path, help="new inventory directory")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    try:
        publish(args.output, lambda source, result: generate(root, source, result))
    except (OSError, RuntimeError, ValueError) as exc:
        parser.exit(1, f"f31 census: {exc}\n")


if __name__ == "__main__":
    main()
