#!/usr/bin/env python3
"""Supplement the elaborated F31 census with parsed commands and non-library declarations.

F31 migration tool: retire with the manifest and companion tools when F31 closes.
Run from the repository root after building both libraries. Writes deterministic JSON to stdout.
This is a conservative qualification surface: inclusion never asserts a semantic change is allowed.
"""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

rows = set()

def add(kind, path, line, subject):
    rows.add((kind, str(path), line, subject))

def tracked_lean_sources(env=None):
    # legacy/ contains the frozen compiler and its archived gates (AGENTS.md, R-0558).
    files = subprocess.check_output(
        ["git", "ls-files", "-z", "--", "*.lean", ":(top,exclude)legacy/**"], env=env,
    )
    return {Path(os.fsdecode(path)) for path in files.split(b"\0") if path}


def driver_output(driver, *args):
    command = ["lake", "env", str(driver), *map(str, args)]
    result = subprocess.run(command, text=True, capture_output=True)
    if result.returncode:
        raise SystemExit(f"F31: frontend failed for {args!r}\n{result.stdout}{result.stderr}")
    return result


def probe_rows(driver, mode, root, path, source, output):
    result = driver_output(driver, mode, root, path, source, output)
    return [(kind, line, subject) for (kind, line), subject in json.loads(output.read_text())], result


def standalone_probes(driver, library_paths, source_root, output, env=None):
    for path in sorted(tracked_lean_sources(env) - library_paths):
        probe, result = probe_rows(driver, "standalone", source_root, path, source_root / path, output)
        yield path, probe, result


destination = Path(sys.argv[1])
consumer = Path("scripts/spec/f31_consumers.lean")
control_probe = Path("scripts/checks/compiler/slice0_bir.lean")
with tempfile.TemporaryDirectory(prefix="f31-probe-") as directory:
    scratch = Path(directory)
    driver = scratch / "probes"
    c_source = driver.with_suffix(".c")
    subprocess.run(["lake", "env", "lean", "-c", str(c_source), "scripts/spec/f31_probes.lean"],
                   check=True)
    subprocess.run(["lake", "env", "leanc", "-rdynamic", "-o", str(driver), str(c_source)],
                   check=True)
    library_paths = {Path(path) for path in json.loads(driver_output(driver, "library-sources").stdout)}
    if not {Path("Concrete.lean"), Path("ConcreteSpec.lean")} <= library_paths:
        raise SystemExit("F31: missing library roots in Lean's import closure")

    missing = library_paths - tracked_lean_sources()
    if missing:
        raise SystemExit(f"F31: library sources outside the active tracked tree: {sorted(missing)}")

    output = scratch / "probe.json"
    for path in sorted(library_paths):
        probe, _ = probe_rows(driver, "library", Path("."), path, path, output)
        for kind, line, subject in probe:
            add(kind, path, line, subject)

    declarations, registry = [], []
    for path, probe, result in standalone_probes(driver, library_paths, Path("."), output):
        for kind, line, subject in probe:
            add(kind, path, line, subject)
        if path == consumer:
            # Use the output of the same elaboration that supplies the command rows.
            lines = (result.stdout + "\n" + result.stderr).splitlines()
            declarations = [line.removeprefix("F31-DECLARATIONS\t") for line in lines
                            if line.startswith("F31-DECLARATIONS\t")]
            registry = [line for line in lines if line.startswith("F31-REGISTRY\t")]

    if len(declarations) != 1 or not registry:
        raise SystemExit("F31: missing declaration census or live premise registry")
    (destination / "registry.txt").write_text("\n".join(registry) + "\n")
    (destination / "declarations.json").write_text(
        json.dumps(json.loads(declarations[0]), indent=2, ensure_ascii=False) + "\n",
    )
    for record in registry:
        _, kind, subject = record.split("\t", 2)
        add(kind, "Concrete/Premises.lean", 1, subject)

    # Exercise tracked discovery and the frontend with an isolated index and working tree.
    control_root = scratch / "worktree"
    outside = Path("tests/F31StandaloneControl.lean")
    planted = control_root / outside
    planted.parent.mkdir(parents=True)
    planted.write_text("import Concrete\nopen Nat\n")
    control_env = dict(os.environ, GIT_INDEX_FILE=str(scratch / "index"),
                       GIT_WORK_TREE=str(control_root))
    subprocess.run(["git", "add", "--intent-to-add", "--", str(outside)], env=control_env, check=True)
    discovered = [(path, sorted(probe)) for path, probe, _ in
                  standalone_probes(driver, library_paths, control_root, output, control_env)]
    if discovered != [(outside, [("probe-command", 1, "0: import Concrete"),
                                  ("probe-command", 2, "0: open Nat")])]:
        raise SystemExit("F31: planted standalone file outside scripts/ was not inventoried exactly")

    # Preserve the logical filename and require exactly one extra command with no declaration delta.
    example = "example : True := True.intro"
    prefix = control_probe.read_text().rstrip("\n") + "\n\n"
    planted = scratch / control_probe.name
    planted.write_text(prefix + example + "\n")
    expected = ("probe-command", prefix.count("\n") + 1, f"0: {example}")
    baseline = [(kind, line, subject) for kind, path, line, subject in rows
                if path == str(control_probe)]
    probe, _ = probe_rows(driver, "standalone", Path("."), control_probe, planted, output)
    if sorted(probe) != sorted(baseline + [expected]):
        raise SystemExit("F31: planted standalone example did not add exactly one command row")

    # Exercise the library census on a scratch copy of the compiler root.
    library_root = Path("Concrete.lean")
    prefix = library_root.read_text().rstrip("\n") + "\n\n"
    scoped = f"open Nat in {example}"
    planted = scratch / library_root.name
    planted.write_text(f"{prefix}{scoped}\n{example} {example}\n")
    line = prefix.count("\n") + 1
    expected = [("library-command", line, f"0: {scoped}"),
                ("library-command", line, "0: open Nat"),
                ("anonymous-example", line, f"{len('open Nat in ')}: {example}"),
                ("anonymous-example", line + 1, f"0: {example}"),
                ("anonymous-example", line + 1, f"{len(example) + 1}: {example}")]
    baseline = [(kind, line, subject) for kind, path, line, subject in rows
                if path == str(library_root) and kind in {"library-command", "anonymous-example"}]
    probe, _ = probe_rows(driver, "library", Path("."), library_root, planted, output)
    if sorted(probe) != sorted(baseline + expected):
        raise SystemExit("F31: planted scoped library example was not inventoried exactly")

# Synthetic modules and exact-name consumers in proof-tool generators must be requalified too.
for path in sorted(Path("scripts/proofs").glob("*.py")):
    add("proof-tool", path, 1, "requalify generated controls and exact-name consumers")

path = Path("tests/manifest.tsv")
for line, text in enumerate(path.read_text().splitlines(), 1):
    if text and not text.startswith("#") and not text.startswith("id\t"):
        fields = text.split("\t")
        add("test", path, line, fields[0] + " " + fields[1] + " " + fields[2])

# Qualification includes both physical backends, compiler identities and all proof controls.
checks = list(Path("scripts/checks").glob("*.sh"))
checks += list(Path("scripts/checks/compiler").glob("*.sh"))
checks += [Path("scripts/proofs") / (name + ".sh")
           for name in ("axioms", "trust_ban", "kernel_replay", "external")]
checks += [Path("scripts/spec/lean_site.sh"), Path("scripts/spec/rules.sh")]
for path in checks:
    add("check", path, 1, "requalify")
for path in ("scripts/proofs/axioms.golden", "tests/golden/slice0_trusted_base.txt"):
    add("golden", path, 1, "exact union; preserve all existing rows")
for path in (".github/workflows/lean_action_ci.yml", "required-checks.txt", "Concrete/README.md",
             "ConcreteSpec/README.md", "ConcreteSpec.lean", "Concrete.lean",
             "scripts/checks/lib/slice0_sources.sh", "scripts/checks/lib/slice0_common.sh"):
    add("integration", path, 1, "preserve imports, diagnostics, premise text and qualification coverage")
for path in sorted(Path("tests").rglob("*.con")):
    add("fixture", path, 1, "retain literal arithmetic/refusal expectations")

# Split long exact strings without adding whitespace; joining fragments reconstructs the subject.
records = [dict(kind=kind, path=path, line=line,
                subject=[subject[i:i + 80] for i in range(0, len(subject), 80)])
           for kind, path, line, subject in sorted(rows)]
print(json.dumps(records, indent=2, ensure_ascii=False))
