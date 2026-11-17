#!/usr/bin/env python3
"""Supplement the elaborated F31 census with erased examples and non-library consumers.

F31 migration tool: retire with the manifest and companion tools when F31 closes.
Run from the repository root after f31_consumers.lean. Writes deterministic JSON to stdout.
This is a conservative qualification surface: inclusion never asserts a semantic change is allowed.
"""
import json
from pathlib import Path
import re
import sys
import subprocess
import tempfile

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "proofs"))
from lean_lex import lex

rows = set()

def add(kind, path, line, subject):
    rows.add((kind, str(path), line, subject))

# Examples are erased from environments: scan every library module, even those with no named consumer.
for path in sorted(list(Path("ConcreteSpec").rglob("*.lean")) + list(Path("Concrete").rglob("*.lean"))):
    code = lex(path.read_text(), literals=False)
    for match in re.finditer(r"(?m)^\s*example\b", code):
        line = code.count("\n", 0, match.end()) + 1
        add("anonymous-example", path, line, "example")

def probe_rows(path, source, output):
    result = subprocess.run(
        ["lake", "env", "lean", "--run", "scripts/spec/f31_probes.lean",
         str(path), str(source), str(output)],
        text=True, capture_output=True,
    )
    if result.returncode:
        raise SystemExit(f"F31: probe {path} failed\n{result.stdout}{result.stderr}")
    return [(kind, line, subject) for (kind, line), subject in json.loads(output.read_text())]


# Elaborate each standalone script as written. The frontend captures commands that leave
# no persistent declaration, including examples, directives and commands that change scope.
control_probe = Path("scripts/checks/compiler/slice0_bir.lean")
with tempfile.TemporaryDirectory(prefix="f31-probe-") as directory:
    output = Path(directory) / "probe.json"
    for path in sorted(Path("scripts").rglob("*.lean")):
        probe = probe_rows(path, path, output)
        for kind, line, subject in probe:
            add(kind, path, line, subject)

    # Plant an anonymous example in a scratch copy of an existing probe. Preserve the
    # logical filename and require exactly the new command row, with no declaration delta.
    example = "example : True := True.intro"
    prefix = control_probe.read_text().rstrip("\n") + "\n\n"
    planted = Path(directory) / control_probe.name
    planted.write_text(prefix + example + "\n")
    expected = ("probe-command", prefix.count("\n") + 1, f"0: {example}")
    baseline = [(kind, line, subject) for kind, path, line, subject in rows
                if path == str(control_probe)]
    if sorted(probe_rows(control_probe, planted, output)) != sorted(baseline + [expected]):
        raise SystemExit("F31: planted standalone example did not add exactly one command row")

for text in Path(sys.argv[1]).read_text().splitlines():
    if text.startswith("F31-REGISTRY\t"):
        _, kind, subject = text.split("\t", 2)
        add(kind, "Concrete/Premises.lean", 1, subject)

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
