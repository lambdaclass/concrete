#!/usr/bin/env python3
"""Supplement the elaborated F31 census with erased examples and non-library consumers.

F31 migration tool: retire with the manifest and companion tool when F31 closes.
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
    assert "\t" not in subject and "\n" not in subject
    rows.add((kind, str(path), line, subject))

# Examples are erased from environments: scan every library module, even those with no named consumer.
for path in sorted(list(Path("ConcreteSpec").rglob("*.lean")) + list(Path("Concrete").rglob("*.lean"))):
    code = lex(path.read_text(), literals=False)
    for match in re.finditer(r"(?m)^\s*example\b", code):
        line = code.count("\n", 0, match.end()) + 1
        add("anonymous-example", path, line, "example")

# These commands are elaborated by gates, outside both library import closures. Include every
# command, not only text mentioning IntTy: inferred constructor tags are common in probes.
probe_suffix = r"""
#eval show Lean.CoreM Unit from do
  let env ← Lean.getEnv
  for (n, _) in env.constants.map₂.toList do
    if (env.getModuleIdxFor? n).isNone then
      IO.println s!"F31-PROBE\t{n}"
"""
for path in sorted(Path("scripts").rglob("*.lean")):
    if path.parts[1] == "spec":
        continue
    with tempfile.TemporaryDirectory(prefix="f31-probe-") as directory:
        probe = Path(directory) / "Inventory.lean"
        probe.write_text("import Lean\n" + path.read_text() + probe_suffix)
        result = subprocess.run(["lake", "env", "lean", str(probe)],
                                check=True, text=True, capture_output=True)
    names = [line.split("\t", 1)[1] for line in result.stdout.splitlines()
             if line.startswith("F31-PROBE\t")]
    if not names:
        add("probe-evaluation", path, 1, "elaborated; no persistent declarations")
    for name in names:
        add("probe-declaration", path, 1, name)

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
