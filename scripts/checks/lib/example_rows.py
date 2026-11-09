"""Validate the frontend's structured observation schema, including empty closures."""
import json
import re
import sys

EXAMPLE_NAME = re.compile(r"(?:^|\.)«?⟪example⟫.+:\d+:\d+(?::[A-Za-z0-9_._@]+)?»?$")
seen = 0
non_example_lines = 0
for raw in open(sys.argv[1], encoding="utf-8"):
    line = raw.rstrip("\r\n")
    if not line:
        continue
    if not line.startswith("example-json "):
        non_example_lines += 1
        continue
    seen += 1
    try:
        row = json.loads(line.removeprefix("example-json "))
    except json.JSONDecodeError:
        sys.exit("FAIL examples: invalid observation schema")
    if (not isinstance(row, dict) or set(row) != {"name", "axioms"}
            or not isinstance(row["name"], str)
            or not EXAMPLE_NAME.search(row["name"])
            or not isinstance(row["axioms"], list)
            or not all(isinstance(a, str) and a for a in row["axioms"])):
        sys.exit("FAIL examples: invalid observation schema")
if seen == 0:
    sys.exit("FAIL examples: no example-json rows observed")
if non_example_lines > 0:
    sys.exit("FAIL examples: invalid observation schema")
