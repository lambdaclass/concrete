"""Validate the frontend's structured observation schema, including empty closures."""
import json
import sys
seen = 0
for line in open(sys.argv[1], encoding="utf-8"):
    if not line.startswith("example-json "): continue
    seen += 1
    row = json.loads(line.removeprefix("example-json "))
    if (set(row) != {"name", "axioms"} or not isinstance(row["name"], str)
            or not isinstance(row["axioms"], list)
            or not all(isinstance(a, str) for a in row["axioms"])):
        sys.exit("FAIL examples: invalid observation schema")
if seen == 0:
    sys.exit("FAIL examples: no example-json rows observed")
