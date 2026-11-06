#!/usr/bin/env python3
"""Declared external authority of a program, read from STRUCTURED facts.

Usage: authority_facts.py <diagnostics-json file>
       authority_facts.py --expand <repo root> NAME...   (validate + expand policy names)

Reads the output of `concrete <src> --report diagnostics-json` and prints one JSON object:

  {"used": [...],            union of capabilities the program's functions DECLARE
   "functions": N,           program functions with a capability fact
   "empty_declarations": K,  of those, how many declare no capability
   "incomplete": [...],      functions whose call-graph coverage is incomplete
   "dependencies_analysed": bool}

Declared capabilities are the compiler-enforced, complete list of a function's external
authority (R-0484): a caller's `with(...)` covers every callee, including through function
values and handles. So a policy over the declared union is a real authority check. Coverage
incompleteness does not weaken it, but it is reported, because conclusions about which
FOREIGN bindings are assumed (and any "no external authority" claim) do depend on it.

Exits 2, printing the reason, when the input cannot establish an answer — it never turns
missing or malformed input into an empty capability set:
  - the file does not parse as JSON, or is not a facts envelope;
  - schema_version is missing or is not the version this checker reads (2);
  - there are no capability facts for any function;
  - a function's facts say assumptions were not computed.

This replaces scraping `--report caps` text. That scrape read only PARENTHESISED tokens, while a
capability headline prints bare (`fn : Console`), so it never saw a capability: the policy and
assumption-file authority checks were vacuous.
"""
import json
import sys

SCHEMA = 2


def fail(msg):
    print(f"authority-facts: {msg}", file=sys.stderr)
    sys.exit(2)


def main():
    if len(sys.argv) != 2:
        fail("usage: authority_facts.py <diagnostics-json file>")
    try:
        doc = json.load(open(sys.argv[1]))
    except Exception as e:  # noqa: BLE001 — any read/parse failure is "no answer"
        fail(f"input is not valid JSON ({e.__class__.__name__}: {e})")
    if not isinstance(doc, dict):
        fail("input is not a facts envelope (expected a JSON object)")
    if "schema_version" not in doc:
        fail("schema_version is missing")
    if doc["schema_version"] != SCHEMA:
        fail(f"schema_version {doc['schema_version']} is not {SCHEMA}")
    facts = doc.get("facts")
    if not isinstance(facts, list):
        fail("envelope has no facts array")
    caps = [f for f in facts if isinstance(f, dict) and f.get("kind") == "capability"
            and not f.get("is_extern")]
    if not caps:
        fail("no capability facts for any function — nothing establishes the program's authority")
    used, incomplete, empty = set(), [], 0
    deps = None
    for f in caps:
        fn = f.get("function", "?")
        if f.get("assumptions_computed") is not True:
            fail(f"{fn}: assumptions were not computed — the facts cannot qualify its authority")
        declared = f.get("capabilities")
        if not isinstance(declared, list):
            fail(f"{fn}: capabilities is not a list")
        used.update(declared)
        if not declared:
            empty += 1
        if f.get("coverage_complete") is not True:
            incomplete.append(fn)
        if deps is None:
            deps = f.get("dependencies_analysed")
    print(json.dumps({"used": sorted(used), "functions": len(caps),
                      "empty_declarations": empty, "incomplete": incomplete,
                      "dependencies_analysed": bool(deps)}))


def language_caps(root):
    """validCaps and the Std alias, read from the compiler's own definitions (AST.lean), so this
    checker cannot drift from the language."""
    import os, re
    src = open(os.path.join(root, "Concrete/Frontend/AST.lean")).read()
    def lst(name):
        m = re.search(r"def " + name + r" : List String :=\s*\[([^\]]*)\]", src)
        if not m:
            fail(f"cannot read {name} from Concrete/Frontend/AST.lean")
        return [x.strip().strip('"') for x in m.group(1).split(",") if x.strip()]
    return lst("validCaps"), lst("stdCaps")


def expand(root, names):
    """`--expand ROOT NAME...`: validate policy capability names and expand the Std alias. An
    unknown name is an ERROR, not a no-op: `Net` (for `Network`) sat in ten policy files and made
    every one of their forbidden checks unmatchable."""
    valid, std = language_caps(root)
    out = []
    for n in names:
        if n == "Std":
            out.extend(std)
        elif n in valid:
            out.append(n)
        else:
            fail(f"'{n}' is not a capability (valid: {', '.join(valid)}; alias: Std)")
    print(" ".join(sorted(set(out))))


if __name__ == "__main__":
    if len(sys.argv) >= 3 and sys.argv[1] == "--expand":
        expand(sys.argv[2], sys.argv[3:])
    else:
        main()
