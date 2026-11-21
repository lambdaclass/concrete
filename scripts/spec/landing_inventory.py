#!/usr/bin/env python3
"""Pin the ownership transition laws and their kernel-checked evidence.

Lean elaborates the source examples in the proof target. This gate binds those
examples and the statements they instantiate to the landing inventory.
"""
import argparse
from collections import defaultdict
from hashlib import sha256
from pathlib import Path
import re
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "proofs"))
from lean_lex import lex

PROOFS = Path("ConcreteSpec/Proofs")
MODULES = ("OwnLegalStep", "OwnProtocols")
GOLDEN = Path("scripts/spec/landing_inventory.tsv")
PROPERTY_LAWS = {
    "issuance_deterministic",
    "issuanceTrace_deterministic",
    "issuance_wellFormed",
    "lifecycle_nonrevival",
}
HEADER = "# Ownership transition law, statement hash, instance hash, planted control hashes\n"
COMMAND = re.compile(r"(?m)^[^\s]")
LAW = re.compile(r"theorem (\w+)\b")
EVIDENCE = re.compile(r"/-- (Instance|Planted) \(law `([^\x60]+)`([^)]*)\)")
DIRECTION = re.compile(r"direction (over-accepting|over-refusing)")


def digest(text):
    return sha256(" ".join(text.split()).encode()).hexdigest()


def declaration(code, end):
    """Read the command immediately following a top-level doc comment."""
    start = re.search(r"\S", code[end:])
    if start is None:
        return "", end
    first = end + start.start()
    next_command = COMMAND.search(code, first + 1)
    last = len(code) if next_command is None else next_command.start()
    return code[first:last].rstrip(), first


def collect(sources):
    laws = {}
    controls = defaultdict(lambda: defaultdict(list))
    for module, source in sources.items():
        spans = []
        code = lex(source, spans)
        namespace = re.search(r"(?m)^namespace LegalStep$", code)
        namespace_end = re.search(r"(?m)^end LegalStep$", code)
        for start, end in spans:
            doc = source[start:end]
            command, offset = declaration(code, end)
            within = namespace and namespace_end and namespace.start() < offset < namespace_end.start()
            match = LAW.match(command)
            if doc.startswith("/-- Law ") and match and (
                module == "OwnLegalStep" and within
                or module == "OwnProtocols" and (within or match[1] == "lifecycle_nonrevival")
            ):
                name = match[1]
                statement, separator, _ = command.partition(":=")
                if name in laws or not separator:
                    raise ValueError(f"ambiguous Law declaration {name}")
                laws[name] = (digest(statement), module, offset, len(statement))
                continue
            evidence = EVIDENCE.match(doc)
            if evidence and command.startswith("example :"):
                kind, name, annotation = evidence.groups()
                name = name.split(".")[-1]
                direction = DIRECTION.search(annotation)
                if kind == "Planted" and direction:
                    kind = direction[1]
                controls[name][kind].append(digest(command))

    rows = []
    for name, (statement, _, _, _) in sorted(laws.items()):
        evidence = controls[name]
        required = ("Instance", "Planted") if name in PROPERTY_LAWS else (
            "Instance", "over-accepting", "over-refusing"
        )
        if any(not evidence[kind] for kind in required):
            raise ValueError(f"{name} is missing evidence for {', '.join(required)}")
        rows.append("\t".join([name, statement, *(",".join(evidence[k]) for k in required)]))
    return HEADER + "\n".join(rows) + "\n", laws


def check(sources, expected):
    actual, laws = collect(sources)
    if actual != expected:
        raise ValueError("ownership transition statements or controls differ from the landing inventory")
    return laws


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--update", action="store_true")
    args = parser.parse_args()
    sources = {module: (PROOFS / f"{module}.lean").read_text() for module in MODULES}
    if args.update:
        GOLDEN.write_text(collect(sources)[0])
    expected = GOLDEN.read_text()
    laws = check(sources, expected)
    # Exercise the same comparison with every law independently weakened. Even
    # if its proof and examples were rewritten to elaborate, the pin must reject it.
    for name, (_, module, offset, length) in laws.items():
        source = sources[module]
        weakened = dict(sources)
        weakened[module] = source[:offset] + f"theorem {name} : True " + source[offset + length:]
        try:
            check(weakened, expected)
        except ValueError as failure:
            if "differ from the landing inventory" not in str(failure):
                raise
        else:
            raise ValueError(f"True replacement survived for {name}")
    print(f"ownership landing inventory: {len(laws)} laws pinned and True replacements rejected")


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError) as error:
        sys.exit(str(error))
