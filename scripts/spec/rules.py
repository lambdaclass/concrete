#!/usr/bin/env python3
"""The rule gate's checks (scripts/spec/rules.sh): the registered rules against the sources.

  rules.py RULES.jsonl SRC_DIR   RULES.jsonl is scripts/spec/rules.lean's output; SRC_DIR holds the
                                 specification's .lean sources (searched recursively).

Prints `ok`/`FAIL` lines; exits 1 on any failure, 3 when an input cannot be read or lexed.
A rule's doc comment must cite a decision row; each rule needs an example whose doc comment says
``Accepted (rule `Name`)`` and one that says ``Refused (rule `Name`)``; every ``(rule `Name`)``
anywhere in a source (either kind of comment, or a literal) must name a registered rule; no rule is registered twice; revisions are >= 1.
"""
import json, os, re, sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "proofs"))
import lean_lex  # noqa: E402

ROW = re.compile(r"\b(Q\d+(\.\d+)+|OD\d+|P\d+|R-\d{4})\b")
CITE = re.compile(r"\(rule `([^`\s]+)`\)")
VERDICT = re.compile(r"\b(Accepted|Refused) \(rule `([^`\s]+)`\)")
EXAMPLE_AFTER = re.compile(r"\s*example(?![\w'!?])")


def main(rules_path, src_dir):
    fails = []
    rules = [json.loads(l) for l in open(rules_path, encoding="utf-8") if l.strip()]
    if not rules or rules[-1] != {"end": True}:
        print("error: the rule listing is truncated (no end record)", file=sys.stderr); return 3
    rules = rules[:-1]
    names = [r["name"] for r in rules]
    for n in sorted({n for n in names if names.count(n) > 1}):
        fails.append(f"rule {n} is registered more than once")
    for r in rules:
        if not r["exists"]: fails.append(f"rule {r['name']} names no declaration")
        if r["rev"] < 1: fails.append(f"rule {r['name']} has revision {r['rev']} (revisions start at 1)")
        if not r["doc"].strip(): fails.append(f"rule {r['name']} has no doc comment")
        elif not ROW.search(r["doc"]): fails.append(f"rule {r['name']}'s doc comment cites no decision row")
    seen = {n: set() for n in names}
    for dirpath, _, files in os.walk(src_dir):
        for f in sorted(files):
            if not f.endswith(".lean"): continue
            path = os.path.join(dirpath, f)
            try:
                src = open(path, encoding="utf-8").read(); spans = []
                lean_lex.lex(src, spans)
            except (OSError, UnicodeDecodeError, lean_lex.LexError) as e:
                print(f"error: {path}: {e}", file=sys.stderr); return 3
            # citations anywhere in the file (comments of both kinds, and literals: fail closed)
            for m in CITE.finditer(src):
                if m.group(1) not in seen:
                    line = src.count("\n", 0, m.start()) + 1
                    fails.append(f"{path}:{line}: cites unregistered rule {m.group(1)}")
            for a, b in spans:
                text, line = src[a:b], src.count("\n", 0, a) + 1
                verdicts = VERDICT.findall(text)
                if not verdicts: continue
                if not (text.startswith("/--") and EXAMPLE_AFTER.match(src, b)):
                    fails.append(f"{path}:{line}: a verdict citation outside an example's doc comment")
                    continue
                for v, n in verdicts:
                    if n in seen: seen[n].add(v)
    for n in names:
        for v in ("Accepted", "Refused"):
            if v not in seen[n]: fails.append(f"rule {n} has no example marked {v} (rule `{n}`)")
    for f in fails: print(f"  FAIL {f}")
    if not fails: print(f"  ok   {len(rules)} registered rules, each documented with a decision row and"
                        " an accepted and a refused example; every citation names a registered rule")
    return 1 if fails else 0


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(f"usage: {sys.argv[0]} RULES.jsonl SRC_DIR", file=sys.stderr); sys.exit(2)
    sys.exit(main(sys.argv[1], sys.argv[2]))
