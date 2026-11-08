#!/usr/bin/env python3
"""The rule gate's checks (scripts/spec/rules.sh): the registered rules against the sources.

  rules.py RULES.jsonl SRC_DIR ROWS...
      RULES.jsonl is scripts/spec/rules.lean's output; SRC_DIR holds the specification's .lean
      sources (searched recursively, at least one); each ROWS file is a decision inventory
      (docs/decisions.md, ROADMAP.md) in which a cited decision row must appear.

Prints `ok`/`FAIL` lines; exits 1 on any failure, 3 when an input cannot be read or lexed.
A rule's doc comment must cite a decision row that exists in an inventory; each rule needs one
example whose doc comment says ``Accepted (rule `Name`)`` and a different one whose doc comment
says ``Refused (rule `Name`)``; a verdict doc comment holds no nested comment and no rule's two
verdicts at once; every ``(rule `Name`)`` anywhere in a source (either kind of comment, or a
literal) must name a registered rule; no rule is registered twice; revisions are >= 1.
"""
import json, os, re, sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "proofs"))
import lean_lex  # noqa: E402

ROW = re.compile(r"\b(Q\d+(?:\.\d+)+|OD\d+|P\d+[a-z]?|R-\d{4})\b")
# A rule name is a Lean name as `toString` prints it: dotted components, a component that needs
# quoting written «…» (which may hold spaces). Anything up to the closing backtick is the name.
CITE = re.compile(r"\(rule `([^`\n]+)`\)")
VERDICT = re.compile(r"\b(Accepted|Refused) \(rule `([^`\n]+)`\)")
# What may stand between a doc comment and the `example` it documents: attributes and modifiers.
EXAMPLE_AFTER = re.compile(
    r"\s*(?:@\[[^\]]*\]\s*)*(?:(?:noncomputable|private|protected|unsafe|partial)\s+)*"
    r"example(?![\w'!?])")


def row_exists(row, inventories):
    pat = re.compile(r"(?<![\w.-])" + re.escape(row) + r"(?![\w-]|\.\d)")
    return any(pat.search(t) for t in inventories)


def main(rules_path, src_dir, row_paths):
    fails = []
    try:
        rules = [json.loads(l) for l in open(rules_path, encoding="utf-8") if l.strip()]
        inventories = [open(p, encoding="utf-8").read() for p in row_paths]
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as e:
        print(f"error: {e}", file=sys.stderr); return 3
    if not rules or rules[-1] != {"end": True}:
        print("error: the rule listing is truncated (no end record)", file=sys.stderr); return 3
    if not inventories:
        print("error: no decision inventory given", file=sys.stderr); return 3
    if not os.path.isdir(src_dir):
        print(f"error: {src_dir}: not a directory", file=sys.stderr); return 3
    rules = rules[:-1]
    names = [r["name"] for r in rules]
    for n in sorted({n for n in names if names.count(n) > 1}):
        fails.append(f"rule {n} is registered more than once")
    for r in rules:
        if not r["exists"]: fails.append(f"rule {r['name']} names no declaration")
        if r["rev"] < 1: fails.append(f"rule {r['name']} has revision {r['rev']} (revisions start at 1)")
        if not r["doc"].strip(): fails.append(f"rule {r['name']} has no doc comment")
        else:
            rows = [m.group(1) for m in ROW.finditer(r["doc"])]
            if not rows: fails.append(f"rule {r['name']}'s doc comment cites no decision row")
            elif not any(row_exists(x, inventories) for x in rows):
                fails.append(f"rule {r['name']}'s doc comment cites no existing decision row ({', '.join(rows)})")
    seen = {n: set() for n in names}
    scanned = 0

    def walk_error(e):
        raise e
    try:
        tree = sorted(os.walk(src_dir, onerror=walk_error))
    except OSError as e:
        print(f"error: {src_dir}: {e}", file=sys.stderr); return 3
    for dirpath, _, files in tree:
        for f in sorted(files):
            if not f.endswith(".lean"): continue
            path = os.path.join(dirpath, f)
            try:
                src = open(path, encoding="utf-8").read(); spans = []
                lean_lex.lex(src, spans)
            except (OSError, UnicodeDecodeError, lean_lex.LexError) as e:
                print(f"error: {path}: {e}", file=sys.stderr); return 3
            scanned += 1
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
                if "/-" in text[3:]:
                    fails.append(f"{path}:{line}: a verdict doc comment holds a nested comment")
                    continue
                kinds = {}
                for v, n in verdicts: kinds.setdefault(n, set()).add(v)
                both = sorted(n for n, k in kinds.items() if len(k) > 1)
                if both:
                    fails.append(f"{path}:{line}: one example is both accepted and refused by {', '.join(both)}")
                    continue
                for n, k in kinds.items():
                    if n in seen: seen[n] |= k
    if scanned == 0:
        print(f"error: {src_dir}: no .lean source", file=sys.stderr); return 3
    for n in names:
        for v in ("Accepted", "Refused"):
            if v not in seen[n]: fails.append(f"rule {n} has no example marked {v} (rule `{n}`)")
    for f in fails: print(f"  FAIL {f}")
    if not fails: print(f"  ok   {len(rules)} registered rules in {scanned} sources, each documented with an"
                        " existing decision row and an accepted and a refused example; every citation"
                        " names a registered rule")
    return 1 if fails else 0


if __name__ == "__main__":
    if len(sys.argv) < 4:
        print(f"usage: {sys.argv[0]} RULES.jsonl SRC_DIR ROWS...", file=sys.stderr); sys.exit(2)
    sys.exit(main(sys.argv[1], sys.argv[2], sys.argv[3:]))
