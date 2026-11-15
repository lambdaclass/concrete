#!/usr/bin/env python3
"""Validate the Lean export's closed schema, rule documentation and conformance links.
No source text supplies rule or example facts. Decision inventories declare documentation rows.
"""
import json, re, sys
ROW = re.compile(r"\b(Q\d+(?:\.\d+)+|OD\d+|P\d+[a-z]?|R-\d{4})\b")
DECLARED = re.compile(r"^\s*(?:- \*\*(\S+?)\*\*|\| *(\S+?) *\|)", re.M)


# Only the decision inventory owns authority; other row inventories cannot add anchors.
ANCHOR = re.compile(r'<a\s+id="([^"\n]+)"\s*></a>')
CITATION = re.compile(r"([^\s`<>\[\](){},;:\"']*)#([^\s`<>\[\](){},;:\"']*)")

NONCURRENT = re.compile(
    r"\b(?:open|undecided|held|hold|partial|queued?|reopen(?:ed|ing)?|superseded|not current)\b")
RECOVERY_WRAPPER = re.compile(r"\b(?:recovery|recovered)\b|\bthe \d+.*(?:answers|list)\b")


def authority_entries(text):
    """Read current status from each entry's own heading, not a parallel anchor list.

    Recovered item headings explicitly distinguish RECOVERED from PARTIAL and
    superseded/not-current evidence. The decided-* namespace identifies decision
    records, but recovery wrappers, queues and reopening notices are not decisions.
    This validates provenance identity, not the scope of a cited language claim.
    """
    entries = set()
    for match in ANCHOR.finditer(text):
        anchor = match.group(1)
        tail = text[match.end():]
        heading = re.match(r"\s*(?:\*\*([^*]+)\*\*|#{1,6} ([^\n]+))", tail)
        if heading is None:
            continue
        status = (heading.group(1) or heading.group(2)).lower()
        if NONCURRENT.search(status) or RECOVERY_WRAPPER.search(status):
            # A fully recovered individual item is current unless its own status
            # explicitly records a qualification that withholds current authority.
            if (re.fullmatch(r'recovered-\d+-\d+', anchor) and 'recovered' in status
                    and not NONCURRENT.search(status)):
                entries.add(anchor)
        elif anchor.startswith('decided-'):
            entries.add(anchor)
    return entries


def decision_citations(doc):
    """Consume the complete fragment token; allow one sentence-ending period.

    Existing landed comments abbreviate subsequent fragments after an explicit
    docs/decisions.md reference. Preserve those citations without rewriting them.
    A bare fragment alone or one attached to another path has no owning inventory.
    """
    for match in CITATION.finditer(doc):
        path, anchor = match.groups()
        if anchor.endswith('.') and not anchor.endswith('..'):
            anchor = anchor[:-1]
        if not path and 'docs/decisions.md#' in doc[:match.start()]:
            path = 'docs/decisions.md'
        yield path, anchor


def unique_object(pairs):
    """JSON must not silently overwrite an earlier field with a later one."""
    obj = {}
    for key, value in pairs:
        if key in obj:
            raise ValueError(f"duplicate JSON field: {key}")
        obj[key] = value
    return obj


def main(rules_path, row_paths):
    fails = []
    try:
        rules = [json.loads(l, object_pairs_hook=unique_object) for l in open(rules_path, encoding="utf-8") if l.strip()]
        inventories = [open(p, encoding="utf-8").read() for p in row_paths]
    except (OSError, UnicodeDecodeError, ValueError) as e:
        print(f"error: {e}", file=sys.stderr); return 3
    if not rules or type(rules[-1]) is not dict or rules[-1] != {"end": True} or type(rules[-1].get("end")) is not bool:
        print("error: the rule listing is truncated (no end record)", file=sys.stderr); return 3
    if not inventories:
        print("error: no decision inventory given", file=sys.stderr); return 3
    records = rules[:-1]
    schemas = {
        "rule": {"kind": str, "name": str, "rev": int, "exists": bool, "doc": str},
        "example": {"kind": str, "rule": str, "name": str, "verdict": str,
                    "elaborated": bool, "usesRule": bool},
    }
    for r in records:
        schema = schemas.get(r.get("kind")) if type(r) is dict and type(r.get("kind")) is str else None
        if (schema is None or set(r) != set(schema)
                or any(type(r[k]) is not t for k, t in schema.items())
                or not r["name"].strip()
                or (r["kind"] == "rule" and r["rev"] < 0)
                or (r["kind"] == "example" and
                    (not r["rule"].strip() or r["verdict"] not in ("Accepted", "Refused", "Invalid")))):
            print("error: malformed registry record (closed rule/example schema required)", file=sys.stderr)
            return 3
    rules = [r for r in records if r["kind"] == "rule"]
    examples = [r for r in records if r["kind"] == "example"]
    declared = {a or b for t in inventories for a, b in DECLARED.findall(t)}
    approved = authority_entries(inventories[0])
    names = [r["name"] for r in rules]
    for n in sorted({n for n in names if names.count(n) > 1}):
        fails.append(f"rule {n} is registered more than once")
    for r in rules:
        if not r["exists"]: fails.append(f"rule {r['name']} names no declaration")
        if r["rev"] < 1: fails.append(f"rule {r['name']} has revision {r['rev']} (revisions start at 1)")
        if not r["doc"].strip(): fails.append(f"rule {r['name']} has no doc comment")
        else:
            rows = [m.group(1) for m in ROW.finditer(r["doc"])]
            citations = list(decision_citations(r["doc"]))
            # A row-only citation retains the existing rule: at least one declared
            # row suffices. Fragment citations, if present, must ALL be current.
            if not rows and not citations:
                fails.append(f"rule {r['name']}'s doc comment cites no decision row or authority entry")
            elif rows and not any(x in declared for x in rows):
                fails.append(f"rule {r['name']}'s doc comment cites no existing decision row ({', '.join(rows)})")
            for path, anchor in citations:
                if path != 'docs/decisions.md' or anchor not in approved:
                    fails.append(f"rule {r['name']}'s doc comment cites no decided authority entry for {path}#{anchor}")
    seen = {n: set() for n in names}
    identities = set()
    for e in examples:
        n, identity = e["rule"], e["name"]
        if identity in identities: fails.append(f"example {identity} is linked more than once")
        identities.add(identity)
        if n not in seen: fails.append(f"example {identity} cites unregistered rule {n}")
        if not e["elaborated"]: fails.append(f"example {identity} is not an elaborated trusted theorem")
        if e["verdict"] == "Invalid": fails.append(f"example {identity} has no concrete rule acceptance/refusal proposition")
        if not e["usesRule"]: fails.append(f"example {identity} does not use linked rule {n}")
        if n in seen and e["elaborated"] and e["usesRule"]: seen[n].add(e["verdict"])
    for n in names:
        for v in ("Accepted", "Refused"):
            if v not in seen[n]: fails.append(f"rule {n} has no example marked {v} (rule `{n}`)")
    for f in fails: print(f"  FAIL {f}")
    if not fails: print(f"  ok   {len(rules)} registered rules, {len(examples)} elaborated example links")
    return 1 if fails else 0


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(f"usage: {sys.argv[0]} RULES.jsonl ROWS...", file=sys.stderr); sys.exit(2)
    sys.exit(main(sys.argv[1], sys.argv[2:]))
