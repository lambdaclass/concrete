#!/usr/bin/env python3
"""A bug's spec ruling: which rule of docs/spec/ its record says the implementation breaks, or why none does.

The spec decides what is a bug. Every bug record's ledger block (docs/bugs/README.md, Filing a bug)
carries one `ruling:` line, in exactly one of these forms:

  ruling: Violates: OWN-1@2                  the implementation contradicts that rule (several:
                                             `Violates: OWN-1@2, REF-8@1`, the primary first)
  ruling: Spec gap: PASS — <the missing rule> no rule decides the behaviour; the fix starts by
                                             writing the rule, in the chapter of that prefix
  ruling: Open: OPEN-17                      it depends on an undecided owner question
  ruling: Permitted by: PAT-2@2              not a bug: the rule allows the behaviour
  ruling: Outside the spec: tooling — <why>  not a language behaviour: tooling (gates, reports,
                                             evidence and proof machinery), performance (compile
                                             time or generated-code speed) or library (a std
                                             function, or a missing library feature)
  ruling: Violates: unrecorded               not ruled yet: allowed only for a FIXED bug numbered
                                             below RULING_FROM, and check_bug_filing_complete.sh
                                             ratchets their count, so it only shrinks

A `Violates:`, `Open:` or `Permitted by:` form may end in ` — <note>`. A cited `ID@V` must be a key
of docs/spec/ids.tsv (append-only, so a key once there stays). An open bug may not cite a retired
version (re-rule it against the rule's current version); a fixed bug may cite any registered one,
since it was ruled against the rule of its time. `Open:` cites an OPEN-n entry with or without a
version. The chapter of a rule is the chapter whose prefix it carries, from the chapter table of
docs/spec/README.md.

  bug_rulings.py check --num NNN --open|--fixed "<ruling>"   exit 0 when the ruling is valid for
                                                              such a record, else 1 with the reason
                                                              (new_bug.sh calls it before reserving)
  bug_rulings.py value --violates ID@V | --spec-gap TEXT | --open OPEN-N | --outside TEXT
                                                              print the ruling those flags make

--root overrides the repository root (a test seam).
"""
import argparse
import os
import re
import sys

RULING_FROM = 300
OUTSIDE = ("tooling", "performance", "library")
RULE = r"[A-Z]+-D?\d+"
KEY = re.compile(r"^(" + RULE + r")@(\d+)$")
OPEN_ID = re.compile(r"^OPEN-\d+(?:@\d+)?$")
NOTE = re.compile(r"^(.*?)(?:\s+—\s+(\S.*))?$")
CHAPTER_ROW = re.compile(r"^\| `([a-z_]+\.md)` \| `([A-Z]+)-` \|")
KINDS = ("violates", "spec gap", "open", "permitted", "outside", "unrecorded")


class RulingError(Exception):
    """A malformed or unresolvable ruling: the caller refuses it."""


def load_spec(root):
    """({key: state} from docs/spec/ids.tsv, {prefix: chapter file} from docs/spec/README.md)."""
    ids_p = os.path.join(root, "docs", "spec", "ids.tsv")
    readme_p = os.path.join(root, "docs", "spec", "README.md")
    try:
        ids_text = open(ids_p, encoding="utf-8").read()
        readme = open(readme_p, encoding="utf-8").read()
    except OSError as e:
        raise RulingError(f"the spec registry is unreadable ({e}); a ruling resolves against docs/spec/ids.tsv")
    ids = {}
    for ln in ids_text.split("\n"):
        if ln and not ln.startswith("#"):
            cells = ln.split("\t")
            if len(cells) >= 2 and "@" in cells[0]:
                ids[cells[0]] = cells[1]
    chapters = {m.group(2): m.group(1) for m in map(CHAPTER_ROW.match, readme.split("\n")) if m}
    if not ids or not chapters:
        raise RulingError(f"no rule keys in {ids_p} or no chapter table in {readme_p}")
    return ids, chapters


def prefix_of(rule):
    return rule.split("-", 1)[0]


def parse(value, name):
    """The ruling's form, syntax only: {kind, rules: [ID@V or OPEN-n], chapter (spec gap), note}."""
    def split_note(rest):
        m = NOTE.match(rest.strip())
        return m.group(1).strip(), (m.group(2) or "").strip()

    if value.startswith("Violates: "):
        body, note = split_note(value[len("Violates: "):])
        if body == "unrecorded":
            if note:
                raise RulingError(f"{name}: 'Violates: unrecorded' takes no note")
            return {"kind": "unrecorded", "rules": [], "chapter": None, "note": ""}
        rules = [r.strip() for r in body.split(",")]
        bad = [r for r in rules if not KEY.match(r)]
        if bad or not rules:
            raise RulingError(f"{name}: 'Violates:' names rules as ID@VERSION (for example OWN-1@2), got '{body[:60]}'")
        if len(set(rules)) != len(rules):
            raise RulingError(f"{name}: 'Violates:' names a rule twice")
        return {"kind": "violates", "rules": rules, "chapter": None, "note": note}
    if value.startswith("Permitted by: "):
        body, note = split_note(value[len("Permitted by: "):])
        rules = [r.strip() for r in body.split(",")]
        if not rules or any(not KEY.match(r) for r in rules):
            raise RulingError(f"{name}: 'Permitted by:' names rules as ID@VERSION, got '{body[:60]}'")
        return {"kind": "permitted", "rules": rules, "chapter": None, "note": note}
    if value.startswith("Open: "):
        body, note = split_note(value[len("Open: "):])
        rules = [r.strip() for r in body.split(",")]
        if not rules or any(not OPEN_ID.match(r) for r in rules):
            raise RulingError(f"{name}: 'Open:' names owner questions as OPEN-N, got '{body[:60]}'")
        return {"kind": "open", "rules": rules, "chapter": None, "note": note}
    if value.startswith("Spec gap: "):
        m = re.match(r"^([A-Z]+) — (\S.*)$", value[len("Spec gap: "):])
        if not m:
            raise RulingError(f"{name}: a spec gap is 'Spec gap: PREFIX — <the missing rule>' (PREFIX the "
                              f"chapter it belongs to, such as OWN), got '{value[:70]}'")
        return {"kind": "spec gap", "rules": [], "chapter": m.group(1), "note": m.group(2)}
    if value.startswith("Outside the spec: "):
        m = re.match(r"^(\w+) — (\S.*)$", value[len("Outside the spec: "):])
        if not m or m.group(1) not in OUTSIDE:
            raise RulingError(f"{name}: 'Outside the spec: REASON — <why>' takes a reason of {', '.join(OUTSIDE)}, "
                              f"got '{value[:70]}'")
        return {"kind": "outside", "rules": [], "chapter": None, "note": m.group(2), "reason": m.group(1)}
    raise RulingError(f"{name}: a ruling is one of 'Violates: ID@V', 'Spec gap: PREFIX — <rule>', 'Open: OPEN-N', "
                      f"'Permitted by: ID@V', 'Outside the spec: {'|'.join(OUTSIDE)} — <why>' or (a fixed bug "
                      f"below {RULING_FROM} only) 'Violates: unrecorded'; got '{value[:60]}'")


def resolve(ruling, spec, name, num, is_open):
    """Refuse a ruling that names an unregistered rule, an unknown chapter, or is not allowed for
    this record. Returns the ruling with `chapters` (the chapter prefixes it counts under)."""
    ids, chapters = spec
    k = ruling["kind"]
    if k == "unrecorded":
        if is_open:
            raise RulingError(f"{name}: an open bug states its ruling; 'Violates: unrecorded' is only for "
                              f"fixed bugs filed before the rulings")
        if int(num) >= RULING_FROM:
            raise RulingError(f"{name}: every bug from {RULING_FROM} on states its ruling; 'Violates: unrecorded' "
                              f"is only for older fixed bugs")
    for r in ruling["rules"]:
        if k == "open":
            base = r.split("@")[0]
            known = (r in ids) if "@" in r else any(key.split("@")[0] == base for key in ids)
            if not known:
                raise RulingError(f"{name}: '{r}' is not an owner question of docs/spec/ids.tsv")
            continue
        if r not in ids:
            known = sorted(key for key in ids if key.split("@")[0] == r.split("@")[0])
            raise RulingError(f"{name}: '{r}' is not a rule version of docs/spec/ids.tsv"
                              + (f" (registered: {', '.join(known)})" if known else " (no such rule)"))
        if prefix_of(r) == "OPEN":
            raise RulingError(f"{name}: {r} is an owner question; cite it with 'Open: {r.split('@')[0]}'")
        if is_open and ids[r].startswith("retired"):
            raise RulingError(f"{name}: {r} is {ids[r]}; an open bug is ruled against the rule's current version")
    if k == "spec gap" and ruling["chapter"] == "OPEN":
        raise RulingError(f"{name}: OPEN holds owner questions, not rules; a gap names the chapter its rule belongs to")
    if k == "spec gap" and ruling["chapter"] not in chapters:
        raise RulingError(f"{name}: spec gap chapter '{ruling['chapter']}' is not a chapter prefix of "
                          f"docs/spec/README.md ({', '.join(sorted(chapters))})")
    out = dict(ruling)
    if k == "spec gap":
        out["chapters"] = [ruling["chapter"]]
    else:
        out["chapters"] = list(dict.fromkeys(prefix_of(r) for r in ruling["rules"]))
    return out


def label(ruling):
    """A short display form: `violates OWN-1@2`, `spec gap (PASS)`, `open OPEN-17`, ..."""
    k = ruling["kind"]
    if k in ("violates", "permitted", "open"):
        return f"{'permitted by' if k == 'permitted' else k} {', '.join(ruling['rules'])}"
    if k == "spec gap":
        return f"spec gap ({ruling['chapter']})"
    if k == "outside":
        return f"outside the spec ({ruling['reason']})"
    return "unrecorded"


BEGIN = ("<!-- BEGIN GENERATED: spec rulings (scripts/gen/bug_ledger.py; edit the records' ruling lines) -->")
END = "<!-- END GENERATED: spec rulings -->"


def _cell(text):
    return text.replace("|", "\\|")


def _none(out):
    """A table that got no rows gets one row of dashes, so it still renders as a table."""
    if out[-1].startswith("|--") or out[-1].startswith("|---"):
        out.append("| " + " | ".join("—" for _ in out[-1].strip("|").split("|")) + " |")


def render_view(recs, chapters):
    """docs/bugs/RULINGS.md's generated block: counts by kind and chapter, every violated rule with
    its bugs, the spec gaps (rules to write) and the open owner questions. `recs` maps NNN to a
    record whose facts carry the resolved `ruling`."""
    rul = {n: r["facts"]["ruling"] for n, r in recs.items()}
    silent = {n for n, r in recs.items() if r["facts"].get("silent") == "yes"}
    opn = {n for n, r in recs.items() if r["open"]}

    def nums(ns):
        return ", ".join((f"**{n}**" if n in opn else n) for n in sorted(ns)) or "—"

    def count(kind):
        return sum(1 for v in rul.values() if v["kind"] == kind)

    out = [BEGIN, ""]
    ruled = len(rul) - count("unrecorded")
    out.append(f"{len(rul)} bug records, {ruled} of them ruled: violates a rule {count('violates')}, spec gap "
               f"{count('spec gap')}, open owner question {count('open')}, permitted by a rule {count('permitted')}, "
               f"outside the spec {count('outside')}; not ruled yet (fixed bugs filed before bug {RULING_FROM}) "
               f"{count('unrecorded')}. Open bugs are in bold.")
    out += ["", "### By kind", "", "| ruling | bugs | open | silent |", "|---|--:|--:|--:|"]
    for kind, title in (("violates", "violates a rule"), ("spec gap", "spec gap"), ("open", "open owner question"),
                        ("permitted", "permitted by a rule (not a bug)"), ("outside", "outside the spec"),
                        ("unrecorded", "not ruled yet")):
        ns = [n for n, v in rul.items() if v["kind"] == kind]
        out.append(f"| {title} | {len(ns)} | {sum(n in opn for n in ns)} | {sum(n in silent for n in ns)} |")
    out += ["", "### By chapter", "",
            "Every bug counts once under each chapter its ruling names: the rules it violates, the owner questions "
            "it waits on, the chapter a spec gap belongs to, or the rule that permits it.", "",
            "| chapter | prefix | bugs | open | silent | violates | spec gap | open question | permitted |",
            "|---|---|--:|--:|--:|--:|--:|--:|--:|"]
    by_ch = {}
    for n, v in rul.items():
        for c in v.get("chapters", []):
            by_ch.setdefault(c, []).append(n)
    order = list(chapters)
    for c in sorted(by_ch, key=lambda c: (-len(by_ch[c]), order.index(c) if c in order else len(order), c)):
        ns = by_ch[c]
        k = lambda kind: sum(1 for n in ns if rul[n]["kind"] == kind)  # noqa: E731
        out.append(f"| {chapters.get(c, '?')} | {c} | {len(ns)} | {sum(n in opn for n in ns)} | "
                   f"{sum(n in silent for n in ns)} | {k('violates')} | {k('spec gap')} | {k('open')} | {k('permitted')} |")
    _none(out)
    out += ["", "### By violated rule", "", "| rule | chapter | bugs | which |", "|---|---|--:|---|"]
    by_rule = {}
    for n, v in rul.items():
        if v["kind"] == "violates":
            for r in v["rules"]:
                by_rule.setdefault(r, []).append(n)
    for r in sorted(by_rule, key=lambda r: (-len(by_rule[r]), r)):
        out.append(f"| {r} | {chapters.get(prefix_of(r), '?')} | {len(by_rule[r])} | {nums(by_rule[r])} |")
    _none(out)
    out += ["", "### Spec gaps: rules to write", "",
            "No rule decides these bugs' behaviour. Each fix starts by writing the rule (spec work, owned by "
            "docs/spec/); bugs that need the same rule share one row.", "",
            "| chapter | the missing rule | bugs |", "|---|---|---|"]
    gaps = {}
    for n, v in rul.items():
        if v["kind"] == "spec gap":
            gaps.setdefault((v["chapter"], v["note"]), []).append(n)
    for (c, note), ns in sorted(gaps.items(), key=lambda kv: (-len(kv[1]), kv[0])):
        out.append(f"| {c} | {_cell(note)} | {nums(ns)} |")
    _none(out)
    out += ["", "### Open owner questions", "", "| question | bugs |", "|---|---|"]
    qs = {}
    for n, v in rul.items():
        if v["kind"] == "open":
            for q in v["rules"]:
                qs.setdefault(q.split("@")[0], []).append(n)
    for q in sorted(qs, key=lambda q: int(q.split("-")[1])):
        out.append(f"| {q} | {nums(qs[q])} |")
    _none(out)
    out += ["", "### Permitted by a rule (not a bug)", "", "| rule | bugs |", "|---|---|"]
    per = {}
    for n, v in rul.items():
        if v["kind"] == "permitted":
            for r in v["rules"]:
                per.setdefault(r, []).append(n)
    for r in sorted(per):
        out.append(f"| {r} | {nums(per[r])} |")
    _none(out)
    out += ["", "### Outside the spec", "", "| reason | bugs |", "|---|---|"]
    for reason in OUTSIDE:
        ns = [n for n, v in rul.items() if v["kind"] == "outside" and v["reason"] == reason]
        out.append(f"| {reason} | {nums(ns)} |")
    _none(out)
    out += ["", END]
    return "\n".join(out)


def build_value(a):
    given = [x for x in (a.violates, a.spec_gap, a.open, a.outside) if x is not None]
    if len(given) != 1:
        raise RulingError("give exactly one of --violates, --spec-gap, --open and --outside")
    if a.violates is not None:
        return f"Violates: {a.violates}"
    if a.spec_gap is not None:
        return f"Spec gap: {a.spec_gap}"
    if a.open is not None:
        return f"Open: {a.open}"
    return f"Outside the spec: {a.outside}"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("mode", choices=["check", "value"])
    ap.add_argument("ruling", nargs="?")
    ap.add_argument("--root", default=os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")))
    ap.add_argument("--num")
    g = ap.add_mutually_exclusive_group()
    g.add_argument("--open-bug", dest="is_open", action="store_true", default=None)
    g.add_argument("--fixed", dest="is_open", action="store_false")
    ap.add_argument("--violates")
    ap.add_argument("--spec-gap")
    ap.add_argument("--open")
    ap.add_argument("--outside")
    a = ap.parse_args()
    try:
        if a.mode == "value":
            v = build_value(a)
            if "\n" in v or "\t" in v or "-->" in v:
                raise RulingError("a ruling is one line with no tab and no '-->'")
            print(v)
            return 0
        if a.ruling is None or a.num is None or a.is_open is None:
            ap.error("check takes the ruling, --num NNN and --open-bug or --fixed")
        if not re.fullmatch(r"\d{3}", a.num):
            raise RulingError("--num takes a three-digit bug number")
        resolve(parse(a.ruling, f"bug {a.num}"), load_spec(a.root), f"bug {a.num}", a.num, a.is_open)
    except RulingError as e:
        print(f"bug ruling: {e}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
