#!/usr/bin/env python3
"""Validate the Lean export's closed schema, rule documentation and conformance links.
No source text supplies rule or example facts. Decision inventories declare documentation rows.
"""
import json, re, sys
ROW = re.compile(r"\b(Q\d+(?:\.\d+)+|OD\d+|P\d+[a-z]?|R-\d{4})\b")
# Inventory parsing is deliberately conservative; see ISS-0083 and authority_status.md.
ANCHOR = re.compile(r'<a\s+id="([^"\n]+)"\s*></a>')
CITATION = re.compile(r"(?:(?P<path>[^\s`<>\[\](){},;:\"'#]+)#(?P<fragment>[^\s`<>\[\](){},;:\"']*)|(?<![\w#])#(?P<bare>[A-Za-z0-9][^\s`<>\[\](){},;:\"']*))")
NONCURRENT = re.compile(r'\b(?:withdrawn|superseded|not current|partial|queued?|reopen(?:ed|ing)?|held|hold|deferred|undecided)\b', re.I)


def authority_entries(text):
    """One status map for rows and fragments, based on delimited entries and provenance.

    Markdown emphasis delimits entries, never establishes their status. See the
    documented rule for provenance inheritance and the retained recovery boundary.
    """
    # Quoted recommendations are data, not inventory structure. Preserve offsets
    # and newlines while hiding fenced contents from the structural reader.
    lines = text.splitlines(keepends=True)
    fence = None
    for i, line in enumerate(lines):
        marker = re.match(r'^[ \t]*(`{3,}|~{3,})', line)
        if marker and fence is None:
            fence = marker[1][0]
            lines[i] = re.sub(r'[^\n]', ' ', line)
        elif fence is not None:
            lines[i] = re.sub(r'[^\n]', ' ', line)
            if marker and marker[1][0] == fence:
                fence = None
    text = ''.join(lines)
    result = {m.group(1): 'non-authority status: not a decision entry' for m in ANCHOR.finditer(text)}
    # Blocks are headings or list entries. An anchor may be on the preceding line.
    blocks = list(re.finditer(
        r'^(?P<indent>[ \t]*)(?:(?:- |\d+\. )?(?:<a id="(?P<anchor>[^"\n]+)"></a>\s*)?'
        r'(?P<title>\*\*[^\n]+|\*[^\n]+|#{1,6} [^\n]+))', text, re.M))
    retained_section = text.split('Already recovered authority, independently checked; existing records retained.', 1)
    retained = set()
    if len(retained_section) == 2:
        table = retained_section[1].split('\n- <a ', 1)[0]
        retained = set(re.findall(r'\]\(#([^)]*)\)', table))
    waiting_section = text.split('## Waiting for the owner', 1)
    waiting = waiting_section[1].split('\n## ', 1)[0] if len(waiting_section) == 2 else ''
    waiting_anchors = set()
    for item in re.split(r'\n\d+\. ', waiting):
        if not item.lstrip().startswith('Closed '):
            waiting_anchors.update(re.findall(r'\]\(#([^)]*)\)', item))
    for i, m in enumerate(blocks):
        title = m['title']; anchor = m['anchor']
        row = re.match(r'\*\*(Q\d+(?:\.\d+)+|P\d+[a-z]?)\*\*', title)
        keys = ([anchor] if anchor else []) + ([row[1]] if row else [])
        if not keys:
            continue
        end = blocks[i+1].start() if i+1 < len(blocks) else len(text)
        body = text[m.start():end]
        # Provenance can be inherited only from the enclosing heading or list entry.
        context = body
        indent = len(m['indent'])
        for j in range(i - 1, -1, -1):
            prev = blocks[j]
            if prev['title'].startswith('#') or len(prev['indent']) < indent:
                # Only the parent's introduction, never intervening siblings, can
                # supply provenance. Continue outward for a nested list's owner.
                context += text[prev.start():blocks[j+1].start()]
                if prev['title'].startswith('#'):
                    break
                indent = len(prev['indent'])
        provenance = bool(re.search(r'owner(?:[- ](?:approved|confirmed)|[, :])|Owner reply(?:,|:)|owner answered every row', context, re.I))
        # Status markers belong to the entry title, not arbitrary language examples
        # ("deferred scopes", "partial moves") or references to other decisions.
        if title.startswith('#'):
            status_text = title
        else:
            delimiter = '**' if title.startswith('**') else '*'
            status_text = title[len(delimiter):].split(delimiter, 1)[0]
        status = NONCURRENT.search(status_text)
        nonentry = re.search(r'primary approvals recovered|authority recovery|recovery.*(?:list|items)|revision register|review implementation|source rules and authority limits|the \d+(?: answers|-item list)|^(?:Core rows|Project rows)', status_text, re.I)
        if title.startswith('#') and not re.match(r'#{1,6} (?:Decided|Owner decisions) \d{4}-', title):
            reason = 'non-authority status: section wrapper'
        elif nonentry:
            reason = 'non-authority status: inventory or status record'
        elif anchor in waiting_anchors:
            reason = 'non-authority status: waiting for owner'
        elif (qualification := re.search(
                r'\b(?:is|remains?|status:)\s+(?:explicitly\s+)?'
                r'(deferred|held|queued|partial|superseded|withdrawn|reopened|not current)\b',
                body, re.I)):
            reason = 'non-authority status: ' + qualification[1].lower()
        elif status:
            reason = 'non-authority status: ' + status[0].lower()
        elif anchor and anchor.startswith('recovered-') and anchor not in retained:
            reason = 'non-authority status: recovery currentness not established (ISS-0083)'
        elif anchor in retained:
            reason = None
        elif not provenance:
            reason = 'non-authority status: owner provenance not established (ISS-0083)'
        else:
            reason = None
        for key in keys:
            result[key] = reason
    # OD table disposition, rather than its historical question/options, owns status.
    for line in text.splitlines():
        if not re.match(r'\| OD\d+ \|', line):
            continue
        cells = re.split(r'(?<!\\)\|', line)
        row = cells[1].strip(); disposition = cells[-2].strip()
        status = re.search(r'\b(?:withdrawn|superseded|reopened|not current)\b', disposition, re.I) or re.match(r'(held|queued|partial)\b', disposition, re.I)
        reason = ('non-authority status: ' + status[0].lower() if status else
                  None if re.match(r'decided .*owner', disposition) else
                  'non-authority status: OD disposition unclear (ISS-0083)')
        result[row] = result[row.lower()] = reason
    return result


def decision_citations(doc):
    """Fragments require a token; path# also captures a missing fragment explicitly."""
    context = False
    for match in CITATION.finditer(doc):
        path = match['path'] or ('docs/decisions.md' if context else '')
        anchor = match['fragment'] if match['path'] else match['bare']
        if anchor.endswith('.') and not anchor.endswith('..'):
            anchor = anchor[:-1]
        if path == 'docs/decisions.md':
            context = True
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
            if not rows and not citations:
                fails.append(f"rule {r['name']}'s doc comment cites no decision row or authority entry")
            for row in rows:
                if row.startswith('R-'):
                    if citations or any(not x.startswith('R-') for x in rows):
                        continue
                    fails.append(f"rule {r['name']}: non-authority status: ROADMAP task {row} decides nothing")
                elif row not in approved:
                    fails.append(f"rule {r['name']}: no existing decision row {row}")
                elif approved[row]:
                    fails.append(f"rule {r['name']}: {row}: {approved[row]}")
            for path, anchor in citations:
                if path != 'docs/decisions.md':
                    fails.append(f"rule {r['name']}: wrong authority path {path}#{anchor}")
                elif anchor not in approved:
                    fails.append(f"rule {r['name']}: missing authority anchor {anchor or '(empty fragment)'}")
                elif approved[anchor]:
                    fails.append(f"rule {r['name']}: {anchor}: {approved[anchor]}")
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
