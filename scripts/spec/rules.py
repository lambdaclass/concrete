#!/usr/bin/env python3
"""Validate the Lean export's closed schema, rule documentation and conformance links.
No source text supplies rule or example facts. Decision inventories declare documentation rows.
"""
import json
from collections import Counter
import posixpath
from urllib.parse import urlsplit, unquote
import re
import sys
from markdown_it import MarkdownIt
ROW = re.compile(r"\b(Q\d+(?:\.\d+)+|OD\d+|P\d+[a-z]?|R-\d{4})\b")
DECLARED = re.compile(r"^\s*(?:- \*\*(\S+?)\*\*|\| *(\S+?) *\|)", re.M)
ANCHOR = re.compile(r'<a\s+id="([^"\n]+)"\s*></a>')
CITATION = re.compile(
    r"(?:(?P<path>[^\s`<>\[\](){},;:\"'\ufffc#]+)#"
    r"(?P<fragment>[^\s`<>\[\](){},;:\"']*)|"
    r"(?<![\w#\ufffc])#(?P<bare>[A-Za-z0-9][^\s`<>\[\](){},;:\"']*))")
DISPOSITION = re.compile(r'\b(?:held|partial|queued|reopened|superseded|withdrawn|deferred)\b', re.I)
# Only the inventory's literal utterance header and transcript locator qualify.
# In particular, prose such as "Owner reply is missing" is not a header.
OWNER_HEADER = re.compile(
    r'^Owner (?:reply|message)(?: \(user\))?'
    r'(?:, L\d+, \d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+Z)?:')
UNRECORDED = ('authority status not machine-readable; cite a row or an entry with '
              'recorded owner approval (ISS-0083)')


# CommonMark defines block and inline boundaries. The table extension is used only
# for the inventory's explicit retained-record table, not to infer prose status.
MARKDOWN = MarkdownIt("commonmark").enable("table")


def inline_content(tokens):
    """Text of known inline tokens; authority titles are separately allowlisted."""
    return ''.join(t.content if t.type in ('text', 'code_inline') else
                   '\n' if t.type in ('softbreak', 'hardbreak') else '' for t in tokens)


def text_content(doc):
    """Preserve inline boundaries: omitted constructs cannot create a word boundary."""
    return '\n'.join(''.join(c.content if c.type == 'text' else
                              '' if c.type in ('em_open', 'em_close', 'strong_open', 'strong_close')
                              else '\n' if c.type in ('softbreak', 'hardbreak') else '\ufffc'
                              for c in t.children or [])
                     for t in MARKDOWN.parse(doc) if t.type == 'inline')


def title_error(tokens):
    allowed = {'text', 'code_inline', 'em_open', 'em_close', 'strong_open', 'strong_close'}
    if any(t.type == 'text' and '[^' in t.content for t in tokens):
        return 'unsupported title construct: footnote-like syntax'
    return next(('unsupported title construct: ' + t.type for t in tokens
                 if t.type not in allowed), None)


def approval_prefix(tokens):
    """A code span or other inline construct interrupts immediate approval."""
    parts = []
    for token in tokens:
        if token.type == 'text':
            parts.append(token.content)
        else:
            break
    return ''.join(parts)


def anchored_title(tokens):
    """Read an anchor followed by a complete, nonempty emphasis subtree."""
    children = [c for c in tokens if c.type != 'text' or c.content]
    if len(children) < 4 or children[0].type != 'html_inline':
        return None
    html = children[0].content
    offset = 1
    if children[1].type == 'html_inline' and children[1].content == '</a>':
        html += children[1].content
        offset += 1
    anchor = ANCHOR.fullmatch(html)
    if not anchor or children[offset].type not in ('strong_open', 'em_open'):
        return None
    depth = 1
    close = offset + 1
    while close < len(children):
        depth += children[close].nesting
        if depth == 0:
            title = inline_content(children[offset + 1:close])
            unsupported = title_error(children[offset + 1:close])
            if title.strip() or unsupported:
                return anchor[1], title, children[offset].type, children[close + 1:], unsupported
            return None
        close += 1
    return None


def decision_destination(target):
    """Normalize Markdown authority destinations relative to the repository root.

    Fragment-only destinations select the decision inventory. Unsupported URL forms
    retain a non-authority path, so neither caller can accept them as authority.
    """
    try:
        url = urlsplit(target)
        path = posixpath.normpath(unquote(url.path)) if url.path else 'docs/decisions.md'
        if url.scheme or url.netloc or url.query or re.search(r'%(?![0-9a-fA-F]{2})', target):
            path = target.split('#', 1)[0] or '(unsupported destination)'
        return path, unquote(url.fragment)
    except ValueError:
        path, _, anchor = target.partition('#')
        return path or '(unsupported destination)', unquote(anchor)


def authority_entries(text):
    """Accept recorded forms within one list item's own paragraph prefix.

    A nested list or heading ends that prefix. No section, child, sibling,
    code block or paragraph outside the item can supply record fields.
    """
    tokens = MARKDOWN.parse(text)
    counts = Counter(re.findall(r'<a\s+id="([^"\n]+)"[^>]*>', text))
    result = dict.fromkeys(counts, UNRECORDED)
    # Preserve informative refusal diagnostics even for non-entry anchor paragraphs.
    # This can only refuse; it never supplies an approval form.
    for token in tokens:
        parsed = anchored_title(token.children or []) if token.type == 'inline' else None
        if parsed and parsed[4]:
            result[parsed[0]] = parsed[4]
        elif parsed and (status := DISPOSITION.search(parsed[1])):
            result[parsed[0]] = 'disposition in title: ' + status[0].lower()
    entries = []
    waiting_targets = {}
    waiting = False
    retained = set()
    for i, token in enumerate(tokens):
        if token.type == 'heading_open' and token.level == 0:
            heading = inline_content(tokens[i + 1].children or [])
            if heading == 'Waiting for the owner':
                waiting = True
            elif token.tag in ('h1', 'h2'):
                waiting = False
        if token.type != 'list_item_open':
            continue
        end = i + 1
        while not (tokens[end].type == 'list_item_close' and tokens[end].level == token.level):
            end += 1
        body = tokens[i + 1:end]
        if waiting and token.level == 1:
            first = next((t for t in body if t.type == 'inline'), None)
            if first and not re.match(r'^Closed \d{4}-\d{2}-\d{2}:', approval_prefix(first.children or [])):
                for t in body:
                    for c in t.children or []:
                        if c.type != 'link_open':
                            continue
                        target = c.attrGet('href') or ''
                        if '#' not in target:
                            continue
                        path, anchor = decision_destination(target)
                        diagnostic = ('open in Waiting for the owner' if path == 'docs/decisions.md'
                                      else 'wrong authority path ' + path + '#' + anchor)
                        waiting_targets[anchor] = diagnostic
        paragraphs = []
        utterance = False
        owner_header = False
        for t in body:
            if t.type in ('bullet_list_open', 'ordered_list_open', 'heading_open'):
                break
            if t.type not in ('paragraph_open', 'paragraph_close', 'inline', 'fence'):
                owner_header = False
            if t.type == 'fence' and t.level == token.level + 1:
                utterance |= owner_header
                owner_header = False
            if t.type == 'inline' and t.level == token.level + 2:
                paragraphs.append(t.children or [])
                owner_header = bool(OWNER_HEADER.match(approval_prefix(t.children or [])))
        if not paragraphs or not body or body[0].type != 'paragraph_open':
            continue
        parsed = anchored_title(paragraphs[0])
        if parsed is None:
            continue
        anchor, title, emphasis, after, unsupported = parsed
        entries.append((anchor, title, emphasis, after, paragraphs, unsupported, utterance))
        if anchor == 'decided-2026-11-13-d4-recovery':
            # Read only the explicitly labelled table's Existing record column.
            labelled = False
            in_table = False
            column = -1
            existing_column = False
            header = False
            for t in body:
                if t.type in ('bullet_list_open', 'ordered_list_open', 'heading_open'):
                    break
                if (t.type == 'inline' and t.level == token.level + 2
                        and inline_content(t.children or []) == (
                        'Already recovered authority, independently checked; existing records retained.')):
                    labelled = True
                elif t.type == 'table_open':
                    in_table = labelled and t.level == token.level + 1
                    labelled = False
                elif t.type == 'table_close':
                    in_table = False
                elif in_table and t.type == 'tr_open':
                    column = -1
                elif in_table and t.type in ('th_open', 'td_open'):
                    column += 1
                    header = t.type == 'th_open'
                elif in_table and t.type == 'inline' and column == 1:
                    if header:
                        existing_column = inline_content(t.children or []) == 'Existing record'
                        continue
                    if not existing_column:
                        continue
                    cs = t.children or []
                    if len(cs) == 3 and cs[0].type == 'link_open' and cs[2].type == 'link_close':
                        href = cs[0].attrGet('href') or ''
                        if href.startswith('#recovered-'):
                            retained.add(href[1:])
    for anchor, title, emphasis, after, paragraphs, unsupported, utterance in entries:
        status = DISPOSITION.search(title)
        if unsupported:
            result[anchor] = unsupported
        elif status:
            result[anchor] = 'disposition in title: ' + status[0].lower()
        elif anchor.startswith('recovered-'):
            if anchor in retained:
                result[anchor] = None
        elif re.match(r'^\s*\(owner,\s*\d{4}-\d{2}-\d{2}:',
                      approval_prefix(after)):
            result[anchor] = None
        elif (emphasis == 'strong_open'
              and (dated := re.fullmatch(r'decided-(\d{4}-\d{2}-\d{2})-[a-z0-9]+(?:-[a-z0-9]+)*', anchor))
              and title.endswith('(' + dated[1] + ').')):
            lines = [approval_prefix(paragraph) for paragraph in paragraphs]
            if (any(re.match(r'^Provenance: \S', line) for line in lines)
                    and any(OWNER_HEADER.match(line) for line in lines)
                    and utterance):
                result[anchor] = None
    for anchor in waiting_targets.keys() & result.keys():
        result[anchor] = waiting_targets[anchor]
    for anchor, count in counts.items():
        if count != 1:
            result[anchor] = 'duplicate authority anchor: ' + anchor
    return result


def decision_citations(doc):
    """Read citations in source order, preserving boundaries around inline constructs."""
    context = False

    def text_citations(prose):
        nonlocal context
        for match in CITATION.finditer(prose):
            path = match['path'] or ('docs/decisions.md' if context else '')
            anchor = match['fragment'] if match['path'] else match['bare']
            if anchor.endswith('.') and not anchor.endswith('..'):
                anchor = anchor[:-1]
            if path == 'docs/decisions.md':
                context = True
            yield path, anchor

    for token in MARKDOWN.parse(doc):
        if token.type != 'inline':
            continue
        parts = []
        for child in token.children or []:
            if child.type == 'text':
                parts.append(child.content)
            elif child.type in ('em_open', 'em_close', 'strong_open', 'strong_close'):
                continue
            elif child.type in ('softbreak', 'hardbreak'):
                parts.append('\n')
            else:
                # The placeholder cannot become a path or create a bare-fragment boundary.
                parts.append('\ufffc')
                if child.type not in ('link_open', 'image'):
                    continue
                target = child.attrGet('href' if child.type == 'link_open' else 'src') or ''
                if '#' not in target:
                    continue
                yield from text_citations(''.join(parts))
                parts = ['\ufffc']
                path, anchor = decision_destination(target)
                if path == 'docs/decisions.md':
                    context = True
                yield path, anchor
        yield from text_citations(''.join(parts))


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
        rules = [json.loads(l, object_pairs_hook=unique_object)
                 for l in open(rules_path, encoding="utf-8") if l.strip()]
        inventories = [open(p, encoding="utf-8").read() for p in row_paths]
    except (OSError, UnicodeDecodeError, ValueError) as e:
        print(f"error: {e}", file=sys.stderr); return 3
    if (not rules or type(rules[-1]) is not dict or rules[-1] != {"end": True}
            or type(rules[-1].get("end")) is not bool):
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
        schema = (schemas.get(r.get("kind"))
                  if type(r) is dict and type(r.get("kind")) is str else None)
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
    declared = {a or b for t in inventories for a, b in DECLARED.findall(t)}
    names = [r["name"] for r in rules]
    for n in sorted({n for n in names if names.count(n) > 1}):
        fails.append(f"rule {n} is registered more than once")
    for r in rules:
        if not r["exists"]: fails.append(f"rule {r['name']} names no declaration")
        if r["rev"] < 1:
            fails.append(f"rule {r['name']} has revision {r['rev']} (revisions start at 1)")
        if not r["doc"].strip(): fails.append(f"rule {r['name']} has no doc comment")
        else:
            rows = [m.group(1) for m in ROW.finditer(text_content(r["doc"]))]
            citations = list(decision_citations(r["doc"]))
            if not rows and not citations:
                fails.append(f"rule {r['name']}'s doc comment cites no decision row or authority entry")
            real_rows = [row for row in rows if not row.startswith('R-')]
            if real_rows and not any(row in declared for row in real_rows):
                fails.append(f"rule {r['name']}'s doc comment cites no existing decision row "
                             f"({', '.join(real_rows)})")
            if rows and not real_rows and not citations:
                fails.append(f"rule {r['name']}: roadmap-only: ROADMAP task {rows[0]} decides nothing")
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
        if e["verdict"] == "Invalid":
            fails.append(f"example {identity} has no concrete rule acceptance/refusal proposition")
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
