#!/usr/bin/env python3
"""The one reader of the Concrete language specification (docs/spec/, format in docs/spec/MAINTAINERS.md).

Every consumer of the spec goes through this module, so the format has one parser:

  lint          S1 format, S2 registry, S5 coverage, S8 tracking, S11 restatement (no compiler)
  history       S3: ids.tsv / codes.tsv transitions against a base revision
  run-examples  S4: every example, natively and with --interp
  codes         the code -> rules map; with --ledger, S6 against `--report diagnostic-codes`
  sites         S7: `spec: ID` comments, and the Lean file that resolves `Enforced by:` names
  lean-result   S7: read the output of that Lean file
  manifest      S9: generate (or --check) an acceptance manifest from the conformance tables
  anchors       rule ID -> `docs/spec/<chapter>.md#ID` (the `spec` evidence of a diagnostic)
  site          S10: write the website's spec pages
  restatements  S11 alone (check_docs_drift.sh runs this)

Every check prints `ok   <what>` or `FAIL [<check>] <what>` and the process exits 1 on any FAIL.
The negative controls in check_spec.sh match the `[<check>]` tag and the message, so a control
cannot pass by failing for another reason.
"""
import argparse, concurrent.futures, glob, hashlib, json, os, re, subprocess, sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
REPO_URL = "https://github.com/unbalancedparentheses/concrete2/blob/main/"
STAGES = {"parse", "resolve", "check", "elab", "mono", "post-mono", "core-check", "ssa-verify", "link"}
KINDS = {"refusal", "guarantee", "definition", "preservation", "open"}
# An `open` entry records a language question only the project owner decides (ROADMAP "Owner
# decisions", OD1 - OD22). It states the question, encodes no answer, and has Status `open ODn`.
OD_RE = re.compile(r"^OD[0-9]+$")
FIELDS = ["Kind", "Uses", "Codes", "Status", "Enforced by", "Semantics", "Obligation", "Evidence"]
CODE_CLASSES = ("rule", "internal", "tooling", "retired")  # plus unmigrated:<chapter>
# Generated acceptance manifests (MAINTAINERS.md §5): name -> (output path, chapters whose tables feed it)
MANIFESTS = {"exit_2i": ("tests/rules/exit_2i.tsv", ["references", "scope", "patterns"])}
MANIFEST_HEADER = """# GENERATED from the conformance tables of docs/spec/{chapters} by
# `python3 scripts/spec/spec.py manifest {name}` -- do not edit; edit the spec (check_spec.sh S9).
# Checked row by row by scripts/tests/check_rules_2i.sh. Columns: rule, fixture, expected
# (reject:<code>:<RULE@version> or run:<exit status>), pair (shared by a reject row and the run row
# of its rewritten program), modes. Rows are referenced, not copied: fixtures keep their
# run_tests.sh rows as well. Rules not enforced yet have pending examples in the spec instead.
"""

# Markdown files of docs/spec that are not chapters: the landing page, the maintainers' document and the
# generated single-page specification.
NON_CHAPTERS = ("README", "MAINTAINERS", "SPEC")
SINGLE_PATH = "docs/spec/SPEC.md"

FAILS = []
def ok(msg): print(f"  ok   {msg}")
def fail(check, msg):
    FAILS.append((check, msg)); print(f"  FAIL [{check}] {msg}")

# ---------------------------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------------------------
ID_RE = r"[A-Z]+-D?[0-9]+"
ANCHOR_RE = re.compile(r'^<a id="(' + ID_RE + r')"></a>\s*$')
HEAD_RE = re.compile(r"^### (" + ID_RE + r")\. (\S.*)$")
SENT_RE = re.compile(r"^\*\*(" + ID_RE + r")@([1-9][0-9]*)\.\*\*\s+(.*)$")
FIELD_RE = re.compile(r"^- ([A-Za-z ]+): (.*)$")
FENCE_RE = re.compile(r"^```(.*)$")
CODE_ITEM_RE = re.compile(r"^(E[0-9]{4}|new) \(([a-z-]+(?:, [a-z-]+)*)\)$")
TRACK_RE = re.compile(r"^(R-[0-9]{4}|bug-[0-9]{3})$")
ATTR_RE = re.compile(r"^([a-z-]+)=(\S+)$")


class Example:
    def __init__(self, entry, info, body, line, chapter):
        self.entry, self.info, self.body, self.line, self.chapter = entry, info, body, line, chapter
        self.attrs, self.verdict, self.errors = {}, None, []
        words = info.split()
        if len(words) < 2 or words[0] != "con" or words[1] not in ("accept", "reject", "abort"):
            self.errors.append(f"info string '{info}' is not `con accept|reject|abort ...`")
            return
        self.verdict = words[1]
        for w in words[2:]:
            m = ATTR_RE.match(w)
            if not m:
                self.errors.append(f"malformed attribute '{w}'"); continue
            k, v = m.groups()
            if k in self.attrs:
                self.errors.append(f"repeated attribute '{k}'")
            self.attrs[k] = v
        allowed = {"name", "pending", "now", "now-interp", "project", "interp-exempt"} | (
            {"exit"} if self.verdict == "accept" else {"code", "stage", "pair"} if self.verdict == "reject" else {"trap"})
        for k in self.attrs:
            if k not in allowed:
                self.errors.append(f"unknown attribute '{k}' for {self.verdict}")
        need = {"name"} | ({"exit"} if self.verdict == "accept" else {"code", "stage", "pair"} if self.verdict == "reject" else {"trap"})
        for k in sorted(need - set(self.attrs)):
            self.errors.append(f"missing attribute '{k}'")
        n = self.attrs.get("name", "")
        if n and not re.fullmatch(r"[a-z0-9-]+", n):
            self.errors.append(f"name '{n}' is not [a-z0-9-]+")
        if "exit" in self.attrs and not (self.attrs["exit"].isdigit() and int(self.attrs["exit"]) <= 125):
            self.errors.append(f"exit '{self.attrs['exit']}' is not 0..125")
        if "stage" in self.attrs and self.attrs["stage"] not in STAGES:
            self.errors.append(f"unknown stage '{self.attrs['stage']}'")
        c = self.attrs.get("code")
        if c is not None and not (re.fullmatch(r"E[0-9]{4}", c) or (c == "new" and "pending" in self.attrs)
                                  or (c == "none" and self.attrs.get("stage") == "link")):
            self.errors.append(f"code '{c}' is not E<4 digits> (or `new` on a pending example, or `none` at stage link)")
        if ("pending" in self.attrs) != ("now" in self.attrs):
            self.errors.append("`now=` is required exactly when `pending=` is present")
        if "now-interp" in self.attrs and "pending" not in self.attrs:
            self.errors.append("`now-interp=` only on a pending example")
        for k in ("now", "now-interp"):
            if k in self.attrs and parse_outcome(self.attrs[k]) is None:
                self.errors.append(f"malformed outcome {k}={self.attrs[k]}")
        if "project" in self.attrs and not re.fullmatch(r"[a-z][a-z0-9_]*", self.attrs["project"]):
            self.errors.append(f"project '{self.attrs['project']}' is not a package name [a-z][a-z0-9_]*")
        if "project" in self.attrs and self.verdict == "accept" and "interp-exempt" not in self.attrs:
            self.errors.append("a project example needs interp-exempt= (the interpreter runs single files)")
        if "pending" in self.attrs and not TRACK_RE.match(self.attrs["pending"]):
            self.errors.append(f"pending '{self.attrs['pending']}' is not R-NNNN or bug-NNN")

    @property
    def name(self): return self.attrs.get("name", "?")
    @property
    def key(self): return f"{self.entry.id}/{self.name}"
    @property
    def pending(self): return self.attrs.get("pending")


def parse_outcome(s):
    m = re.fullmatch(r"accept:([0-9]+)", s)
    if m: return ("accept", int(m.group(1)))
    m = re.fullmatch(r"reject:(E[0-9]{4}):([a-z-]+)", s)
    if m and m.group(2) in STAGES: return ("reject", m.group(1), m.group(2))
    m = re.fullmatch(r"fail:(\S+)", s)
    if m: return ("fail", m.group(1).replace("_", " "))
    return None


class Entry:
    def __init__(self, id_, title, chapter, line):
        self.id, self.title, self.chapter, self.line = id_, title, chapter, line
        self.version, self.sentence, self.fields = None, "", {}
        self.examples, self.rows, self.errors = [], [], []

    @property
    def tag(self): return f"{self.id}@{self.version}"
    @property
    def kind(self): return self.fields.get("Kind", "")
    def status(self):
        s = self.fields.get("Status", "").split()
        return (s[0], s[1]) if len(s) == 2 else ((s[0], None) if s else ("", None))
    def codes(self):
        """[(code, [stages])] of the Codes field ([] for `none`)."""
        v = self.fields.get("Codes", "none")
        if v == "none": return []
        out = []
        for item in split_list(v):
            m = CODE_ITEM_RE.match(item)
            out.append((m.group(1), m.group(2).split(", ")) if m else (None, item))
        return out
    def names(self, field):
        v = self.fields.get(field, "none")
        return [] if v in ("none", "") else [x.strip("`") for x in split_list(v)]


def split_list(v):
    """Split a field value on `, ` outside parentheses."""
    out, depth, cur = [], 0, ""
    i = 0
    while i < len(v):
        ch = v[i]
        if ch == "(": depth += 1
        if ch == ")": depth -= 1
        if depth == 0 and v.startswith(", ", i):
            out.append(cur.strip()); cur = ""; i += 2; continue
        cur += ch; i += 1
    if cur.strip(): out.append(cur.strip())
    return out


class Spec:
    def __init__(self, spec_dir):
        self.dir = spec_dir
        self.chapters = {}     # name -> prefix (README inventory, §1)
        self.entries = []      # in chapter order, then file order
        self.loose = []        # (chapter, line, problem) outside entries
        self.text = {}         # chapter -> raw text
        self._read_inventory()
        for ch in self.chapters:
            p = os.path.join(spec_dir, ch + ".md")
            if os.path.exists(p):
                self._parse_chapter(ch, p)
        for p in sorted(glob.glob(os.path.join(spec_dir, "*.md"))):
            b = os.path.basename(p)[:-3]
            if b not in NON_CHAPTERS and b not in self.chapters:
                self.loose.append((b, 0, f"chapter {b}.md is not in the chapter map (overview.md)"))

    def _read_inventory(self):
        # The chapter map is overview.md's table (`| [Title](name.md) | `PREFIX-` | ... |`), in reading
        # order. A revision from before the 2026-10-30 editorial split kept it in README.md
        # (`| `name.md` | `PREFIX-` | ... |`); history checks read such a revision too.
        # Fail closed: every row that starts like a map row must parse, and no chapter or prefix may
        # appear twice (a later row would silently replace an earlier one). The README fallback is
        # read only when overview.md has no map row at all.
        ov = os.path.join(self.dir, "overview.md")
        text = open(ov, encoding="utf-8").read() if os.path.exists(ov) else ""
        found, where = [], "overview.md"
        for i, ln in enumerate(text.split("\n")):
            if not re.match(r"^\| \[[^\]]*\]\([^)]*\.md\)", ln):
                continue
            m = re.match(r"^\| \[[^\]]*\]\(([a-z]+)\.md\) \| `([A-Z]+)-` \|", ln)
            if m: found.append((m.group(1), m.group(2)))
            else: self.loose.append(("overview", i + 1, f"chapter map row does not parse as | [Title](name.md) | `PREFIX-` | ...: {ln[:80]}"))
        if not found and not any(c == "overview" for c, _, _ in self.loose):
            where = "README.md"
            readme = open(os.path.join(self.dir, "README.md"), encoding="utf-8").read()
            found = re.findall(r"^\| `([a-z]+)\.md` \| `([A-Z]+)-` \|", readme, re.M)
        if not found:
            self.loose.append(("overview", 0, "no chapter map (overview.md's | [Title](name.md) | `PREFIX-` | table)"))
        seen_p = {}
        for name, prefix in found:
            if name in self.chapters:
                self.loose.append(("overview", 0, f"chapter map ({where}) lists {name}.md twice"))
            if prefix in seen_p:
                self.loose.append(("overview", 0, f"chapter map ({where}) gives prefix {prefix}- to both {seen_p[prefix]}.md and {name}.md"))
            seen_p.setdefault(prefix, name)
            self.chapters.setdefault(name, prefix)

    def _parse_chapter(self, ch, path):
        lines = open(path, encoding="utf-8").read().split("\n")
        self.text[ch] = "\n".join(lines)
        cur, i = None, 0
        while i < len(lines):
            ln = lines[i]
            m = ANCHOR_RE.match(ln)
            if m:
                h = HEAD_RE.match(lines[i + 1]) if i + 1 < len(lines) else None
                if not h or h.group(1) != m.group(1):
                    self.loose.append((ch, i + 1, f"anchor {m.group(1)} is not followed by `### {m.group(1)}. <title>`"))
                    i += 1; continue
                cur = Entry(h.group(1), h.group(2), ch, i + 2)
                self.entries.append(cur)
                i = self._parse_entry_head(cur, lines, i + 2)
                continue
            if HEAD_RE.match(ln):
                self.loose.append((ch, i + 1, f"rule heading without its anchor line: {ln}"))
            if ln.startswith("## ") or ln.startswith("# "):
                cur = None
            f = FENCE_RE.match(ln)
            if f and f.group(1).strip():
                info = f.group(1).strip()
                body, j = [], i + 1
                while j < len(lines) and not lines[j].startswith("```"):
                    body.append(lines[j]); j += 1
                if j >= len(lines):
                    self.loose.append((ch, i + 1, f"fence `{info}` is never closed"))
                if info == "con pseudocode" and cur is not None:
                    self.loose.append((ch, i + 1, f"`con pseudocode` inside entry {cur.id} (allowed only outside entries)"))
                elif info.split()[0] == "con" and info != "con pseudocode":
                    if cur is None:
                        self.loose.append((ch, i + 1, f"example `{info}` outside an entry"))
                    else:
                        cur.examples.append(Example(cur, info, "\n".join(body) + "\n", i + 1, ch))
                i = j + 1; continue
            if cur is not None and ln.startswith("| fixture | expected | pair | modes |"):
                j = i + 2
                while j < len(lines) and lines[j].startswith("|"):
                    cells = [c.strip().strip("`") for c in lines[j].strip().strip("|").split("|")]
                    if len(cells) != 4 or not all(cells):
                        cur.errors.append(f"line {j+1}: conformance row needs 4 cells")
                    else:
                        cur.rows.append(cells)
                    j += 1
                i = j; continue
            i += 1

    def _parse_entry_head(self, e, lines, i):
        while i < len(lines) and not lines[i].strip(): i += 1
        para = []
        while i < len(lines) and lines[i].strip():
            para.append(lines[i].strip()); i += 1
        m = SENT_RE.match(" ".join(para)) if para else None
        if not m or m.group(1) != e.id:
            e.errors.append(f"first paragraph is not `**{e.id}@<version>.** <sentence>`")
        else:
            e.version, e.sentence = int(m.group(2)), m.group(3)
        while i < len(lines) and not lines[i].strip(): i += 1
        seen = []
        while i < len(lines) and lines[i].startswith("- "):
            fm = FIELD_RE.match(lines[i])
            if not fm:
                e.errors.append(f"malformed field line: {lines[i]}")
            else:
                k, v = fm.group(1), fm.group(2).strip()
                if k not in FIELDS: e.errors.append(f"unknown field '{k}'")
                elif k in e.fields: e.errors.append(f"repeated field '{k}'")
                else: e.fields[k] = v; seen.append(k)
            i += 1
        if seen != [f for f in FIELDS if f in seen]:
            e.errors.append(f"fields out of order: {seen}")
        return i

    def by_id(self):
        return {e.id: e for e in self.entries}


# ---------------------------------------------------------------------------------------------
# Normalization (sentence hashes and restatement matching use the same function)
# ---------------------------------------------------------------------------------------------
def normalize(text):
    t = re.sub(r"!?\[([^\]]*)\]\([^)]*\)", r"\1", text)          # links -> their text
    # inline HTML tags, only outside code spans (`Box<&Int>` is code, not a tag)
    codes = []
    def keep(m):
        codes.append(m.group(2)); return f"\x00{len(codes) - 1}\x00"
    t = re.sub(r"(`+)(.+?)\1", keep, t, flags=re.S)                  # code spans, any backtick run
    t = re.sub(r"</?[A-Za-z][A-Za-z0-9-]*(\s[^<>]*)?/?>", " ", t)
    t = re.sub(r"\x00([0-9]+)\x00", lambda m: codes[int(m.group(1))].strip(), t)
    t = t.replace("`", "").replace("**", "").replace("__", "")
    t = re.sub(r"(?<![A-Za-z0-9])\*([^*\s][^*]*)\*", r"\1", t)     # *emphasis*
    t = re.sub(r"^\s*([-*+>]|\d+\.)\s+", " ", t, flags=re.M)       # list and quote markers
    return re.sub(r"\s+", " ", t).strip().lower()

def sentence_hash(e):
    return hashlib.sha256(normalize(e.sentence).encode()).hexdigest()[:32]

def sentence_count(s):
    t = re.sub(r"`[^`]*`", "X", s)
    t = t.replace("e.g.", "eg").replace("i.e.", "ie")
    breaks = re.findall(r"[.!?]\s+[A-Z(\"`*]", t)
    return len(breaks) + 1, t.rstrip().endswith((".", "!", "?"))


# ---------------------------------------------------------------------------------------------
# Static checks (S1, S2, S5, S8)
# ---------------------------------------------------------------------------------------------
def read_tsv(path):
    rows = []
    if not os.path.exists(path): return None
    for ln in open(path, encoding="utf-8").read().split("\n"):
        if ln.strip() and not ln.startswith("#"):
            rows.append(ln.split("\t"))
    return rows

def tracking_exists(t, root):
    if OD_RE.match(t):
        p = os.path.join(root, "ROADMAP.md")
        return os.path.exists(p) and re.search(r"^\| " + re.escape(t) + r" \|", open(p, encoding="utf-8").read(), re.M) is not None
    if t.startswith("R-"):
        for f in ("ROADMAP.md", "docs/project/ROADMAP_TASKS.md", "docs/project/ROADMAP_BACKLOG.md"):
            p = os.path.join(root, f)
            if os.path.exists(p) and re.search(r"\b" + re.escape(t) + r"\b", open(p, encoding="utf-8").read()):
                return True
        return False
    return bool(glob.glob(os.path.join(root, "docs/bugs", t[4:] + "_*.md")))

def check_format(spec, root):
    s1 = 0
    if not spec.entries:
        fail("S1", "no entries parsed (refusing a vacuous spec)"); return
    for ch, ln, why in spec.loose:
        fail("S1", f"{ch}.md:{ln}: {why}"); s1 += 1
    ids = {}
    for e in spec.entries:
        where = f"{e.chapter}.md:{e.line} {e.id}"
        if e.id in ids:
            fail("S1", f"{where}: duplicate ID (also {ids[e.id]})"); s1 += 1
        ids[e.id] = where
        pre = spec.chapters[e.chapter]
        if not re.fullmatch(pre + r"-D?[0-9]+", e.id):
            fail("S1", f"{where}: ID prefix is not the chapter's ({pre}-)"); s1 += 1
        for why in e.errors:
            fail("S1", f"{where}: {why}"); s1 += 1
        if e.sentence:
            n, ends = sentence_count(e.sentence)
            if n != 1 or not ends:
                fail("S1", f"{where}: the normative statement must be exactly one sentence (found {n})"); s1 += 1
        for f in FIELDS:
            if f not in e.fields and not (f == "Enforced by" and e.status()[0] in ("pending", "open")):
                fail("S1", f"{where}: missing field '{f}'"); s1 += 1
        if e.kind and e.kind not in KINDS:
            fail("S1", f"{where}: unknown Kind '{e.kind}'"); s1 += 1
        if (e.id.split("-")[1].startswith("D")) != (e.kind == "definition"):
            fail("S1", f"{where}: definitions, and only definitions, have a D-numbered ID"); s1 += 1
        st, tr = e.status()
        if st not in ("enforced", "partial", "pending", "open") or (st == "enforced") != (tr is None):
            fail("S1", f"{where}: malformed Status '{e.fields.get('Status','')}'"); s1 += 1
        if (e.kind == "open") != (st == "open") or (st == "open" and not OD_RE.match(tr or "")):
            fail("S1", f"{where}: an open entry, and only an open entry, has Status `open ODn`"); s1 += 1
        if e.kind == "open" and e.examples:
            fail("S1", f"{where}: an open entry has no examples (it encodes no answer)"); s1 += 1
        if e.kind == "open" and (e.names("Enforced by") or e.names("Semantics")):
            fail("S1", f"{where}: an open entry names no enforcing or semantic declaration (it encodes no answer)"); s1 += 1
        codes = e.codes()
        if e.kind == "refusal" and not codes:
            fail("S1", f"{where}: a refusal names at least one code"); s1 += 1
        if e.kind != "refusal" and codes:
            fail("S1", f"{where}: only a refusal names codes"); s1 += 1
        for c, stages in codes:
            if c is None: fail("S1", f"{where}: malformed Codes item '{stages}'"); s1 += 1
            elif c == "new" and st != "pending": fail("S1", f"{where}: `new` code on a rule that is not pending"); s1 += 1
            else:
                for sg in stages:
                    if sg not in STAGES: fail("S1", f"{where}: unknown stage '{sg}'"); s1 += 1
        ev = e.fields.get("Evidence", "")
        good_ev = (ev == "by-users") if e.kind == "definition" else (ev == "open") if e.kind == "open" else (
            ev == "tested" or re.fullmatch(r"(proved|trusted): \S+", ev) is not None)
        if not good_ev:
            fail("S1", f"{where}: malformed Evidence '{ev}'"); s1 += 1
        for u in e.names("Uses"):
            d = spec.by_id().get(u)
            if d is None or d.kind != "definition":
                fail("S1", f"{where}: Uses names '{u}', which is not a definition"); s1 += 1
        names = set()
        for x in e.examples:
            for why in x.errors:
                fail("S1", f"{x.chapter}.md:{x.line} {x.key}: {why}"); s1 += 1
            if x.name in names:
                fail("S1", f"{x.chapter}.md:{x.line} {x.key}: duplicate example name"); s1 += 1
            names.add(x.name)
            if st == "enforced" and x.pending:
                fail("S1", f"{x.key}: a pending example under an enforced rule (Status must be partial)"); s1 += 1
            if x.verdict == "reject" and x.attrs.get("code") not in ("new", "none", None) and e.kind != "preservation":
                stg = dict((c, s) for c, s in codes if c)
                if x.attrs["code"] not in stg:
                    fail("S1", f"{x.key}: code {x.attrs['code']} is not in the rule's Codes"); s1 += 1
                elif x.attrs.get("stage") not in stg[x.attrs["code"]]:
                    fail("S1", f"{x.key}: stage {x.attrs.get('stage')} is not a stage of {x.attrs['code']}"); s1 += 1
        if st == "partial" and not any(x.pending for x in e.examples):
            fail("S1", f"{where}: Status partial without a pending example"); s1 += 1
    defs_used = {u for e in spec.entries for u in e.names("Uses")}
    for e in spec.entries:
        if e.kind == "definition" and e.id not in defs_used:
            fail("S1", f"{e.id}: a definition no entry uses")
            s1 += 1
    # relative links resolve
    for ch, txt in spec.text.items():
        for m in re.finditer(r"\]\(([^)#\s]+)(#[^)]*)?\)", txt):
            tgt = m.group(1)
            if re.match(r"[a-z]+:", tgt): continue
            if not os.path.exists(os.path.normpath(os.path.join(root, "docs", "spec", tgt))):
                fail("S1", f"{ch}.md: relative link '{tgt}' does not resolve"); s1 += 1
    if s1 == 0:
        ok(f"S1 format: {len(spec.entries)} entries, {sum(len(e.examples) for e in spec.entries)} examples in {len(spec.text)} chapters")

def check_coverage(spec):
    bad, native_only = 0, []
    for e in spec.entries:
        st, _ = e.status()
        acc = {x.name: x for x in e.examples if x.verdict == "accept"}
        rej = [x for x in e.examples if x.verdict == "reject"]
        if e.kind == "refusal":
            if st != "pending" and not any(not x.pending for x in rej):
                fail("S5", f"{e.id}: an enforced refusal needs a non-pending reject example"); bad += 1
            if st == "pending" and not rej:
                fail("S5", f"{e.id}: a pending refusal needs a reject example"); bad += 1
        if rej and e.kind not in ("refusal", "preservation"):
            fail("S5", f"{e.id}: a {e.kind} has no reject examples (only refusals and stage boundaries do)"); bad += 1
        if True:
            for x in rej:
                p = acc.get(x.attrs.get("pair", ""))
                if p is None:
                    fail("S5", f"{x.key}: pair '{x.attrs.get('pair')}' is not an accept example of {e.id}"); bad += 1
                elif not x.pending and p.pending:
                    fail("S5", f"{x.key}: its pair {p.key} is pending"); bad += 1
        if e.kind in ("guarantee", "preservation") and st != "pending":
            std_only = bool(e.names("Enforced by")) and all(is_std_site(n) for n in e.names("Enforced by"))
            both = any(not x.pending and "interp-exempt" not in x.attrs for x in acc.values())
            lib = std_only and any(not x.pending and "project" in x.attrs for x in acc.values())
            if lib and not both: native_only.append(e.id)
            if not (both or lib):
                fail("S5", f"{e.id}: a guarantee needs a non-pending accept example run on both consumers"); bad += 1
    if bad == 0:
        ok("S5 coverage: every refusal has reject examples paired with accepted rewrites; every guarantee runs on both consumers"
           + (f", except std library contracts covered natively by a project example ({', '.join(native_only)})" if native_only else ""))

def check_tracking(spec, root):
    bad, seen = 0, set()
    for e in spec.entries:
        ts = [e.status()[1]] + [x.pending for x in e.examples]
        for t in ts:
            if not t: continue
            seen.add(t)
            if not (TRACK_RE.match(t) or OD_RE.match(t)) or not tracking_exists(t, root):
                fail("S8", f"{e.id}: tracking ID '{t}' does not exist"); bad += 1
    if bad == 0:
        ok(f"S8 tracking: {len(seen)} tracking IDs exist ({', '.join(sorted(seen))})")

def registry_expect(spec):
    """key -> state the chapters imply, plus hash for entry versions."""
    exp = {}
    for e in spec.entries:
        if e.version is None: continue
        exp[e.tag] = ("pending" if e.status()[0] == "pending" else "live", sentence_hash(e))
        for x in e.examples:
            exp[x.key] = ("pending" if x.pending else "live", "-")
    return exp

def check_registry(spec):
    rows = read_tsv(os.path.join(spec.dir, "ids.tsv"))
    if rows is None:
        fail("S2", "docs/spec/ids.tsv is missing"); return
    reg, bad = {}, 0
    for r in rows:
        if len(r) != 3:
            fail("S2", f"ids.tsv row {r} does not have 3 columns"); bad += 1; continue
        if r[0] in reg:
            fail("S2", f"ids.tsv: duplicate key {r[0]}"); bad += 1
        reg[r[0]] = (r[1], r[2])
        if not re.fullmatch(r"live|pending|superseded|retired:\S+", r[1]):
            fail("S2", f"ids.tsv: {r[0]} has malformed state '{r[1]}'"); bad += 1
    exp = registry_expect(spec)
    for k, (state, h) in exp.items():
        if k not in reg:
            fail("S2", f"{k} is in the spec but not registered in ids.tsv"); bad += 1
        elif reg[k][0] != state:
            fail("S2", f"{k}: ids.tsv says {reg[k][0]}, the spec says {state}"); bad += 1
        elif h != "-" and reg[k][1] != h:
            fail("S2", f"{k}: the sentence changed without a new version (hash {h} != registered {reg[k][1]})"); bad += 1
    for k, (state, _) in reg.items():
        if state in ("live", "pending") and k not in exp:
            fail("S2", f"{k} is registered {state} but is no longer in the spec (retire it with a reason)"); bad += 1
    # an entry has at most one live/pending version
    cur = {}
    for k, (state, _) in reg.items():
        if "@" in k and state in ("live", "pending"):
            i = k.split("@")[0]
            if i in cur: fail("S2", f"{i}: two current versions ({cur[i]}, {k})"); bad += 1
            cur[i] = k
    if bad == 0:
        ok(f"S2 registry: {len(exp)} keys registered with matching states and sentence hashes")

def check_restatements(spec, root, scan_root=None):
    sents = [(e.id, normalize(e.sentence)) for e in spec.entries if e.sentence]
    excl = [os.path.join(root, "docs", "spec"), os.path.join(root, "site", "content", "spec"),
            os.path.join(root, "docs", "archive"), os.path.join(root, ".lake"), os.path.join(root, "site", "public")]
    base = scan_root or root
    bad, files = 0, 0
    for dp, dn, fn in os.walk(base):
        dn[:] = [d for d in dn if not d.startswith(".") and os.path.join(dp, d) not in excl and d not in ("node_modules", "public")]
        for f in fn:
            if not f.endswith(".md"): continue
            p = os.path.join(dp, f); files += 1
            try: t = normalize(open(p, encoding="utf-8").read())
            except UnicodeDecodeError: continue
            for i, s in sents:
                if s and s in t:
                    fail("S11", f"{os.path.relpath(p, root)} restates the normative sentence of {i}"); bad += 1
    if files == 0:
        fail("S11", "scanned no Markdown files")
    elif bad == 0:
        ok(f"S11 restatement: {len(sents)} normative sentences, none restated in {files} Markdown files")


# ---------------------------------------------------------------------------------------------
# S3 history
# ---------------------------------------------------------------------------------------------
def tsv_text(t):
    return [ln.split("\t") for ln in t.split("\n") if ln.strip() and not ln.startswith("#")]

def read_opt(p):
    return open(p, encoding="utf-8").read() if p and os.path.exists(p) else None

def check_allow_history(spec, root, base):
    """S12 against the base: an allow row is added only for a (code, rule) pair the base's inventory
    did not have, so an exemption never replaces a witness that existed (the list only shrinks)."""
    def show(path):
        r = subprocess.run(["git", "-C", root, "show", f"{base}:{path}"], capture_output=True, text=True)
        return r.stdout if r.returncode == 0 else None
    old_allow, old_inv = show(ALLOW_PATH), show(INVENTORY_PATH)
    if old_allow is None or old_inv is None:
        ok(f"S12 history: no allow list or inventory at base {base} (first revision)"); return
    keys = lambda t: {(r[0], r[1]) for r in tsv_text(t) if len(r) >= 2}
    old_rows = keys(old_allow)
    old_pairs = {(r[2], r[0]) for r in tsv_text(old_inv) if len(r) >= 3}
    bad = 0
    for r in read_tsv(os.path.join(root, ALLOW_PATH)) or []:
        if len(r) >= 2 and (r[0], r[1]) not in old_rows and (r[0], r[1]) in old_pairs:
            fail("S12", f"{ALLOW_PATH}: {r[0]} {r[1]} was added though the base already named that rule code (an exemption may not replace a witness)"); bad += 1
    if bad == 0:
        ok(f"S12 history: the allow list adds rows only for rule codes new since {base}")

def check_history(spec, base, old_ids_path, old_codes_path, old_spec_dir):
    """S3 against the base revision's ids.tsv, codes.tsv and chapters (extracted by check_spec.sh)."""
    old_ids = read_opt(old_ids_path)
    new_ids = read_tsv(os.path.join(spec.dir, "ids.tsv")) or []
    bad = 0
    if old_ids is None:
        ok(f"S3 history: no ids.tsv at base {base} (first registry)")
    else:
        old = {r[0]: r for r in tsv_text(old_ids)}
        new = {r[0]: r for r in new_ids}
        allowed = {("pending", "live"), ("live", "superseded")}
        for k, r in old.items():
            if k not in new:
                fail("S3", f"ids.tsv row {k} was removed (rows are append-only; retire it instead)"); bad += 1; continue
            o, n = r[1], new[k][1]
            if r[2] != new[k][2]:
                fail("S3", f"{k}: registered hash changed"); bad += 1
            if o != n:
                final = o == "superseded" or o.startswith("retired:")
                if final or not ((o, n) in allowed or (o in ("live", "pending") and n.startswith("retired:"))):
                    fail("S3", f"{k}: state {o} -> {n} is not an allowed transition"); bad += 1
                if n == "superseded":
                    i, v = k.split("@")
                    if f"{i}@{int(v)+1}" not in new:
                        fail("S3", f"{k} superseded without a newer version"); bad += 1
        # a definition that gained a version forces its users to gain one
        for k in new:
            if "@" in k and k not in old:
                i, v = k.split("@")
                d = spec.by_id().get(i)
                if d is not None and d.kind == "definition" and int(v) > 1:
                    for e in spec.entries:
                        if i in e.names("Uses") and e.tag in old:
                            fail("S3", f"{e.id} uses {i}, which gained version {v}, but {e.id} kept {e.tag}"); bad += 1
    # an entry that had a Semantics: link keeps one (the normative link is never dropped)
    if old_spec_dir and os.path.exists(os.path.join(old_spec_dir, "README.md")):
        old_spec = Spec(old_spec_dir)
        new_by = spec.by_id()
        for e in old_spec.entries:
            if e.names("Semantics") and e.id in new_by and not new_by[e.id].names("Semantics"):
                fail("S3", f"{e.id} lost its Semantics: link ({', '.join(e.names('Semantics'))})"); bad += 1
    old_codes = read_opt(old_codes_path)
    if old_codes is not None:
        oldu = {r[0] for r in tsv_text(old_codes) if len(r) > 1 and r[1].startswith("unmigrated:")}
        for r in read_tsv(os.path.join(spec.dir, "codes.tsv")) or []:
            if len(r) > 1 and r[1].startswith("unmigrated:") and r[0] not in oldu:
                fail("S3", f"codes.tsv: {r[0]} newly classified unmigrated (the list only shrinks)"); bad += 1
    if bad == 0:
        ok(f"S3 history: ids.tsv and codes.tsv transitions against {base} are allowed")


# ---------------------------------------------------------------------------------------------
# S4 examples
# ---------------------------------------------------------------------------------------------
ERR_RE = re.compile(r"error\[([a-z-]+)\]: (?:\((E[0-9]{4})\))?")
# a diagnostic must come from a compiler stage; any of these means a lower layer leaked (PIPELINE_CONTRACTS)
LEAK_RE = re.compile(r"llvm-as|LLVM IR validation|Undefined symbols|^ld:|clang: error|llc:|PANIC|panic|Segmentation", re.M)

def first_error(out):
    lines = out.split("\n")
    for i, ln in enumerate(lines):
        m = ERR_RE.search(ln)
        if m:
            tag = None
            for l2 in lines[i + 1:]:
                if ERR_RE.search(l2) or l2.startswith("error"): break
                t = re.match(r"^\s+rule: (\S+)$", l2)
                if t: tag = t.group(1); break
            return ("reject", m.group(2) or "none", m.group(1), tag)
    return None

def run(cmd, timeout=120, cwd=None, env=None):
    try:
        r = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout, stdin=subprocess.DEVNULL, cwd=cwd, env=env)
        return r.returncode, r.stdout + r.stderr, r.stdout
    except subprocess.TimeoutExpired:
        return None, "timeout", ""

def outcome_native(cc, src, workdir, project=None):
    binp = src[:-4] + ".bin"
    if project:
        d = src[:-4] + ".proj"
        os.makedirs(os.path.join(d, "src"), exist_ok=True)
        open(os.path.join(d, "Concrete.toml"), "w").write(f'[package]\nname = "{project}"\nversion = "0.1.0"\n')
        os.replace(src, os.path.join(d, "src", "main.con"))
        rc, out, _ = run([cc, "build", "-o", binp], cwd=d, env=dict(os.environ, CONCRETE_STD=os.path.join(ROOT, "std")))
    else:
        rc, out, _ = run([cc, src, "-o", binp])
    if rc is None or rc < 0:
        return ("fail", f"compiler {'timeout' if rc is None else 'signal ' + str(-rc)}")
    if rc != 0:
        if LEAK_RE.search(out):
            return ("fail", "leaked past the compiler: " + LEAK_RE.search(out).group(0) + " :: " + out)
        return first_error(out) or ("fail", out)
    rc, out, _ = run([binp])
    if rc is None:
        return ("fail", "timeout")
    if rc < 0:
        return ("fail", f"signal {-rc}")
    return ("accept", rc)

def outcome_interp(cc, src):
    rc, out, stdout = run([cc, src, "--interp"])
    if rc is None or rc < 0:
        return ("fail", f"interpreter {'timeout' if rc is None else 'signal ' + str(-rc)}")
    if rc != 0 and LEAK_RE.search(out):
        return ("fail", "leaked past the interpreter: " + LEAK_RE.search(out).group(0) + " :: " + out)
    if rc == 0:
        last = [l for l in stdout.split("\n") if l.strip()]
        v = last[-1].strip() if last else ""
        return ("accept", int(v)) if re.fullmatch(r"-?[0-9]+", v) else ("fail", out)
    return first_error(out) or ("fail", out)

MODEL_SUMMARY = re.compile(r"^MODEL-DIFF: .*\bagree=([0-9]+) mismatch=([0-9]+)", re.M)

def outcome_model(cc, src):
    """The Lean semantics' verdict (decision B17): `--report model-diff` evaluates every extracted
    scalar function of the program with the ProofCore model (`Concrete.Proof.eval`) and with the
    interpreter, on boundary inputs. Returns (agree, mismatch rows); (0, []) when nothing is
    extracted; ("fail", msg) when the report itself fails."""
    rc, out, stdout = run([cc, src, "--report", "model-diff"])
    if rc is None or rc < 0:
        return ("fail", f"model-diff {'timeout' if rc is None else 'signal ' + str(-rc)}")
    if rc != 0:
        return ("fail", "model-diff failed: " + (out.strip().split("\n") or [""])[-1])
    m = MODEL_SUMMARY.search(stdout)
    if not m:
        return ("fail", "model-diff printed no summary")
    bad = [l for l in stdout.split("\n") if l.startswith("MISMATCH\t")]
    return (int(m.group(1)), bad)

def show(o):
    if o[0] == "accept": return f"accept:{o[1]}"
    if o[0] == "reject": return f"reject:{o[1]}:{o[2]}" + (f" ({o[3]})" if o[3] else " (no rule tag)")
    return "fail:" + " ".join(o[1].split())[:120]

def matches_target(x, o):
    e = x.entry
    if x.verdict == "accept":
        return o[0] == "accept" and o[1] == int(x.attrs["exit"])
    if x.verdict == "abort":
        # a run-time trap: the binary is killed by SIGABRT (the abort helpers), the interpreter
        # stops with a run-time error; a compile-time diagnostic is not an abort
        # the interpreter's run-time error must name the expected trap (`trap=`), so an unrelated
        # interpreter failure cannot pass for the trap
        trap = x.attrs.get("trap", "").replace("_", " ")
        return o[0] == "fail" and (o[1] == "signal 6" or (
            o[1].startswith("interp:") and not ERR_RE.search(o[1]) and bool(trap) and trap in o[1]))
    if o[0] != "reject" or o[2] != x.attrs["stage"]:
        return False
    if x.attrs["code"] == "new":
        now = parse_outcome(x.attrs["now"])
        return not (now[0] == "reject" and now[1] == o[1])
    if e.kind == "preservation":   # a stage boundary: the code belongs to another chapter's rule
        return o[1] == x.attrs["code"]
    return o[1] == x.attrs["code"] and o[3] == e.tag

def matches_now(spec_o, o):
    if spec_o[0] == "accept": return o[0] == "accept" and o[1] == spec_o[1]
    if spec_o[0] == "reject": return o[0] == "reject" and o[1] == spec_o[1] and o[2] == spec_o[2]
    return o[0] == "fail" and spec_o[1] in o[1]

def judge(x, nat, itp):
    """-> (verdict, message): verdict in PASS, FAIL, PENDING."""
    legs = [("native", nat)] + ([] if itp is None else [("interp", itp)])
    if not x.pending:
        bad = [f"{n} {show(o)}" for n, o in legs if not matches_target(x, o)]
        if bad:
            return "FAIL", "does not behave as specified: " + "; ".join(bad)
        return "PASS", "; ".join(f"{n} {show(o)}" for n, o in legs)
    if all(matches_target(x, o) for _, o in legs):
        return "FAIL", f"pending example already holds ({'; '.join(show(o) for _, o in legs)}): remove pending={x.pending}" + (
            " and allocate its code" if x.attrs.get("code") == "new" else "")
    now = parse_outcome(x.attrs["now"])
    nowi = parse_outcome(x.attrs.get("now-interp", x.attrs["now"]))
    bad = []
    if not matches_now(now, nat): bad.append(f"native {show(nat)}, now= says {x.attrs['now']}")
    if itp is not None and not matches_now(nowi, itp):
        bad.append(f"interp {show(itp)}, now says {x.attrs.get('now-interp', x.attrs['now'])}")
    if bad:
        return "FAIL", "pending example changed behaviour: " + "; ".join(bad)
    return "PENDING", f"{x.pending}: today {show(nat)}" + ("" if itp is None else f" / interp {show(itp)}")

def run_examples(spec, cc, jobs, workdir, only=None):
    xs = [x for e in spec.entries for x in e.examples if not x.errors]
    if only: xs = [x for x in xs if x.key in only]
    if not xs:
        fail("S4", "no examples to run (refusing a vacuous run)"); return {}
    os.makedirs(workdir, exist_ok=True)
    def one(ix):
        i, x = ix
        src = os.path.join(workdir, f"ex{i:04d}.con")
        open(src, "w").write(x.body)
        # the Lean semantics, before outcome_native (a project example moves its source)
        mdl = None
        if x.verdict in ("accept", "abort") and not x.pending and not x.attrs.get("project"):
            mdl = outcome_model(cc, src)
        nat = outcome_native(cc, src, workdir, x.attrs.get("project"))
        itp = None if "interp-exempt" in x.attrs else outcome_interp(cc, src)
        v, msg = judge(x, nat, itp)
        if mdl is not None and v == "PASS":
            if mdl[0] == "fail": v, msg = "FAIL", "Lean model: " + mdl[1]
            elif mdl[1]: v, msg = "FAIL", "the Lean model disagrees with the interpreter: " + "; ".join(r.replace("\t", " ") for r in mdl[1][:3])
            elif mdl[0]: msg += f"; Lean model agrees on {mdl[0]} input(s)"
        return x, (v, msg, mdl)
    counts = {"PASS": 0, "FAIL": 0, "PENDING": 0}
    modeled = rows = 0
    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as ex:
        for x, (v, msg, mdl) in ex.map(one, enumerate(xs)):
            counts[v] += 1
            if mdl is not None and mdl[0] != "fail" and mdl[0]: modeled += 1; rows += mdl[0]
            ex_note = f" [interp-exempt: {x.attrs['interp-exempt']}]" if "interp-exempt" in x.attrs else ""
            if v == "PASS": ok(f"S4 {x.key} ({x.verdict}){ex_note}: {msg}")
            elif v == "PENDING": print(f"  PENDING {x.key} ({x.verdict}): {msg}")
            else: fail("S4", f"{x.key} ({x.chapter}.md:{x.line}): {msg}")
    print(f"  examples: PASS={counts['PASS']} FAIL={counts['FAIL']} PENDING={counts['PENDING']}")
    print(f"  Lean model: {modeled} accepted examples compared with the interpreter on {rows} inputs, 0 disagreements allowed")
    return counts


# ---------------------------------------------------------------------------------------------
# S6 codes
# ---------------------------------------------------------------------------------------------
def code_map(spec):
    m = {}
    for e in spec.entries:
        for c, _ in e.codes():
            if c and c != "new" and e.version is not None and e.status()[0] != "pending":
                m.setdefault(c, set()).add(e.tag)
    return m

def check_codes(spec, ledger_path):
    try:
        led = json.load(open(ledger_path))
        rows = {r["code"]: r for r in led.get("codes", [])}
    except Exception as ex:
        fail("S6", f"cannot read the diagnostic-codes ledger: {ex}"); return
    if len(rows) < 50:
        fail("S6", f"the ledger lists only {len(rows)} codes (refusing a vacuous ledger)"); return
    cls = {}
    bad = 0
    for r in read_tsv(os.path.join(spec.dir, "codes.tsv")) or []:
        if len(r) < 2: fail("S6", f"codes.tsv row {r} is malformed"); bad += 1; continue
        if r[0] in cls: fail("S6", f"codes.tsv: {r[0]} classified twice"); bad += 1
        cls[r[0]] = r[1]
        if not (r[1] in CODE_CLASSES or (r[1].startswith("unmigrated:") and r[1][11:] in spec.chapters_planned)):
            fail("S6", f"codes.tsv: {r[0]} has unknown class '{r[1]}'"); bad += 1
    cmap = code_map(spec)
    for c in sorted(rows):
        if c not in cls:
            fail("S6", f"{c} ({rows[c].get('description','')}) is in the ledger but not classified in codes.tsv (orphan code)"); bad += 1
            continue
        named = c in cmap
        if (cls[c] == "rule") != named:
            fail("S6", f"{c}: class {cls[c]} but {'some' if named else 'no'} entry names it"); bad += 1
        want = ", ".join(sorted(cmap.get(c, [])))
        got = ", ".join(sorted(t.strip() for t in rows[c].get("rules", "").split(",") if t.strip()))
        if want != got:
            fail("S6", f"{c}: the compiler's ledger names rules '{got}', the spec says '{want}'"); bad += 1
    for c in sorted(set(cls) - set(rows)):
        fail("S6", f"codes.tsv classifies {c}, which is not in the ledger"); bad += 1
    for c in sorted(set(cmap) - set(rows)):
        fail("S6", f"a spec entry names {c}, which is not in the ledger"); bad += 1
    if bad == 0:
        n = sum(1 for v in cls.values() if v.startswith("unmigrated:"))
        ok(f"S6 codes: {len(rows)} ledger codes classified ({len(cmap)} rule, {n} unmigrated); rule tags equal the spec")


# ---------------------------------------------------------------------------------------------
# S7 sites
STD_SITE_RE = re.compile(r"^(std/src/[a-z0-9_/]+\.con)#([A-Za-z_][A-Za-z0-9_]*)$")
def is_std_site(n): return STD_SITE_RE.match(n) is not None

def std_site_has_comment(name, rid):
    """A std declaration `std/src/x.con#Name`: the file declares Name, and the `//` comment block
    directly above that declaration contains `spec: <rid>`."""
    m = STD_SITE_RE.match(name)
    p = os.path.join(ROOT, m.group(1))
    if not os.path.exists(p): return None
    lines = open(p, encoding="utf-8").read().split("\n")
    found = None
    for i, ln in enumerate(lines):
        if re.match(r"^\s*(pub\s+)?(trusted\s+)?(struct|enum|newtype|fn)\s+(Copy\s+)?" + re.escape(m.group(2)) + r"\b", ln):
            found = False
            k = i - 1
            while k >= 0 and lines[k].strip().startswith("//"):
                if re.search(r"spec: " + re.escape(rid) + r"\b", lines[k]): return True
                k -= 1
    return found
# ---------------------------------------------------------------------------------------------
DECL_RE = re.compile(r"^(?:@\[[^\]]*\]\s*)?(?:(?:private|protected|partial|noncomputable|unsafe|nonrec)\s+)*"
                     r"(def|theorem|lemma|abbrev|inductive|structure|instance|opaque|axiom)\s+([^\s(:\[{]+)")

def lean_decls(src_dir):
    """last-name -> [(file, start, end, text)] for every top-level declaration."""
    out = {}
    files = sorted(glob.glob(os.path.join(src_dir, "**", "*.lean"), recursive=True))
    main_lean = os.path.join(os.path.dirname(os.path.abspath(src_dir)), "Main.lean")
    if os.path.exists(main_lean): files.append(main_lean)   # the driver's root `main`
    for p in files:
        lines = open(p, encoding="utf-8").read().split("\n")
        starts = []
        for i, ln in enumerate(lines):
            m = DECL_RE.match(ln)
            if m: starts.append((i, m.group(2)))
        def lead(i):
            # the declaration's own leading comments: line comments and attributes directly above it,
            # and one doc comment block (never the previous declaration's body)
            b = i
            while b > 0:
                prev = lines[b - 1]
                if prev.startswith(("--", "@[")):
                    b -= 1
                elif prev.rstrip().endswith("-/"):
                    k = b - 1
                    while k >= 0 and not lines[k].startswith(("/-", "/--")):
                        k -= 1
                    if k < 0: break
                    b = k
                else:
                    break
            return b
        heads = [lead(i) for i, _ in starts]
        for k, (i, name) in enumerate(starts):
            j = heads[k + 1] if k + 1 < len(starts) else len(lines)
            b = heads[k]
            out.setdefault(name.split(".")[-1], []).append((p, b, j, "\n".join(lines[b:j])))
    return out

LEAN_CHUNK = 50  # names per #eval in the S7 Lean file (see check_sites)

def check_sites(spec, src_dir, lean_out):
    decls = lean_decls(src_dir)
    live = {e.id for e in spec.entries}
    bad, names = 0, []
    for e in spec.entries:
        for n in e.names("Semantics"):
            names.append((n, "any", e.id))
            cands = decls.get(n.split(".")[-1], [])
            if not any(re.search(r"spec: " + re.escape(e.id) + r"\b", t) for _, _, _, t in cands):
                fail("S7", f"{e.id}: Semantics declaration {n} has no `spec: {e.id}` comment" + ("" if cands else " (declaration not found)")); bad += 1
        if e.status()[0] in ("pending", "open"): continue
        en = e.names("Enforced by")
        if not en:
            fail("S7", f"{e.id}: no Enforced by: names"); bad += 1
        for n in en:
            if is_std_site(n):
                r = std_site_has_comment(n, e.id)
                if r is not True:
                    fail("S7", f"{e.id}: std declaration {n} has no `spec: {e.id}` comment" + ("" if r is False else " (declaration not found)")); bad += 1
                continue
            cands = decls.get(n.split(".")[-1], [])
            # the driver's root module (Main.lean) is not importable, so its declarations are checked
            # textually only; every other name must also resolve in Lean
            if not (("." not in n) and cands and all(p.endswith("Main.lean") for p, _, _, _ in cands)):
                names.append((n, "any", e.id))
            if not any(re.search(r"spec: " + re.escape(e.id) + r"\b", t) for _, _, _, t in cands):
                fail("S7", f"{e.id}: declaration {n} has no `spec: {e.id}` comment" + ("" if cands else " (declaration not found)")); bad += 1
        ev = e.fields.get("Evidence", "")
        if ev.startswith("proved: "): names.append((ev[8:], "thm", e.id))
        if ev.startswith("trusted: "):
            tcb = open(os.path.join(ROOT, "docs/verification/TRUSTED_COMPUTING_BASE.md"), encoding="utf-8").read()
            if ev[9:] not in tcb: fail("S7", f"{e.id}: trusted assumption {ev[9:]} is not in TRUSTED_COMPUTING_BASE.md"); bad += 1
    for p in sorted(glob.glob(os.path.join(src_dir, "**", "*.lean"), recursive=True)):
        for i, ln in enumerate(open(p, encoding="utf-8").read().split("\n")):
            for m in re.finditer(r"spec: (" + ID_RE + r")\b", ln):
                if m.group(1) not in live:
                    fail("S7", f"{os.path.relpath(p, ROOT)}:{i+1}: `spec: {m.group(1)}` names no live entry"); bad += 1
    for p in sorted(glob.glob(os.path.join(ROOT, "std", "src", "**", "*.con"), recursive=True)):
        for i, ln in enumerate(open(p, encoding="utf-8").read().split("\n")):
            for m in re.finditer(r"spec: (" + ID_RE + r")\b", ln):
                if m.group(1) not in live:
                    fail("S7", f"{os.path.relpath(p, ROOT)}:{i+1}: `spec: {m.group(1)}` names no live entry"); bad += 1
    with open(lean_out, "w") as f:
        # One #eval per LEAN_CHUNK names: a single do-block holding every name nests one match per
        # name, and Lean's elaborator hits its maximum recursion depth past about 240 of them. A chunk
        # that fails to elaborate prints no result line, which lean_result counts as a shortfall.
        head = ("open Lean Elab Command in\n#eval show CommandElabM Unit from do\n"
                "  let env ← getEnv\n"
                "  let all := env.constants.toList.map (·.1)\n"
                "  -- a `private` declaration is stored under a mangled name; resolve it by its user name\n"
                "  let find (n : Name) : Option ConstantInfo :=\n"
                "    match env.find? n with\n"
                "    | some ci => some ci\n"
                "    | none => (all.find? (fun m => privateToUserName? m == some n)).bind env.find?\n"
                "  let std := #[``propext, ``Classical.choice, ``Quot.sound]\n")
        f.write("import Concrete\n")
        for k, (n, want, rid) in enumerate(names):
            if k % LEAN_CHUNK == 0: f.write(head)
            f.write(f"  match find `{n} with\n")
            f.write(f"  | none => logInfo m!\"SPEC-LEAN missing {n} {rid}\"\n")
            if want == "thm":
                f.write(f"  | some ci =>\n    if !ci.isThm then logInfo m!\"SPEC-LEAN notthm {n} {rid}\"\n")
                f.write(f"    else\n      let axs ← liftCoreM (Lean.collectAxioms ci.name)\n")
                f.write(f"      if axs.any (fun a => !std.contains a) then logInfo m!\"SPEC-LEAN axioms {n} {rid}\"\n")
                f.write(f"      else logInfo m!\"SPEC-LEAN ok {n} {rid}\"\n")
            else:
                f.write(f"  | some _ => logInfo m!\"SPEC-LEAN ok {n} {rid}\"\n")
    with open(lean_out + ".count", "w") as f:
        f.write(str(len(names)))
    if bad == 0:
        ok(f"S7 sites: every non-pending entry's Enforced by: declarations carry its `spec:` comment; every `spec:` comment names a live entry")

def lean_result(out_path, count_path):
    out = open(out_path, encoding="utf-8", errors="replace").read()
    n = int(open(count_path).read())
    oks = len(re.findall(r"SPEC-LEAN ok ", out))
    bad = list(re.finditer(r"SPEC-LEAN (missing|notthm|axioms) (\S+) (\S+)", out))
    for m in bad:
        fail("S7", f"{m.group(3)}: Lean name {m.group(2)} {'does not resolve' if m.group(1)=='missing' else 'is not a theorem' if m.group(1)=='notthm' else 'depends on non-standard axioms'}")
    # Every name prints exactly one result line. Fewer means a chunk did not run (it failed to
    # elaborate, or Lean stopped), and that is reported even when another name already failed.
    tags = oks + len(bad)
    if n == 0 or tags != n:
        fail("S7", f"the Lean check produced a result for {tags} of {n} names (did Lean run?)")
    elif oks == n:
        ok(f"S7 Lean: {n} names resolve")


# ---------------------------------------------------------------------------------------------
# S9 manifests, anchors, S10 site
# ---------------------------------------------------------------------------------------------
def gen_manifest(spec, name):
    out, chs = MANIFESTS[name]
    lines = [MANIFEST_HEADER.format(name=name, chapters=", ".join(c + ".md" for c in chs)).rstrip("\n")]
    for ch in chs:
        for e in spec.entries:
            if e.chapter == ch:
                for r in e.rows:
                    lines.append("\t".join([e.id] + r))
    return out, "\n".join(lines) + "\n"

def anchors(spec):
    return {e.id: f"docs/spec/{e.chapter}.md#{e.id}" for e in spec.entries}

# Old site URLs of absorbed reference pages, kept alive as Zola redirects to the chapter that absorbed
# them (chapter -> URL paths). The reference generator (scripts/gen/site_reference.py) keeps the
# same kind of list for its own pages.
SITE_ALIASES = {
    "types": ["/reference/value-model/"],
}

def site_aliases(ch):
    paths = SITE_ALIASES.get(ch)
    return "aliases = [" + ", ".join(f'"{p}"' for p in paths) + "]\n" if paths else ""

def gen_site(spec, out_dir):
    os.makedirs(out_dir, exist_ok=True)
    for f in glob.glob(os.path.join(out_dir, "*.md")): os.remove(f)
    readme = open(os.path.join(spec.dir, "README.md"), encoding="utf-8").read()
    intro = readme.split("\n## 1.")[0].split("\n", 1)[1]
    open(os.path.join(out_dir, "_index.md"), "w").write(
        '+++\ntitle = "Language Specification"\ndescription = "The checked, readable statement of the Concrete language: '
        'every rule with a stable ID, checked against the compiler, the interpreter and Lean."\nsort_by = "weight"\n+++\n\n'
        "<!-- GENERATED from docs/spec by scripts/spec/spec.py site -- do not edit -->\n"
        + rewrite_links(intro, "README.md") + "\n")
    n = 0
    for w, ch in enumerate(spec.chapters):
        if ch not in spec.text: continue
        body = spec.text[ch].split("\n")
        title = body[0][2:].strip() if body and body[0].startswith("# ") else ch
        res, i = [], 1
        while i < len(body):
            ln = body[i]
            f = FENCE_RE.match(ln)
            if f and f.group(1).strip().startswith("con ") and f.group(1).strip() != "con pseudocode":
                x = Example(None, f.group(1).strip(), "", 0, ch)
                res.append(label(x)); res.append("```con")
                i += 1; continue
            res.append(ln); i += 1
        txt = rewrite_links("\n".join(res), ch + ".md")
        open(os.path.join(out_dir, ch + ".md"), "w").write(
            f'+++\ntitle = "{title}"\nweight = {w + 1}\n{site_aliases(ch)}+++\n\n<!-- GENERATED from docs/spec/{ch}.md by '
            f'scripts/spec/spec.py site -- do not edit -->\n{txt}\n')
        n += 1
    # The rule index: every entry with its chapter, kind, status and codes, one row each, so the
    # site shows the state of the whole specification at a glance (decision B18).
    rows = ["| ID | Title | Kind | Status | Codes |", "|---|---|---|---|---|"]
    for e in spec.entries:
        codes = ", ".join(c for c, _ in e.codes() if c) or "none"
        title = e.title.replace("|", "\\|")
        rows.append(f"| [{e.id}](@/spec/{e.chapter}.md#{e.id}) | {title} | {e.kind} | {e.fields.get('Status', '')} | {codes} |")
    open(os.path.join(out_dir, "rules.md"), "w").write(
        '+++\ntitle = "Rule Index"\nweight = 0\n+++\n\n<!-- GENERATED from docs/spec by '
        'scripts/spec/spec.py site -- do not edit -->\n# Rule Index\n\nEvery rule and definition of the '
        'specification, in chapter order. An entry\'s anchor is its ID; a cited rule names its version '
        '(`OWN-5@1`), and the registry `docs/spec/ids.tsv` keeps every version ever published.\n\n'
        + "\n".join(rows) + "\n")
    return n

def gen_single(spec):
    """The specification as one document: the chapters in the chapter map's order under one table of
    contents, each chapter's headings one level down, links between chapters made local."""
    order = [ch for ch in spec.chapters if ch in spec.text]
    titles = {ch: (spec.text[ch].split("\n", 1)[0][2:].strip() if spec.text[ch].startswith("# ") else ch) for ch in order}
    out = ["# The Concrete Language Specification", "",
           "<!-- GENERATED from docs/spec/*.md by `python3 scripts/spec/spec.py single`; do not edit: edit the chapters. -->", "",
           "This is the whole specification as one document, generated from its chapters in reading order. Each",
           "rule keeps its anchor, so `#REF-3` links here as in its chapter. How to read a rule is in the overview,",
           "the first chapter.", "", "## Contents", ""]
    for ch in order:
        out.append(f"- [{titles[ch]}](#ch-{ch})")
        for e in spec.entries:
            if e.chapter == ch:
                out.append(f"  - [{e.id}. {e.title}](#{e.id})")
    out.append("")
    def link(m):
        text, tgt, frag = m.group(1), m.group(2), m.group(3) or ""
        if tgt.endswith(".md") and "/" not in tgt and tgt[:-3] in spec.text:
            return f"[{text}]({frag or '#ch-' + tgt[:-3]})"
        return m.group(0)
    for ch in order:
        body = spec.text[ch].split("\n")
        res, fence = [f'<a id="ch-{ch}"></a>'], False
        for ln in body:
            if ln.startswith("```"):
                fence = not fence
            elif not fence and re.match(r"^#{1,5} ", ln):
                ln = "#" + ln
            if not fence:
                ln = re.sub(r"\[([^\]]*)\]\(([^)#\s]*)(#[^)\s]*)?\)", link, ln)
            res.append(ln)
        out += res + [""]
    return "\n".join(out).rstrip("\n") + "\n"

def label(x):
    a = x.attrs
    if x.verdict == "abort":
        s = "Accepted, aborts at run time"
    elif x.verdict == "accept":
        s = f"Accepted, exits {a.get('exit')}"
        if "interp-exempt" in a: s += f" (natively; interpreter exempt: {a['interp-exempt']})"
    else:
        s = f"Rejected: {a.get('code')} at {a.get('stage')}"
    if "pending" in a:
        s += f" (pending {a['pending']}; today: {a.get('now')})"
    return f"*{s}.*\n"

def rewrite_links(txt, here):
    def sub(m):
        text, tgt, frag = m.group(1), m.group(2), m.group(3) or ""
        if re.match(r"[a-z]+:", tgt) or tgt.startswith("#") or tgt.startswith("@"): return m.group(0)
        if tgt.endswith(".md") and "/" not in tgt and tgt[:-3] not in NON_CHAPTERS:
            return f"[{text}](@/spec/{tgt}{frag})"
        path = os.path.normpath(os.path.join("docs/spec", tgt))
        return f"[{text}]({REPO_URL}{path}{frag})"
    return re.sub(r"\[([^\]]*)\]\(([^)#\s]*)(#[^)\s]*)?\)", sub, txt)


# ---------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------
# S12 rule sites and negative witnesses (class 13: a documented rule with no enforcing site)
# ---------------------------------------------------------------------------------------------
INVENTORY_PATH = "docs/spec/rule_sites.tsv"
ALLOW_PATH = "docs/spec/coverage_allow.tsv"
INVENTORY_HEADER = """# GENERATED from docs/spec/*.md by `python3 scripts/spec/spec.py inventory` -- do not edit; edit the
# spec (check_spec.sh S12). One row per (refusal entry, diagnostic code): the rule, its status, the
# code, the stages it is reported at, and the declarations that enforce it (`Enforced by:`). S12
# requires every row of an enforced or partial refusal to have a negative witness: a spec reject
# example or a test program the compiler refuses with that code, unless coverage_allow.tsv names it
# with a reason. Columns: rule, status, code, stages, enforced_by.
"""
ALLOW_REASON_RE = re.compile(r"^(od:OD[0-9]+|R-[0-9]{4}|bug-[0-9]{3}|unreachable:\S.*)$")
WITNESS_GLOBS = ["tests/**/*.con"]
# Every program under tests/ (outside a package) and every non-project spec reject example is a
# candidate. S12 compiles each one itself on every run, so S12 is the gate that executes its
# witnesses: deleting the only site that reports a rule's code makes S12 fail, whether or not
# another runner also runs the program.

def inventory_rows(spec):
    rows = []
    for e in spec.entries:
        if e.kind != "refusal": continue
        st = " ".join(x for x in e.status() if x)
        sites = ", ".join(e.names("Enforced by")) or "-"
        for c, stages in e.codes():
            if c is None: continue
            rows.append((e.id, st, c, ",".join(stages) if isinstance(stages, list) else stages, sites))
    return rows

def gen_inventory(spec):
    lines = [INVENTORY_HEADER.rstrip("\n")]
    for r in inventory_rows(spec):
        lines.append("\t".join(r))
    return "\n".join(lines) + "\n"

def check_inventory(spec, root):
    txt = gen_inventory(spec)
    p = os.path.join(root, INVENTORY_PATH)
    cur = open(p, encoding="utf-8").read() if os.path.exists(p) else ""
    if cur != txt: fail("S12", f"{INVENTORY_PATH} differs from `spec.py inventory` (hand edit or stale; regenerate)")
    else: ok(f"S12 inventory: {INVENTORY_PATH} equals its generator's output ({len(inventory_rows(spec))} rule codes)")

ERR_ALL_RE = re.compile(r"error\[([a-z-]+)\]: \((E[0-9]{4})\)")

def observe_codes(spec, cc, jobs, workdir, root):
    """Compile every negative-witness candidate (test programs and the spec's non-project reject
    examples) and record each diagnostic code each one reports: {code: [witness, ...]}."""
    os.makedirs(workdir, exist_ok=True)
    def in_project(f):
        d = os.path.dirname(f)
        while len(d) > len(root):
            if os.path.exists(os.path.join(d, "Concrete.toml")): return True
            d = os.path.dirname(d)
        return False
    srcs = []
    for g in WITNESS_GLOBS:
        for f in sorted(glob.glob(os.path.join(root, g), recursive=True)):
            rel = os.path.relpath(f, root)
            # a project's sources compile only as a package; single files (and their `mod x;` files) only
            if not in_project(f):
                srcs.append((rel, f))
    for i, x in enumerate(x for e in spec.entries for x in e.examples
                          if not x.errors and x.verdict == "reject" and not x.attrs.get("project")):
        p = os.path.join(workdir, f"rej{i:04d}.con")
        open(p, "w").write(x.body)
        srcs.append((f"spec:{x.key}", p))
    def one(item):
        # a refusal counts only when the compiler exits normally with a failure status; each error is
        # keyed by its code and the rule its diagnostic is tagged with (`rule: ID@n`, None if untagged)
        name, path = item
        rc, out, _ = run([cc, path, "-o", os.path.join(workdir, "w%d.bin" % (hash(name) & 0xffffff))], timeout=120)
        if rc is None or rc <= 0 or rc >= 128 or LEAK_RE.search(out or ""):
            return name, set()
        found, lines = set(), (out or "").split("\n")
        for i, ln in enumerate(lines):
            m = ERR_ALL_RE.search(ln)
            if not m: continue
            tag = None
            for l2 in lines[i + 1:]:
                if ERR_ALL_RE.search(l2) or l2.startswith("error"): break
                t = re.match(r"^\s+rule: ([A-Z]+-D?[0-9]+)@[0-9]+$", l2)
                if t: tag = t.group(1); break
            found.add((m.group(2), tag))
        return name, found
    seen = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as ex:
        for name, codes in ex.map(one, srcs):
            for c in codes: seen.setdefault(c, []).append(name)
    if not srcs: fail("S12", "no witness candidates found (refusing a vacuous run)")
    return seen, len(srcs)

def check_code_coverage(spec, root, seen, n):
    allow = {}
    for r in read_tsv(os.path.join(root, ALLOW_PATH)) or []:
        if len(r) != 3: fail("S12", f"{ALLOW_PATH} row {r} does not have 3 columns (code, rule, reason)"); continue
        if not ALLOW_REASON_RE.match(r[2]):
            fail("S12", f"{ALLOW_PATH}: {r[0]} {r[1]}: reason '{r[2]}' is not od:ODn, R-NNNN, bug-NNN or unreachable:<why>")
        elif not r[2].startswith("unreachable:") and not tracking_exists(r[2][3:] if r[2].startswith("od:") else r[2], root):
            fail("S12", f"{ALLOW_PATH}: {r[0]} {r[1]}: reason '{r[2]}' names a tracking ID that does not exist")
        if (r[0], r[1]) in allow: fail("S12", f"{ALLOW_PATH}: {r[0]} {r[1]} listed twice")
        allow[(r[0], r[1])] = r[2]
    rows = inventory_rows(spec)
    keys = {(c, rid) for rid, st, c, _, _ in rows}
    covered = missing = allowed = 0
    for rid, st, c, stages, sites in rows:
        if st.startswith("pending") or c == "new": continue
        # a witness is a refusal with this code tagged with this rule; an untagged one counts only for
        # a code that this rule alone names
        sole = len(code_map(spec).get(c, set())) <= 1
        w = seen.get((c, rid), []) + (seen.get((c, None), []) if sole else [])
        if w:
            covered += 1
            if (c, rid) in allow:
                fail("S12", f"{ALLOW_PATH}: {c} {rid} is allow-listed but {w[0]} now witnesses it (remove the row)")
        elif (c, rid) in allow:
            allowed += 1
            print(f"  ALLOWED {rid} {c}: {allow[(c, rid)]}")
        else:
            missing += 1
            fail("S12", f"{rid} names {c} ({stages}), enforced by {sites}, but no test program or reject example is refused with {c}: add a negative fixture, or an allow row with its reason")
    for k in sorted(set(allow) - keys):
        fail("S12", f"{ALLOW_PATH}: {k[0]} {k[1]} names no (rule, code) of the spec")
    print(f"  S12 witnesses: {covered} rule codes witnessed, {allowed} allow-listed, {missing} missing, over {n} programs")
    if missing == 0: ok(f"S12 witnesses: every enforced rule code has a negative witness or a stated reason")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("cmd")
    ap.add_argument("arg", nargs="?")
    ap.add_argument("--spec", default=os.path.join(ROOT, "docs", "spec"))
    ap.add_argument("--root", default=ROOT)
    ap.add_argument("--scan-root")
    ap.add_argument("--src", default=os.path.join(ROOT, "Concrete"))
    ap.add_argument("--compiler", default=os.path.join(ROOT, ".lake/build/bin/concrete"))
    ap.add_argument("--jobs", type=int, default=8)
    ap.add_argument("--work")
    ap.add_argument("--observed")
    ap.add_argument("--ledger")
    ap.add_argument("--base")
    ap.add_argument("--base-ids")
    ap.add_argument("--base-codes")
    ap.add_argument("--base-spec")
    ap.add_argument("--out")
    ap.add_argument("--count")
    ap.add_argument("--only", action="append")
    ap.add_argument("--check", action="store_true")
    a = ap.parse_args()
    spec = Spec(a.spec)
    plan = os.path.join(a.spec, "MAINTAINERS.md")
    plan = plan if os.path.exists(plan) else os.path.join(a.spec, "README.md")
    spec.chapters_planned = set(spec.chapters) | set(re.findall(r"\*\*([a-z]+)\.md\*\*", open(plan).read()))
    c = a.cmd
    if c == "lint":
        check_format(spec, a.root); check_registry(spec); check_coverage(spec); check_tracking(spec, a.root)
        check_restatements(spec, a.root, a.scan_root)
        check_inventory(spec, a.root)
    elif c == "register":
        # append-only: add a row for every chapter key ids.tsv lacks; never edits an existing row
        path = os.path.join(spec.dir, "ids.tsv")
        have = {r[0] for r in (read_tsv(path) or [])}
        new = [(k, st, h) for k, (st, h) in registry_expect(spec).items() if k not in have]
        with open(path, "a") as f:
            for k, st, h in new: f.write(f"{k}\t{st}\t{h}\n")
        print(f"registered {len(new)} new keys")
    elif c == "bump":
        # a changed sentence gets a new version: the chapter's tag moves to @n+1, the old version
        # row becomes `superseded` (its only allowed transition) and the new row is appended
        e = spec.by_id().get(a.arg)
        if e is None: ap.error(f"no entry {a.arg}")
        old, new = e.tag, f"{e.id}@{e.version + 1}"
        cp = os.path.join(spec.dir, e.chapter + ".md")
        txt = open(cp, encoding="utf-8").read()
        assert txt.count(f"**{old}.**") == 1
        open(cp, "w", encoding="utf-8").write(txt.replace(f"**{old}.**", f"**{new}.**"))
        path = os.path.join(spec.dir, "ids.tsv")
        lines = open(path, encoding="utf-8").read().split("\n")
        lines = [("\t".join([old, "superseded", l.split("\t")[2]]) if l.split("\t")[0] == old else l) for l in lines]
        spec2 = Spec(spec.dir)
        lines = [l for l in lines if l.strip()] + [f"{new}\tlive\t{sentence_hash(spec2.by_id()[e.id])}"]
        open(path, "w", encoding="utf-8").write("\n".join(lines) + "\n")
        print(f"{old} -> {new}; update the compiler's tags for {e.id}")
    elif c == "restatements":
        check_restatements(spec, a.root, a.scan_root)
    elif c == "history":
        check_history(spec, a.base, a.base_ids, a.base_codes, a.base_spec)
        if a.base and a.base != "ctl": check_allow_history(spec, a.root, a.base)
    elif c == "run-examples":
        run_examples(spec, a.compiler, a.jobs, a.work, a.only)
    elif c == "codes":
        if a.ledger: check_codes(spec, a.ledger)
        else:
            for k, v in sorted(code_map(spec).items()): print(f"{k}\t{', '.join(sorted(v))}")
    elif c == "anchors":
        for k, v in anchors(spec).items(): print(f"{k}\t{v}")
    elif c == "sites":
        check_sites(spec, a.src, a.out)
    elif c == "lean-result":
        lean_result(a.out, a.count)
    elif c == "manifest":
        path, txt = gen_manifest(spec, a.arg)
        if a.check:
            cur = open(os.path.join(a.root, path), encoding="utf-8").read() if os.path.exists(os.path.join(a.root, path)) else ""
            if cur != txt: fail("S9", f"{path} differs from `spec.py manifest {a.arg}` (hand edit or stale; regenerate)")
            else: ok(f"S9 manifest: {path} equals its generator's output ({txt.count(chr(10)) - MANIFEST_HEADER.count(chr(10))} rows)")
        else:
            open(os.path.join(a.root, path), "w").write(txt) if a.out is None else open(a.out, "w").write(txt)
    elif c == "inventory":
        if a.check: check_inventory(spec, a.root)
        else:
            open(os.path.join(a.root, INVENTORY_PATH) if a.out is None else a.out, "w").write(gen_inventory(spec))
    elif c == "code-coverage":
        if a.observed:   # a recorded observation (the negative controls replay one without compiling)
            seen, n = {}, 0
            for r in read_tsv(a.observed) or []:
                seen[(r[0], None if r[1] == "-" else r[1])] = [r[3]]; n += 1
        else:
            seen, n = observe_codes(spec, a.compiler, a.jobs, a.work or os.path.join("/tmp", "spec-s12"), a.root)
        if a.out:
            with open(a.out, "w") as f:
                for k in sorted(seen, key=lambda k: (k[0], k[1] or "")):
                    f.write(f"{k[0]}\t{k[1] or '-'}\t{len(seen[k])}\t{seen[k][0]}\n")
        check_code_coverage(spec, a.root, seen, n)
    elif c == "single":
        txt = gen_single(spec)
        path = a.out or os.path.join(a.root, SINGLE_PATH)
        if a.check:
            cur = open(path, encoding="utf-8").read() if os.path.exists(path) else ""
            if cur != txt: fail("S9", f"{os.path.relpath(path, a.root)} differs from `spec.py single` (hand edit or stale; regenerate)")
            else: ok(f"S9 single page: {os.path.relpath(path, a.root)} equals its generator's output ({len(spec.chapters)} chapters, {len(spec.entries)} entries)")
        else:
            open(path, "w", encoding="utf-8").write(txt)
            print(f"wrote {os.path.relpath(path, a.root)}")
    elif c == "site":
        n = gen_site(spec, a.out)
        print(f"wrote {n} chapter pages to {a.out}")
    else:
        ap.error(f"unknown command {c}")
    sys.exit(1 if FAILS else 0)

if __name__ == "__main__":
    main()
