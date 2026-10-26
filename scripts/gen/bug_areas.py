"""Derive each bug's compiler area and dates from the bug records and git history (no other input).

Imported by scripts/gen/bug_ledger.py, which owns the command line (`--write`/`--check` for the
generated view docs/bugs/AREAS.md, `report` for the research tables). Nothing here is a fact store:
every value below is recomputed from three sources, each the single owner of its fact.

  docs/bugs/NNN_*.md        status, silent, classes, fixtures and the optional `area:` override
  docs/bugs/area_map.tsv    which area a repository path belongs to (path prefix -> tier, stage,
                            module); the only hand-kept table here, and it holds no bug fact
  git history (HEAD)        which commits fixed each bug, what they touched, and the dates

FIX COMMITS. A non-merge commit, authored on or after the ledger's first record (earlier history
belongs to another implementation and its "bug #N" are issue numbers), is a fix commit of bug N when
  1. its subject names N ("bug 297", "Bug 006", "bugs 027, 248 and 250", "bugs 273-282",
     "bugs 027/250", "bug-280", "Bug 007 (print), 010 (substr)"; numbers are written with three
     digits, a range spans at most 30 bugs), or
  2. its body names N and it touches N's record or one of N's fixtures, and at most four records.
A commit that names several bugs and touches the record or fixtures of only some of them belongs to
those; otherwise it is shared, each named bug taking an equal share of its changed lines. A bug
with no such commit falls back to the commit that added its record and the commit that first marked
it FIXED, when that commit touched at most four records (old bugs filed with their fix).
RECORD-NAMED FILES. When the record names some of a fix commit's code files (`Lower.lean`,
`Concrete/IR/Lower.lean`, `map.con`) — in its `## Fix` sections, status and `**Fixed in:**` lines
when those name any, else anywhere in the record — those files decide the bug's primary area, and in
a commit shared with other bugs they count for it in full: the record says where its fix lives. The
commit's other files still count, at the commit's share, for the weights and secondary areas (the
naming is evidence of where the fix is, not an exhaustive list).
Duplicate commits (a cherry-pick or a rebased copy reachable twice: same author, date and subject)
count once.

WEIGHT. A fix commit's changed lines (added + deleted, per `git log --numstat`) are summed per
(stage, module) of each path's area_map row. Rows have tiers: code (compiler, std and tooling
source), gate (gates, harness, CI), docs, ignore (tests, fixtures, examples, records and generated
files: never weighted). A bug's PRIMARY area is the heaviest (stage, module) of the highest tier it
has any weight in (code, then gate, then docs), restricted to the record-named files when it has
any; for an OPEN bug only code counts, because a filing commit's gate leg pins the bug, it does not
locate it. Its SECONDARY areas are the other stages of
that tier holding at least a tenth of its weight. Because records and generated views carry no
weight, regenerating a view after a fix never changes any bug's area: a regeneration commit (or an
amend that adds the regenerated files) is a fixed point.

BASIS (how a bug's primary area was obtained), reported per bug:
  override   the record's `area:` line (it replaces the derived primary; secondaries stay derived)
  commits    fix commits, at least half of the primary tier's weight from commits naming only it
  named      fix commits mostly shared with other bugs, placed by the files the record names
  shared     fix commits, more than half of that weight from commits shared with other bugs that
             the record does not disambiguate
  record     the record-commit fallback
  none       no weighted commit: an open bug not fixed yet (unfixed) or a fixed bug the history
             cannot place (underived)
"ambiguous" in the report means shared, record or underived: the bugs an `area:` override may be needed for.

DATES. found = the author date of the first commit adding the record (docs/bugs/NNN_*.md); fixed =
the author date of the first commit whose version of the record reads FIXED (the status vocabulary of
bug_ledger.py); an open bug has none. Author dates survive a rebase, so a landed branch keeps them.

HISTORY. The history is the git repository at the given directory, and must be the project's own: a
shallow clone is refused (exit 2: the derivation would silently lose commits), and a directory that
is not a git work tree, or whose history starts with the bug records already present (a scratch copy
committed as one snapshot), has no usable history: the AREAS.md view is then not regenerated or
compared, and the caller says so.
"""
import os
import re
import subprocess
from collections import defaultdict

STAGES = ["Parse", "Resolve", "Check", "Elab", "Mono", "Lower", "SSA", "EmitSSA", "EmitLLVM", "Interp",
          "Proof", "Report", "Std", "CLI", "Tooling", "Gates", "Docs"]
TIERS = ["code", "gate", "docs"]
MAP_COLS = ["prefix", "tier", "stage", "module"]
RECORD = re.compile(r"^docs/bugs/(\d{3})_[^/]*\.md$")
SECONDARY_SHARE = 0.10
MAX_RECORDS_TOUCHED = 4
MAX_RANGE = 30
AREA_VALUE = re.compile(r"^([A-Za-z]+)(?:/([A-Za-z0-9_.+-]+))?(?:\s+—\s+(\S.*))?$")

BEGIN = ("<!-- BEGIN GENERATED: bug areas (scripts/gen/bug_ledger.py; derived from the records, "
         "docs/bugs/area_map.tsv and git history, never edited here) -->")
END = "<!-- END GENERATED: bug areas -->"

_ITEM = r"#?\d{3}(?!\d)(?:\s*\([^()\n]{0,60}\))?"
_SEP = r"\s*(?:,|/|&|\+|\band\b|-|–|\bto\b)\s*(?:bugs?\s*)?"
NAMES = re.compile(r"\bbugs?\b[ _#-]*(" + _ITEM + r"(?:" + _SEP + _ITEM + r")*)", re.I)


class AreaError(Exception):
    """A malformed input: the caller turns it into exit 2."""


class NoHistory(Exception):
    """The directory has no usable project history; the AREAS view is skipped (the reason is the
    message)."""


def parse_area(value, name):
    """`area: Stage[/module][ — note]` -> (stage, module or None). An unknown stage is refused."""
    m = AREA_VALUE.match(value)
    if not m:
        raise AreaError(f"{name}: area must be 'Stage', 'Stage/module' or either followed by ' — note', "
                        f"got '{value[:60]}'")
    if m.group(1) not in STAGES:
        raise AreaError(f"{name}: area names an unknown stage '{m.group(1)}' (known: {', '.join(STAGES)})")
    return m.group(1), m.group(2)


def names_in(text, known):
    """Bug numbers (three-digit strings, members of `known`) a commit message names."""
    out = []
    for m in NAMES.finditer(text):
        body = re.sub(r"\([^()\n]*\)", " ", m.group(1))
        toks = re.findall(r"\d{3}|-|–|\bto\b", body)
        prev, rng = None, False
        for t in toks:
            if t in ("-", "–", "to"):
                rng = True
                continue
            n = int(t)
            if rng and prev is not None:
                if prev < n <= prev + MAX_RANGE:
                    out += range(prev + 1, n + 1)
            else:
                out.append(n)
            prev, rng = n, False
    return sorted({f"{n:03d}" for n in out} & known)


# ----------------------------------------------------------------------------- the area map
def load_map(root):
    path = os.path.join(root, "docs", "bugs", "area_map.tsv")
    try:
        text = open(path, encoding="utf-8").read()
    except OSError as e:
        raise AreaError(f"cannot read {path}: {e}")
    rows = [ln.split("\t") for ln in text.split("\n") if ln and not ln.startswith("#")]
    if not rows or rows[0] != MAP_COLS:
        raise AreaError(f"{path}: the first row must be the header {' '.join(MAP_COLS)}")
    out, seen = [], set()
    for cells in rows[1:]:
        if len(cells) != 4 or not all(cells):
            raise AreaError(f"{path}: a row needs four non-empty tab-separated cells: {cells}")
        prefix, tier, stage, module = cells
        if prefix in seen:
            raise AreaError(f"{path}: prefix {prefix} has two rows")
        seen.add(prefix)
        if tier == "ignore":
            if stage != "-" or module != "-":
                raise AreaError(f"{path}: an ignore row has stage and module '-': {prefix}")
        elif tier not in TIERS:
            raise AreaError(f"{path}: unknown tier '{tier}' for {prefix} (code, gate, docs or ignore)")
        elif stage not in STAGES:
            raise AreaError(f"{path}: unknown stage '{stage}' for {prefix} (known: {', '.join(STAGES)})")
        out.append((prefix, tier, stage, module))
    out.sort(key=lambda r: -len(r[0]))
    return out


def area_of(path, amap):
    """(tier, stage, module) of a path, or None when no row matches."""
    for prefix, tier, stage, module in amap:
        if path.startswith(prefix):
            if tier == "ignore":
                return ("ignore", None, None)
            if module == "*":
                module = os.path.splitext(os.path.basename(path))[0] or prefix.rstrip("/")
            return (tier, stage, module)
    return None


# ----------------------------------------------------------------------------- git
# A hook exports GIT_DIR and friends; read the history of the directory named, never an inherited one.
_LOCAL_ENV = {"GIT_ALTERNATE_OBJECT_DIRECTORIES", "GIT_CONFIG", "GIT_CONFIG_PARAMETERS", "GIT_CONFIG_COUNT",
              "GIT_OBJECT_DIRECTORY", "GIT_DIR", "GIT_WORK_TREE", "GIT_IMPLICIT_WORK_TREE", "GIT_GRAFT_FILE",
              "GIT_INDEX_FILE", "GIT_NO_REPLACE_OBJECTS", "GIT_REPLACE_REF_BASE", "GIT_PREFIX",
              "GIT_SHALLOW_FILE", "GIT_COMMON_DIR", "GIT_QUARANTINE_PATH"}


def _env():
    return {k: v for k, v in os.environ.items() if k not in _LOCAL_ENV and not k.startswith("GIT_CONFIG_KEY_")
            and not k.startswith("GIT_CONFIG_VALUE_")}


def _run(gitdir, args, data=None):
    return subprocess.run(["git", "-C", gitdir, "-c", "core.quotepath=off", "--no-replace-objects", *args],
                          input=data, capture_output=True, env=_env())


def _git(gitdir, *args, data=None):
    p = _run(gitdir, args, data)
    if p.returncode != 0:
        raise AreaError(f"git {' '.join(args[:3])} failed in {gitdir}: "
                        f"{p.stderr.decode(errors='replace').strip()[:200]}")
    return p.stdout.decode("utf-8", errors="replace")


def check_history(gitdir):
    """Refuse a shallow clone; raise NoHistory for a directory with no project history."""
    p = _run(gitdir, ["rev-parse", "--show-toplevel", "--is-shallow-repository"])
    if p.returncode != 0:
        raise NoHistory(f"{gitdir} is not a git work tree")
    top, shallow = (p.stdout.decode().split("\n") + ["", ""])[:2]
    if os.path.realpath(top) != os.path.realpath(gitdir):
        raise NoHistory(f"{gitdir} is not the top of a git work tree (it lies inside {top})")
    if shallow.strip() == "true":
        raise AreaError(f"{gitdir} is a shallow clone: the bug areas derive from the whole history "
                        "(fetch it with `git fetch --unshallow`, or check out with fetch-depth: 0)")
    roots = _git(gitdir, "rev-list", "--max-parents=0", "HEAD").split()
    for r in roots:
        if any(RECORD.match(p) for p in _git(gitdir, "ls-tree", "-r", "--name-only", r, "docs/bugs/").split("\n")):
            raise NoHistory(f"the history of {gitdir} starts with the bug records already present "
                            f"(root commit {r[:10]}), so it is a snapshot copy, not the project's history")


def _log(gitdir, *args):
    """[(hash, author ident, author date, subject, body)] of non-merge commits, newest first."""
    out = []
    raw = _git(gitdir, "log", "--no-merges", "--format=%x01%H%x1f%an <%ae> %aI%x1f%as%x1f%s%x1f%b", *args)
    for ent in raw.split("\x01")[1:]:
        h, ident, date, subj, body = ent.split("\x1f", 4)
        out.append((h, ident, date, subj, body.rstrip("\n")))
    return out


def _numstat(gitdir, hashes):
    """{hash: [(path, changed lines)]} for the given commits (renames resolved to the new path)."""
    stat = {h: [] for h in hashes}
    if not hashes:
        return stat
    raw = _git(gitdir, "log", "--no-walk=unsorted", "--stdin", "-M", "--numstat", "-z", "--format=%x01%H",
               data=("\n".join(hashes) + "\n").encode())
    for ent in raw.split("\x01")[1:]:
        h, _, rest = ent.partition("\0")
        h = h.strip()
        toks = rest.split("\0")
        i = 0
        while i < len(toks):
            t = toks[i].lstrip("\n")
            if not t:
                i += 1
                continue
            parts = t.split("\t")
            if len(parts) == 3 and parts[2] == "":      # a rename: old and new paths follow
                path = toks[i + 2] if i + 2 < len(toks) else ""
                i += 3
            else:
                path = parts[2] if len(parts) == 3 else ""
                i += 1
            a, d = parts[0], parts[1] if len(parts) > 1 else "0"
            n = (int(a) if a.isdigit() else 0) + (int(d) if d.isdigit() else 0)
            if path:
                stat[h].append((path, n))
    return stat


def _record_history(gitdir):
    """[(hash, date, status letter, path)] of non-merge commits touching a bug record, newest first."""
    raw = _git(gitdir, "log", "--no-merges", "-M", "--name-status", "-z", "--format=%x01%H%x1f%as",
               "--", "docs/bugs/")
    out = []
    for ent in raw.split("\x01")[1:]:
        head, _, rest = ent.partition("\0")
        h, date = head.strip().split("\x1f")
        toks = [t.lstrip("\n") for t in rest.split("\0")]
        i = 0
        while i < len(toks):
            st = toks[i]
            if not st:
                i += 1
                continue
            if st[0] in "RC":
                old, new = toks[i + 1], toks[i + 2]
                i += 3
                mo, mn = RECORD.match(old), RECORD.match(new)
                if mn:
                    added = not mo or mo.group(1) != mn.group(1)
                    out.append((h, date, "A" if added else "M", new))
                continue
            path = toks[i + 1] if i + 1 < len(toks) else ""
            i += 2
            if RECORD.match(path) and st[0] in "AM":
                out.append((h, date, st[0], path))
    return out


def _blobs(gitdir, specs):
    """{"<hash>:<path>": text} via one `git cat-file --batch`."""
    if not specs:
        return {}
    p = _run(gitdir, ["cat-file", "--batch"], ("\n".join(specs) + "\n").encode())
    if p.returncode != 0:
        raise AreaError(f"git cat-file failed in {gitdir}")
    out, data, pos = {}, p.stdout, 0
    for spec in specs:
        nl = data.index(b"\n", pos)
        head = data[pos:nl].decode()
        pos = nl + 1
        if head.endswith(" missing"):
            out[spec] = None
            continue
        size = int(head.split()[2])
        out[spec] = data[pos:pos + size].decode("utf-8", errors="replace")
        pos += size + 1
    return out


# ----------------------------------------------------------------------------- derivation
def derive(gitdir, root, recs, status_open):
    """{num: facts} for every record. `recs` is bug_ledger's record map; `status_open(text)` reads a
    record's status (True open, False fixed, None unreadable). Raises AreaError on a malformed input,
    an unmapped path or an override naming an unknown stage."""
    amap = load_map(root)
    known = set(recs)
    rec_hist = _record_history(gitdir)
    if not rec_hist:
        raise NoHistory(f"no commit in the history of {gitdir} touches a bug record")
    epoch = min(d for _, d, _, _ in rec_hist)

    # found and fixed: walk each record's versions oldest first
    per_rec = defaultdict(list)
    for idx, (h, d, st, path) in enumerate(rec_hist):
        num = RECORD.match(path).group(1)
        per_rec[num].append((idx, h, d, st, path))
    blobs = _blobs(gitdir, sorted({f"{h}:{path}" for h, _, _, path in rec_hist if RECORD.match(path).group(1) in known}))
    dates = {}
    for num in known:
        vers = sorted(per_rec.get(num, []), key=lambda v: (v[2], -v[0]))   # by date, then topology
        adds = [v for v in vers if v[3] == "A"] or vers[:1]
        found = adds[0][2] if adds else None
        add_commit = adds[0][1] if adds else None
        fixed = fixed_commit = None
        filed_fixed = False
        if not recs[num]["open"]:
            for v in vers:
                if status_open(blobs.get(f"{v[1]}:{v[4]}") or "") is False:
                    fixed, fixed_commit = v[2], v[1]
                    filed_fixed = v[1] == add_commit
                    break
        dates[num] = {"found": found, "fixed": fixed, "filed_fixed": filed_fixed,
                      "add_commit": add_commit, "fixed_commit": fixed_commit}

    # fix commits
    commits, seen_dup = [], set()
    for h, ident, adate, subj, body in _log(gitdir):
        if adate < epoch:
            continue
        key = (ident, subj)
        if key in seen_dup:
            continue
        seen_dup.add(key)
        commits.append((h, adate, subj, body))
    fallback = {dates[n]["add_commit"] for n in known} | {dates[n]["fixed_commit"] for n in known}
    named = {}
    for h, adate, subj, body in commits:
        s, b = names_in(subj, known), names_in(body, known)
        if s or b or h in fallback:
            named[h] = (s, b)
    stats = _numstat(gitdir, sorted(named))

    def own_paths(num):
        r = recs[num]
        fx = r["facts"].get("fixtures", "") if r["facts"] else ""
        return {"tests/programs/" + f for f in fx.split()}

    owns = {n: own_paths(n) for n in known}

    def touched(n, paths):
        return any(p.startswith(f"docs/bugs/{n}_") for p in paths) or bool(paths & owns[n])

    attributed = defaultdict(list)      # num -> [(hash, share, via)]
    for h, (s, b) in named.items():
        paths = {p for p, _ in stats.get(h, [])}
        records_touched = {RECORD.match(p).group(1) for p in paths if RECORD.match(p)}
        touches = {n for n in set(s) | set(b) if touched(n, paths)}
        bugs, via = list(s), "subject"
        if not bugs and len(records_touched) <= MAX_RECORDS_TOUCHED:
            bugs, via = [n for n in b if n in touches], "body"
        if len(bugs) > 1 and set(bugs) & touches:
            bugs = sorted(set(bugs) & touches)
        for n in bugs:
            attributed[n].append((h, 1.0 / len(bugs), via))

    unmapped = set()
    out = {}
    for num in sorted(known):
        r = recs[num]
        commits_used = attributed.get(num, [])
        basis = "commits"
        if not _weighted(commits_used, stats, amap, unmapped):
            fb = []
            for h in (dates[num]["add_commit"], dates[num]["fixed_commit"]):
                if not h or any(h == c for c, _, _ in fb):
                    continue
                recs_in = {RECORD.match(p).group(1) for p, _ in stats.get(h, []) if RECORD.match(p)}
                if len(recs_in) <= MAX_RECORDS_TOUCHED:
                    mine = sorted(recs_in & known) or [num]
                    fb.append((h, 1.0 / len(mine), "record"))
            commits_used, basis = fb, "record"
        tiers = {t: defaultdict(float) for t in TIERS}
        fixed_in = {t: defaultdict(float) for t in TIERS}
        single = {t: 0.0 for t in TIERS}
        whole = _record_text(root, r["file"])
        fix_text = _fix_text(whole)
        resolved = []
        for h, share, via in commits_used:
            paths = stats.get(h, [])
            # The code files of a fix commit that the record names (Lower.lean, map.con) are where the
            # record says the fix lives: its fix section when that names any, else the whole record.
            # They decide the primary area, and in a commit shared with other bugs they are this
            # bug's part in full. The commit's other files still count, at the commit's share, for
            # the secondary areas and the weights.
            code = [p for p, _ in paths if (area_of(p, amap) or ("ignore",))[0] == "code"]
            mine = _named(code, fix_text) or _named(code, whole)
            if mine:
                via += "+record-names"
            resolved.append((h, share, via))
            for path, n in paths:
                a = area_of(path, amap)
                if a is None:
                    unmapped.add(path)
                    continue
                if a[0] == "ignore":
                    continue
                full = path in mine
                tiers[a[0]][(a[1], a[2])] += n * (1.0 if full else share)
                if full:
                    fixed_in[a[0]][(a[1], a[2])] += n
                if share == 1.0:
                    single[a[0]] += n
        commits_used = resolved
        usable = ["code"] if r["open"] else TIERS
        tier = next((t for t in usable if sum(tiers[t].values()) > 0), None)
        primary, secondary, single_frac = None, [], 0.0
        if tier:
            w = tiers[tier]
            total = sum(w.values())
            decide = fixed_in[tier] or w
            primary = max(decide.items(), key=lambda kv: (kv[1], kv[0][0], kv[0][1]))[0]
            by_stage = defaultdict(float)
            for (st, _), v in w.items():
                by_stage[st] += v
            secondary = sorted(st for st, v in by_stage.items()
                               if st != primary[0] and v >= SECONDARY_SHARE * total)
            single_frac = single[tier] / total
            if single_frac < 0.5 and basis == "commits":
                basis = "named" if fixed_in[tier] else "shared"
        else:
            basis = "none"
        override = r["facts"].get("area") if r["facts"] else None
        if override:
            st, mod = parse_area(override, r["file"])
            primary = (st, mod or (primary[1] if primary and primary[0] == st else "-"))
            secondary = [s2 for s2 in secondary if s2 != st]
            basis = "override"
        out[num] = {
            "stage": primary[0] if primary else None,
            "module": primary[1] if primary else None,
            "tier": tier,
            "secondary": secondary,
            "basis": basis,
            "single_share": round(single_frac, 3),
            "weights": {t: {f"{k[0]}/{k[1]}": round(v, 2) for k, v in sorted(tiers[t].items())} for t in TIERS},
            "commits": [{"hash": h, "share": round(sh, 3), "via": via} for h, sh, via in commits_used],
            **{k: dates[num][k] for k in ("found", "fixed", "filed_fixed")},
        }
    if unmapped:
        shown = sorted(unmapped)
        raise AreaError("docs/bugs/area_map.tsv maps no row to these paths a fix commit touched: "
                        + ", ".join(shown[:12]) + (f" (and {len(shown) - 12} more)" if len(shown) > 12 else "")
                        + "; add a row for each (the longest matching prefix wins)")
    return out


FIX_HEAD = re.compile(r"^#{2,3} +(?:Fix|Fixed)\b.*$", re.M | re.I)
FIX_LINE = re.compile(r"^(?:\*\*Status:\*\*|Status:|\*\*Fix(?:ed)?(?: in| by)?:\*\*).*$", re.M)


def _named(paths, text):
    """The paths whose file name the text spells (`Lower.lean`, `Concrete/IR/Lower.lean`)."""
    return {p for p in paths if text
            and re.search(r"(?<![\w.-])" + re.escape(os.path.basename(p)) + r"(?![\w-])", text)}


def _fix_text(text):
    """The parts of a record that say where its fix lives: every `## Fix…` / `## Fixed…` section and
    the status and `**Fixed in:**` lines. A record's Cause section names where the defect showed,
    which is not always where it was fixed, so it is not read."""
    parts = [m.group(0) for m in FIX_LINE.finditer(text)]
    for m in FIX_HEAD.finditer(text):
        nxt = re.compile(r"^#{1,2} ", re.M).search(text, m.end())
        parts.append(text[m.end():nxt.start() if nxt else len(text)])
    return "\n".join(parts)


def _record_text(root, name):
    try:
        return open(os.path.join(root, "docs", "bugs", name), encoding="utf-8").read()
    except OSError as e:
        raise AreaError(f"cannot read docs/bugs/{name}: {e}")


def _weighted(commits_used, stats, amap, unmapped):
    for h, _, _ in commits_used:
        for path, n in stats.get(h, []):
            a = area_of(path, amap)
            if a is None:
                unmapped.add(path)
            elif a[0] != "ignore" and n:
                return True
    return False


# ----------------------------------------------------------------------------- shared helpers
def primary_class(rec):
    cs = rec["facts"]["class"] if rec["facts"] else []
    return min((int(c) for c, _ in cs), default=None)


def coverage(recs, areas):
    """Counts of each basis, with `none` split into unfixed (open) and underived (fixed)."""
    c = defaultdict(int)
    for n, a in areas.items():
        b = a["basis"]
        if b == "none":
            b = "unfixed" if recs[n]["open"] else "underived"
        c[b] += 1
    return c


def _stage_order(st):
    return STAGES.index(st) if st in STAGES else len(STAGES)


def _table(header, rows, align=None, pad=True):
    """A Markdown table, deterministic. Padded for the terminal report; unpadded in the committed
    view, so one wider cell does not rewrite every row."""
    cells = [list(map(str, header))] + [list(map(str, r)) for r in rows]
    w = [max(len(r[i]) for r in cells) if pad else 0 for i in range(len(header))]
    align = align or ["l"] + ["r"] * (len(header) - 1)
    fmt = lambda r: "| " + " | ".join(c.ljust(w[i]) if align[i] == "l" else c.rjust(w[i]) for i, c in enumerate(r)) + " |"  # noqa: E731
    sep = "|" + "|".join(("-" * (w[i] + 2)) if align[i] == "l" else ("-" * (w[i] + 1) + ":") for i in range(len(header))) + "|"
    return [fmt(cells[0]), sep] + [fmt(r) for r in cells[1:]]


# ----------------------------------------------------------------------------- the AREAS.md view
def render_view(recs, areas, class_names):
    cov = coverage(recs, areas)
    silent = lambda n: bool(recs[n]["facts"]) and recs[n]["facts"].get("silent") == "yes"  # noqa: E731
    placed = [n for n in sorted(areas) if areas[n]["stage"]]
    out = [BEGIN, ""]
    out.append(f"{len(recs)} records: {cov['commits']} placed by their own fix commits, {cov['named']} by the files "
               f"their record names in commits shared with other bugs, {cov['shared']} by commits shared with "
               f"other bugs, {cov['record']} by the commit that filed or closed the record, "
               f"{cov['override']} by an `area:` override; {cov['unfixed']} open bugs are not fixed yet and "
               f"{cov['underived']} fixed bugs have no commit the history can place.")
    out += ["", "### By stage", ""]
    by_stage = defaultdict(list)
    for n in placed:
        by_stage[areas[n]["stage"]].append(n)
    rows = []
    for st in sorted(by_stage, key=lambda s: (-len(by_stage[s]), _stage_order(s))):
        ns = by_stage[st]
        mods = defaultdict(int)
        for n in ns:
            mods[areas[n]["module"]] += 1
        top = ", ".join(f"{m} {k}" for m, k in sorted(mods.items(), key=lambda kv: (-kv[1], kv[0]))[:4])
        rows.append([st, len(ns), sum(recs[n]["open"] for n in ns), sum(silent(n) for n in ns),
                     sum(areas[n]["basis"] in ("shared", "record") for n in ns), top])  # the less certain bases
    out += _table(["stage", "bugs", "open", "silent", "shared or fallback", "top modules"], rows,
                  ["l", "r", "r", "r", "r", "l"], pad=False)
    out += ["", "### Structural class by stage (primary class)", ""]
    by_cls = defaultdict(lambda: defaultdict(int))
    for n in placed:
        by_cls[primary_class(recs[n])][areas[n]["stage"]] += 1
    rows = []
    for c in sorted(by_cls, key=lambda c: (c is None, c or 0)):
        name = f"{c}. {class_names.get(str(c), '')}".strip() if c is not None else "no class"
        st = by_cls[c]
        rows.append([name, sum(st.values()),
                     ", ".join(f"{s} {k}" for s, k in sorted(st.items(), key=lambda kv: (-kv[1], _stage_order(kv[0]))))])
    out += _table(["class", "bugs", "stages"], rows, ["l", "r", "l"], pad=False)
    out += ["", "### Every bug", ""]
    rows = []
    for n in sorted(areas):
        a = areas[n]
        rows.append([f"[{n}]({recs[n]['file']})", a["stage"] or "—", a["module"] or "—",
                     a["basis"] if a["basis"] != "none" else ("unfixed" if recs[n]["open"] else "underived"),
                     ", ".join(a["secondary"]) or "—"])
    out += _table(["bug", "stage", "module", "basis", "also touches"], rows, ["l"] * 5, pad=False)
    out += ["", END]
    return "\n".join(out)


# ----------------------------------------------------------------------------- the report
def load_discovery_kinds(root):
    """{bug: kind} from docs/bugs/discovery.tsv (bugs before its first attributed row have none)."""
    path = os.path.join(root, "docs", "bugs", "discovery.tsv")
    if not os.path.exists(path):
        return {}
    rows = [ln.split("\t") for ln in open(path, encoding="utf-8").read().split("\n") if ln and not ln.startswith("#")]
    if not rows or "bugs" not in rows[0] or "kind" not in rows[0]:
        raise AreaError(f"{path}: no kind/bugs columns")
    ki, bi = rows[0].index("kind"), rows[0].index("bugs")
    out = {}
    for cells in rows[1:]:
        if len(cells) > max(ki, bi):
            for b in cells[bi].split(","):
                if re.fullmatch(r"\d{3}", b):
                    out[b] = cells[ki]
    return out


def _days(a, b):
    import datetime
    d = lambda s: datetime.date(*map(int, s.split("-")))  # noqa: E731
    return (d(b) - d(a)).days


def bug_rows(recs, areas, kinds):
    rows = []
    for n in sorted(areas):
        r, a = recs[n], areas[n]
        f = r["facts"] or {}
        days = _days(a["found"], a["fixed"]) if a["found"] and a["fixed"] else None
        rows.append({
            "bug": n, "title": r["title"], "open": r["open"], "silent": f.get("silent") == "yes",
            "classes": [int(c) for c, _ in f.get("class", [])], "primary_class": primary_class(r),
            "stage": a["stage"], "module": a["module"], "tier": a["tier"], "secondary": a["secondary"],
            "basis": a["basis"] if a["basis"] != "none" else ("unfixed" if r["open"] else "underived"),
            "single_share": a["single_share"], "found": a["found"], "fixed": a["fixed"],
            "filed_fixed": a["filed_fixed"], "days_to_fix": days, "discovery": kinds.get(n),
            "weights": a["weights"], "commits": a["commits"],
        })
    return rows


def report_tables(rows, class_names):
    """[(title, header, rows, align)] — the report's tables, in order."""
    placed = [r for r in rows if r["stage"]]
    tabs = []
    cov = defaultdict(int)
    for r in rows:
        cov[r["basis"]] += 1
    tabs.append(("Coverage: how each bug's area was obtained", ["basis", "bugs"],
                 [[b, cov[b]] for b in ("commits", "named", "shared", "record", "override", "unfixed", "underived")]
                 + [["ambiguous (shared, record, underived)", cov["shared"] + cov["record"] + cov["underived"]]], None))

    def group(key):
        g = defaultdict(list)
        for r in placed:
            g[key(r)].append(r)
        return g

    g = group(lambda r: r["stage"])
    tabs.append(("Bugs by stage", ["stage", "bugs", "open", "silent", "fixed"],
                 [[st, len(v), sum(x["open"] for x in v), sum(x["silent"] for x in v), sum(not x["open"] for x in v)]
                  for st, v in sorted(g.items(), key=lambda kv: (-len(kv[1]), _stage_order(kv[0])))], None))
    g = group(lambda r: (r["stage"], r["module"]))
    tabs.append(("Bugs by stage and module", ["stage", "module", "bugs", "open", "silent"],
                 [[st, m, len(v), sum(x["open"] for x in v), sum(x["silent"] for x in v)]
                  for (st, m), v in sorted(g.items(), key=lambda kv: (-len(kv[1]), _stage_order(kv[0][0]), kv[0][1]))],
                 ["l", "l", "r", "r", "r"]))
    stages = sorted({r["stage"] for r in placed}, key=_stage_order)
    cls = sorted({r["primary_class"] for r in placed}, key=lambda c: (c is None, c or 0))
    mat = defaultdict(int)
    for r in placed:
        mat[(r["primary_class"], r["stage"])] += 1
    tabs.append(("Primary structural class x stage", ["class"] + stages + ["total"],
                 [[str(c) if c is not None else "-"] + [mat[(c, s)] or "." for s in stages]
                  + [sum(mat[(c, s)] for s in stages)] for c in cls], None))
    months = sorted({r["found"][:7] for r in rows if r["found"]} | {r["fixed"][:7] for r in rows if r["fixed"]})
    tabs.append(("Bugs by month (author dates)", ["month", "found", "fixed", "found silent"],
                 [[m, sum(1 for r in rows if (r["found"] or "")[:7] == m), sum(1 for r in rows if (r["fixed"] or "")[:7] == m),
                   sum(1 for r in rows if (r["found"] or "")[:7] == m and r["silent"])] for m in months], None))
    g = group(lambda r: r["stage"])
    srows = [[st, sum(x["silent"] for x in v), len(v), f"{100 * sum(x['silent'] for x in v) // len(v)}%"]
             for st, v in g.items()]
    tabs.append(("Silent wrong-code bugs by stage", ["stage", "silent", "bugs", "silent share"],
                 sorted([r for r in srows if r[1]], key=lambda r: (-r[1], _stage_order(r[0]))), None))
    kinds = sorted({r["discovery"] or "(before discovery.tsv)" for r in placed})
    mat = defaultdict(int)
    for r in placed:
        mat[(r["discovery"] or "(before discovery.tsv)", r["stage"])] += 1
    tabs.append(("How found (docs/bugs/discovery.tsv kind) x stage", ["kind"] + stages + ["total"],
                 [[k] + [mat[(k, s)] or "." for s in stages] + [sum(mat[(k, s)] for s in stages)] for k in kinds], None))
    trows = []
    for st, v in sorted(group(lambda r: r["stage"]).items(), key=lambda kv: _stage_order(kv[0])):
        fx = [x["days_to_fix"] for x in v if x["days_to_fix"] is not None]
        fo = [x["days_to_fix"] for x in v if x["days_to_fix"] is not None and not x["filed_fixed"]]
        if not fx:
            continue
        mean = lambda xs: f"{sum(xs) / len(xs):.1f}" if xs else "-"  # noqa: E731
        trows.append([st, len(fx), mean(fx), len(fo), mean(fo), _median(fo)])
    tabs.append(("Days from filing to fix per stage (all fixed bugs; and only those filed open)",
                 ["stage", "fixed", "mean days", "filed open", "mean days", "median days"], trows, None))
    pairs = defaultdict(list)
    for r in placed:
        for c in r["classes"] or [None]:
            pairs[(r["stage"], r["module"], c)].append(r["bug"])
    prow = [[st, m, (f"{c}. {class_names.get(str(c), '')}" if c is not None else "no class")[:70], len(bs), ", ".join(bs)]
            for (st, m, c), bs in pairs.items() if len(bs) >= 2]
    prow.sort(key=lambda r: (-r[3], _stage_order(r[0]), r[1], r[2]))
    tabs.append(("Recurring area x class pairs (two or more bugs; every class a bug lists)",
                 ["stage", "module", "class", "bugs", "which"], prow[:30], ["l", "l", "l", "r", "l"]))
    return tabs


def _median(xs):
    if not xs:
        return "-"
    ys, k = sorted(xs), len(xs) // 2
    return f"{(ys[k] if len(ys) % 2 else (ys[k - 1] + ys[k]) / 2):.1f}"


def render_report(tabs):
    out = []
    for title, header, rows, align in tabs:
        out += [f"## {title}", ""] + _table(header, rows, align) + [""]
    return "\n".join(out)


def write_sqlite(path, rows, class_names):
    import sqlite3
    if os.path.exists(path):
        os.remove(path)
    db = sqlite3.connect(path)
    db.executescript("""
        CREATE TABLE bugs (bug TEXT PRIMARY KEY, title TEXT, open INTEGER, silent INTEGER, stage TEXT,
            module TEXT, tier TEXT, basis TEXT, primary_class INTEGER, found TEXT, fixed TEXT,
            filed_fixed INTEGER, days_to_fix INTEGER, discovery TEXT, single_share REAL);
        CREATE TABLE bug_classes (bug TEXT, class INTEGER, name TEXT);
        CREATE TABLE bug_secondary (bug TEXT, stage TEXT);
        CREATE TABLE bug_weights (bug TEXT, tier TEXT, stage TEXT, module TEXT, lines REAL);
        CREATE TABLE bug_commits (bug TEXT, hash TEXT, share REAL, via TEXT);
    """)
    for r in rows:
        db.execute("INSERT INTO bugs VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
                   (r["bug"], r["title"], int(r["open"]), int(r["silent"]), r["stage"], r["module"], r["tier"],
                    r["basis"], r["primary_class"], r["found"], r["fixed"], int(r["filed_fixed"]), r["days_to_fix"],
                    r["discovery"], r["single_share"]))
        db.executemany("INSERT INTO bug_classes VALUES (?,?,?)", [(r["bug"], c, class_names.get(str(c))) for c in r["classes"]])
        db.executemany("INSERT INTO bug_secondary VALUES (?,?)", [(r["bug"], s) for s in r["secondary"]])
        for tier, w in r["weights"].items():
            db.executemany("INSERT INTO bug_weights VALUES (?,?,?,?,?)",
                           [(r["bug"], tier, k.split("/", 1)[0], k.split("/", 1)[1], v) for k, v in w.items()])
        db.executemany("INSERT INTO bug_commits VALUES (?,?,?,?)",
                       [(r["bug"], c["hash"], c["share"], c["via"]) for c in r["commits"]])
    db.commit()
    db.close()
