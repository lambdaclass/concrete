#!/usr/bin/env python3
"""Build the agent work queue (queue.json) from its sources. The manual is
docs/archive/roadmap-2026-11/WORK_QUEUE.md, archived with the old roadmap: the queue goes to legacy/
with the rest of the old tree under R-0558, and comes back only if the owner pulls it back.

  build_queue.py [--root DIR] [--fuzz-dir DIR] [--out FILE]

queue.json is GENERATED: every tick of the dispatcher rebuilds it from the sources below and nobody
edits it. Each source owns its facts; this script only reads them.

  bugs      docs/bugs/NNN_*.md whose first Status line is open (OPEN, CONTAINED, Half closed,
            Partially fixed: the vocabulary of scripts/gen/bug_ledger.py, read the same way). The
            severity comes from the record's `silent:` ledger line when it has one, otherwise from its
            **Severity:** line. The touched areas come from its "Structural cause" and "Structural
            owner" sections (the whole record when it has neither).
  roadmap   ROADMAP.md: the task rows of phase T (development throughput) in their order, then every
            R-NNNN named in the queue's rows, in row order. A task row's "depends on" column gives
            its dependencies, item by item: a live task or a completed one (a `### Completed Task`
            heading in CHANGELOG.md or changelog.d/); a queue row (its tasks); a phase letter (that
            phase's tasks, the task itself excluded); `owner: <decision>`; or `—`. Every row's cell
            is read, queued or not, and any other item exits 2. A task that waits on an owner decision, or on a live task that is not queued
            (the dispatcher counts an unqueued dependency as met), gets `disposition: skip` and
            `blocked_on` naming why; a task file cannot dispatch it, only dispose it `done`. A task
            named in a queue row but without a task row is not queued.
  fuzz      <fuzz-dir>/*.json findings of class silent, crash or unsoundness. Refusals and other
            classes are not tasks; a finding already promoted to a bug (`bug` set) is that bug's.
  explicit  tasks/*.md (format in WORK_QUEUE.md). A file whose `id` names a sourced task instead
            amends it: `disposition: in-flight|done|skip` takes it out of dispatch, and `areas`,
            `depends`, `tier` or a prompt body override the derived values.

PRIORITY (lower tier first, then source order inside a tier):
  0 throughput   phase T's tasks (user priority)
  1 silent       silent wrong code, unsound claims, memory-safety bugs; fuzz silent/unsoundness
  2 crash        crashes, panics and aborts; fuzz crash
  3 explicit     tasks/*.md without a tier (ahead of the roadmap rows)
  4 roadmap      the execution queue, row order
  5 loud         other open bugs: loud defects wait for their structural owner (ROADMAP defect
                 disposition rule), so they come after the roadmap

Exit 2 on a malformed source (a task file without front matter, an unknown key or tier, a fuzz file
that is not JSON). Nothing is written then: a half-read queue must not be dispatched.
"""
import argparse
import glob
import json
import os
import re
import sys

TIERS = ["throughput", "silent", "crash", "explicit", "roadmap", "loud"]
OPEN_WORDS = {"open", "contained", "half", "partially"}
CLOSED_WORDS = {"fixed"}
TASK_KEYS = {"id", "title", "tier", "areas", "depends", "disposition", "branch", "prompt", "kind"}
DISPOSITIONS = {"in-flight", "done", "skip"}
FUZZ_CLASSES = {"silent": "silent", "unsoundness": "silent", "crash": "crash"}
SILENT_RE = re.compile(r"silent|unsound|false[ `]*proved|false proof|wrong evidence|memory.safety|"
                       r"use.after.free|out.of.bounds", re.I)
CRASH_RE = re.compile(r"crash|panic|segfault|sigsegv|abort|stack overflow|hangs?\b", re.I)
RID = re.compile(r"\bR-\d{4}\b")
PATH_RE = re.compile(r"\b((?:Concrete|scripts|std|tests|docs|proofs)/[A-Za-z0-9_./-]*[A-Za-z0-9_])")
# Module basenames that are also ordinary words: they name an area only as `Name`, Name.lean or a path.
GENERIC = {"AST", "Array", "Arith", "Backend", "Calls", "Check", "Core", "Diff", "Digest", "Eval",
           "Evidence", "Format", "Issue", "Json", "Loops", "Policy", "Profile", "Project", "Proof",
           "Receipt", "Replay", "Report", "Shared", "Verify", "Diagnostic", "Pipeline", "Semantics",
           "Frontend", "IR", "Token", "Symbols", "Intrinsic", "Correspondence", "Refinement"}


def die(msg):
    print(f"build_queue: {msg}", file=sys.stderr)
    sys.exit(2)


def read(path):
    with open(path, encoding="utf-8") as f:
        return f.read()


def modules(root):
    names = set()
    for p in glob.glob(os.path.join(root, "Concrete", "**", "*.lean"), recursive=True):
        names.add(os.path.splitext(os.path.basename(p))[0])
    return names


def areas_of(text, mods):
    """The touched-area keys a text names: rules (R-NNNN), paths, compiler modules."""
    out = set("rule:" + r for r in RID.findall(text))
    for p in PATH_RE.findall(text):
        p = p.rstrip(".")
        out.add("path:" + p)
        if p.startswith("Concrete/") and p.endswith(".lean"):
            out.add("mod:" + os.path.splitext(os.path.basename(p))[0])
    for m in mods:
        if m in GENERIC:
            pat = r"`" + re.escape(m) + r"(?:\.[A-Za-z_]+)*`|\b" + re.escape(m) + r"\.lean\b"
        else:
            pat = r"(?<![A-Za-z0-9_])" + re.escape(m) + r"(?![A-Za-z0-9_])"
        if re.search(pat, text):
            out.add("mod:" + m)
    return out


def norm_area(a):
    """An area written by hand: R-NNNN is a rule, anything with '/' or '.' a path, else a module."""
    a = a.strip()
    if a.startswith(("rule:", "mod:", "path:")) or a == "unknown":
        return a
    if RID.fullmatch(a):
        return "rule:" + a
    if "/" in a or "." in a:
        return "path:" + a.rstrip("/")
    return "mod:" + a


def norm_areas(items):
    """Hand-written areas, with a Concrete/**/X.lean path also naming module X, as derived areas do."""
    out = set()
    for a in items:
        k = norm_area(str(a))
        out.add(k)
        if k.startswith("path:Concrete/") and k.endswith(".lean"):
            out.add("mod:" + os.path.splitext(os.path.basename(k))[0])
    return out


def section(text, heading):
    m = re.search(r"^## " + heading + r"[^\n]*\n(.*?)(?=^## |\Z)", text, re.M | re.S)
    return m.group(1) if m else ""


# ---------------------------------------------------------------- bugs
def bug_tasks(root, mods):
    tasks = []
    for path in sorted(glob.glob(os.path.join(root, "docs", "bugs", "[0-9][0-9][0-9]_*.md"))):
        name = os.path.basename(path)
        num = name[:3]
        text = read(path)
        comp = re.findall(r"^compiler: (.*)$", text, re.M)
        if len(comp) != 1 or comp[0] not in ("legacy", "new"):
            die(f"{path}: compiler must be legacy or new")
        if comp[0] == "legacy":
            continue
        m = re.search(r"^(?:\*\*Status:\*\*|Status:)\s*(.*)$", text, re.M)
        if not m:
            continue
        word = (re.findall(r"[A-Za-z]+", m.group(1))[:1] or [""])[0].lower()
        if word in CLOSED_WORDS or word not in OPEN_WORDS:
            continue
        sev = re.search(r"^\*\*Severity:\*\*\s*(.*)$", text, re.M)
        sev = sev.group(1) if sev else ""
        led = re.search(r"^silent:[ \t]*(yes|no)[ \t]*$", text, re.M)
        if (led and led.group(1) == "yes") or (not led and SILENT_RE.search(sev)):
            tier = "silent"
        elif CRASH_RE.search(sev):
            tier = "crash"
        else:
            tier = "loud"
        owner_text = section(text, "Structural cause") + section(text, "Structural owner")
        areas = areas_of(owner_text or text, mods) or {"unknown"}
        t = re.search(r"^# Bug " + num + r"\s*[:—–-]+\s*(.+)$", text, re.M)
        tasks.append({
            "id": f"bug-{num}", "kind": "bug", "tier": tier,
            "title": t.group(1).strip() if t else name,
            "areas": sorted(areas), "depends": [],
            "prompt": f"/fix-bug {int(num)}",
            "source": f"docs/bugs/{name}", "status_line": m.group(1).strip()[:80],
        })
    return tasks


# ---------------------------------------------------------------- roadmap
def task_rows(roadmap):
    """ROADMAP.md's task rows, `| R-NNNN | objective | done when | depends on |` under "## Tasks", in
    file order: {rid: (phase, objective, done_when, depends_cell)}."""
    rows, phase, in_tasks = {}, None, False
    for line in roadmap.split("\n"):
        if line.startswith("## "):
            in_tasks, phase = line.strip() == "## Tasks", None
            continue
        m = re.match(r"^### ([A-GT])\. ", line)
        if in_tasks and m:
            phase = m.group(1)
            continue
        if in_tasks and phase and re.match(r"^\| R-\d{4} \|", line):
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if len(cells) == 4:
                rows[cells[0]] = (phase, cells[1], cells[2], cells[3])
    return rows


def completed_tasks(root):
    """Task ids the changelog records completed, pending fragments included."""
    done = set()
    for f in [os.path.join(root, "CHANGELOG.md")] + sorted(glob.glob(os.path.join(root, "changelog.d", "*.md"))):
        if os.path.exists(f):
            done |= set(re.findall(r"^### Completed Task (R-\d{4})\s*$", read(f), re.M))
    return done


def roadmap_tasks(root, mods):
    """(tasks, live): the queued roadmap tasks, and every live task id in ROADMAP.md."""
    path = os.path.join(root, "ROADMAP.md")
    if not os.path.exists(path):
        return [], set()
    rm = read(path)
    # Since 2026-11-04 ROADMAP.md holds one row per live task under "## Tasks" (objective, done when,
    # depends on); the old task register is archived. Without those rows every queued task would be
    # built blind: no objective, no dependencies, no areas. Refuse rather than schedule work blind.
    rows = task_rows(rm)
    if not rows:
        die(f"{path} has no task rows under '## Tasks': the queue's tasks cannot be read")
    qrows = {}
    sec = re.search(r"^## The queue\s*\n(.*?)(?=^## |\Z)", rm, re.M | re.S)
    if sec:
        for line in sec.group(1).split("\n"):
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if len(cells) == 4 and re.fullmatch(r"[A-GT][1-9][0-9]*", cells[0]):
                qrows[cells[0]] = RID.findall(cells[2])
    completed = completed_tasks(root)
    tasks, seen = [], set()

    def resolve(rid, cell):
        """The "depends on" cell as (task ids, owner decisions). Unknown syntax exits 2: a
        prerequisite this script cannot read would otherwise read as no prerequisite."""
        deps, owner = [], []
        for item in (x.strip() for x in cell.split(",")):
            if item == "—":
                continue
            if re.fullmatch(r"owner: \S.*", item):
                owner.append(item[len("owner: "):])
            elif re.fullmatch(r"R-\d{4}", item):
                if item not in rows and item not in completed:
                    die(f"{path}: {rid} depends on {item}, which is neither a task in ROADMAP.md "
                        f"nor completed in the changelog")
                if item in rows:
                    deps.append(item)
            elif item in qrows:
                deps += qrows[item]
            elif re.fullmatch(r"[A-GT]", item) and any(r[0] == item for r in rows.values()):
                deps += [k for k, r in rows.items() if r[0] == item]
            else:
                die(f"{path}: {rid} depends on {item!r}, which is not a task, a queue row, a phase "
                    f"with tasks, 'owner: <decision>' or '—'")
        return sorted(set(deps) - {rid}), owner

    resolved = {rid: resolve(rid, row[3]) for rid, row in rows.items()}

    def add(rid, tier):
        if rid in seen or rid not in rows:
            return
        seen.add(rid)
        _, obj, done, deps_cell = rows[rid]
        deps, owner = resolved[rid]
        # A row cites other tasks and documents; it touches its own rule and the compiler, script
        # and library files it names. Cited R-ids and docs are references, not areas.
        areas = {a for a in areas_of(obj + "\n" + done, mods)
                 if not a.startswith(("rule:", "path:docs/"))} | {"rule:" + rid}
        tasks.append({
            "id": rid, "kind": "roadmap", "tier": tier,
            "title": obj.strip()[:160] or rid,
            "areas": sorted(areas), "depends": sorted(set(deps)),
            "prompt": f"/roadmap-item {rid}", "source": "ROADMAP.md",
            **({"blocked_on": ["owner: " + o for o in owner]} if owner else {}),
        })

    # The development-throughput track: phase T's rows, in their order (user priority).
    for rid, row in rows.items():
        if row[0] == "T":
            add(rid, "throughput")
    # Then every task named in the queue's rows, in row order.
    for ids in qrows.values():
        for rid in ids:
            add(rid, "roadmap")
    return tasks, set(rows)


# ---------------------------------------------------------------- fuzz-farm findings
def fuzz_tasks(fuzz_dir, mods):
    tasks = []
    if not fuzz_dir or not os.path.isdir(fuzz_dir):
        return tasks
    for path in sorted(glob.glob(os.path.join(fuzz_dir, "*.json"))):
        try:
            f = json.loads(read(path))
        except (OSError, ValueError) as e:
            die(f"{path}: not a JSON finding: {e}")
        if not isinstance(f, dict):
            die(f"{path}: a finding is a JSON object")
        cls = str(f.get("class", "")).lower()
        if cls not in FUZZ_CLASSES or f.get("bug"):
            continue
        fid = str(f.get("id") or os.path.splitext(os.path.basename(path))[0])
        if not re.fullmatch(r"[A-Za-z0-9_.-]+", fid):
            die(f"{path}: finding id '{fid}' is not [A-Za-z0-9_.-]+")
        areas = norm_areas(f.get("areas") or [])
        areas |= areas_of(" ".join(str(f.get(k, "")) for k in ("summary", "owner", "module", "pass")), mods)
        tasks.append({
            "id": f"fuzz-{fid}", "kind": "fuzz", "tier": FUZZ_CLASSES[cls],
            "title": str(f.get("summary", fid))[:160],
            "areas": sorted(areas or {"unknown"}), "depends": [],
            "prompt": (f"Fuzz finding {path} (class {cls}). Promote it to a bug record with the fuzz "
                       f"farm's promote.sh, then follow /fix-bug for the new bug number."),
            "source": path,
        })
    return tasks


# ---------------------------------------------------------------- tasks/*.md
def parse_task_file(path, root):
    text = read(path)
    m = re.match(r"^---[ \t]*\n(.*?)^---[ \t]*\n?(.*)\Z", text, re.M | re.S)
    if not m:
        die(f"{path}: no '---' front matter")
    meta = {}
    for ln in m.group(1).split("\n"):
        if not ln.strip() or ln.lstrip().startswith("#"):
            continue
        kv = re.match(r"^([a-z]+):[ \t]*(.*?)[ \t]*$", ln)
        if not kv:
            die(f"{path}: front-matter line is not 'key: value': {ln[:60]}")
        if kv.group(1) not in TASK_KEYS:
            die(f"{path}: unknown key '{kv.group(1)}' (known: {', '.join(sorted(TASK_KEYS))})")
        meta[kv.group(1)] = kv.group(2)
    for k in ("areas", "depends"):
        if k in meta:
            meta[k] = [x.strip() for x in meta[k].split(",") if x.strip()]
    if "areas" in meta:
        meta["areas"] = sorted(norm_areas(meta["areas"]))
    if "tier" in meta and meta["tier"] not in TIERS:
        die(f"{path}: tier '{meta['tier']}' is not one of {', '.join(TIERS)}")
    if "disposition" in meta and meta["disposition"] not in DISPOSITIONS:
        die(f"{path}: disposition '{meta['disposition']}' is not one of {', '.join(sorted(DISPOSITIONS))}")
    body = m.group(2).strip()
    if body and "prompt" not in meta:
        meta["prompt"] = body
    meta.setdefault("id", "task-" + os.path.splitext(os.path.basename(path))[0])
    if not re.fullmatch(r"[A-Za-z0-9_.-]+", meta["id"]):
        die(f"{path}: id '{meta['id']}' is not [A-Za-z0-9_.-]+")
    meta["source"] = os.path.relpath(path, root)
    return meta


def build(root, fuzz_dir):
    mods = modules(root)
    rtasks, live = roadmap_tasks(root, mods)
    sourced = rtasks + bug_tasks(root, mods) + fuzz_tasks(fuzz_dir, mods)
    by_id = {}
    for t in sourced:
        by_id.setdefault(t["id"], t)
    legacy_ids = {
        "bug-" + os.path.basename(p)[:3]
        for p in glob.glob(os.path.join(root, "docs", "bugs", "[0-9][0-9][0-9]_*.md"))
        if re.search(r"^compiler: legacy$", read(p), re.M)
    }
    explicit, unmatched = [], []
    for path in sorted(glob.glob(os.path.join(root, "tasks", "*.md"))):
        if os.path.basename(path).upper() == "README.MD":
            continue
        meta = parse_task_file(path, root)
        if meta["id"] in legacy_ids:
            if meta.get("prompt"):
                die(f"{meta['source']}: cannot dispatch a legacy compiler bug")
            unmatched.append(meta["source"])
            continue
        if meta["id"] in by_id:
            t = by_id[meta["id"]]
            for k in ("areas", "depends", "tier", "prompt", "title", "disposition", "branch"):
                if k in meta:
                    t[k] = meta[k]
            t["amended_by"] = meta["source"]
        elif "disposition" in meta and not meta.get("prompt"):
            # A disposition for a task its source does not (or no longer) list: nothing to amend.
            unmatched.append(meta["source"])
        else:
            if not meta.get("prompt"):
                die(f"{meta['source']}: a new task needs a prompt (a body or `prompt:`)")
            explicit.append({
                "id": meta["id"], "kind": meta.get("kind", "explicit"),
                "tier": meta.get("tier", "explicit"), "title": meta.get("title", meta["id"]),
                "areas": sorted(set(meta.get("areas", [])) or {"unknown"}),
                "depends": meta.get("depends", []), "prompt": meta["prompt"],
                "source": meta["source"],
                **({"disposition": meta["disposition"]} if "disposition" in meta else {}),
                **({"branch": meta["branch"]} if "branch" in meta else {}),
            })
            by_id[meta["id"]] = explicit[-1]
    # A dependency on a live roadmap task that is in no source (neither queued nor a task file) is
    # unmet, and the dispatcher would count it as met: block the dependent. This is computed on the
    # merged sources, so a prerequisite supplied by a task file (done or not) is a dependency again.
    # A block outlives a task file's amendment: only `done` (the work landed elsewhere) lifts it.
    present = {t["id"] for t in sourced + explicit}
    for t in sourced:
        if t["kind"] == "roadmap":
            t["blocked_on"] = [b for b in t.get("blocked_on", []) if b.startswith("owner: ")] + \
                ["unqueued " + d for d in t["depends"] if d in live and d not in present]
            if not t["blocked_on"]:
                del t["blocked_on"]
        if t.get("blocked_on") and t.get("disposition") != "done":
            t["disposition"] = "skip"
    seen, tasks = set(), []
    for t in sourced + explicit:
        if t["id"] in seen:
            continue
        seen.add(t["id"])
        tasks.append(t)
    tasks.sort(key=lambda t: TIERS.index(t["tier"]))  # stable: source order inside a tier
    # A dependency cycle can never start: mark its members so the dispatcher reports them instead
    # of waiting on them silently.
    deps = {t["id"]: set(t["depends"]) for t in tasks}

    def reaches(a, b, seen):
        for d in deps.get(a, ()):
            if d == b or (d not in seen and (seen.add(d) or reaches(d, b, seen))):
                return True
        return False
    for t in tasks:
        if reaches(t["id"], t["id"], set()):
            t["cycle"] = True
    for i, t in enumerate(tasks):
        t["rank"] = i + 1
    return {"schema": 1, "generated_from": os.path.abspath(root), "tiers": TIERS, "tasks": tasks,
            "unmatched_dispositions": unmatched}


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--root", default=os.getcwd())
    ap.add_argument("--fuzz-dir", default=os.environ.get("QUEUE_FUZZ_DIR", ""))
    ap.add_argument("--out", default="-")
    a = ap.parse_args()
    q = build(a.root, a.fuzz_dir)
    data = json.dumps(q, indent=1, sort_keys=True) + "\n"
    if a.out == "-":
        sys.stdout.write(data)
    else:
        tmp = a.out + ".tmp"
        with open(tmp, "w", encoding="utf-8") as f:
            f.write(data)
        os.replace(tmp, a.out)


if __name__ == "__main__":
    main()
