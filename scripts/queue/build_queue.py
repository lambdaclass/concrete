#!/usr/bin/env python3
"""Build the agent work queue (queue.json) from its sources. docs/project/WORK_QUEUE.md is the manual.

  build_queue.py [--root DIR] [--fuzz-dir DIR] [--out FILE]

queue.json is GENERATED: every tick of the dispatcher rebuilds it from the sources below and nobody
edits it. Each source owns its facts; this script only reads them.

  bugs      docs/bugs/NNN_*.md whose first Status line is open (OPEN, CONTAINED, Half closed,
            Partially fixed: the vocabulary of scripts/gen/bug_ledger.py, read the same way). The
            severity comes from the record's `silent:` ledger line when it has one, otherwise from its
            **Severity:** line. The touched areas come from its "Structural cause" and "Structural
            owner" sections (the whole record when it has neither).
  roadmap   ROADMAP.md: the development-throughput table (R-0539 .. R-0547 and R-0513, in the
            table's order, with its "depends on" column), then every R-NNNN named in the execution
            queue's rows, in row order. A task body's **Depends on:** line adds dependencies.
  fuzz      <fuzz-dir>/*.json findings of class silent, crash or unsoundness. Refusals and other
            classes are not tasks; a finding already promoted to a bug (`bug` set) is that bug's.
  explicit  tasks/*.md (format in WORK_QUEUE.md). A file whose `id` names a sourced task instead
            amends it: `disposition: in-flight|done|skip` takes it out of dispatch, and `areas`,
            `depends`, `tier` or a prompt body override the derived values.

PRIORITY (lower tier first, then source order inside a tier):
  0 throughput   the R-0539 .. R-0547 track (user priority)
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
def task_body(roadmap, rid):
    m = re.search(r"^### Task " + rid + r"\s*\n(.*?)(?=^### |^## |\Z)", roadmap, re.M | re.S)
    return m.group(1) if m else ""


def roadmap_tasks(root, mods):
    path = os.path.join(root, "ROADMAP.md")
    if not os.path.exists(path):
        return []
    rm = read(path)
    tasks, seen = [], set()

    def add(rid, tier, title, deps):
        if rid in seen:
            return
        seen.add(rid)
        body = task_body(rm, rid)
        d = list(deps)
        for line in re.findall(r"^\*\*Depends on:\*\*(.*)$", body, re.M):
            line = line.split("**Blocks")[0]  # "Depends on: nothing. **Blocks:** R-0474."
            d += [x for x in RID.findall(line) if x != rid]
        obj = re.search(r"^\*\*Objective:\*\*\s*(.*)$", body, re.M)
        # A task body cites other tasks and documents; it touches its own rule and the compiler,
        # script and library files it names. Cited R-ids and docs are references, not areas.
        areas = {a for a in areas_of(body, mods)
                 if not a.startswith(("rule:", "path:docs/"))} | {"rule:" + rid}
        tasks.append({
            "id": rid, "kind": "roadmap", "tier": tier,
            "title": (title or (obj.group(1) if obj else rid)).strip()[:160],
            "areas": sorted(areas), "depends": sorted(set(d)),
            "prompt": f"/roadmap-item {rid}", "source": "ROADMAP.md",
        })

    # The throughput table: | order | item | owner | exit criterion | depends on |
    sec = re.search(r"^## Development throughput\s*\n(.*?)(?=^## )", rm, re.M | re.S)
    if sec:
        rows = []
        for line in sec.group(1).split("\n"):
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if len(cells) == 5 and cells[0].isdigit() and RID.fullmatch(cells[2]):
                rows.append(cells)
        by_order = {r[0]: r[2] for r in rows}
        for r in rows:
            deps = [by_order[o] for o in re.findall(r"\d+", r[4]) if o in by_order]
            add(r[2], "throughput", r[1].split(":")[0], deps)

    sec = re.search(r"^## The current execution queue\s*\n(.*?)(?=^## |^### Phase 1 exits)", rm, re.M | re.S)
    if sec:
        for line in sec.group(1).split("\n"):
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if len(cells) >= 3 and cells[0].isdigit():
                for rid in RID.findall(cells[1] + " " + cells[2]):
                    add(rid, "roadmap", "", [])
    return tasks


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
        areas = {norm_area(str(a)) for a in (f.get("areas") or [])}
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
        meta["areas"] = sorted({norm_area(a) for a in meta["areas"]})
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
    sourced = roadmap_tasks(root, mods) + bug_tasks(root, mods) + fuzz_tasks(fuzz_dir, mods)
    by_id = {}
    for t in sourced:
        by_id.setdefault(t["id"], t)
    explicit = []
    for path in sorted(glob.glob(os.path.join(root, "tasks", "*.md"))):
        if os.path.basename(path).upper() == "README.MD":
            continue
        meta = parse_task_file(path, root)
        if meta["id"] in by_id:
            t = by_id[meta["id"]]
            for k in ("areas", "depends", "tier", "prompt", "title", "disposition", "branch"):
                if k in meta:
                    t[k] = meta[k]
            t["amended_by"] = meta["source"]
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
    return {"schema": 1, "generated_from": os.path.abspath(root), "tiers": TIERS, "tasks": tasks}


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
