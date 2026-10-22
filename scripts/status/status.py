#!/usr/bin/env python3
"""Project status page: one static status.json + status.html (no external assets, meta refresh).

  status.py [--out DIR] [--public-out DIR] [--fetch] [--refresh SECONDS]

  --out DIR         write the full page (status.json, status.html) into DIR (default: ./status-out)
  --public-out DIR  also write a SANITIZED page into DIR: no IP addresses, tokens, webhook URLs,
                    e-mail addresses, home paths or private host names; runners become "runner N".
                    Only this variant may ever be published (the repository is public).
  --fetch           `git fetch` origin first (branches, plus the review/gate notes into a private
                    mirror namespace, refs/notes/status-mirror/*, so no local note is overwritten)
  --refresh N       meta-refresh interval of the HTML (default 120)

Sections: main on origin and the mirror (they must agree), CI runs in flight on ci/*, the wip/*,
ready/* and train/* branches with review and gate notes, the landing log (R-0547, read from the
per-train TSVs that scripts/land/train.sh writes under the git common dir), open bugs (docs/bugs/),
and server health (load average, self-hosted runners). docs/project/STATUS_PAGE.md has the setup.

Every collector fails soft: a section that cannot be read says why, and the rest still renders.
Everything here is READ-ONLY against GitHub (gh run list / run view / api GET) and git (ls-remote,
for-each-ref, rev-list, notes show); --fetch is the only command that writes, and only to remote-
tracking refs and the notes mirror.

Test seams: STATUS_GH (gh binary), STATUS_REPO, STATUS_MIRROR_REPO, STATUS_REMOTE,
STATUS_MIRROR_REMOTE, STATUS_ROOT (repository), LAND_LOG_DIR / LAND_STATE_DIR (landing log),
STATUS_LOAD_CMD (a command printing /proc/loadavg-style text, e.g. over ssh), STATUS_NOW (epoch
seconds, for deterministic output), STATUS_PRIVATE_HOSTS (comma-separated names to strip).
"""
import argparse
import datetime
import glob
import html
import json
import os
import re
import shlex
import statistics
import subprocess
import sys
import tempfile

TARGET_LAND_S = 2 * 3600       # time to land (first successful build -> landed) under 2 h
TARGET_CI_S = 40 * 60          # landing CI wall clock (dispatch -> verdict) at most 40 min
STALE_BRANCH_DAYS = 7
OPEN_WORDS = ("open", "contained", "half", "partially")
NOTES_REFS = ("concrete-review", "concrete-gates")


def env(name, default):
    v = os.environ.get(name)
    return v if v else default


ROOT = env("STATUS_ROOT", os.getcwd())
GH = env("STATUS_GH", "gh")
REPO = env("STATUS_REPO", "unbalancedparentheses/concrete2")
MIRROR_REPO = env("STATUS_MIRROR_REPO", "lambdaclass/concrete")
REMOTE = env("STATUS_REMOTE", "origin")
MIRROR_REMOTE = env("STATUS_MIRROR_REMOTE", "lambdaclass")
CI_WORKFLOW = env("STATUS_CI_WORKFLOW", "CI")
NOW = int(env("STATUS_NOW", "0")) or int(datetime.datetime.now(datetime.timezone.utc).timestamp())


# ---------------------------------------------------------------- helpers
def run(cmd, timeout=60, check=True):
    """Run a command in the repository; return stdout, or raise RuntimeError with its stderr."""
    try:
        p = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True, timeout=timeout)
    except (OSError, subprocess.TimeoutExpired) as e:
        raise RuntimeError(f"{cmd[0]}: {e}") from None
    if check and p.returncode != 0:
        msg = (p.stderr or p.stdout).strip().splitlines()
        raise RuntimeError(f"{' '.join(cmd[:3])}: exit {p.returncode}: {msg[-1] if msg else ''}")
    return p.stdout


def gh_json(*args):
    return json.loads(run([GH, *args]) or "null")


def parse_ts(s):
    if not s:
        return None
    try:
        return int(datetime.datetime.strptime(s, "%Y-%m-%dT%H:%M:%SZ")
                   .replace(tzinfo=datetime.timezone.utc).timestamp())
    except ValueError:
        return None


def iso(t):
    return datetime.datetime.fromtimestamp(t, datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ") if t else None


def dur(sec):
    if sec is None:
        return "-"
    sec = int(sec)
    if sec < 0:
        sec = 0
    if sec >= 86400:
        return f"{sec // 86400}d{(sec % 86400) // 3600:02d}h"
    if sec >= 3600:
        return f"{sec // 3600}h{(sec % 3600) // 60:02d}m"
    return f"{sec // 60}m{sec % 60:02d}s"


def section(fn):
    """Run one collector; a failure becomes {"error": ...} instead of taking the page down."""
    try:
        return fn()
    except Exception as e:  # noqa: BLE001 - every collector fails soft by design
        return {"error": f"{type(e).__name__}: {e}"}


# ---------------------------------------------------------------- main on both remotes
def ls_remote_main(remote_or_url):
    out = run(["git", "ls-remote", remote_or_url, "refs/heads/main"], timeout=30)
    for line in out.splitlines():
        sha, ref = line.split("\t", 1)
        if ref == "refs/heads/main":
            return sha
    raise RuntimeError(f"no refs/heads/main on {remote_or_url}")


def remote_exists(name):
    return name in run(["git", "remote"]).split()


def last_main_ci(repo):
    runs = gh_json("run", "list", "-R", repo, "--branch", "main", "--limit", "20",
                   "--json", "databaseId,status,conclusion,headSha,createdAt,updatedAt,url,workflowName,event")
    runs = [r for r in runs or [] if r.get("workflowName") == "CI"] or (runs or [])
    if not runs:
        return None
    r = runs[0]
    done = [x for x in runs if x.get("status") == "completed"]
    return {"run_id": r.get("databaseId"), "status": r.get("status"), "conclusion": r.get("conclusion"),
            "sha": r.get("headSha"), "created": r.get("createdAt"), "updated": r.get("updatedAt"),
            "url": r.get("url"), "event": r.get("event"),
            "last_completed": ({"conclusion": done[0].get("conclusion"), "sha": done[0].get("headSha"),
                                "updated": done[0].get("updatedAt"), "url": done[0].get("url")} if done else None)}


def collect_main():
    out = {"remotes": [], "flags": []}
    for label, remote, repo in (("origin", REMOTE, REPO), ("mirror", MIRROR_REMOTE, MIRROR_REPO)):
        ent = {"label": label, "remote": remote, "repo": repo}
        try:
            ent["sha"] = ls_remote_main(remote if remote_exists(remote) else f"https://github.com/{repo}.git")
        except RuntimeError as e:
            ent["error"] = str(e)
        ent["ci"] = section(lambda repo=repo: last_main_ci(repo))
        ci = ent["ci"] if isinstance(ent["ci"], dict) else None
        if ci and "error" not in ci and ent.get("sha") and ci.get("sha") and ci["sha"] != ent["sha"]:
            ent["ci_note"] = f"latest CI ran {ci['sha'][:10]}, not main {ent['sha'][:10]}"
        out["remotes"].append(ent)
    shas = {e.get("sha") for e in out["remotes"]}
    if None in shas:
        out["flags"].append("could not read main on every remote")
    elif len(shas) != 1:
        a, b = out["remotes"][0]["sha"], out["remotes"][1]["sha"]
        out["flags"].append(f"MISMATCH: origin main {a[:10]} != mirror main {b[:10]}")
    out["in_sync"] = len(shas) == 1 and None not in shas
    for ent in out["remotes"]:
        oc = ent.get("ci")
        if isinstance(oc, dict) and oc.get("last_completed") and oc["last_completed"].get("conclusion") != "success":
            out["flags"].append(f"{ent['label']} main CI {oc['last_completed'].get('conclusion')}")
    return out


# ---------------------------------------------------------------- runs (one listing, shared)
def recent_runs():
    return gh_json("run", "list", "-R", REPO, "--limit", "200",
                   "--json", "databaseId,status,conclusion,headSha,headBranch,createdAt,updatedAt,url,workflowName,event,displayTitle") or []


def failing_jobs(run_id):
    v = gh_json("run", "view", str(run_id), "-R", REPO, "--json", "jobs")
    jobs = (v or {}).get("jobs") or []
    bad = [j.get("name") for j in jobs if j.get("conclusion") in ("failure", "timed_out", "startup_failure")]
    cancelled = sum(1 for j in jobs if j.get("conclusion") == "cancelled")
    active = sum(1 for j in jobs if j.get("status") in ("in_progress", "queued", "waiting", "pending"))
    done = sum(1 for j in jobs if j.get("status") == "completed")
    return {"failing": bad, "jobs_cancelled": cancelled, "jobs_total": len(jobs), "jobs_done": done, "jobs_active": active}


def collect_inflight(runs):
    ci = [r for r in runs if (r.get("headBranch") or "").startswith("ci/") and r.get("workflowName") == CI_WORKFLOW]
    active = [r for r in ci if r.get("status") != "completed"]
    recent = [r for r in ci if r.get("status") == "completed"][:6]
    rows = []
    for r in active + recent:
        created, updated = parse_ts(r.get("createdAt")), parse_ts(r.get("updatedAt"))
        row = {"branch": r.get("headBranch"), "title": (r.get("displayTitle") or "")[:120], "sha": r.get("headSha"), "run_id": r.get("databaseId"),
               "workflow": r.get("workflowName"), "status": r.get("status"), "conclusion": r.get("conclusion"),
               "created": r.get("createdAt"), "url": r.get("url"), "active": r.get("status") != "completed",
               "elapsed_s": (NOW - created) if r.get("status") != "completed" and created else
                            ((updated - created) if created and updated else None)}
        row["over_target"] = row["elapsed_s"] is not None and row["elapsed_s"] > TARGET_CI_S
        if row["active"] or r.get("conclusion") != "success":
            j = section(lambda rid=r.get("databaseId"): failing_jobs(rid))
            row.update(j if "error" not in j else {"jobs_error": j["error"]})
        rows.append(row)
    return {"active": sum(1 for x in rows if x["active"]), "runs": rows}


# ---------------------------------------------------------------- branches + notes
def note_for(sha, ref):
    for full in (f"refs/notes/{ref}", f"refs/notes/status-mirror/{ref}"):
        try:
            txt = run(["git", "notes", f"--ref={full}", "show", sha], timeout=10)
            if txt.strip():
                return txt.strip()
        except RuntimeError:
            continue
    return None


def collect_branches(runs):
    fmt = "%(refname)%09%(objectname)%09%(committerdate:unix)%09%(subject)"
    out = run(["git", "for-each-ref", f"--format={fmt}",
               f"refs/remotes/{REMOTE}/wip", f"refs/remotes/{REMOTE}/ready", f"refs/remotes/{REMOTE}/train"])
    main_ref = f"refs/remotes/{REMOTE}/main"
    last_run = {}
    for r in runs:  # newest first: keep the first run seen per branch
        last_run.setdefault(r.get("headBranch"), r)
    rows = []
    for line in out.splitlines():
        ref, sha, when, subject = (line.split("\t", 3) + [""] * 4)[:4]
        name = ref.split(f"refs/remotes/{REMOTE}/", 1)[1]
        try:
            behind, ahead = run(["git", "rev-list", "--left-right", "--count", f"{main_ref}...{sha}"]).split()
            behind, ahead = int(behind), int(ahead)
        except (RuntimeError, ValueError):
            behind = ahead = None
        r = last_run.get(name)
        row = {"name": name, "kind": name.split("/", 1)[0], "sha": sha, "subject": subject[:120],
               "last_commit": iso(int(when)) if when.isdigit() else None,
               "age_s": (NOW - int(when)) if when.isdigit() else None,
               "ahead": ahead, "behind": behind,
               "health": ({"workflow": r.get("workflowName"), "status": r.get("status"),
                           "conclusion": r.get("conclusion"), "url": r.get("url"),
                           "at_tip": r.get("headSha") == sha} if r else None),
               "review": note_for(sha, NOTES_REFS[0]), "gates": note_for(sha, NOTES_REFS[1])}
        row["merged"] = ahead == 0
        rows.append(row)
    order = {"train": 0, "ready": 1, "wip": 2}
    rows.sort(key=lambda x: (order.get(x["kind"], 9), x["merged"], x["age_s"] if x["age_s"] is not None else 1 << 40))
    return {"branches": rows}


# ---------------------------------------------------------------- landing log (R-0547)
def landing_log_dir():
    if os.environ.get("LAND_LOG_DIR"):
        return os.environ["LAND_LOG_DIR"]
    if os.environ.get("LAND_STATE_DIR"):
        return os.path.join(os.environ["LAND_STATE_DIR"], "log")
    common = run(["git", "rev-parse", "--git-common-dir"]).strip()
    return os.path.join(os.path.abspath(os.path.join(ROOT, common)), "concrete-landing", "log")


def collect_landing():
    log_dir = landing_log_dir()
    files = sorted(glob.glob(os.path.join(log_dir, "*.tsv")))
    trains, to_land, ci_wall = [], [], []
    ok = {"passed", "landing-green", "dispatched"}
    for f in files:
        lines = open(f, encoding="utf-8").read().splitlines()
        if not lines or not lines[0].startswith("utc\ttrain\tstage"):
            continue
        hdr = lines[0].split("\t")
        rows = [dict(zip(hdr, ln.split("\t"))) for ln in lines[1:] if ln]
        if not rows:
            continue
        stages = {}
        for r in rows:
            if r.get("result") in ok:
                try:
                    stages[r["stage"]] = stages.get(r["stage"], 0) + int(r.get("duration_s") or 0)
                except ValueError:
                    pass
        built = [parse_ts(r["utc"]) for r in rows if r["stage"] == "build" and r["result"] == "passed"]
        landed = [parse_ts(r["utc"]) for r in rows if r["stage"] == "land" and r["result"] == "passed"]
        disp = [parse_ts(r["utc"]) for r in rows if r["stage"] == "ci" and r["result"] == "dispatched"]
        verdict = [parse_ts(r["utc"]) for r in rows
                   if r["stage"] == "watch" and r["result"] in ("passed", "landing-green", "failed")]
        t_land = (landed[-1] - built[0]) if built and landed and None not in (built[0], landed[-1]) else None
        t_ci = (verdict[-1] - disp[-1]) if disp and verdict and None not in (disp[-1], verdict[-1]) and verdict[-1] >= disp[-1] else None
        if t_land is not None:
            to_land.append(t_land)
        if t_ci is not None:
            ci_wall.append(t_ci)
        last = rows[-1]
        trains.append({"train": last.get("train"), "last_utc": last.get("utc"), "last_stage": last.get("stage"),
                       "last_result": last.get("result"), "head": last.get("head"), "run_id": last.get("run_id"),
                       "members": len([m for m in (last.get("members") or "").split(",") if m and m != "-"]),
                       "landed": bool(landed), "time_to_land_s": t_land, "ci_wall_s": t_ci,
                       "stages": stages, "note": last.get("note")})
    trains.sort(key=lambda t: t["last_utc"] or "", reverse=True)
    med_land = statistics.median(to_land) if to_land else None
    med_ci = statistics.median(ci_wall) if ci_wall else None
    return {"log_dir_found": bool(files), "trains_logged": len(trains), "landed": len(to_land),
            "median_time_to_land_s": med_land, "median_ci_wall_s": med_ci,
            "target_land_s": TARGET_LAND_S, "target_ci_s": TARGET_CI_S,
            "land_on_target": None if med_land is None else med_land < TARGET_LAND_S,
            "ci_on_target": None if med_ci is None else med_ci <= TARGET_CI_S,
            "trains": trains[:10]}


# ---------------------------------------------------------------- open bugs
STATUS_LINE = re.compile(r"^\s*(?:\*\*Status:\*\*|Status:)\s*(.*)$", re.M)
TITLE_LINE = re.compile(r"^#\s+(.*)$", re.M)


def collect_bugs():
    bug_dir = os.path.join(ROOT, "docs", "bugs")
    ledger = os.path.join(ROOT, "scripts", "gen", "bug_ledger.py")
    source, open_from_ledger = "Status lines", None
    if os.path.isfile(ledger):
        try:
            out = run([sys.executable, "-I", ledger, "corpus"], timeout=60)
            rows = [ln.split("\t") for ln in out.splitlines() if ln.count("\t") >= 2]
            if rows:
                open_from_ledger = {r[0] for r in rows if r[2] == "open"}
                source = "scripts/gen/bug_ledger.py"
        except RuntimeError:
            pass  # pre-migration or refusal: fall back to the records' own Status lines
    bugs = []
    for path in sorted(glob.glob(os.path.join(bug_dir, "[0-9][0-9][0-9]_*.md"))):
        num = os.path.basename(path)[:3]
        text = open(path, encoding="utf-8", errors="replace").read()
        m = STATUS_LINE.search(text)
        status = m.group(1).strip() if m else ""
        word = re.sub(r"[^a-z]", " ", status.lower()).split()
        is_open = (num in open_from_ledger) if open_from_ledger is not None else bool(word and word[0] in OPEN_WORDS)
        if is_open:
            t = TITLE_LINE.search(text)
            bugs.append({"bug": num, "title": (t.group(1).strip() if t else os.path.basename(path))[:140],
                         "status": status[:100], "file": os.path.relpath(path, ROOT)})
    return {"source": source, "open": bugs, "count": len(bugs)}


# ---------------------------------------------------------------- server health
def collect_server():
    out = {}
    cmd = os.environ.get("STATUS_LOAD_CMD")
    try:
        if cmd:
            txt = subprocess.run(shlex.split(cmd), capture_output=True, text=True, timeout=20).stdout.split()
            out["load"] = [float(x) for x in txt[:3]]
            out["load_source"] = "STATUS_LOAD_CMD"
        else:
            out["load"] = list(os.getloadavg())
            out["load_source"] = "this host"
        out["cpus"] = os.cpu_count() if not cmd else None
    except (OSError, ValueError, subprocess.TimeoutExpired) as e:
        out["load_error"] = str(e)
    try:
        v = gh_json("api", f"repos/{REPO}/actions/runners")
        rs = (v or {}).get("runners") or []
        out["runners"] = [{"name": r.get("name"), "status": r.get("status"), "busy": r.get("busy")} for r in rs]
        out["runners_online"] = sum(1 for r in rs if r.get("status") == "online")
        out["runners_busy"] = sum(1 for r in rs if r.get("busy"))
        out["runners_total"] = len(rs)
    except (RuntimeError, ValueError) as e:
        out["runners_error"] = str(e)
    return out


# ---------------------------------------------------------------- sanitizer (public variant)
SECRET_PATTERNS = [
    (re.compile(r"-----BEGIN [A-Z ]*PRIVATE KEY-----.*?-----END [A-Z ]*PRIVATE KEY-----", re.S), "[key]"),
    (re.compile(r"https?://hooks\.slack\.com/\S+"), "[webhook]"),
    (re.compile(r"\b(?:gh[pousr]_[A-Za-z0-9]{20,}|github_pat_[A-Za-z0-9_]{20,})"), "[token]"),
    (re.compile(r"\bxox[abprs]-[A-Za-z0-9-]{10,}"), "[token]"),
    (re.compile(r"\bsk-[A-Za-z0-9_-]{20,}"), "[token]"),
    (re.compile(r"\bAKIA[0-9A-Z]{16}\b"), "[token]"),
    (re.compile(r"\bssh-(?:ed25519|rsa|dss|ecdsa[\w-]*) [A-Za-z0-9+/=]{20,}"), "[key]"),
    # key=value, key: value, "key": "value" for any key that names a credential, including prefixed
    # and suffixed forms (NTFY_TOKEN, AWS_SECRET_ACCESS_KEY, db_password); a quoted value goes whole.
    (re.compile(r"""(?i)\b([A-Za-z0-9_-]*?(?:token|secret|password|passwd|pass|pw|api[_-]?key|credential|topic|webhook[_-]?url)"""
                r"""(?:[_-][A-Za-z0-9_-]*)?)(["']?\s*[=:]\s*)("(?:\\[\s\S]|[^"\\])*"?|'(?:\\[\s\S]|[^'\\])*'?|\S+)"""), r"\1\2[redacted]"),
    (re.compile(r"(?i)\bbearer\s+[A-Za-z0-9._~+/-]{12,}=*"), "Bearer [token]"),
    (re.compile(r"\bntfy\.sh/[A-Za-z0-9_-]+"), "ntfy.sh/[topic]"),
    (re.compile(r"\b(?:\d{1,3}\.){3}\d{1,3}(?::\d+)?\b"), "[ip]"),
    (re.compile(r"(?<![\w:])[0-9A-Fa-f]{1,4}(?::[0-9A-Fa-f]{0,4}){3,7}(?![\w:])"), "[ip]"),
    (re.compile(r"(?<![\w:])[0-9A-Fa-f]{0,4}::(?:[0-9A-Fa-f]{1,4}:?){1,6}(?![\w:])"), "[ip]"),
    (re.compile(r"[\w.+-]+@[\w-]+(?:\.[\w-]+)+"), "[email]"),
    (re.compile(r"(?:/home|/Users|/root)(?:/[\w.-]+)?"), "~"),
]


def private_hosts():
    names = [h.strip() for h in os.environ.get("STATUS_PRIVATE_HOSTS", "").split(",") if h.strip()]
    return names + ["concrete-ci-1"]


def sanitize_text(s):
    if not isinstance(s, str):
        return s
    for host in sorted(private_hosts(), key=len, reverse=True):
        s = s.replace(host, "[host]")
    s = re.sub(r"\bconcrete-ci-\d+\b", "[host]", s)
    for rx, rep in SECRET_PATTERNS:
        s = rx.sub(rep, s)
    return s


def sanitize(obj):
    if isinstance(obj, dict):
        return {k: sanitize(v) for k, v in obj.items()}
    if isinstance(obj, list):
        return [sanitize(v) for v in obj]
    return sanitize_text(obj)


FREE_TEXT_KEYS = ("error", "jobs_error", "load_error", "runners_error", "note")


def public_view(data):
    """The publishable page. Free-form text that agents or tools write (review and gate notes, landing
    notes, error messages) is DROPPED, not pattern-scrubbed: no regex list can promise that arbitrary
    prose carries no secret. Only its presence survives. Every remaining string is sanitized too."""
    d = json.loads(json.dumps(data))

    def strip(o):
        if isinstance(o, dict):
            for k in list(o):
                if k in FREE_TEXT_KEYS and o[k]:
                    o[k] = "withheld in the public view"
                else:
                    strip(o[k])
        elif isinstance(o, list):
            for v in o:
                strip(v)
    strip(d)
    for b in (d.get("branches") or {}).get("branches") or []:
        for k in ("review", "gates"):
            b[k] = "present (withheld in the public view)" if b.get(k) else None
    srv = d.get("server") or {}
    for i, r in enumerate(srv.get("runners") or [], 1):
        r["name"] = f"runner {i}"
    srv.pop("load_source", None)
    d["public"] = True
    return sanitize(d)


# ---------------------------------------------------------------- HTML
CSS = """
:root{--bg:#f6f7f9;--card:#fff;--fg:#14171c;--mute:#5a6270;--line:#dde1e7;--ok:#0b6b2f;--okb:#e3f4e8;
--warn:#7a4b00;--warnb:#fff1d6;--bad:#a1121c;--badb:#fde4e6;--run:#0b4f8a;--runb:#e1eefb;--link:#0b57b0}
@media (prefers-color-scheme:dark){:root{--bg:#0f1115;--card:#181b21;--fg:#e8eaee;--mute:#a3abb8;--line:#2b3039;
--ok:#7ee2a0;--okb:#123221;--warn:#ffd08a;--warnb:#3a2b10;--bad:#ff9aa2;--badb:#3d161b;--run:#9cc9ff;--runb:#132a44;--link:#9cc9ff}}
*{box-sizing:border-box}html{-webkit-text-size-adjust:100%}
body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.45 -apple-system,system-ui,"Segoe UI",Roboto,sans-serif}
main{max-width:860px;margin:0 auto;padding:12px 16px 40px}
h1{font-size:20px;margin:8px 0 2px}h2{font-size:16px;margin:0 0 8px;display:flex;gap:8px;align-items:center;flex-wrap:wrap}
.sub{color:var(--mute);font-size:13px;margin-bottom:12px}
section{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:12px 14px;margin:12px 0}
.row{display:flex;flex-wrap:wrap;gap:4px 10px;align-items:baseline;padding:7px 0;border-top:1px solid var(--line)}
.row:first-of-type{border-top:0}.grow{flex:1 1 220px;min-width:0;overflow-wrap:anywhere}
.mute{color:var(--mute);font-size:13px}code{font:13px ui-monospace,SFMono-Regular,Menlo,monospace}
.b{display:inline-block;border-radius:999px;padding:1px 9px;font-size:12.5px;font-weight:600;white-space:nowrap}
.ok{color:var(--ok);background:var(--okb)}.warn{color:var(--warn);background:var(--warnb)}
.bad{color:var(--bad);background:var(--badb)}.run{color:var(--run);background:var(--runb)}.n{color:var(--mute);background:var(--bg)}
a{color:var(--link)}.flag{font-weight:600;color:var(--bad)}
.kpis{display:grid;grid-template-columns:repeat(auto-fit,minmax(130px,1fr));gap:8px}
.kpi{border:1px solid var(--line);border-radius:8px;padding:8px 10px}.kpi b{display:block;font-size:20px}
details summary{cursor:pointer;color:var(--mute);font-size:13px}pre{white-space:pre-wrap;margin:4px 0;font-size:12.5px}
#stale{display:none}
"""


def e(s):
    return html.escape("" if s is None else str(s))


def badge(text, kind):
    return f'<span class="b {kind}">{e(text)}</span>'


def concl_badge(status, conclusion):
    if status and status != "completed":
        return badge(status.replace("_", " "), "run")
    return badge(conclusion or "unknown", {"success": "ok", "failure": "bad", "cancelled": "warn",
                                           "timed_out": "bad", "skipped": "n"}.get(conclusion, "warn"))


def link(url, text):
    return f'<a href="{e(url)}">{e(text)}</a>' if url else e(text)


def err(sec):
    return f'<p class="mute">unavailable: {e(sec.get("error"))}</p>' if isinstance(sec, dict) and "error" in sec else ""


def render_main(m):
    if "error" in m:
        return err(m)
    h = [f'<h2>Main {badge("in sync", "ok") if m.get("in_sync") else badge("NOT IN SYNC", "bad")}</h2>']
    for f in m.get("flags", []):
        h.append(f'<p class="flag">{e(f)}</p>')
    for r in m["remotes"]:
        ci = r.get("ci") or {}
        lc = ci.get("last_completed") if isinstance(ci, dict) else None
        ci_html = (f'{concl_badge(ci.get("status"), ci.get("conclusion"))} {link(ci.get("url"), "run")} '
                   f'<span class="mute">{e(ci.get("updated"))}</span>') if ci and "error" not in ci else \
            f'<span class="mute">CI: {e(ci.get("error") if isinstance(ci, dict) else "none")}</span>'
        extra = ""
        if lc and ci.get("status") != "completed":
            extra = f' <span class="mute">last finished:</span> {concl_badge("completed", lc.get("conclusion"))}'
        h.append(f'<div class="row"><b>{e(r["label"])}</b><span class="mute">{e(r["repo"])}</span>'
                 f'<code class="grow">{e((r.get("sha") or r.get("error") or "")[:12])}</code>{ci_html}{extra}'
                 + (f'<div class="mute" style="flex-basis:100%">{e(r["ci_note"])}</div>' if r.get("ci_note") else "")
                 + "</div>")
    return "".join(h)


def render_inflight(f):
    if "error" in f:
        return "<h2>In flight</h2>" + err(f)
    h = [f'<h2>In flight (ci/*) {badge(str(f["active"]) + " running", "run" if f["active"] else "n")}</h2>']
    if not f["runs"]:
        h.append('<p class="mute">no ci/* runs</p>')
    for r in f["runs"]:
        jobs = ""
        if r.get("jobs_total"):
            jobs = (f'<span class="mute">jobs {r.get("jobs_done")}/{r["jobs_total"]}'
                    + (f', {r["jobs_cancelled"]} cancelled' if r.get("jobs_cancelled") else "") + "</span>")
        fail = "".join(f'<div class="flag" style="flex-basis:100%">failing: {e(j)}</div>' for j in r.get("failing") or [])
        el = badge(dur(r.get("elapsed_s")), "bad" if r.get("over_target") else "n")
        h.append(f'<div class="row"><b class="grow">{link(r.get("url"), r.get("branch"))}</b>'
                 f'{concl_badge(r.get("status"), r.get("conclusion"))}{el}{jobs}'
                 f'<code class="mute">{e((r.get("sha") or "")[:10])}</code>'
                 f'<div class="mute" style="flex-basis:100%">{e(r.get("title"))}</div>{fail}</div>')
    return "".join(h)


def render_branches(b):
    if "error" in b:
        return "<h2>Branches</h2>" + err(b)
    rows = b["branches"]
    live = [r for r in rows if not r["merged"]]
    h = [f'<h2>Branches {badge(str(len(live)) + " unmerged", "n")}</h2>']
    for r in rows:
        if r["merged"]:
            continue
        hl = r.get("health")
        hb = concl_badge(hl.get("status"), hl.get("conclusion")) + ("" if hl.get("at_tip") else badge("old tip", "n")) if hl else ""
        stale = r["age_s"] is not None and r["age_s"] > STALE_BRANCH_DAYS * 86400
        notes = ""
        for k in ("review", "gates"):
            if r.get(k):
                notes += f'<details><summary>{k} note</summary><pre>{e(r[k][:2000])}</pre></details>'
        behind = r.get("behind")
        h.append(f'<div class="row"><b class="grow">{e(r["name"])}</b>'
                 f'{badge("+" + str(r.get("ahead")) + " / -" + str(behind), "warn" if behind and behind > 50 else "n")}'
                 f'{badge(dur(r.get("age_s")) + " ago", "warn" if stale else "n")}{hb}'
                 + (badge("reviewed", "ok") if r.get("review") else "") + (badge("gated", "ok") if r.get("gates") else "")
                 + f'<div class="mute" style="flex-basis:100%">{e(r["subject"])}</div>{notes}</div>')
    merged = [r["name"] for r in rows if r["merged"]]
    if merged:
        h.append(f'<details><summary>{len(merged)} already in main</summary><p class="mute">{e(", ".join(merged))}</p></details>')
    return "".join(h)


def render_landing(l):
    if "error" in l:
        return "<h2>Landing</h2>" + err(l)

    def kpi(label, val, target, on):
        cls = "n" if on is None else ("ok" if on else "bad")
        return f'<div class="kpi"><span class="mute">{e(label)}</span><b>{e(dur(val) if val is not None else "-")}</b>{badge("target " + target, cls)}</div>'
    h = ["<h2>Landing (R-0547)</h2>", '<div class="kpis">',
         kpi("median time to land", l["median_time_to_land_s"], "< 2h", l["land_on_target"]),
         kpi("median landing CI", l["median_ci_wall_s"], "<= 40m", l["ci_on_target"]),
         f'<div class="kpi"><span class="mute">trains logged / landed</span><b>{l["trains_logged"]} / {l["landed"]}</b></div>',
         "</div>"]
    if not l["log_dir_found"]:
        h.append('<p class="mute">no landing log yet (scripts/land/train.sh writes one per train)</p>')
    for t in l["trains"]:
        st = " ".join(f"{k} {dur(v)}" for k, v in t["stages"].items())
        res = badge("landed", "ok") if t["landed"] else badge(f'{t["last_stage"]}: {t["last_result"]}',
                                                              "bad" if t["last_result"] in ("failed", "conflict", "refused") else "run")
        tl = t.get("time_to_land_s")
        h.append(f'<div class="row"><b class="grow">{e(t["train"])}</b>{res}'
                 + (badge("land " + dur(tl), "ok" if tl < TARGET_LAND_S else "bad") if tl is not None else "")
                 + (badge("CI " + dur(t["ci_wall_s"]), "ok" if t["ci_wall_s"] <= TARGET_CI_S else "bad") if t.get("ci_wall_s") is not None else "")
                 + f'<span class="mute">{t["members"]} member(s) · {e(t["last_utc"])}</span>'
                 + f'<div class="mute" style="flex-basis:100%">{e(st)}</div></div>')
    return "".join(h)


def render_bugs(b):
    if "error" in b:
        return "<h2>Open bugs</h2>" + err(b)
    h = [f'<h2>Open bugs {badge(str(b["count"]), "warn" if b["count"] else "ok")}</h2>',
         f'<p class="mute">source: {e(b["source"])}</p><details><summary>list</summary>']
    for x in b["open"]:
        h.append(f'<div class="row"><code>{e(x["bug"])}</code><span class="grow">{e(x["title"])}</span></div>')
    h.append("</details>")
    return "".join(h)


def render_server(s):
    if "error" in s:
        return "<h2>Server</h2>" + err(s)
    h = ["<h2>Server</h2>"]
    if "load" in s:
        cpus = f' / {s["cpus"]} cpus' if s.get("cpus") else ""
        h.append(f'<div class="row"><b class="grow">load</b><code>{" ".join(f"{x:.2f}" for x in s["load"])}</code>'
                 f'<span class="mute">1/5/15 min{e(cpus)}</span></div>')
    elif s.get("load_error"):
        h.append(f'<p class="mute">load unavailable: {e(s["load_error"])}</p>')
    if "runners_total" in s:
        allup = s["runners_online"] == s["runners_total"] and s["runners_total"] > 0
        h.append(f'<div class="row"><b class="grow">runners</b>'
                 f'{badge(str(s["runners_online"]) + "/" + str(s["runners_total"]) + " online", "ok" if allup else "bad")}'
                 f'{badge(str(s["runners_busy"]) + " busy", "run" if s["runners_busy"] else "n")}</div>')
        for r in s.get("runners") or []:
            if r.get("status") != "online":
                h.append(f'<div class="row flag">{e(r.get("name"))}: {e(r.get("status"))}</div>')
    elif s.get("runners_error"):
        h.append(f'<p class="mute">runners unavailable: {e(s["runners_error"])}</p>')
    return "".join(h)


def headline(d):
    bad, warn = [], []
    m = d.get("main") or {}
    bad += m.get("flags") or []
    f = d.get("inflight") or {}
    for r in f.get("runs") or []:
        if r.get("active") and r.get("failing"):
            bad.append(f'{r["branch"]}: {len(r["failing"])} job(s) failing')
        if r.get("active") and r.get("over_target"):
            warn.append(f'{r["branch"]} over 40m')
    s = d.get("server") or {}
    if "runners_total" in s and s["runners_online"] < s["runners_total"]:
        bad.append(f'{s["runners_total"] - s["runners_online"]} runner(s) offline')
    for k in ("main", "inflight", "branches", "landing", "bugs", "server"):
        if isinstance(d.get(k), dict) and "error" in d[k]:
            warn.append(f"{k} unavailable")
    return bad, warn


def render_html(d, refresh):
    bad, warn = headline(d)
    if bad:
        top = badge("attention", "bad") + "".join(f'<div class="flag">{e(x)}</div>' for x in bad)
    elif warn:
        top = badge("watch", "warn") + "".join(f'<div class="mute">{e(x)}</div>' for x in warn)
    else:
        top = badge("all clear", "ok")
    body = "".join(f"<section>{fn(d[k])}</section>" for k, fn in (
        ("main", render_main), ("inflight", render_inflight), ("landing", render_landing),
        ("branches", render_branches), ("bugs", render_bugs), ("server", render_server)))
    stale = (f'<script>(function(){{var g={d["generated_epoch"]}*1000;function t(){{var el=document.getElementById("stale");'
             f'el.style.display=(Date.now()-g>{refresh * 4}*1000)?"block":"none"}}t();setInterval(t,30000)}})();</script>')
    return ("<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
            "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
            f"<meta http-equiv=\"refresh\" content=\"{int(refresh)}\">"
            "<meta name=\"color-scheme\" content=\"light dark\">"
            f"<title>Concrete status</title><style>{CSS}</style></head><body><main>"
            f"<h1>Concrete status</h1><div class=\"sub\">generated {e(d['generated'])} · refreshes every {int(refresh)}s"
            + (" · public view" if d.get("public") else "") + "</div>"
            f"<section id=\"stale\" class=\"flag\">This page is stale: the generator has not run for a while.</section>"
            f"<section><h2>Now</h2>{top}</section>{body}</main>{stale}</body></html>\n")


# ---------------------------------------------------------------- output
def write_atomic(path, text):
    d = os.path.dirname(path) or "."
    fd, tmp = tempfile.mkstemp(dir=d, prefix=".status-")
    with os.fdopen(fd, "w", encoding="utf-8") as fh:
        fh.write(text)
    os.chmod(tmp, 0o644)
    os.replace(tmp, path)


def write_page(data, out_dir, refresh):
    os.makedirs(out_dir, exist_ok=True)
    write_atomic(os.path.join(out_dir, "status.json"), json.dumps(data, indent=1, sort_keys=True) + "\n")
    write_atomic(os.path.join(out_dir, "status.html"), render_html(data, refresh))


def fetch():
    run(["git", "fetch", "--quiet", "--prune", REMOTE], timeout=300)
    for ref in NOTES_REFS:  # into a private mirror namespace: never overwrite a local note ref
        run(["git", "fetch", "--quiet", REMOTE, f"+refs/notes/{ref}:refs/notes/status-mirror/{ref}"],
            timeout=120, check=False)


def collect():
    runs = section(recent_runs)
    runs_list = runs if isinstance(runs, list) else []
    inflight = section(lambda: collect_inflight(runs_list)) if isinstance(runs, list) else runs
    return {"generated": iso(NOW), "generated_epoch": NOW, "repo": REPO, "mirror_repo": MIRROR_REPO,
            "main": section(collect_main), "inflight": inflight,
            "branches": section(lambda: collect_branches(runs_list)),
            "landing": section(collect_landing), "bugs": section(collect_bugs), "server": section(collect_server)}


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--out", default="status-out")
    ap.add_argument("--public-out")
    ap.add_argument("--fetch", action="store_true")
    ap.add_argument("--refresh", type=int, default=120)
    a = ap.parse_args()
    if a.fetch:
        section(fetch)
    data = collect()
    write_page(data, a.out, a.refresh)
    if a.public_out:
        write_page(public_view(data), a.public_out, a.refresh)
    bad, warn = headline(data)
    print(f"status: wrote {a.out}/status.html" + (f" and {a.public_out}/status.html (public)" if a.public_out else "")
          + f"; {len(bad)} attention, {len(warn)} watch")


if __name__ == "__main__":
    main()
