#!/usr/bin/env python3
"""The work-queue dispatcher. docs/project/WORK_QUEUE.md is the manual; dispatch.sh is the entry.

  dispatch.py tick              one pass: rebuild queue.json, watch running agents, start new ones
  dispatch.py status            print status.json
  dispatch.py release <task>    stop a task's agent (if running) and put the task back in the queue

A TICK is idempotent and holds an exclusive lock, so a systemd timer can run it every few minutes.
Agents outlive the tick: each runs detached in its own session, and later ticks watch it through
its pid, its exit file and its wip pushes.

LIFECYCLE of a task in state.json:

  (absent|pending) --start--> running --process exits--> exited
  running|exited --head of its wip ref has a clean review note and a passing gates note--> done
  running|exited --no new wip push for QUEUE_STUCK_MINUTES--> stuck: killed, released
  running --exits nonzero (not a usage limit)--> failed: released
  running --usage-limit text--> the slot cools down for an hour; the task is released, not charged
  released: pending again, or abandoned once QUEUE_MAX_ATTEMPTS failures are recorded

A done task's branch is the landing bot's: this dispatcher never pushes to main or ci/*.

Everything is configured by environment (dispatch.sh reads ~/.config/concrete-queue/env first);
the knobs are listed in CONFIG below. QUEUE_NOW pins the clock (a test seam).
"""
import fcntl
import json
import os
import re
import signal
import subprocess
import sys
import time
import uuid

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import build_queue  # noqa: E402

HOME = os.path.expanduser("~")
E = os.environ.get


def env_int(name, default):
    v = E(name, "")
    try:
        return int(v) if v != "" else default
    except ValueError:
        sys.exit(f"dispatch: {name}={v!r} is not an integer")


def env_float(name, default):
    v = E(name, "")
    try:
        return float(v) if v != "" else default
    except ValueError:
        sys.exit(f"dispatch: {name}={v!r} is not a number")


CONFIG = {
    "home": E("QUEUE_HOME", os.path.join(HOME, ".local/state/concrete-queue")),
    "repo": E("QUEUE_REPO", os.path.join(HOME, "queue/concrete2")),
    "source_root": E("QUEUE_SOURCE_ROOT", ""),          # default: <home>/source, detached at base
    "remote": E("QUEUE_REMOTE", "origin"),
    "base": E("QUEUE_BASE", "origin/main"),
    "wip_prefix": E("QUEUE_WIP_PREFIX", "wip/queue/"),
    "branch_prefix": E("QUEUE_BRANCH_PREFIX", "queue/"),
    "max_agents": env_int("QUEUE_MAX_AGENTS", 3),
    "max_agent_hours": env_float("QUEUE_MAX_AGENT_HOURS", 36.0),   # per UTC day; 0 = no cap
    "max_daily_usd": env_float("QUEUE_MAX_DAILY_USD", 0.0),         # 0 = no cap
    "max_daily_tokens": env_int("QUEUE_MAX_DAILY_TOKENS", 0),        # 0 = no cap
    "task_max_usd": E("QUEUE_TASK_MAX_USD", ""),                    # claude --max-budget-usd
    "slots": E("QUEUE_SLOTS", "1 2 3 4").split(),
    "slot_cooldown": env_int("QUEUE_SLOT_COOLDOWN", 3600),
    "stuck_minutes": env_float("QUEUE_STUCK_MINUTES", 60.0),
    "max_attempts": env_int("QUEUE_MAX_ATTEMPTS", 2),
    "pause_file": E("QUEUE_PAUSE_FILE", os.path.join(HOME, ".concrete-queue-paused")),
    "claude": E("QUEUE_CLAUDE", "claude"),
    "cswap": E("QUEUE_CSWAP", "cswap"),
    "model": E("QUEUE_MODEL", ""),
    "notify": E("QUEUE_NOTIFY", ""),
    "status_json": E("QUEUE_STATUS_JSON", ""),
    "fuzz_dir": E("QUEUE_FUZZ_DIR", ""),
    "no_fetch": E("QUEUE_NO_FETCH", "0") == "1",
    "require_guard": E("QUEUE_REQUIRE_GUARD", "1") == "1",
    "usage_limit_re": E("QUEUE_USAGE_LIMIT_RE",
                        r"usage limit|hit your (?:usage )?limit|limit reached|out of extra usage|"
                        r"rate_limit_error|\b429\b"),
}
ACTIVE = ("running", "exited")
TOOLS_ALLOW = ["Read", "Edit", "Write", "Glob", "Grep", "NotebookEdit", "TodoWrite", "Task", "Agent",
               "Skill", "Bash"]
# Guard rails, not the security boundary (WORK_QUEUE.md, "Trust model"): the boundary is the
# `agent` Unix user, the queue clone's push guard and branch protection on main.
TOOLS_DENY = [
    "WebFetch", "WebSearch",
    "Bash(git push * main)", "Bash(git push * main *)", "Bash(git push *:main*)",
    "Bash(git push *refs/heads/main*)", "Bash(git push * ci/*)", "Bash(git push *:ci/*)",
    "Bash(git push --force*)", "Bash(git push -f *)", "Bash(git push * --force*)",
    "Bash(git push * -f *)", "Bash(git push fork*)", "Bash(git push lambdaclass*)",
    "Bash(git remote add*)", "Bash(git remote set-url*)",
    "Bash(*--no-verify*)", "Bash(*CONCRETE_SKIP_GATES*)", "Bash(*core.hooksPath*)",
    "Bash(git config --global*)", "Bash(gh pr merge*)", "Bash(gh api *)", "Bash(sudo *)",
    "Bash(*scripts/land/train.sh land*)", "Bash(*concrete-queue-paused*)",
    "Edit(~/.claude/**)", "Write(~/.claude/**)", "Edit(~/.ssh/**)", "Write(~/.ssh/**)",
    "Edit(~/.config/**)", "Write(~/.config/**)", "Edit(~/.local/**)", "Write(~/.local/**)",
    "Edit(~/.bashrc)", "Write(~/.bashrc)", "Edit(~/.profile)", "Write(~/.profile)",
]


def now():
    v = E("QUEUE_NOW", "")
    return float(v) if v else time.time()


def utc(t=None):
    return time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(now() if t is None else t))


def day_start(t):
    return t - (t % 86400)


class Q:
    def __init__(self, cfg):
        self.cfg = cfg
        self.home = cfg["home"]
        os.makedirs(os.path.join(self.home, "runs"), exist_ok=True)
        self.state_path = os.path.join(self.home, "state.json")
        self.log_path = os.path.join(self.home, "events.log")
        self.state = self.load()

    # ------------------------------------------------------------ state and events
    def load(self):
        try:
            with open(self.state_path, encoding="utf-8") as f:
                st = json.load(f)
        except FileNotFoundError:
            st = {}
        except ValueError as e:
            sys.exit(f"dispatch: {self.state_path} is not JSON ({e}); refusing to guess the running set")
        st.setdefault("tasks", {})
        st.setdefault("slots", {})
        st.setdefault("runs", [])
        st.setdefault("flags", {})
        return st

    def save(self):
        tmp = self.state_path + ".tmp"
        with open(tmp, "w", encoding="utf-8") as f:
            json.dump(self.state, f, indent=1, sort_keys=True)
        os.replace(tmp, self.state_path)

    def event(self, kind, task=None, **kw):
        rec = {"utc": utc(), "event": kind}
        if task:
            rec["task"] = task
        rec.update(kw)
        with open(self.log_path, "a", encoding="utf-8") as f:
            f.write(json.dumps(rec, sort_keys=True) + "\n")
        return rec

    def flag(self, name, on, kind, **kw):
        """Log a condition only when it changes, so a paused or exhausted queue does not fill the log."""
        if bool(self.state["flags"].get(name)) != bool(on):
            self.state["flags"][name] = bool(on)
            self.event(kind if on else kind + "_cleared", **kw)
            if on:
                self.notify(f"queue: {kind}", json.dumps(kw, sort_keys=True))

    def notify(self, title, body):
        cmd = self.cfg["notify"]
        if not cmd:
            for rel in ("scripts/status/notify.sh", "scripts/notify.sh"):
                p = os.path.join(self.source_root(), rel)
                if os.path.isfile(p):
                    cmd = p
                    break
        if not cmd:
            return
        try:
            subprocess.run(["bash", cmd, title, body], timeout=60, capture_output=True)
        except (OSError, subprocess.SubprocessError) as e:
            self.event("notify_failed", error=str(e)[:200])

    # ------------------------------------------------------------ git
    def git(self, *args, cwd=None, check=True):
        r = subprocess.run(["git", *args], cwd=cwd or self.cfg["repo"], capture_output=True, text=True)
        if check and r.returncode != 0:
            raise RuntimeError(f"git {' '.join(args)}: {r.stderr.strip()[:300]}")
        return r.stdout.strip()

    def source_root(self):
        return self.cfg["source_root"] or os.path.join(self.home, "source")

    def refresh_sources(self):
        if not self.cfg["no_fetch"]:
            self.git("fetch", "-q", "--prune", self.cfg["remote"])
        if self.cfg["source_root"]:
            return
        src = self.source_root()
        if not os.path.isdir(src):
            self.git("worktree", "add", "-q", "--detach", src, self.cfg["base"])
        else:
            self.git("checkout", "-q", "--detach", "-f", self.cfg["base"], cwd=src)

    def guard_ok(self):
        if not self.cfg["require_guard"]:
            return True
        hp = self.git("config", "--get", "core.hooksPath", check=False)
        p = os.path.join(hp, "pre-push") if hp else ""
        return bool(p) and os.path.isfile(p) and "concrete-queue push guard" in open(p, encoding="utf-8").read()

    def remote_sha(self, ref):
        out = self.git("ls-remote", self.cfg["remote"], "refs/heads/" + ref, check=False)
        return out.split()[0] if out else ""

    def ready(self, sha):
        """The landing bot's readiness: clean review note and passing gates note on exactly this SHA."""
        notes_sh = os.path.join(self.source_root(), "scripts/land/notes.sh")
        env = dict(os.environ, LAND_REMOTE=self.cfg["remote"])
        if os.path.isfile(notes_sh):
            subprocess.run(["bash", notes_sh, "fetch"], cwd=self.cfg["repo"], env=env, capture_output=True)
            return subprocess.run(["bash", notes_sh, "ready", sha], cwd=self.cfg["repo"], env=env,
                                  capture_output=True).returncode == 0
        # The same contract, read directly (notes.sh is on the landing-bot branch until it lands).
        vals = {}
        for kind, key in (("review", "verdict"), ("gates", "gates")):
            self.git("fetch", "-q", self.cfg["remote"],
                     f"+refs/notes/concrete-{kind}:refs/notes/remote/concrete-{kind}", check=False)
            txt = self.git("notes", f"--ref=refs/notes/remote/concrete-{kind}", "show", sha, check=False)
            hits = re.findall(r"^" + key + r":[ \t]*(.*?)[ \t]*$", txt, re.M)
            vals[kind] = hits[-1] if hits else ""
        return vals["review"] == "clean" and vals["gates"] == "pass"

    # ------------------------------------------------------------ processes
    @staticmethod
    def alive(pid):
        if not pid:
            return False
        try:
            os.kill(pid, 0)
        except ProcessLookupError:
            return False
        except PermissionError:
            return True
        try:  # a zombie child of this process counts as gone
            return os.waitpid(pid, os.WNOHANG) == (0, 0)
        except ChildProcessError:
            return True

    def kill(self, t):
        pid = t.get("pid")
        if self.alive(pid):
            try:
                os.killpg(pid, signal.SIGTERM)
            except (ProcessLookupError, PermissionError):
                pass

    def run_result(self, run_dir):
        """(exit code or None, usage-limit hit, cost usd, tokens) from a finished run."""
        rc = None
        try:
            rc = int(open(os.path.join(run_dir, "exit"), encoding="utf-8").read().strip())
        except (OSError, ValueError):
            pass
        cost, tokens, limit_text = 0.0, 0, []
        try:
            lines = open(os.path.join(run_dir, "agent.log"), encoding="utf-8", errors="replace").read().splitlines()
        except OSError:
            lines = []
        result = None
        for ln in lines:
            try:
                ev = json.loads(ln)
            except ValueError:
                limit_text.append(ln)   # stderr and plain-text lines (the CLI's own failures)
                continue
            if isinstance(ev, dict) and ev.get("type") == "result":
                result = ev
        if result:
            cost = float(result.get("total_cost_usd") or 0)
            tokens = sum(int(v) for k, v in (result.get("usage") or {}).items()
                         if k.endswith("_tokens") and isinstance(v, int))
            if result.get("is_error"):
                limit_text.append(str(result.get("result", "")))
        # Only the CLI's own error text decides a usage limit, never tool output inside the stream.
        hit = bool(re.search(self.cfg["usage_limit_re"], "\n".join(limit_text), re.I))
        return rc, hit, cost, tokens

    # ------------------------------------------------------------ budgets
    def usage_today(self, t_now):
        d0 = day_start(t_now)
        hours = usd = 0.0
        tokens = 0
        for r in self.state["runs"]:
            end = r.get("end") or t_now
            hours += max(0.0, min(end, t_now) - max(r["start"], d0)) / 3600
            if r.get("end") and r["end"] >= d0:
                usd += r.get("usd", 0.0)
                tokens += r.get("tokens", 0)
        return {"agent_hours": round(hours, 3), "usd": round(usd, 4), "tokens": tokens}

    def budget_block(self, t_now):
        u = self.usage_today(t_now)
        c = self.cfg
        if c["max_agent_hours"] and u["agent_hours"] >= c["max_agent_hours"]:
            return f"agent-hours {u['agent_hours']} >= {c['max_agent_hours']}"
        if c["max_daily_usd"] and u["usd"] >= c["max_daily_usd"]:
            return f"usd {u['usd']} >= {c['max_daily_usd']}"
        if c["max_daily_tokens"] and u["tokens"] >= c["max_daily_tokens"]:
            return f"tokens {u['tokens']} >= {c['max_daily_tokens']}"
        return ""

    def pick_slot(self, t_now):
        running = {}
        for t in self.state["tasks"].values():
            if t.get("status") == "running":
                running[t.get("slot")] = running.get(t.get("slot"), 0) + 1
        free = [s for s in self.cfg["slots"]
                if t_now - self.state["slots"].get(s, {}).get("limited_at", -1e18) >= self.cfg["slot_cooldown"]]
        if not free:
            return None
        return min(free, key=lambda s: (running.get(s, 0), self.state["slots"].get(s, {}).get("last_start", 0),
                                        self.cfg["slots"].index(s)))

    # ------------------------------------------------------------ lifecycle
    def release(self, tid, t, why, charge):
        self.kill(t)
        if charge:
            t["attempts"] = t.get("attempts", 0) + 1
        t["status"] = "abandoned" if t.get("attempts", 0) >= self.cfg["max_attempts"] else "pending"
        t["released"] = utc()
        t["release_reason"] = why
        t.pop("pid", None)
        self.event("released", tid, reason=why, status=t["status"], attempts=t.get("attempts", 0))
        if t["status"] == "abandoned":
            self.notify(f"queue: {tid} abandoned", f"{why}; {t.get('attempts')} failed attempts; needs a human")

    def close_run(self, t, t_now, rc, hit, cost, tokens):
        for r in reversed(self.state["runs"]):
            if r.get("run_dir") == t.get("run_dir") and not r.get("end"):
                r.update(end=t_now, rc=rc, usd=cost, tokens=tokens, usage_limit=hit)
                break

    def cleanup(self, tid, t):
        """Remove a done task's worktree once its agent is gone and everything in it is published."""
        if self.alive(t.get("pid")):
            return
        if t.get("pid"):  # the agent outlived the readiness notes; account for its run now
            rc, hit, cost, tokens = self.run_result(t["run_dir"])
            self.close_run(t, now(), rc, hit, cost, tokens)
            t.pop("pid", None)
        wt = t.get("worktree")
        if not wt or not os.path.isdir(wt):
            return
        head = self.git("rev-parse", "HEAD", cwd=wt, check=False)
        dirty = self.git("status", "--porcelain", cwd=wt, check=False)
        if head and head == t.get("last_sha") and not dirty:
            self.git("worktree", "remove", "--force", wt, check=False)
            self.event("worktree_removed", tid, worktree=wt)

    def monitor(self, t_now):
        stuck_s = self.cfg["stuck_minutes"] * 60
        for tid, t in self.state["tasks"].items():
            if t.get("status") == "done":
                self.cleanup(tid, t)
            if t.get("status") not in ACTIVE:
                continue
            if t["status"] == "running" and not self.alive(t.get("pid")):
                rc, hit, cost, tokens = self.run_result(t["run_dir"])
                self.close_run(t, t_now, rc, hit, cost, tokens)
                if hit:
                    s = t["slot"]
                    self.state["slots"].setdefault(s, {})["limited_at"] = t_now
                    self.event("usage_limit", tid, slot=s)
                    self.notify(f"queue: slot {s} hit its usage limit", f"{tid} released; slot rests 1 h")
                    self.release(tid, t, f"usage limit on slot {s}", charge=False)
                    continue
                t["status"] = "exited"
                t["exit"] = rc
                self.event("exited", tid, rc=rc, usd=cost, tokens=tokens)
                if rc != 0:
                    self.notify(f"queue: {tid} failed", f"agent exited {rc}; log {t['run_dir']}/agent.log")
                    self.release(tid, t, f"agent exited {rc}", charge=True)
                    continue
            sha = self.remote_sha(t["wip_ref"])
            if sha and sha != t.get("last_sha"):
                t["last_sha"] = sha
                t["last_push"] = t_now
                self.event("heartbeat", tid, sha=sha)
            if t.get("last_sha") and self.ready(t["last_sha"]):
                t["status"] = "done"
                t["done"] = utc()
                self.event("done", tid, sha=t["last_sha"], branch=t["wip_ref"])
                self.notify(f"queue: {tid} ready to land", f"{t['wip_ref']} at {t['last_sha']}")
                continue
            if t_now - (t.get("last_push") or t["started_at"]) > stuck_s:
                self.event("stuck", tid, minutes=self.cfg["stuck_minutes"], last_sha=t.get("last_sha", ""))
                self.notify(f"queue: {tid} stuck",
                            f"no push to {t['wip_ref']} for {self.cfg['stuck_minutes']:g} min; released")
                if t["status"] == "running":
                    self.close_run(t, t_now, None, False, 0.0, 0)
                self.release(tid, t, "stuck: no wip push", charge=True)

    def deps_ok(self, task, by_id):
        for d in task.get("depends", []):
            if d in by_id and self.state["tasks"].get(d, {}).get("status") != "done" \
                    and by_id[d].get("disposition") != "done":
                return False
        return True

    @staticmethod
    def conflicts(a, b):
        for x in a:
            for y in b:
                if x == y:
                    return True
                if x.startswith("path:") and y.startswith("path:"):
                    px, py = x[5:].rstrip("/") + "/", y[5:].rstrip("/") + "/"
                    if px.startswith(py) or py.startswith(px):
                        return True
        return False

    def start(self, task, slot, t_now):
        tid = task["id"]
        t = self.state["tasks"].setdefault(tid, {})
        branch = self.cfg["branch_prefix"] + tid
        wip_ref = self.cfg["wip_prefix"] + tid
        wt = os.path.join(self.home, "worktrees", tid)
        if not os.path.isdir(wt):
            remote_wip = f"refs/remotes/{self.cfg['remote']}/{wip_ref}"
            start_at = remote_wip if self.git("rev-parse", "-q", "--verify", remote_wip, check=False) \
                else self.cfg["base"]
            self.git("worktree", "add", "-q", "-B", branch, wt, start_at)
        n = len([r for r in self.state["runs"] if r["task"] == tid]) + 1
        run_dir = os.path.join(self.home, "runs", f"{tid}.{n}")
        os.makedirs(run_dir, exist_ok=True)
        session = str(uuid.uuid4())
        with open(os.path.join(run_dir, "settings.json"), "w", encoding="utf-8") as f:
            json.dump({"permissions": {"allow": TOOLS_ALLOW, "deny": TOOLS_DENY, "defaultMode": "dontAsk"}}, f,
                      indent=1)
        context = (
            f"You are an autonomous agent started by the Concrete work queue (scripts/queue/, "
            f"docs/project/WORK_QUEUE.md). Task {tid}: {task.get('title', '')}. Your worktree is {wt} on "
            f"branch {branch}; work only there. Publish work in progress with `git push {self.cfg['remote']} "
            f"HEAD:refs/heads/{wip_ref}` at least every 5 minutes of work: the queue reads those pushes as "
            f"your heartbeat and releases the task after {self.cfg['stuck_minutes']:g} minutes without one. "
            f"The task is done when the head of {wip_ref} carries a clean review note and a passing gates "
            f"note (refs/notes/concrete-review, refs/notes/concrete-gates); the landing bot lands it. Never "
            f"push to main or ci/*, never to a remote other than {self.cfg['remote']}, never bypass hooks. "
            f"No human is watching this session: when blocked, record why in a commit on your wip ref and "
            f"stop.")
        cmd = [self.cfg["claude"], "-p", task["prompt"], "--permission-mode", "dontAsk",
               "--settings", os.path.join(run_dir, "settings.json"), "--output-format", "stream-json",
               "--verbose", "--session-id", session, "--append-system-prompt", context]
        if self.cfg["task_max_usd"]:
            cmd += ["--max-budget-usd", self.cfg["task_max_usd"]]
        if self.cfg["model"]:
            cmd += ["--model", self.cfg["model"]]
        runner = os.path.join(os.path.dirname(os.path.abspath(__file__)), "run_agent.sh")
        env = dict(os.environ, QUEUE_TASK_ID=tid, QUEUE_BRANCH=branch, QUEUE_WIP_REF=wip_ref,
                   QUEUE_RUN_DIR=run_dir, QUEUE_SLOT=slot)
        for k in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR"):
            env.pop(k, None)
        with open(os.path.join(run_dir, "command.json"), "w", encoding="utf-8") as f:
            json.dump([self.cfg["cswap"], "run", slot, "--", *cmd], f, indent=1)
        p = subprocess.Popen(["bash", runner, run_dir, wt, self.cfg["cswap"], "run", slot, "--", *cmd],
                             cwd=wt, env=env, stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
                             stderr=subprocess.DEVNULL, start_new_session=True)
        t.update(status="running", pid=p.pid, slot=slot, worktree=wt, branch=branch, wip_ref=wip_ref,
                 run_dir=run_dir, session=session, agent=f"claude:{session}", started=utc(),
                 started_at=t_now, areas=task["areas"], tier=task["tier"], title=task.get("title", ""))
        t.pop("last_push", None)
        t.setdefault("attempts", 0)
        self.state["slots"].setdefault(slot, {})["last_start"] = t_now
        self.state["runs"].append({"task": tid, "slot": slot, "start": t_now, "run_dir": run_dir})
        self.event("claimed", tid, slot=slot, worktree=wt, branch=branch, agent=t["agent"], pid=p.pid,
                   areas=task["areas"])

    def dispatch(self, queue, t_now):
        by_id = {t["id"]: t for t in queue["tasks"]}
        active = {tid: t for tid, t in self.state["tasks"].items() if t.get("status") in ACTIVE}
        running = sum(1 for t in active.values() if t["status"] == "running")
        held = [set(t.get("areas", [])) for t in active.values()]
        cycles = sorted(t["id"] for t in queue["tasks"] if t.get("cycle"))
        self.flag("cycle", bool(cycles), "dependency_cycle", tasks=cycles)
        started = []
        for task in queue["tasks"]:
            if running >= self.cfg["max_agents"]:
                break
            st = self.state["tasks"].get(task["id"], {}).get("status")
            if task.get("disposition") or task.get("cycle") or st in ("running", "exited", "done", "abandoned"):
                continue
            if not self.deps_ok(task, by_id):
                continue
            areas = set(task["areas"])
            if any(self.conflicts(areas, h) for h in held):
                continue
            why = self.budget_block(t_now)
            self.flag("budget", bool(why), "budget_exhausted", reason=why)
            if why:
                break
            slot = self.pick_slot(t_now)
            self.flag("no_slot", slot is None, "no_slot_available")
            if slot is None:
                break
            self.start(task, slot, t_now)
            started.append(task["id"])
            held.append(areas)
            running += 1
        return started

    def write_status(self, queue, t_now, paused):
        tasks = self.state["tasks"]
        head = [{"id": t["id"], "tier": t["tier"], "title": t.get("title", ""), "areas": t["areas"],
                 "blocked": ("disposition:" + t["disposition"]) if t.get("disposition") else
                 ("cycle" if t.get("cycle") else ("" if self.deps_ok(t, {x["id"]: x for x in queue["tasks"]})
                                                  else "dependencies"))}
                for t in queue["tasks"]
                if tasks.get(t["id"], {}).get("status") not in ("running", "exited", "done", "abandoned")][:10]
        st = {
            "generated": utc(t_now), "paused": paused,
            "running": [{"id": k, **{f: v.get(f) for f in ("status", "slot", "branch", "wip_ref", "started",
                                                          "last_sha", "areas", "tier", "title")},
                         "last_push": utc(v["last_push"]) if v.get("last_push") else None}
                        for k, v in tasks.items() if v.get("status") in ACTIVE],
            "queue_head": head,
            "budget": {**self.usage_today(t_now),
                       "running": sum(1 for v in tasks.values() if v.get("status") == "running"),
                       "max_agents": self.cfg["max_agents"], "max_agent_hours": self.cfg["max_agent_hours"],
                       "max_daily_usd": self.cfg["max_daily_usd"], "max_daily_tokens": self.cfg["max_daily_tokens"]},
            "slots": {s: {"cooling_until": utc(v["limited_at"] + self.cfg["slot_cooldown"])}
                      for s, v in self.state["slots"].items()
                      if t_now - v.get("limited_at", -1e18) < self.cfg["slot_cooldown"]},
            "counts": {s: sum(1 for v in tasks.values() if v.get("status") == s)
                       for s in ("running", "exited", "done", "pending", "abandoned")},
        }
        data = json.dumps(st, indent=1, sort_keys=True) + "\n"
        for p in filter(None, [os.path.join(self.home, "status.json"), self.cfg["status_json"]]):
            with open(p + ".tmp", "w", encoding="utf-8") as f:
                f.write(data)
            os.replace(p + ".tmp", p)

    def tick(self):
        t_now = now()
        self.refresh_sources()
        queue = build_queue.build(self.source_root(), self.cfg["fuzz_dir"])
        with open(os.path.join(self.home, "queue.json.tmp"), "w", encoding="utf-8") as f:
            json.dump(queue, f, indent=1, sort_keys=True)
        os.replace(os.path.join(self.home, "queue.json.tmp"), os.path.join(self.home, "queue.json"))
        self.monitor(t_now)
        pause = ""
        if os.path.exists(self.cfg["pause_file"]):
            try:
                pause = open(self.cfg["pause_file"], encoding="utf-8").read().strip().split("\n")[0] or "pause"
            except OSError:
                pause = "pause"
        self.flag("paused", bool(pause), "paused", mode=pause)
        if pause == "kill":
            for tid, t in self.state["tasks"].items():
                if t.get("status") in ACTIVE:
                    if t["status"] == "running":
                        self.close_run(t, t_now, None, False, 0.0, 0)
                    self.release(tid, t, "kill switch", charge=False)
        started = []
        if not pause:
            guard = self.guard_ok()
            self.flag("no_guard", not guard, "push_guard_missing", repo=self.cfg["repo"])
            if guard:
                started = self.dispatch(queue, t_now)
        self.write_status(queue, t_now, bool(pause))
        self.save()
        return started


def main():
    sub = sys.argv[1] if len(sys.argv) > 1 else ""
    q = Q(CONFIG)
    lock = open(os.path.join(q.home, "lock"), "w")
    try:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        print("dispatch: another tick holds the lock; nothing to do")
        return 0
    if sub == "tick":
        started = q.tick()
        print("dispatch: started " + (", ".join(started) if started else "nothing"))
    elif sub == "status":
        p = os.path.join(q.home, "status.json")
        print(open(p, encoding="utf-8").read() if os.path.exists(p) else "{}")
    elif sub == "release" and len(sys.argv) == 3:
        t = q.state["tasks"].get(sys.argv[2])
        if not t:
            sys.exit(f"dispatch: no task {sys.argv[2]} in state")
        if t.get("status") == "running":
            q.close_run(t, now(), None, False, 0.0, 0)
        q.release(sys.argv[2], t, "released by hand", charge=False)
        q.save()
    else:
        print(__doc__.split("\n\n")[1])
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
