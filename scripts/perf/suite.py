#!/usr/bin/env python3
"""Run the versioned compiler benchmark suite against one checkout.

  suite.py run --repo DIR --machine ID --out record.json [--only SECTION[,..]]
               [--bench NAME] [--repeats N] [--no-lake]
  suite.py quiet [--wait SECONDS]          exit 0 once the machine is quiet
  suite.py discover-gates --repo DIR --out slow_gates.json [--top 10]
  suite.py gen FAMILY N                     print one synthetic program

SECTIONS: lake, compile, synthetic, tests, gates. The suite definition is
suite.json next to this file; its suite_version is stamped into every record.
The checkout must already be built (`lake build`) unless the lake section runs.

Peak memory comes from /usr/bin/time (-v on Linux, -l on macOS); wall time from
a monotonic clock around the child. Compile benchmarks repeat `repeats` times
and keep every sample; the record stores the median and the range.

Contamination: while the suite runs a sampler watches for foreign heavy work
(a GitHub Actions job, a gate runner or test suite outside this process tree,
or many foreign runnable non-idle tasks). Any hit marks the record
`contaminated`, and detection never compares against or alarms on such a record.
"""

import argparse
import datetime
import json
import math
import os
import platform
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import perflib  # noqa: E402

SUITE = os.path.join(HERE, "suite.json")
IS_LINUX = platform.system() == "Linux"


# ------------------------------------------------------------------ helpers

def log(msg):
    sys.stderr.write("[perf %s] %s\n" % (time.strftime("%H:%M:%S"), msg))
    sys.stderr.flush()


def git(repo, *args):
    return subprocess.run(["git", "-C", repo] + list(args), capture_output=True,
                          text=True, check=True).stdout.strip()


def harness_sha():
    try:
        return git(HERE, "rev-parse", "HEAD")
    except Exception:
        return "unknown"


TIME_BIN = "/usr/bin/time" if os.path.exists("/usr/bin/time") else None


def timed(cmd, cwd, env=None, timeout=None, stdout=None):
    """Run cmd; return (wall_s, peak_rss_kb or None, returncode)."""
    with tempfile.NamedTemporaryFile("r", suffix=".time", delete=False) as tf:
        tpath = tf.name
    try:
        if TIME_BIN and IS_LINUX:
            full = [TIME_BIN, "-v", "-o", tpath] + cmd
        elif TIME_BIN:
            full = [TIME_BIN, "-l"] + cmd
        else:
            full = cmd
        err = open(tpath, "a") if (TIME_BIN and not IS_LINUX) else subprocess.DEVNULL
        t0 = time.monotonic()
        try:
            p = subprocess.run(full, cwd=cwd, env=env, stdout=stdout or subprocess.DEVNULL,
                               stderr=err, timeout=timeout)
            rc = p.returncode
        except subprocess.TimeoutExpired:
            rc = 124
        wall = time.monotonic() - t0
        if err is not subprocess.DEVNULL:
            err.close()
        rss = None
        try:
            text = open(tpath, errors="replace").read()
        except OSError:
            text = ""
        m = re.search(r"Maximum resident set size \(kbytes\): (\d+)", text)
        if m:
            rss = int(m.group(1))
        else:
            m = re.search(r"(\d+)\s+maximum resident set size", text)
            if m:
                rss = int(m.group(1)) // 1024   # macOS reports bytes
        return wall, rss, rc
    finally:
        try:
            os.unlink(tpath)
        except OSError:
            pass


# ------------------------------------------------------------------ synthetic programs

def gen(family, n):
    """Synthetic stress programs. Shapes extend check_compiler_complexity.sh's
    families (functions/array/match/statements) with deep nesting, a long
    expression chain and many struct types."""
    n = int(n)
    if family == "functions":
        fns = "".join("fn f%d(x: i32) -> i32 { return f%d(x) + 1; }\n" % (i, i + 1) for i in range(n))
        fns += "fn f%d(x: i32) -> i32 { return x; }\n" % n
        return "mod m {\n" + fns + "fn main() -> Int { return f0(1) as Int; }\n}\n"
    if family == "array":
        elems = ", ".join("1" for _ in range(n))
        return ("mod m { fn main() -> Int { let a: [i32; %d] = [%s]; return a[0] as Int; } }\n" % (n, elems))
    if family == "match":
        arms = "".join("%d => %d, " % (i, i) for i in range(n))
        return ("mod m { fn main() -> Int { let x: Int = 0; let r: Int = match x { %s _ => -1 }; return r; } }\n" % arms)
    if family == "statements":
        stmts = "".join("acc = acc + %d;\n" % (i % 7) for i in range(n))
        return "mod m { fn main() -> Int { let mut acc: Int = 0;\n" + stmts + "return acc; } }\n"
    if family == "nesting":
        body = "acc = acc + 1;\n"
        for i in range(n):
            body = "if acc < %d {\nacc = acc + 1;\n%s}\n" % (i + 1000000, body)
        return "mod m { fn main() -> Int { let mut acc: Int = 0;\n" + body + "return acc; } }\n"
    if family == "exprchain":
        expr = " + ".join("(x * %d)" % (i % 5 + 1) for i in range(n))
        return "mod m { fn main() -> Int { let x: Int = 1; let r: Int = %s; return r; } }\n" % expr
    if family == "structs":
        out = []
        for i in range(n):
            out.append("struct S%d { a: i32, b: i64 }\n" % i)
            out.append("fn mk%d(v: i32) -> S%d { return S%d { a: v, b: %d }; }\n" % (i, i, i, i))
        uses = "".join("acc = acc + mk%d(%d).a;\n" % (i, i % 3) for i in range(n))
        return ("mod m {\n" + "".join(out) + "fn main() -> Int { let mut acc: i32 = 0;\n"
                + uses + "return acc as Int; } }\n")
    raise ValueError("unknown family %r" % family)


# ------------------------------------------------------------------ contamination sampler

FOREIGN_HEAVY = re.compile(r"Runner\.Worker|run_ci_gates_local\.sh|run_tests\.sh|test_mutation\.sh"
                           r"|lake build|concrete-remote")


def _proc_table():
    out = subprocess.run(["ps", "-eo", "pid=,ppid=,ni=,stat=,args="], capture_output=True,
                         text=True).stdout
    rows = []
    for line in out.splitlines():
        parts = line.split(None, 4)
        if len(parts) < 4:
            continue
        pid, ppid, ni, stat = parts[:4]
        args = parts[4] if len(parts) > 4 else ""
        try:
            rows.append((int(pid), int(ppid), ni, stat, args))
        except ValueError:
            continue
    return rows


def _ours(rows, root):
    kids = {}
    for pid, ppid, *_ in rows:
        kids.setdefault(ppid, []).append(pid)
    seen, stack = set(), [root]
    while stack:
        p = stack.pop()
        if p in seen:
            continue
        seen.add(p)
        stack.extend(kids.get(p, []))
    # Our ancestors (the runner shell, systemd) are not foreign either.
    parent = {pid: ppid for pid, ppid, *_ in rows}
    p = root
    while p in parent and p not in (0, 1):
        p = parent[p]
        seen.add(p)
    return seen


def snapshot(root_pid):
    rows = _proc_table()
    ours = _ours(rows, root_pid)
    heavy, running, idle_running = set(), 0, 0
    for pid, ppid, ni, stat, args in rows:
        if pid in ours:
            continue
        m = FOREIGN_HEAVY.search(args)
        if m and "grep" not in args:
            heavy.add(m.group(0))
        if stat.startswith("R"):
            try:
                nice = int(ni)
            except ValueError:
                nice = 0   # "-" = realtime / SCHED_IDLE reports vary; count as normal
            if nice >= 19 or ni == "-":
                idle_running += 1
            else:
                running += 1
    try:
        load1 = os.getloadavg()[0]
    except OSError:
        load1 = None
    return {"heavy": sorted(heavy), "running": running, "idle_running": idle_running, "load1": load1}


class Sampler(threading.Thread):
    def __init__(self, period=5.0, running_limit=None):
        super().__init__(daemon=True)
        self.period = period
        self.stop = threading.Event()
        self.samples = []
        ncpu = os.cpu_count() or 4
        self.running_limit = running_limit if running_limit is not None else max(4, ncpu // 8)

    def run(self):
        while not self.stop.wait(self.period):
            try:
                self.samples.append(snapshot(os.getpid()))
            except Exception:
                pass

    def verdict(self):
        heavy = sorted({h for s in self.samples for h in s["heavy"]})
        run = [s["running"] for s in self.samples]
        idle = [s["idle_running"] for s in self.samples]
        loads = [s["load1"] for s in self.samples if s["load1"] is not None]
        mean_run = sum(run) / len(run) if run else 0.0
        contaminated = bool(heavy) or mean_run > self.running_limit
        return contaminated, {
            "samples": len(self.samples), "foreign_heavy": heavy,
            "foreign_running_mean": round(mean_run, 2), "foreign_running_max": max(run) if run else 0,
            "idle_running_mean": round(sum(idle) / len(idle), 2) if idle else 0.0,
            "load1_max": max(loads) if loads else None, "running_limit": self.running_limit,
        }


def wait_quiet(wait_s, settle=3, period=20):
    """Return True once `settle` consecutive snapshots show no foreign heavy work."""
    deadline = time.monotonic() + wait_s
    ncpu = os.cpu_count() or 4
    limit = max(4, ncpu // 8)
    ok = 0
    while True:
        s = snapshot(os.getpid())
        quiet = not s["heavy"] and s["running"] <= limit
        ok = ok + 1 if quiet else 0
        if ok >= settle:
            return True
        if time.monotonic() > deadline:
            log("machine never went quiet: %s" % json.dumps(s))
            return False
        if not quiet:
            log("busy: %s" % json.dumps(s))
        time.sleep(period if not quiet else 5)


# ------------------------------------------------------------------ measurement sections

class Run:
    def __init__(self, repo, suite, repeats, bench=None):
        self.repo = os.path.abspath(repo)
        self.suite = suite
        self.repeats = repeats
        self.bench = bench
        self.metrics = {}
        self.notes = []
        self.failed = []
        self.cc = os.path.join(self.repo, ".lake", "build", "bin", "concrete")
        self.tmp = tempfile.mkdtemp(prefix="concrete-perf-")

    def want(self, name):
        return self.bench is None or self.bench == name

    def put(self, name, samples, unit):
        e = perflib.summarize(samples, unit)
        if e is not None:
            self.metrics[name] = e

    def lake(self):
        cfg = self.suite["lake"]
        if cfg.get("cold") and self.want("cold"):
            shutil.rmtree(os.path.join(self.repo, ".lake", "build"), ignore_errors=True)
            log("lake build (cold)")
            w, rss, rc = timed(["lake", "build"], self.repo)
            if rc != 0:
                self.failed.append("lake.cold")
                return False
            self.put("lake.cold.wall_s", [w], "s")
            if rss:
                self.put("lake.cold.rss_kb", [rss], "kb")
        else:
            w, rss, rc = timed(["lake", "build"], self.repo)
            if rc != 0:
                self.failed.append("lake.build")
                return False
        for rel in cfg.get("incremental_touch", []):
            bench = "incr_" + re.sub(r"[^A-Za-z0-9]+", "_", os.path.splitext(os.path.basename(rel))[0]).lower()
            if not self.want(bench):
                continue
            path = os.path.join(self.repo, rel)
            if not os.path.exists(path):
                self.notes.append("incremental touch target %s absent" % rel)
                continue
            orig = open(path, "rb").read()
            try:
                with open(path, "ab") as f:
                    f.write(b"\n-- perf incremental probe\n")
                log("lake build (incremental, %s edited)" % rel)
                w, rss, rc = timed(["lake", "build"], self.repo)
            finally:
                with open(path, "wb") as f:
                    f.write(orig)
            if rc == 0:
                self.put("lake.%s.wall_s" % bench, [w], "s")
            else:
                self.failed.append("lake." + bench)
            timed(["lake", "build"], self.repo)   # back to the committed tree's artifacts
        return True

    def _trace(self, args, cwd):
        p = subprocess.run([self.cc] + args + ["--emit-trace-json"], cwd=cwd, capture_output=True, text=True)
        if p.returncode != 0:
            return None
        try:
            return json.loads(p.stdout)
        except ValueError:
            return None

    def compile(self):
        n = self.repeats
        for proj in self.suite["projects"]:
            bench = os.path.basename(proj.rstrip("/"))
            if not self.want(bench):
                continue
            d = os.path.join(self.repo, proj)
            if not os.path.isdir(d):
                self.notes.append("project %s absent" % proj)
                continue
            out = os.path.join(self.tmp, bench + ".bin")
            walls, rsss, cwalls = [], [], []
            for _ in range(n):
                w, rss, rc = timed([self.cc, "build", "-o", out], d)
                if rc != 0:
                    self.failed.append("compile." + bench)
                    break
                walls.append(w * 1000)
                rsss.append(rss)
                w2, _, rc2 = timed([self.cc, "check"], d)
                if rc2 == 0:
                    cwalls.append(w2 * 1000)
            log("compile %-28s %s ms" % (bench, perflib.median(walls)))
            self.put("compile.%s.wall_ms" % bench, walls, "ms")
            self.put("compile.%s.rss_kb" % bench, [r for r in rsss if r], "kb")
            self.put("check.%s.wall_ms" % bench, cwalls, "ms")
        for rel in self.suite["single_files"]:
            bench = (os.path.basename(os.path.dirname(os.path.dirname(rel)))
                     if rel.endswith("/src/main.con") else os.path.splitext(os.path.basename(rel))[0])
            if not self.want(bench):
                continue
            path = os.path.join(self.repo, rel)
            if not os.path.exists(path):
                self.notes.append("program %s absent" % rel)
                continue
            self._single(bench, path, n)

    def _single(self, bench, path, n, group="compile"):
        out = os.path.join(self.tmp, bench + ".bin")
        walls, rsss, passes = [], [], {}
        for _ in range(n):
            w, rss, rc = timed([self.cc, path, "-o", out], self.tmp)
            if rc != 0:
                self.failed.append("%s.%s" % (group, bench))
                return None
            walls.append(w * 1000)
            rsss.append(rss)
            tr = self._trace([path], self.tmp)
            if tr:
                for k, v in tr.get("timing_ms", {}).items():
                    passes.setdefault(k, []).append(v)
        log("%s %-28s %s ms" % (group, bench, perflib.median(walls)))
        self.put("%s.%s.wall_ms" % (group, bench), walls, "ms")
        self.put("%s.%s.rss_kb" % (group, bench), [r for r in rsss if r], "kb")
        for k, vs in passes.items():
            q = re.sub(r"[^a-z0-9]+", "_", k.lower()).strip("_")
            self.put("pass.%s.%s_ms" % (bench, q), vs, "ms")
        return {"wall": perflib.median(walls),
                "passes": {k: perflib.median(v) for k, v in passes.items()},
                "total": perflib.median([sum(x) for x in zip(*passes.values())]) if passes else None}

    def synthetic(self):
        g = self.suite.get("growth_factor", 4)
        for spec in self.suite["synthetic"]:
            fam, n = spec["family"], spec["n"]
            bench = "syn_" + fam
            if not self.want(bench):
                continue
            big = os.path.join(self.tmp, "%s_%d.con" % (fam, n))
            small_n = max(1, n // g)
            small = os.path.join(self.tmp, "%s_%d.con" % (fam, small_n))
            open(big, "w").write(gen(fam, n))
            open(small, "w").write(gen(fam, small_n))
            rb = self._single(bench, big, self.repeats, group="compile")
            if rb is None:
                continue
            rs = self._single(bench + "_small", small, self.repeats, group="compile")
            # The small size is only an input to the growth exponent; drop its own metrics.
            for k in [k for k in self.metrics if (".%s_small." % bench) in k or k.startswith("pass.%s_small." % bench)]:
                del self.metrics[k]
            if rs is None:
                continue
            ratio = float(n) / small_n

            def exp(a, b):
                return math.log(max(a, 1e-9) / max(b, 1e-9)) / math.log(ratio)
            if rb["total"] and rs["total"] and rb["total"] >= 20:
                self.put("growth.%s.frontend_exp" % bench, [exp(rb["total"], rs["total"])], "exp")
            self.put("growth.%s.wall_exp" % bench, [exp(rb["wall"], rs["wall"])], "exp")
            worst = None
            for k, vb in rb["passes"].items():
                vs = rs["passes"].get(k)
                if vb is None or vs is None or vb < 50:
                    continue   # too short to read an exponent from
                e = exp(vb, max(vs, 1))
                if worst is None or e > worst[1]:
                    worst = (k, e)
            if worst:
                self.put("growth.%s.worst_pass_exp" % bench, [worst[1]], "exp")
                self.notes.append("growth %s worst pass %s exp %.2f" % (bench, worst[0], worst[1]))

    def tests(self):
        if not self.want("run_tests"):
            return
        log("run_tests.sh")
        w, rss, rc = timed(["bash", "-c", self.suite["run_tests"]], self.repo, timeout=3 * 3600)
        self.put("tests.run_tests.wall_s", [w], "s")
        if rc != 0:
            self.notes.append("run_tests.sh exited %d (time still recorded)" % rc)

    def gates(self):
        path = os.path.join(HERE, os.path.basename(self.suite["slow_gates_file"]))
        if not os.path.exists(path):
            self.notes.append("no slow_gates.json yet; run discover-gates")
            return
        for g in json.load(open(path))["gates"]:
            if not self.want(g["id"]):
                continue
            if not os.path.exists(os.path.join(self.repo, g["script"])):
                self.notes.append("gate %s absent at this commit" % g["id"])
                continue
            log("gate %s" % g["id"])
            w, rss, rc = timed(["bash", "-c", g["cmd"]], self.repo, timeout=2 * 3600)
            self.put("gate.%s.wall_s" % g["id"], [w], "s")
            if rc != 0:
                self.notes.append("gate %s exited %d (time still recorded)" % (g["id"], rc))


def host_info():
    info = {"system": platform.system(), "release": platform.release(), "nproc": os.cpu_count(),
            "python": platform.python_version()}
    try:
        for line in open("/proc/cpuinfo"):
            if line.startswith("model name"):
                info["cpu"] = line.split(":", 1)[1].strip()
                break
    except OSError:
        info["cpu"] = platform.processor()
    try:
        info["load_before"] = round(os.getloadavg()[0], 2)
    except OSError:
        pass
    return info


def cmd_run(a):
    suite = json.load(open(SUITE))
    repeats = a.repeats or suite["repeats"]
    sections = a.only.split(",") if a.only else ["lake", "compile", "synthetic", "tests", "gates"]
    if a.no_lake and "lake" in sections:
        sections.remove("lake")
    repo = os.path.abspath(a.repo)
    sha = git(repo, "rev-parse", "HEAD")
    r = Run(repo, suite, repeats, a.bench)
    sampler = Sampler()
    sampler.start()
    status = "ok"
    t0 = time.monotonic()
    try:
        if "lake" in sections and not r.lake():
            status = "build_failed"
        if status == "ok" and not os.path.exists(r.cc):
            status = "build_failed"
            r.notes.append("compiler binary missing; build the checkout first")
        if status == "ok":
            for s in sections:
                if s != "lake":
                    getattr(r, s)()
    finally:
        sampler.stop.set()
        sampler.join(timeout=10)
        shutil.rmtree(r.tmp, ignore_errors=True)
    if status == "ok" and r.failed:
        status = "partial"
    contaminated, foreign = sampler.verdict()
    rec = {
        "schema": perflib.RECORD_SCHEMA, "suite_version": suite["suite_version"],
        "harness": harness_sha(), "machine": a.machine, "sha": sha,
        "subject": git(repo, "log", "-1", "--format=%s"),
        "commit_time": git(repo, "log", "-1", "--format=%cI"),
        "measured_at": datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "status": status, "contaminated": contaminated,
        "host": dict(host_info(), foreign=foreign, suite_wall_s=round(time.monotonic() - t0, 1)),
        "sections": sections, "bench": a.bench, "repeats": repeats,
        "failed": r.failed, "notes": r.notes, "metrics": r.metrics,
    }
    text = perflib.dump_record(rec)
    with open(a.out, "w") as f:
        f.write(text + "\n")
    log("record written: %s (%d metrics, status %s, contaminated %s)" % (a.out, len(r.metrics), status, contaminated))
    return 0 if status != "build_failed" else 3


def cmd_quiet(a):
    return 0 if wait_quiet(a.wait) else 75


def cmd_discover(a):
    repo = os.path.abspath(a.repo)
    plan = subprocess.run(["bash", "scripts/tests/run_ci_gates_local.sh", "--plan"], cwd=repo,
                          capture_output=True, text=True)
    cmds = [l.strip() for l in plan.stdout.splitlines() if re.search(r"scripts/\S+\.(sh|py)", l)]
    if not cmds:
        log("run_ci_gates_local.sh --plan listed no commands:\n" + plan.stderr[-2000:])
        return 2
    timings = []
    for c in cmds:
        w, _, rc = timed(["bash", "-c", c], repo, timeout=2 * 3600)
        log("%7.1fs rc=%d %s" % (w, rc, c))
        timings.append({"cmd": c, "wall_s": round(w, 1), "rc": rc})
    timings.sort(key=lambda t: -t["wall_s"])
    top = []
    for t in timings:
        if t["rc"] != 0:
            continue   # a red gate's time says nothing about its normal cost
        script = re.search(r"(scripts/\S+\.(?:sh|py))", t["cmd"]).group(1)
        gid = re.sub(r"[^A-Za-z0-9_]+", "_", os.path.splitext(os.path.basename(script))[0])
        if any(g["id"] == gid for g in top):
            gid += "_%d" % len(top)
        top.append({"id": gid, "cmd": t["cmd"], "script": script, "discovered_wall_s": t["wall_s"]})
        if len(top) == a.top:
            break
    out = {"comment": "The slowest passing CI gates, measured once on the perf machine by "
                      "`suite.py discover-gates`. Editing this list is a suite change: bump "
                      "suite_version in suite.json.",
           "discovered_on": git(repo, "rev-parse", "HEAD"), "machine": a.machine,
           "gates": top, "all": timings}
    with open(a.out, "w") as f:
        json.dump(out, f, indent=1)
        f.write("\n")
    return 0


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    r = sub.add_parser("run")
    r.add_argument("--repo", required=True)
    r.add_argument("--machine", default=os.environ.get("PERF_MACHINE") or platform.node().split(".")[0])
    r.add_argument("--out", required=True)
    r.add_argument("--only")
    r.add_argument("--bench")
    r.add_argument("--repeats", type=int)
    r.add_argument("--no-lake", action="store_true")
    q = sub.add_parser("quiet")
    q.add_argument("--wait", type=int, default=0)
    d = sub.add_parser("discover-gates")
    d.add_argument("--repo", required=True)
    d.add_argument("--out", required=True)
    d.add_argument("--top", type=int, default=10)
    d.add_argument("--machine", default=os.environ.get("PERF_MACHINE") or platform.node().split(".")[0])
    g = sub.add_parser("gen")
    g.add_argument("family")
    g.add_argument("n", type=int)
    a = ap.parse_args()
    if a.cmd == "run":
        return cmd_run(a)
    if a.cmd == "quiet":
        return cmd_quiet(a)
    if a.cmd == "discover-gates":
        return cmd_discover(a)
    sys.stdout.write(gen(a.family, a.n))
    return 0


if __name__ == "__main__":
    sys.exit(main())
