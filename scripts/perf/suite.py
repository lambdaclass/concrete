#!/usr/bin/env python3
"""Run the versioned compiler benchmark suite against one checkout.

  suite.py run --repo DIR --machine ID --out record.json [--only SECTION[,..]]
               [--bench NAME] [--repeats N] [--no-lake]
  suite.py counter                          print the counter this host measures with
  suite.py discover-gates --repo DIR --out slow_gates.json [--top 10] [--shard K/N]
  suite.py merge-gates --out slow_gates.json SHARD.json...
  suite.py gen FAMILY N                     print one synthetic program

SECTIONS: lake, compile, synthetic, tests, gates. The suite definition is
suite.json next to this file; its suite_version is stamped into every record.
The checkout must already be built (`lake build`) unless the lake section runs.

WHAT IS MEASURED. No machine is ever quiet (CI runners, the fuzz farm and agents share the
servers), so the primary metric is RETIRED USER-SPACE INSTRUCTIONS, counted by
`perf stat -e instructions:u -x,` over the whole process tree. Instruction counts do not depend
on load, frequency or scheduling, so they are comparable across runs on a busy machine. Every
command is also run under /usr/bin/time (-v on Linux, -l on macOS) for peak RSS, the memory
metric, which is mostly load-independent. Wall time is recorded as an informational column only
and is never alarmed on.

  <group>.<bench>.instr    instructions:u        (alarmed)
  <group>.<bench>.rss_kb   peak RSS              (alarmed)
  <group>.<bench>.wall_ms / wall_s               (information only)
  pass.<bench>.<pass>_ms   per-pass time from --emit-trace-json (information only; the compiler
                           exposes no per-pass instruction count)
  growth.<bench>.instr_exp           growth exponent of the front end (--emit-trace-json run)
  growth.<bench>.compile_instr_exp   growth exponent of the full compile, both from instructions
  growth.<bench>.worst_pass_ms_exp   per-pass exponent from times (information only)

FALLBACK. Where `perf` cannot count instructions:u (macOS; Linux with a restrictive
kernel.perf_event_paranoid), the record says counter "wall" and comparable false: its wall-time
numbers are kept for reading but never become a baseline and never raise an alarm.
PERF_COUNTER=wall forces the fallback.
"""

import argparse
import datetime
import json
import math
import os
import platform
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import perflib  # noqa: E402

SUITE = os.path.join(HERE, "suite.json")
IS_LINUX = platform.system() == "Linux"
EVENT = "instructions:u"


# ------------------------------------------------------------------ helpers

def log(msg):
    sys.stderr.write("[perf %s] %s\n" % (time.strftime("%H:%M:%S"), msg))
    sys.stderr.flush()


def git(repo, *args):
    return subprocess.run(["git", "-C", repo] + list(args), capture_output=True,
                          text=True, check=True).stdout.strip()


def harness_sha():
    try:
        # The harness may be root-owned; name it as safe so git does not refuse to read it.
        return subprocess.run(["git", "-c", "safe.directory=*", "-C", HERE, "rev-parse", "HEAD"],
                              capture_output=True, text=True, check=True).stdout.strip()
    except Exception:
        return "unknown"


TIME_BIN = "/usr/bin/time" if os.path.exists("/usr/bin/time") else None


def parse_perf_csv(text, event=EVENT):
    """Instruction count from `perf stat -x,` output, or None when the event was not counted
    (<not supported>, <not counted>, a paranoid refusal, or no line at all)."""
    for line in text.splitlines():
        parts = line.split(",")
        if len(parts) >= 3 and parts[2].strip() == event:
            v = parts[0].strip()
            if re.match(r"^\d+$", v):
                return int(v)
            return None
    return None


def parse_rss(text):
    m = re.search(r"Maximum resident set size \(kbytes\): (\d+)", text)
    if m:
        return int(m.group(1))
    m = re.search(r"(\d+)\s+maximum resident set size", text)
    if m:
        return int(m.group(1)) // 1024   # macOS reports bytes
    return None


_COUNTER = None


def counter():
    """'instructions:u' when perf can count it for this user, else 'wall'."""
    global _COUNTER
    if _COUNTER is None:
        _COUNTER = "wall"
        if os.environ.get("PERF_COUNTER", "") != "wall" and shutil.which("perf"):
            try:
                p = subprocess.run(["perf", "stat", "-e", EVENT, "-x,", "true"], capture_output=True,
                                   text=True, timeout=30)
                if p.returncode == 0 and parse_perf_csv(p.stderr):
                    _COUNTER = EVENT
            except (OSError, subprocess.TimeoutExpired):
                pass
    return _COUNTER


def timed(cmd, cwd, env=None, timeout=None, stdout=None):
    """Run cmd; return (wall_s, peak_rss_kb or None, returncode, instructions or None).
    Instructions are counted over the whole process tree (perf stat inherits)."""
    tdir = tempfile.mkdtemp(prefix="perf-timed-")
    tpath = os.path.join(tdir, "time")
    ppath = os.path.join(tdir, "perf")
    try:
        full = list(cmd)
        if TIME_BIN and IS_LINUX:
            full = [TIME_BIN, "-v", "-o", tpath] + full
        elif TIME_BIN:
            full = [TIME_BIN, "-l"] + full
        if counter() == EVENT:
            full = ["perf", "stat", "-e", EVENT, "-x,", "-o", ppath, "--"] + full
        err = open(tpath, "a") if (TIME_BIN and not IS_LINUX) else subprocess.DEVNULL
        t0 = time.monotonic()
        # Own process group, so a timeout kills the benchmark's whole tree.
        p = subprocess.Popen(full, cwd=cwd, env=env, stdout=stdout or subprocess.DEVNULL,
                             stderr=err, start_new_session=True)
        try:
            rc = p.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            # TERM the group, a grace period, then KILL the group unconditionally: the leader
            # exiting says nothing about a worker that ignored TERM.
            try:
                os.killpg(p.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            try:
                p.wait(timeout=10)
            except subprocess.TimeoutExpired:
                pass
            try:
                os.killpg(p.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            p.wait()
            rc = 124
        wall = time.monotonic() - t0
        if err is not subprocess.DEVNULL:
            err.close()

        def rd(path):
            try:
                return open(path, errors="replace").read()
            except OSError:
                return ""
        rss = parse_rss(rd(tpath))
        instr = parse_perf_csv(rd(ppath)) if counter() == EVENT else None
        return wall, rss, rc, instr
    finally:
        shutil.rmtree(tdir, ignore_errors=True)


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
        e = perflib.summarize([s for s in samples if s is not None], unit)
        if e is not None:
            self.metrics[name] = e

    def put_instr(self, name, values):
        """Instruction samples. When this host counts instructions, every sample must have a count:
        a run whose counter failed (<not counted>, unparseable output) drops the whole metric and
        marks the record partial, instead of leaving a baseline built from the samples that
        happened to count."""
        if counter() == EVENT and (not values or any(v is None for v in values)):
            if values:
                self.failed.append("instr:" + name)
                self.notes.append("%s: %d of %d runs returned no instruction count" % (
                    name, sum(1 for v in values if v is None), len(values)))
            return
        self.put(name, values, "instr")

    def put_run(self, prefix, runs, wall_unit="ms"):
        """runs: list of (wall_s, rss_kb, instr). Instructions and RSS are the metrics; wall is
        information."""
        scale = 1000.0 if wall_unit == "ms" else 1.0
        self.put("%s.wall_%s" % (prefix, wall_unit), [w * scale for w, _, _ in runs], wall_unit)
        self.put("%s.rss_kb" % prefix, [r for _, r, _ in runs], "kb")
        self.put_instr("%s.instr" % prefix, [i for _, _, i in runs])

    def lake(self):
        cfg = self.suite["lake"]
        if cfg.get("cold") and self.want("cold"):
            shutil.rmtree(os.path.join(self.repo, ".lake", "build"), ignore_errors=True)
            log("lake build (cold)")
            w, rss, rc, ins = timed(["lake", "build"], self.repo)
            if rc != 0:
                self.failed.append("lake.cold")
                return False
            self.put_run("lake.cold", [(w, rss, ins)], "s")
        else:
            w, rss, rc, ins = timed(["lake", "build"], self.repo)
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
                w, rss, rc, ins = timed(["lake", "build"], self.repo)
            finally:
                with open(path, "wb") as f:
                    f.write(orig)
            if rc == 0:
                self.put_run("lake.%s" % bench, [(w, rss, ins)], "s")
            else:
                self.failed.append("lake." + bench)
            timed(["lake", "build"], self.repo)   # back to the committed tree's artifacts
        return True

    def _trace(self, path):
        """One --emit-trace-json run: (instructions of the front end + lowering, per-pass ms)."""
        out = os.path.join(self.tmp, "trace.json")
        with open(out, "w") as f:
            w, rss, rc, ins = timed([self.cc, path, "--emit-trace-json"], self.tmp, stdout=f)
        if rc != 0:
            return None, {}
        try:
            tr = json.load(open(out))
        except ValueError:
            return ins, {}
        return ins, tr.get("timing_ms", {})

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
            runs, checks = [], []
            for _ in range(n):
                w, rss, rc, ins = timed([self.cc, "build", "-o", out], d)
                if rc != 0:
                    self.failed.append("compile." + bench)
                    break
                runs.append((w, rss, ins))
                w2, rss2, rc2, ins2 = timed([self.cc, "check"], d)
                if rc2 == 0:
                    checks.append((w2, rss2, ins2))
            if ("compile." + bench) in self.failed:
                continue   # a build that failed once is not measured at all
            log("compile %-28s %s instr" % (bench, perflib.median([i for _, _, i in runs])))
            self.put_run("compile.%s" % bench, runs)
            self.put_run("check.%s" % bench, checks)
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

    def _single(self, bench, path, n, record=True):
        out = os.path.join(self.tmp, bench + ".bin")
        runs, fe, passes = [], [], {}
        for _ in range(n):
            w, rss, rc, ins = timed([self.cc, path, "-o", out], self.tmp)
            if rc != 0:
                self.failed.append("compile.%s" % bench)
                return None
            runs.append((w, rss, ins))
            fins, tm = self._trace(path)
            fe.append(fins)
            for k, v in tm.items():
                passes.setdefault(k, []).append(v)
        log("compile %-28s %s instr" % (bench, perflib.median([i for _, _, i in runs])))
        if record:
            self.put_run("compile.%s" % bench, runs)
            self.put_instr("frontend.%s.instr" % bench, fe)
            for k, vs in passes.items():
                q = re.sub(r"[^a-z0-9]+", "_", k.lower()).strip("_")
                self.put("pass.%s.%s_ms" % (bench, q), vs, "ms")
        # Growth exponents only from complete instruction samples.
        full = counter() == EVENT and all(i is not None for _, _, i in runs) and all(x is not None for x in fe)
        return {"instr": perflib.median([i for _, _, i in runs]) if full else None,
                "frontend_instr": perflib.median(fe) if full else None,
                "wall": perflib.median([w for w, _, _ in runs]),
                "passes": {k: perflib.median(v) for k, v in passes.items()}}

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
            rb = self._single(bench, big, self.repeats)
            if rb is None:
                continue
            # The small size is only an input to the growth exponents.
            rs = self._single(bench + "_small", small, self.repeats, record=False)
            if rs is None:
                continue
            ratio = float(n) / small_n

            def exp(a, b):
                return math.log(max(a, 1e-9) / max(b, 1e-9)) / math.log(ratio)
            if rb["frontend_instr"] and rs["frontend_instr"]:
                self.put("growth.%s.instr_exp" % bench, [exp(rb["frontend_instr"], rs["frontend_instr"])], "exp")
            if rb["instr"] and rs["instr"]:
                self.put("growth.%s.compile_instr_exp" % bench, [exp(rb["instr"], rs["instr"])], "exp")
            worst = None
            for k, vb in rb["passes"].items():
                vs = rs["passes"].get(k)
                if vb is None or vs is None or vb < 50:
                    continue   # too short to read an exponent from
                e = exp(vb, max(vs, 1))
                if worst is None or e > worst[1]:
                    worst = (k, e)
            if worst:
                self.put("growth.%s.worst_pass_ms_exp" % bench, [worst[1]], "exp")
                self.notes.append("growth %s worst pass (by time) %s exp %.2f" % (bench, worst[0], worst[1]))

    def tests(self):
        if not self.want("run_tests"):
            return
        log("run_tests.sh")
        # The summary is read only if THIS run wrote it: remove any earlier one first, and pin its
        # location so a TEST_SUMMARY_FILE inherited from the environment cannot redirect it.
        summary = os.path.join(self.repo, ".test-summary")
        try:
            os.unlink(summary)
        except OSError:
            pass
        env = dict(os.environ)
        env["TEST_SUMMARY_FILE"] = summary
        t_start = time.time()
        w, rss, rc, ins = timed(["bash", "-c", self.suite["run_tests"]], self.repo, env=env, timeout=3 * 3600)
        # A red or timed-out suite is not a measurement: an early failure would read as a
        # speed-up and the fix as a slowdown. The run is marked partial instead. The one exception
        # is a run that COMPLETED (its .test-summary says completed=1) with a few failing tests:
        # it ran every test, so its cost is comparable; the failures are noted.
        done = None
        if rc not in (0, 124):
            try:
                if os.path.getmtime(summary) >= t_start:
                    summ = open(summary).read()
                    m = re.search(r"completed=1\b.*?failed=(\d+)", summ, re.S)
                    done = int(m.group(1)) if m else None
            except OSError:
                done = None
        if rc == 0 or done is not None:
            self.put_run("tests.run_tests", [(w, rss, ins)], "s")
            if done:
                self.notes.append("run_tests.sh completed with %d failing test(s); measured anyway" % done)
        else:
            self.failed.append("tests.run_tests")
            self.notes.append("run_tests.sh exited %d after %.0fs; not measured" % (rc, w))

    def gates(self):
        path = os.environ.get("PERF_SLOW_GATES") or os.path.join(HERE, os.path.basename(self.suite["slow_gates_file"]))
        if not os.path.exists(path):
            self.failed.append("gates.list")
            self.notes.append("no slow_gates.json; run discover-gates")
            return
        for g in json.load(open(path))["gates"]:
            if not self.want(g["id"]):
                continue
            if not os.path.exists(os.path.join(self.repo, g["script"])):
                self.notes.append("gate %s absent at this commit" % g["id"])
                continue
            log("gate %s" % g["id"])
            w, rss, rc, ins = timed(["bash", "-c", g["cmd"]], self.repo, timeout=2 * 3600)
            if rc == 0:
                self.put_run("gate.%s" % g["id"], [(w, rss, ins)], "s")
            else:
                self.failed.append("gate." + g["id"])
                self.notes.append("gate %s exited %d after %.0fs; not measured" % (g["id"], rc, w))


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
    ctr = counter()
    if ctr != EVENT:
        log("perf cannot count %s here; falling back to wall time (record not comparable)" % EVENT)
    r = Run(repo, suite, repeats, a.bench)
    status = "ok"
    t0 = time.monotonic()
    host = host_info()
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
        shutil.rmtree(r.tmp, ignore_errors=True)
    if status == "ok" and r.failed:
        status = "partial"
    try:
        host["load_after"] = round(os.getloadavg()[0], 2)
    except OSError:
        pass
    host["suite_wall_s"] = round(time.monotonic() - t0, 1)
    rec = {
        "schema": perflib.RECORD_SCHEMA, "suite_version": suite["suite_version"],
        "harness": harness_sha(), "machine": a.machine, "sha": sha,
        "subject": git(repo, "log", "-1", "--format=%s"),
        "commit_time": git(repo, "log", "-1", "--format=%cI"),
        "measured_at": datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "status": status, "counter": ctr, "comparable": ctr == EVENT, "contaminated": False,
        "host": host, "sections": sections, "bench": a.bench, "repeats": repeats,
        "failed": r.failed, "notes": r.notes, "metrics": r.metrics,
    }
    text = perflib.dump_record(rec)
    with open(a.out, "w") as f:
        f.write(text + "\n")
    log("record written: %s (%d metrics, status %s, counter %s)" % (a.out, len(r.metrics), status, ctr))
    return 0 if status != "build_failed" else 3


def select_top(timings, top_n):
    """The top_n passing gates by instructions (wall time when no count exists)."""
    timings = sorted(timings, key=lambda t: -(t["instr"] if t["instr"] is not None else t["wall_s"]))
    top = []
    for t in timings:
        if t["rc"] != 0:
            continue   # a red gate's cost says nothing about its normal cost
        script = re.search(r"(scripts/\S+\.(?:sh|py))", t["cmd"]).group(1)
        gid = re.sub(r"[^A-Za-z0-9_]+", "_", os.path.splitext(os.path.basename(script))[0])
        if any(g["id"] == gid for g in top):
            gid += "_%d" % len(top)
        top.append({"id": gid, "cmd": t["cmd"], "script": script,
                    "discovered_instr": t["instr"], "discovered_wall_s": t["wall_s"]})
        if len(top) == top_n:
            break
    return timings, top


def write_gates(path, sha, machine, timings, top, ctr=None):
    out = {"comment": "The most expensive passing CI gates (by instructions:u), measured once on the "
                      "perf machine by `suite.py discover-gates`. Editing this list is a suite change: "
                      "bump suite_version in suite.json.",
           "discovered_on": sha, "machine": machine, "counter": ctr or counter(), "gates": top, "all": timings}
    with open(path, "w") as f:
        json.dump(out, f, indent=1)
        f.write("\n")


def cmd_discover(a):
    """Rank every CI gate by instructions (wall time when perf is unavailable) and keep the top N
    passing ones. --shard K/N measures every N-th gate starting at K, so N checkouts can rank in
    parallel (instruction counts do not depend on the load that parallelism adds); merge-gates
    combines the shard files."""
    repo = os.path.abspath(a.repo)
    plan = subprocess.run(["bash", "scripts/tests/run_ci_gates_local.sh", "--plan"], cwd=repo,
                          capture_output=True, text=True)
    cmds = [l.strip() for l in plan.stdout.splitlines() if re.search(r"scripts/\S+\.(sh|py)", l)]
    if not cmds:
        log("run_ci_gates_local.sh --plan listed no commands:\n" + plan.stderr[-2000:])
        return 2
    if a.shard:
        k, n = (int(x) for x in a.shard.split("/"))
        cmds = cmds[k::n]
    timings = []
    for c in cmds:
        w, _, rc, ins = timed(["bash", "-c", c], repo, timeout=2 * 3600)
        log("%7.1fs %s instr rc=%d %s" % (w, ins, rc, c))
        timings.append({"cmd": c, "wall_s": round(w, 1), "instr": ins, "rc": rc})
    timings, top = select_top(timings, a.top)
    write_gates(a.out, git(repo, "rev-parse", "HEAD"), a.machine, timings, top)
    return 0


def cmd_merge_gates(a):
    parts = [json.load(open(f)) for f in a.files]
    shas = {p["discovered_on"] for p in parts}
    if len(shas) != 1:
        log("shards were discovered on different commits: %s" % sorted(shas))
        return 2
    ctrs = {p.get("counter") for p in parts}
    machines = {p.get("machine") for p in parts}
    if len(ctrs) != 1 or len(machines) != 1:
        log("shards disagree on counter %s or machine %s" % (sorted(map(str, ctrs)), sorted(map(str, machines))))
        return 2
    timings = [t for p in parts for t in p["all"]]
    cmds = [t["cmd"] for t in timings]
    dups = sorted({c for c in cmds if cmds.count(c) > 1})
    if dups:
        log("a gate appears in more than one shard (same shard passed twice?): %s" % dups[:3])
        return 2
    timings, top = select_top(timings, a.top)
    # Provenance comes from the shards, not from the host that merges them.
    write_gates(a.out, shas.pop(), machines.pop(), timings, top, ctr=ctrs.pop())
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
    sub.add_parser("counter")
    d = sub.add_parser("discover-gates")
    d.add_argument("--repo", required=True)
    d.add_argument("--out", required=True)
    d.add_argument("--top", type=int, default=10)
    d.add_argument("--machine", default=os.environ.get("PERF_MACHINE") or platform.node().split(".")[0])
    d.add_argument("--shard", help="K/N: measure gates K, K+N, K+2N, ... of the plan")
    mg = sub.add_parser("merge-gates")
    mg.add_argument("--out", required=True)
    mg.add_argument("--top", type=int, default=10)
    mg.add_argument("files", nargs="+")
    g = sub.add_parser("gen")
    g.add_argument("family")
    g.add_argument("n", type=int)
    a = ap.parse_args()
    if a.cmd == "run":
        return cmd_run(a)
    if a.cmd == "counter":
        print(counter())
        return 0
    if a.cmd == "discover-gates":
        return cmd_discover(a)
    if a.cmd == "merge-gates":
        return cmd_merge_gates(a)
    sys.stdout.write(gen(a.family, a.n))
    return 0


if __name__ == "__main__":
    sys.exit(main())
