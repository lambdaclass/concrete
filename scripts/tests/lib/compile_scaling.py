#!/usr/bin/env python3
"""COMPILE-TIME SCALING GATE (R-0550, Phase 1 exit 2o; structural class 17, owner R-0422).

Usage: compile_scaling.py --compiler CC --budgets TSV --bounded BOUNDED_SH
                          [--shape NAME]... [--measure] [--keep DIR]

Generates programs of 10^4 - 10^5 elements or statements in each shape the budget file lists,
compiles each at every recorded size, and times every stage. It FAILS when a stage exceeds its
recorded budget at a size, or when a stage's growth between two sizes is worse than near-linear:
  t(n2) / max(t(n1), floor_ms)  >  (n2 / n1) ** exp        (time; see RESOLUTION)
  h(n2) / h(n1)                  >  (n2 / n1) ** hb_exp     (allocation counts)
Both exponents are recorded in the budget file with their reasons (see SIZES).

STAGES. `--emit-trace-json` times parse, resolve-files, resolve, check, elaborate, coreCheck,
monomorphize and lower inside the compiler (`timing_ms`) and counts each stage's small
allocations (`alloc_heartbeats`). The rest of the pipeline has no in-compiler timer, so whole
compiles stand in for it, timed from outside: `core` (`--emit-core`: the front end through
CoreCheck), `llvm` (`--emit-llvm`: through EmitSSA), `native` (`-o`: through clang and the
link) and `interp` (`--interp`: the front end and the interpreter running the program). A stage
proxy contains every in-compiler stage before it, so a quadratic anywhere upstream fails it too.

TWO KINDS OF MEASUREMENT.
  * Heartbeats are deterministic for a given binary and input: their growth check needs no
    retry and is immune to host load. They count allocations, so they see an append-built list
    (bugs 249, 255, 259) but not a list SEARCH (bugs 256 - 258, 260, 261), which allocates
    nothing; that is why wall time is judged as well.
  * Wall time is judged against the budgets and for growth. Host noise is handled by the RETRY
    BUDGET (`retries` in the budget file): when an attempt violates any wall-clock bound, the
    whole shape is measured again, and every (size, stage) keeps its FASTEST run. A noisy spike
    cannot survive a minimum, and a quadratic stage's minimum is still quadratic, so the retry
    never turns a resolvable quadratic (see RESOLUTION) into a pass.

SIZES are a factor of ten apart (10^4 and 10^5 where the shape allows). Between two such sizes a
quadratic stage grows a hundredfold. A linear stage's allocation count grows exactly tenfold, so
the heartbeat bound (hb_exp) is tight. Its time does not: caches, the allocator and memory
bandwidth shared with other jobs cost more at 10^5 elements than at 10^4, and the fastest of three
attempts of a linear stage has grown up to 52-fold on a host running eight gates at once. The time
bound (exp) sits above that and below a quadratic's hundredfold. A stage whose quadratic part is
still small next to its linear part at these sizes grows less than that (10 + 1 ms at 10^4 and
100 + 100 ms at 10^5 is x18) and fails only if it exceeds its budget: such a mixed quadratic can
pass both checks at these sizes, and is caught when it grows past its budget.

RESOLUTION. Every stage's growth is judged, however fast. An in-compiler stage is timed in whole
milliseconds by the compiler's own clock, so the smaller size's time is floored at floor_ms. A
whole compile (core, llvm, native, interp) is timed from outside and judged on its time above a
baseline, the same mode on a one-line program, so process start and clang's fixed cost cannot
dilute its growth; that difference carries the noise of two process runs, so it is floored at
wall_floor_ms (a size-dependent cost too small to resolve that way is held by its budget, and,
inside the compiler, by the per-stage trace). So the growth check resolves a stage whose time at
the larger size exceeds about floor x bound: about 126 ms for an in-compiler stage and about
1.6 s above baseline for a whole compile (floors 2 and 25 ms, x63); a cost below that is held
by its budget only. Heartbeat counts are exact and need no floor; they are judged once the larger
count reaches hb_judge_min.
"""
import argparse
import json
import math
import os
import platform
import re
import shutil
import statistics
import subprocess
import sys
import tempfile
import time

TRACE_STAGES = ["parse", "resolve-files", "resolve", "check", "elaborate", "coreCheck",
                "monomorphize", "lower"]
FRONT = TRACE_STAGES[:6]          # what --emit-core and --interp run before their own work
# A stage proxy CONTAINS these in-compiler stages: an exclusion of one excludes the proxy too.
CONTAINS = {"core": FRONT, "interp": FRONT, "llvm": TRACE_STAGES, "native": TRACE_STAGES}
MODES = ["trace", "core", "llvm", "native", "interp"]
REQUIRED = TRACE_STAGES + ["llvm", "native"]   # the compile path every shape covers or explains
PARAM_DEFAULTS = {"exp": 1.8, "hb_exp": 1.2, "floor_ms": 2.0, "wall_floor_ms": 25.0, "hb_judge_min": 100000.0, "retries": 2.0, "ceiling_factor": 3.0,
                  "ceiling_min_s": 20.0, "measure_runs": 3.0, "budget_factor": 4.0,
                  "budget_slack_ms": 200.0}


# ----------------------------------------------------------------------------- generated corpus
def gen(shape, n):
    """A program of SHAPE at size N whose `main` returns 42 (checked natively and by --interp)."""
    L = []
    a = L.append
    if shape == "array-const":          # bugs 027 (EmitSSA, clang), 249 (parser)
        a("fn main() -> Int {")
        a("    let a: [i64; %d] = [%s];" % (n, ", ".join(["7"] * n)))
        a("    return (a[%d] + 35) as Int;" % (n - 1))
        a("}")
    elif shape == "array-repeat":       # bug 027: `[v; n]`, filled by a loop since the fix
        a("fn main() -> Int {")
        a("    let a: [i64; %d] = [7; %d];" % (n, n))
        a("    return (a[%d] + 35) as Int;" % (n - 1))
        a("}")
    elif shape == "array-distinct":     # bugs 027, 249: no run of equal elements to fold
        a("fn main() -> Int {")
        a("    let a: [i64; %d] = [%s];" % (n, ", ".join(str(i) for i in range(n))))
        a("    return (a[%d] - %d) as Int;" % (n - 1, n - 1 - 42))
        a("}")
    elif shape == "array-writes":       # bug 250 (native place writes), bug 248 (--interp)
        a("fn main() -> Int {")
        a("    let mut a: [i64; %d] = [0; %d];" % (n, n))
        a("    let mut i: i64 = 0;")
        a("    while i < %d {" % n)
        a("        a[i] = (a[i] + i) % 7;")
        a("        i = i + 1;")
        a("    }")
        a("    return (a[%d] - a[%d] + 42) as Int;" % (n - 1, n - 1))
        a("}")
    elif shape == "assignments":        # bugs 255, 256, 258, 259: one long function
        a("fn main() -> Int {")
        a("    let mut s: i64 = 0;")
        a("    let t: i64 = 3;")
        for _ in range(n):
            a("    s = s + t;")
        a("    return (s - %d + 42) as Int;" % (3 * n))
        a("}")
    elif shape == "chained-lets":       # bugs 257 - 260
        a("fn main() -> Int {")
        a("    let s0: i64 = 0;")
        for i in range(1, n + 1):
            a("    let s%d: i64 = s%d + 1;" % (i, i - 1))
        a("    return (s%d - %d + 42) as Int;" % (n, n))
        a("}")
    elif shape == "many-locals":        # n independent bindings, each reading one declared first
        a("fn main() -> Int {")
        a("    let t: i64 = 1;")
        for i in range(n):
            a("    let x%d: i64 = t + %d;" % (i, i % 10))
        a("    return (x0 + x%d - %d + 42) as Int;" % (n - 1, 2 + (n - 1) % 10))
        a("}")
    elif shape in ("lets-if", "lets-while", "lets-match"):   # bugs 258, 261: far reads, blocks
        a("fn main() -> Int {")
        a("    let s0: i64 = 0;")
        a("    let mut t: i64 = 0;")
        for i in range(1, n + 1):
            a("    let s%d: i64 = s%d + 1;" % (i, i - 1))
            if shape == "lets-if":
                a("    if s%d > t { t = t + 1; }" % i)
            elif shape == "lets-while":
                a("    while t < s%d { t = t + 1; }" % i)
            else:
                a("    match s%d { 0 => { t = t + 1; }, _ => { t = t + 2; }, }" % i)
        tv = n if shape != "lets-match" else 2 * n
        a("    return (s%d + t - %d + 42) as Int;" % (n, n + tv))
        a("}")
    elif shape == "functions":          # many declarations
        for i in range(n):
            a("fn f%d(x: i64) -> i64 { return x + %d; }" % (i, i % 3))
        a("fn main() -> Int { return (f0(40) + f%d(2) - %d) as Int; }" % (n - 1, (n - 1) % 3 + 2 - 2))
    elif shape == "call-chain":         # many declarations, each calling the next
        for i in range(n):
            a("fn f%d(x: i64) -> i64 { return f%d(x) + 1; }" % (i, i + 1))
        a("fn f%d(x: i64) -> i64 { return x; }" % n)
        a("fn main() -> Int { return (f0(42) - %d) as Int; }" % n)
    elif shape == "structs":            # many struct declarations
        for i in range(n):
            a("struct S%d { a: i64, b: i64 }" % i)
        m = n - 1
        a("fn main() -> Int { let s: S%d = S%d { a: 40, b: 2 }; let S%d { a, b } = s; "
          "return (a + b) as Int; }" % (m, m, m))
    elif shape == "enums":              # many enum declarations
        for i in range(n):
            a("enum E%d { A { v: i64 }, B }" % i)
        m = n - 1
        a("fn main() -> Int { let e: E%d = E%d::A { v: 42 }; match e { E%d::A { v } => { "
          "return v as Int; }, E%d::B => { return 0; }, } }" % (m, m, m, m))
    elif shape == "nested-ifs":         # deep statement nesting
        a("fn main() -> Int {")
        a("    let mut s: i64 = 0;")
        a("    let t: i64 = 1;")
        a("    " + "if t > 0 { s = s + 1; " * n + "}" * n)
        a("    return (s - %d + 42) as Int;" % n)
        a("}")
    elif shape == "nested-parens":      # deep expression nesting
        a("fn main() -> Int {")
        a("    let s: i64 = " + "(1 + " * n + "0" + ")" * n + ";")
        a("    return (s - %d + 42) as Int;" % n)
        a("}")
    else:
        raise ValueError("unknown shape %r" % shape)
    return "\n".join(L) + "\n"


# ----------------------------------------------------------------------------- the budget file
class Budgets:
    """The checked-in record: params, shapes (sizes and modes), budgets and exclusions."""

    def __init__(self, path):
        self.path = path
        self.params = dict(PARAM_DEFAULTS)
        self.shapes = []                 # [(name, [sizes], [modes])] in file order
        self.budget = {}                 # (shape, n, stage) -> ms
        self.exclude = {}                # (shape, stage) -> reason
        self.errors = []
        self.header = []
        with open(path) as f:
            for lineno, raw in enumerate(f, 1):
                line = raw.rstrip("\n")
                if not line.strip() or line.startswith("#"):
                    if line.startswith("#"):
                        self.header.append(line)
                    continue
                parts = line.split("\t")
                kind = parts[0]
                try:
                    if kind == "param" and len(parts) == 3:
                        if parts[1] not in PARAM_DEFAULTS:
                            raise ValueError("unknown param %s" % parts[1])
                        self.params[parts[1]] = float(parts[2])
                    elif kind == "shape" and len(parts) == 4:
                        sizes = [int(x) for x in parts[2].split(",")]
                        modes = parts[3].split(",")
                        if sorted(sizes) != sizes or len(set(sizes)) != len(sizes) or len(sizes) < 2:
                            raise ValueError("sizes must be at least two, increasing")
                        if any(m not in MODES for m in modes):
                            raise ValueError("unknown mode in %s" % parts[3])
                        gen(parts[1], 2)     # the generator knows the shape
                        self.shapes.append((parts[1], sizes, modes))
                    elif kind == "budget" and len(parts) == 5:
                        self.budget[(parts[1], int(parts[2]), parts[3])] = float(parts[4])
                    elif kind == "exclude" and len(parts) == 4:
                        if not parts[3].strip():
                            raise ValueError("an exclusion needs a reason")
                        for st in parts[2].split(","):
                            if st != "*" and st not in TRACE_STAGES and st not in CONTAINS:
                                raise ValueError("unknown stage %s" % st)
                            if (parts[1], st) in self.exclude:
                                raise ValueError("stage %s of %s excluded twice" % (st, parts[1]))
                            self.exclude[(parts[1], st)] = parts[3].strip()
                    else:
                        raise ValueError("malformed line")
                except ValueError as e:
                    self.errors.append("%s:%d: %s: %r" % (path, lineno, e, line))
        names = [s[0] for s in self.shapes]
        for (sh, _st) in self.exclude:
            if sh not in names:
                self.errors.append("exclusion names unknown shape %s" % sh)
        # Every stage of the compile path is measured for every shape, or its absence has a
        # recorded reason: a shape timed only by --emit-core says why Mono, Lower, EmitSSA and
        # clang are not timed, so no stage drops out of the gate silently.
        for name, _sizes, modes in self.shapes:
            if (name, "*") in self.exclude:
                continue
            measured = stages_of(modes)
            if "core" in modes:              # --emit-core times the front end as one stage
                measured = measured + FRONT
            for st in REQUIRED:
                if st not in measured and (name, st) not in self.exclude:
                    self.errors.append("shape %s does not measure %s and records no reason (exclude line)"
                                       % (name, st))

    def p(self, k):
        return self.params[k]

    def excluded(self, shape, stage):
        """The reason a stage of a shape is not judged, or None."""
        for key in ((shape, "*"), (shape, stage)):
            if key in self.exclude:
                return self.exclude[key]
        for inner in CONTAINS.get(stage, []):
            if (shape, inner) in self.exclude:
                return "contains %s, excluded: %s" % (inner, self.exclude[(shape, inner)])
        return None


def stages_of(modes):
    out = []
    for m in modes:
        out += TRACE_STAGES if m == "trace" else [m]
    return out


# ----------------------------------------------------------------------------- running
class Runner:
    def __init__(self, cc, bounded, budgets, workdir):
        self.cc, self.bounded, self.b, self.dir = cc, bounded, budgets, workdir
        self.base = {}

    def baseline(self, m):
        """The fastest of three whole runs of mode M on a one-line program: process start, the
        standard library, clang's fixed cost and the watchdog. Growth is judged on time above it,
        so a size-independent cost cannot hide a stage's growth."""
        if m not in self.base:
            src = os.path.join(self.dir, "baseline.con")
            with open(src, "w") as f:
                f.write("fn main() -> Int {\n    return 42;\n}\n")
            argv = {"core": ["--emit-core"], "llvm": ["--emit-llvm"], "interp": ["--interp"],
                    "native": ["-o", src + ".bin"]}[m]
            walls = []
            for _ in range(3):
                rc, _o, _e, wall, to = self.bounded_run(60, [self.cc, src] + argv)
                if rc == 0 and not to:
                    walls.append(wall)
            # None: the baseline did not run, and a whole compile is then not judged as measured.
            self.base[m] = min(walls) if walls else None
        return self.base[m]

    def ceiling(self, shape, n, stages):
        known = [self.b.budget.get((shape, n, s)) for s in stages]
        if any(k is None for k in known):
            return 600.0
        return max(self.b.p("ceiling_min_s"), self.b.p("ceiling_factor") * sum(known) / 1000.0)

    def bounded_run(self, secs, argv):
        """(rc, stdout, stderr, wall_ms, timed_out)."""
        t0 = time.monotonic()
        r = subprocess.run(["bash", self.bounded, "%.3f" % secs] + argv,
                           capture_output=True, text=True)
        wall = (time.monotonic() - t0) * 1000.0
        timed_out = r.returncode == 124 and "INCOMPLETE:" in r.stderr
        return r.returncode, r.stdout, r.stderr, wall, timed_out

    def measure(self, shape, n, modes):
        """One attempt at one size: {stage: ms}, {stage: heartbeats}, {stage: 'timeout'|error}."""
        src = os.path.join(self.dir, "%s_%d.con" % (shape, n))
        with open(src, "w") as f:           # always this size's program, even in a --keep dir
            f.write(gen(shape, n))
        times, beats, bad = {}, {}, {}
        for m in modes:
            if m == "trace":
                secs = self.ceiling(shape, n, TRACE_STAGES)
                rc, out, err, _w, to = self.bounded_run(secs, [self.cc, src, "--emit-trace-json"])
                if to:
                    for s in TRACE_STAGES:
                        bad[s] = "timeout %.0fs" % secs
                    continue
                try:
                    j = json.loads(out) if rc == 0 else None
                    tm, hb = j["timing_ms"], j["alloc_heartbeats"]
                    for s in TRACE_STAGES:
                        if not isinstance(tm.get(s), int) or not isinstance(hb.get(s), int):
                            raise KeyError(s)
                        times[s] = float(tm[s])
                        beats[s] = float(hb[s])
                except (ValueError, KeyError, TypeError):
                    msg = "trace failed rc %d: %s" % (rc, (err or out)[:160].replace("\n", " "))
                    for s in TRACE_STAGES:
                        bad[s] = msg
                continue
            secs = self.ceiling(shape, n, [m])
            binary = src + ".bin"
            if os.path.exists(binary):
                os.unlink(binary)            # a stale binary must never stand in for this compile
            if m == "core":
                argv = [self.cc, src, "--emit-core"]
            elif m == "llvm":
                argv = [self.cc, src, "--emit-llvm"]
            elif m == "interp":
                argv = [self.cc, src, "--interp"]
            else:
                argv = [self.cc, src, "-o", binary]
            rc, out, err, wall, to = self.bounded_run(secs, argv)
            if to:
                bad[m] = "timeout %.0fs" % secs
                continue
            # A run is a measurement only if it produced what the mode produces: Core text with
            # `main`, an LLVM module defining `@main`, a fresh binary, the interpreter's 42.
            wrong = None
            if rc != 0:
                wrong = "%s failed rc %d: %s" % (m, rc, (err or out)[:160].replace("\n", " "))
            elif m == "core" and not re.search(r"\bfn main\b", out):
                wrong = "--emit-core printed no `fn main`"
            elif m == "llvm" and not re.search(r"^define [^\n]*@main\(", out, re.M):
                wrong = "--emit-llvm printed no definition of @main"
            elif m == "interp" and out.strip() != "42":
                wrong = "--interp printed %r, want 42" % out.strip()[:40]
            elif m == "native" and not os.path.exists(binary):
                wrong = "-o reported success but wrote no binary"
            if wrong:
                bad[m] = wrong
                continue
            if m == "native":
                # The compile is measured; the binary must also be right (an unmeasured or
                # miscompiled run is not a pass).
                rrc, _o, _e, _w, rto = self.bounded_run(20, [binary])
                if rto or rrc != 42:
                    bad[m] = "binary exited %s, want 42" % ("timeout" if rto else rrc)
                    continue
            times[m] = wall
        return times, beats, bad


def judge(b, shape, sizes, stages, times, beats, bad, base=None):
    """Lines and verdicts for one shape: [(verdict, text)], verdict in ok/FAIL/EXCLUDED/note."""
    exp = b.p("exp")
    res = []
    for s in stages:
        why = b.excluded(shape, s)
        cells = []
        for n in sizes:
            if (n, s) in bad:
                cells.append("%d:%s" % (n, bad[(n, s)]))
            elif (n, s) in times:
                cells.append("%d:%.0fms" % (n, times[(n, s)]))
        detail = " ".join(cells)
        if why is not None:
            res.append(("EXCLUDED", "%s/%s [%s] -- %s" % (shape, s, detail, why)))
            continue
        problems = []
        for n in sizes:
            if (n, s) in bad:
                problems.append("n=%d %s" % (n, bad[(n, s)]))
                continue
            if (n, s) not in times:
                problems.append("n=%d not measured" % n)
                continue
            bud = b.budget.get((shape, n, s))
            if bud is None:
                problems.append("n=%d has no recorded budget" % n)
            elif times[(n, s)] > bud:
                problems.append("n=%d %.0fms > budget %.0fms" % (n, times[(n, s)], bud))
        growth = []
        hbfail = []
        for n1, n2 in zip(sizes, sizes[1:]):
            limit = (n2 / n1) ** exp
            if (n1, s) in times and (n2, s) in times and (n1, s) not in bad and (n2, s) not in bad:
                # Whole compiles are judged above their size-independent baseline.
                b0 = (base or {}).get(s, 0.0)
                t1, t2 = max(0.0, times[(n1, s)] - b0), max(0.0, times[(n2, s)] - b0)
                floor = b.p("floor_ms") if s in TRACE_STAGES else b.p("wall_floor_ms")
                r = t2 / max(t1, floor)
                if r > limit:
                    problems.append("time grew x%.1f from %d to %d > x%.1f (n^%.1f)"
                                    % (r, n1, n2, limit, exp))
                else:
                    growth.append("x%.1f<=x%.1f" % (r, limit))
            if (n1, s) in beats and (n2, s) in beats:
                h1, h2 = beats[(n1, s)], beats[(n2, s)]
                r = h2 / max(h1, 1.0)
                hlimit = (n2 / n1) ** b.p("hb_exp")
                if h2 >= b.p("hb_judge_min") and r > hlimit:
                    hbfail.append("heartbeats grew x%.1f from %d to %d > x%.1f (n^%.1f)"
                                  % (r, n1, n2, hlimit, b.p("hb_exp")))
        if hbfail:
            res.append(("FAIL-HB", "%s/%s %s [%s]" % (shape, s, "; ".join(hbfail), detail)))
        if problems:
            res.append(("FAIL", "%s/%s %s [%s]" % (shape, s, "; ".join(problems), detail)))
        elif not hbfail:
            res.append(("ok", "%s/%s [%s] growth %s" % (shape, s, detail, " ".join(growth) or "-")))
    return res


def run_shape(runner, b, shape, sizes, modes, attempts):
    """Measure a shape up to ATTEMPTS times; keep the fastest run per (size, stage)."""
    stages = stages_of(modes)
    times, beats, bad = {}, {}, {}
    for attempt in range(1, attempts + 1):
        for n in sizes:
            t, h, e = runner.measure(shape, n, modes)
            for s, v in t.items():
                times[(n, s)] = min(v, times.get((n, s), v))
            for s, v in h.items():
                beats[(n, s)] = min(v, beats.get((n, s), v))
            # A failure in ANY attempt stands: a wrong result or a refused compile is never
            # noise, and a later attempt's time does not make it a measurement.
            for s, v in e.items():
                bad.setdefault((n, s), v)
        base = {m: runner.baseline(m) for m in modes if m != "trace"}
        for m, v in base.items():
            if v is None:                   # no baseline, no growth judgement: not a pass
                for n in sizes:
                    bad.setdefault((n, m), "its baseline (a one-line program) did not compile")
        verdicts = judge(b, shape, sizes, stages, times, beats, bad,
                         {m: v for m, v in base.items() if v is not None})
        wall_fail = any(v == "FAIL" for v, _ in verdicts)
        # Heartbeat failures are deterministic: no retry changes them. A compile that failed is
        # not noise, and a run killed at its ceiling twice (bounded.sh already re-ran it alone
        # with twice the time) is over its budget by more than the ceiling factor: neither is
        # retried again.
        hard = bool(bad)
        if not wall_fail or hard or attempt == attempts:
            return verdicts, attempt
    return verdicts, attempts


def git_identity(root):
    try:
        sha = subprocess.run(["git", "-C", root, "rev-parse", "HEAD"], capture_output=True,
                             text=True).stdout.strip()
        dirty = subprocess.run(["git", "-C", root, "status", "--porcelain", "--untracked-files=no"],
                               capture_output=True, text=True).stdout.strip()
        return sha + ("+dirty" if dirty else "")
    except OSError:
        return "unknown"


def host_identity():
    cpu = platform.processor() or platform.machine()
    try:
        with open("/proc/cpuinfo") as f:
            for line in f:
                if line.startswith("model name"):
                    cpu = line.split(":", 1)[1].strip()
                    break
    except OSError:
        pass
    return "%s, %s, %d threads" % (platform.node(), cpu, os.cpu_count() or 0)


def do_measure(runner, b, shapes, root):
    runs = int(b.p("measure_runs"))
    new = {}
    for shape, sizes, modes in shapes:
        if b.excluded(shape, "*") is not None:
            print("  skip %s (excluded: %s)" % (shape, b.excluded(shape, "*")))
            continue
        samples = {}
        for _ in range(runs):
            for n in sizes:
                t, _h, e = runner.measure(shape, n, modes)
                if e:
                    print("  FAIL %s n=%d: %s" % (shape, n, e))
                    return 1
                for s, v in t.items():
                    samples.setdefault((n, s), []).append(v)
        for (n, s), vs in sorted(samples.items()):
            med = statistics.median(vs)
            bud = max(math.ceil(b.p("budget_factor") * med), math.ceil(med + b.p("budget_slack_ms")))
            new[(shape, n, s)] = bud
            print("  %s n=%d %s: median %.0fms of %s -> budget %dms"
                  % (shape, n, s, med, ",".join("%.0f" % v for v in vs), bud))
    # Rewrite the file: keep every other line, replace the measured shapes' budgets. A run over
    # every shape replaces every `measured:` stamp; a run over some shapes keeps the earlier stamps
    # (which then cover the other shapes) and adds one naming the shapes it measured, so every
    # budget row keeps the commit and host it was measured at.
    measured = sorted({k[0] for k in new}, key=lambda n: [x[0] for x in b.shapes].index(n))
    every = all(name in measured or b.excluded(name, "*") is not None for name, _s, _m in b.shapes)
    with open(b.path) as f:
        lines = f.read().splitlines()
    out = []
    for line in lines:
        if line.startswith("budget\t") or (every and line.startswith("# measured:")):
            continue
        out.append(line)
    stamp = "# measured: commit %s on %s, %s, %d runs (median), budget = max(%g x median, median + %gms)%s" % (
        git_identity(root), host_identity(), time.strftime("%Y-%m-%d"), runs,
        b.p("budget_factor"), b.p("budget_slack_ms"),
        "" if every else " -- shapes: " + ", ".join(measured))
    # The stamp goes after the leading comment block (and after earlier stamps).
    i = 0
    while i < len(out) and out[i].startswith("#"):
        i += 1
    out.insert(i, stamp)
    old = {k: v for k, v in b.budget.items() if k[0] not in measured}
    order = {name: k for k, (name, _s, _m) in enumerate(b.shapes)}
    for key in sorted(set(old) | set(new),
                      key=lambda k: (order.get(k[0], 999), k[1], stages_of(MODES).index(k[2]))):
        v = new.get(key, old.get(key))
        out.append("budget\t%s\t%d\t%s\t%d" % (key[0], key[1], key[2], v))
    with open(b.path, "w") as f:
        f.write("\n".join(out) + "\n")
    print("wrote %d budgets to %s" % (len(new), b.path))
    return 0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--compiler", required=True)
    ap.add_argument("--budgets", required=True)
    ap.add_argument("--bounded", required=True)
    ap.add_argument("--root", default=".")
    ap.add_argument("--shape", action="append", default=[])
    ap.add_argument("--measure", action="store_true")
    ap.add_argument("--keep")
    a = ap.parse_args()

    b = Budgets(a.budgets)
    if b.errors:
        for e in b.errors:
            print("FATAL: %s" % e)
        return 2
    shapes = [s for s in b.shapes if not a.shape or s[0] in a.shape]
    if a.shape and len(shapes) != len(set(a.shape)):
        print("FATAL: unknown shape among %s" % a.shape)
        return 2
    workdir = a.keep or tempfile.mkdtemp(prefix="compile_scaling.")
    os.makedirs(workdir, exist_ok=True)
    runner = Runner(a.compiler, a.bounded, b, workdir)
    try:
        if a.measure:
            return do_measure(runner, b, shapes, a.root)
        meas = [l for l in b.header if l.startswith("# measured:")]
        print("=== compile-time scaling (R-0550): per-stage budgets; growth <= n^%.2f in time, n^%.2f in "
              "allocations ===" % (b.p("exp"), b.p("hb_exp")))
        for m in meas or ["# NOT MEASURED"]:
            print("  budgets %s" % m[2:])
        npass = nfail = nexcl = 0
        attempts = 1 + int(b.p("retries"))
        for shape, sizes, modes in shapes:
            why = b.excluded(shape, "*")
            if why is not None:
                print("  EXCLUDED %s (not run) -- %s" % (shape, why))
                nexcl += 1
                continue
            t0 = time.monotonic()
            measured = stages_of(modes)
            for (sh, st), why in sorted(b.exclude.items()):
                if sh == shape and st != "*" and st not in measured:
                    print("  EXCLUDED %s/%s (not measured for this shape) -- %s" % (shape, st, why))
                    nexcl += 1
            if "core" in modes and "trace" not in modes:
                print("  note %s: %s are timed together as `core` (--emit-core); the per-stage trace "
                      "would also run lower" % (shape, ", ".join(FRONT)))
            verdicts, used = run_shape(runner, b, shape, sizes, modes, attempts)
            for v, text in verdicts:
                if v == "ok":
                    npass += 1
                    print("  ok   " + text)
                elif v == "EXCLUDED":
                    nexcl += 1
                    print("  EXCLUDED " + text)
                else:
                    nfail += 1
                    print("  FAIL " + text + ("  (heartbeats: deterministic, not retried)" if v == "FAIL-HB" else ""))
            print("  -- %s: %d attempt(s), %.1fs" % (shape, used, time.monotonic() - t0))
        print()
        print("check_compile_scaling: PASS=%d FAIL=%d EXCLUDED=%d" % (npass, nfail, nexcl))
        return 1 if nfail or npass == 0 else 0
    finally:
        if not a.keep:
            shutil.rmtree(workdir, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
