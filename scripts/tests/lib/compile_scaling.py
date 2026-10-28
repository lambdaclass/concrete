#!/usr/bin/env python3
"""COMPILE-TIME SCALING GATE (R-0550, Phase 1 exit 2o; structural class 17, owner R-0422).

Usage: compile_scaling.py --compiler CC --budgets TSV --bounded BOUNDED_SH
                          [--shape NAME]... [--measure] [--keep DIR]

Generates programs of 10^4 - 10^5 elements or statements in each shape the budget file lists,
compiles each at every recorded size, and times every stage. It FAILS when a stage exceeds its
recorded budget at a size, or when its growth between two sizes is worse than near-linear:
  (I(n2) - I0) / (I(n1) - I0)    >  (n2 / n1) ** instr_exp  (a counted run's instructions)
  t(n2) / max(t(n1), floor_ms)  >  (n2 / n1) ** exp        (time, for a run perf did not count)
  h(n2) / h(n1)                  >  (n2 / n1) ** hb_exp     (allocation counts)
The exponents are recorded in the budget file with their reasons (see SIZES and GROWTH).

STAGES. `--emit-trace-json` times parse, resolve-files, resolve, check, elaborate, coreCheck,
monomorphize and lower inside the compiler (`timing_ms`) and counts each stage's small
allocations (`alloc_heartbeats`). The rest of the pipeline has no in-compiler timer, so whole
compiles stand in for it, timed from outside: `core` (`--emit-core`: the front end through
CoreCheck), `llvm` (`--emit-llvm`: through EmitSSA), `native` (`-o`: through clang and the
link) and `interp` (`--interp`: the front end and the interpreter running the program). A stage
proxy contains every in-compiler stage before it, so a quadratic anywhere upstream fails it too.

THREE KINDS OF MEASUREMENT.
  * Heartbeats are deterministic for a given binary and input: their growth check and their
    budget (`hbbudget`, hb_budget_factor x the count at --measure) need no retry and are immune to
    host load. They count allocations, so they see an append-built list (bugs 249, 255, 259) but
    not a list SEARCH (bugs 256 - 258, 260, 261), which allocates nothing.
  * Retired user instructions (`perf stat -e instructions:u`, on a host whose perf_event_paranoid
    allows it, as check_ssa_lowering_scaling.sh counts them) of every timed run: exact up to a few
    in a million whatever the load (bug 294), so a run's count is judged without retries against
    its budget (`instr`, instr_budget_factor x the median at --measure) and for growth between
    sizes (instr_exp, see GROWTH). They see work that allocates nothing, for the whole run, not
    per stage.
  * Wall time is judged against the budgets, and for growth only in a run perf did not count.
    Host noise is handled two ways.
    The RETRY BUDGET (`retries`): when an attempt violates any wall-clock bound, the whole shape
    is measured again, and every (size, stage) keeps its FASTEST run; a noisy spike cannot
    survive a minimum, and a quadratic stage's minimum is still quadratic. HOST NORMALIZATION
    (bug 294): a CI host under sustained load runs the same instructions several times slower
    (concrete-ci-2 at load 24: one binary's 10^5-assignment trace in 4637, 3585 and 2190 ms at
    the same 13.615 G instructions), which no retry outlasts. So --measure records, per (shape,
    size, mode), the run's milliseconds per 10^9 instructions (`rate`, the session's quietest),
    and the judge divides a stage's time by the factor its run went slower than that, at least 1
    and at most norm_cap, before comparing it with the budget. The run's own instruction count is
    the denominator, so added work does not raise its allowance (it shows in the instruction
    budget); a change that makes the same instructions slower raises it, up to norm_cap: such a
    regression is held by the budget only beyond norm_cap times it. Growth is judged on raw times.
    A host that cannot count does not judge wall time at all (see HOSTS THAT CANNOT COUNT).

SIZES are a factor of ten apart (10^4 and 10^5 where the shape allows). Between two such sizes a
quadratic stage grows a hundredfold. A linear stage's allocation count grows exactly tenfold, so
the heartbeat bound (hb_exp) is tight. Its time does not: caches, the allocator and memory
bandwidth shared with other jobs cost more at 10^5 elements than at 10^4, and the fastest of three
attempts of a linear stage has grown up to 52-fold on a host running eight gates at once. The time
bound (exp) sits above that and below a quadratic's hundredfold; it judges only runs perf did not
count on a host that counts (see GROWTH and HOSTS THAT CANNOT COUNT). A stage whose quadratic part is
still small next to its linear part at these sizes grows less than that (10 + 1 ms at 10^4 and
100 + 100 ms at 10^5 is x18) and fails only if it exceeds its budget: such a mixed quadratic can
pass both checks at these sizes, and is caught when it grows past its budget.

GROWTH (bug 294). A run whose instructions were counted at both sizes, and none of whose stages is
excluded, has its growth judged on those counts above its mode's baseline (the same mode on a
one-line program: process start, the standard library, clang's fixed work), against instr_exp
1.06: x11.5 for x10. A linear compile grows x10.0 - x10.3 above the baseline (functions, enums,
assignments at 10^4 -> 10^5 on main ea21a2e8's stack), a quadratic part multiplies its share by a
hundred, so one that is 1.6 % of the run at 10^4 fails. The counts are exact whatever the host
does, so no retry is needed or allowed. Those runs' stages are then held by their wall budgets
only: a wall RATIO between sizes measures the caches as much as the work (on an idle dev host a
linear stage grows x45 - x60 from 10^4 to 10^5 as its data leaves the caches, close to the x63 a
quadratic had to exceed), so it false-failed linear stages and could not resolve a small quadratic
anyway. A run perf did not count on a host that counts (a baseline that failed) keeps the wall
growth bound below; the negative controls prove both directions on every run.

HOSTS THAT CANNOT COUNT (bug 315). Where perf cannot count instructions:u (perf missing, or
perf_event_paranoid above 2, as on the dev host), every verdict that depends on the host's speed is
NOT JUDGED there: a stage's wall growth (the ratio false-failed linear stages on a loaded dev host:
enums/lower x80 from 2 ms at 10^4, while its instructions grow linearly) and its wall budget (no
host factor can be measured, and the budgets were measured on the CI host class). Such a stage
reports NOT-JUDGED, never ok, and the gate exits NOT_JUDGED_RC (77): the CI-gates runner accepts
that status only from a gate that scripts/tests/lib/host_tools.sh declares judged on CI (judged_on:
ci-counted) and that CI runs as an unconditional step, and records it as deferred to CI, which is
not a pass. What still holds on such a host is what does not depend on its speed: allocation
heartbeats (exact counts: their budget and growth), every compile's result (a refused compile, a
wrong answer, a missing baseline), the run ceiling (a compile killed at ceiling_factor x its
budgets, and again when bounded.sh re-runs it alone with twice the time, is no measurement and fails
its stages), and a catastrophic overrun: a stage over
uncounted_fail_factor (10) x its budget, which is 40 x the CI host's quiet median, and at least 2 s
for the smallest stage (budget_slack_ms); load has slowed a stage at most x2 - x3 (bug 294, this
bug), so no load reaches that, while a hang or an accidental exponential does.
CI must count: on CI (GITHUB_ACTIONS=true or CI=true/1) a host that cannot count FAILS the gate,
because the verdict deferred to CI must be made there, not deferred again.

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
                  "budget_slack_ms": 200.0, "hb_budget_factor": 1.25, "instr_budget_factor": 1.25,
                  "instr_exp": 1.06, "norm_cap": 2.0, "measure_max_load": 8.0,
                  "uncounted_fail_factor": 10.0}
# The exit status of a run whose only shortfall is verdicts not judged on this host (bug 315): not 0
# (it did not pass) and not 1 (nothing failed). run_ci_gates_local.sh reads it through
# scripts/tests/lib/host_tools.sh (GATE_DEFERRED_RC), and only for a gate declared there.
NOT_JUDGED_RC = 77


def on_ci(env):
    """Whether this run is CI's: GitHub sets GITHUB_ACTIONS=true and CI=true on every runner."""
    return env.get("GITHUB_ACTIONS", "") == "true" or env.get("CI", "").lower() in ("true", "1")


def host_policy(counts, env):
    """How this host's wall-time verdicts are made (bug 315): 'counted' where perf counts
    instructions:u, 'deferred' (not judged here; CI decides) where it cannot, and 'refused' where it
    cannot on CI, which is where the deferred verdicts are decided."""
    if counts:
        return "counted"
    return "refused" if on_ci(env) else "deferred"


def exit_status(npass, nfail, nnot):
    """0 only when everything judged passed and nothing was left unjudged; 1 on any failure or when
    nothing at all was judged; NOT_JUDGED_RC when the only shortfall is NOT-JUDGED verdicts."""
    if nfail or (npass == 0 and nnot == 0):
        return 1
    return NOT_JUDGED_RC if nnot else 0


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
        self.hbbudget = {}               # (shape, n, stage) -> allocation heartbeats
        self.instr = {}                  # (shape, n, mode) -> retired user instructions
        self.rate = {}                   # (shape, n, mode) -> ms per 10^9 instructions at --measure
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
                    elif kind == "hbbudget" and len(parts) == 5:
                        self.hbbudget[(parts[1], int(parts[2]), parts[3])] = float(parts[4])
                    elif kind == "instr" and len(parts) == 5:
                        self.instr[(parts[1], int(parts[2]), parts[3])] = float(parts[4])
                    elif kind == "rate" and len(parts) == 5:
                        self.rate[(parts[1], int(parts[2]), parts[3])] = float(parts[4])
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


def instr_of(text):
    """The `instructions:u` count of a `perf stat -x,` report, or None."""
    for line in text.splitlines():
        parts = line.split(",")
        if len(parts) >= 3 and parts[2].strip() == "instructions:u" and re.match(r"^\d+$", parts[0].strip()):
            return int(parts[0].strip())
    return None


def perf_ok():
    """Whether this host can count retired user instructions (perf_event_paranoid <= 2), as
    check_ssa_lowering_scaling.sh asks it."""
    if not shutil.which("perf"):
        return False
    try:
        p = subprocess.run(["perf", "stat", "-e", "instructions:u", "-x,", "true"],
                           capture_output=True, text=True, timeout=30)
    except (OSError, subprocess.TimeoutExpired):
        return False
    return p.returncode == 0 and instr_of(p.stderr) is not None


def mode_of(stage):
    """The run that times a stage: the trace for an in-compiler stage, else the stage itself."""
    return "trace" if stage in TRACE_STAGES else stage


def stages_of(modes):
    out = []
    for m in modes:
        out += TRACE_STAGES if m == "trace" else [m]
    return out


# ----------------------------------------------------------------------------- running
class Runner:
    def __init__(self, cc, bounded, budgets, workdir, perf=False):
        self.cc, self.bounded, self.b, self.dir = cc, bounded, budgets, workdir
        self.base = {}
        self.base_instr = {}
        self.perf = perf

    def baseline(self, m):
        """The fastest of three whole runs of mode M on a one-line program: process start, the
        standard library, clang's fixed cost and the watchdog. Growth is judged on time above it,
        so a size-independent cost cannot hide a stage's growth."""
        if m not in self.base:
            src = os.path.join(self.dir, "baseline.con")
            with open(src, "w") as f:
                f.write("fn main() -> Int {\n    return 42;\n}\n")
            argv = {"trace": ["--emit-trace-json"], "core": ["--emit-core"], "llvm": ["--emit-llvm"],
                    "interp": ["--interp"], "native": ["-o", src + ".bin"]}[m]
            walls, counts = [], []
            for _ in range(3):
                rc, _o, _e, wall, to, ins = self.bounded_run(60, [self.cc, src] + argv, count=True)
                if rc == 0 and not to:
                    walls.append(wall)
                    if ins:
                        counts.append(ins)
            # None: the baseline did not run, and a whole compile is then not judged as measured.
            self.base[m] = min(walls) if walls else None
            # The instructions every run of this mode spends whatever its input (process start, the
            # standard library, clang's fixed work): instruction growth is judged above them (bug 294).
            self.base_instr[m] = min(counts) if counts else None
        return self.base[m]

    def ceiling(self, shape, n, stages):
        known = [self.b.budget.get((shape, n, s)) for s in stages]
        if any(k is None for k in known):
            return 600.0
        return max(self.b.p("ceiling_min_s"), self.b.p("ceiling_factor") * sum(known) / 1000.0)

    def bounded_run(self, secs, argv, count=False):
        """(rc, stdout, stderr, wall_ms, timed_out, instructions). With COUNT on a host that can
        count, the run is wrapped in `perf stat -e instructions:u` (its report goes to a file, so the
        command's own streams are unchanged) and its retired user instructions are returned, else
        None."""
        stat = None
        if count and self.perf:
            stat = os.path.join(self.dir, "perf.stat")
            if os.path.exists(stat):
                os.unlink(stat)
            argv = ["perf", "stat", "-e", "instructions:u", "-x,", "-o", stat, "--"] + argv
        t0 = time.monotonic()
        r = subprocess.run(["bash", self.bounded, "%.3f" % secs] + argv,
                           capture_output=True, text=True)
        wall = (time.monotonic() - t0) * 1000.0
        timed_out = r.returncode == 124 and "INCOMPLETE:" in r.stderr
        instr = None
        if stat is not None and not timed_out and os.path.exists(stat):
            with open(stat) as f:
                instr = instr_of(f.read())
        return r.returncode, r.stdout, r.stderr, wall, timed_out, instr

    def measure(self, shape, n, modes):
        """One attempt at one size: {stage: ms}, {stage: heartbeats}, {stage: 'timeout'|error},
        {mode: instructions}, {mode: whole-run wall ms} (the last two for the runs `perf` counted)."""
        src = os.path.join(self.dir, "%s_%d.con" % (shape, n))
        with open(src, "w") as f:           # always this size's program, even in a --keep dir
            f.write(gen(shape, n))
        times, beats, bad, instrs, walls = {}, {}, {}, {}, {}
        for m in modes:
            if m == "trace":
                secs = self.ceiling(shape, n, TRACE_STAGES)
                rc, out, err, w, to, ins = self.bounded_run(secs, [self.cc, src, "--emit-trace-json"],
                                                            count=True)
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
                    if ins:
                        instrs[m], walls[m] = float(ins), w
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
            rc, out, err, wall, to, ins = self.bounded_run(secs, argv, count=True)
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
                rrc, _o, _e, _w, rto, _i = self.bounded_run(20, [binary])
                if rto or rrc != 42:
                    bad[m] = "binary exited %s, want 42" % ("timeout" if rto else rrc)
                    continue
            times[m] = wall
            if ins:
                instrs[m], walls[m] = float(ins), wall
        return times, beats, bad, instrs, walls


def host_factor(b, shape, n, m, walls, instrs):
    """How much slower this host ran mode M's compile than when the budgets were measured, per
    instruction: (wall / instructions) now over the rate recorded at --measure, at least 1 and at
    most `norm_cap`. 1 when either side was not counted. The compile's own instruction count is the
    denominator, so a change that adds work does not raise its own allowance; a change that makes
    the same instructions slower raises it up to the cap (stated in compile_scaling_budgets.tsv)."""
    ref = b.rate.get((shape, n, m))
    ins, w = instrs.get(m), walls.get(m)
    if ref is None or not ins or not w or ref <= 0:
        return 1.0
    return min(b.p("norm_cap"), max(1.0, (w / (ins / 1e9)) / ref))


def judge(b, shape, sizes, stages, times, beats, bad, base=None, ntimes=None, factors=None,
          instrs=None, modes=None, base_instr=None, wall_judged=True):
    """Lines and verdicts for one shape: [(verdict, text)], verdict in ok/FAIL/FAIL-HB/FAIL-INSTR/
    NOT-JUDGED/EXCLUDED. A stage's time is judged against its budget after dividing by the host
    factor of the run that timed it (`ntimes`); its growth is judged on the raw fastest times.
    WALL_JUDGED False (a host that cannot count instructions, bug 315): wall growth and wall budgets
    are not judged, only a catastrophic overrun (uncounted_fail_factor x the budget) fails, and a
    stage nothing else fails is NOT-JUDGED, never ok."""
    exp = b.p("exp")
    ntimes = ntimes if ntimes is not None else times
    factors = factors or {}
    instrs = instrs or {}
    base_instr = base_instr or {}
    res = []
    # GROWTH BY INSTRUCTIONS (bug 294). A run whose retired instructions were counted at every size
    # and whose stages are not excluded has its growth judged on those counts (below), exactly and
    # whatever the host does; its stages' wall times are then held by their budgets only. A wall
    # ratio between sizes measures the caches as much as the work (a linear stage's time grows x45 -
    # x60 from 10^4 to 10^5 on an idle host, close to a bound a quadratic must exceed), while a
    # quadratic part multiplies the instructions. A run that was not counted keeps the wall bound.
    def instr_judged(m):
        if m == "trace":
            if any(b.excluded(shape, st) for st in TRACE_STAGES):
                return False
        elif b.excluded(shape, m):
            return False
        # A count without its baseline is not judged by instructions: fixed startup work would
        # dilute the growth (Codex review), so the wall bound keeps such a run.
        return base_instr.get(m) is not None and all((n, m) in instrs for n in sizes)
    by_instr = {m for m in (modes or []) if instr_judged(m)}
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
            fac = factors.get((n, s), 1.0)
            if bud is None:
                problems.append("n=%d has no recorded budget" % n)
            elif not wall_judged:
                # No host factor can be measured here: only an overrun no load reaches fails.
                cat = b.p("uncounted_fail_factor")
                if times[(n, s)] > cat * bud:
                    problems.append("n=%d %.0fms > %g x budget %.0fms (catastrophic: judged on any host)"
                                    % (n, times[(n, s)], cat, bud))
            elif ntimes[(n, s)] > bud:
                if fac > 1.0:
                    problems.append("n=%d %.0fms (host x%.2f) > budget %.0fms"
                                    % (n, ntimes[(n, s)] * fac, fac, bud))
                else:
                    problems.append("n=%d %.0fms > budget %.0fms" % (n, times[(n, s)], bud))
        growth = []
        hbfail = []
        for n1, n2 in zip(sizes, sizes[1:]):
            limit = (n2 / n1) ** exp
            if mode_of(s) in by_instr:
                growth.append("by instructions")
            elif not wall_judged:
                pass                        # a wall ratio is not a verdict here (bug 315)
            elif (n1, s) in times and (n2, s) in times and (n1, s) not in bad and (n2, s) not in bad:
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
        # Allocation budgets: exact counts, judged once a budget is recorded (--measure).
        for n in sizes:
            hbud = b.hbbudget.get((shape, n, s))
            if hbud is not None and (n, s) in beats and beats[(n, s)] > hbud:
                hbfail.append("n=%d %d heartbeats > budget %d" % (n, beats[(n, s)], hbud))
        if hbfail:
            res.append(("FAIL-HB", "%s/%s %s [%s]" % (shape, s, "; ".join(hbfail), detail)))
        if problems:
            res.append(("FAIL", "%s/%s %s [%s]" % (shape, s, "; ".join(problems), detail)))
        elif not hbfail and not wall_judged:
            res.append(("NOT-JUDGED", "%s/%s [%s] wall growth and budget not judged: this host cannot"
                        " count instructions:u, CI decides%s" % (shape, s, detail,
                        "; heartbeats ok" if any((n, s) in beats for n in sizes) else "")))
        elif not hbfail:
            hf = [factors[(n, s)] for n in sizes if factors.get((n, s), 1.0) > 1.0]
            res.append(("ok", "%s/%s [%s] growth %s%s" % (shape, s, detail, " ".join(growth) or "-",
                        (" host x%.2f" % max(hf)) if hf else "")))
    # Retired instructions of each counted run: exact up to a few in a million, so their budget and
    # growth are judged without retries and whatever the host's load.
    for m in (modes or []):
        if m == "trace":
            why = next((b.excluded(shape, st) for st in TRACE_STAGES if b.excluded(shape, st)), None)
        else:
            why = b.excluded(shape, m)
        counted = [(n, instrs[(n, m)]) for n in sizes if (n, m) in instrs]
        if why is not None or not counted:
            continue
        problems = []
        for n, v in counted:
            ib = b.instr.get((shape, n, m))
            if ib is not None and v > ib:
                problems.append("n=%d %d instructions > budget %d" % (n, v, ib))
        # Above the mode's baseline (a one-line program), so a size-independent cost cannot dilute
        # the growth; a linear compile grows x10.0 - x10.3 from 10^4 to 10^5 above it.
        b0 = base_instr.get(m)
        ratios = []
        # Without its baseline the growth is not judged by instructions (fixed startup work would
        # dilute it); the wall bound judges the run instead (instr_judged above; Codex review).
        for (n1, v1), (n2, v2) in (zip(counted, counted[1:]) if b0 is not None else []):
            lim = (n2 / n1) ** b.p("instr_exp")
            r = max(0.0, v2 - b0) / max(v1 - b0, 1.0)
            ratios.append("x%.2f<=x%.2f" % (r, lim))
            if r > lim:
                problems.append("instructions grew x%.2f from %d to %d > x%.2f (n^%.2f), above a %.0fM baseline"
                                % (r, n1, n2, lim, b.p("instr_exp"), b0 / 1e6))
        detail = " ".join("%d:%.0fM" % (n, v / 1e6) for n, v in counted)
        if problems:
            res.append(("FAIL-INSTR", "%s/instructions:%s %s [%s]" % (shape, m, "; ".join(problems), detail)))
        else:
            res.append(("ok", "%s/instructions:%s [%s] growth %s" % (shape, m, detail, " ".join(ratios) or "-")))
    return res


def run_shape(runner, b, shape, sizes, modes, attempts, wall_judged=True):
    """Measure a shape up to ATTEMPTS times; keep the fastest run per (size, stage)."""
    stages = stages_of(modes)
    times, beats, bad, ntimes, factors, instrs = {}, {}, {}, {}, {}, {}
    for attempt in range(1, attempts + 1):
        for n in sizes:
            t, h, e, ins, w = runner.measure(shape, n, modes)
            for s, v in t.items():
                times[(n, s)] = min(v, times.get((n, s), v))
                # The budget is judged on the attempt with the least time per host factor.
                f = host_factor(b, shape, n, mode_of(s), w, ins)
                if (n, s) not in ntimes or v / f < ntimes[(n, s)]:
                    ntimes[(n, s)], factors[(n, s)] = v / f, f
            for s, v in h.items():
                beats[(n, s)] = min(v, beats.get((n, s), v))
            for m, v in ins.items():
                # Exact counts: the first attempt's stands (a retry for a wall-clock bound must not
                # replace an instruction overrun with a later count; Codex review).
                instrs.setdefault((n, m), v)
            # A failure in ANY attempt stands: a wrong result or a refused compile is never
            # noise, and a later attempt's time does not make it a measurement.
            for s, v in e.items():
                bad.setdefault((n, s), v)
        base = {m: runner.baseline(m) for m in modes if m != "trace"}
        if "trace" in modes:
            runner.baseline("trace")
        base_instr = {m: runner.base_instr.get(m) for m in modes}
        for m, v in base.items():
            if v is None:                   # no baseline, no growth judgement: not a pass
                for n in sizes:
                    bad.setdefault((n, m), "its baseline (a one-line program) did not compile")
        verdicts = judge(b, shape, sizes, stages, times, beats, bad,
                         {m: v for m, v in base.items() if v is not None}, ntimes, factors,
                         instrs, modes, base_instr, wall_judged)
        wall_fail = any(v == "FAIL" for v, _ in verdicts)
        # Heartbeat and instruction failures are deterministic: no retry changes them. A compile
        # that failed is not noise, and a run killed at its ceiling twice (bounded.sh already re-ran
        # it alone with twice the time) is over its budget by more than the ceiling factor: neither
        # is retried again.
        hard = bool(bad) or any(v in ("FAIL-HB", "FAIL-INSTR") for v, _ in verdicts)
        if not wall_fail or hard or attempt == attempts:
            return verdicts, attempt
    return verdicts, attempts


def controls(workdir):
    """Negative controls of the judge (bug 294), on a synthetic record: a host factor rescues a
    stage only within norm_cap, never an instruction or heartbeat overrun. [(ok, text)]."""
    path = os.path.join(workdir, "controls.tsv")
    with open(path, "w") as f:
        f.write("param\tnorm_cap\t2\nparam\tfloor_ms\t1\nshape\tassignments\t10,100\ttrace\n"
                + "".join("budget\tassignments\t%d\t%s\t100\n" % (n, st)
                          for n in (10, 100) for st in TRACE_STAGES)
                + "hbbudget\tassignments\t100\tlower\t1000\n"
                + "instr\tassignments\t100\ttrace\t1000000\n"
                + "rate\tassignments\t100\ttrace\t100.0\n")
    b = Budgets(path)
    sizes, stages = [10, 100], TRACE_STAGES

    def verdicts(t100, slow100, hb100=500.0, ins100=900000.0, t10=10.0, counted=True, base0=0.0,
                 ins10=100000.0, wall=True):
        # The run at 100 took `slow100` times the recorded 100 ms per 10^9 instructions: its host
        # factor comes from host_factor, as in a real judgment.
        f = host_factor(b, "assignments", 100, "trace", {"trace": 100.0 * slow100 * ins100 / 1e9},
                        {"trace": ins100})
        times = {(n, st): (t10 if n == 10 else t100) for n in sizes for st in stages}
        factors = {(n, st): (1.0 if n == 10 else f) for n in sizes for st in stages}
        ntimes = {k: v / factors[k] for k, v in times.items()}
        beats = {(n, st): (100.0 if n == 10 else hb100) for n in sizes for st in stages}
        instrs = {(10, "trace"): ins10, (100, "trace"): ins100} if counted else {}
        return {v for v, _ in judge(b, "assignments", sizes, stages, times, beats, {}, {}, ntimes,
                                    factors, instrs, ["trace"], {"trace": base0}, wall)}
    out = []
    out.append(("FAIL" in verdicts(150.0, 1.0), "CONTROL: a stage over its budget on an unloaded host fails"))
    out.append(("FAIL" not in verdicts(150.0, 2.0), "CONTROL: the same time on a host running at half speed passes"))
    out.append(("FAIL" in verdicts(250.0, 2.0), "CONTROL: a stage over norm_cap x its budget fails however slow the host"))
    out.append(("FAIL" in verdicts(250.0, 8.0),
                "CONTROL: a host 8 times slower is credited norm_cap (2), not 8: 2.5 x the budget fails"))
    out.append(("FAIL" in verdicts(150.0, 0.5), "CONTROL: a host faster than at --measure is credited nothing"))
    out.append(("FAIL-INSTR" in verdicts(50.0, 2.0, ins100=1200000.0),
                "CONTROL: a run over its instruction budget fails, host factor or not"))
    out.append(("FAIL-HB" in verdicts(50.0, 2.0, hb100=1500.0),
                "CONTROL: a stage over its heartbeat budget fails, host factor or not"))
    # Growth by instructions: a planted quadratic fails however its wall time behaves, and a wall
    # ratio alone no longer fails a counted run (the instructions grew linearly).
    # Within its instruction budget (1000000), so only the growth check can fail it.
    out.append(("FAIL-INSTR" in verdicts(50.0, 1.0, ins10=10000.0, ins100=600000.0),
                "CONTROL: instructions growing x60 for x10 (a planted quadratic) fail"))
    out.append(("FAIL" not in verdicts(95.0, 1.0, t10=1.5),
                "CONTROL: a counted run whose wall time grew x63 at x9 instructions passes (caches, not work)"))
    out.append(("FAIL" in verdicts(95.0, 1.0, t10=1.5, counted=False),
                "CONTROL: the same wall growth fails a run nobody counted (the wall bound still holds there)"))
    out.append(("FAIL" in verdicts(95.0, 1.0, t10=1.5, base0=None),
                "CONTROL: a counted run whose baseline was not counted keeps the wall bound"))
    # A host that cannot count (bug 315): wall verdicts are not judged there, never passed, and CI
    # must count. The judge is the real one; only the host's ability is set.
    uncounted = dict(counted=False, wall=False)
    v = verdicts(95.0, 1.0, t10=1.5, **uncounted)
    out.append((v == {"NOT-JUDGED"},
                "CONTROL: uncounted off CI, wall growth x63 (which fails a counting host's uncounted run)"
                " is NOT-JUDGED, not FAIL"))
    v = verdicts(50.0, 1.0, **uncounted)
    out.append((v == {"NOT-JUDGED"},
                "CONTROL: uncounted off CI, a stage within its budget and linear is NOT-JUDGED, not PASS"))
    v = verdicts(150.0, 1.0, **uncounted)
    out.append((v == {"NOT-JUDGED"},
                "CONTROL: uncounted off CI, a stage over its budget but under uncounted_fail_factor x it"
                " is NOT-JUDGED (its host speed is unknown)"))
    out.append(("FAIL" in verdicts(1500.0, 1.0, **uncounted),
                "CONTROL: uncounted off CI, a stage over uncounted_fail_factor (10) x its budget fails"))
    out.append(("FAIL-HB" in verdicts(50.0, 1.0, hb100=1500.0, **uncounted),
                "CONTROL: uncounted off CI, a heartbeat overrun still fails (exact counts)"))
    out.append((host_policy(False, {"GITHUB_ACTIONS": "true", "CI": "true"}) == "refused"
                and host_policy(False, {"CI": "1"}) == "refused",
                "CONTROL: uncounted on CI (GITHUB_ACTIONS=true, or CI=true/1) is refused: the gate FAILS"))
    out.append((host_policy(False, {}) == "deferred" and host_policy(False, {"CI": "false"}) == "deferred",
                "CONTROL: uncounted off CI is deferred to CI"))
    out.append((host_policy(True, {"GITHUB_ACTIONS": "true"}) == "counted" and host_policy(True, {}) == "counted",
                "CONTROL: a counting host is judged as before, on CI or off it"))
    counted_runs = [verdicts(150.0, 1.0), verdicts(150.0, 2.0), verdicts(95.0, 1.0, t10=1.5),
                    verdicts(95.0, 1.0, t10=1.5, counted=False), verdicts(50.0, 1.0)]
    out.append((all("NOT-JUDGED" not in v for v in counted_runs) and verdicts(50.0, 1.0) == {"ok"},
                "CONTROL: a counting host never reports NOT-JUDGED, and passes a stage within its bounds"))
    out.append((exit_status(5, 0, 3) == NOT_JUDGED_RC and exit_status(5, 1, 3) == 1
                and exit_status(5, 0, 0) == 0 and exit_status(0, 0, 0) == 1,
                "CONTROL: NOT-JUDGED exits %d (not 0); any failure exits 1; nothing judged exits 1"
                % NOT_JUDGED_RC))
    return out


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
    # Scaling to the session's quietest rate removes the variation between runs, not load that
    # lasts the whole session (Codex review): budgets measured on a busy host would be inflated by
    # it, so --measure refuses a host whose load is above measure_max_load.
    try:
        load = os.getloadavg()[0]
    except OSError:
        load = None
    if load is not None and load > b.p("measure_max_load") and not os.environ.get("CONCRETE_SCALING_MEASURE_ANY_LOAD"):
        print("  REFUSED: load average %.1f is above measure_max_load %g; budgets measured now would"
              " carry the load. Re-run when the host is quiet (CONCRETE_SCALING_MEASURE_ANY_LOAD=1"
              " overrides, and the stamp records the load)." % (load, b.p("measure_max_load")))
        return 2
    new, newhb, newinstr, newrate = {}, {}, {}, {}
    if not runner.perf:
        print("  note: perf cannot count instructions:u on this host; no instruction budgets or rates"
              " are recorded, and the wall budgets are judged without host normalization")
    for shape, sizes, modes in shapes:
        if b.excluded(shape, "*") is not None:
            print("  skip %s (excluded: %s)" % (shape, b.excluded(shape, "*")))
            continue
        samples, hbs, isamples, rsamples = {}, {}, {}, {}
        for _ in range(runs):
            for n in sizes:
                t, h, e, ins, w = runner.measure(shape, n, modes)
                if e:
                    print("  FAIL %s n=%d: %s" % (shape, n, e))
                    return 1
                for s, v in t.items():
                    m = mode_of(s)
                    rate = (w[m] / (ins[m] / 1e9)) if ins.get(m) and w.get(m) else None
                    samples.setdefault((n, s), []).append((v, rate))
                for s, v in h.items():
                    hbs[(n, s)] = max(v, hbs.get((n, s), v))
                for m, v in ins.items():
                    isamples.setdefault((n, m), []).append(v)
                    rsamples.setdefault((n, m), []).append(w[m] / (v / 1e9))
        for (n, s), v in sorted(hbs.items()):
            if b.excluded(shape, s) is None:
                newhb[(shape, n, s)] = math.ceil(b.p("hb_budget_factor") * v)
        for (n, m), vs in sorted(isamples.items()):
            newinstr[(shape, n, m)] = math.ceil(b.p("instr_budget_factor") * statistics.median(vs))
            # The reference rate is the session's quietest run of this compile: the rate the
            # budgets below are scaled to.
            newrate[(shape, n, m)] = round(min(rsamples[(n, m)]), 1)
            print("  %s n=%d %s: median %.0fM instructions, %.1f ms per 10^9 -> instruction budget %dM"
                  % (shape, n, m, statistics.median(vs) / 1e6, newrate[(shape, n, m)],
                     newinstr[(shape, n, m)] / 1e6))
        for (n, s), vs in sorted(samples.items()):
            # Each run's time at the quietest rate of the session (a run whose compile went at
            # twice that rate is counted at half its time); unscaled where nothing was counted.
            ref = newrate.get((shape, n, mode_of(s)))
            scaled = [v * (ref / r) if (ref and r) else v for v, r in vs]
            # Counted runs: the fastest scaled run, as the judge keeps the fastest attempt (a scaled
            # run still carries the load a stage felt beyond the whole compile's); else the median.
            med = min(scaled) if ref else statistics.median(scaled)
            bud = max(math.ceil(b.p("budget_factor") * med), math.ceil(med + b.p("budget_slack_ms")))
            new[(shape, n, s)] = bud
            print("  %s n=%d %s: %.0fms of %s (raw %s) -> budget %dms"
                  % (shape, n, s, med, ",".join("%.0f" % v for v in scaled),
                     ",".join("%.0f" % v for v, _r in vs), bud))
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
        if line.split("\t")[0] in ("hbbudget", "instr", "rate"):
            continue
        if line.startswith("# measured:") and " -- shapes: " in line:
            # An earlier stamp gives up the shapes measured now, so each shape has one stamp.
            head, names = line.split(" -- shapes: ", 1)
            keep = [x for x in names.split(", ") if x not in measured]
            if not keep:
                continue
            line = head + " -- shapes: " + ", ".join(keep)
        out.append(line)
    try:
        load = "load average %.1f at the end" % os.getloadavg()[0]
    except OSError:
        load = "load average unknown"
    scaled = ("fastest run, each scaled to the quietest instruction rate of the session" if runner.perf
              else "median, unscaled (no instruction counts)")
    stamp = ("# measured: commit %s on %s, %s, %d runs (%s; %s), "
             "budget = max(%g x t, t + %gms)%s") % (
        git_identity(root), host_identity(), time.strftime("%Y-%m-%d"), runs, scaled, load,
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
    # The exact budgets and the host rates, kept for the shapes not measured now.
    for kind, fresh, kept, fmt in (("hbbudget", newhb, b.hbbudget, "%d"), ("instr", newinstr, b.instr, "%d"),
                                   ("rate", newrate, b.rate, "%.1f")):
        oldk = {k: v for k, v in kept.items() if k[0] not in measured}
        allk = set(oldk) | set(fresh)
        idx = stages_of(MODES)
        for key in sorted(allk, key=lambda k: (order.get(k[0], 999), k[1], idx.index(k[2]) if k[2] in idx else 99)):
            v = fresh.get(key, oldk.get(key))
            out.append(("%s\t%s\t%d\t%s\t" + fmt) % (kind, key[0], key[1], key[2], v))
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
    runner = Runner(a.compiler, a.bounded, b, workdir, perf=perf_ok())
    try:
        if a.measure:
            return do_measure(runner, b, shapes, a.root)
        meas = [l for l in b.header if l.startswith("# measured:")]
        print("=== compile-time scaling (R-0550): per-stage budgets; growth <= n^%.2f in time, n^%.2f in "
              "allocations ===" % (b.p("exp"), b.p("hb_exp")))
        for m in meas or ["# NOT MEASURED"]:
            print("  budgets %s" % m[2:])
        policy = host_policy(runner.perf, os.environ)
        if policy == "deferred":
            print("  note: perf cannot count instructions:u on this host: wall growth and wall budgets are"
                  " NOT JUDGED here (CI decides them; bug 315); heartbeats, compile results, the run"
                  " ceiling and overruns of %g x a budget are" % b.p("uncounted_fail_factor"))
        npass = nfail = nexcl = nnot = 0
        for ok, text in controls(workdir):
            if ok:
                npass += 1
                print("  ok   " + text)
            else:
                nfail += 1
                print("  FAIL " + text)
        if policy == "refused":
            # The verdicts a dev host defers are decided here, so CI may not defer them again.
            nfail += 1
            print("  FAIL CI host must count instructions:u: perf is missing or perf_event_paranoid is"
                  " above 2 on this runner, and the scaling gate's growth and budget verdicts are decided"
                  " on CI (bug 315). Fix the runner; no shape was judged.")
            shapes = []
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
            verdicts, used = run_shape(runner, b, shape, sizes, modes, attempts,
                                       wall_judged=(policy == "counted"))
            for v, text in verdicts:
                if v == "ok":
                    npass += 1
                    print("  ok   " + text)
                elif v == "NOT-JUDGED":
                    nnot += 1
                    print("  NOT-JUDGED " + text)
                elif v == "EXCLUDED":
                    nexcl += 1
                    print("  EXCLUDED " + text)
                else:
                    nfail += 1
                    print("  FAIL " + text + ("  (heartbeats: deterministic, not retried)" if v == "FAIL-HB" else
                                              "  (instructions: deterministic, not retried)" if v == "FAIL-INSTR" else ""))
            print("  -- %s: %d attempt(s), %.1fs" % (shape, used, time.monotonic() - t0))
        print()
        print("check_compile_scaling: PASS=%d FAIL=%d NOT-JUDGED=%d EXCLUDED=%d" % (npass, nfail, nnot, nexcl))
        rc = exit_status(npass, nfail, nnot)
        if rc == NOT_JUDGED_RC:
            print("NOT JUDGED HERE: %d stage verdict(s) need instruction counts this host cannot make;"
                  " CI decides them (judged_on: ci-counted). This run did not pass; exit %d."
                  % (nnot, NOT_JUDGED_RC))
        return rc
    finally:
        if not a.keep:
            shutil.rmtree(workdir, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
