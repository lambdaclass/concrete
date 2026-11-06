#!/usr/bin/env bash
# Bug 178 gate: `match` lowering must scale near-linearly in the number of arms.
#
# A 1000-arm `match` spent 8.45 s in the `lower` stage (Lower + SSAVerify + SSACleanup)
# against 119 ms at 250 arms: ~n^3.1. The cause was list-shaped data structures in the
# trust pass and the cleanup pass (dominator SETS as `List String` intersected with
# `filter`/`contains`, per-block `find?`/`filter` over every block, per-block lists of
# every strict dominator's definitions) plus O(blocks) appends in Lower. See
# docs/bugs/178_superlinear_match_lowering.md.
#
# METRIC. Never wall time. The primary metric is the `lower` stage's Lean HEARTBEATS
# (small allocations) from `--emit-trace-json` `alloc_heartbeats`: deterministic for a
# given binary and input, independent of machine load, and available on every host.
# Where `perf` can count `instructions:u` (Linux with perf_event_paranoid <= 2), the
# retired-instruction count of the whole `--emit-llvm` compile is checked as well; a
# host that cannot count instructions says so and still runs the heartbeat leg.
#
# For each program shape the gate compiles 250/500/1000/2000 arms and fits the
# log-log least-squares slope. It fails above EXP=1.3 (before the fix: ~2.7-3.1 in
# instructions; after: ~1.0). A shape that does not compile, or a trace without
# `alloc_heartbeats.lower`, FAILS — an unmeasured run is not a pass.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="${CONCRETE_SCALING_COMPILER:-.lake/build/bin/concrete}"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
command -v python3 >/dev/null || { echo "error: python3 required" >&2; exit 2; }

python3 - "$COMPILER" <<'PY'
import json, math, os, re, shutil, subprocess, sys, tempfile

CC = sys.argv[1]
SIZES = [250, 500, 1000, 2000]
EXP = 1.3
PASS = 0; FAIL = 0

def gen(shape, n):
    if shape == "value-match":
        # The bug-178 reproducer: a value-producing match with a constant scrutinee.
        arms = "".join("%d => %d, " % (i, i) for i in range(n))
        return ("mod m { fn main() -> Int { let x: Int = 0; let r: Int = match x { %s _ => -1 }; "
                "return r; } }\n" % arms)
    if shape == "assigning-match":
        # Unknown scrutinee (cleanup cannot fold the chain away), every arm assigns a
        # variable: exercises the merge-variable reconciliation and the arm-wide phi.
        arms = "".join("%d => { acc = acc + %d; }, " % (i, i % 7 + 1) for i in range(n))
        return ("mod m { fn pick(x: Int) -> Int { let mut acc: Int = 0; match x { %s _ => { acc = 0; } } "
                "return acc; } fn main() -> Int { return pick(3); } }\n" % arms)
    raise ValueError(shape)

def slope(pts):
    xs = [math.log(n) for n, _ in pts]; ys = [math.log(v) for _, v in pts]
    mx = sum(xs) / len(xs); my = sum(ys) / len(ys)
    return sum((x - mx) * (y - my) for x, y in zip(xs, ys)) / sum((x - mx) ** 2 for x in xs)

def perf_ok():
    if not shutil.which("perf"):
        return False
    try:
        p = subprocess.run(["perf", "stat", "-e", "instructions:u", "-x,", "true"],
                           capture_output=True, text=True, timeout=30)
    except (OSError, subprocess.TimeoutExpired):
        return False
    return p.returncode == 0 and instr_of(p.stderr) is not None

def instr_of(text):
    for line in text.splitlines():
        parts = line.split(",")
        if len(parts) >= 3 and parts[2].strip() == "instructions:u" and re.match(r"^\d+$", parts[0].strip()):
            return int(parts[0].strip())
    return None

PERF = perf_ok()
print("=== match lowering scaling (bug 178): slope of log(cost) vs log(arms) must be <= %.1f ===" % EXP)
if not PERF:
    print("  note: perf cannot count instructions:u on this host; heartbeat leg only")

tmp = tempfile.mkdtemp()
try:
    for shape in ["value-match", "assigning-match"]:
        beats, instrs, err = [], [], None
        for n in SIZES:
            path = os.path.join(tmp, "%s_%d.con" % (shape, n))
            with open(path, "w") as f:
                f.write(gen(shape, n))
            r = subprocess.run([CC, path, "--emit-trace-json"], capture_output=True, text=True, timeout=600)
            if r.returncode != 0:
                err = "size %d: --emit-trace-json failed: %s" % (n, (r.stderr or r.stdout)[:200]); break
            try:
                hb = json.loads(r.stdout)["alloc_heartbeats"]["lower"]
            except (ValueError, KeyError, TypeError):
                err = "size %d: trace has no alloc_heartbeats.lower" % n; break
            # A zero or non-integer count is no measurement: four zeros would fit slope 0.
            if not isinstance(hb, int) or isinstance(hb, bool) or hb <= 0:
                err = "size %d: alloc_heartbeats.lower is not a positive integer (%r)" % (n, hb); break
            beats.append((n, hb))
            if PERF:
                p = subprocess.run(["perf", "stat", "-e", "instructions:u", "-x,", "--", CC, path, "--emit-llvm"],
                                   capture_output=True, text=True, timeout=600)
                v = instr_of(p.stderr)
                # perf reports a count for a compile that FAILED too; only a successful
                # compile is a measurement of lowering.
                if p.returncode != 0:
                    err = "size %d: --emit-llvm failed under perf (rc %d)" % (n, p.returncode); break
                if v is None or v <= 0:
                    err = "size %d: perf counted nothing for --emit-llvm" % n; break
                instrs.append((n, v))
        if err:
            print("  FAIL %s: %s" % (shape, err)); FAIL += 1; continue
        s = slope(beats)
        detail = " ".join("%d:%d" % p for p in beats)
        if s <= EXP:
            print("  ok   %s lower-stage heartbeats slope %.2f [%s]" % (shape, s, detail)); PASS += 1
        else:
            print("  FAIL %s lower-stage heartbeats slope %.2f > %.1f [%s]" % (shape, s, EXP, detail)); FAIL += 1
        if PERF:
            s = slope(instrs)
            detail = " ".join("%d:%d" % p for p in instrs)
            if s <= EXP:
                print("  ok   %s --emit-llvm instructions:u slope %.2f [%s]" % (shape, s, detail)); PASS += 1
            else:
                print("  FAIL %s --emit-llvm instructions:u slope %.2f > %.1f [%s]" % (shape, s, EXP, detail)); FAIL += 1
finally:
    shutil.rmtree(tmp, ignore_errors=True)

print()
print("check_ssa_lowering_scaling: PASS=%d FAIL=%d" % (PASS, FAIL))
sys.exit(1 if FAIL or PASS == 0 else 0)
PY
