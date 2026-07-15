#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Phase 6C #2: anti-superlinear compiler-complexity guard.
#
# Telemetry (6C #1) says what happened; this gate fails when a compiler pass
# quietly goes quadratic-or-worse on ordinary generated programs. For each program
# FAMILY (many functions, large array literals, wide match chains, many
# statements) it compiles the same shape at growing sizes and checks that BOTH
# compile time and emitted-IR size grow sub-quadratically in the size parameter.
#
# Method: compare the growth RATIO across a wide size range (8x), not absolute
# wall-clock. Over an 8x size increase a linear pass costs ~8x; a quadratic pass
# costs ~64x. A family passes only if its growth stays below size_ratio ** EXP
# (EXP=1.7 => ~34x at 8x): well below quadratic, well above linear+noise, so it
# catches a reintroduced quadratic/cubic renderer/collector (the bug-027 family)
# without flaking on constant-factor jitter. Timing is via python (portable; macOS
# `date` has no %N) and takes the MIN of repeated runs. Both compile time AND
# emitted-IR size are checked.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
command -v python3 >/dev/null || { echo "error: python3 required" >&2; exit 2; }

python3 - "$COMPILER" <<'PY'
import subprocess, sys, tempfile, time, os

CC = sys.argv[1]
SIZES = [25, 50, 100, 200]          # 8x range (min -> max)
RUNS  = 2                            # min of RUNS timings per point (noise floor)
PASS = 0; FAIL = 0

# Growth-exponent ceiling: a family passes if time/IR growth over the 8x size
# range stays below size_ratio ** EXP. EXP=1.7 (=> ~34x at 8x) strictly catches any
# quadratic (64x) or worse regression while tolerating linear+noise. This is what
# caught the SSAVerify dominator cubic (the bug-027 family — re-attributed by the
# isolation from EmitSSA to SSAVerify dominators/predecessors): before the fix,
# `match` grew ~35x over this range (FAIL); after the Gauss-Seidel dominator
# convergence fix it grows ~6x (PASS).
#
# Regime note: these sizes (<=200) are the practical range and are where the cubic
# manifested. SSAVerify dominance is still ~O(N^2) in block count at much larger N
# (dom SETS are inherently O(N^2) for a long block chain); driving it near-linear
# needs the idom/CHK SSAVerify rewrite, tracked as a follow-up and guarded by
# scripts/tests/check_ssa_verify_agreement.sh. This gate catches a regression back
# toward the cubic; it does not (yet) exercise the large-N O(N^2) tail.
EXP = 1.7

def gen(fam, n):
    if fam == "functions":
        # n functions, each calls the next; main calls f0. Exercises resolve/check/
        # mono/lower/emit over many decls.
        fns = "".join(f"fn f{i}(x: i32) -> i32 {{ return f{i+1}(x) + 1; }}\n" for i in range(n))
        fns += f"fn f{n}(x: i32) -> i32 {{ return x; }}\n"
        return "mod m {\n" + fns + "fn main() -> Int { return f0(1) as Int; }\n}\n"
    if fam == "array":
        elems = ", ".join("1" for _ in range(n))
        return ("mod m { fn main() -> Int { let a: [i32; %d] = [%s]; return a[0] as Int; } }\n"
                % (n, elems))
    if fam == "match":
        arms = "".join(f"{i} => {i}, " for i in range(n))
        return ("mod m { fn main() -> Int { let x: Int = 0; let r: Int = match x { %s _ => -1 }; return r; } }\n"
                % arms)
    if fam == "statements":
        stmts = "".join(f"acc = acc + {i % 7};\n" for i in range(n))
        return "mod m { fn main() -> Int { let mut acc: Int = 0;\n" + stmts + "return acc; } }\n"
    raise ValueError(fam)

def measure(src):
    with tempfile.NamedTemporaryFile("w", suffix=".con", delete=False) as f:
        f.write(src); path = f.name
    try:
        best = None; irsize = 0
        for _ in range(RUNS):
            t0 = time.time()
            r = subprocess.run([CC, path, "--emit-llvm"], capture_output=True, text=True)
            dt = time.time() - t0
            if r.returncode != 0:
                return None, None, (r.stderr or r.stdout)[:200]
            irsize = len(r.stdout)
            best = dt if best is None else min(best, dt)
        return best, irsize, None
    finally:
        os.unlink(path)
        try: os.unlink(path + ".bin")
        except OSError: pass

for fam in ["functions", "array", "match", "statements"]:
    pts = []
    err = None
    for n in SIZES:
        t, ir, e = measure(gen(fam, n))
        if e is not None: err = f"size {n}: {e}"; break
        pts.append((n, t, ir))
    if err:
        print(f"  FAIL {fam}: compile failed ({err})"); FAIL += 1; continue
    (n0, t0, ir0) = pts[0]; (n1, t1, ir1) = pts[-1]
    size_ratio = n1 / n0
    # Guard against sub-millisecond noise: require a meaningful floor time.
    t0f = max(t0, 0.005)
    time_ratio = t1 / t0f
    ir_ratio = (ir1 / ir0) if ir0 else 1.0
    limit = size_ratio ** EXP
    okt = time_ratio <= limit
    oki = ir_ratio <= limit
    detail = (f"size x{size_ratio:.0f}: time x{time_ratio:.1f}, IR x{ir_ratio:.1f} "
              f"(limit x{limit:.0f}, exp {EXP}) [{'/'.join(f'{n}:{t*1000:.0f}ms' for n,t,_ in pts)}]")
    if okt and oki:
        print(f"  ok   {fam} sub-linear-ish — {detail}"); PASS += 1
    else:
        which = "time" if not okt else "IR-size"
        print(f"  FAIL {fam} SUPERLINEAR {which} — {detail}"); FAIL += 1

print()
print(f"check_compiler_complexity: PASS={PASS} FAIL={FAIL}")
sys.exit(1 if FAIL else 0)
PY
