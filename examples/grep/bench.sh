#!/usr/bin/env bash
#
# bench.sh -- benchmark Concrete cgrep vs C cgrep_c
#
# Steps:
#   1. Generate a 100k-line test file with mixed text
#   2. Compile the C version with -O2
#   3. Build the Concrete version with `concrete build`
#   4. Run both on the same search pattern 5 times each
#   5. Report average wall-clock times
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

TESTFILE="bench_testdata.txt"
C_SRC="bench_c.c"
C_BIN="cgrep_c"
CON_BIN="cgrep"
PATTERN="error"
RUNS=5

# ── 1. Generate test file ────────────────────────────────────────────
echo "==> Generating $TESTFILE (100000 lines)..."
if [ ! -f "$TESTFILE" ]; then
    python3 -c "
import random, string
words = ['the', 'quick', 'brown', 'fox', 'error', 'warning', 'debug',
         'info', 'jumps', 'over', 'lazy', 'dog', 'connection', 'timeout',
         'ERROR', 'failed', 'success', 'retry', 'null', 'pointer',
         'exception', 'traceback', 'segfault', 'ok', 'result']
with open('$TESTFILE', 'w') as f:
    for _ in range(100000):
        n = random.randint(4, 15)
        line = ' '.join(random.choice(words) for _ in range(n))
        f.write(line + '\n')
"
    echo "    done."
else
    echo "    (already exists, skipping)"
fi

# ── 2. Compile C version ─────────────────────────────────────────────
echo "==> Compiling C version ($C_SRC -> $C_BIN) with -O2..."
cc -O2 -o "$C_BIN" "$C_SRC"
echo "    done."

# ── 3. Build Concrete version ────────────────────────────────────────
echo "==> Building Concrete version..."
CONCRETE_COMPILER="$(cd "$SCRIPT_DIR/../.." && pwd)/.lake/build/bin/concrete"
(cd "$SCRIPT_DIR" && "$CONCRETE_COMPILER" build 2>&1) || {
    echo "    WARNING: concrete build failed; skipping Concrete benchmarks."
    CON_BIN=""
}
echo "    done."

# ── Helper: average time over N runs ─────────────────────────────────
#   Usage: bench_avg <label> <command...>
#   Prints each run's time and the average.
bench_avg() {
    local label="$1"; shift
    echo ""
    echo "--- $label ($RUNS runs, pattern='$PATTERN') ---"
    local total=0
    for i in $(seq 1 $RUNS); do
        # Use bash built-in TIMEFORMAT; redirect program stdout to /dev/null
        local t
        t=$( { TIMEFORMAT='%R'; time "$@" > /dev/null; } 2>&1 )
        printf "  run %d: %s s\n" "$i" "$t"
        total=$(echo "$total + $t" | bc)
    done
    local avg
    avg=$(echo "scale=4; $total / $RUNS" | bc)
    printf "  avg:   %s s\n" "$avg"
}

# ── 4 & 5. Run benchmarks ────────────────────────────────────────────
echo ""
echo "==========================================="
echo " Benchmark: plain search (pattern='$PATTERN')"
echo "==========================================="

bench_avg "C (cgrep_c)" ./"$C_BIN" "$PATTERN" "$TESTFILE"

if [ -n "$CON_BIN" ] && [ -x "$CON_BIN" ]; then
    bench_avg "Concrete (cgrep)" ./"$CON_BIN" "$PATTERN" "$TESTFILE"
else
    echo ""
    echo "--- Concrete (cgrep): SKIPPED (binary not available) ---"
fi

echo ""
echo "==========================================="
echo " Benchmark: case-insensitive + line numbers"
echo "==========================================="

bench_avg "C (cgrep_c) -i -n" ./"$C_BIN" -i -n "$PATTERN" "$TESTFILE"

if [ -n "$CON_BIN" ] && [ -x "$CON_BIN" ]; then
    bench_avg "Concrete (cgrep) -i -n" ./"$CON_BIN" -i -n "$PATTERN" "$TESTFILE"
else
    echo ""
    echo "--- Concrete (cgrep) -i -n: SKIPPED ---"
fi

echo ""
echo "==========================================="
echo " Benchmark: count only (-c)"
echo "==========================================="

bench_avg "C (cgrep_c) -c" ./"$C_BIN" -c "$PATTERN" "$TESTFILE"

if [ -n "$CON_BIN" ] && [ -x "$CON_BIN" ]; then
    bench_avg "Concrete (cgrep) -c" ./"$CON_BIN" -c "$PATTERN" "$TESTFILE"
else
    echo ""
    echo "--- Concrete (cgrep) -c: SKIPPED ---"
fi

echo ""
echo "==> Benchmark complete."
