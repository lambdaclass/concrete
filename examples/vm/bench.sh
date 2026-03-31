#!/usr/bin/env bash
#
# bench.sh — compare Concrete vs C bytecode-VM performance on fib(35)
#
# Compiles both versions, runs each 5 times, and reports average wall-clock
# time for the fib(35) VM execution (as reported by the program itself).

set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$DIR/../.." && pwd)"
COMPILER="$ROOT/.lake/build/bin/concrete"
RUNS=5

C_SRC="$DIR/bench_c.c"
C_BIN="$DIR/bench_c"
CON_SRC="$DIR/main.con"
CON_BIN="$DIR/convm"

# ── Build ────────────────────────────────────────────────────────────────

echo "=== Building C version (cc -O2) ==="
cc -O2 -o "$C_BIN" "$C_SRC"

echo "=== Building Concrete version ==="
if [ ! -x "$COMPILER" ]; then
    echo "error: Concrete compiler not found at $COMPILER"
    echo "       Run 'make build' from the repo root first."
    exit 1
fi
"$COMPILER" "$CON_SRC" -o "$CON_BIN"

# ── Helpers ──────────────────────────────────────────────────────────────

# Extract the "vm time: NNN ms" value from program output
extract_ms() {
    # Matches "vm time: 123 ms" and prints just the number
    sed -n 's/.*vm time: \([0-9]*\) ms.*/\1/p'
}

# ── Run benchmarks ───────────────────────────────────────────────────────

echo ""
echo "=== Running fib(35) benchmark ($RUNS iterations each) ==="
echo ""

c_total=0
con_total=0

for i in $(seq 1 $RUNS); do
    c_ms=$("$C_BIN" | extract_ms)
    con_ms=$("$CON_BIN" | extract_ms)
    printf "  run %d:  C = %4d ms   Concrete = %4d ms\n" "$i" "$c_ms" "$con_ms"
    c_total=$((c_total + c_ms))
    con_total=$((con_total + con_ms))
done

c_avg=$((c_total / RUNS))
con_avg=$((con_total / RUNS))

echo ""
echo "=== Results (average of $RUNS runs) ==="
echo "  C (cc -O2):   ${c_avg} ms"
echo "  Concrete:     ${con_avg} ms"

if [ "$c_avg" -gt 0 ]; then
    # Compute ratio as integer percentage (Concrete / C * 100)
    ratio=$(( (con_avg * 100) / c_avg ))
    echo "  Ratio:        Concrete is ${ratio}% of C"
fi

# ── Cleanup ──────────────────────────────────────────────────────────────

rm -f "$C_BIN" "$CON_BIN"
