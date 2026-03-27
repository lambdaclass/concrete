#!/usr/bin/env bash
#
# bench.sh -- benchmark Concrete JSON parser vs C JSON parser
#
# Steps:
#   1. Compile the C version with -O2
#   2. Build the Concrete version (emit LLVM IR, compile with clang -O2)
#   3. Run both 5 times each
#   4. Report average wall-clock times
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

C_SRC="$SCRIPT_DIR/bench_c.c"
C_BIN="$SCRIPT_DIR/json_c"
CON_SRC="$SCRIPT_DIR/main.con"
CON_BIN="$SCRIPT_DIR/json_con"
RUNS=5

# ── 1. Compile C version ─────────────────────────────────────────────
echo "==> Compiling C version with -O2..."
cc -O2 -o "$C_BIN" "$C_SRC"
echo "    done."

# ── 2. Build Concrete version ────────────────────────────────────────
echo "==> Building Concrete version (emit-llvm + clang -O2)..."
CON_OK=1
cd "$ROOT"
if ~/.elan/bin/lake env concrete "$CON_SRC" --emit-llvm > /tmp/json_con.ll 2>&1; then
    if clang /tmp/json_con.ll -o "$CON_BIN" -O2 -Wno-override-module 2>/dev/null; then
        echo "    done."
    else
        echo "    WARNING: clang compilation failed; skipping Concrete benchmarks."
        CON_OK=0
    fi
else
    echo "    WARNING: concrete emit-llvm failed; skipping Concrete benchmarks."
    CON_OK=0
fi

# ── Helper: average time over N runs ─────────────────────────────────
bench_avg() {
    local label="$1"; shift
    echo ""
    echo "--- $label ($RUNS runs) ---"
    local total=0
    for i in $(seq 1 $RUNS); do
        local t
        t=$( { TIMEFORMAT='%R'; time "$@" > /dev/null; } 2>&1 )
        printf "  run %d: %s s\n" "$i" "$t"
        total=$(echo "$total + $t" | bc)
    done
    local avg
    avg=$(echo "scale=4; $total / $RUNS" | bc)
    printf "  avg:   %s s\n" "$avg"
}

# ── 3 & 4. Run benchmarks ────────────────────────────────────────────
echo ""
echo "==========================================="
echo " JSON Parser Benchmark (27 test cases)"
echo "==========================================="

bench_avg "C (-O2)" "$C_BIN"

if [ "$CON_OK" -eq 1 ] && [ -x "$CON_BIN" ]; then
    bench_avg "Concrete (clang -O2)" "$CON_BIN"
else
    echo ""
    echo "--- Concrete: SKIPPED (binary not available) ---"
fi

echo ""
echo "==> Benchmark complete."

# ── Cleanup ──────────────────────────────────────────────────────────
rm -f "$C_BIN" "$CON_BIN" /tmp/json_con.ll
