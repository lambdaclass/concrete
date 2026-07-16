#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

# Performance regression tracking for the Concrete compiler.
# Measures compile time, runtime, and binary/IR size for representative programs.
#
# Usage:
#   bash scripts/tests/test_perf.sh              # run and print results
#   bash scripts/tests/test_perf.sh --save       # run and save baseline to .perf-baseline
#   bash scripts/tests/test_perf.sh --compare    # run and compare against saved baseline
#
# This script does NOT fail on regressions — it reports them for human review.

COMPILER=".lake/build/bin/concrete"
TESTDIR="tests/programs"
TMPDIR_PERF=$(mktemp -d)
trap 'rm -rf "$TMPDIR_PERF"' EXIT

MODE="${1:-}"
BASELINE_FILE=".perf-baseline"
THRESHOLD_PCT=20  # warn if >20% regression

# Representative programs for benchmarking (mix of sizes and features)
BENCH_PROGRAMS=(
    "$TESTDIR/integration_stress_workload.con"
    "$TESTDIR/integration_compiler_stress.con"
    "$TESTDIR/integration_recursive_structures.con"
    "$TESTDIR/test_recursive_fibonacci.con"
    "$TESTDIR/test_loop_nested_three.con"
    "$TESTDIR/test_many_locals.con"
    "$TESTDIR/test_generic_chain.con"
)

# --- Timing helpers ---

time_ms() {
    # Returns elapsed wall time in milliseconds
    local start end
    start=$(date +%s%N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1e9))')
    "$@" > /dev/null 2>&1 || true
    end=$(date +%s%N 2>/dev/null || python3 -c 'import time; print(int(time.time()*1e9))')
    echo $(( (end - start) / 1000000 ))
}

# macOS doesn't have date +%s%N, use perl fallback
if ! date +%s%N > /dev/null 2>&1; then
    time_ms() {
        local start end
        start=$(perl -MTime::HiRes=time -e 'printf("%.0f\n", time()*1000)')
        "$@" > /dev/null 2>&1 || true
        end=$(perl -MTime::HiRes=time -e 'printf("%.0f\n", time()*1000)')
        echo $(( end - start ))
    }
fi

# --- Measurements ---

results=()

echo "=== Concrete Performance Measurements ==="
echo ""

for prog in "${BENCH_PROGRAMS[@]}"; do
    name=$(basename "$prog" .con)
    llpath="$TMPDIR_PERF/${name}.ll"
    binpath="$TMPDIR_PERF/${name}"

    # 1. Compile time (emit-llvm)
    compile_ms=$(time_ms "$COMPILER" "$prog" --emit-llvm)

    # 2. Get LLVM IR
    "$COMPILER" "$prog" --emit-llvm > "$llpath" 2>/dev/null || continue

    # 3. LLVM IR line count (proxy for code size)
    ir_lines=$(wc -l < "$llpath" | tr -d ' ')

    # 4. Build native binary
    if clang "$llpath" -o "$binpath" -Wno-override-module > /dev/null 2>&1; then
        # 5. Binary size
        bin_size=$(wc -c < "$binpath" | tr -d ' ')

        # 6. Runtime (average of 3 runs)
        total_runtime=0
        for i in 1 2 3; do
            run_ms=$(time_ms "$binpath")
            total_runtime=$((total_runtime + run_ms))
        done
        avg_runtime=$((total_runtime / 3))
    else
        bin_size="N/A"
        avg_runtime="N/A"
    fi

    printf "  %-45s compile=%4dms  runtime=%4sms  ir=%5s lines  bin=%s bytes\n" \
        "$name" "$compile_ms" "$avg_runtime" "$ir_lines" "$bin_size"

    results+=("$name $compile_ms $avg_runtime $ir_lines $bin_size")
done

echo ""

# --- Save baseline ---

if [ "$MODE" = "--save" ]; then
    printf "" > "$BASELINE_FILE"
    for r in "${results[@]}"; do
        echo "$r" >> "$BASELINE_FILE"
    done
    echo "Baseline saved to $BASELINE_FILE"
fi

# --- Compare against baseline ---

if [ "$MODE" = "--compare" ] && [ -f "$BASELINE_FILE" ]; then
    echo "=== Comparison against baseline ==="
    echo ""
    regressions=0

    for r in "${results[@]}"; do
        read -r name compile_ms runtime_ms ir_lines bin_size <<< "$r"
        baseline=$(grep "^$name " "$BASELINE_FILE" 2>/dev/null || echo "")
        if [ -z "$baseline" ]; then
            echo "  $name: no baseline (new program)"
            continue
        fi
        read -r _bname b_compile b_runtime b_ir b_bin <<< "$baseline"

        # Compare compile time
        if [ "$b_compile" -gt 0 ] 2>/dev/null; then
            delta=$(( (compile_ms - b_compile) * 100 / b_compile ))
            if [ "$delta" -gt "$THRESHOLD_PCT" ]; then
                echo "  WARNING: $name compile time regressed ${delta}% (${b_compile}ms -> ${compile_ms}ms)"
                regressions=$((regressions + 1))
            fi
        fi

        # Compare runtime
        if [ "$b_runtime" != "N/A" ] && [ "$runtime_ms" != "N/A" ] && [ "$b_runtime" -gt 0 ] 2>/dev/null; then
            delta=$(( (runtime_ms - b_runtime) * 100 / b_runtime ))
            if [ "$delta" -gt "$THRESHOLD_PCT" ]; then
                echo "  WARNING: $name runtime regressed ${delta}% (${b_runtime}ms -> ${runtime_ms}ms)"
                regressions=$((regressions + 1))
            fi
        fi

        # Compare IR size
        if [ "$b_ir" -gt 0 ] 2>/dev/null; then
            delta=$(( (ir_lines - b_ir) * 100 / b_ir ))
            if [ "$delta" -gt "$THRESHOLD_PCT" ]; then
                echo "  WARNING: $name IR size regressed ${delta}% (${b_ir} -> ${ir_lines} lines)"
                regressions=$((regressions + 1))
            fi
        fi
    done

    if [ "$regressions" -eq 0 ]; then
        echo "  No regressions detected (threshold: ${THRESHOLD_PCT}%)"
    else
        echo ""
        echo "  $regressions regression(s) detected"
    fi
fi

echo ""
echo "Done."
