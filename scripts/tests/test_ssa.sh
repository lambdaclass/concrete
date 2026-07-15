#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Run all positive tests with --compile-ssa
set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
OUTDIR=$(mktemp -d)
RESDIR=$(mktemp -d)
trap 'rm -rf "$OUTDIR" "$RESDIR"' EXIT

PASS=0
FAIL=0
SKIP=0
TOTAL=0

# Extract test cases handling multiline expected values, output as individual files
python3 -c '
import re, sys, os
content = open("scripts/tests/run_tests.sh").read()
pattern = r"""run_ok\s+"([^"]+)"\s+("([^"]*(?:\n[^"]*)*?)"|(\S+))"""
resdir = sys.argv[1]
for i, m in enumerate(re.finditer(pattern, content)):
    path = m.group(1).replace("$TESTDIR/", "tests/programs/")
    if m.group(3) is not None:
        expected = m.group(3)
    else:
        expected = m.group(4)
    # run_tests.sh writes multiline expectations as "$(printf ...)"; the regex
    # captures that literally, so expand the idiom or the spec can never match.
    p = re.fullmatch(r"\$\(printf \x27(.*)\x27\)", expected, re.S)
    if p:
        expected = p.group(1).replace("\\n", "\n").replace("\\t", "\t")
    with open(os.path.join(resdir, f"test_{i:04d}.spec"), "w") as f:
        f.write(path + "\n")
        f.write(expected)
' "$RESDIR"

for spec in "$RESDIR"/test_*.spec; do
    [ -f "$spec" ] || continue
    file=$(head -1 "$spec")
    expected=$(tail -n +2 "$spec")
    name=$(basename "$file" .con)
    spec_id=$(basename "$spec" .spec)
    # The scraper sees every run_ok call, including ones run_tests.sh guards
    # behind the flaky-TCP skip — honor the same skip here.
    if [ "${SKIP_FLAKY_TCP_TEST:-0}" = "1" ]; then
        case "$name" in
            tcp_basic|net_tcp_roundtrip)
                echo "skip $name (SKIP_FLAKY_TCP_TEST=1)"
                SKIP=$((SKIP + 1))
                continue ;;
        esac
    fi
    (
        out="$OUTDIR/$spec_id"
        if ! $COMPILER "$file" -o "$out" > /dev/null 2>&1; then
            msg=$($COMPILER "$file" -o "$out" 2>&1 | head -1)
            echo "FAIL $name: $msg"
            exit 1
        fi
        actual=$(perl -e 'alarm 5; exec @ARGV' "$out" 2>&1) || true
        if [ "$actual" = "$expected" ]; then
            echo "ok $name"
        else
            echo "FAIL $name: expected='$expected' got='$actual'"
        fi
    ) > "$RESDIR/$name.out" 2>&1 &

    # Limit to 8 parallel jobs
    TOTAL=$((TOTAL + 1))
    if [ $((TOTAL % 8)) -eq 0 ]; then
        wait
    fi
done

wait

# Collect results
for f in "$RESDIR"/*.out; do
    [ -f "$f" ] || continue
    result=$(cat "$f")
    echo "$result"
    case "$result" in
        ok*) PASS=$((PASS + 1)) ;;
        *) FAIL=$((FAIL + 1)) ;;
    esac
done

echo ""
echo "PASS: $PASS  FAIL: $FAIL  SKIP: $SKIP  TOTAL: $((PASS + FAIL))"
[ "$FAIL" -eq 0 ]
