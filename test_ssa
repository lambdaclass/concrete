#!/usr/bin/env bash
# Run all positive tests with --compile-ssa
set -uo pipefail

COMPILER=".lake/build/bin/concrete"
OUTDIR=$(mktemp -d)
RESDIR=$(mktemp -d)
trap 'rm -rf "$OUTDIR" "$RESDIR"' EXIT

PASS=0
FAIL=0
TOTAL=0

# Extract test cases handling multiline expected values, output as individual files
python3 -c '
import re, sys, os
content = open("run_tests.sh").read()
pattern = r"""run_ok\s+"([^"]+)"\s+("([^"]*(?:\n[^"]*)*?)"|(\S+))"""
resdir = sys.argv[1]
for i, m in enumerate(re.finditer(pattern, content)):
    path = m.group(1).replace("$TESTDIR/", "lean_tests/")
    if m.group(3) is not None:
        expected = m.group(3)
    else:
        expected = m.group(4)
    with open(os.path.join(resdir, f"test_{i:04d}.spec"), "w") as f:
        f.write(path + "\n")
        f.write(expected)
' "$RESDIR"

for spec in "$RESDIR"/test_*.spec; do
    [ -f "$spec" ] || continue
    file=$(head -1 "$spec")
    expected=$(tail -n +2 "$spec")
    name=$(basename "$file" .con)

    (
        out="$OUTDIR/$name"
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
echo "PASS: $PASS  FAIL: $FAIL  TOTAL: $((PASS + FAIL))"
