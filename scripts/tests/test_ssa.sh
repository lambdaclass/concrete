#!/usr/bin/env bash
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
pattern = r"""run_ok\s+"([^"]+)"\s+("([^"]*(?:\n[^"]*)*?)"|(\S+))([ \t]+(\d+))?"""
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
    rc = m.group(6) if m.group(6) is not None else ""
    with open(os.path.join(resdir, f"test_{i:04d}.spec"), "w") as f:
        f.write(path + "\n")
        f.write(rc + "\n")
        f.write(expected)
' "$RESDIR"

for spec in "$RESDIR"/test_*.spec; do
    [ -f "$spec" ] || continue
    file=$(head -1 "$spec")
    expected_rc_raw=$(sed -n '2p' "$spec")
    expected=$(tail -n +3 "$spec")
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
        actual=$(perl -e 'alarm 5; exec @ARGV' "$out" 2>&1) && actual_rc=0 || actual_rc=$?
        # Mirror run_ok's MAIN_EXIT_MODEL semantics (stage 2): explicit rc arg
        # = printed stdout + that exit code; numeric 0-255 = exit code (or
        # self-printing + rc 0); anything else = printed stdout + rc 0.
        pass=0
        if [ -n "$expected_rc_raw" ]; then
            [ "$actual" = "$expected" ] && [ "$actual_rc" = "$expected_rc_raw" ] && pass=1
        else
            case "$expected" in
                ''|*[!0-9]*)
                    [ "$actual" = "$expected" ] && [ "$actual_rc" = "0" ] && pass=1 ;;
                *)
                    if [ "$expected" -le 255 ]; then
                        { [ "$actual_rc" = "$expected" ] && [ -z "$actual" ]; } && pass=1
                        { [ "$actual" = "$expected" ] && [ "$actual_rc" = "0" ]; } && pass=1
                    else
                        [ "$actual" = "$expected" ] && [ "$actual_rc" = "0" ] && pass=1
                    fi ;;
            esac
        fi
        if [ "$pass" = "1" ]; then
            echo "ok $name"
        else
            echo "FAIL $name: expected='$expected' rc='${expected_rc_raw:-auto}' got rc=$actual_rc '$actual'"
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
