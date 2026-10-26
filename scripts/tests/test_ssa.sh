#!/usr/bin/env bash
# Run all positive tests with --compile-ssa
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2

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
    # A scraped path must be literal: a shell variable here (a run_ok inside a loop) would
    # become a program name that does not exist. Refuse the corpus rather than guess.
    if "$" in path:
        sys.exit(f"test_ssa.sh: run_ok path {m.group(1)!r} in run_tests.sh is not literal; "
                 "write one run_ok per program so the SSA scraper sees it")
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
' "$RESDIR" || { echo "FAIL: could not extract the run_ok corpus from run_tests.sh"; exit 1; }

for spec in "$RESDIR"/test_*.spec; do
    [ -f "$spec" ] || continue
    file=$(head -1 "$spec")
    expected_rc_raw=$(sed -n '2p' "$spec")
    expected=$(tail -n +3 "$spec")
    name=$(basename "$file" .con)
    spec_id=$(basename "$spec" .spec)
    # The scraper sees every run_ok call, including the network tests run_tests.sh
    # guards — honor the same skip here, under the same truthful name.
    if [ "${SKIP_NETWORK_TESTS:-0}" = "1" ]; then
        case "$name" in
            tcp_basic|net_tcp_roundtrip)
                echo "skip $name (SKIP_NETWORK_TESTS=1)"
                SKIP=$((SKIP + 1))
                continue ;;
        esac
    fi
    (
        out="$OUTDIR/$spec_id"
        # Private scratch directory per program (bug 191): a program that touches the filesystem
        # builds its paths from CONCRETE_TEST_TMPDIR, never a fixed /tmp name another user or run
        # owns. The timeout re-run below reuses the same directory.
        export CONCRETE_TEST_TMPDIR="$out.scratch"
        mkdir -p "$CONCRETE_TEST_TMPDIR"
        if ! $COMPILER "$file" -o "$out" > /dev/null 2>&1; then
            msg=$($COMPILER "$file" -o "$out" 2>&1 | head -1)
            echo "FAIL $name: $msg"
            exit 1
        fi
        actual=$(perl -e 'alarm 5; exec @ARGV' "$out" 2>&1) && actual_rc=0 || actual_rc=$?
        # A 5-second alarm (142) under machine load is not an observed wrong result: record it as a
        # TIMEOUT and let the sequential pass below decide (pass / FAIL / INCOMPLETE).
        if [ "$actual_rc" = "142" ]; then echo "TIMEOUT $name $spec_id"; exit 0; fi
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
    # Keyed by spec id, not basename: ten tests are named main.con, and a basename key let one
    # test's result overwrite another's (a passing main could hide a failing main).
    ) > "$RESDIR/$spec_id.out" 2>&1 &

    # Limit to 8 parallel jobs
    TOTAL=$((TOTAL + 1))
    if [ $((TOTAL % 8)) -eq 0 ]; then
        wait
    fi
done

wait

# TIMEOUTS ARE RE-RUN ALONE, NOT COUNTED AS FAILURES. Under load the first run of a freshly
# linked binary can exceed the 5-second alarm (a macOS first-launch scan alone took 2-3 s), and
# hundreds of such alarms were reported as test failures. Each timed-out test now re-runs
# sequentially with a 30-second alarm and the same comparison: a pass is recorded as recovered, a
# wrong result is a FAIL, and a second timeout is INCOMPLETE. Only all-pass exits 0, so a real hang
# still fails the gate — it is just labelled for what it is.
INCOMPLETE=0; RECOVERED=0
for f in "$RESDIR"/*.out; do
    [ -f "$f" ] || continue
    result=$(cat "$f")
    case "$result" in TIMEOUT*) ;; *) continue ;; esac
    set -- $result; tname=$2; tspec="$RESDIR/$3.spec"
    expected_rc_raw=$(sed -n '2p' "$tspec"); expected=$(tail -n +3 "$tspec")
    out="$OUTDIR/$3"
    actual=$(CONCRETE_TEST_TMPDIR="$out.scratch" perl -e 'alarm 30; exec @ARGV' "$out" 2>&1) && actual_rc=0 || actual_rc=$?
    pass=0
    if [ "$actual_rc" = "142" ]; then
        echo "INCOMPLETE $tname: timed out twice (5 s in parallel, 30 s alone)" > "$f"; continue
    fi
    if [ -n "$expected_rc_raw" ]; then
        [ "$actual" = "$expected" ] && [ "$actual_rc" = "$expected_rc_raw" ] && pass=1
    else
        case "$expected" in
            ''|*[!0-9]*) [ "$actual" = "$expected" ] && [ "$actual_rc" = "0" ] && pass=1 ;;
            *)
                if [ "$expected" -le 255 ]; then
                    { [ "$actual_rc" = "$expected" ] && [ -z "$actual" ]; } && pass=1
                    { [ "$actual" = "$expected" ] && [ "$actual_rc" = "0" ]; } && pass=1
                else
                    [ "$actual" = "$expected" ] && [ "$actual_rc" = "0" ] && pass=1
                fi ;;
        esac
    fi
    if [ "$pass" = "1" ]; then echo "ok $tname (recovered: timed out under load, passed alone)" > "$f"
    else echo "FAIL $tname: expected='$expected' rc='${expected_rc_raw:-auto}' got rc=$actual_rc '$actual' (re-run alone)" > "$f"; fi
done

# Collect results
for f in "$RESDIR"/*.out; do
    [ -f "$f" ] || continue
    result=$(cat "$f")
    echo "$result"
    case "$result" in
        ok*"(recovered"*) PASS=$((PASS + 1)); RECOVERED=$((RECOVERED + 1)) ;;
        ok*) PASS=$((PASS + 1)) ;;
        INCOMPLETE*) INCOMPLETE=$((INCOMPLETE + 1)) ;;
        *) FAIL=$((FAIL + 1)) ;;
    esac
done

echo ""
echo "PASS: $PASS  FAIL: $FAIL  INCOMPLETE: $INCOMPLETE  RECOVERED: $RECOVERED  SKIP: $SKIP  TOTAL: $((PASS + FAIL + INCOMPLETE))"
[ "$FAIL" -eq 0 ] && [ "$INCOMPLETE" -eq 0 ]
