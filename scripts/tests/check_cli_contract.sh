#!/usr/bin/env bash
# Golden CLI behavior matrix (ROADMAP Phase 4 #15).
#
# Pins the externally-observable contract of each PUBLIC command: exit code,
# which stream carries output (stdout vs stderr), --json well-formedness where
# supported, missing-project behavior, unknown-command / malformed-input behavior,
# and output-artifact location. This is a contract gate, not a refactor: it asserts
# what the commands already promise so future changes cannot silently break them.
#
# First slice (existing commands only): build, run, test, check, prove, --report,
# --version, no-args, unknown command, missing Concrete.toml, malformed input.
# Future commands (inspect, fmt, doc, clean) are listed as NOT-YET so the matrix
# makes their absence explicit rather than inventing them.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

PROJ="examples/project"            # smallest buildable project (has Concrete.toml)
CT="examples/constant_time_tag/src/main.con"   # has a proved function
PP="examples/proof_pressure/src/main.con"      # has a missing obligation
TMP="$(mktemp -d)"
OUT="$TMP/out"; ERR="$TMP/err"
# run <command...> → sets $rc, captures stdout in $OUT, stderr in $ERR
run(){ "$@" >"$OUT" 2>"$ERR"; rc=$?; }
sout(){ cat "$OUT"; }; serr(){ cat "$ERR"; }

echo "=== global hygiene: no args, version, unknown command ==="
run "$C"
{ [ "$rc" = "1" ] && [ ! -s "$OUT" ] && grep -q "Usage: concrete" "$ERR"; } \
  && ok "no args → exit 1, usage on stderr, stdout empty" || no "no-args contract (rc=$rc)"

run "$C" --version
{ [ "$rc" = "0" ] && [ "$(wc -l <"$OUT")" -ge 1 ] && [ ! -s "$ERR" ]; } \
  && ok "--version → exit 0, identity on stdout, stderr empty" || no "--version contract (rc=$rc)"

run "$C" totally-unknown-xyz
{ [ "$rc" != "0" ] && ! grep -qi "uncaught exception" "$ERR" && grep -qi "not a readable file or a known command" "$ERR"; } \
  && ok "unknown command → clean nonzero error (no uncaught exception)" || no "unknown-command contract (rc=$rc)"

echo "=== missing Concrete.toml: project commands fail uniformly ==="
for cmd in build run test check; do
  ( cd "$TMP" && "$C" "$cmd" >"$OUT" 2>"$ERR" ); rc=$?
  { [ "$rc" = "1" ] && grep -q "no Concrete.toml found" "$ERR"; } \
    && ok "$cmd outside a project → exit 1, 'no Concrete.toml' on stderr" \
    || no "$cmd missing-project contract (rc=$rc)"
done

echo "=== build: exit 0, binary written at -o path ==="
BIN="$TMP/proj_bin"
( cd "$PROJ" && "$C" build -o "$BIN" >"$OUT" 2>"$ERR" ); rc=$?
{ [ "$rc" = "0" ] && [ -f "$BIN" ]; } \
  && ok "build -o <path> → exit 0, binary at the requested path" || no "build contract (rc=$rc)"

echo "=== run: builds and executes, propagates the program's exit ==="
( cd "$PROJ" && "$C" run >"$OUT" 2>"$ERR" ); rc=$?
# examples/project main returns a value; run must complete without a crash.
{ ! grep -qi "uncaught exception" "$ERR"; } \
  && ok "run → executes the project without an uncaught exception (exit $rc)" || no "run contract (rc=$rc)"

echo "=== test: defined exit, no crash ==="
( cd "$PROJ" && "$C" test >"$OUT" 2>"$ERR" ); rc=$?
{ { [ "$rc" = "0" ] || [ "$rc" = "1" ]; } && ! grep -qi "uncaught exception" "$ERR"; } \
  && ok "test → exit 0/1, no uncaught exception" || no "test contract (rc=$rc)"

echo "=== check: proof-status report on stdout, defined exit ==="
( cd "$PROJ" && "$C" check >"$OUT" 2>"$ERR" ); rc=$?
{ { [ "$rc" = "0" ] || [ "$rc" = "1" ]; } && grep -qi "Proof Status" "$OUT"; } \
  && ok "check → proof-status report on stdout, exit 0/1" || no "check contract (rc=$rc)"

echo "=== prove: documented exit-code taxonomy + --json shape ==="
"$C" prove >"$OUT" 2>"$ERR"; rc=$?
{ [ "$rc" = "1" ] && grep -q "Usage: concrete prove" "$ERR"; } \
  && ok "prove (no args) → exit 1, usage on stderr" || no "prove usage contract (rc=$rc)"
PJ="$("$C" prove "$CT" constant_time_tag.ct_compare --json 2>/dev/null)"; pe=$?
{ [ "$pe" = "0" ] && printf '%s' "$PJ" | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if d.get('function')=='constant_time_tag.ct_compare' and 'status' in d else 1)"; } \
  && ok "prove proved fn --json → exit 0, well-formed JSON with function+status" || no "prove proved-json contract (pe=$pe)"
"$C" prove "$PP" main.clamp_value --json >/dev/null 2>&1
[ "$?" = "2" ] && ok "prove missing obligation → exit 2 (taxonomy)" || no "prove missing-exit contract"

echo "=== --report: text on stdout (exit 0), --json well-formed ==="
run "$C" "$CT" --report contracts
{ [ "$rc" = "0" ] && [ -s "$OUT" ]; } \
  && ok "--report contracts → exit 0, report on stdout" || no "--report text contract (rc=$rc)"
RJ="$("$C" "$CT" --report obligation-ledger --json 2>/dev/null)"
printf '%s' "$RJ" | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if d.get('schema_kind')=='obligation_ledger' else 1)" \
  && ok "--report obligation-ledger --json → well-formed JSON envelope" || no "--report json contract"

echo "=== malformed input: clean diagnostic, no crash ==="
BAD="$TMP/bad.con"; printf 'fn a( -> Int { return 0; }\n' >"$BAD"
run "$C" "$BAD" --report contracts
{ [ "$rc" != "0" ] && ! grep -qi "uncaught exception" "$ERR" && [ -s "$ERR" ]; } \
  && ok "malformed input → nonzero exit, diagnostic on stderr, no uncaught exception" || no "malformed-input contract (rc=$rc)"

echo "=== future command surfaces are explicitly absent (not invented) ==="
for fut in inspect fmt doc clean; do
  run "$C" "$fut"
  # these are not implemented as subcommands yet → treated as a path/unknown,
  # so they must fail cleanly (the matrix records them as NOT-YET, not supported).
  { [ "$rc" != "0" ] && ! grep -qi "uncaught exception" "$ERR"; } \
    && ok "future '$fut' not yet a command → clean nonzero (NOT-YET)" || no "future '$fut' should fail cleanly (rc=$rc)"
done

# =================================================================================================
# REPORT EXIT-STATUS MATRIX.
#
# Three handlers — attestation-join, generated-implementations, impl-manifest — printed their report
# and then FELL THROUGH to the "Unknown report type" branch, exiting 1 with a diagnostic naming the
# report the caller had just received. Callers checking status saw failure on correct output, and a
# test gate was consuming exactly that. Nothing here asserted a report's exit code, so the defect was
# invisible; the whole point of this matrix is that a report's status is part of its contract.
#
# Success is asserted as COMPLETE output, not merely non-empty: a handler that printed a header and
# died would otherwise still look correct.
TD="examples/thesis_demo/src/main.con"
SP="examples/evidence_classes/stale_proof/src/main.con"

echo "=== reports: the three that used to exit 1 on success ==="
run "$C" "$TD" --report attestation-join
{ [ "$rc" = "0" ] && [ -s "$OUT" ] && head -1 "$OUT" | grep -q '^# role' && ! grep -q 'Unknown report type' "$ERR"; } \
  && ok "attestation-join → exit 0 with its header, no unknown-report diagnostic" \
  || no "attestation-join contract (rc=$rc)"

run "$C" "$TD" --report generated-implementations
{ [ "$rc" = "0" ] && grep -q '^-- emitted=[0-9]' "$OUT" && ! grep -q 'Unknown report type' "$ERR"; } \
  && ok "generated-implementations → exit 0, reaches its '-- emitted=' trailer" \
  || no "generated-implementations contract (rc=$rc)"

run "$C" "$TD" --report impl-manifest
_e="$(sed -n 's/^IMPL-MANIFEST expected=\([0-9]*\).*/\1/p' "$OUT")"
_r="$(sed -n 's/^IMPL-MANIFEST.* rows=\([0-9]*\).*/\1/p' "$OUT")"
{ [ "$rc" = "0" ] && [ -n "$_e" ] && [ "$_e" = "$_r" ] && ! grep -q 'Unknown report type' "$ERR"; } \
  && ok "impl-manifest → exit 0, self-consistent (expected=$_e rows=$_r)" \
  || no "impl-manifest contract (rc=$rc expected=$_e rows=$_r)"

echo "=== reports: the unknown branch still rejects ==="
run "$C" "$TD" --report no-such-report-name
{ [ "$rc" = "1" ] && [ ! -s "$OUT" ] && grep -q 'Unknown report type' "$ERR"; } \
  && ok "unknown --report → exit 1, empty stdout, unknown-report diagnostic on stderr" \
  || no "unknown --report contract (rc=$rc)"

echo "=== reports: legitimate exit 1 is preserved (subject rejection, not a CLI failure) ==="
run "$C" "$SP" --report proof-status
{ [ "$rc" = "1" ] && grep -q '^Totals:' "$OUT"; } \
  && ok "proof-status on a stale fixture → exit 1 WITH a complete report (the subject is rejected)" \
  || no "proof-status stale-fixture contract (rc=$rc)"
run "$C" "$SP" --report consistency
{ [ "$rc" = "1" ] && grep -q 'consistency violation' "$OUT"; } \
  && ok "consistency on a stale fixture → exit 1 WITH its violation report" \
  || no "consistency stale-fixture contract (rc=$rc)"

echo "=== reports: the handlers that return through the shared path stay green (positive controls) ==="
# Static scanning suggested nine handlers lacked a return; measurement showed three. These six take a
# later shared return, and pinning them here is what makes that distinction a fact rather than a
# reading of the source.
for _r in proof-status obligations proof-bundle consistency audit verify; do
  run "$C" "$TD" --report "$_r"
  { [ "$rc" = "0" ] && [ -s "$OUT" ] && ! grep -q 'Unknown report type' "$ERR"; } \
    && ok "--report $_r → exit 0 via the shared return" \
    || no "--report $_r regressed (rc=$rc)"
done

echo ""
echo "CLI-CONTRACT: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
