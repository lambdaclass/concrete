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

echo ""
echo "CLI-CONTRACT: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
