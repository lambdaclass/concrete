#!/usr/bin/env bash
# Bug 065: the program-level stack figure must fail CLOSED.
#
# `--report stack-depth`'s `Max stack bound:` line maximised over the BOUNDED functions only, so a
# program with recursion printed a finite number (`20 bytes (leaf)` while `main` recursed). Three
# consumers read that number: `[policy] max_stack_bytes` (check_policy.sh), `[allocation]
# stack_max_bytes` (check_assumptions.sh) and the release bundle's `evidence.max_stack_bytes`
# (capture_release_bundle.sh) -- so a budget PASSED a program with unbounded stack, and a bundle
# could publish a false bound. The line now reads `unbounded` whenever any listed function has no
# bound, and all three consumers read it through scripts/tests/lib/stack_bound.sh.
#
# Fixture: tests/regressions/bug065_unbounded_stack/{unbounded,bounded}.
#   1. the summary says unbounded (and the bounded leaf keeps its own exact row);
#   2. a max_stack_bytes / stack_max_bytes budget REFUSES it, naming unboundedness, and the
#      release bundle marks it "unbounded" instead of publishing a number;
#   3. positive control: the fully bounded program reports its exact maximum, passes a budget of
#      exactly that, and fails one byte less.
#   4. one reader: no other script scrapes a number out of the summary line.
set -uo pipefail
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "$ROOT_DIR/scripts/tests/lib/stack_bound.sh"
CC="$ROOT_DIR/.lake/build/bin/concrete"
FIX="tests/regressions/bug065_unbounded_stack"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

UNB="$("$CC" "$FIX/unbounded/src/main.con" --report stack-depth 2>&1)"
BND="$("$CC" "$FIX/bounded/src/main.con" --report stack-depth 2>&1)"

echo "=== 1. the summary says unbounded ==="
line="$(grep -E '^Max stack bound:' <<<"$UNB")"
[[ "$line" =~ ^Max\ stack\ bound:\ unbounded\ \(.*main\.depth.*main\.main ]] \
  && ok "summary is unbounded and names the unbounded functions: $line" \
  || no "summary does not say unbounded: '${line:-<no summary line>}'"
grep -qE '^Max stack bound: [0-9]' <<<"$UNB" \
  && no "a numeric program maximum is still printed for a recursive program" \
  || ok "no numeric program maximum is printed"
v="$(stack_max_bound <<<"$UNB")"; rc=$?
[ "$rc" -eq 3 ] && [ "$v" = unbounded ] && ok "lib/stack_bound.sh reads it as unbounded (rc 3)" \
                                        || no "lib/stack_bound.sh read '$v' (rc $rc), expected unbounded (rc 3)"
grep -A1 -E '^  leaf$' <<<"$UNB" | grep -q 'stack: 20 bytes' \
  && ok "the bounded leaf keeps its exact row (unboundedness propagates to callers, not callees)" \
  || no "the bounded leaf lost its row: $(grep -A1 -E '^  leaf$' <<<"$UNB" | tail -1)"

echo "=== 2. budgets refuse it; the bundle marks it ==="
out="$(bash scripts/tests/check_policy.sh "$FIX/unbounded/Concrete.toml" 2>&1)"; rc=$?
[ "$rc" -ne 0 ] && grep -q 'FAIL max_stack_bytes=100000 but the stack is UNBOUNDED' <<<"$out" \
  && ok "[policy] max_stack_bytes = 100000 is REFUSED, naming unboundedness" \
  || no "policy did not refuse the unbounded program (rc $rc): $(grep -E 'max_stack' <<<"$out")"
out="$(bash scripts/tests/check_assumptions.sh "$FIX/unbounded/assumptions.toml" 2>&1)"; rc=$?
[ "$rc" -ne 0 ] && grep -q 'FAIL allocation.stack_max_bytes=100000 but the stack is UNBOUNDED' <<<"$out" \
  && ok "[allocation] stack_max_bytes = 100000 is REFUSED, naming unboundedness" \
  || no "assumptions did not refuse the unbounded program (rc $rc): $(grep -E 'stack_max' <<<"$out")"
bash scripts/tests/capture_release_bundle.sh "$FIX/unbounded" -o "$TMP/rb_unb" >"$TMP/rb_unb.log" 2>&1
python3 - "$TMP/rb_unb/manifest.json" >"$TMP/rb_unb.chk" 2>&1 <<'PY'
import json, sys
print(json.load(open(sys.argv[1]))["evidence"]["max_stack_bytes"])
PY
[ "$(cat "$TMP/rb_unb.chk")" = unbounded ] \
  && ok "release bundle publishes evidence.max_stack_bytes = \"unbounded\" (valid JSON)" \
  || no "release bundle evidence.max_stack_bytes is not \"unbounded\": $(head -2 "$TMP/rb_unb.chk") $(tail -2 "$TMP/rb_unb.log")"
printf 'no summary here\n' > "$TMP/empty.txt"
[ "$(stack_bound_json "$TMP/empty.txt")" = '"unknown"' ] && [ "$(stack_bound_json "$TMP/missing.txt")" = '"unknown"' ] \
  && ok "a missing or unparseable report yields \"unknown\", never a number or invalid JSON" \
  || no "stack_bound_json does not fail closed on a missing/unparseable report"

echo "=== 3. positive control: a bounded program keeps its exact number ==="
line="$(grep -E '^Max stack bound:' <<<"$BND")"
[ "$line" = "Max stack bound: 40 bytes (main)" ] && ok "bounded summary: $line" \
                                                  || no "bounded summary changed: '$line'"
out="$(bash scripts/tests/check_policy.sh "$FIX/bounded/Concrete.toml" 2>&1)"; rc=$?
[ "$rc" -eq 0 ] && grep -q 'ok   max_stack_bytes=40 (actual=40)' <<<"$out" \
  && ok "max_stack_bytes = 40 passes at exactly the bound" || no "bounded policy did not pass (rc $rc): $(grep -E 'FAIL|max_stack' <<<"$out")"
out="$(bash scripts/tests/check_assumptions.sh "$FIX/bounded/assumptions.toml" 2>&1)"; rc=$?
[ "$rc" -eq 0 ] && grep -q 'ok   allocation.stack_max_bytes=40 (actual=40)' <<<"$out" \
  && ok "stack_max_bytes = 40 passes at exactly the bound" || no "bounded assumptions did not pass (rc $rc): $(grep -E 'FAIL|stack_max' <<<"$out")"
mkdir -p "$TMP/tight"; cp -R "$FIX/bounded/src" "$TMP/tight/"
sed 's/^max_stack_bytes = 40 .*/max_stack_bytes = 39/' "$FIX/bounded/Concrete.toml" > "$TMP/tight/Concrete.toml"
out="$(bash scripts/tests/check_policy.sh "$TMP/tight/Concrete.toml" 2>&1)"; rc=$?
[ "$rc" -ne 0 ] && grep -q 'FAIL max_stack_bytes=39 but actual max is 40' <<<"$out" \
  && ok "max_stack_bytes = 39 fails (the budget comparison is live)" || no "a budget one byte short passed (rc $rc)"
bash scripts/tests/capture_release_bundle.sh "$FIX/bounded" -o "$TMP/rb_bnd" >"$TMP/rb_bnd.log" 2>&1
v="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["evidence"]["max_stack_bytes"])' "$TMP/rb_bnd/manifest.json" 2>&1)"
[ "$v" = 40 ] && ok "release bundle publishes evidence.max_stack_bytes = 40" || no "bounded bundle max_stack_bytes is '$v'"

echo "=== 4. one reader of the summary line ==="
scrapers="$(grep -rlE 'Max stack bound:[^|]*\[0-9\]' scripts/ | grep -vE 'scripts/tests/(lib/stack_bound|check_stack_budget)\.sh$' || true)"
[ -z "$scrapers" ] && ok "no script but lib/stack_bound.sh scrapes a number from 'Max stack bound:'" \
                   || no "scripts scrape the summary number directly: $scrapers"

echo
echo "STACK-BUDGET: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
