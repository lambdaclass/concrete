#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Phase 6C #4 gate: counterexample-first pipeline debugging.
#
# Any pipeline failure should reduce to a MINIMIZED `.con` fixture plus a REPLAY
# COMMAND, and the reduced fixture must be saveable as a regression (re-running the
# predicate on it still reproduces). This gate exercises the existing reducer
# (`concrete reduce` + scripts/reduce/*) across the four failure-source mechanisms
# the roadmap names:
#
#   1. panic/error -> diagnostic   (internal stage error; external:expect-error-code)
#   2. interp-vs-compiled mismatch (external:expect-oracle-mismatch)
#   3. fold/trap-preservation      (external:expect-trap — runtime abort survives)
#   4. backend/leak / report claim (external:expect-report-contains)
#
# Classes with a live reproducer (1, 3) are reduced end-to-end and saved as a
# regression. Classes whose real bugs are all fixed (2, and any true backend leak)
# are exercised as detector-+-wiring: the predicate correctly does NOT fire on a
# correct program and the reducer refuses to fabricate a counterexample. Class 4's
# report-finding reduction path is shown live on a benign report claim.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }

TMP=$(mktemp -d)
REGRESS="$TMP/regressions"
mkdir -p "$REGRESS"
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
# count top-level fn decls (a proxy for "minimized")
nfns(){ grep -cE "^\s*fn " "$1" 2>/dev/null || echo 0; }

# reduce_and_save <class> <input.con> <predicate-string> <replay-note>
# Asserts: predicate fires on input; reduce shrinks decl count; reduced still
# reproduces; a saved regression + replay command exist.
reduce_and_save(){
  local cls="$1" inp="$2" pred="$3"
  # predicate must reproduce on the original
  if ! eval "$pred \"$inp\"" >/dev/null 2>&1; then
    no "$cls: predicate does not reproduce on the input (cannot form a counterexample)"; return
  fi
  local before after out="$TMP/${cls}.reduced.con"
  before=$(nfns "$inp")
  "$COMPILER" reduce "$inp" --predicate "external:$pred" -o "$out" >/dev/null 2>&1
  if [ ! -s "$out" ]; then no "$cls: reducer produced no output"; return; fi
  after=$(nfns "$out")
  # minimized: strictly fewer top-level fns than the noisy input
  if [ "$after" -lt "$before" ]; then ok "$cls: minimized ($before -> $after fns)";
  else no "$cls: not minimized ($before -> $after fns)"; fi
  # reduced fixture still reproduces -> saveable as a regression
  if eval "$pred \"$out\"" >/dev/null 2>&1; then ok "$cls: reduced fixture still reproduces (regression-worthy)";
  else no "$cls: reduced fixture no longer reproduces"; fi
  # save as a regression + emit a replay command
  local saved="$REGRESS/${cls}.con"
  cp "$out" "$saved"
  printf 'concrete reduce %s --predicate "external:%s"\n' "$saved" "$pred" > "$REGRESS/${cls}.replay"
  if [ -s "$saved" ] && [ -s "$REGRESS/${cls}.replay" ]; then ok "$cls: saved regression + replay command";
  else no "$cls: failed to save regression/replay"; fi
}

echo "=== class 1: panic/error -> diagnostic (external:expect-error-code) ==="
cat > "$TMP/c1.con" <<'EOF'
mod m {
  fn noise_a() -> Int { return 1; }
  fn noise_b() -> Int { return 2; }
  fn bad() -> Int { let x: Int = true; return x; }
  fn main() -> Int { return 0; }
}
EOF
reduce_and_save "error_to_diagnostic" "$TMP/c1.con" "bash scripts/reduce/expect-error-code.sh E0220"

echo "=== class 3: fold/trap-preservation (external:expect-trap) ==="
cat > "$TMP/c3.con" <<'EOF'
mod m {
  fn noise_a() -> Int { return 1; }
  fn noise_b() -> Int { return 2; }
  fn crashy() -> i32 { let z: i32 = 0; return 10 / z; }
  fn main() -> Int { let r: i32 = crashy(); return r as Int; }
}
EOF
reduce_and_save "trap_preservation" "$TMP/c3.con" "bash scripts/reduce/expect-trap.sh"

echo "=== class 4: report-finding reduction (external:expect-report-contains) ==="
# Reduce a program down to the minimal code that still carries a specific report
# claim (the same mechanism a leaked-authority / backend-claim counterexample uses):
# here, the minimal program whose `--report caps` still names `target_fn`.
cat > "$TMP/c4.con" <<'EOF'
mod m {
  fn noise_a() -> Int { return 1; }
  fn noise_b() -> Int { return 2; }
  fn target_fn() -> Int { return 42; }
  fn main() -> Int { return target_fn(); }
}
EOF
if bash scripts/reduce/expect-report-contains.sh caps target_fn "$TMP/c4.con" >/dev/null 2>&1; then
  reduce_and_save "report_finding" "$TMP/c4.con" "bash scripts/reduce/expect-report-contains.sh caps target_fn"
else
  no "report_finding: 'caps' claim (target_fn) not present on sample"
fi

echo "=== class 2: interp-vs-compiled mismatch (detector + reducer refusal) ==="
# All real interp/compiled divergences are fixed, so there is no live mismatch to
# reduce. Prove the detector is wired: on a CORRECT program the oracle predicate
# must NOT fire, and the reducer must REFUSE to fabricate a counterexample.
cat > "$TMP/c2.con" <<'EOF'
mod m { fn main() -> Int { let x: Int = 2 + 3; return x; } }
EOF
if bash scripts/reduce/expect-oracle-mismatch.sh "$TMP/c2.con" >/dev/null 2>&1; then
  no "oracle_mismatch: detector fired on a correct program (unexpected live mismatch!)"
else
  ok "oracle_mismatch: detector correctly reports no mismatch on correct code"
fi
# The reducer must REFUSE to fabricate a counterexample: it reports "predicate does
# not hold on the original source" and leaves the source unchanged (no shrink).
red_msg="$("$COMPILER" reduce "$TMP/c2.con" --predicate "external:bash scripts/reduce/expect-oracle-mismatch.sh" -o "$TMP/c2.out" 2>&1)"
if grep -q <<<"$red_msg" "predicate does not hold" && cmp -s "$TMP/c2.con" "$TMP/c2.out"; then
  ok "oracle_mismatch: reducer refuses to fabricate a counterexample (source left unchanged)"
else
  no "oracle_mismatch: reducer did not cleanly refuse (msg='$red_msg')"
fi

echo
echo "check_counterexample_reduction: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
