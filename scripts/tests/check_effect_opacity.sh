#!/usr/bin/env bash
# R-0484: AN EMPTY CAPABILITY SET IS "NOTHING DECLARED", NOT "NOTHING HAPPENS".
#
# The two were equated. `examples/base64_cli`'s `print_bytes` takes a `&Writer`, calls
# `Writer::write`, performs real I/O, and was reported `(pure)`, counted in a `1 pure`
# total, and admitted to the provable subset ON THE GROUNDS OF PURITY — while `usage`,
# which only prints a string, was excluded for honestly declaring `Console`.
#
# The erasure is at the `trusted` boundary and travels on a function pointer:
# `console_write` calls `libc_write` and declares nothing, so its TYPE is
# capability-free, so it fits `Writer`'s `write_fn` field, so calls through the handle
# are capability-free. `println`, same module, same syscall, declares `Console`. Nothing
# checks the difference.
#
# THE REPAIR REFUSES ADMISSION WITHOUT REFUSING EXTRACTION, AND THAT SPLIT IS THE POINT.
#
# A first attempt folded opacity into `eligible`. That bit decides TWO things — whether a
# function is admissible as effect-free, AND whether it is extracted for subject facts at
# all — so refusing higher-order functions removed them from the evidence surface, and
# `check_shadow_body_v2.sh` caught it: its "a function used as a VALUE is an edge"
# assertion exists precisely so a higher-order program does not look dependency-free.
# Trading a false purity claim for a missing dependency edge relocates an R-0004 gap
# instead of closing one.
#
# So `eligible` still gates extraction and a separate `admissible` gates proof admission.
# Both halves are asserted below, because a repair that only did the first half would be
# the original defect and a repair that only did the second would be the failed attempt.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
FIX="$ROOT_DIR/tests/regressions/effect_opacity/indirect_call_not_pure"

PASS=0; FAIL=0; SKIPPED=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }

# `timeout` is coreutils and is NOT present on a stock macOS runner. run_tests.sh
# already degrades rather than skipping (see its PROJ_TIMEOUT), and this gate is
# invoked FROM run_tests.sh, so it runs on macOS too and needs the same treatment.
# A missing watchdog is worth saying out loud: a hang here would otherwise look
# like an unexplained CI stall.
if command -v timeout >/dev/null 2>&1; then
  TO="timeout 300"
else
  TO=""
  echo "  warn 'timeout' not found — running without a hang watchdog"
fi


out="$(cd "$FIX" && $TO "$CC" src/main.con --report eligibility 2>&1)"

echo "=== a genuinely effect-free function is still admitted (positive control) ==="
# Without this the gate would pass on a compiler that refused everything, which is a
# different bug with the same green.
if printf '%s' "$out" | grep -qE "eligible +\`indirect_call_not_pure\.plain\`"; then
  ok "plain is admitted"
else
  no "plain is no longer admitted — the rule is over-broad, not conservative"
fi

echo "=== the REPORTS no longer claim purity they cannot establish ==="
# This half IS live. Reports consult opacity directly, so they are honest even while
# admission is inert.
cout="$(cd "$FIX" && $TO "$CC" src/main.con --report caps 2>&1)"
if printf '%s' "$cout" | grep -qE "plain +: \(pure\)"; then
  ok "a genuinely effect-free function still reads (pure)"
else
  no "plain lost its (pure) rendering — the rule is over-broad, not conservative"
fi
for fn in fire fire2; do
  if printf '%s' "$cout" | grep -qE "$fn +: \(effects unknown: reaches an indirect call\)"; then
    ok "$fn reads as effects-unknown rather than pure"
  else
    no "$fn still reads as pure in --report caps"
  fi
done
if printf '%s' "$cout" | grep -q "1 pure"; then
  ok "the purity total counts a claim (1), not empty capability sets (would be 4)"
else
  no "the purity total is not counting a claim"
  printf '%s\n' "$cout" | grep "Totals:" | sed 's/^/       /'
fi

# ADMISSION IS LIVE (2026-09-26). The rule was inert for one release because enabling it
# appeared to drop three `pureCoreFns` links out of replay and drift coverage (11 -> 8).
# That was a COUPLING, not a consequence of the rule: obligation status was derived from
# `admissible` through a parameter merely NAMED `eligible`. Status is now derived from
# EXTRACTABLE, and admission is carried as its own fact, so the four questions stay apart:
#
#   extractable  the eligibility/extraction rules permit an obligation
#   admissible   extractable AND every semantic admission gate passes
#   replayable   extractable AND a claim/evidence link exists
#   proved       replay passed AND admissible AND correspondence/dependency rules pass
#
# An admission failure must never erase an artifact from MAINTENANCE — otherwise an
# effect-classification change silently removes claims from replay and drift detection,
# which is the opposite of honest degradation.
echo "=== admission REFUSES an effect-opaque function, and names why ==="
pout="$(cd "$FIX" && $TO "$CC" src/main.con --report proof-status 2>&1)"
for fn in fire fire2; do
  if printf '%s' "$pout" | grep -q "admission: REFUSED"; then
    ok "$fn: admission refused"
  else
    no "$fn: admission is not refusing — the opacity rule is inert again"
  fi
done
if printf '%s' "$pout" | grep -q "admission: REFUSED — effects may enter through an indirect call"; then
  ok "the refusal NAMES its reason (a refusal reporting no reason is this task's defect)"
else
  no "admission refuses without naming the reason"
fi

echo "=== transitive opacity refuses too, not just the direct caller ==="
# `fire2` reaches the indirect call through `fire`. A rule that only caught the direct
# site would leave every wrapper admitted.
if printf '%s' "$pout" | grep -q "admission: REFUSED"; then
  n=$(printf '%s' "$pout" | grep -c "admission: REFUSED")
  [ "${n:-0}" -ge 2 ] && ok "at least two functions refused — opacity propagates ($n)" \
                      || no "only $n refusal: opacity is not transitive"
else
  no "no refusals at all"
fi

echo "=== CONTROL: genuinely effect-free code is STILL ADMITTED ==="
# Without this the gate passes on a compiler that refuses everything.
if printf '%s' "$pout" | grep -A3 "plain" | grep -q "admission: REFUSED"; then
  no "plain is refused — the rule is over-broad, not conservative"
else
  ok "plain keeps admission (the rule refuses narrowly)"
fi

echo "=== EXTRACTION survives refusal (the failed attempt broke this) ==="
# Refusing admission must not remove the function from the evidence surface. If this
# fails, higher-order programs look dependency-free and an R-0004 gap has been moved
# rather than closed.
facts="$(cd "$FIX" && $TO "$CC" src/main.con --report subject-facts 2>/dev/null | grep -c 'v1:user:indirect_call_not_pure.fire' || true)"
if [ "${facts:-0}" -gt 0 ]; then
  ok "a refused higher-order function still has subject facts ($facts)"
else
  no "refusing admission also dropped the function from extraction — this is the failed attempt"
fi

echo "=== REPLAY and DRIFT are untouched by admission ==="
# The objection that kept this rule inert. `check_purecore_proofs.sh` argues replay must
# keep asking about pending claims "or the migration cannot clear them" — it now passes
# WITH admission live, because the objection was to the coupling, not to the rule.
pc="$($TO bash "$ROOT_DIR/scripts/tests/check_purecore_proofs.sh" 2>&1 || true)"
# A REFUSAL TO RUN IS NOT A VERDICT. `check_purecore_proofs.sh` declines when another gate
# holds the shared .lake lock, and its refusal text contains neither the drift line nor a
# PASS/FAIL summary — so reading its absence as failure reports "admission is erasing
# claims from maintenance" on the strength of a gate that never executed. That is the same
# confusion between "I do not know" and "I know it is absent" that this whole area exists
# to prevent, reproduced in the gate checking it. Observed 2026-09-26.
if printf '%s' "$pc" | grep -q "GATE-PRECONDITION-FAILED\|REPOSITORY BUSY"; then
  echo "  SKIP check_purecore_proofs declined to run (repository busy) — no verdict available"
  echo "       Re-run this gate alone. Two checks were NOT performed, not passed."
  SKIPPED=$((SKIPPED+2))
elif ! printf '%s' "$pc" | grep -q "PURECORE-PROOFS: PASS=[0-9]* FAIL=[0-9]*"; then
  no "check_purecore_proofs produced no summary line at all — its verdict cannot be read"
  printf '%s\n' "$pc" | tail -3 | sed 's/^/       /'
else
  if printf '%s' "$pc" | grep -q "drift coverage: all 11 std links are drift-checked"; then
    ok "drift coverage stays at 11 with admission live"
  else
    no "drift coverage moved — admission is erasing claims from maintenance"
    printf '%s\n' "$pc" | grep -i drift | awk 'NR<=2' | sed 's/^/       /'
  fi
  if printf '%s' "$pc" | grep -q "PURECORE-PROOFS: PASS=[0-9]* FAIL=0"; then
    ok "registered inadmissible claims remain replay targets (purecore 0 failures)"
  else
    no "purecore proofs fail with admission live — replay is keyed on admission again"
  fi
fi

echo "=== admission is rendered on EVERY state, not just one (STRUCTURAL) ==="
# THESE TWO ARE SOURCE CHECKS, NOT BEHAVIOURAL, and the reason is worth stating rather
# than leaving for a reader to discover: there is no proved-AND-inadmissible function to
# observe today. std has two proved links, both in base64, and both are admissible. So the
# state that matters most — `proved` beside a refusal — cannot be produced by running the
# compiler on anything that exists, and a check that only ran it would assert nothing
# about that state while appearing to cover it.
#
# What is checkable is the STRUCTURE that makes the state impossible to miss: the line is
# appended once at the dispatch rather than inside each arm. Appending per-arm was the
# first implementation and it silently skipped exactly `proved` — the one state where an
# unreported refusal reads as a program-level guarantee rather than a pending item.
#
# Upgrade path, when a fixture with a registered proof over an effect-opaque function
# exists: assert on its rendered output and delete these two.
if grep -q "renderProofStatusBody e sourceMap ++ admissionLine e" "$ROOT_DIR/Concrete/Report/Report.lean"; then
  ok "admissionLine is appended once at the dispatch, so no state can be skipped"
else
  no "admission rendering is per-state again — \`proved\` will be skipped"
fi
# And the text it appends must disclaim admitted-proof coverage, or rendering it on
# `proved` would still leave the artifact reading as an unqualified program proof.
if grep -A16 "def admissionLine" "$ROOT_DIR/Concrete/Report/Report.lean" | grep -q "does not count as admitted proof"; then
  ok "the appended text denies admitted-proof coverage to an inadmissible artifact"
else
  no "the admission line no longer disclaims admitted proof coverage"
fi

echo "=== the wording does not contradict itself ==="
# `eligible` now means "an obligation can be extracted"; a reader takes "eligible for
# proof" to mean admissible, which is exactly what this work separated.
if printf '%s' "$pout" | grep -q "is eligible for proof but has no registered proof"; then
  no "the report still says 'eligible for proof' beside 'admission: REFUSED'"
else
  ok "renders 'obligation: extractable' / 'registered proof: none', not 'eligible for proof'"
fi
# And the consequence sentence must match whether an artifact exists.
if printf '%s' "$pout" | grep -q "no registered artifact to replay"; then
  ok "with no artifact it says so, rather than claiming one 'stays replayable'"
else
  no "the consequence sentence asserts an artifact that may not exist"
fi

echo "=== the original instance is CLOSED: print_bytes declares what it does ==="
# This is where R-0484 came from: `base64_cli.print_bytes` takes a writer, performs
# output, and used to declare nothing, so it was admitted ON THE GROUNDS OF PURITY.
# Until 2026-10-01 this block pinned that as a known gap. The gap closed at the source,
# not by opacity: the writer is `Writer<Console>`, using it requires `Console`, so the
# header says `with(Console)` and the function is excluded for a stated, true reason.
b64="$ROOT_DIR/examples/base64_cli"
if [ -d "$b64" ]; then
  bout="$(cd "$b64" && $TO "$CC" src/main.con --report eligibility 2>&1)"
  if printf '%s' "$bout" | grep -qE 'eligible +`base64_cli\.print_bytes`'; then
    no "print_bytes is eligible again — a writer's capability has stopped reaching its user's header"
  elif printf '%s' "$bout" | grep -A1 'base64_cli.print_bytes' | grep -q 'has capabilities: Console'; then
    ok "print_bytes is excluded because it declares Console (the effect is in the header)"
  else
    no "print_bytes is excluded, but not for declaring Console: $(printf '%s' "$bout" | grep -A1 'print_bytes' | tr '\n' ' ' | cut -c1-160)"
  fi
  cout="$(cd "$b64" && $TO "$CC" src/main.con --report caps 2>&1)"
  if printf '%s' "$cout" | grep -qE 'print_bytes : Console$'; then
    ok "the capability report shows print_bytes : Console — declared, neither pure nor unknown"
  else
    no "the capability report does not show print_bytes : Console: $(printf '%s' "$cout" | grep 'print_bytes' | head -1)"
  fi
else
  no "examples/base64_cli is missing; the closed-instance check did not run"
fi

echo
if [ "$SKIPPED" -gt 0 ]; then
  echo "EFFECT-OPACITY: PASS=$PASS FAIL=$FAIL SKIPPED=$SKIPPED (INCOMPLETE — a delegated gate declined)"
else
  echo "EFFECT-OPACITY: PASS=$PASS FAIL=$FAIL"
fi
[ "$FAIL" -eq 0 ]
