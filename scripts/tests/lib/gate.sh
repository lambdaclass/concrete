#!/usr/bin/env bash
# Shared gate harness (ROADMAP #34b). Source this from a check_*.sh gate:
#
#   source "$(dirname "${BASH_SOURCE[0]}")/lib/gate.sh"
#   gate_init "my-gate"
#   ...
#   rejects "label" prog.con E0217     # compile must fail with the code
#   agree   "label" prog.con "42"      # interp AND compiled print exactly this
#   accepts "label" prog.con           # compile must succeed
#   ok "label" / no "label"            # manual assertions
#   gate_finish                        # prints summary, exits nonzero on FAIL
#
# Rationale: 90+ gates each hand-roll the same ok/no/rejects helpers, and
# fixtures buried in per-gate heredocs are invisible to corpus sweeps (three
# stale-fixture CI failures on 2026-07-01/02 were exactly this). New gates
# should source this; old gates migrate opportunistically.

GATE_NAME="gate"
GATE_PASS=0
GATE_FAIL=0
GATE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
GATE_COMPILER="$GATE_ROOT/.lake/build/bin/concrete"
GATE_TMP="$(mktemp -d)"
trap 'rm -rf "$GATE_TMP"' EXIT

gate_init() {
  GATE_NAME="$1"
  [ -x "$GATE_COMPILER" ] || { echo "error: build first ($GATE_COMPILER missing)" >&2; exit 2; }
}

ok() { echo "  ok   $1"; GATE_PASS=$((GATE_PASS+1)); }
no() { echo "  FAIL $1"; GATE_FAIL=$((GATE_FAIL+1)); }

# rejects <label> <file.con> <error-code>: compile must fail mentioning the code.
rejects() {
  local label="$1" f="$2" code="$3" out
  out="$("$GATE_COMPILER" "$f" -o "$GATE_TMP/r.bin" 2>&1)"
  if [ $? -ne 0 ] && grep -q <<<"$out" "($code)"; then ok "$label"
  else no "$label (want $code; got: $(printf '%s' "$out" | head -1))"; fi
}

# accepts <label> <file.con>: compile must succeed.
accepts() {
  local label="$1" f="$2"
  if "$GATE_COMPILER" "$f" -o "$GATE_TMP/a.bin" >/dev/null 2>&1; then ok "$label"
  else no "$label (expected to compile)"; fi
}

# agree <label> <file.con> <expected>: interp and compiled binary both print
# exactly <expected> — the differential assertion.
agree() {
  local label="$1" f="$2" want="$3" i c
  i="$("$GATE_COMPILER" "$f" --interp 2>&1)"
  if "$GATE_COMPILER" "$f" -o "$GATE_TMP/g.bin" >/dev/null 2>&1; then
    c="$("$GATE_TMP/g.bin")"
  else
    c="<compile failed>"
  fi
  if [ "$i" = "$want" ] && [ "$c" = "$want" ]; then ok "$label"
  else no "$label (want $want, interp=$i compiled=$c)"; fi
}

# agree_both <label> <file.con>: interp and compiled must agree with EACH OTHER
# (value or matching trap), without pinning the value — for generated matrices.
agree_both() {
  local label="$1" f="$2" i irc c crc
  i="$("$GATE_COMPILER" "$f" --interp 2>&1)"; irc=$?
  if "$GATE_COMPILER" "$f" -o "$GATE_TMP/gb.bin" >/dev/null 2>&1; then
    c="$("$GATE_TMP/gb.bin" 2>&1)"; crc=$?
  else
    no "$label (compile failed)"; return
  fi
  if [ "$irc" -eq 0 ] && [ "$crc" -eq 0 ] && [ "$i" = "$c" ]; then ok "$label ($i)"
  elif [ "$irc" -ne 0 ] && [ "$crc" -ne 0 ]; then ok "$label (both trap)"
  else no "$label (interp rc=$irc '$i' vs compiled rc=$crc '$c')"; fi
}

gate_finish() {
  echo
  echo "$GATE_NAME: PASS=$GATE_PASS FAIL=$GATE_FAIL"
  [ "$GATE_FAIL" -eq 0 ]
}
