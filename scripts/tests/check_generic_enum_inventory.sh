#!/usr/bin/env bash
# R-0001 / bug 051 — permanent user-generic-enum fixture inventory gate.
#
# Turns the one-off "loose whole-corpus sweep" (which a too-strict grep once
# missed, reddening the board) into a fail-closed inventory. EVERY .con file
# anywhere that declares a USER generic enum (any modifier: `enum Copy Foo<T>`,
# `enum Foo <T>`, …; builtin Option/Result excluded) must be explicitly
# classified as one of:
#
#   (a) E0808 CONTAINMENT coverage  — asserted with `run_err "<file>" "(E0808)"`
#       in run_tests.sh (rejected fail-closed until per-instantiation enum
#       monomorphization lands), OR
#   (b) POSITIVE monomorphization coverage — asserted with `run_ok "<file>" <n>`
#       in run_tests.sh (compiles + runs correctly; valid only AFTER the root
#       fix lands and lifts E0808 for that shape).
#
# A user-generic-enum fixture that is neither — or that lives outside
# tests/programs where run_ok/run_err do not reach — FAILS. This forces a
# deliberate classification decision on every future generic-enum fixture and
# prevents an un-vetted mixed-layout program from silently compiling.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
RUN_TESTS="scripts/tests/run_tests.sh"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Loose pattern: `enum` + optional modifier words (Copy, pub, …) + Name + `<`.
PAT='enum ([A-Za-z]+ )*[A-Z][A-Za-z0-9_]*[[:space:]]*<'

echo "=== every user-generic-enum .con fixture is classified (containment or positive) ==="
mapfile -t FILES < <(grep -rlE --include='*.con' "$PAT" . 2>/dev/null \
  | grep -vE '/\.lake/' \
  | while IFS= read -r f; do
      # exclude files whose only generic-enum matches are Option/Result
      if grep -oE "$PAT" "$f" | grep -qvE 'enum (Copy |pub )*(Option|Result)[[:space:]]*<'; then echo "$f"; fi
    done | sort -u)

if [ "${#FILES[@]}" -eq 0 ]; then
  no "sweep found NO user-generic-enum fixtures — pattern drift? (expected >=1: the historical mixed-layout witness)"
fi

for f in "${FILES[@]}"; do
  base="$(basename "$f")"
  in_programs=0; [[ "$f" == ./tests/programs/* || "$f" == tests/programs/* ]] && in_programs=1
  # classification from run_tests.sh
  errline="$(grep -E "run_err\s+\"[^\"]*${base%.con}\.con\"" "$RUN_TESTS" 2>/dev/null || true)"
  okline="$(grep -E "run_ok\s+\"[^\"]*${base%.con}\.con\"" "$RUN_TESTS" 2>/dev/null || true)"
  if [ "$in_programs" -ne 1 ]; then
    no "$f declares a user generic enum but is OUTSIDE tests/programs — no run_ok/run_err reaches it; classify it via a corpus with an assertion mechanism first"
  elif [ -n "$errline" ] && grep -q '(E0808)' <<<"$errline"; then
    ok "$base — E0808 containment coverage"
  elif [ -n "$errline" ]; then
    no "$base — run_err present but does NOT require (E0808) specifically (a parser/checker/linker/crash failure would masquerade as containment)"
  elif [ -n "$okline" ]; then
    ok "$base — positive monomorphization coverage (run_ok) [valid only post-root-fix]"
  else
    no "$base — UNCLASSIFIED user-generic-enum fixture: add run_err \"...\" \"(E0808)\" (containment) or run_ok \"...\" <n> (positive). See docs/bugs/051_generic_enums_not_monomorphized.md"
  fi
done

echo
echo "GENERIC-ENUM-INVENTORY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
