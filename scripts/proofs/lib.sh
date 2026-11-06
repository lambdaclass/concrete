#!/usr/bin/env bash
# Shared by the proof-integrity checks (scripts/proofs/*.sh). Sourced after gate_env.sh.
#
# These checks need Lean, which the gate shell does not carry: CI takes it from elan (lean-action),
# and a check started without `lake` on PATH refuses (exit 2), never passes. Each check reads the
# specification's built environment, so each first runs `lake build ConcreteSpec` (a no-op when the
# build is current): `lake env lean` would otherwise read whatever .olean files are there, stale
# ones included, and judge a library other than the sources.
#
#   population MODE [CONTROL-FILE]  prints scripts/proofs/theorems.lean's view in MODE. With a
#                                   CONTROL-FILE, the enumerator runs on a temporary copy with that
#                                   file's declarations inserted at its marker (negative controls).
#   spec_modules                    every module the ConcreteSpec sources declare (from file paths).

PROOFS_DIR="$ROOT_DIR/scripts/proofs"
ENUMERATOR="$PROOFS_DIR/theorems.lean"
STANDARD_AXIOMS="Classical.choice Quot.sound propext"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
finish(){ echo "$CHECK_NAME: PASS=$PASS FAIL=$FAIL"; [ "$FAIL" = 0 ] && exit 0 || exit 1; }
TMP="$(mktemp -d)" || { echo "FATAL: mktemp failed" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT
export LC_ALL=C

command -v lake >/dev/null || { echo "error: $CHECK_NAME needs Lean's lake on PATH (elan)" >&2; exit 2; }
LAKE="$(command -v lake)"
"$LAKE" build ConcreteSpec > "$TMP/lake-build.log" 2>&1 \
  || { tail -20 "$TMP/lake-build.log" >&2; echo "error: $CHECK_NAME: lake build ConcreteSpec failed" >&2; exit 2; }

MARKER='-- CONTROL DECLARATIONS (negative controls are inserted here, in a temporary copy only)'
[ "$(grep -cxF -- "$MARKER" "$ENUMERATOR")" = 1 ] \
  || { echo "error: $ENUMERATOR must carry the control marker line exactly once" >&2; exit 2; }

population() {
  local mode=$1 control=${2:-} src=$ENUMERATOR
  if [ -n "$control" ]; then
    src="$TMP/control-$RANDOM-$RANDOM.lean"
    awk -v m="$MARKER" -v c="$control" '{ print } $0 == m { while ((getline l < c) > 0) print l }' \
      "$ENUMERATOR" > "$src"
  fi
  PROOF_MODE=$mode "$LAKE" env lean "$src"
}

spec_modules() {
  { echo ConcreteSpec.lean; find ConcreteSpec -name '*.lean'; } | sed -E 's#\.lean$##; s#/#.#g' | sort -u
}
# closure_gap IMPORTED → the source modules missing from an import closure (PROOF_MODE=imported):
# a module the root does not import is invisible to every environment check
closure_gap() { spec_modules | comm -23 - <(sort -u "$1"); }
