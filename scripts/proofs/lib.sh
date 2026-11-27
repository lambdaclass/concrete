#!/usr/bin/env bash
# Shared by the proof-integrity checks (scripts/proofs/*.sh). Sourced after gate_env.sh.
#
# These checks need Lean, which the gate shell does not carry: CI takes it from elan (lean-action),
# and a check started without `lake` on PATH refuses (exit 2), never passes. Each check reads the
# three libraries' built environments, so each first builds all three (a no-op when the
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

source_modules() {
  { echo "$1.lean"; find "$1" -name '*.lean'; } | sed -E 's#\.lean$##; s#/#.#g' | sort -u
}
spec_modules() { source_modules ConcreteSpec; }
evidence_modules() { source_modules ConcreteSpecEvidence; }

"$LAKE" build ConcreteSpec ConcreteSpecEvidence Concrete > "$TMP/lake-build.log" 2>&1 \
  || { tail -20 "$TMP/lake-build.log" >&2; echo "error: $CHECK_NAME: lake build of specification, evidence and compiler failed" >&2; exit 2; }

MARKER='-- CONTROL DECLARATIONS (negative controls are inserted here, in a temporary copy only)'
[ "$(grep -cxF -- "$MARKER" "$ENUMERATOR")" = 1 ] \
  || { echo "error: $ENUMERATOR must carry the control marker line exactly once" >&2; exit 2; }

# Resolve the file's imports from the current Lake configuration and sources. Lean
# can fall back to its toolchain for core modules, but a retired library on the
# workspace search path must never satisfy an import missing from the setup.
run_enumerator() {
  local src=$1
  local setup
  setup=$(mktemp "$TMP/enumerator-setup.XXXXXX") || return 1
  "$LAKE" setup-file "$src" > "$setup" || return 1
  "$LAKE" env env -u LEAN_PATH PROOF_SETUP="$setup" lean --setup="$setup" "$src"
}

population() {
  local mode=$1
  local control=${2:-}
  local src=$ENUMERATOR
  if [ -n "$control" ]; then
    src="$TMP/control-$RANDOM-$RANDOM.lean"
    # The trust controls must elaborate deliberate sorry declarations to inspect their axioms.
    awk -v m="$MARKER" -v c="$control" '{ print } $0 == m {
      print "section\nset_option warningAsError false"; while ((getline l < c) > 0) print l; print "end"
    }' "$ENUMERATOR" > "$src" || return 1
  fi
  PROOF_MODE=$mode run_enumerator "$src"
}

# closure_gap IMPORTED PACKAGE [SOURCES_OUT]: source modules absent from the environment or root.
closure_gap() {
  local pkg=$2
  local sources_out=${3:-"$TMP/closure-sources.txt"}
  if [ "$pkg" = SpecAndEvidence ]; then
    { spec_modules && evidence_modules; } | sort -u
  else
    source_modules "$pkg"
  fi > "$sources_out" || return 1
  sort -u "$1" > "$TMP/closure-imports.txt" || return 1
  comm -23 "$sources_out" "$TMP/closure-imports.txt"
}
