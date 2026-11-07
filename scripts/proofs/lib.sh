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
#   prune_orphans [BUILD-DIR]       delete ConcreteSpec build files (.olean, .ilean, .trace, .hash, IR)
#                                   of modules with no source, and print each one (ISS-0016). Run before
#                                   the build below, so a build directory restored from an older cache
#                                   cannot carry a deleted or renamed module into any check.

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

spec_modules() {
  { echo ConcreteSpec.lean; find ConcreteSpec -name '*.lean'; } | sed -E 's#\.lean$##; s#/#.#g' | sort -u
}
prune_orphans() {
  local build="${1:-.lake/build}" d f rel m
  # Deletion keys on this list, so a partial list must stop the prune before anything is removed:
  # the enumeration must succeed, include the root module, and name only modules whose source exists.
  spec_modules > "$TMP/prune-modules.txt" || { echo "prune_orphans: listing the source modules failed" >&2; return 1; }
  grep -qxF ConcreteSpec "$TMP/prune-modules.txt" || { echo "prune_orphans: the source list lacks ConcreteSpec" >&2; return 1; }
  while read -r m; do [ -f "${m//.//}.lean" ] || { echo "prune_orphans: listed module $m has no source" >&2; return 1; }; done < "$TMP/prune-modules.txt"
  for d in "$build/lib/lean" "$build/ir"; do
    [ -d "$d" ] || continue
    find "$d" -type f \( -path "$d/ConcreteSpec/*" -o -name 'ConcreteSpec.*' \) | while read -r f; do
      rel="${f#"$d"/}"; m="$(dirname "$rel")/$(basename "$rel" | cut -d. -f1)"; m="${m#./}"; m="${m//\//.}"
      grep -qxF "$m" "$TMP/prune-modules.txt" || { rm -f "$f" && echo "pruned orphan build file $f (no source module $m)"; }
    done
  done
}
prune_orphans >&2 || { echo "error: $CHECK_NAME: pruning orphaned build files failed" >&2; exit 2; }
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

# closure_gap IMPORTED → the source modules missing from an import closure (PROOF_MODE=imported):
# a module the root does not import is invisible to every environment check
closure_gap() { spec_modules | comm -23 - <(sort -u "$1"); }
