#!/usr/bin/env bash
# The one test manifest is valid and owns every test file under tests/ (except its own manifest and
# tests/golden/). The shared reader checks every row; this check also compares the con: inventory in
# both directions. Controls plant malformed rows and source-inventory defects in private copies and
# require the matching diagnostic. Each mutation must change its copy before the control runs.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
MANIFEST=${CONCRETE_TEST_MANIFEST:-tests/manifest.tsv}
[[ "$MANIFEST" = /* ]] || MANIFEST="$ROOT_DIR/$MANIFEST"
TEST_DIR=$(dirname "$MANIFEST")
TEST_PARENT=$(cd "$TEST_DIR/.." && pwd)
cd "$TEST_PARENT"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
PASS=0
FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS + 1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL + 1)); }
reader=(python3 -I "$ROOT_DIR/scripts/checks/lib/test_manifest.py")

if out=$("${reader[@]}" validate "$MANIFEST" 2>&1); then
  ok "$out"
else
  no "the manifest is invalid: $out"
fi
if out=$("${reader[@]}" self-test 2>&1); then
  ok "$out"
else
  no "the reader's self-test failed: $out"
fi

if "${reader[@]}" rows "$MANIFEST" >"$TMP/rows"; then
  awk -F '\t' '$2 ~ /^con:/ {sub(/^con:/, "", $2); print $2}' "$TMP/rows" |
    LC_ALL=C sort >"$TMP/listed"
  find tests -type f ! -path tests/manifest.tsv ! -path 'tests/golden/*' |
    LC_ALL=C sort >"$TMP/files"
  comm -23 "$TMP/files" "$TMP/listed" >"$TMP/unlisted"
  comm -13 "$TMP/files" "$TMP/listed" >"$TMP/not-on-disk"
  if [ ! -s "$TMP/unlisted" ] && [ ! -s "$TMP/not-on-disk" ]; then
    ok "every test file has exactly one con: row"
  else
    while IFS= read -r file; do no "unlisted test file: $file"; done <"$TMP/unlisted"
    while IFS= read -r file; do no "con: row has no discovered file: $file"; done <"$TMP/not-on-disk"
  fi
else
  no "cannot enumerate the test inventory"
fi

control() { # control LABEL EXPECTED-MESSAGE MUTATION-FUNCTION CHANGED-PATH
  local label=$1 expected=$2 mutation=$3 changed=$4 dir="$TMP/control-$1" rc=0
  mkdir -p "$dir"
  if ! cp -R "$TEST_DIR" "$dir/tests"; then no "control: $label could not copy tests/"; return; fi
  if ! "$mutation" "$dir"; then no "control: $label mutation failed"; return; fi
  if [ ! -e "$dir/tests/$changed" ]; then
    no "control: $label mutation produced no $changed"; return
  fi
  if cmp -s "$TEST_DIR/$changed" "$dir/tests/$changed" 2>/dev/null; then
    no "control: $label mutation changed nothing"; return
  fi
  CONCRETE_TEST_MANIFEST="$dir/tests/manifest.tsv" CONCRETE_TEST_MANIFEST_SKIP_CONTROLS=1 \
    bash "$ROOT_DIR/scripts/checks/test_manifest.sh" >"$dir/log" 2>&1 || rc=$?
  if [ "$rc" -ne 0 ] && has_match -F "$expected" <"$dir/log"; then
    ok "control: $label is refused for the planted reason"
  else
    no "control: $label was not refused as '$expected'"
    cat "$dir/log"
  fi
}
duplicate_id() {
  local row
  row=$(awk -F '\t' '$1 == "answer" {print; exit}' "$1/tests/manifest.tsv")
  [ -n "$row" ] || { echo "duplicate_id: answer row lookup was empty" >&2; return 1; }
  printf '%s\n' "$row" >>"$1/tests/manifest.tsv"
}
unlisted_source() { mkdir -p "$1/tests/slice0" && : >"$1/tests/slice0/x.con"; }
missing_source() { printf 'missing\tcon:tests/slice0/missing.con\texit 0\tslice0\n' >>"$1/tests/manifest.tsv"; }
exit_256() { printf 'bad_exit\tbir:other\texit 256\tslice0\n' >>"$1/tests/manifest.tsv"; }
bad_trap() { printf 'bad_trap\tbir:other\ttrap foo\tslice0\n' >>"$1/tests/manifest.tsv"; }
wrong_columns() { printf 'bad_columns\tbir:answer\texit 0\n' >>"$1/tests/manifest.tsv"; }
duplicate_bir() { printf 'duplicate_bir\tbir:answer\texit 0\tslice0\n' >>"$1/tests/manifest.tsv"; }
refuse_bir() { printf 'refused_bir\tbir:other\trefuse SYN-5\tslice0\n' >>"$1/tests/manifest.tsv"; }

if [ "${CONCRETE_TEST_MANIFEST_SKIP_CONTROLS:-0}" != 1 ]; then
  control duplicate-id "duplicate id 'answer'" duplicate_id manifest.tsv
  control unlisted-source "unlisted test file: tests/slice0/x.con" unlisted_source slice0/x.con
  control missing-source "con: file 'tests/slice0/missing.con' does not exist" missing_source manifest.tsv
  control exit-256 "exit status 256 is outside 0..255" exit_256 manifest.tsv
  control malformed-trap "invalid expect 'trap foo'" bad_trap manifest.tsv
  control wrong-columns "needs 4 tab-separated columns" wrong_columns manifest.tsv
  control duplicate-bir "duplicate bir program 'answer'" duplicate_bir manifest.tsv
  control refuse-bir "bir: row 'refused_bir' cannot use refuse" refuse_bir manifest.tsv
fi

echo "test_manifest: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
