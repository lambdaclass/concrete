#!/usr/bin/env bash
# Phase 7 workload 4 gate: tar_list must stay byte-identical to the system
# `tar -tf` (the differential oracle) across file-size edges (0/511/512/513),
# directories, and >100-char paths (ustar prefix field), and keep its 13t
# exit codes. tar is required (present on macOS + ubuntu CI).
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

( cd examples/tar_list && "$C" build ) >/dev/null 2>&1 \
  && ok "tar_list builds" || { no "tar_list build failed"; echo "TAR-LIST: PASS=$PASS FAIL=$FAIL"; exit 1; }
B="$ROOT_DIR/examples/tar_list/tar_list"

# fixture archive: size edges + empty file + directory tree
( cd "$TMP"
  echo hello > a.txt
  head -c 511 /dev/urandom > b511.bin
  head -c 512 /dev/urandom > c512.bin
  head -c 513 /dev/urandom > d513.bin
  : > empty.txt
  mkdir -p sub/deep && echo nested > sub/deep/n.txt
  tar cf t.tar a.txt b511.bin c512.bin d513.bin empty.txt sub
  # >100-char path exercises the ustar prefix/name split
  longdir=$(python3 -c "print('p'*60)")
  mkdir -p "$longdir" && echo x > "$longdir/$(python3 -c "print('q'*60)")"
  tar --format=ustar -cf long.tar "$longdir"
) >/dev/null 2>&1

"$B" "$TMP/t.tar" > "$TMP/ours.txt" 2>/dev/null; rc=$?
tar -tf "$TMP/t.tar" > "$TMP/theirs.txt"
[ $rc -eq 0 ] && diff -q "$TMP/ours.txt" "$TMP/theirs.txt" >/dev/null \
  && ok "tar-identical: size edges + dirs (rc 0)" || no "differs from tar -tf (rc=$rc)"

"$B" "$TMP/long.tar" > "$TMP/ours2.txt" 2>/dev/null
tar -tf "$TMP/long.tar" > "$TMP/theirs2.txt"
diff -q "$TMP/ours2.txt" "$TMP/theirs2.txt" >/dev/null \
  && ok "tar-identical: >100-char path (prefix field join)" || no "prefix-field path differs"

# 13t exit codes
"$B" >/dev/null 2>&1;                 [ $? -eq 2 ] && ok "no args -> 2 (usage)"       || no "no-args rc"
"$B" a b >/dev/null 2>&1;             [ $? -eq 2 ] && ok "two positionals -> 2"       || no "arity rc"
"$B" "$TMP/nope.tar" >/dev/null 2>&1; [ $? -eq 1 ] && ok "missing file -> 1"          || no "missing rc"
head -c 2048 /dev/urandom > "$TMP/junk.tar"
"$B" "$TMP/junk.tar" >/dev/null 2>&1; [ $? -eq 1 ] && ok "non-ustar junk -> 1"        || no "junk rc"
head -c 700 "$TMP/t.tar" > "$TMP/trunc.tar"
"$B" "$TMP/trunc.tar" >/dev/null 2>&1; [ $? -eq 1 ] && ok "truncated archive -> 1 (no trap)" || no "trunc rc"

echo
echo "TAR-LIST: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
