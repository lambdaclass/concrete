#!/usr/bin/env bash
# Phase 7 workload 3 gate: hexdump must stay byte-identical to the system
# `xxd` (default form + -s/-n + edge cases) and keep its 13t exit codes.
# xxd is the DIFFERENTIAL ORACLE — if it is absent, the format legs are
# skipped loudly (the build/exit-code legs still run).
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

( cd examples/hexdump && "$C" build ) >/dev/null 2>&1 \
  && ok "hexdump builds" || { no "hexdump build failed"; echo "HEXDUMP: PASS=$PASS FAIL=$FAIL"; exit 1; }
B="examples/hexdump/hexdump"

# fixtures: text+binary, empty, size not a multiple of 16
printf 'Hello, world!\nBinary\x00\x01\x02\xff tail to cross a few lines......' > "$TMP/t.bin"
: > "$TMP/empty.bin"
head -c 4103 /dev/urandom > "$TMP/rand.bin"

# exit codes (13t: usage=2, io=1, ok=0) — oracle-free
"$B" "$TMP/t.bin" >/dev/null 2>&1;      [ $? -eq 0 ] && ok "clean run exits 0"        || no "clean run rc != 0"
"$B" >/dev/null 2>&1;                   [ $? -eq 2 ] && ok "no args exits 2 (usage)"  || no "no-args rc != 2"
"$B" -n zzz "$TMP/t.bin" >/dev/null 2>&1; [ $? -eq 2 ] && ok "malformed -n exits 2"   || no "malformed -n rc != 2"
"$B" "$TMP/nope" >/dev/null 2>&1;       [ $? -eq 1 ] && ok "missing file exits 1"     || no "missing-file rc != 1"

if ! command -v xxd >/dev/null 2>&1; then
  echo "  SKIP xxd not available — differential format legs skipped"
else
  for case in "" "-s 5 -n 20" "-s 4096 -n 16" "-s 999999" "-n 0"; do
    # shellcheck disable=SC2086
    "$B" $case "$TMP/rand.bin" > "$TMP/ours.txt" 2>/dev/null
    # xxd spells the length flag -l
    xcase=${case/-n/-l}
    # shellcheck disable=SC2086
    xxd $xcase "$TMP/rand.bin" > "$TMP/xxd.txt" 2>/dev/null
    diff -q "$TMP/ours.txt" "$TMP/xxd.txt" >/dev/null \
      && ok "xxd-identical: '${case:-default}' (4103-byte random)" \
      || no "differs from xxd: '${case:-default}'"
  done
  "$B" "$TMP/t.bin" > "$TMP/ours.txt"; xxd "$TMP/t.bin" > "$TMP/xxd.txt"
  diff -q "$TMP/ours.txt" "$TMP/xxd.txt" >/dev/null \
    && ok "xxd-identical: text+binary fixture" || no "differs from xxd: text fixture"
  "$B" "$TMP/empty.bin" > "$TMP/ours.txt"; xxd "$TMP/empty.bin" > "$TMP/xxd.txt"
  diff -q "$TMP/ours.txt" "$TMP/xxd.txt" >/dev/null \
    && ok "xxd-identical: empty file" || no "differs from xxd: empty file"
fi

echo
echo "HEXDUMP: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
