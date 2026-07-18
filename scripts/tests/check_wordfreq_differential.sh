#!/usr/bin/env bash
# Phase 7 workload 6 gate: wordfreq must stay byte-identical to the system
# oracle (LC_ALL=C tr | sort | uniq -c — C locale everywhere: the oracle
# must be byte-oriented like we are, or IT mangles multibyte words) across
# real text, scale (200k words), and edge inputs, and keep its 13t exit
# codes. This is the first gate driving OrderedMap at data scale — the
# in-order traversal IS the differential axis (a broken rotation or
# comparator shows up as misordered output, not a crash).
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

( cd examples/wordfreq && "$C" build ) >/dev/null 2>&1 \
  && ok "wordfreq builds" || { no "wordfreq build failed"; echo "WORDFREQ: PASS=$PASS FAIL=$FAIL"; exit 1; }
B="$ROOT_DIR/examples/wordfreq/wordfreq"

oracle(){ LC_ALL=C tr -s '[:space:]' '\n' < "$1" | LC_ALL=C sed '/^$/d' \
          | LC_ALL=C sort | LC_ALL=C uniq -c | awk '{print $2" "$1}'; }

# 1. real text with multibyte words: the repo's own ROADMAP
"$B" ROADMAP.md > "$TMP/ours1.txt" 2>/dev/null; rc=$?
oracle ROADMAP.md > "$TMP/want1.txt"
[ $rc -eq 0 ] && diff -q "$TMP/want1.txt" "$TMP/ours1.txt" >/dev/null \
  && ok "oracle-identical: ROADMAP.md ($(wc -l < "$TMP/ours1.txt" | tr -d ' ') distinct words, multibyte included)" \
  || { no "ROADMAP.md differs (rc=$rc)"; diff "$TMP/want1.txt" "$TMP/ours1.txt" | head -3; }

# 2. scale: 200k words, fixed seed (map growth + rebalancing under load)
python3 -c "
import random
random.seed(7)
words=['alpha','beta','gamma','delta','x1','xx','the','a','zz9']+['w%d'%i for i in range(400)]
print(' '.join(random.choice(words) for _ in range(200000)))" > "$TMP/big.txt"
"$B" "$TMP/big.txt" > "$TMP/ours2.txt" 2>/dev/null; rc=$?
oracle "$TMP/big.txt" > "$TMP/want2.txt"
[ $rc -eq 0 ] && diff -q "$TMP/want2.txt" "$TMP/ours2.txt" >/dev/null \
  && ok "oracle-identical: 200k words / 409 distinct" || no "200k-word output differs (rc=$rc)"

# 3. edges: empty file, whitespace-only, single word no newline, all-same word
: > "$TMP/empty.txt"
"$B" "$TMP/empty.txt" > "$TMP/got.txt" 2>/dev/null; rc=$?
[ $rc -eq 0 ] && [ ! -s "$TMP/got.txt" ] && ok "empty file -> empty output, rc 0" || no "empty-file (rc=$rc)"
printf '  \t\n \n' > "$TMP/ws.txt"
"$B" "$TMP/ws.txt" > "$TMP/got.txt" 2>/dev/null; rc=$?
[ $rc -eq 0 ] && [ ! -s "$TMP/got.txt" ] && ok "whitespace-only -> empty output, rc 0" || no "whitespace-only (rc=$rc)"
printf 'lonely' > "$TMP/one.txt"
"$B" "$TMP/one.txt" > "$TMP/got.txt" 2>/dev/null
grep -qx "lonely 1" "$TMP/got.txt" && ok "single word, no trailing newline" || no "single-word case"
python3 -c "print('same '*50000)" > "$TMP/same.txt"
"$B" "$TMP/same.txt" > "$TMP/got.txt" 2>/dev/null
grep -qx "same 50000" "$TMP/got.txt" && ok "50k repeats of one word -> count 50000" || no "repeat-count case"

# 4. 13t exit codes
"$B" >/dev/null 2>&1;                 [ $? -eq 2 ] && ok "no args -> 2 (usage)"  || no "no-args rc"
"$B" a b >/dev/null 2>&1;             [ $? -eq 2 ] && ok "two positionals -> 2"  || no "arity rc"
"$B" "$TMP/nope.txt" >/dev/null 2>&1; [ $? -eq 1 ] && ok "missing file -> 1"     || no "missing rc"
printf 'ok \xff\xfe bad\n' > "$TMP/binword.txt"
"$B" "$TMP/binword.txt" >/dev/null 2>&1; [ $? -eq 1 ] && ok "invalid-UTF-8 word -> 1 (checked crossing)" || no "invalid-utf8 rc"

echo
echo "WORDFREQ: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
