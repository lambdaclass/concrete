#!/usr/bin/env bash
# Phase 7 workload 5 gate: envcfg resolves KEY=VALUE config files with
# environment override (env wins, file value is the fallback) and keeps its
# 13t exit codes. This is the first gate compiling std.env — bug 039
# (import-alias rebinding get -> args_get, segfault) lived exactly here, so
# the env-override legs are load-bearing for the whole env boundary.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

( cd examples/envcfg && "$C" build ) >/dev/null 2>&1 \
  && ok "envcfg builds" || { no "envcfg build failed"; echo "ENVCFG: PASS=$PASS FAIL=$FAIL"; exit 1; }
B="$ROOT_DIR/examples/envcfg/envcfg"

cat > "$TMP/app.conf" <<'EOF'
# database settings
DB_HOST = localhost
DB_PORT=5432

  CACHE = off
EMPTY_VAL=
EOF

# file values only (comments/blanks skipped, whitespace trimmed)
cat > "$TMP/want1.txt" <<'EOF'
DB_HOST=localhost
DB_PORT=5432
CACHE=off
EMPTY_VAL=
EOF
env -u DB_HOST -u DB_PORT -u CACHE -u EMPTY_VAL "$B" "$TMP/app.conf" > "$TMP/got1.txt" 2>/dev/null; rc=$?
[ $rc -eq 0 ] && diff -q "$TMP/want1.txt" "$TMP/got1.txt" >/dev/null \
  && ok "file values: comments/blanks skipped, spaces trimmed (rc 0)" \
  || { no "file-values output differs (rc=$rc)"; diff "$TMP/want1.txt" "$TMP/got1.txt" | head -4; }

# env override wins over file value; untouched keys keep file values
cat > "$TMP/want2.txt" <<'EOF'
DB_HOST=prod.example.com
DB_PORT=5432
CACHE=on
EMPTY_VAL=
EOF
env -u DB_PORT -u EMPTY_VAL DB_HOST=prod.example.com CACHE=on \
  "$B" "$TMP/app.conf" > "$TMP/got2.txt" 2>/dev/null; rc=$?
[ $rc -eq 0 ] && diff -q "$TMP/want2.txt" "$TMP/got2.txt" >/dev/null \
  && ok "env override wins, others keep file values (bug 039 leg)" \
  || { no "env-override output differs (rc=$rc)"; diff "$TMP/want2.txt" "$TMP/got2.txt" | head -4; }

# env override with empty value still wins (present-but-empty != absent)
env -u DB_PORT -u CACHE -u EMPTY_VAL DB_HOST= "$B" "$TMP/app.conf" > "$TMP/got3.txt" 2>/dev/null
head -1 "$TMP/got3.txt" | grep -qx "DB_HOST=" \
  && ok "present-but-empty env var overrides to empty" || no "empty env override"

# last line without trailing newline is still processed
printf 'A=1\nB=2' > "$TMP/nonl.conf"
env -u A -u B "$B" "$TMP/nonl.conf" > "$TMP/got4.txt" 2>/dev/null
grep -qx "B=2" "$TMP/got4.txt" \
  && ok "final line without newline processed" || no "no-trailing-newline line lost"

# 13t exit codes
"$B" >/dev/null 2>&1;                  [ $? -eq 2 ] && ok "no args -> 2 (usage)"   || no "no-args rc"
"$B" a b >/dev/null 2>&1;              [ $? -eq 2 ] && ok "two positionals -> 2"   || no "arity rc"
"$B" "$TMP/nope.conf" >/dev/null 2>&1; [ $? -eq 1 ] && ok "missing file -> 1"      || no "missing rc"
printf 'GOOD=1\nnot a kv line\n' > "$TMP/bad.conf"
"$B" "$TMP/bad.conf" >/dev/null 2>&1;  [ $? -eq 1 ] && ok "malformed line -> 1 (good lines still printed)" || no "malformed rc"
printf '=value\n' > "$TMP/nokey.conf"
"$B" "$TMP/nokey.conf" >/dev/null 2>&1; [ $? -eq 1 ] && ok "empty key -> 1"        || no "empty-key rc"

echo
echo "ENVCFG: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
