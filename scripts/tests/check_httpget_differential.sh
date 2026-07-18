#!/usr/bin/env bash
# Phase 7 workload 7 gate: httpget over real loopback TCP — body bytes must
# be identical to the served file (text AND binary) with python3's
# http.server as the peer, plus 13t exit codes. First compiled consumer of
# TcpStream::write_all/read_all (the coverage fixture only proves
# bind/connect setup); building it found bug 045 (match-binder shadowing).
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
TMP=$(mktemp -d)
SRV_PID=""
cleanup(){ [ -n "$SRV_PID" ] && kill "$SRV_PID" 2>/dev/null; rm -rf "$TMP"; }
trap cleanup EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

( cd examples/httpget && "$C" build ) >/dev/null 2>&1 \
  && ok "httpget builds" || { no "httpget build failed"; echo "HTTPGET: PASS=$PASS FAIL=$FAIL"; exit 1; }
B="$ROOT_DIR/examples/httpget/httpget"

# fixtures: text + binary (all 256 byte values, 8KB — multiple recv() fills)
printf 'hello over tcp\nsecond line\n' > "$TMP/hello.txt"
python3 -c "import sys; sys.stdout.buffer.write(bytes(range(256))*32)" > "$TMP/blob.bin"

# loopback server on a port derived from PID (retry once on collision)
PORT=$(( ($$ % 20000) + 24000 ))
( cd "$TMP" && exec python3 -m http.server "$PORT" --bind 127.0.0.1 ) >/dev/null 2>&1 &
SRV_PID=$!
ready=false
for _ in $(seq 1 50); do
  curl -s -o /dev/null "http://127.0.0.1:$PORT/hello.txt" && { ready=true; break; }
  sleep 0.2
done
$ready && ok "loopback http.server ready on $PORT" || { no "server never became ready"; echo "HTTPGET: PASS=$PASS FAIL=$FAIL"; exit 1; }

"$B" 127.0.0.1 "$PORT" /hello.txt > "$TMP/got.txt" 2>/dev/null; rc=$?
[ $rc -eq 0 ] && diff -q "$TMP/hello.txt" "$TMP/got.txt" >/dev/null \
  && ok "text body byte-identical to served file (rc 0)" || no "text body differs (rc=$rc)"

"$B" 127.0.0.1 "$PORT" /blob.bin > "$TMP/got.bin" 2>/dev/null; rc=$?
[ $rc -eq 0 ] && cmp -s "$TMP/blob.bin" "$TMP/got.bin" \
  && ok "8KB binary body byte-identical (all 256 byte values, multi-recv)" || no "binary body differs (rc=$rc)"

# curl cross-check: two independent clients agree
curl -s "http://127.0.0.1:$PORT/blob.bin" > "$TMP/curl.bin"
cmp -s "$TMP/curl.bin" "$TMP/got.bin" && ok "agrees with curl byte-for-byte" || no "curl cross-check differs"

# 13t exit codes
"$B" 127.0.0.1 "$PORT" /nope.txt >/dev/null 2>&1; [ $? -eq 1 ] && ok "404 -> 1" || no "404 rc"
"$B" 127.0.0.1 1 /x >/dev/null 2>&1;              [ $? -eq 1 ] && ok "connection refused -> 1" || no "refused rc"
"$B" >/dev/null 2>&1;                             [ $? -eq 2 ] && ok "no args -> 2 (usage)" || no "no-args rc"
"$B" 127.0.0.1 notaport /x >/dev/null 2>&1;       [ $? -eq 2 ] && ok "non-numeric port -> 2" || no "port-parse rc"

echo
echo "HTTPGET: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
