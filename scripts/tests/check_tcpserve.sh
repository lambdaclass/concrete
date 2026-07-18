#!/usr/bin/env bash
# Phase 7 workload 8 gate: tcpserve — the SERVER side of the socket
# (TcpListener::accept + accepted-stream lifecycle), the last never-compiled
# std.net path. Every leg asserts exit status, stdout, and stderr
# SEPARATELY. IPv4 only by design (std.net sockaddr is AF_INET); IPv6 is
# classified out of scope here, not accidentally tested. Scope honesty:
# the tiny server-side read buffer forces PARTIAL READS by construction;
# write_all is executed end-to-end but loopback does not guarantee the OS
# splits writes — deterministic short-write coverage belongs to the
# planned capability-fault simulation gate (docs/DETERMINISTIC_SIMULATION.md).
# A watchdog kills a hung server and reports a DISTINCT infra-timeout
# failure, never conflated with expected application failures. All
# processes and temp files are cleaned in the EXIT trap even when an
# assertion fails.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
TMP=$(mktemp -d)
PIDS=""
cleanup(){ for p in $PIDS; do kill "$p" 2>/dev/null; done; rm -rf "$TMP"; }
trap cleanup EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

( cd examples/tcpserve && "$C" build ) >/dev/null 2>&1 \
  && ok "tcpserve builds" || { no "tcpserve build failed"; echo "TCPSERVE: PASS=$PASS FAIL=$FAIL"; exit 1; }
B="$ROOT_DIR/examples/tcpserve/tcpserve"
PORT_BASE=$(( ($$ % 10000) + 30000 ))

# start_server <port> <tag>: run under a watchdog; READY-wait with timeout.
# Watchdog kill is a DISTINCT infra failure (rc 99 recorded in <tag>.rc).
start_server(){ local port="$1" tag="$2"
  ( "$B" "$port" > "$TMP/$tag.out" 2> "$TMP/$tag.err"; echo $? > "$TMP/$tag.rc" ) &
  local wrapper=$!
  PIDS="$PIDS $wrapper"
  ( sleep 20; if kill "$wrapper" 2>/dev/null; then echo 99 > "$TMP/$tag.rc"; fi ) &
  PIDS="$PIDS $!"
  for _ in $(seq 1 50); do grep -q READY "$TMP/$tag.out" 2>/dev/null && return 0; sleep 0.1; done
  return 1
}
finish_server(){ local tag="$1"   # wait (bounded) for the rc file
  for _ in $(seq 1 100); do [ -s "$TMP/$tag.rc" ] && break; sleep 0.1; done
  cat "$TMP/$tag.rc" 2>/dev/null || echo 98
}

echo "=== happy path: one connection, curl as the external client ==="
P=$PORT_BASE
start_server "$P" happy || no "server never printed READY"
curl -s --max-time 10 -o "$TMP/body.bin" "http://127.0.0.1:$P/x"
crc=$?
rc=$(finish_server happy)
python3 -c "import sys; sys.stdout.buffer.write(bytes(range(256)))" > "$TMP/want.bin"
[ "$rc" = "0" ] && ok "server exit 0 after exactly one request" || no "server rc=$rc (99 = INFRA TIMEOUT, not an app failure)"
[ "$crc" -eq 0 ] && cmp -s "$TMP/body.bin" "$TMP/want.bin" \
  && ok "body = all 256 byte values, compared BYTEWISE after curl's header strip" \
  || no "body differs or curl failed (rc=$crc)"
[ "$(cat "$TMP/happy.out")" = "READY
DONE" ] && ok "stdout is exactly READY then DONE" || no "stdout: [$(tr '\n' ' ' < "$TMP/happy.out")]"
[ ! -s "$TMP/happy.err" ] && ok "stderr empty on success" || no "stderr not empty: $(cat "$TMP/happy.err")"

echo "=== headers stripped bytewise by a checked reference client ==="
P=$((PORT_BASE + 1))
start_server "$P" ref || no "server never printed READY (ref leg)"
python3 - "$P" > "$TMP/ref.body" <<'PYEOF'
import socket, sys
port = int(sys.argv[1])
c = socket.create_connection(("127.0.0.1", port), timeout=10)
c.sendall(b"GET /y HTTP/1.0\r\n\r\n")
buf = b""
while True:
    d = c.recv(4096)
    if not d: break
    buf += d
c.close()
# strip headers BYTEWISE — never decode the body as text
i = buf.find(b"\r\n\r\n")
assert i >= 0, "no header terminator in response"
assert buf[:12] == b"HTTP/1.0 200", buf[:20]
sys.stdout.buffer.write(buf[i+4:])
PYEOF
rc=$(finish_server ref)
[ "$rc" = "0" ] && cmp -s "$TMP/ref.body" "$TMP/want.bin" \
  && ok "reference client (raw socket, bytewise strip) sees the identical body" \
  || no "reference-client body differs (rc=$rc)"

echo "=== failure classification (injected) ==="
# bind failure: pre-occupy the port
P=$((PORT_BASE + 2))
python3 -c "
import socket, time
s = socket.socket(); s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(('127.0.0.1', $P)); s.listen(1); time.sleep(15)" &
PIDS="$PIDS $!"
sleep 0.5
"$B" "$P" > "$TMP/bind.out" 2> "$TMP/bind.err"; rc=$?
[ "$rc" -eq 1 ] && ok "occupied port -> exit 1" || no "bind-occupied rc=$rc"
grep -q "bind failed" "$TMP/bind.err" && ok "bind failure named on stderr" || no "bind stderr: [$(cat "$TMP/bind.err")]"
[ ! -s "$TMP/bind.out" ] && ok "no READY and no DONE on bind failure" || no "stdout leaked: [$(cat "$TMP/bind.out")]"

# early EOF: peer sends a partial request with binary junk, then closes
P=$((PORT_BASE + 3))
start_server "$P" eof || no "server never printed READY (eof leg)"
python3 -c "
import socket
c = socket.create_connection(('127.0.0.1', $P), timeout=10)
c.sendall(b'GET /x HT\x00\xff')
c.close()"
rc=$(finish_server eof)
[ "$rc" = "1" ] && ok "peer close before terminator -> exit 1" || no "early-eof rc=$rc"
grep -q "peer closed" "$TMP/eof.err" && ok "early EOF named on stderr" || no "eof stderr: [$(cat "$TMP/eof.err")]"
grep -q DONE "$TMP/eof.out" && no "DONE leaked on failure path" || ok "no DONE on early-EOF failure (READY only)"

echo "=== partial reads exercised by a dribbling client ==="
P=$((PORT_BASE + 4))
start_server "$P" drib || no "server never printed READY (dribble leg)"
python3 -c "
import socket, time
c = socket.create_connection(('127.0.0.1', $P), timeout=10)
for chunk in [b'GET /z', b' HTTP/1.0', b'\r\n', b'\r', b'\n']:
    c.sendall(chunk); time.sleep(0.05)     # cross-buffer terminator split
buf = b''
while True:
    d = c.recv(4096)
    if not d: break
    buf += d
c.close()
import sys
sys.exit(0 if buf.find(b'\r\n\r\n') >= 0 and buf[buf.find(b'\r\n\r\n')+4:] == bytes(range(256)) else 1)"
drc=$?
rc=$(finish_server drib)
[ "$rc" = "0" ] && [ "$drc" -eq 0 ] \
  && ok "terminator split across writes + 8-byte reads still served correctly" \
  || no "dribble leg (server rc=$rc client rc=$drc)"

echo "=== 13t exit codes ==="
"$B" >/dev/null 2>&1;        [ $? -eq 2 ] && ok "no args -> 2 (usage)"      || no "no-args rc"
"$B" nope >/dev/null 2>&1;   [ $? -eq 2 ] && ok "non-numeric port -> 2"     || no "bad-port rc"
"$B" 0 >/dev/null 2>&1;      [ $? -eq 2 ] && ok "port 0 -> 2 (rejected)"    || no "port-0 rc"

echo
echo "TCPSERVE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
