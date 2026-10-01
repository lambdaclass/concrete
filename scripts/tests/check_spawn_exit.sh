#!/usr/bin/env bash
# BUG 072: A CHILD THAT FAILS TO EXEC MUST NOT FLUSH THE PARENT'S STDIO BUFFERS.
#
# `std.process.spawn` forks, and the child calls `execvp`. If exec fails, the child
# used to call `exit(127)`. `exit` runs atexit handlers and flushes stdio — and the
# child's stdio buffers are COPIES of the parent's, so any output the parent had
# buffered but not yet flushed was written a second time. Measured before the fix: one
# byte written through a buffered `fs.File`, then a failed spawn, left "XX" in the file.
# The child now calls `_exit(127)`, which terminates without flushing.
#
# The fixture returns 0 only if the child really exited 127 — the exec-failure path this
# gate is about — so a spawn that took some other path cannot make it pass vacuously.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
FIX="$ROOT_DIR/tests/regressions/spawn_exit/exec_failure_no_double_flush"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
if command -v timeout >/dev/null 2>&1; then TO="timeout 300"; else TO=""; echo "  warn 'timeout' not found — running without a hang watchdog"; fi

TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

echo "=== the fixture builds ==="
if (cd "$FIX" && $TO "$CC" build . -o "$TMP/spawn_exit" >"$TMP/build.log" 2>&1); then
  ok "exec_failure_no_double_flush builds"
else
  no "exec_failure_no_double_flush does not build"; sed 's/^/       /' "$TMP/build.log" | head -10
  echo; echo "SPAWN-EXIT: PASS=$PASS FAIL=$FAIL"; exit 1
fi

echo "=== a failed exec leaves the parent's buffered output written once ==="
(cd "$TMP" && $TO ./spawn_exit); rc=$?
if [ "$rc" -eq 0 ]; then
  ok "the child took the exec-failure path (exited 127)"
else
  no "fixture returned $rc (5/6: child did not exit 127; 2: create failed; 3: wait failed; 4: spawn failed)"
fi
content="$(cat "$TMP/spawn_exit_out.txt" 2>/dev/null)"
if [ "$content" = "X" ]; then
  ok "the buffered byte reached the file exactly once"
else
  no "file holds '$content', expected 'X' — the child flushed the parent's stdio buffer (bug 072)"
fi

echo
echo "SPAWN-EXIT: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
