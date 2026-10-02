#!/usr/bin/env bash
# R-0484 R10: A PROGRAM'S REPORT NAMES THE FOREIGN-BINDING ASSUMPTIONS IT INHERITS.
#
# A foreign binding's effect declaration (`extern fn write(..) with(Console)`) is enforced
# on every caller but is itself an ASSUMPTION about C code. Most of a program's foreign
# assumptions live in its dependencies — std's libc bindings above all — so a report that
# listed only the program's own bindings would read "no foreign assumptions" for almost
# every program. `--report unsafe` in project mode lists each dependency binding the
# program can reach (direct calls, or a function stored as a value), with its declared
# effects, trust kind and the program functions that reach it.
#
# Asserted: the inherited listing for base64_cli (std's `write`, with(Console), reached by
# `usage`); resolution in the calling module first (two std modules each bind `realloc`;
# only the one actually reached is listed); and the coverage note in both modes — project
# mode says dependencies were analysed, single-file mode says they were not.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
if command -v timeout >/dev/null 2>&1; then TO="timeout 300"; else TO=""; echo "  warn 'timeout' not found — running without a hang watchdog"; fi

echo "=== project mode: inherited assumptions are listed ==="
out="$(cd "$ROOT_DIR/examples/base64_cli" && $TO "$CC" src/main.con --report unsafe 2>&1)"
if printf '%s' "$out" | grep -q 'Inherited foreign assumptions (from dependencies):'; then
  ok "base64_cli's report has an inherited-assumptions section"
else
  no "no inherited-assumptions section — dependency bindings are not reaching the report"
fi
if printf '%s' "$out" | grep -A1 'std.libc.write: assumed to perform only with(Console)' | grep -q 'reached by:.*base64_cli.usage'; then
  ok "std.libc.write is listed with(Console), reached by base64_cli.usage"
else
  no "std.libc.write with(Console) reached by usage is not listed"
fi
if printf '%s' "$out" | grep -q 'std.libc.realloc'; then
  no "std.libc.realloc is listed — a same-named binding in another module was attributed (resolution must prefer the calling module)"
else
  ok "only the realloc binding actually reached is listed (calling-module resolution)"
fi
if printf '%s' "$out" | grep -q 'Dependency coverage: dependencies analysed'; then
  ok "project mode states that dependencies were analysed"
else
  no "project mode does not state its dependency coverage"
fi

echo "=== single-file mode says it did not see dependencies ==="
sout="$($TO "$CC" "$ROOT_DIR/tests/programs/trusted_absorbs_extern_unsafe.con" --report unsafe 2>&1)"
if printf '%s' "$sout" | grep -q 'Dependency coverage: incomplete'; then
  ok "single-file mode reports incomplete dependency coverage, so an empty list cannot read as none"
else
  no "single-file mode does not state that dependency coverage is incomplete"
fi

echo
echo "FOREIGN-ASSUMPTIONS-REPORT: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
