#!/usr/bin/env bash
# std.cli v1 gate (design: research/stdlib/cli.md): examples/cli_tool declares
# both flag kinds + positional arity and is driven through good/malformed/
# unknown/missing/arity inputs with output + 13t exit codes asserted
# (usage class = 2). ALSO the load-bearing regression anchor for compiler
# bug 034 (short-circuit && RHS borrow promotion): under the pre-fix
# compiler, 'cli_tool a b' aborts (rc 134) — verified 2026-07-16.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

( cd examples/cli_tool && "$C" build ) >/dev/null 2>&1 \
  && ok "cli_tool builds" || { no "cli_tool build failed"; echo "CLI-HELPERS: PASS=$PASS FAIL=$FAIL"; exit 1; }
B="examples/cli_tool/cli_tool"

run(){ # run <expected_rc> <args...>; stdout captured in $OUT
  local want="$1"; shift
  OUT=$("$B" "$@" 2>/dev/null); RC=$?
  [ "$RC" -eq "$want" ]
}

# happy paths
run 0 -n 16 -v file.bin && grep -q "n=16 s=0 sseen=0 v=1 npos=1" <<<"$OUT" && grep -q "pos=file.bin" <<<"$OUT" \
  && ok "value+presence flags + positional" || no "value+presence flags: rc=$RC out=$OUT"
run 0 a b && grep -q "npos=2" <<<"$OUT" \
  && ok "two positionals (bug-034 regression shape)" || no "two positionals: rc=$RC out=$OUT"
run 0 -s 5 one && grep -q "s=5 sseen=1" <<<"$OUT" \
  && ok "seen() distinguishes given-vs-default" || no "seen(): rc=$RC out=$OUT"
run 0 one && grep -q "n=7" <<<"$OUT" \
  && ok "defaults apply when flag absent" || no "defaults: rc=$RC out=$OUT"
run 0 -n 1 -n 2 x && grep -q "n=2" <<<"$OUT" \
  && ok "repeated value flag: last wins" || no "repeated flag: rc=$RC out=$OUT"
run 0 - && grep -q "pos=-" <<<"$OUT" \
  && ok "bare dash stays a positional" || no "bare dash: rc=$RC out=$OUT"

# 13t error bucket: every parse failure = exit 2
run 2 && ok "no positionals -> 2 (WrongArity)" || no "no-args rc=$RC"
run 2 a b c && ok "too many positionals -> 2 (WrongArity)" || no "too-many rc=$RC"
run 2 -x foo && ok "unknown flag -> 2" || no "unknown flag rc=$RC"
run 2 -n zzz x && ok "malformed value -> 2" || no "malformed value rc=$RC"
run 2 -n && ok "missing value -> 2" || no "missing value rc=$RC"

# proof-of-pull: the consumers actually deleted their hand loops
grep -q "import std.cli" examples/hexdump/src/main.con \
  && ok "hexdump imports std.cli (hand loop deleted)" || no "hexdump not switched over"
grep -q "import std.cli" examples/base64_cli/src/main.con \
  && ok "base64_cli imports std.cli (hand loop deleted)" || no "base64_cli not switched over"

echo
echo "CLI-HELPERS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
