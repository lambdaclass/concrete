#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# `concrete test` gate (ROADMAP Phase 5 #3).
#
# Locks the project-mode test runner: `concrete test` discovers and runs #[test]
# functions across a multi-module project, reports PASS/FAIL per test, exits
# nonzero when any test fails, and `--module` scopes to one module. This also
# regression-locks the dup-`@__concrete_argc` miscompile: the argc/argv stubs were
# emitted per module, so any multi-module test build (a project pulls in std)
# produced a duplicate global and llvm-as rejected it — now emitted once in the
# test runner.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/p/src"
printf '[package]\nname = "p"\nversion = "0.1.0"\n' > "$TMP/p/Concrete.toml"
# Multi-module project (lib + main) so the test build spans >1 module — the shape
# that triggered the dup-global miscompile.
cat > "$TMP/p/src/main.con" <<'EOF'
mod lib {
    pub fn add(a: i64, b: i64) -> i64 { return a + b; }
}
mod p {
    import lib.{ add };
    fn main() -> i64 { return 0; }
    #[test]
    fn t_ok() -> i32 { if add(2, 3) == 5 { return 0; } return 1; }
    #[test]
    fn t_bad() -> i32 { return 9; }
}
EOF

echo "=== 1. project 'concrete test' builds + runs (no dup-global miscompile) ==="
out="$(cd "$TMP/p" && "$COMPILER" test 2>&1)"; rc=$?
if printf '%s' "$out" | grep -qiE "redefinition|LLVM IR validation failed"; then
  no "project test build miscompiled (dup global regressed)"; printf '%s\n' "$out" | head -3 | sed 's/^/      /'
else
  ok "project test build is valid LLVM (no dup global)"
fi

echo "=== 2. a passing #[test] reports PASS ==="
printf '%s' "$out" | grep -qE "PASS: t_ok" \
  && ok "passing test reported PASS" || no "passing test not reported"

echo "=== 3. a failing #[test] reports FAIL and the run exits nonzero ==="
if printf '%s' "$out" | grep -qE "FAIL: t_bad" && [ "$rc" -ne 0 ]; then
  ok "failing test reported FAIL; run exited nonzero ($rc)"
else
  no "failing test not reported / exit code wrong (rc=$rc)"
fi

echo "=== 4. --module scopes to one module's tests ==="
out2="$(cd "$TMP/p" && "$COMPILER" test --module p 2>&1)"
if printf '%s' "$out2" | grep -qE "PASS: t_ok" && printf '%s' "$out2" | grep -qE "FAIL: t_bad"; then
  ok "--module p runs that module's tests"
else
  no "--module filter did not run the expected tests"
fi

echo ""
echo "CONCRETE-TEST: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
