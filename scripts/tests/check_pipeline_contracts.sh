#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Pipeline stage-contract gate (ROADMAP Phase 6.5 #3).
#
# The pipeline is a sequence of stages, each of which PROMISES something to the
# next: AST → Check → Elab/Core → Mono → CoreCheck → Lower → SSA → SSAVerify →
# Emit → link. The world-class property this gate locks:
#
#   every user-triggerable contract violation becomes a DIAGNOSTIC at the FIRST
#   responsible boundary — never leaking to Lower, LLVM, the linker, or a panic.
#
# For each boundary contract, a fixture that violates it must (a) be rejected
# with a clean `error[<pass>]` diagnostic, (b) be reported by the EXPECTED pass
# (the first one responsible), and (c) emit NO internal-layer leak marker
# (llvm-as / ld / clang / PANIC). Together these assert the contract holds and
# is enforced at the right place — the difference between "strong individual
# gates" and a pipeline that can say which earlier contract makes a case
# impossible.
#
# See docs/PIPELINE_CONTRACTS.md for the per-boundary promises. Needs the
# compiler built; runs in the compiler test job.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

LEAK='llvm-as|LLVM IR validation|Undefined symbols|^ld:|clang: error|llc:|PANIC|panic|Segmentation'

# contract <label> <expected-pass-tag> <program>
# The violating program must be rejected with a clean `error[<pass>]` from the
# expected pass, and NOT leak a lower-layer error.
contract(){
  local label="$1" passtag="$2" prog="$3"
  printf '%s' "$prog" > "$TMP/$label.con"
  local out rc
  out="$("$C" "$TMP/$label.con" -o "$TMP/$label.bin" 2>&1)" && rc=0 || rc=$?
  if [ "$rc" -eq 0 ]; then no "$label: contract not enforced — it compiled"; return; fi
  if printf '%s' "$out" | grep -qE "$LEAK"; then
    no "$label: LEAKED past the boundary: $(printf '%s' "$out" | grep -oE "$LEAK" | head -1)"; return; fi
  if ! printf '%s' "$out" | grep -q "error\[${passtag}\]"; then
    no "$label: not caught at the '${passtag}' boundary: $(printf '%s' "$out" | grep -oE 'error\[[a-z-]+\]' | head -1)"; return; fi
  ok "$label (caught at ${passtag})"
}

echo "=== after Check: no unconsumed owned linear locals (contract → error[check]) ==="
contract check_linear_local check 'mod m { struct R { x: i32 } fn f() { let r: R = R { x: 1 }; } fn main() -> Int { return 0; } }'

echo "=== front-end type agreement: mixed-width binop rejected before Lower (→ error[check]) ==="
contract mixed_width check 'mod m { fn main() -> Int { let a: i32 = 1; let b: i64 = 2; return (a + b) as Int; } }'

echo "=== after CoreCheck: capability discipline (→ error[core-check]) ==="
contract cap_undeclared core-check 'mod m { fn needs() with(Network) -> Int { return 0; } fn main() -> Int { return needs(); } }'

echo "=== after CoreCheck: Copy constraints — a Copy struct with a non-Copy field (→ error[core-check]) ==="
contract copy_field core-check 'mod m { struct S { x: i32 } struct Copy T { s: S } fn main() -> Int { return 0; } }'

echo "=== after CoreCheck: no infinite-size Core — recursive struct rejected, not leaked to llvm-as (→ error[core-check]) ==="
contract recursive_type core-check 'mod m { struct S { x: S } fn main() -> Int { return 0; } }'

echo "=== emit/link boundary: an executable needs an entry point (→ error[link], not an ld leak) ==="
contract no_main link 'mod m { fn helper() -> Int { return 1; } }'

echo ""
echo "PIPELINE-CONTRACTS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
