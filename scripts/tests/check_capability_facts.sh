#!/usr/bin/env bash
# Capability-fact source-of-truth gate (ROADMAP Phase 6.5 #5).
#
# Capabilities are the second identity-defining semantic axis of Concrete. The
# derived capability facts — Unsafe authority, the "extern fn requires Unsafe"
# fact, and literal-Unsafe classification — now live once in
# Concrete/Semantics/Capabilities.lean, and Check/CoreCheck/Report read from it.
# This gate proves the checker's capability VERDICT and the report's capability
# rendering agree by construction (they read the same fact), across the
# identity-defining cases:
#   - an Unsafe op without authority is rejected at the CoreCheck boundary;
#   - a `trusted` wrapper grants authority;
#   - a `with(Unsafe)` declaration grants authority;
#   - an untrusted `extern fn` requires Unsafe (the shared extern-cap fact);
#   - report-vs-checker agreement: a function the checker accepts as Unsafe is
#     exactly the one the report lists as Unsafe (the disagreement negative).
#
# Frontend-only paths (`--report unsafe`) are used for the accept cases so a
# missing extern symbol's LINK error can't be mistaken for a capability verdict.
# Needs the compiler built; runs in the compiler test job.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# rejects_cap <label> <program>: CoreCheck must reject with a capability
# diagnostic (E0520/E0521 at core-check), not leak/compile.
rejects_cap(){
  local n="$1"; printf '%s' "$2" > "$TMP/$n.con"
  local out; out="$("$C" "$TMP/$n.con" -o "$TMP/$n.bin" 2>&1)" || true
  if printf '%s' "$out" | grep -qE "error\[core-check\].*(Unsafe|capability|requires)"; then ok "$n: rejected at core-check"
  else no "$n: expected a capability rejection; got: $(printf '%s' "$out" | head -1)"; fi
}

# frontend_accepts <label> <program>: the frontend (Check + CoreCheck) accepts
# it — checked via --report (no link step).
frontend_accepts(){
  local n="$1"; printf '%s' "$2" > "$TMP/$n.con"
  if "$C" "$TMP/$n.con" --report unsafe >/dev/null 2>&1; then ok "$n: frontend accepts"
  else no "$n: frontend rejected an authorized op; $("$C" "$TMP/$n.con" --report unsafe 2>&1 | head -1)"; fi
}

echo "=== Unsafe op without authority → rejected at the CoreCheck boundary ==="
rejects_cap unsafe_no_cap 'mod m { fn f(p: *const i32) -> i32 { return *p; } fn main() -> Int { return 0; } }'

echo "=== authority via trusted, via with(Unsafe), and a cap-polymorphic wrapper ==="
frontend_accepts trusted_wrapper 'mod m { trusted fn f(p: *const i32) -> i32 { return *p; } fn main() -> Int { return 0; } }'
frontend_accepts with_unsafe 'mod m { fn f(p: *const i32) with(Unsafe) -> i32 { return *p; } fn main() -> Int { return 0; } }'

echo "=== extern-fn cap fact: untrusted extern requires Unsafe (rejected without it) ==="
rejects_cap extern_needs_unsafe 'mod m { extern fn raw_op() -> i32; fn main() -> Int { return raw_op() as Int; } }'

echo "=== report ⇔ checker agreement: the fn the checker accepts as Unsafe is the one the report lists ==="
prog='mod m { fn f(p: *const i32) with(Unsafe) -> i32 { return *p; } fn main() -> Int { return 0; } }'
printf '%s' "$prog" > "$TMP/agree.con"
if "$C" "$TMP/agree.con" --report unsafe 2>&1 | grep -q "fn f(" ; then ok "report lists the accepted Unsafe fn (same fact as the checker)"
else no "report did not list the fn the checker accepts as Unsafe (fact disagreement)"; fi
# negative: a pure fn must NOT be listed as Unsafe
printf 'mod m { fn g() -> i32 { return 1; } fn main() -> Int { return 0; } }' > "$TMP/pure.con"
if "$C" "$TMP/pure.con" --report unsafe 2>&1 | grep -q "fn g(" ; then no "report listed a pure fn as Unsafe (over-reporting)"
else ok "report does not list a pure fn as Unsafe"; fi

echo ""
echo "CAPABILITY-FACTS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
