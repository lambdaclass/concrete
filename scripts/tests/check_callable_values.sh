#!/usr/bin/env bash
# Callable-values gate (ROADMAP Phase 5 #24 — docs/CALLABLE_VALUES_AND_CAPABILITIES.md §9).
#
# STEP 1 (bound-callback context threading) is implemented and checked here:
#   - the three context modes (shared &Ctx / mutable &mut Ctx / consuming Ctx)
#     compile and thread context correctly;
#   - the REBORROW (`&mut *ctx`) aliases the original storage — no copy — so a
#     mutable context threaded across repeated callback calls accumulates into
#     the caller's value. Before the fix, `&mut *ctx` copied the pointee into a
#     fresh alloca and the callback mutated the throwaway (a silent miscompile:
#     wrong result, and an infinite-style loss of writes).
#
# Steps 2+ (scoped collection callbacks `with_value`/`with_value_mut`, the
# container-not-in-context invariant, and the proof/evidence shape) add their
# checks here when they land; see the doc's §9 and §11 build order.
#
# Permanent invariant (also locked by check_capability_polymorphism_design.sh):
# a callback's declared capabilities are required at the call site and never
# erased — exercised here via a cap-polymorphic combinator.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

run() {
  local name="$1" src="$2" exp="$3"
  printf '%s' "$src" > "$TMP/$name.con"
  if "$COMPILER" "$TMP/$name.con" -o "$TMP/$name" >/dev/null 2>&1; then
    local out rc val; out="$(timeout 10 "$TMP/$name" 2>/dev/null)"; rc=$?
    if [ "$rc" = "124" ]; then no "$name: TIMED OUT (hang/miscompile)"; return; fi
    val="$out"; [ -z "$val" ] && val="$rc"
    [ "$val" = "$exp" ] && ok "$name = $exp" || no "$name: got '$val' expect $exp"
  else
    no "$name failed to compile: $("$COMPILER" "$TMP/$name.con" 2>&1 | grep -i error | head -1)"
  fi
}

echo "=== three context modes thread correctly (shared / mutable / consuming) ==="
if "$COMPILER" tests/programs/callback_context_modes.con -o "$TMP/modes" >/dev/null 2>&1; then
  V="$(timeout 10 "$TMP/modes" 2>/dev/null)"; RC=$?
  if [ "$RC" = "124" ]; then no "callback_context_modes TIMED OUT"; else
    [ "$V" = "245" ] && ok "callback_context_modes = 245 (203 shared + 30 mutable + 12 consuming)" \
      || no "callback_context_modes: got '$V' expect 245"
  fi
else no "callback_context_modes failed to compile"; fi

echo "=== reborrow &mut *ctx aliases original storage across repeated calls ==="
# The regression: a mutable context threaded through TWO callback calls must
# accumulate into the caller's value. A copy-to-temp reborrow yields 0.
run reborrow_twice 'struct Copy Sum { total: i64 }
fn addto(acc: &mut Sum, x: &i64) { acc.total = acc.total + *x; }
fn twice(ctx: &mut Sum, a: &i64, b: &i64, f: fn(&mut Sum, &i64)) { f(&mut *ctx, a); f(&mut *ctx, b); }
fn main() -> i64 { let mut s: Sum = Sum { total: 0 }; let x: i64 = 10; let y: i64 = 20;
  twice(&mut s, &x, &y, addto); return s.total; }' 30

echo "=== reborrow lowers to the pointer itself — NO temp copy in IR ==="
cat > "$TMP/rb.con" <<'EOF'
struct Copy Sum { total: i64 }
fn addto(acc: &mut Sum, x: &i64) { acc.total = acc.total + *x; }
fn twice(ctx: &mut Sum, a: &i64, b: &i64, f: fn(&mut Sum, &i64)) { f(&mut *ctx, a); f(&mut *ctx, b); }
fn main() -> i64 { let mut s: Sum = Sum { total: 0 }; let x: i64 = 1; let y: i64 = 2; twice(&mut s, &x, &y, addto); return s.total; }
EOF
TWICE_IR="$("$COMPILER" "$TMP/rb.con" --emit-llvm 2>/dev/null | awk '/define.*@twice/,/^}/')"
if echo "$TWICE_IR" | grep -q "alloca"; then
  no "reborrow still allocates a temp in twice() — the copy-to-temp miscompile reappeared:"
  echo "$TWICE_IR" | sed 's/^/       /'
else
  ok "twice() reborrow has no alloca (passes the ctx pointer directly)"
fi

echo "=== capability-polymorphic callback: caps required at the call site ==="
# A combinator over a with(File) callback must itself carry the capability;
# a caller lacking it is rejected (no silent erasure).
run cap_ok 'fn apply<T, cap C>(f: fn(T) with(C) -> T, x: T) with(C) -> T { return f(x); }
fn fop(x: i64) with(File) -> i64 { return x + 1; }
fn main() with(File) -> i64 { return apply(fop, 41); }' 42
printf '%s' 'fn apply<T, cap C>(f: fn(T) with(C) -> T, x: T) with(C) -> T { return f(x); }
fn fop(x: i64) with(File) -> i64 { return x; }
fn main() -> i64 { return apply(fop, 42); }' > "$TMP/cap_no.con"
if "$COMPILER" "$TMP/cap_no.con" -o "$TMP/cap_no" >/dev/null 2>&1; then
  no "pure caller invoking a with(File) callback was NOT rejected (capability erased)"
else
  ok "pure caller invoking a with(File) callback is rejected (caps not erased)"
fi

echo ""
echo "CALLABLE-VALUES: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
