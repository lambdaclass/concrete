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
if grep <<<"$TWICE_IR" -q "alloca"; then
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

echo "=== capability-polymorphic METHODS infer cap + type vars without turbofish ==="
# Prerequisite for scoped collection callbacks (with_value): a `cap C` method
# must infer C (and its own type params) from the callback argument.
run method_cap_infer 'mod m { pub struct Copy H { v: i64 }
  impl H { pub fn run<cap C>(&self, f: fn(i64) with(C) -> i64) with(C) -> i64 { return f(self.v); } } }
import m.{H};
fn pure_inc(x: i64) -> i64 { return x + 1; }
fn main() -> i64 { let h: H = H { v: 41 }; return h.run(pure_inc); }' 42

run method_ret_infer 'mod m { pub struct Copy H { v: i64 }
  impl H { pub fn run<R>(&self, f: fn(i64) -> R) -> R { return f(self.v); } } }
import m.{H};
fn to_i(x: i64) -> i64 { return x + 2; }
fn main() -> i64 { let h: H = H { v: 40 }; return h.run(to_i); }' 42

# A cap-polymorphic method must require the callback's caps at the call site:
# a caller lacking them is rejected with the capability error (E0240), NOT a
# spurious type mismatch.
printf '%s' 'mod m { pub struct Copy H { v: i64 }
  impl H { pub fn run<cap C>(&self, f: fn(i64) with(C) -> i64) with(C) -> i64 { return f(self.v); } } }
import m.{H};
fn nf(x: i64) with(File) -> i64 { return x; }
fn main() -> i64 { let h: H = H { v: 1 }; return h.run(nf); }' > "$TMP/method_cap_no.con"
ERR="$("$COMPILER" "$TMP/method_cap_no.con" -o "$TMP/method_cap_no" 2>&1)"
if "$COMPILER" "$TMP/method_cap_no.con" -o "$TMP/method_cap_no" >/dev/null 2>&1; then
  no "cap-poly method invoked without the required capability was NOT rejected"
elif grep <<<"$ERR" -q "E0240"; then
  ok "cap-poly method without required cap rejected with E0240 (capability, not type mismatch)"
else
  no "cap-poly method rejected, but not with E0240: $(grep <<<"$ERR" -i error | head -1)"
fi

echo "=== references are second-class: no function/callback may RETURN a reference ==="
# This is what makes scoped callbacks sound WITHOUT lifetimes/provenance: the
# callback cannot return the borrowed element, so the borrow cannot escape.
# (1) a function-pointer TYPE returning a ref is rejected.
printf '%s' 'fn apply(x: &i64, f: fn(&i64) -> &i64) -> i64 { return *f(x); }
fn id(x: &i64) -> &i64 { return x; }
fn main() -> i64 { let a: i64 = 5; return apply(&a, id); }' > "$TMP/fnret.con"
if "$COMPILER" "$TMP/fnret.con" -o "$TMP/fnret" >/dev/null 2>&1; then
  no "fn(&T) -> &T type was NOT rejected (ref-returning callback constructable)"
else
  ok "fn(&T) -> &T function-pointer type is rejected"
fi
# (2) the with_value backdoor: a callback that returns its borrowed &V (R=&V).
printf '%s' 'mod m { pub struct Copy H { v: i64 }
  trusted impl H { pub fn with_value<R, cap C>(&self, f: fn(&i64) with(C) -> R) with(C) -> R { return f(&self.v); } } }
import m.{H};
fn ret_ref(x: &i64) -> &i64 { return x; }
fn main() -> i64 { let h: H = H { v: 42 }; return *h.with_value(ret_ref); }' > "$TMP/wvbackdoor.con"
if "$COMPILER" "$TMP/wvbackdoor.con" -o "$TMP/wvbackdoor" >/dev/null 2>&1; then
  no "with_value with an R=&V callback was NOT rejected (H1 escape reintroduced)"
else
  ok "with_value with a ref-returning callback (R=&V) is rejected"
fi
# (3) the generic backdoor: a type param instantiated to a ref in return position.
printf '%s' 'fn wrap<R>(r: R) -> Option<R> { return Option::<R>::Some { value: r }; }
fn make() -> Option<&i64> { let x: i64 = 42; return wrap::<&i64>(&x); }
fn main() -> i64 { match make() { Option::Some { value } => { return *value; } Option::None => { return 0; } } }' > "$TMP/genbackdoor.con"
if "$COMPILER" "$TMP/genbackdoor.con" -o "$TMP/genbackdoor" >/dev/null 2>&1; then
  no "generic R instantiated to a reference in return position was NOT rejected (Option<&T> backdoor)"
else
  ok "generic R=&T in return position is rejected (no Option<&T> backdoor)"
fi
# (4) a value-returning callback (R = a value) still works — the sound case.
run wv_value_ok 'mod m { pub struct Copy H { v: i64 }
  trusted impl H { pub fn with_value<R, cap C>(&self, f: fn(&i64) with(C) -> R) with(C) -> R { return f(&self.v); } } }
import m.{H};
fn rd(x: &i64) -> i64 { return *x; }
fn main() -> i64 { let h: H = H { v: 42 }; return h.with_value(rd); }' 42

echo ""
echo "NOTE: immutable HashMap::with_value behavior is gated by the map stdlib"
echo "      #[test]s (run via --stdlib-module map): test_map_with_value,"
echo "      test_map_with_value_missing. with_value_mut/modify are DEFERRED"
echo "      (separate container-not-in-context obligation)."
echo ""
echo "CALLABLE-VALUES: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
