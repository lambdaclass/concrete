#!/usr/bin/env bash
# Returned-reference provenance gate (H1) — CLOSED (2026-06-13).
#
# H1 is resolved by the language invariant "references are second-class — never
# returned" (docs/VALUE_MODEL.md): no safe-callable function or function TYPE may
# return a reference, directly, nested in an aggregate, or via generic
# instantiation. This is enforced by the checker. The accessor surface was
# migrated to the value model (get -> Option<V> for Copy; with_value/with_at to
# borrow; remove/pop to move out; raw pointers for low-level access).
#
# This gate asserts the closure holds: returning a reference is rejected, and the
# old unsound patterns no longer compile.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

reject() {  # name, source, expected-substring
  printf '%s' "$2" > "$TMP/$1.con"
  local err; err="$("$COMPILER" "$TMP/$1.con" -o "$TMP/$1" 2>&1)"
  if "$COMPILER" "$TMP/$1.con" -o "$TMP/$1" >/dev/null 2>&1; then
    no "$1: expected REJECT but it compiled"
  elif grep <<<"$err" -q "$3"; then
    ok "$1 rejected ($3)"
  else
    no "$1 rejected but not for the expected reason: $(grep <<<"$err" -i error | head -1)"
  fi
}

echo "=== returning a reference is rejected (the H1 closure) ==="
reject bare_ref_return \
'fn f(x: &i64) -> &i64 { return x; }
fn main() -> i64 { return 0; }' \
'may not return a reference'

reject ref_to_local \
'fn f() -> &i64 { let local: i64 = 42; return &local; }
fn main() -> i64 { return 0; }' \
'may not return a reference'

reject aggregate_ref_return \
'fn f(x: &i64) -> Option<&i64> { return Option::<&i64>::Some { value: x }; }
fn main() -> i64 { return 0; }' \
'may not return a reference'

reject ref_returning_fn_type \
'fn apply(x: &i64, f: fn(&i64) -> &i64) -> i64 { return *f(x); }
fn main() -> i64 { return 0; }' \
'function type may not return a reference'

reject generic_ref_in_return \
'fn wrap<R>(r: R) -> R { return r; }
fn main() -> i64 { let x: i64 = 42; let r: &i64 = wrap::<&i64>(&x); return *r; }' \
'instantiated to a reference'

echo "=== the registered negative fixture is rejected ==="
if "$COMPILER" tests/programs/error_escape_return.con -o "$TMP/esc" >/dev/null 2>&1; then
  no "error_escape_return.con compiled (should be rejected)"
else
  ok "error_escape_return.con rejected (return of a borrow-block ref)"
fi

echo "=== mutable half stays gone: no public Option<&mut> in std ==="
if grep -rn "pub fn.*-> *Option<&mut" std/src/*.con >/dev/null 2>&1; then
  no "a public API returns Option<&mut ...> — must not reappear"
else
  ok "no public Option<&mut ...> accessor in std"
fi

echo "=== no public reference-returning accessor remains in std ==="
if grep -rnE "pub fn [a-z_]+.*\) *(with\([^)]*\) *)?-> *(&|Option<&|Result<&)" std/src/*.con >/dev/null 2>&1; then
  no "a public std fn still returns a reference (directly or aggregate-wrapped):"
  grep -rnE "pub fn [a-z_]+.*\) *(with\([^)]*\) *)?-> *(&|Option<&|Result<&)" std/src/*.con | sed 's/^/       /'
else
  ok "no public reference-returning accessor in std (all migrated to the value model)"
fi

echo "=== the obsolete hole-repro examples no longer compile (hole closed) ==="
for h in returned_ref_provenance_map returned_ref_provenance_vec; do
  d="examples/known_holes/$h"
  [ -d "$d" ] || { ok "$h removed"; continue; }
  if (cd "$d" && "$COMPILER" build >/dev/null 2>&1); then
    no "$h still builds — its unsound returned-ref read should now be rejected"
  else
    ok "$h no longer builds (its returned-ref read is now rejected — hole closed)"
  fi
  rm -f "$d/main" "$d/$h"
done

echo ""
echo "RETURNED-REF-PROVENANCE (H1 CLOSED): PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
