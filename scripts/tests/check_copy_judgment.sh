#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Copy-judgment gate (Phase 6.5 CopyJudgment axis).
#
# "Is this type Copy?" (freely duplicated) vs linear (use-once) decides linearity
# everywhere. It used to be implemented TWICE — a monadic front-end version over
# `StructDef`/`EnumDef`/`NewtypeDef` (`CheckHelpers.isCopyType`) and a pure Core
# version over `CStructDef`/`CEnumDef` (`Layout.isCopyTyCore`) — the two drifting
# on `typeVar` (bounds vs a fixed flag) and `newtype` (recurse vs absent), kept in
# agreement only by ordering luck. They now share ONE recursion
# (`Layout.isCopyTyGeneric`) parameterized by lookups.
#
# This gate exercises both sides of that single judgment: the front-end Copy
# decision (Copy struct, newtype-over-Copy, non-Copy linear enforcement) and the
# Core-side field-recursion (a Copy struct may not contain a non-Copy field). A
# regression that breaks a branch of the shared judgment fails a case here.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
emit(){ printf '%s\n' "$2" > "$TMPDIR/$1.con"; }

# value <label> <name> <expected>: interp accepts and prints <expected>.
value(){ local label="$1" F="$TMPDIR/$2.con" want="$3"
  local OUT; OUT="$("$COMPILER" "$F" --interp 2>&1)"
  if [ "$OUT" = "$want" ]; then ok "$label"; else no "$label (want $want, got $(printf '%s' "$OUT"|head -1))"; fi; }

# rejects <label> <name> <code>: check rejects with the given diagnostic code.
rejects(){ local label="$1" F="$TMPDIR/$2.con" code="$3"
  local OUT; OUT="$("$COMPILER" "$F" --interp 2>&1)"
  if [ $? -ne 0 ] && printf '%s' "$OUT" | grep -q "$code"; then ok "$label"
  else no "$label (want $code, got $(printf '%s' "$OUT"|head -1))"; fi; }

echo "=== Copy types may be duplicated; linear types may not ==="

# A Copy struct value can be read into two bindings (duplicated) — front-end
# Copy decision on a named Copy struct.
emit copyst 'mod m {
    struct Copy Point { x: i32, y: i32 }
    fn main() -> Int {
        let p: Point = Point { x: 3, y: 4 };
        let q: Point = p;
        let r: Point = p;
        return (q.x + r.y) as Int;
    }
}'
value "Copy struct duplicated" copyst "7"

# A newtype over a Copy type is Copy — exercises the newtype recursion, the path
# the two old implementations disagreed on (front-end recursed; Core returned
# not-found -> false).
emit nt 'mod m {
    newtype Meters = i32;
    fn main() -> Int {
        let a: Meters = Meters(5);
        let b: Meters = a;
        let c: Meters = a;
        return (b.0 + c.0) as Int;
    }
}'
value "newtype over Copy is Copy (used twice)" nt "10"

# A non-Copy struct is linear: a second read after the move is E0205.
emit linst 'mod m {
    struct Point { x: i32, y: i32 }
    fn main() -> Int {
        let p: Point = Point { x: 1, y: 2 };
        let q: Point = p;
        let r: Point = p;
        return 0;
    }
}'
rejects "non-Copy struct is linear (use-after-move)" linst "E0205"

echo "=== a Copy aggregate may not contain a non-Copy field (Core field recursion) ==="

# The Core-side of the judgment: a struct declared Copy whose field is linear is
# rejected — the field-Copy recursion must see through to the field type.
emit badcopy 'mod m {
    struct Linear { v: i32 }
    struct Copy Bad { inner: Linear }
    fn main() -> Int { return 0; }
}'
rejects "Copy struct with non-Copy field rejected" badcopy "non-copy field"

echo "=== consistency under refinement: a conditional-Copy generic refines to the ==="
echo "=== concrete Copy-ness of its instantiation (roadmap CopyJudgment gate) ==="

# `struct Copy Wrap<T>` is Copy MODULO {T : Copy}. Instantiated with a Copy type
# it IS Copy (freely duplicated); instantiated with a non-Copy type it refines to
# LINEAR (a second use after move is E0205) — not an error, just not Copy. The
# same judgment (Layout.isCopyTyGeneric) drives Check pre-mono and Verify
# post-mono, so the conditional decision and its concrete refinement agree.
emit wcopy 'mod m {
    struct Copy Wrap<T> { v: T }
    fn main() -> Int {
        let w: Wrap<i32> = Wrap::<i32> { v: 5 };
        let a: Wrap<i32> = w;
        let b: Wrap<i32> = w;
        return (a.v + b.v) as Int;
    }
}'
value "Copy Wrap<i32> refines to Copy (duplicated)" wcopy "10"

emit wlin 'mod m {
    struct Copy Wrap<T> { v: T }
    fn main() -> Int {
        let w: Wrap<String> = Wrap::<String> { v: "x" };
        let a: Wrap<String> = w;
        let b: Wrap<String> = w;
        return 0;
    }
}'
rejects "Copy Wrap<String> refines to linear (use-after-move)" wlin "E0205"

# The same refinement on a builtin conditional-Copy enum (Option<T>).
emit ocopy 'mod m {
    fn main() -> Int {
        let o: Option<i32> = Option::<i32>::Some { value: 7 };
        let a: Option<i32> = o;
        let b: Option<i32> = o;
        return 0;
    }
}'
value "Option<i32> refines to Copy (duplicated)" ocopy "0"

emit olin 'mod m {
    fn main() -> Int {
        let o: Option<String> = Option::<String>::Some { value: "x" };
        let a: Option<String> = o;
        let b: Option<String> = o;
        return 0;
    }
}'
rejects "Option<String> refines to linear (use-after-move)" olin "E0205"

echo
echo "check_copy_judgment: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
