#!/usr/bin/env bash
# Capability-judgment gate (Phase 6.5 CapabilityJudgment, slice 1 — direct calls).
#
# "Does the caller's authority cover this call, and if not which caps are missing"
# is now ONE decision (Capabilities.decideCall / Capabilities.missingCaps): Check
# renders the per-capability E0240 from the record's `missing`, CoreCheck renders
# the whole-set E0520 from the record's satisfaction, and reports read the same
# record. This gate exercises that decision so a regression (or a future surface
# recomputing it) is caught.

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

# accepts <label> <name>: compiles (caller's authority covers the call).
accepts(){ local F="$TMPDIR/$2.con"; if "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1; then ok "$1"; else no "$1 (rejected: $("$COMPILER" "$F" -o "$F.bin" 2>&1 | head -1))"; fi; }
# rejects <label> <name>: does not compile (missing authority).
rejects(){ local F="$TMPDIR/$2.con"; if "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1; then no "$1 (compiled — missing cap not caught!)"; else ok "$1"; fi; }
# rejects_code <label> <name> <code>: rejected with a SPECIFIC diagnostic code.
rejects_code(){ local F="$TMPDIR/$2.con" want="$3" out
  out="$("$COMPILER" "$F" -o "$F.bin" 2>&1)"
  if "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1; then no "$1 (compiled — $want not raised!)"
  elif printf '%s' "$out" | grep -q "$want"; then ok "$1"
  else no "$1 (rejected but not $want: $(printf '%s' "$out" | head -1))"; fi; }

echo "=== direct-call capability decision (caller must cover callee) ==="

emit have 'mod m { fn needs() with(File) -> Int { return 0; } fn main() with(File) -> Int { return needs(); } }'
accepts "caller declares the required cap" have

emit havemore 'mod m { fn needs() with(File) -> Int { return 0; } fn main() with(File) -> Int { let x: Int = needs(); return x; } }'
accepts "caller declares required cap (used)" havemore

emit missing 'mod m { fn needs() with(File) -> Int { return 0; } fn main() -> Int { return needs(); } }'
rejects "caller lacks the required cap" missing

emit missingone 'mod m { fn needs() with(File) -> Int { return 0; } fn main() with(Network) -> Int { return needs(); } }'
rejects "caller has a different cap (missing the required one)" missingone

emit pure 'mod m { fn pure_fn() -> Int { return 1; } fn main() -> Int { return pure_fn(); } }'
accepts "pure callee needs no cap" pure

echo "=== calls THROUGH a function pointer must hold the pointer type's caps (anti-smuggling) ==="

# A caller with no authority must NOT invoke a `fn() with(Network)` pointer — that
# would smuggle Network past its own header (Capabilities.missingCapsThroughPtr).
emit smuggle 'mod m { fn caller(f: fn() with(Network) -> Int) -> Int { return f(); } fn main() -> Int { return 0; } }'
rejects "pure caller invokes a fn()with(Network) pointer" smuggle

# Declaring the capability makes the same indirect call legal.
emit legit 'mod m { fn caller(f: fn() with(Network) -> Int) with(Network) -> Int { return f(); } fn main() -> Int { return 0; } }'
accepts "caller with(Network) invokes the pointer" legit

echo "=== capability-polymorphic propagation (callback needs C => caller needs C) ==="

# `apply<cap C>(f: fn(..) with(C) ..) with(C)`: passing a File-requiring callback
# resolves C := File (Capabilities.resolveCaps), so the caller must hold File.
emit propmiss 'mod m {
    fn apply<T, U, cap C>(f: fn(T) with(C) -> U, x: T) with(C) -> U { return f(x); }
    fn io(x: Int) with(File) -> Int { return x; }
    fn main() -> Int { return apply(io, 5); }
}'
rejects "pure main applies a File-requiring callback (C := File)" propmiss

emit prophave 'mod m {
    fn apply<T, U, cap C>(f: fn(T) with(C) -> U, x: T) with(C) -> U { return f(x); }
    fn io(x: Int) with(File) -> Int { return x; }
    fn main() with(File) -> Int { return apply(io, 5); }
}'
accepts "main with(File) applies the File-requiring callback" prophave

echo "=== Unsafe-op authority (trusted OR with(Unsafe); Capabilities.capsAllowUnsafeOp) ==="

# A raw-pointer deref needs authority: rejected in a plain function, allowed inside
# a `trusted` function or one that declares `with(Unsafe)`. One decision behind all
# four of CoreCheck's unsafe-op gates.
emit unsafeplain 'mod m { fn main() -> Int { let x: i32 = 5; let p: *const i32 = &x as *const i32; let v: i32 = *p; return v as Int; } }'
rejects "raw-ptr deref in a plain fn (no authority)" unsafeplain

emit unsafetrusted 'mod m { trusted fn d() -> Int { let x: i32 = 5; let p: *const i32 = &x as *const i32; let v: i32 = *p; return v as Int; } fn main() -> Int { return d(); } }'
accepts "raw-ptr deref inside a trusted fn" unsafetrusted

emit unsafecap 'mod m { fn d() with(Unsafe) -> Int { let x: i32 = 5; let p: *const i32 = &x as *const i32; let v: i32 = *p; return v as Int; } fn main() with(Unsafe) -> Int { return d(); } }'
accepts "raw-ptr deref in a fn with(Unsafe)" unsafecap

echo "=== slice 5: discarding a pure, trap-free Copy value is a dead computation (E0294) ==="

# A pure, trap-free, non-Unit Copy value used as a statement (`expr;`) computes
# something and throws it away — an accidental lost computation. The value is
# Copy (non-Copy is E0287) and non-fallible (E0286); the flagged forms are
# literals, variable/field reads, and pure operators over such.
emit deadconst 'mod m { fn main() -> Int { 2 + 3; return 0; } }'
rejects_code "pure constant arithmetic discarded (2 + 3;)" deadconst "E0294"

emit deadvar 'mod m { fn main() -> Int { let x: Int = 10; x; return 0; } }'
rejects_code "bare variable read discarded (x;)" deadvar "E0294"

emit deadbin 'mod m { fn main() -> Int { let a: Int = 1; let b: Int = 2; a + b; return 0; } }'
rejects_code "pure variable arithmetic discarded (a + b;)" deadbin "E0294"

# The escape: `discard(expr)` acknowledges an intentional discard of a Copy value.
emit ackconst 'mod m { fn main() -> Int { discard(2 + 3); return 0; } }'
accepts "discard(2 + 3) acknowledges the discard" ackconst

# `discard(e)` is Unit-typed in value position too (`let u = discard(e)`), not the
# inner expr's type — Check and Core agree on Unit (no silent type drift).
emit ackval 'mod m { fn main() -> Int { let u = discard(2 + 3); return 0; } }'
accepts "discard in value position is Unit-typed (let u = discard(..))" ackval

# NOT flagged: a trap-assertion (division by zero is a real runtime check).
emit trapdiv 'mod m { fn main() -> Int { let a: Int = 10; let b: Int = 2; a / b; return 0; } }'
accepts "division discarded is a trap-assertion, not dead (a / b;)" trapdiv

# NOT flagged: an effectful call is legitimately invoked for its effect. The
# capability model does not track `&mut`/FFI effects, so calls are never flagged
# as pure — this row pins that a Console-effect call's discarded result is fine.
emit effcall 'mod m { fn h() with(Console) -> i32 { print_int(7); return 9; } fn main() with(Console) -> Int { h(); return 0; } }'
accepts "effectful (Console) call discarded is allowed (h();)" effcall

# NOT flagged: even an empty-cap user call is not treated as pure (calls excluded
# — empty caps != pure because &mut mutation is untracked).
emit purecall 'mod m { fn add2(a: i32, b: i32) -> i32 { return a + b; } fn main() -> Int { add2(1, 2); return 0; } }'
accepts "empty-cap user call discarded is allowed (add2(1,2);)" purecall

# `discard(...)` is the Copy-only escape; a non-Copy resource must be consumed or
# `destroy()`d (dropping it via discard would leak it).
emit discardlinear 'mod m { fn main() with(Std) -> Int { let s: String = int_to_string(5); discard(s); return 0; } }'
rejects_code "discard() of a non-Copy value is rejected (String)" discardlinear "E0295"

echo
echo "check_capability_judgment: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
