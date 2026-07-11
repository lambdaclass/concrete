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

echo
echo "check_capability_judgment: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
