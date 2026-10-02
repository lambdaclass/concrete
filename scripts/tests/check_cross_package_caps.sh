#!/usr/bin/env bash
# A CAPABILITY DECLARED IN A DEPENDENCY MUST BIND ON ITS CALLER (bug 071).
#
# THE ESCAPE. A function declaring NOTHING, not `trusted`, called `std.env.get` —
# `with(Env, Alloc, Unsafe)` — and the program printed the value of `$HOME`. It checked
# clean, built clean and ran. The same held for the filesystem, the clock, the network,
# process exit, and `String::clone`.
#
# `std.io.println` was the sole exception, and not for a good reason: it is an INTRINSIC
# carrying a hardcoded capability (`Intrinsic.lean`), reached through `lookupBuiltinCap`'s
# fallback. So the enforced set across a package boundary was exactly the hardcoded
# intrinsic table, and every capability a dependency actually declared was decorative.
# That is why this gate asserts on `env`/`fs`/`time`/`net`/`process` and NOT on `println`:
# a gate built around `println` would have passed throughout.
#
# THE REPAIR. `FileSummary` already computes a capSet per imported callable; only Elab
# sees both that and the import list, so Elab joins them onto `CModule.importedFnCaps`,
# keyed by the LOCAL spelling a call site uses (`get` is declared in ten std modules, so
# a bare-name table would attach one module's requirement to another's function).
# CoreCheck consults it on the same `decideCall` path as local and sibling calls.
#
# COMPLETED 2026-09-25. `Unsafe` binds across a package like every other capability. It
# was held back for one release because enforcing it alongside the sinks produced 91
# diagnostics over 32 of 95 packages — 82 missing ONLY `Unsafe`, with the caller already
# holding the full `Std` set, which is DEFINED as every capability except `Unsafe`. The
# fix was not an exemption but the std migration: public `Unsafe` declarations 154 -> 21,
# because a safe wrapper marked `trusted` no longer re-exports a requirement its callers
# never owed. The last section asserts the uniform state and the `Std` control.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
FIX="$ROOT_DIR/tests/regressions/cross_package_caps"

if command -v timeout >/dev/null 2>&1; then TO="timeout 300"; else TO=""
  echo "  warn 'timeout' not found — running without a hang watchdog"; fi

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# A HEADLINE CLAIM MUST ENUMERATE THE CALL FORMS IT COVERS. "Free functions and methods
# alike" was true of imported free functions, of an explicitly imported receiver
# (`RawCursor::read_u8`) and of an associated call (`TcpStream::connect`) — and FALSE of a
# prelude receiver (`String::drop`), because the table was built by walking `m.imports`
# and `String` has no import statement. Two gates were green while asserting incompatible
# headlines. The four forms are now named, and the prelude form is covered by
# `check_cap_sibling_module.sh`'s inverted fixture.
#
# COVERED HERE: imported free function · imported receiver method · associated call.
# COVERED THERE: prelude receiver method.
echo "=== a caller declaring nothing cannot reach a dependency's sinks ==="
# Each row is a DIFFERENT capability, because the mechanism is per-capability set and a
# single sink would not show that the whole set travels.
check_refused() { # dir callee cap
  local out; out="$(cd "$FIX/$1" && $TO "$CC" check . 2>&1)"
  if printf '%s' "$out" | grep -q "function '$2' requires .*$3.* but caller has (none)"; then
    ok "$3: '$2' is refused"
  else
    no "$3: '$2' is NOT refused — the declared capability does not bind across the package"
    printf '%s\n' "$out" | grep -E 'error\[' | awk 'NR<=2' | sed 's/^/       /'
  fi
}
check_refused env_refused     get               Env
check_refused fs_refused      file_exists       File
check_refused time_refused    sleep             Time
check_refused process_refused process_exit      Process
# A cross-package METHOD, which was the originally-filed half of this defect: methods on
# an imported type must go through the same judgment as free functions.
check_refused net_refused     TcpStream_connect Network

echo "=== and the refusal is not merely a failure to build ==="
if (cd "$FIX/env_refused" && $TO "$CC" build . -o "$TMP/env" >/dev/null 2>&1); then
  no "the env escape still builds"
else
  ok "the env escape does not build"
fi

echo "=== CONTROL: declared authority is ACCEPTED, and the program RUNS ==="
dout="$(cd "$FIX/declared_ok" && $TO "$CC" check . 2>&1)"
if printf '%s' "$dout" | grep -qE 'error\['; then
  no "the positive control is refused — the rule over-applies"
  printf '%s\n' "$dout" | grep -E 'error\[' | awk 'NR<=3' | sed 's/^/       /'
else
  ok "Env+File+Time+Console declared: every call is legal"
fi
# Behaviour, not just acceptance. A compiler that refused everything would pass the
# refusal checks above and fail only here.
if (cd "$FIX/declared_ok" && $TO "$CC" build . -o "$TMP/ok" >/dev/null 2>&1); then
  run="$("$TMP/ok" 2>&1)"; rc=$?
  if [ "$rc" -eq 0 ] && [ "$run" = "cross-package authority declared" ]; then
    ok "it reads the env, stats a path, sleeps and prints (exit 0)"
  else
    no "positive control built but produced rc=$rc out='$run'"
  fi
else
  no "the positive control does not build"
fi

echo "=== a capability-FREE dependency call stays accepted, and cap-POLYMORPHIC instantiates ==="
cout="$(cd "$FIX/capfree_ok" && $TO "$CC" check . 2>&1)"
if printf '%s' "$cout" | grep -qE 'error\['; then
  no "a capability-free or cap-polymorphic import is now refused"
  printf '%s\n' "$cout" | grep -E 'error\[' | awk 'NR<=3' | sed 's/^/       /'
else
  ok "Duration::from_millis (declares nothing) and Vec::for_each_ctx (cap C) both check"
fi
if (cd "$FIX/capfree_ok" && $TO "$CC" build . -o "$TMP/cf" >/dev/null 2>&1) && "$TMP/cf" >/dev/null 2>&1; then
  ok "the polymorphic callback dispatches at runtime (exit 0)"
else
  no "capfree_ok does not build or does not run"
fi

echo "=== the requirement travels as a SET, not a single capability ==="
# `get` declares Env AND Alloc. Reporting only the first would look like enforcement
# while letting the rest through.
gout="$(cd "$FIX/env_refused" && $TO "$CC" check . 2>&1)"
if printf '%s' "$gout" | grep -qE "function 'get' requires .*Alloc.*Env|function 'get' requires .*Env.*Alloc"; then
  ok "both Env and Alloc are reported, not just one"
else
  no "only part of the declared set crossed the boundary"
fi

echo "=== the mechanism is keyed by LOCAL spelling, not bare name ==="
# `get` exists in ten std modules. A bare-name table would attach std.map's `get` (which
# requires nothing) or std.env's (which requires Env) to whichever was seen last.
if grep -q "importedFnCaps" "$ROOT_DIR/Concrete/Elab/Core.lean" &&
   grep -q "importedCaps" "$ROOT_DIR/Concrete/Check/CoreCheck.lean"; then
  ok "importedFnCaps is carried on the module and consulted by CoreCheck"
else
  no "the imported-capability transport is gone"
fi
# Local definitions must still win a name they share with an import.
if grep -A6 "env.fnSigs.find?" "$ROOT_DIR/Concrete/Check/CoreCheck.lean" | grep -q "importedCaps"; then
  ok "imports are consulted AFTER the module's own signatures"
else
  no "lookup order changed — an import may now shadow a local definition"
fi

echo "=== Unsafe binds across a package, like every other capability ==="
# REMOVED 2026-09-25. `Unsafe` now binds across a package like every other capability.
# What made that affordable was not an exemption but the std migration: public `Unsafe`
# declarations 154 -> 21, because a safe wrapper marked `trusted` no longer re-exports a
# requirement its callers never owed. The 82 diagnostics went with them.
if grep -q "dropCrossPackageUnsafe caps) *$" "$ROOT_DIR/Concrete/Check/CoreCheck.lean"; then
  no "the cross-package Unsafe exception is BACK — every capability must bind uniformly"
else
  ok "no cross-package Unsafe exception: every declared capability binds uniformly"
fi
# The load-bearing consequence, asserted on a real dependency API rather than a fixture:
# an unchecked read must refuse a caller that declares nothing.
if [ -d "$FIX/rawcursor_refused" ]; then
  rout="$(cd "$FIX/rawcursor_refused" && $TO "$CC" check . 2>&1)"
  if printf '%s' "$rout" | grep -q "requires Unsafe but caller has (none)"; then
    ok "Unsafe: RawCursor::read_u8 is refused across a package"
  else
    no "a cross-package Unsafe obligation does not bind"
  fi
else
  no "the rawcursor_refused fixture is missing"
fi
# CONTROL, and the whole point: ordinary `Std` code is untouched. `Std` is DEFINED as
# every capability except `Unsafe`, so if this failed the enforcement would have made the
# standard capability set unable to use the standard library.
if [ -d "$FIX/std_program_ok" ]; then
  if (cd "$FIX/std_program_ok" && $TO "$CC" build . -o "$TMP/stdok" >/dev/null 2>&1) \
     && [ "$("$TMP/stdok" 2>&1)" = "Std program, no Unsafe" ]; then
    ok "a with(Std) program using Vec::push and println builds and RUNS"
  else
    no "ordinary Std code broke — enforcement is not narrow"
  fi
else
  no "the std_program_ok fixture is missing"
fi
# It must be the ONLY thing held back: a local or sibling Unsafe requirement still binds.
if [ -d "$ROOT_DIR/tests/regressions/cap_sibling_module/extern_unsafe" ]; then
  sout="$(cd "$ROOT_DIR/tests/regressions/cap_sibling_module/extern_unsafe" && $TO "$CC" check . 2>&1)"
  if printf '%s' "$sout" | grep -qE "requires ([A-Za-z]+, )*Unsafe"; then
    ok "a SIBLING-module Unsafe requirement binds too — local, sibling and dependency agree"
  else
    no "Unsafe stopped binding within a package"
  fi
else
  no "the sibling-module extern fixture is missing, so the exception's scope is unchecked"
fi
# And the raw-OPERATION gate is untouched by any of this.
if grep -q "def capsAllowUnsafeOp" "$ROOT_DIR/Concrete/Semantics/Capabilities.lean"; then
  ok "capsAllowUnsafeOp still gates raw operations (E0521), independent of any call rule"
else
  no "the raw-operation gate is gone"
fi

echo
echo "CROSS-PACKAGE-CAPS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
