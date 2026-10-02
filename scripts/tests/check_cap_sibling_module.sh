#!/usr/bin/env bash
# A CAPABILITY HEADER MUST BIND ACROSS A SIBLING SUBMODULE, NOT ONLY WITHIN ONE FILE.
#
# THE HOLE. CoreCheck built its function-signature table from ONE module's own
# functions and externs. A call into a sibling submodule therefore found no entry, and
# the lookup's `none` was read as "this call requires nothing" rather than "I do not know
# what this requires". The consequence was not a bad diagnostic — it was that a
# capability-free, non-`trusted` function could call a `with(Console)` sibling and PRINT.
# The `launder` fixture is the program that did: it compiled clean and wrote to stdout.
#
# The same lookup miss also dropped `Unsafe` from an `extern` reached through a sibling
# module (`extern_unsafe`), which is the leg that matters most — `Unsafe` is the one
# capability the language promises `extern` costs. A call to the SAME extern from within
# its own module was refused correctly the whole time, so the enforcement existed and only
# the cross-module spelling escaped it.
#
# TWO SPELLINGS, ONE REQUIREMENT. `prefixModuleFnNames` renames submodule functions
# (`sink::shout` becomes `sink_shout`) but deliberately leaves externs alone, since they
# name real C symbols. The call site still emits the prefixed form. The signature table
# records both, which is why `extern_unsafe` needs its own fixture rather than being
# assumed to follow from `launder`: the first fix closed the function path and left the
# extern path open, and only a separate program showed it.
#
# THE POSITIVE CONTROL IS NOT OPTIONAL. A rule that refuses every cross-submodule call
# would pass both negatives. `declared_ok` calls the same sibling function and the same
# extern with the authority declared, and must BUILD AND RUN.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
FIX="$ROOT_DIR/tests/regressions/cap_sibling_module"

if command -v timeout >/dev/null 2>&1; then TO="timeout 300"; else TO=""
  echo "  warn 'timeout' not found — running without a hang watchdog"; fi

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

echo "=== a capability-free caller cannot reach a with(Console) SIBLING ==="
lout="$(cd "$FIX/launder" && $TO "$CC" check . 2>&1)"
if printf '%s' "$lout" | grep -q "E0520"; then
  ok "the laundering call is refused (E0520)"
else
  no "the laundering call is ACCEPTED — a capability-free function can print"
fi
# The message must name the callee and the caller's actual authority, or the author
# cannot tell which of the two to change.
if printf '%s' "$lout" | grep -q "function 'sink_shout' requires Console but caller has (none)"; then
  ok "the diagnostic names the callee, the requirement and the caller's authority"
else
  no "the diagnostic does not identify the call precisely"
  printf '%s\n' "$lout" | grep -E 'error\[' | awk 'NR<=2' | sed 's/^/       /'
fi
# It must not merely fail to BUILD for some unrelated reason.
if (cd "$FIX/launder" && $TO "$CC" build . -o "$TMP/launder" >/dev/null 2>&1); then
  no "the laundering program still builds"
else
  ok "the laundering program does not build"
fi

echo "=== and an extern reached through a sibling still costs Unsafe ==="
eout="$(cd "$FIX/extern_unsafe" && $TO "$CC" check . 2>&1)"
if printf '%s' "$eout" | grep -qE "function 'raw_write' requires ([A-Za-z]+, )*Unsafe but caller has \(none\)"; then
  ok "the sibling-module extern call is refused under its PREFIXED spelling"
else
  no "the extern's Unsafe requirement is lost across a submodule boundary"
  printf '%s\n' "$eout" | grep -E 'error\[' | awk 'NR<=2' | sed 's/^/       /'
fi

echo "=== CONTROL: the same two calls are ACCEPTED when the authority is declared ==="
dout="$(cd "$FIX/declared_ok" && $TO "$CC" check . 2>&1)"
if printf '%s' "$dout" | grep -qE 'error\['; then
  no "the positive control is refused — the rule is refusing broadly, not narrowly"
  printf '%s\n' "$dout" | grep -E 'error\[' | awk 'NR<=3' | sed 's/^/       /'
else
  ok "declaring Console+Alloc, and Unsafe, makes both calls legal"
fi
# Behaviour, not just acceptance: it must actually run and print through the sibling.
if (cd "$FIX/declared_ok" && $TO "$CC" build . -o "$TMP/ok" >/dev/null 2>&1); then
  run="$("$TMP/ok" 2>&1)"; rc=$?
  if [ "$rc" -eq 0 ] && [ "$run" = "declared" ]; then
    ok "it runs and prints through the sibling (exit 0)"
  else
    no "positive control built but produced rc=$rc out='$run'"
  fi
else
  no "the positive control does not build"
fi

echo "=== trusted absorbs an extern call's Unsafe, never its declared effect (R-0484 R2) ==="
# Until 2026-10-01 a trusted body could NOT absorb an extern call's Unsafe, so effectful
# C functions were bound `trusted extern` — callable with no capability at all — and a
# trusted wrapper could perform I/O while declaring nothing. The rule is reversed, and the
# reversal is coupled to mandatory effect declarations (E0116): trust discharges the
# memory-safety obligation and nothing else. Both halves are compiled here, not just
# checked for presence.
pos="$ROOT_DIR/tests/programs/trusted_absorbs_extern_unsafe.con"
neg="$ROOT_DIR/tests/programs/error_trusted_extern_effect_not_absorbed.con"
if [ -f "$pos" ] && $TO "$CC" "$pos" -o "$TMP/absorb" >/dev/null 2>&1; then
  ok "a trusted body declaring Console calls a plain extern with(Console): Unsafe absorbed"
else
  no "trusted_absorbs_extern_unsafe.con does not build — trusted no longer absorbs extern Unsafe"
fi
nout="$( [ -f "$neg" ] && $TO "$CC" "$neg" -o "$TMP/noabsorb" 2>&1 )"
if printf '%s' "$nout" | grep -qE "function 'putchar' requires Console"; then
  ok "a trusted body without Console calling that extern is refused for Console — the effect is not absorbed"
else
  no "error_trusted_extern_effect_not_absorbed.con is not refused for Console: $(printf '%s' "$nout" | grep error | head -1 | cut -c1-160)"
fi

echo "=== a BODY-LESS declaration keeps the trusted modifier the author wrote ==="
# The parser carried `isPublic` on the body-less branch and dropped `isTrusted`, so
# `pub trusted fn sizeof<T>() -> u64;` reached `externFnRequiredCaps` as UNTRUSTED and was
# charged `Unsafe`. Invisible until cross-module requirements bound at all: the `mem` probe
# in check_std_compiled_coverage.sh compiles under the full `Std` set, which excludes Unsafe.
# Both fixtures are CHECK-ONLY — `#[intrinsic]` resolves by the compiler's own name table,
# so a renamed copy has no symbol to link. What is under test is what the checker charges.
tout="$(cd "$FIX/bodyless_trusted" && $TO "$CC" check . 2>&1)"
if printf '%s' "$tout" | grep -qE 'error\['; then
  no "a trusted body-less declaration is still charged a capability"
  printf '%s\n' "$tout" | grep -E 'error\[' | awk 'NR<=2' | sed 's/^/       /'
else
  ok "a trusted body-less declaration costs nothing"
fi
# CONTROL: without `trusted` it must STILL cost Unsafe, or carrying the modifier through
# made it meaningless rather than effective.
uout="$(cd "$FIX/bodyless_untrusted" && $TO "$CC" check . 2>&1)"
if printf '%s' "$uout" | grep -q "function 'my_sizeof' requires Unsafe"; then
  ok "the same shape WITHOUT trusted still costs Unsafe"
else
  no "an untrusted body-less declaration is free — the modifier now means nothing"
fi
# And the real instance: std.mem.sizeof must be reachable under Std, which excludes Unsafe.
if grep -q "pub trusted fn sizeof" "$ROOT_DIR/std/src/mem.con"; then
  ok "std.mem.sizeof is declared trusted (a size query is not raw-memory authority)"
else
  no "std.mem.sizeof lost its trusted marker — every Std-only program loses sizeof"
fi

echo "=== a PRELUDE-receiver method binds too (the last bug-071 escape) ==="
# INVERTED 2026-09-25. This check used to assert the hole was OPEN, and it passed while
# `check_cross_package_caps.sh` passed with the headline "free functions and methods
# alike" — two green gates asserting incompatible things. The newer gate only exercised
# an ASSOCIATED call (`TcpStream::connect`) and an EXPLICITLY IMPORTED receiver
# (`RawCursor::read_u8`); both have an import to walk, and `CModule.importedFnCaps` was
# built from `m.imports`. `String` needs no import, so its methods never entered the
# table and `String::drop` — `with(Alloc)` — was callable from a function declaring
# nothing. The program built and ran.
#
# THE LESSON, and it is why this block now enumerates: a headline claim must name the
# CALL FORMS it covers. "Methods bind" was true of two forms and false of a third.
hout="$(cd "$FIX/known_hole_cross_package_method" && $TO "$CC" check . 2>&1)"
if printf '%s' "$hout" | grep -q "function 'String_drop' requires Alloc but caller has (none)"; then
  ok "prelude receiver: String::drop is refused to a caller declaring nothing"
else
  no "the prelude-method escape is back — String::drop does not bind"
  printf '%s\n' "$hout" | grep -E 'error\[' | awk 'NR<=2' | sed 's/^/       /'
fi
if printf '%s' "$hout" | grep -q "function 'String_clone' requires Alloc"; then
  ok "and String::clone too — the whole declared set travels, not one method"
else
  no "only part of the prelude method surface binds"
fi
# It must be REFUSED, not merely fail to build for an unrelated reason.
if (cd "$FIX/known_hole_cross_package_method" && $TO "$CC" build . -o "$TMP/hole" >/dev/null 2>&1); then
  no "the prelude-method escape still builds"
else
  ok "the escape does not build"
fi
# POSITIVE CONTROL: the same calls with Alloc declared must build AND RUN, or the rule
# is refusing every prelude method rather than the undeclared ones.
if (cd "$FIX/prelude_method_ok" && $TO "$CC" build . -o "$TMP/pok" >/dev/null 2>&1) && "$TMP/pok" >/dev/null 2>&1; then
  ok "CONTROL: the same calls with Alloc declared build and run (exit 0)"
else
  no "the positive control fails — prelude methods are refused even when declared"
fi

echo "=== a union of capabilities covers a requirement that spans it ==="
# `bodyAuthority` produces `declared ∪ Unsafe`, which is what first exposed that
# `capsContain` asked whether EITHER side of a union covered the WHOLE requirement and
# answered no. That also made it disagree with `missingCaps`, which always normalized, so
# `decideCall` could report unsatisfied with nothing missing and render the self-refuting
# "requires Alloc, Unsafe but caller has Alloc + Unsafe".
if grep -A14 "def capsContain" "$ROOT_DIR/Concrete/Semantics/Capabilities.lean" |
     grep -q "caller.normalize"; then
  ok "capsContain normalizes the caller before testing membership"
else
  no "capsContain no longer normalizes — a union caller will under-approximate again"
fi

echo
echo "CAP-SIBLING-MODULE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
