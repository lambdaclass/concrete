#!/usr/bin/env bash
# PER-METHOD `trusted`, AND THE TWO AXES IT KEEPS APART.
#
# WHY IT EXISTS. `trusted` was only a whole-impl modifier, so the commonest shape in a
# systems standard library — SAFE TO CALL, AUDITED IMPLEMENTATION, i.e. `Vec::push` —
# was not expressible for a method. The only way to license a method's raw work was to
# mark the entire `impl` trusted, vouching for every method in the block, which is the
# opposite of a small explicit boundary. Measured before this landed: 67 of 154 public
# `std` signatures declaring `Unsafe` perform raw operations in their OWN body, so this
# was not a corner case, it was most of the library.
#
# THE TWO AXES ARE INDEPENDENT, and that is the whole point:
#   trusted            the IMPLEMENTATION is audited
#   with(Unsafe)       the CALLER owes an invariant the language cannot establish
# Either alone is meaningful; both together are meaningful
# (`pub trusted fn read_unchecked(..) with(Unsafe)`); and neither implies the other.
#
# WHAT TRUST MUST NEVER DO is erase operational authority. A trusted method declaring
# `with(Console)` is still refused to a caller holding none — trust is about memory
# discipline, not about permission to reach a sink.
set -uo pipefail
# Matches read here-strings, not `printf | grep -q`: under pipefail an early-exiting grep -q
# SIGPIPEs printf on large output and turns a MATCH into a failure (CI run 37678716769, step 99).
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
FIX="$ROOT_DIR/tests/regressions/trusted_method"

if command -v timeout >/dev/null 2>&1; then TO="timeout 300"; else TO=""
  echo "  warn 'timeout' not found — running without a hang watchdog"; fi

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# EVERY RESULT FALLS IN EXACTLY ONE BUCKET: success, expected refusal, unexpected
# failure. "No diagnostic I grepped for" is NOT success — a parse or dependency error
# produces no E0520 either, and reading that as a pass is how a whole measurement round
# was lost earlier: a std that failed to PARSE scored 0 capability errors and looked green.
# BUILD, not CHECK, is the compile signal. `concrete check` exits 1 when a function is
# proof-eligible with no registered proof — a legitimate nonzero that says nothing about
# whether the program compiles. Keying "positive" on check's exit code therefore fails a
# perfectly good program, and keying it on "no error[ lines" alone is the weakness that
# lost a measurement round. `build` exiting 0 is strictly stronger than either: it links.
expect_clean() { # dir label
  local out rc
  out="$(cd "$FIX/$1" && $TO "$CC" build . -o "$TMP/ec_$1" 2>&1)"; rc=$?
  if [ "$rc" -ne 0 ] || grep -qE 'error\[' <<<"$out"; then
    no "$2 — expected a clean build, got rc=$rc"
    printf '%s\n' "$out" | grep -E 'error' | awk 'NR<=3' | sed 's/^/       /'
  else
    ok "$2"
  fi
}
expect_refusal() { # dir code label
  local out
  out="$(cd "$FIX/$1" && $TO "$CC" check . 2>&1)"
  if grep -qE 'error\[parse\]|unknown module|unknown function' <<<"$out"; then
    no "$3 — UNEXPECTED failure kind (parse/resolve), not the refusal under test"
    printf '%s\n' "$out" | grep -E 'error\[' | awk 'NR<=2' | sed 's/^/       /'
  elif grep -q "($2)" <<<"$out"; then
    ok "$3"
  else
    no "$3 — expected $2, got: $(printf '%s' "$out" | grep -oE '\(E0[0-9]+\)' | head -1 | tr -d '\n')${out:+}"
  fi
}

echo "=== the two axes, independently and together ==="
expect_clean axes_ok "a trusted method with a safe interface is callable by a caller declaring nothing"

echo "=== trust does not discharge what the CALLER owes ==="
expect_refusal obligation_binds E0520 "a trusted method declaring with(Unsafe) still requires it of the caller"

echo "=== trust does not erase OPERATIONAL authority ==="
expect_refusal operational_binds E0520 "a trusted method declaring with(Console) still requires it of the caller"

echo "=== CONTROL: trust is what licenses the raw operation ==="
# Without this the fixtures above would pass on a compiler that simply stopped checking
# raw operations, which is a different bug with the same green.
expect_refusal raw_needs_trust E0521 "the SAME body in a NON-trusted method is refused"

echo "=== it lowers, links and runs — not merely checks ==="
if (cd "$FIX/axes_ok" && $TO "$CC" build . -o "$TMP/ax" >/dev/null 2>&1); then
  run="$("$TMP/ax" 2>&1)"; rc=$?
  if [ "$rc" -eq 0 ] && [ "$run" = "trusted method ran" ]; then
    ok "the trusted method dispatches and its raw read returns the right value (exit 0)"
  else
    no "axes_ok built but produced rc=$rc out='$run'"
  fi
else
  no "axes_ok does not build — per-method trusted survives parsing but not lowering"
fi

echo "=== provenance: the call is RECORDED, not silently absorbed ==="
# Trust licenses the operation; it must not hide that the operation is there. This is
# the fact a consumer needs once `Unsafe` stops appearing in safe signatures.
te="$(cd "$FIX/axes_ok" && $TO "$CC" src/main.con --report trust-edges 2>&1)"
if grep -q "calls-trusted	Box_peek" <<<"$te"; then
  ok "--report trust-edges records calls-trusted for the method"
else
  no "the trusted method call is not recorded as a trust edge"
fi
if grep -q "contains-raw-op" <<<"$te"; then
  ok "and the raw operation inside it is recorded too"
else
  no "the raw operation inside a trusted method vanished from the edges"
fi

echo "=== trust edges cross a PACKAGE boundary ==="
# Single-file mode elaborates one module, so a std-using program produced an EMPTY edge
# set — the same one-module blind spot behind R-0484's cross-package opacity. Project
# mode checks the dependency (its diagnostics arrive prefixed `[std] [string]`), so the
# edges exist there. This is what has to work before `Unsafe` can be removed from a safe
# wrapper: otherwise the removal deletes the only signal a consumer has.
xp="$(cd "$ROOT_DIR/examples/base64_cli" && $TO "$CC" check . --report trust-edges 2>&1)"
mods="$(printf '%s' "$xp" | grep -v '^#' | cut -f1 | sort -u | grep -cE '^[a-z_]+$')"
if [ "${mods:-0}" -ge 20 ]; then
  ok "a consumer sees its dependency's edges ($mods modules)"
else
  no "cross-package trust edges are missing — only $mods modules visible"
fi
if grep -q "^alloc	alloc_dealloc	calls-ffi" <<<"$xp"; then
  ok "and they name the actual FFI leaf (alloc_dealloc -> free)"
else
  no "the dependency's FFI leaves are not visible to a consumer"
fi
# JUSTIFICATION USES AUTHORITY, NOT LITERAL MEMBERSHIP. A `cap C` function satisfies the
# raw-operation gate through its variable; asking `capSetHasUnsafe` instead invented four
# unjustified raw operations in `ordered_set` that the checker had authorized all along.
# A provenance report inventing an unjustified operation is the defect class this whole
# area exists to prevent, so the count is pinned at zero.
if grep -q "unjustified-raw-op=0" <<<"$xp"; then
  ok "no unjustified raw operations across the dependency (cap variables counted as authority)"
else
  no "unjustified raw operations reported: $(printf '%s' "$xp" | grep -oE 'unjustified-raw-op=[0-9]+')"
fi

echo "=== the report does not overclaim about the IMPL ==="
# `trustedImplOrigin` is set from the FUNCTION's flag, so with per-method trust a method
# can be audited inside a block that is not. Labelling that "trusted impl X" asserts
# something about the block that need not be true.
if grep -q "trusted methods in impl" "$ROOT_DIR/Concrete/Report/ReportInterface.lean"; then
  ok "the unsafe report says 'trusted methods in impl X', not 'trusted impl X'"
else
  no "the report labels a per-method trust as a trusted IMPL — an overclaim"
fi

echo
echo "TRUSTED-METHOD: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
