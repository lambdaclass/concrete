#!/usr/bin/env bash
# R-0483: BORROWED ACCESS CANNOT OUTLIVE ITS OWNER, AND VALIDATION CANNOT OUTLIVE ITS BYTES.
#
# Three properties failed independently before the repair, and each needs its own check
# because fixing one does not fix the others:
#
#   lifetime  does the buffer still exist?            ByteCursor/Text kept raw pointers
#   identity  is it the buffer this view describes?   `describes` compared a LENGTH
#   validity  do the bytes still satisfy the check?   try_text never re-validated
#
# THE HISTORICAL FIXTURES ARE NOT THE COVERAGE. They stop compiling because the unsafe
# constructors are gone, and a compile error caused only by deleting an API proves the
# old spelling is absent, not that the replacement is sound. They are kept, and asserted
# to fail FOR THE RIGHT REASON, so that re-admitting the old surface is caught. The
# `current_*` fixtures are what actually exercise the replacement.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
FIX="$ROOT_DIR/tests/regressions/view_lifetime"
CC="$ROOT_DIR/.lake/build/bin/concrete"

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }

# `timeout` is coreutils and is NOT present on a stock macOS runner. run_tests.sh
# already degrades rather than skipping (see its PROJ_TIMEOUT), and this gate is
# invoked FROM run_tests.sh, so it runs on macOS too and needs the same treatment.
# A missing watchdog is worth saying out loud: a hang here would otherwise look
# like an unexplained CI stall.
if command -v timeout >/dev/null 2>&1; then
  TO="timeout 300"
else
  TO=""
  echo "  warn 'timeout' not found — running without a hang watchdog"
fi


# check_rejects <dir> <regex the diagnostic must match> <label>
check_rejects() {
  local d="$FIX/$1" pat="$2" label="$3" out
  [ -d "$d" ] || { no "$label: fixture directory is missing"; return; }
  out="$(cd "$d" && $TO "$CC" check . 2>&1)"
  if [ -z "$(printf '%s' "$out" | grep -E 'error\[')" ]; then
    no "$label: expected a compile error, got none"
    return
  fi
  if printf '%s' "$out" | grep -qE "$pat"; then
    ok "$label"
  else
    no "$label: rejected, but not for the stated reason (/$pat/)"
    printf '%s\n' "$out" | grep -E 'error\[' | head -2 | sed 's/^/       /'
  fi
}

# check_runs <dir> <expected exit> <label>
check_runs() {
  local d="$FIX/$1" want="$2" label="$3" rc
  [ -d "$d" ] || { no "$label: fixture directory is missing"; return; }
  # Build OUT OF TREE. `concrete build` otherwise drops a binary next to the sources,
  # and a gate that litters the working tree gets its droppings committed eventually.
  local out="$TMP/$(basename "$1")"   # basename: a fixture may live outside $FIX
  if ! (cd "$d" && $TO "$CC" build . -o "$out" >/dev/null 2>&1); then
    no "$label: expected it to build, and it did not"
    return
  fi
  "$out" >/dev/null 2>&1; rc=$?
  if [ "$rc" -eq "$want" ]; then ok "$label (exit $rc)"; else no "$label: expected exit $want, got $rc"; fi
}

echo "=== the replacement API works (positive controls) ==="
# Without these the gate would pass on a stdlib with the cursor deleted entirely.
check_runs current_cursor_valid_access 65 "valid borrowed read still returns its byte"
check_runs current_text_survives_mutation 66 "validated Text keeps its bytes after the source mutates"

echo "=== the replacement API refuses what it must (negative control) ==="
# A RUNTIME refusal, not a compile error: the cursor is re-checked against the buffer it
# is handed, so an emptied buffer yields Err rather than a read of whatever is there.
check_runs current_cursor_refuses_shrunk_buffer 0 "read against an emptied buffer is refused"

echo "=== the unsafe boundary still bites ==="
check_rejects unsafe_boundary 'E0521' "a hand-written pointer view demands Unsafe"

echo "=== the removed surface stays removed (historical reproductions) ==="
# Each names the constructor whose absence is the repair. If one of these starts failing
# for a different reason, the fixture has drifted and stopped testing re-admission.
check_rejects use_after_free 'from_bytes' "ByteCursor::from_bytes is gone"
check_rejects control 'from_bytes' "the original control's constructor is gone"
check_rejects cursor_across_realloc 'from_bytes' "the realloc reproduction's constructor is gone"
check_rejects text_use_after_free 'from_string|drop|Text' "borrowing Text::from_string is gone"
check_rejects byteview_cursor_use_after_free 'read_u8|cursor' "the pointer-bearing cursor conversion is gone"
check_rejects byteview_try_text_use_after_free 'try_text' "ByteView::try_text is gone"
check_rejects text_content_mutation 'try_text' "the content-mutation reproduction's entry point is gone"
check_rejects byteview_byte_rejected 'E0205' "consuming the owner still forbids a later coordinate read"

echo "=== the coordinate contract is stated, not silently weakened ==="
# `byteview_wrong_buffer_same_length` STILL COMPILES AND STILL READS THE OTHER BUFFER.
# That is deliberate. A ByteView is a range; it applies to any buffer satisfying its
# bounds, and it no longer pretends otherwise. The old `buf_len` brand rejected this
# case only when the lengths happened to differ and accepted it silently when they did
# not, which is the substitution that mattered. Asserting the documented behaviour here
# keeps a future "fix" from re-adding a half-brand without also re-deciding the contract.
check_runs byteview_wrong_buffer_same_length 91 "a view applies to any in-bounds buffer, by contract"

echo "=== R-0483 owner-bound survey: what is valid, rejected, detected, or accepted today ==="
# The survey that the owner-bound repair is measured against (tests/regressions/owner_bound_survey).
# VALID: moving the owner keeps a view's storage, so the read is correct.
check_runs ../owner_bound_survey/owner_moved 11 "moving the owner keeps the view valid (same storage)"
# ACCEPTED BY CONTRACT, pinned so a repair flips them deliberately: nothing binds a view to the
# owner it was made from — not a returned view past its owner's scope, not a cursor-derived
# view, not a view handed across a package boundary.
check_runs ../owner_bound_survey/view_outlives_scope 71 "an escaped view reads an unrelated owner (no owner identity)"
check_runs ../owner_bound_survey/of_cursor_substitution 51 "a cursor-derived view reads a different buffer (no owner identity)"
check_runs ../owner_bound_survey/xpkg_app 81 "a view from another package reads a different buffer (no owner identity)"

echo
echo "VIEW-LIFETIME: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
