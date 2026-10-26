#!/usr/bin/env bash
# ByteView gate (ROADMAP Phase 5 #5a — the owned, stored, zero-copy idiom).
#
# ByteView is the value-model-clean substitute for a stored `&[u8]` field: an
# owned, Copy, reference-free [off, len) handle branded to a buffer length. This
# gate locks the three things that make it sound:
#
#   1. VALUE MODEL — ByteView stores no pointer/reference. It is `struct Copy`
#      with only u64 fields. (If a *ptr field ever appears, it is no longer a
#      reference-free owned value and the stored-view guarantee is broken.)
#   2. ACCESS GOES BACK THROUGH A BUFFER — access methods take `buf: &Bytes` and
#      return Option (None on any check failure); nothing returns a reference.
#   3. THE GUARDS FIRE — the example programs self-verify that wrong-buffer,
#      out-of-bounds, and overflow uses return None rather than silently passing.
#      They exit 0 only when every unsafe use is rejected.
#
# See docs/language/BYTE_VIEW.md.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }

NUMERIC="std/src/numeric.con"
DOC="docs/language/BYTE_VIEW.md"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== 1. value model: ByteView is a reference-free Copy value ==="
[ -f "$NUMERIC" ] || { echo "error: $NUMERIC missing" >&2; exit 2; }
grep -q 'pub struct Copy ByteView' "$NUMERIC" \
  && ok "ByteView is 'struct Copy' (owned, copyable)" \
  || no "ByteView is not declared 'pub struct Copy ByteView'"

# Isolate the struct body and assert it has only u64 fields (off/len/buf_len) and
# no pointer field — a *ptr would make it a stored reference, not coordinates.
body="$(awk '/pub struct Copy ByteView \{/{f=1} f{print} /^    \}/{if(f) exit}' "$NUMERIC")"
if has_match -E '\*\s*(const|mut)' <<<"$body"; then
  no "ByteView struct contains a pointer field (not reference-free)"
else
  ok "ByteView struct has no pointer field (coordinates only)"
fi
for f in off len; do
  has_match -E "^\s*$f:\s*u64" <<<"$body" \
    && ok "ByteView.$f : u64" \
    || no "ByteView.$f : u64 missing"
done
# R-0483: `buf_len` is GONE and its absence is the assertion. It was described as a
# wrong-buffer brand, but it compared a LENGTH: it rejected a substituted buffer only
# when the lengths happened to differ and accepted a different buffer of the same length
# silently, which is the substitution that causes wrong answers. A guard that catches
# some substitutions reads at the call site like one that catches all of them, so it was
# removed rather than strengthened, and a ByteView is now documented as coordinates that
# apply to any buffer satisfying its bounds. Re-adding a length brand must fail here.
if has_match -E "^\s*buf_len:" <<<"$body"; then
  no "ByteView.buf_len is back — a length is not a buffer identity (R-0483)"
else
  ok "ByteView has no length brand (coordinates, not a claimed identity)"
fi

echo "=== 2. access takes an explicit buffer and returns Option (no returned ref) ==="
grep -qE 'pub fn cursor\(&self, buf: &Bytes\) -> Option<ByteCursor>' "$NUMERIC" \
  && ok "cursor(&self, buf: &Bytes) -> Option<ByteCursor>" \
  || no "cursor access method signature changed/missing"
grep -qE 'pub fn byte\(&self, buf: &Bytes, i: u64\) -> Option<u8>' "$NUMERIC" \
  && ok "byte(&self, buf: &Bytes, i: u64) -> Option<u8>" \
  || no "byte access method signature changed/missing"
grep -qE 'pub fn new\(off: u64, len: u64, buf: &Bytes\) -> Option<ByteView>' "$NUMERIC" \
  && ok "new(...) -> Option<ByteView> (checked construction)" \
  || no "checked constructor new(...) -> Option<ByteView> changed/missing"
# No access method may return a bare ByteView reference or Bytes — only Option/scalars/views.
if grep -E 'pub fn (cursor|byte|new|of_cursor|try_text)\(' "$NUMERIC" | has_match -E -- '->[[:space:]]*&'; then
  no "a ByteView API returns a reference (violates value model)"
else
  ok "no ByteView API returns a reference"
fi

echo "=== 2b. raw-bytes -> Text is an explicit, UTF-8-validated step ==="
# R-0483: `try_text` returned a Text BORROWING `buf`, so mutating the source afterwards
# left a "validated" value yielding bytes that were never validated. `to_text` copies
# into storage the source cannot reach; it needs Alloc, and that cost is the guarantee.
# The load-bearing facts are the SHAPE (takes the buffer, returns an owned Option<Text>)
# and that it COSTS Alloc — that is the copy being paid for. The capability list is matched
# with Alloc required and the rest open: pinning the exact set made this fail when the
# sibling-submodule repair (R-0484) added the `Unsafe` that `std.alloc` has always required,
# which is a stronger signature, not a weaker one. A gate that reds on a tightening teaches
# people to edit the gate.
grep -qE 'pub fn to_text\(&self, buf: &Bytes\) with\([^)]*\bAlloc\b[^)]*\) -> Option<Text>' "$NUMERIC" \
  && ok "ByteView::to_text(&self, buf) with(... Alloc ...) -> Option<Text> (copies)" \
  || no "ByteView::to_text signature changed/missing, or no longer costs Alloc"
grep -qE 'pub fn try_text\(' "$NUMERIC" \
  && no "ByteView::try_text is back — a borrowed Text cannot keep its validation" \
  || ok "the borrowing try_text is gone"
# The bounds test must not be spelled like an identity test.
grep -qE 'pub fn fits\(&self, buf: &Bytes\) -> bool' "$NUMERIC" \
  && ok "fits(&self, buf) -> bool names a bounds test, not an identity check" \
  || no "ByteView::fits missing — the bounds predicate must be named honestly"
TEXT="std/src/text.con"
[ -f "$TEXT" ] || { echo "error: $TEXT missing" >&2; exit 2; }
# R-0483: Text OWNS its storage now, so the raw constructor copies and needs Alloc. The
# old `try_from_raw` aliased the caller's region and was `Copy`, which is what let a
# validated value outlive — and disagree with — the bytes it validated.
grep -qE 'pub fn copy_from_raw\(ptr: \*const u8, len: u64\) with\([^)]*\bAlloc\b[^)]*\) -> Option<Text>' "$TEXT" \
  && ok "Text::copy_from_raw(ptr, len) with(... Alloc ...) -> Option<Text> (validated, copying)" \
  || no "Text::copy_from_raw changed/missing, or no longer costs Alloc"
grep -qE 'pub struct Copy Text' "$TEXT" \
  && no "Text is Copy again — an owning validated string must be linear" \
  || ok "Text is linear, not Copy (it owns its storage)"
grep -q 'fn validate_utf8(' "$TEXT" \
  && ok "UTF-8 validator (validate_utf8) present" \
  || no "validate_utf8 missing (copy_from_raw would be unvalidated)"

echo "=== 3. the guards fire: example programs self-verify and exit 0 ==="
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
for ex in http_header_view tlv_packet_view utf8_text_slice wrong_buffer; do
  proj="examples/byte_view/$ex"
  if [ ! -f "$proj/Concrete.toml" ]; then
    no "$ex: project missing ($proj/Concrete.toml)"
    continue
  fi
  if ! (cd "$proj" && "$COMPILER" build > "$TMP/$ex.build" 2>&1); then
    no "$ex: build failed"; sed 's/^/      /' "$TMP/$ex.build" | head -5
    continue
  fi
  bin="$proj/$ex"
  if [ ! -x "$bin" ]; then
    no "$ex: binary not produced at $bin"
    continue
  fi
  if "$bin" > "$TMP/$ex.out" 2>&1; then
    ok "$ex: built and ran, exit 0 (all guards held)"
  else
    no "$ex: ran with non-zero exit (a guard did not fire)"; sed 's/^/      /' "$TMP/$ex.out" | head -5
  fi
done

echo "=== 4. design doc present and referenced ==="
[ -f "$DOC" ] && ok "docs/language/BYTE_VIEW.md present" || no "docs/language/BYTE_VIEW.md missing"

echo ""
echo "BYTE-VIEW: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
