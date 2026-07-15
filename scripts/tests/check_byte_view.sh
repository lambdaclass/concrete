#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
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
# See docs/BYTE_VIEW.md.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }

NUMERIC="std/src/numeric.con"
DOC="docs/BYTE_VIEW.md"

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
if printf '%s\n' "$body" | grep -qE '\*\s*(const|mut)'; then
  no "ByteView struct contains a pointer field (not reference-free)"
else
  ok "ByteView struct has no pointer field (coordinates only)"
fi
for f in off len buf_len; do
  printf '%s\n' "$body" | grep -qE "^\s*$f:\s*u64" \
    && ok "ByteView.$f : u64" \
    || no "ByteView.$f : u64 missing"
done

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
if grep -E 'pub fn (cursor|byte|new|of_cursor|try_text)\(' "$NUMERIC" | grep -qE -- '->[[:space:]]*&'; then
  no "a ByteView API returns a reference (violates value model)"
else
  ok "no ByteView API returns a reference"
fi

echo "=== 2b. raw-bytes -> Text is an explicit, UTF-8-validated step ==="
grep -qE 'pub fn try_text\(&self, buf: &Bytes\) -> Option<Text>' "$NUMERIC" \
  && ok "ByteView::try_text(&self, buf) -> Option<Text>" \
  || no "ByteView::try_text signature changed/missing"
TEXT="std/src/text.con"
[ -f "$TEXT" ] || { echo "error: $TEXT missing" >&2; exit 2; }
grep -qE 'pub fn try_from_raw\(ptr: \*const u8, len: u64\) -> Option<Text>' "$TEXT" \
  && ok "Text::try_from_raw(ptr, len) -> Option<Text> (validated)" \
  || no "Text::try_from_raw changed/missing"
grep -q 'fn validate_utf8(' "$TEXT" \
  && ok "UTF-8 validator (validate_utf8) present" \
  || no "validate_utf8 missing (try_from_raw would be unvalidated)"

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
[ -f "$DOC" ] && ok "docs/BYTE_VIEW.md present" || no "docs/BYTE_VIEW.md missing"

echo ""
echo "BYTE-VIEW: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
