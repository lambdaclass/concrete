#!/usr/bin/env bash
# string_char_at semantics gate (audit 2026-07-16).
#
# The interpreter drifted from the backend on this builtin: it indexed
# CODEPOINTS and returned 0 on out-of-range, while compiled code indexes BYTES
# (UTF-8 data) and returns -1 on negative/out-of-range. Any non-ASCII string
# or OOB index made the differential oracle disagree with codegen — hiding
# real backend bugs and flagging non-bugs. The interp now matches the backend
# exactly. This gate pins the agreement over the drift surface: ASCII,
# non-ASCII (byte vs codepoint distinction), out-of-range, and negative index.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
PASS=0; DIV=0

agree(){ local label="$1" F="$2"
  local IOUT IRC COUT CRC
  IOUT="$("$COMPILER" "$F" --interp 2>&1)"; IRC=$?
  if "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1; then COUT="$("$F.bin" 2>&1)"; CRC=$?; else COUT="<compile-fail>"; CRC=250; fi
  if [ $IRC -eq 0 ] && [ $CRC -eq 0 ] && [ "$IOUT" = "$COUT" ]; then
    PASS=$((PASS+1)); echo "  ok   $label ($IOUT)"
  else
    DIV=$((DIV+1)); echo "  DIVERGE  $label"
    echo "      interp:   rc=$IRC $(printf '%s' "$IOUT" | head -1)"
    echo "      compiled: rc=$CRC $(printf '%s' "$COUT" | head -1)"
  fi; }

emit(){ printf '%s\n' "$2" > "$TMPDIR/$1.con"; }

emit ascii 'mod m { fn main() with(Console) { let s: String = "ABC"; print_int(string_char_at(&s, 1)); drop_string(s); } }'
agree "ascii index 1 (=> 66)" "$TMPDIR/ascii.con"

# "é" is two UTF-8 bytes (0xC3 0xA9): byte index 1 => 169, codepoint index 1
# would be 'b' = 98 — the exact interp/backend drift.
emit nonascii 'mod m { fn main() with(Console) { let s: String = "éb"; print_int(string_char_at(&s, 1)); drop_string(s); } }'
agree "non-ASCII byte index 1 (=> 169, not 98)" "$TMPDIR/nonascii.con"

emit oob 'mod m { fn main() with(Console) { let s: String = "abc"; print_int(string_char_at(&s, 3)); drop_string(s); } }'
agree "out-of-range (=> -1, not 0)" "$TMPDIR/oob.con"

emit neg 'mod m { fn main() with(Console) { let s: String = "abc"; print_int(string_char_at(&s, 0 - 1)); drop_string(s); } }'
agree "negative index (=> -1, not first char)" "$TMPDIR/neg.con"

# Same drift family (fixed with the above): string_length must be the BYTE
# length (backend len field), not codepoints — "é" is 2 bytes, 1 char.
emit lennonascii 'mod m { fn main() with(Console) { let s: String = "éb"; print_int(string_length(&s)); drop_string(s); } }'
agree "non-ASCII length (=> 3 bytes, not 2 chars)" "$TMPDIR/lennonascii.con"

# In-place mutation through &mut (interp gained string_push_char/string_append
# for the fuzzer grammar): push two ASCII bytes and append, then observe.
emit push 'mod m { fn main() with(Console) { let mut s: String = "a"; string_push_char(&mut s, 98); print_int(string_length(&s)); drop_string(s); } }'
agree "push_char then length (=> 2)" "$TMPDIR/push.con"

emit appnd 'mod m { fn main() with(Console) { let mut s: String = "ab"; let t: String = "cd"; string_append(&mut s, &t); print_int(string_char_at(&s, 3)); drop_string(t); drop_string(s); } }'
agree "append then char_at 3 (=> 100)" "$TMPDIR/appnd.con"

echo "STRING-CHAR-AT: PASS=$PASS DIVERGE=$DIV"
[ "$DIV" -eq 0 ]
