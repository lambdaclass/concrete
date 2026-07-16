#!/usr/bin/env bash
# P7 #4 gate: the Bytes/Text boundary — Bytes is raw data; String/Text is
# validated UTF-8. No implicit conversion, no lossy conversion, no
# "string-ish bytes": every bytes->text crossing is CHECKED (Option) or an
# explicitly-named _unchecked obligation.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
M="docs/stdlib/STDLIB_SURFACE_MANIFEST.tsv"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# 1. the checked crossing: to_string is fallible (Option), validated
grep -P "^bytes\tto_string\t" "$M" | awk -F'\t' '$5=="option"' | grep -q . \
  && ok "Bytes::to_string is CHECKED (Option)" || no "to_string not Option"
grep -q "validate_utf8" std/src/bytes.con \
  && ok "to_string validates UTF-8" || no "to_string does not validate"

# 2. the unchecked crossing names its obligation
grep -P "^bytes\tto_string_unchecked\t" "$M" | grep -q . \
  && ok "_unchecked variant present (named obligation, 2a rule)" || no "_unchecked missing"
grep -qE "caller PROVES|valid UTF-8" std/src/bytes.con \
  && ok "_unchecked documents the caller obligation" || no "_unchecked undocumented"

# 3. no OTHER raw String construction from foreign bytes outside the sanctioned
#    modules (string.con owns String; bytes.con owns the two crossings above;
#    args.con validates argv before construction (gate-verified below);
#    text.con owns validated Text).
leaks=$(grep -rn "String { ptr" std/src/*.con | grep -vE "std/src/(string|bytes|args|path)\.con" | head -3)
[ -z "$leaks" ] && ok "no stray raw String construction outside string/bytes" \
  || no "stray String{ptr} construction: $leaks"

# 3b-pre. path validates before its String construction
grep -B16 "String { ptr: buf" std/src/path.con | grep -q "validate_utf8" \
  && ok "path.to_string validates before String construction" \
  || no "path builds String without validation"

# 3b. args validates argv before it becomes a String
grep -q "validate_utf8(raw, n)" std/src/args.con \
  && ok "args.get validates argv (OS bytes) before String construction" \
  || no "args.get builds String from unvalidated argv"

# 4. text's validated entry exists
grep -q "try_from_raw\|validate_utf8" std/src/text.con \
  && ok "Text keeps a validated entry path" || no "Text validation path missing"

echo "=== unicode policy (P7 5-6, docs/stdlib/UNICODE_POLICY.md) ==="
# unvalidated constructors of validated types must carry the _unchecked name
bad=$(grep -nE "pub fn from_raw\(" std/src/text.con std/src/string.con | head -2)
[ -z "$bad" ] && ok "no unvalidated from_raw on Text/String (only _unchecked names)"   || no "unvalidated validated-type constructor: $bad"
# ASCII helpers never transform bytes > 127
grep -qE "is_upper\(ch\)" std/src/ascii.con && grep -qE "is_lower\(ch\)" std/src/ascii.con   && ok "ascii to_lower/to_upper transform only A-Z/a-z (guarded by is_upper/is_lower)"   || no "ascii case helpers lost their ASCII-only guards"
# no normalization / case-folding / width APIs may appear
drift=$(grep -rlnE "pub fn (nfc|nfd|nfkc|nfkd|normalize|casefold|case_fold|grapheme|display_width|wcwidth)" std/src/*.con | head -2)
[ -z "$drift" ] && ok "no normalization/case-folding/display-width APIs (v1 non-goals)"   || no "v1 non-goal API appeared: $drift"
# the policy doc exists and pins the args long-term note
grep -q "get_bytes" docs/stdlib/UNICODE_POLICY.md   && ok "policy doc present incl. args.get_bytes long-term note" || no "UNICODE_POLICY.md missing/incomplete"

echo "=== path (P7 #7): OS bytes, pure value manipulation, display is separate ==="
# path APIs never carry File — filesystem authority belongs to std.fs
fleak=$(awk -F'\t' '$1=="path" && $6 ~ /File/ {print $2}' "$M" | head -3)
[ -z "$fleak" ] && ok "std.path carries no File (pure path-value manipulation)" \
  || no "path API with File authority (belongs in std.fs): $fleak"
# display is checked: to_string -> Option
grep -P "^path\tto_string\t" "$M" | awk -F'\t' '$5=="option"' | grep -q . \
  && ok "Path::to_string is CHECKED (Option — display separate from raw bytes)" \
  || no "path to_string not Option"
# raw accessor exists (the honest non-UTF-8 route)
grep -P "^path\tbytes_view\t" "$M" | grep -q . \
  && ok "raw bytes_view accessor present" || no "bytes_view missing"
# no normalization / canonicalization / symlink promises in the value layer
pd=$(grep -rlnE "pub fn (normalize|canonicalize|resolve|realpath|readlink|symlink)" std/src/path.con | head -1)
[ -z "$pd" ] && ok "no normalization/symlink/TOCTOU promises in std.path" \
  || no "filesystem-semantics API in the pure path layer: $pd"

echo
echo "BYTES-TEXT-BOUNDARY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
