#!/usr/bin/env bash
# Stdlib hardening pass (ROADMAP Phase 7 item 0, H1-H5) — CLASS gates, one per
# hardening item, so none can regress to a one-instance fix. Plus the
# unproven-content check: exact-COUNT gates once blessed an unproven std
# obligation (io.read_all's indexed buffer); counts are not content.
set -uo pipefail
cd "$(dirname "$0")/../.."
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# H1: every radix parser guards overflow -> None (and the class test exists)
for f in parse_hex parse_bin parse_oct; do
  awk "/pub fn $f/,/^    }/" std/src/parse.con | grep -q "return Option::<u64>::None" \
    && ok "H1: $f has a None path" || no "H1: $f lost its None path"
done
grep -q "test_parse_radix_overflow_is_none" std/src/parse.con \
  && ok "H1: per-radix overflow class test present" \
  || no "H1: overflow class test missing"

# H2: no overflow-prone additive bounds guards in std (the lint). The safe
# forms are `A > total || B > total - A` or a pre-checked subtraction.
if grep -nE 'if [a-z_.]+ \+ [a-z_.()0-9]+ >=? ' std/src/*.con | grep -vE '//' | grep -q .; then
  no "H2: additive bounds guard found in std (overflow-prone):"
  grep -nE 'if [a-z_.]+ \+ [a-z_.()0-9]+ >=? ' std/src/*.con | grep -vE '//' | head -3
else
  ok "H2: no additive bounds guards in std (lint clean)"
fi

# H3: IO ops surface real OS errors
grep -q "ferror" std/src/io.con && ok "H3: file_read consults ferror (EOF vs ReadFailed)" || no "H3: file_read does not consult ferror"
grep -q "FlushFailed" std/src/io.con && ok "H3: flush surfaces FlushFailed" || no "H3: flush swallows errors"
grep -q "CloseFailed" std/src/io.con && ok "H3: close surfaces CloseFailed" || no "H3: close swallows errors"
awk '/trusted fn console_write/,/^    }/' std/src/io.con | grep -q "WriteFailed" \
  && ok "H3: console_write reports short/failed writes" || no "H3: console_write pretends writes always land"

# H4: ordered traversal is unbounded (borrowed non-Copy values) + fixture
grep -q "test_omap_traversal_non_copy_values" std/src/ordered_map.con \
  && ok "H4: non-Copy traversal fixture present" || no "H4: non-Copy traversal fixture missing"
awk '/trusted impl<K, V> OrderedMap/,/^    }$/' std/src/ordered_map.con | grep -q "pub fn fold" \
  && ok "H4: fold lives in the unbounded impl" || no "H4: fold still Copy-bounded"

# H5: strict canonical base64 padding + reject fixtures
grep -q 'reject_case("Zh==")' std/src/base64.con && grep -q 'reject_case("Zm9=")' std/src/base64.con \
  && ok "H5: non-canonical pad-bit reject fixtures present" \
  || no "H5: pad-bit reject fixtures missing"
grep -q "STRICT canonical padding" std/src/base64.con \
  && ok "H5: strict pad-bit checks in decode" || no "H5: decode permissive on pad bits"

# Unproven-content check: committed contract snapshots must carry NO unproven
# obligation under any std module section (counts bless; content catches).
STD_MODS=$(ls std/src/*.con | xargs -n1 basename | sed 's/\.con$//' | paste -sd'|' -)
bad=0
for snap in scripts/tests/phase1_snapshots/*.txt examples/*/snapshot/contracts.txt; do
  [ -f "$snap" ] || continue
  hits=$(awk -v mods="^($STD_MODS)\\\\." '
    $0 ~ mods { insec=1; sect=$0; next }
    /^[a-zA-Z_]/ { insec=0 }
    insec && /status: unproven/ { print FILENAME": "sect }
  ' "$snap" | head -2)
  if [ -n "$hits" ]; then bad=1; echo "$hits"; fi
done
[ "$bad" = "0" ] && ok "unproven-content: no std module carries an unproven obligation in committed snapshots" \
                 || no "unproven-content: std snapshot(s) contain unproven obligations (above)"

echo
echo "STD-HARDENING: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
