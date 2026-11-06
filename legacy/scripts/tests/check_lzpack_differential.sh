#!/usr/bin/env bash
# Phase 7 workload 9 gate: lzpack (LZSS + PackBits RLE codec in a CRC-checked
# container) must produce compressed streams byte-identical to an independent
# Python implementation of the same format specification
# (scripts/tests/lib/lzpack_ref.py), decode that implementation's streams, round
# trip real text, binary and synthetic inputs at scale, refuse corrupt streams
# with exit 1, and keep its 13t exit codes (usage 2). The greedy hash-chain
# parse is fully specified, so byte identity is the differential axis: a
# miscompiled shift, mask or loop bound shows up as a different stream.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
REF="$ROOT_DIR/scripts/tests/lib/lzpack_ref.py"
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

( cd examples/lzpack && "$C" build ) >/dev/null 2>&1 \
  && ok "lzpack builds" || { no "lzpack build failed"; echo "LZPACK: PASS=$PASS FAIL=$FAIL"; exit 1; }
B="$ROOT_DIR/examples/lzpack/lzpack"

# Inputs: real text, source code, binary, and synthetic edge shapes.
cp ROADMAP.md "$TMP/roadmap.txt"
cp Concrete/Frontend/Parser.lean "$TMP/parser.lean"
head -c 300000 "$C" > "$TMP/binary.bin"
: > "$TMP/empty.bin"
printf 'q' > "$TMP/one.bin"
python3 - "$TMP" <<'EOF'
import random, sys
d = sys.argv[1]
random.seed(9)
open(d + "/random.bin", "wb").write(bytes(random.getrandbits(8) for _ in range(65536)))
open(d + "/zeros.bin", "wb").write(bytes(100000))
open(d + "/allbytes.bin", "wb").write(bytes(range(256)) * 40)
# runs of every length 1..140 around the RLE thresholds (2/3, 128/130)
open(d + "/runs.bin", "wb").write(b"".join(bytes([i % 251]) * i for i in range(1, 141)))
# matches at the window edge: a 20-byte phrase repeated 4090..4100 bytes apart
phrase = b"window-edge-phrase!!"
buf = bytearray()
for gap in range(4070, 4102):
    buf += phrase + bytes(random.getrandbits(8) for _ in range(gap - len(phrase)))
buf += phrase
open(d + "/window.bin", "wb").write(bytes(buf))
EOF

# 1. Byte-identical compressed streams, both methods, plus round trips.
for f in roadmap.txt parser.lean binary.bin empty.bin one.bin random.bin zeros.bin allbytes.bin runs.bin window.bin; do
  for m in lz rle; do
    if [ "$m" = lz ]; then flag=""; rc_mode=c; else flag="-r"; rc_mode=cr; fi
    "$B" c $flag "$TMP/$f" "$TMP/$f.$m.ours" >/dev/null 2>&1; rc=$?
    python3 "$REF" "$rc_mode" "$TMP/$f" "$TMP/$f.$m.want"
    if [ $rc -eq 0 ] && cmp -s "$TMP/$f.$m.want" "$TMP/$f.$m.ours"; then
      ok "$m stream byte-identical to the reference: $f ($(wc -c < "$TMP/$f" | tr -d ' ') -> $(wc -c < "$TMP/$f.$m.ours" | tr -d ' ') bytes)"
    else
      no "$m stream differs from the reference: $f (rc=$rc)"
    fi
    # the Concrete decoder on the REFERENCE stream
    "$B" d "$TMP/$f.$m.want" "$TMP/$f.$m.back" >/dev/null 2>&1; rc=$?
    [ $rc -eq 0 ] && cmp -s "$TMP/$f" "$TMP/$f.$m.back" \
      && ok "$m decode of the reference stream reproduces $f" \
      || no "$m decode of the reference stream: $f (rc=$rc)"
  done
done

# 2. `t` (in-memory round trip of both codecs) on real text.
"$B" t "$TMP/parser.lean" > "$TMP/t.out" 2>&1; rc=$?
[ $rc -eq 0 ] && [ "$(grep -c ' ok$' "$TMP/t.out")" -eq 2 ] \
  && ok "t: both codecs round-trip Parser.lean in memory" || no "t on Parser.lean (rc=$rc): $(head -2 "$TMP/t.out" | tr '\n' ' ')"

# 3. selftest: built-in vectors and corrupt-stream classification.
"$B" selftest > "$TMP/self.out" 2>&1; rc=$?
[ $rc -eq 0 ] && tail -1 "$TMP/self.out" | has_match -x 'selftest: all ok' \
  && ok "selftest: all vectors and corrupt streams classified" || no "selftest (rc=$rc)"

# 4. Corrupt streams: the decoder must exit 1 with the reference's diagnosis.
python3 - "$TMP" <<'EOF'
import sys
d = sys.argv[1]
good = open(d + "/parser.lean.lz.want", "rb").read()
cases = {
    "cut": good[:-1],
    "header_only": good[:11],
    "magic": b"LZQ" + good[3:],
    "method": good[:3] + b"9" + good[4:],
    "trailing": good + b"\0",
    "crc": good[:8] + bytes([good[8] ^ 1]) + good[9:],
    "distance": b"LZP1" + (10).to_bytes(4, "little") + bytes(4) + bytes([1, 0, 0]),
    "rle_overrun": b"LZP2" + (2).to_bytes(4, "little") + bytes(4) + bytes([128, 65]),
}
for k, v in cases.items():
    open(d + "/bad_" + k + ".lzp", "wb").write(v)
EOF
for k in cut header_only magic method trailing crc distance rle_overrun; do
  "$B" d "$TMP/bad_$k.lzp" "$TMP/bad.out" > /dev/null 2> "$TMP/bad.err"; rc=$?
  want=$(python3 "$REF" d "$TMP/bad_$k.lzp" "$TMP/bad.ref" 2>&1 >/dev/null)
  got=$(sed 's/^lzpack: //' "$TMP/bad.err")
  [ $rc -eq 1 ] && [ "$got" = "$want" ] \
    && ok "corrupt $k -> exit 1, '$got'" || no "corrupt $k: rc=$rc got='$got' want='$want'"
done

# 5. 13t exit codes.
"$B" >/dev/null 2>&1;                               [ $? -eq 2 ] && ok "no args -> 2"            || no "no-args rc"
"$B" x a >/dev/null 2>&1;                           [ $? -eq 2 ] && ok "unknown command -> 2"    || no "unknown-command rc"
"$B" c "$TMP/one.bin" >/dev/null 2>&1;              [ $? -eq 2 ] && ok "c with one path -> 2"    || no "c-arity rc"
"$B" d -r "$TMP/a" "$TMP/b" >/dev/null 2>&1;        [ $? -eq 2 ] && ok "-r on d -> 2"            || no "d -r rc"
"$B" c -z "$TMP/a" "$TMP/b" >/dev/null 2>&1;        [ $? -eq 2 ] && ok "unknown flag -> 2"       || no "unknown-flag rc"
"$B" c "$TMP/nope" "$TMP/b" >/dev/null 2>&1;        [ $? -eq 1 ] && ok "missing input -> 1"      || no "missing-input rc"
"$B" c "$TMP/one.bin" "$TMP/no/such/dir" >/dev/null 2>&1; [ $? -eq 1 ] && ok "unwritable output -> 1" || no "unwritable-output rc"

echo
echo "LZPACK: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
