#!/usr/bin/env bash
# Phase 7 workload 10 gate: stackvm (assembler, bytecode codec and verifier, interpreter,
# disassembler) must agree byte for byte with an independent Python implementation of the same
# specifications (scripts/tests/lib/stackvm_ref.py) on every output a run has: the assembled
# file, the listing, the program's output, the diagnostic on stderr and the exit code. Inputs:
# the example's test programs, seeded generated programs (scripts/tests/lib/stackvm_gen.py, valid
# ones and one line-level mutation of each), and seeded corruptions of assembled files for the
# loader. Plus the 13t exit codes.
set -uo pipefail
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
REF="$ROOT_DIR/scripts/tests/lib/stackvm_ref.py"
GEN="$ROOT_DIR/scripts/tests/lib/stackvm_gen.py"
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

( cd examples/stackvm && "$C" build ) >/dev/null 2>&1 \
  && ok "stackvm builds" || { no "stackvm build failed"; echo "STACKVM: PASS=$PASS FAIL=$FAIL"; exit 1; }
B="$ROOT_DIR/examples/stackvm/stackvm"

# One observation: stdout, stderr and exit code of a command, as one file.
observe(){ local out="$1"; shift; "$@" > "$out.o" 2> "$out.e"; echo "rc=$?" > "$out.rc"; cat "$out.o" "$out.e" "$out.rc" > "$out"; }

# agree <label> <args...>: the binary and the reference give the same observation.
agree(){
  local label="$1"; shift
  observe "$TMP/ours" "$B" "$@"
  observe "$TMP/want" python3 "$REF" "$@"
  if cmp -s "$TMP/ours" "$TMP/want"; then return 0; fi
  echo "    $label differs:"; diff "$TMP/want" "$TMP/ours" | head -6 | sed 's/^/      /'
  return 1
}

# 1. The test programs: every command, plus the assembled bytes.
for f in examples/stackvm/testdata/*.sasm; do
  n=$(basename "$f" .sasm); bad=0
  agree "$n exec" exec "$f" || bad=1
  agree "$n check" check "$f" || bad=1
  observe "$TMP/ours" "$B" asm "$f" "$TMP/$n.ours.svm"
  observe "$TMP/want" python3 "$REF" asm "$f" "$TMP/$n.ref.svm"
  cmp -s "$TMP/ours" "$TMP/want" || { echo "    $n asm differs"; diff "$TMP/want" "$TMP/ours" | head -4; bad=1; }
  if [ -f "$TMP/$n.ref.svm" ]; then
    cmp -s "$TMP/$n.ref.svm" "$TMP/$n.ours.svm" || { echo "    $n: assembled bytes differ"; bad=1; }
    agree "$n dis" dis "$TMP/$n.ref.svm" || bad=1
    agree "$n run" run "$TMP/$n.ref.svm" || bad=1
  fi
  [ $bad -eq 0 ] && ok "testdata/$n.sasm: exec, check, asm bytes, dis and run agree with the reference" \
    || no "testdata/$n.sasm disagrees with the reference"
done

# 2. Generated programs (valid, and one mutation of each).
mkdir -p "$TMP/gen"
python3 "$GEN" "$TMP/gen" 20261024 120
for kind in gen mut; do
  total=0; bad=0; failing=""
  for f in "$TMP"/gen/${kind}_*.sasm; do
    total=$((total+1))
    if ! agree "$(basename "$f")" exec "$f" >/dev/null || ! agree "$(basename "$f")" check "$f" >/dev/null; then
      bad=$((bad+1)); failing="$failing $(basename "$f")"
    fi
  done
  [ $bad -eq 0 ] && ok "$total generated $kind programs: exec and check agree with the reference" \
    || no "$bad of $total generated $kind programs disagree:$(echo "$failing" | cut -c1-200)"
done

# 3. Loader: seeded corruptions of assembled files (bytes flipped, truncated, extended).
python3 - "$TMP" "$REF" <<'EOF'
import random, subprocess, sys
tmp, ref = sys.argv[1], sys.argv[2]
r = random.Random(7)
for src in ["fib", "loop", "recurse"]:
    good = open("%s/%s.ref.svm" % (tmp, src), "rb").read()
    for k in range(40):
        b = bytearray(good)
        m = r.randrange(4)
        if m == 0:
            i = r.randrange(len(b)); b[i] = r.randrange(256)
        elif m == 1:
            b = b[:r.randrange(len(b))]
        elif m == 2:
            b += bytes([r.randrange(256)])
        else:
            i = r.randrange(4, min(len(b), 80)); b[i] ^= 1 << r.randrange(8)
        open("%s/bad_%s_%02d.svm" % (tmp, src, k), "wb").write(bytes(b))
EOF
total=0; bad=0; failing=""
for f in "$TMP"/bad_*.svm; do
  total=$((total+1))
  agree "$(basename "$f")" run "$f" >/dev/null && agree "$(basename "$f")" dis "$f" >/dev/null \
    || { bad=$((bad+1)); failing="$failing $(basename "$f")"; }
done
[ $bad -eq 0 ] && ok "$total corrupted bytecode files: run and dis agree with the reference" \
  || no "$bad of $total corrupted files disagree:$(echo "$failing" | cut -c1-200)"

# 4. 13t exit codes.
"$B" >/dev/null 2>&1;                          [ $? -eq 2 ] && ok "no args -> 2"            || no "no-args rc"
"$B" bogus x >/dev/null 2>&1;                  [ $? -eq 2 ] && ok "unknown command -> 2"    || no "unknown-command rc"
"$B" asm x >/dev/null 2>&1;                    [ $? -eq 2 ] && ok "asm with one path -> 2"  || no "asm-arity rc"
"$B" exec "$TMP/nope.sasm" >/dev/null 2>&1;    [ $? -eq 1 ] && ok "missing input -> 1"      || no "missing-input rc"
"$B" asm examples/stackvm/testdata/fib.sasm "$TMP/no/such/dir" >/dev/null 2>&1; [ $? -eq 1 ] && ok "unwritable output -> 1" || no "unwritable-output rc"

echo
echo "STACKVM: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
