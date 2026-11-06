#!/usr/bin/env bash
# Phase 7 workload 10 gate: stackvm (assembler, bytecode codec and verifier, interpreter,
# disassembler) must agree byte for byte with an independent Python implementation of the same
# specifications (scripts/tests/lib/stackvm_ref.py) on every output a run has: the assembled
# file, the listing, the program's output (stdout), the diagnostic (stderr) and the exit code,
# each compared on its own. Inputs: the example's test programs, seeded generated programs
# (scripts/tests/lib/stackvm_gen.py, valid ones and one line-level mutation of each), and seeded
# corruptions of assembled files for the loader. Every input set is counted before it is used, so
# a generator that wrote nothing fails the gate instead of passing it vacuously. Plus the 13t exit
# codes.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
REF="$ROOT_DIR/scripts/tests/lib/stackvm_ref.py"
GEN="$ROOT_DIR/scripts/tests/lib/stackvm_gen.py"
NGEN=120        # generated programs, and as many mutations
NCORRUPT=40     # corruptions per assembled test program
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

( cd examples/stackvm && "$C" build ) >/dev/null 2>&1 \
  && ok "stackvm builds" || { no "stackvm build failed"; echo "STACKVM: PASS=$PASS FAIL=$FAIL"; exit 1; }
B="$ROOT_DIR/examples/stackvm/stackvm"

# One observation of a command: <out>.o (stdout), <out>.e (stderr), <out>.rc (exit code).
observe(){ local out="$1"; shift; "$@" > "$out.o" 2> "$out.e"; echo "$?" > "$out.rc"; }

# same <a> <b>: two observations agree stream by stream (stdout, stderr and exit code separately,
# so a diagnostic printed to the wrong stream is a disagreement).
same(){
  local s bad=0
  for s in o e rc; do
    if ! cmp -s "$1.$s" "$2.$s"; then
      bad=1
      echo "      $(case $s in o) echo stdout;; e) echo stderr;; rc) echo "exit code";; esac) differs:"
      diff "$2.$s" "$1.$s" | head -4 | sed 's/^/        /'
    fi
  done
  return $bad
}

# agree <label> <args...>: the binary and the reference give the same observation.
agree(){
  local label="$1"; shift
  observe "$TMP/ours" "$B" "$@"
  observe "$TMP/want" python3 "$REF" "$@"
  same "$TMP/ours" "$TMP/want" && return 0
  echo "    $label ($*) disagrees"
  return 1
}

# full <file> <tag>: exec and check, asm (its observation and the bytes written), and dis and run
# of the reference's bytes when it wrote any. Prints nothing on agreement.
full(){
  local f="$1" tag="$2" bad=0
  agree "$tag exec" exec "$f" || bad=1
  agree "$tag check" check "$f" || bad=1
  rm -f "$TMP/$tag.ours.svm" "$TMP/$tag.ref.svm"
  observe "$TMP/ours" "$B" asm "$f" "$TMP/$tag.ours.svm"
  observe "$TMP/want" python3 "$REF" asm "$f" "$TMP/$tag.ref.svm"
  same "$TMP/ours" "$TMP/want" || { echo "    $tag asm disagrees"; bad=1; }
  if [ -f "$TMP/$tag.ref.svm" ] || [ -f "$TMP/$tag.ours.svm" ]; then
    cmp -s "$TMP/$tag.ref.svm" "$TMP/$tag.ours.svm" || { echo "    $tag: assembled bytes differ"; bad=1; }
    if [ -f "$TMP/$tag.ref.svm" ]; then
      agree "$tag dis" dis "$TMP/$tag.ref.svm" || bad=1
      agree "$tag run" run "$TMP/$tag.ref.svm" || bad=1
    fi
  fi
  return $bad
}

# 1. The test programs: every command, plus the assembled bytes.
ntest=0
for f in examples/stackvm/testdata/*.sasm; do
  [ -f "$f" ] || continue
  ntest=$((ntest+1))
  n=$(basename "$f" .sasm)
  full "$f" "$n" && ok "testdata/$n.sasm: exec, check, asm bytes, dis and run agree with the reference" \
    || no "testdata/$n.sasm disagrees with the reference"
done
[ "$ntest" -ge 6 ] && ok "$ntest test programs checked" || no "only $ntest test programs found (want at least 6)"

# 1b. Token edges: every operand form with digit strings of any length, signs, leading zeros,
# the i64 bounds and non-ASCII bytes (which a diagnostic quotes byte for byte).
mkdir -p "$TMP/edge"
if ! python3 - "$TMP/edge" <<'EOF'
import sys
out = sys.argv[1]
toks = [b"0" * 5000, b"0" * 4999 + b"7", b"1" + b"0" * 30, b"-0005", b"+5", b"-", b"",
        b"9223372036854775807", b"-9223372036854775807", b"-9223372036854775808",
        b"9223372036854775808", b"18446744073709551616", b"255", b"256", b"0255",
        b"\xff", b"na\xefve", b"x" * 33, b"_" * 32, b"9x"]
forms = [b"push %s\n    print", b"load %s", b"store %s", b"jmp %s", b"call %s"]
k = 0
for t in toks:
    for f in forms:
        prog = b"fn main 0 3\n    " + (f % t) + b"\n    push 0\n    ret\nend\n"
        open("%s/edge_%03d.sasm" % (out, k), "wb").write(prog)
        k += 1
    open("%s/edge_%03d.sasm" % (out, k), "wb").write(b"fn " + t + b" 0 0\n    push 0\n    ret\nend\n")
    k += 1
EOF
then
  no "the token-edge generator failed"
fi
total=0; bad=0; failing=""
for f in "$TMP"/edge/edge_*.sasm; do
  [ -f "$f" ] || continue
  total=$((total+1))
  full "$f" "$(basename "$f" .sasm)" > "$TMP/full.log" || { bad=$((bad+1)); failing="$failing $(basename "$f")"; sed -n 1,6p "$TMP/full.log"; }
done
if [ "$total" -ne 120 ]; then
  no "$total token-edge programs, want 120"
elif [ $bad -eq 0 ]; then
  ok "$total token-edge programs: exec, check, asm bytes, dis and run agree with the reference"
else
  no "$bad of $total token-edge programs disagree:$(echo "$failing" | cut -c1-200)"
fi

# 2. Generated programs (valid, and one mutation of each), with every command and the bytes.
mkdir -p "$TMP/gen"
if ! python3 "$GEN" "$TMP/gen" 20261024 "$NGEN"; then
  no "the program generator failed"
fi
nasm=0
for kind in gen mut; do
  total=0; bad=0; failing=""
  for f in "$TMP"/gen/${kind}_*.sasm; do
    [ -f "$f" ] || continue
    total=$((total+1))
    if ! full "$f" "$(basename "$f" .sasm)" > "$TMP/full.log"; then
      bad=$((bad+1)); failing="$failing $(basename "$f")"; sed -n 1,6p "$TMP/full.log"
    fi
    [ "$kind" = gen ] && [ -f "$TMP/$(basename "$f" .sasm).ref.svm" ] && nasm=$((nasm+1))
  done
  if [ "$total" -ne "$NGEN" ]; then
    no "$total generated $kind programs, want $NGEN"
  elif [ $bad -eq 0 ]; then
    ok "$total generated $kind programs: exec, check, asm bytes, dis and run agree with the reference"
  else
    no "$bad of $total generated $kind programs disagree:$(echo "$failing" | cut -c1-200)"
  fi
done
# Every valid program must assemble (else the bytes, dis and run legs compared nothing for it).
[ "$nasm" -eq "$NGEN" ] && ok "all $nasm valid generated programs assembled (bytes, dis and run compared)" \
  || no "$nasm of $NGEN valid generated programs assembled"

# 3. Loader: seeded corruptions of assembled files (bytes flipped, truncated, extended).
if ! python3 - "$TMP" "$NCORRUPT" <<'EOF'
import random, sys
tmp, n = sys.argv[1], int(sys.argv[2])
r = random.Random(7)
for src in ["fib", "loop", "recurse"]:
    good = open("%s/%s.ref.svm" % (tmp, src), "rb").read()
    for k in range(n):
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
then
  no "the corruption generator failed"
fi
total=0; bad=0; failing=""
for f in "$TMP"/bad_*.svm; do
  [ -f "$f" ] || continue
  total=$((total+1))
  agree "$(basename "$f")" run "$f" >/dev/null && agree "$(basename "$f")" dis "$f" >/dev/null \
    || { bad=$((bad+1)); failing="$failing $(basename "$f")"; }
done
if [ "$total" -ne $((3 * NCORRUPT)) ]; then
  no "$total corrupted bytecode files, want $((3 * NCORRUPT))"
elif [ $bad -eq 0 ]; then
  ok "$total corrupted bytecode files: run and dis agree with the reference"
else
  no "$bad of $total corrupted files disagree:$(echo "$failing" | cut -c1-200)"
fi

# 4. 13t exit codes.
"$B" >/dev/null 2>&1;                          [ $? -eq 2 ] && ok "no args -> 2"            || no "no-args rc"
"$B" bogus x >/dev/null 2>&1;                  [ $? -eq 2 ] && ok "unknown command -> 2"    || no "unknown-command rc"
"$B" asm x >/dev/null 2>&1;                    [ $? -eq 2 ] && ok "asm with one path -> 2"  || no "asm-arity rc"
"$B" exec "$TMP/nope.sasm" >/dev/null 2>&1;    [ $? -eq 1 ] && ok "missing input -> 1"      || no "missing-input rc"
"$B" asm examples/stackvm/testdata/fib.sasm "$TMP/no/such/dir" >/dev/null 2>&1; [ $? -eq 1 ] && ok "unwritable output -> 1" || no "unwritable-output rc"

echo
echo "STACKVM: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
