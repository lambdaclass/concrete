#!/usr/bin/env bash
# `concrete fmt` gate (ROADMAP Phase 6 #1).
#
# #1 is CLI PROMOTION: the formatter already existed as the `--fmt` flag; this
# makes it a normal daily-workflow tool (`concrete fmt`) with --check / --write /
# --stdin. The gate locks the tool's contract AND the one property that makes a
# formatter safe to run on proof-bearing code:
#
#   FORMATTING IS SEMANTICS-PRESERVING — reformatting a file must not change its
#   semantic body fingerprints (`--report fingerprints`). If it did, running
#   `concrete fmt` could silently invalidate proof links / manifests. This is the
#   "formatting must not churn semantic fingerprints" ROADMAP requirement, made
#   mechanical.
#
# Plus the tool contract: default→stdout, --check exit codes, --write idempotence,
# --stdin, and that the legacy `--fmt` flag still works (golden baselines use it).

set -uo pipefail
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# Deliberately under-formatted source (bad spacing, no indent).
SRC="$TMP/m.con"
printf 'mod m {\nfn  f( x:i32 )->i32{return  x+1;}\n}\n' > "$SRC"

echo "=== 1. default formats to stdout (and actually reformats) ==="
"$C" fmt "$SRC" > "$TMP/out1.txt" 2>/dev/null
if grep -qE '^    fn f\(x: i32\) -> i32 \{' "$TMP/out1.txt"; then
  ok "fmt <file> writes formatted source to stdout"
else
  no "fmt <file> did not produce expected formatting"; sed 's/^/      /' "$TMP/out1.txt"
fi
# stdout must not have mutated the input file
if cmp -s "$SRC" <(printf 'mod m {\nfn  f( x:i32 )->i32{return  x+1;}\n}\n'); then
  ok "fmt <file> (default) leaves the file untouched"
else
  no "fmt <file> (default) mutated the input file"
fi

echo "=== 2. --check exit codes ==="
"$C" fmt --check "$SRC" >/dev/null 2>&1 && rc=0 || rc=$?
[ "$rc" -ne 0 ] && ok "--check on unformatted file exits nonzero ($rc)" \
                || no "--check on unformatted file should exit nonzero"

echo "=== 3. --write rewrites in place, then --check passes ==="
"$C" fmt --write "$SRC" >/dev/null 2>&1
"$C" fmt --check "$SRC" >/dev/null 2>&1 && ok "after --write, --check passes (exit 0)" \
                                        || no "after --write, --check still fails"

echo "=== 4. --write is idempotent (second write changes nothing) ==="
cp "$SRC" "$TMP/after1.con"
"$C" fmt --write "$SRC" >/dev/null 2>&1
cmp -s "$SRC" "$TMP/after1.con" && ok "--write is idempotent" \
                                || no "--write is NOT idempotent (formatter not a fixpoint)"

echo "=== 5. --stdin reads stdin -> stdout, matches file-mode output ==="
printf 'mod m {\nfn  f( x:i32 )->i32{return  x+1;}\n}\n' | "$C" fmt --stdin > "$TMP/stdin_out.txt" 2>/dev/null
# file-mode stdout of the same unstable input
printf 'mod m {\nfn  f( x:i32 )->i32{return  x+1;}\n}\n' > "$TMP/u.con"
"$C" fmt "$TMP/u.con" > "$TMP/file_out.txt" 2>/dev/null
cmp -s "$TMP/stdin_out.txt" "$TMP/file_out.txt" && ok "--stdin output matches file-mode output" \
                                                || no "--stdin and file-mode disagree"

echo "=== 6. SEMANTICS-PRESERVING: formatting does not change fingerprints ==="
# Use a real proof-bearing example: compare semantic fingerprints of the original
# against a freshly-formatted copy. They must be identical.
EX="examples/elf_header/src/main.con"
if [ -f "$EX" ]; then
  "$C" "$EX" --report fingerprints 2>/dev/null > "$TMP/fp_orig.txt"
  "$C" fmt "$EX" > "$TMP/fmted.con" 2>/dev/null
  "$C" "$TMP/fmted.con" --report fingerprints 2>/dev/null > "$TMP/fp_fmt.txt"
  if [ -s "$TMP/fp_orig.txt" ] && cmp -s "$TMP/fp_orig.txt" "$TMP/fp_fmt.txt"; then
    ok "fingerprints identical before/after formatting ($EX)"
  else
    no "formatting CHANGED semantic fingerprints ($EX)"; diff "$TMP/fp_orig.txt" "$TMP/fp_fmt.txt" | head -6 | sed 's/^/      /'
  fi
  # And reformatting an already-formatted example is a no-op under --check.
  "$C" fmt --check "$TMP/fmted.con" >/dev/null 2>&1 && ok "formatted example is a --check fixpoint" \
                                                     || no "formatted example fails --check (not a fixpoint)"
else
  no "fingerprint-stability probe example missing: $EX"
fi

echo "=== 6b. every declaration-level form survives formatting ==="
# ONE EXAMPLE WAS NOT ENOUGH. Section 6 formats elf_header, which happens to declare no
# capability parameters and no Copy enum, so it stayed green while `fmt` dropped `enum Copy`,
# a function's or impl's `cap C`, and `-> ()` on function-pointer types it printed unparseably.
# It went red only when R-0484 made extern effect declarations mandatory and `fmt` dropped
# those too. A sweep of every single-file program found 56 such programs on 2026-10-03. This
# fixture holds one instance of each lost form; a dropped form makes the formatted copy
# fail to compile, which empties its fingerprints and fails the comparison.
FX="tests/regressions/fmt_roundtrip/declarations.con"
if [ -f "$FX" ]; then
  "$C" "$FX" --report fingerprints 2>/dev/null > "$TMP/fx_orig.txt"
  "$C" fmt "$FX" > "$TMP/fx_fmted.con" 2>/dev/null
  "$C" "$TMP/fx_fmted.con" --report fingerprints 2>/dev/null > "$TMP/fx_fmt.txt"
  if grep -q Fingerprints "$TMP/fx_orig.txt" && cmp -s "$TMP/fx_orig.txt" "$TMP/fx_fmt.txt"; then
    ok "fingerprints identical before/after formatting ($FX)"
  else
    no "formatting CHANGED the meaning of $FX"
    "$C" "$TMP/fx_fmted.con" --report fingerprints 2>&1 | grep -E 'error' | head -3 | sed 's/^/      /'
  fi
else
  no "declaration round-trip fixture missing: $FX"
fi

echo "=== 7. legacy --fmt flag still works (golden baselines depend on it) ==="
"$C" "$TMP/u.con" --fmt > "$TMP/legacy.txt" 2>/dev/null
cmp -s "$TMP/legacy.txt" "$TMP/file_out.txt" && ok "legacy --fmt output matches 'concrete fmt'" \
                                             || no "legacy --fmt diverged from 'concrete fmt'"

echo "=== 8. float literals round-trip exactly (bug 135) ==="
# The formatter printed floats with Lean's toString (six decimals): `0.0000000001` became
# `0.000000`, silently changing the program. Section 6 could not see it: the legacy
# fingerprint renders floats with the same lossy toString, so both versions fingerprinted alike.
# The judge here is independent of every compiler printer: the LLVM emitter writes each f64
# constant as its exact IEEE bits (bug 109), and Python reads the original lexeme. For every
# float lexeme in the corpus plus a fixed pseudo-random sample across the exponent range
# (subnormals and the largest finite included), the constants compiled from the source AND
# from its formatted copy must both equal Python's reading of the lexeme.
RT="scripts/tests/lib/float_roundtrip.py"
grep -rhoE '(^|[^0-9A-Za-z_.])[0-9]+[.][0-9]+' --include='*.con' tests examples std \
  | grep -oE '[0-9]+[.][0-9]+' | sort -u > "$TMP/lexemes.txt" || true
nlex="$(wc -l < "$TMP/lexemes.txt" | tr -d ' ')"
if [ "$nlex" -lt 10 ]; then
  no "corpus float-lexeme scan found only $nlex literals: the scan is broken, the leg would be vacuous"
elif ! python3 "$RT" gen "$TMP/lexemes.txt" "$TMP/floats.con" "$TMP/want.txt"; then
  no "float round-trip generator failed"
else
  "$C" fmt "$TMP/floats.con" > "$TMP/floats_fmt.con" 2>/dev/null || true
  "$C" "$TMP/floats.con" --emit-llvm > "$TMP/floats.ll" 2>/dev/null || true
  "$C" "$TMP/floats_fmt.con" --emit-llvm > "$TMP/floats_fmt.ll" 2>/dev/null || true
  python3 "$RT" bits "$TMP/floats.ll" > "$TMP/got.txt"
  python3 "$RT" bits "$TMP/floats_fmt.ll" > "$TMP/got_fmt.txt"
  nwant="$(wc -l < "$TMP/want.txt" | tr -d ' ')"
  if [ "$nwant" -le "$nlex" ]; then
    no "round-trip program holds $nwant literals, not the corpus's $nlex plus the sample"
  elif ! cmp -s "$TMP/want.txt" "$TMP/got.txt"; then
    no "the compiler reads $(diff "$TMP/want.txt" "$TMP/got.txt" | grep -c '^>') of $nwant float lexemes differently from Python (lexer, not formatter)"
  elif cmp -s "$TMP/want.txt" "$TMP/got_fmt.txt"; then
    ok "all $nwant float literals ($nlex from the corpus) keep their exact value through fmt"
  else
    no "fmt changed the value of $(diff "$TMP/want.txt" "$TMP/got_fmt.txt" | grep -c '^>') of $nwant float literals"
    paste "$TMP/want.txt" "$TMP/got_fmt.txt" | awk '$1 != $2' | head -3 | sed 's/^/      /'
  fi
fi
# The bug's own literals, as text: the shortest round-trip spelling of a literal written that
# way is the literal itself, so formatting keeps every lexeme of the regression file.
FL="tests/regressions/fmt_roundtrip/floats.con"
"$C" fmt "$FL" > "$TMP/fl_fmt.con" 2>/dev/null || true
# (code lines only: the comment above the code quotes the old lossy output, and fmt drops comments)
lx_src="$(grep -vE '^[[:space:]]*//' "$FL" | grep -oE '[0-9]+[.][0-9]+' | sort)"
lx_fmt="$(grep -oE '[0-9]+[.][0-9]+' "$TMP/fl_fmt.con" | sort)"
if [ -n "$lx_fmt" ] && [ "$lx_src" = "$lx_fmt" ]; then
  ok "fmt keeps every float lexeme of $FL as written (0.0000000001 stays 0.0000000001)"
else
  no "fmt rewrote float lexemes of $FL:"; diff <(echo "$lx_src") <(echo "$lx_fmt") | head -6 | sed 's/^/      /'
fi

echo ""
echo "CONCRETE-FMT: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
