#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
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

echo "=== 7. legacy --fmt flag still works (golden baselines depend on it) ==="
"$C" "$TMP/u.con" --fmt > "$TMP/legacy.txt" 2>/dev/null
cmp -s "$TMP/legacy.txt" "$TMP/file_out.txt" && ok "legacy --fmt output matches 'concrete fmt'" \
                                             || no "legacy --fmt diverged from 'concrete fmt'"

echo ""
echo "CONCRETE-FMT: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
