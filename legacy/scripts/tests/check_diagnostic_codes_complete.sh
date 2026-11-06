#!/usr/bin/env bash
# Diagnostic-code registry completeness gate (ROADMAP 13m, first slice).
#
# `--report diagnostic-codes` (Concrete/Report/Report.lean) claims to be the ledger
# of every diagnostic the compiler can emit. It silently drifted 8 codes
# behind Check.lean (E0286–E0293 were shipped but unlisted, found 2026-07-06).
# This gate extracts every `=> "E NNNN"` code from the CheckError.code and
# CoreCheckError-style mappings and requires each to appear as an `entry` in
# Report.lean — a new diagnostic cannot land without its ledger row.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== every emittable E-code has a --report diagnostic-codes entry ==="
# The emitters are every Lean module of the compiler, not a list: a diagnostic code is a string
# literal "E<4 digits>" (in a `=> "E..."` mapping, a `code := "E..."` field, `mkDiag "E..."`, a
# `loadError`, a post-mono verifier pair, ...), and every one outside the ledger itself
# (Concrete/Report/Report.lean) must have a ledger row. A fixed module list let Policy, ProofCore,
# Mono and ReportObligations emit eleven codes with no row. Comment lines are skipped, since a
# comment may name a code that is retired.
# emitted_codes ROOT: prints "<code> <file>" for every code literal under ROOT/Concrete
emitted_codes() {
  python3 - "$1" <<'PY'
import os, re, sys
root = sys.argv[1]
out = set()
for d, _, fs in os.walk(os.path.join(root, "Concrete")):
    for f in fs:
        if not f.endswith(".lean"): continue
        rel = os.path.relpath(os.path.join(d, f), root)
        if rel == "Concrete/Report/Report.lean": continue
        t = open(os.path.join(d, f), encoding="utf-8").read()
        # drop comments: nested block comments /- -/ (doc comments too) and line comments, leaving
        # string literals alone, so a code named only in a comment is not read as emitted
        res, k, depth, n = [], 0, 0, len(t)
        while k < n:
            if depth == 0 and t[k] == '"':
                e = k + 1
                while e < n and t[e] != '"': e += 2 if t[e] == "\\" else 1
                res.append(t[k:e + 1]); k = e + 1
            elif t.startswith("/-", k): depth += 1; k += 2
            elif depth and t.startswith("-/", k): depth -= 1; k += 2
            elif depth: k += 1
            elif t.startswith("--", k):
                while k < n and t[k] != "\n": k += 1
            else: res.append(t[k]); k += 1
        for m in re.finditer(r'"(E[0-9]{4})"', "".join(res)): out.add((m.group(1), rel))
for c, rel in sorted(out): print(c, rel)
PY
}
# check_root ROOT: 0 when every emitted code under ROOT has a row in ROOT's ledger; prints each gap
check_root() {
  local root="$1" bad=0 c f n
  n=0
  while read -r c f; do
    n=$((n+1))
    if ! grep -qE "^[[:space:]]*entryR? \"$c\"" "$root/Concrete/Report/Report.lean"; then
      echo "    $c (emitted by $f) has no entry in Report.lean diagnostic-codes"; bad=1
    fi
  done < <(emitted_codes "$root")
  # fail closed: a scan that finds almost nothing has drifted, not passed
  [ "$n" -ge 200 ] || { echo "    the scan found only $n code sites (pattern drift?)"; bad=1; }
  return $bad
}
if check_root "$ROOT_DIR"; then
  ok "all emittable codes ($(emitted_codes "$ROOT_DIR" | awk '{print $1}' | sort -u | wc -l) codes in $(emitted_codes "$ROOT_DIR" | awk '{print $2}' | sort -u | wc -l) modules) are in the report ledger"
else
  no "an emitted code has no ledger row (see above)"
fi

echo ""
echo "=== N. negative controls ==="
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/n1"; cp -R Concrete "$TMP/n1/Concrete"
# N1: a code emitted from a module no list names (a new directory) is caught
mkdir -p "$TMP/n1/Concrete/Planted"
printf 'def plantedCode : String := "E0999"\n' > "$TMP/n1/Concrete/Planted/Emitter.lean"
out="$(check_root "$TMP/n1")" && no "N1: a code emitted from an unlisted module went unnoticed" \
  || { grep -qF 'E0999 (emitted by Concrete/Planted/Emitter.lean)' <<<"$out" \
       && ok "N1: a code emitted from an unlisted module is refused" || no "N1: refused for another reason: $out"; }
# N2: deleting one ledger row is caught
mkdir -p "$TMP/n2"; cp -R Concrete "$TMP/n2/Concrete"
sed -i '/entry "E0901"/d' "$TMP/n2/Concrete/Report/Report.lean"
out="$(check_root "$TMP/n2")" && no "N2: a removed ledger row went unnoticed" \
  || { grep -qF 'E0901 (emitted by Concrete/Report/ReportObligations.lean)' <<<"$out" \
       && ok "N2: an emitted code whose ledger row is removed is refused" || no "N2: refused for another reason: $out"; }
# N3: a commented-out code does not count as emitted
mkdir -p "$TMP/n3"; cp -R Concrete "$TMP/n3/Concrete"
printf -- '-- "E0998" was retired\ndef x : Nat := 0\n' > "$TMP/n3/Concrete/Planted.lean"
check_root "$TMP/n3" > /dev/null && ok "N3: a code named only in a comment is not read as emitted" \
  || no "N3: a comment line was read as an emitted code"
# N4: a code inside a block or doc comment is not emitted either
mkdir -p "$TMP/n4"; cp -R Concrete "$TMP/n4/Concrete"
printf '/-- retired: "E0997" -/\ndef y : Nat := 0\n/- "E0996"\n /- nested -/ "E0995" -/\n' > "$TMP/n4/Concrete/Planted.lean"
check_root "$TMP/n4" > /dev/null && ok "N4: a code named only in a block or doc comment is not read as emitted" \
  || no "N4: a block comment was read as an emitted code"
# N5: a commented-out ledger row does not count as the code's row
mkdir -p "$TMP/n5"; cp -R Concrete "$TMP/n5/Concrete"
sed -i 's/^\([[:space:]]*\)entry "E0901"/\1-- entry "E0901"/' "$TMP/n5/Concrete/Report/Report.lean"
grep -q -- '-- entry "E0901"' "$TMP/n5/Concrete/Report/Report.lean" || no "N5: control setup did not comment the row out"
out="$(check_root "$TMP/n5")" && no "N5: a commented-out ledger row still satisfied its code" \
  || { grep -qF 'E0901 (emitted by' <<<"$out" && ok "N5: a commented-out ledger row is not a row" || no "N5: refused for another reason: $out"; }

echo ""
echo "DIAGNOSTIC-CODES-COMPLETE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
