#!/usr/bin/env bash
# Phase 7 item 2a gate: the five-fact stdlib surface manifest.
#
# Every public stdlib item must have a complete row in
# docs/stdlib/STDLIB_SURFACE_MANIFEST.tsv (allocates/ownership/fails/capability/
# proof-class — absence is explicit, never blank), and every row must AGREE with
# the item's actual signature (derived facts): a row claiming `allocates: no`
# against a `with(Alloc)` signature fails — the "stale hand-written string
# disagrees with the compiler fact" negative.
# Also carries the item-2b check: a public free-function that duplicates a
# method name in the same module is flagged (method style is canonical).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
MANIFEST="docs/stdlib/STDLIB_SURFACE_MANIFEST.tsv"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT

python3 scripts/tests/lib/stdlib_manifest.py > "$TMP/derived.tsv" || { echo "error: generator failed"; exit 2; }
grep -v '^#' "$MANIFEST" > "$TMP/committed.tsv"

# 1. completeness + agreement in one diff (order-stable: generator sorts by file)
if diff -u "$TMP/committed.tsv" "$TMP/derived.tsv" > "$TMP/drift.txt"; then
  ok "manifest complete + every row agrees with its signature ($(wc -l < "$TMP/derived.tsv" | tr -d ' ') items)"
else
  no "manifest drift (missing rows / stale facts):"
  head -12 "$TMP/drift.txt" | sed 's/^/       /'
  echo "       regenerate: python3 scripts/tests/lib/stdlib_manifest.py (with header) > $MANIFEST"
fi

# 2. no blank facts (9 non-empty fields per row; 22a added evidence+signature)
bad=$(awk -F'\t' 'NF!=9 || $3=="" || $4=="" || $5=="" || $6=="" || $7=="" || $8=="" || $9==""' "$TMP/committed.tsv" | head -3)
[ -z "$bad" ] && ok "no blank facts (absence is explicit no/none/infallible)" || no "blank facts: $bad"

# 2a. item 22a EVIDENCE marking is exact: `proved` rows are precisely the
#     public fns carrying kernel proof links today — no spillover, no drift.
#     A new proof link updates BOTH this pin and the TSV (that is the gate).
want_proved="numeric.try_from_u32
numeric.try_from_u64
numeric.try_new
numeric.try_new
numeric.try_new
option.map
option.unwrap_or
result.map
result.map_err"
got_proved=$(awk -F'\t' '$8=="proved"{print $1"."$2}' "$TMP/committed.tsv" | sort)
[ "$got_proved" = "$want_proved" ] \
  && ok "evidence: exactly the 9 kernel-linked public APIs marked proved" \
  || no "evidence drift: proved set is [$(echo $got_proved | tr '\n' ' ')]"

# 2b. proved is never decorative: each proved row's source fn also carries a
#     #[proof_fingerprint] (the stale-detection half of the link).
pf_fns=$(grep -B1 -h 'pub fn' std/src/*.con | grep -c 'proof_fingerprint' || true)
n_proved=$(awk -F'\t' '$8=="proved"' "$TMP/committed.tsv" | wc -l | tr -d ' ')
fp_count=$(grep -h 'proof_fingerprint' std/src/*.con | wc -l | tr -d ' ')
[ "$fp_count" -ge "$n_proved" ] \
  && ok "every proved row is fingerprint-backed ($n_proved proved, $fp_count fingerprints in std)" \
  || no "proved rows without fingerprints ($n_proved proved > $fp_count fingerprints)"

# 3. item 2b: free-function (constructs) duplicating a method name in its module
ALLOW="new|with_capacity|from|default|empty"   # genuine no-receiver constructors
dups=$(awk -F'\t' '$4=="constructs"{print $1"\t"$2}' "$TMP/derived.tsv" | while IFS=$'\t' read -r m f; do
  if awk -F'\t' -v m="$m" -v f="$f" '$1==m && $2==f && $4!="constructs"' "$TMP/derived.tsv" | grep -q .; then
    echo "$m.$f" | grep -vE "\.($ALLOW)$" || true
  fi
done)
[ -z "$dups" ] && ok "no free-function duplicates a same-module method (2b method-canonical)" \
  || no "free-fn/method duplicates (extend allowlist only for genuine no-receiver helpers): $dups"

# 4. DOMAIN-CAPABILITY rule (the fs/env/time leak class, audit 2026-07-14):
# public items in capability-owning modules must carry their domain capability —
# Unsafe alone is NOT filesystem/env/clock authority. Pure helpers (no caps at
# all) are exempt; anything HOSTED (any with(...) clause) must include the domain.
declare -A DOMAIN=( [fs]=File [env]=Env [time]=Time [args]=Env [net]=Network [process]=Process )
for mod in "${!DOMAIN[@]}"; do
  want="${DOMAIN[$mod]}"
  leak=$(awk -F'	' -v m="$mod" -v c="$want" '$1==m && $6!="none" && $6 !~ c {print $2" ("$6")"}' "$TMP/derived.tsv" | head -5)
  [ -z "$leak" ] && ok "std.$mod hosted items all carry $want"     || no "std.$mod hosted items MISSING $want (Unsafe is not domain authority): $leak"
done
# io: TextFile + writer_from_file must carry File; Writer methods stay cap-free.
ioleak=$(awk -F'	' '$1=="io" && $6 ~ /Unsafe/ && $6 !~ /File/ {print $2" ("$6")"}' "$TMP/derived.tsv" | grep -vE "^fixed_(writer|reader)" | head -5)
[ -z "$ioleak" ] && ok "std.io Unsafe items carry File where they touch files (fixed_writer/fixed_reader exempt: raw memory, not fs)"   || no "std.io items with Unsafe but no File: $ioleak"

echo
# 0a: parser self-test — every previously-misparsed shape (fn-pointer
# params, nested generics, [trusted] extern, enclosing trusted impl,
# ')' in comments/strings, cap-HOF, consuming receiver) round-trips to a
# golden. A single-regex regression corrupts these rows and fails here.
STF=scripts/tests/fixtures/manifest_selftest.con
STF_GOLD=scripts/tests/fixtures/manifest_selftest.expected.tsv
if diff -q <(python3 scripts/tests/lib/stdlib_manifest.py "$STF") "$STF_GOLD" >/dev/null; then
  ok "parser self-test: all previously-misparsed shapes round-trip"
else
  no "parser self-test drift:"; diff <(python3 scripts/tests/lib/stdlib_manifest.py "$STF") "$STF_GOLD" | head -8 | sed 's/^/       /'
fi


echo "STDLIB-MANIFEST: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
