#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
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

# 2. no blank facts (7 non-empty fields per row)
bad=$(awk -F'\t' 'NF!=7 || $3=="" || $4=="" || $5=="" || $6=="" || $7==""' "$TMP/committed.tsv" | head -3)
[ -z "$bad" ] && ok "no blank facts (absence is explicit no/none/infallible)" || no "blank facts: $bad"

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
echo "STDLIB-MANIFEST: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
