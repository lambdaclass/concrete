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

echo
echo "STDLIB-MANIFEST: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
