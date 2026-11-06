#!/usr/bin/env bash
# Is the checked-in classification table still what the live environment says?
#
# SEPARATE FROM check_dependency_edges.sh because it is SLOW: verifying freshness means running
# the generator, which classifies every linked theorem and digests each proof term. A per-commit
# gate that costs minutes is one people start skipping, and a skipped gate protects nothing.
#
# What it protects: `Concrete/Proof/ClassificationTable.lean` is generated data the compiler
# TRUSTS to type dependency edges. Nothing else in R-0004 rests on a value nobody re-derives,
# and this must not be the exception. A stale row types edges from a theorem that has since been
# reproved, restated, or replaced by a different theorem of the same name — and every downstream
# check passes, because the row is structurally valid. Only re-derivation catches it.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "$ROOT_DIR/scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# === THE CHECKED-IN TABLE MUST NOT BE STALE (slice 6, blocker b) =============================
# `ClassificationTable.lean` is generated data the compiler trusts. Nothing else in R-0004 rests
# on a value nobody re-derives, and this must not be the exception: a stale table types edges
# from a theorem that has since been reproved, restated, or replaced by a different theorem of
# the same name — and every downstream check would pass, because the row is structurally valid.
#
# So the table is re-derived from the LIVE environment and compared. A stale table is a gate
# failure, not a silent wrong answer.
# THE NAME INVENTORY IS REDISCOVERED HERE, not taken from the generator. `classifications.lean`
# carries a GENERATED `sourceLinkedThms` list, so running it proves the table matches that list —
# not that the list matches the source. A new `#[proof_by]` annotation omitted from the list would
# leave the gate green while its theorem was never classified, which is exactly the gap that made
# `main.verify_message` proved-with-a-refusing-root.
#
# So: re-extract from source and compare against what the generator will actually ask about.
echo "=== theorem-name inventory is exhaustive ==="
grep -rhoE '#\[(proof_by|ensures_proof)\(([A-Za-z0-9_.]+)\)\]' examples/ \
  | sed -E 's/.*\(([^)]*)\)\]/\1/' | LC_ALL=C sort -u > "$TMP/src_names.txt"
grep -oE '`[A-Za-z0-9_.]+' scripts/gen/classifications.lean | sed 's/^`//' | LC_ALL=C sort -u > "$TMP/gen_names.txt"
# Collation is pinned to C for this comparison. `comm` assumes its inputs are sorted the way it
# compares them, and a UTF-8 collator ignores punctuation at the primary level — so an entry like
# `__concrete_check_oom` sorts differently under LANG=en_US.UTF-8 than under C, and the set
# difference comes out WRONG rather than merely reordered. That made one gate's verdict depend on
# the developer's locale; pinned here so it cannot.
MISSING="$(LC_ALL=C comm -23 "$TMP/src_names.txt" "$TMP/gen_names.txt" || true)"
if [ -z "$MISSING" ]; then
  ok "every source-linked proof name is in the generator's inventory ($(wc -l < "$TMP/src_names.txt") names)"
else
  no "source-linked proof(s) missing from the generator inventory — run scripts/gen/refresh_classifications.sh:"
  printf '%s\n' "$MISSING" | head -5 | sed 's/^/      /'
fi

echo "=== classification table freshness ==="
FRESH="$TMP/fresh_rows.txt"
if lake env lean scripts/gen/classifications.lean > "$FRESH" 2>&1; then
  # Compare the (theorem, edge, digest) triples, order-insensitively: the generator emits in
  # request order, and the checked-in file may have been reordered by hand without meaning
  # anything. Content is what must agree.
  # LIMIT, recorded because this comparison is textual. It matches one-line rows, so a harmless
  # reformatting would read as staleness and a MULTILINE row would escape the regex entirely.
  # Adequate while the generator emits one line per row and nothing else writes this file;
  # before receipts become authoritative it should compare a canonical machine format or parsed
  # Lean values, not source text. Recorded rather than left as an assumption about formatting.
  #
  # WHOLE ROWS, not a 3-tuple prefix. The row grew to carry table identities, digests and
  # quantification, and a regex matching only the first three fields would have compared the
  # theorem/edge/digest while ignoring exactly the dependency evidence this slice added — a
  # freshness check that stops looking where the new information is.
  CHECKED="$(grep -oE '^  \(".*\),?$' Concrete/Proof/ClassificationTable.lean | sed 's/,$//' | LC_ALL=C sort || true)"
  LIVE="$(grep -oE '^  \(".*\),?$' "$FRESH" | sed 's/,$//' | LC_ALL=C sort || true)"
  # BOTH SIDES MUST BE NON-EMPTY. Guarding only `LIVE` left the other half of the comparison
  # unguarded, and it fired for real on 2026-08-16: four NUL bytes landed inside string literals in
  # the checked-in file, `grep` classified it as a binary file, `-o` returned nothing, and the gate
  # reported "STALE classification table" — sending the reader to regenerate a table that was
  # perfectly current. Lean had compiled it without complaint, because a NUL inside a string literal
  # is legal. An empty extraction is a FAILURE TO READ, which is a different fact from staleness and
  # has a different repair.
  if [ -z "$CHECKED" ]; then
    no "extracted NO rows from Concrete/Proof/ClassificationTable.lean — the file could not be read as text (a NUL byte makes grep treat it as binary). This is not staleness; check the file's bytes."
  elif [ -z "$LIVE" ]; then
    no "the generator produced no rows — cannot establish table freshness, which is not the same as the table being fresh"
  elif [ "$CHECKED" = "$LIVE" ]; then
    ok "the checked-in classification table matches a fresh derivation ($(printf '%s\n' "$LIVE" | wc -l) rows)"
  else
    no "STALE classification table — regenerate with scripts/gen/refresh_classifications.sh. A changed theorem artifact or dependency identity must move the checked-in row."
    diff <(printf '%s\n' "$CHECKED") <(printf '%s\n' "$LIVE") | head -6 | sed 's/^/      /'
  fi
else
  no "the classification generator failed to run — table freshness is unverified"
fi

# The command named by the failure above must be the producer that actually writes every generated
# block. It once refreshed only `sourceLinkedThms`, printed success, and left the trusted table
# untouched. `--check` derives both files through the production refresh path and compares their
# complete bytes, including external and external-scoped rows the row-only check above does not see.
echo "=== the advertised refresh command owns every generated block ==="
REFRESH_LOG="$TMP/refresh-check.log"
if bash scripts/gen/refresh_classifications.sh --check > "$REFRESH_LOG" 2>&1; then
  ok "refresh_classifications.sh --check reproduces the theorem inventory and all three table blocks"
else
  no "refresh_classifications.sh --check found generated output the checked-in files do not carry"
  sed -n '1,12p' "$REFRESH_LOG" | sed 's/^/      /'
fi


echo ""
echo "CLASSIFICATION-FRESHNESS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
