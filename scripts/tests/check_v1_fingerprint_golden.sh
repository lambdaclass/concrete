#!/usr/bin/env bash
# R-0004 V2 probe, step 1 — V1 body fingerprints are FROZEN.
#
# V2 is built ALONGSIDE V1, never by modifying it: the stored
# `#[proof_fingerprint]` corpus was computed under V1, so a change to
# `bodyFingerprint` reports the corpus stale and invites the backfill that is
# forbidden until kernel replay issues a trustworthy receipt. "V1 untouched"
# must therefore be CHECKED, not intended.
#
# Two DIFFERENT populations, counted separately because they answer different
# questions and conflating them was a real error here:
#   * 77 EXTRACTED functions across eight examples — what the golden hashes;
#   * 43 STORED #[proof_fingerprint] values — the migration input (44 before one wrong link was deleted).
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
[ -x ".lake/build/bin/concrete" ] || { echo "error: build first" >&2; exit 2; }
CC=".lake/build/bin/concrete"
GOLDEN="tests/golden/v1_body_fingerprints.txt"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
EXAMPLES="constant_time_tag crypto_verify elf_header fixed_capacity hmac_sha256 loop_invariant parse_validate thesis_demo"

# Module names contain DIGITS (`hmac_sha256`) and may be NESTED (`a.b.f`). A
# first version matched `^  [a-z_]+\.[a-zA-Z_]+\(`, which excluded both: it
# silently dropped all 24 hmac_sha256 functions and 2 of fixed_capacity's, and
# the gate still reported "unchanged". A golden that quietly covers less than it
# claims is the worst kind — it looks tested. The per-example inventory below is
# what makes such a drop impossible to miss.
emit() {
  for ex in $EXAMPLES; do
    src="examples/$ex/src/main.con"
    if [ ! -f "$src" ]; then printf 'MISSING-EXAMPLE\t%s\t-\n' "$ex"; continue; fi
    if ! "$CC" "$src" --report extraction >"$TMP/$ex.out" 2>"$TMP/$ex.err"; then
      printf 'REPORT-FAILED\t%s\t-\n' "$ex"; continue
    fi
    awk -v ex="$ex" '
      /^  [A-Za-z_][A-Za-z_0-9.]*\(/ { name=$1; sub(/\(.*/, "", name); next }
      /^    fingerprint: / && name != "" {
        line=$0; sub(/^    fingerprint: /, "", line); print ex "\t" name "\t" line; name=""
      }' "$TMP/$ex.out"
  done | while IFS=$'\t' read -r ex name fp; do
    case "$ex" in MISSING-EXAMPLE|REPORT-FAILED) printf '%s\t%s\t%s\n' "$ex" "$name" "$fp"; continue ;; esac
    printf '%s\t%s\t%s\n' "$ex" "$name" "$(printf '%s' "$fp" | shasum -a 256 | cut -c1-32)"
  done | LC_ALL=C sort
}

if [ "${1:-}" = "--update" ]; then
  mkdir -p "$(dirname "$GOLDEN")"; emit > "$GOLDEN"
  echo "wrote $GOLDEN ($(wc -l < "$GOLDEN" | tr -d ' ') entries)"
  echo "NOTE: updating this golden means V1 CHANGED — the thing it exists to prevent."
  exit 0
fi

emit > "$TMP/current"
fail=0
note(){ echo "  FAIL $1" >&2; fail=1; }
okk(){ echo "  ok   $1"; }

# (1) EXACT, POSITIVE per-example inventories. Zero must never read as "no drift".
total=0
for row in "constant_time_tag 2" "crypto_verify 5" "elf_header 8" "fixed_capacity 20" \
           "hmac_sha256 24" "loop_invariant 3" "parse_validate 10" "thesis_demo 5"; do
  ex=${row% *}; want=${row#* }
  got=$(awk -F'\t' -v e="$ex" '$1==e' "$TMP/current" | wc -l | tr -d ' ')
  total=$((total + got))
  [ "$got" = "$want" ] && okk "$ex: $got functions" \
    || note "$ex: expected $want, got $got (a silent parser drop looks exactly like this)"
done
[ "$total" = "77" ] && okk "77 functions total" || note "expected 77 functions, got $total"

# (2) Report production + CLASSIFIED stderr, per example.
if grep -qE "^(MISSING-EXAMPLE|REPORT-FAILED)" "$TMP/current"; then
  note "an example is missing or failed: $(grep -E '^(MISSING-EXAMPLE|REPORT-FAILED)' "$TMP/current" | tr '\n' ' ')"
else okk "every example produced a report"; fi
unclass=0
for ex in $EXAMPLES; do
  e="$TMP/$ex.err"; [ -f "$e" ] || continue
  tot=$( { grep -c "^error:" "$e" || true; } ); unb=$( { grep -c "has no stored proof subject" "$e" || true; } )
  [ "$tot" = "$unb" ] || { note "$ex: $((tot-unb)) unclassified stderr diagnostic(s)"; unclass=1; }
done
[ "$unclass" = "0" ] && okk "all stderr diagnostics are the expected unbound-proof kind"

# (3) The STORED corpus — a different population, counted on its own.
# From `lib/fingerprints.sh` — the ONE definition of a stored link. This regex used to live
# here AND in check_migration_manifest.sh as separate text, which is how four different
# denominators (23/44/53/67) came to be quoted for the same population.
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fingerprints.sh"
stored="$(fp_count)"
# 44 -> 43 on 2026-08-18, and the reason is recorded because a denominator that moves without one is
# indistinguishable from a corpus that quietly lost evidence.
#
# `proof_pressure.validate_header` carried `#[proof_by(Examples.ElfHeader.Proofs.validate_header_correct)]`
# and a stored fingerprint. That link was WRONG: the theorem is about `elf_header`'s
# `validate_header`, which checks ELF magic/class/data/version bytes, while this function checks a
# nonce and a version field and calls `check_nonce`. They shared a qualified name and nothing else,
# and the link passed every name-based check for months until correspondence caught it.
#
# It was repaired by DELETING THE CLAIM rather than attaching a different theorem — no theorem proves
# that body, and substituting the nearest plausible one is the defect being repaired, not a fix. So
# the population is genuinely one smaller, and the pin says so.
[ "$stored" = "$FP_EXPECTED_COUNT" ] && okk "$FP_EXPECTED_COUNT stored #[proof_fingerprint] values (44 before the retargeted link was deleted; 43 -> 50 when bug 083 moved the hardcoded table's claims into source; 50 -> 57 for the seven workload-proof links)" \
  || note "expected $FP_EXPECTED_COUNT stored fingerprints, found $stored"

# (4) CROSS-VALIDATE the compiler's shortHash against the system SHA-256. Hashing
# with shasum alone only checks the golden against itself.
pin="concrete-shortHash-crossvalidation"
sys=$(printf '%s' "$pin" | shasum -a 256 | cut -c1-32)
printf 'import Concrete\n#eval Concrete.shortHash "%s"\n' "$pin" > "$TMP/sh.lean"
got=$(lake env lean "$TMP/sh.lean" 2>&1 | tr -d '" \n')
[ "$got" = "$sys" ] && okk "Concrete.shortHash agrees with system SHA-256 ($sys)" \
  || note "shortHash MISMATCH: compiler='$got' system='$sys'"

# (5) Duplicate keys and parser drift.
dups=$( { awk -F'\t' '{print $1"\t"$2}' "$TMP/current" | LC_ALL=C sort | uniq -d | head -3 || true; } )
[ -z "$dups" ] && okk "no duplicate example/function keys" || note "duplicate keys: $(echo "$dups" | tr '\n' ' ')"
mal=$(awk -F'\t' 'NF!=3 || $3 !~ /^[0-9a-f]{32}$/' "$TMP/current" | wc -l | tr -d ' ')
[ "$mal" = "0" ] && okk "every row is a well-formed 32-hex digest" || note "$mal malformed row(s) — parser drift"

# (6) The golden itself.
n=$( { wc -l < "$GOLDEN" 2>/dev/null || echo 0; } | tr -d ' ')
[ "${n:-0}" -ge 77 ] || note "golden has ${n:-0} rows, expected >= 77 (a shrunken golden is a vacuous pass)"
if diff -u "$GOLDEN" "$TMP/current" > "$TMP/diff" 2>&1; then okk "$n V1 fingerprints unchanged"
else note "V1 BODY FINGERPRINTS CHANGED — V2 must be built ALONGSIDE V1"; head -20 "$TMP/diff" >&2; fi

echo ""
[ "$fail" = "0" ] && { echo "V1-FINGERPRINT-GOLDEN: PASS ($n extracted, $FP_EXPECTED_COUNT stored)"; exit 0; }
echo "V1-FINGERPRINT-GOLDEN: FAIL" >&2; exit 1
