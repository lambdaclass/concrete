#!/usr/bin/env bash
# EVIDENCE REPRODUCIBILITY (R-0004 package 3).
#
# Every identity R-0004 mints is supposed to be a property of the PROGRAM, not of where the program
# happens to sit. `PackageIdentity.of?` refuses a component that looks location-dependent, which
# establishes the rule at the type level and says nothing about whether the pipeline OBEYS it: a path
# could still reach an identity through a digest, a source map, or a report that composes one.
#
# So this gate compiles the same program from a different absolute path and requires the evidence to
# be identical — subject identities, dependency roots, correspondence, and the attestation join.
#
# WITH A NEGATIVE CONTROL, because "two runs agree" is exactly what a check comparing constants would
# also report. A copy whose CONTENT differs by one character must produce different identities, or
# the comparison above is measuring nothing.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
BIN="$ROOT_DIR/.lake/build/bin/concrete"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# The evidence surface, as one string per program. Captured rather than piped so a compiler exit code
# cannot silently decide the comparison — these fixtures exit non-zero when they carry proof defects,
# which is exactly the population worth checking.
evidence_of() {
  local src="$1"
  {
    "$BIN" "$src" --report attestation-join 2>/dev/null | grep -E '^(subject|dependency)' || true
    "$BIN" "$src" --report subject-facts 2>/dev/null | grep -E 'depRoot|shadow correspondence' || true
  }
}

echo "=== the same program at a different path yields the same evidence ==="

# TWO FIXTURES, deliberately different in kind: one whose claims are clean, one whose claims refuse.
# A reproducibility check that only ever sees passing material would not notice a refusal REASON that
# embedded a path — and a refusal is exactly where a path is most likely to leak into a message.
for fixture in elf_header proof_pressure; do
  src="examples/$fixture/src/main.con"
  [ -f "$src" ] || { no "fixture $src is missing — this gate lost its subject"; continue; }

  here="$(evidence_of "$src")"

  # A DIFFERENT ABSOLUTE PATH, and a different directory NAME. The name matters separately: the
  # package identity comes from the manifest's declared name, so a copy under another directory name
  # must still agree — if it did not, the identity would be tracking the checkout layout.
  cp -r "examples/$fixture" "$TMP/relocated_$fixture"
  there="$(evidence_of "$TMP/relocated_$fixture/src/main.con")"

  if [ -z "$here" ]; then
    no "$fixture produced no evidence at all — the comparison below would be vacuous"
  elif [ "$here" = "$there" ]; then
    ok "$fixture: identical evidence from $TMP/relocated_$fixture (identities are not location-dependent)"
  else
    no "$fixture: evidence DIFFERS when the program moves — a path is reaching an identity"
    diff <(printf '%s' "$here") <(printf '%s' "$there") | head -6 | sed 's/^/      /'
  fi
done

echo "=== a content change DOES move the evidence (non-vacuity) ==="
# Without this, every assertion above would also pass if the pipeline emitted a constant. One byte,
# inside a function body, so the change is semantic rather than cosmetic.
cp -r "examples/elf_header" "$TMP/mutated_elf"
MUT="$TMP/mutated_elf/src/main.con"
if grep -q 'b0 == 127' "$MUT"; then
  sed -i 's/b0 == 127/b0 == 126/' "$MUT"
  mutated="$(evidence_of "$MUT")"
  baseline="$(evidence_of "examples/elf_header/src/main.con")"
  if [ "$mutated" != "$baseline" ]; then
    ok "changing one byte of a body changes the evidence (the comparison is measuring something)"
  else
    no "a changed body produced IDENTICAL evidence — the reproducibility check above proves nothing"
  fi
else
  no "could not find the expected literal to mutate — the non-vacuity control lost its target"
fi

echo "=== the same program at the same path yields the same evidence twice ==="
# Determinism is a different property from path-independence: a run could be reproducible across
# locations and still vary between invocations (map iteration order, a timestamp, a temp path).
a="$(evidence_of "examples/elf_header/src/main.con")"
b="$(evidence_of "examples/elf_header/src/main.con")"
if [ "$a" = "$b" ]; then
  ok "two invocations over the same tree agree byte for byte"
else
  no "two invocations DISAGREE — evidence is not deterministic, so nothing derived from it is reproducible"
fi

echo "=== no absolute path appears in any identity ==="
# The strongest form of the rule, checked against the material rather than against the constructor:
# an identity that embedded a path would be reproducible only by accident of this machine's layout.
JOIN="$("$BIN" examples/elf_header/src/main.con --report attestation-join 2>/dev/null || true)"
IDS="$(printf '%s' "$JOIN" | grep -E '^(subject|dependency)' | awk -F'\t' '{print $2, $3, $4, $5, $6}' || true)"
if printf '%s' "$IDS" | grep -qE '/|\\\\'; then
  no "an identity component contains a path separator — identity is tracking the checkout"
  printf '%s' "$IDS" | grep -E '/|\\\\' | head -3 | sed 's/^/      /'
else
  ok "no identity component contains a path separator"
fi

GATE_DONE=1
echo "EVIDENCE-REPRODUCIBILITY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
