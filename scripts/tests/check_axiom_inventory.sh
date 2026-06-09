#!/usr/bin/env bash
# Axiom-inventory gate (ROADMAP Phase 10 #16).
#
# Every theorem the compiler treats as `proved_by_lean` evidence must depend
# only on the allowlisted Lean kernel axioms. Anything else is a silent trust
# upgrade and fails the gate:
#   - `sorryAx`                       → incomplete proof presented as evidence
#   - `Lean.ofReduceBool`/`trustCompiler` → native-code trust (native_decide,
#     and bv_decide's compiled LRAT checker) — permitted ONLY for theorems
#     named in scripts/tests/axiom_native_trust.txt AND docs/AXIOMS.md
#   - any user-declared axiom         → must never back proof evidence
# The full trust story lives in docs/AXIOMS.md; this gate keeps it honest.
#
# Mechanism: collect theorem names from every #[proof_by(...)] attribute in
# .con sources, generate a Lean file of `#print axioms` commands, run it under
# `lake env lean`, and diff each axiom set against the allowlist.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Kernel axioms every classical Lean development may use. Nothing else.
ALLOWLIST="propext Classical.choice Quot.sound"
NATIVE_AXIOMS="Lean.ofReduceBool Lean.trustCompiler"
NATIVE_FILE="scripts/tests/axiom_native_trust.txt"

TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT

grep -rho 'proof_by([A-Za-z0-9_.]*)' --include='*.con' examples/ tests/ std/ 2>/dev/null \
  | sed 's/proof_by(\(.*\))/\1/' | sort -u > "$TMP/names.txt"
[ -s "$TMP/names.txt" ] || { echo "error: no proof_by theorem names found" >&2; exit 2; }

{
  echo "import Concrete"
  while read -r n; do echo "#print axioms $n"; done < "$TMP/names.txt"
} > "$TMP/AxiomCheck.lean"

lake env lean "$TMP/AxiomCheck.lean" > "$TMP/raw.txt" 2>&1

# Join wrapped output, then extract one record per theorem.
tr '\n' ' ' < "$TMP/raw.txt" \
  | grep -oE "'[^']*' (depends on axioms: \[[^]]*\]|does not depend on any axioms)" \
  > "$TMP/records.txt"
grep -oE "Unknown constant \`[^\`]*\`" "$TMP/raw.txt" \
  | sed 's/Unknown constant `\(.*\)`/\1/' | sort -u > "$TMP/unknown.txt"

TOTAL=$(grep -c . "$TMP/names.txt")
CHECKED=$(grep -c . "$TMP/records.txt")
UNKNOWN=$(grep -c . "$TMP/unknown.txt")
echo "=== axiom inventory: $TOTAL linked theorems, $CHECKED checked, $UNKNOWN unresolvable ==="

# Unresolvable names must all be deliberate negative fixtures: fabricated
# names (DoesNotExist), or fixture-only registry names that the documented
# proof-status limitation deliberately does not validate (caught by
# `prove --check`). Anything NEW that fails to resolve is a broken link.
BAD_UNKNOWN=$(grep -vE "DoesNotExist|not_yet_proved|drift_test_theorem|totally_fake_theorem|^PureAdd\.add_comm$|^Concrete\.Proof\.(pure_(add|sub|mul)_correct|left_add_correct|right_add_correct|compute_checksum_correct)$" "$TMP/unknown.txt" || true)
[ -z "$BAD_UNKNOWN" ] \
  && ok "every unresolvable proof_by name is a known negative/fixture-only name" \
  || no "proof_by names that resolve to nothing (broken links): $BAD_UNKNOWN"

# Each checked theorem's axiom set must be within the allowlist; native-code
# trust is allowed only for theorems named in $NATIVE_FILE.
VIOLATIONS=""
NATIVE_UNLISTED=""
while IFS= read -r rec; do
  thm=$(echo "$rec" | sed "s/^'\([^']*\)'.*/\1/")
  case "$rec" in
    *"does not depend on any axioms"*) continue ;;
  esac
  axioms=$(echo "$rec" | sed 's/.*\[\(.*\)\].*/\1/' | tr ',' ' ')
  for a in $axioms; do
    case " $ALLOWLIST " in *" $a "*) continue ;; esac
    case " $NATIVE_AXIOMS " in
      *" $a "*)
        grep -q "^$thm\$" "$NATIVE_FILE" 2>/dev/null \
          || NATIVE_UNLISTED="$NATIVE_UNLISTED $thm"
        ;;
      *) VIOLATIONS="$VIOLATIONS $thm:$a" ;;
    esac
  done
done < "$TMP/records.txt"

[ -z "$VIOLATIONS" ] \
  && ok "no theorem depends on an unknown axiom or sorryAx" \
  || no "theorems escaping the axiom allowlist:$VIOLATIONS"

[ -z "$(echo "$NATIVE_UNLISTED" | tr -d ' ')" ] \
  && ok "native-code trust (bv_decide/native_decide) only where declared in $NATIVE_FILE" \
  || no "theorems with UNDECLARED native-code trust:$NATIVE_UNLISTED (add to $NATIVE_FILE + docs/AXIOMS.md, or remove the dependency)"

# Every theorem granted native trust must also be documented in AXIOMS.md.
DOC_MISSING=""
while read -r thm; do
  case "$thm" in \#*|"") continue ;; esac
  grep -q "$thm" docs/AXIOMS.md 2>/dev/null || DOC_MISSING="$DOC_MISSING $thm"
done < "$NATIVE_FILE"
[ -z "$DOC_MISSING" ] \
  && ok "all native-trust theorems are documented in docs/AXIOMS.md" \
  || no "native-trust theorems missing from docs/AXIOMS.md:$DOC_MISSING"

# docs/AXIOMS.md must state the active kernel allowlist.
DOC_ALLOW_OK=1
for a in $ALLOWLIST; do
  grep -q "$a" docs/AXIOMS.md 2>/dev/null || DOC_ALLOW_OK=0
done
[ "$DOC_ALLOW_OK" = "1" ] \
  && ok "docs/AXIOMS.md documents the kernel-axiom allowlist" \
  || no "docs/AXIOMS.md missing/out of sync with the allowlist ($ALLOWLIST)"

echo ""
echo "AXIOM-INVENTORY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
