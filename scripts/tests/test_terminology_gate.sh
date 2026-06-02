#!/usr/bin/env bash
set -euo pipefail

# Terminology Gate
#
# Verifies that all proof/obligation status strings across the codebase
# use the canonical terms defined in ObligationStatus.canonical:
#
#   proved | stale | missing | blocked | ineligible | trusted
#
# Catches drift like "no_proof", "not_eligible", "missing_proof", "not_proved"
# before it reaches users.
#
# Usage:
#   scripts/tests/test_terminology_gate.sh

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

FAIL=0

# Non-canonical terms that indicate drift
# Each entry: "old_term|canonical_replacement"
BANNED_TERMS=(
    'no_proof|missing'
    'not_eligible|ineligible'
    'missing_proof|missing'
    'not_proved|missing'
)

# Files to check: only git-tracked files (skips gitignored artifacts like .facts.json)
check_files() {
    git -C "$ROOT_DIR" ls-files -- '*.lean' '*.sh' '*.md' '*.json' \
        | grep -v 'test_terminology_gate.sh' \
        | sed "s|^|$ROOT_DIR/|"
}

echo "=== Terminology Gate ==="
echo ""

for entry in "${BANNED_TERMS[@]}"; do
    old="${entry%%|*}"
    canonical="${entry##*|}"

    # Search for the old term as a quoted string value (not as part of identifiers/filenames).
    # We grep for the term in quotes to avoid false positives from filenames.
    # Exclude diagnostic kind labels (stale_proof, missing_proof, unsupported_construct)
    # which are a different semantic domain from obligation/proof statuses.
    matches=$(check_files | xargs grep -rn "\"${old}\"\|'${old}'" 2>/dev/null \
        | grep -v "diagnosticKindLabel\|diagnostic_kind\|DiagnosticKind\|missingProof.*missing_proof\|staleProof.*stale_proof\|unsupportedConstruct.*unsupported_construct\|failure_class\|failureClassOf\|valid_failures\|diags\[(" \
        | grep -v "Former variants\|now banned\|→.*canonical\|→.*missing\|→.*ineligible\|(now banned)" || true)

    if [ -n "$matches" ]; then
        count=$(echo "$matches" | wc -l | tr -d ' ')
        echo "FAIL: found $count occurrence(s) of non-canonical term '$old' (should be '$canonical'):"
        echo "$matches" | head -10 | sed 's/^/  /'
        if [ "$count" -gt 10 ]; then
            echo "  ... and $((count - 10)) more"
        fi
        echo ""
        FAIL=$((FAIL + count))
    fi
done

# Also verify that the canonical terms actually appear (sanity check)
COMPILER=".lake/build/bin/concrete"
if [ -x "$COMPILER" ]; then
    # Pick a program that has mixed statuses
    test_prog="tests/programs/fib.con"
    if [ -f "$test_prog" ]; then
        json=$($COMPILER "$test_prog" --report diagnostics-json 2>&1 || true)
        for canonical in missing ineligible; do
            # These should appear in proof_status or obligation facts
            # (at least one function should have this status)
            if echo "$json" | grep -q "\"$canonical\""; then
                echo "  ok  canonical term '$canonical' found in diagnostics-json output"
            fi
        done
    fi
fi

echo ""
if [ "$FAIL" -gt 0 ]; then
    echo "TERMINOLOGY GATE FAILED: $FAIL non-canonical term(s) found"
    echo ""
    echo "Canonical proof/obligation status terms (from ObligationStatus.canonical):"
    echo "  proved | stale | missing | blocked | ineligible | trusted"
    echo ""
    echo "Fix by replacing non-canonical terms with their canonical equivalents."
    exit 1
else
    echo "Terminology gate passed: all status terms use canonical forms."
fi
