#!/usr/bin/env bash
# Axiom-inventory gate (ROADMAP Phase 10 #16).
#
# Every theorem the compiler treats as `proved_by_lean` evidence must depend
# only on the allowlisted Lean kernel axioms. Anything else is a silent trust
# upgrade and fails the gate:
#   - `sorryAx`                       → incomplete proof presented as evidence
#   - `Lean.ofReduceBool`/`trustCompiler` → native-code trust (native_decide,
#     and bv_decide's compiled LRAT checker) — permitted ONLY for theorems
#     named in scripts/tests/axiom_native_trust.txt AND docs/verification/AXIOMS.md
#   - any user-declared axiom         → must never back proof evidence
# The full trust story lives in docs/verification/AXIOMS.md; this gate keeps it honest.
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
# NATIVE-CODE TRUST HAS TWO SPELLINGS. Lean 4.28's bv_decide/native_decide rested on the axioms above;
# Lean 4.34's emit a fresh axiom per proof, named `<decl>._native.bv_decide.ax_<n>` (or
# `._native.native_decide.ax_<n>`). Same trust, same rule: permitted only for theorems listed in
# $NATIVE_FILE and documented in AXIOMS.md. Matched narrowly by that generated naming.
is_native() {
  case " $NATIVE_AXIOMS " in *" $1 "*) return 0 ;; esac
  case "$1" in *._native.bv_decide.ax_*|*._native.native_decide.ax_*) return 0 ;; esac
  return 1
}

TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT

grep -rho 'proof_by([A-Za-z0-9_.]*)' --include='*.con' examples/ tests/ std/ 2>/dev/null \
  | sed 's/proof_by(\(.*\))/\1/' | sort -u > "$TMP/names.txt"
[ -s "$TMP/names.txt" ] || { echo "error: no proof_by theorem names found" >&2; exit 2; }

{
  # `import Examples` too: the flagship example proofs live in the separate
  # `Examples` Lake library (proofs/), not the compiler umbrella — same rule as
  # the check-proofs / prove --check codegen in Main.lean.
  echo "import Concrete"
  echo "import Examples"
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
BAD_UNKNOWN=$(grep -vE "DoesNotExist|not_yet_proved|drift_test_theorem|totally_fake_theorem|^Concrete\.Proof\.compute_checksum_correct$" "$TMP/unknown.txt" || true)
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
    if is_native "$a"; then
      grep -q "^$thm\$" "$NATIVE_FILE" 2>/dev/null \
        || NATIVE_UNLISTED="$NATIVE_UNLISTED $thm"
    else
      VIOLATIONS="$VIOLATIONS $thm:$a"
    fi
  done
done < "$TMP/records.txt"

[ -z "$VIOLATIONS" ] \
  && ok "no theorem depends on an unknown axiom or sorryAx" \
  || no "theorems escaping the axiom allowlist:$VIOLATIONS"

[ -z "$(echo "$NATIVE_UNLISTED" | tr -d ' ')" ] \
  && ok "native-code trust (bv_decide/native_decide) only where declared in $NATIVE_FILE" \
  || no "theorems with UNDECLARED native-code trust:$NATIVE_UNLISTED (add to $NATIVE_FILE + docs/verification/AXIOMS.md, or remove the dependency)"

# Every theorem granted native trust must also be documented in AXIOMS.md.
DOC_MISSING=""
while read -r thm; do
  case "$thm" in \#*|"") continue ;; esac
  grep -q "$thm" docs/verification/AXIOMS.md 2>/dev/null || DOC_MISSING="$DOC_MISSING $thm"
done < "$NATIVE_FILE"
[ -z "$DOC_MISSING" ] \
  && ok "all native-trust theorems are documented in docs/verification/AXIOMS.md" \
  || no "native-trust theorems missing from docs/verification/AXIOMS.md:$DOC_MISSING"

# docs/verification/AXIOMS.md must state the active kernel allowlist.
DOC_ALLOW_OK=1
for a in $ALLOWLIST; do
  grep -q "$a" docs/verification/AXIOMS.md 2>/dev/null || DOC_ALLOW_OK=0
done
[ "$DOC_ALLOW_OK" = "1" ] \
  && ok "docs/verification/AXIOMS.md documents the kernel-axiom allowlist" \
  || no "docs/verification/AXIOMS.md missing/out of sync with the allowlist ($ALLOWLIST)"

# ---------------------------------------------------------------------------
# NEGATIVE CONTROL: does the detection actually catch anything?
#
# Everything above asserts that NO registered proof escapes the allowlist. That
# passes just as happily if the detection is broken -- an empty theorem list, a
# `#print axioms` that silently produced nothing, an allowlist that accidentally
# swallowed `sorryAx`. "No violations found" and "the finder is broken" look
# identical from the outside, and this gate guards the single most important claim
# in the repo: that a `sorry` cannot be presented as evidence.
#
# So: build a theorem that genuinely depends on `sorryAx`, run the SAME pipeline
# over it, and require that it is flagged.
echo "=== negative control: a sorry-backed theorem must be CAUGHT ==="
NC="$TMP/nc"; mkdir -p "$NC"
cat > "$NC/Bad.lean" <<'LEAN'
theorem nc_incomplete : ∀ n : Nat, n + 0 = n := by sorry
#print axioms nc_incomplete
LEAN
# `lake env lean` prints the axioms to stdout; capture separately from the exit
# status, because a file containing `sorry` exits non-zero (warning) and
# `set -o pipefail` would otherwise turn a MATCH into a failure.
NC_OUT="$( lake env lean "$NC/Bad.lean" 2>&1 || true )"
NC_AXIOMS="$( printf '%s' "$NC_OUT" | sed -n 's/^.*depends on axioms: \[\(.*\)\]$/\1/p' | tr -d ',' )"
if [ -z "$NC_AXIOMS" ]; then
  no "the control produced NO axiom line — \`#print axioms\` parsing is broken, so the gate above proves nothing"
else
  # Run the real classification over it: sorryAx is neither allowlisted nor a
  # native-trust axiom, so it must land in VIOLATIONS.
  NC_VIOL=""
  for a in $NC_AXIOMS; do
    case " $ALLOWLIST " in *" $a "*) continue ;; esac
    is_native "$a" && continue
    NC_VIOL="$NC_VIOL $a"
  done
  case "$NC_VIOL" in
    *sorryAx*) ok "a sorry-backed theorem is classified as a violation (detection has teeth)" ;;
    "")        no "sorryAx was NOT flagged — the allowlist or classifier is swallowing it" ;;
    *)         no "the control was flagged, but not for sorryAx:$NC_VIOL" ;;
  esac
  # And the allowlist must never contain it, which is the one-line way this gate
  # could be silently disabled.
  case " $ALLOWLIST " in
    *" sorryAx "*) no "sorryAx is IN the allowlist — every incomplete proof would pass" ;;
    *)             ok "sorryAx is not allowlisted" ;;
  esac
fi

echo ""
echo "AXIOM-INVENTORY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
