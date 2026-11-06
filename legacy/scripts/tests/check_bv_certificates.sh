#!/usr/bin/env bash
# Independent DRAT check of bv_decide's bit-blasting.
#
# docs/verification/AXIOMS.md records a real trust extension: `bv_decide` proves a goal by
# bit-blasting to SAT and validating the solver's LRAT certificate, but that
# validation runs as COMPILED LEAN (Lean.ofReduceBool / Lean.trustCompiler). So a
# theorem "proved by kernel-checked bitblasting" is not kernel-only — six named
# theorems in the SHA-256 refinement stack currently rest on it.
#
# This gate does not remove that trust. It CORROBORATES it with a separately
# implemented checker: the exact CNF Lean bit-blasted is captured (via a sat.solver
# shim, since Lean exposes no option to retain it), independently re-solved, and its
# DRAT certificate verified by drat-trim — a different implementation, in C, by
# different authors. A bug in Lean's native LRAT checker alone therefore cannot make
# an unsound bitblasting claim pass unnoticed.
#
# Honest boundary: drat-trim is itself unverified C. Two independent checkers
# agreeing is strictly better than one, and strictly weaker than a verified checker
# (cake_lpr, which is not packaged in nixpkgs — see the report in docs).
#
# Skip-if-absent: cadical ships with Lean, but drat-trim comes from
# `nix develop .#provers`, so this is a no-op in the default shell.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

if ! command -v drat-trim >/dev/null 2>&1; then
  echo "=== drat-trim absent — skipping (use \`nix develop .#provers\`) ==="
  echo ""
  echo "BV-CERTIFICATES: PASS=0  FAIL=0 (skipped)"
  exit 0
fi

SHIM="$ROOT_DIR/scripts/bv_capture_solver.sh"
if [ ! -x "$SHIM" ]; then
  echo "error: $SHIM missing or not executable" >&2
  exit 2
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
export CNF_CAPTURE_DIR="$WORK/cnf"
mkdir -p "$CNF_CAPTURE_DIR"

# Goals shaped like the ones the project actually discharges with bv_decide: word
# level identities and masking/width round-trips (cf. Concrete/ProofKit/Arith.lean).
# Each must genuinely need SAT — a goal closed by normalization never calls the
# solver, captures no CNF, and would make this gate vacuous.
cat > "$WORK/BvGoals.lean" <<LEOF
import Std.Tactic.BVDecide
set_option sat.solver "$SHIM"

example (x y : BitVec 16) : (x ^^^ y) ^^^ y = x := by bv_decide
example (x y : BitVec 8)  : x + y = y + x := by bv_decide
example (y : BitVec 32)   : y &&& 255#32 = (BitVec.setWidth 8 y).setWidth 32 := by bv_decide
LEOF

echo "=== bit-blasting bv_decide goals through the capture shim ==="
if lake env lean "$WORK/BvGoals.lean" > "$WORK/lean.out" 2>&1; then
  ok "bv_decide closed every goal (shim did not change its behaviour)"
else
  no "bv_decide failed under the shim"
  head -20 "$WORK/lean.out" >&2
fi

CNFS=$(find "$CNF_CAPTURE_DIR" -name '*.dimacs' | sort)
COUNT=$(printf '%s\n' "$CNFS" | grep -c '[^[:space:]]' || true)

# Non-vacuity: if nothing was captured, everything below would "pass" while checking
# nothing at all. That is the failure mode this gate must not have.
if [ "$COUNT" -ge 1 ]; then
  ok "captured $COUNT bit-blasted CNF(s) — the check is not vacuous"
else
  no "captured NO CNF: goals were closed without SAT, so nothing was verified"
  echo ""
  echo "BV-CERTIFICATES: PASS=$PASS  FAIL=$FAIL"
  [ "$FAIL" -eq 0 ]
  exit
fi

# drat-trim prints its verdict prefixed by a CARRIAGE RETURN (it overwrites a
# progress line), so `^s VERIFIED` never matches on the raw output. Strip CRs before
# anchoring — and anchor, because an unanchored "s VERIFIED" also matches the
# failure verdict "s NOT VERIFIED".
verdict_verified(){ printf '%s' "$1" | tr -d '\r' | has_match '^s VERIFIED$'; }

# check_cnfs TAG CNF... — independently re-solve each CNF and check its DRAT certificate; counts the
# non-trivial certificates verified in CERTS.
check_cnfs() {
  local tag=$1; shift
  local i=0 c vars cls rc DT
  CERTS=0
  for c in "$@"; do
    i=$((i+1))
    vars=$(awk '/^p cnf/{print $3; exit}' "$c")
    cls=$(awk '/^p cnf/{print $4; exit}' "$c")
    # Independent solve. --no-binary so drat-trim reads a textual DRAT proof (Lean asks
    # cadical for BINARY LRAT, a different format drat-trim does not consume).
    cadical "$c" "$WORK/proof.$tag.$i.drat" --no-binary --unsat --quiet >/dev/null 2>&1
    rc=$?
    if [ "$rc" -eq 20 ]; then
      ok "$tag CNF $i ($vars vars, $cls clauses): independently UNSAT"
    else
      no "$tag CNF $i: expected UNSAT (exit 20), got exit $rc — bit-blasting disagreement"
      continue
    fi
    DT="$(drat-trim "$c" "$WORK/proof.$tag.$i.drat" 2>&1)"
    if ! verdict_verified "$DT"; then
      no "$tag CNF $i: drat-trim did not verify the certificate"
      printf '%s\n' "$DT" | tr -d '\r' | tail -5 >&2
      continue
    fi
    # A CNF that is contradictory on its own (complementary unit clauses) is reported
    # VERIFIED without the proof being read at all. That verifies nothing about the
    # certificate, so it must not be counted as a certificate check.
    if printf '%s' "$DT" | tr -d '\r' | has_match 'stop reading proof'; then
      ok "$tag CNF $i: trivially UNSAT from the CNF alone (certificate not exercised)"
    else
      CERTS=$((CERTS+1))
      ok "$tag CNF $i: DRAT certificate VERIFIED by drat-trim (independent of Lean)"
    fi
  done
}

echo "=== independently re-solving each CNF and checking its DRAT certificate ==="
# shellcheck disable=SC2086
check_cnfs sample $CNFS

# The whole point is checking at least one REAL certificate. Trivial CNFs passing
# would otherwise let this gate look green while exercising no proof.
if [ "$CERTS" -ge 1 ]; then
  ok "$CERTS non-trivial certificate(s) actually checked"
else
  no "no non-trivial certificate was checked — the gate would be vacuous"
fi

echo "=== teeth: drat-trim must REJECT an inadequate certificate ==="
# Without this, a checker that rubber-stamps everything would pass the gate above.
# An EMPTY proof is the unambiguous corruption: dropping the trailing empty clause is
# NOT enough, because drat-trim checks backwards and simply re-derives the conflict
# from the remaining lemmas — which is correct behaviour, not a missing tooth.
# The CNF used here needed 528 resolution steps, so no proof means no contradiction.
FIRST=$(printf '%s\n' "$CNFS" | head -1)
: > "$WORK/bad.drat"
BAD="$(drat-trim "$FIRST" "$WORK/bad.drat" 2>&1)"
if verdict_verified "$BAD"; then
  no "drat-trim VERIFIED an EMPTY certificate — the checker has no teeth"
else
  ok "an empty certificate is rejected (checker has teeth)"
fi

echo "=== the project's own bv_decide goals ==="
# The goals above are SAMPLES shaped like the project's. The trust AXIOMS.md names is in the
# project's own: each lemma in axiom_native_owners.txt mints one `._native.bv_decide.ax_<n>` axiom
# per SAT-backed bv_decide call. Re-elaborating the files that define those lemmas through the
# shim captures exactly those SAT problems; their count must equal the number of native axioms
# Lean's environment holds, so a goal can neither be missed nor be counted twice.
PROJ_DIR="$WORK/cnf-project"; mkdir -p "$PROJ_DIR"
OWNER_FILES=$(grep -vE '^(#|$)' scripts/tests/axiom_native_owners.txt | while read -r o; do
  grep -rlE "^(private |protected )*theorem ${o##*.}( |$)" Concrete proofs; done | sort -u)
for f in $OWNER_FILES; do
  CNF_CAPTURE_DIR="$PROJ_DIR" lake env lean -Dsat.solver="$SHIM" "$f" > "$WORK/proj.out" 2>&1 \
    || { no "re-elaborating $f under the shim failed"; head -5 "$WORK/proj.out" >&2; }
done
EXPECTED=$(lake env lean scripts/tools/proof_layer_theorems.lean 2>/dev/null \
  | awk -F'\t' '{n=split($2,a," "); for(i=1;i<=n;i++) print a[i]}' | sort -u \
  | grep -cE '\._native\.bv_decide\.ax_[0-9_]+$' || true)
PCNFS=$(find "$PROJ_DIR" -name '*.dimacs' | sort)
PCOUNT=$(printf '%s\n' "$PCNFS" | grep -c '[^[:space:]]' || true)
if [ "$PCOUNT" -ge 1 ] && [ "$PCOUNT" = "$EXPECTED" ]; then
  ok "captured $PCOUNT SAT problems from $(echo $OWNER_FILES | wc -w | tr -d ' ') file(s) — one per native axiom ($EXPECTED)"
else
  no "captured $PCOUNT SAT problems from the owners' files, but the environment holds $EXPECTED native bv_decide axioms"
fi
# shellcheck disable=SC2086
check_cnfs project $PCNFS
# MEASURED (2026-10-15): all six are 1-variable, 2-clause CNFs — bv_decide's normalization and AIG
# construction already reduced each goal to a contradiction, so there is no non-trivial certificate
# for drat-trim to check. The trust the native axioms carry is therefore the COMPILED bit-blasting
# (`verifyBVExpr` evaluated natively), which no certificate check can reach; AXIOMS.md says so. The
# split is reported, not asserted, so that a goal that does need a real certificate later is
# checked here, not waved through.
echo "  note project certificates: $CERTS non-trivial, $((PCOUNT - CERTS)) trivially UNSAT from the CNF alone"

echo ""
echo "BV-CERTIFICATES: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
