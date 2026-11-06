#!/usr/bin/env bash
# Register C gate — the evidence algebra (Concrete/Report/Evidence.lean).
#
# Registers A (obligation sufficiency) and B (transformation soundness) are about
# program semantics and are years of work. Register C is about the evidence DATA
# STRUCTURE, so its rows are dischargeable now — and they are discharged as
# compile-time theorems, not tests. A green build is the proof; this gate exists to
# make deleting or weakening a row LOUD rather than silent, and to lock the
# structural properties a theorem cannot state (one construction site, wired
# consumers, no axiom escape hatches).
#
# Why the rows matter, concretely: H23 shipped a guaranteed out-of-bounds access as
# `proved_by_multi_kernel (3: lean, rocq, isabelle)` because an obligation's status
# was computed without consulting the hypotheses it rested on. C2/C2'/C3 make that
# combination unrepresentable rather than merely wrong.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
EV="Concrete/Report/Evidence.lean"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

[ -f "$EV" ] || { echo "FAIL evidence algebra module missing: $EV"; exit 1; }

echo "=== Register C rows are present and kernel-checked (green build ⇒ proven) ==="
# Each row is a THEOREM in the module. Deleting one is a red gate here; weakening one
# to `sorry` is caught below. The build having succeeded is what makes them proofs.
for t in c1_combine_keeps_left c1_combine_keeps_right \
         c2_combine_proved c2_under_hypotheses_proved \
         c3_caps_to_assumed c3_present_unchanged_when_proved \
         c4_discharge_shrinks c5_self_justifying_free; do
  grep -q "^theorem $t\|^theorem $t " "$EV" \
    && ok "row present: $t" || no "Register C row MISSING: $t"
done

echo "=== no escape hatches in the proofs ==="
# `sorry`/`admit` would make a row vacuous while the gate above still passed.
# `native_decide` would discharge it via compiled code, extending trust to
# Lean.ofReduceBool / Lean.trustCompiler — the very extension docs/verification/AXIOMS.md gates
# elsewhere. Neither belongs in a register that exists to REMOVE trust.
for bad in sorry admit native_decide; do
  grep -qE "\b$bad\b" "$EV" \
    && no "$EV contains '$bad' — a Register C row is not actually proved" \
    || ok "no '$bad' in the evidence algebra"
done

echo "=== behaviour is locked at COMPILE time, not by this gate ==="
# An external review defeated the earlier version of this gate: deleting the validation
# filter from multiKernelVerdict — which lets a kernel whose rendering denotes a DIFFERENT
# proposition attest to this one — left it green at 24/24, because everything here checks
# names, counts and construction sites, never behaviour.
#
# The fix is not more grepping. `example`s in Evidence.lean pin the verdict truth table by
# `rfl`, so such a mutation is a BUILD failure. This gate's job is only to prove the locks
# were not deleted; the build is what proves they hold.
for lock in "unvalidated .closed" "wrongObligation .closed" "wrongKernel .closed" "ok .absent" "ok .refused"; do
  grep -q "$lock" "$EV" && ok "behavioural lock present: $lock" \
                        || no "behavioural lock MISSING: $lock — a truth-table row is unpinned"
done
LOCKS="$(grep -c "^example : (multiKernelVerdict" "$EV")"
[ "$LOCKS" -ge 8 ] && ok "verdict truth table pinned by $LOCKS compile-time examples" \
                   || no "only $LOCKS verdict examples (expected >=8) — rows were removed"

echo "=== validation is a WITNESS, not a boolean claim ==="
# The deepest fix from that review. `loweringAgreed : Bool` was a claim ABOUT a check
# rather than a product OF one: any caller could write `true`, and two of the three
# consumers did exactly that, relying on an upstream filter. The third derived it from a
# set that failed open. Both were possible only because the type let a caller assert the
# check instead of performing it.
#
# `LoweringValidated` has a private constructor and is minted only from the set of
# obligations whose agreement lemma CLOSED, and it is bound to the kernel and obligation it
# validates so it cannot be reused across either.
grep -q "structure LoweringValidated where" "$EV" \
  && ok "LoweringValidated exists" || no "LoweringValidated missing — validation is a bare Bool again"
awk '/structure LoweringValidated where/,/deriving/' "$EV" | has_match "private mk ::" \
  && ok "LoweringValidated constructor is private (cannot be fabricated)" \
  || no "LoweringValidated constructor is PUBLIC — a caller can fabricate a witness"
grep -q "def LoweringValidated.mint" "$EV" \
  && ok "the only route to a witness is mint (requires the agreement-closed set)" \
  || no "no mint function — how is a witness obtained?"
grep -q "validated : Option LoweringValidated" "$EV" \
  && ok "KernelInput carries a witness, not a Bool" \
  || no "KernelInput does not carry Option LoweringValidated"
grep -qE "w.kernel == k.name && w.obligation == ob" "$EV" \
  && ok "the witness is checked against THIS kernel and THIS obligation (no reuse)" \
  || no "witness binding not checked — a witness could be reused across kernels/obligations"
# And no consumer may assert validation as a literal, which is what this replaced.
#
# Match CODE, not prose: require `kernel :=` on the same line so a receipt literal is
# matched and the comments explaining this history are not. The first version of this
# check flagged its own explanatory comments — a gate whose failure mode is describing
# the defect it guards against is noise, and noise is how gates get ignored.
#
# `lean` is the one permitted literal, and the reason once recorded for it was WRONG. The
# claim was that Lean's rendering IS the reference the others are validated against, so
# nothing exists to validate it with. The agreement check actually validates a rendering
# against the reference EVALUATOR (safeOn/evalBoolEnv, walking the AST) — Lean's rendering is
# not the yardstick, it is simply the one never measured. The real obstacle was that Lean's
# lowering was not expressible as a driver, which R-0450's slice (2026-08-03) removed. Still
# a real gap, still R-0450, but a TODO rather than an asymmetry that cannot be closed.
if grep -rn "loweringAgreed := true" Main.lean Concrete/Report/Report.lean 2>/dev/null \
     | grep "kernel :=" | has_match .; then
  no "a consumer writes 'loweringAgreed := true' — validation asserted, not derived"
else
  ok "NO kernel asserts validation as a literal — lean included, since R-0450"
fi
grep -q "loweringAgreed := if leanChecked then leanW.isSome else true" Concrete/Report/Report.lean \
  && ok "lean's receipt DERIVES from a minted witness when the check ran" \
  || no "lean's receipt no longer derives from a witness"
grep -q "loweringAgreed := rocqW.isSome" Concrete/Report/Report.lean \
  && ok "receipts DERIVE loweringAgreed from the minted witness" \
  || no "receipts no longer derive loweringAgreed from a witness"

echo "=== C4 is enforced by the TYPE, not only by the theorem ==="
# C4 says discharge is the only operation that shrinks an assumption set. That constrains
# the function; the private constructor is what constrains the type. Without it,
# `{ e with assumes := [] }` forges a discharge and the claim presents as proved — the
# review demonstrated exactly that. Both halves are asserted because deleting either
# reopens the hole.
grep -q "private mk ::" "$EV" \
  && ok "Evidence has a private constructor (record-update forgery is a compile error)" \
  || no "Evidence constructor is PUBLIC — '{ e with assumes := [] }' forges a discharge"

echo "=== C3's companion: the capped status is not a proof class ==="
# C3 proves a claim with outstanding assumptions presents as exactly "assumed".
# This proves "assumed" is outside proofClasses. Together they close the H23 CLASS —
# a capped claim can never read as proved.
#
# R-0461 (2026-08-03) closed H23 ITSELF by populating `assumes` from real hypothesis
# provenance, which is what turned these rows from proved substrate into rows that fire on
# live verdicts. The fixture assertions in check_known_wrong_corpus.sh are now inverted and
# guard the fix; that gate, not this one, is what proves the hole is shut.
grep -q 'example : (!proofClasses.contains "assumed") = true := rfl' Concrete/Report/Report.lean \
  && ok "compile-time proof that the capped status is not a proof class" \
  || no "missing companion example tying C3 to proofClasses"

echo "=== ONE derivation: badge strings are constructed in exactly one module ==="
# The structural property no theorem can state, and the one whose absence produced
# the report/artifact divergence: if two surfaces can each assemble a badge, they can
# disagree, and one of them can learn about a class the other never hears about.
# `kernel_disagreement` existed in the report and not in the stored artifact for
# exactly this reason. Vocabulary LISTS are not construction, so they are excluded.
for cls in proved_by_two_kernels proved_by_multi_kernel kernel_disagreement; do
  SITES="$(grep -rln "s!\"$cls\|(\"$cls\"," Concrete/ Main.lean 2>/dev/null | sort -u)"
  N="$(printf '%s' "$SITES" | grep -c . || true)"
  if [ "$N" = "1" ] && has_match "Evidence.lean" <<<"$SITES"; then
    ok "$cls is constructed only in Evidence.lean"
  else
    no "$cls constructed in $N site(s): $(printf '%s' "$SITES" | tr '\n' ' ')"
  fi
done

echo "=== every consumer goes through the shared derivation ==="
# Report, ledger fold and release policy must all call multiKernelVerdict. A surface
# that computes its own class is a surface that can know something the others do not.
CONSUMERS="$(grep -rc "multiKernelVerdict" Main.lean Concrete/Report/Report.lean 2>/dev/null | tr '\n' ' ')"
[ "$(grep -c "multiKernelVerdict" Main.lean)" -ge 2 ] \
  && ok "Main.lean routes report AND policy through multiKernelVerdict" \
  || no "a Main.lean surface still computes its own multi-kernel class ($CONSUMERS)"
grep -q "multiKernelVerdict" Concrete/Report/Report.lean \
  && ok "the ledger fold routes through multiKernelVerdict" \
  || no "foldMultiKernelResults does not use the shared derivation"

echo "=== the ledger stores a CANONICAL class, never a composite string ==="
# R-0440: a friendly composite label may not erase the underlying dimensions. The
# display form (`proved_by_two_kernels (lean, rocq)`) is for humans; the stored status
# must be a bare vocabulary word so it stays checkable against statusVocabulary.
grep -q '"kernel_disagreement"' Concrete/Proof/ObligationCore.lean \
  && ok "kernel_disagreement is in statusVocabulary" \
  || no "kernel_disagreement missing from statusVocabulary (ledger can emit it)"
grep -q "def MultiKernelVerdict.displayStatus" "$EV" \
  && ok "display form is separated from the canonical class" \
  || no "no displayStatus — the composite string risks being stored as a status"

echo "=== hypothesis provenance is total: every origin decides its own debt ==="
# The four origins have genuinely different justification status, and erasing that is
# what made H23 possible. `debt` must handle each explicitly — a catch-all would
# silently give a new origin zero debt, which is the fail-OPEN default this codebase
# keeps finding.
for o in guard statement invariant assumed; do
  grep -q "| \.$o" "$EV" && ok "HypOrigin.debt handles .$o explicitly" \
                         || no "HypOrigin.debt missing case: .$o"
done
# A catch-all over one of the algebra's own INDUCTIVES is the fail-open shape this forbids:
# add a constructor, and it silently inherits someone else's answer instead of failing to
# compile. A catch-all over a `String` is a different thing — String cannot be exhausted, so
# the question is only whether the default is fail-closed.
#
# Rather than weaken the rule to accommodate the second case, a catch-all may opt out with
# `CATCH-ALL-OK: <reason>` on the same line. `foundationOf` uses it: its input is a kernel
# NAME, and its default (`other`) counts an unrecognised kernel as its own foundation, which
# withholds independence credit rather than granting it. An unjustified catch-all still fails.
BAD_CATCHALL="$(grep -nE "^\s*\|\s*_\s*=>" "$EV" | grep -v "CATCH-ALL-OK" || true)"
if [ -n "$BAD_CATCHALL" ]; then
  no "evidence algebra has an UNJUSTIFIED catch-all (fail-open default):"
  printf '%s\n' "$BAD_CATCHALL" | head -3 | sed 's/^/         /'
else
  ok "no unjustified catch-all — a new constructor is a missing-case ERROR, not silent"
fi

echo "=== R-0461: the cap is wired, and enforced as well as displayed ==="
# Register C only stops being decorative if something populates `assumes` from real
# provenance and something acts on the result. Both halves are asserted because H23's
# first fix had the display and not the enforcement, and a report that reads `assumed`
# while `check` exits 0 is a hole with better manners.
grep -q "def capOnHypothesisDebt" Concrete/Report/Report.lean \
  && ok "capOnHypothesisDebt exists" \
  || no "capOnHypothesisDebt MISSING — nothing populates Evidence.assumes (H23 reopens)"
grep -q "Report.capOnHypothesisDebt (Report.dischargeVCs" Main.lean \
  && ok "the cap runs AFTER discharge (statuses are final when it reads them)" \
  || no "the cap is not composed with dischargeVCs — it would read non-final statuses"
grep -q "def loopInvariantDebt" Concrete/Report/ReportObligations.lean \
  && ok "hypothesis debt is derived from loop contracts, not asserted" \
  || no "loopInvariantDebt MISSING — hypDebt would be empty and the cap vacuous"
grep -q "def enforceNoCappedHypotheses" Concrete/Check/Policy.lean \
  && ok "a capped obligation is release-BLOCKING, not merely displayed (E0617)" \
  || no "no enforcement for capped obligations — the cap is display-only"
grep -q "cappedObligations := cap" Main.lean \
  && ok "the policy gate is fed from the same discharged ledger as the reports" \
  || no "capped obligations are not threaded to the policy — the gate sees an empty list"

echo "=== R-0458: the badge states its INDEPENDENCE, not just its count ==="
# The badge's value rests on the word "independent". Lean and Rocq are both CIC-family, so
# `proved_by_two_kernels (lean, rocq)` read as two chances to catch a foundational error when
# it was one. The count is STRENGTH; the foundation span is INDEPENDENCE; R-0440 forbids
# collapsing one into the other, so both are displayed.
grep -q "inductive Foundation" "$EV" \
  && ok "Foundation is a named axis, not an ad-hoc string test" \
  || no "no Foundation type — kernel independence is implicit again"
grep -q "def foundationOf" "$EV" && grep -q "def foundationSummary" "$EV" \
  && ok "one definition of which kernel rests on which foundation" \
  || no "foundationOf/foundationSummary missing"
# The duplicate this replaced: independenceOf had its own contains-chain in another module.
if grep -q 'attest.contains "isabelle"' Concrete/Report/Report.lean; then
  no "independenceOf still hard-codes kernel foundations — two copies can disagree"
else
  ok "independenceOf derives from foundationSummary (no second copy)"
fi
grep -q "foundationSummary attest" Concrete/Report/Report.lean \
  && ok "the ledger's independence facts and the badge share one derivation" \
  || no "independenceOf does not call foundationSummary"
# `other` must count as its OWN foundation: fail-open here flatters the badge.
grep -q 'example : foundationSummary \["lean", "mystery"\] = (2, "CIC×?") := rfl' "$EV" \
  && ok "an unclassified kernel counts as a separate foundation (no flattering default)" \
  || no "missing the unclassified-kernel lock — independence could be over-claimed"
grep -q 'example : foundationSummary \["lean", "rocq"\] = (1, "CIC") := rfl' "$EV" \
  && ok "compile-time proof that lean+rocq span ONE foundation" \
  || no "the CIC×CIC case is unpinned — the badge could over-claim again"

echo "=== R-0450 (partial): the linear fragment is lowered by ONE function ==="
# `exprToLeanProp` used to be an independent recursion that happened to match
# `exprToProver` case for case — differing only in negative-literal parentheses, a space,
# and `¬` vs `~`. Two functions that must agree, which is R-0450's stated defect. Two of
# those differences were cosmetic; the third is now a driver parameter.
RO="Concrete/Report/ReportObligations.lean"
grep -q "def exprToProverU" "$RO" \
  && ok "one parameterised lowering (exprToProverU) exists" \
  || no "exprToProverU missing — the fragment may be duplicated again"
grep -qE "^  exprToProverU leanBinOp \"¬\" e$" "$RO" \
  && ok "exprToLeanProp DELEGATES rather than re-implementing the recursion" \
  || no "exprToLeanProp is not a delegation — the fragment is defined twice"
# A RATCHET, not a claim of victory. R-0450 is only partly done: the module still contains
# several `.binOp` recursions, and they are not all duplicates —
#   exprToProverU   the one string lowering (Rocq / Isabelle / Lean)
#   exprToSmt       SMT-LIB, a genuinely different target syntax
#   arithToBVW      bit-vector widening for bv_decide
#   exprIntervalMax interval analysis, not a lowering at all
#   evalIntEnv      the reference EVALUATOR — must stay independent, since it is what
#                   renderings are validated against; merging it would be circular
#   evalBoolEnv     ditto, boolean
# Six today. Removing exprToLeanProp took it from seven. The number may only go DOWN: a new
# recursion is either a new duplicate or a change that should update this bound deliberately.
N="$(grep -c "| .binOp _ op l r => do" "$RO" || true)"
[ "$N" -le 6 ] \
  && ok "$N expression recursions over .binOp (ratchet: <=6; was 7 before R-0450's slice)" \
  || no "$N separate .binOp recursions — one was ADDED; R-0450 exists to reduce this"

echo "=== R-0450: Lean's own rendering is validated, and it is ENFORCED ==="
# Lean was the last unvalidated rendering, and the one that matters most: every other
# kernel checks a goal derived from it, so a fault there produces unanimous agreement on the
# wrong proposition — the single failure mode kernel multiplicity provably cannot detect.
grep -q "def leanLowering" "$RO" \
  && ok "a Lean driver exists (its lowering is expressible like any other)" \
  || no "no leanLowering — Lean's rendering cannot be put on the agreement scale"
grep -q "notSym : String" "$RO" \
  && ok "expression-level negation is driver-supplied (the blocker that kept Lean out)" \
  || no "notSym gone — exprToProver hard-codes negation again"
grep -q "def leanAgreementGoals" "$RO" && grep -q "def leanValidatedObligations" "$RO" \
  && ok "Lean agreement goals and the all-instances-closed rule exist" \
  || no "leanAgreementGoals/leanValidatedObligations missing"
# The domain restriction is load-bearing: omega is LINEAR, and the in-process path reports
# only the closed set, so an undecided nonlinear instance is indistinguishable from a
# refusal. Measured: without this, 48 of 96 instances read as DISAGREES with no real fault.
grep -q "exprHasNonlinMul o.mainExpr then none" "$RO" \
  && ok "restricted to omega's linear domain (no false 'disagrees' on nonlinear)" \
  || no "the nonlinear filter is gone — this check will cry wolf and be ignored"
# Enforcement, not display. This is the H23 lesson applied in advance.
grep -q "if leanChecked && leanW.isNone then base" Concrete/Report/Report.lean \
  && ok "an unvalidated Lean rendering earns NO badge (enforced, not just reported)" \
  || no "a faulty Lean rendering would still be badged — display without enforcement"
grep -q "engine := if leanChecked && leanW.isNone then v.engine" Concrete/Report/Report.lean \
  && ok "and the engine field does not advertise kernels whose badge was withheld" \
  || no "engine still lists attesters beside a deliberately withheld badge"

echo ""
echo "EVIDENCE-ALGEBRA: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
