#!/usr/bin/env bash
# ONE PRODUCER PER FACT — generalized from the obligation-record guard to a registry.
#
# WHY THIS EXISTS, concretely. "Two producers of one fact" is named as a defect class 14 times
# across this codebase and ROADMAP.md. It is enforced for exactly one fact
# (check_obligation_single_truth_source.sh, the obligation record). Everywhere else it lives in
# prose — and prose is not a gate.
#
# The cost of that was measured on 2026-08-13. `Report.lean:1763` states the rule for `depRoot`
# ("Both consumers read `ProofCore.dependencyNodesOf`, so there is one set of nodes rather than a
# Report-local rebuild — two builders is how a second, weaker answer appears"). Three lines away,
# `shadowEdgeKinds` violated it: it derived edge kinds as `if isTrusted then "trusted" else
# "unclassified"`, consulting no classification table, so it reported `unclassified` for edges the
# hand-back had classified `body`. Nothing checked. The stale line then caused a WRONG DIAGNOSIS —
# a coverage fail-open was reported, and root coverage was predicted to fall from 62/64 to ~53/64,
# when in fact the roots were correct and the report was lying. Two producers did not just risk
# divergence; they produced a false finding that was committed and had to be retracted.
#
# TWO ENTRY KINDS, because the class has two shapes:
#
#   FORMULA  a digest/preimage expression that must exist in exactly ONE file. A second copy is a
#            second answer to "did this change", and the copies drift silently because both are
#            plausible.
#   OWNERS   a derived fact (a kind, a currency decision, a node set) whose definition and
#            permitted consumers are named. A new consumer is not automatically wrong, but it must
#            be looked at — the failure names the file so the question is forced.
#
# CODE REFERENCES ONLY, via lib/code_refs.sh. A first version of a sibling guard grepped raw text
# and fired on a doc comment; and while writing THIS gate, a comment in `Report.lean` explaining
# the fix above made `classifiedEdgeOf` look like it had a consumer there. Documenting a boundary is
# not crossing it. The stripper is self-tested below before any verdict uses it, because narrowing
# what a guard reads is only safe if it still fires.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "scripts/tests/lib/code_refs.sh"

PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# --- the stripper must be trustworthy before it decides anything --------------------------------
if code_refs_selftest "$TMP/st" bodyBytesV2; then
  ok "code_refs ignores comment-only mentions and still catches real code references"
else
  no "code_refs self-test FAILED — every verdict below would be unreliable"
  echo "ONE-PRODUCER: PASS=$PASS FAIL=$FAIL"; exit 1
fi

SCOPE=(Concrete/ Main.lean)

# ================================================================================================
# FORMULA REGISTRY — each must live in exactly one file.
# ================================================================================================
echo "=== formulas have exactly one producer ==="

formula() {  # label | fragment | expected sole owner
  local label="$1" frag="$2" want="$3"
  local got; got="$(code_refs "$frag" "${SCOPE[@]}")"
  if [ -z "$got" ]; then
    no "$label: fragment '$frag' matches NO file — the registry entry is stale, so it was checking nothing"
  elif [ "$got" = "$want " ]; then
    ok "$label: one producer ($want)"
  else
    no "$label: expected sole producer '$want', found: $got — a second copy of a formula is a second answer"
  fi
}

formula "V1 source-body digest"  'shortHash (Proof.pexprCanonical' 'Concrete/Proof/BodyIdentity.lean'
formula "implementation digest"  'shortHash ("implementationV1:'   'Concrete/Proof/ImplementationIdentity.lean'

# THE "NOT READ" MARKERS. Registered as formulas because that is what they are: a fact whose spelling
# several surfaces must agree on. Three call sites spelled `no_obligation_record` independently right
# after the R-0479 fix, and a marker whose spelling can drift is one a consumer cannot match on —
# a flat-string fact with several producers, which is the shape docs/verification/EVIDENCE_ARCHITECTURE.md names as
# what typed evidence replaces. Until these become typed, one definition each is the enforceable half.
formula "no-obligation marker"    '"no_obligation_record"' 'Concrete/Proof/ProofCore.lean'
formula "origin-unread marker"    '"origin_unavailable"'   'Concrete/Proof/ProofCore.lean'

# ================================================================================================
# OWNER REGISTRY — definition plus permitted consumers, asserted as an exact set.
# ================================================================================================
echo "=== derived facts have one definition and named consumers ==="

owners() {  # label | fragment | expected set (space separated, in code_refs order)
  local label="$1" frag="$2" want="$3"
  local got; got="$(code_refs "$frag" "${SCOPE[@]}")"
  if [ -z "$got" ]; then
    no "$label: fragment '$frag' matches NO file — stale registry entry, checking nothing"
  elif [ "$got" = "$want " ]; then
    ok "$label: owners are exactly as registered"
  else
    no "$label: owner set changed. expected '$want' got '$got' — if a new consumer is legitimate, add it HERE and say why; if it RECOMPUTES the fact, it is the defect this gate exists to catch"
  fi
}

# The canonical body-digest producer. Report and DependencyEdge must READ it, never re-derive it —
# Report held a second copy of this formula until 2d351a2d. ProofCore reads it for the subject binding
# (bug 079), the same digest the generator computes over each specification's value.
owners "sourceBodyDigestV1Of" 'sourceBodyDigestV1Of' \
  'Concrete/Proof/BodyIdentity.lean Concrete/Proof/DependencyEdge.lean Concrete/Proof/ImplementationIdentity.lean Concrete/Proof/ProofCore.lean Concrete/Report/Report.lean'

# Edge KIND classification. This is the fact `shadowEdgeKinds` had a second, weaker producer for.
owners "classifiedEdgeOf" 'classifiedEdgeOf' \
  'Concrete/Proof/ClassificationTable.lean Concrete/Proof/ProofCore.lean'

# The node/edge set. Both the root builder and the report must read ONE set of nodes.
#
# `Concrete/Proof/Issue.lean` joined on 2026-08-16 and is a CONSUMER, not a second producer: receipt
# issuance needs the dependency root a claim rests on, and it obtains the nodes by CALLING this
# rather than rebuilding a node set of its own. That distinction is the whole point of the gate —
# a receipt computed over a different node set than the status report reads would be evidence about
# a closure nobody else believes in.
owners "dependencyNodesOf" 'dependencyNodesOf' \
  'Concrete/Proof/Issue.lean Concrete/Proof/ProofCore.lean Concrete/Report/Report.lean'

# The currency decision. A second opinion on "is this edge current" is a fail-open waiting to
# happen — this predicate was `| _ => true` once, which made `unclassified` current by default.
#
# `Main.lean` joined the owner set on 2026-08-15 and is a CONSUMER, not a second producer: the
# `concrete check` exit code used to enumerate three statuses by hand (`stale || missing || blocked`)
# and therefore exited 0 on a project holding an unjustified closure — a false green in the command a
# release gate runs, and one that would have repeated for every status added since. It now asks this
# predicate instead of restating a subset of it, which is the direction this gate wants.
owners "isCurrentForDependents" 'isCurrentForDependents' \
  'Concrete/Proof/DependencyEdge.lean Concrete/Proof/DependencyRoot.lean Concrete/Proof/ProofCore.lean Concrete/Report/Report.lean Main.lean'

# ================================================================================================
# R-0479 — A FACT IS NEVER DEFAULTED TO A VALID VALUE OF ITSELF.
#
# Housed here because it is the same family as one-producer: both are ways a fact can be FAKED. But
# it is a distinct failure mode, and the more deceptive one. One-producer catches a second
# computation of a fact; this catches a single producer that did not look and returned a plausible
# in-domain value anyway. Such a value survives every type check and every set-comparison gate,
# because it IS a legal value — it is simply not this one.
#
# The instance that motivated it: `shadowEdgeKinds` returned `unclassified`, a real
# `DependencyEdge`, having consulted no classifier, and so produced a false finding rather than an
# obvious wrong answer. The instance this check found: `Report.lean` had
# `(pc.obligations.find? ...).map (·.status.canonical) |>.getD "missing"`, and `missing` is a real
# canonical status (`.notProved => "missing"`), so "no obligation record" rendered identically to
# "an obligation exists and is missing".
#
# NOTE ON WHAT IS *NOT* A VIOLATION: a COMPUTED branch may legitimately yield a domain value —
# `if dep.isEmpty then ("none", "missing", "")` is a real determination that the obligation is
# missing, not a failed lookup being papered over. This checks `.getD` defaults specifically,
# because that is the failed-lookup shape.
echo "=== no fact is defaulted to a valid value of itself ==="

# Domain values that must never appear as a `.getD` default: canonical obligation statuses and
# dependency-edge kinds. Both sets are small, closed, and meaningful — which is exactly what makes
# a default drawn from them indistinguishable from a real answer.
#
# THIS LIST IS OVER-STRICT BY DESIGN, and the escape hatch is deliberate. The real test is not
# membership in a vocabulary but whether the default ASSERTS something the failed read did not
# establish. Those come apart at the BOTTOM of an assurance ladder: `SourceCorrespondence.missing`
# (see docs/verification/EVIDENCE_ARCHITECTURE.md) means exactly "no correspondence evidence established", so a
# failed read defaulting to it asserts nothing and is fail-closed — legitimate, unlike obligation
# `missing`, which describes an existing record whose proof is absent.
#
# So when the typed-evidence objects land, this check WILL reject a correct default. The fix is an
# explicit allowlist entry naming the type and stating why its ⊥ is safe. NOT deleting the check, and
# NOT renaming the value to evade it. `missing` already means different things in two vocabularies,
# which is why the justification has to be per type rather than per name.
DOMAIN_VALUES="missing proved stale trusted unclassified body contract enforced assumed partial vacuous needs_recheck"
# Via code_refs, because the FIRST run of this check flagged the very comment written to explain the
# fix — the third guard in this suite to confuse prose for code. A comment quoting `.getD "missing"`
# documents the defect; it does not commit it.
viol=0
for v in $DOMAIN_VALUES; do
  hits="$(code_refs ".getD \"$v\"" Concrete/ Main.lean)"
  if [ -n "$hits" ]; then
    no "a fact is defaulted to the domain value '$v', in: $hits"
    viol=$((viol+1))
  fi
done
if [ "$viol" = "0" ]; then
  ok "no .getD default is a canonical status or edge kind (closed vocabulary of 12 checked)"
fi

# NON-VACUITY: the check must actually fire on the shape it exists for. Without this, a typo in the
# grep would read as a clean codebase — the exact failure mode R-0479 is about.
mkdir -p "$TMP/dom"
printf 'def bad : String := (lookup x).map f |>.getD "missing"\n' > "$TMP/dom/Bad.lean"
if [ -n "$(code_refs '.getD "missing"' "$TMP/dom")" ]; then
  ok "the domain-default check fires on a synthetic violation (it is not a typo passing silently)"
else
  no "the domain-default check did NOT fire on a synthetic violation — the pattern is wrong"
fi

# ================================================================================================
# The registry must not be empty of teeth: prove an added second producer would FAIL.
# ================================================================================================
echo "=== the registry would catch a second producer ==="
mkdir -p "$TMP/neg/Concrete"
printf 'def sneaky : String := shortHash (Proof.pexprCanonical (normalizePExpr pe))\n' \
  > "$TMP/neg/Concrete/Sneaky.lean"
neg="$(code_refs 'shortHash (Proof.pexprCanonical' "$TMP/neg")"
if [ "$neg" = "$TMP/neg/Concrete/Sneaky.lean " ]; then
  ok "a duplicated formula in a new file IS detected (the check has teeth)"
else
  no "a duplicated formula was NOT detected (got '$neg') — this gate cannot catch what it exists for"
fi

echo "ONE-PRODUCER: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
