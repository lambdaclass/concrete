#!/usr/bin/env bash
# CONVERGENCE INVENTORY — every known gap, machine-checked.
#
# The roadmap records the remaining R-0004 and multi-prover gaps in prose. Prose cannot
# fail, so a gap that is silently dropped, or silently CLOSED without the inventory being
# updated, leaves no trace. This gate makes each one executable.
#
# Every entry asserts the gap's CURRENT state. A gap that closes FAILS this gate — that is
# the point: closing one must be a deliberate act that updates the inventory, not a side
# effect nobody notices. The failure message says what to do.
#
# This is the same discipline as the quadratic-append ratchet and the struct-literal
# tripwire, applied to the gap list itself.
set -Eeuo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
fatal() { local rc=$?; echo "FATAL: check_convergence_inventory stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2; exit "$rc"; }
trap fatal ERR
PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# --- R-0004: evidence producer -------------------------------------------------------
# 1. CLOSED. The independent accumulator is deleted; the flat identity-use view is derived
#    from the structural evidence body, so there is one producer.
#
#    Deleting it CHANGED the fact, it did not merely relocate it: containment was proven,
#    not equality, and the derived view turned out to be strictly richer. It records an
#    assignment TARGET as a use of a binder and a match pattern's FIELD identity, neither
#    of which the accumulator ever saw. Two gates that pinned the old, lossy numbers had to
#    be updated to assert the recovered identities (check_type_identity 7 -> 10,
#    check_binder_refs 4 -> 5).
#
#    The entry stays here, inverted: the gap is closed and must STAY closed.
if grep -q "bodyIdentityUses" Concrete/Elab/Elab.lean; then
  no "an identity-use accumulator is back in Elab — the flat view must stay DERIVED from the structural body"
else
  ok "CLOSED: one producer of identity uses; the flat view is derived from the tree"
fi

# 2. CLOSED. Shadow mode reports the structural body too. BOTH lines are printed, and both
#    are meant to stay: they answer different questions. `identityUses` digests the flat
#    list of identity references, where `p + 1`, `p * 2` and `p - 9` collide; `bodyV2`
#    digests the structural body, where they do not. Dropping the flat line would remove
#    the comparison the shadow period exists for.
if grep -q "shadow bodyV2" Concrete/Report/Report.lean; then
  ok "CLOSED: shadow mode reports the structural body digest (gated by check_shadow_body_v2)"
else
  no "the structural shadow line is gone from the report — the comparison it enables is how V2 is observed before it is trusted"
fi
if grep -qE "Proof\.shadowIdentityUseDigest " Concrete/Report/Report.lean; then
  ok "the flat identity-use line is still reported alongside it, so the two can be compared"
else
  no "the flat shadow line was removed — without it there is nothing to compare the structural digest against"
fi

# 3. CLOSED by the V2 activation (R-0004 package 3, 2026-08-17). This entry said structural bytes
#    must reach no VERDICT while V1 was the frozen authoritative domain. That period is over:
#    `proofSubjectDigestV2` is now the subject a stored link is compared against, and the whole
#    corpus has been migrated to carry the `v2:` discriminator. Inverted per the pattern entry 1
#    established — the gap is closed and must STAY closed.
#
#    ANCHORED ON CODE, NOT PROSE, which is the specific defect this entry had. Its previous form
#    grepped `bodyBytesV2` across three files, and the ONLY match in ProofCore.lean was a docstring.
#    It therefore reported "a status may now depend on structural bytes" on the evidence of a
#    comment — and it did so at the baseline of this session too, so the inventory had been red, and
#    wrong, for the whole period it was supposed to be governing.
if grep -qE '^ *\(subjectDigestV2 : Option String' Concrete/Proof/ProofCore.lean; then
  ok "CLOSED: the status deriver consumes the V2 subject digest — freshness verdicts derive from it"
else
  no "deriveObligationStatus no longer takes subjectDigestV2 — if freshness stopped consuming V2, the migration has been reverted and this inventory must say so"
fi
# ONE PRODUCER of the v2 comparison. The `v2:` prefix is the discriminator between a v1 and a v2
# stored value, so a second place testing it is a second freshness rule — the defect that put a
# private copy in the consistency checker and made it accuse nine correct `proved` claims.
v2cmp="$(grep -rc '"v2:"' Concrete/Proof/ProofCore.lean || true)"
if [ "${v2cmp:-0}" -le 2 ]; then
  ok "the v2 discriminator is tested in one place ($v2cmp site(s) in ProofCore) — storedFreshness owns it"
else
  no "the v2 discriminator is tested in $v2cmp places in ProofCore — a second comparison is a second freshness rule; route it through storedFreshness"
fi
# THE CORPUS ITSELF, because the code above could be correct while nothing had actually migrated.
# Counted, and the total is asserted non-zero: "no v1 links remain" is vacuously true of a corpus
# with no links at all, and a vacuous pass here would read exactly like a completed migration.
# ALL THREE CORPORA, and this counted only `examples/` until 2026-08-18. That omission was not
# hypothetical: `std` carried 11 stored links, the migration script scanned `examples tests` and
# never touched them, and this leg went green while an entire corpus sat unmigrated on the far side
# of an activation described as atomic. Every std link became `needs_recheck` and the standard
# library reported ZERO proved functions.
#
# The remainder is pinned EXACTLY rather than required to be zero, because ten links genuinely cannot
# migrate: their v2 subject is INCOMPLETE, so no digest is minted and there is nothing to migrate to.
# `--report subject-facts` gives the reason per subject, e.g. `shadow bodyV2: REFUSED (1 gap(s):
# intrinsic cast: target TypeId not minted here)`. That is a real remaining gap in the V2 structural
# producer, and pinning the number is what keeps it from being forgotten or quietly growing.
FP_ALL="$(grep -rhoE 'proof_fingerprint\("[^"]*"\)' examples tests std --include='*.con' || true)"
fp_tot="$(printf '%s\n' "$FP_ALL" | grep -c . || true)"
fp_v1="$(printf '%s\n' "$FP_ALL" | grep -vc '"v2:' || true)"
fp_v2=$(( fp_tot - fp_v1 ))
if [ "${fp_tot:-0}" -eq 0 ]; then
  no "no stored proof links found in examples/, tests/ or std/ — the migration check is vacuous, not satisfied"
# 55 -> 60 on 2026-10-19: the five bug 079 negative subject-binding fixtures (tests/programs/spec_in_*),
# each a v2 stored link explained as an added identity row.
# 60 -> 80 v2: bug 083 (2026-10-18) added 20 v2 pins when the name-keyed `provedFunctions` table's
# claims moved into source — 7 in examples/, 13 in tests/programs/.
# 80 -> 85 v2: bug 081's tests/programs/bug081_alias_key pins its five elf_header claims in source.
# 85 -> 88 v2: bug 089's ens_coverage_only, ens_missing_with_proof_by and ens_unrelated_theorem
# fixtures (tests/regressions/bug089_ensures_proof) each pin their compute_tag claim.
# 88 -> 95 v2 (2026-10-21): the seven workload-proof links (toml, hexdump, png_chunks) were written
# with v2 fingerprints from the start; v1 is unchanged.
elif [ "$fp_v2" -eq 95 ] && [ "$fp_v1" -eq 10 ]; then
  ok "CLOSED: $fp_v2 of $fp_tot stored links carry the v2 discriminator; the $fp_v1 that do not have no computable v2 subject"
else
  no "stored-link migration moved: $fp_v2 v2 / $fp_v1 v1 of $fp_tot (expected 95/10). If a link became migratable, migrate it; if one regressed to v1, the activation is being undone"
fi

# 4. constRef names a constant without binding its MEANING.
if grep -qE "unboundConsts : List ConstId" Concrete/Proof/EvidenceTree.lean; then
  ok "GAP OPEN: constant dependency binding is still an unmet obligation (unboundConsts)"
else
  no "unboundConsts is gone — if ConstId now binds to an initializer digest, record it"
fi

# 4a. TRIPWIRE for what that gap COSTS. The entry above states an unmet obligation; this
#     one states the consequence, executably.
#
#     A body referencing `LIMIT` records the ConstId and nothing about its value, so
#     changing `const LIMIT: Int = 10` to `= 99` moves neither the body digest nor the
#     subject digest. A proof about that function stays valid-looking after the constant
#     it depends on changes — freshness failing through the dependency axis.
#
#     Asserts the CURRENT verdict, and note WHICH digest: `subjectDigest` is
#     `proofSubjectDigestV2` and already drives freshness — it is not the frozen V1 body
#     fingerprint. So step 3 must NOT move it. Dependency material lands in shadow first,
#     is measured against the corpus, and only becomes authoritative at the step-5
#     migration; moving this digest earlier strands the stored proof links.
if [ -x .lake/build/bin/concrete ]; then
  TD_CONST="$(mktemp -d)"
  printf 'mod m { const LIMIT: Int = 10;\n  pub fn f(p: Int) -> Bool { return p < LIMIT; } }\n' > "$TD_CONST/a.con"
  printf 'mod m { const LIMIT: Int = 99;\n  pub fn f(p: Int) -> Bool { return p < LIMIT; } }\n' > "$TD_CONST/b.con"
  cdig() { .lake/build/bin/concrete "$1" --report subject-facts 2>/dev/null \
             | grep "subject digest:" | head -1 | sed 's/^ *subject digest: //' || true; }
  da="$(cdig "$TD_CONST/a.con")"; db="$(cdig "$TD_CONST/b.con")"
  rm -rf "$TD_CONST"
  if [ -z "$da" ] || [ -z "$db" ]; then
    no "the constant-freshness tripwire produced no digest — inconclusive, not a verdict"
  elif [ "$da" = "$db" ]; then
    ok "TRIPWIRE: a constant's VALUE does not reach the subject digest (LIMIT 10 vs 99 collide)"
  else
    no "a constant's value now moves the SUBJECT DIGEST. That digest drives freshness verdicts, so moving it invalidates every stored proof link — which is what the step-5 migration exists to sequence. If dependency binding has landed, it must first appear as a SHADOW line (like bodyBytesV2) reaching no verdict, and this tripwire converts to assert THAT. If the subject digest moved before the migration, revert and re-land it in shadow."
  fi
else
  no "no built compiler; the constant-freshness tripwire did NOT run"
fi

# --- R-0004: open language decision --------------------------------------------------
# 5. Struct-literal initializer evaluation order. STILL UNDECIDED, and the entry now says
#    what is actually true about it.
#
#    The producer was believed to REFUSE struct literals until this was decided —
#    `EvidenceBuild.evStructLitPending` existed and its docstring said so. It was never
#    called. The producer emits a real `structLit` ordered by the DECLARATION list, so the
#    undecided order has been in the shadow bytes the whole time: the outcome the refusal
#    was written to prevent, guarded by a comment instead of by behaviour.
#
#    The bytes are accurate to today's semantics and stay. What must not happen is the
#    order becoming AUTHORITATIVE while undecided, so the close condition is ratification
#    before the fingerprint migration — asserted below rather than trusted to prose.
# 5-CLOSED (2026-08-08). The close condition this entry stated was RATIFICATION before the
#    fingerprint migration, and that is what happened: the matrix records "struct-literal
#    initializers evaluate in SOURCE order". The entry was never converted when its own condition
#    was met, so it kept demanding an OPEN section that had correctly become a ratified one — the
#    gate failing for the one reason it was designed to treat as success.
if grep -q "RATIFIED: struct-literal initializers evaluate in SOURCE order" docs/verification/EVIDENCE_PRODUCER_MATRIX.md; then
  ok "CLOSED: struct-literal initializer evaluation order is RATIFIED (source order, 2026-08-08)"
else
  no "the struct-literal ratification is gone from the matrix — the order is now authoritative in freshness verdicts, so removing its ratification leaves an undecided rule deciding proofs"
fi

# 5a. The refusal must not be reintroduced as a comment-only guard. Either the producer
#     refuses struct literals in BEHAVIOUR, or it does not claim to.
# Matches a DEFINITION, not any mention: the comment above the removal names the symbol to
# explain why it went, and a bare name match flagged that prose as the thing it forbids.
if grep -qE "^ *def evStructLitPending" Concrete/Proof/EvidenceBuild.lean; then
  no "evStructLitPending is back — if struct literals are refused, the producer must CALL it; a definition nothing calls is a guard that does not guard"
else
  ok "no comment-only struct-literal refusal: the producer's behaviour and its documentation agree"
fi

# 5b. THE CLOSE CONDITION, KEPT AS AN IMPLICATION rather than retired. It was written for the
#     pre-migration world ("do not let unratified bytes become authoritative"), and simply deleting
#     it now would discard the guarantee at the moment it started to matter: the bytes ARE
#     authoritative, so the ratification is load-bearing — removing it would leave an undecided
#     evaluation order deciding real freshness verdicts.
#
#     So the direction is inverted to match: IF the subject digest is authoritative, THEN the order
#     must be ratified. That keeps failing for the right reason in the world we are actually in,
#     instead of passing because its first conjunct went false.
if grep -qE '^ *\(subjectDigestV2 : Option String' Concrete/Proof/ProofCore.lean; then
  if grep -q "RATIFIED: struct-literal initializers evaluate in SOURCE order" docs/verification/EVIDENCE_PRODUCER_MATRIX.md; then
    ok "the authoritative subject digest rests on a RATIFIED struct-literal evaluation order"
  else
    no "structural bytes are authoritative but the struct-literal evaluation order is not ratified — an undecided rule is deciding freshness verdicts"
  fi
else
  ok "the subject digest is not authoritative, so the struct-literal order is not load-bearing"
fi

# --- R-0004: freshness, migration, receipts ------------------------------------------
# 6. CLOSED. Bugs 059 (the subject omits declared types) and 060 (contracts sit outside it) were the
#    two defects the V2 activation existed to fix, and they are fixed: a whole-signature `i32 -> u32`
#    change and an added `#[ensures]` both stale the claim now. Verified BEHAVIOURALLY by
#    check_proof_freshness.sh, whose two tripwires fired — they asserted the wrong verdict on
#    purpose while the defects were open — and were inverted into `059 CLOSED` / `060 CLOSED`.
#
#    THIS ENTRY WAS A CONTRADICTORY LEDGER, and it passed while being one: it asserted the corpus
#    still recorded both bugs OPEN, so it went green precisely because nobody had updated the record
#    after the fix landed. A gate that passes by confirming a stale claim is worse than one that
#    fails, because it certifies the staleness.
#    Since the ledger migration (R-0542) the status has one owner, the record's **Status:** line, and
#    the corpus is generated from the records (`bug_ledger.py corpus`: NNN, kind, open|fixed, ...);
#    this leg reads that, not the emptied legacy map in audit_bug_corpus.sh. The corpus is captured
#    first, and an unreadable or empty corpus fails the leg rather than passing it.
ledger_corpus="$(python3 -I scripts/gen/bug_ledger.py corpus 2>/dev/null)" || ledger_corpus=""
for b in 059 060; do
  st="$(awk -F'\t' -v b="$b" '$1 == b { print $3 }' <<<"$ledger_corpus")"
  if [ "$st" = fixed ]; then
    ok "CLOSED: bug $b is recorded FIXED — the V2 subject binds what the body-only fingerprint could not"
  elif [ -z "$ledger_corpus" ] || [ -z "$st" ]; then
    no "bug $b has no row in the generated bug corpus (scripts/gen/bug_ledger.py corpus) — cannot confirm the record agrees with check_proof_freshness.sh"
  else
    no "bug $b is not recorded FIXED in the corpus, but check_proof_freshness.sh asserts its closure — the corpus and the executable evidence disagree"
  fi
done
# AND THE EXECUTABLE SIDE OF THE SAME CLAIM. A corpus line is prose; it is only worth anything while
# a live leg holds the behaviour. Anchored on the leg labels so deleting the assertions is caught
# here rather than silently leaving two bugs marked FIXED with nothing testing them.
for leg in '059 CLOSED' '060 CLOSED'; do
  if grep -q "$leg" scripts/tests/check_proof_freshness.sh; then
    ok "the '$leg' regression leg still exists to hold it closed"
  else
    no "the '$leg' leg is gone from check_proof_freshness.sh — the corpus claims FIXED with nothing asserting it"
  fi
done

# 7. CONVERTED. V1 is no longer the authoritative domain — V2 is, per entry 3 — but the V1 golden
#    keeps a different and still-useful job: proving the V1 body-fingerprint corpus did NOT move
#    while the authority changed underneath it. A migration that silently perturbed the values it
#    was migrating FROM could not be distinguished from one that migrated them correctly.
#
#    STDERR IS NO LONGER DISCARDED. This read only stdout, and the golden prints its FAIL line to
#    stderr — so a failing sub-gate arrived here as an empty string and was reported as "did not
#    report — inconclusive". The real failure (a denominator that had moved) was invisible for as
#    long as it took to run the sub-gate by hand. Silence must never be a gate's evidence.
v1="$(bash scripts/tests/check_v1_fingerprint_golden.sh 2>&1 | tail -1 || true)"
case "$v1" in
  *"PASS (77 extracted"*) ok "CLOSED: the frozen V1 corpus is unmoved under V2 authority ($v1)" ;;
  "") no "the V1 golden produced no output at all — inconclusive, not progress" ;;
  *) no "the V1 golden did not pass with 77 extracted: '$v1'. Either the V1 corpus moved during the V2 migration, or the golden itself is failing — run scripts/tests/check_v1_fingerprint_golden.sh directly" ;;
esac

# --- multi-prover: post-R-0004 --------------------------------------------------------
# 8. The kernel statuses must stay non-authoritative until the subject-digest cross-check
#    exists. The spike's own comment records the gap; assert it is still stated.
# Matched on a fragment that does not span the line break: the full phrase
# "there is no subject-digest cross-check" wraps in the source, and my first pattern
# failed on that rather than on a missing caveat.
if grep -q "there is no subject-digest" Concrete/Proof/ObligationCore.lean; then
  ok "GAP OPEN: multi-kernel statuses attest checker diversity, not proposition agreement"
else
  no "the subject-digest cross-check caveat is gone from ObligationCore — if the cross-check now EXISTS, say so; if the comment was merely deleted, restore it"
fi

# 9. The post-R-0004 multi-prover list must stay recorded. Since 2026-10-19 it lives in the roadmap
#    history archive, and its open items are owned by roadmap tasks (`solver_error` is R-0520).
HIST=docs/archive/ROADMAP_HISTORY.md
if grep -q "Multi-prover work that remains AFTER R-0004" ROADMAP.md "$HIST" 2>/dev/null \
   && grep -q '^### Task R-0520$' docs/project/ROADMAP_TASKS.md docs/project/ROADMAP_BACKLOG.md; then
  ok "GAP OPEN: the post-R-0004 multi-prover work is recorded (roadmap history) and owned (R-0520)"
else
  no "the post-R-0004 multi-prover section is missing from ROADMAP.md and $HIST, or its owner R-0520 is gone"
fi

# 10. The spike pin must be a fixed SHA, never a branch name.
if grep -qE "spike/multi-prover-evidence @ 80be5368|pin \`?80be5368" ROADMAP.md "$HIST" 2>/dev/null; then
  ok "the spike is pinned to an immutable SHA, not a moving head"
else
  no "the pinned spike SHA is absent from ROADMAP.md and $HIST — later deltas must integrate as new PINNED epochs"
fi

# 11. `solver_error` is never produced. Three independent sites expected it — absent
#     solver, garbage solver, and the negatives gate — and all three get `unproven`. Every
#     case is FAIL-CLOSED (no false proof, asserted hard at each site), but a consumer
#     cannot distinguish "the solver misbehaved or was missing" from "not proved", which
#     are different facts: one is an environment defect, the other a proof-difficulty
#     statement.
if grep -q "TRACKED: absent solver reports" scripts/tests/check_smt_path.sh; then
  ok "GAP OPEN: solver_error is never produced; absent/garbage solver reports unproven"
else
  no "the solver_error tracking leg is gone — if the diagnostic landed, restore the strict assertions at all three sites"
fi

echo "CONVERGENCE-INVENTORY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
