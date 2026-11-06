#!/usr/bin/env bash
# R-0004: corpus-wide inventory of subject-facts coverage, grouped by cause.
#
# A subject digest is only produced when a declaration's contracts are fully
# encodable. An UNCOVERED declaration is fail-closed and correct, but it also
# cannot participate in V2 freshness: replay can never establish a V2 claim for
# it. So before freshness is activated authoritatively, the uncovered set must be
# a known, named inventory rather than a number nobody has looked at.
#
# This gate pins that inventory. It fails when coverage REGRESSES (a new uncovered
# declaration appears) and equally when it silently IMPROVES, because a drop must
# come with a deliberate update naming which cause was closed.
set -Eeuo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fatal() {
  local rc=$?
  echo "FATAL: check_subject_coverage stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2
  exit "$rc"
}
trap fatal ERR

CC=".lake/build/bin/concrete"
[ -x "$CC" ] || { echo "error: build first" >&2; exit 2; }

PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Measured 2026-08-03 over examples/; covered count updated when the ghost cause closed. The proof corpus, not the whole
# test tree: tests/programs/ holds deliberate uncovered probes whose whole purpose
# is to be uncovered, and mixing them in would hide a real regression in the noise.
EXPECT_COVERED=419
# 1 -> 0 on 2026-08-07: the last uncovered case (invalid_contract_expression) stopped
# being one when contract type-checking landed and rejected it before subject
# construction. See the cause notes below. ZERO is now the assertion — every declaration
# the corpus reaches produces a covered subject — which is a stronger claim than the count
# it replaces, and it fails on any RISE.
EXPECT_UNCOVERED=0

# The uncovered set, by cause. Both causes are OPEN work, tracked here because a
# bare count would not say what closing them requires.
#
#   const  — CLOSED. A module constant now encodes as `k:<ConstId>=<initializer>`:
#            semantic identity so it cannot be confused with a same-spelled
#            function, AND its value so that changing `const LIMIT = 16` to `= 32`
#            invalidates dependent subjects. LOCAL constants only — ConstSummary
#            carries neither an initializer nor a defining module, so an IMPORTED
#            constant has neither half and still fails closed.
#   ghost  — CLOSED. Ghost bindings now form their own binder frame, collected
#            from the AST in source order and rendered `h:<i>`, so a contract may
#            name them without going uncovered. Both ghost programs are covered as
#            of this change; the cause is retained here as a record of what the
#            count used to include.
#   invalid — a deliberate negative example; uncovered WAS the correct verdict.
#
#             CAUSE CLOSED 2026-08-07 by the spike/multi-kernel-theories merge. Contracts
#             are now TYPE-CHECKED, so `invalid_contract_expression` is rejected during
#             check ("contract on 'bad': unknown identifier 'nonexistent'") and never
#             becomes a subject at all. A program that does not reach subject construction
#             cannot be uncovered; the fixture still earns its keep as a contract negative,
#             it just stopped being a COVERAGE case.
#
#             This gate failed on the DROP rather than passing quietly, which is the whole
#             point of asserting an exact count: a cause closing has to be recorded
#             deliberately, or the inventory drifts from why the number is what it is.
EXPECT_CONST=""   # cause closed for local constants; see the note above
EXPECT_GHOST=""   # cause closed; see the note above
EXPECT_INVALID="" # cause closed by contract type-checking; see the note above

covered=0; uncovered=0; offenders=""
while IFS= read -r f; do
  out="$("$CC" "$f" --report subject-facts 2>/dev/null)" || continue
  c="$(printf '%s\n' "$out" | grep -c "contracts covered: true"  || true)"
  u="$(printf '%s\n' "$out" | grep -c "contracts covered: false" || true)"
  covered=$((covered+c)); uncovered=$((uncovered+u))
  [ "$u" -gt 0 ] && offenders="$offenders $f"
done <<< "$(find examples -name '*.con' | sort)"

# Vacuity control FIRST. A zero-uncovered result is indistinguishable from a
# report that stopped emitting the field at all, and that exact shape has produced
# a false pass in this repo before.
if [ "$covered" -gt 0 ]; then
  ok "the coverage field is present ($covered covered declarations observed)"
else
  no "no covered declarations seen at all — the report field may have moved; every other leg here would be vacuous"
fi

if [ "$uncovered" -eq "$EXPECT_UNCOVERED" ]; then
  ok "uncovered declarations = $EXPECT_UNCOVERED, matching the recorded inventory"
else
  no "uncovered = $uncovered, expected $EXPECT_UNCOVERED. A RISE is a coverage regression; a DROP means a cause was closed and this inventory plus its cause notes must be updated deliberately. Offenders:$offenders"
fi

if [ "$covered" -eq "$EXPECT_COVERED" ]; then
  ok "covered declarations = $EXPECT_COVERED"
else
  echo "  note covered = $covered (recorded $EXPECT_COVERED); corpus size changes are expected as examples are added"
  PASS=$((PASS+1))
fi

for f in $EXPECT_CONST $EXPECT_GHOST $EXPECT_INVALID; do
  if has_match -F "$f" <<<"$offenders"; then
    ok "known uncovered program still accounted for: $f"
  else
    no "$f is no longer uncovered — if its cause was closed, update the inventory and cause notes here"
  fi
done

for f in $offenders; do
  case " $EXPECT_CONST $EXPECT_GHOST $EXPECT_INVALID " in
    *" $f "*) ;;
    *) no "UNACCOUNTED uncovered program: $f — a declaration lost its subject digest and no recorded cause explains it" ;;
  esac
done
[ -n "$offenders" ] && ok "every uncovered program maps to a recorded cause"

# The ghost frame's whole claim is that a ghost binding is a BINDER, so renaming it
# must not move the subject digest — otherwise `h:<i>` would just be a source name
# with extra steps. Derived from a real example rather than a synthetic one: a
# hand-written probe produced "0 entries" and would have compared two empty strings
# to each other and called that invariance.
GT="$(mktemp -d)"; trap 'rm -rf "$GT"' EXIT
cp examples/ghost_state/src/main.con "$GT/a.con"
sed 's/\bbound\b/snapshot/g' "$GT/a.con" > "$GT/b.con"
ga="$("$CC" "$GT/a.con" --report subject-facts 2>/dev/null | grep -oE 'subject digest: [a-f0-9]+' || true)"
gb="$("$CC" "$GT/b.con" --report subject-facts 2>/dev/null | grep -oE 'subject digest: [a-f0-9]+' || true)"
if [ -z "$ga" ] || [ -z "$gb" ]; then
  no "ghost-rename probe produced no digest — inconclusive, not invariant"
elif [ "$ga" = "$gb" ]; then
  ok "renaming a ghost binder does not move the subject digest"
else
  no "a ghost binder's NAME reaches the digest ($ga vs $gb) — the frame is not alpha-invariant"
fi
# Identity WITHOUT meaning is not enough. A ghost's position is what the contract
# refers to, but its VALUE is part of what the contract asserts: `i <= bound` with
# `bound = 8` is a different claim than with `= 9`. Before ghost initializers were
# encoded, those two produced byte-identical subjects — a hole the alpha-invariance
# leg above could never catch, because both properties must hold AT ONCE.
sed 's/ghost let bound: i32 = 8;/ghost let bound: i32 = 9;/' "$GT/a.con" > "$GT/c.con"
if diff -q "$GT/a.con" "$GT/c.con" >/dev/null 2>&1; then
  no "the initializer probe did not change the program — it would pass vacuously"
else
  ok "the initializer probe genuinely changed the initializer"
fi
gc="$("$CC" "$GT/c.con" --report subject-facts 2>/dev/null | grep -oE 'subject digest: [a-f0-9]+' || true)"
if [ -z "$gc" ]; then
  no "initializer probe produced no digest — inconclusive"
elif [ "$ga" != "$gc" ]; then
  ok "changing a ghost initializer moves the subject digest"
else
  no "a ghost initializer change leaves the digest identical — the subject is blind to what the contract asserts"
fi

if grep -q "bound" "$GT/b.con"; then
  no "the rename probe did not actually rename anything — it would pass vacuously"
else
  ok "the rename probe genuinely changed the program text"
fi

echo "SUBJECT-COVERAGE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
