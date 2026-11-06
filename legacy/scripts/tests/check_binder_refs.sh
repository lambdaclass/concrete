#!/usr/bin/env bash
# R-0004: binder references emitted during elaboration, as RELATIVE lexical
# positions.
#
# Three forcing properties, because each is satisfiable by a broken producer that
# passes the other two:
#   1. A scope that binds NOTHING must not shift framesOut.
#   2. Binding TIMING is construct-specific — lets, loop variables and arm patterns
#      are not interchangeable, and testing only lets proves nothing about arms.
#   3. A construct that cannot be given a semantic identity must mark the subject
#      UNCOVERED, never silently emit nothing.
set -Eeuo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fatal() {
  local rc=$?
  echo "FATAL: check_binder_refs stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2
  exit "$rc"
}
trap fatal ERR

PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

cat > "$TMP_DIR/p.lean" <<'LEAN'
import Concrete
open Concrete Concrete.Proof

-- Bug 207: a failed assertion PRINTS its verdict and the probe goes on. It used to throw, so
-- the first failure aborted the probe, the shell stopped at FATAL, and the gate never printed
-- its verdict line: the mutation harness read every family this gate kills as "no verdict".
def a (label : String) (c : Bool) : IO Unit :=
  IO.println ((if c then "  ok   " else "  FAIL ") ++ label)

def facts (s : String) : Option ProofBodyIdentityInputsV2 :=
  match (do
    let parsed ← Pipeline.parse s
    let summary := Pipeline.buildSummary parsed
    let resolved ← Pipeline.resolve parsed summary
    Pipeline.check resolved summary
    let el ← Pipeline.elaborate resolved summary
    pure el.coreModules : Except Diagnostics (List CModule)) with
  | .error _ => none
  | .ok ms => (ms.head?.bind (·.declFacts.head?)).map (·.bodyIdentityInputs)

def binders (s : String) : List BodyIdentityUse :=
  (((facts s).map ProofBodyIdentityInputsV2.uses).getD []).filter fun u =>
    match u with | .binderRef _ _ => true | _ => false

def noBind : String :=
  "mod m { pub fn f(p: Int) -> Int { if p > 0 { return p; } return p; } }"
-- Bug 268: a scope that ENTERS a frame scope (a match arm calls openScope) yet binds nothing. The
-- `if` above never enters one, so an eager frame (pushed on scope entry rather than on the first
-- binder) passed CHECK 1; here it would put the reference to `p` inside the arm that binds nothing one frame out.
def noBindArm : String :=
  "mod m {
     enum Copy E { A { v: Int }, B { v: Int } }
     pub fn f(e: E, p: Int) -> Int { match e { E::A { v } => { return v; }, E::B { } => { return p; } } }
   }"
def withLet : String :=
  "mod m { pub fn f(p: Int) -> Int { let q: Int = p; return q; } }"
def renamed : String :=
  "mod m { pub fn f(p: Int) -> Int { let zzz: Int = p; return zzz; } }"
def loopVar : String :=
  "mod m { pub fn f(p: Int) -> Int { let mut i: Int = 0; while i < p { i = i + 1; } return i; } }"
-- The DISCRIMINATOR for arm frames: from inside an arm, reach the outer parameter.
-- Without this, an arm binding at (0,0) is indistinguishable from a binding that
-- landed in the enclosing frame — the ambiguity that hid the missing push at first.
def outerFromArm : String :=
  "mod m {
     enum Copy E { A { v: Int }, B { v: Int } }
     pub fn f(e: E, p: Int) -> Int { match e { E::A { v } => { return p; }, E::B { v } => { return v; } } }
   }"
-- Bug 284: a match whose scrutinee is not an enum. Its arms reached Core and NOT evidence, so the
-- evidence held `match k {}` and these references were absent from the body, its digest and the
-- implementation identity. Literal, variable, range and Bool arms are each a separate producer
-- path, so each has a program; `litArm2` differs from `litArm` only inside an arm body.
def litArm : String :=
  "mod m { pub fn f(k: Int, p: Int) -> Int { match k { 0 => { return p; }, _ => { return k; } } } }"
def litArm2 : String :=
  "mod m { pub fn f(k: Int, p: Int) -> Int { match k { 0 => { return k; }, _ => { return k; } } } }"
def varArmInt : String :=
  "mod m { pub fn f(k: Int, p: Int) -> Int { match k { 0 => { return 1; }, n => { return n + p; } } } }"
def rangeArm : String :=
  "mod m { pub fn f(k: Int, p: Int) -> Int { match k { 0..10 => { return p; }, _ => { return 0; } } } }"
def boolArm : String :=
  "mod m { pub fn f(b: Bool, p: Int) -> Int { match b { true => { return p; }, false => { return 0; } } } }"
def negArm : String :=
  "mod m { pub fn f(k: Int, p: Int) -> Int { match k { -1 => { return p; }, _ => { return 0; } } } }"

def bodyBytes (s : String) : Option String :=
  match (do
    let parsed ← Pipeline.parse s
    let summary := Pipeline.buildSummary parsed
    let resolved ← Pipeline.resolve parsed summary
    Pipeline.check resolved summary
    let el ← Pipeline.elaborate resolved summary
    pure el.coreModules : Except Diagnostics (List CModule)) with
  | .error _ => none
  | .ok ms => ms.head?.bind fun m => m.evidenceBodies.head?.bind fun (_, d) =>
      match validate d with | .ok c => some (bodyBytesV2 c) | .error _ => none

#eval do
  -- Sanity FIRST: an empty binder list would make every leg below vacuous.
  a "the producer emits binder references at all"
    ((binders withLet).length > 0)

  -- CHECK 1. Every reference stays at framesOut 0: the `if` binds nothing, so it
  -- materializes no frame. An eager push-per-block would report 1 here and move
  -- digests for a semantically irrelevant edit.
  a "a scope that binds nothing does not shift framesOut"
    ((binders noBind).all fun u => match u with
      | .binderRef out _ => out == 0
      | _ => true)

  -- CHECK 1b. The same for a match arm that binds nothing (bug 268): its scope is entered
  -- (openScope) but must materialize no frame, so `p` stays in the function frame (0 frames
  -- out, index 1) inside `E::B { }`. The scrutinee `e` is (0, 0), and `v` in the arm that binds
  -- it is (0, 0) in that arm's own frame, so the leg pins all three positions.
  a "a match arm that binds nothing does not shift framesOut"
    (binders noBindArm == [BodyIdentityUse.binderRef 0 0, BodyIdentityUse.binderRef 0 0,
                           BodyIdentityUse.binderRef 0 1])

  -- CHECK 2a. Progressive indexing: the let is a LATER binder than the parameter.
  a "a let binds after the parameters, in source order"
    (binders withLet == [BodyIdentityUse.binderRef 0 0, BodyIdentityUse.binderRef 0 1])
  a "renaming the local leaves the positions identical"
    (binders withLet == binders renamed)
  a "the rename probe genuinely changed the program"
    (withLet != renamed)

  -- CHECK 2b. A loop variable is an ordinary binder of its enclosing block, and is
  -- in scope in the loop CONDITION — a different timing answer than a let's own
  -- initializer, which is why lets alone prove nothing here.
  -- Five references, in evaluation order: `i` and `p` in the condition, `i` as the
  -- ASSIGNMENT PLACE, `i` in the right-hand side, `i` in the return.
  --
  -- Four until the flat view became derived from the structural body. The accumulator
  -- never recorded an assignment TARGET, so `i = i + 1` and `j = i + 1` produced the
  -- same flat view — the destination of a write was not a use of a binder. Position 2
  -- below is that recovered reference, which is why this asserts the exact sequence
  -- rather than a count: a count of 5 would also pass if the place were recorded and
  -- the return dropped.
  a "a loop variable is in scope in the condition and the body"
    (binders loopVar == [BodyIdentityUse.binderRef 0 1, BodyIdentityUse.binderRef 0 0,
                         BodyIdentityUse.binderRef 0 1, BodyIdentityUse.binderRef 0 1,
                         BodyIdentityUse.binderRef 0 1])

  -- CHECK 2c. Arm patterns open their OWN frame, proved by crossing outward.
  a "an arm pattern opens a frame — an outer binder is one frame out"
    (binders outerFromArm == [BodyIdentityUse.binderRef 0 0,
      BodyIdentityUse.binderRef 1 1, BodyIdentityUse.binderRef 0 0])
  a "arm bodies stay covered"
    (((facts outerFromArm).map ProofBodyIdentityInputsV2.covered) == some true)

  -- CHECK 4 (bug 284). Every arm of a non-enum match reaches evidence. A literal arm binds
  -- nothing, so `p` inside it is (0, 1) and the `_` arm's `k` is (0, 0); before the fix only the
  -- scrutinee's (0, 0) was present.
  a "a literal arm's body references reach evidence"
    (binders litArm == [BodyIdentityUse.binderRef 0 0, BodyIdentityUse.binderRef 0 1,
                        BodyIdentityUse.binderRef 0 0])
  -- A variable arm over an Int binds `n` in its own frame: `n` is (0, 0), `p` one frame out.
  a "a variable arm over an integer opens a frame and its body reaches evidence"
    (binders varArmInt == [BodyIdentityUse.binderRef 0 0, BodyIdentityUse.binderRef 0 0,
                           BodyIdentityUse.binderRef 1 1])
  a "a range arm's body references reach evidence"
    (binders rangeArm == [BodyIdentityUse.binderRef 0 0, BodyIdentityUse.binderRef 0 1])
  a "a Bool arm's body references reach evidence"
    (binders boolArm == [BodyIdentityUse.binderRef 0 0, BodyIdentityUse.binderRef 0 1])
  -- A negative literal arm is the integer pattern of its value, not a gap.
  a "a negative literal arm is described, not refused"
    ((bodyBytes negArm).isSome && (binders negArm).length == 2)
  -- THE DIGEST LEG: two bodies that differ only inside a literal arm must not share body bytes,
  -- which is what let a v2 pin and an implementation identity read current after the edit.
  a "a body edit inside a literal arm changes the body bytes"
    ((bodyBytes litArm).isSome && (bodyBytes litArm2).isSome && bodyBytes litArm != bodyBytes litArm2)
LEAN

OUT="$TMP_DIR/out.txt"
# Every outcome of the probe is a counted verdict and the gate always reaches its summary line
# (bug 207): a probe that does not elaborate, emits a diagnostic, or fails an assertion is a
# FAIL here, never an exit before the verdict.
PRC=0; lake env lean "$TMP_DIR/p.lean" > "$OUT" 2>&1 || PRC=$?
if [ "$PRC" -ne 0 ]; then
  no "binder-ref probe did not elaborate (rc=$PRC): $(grep -m3 -E 'error' "$OUT" | tr '\n' ' ' | cut -c1-400)"
elif grep -qE "error|sorry" "$OUT"; then
  no "probe emitted a diagnostic: $(grep -m3 -E 'error|sorry' "$OUT" | tr '\n' ' ' | cut -c1-400)"
fi
cat "$OUT"
NOK="$(grep -c '^  ok   ' "$OUT" || true)"; NFL="$(grep -c '^  FAIL ' "$OUT" || true)"
PASS=$(( PASS + NOK )); FAIL=$(( FAIL + NFL ))
# The probe's own assertion count is pinned: a probe that stops early (or loses legs) prints
# fewer verdicts, and that is a failure, not a smaller pass.
if [ "$PRC" -eq 0 ] && [ $(( NOK + NFL )) -ne 15 ]; then
  no "the probe printed $(( NOK + NFL )) verdicts, expected 15"
fi

# CHECK 4, end to end (bug 284): the regression fixture and a copy that differs only inside one
# literal arm's body must report different subject digests. Before the fix both read the same
# digest, so a `#[proof_fingerprint("v2:...")]` pinned on one stayed current on the other.
B284="tests/regressions/bug284_match_arm_evidence_dropped/repro.con"
if [ -f "$B284" ]; then
  sed 's/0 => p + 1,/0 => p * 9,/' "$B284" > "$TMP_DIR/bug284_edited.con"
  if cmp -s "$B284" "$TMP_DIR/bug284_edited.con"; then
    no "bug 284: the arm edit did not change the fixture (its arm text moved)"
  else
    d284a="$(.lake/build/bin/concrete "$B284" --report subject-facts 2>&1 | grep -A12 '^v1:user:main.pick$' | sed -n 's/^  subject digest: //p' | head -1)" || true
    d284b="$(.lake/build/bin/concrete "$TMP_DIR/bug284_edited.con" --report subject-facts 2>&1 | grep -A12 '^v1:user:main.pick$' | sed -n 's/^  subject digest: //p' | head -1)" || true
    if [[ "$d284a" =~ ^[0-9a-f]{32}$ ]] && [[ "$d284b" =~ ^[0-9a-f]{32}$ ]] && [ "$d284a" != "$d284b" ]; then
      ok "bug 284: an edit inside a literal arm moves the subject digest"
    else
      no "bug 284: subject digests '$d284a' and '$d284b' (want two different digests)"
    fi
  fi
else
  no "bug 284: regression fixture $B284 is missing"
fi
# Structurally: a Core arm cannot be accumulated apart from its evidence. The two separate lists
# are what let three branches push Core alone.
if grep -q "cArms := cArms ++" Concrete/Elab/Elab.lean; then
  no "bug 284: Elab accumulates Core match arms apart from their evidence"
else
  ok "bug 284: each match arm's Core and evidence are pushed as one entry"
fi

# CHECK 3, structurally: the local-reference path must not be able to emit nothing.
# Every scope-resolution failure has to reach markBodyIdentityUncovered, or a binder
# the frame stack cannot place would silently vanish from the body — an
# under-approximated subject, which is worse than an uncovered one because it looks
# complete.
# Searched DIRECTLY, not via an awk range. These legs used `awk '/comment/,/return \.ident/'`
# and the cutover changed `return .ident` to `return ElaboratedExprV2.mk (CExpr.ident ...)`,
# so the range's end pattern stopped matching. An unterminated awk range runs to EOF, which
# happened to still contain both strings on macOS and did not on CI's awk — so the gate
# passed locally and failed remotely on the same SHA. A range whose end can be edited away
# is a false-pass waiting to happen; the exact code shapes are the durable thing to assert.
if grep -q "markBodyIdentityUncovered" Concrete/Elab/Elab.lean; then
  ok "an unplaceable local marks the subject uncovered rather than emitting nothing"
else
  no "the local-reference path has no uncovered branch — a missing binder would vanish silently"
fi
# Repointed when the accumulator was deleted. It asserted the ACCUMULATOR call, so it was
# checking the side channel rather than the node that reaches evidence; the structural
# builder is the one producer now, and the behavioural legs above already pin the positions
# it mints.
if grep -q "(CExpr.ident b.core b.ty) (← binderRefEv b)" Concrete/Elab/Elab.lean \
   && grep -q "Proof.evBinderRefAt (depth - 1 - frame) idx" Concrete/Elab/Elab.lean; then
  ok "a placeable local emits a typed binderRef node via the structural builder"
else
  no "the local-reference path does not emit a binderRef"
fi

# Frames must open lazily. An eager push would satisfy every positional leg above
# while breaking CHECK 1, so assert the mechanism, not only its outputs.
if grep -q "pendingFrame" Concrete/Elab/Elab.lean; then
  ok "frames open lazily via pendingFrame, so empty scopes cost nothing"
else
  no "no lazy-frame mechanism — a non-binding scope would shift framesOut"
fi

# ONE PRODUCER. Every semantic child edge inside the evidence-producing mutual block
# must call elabExprEv; a Core-only projection there would silently discard the child's
# evidence and still compile, so the count is asserted rather than trusted.
blk="$(python3 - <<'PYEOF'
lines=open('Concrete/Elab/Elab.lean').read().split('\n')
s=e=None
for i,l in enumerate(lines):
    if l=='mutual' and s is None: s=i
    elif l=='end' and s is not None: e=i; break
print('\n'.join(lines[s:e]))
PYEOF
)"
stale=$(printf '%s' "$blk" | grep -cE '\belabExpr[^E]' || true)
if [ "$stale" -eq 0 ]; then
  ok "no Core-only elabExpr projection inside the producing block"
else
  no "$stale Core-only elabExpr call(s) inside the producing block — child evidence would be discarded"
fi

# The pairing types must live where CExpr is genuinely in scope. Declaring them in the
# Proof layer made Lean AUTO-BIND CExpr as an implicit type variable, so the structure
# was polymorphic over a made-up type and its core field landed in Prop — invisible
# until something constructed it.
if grep -q "ElaboratedExprV2" Concrete/Proof/EvidenceTree.lean; then
  no "the Proof layer declares ElaboratedExprV2; CExpr is not in scope there and gets auto-bound"
else
  ok "the pairing types live on the Elab side, where CExpr is in scope"
fi

# ONE RESOLUTION OWNER. Callee identity must be an OUTPUT of the resolution that also
# selects runtime behaviour. A second lookup — even one "sharing a precedence helper" —
# can be handed a different spelling or observe different table state and diverge, with
# runtime taking the intrinsic branch while evidence names the builtin. Both would be
# individually defensible and jointly wrong, and no evidence-to-evidence gate would see
# it.
owners=$(grep -c "let resolveContractCall" Concrete/Elab/Elab.lean || true)
if [ "$owners" -eq 1 ]; then
  ok "exactly one callee-resolution owner"
else
  no "$owners callee-resolution owners — runtime and evidence could resolve differently"
fi

# Installed at ElabEnv CONSTRUCTION, not patched in afterwards: an optional resolver
# added later leaves a window where identity silently answers none and callers emit gaps
# for a reason that is not real.
if grep -qE "^\s+resolveCallee := resolveContractCall" Concrete/Elab/Elab.lean; then
  ok "the resolver is installed at ElabEnv construction, with no fail-open window"
else
  no "resolveCallee is not installed at construction — there would be a transient fail-open state"
fi

echo "BINDER-REFS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
