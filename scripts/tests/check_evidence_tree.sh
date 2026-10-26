#!/usr/bin/env bash
# R-0004: the typed evidence tree — ProofBodyCanonicalV2's input.
#
# The properties are about REPRESENTABILITY, since nothing consumes the tree yet:
#   - nesting is distinguishable (the reason it is a tree and not a flat stream)
#   - "handled nothing" is unrepresentable: every level has a `gap`
#   - bound names cannot enter the tree, because there is nowhere to put them
set -Eeuo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fatal() {
  local rc=$?
  echo "FATAL: check_evidence_tree stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2
  exit "$rc"
}
trap fatal ERR

PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

cat > "$TMP_DIR/p.lean" <<'LEAN'
import Lean
import Concrete
open Concrete Concrete.Proof

def a (label : String) (c : Bool) : IO Unit :=
  if c then IO.println ("  ok   " ++ label)
  else throw (IO.userError ("FAILED: " ++ label))

open Lean in
def ctorsOf (n : Name) : Lean.MetaM (List Name) := do
  match (← getEnv).find? n with
  | some (.inductInfo iv) => pure iv.ctors
  | _ => throwError m!"{n} is not an inductive"

-- An explicit injection into a comparable form. The tree derives NO Repr on purpose:
-- a derived instance across a mutual inductive is exactly where a serializer could
-- quietly acquire a fallback for an unclassified constructor. Writing this by hand in
-- the GATE keeps that pressure on the real serializer rather than relieving it.
partial def shape : EvidenceExprV2 → String
  | .intLit v t    => s!"i{v}:{repr t}"
  | .floatLit b t  => s!"fl{b}:{repr t}"
  | .boolLit v     => s!"b{v}"
  | .strLit v      => s!"s{v}"
  | .charLit v     => s!"c{v.toNat}"
  | .binderRef o i => s!"r{o}.{i}"
  | .constRef id   => s!"k{id.render}"
  | .unary op x    => s!"u({op},{shape x})"
  | .binary op l r => s!"n({op},{shape l},{shape r})"
  | .call c args   => s!"C({c.render},[{",".intercalate (args.map shape)}])"
  | .field id o    => s!"f({id.render},{shape o})"
  | .structLit t fs   => s!"S({t.render},[{",".intercalate (fs.map fun fe => shape fe.2)}])"
  | .variantLit id fs => s!"V({id.render},[{",".intercalate (fs.map fun fe => shape fe.2)}])"
  | .deref x       => s!"D({shape x})"
  | .borrow m x    => s!"B({m},{shape x})"
  | .index c i     => s!"X({shape c},{shape i})"
  | .cast t x      => s!"A({typeRefBytes t},{shape x})"
  | .fnRef id      => s!"F({id.render})"
  | .arrayLit t els => s!"AR({typeRefBytes t},[{",".intercalate (els.map shape)}])"
  | .matchExpr sc arms => s!"Q({shape sc},{arms.length})"
  -- Branch LENGTHS, not their contents: `shape` is declared before `sshape` and the two
  -- are not mutual, so this mirrors how `matchExpr` treats its arms.
  | .ifExpr c t e  => s!"H({shape c},{t.length},{e.length})"
  | .gap _         => "GAP"

partial def sshape : EvidenceStmtV2 → String
  -- The ghost flag is in the shape: a ghost binding is ERASED, so it is not the same
  -- statement as a runtime one with the same initializer (bug 068).
  | .letBind g _ e    => s!"L({if g then "g" else "r"},{shape e})"
  | .assign pl v      => s!"=({shape pl},{shape v})"
  | .ret v            => s!"R({(v.map shape).getD "-"})"
  | .branch c t e     => s!"?({shape c},[{",".intercalate (t.map sshape)}],[{",".intercalate (e.map sshape)}])"
  | .match_ sc _      => s!"M({shape sc})"
  | .loop c i _ b     => s!"W({shape c},{i.length},[{",".intercalate (b.map sshape)}])"
  | .block sts        => s!"BL([{",".intercalate (sts.map sshape)}])"
  | .exprStmt e v     => s!"E({shape e},{v})"
  | .breakStmt t v    => s!"BR({t},{(v.map shape).getD "-"})"
  | .continueStmt t   => s!"CO({t})"
  | .deferStmt x      => s!"DF({shape x})"
  | .assertStmt x     => s!"AS({shape x})"
  | .assumeStmt x     => s!"AM({shape x})"
  | .gap _            => "sGAP"

partial def pshape : EvidencePatternV2 → String
  | .wildcard      => "_"
  | .binder        => "@"
  | .intLit v _    => s!"pi{v}"
  | .boolLit v     => s!"pb{v}"
  | .strLit v      => s!"ps{v}"
  | .charLit v     => s!"pc{v.toNat}"
  | .variant id fs => s!"pV({id.render},[{",".intercalate (fs.map fun fp => pshape fp.2)}])"
  | .structPat t fs => s!"pS({t.render},[{",".intercalate (fs.map fun fp => pshape fp.2)}])"
  | .range lo hi i => s!"pR({shape lo},{shape hi},{i})"
  | .gap _         => "pGAP"

def p : EvidenceExprV2 := .binderRef 0 0
def q : EvidenceExprV2 := .binderRef 0 1
def rr : EvidenceExprV2 := .binderRef 0 2
def fid : CallableId := CallableId.ofUser "m" "f"
def limitId : ConstId := { defModule := "m", declName := "LIMIT" }
def tid : TypeId := TypeId.user "m" "T"
-- Type POSITIONS now take an EvidenceTypeRef, not a bare TypeId: `TypeId` names only
-- nominal types and cannot express `Int` or `[Int; 3]`.
def tref : EvidenceTypeRef := .nominal tid

#eval show Lean.MetaM Unit from do
  let ex ← ctorsOf ``Concrete.Proof.EvidenceExprV2
  let st ← ctorsOf ``Concrete.Proof.EvidenceStmtV2
  let ar ← ctorsOf ``Concrete.Proof.EvidenceArmV2
  -- FAIL-CLOSED REPRESENTABILITY. Without a `gap` at a level, a producer meeting an
  -- unknown construct there has no honest option and will emit nothing — the silent
  -- under-approximation measured on the flat serializer.
  a s!"EvidenceExprV2 has a gap constructor ({ex.length} ctors)"
    (ex.any fun c => c.toString.endsWith ".gap")
  a s!"EvidenceStmtV2 has a gap constructor ({st.length} ctors)"
    (st.any fun c => c.toString.endsWith ".gap")
  a s!"EvidenceArmV2 has a gap constructor ({ar.length} ctors)"
    (ar.any fun c => c.toString.endsWith ".gap")
  let pt ← ctorsOf ``Concrete.Proof.EvidencePatternV2
  a s!"EvidencePatternV2 has a gap constructor ({pt.length} ctors)"
    (pt.any fun c => c.toString.endsWith ".gap")

#eval do
  -- THE REASON IT IS A TREE. A flat node stream cannot tell these apart.
  a "differently-nested binaries are different values"
    (shape (.binary "add" (.binary "mul" p q) rr)
      != shape (.binary "add" p (.binary "mul" q rr)))
  a "argument ORDER in a call is semantic"
    (shape (.call fid [p, q]) != shape (.call fid [q, p]))
  a "operand order in a binary is semantic"
    (shape (.binary "sub" p q) != shape (.binary "sub" q p))
  a "distinct literals are distinct evidence"
    (shape (.intLit 1 .int) != shape (.intLit 2 .int))
  a "a gap is distinguishable from any real expression"
    (shape (.gap { code := .unhandledExpr }) != shape p)
  -- Control: the injection is not collapsing everything to one string.
  a "the injection distinguishes structurally different trees"
    ((shape p != shape q) && (shape p != shape (.deref p)))

  -- LITERAL TYPE IS SEMANTIC. `1i32` and `1i64` are different program data; a shape
  -- that ignored width would merge them.
  a "a literal's semantic width is part of its evidence"
    (shape (.intLit 1 .i32) != shape (.intLit 1 .int))
  -- FLOAT BITS, deliberately: distinct NaN payloads and signed zeros stay distinct.
  a "float evidence is a bit pattern, so -0.0 and 0.0 differ"
    (shape (.floatLit 0 .f64) != shape (.floatLit 0x8000000000000000 .f64))

  -- PATTERNS SELECT. bindingCount alone cannot distinguish these: both bind one
  -- value, and both would have been bindingCount = 1.
  a "two patterns binding one value each are still distinguishable"
    (pshape (.variant ⟨TypeId.user "m" "E", "A"⟩ [(⟨TypeId.user "m" "E", "v"⟩, .binder)])
      != pshape (.variant ⟨TypeId.user "m" "E", "B"⟩ [(⟨TypeId.user "m" "E", "v"⟩, .binder)]))
  a "a wildcard and a binder are different patterns"
    (pshape .wildcard != pshape .binder)
  a "nested pattern ORDER is semantic"
    (pshape (.structPat (TypeId.user "m" "S")
       [(⟨TypeId.user "m" "S", "a"⟩, .binder), (⟨TypeId.user "m" "S", "b"⟩, .wildcard)])
     != pshape (.structPat (TypeId.user "m" "S")
       [(⟨TypeId.user "m" "S", "a"⟩, .wildcard), (⟨TypeId.user "m" "S", "b"⟩, .binder)]))

  -- LABELLED CONTROL FLOW resolves to a relative target, not a label spelling.
  a "break targets a loop by relative depth, not by label"
    (sshape (.breakStmt 0 none) != sshape (.breakStmt 1 none))

  -- bindingCount is DERIVED. Storing it would let pattern structure and frame count
  -- drift; a frame with the wrong slot count shifts every relative position inside it.
  a "a wildcard binds nothing and a binder binds one"
    (patternBindingCount .wildcard == 0 && patternBindingCount .binder == 1)
  a "nested pattern bindings are summed, not counted per level"
    (patternBindingCount (.variant ⟨TypeId.user "m" "E", "A"⟩
       [(⟨TypeId.user "m" "E", "a"⟩, .binder), (⟨TypeId.user "m" "E", "b"⟩, .binder)]) == 2)
  a "a literal pattern binds nothing"
    (patternBindingCount (.intLit 3 .i32) == 0)

  -- SEPARATE AXES, built only through `of`. The structure's constructor is private,
  -- so these cannot be forged: a caller who could write `{ bodyGaps := [] }` beside a
  -- gap-bearing body would restate a fact where it can disagree.
  a "a complete body with an unbound constant blocks on DEPENDENCIES, not the body"
    (let sc := SubjectCompletenessV2.of
       { statements := [.ret (some (.constRef limitId))] } []
     sc.bodyComplete && !sc.dependenciesComplete && !sc.isComplete
       && sc.blockers.length == 1)
  a "binding that constant clears the dependency axis"
    (let sc := SubjectCompletenessV2.of
       { statements := [.ret (some (.constRef limitId))] } [limitId]
     sc.isComplete && sc.blockers.isEmpty)
  a "a gap-bearing body blocks on the BODY axis"
    (let sc := SubjectCompletenessV2.of
       { statements := [.gap { code := .unhandledStmt }] } []
     !sc.bodyComplete && sc.dependenciesComplete && !sc.isComplete
       && sc.blockers.length == 1)
  a "both axes blocked reports BOTH blockers, not one"
    (let sc := SubjectCompletenessV2.of
       { statements := [.gap { code := .unhandledStmt },
                        .ret (some (.constRef limitId))] } []
     sc.blockers.length == 2)
  a "a clean body with no constants is complete"
    (let sc := SubjectCompletenessV2.of { statements := [.ret (some p)] } []
     sc.isComplete && sc.blockers.isEmpty)
  -- Determinism: repeated references must not multiply the blocker list.
  a "a constant referenced twice is reported once"
    (let sc := SubjectCompletenessV2.of
       { statements := [.ret (some (.binary "add" (.constRef limitId) (.constRef limitId)))] } []
     sc.unboundConsts.length == 1)
  a "constants nested in a loop invariant are collected"
    (let sc := SubjectCompletenessV2.of
       { statements := [.loop p [.constRef limitId] none []] } []
     sc.unboundConsts.length == 1)

  -- THE FIVE DECIDED CASES.
  a "array element ORDER is semantic"
    (shape (.arrayLit tref [p, q]) != shape (.arrayLit tref [q, p]))
  a "array element MULTIPLICITY is semantic"
    (shape (.arrayLit tref [p, p]) != shape (.arrayLit tref [p]))
  a "reordering defers changes the body"
    (sshape (.block [.deferStmt p, .deferStmt q])
      != sshape (.block [.deferStmt q, .deferStmt p]))
  -- assert and assume must NEVER collide: one is discharged, the other is relied upon.
  a "assert and assume never collide in the bytes"
    (sshape (.assertStmt p) != sshape (.assumeStmt p))

  -- THE ASSUMPTION AXIS. Bytes alone are insufficient — a proof leaning on an
  -- assumption must not surface unqualified.
  a "a body with no assume is UNQUALIFIED"
    ((SubjectQualificationV2.of { statements := [.ret (some p)] }).isUnqualified)
  a "a single assume qualifies the claim"
    (let q := SubjectQualificationV2.of { statements := [.assumeStmt p] }
     !q.isUnqualified && q.assumptionCount == 1)
  a "an assert does NOT qualify — it is discharged, not assumed"
    ((SubjectQualificationV2.of { statements := [.assertStmt p] }).isUnqualified)
  a "assumptions nested in a branch or loop are still collected"
    ((SubjectQualificationV2.of
       { statements := [.branch p [.assumeStmt q] [.loop p [] none [.assumeStmt p]]] }
     ).assumptionCount == 2)
  a "CHANGING an assumption changes both the body bytes and the axis content"
    (let b1 : EvidenceBodyDraftV2 := { statements := [.assumeStmt p] }
     let b2 : EvidenceBodyDraftV2 := { statements := [.assumeStmt q] }
     (String.join (b1.statements.map sshape) != String.join (b2.statements.map sshape))
       && (shape ((SubjectQualificationV2.of b1).assumptions.headD p)
             != shape ((SubjectQualificationV2.of b2).assumptions.headD p)))
  a "REMOVING an assumption changes both the bytes and the axis"
    (let b1 : EvidenceBodyDraftV2 := { statements := [.assumeStmt p] }
     let b2 : EvidenceBodyDraftV2 := { statements := [.exprStmt p false] }
     (String.join (b1.statements.map sshape) != String.join (b2.statements.map sshape))
       && (SubjectQualificationV2.of b1).assumptionCount
            != (SubjectQualificationV2.of b2).assumptionCount)

  -- Gap CODES are stable and comparable, so blocker classes can be counted across
  -- versions; free-form strings could not be.
  a "gap codes are comparable as classes, independent of detail"
    (({ code := .unhandledExpr, detail := "one" } : EvidenceGap).code
      == ({ code := .unhandledExpr, detail := "other" } : EvidenceGap).code)
  a "different blocker classes are distinguishable"
    (EvidenceGapCode.unhandledExpr != EvidenceGapCode.unresolvedConst)

  -- DRAFT vs COMPLETE. A gap-bearing draft must be REJECTED with its gaps reported,
  -- and a clean draft must validate. `validate` is the only bridge.
  a "a gap-bearing draft fails validation and reports its gaps"
    (match validate { statements := [.gap { code := .unhandledStmt }] } with
     | .error gs => gs.length == 1
     | .ok _ => false)
  a "a gap NESTED deep inside is still found"
    (match validate { statements :=
        [.branch p [] [.loop q [] none [.exprStmt (.gap { code := .unresolvedType }) false]]] } with
     | .error gs => gs.length == 1
     | .ok _ => false)
  a "a gap-free draft validates"
    (match validate { statements := [.ret (some p)] } with
     | .ok _ => true | .error _ => false)
  -- Control: validation is not vacuously accepting or rejecting everything.
  a "validation discriminates rather than answering uniformly"
    ((match validate { statements := [.ret (some p)] } with | .ok _ => true | _ => false)
      && (match validate { statements := [.gap { code := .unhandledStmt }] } with
          | .error _ => true | _ => false))
LEAN

OUT="$TMP_DIR/out.txt"
if ! lake env lean "$TMP_DIR/p.lean" > "$OUT" 2>&1; then
  echo "FATAL: evidence-tree probe did not elaborate" >&2; grep -m3 "error" "$OUT" >&2; exit 1
fi
if grep -qE "error|sorry" "$OUT"; then
  echo "FATAL: probe emitted a diagnostic" >&2; grep -m3 "error" "$OUT" >&2; exit 1
fi
cat "$OUT"
PASS=$(( PASS + $(grep -c '^  ok   ' "$OUT" || true) ))

# Bound NAMES must have nowhere to live. Alpha-invariance by construction beats a
# normalization pass that can be forgotten.
if grep -qE "letBind .*\(name" Concrete/Proof/EvidenceTree.lean; then
  no "letBind carries a source name — alpha-invariance would depend on normalization"
else
  ok "letBind has no name field; a bound name cannot enter the tree"
fi
if grep -q "bindingCount" Concrete/Proof/EvidenceTree.lean; then
  ok "arm patterns record a binding COUNT, not binding names"
else
  no "arm patterns do not use a positional binding count"
fi
# Loop contracts must be part of the body, or an invariant edit is invisible.
if grep -qE "loop .*invariants" Concrete/Proof/EvidenceTree.lean; then
  ok "loops carry their invariants, so an invariant edit changes the body"
else
  no "loops omit invariants — an invariant edit would be invisible to the subject"
fi
# THE TYPE-LEVEL SEPARATION. A serializer accepting the gap-bearing draft type would
# put a gap REASON — a diagnostic string — into digest bytes and present a partial body
# as complete. Enforced by the type, not by convention: CompleteEvidenceBodyV2 carries
# a proof that its draft has no gaps, and `validate` is the only way to obtain one.
if grep -q "def CompleteEvidenceBodyV2 := { d : EvidenceBodyDraftV2 // draftGaps d = \[\] }" Concrete/Proof/EvidenceTree.lean; then
  ok "CompleteEvidenceBodyV2 is a subtype carrying a no-gaps proof"
else
  no "CompleteEvidenceBodyV2 does not carry a proof — completeness would be conventional"
fi
# Multi-line signature: a single-line grep missed it and reported a false failure.
validate_sig="$(awk '/^def validate/,/:=/' Concrete/Proof/EvidenceTree.lean | tr '\n' ' ')"
if printf '%s' "$validate_sig" | grep -qE "Except \(List EvidenceGap\) CompleteEvidenceBodyV2"; then
  ok "validate is the sole draft-to-complete bridge and reports gaps"
else
  no "validate does not report the gaps it found — an incomplete body would be undiagnosable"
fi
# The gap-walkers must be exhaustive; a wildcard would report a new construct gap-free.
for fn in exprGaps patternGaps stmtGaps armGaps patternBindingCount exprConstRefs stmtConstRefs armConstRefs patternConstRefs stmtAssumptions armAssumptions; do
  body="$(awk "/^partial def $fn/,/^\$/" Concrete/Proof/EvidenceTree.lean)"
  if printf '%s' "$body" | grep -qE '^\s*\|\s*_\s*=>'; then
    no "$fn has a wildcard — a new constructor would be reported as gap-free"
  else
    ok "$fn is exhaustive with no wildcard"
  fi
done

# THE MATRIX MUST COVER EVERY AST CONSTRUCTOR. A classification that silently omits a
# constructor is how a producer ends up with an unclassified case, so the coverage is
# checked against the AST rather than trusted.
MATRIX="docs/verification/EVIDENCE_PRODUCER_MATRIX.md"
if [ -f "$MATRIX" ]; then
  missing=""
  for ctor in $(awk '/^inductive Stmt where/,/^$/' Concrete/Frontend/AST.lean \
                  | sed -n 's/^  | \([a-zA-Z_]*\).*/\1/p'); do
    grep -q "\`$ctor\`" "$MATRIX" || missing="$missing $ctor"
  done
  if [ -z "$missing" ]; then
    ok "every Stmt constructor is classified in the producer matrix"
  else
    no "producer matrix omits Stmt constructors:$missing"
  fi
  # Undecided cases must be explicit, not absent.
  if grep -q "undecided" "$MATRIX"; then
    ok "the matrix marks undecided constructors explicitly rather than omitting them"
  else
    no "the matrix has no undecided marker — an unfinished decision would be invisible"
  fi
else
  no "producer matrix is missing; producer cases would be decided ad hoc"
fi

# NON-FORGEABLE. A private constructor is what stops the axes being written by hand.
if grep -q "private mk ::" Concrete/Proof/EvidenceTree.lean; then
  ok "SubjectCompletenessV2 has a private constructor; axes come only from \`of\`"
else
  no "SubjectCompletenessV2 can be constructed by hand — its inputs could contradict the body"
fi

# No derived Repr/BEq across the MUTUAL tree. Scoped to the mutual block: EvidenceGap
# is a flat enum outside it and derives BEq/Repr legitimately — a first version of
# this leg flagged that and was wrong, not the code.
mutual_block="$(awk '/^mutual$/,/^end$/' Concrete/Proof/EvidenceTree.lean)"
if printf '%s' "$mutual_block" | grep -qE "^deriving (BEq|Repr)|deriving.*\bRepr\b.*\bBEq\b"; then
  no "the mutual tree derives BEq/Repr — a derived instance can absorb new constructors"
else
  ok "the mutual tree derives no BEq/Repr"
fi

echo "EVIDENCE-TREE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
