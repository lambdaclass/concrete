#!/usr/bin/env bash
# R-0004 slice 5 step 1 — facts captured BEFORE contract erasure.
#
# `CFnDef` drops requires/ensures, type bounds and capability parameters. Those
# are exactly the facts bugs 059 and 060 are filed against, so a subject digest
# must be defined from a record captured while they still exist — not recomputed
# in Report, which would define a semantic fact inside the layer that renders
# semantic facts.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
expect_no_compile() {
  local label="$1" body="$2"
  cat > "$TMP/h.lean" <<LEAN
import Concrete
import Examples
open Lean Meta Concrete Concrete.Proof
$body
LEAN
  local out; out="$(lake env lean "$TMP/h.lean" 2>&1 || true)"
  if grep -qE "error:|error\(lean" <<<"$out"; then ok "$label"
  else no "$label — IT COMPILED, so the invariant is documentation rather than a type"; fi
}

probe() {
  local label="$1" want="$2" body="$3"
  cat > "$TMP/p.lean" <<LEAN
import Concrete
open Concrete Concrete.Proof
def sp : Span := default
$body
LEAN
  local out; out="$(lake env lean "$TMP/p.lean" 2>&1 || true)"
  # Match a LEAN DIAGNOSTIC, not the bare word: `Except.error` is a legitimate
  # value and matching "error" made every probe of a refusal a false negative —
  # the vacuity guard corrupting the measurement it exists to protect.
  if grep -qE "error:|error\(lean" <<<"$out"; then
    no "$label — probe did not elaborate: $(printf '%s' "$out" | tr '\n' ' ' | cut -c1-160)"
  elif grep -qF -- "$want" <<<"$out"; then ok "$label"
  else no "$label — got: $(printf '%s' "$out" | tr '\n' ' ' | cut -c1-160)"; fi
}

echo "=== contracts are encoded, and bug 060's shape is distinguishable ==="
# A TRUE and a FALSE #[ensures] currently digest ALIKE — that is bug 060. The
# encoder must tell them apart, or nothing built on it can.
probe "a TRUE and a FALSE ensures encode differently" "true" \
'#eval (contractCanonical (.binOp sp .eq (.ident sp "result") (.intLit sp 1)))
    != (contractCanonical (.binOp sp .eq (.ident sp "result") (.intLit sp 0)))'
# Operand order is semantic for comparisons — a < b is not b < a.
probe "operand order is preserved for a non-commutative comparison" "true" \
'#eval
  let e := contractCanonicalIn ["a", "b"] [] [] [] [] false (fun _ => none)
  (e (.binOp sp .lt (.ident sp "a") (.ident sp "b")))
    != (e (.binOp sp .lt (.ident sp "b") (.ident sp "a")))'
# Width variants are different OPERATIONS, not spellings.
probe "wrapping and saturating add are different operations" "true" \
'#eval binOpTag BinOp.wrappingAdd != binOpTag BinOp.saturatingAdd'

echo ""
echo "=== outside the fragment, the encoding is UNAVAILABLE, not approximate ==="
# Silently skipping an unreadable node yields a digest blind to the edit it must
# detect. `none` forces the containing record to declare itself uncovered.
probe "an out-of-fragment expression does not encode" "none" \
'#eval contractCanonical (.arrayLit sp [.intLit sp 1])'
probe "one unencodable contract makes the whole set uncovered" "false" \
'#eval (ContractFacts.of [] [.arrayLit sp [.intLit sp 1]]).covered'
probe "an encodable set stays covered" "true" \
'#eval (ContractFacts.ofResolved ["r"] [] [] [] [] (fun _ => none) []
        [.binOp sp .eq (.ident sp "r") (.intLit sp 1)]).covered'
# "could not read the contracts" and "there are no contracts" are DIFFERENT
# states; a digest that merges them lets an unreadable contract pass as absent.
probe "uncovered and genuinely-absent contracts do not digest alike" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", contracts := ContractFacts.of [] [.arrayLit sp [.intLit sp 1]] }
  let b : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" }
  a.canonical != b.canonical'

echo ""
echo "=== the facts cover what bug 059 says the body hash omits ==="
# 059: the fingerprint omits parameters, return type, generics/bounds and
# capabilities. Each must move the canonical form.
probe "a return-type change moves the facts" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", retTy := "i32" }
  let b : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", retTy := "u32" }
  a.canonical != b.canonical'
probe "a parameter type change moves the facts" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", params := [("x", "i32")] }
  let b : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", params := [("x", "u32")] }
  a.canonical != b.canonical'
probe "a capability change moves the facts" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", capSet := ["File"] }
  let b : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", capSet := ["File", "Net"] }
  a.canonical != b.canonical'
probe "a generic bound change moves the facts" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" 1, typeBounds := [("T", ["Copy"])] }
  let b : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" 1, typeBounds := [("T", ["Destroy"])] }
  a.canonical != b.canonical'
probe "losing a trust boundary moves the facts" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", isTrusted := true }
  let b : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", isTrusted := false }
  a.canonical != b.canonical'

echo ""
echo "=== keyed by identity, not by name ==="
probe "lookup resolves by CallableId" "true" \
'#eval
  let d : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" }
  let p : ProgramFacts := { decls := [d] }
  (p.find? (CallableId.ofUser "m" "f")).isSome && (p.find? (CallableId.ofUser "other" "f")).isNone'

echo ""
echo "=== the producer runs at the erasure point, not downstream ==="
# The facts must be built where requires/ensures still exist. If this moves into
# Report, the definition of a semantic fact has moved into the rendering layer.
if grep -q "declFacts" "$ROOT_DIR/Concrete/Elab/Elab.lean"; then
  ok "Elab (pre-erasure) populates declFacts"
else
  no "declFacts is not populated in Elab — the facts would have to be rebuilt downstream"
fi
if grep -q "declFacts" "$ROOT_DIR/Concrete/Elab/Core.lean"; then
  ok "CModule carries declFacts as a parallel record"
else
  no "CModule does not carry declFacts"
fi
# And they must NOT have been bolted onto CFnDef: Core excludes contracts on
# purpose and no codegen path consumes them.
if grep -A26 "^structure CFnDef" "$ROOT_DIR/Concrete/Elab/Core.lean" | has_match -E "requires|ensures"; then
  no "CFnDef gained contracts — Core excludes them deliberately; keep facts parallel"
else
  ok "CFnDef did NOT gain contracts; the facts stay a parallel record"
fi

echo ""
echo "=== the digest is defined ONCE, and sees what the legacy hash cannot ==="
# Side by side with the defect. The legacy body fingerprint is blind to both of
# these; that blindness IS bugs 059 and 060.
probe "059: a signature change moves the subject digest with an identical body" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", retTy := "i32" }
  let b : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", retTy := "u32" }
  proofSubjectDigestV2 a (some {}) none none != proofSubjectDigestV2 b (some {}) none none'
probe "060: a TRUE/FALSE ensures flip moves it with an identical body" "true" \
'#eval
  let t : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", contracts := ContractFacts.of [] [.binOp sp .eq (.ident sp "result") (.intLit sp 1)] }
  let f : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", contracts := ContractFacts.of [] [.binOp sp .eq (.ident sp "result") (.intLit sp 0)] }
  proofSubjectDigestV2 t (some {}) none none != proofSubjectDigestV2 f (some {}) none none'
# The subject binds the STRUCTURAL body (2026-08-09), not the legacy Core-statement hash, so
# "a body change is detected" is no longer expressible as two hash strings at this level —
# it needs two real bodies. That coverage MOVED rather than being dropped:
# `check_proof_freshness.sh` asserts it end-to-end on a real program (SHADOW(body): a BODY
# edit moves the v2 digest), which is stronger than this unit probe was.
#
# What remains testable here is the fail-closed half: no structural body, no digest. A digest
# over an absent body would describe nothing while looking like it described the program.
# SELECTED SPECIFICATION is part of the subject: a proof established against one specification
# is not evidence for another, so re-pointing a link must move the subject. Both directions,
# because binding a value that never changes the digest would be indistinguishable from not
# binding it at all.
probe "a DIFFERENT selected spec moves the subject" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" }
  proofSubjectDigestV2 a (some {}) (some "SpecA") none != proofSubjectDigestV2 a (some {}) (some "SpecB") none'
probe "the SAME selected spec leaves it unchanged" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" }
  proofSubjectDigestV2 a (some {}) (some "SpecA") none == proofSubjectDigestV2 a (some {}) (some "SpecA") none'
# Absence is a VALUE, not a refusal — a function with no attached spec still has a subject, it
# is simply unproved. It must be stable, and must not collide with a spec whose name is empty.
probe "NO selected spec is stable across calls" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" }
  proofSubjectDigestV2 a (some {}) none none == proofSubjectDigestV2 a (some {}) none none'
probe "NO spec differs from a spec named the empty string" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" }
  proofSubjectDigestV2 a (some {}) none none != proofSubjectDigestV2 a (some {}) (some "") none'
# === IMPLEMENTATION IDENTITY vs PROOF SUBJECT (slice 6) ======================================
# The two must move on different things. An implementation is what a callable IS; a proof subject
# adds what a particular LINK claims about it. If spec/scope moved the implementation digest, a
# table entry would change when a specification was re-pointed — which has nothing to do with the
# implementation, and would make table membership depend on proof-link metadata.
echo "=== implementation identity is independent of proof-link metadata ==="

# Independence from spec/scope is STRUCTURAL, not behavioural: `implementationDigest` takes no
# spec or scope argument, so there is no input to vary. The probe that used to sit here compared
# `implementationDigest f b` with ITSELF and called that independence — a tautology that would
# pass for any function whatsoever.
#
# What is testable is that the API cannot ACCEPT them, which is where the property actually lives.
expect_no_compile "implementationDigest cannot take a spec or scope argument" '
#eval
  let f : Concrete.Proof.CheckedDeclFacts := { id := Concrete.CallableId.ofUser "m" "f" }
  match Concrete.Proof.validate ({} : Concrete.Proof.EvidenceBodyDraftV2) with
  | .ok b => Concrete.implementationDigest f b (some "SpecA")
  | .error _ => ""'

# ...while the SUBJECT does move on spec and scope. Both directions, or "independent" would be
# indistinguishable from "ignores everything".
probe "selected spec DOES move the proof subject" "true" \
'#eval
  let f : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", retTy := "i32" }
  proofSubjectDigestV2 f (some {}) (some "SpecA") (some "iff")
    != proofSubjectDigestV2 f (some {}) (some "SpecB") (some "iff")'
probe "claim scope DOES move the proof subject" "true" \
'#eval
  let f : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", retTy := "i32" }
  proofSubjectDigestV2 f (some {}) (some "S") (some "iff")
    != proofSubjectDigestV2 f (some {}) (some "S") (some "one_direction")'

# The SIGNATURE does move the implementation digest — otherwise it would bind nothing.
probe "a signature change DOES move the implementation digest" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", retTy := "i32" }
  let b : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", retTy := "u32" }
  match Proof.validate ({} : Proof.EvidenceBodyDraftV2) with
  | .ok bd => implementationDigest a bd != implementationDigest b bd
  | .error _ => false'
probe "a CONTRACT change DOES move the implementation digest" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", contracts := ContractFacts.of [] [.binOp sp .eq (.ident sp "result") (.intLit sp 1)] }
  let b : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", contracts := ContractFacts.of [] [.binOp sp .eq (.ident sp "result") (.intLit sp 0)] }
  match Proof.validate ({} : Proof.EvidenceBodyDraftV2) with
  | .ok bd => implementationDigest a bd != implementationDigest b bd
  | .error _ => false'

# Domain separation: the implementation digest and the proof subject over the same inputs must
# not collide, or two different identities would be indistinguishable by construction.
probe "implementation digest and proof subject are in DIFFERENT domains" "true" \
'#eval
  let f : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", retTy := "i32" }
  match Proof.validate ({} : Proof.EvidenceBodyDraftV2) with
  | .ok b => some (implementationDigest f b) != proofSubjectDigestV2 f (some {}) none none
  | .error _ => false'

# === SUBJECT SCHEMA FREEZE ==================================================================
# The subject's components are complete: semantic identity, typed signature, structural V2 body,
# contracts, selected specification, claim scope. From here the digest is a STORED VALUE — step 7
# migrates 44 links to it — so any change to its composition invalidates every stored subject.
#
# This pins the digest of a fixed input. It is not testing a property; it is a FREEZE. Adding,
# removing or reordering a component moves this value, and the correct response is never to
# update the constant: it is to bump the schema tag (`subjectV2:` -> `subjectV3:`) so stored
# values are recognised as a DIFFERENT SCHEMA and become `needs_recheck` rather than silently
# comparing unequal and reading as `stale`.
#
# Updating the constant without bumping the tag is the one edit that turns every stored proof
# link into a false staleness report, which is why this says so here rather than in a commit
# message nobody will be reading at the time.
# THE FREEZE IS REVOKED (2026-08-09) — see V2_FREEZE_REVOKED in ROADMAP.md. Cross-compilation
# determinism failed, which is a freeze PREREQUISITE, so this pin is no longer a freeze: it is a
# composition tripwire that still catches an accidental change to the digest's structure.
#
# The two states are tied together below so neither can drift: while the order defect is open the
# roadmap MUST carry the revocation, and when the defect is fixed the revocation MUST be removed
# in the same commit that re-establishes the freeze. A revocation nobody retracts is how a
# temporary state becomes permanent.
# REFROZEN 2026-08-09, in the same commit that removed the revocation marker — the two are tied
# in both directions and neither may drift alone. The freeze prerequisite that failed
# (cross-compilation determinism) now holds: see the ORDER assertions in check_proof_freshness.sh.
if grep -q "V2_FREEZE_REVOKED" ROADMAP.md docs/project/ROADMAP_TASKS.md docs/project/ROADMAP_BACKLOG.md; then
  no "ROADMAP.md still carries V2_FREEZE_REVOKED, but determinism is repaired and V2 is refrozen — remove the marker"
else
  ok "V2 freeze is in force and the revocation marker is gone"
fi
probe "SCHEMA FREEZE: the subject digest of a fixed input is unchanged" "db0c59105288dea1437135ef062f9e86" \
'#eval
  let f : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", retTy := "i32" }
  (proofSubjectDigestV2 f (some {}) (some "SpecA") (some "iff")).get!'

# CLAIM SCOPE. A proof covering ONE DIRECTION of a postcondition and one covering both are not
# the same claim; narrowing `iff` to `one_direction` must move the subject, or less was proved
# and nothing said so.
probe "a NARROWED claim scope moves the subject" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" }
  proofSubjectDigestV2 a (some {}) (some "S") (some "iff") != proofSubjectDigestV2 a (some {}) (some "S") (some "one_direction")'
probe "the SAME claim scope leaves it unchanged" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" }
  proofSubjectDigestV2 a (some {}) (some "S") (some "iff") == proofSubjectDigestV2 a (some {}) (some "S") (some "iff")'
probe "NO claim scope differs from a scope named the empty string" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" }
  proofSubjectDigestV2 a (some {}) (some "S") none != proofSubjectDigestV2 a (some {}) (some "S") (some "")'
probe "an ABSENT structural body yields NO digest" "none" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" }
  proofSubjectDigestV2 a none none none'
# The schema tag must be in the bytes, so a stored v1 hash is recognisable as a
# DIFFERENT SCHEMA rather than as a mismatch — that is what makes `needs_recheck`
# possible instead of a false `stale`.
probe "the subject digest is not the bare body hash" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" }
  proofSubjectDigestV2 a (some {}) none none != shortHash "B"'

echo ""
echo "=== completeness is ENFORCED, not advisory ==="
# isComplete existed and nothing consulted it: an uncovered subject still got an
# ordinary-looking digest. A digest indistinguishable from a complete one IS a
# claim of completeness, whatever a neighbouring flag says.
probe "an uncovered contract yields NO digest" "none" \
'#eval
  let f : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", contracts := ContractFacts.of [] [.arrayLit sp [.intLit sp 1]] }
  proofSubjectDigestV2 f (some {}) none none'
probe "a complete subject yields a digest" "some" \
'#eval
  let f : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" }
  proofSubjectDigestV2 f (some {}) none none'
# an incomplete IDENTITY (type-erased generic) must refuse too
probe "an incomplete identity yields NO digest" "none" \
'#eval
  let f : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" 1 }
  proofSubjectDigestV2 f (some {}) none none'
# loop contracts are part of the subject: R-0004 names them, and they are erased
# with requires/ensures
probe "a loop invariant change moves the subject" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", contracts := { loops := ["inv:A"] } }
  let b : CheckedDeclFacts := { id := CallableId.ofUser "m" "f", contracts := { loops := ["inv:B"] } }
  a.canonical != b.canonical'

echo ""
echo "=== spec fns are resolvable contract targets, in their own namespace ==="
# `spec fn` declarations are bodyless abstractions whose meaning comes from Lean.
# They were in none of the resolution tables, so every contract mentioning one
# resolved to nothing and made the whole subject UNCOVERED — measured on
# hmac_sha256, a flagship, whose `result == ch_spec(x, y, z)` put two entries
# outside the digest entirely.
CC=".lake/build/bin/concrete"
HM="examples/hmac_sha256/src/main.con"
# CAPTURE stderr and CLASSIFY it. The earlier version used 2>/dev/null, so a run
# emitting 11 diagnostics still read as clean. Hiding a channel is not the same as
# knowing what is on it.
HM_OUT="$TMP/hm.out"; HM_ERR="$TMP/hm.err"
"$CC" "$HM" --report subject-facts >"$HM_OUT" 2>"$HM_ERR"
n_entries=$(grep -c '^v1:' "$HM_OUT" || true)
[ "$n_entries" = "23" ] \
  && ok "hmac_sha256 reports exactly 23 subjects" \
  || no "expected exactly 23 hmac_sha256 subjects, got $n_entries"
inc=$(grep -cE "INCOMPLETE|ABSENT" "$HM_OUT" || true)
[ "$inc" = "0" ] \
  && ok "hmac_sha256 has no incomplete or absent subjects" \
  || no "$inc hmac_sha256 subjects are incomplete/absent — contracts are escaping the digest"
withc=$(grep -c "ens: 1" "$HM_OUT" || true)
[ "$withc" = "2" ] \
  && ok "hmac_sha256 has exactly 2 contract-bearing subjects" \
  || no "expected exactly 2 contract-bearing hmac_sha256 subjects, got $withc"
# Every stderr line must be an EXPECTED kind. The unbound-proof-subject errors are
# bug 058's containment working (a #[proof_by] with no #[proof_fingerprint] is
# unbound, not proved); anything else is unclassified and must fail.
tot_err=$(grep -c "^error:" "$HM_ERR" || true)
unbound=$(grep -c "has no stored proof subject" "$HM_ERR" || true)
if [ "$tot_err" = "$unbound" ]; then
  ok "all $tot_err stderr diagnostics are the expected unbound-proof-subject kind (bug 058 containment)"
else
  no "stderr carries $((tot_err - unbound)) UNCLASSIFIED diagnostic(s) beyond the $unbound expected"
fi

echo ""
echo "=== a name meaning two callables fails closed, with controls ==="
# Ambiguity was implemented but only inspection-verified. This fixture declares
# `dup` as BOTH a spec fn and an ordinary function; a contract naming it cannot
# say which, and resolving by table order would put an abstraction where an
# implementation was meant. The controls are what stop "uncovered" from being an
# artifact of the fixture.
AMB="tests/programs/subject_spec_name_ambiguity.con"
[ -f "$AMB" ] && ok "the ambiguity fixture is committed" || no "ambiguity fixture missing"
AMB_OUT="$TMP/amb.out"
"$CC" "$AMB" --report subject-facts >"$AMB_OUT" 2>/dev/null
amb_cov=$(grep -A7 '^v1:user:amb.uses_ambiguous$' "$AMB_OUT" 2>/dev/null | grep -oE "covered: (true|false)" | head -1 || true)
[ "$amb_cov" = "covered: false" ] \
  && ok "a contract naming an ambiguous callable is UNCOVERED" \
  || no "the ambiguous case is '$amb_cov' — order-dependent resolution is back"
for ctl in uses_spec uses_fn; do
  c=$(grep -A7 "^v1:user:amb.$ctl\$" "$AMB_OUT" | grep -oE "covered: (true|false)" | head -1)
  e=$(grep -A7 "^v1:user:amb.$ctl\$" "$AMB_OUT" | grep -oE "ens: [0-9]+" | head -1)
  if [ "$c" = "covered: true" ] && [ "$e" = "ens: 1" ]; then
    ok "control $ctl resolves unambiguously and stays covered"
  else
    no "control $ctl is '$c'/'$e' — uncovered may be an artifact, not the ambiguity"
  fi
done

probe "a spec fn gets its own namespace" "false" \
'#eval (CallableId.ofSpec "m" "f").render == (CallableId.ofUser "m" "f").render'
probe "the namespace list covers every constructor" "true" \
'#eval CallableNamespace.all.length == 5
  && (CallableNamespace.all.map CallableNamespace.canonical).eraseDups.length == 5'

echo ""
echo "=== facts lookup compares identity, not a rendering ==="
# `find?` keyed on `.render`, which keeps the rendered string as operational
# identity underneath a typed field — the same defect corrected in the dependency
# graph, where storing CallableId and then comparing renderings made the type
# cosmetic.
probe "ProgramFacts.find? resolves by CallableId value" "true" \
'#eval
  let d : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" }
  let p : ProgramFacts := { decls := [d] }
  (p.find? (CallableId.ofUser "m" "f")).isSome
    && (p.find? (CallableId.ofUser "other" "f")).isNone
    && (p.find? (CallableId.ofUser "m" "f" 1)).isNone'
# SCOPED TO THE PRODUCTION FILES, not just the helper. The previous version
# grepped only SubjectFacts.lean, so it reported "no lookup routes through
# .render" while ProofCore's actual subject-digest lookup did exactly that — the
# gate gave assurance about the API nobody calls.
# `|| true` INSIDE the substitution: under pipefail a no-match grep returns 1,
# and NO MATCHES is the passing case here — so the guard would abort exactly
# when the code is correct.
render_lookups=$( { grep -rnE "\.render *== *[a-zA-Z_]+\.render" \
  "$ROOT_DIR/Concrete/Proof/ProofCore.lean" "$ROOT_DIR/Concrete/Proof/SubjectFacts.lean" \
  "$ROOT_DIR/Concrete/Proof/Proof.lean" 2>/dev/null || true; } | wc -l | tr -d " ")
[ "$render_lookups" = "0" ] \
  && ok "no lookup in the production proof path compares renderings" \
  || no "$render_lookups lookup(s) still compare renderings in the production path"
# FnTable.lookupById must match on the identity, not on identityKey (which is the
# canonical serialization and belongs in the ROOT).
if grep -q "identityKey == id.render" "$ROOT_DIR/Concrete/Proof/Proof.lean"; then
  no "FnTable.lookupById still matches on the rendered identityKey"
else
  ok "FnTable.lookupById matches on the identity itself"
fi
# SEMANTIC SANITY CHECK, not proof that lookup avoids rendering: `render` includes
# the type-parameter arity (`/1`), so a rendered comparison would pass this too.
# The cross-file structural guard above is what enforces the architectural rule;
# this only confirms the two identities are genuinely distinct subjects.
probe "sanity: lookupById treats a differing type-param arity as a different subject" "true" \
'def gA : CallableId := CallableId.ofUser "m" "g"
def gB : CallableId := CallableId.ofUser "m" "g" 1
def geA : PFnDef := { identity := .semantic gA, operationalKey := "g", displayName := "g", params := ["x"], body := .lit (.int 1) }
def tblA : FnTable := { entries := #[geA], globals := fun n => if n == "g" then some geA else none }
#eval (tblA.lookupById gA).isSome && (tblA.lookupById gB).isNone'

# THE REAL PATH: a ProofCoreEntry must actually carry facts and a digest for a
# program, which is what the rendering comparison would have silently broken had
# the two spellings ever diverged.
real=$("$CC" "examples/crypto_verify/src/main.con" --report subject-facts 2>/dev/null | grep -c "subject digest: [0-9a-f]" || true)
[ "${real:-0}" -ge 4 ] \
  && ok "ProofCore's own declFacts lookup yields digests for a real program ($real)" \
  || no "ProofCore produced ${real:-0} real subject digests — the production lookup is not resolving"

echo ""
echo "=== producer and invariance cases, on one committed program ==="
INV="tests/programs/subject_invariance.con"
[ -f "$INV" ] && ok "the invariance fixture is committed" || no "invariance fixture missing"
INV_OUT="$TMP/inv.out"; "$CC" "$INV" --report subject-facts >"$INV_OUT" 2>/dev/null
idsOf() { grep -c "^v1:user:$1\$" "$INV_OUT" 2>/dev/null || true; }

# (1) SAME SPELLING, DIFFERENT MODULE -> different identities. The case a
# name-keyed table gets wrong.
if [ "$(idsOf 'inner.shared_name')" = "1" ] && [ "$(idsOf 'other.shared_name')" = "1" ]; then
  ok "same declName in two modules yields two distinct subjects"
else
  no "inner.shared_name / other.shared_name did not both appear as distinct subjects"
fi

# (2) IMPORTED ALIAS. `import inner.{ aliased as renamed }` must NOT mint a
# subject under the alias: identity belongs to the DEFINITION site, so a rename at
# the use site cannot move it.
[ "$(idsOf 'main.renamed')" = "0" ] && [ "$(idsOf 'inner.aliased')" = "1" ] \
  && ok "an import alias mints no subject; identity stays at the definition site" \
  || no "the alias 'renamed' produced its own subject — identity followed the use site"

# (3) NESTED and DOUBLY NESTED modules receive facts, keyed under the full path.
[ "$(idsOf 'deep.reach')" = "1" ] \
  && ok "a nested module function receives facts" \
  || no "deep.reach has no facts"
[ "$(idsOf 'deep.nested.nested_buried')" = "1" ] \
  && ok "a DOUBLY nested module function receives facts under its full path" \
  || no "deep.nested.nested_buried has no facts — the pre-prefix keying bug is back"

# (4) IMPL METHODS receive facts, under the TypeName_method naming ProofCore uses.
[ "$(idsOf 'main.Holder_get')" = "1" ] \
  && ok "an impl method receives facts" \
  || no "Holder.get has no facts — impl methods are being skipped"

# (4b) TRAIT-IMPL METHODS take a different declaration path from inherent ones,
# so they need their own coverage rather than being assumed to follow.
[ "$(idsOf 'main.Holder_size')" = "1" ] \
  && ok "a trait-impl method receives facts" \
  || no "Sized for Holder :: size has no facts — trait-impl methods are being skipped"

# (2b) ALIAS USE SITE. Proving no alias SUBJECT was minted is not the same as
# proving a USE through the alias resolves to the definition-site identity. The
# contract in `via_alias` names `renamed`; if that failed to resolve, the subject
# would be uncovered.
alias_cov=$(grep -A7 "^v1:user:main.main.main_via_alias\$" "$INV_OUT" 2>/dev/null | grep -oE "covered: (true|false)" | head -1 || true)
[ "$alias_cov" = "covered: true" ] \
  && ok "a contract USING an import alias resolves (definition-site identity)" \
  || no "a use through the alias did not resolve ('$alias_cov')"

# (5) ALPHA-RENAMING. Type and capability binders are rendered BY INDEX, so their
# spelling is not semantic. VALUE binders are deliberately NOT invariant: a
# contract names its parameters, so renaming one changes what the contracts
# denote. Asserted as it is, rather than claimed invariant.
probe "renaming a TYPE binder does not change a signature rendering" "true" \
'#eval boundTyCanonical ["T"] [] (Ty.typeVar "T") == boundTyCanonical ["U"] [] (Ty.typeVar "U")'
probe "renaming a CAPABILITY binder does not change a fn-type rendering" "true" \
'#eval
  let f := fun (c : String) => boundTyCanonical [] [c] (Ty.fn_ [] (CapSet.var c) Ty.int)
  f "C" == f "D"'
ab=$(grep -A1 "^v1:user:main.alpha.alpha_named_ab\$" "$INV_OUT" 2>/dev/null | grep -oE "params: .*" | head -1 || true)
xy=$(grep -A1 "^v1:user:main.alpha.alpha_named_xy\$" "$INV_OUT" 2>/dev/null | grep -oE "params: .*" | head -1 || true)
[ -n "$ab" ] && [ "$ab" != "$xy" ] \
  && ok "renaming a VALUE binder DOES change the facts (contracts name parameters)" \
  || no "value-binder renaming was invisible — a contract referring to 'a' would survive a rename to 'x'"

# (6) BOUND POSITION is semantic: the same bound on a different parameter
# position is a different signature.
# Compared on SUBJECT DIGESTS, not on `params:` diagnostic output. Diagnostics
# intentionally keep names and would differ for reasons unrelated to the digest,
# so comparing them proved nothing about what evidence actually binds.
# POSITION IS SEMANTIC, asserted on SUBJECT DIGESTS rather than on `params:`
# diagnostic output, which intentionally keeps names and would differ for reasons
# unrelated to what evidence binds.
#
# The GENERIC bound-position pair cannot witness this at the digest level: a
# generic with no recorded instantiation is refused by `isComplete`, so neither
# mints a digest. That refusal is correct, so the witness is a NON-GENERIC pair
# differing only in the order of their parameter types.
bf=$(grep -A12 "^v1:user:main.boundpos.boundpos_pos_int_bool\$" "$INV_OUT" 2>/dev/null | grep -oE "subject digest: [0-9a-f]+" | head -1 || true)
bs=$(grep -A12 "^v1:user:main.boundpos.boundpos_pos_bool_int\$" "$INV_OUT" 2>/dev/null | grep -oE "subject digest: [0-9a-f]+" | head -1 || true)
[ -n "$bf" ] && [ -n "$bs" ] && [ "$bf" != "$bs" ] \
  && ok "swapping parameter type positions moves the SUBJECT DIGEST" \
  || no "parameter position does not move the digest ($bf vs $bs)"
# And the generic pair must be refused rather than silently digested.
gf=$(grep -A12 "^v1:user:main.boundpos.boundpos_bounded_first/1\$" "$INV_OUT" 2>/dev/null | grep -c "INCOMPLETE" || true)
[ "${gf:-0}" = "1" ] \
  && ok "a generic with no instantiation mints no digest (refused, not guessed)" \
  || no "a type-erased generic produced a digest"
probe "changing a bound itself moves the facts" "true" \
'#eval
  let a : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" 1, typeBounds := [("", ["Copy"])] }
  let b : CheckedDeclFacts := { id := CallableId.ofUser "m" "f" 1, typeBounds := [("", ["Destroy"])] }
  a.canonical != b.canonical'

echo ""
echo "=== a free identifier is not a name in the bytes ==="
# `.ident` used to emit `g<len>:<name>` for anything the declaration did not
# bind — a raw source name straight into evidence, the same defect class as the
# call names `resolveCall` already fixed, one layer down. Two constants sharing a
# spelling would have digested alike.
FID="tests/programs/subject_free_identifier.con"
[ -f "$FID" ] && ok "the free-identifier fixture is committed" || no "free-identifier fixture missing"
# TRIPWIRE CONVERTED. This leg asserted `covered: false` while a constant had no
# semantic identity to encode. ConstId closed that: this fixture's `LIMIT` is an
# unambiguous module constant, so refusing is no longer the honest answer — it now
# encodes as `k:<ConstId>=<initializer>` and the subject is legitimately covered.
# The property that mattered is unchanged and asserted below: the constant's NAME
# must not be in the bytes, and its VALUE must be.
fid_cov=$("$CC" "$FID" --report subject-facts 2>/dev/null | grep -A7 "fid.clamp" | grep -oE "covered: (true|false)" | head -1 || true)
[ "$fid_cov" = "covered: true" ] \
  && ok "a contract naming a resolved local constant is COVERED via ConstId" \
  || no "a resolved constant is '$fid_cov' — ConstId did not reach this contract"

# The rename/meaning pair, on the constant instead of the ghost. Identity without
# meaning was the ghost-frame mistake; do not repeat it one entity over.
FT="$(mktemp -d)"
sed 's/const LIMIT: Int = 10;/const LIMIT: Int = 20;/' "$FID" > "$FT/v.con"
fid_a=$("$CC" "$FID"      --report subject-facts 2>/dev/null | grep -oE "subject digest: [a-f0-9]+" | head -1 || true)
fid_b=$("$CC" "$FT/v.con" --report subject-facts 2>/dev/null | grep -oE "subject digest: [a-f0-9]+" | head -1 || true)
if [ -z "$fid_a" ] || [ -z "$fid_b" ]; then
  no "constant-initializer probe produced no digest — inconclusive"
elif [ "$fid_a" != "$fid_b" ]; then
  ok "changing a constant's initializer invalidates the dependent subject"
else
  no "a constant initializer change leaves the dependent subject identical"
fi
grep -q "= 20" "$FT/v.con" && ok "the constant-initializer probe genuinely changed the value" \
  || no "the constant-initializer probe changed nothing — it would pass vacuously"
rm -rf "$FT"

# A CONSTANT IS NOT A CALLABLE. Routing an unbound identifier through the CALLABLE
# resolver encoded a constant under a same-named FUNCTION's CallableId — a
# confidently wrong identity, worse than the textual fallback it replaced because
# it looks resolved. The witness declares `LIMIT` as BOTH, which the language
# permits; an earlier version used `LIMIT` and `LIMIT_fn`, two different
# spellings, and so exercised no collision at all.
COLL="tests/programs/subject_const_fn_collision.con"
[ -f "$COLL" ] && ok "the constant/function collision witness is committed" || no "collision witness missing"
coll_cov=$("$CC" "$COLL" --report subject-facts 2>/dev/null | grep -A7 "coll.uses_const" | grep -oE "covered: (true|false)" | head -1 || true)
[ "$coll_cov" = "covered: false" ] \
  && ok "a constant sharing a function's spelling does NOT borrow its identity" \
  || no "the collision case is '$coll_cov' — a constant may be encoded as a function"
# LOOP BINDERS ARE BINDERS. Encoding a loop's clauses with only the function's
# parameters in scope made `#[invariant(0 <= i && i <= 16)]` an unresolved free
# identifier and took the whole subject out — measured on constant_time_tag.
#
# NOT A COUNT OF ZEROES. The earlier version discarded stderr and counted
# INCOMPLETE lines, so a compiler failure that produced NO report yielded zero and
# passed — the vacuity this suite guards against elsewhere. Each example must now
# produce a report SUCCESSFULLY, with a positive subject inventory, and its stderr
# must be classifiable.
for ex in constant_time_tag hmac_sha256 crypto_verify elf_header parse_validate fixed_capacity; do
  out="$TMP/$ex.out"; err="$TMP/$ex.err"
  if ! "$CC" "examples/$ex/src/main.con" --report subject-facts >"$out" 2>"$err"; then
    no "$ex: the compiler failed to produce a subject-facts report"
    continue
  fi
  n_sub=$(grep -c '^v1:' "$out" || true)
  if [ "${n_sub:-0}" -lt 1 ]; then
    no "$ex: report produced but contains NO subjects — a zero here is vacuous"
    continue
  fi
  n_inc=$(grep -cE "INCOMPLETE|ABSENT" "$out" || true)
  tot_e=$(grep -c "^error:" "$err" || true)
  unb_e=$(grep -c "has no stored proof subject" "$err" || true)
  if [ "$n_inc" != "0" ]; then
    no "$ex: $n_inc of $n_sub subjects incomplete"
  elif [ "$tot_e" != "$unb_e" ]; then
    no "$ex: $((tot_e - unb_e)) unclassified stderr diagnostic(s) beyond $unb_e expected unbound-proof errors"
  else
    ok "$ex: $n_sub subjects, 0 incomplete, $tot_e stderr diagnostics all expected"
  fi
done

echo ""
echo "=== loop contracts reach the subject FROM SOURCE ==="
# The field existed and nothing read `f.loopContracts`, so the earlier check only
# proved the field affects canonicalization — not that a source invariant reaches
# it. This drives a real program and edits a real invariant.
CC=".lake/build/bin/concrete"
CT="examples/constant_time_tag/src/main.con"
nloops=$("$CC" "$CT" --report subject-facts 2>/dev/null | grep -oE "loopClauses: [0-9]+" | head -1 | grep -oE "[0-9]+")
# EXACT structure, not "> 0". constant_time_tag's ct_compare has ONE loop with
# ONE invariant and ONE variant, so the encoded clause count is 2 — "loops: 2" is
# a count of CLAUSES, not of loops, and reading it as two loops was my
# mislabelling. A `> 0` assertion would pass on any miscount.
[ "${nloops:-0}" = "2" ] \
  && ok "ct_compare contributes exactly 2 loop clauses (1 invariant + 1 variant)" \
  || no "expected 2 loop clauses for ct_compare, got ${nloops:-0}"
WORK="$TMP/ctedit"; mkdir -p "$WORK"; cp -r examples/constant_time_tag "$WORK/ct"
D1=$("$CC" "$WORK/ct/src/main.con" --report subject-facts 2>/dev/null | grep "subject digest" | head -1)
python3 - "$WORK/ct/src/main.con" <<'PYEOF'
import sys,re
p=sys.argv[1]; s=open(p).read()
s2=re.sub(r'(#\[invariant\([^)]*?)0 <= i', r'\g<1>1 <= i', s, count=1)
open(p,'w').write(s2)
PYEOF
D2=$("$CC" "$WORK/ct/src/main.con" --report subject-facts 2>/dev/null | grep "subject digest" | head -1)
[ -n "$D1" ] && [ "$D1" != "$D2" ] \
  && ok "editing a source invariant moves the subject digest" \
  || no "a source invariant edit did NOT move the digest ($D1 vs $D2)"

echo ""
echo "=== threaded, not recomputed ==="
if grep -q "declFacts.find?" "$ROOT_DIR/Concrete/Proof/ProofCore.lean"; then
  ok "ProofCore reads the facts off the module by IDENTITY"
else
  no "ProofCore does not read threaded facts — they would have to be rebuilt"
fi
if grep -q "subjectDigest" "$ROOT_DIR/Concrete/Proof/ProofCore.lean"; then
  ok "entries carry the subject digest"
else
  no "entries do not carry a subject digest"
fi
# 059/060 ARE CLOSED, AND THIS NOW ASSERTS THE FIX RATHER THAN THE BUG.
#
# This was a tripwire: it grepped for the old body-fingerprint comparison and failed if that
# comparison CHANGED, so that "the digest exists" could not be read as "the bugs are closed". The
# comparison did change — `88145c64 fix: the consistency checker audited freshness with a rule the
# deriver had stopped using` — and the tripwire fired as designed, asking someone to re-examine.
# Verified: ProofCore now routes a v1 stored value to `.needsRecheck`, and does so BEFORE the
# staleness comparison, which is the whole substance. Running the v1 check afterwards would compare
# a v2 digest against a v1 hash and report the guaranteed mismatch as a body change — the defect
# 059/060 named. Asserting the fix is strictly stronger than asserting the bug.
_PC="$ROOT_DIR/Concrete/Proof/ProofCore.lean"
_v1_line="$(grep -n 'storedIsV1 a then \.needsRecheck' "$_PC" | head -1 | cut -d: -f1)"
_stale_line="$(grep -n 'isStale a then \.stale' "$_PC" | head -1 | cut -d: -f1)"
if [ -n "$_v1_line" ] && [ -n "$_stale_line" ] && [ "$_v1_line" -lt "$_stale_line" ]; then
  ok "a v1 stored value becomes needs_recheck, decided BEFORE the staleness comparison (059/060 closed)"
else
  no "the v1 -> needs_recheck routing is missing or is decided AFTER staleness (v1=${_v1_line:-absent} stale=${_stale_line:-absent}) — a v2 digest compared against a v1 hash reports a body change that never happened"
fi
# ...and the status it produces must not count as established for anything downstream.
if grep -qE '^\s*\|.*\.needsRecheck' "$_PC" && \
   grep -A6 'def ObligationStatus.isCurrentForDependents' "$_PC" | has_match 'needsRecheck'; then
  ok "needs_recheck is enumerated as NOT current for dependents"
else
  no "needs_recheck is not excluded from isCurrentForDependents — an older, weaker answer would propagate as established"
fi

GATE_DONE=1
echo ""
echo "SUBJECT-FACTS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
