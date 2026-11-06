#!/usr/bin/env bash
# SHADOW BODY V2 — the structural body reaches the report, and nothing else.
#
# `--report subject-facts` now prints two shadow lines. They answer different questions and
# the distinction is the whole reason both exist:
#
#   shadow identityUses  digests the flat list of identity REFERENCES. Measured on the
#                        corpus, `p + 1`, `p * 2` and `p - 9` hash identically there — it is
#                        a reference inventory, not a body representation.
#   shadow bodyV2        digests the STRUCTURAL body, where those three differ.
#
# Both are shadow. The authoritative subject digest stays V1-frozen, and this gate asserts
# that it stays that way: structural bytes reaching a STATUS is the thing that must not
# happen by accident.
set -Eeuo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/code_refs.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
fatal() { local rc=$?; echo "FATAL: check_shadow_body_v2 stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2; exit "$rc"; }
trap fatal ERR
PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

BIN=".lake/build/bin/concrete"
[ -x "$BIN" ] || { echo "FATAL: build first (missing $BIN)" >&2; exit 1; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# --- containment: the structural bytes must not reach anything authoritative -----------
# Exactly two references are expected: the definition, and the report line. A third is not
# automatically wrong, but it must be looked at, which is why this asserts the set rather
# than a count.
# OWNER SET UPDATED 2026-08-09: ProofCore joins it, because `proofSubjectDigestV2` now binds the
# structural body instead of the legacy Core-statement hash. That is the point of slice 5 — the
# subject the shadow comparison digests must be the real one — and it is why a file-list proxy
# alone is no longer the right guard.
#
# The property this section actually protects is one step further in: no STATUS may depend on
# structural bytes before the step-7 migration. The owner list is kept as a cheap tripwire (a
# FOURTH owner still needs looking at), and the property is asserted directly below it, because
# a proxy that has to be widened every time the design advances stops meaning anything.
# The tripwire measures CODE references, not prose. It used to `grep -rln`, which counts any
# mention -- and on 2026-08-11 that fired on a DOC COMMENT in `DependencyEdge` explaining where the
# manifest's implementation digest comes from. Documenting the containment boundary is not crossing
# it, and a guard that cannot tell the difference trains you to widen its allowlist for prose,
# which is exactly how an allowlist stops meaning anything.
#
# So: strip Lean comments (`/- ... -/` blocks, including `/-- ... -/`, and `--` to end of line),
# THEN look for the identifier. Narrowing what the tripwire reads is only safe if it still catches a
# real reference, so `code_refs` is self-tested below on a synthetic tree before it is trusted here.
# SELF-TEST of the stripper, before its verdict is used for anything. A narrowed guard that has not
# been shown to still fire is indistinguishable from a disabled one.
ST="$TMP/selftest"
if code_refs_selftest "$ST" bodyBytesV2; then
  ok "the owner tripwire ignores comment-only mentions and still catches real code references"
else
  no "owner-tripwire self-test failed — the tripwire's reading is not trustworthy"
fi

refs="$(code_refs bodyBytesV2 Concrete/ Main.lean)"
# OWNER SET UPDATED 2026-08-11: `ProofCore` LEAVES and `ImplementationIdentity` ENTERS. This is a
# relocation, not an expansion -- `implementationPreimage` moved verbatim into its own module so the
# manifest constructor can compute digests instead of accepting them from a caller, and the set is
# still three files. An owner set that GREW would need the argument to be made again; one where a
# name is substituted for the same code does not, and recording which of the two happened is the
# reason this asserts the set rather than a count.
if [ "$refs" = "Concrete/Proof/IdentityUseBytes.lean Concrete/Proof/ImplementationIdentity.lean Concrete/Report/Report.lean " ]; then
  ok "bodyBytesV2 owners are the definition, the implementation digest, and the shadow report line"
else
  no "bodyBytesV2 reached a new owner ($refs) — if a STATUS now depends on structural bytes, that is the V1 freeze breaking"
fi

if grep -q "bodyBytesV2" Concrete/Proof/SubjectFacts.lean 2>/dev/null; then
  no "SubjectFacts references bodyBytesV2 — structural bytes could enter the canonical subject digest"
else
  ok "the canonical subject digest cannot include structural body bytes"

# THE PROPERTY, not the proxy — INVERTED at the V2 activation (2026-08-17). This asserted that no
# STATUS reads the subject digest, which was the whole meaning of "still shadow": the live verdict
# could not move when the structural body did. The migration has since happened deliberately and
# WITH its manifest (`--report migration`, applied by scripts/gen/apply_v2_migration.sh across all
# three corpora), so the requirement is now the opposite one.
#
# It matters that this is asserted rather than merely stopped being asserted. If `subjectDigest` ever
# stopped reaching the deriver, freshness would silently fall back to the body-only fingerprint —
# which is exactly bugs 059 and 060 reopening, since that hash sees neither declared types nor
# contracts. The former tripwire becomes the guard on the thing it used to forbid.
if grep -A2 "deriveObligationStatus e.eligibility" Concrete/Proof/ProofCore.lean | has_match "subjectDigest"; then
  ok "the status derivation consumes the subject digest — freshness is on V2, not the legacy fingerprint"
else
  no "deriveObligationStatus no longer receives the subject digest — freshness has fallen back to the body-only fingerprint, which is bugs 059 and 060 reopening"
fi
fi

# --- behaviour: the structural digest SEPARATES bodies the flat digest merges -----------
# This is the property the whole representation exists for. The flat view of these three
# bodies is identical (one binder reference each); only the structural bytes differ.
mk() { printf 'mod m { pub fn f(p: Int) -> Int { return %s; } }\n' "$1" > "$2"; }
mk "p + 1" "$TMP/a.con"
mk "p * 2" "$TMP/b.con"
mk "p - 9" "$TMP/c.con"

# `|| true` at the end: under `set -o pipefail` a non-matching grep makes this return
# non-zero, the ERR trap fires, and the gate DIES instead of reporting "line missing".
# An absent measurement must reach the case statements below as an empty string.
# Anchored on the PREFIX, not `s/.*: //`: the refused form is
# "REFUSED (1 gap(s): desugared for-loop)" and a greedy match strips up to the LAST
# colon, yielding "desugared for-loop)" — which then fails the digest/refusal case
# analysis below for a reason that has nothing to do with the compiler.
line_of() { "$BIN" "$1" --report subject-facts 2>/dev/null | grep "shadow $2:" | head -1 | sed "s/^ *shadow $2: //" || true; }

fa="$(line_of "$TMP/a.con" bodyV2)"; fb="$(line_of "$TMP/b.con" bodyV2)"; fc="$(line_of "$TMP/c.con" bodyV2)"
ua="$(line_of "$TMP/a.con" identityUses)"; ub="$(line_of "$TMP/b.con" identityUses)"

if [ -z "$fa" ] || [ -z "$fb" ] || [ -z "$fc" ]; then
  no "the shadow bodyV2 line is missing — inconclusive, not agreement"
elif [ "$fa" != "$fb" ] && [ "$fb" != "$fc" ] && [ "$fa" != "$fc" ]; then
  ok "structural bytes separate p+1, p*2 and p-9"
else
  no "structural bytes MERGE operator-different bodies (a=$fa b=$fb c=$fc) — the representation is not describing the body"
fi

# The companion fact, asserted so the two lines are never assumed to be redundant. If this
# ever starts differing, the flat digest gained body structure and this gate should say so.
if [ -n "$ua" ] && [ "$ua" = "$ub" ]; then
  ok "the flat identity-use digest still merges them, which is why it is not a body representation"
else
  no "the flat digest now separates p+1 from p*2 ($ua vs $ub) — re-examine what identityUses means before trusting either line"
fi

# --- the desugared for-loop --------------------------------------------------------------
# `for (init; cond; step) { body }` lowers to `init; while cond { body; step }` and the
# evidence mirrors that LOWERING, not the surface form. Each of the four parts must move
# the digest, or the evidence describes a loop the program does not run.
forloop() { printf 'mod m { pub fn f(n: Int) -> Int { let mut t: Int = 0;\n  for (%s; %s; %s) { %s }\n  return t; } }\n' "$1" "$2" "$3" "$4" > "$5"; }

forloop "let mut i: Int = 0" "i < n" "i = i + 1" "t = t + i;"  "$TMP/f_base.con"
forloop "let mut i: Int = 5" "i < n" "i = i + 1" "t = t + i;"  "$TMP/f_init.con"
forloop "let mut i: Int = 0" "i < t" "i = i + 1" "t = t + i;"  "$TMP/f_cond.con"
forloop "let mut i: Int = 0" "i < n" "i = i + 2" "t = t + i;"  "$TMP/f_step.con"
forloop "let mut i: Int = 0" "i < n" "i = i + 1" "t = t + 1;"  "$TMP/f_body.con"

base="$(line_of "$TMP/f_base.con" bodyV2)"
case "$base" in
  REFUSED*|ABSENT*|"") no "a plain for-loop does not digest ($base) — the producer should describe it" ;;
  *)                   ok "a desugared for-loop digests" ;;
esac

for part in init cond step body; do
  v="$(line_of "$TMP/f_$part.con" bodyV2)"
  if [ -z "$base" ] || [ -z "$v" ]; then
    no "for-loop $part probe produced no digest — inconclusive"
  elif [ "$v" != "$base" ]; then
    ok "changing the for-loop $part moves the structural digest"
  else
    no "the for-loop $part is INVISIBLE to the structural digest — two different loops share bytes"
  fi
done

# The loop FRAME. `break` inside a for-loop must resolve to a relative loop depth. Before
# the evidence was wired, `forLoop` never pushed `loopFrames` — invisible only because the
# whole construct was a gap, since a gap node discards its subtree.
cat > "$TMP/f_break.con" <<'CON'
mod m { pub fn f(n: Int) -> Int { let mut t: Int = 0;
  for (let mut i: Int = 0; i < n; i = i + 1) { t = t + i; if t > 10 { break; } }
  return t; } }
CON
b="$(line_of "$TMP/f_break.con" bodyV2)"
case "$b" in
  *"loop target"*|*"LoopTarget"*) no "a break inside a for-loop has no enclosing loop frame ($b)" ;;
  REFUSED*|ABSENT*|"")            no "the for-loop break probe did not digest ($b)" ;;
  *)                              ok "a break inside a for-loop resolves to its enclosing loop" ;;
esac

# --- constructs wired in the 2a assembly pass -------------------------------------------
# Each was a gap that discarded information the site had already computed. The legs below
# assert the information is now DISTINGUISHING, not merely present.

# Some probes declare helper functions or an impl method, which are subjects too, so the
# digest must be selected by CALLABLE IDENTITY. Neither `head -1` nor `tail -1` works:
# subjects are not emitted in declaration order (`m.f` precedes `m.P_get` even though the
# impl is written first), and picking by position silently compared the wrong subject —
# this leg passed by hand and failed in the gate for exactly that reason.
#
# Reads the `shadow bodyV2` line inside the block introduced by `v1:user:<id>`.
digest_of() {
  "$BIN" "$1" --report subject-facts 2>/dev/null \
    | awk -v want="v1:user:$2" '
        $0 == want { inblock = 1; next }
        /^v1:user:/ { inblock = 0 }
        inblock && /shadow bodyV2:/ { sub(/^ *shadow bodyV2: /, ""); print; exit }
      ' || true
}

# FIELD PLACE. The written field's OWNER is part of the place: two structs with a
# same-spelled field are different writes. Previously the place was a gap, so every field
# assignment in the corpus was described identically.
# The bodies must differ ONLY in the write's OWNER — which took two corrections, both found
# by mutating the owner to a constant and watching the leg still pass:
#
#   1. ending `return x.v` let the field READ (its own owner-bearing FieldId) carry the
#      difference;
#   2. taking `(a: A, b: B)` and using one or the other made the initializers read
#      different BINDER POSITIONS.
#
# Both probes therefore take a single parameter at position 0 and never read the field
# back, leaving the write's owner as the only thing that differs. Parameter types differ
# between the two programs, but parameters are not part of the BODY digest.
cat > "$TMP/fp_a.con" <<'CON'
mod m { pub struct Copy A { pub v: Int } pub struct Copy B { pub v: Int }
  pub fn f(a: A) -> Int { let mut x: A = a; x.v = 1; return 0; } }
CON
cat > "$TMP/fp_b.con" <<'CON'
mod m { pub struct Copy A { pub v: Int } pub struct Copy B { pub v: Int }
  pub fn f(b: B) -> Int { let mut x: B = b; x.v = 1; return 0; } }
CON
fpa="$(digest_of "$TMP/fp_a.con" m.f)"; fpb="$(digest_of "$TMP/fp_b.con" m.f)"
case "$fpa" in
  REFUSED*|ABSENT*|"") no "a field assignment does not digest ($fpa)" ;;
  *) if [ "$fpa" != "$fpb" ]; then
       ok "the field place carries its owning type, so A.v and B.v are different writes"
     else
       no "writing A.v and B.v produce the SAME bytes — the place is keyed on spelling, not owner"
     fi ;;
esac

# The field NAME, separately from its owner. A mutation that blanked the name SURVIVED the
# owner leg above — both writes kept distinct owners, so they still differed — which is why
# the two halves of a FieldId need two probes rather than one.
cat > "$TMP/fp_v.con" <<'CON'
mod m { pub struct Copy A { pub v: Int, pub w: Int }
  pub fn f(a: A) -> Int { let mut x: A = a; x.v = 1; return x.v; } }
CON
cat > "$TMP/fp_w.con" <<'CON'
mod m { pub struct Copy A { pub v: Int, pub w: Int }
  pub fn f(a: A) -> Int { let mut x: A = a; x.w = 1; return x.v; } }
CON
fpv="$(digest_of "$TMP/fp_v.con" m.f)"; fpw="$(digest_of "$TMP/fp_w.con" m.f)"
if [ -z "$fpv" ] || [ -z "$fpw" ]; then
  no "the field-name probe produced no digest"
elif [ "$fpv" != "$fpw" ]; then
  ok "the field place carries which field is written, so x.v = 1 and x.w = 1 differ"
else
  no "writing x.v and x.w produce the SAME bytes — the field name is not in the place"
fi

# DISCARD. Described as the intrinsic call it is, WITH its argument: `discard(e)` runs `e`.
cat > "$TMP/dsc_a.con" <<'CON'
mod m { pub fn f(p: Int) -> Int { discard(p + 1); return 0; } }
CON
cat > "$TMP/dsc_b.con" <<'CON'
mod m { pub fn f(p: Int) -> Int { discard(p * 2); return 0; } }
CON
da="$(digest_of "$TMP/dsc_a.con" m.f)"; db="$(digest_of "$TMP/dsc_b.con" m.f)"
case "$da" in
  REFUSED*|ABSENT*|"") no "discard() does not digest ($da)" ;;
  *) if [ "$da" != "$db" ]; then
       ok "discard() carries its argument, so discarding different expressions differs"
     else
       no "discard(p+1) and discard(p*2) share bytes — the argument is not carried"
     fi ;;
esac

# METHOD RECEIVER. The self argument was a gap, so every method call on a given callee was
# described identically regardless of what it was called on.
cat > "$TMP/mc_a.con" <<'CON'
mod m { pub struct Copy P { pub v: Int }
  impl P { pub fn get(&self) -> Int { return self.v; } }
  pub fn f(a: P, b: P) -> Int { return a.get(); } }
CON
cat > "$TMP/mc_b.con" <<'CON'
mod m { pub struct Copy P { pub v: Int }
  impl P { pub fn get(&self) -> Int { return self.v; } }
  pub fn f(a: P, b: P) -> Int { return b.get(); } }
CON
ma="$(digest_of "$TMP/mc_a.con" m.f)"; mb="$(digest_of "$TMP/mc_b.con" m.f)"
case "$ma" in
  REFUSED*|ABSENT*|"") no "a method call does not digest ($ma)" ;;
  *) if [ "$ma" != "$mb" ]; then
       ok "the method receiver is part of the call, so a.get() and b.get() differ"
     else
       no "a.get() and b.get() share bytes — the receiver is not in the evidence"
     fi ;;
esac

# IF-EXPRESSION. Its own constructor and its own byte tag, separate from the statement
# `branch`. Each part must move the digest.
ifexpr() { printf 'mod m { pub fn f(p: Int) -> Int { let x: Int = if %s { %s } else { %s }; return x; } }\n' "$1" "$2" "$3" > "$4"; }
ifexpr "p > 0" "1" "2" "$TMP/ie_base.con"
ifexpr "p > 7" "1" "2" "$TMP/ie_cond.con"
ifexpr "p > 0" "9" "2" "$TMP/ie_then.con"
ifexpr "p > 0" "1" "9" "$TMP/ie_else.con"
ieb="$(digest_of "$TMP/ie_base.con" m.f)"
case "$ieb" in
  REFUSED*|ABSENT*|"") no "an if-expression does not digest ($ieb)" ;;
  *)                   ok "an if-expression digests" ;;
esac
for part in cond then else; do
  v="$(digest_of "$TMP/ie_$part.con" m.f)"
  if [ -z "$ieb" ] || [ -z "$v" ]; then no "if-expression $part probe produced no digest"
  elif [ "$v" != "$ieb" ]; then ok "changing the if-expression $part moves the digest"
  else no "the if-expression $part is INVISIBLE to the digest"; fi
done

# The if-EXPRESSION and the if-STATEMENT must not share bytes. These are different
# programs, so a difference is expected — the leg exists to catch the two node kinds being
# given one encoding, which would make it an accident that they differ at all.
cat > "$TMP/ie_stmt.con" <<'CON'
mod m { pub fn f(p: Int) -> Int { let mut x: Int = 2; if p > 0 { x = 1; } else { x = 2; } return x; } }
CON
ist="$(digest_of "$TMP/ie_stmt.con" m.f)"
if [ -n "$ist" ] && [ "$ist" != "$ieb" ]; then
  ok "the if-expression and the if-statement do not share an encoding"
else
  no "an if-expression and an if-statement produced the same bytes ($ieb) — one value-bearing, one not"
fi

# BUG 068. A ghost binding is ERASED before Core: its initializer never runs, cannot trap,
# and produces no runtime value. A runtime binding does all three. Both emitted the same
# `letBind` node, so the two digested identically and a proof over one stayed valid-looking
# after a change to the other — freshness failing through a construct nobody had probed.
printf 'mod m { pub fn f(p: Int) -> Int { ghost let g: Int = p + 1; return p; } }\n' > "$TMP/gl.con"
printf 'mod m { pub fn f(p: Int) -> Int {       let g: Int = p + 1; return p; } }\n' > "$TMP/rl.con"
gl="$(digest_of "$TMP/gl.con" m.f)"; rl="$(digest_of "$TMP/rl.con" m.f)"
if [ -z "$gl" ] || [ -z "$rl" ]; then
  no "the ghost/runtime let probe produced no digest"
elif [ "$gl" != "$rl" ]; then
  ok "a ghost let and a runtime let do not share bytes (bug 068)"
else
  no "ghost let and runtime let share bytes — erasure is invisible to the subject (bug 068 regressed)"
fi

# --- the type vocabulary (EvidenceTypeRef) ----------------------------------------------
# `TypeId` names only nominal types, so an array element type and a cast target had no way
# to be written and were gaps. `tyCanonical`/`boundTyCanonical` cover primitives but render
# a named type by its SOURCE SPELLING, so neither could be reused here. The replacement is
# structural over primitives AND nominal-by-identity, with type variables by binder
# position. These legs assert all three of those properties.

printf 'mod m { pub fn f(p: Int) -> i32 { return p as i32; } }\n' > "$TMP/c32.con"
printf 'mod m { pub fn f(p: Int) -> i16 { return p as i16; } }\n' > "$TMP/c16.con"
c32="$(digest_of "$TMP/c32.con" m.f)"; c16="$(digest_of "$TMP/c16.con" m.f)"
case "$c32" in
  REFUSED*|ABSENT*|"") no "a cast does not digest ($c32)" ;;
  *) if [ "$c32" != "$c16" ]; then ok "the cast TARGET type is in the digest (i32 vs i16)"
     else no "x as i32 and x as i16 share bytes — the cast target is invisible"; fi ;;
esac

printf 'mod m { pub fn f() -> Int { let a: [Int; 2] = [1, 2]; return 0; } }\n' > "$TMP/aInt.con"
printf 'mod m { pub fn f() -> Int { let a: [i32; 2] = [1, 2]; return 0; } }\n' > "$TMP/a32.con"
printf 'mod m { pub fn f() -> Int { let a: [Int; 2] = [2, 1]; return 0; } }\n' > "$TMP/aOrd.con"
ai="$(digest_of "$TMP/aInt.con" m.f)"; a32="$(digest_of "$TMP/a32.con" m.f)"; aor="$(digest_of "$TMP/aOrd.con" m.f)"
case "$ai" in
  REFUSED*|ABSENT*|"") no "an array literal does not digest ($ai)" ;;
  *) if [ "$ai" != "$a32" ]; then ok "the array ELEMENT TYPE is in the digest ([Int;2] vs [i32;2])"
     else no "[Int;2] and [i32;2] share bytes — the element type is invisible"; fi
     if [ "$ai" != "$aor" ]; then ok "array elements are ordered, so [1,2] and [2,1] differ"
     else no "[1,2] and [2,1] share bytes — element order or the elements themselves are lost"; fi ;;
esac

# NOMINAL BY IDENTITY, not spelling — the property that ruled out reusing tyCanonical.
# The two bodies are textually IDENTICAL and neither reads a field, so the element type is
# the only thing that can differ. (A `return q.x` here would make the leg pass through the
# field READ's owner-bearing FieldId instead; that mistake cost three iterations earlier.)
cat > "$TMP/nom.con" <<'CON'
mod a { pub struct Copy Point { pub x: Int }
  pub fn f(p: Point) -> Int { let arr: [Point; 1] = [p]; return 0; } }
mod b { pub struct Copy Point { pub x: Int }
  pub fn f(p: Point) -> Int { let arr: [Point; 1] = [p]; return 0; } }
CON
na="$(digest_of "$TMP/nom.con" a.f)"; nb="$(digest_of "$TMP/nom.con" b.f)"
if [ -z "$na" ] || [ -z "$nb" ]; then
  no "the nominal-identity probe produced no digest"
elif [ "$na" != "$nb" ]; then
  ok "a nominal element type is carried BY IDENTITY: same-spelled Point in two modules differs"
else
  no "a.Point and b.Point share bytes as an element type — the type is keyed on spelling"
fi

# ALPHA-INVARIANCE, the same rule binderRef follows: a type parameter is a POSITION.
# Generic subjects render with an arity suffix (`m.f/1`), not a bare name.
printf 'mod m { pub fn f<T: Copy>(x: T) -> Int { let a: [T; 1] = [x]; let y: T = a[0]; return 0; } }\n' > "$TMP/tvT.con"
printf 'mod m { pub fn f<Zed: Copy>(x: Zed) -> Int { let a: [Zed; 1] = [x]; let y: Zed = a[0]; return 0; } }\n' > "$TMP/tvZ.con"
tvt="$(digest_of "$TMP/tvT.con" m.f/1)"; tvz="$(digest_of "$TMP/tvZ.con" m.f/1)"
if [ -z "$tvt" ] || [ -z "$tvz" ]; then
  no "the type-parameter probe produced no digest — inconclusive, not invariance"
elif ! diff -q "$TMP/tvT.con" "$TMP/tvZ.con" >/dev/null 2>&1 && [ "$tvt" = "$tvz" ]; then
  # THE CONTROL, not decoration. "renaming does NOT move the digest" is TRUE when no rename
  # happened, so an edit that made the two probes identical would turn this into a
  # permanent pass. check_binder_refs carries the same control ("the rename probe genuinely
  # changed the program"); this gate was written later and lacked it.
  ok "renaming a type parameter does NOT move the digest (binder position, not spelling), and the probes genuinely differ"
else
  no "renaming T to Zed moved the digest — a type variable is encoded by spelling"
fi

# A gap INSIDE A TYPE must make the subject incomplete. The gap traversal originally
# discarded the type field with `_`, so `typeRefGaps` was unreachable and an unresolvable
# element type validated as a COMPLETE body — fail-open, and silent, because it is not a
# type error. A function type is the construct `evTypeRef` currently refuses (encoding one
# means encoding a capability SET, a separate identity question), which makes it the probe
# that keeps this reachable.
cat > "$TMP/fnty.con" <<'CON'
mod m {
  fn g(x: Int) -> Int { return x + 1; }
  pub fn f() -> Int { let h: fn(Int) -> Int = g; let arr: [fn(Int) -> Int; 1] = [h]; return 0; }
}
CON
ft="$(digest_of "$TMP/fnty.con" m.f)"
case "$ft" in
  REFUSED*"function type"*) ok "a gap inside a TYPE refuses the subject, naming the type" ;;
  REFUSED*)                 no "refused, but not for the type reason ($ft)" ;;
  "")                       no "the type-gap probe produced no line" ;;
  *)                        no "a body with an unresolvable element TYPE digested ($ft) — type gaps are being swallowed" ;;
esac

# --- proof-only predicates: assert and assume -------------------------------------------
# Neither was elaborated at all (39 refusals), because their predicates may legally read
# ghost bindings that `elabExprEv` rejects. They are now elaborated for EVIDENCE ONLY, with
# the Core discarded — a proof-only construct must not generate code.
printf 'mod m { pub fn f(n: Int) -> Int { assert(n > 0); return n; } }\n'  > "$TMP/as0.con"
printf 'mod m { pub fn f(n: Int) -> Int { assume(n > 0); return n; } }\n'  > "$TMP/am0.con"
printf 'mod m { pub fn f(n: Int) -> Int { assume(n > 1); return n; } }\n'  > "$TMP/am1.con"
as0="$(digest_of "$TMP/as0.con" m.f)"; am0="$(digest_of "$TMP/am0.con" m.f)"; am1="$(digest_of "$TMP/am1.con" m.f)"
case "$as0" in
  REFUSED*|ABSENT*|"") no "an assert does not digest ($as0)" ;;
  *)                   ok "an assert predicate reaches the digest" ;;
esac
# ASSERT AND ASSUME MUST NEVER COLLIDE: one is discharged, the other is relied upon.
if [ -n "$am0" ] && [ "$as0" != "$am0" ]; then
  ok "assert and assume with the SAME predicate do not share bytes"
else
  no "assert(n>0) and assume(n>0) share bytes ($as0) — one is discharged, the other relied upon"
fi
if [ -n "$am1" ] && [ "$am0" != "$am1" ]; then
  ok "the predicate itself is in the digest (assume n>0 vs n>1)"
else
  no "assume(n>0) and assume(n>1) share bytes — the predicate is not carried"
fi

# FAIL-SAFE. Nothing elaborated these before, so a throw here is a program that used to
# build and now does not. A predicate reading a `ghost let` is legal and `elabExprEv`
# rejects it, which makes it the probe for that boundary: it must REFUSE, not error.
cat > "$TMP/ghostref.con" <<'CON'
mod m {
  pub fn f(n: Int) -> Int {
    ghost let g: Int = n + 1;
    assert(g > 0);
    return n;
  }
}
CON
if ! "$BIN" "$TMP/ghostref.con" --report subject-facts >/dev/null 2>&1; then
  no "an assert reading a ghost binding FAILED THE COMPILATION — elaborating proof predicates must never break a program that used to build"
else
  gr="$(digest_of "$TMP/ghostref.con" m.f)"
  case "$gr" in
    REFUSED*"ghost"*) ok "a predicate that cannot elaborate refuses the subject instead of failing the build" ;;
    REFUSED*)         no "refused, but not for the predicate reason ($gr)" ;;
    *)                no "a predicate reading an erased ghost binding produced a DIGEST ($gr)" ;;
  esac
fi

# THE ASSUMPTION AXIS. `assume` must not yield an unqualified claim, so the axis has to
# SEE it. Before the predicates were elaborated an assume emitted a gap, so the axis
# counted zero for the only construct that feeds it — vacuous, though fail-closed, because
# the gap refused the subject anyway.
AX="$(mktemp -d)"
cat > "$AX/p.lean" <<'LEAN'
import Concrete
open Concrete Concrete.Proof
def bodyOf (src : String) : Option EvidenceBodyDraftV2 :=
  match (do
    let pa ← Pipeline.parse src
    let sm := Pipeline.buildSummary pa
    let r ← Pipeline.resolve pa sm
    Pipeline.check r sm
    let el ← Pipeline.elaborate r sm
    pure el.coreModules : Except Diagnostics (List CModule)) with
  | .error _ => none
  | .ok ms => ((ms.map CModule.evidenceBodies).flatten).head?.map Prod.snd
def n (src : String) : Nat :=
  match bodyOf src with
  | none => 999
  | some b => (b.statements.flatMap stmtAssumptions).length
#eval IO.println s!"assume={n "mod m { pub fn f(k: Int) -> Int { assume(k > 0); return k; } }"} assert={n "mod m { pub fn f(k: Int) -> Int { assert(k > 0); return k; } }"} plain={n "mod m { pub fn f(k: Int) -> Int { return k; } }"}"
LEAN
axout="$(lake env lean "$AX/p.lean" 2>/dev/null | tr -d '
' || true)"
rm -rf "$AX"
case "$axout" in
  "assume=1 assert=0 plain=0") ok "the assumption axis sees an assume, and does not count an assert (discharged, not assumed)" ;;
  "")                          no "the assumption-axis probe produced no output — inconclusive" ;;
  *)                           no "the assumption axis is wrong ($axout) — expected assume=1 assert=0 plain=0" ;;
esac

# IMPORTED IMPL METHODS. `importedCallIds` searched only a module's FUNCTIONS, so a method
# on an imported type had no CallableId and every call to one refused the subject — 18 of
# the last 21 refusals, and not a resolution defect: the table never held the entries.
#
# Asserts identity, not just presence: same-spelled method on same-spelled types in two
# modules must stay distinct, or an import would launder one type's method into another's.
cat > "$TMP/imp.con" <<'CON'
mod a { pub struct Copy P { pub v: Int } impl P { pub fn get(&self) -> Int { return self.v; } } }
mod b { pub struct Copy P { pub v: Int } impl P { pub fn get(&self) -> Int { return self.v + 1; } } }
mod c { import a.{P}; pub fn f(x: P) -> Int { return x.get(); } }
mod d { import b.{P}; pub fn f(x: P) -> Int { return x.get(); } }
CON
ic="$(digest_of "$TMP/imp.con" c.f)"; id_="$(digest_of "$TMP/imp.con" d.f)"
case "$ic" in
  REFUSED*|ABSENT*|"") no "a call to an imported impl method does not digest ($ic)" ;;
  *) if [ "$ic" != "$id_" ]; then
       ok "imported impl methods resolve, and a.P_get is not b.P_get"
     else
       no "a.P.get() and b.P.get() share bytes — an imported method is identified by spelling, not defining module"
     fi ;;
esac

# --- refusal: a body with gaps prints its REASONS, never a digest -----------------------
# A function type is what `evTypeRef` still refuses, and it is the last construct in the
# corpus with a stable refusal. (This probe has been an array literal and then an assert;
# both now digest.)
# Note the ARRAY. A bare `let h: fn(Int) -> Int = g` digests, because `letBind` passes
# `none` for its declared type — the fn type never reaches evidence there. Only a position
# that actually carries a type (here the array's element type) can produce the gap.
cat > "$TMP/gap.con" <<'CON'
mod m {
  fn g(x: Int) -> Int { return x + 1; }
  pub fn f() -> Int { let h: fn(Int) -> Int = g; let arr: [fn(Int) -> Int; 1] = [h]; return 0; }
}
CON
# By IDENTITY: this probe declares a helper `m.g` as well, and `line_of` took the
# FIRST line — the helper's, which digests fine. Same positional-selection mistake the
# receiver leg hit earlier.
g="$(digest_of "$TMP/gap.con" m.f)"
case "$g" in
  REFUSED*gap*) ok "a body the producer cannot describe is refused with its reasons" ;;
  "")           no "no shadow bodyV2 line for the gap probe — inconclusive" ;;
  ABSENT*)      no "the gap probe threaded no structural body at all ($g) — that is a different fault from a gap" ;;
  *)            no "a body with an unhandled construct produced a DIGEST ($g) — a partial body must not be comparable" ;;
esac

# The reasons must NAME the construct. The nine gap codes are stable and few, so a tally of
# codes says "statements and expressions" — true and useless. The detail is what drives the
# remaining producer work.
case "$g" in
  *"function type"*) ok "the refusal names the construct, not just its gap code" ;;
  *)                 no "the refusal does not name the unhandled construct ($g)" ;;
esac

# --- the DEPENDENCY axis (step 3, shadow) ------------------------------------------------
# A `constRef` names a constant and says nothing about its value, so a proof over a body
# reading LIMIT stayed valid-looking when LIMIT changed from 10 to 99. `shadow deps`
# digests the initializer encodings of exactly the constants a body reaches.
#
# THE TWO HALVES MUST BOTH HOLD. The dependency digest has to MOVE with the value, and the
# SUBJECT digest has to STAY — the latter already drives freshness verdicts, so moving it
# before the step-5 migration would strand every stored proof link. A change that satisfied
# only the first half would be the wrong fix shipped early.
deps_of() {
  "$BIN" "$1" --report subject-facts 2>/dev/null \
    | awk -v w="v1:user:$2" '$0==w{i=1;next} /^v1:user:/{i=0} i&&/shadow deps:/{sub(/^ *shadow deps: /,"");print;exit}' || true
}
subj_of() {
  "$BIN" "$1" --report subject-facts 2>/dev/null \
    | awk -v w="v1:user:$2" '$0==w{i=1;next} /^v1:user:/{i=0} i&&/subject digest:/{sub(/^ *subject digest: /,"");print;exit}' || true
}
printf 'mod m { const LIMIT: Int = 10;\n  pub fn f(p: Int) -> Bool { return p < LIMIT; } }\n' > "$TMP/d10.con"
printf 'mod m { const LIMIT: Int = 99;\n  pub fn f(p: Int) -> Bool { return p < LIMIT; } }\n' > "$TMP/d99.con"
printf 'mod m { pub fn f(p: Int) -> Bool { return p < 10; } }\n' > "$TMP/dnone.con"

d10="$(deps_of "$TMP/d10.con" m.f)"; d99="$(deps_of "$TMP/d99.con" m.f)"
s10="$(subj_of "$TMP/d10.con" m.f)"; s99="$(subj_of "$TMP/d99.con" m.f)"
dnone="$(deps_of "$TMP/dnone.con" m.f)"

case "$d10" in
  ""|ABSENT*|REFUSED*) no "a body reading a module constant has no dependency digest ($d10)" ;;
  *) if [ "$d10" != "$d99" ]; then
       ok "the dependency digest MOVES when a constant's value changes (10 vs 99)"
     else
       no "LIMIT=10 and LIMIT=99 produce the same dependency digest — the constant's MEANING is not bound"
     fi ;;
esac

if [ -n "$s10" ] && [ "$s10" = "$s99" ]; then
  ok "the SUBJECT digest does not move — dependency material stays shadow until the migration"
else
  no "the subject digest moved with a constant's value ($s10 vs $s99). It drives freshness verdicts, so this strands every stored proof link; dependency material must stay shadow until step 5"
fi

case "$dnone" in
  none*) ok "a body referencing no constants says so, distinctly from a bound-but-empty digest" ;;
  *)     no "a body with no constant references reported '$dnone'" ;;
esac

# NOT GATED, and why: the REFUSED path (a referenced constant with no binding) is
# unreachable today because constants cannot be imported — `import lib.{LIMIT}` fails with
# "'LIMIT' is not public in module 'lib'" regardless of `pub`. The branch is written the
# correct way round (refuse rather than digest a partial dependency root) and will become
# reachable if constants gain an import path. Same shape as the E0264 alias limitation.
#
# The CORPUS barely exercises this axis: 431 of 432 subjects reference no constant at all,
# because only 2 example files declare one. That is a real measurement, not a producer
# blindness — verified by counting `const` declarations — but it means these constructed
# probes, not corpus coverage, are what tests dependency binding.

# --- the ASSUMPTION axis (checker assumptions, shadow) -----------------------------------
# The verification charter requires a result to record its checker assumptions, and item 5
# of its contract is "accepted as an explicit assumption". An assumption does not make a
# body incomplete — it QUALIFIES any claim about it, which is why this is a third line and
# not folded into completeness. A body full of `assume` can be perfectly complete; what
# must not happen is a proof over it surfacing as unqualified.
asm_of() {
  "$BIN" "$1" --report subject-facts 2>/dev/null \
    | awk -v w="v1:user:$2" '$0==w{i=1;next} /^v1:user:/{i=0} i&&/shadow assumes:/{sub(/^ *shadow assumes: /,"");print;exit}' || true
}
printf 'mod m { pub fn f(n: Int) -> Int { assume(n > 0); return n; } }\n' > "$TMP/q1.con"
printf 'mod m { pub fn f(n: Int) -> Int { assume(n > 1); return n; } }\n' > "$TMP/q2.con"
printf 'mod m { pub fn f(n: Int) -> Int { assume(n > 0); assume(n < 9); return n; } }\n' > "$TMP/q3.con"
printf 'mod m { pub fn f(n: Int) -> Int { assume(n < 9); assume(n > 0); return n; } }\n' > "$TMP/q4.con"
printf 'mod m { pub fn f(n: Int) -> Int { assert(n > 0); return n; } }\n' > "$TMP/q5.con"
printf 'mod m { pub fn f(n: Int) -> Int { return n; } }\n'               > "$TMP/q6.con"

q1="$(asm_of "$TMP/q1.con" m.f)"; q2="$(asm_of "$TMP/q2.con" m.f)"
q3="$(asm_of "$TMP/q3.con" m.f)"; q4="$(asm_of "$TMP/q4.con" m.f)"
q5="$(asm_of "$TMP/q5.con" m.f)"; q6="$(asm_of "$TMP/q6.con" m.f)"

case "$q1" in
  ""|ABSENT*)      no "an assuming body has no assumption line ($q1)" ;;
  unqualified*)    no "a body containing assume() reports UNQUALIFIED — a proof over it would hide what it rests on" ;;
  *)               ok "an assuming body is marked qualified, with a digest of what it assumes" ;;
esac

if [ -n "$q2" ] && [ "$q1" != "$q2" ]; then
  ok "the assumption PREDICATE is in the digest (n>0 vs n>1)"
else
  no "assume(n>0) and assume(n>1) share an assumption digest — the content is not carried"
fi

# ORDER and MULTIPLICITY. Two assumes are not one, and swapping them changes the body, so
# this is a sequence and not a set.
if [ -n "$q3" ] && [ -n "$q4" ] && [ "$q3" != "$q4" ]; then
  ok "assumption order is semantic: swapping two assumes changes the digest"
else
  no "reordering two assumes left the assumption digest unchanged — it is being treated as a set"
fi

# ASSERT IS DISCHARGED, NOT ASSUMED. Conflating them would let a proof lean on something
# it was supposed to prove.
case "$q5" in
  unqualified*) ok "assert does not qualify a claim — it is discharged, not assumed" ;;
  *)            no "a body with only an assert reports assumptions ($q5)" ;;
esac
case "$q6" in
  unqualified*) ok "a body assuming nothing says so positively" ;;
  *)            no "a body with no assume reported '$q6'" ;;
esac

# --- the DEPENDENCY EDGE SET (DepNode material, shadow) ----------------------------------
# `DependencyRoot` has had ZERO producers. This is the material it consumes, and it comes
# from the EVIDENCE BODY rather than `buildCallGraph`: that graph keys on qualified NAME
# STRINGS, which DepNode's own comment calls the defect class R-0004 exists to close
# appearing inside the dependency material itself. Every `call` and `fnRef` node already
# carries a resolver-minted CallableId.
edges_of() {
  "$BIN" "$1" --report subject-facts 2>/dev/null \
    | awk -v w="v1:user:$2" '$0==w{i=1;next} /^v1:user:/{i=0} i&&/shadow edges:/{sub(/^ *shadow edges: /,"");print;exit}' || true
}
two_fns='mod m { fn g(x: Int) -> Int { return x + 1; } fn h(x: Int) -> Int { return x + 2; }'
printf '%s\n  pub fn f(p: Int) -> Int { return g(p); } }\n'          "$two_fns" > "$TMP/eg.con"
printf '%s\n  pub fn f(p: Int) -> Int { return h(p); } }\n'          "$two_fns" > "$TMP/eh.con"
printf '%s\n  pub fn f(p: Int) -> Int { return g(p) + h(p); } }\n'   "$two_fns" > "$TMP/egh.con"
printf 'mod m { pub fn f(p: Int) -> Int { return p + 1; } }\n'         > "$TMP/enone.con"
printf '%s\n  pub fn f(p: Int) -> Int { let k: fn(Int) -> Int = g; return k(p); } }\n' "$two_fns" > "$TMP/efg.con"
printf '%s\n  pub fn f(p: Int) -> Int { let k: fn(Int) -> Int = h; return k(p); } }\n' "$two_fns" > "$TMP/efh.con"
printf 'mod m { fn g(x: Int) -> Int { return x + 1; } pub fn f(zzz: Int) -> Int { let yyy: Int = zzz; return g(yyy); } }\n' > "$TMP/eren.con"
printf 'mod m { fn g(x: Int) -> Int { return x + 1; } pub fn f(p: Int) -> Int { let q: Int = p; return g(q); } }\n'         > "$TMP/eorig.con"

eg="$(edges_of "$TMP/eg.con" m.f)"; eh="$(edges_of "$TMP/eh.con" m.f)"
egh="$(edges_of "$TMP/egh.con" m.f)"; enone="$(edges_of "$TMP/enone.con" m.f)"
efg="$(edges_of "$TMP/efg.con" m.f)"; efh="$(edges_of "$TMP/efh.con" m.f)"
eren="$(edges_of "$TMP/eren.con" m.f)"; eorig="$(edges_of "$TMP/eorig.con" m.f)"

case "$eg" in
  ""|ABSENT*|none*) no "a body calling a function has no edge set ($eg)" ;;
  *)                ok "a call produces a dependency edge, by identity" ;;
esac
if [ -n "$eh" ] && [ "$eg" != "$eh" ]; then
  ok "calling a DIFFERENT function changes the edge set"
else
  no "calling g and calling h produce the same edge set — the callee identity is not carried"
fi
case "$egh" in
  *"m.g"*"m.h"*) ok "both callees appear when a body reaches two" ;;
  *)             no "a body calling g and h did not list both ($egh)" ;;
esac
case "$enone" in
  none*) ok "a body reaching no callable says so" ;;
  *)     no "a body with no calls reported '$enone'" ;;
esac

# `fnRef` IS an edge. Taking a function as a VALUE is a dependency on it: behaviour changes
# when that function's body changes, whether it is called here or handed elsewhere. Without
# this a higher-order program looks dependency-free.
if [ -n "$efg" ] && [ -n "$efh" ] && [ "$efg" != "$efh" ]; then
  ok "a function used as a VALUE is an edge (fn ptr to g vs h differ)"
else
  no "assigning g or h to a fn-typed local produced the same edge set — fnRef is not an edge, so higher-order dependencies vanish"
fi

# Identity, not spelling: renaming binders must not move the material.
if ! diff -q "$TMP/eren.con" "$TMP/eorig.con" >/dev/null 2>&1 && [ -n "$eren" ] && [ "$eren" = "$eorig" ]; then
  # Same control: the two probes must actually be different programs, or "renaming changes
  # nothing" is a statement about nothing.
  ok "renaming locals does not move the edge set, and the probes genuinely differ"
else
  no "renaming binders moved the dependency edge set ($eorig vs $eren)"
fi

# NOT GATED: self-edges. The traversal keeps them — a recursive function does depend on
# itself, and excluding `self` is the containment pass's POLICY, not a property of the
# material. It cannot be observed here because recursion excludes a function from the
# provable subset entirely ("profile: recursion (direct)"), so a recursive function is
# never a subject and has no report line.

# --- ratchet: corpus coverage must not regress ------------------------------------------
# Measured 2026-08-06: 421 of 432 subjects digest, 0 absent. Trail: 292 -> 316 (for-loop)
# -> 323 (assembly rows) -> 376 (EvidenceTypeRef) -> 411 (assert/assume predicates)
# -> 421 (imported impl methods reach the callee table).
#
# The 11 left are three distinct causes, each named at its site: 5 incomplete callee
# identity (a generic callable whose type arguments are not recorded), 4 method calls
# whose mangled name still has no CallableId, 2 trait methods on a type parameter (not
# one function before monomorphization, and needing a TraitId that does not exist yet).
#
# A GAP NODE DISCARDS ITS SUBTREE, so this number does not move by the count of refusals
# closed. Wiring the for-loop closed 54 of them and coverage rose by 24: the other 30 bodies
# contained a second unhandled construct that the for-loop gap had been hiding, and the cast
# row went 11 -> 23 for exactly that reason. Read the per-construct counts as a lower bound
# on remaining work, never as a partition of it.
#
# The corpus is NAMED and its existence asserted. The first version of this gate passed
# `tests/valid_programs examples proofs` to find with stderr silenced; the first does not
# exist and the third holds no .con files, so it measured `examples/` alone while its own
# message claimed three corpora. A ratchet over a silently smaller corpus is the
# green-for-the-wrong-reason failure — the floor stays satisfied precisely because the
# subjects that would break it were never measured.
#
# The missing directory also made `find` exit non-zero inside a process substitution, which
# tripped the ERR trap and printed FATAL while the gate went on to report PASS=9 FAIL=0.
CORPORA=(examples)
for d in "${CORPORA[@]}"; do
  if [ -d "$d" ]; then
    ok "corpus directory present: $d"
  else
    no "corpus directory $d is MISSING — the coverage floor below would be measured over less than it claims"
  fi
done

: > "$TMP/all.txt"
# No `| head -N`. `head` closes the pipe, `find` takes SIGPIPE, and under `set -o pipefail`
# that kills the gate mid-measurement — which reads as a failure of the thing measured.
while IFS= read -r f; do
  "$BIN" "$f" --report subject-facts 2>/dev/null | grep "shadow bodyV2:" >> "$TMP/all.txt" || true
done < <(find "${CORPORA[@]}" -type f -name '*.con' | sort)

total="$(wc -l < "$TMP/all.txt" | tr -d ' ')"
digested="$(grep -cv 'REFUSED\|ABSENT' "$TMP/all.txt" || true)"
absent="$(grep -c 'ABSENT' "$TMP/all.txt" || true)"

if [ "$total" -lt 400 ]; then
  no "only $total subjects measured — the corpus shrank or the report changed shape; a smaller corpus makes the ratchet below meaningless"
else
  ok "corpus measured: $total subjects across ${CORPORA[*]}"
fi

# ABSENT is a different fault from REFUSED: it means no structural body was threaded at
# all, so the producer never ran rather than declining to describe something.
if [ "$absent" -eq 0 ]; then
  ok "every subject has a structural body threaded (0 absent)"
else
  no "$absent subjects have NO structural body — the body is not reaching ProofCore for them"
fi

FLOOR=421
if [ "$digested" -ge "$FLOOR" ]; then
  ok "structural coverage $digested/$total (floor $FLOOR)"
else
  no "structural coverage REGRESSED to $digested/$total, below the $FLOOR floor — a construct the producer used to describe now yields a gap"
fi

echo "SHADOW-BODY-V2: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
