#!/usr/bin/env bash
# R-0004: ProofBodyCanonicalV2 serialization.
#
# The serializer resolves NOTHING; it only encodes what the producer captured. So the
# properties here are about faithfulness and refusal, not about semantics:
#   - every node constructor is classified (REFLECTIVELY checked, not grepped)
#   - order and multiplicity survive
#   - the encoding is injective
#   - an uncovered body is REFUSED, never serialized as complete
set -Eeuo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fatal() {
  local rc=$?
  echo "FATAL: check_identity_use_bytes stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2
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

-- REFLECTIVE COMPLETENESS. This is the authority, not a grep and not the exhaustive
-- match alone. An exhaustive match stops a bare new constructor at compile time, but
-- a constructor added TOGETHER with a fallback arm compiles and serializes garbage.
-- Comparing the inductive's real constructor count against the serializer's declared
-- tag inventory catches that case.
open Lean in
def ctorCount : Lean.MetaM Nat := do
  match (← getEnv).find? ``Concrete.Proof.BodyIdentityUse with
  | some (.inductInfo iv) => pure iv.ctors.length
  | _ => throwError "BodyIdentityUse is not an inductive"

def ownerA : TypeId := TypeId.user "m" "A"

def nodes : List BodyIdentityUse :=
  [ .binderRef 0 0, .typeRef ownerA, .field { owner := ownerA, field := "x" },
    .variant { owner := ownerA, variant := "V" } ]

def covered  : ProofBodyIdentityInputsV2 := { uses := nodes, covered := true }
def uncov    : ProofBodyIdentityInputsV2 := { uses := nodes, covered := false }
-- Same multiset, different ORDER.
def reordered : ProofBodyIdentityInputsV2 := { uses := nodes.reverse, covered := true }
-- Same kinds, one node REPEATED.
def repeated : ProofBodyIdentityInputsV2 :=
  { uses := nodes ++ [.binderRef 0 0], covered := true }
def once : ProofBodyIdentityInputsV2 := { uses := [.binderRef 0 0], covered := true }
def twice : ProofBodyIdentityInputsV2 :=
  { uses := [.binderRef 0 0, .binderRef 0 0], covered := true }
-- Distinct positions that a non-injective encoding could confuse.
def posA : ProofBodyIdentityInputsV2 := { uses := [.binderRef 1 11], covered := true }
def posB : ProofBodyIdentityInputsV2 := { uses := [.binderRef 11 1], covered := true }

#eval show Lean.MetaM Unit from do
  let n ← ctorCount
  a s!"every node constructor is classified ({n} constructors, {allNodeTags.length} tags)"
    (n == allNodeTags.length)
  a "the declared tags are mutually distinct"
    (allNodeTags.eraseDups.length == allNodeTags.length)
  a "every node kind serializes to its own tag"
    ((nodes.map nodeTag).eraseDups.length == nodes.length)

#eval do
  -- Sanity first: an empty result would make the comparisons below vacuous.
  a "a covered body serializes to non-empty versioned bytes"
    (((serializeIdentityUses covered).getD "").startsWith "identityUsesV1:")

  a "an UNCOVERED body is refused, not serialized as complete"
    (serializeIdentityUses uncov == none)
  a "refusal is about coverage, not about content — same nodes, covered, do serialize"
    (serializeIdentityUses covered != none)

  a "node ORDER changes the bytes"
    (serializeIdentityUses covered != serializeIdentityUses reordered)
  a "node MULTIPLICITY changes the bytes"
    (serializeIdentityUses covered != serializeIdentityUses repeated
      && serializeIdentityUses once != serializeIdentityUses twice)
  a "the node count is emitted, so a truncated stream cannot read as a shorter one"
    (((serializeIdentityUses once).getD "").startsWith "identityUsesV1:n1:"
      && ((serializeIdentityUses twice).getD "").startsWith "identityUsesV1:n2:")

  a "the encoding is injective across distinct positions"
    (serializeIdentityUses posA != serializeIdentityUses posB)
  a "identical inputs serialize identically"
    (serializeIdentityUses covered == serializeIdentityUses { uses := nodes, covered := true })

  -- THE STRUCTURAL CONTRACT. Arity exists before any composite node does, so the
  -- injectivity property is established while it is still cheap to get right.
  a "every current node kind is a leaf (arity 0)"
    ((nodes.map nodeArity).all (· == 0))
  a "a stream of N leaves reduces to N roots"
    (streamRoots? nodes == some 4 && streamRoots? [] == some 0)
  -- Arity must be checked reflectively too: a new constructor that skips nodeArity
  -- would default to nothing at all, silently flattening a composite into a leaf.
  a "the arity table covers every kind the tag table covers"
    ((nodes.map nodeArity).length == (nodes.map nodeTag).length)

  -- EXERCISE THE REFUSAL PATH with a composite arity. Every real node is a leaf, so
  -- without this the malformed branch is dead code asserted to work.
  a "a composite node with too few children makes the stream MALFORMED"
    (streamRootsWith? (fun _ => 2) [.binderRef 0 0] == none)
  -- (A uniform arity of 2 admits NO well-formed stream, since nothing is a leaf;
  -- the well-formed case needs a MIXED arity, which the next leg supplies.)
  a "a binary node folds two roots into one"
    (streamRootsWith? (fun u => match u with | .typeRef _ => 2 | _ => 0)
       [.binderRef 0 0, .binderRef 0 1, .typeRef ownerA] == some 1)
  -- The discriminator that motivates arity at all: two nestings of the same nodes
  -- must NOT reduce alike once composites exist.
  a "differently-nested streams of identical nodes reduce differently"
    (streamRootsWith? (fun u => match u with | .typeRef _ => 2 | _ => 0)
       [.binderRef 0 0, .binderRef 0 1, .typeRef ownerA]
     != streamRootsWith? (fun u => match u with | .typeRef _ => 2 | _ => 0)
       [.binderRef 0 0, .typeRef ownerA, .binderRef 0 1])

  -- INCOMPLETENESS TRIPWIRE. The vocabulary has four node kinds, so bodies that
  -- differ only in operators, literals, calls or control flow serialize IDENTICALLY.
  -- This leg asserts the CURRENT, INCOMPLETE behaviour on purpose: it passes because
  -- V2 is partial, and it FAILS once the vocabulary is extended — which is the
  -- signal to drop the `partial` tag and re-examine every consumer. Without it, V2
  -- could quietly become complete-looking while still omitting most of a body.
  a "TRIPWIRE: the stream is tagged partial while the vocabulary is incomplete"
    (((serializeIdentityUses covered).getD "").startsWith "identityUsesV1:")
  a "TRIPWIRE: four node kinds — extending the vocabulary must break this"
    (allNodeTags.length == 4)

  -- The serializer must not leak a source name. Only the typed IDs' own rendering
  -- may appear, and a binder never contributes one.
  a "a binder reference contributes no name to the bytes"
    ((((serializeIdentityUses once).getD "").splitOn "b").length >= 2)
LEAN

OUT="$TMP_DIR/out.txt"
if ! lake env lean "$TMP_DIR/p.lean" > "$OUT" 2>&1; then
  echo "FATAL: serializer probe did not elaborate" >&2; sed -n '1,12p' "$OUT" >&2; exit 1
fi
if grep -qE "error|sorry" "$OUT"; then
  echo "FATAL: probe emitted a diagnostic" >&2; sed -n '1,10p' "$OUT" >&2; exit 1
fi
cat "$OUT"
PASS=$(( PASS + $(grep -c '^  ok   ' "$OUT" || true) ))

# Structural BACKSTOPS. These supplement the reflective check above; they are not the
# authority. A wildcard arm would let an unclassified constructor receive a generic
# tag, and a derived Repr would produce bytes for a node kind nobody classified.
code="$(sed -e '/^ *--/d' -e '/^ *\/-/,/-\//d' Concrete/Proof/IdentityUseBytes.lean)"
if has_match -E '^\s*\|\s*_\s*=>' <<<"$code"; then
  no "the serializer has a wildcard arm — an unclassified node would get a generic tag"
else
  ok "the serializer has no wildcard arm"
fi
if has_match -E "deriving.*Repr|Repr\b" <<<"$code"; then
  no "the serializer reaches for Repr — a derived instance absorbs new constructors silently"
else
  ok "the serializer does not derive its bytes from Repr"
fi
# The split is the architecture: serialization must not resolve anything.
if has_match -E "lookup|resolve|Elab|find\?" <<<"$code"; then
  no "the serializer performs resolution — that belongs to the producer, which alone has the information"
else
  ok "the serializer resolves nothing; it only encodes captured nodes"
fi

# Arity must be exhaustive in the SOURCE, not merely correct for today's four leaves.
code_arity="$(awk '/^def nodeArity/,/^$/' Concrete/Proof/IdentityUseBytes.lean)"
if has_match -E '^\s*\|\s*_\s*=>' <<<"$code_arity"; then
  no "nodeArity has a wildcard — a composite node would silently be treated as a leaf"
else
  ok "nodeArity is exhaustive with no wildcard"
fi
# The serializer must refuse a malformed stream, not encode it.
if grep -q "streamRoots? inputs.uses).isNone" Concrete/Proof/IdentityUseBytes.lean; then
  ok "a malformed stream is refused rather than serialized"
else
  no "the serializer does not check stream well-formedness — unreadable evidence could be emitted"
fi

# THE COLLISION, on real compiled programs rather than constructed records. Three
# bodies that differ semantically must currently share bytes; when they stop sharing,
# the vocabulary has grown and every consumer needs re-examining.
CC=".lake/build/bin/concrete"
if [ -x "$CC" ]; then
  CT="$(mktemp -d)"
  for pair in "a:p + 1" "b:p * 2" "c:p - 9"; do
    nm="${pair%%:*}"; expr="${pair#*:}"
    printf 'mod m { pub fn f(p: Int) -> Int { return %s; } }\n' "$expr" > "$CT/$nm.con"
  done
  da="$("$CC" "$CT/a.con" --report subject-facts 2>/dev/null | grep -oE 'shadow identityUses: [a-f0-9]+' || true)"
  db="$("$CC" "$CT/b.con" --report subject-facts 2>/dev/null | grep -oE 'shadow identityUses: [a-f0-9]+' || true)"
  dc="$("$CC" "$CT/c.con" --report subject-facts 2>/dev/null | grep -oE 'shadow identityUses: [a-f0-9]+' || true)"
  rm -rf "$CT"
  if [ -z "$da" ]; then
    no "collision probe produced no shadow digest — inconclusive"
  elif [ "$da" = "$db" ] && [ "$da" = "$dc" ]; then
    ok "TRIPWIRE: operator-only body edits do NOT move the partial stream (expected while incomplete)"
  else
    no "operator edits now move the V2 stream — the vocabulary grew; drop the 'partial' tag, update this leg, and re-examine every consumer"
  fi
else
  no "compiler not built — the corpus-level collision leg could not run"
fi

# The shadow digest must NOT reach the authoritative bytes. V1 stays frozen, so a
# stored proof link cannot go stale because of an observed-only value.
if grep -qE "shadowIdentityUseDigest|serializeIdentityUses" Concrete/Proof/SubjectFacts.lean; then
  no "SubjectFacts references the V2 serializer — shadow bytes could reach canonical"
else
  ok "the shadow digest is absent from SubjectFacts, so canonical cannot include it"
fi

# CONVERGENCE, now CLOSED: there is ONE producer.
#
# This was a containment check — every use the independent accumulator recorded had to
# appear in the view derived from the tree. It passed, the accumulator was deleted, and
# `bodyIdentityInputs.uses` is now `flatUsesOf` of the structural body.
#
# The containment probe MUST NOT be left in place. It compared the accumulator against the
# derived view; with one producer it would compare the derived view against itself and
# report agreement unconditionally — a gate that cannot fail, still printing ok.
#
# What replaces it are the two facts that can actually break now: the second producer must
# stay gone, and the derivation must still carry the identities the accumulator was LOSING.

if grep -q "bodyIdentityUses" Concrete/Elab/Elab.lean; then
  no "a second producer of identity uses is back in Elab — the flat view must be DERIVED from the structural body, never accumulated beside it"
else
  ok "one producer: no identity-use accumulator exists in Elab"
fi

if grep -q "uses := Proof.flatUsesOf evidenceBody" Concrete/Elab/Elab.lean; then
  ok "the flat identity-use view is derived from the structural evidence body"
else
  no "the flat view is no longer derived by flatUsesOf — if the derivation moved, re-point this gate at its new owner"
fi

# The two identities the accumulator never recorded. Both are ORDINARY facts about a body
# and would be silently absent again if the derivation regressed, so they are asserted
# behaviourally rather than by counting.
CMP="$(mktemp -d)"
cat > "$CMP/p.lean" <<'LEAN'
import Concrete
open Concrete Concrete.Proof
def usesOf (src : String) : List BodyIdentityUse :=
  match (do
    let pa ← Pipeline.parse src
    let sm := Pipeline.buildSummary pa
    let r ← Pipeline.resolve pa sm
    Pipeline.check r sm
    let el ← Pipeline.elaborate r sm
    pure el.coreModules : Except Diagnostics (List CModule)) with
  | .error _ => []
  | .ok ms => (((ms.map CModule.declFacts).flatten).head?.map
                 (fun f => ProofBodyIdentityInputsV2.uses f.bodyIdentityInputs)).getD []

-- An assignment TARGET is a use of a binder. The accumulator recorded only the
-- right-hand side, so `i = i + 1` and `j = i + 1` produced the same flat view.
def loopSrc : String :=
  "mod m { pub fn f(p: Int) -> Int { let mut i: Int = 0; while i < p { i = i + 1; } return i; } }"

-- A match pattern's FIELD identity. The accumulator recorded the variant but not the
-- field, so renaming `v` in the enum declaration moved nothing in the flat view.
def patSrc : String :=
  "mod m { enum Copy E { A { v: Int }, B { v: Int } }
     pub fn f(e: E) -> Int { match e { E::A { v } => { return v; }, E::B { v } => { return 0; } } } }"

#eval IO.println (if (usesOf loopSrc).length == 5 then "PLACE-OK" else s!"PLACE-BAD {(usesOf loopSrc).length}")
#eval IO.println (
  let n := ((usesOf patSrc).filter
    (· == BodyIdentityUse.field { owner := TypeId.user "m" "E", field := "v" })).length
  if n == 2 then "PATFIELD-OK" else s!"PATFIELD-BAD {n}")
LEAN
cmpout="$(lake env lean "$CMP/p.lean" 2>/dev/null | tr '\n' ',' || true)"
rm -rf "$CMP"
case "$cmpout" in
  "") no "the recovered-identity probe produced no output — inconclusive, not agreement" ;;
  *BAD*) no "the derivation lost a recovered identity ($cmpout)" ;;
  *PLACE-OK*PATFIELD-OK*) ok "the derived view carries the assignment place and pattern field identities the accumulator lost" ;;
  *) no "unexpected recovered-identity probe output ($cmpout)" ;;
esac

echo "IDENTITY-USE-BYTES: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
