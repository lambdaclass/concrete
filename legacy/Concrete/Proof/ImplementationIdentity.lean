import Concrete.Proof.IdentityUseBytes
import Concrete.Proof.Digest
import Concrete.Proof.BodyIdentity

/-!
# Implementation identity

What a callable IS — signature, generics, capabilities, contracts and structural body — with no
proof-link metadata. Separate from the proof SUBJECT, which additionally binds the selected
specification and claim scope.

**Placement below `DependencyEdge` is load-bearing.** The manifest that a `body` dependency edge
binds against must compute its own digests rather than accept them from a caller: a public entry
point taking `(CallableId x String x String)` cannot tell a computed digest from a well-formed
invented one. Computing them inside the manifest constructor requires this module to be reachable
from `DependencyEdge`, and it is: `SubjectFacts`, `EvidenceTree` and `IdentityUseBytes` mention
`DependencyEdge` nowhere, and `shortHash` lives in `Digest`.

Moved verbatim from `ProofCore`. The V2 preimage bytes are frozen and this move does not touch
them — `implementationPreimage` is shared with `proofSubjectDigestV2`, which is why the
domain-separating prefix is applied to the HASH rather than to the preimage.
-/

namespace Concrete

/-- The IMPLEMENTATION preimage: what a callable IS, with no proof-link metadata.

    Identity, typed signature, generics, capabilities and contracts (via `facts.canonical`) plus
    the structural body. Deliberately excludes the selected specification and claim scope: those
    say what a particular proof CLAIMS about this implementation, and an implementation does not
    change when a different proof is pointed at it.

    Exposed as the preimage rather than only as a digest so `proofSubjectDigestV2` can extend it
    without nesting a hash — which would change V2's bytes and break its freeze for a refactor
    that adds no information. -/
def implementationPreimage (facts : Proof.CheckedDeclFacts)
    (body : Proof.CompleteEvidenceBodyV2) : String :=
  "subjectV2:" ++ facts.canonical ++ "|body:" ++ shortHash (Proof.bodyBytesV2 body)

/-- The implementation digest — the identity a `body` dependency edge should bind.

    Distinct from the proof-subject digest, and the distinction is the point: a table entry
    describes an implementation, so binding the full subject there would couple table membership
    to which specification a proof link happened to select. -/
def implementationDigest (facts : Proof.CheckedDeclFacts)
    (body : Proof.CompleteEvidenceBodyV2) : String :=
  -- DOMAIN-SEPARATED. The preimage begins `subjectV2:` because it is shared with
  -- `proofSubjectDigestV2`, whose bytes are frozen. Hashing it unprefixed would give the
  -- implementation identity the same domain as a proof subject over the same inputs — two
  -- different identities whose values could not be told apart by their construction. The prefix
  -- is on the HASH, not the preimage, so V2 values stay byte-identical.
  shortHash ("implementationV1:" ++ implementationPreimage facts body)


/-! ## A complete implementation, as one inseparable record

The manifest must not accept digests from a caller: a public entry point taking
`(CallableId x String x String)` cannot tell a computed digest from a well-formed invented one.
Computing them inside the constructor requires the constructor to receive the INPUTS instead — and
those inputs must not be independently pairable, or the forgery just moves one level up.

FOUR fields, not the three the design sketch listed, and the fourth is forced by what a manifest row
actually holds. The row's body component is `sourceBodyDigestV1Of` of the EXTRACTED `PExpr` — the
same value a table entry stores, which is the whole point of carrying it — and that is a different
representation from `CompleteEvidenceBodyV2`. Deriving one from the other is not available, so the
extracted body travels with the rest.

**WHAT THIS RECORD DOES AND DOES NOT ESTABLISH.** `of?` verifies `facts.id == callable` and that the
facts are complete; the body is a `CompleteEvidenceBodyV2`, so its validation is discharged by the
type rather than re-checked. It does NOT establish that all four parts came from the same
`ProofCoreEntry`:

  `CompleteEvidenceBodyV2` carries only `val.statements` — NO identity. Neither does `PExpr`. So
  pairing entry A's body or extracted expression with entry B's facts and `CallableId` satisfies
  every check available here and silently yields a digest for an implementation that does not exist.

That is a NAMED GAP, not a covered case. It cannot be closed from this module: `ProofCoreEntry` is
defined above it, so a `private mk` here is unreachable from a producer that walks entries, which is
why `of?` is a validating function rather than a sealed factory. Closing it needs the evidence body
to carry its owning `CallableId`, and that is a V2 schema change — deliberately not done while the
freeze holds. The `facts`/`callable` mispairing IS caught; the `body`/`extracted` mispairing is not,
and no control may be written that implies otherwise. -/
structure CompleteImplementation where
  private mk ::
  callable  : CallableId
  facts     : Proof.CheckedDeclFacts
  body      : Proof.CompleteEvidenceBodyV2
  /-- The extracted proof-model body. Present because the manifest row's comparable component is
      its V1 source-body digest, which no other field can produce. -/
  extracted : Proof.PExpr

/-- Build a complete implementation, or refuse.

    Refuses when the facts describe a DIFFERENT callable than the one claimed, which is the
    mispairing this record exists to prevent, and when the facts are incomplete, since a digest over
    incomplete facts is a digest over an unknown. -/
def CompleteImplementation.of? (callable : CallableId) (facts : Proof.CheckedDeclFacts)
    (body : Proof.CompleteEvidenceBodyV2) (extracted : Proof.PExpr)
    : Option CompleteImplementation :=
  if facts.id != callable then none
  else if !facts.isComplete then none
  else some (CompleteImplementation.mk callable facts body extracted)

/-- The authoritative V1 source-body component — the value a table entry stores, computed by the
    same single producer, so the join compares a value against itself rather than an approximation. -/
def CompleteImplementation.sourceBodyComponent (ci : CompleteImplementation) : String :=
  sourceBodyDigestV1Of ci.extracted

/-- The authoritative implementation digest. -/
def CompleteImplementation.implementationComponent (ci : CompleteImplementation) : String :=
  implementationDigest ci.facts ci.body

end Concrete
