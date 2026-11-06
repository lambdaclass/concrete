import Concrete.Proof.SubjectFacts
import Concrete.Proof.DependencyEdge

/-! # Deterministic dependency roots (R-0004 slice 6)

Freshness computed TRANSITIVELY, so that a deep callee edit stales every claim
that depends on it, while recursion stays finite and nothing about source
location or alpha-renaming becomes semantic.

## The shape chosen, and the one rejected

A Merkle DAG over the SCC condensation is the textbook answer: condense cycles,
then hash each node from its own content plus its children's roots. It shares
structure, so a root can be recomputed incrementally.

This uses a REACHABILITY-CLOSURE digest instead: a node's root is its own subject
digest plus the sorted subject digests of everything reachable from it. The two
agree on the properties that matter here —

* deterministic (the reachable set is sorted by identity, never by insertion
  order or source position);
* finite under recursion (a reachable set cannot grow past the node count, so
  mutual recursion terminates rather than diverging);
* every member of a cycle gets the SAME CLOSURE — they are mutually reachable, so
  the set is entry-point-independent, which is the property SCC condensation
  provides. Their ROOTS still differ, because a root binds the node's own subject
  as well as its closure and two functions in a cycle are two different subjects.
  (An earlier draft of this header claimed cycle members share a root. They do
  not, and should not; the test that asserts closure agreement is the honest form
  of the property.);
* a deep callee edit moves every dependent's root.

— and the closure form is materially easier to check: the root is a function of a
SET, so verifying "this root binds exactly these subjects" needs no reasoning
about traversal order or condensation correctness. What it gives up is incremental
sharing: recomputing one root costs its whole closure. That is the right trade
while the corpus is dozens of functions, and the wrong one if it becomes
thousands — recorded here so the decision is revisited on evidence rather than
rediscovered.

## Never under-approximate

An edge to a callee with no known digest is REFUSED. An earlier version bound it
as `unknown:<id>` and reported completeness through a separate flag; that let a
root be minted over a partly unknown graph, and a flag beside a value invites
comparing the value and ignoring the flag. Refusing is the only answer that
cannot be misread.
-/

namespace Concrete.Proof

/-- One node: an identity, the subject digest of that declaration, and its TYPED
    outgoing edges.

    `digest` is an `Option`, never a string that may be empty. An empty digest is
    indistinguishable from a computed one at the type level, and a root built over
    "" would be a confident value derived from a missing subject.

    Edges carry their kind, so the root can refuse to rest on a `missing` one
    rather than treating every callee as equivalent. -/
structure DepNode where
  /-- The compiler-minted semantic identity, NOT a name.

      This was a raw `String`, which is the defect class R-0004 exists to close
      appearing inside the dependency material itself: a name is a
      representation, two callables can share one, and a rename must not move a
      node. Keyed on the canonical rendering so comparison is the identity's own
      notion of equality rather than an arbitrary string's. -/
  id      : DefinitionIdentity
  /-- The compiler-local NAME, for diagnosis only. NEVER compared — comparison is on `id`. A
      dependency root that keyed on this would justify one program's closure with another program's
      nodes whenever the names agreed, which is the defect the correspondence join already closed
      and this boundary was still open to. -/
  label   : CallableId
  digest  : Option String
  /-- Typed edges to SCOPED identities, for the same reason. -/
  edges   : List (DependencyEdge × DefinitionIdentity)
  /-- Edges whose callee has no scoped identity. Carried, never dropped: a root computed over a
      closure missing an edge is a confident answer over material it never saw. Any non-empty list
      REFUSES the root. -/
  unscoped : List (CallableId × DependencyEdge) := []
deriving Repr

/-- Canonical rendering, for SERIALIZATION and DIAGNOSTICS only. Lookup and
    deduplication compare `CallableId` directly; routing them through this would
    reinstate the rendered string as operational identity. -/
def DepNode.render (d : DepNode) : String := d.id.canonical

/-- Why a root could not be built. Carried rather than collapsed to `none`, so a
    caller can say WHICH fail-closed condition fired instead of reporting a
    generic absence. -/
inductive DepRootError where
  | missingStart (id : DefinitionIdentity)
  | duplicateId (id : DefinitionIdentity)
  | incompleteDigest (id : DefinitionIdentity)
  | unresolvedEdge (from_ to_ : DefinitionIdentity)
  | missingEdge (from_ to_ : DefinitionIdentity)
  /-- An edge whose callee has no scoped identity at all. Distinct from `unresolvedEdge`, which is
      an edge to an identity with no node: here there is no identity to look up. -/
  | unscopedEdge (from_ : DefinitionIdentity) (to_ : CallableId)
deriving Repr

/-- Rendering happens HERE and nowhere else in this module: a diagnostic is the
    one place a name is the right answer. -/
def DepRootError.explain : DepRootError → String
  | .missingStart id      => s!"no node for start identity '{id.localName}'"
  | .duplicateId id       => s!"duplicate node identity '{id.localName}'"
  | .incompleteDigest id  => s!"'{id.localName}' has no subject digest"
  | .unresolvedEdge f t   => s!"'{f.localName}' depends on '{t.localName}', which has no node"
  | .missingEdge f t      => s!"'{f.localName}' has a non-current edge (`missing` or `unclassified`) to '{t.localName}'"
  | .unscopedEdge f t     => s!"'{f.localName}' depends on '{t.render}', which has no scoped identity"

/-- Identities reachable from `start` over typed edges.

    Bounded by the node count: each round adds at least one identity or stops, and
    there are finitely many. That bound is what makes recursion terminate rather
    than diverge. -/
def reachableFrom (nodes : List DepNode) (start : DefinitionIdentity) : List DefinitionIdentity :=
  let succs := fun (n : DefinitionIdentity) =>
    match nodes.find? (fun d => d.id.sameDefinition n) with
    | some d => d.edges.map Prod.snd
    | none   => []
  let rec go (fuel : Nat) (frontier acc : List DefinitionIdentity) : List DefinitionIdentity :=
    match fuel with
    | 0 => acc
    | Nat.succ f =>
      let next := (frontier.flatMap succs).eraseDups
      let fresh := next.filter (fun n => !acc.contains n)
      if fresh.isEmpty then acc else go f fresh (acc ++ fresh)
  let seed := (succs start).eraseDups
  (go nodes.length seed seed).eraseDups

/-- One validated result: the preimage and the trust qualification together.

    Returned as a STRUCTURE rather than a preimage plus a separate boolean,
    because a flag beside a value is an invitation to compare the value and
    ignore the flag — which is precisely how an unqualified `proved_by_lean`
    would get minted over a trust boundary. -/
structure DependencyRootMaterial where
  preimage     : String
  carriesTrust : Bool
deriving Repr, BEq, Inhabited

/-- The canonical PREIMAGE of a dependency root, plus its trust qualification.

    A serialization, NOT a compact hash and NOT a Merkle root — it shares no
    structure and does not shrink. A consumer wanting a fixed-width value hashes
    it; calling it a root oversold what it is.

    THE EDGES ARE IN THE BYTES. An earlier version serialized only reachable
    identities and their digests, so changing an edge from `contract` to `body`
    produced the SAME preimage — the typed edges were traversed and then thrown
    away, which defeats the point of typing them. Each edge now contributes its
    canonical kind, its target identity and that target's length-prefixed digest.

    FAIL CLOSED, reason carried. Every one of these would otherwise yield a
    confident value from incomplete information: a missing start; a duplicate
    identity (first-match lookup would hide the conflict); an absent OR EMPTY
    subject digest; an edge to an identity with no node; an edge typed
    `missing`. -/
def dependencyRootMaterial (nodes : List DepNode) (id : DefinitionIdentity)
    : Except DepRootError DependencyRootMaterial := do
  -- Comparison and deduplication are on the IDENTITY, not on a rendering. An
  -- earlier version stored `CallableId` and then keyed every lookup on
  -- `.render`, which kept rendered strings as the operational identity under a
  -- typed wrapper — the very thing the type was introduced to remove.
  let ids := nodes.map (·.id)
  match ids.find? (fun n => (ids.filter (· == n)).length > 1) with
  | some dup => throw (.duplicateId dup)
  | none => pure ()
  let start ← match nodes.find? (fun d => d.id == id) with
    | some d => pure d
    | none   => throw (.missingStart id)
  let digestOf := fun (n : DefinitionIdentity) => do
    match nodes.find? (fun d => d.id == n) with
    | none   => throw (.unresolvedEdge id n)
    | some d => match d.digest with
      | some v => if v.isEmpty then throw (.incompleteDigest n) else pure v
      | none   => throw (.incompleteDigest n)
  let ownDigest ← digestOf start.id
  -- `.render` appears from here on ONLY to impose a total order and to build the
  -- serialization. Those are the two places a canonical spelling is the point.
  let allNodes := (id :: reachableFrom nodes id).eraseDups.mergeSort
    (fun a b => a.canonical ≤ b.canonical)
  let closure := allNodes.filter (· != id)
  let mut parts : List String := []
  let mut trust := false
  for n in allNodes do
    let node ← match nodes.find? (fun d => d.id == n) with
      | some d => pure d
      | none   => throw (.unresolvedEdge id n)
    let nDigest ← digestOf n
    -- AN UNKEYABLE EDGE REFUSES THE ROOT. It cannot be traversed and must not be skipped: a closure
    -- computed without it is a confident answer over material the compiler never examined. Checked
    -- per node in the closure, not only at the start, because a dependency's unkeyable edge makes
    -- the dependant's closure incomplete just as surely.
    match node.unscoped with
    | (c, _) :: _ => throw (.unscopedEdge n c)
    | [] => pure ()
    let nr := n.canonical
    parts := parts ++ [s!"N{nr.length}:{nr}:{nDigest.length}:{nDigest}"]
    -- edges deduplicated as TYPED pairs, then ordered for serialization
    let sorted := node.edges.eraseDups.mergeSort
      fun a b => (a.1.canonical ++ a.2.canonical) ≤ (b.1.canonical ++ b.2.canonical)
    for (k, tgt) in sorted do
      -- Refuse on any edge that is not current for dependents, rather than on `missing` alone.
      -- The literal check admitted `unclassified` the moment that constructor existed: an edge
      -- nobody has classified would have been traversed as though validated. Routing through
      -- the predicate means a new edge kind is refused until someone decides otherwise, which
      -- is the fail-closed default.
      if !k.isCurrentForDependents then throw (.missingEdge n tgt)
      let tgtDigest ← digestOf tgt
      if k == DependencyEdge.trusted then trust := true
      let tr := tgt.canonical
      parts := parts ++
        [s!"E{k.canonical.length}:{k.canonical}:{tr.length}:{tr}:{tgtDigest.length}:{tgtDigest}"]
  -- V3 (R-0482): every node renders `defIdV2`, scoped by its owner's scope rather than the
  -- over-bound package digest, so a root now moves with what the claim reaches and nothing else.
  return { preimage := "depRootPreimageV3:" ++ s!"o{ownDigest.length}:{ownDigest}"
                        ++ s!"n{closure.length}" ++ String.join parts
         , carriesTrust := trust }

/-- Trust for an ALREADY-VALIDATED material. Takes the material rather than the
    raw graph, so there is no public path that answers the trust question over an
    unvalidated graph.

    A previous `closureCarriesTrust (nodes) (id)` existed beside
    `dependencyRootMaterial` and would happily answer for a graph the material
    would have refused — a fail-open alternative sitting next to the fail-closed
    one, which is the more attractive of the two to call. -/
def DependencyRootMaterial.requiresTrustQualification (m : DependencyRootMaterial) : Bool :=
  m.carriesTrust

end Concrete.Proof
