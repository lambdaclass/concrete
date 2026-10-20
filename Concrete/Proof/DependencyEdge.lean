import Concrete.Proof.Proof
import Concrete.Proof.BodyIdentity
import Concrete.Proof.ImplementationIdentity

/-! # Typed proof-dependency edges (R-0004 step 6)

What a caller RELIES ON when it depends on a callee, and therefore what
invalidates it. Four kinds, and the kind is DERIVED from what the theorem
actually uses — never selected by an author:

| edge | the caller relies on | invalidated by |
| --- | --- | --- |
| `contract` | the callee's proved contract | that contract, or the callee's proof receipt, changing |
| `body` | the exact callee implementation | the callee's body / type / semantic digest changing |
| `trusted` | a declared trust boundary | the boundary changing; and the trust PROPAGATES |
| `missing` | nothing validated | always: the caller is `depsNotCurrent` |

Deriving rather than declaring is the whole point. A mode flag would let an
author assert a relationship the proof does not have — claim `contract` while the
proof unfolds a concrete table, and an implementation change that preserves the
contract would then not stale a caller that actually depends on the body.

## Why the signal is structural, not textual

A `contract` proof quantifies over the table: `∀ (fns : FnTable), …`, so it holds
for ANY table meeting its hypotheses and no callee body can affect it. A `body`
proof names concrete tables, so it is about those exact entries.

Source text cannot tell these apart — `fns` and `combineFns` are both just
identifiers on the page. The distinction exists only after elaboration, in whether
the `FnTable`-typed subterm is a bound variable or a constant.

Measured over the corpus before building this: 271 theorems depend on an
`FnTable`; 113 quantify over it, 158 name constants, and ZERO do both. No
tie-break rule is needed because no theorem needs one.

## `trusted` and `missing` come from elsewhere

Those two are not properties of the theorem's type. A dependency is `trusted`
because the callee declares a trust boundary, and `missing` because nothing
validated it. They are recorded here so consumers handle four cases rather than
two, and so `trusted`'s obligation is stated once: it may count as CURRENT for
traversal, but its trust propagates into the caller's evidence assumptions — a
proof reaching one records `proved_by_lean_modulo_trusted`, never unqualified
`proved_by_lean`.

## Future owner

Per the roadmap this belongs under `Concrete/Proof/Core/DependencyEdge.lean`,
with the classifier in `Concrete/Proof/Extract/DependencyEdges.lean`. That split
depends on R-0114-R-0118 (the import-layer work), so it lands flat here for now
with the intended home recorded rather than forgotten.
-/

namespace Concrete.Proof

/-- What a caller relies on in a dependency. See the module header for the
    invalidation rule attached to each. -/
inductive DependencyEdge where
  /-- The callee's proved contract. Survives an implementation change that
      preserves the contract. -/
  | contract
  /-- The exact callee implementation. Any relevant body change stales the
      caller. -/
  | body
  /-- A declared trust boundary. Counts as current for traversal, but the trust
      PROPAGATES into the caller's evidence assumptions. -/
  | trusted
  /-- Nothing validated. The caller is `depsNotCurrent`. -/
  | missing
  /-- The classification has not been PERFORMED. Distinct from `missing`, and the distinction is
      the same one `needsRecheck` draws against `stale`: `missing` asserts that nothing validates
      this dependency, which is a claim about the dependency; `unclassified` says only that we
      have not asked, which is a claim about our own state.

      The compiler cannot mint `contract` or `body` on its own — that split needs
      `classifyTheorem`, which reads a theorem's elaborated type and lives in `MetaM` on the Lean
      side. Without this constructor the compiler's honest answer would have to be spelled
      `missing`, which asserts something stronger than it knows and would be indistinguishable
      from a genuinely unvalidated dependency once the Lean side DID answer.

      Both fail closed, so nothing is weakened by having two. What differs is the repair: `missing`
      needs a proof, `unclassified` needs the classification hand-back to run. -/
  | unclassified
deriving BEq, Repr, DecidableEq, Inhabited

/-- Canonical tag. Explicit rather than `toString (repr e)`: this appears in
    receipts, and `repr` is derived FORMATTING that can change with a Lean version
    or printer setting. Evidence may not rest on that. -/
def DependencyEdge.canonical : DependencyEdge → String
  | .contract => "contract"
  | .body     => "body"
  | .trusted  => "trusted"
  | .missing  => "missing"
  | .unclassified => "unclassified"

/-- Every constructor, so a consumer that must handle each can be checked against
    the list instead of hand-maintaining a copy of it. -/
def DependencyEdge.all : List DependencyEdge :=
  [.contract, .body, .trusted, .missing, .unclassified]

/-- Parse a canonical tag back to an edge, or `none`.

    DERIVED FROM `all` AND `canonical`, not written as a second table: a hand-maintained inverse
    would eventually accept a tag the renderer never emits, or refuse one it does — and this is the
    function a decoder uses on bytes it did not write, so a mismatch there is a parser disagreeing
    with the serializer about what a receipt says. `none` for anything unrecognized, because an
    unknown tag is not a default edge. -/
def DependencyEdge.ofCanonical? (tag : String) : Option DependencyEdge :=
  DependencyEdge.all.find? (fun e => e.canonical == tag)

/-- Every constructor is in `all`. A THEOREM, not a length assertion in a gate.

    `check_dependency_edges.sh` pinned `all.length == 5`, which protects nothing against a sixth
    constructor whose author also updates the 5 to a 6 without adding the entry — the count and
    the list are edited in the same breath, so the test agrees with whatever was written. This
    cannot: adding a constructor leaves an unsolved case unless the constructor is genuinely in
    the list.

    Consumers that must handle every kind should rely on THIS, not on the literal length. -/
theorem DependencyEdge.mem_all (e : DependencyEdge) : e ∈ DependencyEdge.all := by
  cases e <;> simp [DependencyEdge.all]

/-- Does this edge let the dependent be considered current?

    `missing` never does. The other three can, but `trusted` does so only with its
    trust carried forward — which is why `propagatesTrust` exists separately
    rather than being folded in here. Answering "is it current?" and "does the
    answer come with an assumption?" in one boolean is how an unqualified
    `proved_by_lean` would be minted over a trust boundary. -/
def DependencyEdge.isCurrentForDependents : DependencyEdge → Bool
  -- EXHAUSTIVE, no catch-all. This was `| .missing => false | _ => true`, and adding
  -- `unclassified` therefore made it CURRENT by default — a dependency nobody has classified
  -- would have let its dependent be considered current, which is fail-open and exactly the
  -- direction that must never be the default.
  --
  -- A wildcard in a function that decides currency means every future edge kind is born
  -- trusted. Listing every constructor makes adding one a compile error, which is the only
  -- reliable prompt to think about it.
  | .contract     => true
  | .body         => true
  | .trusted      => true
  | .missing      => false
  | .unclassified => false

/-- Does relying on this edge oblige the caller to qualify its claim?

    Only `trusted`. A proof reaching a trusted boundary records
    `proved_by_lean_modulo_trusted`; a receipt that carries the edge but drops
    this distinction has laundered the trust. -/
def DependencyEdge.propagatesTrust : DependencyEdge → Bool
  | .trusted => true
  | _        => false

/-- Which subject-level change invalidates a dependent resting on this edge.

    Stated as data rather than prose so a consumer cannot invent its own rule:
    `contract` survives a body change that preserves the contract, `body` does
    not, `trusted` tracks the boundary, `missing` is never current to begin
    with. -/
def DependencyEdge.invalidatedByBodyChange : DependencyEdge → Bool
  | .body    => true
  | .missing => true   -- already not current; a body change cannot improve that
  | _        => false

/-! ## Canonical table-ENTRY evidence (R-0004 slice 6, blocker c prerequisite)

A whole-table digest answers "did this table change". It cannot answer "does this table CONTAIN
that callee", and correspondence needs the second: a `body` edge is justified only when the bound
table holds the exact callee implementation. Digest and membership are different questions, and
one has been standing in for the other.

So entry-level material: per entry, the callable IDENTITY and the subject it was extracted from.
`PFnDef` already carries both — `identity : PFnIdentity` and `sourceBodyDigest` — so this reads
what exists rather than inventing a parallel record.

**Whole-table digests remain necessary**, and this does not replace them. A dynamic index can
reach ANY entry, so a dependency on a dynamically-indexed table is a dependency on all of it;
entry evidence answers membership for the statically-known case and cannot narrow the dynamic
one. Both, for different questions. -/

/-- One entry's correspondence material: which callable, and which subject it came from. -/
structure TableEntryEvidence where
  callee : CallableId
  /-- The V1 SOURCE-BODY digest, and the name says V1 because that is what it holds.
      `PFnDef.sourceBodyDigest` is `sourceBodyDigestV1`: it covers the extracted BODY and binds
      no typed signature, no contracts, no selected specification, no claim scope.
      An earlier version of this field was called `subjectDigest` and documented as "which
      subject it came from" — reinterpreting a V1 body digest as the frozen V2 subject. A
      signature or contract change would leave such evidence looking current, and the eventual
      per-edge join would then be exact over the WRONG subject identity.

      `none` is carried rather than defaulted: an entry whose provenance is unrecorded must not
      compare equal to one whose provenance is recorded and happens to match.

      **A VERIFIED value, and `String` rather than `Option String` for that reason.**
      This field used to be optional and merely COPIED from `PFnDef.sourceBodyDigest`, which made
      two bad states representable: an entry with no provenance, and an entry whose stored
      provenance describes a body it no longer holds. The second is the dangerous one — a body
      could be substituted while keeping its callable identity AND its stored digest, and the join
      still bound, because every leg compared metadata against metadata and nothing ever read the
      body.

      Now `tableEntryEvidence` RECOMPUTES from `PFnDef.body` and refuses the whole table unless the
      stored digest agrees, so a value here has been checked against the actual body. There is no
      constructor for an unverified one. -/
  sourceBodyDigestV1 : String
deriving Repr, BEq

/-! ## Scoped membership — the DefinitionIdentity-keyed form

`tableEntryEvidence` keys on `CallableId`, which measurement showed is a source NAME: two packages
may define `main.validate_header` and a shared table could match the wrong one. This is the same
question asked of the scoped identity instead.

**A table with no attestations is LEGACY**, not empty. It evaluates fine — models are all evaluation
needs — but it cannot say which source definitions it describes, so it cannot participate in a scoped
join and yields `needs_recheck`. That is a refusal to re-derive, not one to argue with: the missing
scope is exactly what would distinguish two same-named definitions. -/

/-- One attested entry's membership material, keyed by SCOPED identity. -/
structure ScopedEntryEvidence where
  definition       : DefinitionIdentity
  sourceBodyDigest : String
  /-- The key a specification's `.call` selects this entry by — the attested model's
      `operationalKey` (bug 081). Containment alone says the table holds the definition SOMEWHERE;
      a call names a key, and what the call denotes is the entry under that key. -/
  operationalKey   : String
deriving Repr, BEq

/-- Why a table yielded no scoped membership. -/
inductive ScopedMembershipRefusal where
  /-- No attestations: the table carries models only. `needs_recheck` — it must be rebuilt through
      `ofAttested` with generated references before it can describe definitions. -/
  | legacyUnattested
  /-- An attested entry's stored provenance disagrees with the digest recomputed from its model body.
      Same check `tableEntryEvidence` performs, kept here so the scoped path is not weaker. -/
  | bodyMismatch (definition : DefinitionIdentity) (stored : String) (recomputed : String)
  /-- An attested entry records no provenance at all. -/
  | provenanceMissing (definition : DefinitionIdentity)
  /-- Two entries attest the same definition, so a lookup cannot say which model it selects. -/
  | duplicateDefinition (definition : DefinitionIdentity)
  /-- One or more generated references failed validation. `needs_recheck`: the references must be
      regenerated, and this is DISTINCT from `legacyUnattested` — a broken reference is not the same
      fact as a table nobody attested, and the fixes differ. -/
  | attestationIncomplete (failures : Nat)
  /-- An attested model is not among the table's own entries. The attestation would then describe a
      model the table does not contain, so membership answered from it would be about something the
      table cannot dispatch to. -/
  | attestedModelNotInTable (definition : DefinitionIdentity)
  /-- The table's DISPATCH does not answer an attested model's key with that exact model (Codex,
      land3). Evaluation resolves `.call key` through `globals`, while this evidence is read from
      `attested`/`entries`; a table attesting `checkClassFn` under `check_class` while `globals`
      sends `check_class` to `checkVersionFn` would otherwise be evidence about a model no call can
      reach. Exact `PFnDef` equality — parameters and body, not a body digest. -/
  | dispatchMismatch (definition : DefinitionIdentity) (key : String)
deriving Repr, BEq

def ScopedMembershipRefusal.explain : ScopedMembershipRefusal → String
  | .legacyUnattested => "table carries models but no attestations — needs_recheck"
  | .bodyMismatch d st rc =>
      s!"'{d.localName}' stores body digest {st} but its model recomputes to {rc}"
  | .provenanceMissing d => s!"'{d.localName}' records no source-body provenance"
  | .duplicateDefinition d => s!"'{d.localName}' is attested twice"
  | .attestationIncomplete n => s!"{n} generated reference(s) failed validation — needs_recheck"
  | .attestedModelNotInTable d => s!"'{d.localName}' attests a model the table does not contain"
  | .dispatchMismatch d k =>
      s!"'{d.localName}' is attested under key `{k}`, but the table's dispatch does not answer `{k}` with that model"

/-- Scoped membership for a table, or a named refusal.

    Recomputes each body digest from the attested MODEL, exactly as the unscoped path does, so
    migrating the join does not weaken the body check while strengthening the identity one. -/
def scopedEntryEvidence (t : FnTable)
    : Except ScopedMembershipRefusal (List ScopedEntryEvidence) := do
  -- ORDER MATTERS. A broken reference is reported as such even when it is the ONLY attestation:
  -- checking emptiness first would report `legacyUnattested` for a table someone DID attest, sending
  -- a reader to write attestations that already exist.
  if t.attestationFailures > 0 then .error (.attestationIncomplete t.attestationFailures)
  -- AN EMPTY TABLE IS VACUOUSLY ATTESTED, not legacy. `FnTable.empty` has no entries, so there is
  -- nothing to attest and membership is empty for every callee — which is a complete answer, not a
  -- missing one. TEN packages share that single constant (13 attestation rows across them; the
  -- earlier note said "thirteen packages" and was counting rows), so it cannot be attested
  -- per-package anyway; treating it as `legacyUnattested` would refuse every subject that names it
  -- while there is nothing about it left to establish.
  --
  -- Checked BEFORE the attestation test: a table with entries and no attestations is legacy, and a
  -- table with neither is empty. Collapsing those would either refuse an empty table or admit an
  -- unattested one.
  else if t.entries.isEmpty then .ok []
  else if t.attested.isEmpty then .error .legacyUnattested
  else
    let rows ← t.attested.toList.foldlM (init := ([] : List ScopedEntryEvidence))
      fun acc (model, d) =>
        -- The attested model must BE one of the table's entries. `attested` and `entries` are
        -- separate arrays, so nothing structural stops a table attesting a model it does not hold —
        -- and membership answered from such an attestation would describe something the table
        -- cannot dispatch to.
        if !t.entries.toList.contains model then .error (.attestedModelNotInTable d) else
        -- ...and it must be what a CALL reaches. `eval` dispatches `.call key` through `globals`,
        -- not through `entries`, so membership in `entries` says nothing about which model a
        -- specification's call evaluates. The whole model is compared (parameters included): two
        -- models with one body and different parameter lists are different functions. A key two
        -- attested models share cannot dispatch to both, so this also refuses conflicting entries.
        if t.globals model.operationalKey != some model then
          .error (.dispatchMismatch d model.operationalKey) else
        match model.sourceBodyDigest with
        | none => .error (.provenanceMissing d)
        | some stored =>
          let recomputed := Concrete.sourceBodyDigestV1Of model.body
          if stored.value != recomputed then
            .error (.bodyMismatch d stored.value recomputed)
          else .ok (acc ++ [{ definition := d, sourceBodyDigest := stored.value
                            , operationalKey := model.operationalKey }])
    match (rows.map (·.definition.digest)).find?
            (fun x => ((rows.map (·.definition.digest)).filter (· == x)).length > 1) with
    | some dup =>
      match rows.find? (fun r => r.definition.digest == dup) with
      | some r => .error (.duplicateDefinition r.definition)
      | none   => .ok rows
    | none => .ok rows

/-- Does this table contain that DEFINITION? All four identity components, never a name. -/
def scopedEvidenceContains (rows : List ScopedEntryEvidence) (d : DefinitionIdentity) : Bool :=
  rows.any (fun r => r.definition.sameDefinition d)

/-- The entries a table holds UNDER A KEY — what a specification's `.call key` can select (bug 081). -/
def scopedEntriesUnderKey (rows : List ScopedEntryEvidence) (key : String) : List ScopedEntryEvidence :=
  rows.filter (·.operationalKey == key)

/-- Why a table yielded no entry evidence. NAMED, one constructor per distinct check.

    `DependencyClosure` (docs/verification/EVIDENCE_ARCHITECTURE.md) requires missing, surplus, duplicate,
    ambiguous, unclassified and mismatched to be named refusals, "never discarded by `filterMap`, a
    first-match lookup, or an advisory-only warning". This function previously returned `Option`, so
    all six checks below collapsed to `none` — the recompute was right and the reason was thrown
    away. A caller could not distinguish "this table holds a SUBSTITUTED body" from "this table has
    an entry without identity", and those warrant different responses: the first is a possible
    attack, the second is an incomplete build.

    `bodyMismatch` is the `mismatched` set. It carries the callee and both digests, because "a body
    disagreed" does not say which one or by how much, and a refusal you cannot act on is only
    marginally better than a silent one. -/
inductive EntryEvidenceRefusal where
  /-- An entry carries no semantic identity, so membership cannot be decided for it. -/
  | noIdentity
  /-- An entry records no source-body provenance. Absence is not agreement. -/
  | provenanceMissing (callee : CallableId)
  /-- Provenance recorded under a schema this producer does not implement — a DIFFERENT formula,
      so comparing it here would report body drift where there is a formula difference. -/
  | schemaUnsupported (callee : CallableId) (schema : String)
  /-- Provenance recorded under a different scope, for the same reason. -/
  | scopeUnsupported (callee : CallableId) (scope : String)
  /-- The stored digest disagrees with the digest recomputed from `PFnDef.body`. THE mismatched
      case: identity and metadata are intact and the body is not the one described. -/
  | bodyMismatch (callee : CallableId) (stored : String) (recomputed : String)
  /-- One callable appears twice, so a static lookup cannot say which implementation it selects. -/
  | duplicateIdentity
deriving Repr, BEq

def EntryEvidenceRefusal.explain : EntryEvidenceRefusal → String
  | .noIdentity => "a table entry carries no semantic identity"
  | .provenanceMissing c => s!"'{c.render}' records no source-body provenance"
  | .schemaUnsupported c sc => s!"'{c.render}' records provenance under schema '{sc}', not sourceBodyDigestV1"
  | .scopeUnsupported c sc => s!"'{c.render}' records provenance under scope '{sc}', not body_only"
  | .bodyMismatch c st rc =>
      s!"'{c.render}' stores body digest {st} but its body recomputes to {rc}"
  | .duplicateIdentity => "one callable appears twice in the table"

/-- Entry evidence for a table, or a NAMED refusal.

    All-or-nothing deliberately. A partial membership list answers "is this callee present" with
    "not in the part I could read", which is indistinguishable from "absent" — and absence is
    what justifies refusing an edge. A caller cannot tell those apart from a shortened list, so
    it does not get one. This mirrors `FnTable.allIdentified`: evidence requires identity for the
    WHOLE table, not most of it. -/
def tableEntryEvidence (t : FnTable) : Except EntryEvidenceRefusal (List TableEntryEvidence) := do
  let rows ← t.canonicalEntries.toList.foldlM (init := ([] : List TableEntryEvidence)) fun rows d =>
    match d.identity.id?, d.sourceBodyDigest with
    | some cid, some stored =>
        -- RECOMPUTED FROM THE BODY, not copied from the metadata beside it. Copying is what made
        -- the acceptance mutation survive: replace `d.body`, keep `d.identity` and keep
        -- `d.sourceBodyDigest`, and every downstream comparison still agreed, because the stored
        -- digest was the only thing anyone read and it had not changed.
        --
        -- SCHEMA AND SCOPE FIRST, and each gets its own refusal: `sourceBodyDigestV1Of` implements
        -- exactly `sourceBodyDigestV1`/`body_only`, so a digest under another schema or scope is a
        -- different formula and a mismatch there is not body drift.
        if stored.schema != "sourceBodyDigestV1" then
          .error (.schemaUnsupported cid stored.schema)
        else if stored.scope != "body_only" then
          .error (.scopeUnsupported cid stored.scope)
        else
          let recomputed := Concrete.sourceBodyDigestV1Of d.body
          if stored.value != recomputed then
            .error (.bodyMismatch cid stored.value recomputed)
          else .ok (rows ++ [{ callee := cid, sourceBodyDigestV1 := stored.value }])
    -- An entry with NO recorded provenance is refused rather than carried: evidence that cannot be
    -- checked against a body must not stand in for evidence that was.
    | some cid, none => .error (.provenanceMissing cid)
    | none, _ => .error .noIdentity
  -- UNIQUE IDENTITIES, or nothing. A table holding one callable twice cannot say which
  -- implementation a static lookup selects, and `entryEvidenceContains` answering "at least one"
  -- would let a `body` edge be justified by an entry that is not the one dispatch reaches.
  if (rows.map (·.callee)).eraseDups.length != rows.length then .error .duplicateIdentity
  else .ok rows

/-- The canonical ENTRY-DERIVED table identity digest.

    The table digest the hand-back already carries is `shortHash ("tableV1:" ++ name ++ toString v)`
    over the SYNTACTIC term at generation time. The compiler cannot reproduce that — it has no term,
    only a linked value or a row of data — so it has never been able to verify that a name it
    resolved denotes the table a theorem actually bound. This digest can be recomputed from either
    side, which is the property that was missing.

    BINDS THE TABLE'S OWN IDENTITY, not just its membership. Without the name in the preimage, a
    valid digest could be copied from another table and would verify — name, entry list and digest
    must agree, and none may be substituted independently.

    BINDS SCHEMA AND SCOPE per entry. `tableEntryEvidence` accepts only
    `sourceBodyDigestV1`/`body_only` and refuses anything else, so these are the sole accepted
    values today; binding them anyway means a future schema whose digests happen to collide cannot
    silently satisfy a check written for this one.

    CANONICALLY ORDERED BY IDENTITY before hashing, so declaration or traversal order cannot enter
    the value: two producers walking the same table in different orders must agree, or the digest
    measures iteration rather than membership. Reordering is therefore EQUIVALENT by construction,
    while adding, removing, duplicating or altering an entry is not.

    LENGTH-PREFIXED per field, so `("ab","c")` and `("a","bc")` cannot serialize alike.

    **Agreement does NOT upgrade evidence, and is not an independent check.** Generator and
    compiler run this same implementation, so agreement establishes canonical-encoding consistency
    and the binding between a table's name, its entries and its stored digest — enough to catch
    corruption, staleness and a copied digest. It establishes nothing about the formula's
    correctness, the generator's correctness, or whether the entries describe real bodies. A
    consistently-altered entry list and digest verify structurally, by design. Independence would
    require a standalone verifier implementing this canonical format separately. -/
def entryTableDigest (tableName : String) (rows : List TableEntryEvidence) : String :=
  let sorted := rows.mergeSort (fun a b => a.callee.render ≤ b.callee.render)
  let parts := sorted.map (fun r =>
    let c := r.callee.render
    -- schema and scope are the values `tableEntryEvidence` verified before admitting the row
    s!"C{c.length}:{c}|S20:sourceBodyDigestV1|P9:body_only|B{r.sourceBodyDigestV1.length}:{r.sourceBodyDigestV1}")
  Concrete.shortHash ("tableIdentityV1:T" ++ toString tableName.length ++ ":" ++ tableName
    ++ "|n" ++ toString sorted.length ++ ":" ++ String.join parts)

/-- Does this table contain the callee, by IDENTITY?

    Identity, never name: two callables can share a display name, and `PFnDef.displayName` is
    explicitly not identity. Membership decided on a rendering would be the defect R-0004 exists
    to close, appearing in the check meant to close it. -/
def entryEvidenceContains (rows : List TableEntryEvidence) (callee : CallableId) : Bool :=
  rows.any fun r => r.callee == callee

/-! ## Implementation identity and table-entry binding (R-0004 slice 6)

Lives HERE rather than in `DependencyEdges` because `ProofCore` must produce the manifest and
`DependencyEdges` imports `ProofCore` — putting it there made the producer a cycle. The types are
pure data over `CallableId`, so this is their natural home anyway.
-/

/-- A closed, validated map from callable identity to IMPLEMENTATION digest.

    **Why implementation and not proof subject.** The frozen V2 subject includes the selected
    specification and claim scope, which describe what a particular proof LINK claims. One
    callable can carry several proof links, so `CallableId -> ProofSubjectDigestV2` is not a
    function and a join on it is ill-defined. The IMPLEMENTATION digest excludes spec and scope,
    so it is one per callable — which is what a table entry needs, since an entry describes an
    implementation and does not change when a different specification is pointed at it.

    **Why a closed type and not a callback.** The previous version took
    `CallableId -> Option String`, so any caller could mint a "bound" entry from any non-empty
    string — the private constructor required only non-emptiness, not provenance. The tests
    passed `"v2:abc"` and proved exactly that. A private constructor guarding a value the caller
    supplies is not a guard.

    Constructor is private, and so is `ofRows`; `ofImplementations` is the only way in, and it
    computes the digests rather than accepting them. -/
structure ImplementationManifest where
  private mk ::
  /-- `(callable, authoritative source-body digest, implementation digest)`.
      The BODY component is carried so the join can compare provenance rather than merely attach
      it: identity alone does not establish that a table entry holds the implementation the
      manifest describes. -/
  private rows : List (CallableId × String × String)
deriving Repr

/-- Is a candidate row set WELL-FORMED? Format only, and public so the format refusals can be
    tested without a way to construct a manifest from chosen digests.

    Refuses a duplicate callable — the manifest claims to be a FUNCTION, and two rows for one
    callable means it is not — and a digest that is not canonical 32-hex. Both refuse the WHOLE row
    set rather than the offending row: a manifest missing entries answers "no implementation for
    this callable" indistinguishably from a callable that genuinely has none.

    This is a predicate, not a constructor, and the split is the point. It says nothing about
    provenance: thirty-two zeros are well-formed. Exposing it as a Bool means a caller can ask
    whether rows are syntactically acceptable without that question yielding a manifest. -/
def ImplementationManifest.rowsWellFormed (rows : List (CallableId × String × String)) : Bool :=
  let isHex := fun (d : String) => d.length == 32 && d.all fun c => c.isDigit || ('a' ≤ c && c ≤ 'f')
  (rows.map (·.1)).eraseDups.length == rows.length
  -- BOTH digests validated: the body component is the value the join compares against, so a
  -- malformed one would make every comparison fail or, worse, succeed against another malformed
  -- entry.
  && rows.all fun r => isHex r.2.1 && isHex r.2.2

/-- Build a manifest from raw rows. **PRIVATE, and that is the containment.**

    It validates FORMAT ONLY: a caller cannot pass `"v2:abc"`, but can pass thirty-two zeros. While
    this was public, "validated manifest" meant "well-formed hex", and the join's whole guarantee
    rested on a value any caller could invent. `ofImplementations` is the way in — it takes inputs
    and computes the digests — and this exists only for it to route through, so the duplicate and
    format refusals stay in one place.

    Made private 2026-08-12. The format refusals it used to be tested through are now tested via
    `rowsWellFormed`, so nothing was lost by closing it; the compile-failure controls in
    `check_dependency_edges.sh` pin that it stays closed. -/
private def ImplementationManifest.ofRows (rows : List (CallableId × String × String))
    : Option ImplementationManifest :=
  if !ImplementationManifest.rowsWellFormed rows then none
  else some (ImplementationManifest.mk rows)

/-- The `(authoritative body digest, implementation digest)` for a callable. -/
def ImplementationManifest.find? (m : ImplementationManifest) (c : CallableId)
    : Option (String × String) :=
  (m.rows.find? fun r => r.1 == c).map (·.2)

/-- Build a manifest from COMPLETE IMPLEMENTATIONS, computing both digests internally.

    This is the entry point a caller should use, and the reason is provenance rather than
    convenience: `ofRows` takes digests, and a digest handed to a constructor is a digest the
    constructor cannot vouch for — it validates that thirty-two hex characters are thirty-two hex
    characters. Here the caller supplies INPUTS and the digests are computed from them, so there is
    nowhere to put an invented value.

    Still routed through `ofRows`, so the duplicate-callable and canonical-format refusals apply
    unchanged. Those are not redundant once digests are computed: a duplicate means the manifest is
    not the function it claims to be, and a malformed digest from the AUTHORITATIVE producer is a
    different failure from a forged one — worth refusing rather than assuming cannot happen.

    LIMIT, stated because the type looks stronger than it is: this inherits exactly the guarantees
    of `CompleteImplementation`. The `facts`/`callable` mispairing is refused; a `body` or
    `extracted` belonging to another entry is NOT detectable, because neither carries identity. See
    the named gap on `CompleteImplementation`. -/
def ImplementationManifest.ofImplementations (impls : List CompleteImplementation)
    : Option ImplementationManifest :=
  ImplementationManifest.ofRows (impls.map fun ci =>
    (ci.callable, ci.sourceBodyComponent, ci.implementationComponent))

/-! ## Manifest completeness, self-denominating

`filterMap` launders. A producer that drops what it cannot handle returns a SMALLER manifest that
looks complete, because the only record of how many rows there should have been is the number of rows
there are. The denominator has to be stored, not inferred from the numerator.

So the producer returns every eligible identity it set out to cover, the rows it built, and a NAMED
refusal for each identity it could not — and a manifest fit for correspondence exists only when the
refusal list is empty and the rows account for exactly the expected identities. -/

/-- Why one identity produced no manifest row. Named, because "absent" is the one answer that must
    never stand in for the others: an entry with no facts and an entry whose evidence failed
    validation are different failures, and a count cannot tell them apart. -/
inductive ManifestRefusal where
  /-- No checked declaration facts were captured for this identity. -/
  | factsMissing
  /-- Facts captured but incomplete — a digest over incomplete facts is a digest over an unknown. -/
  | factsIncomplete
  /-- An evidence body draft was present but failed validation. -/
  | evidenceInvalid
  /-- No evidence body draft at all. -/
  | evidenceMissing
  /-- No extracted proof-model body, so no comparable source-body component. -/
  | extractedMissing
  /-- All parts present, but `CompleteImplementation.of?` refused them — currently only reachable
      by a facts/callable mispairing, since that is the mispairing it can see. -/
  | inputsRefused
deriving Repr, BEq

def ManifestRefusal.render : ManifestRefusal → String
  | .factsMissing      => "facts-missing"
  | .factsIncomplete   => "facts-incomplete"
  | .evidenceInvalid   => "evidence-invalid"
  | .evidenceMissing   => "evidence-missing"
  | .extractedMissing  => "extracted-missing"
  | .inputsRefused     => "inputs-refused"

/-- The outcome of trying to build a manifest for a whole program.

    `expected` is the DENOMINATOR, recorded up front. Every eligible identity appears either in
    `impls` or in `refusals`, never in neither and never in both. -/
structure ManifestResult where
  expected : List CallableId
  impls    : List CompleteImplementation
  refusals : List (CallableId × ManifestRefusal)

/-- Total accounted-for identities. Equal to `expected.length` when the result is well-formed; a
    difference means the producer lost an identity, which is the failure this type exists to make
    visible rather than silent. -/
def ManifestResult.accounted (r : ManifestResult) : Nat := r.impls.length + r.refusals.length

/-- The manifest, ONLY if the result is complete — or `none`, with no partial answer available.

    Three conditions, each refusing for a different reason:
    - **no refusals**: a refusal means some identity has no row, and the join cannot distinguish
      "this callable has no implementation" from "we failed to build its row";
    - **rows account for exactly the expected identities**: this is the anti-`filterMap` condition.
      Comparing against a stored denominator is the whole point; comparing rows against themselves
      is what the old producer effectively did;
    - **well-formed and duplicate-free**, via `ofImplementations`, which still applies.

    Order-sensitive equality is deliberate and is not merely convenient: the producer walks entries
    in one order and appends in that order, so a permutation means an identity was reordered relative
    to its source, which is a bug in the producer rather than a harmless difference. -/
def ManifestResult.usable? (r : ManifestResult) : Option ImplementationManifest :=
  if !r.refusals.isEmpty then none
  else if r.impls.map (·.callable) != r.expected then none
  else ImplementationManifest.ofImplementations r.impls

/-- A table entry bound to its authoritative IMPLEMENTATION digest.

    `implDigest` is a plain `String` with a private constructor, so an entry without an
    authoritative implementation has no representation — and the value can only have come from a
    validated manifest, not from a caller-supplied callback. -/
structure BoundTableEntry where
  private mk ::
  callee     : CallableId
  implDigest : String
deriving Repr, BEq

/-- Join entry evidence against a CLOSED manifest, or refuse.

    Refuses the whole list when any entry has no manifest row. Dropping unbindable entries would
    shrink the membership set, and a shrunken set answers "is this callee present" with "not
    among the ones I could bind" — indistinguishable from "absent", which is what justifies
    refusing an edge. A membership list must never be able to under-report. -/
def bindEntryImplementations (rows : List TableEntryEvidence) (manifest : ImplementationManifest)
    : Option (List BoundTableEntry) :=
  rows.foldl (init := some []) fun acc r =>
    match acc, manifest.find? r.callee with
    -- PROVENANCE IS COMPARED, not attached. Identity alone let a stale or substituted entry with
    -- the right `CallableId` acquire the current authoritative digest — the misattachment this
    -- join exists to prevent. The entry's body digest must EQUAL the authoritative one.
    --
    -- Two independent checks, and both are needed. `tableEntryEvidence` established that this
    -- value describes the body the table actually holds; this establishes that the body the table
    -- holds is the one the manifest is authoritative for. Either alone leaves a gap: the first
    -- without the second admits a self-consistent table of the wrong implementations, the second
    -- without the first admits a substituted body wearing correct-looking metadata.
    | some out, some (authBody, implD) =>
        if r.sourceBodyDigestV1 == authBody then some (out ++ [BoundTableEntry.mk r.callee implD])
        else none
    -- No manifest row: refuse. The "no stored digest" arm this used to also cover is gone because
    -- `TableEntryEvidence.sourceBodyDigestV1` is no longer optional — `tableEntryEvidence` refuses
    -- an entry with unrecorded provenance, so an unverifiable one cannot reach this join at all.
    | _, _ => none

/-- The bound implementation digest for a callee, if it is a member.

    Returns the DIGEST rather than a Bool: a `body` justification must record which
    implementation justified it, or a later check cannot tell whether the entry it matched is the
    one still there. -/
def boundEntryImplementationOf (rows : List BoundTableEntry) (callee : CallableId) : Option String :=
  (rows.find? fun r => r.callee == callee).map (·.implDigest)


end Concrete.Proof
