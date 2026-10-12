import Concrete.Proof.DependencyEdges
import Concrete.Proof.Replay
open Concrete in

/-! # The proof-evidence receipt envelope (R-0004 slice 4)

**Declared future owner: `Concrete/Proof/Receipt/`.** Landed flat here because that directory
split depends on R-0114-R-0118 (see the repo-reorganization plan) and is premature; the header
records the intended home so the move is a rename rather than a rediscovery.

## What a receipt is for

A receipt says: *this evidence was established against exactly this subject, under exactly these
dependencies, in exactly this environment.* Its value is entirely in what it BINDS — a receipt
that omits something is not a weaker receipt, it is a receipt that silently claims independence
from whatever it left out.

## The design commitment: unbindable evidence is UNREPRESENTABLE, not rejected

`EdgeEvidence.tablesFullyBound` is a predicate, and a predicate can be forgotten. The type here
is built so it cannot be:

* `tableBindings : List (Name × String)` — a digest per table, **not** `Option String`. There is
  no way to put an unbound table inside a receipt, so no consumer has to remember to check.
* the structure constructor is PRIVATE and `Inhabited` is not derived, so `mint` really is the
  only way to obtain one. Both were missing in the first version, and the claim below was
  therefore false: a caller could build a receipt with an empty subject and the current schema
  version, and `default` produced one with an empty schema.
* `ReceiptMaterial.of?` returns `Option` and refuses when any named table is unbound, when the
  digests do not correspond to the named tables IDENTITY-wise, when a table is bound twice, when
  the subject digest is absent OR empty, or when an environment identity is empty.
* `mint` requires a `SuccessfulReplay`, whose constructor is private and whose only producer needs
  a `ReplayResult` that only `Concrete.Proof.replay` can produce. So the receipt binds a kernel run
  the same way it binds a subject: not by recording a claim about one, but by being unable to exist
  without one.

This is the same move Register C made for status composition and `proofSubjectDigestV2` made for
incomplete facts: make the bad state unrepresentable rather than merely discouraged. A guard that
must be remembered is a guard that will eventually be forgotten, and a receipt minted from
partial evidence is indistinguishable from one minted from complete evidence — which is precisely
the failure a receipt exists to prevent.

## What it binds, and why each is separate

Four environment identities rather than one blob, because they fail independently and a consumer
needs to know WHICH moved:

| field | binds | a change means |
|---|---|---|
| `toolchainId` | the Lean/compiler version the proof was checked under | the checker changed; the proof may not replay |
| `workspaceId` | the deterministic workspace root | the proof was established somewhere else |
| `importsId` | the transitive import surface | something the proof could see has changed |
| `schemaVersion` | this envelope's own format | the receipt cannot be compared field-wise |

`schemaVersion` is in the bytes for the reason `v2:` is in the subject digest: a receipt written
under an older envelope must read as a DIFFERENT SCHEMA rather than as a mismatch, so it becomes
`needs_recheck` and not `stale`. Without it, the first envelope change would report every stored
receipt as a failed proof.

## What this does NOT do

It does not replay anything, and as of 2026-08-16 it cannot pretend to: minting takes a
`SuccessfulReplay` token, so a receipt minted without a replay — a claim about a proof nobody ran —
does not typecheck. What remains open is the other direction: nothing in the production pipeline
mints yet, so no stored receipt affects any status. Until it does, these are helper materials with
their authority enforced, not evidence in use.
-/

namespace Concrete.Proof

open Lean

/-- A proof-evidence receipt. Every field is bound; there is no partial receipt.

    Construct with `mint` only — the fields are public for reading and pattern-matching, but
    building one directly would let a caller assemble the very state minting refuses. -/
structure ProofEvidenceReceipt where
  /-- PRIVATE constructor. Without it, "minting is the only constructor" was simply false: any
      caller could assemble a receipt with an empty subject, no bindings and the CURRENT schema
      version — which would then read as comparable. The gate in this repo demonstrated the
      bypass while claiming the invariant held.

      Projections stay public; only construction is closed. -/
  private mk ::
  /-- This envelope's format. Compared BEFORE the contents, so an older receipt is
      `needs_recheck` rather than a failed comparison. -/
  schemaVersion : String
  /-- The v2 subject digest the evidence was established against. -/
  subjectDigest : String
  /-- What the proof relies on, derived from the theorem rather than declared. -/
  edge : DependencyEdge
  /-- One digest per named table, all present by construction. `Option` deliberately absent:
      an unbound table has no representation inside a receipt.

      **Ordering is NORMALIZED, not semantic**, and stating which is the point. The pairs
      arrive in Lean's traversal order, which is deterministic today and is not a promise —
      it is an artifact of how `getUsedConstants` happens to walk an expression. A durable
      receipt cannot rest on that: the same dependency set discovered in a different order
      would serialize differently and compare unequal, reporting drift where there is none.

      So `ReceiptMaterial.of?` sorts by table identity. The consequence to be aware of: order carries no
      information, and two receipts differing only in the ORDER of the same pairs are the
      same receipt. What order cannot hide is a SWAP — exchanging two tables' digests changes
      which name is paired with which value, and that survives sorting. -/
  tableBindings : List (Name × String)
  /-- The DEPENDENCY ROOT this evidence was established over, as the digest of the validated root
      preimage. Named by the authority criterion and absent until 2026-08-16: without it a receipt
      binds the tables a theorem mentions and says nothing about the CLOSURE the claim rests on, so
      a change deep in that closure — a callee's subject digest moving, an edge kind changing — left
      the receipt reading current. The root already refuses to compute over incomplete material, so
      binding its digest inherits every one of those refusals. -/
  dependencyRoot : String
  /-- The THEOREM ARTIFACT the claim was replayed from. A subject digest says which function was
      proved; this says which proof term did it. Two different proofs of the same statement are
      different artifacts, and a receipt that could not tell them apart would survive a proof being
      replaced by a weaker one that happens to typecheck. -/
  theoremArtifact : String
  /-- Whether the closure crosses a trusted boundary, and WHICH boundaries. Carried as data rather
      than folded into the digest so a consumer can read the assumption without recomputing it: an
      unconditional `proved_by_lean` and a `proved_by_lean_modulo_trusted` must not be the same
      receipt with a different summary line. Sorted, for the reason the table bindings are. -/
  carriesTrust : Bool
  trustedBoundaries : List String
  toolchainId : String
  /-- The compiler and the checker `toolchainId` was computed from, IN THE CLEAR (receiptV2,
      R-0208). The digest alone binds them but cannot be read: advisory policy has to know WHICH
      checker accepted a theorem to say whether an advisory affects it. Both are re-hashed against
      `toolchainId` on decode, so editing the readable identity to dodge an advisory makes the
      record malformed rather than unaffected. -/
  compilerVersion : String
  checker : String
  workspaceId : String
  importsId : String
deriving Repr

-- NO `Inhabited`. `deriving Inhabited` manufactures a default receipt — empty schema, empty
-- subject, no bindings — which is another route to an invalid value and defeats the private
-- constructor entirely. A type whose invalid state is one `default` away is not closed.

/-! ## The three environment identities

The receipt REFUSES an empty identity, so until something produces these no receipt can mint —
fail-closed, and loud. These are those producers.

Each is a digest over versioned canonical bytes rather than a display string, and each is
deliberately independent of the machine it ran on, because clean-machine reproducibility is a
completion requirement: an identity containing an absolute path would make every receipt
un-replayable anywhere else, which is the opposite of what binding the environment is for. -/

/-- The toolchain the proof was checked under: compiler version AND the Lean checker.

    Both, not either. The compiler decides what the subject IS; Lean decides what a proof of it
    MEANS. A receipt that pinned only one would survive a change to the other, and
    `tableValueDigest`'s structural rendering is explicitly toolchain-relative — that limit is
    only acceptable because this exists.

    `checker` is `CheckerIdentity.canonical`: the version, githash, platform and build digest the
    checking PROCESS reported (R-0208). V1 bound the text of `lean-toolchain`, a file the checker
    that actually ran need not honour; the tag moved so no V1 identity can equal a V2 one. -/
def toolchainIdOf (compilerVersion checker : String) : String :=
  shortHash ("toolchainV2:" ++ toString compilerVersion.length ++ ":" ++ compilerVersion
             ++ "|checker:" ++ toString checker.length ++ ":" ++ checker)

/-- The workspace, identified by its PACKAGE identity rather than its path.

    A filesystem path is not a workspace identity: it differs between two checkouts of the same
    commit, so receipts would disagree across machines while describing identical evidence, and
    an identical path on a different machine would wrongly agree. Package name plus the module
    set is reproducible from the source alone.

    Modules are SORTED, so discovery order cannot enter the identity. -/
def workspaceIdOf (packageName : String) (moduleNames : List String) : String :=
  let mods := (moduleNames.mergeSort (· ≤ ·)).map (fun m => "m" ++ toString m.length ++ ":" ++ m)
  shortHash ("workspaceV1:" ++ toString packageName.length ++ ":" ++ packageName
             ++ "|n" ++ toString moduleNames.length ++ ":" ++ String.join mods)

/-- The import closure a proof could see, as (module, digest) pairs.

    Names alone would not move when an imported module's CONTENT changes, which is the same
    defect `tableValueDigest` exists to prevent one level down. Sorted by module name for the
    reason the workspace's module list is: import order is not part of what a proof depends on,
    but import CONTENT is. -/
def importsIdOf (imports : List (String × String)) : String :=
  let ps := (imports.mergeSort (fun a b => a.1 ≤ b.1)).map fun (m, d) =>
    "i" ++ toString m.length ++ ":" ++ m ++ "=" ++ toString d.length ++ ":" ++ d
  shortHash ("importsV1:n" ++ toString imports.length ++ ":" ++ String.join ps)

/-- The current envelope format. Bumping this makes every stored receipt `needs_recheck`,
    which is the intended behaviour and the reason the field exists. -/
def receiptSchemaVersion : String := "receiptV2"

/-- Receipt material that has passed every field and consistency check, and nothing more.

    THIS IS NOT A RECEIPT, and the separation is the point. Validating the material a receipt would
    record is a different question from whether a kernel ever accepted the theorem, and fusing them
    into one function meant the second question had no answerer at all: `mint?` took the theorem
    artifact as a plain `String`, so any caller could name a theorem nobody had replayed and receive
    a well-formed receipt for it.

    So the two are split. `ReceiptMaterial.of?` answers "is this material complete and
    self-consistent" — pure, cheap, and the subject of most of the tests. `ProofEvidenceReceipt.mint`
    answers "may this become evidence", and it is total: by the time it is called there is nothing
    left to refuse, because it can only be reached holding a `SuccessfulReplay`.

    Private constructor for the same reason the receipt has one: `of?` really is the only producer,
    and validated material that could be assembled directly would validate nothing. -/
structure ReceiptMaterial where
  private mk ::
  subjectDigest     : String
  edge              : DependencyEdge
  tableBindings     : List (Name × String)
  dependencyRoot    : String
  carriesTrust      : Bool
  trustedBoundaries : List String
  /-- The COMPILER's version only. The Lean toolchain half of the identity is not accepted here —
      it comes from the environment the kernel actually ran in, via the minting token, so a receipt
      cannot name a toolchain other than the one that checked it. -/
  compilerVersion   : String
  workspaceId       : String
  importsId         : String
  deriving Repr

/-- Validate receipt material, or refuse.

    Refuses — returning `none` rather than a degraded receipt — when:

    * any named table is unbound (`tablesFullyBound` false), because a `body` edge that cannot
      detect a change in a table it names reads exactly like a dependency that never changes;
    * the subject digest is absent, which `proofSubjectDigestV2` already signals for incomplete
      or missing facts;
    * any environment identity is empty. An empty string is not "unknown", it is a value that
      compares equal to another empty string — so two proofs established under different
      toolchains would agree. Refusing is the only reading that does not invent agreement. -/
def ReceiptMaterial.of?
    (subjectDigest? : Option String) (ev : EdgeEvidence)
    (dependencyRoot : String)
    (carriesTrust : Bool) (trustedBoundaries : List String)
    (compilerVersion workspaceId importsId : String) : Option ReceiptMaterial := do
  let subj ← subjectDigest?
  -- An EMPTY subject is not a subject. `none` was refused and `some ""` was not, which is the
  -- same hole as an empty environment identity: "" is a value that compares equal to another
  -- "", so two proofs over different subjects would agree.
  if subj.isEmpty then none
  -- THE INVARIANT: `unclassified` may flow into diagnostics and shadow reports, never into a
  -- current dependency root or a replay receipt. The root already refuses it (via
  -- `isCurrentForDependents`); this is the other door. A receipt records what evidence was
  -- established against, and "we had not classified this dependency" is not something evidence
  -- can be established against.
  --
  -- `missing` is refused for the same reason and would be caught by the root, but a receipt that
  -- relied on the root having run would be trusting a caller to have called it — which is the
  -- predicate-to-remember shape this file exists to avoid.
  else if ev.edge == DependencyEdge.unclassified || ev.edge == DependencyEdge.missing then none
  else if !ev.tablesFullyBound then none
  -- IDENTITY CORRESPONDENCE, not just arity. `tablesFullyBound` checks that the lists are the
  -- same LENGTH and every digest is present — which admits `tables := [X]` with
  -- `tableDigests := [(Y, …)]`, minting a receipt that claims Y while the theorem depended on
  -- X. Equal counts of unrelated things is not correspondence.
  else if (ev.tables.map toString).mergeSort (· ≤ ·)
          != (ev.tableDigests.map (toString ·.1)).mergeSort (· ≤ ·) then none
  -- A duplicate binding for one table means the evidence disagrees with itself about that
  -- table's digest, and picking one would make the receipt depend on list order.
  else if (ev.tableDigests.map (toString ·.1)).eraseDups.length != ev.tableDigests.length then none
  else if compilerVersion.isEmpty || workspaceId.isEmpty || importsId.isEmpty then none
  -- THE ROOT IS REQUIRED, on the same reasoning as the environment identities: an empty string is
  -- not "unknown", it is a value that compares equal to another empty string, so two claims
  -- established over different closures would agree.
  --
  -- The THEOREM ARTIFACT is no longer checked here, and its absence is not an omission: the artifact
  -- is taken from the minting token rather than from a parameter, and a token cannot exist for an
  -- unnamed theorem — `ReplayRequest.validate` refuses a target with an empty name before any kernel
  -- runs. A whole class of runtime check became structurally unreachable.
  else if dependencyRoot.isEmpty then none
  -- A TRUST CLAIM MUST BE CONSISTENT WITH ITS EVIDENCE. `carriesTrust` with no boundary named is a
  -- qualification a reader cannot act on; boundaries named while the flag is false is a receipt
  -- disagreeing with itself about whether the claim is conditional.
  else if carriesTrust != !trustedBoundaries.isEmpty then none
  else
    -- Safe by the guard above: `tablesFullyBound` establishes every digest is `some`, so this
    -- filterMap drops nothing. Written as filterMap rather than `!` so the total function stays
    -- total if the guard is ever moved.
    -- SORTED BY TABLE IDENTITY. See the field's note: traversal order is deterministic but
    -- not meaningful, and a receipt that compares unequal because a walker visited two
    -- constants in a different order would report drift that did not happen.
    let bindings := (ev.tableDigests.filterMap fun (n, d?) => d?.map fun d => (n, d))
                    |>.mergeSort (fun a b => toString a.1 ≤ toString b.1)
    some { subjectDigest := subj
         , edge := ev.edge
         , tableBindings := bindings
         , dependencyRoot, carriesTrust
         , trustedBoundaries := trustedBoundaries.mergeSort (· ≤ ·)
         , compilerVersion, workspaceId, importsId }

/-- Mint a receipt. The ONLY producer of `ProofEvidenceReceipt`.

    TOTAL — it returns a receipt, not an `Option`. Every refusal already happened: the material was
    validated by `ReceiptMaterial.of?`, and the kernel's acceptance is carried by `SuccessfulReplay`,
    which cannot be constructed without a `ReplayResult`, which `Concrete.Proof.replay` is the only
    producer of. That chain is what makes

        unchecked facts -> ProofEvidenceReceipt

    fail to TYPECHECK rather than fail at runtime. There is no argument a caller can pass to claim a
    kernel ran; the only way to say it is to have done it.

    The artifact and the toolchain are taken from the token rather than from parameters, so a receipt
    cannot name a theorem other than the one replayed, or a toolchain other than the one that checked
    it. Those were the two fields a caller could previously simply assert. -/
def ProofEvidenceReceipt.mint
    (sr : SuccessfulReplay) (m : ReceiptMaterial) : ProofEvidenceReceipt :=
  { schemaVersion := receiptSchemaVersion
  , subjectDigest := m.subjectDigest
  , edge := m.edge
  , tableBindings := m.tableBindings
  , dependencyRoot := m.dependencyRoot
  , theoremArtifact := sr.theoremName
  , carriesTrust := m.carriesTrust
  , trustedBoundaries := m.trustedBoundaries
  , toolchainId := toolchainIdOf m.compilerVersion sr.checker.canonical
  , compilerVersion := m.compilerVersion
  , checker := sr.checker.canonical
  , workspaceId := m.workspaceId
  , importsId := m.importsId }

/-! ## Storage: a receipt that has left the process is no longer evidence

A minted `ProofEvidenceReceipt` carries authority, because obtaining one required a kernel run. A
receipt READ BACK FROM A FILE carries none: the file is ordinary bytes that anything can write.
Modelling both as the same type would make the forgery trivial — decode a hand-written file and hold
a value indistinguishable from one the kernel earned.

So they are different types. `StoredReceipt` is what decoding produces, it has no minting path, and
the only thing it can do is be COMPARED against freshly computed material. That is what "a stored
receipt alone is never current status" means, expressed so a consumer cannot ignore it. -/

/-- Canonical bytes for a receipt. Field-tagged and line-oriented rather than positional, so a
    decoder can name a missing field instead of silently shifting every value by one.

    Repeated fields are already sorted inside the receipt, so the bytes are reproducible from the
    receipt alone. -/
def ProofEvidenceReceipt.encode (r : ProofEvidenceReceipt) : String :=
  String.intercalate "\n" (
    [ s!"schema {r.schemaVersion}"
    , s!"subject {r.subjectDigest}"
    , s!"edge {r.edge.canonical}" ]
    -- UNESCAPED. `s!"{n}"` renders a `Name` in Lean's display form, so a single component
    -- containing dots comes out as «Concrete.Proof.elfFns» — and decoding read the guillemets back
    -- as part of the name, so every receipt disagreed with itself the instant it was written. The
    -- store holds the classification table's STRINGS, which is what `edgeEvidenceOfRow` built these
    -- names from; it is not a structured Lean name and must not pretend to be.
    ++ (r.tableBindings.map fun (n, d) => s!"table {n.toString false} {d}")
    ++ [ s!"root {r.dependencyRoot}"
       , s!"artifact {r.theoremArtifact}"
       , s!"trust {if r.carriesTrust then "true" else "false"}" ]
    ++ (r.trustedBoundaries.map fun b => s!"boundary {b}")
    ++ [ s!"toolchain {r.toolchainId}"
       , s!"compiler {r.compilerVersion}"
       , s!"checker {r.checker}"
       , s!"workspace {r.workspaceId}"
       , s!"imports {r.importsId}" ])

/-- Why a stored receipt could not be decoded.

    PARTIAL DECODING IS THE ATTACK. A decoder that fills missing fields with defaults produces a
    receipt that compares equal to another defaulted one, so two unrelated claims agree — the same
    "empty string is not unknown" failure the mint refusals exist for. Every one of these is a
    refusal rather than a repair. -/
inductive ReceiptDecodeRefusal where
  /-- No `schema` line, or one naming a version this build cannot read. NOT the same as a field
      mismatch: the format changed, and the program may not have. -/
  | schemaUnreadable (found : String)
  /-- A required field is absent. Named, because "malformed receipt" sends a reader to look at the
      whole file. -/
  | missingField (name : String)
  /-- A required field appears twice. Taking either is a decoder choosing which claim to believe. -/
  | duplicateField (name : String)
  /-- A line whose key this decoder does not know. Refused rather than skipped: an unknown key is
      either a newer format or an attempt to smuggle state past the comparison, and skipping makes
      both look like a clean parse. -/
  | unknownKey (key : String)
  /-- A field present but unusable — an empty value, or an edge tag outside the vocabulary. -/
  | malformedField (name : String) (value : String)
  /-- The store holds more than one record for this subject, and they do not agree. Reporting one of
      them as current would let a reader conclude a claim is receipt-backed from a store that
      contradicts itself — and a consumer that picked one would be choosing which claim to believe.
      Zero and several are equally unusable, which is the same disposition `validatedRowOf` gives an
      ambiguous classification and `duplicateField` gives a repeated field. -/
  | duplicateRecord (subject : String)
  /-- The readable compiler/checker identity does not re-hash to the bound `toolchain` id: one of
      them was edited. Policy reads the readable one and currency compares the bound one, so they
      must be the same identity or the record is refused. -/
  | identityNotBound (toolchain : String)
  deriving BEq, Repr

def ReceiptDecodeRefusal.canonical : ReceiptDecodeRefusal → String
  | .schemaUnreadable _ => "schema_unreadable"
  | .missingField _     => "missing_field"
  | .duplicateField _   => "duplicate_field"
  | .unknownKey _       => "unknown_key"
  | .malformedField ..  => "malformed_field"
  | .duplicateRecord _  => "duplicate_record"
  | .identityNotBound _ => "identity_not_bound"

def ReceiptDecodeRefusal.explain : ReceiptDecodeRefusal → String
  | .schemaUnreadable f =>
      s!"stored under schema '{f}', which this build cannot read — re-verify and re-record rather than comparing"
  | .missingField n     => s!"required field '{n}' is absent; a defaulted value would compare equal to another default"
  | .duplicateField n   => s!"field '{n}' appears more than once; taking either would be a decoder choosing which claim to believe"
  | .unknownKey k       => s!"unknown field '{k}' — refusing rather than skipping, since skipping makes a newer format look like a clean parse"
  | .malformedField n v => s!"field '{n}' carries an unusable value '{v}'"
  | .duplicateRecord sub =>
      s!"the store holds more than one record for '{sub}'; a store that contradicts itself about a "
      ++ "claim cannot make it current, and picking one would be choosing which to believe"
  | .identityNotBound t =>
      s!"the readable compiler/checker identity does not hash to the bound toolchain id '{t}'; one "
      ++ "of them was edited, so the record names a checker it was not established under"

/-- A receipt read back from storage. NOT evidence.

    Deliberately a different type from `ProofEvidenceReceipt`. Minting one of those required a
    `SuccessfulReplay`, so holding one means a kernel ran; this came out of a file, and a file is
    bytes anything can write. There is no function from `StoredReceipt` to `ProofEvidenceReceipt`
    and there must not be — the way to turn a stored receipt into evidence is to replay and mint a
    new one.

    Private constructor so `decode` is the only producer, for the same reason it is on the receipt:
    a value assembled around a failed parse validates nothing. -/
structure StoredReceipt where
  private mk ::
  schemaVersion     : String
  subjectDigest     : String
  edge              : DependencyEdge
  tableBindings     : List (Name × String)
  dependencyRoot    : String
  theoremArtifact   : String
  carriesTrust      : Bool
  trustedBoundaries : List String
  toolchainId       : String
  compilerVersion   : String
  checker           : String
  workspaceId       : String
  importsId         : String
  deriving Repr

/-- Decode stored bytes, or refuse and say why. -/
def StoredReceipt.decode (s : String) : Except ReceiptDecodeRefusal StoredReceipt := do
  let lines := (s.splitOn "\n").filter (fun l => !l.trimAscii.isEmpty)
  let mut single : List (String × String) := []
  let mut tables : List (Name × String) := []
  let mut boundaries : List String := []
  for line in lines do
    let parts := line.trimAscii.toString.splitOn " "
    match parts with
    | key :: rest =>
      let val := " ".intercalate rest
      if key == "table" then
        match rest with
        | n :: d :: _ => tables := tables ++ [(Name.mkSimple n, d)]
        | _ => throw (.malformedField "table" val)
      else if key == "boundary" then
        if val.isEmpty then throw (.malformedField "boundary" val)
        boundaries := boundaries ++ [val]
      else if ["schema", "subject", "edge", "root", "artifact", "trust",
               "toolchain", "compiler", "checker", "workspace", "imports"].contains key then
        if single.any (·.1 == key) then throw (.duplicateField key)
        single := single ++ [(key, val)]
      else throw (.unknownKey key)
    | [] => pure ()
  let field := fun (n : String) => (single.find? (·.1 == n)).map (·.2)
  let some schema := field "schema" | throw (.missingField "schema")
  -- SCHEMA FIRST, before any content is read. An older envelope is not comparable at all, and
  -- reporting a field mismatch for it would claim the program moved when the format did.
  if schema != receiptSchemaVersion then throw (.schemaUnreadable schema)
  let req := fun (n : String) =>
    match field n with
    | none => Except.error (ReceiptDecodeRefusal.missingField n)
    | some v => if v.isEmpty then Except.error (.malformedField n v) else Except.ok v
  let subject ← req "subject"
  let edgeTag ← req "edge"
  let some edge := DependencyEdge.ofCanonical? edgeTag | throw (.malformedField "edge" edgeTag)
  let root ← req "root"
  let artifact ← req "artifact"
  let trustTag ← req "trust"
  let carriesTrust ←
    if trustTag == "true" then pure true
    else if trustTag == "false" then pure false
    else throw (.malformedField "trust" trustTag)
  let toolchain ← req "toolchain"
  let compiler ← req "compiler"
  let checker ← req "checker"
  -- THE READABLE IDENTITY MUST BE THE BOUND ONE. Advisory policy reads `checker`; currency compares
  -- `toolchain`. If they could disagree, a record could be current under one checker while policy
  -- judged another, so a record whose readable identity does not re-hash to its binding is refused.
  if toolchainIdOf compiler checker != toolchain then throw (.identityNotBound toolchain)
  let workspace ← req "workspace"
  let imports ← req "imports"
  -- THE SAME CONSISTENCY THE MINT REQUIRES. A stored receipt claiming trust while naming no
  -- boundary is a qualification a reader cannot act on, and one naming boundaries with the flag
  -- false disagrees with itself. Re-checked here because these bytes did not come from the mint.
  if carriesTrust != !boundaries.isEmpty then
    throw (.malformedField "trust" trustTag)
  return { schemaVersion := schema, subjectDigest := subject, edge := edge
         , tableBindings := tables, dependencyRoot := root, theoremArtifact := artifact
         , carriesTrust, trustedBoundaries := boundaries
         , toolchainId := toolchain, compilerVersion := compiler, checker
         , workspaceId := workspace, importsId := imports }

/-- The fields a currency comparison reads, from either kind of receipt.

    ONE STRUCTURE so the comparison exists ONCE. A minted receipt and a stored one are deliberately
    different types, and writing the comparison twice would mean the authoritative path and the
    untrusted-input path could disagree about what "current" means — with the stored side, the one
    that reads attacker-controlled bytes, being the copy nobody re-checks. -/
structure ReceiptFacts where
  subjectDigest     : String
  edge              : DependencyEdge
  tableBindings     : List (Name × String)
  dependencyRoot    : String
  theoremArtifact   : String
  carriesTrust      : Bool
  trustedBoundaries : List String
  toolchainId       : String
  workspaceId       : String
  importsId         : String
  deriving Repr

def ProofEvidenceReceipt.facts (r : ProofEvidenceReceipt) : ReceiptFacts :=
  { subjectDigest := r.subjectDigest, edge := r.edge, tableBindings := r.tableBindings
  , dependencyRoot := r.dependencyRoot, theoremArtifact := r.theoremArtifact
  , carriesTrust := r.carriesTrust, trustedBoundaries := r.trustedBoundaries
  , toolchainId := r.toolchainId, workspaceId := r.workspaceId, importsId := r.importsId }

def StoredReceipt.facts (r : StoredReceipt) : ReceiptFacts :=
  { subjectDigest := r.subjectDigest, edge := r.edge, tableBindings := r.tableBindings
  , dependencyRoot := r.dependencyRoot, theoremArtifact := r.theoremArtifact
  , carriesTrust := r.carriesTrust, trustedBoundaries := r.trustedBoundaries
  , toolchainId := r.toolchainId, workspaceId := r.workspaceId, importsId := r.importsId }

/-- Which bound fields of `r` differ from `fresh`, by name, in envelope order (R-0208).

    A REPORTING aid, never a verdict: `isCurrentAgainst` decides currency. It exists so a downgrade
    of ONE dimension stays legible — a receipt established under a since-advised checker differs
    only in `toolchain`, and its subject, correspondence and dependency facts must remain visibly
    intact rather than disappear into a single "not current". -/
def ReceiptFacts.differingFields (r fresh : ReceiptFacts) : List String :=
  let norm := fun (t : List (Name × String)) => t.mergeSort (fun a b => toString a.1 ≤ toString b.1)
  [ ("subject", r.subjectDigest != fresh.subjectDigest)
  , ("edge", r.edge != fresh.edge)
  , ("tables", norm r.tableBindings != norm fresh.tableBindings)
  , ("root", r.dependencyRoot != fresh.dependencyRoot)
  , ("artifact", r.theoremArtifact != fresh.theoremArtifact)
  , ("trust", r.carriesTrust != fresh.carriesTrust
              || r.trustedBoundaries.mergeSort (· ≤ ·) != fresh.trustedBoundaries.mergeSort (· ≤ ·))
  , ("toolchain", r.toolchainId != fresh.toolchainId)
  , ("workspace", r.workspaceId != fresh.workspaceId)
  , ("imports", r.importsId != fresh.importsId) ].filterMap fun (n, d) => if d then some n else none

/-- Every bound field name, in envelope order. -/
def receiptFieldNames : List String :=
  ["subject", "edge", "tables", "root", "artifact", "trust", "toolchain", "workspace", "imports"]

/-- Is a stored receipt still current against freshly computed material?

    Schema is checked FIRST and separately: an older envelope is not comparable at all, and
    answering `false` there would mean "the proof went stale", which is a claim about the
    program rather than about the format. Callers must branch on `comparable` before reading
    this — the two questions have different repairs, which is the same distinction
    `needsRecheck` draws against `stale`.

    Everything the receipt binds participates. A change to a table's body moves its digest; a
    different toolchain, workspace or import closure moves those identities; a swapped pair of
    table digests changes which name carries which value and survives the sort. Any of them
    means the recorded evidence was established against something other than what is here now. -/
def ReceiptFacts.isCurrentAgainst
    (r : ReceiptFacts) (subjectDigest : String) (edge : DependencyEdge)
    (tableBindings : List (Name × String))
    (dependencyRoot theoremArtifact : String)
    (carriesTrust : Bool) (trustedBoundaries : List String)
    (toolchainId workspaceId importsId : String) : Bool :=
  let normalized := tableBindings.mergeSort (fun a b => toString a.1 ≤ toString b.1)
  r.subjectDigest == subjectDigest
    -- The EDGE participates, and its omission was a real hole: a receipt recorded for a
    -- `contract` edge would have read current against `body` material, which is the direction
    -- that matters — a claim that survives an implementation change it actually depends on.
    && r.edge == edge
    && r.tableBindings == normalized
    -- EVERYTHING THE RECEIPT BINDS PARTICIPATES, or it is decoration. A field added to the envelope
    -- and left out of this comparison is worse than an absent field: it reads as bound.
    && r.dependencyRoot == dependencyRoot
    && r.theoremArtifact == theoremArtifact
    && r.carriesTrust == carriesTrust
    && r.trustedBoundaries == trustedBoundaries.mergeSort (· ≤ ·)
    && r.toolchainId == toolchainId
    && r.workspaceId == workspaceId
    && r.importsId == importsId

/-- Thin wrapper: a minted receipt's currency, through the one comparison. -/
def ProofEvidenceReceipt.isCurrentAgainst
    (r : ProofEvidenceReceipt) (subjectDigest : String) (edge : DependencyEdge)
    (tableBindings : List (Name × String))
    (dependencyRoot theoremArtifact : String)
    (carriesTrust : Bool) (trustedBoundaries : List String)
    (toolchainId workspaceId importsId : String) : Bool :=
  r.facts.isCurrentAgainst subjectDigest edge tableBindings dependencyRoot theoremArtifact
    carriesTrust trustedBoundaries toolchainId workspaceId importsId

/-- Thin wrapper: a STORED receipt's currency, through the same comparison.

    This is the only thing a stored receipt can do. It cannot become evidence, it cannot be minted
    from, and it cannot make a claim proved on its own — it can only agree or disagree with material
    computed fresh from the program right now. A receipt swapped onto another claim's name in the
    storage file disagrees here, because the subject digest is compared rather than the file key. -/
def StoredReceipt.isCurrentAgainst
    (r : StoredReceipt) (subjectDigest : String) (edge : DependencyEdge)
    (tableBindings : List (Name × String))
    (dependencyRoot theoremArtifact : String)
    (carriesTrust : Bool) (trustedBoundaries : List String)
    (toolchainId workspaceId importsId : String) : Bool :=
  r.facts.isCurrentAgainst subjectDigest edge tableBindings dependencyRoot theoremArtifact
    carriesTrust trustedBoundaries toolchainId workspaceId importsId

/-- A stored receipt is comparable only when it was written under this envelope version.
    Distinct from "the contents differ" for the same reason `needsRecheck` is distinct from
    `stale`: the format changed, and the program may not have. -/
def ProofEvidenceReceipt.comparable (r : ProofEvidenceReceipt) : Bool :=
  r.schemaVersion == receiptSchemaVersion

/-- What a stored receipt is worth against current material. ONE value, not two booleans.

    `comparable` and `isCurrentAgainst` are each correct and had to be called in the right
    order: schema first, then contents. That is a predicate a consumer must remember, and this
    file already contains one lesson about predicates that must be remembered — the receipt's
    own invariant was documentation until the constructor was closed. A consumer that reads
    them out of order reports "the proof went stale" when the ENVELOPE changed, which is a
    claim about the program rather than about the format.

    So the sequencing lives here, once:

      `needsRecheck`  written under a different envelope — not comparable, and NOT a statement
                      about the program. Repair: re-verify and re-record.
      `notCurrent`    same envelope, and something it binds has moved. Repair: fix or re-prove.
      `current`       same envelope, everything it binds matches. -/
inductive ReceiptDisposition where
  | current
  | notCurrent
  | needsRecheck
deriving Repr, DecidableEq, Inhabited

/-- The single entry point a consumer should use. Checks schema BEFORE contents, so the
    "different format" and "different program" answers can never be swapped. -/
def ProofEvidenceReceipt.disposition
    (r : ProofEvidenceReceipt) (subjectDigest : String) (edge : DependencyEdge)
    (tableBindings : List (Name × String))
    (dependencyRoot theoremArtifact : String)
    (carriesTrust : Bool) (trustedBoundaries : List String)
    (toolchainId workspaceId importsId : String) : ReceiptDisposition :=
  if !r.comparable then .needsRecheck
  else if r.isCurrentAgainst subjectDigest edge tableBindings dependencyRoot theoremArtifact
      carriesTrust trustedBoundaries toolchainId workspaceId importsId
    then .current else .notCurrent


end Concrete.Proof
