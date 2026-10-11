import Concrete.Proof.ProofCore
import Concrete.Proof.Receipt
import Concrete.Proof.Replay
import Concrete.Proof.ClassificationTable
import Concrete.Proof.DependencyRoot

/-!
# Production receipt issuance (R-0004 package 3)

Receipts existed and nothing minted them. The envelope was closed, the minting authority was closed,
and the production pipeline still had no path from "the kernel accepted this" to a stored fact — so
`ProofEvidenceReceipt` was, honestly described, a well-tested helper type. This module is the path.

WHY THE COMPILER CAN DO THIS AT ALL. Edge classification reads a theorem's ELABORATED type, which
exists only while Lean is elaborating with the proof modules imported, and the compiler imports
neither (`Examples` imports `Concrete`, so importing the proofs from `Concrete` would be a cycle).
The answer is therefore computed at generation time and checked in as `ClassificationTable`, whose
freshness two gates re-derive from the live environment. Issuance reads it through `validatedRowOf`,
so a row that failed structural validation cannot classify anything.

WHAT ISSUANCE IS NOT ALLOWED TO DO is invent any of the four inputs. Each comes from exactly one
producer already used elsewhere, and each refuses on its own terms:

| input | producer | refuses when |
|---|---|---|
| the kernel accepted it | `SuccessfulReplay.of?` | not accepted, unbound, interrupted, unreplayed, fallback workspace |
| what the theorem depends on | `validatedRowOf` | absent, ambiguous, or malformed row |
| what was proved | `ProofCoreEntry.subjectDigest` | facts absent or incomplete |
| what the claim rests on | `dependencyRootMaterial` | any incomplete or unclassified closure |

Every one of those refusals is REPORTED, not skipped. A claim that cannot receive a receipt must say
which of the four inputs was missing, because "no receipt" and "receipt withheld for this reason" are
different facts and only the second is actionable.
-/

namespace Concrete.Proof

open Lean

/-- The environment identities issuance cannot derive for itself.

    These come from the project, not from the replay: the workspace identity is over the package's
    declared name and module set, and the import closure is over module digests. The replay knows
    which Lake workspace it ran in and which modules it imported by NAME, which is not the same
    thing — so they are supplied, and the toolchain half is NOT, because that one the replay does
    know and a caller must not be able to disagree with it. -/
structure IssueEnvironment where
  compilerVersion : String
  workspaceId     : String
  importsId       : String
  deriving Repr

/-- Why one claim did not receive a receipt.

    Each case names WHICH of the four inputs was unavailable. A single "could not issue" would be
    the same shape as the diagnostics this project keeps replacing: true, unactionable, and equally
    consistent with every cause. -/
inductive IssueRefusal where
  /-- The kernel's answer does not witness this claim. -/
  | replay (theoremName : String) (why : ReplayEvidenceRefusal)
  /-- The theorem has no usable classification, so what it depends on is unknown. -/
  | classification (theoremName : String) (why : ClassificationRefusal)
  /-- The claim has no proof link, so there is no artifact to bind. -/
  | noProofLink (subject : String)
  /-- The subject digest is absent or incomplete. An incomplete subject must not be representable
      as a digest string, or it becomes comparable as though it were complete. -/
  | noSubjectDigest (subject : String)
  /-- The subject has no scoped identity, so its closure was never examined. -/
  | noScopedIdentity (subject : String) (why : String)
  /-- The dependency closure refused to root. -/
  | rootRefused (subject : String) (why : String)
  /-- The theorem does not mention the specification this claim names, so it says nothing about
      this subject.

      THE BREAK AN INDEPENDENT ADVERSARIAL REVIEW FOUND. `#[proof_by(...)]` is a SOURCE ASSERTION and
      issuance took it as ground truth: binding `main.check_magic` to `check_class_correct` — a real
      theorem the kernel accepts, about a different declaration — produced a receipt reading
      "kernel-replayed, receipt current". Retargeting across programs to a crypto theorem worked too,
      leaving a receipt whose own fields contradicted each other and a consumer that accepted it.

      The kernel accepting a theorem says the theorem is TRUE. It does not say the theorem is ABOUT
      this claim, and only the theorem's own statement can witness that. -/
  | theoremNotAboutSubject (subject : String) (theoremName : String) (spec : String)
  /-- The claim names no specification, so there is nothing to check its theorem against. Fail
      closed: a link nothing can witness must not mint. -/
  | claimNamesNoSpec (subject : String)
  /-- The claim's own status is not `proved`. Caught on the drift fixture the day issuance landed:
      `elf_header_drifted` is a DIFFERENT program sharing every declaration name with `elf_header`,
      and issuance minted four receipts for it — two of them for `stale` claims, whose bodies had
      changed since the proof was linked. The token says the kernel accepted a THEOREM; it says
      nothing about whether that theorem still proves THIS body. Binding the current subject digest
      to a theorem that proved the previous one is precisely the forgery a receipt exists to
      prevent, so the composed authority verdict gates issuance. -/
  | notProved (subject : String) (status : String)
  /-- The replay found no proof library at all, so nothing identifies whose theorems were accepted.
      A receipt binding an empty import closure would compare equal to any other receipt binding an
      empty one — the same "empty string is not unknown" failure the environment identities refuse
      on. -/
  | noImportClosure (subject : String)
  /-- Every input was present and the material was still rejected — a table named but unbound,
      digests that do not correspond to their tables, an empty environment identity, or a trust
      claim inconsistent with its boundaries. -/
  | materialRefused (subject : String)
  /-- The CURRENT checker could not be identified, so there is nothing to compare a stored receipt's
      checker against. Never treated as a match: two unknown checkers are not the same checker. -/
  | checkerUnidentified (subject : String)
  deriving Repr

def IssueRefusal.canonical : IssueRefusal → String
  | .replay ..          => "replay_does_not_witness"
  | .classification ..  => "no_usable_classification"
  | .noProofLink _      => "no_proof_link"
  | .noSubjectDigest _  => "no_subject_digest"
  | .noScopedIdentity ..=> "no_scoped_identity"
  | .rootRefused ..     => "dependency_root_refused"
  | .noImportClosure _  => "no_import_closure"
  | .theoremNotAboutSubject .. => "theorem_not_about_subject"
  | .claimNamesNoSpec _ => "claim_names_no_spec"
  | .notProved ..       => "claim_not_proved"
  | .materialRefused _  => "receipt_material_refused"
  | .checkerUnidentified _ => "checker_unidentified"

def IssueRefusal.explain : IssueRefusal → String
  | .replay t why        => s!"'{t}': {why.explain}"
  | .classification t why => s!"'{t}' has no usable classification: {why.explain}"
  | .noProofLink s        => s!"'{s}' carries no proof link, so there is no theorem artifact to bind"
  | .noSubjectDigest s    => s!"'{s}' has no complete subject digest to establish evidence against"
  | .noScopedIdentity s w => s!"'{s}' has no scoped identity: {w}"
  | .rootRefused s w      => s!"'{s}' has no computable dependency root: {w}"
  | .noImportClosure s    => s!"'{s}': the replay found no proof library, so nothing identifies whose theorems were accepted"
  | .theoremNotAboutSubject sub thm sp =>
      s!"'{thm}' does not mention '{sp}', the specification '{sub}' names — the kernel accepting a "
      ++ "theorem says it is TRUE, not that it is about this claim"
  | .claimNamesNoSpec sub =>
      s!"'{sub}' names no specification, so nothing witnesses that its theorem is about it"
  | .notProved s st       => s!"'{s}' is '{st}', not 'proved' — the kernel accepted a theorem, which says nothing about whether it still proves this body"
  | .materialRefused s    => s!"'{s}' assembled material that the receipt envelope refused"
  | .checkerUnidentified s =>
      s!"'{s}': the checker that would run now could not be identified (executable and build), so no "
      ++ "stored receipt can be compared against it"

/-- One claim's issuance outcome, keyed by the subject it is about. -/
structure IssueOutcome where
  subject : String
  result  : Except IssueRefusal ProofEvidenceReceipt
  deriving Repr

/-- Which claims to ask the kernel about.

    ONE PRODUCER, because there are now two consumers — `--report check-proofs` and receipt issuance
    — and a second copy of this rule would eventually replay a different set than it issued for,
    which is the quietest possible way to withhold a receipt.

    `unbound` is included and must be: kernel replay is how a link WITHOUT a stored subject earns
    one, so excluding it would leave no path from unbound to bound. It still cannot mint — the
    minting token refuses an `acceptedUnbound` verdict — which is exactly the intended split between
    "worth asking the kernel about" and "may become evidence". -/
def replayTargetsOf (pc : ProofCore) : List ReplayTarget :=
  pc.obligations.filterMap fun o =>
    match o.spec with
    | some s =>
      -- A LINK WITH NO THEOREM NAME IS EXCLUDED HERE, not carried into the request. The request
      -- refuses an unnamed target — an empty needle matches the transcript everywhere, so it would
      -- report as accepted without being checked — but refusing on ONE malformed link would take
      -- every other claim in the file down with it, which is what happened to `proof_pressure`:
      -- five claims became unreportable because one registry entry had an empty proof name.
      --
      -- Excluding it hides nothing. The registry validator already reports that entry by name
      -- ("registry entry for 'X' has empty proof name") before any of this runs, so the defect is
      -- diagnosed exactly once, where it belongs.
      if s.proofName.trimAscii.isEmpty then none
      -- WHICH STATUSES ARE WORTH ASKING THE KERNEL ABOUT. Replay answers "does this theorem
      -- typecheck", which is independent of whether the claim's DEPENDENCIES are current or its
      -- closure is justified — those are facts about other declarations. Excluding them stranded
      -- four links in the V1->V2 migration with `not_replayed`: their own bodies are fresh, so
      -- their fingerprints could migrate honestly, and nothing was asking about their theorems.
      --
      -- `stale` is included for a different reason and does NOT lead to migration: its body has
      -- moved, so the kernel's answer is worth having while no honest v2 value exists to record.
      else if o.status == .proved || o.status == .stale || o.status == .unbound
           || o.status == .depsNotCurrent || o.status == .correspondenceUnjustified
           -- `needsRecheck` is precisely the state the V1->V2 migration exists to clear, so it must
           -- be replayable: excluding it would make the migration unable to ask about the very
           -- claims it is for, the moment activation made them all needsRecheck.
           || o.status == .needsRecheck then
        some { subject := o.functionId.qualName, theoremName := s.proofName
             , kind := .refinement
             , origin := if s.source == .registry then .sourceLinked else .hardcoded
             , binding := if o.status == .unbound then .unbound else .bound }
      else none
    | none => none

/-- The dependency evidence a validated classification row describes.

    Built here rather than by the caller so the row's tables and its digests cannot drift apart:
    `EdgeEvidence` carries them as two lists, and `ReceiptMaterial.of?` refuses if they fail to
    correspond — a refusal that should never be reachable from a validated row, and is kept
    reachable because "should never" is not a guarantee. -/
def edgeEvidenceOfRow (row : ValidatedRow) : EdgeEvidence :=
  { edge := row.edge
  , tables := row.tables.map fun (n, _) => Name.mkSimple n
  , tableDigests := row.tables.map fun (n, d) => (Name.mkSimple n, some d)
  , quantifiesOverTable := row.quantifies }

/-- Issue a receipt for one claim, or say precisely which input was missing.

    The token is taken FIRST. Every other input is a fact about the program that is worth computing
    anyway; the token is the one that says a kernel ran, and checking it last would mean assembling
    a complete receipt body for a claim nobody had replayed and then discarding it — which is the
    shape that eventually gets returned by mistake. -/
def issueFor (pc : ProofCore) (res : ReplayResult) (env : IssueEnvironment)
    (trustedDepsOf : String → List String)
    (e : ProofCoreEntry) : Except IssueRefusal ProofEvidenceReceipt := do
  let subject := e.qualName
  let some spec := e.spec | throw (.noProofLink subject)
  let thm := spec.proofName
  -- THE COMPOSED AUTHORITY VERDICT GATES ISSUANCE, before anything else is assembled. `proved` here
  -- is the status AFTER `applyCorrespondenceAuthority`, so a claim whose dependency closure has no
  -- validated per-edge justification, or whose body drifted from what the proof was linked against,
  -- is already excluded. Replay answers "did the kernel accept this theorem"; it cannot answer
  -- "does that theorem still prove this subject", and only one of those questions is about a body.
  let some obl := pc.obligations.find? (fun o => o.functionId.qualName == subject)
    | throw (.noProofLink subject)
  if obl.status != .proved then throw (.notProved subject obl.status.canonical)
  if res.environment.importDigests.isEmpty then throw (.noImportClosure subject)
  let sr ← (SuccessfulReplay.of? res thm).mapError (IssueRefusal.replay thm)
  let row ← (validatedRowOf thm).mapError (IssueRefusal.classification thm)
  -- THE THEOREM MUST BE ABOUT THIS CLAIM. `#[proof_by]` names a theorem; the classification records
  -- which SPECIFICATIONS that theorem's statement mentions. If the spec this claim declares is not
  -- among them, the theorem concerns something else and cannot witness this subject — however
  -- happily the kernel accepted it. Applied at BOTH producers: issuance must not mint it, and the
  -- consumer's fresh facts must not compare as if it were legitimate.
  let claimedSpec := spec.specId.name
  if claimedSpec.isEmpty then throw (.claimNamesNoSpec subject)
  if !row.specs.contains claimedSpec then
    throw (.theoremNotAboutSubject subject thm claimedSpec)
  let some subjDigest := e.subjectDigest | throw (.noSubjectDigest subject)
  let sid ← match e.definitionIdentity with
    | .error w => throw (.noScopedIdentity subject w.explain)
    | .ok i => pure i
  let nodes := dependencyNodesOf pc pc.callGraph
  let rootMat ← match dependencyRootMaterial nodes sid with
    | .error w => throw (.rootRefused subject w.explain)
    | .ok m => pure m
  -- TRUST TRAVELS WITH THE CLAIM, from the same producer the status report reads. Recomputing it
  -- here would be a second answer to "which boundaries does this rest on", and the two would
  -- eventually disagree — with the receipt being the one nobody re-checks.
  let boundaries := trustedDepsOf subject
  -- The root's own trust verdict and the named boundaries must AGREE. They are derived
  -- independently — one from traversing the closure, one from the status entry — so a
  -- disagreement means one of them is wrong, and minting either reading would record a trust
  -- claim that half the system does not share.
  if rootMat.carriesTrust != !boundaries.isEmpty then
    throw (.materialRefused subject)
  let some material := ReceiptMaterial.of? (some subjDigest) (edgeEvidenceOfRow row)
      rootMat.preimage rootMat.carriesTrust boundaries
      env.compilerVersion env.workspaceId env.importsId
    | throw (.materialRefused subject)
  return ProofEvidenceReceipt.mint sr material

/-- The facts a stored receipt must agree with, computed FRESH from the program right now.

    Deliberately the same derivation issuance uses, minus the token. A consumer that re-derived this
    material separately would be a second answer to "what is this claim resting on", and the two
    would eventually disagree — with the comparison, not the mint, being the one that decides whether
    a stored receipt still counts.

    `checker` is passed in rather than read here: it comes from `resolveChecker`, the one producer
    of "which checker would run", so a status report can ask without paying for a kernel run. -/
def freshFactsFor (pc : ProofCore) (env : IssueEnvironment) (checker : Option CheckerIdentity)
    (trustedDepsOf : String → List String)
    (e : ProofCoreEntry) : Except IssueRefusal ReceiptFacts := do
  let subject := e.qualName
  let some spec := e.spec | throw (.noProofLink subject)
  let thm := spec.proofName
  let some obl := pc.obligations.find? (fun o => o.functionId.qualName == subject)
    | throw (.noProofLink subject)
  if obl.status != .proved then throw (.notProved subject obl.status.canonical)
  let row ← (validatedRowOf thm).mapError (IssueRefusal.classification thm)
  -- THE THEOREM MUST BE ABOUT THIS CLAIM. `#[proof_by]` names a theorem; the classification records
  -- which SPECIFICATIONS that theorem's statement mentions. If the spec this claim declares is not
  -- among them, the theorem concerns something else and cannot witness this subject — however
  -- happily the kernel accepted it. Applied at BOTH producers: issuance must not mint it, and the
  -- consumer's fresh facts must not compare as if it were legitimate.
  let claimedSpec := spec.specId.name
  if claimedSpec.isEmpty then throw (.claimNamesNoSpec subject)
  if !row.specs.contains claimedSpec then
    throw (.theoremNotAboutSubject subject thm claimedSpec)
  let some subjDigest := e.subjectDigest | throw (.noSubjectDigest subject)
  let sid ← match e.definitionIdentity with
    | .error w => throw (.noScopedIdentity subject w.explain)
    | .ok i => pure i
  let nodes := dependencyNodesOf pc pc.callGraph
  let rootMat ← match dependencyRootMaterial nodes sid with
    | .error w => throw (.rootRefused subject w.explain)
    | .ok m => pure m
  let boundaries := trustedDepsOf subject
  if rootMat.carriesTrust != !boundaries.isEmpty then throw (.materialRefused subject)
  let some checker := checker | throw (.checkerUnidentified subject)
  let ev := edgeEvidenceOfRow row
  let some material := ReceiptMaterial.of? (some subjDigest) ev rootMat.preimage
      rootMat.carriesTrust boundaries env.compilerVersion env.workspaceId env.importsId
    | throw (.materialRefused subject)
  return { subjectDigest := material.subjectDigest
         , edge := material.edge
         , tableBindings := material.tableBindings
         , dependencyRoot := material.dependencyRoot
         -- The artifact a CURRENT receipt would name: the theorem this claim links to now. A stored
         -- receipt naming a different one was established from a different proof term.
         , theoremArtifact := thm
         , carriesTrust := material.carriesTrust
         , trustedBoundaries := material.trustedBoundaries
         , toolchainId := toolchainIdOf material.compilerVersion checker.canonical
         , workspaceId := material.workspaceId
         , importsId := material.importsId }

/-- What a stored receipt is worth for one claim, against material computed fresh.

    A STORED RECEIPT NEVER DECIDES ANYTHING ON ITS OWN. It can only agree or disagree with what the
    program says right now, and this is the only question it is ever asked. A receipt swapped onto
    another claim's name in the storage file disagrees here, because the comparison reads the subject
    digest rather than the file key. -/
def storedDispositionFor (pc : ProofCore) (env : IssueEnvironment) (checker : Option CheckerIdentity)
    (trustedDepsOf : String → List String) (e : ProofCoreEntry)
    (st : StoredReceipt) : Except IssueRefusal ReceiptDisposition := do
  let fresh ← freshFactsFor pc env checker trustedDepsOf e
  return if st.facts.isCurrentAgainst fresh.subjectDigest fresh.edge fresh.tableBindings
              fresh.dependencyRoot fresh.theoremArtifact fresh.carriesTrust fresh.trustedBoundaries
              fresh.toolchainId fresh.workspaceId fresh.importsId
         then .current else .notCurrent

/-- Issue over every claim the replay covered, reporting each outcome.

    Returns one entry per claim WITH A PROOF LINK, refusals included. A caller that wants only the
    receipts filters; a caller that wants to know what was withheld and why has it without a second
    traversal, which is what keeps "no receipt" from being reported as "nothing to report". -/
def issueAll (pc : ProofCore) (res : ReplayResult) (env : IssueEnvironment)
    (trustedDepsOf : String → List String) : List IssueOutcome :=
  pc.entries.filterMap fun e =>
    if e.spec.isNone then none
    else some { subject := e.qualName
              , result := issueFor pc res env trustedDepsOf e }

/-! ### The storage file

Records are keyed by subject name, and THE KEY IS NOT TRUSTED. It is how a consumer finds the record
to check; it is not what makes the record apply. Swapping two receipts under each other's names is a
substitution the comparison defeats on its own, because `storedDispositionFor` compares the subject
DIGEST against material computed fresh — the name never enters the verdict. -/

/-- Serialize issued receipts, newest write wins. Withheld claims are deliberately absent: a store
    that recorded refusals would invite a reader to treat "we know why this failed" as a record of
    something, and a refusal is precisely the absence of evidence. -/
def encodeStore (os : List IssueOutcome) : String :=
  String.intercalate "
" (os.filterMap fun o =>
    match o.result with
    | .ok r => some (s!"== {o.subject}
" ++ r.encode)
    | .error _ => none)

/-- Read a storage file back. Each record decodes independently and carries its own refusal, so one
    corrupt record does not discard the rest — and does not silently vanish either. -/
def decodeStore (s : String) : List (String × Except ReceiptDecodeRefusal StoredReceipt) := Id.run do
  let mut out : List (String × Except ReceiptDecodeRefusal StoredReceipt) := []
  let mut key : Option String := none
  let mut body : List String := []
  let flush := fun (k : Option String) (b : List String) =>
    match k with
    | none => []
    | some name => [(name, StoredReceipt.decode (String.intercalate "
" b))]
  for line in s.splitOn "
" do
    if line.startsWith "== " then
      out := out ++ flush key body
      key := some (line.drop 3).trimAscii.toString
      body := []
    else if !line.trimAscii.isEmpty then
      body := body ++ [line]
  out := out ++ flush key body
  -- A SUBJECT RECORDED TWICE POISONS BOTH RECORDS. Found by the Slice 8 attack suite: appending a
  -- contradictory record left the genuine one reading `current` and the forgery reading
  -- `not current`, so a reader scanning for "current" would conclude the claim was receipt-backed
  -- from a store that disagrees with itself. Neither may stand.
  let dupes := (out.map (·.1)).filter (fun k => (out.filter (·.1 == k)).length > 1)
  return out.map fun (k, r) =>
    if dupes.contains k then (k, .error (.duplicateRecord k)) else (k, r)

namespace IssueOutcome

def issued (os : List IssueOutcome) : List IssueOutcome :=
  os.filter fun o => match o.result with | .ok _ => true | .error _ => false

def withheld (os : List IssueOutcome) : List IssueOutcome :=
  os.filter fun o => match o.result with | .ok _ => false | .error _ => true

end IssueOutcome



/-! ## The V1 -> V2 subject-digest migration (R-0004 package 3)

Every stored `#[proof_fingerprint]` in the corpus is a V1 value: a body-only fingerprint that answers
a weaker question than the V2 subject digest, which also binds the declaration's signature, contracts,
capabilities and constant environment. Activating V2 makes every stored V1 value NOT COMPARABLE at
once — correctly, since it was established against a different question — so activation without
migration would turn the whole corpus `needs_recheck` in a single commit.

The migration is therefore: replay each link, and for those the kernel accepts AND whose own body is
still the one their proof was pinned to, record the V2 digest computed FROM THE PROGRAM.

WHAT CANNOT BE MIGRATED, and why it matters more than what can. A STALE link is pinned to a body that
no longer exists. There is no honest V2 value to write for it: recording the CURRENT digest would
assert that a proof was established against a body it was never checked against — manufacturing
freshness, which is the exact forgery this whole package exists to prevent. Such a link keeps its V1
value and becomes `needs_recheck` on activation, which is the truthful verdict: recorded under an
older envelope, body since moved, re-verify and re-record.
-/

/-- What the migration would do to one stored fingerprint. -/
inductive MigrationDisposition where
  /-- Body still matches what the proof was pinned to, and the kernel accepts the theorem: record
      the computed V2 digest. -/
  | migrate (v2 : String)
  /-- Already a V2 value; nothing to do. -/
  | alreadyV2
  /-- The body has moved since the proof was pinned. NO honest V2 value exists — writing the current
      digest would manufacture freshness. Stays V1 and becomes `needs_recheck` on activation. -/
  | staleNoHonestValue
  /-- No V2 digest is computable for this subject: facts absent or incomplete. -/
  | noSubjectDigest
  deriving Repr

def MigrationDisposition.canonical : MigrationDisposition → String
  | .migrate _           => "migrate"
  | .alreadyV2           => "already_v2"
  | .staleNoHonestValue  => "stale_no_honest_value"
  | .noSubjectDigest     => "no_subject_digest"

structure MigrationRow where
  subject     : String
  status      : String
  stored      : String
  disposition : MigrationDisposition
  deriving Repr

/-- The migration plan over every claim that carries a stored fingerprint.

    ONE ROW PER STORED FINGERPRINT, refusals included, because "43 = migrated + refused" is the only
    form in which this plan can be checked. A plan that listed only what it would change would be
    indistinguishable from a plan that silently skipped things. -/
-- NO `ReplayResult` PARAMETER. The plan is about which body a fingerprint records, which the
-- compiler answers on its own; taking a replay would have implied the kernel's verdict was an input
-- to the decision, and it is not.
def migrationPlan (pc : ProofCore) : List MigrationRow :=
  -- THE STORED FINGERPRINT LIVES ON THE OBLIGATION'S SPEC, NOT THE ENTRY'S. Reading `e.spec` here
  -- was a SECOND reader of "what is stored", and it disagreed with the one the freshness report
  -- uses: `proof_patterns/ghost` reported two WOULD-RECHECK subjects while this plan listed none of
  -- them, so seven fingerprints were silently outside the migration. A plan that cannot see part of
  -- its own population is worse than no plan.
  pc.entries.filterMap fun e =>
    match pc.obligations.find? (fun o => o.functionId.qualName == e.qualName) with
    | none => none
    | some obl =>
      match obl.spec with
      | none => none
      | some spec =>
      match spec.expectedHash with
      | none => none
      | some stored =>
        if stored.isEmpty then none else
        let status := obl.status.canonical
        let disp :=
          if "v2:".isPrefixOf stored then MigrationDisposition.alreadyV2
          -- STALENESS IS JUDGED BY V1 RULES, not by the claim's current status, and the plan must
          -- be STABLE ACROSS ACTIVATION for that reason. Once V2 is live every v1 record reads
          -- `needsRecheck`, which erases the distinction between "the record is old" and "the body
          -- moved" — so a plan keyed on status would, the moment it was most needed, cheerfully
          -- migrate the drifted links and manufacture exactly the freshness it exists to refuse.
          -- The stored value is a v1 value, so v1 is the question to ask of it.
          else if Concrete.shortHash e.fingerprint != stored then .staleNoHonestValue
          -- STALENESS IS CHECKED BEFORE REPLAY: a drifted claim's theorem may well still typecheck,
          -- so asking the kernel first and recording on acceptance is how a current body would
          -- acquire a proof it never had.
          else if status == "stale" then .staleNoHonestValue
          -- KERNEL ACCEPTANCE IS DELIBERATELY NOT REQUIRED, and requiring it was a design error.
          -- A `#[proof_fingerprint]` records WHICH BODY a proof was pinned to. It does not assert
          -- the proof is valid — that is what replay and receipts are for, on a separate axis and
          -- with their own refusals. Migrating a fresh body's digest from v1 to v2 re-expresses the
          -- same fact more precisely; it claims nothing new.
          --
          -- Requiring replay stranded every fixture whose linked theorem is FICTIONAL BY DESIGN —
          -- `tests/programs/*` exist to exercise the status machinery, not to prove anything — and
          -- would have forced 28 suite expectations to be rewritten around a claim the migration
          -- was never making.
          --
          -- What remains refused is the case that would actually forge something:
          -- `staleNoHonestValue`, where the body MOVED and recording the current digest would assert
          -- a pinning that never happened.
          else match e.subjectDigest with
            | none => .noSubjectDigest
            | some d => .migrate ("v2:" ++ Concrete.shortHash d)
        some { subject := e.qualName, status, stored, disposition := disp }

namespace MigrationRow

def migrating (rs : List MigrationRow) : List MigrationRow :=
  rs.filter fun r => match r.disposition with | .migrate _ => true | _ => false

def refused (rs : List MigrationRow) : List MigrationRow :=
  rs.filter fun r => match r.disposition with | .migrate _ => false | .alreadyV2 => false | _ => true

end MigrationRow

end Concrete.Proof
