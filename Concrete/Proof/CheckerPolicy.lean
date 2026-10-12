import Concrete.Proof.Digest
import Concrete.Proof.Replay

/-!
# Checker advisory policy (R-0208, decisions 3 and 4)

A receipt says which checker accepted a theorem (receiptV2's readable `checker`, bound to its
`toolchain` id). Whether that checker is still TRUSTED is a different question with a different
owner: it changes when an upstream advisory is published, not when the program does. So it is
answered here, by a register compiled into the consumer, and recorded in each POLICY DECISION —
never written into the receipt, which stays an immutable record of what was checked.

**A register change re-evaluates; it does not invalidate.** Receipts are untouched by an advisory.
A decision is computed at read time from (receipt checker, register), so publishing an advisory
moves the decision for exactly the receipts whose checker it names, and withdrawing it moves them
back, while every other receipt keeps its decision.

**Fail closed.** A checker the register does not explicitly accept is `unknown`, which is not
accepted. An empty or unparseable checker identity is `unknown` too.

**Who controls it.** The register is source in this repository, versioned with the consumer and
compiled into it; its identity (`policyIdentity`) is a digest of its canonical rendering and is
named in every decision. Rollback protection is LIMITED and stated as such: `policyVersion` must not
fall below the separately owned minimum that `check_checker_policy.sh` enforces, so reverting the
register alone is refused; reverting both the register and that minimum together is visible in
history but is not refused by anything here.
-/

namespace Concrete.Proof

/-- What the register says about one checker build. -/
inductive AdvisoryStatus where
  | accepted
  | affected
  | revoked
  | unknown
  deriving BEq, Repr

def AdvisoryStatus.canonical : AdvisoryStatus → String
  | .accepted => "accepted"
  | .affected => "affected"
  | .revoked  => "revoked"
  | .unknown  => "unknown"

/-- One upstream advisory against a checker. Keyed by EXACT versions, never a prose range: a range
    is an interpretation, and two readers can disagree about whether a build falls in it. -/
structure CheckerAdvisory where
  /-- Upstream identifier with its tracker, e.g. `lean4#14576`. Not a CVE label: none is verified. -/
  id          : String
  /-- The checker implementation the advisory concerns. -/
  checker     : String
  /-- Exact `Lean.versionString` values affected. -/
  versions    : List String
  status      : AdvisoryStatus
  /-- The evidence class whose assurance the advisory removes. -/
  affects     : String
  /-- Versions in which upstream fixed it, exact. Empty while no fixed toolchain is selected. -/
  replacement : List String
  /-- What restores acceptance for evidence the advisory affects. -/
  replay      : String
  deriving Repr

/-- One MEASURED checker build the register accepts. Every field must match the receipt's readable
    checker exactly: a binary reporting the official release commit is not thereby shown to contain
    that code, so acceptance names the build digest that was validated, per platform. A rebuild — a
    new digest — is a new candidate that needs its own validation before it is listed. -/
structure AcceptedBuild where
  checker  : String
  version  : String
  commit   : String
  platform : String
  build    : String
  /-- How and where this build was validated, for the record. -/
  evidence : String
  deriving Repr

/-- The register: the explicitly accepted checker builds and the advisories. -/
structure CheckerRegister where
  /-- Monotone. Every change to the register raises it, and the gate holds it at or above the
      separately owned minimum. -/
  policyVersion : Nat
  /-- Measured builds explicitly accepted. Anything else is `unknown`. -/
  accepted      : List AcceptedBuild
  advisories    : List CheckerAdvisory
  deriving Repr

/-- The register this consumer is built with.

    **Lean #14576 fire drill.** The upstream issue shows the affected kernel accepting an axiom-free
    proof of `False` through checked declarations. The pinned `leanprover/lean4:v4.28.0` predates the
    fix, so its kernel's acceptance is not accepted for new high-assurance claims until an exact fixed
    toolchain is selected and the retained reproducer is tested against it. That removes the
    assurance the affected checker provided; it does NOT show that any theorem in this repository is
    false, and decisions must say the former.

    **Lean 4.34.1 accepted, per measured build (policy v2, 2026-10-12).** Accepted only after the
    upgrade (`ca7852e1`), the hostile controls (`check_checker_identity.sh`, `check_kernel_recheck.sh`
    including the declared-axiom and native-axiom legs) and a fresh replay of the frozen claim
    inventory (all 71 rows identical: docs/verification/claim-inventory-v4.34.1.tsv) passed on that
    build. macOS aarch64 only: the Nix devshell and elan load byte-identical images there and share the
    entry. Linux is NOT listed until its CI-measured digest has passed the same controls; until then a
    Linux checker reads `unknown`, never a commit-level fallback. -/
def checkerRegister : CheckerRegister :=
  { policyVersion := 2
  , accepted :=
      [ { checker := "lean", version := "4.34.1", commit := "5045d0056413266e57c625dcd7c365b10e377c52"
        , platform := "arm64-apple-darwin24.6.0"
        , build := "137d34023aa9f455f7400b746c400d05315331cfd41851969b3611319a8bfa81"
        , evidence := "macOS aarch64, official release archive via elan and the Nix devshell (identical images); upgrade ca7852e1, controls + 71-claim replay 2026-10-12" } ]
  , advisories :=
      [ { id := "lean4#14576", checker := "lean", versions := ["4.28.0"], status := .affected
        , affects := "kernel acceptance (logical validity of checked declarations)"
        , replacement := []
        , replay := "fresh kernel re-check under an accepted, fixed checker" } ] }

/-- Canonical bytes of a register: what `policyIdentity` digests. Every field, in a fixed order. -/
def CheckerRegister.canonical (r : CheckerRegister) : String :=
  let adv := r.advisories.map fun a =>
    s!"advisory {a.id}|{a.checker}|{",".intercalate a.versions}|{a.status.canonical}|{a.affects}|"
    ++ s!"{",".intercalate a.replacement}|{a.replay}"
  "\n".intercalate
    ([s!"policyVersion {r.policyVersion}"]
     ++ r.accepted.map (fun a => s!"accepted {a.checker}|{a.version}|{a.commit}|{a.platform}|{a.build}|{a.evidence}")
     ++ adv)

/-- The register's identity, named in every decision. -/
def CheckerRegister.policyIdentity (r : CheckerRegister) : String :=
  shortHash ("checkerPolicyV1:" ++ r.canonical)

/-- The full readable identity: implementation, version, commit, platform, build — or `none`. -/
def checkerFieldsOf (checker : String) : Option (String × String × String × String × String) :=
  match checker.splitOn " " with
  | [impl, ver, "commit", c, "platform", p, "build", b] =>
    if [impl, ver, c, p, b].any (·.isEmpty) then none else some (impl, ver, c, p, b)
  | _ => none

/-- The checker implementation and version a receipt's readable identity names, or `none`.
    Parsed from `CheckerIdentity.canonical` (`lean <version> commit <githash> platform ... build ...`). -/
def checkerVersionOf (checker : String) : Option (String × String) :=
  match checker.splitOn " " with
  | impl :: ver :: "commit" :: _ => if impl.isEmpty || ver.isEmpty then none else some (impl, ver)
  | _ => none

/-- One policy decision about one receipt's checker. Separate from the receipt. -/
structure PolicyDecision where
  status         : AdvisoryStatus
  /-- The advisories that decided it, by id. -/
  advisories     : List String
  /-- Which register decided: its version and identity. -/
  policyVersion  : Nat
  policyIdentity : String
  deriving Repr

/-- Decide whether the checker a receipt names is accepted under `r`.

    Precedence: a `revoked` advisory wins over `affected`, which wins over an explicit acceptance —
    an accepted build named by an advisory is not accepted. A checker neither accepted nor named is
    `unknown`. -/
def CheckerRegister.decide (r : CheckerRegister) (checker : String) : PolicyDecision :=
  let base := { status := AdvisoryStatus.unknown, advisories := [], policyVersion := r.policyVersion
              , policyIdentity := r.policyIdentity : PolicyDecision }
  match checkerVersionOf checker with
  | none => base
  | some (impl, ver) =>
    let hits := r.advisories.filter fun a => a.checker == impl && a.versions.contains ver
    let ids := hits.map (·.id)
    if hits.any (·.status == .revoked) then { base with status := .revoked, advisories := ids }
    else if hits.any (·.status == .affected) then { base with status := .affected, advisories := ids }
    else if (checkerFieldsOf checker).any (fun (i, v, c, p, b) => r.accepted.any fun a =>
        a.checker == i && a.version == v && a.commit == c && a.platform == p && a.build == b) then
      { base with status := .accepted, advisories := ids }
    else { base with advisories := ids }

/-- ACCEPTED NOW: a current receipt counts as accepted only when the checker THIS run used is accepted
    AND this run's kernel re-check of its theorem said `rechecked`. A pure function so the rule is
    tested on synthetic inputs on every platform — an oracle that only bites where some build happens
    to be registered (as the inline version did: Linux, unregistered, could not observe it) is not one. -/
def acceptedNow (d : PolicyDecision) (rv : RecheckVerdict) : Bool :=
  d.status == .accepted && rv == .rechecked

end Concrete.Proof
